-- ============================================
-- 62-15 カメラで色の分類器 ─ 映像の上をクリックして「A の色」「B の色」を教えると、決定木が全部の画素を塗り分ける
-- 1 回のクリック = 訓練データ 1 個（その画素の R, G, B と、F なら位置 x, y も）。CART で学習 → 160 × 120 の全画素をたどって分類
--   木の図で「R ≤ 0.52？」のような質問が見える。深さ（↑↓）を上げると細かい色の差まで分ける
--   テスト画像のときは見本のクリックが入っている（C で消せる。カメラが映ると自動で消える）。カメラがなければテスト画像で動く
-- 操作：1 / 2 クリックで集めるクラス（A / B）/ クリック・ドラッグ 色を集める / ↑↓ 深さ / F 位置も使う / C 消す
--       V カメラ ⇔ テスト画像 / S 保存（SVG）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch :: Int
cw = 160                   -- カメラの大きさ
ch = 120

featNames :: [String]
featNames = ["R", "G", "B", "x", "y"]

-- 集めた見本：色（0〜1）、位置（0〜1）、クラス、テスト画像の見本か
data Sample = Sample { smF :: !Pt, smX :: !Double, smY :: !Double, smC :: !Int, smSeed :: !Bool }

data World = World
  { wDepth   :: !Int                  -- ★ 木の深さの上限
  , wUseXY   :: !Bool
  , wClass   :: !Int                  -- いま集めるクラス
  , wUseCam  :: !Bool
  , wCamSeen :: !Bool
  , wCam     :: Maybe Camera
  , wLive    :: !Bool
  , wPx      :: !(U.Vector Word8)     -- いまの画像（R, G, B を順に）
  , wSamples :: [Sample]
  , wTree    :: Maybe Node
  , wCounts  :: !(Int, Int)           -- A と B の画素の数
  , wSrc     :: !Picture
  , wOut     :: !Picture
  , wLastAdd :: !Float
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-15 Camera tree classifier" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch          -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = \env w -> drawW env w
  , skEvent = event
  }

world0 :: World
world0 = refresh (World 4 False 0 True False Nothing False testPix seedSamples Nothing (0, 0) Blank Blank (-1) [])

-- その画素の特徴量：R, G, B（0〜1）と、xy なら位置
featOf :: U.Vector Word8 -> Int -> Bool -> Pt
featOf px i xy = U.fromList ([c 0, c 1, c 2] ++ (if xy then [fromIntegral (i `mod` cw) / fromIntegral cw, fromIntegral (i `div` cw) / fromIntegral ch] else []))
  where c q = fromIntegral (px U.! (3 * i + q)) / 255

-- 点を作らずに木をたどる（速さのため。f = 特徴量の番号 → 値）
predictBy :: Node -> (Int -> Double) -> Int
predictBy t f = if nLeaf t then nPred t else predictBy (if f (nFeat t) <= nThr t then nLeft t else nRight t) f

-- ★ 集めた点から木を学習（1 クラスしかなければ「葉 1 枚」の木）
retrain :: World -> World
retrain w
  | null (wSamples w) = w { wTree = Nothing }
  | otherwise = let xs = V.fromList [if wUseXY w then smF s U.++ U.fromList [smX s, smY s] else smF s | s <- wSamples w]
                    ys = U.fromList (map smC (wSamples w))
                in w { wTree = Just (growTreeP defOpt { oMaxDepth = wDepth w, oNClass = 2 } xs ys) }

mkSample :: U.Vector Word8 -> Int -> Int -> Bool -> Sample
mkSample px i c sd = Sample (featOf px i False) (fromIntegral (i `mod` cw) / fromIntegral cw) (fromIntegral (i `div` cw) / fromIntegral ch) c sd

-- テスト画像の上の見本：A = 赤い円、B = 背景と緑
seedSamples :: [Sample]
seedSamples =
  [ mkSample testPix (jsRoundI (50 + 10 * cos a) + cw * jsRoundI (60 + 10 * sin a)) 0 True | k <- [0 .. 7 :: Int], let a = fromIntegral k / 8 * pi * 2 ]
  ++ [ mkSample testPix (x + cw * y) 1 True | (x, y) <- [(10, 10), (150, 10), (100, 100), (130, 30), (120, 80), (20, 110), (90, 20), (140, 60)] ]

-- ★ 全画素を木でたどって分類（−1 = 木がない）
classifyAll :: U.Vector Word8 -> Maybe Node -> Bool -> U.Vector Int
classifyAll px mt xy = U.generate (cw * ch) $ \i -> case mt of
  Nothing -> -1
  Just t -> predictBy t (\f -> if f < 3 then fromIntegral (px U.! (3 * i + f)) / 255
                               else if f == 3 then fromIntegral (i `mod` cw) / fromIntegral cw
                               else fromIntegral (i `div` cw) / fromIntegral ch)

countAB :: U.Vector Int -> (Int, Int)
countAB ls = (U.length (U.filter (== 0) ls), U.length (U.filter (== 1) ls))

-- 分類して 2 枚の絵を作り直す
refresh :: World -> World
refresh w0 = w { wCounts = countAB labels, wSrc = src, wOut = out }
  where
    w = retrain w0
    px = wPx w
    labels = classifyAll px (wTree w) (wUseXY w)
    src = pixelImage cw ch 1 (\x y -> let i = 3 * (x + cw * y) in (px U.! i, px U.! (i + 1), px U.! (i + 2)))
    out = pixelImage cw ch 1 (\x y ->
            let i = x + cw * y
                g = (fromIntegral (px U.! (3 * i)) + fromIntegral (px U.! (3 * i + 1)) + fromIntegral (px U.! (3 * i + 2))) / 3 :: Double
                k = labels U.! i
                (r, gg, b) = clsCol k
                q v = fromIntegral (rnd8 v) :: Word8
            in if k < 0 then (q g, q g, q g) else (q (r * 0.65 + g * 0.35), q (gg * 0.65 + g * 0.35), q (b * 0.65 + g * 0.35)))

step :: Env -> Float -> World -> IO World
step _ _ w = do
  fr <- case wCam w of
    Just cam | wUseCam w -> do
      ok <- camLive cam
      if not ok then pure Nothing else do
        f <- cameraFrame cam
        pure (if B.length f >= cw * ch * 3 then Just f else Nothing)
    _ -> pure Nothing
  case fr of
    Just f -> do
      let px = U.generate (cw * ch * 3) (BU.unsafeIndex f)
          w1 = w { wPx = px, wLive = True }
          w2 = if wCamSeen w1 then w1 else w1 { wCamSeen = True, wSamples = filter (not . smSeed) (wSamples w1) }   -- カメラが映ったら、テスト画像の見本は消す
      pure (refresh w2)
    Nothing | wLive w -> pure (refresh w { wPx = testPix, wLive = False })
            | otherwise -> pure w

boxes :: Env -> (Box, Box)
boxes env = let s = min ((envW env - 140) / (2 * fromIntegral cw)) ((envH env - 380) / fromIntegral ch)
                bw = fromIntegral cw * s
                bh = fromIntegral ch * s
            in (Box 40 100 bw bh, Box (80 + bw) 100 bw bh)

drawW :: Env -> World -> Picture
drawW env w = view env
  [ imageBox (bxX vb) (bxY vb) (bxW vb) (bxH vb) cw ch (wSrc w)
  , imageBox (bxX ob) (bxY ob) (bxW ob) (bxH ob) cw ch (wOut w)
  -- 集めた点
  , Pictures [ let x = bxX vb + f2 (smX s) * bxW vb
                   y = bxY vb + f2 (smY s) * bxH vb
               in Pictures [Color white (circleAt x y 13), Color (clsC (smC s)) (circleAt x y 10)] | s <- wSamples w ]
  -- ---- 木の図
  , maybe Blank (\t -> fst (drawTree t tb treeDraw { tdText = nodeText })) (wTree w)
  -- ---- 文字
  , say ("Camera tree classifier   " ++ (if wLive w then "camera" else "test image") ++ "   collecting: " ++ (if wClass w == 1 then "B (blue)" else "A (orange)")
         ++ "   max depth " ++ show (wDepth w) ++ (if wUseXY w then "   features R,G,B,x,y" else "   features R,G,B")) 40 40 20 (grayC 240)
  , say "1/2 class A/B   click/drag collect colours   Up/Down depth   F use position   C clear   V camera/test image   S save" 40 66 15 (grayC 170)
  , say ("Training data: A " ++ show nA ++ ", B " ++ show (length (wSamples w) - nA)
         ++ maybe "  -> nothing to learn yet (click to collect)" (\t -> "  -> " ++ show (countLeaves t) ++ " leaves, pixels A " ++ show (fst (wCounts w)) ++ " / B " ++ show (snd (wCounts w))) (wTree w))
        40 (bxY vb + bxH vb + 28) 15 (grayC 220)
  , drawSelfTest env (wTest w) ]
  where
    (vb, ob) = boxes env
    tb = Box 40 (bxY vb + bxH vb + 50) (envW env - 80) (envH env - bxY vb - bxH vb - 90)
    nA = length (filter ((== 0) . smC) (wSamples w))

nodeText :: Node -> Bool -> [String]
nodeText t leaf
  | leaf = [(if nPred t == 1 then "B" else "A") ++ " " ++ countsStr (nCounts t), ""]
  | otherwise = [headOr "?" (drop (nFeat t) featNames) ++ " <= " ++ fmtNum 2 (nThr t), "n=" ++ show (nN t)]

-- マウスの下の画素を見本にする（左の映像でも右の塗り分けでもよい）
sampleAt :: Env -> (Float, Float) -> World -> World
sampleAt env p w = case [i | b <- [vb, ob], let u = (fst p - bxX b) / bxW b, let v = (snd p - bxY b) / bxH b
                           , u >= 0 && u < 1 && v >= 0 && v < 1, let i = floor (u * fromIntegral cw) + cw * floor (v * fromIntegral ch)] of
  (i : _) -> refresh w { wSamples = wSamples w ++ [mkSample (wPx w) i (wClass w) False], wLastAdd = envTime env }
  [] -> w
  where (vb, ob) = boxes env

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (sampleAt env (toP5 env pos) w) { wLastAdd = envTime env }
  EventMotion pos | envDown env && envTime env - wLastAdd w > 0.08 && length (wSamples w) < 400 -> pure (sampleAt env (toP5 env pos) w)
  EventKey (Char '1') Down _ _ -> pure w { wClass = 0 }
  EventKey (Char '2') Down _ _ -> pure w { wClass = 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (refresh w { wDepth = min 10 (wDepth w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (refresh w { wDepth = max 1 (wDepth w - 1) })
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> pure (refresh w { wUseXY = not (wUseXY w) })
    | c == 'c' || c == 'C' -> pure (refresh w { wSamples = [] })
    | c == 'v' || c == 'V' -> let w1 = w { wUseCam = not (wUseCam w), wCamSeen = False }
                              in pure (refresh (if not (wUseCam w1) && null (wSamples w1) then w1 { wSamples = seedSamples } else w1))
    | c == 's' || c == 'S' -> do
        saveSvg "tree_classifier" env (drawW env w)
        pure w
  _ -> pure w

-- カメラがないときのテスト画像（青い背景、赤い円、緑の箱、黄色の線）
testPix :: U.Vector Word8
testPix = U.generate (cw * ch * 3) $ \j ->
  let (i, q) = j `divMod` 3
      (y, x) = i `divMod` cw
      fx = fromIntegral x :: Double
      fy = fromIntegral y :: Double
      c0 = (40 + 0.4 * fx, 70 + 0.3 * fy, 140 + 0.3 * fx)
      c1 = if (x - 50) * (x - 50) + (y - 60) * (y - 60) < 22 * 22 then (215 - 0.6 * abs (fy - 60), 70, 55) else c0
      c2 = if x > 95 && x < 140 && y > 60 && y < 95 then (70, 170, 80) else c1
      (r, g, b) = if abs (fy - 0.3 * fx - 15) < 2 && x > 90 then (235, 210, 60) else c2
  in fromIntegral (rnd8 (case q of { 0 -> r; 1 -> g; _ -> b }))   -- Uint8ClampedArray と同じ丸め

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("赤 と 青 の見本 20 個：深さ 1 の木で 100%、新しい赤 (0.9, 0.1, 0.1) → A、青 (0.1, 0.2, 0.9) → B"
    , accuracyOf (treePredict t1) xs ys == 1 && treePredict t1 (U.fromList [0.9, 0.1, 0.1]) == 0 && treePredict t1 (U.fromList [0.1, 0.2, 0.9]) == 1)
  , ("ばらばらな色 60 個でも、最後まで育てると訓練 100%。深さの上限 3 なら木の深さ ≤ 3"
    , accuracyOf (treePredict full) xr yr == 1 && treeDepth d3 <= 3)
  , ("見本が 0 個なら分類しない（全部 −1、NaN もエラーもなし）", let (a, b) = countAB (classifyAll testPix Nothing False) in a + b == 0)
  , ("1 クラスしかない見本 → 葉 1 枚の木、全画素がそのクラス", nLeaf one && snd (countAB (classifyAll testPix (Just one) False)) == cw * ch)
  , ("色が同じでも、位置（横 x）を使えば左右で分けられる"
    , not (nLeaf tpos) && nFeat tpos == 3 && treePredict tpos (U.fromList [0.5, 0.5, 0.5, 0.05, 0.3]) == 0 && treePredict tpos (U.fromList [0.5, 0.5, 0.5, 0.95, 0.3]) == 1)
  , ("テスト画像：見本の木で 160 × 120 の全画素を分類（A + B = 19200）、赤い円の中心は A"
    , let (a, b) = countAB (classifyAll testPix (wTree sw) False)
      in a + b == cw * ch && maybe False (\t -> treePredict t (featOf testPix (50 + cw * 60) False) == 0) (wTree sw)) ]
  where
    xs = V.fromList (concat [[U.fromList [0.8 + 0.01 * k, 0.2, 0.2], U.fromList [0.2, 0.3 + 0.01 * k, 0.8]] | k <- [0 .. 9]])
    ys = U.fromList (concat (replicate 10 [0, 1]))
    t1 = growTreeP defOpt { oMaxDepth = 1 } xs ys
    (xr, yr) = let ps = fst (evalR (rngOf 1) (seedRng 15 >> mapM (\_ -> do
                          a <- rnd; b <- rnd; c <- rnd; r <- rnd
                          pure (U.fromList [a, b, c], if r < 0.5 then 1 else 0)) [1 .. 60 :: Int]))
               in (V.fromList (map fst ps), U.fromList (map snd ps))
    full = growTreeP defOpt xr yr
    d3 = growTreeP defOpt { oMaxDepth = 3 } xr yr
    one = growTreeP defOpt { oNClass = 2 } (V.fromList [U.fromList [0.5, 0.5, 0.5], U.fromList [0.4, 0.4, 0.4]]) (U.fromList [1, 1])
    tpos = growTreeP defOpt (V.fromList [U.fromList [0.5, 0.5, 0.5, v, 0.5] | v <- [0.1, 0.2, 0.8, 0.9]]) (U.fromList [0, 0, 1, 1])
    sw = retrain world0 { wSamples = seedSamples }

-- ---------- やってみよう ----------
-- 1. 2 を押して自分の服の色を B に、1 を押して肌や背景を A に。何回クリックするとうまく分かれる？
-- 2. ↑↓ で深さを 1 と 8 にして、塗り分けのちがいを見よう。深さ 1 は「R ≤ ○？」1 回だけ
-- 3. F で位置も使うと、同じ色でも場所で分けられる。でもカメラを動かすと？
-- 4. 明るさが変わると（電気を消すなど）分類はどうなる？ 色の特徴量の弱点
-- 5. 木の図の質問を読んで、「この木は何を見て A と言っているか」を説明しよう。S で保存

-- ==================================================
-- ここから下は「決定木の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・いちばん大きい番号
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1, 2）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

-- いちばん大きい番号（同点なら小さい番号）
argMaxU :: U.Vector Double -> Int
argMaxU a = if U.null a then 0 else U.ifoldl' (\b i v -> if v > a U.! b then i else b) 0 a

maxU :: U.Vector Double -> Double
maxU a = if U.null a then 0 else U.maximum a

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 不純度：counts = 各クラスの個数（重みつきでもよい）
data Crit = Gini | Entropy | MisErr deriving (Eq, Show)

log2 :: Double -> Double
log2 q = log q / log 2

entropyP :: Double -> Double               -- 2 クラスのエントロピー（ビット）
entropyP p = if p <= 0 || p >= 1 then 0 else -p * log2 p - (1 - p) * log2 (1 - p)

entropyOf :: U.Vector Double -> Double
entropyOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\h c -> if c > 0 then let q = c / n in h - q * log2 q else h) 0 counts)

giniOf :: U.Vector Double -> Double
giniOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\s c -> s - (c / n) * (c / n)) 1 counts)

errOf :: U.Vector Double -> Double           -- 誤分類率
errOf counts = let n = sumU counts in if n <= 0 then 0 else max 0 (1 - maxU counts / n)

impurityOf :: Crit -> U.Vector Double -> Double
impurityOf crit = case crit of
  Entropy -> entropyOf
  MisErr -> errOf
  Gini -> giniOf

-- 分けたときの「不純度の減り」= 親 − (左の割合·左 + 右の割合·右)。Entropy なら情報利得
gainOf :: Crit -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Double
gainOf crit parent left right =
  let n = sumU parent
      nl = sumU left
      nr = sumU right
  in if n <= 0 then 0 else impurityOf crit parent - (nl / n) * impurityOf crit left - (nr / n) * impurityOf crit right

cnt :: [Double] -> U.Vector Double
cnt = U.fromList

-- ---- CART（分類）：全部の特徴量 × 全部のしきい値（並べた値の中点）を調べて、不純度がいちばん減る分割を選ぶ
-- ノード：分類の木も回帰の木もこの形。葉の nLeft, nRight は使わない
data Node = Node
  { nLeaf   :: !Bool
  , nFeat   :: !Int                 -- 何番目の特徴量で切るか（0 = x1）
  , nThr    :: !Double              -- しきい値（x ≤ thr なら左）
  , nLeft   :: Node
  , nRight  :: Node
  , nCounts :: !(U.Vector Double)   -- 各クラスの個数（重みつき）
  , nN      :: !Int                 -- 点の数
  , nImp    :: !Double              -- 不純度
  , nPred   :: !Int                 -- 多数決のクラス
  , nDec    :: !Double              -- 分けたときの重みつきの不純度の減り（回帰木では SSE の減り）
  , nDepth  :: !Int
  , nId     :: !Int                 -- 行きがけ順の番号（0 = 根）
  , nCut    :: !Double              -- 剪定：この α 以上で葉として扱う（刈られないなら +∞）
  , nValue  :: !Double              -- 回帰木：葉の値（平均）
  , nSse    :: !Double              -- 回帰木：2 乗誤差
  }

nilNode :: Node
nilNode = Node True 0 0 nilNode nilNode U.empty 0 0 0 0 0 0 posInf 0 0

-- o = { maxDepth, minLeaf, crit, mtry（分割ごとに見る特徴量の数、0 = 全部）, w（点の重み）, idx（使う点の番号）, nClass（0 = 自動）}
data TreeOpt = TreeOpt
  { oMaxDepth :: !Int, oMinLeaf :: !Int, oCrit :: !Crit, oMtry :: !Int
  , oW :: !(Maybe (U.Vector Double)), oIdx :: !(Maybe (U.Vector Int)), oNClass :: !Int }

defOpt :: TreeOpt
defOpt = TreeOpt 40 1 Gini 0 Nothing Nothing 0

-- 育てるときの道具箱：cCols = 特徴量ごとの列、cW = 点の重み
data Ctx = Ctx { cCols :: !(V.Vector (U.Vector Double)), cY :: !(U.Vector Int), cW :: !(U.Vector Double)
               , cK :: !Int, cD :: !Int, cO :: !TreeOpt }

-- 木を育てる（mtry を使うときだけ乱数を引く）
-- 速くするため、根で特徴量ごとに点を 1 回だけ並べ、子にはその並びを分けて渡す（並べ直したのと同じ順になる）
growTree :: TreeOpt -> V.Vector Pt -> U.Vector Int -> R Node
growTree o xs ys = numberTree <$> growNode ctx idx0 ords0 0
  where
    k = if oNClass o > 0 then oNClass o else max 1 (if U.null ys then 1 else U.maximum ys + 1)
    d = if V.null xs then 0 else U.length (V.head xs)
    n = V.length xs
    idx0 = fromMaybe (U.enumFromN 0 n) (oIdx o)
    cols = V.generate d (\f -> U.generate n (\i -> xAt xs i f))
    ords0 = V.map (\col -> U.fromList (sortOn (col U.!) (U.toList idx0))) cols
    ctx = Ctx cols ys (fromMaybe (U.replicate n 1) (oW o)) k d o { oMinLeaf = max 1 (oMinLeaf o) }

-- 乱数を使わない木（mtry = 0）
growTreeP :: TreeOpt -> V.Vector Pt -> U.Vector Int -> Node
growTreeP o xs ys = fst (runR (growTree o { oMtry = 0 } xs ys) (Rng 1))

-- idx = このノードの点（重複あり）、ords = 特徴量ごとに値の順に並べた idx
growNode :: Ctx -> U.Vector Int -> V.Vector (U.Vector Int) -> Int -> R Node
growNode ctx idx ords depth = do
  let o = cO ctx
      counts = U.accumulate (+) (U.replicate (cK ctx) 0) (U.map (\i -> (cY ctx U.! i, cW ctx U.! i)) idx)
      node = nilNode { nLeaf = True, nCounts = counts, nN = U.length idx, nImp = impurityOf (oCrit o) counts
                     , nPred = argMaxU counts, nDepth = depth, nDec = 0 }
      kinds = U.length (U.filter (> 0) counts)
  if depth >= oMaxDepth o || U.length idx < 2 * oMinLeaf o || kinds <= 1 then pure node else do
    mb <- bestSplit ctx ords counts
    case mb of
      Nothing -> pure node
      Just (f, thr, dec) -> do
        let col = cCols ctx V.! f
            goL i = col U.! i <= thr
            (l, r) = U.partition goL idx
        lt <- growNode ctx l (V.map (U.filter goL) ords) (depth + 1)
        rt <- growNode ctx r (V.map (U.filter (not . goL)) ords) (depth + 1)
        pure node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }

-- ★ いちばんよい分割：特徴量ごとに点を並べ、左から 1 つずつ動かしながら左右の個数を更新する
bestSplit :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> R (Maybe (Int, Double, Double))
bestSplit ctx ords counts = do
  let d = cD ctx
      mtry = oMtry (cO ctx)
  feats <- if mtry > 0 && mtry < d then U.toList <$> shuffleU (U.enumFromN 0 d) else pure [0 .. d - 1]   -- ランダムフォレスト：前の mtry 個だけを見る
  let first = if mtry > 0 then min mtry d else d
      best = splitOver ctx ords counts (take first feats)
  pure (if isNothing best && first < d then splitOver ctx ords counts (drop first feats) else best)   -- 切れる特徴量が 1 つもないときだけ残りを見る

splitOver :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> [Int] -> Maybe (Int, Double, Double)
splitOver ctx ords counts = foldl' feat Nothing
  where
    o = cO ctx
    crit = oCrit o
    minLeaf = oMinLeaf o
    w = sumU counts
    pImp = impurityOf crit counts
    feat best f =
      let ord = ords V.! f
          col = cCols ctx V.! f
          len = U.length ord
          go !k !lc !lw !b
            | k >= len - 1 = b
            | otherwise =
                let i = U.unsafeIndex ord k
                    wi = U.unsafeIndex (cW ctx) i
                    yi = U.unsafeIndex (cY ctx) i
                    lc' = U.unsafeUpd lc [(yi, U.unsafeIndex lc yi + wi)]
                    lw' = lw + wi
                    v0 = U.unsafeIndex col i
                    v1 = U.unsafeIndex col (U.unsafeIndex ord (k + 1))
                in if not (v1 > v0) || k + 1 < minLeaf || len - k - 1 < minLeaf then go (k + 1) lc' lw' b else
                   let rc = U.zipWith (-) counts lc'
                       dec = w * pImp - lw' * impurityOf crit lc' - (w - lw') * impurityOf crit rc
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) lc' lw' b'
      in go 0 (U.replicate (cK ctx) 0) 0 best

-- 行きがけ順に番号をつけ直す（p5.js 版の node.id と同じ順）
numberTree :: Node -> Node
numberTree t0 = fst (go 0 t0)
  where
    go !n t | nLeaf t = (t { nId = n }, n + 1)
            | otherwise = let (l, n1) = go (n + 1) (nLeft t)
                              (r, n2) = go n1 (nRight t)
                          in l `seq` r `seq` (t { nId = n, nLeft = l, nRight = r }, n2)

-- 見せる深さ dLim、剪定の α = aLim で「葉として扱う」か
isCut :: Int -> Double -> Node -> Bool
isCut dLim aLim t = nLeaf t || nDepth t >= dLim || aLim >= nCut t

noD :: Int                                    -- 深さの制限なし
noD = maxBound

-- 根から葉までたどる。dLim = 見せる深さ、aLim = 剪定の α（制限なし = noD, negInf）
leafOfL :: Int -> Double -> Node -> Pt -> Node
leafOfL dLim aLim node x
  | isCut dLim aLim node = node
  | otherwise = leafOfL dLim aLim (if x U.! nFeat node <= nThr node then nLeft node else nRight node) x

leafOf :: Node -> Pt -> Node
leafOf = leafOfL noD negInf

treePredict :: Node -> Pt -> Int
treePredict t x = nPred (leafOf t x)

treePredictL :: Int -> Double -> Node -> Pt -> Int
treePredictL dl al t x = nPred (leafOfL dl al t x)

-- 見えているノードを全部（行きがけ順）
treeNodesL :: Int -> Double -> Node -> [Node]
treeNodesL dl al t = t : if isCut dl al t then [] else treeNodesL dl al (nLeft t) ++ treeNodesL dl al (nRight t)

treeNodes :: Node -> [Node]
treeNodes = treeNodesL noD negInf

countLeavesL :: Int -> Double -> Node -> Int
countLeavesL dl al t = length (filter (isCut dl al) (treeNodesL dl al t))

countLeaves :: Node -> Int
countLeaves = countLeavesL noD negInf

treeDepthL :: Int -> Double -> Node -> Int
treeDepthL dl al t = maxOr 0 (map nDepth (treeNodesL dl al t))

treeDepth :: Node -> Int
treeDepth = treeDepthL noD negInf

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double   -- fn(x) → クラス
accuracyOf fn xs ys = let n = V.length xs
                          ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) [0 .. n - 1])
                      in if n > 0 then fromIntegral ok / fromIntegral n else 0

accuracyIdx :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> [Int] -> Double   -- idx の点だけ
accuracyIdx fn xs ys ids = let ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) ids)
                           in if null ids then 0 else fromIntegral ok / fromIntegral (length ids)

-- ---- 描く道具：クラスの色、領域の絵、点、数の書き方
type RGB = (Double, Double, Double)

clsCol :: Int -> RGB                          -- ★ クラス A・B・C の色
clsCol c = case c of
  0 -> (245, 140, 75)
  1 -> (85, 175, 240)
  2 -> (175, 225, 95)
  _ -> (200, 200, 200)

bgCol :: RGB                                  -- 背景（森の色）
bgCol = (20, 30, 26)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

clsC :: Int -> Color
clsC = colOf . clsCol

-- クラスの割合 → 背景と混ぜた色（k = 色の濃さ）
mixCol :: U.Vector Double -> Double -> RGB
mixCol probs k =
  let s0 = sumU probs
      s = if s0 == 0 then 1 else s0
      add (r, g, b) (j, p) = let (cr, cg, cb) = clsCol j
                                 (br, bg, bb) = bgCol
                             in (r + (p / s) * (cr - br) * k, g + (p / s) * (cg - bg) * k, b + (p / s) * (cb - bb) * k)
  in foldl' add bgCol (zip [0 ..] (U.toList probs))

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ws = 点ごとの大きさの倍率）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Maybe (U.Vector Double) -> Picture
drawPoints bx xs ys r ws = Pictures
  [ Pictures [Color (grayC 15) (circleAt px py (d + 2)), Color (clsC (ys U.! i)) (circleAt px py d)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1)
        d = r * maybe 1 (\w -> f2 (w U.! i)) ws ]

-- 数を短く書く（toFixed(d) のあと余分な 0 を消す。p5.js 版の fmtNum）
-- toFixed と同じく、double のちょうどの値で丸め、ちょうど半分なら絶対値の大きいほうへ
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = stripZeros (if d > 0 then ip ++ "." ++ fp else ip)
                in if n == 0 || v > 0 then body else '-' : body
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if lastOr ' ' t == '.' then init t else t
                     | otherwise = s

-- 件数 [5|7] の形
countsStr :: U.Vector Double -> String
countsStr c = "[" ++ intercalate "|" (map (fmtNum 2) (U.toList c)) ++ "]"

-- ---- 木の図：葉を左から順に並べ、親は子の真ん中。tdText t leaf → 箱の 2 行、tdCol → 色、tdHl = 光らせる id
data TreeDraw = TreeDraw { tdDLim :: !Int, tdALim :: !Double, tdText :: Node -> Bool -> [String]
                         , tdCol :: Node -> RGB, tdHl :: S.Set Int }

treeDraw :: TreeDraw
treeDraw = TreeDraw noD negInf (\_ _ -> []) (clsCol . nPred) S.empty

data TreeInfo = TreeInfo { tiLeaves :: !Int, tiMaxD :: !Int, tiPos :: IM.IntMap (Float, Float), tiBig :: !Bool }

drawTree :: Node -> Box -> TreeDraw -> (Picture, TreeInfo)
drawTree root bx o = (Pictures (edges ++ boxes), TreeInfo leafN maxD posMap big)
  where
    cut = isCut (tdDLim o) (tdALim o)
    vis = treeNodesL (tdDLim o) (tdALim o) root
    maxD = maxOr 0 (map nDepth vis)
    place (!k, m) t
      | cut t = ((k + 1, IM.insert (nId t) (fromIntegral k) m), fromIntegral k)
      | otherwise = let (s1, a) = place (k, m) (nLeft t)
                        ((k2, m2), b) = place s1 (nRight t)
                        p = (a + b) / 2
                    in ((k2, IM.insert (nId t) p m2), p)
    ((leafN, slot), _) = place (0 :: Int, IM.empty) root
    cw = bxW bx / fromIntegral (max 1 leafN)
    rh = min 92 (bxH bx / fromIntegral (maxD - nDepth root + 1))
    bw = min 150 (cw - 4)
    big = bw >= 96 && rh >= 50
    pOf t = (bxX bx + (IM.findWithDefault 0 (nId t) slot + 0.5) * cw, bxY bx + fromIntegral (nDepth t - nDepth root) * rh + 18)
    posMap = IM.fromList [(nId t, pOf t) | t <- vis]
    off = if big then 18 else 4
    edges = [ Color (rgbC 120 140 125) (lineW 1.5 (ax, ay + off) (bx', by - off))
            | t <- vis, not (cut t), c <- [nLeft t, nRight t], let (ax, ay) = pOf t, let (bx', by) = pOf c ]
    boxes = map one vis
    one t =
      let (x, yy) = pOf t
          leaf = cut t
          col@(cr, cg, cb) = tdCol o t
          hl = S.member (nId t) (tdHl o)
      in if big
           then Pictures
                  [ Color (if leaf then colOf (cr * 0.35 + 14, cg * 0.35 + 20, cb * 0.35 + 17) else rgbC 30 44 38) (rectTL (x - bw / 2) (yy - 18) bw 36)
                  , Color (if hl then rgbC 255 240 150 else if leaf then colOf col else rgbC 110 130 115) (frameTL (if hl then 3 else 1.5) (x - bw / 2) (yy - 18) bw 36)
                  , Pictures [sayM s x (yy - 8 + fromIntegral q * 16) 14 (grayC 235) | (q, s) <- zip [0 :: Int ..] (take 2 (tdText o t leaf))] ]
           else let d = max 5 (min 14 (cw * 0.6))
                in Pictures [ Color (if hl then rgbC 255 240 150 else grayC 15) (circleAt x yy (d + if hl then 6 else 2))
                            , Color (colOf col) (circleAt x yy d) ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

-- 段落をいくつか上から順に（左上 = (x, y)、段落の間は gap）
sayParas :: [String] -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
sayParas ps x y maxW size gap c = Pictures (go y ps)
  where go _ [] = []
        go yy (p : rest) = let n = length (wrapText maxW size p)
                           in sayWrap p x yy maxW size c : go (yy + fromIntegral n * size * 1.3 + gap) rest

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- ---- 数の道具
-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
