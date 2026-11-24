-- ============================================
-- 64-00 集合知 ─ ビンの中の玉の数を 200 人が推測する。みんなの推測の「平均」は、ふつうの 1 人よりよく当たる
-- 推測 = 本当の数 + みんな共通の偏り b + 人それぞれのばらつき σ·z（z は標準正規分布）
--   多様性予測定理：集団の誤差 =「個人の誤差の平均」−「多様性（推測どうしのばらつき）」← どんな推測でも毎回ぴったり成り立つ式
--   ばらばらな推測ほど（多様性が大きいほど）集団は個人より強い。でも全員が同じ方向に偏る b は、平均しても消えない
-- 操作：スライダー（ドラッグ）共通の偏り b・ばらつき σ・人数 n / スペース 新しい集団 / A 自動 / R 元に戻す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)

trueCount :: Double
trueCount = 850                -- ★ ビンの中の本当の玉の数

-- 多様性予測定理の 3 つの量（と、集団の答えより遠い人の割合）
data Stats = Stats { csMean :: !Double, csCrowd :: !Double, csIndiv :: !Double, csDiv :: !Double, csBeat :: !Double }

data World = World
  { wSl      :: ![Slider]            -- 共通の偏り b・ばらつき σ・人数 n
  , wDrag    :: !(Maybe Int)         -- ドラッグ中のスライダー
  , wZs      :: ![Double]            -- 人それぞれの z（400 人ぶん）
  , wGuesses :: ![Double]
  , wSeed    :: !Int                 -- 次の集団の番号
  , wAuto    :: !Bool
  , wHist    :: ![Stats]             -- これまでの集団（古い順、60 まで）
  , wTest    :: [(String, Bool)] }

sliders0 :: [Slider]
sliders0 =
  [ Slider 60 0 260 (-300) 300 0 "shared bias b" 0 False     -- ★ 全員がそろって多め（+）/ 少なめ（−）に言う量
  , Slider 60 0 260 0 400 200 "spread sigma" 0 False         -- ★ 人それぞれのばらつき
  , Slider 60 0 260 1 400 200 "people n" 0 True ]            -- ★ 推測する人の数

main :: IO ()
main = runSketch (sketch "64-00 Wisdom of crowds" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (length (wGuesses world0))
      _ <- evaluate jarPic
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newCrowd (World sliders0 Nothing [] [] 1 False [] [])

-- 新しい集団：種 = 番号 × 7919 で z を 400 個
newCrowd :: World -> World
newCrowd w =
  let zs = withSeed (wSeed w * 7919) (mapM (const rndGauss) [1 .. 400 :: Int])
  in remake True w { wSeed = wSeed w + 1, wZs = zs }

-- ★ 推測を作る：本当の数 + b + σ·z（0 より小さくはならない）
makeGuesses :: Double -> Double -> Int -> [Double] -> [Double]
makeGuesses b sg n z = [ fromIntegral (max 0 (jsRoundI (trueCount + b + sg * zi))) | zi <- take n z ]

crowdStats :: [Double] -> Double -> Stats
crowdStats g t =
  let m = meanOf g
      len = fromIntegral (length g)
      indiv = foldl' (\s v -> s + (v - t) * (v - t)) 0 g / len
      dv = foldl' (\s v -> s + (v - m) * (v - m)) 0 g / len
      beat = fromIntegral (length (filter (\v -> abs (v - t) > abs (m - t)) g)) / len
  in Stats m ((m - t) * (m - t)) indiv dv beat

slv :: World -> Int -> Double
slv w k = slV (wSl w !! k)

remake :: Bool -> World -> World
remake record w =
  let g = makeGuesses (slv w 0) (slv w 1) (round (slv w 2)) (wZs w)
      h = wHist w ++ [crowdStats g trueCount]
  in w { wGuesses = g, wHist = if record then drop (length h - 60) h else wHist w }

step :: Env -> Float -> World -> IO World
step env _ w = pure (if wAuto w && envFrame env `mod` 20 == 0 then newCrowd w else w)

-- ビンの絵（玉は trueCount 個。最初に 1 回だけ作っておく）
jarPic :: Picture
jarPic = Pictures (Color (colOf ink) (thickPath 3 [(90, 126), (290, 126), (290, 386), (90, 386), (90, 124.5)]) : balls)
  where
    balls = withSeed 5 (mapM (const ball) [1 .. round trueCount :: Int])
    ball = do
      a <- rnd
      c <- if a < 0.5 then pure gold else do b <- rnd; pure (if b < 0.5 then teal else wine)
      x <- rnd
      y <- rnd
      pure (Color (colOf c) (circleAt (40 + 60 + 180 * f2 x) (96 + 290 - 8 - 220 * f2 (sqrt y)) 6))

draw :: Env -> World -> Picture
draw env w = view env
  [ jarPic
  , Pictures [ drawSlider (wDrag w == Just k) s | (k, s) <- zip [0 ..] (placed w) ]
  , say ("True count " ++ show (round trueCount :: Int) ++ ", crowd mean " ++ fmtNum 1 (csMean st)) 40 640 14 (rgbC 200 185 170)
  , say ("Crowd beats " ++ pctStr (csBeat st) ++ " of the people") 40 664 14 (rgbC 200 185 170)
  -- ---- 右上：推測のヒストグラム
  , plotAxes g "guessed count" "people (histogram)   white = true count, gold = crowd mean" [0, 500, 1000, 1500, 2000] [0, fromIntegral top]
  , Color (colA 200 teal) (Pictures [ rectTL (gX g (fromIntegral k * 40) + 1) (gY g (fromIntegral c)) (gX g 40 - gX g 0 - 2) (grY g + grH g - gY g (fromIntegral c))
                                    | (k, c) <- zip [0 :: Int ..] bins, c > 0 ])
  , Color (colOf ink) (lineW 3 (gX g trueCount, grY g) (gX g trueCount, grY g + grH g))
  , Color (colOf gold) (lineW 3 (gX g (csMean st), grY g) (gX g (csMean st), grY g + grH g))
  -- ---- 右中：定理の 3 本の棒（集団の誤差 = 個人の誤差の平均 − 多様性）
  , Pictures [ Pictures [ Color (colOf c) (rectTL (gx + 200) yy (f2 (max 1 (v * bw))) 22)
                        , sayRM nm (gx + 190) (yy + 11) 14 (rgbC 220 210 195)
                        , sayLM (fmtNum 1 v) (gx + 208 + f2 (max 1 (v * bw))) (yy + 11) 14 (rgbC 220 210 195) ]
             | (k, (nm, v, c)) <- zip [0 :: Int ..] bars, let yy = by + 34 * fromIntegral k ]
  , say (fmtNum 3 (csCrowd st) ++ " = " ++ fmtNum 3 (csIndiv st) ++ " - " ++ fmtNum 3 (csDiv st) ++ "   (difference " ++ fmtNum 9 (csIndiv st - csDiv st - csCrowd st) ++ ")") gx (by + 130) 16 (colOf ink)
  -- ---- 右下：これまでの集団（赤 = 個人の誤差の平均、金 = 集団の誤差）
  , plotAxes h2 "crowd number (Space / A adds one)" "past crowds: red = avg individual error, gold = crowd error" [0, 30, 60] [0, fromIntegral (jsRoundI (grY1 h2))]
  , Color (colOf wine) (polyline 2 [ gPt h2 (fromIntegral k) (csIndiv s) | (k, s) <- zip [0 :: Int ..] (wHist w) ])
  , Color (colOf gold) (polyline 2 [ gPt h2 (fromIntegral k) (csCrowd s) | (k, s) <- zip [0 :: Int ..] (wHist w) ])
  -- ---- 文字
  , say "Wisdom of crowds: crowd error = avg individual error - diversity" 40 40 20 (colOf ink)
  , say ("Drag sliders b, sigma, n   Space new crowd   A auto" ++ (if wAuto w then " (ON)" else " (OFF)") ++ "   R reset") 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    st = crowdStats (wGuesses w) trueCount
    gx = 400
    g0 = Graph gx 120 (envW env - gx - 50) (envH env * 0.3) 0 2000 0 1
    bins = U.toList (U.accum (+) (U.replicate 50 (0 :: Int)) [ (min 49 (floor (v / 40)), 1) | v <- wGuesses w ])
    top = maximum (5 : bins)
    g = g0 { grY1 = fromIntegral top }
    by = grY g + grH g + 70
    big = max 1 (csIndiv st)
    bw = realToFrac (envW env - gx - 330) / big
    bars = [("avg individual error", csIndiv st, wine), ("diversity (spread)", csDiv st, teal), ("crowd error", csCrowd st, gold)]
    h2 = Graph gx (by + 175) (envW env - gx - 50) (max 60 (envH env - by - 255)) 0 60 0 (maximum (100 : map csIndiv (wHist w)) * 1.1)

-- スライダーの位置（y は 450 から 56 ずつ）
placed :: World -> [Slider]
placed w = [ s { slY = 450 + 56 * fromIntegral k } | (k, s) <- zip [0 :: Int ..] (wSl w) ]

setSl :: Int -> Float -> World -> World
setSl k mx w = w { wSl = [ if j == k then sliderSet s mx else s | (j, s) <- zip [0 ..] (wSl w) ] }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
    in case sliderHit (placed w) (mx, my) of
         Just k -> remake False (setSl k mx w { wDrag = Just k })
         Nothing -> w
  EventMotion p | Just k <- wDrag w -> remake False (setSl k (fst (toP5 env p)) w)
  EventKey (MouseButton LeftButton) Up _ _ | Just _ <- wDrag w -> remake True w { wDrag = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> newCrowd w
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'r' || c == 'R' -> newCrowd w { wSl = sliders0, wHist = [] }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("多様性予測定理：でたらめな集団 20 組すべてで 集団の誤差 = 個人の誤差の平均 − 多様性（相対 1e-9）", all okId rs)
  , ("集団の誤差 ≤ 個人の誤差の平均（多様性 ≥ 0 だから、いつも）", all (\s -> csCrowd s <= csIndiv s + 1e-9) rs)
  , ("σ = 0・偏り b = 120：多様性 = 0、集団の誤差 = b² = 14400（平均しても偏りは消えない）", csDiv s0 == 0 && near (csCrowd s0) 14400 1e-9)
  , ("偏りなし・σ = 200・1 万人：集団の誤差 " ++ fmtNum 1 (csCrowd sb) ++ " ≪ 個人の誤差の平均 " ++ fmtNum 0 (csIndiv sb) ++ "（≈ σ² = 40000）"
    , csCrowd sb < 0.01 * csIndiv sb && abs (csIndiv sb / 40000 - 1) < 0.05)
  , ("1 人だけの集団：多様性 = 0、集団の誤差 = 個人の誤差 = 150² = 22500", csDiv s1 == 0 && csCrowd s1 == 22500 && csIndiv s1 == 22500) ]
  where
    -- でたらめな集団 20 組：b・σ・n もでたらめ
    rs = [ withSeed (100 + r) (do n <- rndInt 1 300
                                  z <- mapM (const rndGauss) [1 .. n]
                                  b <- rnd
                                  sg <- rnd
                                  pure (crowdStats (makeGuesses (600 * b - 300) (400 * sg) n z) trueCount))
         | r <- [0 .. 19] ]
    okId s = abs (csCrowd s - (csIndiv s - csDiv s)) <= 1e-9 * max 1 (csIndiv s)
    s0 = crowdStats (makeGuesses 120 0 200 (replicate 200 1)) trueCount
    sb = crowdStats (makeGuesses 0 200 10000 (withSeed 9 (mapM (const rndGauss) [1 .. 10000 :: Int]))) trueCount
    s1 = crowdStats [1000] trueCount

-- ---------- やってみよう ----------
-- 1. b = 0 のまま σ を大きくしていこう。個人の誤差は増えるのに、集団の誤差はあまり増えないのはなぜ？
-- 2. b = +200 にすると、人数 n をいくら増やしても集団の誤差は 200² 付近から下がらない（全員が同じ方向にまちがう）
-- 3. 人数 n = 1 にすると、多様性は 0 で「集団の誤差 = 個人の誤差」。n = 2, 3 だと？
-- 4. 定理の式の「差」がいつも 0（計算の誤差だけ）なのを確かめよう。どんな推測の集まりでも成り立つ式
-- 5. 03 では「相関がある」推測（みんな少しずつ似ている）を平均すると、どこまで誤差が下がるかを計算する

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

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

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

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

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

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

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 95 70 75) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (rgbC 175 160 150) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gY g v + 6) 14 (rgbC 175 160 150) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (rgbC 215 205 190), say yl (grX g) (grY g - 8) 14 (rgbC 215 205 190) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.2 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashed wd on off pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
              k = max 1 (ceiling (l / (on + off))) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
              fr = on / (on + off)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + fr) / fromIntegral k)) | i <- [0 .. k - 1] ]

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (colOf c) (rectTL x (yy - 5) 14 10), say s (x + 22) (yy + 5) 14 (rgbC 220 210 195)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ]

-- ---- 画面の中のスライダー：Slider x y w lo hi v name d（表示の桁）int（整数か）。マウスでドラッグ
data Slider = Slider { slX :: !Float, slY :: !Float, slW :: !Float, slLo :: !Double, slHi :: !Double, slV :: !Double
                     , slName :: !String, slD :: !Int, slInt :: !Bool }

drawSlider :: Bool -> Slider -> Picture
drawSlider active s =
  let t = f2 ((slV s - slLo s) / (slHi s - slLo s))
  in Pictures [ Color (rgbC 110 80 85) (lineW 4 (slX s, slY s) (slX s + slW s, slY s))
              , Color (colOf gold) (lineW 4 (slX s, slY s) (slX s + t * slW s, slY s))
              , Color (if active then rgbC 255 233 214 else colOf ink) (circleAt (slX s + t * slW s) (slY s) 16)
              , say (slName s ++ " = " ++ fmtNum (slD s) (slV s)) (slX s) (slY s - 12) 14 (rgbC 215 205 190) ]

sliderHit :: [Slider] -> (Float, Float) -> Maybe Int
sliderHit sl (mx, my) = headOr Nothing [ Just k | (k, s) <- zip [0 ..] sl, mx > slX s - 10, mx < slX s + slW s + 10, abs (my - slY s) < 14 ]

sliderSet :: Slider -> Float -> Slider
sliderSet s mx = let v = slLo s + clampv (realToFrac ((mx - slX s) / slW s)) 0 1 * (slHi s - slLo s)
                 in s { slV = if slInt s then fromIntegral (jsRoundI v) else v }

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

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

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)
