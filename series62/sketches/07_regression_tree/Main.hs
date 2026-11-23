-- ============================================
-- 62-07 回帰木 ─ 数（連続の値）を当てる木。葉の答え = その葉に入った点の平均 → 区分的に一定（階段）の関数になる
-- 分割は「2 乗誤差の合計 SSE がいちばん減るしきい値」。SSE = Σ(y − 平均)²。減り = SSE(親) − SSE(左) − SSE(右)
--   深さ（↑↓）と、葉に残す最小の点の数（← →）で、階段の細かさが変わる。細かすぎるとノイズまでなぞる
--   V で 2 次元：本当の関数（左）と、深さ d の回帰木（右）を色で比べる
-- 操作：↑↓ 深さ / ← → 葉の最小の点の数 / クリック 点を足す（1 次元）/ V 1 次元 ⇔ 2 次元 / R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, nub, sort, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 80                  -- ★ 1 次元の点の数

noiseS :: Double
noiseS = 0.15              -- ★ ノイズの大きさ

imgN :: Int
imgN = 120

hidden1 :: Double -> Double             -- ★ 本当の関数（ここを変えてみよう）
hidden1 x = 0.6 * sin (8 * x) + (if x > 0.62 then 0.7 else 0) + 0.3 * x

hidden2 :: Double -> Double -> Double
hidden2 u v = sin (5 * u) * cos (4 * v) + (if u + v > 1.2 then 0.6 else 0)

data World = World
  { wDepth  :: !Int         -- ★ 深さ
  , wMinLeaf :: !Int        -- ★ 葉に残す最小の点の数
  , wSeed   :: !Int
  , wTwoD   :: !Bool
  , wX1     :: !(V.Vector Pt)
  , wY1     :: !(U.Vector Double)
  , wX2     :: !(V.Vector Pt)
  , wY2     :: !(U.Vector Double)
  , wTree1  :: !Node
  , wTree2  :: !Node
  , wSseC   :: [Double]     -- 深さ 0〜10 の訓練の SSE
  , wImgT   :: !Picture     -- 本当の関数の絵
  , wImgF   :: !Picture     -- 回帰木の絵
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-07 Regression tree" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      sum (wSseC w) `seq` wImgF w `seq` wImgT w `seq` pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newData (World 3 3 4 False V.empty U.empty V.empty U.empty nilNode nilNode [] imgT Blank [])
  where imgT = paintImage imgN imgN (\u v -> heat (hidden2 u v))

newData :: World -> World
newData w =
  let ((p1, p2), _) = evalR (rngOf 1) $ do
        seedRng (wSeed w)
        a <- mapM (\_ -> do x <- rnd; g <- rndGauss; pure (U.singleton x, hidden1 x + noiseS * g)) [1 .. nPts]
        b <- mapM (\_ -> do u <- rnd; v <- rnd; g <- rndGauss; pure (pt2 u v, hidden2 u v + noiseS * g)) [1 .. 500 :: Int]
        pure (a, b)
  in retrain w { wX1 = V.fromList (map fst p1), wY1 = U.fromList (map snd p1), wX2 = V.fromList (map fst p2), wY2 = U.fromList (map snd p2) }

retrain :: World -> World
retrain w =
  let t1 = growReg (wDepth w) (wMinLeaf w) Nothing (wX1 w) (wY1 w)
      t2 = growReg (wDepth w + 3) (wMinLeaf w) Nothing (wX2 w) (wY2 w)
      full = growReg 40 (wMinLeaf w) Nothing (wX1 w) (wY1 w)      -- 深さと SSE のグラフ用（最後まで育てて深さ d で切る）
      sseAt d = foldl' (+) 0 [sqr (regPredictL d full x - y) | (x, y) <- zip (V.toList (wX1 w)) (U.toList (wY1 w))]
  in w { wTree1 = t1, wTree2 = t2, wSseC = map sseAt [0 .. 10]
       , wImgF = paintImage imgN imgN (\u v -> heat (regPredict t2 (pt2 u v))) }

heat :: Double -> RGB
heat v = let q = clampv ((v + 1.5) / 3.2) 0 1 in (40 + 210 * q, 70 + 120 * sin (q * pi), 200 - 170 * q)

plotG :: Env -> Graph
plotG env = Graph 70 110 (envW env * 0.56) (envH env - 240) 0 1 (-1.2) 1.8

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Regression tree   " ++ (if wTwoD w then "2-D" else "1-D") ++ "   depth " ++ show (wDepth w) ++ (if wTwoD w then " (+3 in 2-D)" else "")
         ++ "   min points per leaf " ++ show (wMinLeaf w)) 40 40 20 (grayC 240)
  , say "Up/Down depth   <- -> min points per leaf   click add point (1-D)   V 1-D <-> 2-D   R new data" 40 66 15 (grayC 170) ]
  ++ (if wTwoD w then draw2D env w else draw1D env w)
  ++ [drawSelfTest env (wTest w)]

draw1D :: Env -> World -> [Picture]
draw1D env w =
  [ plotAxes g "x" "y (white = true function, yellow = regression tree)" [0, 0.25, 0.5, 0.75, 1] [-1, 0, 1]
  , Color (rgbaC 255 255 255 90) (polyline 1.5 [gPt g (q / 200) (hidden1 (q / 200)) | q <- [0 .. 200]])
  -- 分割の線
  , Pictures [ Color (rgbaC 255 240 150 (60 + 120 `div` (nDepth t + 1))) (lineW 1 (gxOf g (nThr t), grY g) (gxOf g (nThr t), grY g + grH g))
             | t <- treeNodes t1, not (nLeaf t) ]
  , Color (rgbC 85 175 240) (Pictures [uncurry circleAt (gPt g (x U.! 0) y) 7 | (x, y) <- zip (V.toList (wX1 w)) (U.toList (wY1 w))])
  -- 階段（x を細かく動かして葉の値をつなぐ）
  , Color (rgbC 255 225 110) (polyline 3 stairs)
  -- ---- 分割の一覧（SSE の減り）
  , sayWrap ("root SSE " ++ fmtNum 2 (nSse t1) ++ " -> tree SSE " ++ fmtNum 2 sse ++ " (" ++ show (countLeaves t1) ++ " leaves)") px 104 pw 16 (grayC 230)
  , Pictures [ say ("depth " ++ show (nDepth t) ++ ": x <= " ++ fmtNum 3 (nThr t) ++ "  SSE " ++ fmtNum 2 (nSse t) ++ " -> " ++ fmtNum 2 (nSse (nLeft t))
                    ++ " + " ++ fmtNum 2 (nSse (nRight t)) ++ " (-" ++ fmtNum 2 (nDec t) ++ ")") px (166 + fromIntegral k * 20) 14 (grayC 200)
             | (k, t) <- zip [0 :: Int ..] (take 15 list) ]
  , if length list > 15 then say ("... and " ++ show (length list - 15) ++ " more") px (166 + 15 * 20) 14 (grayC 200) else Blank
  -- 深さと SSE のグラフ
  , plotAxes g2 "depth" "training SSE (current min points)" [0, 5, 10] [0, fromIntegral (round (grY1 g2) :: Int)]
  , Color (rgbC 255 225 110) (polyline 2 pts)
  , case drop (min 10 (wDepth w)) pts of
      (p : _) -> Color (rgbC 255 240 150) (uncurry circleAt p 10)
      [] -> Blank
  , sayWrap "Leaf value = mean, so inside one leaf the sum of squared deviations from the mean (SSE) is as small as possible." (grX g) (envH env - 76) (grW g) 14 (grayC 160) ]
  where
    g = plotG env
    t1 = wTree1 w
    stairs = foldl' addSt [] [q / 400 | q <- [0 .. 400]]
    addSt acc x = let v = gyOf g (regPredict t1 (U.singleton x))
                      px' = gxOf g x
                  in case acc of
                       ((_, ly) : _) | abs (ly - v) > 0.5 -> (px', v) : (px', ly) : acc
                       _ -> (px', v) : acc
    list = sortOn (\t -> (nDepth t, nThr t)) (filter (not . nLeaf) (treeNodes t1))
    sse = mseOf [regPredict t1 x | x <- V.toList (wX1 w)] (U.toList (wY1 w)) * fromIntegral (V.length (wX1 w))
    px = grX g + grW g + 50
    pw = envW env - px - 30
    g2 = Graph (px + 10) (envH env - 260) (pw - 30) 150 0 10 0 (max 1e-6 (let s = headOr 1 (wSseC w) in if s == 0 then 1 else s))
    pts = [gPt g2 (fromIntegral d) e | (d, e) <- zip [0 :: Int ..] (wSseC w)]

draw2D :: Env -> World -> [Picture]
draw2D env w =
  [ imageBox 40 110 side side imgN imgN (wImgT w)
  , imageBox (80 + side) 110 side side imgN imgN (wImgF w)
  , say "true function sin(5u) cos(4v) + step" 40 (105 + side + 30) 15 (grayC 220)
  , say ("regression tree (depth " ++ show (wDepth w + 3) ++ ", " ++ show (countLeaves (wTree2 w)) ++ " leaves) train MSE " ++ fmtNum 4 mse) (80 + side) (105 + side + 30) 15 (grayC 220)
  , sayWrap "Each rectangle is one constant colour. The diagonal step u + v > 1.2 can only be approximated by a staircase." (80 + side) (105 + side + 40) side 14 (grayC 160) ]
  where
    side = min ((envW env - 160) / 2) (envH env - 230)
    mse = mseOf [regPredict (wTree2 w) x | x <- V.toList (wX2 w)] (U.toList (wY2 w))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos | not (wTwoD w) ->
    let g = plotG env
        (mx, my) = toP5 env pos
        x = realToFrac ((mx - grX g) / grW g) :: Double
        yv = grY1 g - realToFrac ((my - grY g) / grH g) * (grY1 g - grY0 g)
    in if x < 0 || x > 1 || my < grY g || my > grY g + grH g then w
       else retrain w { wX1 = V.snoc (wX1 w) (U.singleton x), wY1 = U.snoc (wY1 w) yv }
  EventKey (SpecialKey KeyUp) Down _ _ -> retrain w { wDepth = min 10 (wDepth w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> retrain w { wDepth = max 0 (wDepth w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> retrain w { wMinLeaf = min 20 (wMinLeaf w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> retrain w { wMinLeaf = max 1 (wMinLeaf w - 1) }
  EventKey (Char c) Down _ _
    | c == 'v' || c == 'V' -> w { wTwoD = not (wTwoD w) }
    | c == 'r' || c == 'R' -> newData w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("どの分割でも、記録した SSE の減り = SSE(親) − SSE(左) − SSE(右)（点から計算し直した値）", okD)
  , ("どのノードの値も、そこに入った点の平均。SSE も一致", okM)
  , ("根の分割の減り = 全部の中点を総当たりした最大", near (nDec t) best 1e-9)
  , ("最後まで育てる（葉に 1 点）と訓練の SSE = 0（x がぜんぶちがうとき）", near (sseD noD) 0 1e-12)
  , ("深さを増やすと訓練の SSE は増えない（深さ 0〜8）", and (zipWith (\p q -> q <= p + 1e-9) es (drop 1 es)))
  , ("深さ d の回帰木は、たかだか 2^d 段の階段", and [S.size (S.fromList [regPredictL d full (U.singleton (q / 1000)) | q <- [0 .. 1000]]) <= 2 ^ d | d <- [0 .. 8]]) ]
  where
    (pts, _) = evalR (rngOf 1) (seedRng 71 >> mapM (\_ -> do x <- rnd; g <- rndGauss; pure (x, hidden1 x + 0.2 * g)) [1 .. 60 :: Int])
    xs = V.fromList (map (U.singleton . fst) pts)
    ys = U.fromList (map snd pts)
    yOf i = ys U.! i
    xOf i = fst (headOr (0, 0) (drop i pts))
    sseDirect ids = let m = foldl' (+) 0 (map yOf ids) / fromIntegral (max 1 (length ids)) in foldl' (+) 0 [sqr (yOf i - m) | i <- ids]
    t = growReg 4 1 Nothing xs ys
    walk q ids =
      let okM' = abs (nSse q - sseDirect ids) <= 1e-9 && abs (nValue q - foldl' (+) 0 (map yOf ids) / fromIntegral (length ids)) <= 1e-12
          l = filter (\i -> xOf i <= nThr q) ids
          r = filter (\i -> xOf i > nThr q) ids
      in if nLeaf q then (True, okM')
         else let (d1, m1) = walk (nLeft q) l
                  (d2, m2) = walk (nRight q) r
              in (abs (nDec q - (sseDirect ids - sseDirect l - sseDirect r)) <= 1e-9 && d1 && d2, okM' && m1 && m2)
    allI = [0 .. length pts - 1]
    (okD, okM) = walk t allI
    srt = sort (map fst pts)
    best = maxOr (-1) [ sseDirect allI - sseDirect (filter (\i -> xOf i <= th) allI) - sseDirect (filter (\i -> xOf i > th) allI)
                      | (a, b) <- zip srt (drop 1 srt), let th = (a + b) / 2 ]
    full = growReg 40 1 Nothing xs ys
    sseD d = foldl' (+) 0 [sqr (regPredictL d full (xs V.! i) - yOf i) | i <- allI]
    es = map sseD [0 .. 8]

-- ---------- やってみよう ----------
-- 1. ↑↓ で深さを 0 から 10 まで。深さ 0 は「全部の平均」の横一直線
-- 2. ← → で葉の最小の点の数を 1 と 15 にして比べよう。どちらがノイズをなぞる？
-- 3. 右の一覧で、最初の分割（深さ 0）の SSE の減りがいちばん大きい？ いつもそう？
-- 4. hidden1 を別の関数（たとえば x * x）に変えてみよう。段差のある関数とない関数、どちらが得意？
-- 5. 11 の勾配ブースティングは、この小さな回帰木を「残差」にあてはめて足していく

sqr :: Double -> Double                  -- 2 乗
sqr a = a * a

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

-- ---- 回帰木：葉の値 = 平均、分割は 2 乗誤差（SSE）がいちばん減るところ。SSE = Σy² − (Σy)²/n
sseOf :: Double -> Double -> Int -> Double
sseOf s s2 n = if n > 0 then max 0 (s2 - (s * s) / fromIntegral n) else 0

growReg :: Int -> Int -> Maybe (U.Vector Int) -> V.Vector Pt -> U.Vector Double -> Node   -- maxDepth, minLeaf, idx
growReg maxDepth minLeaf0 mIdx xs ys = numberTree (growRegNode xs ys maxDepth (max 1 minLeaf0) idx0 0)
  where idx0 = fromMaybe (U.enumFromN 0 (V.length xs)) mIdx

growRegNode :: V.Vector Pt -> U.Vector Double -> Int -> Int -> U.Vector Int -> Int -> Node
growRegNode xs ys maxDepth minLeaf idx depth
  | depth >= maxDepth || n < 2 * minLeaf || sse0 < 1e-12 = node
  | otherwise = case best of
      Nothing -> node
      Just (f, thr, dec) ->
        let (l, r) = U.partition (\i -> xAt xs i f <= thr) idx
            lt = growRegNode xs ys maxDepth minLeaf l (depth + 1)
            rt = growRegNode xs ys maxDepth minLeaf r (depth + 1)
        in lt `seq` rt `seq` node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }
  where
    n = U.length idx
    (s, s2) = U.foldl' (\(!a, !b) i -> let v = ys U.! i in (a + v, b + v * v)) (0, 0) idx
    sse0 = sseOf s s2 n
    node = nilNode { nLeaf = True, nValue = if n > 0 then s / fromIntegral n else 0, nN = n, nSse = sse0, nDepth = depth, nDec = 0 }
    d = if V.null xs then 0 else U.length (V.head xs)
    best = foldl' feat Nothing [0 .. d - 1]
    feat b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) (U.toList idx))
          go !k !ls !ls2 !b
            | k >= n - 1 = b
            | otherwise =
                let v = ys U.! (ord U.! k)
                    ls' = ls + v
                    ls2' = ls2 + v * v
                    v0 = xAt xs (ord U.! k) f
                    v1 = xAt xs (ord U.! (k + 1)) f
                in if not (v1 > v0) || k + 1 < minLeaf || n - k - 1 < minLeaf then go (k + 1) ls' ls2' b else
                   let dec = sse0 - sseOf ls' ls2' (k + 1) - sseOf (s - ls') (s2 - ls2') (n - k - 1)
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) ls' ls2' b'
      in go 0 0 0 b0

regPredict :: Node -> Pt -> Double
regPredict t x = nValue (leafOf t x)

regPredictL :: Int -> Node -> Pt -> Double
regPredictL dl t x = nValue (leafOfL dl negInf t x)

mseOf :: [Double] -> [Double] -> Double
mseOf pr ys = if null ys then 0 else foldl' (+) 0 (zipWith (\p y -> (p - y) * (p - y)) pr ys) / fromIntegral (length ys)

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

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gxOf :: Graph -> Double -> Float
gxOf g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gyOf :: Graph -> Double -> Float
gyOf g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gxOf g a, gyOf g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 80 100 90) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gxOf g v) (grY g + grH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gyOf g v + 6) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (grayC 205), say yl (grX g) (grY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = if length pts < 2 then Blank else thickPath w pts

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
