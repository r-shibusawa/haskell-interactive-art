-- ============================================
-- 60-13 外れ値に弱い ─ たった 1 つの変な点で、第 1 主成分が大きく回ってしまう
-- 分散は「ずれの 2 乗」の平均なので、遠い点ほど 2 乗で効く。外れ値（ピンクの ●）をドラッグして試そう
--   工夫：① 平均のかわりに中央値で中心をとる ② 中心からの距離（マハラノビス距離 d）が大きい点の重みを w = 1/(1 + (d/2)²) に下げる
--   ③ 重みつきの共分散で主成分を求め直す → ② にもどる（20 回くり返す）。点の大きさ = 重み
-- 操作：ドラッグ 外れ値 / ↑↓ 外れ値の数 / O 外れ値 ON/OFF / R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8, Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 40                        -- ★ ふつうの点の数

data World = World
  { wBase  :: [Vec]               -- ふつうの点
  , wOutl  :: [Vec]               -- 外れ値（★ はじめは (−3, 5) の 1 個）
  , wNOut  :: !Int
  , wUseOut :: !Bool
  , wSeed  :: !Int
  , wDrag  :: !Int
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-13 Weak against outliers" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sum (map U.sum (wBase w)))
      pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [vec [-3, 5]] 1 True 31 (-1) []

rebuild :: World -> World
rebuild w = w { wBase = V.toList (withSeed (wSeed w) (gaussCloud nPts (vec [0, 0]) (covFromAxes 2 0.6 30))) }

allPts :: World -> [Vec]
allPts w = if wUseOut w then wBase w ++ take (wNOut w) (wOutl w) else wBase w

-- 画面の大きさと中心
geo :: Env -> (Float, Float, Float)
geo env = (min (envW env * 0.05) (envH env * 0.06), envW env * 0.32, envH env * 0.52)

toScr :: Env -> Vec -> (Float, Float)
toScr env p = let (s, cx, cy) = geo env in (cx + f2 (vix p 0) * s, cy - f2 (vix p 1) * s)

-- 点 μ を通る向き v の直線
lineThrough :: Env -> Vec -> Vec -> Color -> Bool -> Picture
lineThrough env mu v col dash = Color col (if dash then dashLine 2.5 8 6 a b else lineW 2.5 a b)
  where a = toScr env (U.zipWith (\m q -> m - q * 9) mu v)
        b = toScr env (U.zipWith (\m q -> m + q * 9) mu v)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgbC 45 55 68) (lineW 1 (cx - 9 * s, cy) (cx + 9 * s, cy)), Color (rgbC 45 55 68) (lineW 1 (cx, cy - 7 * s) (cx, cy + 7 * s))
  , lineThrough env mu0 (e2V1 e0) (rgbC 150 160 175) True
  , lineThrough env mu (e2V1 e) (rgbC 255 125 105) False
  , lineThrough env (rbMu rb) (e2V1 (rbEg rb)) (rgbC 150 220 160) False ]
  ++ concat [ [ Color (if isOut then rgbC 255 120 200 else rgbC 225 232 242) (circleAt qx qy (4 + 10 * f2 wt)) ]
              ++ [ Color (rgbC 255 120 200) (ringAt qx qy 22 1.5) | isOut ]
            | (i, p, wt) <- zip3 [0 ..] ps (rbW rb), let (qx, qy) = toScr env p, let isOut = i >= nPts ]
  ++ [ Color (rgbC 255 125 105) (uncurry circleAt (toScr env mu) 10), Color (rgbC 150 220 160) (uncurry circleAt (toScr env (rbMu rb)) 10) ]
  -- ---- 数字
  ++ [ say "Weak against outliers" 40 44 22 (grayC 240)
     , say ("Drag outlier (pink)   Up/Down number of outliers (" ++ show (wNOut w) ++ ")   O outliers (" ++ (if wUseOut w then "ON" else "OFF") ++ ")   R new data") 40 72 15 (rgbC 165 175 190)
     , say "Dashed: PC1 without outliers (truth)" x y 15 (rgbC 150 160 175)
     , say ("Coral: plain PCA PC1, off " ++ fmtN rot 2 ++ " deg") x (y + 30) 15 (rgbC 255 125 105)
     , say ("Green: robust PC1, off " ++ fmtN rotR 2 ++ " deg") x (y + 60) 15 (rgbC 150 220 160)
     , say ("mean = " ++ fmtVec mu 2 ++ "  (no outliers " ++ fmtVec mu0 2 ++ ")") x (y + 100) 15 (grayC 220)
     , say ("weighted centre = " ++ fmtVec (rbMu rb) 2) x (y + 124) 15 (grayC 220)
     , say ("l1 = " ++ fmtN (e2L1 e) 2 ++ "  (no outliers " ++ fmtN (e2L1 e0) 2 ++ ")") x (y + 148) 15 (grayC 220) ]
  ++ [ say ("outlier weights w = " ++ joinWith ", " [ fmtN (nth 0 (rbW rb) (nPts + k)) 3 | k <- [0 .. wNOut w - 1] ]) x (y + 172) 15 (grayC 220) | wUseOut w ]
  ++ [ sayWrap ("Green = median centre, then points far away (Mahalanobis distance d) get weight w = 1/(1 + (d/2)^2), "
                ++ "and the weighted covariance is recomputed 20 times. An outlier at distance r affects the covariance like r^2. "
                ++ "The median hardly moves unless almost half the points move. This is a kind of 'M-estimation'; "
                ++ "standardising (10) with mean and SD is also weak against outliers.")
               x (y + 196) (envW env - x - 30) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    (s, cx, cy) = geo env
    ps = allPts w
    pm = V.fromList ps
    e = eig2of (covMatrix pm)
    mu = colMeans pm
    bm = V.fromList (wBase w)
    e0 = eig2of (covMatrix bm)
    mu0 = colMeans bm
    rb = robustPCA2 pm 20
    x = envW env * 0.64
    y = 140
    rot = angleBetween (e2V1 e) (e2V1 e0)
    rotR = angleBetween (e2V1 (rbEg rb)) (e2V1 e0)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hits = [ k | k <- [0 .. wNOut w - 1], wUseOut w
                   , let (qx, qy) = toScr env (nth (vec [0, 0]) (wOutl w) k), sqrt ((mx - qx) ^ (2 :: Int) + (my - qy) ^ (2 :: Int)) < 16 ]
    in pure w { wDrag = if null hits then -1 else last hits }
  EventMotion p | wDrag w >= 0 ->
    let (mx, my) = toP5 env p
        (s, cx, cy) = geo env
        q = vec [clampv (realToFrac ((mx - cx) / s)) (-9) 9, clampv (realToFrac (-(my - cy) / s)) (-7) 7]
    in pure w { wOutl = [ if k == wDrag w then q else o | (k, o) <- zip [0 ..] (wOutl w) ] }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _
    | wNOut w < 5 -> let n = wNOut w + 1
                         o0 = nth (vec [0, 0]) (wOutl w) 0
                         extra = [ vec [vix o0 0 + 0.4 * fromIntegral n, vix o0 1 - 0.3 * fromIntegral n] | length (wOutl w) < n ]
                     in pure w { wNOut = n, wOutl = wOutl w ++ extra }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wNOut = max 1 (wNOut w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> pure w { wUseOut = not (wUseOut w), wDrag = -1 }
    | c `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("one outlier (about 14 from the centre) turns plain PC1 by more than 20 deg", angleBetween v0 v1 > 20)
  , ("median + down-weighting keeps it within 3 deg", angleBetween v0 (e2V1 (rbEg rb)) < 3)
  , ("without outliers, robust PC1 ~ plain PC1 (within 3 deg)", angleBetween v0 (e2V1 (rbEg rb0)) < 3)
  , ("outlier weight < 0.05, median weight of normal points >= 0.5", nth 1 (rbW rb) nPts < 0.05 && medianOf (take nPts (rbW rb)) > 0.5)
  , ("all weights 1: weighted covariance = covariance x (n - 1)/n",
      maxAbsDiff (weightedCov x (replicate nPts 1) (colMeans x)) (V.map (U.map (\v -> v * fromIntegral (nPts - 1) / fromIntegral nPts)) (covMatrix x)) < 1e-12)
  , ("an outlier moves the mean a lot (> 0.3) but the median hardly (< 0.15)", abs mxd > 0.3 && abs md < 0.15)
  , ("identical points or only 2 points give no NaN",
      noNaN (U.toList (e2V1 (rbEg (robustPCA2 (mat [[1, 1], [1, 1], [1, 1]]) 5)))) && noNaN (U.toList (e2V1 (rbEg (robustPCA2 (mat [[0, 0], [1, 2]]) 5))))) ]
  where
    x = withSeed 31 (gaussCloud nPts (vec [0, 0]) (covFromAxes 2 0.6 30))
    xo = V.snoc x (vec [-2, 14])
    v0 = mrow (eVecs (jacobiEigen (covMatrix x))) 0
    v1 = mrow (eVecs (jacobiEigen (covMatrix xo))) 0
    rb = robustPCA2 xo 30
    rb0 = robustPCA2 x 30
    mxd = vix (colMeans xo) 1 - vix (colMeans x) 1
    md = medianOf (map (`vix` 1) (V.toList xo)) - medianOf (map (`vix` 1) (V.toList x))

-- ---------- やってみよう ----------
-- 1. 外れ値を楕円の長い軸の上に置くと、PC1 は回る？ 短い軸の上なら？
-- 2. ↑ で外れ値を 5 個にすると、緑の線も負けてしまう？ どのくらいから？
-- 3. 外れ値を中心に近づけていくと、重み w はどう変わる？
-- 4. robustPCA2 の (d/2)² の 2 を 1 や 4 にすると？（小さいほど厳しく重みを下げる）
-- 5. 本物のデータでは、外れ値は「入力のまちがい」か「大事な発見」か。消す前に見てみよう

-- ==================================================
-- ここから下は「主成分分析の道具」（中身は読まなくても OK。消さないでね）
-- 約束：データ X は「n 行 × d 列」（1 行 = 1 個のデータ）。共分散は n − 1 で割る（C = Xcᵀ Xc / (n − 1)）
-- ベクトル Vec = Double の並び（unboxed）、行列 Mat = Vec（行）の並び。計算は Double、描くときだけ Float にする
-- ==================================================
-- ---- 数の表示と安全のための道具
clampv :: Double -> Double -> Double -> Double      -- clampv v a b：v を a〜b に（p5.js 版と同じ順）
clampv v a b = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double                   -- NaN・∞ を fb に
fin v fb = if isNaN v || isInfinite v then fb else v

f2 :: Double -> Float                               -- 描くときの Float（NaN は 0）
f2 v = realToFrac (fin v 0)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 数を小数 d 桁で（NaN は「?」、"-0.00" は "0.00"）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v || isInfinite v = "?"
  | otherwise = let s = showFFloat (Just d) v ""
                in if take 1 s == "-" && all (`elem` "0.") (drop 1 s) then drop 1 s else s

fmtVec :: Vec -> Int -> String
fmtVec v d = "(" ++ joinWith ", " (map (`fmtN` d) (U.toList v)) ++ ")"

-- 指数の形で小数 d 桁（toExponential）
fmtE :: Double -> Int -> String
fmtE v d = if isNaN v || isInfinite v then "?" else showEFloat (Just d) v ""

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin v0 0

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Integer)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- リストの k 番目（なければ d）
nth :: a -> [a] -> Int -> a
nth d xs k = if k < 0 then d else case drop k xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Int -> Rng                               -- (s >>> 0) || 1
seedRng s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- seedRng(s) してから計算し、結果だけ取り出す
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double                                     -- 0〜1 の一様乱数
rnd = R rndStep

rndRange :: Double -> Double -> R Double            -- a〜b の一様乱数
rndRange a b = do u <- rnd; pure (a + (b - a) * u)

gaussRnd :: R Double                                -- 標準正規分布（ボックス-ミュラー法。u, v の順に 2 つ引く）
gaussRnd = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- 行列の道具。データ X は「n 行 × d 列」（1 行 = 1 個のデータ）
type Vec = U.Vector Double
type Mat = V.Vector Vec

vec :: [Double] -> Vec
vec = U.fromList

mat :: [[Double]] -> Mat
mat = V.fromList . map U.fromList

vix :: Vec -> Int -> Double                         -- 成分（範囲の外は 0）
vix v i = maybe 0 id (v U.!? i)

mrow :: Mat -> Int -> Vec                           -- 行（範囲の外は空）
mrow m i = maybe U.empty id (m V.!? i)

mix :: Mat -> Int -> Int -> Double                  -- (i, j) 成分
mix m i j = vix (mrow m i) j

nRows, nCols :: Mat -> Int
nRows = V.length
nCols m = U.length (mrow m 0)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = V.generate n (\i -> U.generate n (\j -> if i == j then 1 else 0))

matT :: Mat -> Mat
matT a = V.generate (nCols a) (\j -> U.generate (nRows a) (\i -> mix a i j))

matMul :: Mat -> Mat -> Mat
matMul a b = V.map row a
  where m = nCols b
        row r = U.ifoldl' (\acc k x -> if x == 0 then acc else U.zipWith (\c bk -> c + x * bk) acc (mrow b k)) (U.replicate m 0) r

matVec :: Mat -> Vec -> Vec
matVec a x = V.convert (V.map (`dotv` x) a)

dotv :: Vec -> Vec -> Double                       -- 内積（左から順に足す）
dotv a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

vlen :: Vec -> Double
vlen a = sqrt (dotv a a)

unitv :: Vec -> Vec                                 -- 長さ 1 に（0 ベクトルは (1, 0, …)）
unitv a = let l = vlen a in if l > 1e-300 then U.map (/ l) a else U.imap (\i _ -> if i == 0 then 1 else 0) a

scaleV :: Double -> Vec -> Vec
scaleV k = U.map (k *)

addV, subV :: Vec -> Vec -> Vec
addV = U.zipWith (+)
subV = U.zipWith (-)

maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = V.foldl' max 0 (V.zipWith (\r s -> U.foldl' max 0 (U.zipWith (\x y -> abs (x - y)) r s)) a b)

colMeans :: Mat -> Vec                              -- 平均ベクトル（列ごとの平均）
colMeans x = U.map (/ fromIntegral (max 1 (nRows x))) (V.foldl' addV (U.replicate (nCols x) 0) x)

centerCols :: Mat -> Vec -> Mat                     -- 平均を引く（中心化）
centerCols x mu = V.map (`subV` mu) x

-- ★ 共分散行列 C = Xcᵀ Xc / (n − 1)（n − 1 で割る「不偏」の約束。点が 1 個以下ならゼロ行列）
covMatrix :: Mat -> Mat
covMatrix x
  | n < 2 = zerosMat d d
  | otherwise = toMat d $ runST $ do
      c <- UM.replicate (d * d) 0
      V.forM_ x $ \r -> forM_ [0 .. d - 1] $ \i -> do
        let a = vix r i - vix mu i
        forM_ [i .. d - 1] $ \j -> UM.modify c (+ a * (vix r j - vix mu j)) (i * d + j)
      forM_ [0 .. d - 1] $ \i -> forM_ [i .. d - 1] $ \j -> do
        v <- UM.read c (i * d + j)
        let v' = v / fromIntegral (n - 1)
        UM.write c (i * d + j) v'
        UM.write c (j * d + i) v'
      U.freeze c
  where n = nRows x
        d = nCols x
        mu = colMeans x

-- 長さ d·d の並びを d×d の行列に
toMat :: Int -> Vec -> Mat
toMat d flat = V.generate (if d > 0 then U.length flat `div` d else 0) (\i -> U.slice (i * d) d flat)

-- 固有値分解の結果：vals = 固有値（大きい順）、vecs の k 行目 = k 番目の固有ベクトル
data Eig = Eig { eVals :: !Vec, eVecs :: !Mat }

-- ★ ヤコビ法（対称行列の固有値分解）：非対角を 1 つずつ回転で 0 にしていく。d ≤ 64 くらいまで
-- 固有ベクトルは長さ 1、いちばん大きい成分を正に
jacobiEigen :: Mat -> Eig
jacobiEigen = jacobiEigenN 80

jacobiEigenN :: Int -> Mat -> Eig
jacobiEigenN maxSweeps c = Eig (U.fromList (map (\k -> vix diag k) idx)) (V.fromList (map vecOf idx))
  where
    d = nRows c
    (af, vf) = runST $ do
      a <- U.thaw (U.concat (V.toList (V.map (\r -> U.generate d (vix r)) c)))
      v <- U.thaw (U.generate (d * d) (\k -> if k `div` d == k `mod` d then 1 else 0))
      let rd i j = UM.unsafeRead a (i * d + j)
          wr i j x = UM.unsafeWrite a (i * d + j) x
          sweep sw = when (sw < maxSweeps) $ do
            (off, tot) <- foldM (\(!o, !t) k -> do
                                   x <- UM.unsafeRead a k
                                   let s = x * x
                                   pure (if k `div` d /= k `mod` d then o + s else o, t + s)) (0, 0) [0 .. d * d - 1]
            when (off > 1e-30 * tot) $ do
              forM_ [0 .. d - 2] $ \p -> forM_ [p + 1 .. d - 1] $ \q -> do
                apq <- rd p q
                when (abs apq >= 1e-300) $ do
                  app <- rd p p
                  aqq <- rd q q
                  let th = (aqq - app) / (2 * apq)
                      t = if th == 0 then 1 else signum th / (abs th + sqrt (th * th + 1))
                      cs = 1 / sqrt (t * t + 1)
                      sn = t * cs
                  forM_ [0 .. d - 1] $ \k -> do
                    akp <- rd k p
                    akq <- rd k q
                    wr k p (cs * akp - sn * akq)
                    wr k q (sn * akp + cs * akq)
                  forM_ [0 .. d - 1] $ \k -> do
                    apk <- rd p k
                    aqk <- rd q k
                    wr p k (cs * apk - sn * aqk)
                    wr q k (sn * apk + cs * aqk)
                  forM_ [0 .. d - 1] $ \k -> do
                    vkp <- UM.unsafeRead v (k * d + p)
                    vkq <- UM.unsafeRead v (k * d + q)
                    UM.unsafeWrite v (k * d + p) (cs * vkp - sn * vkq)
                    UM.unsafeWrite v (k * d + q) (sn * vkp + cs * vkq)
              sweep (sw + 1)
      sweep 0
      af' <- U.freeze a
      vf' <- U.freeze v
      pure (af', vf')
    diag = U.generate d (\i -> vix af (i * d + i))
    idx = sortBy (\x y -> compare (vix diag y) (vix diag x)) [0 .. d - 1]
    vecOf k = fixSign (U.generate d (\i -> vix vf (i * d + k)))

-- いちばん大きい成分（同じくらいなら前のほう）を正にそろえる
fixSign :: Vec -> Vec
fixSign v = let big = U.ifoldl' (\b i x -> if abs x > abs (vix v b) + 1e-12 then i else b) 0 v
            in if vix v big < 0 then U.map negate v else v

traceOf :: Mat -> Double
traceOf c = sum [ mix c i i | i <- [0 .. nRows c - 1] ]

-- C V = V Λ の残り（いちばん大きいずれ）
eigResidual :: Mat -> Eig -> Double
eigResidual c eg = foldl' max 0 [ abs (vix cv i - vix (eVals eg) k * vix v i)
                                | (k, v) <- zip [0 ..] (V.toList (eVecs eg)), let cv = matVec c v, i <- [0 .. U.length v - 1] ]

-- 固有ベクトルどうしの直交のずれ
orthoError :: Mat -> Double
orthoError vs = foldl' max 0 [ abs (dotv a b - (if i == j then 1 else 0)) | (i, a) <- zip [0 :: Int ..] l, (j, b) <- zip [0 ..] l ]
  where l = V.toList vs

-- 射影（主成分得点）：score_k = (x − μ)·v_k（k 個まで）
projectData :: Mat -> Vec -> Mat -> Int -> Mat
projectData x mu vecs k = V.map (\r -> V.convert (V.map (\v -> U.sum (U.zipWith3 (\a m b -> (a - m) * b) r mu v)) (V.take k vecs))) x

-- 復元：x_hat = μ + Σ score_k v_k
reconstructRow :: Vec -> Vec -> Mat -> Vec
reconstructRow score mu vecs = U.ifoldl' (\x k s -> U.zipWith (\a b -> a + s * b) x (mrow vecs k)) mu score

-- 向きのちがい（符号は無視、0〜90°）。atan2(‖v − (v·u)u‖, |v·u|)
angleBetween :: Vec -> Vec -> Double
angleBetween u v = let a = unitv u
                       b = unitv v
                       c = dotv a b
                       r = U.sum (U.zipWith (\ai bi -> (bi - c * ai) ^ (2 :: Int)) a b)
                   in atan2 (sqrt r) (abs c) * 180 / pi

-- 相関係数
corrOf :: [Double] -> [Double] -> Double
corrOf a b = if saa > 0 && sbb > 0 then sab / sqrt (saa * sbb) else 0
  where n = fromIntegral (length a)
        ma = foldl' (+) 0 a / n
        mb = foldl' (+) 0 b / n
        (sab, saa, sbb) = foldl' (\(!p, !q, !r) (x, y) -> (p + (x - ma) * (y - mb), q + (x - ma) ^ (2 :: Int), r + (y - mb) ^ (2 :: Int))) (0, 0, 0) (zip a b)

-- 計算を全部すませる（起動や作り直しのあと、最初のコマが重くならないように）
sumMat :: Mat -> Double
sumMat = V.foldl' (\s r -> s + U.sum r) 0
-- ---- 2×2 対称行列 [[a, b], [b, c]] の固有値・固有ベクトル（解の公式）
data E2 = E2 { e2L1, e2L2 :: !Double, e2V1, e2V2 :: !Vec, e2Angle :: !Double }

eig2sym :: Double -> Double -> Double -> E2
eig2sym a b c = E2 l1 l2 v1 (vec [-(vix v1 1), vix v1 0]) (atan2 (vix v1 1) (vix v1 0) * 180 / pi)
  where
    tr = a + c
    h = sqrt (((a - c) / 2) ^ (2 :: Int) + b * b)
    l1 = tr / 2 + h
    l2 = tr / 2 - h
    v0 = if abs b > 1e-14 * (abs a + abs c + 1e-300) then unitv (vec [l1 - c, b]) else if a >= c then vec [1, 0] else vec [0, 1]
    v1 = if vix v0 0 < 0 || (vix v0 0 == 0 && vix v0 1 < 0) then U.map negate v0 else v0

-- 2×2 の行列 C の固有分解
eig2of :: Mat -> E2
eig2of c = eig2sym (mix c 0 0) (mix c 0 1) (mix c 1 1)
-- ---- 好きな共分散の正規分布の点群：x = μ + L z（C = L Lᵀ、コレスキー分解）
cholesky :: Mat -> Mat
cholesky c = mat (map (\r -> r ++ replicate (d - length r) 0) rows)
  where
    d = nRows c
    rows = foldl' (\done i -> done ++ [row done i]) [] [0 .. d - 1]
    row done i = foldl' (\cur j -> cur ++ [entry done cur i j]) [] [0 .. i]
    entry done cur i j =
      let lj = if j == i then cur else nth [] done j
          s = foldl' (\acc k -> acc - nth 0 cur k * nth 0 lj k) (mix c i j) [0 .. j - 1]
      in if i == j then sqrt (max s 1e-15) else s / nth 0 lj j

gaussCloud :: Int -> Vec -> Mat -> R Mat
gaussCloud n mu c = V.replicateM n $ do
  z <- U.replicateM (U.length mu) gaussRnd
  pure (U.imap (\i m -> m + dotv (mrow l i) z) mu)
  where l = cholesky c

-- 長軸の標準偏差 s1、短軸 s2、長軸の角度 deg（度）の 2×2 共分散
covFromAxes :: Double -> Double -> Double -> Mat
covFromAxes s1 s2 deg = mat [[c * c * s1 * s1 + s * s * s2 * s2, c * s * (s1 * s1 - s2 * s2)], [c * s * (s1 * s1 - s2 * s2), s * s * s1 * s1 + c * c * s2 * s2]]
  where t = deg * pi / 180
        c = cos t
        s = sin t
-- ---- 外れ値に強くする工夫：中央値で中心化 → 距離が大きい点ほど重みを下げてくり返す（重みつき共分散）
medianOf :: [Double] -> Double
medianOf a = let s = sortBy compare a
                 n = length s
                 m = n `div` 2
             in if n == 0 then 0 else if odd n then nth 0 s m else (nth 0 s (m - 1) + nth 0 s m) / 2

weightedCov :: Mat -> [Double] -> Vec -> Mat
weightedCov x w mu = V.map (U.map (/ max ws 1e-12)) c
  where
    d = U.length mu
    ws = foldl' (+) 0 (zipWith const w (V.toList x))
    c = mat [ [ foldl' (+) 0 [ wn * (vix r i - vix mu i) * (vix r j - vix mu j) | (r, wn) <- zip (V.toList x) w ] | j <- [0 .. d - 1] ] | i <- [0 .. d - 1] ]

data Robust = Robust { rbMu :: !Vec, rbC :: !Mat, rbW :: [Double], rbEg :: !E2 }

-- 2 次元用。マハラノビス距離 d で重み w = 1 / (1 + (d/2)²)
robustPCA2 :: Mat -> Int -> Robust
robustPCA2 x iters = go 0 mu0 w0 (weightedCov x w0 mu0)
  where
    rows = V.toList x
    mu0 = vec [medianOf (map (`vix` 0) rows), medianOf (map (`vix` 1) rows)]
    w0 = map (const 1) rows
    go it mu w c
      | it >= iters = Robust mu c w (eig2of c)
      | otherwise =
          let e = eig2of c
              l1 = max (e2L1 e) 1e-9
              l2 = max (e2L2 e) 1e-9
              w' = [ 1 / (1 + (dd / 2) ^ (2 :: Int))
                   | r <- rows
                   , let a = (vix r 0 - vix mu 0) * vix (e2V1 e) 0 + (vix r 1 - vix mu 1) * vix (e2V1 e) 1
                   , let b = (vix r 0 - vix mu 0) * vix (e2V2 e) 0 + (vix r 1 - vix mu 1) * vix (e2V2 e) 1
                   , let dd = sqrt (a * a / l1 + b * b / l2) ]
              ws = foldl' (+) 0 w'
              mu' = vec [ foldl' (+) 0 (zipWith (\r wn -> wn * vix r j) rows w') / ws | j <- [0, 1] ]
          in go (it + 1) mu' w' (weightedCov x w' mu')
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
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 破線（線 dash、すき間 gap）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine wt dash gap (xa, ya) (xb, yb) = Pictures [ thickLine wt (at' t0) (at' (min l (t0 + dash))) | t0 <- takeWhile (< l) [0, dash + gap ..] ]
  where l = sqrt ((xb - xa) ^ (2 :: Int) + (yb - ya) ^ (2 :: Int))
        at' t = if l <= 0 then (xa, ya) else (xa + (xb - xa) * t / l, ya + (yb - ya) * t / l)

-- 幅 w × 高さ h の画素の絵を k 倍にして、左上が (x, y) になるように置く（f x y は左上が (0, 0)）
imageTL :: Float -> Float -> Int -> Int -> Float -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
imageTL x y w h k f = Translate (x + fromIntegral w * k / 2) (y + fromIntegral h * k / 2) (Scale 1 (-1) (pixelImage w h k f))

-- 0〜255 に丸めた画素の値
byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round (fin v 0) :: Int)))
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any (\v -> isNaN v || isInfinite v)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs && not (null rs) then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

selfTestOk :: [(String, Bool)] -> Bool
selfTestOk rs = not (null rs) && all snd rs

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if selfTestOk rs then rgbC 120 210 170 else rgbC 255 90 90)
