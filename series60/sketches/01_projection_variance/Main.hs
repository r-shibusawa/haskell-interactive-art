-- ============================================
-- 60-01 射影した分散が最大の向き ─ 直線を回して、点の「影」がいちばんばらつく向きを探す
-- 向き u（長さ 1）への射影 = (x − μ)·u。その分散は uᵀ C u（C = 共分散行列）→ 角度の関数としてグラフに
--   最大の向き = 第 1 主成分（PC1）、最小の向き = 第 2 主成分（PC2）。2 つはいつも直角
--   PCA の直線は「直線までの垂直な距離²の和」が最小。最小 2 乗の回帰直線は「縦の距離²の和」が最小（シリーズ 17 の 15）
-- 操作：ドラッグ 直線の向き / ← → 1° ずつ / P いちばんの向きへ / L 回帰直線と比べる / ↑↓ 細さ / R 新しい点群
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
nPts = 60                        -- ★ 点の数

data World = World
  { wPts   :: !Mat                -- 点
  , wTheta :: !Double             -- 直線の向き（ラジアン）
  , wSeed  :: !Int
  , wThin  :: !Double             -- ★ 短い軸 / 長い軸
  , wLS    :: !Bool               -- 回帰直線を見せるか
  , wC     :: !Mat                -- 共分散行列
  , wMu    :: !Vec                -- 平均
  , wCurve :: [Double]            -- 0〜180° を 0.5° きざみの射影の分散
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-01 Direction of maximum projected variance" (newCloud world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = newCloud world0 { wTest = selfTest }
      _ <- evaluate (sum (wCurve w) + sumMat (wPts w))
      pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World V.empty 0 3 0.45 True (zerosMat 2 2) (vec [0, 0]) [] []

newCloud :: World -> World
newCloud w = w { wPts = ps, wC = c, wMu = colMeans ps, wCurve = [ projVar c (fromIntegral i * pi / 360) | i <- [0 .. 360 :: Int] ] }
  where
    ps = withSeed (wSeed w) $ do
      u <- rnd
      gaussCloud nPts (vec [0, 0]) (covFromAxes 2 (2 * wThin w) (25 + 40 * u))
    c = covMatrix ps

-- ★ 向き t（ラジアン）への射影の分散 = uᵀ C u
projVar :: Mat -> Double -> Double
projVar c t = let u = vec [cos t, sin t] in dotv u (matVec c u)

-- 最小 2 乗（y を x で予測）：傾き b = σxy / σx²、切片 a
lsLine :: Mat -> Vec -> (Double, Double)
lsLine c mu = let b = if mix c 0 0 > 1e-12 then mix c 0 1 / mix c 0 0 else 0 in (b, vix mu 1 - b * vix mu 0)

-- 直線（点 mu を通り向き t）までの距離²の和：(垂直, 縦)
residSS :: Mat -> Vec -> Double -> (Double, Double)
residSS ps mu t = V.foldl' step (0, 0) ps
  where
    u0 = cos t
    u1 = sin t
    step (!perp, !vert) p =
      let dx = vix p 0 - vix mu 0
          dy = vix p 1 - vix mu 1
          c = dx * u0 + dy * u1
      in (perp + (dx * dx + dy * dy - c * c), vert + (if abs u0 > 1e-9 then (dy - (u1 / u0) * dx) ^ (2 :: Int) else 1 / 0))

-- JavaScript の % （符号は左の数と同じ）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

layout :: Env -> (Double, Double, Double)        -- (s, cx, cy)
layout env = (min (w * 0.055) (h * 0.085), w * 0.27, h * 0.53)
  where w = realToFrac (envW env)
        h = realToFrac (envH env)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 点群・直線・射影
  [ Color (rgbC 45 55 68) (lineW 1 (scr (cx - 5 * s, cy)) (scr (cx + 5 * s, cy)))
  , Color (rgbC 45 55 68) (lineW 1 (scr (cx, cy - 5 * s)) (scr (cx, cy + 5 * s)))
  , Color (rgbC 255 225 140) (lineW 2.5 (scr (mx - u0 * 4.6 * s, my + u1 * 4.6 * s)) (scr (mx + u0 * 4.6 * s, my - u1 * 4.6 * s))) ]
  ++ concat [ [ Color (rgbaC 255 225 140 90) (lineW 1 q f), Color (rgbC 255 225 140) (circleAt (fst f) (snd f) 5), Color (rgbC 225 232 242) (circleAt (fst q) (snd q) 8) ]
            | p <- V.toList (wPts w)
            , let c = (vix p 0 - vix mu 0) * u0 + (vix p 1 - vix mu 1) * u1
            , let f = toS (vix mu 0 + c * u0) (vix mu 1 + c * u1)
            , let q = toS (vix p 0) (vix p 1) ]
  ++ [ Color (rgbC 255 125 105) (dashLine 2 7 6 (toS (-5) (la - 5 * lb)) (toS 5 (la + 5 * lb))) | wLS w ]
  ++ [ Color (rgbC 90 165 240) (lineW 2 (scr (mx, my)) (scr (mx + vix (e2V1 e) 0 * 2 * sqrt (e2L1 e) * s, my - vix (e2V1 e) 1 * 2 * sqrt (e2L1 e) * s)))
     , Color (rgbC 90 165 240) (lineW 2 (scr (mx, my)) (scr (mx + vix (e2V2 e) 0 * 2 * sqrt (max 0 (e2L2 e)) * s, my - vix (e2V2 e) 1 * 2 * sqrt (max 0 (e2L2 e)) * s))) ]
  -- ---- 角度と分散のグラフ
  ++ [ plotAxes g "line angle theta (deg)" "" [0, 45, 90, 135, 180] [0, read2 (vmax / 2), read2 vmax]
     , say "projected variance u^T C u" (pX g) (pY g - 26) 14 (rgbC 205 212 222)
     , Color (rgbC 90 165 240) (polyline 2.5 [ (gX g (fromIntegral i / 2), gY g v) | (i, v) <- zip [0 :: Int ..] (wCurve w) ])
     , Color (rgbaC 90 165 240 120) (lineW 1 (gX g d1, pY g) (gX g d1, pY g + pH g))
     , Color (rgbaC 90 165 240 120) (lineW 1 (gX g d2, pY g) (gX g d2, pY g + pH g))
     , Color (rgbC 255 225 140) (circleAt (gX g deg) (gY g (projVar (wC w) (wTheta w))) 12)
     , sayC ("PC1 " ++ fmtN d1 1 ++ " deg") (gX g d1) (pY g - 4) 14 (rgbC 90 165 240)
     , sayC ("PC2 " ++ fmtN d2 1 ++ " deg") (gX g d2) (pY g + 16) 14 (rgbC 90 165 240) ]
  -- ---- 数字
  ++ [ say "Direction of maximum projected variance" 40 44 22 (grayC 240)
     , say ("Drag: line direction   <- -> 1 deg   P to PC1   L regression line (" ++ (if wLS w then "ON" else "OFF") ++ ")   Up/Down thinness (" ++ fmtN (wThin w) 2 ++ ")   R new cloud") 40 72 15 (rgbC 165 175 190)
     , say ("Line theta = " ++ fmtN deg 1 ++ " deg   variance = " ++ fmtN (projVar (wC w) (wTheta w)) 3) tx ty 17 (rgbC 255 225 140)
     , say ("max l1 = " ++ fmtN (e2L1 e) 3 ++ " (PC1)   min l2 = " ++ fmtN (e2L2 e) 3 ++ " (PC2)") tx (ty + 28) 17 (rgbC 90 165 240)
     , say ("l1 + l2 = " ++ fmtN (e2L1 e + e2L2 e) 3 ++ " = sx^2 + sy^2 (any 2 perpendicular directions)") tx (ty + 50) 14 (rgbC 165 175 190)
     , say "Sum of squared distances" tx (ty + 80) 15 (grayC 220), say "perp" (tx + 225) (ty + 80) 15 (grayC 220), say "vertical" (tx + 310) (ty + 80) 15 (grayC 220)
     , say "current line" tx (ty + 104) 15 (grayC 220), say (fmtN (fst rs) 2) (tx + 225) (ty + 104) 15 (grayC 220), say (fmtN (snd rs) 2) (tx + 310) (ty + 104) 15 (grayC 220)
     , say "PCA line (PC1)" tx (ty + 128) 15 (rgbC 90 165 240), say (fmtN (fst rp) 2) (tx + 225) (ty + 128) 15 (rgbC 90 165 240), say (fmtN (snd rp) 2) (tx + 310) (ty + 128) 15 (rgbC 90 165 240)
     , say "<- perp is min" (tx + 395) (ty + 128) 15 (rgbC 90 165 240)
     , say "regression (dashed)" tx (ty + 152) 15 (rgbC 255 125 105), say (fmtN (fst rl) 2) (tx + 225) (ty + 152) 15 (rgbC 255 125 105), say (fmtN (snd rl) 2) (tx + 310) (ty + 152) 15 (rgbC 255 125 105)
     , say "<- vertical min" (tx + 395) (ty + 152) 15 (rgbC 255 125 105)
     , sayWrap ("Regression predicts y from x, so it looks only at vertical gaps (series 17-15). PCA treats x and y equally and minimises perpendicular gaps. Slope: regression "
                ++ fmtN lb 3 ++ ", PCA " ++ fmtN (if abs (vix (e2V1 e) 0) > 1e-9 then vix (e2V1 e) 1 / vix (e2V1 e) 0 else 1 / 0) 3)
               tx (ty + 166) (pW g) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    (s, cx, cy) = layout env
    scr (a, b) = (f2 a, f2 b)
    mu = wMu w
    toS a b = (f2 (cx + a * s), f2 (cy - b * s))
    u0 = cos (wTheta w)
    u1 = sin (wTheta w)
    mx = cx + vix mu 0 * s
    my = cy - vix mu 1 * s
    e = eig2of (wC w)
    (lb, la) = lsLine (wC w) mu
    deg = jsMod (jsMod (wTheta w * 180 / pi) 180 + 180) 180
    vmax = foldl' max 0 (wCurve w) * 1.1
    read2 v = jsRound (v * 100) / 100
    g = G (envW env * 0.56) 130 (envW env * 0.4) (envH env * 0.36) 0 180 0 vmax
    d1 = jsMod (jsMod (e2Angle e) 180 + 180) 180
    d2 = jsMod (d1 + 90) 180
    rs = residSS (wPts w) mu (wTheta w)
    rp = residSS (wPts w) mu (d1 * pi / 180)
    rl = residSS (wPts w) (vec [0, la]) (atan lb)
    tx = pX g
    ty = pY g + pH g + 66

setFromMouse :: Env -> World -> (Float, Float) -> World
setFromMouse env w (x, y) = if hypot dx dy > 5 then w { wTheta = atan2 dy dx } else w
  where (s, cx, cy) = layout env
        dx = realToFrac x - (cx + vix (wMu w) 0 * s)
        dy = -(realToFrac y - (cy - vix (wMu w) 1 * s))

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p | left p -> pure (setFromMouse env w (toP5 env p))
  EventMotion p | envDown env && left p -> pure (setFromMouse env w (toP5 env p))
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wTheta = wTheta w - pi / 180 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wTheta = wTheta w + pi / 180 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (newCloud w { wThin = clampv (wThin w + 0.05) 0.05 1 })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (newCloud w { wThin = clampv (wThin w - 0.05) 0.05 1 })
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> pure w { wTheta = e2Angle (eig2of (wC w)) * pi / 180 }
    | c `elem` "lL" -> pure w { wLS = not (wLS w) }
    | c `elem` "rR" -> pure (newCloud w { wSeed = wSeed w + 1 })
  _ -> pure w
  where left p = fst (toP5 env p) < envW env * 0.52

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種 101・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("brute force over 3600 directions: max angle = 1st eigenvector angle (within 0.1 deg), min = that + 90", allOf 0)
  , ("max projected variance = l1, min = l2", allOf 1)
  , ("u^T C u = projected values squared and divided by (n - 1)", allOf 2)
  , ("variances of two perpendicular directions add up to the trace sx^2 + sy^2", allOf 3)
  , ("PCA line: sum of squared perpendicular distances = (n - 1) l2, smaller than all 180 directions", allOf 4)
  , ("regression line has the smallest vertical sum (shifting slope or intercept increases it)", allOf 5)
  , ("positive correlation: regression slope <= PCA slope", allOf 6) ]
  where
    rs = withSeed 101 (mapM (const trial) [0 .. 19 :: Int])
    allOf k = all (\r -> nth False r k) rs
    dAng a b = let d = jsMod (abs (a - b)) 180 in min d (180 - d)
    trial = do
      m0 <- rndRange (-1) 1
      m1 <- rndRange (-1) 1
      a1 <- rndRange 1 2.5
      a2 <- rndRange 0.2 0.9
      a3 <- rndRange 0 180
      ps <- gaussCloud 40 (vec [m0, m1]) (covFromAxes a1 a2 a3)
      th <- rndRange 0 pi
      let c = covMatrix ps
          mu = colMeans ps
          e = eig2of c
          n = fromIntegral (nRows ps)
          vs = [ (i, projVar c (fromIntegral i * pi / 3600)) | i <- [0 .. 3599 :: Int] ]
          (bi, bv) = foldl' (\(bi', bv') (i, v) -> if v > bv' then (i, v) else (bi', bv')) (0, -1) vs
          (mi, mv) = foldl' (\(mi', mv') (i, v) -> if v < mv' then (i, v) else (mi', mv')) (0, 1 / 0) vs
          okMax = not (dAng (fromIntegral bi / 20) (e2Angle e) > 0.1 || dAng (fromIntegral mi / 20) (e2Angle e + 90) > 0.1)
          okVal = near bv (e2L1 e) (1e-6 * e2L1 e) && near mv (e2L2 e) (1e-6 * e2L1 e)
          pr = [ (vix p 0 - vix mu 0) * cos th + (vix p 1 - vix mu 1) * sin th | p <- V.toList ps ]
          okDirect = near (foldl' (\s' v -> s' + v * v) 0 pr / (n - 1)) (projVar c th) 1e-9
          okTr = near (projVar c th + projVar c (th + pi / 2)) (mix c 0 0 + mix c 1 1) 1e-9
          pc = fst (residSS ps mu (e2Angle e * pi / 180))
          okPerp = near pc ((n - 1) * e2L2 e) 1e-8 && not (any (\i -> fst (residSS ps mu (fromIntegral i * pi / 180)) < pc - 1e-9) [0 .. 179 :: Int])
          (lb, la) = lsLine c mu
          vLS = snd (residSS ps (vec [0, la]) (atan lb))
          dbs = takeWhile (<= 0.5) (iterate (+ 0.05) (-0.5))
          okLS = not (any (\db -> snd (residSS ps (vec [0, la + 0.01]) (atan (lb + db))) < vLS - 1e-9) dbs)
          okSlope = not (mix c 0 1 > 0 && not (lb <= vix (e2V1 e) 1 / vix (e2V1 e) 0 + 1e-12))
      pure [okMax, okVal, okDirect, okTr, okPerp, okLS, okSlope]

-- ---------- やってみよう ----------
-- 1. 直線を回しながら、黄色い点（影）の広がりとグラフの山を見比べよう。山と谷はなぜ 90° 離れている？
-- 2. ↑ で丸い点群（細さ 1）にすると、グラフはどうなる？ PC1 の向きは決まる？
-- 3. 回帰直線（破線）と PCA の直線は、細い点群ほど近づく？ 丸いほど離れる？
-- 4. x と y を入れかえて回帰すると別の直線になるが、PCA は入れかえても同じ直線。なぜ？
-- 5. グラフの曲線は a + b·cos(2θ − φ) の形。02 の固有値 λ1, λ2 で a と b を書いてみよう

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
-- ---- グラフと図の道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 80 92 108) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 160 170 185) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 160 170 185) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 212 222), say yl (pX g) (pY g - 8) 14 (rgbC 205 212 222) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 共分散 C の「k σ の楕円」の点（データの単位）。中心 μ
covEllipsePts :: Vec -> Mat -> Double -> Int -> [(Double, Double)]
covEllipsePts mu c k segs =
  [ (vix mu 0 + u * vix (e2V1 e) 0 + w * vix (e2V2 e) 0, vix mu 1 + u * vix (e2V1 e) 1 + w * vix (e2V2 e) 1)
  | i <- [0 .. segs], let t = 2 * pi * fromIntegral i / fromIntegral segs, let u = a * cos t, let w = b * sin t ]
  where e = eig2of c
        a = k * sqrt (max 0 (e2L1 e))
        b = k * sqrt (max 0 (e2L2 e))

-- 矢印（太さ wt、矢じりの大きさ hs）
arrow2 :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
arrow2 wt hs (xa, ya) (xb, yb) = Pictures [thickLine wt (xa, ya) (xb, yb), Polygon [(xb, yb), (xb - hs * cos (a - 0.4), yb - hs * sin (a - 0.4)), (xb - hs * cos (a + 0.4), yb - hs * sin (a + 0.4))]]
  where a = atan2 (yb - ya) (xb - xa)
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
