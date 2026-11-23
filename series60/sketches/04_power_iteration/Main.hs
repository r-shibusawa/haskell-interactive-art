-- ============================================
-- 60-04 べき乗法とデフレーション ─ 「C を掛けて長さ 1 にもどす」をくり返すと第 1 主成分に近づく
-- v ← C v / ‖C v‖。v を固有ベクトルで分けると、k 回で λ_i^k 倍 → いちばん大きい λ1 の成分だけが残る
--   ずれの角 θ_k は tan θ_k ≤ tan θ_0 · (λ2/λ1)^k。λ2/λ1 が 1 に近いほど遅い（片対数グラフで直線）
--   デフレーション：C2 = C − λ1 v1 v1ᵀ（第 1 成分を引く）にまたべき乗法 → 第 2 主成分。これは固有値の計算法で、最適化ではない
-- 操作：スペース 1 回掛ける（左）/ A 自動 / D デフレーション（左）/ ↑↓ λ2/λ1（右）/ R 新しい始まり
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

dimR :: Int
dimR = 8                         -- ★ 右のグラフの次元

kMax :: Int
kMax = 60                        -- ★ 右のグラフの反復回数

-- 右のグラフ：ずれの角（度）の列と固有値
data Curves = Curves { cE1, cE2 :: [Double], cVals :: [Double], cL1, cL2 :: !Double }

data World = World
  { wRatio  :: !Double            -- ★ λ2/λ1
  , wSeed   :: !Int
  , wC2d    :: !Mat               -- 左の 2×2 の共分散行列
  , wHist   :: [Vec]              -- 左のべき乗法の v の歴史（古い順）
  , wDefl   :: !Bool
  , wAuto   :: !Bool
  , wLastT  :: !Float
  , wCurves :: Curves
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-04 Power iteration and deflation" (buildCurves (restart2d world0)))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = buildCurves (restart2d world0 { wTest = selfTest })
      _ <- evaluate (sum (cE1 (wCurves w)) + sum (cE2 (wCurves w)) + cL2 (wCurves w))
      pure w
  , skStep  = \env _ w -> pure (if wAuto w && envTime env - wLastT w > 0.6 then doStep w { wLastT = envTime env } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.8 2 (covFromAxes 2 1.1 35) [] False True 0 (Curves [] [] [] 0 0) []

-- ★ べき乗法の 1 回：v ← C v / ‖C v‖
powerStep :: Mat -> Vec -> Vec
powerStep c v = unitv (matVec c v)

-- tan θ（θ = v と q の角）
tanTo :: Vec -> Vec -> Double
tanTo v q = let u = unitv q
                w = unitv v
                c = dotv w u
                r = U.sum (U.zipWith (\wi ui -> (wi - c * ui) ^ (2 :: Int)) w u)
            in sqrt r / max (abs c) 1e-300

-- λ の見積もり vᵀ C v（レイリー商）
rayleigh :: Mat -> Vec -> Double
rayleigh c v = dotv v (matVec c v) / max (dotv v v) 1e-300

-- C − λ v vᵀ
deflate :: Mat -> Double -> Vec -> Mat
deflate c lam v = V.imap (\i r -> U.imap (\j x -> x - lam * vix v i * vix v j) r) c

-- 固有値 vals をもつ d×d の対称行列（向きは乱数）。戻り値 (C, Q)
matWithEigen :: [Double] -> R (Mat, Mat)
matWithEigen vals = do
  let d = length vals
  q <- orthonormalize =<< replicateM d (U.replicateM d gaussRnd)
  let c = mat [ [ foldl' (+) 0 [ l * vix qk i * vix qk j | (l, qk) <- zip vals q ] | j <- [0 .. d - 1] ] | i <- [0 .. d - 1] ]
  pure (c, V.fromList q)

restart2d :: World -> World
restart2d w = w { wHist = [vec [cos a, sin a]], wDefl = False }
  where a = withSeed (wSeed w) rnd * pi

-- 右：d 次元でべき乗法 → デフレーション → べき乗法。角度のずれを記録
buildCurves :: World -> World
buildCurves w = w { wCurves = withSeed (40 + wSeed w) $ do
    let r = wRatio w
        vals = take dimR (4 : 4 * r : map (* 0.7) (drop 1 vals))
    (c, q) <- matWithEigen vals
    v0 <- unitv <$> U.replicateM dimR gaussRnd
    let vs = take (kMax + 2) (iterate (powerStep c) v0)
        e1 = map (\v -> angleBetween v (mrow q 0)) (take (kMax + 1) vs)
        -- デフレーションの前に v1 を十分に収束させる（変化が 1e-14° 未満まで）
        conv k v dv = if k >= 20000 || not (dv > 1e-14) then v else let w' = powerStep c v in conv (k + 1 :: Int) w' (angleBetween v w')
        v1 = conv 0 (nth v0 vs (kMax + 1)) 1
        l1 = rayleigh c v1
        cd = deflate c l1 v1
    w0 <- unitv <$> U.replicateM dimR gaussRnd
    let ws = take (kMax + 2) (iterate (powerStep cd) w0)
        e2 = map (\x -> angleBetween x (mrow q 1)) (take (kMax + 1) ws)
    pure (Curves e1 e2 vals l1 (rayleigh c (nth w0 ws (kMax + 1)))) }

curMat :: World -> Mat
curMat w = let e = eig2of (wC2d w) in if wDefl w then deflate (wC2d w) (e2L1 e) (e2V1 e) else wC2d w

doStep :: World -> World
doStep w = w { wHist = h ++ [powerStep (curMat w) (lastOr h)] }
  where h = if length (wHist w) > 30 then drop 1 (wHist w) else wHist w
        lastOr xs = if null xs then vec [1, 0] else last xs

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：2D でべき乗法
  [ Color (rgbaC 90 165 240 160) (polyline 1.5 [ toS a b | (a, b) <- covEllipsePts (vec [0, 0]) (wC2d w) k 72 ]) | k <- [1, 2] ]
  ++ [ Color (rgbaC 90 165 240 90) (lineW 1.5 (toS (-ev1x * 4) (-ev1y * 4)) (toS (ev1x * 4) (ev1y * 4)))
     , Color (rgbaC 90 165 240 90) (lineW 1.5 (toS (-ev2x * 3) (-ev2y * 3)) (toS (ev2x * 3) (ev2y * 3))) ]
  ++ [ Color (rgbaC 255 225 140 (round a)) (arrow2 (if i == nh - 1 then 3 else 1.2) 9 (toS 0 0) (toS (vix v 0 * 3) (vix v 1 * 3)))
     | (i, v) <- zip [0 :: Int ..] (wHist w), let a = 60 + 195 * fromIntegral (i + 1) / fromIntegral nh :: Double ]
  ++ [ say ((if wDefl w then "After deflation C2 = C - l1 v1 v1^T: " else "C as is: ") ++ show (nh - 1) ++ " steps   error " ++ fmtN (angleBetween vk target) 4 ++ " deg")
           40 (f2 (cy + 4.3 * s)) 15 (grayC 220)
     , say ("v^T C v = " ++ fmtN (rayleigh (curMat w) vk) 4 ++ "   (" ++ (if wDefl w then "l2" else "l1") ++ " = " ++ fmtN (if wDefl w then e2L2 e else e2L1 e) 4 ++ ")   l2/l1 = " ++ fmtN (e2L2 e / e2L1 e) 3)
           40 (f2 (cy + 4.3 * s) + 24) 15 (grayC 220) ]
  -- ---- 右：片対数グラフ
  ++ [ plotAxes g "iteration k" "log10 (error angle, deg)" [0, 20, 40, 60] [-12, -8, -4, 0, 2]
     , Color (rgbC 255 225 140) (polyline 2.5 [ (gX g (fromIntegral k), gY g (lg d)) | (k, d) <- zip [0 :: Int ..] (cE1 cv) ])
     , Color (rgbC 255 125 105) (polyline 2.5 [ (gX g (fromIntegral k), gY g (lg d)) | (k, d) <- zip [0 :: Int ..] (cE2 cv) ])
     , Color (rgbaC 255 225 140 120) (dashLine 1 6 6 (gX g 0, gY g t0) (gX g km, gY g (t0 + km * logBase 10 (wRatio w))))
     , Color (rgbaC 255 125 105 120) (dashLine 1 6 6 (gX g 0, gY g t2) (gX g km, gY g (t2 + km * logBase 10 r2))) ]
  -- ---- 文字
  ++ [ say "Power iteration and deflation" 40 44 22 (grayC 240)
     , say ("Space multiply once   A auto (" ++ onOff (wAuto w) ++ ")   D deflation (" ++ onOff (wDefl w) ++ ")   Up/Down l2/l1 (right)   R new start") 40 72 15 (rgbC 165 175 190)
     , say ("Yellow: error to PC1. Dashed slope = log10(l2/l1) = log10(" ++ fmtN (wRatio w) 2 ++ ")") tx ty 15 (rgbC 255 225 140)
     , say "Coral: after deflation, error to PC2. Dashed = log10(l3/l2)" tx (ty + 24) 15 (rgbC 255 125 105)
     , say (show dimR ++ " dims, eigenvalues " ++ joinWith ", " (map (`fmtN` 2) (cVals cv))) tx (ty + 52) 15 (grayC 220)
     , say ("power iteration l1 = " ++ fmtN (cL1 cv) 6 ++ "   l2 after deflation = " ++ fmtN (cL2 cv) 6) tx (ty + 76) 15 (grayC 220)
     , sayWrap "Deflation uses the exact v1, iterated until it stops changing (an error in v1 would remain in v2). With l2/l1 = 0.98, 60 steps barely move."
               tx (ty + 92) (pW g) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    e = eig2of (wC2d w)
    ev1x = vix (e2V1 e) 0
    ev1y = vix (e2V1 e) 1
    ev2x = vix (e2V2 e) 0
    ev2y = vix (e2V2 e) 1
    s = min (realToFrac (envW env) * 0.075) (realToFrac (envH env) * 0.1) :: Double
    cx = realToFrac (envW env) * 0.24 :: Double
    cy = realToFrac (envH env) * 0.52 :: Double
    toS a b = (f2 (cx + a * s), f2 (cy - b * s))
    nh = length (wHist w)
    vk = if null (wHist w) then vec [1, 0] else last (wHist w)
    target = if wDefl w then e2V2 e else e2V1 e
    cv = wCurves w
    g = G (envW env * 0.52) 130 (envW env * 0.44) (envH env * 0.45) 0 (fromIntegral kMax) (-12) 2.2
    km = fromIntegral kMax
    lg d = logBase 10 (max d 1e-12)
    t0 = lg (nth 0 (cE1 cv) 0)
    r2 = nth 0 (cVals cv) 2 / nth 1 (cVals cv) 1
    t2 = lg (nth 0 (cE2 cv) 0)
    tx = pX g
    ty = pY g + pH g + 60
    onOff b = if b then "ON" else "OFF"

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (doStep w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (buildCurves w { wRatio = clampv (jsRound ((wRatio w + 0.05) * 100) / 100) 0.1 0.98 })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (buildCurves w { wRatio = clampv (jsRound ((wRatio w - 0.05) * 100) / 100) 0.1 0.98 })
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "dD" -> pure w { wDefl = not (wDefl w), wHist = take 1 (wHist w) }
    | c `elem` "rR" -> pure (buildCurves (restart2d w { wSeed = wSeed w + 1 }))
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 104・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("power iteration (300 steps) -> Jacobi 1st eigenvector (within 1e-6 deg)", all (\(a, _, _) -> a) rs)
  , ("Rayleigh quotient v^T C v -> l1 (1e-10)", all (\(_, a, _) -> a) rs)
  , ("repeated deflation finds all 8 eigenvalues and eigenvectors of the Jacobi method", all (\(_, _, a) -> a) rs)
  , ("tan theta_k <= tan theta_0 (l2/l1)^k  (l2/l1 = 0.5, 0.8, 0.95)", all fst rates)
  , ("late shrink per step tan theta_(k+1) / tan theta_k ~ l2/l1 (+-0.02)", all snd rates)
  , ("2x2 deflated matrix maps v1 to 0 and v2 to l2 v2", vlen (matVec dm (e2V1 e2)) < 1e-12 && near (rayleigh dm (e2V2 e2)) (e2L2 e2) 1e-12) ]
  where
    (rs, rates) = withSeed 104 $ do
      a <- forM [0 .. 9 :: Int] $ \t -> do
        let vals = [ 5 * 0.7 ** fromIntegral k + 0.01 * fromIntegral t | k <- [0 .. 7 :: Int] ]
        (c, _) <- matWithEigen vals
        let j = jacobiEigen c
            jv k = vix (eVals j) k
        v0 <- unitv <$> U.replicateM 8 gaussRnd
        let v = iterate (powerStep c) v0 !! 300
            okV = angleBetween v (mrow (eVecs j) 0) <= 1e-6
            okL = near (rayleigh c v) (jv 0) (1e-10 * jv 0)
        (_, found, okVecs) <- foldM (\(cd, fs, ok) i -> do
                                       w0 <- unitv <$> U.replicateM 8 gaussRnd
                                       let wv = iterate (powerStep cd) w0 !! 400
                                           l = rayleigh cd wv
                                       pure (deflate cd l wv, fs ++ [l], ok && not (angleBetween wv (mrow (eVecs j) i) > 1e-4)))
                                    (c, [], True) [0 .. 7]
        let okAll = okVecs && and [ near l (jv i) (1e-8 * jv 0) | (i, l) <- zip [0 ..] found ]
        pure (okV, okL, okAll)
      b <- forM [0.5, 0.8, 0.95] $ \r -> do
        let vals = [4, 4 * r, 4 * r * 0.6, 1, 0.5, 0.3]
        (c, q) <- matWithEigen vals
        v0 <- unitv <$> U.replicateM 6 gaussRnd
        let q0 = mrow q 0
            tan0 = tanTo v0 q0
            vs = iterate (powerStep c) v0
            steps = [ (k, tanTo (vs !! (k - 1)) q0, tanTo (vs !! k) q0) | k <- [1 .. 40 :: Int] ]
            okBound = not (any (\(k, _, tk) -> tk > tan0 * r ** fromIntegral k * (1 + 1e-9) + 1e-12) steps)
            rts = [ tk / prev | (k, prev, tk) <- steps, k > 30, prev > 1e-10 ]
            okRate = null rts || all (\x -> abs (x - r) < 0.02) rts
        pure (okBound, okRate)
      pure (a, b)
    e2 = eig2sym 3 1 2
    dm = deflate (mat [[3, 1], [1, 2]]) (e2L1 e2) (e2V1 e2)

-- ---------- やってみよう ----------
-- 1. スペースで 1 回ずつ掛け、黄色の矢印が長い軸に吸いよせられる様子を見よう。何回で 1° 以内？
-- 2. ↑↓ で λ2/λ1 を 0.5 と 0.95 にして、右のグラフの傾きを比べよう
-- 3. D でデフレーション。2 次元では 1 回で v2 になるのはなぜ？
-- 4. 始まりの v がちょうど v2 の向きだと、べき乗法はどうなる？（丸め誤差が助けてくれる）
-- 5. 100 次元のデータでも、上の数個だけならべき乗法は速い（08 の固有顔で使う）

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
-- ---- 部分空間反復法とグラム行列（大きいほうから k 個の固有ベクトルだけ）
-- グラム-シュミット（修正版）。長さがほぼ 0 になったら乱数で作り直す
orthonormalize :: [Vec] -> R [Vec]
orthonormalize = go []
  where
    go done [] = pure (reverse done)
    go done (q : qs) = do
      let q' = foldl' (\acc b -> let p = dotv acc b in U.zipWith (\x y -> x - p * y) acc b) q (reverse done)
          l = vlen q'
      if l < 1e-12
        then do fresh <- U.replicateM (U.length q) gaussRnd
                go done (fresh : qs)
        else go (U.map (/ l) q' : done) qs

-- ★ 部分空間反復法：べき乗法を k 本まとめて回し、毎回直交化。applyM v = M v（M を作らなくてよい）
-- 最後に k×k の小さい行列をヤコビ法で（レイリー-リッツ）
subspaceEigen :: (Vec -> Vec) -> Int -> Int -> Int -> Double -> R Eig
subspaceEigen applyM dim k iters tol = do
  q0 <- orthonormalize =<< replicateM k (U.replicateM dim gaussRnd)
  q <- loop 0 q0
  let mq = map applyM q
      t0 = [ [ dotv qa mb | mb <- mq ] | qa <- q ]
      t = [ [ if b < a || b > a then (nth 0 (nth [] t0 a) b + nth 0 (nth [] t0 b) a) / 2 else nth 0 (nth [] t0 a) b
            | b <- [0 .. k - 1] ] | a <- [0 .. k - 1] ]
      eg = jacobiEigen (mat t)
      comb w = fixSign (U.ifoldl' (\v a c -> U.zipWith (\x y -> x + c * y) v (nth U.empty q a)) (U.replicate dim 0) w)
  pure (Eig (eVals eg) (V.map comb (eVecs eg)))
  where
    loop it q
      | it >= iters = pure q
      | otherwise = do
          z <- orthonormalize (map applyM q)
          let diff = foldl' max 0 (zipWith (\za qa -> 1 - abs (dotv za qa)) z q)
          if tol > 0 && diff < tol && it > 5 then pure z else loop (it + 1) z

-- PCA の結果（グラム行列のやり方）：vals = 固有値、vecs = 主成分（d 次元、長さ 1）、gramTrace = 全分散
data PcaG = PcaG { pgVals :: !Vec, pgVecs :: !Mat, pgTrace :: !Double }

-- ★ 次元 d が点の数 n より大きいとき：グラム行列 G = Xc Xcᵀ / (n − 1)（n×n）の固有ベクトル u から
-- 主成分 v = Xcᵀ u / ‖Xcᵀ u‖ を作る（G と C は 0 でない固有値が同じ）。u は部分空間反復法で上から k 個
pcaGram :: Mat -> Int -> Int -> R PcaG
pcaGram xc k iters = do
  eg <- subspaceEigen (matVec g) n (min (k + 2) n) iters 1e-13      -- 2 本多めに回すと k 本目までの収束が速い
  let vecs = V.map (\u -> unitv (U.ifoldl' (\v i ui -> U.zipWith (\a x -> a + ui * x) v (mrow xc i)) (U.replicate d 0) u)) (V.take k (eVecs eg))
  pure (PcaG (U.map (max 0) (U.take k (eVals eg))) vecs (traceOf g))
  where
    n = nRows xc
    d = nCols xc
    half = V.generate n (\i -> U.generate (n - i) (\t -> dotv (mrow xc i) (mrow xc (i + t))))   -- 上半分だけ計算（対称）
    g = V.generate n (\i -> U.generate n (\j -> (if j >= i then vix (mrow half i) (j - i) else vix (mrow half j) (i - j)) / fromIntegral (max 1 (n - 1))))
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
