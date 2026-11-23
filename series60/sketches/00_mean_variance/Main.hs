-- ============================================
-- 60-00 平均・分散・共分散 ─ 点の集まりの「真ん中」と「ばらつき」を数で表す
-- 平均（重心）μ、x と y の分散 = 平均からのずれの 2 乗の平均、共分散 = (x のずれ) × (y のずれ) の平均
--   長方形 = 各点の (x − μx)(y − μy)。右上・左下（青）は正、左上・右下（コーラル）は負 → 合計 / (n − 1) が共分散
--   相関係数 r = 共分散 / (σx σy) は −1〜1。n − 1 で割る（不偏分散）約束はこのシリーズ全部で同じ
-- 操作：クリック 点を追加 / ドラッグ 点を動かす / X 近くの点を消す / R 新しい点群 / ← → 相関の強さ / C 全部消す / V 長方形の表示
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

-- ★ 画面に映すデータの範囲
viewX0, viewX1, viewY0, viewY1 :: Double
viewX0 = -5
viewX1 = 5
viewY0 = -4
viewY1 = 4

data World = World
  { wPts   :: [Vec]               -- 点（データの単位）
  , wDrag  :: !Int                -- ドラッグ中の点（−1 = なし）
  , wCorr  :: !Double             -- ★ R で作る点群の相関
  , wSeed  :: !Int
  , wRects :: !Bool               -- 偏差の長方形を描くか
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-00 Mean, variance, covariance" (newCloud world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = newCloud world0 { wTest = selfTest }
      _ <- evaluate (sum (map U.sum (wPts w)))
      pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] (-1) 0.6 1 True []

-- 種 wSeed から、相関 wCorr の 30 点を作る（画面の範囲に収める）
newCloud :: World -> World
newCloud w = w { wPts = map clip (V.toList cloud) }
  where
    r = wCorr w
    c = mat [[2.2, r * sqrt (2.2 * 1.3)], [r * sqrt (2.2 * 1.3), 1.3]]
    cloud = withSeed (wSeed w) (gaussCloud 30 (vec [0.5, 0.2]) c)
    clip p = vec [clampv (vix p 0) viewX0 viewX1, clampv (vix p 1) viewY0 viewY1]

-- ★ 平均・分散・共分散・相関を計算（点が 2 個未満なら 0）
data Stats = Stats { stMu :: !Vec, stVx, stVy, stCxy, stR :: !Double, stN :: !Int }

stats :: [Vec] -> Stats
stats ps
  | null ps = Stats (vec [0, 0]) 0 0 0 0 0
  | otherwise = Stats mu (mix c 0 0) (mix c 1 1) (mix c 0 1) (clampv r (-1) 1) (length ps)
  where
    x = V.fromList ps
    c = covMatrix x
    mu = colMeans x
    r = if mix c 0 0 > 1e-12 && mix c 1 1 > 1e-12 then mix c 0 1 / sqrt (mix c 0 0 * mix c 1 1) else 0

-- データの座標 ⇔ 画面の座標
data Box = Box { bS, bOx, bOy :: !Double }

plotBox :: Env -> Box
plotBox env = Box s (40 + s * (viewX1 - viewX0) / 2) (110 + s * (viewY1 - viewY0) / 2)
  where s = min (realToFrac (envW env) * 0.58 / (viewX1 - viewX0)) ((realToFrac (envH env) - 170) / (viewY1 - viewY0))

toScr :: Box -> Vec -> (Float, Float)
toScr b p = (f2 (bOx b + vix p 0 * bS b), f2 (bOy b - vix p 1 * bS b))

toData :: Box -> (Float, Float) -> Vec
toData b (x, y) = vec [clampv ((realToFrac x - bOx b) / bS b) viewX0 viewX1, clampv ((bOy b - realToFrac y) / bS b) viewY0 viewY1]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 座標と枠
  [ Color (rgbC 50 60 74) (frameTL 1 (sx viewX0) (sy viewY1) (sx viewX1 - sx viewX0) (sy viewY0 - sy viewY1)) ]
  ++ [ Color (rgbC 50 60 74) (lineW 1 (sx gx, sy viewY1) (sx gx, sy viewY0)) | gx <- [viewX0 .. viewX1] ]
  ++ [ Color (rgbC 50 60 74) (lineW 1 (sx viewX0, sy gy) (sx viewX1, sy gy)) | gy <- [viewY0 .. viewY1] ]
  -- ---- 偏差の長方形（正 = 青、負 = コーラル）
  ++ (if wRects w then [ Color (if a >= 0 then rgbaC 90 165 240 40 else rgbaC 255 125 105 40)
                                (rectTL (min mx qx) (min my qy) (abs (qx - mx)) (abs (qy - my)))
                       | (p, a) <- zip (wPts w) areas, let (qx, qy) = toScr b p ] else [])
  -- ---- 平均の十字と点
  ++ [ Color (rgbC 255 225 140) (lineW 1.5 (mx, sy viewY1) (mx, sy viewY0))
     , Color (rgbC 255 225 140) (lineW 1.5 (sx viewX0, my) (sx viewX1, my)) ]
  ++ [ Color (if i == wDrag w then rgbC 255 225 140 else rgbC 235 240 248) (circleAt qx qy 11) | (i, p) <- zip [0 ..] (wPts w), let (qx, qy) = toScr b p ]
  ++ [ Color (rgbC 255 225 140) (circleAt mx my 16), Color (rgbC 24 30 38) (circleAt mx my 6) ]
  -- ---- 数字
  ++ [ say "Mean, variance, covariance" 40 44 22 (grayC 240)
     , say ("Click add   drag move   X delete near   R new cloud   <- -> correlation (now " ++ fmtN (wCorr w) 1 ++ ")   C clear   V rectangles") 40 72 15 (rgbC 165 175 190) ]
  ++ concat [ [ say k px (y + 62 * i) 15 (rgbC 170 180 195), say v px (y + 62 * i + 26) 22 (grayC 240) ] | (i, (k, v)) <- zip [0 ..] rows ]
  ++ [ say ("Blue area total (positive)  " ++ fmtN pos 2) px (yb + 4) 16 (rgbC 90 165 240)
     , say ("Coral area total (negative)  " ++ fmtN neg 2) px (yb + 28) 16 (rgbC 255 125 105)
     , say ("(pos + neg) / (n - 1) = " ++ fmtN (if stN st > 1 then (pos + neg) / fromIntegral (stN st - 1) else 0) 3 ++ "  = covariance") px (yb + 52) 16 (grayC 240)
     , sayWrap "The covariance matrix C = [[sx^2, sxy], [sxy, sy^2]] is symmetric, and for every direction u, u^T C u = 'spread along u' >= 0. Turning this direction to search is the next sketch 01."
               px (yb + 76) (envW env - px - 30) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    b = plotBox env
    st = stats (wPts w)
    (mx, my) = toScr b (stMu st)
    sx v = f2 (bOx b + v * bS b)
    sy v = f2 (bOy b - v * bS b)
    areas = [ (vix p 0 - vix (stMu st) 0) * (vix p 1 - vix (stMu st) 1) | p <- wPts w ]
    pos = foldl' (+) 0 (filter (>= 0) areas)
    neg = foldl' (+) 0 (filter (< 0) areas)
    px = sx viewX1 + 40
    y = 130
    yb = y + 62 * 6
    rows = [ ("Number of points n", show (stN st))
           , ("Mean mu = (mu_x, mu_y)", fmtVec (stMu st) 3)
           , ("Var of x  sx^2 = sum (x - mu_x)^2 / (n - 1)", fmtN (stVx st) 3)
           , ("Var of y  sy^2", fmtN (stVy st) 3)
           , ("Covariance sxy = sum (x-mu_x)(y-mu_y) / (n-1)", fmtN (stCxy st) 3)
           , ("Correlation r = sxy / (sx sy)", fmtN (stR st) 3) ]

-- いちばん近い点（14 px 以内、なければ −1）
nearest :: Env -> World -> (Float, Float) -> Int
nearest env w (x, y) = fst (foldl' pick (-1, 14) (zip [0 ..] (wPts w)))
  where
    b = plotBox env
    pick (bi, bd) (i, p) = let (qx, qy) = toScr b p
                               dd = sqrt ((qx - x) ^ (2 :: Int) + (qy - y) ^ (2 :: Int))
                           in if dd < bd then (i, dd) else (bi, bd)

inPlot :: Env -> (Float, Float) -> Bool
inPlot env (x, y) = x > sx viewX0 && x < sx viewX1 && y > sy viewY1 && y < sy viewY0
  where b = plotBox env
        sx v = f2 (bOx b + v * bS b)
        sy v = f2 (bOy b - v * bS b)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let q = toP5 env p
        i = nearest env w q
    in pure (if i < 0 && inPlot env q && length (wPts w) < 200
               then w { wPts = wPts w ++ [toData b q], wDrag = length (wPts w) }
               else w { wDrag = i })
  EventMotion p | wDrag w >= 0 ->
    pure w { wPts = [ if i == wDrag w then toData b (toP5 env p) else q | (i, q) <- zip [0 ..] (wPts w) ] }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> let i = nearest env w (mouseP env)
                       in pure (if i >= 0 then w { wPts = [ q | (k, q) <- zip [0 ..] (wPts w), k /= i ], wDrag = -1 } else w)
    | c `elem` "rR" -> pure (newCloud w { wSeed = wSeed w + 1, wDrag = -1 })
    | c `elem` "cC" -> pure w { wPts = [], wDrag = -1 }
    | c `elem` "vV" -> pure w { wRects = not (wRects w) }
  EventKey (SpecialKey k) Down _ _
    | k == KeyLeft || k == KeyRight ->
        let d = if k == KeyRight then 0.1 else -0.1
        in pure (newCloud w { wCorr = clampv (jsRound ((wCorr w + d) * 10) / 10) (-0.9) 0.9, wDrag = -1 })
  _ -> pure w
  where b = plotBox env

-- ---- 自己テスト（p5.js 版と同じ 8 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("covariance matrix is symmetric (30 random clouds)", all (\(a, _, _, _, _) -> a) rs)
  , ("covariance matrix is positive semidefinite (smaller 2x2 eigenvalue >= 0)", all (\(_, a, _, _, _) -> a) rs)
  , ("sxy = (sum xy / n - mu_x mu_y) n / (n - 1)", all (\(_, _, a, _, _) -> a) rs)
  , ("correlation is between -1 and 1", all (\(_, _, _, a, _) -> a) rs)
  , ("shifting changes nothing; doubling y doubles sxy and quadruples sy^2", all (\(_, _, _, _, a) -> a) rs)
  , ("points on a line give r = 1 (r = -1 for a negative slope)", near (stR (stats line1)) 1 1e-12 && near (stR (stats lineN)) (-1) 1e-12)
  , ("20000 Cholesky points have covariance ~ [[2, 0.9], [0.9, 1]] (+-0.06)", maxAbsDiff (covMatrix big) (mat [[2, 0.9], [0.9, 1]]) < 0.06)
  , ("0 or 1 points give no NaN", noNaN [stVx one, stCxy one, stR one, stVx none, stR none] && stVx one == 0) ]
  where
    rs = withSeed 100 (mapM trial [0 .. 29])
    trial :: Int -> R (Bool, Bool, Bool, Bool, Bool)
    trial t = do
      ps <- replicateM (5 + t) (do a <- gaussRnd; b' <- gaussRnd; pure (vec [a * 3, b' + 0.4 * fromIntegral t]))
      let x = V.fromList ps
          c = covMatrix x
          st = stats ps
          n = fromIntegral (length ps)
          e = eig2of c
          sxy = foldl' (+) 0 [ vix p 0 * vix p 1 | p <- ps ] / n
          mu = stMu st
          c2 = covMatrix (V.map (\p -> vec [vix p 0 + 7, 2 * vix p 1 - 3]) x)
      pure ( mix c 0 1 == mix c 1 0
           , not (e2L2 e < -1e-12 * (mix c 0 0 + mix c 1 1))
           , near (mix c 0 1) ((sxy - vix mu 0 * vix mu 1) * n / (n - 1)) (1e-9 * (1 + abs sxy))
           , stR st >= -1 && stR st <= 1
           , near (mix c2 0 0) (mix c 0 0) 1e-9 && near (mix c2 0 1) (2 * mix c 0 1) 1e-9 && near (mix c2 1 1) (4 * mix c 1 1) 1e-8 )
    line1 = map vec [[0, 1], [1, 3], [2, 5], [3, 7]]
    lineN = map (\p -> vec [vix p 0, -(vix p 1)]) line1
    big = withSeed 7 (gaussCloud 20000 (vec [1, -2]) (mat [[2, 0.9], [0.9, 1]]))
    one = stats [vec [1, 2]]
    none = stats []

-- ---------- やってみよう ----------
-- 1. 右上に点を 1 つ足すと共分散はどう変わる？ 平均の真上に足したら？（長方形の面積 0）
-- 2. ← → で相関を −0.9〜0.9 に変え、青とコーラルの面積の比べ方を見よう
-- 3. 点を 1 列に並べると r = ±1 になる？ 縦一列だと？（σx = 0 のときは r を 0 と表示）
-- 4. 全部の点を右へ 2 ずらしても、分散・共分散は変わらない。なぜ？
-- 5. 「x の向き」「y の向き」以外の向きのばらつきは？ → 01 で直線を回して調べる

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
