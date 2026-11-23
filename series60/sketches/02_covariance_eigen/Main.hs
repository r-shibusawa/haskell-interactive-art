-- ============================================
-- 60-02 共分散行列と固有ベクトル ─ C v = λ v。掛けても向きが変わらないベクトルが「データの軸」
-- 2×2 の共分散行列 C = [[a, b], [b, c]] の固有値は λ = (a + c)/2 ± √(((a − c)/2)² + b²)（解の公式）
--   固有ベクトル v1・v2 は直角。楕円の半径 = k √λ（k = 1, 2 で 1σ・2σ の楕円）
--   M：円の上のベクトル v に C を掛けるアニメーション。ほとんどの矢印は向きが変わるが、v1・v2 の向きだけはのびるだけ
-- 操作：黄色い ● をドラッグ 長い軸 / ↑↓ 短い軸 / M 掛けるアニメーション / R 新しい点 / スペース 点の表示
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
nPts = 400                       -- ★ 点の数

data World = World
  { wS1, wS2 :: !Double           -- ★ 軸の標準偏差
  , wAng     :: !Double           -- 長い軸の角度（度）
  , wC       :: !Mat              -- 共分散行列
  , wPts     :: !Mat              -- 点
  , wSeed    :: !Int
  , wAnim    :: !Double           -- アニメーションの時刻（0〜1.6）
  , wAnimOn  :: !Bool
  , wShowPts :: !Bool
  , wDrag    :: !Bool
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-02 Covariance matrix and eigenvectors" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sumMat (wPts w))
      pure w
  , skStep  = \_ _ w -> pure (if wAnimOn w then w { wAnim = jsMod (wAnim w + 0.006) 1.6 } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2.2 0.8 30 (zerosMat 2 2) V.empty 5 0 True True False []

rebuild :: World -> World
rebuild w = w { wC = c, wPts = withSeed (wSeed w) (gaussCloud nPts (vec [0, 0]) c) }
  where c = covFromAxes (wS1 w) (wS2 w) (wAng w)

jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

scl :: Env -> Double
scl env = min (realToFrac (envW env) * 0.06) (realToFrac (envH env) * 0.085)

ctr :: Env -> (Double, Double)
ctr env = (realToFrac (envW env) * 0.3, realToFrac (envH env) * 0.54)

toScr :: Env -> Double -> Double -> (Float, Float)
toScr env a b = let (cx, cy) = ctr env; s = scl env in (f2 (cx + a * s), f2 (cy - b * s))

-- 1σ・2σ の楕円の内側にある点の割合
insideFrac :: E2 -> Mat -> Double -> Double
insideFrac e ps k = fromIntegral (V.length (V.filter inside ps)) / fromIntegral (max 1 (nRows ps))
  where inside p = let a = dotv p (e2V1 e)
                       b = dotv p (e2V2 e)
                   in a * a / e2L1 e + b * b / max (e2L2 e) 1e-12 <= k * k

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 点・楕円・軸
  [ Color (rgbaC 225 232 242 120) (Pictures [ let (qx, qy) = toScr env (vix p 0) (vix p 1) in circleAt qx qy 4 | p <- V.toList (wPts w) ]) | wShowPts w ]
  ++ [ Color (rgbaC 90 165 240 (if k == 1 then 255 else 150)) (polyline 2 [ toScr env a b | (a, b) <- covEllipsePts (vec [0, 0]) c k 72 ]) | k <- [1, 2] ]
  ++ [ Color (rgbC 255 225 140) (arrow2 3 14 o h1)
     , Color (rgbC 255 125 105) (arrow2 3 12 o h2)
     , Color (rgbC 255 225 140) (circleAt (fst h1) (snd h1) 18) ]
  -- ---- 掛けるアニメーション：v → (1 − t) v + t C v（右の図）
  ++ [ Color (rgbC 60 72 88) (ringAt rcx rcy (2 * f2 (bigR / max (e2L1 e) 1e-9)) 1)
     , Color (rgbC 60 72 88) (polyline 1 [ rp a b | (a, b) <- covEllipsePts (vec [0, 0]) (matMul c c) 1 72 ]) ]
  ++ concat [ [ Color (rgbaC 160 175 195 110) (lineW 1 (rcx, rcy) (rp (vix v 0) (vix v 1)))
              , Color (rgbC 220 228 240) (arrow2 1.5 7 (rcx, rcy) (rp ((1 - tt) * vix v 0 + tt * vix cv 0) ((1 - tt) * vix v 1 + tt * vix cv 1))) ]
            | i <- [0 .. 23 :: Int], let a = fromIntegral i * pi / 12, let v = vec [cos a, sin a], let cv = matVec c v ]
  ++ [ Color col (arrow2 3 10 (rcx, rcy) (rp (vix v 0 * f) (vix v 1 * f)))
     | (v, l, col) <- [ (e2V1 e, e2L1 e, yel), (e2V2 e, e2L2 e, cor), (U.map negate (e2V1 e), e2L1 e, yel), (U.map negate (e2V2 e), e2L2 e, cor) ]
     , let f = (1 - tt) + tt * l ]
  -- ---- 文字
  ++ [ say "Covariance matrix and eigenvectors   C v = l v" 40 44 22 (grayC 240)
     , say ("Drag yellow dot: long axis   Up/Down short axis   M animation (" ++ (if wAnimOn w then "ON" else "OFF") ++ ")   R new points   Space show points") 40 72 15 (rgbC 165 175 190)
     , say ("C = [[" ++ fmtN (mix c 0 0) 3 ++ ", " ++ fmtN (mix c 0 1) 3 ++ "], [" ++ fmtN (mix c 1 0) 3 ++ ", " ++ fmtN (mix c 1 1) 3 ++ "]]") tx ty 15 (grayC 220)
     , say ("l1 = " ++ fmtN (e2L1 e) 3 ++ "   v1 = " ++ fmtVec (e2V1 e) 3) tx (ty + 24) 15 yel
     , say ("C v1 = " ++ fmtVec (matVec c (e2V1 e)) 3) tx (ty + 44) 15 yel
     , say ("l2 = " ++ fmtN (e2L2 e) 3 ++ "   v2 = " ++ fmtVec (e2V2 e) 3) tx (ty + 68) 15 cor
     , say ("C v2 = " ++ fmtVec (matVec c (e2V2 e)) 3) tx (ty + 88) 15 cor
     , say ("l1 + l2 = " ++ fmtN (e2L1 e + e2L2 e) 3 ++ " = a + c   l1 l2 = " ++ fmtN (e2L1 e * e2L2 e) 3 ++ " = ac - b^2") tx (ty + 114) 15 (grayC 220)
     , say ("Point covariance (" ++ show nPts ++ " pts) [[" ++ fmtN (mix c0 0 0) 2 ++ ", " ++ fmtN (mix c0 0 1) 2 ++ "], [" ++ fmtN (mix c0 1 0) 2 ++ ", " ++ fmtN (mix c0 1 1) 2 ++ "]]") tx (ty + 140) 15 (rgbC 90 165 240)
     , say ("Inside 1 sigma " ++ fmtN (100 * in1) 1 ++ "% (39.3%)   2 sigma " ++ fmtN (100 * in2) 1 ++ "% (86.5%)") tx (ty + 164) 14 (rgbC 90 165 240)
     , sayWrap "Right: multiplying an arrow v on the circle by C (white) usually turns it. Only the yellow and coral eigenvectors just stretch by l."
               tx (rcy - f2 bigR - 30) (envW env - tx - 30) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    c = wC w
    e = eig2of c
    c0 = covMatrix (wPts w)
    t = clampv (wAnim w) 0 1
    tt = t * t * (3 - 2 * t)
    o = toScr env 0 0
    h1 = toScr env (vix (e2V1 e) 0 * 2 * sqrt (e2L1 e)) (vix (e2V1 e) 1 * 2 * sqrt (e2L1 e))
    h2 = toScr env (vix (e2V2 e) 0 * 2 * sqrt (e2L2 e)) (vix (e2V2 e) 1 * 2 * sqrt (e2L2 e))
    in1 = insideFrac e (wPts w) 1
    in2 = insideFrac e (wPts w) 2
    bigR = min (realToFrac (envW env) * 0.19) (realToFrac (envH env) * 0.24) :: Double
    rcxD = realToFrac (envW env) * 0.76 :: Double
    rcyD = realToFrac (envH env) * 0.4 :: Double
    rcx = f2 rcxD
    rcy = f2 rcyD
    sc = bigR / max (e2L1 e) 1e-9
    rp a b = (f2 (rcxD + a * sc), f2 (rcyD - b * sc))
    tx = envW env * 0.58
    ty = rcy + f2 bigR + 60
    yel = rgbC 255 225 140
    cor = rgbC 255 125 105

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        e = eig2of (wC w)
        (hx, hy) = toScr env (vix (e2V1 e) 0 * 2 * wS1 w) (vix (e2V1 e) 1 * 2 * wS1 w)
    in pure w { wDrag = sqrt ((mx - hx) ^ (2 :: Int) + (my - hy) ^ (2 :: Int)) < 20 }
  EventMotion p | wDrag w ->
    let (mx, my) = toP5 env p
        (cx, cy) = ctr env
        s = scl env
        dx = (realToFrac mx - cx) / s
        dy = -(realToFrac my - cy) / s
    in pure (rebuild w { wS1 = clampv (hypot dx dy / 2) (max (wS2 w) 0.2) 4, wAng = atan2 dy dx * 180 / pi })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = False }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild w { wS2 = clampv (wS2 w + 0.1) 0.1 (wS1 w) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild w { wS2 = clampv (wS2 w - 0.1) 0.1 (wS1 w) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wShowPts = not (wShowPts w) }
  EventKey (Char ch) Down _ _
    | ch `elem` "mM" -> pure w { wAnimOn = not (wAnimOn w) }
    | ch `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 102 と 9・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("formula eigenpairs satisfy C v = l v (45 matrices incl. diagonal, double root, rank 1; 1e-12)", all (\(a, _, _, _) -> a) rs)
  , ("v1 perpendicular to v2, l1 >= l2", all (\(_, a, _, _) -> a) rs)
  , ("l1 + l2 = trace, l1 l2 = determinant", all (\(_, _, a, _) -> a) rs)
  , ("agrees with the Jacobi eigenvalues", all (\(_, _, _, a) -> a) rs)
  , ("C built from axes (s1, s2, angle) has eigenvalues s1^2, s2^2 and PC1 angle = that angle", okAx)
  , ("of 20000 points, inside the 1 and 2 sigma ellipses = 1 - e^(-k^2/2) (39.3%, 86.5%, +-1%)", near f1 (1 - exp (-0.5)) 0.01 && near f2' (1 - exp (-2)) 0.01) ]
  where
    (mats, axes) = withSeed 102 $ do
      rand <- replicateM 40 $ do
        a <- rndRange 0.1 5
        c <- rndRange 0.1 5
        b0 <- rndRange (-1) 1
        let b = b0 * sqrt (a * c)
        pure [[a, b], [b, c]]
      ax <- replicateM 20 ((,,) <$> rndRange 0.5 3 <*> rndRange 0.1 0.45 <*> rndRange (-89) 89)
      pure (map mat ([[[2, 0], [0, 1]], [[1, 0], [0, 3]], [[2, 0], [0, 2]], [[1, 1], [1, 1]], [[1e-6, 0], [0, 0]]] ++ rand), ax)
    rs = map one mats
    one c =
      let e = eig2of c
          sc = max 1 (abs (e2L1 e))
          okPair v l = let cv = matVec c v in not (abs (vix cv 0 - l * vix v 0) > 1e-12 * sc || abs (vix cv 1 - l * vix v 1) > 1e-12 * sc || not (near (vlen v) 1 1e-12))
          j = jacobiEigen c
      in ( okPair (e2V1 e) (e2L1 e) && okPair (e2V2 e) (e2L2 e)
         , not (abs (dotv (e2V1 e) (e2V2 e)) > 1e-12 || e2L1 e < e2L2 e)
         , near (e2L1 e + e2L2 e) (mix c 0 0 + mix c 1 1) (1e-12 * sc) && near (e2L1 e * e2L2 e) (mix c 0 0 * mix c 1 1 - mix c 0 1 ^ (2 :: Int)) (1e-11 * sc * sc)
         , near (vix (eVals j) 0) (e2L1 e) (1e-12 * sc) && near (vix (eVals j) 1) (e2L2 e) (1e-12 * sc) )
    okAx = and [ near (e2L1 e) (a1 * a1) 1e-12 && near (e2L2 e) (a2 * a2) 1e-12 && min dd (180 - dd) <= 1e-9
               | (a1, a2, d) <- axes, let e = eig2of (covFromAxes a1 a2 d), let dd = jsMod (abs (e2Angle e - d)) 180 ]
    cT = covFromAxes 2 0.7 40
    eT = eig2of cT
    pT = withSeed 9 (gaussCloud 20000 (vec [0, 0]) cT)
    frac k = fromIntegral (V.length (V.filter (\p -> let a = dotv p (e2V1 eT); b = dotv p (e2V2 eT) in a * a / e2L1 eT + b * b / e2L2 eT <= k * k) pT)) / fromIntegral (nRows pT)
    f1 = frac 1
    f2' = frac 2

-- ---------- やってみよう ----------
-- 1. ↓ で短い軸を細くしていくと λ2 → 0。そのとき点群は何次元に見える？
-- 2. s1 = s2（丸い点群）にすると、どの向きも固有ベクトルになる。右のアニメーションで確かめよう
-- 3. 楕円の内側の割合は 1D の「±1σ に 68%」と同じ？ ちがうのはなぜ？（2 次元のカイ 2 乗分布）
-- 4. 右の灰色の楕円 = 円を C で写した形（半径 λ1, λ2）。左の 1σ の楕円（半径 √λ）とくらべよう
-- 5. 3 次元ではヤコビ法で同じことをする → 03

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
