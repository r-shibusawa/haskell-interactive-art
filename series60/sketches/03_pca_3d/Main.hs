-- ============================================
-- 60-03 3 次元の点群 ─ 3×3 の共分散行列をヤコビ法で固有値分解して、3 本の主軸を描く
-- ヤコビ法：非対角の成分を 1 つずつ「回転」で 0 にしていくと、対角に固有値、回転の積に固有ベクトルが残る
--   第 1・第 2 主成分の平面へ射影すると、3D の点群を「いちばん情報が残る 2D」で見られる（右下の図）
--   捨てた第 3 成分の分散 λ3 = 平面からの距離²の平均。λ3 が小さい（板・棒）ほど 2D で十分
-- 操作：ドラッグ 視点を回す / 1 棒 2 板 3 球 4 雲 / R 向きを変える / P 平面へ押しつぶす / スペース 自動回転
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
nPts = 500                       -- ★ 点の数

-- ★ 形の名前と 3 本の軸の標準偏差
shapes :: [(String, [Double])]
shapes = [ ("rod", [3, 0.45, 0.3]), ("plate", [2.6, 1.8, 0.22]), ("sphere", [1.5, 1.5, 1.5]), ("cloud", [2.6, 1.3, 0.6]) ]

data World = World
  { wShape  :: !Int
  , wSeed   :: !Int
  , wPts    :: !Mat
  , wMu     :: !Vec
  , wEg     :: !Eig
  , wSc3    :: !Mat               -- 主成分得点（3 つ）
  , wFlat   :: !Double            -- 0 = そのまま、1 = 平面へ押しつぶした
  , wFlatOn :: !Bool
  , wSpin   :: !Bool
  , wRotY   :: !Float             -- 自動回転の角度
  , wYaw, wPitch :: !Float        -- ドラッグで回した視点
  , wLast   :: !(Maybe (Float, Float))
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-03 3D point cloud" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sumMat (wSc3 w) + U.sum (eVals (wEg w)))
      pure w
  , skStep  = \_ _ w -> pure w { wRotY = if wSpin w then wRotY w + 0.004 else wRotY w
                              , wFlat = clampv (wFlat w + (if wFlatOn w then 0.03 else -0.03)) 0 1 }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 4 V.empty (vec [0, 0, 0]) (Eig U.empty V.empty) V.empty 0 False True 0 0 0 Nothing []

-- 好きな向き（ランダムな回転 Q）
randomRotation :: R Mat
randomRotation = V.fromList <$> (orthonormalize =<< replicateM 3 (vec <$> replicateM 3 gaussRnd))

-- 軸の長さ s で共分散 C = Q diag(s²) Qᵀ を作る
covFromRot :: Mat -> [Double] -> Mat
covFromRot q s = mat [ [ foldl' (+) 0 [ sk * sk * mix q k i * mix q k j | (k, sk) <- zip [0 ..] s ] | j <- [0 .. 2] ] | i <- [0 .. 2] ]

shapeS :: Int -> [Double]
shapeS k = snd (nth ("", [1, 1, 1]) shapes k)

rebuild :: World -> World
rebuild w = w { wPts = ps, wMu = mu, wEg = eg, wSc3 = projectData ps mu (eVecs eg) 3 }
  where
    ps = withSeed (wSeed w) $ do
      q <- randomRotation
      gaussCloud nPts (vec [0, 0, 0]) (covFromRot q (shapeS (wShape w)))
    mu = colMeans ps
    eg = jacobiEigen (covMatrix ps)

-- ---- 3D：視点を回して透視投影（奥から順に描く）
camD :: Env -> Float
camD env = envH env / 2 / tan (pi / 6)          -- p5.js の WEBGL と同じ画角

toView :: World -> V3 -> V3
toView w p = rotX (wPitch w) (rotY (wRotY w + wYaw w) p)

-- 描くもの：深さと絵
type Item = (Float, Picture)

pointItem :: Env -> World -> Color -> V3 -> Item
pointItem env w col p = let q@(V3 _ _ z) = toView w p
                            k = camD env / max 1 (camD env - z)
                        in (z, Color col (at (project (camD env) q) (disc (2.2 * k))))

segItems :: Env -> World -> Color -> Float -> V3 -> V3 -> [Item]
segItems env w col wt a b = [ ((za + zb) / 2, Color col (thickLine wt (project (camD env) pa) (project (camD env) pb)))
                            | i <- [0 .. 7 :: Int]
                            , let pa@(V3 _ _ za) = toView w (lerp3 (fromIntegral i / 8))
                            , let pb@(V3 _ _ zb) = toView w (lerp3 (fromIntegral (i + 1) / 8)) ]
  where lerp3 t = add3 a (scale3 t (sub3 b a))

quadItem :: Env -> World -> Color -> [V3] -> Item
quadItem env w col ps = let qs = map (toView w) ps
                        in (sum [ z | V3 _ _ z <- qs ] / 4, Color col (Polygon (map (project (camD env)) qs)))

v3of :: Vec -> V3
v3of v = V3 (f2 (vix v 0)) (f2 (vix v 1)) (f2 (vix v 2))

ptColor :: Vec -> Int -> Color
ptColor r a = rgbaC (round (150 + 60 * tanh (vix r 0 / 2))) 200 (round (255 - 60 * tanh (vix r 1 / 2))) a

cols :: [Color]
cols = [rgbC 255 225 140, rgbC 90 165 240, rgbC 255 125 105]

draw :: Env -> World -> Picture
draw env w = Pictures [ Pictures (map snd (depthSort fst items)), hud ]
  where
    sc = realToFrac (min (envW env) (envH env)) * 0.06 :: Double
    eg = wEg w
    pc k = mrow (eVecs eg) k
    lam k = vix (eVals eg) k
    mu = wMu w
    -- 点（P で第 3 成分を 0 に近づける = 平面へ射影）
    pts3 = [ pointItem env w (ptColor r 255) (v3of (U.map (* sc) q))
           | r <- V.toList (wSc3 w)
           , let s3 = vix r 2 * (1 - wFlat w)
           , let q = U.generate 3 (\j -> vix mu j + vix r 0 * vix (pc 0) j + vix r 1 * vix (pc 1) j + s3 * vix (pc 2) j) ]
    -- 3 本の主軸（長さ 2√λ）と PC1-PC2 の平面
    m3 = v3of (U.map (* sc) mu)
    axes = concat [ segItems env w col 3 (sub3 m3 v) (add3 m3 v)
                  | (k, col) <- zip [0 .. 2] cols, let v = scale3 (f2 (2 * sqrt (max 0 (lam k)) * sc)) (v3of (pc k)) ]
    a3 = scale3 (f2 (2.2 * sqrt (max 0 (lam 0)) * sc)) (v3of (pc 0))
    b3 = scale3 (f2 (2.2 * sqrt (max 0 (lam 1)) * sc)) (v3of (pc 1))
    corner p q = add3 m3 (add3 (scale3 p a3) (scale3 q b3))
    plane = [ quadItem env w (rgbaC 90 165 240 40) [corner p0 q0, corner p1 q0, corner p1 q1, corner p0 q1]
            | i <- [0 .. 3 :: Int], j <- [0 .. 3 :: Int]
            , let p0 = -1 + fromIntegral i / 2, let p1 = p0 + 0.5, let q0 = -1 + fromIntegral j / 2, let q1 = q0 + 0.5 ]
    items = pts3 ++ axes ++ plane
    -- 画面に重ねる文字と 2D の図
    tot = U.sum (eVals eg)
    l0 = lam 0
    pw = min 340 (envW env * 0.3)
    px0 = envW env - pw - 40
    py0 = envH env - pw - 60
    ps = pw / f2 (4.4 * sqrt (max 0 l0) + 1e-9)
    hud = view env $
      [ say ("3D point cloud and Jacobi principal axes   shape: " ++ fst (nth ("", []) shapes (wShape w))) 40 44 22 (grayC 240)
      , say ("Drag view   1 rod 2 plate 3 sphere 4 cloud   R orientation   P flatten onto plane (" ++ (if wFlatOn w then "ON" else "OFF") ++ ")   Space auto spin") 40 72 15 (rgbC 165 175 190) ]
      ++ concat [ [ say ("l" ++ show (k + 1) ++ " = " ++ fmtN (lam k) 3 ++ "  (" ++ fmtN (100 * lam k / tot) 1 ++ "%)   v" ++ show (k + 1) ++ " = " ++ fmtVec (pc k) 3) 40 y 17 col
                  , Color col (rectTL 40 (y + 8) (300 * f2 (lam k / max l0 1e-9)) 10) ]
                | (k, col) <- zip [0 .. 2] cols, let y = 120 + 56 * fromIntegral k ]
      ++ [ say ("l1 : l2 : l3 = 1 : " ++ fmtN (lam 1 / l0) 3 ++ " : " ++ fmtN (lam 2 / l0) 3 ++ "    l1 + l2 + l3 = " ++ fmtN tot 3 ++ " = trace") 40 300 15 (grayC 220)
         , say ("mean squared distance to the plane = " ++ fmtN (V.foldl' (\s r -> s + vix r 2 * vix r 2) 0 (wSc3 w) / fromIntegral (nRows (wSc3 w) - 1)) 3 ++ " = l3 (dropped variance)") 40 324 15 (grayC 220)
         , Color (rgbC 70 82 98) (frameTL 1 px0 py0 pw pw)
         , Color (rgbC 70 82 98) (lineW 1 (px0, py0 + pw / 2) (px0 + pw, py0 + pw / 2))
         , Color (rgbC 70 82 98) (lineW 1 (px0 + pw / 2, py0) (px0 + pw / 2, py0 + pw)) ]
      ++ [ Color (ptColor r 200) (circleAt (px0 + pw / 2 + clampF (-pw / 2) (pw / 2) (f2 (vix r 0) * ps)) (py0 + pw / 2 - clampF (-pw / 2) (pw / 2) (f2 (vix r 1) * ps)) 4)
         | r <- V.toList (wSc3 w) ]
      ++ [ say "PC1-PC2 plane (PC1 across, PC2 up)" px0 (py0 - 10) 14 (grayC 220)
         , drawSelfTest env (wTest w) ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wLast = Just p }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wLast = Nothing }
  EventMotion (mx, my) | Just (lx, ly) <- wLast w ->
    pure w { wYaw = wYaw w + (mx - lx) * 0.01, wPitch = clampF (-1.5) 1.5 (wPitch w - (my - ly) * 0.01), wLast = Just (mx, my) }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure (rebuild w { wShape = fromEnum c - fromEnum '1' })
    | c `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
    | c `elem` "pP" -> pure w { wFlatOn = not (wFlatOn w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpin = not (wSpin w) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種 103・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Jacobi eigenvectors are orthonormal (30 random 3x3, 1e-12)", all (\(a, _, _) -> a) rs)
  , ("C V = V Lambda (error <= 1e-9 x trace)", all (\(_, a, _) -> a) rs)
  , ("sum of eigenvalues = trace, in decreasing order", all (\(_, _, a) -> a) rs)
  , ("eigenvalues of Q diag(s^2) Q^T = s^2, eigenvectors = rows of Q (< 1e-6 deg)", and [ near (vix (eVals e) k) (sk * sk) 1e-10 | (k, sk) <- zip [0 ..] s ] && and [ angleBetween (mrow (eVecs e) k) (mrow q k) < 1e-6 | k <- [0 .. 2] ])
  , ("principal axes from 20000 points ~ the axes used to make them (within 2 deg)", and [ angleBetween (mrow (eVecs ep) k) (mrow q k) < 2 | k <- [0 .. 2] ])
  , ("covariance of the PC scores = diag(l1, l2, l3) (uncorrelated, 1e-9)", maxAbsDiff (covMatrix sc3) (mat [[vix (eVals ep) 0, 0, 0], [0, vix (eVals ep) 1, 0], [0, 0, vix (eVals ep) 2]]) < 1e-9)
  , ("sum of squared distances to the PC1-PC2 plane / (n - 1) = l3", near dist2 (vix (eVals ep) 2) (1e-9 * traceOf (covMatrix p))) ]
  where
    s = [3, 1.6, 0.4]
    (rs, q, p) = withSeed 103 $ do
      r <- replicateM 30 $ do
        qq <- randomRotation
        a <- rndRange 0.1 3
        b <- rndRange 0.1 3
        c' <- rndRange 0.05 3
        let c = covFromRot qq [a, b, c']
            e' = jacobiEigen c
            sc = traceOf c
            vs = U.toList (eVals e')
        pure ( orthoError (eVecs e') <= 1e-12
             , eigResidual c e' <= 1e-9 * sc
             , near (sum vs) sc (1e-12 * sc) && not (nth 0 vs 0 < nth 0 vs 1 || nth 0 vs 1 < nth 0 vs 2) )
      q' <- randomRotation
      p' <- gaussCloud 20000 (vec [0, 0, 0]) (covFromRot q' s)
      pure (r, q', p')
    e = jacobiEigen (covFromRot q s)
    mu = colMeans p
    ep = jacobiEigen (covMatrix p)
    sc3 = projectData p mu (eVecs ep) 3
    dist2 = foldl' (+) 0 [ U.sum (U.map (^ (2 :: Int)) (U.zipWith (-) r x)) | (r, sr) <- zip (V.toList p) (V.toList sc3), let x = reconstructRow (U.take 2 sr) mu (eVecs ep) ] / fromIntegral (nRows p - 1)

-- ---------- やってみよう ----------
-- 1. 1〜4 で形を変え、λ1 : λ2 : λ3 の比を予想してから確かめよう。棒は？ 板は？
-- 2. P で平面へ押しつぶすと、どの形がいちばん「変わらない」？ λ3 と比べよう
-- 3. 球（3）の主軸の向きは R のたびにばらばら。なぜ？（固有値がほぼ同じだと向きが決まらない）
-- 4. shapes の軸の長さを変えて、自分の形を作ろう（例：[2, 2, 0.1] = 円盤）
-- 5. 次元がもっと多いと、ヤコビ法は重くなる → 04 のべき乗法、05 の特異値分解

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
