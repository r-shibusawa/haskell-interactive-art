-- ============================================
-- 60-05 特異値分解 ─ どんな行列も「回転 → 伸び縮み → 回転」：X = U Σ Vᵀ
-- このスケッチの計算法：XᵀX（d×d）をヤコビ法で固有値分解 → V と σ_k = √(固有値)、U の列 = X v_k / σ_k（σ ≈ 0 の列は作らない）
--   上：2×2 の行列 A が単位円を楕円に写す様子を 3 段に分ける（Vᵀ で回す → Σ で縦横にのばす → U で回す）
--   下：中心化したデータ行列 Xc（n×2）の SVD。V の列 = 主成分、σ²/(n − 1) = 共分散行列の固有値 λ
-- 操作：右上の図の ● をドラッグ A の列（A e1・A e2）/ R 新しいデータ / ↑↓ データの細さ / スペース 円の点を動かす
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

data World = World
  { wA      :: !Mat               -- ★ 上の 2×2 行列 A
  , wDrag   :: !Int               -- ドラッグ中の列（−1 = なし）
  , wPts    :: !Mat
  , wSeed   :: !Int
  , wThin   :: !Double
  , wPhase  :: !Double
  , wMoving :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-05 Singular value decomposition" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sumMat (wPts w))
      pure w
  , skStep  = \_ _ w -> pure (if wMoving w then w { wPhase = wPhase w + 0.02 } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (mat [[1.4, 0.6], [0.3, 0.9]]) (-1) V.empty 6 0.4 0 True []

-- ★ 特異値分解（XᵀX の固有値分解から）。X は n 行 × d 列
--   svU の行 k = U の列 k（長さ n）、svSig = 特異値、svV の行 k = V の列 k、svRank = 作った U の列の数
data Svd = Svd { svU :: !Mat, svSig :: !Vec, svV :: !Mat, svRank :: !Int }

svdViaEig :: Mat -> Svd
svdViaEig x = Svd u sig (eVecs eg) (nRows u)
  where
    eg = jacobiEigen (matMul (matT x) x)
    sig = U.map (\l -> sqrt (max 0 l)) (eVals eg)
    tol = 1e-7 * max (vix sig 0) 1e-300
    u = V.fromList [ V.convert (V.map (\r -> dotv r v / vix sig k) x) | (k, v) <- zip [0 ..] (V.toList (eVecs eg)), vix sig k > tol ]

rebuild :: World -> World
rebuild w = w { wPts = withSeed (wSeed w) $ do
                  u <- rnd
                  gaussCloud 40 (vec [0, 0]) (covFromAxes 1.8 (1.8 * wThin w) (20 + 60 * u)) }

diag2 :: Vec -> Mat
diag2 s = mat [[vix s 0, 0], [0, vix s 1]]

-- 2×2 の SVD：(Um（列 = U の列）, σ, Vt)
svd2 :: Mat -> (Mat, Vec, Mat)
svd2 a = (um, svSig r, svV r)
  where
    r = svdViaEig a
    um = if svRank r == 2 then matT (svU r)
         else let u0 = if svRank r >= 1 then mrow (svU r) 0 else vec [1, 0]
              in mat [[vix u0 0, -(vix u0 1)], [vix u0 1, vix u0 0]]

-- 上の 4 つの図の大きさ
panelW :: Env -> Float
panelW env = min ((envW env - 140) / 4) (envH env * 0.33)

panelSc :: Env -> Mat -> Double
panelSc env a = let (_, sg, _) = svd2 a in realToFrac (panelW env) / 2 / max 2.2 (vix sg 0 * 1.1)

-- 小さな座標の枠
panel :: Float -> Float -> Float -> String -> Picture
panel x y w title = Pictures
  [ Color (rgbC 60 72 88) (frameTL 1 x y w w)
  , Color (rgbC 60 72 88) (lineW 1 (x, y + w / 2) (x + w, y + w / 2))
  , Color (rgbC 60 72 88) (lineW 1 (x + w / 2, y) (x + w / 2, y + w))
  , say title x (y - 10) 15 (rgbC 205 212 222) ]

-- 単位円と e1・e2（M で写したもの）。戻り値 2 つ目 = A e1・A e2 の点
drawShape :: Float -> Float -> Float -> Double -> Double -> Mat -> (Picture, ((Float, Float), (Float, Float)))
drawShape x y w sc phase m = (Pictures
  [ Color (rgbaC 90 165 240 40) (Polygon (map pp circ))
  , Color (rgbC 90 165 240) (polyline 2 (map pp circ))
  , Color (rgbC 255 225 140) (arrow2 2 9 o a)
  , Color (rgbC 255 125 105) (arrow2 2 9 o b)
  , Color (grayC 240) (circleAt (fst t) (snd t) 9) ], (a, b))
  where
    pp v = (x + w / 2 + f2 (vix v 0 * sc), y + w / 2 - f2 (vix v 1 * sc))
    circ = [ matVec m (vec [cos t', sin t']) | i <- [0 .. 72 :: Int], let t' = fromIntegral i * pi / 36 ]
    o = pp (vec [0, 0])
    a = pp (matVec m (vec [1, 0]))
    b = pp (matVec m (vec [0, 1]))
    t = pp (matVec m (vec [cos phase, sin phase]))

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 上：A = U Σ Vᵀ を 3 段で
  concat [ [ panel x y0 pw nm, pic ] ++ (if i == 3 then [Color (rgbC 255 225 140) (circleAt (fst pa) (snd pa) 16), Color (rgbC 255 125 105) (circleAt (fst pb) (snd pb) 16)] else [])
         | (i, (m, nm)) <- zip [0 :: Int ..] (zip steps names)
         , let x = 40 + fromIntegral i * (pw + 20)
         , let (pic, (pa, pb)) = drawShape x y0 pw sc (wPhase w) m ]
  -- ---- 下：データ行列の SVD
  ++ [ panel 40 by bw ("Data Xc (" ++ show n ++ " points) and V columns x s/sqrt(n - 1)") ]
  ++ [ Color (rgbaC 225 232 242 190) (Pictures [ circleAt (cx + f2 (vix r 0 * s2)) (cy - f2 (vix r 1 * s2)) 6 | r <- V.toList xc ]) ]
  ++ [ Color (if k == 0 then rgbC 255 225 140 else rgbC 255 125 105) (arrow2 2.5 10 (cx, cy) (cx + f2 (vix v 0 * l * s2), cy - f2 (vix v 1 * l * s2)))
     | (k, v) <- zip [0 :: Int ..] (V.toList (svV sd)), let l = 2 * vix (svSig sd) k / sqrt (fromIntegral n - 1) ]
  -- ---- 数字
  ++ [ say "Singular value decomposition   X = U S V^T" 40 44 22 (grayC 240)
     , say ("Drag the dots in the top-right panel: columns of A   R new data   Up/Down thinness (" ++ fmtN (wThin w) 2 ++ ")   Space move the point") 40 72 15 (rgbC 165 175 190)
     , say ("A = [[" ++ fmtN (mix a 0 0) 2 ++ ", " ++ fmtN (mix a 0 1) 2 ++ "], [" ++ fmtN (mix a 1 0) 2 ++ ", " ++ fmtN (mix a 1 1) 2 ++ "]]   singular values s = " ++ fmtN (vix sg 0) 3 ++ ", " ++ fmtN (vix sg 1) 3) tx ty 15 (grayC 220)
     , say ("|det A| = " ++ fmtN (abs (mix a 0 0 * mix a 1 1 - mix a 0 1 * mix a 1 0)) 3 ++ " = s1 s2") tx (ty + 22) 15 (grayC 220)
     , say ("data: s1 = " ++ fmtN (sgD 0) 4 ++ "   s1^2/(n - 1) = " ++ fmtN (sgD 0 ^ (2 :: Int) / (fromIntegral n - 1)) 5 ++ "   covariance l1 = " ++ fmtN (vix (eVals ec) 0) 5) tx (ty + 50) 15 (rgbC 255 225 140)
     , say ("s2 = " ++ fmtN (sgD 1) 4 ++ "   s2^2/(n - 1) = " ++ fmtN (sgD 1 ^ (2 :: Int) / (fromIntegral n - 1)) 5 ++ "   covariance l2 = " ++ fmtN (vix (eVals ec) 1) 5) (tx + 44) (ty + 74) 15 (rgbC 255 125 105)
     , say ("V columns = " ++ joinWith ", " (map (`fmtVec` 3) (V.toList (svV sd))) ++ "   (= covariance eigenvectors)") tx (ty + 104) 15 (grayC 220)
     , sayWrap ("U is " ++ show n ++ "x2 (each point's PC scores scaled to length 1)   U^T U = I error " ++ fmtE (orthoError (svU sd)) 1 ++ "   U S V^T vs Xc error " ++ fmtE rec 1)
               tx (ty + 114) (envW env - tx - 30) 15 (grayC 220)
     , sayWrap "PCA = the SVD of Xc. PC scores = U S = Xc V. Here it is built from the eigen decomposition of X^T X (squaring loses accuracy for small s, so serious codes compute the SVD directly, e.g. Golub-Kahan)."
               tx (ty + 166) (envW env - tx - 30) 14 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    a = wA w
    (um, sg, vt) = svd2 a
    pw = panelW env
    y0 = 120
    sc = panelSc env a
    steps = [eyeMat 2, vt, matMul (diag2 sg) vt, matMul um (matMul (diag2 sg) vt)]
    names = ["Unit circle", "1. V^T (rotate or flip)", "2. stretch by S", "3. rotate by U = A"]
    xc = centerCols (wPts w) (colMeans (wPts w))
    sd = svdViaEig xc
    sgD k = vix (svSig sd) k
    ec = jacobiEigen (covMatrix (wPts w))
    n = nRows (wPts w)
    by = y0 + pw + 60
    bw = min (envH env - by - 40) (envW env * 0.3)
    s2 = realToFrac bw / 2 / 4.2 :: Double
    cx = 40 + bw / 2
    cy = by + bw / 2
    tx = 80 + bw
    ty = by + 10
    rec = foldl' max 0 [ abs (s - vix r j) | (i, r) <- zip [0 ..] (V.toList xc), j <- [0 .. U.length r - 1]
                       , let s = foldl' (+) 0 [ mix (svU sd) k i * vix (svSig sd) k * mix (svV sd) k j | k <- [0 .. svRank sd - 1] ] ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hit j = let qx = px + pw / 2 + f2 (mix (wA w) 0 j * sc)
                    qy = py + pw / 2 - f2 (mix (wA w) 1 j * sc)
                in sqrt ((mx - qx) ^ (2 :: Int) + (my - qy) ^ (2 :: Int)) < 14
    in pure w { wDrag = foldl' (\d j -> if hit j then j else d) (-1) [0, 1] }
  EventMotion p | wDrag w >= 0 ->
    let (mx, my) = toP5 env p
        j = wDrag w
        ax = clampv (realToFrac (mx - px - pw / 2) / sc) (-2.5) 2.5
        ay = clampv (-(realToFrac (my - py - pw / 2)) / sc) (-2.5) 2.5
    in pure w { wA = V.imap (\i r -> U.imap (\c v -> if c == j then (if i == 0 then ax else ay) else v) r) (wA w) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild w { wThin = clampv (wThin w + 0.1) 0 1 })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild w { wThin = clampv (wThin w - 0.1) 0 1 })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wMoving = not (wMoving w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
  _ -> pure w
  where
    pw = panelW env
    px = 40 + 3 * (pw + 20)
    py = 120
    sc = panelSc env (wA w)

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 105・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("U S V^T = Xc (10 data matrices, error <= 1e-9 x s1)", all (\(a, _, _, _) -> a) rs)
  , ("columns of U and of V are orthonormal", all (\(_, a, _, _) -> a) rs)
  , ("s_k^2 / (n - 1) = eigenvalues l_k of the covariance matrix", all (\(_, _, a, _) -> a) rs)
  , ("columns of V = covariance eigenvectors (within 1e-6 deg)", all (\(_, _, _, a) -> a) rs)
  , ("2x2: A = U S V^T, s1 s2 = |det A|, s1 = longest radius of the image of the unit circle", and okA)
  , ("rank 1 matrix: s2 ~ 0 and U gets only one column (no division by 0, no NaN)", svRank rk == 1 && noNaN (concatMap U.toList (V.toList (svU rk))) && vix (svSig rk) 1 < 1e-6) ]
  where
    (rs, okA) = withSeed 105 $ do
      r <- forM [0 .. 9 :: Int] $ \t -> do
        let n = 20 + 5 * t
            d = 2 + t `mod` 5
        x <- V.replicateM n (U.generateM d (\j -> (* (1 + fromIntegral j)) <$> gaussRnd))
        let xc = centerCols x (colMeans x)
            sv = svdViaEig xc
            sc = vix (svSig sv) 0
            err = foldl' max 0 [ abs (s - vix row j) | (i, row) <- zip [0 ..] (V.toList xc), j <- [0 .. d - 1]
                               , let s = foldl' (+) 0 [ mix (svU sv) k i * vix (svSig sv) k * mix (svV sv) k j | k <- [0 .. svRank sv - 1] ] ]
            ec = jacobiEigen (covMatrix x)
            l0 = vix (eVals ec) 0
        pure ( not (err > 1e-9 * sc)
             , not (orthoError (svU sv) > 1e-9 || orthoError (svV sv) > 1e-12)
             , and [ near (vix (svSig sv) k ^ (2 :: Int) / fromIntegral (n - 1)) l (1e-9 * l0) | (k, l) <- zip [0 ..] (U.toList (eVals ec)) ]
             , and [ angleBetween v (mrow (svV sv) k) < 1e-6 | (k, v) <- zip [0 ..] (V.toList (eVecs ec)) ] )
      a <- replicateM 30 $ do
        a00 <- rndRange (-2) 2
        a01 <- rndRange (-2) 2
        a10 <- rndRange (-2) 2
        a11 <- rndRange (-2) 2
        let am = mat [[a00, a01], [a10, a11]]
            (um, sg, vt) = svd2 am
            b = matMul um (matMul (diag2 sg) vt)
            mx = foldl' max 0 [ vlen (matVec am (vec [cos t, sin t])) | i <- [0 .. 719 :: Int], let t = fromIntegral i * pi / 360 ]
        pure (not (maxAbsDiff am b > 1e-12 || not (near (vix sg 0 * vix sg 1) (abs (a00 * a11 - a01 * a10)) 1e-12)) && near mx (vix sg 0) (1e-4 * vix sg 0))
      pure (r, a)
    rk = svdViaEig (mat [[1, 2], [2, 4], [3, 6], [-6, -12]])

-- ---------- やってみよう ----------
-- 1. 右上の ● を動かし、A を回転行列（例 [[0, −1], [1, 0]]）にしてみよう。Σ はどうなる？
-- 2. A e1 と A e2 を同じ向きにそろえると（ランク 1）、② の楕円はどうなる？ σ2 は？
-- 3. ↓ でデータを細くすると σ2 → 0。そのとき U の 2 列目は何を表している？
-- 4. σ1 σ2 = |det A| なのはなぜ？（面積の倍率）
-- 5. 画像の行列を SVD して大きい σ だけ残すと画像の圧縮になる → 07 の復元と同じ考え

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
