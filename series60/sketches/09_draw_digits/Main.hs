-- ============================================
-- 60-09 自分で描いた文字の主成分 ─ 手書きの文字を 16×16 = 256 次元にして、2 次元の主成分平面に並べる
-- 左の枠にマウスで文字を描く → 大きさと位置をそろえて 16×16 のマスに（線からの距離でぼかす）
--   お手本（0・1・2・7 を少しずつゆがめた 40 枚）＋ 自分の文字で PCA。点の数 n < 次元 256 なので
--   グラム行列（n×n）の固有ベクトルから主成分を作る（08 と同じやり方）。似た文字が近くに集まる？
-- 操作：ドラッグ 描く / スペース 追加 / 0 1 2 7 ラベル / C 描き直す / Z 最後の追加を消す / R お手本を作り直す
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

gn, dimG :: Int
gn = 16                           -- ★ マスの大きさ（16×16）
dimG = gn * gn

labels :: [Int]
labels = [0, 1, 2, 7]

labelCol :: Int -> Color
labelCol l = case l of
  0 -> rgbC 255 225 140
  1 -> rgbC 90 165 240
  2 -> rgbC 255 125 105
  _ -> rgbC 150 220 160

-- 1 枚の文字：16×16 の明るさ、ラベル、自分で描いたか
data Sample = Sample { sImg :: !Vec, sLab :: !Int, sMine :: !Bool }

-- PCA の結果（上位 2 本）と、全部の点の得点
data Pca = Pca { pMu :: !Vec, pRes :: Maybe PcaG, pScores :: [(Double, Double)] }

data World = World
  { wSamples :: [Sample]
  , wStrokes :: [[(Double, Double)]]   -- いま描いている線（画面の座標、最大 20 本 × 400 点）
  , wCur     :: !Vec                    -- いま描いている文字を 16×16 にしたもの
  , wLabel   :: !Int
  , wSeed    :: !Int
  , wRng     :: Rng                     -- 乱数の続き（p5.js 版と同じく、作り直すたびに続きから引く）
  , wPca     :: Pca
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-09 Principal components of your own drawing" (makeSamples world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = makeSamples world0 { wTest = selfTest }
      _ <- evaluate (U.sum (pMu (wPca w)) + sum (map fst (pScores (wPca w))))
      pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] (rasterStrokes [] gn) 2 4 (seedRng 4) (Pca U.empty Nothing []) []

-- 種 wSeed からお手本 40 枚を作って PCA
makeSamples :: World -> World
makeSamples w = analyseW w { wSamples = ss, wRng = rng1 }
  where
    (ss, rng1) = runR (forM [0 .. 39 :: Int] $ \i -> do
                         let lab = nth 0 labels (i `mod` 4)
                         st <- sampleGlyph lab
                         pure (Sample (rasterStrokes st gn) lab False)) (seedRng (wSeed w))

-- ★ PCA（グラム行列から上位 2 本）と、全部の点の得点
analyse :: [Sample] -> R Pca
analyse ss = do
  let x = V.fromList (map sImg ss)
      mu = colMeans x
  if length ss >= 3
    then do r <- pcaGram (centerCols x mu) 2 300
            let sc = projectData x mu (pgVecs r) 2
            pure (Pca mu (Just r) [ (vix s 0, vix s 1) | s <- V.toList sc ])
    else pure (Pca mu Nothing (map (const (0, 0)) ss))

analyseW :: World -> World
analyseW w = let (p, rng) = runR (analyse (wSamples w)) (wRng w) in w { wPca = p, wRng = rng }

-- 描く枠と主成分平面の位置
padGeo :: Env -> (Float, Float, Float)
padGeo env = (40, 120, min 300 (envH env * 0.4))

plotGeo :: Env -> (Float, Float, Float)
plotGeo env = (envW env * 0.42, 120, min (envW env * 0.54) (envH env - 200))

-- 16×16 の絵（signed なら オレンジ = 正、青 = 負）
gridPic :: Vec -> Bool -> Picture
gridPic img signed = imageTL 0 0 gn gn 6 (\x y -> let v = vix img (x + gn * y) / m in
                                             if signed then (if v > 0 then (byte (128 + 127 * v), byte (128 + 70 * v), byte (128 - 60 * v))
                                                                     else (byte (128 + 60 * v), byte (128 - 10 * v), byte (128 - 127 * v)))
                                             else let g = byte (30 + 225 * v) in (g, g, g))
  where m = if signed then (let a = U.foldl' (\s v -> max s (abs v)) 0 img in if a == 0 then 1 else a) else 1

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 描く枠
  [ Color (rgbC 80 92 108) (frameTL 1 px py ps ps) ]
  ++ concat [ [ Color (grayC 240) (polyline 10 [ (f2 a, f2 b) | (a, b) <- st ]) ] ++ [ Color (grayC 240) (circleAt (f2 a) (f2 b) 10) | [(a, b)] <- [st] ] | st <- wStrokes w ]
  ++ [ say ("Draw here (label " ++ show (wLabel w) ++ ")") px (py - 10) 14 (rgbC 205 212 222)
     , say "As 16x16" px (py + ps + 24) 14 (rgbC 205 212 222)
     , Translate px (py + ps + 32) (gridPic (wCur w) False) ]
  ++ (case pRes p of
        Just r -> [ say "Mean" (px + 116) (py + ps + 24) 14 (rgbC 205 212 222), Translate (px + 116) (py + ps + 32) (gridPic (pMu p) False)
                  , say "PC1 (orange +, blue -)" (px + 232) (py + ps + 24) 14 (rgbC 205 212 222), Translate (px + 232) (py + ps + 32) (gridPic (mrow (pgVecs r) 0) True) ]
        Nothing -> [])
  -- ---- 主成分平面
  ++ [ Color (rgbC 60 72 88) (frameTL 1 gx gy gs gs), Color (rgbC 60 72 88) (lineW 1 (gx, gy + gs / 2) (gx + gs, gy + gs / 2))
     , Color (rgbC 60 72 88) (lineW 1 (gx + gs / 2, gy) (gx + gs / 2, gy + gs)) ]
  ++ concat [ [ Color (labelCol (sLab s)) (ringAt qx qy 16 2) | sMine s ] ++ [ sayM (show (sLab s)) qx qy (if sMine s then 16 else 14) (labelCol (sLab s)) ]
            | (s, sc) <- zip (wSamples w) (pScores p), let (qx, qy) = toPlot sc ]
  ++ (case nearInfo of
        Just ((qx, qy), _) -> [ Color white (ringAt qx qy 26 2), Color white (lineW 2 (qx - 18, qy) (qx + 18, qy)), Color white (lineW 2 (qx, qy - 18) (qx, qy + 18)) ]
        Nothing -> [])
  -- ---- 文字
  ++ [ say "Principal components of your own drawing" 40 44 22 (grayC 240)
     , say "Drag draw   Space add   0 1 2 7 label   C clear drawing   Z remove last added   R new examples" 40 72 15 (rgbC 165 175 190)
     , say "PC plane (PC1 across, PC2 up). Digits = examples, ring = yours, cross = now" gx (gy - 10) 14 (rgbC 205 212 222)
     , say (case pRes p of
              Just r -> show (length (wSamples w)) ++ " images, 256 dims. L1 = " ++ fmtN (vix (pgVals r) 0) 3 ++ " (" ++ fmtN (100 * vix (pgVals r) 0 / pgTrace r) 1 ++ "%)  L2 = "
                        ++ fmtN (vix (pgVals r) 1) 3 ++ " (" ++ fmtN (100 * vix (pgVals r) 1 / pgTrace r) 1 ++ "%)"
              Nothing -> "PCA needs 3 or more images") gx (gy + gs + 26) 15 (grayC 220) ]
  ++ [ say ("Nearest on the plane to your drawing: " ++ show l) gx (gy + gs + 50) 15 (rgbC 255 225 140) | Just (_, l) <- [nearInfo] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    p = wPca w
    (px, py, ps) = padGeo env
    (gx, gy, gs) = plotGeo env
    rr = max 1e-9 (foldl' (\m (a, b) -> max m (max (abs a) (abs b))) 0 (pScores p)) * 1.15
    toPlot (a, b) = (gx + gs / 2 + f2 (a / rr) * gs / 2, gy + gs / 2 - f2 (b / rr) * gs / 2)
    hasCur = any (not . null) (wStrokes w)
    nearInfo = case pRes p of
      Just r | hasCur ->
        let s = mrow (projectData (V.singleton (wCur w)) (pMu p) (pgVecs r) 2) 0
            (a, b) = (vix s 0, vix s 1)
            best = foldl' (\(bd, bl) (sm, (c, d)) -> let d2 = (c - a) ^ (2 :: Int) + (d - b) ^ (2 :: Int) in if d2 < bd then (d2, sLab sm) else (bd, bl))
                          (1 / 0, -1) (zip (wSamples w) (pScores p))
        in if snd best >= 0 then Just (toPlot (a, b), snd best) else Just (toPlot (a, b), -1)
      _ -> Nothing

inPad :: Env -> (Float, Float) -> Bool
inPad env (x, y) = let (px, py, ps) = padGeo env in x > px && x < px + ps && y > py && y < py + ps

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p0 ->
    let (x, y) = toP5 env p0
    in pure (if inPad env (x, y) && length (wStrokes w) < 20 then redraw w { wStrokes = wStrokes w ++ [[(realToFrac x, realToFrac y)]] } else w)
  EventMotion p0 | envDown env ->
    let (x, y) = toP5 env p0
        (xd, yd) = (realToFrac x, realToFrac y)
    in pure (case reverse (wStrokes w) of
               (st : rest) | inPad env (x, y) ->
                 let (lx, ly) = if null st then (1 / 0, 0) else last st
                 in if hypot (xd - lx) (yd - ly) > 3 && length st < 400 then redraw w { wStrokes = reverse rest ++ [st ++ [(xd, yd)]] } else w
               _ -> w)
  EventKey (SpecialKey KeySpace) Down _ _
    | any (\s -> length s > 1) (wStrokes w) ->
        pure (analyseW w { wSamples = wSamples w ++ [Sample (rasterStrokes (wStrokes w) gn) (wLabel w) True] , wStrokes = [], wCur = rasterStrokes [] gn })
  EventKey (Char c) Down _ _
    | c `elem` "0127" -> pure w { wLabel = fromEnum c - fromEnum '0' }
    | c `elem` "cC" -> pure (redraw w { wStrokes = [] })
    | c `elem` "zZ" -> pure (case break sMine (reverse (wSamples w)) of
                               (after, _ : before) -> analyseW w { wSamples = reverse before ++ reverse after }
                               _ -> w)
    | c `elem` "rR" -> let mine = filter sMine (wSamples w)
                           w1 = makeSamples w { wSeed = wSeed w + 1 }
                       in pure (analyseW w1 { wSamples = wSamples w1 ++ mine })
  _ -> pure w
  where redraw w' = w' { wCur = rasterStrokes (wStrokes w') gn }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("components from the Gram matrix satisfy C v = L v (checked without making C, 1e-8 x trace)",
      and [ U.and (U.zipWith (\x vj -> abs (x - vix (pgVals r) k * vj) < 1e-8 * tr) (cv v) v) | (k, v) <- zip [0 ..] (V.toList (pgVecs r)) ])
  , ("same top 2 as subspace iteration directly in 256 dimensions", and [ near l (vix (pgVals r) k) (1e-8 * tr) | (k, l) <- zip [0 ..] (U.toList (eVals dd)) ]
                                                                    && and [ angleBetween v (mrow (pgVecs r) k) < 1e-3 | (k, v) <- zip [0 ..] (V.toList (eVecs dd)) ])
  , ("16x16 rasterizing normalizes position and size (shifted and 3x gives the same cells)", U.and (U.zipWith (\u v -> near u v 1e-12) ra rb))
  , ("empty drawing or a single point gives no NaN", noNaN (U.toList (rasterStrokes [] gn)) && noNaN (U.toList (rasterStrokes [[(5, 5)]] gn)))
  , ("on the PC plane, mean distance within a digit < mean distance between digits", within / nw < between / nb)
  , ("nearest example on the plane has the same label >= 70% (leave one out)", fromIntegral okNN / fromIntegral n >= (0.7 :: Double)) ]
  where
    (ss, r, dd, ra, rb) = withSeed 109 $ do
      ss' <- forM [0 .. 59 :: Int] $ \i -> do
        let lab = nth 0 labels (i `mod` 4)
        st <- sampleGlyph lab
        pure (rasterStrokes st gn, lab)
      let x = V.fromList (map fst ss')
          xc' = centerCols x (colMeans x)
      r' <- pcaGram xc' 2 300
      d' <- subspaceEigen (cvOf xc') dimG 2 400 1e-14
      st <- nth [] <$> sampleGlyph 2 <*> pure 0
      pure (ss', r', d', rasterStrokes [st] gn, rasterStrokes [map (\(a, b) -> (3 * a + 50, 3 * b - 7)) st] gn)
    x = V.fromList (map fst ss)
    mu = colMeans x
    xc = centerCols x mu
    n = length ss
    cvOf m v = V.foldl' (\out row -> let t = dotv row v in U.zipWith (\o rj -> o + (t * rj) / fromIntegral (nRows m - 1)) out row) (U.replicate dimG 0) m
    cv = cvOf xc
    tr = pgTrace r
    sc = [ (vix s 0, vix s 1) | s <- V.toList (projectData x mu (pgVecs r) 2) ]
    labs = map snd ss
    pairs = [ (li == lj, hypot (a - c) (b - d)) | (i, (a, b), li) <- zip3 [0 :: Int ..] sc labs, (j, (c, d), lj) <- zip3 [0 ..] sc labs, i /= j ]
    within = sum [ d | (True, d) <- pairs ]
    nw = fromIntegral (length [ () | (True, _) <- pairs ]) :: Double
    between = sum [ d | (False, d) <- pairs ]
    nb = fromIntegral (length [ () | (False, _) <- pairs ]) :: Double
    okNN = length [ () | (i, (a, b), li) <- zip3 [0 :: Int ..] sc labs
                       , let bl = snd (foldl' (\(bd, l) (j, (c, d), lj) -> let dd' = hypot (a - c) (b - d) in if j /= i && dd' < bd then (dd', lj) else (bd, l)) (1 / 0, -1) (zip3 [0 ..] sc labs))
                       , bl == li ]

-- ---------- やってみよう ----------
-- 1. 2 を 3 枚描いて追加しよう。お手本の 2 の近くに集まる？
-- 2. わざと崩した文字（大きな 0、細い 0）を描くと、平面のどこに来る？
-- 3. 平面で 1 と 7 が近いのはなぜ？ PC1・PC2 の画像を見て考えよう
-- 4. お手本にない文字（例 4）をラベル 7 として追加すると、主成分はどう変わる？
-- 5. 2 次元では分かれない文字も、3 次元目まで使うと分かれる？（pcaGram の 2 を 3 に）

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
-- ---- 手書きの線を N×N のマスに（大きさと位置をそろえてから、線からの距離でぼかして塗る）
rasterStrokes :: [[(Double, Double)]] -> Int -> Vec
rasterStrokes strokes n
  | null pts = U.replicate (n * n) 0
  | otherwise = U.generate (n * n) (\i -> let x = fromIntegral (i `mod` n)
                                              y = fromIntegral (i `div` n)
                                          in clampv (1.6 - foldl' (\b s -> min b (segDist x y s)) 1e9 segs) 0 1)
  where
    pts = concat strokes
    xa = minimum (map fst pts)
    xb = maximum (map fst pts)
    ya = minimum (map snd pts)
    yb = maximum (map snd pts)
    sc = maximum [xb - xa, yb - ya, 1e-9]
    cx = (xa + xb) / 2
    cy = (ya + yb) / 2
    nn = fromIntegral (n - 1)
    tr (px, py) = ((px - cx) / sc * 0.8 * nn + nn / 2, (py - cy) / sc * 0.8 * nn + nn / 2)
    segs = concat [ case q of
                      [a] -> [(a, a)]
                      _ -> zip q (drop 1 q)
                  | st <- strokes, let q = map tr st ]
    segDist x y ((ax, ay), (bx, by)) =
      let dx = bx - ax
          dy = by - ay
          l2 = dx * dx + dy * dy
          t = if l2 > 0 then clampv (((x - ax) * dx + (y - ay) * dy) / l2) 0 1 else 0
      in hypot (x - ax - t * dx) (y - ay - t * dy)

-- お手本の文字（0・1・2・7）を少しずつゆがめて作る。座標は 0〜1（乱数を引く順は p5.js 版と同じ）
sampleGlyph :: Int -> R [[(Double, Double)]]
sampleGlyph kind = do
  sl <- (\u -> (u - 0.5) * 0.3) <$> rnd
  let j = (\u -> (u - 0.5) * 0.08) <$> rnd
      pp x y = do a <- j; b <- j; pure (x + sl * (0.5 - y) + a * 0.3, y + b * 0.3)
  case kind of
    0 -> do a <- rndRange 0.22 0.32
            b <- rndRange 0.36 0.44
            out <- forM [0 .. 24 :: Int] $ \i -> let t = 2 * pi * fromIntegral i / 24 in pp (0.5 + a * sin t) (0.5 - b * cos t)
            pure [out]
    1 -> do f <- (< 0.6) <$> rnd
            first <- if f then (: []) <$> pp 0.38 0.25 else pure []
            rest <- sequence [pp 0.5 0.1, pp 0.5 0.5, pp 0.5 0.9]
            pure [first ++ rest]
    2 -> do arc <- forM [0 .. 10 :: Int] $ \i -> let t = pi * (1.1 - 1.25 * fromIntegral i / 10) in pp (0.5 + 0.28 * cos t) (0.33 - 0.22 * sin t)
            p1 <- pp 0.25 0.88
            jx <- j
            p2 <- pp (0.78 + jx) 0.88
            pure [arc ++ [p1, p2]]
    _ -> do p1 <- pp 0.22 0.12
            p2 <- pp 0.78 0.12
            jx <- j
            p3 <- pp (0.45 + jx) 0.9
            pure [[p1, p2, p3]]
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
