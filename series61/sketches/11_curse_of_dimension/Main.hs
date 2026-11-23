-- ============================================
-- 61-11 次元の呪い ─ 次元 d が大きくなると「近い」「遠い」の差がなくなっていく
--   左：d 次元の立方体 [0, 1]^d にランダムな点を 200 個。ある点からの距離の分布は、d が大きいと平均のまわりに集まり、
--   いちばん遠い点 ÷ いちばん近い点 → 1。距離で「似たもの」を探すクラスタリングには困ったこと（平均距離 ≈ √(d/6)）
--   右：2 次元の 3 つのかたまりに、意味のない「ノイズの次元」を m 個足す → k-means の ARI が下がる
--   PCA（主成分分析、シリーズ 60）でばらつきの大きい 2 方向だけ残してから k-means すると、また分けられる
--   右の実験は重いので、k-means の 1 回・PCA のくり返し数回ずつ、何フレームかに分けて計算する
-- 操作：↑↓ 次元 d（左）/ ← → ノイズの次元 m（右）/ R 新しい乱数
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Control.Monad (forM, forM_, replicateM, when, foldM)
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

dims :: [Int]
dims = [1, 2, 3, 5, 10, 20, 50, 100, 200, 500, 1000]   -- ★ 調べる次元

ms :: [Int]
ms = [0, 50, 100, 200, 300, 400, 600]                  -- ★ 足すノイズの次元の数

np, nq :: Int
np = 200                     -- ★ 点の数
nq = 5                       -- ★ 基準点の数

noiseSd, blobR :: Double
noiseSd = 0.5                -- ★ ノイズの大きさ
blobR = 1.3                  -- ★ かたまりの離れ具合

-- d 次元での距離の集まり
data Conc = Conc { cD :: !Int, cDists :: !(U.Vector Double), cRatio :: !Double, cMean :: !Double, cSd :: !Double }

-- ノイズの次元 m の実験の結果
data NoiseRes = NoiseRes { nrM :: !Int, nrPts :: !Pts, nrKm :: !(U.Vector Int), nrKp :: !(U.Vector Int), nrProj :: !Pts, nrAriK :: !Double, nrAriP :: !Double }

data World = World
  { wConc   :: ![Conc]
  , wConcNew :: ![Conc]             -- R のあと 1 フレームに 1 つずつ作っている途中（全部できたら入れかえる）
  , wConcStale :: !Bool             -- wConc が古い（作りなおし中）
  , wNoise  :: ![NoiseRes]          -- できた順
  , wJob    :: !(Maybe Exp)         -- 計算中の実験
  , wDSel   :: !Int
  , wMSel   :: !Int
  , wSeedC  :: !Int
  , wSeedN  :: !Int
  , wTest   :: ![(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-11 Curse of dimensionality" (recompute (World [] [] True [] Nothing 5 5 1 5 [])))
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = finishConc (recompute (World [] [] True [] Nothing 5 5 1 5 []))
      foldl' (\s c -> s + cRatio c) 0 (wConc w) `seq` pure w { wTest = selfTest }
  , skStep  = \env _ w -> pure (step (envFrame env) w)
  , skDraw  = draw
  , skEvent = event
  }

recompute :: World -> World
recompute w = w { wConcNew = [], wConcStale = True, wNoise = [], wJob = Just (expStart (headOr 0 ms) (wSeedN w)) }

-- 次の次元の距離を 1 つ計算する
concStep :: World -> World
concStep w
  | k >= length dims = w
  | otherwise = let c = concentration d np nq (fromIntegral (1000 * wSeedC w + d))
                    new = wConcNew w ++ [c]
                in cRatio c `seq` if length new == length dims then w { wConc = new, wConcNew = [], wConcStale = False } else w { wConcNew = new }
  where
    k = length (wConcNew w)
    d = headOr 1 (drop k dims)

concBusy :: World -> Bool
concBusy = wConcStale

-- 初めは全部まとめて
finishConc :: World -> World
finishConc w = if concBusy w then finishConc (concStep w) else w

-- ★ d 次元の一様な点で、基準点から全部の点までの距離を調べる
concentration :: Int -> Int -> Int -> Word32 -> Conc
concentration d n q seed = Conc d dists (meanOf ratios) mean sd
  where
    pts = fst (runR (uniformN (n * d)) (seedRng seed))       -- 点 i の座標 j = pts[i·d + j]
    row i = U.slice (i * d) d pts
    perA = [ U.fromList [ sqrt (U.foldl' (+) 0 (U.zipWith (\x y -> (x - y) * (x - y)) (row a) (row i))) | i <- [0 .. n - 1], i /= a ] | a <- [0 .. q - 1] ]
    ratios = [ U.maximum ds / max 1e-12 (U.minimum ds) | ds <- perA, not (U.null ds) ]
    dists = U.concat perA
    mean = mean' dists
    sd = sqrt (mean' (U.map (\v -> (v - mean) * (v - mean)) dists))
    mean' v = if U.null v then 0 else U.foldl' (+) 0 v / fromIntegral (U.length v)

-- 一様乱数・正規乱数を k 個（速いように ST の中でまとめて引く）
uniformN, gaussN :: Int -> R (U.Vector Double)
uniformN k = bulk k rnd
gaussN k = bulk k gauss

bulk :: Int -> R Double -> R (U.Vector Double)
bulk k gen = R $ \s0 -> runST $ do
  v <- UM.new k
  let go !j s | j >= k = pure s
              | otherwise = let (x, s') = runR gen s in x `seq` UM.unsafeWrite v j x >> go (j + 1) s'
  s1 <- go 0 s0
  fv <- U.unsafeFreeze v
  pure (fv, s1)

-- ★ 3 つのかたまり（2 次元）+ ノイズの次元 m 個
--   （点 i のグループ = i mod 3。p5.js 版の noisyBlobs の 1 点ぶん。種は実験の初めに 1 回だけ）
noisyPoint :: Int -> Int -> R P
noisyPoint m i = do
  let k = i `mod` 3
      a = 2 * pi * fromIntegral k / 3
  gx <- gauss
  gy <- gauss
  noise <- gaussN m
  pure (U.fromList [blobR * cos a + 0.3 * gx, blobR * sin a + 0.3 * gy] U.++ U.map (noiseSd *) noise)

-- ---- 実験 1 つを、何フレームかに分けて計算する（k-means 1 回ずつ、PCA のくり返し数回ずつ）
-- 段階：0 データを作る → 1 k-means（3 回やり直し、1 フレームに数回のくり返し）→ 2 PCA → 3 PCA の 2 次元で k-means・ARI
data Run = Run { rC :: !Pts, rL :: !(U.Vector Int), rHist :: ![Double], rIt :: !Int }

data Exp = Exp
  { eM :: !Int, eSeed :: !Int, eStage :: !Int
  , ePts :: !Pts, eTruth :: !(U.Vector Int), eRng :: !Rng
  , eRestart :: !Int, eRun :: !(Maybe Run), eBest :: !(Maybe (Double, U.Vector Int))
  , ePca :: !(Maybe Pca) }

expStart :: Int -> Int -> Exp
expStart m seed = Exp m seed 0 V.empty U.empty (seedRng 1) 0 Nothing Nothing Nothing

-- 1 フレームぶん進める。終わったら Right 結果
expStep :: Exp -> Either Exp NoiseRes
expStep e = case eStage e of
  0 -> let i0 = V.length (ePts e)
           rng0 = if i0 == 0 then seedRng (fromIntegral (eSeed e)) else eRng e
           cnt = min (300 - i0) (max 1 (60000 `div` (eM e + 2)))     -- 1 フレームに作る点の数
           (new, r') = runR (mapM (noisyPoint (eM e)) [i0 .. i0 + cnt - 1]) rng0
           pts' = ePts e V.++ V.fromList new
       in V.foldl' (\a q -> a + U.length q) 0 pts' `seq`
          if V.length pts' >= 300 then Left e { ePts = pts', eTruth = U.generate 300 (`mod` 3), eStage = 1, eRng = seedRng 1 }
          else Left e { ePts = pts', eRng = r' }
  1 -> Left (kmStep e)
  2 -> Left (case ePca e of
               Nothing -> e { ePca = Just (pcaStart (ePts e) 2 60) }
               Just pc -> let pc' = pcaRun (chunkPca (ePts e)) pc in if pcDone pc' then e { ePca = Just pc', eStage = 3 } else e { ePca = Just pc' })
  _ -> let pc = maybe (pcaStart V.empty 2 60) id (ePca e)
           proj = pcaProj pc
           kp = withSeed 1 (kmeansBest proj 3 3 True 60)
           kmL = maybe U.empty snd (eBest e)
       in Right (NoiseRes (eM e) (ePts e) kmL (kmLabels kp) proj (ariScore kmL (eTruth e)) (ariScore (kmLabels kp) (eTruth e)))

-- 1 フレームの k-means のくり返し回数（次元が大きいほど少なく）
chunkKm, chunkPca :: Pts -> Int
chunkKm pts = max 1 (1500000 `div` max 1 (8 * V.length pts * U.length (V.head pts)))
chunkPca pts = max 1 (750000 `div` max 1 (2 * V.length pts * U.length (V.head pts)))

-- kmeansBest pts 3 3 True 60 と同じ計算（ロイドのくり返しを少しずつ）
kmStep :: Exp -> Exp
kmStep e = case eRun e of
  Nothing
    | eRestart e >= 3 -> e { eStage = 2 }
    | otherwise ->
        let (c0, r') = runR (initPlusPlus pts 3) (eRng e)
            l0 = assignLabels pts c0
        in e { eRun = Just (Run c0 l0 [inertiaOf pts c0 l0] 0), eRng = r' }
  Just run -> go (chunkKm pts) run
  where
    pts = ePts e
    go 0 run = e { eRun = Just run }
    go k run
      | rIt run >= 60 = finish run
      | otherwise =
          let nc = updateCenters pts (rL run) (rC run)
              h1 = inertiaOf pts nc (rL run)
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              run' = Run nc nl (h2 : h1 : rHist run) (rIt run + 1)
          in if nl == rL run then finish run' else go (k - 1 :: Int) run'
    finish run =
      let inertia = headOr 0 (rHist run)
          best = case eBest e of
            Just (bi, bl) | not (inertia < bi) -> Just (bi, bl)
            _ -> Just (inertia, rL run)
      in e { eRun = Nothing, eRestart = eRestart e + 1, eBest = best }

-- ★ PCA：共分散がいちばん大きい方向をべき乗法で 1 本ずつ（前の方向を引いてから）
data Pca = Pca { pcN :: !Int, pcD :: !Int, pcX :: !(U.Vector Double)   -- 平均を引いたデータ（n × D）
               , pcDims :: !Int, pcIters :: !Int, pcComp :: !Int, pcIt :: !Int
               , pcV :: !(U.Vector Double), pcLam :: !Double, pcVecs :: ![U.Vector Double], pcLams :: ![Double], pcDone :: !Bool }

pcaStart :: Pts -> Int -> Int -> Pca
pcaStart pts nd iters = Pca n d x nd (if iters <= 0 then 100 else iters) 0 0 (v0 0) 0 [] [] (n == 0 || nd <= 0)
  where
    n = V.length pts
    d = if n == 0 then 0 else U.length (V.head pts)
    mu = V.foldl' (\acc p -> U.zipWith (\a b -> a + b / fromIntegral n) acc p) (U.replicate d 0) pts
    x = U.concat [ U.zipWith (-) p mu | p <- V.toList pts ]
    v0 c = U.generate d (\j -> sin (1.3 * fromIntegral j + fromIntegral c) + 0.5)

-- べき乗法を k 回すすめる（1 本ぶん終わったら次の向きへ）
pcaRun :: Int -> Pca -> Pca
pcaRun k pc
  | pcDone pc || k <= 0 = pc
  | pcIt pc >= pcIters pc =
      let vecs = pcVecs pc ++ [pcV pc]
          c = pcComp pc + 1
      in pcaRun k pc { pcVecs = vecs, pcLams = pcLams pc ++ [pcLam pc], pcComp = c, pcIt = 0, pcLam = 0
                     , pcV = U.generate (pcD pc) (\j -> sin (1.3 * fromIntegral j + fromIntegral c) + 0.5), pcDone = c >= pcDims pc }
  | otherwise = pcaRun (k - 1) (pcaIter pc)

pcaIter :: Pca -> Pca
pcaIter pc = pc { pcV = U.map (/ max 1e-300 lam) w, pcLam = lam, pcIt = pcIt pc + 1 }
  where
    n = pcN pc
    d = pcD pc
    x = pcX pc
    v = pcV pc
    s = U.generate n (\i -> U.foldl' (+) 0 (U.zipWith (*) (U.slice (i * d) d x) v))
    w0 = runST $ do
      acc <- UM.replicate d 0 :: ST s (UM.MVector s Double)
      forM_ [0 .. n - 1] $ \i -> do
        let si = s U.! i
        forM_ [0 .. d - 1] $ \j -> UM.unsafeModify acc (\a -> a + (U.unsafeIndex x (i * d + j) * si) / fromIntegral n) j
      U.freeze acc
    w = foldl' (\wv u -> let dd = U.foldl' (+) 0 (U.zipWith (*) wv u) in U.zipWith (\a b -> a - dd * b) wv u) w0 (pcVecs pc)
    lam = sqrt (U.foldl' (\a b -> a + b * b) 0 w)

pcaFull :: Pts -> Int -> Int -> Pca           -- 全部まとめて（自己テスト用）
pcaFull pts nd iters = pcaRun maxBound (pcaStart pts nd iters)

pcaProj :: Pca -> Pts                         -- 各点を pcDims 本の向きに射影した座標
pcaProj pc = V.generate (pcN pc) (\i -> let xi = U.slice (i * pcD pc) (pcD pc) (pcX pc) in U.fromList [ U.foldl' (+) 0 (U.zipWith (*) xi v) | v <- pcVecs pc ])

noiseExperiment :: Int -> Int -> NoiseRes      -- 実験を最後まで（自己テスト用）
noiseExperiment m seed = go (expStart m seed)
  where go e = either go id (expStep e)

-- p5.js 版と同じく 2 フレームに 1 回だけ重い計算をする（奇数フレーム = 距離、偶数フレーム = 実験）
step :: Int -> World -> World
step frame w
  | odd frame = if concBusy w then concStep w else w
  | otherwise = stepExp w

stepExp :: World -> World
stepExp w = case wJob w of
  Nothing -> w
  Just e -> case expStep e of
    Left e' -> w { wJob = Just e' }
    Right r -> let done = wNoise w ++ [r]
                   k = length done
               in w { wNoise = done, wJob = if k < length ms then Just (expStart (headOr 0 (drop k ms)) (wSeedN w)) else Nothing }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 距離の分布（平均で割った値）
  [ plotAxes g1 "distance / mean distance" ("d = " ++ show (cD c) ++ ": distance distribution (" ++ show nq ++ " reference points x " ++ show (np - 1) ++ " points)") [0, 0.5, 1, 1.5, 2] [] ]
  ++ [ Color (rgbC 176 140 255) (rectTL (gX g1 (fromIntegral i / 20)) (gY g1 (fromIntegral b / fromIntegral bm)) (pW g1 / 40 - 1) (pY g1 + pH g1 - gY g1 (fromIntegral b / fromIntegral bm)))
     | (i, b) <- zip [0 :: Int ..] (U.toList bins), b > 0 ]
  -- ---- 最大 ÷ 最小 のグラフ（横は log d）
  ++ [ plotAxes g2 "log10 d" "log10 (farthest / nearest)  -> 0 means ratio 1" [0, 1, 2, 3] [0, fromIntegral (jsRoundI (y1 g2 * 10)) / 10]
     , Color (rgbC 255 190 80) (polyline 2 [ (gX g2 (logBase 10 (fromIntegral (cD e))), gY g2 (logBase 10 (cRatio e))) | e <- wConc w ]) ]
  ++ [ Color (if i == wDSel w then white else rgbC 255 190 80) (circleAt (gX g2 (logBase 10 (fromIntegral (cD e)))) (gY g2 (logBase 10 (cRatio e))) (if i == wDSel w then 12 else 6))
     | (i, e) <- zip [0 ..] (wConc w) ]
  ++ [ sayWrap ("d = " ++ show (cD c) ++ ": max / min = " ++ fmt 3 (cRatio c) ++ "  mean distance " ++ fmt 3 (cMean c) ++ " (sqrt(d/6) = " ++ fmt 3 (sqrt (fromIntegral (cD c) / 6)) ++ ")  spread / mean = " ++ fmt 3 (cSd c / cMean c))
               60 (envH env - 66) half 15 (grayC 220) ]
  -- ---- 右：ノイズの次元と k-means
  ++ [ plotAxes g3 "noise dimensions m" "ARI vs truth: red = k-means as is, green = PCA to 2-D first" [0, 200, 400, 600] [0, 0.5, 1]
     , Color (rgbC 255 120 130) (polyline 2 [ (gX g3 (fromIntegral (nrM e)), gY g3 (nrAriK e)) | e <- wNoise w ])
     , Color (rgbC 110 230 190) (polyline 2 [ (gX g3 (fromIntegral (nrM e)), gY g3 (nrAriP e)) | e <- wNoise w ]) ]
  ++ concat [ [ Color (rgbC 255 120 130) (circleAt (gX g3 (fromIntegral (nrM e))) (gY g3 (nrAriK e)) sz), Color (rgbC 110 230 190) (circleAt (gX g3 (fromIntegral (nrM e))) (gY g3 (nrAriP e)) sz) ]
            | (i, e) <- zip [0 ..] (wNoise w), let sz = if i == wMSel w then 12 else 6 ]
  ++ (case cur of
        Just e ->
          let vA = fitView rx y2 (rw / 2 - 20) hh 0 0 4.0
              vB = fitView (rx + rw / 2 + 20) y2 (rw / 2 - 20) hh 0 0 (max 1 (2.2 * V.foldl' (\a p -> max a (abs (ptX p))) 0 (nrProj e)))
          in [ Color (grayC 70) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) | v <- [vA, vB] ]
             ++ [ drawPts vA (V.map (U.take 2) (nrPts e)) (Just (nrKm e)) 5 255, drawPts vB (nrProj e) (Just (nrKp e)) 5 255
                , say (show (2 + nrM e) ++ " dims as is: k-means ARI " ++ fmt 2 (nrAriK e)) (pX vA) (pY vA - 8) 14 (grayC 200)
                , say ("PCA -> k-means: ARI " ++ fmt 2 (nrAriP e)) (pX vB) (pY vB - 8) 14 (grayC 200) ]
        Nothing -> [ say "computing ..." rx (y2 + 20) 14 (grayC 150) ])
  -- ---- 文字
  ++ [ say ("Curse of dimensionality   d = " ++ show (cD c) ++ ": max / min = " ++ fmt 2 (cRatio c) ++ "   |   noise dims m = " ++ show mNow
            ++ (case cur of { Just e | nrM e == mNow -> ": k-means ARI " ++ fmt 2 (nrAriK e) ++ " -> after PCA " ++ fmt 2 (nrAriP e); _ -> "" })) 40 40 20 (grayC 240)
     , say ("Up/Down dimension d (left)   <- -> noise dimensions m (right)   R new random numbers" ++ (if busy then "   (computing m = " ++ show (maybe 0 eM (wJob w)) ++ " ...)" else "")) 40 66 15 (grayC 170)
     , sayWrap ("Each noise dim is small (sd " ++ show noiseSd ++ "), but m of them drown the distance differences. PCA keeps the 2 directions of largest spread. Bottom: first 2 dims (left), 2 PCA directions (right).")
               rx (envH env - 98) (rw - 160) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    width = envW env
    height = envH env
    c = headOr (Conc 1 U.empty 1 1 0) (drop (wDSel w) (wConc w))
    half = width / 2 - 60
    hh = (height - 330) * 0.5
    y2 = 130 + hh + 80
    g1 = G 60 130 (half - 40) hh 0 2 0 1
    bins = U.accumulate (+) (U.replicate 40 (0 :: Int)) (U.map (\v -> (max 0 (min 39 (floorI (v / cMean c * 20))), 1)) (cDists c))
    bm = max 1 (U.maximum bins)
    g2 = G 60 y2 (half - 40) hh 0 3 0 (logBase 10 (foldl' max 1e-9 (map cRatio (wConc w))) * 1.05)
    rx = width / 2 + 40
    rw = width / 2 - 80
    g3 = G rx 130 rw hh 0 (fromIntegral (last ms)) (-0.1) 1.05
    cur = headOr Nothing (map Just (drop (min (wMSel w) (length (wNoise w) - 1)) (wNoise w)))
    mNow = headOr 0 (drop (wMSel w) ms)
    busy = maybe False (const True) (wJob w)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wDSel = min (length dims - 1) (wDSel w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wDSel = max 0 (wDSel w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wMSel = min (length ms - 1) (wMSel w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wMSel = max 0 (wMSel w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> recompute w { wSeedC = wSeedC w + 1, wSeedN = wSeedN w + 1 }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("max / min approaches 1 as d grows: " ++ joinArrow [ fmt 2 (cRatio e) | e <- cs ], and (zipWith (\a b -> cRatio b < cRatio a) cs (drop 1 cs)) && cRatio (cs !! 3) < 1.3 && cRatio (cs !! 0) > 5)
  , ("mean distance ~ sqrt(d/6) (within 3% for d = 100, 1000)", all (\e -> abs (cMean e / sqrt (fromIntegral (cD e) / 6) - 1) < 0.03) (drop 2 cs))
  , ("spread / mean < 0.05 at d = 1000 (distances are almost equal)", cSd (cs !! 3) / cMean (cs !! 3) < 0.05)
  , ("no noise: k-means ARI " ++ fmt 2 (nrAriK e0) ++ " >= 0.95. 400 noise dims: ARI " ++ fmt 2 (nrAriK e4) ++ " < 0.6", nrAriK e0 >= 0.95 && nrAriK e4 < 0.6)
  , ("after PCA to 2-D it recovers: ARI " ++ fmt 2 (nrAriP e4) ++ " >= 0.85", nrAriP e4 >= 0.85)
  , ("power-method direction v is an eigenvector of the covariance C: |C v - lambda v| < 1e-9", and (zipWith (\x a -> abs (x - lam0 * v U.! a) < 1e-9) cv [0 ..]))
  ]
  where
    cs = [ concentration d 150 3 (fromIntegral (50 + d)) | d <- [2, 10, 100, 1000] ]
    joinArrow xs = foldr1 (\a b -> a ++ " -> " ++ b) xs
    e0 = noiseExperiment 0 5
    e4 = noiseExperiment 400 5
    small = withSeed 3 (V.fromList <$> mapM (\_ -> do { a <- gauss; b <- gauss; c <- gauss; pure (U.fromList [a * 2, b, c * 0.5]) }) [1 .. 60 :: Int])
    pc = pcaFull small 1 300
    v = headOr U.empty (pcVecs pc)
    lam0 = headOr 0 (pcLams pc)
    xs = [ U.slice (i * 3) 3 (pcX pc) | i <- [0 .. 59] ]
    cv = [ foldl' (\s b -> s + meanOf [ x U.! a * x U.! b | x <- xs ] * v U.! b) 0 [0 .. 2] | a <- [0 .. 2] ]

-- ---------- やってみよう ----------
-- 1. ↑↓ で d を 1 → 1000 に。距離の分布（左上）はどんどん細くなる。「いちばん近い点」に意味はある？
-- 2. ← → で m を変えて、右下の 2 つの図を比べよう。m = 600 ではそのままの k-means はほぼでたらめ
-- 3. noiseSd を 0.3 や 0.8 に変えると？ PCA でも回復できなくなるのはどんなとき？
-- 4. 02 のシルエットも距離で計算する。高い次元では a と b の差が小さくなって 0 に近づく
-- 5. シリーズ 60（PCA）で、主成分がばらつきの大きい向きになることをくわしく見よう

-- ==================================================
-- ここから下は「クラスタリングの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点は U.Vector Double（[x, y] や [r, g, b]）、
-- 点の集まりは V.Vector（p5.js 版の配列の配列）、ラベルは U.Vector Int（−1 = ノイズ）
-- ==================================================
-- ---- 共通：クラスタの色・距離（点は [x, y] や [r, g, b] のような U.Vector Double）
type P = U.Vector Double
type Pts = V.Vector P

pt2 :: Double -> Double -> P                -- 2 次元の点 [x, y]
pt2 x y = U.fromList [x, y]

ptX, ptY :: P -> Double                     -- x 座標・y 座標（なければ 0）
ptX p = if U.length p > 0 then U.unsafeIndex p 0 else 0
ptY p = if U.length p > 1 then U.unsafeIndex p 1 else 0

palette :: [(Int, Int, Int)]
palette = [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130), (200, 240, 110), (255, 150, 220), (90, 210, 220), (255, 150, 70), (190, 190, 255)]

palRGB :: Int -> (Int, Int, Int)            -- クラスタ k の色（k < 0 = ノイズ = 灰色）
palRGB k | k < 0 = (120, 120, 135)
         | otherwise = headOr (0, 0, 0) (drop (k `mod` length palette) palette)

palC :: Int -> Color
palC k = palCA k 255

palCA :: Int -> Int -> Color                -- 不透明度 a（0〜255）つき
palCA k a = let (r, g, b) = palRGB k in rgbaC r g b a

sqd :: P -> P -> Double                     -- 距離の 2 乗
sqd a b = go 0 0
  where
    n = min (U.length a) (U.length b)
    go !d !s | d >= n = s
             | otherwise = let e = U.unsafeIndex a d - U.unsafeIndex b d in go (d + 1) (s + e * e)
{-# INLINE sqd #-}

edist :: P -> P -> Double                   -- ユークリッド距離
edist a b = sqrt (sqd a b)

meanOf :: [Double] -> Double
meanOf [] = 0
meanOf xs = foldl' (+) 0 xs / fromIntegral (length xs)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ。gauss の 2 つ目の値もとっておく）
data Rng = Rng !Word32 !(Maybe Double)

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s) Nothing

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s sp) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, Rng a sp)   -- Int を通すと速い

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

seedR :: Word32 -> R ()                     -- 計算の途中で seedRng(s)
seedR s = R (\_ -> ((), seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int               -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

gauss :: R Double                           -- 標準正規分布（ボックス-ミュラー法）
gauss = R $ \(Rng s sp) -> case sp of
  Just g -> (g, Rng s Nothing)
  Nothing ->
    let (u0, r1) = rndStep (Rng s Nothing)
        (v, Rng s2 _) = rndStep r1
        u = 1 - u0
        r = sqrt (-2 * log u)
    in (r * cos (2 * pi * v), Rng s2 (Just (r * sin (2 * pi * v))))

-- ---- k-means（ロイドのアルゴリズム）。目的関数 = 慣性 = Σ |x_i − 中心|²
nearestCenter :: P -> Pts -> Int
nearestCenter p cs = go 0 0 (1 / 0)
  where
    k = V.length cs
    go !j !b !bd
      | j >= k = b
      | otherwise = let d = sqd p (V.unsafeIndex cs j) in if d < bd then go (j + 1) j d else go (j + 1) b bd

assignLabels :: Pts -> Pts -> U.Vector Int  -- ① 割り当て
assignLabels pts cs = U.generate (V.length pts) (\i -> nearestCenter (V.unsafeIndex pts i) cs)

inertiaOf :: Pts -> Pts -> U.Vector Int -> Double
inertiaOf pts cs labels = U.ifoldl' (\s i l -> s + sqd (pts V.! i) (cs V.! l)) 0 (U.take (V.length pts) labels)

-- ② 更新（仲間の平均。仲間 0 なら動かさない）
updateCenters :: Pts -> U.Vector Int -> Pts -> Pts
updateCenters pts labels cs
  | V.null cs = cs
  | otherwise = V.generate k (\j -> let c = cnt U.! j in if c > 0 then U.generate dim (\d -> sums U.! (j * dim + d) / fromIntegral c) else cs V.! j)
  where
    k = V.length cs
    dim = U.length (V.head cs)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) labels)
    sums = runST $ do
      s <- UM.replicate (k * dim) 0
      forM_ [0 .. V.length pts - 1] $ \i -> do
        let l = labels U.! i
            p = pts V.! i
        forM_ [0 .. min dim (U.length p) - 1] $ \d -> UM.unsafeModify s (+ U.unsafeIndex p d) (l * dim + d)
      U.freeze s

initRandomCenters :: Pts -> Int -> R Pts    -- ランダムに k 点えらぶ（重ならないように）
initRandomCenters pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = go 0 (U.enumFromN 0 n) []
  where
    n = V.length pts
    go j idx out
      | j >= min k n = pure (V.fromList (reverse out ++ replicate (k - length out) (V.head pts)))
      | otherwise = do
          u <- rnd
          let r = j + floor (u * fromIntegral (n - j))
              idx' = idx U.// [(j, idx U.! r), (r, idx U.! j)]
          go (j + 1) idx' (pts V.! (idx' U.! j) : out)

-- k-means++：今ある中心から遠い点ほど（距離² に比例して）えらばれやすい
initPlusPlus :: Pts -> Int -> R Pts
initPlusPlus pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = do
      u <- rnd
      let c0 = pts V.! min (n - 1) (floor (u * fromIntegral n))
      loop [c0] (U.generate n (\i -> sqd (pts V.! i) c0))
  where
    n = V.length pts
    loop out d2
      | length out >= k = pure (V.fromList (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pts V.! pick
          loop (c : out) (U.imap (\i v -> min v (sqd (pts V.! i) c)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

-- k-means の結果。hist = 半ステップ（割り当て・更新）ごとの慣性、all = やり直しごとの慣性
data KM = KM { kmCenters :: !Pts, kmLabels :: !(U.Vector Int), kmInertia :: !Double, kmHist :: ![Double], kmIters :: !Int, kmAll :: ![Double] }

-- 最後まで回す（maxIter 回まで。0 なら 100 回）
lloyd :: Pts -> Pts -> Int -> KM
lloyd pts c0 maxIter = go 0 c0 l0 [inertiaOf pts c0 l0]
  where
    mi = if maxIter <= 0 then 100 else maxIter
    l0 = assignLabels pts c0
    go it cs ls hist
      | it >= mi = finish it cs ls hist
      | otherwise =
          let nc = updateCenters pts ls cs
              h1 = inertiaOf pts nc ls
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              hist' = h2 : h1 : hist
          in if nl == ls then finish (it + 1) nc nl hist' else go (it + 1) nc nl hist'
    finish it cs ls hist = KM cs ls (headOr 0 hist) (reverse hist) it []

-- restarts 回やり直して慣性がいちばん小さいものを選ぶ（usePP = k-means++ で始める）
kmeansBest :: Pts -> Int -> Int -> Bool -> Int -> R KM
kmeansBest pts k restarts usePP maxIter = do
  rs <- forM [1 .. max 1 restarts] $ \_ -> do
    c0 <- if usePP then initPlusPlus pts k else initRandomCenters pts k
    pure (lloyd pts c0 maxIter)
  let best = foldl' (\b r -> if kmInertia r < kmInertia b then r else b) (headOr (KM V.empty U.empty 0 [] 0 []) rs) (drop 1 rs)
  pure best { kmAll = map kmInertia rs }

-- ---- 正解との一致：分割表、純度、調整ランド指数（ARI）、正規化相互情報量（NMI）。ラベル −1（ノイズ）も 1 つのグループとして数える
-- 分割表 M（行 = a のグループ、列 = b のグループ。どちらも小さい番号順）
contingency :: U.Vector Int -> U.Vector Int -> ([[Int]], [Int], [Int])
contingency a b = ([ [ M.findWithDefault 0 (x, y) cnt | y <- cb ] | x <- ra ], ra, cb)
  where
    ra = M.keys (M.fromList [ (x, ()) | x <- U.toList a ])
    cb = M.keys (M.fromList [ (y, ()) | y <- U.toList b ])
    cnt = M.fromListWith (+) [ ((x, y), 1 :: Int) | (x, y) <- zip (U.toList a) (U.toList b) ]

comb2 :: Double -> Double
comb2 x = x * (x - 1) / 2

ariScore :: U.Vector Int -> U.Vector Int -> Double
ariScore a b
  | U.null a = 1
  | otherwise = if maxv - expv == 0 then 1 else (sij - expv) / (maxv - expv)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a)
    sij = foldl' (+) 0 [ comb2 (fromIntegral v) | r <- mt, v <- r ]
    sa = foldl' (+) 0 [ comb2 (fromIntegral (sum r)) | r <- mt ]
    sb = foldl' (+) 0 [ comb2 (fromIntegral v) | v <- foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt ]
    expv = sa * sb / max 1 (comb2 n)
    maxv = (sa + sb) / 2

nmiScore :: U.Vector Int -> U.Vector Int -> Double   -- NMI = 2·I(A; B) / (H(A) + H(B))
nmiScore a b
  | U.null a = 1
  | otherwise = let hs = h rs + h cs in if hs == 0 then 1 else clampv 0 1 (2 * mi / hs)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a) :: Double
    rs = [ fromIntegral (sum r) | r <- mt ] :: [Double]
    cs = map fromIntegral (foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt) :: [Double]
    h arr = negate (foldl' (\s v -> s + (if v > 0 then (v / n) * log (v / n) else 0)) 0 arr)
    mi = foldl' (+) 0 [ (v / n) * log (n * v / (ri * cj)) | (r, ri) <- zip mt rs, (vi, cj) <- zip r cs, let v = fromIntegral vi, v > 0 ]

purityScore :: U.Vector Int -> U.Vector Int -> Double
purityScore pred' truth
  | U.null pred' = 0
  | otherwise = let (mt, _, _) = contingency pred' truth in fromIntegral (sum (map (foldl' max 0) mt)) / fromIntegral (U.length pred')

-- ---- グラフと図の道具：G = 画面の四角（左上 x, y、幅、高さ）とデータの範囲（p5.js と同じ、左上が原点）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

invX :: G -> Float -> Double                -- 画面の x → データの x
invX g sx = x0 g + realToFrac ((sx - pX g) / pW g) * (x1 g - x0 g)

invY :: G -> Float -> Double
invY g sy = y0 g + realToFrac ((pY g + pH g - sy) / pH g) * (y1 g - y0 g)

inBox :: G -> Float -> Float -> Bool
inBox g sx sy = sx >= pX g && sx <= pX g + pW g && sy >= pY g && sy <= pY g + pH g

-- 縦横の縮尺をそろえる（span = 短い辺に入るデータの幅、(cx, cy) = まん中）
fitView :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> G
fitView x y w h cx cy sp = G x y w h (cx - hw) (cx + hw) (cy - hh) (cy + hh)
  where
    s = realToFrac (min w h) / sp
    hw = realToFrac w / 2 / s
    hh = realToFrac h / 2 / s

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- クラスタの色で点を描く（ラベルがなければ白）。r = 直径、al = 不透明度（0〜255）
drawPts :: G -> Pts -> Maybe (U.Vector Int) -> Float -> Int -> Picture
drawPts g pts labels r al = Pictures
  [ Color (maybe (rgbaC 230 230 240 al) (\ls -> palCA (if i < U.length ls then ls U.! i else 0) al) labels) (circleAt (gX g (ptX p)) (gY g (ptY p)) r)
  | (i, p) <- zip [0 ..] (V.toList pts) ]

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
clampv :: Double -> Double -> Double -> Double   -- clampv 下限 上限 値
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = let s = showFFloat (Just n) (fin 0 v) "" in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

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
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

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

-- 右下に小さく（view の中で使う）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
