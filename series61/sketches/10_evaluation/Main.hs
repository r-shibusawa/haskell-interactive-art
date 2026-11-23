-- ============================================
-- 61-10 正解との一致を測る ─ 正解のラベルがあるとき、クラスタリングの結果がどれくらい合っているかを数にする
--   分割表 n_ij = 「結果のクラスタ i」かつ「正解のグループ j」の点の数。クラスタの番号は勝手につくので、番号どうしを比べてはだめ
--   純度 = 各クラスタでいちばん多い正解の数の合計 / n（1 点ずつばらばらにすると 1 になってしまう弱点）
--   調整ランド指数 ARI = 「2 点の組が、両方で同じ／両方で別」の一致を、でたらめのときの期待値で調整（1 = 完全一致、≈ 0 = でたらめ）
--   正規化相互情報量 NMI = 2·I(結果; 正解) / (H(結果) + H(正解))（0〜1）。どれも番号を入れかえても同じ値
--   7 つの方法の計算は重いので、R のあとは 1 フレームに 1 つずつ計算する
-- 操作：1〜7 方法 / L 番号を入れかえる / R 新しいデータ / スペース でたらめ（6）をやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.List (foldl')
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 240                   -- ★ 点の数

methods :: [String]
methods = ["k-means", "Gaussian mix", "DBSCAN", "single link", "Ward", "random", "1 point each"]

data Score = Score { sPur, sAri, sNmi :: !Double }

-- R のあとの計算の途中（1 フレームに 1 段ずつ）
data Pending = Pending
  { pData  :: !DS
  , pStage :: !Int
  , pRng   :: !Rng
  , pPreds :: ![U.Vector Int]       -- できた順（k-means, 混合ガウス, DBSCAN, 単連結, ウォード法）
  , pGmm   :: !(Maybe GFit)         -- 3 回のうち、いちばん尤度の高い混合ガウス
  , pEm    :: !(Maybe Em)           -- 計算中の EM
  , pAgg   :: !(Maybe Agg) }        -- 計算中の凝集型クラスタリング

data World = World
  { wData    :: !DS
  , wPreds   :: ![U.Vector Int]     -- 7 つの方法の結果（6 番目 = でたらめ）
  , wScores  :: ![Score]
  , wMethod  :: !Int
  , wPerm    :: !(Maybe [Int])      -- 番号の入れかえ（0〜9）
  , wSeedD   :: !Int
  , wSeedR   :: !Int
  , wPending :: !(Maybe Pending)
  , wTest    :: ![(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-10 Measuring agreement with the truth" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = finishAll (newData world0)
      length (wScores w) `seq` pure w { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (DS V.empty U.empty 0) [] [] 0 Nothing 101 1 Nothing []

finishAll :: World -> World
finishAll w = case wPending w of
  Nothing -> w
  Just _ -> finishAll (step w)

newData :: World -> World
newData w = w { wPending = Just (Pending d 1 (seedRng 1) [] Nothing Nothing Nothing) }
  where d = withSeed 1 (makeDataset "noise" nPts (fromIntegral (wSeedD w)))

-- ★ 1 フレームに 1 段ずつ：k-means → 混合ガウス 3 回 → DBSCAN → 単連結 → ウォード法 → でたらめと 1 点ずつ
step :: World -> World
step w = case wPending w of
  Nothing -> w
  Just p ->
    let pts = dsPts (pData p)
        n = V.length pts
    in case pStage p of
      1 -> let !lab = kmLabels (withRng (kmeansBest pts 3 10 True 100))
           in next p { pPreds = [lab], pRng = seedRng 2 }     -- 次は seedRng(2)
      s | s >= 2 && s <= 4 ->                                   -- 混合ガウス（1 フレームに EM 10 回ずつ）
           case pEm p of
             Nothing -> let (gm0, r) = runR (gmmInit pts 3) (pRng p)
                            !e0 = emStart pts gm0
                        in w { wPending = Just p { pEm = Just e0, pRng = r } }
             Just em ->
               let em' = emRun pts 150 10 em
               in case emResult em' of
                    Nothing -> w { wPending = Just p { pEm = Just em' } }
                    Just f ->
                      let better = case pGmm p of
                            Nothing -> f
                            Just g -> if gfLL f > gfLL g then f else g
                      in if s < 4 then next p { pGmm = Just better, pEm = Nothing }
                         else let !lab = V.convert (V.map argMax (gfResp better)) in next p { pGmm = Nothing, pEm = Nothing, pPreds = pPreds p ++ [lab] }
      5 -> let !lab = dbLabels (dbscan pts 0.25 6) in next p { pPreds = pPreds p ++ [lab] }
      s | s == 6 || s == 7 ->                                   -- 単連結・ウォード法（1 フレームに 64 回の合体ずつ）
           let ag = aggRun 64 (maybe (aggStart pts (if s == 6 then 0 else 3)) id (pAgg p))
           in if not (aggDone ag) then w { wPending = Just p { pAgg = Just ag } }
              else let !lab = cutTreeK (aggMerges ag) n 3 in next p { pPreds = pPreds p ++ [lab], pAgg = Nothing }
      _ -> let ps = pPreds p
               preds = take 5 (ps ++ repeat (U.replicate n 0)) ++ [U.empty, U.enumFromN 0 n]
           in randomPred w { wData = pData p, wPreds = preds, wPerm = Nothing, wPending = Nothing }
  where
    next p = w { wPending = Just p { pStage = pStage p + 1 } }
    withRng m = fst (runR m (maybe (seedRng 1) pRng (wPending w)))

argMax :: U.Vector Double -> Int              -- r.indexOf(Math.max(...r))
argMax r = fst (U.ifoldl' (\(bi, bv) i v -> if v > bv then (i, v) else (bi, bv)) (0, -1 / 0) r)

-- でたらめ（3 つのグループ）を作りなおして、7 つの方法の点数を計算する
randomPred :: World -> World
randomPred w = let sc = [ scoreOf p (dsTruth (wData w)) | p <- preds ] in foldl' (\a s -> a + sAri s + sNmi s + sPur s) 0 sc `seq` w { wPreds = preds, wScores = sc }
  where
    n = V.length (dsPts (wData w))
    rp = withSeed (fromIntegral (700 + wSeedR w)) (U.fromList <$> mapM (const (rndInt 0 2)) [1 .. n])
    preds = [ if m == 5 then rp else p | (m, p) <- zip [0 :: Int ..] (wPreds w) ]

scoreOf :: U.Vector Int -> U.Vector Int -> Score
scoreOf p t = Score (purityScore p t) (ariScore p t) (nmiScore p t)

-- 0〜9 だけ入れかえる（10 以上はずらすだけ）
shownPred :: World -> U.Vector Int
shownPred w = case wPerm w of
  Nothing -> p
  Just pm -> U.map (\l -> if l < 0 then l else if l < 10 then headOr l (drop l pm) else l + 100) p
  where p = headOr U.empty (drop (wMethod w) (wPreds w))

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX vw) (pY vw) (pW vw) (pH vw))
  , drawPts vw pts (Just (if wMethod w == 6 then U.map (`mod` 10) pred' else pred')) 7 255 ]
  -- 正解は形で：●（グループ）と ×（外れ値）
  ++ concat [ [ Color (rgbaC 255 255 255 120) (lineW 1 (x - 6, y - 6) (x + 6, y + 6)), Color (rgbaC 255 255 255 120) (lineW 1 (x - 6, y + 6) (x + 6, y - 6)) ]
            | (p, t) <- zip (V.toList pts) (U.toList truth), t < 0, let x = gX vw (ptX p), let y = gY vw (ptY p) ]
  -- ---- 分割表
  ++ [ say "Contingency (rows = result, cols = truth, -1 = noise/outlier)" tx (ty - 22) 14 (grayC 205) ]
  ++ [ sayM ("truth " ++ show c) (tx + 80 + fromIntegral j * cw + cw / 2) (ty + rh / 2) 14 (grayC 180) | (j, c) <- zip [0 :: Int ..] cb ]
  ++ concat [ [ sayM (if wMethod w == 6 then "..." else "result " ++ show r) (tx + 40) (y + rh / 2) 14 (if r < 0 then grayC 150 else palC r) ]
              ++ concat [ [ Color (rgbC (round (60 + 150 * u)) (round (50 + 100 * u)) (round (110 + 120 * u))) (rectTL (tx + 80 + fromIntegral j * cw + 2) (y + 2) (cw - 4) (rh - 4))
                          , sayM (show v) (tx + 80 + fromIntegral j * cw + cw / 2) (y + rh / 2) 14 (grayC 240) ]
                        | (j, v) <- zip [0 :: Int ..] row, let u = fromIntegral v / fromIntegral mxc :: Float ]
            | (i, (r, row)) <- zip [0 :: Int ..] (take rows (zip ra mt)), let y = ty + fromIntegral (i + 1) * rh ]
  ++ [ say ("... and " ++ show (length ra - rows) ++ " more rows (single-point clusters)") (tx + 80) (ty + (fromIntegral rows + 1.5) * rh + 5) 14 (grayC 150) | length ra > rows ]
  -- ---- いまの方法の数
  ++ [ say ("purity " ++ fmt 3 (sPur sc) ++ "    ARI " ++ fmt 3 (sAri sc) ++ "    NMI " ++ fmt 3 (sNmi sc)) tx sy 17 (grayC 230) ]
  ++ [ sayWrap ("Labels permuted: 0->" ++ show (pmAt 0) ++ ", 1->" ++ show (pmAt 1) ++ ", 2->" ++ show (pmAt 2) ++ ". Only the rows of the table swap; the three numbers stay the same.")
               tx (sy + 8) (envW env - tx - 40) 14 (rgbC 255 210 130) | Just pm <- [wPerm w], let pmAt i = headOr i (drop i pm) ]
  -- ---- 全部の方法の表
  ++ [ say t x by 14 (grayC 205) | (t, x) <- [("method", 40), ("purity", 200), ("ARI", 270), ("NMI", 340)] ]
  ++ concat [ [ say (show (m + 1) ++ " " ++ nm) 40 y 14 col, say (fmt 3 (sPur s)) 200 y 14 col, say (fmt 3 (sAri s)) 270 y 14 col, say (fmt 3 (sNmi s)) 340 y 14 col
              , Color (if m == wMethod w then rgbC 176 140 255 else rgbC 90 95 120) (rectTL 410 (y - 12) (max 0 (f2 (sAri s)) * 200) 12) ]
            | (m, (nm, s)) <- zip [0 :: Int ..] (zip methods (wScores w)), let y = by + 24 + fromIntegral m * 22
            , let col = if m == wMethod w then rgbC 200 180 255 else grayC 190 ]
  -- ---- 文字
  ++ [ say ("Measuring agreement with the truth   " ++ show (wMethod w + 1) ++ ". " ++ headOr "" (drop (wMethod w) methods) ++ "   ARI " ++ fmt 3 (sAri sc)) 40 40 20 (grayC 240)
     , say ("1-7 method   L permute labels" ++ (if wPerm w /= Nothing then " (permuted)" else "") ++ "   Space new random labels   R new data" ++ (if busy then "   (computing ...)" else "")) 40 66 15 (if busy then rgbC 255 210 130 else grayC 170)
     , sayWrap "x = outlier in the truth. 7 (1 point each) has purity 1 but ARI ~ 0, so never compare by purity alone. 6 (random) has ARI ~ 0." tx (envH env - 76) (envW env - tx - 40) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    vw = fitView 40 100 (envW env * 0.38) (envH env * 0.5) 0 0 4.0
    pts = dsPts (wData w)
    truth = dsTruth (wData w)
    pred' = shownPred w
    (mt, ra, cb) = contingency pred' truth
    sc = scoreOf pred' truth
    busy = maybe False (const True) (wPending w)
    tx = pX vw + pW vw + 60
    ty = 130
    rows = min (length ra) 12
    cw = 70
    rh = 26
    mxc = foldl' max 1 (concat mt)
    sy = ty + (fromIntegral rows + 2.5) * rh + 10
    by = pY vw + pH vw + 40

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "1234567" -> w { wMethod = fromEnum c - fromEnum '1', wPerm = Nothing }
    | c `elem` "lL" -> w { wPerm = Just (withSeed (fromIntegral (300 + envFrame env)) (shuffle10 [0 .. 9])) }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> randomPred w { wSeedR = wSeedR w + 1 }
  _ -> w

-- フィッシャー-イェーツ（i = 9, 8, …, 1 で j = 0〜i をえらんで入れかえ）
shuffle10 :: [Int] -> R [Int]
shuffle10 xs0 = go (length xs0 - 1) xs0
  where
    go i xs | i <= 0 = pure xs
            | otherwise = do
                j <- rndInt 0 i
                let xi = xs !! i
                    xj = xs !! j
                go (i - 1) [ if k == i then xj else if k == j then xi else x | (k, x) <- zip [0 ..] xs ]

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
-- 答え合わせ用：全部の 2 点の組を数える（O(n²)）
ariPairs :: U.Vector Int -> U.Vector Int -> Double
ariPairs a b = if den == 0 then 1 else 2 * (n00 * n11 - n01 * n10) / den
  where
    n = U.length a
    (n11, n10, n01, n00) = foldl' cnt (0, 0, 0, 0) [ (a U.! i == a U.! j, b U.! i == b U.! j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
    cnt (!p, !q, !r, !s) (sa, sb) | sa && sb = (p + 1, q, r, s)
                                  | sa = (p, q + 1, r, s)
                                  | sb = (p, q, r + 1, s)
                                  | otherwise = (p, q, r, s + 1)
    den = (n00 + n01) * (n01 + n11) + (n00 + n10) * (n10 + n11)

-- JavaScript の arr.sort(() => rnd() − 0.5) と同じ順に乱数を引く（V8 は短い配列を「二分挿入ソート」で並べる）
v8RandomSort :: [a] -> R [a]
v8RandomSort xs0
  | n < 2 = pure xs0
  | otherwise = do
      o0 <- cmp
      let desc = o0 < 0
          run k | k >= n = pure k
                | otherwise = do
                    o <- cmp
                    if (desc && o >= 0) || (not desc && o < 0) then pure k else run (k + 1)
      len <- run 2
      let start = if desc then reverse (take len xs0) ++ drop len xs0 else xs0
      ins len start
  where
    n = length xs0
    cmp = (\u -> u - 0.5) <$> rnd
    ins s arr
      | s >= n = pure arr
      | otherwise = do
          let pivot = arr !! s
              search l r | l >= r = pure l
                         | otherwise = do
                             let mid = l + (r - l) `div` 2
                             o <- cmp
                             if o < 0 then search l mid else search (mid + 1) r
          left <- search 0 s
          let rest = take s arr
          ins (s + 1) (take left rest ++ [pivot] ++ drop left rest ++ drop (s + 1) arr)

selfTest :: [(String, Bool)]
selfTest =
  [ ("same partition with any relabelling: ARI = NMI = purity = 1", relOk)
  , ("mean ARI of random vs random ~ 0 (" ++ fmt 4 (meanOf ar) ++ ", 40 times)", abs (meanOf ar) < 0.01)
  , ("ARI from the contingency table = ARI by counting all pairs (20 cases)", pairOk)
  , ("ARI and NMI are symmetric (swap result and truth), NMI and purity lie in 0..1", symOk && rangeOk)
  , ("one point each: purity = " ++ fmt 2 (purityScore single (dsTruth d)) ++ " but ARI = " ++ fmt 3 (ariScore single (dsTruth d)) ++ " (~ 0)", purityScore single (dsTruth d) == 1 && abs (ariScore single (dsTruth d)) < 0.01)
  , ("table total = n, row sums = cluster sizes", tot == U.length a && and [ sum r == U.length (U.filter (== c) a) | (r, c) <- zip ctM ctRa ])
  ]
  where
    ((a, relOk, ar, pairOk, symOk, rangeOk), _) = flip runR (seedRng 11) $ do
      a' <- U.fromList <$> mapM (const (rndInt 0 4)) [1 .. 150 :: Int]
      rels <- mapM (\_ -> do
        p <- map (\v -> v * 7 + 3) <$> v8RandomSort [0 .. 4 :: Int]
        let b = U.map (\l -> p !! l) a'
        pure (near (ariScore a' b) 1 1e-12 && near (nmiScore a' b) 1 1e-12 && near (purityScore b a') 1 1e-12)) [1 .. 10 :: Int]
      ars <- mapM (\_ -> do
        x <- U.fromList <$> mapM (const (rndInt 0 3)) [1 .. 400 :: Int]
        y <- U.fromList <$> mapM (const (rndInt 0 3)) [1 .. 400 :: Int]
        pure (ariScore x y)) [1 .. 40 :: Int]
      trials <- mapM (\_ -> do
        x <- U.fromList <$> mapM (const (rndInt (-1) 3)) [1 .. 80 :: Int]
        y <- U.fromList <$> mapM (\v -> do { u <- rnd; if u < 0.6 then pure v else rndInt 0 4 }) (U.toList x)
        let nm = nmiScore x y
            pu = purityScore x y
        pure ( near (ariScore x y) (ariPairs x y) 1e-12
             , near (ariScore x y) (ariScore y x) 1e-12 && near (nmiScore x y) (nmiScore y x) 1e-12
             , nm >= 0 && nm <= 1 && pu > 0 && pu <= 1 )) [1 .. 20 :: Int]
      pure (a', and rels, ars, all (\(p, _, _) -> p) trials, all (\(_, s, _) -> s) trials, all (\(_, _, r) -> r) trials)
    d = withSeed 1 (makeDataset "noise" 200 12)
    single = U.enumFromN 0 (V.length (dsPts d))
    (ctM, ctRa, _) = contingency a (U.map (\v -> (v + 1) `mod` 3) a)
    tot = sum (map sum ctM)

-- ---------- やってみよう ----------
-- 1. 1〜5 を切りかえて、ARI がいちばん高い方法はどれ？ 外れ値（×）をうまくあつかえるのは？
-- 2. L で番号を入れかえても、分割表の行が入れかわるだけで 3 つの数は変わらないことを確かめよう
-- 3. 6（でたらめ）でスペースを何回も押そう。ARI は 0 のまわりでゆれる。NMI は 0 より少し大きくなりがち（偏りがある）
-- 4. 7（1 点ずつ）は純度 1。なぜ？ ARI と NMI はどうなっている？
-- 5. 正解がないとき（ふつうはこっち）は、02 のシルエットのような「内部の指標」を使う

-- ---- 凝集型クラスタリングを何フレームかに分けて計算する版（中身は「クラスタリングの道具」の agglomerate と同じ）
data Agg = Agg { agN :: !Int, agMethod :: !Int, agT :: !Int, agD :: !(U.Vector Double), agSize :: !(U.Vector Int)
               , agIds :: !(U.Vector Int), agAct :: !(U.Vector Bool), agMerges :: ![Merge] }   -- agMerges は新しいものが先頭

aggStart :: Pts -> Int -> Agg
aggStart pts method = Agg n method 0 dm (U.replicate n 1) (U.enumFromN 0 n) (U.replicate n True) []
  where
    n = V.length pts
    dm = U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in if method == 3 then sqd (pts V.! i) (pts V.! j) else edist (pts V.! i) (pts V.! j))

aggDone :: Agg -> Bool
aggDone a = agT a >= agN a - 1

aggMerges :: Agg -> [Merge]
aggMerges = reverse . agMerges

-- 合体を k 回すすめる
aggRun :: Int -> Agg -> Agg
aggRun k0 a0
  | aggDone a0 = a0
  | otherwise = runST $ do
      dm <- U.thaw (agD a0)
      size <- U.thaw (agSize a0)
      ids <- U.thaw (agIds a0)
      act <- U.thaw (agAct a0)
      let n = agN a0
          method = agMethod a0
          findBest !i !bi !bj !bd
            | i >= n = pure (bi, bj, bd)
            | otherwise = do
                ai <- UM.unsafeRead act i
                if ai then inner i (i + 1) bi bj bd else findBest (i + 1) bi bj bd
          inner !i !j !bi !bj !bd
            | j >= n = findBest (i + 1) bi bj bd
            | otherwise = do
                aj <- UM.unsafeRead act j
                if not aj then inner i (j + 1) bi bj bd else do
                  d <- UM.unsafeRead dm (i * n + j)
                  if d < bd then inner i (j + 1) i j d else inner i (j + 1) bi bj bd
          loop !k !t ms
            | k <= 0 || t >= n - 1 = pure (t, ms)
            | otherwise = do
                (bi0, bj0, bd) <- findBest 0 (-1) (-1) (1 / 0)
                let (bi, bj) = if bi0 >= 0 then (bi0, bj0) else (0, 1)
                ni <- UM.unsafeRead size bi
                nj <- UM.unsafeRead size bj
                ia <- UM.unsafeRead ids bi
                ib <- UM.unsafeRead ids bj
                forM_ [0 .. n - 1] $ \kk -> do
                  ak <- UM.unsafeRead act kk
                  when (ak && kk /= bi && kk /= bj) $ do
                    dik <- UM.unsafeRead dm (bi * n + kk)
                    djk <- UM.unsafeRead dm (bj * n + kk)
                    nk <- UM.unsafeRead size kk
                    let fi = fromIntegral ni; fj = fromIntegral nj; fk = fromIntegral nk
                        v = case method of
                          0 -> min dik djk
                          1 -> max dik djk
                          2 -> (fi * dik + fj * djk) / (fi + fj)
                          _ -> ((fi + fk) * dik + (fj + fk) * djk - fk * bd) / (fi + fj + fk)
                    UM.unsafeWrite dm (bi * n + kk) v
                    UM.unsafeWrite dm (kk * n + bi) v
                UM.unsafeWrite size bi (ni + nj)
                UM.unsafeWrite act bj False
                UM.unsafeWrite ids bi (n + t)
                loop (k - 1) (t + 1) (Merge ia ib (if method == 3 then sqrt (max 0 bd) else bd) (ni + nj) : ms)
      (t', ms') <- loop k0 (agT a0) (agMerges a0)
      d' <- U.freeze dm
      s' <- U.freeze size
      i' <- U.freeze ids
      a' <- U.freeze act
      pure a0 { agT = t', agD = d', agSize = s', agIds = i', agAct = a', agMerges = ms' }

-- ---- 混合ガウス分布の EM を何フレームかに分けて計算する版（中身は「クラスタリングの道具」の gmmFit と同じ）
data Em = Em { emT :: !Int, emGm :: !GMM, emResp :: !(V.Vector (U.Vector Double)), emLL :: !Double, emHist :: ![Double], emFit :: !(Maybe GFit) }

emStart :: Pts -> GMM -> Em
emStart pts gm0 = let (resp, ll) = gmmE pts gm0 in Em 0 gm0 resp ll [] Nothing

emResult :: Em -> Maybe GFit
emResult = emFit

-- EM を k 回すすめる（全部で iters 回まで。対数尤度がほとんど変わらなくなったら終わり）
emRun :: Pts -> Int -> Int -> Em -> Em
emRun pts iters k em
  | maybe False (const True) (emFit em) = em
  | emT em >= iters = em { emFit = Just (GFit (emGm em) (emLL em) (emResp em) (reverse (emLL em : emHist em))) }
  | k <= 0 = em
  | otherwise =
      let gm' = gmmM pts (emResp em) (Just (emGm em))
          (resp', ll') = gmmE pts gm'
          t = emT em
      in if t > 3 && abs (ll' - emLL em) < 1e-9 * abs ll'
           then em { emFit = Just (GFit gm' ll' resp' (reverse (ll' : emLL em : emHist em))) }
           else ll' `seq` emRun pts iters (k - 1) (Em (t + 1) gm' resp' ll' (emLL em : emHist em) Nothing)

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

-- ---- 合成データ（種つき）。どれも DS { 点, 正解のグループ番号, 正解のグループ数 } を返す
-- blob = 点の数、中心 (x, y)、(長い向きの標準偏差, 短い向き)、傾き（ラジアン）
data Blob = Blob { bN :: !Int, bC :: !(Double, Double), bSd :: !(Double, Double), bRot :: !Double }
data DS = DS { dsPts :: !Pts, dsTruth :: !(U.Vector Int), dsK :: !Int }

makeBlobs :: [Blob] -> R DS
makeBlobs specs = do
  groups <- forM (zip [0 ..] specs) $ \(k, s) -> do
    let cr = cos (bRot s)
        sr = sin (bRot s)
        (cx, cy) = bC s
        (s0, s1) = bSd s
    replicateM (bN s) $ do
      a <- (* s0) <$> gauss
      b <- (* s1) <$> gauss
      pure (pt2 (cx + cr * a - sr * b) (cy + sr * a + cr * b), k)
  let ps = concat groups
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) (length specs))

makeMoons :: Int -> Double -> R DS          -- 2 つの三日月（かみ合った半円）
makeMoons n noise = do
  ps <- forM [0 .. n - 1] $ \i -> do
    t <- (* pi) <$> rnd
    let k = i `mod` 2
        x = if k == 1 then 1 - cos t else cos t
        y = if k == 1 then 0.5 - sin t else sin t
    gx <- gauss
    gy <- gauss
    pure (pt2 ((x - 0.5) * 1.6 + gx * noise) ((y - 0.25) * 1.6 + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

makeCircles :: Int -> Double -> Double -> R DS   -- 同心円（外の輪と内の輪。点の数は輪の長さに比例）
makeCircles n noise inner = do
  let nOut = jsRoundI (fromIntegral n * 1.3 / (1.3 + inner))
  ps <- forM [0 .. n - 1] $ \i -> do
    let k = if i < nOut then 0 else 1
        r = if k == 1 then inner else 1.3
    t <- (* (2 * pi)) <$> rnd
    gx <- gauss
    gy <- gauss
    pure (pt2 (r * cos t + gx * noise) (r * sin t + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

dataName :: String -> String                -- 画面に出すデータの名前
dataName kind = case kind of
  "blobs" -> "blobs"
  "moons" -> "moons"
  "circles" -> "circles"
  "aniso" -> "elongated"
  "density" -> "different densities"
  "sizes" -> "different sizes"
  "noise" -> "blobs + outliers"
  "chain" -> "chained"
  _ -> kind

appendDS :: DS -> [(P, Int)] -> DS          -- 点を後ろに足す
appendDS d extra = d { dsPts = dsPts d V.++ V.fromList (map fst extra), dsTruth = dsTruth d U.++ U.fromList (map snd extra) }

makeDataset :: String -> Int -> Word32 -> R DS
makeDataset kind n seed = do
  seedR seed
  case kind of
    "moons" -> makeMoons n 0.07
    "circles" -> makeCircles n 0.05 0.55
    "aniso" -> let m = n `div` 3 in makeBlobs [Blob m (-0.6, 0.9) (0.9, 0.1) 0.5, Blob m (0, 0) (0.9, 0.1) 0.5, Blob (n - 2 * m) (0.6, -0.9) (0.9, 0.1) 0.5]
    "density" -> let m = floor (fromIntegral n * 0.45 :: Double) in makeBlobs [Blob m (-0.9, 0.3) (0.12, 0.12) 0, Blob m (-0.3, 0.3) (0.12, 0.12) 0, Blob (n - 2 * m) (0.9, -0.2) (0.65, 0.65) 0]
    "sizes" -> let m = floor (fromIntegral n * 0.12 :: Double) in makeBlobs [Blob (n - 2 * m) (-0.5, 0) (0.6, 0.6) 0, Blob m (0.85, 0.75) (0.1, 0.1) 0, Blob m (0.85, -0.75) (0.1, 0.1) 0]
    "chain" -> do
      let m = floor (fromIntegral n * 0.4 :: Double)
          rest = n - 2 * m
      d <- makeBlobs [Blob m (-1.2, 0) (0.25, 0.25) 0, Blob m (1.2, 0) (0.25, 0.25) 0]
      extra <- forM [0 .. rest - 1] $ \i -> do
        g <- gauss
        pure (pt2 (-0.8 + 1.6 * (fromIntegral i + 0.5) / fromIntegral rest) (0.04 * g), if fromIntegral i < (fromIntegral rest / 2 :: Double) then 0 else 1)
      pure (appendDS d extra)
    _ -> do
      let m = n `div` 3
      d <- makeBlobs [Blob m (-1, 0.7) (0.25, 0.25) 0, Blob m (1, 0.6) (0.3, 0.22) 0.4, Blob (n - 2 * m) (0, -0.9) (0.28, 0.28) 0]
      if kind == "noise"
        then do
          let q = floor (fromIntegral n * 0.08 :: Double)
          extra <- replicateM q $ do
            x <- rnd
            y <- rnd
            pure (pt2 (-2 + 4 * x) (-1.8 + 3.6 * y), -1)
          pure (appendDS d extra)
        else pure d

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

-- ---- 凝集型クラスタリング（ランス-ウィリアムズの更新式）。Merge { 合体した 2 つの番号, 高さ, 大きさ }
--   葉 = 0〜n−1、t 番目の合体でできたクラスタ = n + t。ウォード法は距離² で更新し、高さ = √（更新した値）
data Merge = Merge { mA :: !Int, mB :: !Int, mH :: !Double, mSize :: !Int }

linkName :: Int -> String
linkName m = headOr "" (drop m ["single", "complete", "average", "Ward"])

agglomerate :: Pts -> Int -> [Merge]        -- method: 0 単連結 / 1 完全連結 / 2 群平均 / 3 ウォード法
agglomerate pts method
  | n < 2 = []
  | otherwise = runST $ do
      dm <- U.thaw (U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in if method == 3 then sqd (pts V.! i) (pts V.! j) else edist (pts V.! i) (pts V.! j)))
      size <- UM.replicate n (1 :: Int)
      ids <- U.thaw (U.enumFromN 0 n)
      act <- UM.replicate n True
      let -- いちばん近い 2 つ（i < j）を探す
          findBest !i !bi !bj !bd
            | i >= n = pure (bi, bj, bd)
            | otherwise = do
                ai <- UM.unsafeRead act i
                if ai then inner i (i + 1) bi bj bd else findBest (i + 1) bi bj bd
          inner !i !j !bi !bj !bd
            | j >= n = findBest (i + 1) bi bj bd
            | otherwise = do
                aj <- UM.unsafeRead act j
                if not aj then inner i (j + 1) bi bj bd else do
                  d <- UM.unsafeRead dm (i * n + j)
                  if d < bd then inner i (j + 1) i j d else inner i (j + 1) bi bj bd
      forM [0 .. n - 2] $ \t -> do
        (bi0, bj0, bd) <- findBest 0 (-1) (-1) (1 / 0)
        (bi, bj) <- if bi0 >= 0 then pure (bi0, bj0) else do      -- 距離が全部 ∞ のとき（ふつうは起きない）
          alive <- filterM' (UM.unsafeRead act) [0 .. n - 1]
          pure (case alive of { (x : y : _) -> (x, y); _ -> (0, 1) })
        ni <- UM.unsafeRead size bi
        nj <- UM.unsafeRead size bj
        ia <- UM.unsafeRead ids bi
        ib <- UM.unsafeRead ids bj
        forM_ [0 .. n - 1] $ \k -> do
          ak <- UM.unsafeRead act k
          when (ak && k /= bi && k /= bj) $ do
            dik <- UM.unsafeRead dm (bi * n + k)
            djk <- UM.unsafeRead dm (bj * n + k)
            nk <- UM.unsafeRead size k
            let fi = fromIntegral ni; fj = fromIntegral nj; fk = fromIntegral nk
                v = case method of
                  0 -> min dik djk
                  1 -> max dik djk
                  2 -> (fi * dik + fj * djk) / (fi + fj)
                  _ -> ((fi + fk) * dik + (fj + fk) * djk - fk * bd) / (fi + fj + fk)
            UM.unsafeWrite dm (bi * n + k) v
            UM.unsafeWrite dm (k * n + bi) v
        UM.unsafeWrite size bi (ni + nj)
        UM.unsafeWrite act bj False
        UM.unsafeWrite ids bi (n + t)
        pure (Merge ia ib (if method == 3 then sqrt (max 0 bd) else bd) (ni + nj))
  where
    n = V.length pts
    filterM' f = foldr (\x acc -> do { ok <- f x; xs <- acc; pure (if ok then x : xs else xs) }) (pure [])

membersOf :: [Merge] -> Int -> V.Vector [Int]   -- 番号 → 中の点のリスト
membersOf merges n = mem
  where mem = V.fromList (map (: []) [0 .. n - 1] ++ [ (mem V.! mA m) ++ (mem V.! mB m) | m <- merges ])

-- はじめの count 回だけ合体したときのラベル（0, 1, 2… 小さい点の番号順）
labelsAfter :: [Merge] -> Int -> Int -> U.Vector Int
labelsAfter merges n count = U.replicate n 0 U.// [ (i, k) | (k, g) <- zip [0 ..] groups, i <- g ]
  where
    mem = membersOf merges n
    alive = foldl' (\s (t, m) -> IS.insert (n + t) (IS.delete (mB m) (IS.delete (mA m) s))) (IS.fromList [0 .. n - 1]) (zip [0 ..] (take count merges))
    groups = map snd (sortBy (\x y -> compare (fst x) (fst y)) [ (foldl' min maxBound g, g) | c <- IS.toList alive, let g = mem V.! c ])

cutTreeK :: [Merge] -> Int -> Int -> U.Vector Int   -- k 個のクラスタになるように切る
cutTreeK merges n k = labelsAfter merges n (max 0 (min (n - 1) (n - k)))

cutTreeH :: [Merge] -> Int -> Double -> U.Vector Int   -- 高さ h で切る
cutTreeH merges n h = labelsAfter merges n (length (takeWhile (\m -> mH m <= h) merges))

leafOrder :: [Merge] -> Int -> [Int]        -- デンドログラムの葉の並び（枝が交差しない順）
leafOrder merges n
  | n <= 1 = [0 .. n - 1]
  | otherwise = go [2 * n - 2]
  where
    mv = V.fromList merges
    go [] = []
    go (v : st)
      | v < n = v : go st
      | v - n < V.length mv = let m = mv V.! (v - n) in go (mA m : mB m : st)
      | otherwise = go st

copheneticMatrix :: [Merge] -> Int -> U.Vector Double   -- c_ij = i と j が初めて同じクラスタになった高さ（n × n）
copheneticMatrix merges n = runST $ do
  c <- UM.replicate (n * n) 0
  forM_ merges $ \m -> forM_ (mem V.! mA m) $ \x -> forM_ (mem V.! mB m) $ \y -> do
    UM.write c (x * n + y) (mH m)
    UM.write c (y * n + x) (mH m)
  U.freeze c
  where mem = membersOf merges n

pearson :: U.Vector Double -> U.Vector Double -> Double
pearson xs ys = if sxx > 0 && syy > 0 then clampv (-1) 1 (sxy / sqrt (sxx * syy)) else 0
  where
    n = U.length xs
    mx = if n == 0 then 0 else U.foldl' (+) 0 xs / fromIntegral n
    my = if n == 0 then 0 else U.foldl' (+) 0 ys / fromIntegral n
    (sxy, sxx, syy) = U.foldl' (\(a1, a2, a3) (x, y) -> let a = x - mx; b = y - my in (a1 + a * b, a2 + a * a, a3 + b * b)) (0, 0, 0) (U.zip xs ys)

copheneticCorr :: Pts -> [Merge] -> Double  -- もとの距離とコーフェン距離の相関
copheneticCorr pts merges = pearson (U.fromList (map fst prs)) (U.fromList (map snd prs))
  where
    n = V.length pts
    c = copheneticMatrix merges n
    prs = [ (edist (pts V.! i) (pts V.! j), c U.! (i * n + j)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]

-- ---- DBSCAN：半径 eps の中に minPts 個以上（自分も数える）ある点 = コア点。コア点どうしが eps でつながって 1 つのクラスタ
--   ラベル −1 = ノイズ。近くの点は「eps の大きさのマス目」で探す（周り 3 × 3 マスだけ見ればよい）
-- 結果：ラベル、コア点か、広がっていく順（アニメーション用）、クラスタの数、各点の近くの点
data DB = DB { dbLabels :: !(U.Vector Int), dbCore :: !(U.Vector Bool), dbOrder :: ![Int], dbN :: !Int, dbNb :: !(V.Vector [Int]) }

gridKey :: Int -> Int -> Int
gridKey cx cy = cx * 100003 + cy

cellOf :: Double -> Double -> Int
cellOf v eps = floorI (v / eps)

buildGrid :: Pts -> Double -> IM.IntMap [Int]
buildGrid pts eps = IM.map reverse (IM.fromListWith (++) [ (gridKey (cellOf (ptX p) eps) (cellOf (ptY p) eps), [i]) | (i, p) <- zip [0 ..] (V.toList pts) ])

neighborsGrid :: IM.IntMap [Int] -> Pts -> Int -> Double -> [Int]
neighborsGrid g pts i eps = sort [ j | dx <- [-1, 0, 1], dy <- [-1, 0, 1], j <- IM.findWithDefault [] (gridKey (cx + dx) (cy + dy)) g, sqd p (pts V.! j) <= eps * eps ]
  where
    p = pts V.! i
    cx = cellOf (ptX p) eps
    cy = cellOf (ptY p) eps

dbscan :: Pts -> Double -> Int -> DB
dbscan pts eps minPts = runST $ do
  lab <- UM.replicate n (-2 :: Int)
  (ordR, c) <- foldM (\(ord, c) i -> do
      li <- UM.read lab i
      if li /= -2 || not (core U.! i) then pure (ord, c) else do
        UM.write lab i c
        let bfs [] o = pure o
            bfs (q : qs) o = do
              (o', new) <- foldM (\(oa, na) j -> do
                  lj <- UM.read lab j
                  if lj >= 0 then pure (oa, na) else do
                    UM.write lab j c
                    pure (j : oa, if core U.! j then j : na else na)) (o, []) (nb V.! q)
              bfs (qs ++ reverse new) o'
        o2 <- bfs [i] (i : ord)
        pure (o2, c + 1)) ([], 0) [0 .. n - 1]
  labs <- U.freeze lab
  pure (DB (U.map (\l -> if l == -2 then -1 else l) labs) core (reverse ordR) c nb)
  where
    n = V.length pts
    e = max 1e-6 eps
    g = buildGrid pts e
    nb = V.generate n (\i -> neighborsGrid g pts i e)
    core = U.generate n (\i -> length (nb V.! i) >= minPts)

-- ---- 混合ガウス分布（2 次元、共分散は全部）。共分散 = (s_xx, s_xy, s_yy)。GMM { 重み, 平均, 共分散 }
type Cov = (Double, Double, Double)
data GMM = GMM { gW :: ![Double], gMu :: ![(Double, Double)], gCv :: ![Cov] }

covFloor :: Double
covFloor = 1e-4                             -- 共分散の固有値の下限（1 点に縮んで尤度が ∞ になるのを防ぐ）

logGauss2 :: P -> (Double, Double) -> Cov -> Double
logGauss2 p (mx, my) (a, b, c) = -log (2 * pi) - 0.5 * log det - 0.5 * q
  where
    det = a * c - b * b
    dx = ptX p - mx
    dy = ptY p - my
    q = (c * dx * dx - 2 * b * dx * dy + a * dy * dy) / det

logSumExp :: [Double] -> Double
logSumExp arr = let m = foldl' max (-1 / 0) arr
                in if m == -1 / 0 then -1 / 0 else m + log (foldl' (\s v -> s + exp (v - m)) 0 arr)

-- E ステップ：所属確率 r_ik（各点で合計 1）と対数尤度
gmmE :: Pts -> GMM -> (V.Vector (U.Vector Double), Double)
gmmE pts gm = (V.map fst rl, V.foldl' (\s (_, l) -> s + l) 0 rl)
  where
    rl = V.map one pts
    one p = let lg = zipWith3 (\w mu cv -> log (max w 1e-300) + logGauss2 p mu cv) (gW gm) (gMu gm) (gCv gm)
                lse = logSumExp lg
            in (U.fromList (map (\v -> exp (v - lse)) lg), lse)

floorCov :: Cov -> Cov                      -- 固有値が covFloor より小さければ持ち上げる（ふだんは何もしない）
floorCov cv@(a, b, c)
  | l2 >= covFloor && not (isInfinite l1 || isNaN l1) = cv
  | otherwise = (m1 * co * co + m2 * si * si, (m1 - m2) * co * si, m1 * si * si + m2 * co * co)
  where
    tr = (a + c) / 2
    df = sqrt (((a - c) / 2) * ((a - c) / 2) + b * b)
    l1 = tr + df
    l2 = tr - df
    th = 0.5 * atan2 (2 * b) (a - c)
    co = cos th
    si = sin th
    finite v = not (isInfinite v || isNaN v)
    m1 = max (if finite l1 then l1 else 1) covFloor
    m2 = max (if finite l2 then l2 else covFloor) covFloor

-- M ステップ：重み・平均・共分散を所属確率の重みつきで計算しなおす
gmmM :: Pts -> V.Vector (U.Vector Double) -> Maybe GMM -> GMM
gmmM pts resp old
  | V.null resp = maybe (GMM [] [] []) id old
  | otherwise = let sw = foldl' (+) 0 (map fst3 comps) in GMM (map (\(w, _, _) -> w / sw) comps) (map snd3 comps) (map thd3 comps)
  where
    k = U.length (V.head resp)
    n = V.length pts
    comps = map comp [0 .. k - 1]
    comp j =
      let (nk, sx, sy) = foldl' (\(a0, a1, a2) i -> let r = resp V.! i U.! j; p = pts V.! i in (a0 + r, a1 + r * ptX p, a2 + r * ptY p)) (0, 0, 0) [0 .. n - 1]
          mx = sx / nk
          my = sy / nk
          (a, b, c) = foldl' (\(a0, a1, a2) i -> let r = resp V.! i U.! j; p = pts V.! i; dx = ptX p - mx; dy = ptY p - my in (a0 + r * dx * dx, a1 + r * dx * dy, a2 + r * dy * dy)) (0, 0, 0) [0 .. n - 1]
      in if nk < 1e-8
           then (1e-8, maybe (let p = pts V.! (j `mod` n) in (ptX p, ptY p)) (\o -> headOr (0, 0) (drop j (gMu o))) old, maybe (1, 0, 1) (\o -> headOr (1, 0, 1) (drop j (gCv o))) old)
           else (nk / fromIntegral n, (mx, my), floorCov (a / nk, b / nk, c / nk))
    fst3 (x, _, _) = x
    snd3 (_, x, _) = x
    thd3 (_, _, x) = x

gmmInit :: Pts -> Int -> R GMM              -- k-means++ の中心から始めて、近い点を仲間にして M ステップ 1 回
gmmInit pts k = do
  cs <- initPlusPlus pts k
  let lab = assignLabels pts cs
      resp = V.generate (V.length pts) (\i -> U.generate k (\j -> if j == lab U.! i then 1 else 0))
  pure (gmmM pts resp (Just (GMM [] [ (ptX c, ptY c) | c <- V.toList cs ] (replicate k (0.3, 0, 0.3)))))

-- EM の結果。hist = 対数尤度の記録（必ず増えるか同じ）
data GFit = GFit { gfGm :: !GMM, gfLL :: !Double, gfResp :: !(V.Vector (U.Vector Double)), gfHist :: ![Double] }

gmmFit :: Pts -> Int -> Int -> R GFit       -- EM をくり返す
gmmFit pts k iters = do
  gm0 <- gmmInit pts k
  let go t gm (resp, ll) hist
        | t >= iters = GFit gm ll resp (reverse (ll : hist))
        | otherwise =
            let gm' = gmmM pts resp (Just gm)
                e'@(_, ll') = gmmE pts gm'
            in if t > 3 && abs (ll' - ll) < 1e-9 * abs ll'
                 then GFit gm' ll' (fst e') (reverse (ll' : ll : hist))
                 else go (t + 1) gm' e' (ll : hist)
  pure (go 0 gm0 (gmmE pts gm0) [])

gmmBIC :: Double -> Int -> Int -> Double    -- 母数の数 = 平均 2k + 共分散 3k + 重み (k − 1)
gmmBIC ll k n = -2 * ll + fromIntegral (6 * k - 1) * log (fromIntegral n)

covAxes :: Cov -> (Double, Double, Double)  -- 共分散の楕円：(長い軸の標準偏差, 短い軸, 傾き)
covAxes (a, b, c) = (sqrt (max 0 (tr + df)), sqrt (max 0 (tr - df)), 0.5 * atan2 (2 * b) (a - c))
  where
    tr = (a + c) / 2
    df = sqrt (((a - c) / 2) * ((a - c) / 2) + b * b)

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
