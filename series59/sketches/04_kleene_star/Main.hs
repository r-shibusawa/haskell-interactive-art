-- ============================================
-- 59-04 推移的閉包 A* ─ A* = E ⊕ A ⊕ A² ⊕ … ⊕ A^{n−1} の (i, j) = 「i から j へ、何本の辺でもよい道」の最大重み
-- 家を建てる作業（シリーズ 30 の 05 PERT と同じデータ）。a_ij = d_i（作業 i が始まって d_i 日後に j が始められる）
--   最早開始 ES_j = max_i (A*)_ij（どこから始めても 0 日目以降）、工期 = max_j (ES_j + d_j)。A* 1 つで全部の答え
--   「I は B の開始から W 日以内に始める」という締め切りは、I → B の重み −W の辺。きつすぎると正の回路 = 無限にお得な回り道 → A* は発散（解なし）
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps と書く
-- 操作：クリック 作業を選ぶ / ↑↓ 日数 / D 締め切りの辺 ON・OFF / ← → W / R 元にもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 作業：記号、名前、日数、前の作業
data Act = Act { aId :: !Char, aName :: String, aD :: !Int, aPre :: [Char] }

acts0 :: [Act]                     -- ★ 作業（シリーズ 30 の 05 と同じ）
acts0 =
  [ Act 'A' "Foundation" 4 "", Act 'B' "Frame" 6 "A", Act 'C' "Roof" 4 "B"
  , Act 'D' "Wiring" 3 "B", Act 'E' "Plumbing" 5 "B", Act 'F' "Interior" 6 "DE"
  , Act 'G' "Exterior" 5 "C", Act 'H' "Garden" 3 "A", Act 'I' "Inspection" 2 "FGH" ]

-- A* から計算した予定（正の回路があれば sPositive = True で ES などは空）
data Sched = Sched { sPositive :: !Bool, sA :: Mat, sStar :: Mat, sES :: [Double], sT :: !Double, sLS :: [Double] }

data World = World
  { wActs :: [Act]
  , wDl   :: !Bool            -- 締め切りの辺を使うか
  , wW    :: !Int             -- ★ 締め切り W（日）
  , wSel  :: !Int
  , wRes  :: Sched
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-04 Kleene star A*" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = recompute (World acts0 False 20 4 (schedule acts0 []) [])

idxOf :: [Act] -> Char -> Int
idxOf list c = fromMaybe 0 (elemIndex c (map aId list))

-- ★ 行列 A：a_ij = d_i（i が j の前の作業）。締め切りの辺 I → B は −W
buildA :: [Act] -> Bool -> Int -> Mat
buildA list dl w = (if dl then setM (idxOf list 'I') (idxOf list 'B') (fromIntegral (negate w)) else id) a0
  where
    n = length list
    a0 = foldl' (\m (j, a) -> foldl' (\m' p -> let i = idxOf list p in setM i j (fromIntegral (aD (list !! i))) m') m (aPre a))
                (mpMat n n negInf) (zip [0 ..] list)

-- ★ A* から ES・工期・余裕を計算
schedule :: [Act] -> Mat -> Sched
schedule list a
  | positive = Sched True a st [] 0 []
  | otherwise = Sched False a st es t ls
  where
    (st, positive) = mpStar 1e-9 a
    n = length list
    ds = map (fromIntegral . aD) list
    es = [ foldl' max negInf [ ix st i j | i <- [0 .. n - 1] ] | j <- [0 .. n - 1] ]
    t = foldl' max negInf (zipWith (+) es ds)
    ls = [ t - foldl' max negInf [ mpMul (ix st i j) d | (j, d) <- zip [0 ..] ds ] | i <- [0 .. n - 1] ]

recompute :: World -> World
recompute w = w { wRes = schedule (wActs w) (buildA (wActs w) (wDl w) (wW w)) }

-- 段（前の作業の段 + 1）ごとに並べた位置
positions :: [Act] -> [(Float, Float)]
positions list = snd (foldl' place ([], []) lvs)
  where
    lvs = foldl' (\acc a -> acc ++ [ if null (aPre a) then 0 else maximum [ vixI acc (idxOf list p) + 1 | p <- aPre a ] ]) [] list
    place (cnt, ps) l = let c = length (filter (== l) cnt) + 1
                        in (cnt ++ [l], ps ++ [(70 + fromIntegral l * 100, 200 + fromIntegral (c - 1) * 90)])

-- B から I までの最長の道（締め切りなし）
longestBI :: [Act] -> Double
longestBI list = ix (fst (mpStar 1e-9 (buildA list False 0))) 1 8

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Kleene star A* and PERT" ++ (if wDl w then "   deadline: I starts within " ++ show (wW w) ++ " days of B" else "")) 40 40 20 (grayC 240)
  , say ("Click: select task   Up/Down days   D deadline arc " ++ (if wDl w then "ON" else "OFF") ++ "   <- -> W   R reset") 40 66 15 (grayC 170) ]
  -- ---- 作業のグラフ
  ++ concat [ let neg = aij < 0
                  hot = crit i && crit j && not neg
                  col = if neg then rgbC 220 150 255 else if hot then rgbC 255 170 70 else grayC 140
                  (pic, (lx, ly)) = arrowTo (if hot then 3 else 1.5) (pos !! i) (pos !! j) 26 (if neg then 60 else 0)
              in [ Color col pic, sayM (mpFmt 0 aij) lx (ly - 10) 14 (if neg then rgbC 220 150 255 else grayC 200) ]
            | i <- [0 .. n - 1], j <- [0 .. n - 1], let aij = ix a i j, not (isEps aij) ]
  ++ concat [ [ if i == wSel w then Color (rgbC 255 220 120) (ringAt x y 50 3) else Blank
              , Color (if crit i then rgbC 255 170 70 else rgbC 60 90 140) (circleAt x y 47)
              , sayM [aId ac] x (y - 7) 17 (if crit i then grayC 20 else grayC 255)
              , sayM (show (aD ac) ++ " d") x (y + 12) 14 (if crit i then grayC 20 else grayC 255) ]
            | (i, ac, (x, y)) <- zip3 [0 ..] acts pos ]
  -- ---- A* の表
  ++ [ say (if sPositive res then "A* diverges (positive circuit)" else "A* (longest path i -> j; diagonal e = 0)") mx (my - 10) 15 (grayC 220) ]
  ++ concat [ let v = if sPositive res then Nothing else Just (ix (sStar res) i j)
                  cx = mx + fromIntegral j * cs + 20
                  cy = my + fromIntegral i * rh
              in [ Color (case v of { Nothing -> rgbC 80 30 40; Just x | isEps x -> grayC 28; _ -> grayC 42 }) (rectTL cx cy cs rh)
                 , Color (grayC 60) (frameTL 1 cx cy cs rh)
                 , sayM (maybe "inf" (mpFmt 0) v) (cx + cs / 2) (cy + rh / 2) 14 (if maybe False isEps v then grayC 100 else grayC 230) ]
            | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  ++ concat [ [ sayM [aId ac] (mx + 8) (my + (fromIntegral i + 0.5) * rh) 14 (grayC 150)
              , sayM [aId ac] (mx + (fromIntegral i + 0.5) * cs + 20) (my + fromIntegral n * rh + 10) 14 (grayC 150) ]
            | (i, ac) <- zip [0 :: Int ..] acts ]
  -- ---- ガントチャート（または正の回路）
  ++ (if sPositive res then positivePart else gantt)
  ++ [ drawSelfTest env (wTest w) ]
  where
    acts = wActs w
    res = wRes w
    a = sA res
    n = length acts
    pos = positions acts
    crit i = not (sPositive res) && abs (vix (sLS res) i - vix (sES res) i) < 1e-9
    cs = 40
    rh = 24
    mx = 40
    my = 520
    gx = envW env * 0.5
    gw = envW env * 0.46
    gy = 110
    grh = 38
    positivePart =
      let cycs = [ c | c <- allCycles a, cyW c > 0 ]
          best = foldl' (\m c -> case m of { Nothing -> Just c; Just b -> if cyW c > cyW b then Just c else m }) Nothing cycs
          disp c = let ns = cyNodes c in take 1 ns ++ reverse (drop 1 ns)    -- allCycles は j → i とたどるので i → j の向きに直す
      in [ sayWrap "There is a positive circuit, so A* diverges -> this deadline cannot be met" gx (gy + 8) gw 22 (rgbC 255 120 110) ]
         ++ [ sayWrap ("Circuit " ++ concatMap (\v -> [aId (acts !! v)] ++ " -> ") (disp c) ++ [aId (acts !! headOr 0 (disp c))]
                       ++ " has weight +" ++ mpFmt 0 (cyW c) ++ " (each loop delays the starts by " ++ mpFmt 0 (cyW c) ++ " days)") gx (gy + 70) gw 17 (grayC 230)
            | Just c <- [best] ]
         ++ [ sayWrap ("Longest path B -> I (" ++ mpFmt 0 (longestBI acts) ++ " days) > W = " ++ show (wW w) ++ " days. Make W larger or tasks shorter") gx (gy + 130) gw 17 (grayC 230) ]
    gantt =
      let t = sT res
          sx v = gx + f2 (v / max t 1) * gw
          es i = vix (sES res) i
          ls i = vix (sLS res) i
          by = gy + fromIntegral n * grh
      in concat [ [ Color (grayC 40) (rectTL gx y gw (grh - 8))
                  , Color (if crit i then rgbC 255 170 70 else rgbC 110 170 255) (rectTL (sx (es i)) y (sx (es i + d) - sx (es i)) (grh - 8))
                  , if crit i then Blank else Color (rgbaC 110 170 255 60) (rectTL (sx (es i + d)) y (sx (ls i + d) - sx (es i + d)) (grh - 8))
                  , sayR ([aId ac] ++ " " ++ aName ac) (gx - 8) (y + 20) 14 (grayC 230)
                  , say ("ES " ++ mpFmt 0 (es i)) (sx (es i) + 4) (y + 20) 14 (grayC 20) ]
                | (i, ac) <- zip [0 ..] acts, let y = gy + fromIntegral i * grh, let d = fromIntegral (aD ac) ]
         ++ [ say ("Duration = " ++ mpFmt 0 t ++ " days") gx (by + 30) 20 (grayC 240)
            , say "orange = no slack (critical path), light blue = slack" (gx + 210) (by + 30) 15 (grayC 190)
            , sayWrap "ES_j = max_i (A*)_ij,  LS_i = T - max_j ((A*)_ij + d_j). Same answer as the forward / backward pass of series 30" gx (by + 40) gw 15 (grayC 190) ]
         ++ [ say ("Deadline: ES_I - ES_B = " ++ mpFmt 0 (es 8 - es 1) ++ " <= W = " ++ show (wW w) ++ " (met)") gx (by + 112) 15 (grayC 190) | wDl w ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hits = [ i | (i, (x, y)) <- zip [0 ..] (positions (wActs w)), sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int)) < 26 ]
    in w { wSel = if null hits then wSel w else last hits }
  EventKey (SpecialKey KeyUp) Down _ _ -> recompute (setD (\d -> min 15 (d + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> recompute (setD (\d -> max 1 (d - 1)))
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wW = min 40 (wW w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wW = max 0 (wW w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> recompute w { wDl = not (wDl w) }
    | c `elem` "rR" -> recompute w { wActs = acts0, wDl = False, wW = 20 }
  _ -> w
  where setD f = w { wActs = [ if i == wSel w then ac { aD = f (aD ac) } else ac | (i, ac) <- zip [0 ..] (wActs w) ] }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 4・同じ乱数の順）
-- 答え合わせ用：フロイド-ワーシャル法（max 版）
floydLongest :: Mat -> Mat
floydLongest a = foldl' stepK d0 [0 .. n - 1]
  where
    n = length a
    d0 = [ [ if i == j then max 0 v else v | (j, v) <- zip [0 :: Int ..] r ] | (i, r) <- zip [0 :: Int ..] a ]
    stepK d k = [ [ let dik = ix d i k; dkj = ix d k j
                    in if not (isEps dik) && not (isEps dkj) && dik + dkj > dij then dik + dkj else dij
                  | (j, dij) <- zip [0 ..] r ] | (i, r) <- zip [0 ..] d ]
    -- （JS は 1 つの表をその場で書きかえるが、k ごとの結果は同じ）

-- シリーズ 30 と同じ前向き計算（作業は前の作業が先に並ぶ）
cpmForward :: [Act] -> [Double]
cpmForward list = foldl' (\es a -> es ++ [ foldl' max 0 [ vix es i + fromIntegral (aD (list !! i)) | p <- aPre a, let i = idxOf list p ] ]) [] list

selfTest :: [(String, Bool)]
selfTest =
  [ ("A* = Floyd-Warshall longest paths (for the graphs of 40 with no positive circuit)", all (\(f, _, _) -> f) rs)
  , ("positive-circuit detection = brute force finds a circuit of positive weight", all (\(_, d, _) -> d) rs)
  , ("A* = E (+) A (x) A* (A* solves X = E (+) A (x) X)", all (\(_, _, x) -> x) rs)
  , ("PERT: duration 23 days, ES equals the forward pass of series 30", sT r0 == 23 && and (zipWith (==) (sES r0) (cpmForward acts0)))
  , ("deadline W = 20 can be met (ES_I - ES_B <= 20), W = 16 < 17 (longest B->I) diverges"
    , not (sPositive r1) && vix (sES r1) 8 - vix (sES r1) 1 <= 20 && sPositive r2 && longestBI acts0 == 17)
  , ("slack LS - ES >= 0, critical path A->B->E->F->I"
    , and (zipWith (\l e -> l - e >= 0) (sLS r0) (sES r0)) && all (\c -> let i = idxOf acts0 c in vix (sLS r0) i == vix (sES r0) i) "ABEFI") ]
  where
    r0 = schedule acts0 (buildA acts0 False 0)
    r1 = schedule acts0 (buildA acts0 True 20)
    r2 = schedule acts0 (buildA acts0 True 16)
    rs = withSeed 4 (mapM one [0 .. 39 :: Int])
    one t = do
      let n = 4 + t `mod` 4
      a <- foldM (\m (i, j) -> if i == j then pure m else do
                    u <- rnd
                    if u < 0.45
                      then if t < 20 then (if i < j then do v <- rndInt 1 9; pure (setM i j (fromIntegral v) m) else pure m)
                           else do v <- rndInt (-9) 4; pure (setM i j (fromIntegral v) m)
                      else pure m)
                 (mpMat n n negInf) [ (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
      let (st, pos) = mpStar 1e-9 a
          hasPos = bruteMaxMean a > 0
          okD = pos == hasPos
          okF = pos || mpVecEq 1e-9 (concat st) (concat (floydLongest a))
          okX = pos || mpVecEq 1e-9 (concat st) (concat (mpMatAdd (mpEye n) (mpMatMul a st)))
      pure (okF, okD, okX)

-- ---------- やってみよう ----------
-- 1. E（水道配管）を選んで ↓ で短くしていくと、クリティカルパスはどこで入れかわる？
-- 2. D で締め切りの辺を ON にし、← で W を小さくしていこう。何日で「正の回路」になる？ その回路は？
-- 3. A* の表の 1 行目（A の行）が、各作業の ES とどう関係しているか確かめよう
-- 4. 作業 J「引っ越し」（前の作業 I、1 日）を acts0 に足してみよう
-- 5. 03 の A^{⊗k} と A* のちがいは？（「ちょうど k 本」と「何本でも」）

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- マックスプラス行列（リストのリスト）。(A ⊗ B)_ij = max_k (a_ik + b_kj)
type Mat = [[Double]]
type Vec = [Double]

-- (i, j) の成分（範囲の外は ε）
ix :: Mat -> Int -> Int -> Double
ix a i j = vix (rowOf a i) j

rowOf :: Mat -> Int -> Vec
rowOf a i = case drop i a of
  (r : _) | i >= 0 -> r
  _ -> []

vix :: Vec -> Int -> Double
vix v j = case drop j v of
  (x : _) | j >= 0 -> x
  _ -> negInf

setM :: Int -> Int -> Double -> Mat -> Mat
setM i j x = zipWith (\r row -> if r == i then zipWith (\c v -> if c == j then x else v) [0 ..] row else row) [0 ..]

mpMat :: Int -> Int -> Double -> Mat
mpMat n m v = replicate n (replicate m v)

mpEye :: Int -> Mat                           -- 単位行列 E（対角 e = 0、他は ε）
mpEye n = [ [ if i == j then 0 else negInf | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

transposeM :: Mat -> Mat
transposeM a = case a of
  [] -> []
  (r : _) -> [ map (`vix` j) a | j <- [0 .. length r - 1] ]

mpMatMul :: Mat -> Mat -> Mat
mpMatMul a b = let bt = transposeM b
               in [ [ foldl' mpAdd negInf (zipWith mpMul r c) | c <- bt ] | r <- a ]

mpMatAdd :: Mat -> Mat -> Mat
mpMatAdd = zipWith (zipWith mpAdd)

mpMatVec :: Mat -> Vec -> Vec
mpMatVec a x = [ foldl' mpAdd negInf (zipWith mpMul r x) | r <- a ]

mpPow :: Mat -> Int -> Mat
mpPow a k = foldl' (\p _ -> mpMatMul p a) (mpEye (length a)) [1 .. k]

-- A* = E ⊕ A ⊕ A² ⊕ … ⊕ A^(n−1)。正の回路があると A* は発散する → 2 つ目が True（tol は計算誤差のゆとり）
mpStar :: Double -> Mat -> (Mat, Bool)
mpStar tol a = (st, positive)
  where
    n = length a
    pows = take (max 1 (n - 1)) (iterate (`mpMatMul` a) a)      -- A, A², …, A^(n−1)（n = 1 なら A だけ）
    st = foldl' mpMatAdd (mpEye n) pows
    plus = foldl' mpMatAdd a (take (max 0 (n - 1)) (drop 1 (iterate (`mpMatMul` a) a)))   -- A ⊕ … ⊕ A^n
    positive = or [ ix plus i i > tol | i <- [0 .. n - 1] ]

-- 残余（いちばん大きい劣解）：A ⊗ x ≤ b を満たす最大の x。x_j = min_i (b_i − a_ij)（a_ij = ε の行は制約なし）
mpResid :: Mat -> Vec -> Vec
mpResid a b = [ foldl' min posInf [ if isEps bi then negInf else bi - aij
                                  | (r, bi) <- zip a b, let aij = vix r j, not (isEps aij) ]
              | j <- [0 .. m - 1] ]
  where m = case a of { (r : _) -> length r; [] -> 0 }

mpVecLeq :: Double -> Vec -> Vec -> Bool
mpVecLeq tol u v = and (zipWith (\a b -> isEps a || a <= b + tol) u v) && length u <= length v

mpVecEq :: Double -> Vec -> Vec -> Bool
mpVecEq tol u v = length u <= length v && and (zipWith (\a b -> (isEps a && isEps b) || (not (isEps a) && not (isEps b) && abs (a - b) <= tol)) u v)
-- ---- 固有値・臨界グラフ・周期（約束：a_ij = 「j が起きてから i が起きるまで」= 辺 j → i の重み、x(k+1) = A ⊗ x(k)）
-- カープのアルゴリズム：D_k(v) = ちょうど k 本の辺で v に着く最大重み。λ = max_v min_k (D_n(v) − D_k(v)) / (n − k)
karp :: Mat -> Double
karp a = foldl' max negInf
  [ m | v <- [0 .. n - 1], let dn = vix (vix' n) v, not (isEps dn)
      , let ms = [ (dn - dk) / fromIntegral (n - k) | k <- [0 .. n - 1], let dk = vix (vix' k) v, not (isEps dk) ]
      , not (null ms), let m = minimum ms ]
  where
    n = length a
    ds = take (n + 1) (iterate (mpMatVec a) (replicate n 0))
    vix' k = case drop k ds of { (d : _) -> d; [] -> [] }

-- 単純な回路（ノードは辺の向きの順、重み、平均）
data Cycle = Cycle { cyNodes :: [Int], cyW :: !Double, cyMean :: !Double }

-- 全部の単純な回路（小さいグラフ用の答え合わせ）
allCycles :: Mat -> [Cycle]
allCycles a = concat [ dfs s s [s] 0 | s <- [0 .. n - 1] ]
  where
    n = length a
    dfs s v path w = concat
      [ if u == s then [Cycle path (w + auv) ((w + auv) / fromIntegral (length path))]
        else if u `elem` path then [] else dfs s u (path ++ [u]) (w + auv)
      | u <- [s .. n - 1], let auv = ix a u v, not (isEps auv) ]

bruteMaxMean :: Mat -> Double
bruteMaxMean a = foldl' max negInf (map cyMean (allCycles a))

-- どのノードからどのノードへも行ける（強連結）
isIrreducible :: Mat -> Bool
isIrreducible a = n > 0 && reach True && reach False
  where
    n = length a
    reach fwd = go [0] [0]
      where go seen [] = length seen == n
            go seen (v : st) = let new = [ u | u <- [0 .. n - 1], not (isEps (if fwd then ix a u v else ix a v u)), u `notElem` seen ]
                               in go (seen ++ new) (reverse new ++ st)

mpShift :: Mat -> Double -> Mat                -- A − λ
mpShift a lam = map (map (\v -> if isEps v then negInf else v - lam)) a

-- 臨界グラフ：平均 λ の回路にのる辺とノード。固有ベクトル v = (A − λ)* の臨界ノード c の列
data Crit = Crit { ciNodes :: [Int], ciArcs :: [(Int, Int)], ciVec :: Maybe Vec, ciStar :: Mat }

criticalInfo :: Mat -> Double -> Crit
criticalInfo a lam = Crit nodes arcs vec bs
  where
    n = length a
    b = mpShift a lam
    bs = fst (mpStar 1e-7 b)
    arcs = [ (j, i) | i <- [0 .. n - 1], j <- [0 .. n - 1]
           , let bij = ix b i j, let bji = ix bs j i
           , not (isEps bij), not (isEps bji), abs (bij + bji) < 1e-7 ]
    nodes = sort (nub (concat [ [j, i] | (j, i) <- arcs ]))
    vec = case nodes of
      [] -> Nothing
      (c : _) -> let col = map (`vix` c) bs
                     mx = foldl' max negInf col
                 in Just (map (\v -> if isEps v then negInf else v - mx) col)

-- 巡回性：臨界グラフの強連結成分ごとに回路の長さの最大公約数、全体はその最小公倍数
cyclicity :: Mat -> Double -> Int
cyclicity a lam = go [] 1 (ciNodes ci)
  where
    ci = criticalInfo a lam
    arcs = ciArcs ci
    go _ cyc [] = cyc
    go done cyc (r : rs)
      | r `elem` done = go done cyc rs
      | otherwise =
          let lev = bfs [(r, 0)] [r]
              levOf u = lookup u lev
              comp = [ u | u <- ciNodes ci, levOf u /= Nothing, reachesBack arcs u r ]
              g = foldl' gcd 0 [ lj + 1 - li | (j, i) <- arcs, j `elem` comp, i `elem` comp
                                             , Just lj <- [levOf j], Just li <- [levOf i] ]
              cyc' = if g > 0 then lcm cyc g else cyc
          in go (done ++ comp) cyc' rs
    bfs lev [] = lev
    bfs lev (v : q) = let lv = maybe 0 id (lookup v lev)
                          new = nubOrd' [ i | (j, i) <- arcs, j == v ] lev
                          lev' = lev ++ [ (i, lv + 1) | i <- new ]
                      in bfs lev' (q ++ new)
    nubOrd' is lev = foldl' (\acc i -> if lookup i lev /= Nothing || i `elem` acc then acc else acc ++ [i]) [] is

reachesBack :: [(Int, Int)] -> Int -> Int -> Bool
reachesBack arcs u r = go [u] [u]
  where go _ [] = False
        go seen (v : q) | v == r = True
                        | otherwise = let new = foldl' (\acc i -> if i `elem` seen || i `elem` acc then acc else acc ++ [i]) [] [ i | (j, i) <- arcs, j == v ]
                                      in go (seen ++ new) (q ++ new)

-- べき乗法：x(k+1) = A ⊗ x(k) を回し、はじめて x(k0 + c) = c·λ ⊗ x(k0) になる (k0, c) を探す
--   見つからなければ ppK0 = −1、ppC = 0、ppLam = Nothing
data Period = Period { ppK0 :: !Int, ppC :: !Int, ppLam :: Maybe Double, ppXs :: [Vec] }

powerPeriod :: Mat -> Vec -> Int -> Int -> Period
powerPeriod a x0 kmax cmax = go 1 [x0]
  where
    go k rev
      | k > kmax = Period (-1) 0 Nothing (reverse rev)
      | otherwise =
          let xk = mpMatVec a (headOr [] rev)
              rev' = xk : rev
              hits = [ (c, d) | c <- [1 .. min k cmax], Just d <- [sameShift xk (headOr [] (drop c rev'))] ]
          in case hits of
               ((c, d) : _) -> Period (k - c) c (Just (d / fromIntegral c)) (reverse rev')
               [] -> go (k + 1) rev'
    -- a と b がどの成分も同じ d だけずれているか（ε は ε どうしだけ一致）
    sameShift xa xb = chk Nothing (zip xa xb)
      where chk d [] = d
            chk d ((p, q) : rest)
              | isEps p || isEps q = if p == q then chk d rest else Nothing
              | otherwise = let di = p - q in case d of
                  Nothing -> chk (Just di) rest
                  Just d0 -> if abs (di - d0) > 1e-9 then Nothing else chk d rest

-- 強連結なランダムグラフ：ハミルトン閉路（全ノードを 1 周）を必ず入れる（乱数を使う順は p5.js 版と同じ）
randomIrreducible :: Int -> Double -> Int -> Int -> R Mat
randomIrreducible n p wmin wmax = do
  perm <- shuffle [0 .. n - 1]
  let pv i = vixI perm (i `mod` n)
  ws <- mapM (const (rndInt wmin wmax)) [0 .. n - 1]
  let a0 = foldl' (\m (k, w) -> setM (pv (k + 1)) (pv k) (fromIntegral w) m) (mpMat n n negInf) (zip [0 ..] ws)
  foldM (\m (i, j) -> if isEps (ix m i j)
                        then do u <- rnd
                                if u < p then do w <- rndInt wmin wmax; pure (setM i j (fromIntegral w) m) else pure m
                        else pure m) a0 [ (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  where
    -- フィッシャー–イェーツ（i = n−1 から 1 へ、j = floor(rnd·(i+1))）
    shuffle xs = foldM (\ys i -> do u <- rnd
                                    let j = floor (u * fromIntegral (i + 1))
                                        yi = vixI ys i
                                        yj = vixI ys j
                                    pure [ if k == i then yj else if k == j then yi else y | (k, y) <- zip [0 ..] ys ])
                       xs [n - 1, n - 2 .. 1]

vixI :: [Int] -> Int -> Int
vixI xs k = case drop k xs of { (x : _) | k >= 0 -> x; _ -> 0 }
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

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

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- 矢印（ノードの半径 r の手前で止める）。bend で少し曲げる（往復の辺を分ける）。返り値の 2 つ目 = 数字を書く位置
arrowTo :: Float -> Point -> Point -> Float -> Float -> (Picture, Point)
arrowTo wt (xa, ya) (xb, yb) r bend = (Pictures [thickLine wt (ax, ay) (bx, by), Polygon [(bx, by), (hx - uy * 6, hy + ux * 6), (hx + uy * 6, hy - ux * 6)]], lp)
  where
    dx = xb - xa
    dy = yb - ya
    l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
    ux = dx / l
    uy = dy / l
    ox = -uy * bend
    oy = ux * bend
    ax = xa + ux * r + ox
    ay = ya + uy * r + oy
    bx = xb - ux * r + ox
    by = yb - uy * r + oy
    hx = bx - ux * 12
    hy = by - uy * 12
    lp = ((ax + bx) / 2 + ox * 0.6, (ay + by) / 2 + oy * 0.6)
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
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
