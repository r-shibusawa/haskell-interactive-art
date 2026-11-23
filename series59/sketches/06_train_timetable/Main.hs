-- ============================================
-- 59-06 列車ダイヤ ─ 発車時刻は「全部の条件がそろったら」= max、走る時間 = +。だから x(k) = M ⊗ x(k−1)
-- 駅 S0 − S1 − S2 … を、路線 l の列車（1 編成）が S_l ⇔ S_{l+1} を往復する。S_{l+1} で隣の路線と乗り継ぎを守る
--   同じ周期 k の中の条件 A0、前の周期からの条件 A1 → x(k) = A0 ⊗ x(k) ⊕ A1 ⊗ x(k−1) → x(k) = A0* ⊗ A1 ⊗ x(k−1)
--   全体の周期 λ = M の固有値（カープのアルゴリズム）。乗り継ぎを守ると、路線ごとの往復時間より長くなることがある
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps と書く
-- 操作：2・3・4 路線の数 / ← → 路線を選ぶ / ↑↓ 走る時間 / T 乗り継ぎ時間 / スペース 列車を動かす / R 元にもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub, intercalate)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

turnT :: Double
turnT = 3                  -- ★ 折り返しの時間（分）

kShow :: Int
kShow = 8                  -- 図に描く周期の数

run0 :: [Int]
run0 = [12, 15, 10, 14]    -- ★ 路線ごとの走る時間（分）

-- 行列にした条件：A0（同じ周期）、A1（前の周期から）、M = A0* ⊗ A1、My = 内向き発車だけの行列
data Sys = Sys { sA0, sA1, sM, sMy :: Mat, sN :: !Int }

-- 計算した時刻表（キーを押したときだけ作り直す）
data TT = TT { tSys :: Sys, tLam :: !Double, tXs :: [Vec], tPP :: Period, tCyc :: [Int] }

data World = World
  { wL     :: !Int
  , wRun   :: [Int]
  , wSel   :: !Int
  , wTr    :: !Int            -- ★ 乗り継ぎ時間（分）
  , wPlay  :: !Bool
  , wNow   :: !Double
  , wTT    :: TT
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-06 Train timetable" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuild (World 2 run0 0 4 True 0 undefinedTT [])
  where undefinedTT = TT (Sys [] [] [] [] 0) 0 [] (Period (-1) 0 Nothing []) []

-- ★ 条件を行列に。f_l = 2l（S_l を外向きに発車）、b_l = 2l + 1（S_{l+1} を内向きに発車）
buildSystem :: Int -> [Int] -> Int -> Sys
buildSystem nl run tr = Sys a0 a1 m my n
  where
    n = 2 * nl
    r l = fromIntegral (vixI run l) :: Double
    trD = fromIntegral tr
    a0 = foldl' (\a (i, j, v) -> setM i j v a) (mpMat n n negInf) $
           [ (2 * l + 1, 2 * l, r l + turnT) | l <- [0 .. nl - 1] ]                          -- 着いて折り返したら戻る
           ++ [ (2 * (l + 1), 2 * l, r l + trD) | l <- [0 .. nl - 2] ]                       -- 外向き：内側の列車が着いてから乗り継いで発車
    a1 = foldl' (\a (i, j, v) -> setM i j v a) (mpMat n n negInf) $
           [ (2 * l, 2 * l + 1, r l + turnT) | l <- [0 .. nl - 1] ]                          -- 戻って折り返したら次の周期
           ++ [ (2 * l + 1, 2 * (l + 1) + 1, r (l + 1) + trD) | l <- [0 .. nl - 2] ]         -- 内向き：外側の列車（前の周期）が着いてから発車
    m = mpMatMul (fst (mpStar 1e-9 a0)) a1
    bIdx = [ 2 * l + 1 | l <- [0 .. nl - 1] ]
    my = [ [ ix m i j | j <- bIdx ] | i <- bIdx ]       -- トークンのある事象（内向き発車）だけの行列

-- x(0) … x(K)。x(0) = 0（はじめに全列車が外側の駅で待っている）
timetable :: Sys -> Int -> [Vec]
timetable sys k = take (k + 1) (iterate (mpMatVec (sM sys)) (replicate (sN sys) 0))

rebuild :: World -> World
rebuild w = w { wTT = TT sys lam (timetable sys (kShow + 1)) pp cyc }
  where
    sys = buildSystem (wL w) (wRun w) (wTr w)
    lam = karp (sMy sys)
    pp = powerPeriod (sMy sys) (replicate (wL w) 0) 200 12
    cyc = cyNodes (foldl' (\m c -> if cyMean c > cyMean m then c else m) (Cycle [] 0 negInf) (allCycles (sMy sys)))

tMaxOf :: World -> Double
tMaxOf w = foldl' max 0 (vixL (tXs (wTT w)) kShow) + fromIntegral (maximum (1 : wRun w))
  where vixL xs k = case drop k xs of { (x : _) -> x; [] -> [] }

step :: World -> World
step w | wPlay w = let tm = tMaxOf w; t = wNow w + 0.25 in w { wNow = t - tm * fromIntegral (floor (t / tm) :: Int) }
       | otherwise = w

lineCol :: Int -> Int -> Color
lineCol l a = case l of
  0 -> rgbaC 255 170 70 a
  1 -> rgbaC 110 190 255 a
  2 -> rgbaC 150 230 160 a
  _ -> rgbaC 220 150 255 a

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 駅の線
  [ Color (grayC 60) (Line [(pX g, sy s), (pX g + pW g, sy s)]) | s <- [0 .. nl] ]
  ++ [ sayR ("S" ++ show s) (pX g - 12) (sy s + 5) 15 (grayC 200) | s <- [0 .. nl] ]
  ++ [ sayC (mpFmt 0 t ++ " min") (sx t) (pY g + pH g + 24) 14 (grayC 140) | t <- takeWhile (<= tMax) [0, 30 ..] ]
  -- ---- ダイヤ（時間 − 距離図）
  ++ [ Color (lineCol l (if l == wSel w then 255 else 170))
         (polyline (if l == wSel w then 3 else 2) [ (sx f, sy l), (sx (f + rl), sy (l + 1)), (sx b, sy (l + 1)), (sx (b + rl), sy l), (sx (min fn tMax), sy l) ])
     | l <- [0 .. nl - 1], k <- [1 .. kShow]
     , let rl = runOf l; f = x k (2 * l); b = x k (2 * l + 1); fn = x (k + 1) (2 * l) ]
  -- 動く列車
  ++ [ Color (lineCol l 255) (circleAt (sx tNow) (syF s) 14)
     | l <- [0 .. nl - 1], k <- [1 .. kShow]
     , let rl = runOf l; f = x k (2 * l); b = x k (2 * l + 1)
     , Just s <- [ if tNow >= f && tNow < f + rl then Just (fromIntegral l + (tNow - f) / rl)
                   else if tNow >= f + rl && tNow < b then Just (fromIntegral l + 1)
                   else if tNow >= b && tNow < b + rl then Just (fromIntegral l + 1 - (tNow - b) / rl)
                   else Nothing ] ]
  ++ [ Color (rgbaC 255 255 255 90) (Line [(sx tNow, pY g), (sx tNow, pY g + pH g)]) ]
  -- ---- 文字
  ++ [ say ("Train timetable  " ++ show nl ++ " lines, transfer " ++ show (wTr w) ++ " min, turn " ++ mpFmt 0 turnT ++ " min  ->  period lambda = "
            ++ mpFmt 2 lam ++ " min (" ++ (if lam > 0 then fmt 2 (60 / lam) else "?") ++ " per hour)") 40 40 20 (grayC 240)
     , say ("2 3 4 lines   <- -> select line (now " ++ show (wSel w + 1) ++ ")   Up/Down run time   T transfer   Space run / stop   R reset") 40 66 15 (grayC 170)
     , say ("Round trip 2(run + turn) per line: " ++ intercalate ", " [ "line " ++ show (l + 1) ++ " = " ++ show (2 * (r + 3)) | (l, r) <- zip [0 :: Int ..] (take nl (wRun w)) ]
            ++ "  -> with transfers lambda = " ++ mpFmt 2 lam) 40 y0 16 (grayC 220)
     , say ("Slowest (critical) circuit: " ++ intercalate " -> " [ "line " ++ show (v + 1) | v <- cyc ]
            ++ (if length cyc > 1 then " (" ++ show (length cyc) ++ " lines linked by transfers hold it back)" else " (this line's round trip is the bottleneck)")) 40 (y0 + 26) 16 (rgbC 255 220 120)
     , sayWrap ("Transient: from period " ++ show (ppK0 pp) ++ " on, x(k) repeats every " ++ show (ppC pp) ++ " periods, advancing lambda*" ++ show (ppC pp)
                ++ " min (power method). M (inward departures only) = [" ++ intercalate "," [ "[" ++ intercalate "," (map mpF r) ++ "]" | r <- sMy (tSys tt) ] ++ "]") 40 (y0 + 36) (envW env - 80) 15 (grayC 190)
     , drawSelfTest env (wTest w) ]
  where
    nl = wL w
    tt = wTT w
    xs = tXs tt
    lam = tLam tt
    pp = tPP tt
    cyc = tCyc tt
    tMax = tMaxOf w
    tNow = wNow w
    g = G 110 130 (envW env - 160) (envH env - 330) 0 1 0 1
    sx t = pX g + f2 (t / tMax) * pW g
    syF s = pY g + pH g - f2 (s / fromIntegral nl) * pH g
    sy :: Int -> Float
    sy s = syF (fromIntegral s)
    x k i = vix (case drop k xs of { (v : _) -> v; [] -> [] }) i
    runOf l = fromIntegral (vixI (wRun w) l) :: Double
    y0 = pY g + pH g + 56

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "234" -> let k = read [c] in rebuild w { wL = k, wSel = min (wSel w) (k - 1) }
    | c `elem` "tT" -> rebuild w { wTr = (wTr w + 2) `mod` 14 }
    | c `elem` "rR" -> rebuild w { wRun = run0, wTr = 4, wL = 2, wSel = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlay = not (wPlay w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w + wL w - 1) `mod` wL w }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` wL w }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild (setRun (\r -> min 30 (r + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild (setRun (\r -> max 4 (r - 1)))
  _ -> w
  where setRun f = w { wRun = [ if l == wSel w then f r else r | (l, r) <- zip [0 ..] (wRun w) ] }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 6・同じ乱数の順）
-- 答え合わせ：行列を使わず、条件を 1 つずつ max でたどる。(F, B) の周期ごとのリスト
directTimetable :: Int -> [Int] -> Int -> Int -> [(Vec, Vec)]
directTimetable nl run tr kk = take (kk + 1) (iterate next (replicate nl 0, replicate nl 0))
  where
    r l = fromIntegral (vixI run l) :: Double
    trD = fromIntegral tr
    next (_, bPrev) =
      let f0 = [ vix bPrev l + r l + turnT | l <- [0 .. nl - 1] ]
          f = foldl' (\fs l -> [ if m == l then max v (vix fs (l - 1) + r (l - 1) + trD) else v | (m, v) <- zip [0 ..] fs ]) f0 [1 .. nl - 1]
          b = foldr (\l bs -> let base = vix f l + r l + turnT
                                  v = if l + 1 < nl then max base (vix bPrev (l + 1) + r (l + 1) + trD) else base
                              in v : bs) [] [0 .. nl - 1]
      in (f, b)

selfTest :: [(String, Bool)]
selfTest =
  [ ("M (inward departures) is strongly connected (irreducible): 24 random cases of 2-4 lines", all (\(a, _, _, _, _, _) -> a) rs)
  , ("Karp's lambda = maximum mean over all circuits (brute force)", all (\(_, a, _, _, _, _) -> a) rs)
  , ("same-period conditions A0 have no circuit (A0^n = eps matrix, A0* finite)", all (\(_, _, a, _, _, _) -> a) rs)
  , ("matrix recursion x(k) = A0* (x) A1 (x) x(k-1) = timetable traced condition by condition (30 periods)", all (\(_, _, _, a, _, _) -> a) rs)
  , ("power method: eventually x(k + c) = x(k) + c lambda (slope = Karp's lambda)", all (\(_, _, _, _, a, _) -> a) rs)
  , ("first example: lambda = 36 (line 2 round trip 2(15 + 3)); with transfer 8, lambda = 12 + 15 + 3 + 8 = 38"
    , karp (sMy (buildSystem 2 [12, 15] 4)) == 36 && karp (sMy (buildSystem 2 [12, 15] 8)) == 38 && all (\(_, _, _, _, _, a) -> a) rs) ]
  where
    rs = withSeed 6 (mapM one [0 .. 23 :: Int])
    one t = do
      let nl = 2 + t `mod` 3
      run <- replicateM 4 (rndInt 5 25)
      tr <- rndInt 0 12
      let sys = buildSystem nl run tr
          lam = karp (sMy sys)
          okIrr = isIrreducible (sMy sys)
          okK = abs (lam - bruteMaxMean (sMy sys)) <= 1e-9
          okNil = not (snd (mpStar 1e-9 (sA0 sys))) && all isEps (concat (mpPow (sA0 sys) (sN sys)))
          xs = timetable sys 30
          d = directTimetable nl run tr 30
          okD = and [ vix xk (2 * l) == vix fk l && vix xk (2 * l + 1) == vix bk l
                    | (k, xk, (fk, bk)) <- zip3 [0 :: Int ..] xs d, k >= 1, l <- [0 .. nl - 1] ]
          pp = powerPeriod (sMy sys) (replicate nl 0) 300 12
          okP = ppC pp > 0 && maybe False (\l -> abs (l - lam) < 1e-9) (ppLam pp)
          okN = noNaN (concat xs)
      pure (okIrr, okK, okNil, okD, okP, okN)

-- ---------- やってみよう ----------
-- 1. T で乗り継ぎ時間を長くしていくと、どこで λ が路線ごとの往復より長くなる？ 臨界回路の表示も見よう
-- 2. 路線 1 の走る時間を ↑ で長くすると、路線 2 のダイヤ（青）はどう変わる？（待ち = 水平の線）
-- 3. 4 路線にして、まん中の路線だけ遅くすると？ 遅れはどこまで広がる？（10 で詳しく）
-- 4. λ が決まると「1 時間に何本」が決まる。60 / λ を計算してみよう
-- 5. turnT（折り返し時間）を 0 にすると、λ はいくつ減る？

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
-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

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
