-- ============================================
-- 59-05 線形方程式を解く ─ 引き算がないのに、どうやって A ⊗ x = b を解く？
-- 部品 j を時刻 x_j に投入すると、製品 i は max_j (a_ij + x_j) = (A ⊗ x)_i に完成する。締め切りは b_i
--   「全部の製品が締め切りに間に合う中で、いちばん遅く始める」= A ⊗ x ≤ b の最大の x（残余・最大劣解）
--   x⁺_j = min_i (b_i − a_ij)。A ⊗ x⁺ = b なら方程式は解ける、ちがえばどんな x でも解けない
-- 下：x = A ⊗ x ⊕ b（「準備 b_i と、前の作業 j の a_ij 後の遅いほう」）の最小解は x = A* ⊗ b。x ← A ⊗ x ⊕ b をくり返すと n 回以内で止まる
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps、x⁺ を x+ と書く
-- 操作：● ドラッグ 締め切り b_i / 表のマスにマウスで ↑↓ a_ij、E = ε / R ランダム / N 下の行列を作り直す
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

tSpan :: Double
tSpan = 30

data World = World
  { wA    :: Mat              -- ★ a_ij = 部品 j の投入から製品 i の完成まで（分）
  , wB    :: Vec              -- ★ 締め切り b_i（分）
  , wQ    :: Mat              -- ★ 下の x = Q ⊗ x ⊕ b
  , wBQ   :: Vec
  , wDrag :: !Int
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-05 Linear equations" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [[5, 8, negInf], [negInf, 4, 6], [3, negInf, 7]] [20, 18, 22]
               [[negInf, 2, negInf], [negInf, negInf, 3], [-6, negInf, negInf]] [1, 0, 2] (-1) []

-- ★ 最小解 x = A* ⊗ b（正の回路があれば解なし = Nothing）と、くり返し x ← A ⊗ x ⊕ b の途中経過
leastSolution :: Mat -> Vec -> Maybe (Vec, [Vec])
leastSolution a b
  | pos = Nothing
  | otherwise = Just (mpMatVec st b, take (length a + 2) (iterate (\x -> zipWith mpAdd (mpMatVec a x) b) b))
  where (st, pos) = mpStar 1e-9 a

tlX :: Float
tlX = 380

tx :: Env -> Double -> Float
tx env t = tlX + f2 (clampv (-5) tSpan t / tSpan) * (envW env - 440)

yIn, yOut :: Int -> Float
yIn j = 150 + fromIntegral j * 46
yOut i = 340 + fromIntegral i * 46

-- 表のマス（マウスの下にあるもの）
cellAt :: (Float, Float) -> Maybe (Int, Int)
cellAt (mx, my) = headOr Nothing [ Just (i, j) | i <- [0 .. 2], j <- [0 .. 2]
                                 , let x = 40 + fromIntegral j * 46; y = 120 + fromIntegral i * 46
                                 , mx > x, mx < x + 46, my > y, my < y + 46 ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Solving linear equations: A (x) x = b" 40 40 20 (grayC 240)
  , say "Drag a dot: deadline b_i   mouse on a cell: Up/Down a_ij, E = eps   R random   N new lower matrix" 40 66 15 (grayC 170) ]
  -- ---- 時間軸
  ++ [ Color (grayC 55) (Line [(tx env t, 120), (tx env t, 470)]) | t <- [0, 5 .. tSpan] ]
  ++ [ sayC (mpFmt 0 t) (tx env t) 492 14 (grayC 140) | t <- [0, 5 .. tSpan] ]
  -- 部品 → 製品の線（x⁺_j + a_ij。ぴったり締め切りに着く線 = 効いている制約）
  ++ [ let tight = abs (xj + aij - vix b i) < 1e-9
       in Color (if tight then rgbC 255 170 70 else rgbaC 120 140 170 120) (lineW (if tight then 3 else 1.5) (tx env xj, yIn j) (tx env (xj + aij), yOut i))
     | i <- [0 .. 2], j <- [0 .. 2], let aij = ix a i j, let xj = vix xh j, not (isEps aij), not (isInfinite xj) ]
  ++ concat [ [ Color (rgbC 110 190 255) (circleAt (tx env (if isInfinite xj then tSpan else xj)) (yIn j) 16)
              , sayR ("x+" ++ show (j + 1) ++ " = " ++ mpF xj ++ " (part " ++ show (j + 1) ++ ")") (tlX - 20) (yIn j + 5) 14 (grayC 220) ]
            | j <- [0 .. 2], let xj = vix xh j ]
  ++ concat [ [ Color (rgbC 255 220 120) (lineW 2 (tx env bi, yOut i - 16) (tx env bi, yOut i + 16))
              , Color (rgbC 255 220 120) (circleAt (tx env bi) (yOut i - 18) 12)
              , if isEps axi then Blank else Color (rgbC 150 230 160) (circleAt (tx env axi) (yOut i) 12)
              , sayR ("product " ++ show (i + 1) ++ ": done " ++ mpF axi ++ ", b = " ++ mpF bi) (tlX - 20) (yOut i + 5) 14 (grayC 220) ]
            | i <- [0 .. 2], let bi = vix b i, let axi = vix ax i ]
  -- ---- 行列 A
  ++ [ say "A (row = product, col = part)" 40 112 14 (grayC 200) ]
  ++ concat [ [ Color (if isEps v then grayC 28 else grayC 42) (rectTL x y 46 46)
              , Color (if hover == Just (i, j) then rgbC 255 220 120 else grayC 70) (frameTL (if hover == Just (i, j) then 2 else 1) x y 46 46)
              , sayM (mpF v) (x + 23) (y + 23) 15 (if isEps v then grayC 110 else grayC 240) ]
            | i <- [0 .. 2], j <- [0 .. 2], let x = 40 + fromIntegral j * 46, let y = 120 + fromIntegral i * 46, let v = ix a i j ]
  ++ [ say (if solv then "OK  A (x) x+ = b: solvable exactly (every product is done exactly at its deadline)"
            else "NO  A (x) x+ /= b: some product is done early -> this equation has no solution") 40 530 17 (if solv then rgbC 120 210 150 else rgbC 255 170 110)
     , sayWrap "x+_j = min_i (b_i - a_ij): delay part j by even 1 min past x+_j and the product on the orange line misses its deadline (the greatest subsolution)" 40 540 (envW env - 80) 15 (grayC 200) ]
  -- ---- 下：x = Q ⊗ x ⊕ b
  ++ [ say ("x = Q (x) x (+) b   Q = [" ++ intercalate "," [ "[" ++ intercalate "," (map mpF r) ++ "]" | r <- wQ w ] ++ "], b = (" ++ intercalate ", " (map mpF (wBQ w)) ++ ")") 40 616 17 (grayC 240) ]
  ++ (case leastSolution (wQ w) (wBQ w) of
        Nothing -> [ say "Q has a positive circuit -> no least solution (iterating grows forever). Press N" 40 642 15 (grayC 200) ]
        Just (x, it) ->
          [ sayWrap ("Iterate x <- Q (x) x (+) b:  " ++ intercalate "   " [ "x" ++ show k ++ " = (" ++ intercalate ", " (map mpF v) ++ ")" | (k, v) <- zip [0 :: Int ..] it ]) 40 628 (envW env - 80) 15 (grayC 200)
          , say ("Least solution x = Q* (x) b = (" ++ intercalate ", " (map mpF x) ++ ")  -> equals the iteration") 40 700 15 (rgbC 150 230 160) ])
  ++ [ drawSelfTest env (wTest w) ]
  where
    a = wA w
    b = wB w
    xh = mpResid a b
    ax = mpMatVec a xh
    solv = mpVecEq 1e-9 ax b
    hover = cellAt (mouseP env)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hits = [ i | i <- [0 .. 2], sqrt ((mx - (tlX + f2 (vix (wB w) i / tSpan) * (envW env - 440))) ^ (2 :: Int) + (my - (yOut i - 18)) ^ (2 :: Int)) < 14 ]
    in pure w { wDrag = if null hits then -1 else last hits }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion p | wDrag w >= 0 ->
    let (mx, _) = toP5 env p
        v = jsRound (clampv 0 tSpan (realToFrac ((mx - 380) / (envW env - 440)) * tSpan))
    in pure w { wB = [ if i == wDrag w then v else x | (i, x) <- zip [0 ..] (wB w) ] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (onHover (\v -> if isEps v then 0 else min 15 (v + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (onHover (\v -> if isEps v then 0 else max 0 (v - 1)))
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> pure (onHover (\v -> if isEps v then 3 else negInf))
    | c `elem` "rR" -> do
        a' <- mapM (mapM (const (do u <- randomRIO (0, 1 :: Double)
                                    if u < 0.3 then pure negInf else fromIntegral <$> randomRIO (0, 9 :: Int)))) (wA w)
        b' <- mapM (const (fromIntegral <$> randomRIO (10, 24 :: Int))) (wB w)
        pure w { wA = a', wB = b' }
    | c `elem` "nN" -> do
        q' <- sequence [ sequence [ do u <- randomRIO (0, 1 :: Double)
                                       if i == j || u < 0.5 then pure negInf else fromIntegral <$> randomRIO (-5, 3 :: Int)
                                  | j <- [0 .. 2 :: Int] ] | i <- [0 .. 2 :: Int] ]
        bq <- replicateM 3 (fromIntegral <$> randomRIO (0, 4 :: Int))
        pure w { wQ = q', wBQ = bq }
  _ -> pure w
  where
    onHover f = case cellAt (mouseP env) of
      Just (i, j) -> w { wA = setM i j (f (ix (wA w) i j)) (wA w) }
      Nothing -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種 5・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("residuation x+ = A \\ b satisfies A (x) x+ <= b (with eps, no NaN)", all (\(r, _, _, _, _, _) -> r) rs)
  , ("greatest: raising x+_j by 0.5 always breaks a deadline", all (\(_, r, _, _, _, _) -> r) rs)
  , ("every random y with A (x) y <= b has y <= x+", all (\(_, _, r, _, _, _) -> r) rs)
  , ("with b = A (x) y: A (x) x+ = b (solvable) and y <= x+", all (\(_, _, _, r, _, _) -> r) rs)
  , ("if A (x) x+ /= b, no grid point y gives A (x) y = b (26^3 cases, where applicable)", all (\(_, _, _, _, r, _) -> r) rs)
  , ("least solution Q* (x) b of x = Q (x) x (+) b: solves it, equals the iteration, below other solutions", all (\(_, _, _, _, _, r) -> r) rs)
  , ("a part in no row (column all eps) gets x+ = +inf, no NaN in A (x) x+"
    , let a = [[3, negInf], [4, negInf]]; x = mpResid a [10, 9] in vix x 1 == posInf && noNaN (mpMatVec a x) && vix x 0 == 5) ]
  where
    rs = withSeed 5 (mapM one [0 .. 59 :: Int])
    randMat p lo hi offDiag = foldM (\m (i, j) -> if offDiag && i == j then pure m else do
                                        u <- rnd
                                        if u < p then do v <- rndInt lo hi; pure (setM i j (fromIntegral v) m) else pure m)
                                     (mpMat 3 3 negInf) [ (i, j) | i <- [0 .. 2], j <- [0 .. 2 :: Int] ]
    vec3 lo hi = replicateM 3 (fromIntegral <$> rndInt lo hi)
    one t = do
      a <- randMat 0.7 0 9 False
      b <- vec3 5 20
      let xh = mpResid a b
          ax = mpMatVec a xh
          okLeq = noNaN ax && mpVecLeq 1e-9 ax b
          okMax = and [ not (mpVecLeq 1e-9 (mpMatVec a [ if k == j then v + 0.5 else v | (k, v) <- zip [0 ..] xh ]) b)
                      | j <- [0 .. 2], not (isInfinite (vix xh j)) ]
      samp <- replicateM 200 (vec3 (-5) 20)
      let okSamp = and [ not (mpVecLeq 1e-9 (mpMatVec a y) b) || and (zipWith (<=) y xh) | y <- samp ]
      y <- vec3 0 10
      let by = mpMatVec a y
          okSolv = any isEps by || (let x2 = mpResid a by in mpVecEq 1e-9 (mpMatVec a x2) by && and (zipWith (<=) y x2))
          grid = [-5 .. 20]
          okNo = mpVecEq 1e-9 ax b || t >= 20 || not (or [ mpVecEq 1e-9 (mpMatVec a [p, q, r]) b | p <- grid, q <- grid, r <- grid ])
      q <- randMat 0.5 (-6) 3 True
      okLeast <- case leastSolution q b of
        Just (x, it) -> do
          extra <- replicateM 3 (fromIntegral <$> rndInt 0 4)
          let fix = zipWith mpAdd (mpMatVec q x) b
              other = mpMatVec (fst (mpStar 1e-9 q)) (zipWith (+) b extra)
          pure (mpVecEq 1e-9 fix x && mpVecEq 1e-9 (last (x : it)) x && and (zipWith (<=) x other))
        Nothing -> pure (bruteMaxMean q > 0)
      pure (okLeq, okMax, okSamp, okSolv, okNo, okLeast)

-- ---------- やってみよう ----------
-- 1. 製品 1 の締め切りを ● で早くすると、どの部品の投入 x⁺ が早まる？ オレンジの線に注目
-- 2. OK（ぴったり解ける）になる締め切りの組を探そう。NO のとき、早く完成する製品はどれ？
-- 3. 表の a_ij を E で ε にすると、その部品は何に縛られる？ 列が全部 ε なら？
-- 4. 下の式で N を押し、くり返しが何回で止まるか数えよう（n = 3 回以内）
-- 5. 04 の A* と下の Q* は同じもの。「準備 b」を加えると PERT の最早開始時刻そのもの

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
