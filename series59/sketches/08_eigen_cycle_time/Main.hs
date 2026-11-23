-- ============================================
-- 59-08 固有値とサイクル時間 ─ A ⊗ v = λ ⊗ v。λ = 「いちばん遅い回路の 1 本あたりの平均」（最大平均回路）
-- a_ij = 事象 j が起きてから i が起きるまでの時間（辺 j → i）。x(k+1) = A ⊗ x(k) は、やがて 1 周期に λ ずつ進む
--   λ はカープのアルゴリズム（D_k = ちょうど k 本の道の最大）で、全部の回路を調べなくても計算できる
--   臨界グラフ（オレンジ）= 平均 λ の回路の辺。固有ベクトル v = (A − λ)* の臨界ノードの列 → 「定常ダイヤ」
--   グラフは必ず強連結（ハミルトン閉路を入れてある）。強連結でないと λ が場所ごとにちがうことがある
--   画面では ⊗ を (x)、ε を eps、λ を lambda と書く
-- 操作：R 新しいグラフ / 4〜7 ノードの数 / ノードをドラッグ / 辺の数字をクリックして ↑↓ 重み / スペース x(k) を 1 つずつ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 固有値・臨界グラフ・固有ベクトル・x(k) をまとめたもの
data Info = Info
  { iLam   :: !Double
  , iCi    :: Crit
  , iXs    :: [Vec]        -- x(0) … x(40)
  , iAv    :: Vec          -- A ⊗ v
  , iBrute :: !Double      -- 全部の回路の平均の最大
  , iIrr   :: !Bool }

data World = World
  { wN     :: !Int                  -- ★ ノードの数
  , wA     :: Mat
  , wPos   :: [(Float, Float)]
  , wInfo  :: Info
  , wSelArc :: Maybe (Int, Int)
  , wDragN :: !Int
  , wKShow :: !Int
  , wSeedG :: !Word32               -- ★ グラフの種
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-08 Eigenvalue and cycle time" (newGraph (1280, 800) world0))
  { skBack  = rgbC 22 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (newGraph (envW env, envH env) world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 5 [] [] (analyse []) Nothing (-1) 30 8 []

newGraph :: (Float, Float) -> World -> World
newGraph (w, h) wd = wd { wA = a, wPos = pos, wInfo = analyse a }
  where
    n = wN wd
    a = withSeed (wSeedG wd) (randomIrreducible n 0.25 1 9)
    pos = [ (w * 0.22 + 180 * cos (tau * fromIntegral i / fromIntegral n - pi / 2), h * 0.47 + 180 * sin (tau * fromIntegral i / fromIntegral n - pi / 2)) | i <- [0 .. n - 1] ]

-- ★ 固有値・臨界グラフ・固有ベクトル・x(k) をまとめて計算
analyse :: Mat -> Info
analyse a = Info lam ci xs av (bruteMaxMean a) (isIrreducible a)
  where
    lam = karp a
    ci = criticalInfo a lam
    xs = take 41 (iterate (mpMatVec a) (replicate (length a) 0))
    av = maybe [] (mpMatVec a) (ciVec ci)

colsN :: [(Int, Int, Int)]
colsN = [(255, 170, 70), (110, 190, 255), (150, 230, 160), (255, 220, 120), (220, 150, 255), (255, 130, 150), (180, 180, 180)]

colN :: Int -> Color
colN i = let (r, g, b) = headOr (200, 200, 200) (drop i colsN) in rgbC r g b

posOf :: World -> Int -> (Float, Float)
posOf w i = headOr (0, 0) (drop i (wPos w))

bendOf :: World -> Int -> Int -> Float
bendOf w i j = if isEps (ix (wA w) j i) then 0 else 9

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- グラフ（辺 j → i の重み a_ij）
  concat [ let c = (j, i) `elem` ciArcs (iCi info)
               s = wSelArc w == Just (i, j)
               (x, y) = posOf w i
               (pic, lp) = if i == j then (ringAt x (y - 34) 32 (if c then 4 else 1.5), (x, y - 58))
                           else arrowTo (if c then 4 else 1.5) (posOf w j) (posOf w i) 24 (bendOf w i j)
           in [ Color (if c then rgbC 255 170 70 else rgbaC 140 160 190 170) pic
              , Color (if s then rgbC 255 220 120 else grayC 30) (circleAt (fst lp) (snd lp) 24)
              , sayM (mpFmt 0 v) (fst lp) (snd lp) 14 (grayC (if s then 20 else 235)) ]
         | (i, row) <- zip [0 ..] (wA w), (j, v) <- zip [0 ..] row, not (isEps v) ]
  ++ concat [ [ Color (rgbC 60 90 140) (circleAt x y 44)
              , Color (if cn then rgbC 255 170 70 else grayC 200) (ringAt x y 44 (if cn then 4 else 1.5))
              , sayM (show i) x y 17 (grayC 255)
              , sayM ("v = " ++ maybe "?" (mpFmt 2 . (`vix` i)) (ciVec (iCi info))) x (y + 36) 14 (rgbC 150 230 160) ]
            | i <- [0 .. wN w - 1], let (x, y) = posOf w i, let cn = i `elem` ciNodes (iCi info) ]
  -- ---- x(k) のグラフ
  ++ [ plotAxes g1 "k" "x_i(k)   (dashed = line k*lambda with slope lambda)" [0, 10, 20, 30, 40] [0, jsRound (y1 g1)] ]
  ++ [ Color (colN i) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 (vix x i)) | (k, x) <- zip [0 :: Int ..] (take (wKShow w + 1) xs) ]) | i <- [0 .. wN w - 1] ]
  ++ [ Color (rgbaC 255 255 255 120) (dashed (gX g1 0, gY g1 0) (gX g1 40, gY g1 (40 * lam))) ]
  ++ [ plotAxes g2 "k" "x_i(k) / k -> lambda" [0, 10, 20, 30, 40] (map (\v -> read (fmt 2 v) :: Double) [lam - 3, lam, lam + 3])
     , Color (rgbC 255 220 120) (Line [(pX g2, gY g2 lam), (pX g2 + pW g2, gY g2 lam)]) ]
  ++ [ Color (colN i) (polyline 2 [ (gX g2 (fromIntegral k + 1), gY g2 (vix x i / (fromIntegral k + 1))) | (k, x) <- zip [0 :: Int ..] (take (wKShow w) (drop 1 xs)) ]) | i <- [0 .. wN w - 1] ]
  -- ---- 文字
  ++ [ say ("Eigenvalue and cycle time   lambda = " ++ mpFmt 3 lam ++ " (Karp)") 40 40 20 (grayC 240)
     , say ("all cycles: max mean = " ++ mpFmt 3 (iBrute info) ++ (if iIrr info then "   strongly connected: yes" else "   NOT strongly connected!")) 700 40 15 (grayC 240)
     , say ("R new graph   4-7 nodes   drag nodes   click an arc number, Up/Down weight   Space step k (now k = " ++ show (wKShow w) ++ ")") 40 66 15 (grayC 170)
     , say ("A (x) v = (" ++ joinWith ", " (map (mpFmt 2) (iAv info)) ++ ")   lambda (x) v = (" ++ joinWith ", " (map (mpFmt 2 . mpMul lam) vec) ++ ")"
            ++ (if mpVecEq 1e-7 (iAv info) (map (+ lam) vec) && not (null vec) then "   OK equal" else "")) 40 ty 16 (rgbC 150 230 160)
     , sayWrap ("Critical graph (orange) arcs: " ++ joinWith ", " [ show j ++ "->" ++ show i | (j, i) <- ciArcs (iCi info) ]
                ++ " = arcs on cycles with mean lambda. Raising one of these weights by 1 raises lambda.") 40 (ty + 10) (envW env - 80) 16 (rgbC 255 200 120)
     , sayWrap "Started from v, all x_i(k) move with slope lambda at once (steady timetable). From 0 they settle after a while (09)." 40 (ty + 56) (envW env - 300) 14 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    info = wInfo w
    lam = iLam info
    xs = iXs info
    vec = maybe [] id (ciVec (iCi info))
    gx = envW env * 0.5
    gw = envW env * 0.46
    gh = (envH env - 350) / 2
    g1 = G gx 120 gw gh 0 40 0 (max 1 (foldl' max negInf (headOr [] (drop 40 xs))))
    g2 = G gx (pY g1 + pH g1 + 70) gw gh 0 40 (lam - 3) (lam + 3)
    ty = envH env - 110

-- 破線（5 ピクセルずつ）
dashed :: (Float, Float) -> (Float, Float) -> Picture
dashed (xa, ya) (xb, yb) =
  let l = sqrt ((xb - xa) ^ (2 :: Int) + (yb - ya) ^ (2 :: Int))
      n = floor (l / 10) :: Int
      pt t = (xa + (xb - xa) * t, ya + (yb - ya) * t)
  in if l < 1 then Blank else Pictures [ Line [pt (fromIntegral k * 10 / l), pt (min 1 ((fromIntegral k * 10 + 5) / l))] | k <- [0 .. n] ]

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

labelAt :: World -> (Float, Float) -> Maybe (Int, Int)
labelAt w (x, y) = headOr Nothing
  [ Just (i, j) | (i, row) <- zip [0 ..] (wA w), (j, v) <- zip [0 ..] row, not (isEps v)
  , let (px, py) = lp i j, sqrt ((x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)) < 13 ]
  where
    -- p5 の labelAt と同じ位置
    lp i j
      | i == j = let (px, py) = posOf w i in (px, py - 58)
      | otherwise = let (xi, yi) = posOf w i
                        (xj, yj) = posOf w j
                        dx = xi - xj
                        dy = yi - yj
                        l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
                        b = bendOf w i j
                    in ((xi + xj) / 2 - dy / l * b * 1.6, (yi + yj) / 2 + dx / l * b * 1.6)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let q@(mx, my) = toP5 env p
    in case labelAt w q of
         Just l -> w { wSelArc = Just l }
         Nothing -> w { wDragN = foldl' (\d i -> let (px, py) = posOf w i in if sqrt ((mx - px) ^ (2 :: Int) + (my - py) ^ (2 :: Int)) < 22 then i else d) (-1) [0 .. wN w - 1] }
  EventMotion p | wDragN w >= 0 -> w { wPos = [ if i == wDragN w then toP5 env p else q | (i, q) <- zip [0 ..] (wPos w) ] }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragN = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> newGraph sz w { wSeedG = wSeedG w + 1, wSelArc = Nothing }
    | c `elem` "4567" -> newGraph sz w { wN = fromEnum c - fromEnum '0', wSelArc = Nothing }
  EventKey (SpecialKey KeyUp) Down _ _ -> bump 1
  EventKey (SpecialKey KeyDown) Down _ _ -> bump (-1)
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wKShow = if wKShow w >= 40 then 1 else wKShow w + 1 }
  _ -> w
  where
    sz = (envW env, envH env)
    bump d = case wSelArc w of
      Just (i, j) -> let a = setM i j (clampv 0 15 (ix (wA w) i j + d)) (wA w) in w { wA = a, wInfo = analyse a }
      Nothing -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 80・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Karp's lambda = max mean over all simple cycles (40 random strongly connected graphs)", all (\(k, _, _, _, _) -> k) rs)
  , ("eigenvector v (critical column of (A - lambda)*) is finite and A (x) v = lambda (x) v", all (\(_, v, _, _, _) -> v) rs)
  , ("critical graph arcs = arcs on cycles with mean lambda (exactly the same set)", all (\(_, _, c, _, _) -> c) rs)
  , ("power method: x(k + c) = x(k) + c*lambda (slope = lambda)", all (\(_, _, _, p, _) -> p) rs)
  , ("x(400)/400 is within +-0.1 of lambda", all (\(_, _, _, _, l) -> l) rs)
  , ("not strongly connected (two separate self-loops) is detected; lambda = 5 but x_1(k)/k -> 2"
    , not (isIrreducible rd) && karp rd == 5 && ix (mpPow rd 10) 0 0 == 20) ]
  where
    rd = [[2, negInf], [negInf, 5]]
    rs = withSeed 80 (mapM one [0 .. 39 :: Int])
    one t = do
      let n = 3 + t `mod` 5
      a <- randomIrreducible n 0.3 1 9
      let lam = karp a
          ci = criticalInfo a lam
          cyc = allCycles a
          okK = isIrreducible a && abs (lam - bruteMaxMean a) <= 1e-9
          okV = case ciVec ci of
                  Just v -> all (\x -> not (isInfinite x || isNaN x)) v && mpVecEq 1e-7 (mpMatVec a v) (map (+ lam) v)
                  Nothing -> False
          onMax = [ (v, headOr 0 (drop ((q + 1) `mod` length ns) ns)) | c <- cyc, abs (cyMean c - lam) < 1e-9, let ns = cyNodes c, (q, v) <- zip [0 ..] ns ]
          critS = ciArcs ci
          okC = all (`elem` onMax) critS && all (`elem` critS) onMax
          pp = powerPeriod a (replicate n 0) 400 24
          okP = ppC pp > 0 && maybe False (\l -> abs (l - lam) < 1e-9) (ppLam pp)
          x400 = headOr [] (drop 400 (iterate (mpMatVec a) (replicate n 0)))
          okL = all (\v -> abs (v / 400 - lam) < 0.1) x400
      pure (okK, okV, okC, okP, okL)

-- ---------- やってみよう ----------
-- 1. オレンジ（臨界）の辺の重みを ↑ で 1 増やすと λ はどう変わる？ オレンジでない辺なら？
-- 2. R で何回かグラフを変え、λ がどの回路で決まるか予想してから確かめよう
-- 3. 固有ベクトル v の差 v_i − v_j は「定常ダイヤでの i と j の時刻の差」。グラフで確かめよう
-- 4. 下のグラフ x_i(k)/k は、どれくらいの k で λ にそろう？ 7 ノードでは？
-- 5. 06・07 の λ も、このカープのアルゴリズムで計算している（06）

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
