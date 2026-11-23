-- ============================================
-- 59-10 遅延の伝わり方 ─ 1 本の列車が d 分遅れると、乗り継ぎを守る列車にも遅れがうつる。どこまで広がり、何周期で消える？
-- 時刻表（ダイヤ）：周期 T = λ + 余裕 s で、計画 x̄(k) = v + k·T（v = 固有ベクトル、08）。列車は計画より早くは出ない
--   x(k+1) = A ⊗ x(k) ⊕ x̄(k+1)。遅れ z(k) = x(k) − x̄(k) ≥ 0 は 1 周期ごとに少なくとも s ずつ減る → 回復は ⌈d / s⌉ 周期以内
--   余裕 s = 0 だと、臨界回路にのった遅れはいつまでも消えない
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps、★ を * と書く
-- 操作：マスをクリック = その列車・周期に遅れを入れる / ← → 遅れ d / ↑↓ 余裕 s / 1・2 路線網 / R 遅れを消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ 路線網：a_ij = 列車 j が出てから、次の周期に列車 i が出られるまで（分）
data Net = Net { nName :: String, nNames :: [String], nA :: Mat }

nets :: [Net]
nets =
  [ Net "4 lines in a ring of transfers" ["Red", "Blue", "Green", "Yellow"]
      [[52, 56, e, e], [e, 55, 57, e], [e, e, 50, 55], [56, e, e, 48]]
  , Net "5 lines all transferring at a hub" ["Line 1", "Line 2", "Line 3", "Line 4", "Line 5"]
      [[40, 38, e, e, 36], [37, 42, 39, e, e], [e, 40, 41, 38, e], [e, e, 36, 44, 40], [39, e, e, 41, 38]] ]
  where e = negInf

kN :: Int
kN = 26                    -- 計算する周期の数

-- 計算結果：λ、周期 T、計画、実際、遅れ z、回復までの周期、最後に遅れがあった周期
data Sim = Sim { sLam, sT :: !Double, sPlan, sX, sZ :: [Vec], sRecover, sLast :: !Int, sCrit :: [Int] }

data World = World
  { wNet   :: !Int
  , wSlack :: !Int            -- ★ 余裕 s（分）
  , wD     :: !Int            -- ★ 遅れ d（分）
  , wAt    :: !(Int, Int)     -- ★ (列車, 周期)
  , wSim   :: Sim
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-10 Delay propagation" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = run (World 0 3 20 (1, 2) (propagate [[0]] 0 0 0 0 0) [])

netOf :: World -> Net
netOf w = case drop (wNet w) nets of { (n : _) -> n; [] -> head nets }

-- ★ 計画ダイヤと実際の発車時刻（遅れを 1 つ入れる）
propagate :: Mat -> Double -> Double -> Int -> Int -> Int -> Sim
propagate a s d who kd kk = Sim lam t plan xs z recover lastK []
  where
    lam = karp a
    v = fromMaybe (replicate (length a) 0) (ciVec (criticalInfo a lam))
    t = lam + s
    plan = [ map (+ fromIntegral k * t) v | k <- [0 .. kk] ]
    bump k x = if k == kd then [ if i == who then u + d else u | (i, u) <- zip [0 ..] x ] else x   -- この列車がこの周期に d 分遅れる
    xs = reverse (foldl' (\acc (k, pk) -> case acc of
                            [] -> [bump k pk]
                            (prev : _) -> bump k (zipWith mpAdd (mpMatVec a prev) pk) : acc) [] (zip [0 ..] plan))
    z = zipWith (zipWith (\u p -> max 0 (u - p))) xs plan
    lastK = foldl' (\l (k, r) -> if any (> 1e-9) r then k else l) (-1) (zip [0 ..] z)
    recover = if lastK < 0 then 0 else lastK - kd + 1

run :: World -> World
run w = w { wSim = sim { sCrit = ciNodes (criticalInfo a (sLam sim)) } }
  where a = nA (netOf w)
        sim = propagate a (fromIntegral (wSlack w)) (fromIntegral (wD w)) (fst (wAt w)) (snd (wAt w)) kN

cellW :: Env -> Float
cellW env = (envW env - 220) / fromIntegral (kN + 1)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 遅れの表（行 = 列車、列 = 周期 k）
  concat [ [ sayR (nm ++ (if i `elem` sCrit sim then " *" else "")) (gx - 10) (gy + fromIntegral i * ch + ch / 2 + 5) 15 (grayC 220) ]
           ++ concat [ let u = vix (rowAt z k) i
                           t = realToFrac (clampv 0 1 (u / max 1 (fromIntegral (wD w)))) :: Float
                           src = (i, k) == wAt w
                           x = gx + fromIntegral k * cw
                           y = gy + fromIntegral i * ch
                       in [ Color (if u > 1e-9 then makeColor ((60 + 195 * t) / 255) ((60 + 110 * t) / 255) ((70 - 30 * t) / 255) 1 else grayC 30) (rectTL x y cw ch)
                          , Color (if src then grayC 255 else grayC 45) (frameTL (if src then 2 else 1) x y cw ch) ]
                          ++ [ sayM (mpFmt 1 u) (x + cw / 2) (y + ch / 2) 14 (if t > 0.5 then grayC 20 else grayC 230) | u > 1e-9 ]
                     | k <- [0 .. kN] ]
         | (i, nm) <- zip [0 ..] (nNames net) ]
  ++ [ sayC ("k=" ++ show k) (gx + (fromIntegral k + 0.5) * cw) (gy + fromIntegral n * ch + 18) 14 (grayC 140) | k <- [0, 2 .. kN] ]
  -- ---- 遅れの合計と最大のグラフ
  ++ [ plotAxes g "period k" "total delay (orange) and largest (blue)" [0, 5, 10, 15, 20, 25] [0, jsRound (y1 g)]
     , Color (rgbC 255 170 70) (polyline 2.5 [ (gX g (fromIntegral k), gY g u) | (k, u) <- zip [0 :: Int ..] tot ])
     , Color (rgbC 110 190 255) (polyline 2 [ (gX g (fromIntegral k), gY g u) | (k, u) <- zip [0 :: Int ..] mx ]) ]
  ++ [ Color (rgbaC 255 255 255 90) (dashPath 1 [ (gX g (fromIntegral k), gY g (if k < snd (wAt w) then 0 else max 0 (fromIntegral (wD w) - fromIntegral ((k - snd (wAt w)) * wSlack w))))
                                               | k <- [0 .. kN] ])
     | wSlack w > 0 ]
  -- ---- 文字
  ++ [ say ("Delay propagation: " ++ nName net ++ "   lambda = " ++ mpFmt 2 (sLam sim) ++ " min, period T = lambda + s = " ++ mpFmt 2 (sT sim) ++ " min") 40 40 20 (grayC 240)
     , say ("Click a cell = where the delay starts   <- -> delay d = " ++ show (wD w) ++ " min   Up/Down slack s = " ++ show (wSlack w) ++ " min   1 2 network   R clear delay") 40 66 15 (grayC 170)
     , say (if sLast sim >= kN then "Never recovers! (slack s = 0 and a delay on the critical circuit -> it goes on forever)"
            else if wD w == 0 then "No delay: on schedule"
            else "The delay vanished after " ++ show (sRecover sim) ++ " periods (bound ceil(d/s) = "
                 ++ (if wSlack w > 0 then show (ceiling (fromIntegral (wD w) / fromIntegral (wSlack w) :: Double) :: Int) else "inf")
                 ++ ")   delayed departures: " ++ show affected) 40 y0 17 (rgbC 255 220 120)
     , sayWrap "* = trains on the critical circuit (without slack there the delay keeps circling). Dotted = bound d - (k - k_d) s. More slack recovers faster but makes T longer (fewer trains)" 40 (y0 + 8) (envW env - 280) 14 (grayC 190)
     , drawSelfTest env (wTest w) ]
  where
    net = netOf w
    sim = wSim w
    n = length (nA net)
    z = sZ sim
    cw = cellW env
    ch = 40
    gx = 160
    gy = 120
    rowAt xs k = case drop k xs of { (r : _) -> r; [] -> [] }
    tot = map sum z
    mx = map (foldl' max negInf) z
    g = G gx (gy + fromIntegral n * ch + 70) (fromIntegral (kN + 1) * cw) (envH env - (gy + fromIntegral n * ch + 70) - 150) 0 (fromIntegral kN) 0 (foldl' max 1 tot * 1.1)
    y0 = envH env - 70
    affected = length (filter (> 1e-9) (concat z))

-- 点線（4 ピクセルずつ）
dashPath :: Float -> [(Float, Float)] -> Picture
dashPath wt pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (xa, ya) (xb, yb) =
          let l = sqrt ((xb - xa) ^ (2 :: Int) + (yb - ya) ^ (2 :: Int))
              m = floor (l / 8) :: Int
              at t = (xa + (xb - xa) * t, ya + (yb - ya) * t)
          in if l < 1e-6 then [] else [ thickLine wt (at (fromIntegral q * 8 / l)) (at (min 1 ((fromIntegral q * 8 + 4) / l))) | q <- [0 .. m] ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        i = floor ((my - 120) / 40) :: Int
        k = floor ((mx - 160) / cellW env) :: Int
    in if i >= 0 && i < length (nA (netOf w)) && k >= 0 && k <= kN then run w { wAt = (i, k) } else w
  EventKey (SpecialKey KeyRight) Down _ _ -> run w { wD = min 90 (wD w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> run w { wD = max 0 (wD w - 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> run w { wSlack = min 15 (wSlack w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> run w { wSlack = max 0 (wSlack w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "12" -> run w { wNet = if c == '1' then 0 else 1, wAt = (1, 2) }
    | c `elem` "rR" -> run w { wD = 0 }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 10・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("both networks are strongly connected (irreducible)", all (isIrreducible . nA) nets)
  , ("with no delay the actual departures = the plan (the plan satisfies A (x) x(k) <= x(k+1))", all (\(a, _, _, _, _) -> a) rs)
  , ("delays z(k) >= 0, no NaN", all (\(_, a, _, _, _) -> a) rs)
  , ("the largest delay drops by at least s each period -> recovery within ceil(d/s) periods (40 random cases)", all (\(_, _, a, _, _) -> a) rs)
  , ("larger d never lowers any delay (monotone); more slack recovers as fast or faster", all (\(_, _, _, a, b) -> a && b) rs)
  , ("slack s = 0, 15 min delay on a critical train -> after 60 periods the largest delay is still 15 min"
    , all (\r -> abs (foldl' max negInf r - 15) < 1e-9) (drop 1 (sZ q))) ]
  where
    a0 = nA (head nets)
    q = propagate a0 0 15 (headOr 0 (ciNodes (criticalInfo a0 (karp a0)))) 1 60
    rs = withSeed 10 (mapM one [0 .. 39 :: Int])
    one t = do
      let n = 3 + t `mod` 4
      a <- randomIrreducible n 0.35 20 60
      s <- fromIntegral <$> rndInt 1 8
      d <- fromIntegral <$> rndInt 5 60
      who <- rndInt 0 (n - 1)
      let p0 = propagate a s 0 who 1 30
          okPlan = all (== 0) (concat (sZ p0)) && noNaN (concat (sX p0))
          p = propagate a s d who 1 40
          okPos = all (>= 0) (concat (sZ p)) && noNaN (concat (sZ p))
          mxz = map (foldl' max negInf) (sZ p)
          okBound = sRecover p <= ceiling (d / s) && and [ m1 <= max 0 (m0 - s) + 1e-9 | (k, m0, m1) <- zip3 [1 :: Int ..] mxz (drop 1 mxz), k >= 2 ]
          p2 = propagate a s (d + 10) who 1 40
          okMono = and (zipWith (\r2 r1 -> and (zipWith (\u v -> u >= v - 1e-9) r2 r1)) (sZ p2) (sZ p))
          p3 = propagate a (s + 3) d who 1 40
          okSlack = sRecover p3 <= sRecover p
      pure (okPlan, okPos, okBound, okMono, okSlack)

-- ---------- やってみよう ----------
-- 1. ↑↓ で余裕 s を 0 → 1 → 5 と変えて、遅れが消えるまでの周期を比べよう。⌈d/s⌉ と比べると？
-- 2. *（臨界回路）の列車と、* でない列車に同じ遅れを入れて比べよう。どちらが長引く？
-- 3. 2 の路線網（ハブ駅）では、遅れはいくつの列車に広がる？
-- 4. 余裕を大きくすると回復は速いが、1 時間あたりの本数（60 / T）は減る。ちょうどよい s は？
-- 5. 実際の鉄道の時刻表づくり（オランダなど）でも、このマックスプラスの計算で余裕の置き場所を決めている

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
