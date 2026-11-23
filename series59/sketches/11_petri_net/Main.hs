-- ============================================
-- 59-11 時間付きペトリネット（イベントグラフ） ─ □ トランジション（できごと）、○ プレース（条件）、● トークン（そろった条件）
-- 入ってくるプレース全部に「保持時間 h を過ぎたトークン」があると発火し、出ていくプレースにトークンを 1 つずつ置く
--   プレース j → i（トークン m 個、保持 h）は「x_i(k) ≥ x_j(k − m) + h」→ x(k) = A0⊗x(k) ⊕ A1⊗x(k−1) ⊕ A2⊗x(k−2)
--   トークンを 1 つずつ動かすシミュレーションと、マックスプラスの漸化式の発火時刻がぴったり一致する
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps と書く
-- 操作：プレース ○ をクリックで選ぶ / ↑↓ 保持時間 / ← → トークンの数（0〜2）/ スペース 一時停止 / R 時刻を 0 に
--       （スペース以外のキーを押すと、時刻は 0 にもどる）
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

trNames :: [String]
trNames = ["T0 input", "T1 M1->M2", "T2 M2 done", "T3 output"]

-- プレース：from → to、保持時間 h（分）、はじめのトークン m、説明
data Place = Place { plFrom, plTo :: !Int, plH :: !Double, plM :: !Int, plNote :: String }

places0 :: [Place]                 -- ★ プレース
places0 =
  [ Place 0 1 4 0 "machine 1 works", Place 1 0 0 1 "machine 1 is free"
  , Place 1 2 5 0 "machine 2 works", Place 2 1 0 1 "machine 2 is free"
  , Place 2 3 2 0 "transport", Place 3 0 3 2 "pallet returns", Place 3 3 1 1 "output robot" ]

nT, kFire :: Int
nT = 4
kFire = 40

data World = World
  { wPl     :: [Place]
  , wSel    :: !Int
  , wNow    :: !Double
  , wPaused :: !Bool
  , wRec    :: Maybe [Vec]        -- 漸化式の発火時刻 x(1) … x(K)（デッドロックなら Nothing）
  , wSim    :: [[Double]]         -- シミュレーションの発火時刻 fires[t][k−1]
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-11 Timed Petri net" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = recompute (World places0 2 0 False Nothing [] [])

-- ★ ネットから行列 A0, A1, A2（a_ij = プレース j → i の保持時間、トークン m 個なら A_m）
netMatrices :: [Place] -> [Mat]
netMatrices pl = [ foldl' (\a p -> if plM p == m then setM (plTo p) (plFrom p) (mpAdd (ix a (plTo p) (plFrom p)) (plH p)) a else a) (mpMat nT nT negInf) pl
                 | m <- [0 .. 2] ]

-- ★ マックスプラスの漸化式：x(k) = A0* ⊗ (A1 ⊗ x(k−1) ⊕ A2 ⊗ x(k−2))、k ≤ 0 は 0（はじめのトークンは時刻 0 に置いてある）
recursion :: [Place] -> Int -> Maybe [Vec]
recursion pl kk
  | any (not . isEps) (concat (mpPow a0 nT)) = Nothing         -- トークン 0 の回路 = デッドロック
  | otherwise = Just (go kk zeros zeros)
  where
    (a0, a1, a2) = case netMatrices pl of { [x, y, z] -> (x, y, z); _ -> ([], [], []) }
    st = fst (mpStar 1e-9 a0)
    zeros = replicate nT 0
    go 0 _ _ = []
    go k x1 x2 = let x = mpMatVec st (zipWith mpAdd (mpMatVec a1 x1) (mpMatVec a2 x2)) in x : go (k - 1) x x1

-- ★ トークンを 1 つずつ動かすシミュレーション（いちばん早く発火できるトランジションから順に）
tokenGame :: [Place] -> Int -> [[Double]]
tokenGame pl kk = map reverse (loop (0 :: Int) [ replicate (plM p) 0 | p <- pl ] (replicate nT []))
  where
    ipl = zip [0 ..] pl
    loop guard q fires
      | guard >= nT * kk * 4 = fires
      | otherwise =
          let cands = [ (e, t) | t <- [0 .. nT - 1], length (fires !! t) < kk
                               , let ins = [ i | (i, p) <- ipl, plTo p == t ]
                               , all (\i -> not (null (q !! i))) ins
                               , let e = foldl' max 0 [ headOr 0 (q !! i) + plH p | (i, p) <- ipl, plTo p == t ] ]
              -- e がいちばん小さいもの（同じなら番号の小さいもの）
              pick = foldl' (\b c -> case b of { Nothing -> Just c; Just (be, _) -> if fst c < be then Just c else b }) Nothing cands
          in case pick of
               Nothing -> fires
               Just (bt, best) ->
                 let q1 = [ if plTo p == best then drop 1 qi else qi | (qi, p) <- zip q pl ]
                     q2 = [ if plFrom p == best then qi ++ [bt] else qi | (qi, p) <- zip q1 pl ]
                     fires' = [ if t == best then bt : f else f | (t, f) <- zip [0 ..] fires ]
                 in loop (guard + 1) q2 fires'

-- 回路の (保持時間の和) / (トークンの和) の最大 = 周期 λ
cycleRatio :: [Place] -> Double
cycleRatio pl = foldl' (\b s -> dfs s s [s] 0 0 b) negInf [0 .. nT - 1]
  where
    dfs s v used w m best0 = foldl' visit best0 [ p | p <- pl, plFrom p == v ]
      where visit best p
              | plTo p == s = if m + plM p > 0 then max best ((w + plH p) / fromIntegral (m + plM p)) else best
              | plTo p > s && plTo p `notElem` used = dfs s (plTo p) (used ++ [plTo p]) (w + plH p) (m + plM p) best
              | otherwise = best

recompute :: World -> World
recompute w = w { wRec = recursion (wPl w) kFire, wSim = tokenGame (wPl w) kFire, wNow = 0 }

step :: World -> World
step w = case wRec w of
  Nothing -> w
  Just r -> let t = if wPaused w then wNow w else wNow w + 0.05
                tEnd = vix (lastOr [] r) 0
            in w { wNow = if t > tEnd then 0 else t }

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

tpos :: Env -> [(Float, Float)]
tpos env = [ (x * envW env, y * envH env) | (x, y) <- [(0.12, 0.42), (0.32, 0.25), (0.52, 0.42), (0.32, 0.62)] ]

placePos :: Env -> [Place] -> Int -> (Float, Float)
placePos env pl pi' = case drop pi' pl of
  [] -> (0, 0)
  (p : _) ->
    let tp = tpos env
        (ax, ay) = tp !! plFrom p
        (bx, by) = tp !! plTo p
        dx = bx - ax
        dy = by - ay
        l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
        twin = if any (\q -> plFrom q == plTo p && plTo q == plFrom p) pl then 34 else 0
    in if plFrom p == plTo p then (ax + 70, ay + 40) else ((ax + bx) / 2 - (dy / l) * twin, (ay + by) / 2 + (dx / l) * twin)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- プレースと矢印
  concat [ let (ax, ay) = tp !! plFrom p
               (bx, by) = tp !! plTo p
               (cx, cy) = placePos env pl pi'
               act = pi' == wSel w
               lc = if act then rgbC 255 220 120 else grayC 120
               lw = if act then 2.5 else 1.5
               dep = replicate (plM p) 0 ++ map (`vix` plFrom p) recL
               con = map (`vix` plTo p) recL
               here = [ tNow - d0 >= plH p | (d0, c0) <- zip dep con, d0 <= tNow, tNow < c0 ]
           in [ Color lc (if plFrom p == plTo p then Pictures [lineW lw (ax + 11, ay - 10) (cx, cy), lineW lw (cx, cy) (ax + 11, ay + 20)]
                          else Pictures [lineW lw (ax, ay) (cx, cy), lineW lw (cx, cy) (bx, by)])
              , Color (grayC 34) (circleAt cx cy 46)
              , Color (if act then rgbC 255 220 120 else grayC 200) (ringAt cx cy 46 1.5) ]
              ++ [ Color (if ready then rgbC 255 170 70 else rgbC 110 190 255) (circleAt (cx - 12 + fromIntegral (q `mod` 3) * 12) (cy - 6 + fromIntegral (q `div` 3) * 12) 9)
                 | (q, ready) <- zip [0 :: Int ..] (take 6 here) ]
              ++ [ sayC ("h=" ++ mpFmt 0 (plH p) ++ " m=" ++ show (plM p)) cx (cy + 40) 14 (grayC 200) ]
         | (pi', p) <- zip [0 ..] pl ]
  ++ concat [ [ Color (if fired then rgbC 255 220 120 else rgbC 60 90 140) (rect x y 22 64)
              , Color (grayC 220) (frameTL 2 (x - 11) (y - 32) 22 64)
              , sayC nm x (y - 38) 14 (grayC 230) ]
            | (t, (x, y), nm) <- zip3 [0 ..] tp trNames, let fired = any (\x' -> abs (vix x' t - tNow) < 0.15) recL ]
  -- ---- 発火時刻の表：漸化式 / トークンのシミュレーション
  ++ [ say "Firing times x_i(k) (recursion / simulation)" tx (ty - 16) 15 (grayC 220) ]
  ++ [ say ("T" ++ show t) (tx + 50 + fromIntegral t * 105) (ty + 12) 14 (grayC 200) | t <- [0 .. nT - 1] ]
  ++ concat [ say ("k=" ++ show k) tx (ty + 12 + fromIntegral k * 22) 14 (grayC 150)
              : [ let r = vix (rowOr k) t
                      s = case drop (k - 1) (sim !! t) of { (v : _) -> Just v; [] -> Nothing }
                  in say (case rec of
                            Nothing -> "-"
                            Just _ -> mpF r ++ "/" ++ maybe "-" mpF s) (tx + 50 + fromIntegral t * 105) (ty + 12 + fromIntegral k * 22) 14
                         (if rec /= Nothing && Just r == s then rgbC 160 220 180 else rgbC 255 130 120)
                | t <- [0 .. nT - 1] ]
            | k <- [1 .. 10] ]
  ++ [ sayWrap (case rec of
                  Nothing -> "Deadlock: a circuit with 0 tokens (nobody can fire)"
                  Just _ -> if ok then "OK  all " ++ show kFire ++ " firings agree" else "NO  they disagree") tx (y0 - 17) (envW env - tx - 30) 17
               (if ok then rgbC 120 210 150 else rgbC 255 120 110) ]
  ++ [ sayWrap ("Period lambda = max(sum of holding times / tokens over circuits) = " ++ mpFmt 3 lam ++ " min  (slope of the simulation "
                ++ fmt 3 ((vix (rowOr kFire) 0 - vix (rowOr (kFire - 20)) 0) / 20) ++ ")") tx (y0 + 10) (envW env - tx - 30) 15 (grayC 220)
     | rec /= Nothing ]
  ++ [ sayWrap ("Selected place: T" ++ show (plFrom p) ++ " -> T" ++ show (plTo p) ++ " \"" ++ plNote p ++ "\"  h = " ++ mpFmt 0 (plH p) ++ ", m = " ++ show (plM p))
               tx (y0 + 74) (envW env - tx - 30) 15 (rgbC 255 220 120)
     | (p : _) <- [drop (wSel w) pl] ]
  -- ---- タイトル
  ++ [ say ("Timed Petri net   t = " ++ fmt 1 tNow ++ " min" ++ (if wPaused w then " (paused)" else "")) 40 40 20 (grayC 240)
     , say "Click a place  Up/Down holding time  <- -> tokens (0-2)  Space pause  R time 0   dot: orange = usable, blue = holding" 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    pl = wPl w
    tp = tpos env
    rec = wRec w
    recL = maybe [] id rec
    sim = wSim w ++ repeat []
    tNow = wNow w
    rowOr k = case drop (k - 1) recL of { (v : _) | k >= 1 -> v; _ -> [] }
    tx = envW env * 0.62
    ty = 130
    y0 = ty + 280
    ok = rec /= Nothing && and [ length f == kFire && and (zipWith (\v x -> v == vix x t) f recL) | (t, f) <- zip [0 ..] (wSim w) ]
    lam = cycleRatio pl

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hits = [ i | i <- [0 .. length (wPl w) - 1], let (cx, cy) = placePos env (wPl w) i, sqrt ((mx - cx) ^ (2 :: Int) + (my - cy) ^ (2 :: Int)) < 24 ]
    in w { wSel = if null hits then wSel w else last hits }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> recompute (setP (\p -> p { plH = min 12 (plH p + 1) }))
  EventKey (SpecialKey KeyDown) Down _ _ -> recompute (setP (\p -> p { plH = max 0 (plH p - 1) }))
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute (setP (\p -> p { plM = min 2 (plM p + 1) }))
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute (setP (\p -> p { plM = max 0 (plM p - 1) }))
  EventKey (MouseButton _) _ _ _ -> w
  EventKey _ Down _ _ -> recompute w            -- スペース以外のキー：時刻を 0 に（R もここ）
  _ -> w
  where setP f = w { wPl = [ if i == wSel w then f p else p | (i, p) <- zip [0 ..] (wPl w) ] }

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種 11・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("initial net: token simulation = max-plus recursion (40 firings x 4 transitions)", same places0 40)
  , ("random holding times / tokens (" ++ show cnt ++ " live cases): simulation = recursion, no NaN", all (\(a, _, _) -> a) rs && cnt > 10)
  , ("with a 0-token circuit (A0 not nilpotent) the simulation really stops", all (\(_, a, _) -> a) rs)
  , ("long-run firing interval = max over circuits of (sum of holding times)/(tokens)", all (\(_, _, a) -> a) rs)
  , ("initial net: A0 (0-token places) has no circuit, lambda = max(4+0, 5+0, (4+5+2+3)/2, 1) = 7"
    , all isEps (concat (mpPow (headOr [] (netMatrices places0)) nT)) && cycleRatio places0 == 7) ]
  where
    same pl kk = case recursion pl kk of
      Nothing -> False
      Just r -> and [ length f == kk && and (zipWith (\v x -> v == vix x t) f r) | (t, f) <- zip [0 ..] (tokenGame pl kk) ]
    pls = withSeed 11 (mapM (const (mapM (\p -> do h <- rndInt 0 9; m <- rndInt 0 2; pure p { plH = fromIntegral h, plM = m }) places0)) [1 .. 60 :: Int])
    results = [ case recursion pl 60 of
                  Just r -> let lam = cycleRatio pl
                                at k = vix (headOr [] (drop k r)) 0
                                okL = abs ((at 59 - at 39) / 20 - lam) <= 1e-9 || abs ((at 59 - at (59 - 24)) / 24 - lam) <= 1e-9
                            in (True, same pl 60 && noNaN (concat r), True, okL)
                  Nothing -> (False, True, not (all ((== 5) . length) (tokenGame pl 5)), True)
              | pl <- pls ]
    cnt = length (filter (\(live, _, _, _) -> live) results)
    rs = [ (a, b, c) | (_, a, b, c) <- results ]

-- ---------- やってみよう ----------
-- 1. 「パレットが戻る」プレースのトークンを ← → で 1・2 に変えると、周期 λ はどう変わる？
-- 2. 機械 2 の加工時間 h を ↑ で長くしていくと、どこでボトルネックが入れかわる？
-- 3. 「機械 1 が空く」のトークンを 0 にするとデッドロック。なぜ？（どの条件もそろわない回路）
-- 4. 表の 2 つの数（漸化式 ／ シミュレーション）がいつも同じなのはなぜ？
-- 5. 07 の生産ラインをこのペトリネットで描いてみよう（パレット = 戻りのプレースのトークン）

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
