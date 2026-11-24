-- ============================================
-- 65-10 連続時間マルコフ連鎖 ─ 1 日ごとではなく、好きな時刻に状態が変わる。例：機械の 稼働・点検・故障・修理
-- 状態 i にいる時間は指数分布（平均 1 / q_i）。時間がきたら、率 q_ij に比例する確率で次の状態 j へ。
-- 率をまとめた生成行列 Q（対角 = −q_i、各行の合計 = 0）。時刻 t の分布 p(t) = p(0) e^{Qt} は、
-- 一様化：P~ = I + Q/Λ を「ポアソン回数」だけかける式で正確に計算できる。定常分布は π Q = 0（待ち行列の出生死滅過程は series29/05）
-- 操作：↑↓ 故障の率 / ← → 修理の率 / R 全部を稼働から / スペース 一時停止 / F 速さ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

stateNames :: [String]
stateNames = ["Run", "Check", "Fail", "Fix"]   -- 稼働・点検・故障・修理

inspect, back, startFix :: Double
inspect = 0.3                                 -- ★ 稼働 → 点検 の率（1 日あたり）
back = 2.0                                    -- ★ 点検 → 稼働
startFix = 1.0                                -- ★ 故障 → 修理

nM :: Int
nM = 400                                      -- ★ 機械の台数

tMax :: Double
tMax = 30                                     -- ★ グラフの長さ（日）

-- 機械 1 台：いまの状態、入った時刻、次に変わる時刻、点の位置のずれ
data Machine = Machine { mS :: !Int, mEnter :: !Double, mNext :: !Double, mOx :: !Double, mOy :: !Double }

data World = World
  { wFail    :: !Double                       -- ★ 稼働 → 故障 の率
  , wRepair  :: !Double                       -- ★ 修理 → 稼働 の率
  , wQ       :: Mat                           -- 生成行列
  , wPi      :: Vec                           -- 定常分布
  , wCurves  :: [Vec]                         -- 一様化で計算した p(t)（201 点）
  , wFleet   :: [Machine]
  , wSimT    :: !Double
  , wPaused  :: !Bool
  , wFast    :: !Bool
  , wLog     :: [(Double, [Double])]          -- 400 台の割合の記録（新しい順）
  , wHolds   :: [Double]                      -- 稼働にとどまった時間（最大 4000）
  , wNHolds  :: !Int
  , wGantt   :: [(Int, Double)]               -- 1 台めの記録（状態, 入った時刻）、新しい順
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-10 Continuous-time Markov chain" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPaused w then w else advance (if wFast w then 0.25 else 0.05) w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = rebuild (World 0.2 0.5 [] [] [] [] 0 False False [] [] 0 [] (rngOf 10) [])

-- 生成行列：非対角 = 率、対角 = −（行のほかの合計）
makeQ :: Double -> Double -> Mat
makeQ f r =
  let q0 = [ [0, inspect, f, 0], [back, 0, 0, 0], [0, 0, 0, startFix], [r, 0, 0, 0] ]
  in [ [ if i == j then negate (sumOf row - at' row i 0) else v | (j, v) <- zip [0 ..] row ] | (i, row) <- zip [0 :: Int ..] q0 ]

unifRate :: Mat -> Double
unifRate q = let m = foldl' max 0 [ negate (at' r i 0) | (i, r) <- zip [0 ..] q ] in if m == 0 then 1 else m

unifP :: Mat -> Mat                           -- P~ = I + Q/Λ
unifP q = let lam = unifRate q in [ [ (if i == j then 1 else 0) + v / lam | (j, v) <- zip [0 :: Int ..] r ] | (i, r) <- zip [0 ..] q ]

-- ★ 一様化：p(t) = Σ_n e^{−Λt} (Λt)^n / n! · p0 P~^n（ポアソンの重みの合計が 1 − 10⁻¹⁵ になるまで）
uniformDist :: Vec -> Mat -> Double -> Vec
uniformDist p0 q t = go 1 p0 w0 (map (* w0) p0) w0
  where
    lam = unifRate q
    pt = unifP q
    w0 = exp (negate lam * t)
    go :: Int -> Vec -> Double -> Vec -> Double -> Vec
    go n v w out acc
      | n < 2000 && acc < 1 - 1e-15 =
          let v' = vecMat v pt
              w' = w * (lam * t) / i2d n
          in go (n + 1) v' w' (forceL (zipWith (\o x -> o + w' * x) out v')) (acc + w')
      | otherwise = out

stationaryCtmc :: Mat -> Vec
stationaryCtmc q = maybe (replicate (length q) (1 / i2d (length q))) id (stationaryExact (unifP q))

holdTime :: Mat -> Int -> R Double            -- 指数分布の待ち時間
holdTime q s = do u <- rnd; pure (negate (log (1 - u)) / negate (at' (at' q s []) s 0))

jumpFrom :: Mat -> Int -> R Int               -- 率に比例して次へ
jumpFrom q s = sampleFrom [ if j == s then 0 else v | (j, v) <- zip [0 ..] (at' q s []) ]

rebuild :: World -> World
rebuild w =
  let q = makeQ (wFail w) (wRepair w)
  in restart w { wQ = q, wPi = stationaryCtmc q, wCurves = [ uniformDist [1, 0, 0, 0] q (i2d k / 200 * tMax) | k <- [0 .. 200 :: Int] ] }

restart :: World -> World
restart w =
  let q = wQ w
      mk = do
        nx <- holdTime q 0
        ox <- randn
        oy <- randn
        pure (Machine 0 0 nx (ox * 0.35) (oy * 0.35))
      (fl, g) = runR (mapM (const mk) [1 .. nM]) (wRng w)
  in w { wSimT = 0, wLog = [], wHolds = [], wNHolds = 0, wGantt = [(0, 0)], wFleet = fl, wRng = g }

-- 全部の機械を時刻 simT + dt まで進める（機械の順に乱数を引く）
advance :: Double -> World -> World
advance dt w =
  let t1 = wSimT w + dt
      q = wQ w
      oneM (m, f) (hs, nh, ga, acc) = go f hs nh ga
        where
          go !f0 hs0 !nh0 ga0
            | mNext f0 <= t1 = do
                let (hs1, nh1) = if mS f0 == 0 && nh0 < 4000 then ((mNext f0 - mEnter f0) : hs0, nh0 + 1) else (hs0, nh0)
                s' <- jumpFrom q (mS f0)
                h <- holdTime q s'
                let f1 = f0 { mS = s', mEnter = mNext f0, mNext = mNext f0 + h }
                    ga1 = if m == (0 :: Int) && mEnter f1 <= tMax then (s', mEnter f1) : ga0 else ga0
                go f1 hs1 nh1 ga1
            | otherwise = pure (hs0, nh0, ga0, f0 : acc)
      stepAll = foldl' (\r mf -> r >>= oneM mf) (pure (wHolds w, wNHolds w, wGantt w, [])) (zip [0 ..] (wFleet w))
      ((hs', nh', ga', fl'), g) = runR stepAll (wRng w)
      fleet = reverse fl'
      cnt = foldl' (\c f -> modifyAt (mS f) (+ 1) c) [0, 0, 0, 0 :: Int] fleet
      lg = if t1 <= tMax then (t1, map (\v -> i2d v / i2d nM) cnt) : wLog w else wLog w
  in w { wSimT = t1, wFleet = fleet, wHolds = hs', wNHolds = nh', wGantt = ga', wLog = lg, wRng = g }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Continuous-time Markov chain: " ++ show nM ++ " machines   time " ++ fmtN 1 (wSimT w) ++ " days")
             ("Up/Down failure rate " ++ fmtN 2 (wFail w) ++ "   <- -> repair rate " ++ fmtN 2 (wRepair w) ++ "   R restart (all running)   Space pause   F speed (" ++ (if wFast w then "fast" else "normal") ++ ")")
  -- ---- 遷移図（太さ = 率）と機械の点
  , graphPic
  , Pictures [ sayM (fmtN 2 (at' (at' q (hI h) []) (hJ h) 0)) (hX h) (hY h - 12) 14 (col cream) | h <- hits ]
  , Pictures [ Color (colA 200 (stateCol (mS f))) (circleAt (nx + (ux / l) * 68 + f2 (mOx f) * 26) (ny + (uy / l) * 66 + f2 (mOy f) * 20) 4)
             | f <- wFleet w, let (nx, ny) = at' nodes (mS f) (0, 0), let ux = nx - gcx, let uy = ny - gcy, let l = let l0 = hyp ux uy in if l0 == 0 then 1 else l0 ]
  -- 生成行列
  , say "Generator matrix Q (row sums = 0)" qx (qy - 26) 14 (col cream)
  , drawMatrix q (qx + 60) qy 62 24 (Just stateNames) 14 2 2.3
  , say ("pi (pi Q = 0): " ++ unwords [ nm ++ " " ++ fmtN 3 v | (nm, v) <- zip stateNames (wPi w) ]) qx (qy + 4 * 24 + 22) 14 (col sub)
  , say ("Running share = " ++ show (jsRoundI (at' (wPi w) 0 0 * 100)) ++ "%") qx (qy + 4 * 24 + 42) 14 (col sub)
  -- ---- 時刻 t の分布：一様化の計算（線）と 400 台の割合（点）
  , plotAxes g ("time t (days)") ("p_i(t)   line = uniformization, dots = " ++ show nM ++ " machines") [0, 10, 20, 30] [0, 0.5, 1]
  , Pictures [ Pictures [ Color (col (stateCol i)) (polyline 2 [ (gX g (i2d k / 200 * tMax), gY g (at' p i 0)) | (k, p) <- zip [0 :: Int ..] (wCurves w) ])
                        , Color (colA 90 (stateCol i)) (lineW 1 (grX g, gY g (at' (wPi w) i 0)) (grX g + grW g, gY g (at' (wPi w) i 0)))
                        , Color (col (stateCol i)) (Pictures [ circleAt (gX g t) (gY g (at' p i 0)) 4 | (k, (t, p)) <- zip [0 :: Int ..] logs, k `mod` 4 == 0 ]) ]
             | i <- [0 .. 3] ]
  , legend (grX g + grW g - 70) (grY g + 14) [ (stateCol i, nm) | (i, nm) <- zip [0 ..] stateNames ]
  -- ---- 1 台めの機械の記録（帯）
  , say "Machine #1 record (colour = state, length = time spent)" (grX g) (by - 6) 14 (col cream)
  , Pictures [ Color (col (stateCol s)) (rectTL (gX g t0) by (gX g t1 - gX g t0) 16)
             | (k, (s, t0)) <- zip [0 :: Int ..] gan
             , let t1 = min tMax (if k + 1 < length gan then snd (at' gan (k + 1) (0, 0)) else wSimT w)
             , t1 > t0 ]
  -- ---- 稼働にとどまった時間のヒストグラムと指数分布
  , plotAxes hg "time spent running (days)" ("Run durations (" ++ show (wNHolds w) ++ ") vs q e^(-qt), mean 1/q = " ++ fmtN 2 (-1 / at' (at' q 0 []) 0 0) ++ " d") [0, 2, 4, 6, 8] []
  , Color (colA 160 (stateCol 0)) (Pictures [ rectTL (gX hg (i2d b * 8 / i2d nb)) (gY hg d) (grW hg / i2f nb - 1) (grY hg + grH hg - gY hg d)
                                            | (b, c) <- zip [0 ..] bins, let d = if wNHolds w > 0 then i2d c / i2d (wNHolds w) / (8 / i2d nb) else 0 ])
  , Color (col cream) (polyline 2 [ (gX hg t, gY hg (q0 * exp (negate q0 * t))) | k <- [0 .. 80 :: Int], let t = i2d k / 80 * 8 ])
  , drawSelfTest env (wTest w) ]
  where
    q = wQ w
    gcx = envW env * 0.2
    gcy = envH env * 0.42
    rad = min (envW env * 0.12) (envH env * 0.17)
    nodes = [(gcx, gcy - rad), (gcx + rad, gcy), (gcx, gcy + rad), (gcx - rad, gcy)]
    mx = maxOr 1 [ v | (i, r) <- zip [0 :: Int ..] q, (j, v) <- zip [0 ..] r, i /= j ]
    wide = [ [ if i == j then 0 else v / mx | (j, v) <- zip [0 :: Int ..] r ] | (i, r) <- zip [0 ..] q ]
    (graphPic, hits) = drawChainGraph nodes wide 40 stateCol defOpts { goNames = Just stateNames, goLabels = False, goNameSize = 14 }
    qx = 40
    qy = gcy + rad + 128
    g = Graph (envW env * 0.47) 110 (envW env * 0.49) (envH env * 0.36) 0 tMax 0 1
    logs = reverse (wLog w)
    by = grY g + grH g + 56
    gan = reverse (wGantt w)
    hg = Graph (grX g) (by + 60) (grW g) (envH env - by - 130) 0 8 0 (negate (at' (at' q 0 []) 0 0) * 1.1)
    nb = 32 :: Int
    bins = foldl' (\bs h -> let b = floor (h / 8 * i2d nb) in if b < nb then modifyAt b (+ (1 :: Int)) bs else bs) (replicate nb 0) (wHolds w)
    q0 = negate (at' (at' q 0 []) 0 0)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> w { wFast = not (wFast w) }
    | c == 'r' || c == 'R' -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wFail = min 2 (round2 (wFail w + 0.05)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wFail = max 0.05 (round2 (wFail w - 0.05)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wRepair = min 3 (round1 (wRepair w + 0.1)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wRepair = max 0.1 (round1 (wRepair w - 0.1)) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 修理の率を 2 倍にすると、稼働している割合 π_稼働 はどれだけ上がる？
-- 2. 点の割合（シミュレーション）は線（計算）にどのくらい近い？ nM を 4000 にすると？
-- 3. ヒストグラムが指数分布になるのは「いままで何日動いたかに関係なく、壊れやすさは同じ」だから（無記憶性）
-- 4. 状態を 1 つ増やして（例：部品待ち）、生成行列の行の合計が 0 になるように率を書こう

-- ---- 自己テスト
-- 行列の指数関数：2^s で割ってテイラー展開 30 項、s 回 2 乗する（s = ⌈log₂(‖A‖ / 0.25)⌉ を 2 のべきで正確に）
expmTaylor :: Mat -> Mat
expmTaylor a =
  let n = length a
      nm = foldl' max 0 [ sumOf (map abs r) | r <- a ]
      s = head [ k | k <- [0 :: Int ..], 2 ^^ k >= nm / 0.25 ]
      b = map (map (/ (2 ^^ s))) a
      (_, out) = foldl' (\(term, o) k -> let t' = map (map (/ i2d k)) (matMul term b) in (t', zipWith (zipWith (+)) o t')) (eyeMat n, eyeMat n) [1 .. 30 :: Int]
  in iterate (\o -> matMul o o) out !! s

selfTest :: [(String, Bool)]
selfTest =
  [ ("生成行列：各行の合計 = 0、対角の外は ≥ 0", rowSumErr q 0 < 1e-12 && and [ i == j || v >= 0 | (i, r) <- zip [0 :: Int ..] q, (j, v) <- zip [0 ..] r ])
  , ("一様化 = 行列の指数関数 e^{Qt}（テイラー + 2 乗、t = 0.5, 3, 10）ずれ " ++ fmtE worst, worst < 1e-9)
  , ("定常分布：π Q = 0、Σ π = 1", maxAbsDiffV piQ [0, 0, 0, 0] < 1e-12 && near (sumOf pv) 1 1e-12)
  , ("p(200) → π（どこから始めても）", maxAbsDiffV (uniformDist [0, 0, 1, 0] q 200) pv < 1e-9)
  , ("π_i ∝ ν_i × 平均の滞在時間（飛び移りの連鎖から）", maxAbsDiffV (map (/ sw) wv) pv < 1e-12)
  , ("1 台を 10 万日動かした時間の割合 ≈ π（ずれ < 0.01）", maxAbsDiffV (map (/ tEnd) occ) pv < 0.01)
  , ("稼働にとどまる時間の平均 ≈ 1/q_0 = " ++ fmtN 2 (1 / 0.5) ++ " 日（ずれ < 3%）", abs (hsMean * 0.5 - 1) < 0.03)
  , ("4000 台の t = 2 の割合 ≈ 一様化の p(2)（ずれ < 0.03）", maxAbsDiffV (map (\v -> i2d v / i2d nF) cnt) (uniformDist [1, 0, 0, 0] q 2) < 0.03) ]
  where
    q = makeQ 0.2 0.5
    worst = foldl' maxN 0 [ maxAbsDiffV (uniformDist [if k == i then 1 else 0 | k <- [0 .. 3]] q t) (at' e i [])
                          | t <- [0.5, 3, 10], let e = expmTaylor (map (map (* t)) q), i <- [0 .. 3 :: Int] ]
    pv = stationaryCtmc q
    piQ = vecMat pv q
    jm = [ [ if i == j then 0 else v / negate (at' r i 0) | (j, v) <- zip [0 :: Int ..] r ] | (i, r) <- zip [0 ..] q ]
    nu = maybe [] id (stationaryExact jm)
    wv = [ v / negate (at' (at' q i []) i 0) | (i, v) <- zip [0 ..] nu ]
    sw = sumOf wv
    tEnd = 100000 :: Double
    nF = 4000 :: Int
    ((occ, hsMean), cnt) = withSeed 100 $ do
      (o, hsS, hsN) <- simOne 0 0 [0, 0, 0, 0] 0 0
      c <- loopR nF (\cs -> do
              h0 <- holdTime q 0
              st <- fleetTo 0 h0
              pure (modifyAt st (+ 1) cs)) [0, 0, 0, 0 :: Int]
      pure ((o, hsS / i2d hsN), c)
    simOne :: Int -> Double -> [Double] -> Double -> Int -> R ([Double], Double, Int)
    simOne s t o !hsS !hsN
      | t < tEnd = do
          h <- holdTime q s
          let o' = forceL (modifyAt s (+ min h (tEnd - t)) o)
              (hsS', hsN') = if s == 0 then (hsS + h, hsN + 1) else (hsS, hsN)
          s' <- jumpFrom q s
          simOne s' (t + h) o' hsS' hsN'
      | otherwise = pure (o, hsS, hsN)
    fleetTo st tt
      | tt <= 2 = do
          st' <- jumpFrom q st
          h <- holdTime q st'
          fleetTo st' (tt + h)
      | otherwise = pure st

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- 遷移図を描く道具：矢印の太さ = 確率
-- 少しふくらんだ曲線の点（両はしの円 r の中は除く）
bezPts :: (Float, Float) -> (Float, Float) -> Float -> Float -> [(Float, Float)]
bezPts (ax, ay) (bx, by) bend r =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      dx = bx - ax
      dy = by - ay
      l0 = sqrt (dx * dx + dy * dy)
      l = if l0 == 0 then 1 else l0
      cx = mx - (dy / l) * bend * l
      cy = my + (dx / l) * bend * l
  in [ (x, y) | k <- [0 .. 30 :: Int], let t = i2f k / 30
                                           x = (1 - t) * (1 - t) * ax + 2 * t * (1 - t) * cx + t * t * bx
                                           y = (1 - t) * (1 - t) * ay + 2 * t * (1 - t) * cy + t * t * by
     , hyp (x - ax) (y - ay) > r + 2, hyp (x - bx) (y - by) > r + 6 ]

-- 折れ線の最後に三角の矢じり
arrowHeadAt :: [(Float, Float)] -> Float -> Picture
arrowHeadAt pts sz = case reverse pts of
  ((x2, y2) : (x1, y1) : _) ->
    let l0 = hyp (x2 - x1) (y2 - y1)
        l = if l0 == 0 then 1 else l0
        ux = (x2 - x1) / l
        uy = (y2 - y1) / l
    in Polygon [(x2 + ux * sz, y2 + uy * sz), (x2 - uy * sz * 0.6, y2 + ux * sz * 0.6), (x2 + uy * sz * 0.6, y2 - ux * sz * 0.6)]
  _ -> Blank

-- 矢印のラベルやクリック判定に使う点：i → j の矢印の真ん中 (hx, hy)
data Hit = Hit { hI :: !Int, hJ :: !Int, hX :: !Float, hY :: !Float }

data GraphOpts = GraphOpts { goNames :: Maybe [String], goHl :: !Int, goNameSize :: !Float, goLabels :: !Bool, goAlpha :: Maybe Float }

defOpts :: GraphOpts
defOpts = GraphOpts Nothing (-1) 18 True Nothing

-- nodes = [(x, y), …]、a = 遷移行列、r = 円の半径、cols i = 状態 i の色。戻り値：絵と、矢印ごとの Hit
drawChainGraph :: [(Float, Float)] -> Mat -> Float -> (Int -> RGB) -> GraphOpts -> (Picture, [Hit])
drawChainGraph nodes a r cols opts = (Pictures (edgePics ++ nodePics), hits)
  where
    nn = length nodes
    cx = realToFrac (meanOf (map (realToFrac . fst) nodes)) :: Float
    cy = realToFrac (meanOf (map (realToFrac . snd) nodes)) :: Float
    edges = [ (i, j, p, c, w, pts, h)
            | i <- [0 .. nn - 1], j <- [0 .. nn - 1]
            , let p = at' (at' a i []) j 0
            , p > 1e-9
            , let c = cols i
                  w = 1 + 11 * f2 p
                  (pts, h) = edgeShape i j ]
    hits = [ h | (_, _, _, _, _, _, Just h) <- edges ]
    edgeShape i j
      | i == j = let (nx, ny) = at' nodes i (0, 0)
                     ox = nx - cx
                     oy = ny - cy
                     l0 = hyp ox oy
                     l = if l0 == 0 then 1 else l0
                     ux = if l <= 1 then 0 else ox / l
                     uy = if l > 1 then oy / l else -1
                     lx = nx + ux * r * 1.55
                     ly = ny + uy * r * 1.55
                     rr = r * 0.62
                     base = atan2 (negate uy) (negate ux)
                     pts = [ (lx + rr * cos t, ly + rr * sin t) | k <- [0 .. 32 :: Int], let t = base + 0.9 + (i2f k / 32) * (2 * pi - 1.8) ]
                 in (pts, Just (Hit i j (lx + ux * rr) (ly + uy * rr)))
      | otherwise = let pts = bezPts (at' nodes i (0, 0)) (at' nodes j (0, 0)) 0.14 r
                        (hx, hy) = at' pts (length pts `div` 2) (0, 0)
                    in (pts, if null pts then Nothing else Just (Hit i j hx hy))
    aE = maybe 210 id (goAlpha opts)
    aH = maybe 230 id (goAlpha opts)
    edgePics = [ Pictures [ Color (colA aE c) (polyline w pts), Color (colA aH c) (arrowHeadAt pts (8 + w * 0.8))
                          , case h of
                              Just hh | goLabels opts -> sayM (fmtN 2 p) (hX hh) (hY hh - 12) 14 (col cream)
                              _ -> Blank ]
               | (_, _, p, c, w, pts, h) <- edges ]
    nodePics = [ Pictures [ Color (col cream) (circleAt x y (2 * r + sw)), Color (col (scaleRGB 0.55 c)) (circleAt x y (2 * r - sw))
                          , sayM (maybe (show i) (\ns -> at' ns i (show i)) (goNames opts)) x y (goNameSize opts) (col cream) ]
               | (i, (x, y)) <- zip [0 ..] nodes, let c = cols i, let sw = if goHl opts == i then 4 else 1.5 ]

-- 行列を色つきの表で（濃い = 大きい）。左上 (x, y)、マスの大きさ cw × ch、d = 小数の桁、hi = いちばん濃い値
drawMatrix :: Mat -> Float -> Float -> Float -> Float -> Maybe [String] -> Float -> Int -> Double -> Picture
drawMatrix a x y cw ch names ts d hi = Pictures $
  [ Pictures [ Color (col (48 + t * (tr - 48), 42 + t * (tg - 42), 40 + t * (tb - 40))) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
             , sayM (fmtN d v) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col cream) ]
  | (i, row) <- zip [0 ..] a, (j, v) <- zip [0 ..] row, let t = f2 (clampv (abs v / hi) 0 1) ]
  ++ case names of
       Nothing -> []
       Just ns -> [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col sub) | (i, nm) <- zip [0 .. length a - 1] ns ]
                  ++ [ sayC nm (x + i2f j * cw + cw / 2) (y - 5) ts (col sub) | (j, nm) <- zip [0 .. nCols a - 1] ns ]
  where (tr, tg, tb) = teal

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
