-- ============================================
-- 52-11 等ラウドネス 🔊 ─ 低い音・高い音は、同じ大きさに聞こえるのにどれだけの音量が必要？
-- スペースで「1 kHz の基準音 → 比べる音」を鳴らす。↑↓ で比べる音の音量を 1 dB ずつ変え、同じ大きさに聞こえたら Enter（または N）
-- 125 Hz〜8 kHz の 6 つの音で 2 回ずつ合わせると、自分の「等ラウドネス曲線」ができる（端末やヘッドホンでも形が変わる。形の傾向だけを見る）
-- 曲線に放物線（log 周波数の 2 次式）を当てはめ、いちばん敏感な周波数を求める
-- 操作：スペース = 再生 / ↑↓ = 音量 / Enter（N）= 決定 / O = シミュレーション観察者 / R = やり直し
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

freqs :: [Double]
freqs = [125, 250, 500, 2000, 4000, 8000]   -- ★ 比べる音の周波数 [Hz]

refVol, maxDb :: Double
refVol = 0.2                         -- ★ 基準音（1 kHz）の大きさ（0〜1）
maxDb = 13                           -- 比べる音の上限：refVol × 10^(13/20) ≈ 0.89（これ以上は大きくしない）

passes :: Int
passes = 2                           -- ★ 1 つの周波数を何回合わせるか

-- ★ シミュレーション観察者の「本当の」曲線（作り物）[dB]
simTrue :: Double -> Double
simTrue f = case round f :: Int of
  125 -> 12
  250 -> 7
  500 -> 3
  2000 -> -1
  4000 -> -4
  _ -> 6

data World = World
  { wOrder   :: [Double]           -- 合わせる周波数の順番
  , wTi      :: !Int               -- いま何番目か
  , wLev     :: !Double            -- 比べる音の音量 [dB]（整数）
  , wSets    :: [(Double, Double)] -- 合わせた結果（周波数, dB）。古い順
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wPlaying :: !Float             -- 最後に再生した時刻 [秒]
  , wNotes   :: [Note]             -- 予約した音
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-11 Equal loudness" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      r <- freshSeed
      pure (resetData False r world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetData False (seedRng 1) (World [] 0 0 [] False 1 (-10) [] (seedRng 1) [])

resetData :: Bool -> Rng -> World -> World
resetData sim r0 w = w { wSim = sim, wSets = [], wTi = 0, wOrder = concat os, wLev = lev, wRng = r' }
  where
    ((os, lev), r') = flip runRand r0 $ do
      o <- mapM (const (shuffleArr freqs)) [1 .. passes]
      l <- newLev
      pure (o, l)

-- はじめの音量はばらばらに（はじめの値に引っぱられないように）
newLev :: Rand Double
newLev = do
  u <- rnd
  pure (roundJ ((u - 0.5) * 12))

compVol :: Double -> Double
compVol db = refVol * dbToGain (clampv (-30) maxDb db)

-- 1 kHz → 比べる音（鳴っている間は重ねない）
playPair :: Env -> World -> World
playPair env w
  | wTi w >= length (wOrder w) || envTime env - wPlaying w < 1.4 = w
  | otherwise = w { wNotes = wNotes w ++ [ Note (t + 0.05) 1000 0.5 refVol 0
                                         , Note (t + 0.8) (getAt 1000 (wOrder w) (wTi w)) 0.5 (compVol (wLev w)) (round (wLev w) + 40) ]
                  , wPlaying = t }
  where t = envTime env

accept :: World -> World
accept w
  | wTi w >= length (wOrder w) || wSim w = w
  | otherwise = w { wSets = wSets w ++ [(getAt 0 (wOrder w) (wTi w), wLev w)], wTi = wTi w + 1, wLev = l, wRng = r' }
  where (l, r') = runRand newLev (wRng w)

meanAt :: Double -> [(Double, Double)] -> Maybe Double
meanAt f s = let r = [ v | (g, v) <- s, g == f ] in if null r then Nothing else Just (foldl1 (+) r / fromIntegral (length r))

-- dB = a + b u + c u²（u = log₂(f/1000)）を最小二乗で（1 kHz の 0 dB も 1 点として入れる）
data Curve = Curve { cvA, cvB, cvC :: !Double, cvBest :: Maybe Double }

curveAt :: Curve -> Double -> Double
curveAt c f = let u = logBase 2 (f / 1000) in cvA c + cvB c * u + cvC c * u * u

fitCurve :: [(Double, Double)] -> Maybe Curve
fitCurve s
  | length pts < 3 || abs d < 1e-9 = Nothing
  | otherwise = Just (Curve a b c (if c > 0 then Just (1000 * 2 ** (-b / (2 * c))) else Nothing))
  where
    pts = (0, 0) : [ (logBase 2 (f / 1000), m) | f <- freqs, Just m <- [meanAt f s] ]
    qs (u, _) = [1, u, u * u]
    m = [ [ sum [ qs p !! i * qs p !! j | p <- pts ] | j <- [0 .. 2] ] | i <- [0 .. 2] ]
    v = [ sum [ qs p !! i * snd p | p <- pts ] | i <- [0 .. 2] ]
    d = det3 m
    sol k = det3 [ [ if j == k then v !! i else x | (j, x) <- zip [0 ..] row ] | (i, row) <- zip [0 ..] m ] / d
    a = sol 0
    b = sol 1
    c = sol 2

det3 :: [[Double]] -> Double
det3 m = e 0 0 * (e 1 1 * e 2 2 - e 1 2 * e 2 1) - e 0 1 * (e 1 0 * e 2 2 - e 1 2 * e 2 0) + e 0 2 * (e 1 0 * e 2 1 - e 1 1 * e 2 0)
  where e i j = getAt 0 (getAt [] m i) j

-- シミュレーション観察者：本当の値 + ゆらぎ 1.5 dB で合わせる（音は鳴らさない）
simSets :: Int -> Rand [(Double, Double)]
simSets reps = sequence [ do z <- rndNormal
                             pure (f, roundJ (clampv (-30) maxDb (simTrue f + 1.5 * z)))
                        | _ <- [1 .. reps], f <- freqs ]

runSim :: Word32 -> World -> World
runSim seed w0 = w { wSets = s, wRng = r', wTi = length (wOrder w) }
  where
    w = resetData True (wRng w0) w0
    (s, r') = runRand (simSets 4) (seedRng seed)

step :: Env -> Float -> World -> IO World
step env _ w = do
  ns <- playDue (envTime env) (wNotes w)
  pure w { wNotes = ns }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Space play   Up/Down comparison volume   Enter (N) accept   O simulated observer   R reset" 20 32 17 (gray255 235)
  , say "Keep the volume low, especially with headphones. If it is too loud, turn the device down at once." 20 60 15 (rgb255 255 200 120) ]
  ++ (if wTi w < length (wOrder w) then trialPart else
       [ say (if wSim w then "Simulated observer (4 each, no sound)" else "Well done! This is your equal-loudness curve") bx by 17 (gray255 220) ])
    -- ---- 表
  ++ [ say "Freq [Hz]" bx ty 14 (gray255 160), say "matched volume [dB] -> mean" (bx + 90) ty 14 (gray255 160) ]
  ++ [ say (fmt 0 f) bx (ty + 22 + fromIntegral i * 20) 14 (gray255 210) | (i, f) <- zip [0 :: Int ..] freqs ]
  ++ [ say (commas [ fmt 0 v | (g, v) <- wSets w, g == f ] ++ maybe "" (\m -> "  -> " ++ fmt 1 m) (meanAt f (wSets w)))
           (bx + 90) (ty + 22 + fromIntegral i * 20) 14 (gray255 210) | (i, f) <- zip [0 :: Int ..] freqs ]
    -- ---- 等ラウドネス曲線
  ++ [ plotAxes g "Frequency [Hz] (log)" "Volume that sounds equally loud [dB] (1 kHz = 0)" [125, 250, 500, 1000, 2000, 4000, 8000] [-20, -10, 0, 10, 20]
     , Color (gray255 70) (Line [(gx g, gY g 0), (gx g + gw g, gY g 0)]) ]
  ++ [ Color (rgb255 120 200 140) (polyline 1.5 (sortOn fst ((gX g 1000, gY g 0) : [ (gX g f, gY g (simTrue f)) | f <- freqs ]))) | wSim w ]
  ++ [ Color (rgb255 255 170 90) (polyline 3 [ (gX g f, gY g (clampv (-20) 20 (curveAt c f))) | i <- [0 .. 100 :: Int], let f = 110 * (9000 / 110) ** (fromIntegral i / 100) ]) | Just c <- [fit] ]
  ++ [ Color (rgba255 120 190 255 120) (circleD (gX g f) (gY g v) 7) | (f, v) <- wSets w ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g f) (gY g m) 13) | f <- freqs, Just m <- [meanAt f (wSets w)] ]
  ++ [ Color (rgb255 255 220 120) (circleD (gX g 1000) (gY g 0) 13)
     , say (case fit >>= cvBest of
              Just b -> "Most sensitive frequency (parabola bottom) ~ " ++ fmt 0 b ++ " Hz"
              Nothing -> "Fit a parabola with 3 or more frequencies") (gx g) ry 16 (gray255 235)
     , say "Higher = needs more volume (less sensitive)." (gx g) (ry + 24) 14 (gray255 170)
     , say "Ears are most sensitive near 2-5 kHz." (gx g) (ry + 42) 14 (gray255 170) ]
  ++ [ say "Green = true curve of the simulated observer" (gx g) (ry + 66) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ say "(made up, not ISO 226)" (gx g) (ry + 84) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    bx = 20
    by = 108
    ty = by + 250
    lev = wLev w
    t = envTime env - wPlaying w
    trialPart =
      [ say ((if wSim w then "" else "You  ") ++ show (wTi w + 1) ++ " / " ++ show (length (wOrder w)) ++ "   comparison tone " ++ fmt 0 (getAt 0 (wOrder w) (wTi w)) ++ " Hz") bx by 17 (gray255 220)
      , say ("Volume vs the 1000 Hz reference: " ++ (if lev >= 0 then "+" else "") ++ fmt 0 lev ++ " dB" ++ (if lev >= maxDb then " (max)" else if lev <= -30 then " (min)" else "")) bx (by + 30) 17 (gray255 220)
        -- 音量のメーター
      , Color (if lev >= maxDb then rgb255 255 120 100 else rgb255 120 190 255) (rectTL bx (by + 50) (f2 (360 * (lev + 30) / (maxDb + 30))) 22)
      , Color (gray255 90) (frameTL 1 bx (by + 50) 360 22)
      , say "-30 dB" bx (by + 92) 14 (gray255 160)
      , say ("+" ++ fmt 0 maxDb ++ " dB (max)") (bx + 260) (by + 92) 14 (gray255 160)
        -- 再生中の表示（ゆっくり：1 回の再生で 2 回だけ光る）
      , Color (if t < 0.55 then rgb255 255 220 120 else gray255 60) (circleD (bx + 30) (by + 140) 34)
      , Color (if t > 0.75 && t < 1.3 then rgb255 120 190 255 else gray255 60) (circleD (bx + 110) (by + 140) 34)
      , say "Ref" (bx + 16) (by + 178) 14 (gray255 170)
      , say "Compare" (bx + 80) (by + 178) 14 (gray255 170) ]
    g = Graph (envW env * 0.52) 120 (envW env * 0.43) (envH env * 0.5) 100 10000 (-20) 20 True False
    fit = fitCurve (wSets w)
    ry = gy g + gh g + 70
    commas xs = foldr (\a b -> if null b then a else a ++ ", " ++ b) "" xs

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (playPair env w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wLev = min maxDb (wLev w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wLev = max (-30) (wLev w - 1) }
  EventKey (SpecialKey KeyEnter) Down _ _ -> pure (accept w)
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> pure (accept w)
    | c `elem` "oO" -> do r <- freshSeed
                          let s = wSimSeed w + 1
                          pure (runSim s w { wSimSeed = s, wRng = r })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（音は鳴らさない。p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("dB -> gain: +20 dB = 10x, -20 dB = 0.1x, +6 dB ~ 1.995x", near (dbToGain 20) 10 1e-12 && near (dbToGain (-20)) 0.1 1e-12 && near (dbToGain 6) 1.9953 1e-4)
  , ("volume limit: comparison tone at most 1.0, master volume <= 0.2", compVol 100 <= 1.0001 && master <= 0.2)
  , ("noise-free parabola -3u + 1.5u^2 (0 at 1 kHz) -> bottom at 2^1 x 1000 = 2000 Hz", maybe False (\b -> near b 2000 1e-6) (fitCurve s0 >>= cvBest))
  , ("no data -> no fit (null)", case fitCurve [] of { Nothing -> True; _ -> False })
  , ("simulated observer (30 each): means within +-1 dB of the true values", all (\f -> nearM (meanAt f ss) (simTrue f) 1) freqs) ]
  where
    s0 = [ (f, -3 * u + 1.5 * u * u) | f <- freqs, let u = logBase 2 (f / 1000) ]
    ss = fst (runRand (simSets 30) (seedRng 52))

-- ---------- やってみよう ----------
-- 1. 6 つの周波数を 2 回ずつ合わせて、自分の曲線を描こう。いちばん敏感なのは何 Hz？
-- 2. スピーカーとヘッドホンで測り比べよう。低い音（125 Hz）の値は大きく変わる？（小さなスピーカーは低音が出にくい）
-- 3. refVol を半分にして測り直すと、曲線の形は変わる？（小さい音ほど低音は聞こえにくい）
-- 4. 2 回の合わせた値のちがい（ばらつき）は何 dB くらい？
-- 5. 音楽のミックスで低音や高音を「どれだけ上げるか」を、この曲線から考えよう

-- ==================================================
-- ここから下は「心理物理の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列）。p5.js 版の rnd()（mulberry32）と同じ数が出る
newtype Rng = Rng Word32

-- 種 0 は 1 にする（p5.js 版の (s >>> 0) || 1 と同じ）
seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 乱数を「順番に」使う小さなしくみ：Rand a は「乱数の状態を受け取って、値と次の状態を返す」計算
newtype Rand a = Rand { runRand :: Rng -> (a, Rng) }

instance Functor Rand where
  fmap f (Rand g) = Rand (\r -> let (a, r') = g r in (f a, r'))
instance Applicative Rand where
  pure a = Rand (\r -> (a, r))
  Rand f <*> Rand g = Rand (\r -> let (h, r1) = f r; (a, r2) = g r1 in (h a, r2))
instance Monad Rand where
  Rand g >>= k = Rand (\r -> let (a, r1) = g r in runRand (k a) r1)

-- 0〜1 の一様乱数
rnd :: Rand Double
rnd = Rand $ \(Rng s0) ->
  let a = s0 + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法。u を先、v を後に引く）
rndNormal :: Rand Double
rndNormal = do
  u <- fmap (1 -) rnd
  v <- rnd
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー＝イェーツのシャッフル（後ろから順に入れかえる）
shuffleArr :: [a] -> Rand [a]
shuffleArr xs = go (V.fromList xs) (length xs - 1)
  where
    go v i | i <= 0 = pure (V.toList v)
           | otherwise = do
               u <- rnd
               let j = floor (u * fromIntegral (i + 1))
               go (v V.// [(i, v V.! j), (j, v V.! i)]) (i - 1)

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Rng
freshSeed = seedRng <$> randomRIO (1, maxBound)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の Math.round（.5 は大きい方へ）
roundJ :: Double -> Double
roundJ x = fromIntegral (floor (x + 0.5) :: Int)

-- ---- 正規分布：Φ（累積分布）と z（その逆関数。二分法で数値的に解く）
-- 誤差関数（Abramowitz & Stegun 7.1.26、誤差 1.5e-7 以下）
erfA :: Double -> Double
erfA x0 = s * y
  where
    s = if x0 < 0 then -1 else 1
    x = abs x0
    t = 1 / (1 + 0.3275911 * x)
    y = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-x * x)

phi :: Double -> Double
phi x = 0.5 * (1 + erfA (x / sqrt 2))

-- Φ(z) = p となる z
zOf :: Double -> Double
zOf p0 = go (-9) 9 (70 :: Int)
  where
    p = clampv 1e-9 (1 - 1e-9) p0
    go lo hi 0 = (lo + hi) / 2
    go lo hi n = let m = (lo + hi) / 2 in if phi m < p then go m hi (n - 1) else go lo m (n - 1)

-- 割合を 1/(2N) で補正（0 や 1 だと z が無限大になるので）
rateFix :: Double -> Double -> Double
rateFix k n = if n > 0 then clampv (1 / (2 * n)) (1 - 1 / (2 * n)) (k / n) else 0.5

-- ---- 上下法（階段法）：nDown 回続けて正答で 1 段下げ（難しく）、1 回まちがえたら 1 段上げ（やさしく）
-- 値は掛け算で動かす（factor 倍）。1-up-2-down は正答率 70.7%（p² = 0.5）、1-up-3-down は 79.4% に集まる
-- fast = True なら、最初にまちがえるまでは 1 回正解ごとに factor² ずつ下げて、すばやくしきい値の近くへ
data Stair = Stair
  { stV, stFactor :: !Double, stNDown :: !Int, stLo, stHi :: !Double, stFast :: !Bool
  , stRun, stDir :: !Int
  , stTrack :: [(Double, Bool)]     -- 試行の記録（値, 正答か）。新しい順
  , stRevs  :: [(Int, Double)] }    -- 反転点（試行番号, 値）。新しい順

makeStair :: Double -> Double -> Int -> Double -> Double -> Bool -> Stair
makeStair start f nd lo hi fast = Stair start f nd lo hi fast 0 0 [] []

-- これまでの試行数
stN :: Stair -> Int
stN = length . stTrack

stairUpdate :: Stair -> Bool -> Stair
stairUpdate st0 correct
  | stFast st = st { stDir = -1, stV = clampv (stLo st) (stHi st) (stV st / (stFactor st * stFactor st)) }
  | move == 0 = st { stRun = run' }
  | otherwise = st { stRun = run', stDir = move
                   , stRevs = if stDir st /= 0 && move /= stDir st then (stN st - 1, stV st) : stRevs st else stRevs st   -- 向きが変わった = 反転点
                   , stV = clampv (stLo st) (stHi st) (if move < 0 then stV st / stFactor st else stV st * stFactor st) }
  where
    st1 = st0 { stTrack = (stV st0, correct) : stTrack st0 }
    st = if stFast st1 && not correct then st1 { stFast = False } else st1
    (run', move) | correct = if stRun st + 1 >= stNDown st then (0, -1) else (stRun st + 1, 0 :: Int)
                 | otherwise = (0, 1)

-- しきい値 = 反転点の幾何平均（最初の skip 個は練習とみなして捨てる）。足りなければ最後の 6 試行の幾何平均
stairThreshold :: Stair -> Int -> Maybe Double
stairThreshold st skip
  | null r = Nothing
  | otherwise = Just (exp (foldl' (\s v -> s + log v) 0 r / fromIntegral (length r)))
  where
    r0 = map snd (drop skip (reverse (stRevs st)))
    r = if length r0 < 2 then map fst (reverse (take 6 (stTrack st))) else r0

-- ---- 直線 y = a + b x を最小二乗法で当てはめる（r² = 当てはまりのよさ）
data Lin = Lin { linA, linB, linR2 :: !Double }

linFit :: [Double] -> [Double] -> Maybe Lin
linFit xs ys
  | n < 2 || sxx < 1e-12 = Nothing
  | otherwise = Just (Lin (my - b * mx) b (if syy > 1e-12 then sxy * sxy / (sxx * syy) else 1))
  where
    n = length xs
    nf = fromIntegral n
    mx = sum xs / nf
    my = sum ys / nf
    sxx = sum [ (x - mx) ^ (2 :: Int) | x <- xs ]
    sxy = sum [ (x - mx) * (y - my) | (x, y) <- zip xs ys ]
    syy = sum [ (y - my) ^ (2 :: Int) | y <- ys ]
    b = sxy / sxx

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 t) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 t) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (gray255 95) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 20) 14 (gray255 165) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 6) (gY g v + 6) 14 (gray255 165) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 44) 14 (gray255 205)
     , say yl (gx g) (gy g - 10) 14 (gray255 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

gray255 :: Float -> Color
gray255 v = gray (v / 255)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- Maybe の値（null なら不合格）
nearM :: Maybe Double -> Double -> Double -> Bool
nearM m b tol = maybe False (\a -> near a b tol) m

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 120 210 150 else rgb255 255 90 90)
-- ---- 音の道具（Art.Sound の playTone で鳴らす）
-- 音量の上限：全体の音量 master は 0.2 以下（大きな音は出ない）。1 音の大きさ vol も 0〜1 に制限
master :: Double
master = 0.15                          -- ★ 全体の音量（0.2 以下にしておく）

-- 予約した 1 音：鳴らす時刻 [秒]、周波数、長さ、大きさ、音量の段（tag）
data Note = Note { nAt :: !Float, nF, nDur, nVol :: !Double, nTag :: !Int }

-- 1 音を鳴らす：周波数 f、長さ dur 秒、大きさ vol（0〜1）。全体の音量 master をかける
-- Art.Sound は WAV を（周波数, 0.01 刻みに丸めた音量, 長さ）で使い回すので、小さな音量の差が消えないように
-- 周波数に「音量の段 tag × 0.01 Hz」の聞こえない差をつけて、段ごとに別の音にする
tone :: Double -> Double -> Double -> Int -> IO ()
tone f dur vol tag
  | isNaN f || isInfinite f || f <= 0 || isNaN vol || isInfinite vol = pure ()
  | otherwise = playTone (realToFrac (clampv 20 10000 f + 0.01 * fromIntegral tag)) (realToFrac (master * clampv 0 1 vol))
                         (realToFrac (clampv 0.03 2 dur)) Sine

-- 時刻が来た音を鳴らし、残りを返す
playDue :: Float -> [Note] -> IO [Note]
playDue now ns = do
  let (due, rest) = (filter ((<= now) . nAt) ns, filter ((> now) . nAt) ns)
  mapM_ (\n -> tone (nF n) (nDur n) (nVol n) (nTag n)) due
  pure rest

-- +6 dB ≈ 2 倍、−20 dB = 0.1 倍
dbToGain :: Double -> Double
dbToGain db = 10 ** (db / 20)

