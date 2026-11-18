-- ============================================
-- 52-07 コントラスト感度関数（CSF）─ どの細かさの縞がいちばん見えやすい？
-- 左の図は計算で作った縞（キャンベル-ロブソン型）：右へ行くほど細かく（空間周波数が高く）、上へ行くほどうすく（コントラストが低く）なる
-- モード 1：縞が見えなくなる高さを、横の位置を変えながら何か所かクリック → 感度 S = 1 / コントラスト を両対数で描き、放物線を当てはめる
-- モード 2：周波数を ←→ で選び、縞が + の上か下かを ↑↓ で答える上下法（1-up-2-down）。しきい値を CSF に足す
-- 操作：1 / 2 = モード / クリック / ←→ ↑↓ スペース（モード 2）/ O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

viewCm, pxPerCm :: Double
viewCm = 50                          -- ★ 目と画面の距離 [cm]
pxPerCm = 38                         -- ★ 画面の 1 cm の画素数（だいたいでよい）

pxPerDeg :: Double
pxPerDeg = viewCm * tan (pi / 180) * pxPerCm   -- 視角 1 度あたりの画素数

f0, f1, cMin, cMax :: Double
f0 = 0.15                            -- ★ 図の左はしの空間周波数 [cycles/deg]
f1 = 14                              -- ★ 図の右はし
cMin = 0.003                         -- ★ 図の上はしのコントラスト
cMax = 0.8                           -- ★ 図の下はし

freqs :: [Double]
freqs = [0.5, 1, 2, 4, 8]            -- ★ モード 2 で測る周波数 [c/deg]

-- ★ シミュレーション観察者の CSF：ln S = ln Smax − k (ln f − ln fp)²
simSmax, simFp, simK :: Double
simSmax = 150
simFp = 3
simK = 0.5

chartRows :: Int
chartRows = 120                      -- 図のたての画素数（横は画面と同じ。8 万画素以下にするため、たてだけ粗く）

data Phase = Ready | Fix | Stim | Wait | Gap deriving Eq

data World = World
  { wCw, wCh    :: !Int            -- 図の大きさ（起動時の画面から決める）
  , wChart      :: Picture         -- 縞の図（起動時に 1 回だけ作る）
  , wPatch      :: Picture         -- モード 2 の刺激（試行ごとに 1 回だけ作る）
  , wClicks     :: [(Double, Double)]   -- (周波数, 感度)：クリック
  , wStairPts   :: [(Double, Double)]   -- (周波数, 感度)：上下法
  , wFi         :: !Int
  , wSt         :: Stair
  , wMode       :: !Int
  , wPhase      :: !Phase
  , wT          :: !Float
  , wUp         :: !Bool            -- 模様は + の上
  , wSim        :: !Bool
  , wSimSeed    :: !Word32
  , wRng        :: !Rng
  , wTest       :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-07 Contrast sensitivity" (world0 1280 800))
  { skBack  = rgb255 30 32 40
  , skStart = \env -> do
      printSelfTest selfTest
      r <- freshSeed
      pure (world0 (envW env) (envH env)) { wRng = r, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> Float -> World
world0 sw sh = resetData False (seedRng 1) w
  where
    cw = floor (sw * 0.52)
    ch = floor (sh * 0.62)
    w = World cw ch (makeChart cw ch) Blank [] [] 2 newStair 1 Ready 0 False False 1 (seedRng 1) []

newStair :: Stair
newStair = makeStair 0.05 1.3 2 cMin cMax True

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wClicks = [], wStairPts = [], wSt = newStair, wPhase = Ready, wRng = r }

-- 図の位置 → 周波数・コントラスト（どちらも対数で等間隔）
freqAt :: Double -> Double -> Double
freqAt cw x = f0 * (f1 / f0) ** (x / cw)

contrastAt :: Double -> Double -> Double
contrastAt ch y = cMin * (cMax / cMin) ** (y / ch)

-- 縞の図：横は 1 画素ずつ、たては chartRows 段（種 1 の乱数で ±0.5 のディザ）
makeChart :: Int -> Int -> Picture
makeChart cw ch = Scale 1 (-ky) (pixelImage cw chartRows 1 (\x y -> let v = vals U.! (x * chartRows + y) in (v, v, v)))
  where
    cwd = fromIntegral cw :: Double
    ky = fromIntegral ch / fromIntegral chartRows :: Float
    r = log (f1 / f0)
    k = 2 * pi * f0 * cwd / (r * pxPerDeg)
    noise = U.fromListN (cw * chartRows) (take (cw * chartRows) (rndList (seedRng 1)))
    vals = U.generate (cw * chartRows) $ \i ->
      let (x, y) = i `divMod` chartRows
          ph = k * (exp (r * fromIntegral x / cwd) - 1)     -- 周波数が変わっても縞がつながるように、周波数を積分した位相
          yc = (fromIntegral y + 0.5) * fromIntegral ch / fromIntegral chartRows
          v = 128 * (1 + contrastAt (fromIntegral ch) yc * sin ph) + noise U.! i - 0.5
      in fromIntegral (round (clampv 0 255 (roundJ v)) :: Int) :: Word8

rndList :: Rng -> [Double]
rndList r = let (v, r') = runRand rnd r in v : rndList r'

-- ガボール模様（周波数 f、コントラスト c）。130 × 130 画素
makePatch :: Double -> Double -> Rand B.ByteString
makePatch f c = B.pack <$> mapM px [ (x, y) | y <- [0 .. n - 1], x <- [0 .. n - 1] ]
  where
    n = 130 :: Int
    h = fromIntegral n / 2
    px (x, y) = do
      u <- rnd
      let dx = fromIntegral x - h
          dy = fromIntegral y - h
          env = exp (-(dx * dx + dy * dy) / (2 * 22 * 22))
          v = 128 * (1 + c * env * sin (2 * pi * f * dx / pxPerDeg)) + u - 0.5
      pure (fromIntegral (round (clampv 0 255 (roundJ v)) :: Int))

curF :: World -> Double
curF w = getAt 2 freqs (wFi w)

startTrial :: Env -> World -> World
startTrial env w
  | stN (wSt w) >= 30 = finishStair w
  | otherwise = w { wUp = up, wPatch = Scale 1 (-1) (grayImage 130 130 1 bs), wPhase = Fix, wT = envTime env, wRng = r' }
  where
    ((up, bs), r') = flip runRand (wRng w) $ do
      u <- fmap (< 0.5) rnd
      b <- makePatch (curF w) (stV (wSt w))
      pure (u, b)

finishStair :: World -> World
finishStair w = w { wStairPts = wStairPts w ++ [ (curF w, 1 / t) | Just t <- [stairThreshold (wSt w) 2], t /= 0 ], wSt = newStair, wPhase = Ready }

answer :: Env -> Bool -> World -> World
answer env up w
  | wMode w /= 2 || (wPhase w /= Stim && wPhase w /= Wait) = w
  | otherwise = w { wSt = stairUpdate (wSt w) (up == wUp w), wPhase = Gap, wT = envTime env }

-- 両対数で放物線：ln S = a + b u + c u²（u = ln f）を最小二乗で → ピーク周波数 = e^(−b / 2c)
data Csf = Csf { csA, csB, csC :: !Double, csPeak, csSmax :: Maybe Double }

csfS :: Csf -> Double -> Double
csfS q f = exp (csA q + csB q * log f + csC q * log f ^ (2 :: Int))

fitCSF :: [(Double, Double)] -> Maybe Csf
fitCSF pts
  | length pts < 3 = Nothing
  | abs d < 1e-9 = Nothing
  | otherwise = Just (Csf a b c peak (fmap (const (exp (a - b * b / (4 * c)))) peak))
  where
    qs = [ ([1, u, u * u], log s) | (f, s) <- pts, let u = log f ]
    m = [ [ sum [ (q !! i) * (q !! j) | (q, _) <- qs ] | j <- [0 .. 2] ] | i <- [0 .. 2 :: Int] ]
    v = [ sum [ (q !! i) * s | (q, s) <- qs ] | i <- [0 .. 2 :: Int] ]
    d = det3 m
    sol k = det3 [ [ if j == k then vi else x | (j, x) <- zip [0 :: Int ..] row ] | (row, vi) <- zip m v ] / d
    a = sol 0
    b = sol 1
    c = sol 2
    peak = if c < 0 then Just (exp (-b / (2 * c))) else Nothing

det3 :: [[Double]] -> Double
det3 m = g 0 0 * (g 1 1 * g 2 2 - g 1 2 * g 2 1) - g 0 1 * (g 1 0 * g 2 2 - g 1 2 * g 2 0) + g 0 2 * (g 1 0 * g 2 1 - g 1 1 * g 2 0)
  where g i j = getAt 0 (getAt [] m i) j

-- シミュレーション観察者
simS :: Double -> Double
simS f = simSmax * exp (-simK * (log f - log simFp) ^ (2 :: Int))

simPc :: Double -> Double -> Double
simPc f c = 0.5 + 0.5 / (1 + exp (-(log c - log alpha) / s))
  where s = 0.35
        alpha = (1 / simS f) / exp (s * log (0.4142 / 0.5858))

-- 周波数 f の階段を n 回、シミュレーション観察者で進める
simStair :: Int -> Double -> Stair -> Rand Stair
simStair 0 _ st = pure st
simStair n f st = do
  u <- rnd
  simStair (n - 1) f (stairUpdate st (u < simPc f (stV st)))

runSim :: Word32 -> World -> World
runSim seed w0 = w { wClicks = cl, wStairPts = sp, wRng = r' }
  where
    w = resetData True (seedRng seed) w0
    ((cl, sp), r') = flip runRand (wRng w) $ do
      c <- mapM (\i -> do let f = f0 * (f1 / f0) ** ((fromIntegral i + 0.5) / 10)
                          e <- rndNormal
                          pure (f, simS f * exp (0.15 * e))) [0 .. 9 :: Int]
      s <- mapM (\f -> do st <- simStair 60 f newStair
                          pure (f, 1 / maybe 1 id (stairThreshold st 2))) freqs
      pure (c, s)

step :: Env -> Float -> World -> IO World
step env _ w
  | wMode w /= 2 = pure w
  | wPhase w == Gap && dt > 0.5 = pure (startTrial env w)
  | wPhase w == Fix && dt > 0.4 = pure w { wPhase = Stim, wT = envTime env }
  | wPhase w == Stim && dt > 0.5 = pure w { wPhase = Wait }
  | otherwise = pure w
  where dt = envTime env - wT w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "1 click mode   2 staircase (Left/Right frequency, space start, Up/Down answer)" 20 26 18 (gray255 235)
  , say ("Now: " ++ (if wMode w == 1 then "click" else "staircase") ++ "   O simulated observer   R reset") 20 48 16 (gray255 235)
  , say ("Click where the stripes vanish. Right = finer (c/deg = stripes per degree, at " ++ fmt 0 viewCm ++ " cm)") 20 70 14 (gray255 170) ]
  ++ (if wMode w == 1
        then [ at (sx + cw / 2, sy + ch / 2) (wChart w) ]
             ++ [ Color (rgb255 255 170 90) (circleD (sx + cw * f2 (log (f / f0) / log (f1 / f0))) (sy + ch * f2 (log (1 / s / cMin) / log (cMax / cMin))) 9) | (f, s) <- wClicks w ]
             ++ [ say ("<- coarse (" ++ show f0 ++ " c/deg)") sx (sy + ch + 20) 14 (gray255 170)
                , sayR ("fine (" ++ fmt 0 f1 ++ " c/deg) ->") (sx + cw) (sy + ch + 20) 14 (gray255 170) ]
        else [ Color (gray255 128) (rectTL sx sy cw ch) ]
             ++ [ at (px + 65, py + 65) (wPatch w) | wPhase w == Stim ]
             ++ [ Color (gray255 70) (thickLine 2 (sx + cw / 2 - 7, sy + ch / 2) (sx + cw / 2 + 7, sy + ch / 2))
                , Color (gray255 70) (thickLine 2 (sx + cw / 2, sy + ch / 2 - 7) (sx + cw / 2, sy + ch / 2 + 7))
                , say ("Frequency " ++ prec3 (curF w) ++ " c/deg   trial " ++ show (stN (wSt w)) ++ " / 30   contrast " ++ fmt 2 (stV (wSt w) * 100) ++ "%   "
                       ++ (case wPhase w of { Ready -> "space to start"; Wait -> "up (Up) or down (Down)?"; _ -> "" })) sx (sy + ch + 22) 15 (gray255 205) ])
  ++ [ say (if wSim w then "Simulated observer" else "You are the observer") sx (sy + ch + 44) 14 (gray255 200)
    -- ---- CSF のグラフ
     , plotAxes g "Spatial frequency [c/deg]" "Sensitivity S = 1 / contrast" [0.2, 0.5, 1, 2, 5, 10] [1, 3, 10, 30, 100, 300] ]
  ++ [ drawS simS (rgb255 120 200 140) 1.5 | wSim w ]
  ++ [ drawS (csfS q) (rgb255 255 170 90) 3 | Just q <- [fit] ]
  ++ [ Color (rgb255 255 170 90) (circleD (gX g f) (gY g s) 9) | (f, s) <- wClicks w ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g f) (gY g s) 13) | (f, s) <- wStairPts w ]
  ++ (case fit of
        Just q | Just pk <- csPeak q, Just sm <- csSmax q ->
          [ say ("Peak frequency ~ " ++ fmt 2 pk ++ " c/deg") (gx g - 30) ry 16 (gray255 235)
          , say ("Max sensitivity ~ " ++ fmt 0 sm ++ " (contrast " ++ fmt 2 (100 / sm) ++ "%)") (gx g - 30) (ry + 22) 16 (gray255 235) ]
        _ -> [ say "Fits a parabola with 3 or more points" (gx g - 30) ry 16 (gray255 235)
             , say "(shows the peak when it bends down)" (gx g - 30) (ry + 22) 16 (gray255 235) ])
  ++ [ say "Orange = clicks, blue = staircase," (gx g - 30) (ry + 46) 14 (gray255 170)
     , say "orange line = parabola in log-log" (gx g - 30) (ry + 64) 14 (gray255 170) ]
  ++ [ say ("Green = true CSF (peak " ++ fmt 0 simFp ++ " c/deg, max " ++ fmt 0 simSmax ++ ")") (gx g - 30) (ry + 86) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    sx = 20
    sy = 84
    cw = fromIntegral (wCw w)
    ch = fromIntegral (wCh w)
    px = sx + cw / 2 - 65
    py = sy + ch / 2 + (if wUp w then -175 else 45)
    g = Graph (envW env * 0.62) 110 (envW env * 0.34) (envH env * 0.5) f0 f1 1 (1 / cMin) True True
    fit = fitCSF (wClicks w ++ wStairPts w)
    drawS fn c wt = Color c (polyline wt [ (gX g f, gY g s) | i <- [0 .. 120 :: Int], let f = f0 * (f1 / f0) ** (fromIntegral i / 120), let s = fn f, s > 0, not (isNaN s || isInfinite s) ])
    ry = gy g + gh g + 64

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) | wMode w == 1 -> do
    let x = float2 (mx + envW env / 2 - 20)
        y = float2 (envH env / 2 - my - 84)
        cw = fromIntegral (wCw w)
        ch = fromIntegral (wCh w)
    if x >= 0 && x < cw && y >= 0 && y < ch
      then do w1 <- if wSim w then (\r -> resetData False r w) <$> freshSeed else pure w
              pure w1 { wClicks = wClicks w1 ++ [(freqAt cw x, 1 / contrastAt ch y)] }
      else pure w
  EventKey (Char c) Down _ _ | c `elem` "12" -> pure w { wMode = if c == '1' then 1 else 2, wPhase = Ready }
  EventKey (SpecialKey KeySpace) Down _ _ | wMode w == 2 && wPhase w == Ready -> do
    w1 <- if wSim w then (\r -> resetData False r w) <$> freshSeed else pure w
    pure (startTrial env w1)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (answer env True w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey k) Down _ _ | wMode w == 2 && wPhase w == Ready && (k == KeyLeft || k == KeyRight) ->
    pure w { wFi = max 0 (min (length freqs - 1) (wFi w + (if k == KeyLeft then -1 else 1))), wSt = newStair }
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w
  where float2 = realToFrac :: Float -> Double

-- ---- 自己テスト（p5.js 版と同じ 7 項目。図の大きさは 1280 × 800 の画面のとき）
selfTest :: [(String, Bool)]
selfTest =
  [ ("chart top contrast = " ++ show cMin ++ ", bottom = " ++ show cMax, near (contrastAt ch0 0) cMin 1e-12 && near (contrastAt ch0 ch0) cMax 1e-12)
  , ("chart left, right and middle frequency (middle = sqrt(F0 F1))", near (freqAt cw0 0) f0 1e-12 && near (freqAt cw0 cw0) f1 1e-9 && near (freqAt cw0 (cw0 / 2)) (sqrt (f0 * f1)) 1e-9)
  , ("noise-free CSF points give back peak 3 c/deg and max sensitivity 150"
    , case fitCSF [ (f, simS f) | f <- [0.3, 0.7, 1.5, 3, 6, 10] ] of
        Just q -> nearM (csPeak q) simFp 1e-6 && nearM (csSmax q) simSmax 1e-4
        Nothing -> False)
  , ("2 points: no fit (null)", maybe True (const False) (fitCSF [(1, 10), (2, 20)]))
  , ("simulated observer: 70.7% point = 1 / S(f)", near (simPc 2 (1 / simS 2)) 0.7071 1e-3)
  , ("simulated staircases: sensitivity ~ S(f) at all 5 frequencies (within one step x1.3)", all (\(f, s) -> abs (log (s / simS f)) < log 1.3) sp)
  , ("from those 5 points: peak ~ 3 c/deg (+-30%)", maybe False (\q -> nearM (csPeak q) simFp (0.3 * simFp)) (fitCSF sp)) ]
  where
    cw0 = fromIntegral (floor (1280 * 0.52 :: Double) :: Int)
    ch0 = fromIntegral (floor (800 * 0.62 :: Double) :: Int)
    sp = fst $ flip runRand (seedRng 52) $ mapM (\f -> do
           th <- mapM (\_ -> do st <- simStair 80 f newStair
                                pure (log (maybe 1 id (stairThreshold st 2)))) [1 .. 20 :: Int]
           pure (f, 1 / exp (sum th / fromIntegral (length th)))) freqs

-- ---------- やってみよう ----------
-- 1. 図の上のほう（うすい所）で、縞がいちばん高くまで見えるのはどの細かさ？ そこが CSF の山
-- 2. 横の位置を変えて 6 か所以上クリックし、自分の CSF を描こう。山は何 c/deg？
-- 3. 画面から離れて（viewCm を変えて）もう一度。山の位置はどう動く？（c/deg が変わる）
-- 4. モード 2 で 3 つの周波数を上下法で測り、クリックの結果と比べよう
-- 5. 文字の読みやすさ・模様のデザインと、CSF の山の関係を考えよう

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
