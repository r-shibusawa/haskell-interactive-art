-- ============================================
-- 52-13 マスキングと臨界帯域 🔊 ─ ノイズの中の音が聞こえるしきい値
-- ノイズが 2 回鳴り、どちらか一方にだけ 1 kHz の音が入っている。入っていたのは 1 回目（←）か 2 回目（→）か
-- 上下法で音の大きさのしきい値を測る。ノイズの帯域幅 BW（1 kHz を中心）を ↑↓ で変えて何回か測る
-- ノイズの「1 Hz あたりの強さ」は同じなので、帯域を広げるとノイズ全体は強くなる。でもしきい値が上がるのはある幅（臨界帯域）まで
-- → しきい値 = K + 10·log₁₀(min(BW, CB)) の折れ線を当てはめて、臨界帯域 CB を推定する
-- 操作：スペース = 開始 / ← 1 回目  → 2 回目 / ↑↓ = 帯域幅 / O = シミュレーション観察者 / R = やり直し
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
-- ※ Art.Sound には雑音がないので、ノイズは「帯域の中にばらまいた 6 つの正弦波」で代わりにする
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

bws :: [Double]
bws = [50, 100, 200, 400, 800, 1600]   -- ★ ノイズの帯域幅 [Hz]

nTrials :: Int
nTrials = 24                         -- ★ 1 つの帯域幅あたりの試行

noiseVol, burst, gap :: Double
noiseVol = 0.8                       -- ★ ノイズの大きさ（白色ノイズを帯域フィルタに通す前、0〜1）
burst = 0.45                         -- ★ ノイズの長さ [秒]
gap = 0.35                           -- ★ ノイズの間 [秒]

nSines :: Int
nSines = 6                           -- ★ ノイズの代わりの正弦波の数（同時に鳴らせるのは 8 音まで）

-- ★ シミュレーション観察者：しきい値 [dB] = K + 10 log₁₀(min(BW, CB))、ばらつき s
simK, simCB, simS :: Double
simK = -38
simCB = 130
simS = 1.5

data Phase = Ready | Play | Wait | Gap | Done deriving Eq

data World = World
  { wBi      :: !Int                -- 帯域幅の番号
  , wSt      :: Stair               -- いまの階段（値 = 音の大きさの倍率）
  , wThr     :: M.Map Int Double    -- 帯域幅ごとのしきい値 [dB]
  , wPhase   :: !Phase
  , wT       :: !Float
  , wBusy    :: !Float
  , wSecond  :: !Bool               -- 2 回目に音が入っているか
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wNotes   :: [Note]
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-13 Masking and critical band" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      r <- freshSeed
      pure world0 { wRng = r, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2 newStair M.empty Ready 0 0 False False 1 [] (seedRng 1) []

-- 音の大きさ（倍率）を 2 dB ずつ
newStair :: Stair
newStair = makeStair 0.1 (10 ** (2 / 20)) 2 0.001 1 True

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wSt = newStair, wThr = M.empty, wPhase = Ready, wRng = r }

dB :: Double -> Double
dB v = 20 * logBase 10 v

bwNow :: World -> Double
bwNow w = getAt 200 bws (wBi w)

-- ノイズの代わり：帯域 1000 ± BW/2 にランダムにばらまいた正弦波（毎回ちがう周波数。ノイズの乱数は種なし）
-- p5.js 版の帯域フィルタ（Q = 1000 / BW）を通した白色ノイズと全体の強さ（パワー）をそろえる：
-- パワー = (noiseVol)² / 3 × (π/2) × BW / 24000（48 kHz の白色ノイズ）→ 1 つの正弦波の大きさ = √(2 × パワー / 本数)
noiseNotes :: Float -> Int -> Double -> IO [Note]
noiseNotes t bi bw = do
  fs <- replicateM nSines (randomRIO (-0.5, 0.5 :: Double))
  let pw = noiseVol * noiseVol / 3 * (pi / 2) * bw / 24000
      a = sqrt (2 * pw / fromIntegral nSines)
      q = max 1 (bw / 40)            -- 周波数は 41 段に丸める（作る WAV が増えすぎないように）
  pure [ Note t (1000 + q * fromIntegral (round (u * bw / q) :: Int)) burst a (bi + 1) | u <- fs ]

startTrial :: Env -> World -> IO World
startTrial env w
  | stN (wSt w) >= nTrials = pure w { wThr = maybe id (\t -> M.insert (round (bwNow w)) (dB t)) (stairThreshold (wSt w) 2) (wThr w), wPhase = Done }
  | otherwise = do
      let (sec, r') = runRand (fmap (< 0.5) rnd) (wRng w)
          t = now + 0.1
          v = stV (wSt w)
      n1 <- noiseNotes t (wBi w) (bwNow w)
      n2 <- noiseNotes (t + realToFrac (burst + gap)) (wBi w) (bwNow w)
      -- 音はノイズのまん中に（音量の段 = dB の整数で、別の音として鳴らす）
      let sig = Note (t + (if sec then realToFrac (burst + gap) else 0) + 0.075) 1000 (burst - 0.15) v (100 + round (dB v))
      pure w { wSecond = sec, wRng = r', wBusy = now + realToFrac (2 * burst + gap + 0.2)
             , wNotes = wNotes w ++ n1 ++ n2 ++ [sig], wPhase = Play, wT = now }
  where now = envTime env

answer :: Env -> Bool -> World -> World
answer env second w
  | wPhase w /= Wait = w
  | otherwise = w { wSt = stairUpdate (wSt w) (second == wSecond w), wPhase = Gap, wT = envTime env }

-- 折れ線 t = K + 10 log₁₀(min(BW, CB)) を当てはめる：CB を 10〜3000 Hz で細かく試し、K は平均で決まる
data CBFit = CBFit { cbCB, cbK, cbE :: !Double }

fitCB :: M.Map Int Double -> Maybe CBFit
fitCB tm
  | length bw < 3 = Nothing
  | otherwise = Just (foldl' better (try 0) (map try [1 .. 600]))
  where
    bw = M.toList tm
    n = fromIntegral (length bw)
    try :: Int -> CBFit
    try i = let cb = 10 * 300 ** (fromIntegral i / 600)
                gs = [ (t, 10 * logBase 10 (min (fromIntegral b) cb)) | (b, t) <- bw ]
                k = foldl' (\s (t, g) -> s + t - g) 0 gs / n
                e = foldl' (\s (t, g) -> s + (t - k - g) ^ (2 :: Int)) 0 gs
            in CBFit cb k e
    better b c = if cbE c < cbE b - 1e-12 then c else b

-- グラスバーグとムーアの等価矩形帯域幅 [Hz]
erb :: Double -> Double
erb f = 24.7 * (4.37 * f / 1000 + 1)

-- シミュレーション観察者（音は鳴らさない）：正答率 = 0.5 + 0.5·L((dB − α)/s)。70.7% 点 = モデルのしきい値
simThr :: Double -> Double
simThr bw = simK + 10 * logBase 10 (min bw simCB)

simPc :: Double -> Double -> Double
simPc v bw = let a = simThr bw - simS * log (0.4142 / 0.5858) in 0.5 + 0.5 / (1 + exp (-(dB v - a) / simS))

simStair :: Double -> Int -> Rand Stair
simStair bw n = go n newStair
  where go 0 s = pure s
        go k s = do u <- rnd
                    go (k - 1) (stairUpdate s (u < simPc (stV s) bw))

runSim :: Word32 -> World -> World
runSim seed w0 = w { wThr = M.fromList [ (round b, dB t) | (b, Just t) <- zip bws ths ], wSt = st, wPhase = Done, wRng = r' }
  where
    w = resetData True (wRng w0) w0
    ((ths, st), r') = flip runRand (seedRng seed) $ do
      t <- mapM (\b -> fmap (`stairThreshold` 2) (simStair b 80)) bws
      s <- simStair (bwNow w) 80
      pure (t, s)

step :: Env -> Float -> World -> IO World
step env _ w = do
  ns <- playDue (envTime env) (wNotes w)
  let w1 = w { wNotes = ns }
      el = envTime env - wT w1
  if wPhase w1 == Gap && el > 0.7 then startTrial env w1
    else pure (if wPhase w1 == Play && el > realToFrac (2 * burst + gap + 0.15) then w1 { wPhase = Wait } else w1)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   Left = 1st   Right = 2nd   Up/Down bandwidth " ++ fmt 0 bw ++ " Hz   O simulated observer   R reset") 20 32 16 (gray255 235)
  , say "Keep the volume low, especially with headphones. Which noise had the tone in it?" 20 60 15 (rgb255 255 200 120)
  , Color (if lit 0.1 then rgb255 150 200 255 else gray255 55) (circleD (bx + 40) (by + 40) 50)
  , Color (if lit (0.1 + bu + gp) then rgb255 150 200 255 else gray255 55) (circleD (bx + 140) (by + 40) 50)
  , sayC "1" (bx + 40) (by + 48) 18 (gray255 30)
  , sayC "2" (bx + 140) (by + 48) 18 (gray255 30)
  , say msg bx (by + 110) 16 (gray255 215)
  , say ((if wSim w then "Simulated observer" else "You") ++ "   bandwidth " ++ fmt 0 bw ++ " Hz   trial " ++ show (stN st)
         ++ (if wSim w then "" else " / " ++ show nTrials) ++ "   tone now " ++ fmt 1 (dB (stV st)) ++ " dB") bx (by + 136) 16 (gray255 215)
    -- ノイズの帯域の図
  , Color (rgba255 120 170 255 110) (rectTL (fX lo) (sy + 40) (fX (lo + bw) - fX lo) (sh - 40))
  , Color (rgb255 255 220 120) (rectTL (fX 1000 - 1.5) (sy + 10) 3 (sh - 10))
  , Color (rgba255 255 120 120 180) (thickLine 1.5 (fX (1000 - erb 1000 / 2), sy + 25) (fX (1000 + erb 1000 / 2), sy + 25))
  , Color (gray255 95) (frameTL 1 sx sy sw sh)
  , say "250 Hz" sx (sy + sh + 18) 14 (gray255 170)
  , sayR "4000 Hz" (sx + sw) (sy + sh + 18) 14 (gray255 170)
  , say ("red: critical band at 1 kHz (ERB ~ " ++ fmt 0 (erb 1000) ++ " Hz)   blue: noise   yellow: tone") sx (sy + sh + 38) 14 (gray255 170)
  , say ("Noise here = " ++ show nSines ++ " random sine tones in the band (no noise source)") sx (sy + sh + 58) 14 (gray255 140)
    -- ---- しきい値と帯域幅
  , plotAxes g "Noise bandwidth [Hz]" "Threshold [dB]" bws [-40, -30, -20, -10, 0] ]
  ++ [ Color (rgb255 120 200 140) (polyline 1.5 [ (gX g b, gY g (simThr b)) | b <- bws ]) | wSim w ]
  ++ concat [ [ Color (rgb255 255 170 90) (polyline 3 [ (gX g b, gY g (cbK f + 10 * logBase 10 (min b (cbCB f)))) | i <- [0 .. 100 :: Int], let b = 40 * (2000 / 40) ** (fromIntegral i / 100) ])
              , Color (rgba255 255 170 90 120) (Line [(gX g (cbCB f), gy g), (gX g (cbCB f), gy g + gh g)]) ] | Just f <- [fit] ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g (fromIntegral b)) (gY g t) 13) | (b, t) <- M.toList (wThr w) ]
  ++ [ say (case fit of
              Just f -> "Estimated CB ~ " ++ fmt 0 (cbCB f) ++ " Hz (ERB formula: " ++ fmt 0 (erb 1000) ++ " Hz)"
              Nothing -> "Fit the broken line with 3 or more bandwidths") (gx g - 30) ry 16 (gray255 235)
     , say "Doubling the band adds +3 dB of noise, but band beyond CB" (gx g - 30) (ry + 24) 14 (gray255 170)
     , say "does not make the tone harder to hear." (gx g - 30) (ry + 42) 14 (gray255 170) ]
  ++ [ say ("True CB = " ++ fmt 0 simCB ++ " Hz") (gx g - 30) (ry + 66) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    bx = 20
    by = 100
    st = wSt w
    bw = bwNow w
    el = envTime env - wT w
    bu = realToFrac burst
    gp = realToFrac gap
    lit t0 = wPhase w == Play && el > t0 && el < t0 + bu
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show nTrials ++ " trials)"
      Wait -> "Which one had the tone? (Left = 1st, Right = 2nd)"
      Done -> if wSim w then "Simulated observer (80 trials each, no sound)" else "Finished! Up/Down for another bandwidth -> space"
      _ -> ""
    sx = bx + 40
    sy = by + 190
    sw = envW env * 0.4
    sh = 140
    fX f = sx + sw * f2 (log (f / 250)) / f2 (log 16)
    lo = max 250 (1000 - bw / 2)
    g = Graph (envW env * 0.6) 120 (envW env * 0.35) (envH env * 0.45) 35 2300 (-45) 0 True False
    fit = fitCB (wThr w)
    ry = gy g + gh g + 70

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _
    | (wPhase w == Ready || wPhase w == Done) && envTime env > wBusy w ->
        startTrial env (if wPhase w == Done || wSim w then w { wSim = False, wSt = newStair } else w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env True w)
  EventKey (SpecialKey k) Down _ _
    | (k == KeyUp || k == KeyDown) && (wPhase w == Ready || wPhase w == Done) ->
        pure w { wBi = max 0 (min (length bws - 1) (wBi w + (if k == KeyUp then 1 else -1))), wSt = newStair, wPhase = Ready }
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> do r <- freshSeed
                          let s = wSimSeed w + 1
                          pure (runSim s w { wSimSeed = s, wRng = r })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（音は鳴らさない。p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("ERB(1000 Hz) = 24.7 x (4.37 + 1) = 132.6 Hz", near (erb 1000) 132.639 1e-3)
  , ("doubling the band -> noise +10 log10 2 = 3.01 dB", near (10 * logBase 10 2) 3.0103 1e-4)
  , ("noise-free broken line (CB = 150) -> CB ~ 150 Hz (+-2%) and K = -40", maybe False (\f -> near (cbCB f) 150 3 && near (cbK f) (-40) 0.1) (fitCB t0))
  , ("only 2 points -> no fit (null)", case fitCB (M.fromList [(100, -20), (200, -17)]) of { Nothing -> True; _ -> False })
  , ("simulated observer's 70.7% point = the model threshold", near (simPc (10 ** (simThr 400 / 20)) 400) 0.7071 1e-3)
  , ("simulated staircase: thresholds within +-2 dB (1 step) of the model", all (\(b, s) -> near s (simThr b) 2) (zip bws ss))
  , ("from those 6 points CB ~ 130 Hz (/1.6 to x1.6)", maybe False (\f -> abs (log (cbCB f / simCB)) < log 1.6) (fitCB (M.fromList (zip (map round bws) ss))))
  , ("volume limit: noise <= 1, tone <= 1, master <= 0.2", noiseVol <= 1 && stHi newStair <= 1 && master <= 0.2) ]
  where
    t0 = M.fromList [ (round b, -40 + 10 * logBase 10 (min b 150)) | b <- bws ]
    ss = fst (runRand (mapM (\b -> do a <- mapM (const (fmap (\s -> dB (maybe 0 id (stairThreshold s 2))) (simStair b 80))) [1 .. 20 :: Int]
                                      pure (foldl1 (+) a / fromIntegral (length a))) bws) (seedRng 52))

-- ---------- やってみよう ----------
-- 1. 帯域幅 50 Hz と 800 Hz でしきい値を測り比べよう。差は何 dB？ ノイズ全体は 12 dB ちがうのに？
-- 2. 3〜4 つの帯域幅を測って折れ線を当てはめ、自分の臨界帯域を推定しよう
-- 3. 音の周波数を 2000 Hz にしたら、臨界帯域は広くなる？（erb 2000 を計算してみよう）
-- 4. にぎやかな所で話し声が聞こえにくい理由を、マスキングで説明しよう
-- 5. 音楽の圧縮（MP3 など）が「聞こえない音」を削る考え方と、この実験の関係を調べよう

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

