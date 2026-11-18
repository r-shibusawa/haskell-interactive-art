-- ============================================
-- 52-01 フェヒナーの法則 ─ JND を積み上げると対数になる
-- 光の量 I が JND（ΔI = k·I）ずつ増えるたびに「感じ」が 1 段上がるとすると、段の数 n = ln(I/I₀) ÷ ln(1 + k)（対数）
-- 上：灰色の階段を「光の量で等間隔」「対数で等間隔（フェヒナー）」「L*（べき 1/3）」で並べて比べる
-- 右：二分法の実験。両はしの灰色の「ちょうど真ん中」に見える灰色を ←→（細かく）↑↓（大きく）で選び、スペースで決定。7 回で自分の尺度ができる
-- 操作：←→ ↑↓ = 真ん中の灰色を選ぶ / スペース = 決定 / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

lLo, lHi :: Double
lLo = 0.005                          -- ★ いちばん暗い光の量（画面の白 = 1）
lHi = 1                              -- ★ いちばん明るい光の量

steps :: Int
steps = 8                            -- ★ 灰色の階段の段数

jndK :: Double
jndK = 0.25                          -- ★ 左下の図の JND の比 k（実際は 0.01〜0.08 くらい。見やすく大きめ）

simGamma, simNoise :: Double
simGamma = 0.4                       -- ★ シミュレーション観察者：感じ = I^γ
simNoise = 0.02                      -- ★ 真ん中を選ぶときのゆらぎ

-- 二分する区間（感じの目盛り）
order :: [(Double, Double)]
order = [(0, 1), (0, 0.5), (0.5, 1), (0, 0.25), (0.25, 0.5), (0.5, 0.75), (0.75, 1)]

-- 当てはめの結果：γ、誤差（RMS）
data FitG = FitG { fgG, fgRms :: !Double }

data World = World
  { wLum     :: [(Double, Double)]     -- (感じ S, 選んだ光の量 I)。決めた順
  , wQi      :: !Int                   -- いま何番目の区間か
  , wMid     :: !Double                -- いま選んでいる真ん中の光の量
  , wFit     :: Maybe FitG
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-01 Fechner's law" world0)
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
world0 = resetData False (seedRng 1) (World [] 0 0.2 Nothing False 1 (seedRng 1) [])

lum0 :: [(Double, Double)]
lum0 = [(0, lLo), (1, lHi)]

lumAt :: [(Double, Double)] -> Double -> Double
lumAt m s = maybe lLo id (lookup s m)

resetData :: Bool -> Rng -> World -> World
resetData sim r w = newMid w { wSim = sim, wLum = lum0, wQi = 0, wFit = Nothing, wRng = r }

-- 最初の位置は区間の中のランダムな所（かたよりを防ぐ）
newMid :: World -> World
newMid w
  | wQi w >= length order = w
  | otherwise = w { wMid = exp (log a + (0.2 + 0.6 * u) * (log b - log a)), wRng = r' }
  where
    (q0, q1) = getAt (0, 1) order (wQi w)
    a = lumAt (wLum w) q0
    b = lumAt (wLum w) q1
    (u, r') = runRand rnd (wRng w)

accept :: World -> World
accept w
  | wQi w >= length order = w
  | otherwise = let (q0, q1) = getAt (0, 1) order (wQi w)
                    lum' = wLum w ++ [((q0 + q1) / 2, wMid w)]
                    w' = newMid w { wLum = lum', wQi = wQi w + 1 }
                in w' { wFit = fitGamma lum' }

-- 感じ S = (I^γ − I₀^γ) ÷ (I₁^γ − I₀^γ)。γ → 0 の極限が対数（フェヒナー）、γ = 1 が物理的に等間隔
senseOf :: Double -> Double -> Double
senseOf i g
  | abs g < 1e-6 = log (i / lLo) / log (lHi / lLo)
  | otherwise = (i ** g - lLo ** g) / (lHi ** g - lLo ** g)

-- senseOf の逆
lumOf :: Double -> Double -> Double
lumOf s0 g
  | abs g < 1e-6 = lLo * (lHi / lLo) ** s
  | otherwise = (lLo ** g + s * (lHi ** g - lLo ** g)) ** (1 / g)
  where s = clampv 0 1 s0

-- 自分の点 (I, S) に γ を当てはめる（二乗誤差が最小の γ を黄金分割探索で）
fitGamma :: [(Double, Double)] -> Maybe FitG
fitGamma m
  | null pts = Nothing
  | otherwise = Just (FitG g (sqrt (err g / fromIntegral (length pts))))
  where
    pts = [ (i, s) | (s, i) <- m, s > 0, s < 1 ]
    err gm = foldl' (\e (i, s) -> e + (senseOf i gm - s) ^ (2 :: Int)) 0 pts
    r = (sqrt 5 - 1) / 2
    go a b (0 :: Int) = (a + b) / 2
    go a b n = let c = b - r * (b - a)
                   d = a + r * (b - a)
               in if err c < err d then go a d (n - 1) else go c b (n - 1)
    g = go 0.001 1.5 80

-- シミュレーション観察者：感じの真ん中 (Sa + Sb)/2 にゆらぎを足し、その感じの明るさを選ぶ
simPick :: Double -> Double -> Rand Double
simPick a b = do
  e <- rndNormal
  pure (lumOf ((senseOf a simGamma + senseOf b simGamma) / 2 + simNoise * e) simGamma)

-- 7 つの区間を順に二分する
simScale :: Rand [(Double, Double)]
simScale = go lum0 order
  where go m [] = pure m
        go m ((q0, q1) : qs) = do v <- simPick (lumAt m q0) (lumAt m q1)
                                  go (m ++ [((q0 + q1) / 2, v)]) qs

runSim :: Word32 -> World -> World
runSim seed w0 = w { wLum = m, wQi = length order, wFit = fitGamma m, wRng = r' }
  where
    w = resetData True (seedRng 1) w0
    (m, r') = runRand simScale (seedRng seed)

-- 光の量 → 画面の灰色
grayI :: Double -> Color
grayI i = gray255 (f2 (linToSrgb i))

-- L* の範囲（暗い所は近似）と、L* → 光の量
lerpL :: Double -> Double
lerpL t = let a = 116 * (lLo ** (1 / 3)) - 16 in a + t * (100 - a)

lstarToY :: Double -> Double
lstarToY ls = ((ls + 16) / 116) ^ (3 :: Int)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Left/Right fine   Up/Down coarse   Space: this is the middle   O simulated observer   R reset" 20 32 17 (gray255 235)
  , say "JND grows with the light (Weber) -> 'sensation' counted in JNDs is a log (Fechner). Check it with your eyes." 20 58 14 (gray255 170) ]
    -- ---- 3 つの階段
  ++ concat [ say nm 20 (y + fromIntegral k * 78 + 14) 14 (gray255 200)
              : [ Color (grayI (f (fromIntegral i / fromIntegral (steps - 1)))) (rectTL (20 + fromIntegral i * sw) (y + fromIntegral k * 78 + 22) sw 46) | i <- [0 .. steps - 1] ]
            | (k, (nm, f)) <- zip [0 :: Int ..] rows ]
    -- ---- JND の階段 → 対数（左下）
  ++ [ plotAxes g1 "Light I" "Number of JNDs n" [0, 0.25, 0.5, 0.75, 1] [0, roundJ (gy1 g1 / 2), fromIntegral (floor (gy1 g1) :: Int)]
     , Color (rgb255 120 190 255) (polyline 2 jndStairs)
     , Color (rgb255 255 170 90) (polyline 2.5 [ (gX g1 x, gY g1 (log (x / lLo) / log (1 + jndK))) | i <- [0 .. 200 :: Int], let x = lLo + fromIntegral i / 200 * (lHi - lLo) ])
     , say ("Blue: one step each time I grows x(1 + k) (k = " ++ show jndK ++ ")") (gx g1) (gy g1 + gh g1 + 66) 14 (gray255 180)
     , say "Orange: n = ln(I/I0) / ln(1 + k)" (gx g1) (gy g1 + gh g1 + 84) 14 (gray255 180) ]
    -- ---- 二分法の実験（右上）
  ++ [ Color (grayI 0.18) (rectTL (ex - 20) (ey - 20) (pw * 3 + 80) (pw + 40)) ]   -- まわりは一定の灰色
  ++ (if wQi w < length order
        then let (q0, q1) = getAt (0, 1) order (wQi w)
             in [ Color (grayI (lumAt (wLum w) q0)) (rectTL ex ey pw pw)
                , Color (grayI (wMid w)) (rectTL (ex + pw + 20) ey pw pw)
                , Color (grayI (lumAt (wLum w) q1)) (rectTL (ex + 2 * pw + 40) ey pw pw)
                , say ((if wSim w then "" else "Make the middle look halfway   ") ++ show (wQi w + 1) ++ " / " ++ show (length order)
                       ++ "  (S " ++ prec3 q0 ++ " to " ++ prec3 q1 ++ ")") (ex - 20) (ey + pw + 46) 14 (gray255 220) ]
        else [ say (if wSim w then "Result of the simulated observer" else "Done! This is your brightness scale. R to try again") (ex - 20) (ey + pw + 46) 15 (gray255 220) ])
    -- ---- 自分の尺度のグラフ（右下）
  ++ [ plotAxes g2 "Light I" "Sensation S (0 to 1)" [0, 0.25, 0.5, 0.75, 1] [0, 0.25, 0.5, 0.75, 1]
     , curve2 1 (gray255 110) 1.5, curve2 0 (rgb255 200 120 255) 1.5 ]
  ++ [ curve2 simGamma (rgb255 120 200 140) 1.5 | wSim w ]
  ++ [ curve2 (fgG f) (rgb255 255 170 90) 3 | Just f <- [wFit w] ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g2 i) (gY g2 s) 10) | (s, i) <- wLum w ]
  ++ [ say (case wFit w of
              Just f -> "Fit: S ~ I^gamma   gamma = " ++ fmt 3 (fgG f) ++ " (error " ++ fmt 3 (fgRms f) ++ ")"
              Nothing -> "Each Space adds a point") (gx g2) (gy g2 + gh g2 + 62) 16 (gray255 235)
     , say "Gray: gamma = 1 (equal steps of light)" (gx g2) (gy g2 + gh g2 + 86) 14 (gray255 170)
     , say "Purple: gamma -> 0 (log = Fechner)" (gx g2) (gy g2 + gh g2 + 104) 14 (rgb255 200 120 255) ]
  ++ [ say ("Green: true gamma = " ++ show simGamma) (gx g2) (gy g2 + gh g2 + 122) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    sw = min 70 (envW env * 0.5 / fromIntegral steps)
    y = 84
    rows = [ ("Equal steps of light", \t -> lLo + t * (lHi - lLo))
           , ("Equal steps of log (Fechner)", \t -> lLo * (lHi / lLo) ** t)
           , ("Equal steps of L* (power 1/3)", lstarToY . lerpL) ]
    g1 = Graph 60 (y + 270) (envW env * 0.42) (envH env - y - 420) 0 lHi 0 (log (lHi / lLo) / log (1 + jndK)) False False
    -- I を 1 + k 倍するごとに 1 段
    jndStairs = concat (go lLo 0)
      where go i n | i * (1 + jndK) <= lHi = let i' = i * (1 + jndK)
                                                in [(gX g1 i, gY g1 n), (gX g1 i', gY g1 n), (gX g1 i', gY g1 (n + 1))] : go i' (n + 1)
                   | otherwise = []
    ex = envW env * 0.56
    ey = 100
    pw = 110
    g2 = Graph (ex + 20) (ey + pw + 90) (envW env * 0.38) (envH env - ey - pw - 270) 0 1 0 1 False False
    curve2 gm c wt = Color c (polyline wt [ (gX g2 x, gY g2 (senseOf x gm)) | i <- [0 .. 200 :: Int], let x = lLo + (fromIntegral i / 200) ^ (2 :: Int) * (lHi - lLo) ])

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wMid = clampv lLo lHi (wMid w / 1.02) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wMid = clampv lLo lHi (wMid w * 1.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wMid = clampv lLo lHi (wMid w / 1.15) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wMid = clampv lLo lHi (wMid w * 1.15) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (accept w)
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("sRGB <-> light: 0 -> 0, 255 -> 1, 128 -> 0.216 -> 128"
    , near (srgbToLin 0) 0 1e-9 && near (srgbToLin 255) 1 1e-9 && near (srgbToLin 128) 0.2158605 1e-6 && near (linToSrgb (srgbToLin 128)) 128 1e-9)
  , ("n JNDs stacked give I0 (1 + k)^n: n = floor(ln(I1/I0) / ln 1.1) = " ++ show n
    , n == floor (log (lHi / lLo) / log 1.1) && near iN (lLo * 1.1 ^ n) 1e-12)
  , ("gamma -> 0 is the log: S(gamma = 0.0001) ~ S(log)", near (senseOf 0.07 1e-4) (senseOf 0.07 0) 1e-3)
  , ("lumOf is the inverse of senseOf (gamma = 0.4, S = 0.3)", near (senseOf (lumOf 0.3 0.4) 0.4) 0.3 1e-12)
  , ("noise-free observer (gamma = 0.4): 7 points give back gamma = 0.400", maybe False (\f -> near (fgG f) 0.4 1e-3) (fitGamma exact))
  , ("simulated observer (noise 0.02): gamma ~ 0.4 (+-0.1)", maybe False (\f -> near (fgG f) 0.4 0.1) (fitGamma (fst (runRand simScale (seedRng 52)))))
  , ("log steps have a constant ratio (same ratio between neighbours)"
    , near (lLo * (lHi / lLo) ** (2 / 7) / (lLo * (lHi / lLo) ** (1 / 7))) ((lHi / lLo) ** (1 / 7)) 1e-9) ]
  where
    (iN, n) = until (\(i, _) -> not (i * 1.1 <= lHi)) (\(i, k) -> (i * 1.1, k + 1)) (lLo, 0 :: Int)
    exact = lum0 ++ [ ((q0 + q1) / 2, lumOf ((q0 + q1) / 2) 0.4) | (q0, q1) <- order ]

-- ---------- やってみよう ----------
-- 1. 3 つの階段のうち、段の差がいちばん「そろって」見えるのはどれ？
-- 2. 7 回の二分法で自分の γ を出そう。0（対数）と 1（物理的に等間隔）のどちらに近い？
-- 3. まわりの灰色（grayI 0.18）を黒や白に変えると、真ん中の選び方は変わる？
-- 4. jndK を 0.05 にすると左下の階段は何段になる？ 実際の明るさの JND（1〜2%）なら？
-- 5. 画面の明るさ設定を変えて測り直すと γ は変わる？ 画面ごとのちがいも考えよう

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

-- ---- 色の道具：画面の値（sRGB 0〜255）⇔ 光の量（0〜1）
srgbToLin :: Double -> Double
srgbToLin c0 = let c = c0 / 255 in if c <= 0.04045 then c / 12.92 else ((c + 0.055) / 1.055) ** 2.4

linToSrgb :: Double -> Double
linToSrgb v0 = let v = clampv 0 1 v0 in 255 * (if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055)
