-- ============================================
-- 52-12 音の高さの弁別 🔊 ─ どれくらいの差なら「高い」とわかる？
-- 2 つの音が続けて鳴る。高かったのは 1 つ目（←）か 2 つ目（→）か（2 肢強制選択）
-- 周波数の差 Δf を上下法（1-up-2-down、正答率 70.7%）で小さくしていき、弁別閾 Δf とウェーバー比 Δf/f を求める
-- 基準の周波数（↑↓）を変えて測ると、Δf/f はほぼ一定（0.2〜1% くらい）になるか確かめる
-- 操作：スペース = 開始 / ← 1 つ目  → 2 つ目 / ↑↓ = 基準の周波数 / O = シミュレーション観察者 / R = やり直し
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

bases :: [Double]
bases = [250, 500, 1000, 2000]       -- ★ 基準の周波数 [Hz]

nTrials :: Int
nTrials = 30                         -- ★ 1 つの基準あたりの試行

vol, toneLen, gap, simSigma :: Double
vol = 0.3                            -- ★ 音の大きさ（0〜1）
toneLen = 0.3                        -- ★ 音の長さ [秒]
gap = 0.35                           -- ★ 音の間 [秒]
simSigma = 0.004                     -- ★ シミュレーション観察者：1 つの音の高さの感じ方のゆらぎ（対数）

data Phase = Ready | Play | Wait | Gap | Done deriving Eq

data World = World
  { wBi      :: !Int                -- 基準の番号
  , wSt      :: Stair               -- いまの階段（値 = Δf/f）
  , wResults :: M.Map Int Double    -- 基準ごとのしきい値 Δf/f
  , wPhase   :: !Phase
  , wT       :: !Float              -- いまの段階が始まった時刻 [秒]
  , wBusy    :: !Float              -- この時刻までは次を始めない
  , wSecond  :: !Bool               -- 2 つ目が高いか
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wNotes   :: [Note]
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-12 Pitch discrimination" world0)
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

-- Δf/f を 3% から
newStair :: Stair
newStair = makeStair 0.03 1.3 2 0.0005 0.3 True

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wSt = newStair, wResults = M.empty, wPhase = Ready, wRng = r }

baseF :: World -> Double
baseF w = getAt 1000 bases (wBi w)

startTrial :: Env -> World -> World
startTrial env w
  | stN (wSt w) >= nTrials = w { wResults = maybe id (M.insert (round (baseF w))) (stairThreshold (wSt w) 2) (wResults w), wPhase = Done }
  | otherwise = w { wSecond = sec, wBusy = now + realToFrac (2 * toneLen + gap + 0.2), wRng = r'
                  , wNotes = wNotes w ++ [ Note (now + 0.1) (if sec then f else hi) toneLen vol 0
                                         , Note (now + 0.1 + realToFrac (toneLen + gap)) (if sec then hi else f) toneLen vol 0 ]
                  , wPhase = Play, wT = now }
  where
    now = envTime env
    ((f, sec), r') = flip runRand (wRng w) $ do
      u <- rnd    -- 基準も少しずつずらす（前の音を覚えて比べないように）
      s <- rnd
      pure (baseF w * exp ((u - 0.5) * 0.1), s < 0.5)
    hi = f * (1 + stV (wSt w))

answer :: Env -> Bool -> World -> World
answer env second w
  | wPhase w /= Wait = w
  | otherwise = w { wSt = stairUpdate (wSt w) (second == wSecond w), wPhase = Gap, wT = envTime env }

-- シミュレーション観察者：それぞれの音の高さ（対数）にゆらぎ σ。正答率 = Φ(ln(1 + δ) ÷ (√2 σ))
simCorrect :: Double -> Rand Bool
simCorrect d = do
  a <- rndNormal
  b <- rndNormal
  pure (log (1 + d) + simSigma * a - simSigma * b > 0)

trueDelta :: Double
trueDelta = exp (sqrt 2 * simSigma * zOf (sqrt 0.5)) - 1

simStair :: Int -> Rand Stair
simStair n = go n newStair
  where go 0 s = pure s
        go k s = do ok <- simCorrect (stV s)
                    go (k - 1) (stairUpdate s ok)

runSim :: Word32 -> World -> World
runSim seed w0 = w { wResults = M.fromList [ (round f, t) | (f, Just t) <- zip bases ths ], wSt = st, wPhase = Done, wRng = r' }
  where
    w = resetData True (wRng w0) w0
    ((ths, st), r') = flip runRand (seedRng seed) $ do
      t <- mapM (const (fmap (`stairThreshold` 2) (simStair 80))) bases
      s <- simStair 80
      pure (t, s)

step :: Env -> Float -> World -> IO World
step env _ w = do
  ns <- playDue (envTime env) (wNotes w)
  let w1 = w { wNotes = ns }
      el = envTime env - wT w1
  pure $ if wPhase w1 == Gap && el > 0.7 then startTrial env w1
         else if wPhase w1 == Play && el > realToFrac (toneLen * 2 + gap + 0.1) then w1 { wPhase = Wait }
         else w1

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   Left = 1st higher   Right = 2nd higher   Up/Down base " ++ fmt 0 (baseF w) ++ " Hz   O simulated observer   R reset") 20 32 16 (gray255 235)
  , say "Keep the volume low, especially with headphones. Always answer, even when unsure." 20 60 15 (rgb255 255 200 120)
    -- 1 つ目・2 つ目の表示（鳴っている間だけ明るく。1 試行に 2 回だけ）
  , Color (if lit 0.1 then rgb255 255 220 120 else gray255 55) (circleD (bx + 40) (by + 40) 50)
  , Color (if lit (0.1 + tl + gp) then rgb255 255 220 120 else gray255 55) (circleD (bx + 140) (by + 40) 50)
  , sayC "1" (bx + 40) (by + 48) 18 (gray255 30)
  , sayC "2" (bx + 140) (by + 48) 18 (gray255 30)
  , say msg bx (by + 110) 16 (gray255 215)
  , say ((if wSim w then "Simulated observer" else "You") ++ "   base " ++ fmt 0 (baseF w) ++ " Hz   trial " ++ show (stN st)
         ++ "   now Df/f = " ++ fmt 2 (stV st * 100) ++ "% (" ++ fmt 1 (baseF w * stV st) ++ " Hz)") bx (by + 136) 16 (gray255 215)
    -- ---- 階段（左下）
  , plotAxes g1 "Trial" "Df/f (log)" [0, gx1 g1] [0.001, 0.01, 0.1]
  , Color (gray255 120) (polyline 1.5 [ (gX g1 (fromIntegral i), gY g1 v) | (i, (v, _)) <- zip [0 :: Int ..] tr ]) ]
  ++ [ Color (if ok then rgb255 120 210 150 else rgb255 255 100 100) (circleD (gX g1 (fromIntegral i)) (gY g1 v) 6) | (i, (v, ok)) <- zip [0 :: Int ..] tr ]
    -- ---- Δf/f と基準の周波数（右）
  ++ [ plotAxes g "Base frequency [Hz]" "Weber ratio Df/f (log)" bases [0.001, 0.003, 0.01, 0.03, 0.1] ]
  ++ [ Color (rgb255 120 200 140) (thickLine 1.5 (gx g, gY g trueDelta) (gx g + gw g, gY g trueDelta)) | wSim w ]
  ++ concat [ case M.lookup (round f) (wResults w) of
                Just r | r > 0 -> [ Color (rgb255 120 190 255) (circleD (gX g f) (gY g r) 13)
                                  , say (fmt 0 f ++ " Hz: Df = " ++ fmt 2 (f * r) ++ " Hz, Df/f = " ++ fmt 2 (r * 100) ++ "%") (gx g) (ry + fromIntegral i * 22) 15 (gray255 220) ]
                _ -> [ say (fmt 0 f ++ " Hz: not yet") (gx g) (ry + fromIntegral i * 22) 15 (gray255 130) ]
            | (i, f) <- zip [0 :: Int ..] bases ]
  ++ [ say ("True 70.7% point: Df/f = " ++ fmt 3 (trueDelta * 100) ++ "% (same at every frequency)") (gx g) (ry + 96) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    bx = 20
    by = 100
    st = wSt w
    tr = reverse (stTrack st)
    el = envTime env - wT w
    tl = realToFrac toneLen
    gp = realToFrac gap
    lit t0 = wPhase w == Play && el > t0 && el < t0 + tl
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show nTrials ++ " trials)"
      Wait -> "Which was higher? (Left = 1st, Right = 2nd)"
      Done -> if wSim w then "Simulated observer (80 trials each, no sound)" else "Finished! Up/Down for another base -> space"
      _ -> ""
    g1 = Graph (bx + 60) (by + 190) (envW env * 0.42) (envH env - by - 310) 0 (fromIntegral (max nTrials (stN st))) 0.0005 0.3 False True
    g = Graph (envW env * 0.6) 120 (envW env * 0.35) (envH env * 0.45) 180 2800 0.0005 0.1 True True
    ry = gy g + gh g + 70

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _
    | (wPhase w == Ready || wPhase w == Done) && envTime env > wBusy w ->
        pure (startTrial env (if wPhase w == Done || wSim w then w { wSim = False, wSt = newStair } else w))
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env True w)
  EventKey (SpecialKey k) Down _ _
    | (k == KeyUp || k == KeyDown) && (wPhase w == Ready || wPhase w == Done) ->
        pure w { wBi = max 0 (min (length bases - 1) (wBi w + (if k == KeyUp then 1 else -1))), wSt = newStair, wPhase = Ready }
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> do r <- freshSeed
                          let s = wSimSeed w + 1
                          pure (runSim s w { wSimSeed = s, wRng = r })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（音は鳴らさない。p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("at the true 70.7% point, Phi(ln(1 + d)/(sqrt2 sigma)) = 0.7071", near (phi (log (1 + d) / (sqrt 2 * simSigma))) (sqrt 0.5) 1e-5)
  , ("d = 0 gives 0.5 correct (guessing)", near (phi 0) 0.5 1e-6)
  , ("Df/f = 0.5% at 1000 Hz -> the tones are 1000 Hz and 1005 Hz", near (1000 * (1 + 0.005)) 1005 1e-9)
  , ("simulated observer 80 trials x 40: mean threshold ~ true 70.7% point (within one x1.3 step)", abs (log (m / d)) < log 1.3)
  , ("volume limit: one tone <= 1, master <= 0.2", vol <= 1 && master <= 0.2) ]
  where
    d = trueDelta
    th = fst (runRand (mapM (const (fmap (\s -> log (maybe 0 id (stairThreshold s 2))) (simStair 80))) [1 .. 40 :: Int]) (seedRng 52))
    m = exp (foldl1 (+) th / fromIntegral (length th))

-- ---------- やってみよう ----------
-- 1. 1000 Hz で 30 試行。自分の Δf は何 Hz？ Δf/f は何 %？
-- 2. ↑↓ で 250 Hz と 2000 Hz も測ろう。Δf/f はだいたい同じ？（ウェーバーの法則）
-- 3. 楽器をやっている人とやっていない人で比べてみよう。半音（約 6%）よりずっと小さい？
-- 4. toneLen を 0.1 秒にすると、しきい値はどう変わる？（短い音ほど高さがあいまい）
-- 5. O でシミュレーション観察者の階段と、自分の階段のゆれ方を比べよう

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

