-- ============================================
-- 52-00 ウェーバーの法則 ─ 違いがわかる最小の差（弁別閾 JND）は、基準に比例する？
-- 左右の点の数（または線の長さ）のどちらが多いかを答える。基準の大きさ 4 段階それぞれで上下法を同時に進め（交互配置）、JND を測る
-- JND と基準のグラフが原点を通る直線 ΔI = k·I になれば、ウェーバーの法則。k = ウェーバー比
-- 操作：スペースで開始 / ← = 左が多い（長い） → = 右 / 1 = 点の数  2 = 線の長さ / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

bases :: Int -> [Double]
bases 1 = [10, 20, 40, 80]           -- ★ 基準の大きさ（点の数）
bases _ = [60, 110, 160, 210]        -- ★ 基準の大きさ（たての線の長さ px）

per :: Int
per = 10                             -- ★ 基準 1 つあたりの試行（4 × 10 = 40 試行）

showT :: Float
showT = 0.9                          -- ★ 点を見せる時間 [秒]（数えられないように短く）

startR, simSigma :: Int -> Double
startR t = if t == 1 then 0.3 else 0.15      -- ★ 差 Δ のはじめの大きさ（基準に対する比）
simSigma t = if t == 1 then 0.2 else 0.05    -- ★ シミュレーション観察者の「感じ方のゆらぎ」（対数でのばらつき）

data Phase = Ready | Stim | Wait | Gap | Done deriving Eq

-- いまの試行：どの階段か、基準 I、差 d、大きい方が右か、刺激の絵（試行の間は変わらない）
data Trial = Trial { trSi :: !Int, trI, trD :: !Double, trBigRight :: !Bool, trPic :: Picture }

-- 結果：基準と JND の組、ウェーバー比 k、切片ありの直線
data Result = Result { rIs, rDs :: [Double], rK :: Maybe Double, rLin :: Maybe Lin }

data World = World
  { wTask   :: !Int              -- 1 = 点の数、2 = 線の長さ
  , wStairs :: [Stair]
  , wPhase  :: !Phase
  , wT      :: !Float            -- いまの段階が始まった時刻 [秒]
  , wCur    :: Maybe Trial
  , wSim    :: !Bool
  , wSimSeed :: !Word32
  , wRng    :: !Rng
  , wResult :: Maybe Result
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-00 Weber's law" world0)
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
world0 = resetData False (seedRng 1) (World 1 [] Ready 0 Nothing False 1 (seedRng 1) Nothing [])

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wPhase = Ready, wResult = Nothing, wRng = r, wCur = Nothing
                      , wStairs = [ newStair (wTask w) i | i <- bases (wTask w) ] }

-- 差 Δ は基準の 30%（線は 15%）から
newStair :: Int -> Double -> Stair
newStair task i = makeStair (i * startR task) 1.3 2 1 (i * (if task == 1 then 1.5 else 0.5)) True

-- 見せる差（点の数は整数）
deltaShown :: Int -> Stair -> Double
deltaShown task st = if task == 1 then max 1 (roundJ (stV st)) else stV st

-- 刺激の絵の大きさ（p5.js 版の pg）
pgSize :: Env -> (Float, Float)
pgSize env = (fromIntegral (floor (envW env * 0.5) :: Int), fromIntegral (floor (envH env * 0.55) :: Int))

startTrial :: Env -> World -> World
startTrial env w
  | null cand = w { wPhase = Done }
  | otherwise = w { wCur = Just tr, wPhase = Stim, wT = envTime env, wRng = r' }
  where
    sts = wStairs w
    cand0 = [ i | (i, s) <- zip [0 ..] sts, stN s < per ]
    m = minimum (map (stN . (sts !!)) cand0)
    cand = filter (\i -> stN (sts !! i) == m) cand0
    task = wTask w
    (tr, r') = flip runRand (wRng w) $ do
      u <- rnd
      let si = cand !! floor (u * fromIntegral (length cand))
          i = bases task !! si
          d = deltaShown task (sts !! si)
      br <- fmap (< 0.5) rnd
      pic <- drawStim (pgSize env) task i d br
      pure (Trial si i d br pic)

-- 刺激を 1 回だけ作る（試行の間は変わらない。左上が原点の座標）
drawStim :: (Float, Float) -> Int -> Double -> Double -> Bool -> Rand Picture
drawStim (pw, ph) task i d bigRight = do
  sides <- mapM side [0, 1 :: Int]
  pure (Pictures (Color (rgb255 30 32 40) (rectTL 0 0 pw ph) : sides))
  where
    hw = pw / 2
    side s = do
      let big = (s == 1) == bigRight
          v = if big then i + d else i
          cx = hw * fromIntegral s + hw / 2
          cy = ph / 2
      if task == 1
        then do
          let rr = min hw ph * 0.42
              -- 重ならないように点を置く（2 万回まで試す）
              go pts n tries
                | n >= v || tries >= (20000 :: Int) = pure pts
                | otherwise = do
                    a <- rnd
                    q <- rnd
                    let r = rr * f2 (sqrt q)
                        x = cx + r * cos (f2 (a * 2 * pi))
                        y = cy + r * sin (f2 (a * 2 * pi))
                    if all (\(px, py) -> (px - x) ^ (2 :: Int) + (py - y) ^ (2 :: Int) > 81) pts
                      then go ((x, y) : pts) (n + 1) (tries + 1)
                      else go pts n (tries + 1)
          pts <- go [] 0 0
          pure (Color (gray255 235) (Pictures [ circleD x y 6 | (x, y) <- pts ]))
        else do
          -- たての線（上下の位置は少しずらす）
          a <- rnd
          b <- rnd
          let x = cx + f2 (a - 0.5) * hw * 0.5
              y0 = cy - f2 v / 2 + f2 (b - 0.5) * 40
          pure (Color (gray255 235) (thickLine 5 (x, y0) (x, y0 + f2 v)))

answer :: Env -> Bool -> World -> World
answer env right w = case wCur w of
  Just c | wPhase w == Stim || wPhase w == Wait ->
    computeResult w { wStairs = [ if k == trSi c then stairUpdate s (right == trBigRight c) else s | (k, s) <- zip [0 ..] (wStairs w) ]
                    , wPhase = Gap, wT = envTime env }
  _ -> w

-- 4 つの階段のしきい値（JND）→ 原点を通る直線 ΔI = k·I を最小二乗で（k = ΣIΔ ÷ ΣI²）
computeResult :: World -> World
computeResult w = w { wResult = Just (Result is ds (if length is >= 2 then weberK is ds else Nothing) (if length is >= 2 then linFit is ds else Nothing)) }
  where
    pairs = [ (i, t) | (i, s) <- zip (bases (wTask w)) (wStairs w), stN s >= 4, Just t <- [stairThreshold s 2], t /= 0 ]
    is = map fst pairs
    ds = map snd pairs

weberK :: [Double] -> [Double] -> Maybe Double
weberK is ds = let a = sum (zipWith (*) is ds); b = sum (map (^ (2 :: Int)) is) in if b > 0 then Just (a / b) else Nothing

-- シミュレーション観察者：感じる大きさ = I × e^(σ·ゆらぎ)。大きく感じたほうを答える
simAnswer :: Double -> Double -> Double -> Rand Bool
simAnswer i d sg = do
  e1 <- rndNormal
  e2 <- rndNormal
  pure ((i + d) * exp (sg * e1) > i * exp (sg * e2))

-- 正答率 70.7% になる Δ/I
trueK :: Double -> Double
trueK sg = exp (sqrt 2 * sg * zOf (sqrt 0.5)) - 1

-- 階段を n 回、シミュレーション観察者で進める（diff = 見せる差の決め方）
simStair :: Int -> Double -> (Stair -> Double) -> Double -> Stair -> Rand Stair
simStair n i diff sg st0 = go n st0
  where go 0 st = pure st
        go k st = do ok <- simAnswer i (diff st) sg
                     go (k - 1) (stairUpdate st ok)

runSim :: Int -> Word32 -> World -> World
runSim n seed w0 = computeResult w { wStairs = sts, wRng = r', wPhase = Done }
  where
    w = resetData True (seedRng seed) w0
    task = wTask w
    (sts, r') = runRand (mapM (\(i, s) -> simStair n i (deltaShown task) (simSigma task) s) (zip (bases task) (wStairs w))) (wRng w)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && envTime env - wT w > 0.5 = pure (startTrial env w)
  | wPhase w == Stim && wTask w == 1 && envTime env - wT w > showT = pure w { wPhase = Wait }   -- 点は短く見せて消す
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   Left = left " ++ more ++ "   Right = right " ++ more) 20 30 18 (gray255 235)
  , say ("1 dots / 2 line length (now: " ++ (if task == 1 then "dots" else "line length") ++ ")   O simulated observer   R reset") 20 54 16 (gray255 235)
  , say "Staircase: 2 correct in a row -> smaller difference, 1 wrong -> larger. Converges on 70.7% correct = JND." 20 78 14 (gray255 170)
  , say "The 4 base sizes are measured at the same time in mixed order." 20 96 14 (gray255 170) ]
  ++ stimArea
    -- ---- 階段の表
  ++ [ say h (sx + cx) ty 14 (gray255 170) | (h, cx) <- zip ["Base I", "Trials", "Delta now", "Revs", "JND (threshold)"] cols ]
  ++ concat [ [ say s (sx + cx) (ty + 22 + fromIntegral k * 20) 14 (gray255 215) | (s, cx) <- zip (row i st) cols ]
            | (k, i, st) <- zip3 [0 :: Int ..] (bases task) (wStairs w) ]
  ++ [ say (if wSim w then "Simulated observer (" ++ show (stN (getAt (makeStair 0 1 1 0 0 False) (wStairs w) 0)) ++ " trials each)" else "You are the observer") sx (ty + 112) 15 (gray255 200) ]
    -- ---- JND と基準のグラフ
  ++ [ plotAxes g "Base size I" "JND Delta I" (0 : bases task) [0, gy1 g / 2, gy1 g] ]
  ++ [ Color (rgba255 120 200 140 170) (thickLine 1.5 (gX g 0, gY g 0) (gX g imax, gY g (trueK (simSigma task) * imax))) | wSim w ]
  ++ [ Color (rgb255 255 170 90) (thickLine 3 (gX g 0, gY g 0) (gX g imax, gY g (k * imax))) | Just k <- [rK =<< wResult w] ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g i) (gY g d) 11) | Just res <- [wResult w], (i, d) <- zip (rIs res) (rDs res) ]
  ++ resultText
  ++ [ say ("True k = " ++ fmt 3 (trueK (simSigma task)) ++ " (sigma = " ++ show (simSigma task) ++ ")") rx (ry + 74) 15 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    task = wTask w
    more = if task == 1 then "more" else "longer"
    sx = 20
    sy = 112
    (pw, ph0) = pgSize env
    ph = min ph0 (envH env * 0.52)
    stimArea = case (wPhase w, wCur w) of
      (Stim, Just c) -> [ Translate sx sy (trPic c) ]
      _ -> [ Color (gray255 70) (frameTL 1 sx sy pw ph)
           , sayC msg (sx + pw / 2) (sy + ph / 2) 16 (gray255 200) ]
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show (length (bases task) * per) ++ " trials)"
      Wait -> "Which side had more? (Left / Right)"
      Done -> if wSim w then "Result of the simulated observer" else "Well done! R to try again"
      Gap -> "+"
      Stim -> ""
    ty = sy + ph + 34
    cols = [0, 90, 170, 280, 350]
    row i st = [ fmt 0 i, show (stN st), fmt (if task == 1 then 0 else 1) (deltaShown task st), show (length (stRevs st))
               , case (if stN st >= 4 then stairThreshold st 2 else Nothing) of
                   Just t -> fmt 1 t ++ " (" ++ fmt 1 (t / i * 100) ++ "%)"
                   Nothing -> "-" ]
    imax = getAt 0 (bases task) 3 * 1.1
    g = Graph (envW env * 0.62) 130 (envW env * 0.34) (envH env * 0.46) 0 imax 0 (imax * (if task == 1 then 0.4 else 0.12)) False False
    rx = gx g - 40
    ry = gy g + gh g + 70
    resultText = case wResult w of
      Just res | Just k <- rK res ->
        [ say "Delta I = k * I" rx ry 17 (gray255 235)
        , say ("Weber ratio k = " ++ fmt 3 k ++ " (a " ++ fmt 1 (k * 100) ++ "% change is noticed)") rx (ry + 24) 16 (gray255 235) ]
        ++ [ say ("With intercept: " ++ fmt 2 (linA l) ++ " + " ++ fmt 3 (linB l) ++ " * I (intercept ~ 0 = Weber)") rx (ry + 48) 14 (gray255 180) | Just l <- [rLin res] ]
      _ -> [ say "Answer 4 trials at 2 or more base sizes" rx ry 16 (gray255 235)
           , say "to fit the line" rx (ry + 22) 16 (gray255 235) ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready -> pure (startTrial env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env True w)
  EventKey (Char c) Down _ _
    | c `elem` "12" -> do r <- freshSeed
                          pure (resetData False r w { wTask = if c == '1' then 1 else 2 })
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim 150 s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("line through the origin: data Delta I = 0.1 I gives k = 0.1", maybe False (\k -> near k 0.1 1e-9) (weberK [10, 20, 40] [1, 2, 4]))
  , ("1-up-2-down converges where p^2 = 0.5 (p = 0.7071)", near (sqrt 0.5 ^ (2 :: Int)) 0.5 1e-9 && near (phi (zOf (sqrt 0.5))) 0.7071 1e-4)
  , ("sigma = 0 gives true k = 0 (no noise: any difference is seen)", near (trueK 0) 0 1e-9)
  , ("staircase: right right -> 1 step down (10 -> 7.69), wrong -> 1 step up (-> 10), 1 reversal"
    , near (fst (getAt (0, False) (reverse (stTrack st)) 2)) (10 / 1.3) 1e-9 && near (stV st) 10 1e-9 && length (stRevs st) == 1)
  , ("simulated observer (sigma = 0.2): estimated k ~ true k " ++ fmt 3 (trueK 0.2) ++ " (+-25%)", maybe False (\k -> near k (trueK 0.2) (0.25 * trueK 0.2)) k1)
  , ("line length (sigma = 0.05): estimated k ~ " ++ fmt 3 (trueK 0.05) ++ " (+-25%)", maybe False (\k -> near k (trueK 0.05) (0.25 * trueK 0.05)) k2) ]
  where
    st = foldl' stairUpdate (makeStair 10 1.3 2 1 100 False) [True, True, False]
    estimate task sg seed diff =
      let is = bases task
          sts = fst (runRand (mapM (\i -> simStair 400 i (diff task) sg (newStair task i)) is) (seedRng seed))
      in weberK is [ maybe 0 id (stairThreshold s 4) | s <- sts ]
    k1 = estimate 1 0.2 520 deltaShown
    k2 = estimate 2 0.05 521 (\_ s -> stV s)

-- ---------- やってみよう ----------
-- 1. 点の数で 40 試行。自分の k はいくつ？ 線の長さ（2）の k と比べると、どちらが細かく見分けられる？
-- 2. グラフの点は原点を通る直線に乗った？ 基準が小さいところ（10 個）では法則からずれていない？
-- 3. showT を 0.3 にすると k はどう変わる？ 長くすると数えられてしまうのはなぜ？
-- 4. O でシミュレーション観察者を何回か走らせ、推定 k のばらつきを見よう。per を増やすとどうなる？
-- 5. 重さや音の大きさのウェーバー比を調べて、ここで測った値と比べよう

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
