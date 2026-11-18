-- ============================================
-- 52-10 フィッツの法則 ─ 遠くて小さいものほど、指すのに時間がかかる
-- 出てきた円（ターゲット）をできるだけ速く正確にクリックする。距離 D と幅 W を 3 × 3 通りに変える
-- 難しさ ID = log₂(D/W + 1) [bit]（シャノン形）。移動時間 MT = a + b·ID を回帰し、スループット = ID ÷ MT [bit/秒] を出す
-- 操作：灰色の円をクリックして開始 / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

ds, ws :: [Double]
ds = [150, 300, 500]                 -- ★ 距離 D [px]
ws = [16, 32, 64]                    -- ★ 幅 W [px]（円の直径）

reps :: Int
reps = 4                             -- ★ 1 条件の回数（9 × 4 = 36 回）

simA, simB, simSd, simErr :: Double
simA = 150                           -- ★ シミュレーション観察者：MT = a + b·ID [ms]
simB = 120
simSd = 40
simErr = 0.04

data Phase = Ready | Go | Done deriving Eq

-- 1 回の記録：距離・幅・ID・移動時間 [ms]・当たったか
data TrialF = TrialF { tD, tW, tId, tMt :: !Double, tHit :: !Bool }

-- ターゲット（画面の位置、D と W、画面での直径）
data Tgt = Tgt { tgX, tgY :: !Double, tgD, tgW, tgSize :: !Double }

data World = World
  { wOrder   :: [(Double, Double)]   -- (D, W) の順番（シャッフル済み）
  , wTi      :: !Int
  , wTrials  :: [TrialF]             -- 古い順
  , wPhase   :: !Phase
  , wFrom    :: !(Double, Double)    -- いまのマウスの出発点（左上が原点）
  , wTgt     :: Maybe Tgt
  , wT0      :: !Float               -- ターゲットが出た時刻 [秒]
  , wSc      :: !Double              -- 画面がせまいときの縮め率
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wFit     :: Maybe Lin
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-10 Fitts' law" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \env -> do
      printSelfTest selfTest
      r <- freshSeed
      pure (resetData env False r world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] 0 [] Ready (0, 0) Nothing 0 1 False 1 (seedRng 1) Nothing []

-- クリックする場所（左上 x, y、幅、高さ）
arena :: Env -> (Double, Double, Double, Double)
arena env = (20, 90, realToFrac (envW env) * 0.55, realToFrac (envH env) - 200)

resetData :: Env -> Bool -> Rng -> World -> World
resetData env sim r w = w { wSim = sim, wTrials = [], wTi = 0, wFit = Nothing, wOrder = order, wRng = r'
                          , wSc = min 1 ((aw - 80) / getAt 500 ds (length ds - 1))   -- 画面がせまいときは全体を縮める（ID は変わらない）
                          , wFrom = (ax + aw / 2, ay + ah / 2), wPhase = Ready, wTgt = Nothing }
  where
    (ax, ay, aw, ah) = arena env
    (order, r') = runRand (shuffleArr [ (d, wd) | _ <- [1 .. reps], d <- ds, wd <- ws ]) r

idOf :: Double -> Double -> Double
idOf d wd = logBase 2 (d / wd + 1)

-- 次のターゲット：いまの場所から D はなれた所（左右はランダム、はみ出すなら反対へ）
placeTarget :: Env -> World -> World
placeTarget env w = w { wTgt = Just (Tgt (fx + dir * dd) fy d wd (wd * wSc w)), wRng = r' }
  where
    (ax, _, aw, _) = arena env
    (d, wd) = getAt (150, 16) (wOrder w) (wTi w)
    dd = d * wSc w
    (fx, fy) = wFrom w
    (u, r') = runRand rnd (wRng w)
    dir0 = if u < 0.5 then -1 else 1
    dir = if fx + dir0 * dd < ax + 40 || fx + dir0 * dd > ax + aw - 40 then -dir0 else dir0

-- クリック（(mx, my) は左上が原点の座標）
click :: Env -> (Double, Double) -> World -> World
click env (mx, my) w
  | wSim w = w
  | wPhase w == Ready = if dist (mx, my) (wFrom w) < 22 then (placeTarget env w) { wPhase = Go, wT0 = envTime env } else w
  | wPhase w /= Go = w
  | otherwise = case wTgt w of
      Nothing -> w
      Just t ->
        let mt = realToFrac (envTime env - wT0 w) * 1000
            hit = dist (mx, my) (tgX t, tgY t) <= tgSize t / 2
            w1 = refit w { wTrials = wTrials w ++ [TrialF (tgD t) (tgW t) (idOf (tgD t) (tgW t)) mt hit]
                         , wFrom = (tgX t, tgY t), wTi = wTi w + 1 }
        in if wTi w1 >= length (wOrder w1) then w1 { wPhase = Done }
           else (placeTarget env w1) { wT0 = envTime env }
  where dist (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))

-- 条件ごとの平均 MT（当たりだけ）→ (ID, 平均 MT)
condMeans :: [TrialF] -> [(Double, Double)]
condMeans tr = [ (idOf d wd, sum r / fromIntegral (length r))
               | d <- ds, wd <- ws, let r = [ tMt t | t <- tr, tD t == d, tW t == wd, tHit t ], not (null r) ]

-- スループット = ID ÷ MT の平均 [bit/秒]
throughput :: [TrialF] -> Maybe Double
throughput tr = let r = [ tId t / (tMt t / 1000) | t <- tr, tHit t, tMt t > 0 ]
                in if null r then Nothing else Just (sum r / fromIntegral (length r))

refit :: World -> World
refit w = let c = condMeans (wTrials w) in w { wFit = linFit (map fst c) (map snd c) }

simTrials :: Int -> Rand [TrialF]
simTrials n = sequence [ one d wd | _ <- [1 .. n], d <- ds, wd <- ws ]
  where one d wd = do
          let i = idOf d wd
          e <- rndNormal
          u <- rnd
          pure (TrialF d wd i (max 80 (simA + simB * i + simSd * e)) (u > simErr))

runSim :: Env -> Word32 -> World -> World
runSim env seed w0 = refit w { wTrials = tr, wRng = r', wTi = length (wOrder w), wPhase = Done }
  where
    w = resetData env True (seedRng seed) w0   -- p5.js 版はここで Date.now() の種でシャッフルするが、結果には影響しない
    (tr, r') = runRand (simTrials 15) (wRng w)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Click the grey circle to start -> then click each circle that appears" 20 30 18 (gray255 235)
  , say "O simulated observer   R reset" 20 54 16 (gray255 235)
  , say "Fast, but do not miss. Misses are recorded too (only hits are used for the fit)." 20 78 14 (gray255 170)
  , Color (gray255 55) (frameTL 1 (f2 ax) (f2 ay) (f2 aw) (f2 ah)) ]
  ++ (if wPhase w == Ready && not (wSim w)
        then [ Color (gray255 150) (circleD (f2 fx) (f2 fy) 40), sayC "start" (f2 fx) (f2 fy + 5) 14 (gray255 30) ]
        else [])
  ++ (case (wPhase w, wTgt w) of
        (Go, Just t) ->
          [ Color (gray255 70) (Line [(f2 fx, f2 fy), (f2 (tgX t), f2 (tgY t))])
          , Color (rgb255 255 200 110) (circleD (f2 (tgX t)) (f2 (tgY t)) (f2 (tgSize t)))
          , say ("D = " ++ fmt 0 (tgD t) ++ "   W = " ++ fmt 0 (tgW t) ++ "   ID = " ++ fmt 2 (idOf (tgD t) (tgW t)) ++ " bit") (f2 ax + 10) (f2 (ay + ah) - 10) 14 (gray255 170) ]
        _ -> [])
  ++ [ say ((if wSim w then "Simulated observer   " else "You   ") ++ show nT ++ (if wSim w then "" else " / " ++ show (length (wOrder w))) ++ " clicks   misses "
            ++ show (nT - hits) ++ " (" ++ (if nT > 0 then fmt 0 (100 * fromIntegral (nT - hits) / fromIntegral nT) else "0") ++ "%)"
            ++ (if wPhase w == Done && not (wSim w) then "   Well done!" else "")) (f2 ax) (f2 (ay + ah) + 30) 15 (gray255 205) ]
    -- ---- MT と ID のグラフ
  ++ [ plotAxes g "Difficulty ID = log2(D/W + 1) [bit]" "Movement time MT [ms]" [0 .. 6] [0, 400, 800, 1200] ]
  ++ [ Color (if tHit t then rgba255 120 190 255 90 else rgba255 255 100 100 150) (circleD (gX g (tId t)) (gY g (min (tMt t) 1200)) 5) | t <- wTrials w ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g i) (gY g m) 12) | (i, m) <- condMeans (wTrials w) ]
  ++ [ Color (rgb255 120 200 140) (thickLine 1.5 (gX g 0, gY g simA) (gX g 6, gY g (simA + 6 * simB))) | wSim w ]
  ++ fitPart
  ++ [ say ("Throughput (mean of ID / MT) = " ++ fmt 2 tp ++ " bit/s") rx (ry + 26) 15 (gray255 235) | Just tp <- [throughput (wTrials w)], tp /= 0 ]
  ++ [ say ("(1/b = " ++ fmt 2 (1000 / linB f) ++ ")") rx (ry + 46) 15 (gray255 235) | Just _ <- [throughput (wTrials w)], Just f <- [wFit w], linB f > 1 ]
  ++ [ say ("True values: a = " ++ fmt 0 simA ++ " ms, b = " ++ fmt 0 simB ++ " ms/bit") rx (ry + 72) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    (ax, ay, aw, ah) = arena env
    (fx, fy) = wFrom w
    nT = length (wTrials w)
    hits = length (filter tHit (wTrials w))
    g = Graph (envW env * 0.64) 110 (envW env * 0.32) (envH env * 0.5) 0 6 0 1200 False False
    rx = gx g - 30
    ry = gy g + gh g + 64
    fitPart = case wFit w of
      Just f -> [ Color (rgb255 255 170 90) (thickLine 3 (gX g 0, gY g (linA f)) (gX g 6, gY g (linA f + 6 * linB f)))
                , say ("MT = " ++ fmt 0 (linA f) ++ " + " ++ fmt 0 (linB f) ++ " * ID [ms]   r2 = " ++ fmt 2 (linR2 f)) rx ry 17 (gray255 235) ]
      Nothing -> [ say "Fit needs 2 or more conditions with different ID" rx ry 16 (gray255 235) ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (x, y) ->
    pure (click env (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y)) w)
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim env s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData env False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("ID(D = 300, W = 20) = log2 16 = 4 bit, ID(150, 50) = 2 bit", near (idOf 300 20) 4 1e-12 && near (idOf 150 50) 2 1e-12)
  , ("scaling D and W by the same factor keeps ID", near (idOf (500 * 0.7) (16 * 0.7)) (idOf 500 16) 1e-12)
  , ("noise-free MT -> a = 130, b = 120 exactly"
    , case linFit [1, 2, 3, 4] [250, 370, 490, 610] of
        Just f -> near (linA f) 130 1e-9 && near (linB f) 120 1e-9
        Nothing -> False)
  , ("throughput: ID 4 bit in 0.5 s -> 8 bit/s", nearM (throughput [TrialF 0 0 4 500 True]) 8 1e-12)
  , ("only misses / no data -> no NaN (nothing)", null' (throughput [TrialF 0 0 4 500 False]) && null' (throughput []))
  , ("simulated observer (360 clicks): b ~ 120 ms/bit (+-12), a ~ 150 (+-30)", maybe False (\f -> near (linB f) simB 12 && near (linA f) simA 30) f2') ]
  where
    null' = maybe True (const False)
    c = condMeans (fst (runRand (simTrials 40) (seedRng 52)))
    f2' = linFit (map fst c) (map snd c)

-- ---------- やってみよう ----------
-- 1. 36 回やって、自分の a、b、スループットを出そう。隣の人と比べると？
-- 2. マウスとトラックパッド（またはタッチ）で測り比べよう。どのくらいちがう？
-- 3. 「速く！」と「絶対に外さない」で意識を変えると、外れの割合と MT はどう変わる？
-- 4. 画面の端にあるボタン（メニューバーなど）が押しやすいのはなぜ？ W がどうなると考えられる？
-- 5. ws に 8 を足して、とても小さいターゲットでも直線に乗るか確かめよう

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
