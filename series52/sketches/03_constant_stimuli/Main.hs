-- ============================================
-- 52-03 恒常法と心理測定関数 ─「下の線は上の線より長い？」
-- 決まった 7 段階の長さをランダムな順に何回も見せ、「長い」と答えた割合をグラフにする（恒常法）
-- その点にロジスティック関数を最尤法で当てはめ、PSE（主観的等価点 = 50%）と JND（25%〜75% の幅の半分）を求める
-- 操作：スペースで開始 / ← = 短い  → = 長い / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', nub)
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

stdL :: Double
stdL = 200                                       -- ★ 基準の線の長さ [px]

levels :: [Double]
levels = [180, 187, 193, 200, 207, 213, 220]     -- ★ 比較の線の長さ（7 段階）

reps :: Int
reps = 5                                         -- ★ 1 段階あたりの回数（7 × 5 = 35 試行）

simPse, simS :: Double
simPse = 203                                     -- ★ シミュレーション観察者の本当の PSE
simS = 6                                         -- ★ ロジスティックの広がり

data Phase = Ready | Stim | Gap | Done deriving Eq

data World = World
  { wOrder   :: [Int]           -- 見せる段階の順番
  , wTi      :: !Int
  , wKs, wNs :: [Double]        -- 段階ごとの「長い」の回数と試行数
  , wPhase   :: !Phase
  , wT       :: !Float
  , wOffs    :: !(Float, Float) -- 左右のずらし
  , wFit     :: Maybe Logi
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wLastAns :: String
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-03 Method of constant stimuli" world0)
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
world0 = resetData False (seedRng 1) (World [] 0 [] [] Ready 0 (0, 0) Nothing False 1 "" (seedRng 1) [])

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wKs = zeros, wNs = zeros, wOrder = o, wRng = r', wTi = 0, wPhase = Ready, wFit = Nothing }
  where
    zeros = map (const 0) levels
    (o, r') = runRand (shuffleArr [ i | _ <- [1 .. reps], i <- [0 .. length levels - 1] ]) r

-- 端の位置で比べられないように左右にずらす
startTrial :: Env -> World -> World
startTrial env w = w { wPhase = Stim, wT = envTime env, wOffs = (realToFrac a, realToFrac b), wRng = r' }
  where ((a, b), r') = flip runRand (wRng w) $ do
          u <- rnd
          v <- rnd
          pure (roundJ ((u - 0.5) * 120), roundJ ((v - 0.5) * 120))

answer :: Env -> Bool -> World -> World
answer env longer w
  | wPhase w /= Stim = w
  | otherwise = w { wNs = ns', wKs = ks', wLastAns = if longer then "longer" else "shorter"
                  , wFit = fitLogistic levels ks' ns' 0, wTi = ti', wPhase = if ti' >= length (wOrder w) then Done else Gap, wT = envTime env }
  where
    i = getAt 0 (wOrder w) (wTi w)
    ns' = [ if k == i then n + 1 else n | (k, n) <- zip [0 ..] (wNs w) ]
    ks' = [ if k == i && longer then n + 1 else n | (k, n) <- zip [0 ..] (wKs w) ]
    ti' = wTi w + 1

-- 本当の心理測定関数
trueP :: Double -> Double
trueP x = 1 / (1 + exp (-(x - simPse) / simS))

-- シミュレーション観察者：本当の心理測定関数 p(x) で「長い」と答える（コンピュータの中だけで、画面はちらつかない）
simObserver :: Double -> Rand Bool
simObserver x = (< trueP x) <$> rnd

-- 段階ごとに n 回答えて「長い」の回数を数える
simCounts :: Int -> Rand [Double]
simCounts n = mapM (\l -> fromIntegral . length . filter id <$> mapM (const (simObserver l)) [1 .. n]) levels

runSim :: Int -> Word32 -> World -> World
runSim perLevel seed w0 = w { wKs = ks, wNs = ns, wRng = r', wTi = length (wOrder w), wPhase = Done, wFit = fitLogistic levels ks ns 0 }
  where
    w = resetData True (wRng w0) w0
    (ks, r') = runRand (simCounts perLevel) (seedRng seed)
    ns = map (const (fromIntegral perLevel)) levels

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && envTime env - wT w > 0.6 = pure (startTrial env w)   -- 0.6 秒あけて次へ（速く切りかわらない）
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Space start   Left = shorter   Right = longer   O simulated observer   R reset" 20 34 18 (gray255 235)
  , say ("Top is the standard (" ++ fmt 0 stdL ++ " px). Is the bottom line longer? Always answer one way, even if unsure.") 20 60 14 (gray255 170) ]
    -- ---- 刺激（左半分）
  ++ (if wPhase w == Stim
        then let (o0, o1) = wOffs w
                 l = f2 (getAt stdL levels (getAt 0 (wOrder w) (wTi w)))
                 sl = f2 stdL
             in [ Color (gray255 235) (thickLine 6 (cx - sl / 2 + o0, cy - 60) (cx + sl / 2 + o0, cy - 60))
                , Color (gray255 235) (thickLine 6 (cx - l / 2 + o1, cy + 60) (cx + l / 2 + o1, cy + 60))
                , sayC "standard" (cx + o0) (cy - 80) 14 (gray255 150)
                , sayC "compare" (cx + o1) (cy + 98) 14 (gray255 150) ]
        else [ Color (gray255 120) (thickLine 2 (cx - 8, cy) (cx + 8, cy)), Color (gray255 120) (thickLine 2 (cx, cy - 8) (cx, cy + 8)) ]   -- 注視点
             ++ [ sayC (if wSim w then "" else "Press space to start (" ++ show (length (wOrder w)) ++ " trials)") cx (cy + 56) 16 (gray255 200) | wPhase w == Ready ]
             ++ [ sayC (if wSim w then "Result of the simulated observer" else "Well done! R to try again") cx (cy + 56) 16 (gray255 200) | wPhase w == Done ])
  ++ [ say ((if wSim w then "Simulated observer   trials " ++ fmt 0 (sum (wNs w))
             else "You are the observer   trial " ++ show (min (wTi w) (length (wOrder w))) ++ " / " ++ show (length (wOrder w)))
            ++ (if not (null (wLastAns w)) && not (wSim w) then "   (last answer: " ++ wLastAns w ++ ")" else "")) 20 (envH env - 116) 15 (gray255 200) ]
    -- ---- 結果の表
  ++ [ say s 20 (ty + dy) 14 (gray255 160) | (s, dy) <- zip ["Length", "Count", "'Longer'"] [0, 19, 38] ]
  ++ concat [ [ say (fmt 0 l) x ty 14 (gray255 210), say (fmt 0 n) x (ty + 19) 14 (gray255 210)
              , say (if n > 0 then fmt 2 (k / n) else "-") x (ty + 38) 14 (gray255 210) ]
            | (i, l, k, n) <- zip4' [0 :: Int ..] levels (wKs w) (wNs w), let x = 100 + fromIntegral i * 56 ]
    -- ---- 心理測定関数のグラフ（右半分）
  ++ [ plotAxes g "Comparison length [px]" "Proportion 'longer'" [180, 190, 200, 210, 220] [0, 0.25, 0.5, 0.75, 1] ]
  ++ [ Color (gray255 70) (Line [(gx g, gY g v), (gx g + gw g, gY g v)]) | v <- [0.25, 0.5, 0.75] ]
  ++ [ Color (rgba255 120 200 140 150) (polyline 1.5 [ (gX g x, gY g (trueP x)) | x <- xsC ]) | wSim w ]   -- 本当の関数
  ++ (case wFit w of
        Just f -> let x25 = lgPse f - lgJnd f
                      x75 = lgPse f + lgJnd f
                      c = rgba255 255 170 90 160
                  in [ Color (rgb255 255 170 90) (polyline 3 [ (gX g x, gY g (logiP f x)) | x <- xsC ])
                     , Color c (Line [(gX g (lgPse f), gY g 0), (gX g (lgPse f), gY g 0.5)])
                     , Color c (Line [(gX g x25, gY g 0.25), (gX g x25, gY g 0)])
                     , Color c (Line [(gX g x75, gY g 0.75), (gX g x75, gY g 0)]) ]
        Nothing -> [])
  ++ [ Color (rgb255 120 190 255) (circleD (gX g l) (gY g (k / n)) (6 + f2 (sqrt n) * 2)) | (l, k, n) <- zip3 levels (wKs w) (wNs w), n > 0 ]
  ++ (case wFit w of
        Just f -> [ say ("PSE (50% point) = " ++ fmt 1 (lgPse f) ++ " px  (shift from " ++ fmt 0 stdL ++ ": " ++ fmt 1 (lgPse f - stdL) ++ ")") (gx g) ry 16 (gray255 235)
                  , say ("JND (half the 25%-75% width) = " ++ fmt 1 (lgJnd f) ++ " px") (gx g) (ry + 24) 16 (gray255 235)
                  , say ("-> Weber ratio " ++ fmt 3 (lgJnd f / stdL)) (gx g) (ry + 48) 16 (gray255 235) ]
                  ++ [ say "(answers fully separated: slope at its limit)" (gx g) (ry + 70) 14 (gray255 200) | lgSteep f ]
        Nothing -> [ say "Not enough data (fit needs 2+ levels and 4+ trials)" (gx g) ry 16 (gray255 235) ])
  ++ [ say ("True values: PSE = " ++ fmt 0 simPse ++ " px, JND = " ++ fmt 2 (simS * log 3) ++ " px (O for new random)") (gx g) (ry + 96) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    cx = envW env * 0.27
    cy = envH env * 0.45
    ty = envH env - 84
    g = Graph (envW env * 0.56) 110 (envW env * 0.4) (envH env * 0.5) 175 225 0 1 False False
    ry = gy g + gh g + 70
    xsC = [175, 175.5 .. 225]
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready -> pure (startTrial env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env True w)
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim 40 s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト：答えのわかっているデータで当てはめを確かめる（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Phi(0) = 0.5, z(Phi(1)) = 1", near (phi 0) 0.5 1e-6 && near (zOf (phi 1)) 1 1e-5)
  , ("proportions exactly on the curve give back a, b (a = -6, b = 2)", maybe False (\f -> near (lgA f) a 1e-4 && near (lgB f) b 1e-4) f0)
  , ("PSE = -a/b = 3, JND = ln3 / b = 0.549", maybe False (\f -> near (lgPse f) 3 1e-4 && near (lgJnd f) (log 3 / 2) 1e-4) f0)
  , ("the fitted curve is 0.5 at the PSE, 0.25 and 0.75 at PSE -+ JND"
    , maybe False (\f -> near (logiP f (lgPse f)) 0.5 1e-6 && near (logiP f (lgPse f + lgJnd f)) 0.75 1e-6 && near (logiP f (lgPse f - lgJnd f)) 0.25 1e-6) f0)
  , ("simulated observer, 2100 trials: PSE ~ 203 (+-1.5)", maybe False (\f -> near (lgPse f) simPse 1.5) f1)
  , ("simulated observer, 2100 trials: JND ~ 6.59 (+-15%)", maybe False (\f -> near (lgJnd f) (simS * log 3) (0.15 * simS * log 3)) f1)
  , ("not enough data (1 level only) -> no fit, no NaN", maybe True (const False) (fitLogistic [200] [3] [5] 0))
  , ("answers fully separated: PSE is still finite, between 200 and 210", maybe False (\f -> lgPse f > 200 && lgPse f < 210) (fitLogistic [190, 200, 210] [0, 0, 5] [5, 5, 5] 0)) ]
  where
    xs = [1, 2, 3, 4, 5]
    a = -6
    b = 2
    ps = [ 1 / (1 + exp (-(a + b * x))) | x <- xs ]
    f0 = fitLogistic xs (map (* 1e4) ps) (map (const 1e4) xs) 0
    k1 = fst (runRand (simCounts 300) (seedRng 52))
    f1 = fitLogistic levels k1 (map (const 300) levels) 0

-- ---------- やってみよう ----------
-- 1. 35 試行やって、自分の PSE と JND を隣の人と比べよう。PSE が 200 からずれるのはなぜ？（上下の位置、線の太さ…）
-- 2. reps を 10 にすると点のばらつきはどう変わる？ 当てはめた曲線は安定する？
-- 3. O を何回か押して、40 回 × 7 段階でも推定値が本当の値のまわりでどれくらいばらつくかを見よう
-- 4. levels の間かくを広げすぎる・狭めすぎると、当てはめはどうなる？（全部 0 か 1 になる段階が増える）
-- 5. 比較の線を斜めにしたり色を変えたりして、PSE が動くかを測ってみよう

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

-- ---- ロジスティック関数 p(x) = g + (1 − g) / (1 + e^−(a + b x)) を最尤法で当てはめる
-- （ニュートン法 = フィッシャーのスコア法。x は平均 0・広がり 1 にそろえてから解き、最後にもどす）
-- g = あてずっぽうで当たる割合（Yes/No なら 0、2 択なら 0.5）
data Logi = Logi { lgA, lgB, lgPse, lgJnd :: !Double, lgSteep :: !Bool, lgIters :: !Int, lgG :: !Double }

-- 当てはめた関数の値
logiP :: Logi -> Double -> Double
logiP f x = lgG f + (1 - lgG f) / (1 + exp (-(lgA f + lgB f * x)))

fitLogistic :: [Double] -> [Double] -> [Double] -> Double -> Maybe Logi
fitLogistic xs ks ns g
  | bigN < 4 || length vals < 2 = Nothing
  | isNaN bA || isInfinite bA || isNaN bB || isInfinite bB || abs bB < 1e-12 = Nothing
  | otherwise = Just (Logi bA bB (-bA / bB) (log 3 / abs bB) (abs bF >= bmax * 0.999) itF g)
  where
    used = [ (x, n) | (x, n) <- zip xs ns, n > 0 ]
    bigN = sum (map snd used)
    vals = nub (map fst used)
    mx = sum [ n * x | (x, n) <- used ] / bigN
    sx0 = sqrt (sum [ n * (x - mx) * (x - mx) | (x, n) <- zip xs ns ] / bigN)
    sx = if sx0 == 0 || isNaN sx0 then 1 else sx0
    bmax = 25                          -- 傾きの上限（全部そろった答えでも無限大にしない）
    us = [ (x - mx) / sx | x <- xs ]
    dat = zip3 us ks ns
    pAt a b u = clampv 1e-9 (1 - 1e-9) (g + (1 - g) / (1 + exp (-(a + b * u))))
    ll a b = foldl' (\s (u, k, n) -> let p = pAt a b u in s + k * log p + (n - k) * log (1 - p)) 0 dat
    (aF, bF, itF) = newton 0 1 0
    newton a b it
      | it >= 60 = (a, b, it)
      | not (det > 1e-12) = (a, b, it)
      | abs (t * da) + abs (t * db) < 1e-10 = (a', b', it)
      | otherwise = newton a' b' (it + 1)
      where
        terms = [ (w, w * u, w * u * u, sc, sc * u)
                | (u, k, n) <- dat, n > 0
                , let lo = 1 / (1 + exp (-(a + b * u)))
                      p = pAt a b u
                      dp = (1 - g) * lo * (1 - lo)
                      w = n * dp * dp / (p * (1 - p))
                      sc = (k - n * p) / (p * (1 - p)) * dp ]
        (haa, hab, hbb, ga, gb) = foldl' (\(q1, q2, q3, q4, q5) (r1, r2, r3, r4, r5) -> (q1 + r1, q2 + r2, q3 + r3, q4 + r4, q5 + r5)) (1e-6, 0, 1e-6, 0, 0) terms
        det = haa * hbb - hab * hab
        da0 = (hbb * ga - hab * gb) / det
        db0 = (haa * gb - hab * ga) / det
        mag = sqrt (da0 * da0 + db0 * db0)
        (da, db) = if mag > 5 then (da0 * 5 / mag, db0 * 5 / mag) else (da0, db0)
        l0 = ll a b
        t = halve 1
        halve s = if s > 1e-4 && ll (a + s * da) (clampv (-bmax) bmax (b + s * db)) < l0 - 1e-12 then halve (s / 2) else s
        a' = a + t * da
        b' = clampv (-bmax) bmax (b + t * db)
    bB = bF / sx
    bA = aF - bF * mx / sx
