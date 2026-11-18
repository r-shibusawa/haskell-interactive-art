-- ============================================
-- 52-04 上下法（階段法）─ かすかな点が見えるしきい値
-- 中心の + を見つめる。かすかな点が左か右に一瞬出るので、どちらだったかを答える（わからなくても必ず答える）
-- 2 回続けて正解ならコントラストを 1 段下げ、1 回まちがえたら 1 段上げる（1-up-2-down）→ 正答率 70.7% の所を行ったり来たり
-- （はじめは 1 回まちがえるまで 2 段ずつ下げて、すばやくしきい値の近くへ行く）
-- しきい値 = 反転点（上がり下がりが変わった所）の平均。少ない試行で効率よく測れる
-- 操作：スペースで開始 / ← → で答える / 2 = 1-up-2-down  3 = 1-up-3-down / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nTrials :: Int
nTrials = 30                         -- ★ 試行の数

factor :: Double
factor = 1.25                        -- ★ 1 段の大きさ（1.25 倍 ≈ 2 dB）

bg :: Double
bg = 110                             -- ★ 背景の灰色（画面の値）

blob :: Double
blob = 5                             -- ★ 点の大きさ（ガウスの σ px）

showT :: Float
showT = 0.4                          -- ★ 点を見せる時間 [秒]

simAlpha, simS :: Double
simAlpha = 0.04                      -- ★ シミュレーション観察者：正答率 = 0.5 + 0.5·L((ln c − ln α)/s)
simS = 0.35

pgH :: Int
pgH = 260                            -- 刺激の画像の高さ（幅は画面の 42%）

patch :: Int
patch = 30                           -- 点のまわり ±30 px だけ画像にする（その外はディザをしても背景と同じ値）

data Phase = Ready | Fix | Stim | Wait | Gap | Done deriving Eq

data World = World
  { wSt      :: Stair
  , wNDown   :: !Int
  , wPhase   :: !Phase
  , wT       :: !Float
  , wRight   :: !Bool             -- 点は右か
  , wPic     :: Picture           -- 点の画像（試行の間は変わらない）
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wSimAvg  :: Maybe Double
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-04 Staircase method" world0)
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
world0 = resetData False (seedRng 1) (World (newStair 2) 2 Ready 0 False Blank False 1 Nothing (seedRng 1) [])

newStair :: Int -> Stair
newStair nd = makeStair 0.12 factor nd 0.002 0.6 True

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wSt = newStair (wNDown w), wPhase = Ready, wSimAvg = Nothing, wRng = r }

pgW :: Env -> Int
pgW env = floor (envW env * 0.42)

-- 点の画像を 1 回だけ作る（ディザ = 小さな乱数を足して丸め、8 ビットより細かいコントラストも平均では正しく出す）
-- p5.js 版と同じく全部の画素で乱数を 1 つずつ使うが、画像にするのは点のまわりだけ
makeStim :: Int -> Double -> Bool -> Rand Picture
makeStim w c right = Rand $ \r0 ->
  let h = pgH
      cx = if right then fromIntegral w * 0.78 else fromIntegral w * 0.22
      cy = fromIntegral h / 2
      x0 = round cx - patch
      y0 = round cy - patch
      n = 2 * patch + 1
      inP x y = x >= x0 && x < x0 + n && y >= y0 && y < y0 + n
      val x y u = let r2 = (fromIntegral x - cx) ^ (2 :: Int) + (fromIntegral y - cy) ^ (2 :: Int)
                      v = bg * (1 + c * exp (-r2 / (2 * blob * blob))) + u - 0.5
                  in fromIntegral (max 0 (min 255 (floor (v + 0.5) :: Int))) :: Word8
      go !r !y !x acc
        | y >= h = (acc, r)
        | x >= w = go r (y + 1) 0 acc
        | otherwise = let (u, r') = runRand rnd r
                      in go r' y (x + 1) (if inP x y then val x y u : acc else acc)
      (vals, rEnd) = go r0 0 0 []
      buf = U.fromListN (n * n) (reverse vals)
      img = pixelImage n n 1 (\x y -> let v = buf U.! ((n - 1 - y) * n + x) in (v, v, v))   -- 画面は y 下向きなので上下を返す
  in (Translate (fromIntegral x0 + fromIntegral n / 2) (fromIntegral y0 + fromIntegral n / 2) img, rEnd)

startTrial :: Env -> World -> World
startTrial env w
  | stN (wSt w) >= nTrials = w { wPhase = Done }
  | otherwise = w { wRight = right, wPic = pic, wPhase = Fix, wT = envTime env, wRng = r' }
  where ((right, pic), r') = flip runRand (wRng w) $ do
          u <- rnd
          let rt = u < 0.5
          p <- makeStim (pgW env) (stV (wSt w)) rt
          pure (rt, p)

answer :: Env -> Bool -> World -> World
answer env right w
  | wPhase w /= Stim && wPhase w /= Wait = w
  | otherwise = w { wSt = stairUpdate (wSt w) (right == wRight w), wPhase = Gap, wT = envTime env }

-- シミュレーション観察者
simPc :: Double -> Double
simPc c = 0.5 + 0.5 / (1 + exp (-(log c - log simAlpha) / simS))

-- pⁿ = 0.5
targetP :: Int -> Double
targetP nd = 0.5 ** (1 / fromIntegral nd)

trueThreshold :: Int -> Double
trueThreshold nd = let l = 2 * targetP nd - 1 in exp (log simAlpha + simS * log (l / (1 - l)))

simRun :: Int -> Int -> Rand Stair
simRun n nd = go n (newStair nd)
  where go 0 s = pure s
        go k s = do u <- rnd
                    go (k - 1) (stairUpdate s (u < simPc (stV s)))

-- しきい値の対数（なければ小さな値）
logTh :: Stair -> Double
logTh s = log (maybe 1e-9 id (stairThreshold s 2))

runSim :: Word32 -> World -> World
runSim seed w0 = w { wSt = s, wSimAvg = Just avg, wPhase = Done, wRng = r' }
  where
    w = resetData True (wRng w0) w0
    nd = wNDown w
    ((s, avg), r') = flip runRand (seedRng seed) $ do
      s1 <- simRun 60 nd
      th <- mapM (const (logTh <$> simRun 60 nd)) [1 .. 20 :: Int]
      pure (s1, exp (sum th / fromIntegral (length th)))

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && dt > 0.5 = pure (startTrial env w)
  | wPhase w == Fix && dt > 0.5 = pure w { wPhase = Stim, wT = envTime env }
  | wPhase w == Stim && dt > showT = pure w { wPhase = Wait }
  | otherwise = pure w
  where dt = envTime env - wT w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   Left / Right answer   2: 1-up-2-down   3: 1-up-3-down   (now 1-up-" ++ show nd ++ "-down -> " ++ fmt 1 (targetP nd * 100) ++ "%)") 20 30 17 (gray255 235)
  , say "O simulated observer   R reset" 20 54 17 (gray255 235)
  , say "Look at the +. Was the faint spot left or right? Always answer, even if you saw nothing (guessing gives 50%)." 20 78 14 (gray255 170)
    -- ---- 刺激
  , Color (gray255 (f2 bg)) (rectTL sx sy pw ph) ]
  ++ [ Translate sx sy (wPic w) | wPhase w == Stim ]
  ++ [ Color fc (thickLine 2 (fx - 8, fy) (fx + 8, fy)), Color fc (thickLine 2 (fx, fy - 8) (fx, fy + 8))
     , say msg sx (sy + ph + 26) 15 (gray255 200)
     , say ((if wSim w then "Simulated observer" else "You are the observer") ++ "   trial " ++ show (stN st) ++ (if wSim w then "" else " / " ++ show nTrials)
            ++ "   contrast now " ++ fmt 2 (stV st * 100) ++ "%   reversals " ++ show (length (stRevs st))) sx (sy + ph + 50) 15 (gray255 200) ]
    -- ---- 階段のグラフ（コントラストは対数）
  ++ [ plotAxes g "Trial" "Contrast (log)" [0, roundJ (gx1 g / 2), gx1 g] [0.005, 0.01, 0.03, 0.1, 0.3]
     , Color (gray255 120) (polyline 1.5 [ (gX g i, gY g v) | (i, (v, _)) <- zip [0 ..] track ]) ]
  ++ [ if ok then Color (rgb255 120 210 150) (circleD x y 8)
       else Color (rgb255 255 100 100) (Pictures [thickLine 2 (x - 4, y - 4) (x + 4, y + 4), thickLine 2 (x - 4, y + 4) (x + 4, y - 4)])
     | (i, (v, ok)) <- zip [0 :: Int ..] track, let x = gX g (fromIntegral i), let y = gY g v ]
  ++ [ Color (rgb255 255 210 120) (at (gX g (fromIntegral i), gY g v) (ring 8 1.5)) | (i, v) <- stRevs st ]
  ++ [ Color (rgb255 255 170 90) (thickLine 2.5 (gx g, gY g t) (gx g + gw g, gY g t)) | Just t <- [th] ]
  ++ [ Color (rgb255 120 200 140) (thickLine 1.5 (gx g, gY g t) (gx g + gw g, gY g t)) | wSim w, let t = trueThreshold nd ]
  ++ [ say (case th of
              Just t -> "Threshold (mean of reversals, skip first 2) = " ++ fmt 2 (t * 100) ++ "%"
              Nothing -> "Threshold is computed after 4 trials") (gx g) ry 16 (gray255 235)
     , say "Dot = correct   x = wrong   ring = reversal   orange = threshold" (gx g) (ry + 24) 14 (gray255 170) ]
  ++ (if wSim w
        then [ say ("True " ++ fmt 1 (targetP nd * 100) ++ "% point = " ++ fmt 2 (trueThreshold nd * 100) ++ "%") (gx g) (ry + 50) 15 (rgb255 120 200 140)
             , say ("Mean of 20 runs x 60 trials = " ++ maybe "-" (\a -> fmt 2 (a * 100) ++ "%") (wSimAvg w)) (gx g) (ry + 70) 15 (rgb255 120 200 140) ]
        else [])
  ++ [ drawSelfTest env (wTest w) ]
  where
    st = wSt w
    nd = wNDown w
    track = reverse (stTrack st)
    sx = 20
    sy = 96
    pw = fromIntegral (pgW env)
    ph = fromIntegral pgH
    fx = sx + pw / 2
    fy = sy + ph / 2
    fc = gray255 (if wPhase w == Wait then 60 else 30)
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show nTrials ++ " trials)"
      Wait -> "Left or right? (Left / Right)"
      Done -> if wSim w then "" else "Well done! R to try again"
      _ -> ""
    g = Graph (envW env * 0.52) 120 (envW env * 0.44) (envH env * 0.5) 0 (fromIntegral (max nTrials (stN st))) 0.003 0.6 False True
    th = if stN st >= 4 then stairThreshold st 2 else Nothing
    ry = gy g + gh g + 66

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready -> pure (startTrial env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env True w)
  EventKey (Char c) Down _ _
    | c `elem` "23" -> do r <- freshSeed
                          pure (resetData False r w { wNDown = if c == '2' then 2 else 3 })
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("1-up-2-down target p^2 = 0.5 -> 70.71%, 1-up-3-down p^3 = 0.5 -> 79.37%", near (targetP 2) 0.70711 1e-5 && near (targetP 3) 0.79370 1e-5)
  , ("at the true 70.7% point the simulated observer is exactly 0.7071 correct", near (simPc (trueThreshold 2)) (targetP 2) 1e-9)
  , ("the staircase stays inside its limits (all correct stops at 0.005)", near (stV sLim) 0.005 1e-9)
  , ("no data -> threshold is null (no division by 0)", maybe True (const False) (stairThreshold (makeStair 0.1 1.25 2 0.01 1 False) 2))
  , ("simulated observer 1-up-2-down: mean threshold ~ 70.7% point (within one x1.25 step)", abs (log (ma / trueThreshold 2)) < log factor)
  , ("1-up-3-down: mean threshold ~ 79.4% point (within one step), higher than 2-down", abs (log (mb / trueThreshold 3)) < log factor && mb > ma) ]
  where
    sLim = foldl' stairUpdate (makeStair 0.01 2 2 0.005 0.02 False) (replicate 10 True)
    (as, bs) = unzip (fst (runRand (mapM (const ((,) <$> (logTh <$> simRun 80 2) <*> (logTh <$> simRun 80 3))) [1 .. 40 :: Int]) (seedRng 52)))
    ma = exp (sum as / fromIntegral (length as))
    mb = exp (sum bs / fromIntegral (length bs))

-- ---------- やってみよう ----------
-- 1. 30 試行で自分のしきい値を測ろう。グラフの階段はどこで行ったり来たりしている？
-- 2. 3 を押して 1-up-3-down にすると、しきい値は上がる？ 下がる？ なぜ？
-- 3. blob を 2 や 15 にすると、しきい値はどう変わる？（点が大きいほど見えやすい？ 15 なら patch も 90 に）
-- 4. factor を 2 にすると速く近づくが、ゆれは大きくなる。O で比べよう
-- 5. 背景 bg を暗く（40）すると、同じコントラストでも見え方は変わる？

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
