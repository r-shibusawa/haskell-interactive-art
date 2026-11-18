-- ============================================
-- 52-06 2 肢強制選択（2AFC）─ 左右どちらに模様があった？
-- 2 つのノイズの片方にだけ、かすかな縞模様が入っている。どちらだったかを ← → で答える
-- 正答率 Pc と感度の関係は Pc = Φ(d′/√2)。「あった/なかった」（Yes/No）とちがい、答えのくせ（基準 c）の影響を受けない
-- Y で Yes/No 課題（模様は 1 つ、J = あった F = なかった）に切りかえて、同じ濃さで d′ を比べる
-- 操作：スペースで開始 / ← → / Y = 課題の切りかえ / ↑↓ = 濃さ / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

per :: Int
per = 30                             -- ★ 課題 1 つあたりの試行

noiseSd :: Double
noiseSd = 28                         -- ★ ノイズの強さ

simD, simCYN :: Double
simD = 1.2                           -- ★ シミュレーション観察者の本当の d′
simCYN = 0.4                         -- ★ Yes/No での基準（「なかった」と言いやすい）

nPix :: Int
nPix = 112                           -- 刺激の画素数（たて・よこ）

data Phase = Ready | Fix | Stim | Wait | Gap | Done deriving Eq

-- 2AFC の記録：正答・試行・「左」と答えた回数
data Afc = Afc { aOk, aN, aLeft :: !Int }

-- Yes/No の記録：ヒット・見逃し・誤警報・正しい棄却
data Yn = Yn { yH, yM, yF, yC :: !Int }

data YnRes = YnRes { ydD, ydC, ydPc :: !Double }

data World = World
  { wAmp     :: !Double          -- ★ 縞模様の濃さ（↑↓）
  , wTask    :: !Int             -- 0 = 2AFC、1 = Yes/No
  , wAfc     :: !Afc
  , wYn      :: !Yn
  , wPhase   :: !Phase
  , wT       :: !Float
  , wRight   :: !Bool            -- 2AFC：模様は右
  , wSigYN   :: !Bool            -- Yes/No：模様がある
  , wPics    :: [Picture]        -- いまの刺激（試行ごとに 1 回だけ作る）
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-06 Two-alternative forced choice" world0)
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
world0 = resetData False (seedRng 1) (World 10 0 afc0 yn0 Ready 0 False False [] False 1 (seedRng 1) [])

afc0 :: Afc
afc0 = Afc 0 0 0

yn0 :: Yn
yn0 = Yn 0 0 0 0

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wAfc = afc0, wYn = yn0, wPhase = Ready, wRng = r }

stimSize :: Env -> Float
stimSize env = min (envW env * 0.2) (envH env * 0.34)

-- ノイズ ＋（sig のときだけ）ガボール模様
fillNoise :: Double -> Bool -> Rand B.ByteString
fillNoise amp sig = B.pack <$> mapM px [ (x, y) | y <- [0 .. nPix - 1], x <- [0 .. nPix - 1] ]
  where
    h = fromIntegral nPix / 2
    px (x, y) = do
      e <- rndNormal
      let dx = fromIntegral x - h
          dy = fromIntegral y - h
          env = exp (-(dx * dx + dy * dy) / (2 * 16 * 16))
          v = 128 + noiseSd * e + (if sig then amp * env * sin (2 * pi * dx / 11) else 0)
      pure (fromIntegral (round (clampv 0 255 v) :: Int))

nDone :: World -> Int
nDone w = if wTask w == 0 then aN (wAfc w) else let k = wYn w in yH k + yM k + yF k + yC k

startTrial :: Env -> World -> World
startTrial env w
  | nDone w >= per = w { wPhase = Done }
  | otherwise = w { wRight = right, wSigYN = sig, wPics = map toPic bss, wPhase = Fix, wT = envTime env, wRng = r' }
  where
    ((right, sig, bss), r') = flip runRand (wRng w) $ do
      rt <- fmap (< 0.5) rnd
      sg <- fmap (< 0.5) rnd
      bs <- if wTask w == 0 then sequence [fillNoise (wAmp w) (not rt), fillNoise (wAmp w) rt] else (: []) <$> fillNoise (wAmp w) sg
      pure (rt, sg, bs)
    sz = stimSize env
    toPic bs = Scale 1 (-1) (grayImage nPix nPix (sz / fromIntegral nPix) bs)

answer2 :: Env -> Bool -> World -> World
answer2 env right w
  | wTask w /= 0 || (wPhase w /= Stim && wPhase w /= Wait) = w
  | otherwise = w { wAfc = Afc (aOk a + fromEnum (right == wRight w)) (aN a + 1) (aLeft a + fromEnum (not right)), wPhase = Gap, wT = envTime env }
  where a = wAfc w

addYN :: Bool -> Bool -> Yn -> Yn
addYN sig yes k
  | sig && yes = k { yH = yH k + 1 }
  | sig = k { yM = yM k + 1 }
  | yes = k { yF = yF k + 1 }
  | otherwise = k { yC = yC k + 1 }

answerYN :: Env -> Bool -> World -> World
answerYN env yes w
  | wTask w /= 1 || (wPhase w /= Stim && wPhase w /= Wait) = w
  | otherwise = w { wYn = addYN (wSigYN w) yes (wYn w), wPhase = Gap, wT = envTime env }

-- 2AFC：d′ = √2 · z(Pc)　／　Yes/No：d′ = z(H) − z(F)
d2afc :: Afc -> Maybe Double
d2afc r = if aN r > 0 then Just (sqrt 2 * zOf (rateFix (fromIntegral (aOk r)) (fromIntegral (aN r)))) else Nothing

dYN :: Yn -> Maybe YnRes
dYN k
  | ns == 0 || nn == 0 = Nothing
  | otherwise = Just (YnRes (zH - zF) (-(zH + zF) / 2) (fromIntegral (yH k + yC k) / fromIntegral (ns + nn)))
  where
    ns = yH k + yM k
    nn = yF k + yC k
    zH = zOf (rateFix (fromIntegral (yH k)) (fromIntegral ns))
    zF = zOf (rateFix (fromIntegral (yF k)) (fromIntegral nn))

pc2afc :: Double -> Double
pc2afc d = phi (d / sqrt 2)

-- シミュレーション観察者（2AFC：左右の内部反応を比べる。Yes/No：基準より上なら「あった」）
sim2 :: Double -> Afc -> Rand Afc
sim2 d r = do
  right <- fmap (< 0.5) rnd
  el <- rndNormal
  er <- rndNormal
  let xl = el + (if right then 0 else d)
      xr = er + (if right then d else 0)
      ans = xr > xl
  pure (Afc (aOk r + fromEnum (ans == right)) (aN r + 1) (aLeft r + fromEnum (not ans)))

simYN :: Double -> Double -> Yn -> Rand Yn
simYN d c k = do
  sig <- fmap (< 0.5) rnd
  e <- rndNormal
  pure (addYN sig (e + (if sig then d else 0) > d / 2 + c) k)

-- 2AFC と Yes/No を交互に n 回
simBoth :: Int -> Afc -> Yn -> Rand (Afc, Yn)
simBoth 0 a b = pure (a, b)
simBoth n a b = do
  a' <- sim2 simD a
  b' <- simYN simD simCYN b
  a' `seq` b' `seq` simBoth (n - 1) a' b'

runSim :: Word32 -> World -> World
runSim seed w0 = w { wAfc = a, wYn = b, wRng = r', wPhase = Done }
  where
    w = resetData True (seedRng seed) w0
    ((a, b), r') = runRand (simBoth 300 afc0 yn0) (wRng w)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && dt > 0.6 = pure (startTrial env w)
  | wPhase w == Fix && dt > 0.4 = pure w { wPhase = Stim, wT = envTime env }
  | wPhase w == Stim && dt > 0.5 = pure w { wPhase = Wait }
  | otherwise = pure w
  where dt = envTime env - wT w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   " ++ (if task == 0 then "Left / Right" else "J = yes   F = no") ++ "   Y task (now: " ++ (if task == 0 then "2AFC" else "Yes/No")
         ++ ")   Up/Down strength (" ++ fmt 0 (wAmp w) ++ ")") 20 30 18 (gray255 235)
  , say "O simulated observer   R reset" 20 54 16 (gray255 235)
  , say (if task == 0 then "Look at the +. Was the pattern on the left or the right? (always answer)" else "Was the pattern there or not?") 20 80 15 (gray255 170)
    -- ---- 刺激
  , Color (gray255 128) (rectTL sx sy (sz * 2 + 40) sz) ]
  ++ (if wPhase w == Stim
        then (if task == 0 then [ at (sx + sz / 2, sy + sz / 2) p | p <- take 1 (wPics w) ] ++ [ at (sx + sz * 1.5 + 40, sy + sz / 2) p | p <- drop 1 (wPics w) ]
                           else [ at (sx + sz + 20, sy + sz / 2) p | p <- take 1 (wPics w) ])
        else [])
  ++ [ Color (gray255 60) (thickLine 2 (fx - 7, fy) (fx + 7, fy)), Color (gray255 60) (thickLine 2 (fx, fy - 7) (fx, fy + 7))
     , say msg sx (sy + sz + 28) 15 (gray255 205)
     , say ((if wSim w then "Simulated observer" else "You are the observer") ++ "   " ++ (if task == 0 then "2AFC" else "Yes/No") ++ "   "
            ++ show (nDone w) ++ (if wSim w then "" else " / " ++ show per)) sx (sy + sz + 52) 15 (gray255 205)
    -- ---- 結果
     , say ("2AFC: correct " ++ show (aOk r0) ++ " / " ++ show (aN r0)
            ++ (case d0 of
                  Just d -> "   Pc = " ++ fmt 2 (fi (aOk r0) / fi (aN r0)) ++ "   d' = sqrt2 z(Pc) = " ++ fmt 2 d ++ "   'left' rate " ++ fmt 2 (fi (aLeft r0) / fi (aN r0))
                  Nothing -> "")) sx ty 16 (gray255 235)
     , say ("Yes/No: hit " ++ show (yH k) ++ "  miss " ++ show (yM k) ++ "  false alarm " ++ show (yF k) ++ "  reject " ++ show (yC k)
            ++ maybe "" (\r -> "   Pc " ++ fmt 2 (ydPc r) ++ "   d' = " ++ fmt 2 (ydD r) ++ "   c = " ++ fmt 2 (ydC r)) r1) sx (ty + 28) 16 (gray255 235)
     , say "In 2AFC a bias toward 'left' hardly changes the percent correct." sx (ty + 54) 14 (gray255 170)
     , say "In Yes/No a shifted criterion c lowers it." sx (ty + 72) 14 (gray255 170) ]
  ++ [ say ("True d' = " ++ show simD ++ " (same in both tasks), Yes/No criterion c = " ++ show simCYN) sx (ty + 98) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ say ("-> theoretical 2AFC Pc " ++ fmt 3 (pc2afc simD)) sx (ty + 116) 14 (rgb255 120 200 140) | wSim w ]
    -- ---- Pc と d′ のグラフ
  ++ [ plotAxes g "Sensitivity d'" "Percent correct Pc" [0, 1, 2, 3] [0.5, 0.75, 1]
     , curveOf pc2afc (rgb255 255 170 90), curveOf (\d -> phi (d / 2)) (rgb255 120 190 255)
     , say "2AFC: Pc = Phi(d'/sqrt2)" (gx g + 10) (gy g + 22) 14 (rgb255 255 170 90)
     , say "Yes/No (unbiased c = 0): Pc = Phi(d'/2)" (gx g + 10) (gy g + 42) 14 (rgb255 120 190 255) ]
  ++ [ Color (rgb255 255 170 90) (circleD (gX g (clampv 0 3 d)) (gY g (fi (aOk r0) / fi (aN r0))) 14) | Just d <- [d0] ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g (clampv 0 3 (ydD r))) (gY g (ydPc r)) 14) | Just r <- [r1] ]
  ++ [ say "Same d': 2AFC gives a higher Pc (two can be compared)" (gx g) (gy g + gh g + 70) 14 (gray255 170)
     , drawSelfTest env (wTest w) ]
  where
    task = wTask w
    fi = fromIntegral :: Int -> Double
    sx = 20
    sy = 104
    sz = stimSize env
    fx = sx + sz + 20
    fy = sy + sz / 2
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show per ++ " trials)"
      Wait -> if task == 0 then "Left? Right?" else "Was it there (J) or not (F)?"
      Done -> if wSim w then "Simulated observer (300 trials each)" else "Y = the other task -> space"
      _ -> ""
    r0 = wAfc w
    d0 = d2afc r0
    k = wYn w
    r1 = dYN k
    ty = sy + sz + 96
    g = Graph (envW env * 0.6) 110 (envW env * 0.35) (envH env * 0.42) 0 3 0.4 1 False False
    curveOf f c = Color c (polyline 2.5 [ (gX g d, gY g (f d)) | i <- [0 .. 100 :: Int], let d = 3 * fromIntegral i / 100 ])

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | (wPhase w == Ready || wPhase w == Done) && nDone w < per -> pure (startTrial env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer2 env False w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer2 env True w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAmp = min 60 (wAmp w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAmp = max 0 (wAmp w - 2) }
  EventKey (Char c) Down _ _
    | c `elem` "jJ" -> pure (answerYN env True w)
    | c `elem` "fF" -> pure (answerYN env False w)
    | c `elem` "yY" -> do
        let w1 = w { wTask = 1 - wTask w, wPhase = Ready }
        if wSim w then (\r -> resetData False r w1) <$> freshSeed else pure w1
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("d' = 0: 2AFC percent correct is 0.5 (guessing)", near (pc2afc 0) 0.5 1e-6)
  , ("d' = 1 -> Pc = Phi(1/sqrt2) = 0.760", near (pc2afc 1) 0.7602 1e-4)
  , ("Pc back to d': sqrt2 z(Phi(1.2/sqrt2)) = 1.2", near (sqrt 2 * zOf (pc2afc 1.2)) 1.2 1e-5)
  , ("simulation 5000 trials: 2AFC percent correct ~ Phi(d'/sqrt2) (+-0.015)", near (fromIntegral (aOk a) / fromIntegral (aN a)) (pc2afc simD) 0.015)
  , ("d' from 2AFC ~ 1.2 (+-0.08)", nearM (d2afc a) simD 0.08)
  , ("Yes/No (even with a biased criterion) d' ~ 1.2 (+-0.1), c ~ 0.4 (+-0.08)", maybe False (\y -> near (ydD y) simD 0.1 && near (ydC y) simCYN 0.08) yr)
  , ("Yes/No with a biased criterion has a lower Pc (< Phi(d'/2))", maybe False (\y -> ydPc y < phi (simD / 2)) yr) ]
  where
    (a, b) = fst (runRand (simBoth 5000 afc0 yn0) (seedRng 52))
    yr = dYN b

-- ---------- やってみよう ----------
-- 1. 同じ濃さで 2AFC と Yes/No を 30 試行ずつ。d′ は近い値になった？ 正答率はどちらが高い？
-- 2. 2AFC で「左」と答えた割合を見よう。くせがあっても d′ はあまり変わらないのはなぜ？
-- 3. ↑↓ で濃さを変えて 2AFC の正答率をいくつか測り、グラフの曲線の上に乗るか確かめよう
-- 4. simCYN を 0 や 1 にして O。Yes/No の正答率はどう変わる？ d′ は？
-- 5. 3 つや 4 つから選ぶ課題（3AFC、4AFC）では、当てずっぽうの正答率はいくつ？

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
