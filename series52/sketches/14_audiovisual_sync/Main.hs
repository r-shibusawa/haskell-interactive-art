-- ============================================
-- 52-14 音と光の同時性 🔊 ─ どれくらいずれていても「同時」に感じる？
-- 白い円が 1 回だけ光り、短い音が 1 回鳴る。そのずれ（SOA）を −300〜+300 ms で変える（+ は光が先）
-- 「光が先」「同時」「音が先」を答え、「光が先」「音が先」の割合にそれぞれロジスティック関数を最尤法で当てはめる
-- 2 つの 50% 点の間が「同時に感じる窓」、そのまん中が主観的同時点（PSS）
-- K = ブーバ／キキ（形と音の対応）のミニ課題
-- 操作：スペース = 開始 / ← 光が先  ↓ 同時  → 音が先 / K = ブーバ／キキ / O = シミュレーション観察者 / R = やり直し
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）。光は 1 試行に 1 回だけ（1.5 秒以上あける）
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', nub)
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

soas :: [Double]
soas = [-300, -200, -100, 0, 100, 200, 300]   -- ★ 光と音のずれ [ms]（+ = 光が先）

reps :: Int
reps = 4                             -- ★ 1 つのずれの回数（7 × 4 = 28 試行）

flashMs, iti :: Double
flashMs = 80                         -- ★ 光っている時間 [ms]
iti = 1500                           -- ★ 次の試行までの間 [ms]（ゆっくり：点滅しない）

audioLead :: Double
audioLead = 0                        -- ★ 音が出るまでの遅れ [秒]。この分だけ早めに鳴らす（端末に合わせて 0.03〜0.1 くらい）

-- ★ シミュレーション観察者：同時点 40 ms、窓の半分 100 ms、ゆらぎ 70 ms
simPss, simHalf, simSd :: Double
simPss = 40
simHalf = 100
simSd = 70

data Phase = Ready | Trial | Resp | Iti | Done deriving Eq

-- 答えの数（光が先, 同時, 音が先）
data Cnt = Cnt { cLight, cSame, cSound :: !Double }

data Ans = ALight | ASame | ASound deriving Eq

-- ブーバ／キキ
data KK = KK { kkOn :: !Bool, kkShapeLeft, kkSnd, kkN, kkMatch :: !Int, kkBusy :: !Float }

data World = World
  { wOrder   :: [Int]
  , wTi      :: !Int
  , wCnt     :: [Cnt]
  , wPhase   :: !Phase
  , wT       :: !Float              -- いまの段階が始まった時刻 [秒]
  , wFlashAt :: !Float              -- 光る時刻 [秒]
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wFL, wFS :: Maybe Logi          -- 「光が先」「音が先」の当てはめ
  , wKK      :: !KK
  , wNotes   :: [Note]
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-14 Audiovisual synchrony" world0)
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
world0 = resetData False (seedRng 1) (World [] 0 [] Ready 0 (-10) False 1 Nothing Nothing (KK False 0 0 0 0 0) [] (seedRng 1) [])

cnt0 :: Cnt
cnt0 = Cnt 0 0 0

resetData :: Bool -> Rng -> World -> World
resetData sim r0 w = w { wSim = sim, wCnt = map (const cnt0) soas, wFL = Nothing, wFS = Nothing
                       , wOrder = o, wRng = r', wTi = 0, wPhase = Ready }
  where (o, r') = runRand (shuffleArr [ i | _ <- [1 .. reps], i <- [0 .. length soas - 1] ]) r0

-- 試行の時刻 [ms]：基準 600 ms 後に、光と音のうち先のほう。もう一方は |SOA| 遅れる
schedule :: Double -> (Double, Double)
schedule soa = (600 + max 0 (-soa), 600 + max 0 soa)   -- (光, 音)

startTrial :: Env -> World -> World
startTrial env w
  | wTi w >= length (wOrder w) = w { wPhase = Done }
  | otherwise = w { wFlashAt = now + realToFrac (light / 1000)
                  , wNotes = wNotes w ++ [beep (now + realToFrac (max 0.05 (sound / 1000 - audioLead)))]
                  , wPhase = Trial, wT = now }
  where
    now = envTime env
    (light, sound) = schedule (getAt 0 soas (getAt 0 (wOrder w) (wTi w)))

beep :: Float -> Note
beep t = Note t 700 0.05 0.5 0

answer :: Env -> Ans -> World -> World
answer env a w
  | wPhase w /= Resp = w
  | otherwise = refit w { wCnt = [ if k == i then bump c else c | (k, c) <- zip [0 ..] (wCnt w) ], wTi = wTi w + 1, wPhase = Iti, wT = envTime env }
  where
    i = getAt 0 (wOrder w) (wTi w)
    bump c = case a of
      ALight -> c { cLight = cLight c + 1 }
      ASame -> c { cSame = cSame c + 1 }
      ASound -> c { cSound = cSound c + 1 }

total :: Cnt -> Double
total c = cLight c + cSame c + cSound c

refit :: World -> World
refit w = w { wFL = fitLogistic soas (map cLight (wCnt w)) ns 0, wFS = fitLogistic soas (map cSound (wCnt w)) ns 0 }
  where ns = map total (wCnt w)

-- 窓と PSS（両方の当てはめがそろって、向きも正しいときだけ）：(音が先の 50% 点, 光が先の 50% 点)
window :: World -> Maybe (Double, Double)
window w = case (wFL w, wFS w) of
  (Just fl, Just fs) | lgB fl > 0 && lgB fs < 0 && lgPse fl > lgPse fs -> Just (lgPse fs, lgPse fl)
  _ -> Nothing

-- シミュレーション観察者：感じたずれ = SOA − PSS + ゆらぎ。窓の外なら「光が先」「音が先」
simAnswer :: Double -> Rand Ans
simAnswer soa = do
  z <- rndNormal
  let e = soa - simPss + simSd * z
  pure (if e > simHalf then ALight else if e < -simHalf then ASound else ASame)

simCounts :: Int -> Rand [Cnt]
simCounts n = mapM (\s -> foldl' add cnt0 <$> mapM (const (simAnswer s)) [1 .. n]) soas
  where add c a = case a of
          ALight -> c { cLight = cLight c + 1 }
          ASame -> c { cSame = cSame c + 1 }
          ASound -> c { cSound = cSound c + 1 }

runSim :: Word32 -> World -> World
runSim seed w0 = refit w { wCnt = c, wRng = r', wTi = length (wOrder w), wPhase = Done }
  where
    w = resetData True (wRng w0) w0
    (c, r') = runRand (simCounts 40) (seedRng seed)

-- ---- ブーバ／キキ：丸い形ととがった形、やわらかい音と鋭い音
playKK :: Env -> World -> World
playKK env w
  | now < kkBusy k = w     -- 鳴っている間は重ねない
  | kkSnd k == 0 = w { wKK = k', wNotes = wNotes w ++ [Note (now + 0.05) 220 0.35 0.6 0, Note (now + 0.43) 180 0.4 0.6 0] }   -- 「ブーバ」：低く、長く
  | otherwise = w { wKK = k', wNotes = wNotes w ++ [Note (now + 0.05) 1300 0.06 0.4 0, Note (now + 0.17) 1600 0.06 0.4 0] }  -- 「キキ」：高く、短く
  where
    now = envTime env
    k = wKK w
    k' = k { kkBusy = now + 0.9 }

newKK :: World -> World
newKK w = w { wKK = (wKK w) { kkShapeLeft = a, kkSnd = b }, wRng = r' }
  where ((a, b), r') = flip runRand (wRng w) $ do
          u <- rnd
          v <- rnd
          pure (if u < 0.5 then 0 else 1, if v < 0.5 then 0 else 1)

-- 形（0 = 丸い、1 = とがった）。中心からの三角形で塗る（とがった形もきれいに塗れる）
drawShape :: Int -> Float -> Float -> Float -> Picture
drawShape typ cx cy r = Pictures [ Polygon [(cx, cy), p, q] | (p, q) <- zip pts (drop 1 pts ++ take 1 pts) ]
  where
    pts = [ (cx + rr * cos a, cy + rr * sin a)
          | i <- [0 .. 59 :: Int]
          , let a = fromIntegral i / 60 * tau
                rr = if typ == 0 then r * (1 + 0.12 * sin (3 * a) + 0.06 * sin (5 * a))
                     else r * (if i `mod` 6 == 0 then 1.15 else if i `mod` 3 == 0 then 0.45 else 0.8) ]

step :: Env -> Float -> World -> IO World
step env _ w = do
  ns <- playDue (envTime env) (wNotes w)
  let w1 = w { wNotes = ns }
      now = envTime env
  pure $ if wPhase w1 == Trial && now > wT w1 + 1.0 then w1 { wPhase = Resp }
         else if wPhase w1 == Iti && now > wT w1 + realToFrac (iti / 1000) then startTrial env w1
         else w1

draw :: Env -> World -> Picture
draw env w
  | kkOn (wKK w) = drawKK env w
  | otherwise = view env $
  [ say "Space start   Left = light first   Down = same time   Right = sound first   K bouba/kiki   O sim   R reset" 20 32 16 (gray255 235)
  , say "Keep the volume low, especially with headphones. The light flashes only once per trial." 20 60 15 (rgb255 255 200 120)
  , Color (gray255 80) (Pictures [thickLine 2 (cx - 8, cy + 120) (cx + 8, cy + 120), thickLine 2 (cx, cy + 112) (cx, cy + 128)])
  , Color (if wPhase w /= Ready && now >= wFlashAt w && now < wFlashAt w + realToFrac (flashMs / 1000) then gray255 245 else gray255 45) (circleD cx cy 120)
  , sayC msg cx (cy + 170) 16 (gray255 215)
  , say ((if wSim w then "Simulated observer   " ++ fmt 0 (sum (map total (wCnt w))) ++ " answers" else "You   " ++ show (min (wTi w) (length (wOrder w))) ++ " / " ++ show (length (wOrder w)))) 20 (cy + 210) 16 (gray255 215)
    -- ---- 表
  , say "SOA [ms]" 20 ty 14 (gray255 160), say "Light 1st" 20 (ty + 20) 14 (gray255 160)
  , say "Same" 20 (ty + 40) 14 (gray255 160), say "Sound 1st" 20 (ty + 60) 14 (gray255 160) ]
  ++ concat [ [ say (fmt 0 s) x ty 14 (gray255 210), say (fmt 0 (cLight c)) x (ty + 20) 14 (gray255 210)
              , say (fmt 0 (cSame c)) x (ty + 40) 14 (gray255 210), say (fmt 0 (cSound c)) x (ty + 60) 14 (gray255 210) ]
            | (i, s, c) <- zip3 [0 :: Int ..] soas (wCnt w), let x = 110 + fromIntegral i * 54 ]
    -- ---- 割合のグラフ
  ++ [ plotAxes g "SOA [ms] (+ = light first)" "Proportion of answers" soas [0, 0.5, 1] ]
  ++ (case (wFL w, wFS w) of
        (Just fl, Just fs) ->
          [ Color col (polyline 2.5 [ (gX g s, gY g (f s)) | s <- [-330, -325 .. 330] ])
          | (col, f) <- [ (cLightC, logiP fl), (cSoundC, logiP fs), (cSameC, \s -> max 0 (1 - logiP fl s - logiP fs s)) ] ]
        _ -> [])
  ++ concat [ [ Color cLightC (circleD (gX g s) (gY g (cLight c / n)) 9), Color cSameC (circleD (gX g s) (gY g (cSame c / n)) 9)
              , Color cSoundC (circleD (gX g s) (gY g (cSound c / n)) 9) ]
            | (s, c) <- zip soas (wCnt w), let n = total c, n > 0 ]
  ++ [ Color cLightC (circleD (gx g + 16) (gy g + 13) 9), say "light 1st" (gx g + 24) (gy g + 18) 14 cLightC
     , Color cSameC (circleD (gx g + 116) (gy g + 13) 9), say "same" (gx g + 124) (gy g + 18) 14 cSameC
     , Color cSoundC (circleD (gx g + 186) (gy g + 13) 9), say "sound 1st" (gx g + 194) (gy g + 18) 14 cSoundC ]
  ++ (case window w of
        Just (lo, hi) ->
          [ Color (rgba255 235 235 235 120) (Pictures [Line [(gX g lo, gy g), (gX g lo, gy g + gh g)], Line [(gX g hi, gy g), (gX g hi, gy g + gh g)]])
          , say ("Window of 'same': " ++ fmt 0 lo ++ " to " ++ fmt 0 hi ++ " ms (width " ++ fmt 0 (hi - lo) ++ " ms)") (gx g) ry 16 (gray255 235)
          , say ("PSS = " ++ fmt 0 ((lo + hi) / 2) ++ " ms (+ = feels simultaneous") (gx g) (ry + 26) 16 (gray255 235)
          , say "when the light is a bit first)" (gx g) (ry + 48) 16 (gray255 235) ]
        Nothing -> [ say "Fits once both 'light first' and 'sound first' answers exist" (gx g) ry 16 (gray255 235) ])
  ++ [ say ("True values: PSS = " ++ fmt 0 simPss ++ " ms, window = " ++ fmt 0 (simPss - simHalf) ++ " to " ++ fmt 0 (simPss + simHalf) ++ " ms") (gx g) (ry + 74) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ say "The screen flashes in whole frames (~17 ms) and the sound player" (gx g) (ry + 98) 14 (gray255 170)
     , say "starts with a delay, so the offsets have an error of tens of ms." (gx g) (ry + 116) 14 (gray255 170)
     , drawSelfTest env (wTest w) ]
  where
    now = envTime env
    cx = envW env * 0.25
    cy = envH env * 0.36
    ty = cy + 250
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show (length (wOrder w)) ++ " trials)"
      Resp -> "Left = light first   Down = same   Right = sound first"
      Done -> if wSim w then "Simulated observer (40 each, no sound or light)" else "Well done! R to try again"
      _ -> ""
    g = Graph (envW env * 0.55) 120 (envW env * 0.4) (envH env * 0.46) (-330) 330 0 1 False False
    ry = gy g + gh g + 76

cLightC, cSameC, cSoundC :: Color
cLightC = rgb255 255 220 120
cSameC = rgb255 120 210 150
cSoundC = rgb255 120 190 255

drawKK :: Env -> World -> Picture
drawKK env w = view env
  [ say "Bouba/kiki: Space plays a sound, pick the shape that fits it: Left / Right.   K to go back" 20 32 16 (gray255 235)
  , say "Keep the volume low, especially with headphones." 20 60 15 (rgb255 255 200 120)
  , Color (rgb255 220 200 255) (drawShape (kkShapeLeft k) (envW env * 0.3) cy 110)
  , Color (rgb255 150 220 255) (drawShape (1 - kkShapeLeft k) (envW env * 0.62) cy 110)
  , say ("Sound now: " ++ (if kkSnd k == 0 then "low and long" else "high and short") ++ " (space to play)") 20 (envH env - 110) 17 (gray255 235)
  , say ("Picked 'low long = round' / 'high short = spiky' in " ++ show (kkMatch k) ++ " of " ++ show (kkN k)
         ++ " (" ++ (if kkN k > 0 then fmt 0 (100 * fromIntegral (kkMatch k) / fromIntegral (kkN k)) else "-") ++ "%)") 20 (envH env - 80) 17 (gray255 235)
  , drawSelfTest env (wTest w) ]
  where
    k = wKK w
    cy = envH env * 0.42

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _ | c `elem` "kK" -> pure (newKK w { wKK = (wKK w) { kkOn = not (kkOn (wKK w)) } })
  _ | kkOn (wKK w) -> pure $ case ev of
        EventKey (SpecialKey KeySpace) Down _ _ -> playKK env w
        EventKey (SpecialKey k) Down _ _ | k == KeyLeft || k == KeyRight ->
          let kk = wKK w
              pick = if k == KeyLeft then kkShapeLeft kk else 1 - kkShapeLeft kk
          in newKK w { wKK = kk { kkN = kkN kk + 1, kkMatch = kkMatch kk + (if pick == kkSnd kk then 1 else 0) } }
        _ -> w
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready -> pure (startTrial env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer env ALight w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (answer env ASame w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer env ASound w)
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> do r <- freshSeed
                          let s = wSimSeed w + 1
                          pure (runSim s w { wSimSeed = s, wRng = r })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（音も光も出さない。p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("schedule: sound time - light time = SOA (for every offset)", all (\s -> let (l, so) = schedule s in so - l == s) soas)
  , ("one flash per trial, >= 1.5 s apart -> under 1 Hz (well below 3 Hz)", 1000 / (iti + 600) < 1 && flashMs < 200)
  , ("proportions on a logistic -> recovers the 50% point 120 ms", maybe False (\f -> near (lgPse f) 120 1e-3) f0)
  , ("simulated observer (400 each): PSS ~ 40 ms (+-12)", case (fL, fS) of { (Just a, Just b) -> near ((lgPse a + lgPse b) / 2) simPss 12; _ -> False })
  , ("window width ~ 200 ms (+-25)", case (fL, fS) of { (Just a, Just b) -> near (lgPse a - lgPse b) (2 * simHalf) 25; _ -> False })
  , ("volume limit: master <= 0.2", master <= 0.2) ]
  where
    f0 = fitLogistic soas [ 1000 / (1 + exp (-(s - 120) / 30)) | s <- soas ] (map (const 1000) soas) 0
    c = fst (runRand (simCounts 400) (seedRng 52))
    n = map total c
    fL = fitLogistic soas (map cLight c) n 0
    fS = fitLogistic soas (map cSound c) n 0

-- ---------- やってみよう ----------
-- 1. 28 試行で自分の窓と PSS を出そう。PSS は 0 より光が先（+）？ 音が先（−）？
-- 2. 遠くの花火は光が先に見えるのに、同時と感じることがある？ 窓の幅から考えよう（音は 1 秒に約 340 m）
-- 3. 隣の人と窓の幅を比べよう。音楽やゲームをする人は狭い？
-- 4. K のブーバ／キキで何 % が「丸い形 = 低く長い音」になった？ クラス全体で集計しよう
-- 5. 動画の音ずれは何 ms から気になる？ 窓の幅と比べよう。audioLead を変えると PSS はどう動く？

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

