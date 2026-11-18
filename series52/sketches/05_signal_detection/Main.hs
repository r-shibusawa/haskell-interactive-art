-- ============================================
-- 52-05 信号検出理論 ─「あった」「なかった」を感度 d′ と基準 c に分ける
-- ノイズの中に、かすかな縞模様が半分の確率で入っている。見えたら J（あった）、見えなかったら F（なかった）
-- ヒット率 H と誤警報率 F から、感度 d′ = z(H) − z(F)、基準 c = −(z(H) + z(F)) / 2 を計算する
-- 得点表（P で切りかえ）を変えると、感度は同じでも基準 c が動く。ROC 曲線（H と F の関係）で確かめる
-- 操作：スペースで開始 / J = あった  F = なかった / P = 得点表 / ↑↓ = 模様の濃さ / O = シミュレーション観察者 / R = やり直し
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
per = 30                             -- ★ 得点表 1 つあたりの試行

noiseSd :: Double
noiseSd = 28                         -- ★ ノイズの強さ（画面の値の標準偏差）

-- 得点表：名前・ヒット・見逃し・誤警報・正しい棄却
data Payoff = Payoff { pName :: String, pHit, pMiss, pFa, pCr :: !Double }

payoffs :: [Payoff]
payoffs =                            -- ★ 得点表
  [ Payoff "Neutral" 1 (-1) (-1) 1
  , Payoff "Don't miss" 3 (-3) (-1) 1
  , Payoff "No false alarms" 1 (-1) (-3) 3 ]

simD :: Double
simD = 1.5                           -- ★ シミュレーション観察者の本当の d′

nPix :: Int
nPix = 128                           -- 刺激の画素数（たて・よこ）

data Phase = Ready | Fix | Stim | Wait | Gap | Done deriving Eq

-- ヒット・見逃し・誤警報・正しい棄却の回数
data Counts = Counts { kH, kM, kF, kC :: !Int }

data Kind = Hit | Miss | FalseAlarm | Reject deriving Eq

data Sdt = Sdt { sdH, sdF, sdD, sdC :: !Double }

data World = World
  { wAmp     :: !Double          -- ★ 縞模様の濃さ（↑↓）
  , wPay     :: !Int
  , wCounts  :: [Counts]
  , wPhase   :: !Phase
  , wT       :: !Float           -- いまの段階が始まった時刻 [秒]
  , wSig     :: !Bool            -- いまの試行に模様が入っているか
  , wPic     :: Picture          -- いまの刺激（試行ごとに 1 回だけ作る）
  , wScore   :: !Double
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wLast    :: String
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-05 Signal detection" world0)
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
world0 = resetData False (seedRng 1) (World 10 0 [] Ready 0 False Blank 0 False 1 "" (seedRng 1) [])

counts0 :: Counts
counts0 = Counts 0 0 0 0

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wCounts = map (const counts0) payoffs, wPhase = Ready, wScore = 0, wLast = "", wRng = r }

-- 刺激の大きさ（画面上の 1 辺）
stimSize :: Env -> Float
stimSize env = min (envW env * 0.36) (envH env * 0.5)

-- ノイズ ＋（sig のときだけ）ガボール模様。0〜255 の灰色を左上から順に
makeStim :: Double -> Bool -> Rand B.ByteString
makeStim amp sig = B.pack <$> mapM px [ (x, y) | y <- [0 .. nPix - 1], x <- [0 .. nPix - 1] ]
  where
    h = fromIntegral nPix / 2
    px (x, y) = do
      e <- rndNormal
      let dx = fromIntegral x - h
          dy = fromIntegral y - h
          env = exp (-(dx * dx + dy * dy) / (2 * 18 * 18))
          v = 128 + noiseSd * e + (if sig then amp * env * sin (2 * pi * dx / 12) else 0)
      pure (fromIntegral (round (clampv 0 255 v) :: Int))

nDone :: World -> Int
nDone w = let k = getAt counts0 (wCounts w) (wPay w) in kH k + kM k + kF k + kC k

isDone :: World -> Bool
isDone w = nDone w >= per

startTrial :: Env -> World -> World
startTrial env w
  | isDone w = w { wPhase = Done }
  | otherwise = w { wSig = sig, wPic = pic, wPhase = Fix, wT = envTime env, wRng = r' }
  where
    ((sig, bs), r') = flip runRand (wRng w) $ do
      s <- fmap (< 0.5) rnd
      b <- makeStim (wAmp w) s
      pure (s, b)
    sz = stimSize env
    pic = Scale 1 (-1) (grayImage nPix nPix (sz / fromIntegral nPix) bs)

addKind :: Kind -> Counts -> Counts
addKind k c = case k of
  Hit -> c { kH = kH c + 1 }
  Miss -> c { kM = kM c + 1 }
  FalseAlarm -> c { kF = kF c + 1 }
  Reject -> c { kC = kC c + 1 }

kindOf :: Bool -> Bool -> Kind
kindOf sig yes = if sig then (if yes then Hit else Miss) else (if yes then FalseAlarm else Reject)

answer :: Env -> Bool -> World -> World
answer env yes w
  | wPhase w /= Stim && wPhase w /= Wait = w
  | otherwise = w { wCounts = [ if i == wPay w then addKind kind c else c | (i, c) <- zip [0 ..] (wCounts w) ]
                  , wScore = wScore w + pts, wLast = nm, wPhase = Gap, wT = envTime env }
  where
    kind = kindOf (wSig w) yes
    p = getAt (head' payoffs) payoffs (wPay w)
    (pts, nm) = case kind of
      Hit -> (pHit p, "hit")
      Miss -> (pMiss p, "miss")
      FalseAlarm -> (pFa p, "false alarm")
      Reject -> (pCr p, "correct rejection")
    head' xs = getAt (Payoff "" 0 0 0 0) xs 0

-- d′ と c（割合は 1/(2N) で補正して z が無限大にならないように）
sdt :: Counts -> Maybe Sdt
sdt k
  | ns < 1 || nn < 1 = Nothing
  | otherwise = Just (Sdt h f (zH - zF) (-(zH + zF) / 2))
  where
    ns = fromIntegral (kH k + kM k)
    nn = fromIntegral (kF k + kC k)
    h = rateFix (fromIntegral (kH k)) ns
    f = rateFix (fromIntegral (kF k)) nn
    zH = zOf h
    zF = zOf f

-- 得点の期待値が最大になる基準（信号と雑音は半々）
optimalC :: Payoff -> Double -> Double
optimalC p d = log ((pCr p - pFa p) / (pHit p - pMiss p)) / d

-- シミュレーション観察者：内部の反応 x ~ 正規分布（信号なら平均 d′）。x > d′/2 + c なら「あった」
simTrial :: Double -> Double -> Counts -> Rand Counts
simTrial d c k = do
  sig <- fmap (< 0.5) rnd
  e <- rndNormal
  let x = e + (if sig then d else 0)
  pure (addKind (kindOf sig (x > d / 2 + c)) k)

simMany :: Int -> Double -> Double -> Counts -> Rand Counts
simMany 0 _ _ k = pure k
simMany n d c k = simTrial d c k >>= \k' -> k' `seq` simMany (n - 1) d c k'

runSim :: Word32 -> World -> World
runSim seed w0 = w { wCounts = cs, wRng = r', wPhase = Done }
  where
    w = resetData True (seedRng seed) w0
    (cs, r') = runRand (mapM (\p -> simMany 200 simD (optimalC p simD) counts0) payoffs) (wRng w)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && dt > 0.6 = pure (startTrial env w)
  | wPhase w == Fix && dt > 0.4 = pure w { wPhase = Stim, wT = envTime env }
  | wPhase w == Stim && dt > 0.5 = pure w { wPhase = Wait }
  | otherwise = pure w
  where dt = envTime env - wT w

cols :: [Color]
cols = [rgb255 120 190 255, rgb255 255 120 120, rgb255 220 200 90]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Space start   J = yes, it was there   F = no   P payoff table   Up/Down strength (" ++ fmt 0 (wAmp w) ++ ")") 20 30 18 (gray255 235)
  , say "O simulated observer   R reset" 20 54 16 (gray255 235)
  , say ("Payoff: " ++ pName p0 ++ " (hit " ++ sg (pHit p0) ++ ", miss " ++ sg (pMiss p0) ++ ", false alarm " ++ sg (pFa p0) ++ ", reject " ++ sg (pCr p0) ++ ")") 20 78 14 (gray255 170)
  , say "Sensitivity d' = how well you tell them apart.  Criterion c = how readily you say 'yes'." 20 96 14 (gray255 170)
    -- ---- 刺激
  , Color (gray255 128) (rectTL sx sy sz sz) ]
  ++ [ at (sx + sz / 2, sy + sz / 2) (wPic w) | wPhase w == Stim ]
  ++ [ Color (gray255 60) (thickLine 2 (fx - 7, fy) (fx + 7, fy)), Color (gray255 60) (thickLine 2 (fx, fy - 7) (fx, fy + 7))
     , say msg sx (sy + sz + 26) 15 (gray255 205)
     , say ("Last trial: " ++ wLast w ++ "   score " ++ fmt 0 (wScore w)) sx (sy + sz + 50) 15 (gray255 205) ]
    -- ---- 表
  ++ [ say h (sx + cx) ty 14 (gray255 160) | (h, cx) <- zip ["Payoff", "Hit", "Miss", "FA", "Rej", "H", "F", "d'", "c"] colX ]
  ++ concat [ [ say v (sx + cx) (ty + 20 + fromIntegral i * 20) 14 (gray255 (if i == wPay w then 240 else 170)) | (v, cx) <- zip (cells p k) colX ]
            | (i, p, k) <- zip3 [0 :: Int ..] payoffs (wCounts w) ]
    -- ---- ROC 曲線
  ++ [ plotAxes g "False alarm rate F" "Hit rate H" [0, 0.5, 1] [0, 0.5, 1]
     , Color (gray255 80) (Line [(gx g, gy g + gh g), (gx g + gw g, gy g)]) ]
  ++ [ roc simD (rgb255 120 200 140) 1.5 | wSim w ]
  ++ [ roc d (rgb255 255 170 90) 2.5 | Just d <- [dm], not (isNaN d || isInfinite d) ]
  ++ [ Color c (circleD (gX g (sdF r)) (gY g (sdH r)) 13) | (c, k) <- zip cols (wCounts w), Just r <- [sdt k] ]
  ++ concat [ [ Color c (circleD (gx g + gw g - 176) (gy g + gh g - 69 + fromIntegral i * 22) 9), say (pName p) (gx g + gw g - 166) (gy g + gh g - 64 + fromIntegral i * 22) 14 c ]
            | (i, c, p) <- zip3 [0 :: Int ..] cols payoffs ]
  ++ [ say ("Mean d' = " ++ fmt 2 d) (gx g) ry 16 (gray255 235) | Just d <- [dm] ]
  ++ [ say "Orange: theoretical ROC for that d', H = Phi(d' + z(F))" (gx g) (ry + 22) 14 (rgb255 255 170 90) | Just _ <- [dm] ]
  ++ [ say "Same d' -> the points move along one curve (only c changes)." (gx g) (ry + 44) 14 (gray255 170)
     , say "Diagonal = d' 0" (gx g) (ry + 62) 14 (gray255 170) ]
  ++ [ say ("True d' = " ++ show simD ++ ", c = " ++ concatSep " / " [ fmt 2 (optimalC p simD) | p <- payoffs ]) (gx g) (ry + 86) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ say "(the criterion that maximizes the score)" (gx g) (ry + 104) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    p0 = getAt (Payoff "" 0 0 0 0) payoffs (wPay w)
    sg v = (if v > 0 then "+" else "") ++ fmt 0 v
    sx = 20
    sy = 112
    sz = stimSize env
    fx = sx + sz / 2
    fy = sy + sz / 2
    msg = case wPhase w of
      Ready -> if wSim w then "" else "Press space to start (" ++ show per ++ " trials)"
      Wait -> "Was it there (J) or not (F)?"
      Done -> if wSim w then "Simulated observer: 200 trials each" else "This table is done. P = next payoff table -> space"
      _ -> ""
    ty = sy + sz + 86
    colX = [0, 150, 200, 250, 300, 350, 400, 450, 500]
    cells p k = [pName p, show (kH k), show (kM k), show (kF k), show (kC k)]
                ++ maybe ["-"] (\r -> map (fmt 2) [sdH r, sdF r, sdD r, sdC r]) (sdt k)
    side = min (envW env * 0.36) (envH env * 0.48)
    g = Graph (envW env * 0.55) 130 side side 0 1 0 1 False False
    rs = [ r | k <- wCounts w, Just r <- [sdt k] ]
    dm = if null rs then Nothing else Just (sum (map sdD rs) / fromIntegral (length rs))
    roc d c wt = Color c (polyline wt [ (gX g (phi z), gY g (phi (z + d))) | i <- [0 .. 100 :: Int], let z = -4 + 8 * fromIntegral i / 100 ])
    ry = gy g + gh g + 72
    concatSep s xs = foldr1' (\a b -> a ++ s ++ b) xs
    foldr1' f xs = if null xs then "" else foldr1 f xs

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready || (wPhase w == Done && not (isDone w)) -> pure (startTrial env w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAmp = min 60 (wAmp w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAmp = max 0 (wAmp w - 2) }
  EventKey (Char c) Down _ _
    | c `elem` "jJ" -> pure (answer env True w)
    | c `elem` "fF" -> pure (answer env False w)
    | c `elem` "pP" -> do
        let w1 = w { wPay = (wPay w + 1) `mod` length payoffs }
        w2 <- if wSim w then (\r -> resetData False r w1) <$> freshSeed else pure w1
        pure w2 { wPhase = Ready }
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Phi(0) = 0.5, z(Phi(1)) = 1", near (phi 0) 0.5 1e-6 && near (zOf (phi 1)) 1 1e-5)
  , ("H = Phi(1), F = Phi(-0.5) -> d' = 1.5, c = -0.25", near (zH - zF) 1.5 1e-5 && near (-(zH + zF) / 2) (-0.25) 1e-5)
  , ("all correct (H = 1, F = 0): the 1/(2N) fix keeps d' finite (= 2 z(1 - 1/40) = 3.92)"
    , maybe False (\r -> near (sdD r) (2 * zOf (1 - 1 / 40)) 1e-6) (sdt (Counts 20 0 0 20)))
  , ("no signal or no noise trials: not computed (null)", maybe True (const False) (sdt (Counts 3 2 0 0)))
  , ("simulated observer 3000 trials x 3: d' ~ 1.5 for every payoff (+-0.12)", all (\q -> nearM (sdD <$> q) simD 0.12) rr)
  , ("criterion c recovered (+-0.08): Don't miss < Neutral < No false alarms"
    , and [ nearM (sdC <$> q) (optimalC p simD) 0.08 | (q, p) <- zip rr payoffs ] && cOf 1 < cOf 0 && cOf 0 < cOf 2)
  , ("theoretical ROC: F = 0.5, d' = 1 -> H = Phi(1) = 0.841", near (phi (zOf 0.5 + 1)) 0.8413 1e-4) ]
  where
    zH = zOf (phi 1)
    zF = zOf (phi (-0.5))
    rr = map sdt (fst (runRand (mapM (\p -> simMany 3000 simD (optimalC p simD) counts0) payoffs) (seedRng 52)))
    cOf i = maybe 0 sdC (getAt Nothing rr i)

-- ---------- やってみよう ----------
-- 1. 中立で 30 試行。自分の d′ と c はいくつ？ c が負なら「あった」と言いやすいタイプ
-- 2. P で「見逃すな」「空振りするな」に変えて測ると、ROC の点はどう動く？ d′ は変わった？
-- 3. ↑↓ で模様を濃くすると、d′ はどれくらい上がる？
-- 4. O で本当の値と推定値を比べよう。per を 10 にすると推定はどれくらいばらつく？
-- 5. 医師の診断、空港の手荷物検査、迷惑メールのフィルタ……「基準 c」をどこに置くべきか考えよう

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
