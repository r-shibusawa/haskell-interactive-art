-- ============================================
-- 52-02 スティーヴンスのべき法則 ─ 感じの大きさ R = c · I^n
-- 左の基準を「100」としたとき、右はいくつに感じる？ 数字で答える（マグニチュード推定法）
-- 両対数グラフ（log R と log I）で直線を当てはめると、傾きがべき指数 n。長さ・面積・明るさで n がちがう
-- 操作：数字キーで入力して Enter（Backspace で消す）/ T = 刺激の種類 / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 刺激の種類：名前、基準の大きさ、見せる大きさ（6 段階）、シミュレーション観察者の本当の n
data SType = SType { tyName :: String, tyStd :: !Double, tyXs :: [Double], tySimN :: !Double }

types :: [SType]
types =                              -- ★ 刺激の種類
  [ SType "Line length" 150 [40, 70, 110, 160, 230, 320] 1.0
  , SType "Circle area" 11000 [1600, 3300, 6600, 13000, 26000, 44000] 0.7
  , SType "Brightness" 0.2 [0.02, 0.05, 0.1, 0.25, 0.5, 1] 0.33 ]

reps :: Int
reps = 2                             -- ★ 1 段階あたりの回数（6 × 2 = 12 試行）

simNoise :: Double
simNoise = 0.15                      -- ★ シミュレーション観察者の答えのばらつき（対数）

data Phase = Stim | Gap | Done deriving Eq

-- べき関数の当てはめ：指数 n、係数 c、r²
data Power = Power { pwN, pwC, pwR2 :: !Double }

data World = World
  { wTy      :: !Int                    -- いまの種類（0〜2）
  , wData    :: [[(Double, Double)]]    -- 種類ごとの (刺激, 答え)。答えた順
  , wOrder   :: [Double]
  , wTi      :: !Int
  , wTyped   :: String
  , wPhase   :: !Phase
  , wT       :: !Float
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-02 Stevens' power law" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      r <- freshSeed
      pure (newOrder r world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newOrder (seedRng 1) (World 0 [[], [], []] [] 0 "" Stim 0 False 1 (seedRng 1) [])

curType :: World -> SType
curType w = getAt (SType "" 1 [] 1) types (wTy w)

-- 新しい順番（同じ大きさを reps 回ずつ、シャッフル）
newOrder :: Rng -> World -> World
newOrder r w = w { wOrder = o, wRng = r', wTi = 0, wTyped = "", wPhase = Stim, wSim = False
                 , wData = [ if k == wTy w then [] else d | (k, d) <- zip [0 ..] (wData w) ] }
  where (o, r') = runRand (shuffleArr (concat (replicate reps (tyXs (curType w))))) r

-- 打った数字を数にする（"." だけなどは 0 = 受けつけない）
parseNum :: String -> Double
parseNum s = case reads (fixs s) :: [(Double, String)] of
  [(v, "")] -> v
  _ -> 0
  where fixs t = let t1 = if take 1 t == "." then '0' : t else t in if not (null t1) && last t1 == '.' then t1 ++ "0" else t1

submit :: Env -> World -> World
submit env w
  | wPhase w /= Stim || not (v > 0) = w { wTyped = "" }   -- 0 以下は対数にできないので受けつけない
  | otherwise = w { wData = [ if k == wTy w then d ++ [(x, v)] else d | (k, d) <- zip [0 ..] (wData w) ]
                  , wTyped = "", wTi = ti', wPhase = if ti' >= length (wOrder w) then Done else Gap, wT = envTime env }
  where v = parseNum (wTyped w)
        x = getAt 0 (wOrder w) (wTi w)
        ti' = wTi w + 1

-- 両対数で直線：log R = log c + n · log(I / 基準)
fitPower :: [(Double, Double)] -> Double -> Maybe Power
fitPower d std = do
  let p = [ q | q@(x, r) <- d, x > 0, r > 0 ]
  l <- linFit [ log (x / std) | (x, _) <- p ] [ log r | (_, r) <- p ]
  pure (Power (linB l) (exp (linA l)) (linR2 l))

-- シミュレーション観察者：R = 100 · (I / 基準)^n · e^(ゆらぎ)、整数に丸める
simAnswer :: SType -> Double -> Rand Double
simAnswer t x = do
  e <- rndNormal
  pure (max 1 (roundJ (100 * (x / tyStd t) ** tySimN t * exp (simNoise * e))))

runSim :: Word32 -> World -> World
runSim seed w = w { wData = d, wRng = r', wSim = True, wPhase = Done, wTi = length (wOrder w) }
  where (d, r') = runRand (mapM (\t -> sequence [ (,) x <$> simAnswer t x | _ <- [1 .. 8 :: Int], x <- tyXs t ]) types) (seedRng seed)

-- 刺激を描く（左上が原点の座標）
drawStim :: Int -> Double -> Float -> Float -> Picture
drawStim k x cx cy = case k of
  0 -> Color (gray255 235) (thickLine 6 (cx - f2 x / 2, cy) (cx + f2 x / 2, cy))
  1 -> Color (gray255 235) (circleD cx cy (2 * f2 (sqrt (x / pi))))
  _ -> Color (gray255 (f2 (linToSrgb x))) (rectTL (cx - 70) (cy - 70) 140 140)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Gap && envTime env - wT w > 0.5 = pure w { wPhase = Stim }
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Type digits, Enter to answer   Backspace delete   T type (now: " ++ tyName t ++ ")   O simulated observer   R reset") 20 32 16 (gray255 235)
  , say "Left is 100. What number is the right one? Twice as much = 200, half = 50. Any number you feel (no right answer)." 20 58 14 (gray255 170)
    -- ---- 刺激
  , Color (gray255 18) (rectTL bx by bw bh) ]
  ++ (case wPhase w of
        Stim -> [ drawStim (wTy w) (tyStd t) (bx + bw * 0.25) (by + bh * 0.5)
                , drawStim (wTy w) (getAt 1 (wOrder w) (wTi w)) (bx + bw * 0.72) (by + bh * 0.5)
                , sayC "Standard = 100" (bx + bw * 0.25) (by + bh - 20) 16 (gray255 200)
                , sayC "This one?" (bx + bw * 0.72) (by + bh - 20) 16 (gray255 200)
                , sayC (if null (wTyped w) then "_" else wTyped w) (bx + bw * 0.72) (by + 60) 40 (rgb255 255 210 120) ]
        Done -> [ sayC (if wSim w then "Simulated observer (3 types x 48 answers)" else "Well done! T for the other types") (bx + bw / 2) (by + bh / 2) 16 (gray255 200) ]
        Gap -> [])
  ++ [ say ((if wSim w then "Simulated observer" else "You are the observer") ++ "   " ++ tyName t ++ "   " ++ show (min (wTi w) (length (wOrder w))) ++ " / " ++ show (length (wOrder w))) bx (by + bh + 26) 15 (gray255 200)
     , say ("Last answers: " ++ intercalate "   " [ (if wTy w == 2 then prec3 x else fmt 0 (roundJ x)) ++ "->" ++ fmt 0 r | (x, r) <- lastN 6 curD ]) bx (by + bh + 48) 14 (gray255 160) ]
    -- ---- 両対数グラフ（3 種類を重ねる）
  ++ [ plotAxes g "Stimulus / standard (log)" "Answer R (log)" [0.1, 0.3, 1, 3] [10, 30, 100, 300, 1000] ]
  ++ concat [ [ Color c (circleD (gX g (x / tyStd tk)) (gY g r) (if k == wTy w then 8 else 5)) | (x, r) <- d ]
              ++ case fitPower d (tyStd tk) of
                   Just f -> [ Color c (thickLine (if k == wTy w then 3 else 1.5) (gX g 0.08, gY g (pwC f * 0.08 ** pwN f)) (gX g 5, gY g (pwC f * 5 ** pwN f)))
                             , say (tyName tk ++ ": n = " ++ fmt 2 (pwN f) ++ "  c = " ++ fmt 0 (pwC f) ++ "  r2 = " ++ fmt 2 (pwR2 f) ++ (if wSim w then "  (true n = " ++ show (tySimN tk) ++ ")" else "")) (gx g - 30) (ry + fromIntegral k * 26) 15 c ]
                   Nothing -> [ say (tyName tk ++ ": no data yet") (gx g - 30) (ry + fromIntegral k * 26) 15 c ]
            | (k, tk, d, c) <- zip4' [0 ..] types (wData w) cols ]
  ++ [ say "n < 1: grows more and more slowly (brightness)" (gx g - 30) (ry + 86) 14 (gray255 170)
     , say "n ~ 1: proportional (length)" (gx g - 30) (ry + 104) 14 (gray255 170)
     , drawSelfTest env (wTest w) ]
  where
    t = curType w
    curD = getAt [] (wData w) (wTy w)
    lastN n xs = drop (length xs - n) xs
    bx = 20
    by = 84
    bw = envW env * 0.5
    bh = envH env * 0.5
    g = Graph (envW env * 0.6) 110 (envW env * 0.36) (envH env * 0.5) 0.08 5 5 1000 True True
    ry = gy g + gh g + 66
    cols = [rgb255 120 190 255, rgb255 255 170 90, rgb255 220 130 255]
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '0' && c <= '9' && length (wTyped w) < 5 -> pure w { wTyped = wTyped w ++ [c] }
    | c == '.' && '.' `notElem` wTyped w -> pure w { wTyped = wTyped w ++ "." }
    | c == '\b' || c == '\DEL' -> pure w { wTyped = dropLast (wTyped w) }
    | c == '\r' || c == '\n' -> pure (submit env w)
    | c `elem` "tT" -> do r <- freshSeed
                          pure (newOrder r w { wTy = (wTy w + 1) `mod` length types })
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (newOrder r w { wData = [[], [], []] })
  EventKey (SpecialKey KeyBackspace) Down _ _ -> pure w { wTyped = dropLast (wTyped w) }
  EventKey (SpecialKey KeyDelete) Down _ _ -> pure w { wTyped = dropLast (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ -> pure (submit env w)
  EventKey (SpecialKey KeyPadEnter) Down _ _ -> pure (submit env w)
  _ -> pure w
  where dropLast s = take (length s - 1) s

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("noise-free answers R = 100 (I/std)^0.7 -> n = 0.7, c = 100 exactly", maybe False (\f -> near (pwN f) 0.7 1e-9 && near (pwC f) 100 1e-6) f1)
  , ("a straight line on log-log axes, so r^2 = 1", maybe False (\f -> near (pwR2 f) 1 1e-9) f1)
  , ("2 points (1, 50) and (4, 100) -> n = log 2 / log 4 = 0.5", maybe False (\f -> near (pwN f) 0.5 1e-12) (fitPower [(1, 50), (4, 100)] 1))
  , ("zero or negative answers are dropped (no log) -> no NaN"
    , isNothing' (fitPower [(1, 0), (2, -3)] 1) && maybe False (\f -> pwN f > 0) (fitPower [(1, 0), (2, 10), (4, 20)] 1))
  , ("simulated observer (brightness, 120 answers): n ~ 0.33 (+-0.05)", maybe False (\f -> near (pwN f) 0.33 0.05) (fitPower d3 (tyStd tb))) ]
  where
    ta = getAt (SType "" 1 [] 1) types 1
    tb = getAt (SType "" 1 [] 1) types 2
    f1 = fitPower [ (x, 100 * (x / tyStd ta) ** 0.7) | x <- tyXs ta ] (tyStd ta)
    d3 = fst (runRand (sequence [ (,) x <$> simAnswer tb x | _ <- [1 .. 20 :: Int], x <- tyXs tb ]) (seedRng 52))
    isNothing' = maybe True (const False)

-- ---------- やってみよう ----------
-- 1. 3 種類を全部やって、n を比べよう。明るさの n は 1 より小さい？ 長さは 1 に近い？
-- 2. 面積の n が 1 より小さいと、グラフの円の大きさで量を表すときにどんな誤解が起きる？
-- 3. 基準の数を 100 ではなく 10 にすると、答え方や n は変わる？
-- 4. O を押して、本当の n と推定した n を比べよう。simNoise を大きくするとどうなる？
-- 5. types に「線の太さ」や「点の数」を足して、自分の n を測ってみよう

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
