-- ============================================
-- 53-11 系外惑星を見つける ─ 星のかすかな暗くなり（トランジット）と、星のふらつき（視線速度）
-- 惑星が星の前を通ると、隠れた面積の分だけ暗くなる：減光率 = (Rp/R*)²（星の明るさは一様とする近似）
-- ノイズのある 30 日分の観測データを、周期 P で折りたたむ（位相 = 時刻 ÷ P の小数部分）と、くぼみが重なって見える
-- 視線速度の振幅 K = (2πG/P)^(1/3) × Mp sin i ÷ M*^(2/3)（円軌道、Mp ≪ M*）
-- 操作：←→ = ためす周期（Shift で細かく）/ B = 周期を自動で探す / ↑↓ = 惑星の質量 / R = 新しい星 / 星は太陽と同じ（1 M☉、1 R☉）
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

days, cadence :: Double
days = 30                              -- ★ 観測日数
cadence = 1 / 72                       -- ★ 観測の間隔（20 分）

sigmaN :: Double
sigmaN = 0.0012                        -- ★ 明るさの測定ノイズ（1 = 星の明るさ）

grav, mSun, mJup, mEarth, rSunRE, rSunRJ, auRsun :: Double
grav = 6.674e-11
mSun = 1.989e30
mJup = 1.898e27
mEarth = 5.972e24
rSunRE = 109.1                         -- 太陽の半径 = 地球の 109.1 倍
rSunRJ = 9.73                          --            = 木星の 9.73 倍
auRsun = 215.03                        -- 1 AU = 太陽の半径の 215 倍

-- 本当の惑星：周期 P [日]、Rp/R* = k、衝突パラメータ b、最初のトランジットの時刻 t0
data Truth = Truth { trP, trK, trB, trT0 :: !Double }

-- 折りたたんだ結果：深さ、位相、はっきりさ
data Fold = Fold { fdDepth, fdPhase, fdSnr :: !Double }

data World = World
  { wTruth  :: !Truth
  , wData   :: !(U.Vector (Double, Double))   -- (時刻 [日], 明るさ)
  , wTrialP :: !Double
  , wFound  :: Maybe (Double, Fold)
  , wSpec   :: Maybe [(Double, Double)]
  , wMpJ    :: !Double                 -- 惑星の質量 [木星質量]
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-11 Finding exoplanets" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      s <- freshSeed
      pure (newStar s world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newStar 1 (World (Truth 3 0.1 0 0) U.empty 3.0 Nothing Nothing 1.0 [])

-- 半径 1 の星と、半径 k の惑星（中心の距離 z）の重なりの面積
overlap :: Double -> Double -> Double
overlap z k
  | z >= 1 + k = 0
  | z <= 1 - k = pi * k * k
  | z <= k - 1 = pi
  | otherwise = k * k * a + b - 0.5 * sqrt (max 0 ((-z + k + 1) * (z + k - 1) * (z - k + 1) * (z + k + 1)))
  where
    a = acos (clampv (-1) 1 ((z * z + k * k - 1) / (2 * z * k)))
    b = acos (clampv (-1) 1 ((z * z + 1 - k * k) / (2 * z)))

-- ケプラーの第 3 法則（M* = 1 M☉）
aOverR :: Double -> Double
aOverR p = (p / 365.25) ** (2 / 3) * auRsun

-- 時刻 t の惑星の空の上の位置（星の半径 = 1）と、手前にあるか
skyPos :: Double -> Truth -> (Double, Double, Bool)
skyPos t tr = (aOverR (trP tr) * sin ph, trB tr * cos ph, cos ph > 0)
  where ph = (2 * pi * (t - trT0 tr)) / trP tr

fluxAt :: Double -> Truth -> Double
fluxAt t tr = let (x, y, front) = skyPos t tr in if front then 1 - overlap (sqrt (x * x + y * y)) (trK tr) / pi else 1

-- [m/s]
rvK :: Double -> Double -> Double
rvK p mp = ((2 * pi * grav) / (p * 86400)) ** (1 / 3) * mp / mSun ** (2 / 3)

-- 観測の時刻（JavaScript と同じく t += CADENCE で足していく）
times :: [Double]
times = takeWhile (< days) (iterate (+ cadence) 0)

newStar :: Word32 -> World -> World
newStar seed w = w { wTruth = tr, wData = U.fromList dat, wFound = Nothing, wSpec = Nothing }
  where
    ((tr, dat), _) = flip runRand (seedRng seed) $ do
      p <- rnd
      k <- rnd
      b <- rnd
      t0 <- rnd
      let t = Truth (2 + 6 * p) (0.06 + 0.08 * k) (0.5 * b) (t0 * 2)
      ns <- mapM (const rndNormal) times
      pure (t, [ (tt, fluxAt tt t + sigmaN * e) | (tt, e) <- zip times ns ])

-- 位相（時刻 ÷ P の小数部分）
phaseOf :: Double -> Double -> Double
phaseOf t p = rem1 (rem1 (t / p) + 1)
  where rem1 x = x - fromIntegral (truncate x :: Int)     -- JavaScript の x % 1（ここでは x は小さい数）

-- 折りたたんで 200 区間の平均。くぼみの深さ = 1 − （となり 5 区間の平均のいちばん小さい値）
fold :: U.Vector (Double, Double) -> Double -> Fold
fold d p = Fold (1 - best) (fromIntegral bi / fromIntegral nb) (((1 - best) * sqrt bn) / sigmaN)
  where
    nb = 200 :: Int
    idx t = floor (phaseOf t p * fromIntegral nb) `mod` nb
    sums = U.accumulate (+) (U.replicate nb 0) (U.map (\(t, f) -> (idx t, f)) d)
    cnts = U.accumulate (+) (U.replicate nb 0) (U.map (\(t, _) -> (idx t, 1 :: Double)) d)
    (best, bi, bn) = foldl' pick (1e9, 0, 0) [0 .. nb - 1]
    pick acc@(bv, _, _) i =
      let qs = [ (i + j + nb) `mod` nb | j <- [-2 .. 2] ]
          ss = sum [ sums U.! q | q <- qs ]
          nn = sum [ cnts U.! q | q <- qs ]
      in if nn > 0 && ss / nn < bv then (ss / nn, i, nn) else acc

-- 周期を 1〜10 日で調べて、くぼみがいちばんはっきりする周期（箱型の当てはめの簡単版）
searchPeriod :: U.Vector (Double, Double) -> ([(Double, Double)], (Double, Fold))
searchPeriod d = (spec, fine)
  where
    coarse = [ (p, fold d p) | i <- [0 .. 900 :: Int], let p = 1 + (9 * fromIntegral i) / 900 ]
    spec = [ (p, fdSnr f) | (p, f) <- coarse ]
    better a@(_, fa) b@(_, fb) = if fdSnr fb > fdSnr fa then b else a
    best0 = foldl' better (getAt (1, Fold 0 0 0) coarse 0) (drop 1 coarse)
    -- まわりを細かく
    fine = foldl' better best0 [ (p, fold d p) | i <- [-50 .. 50 :: Int], let p = fst best0 + fromIntegral i * 0.0002 ]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 星と惑星（空の上）
  [ Color (rgb255 255 235 200) (circleD sx sy (2 * sr)) ]
  ++ [ Color (gray (10 / 255)) (circleD (sx + f2 px * sr) (sy + f2 py * sr) (2 * f2 (trK tr) * sr)) | front, abs px < 3 ]
  ++ [ sayC "Seen only when in front" sx (sy + sr + 24) 14 (rgb255 200 205 225)
     , sayC ("(day " ++ fmt 1 t ++ ")") sx (sy + sr + 42) 14 (rgb255 200 205 225)
  -- ---- 観測データ（30 日）
     , plotAxes g1 "Time [days]" "Brightness (observed, 30 days)" [0, 5, 10, 15, 20, 25, 30] [0.97, 0.98, 0.99, 1]
     , Color (rgba255 250 225 140 140) (Pictures [ rectTL (gX g1 tt) (gY g1 f) 1.5 1.5 | (tt, f) <- U.toList dat ])
     , Color (rgba255 255 255 255 120) (Line [(gX g1 t, gy g1), (gX g1 t, gy g1 + gh g1)])
  -- ---- 折りたたんだ図
     , plotAxes g2 "Phase (time / P, fractional part)" ("Folded with P = " ++ fmt 4 trial ++ " days") [0, 0.25, 0.5, 0.75, 1] [0.97, 0.98, 0.99, 1]
     , Color (rgba255 140 190 255 150) (Pictures [ rectTL (gX g2 (phaseOf tt trial)) (gY g2 f) 1.5 1.5 | (tt, f) <- U.toList dat ])
     , Color (rgb255 255 140 110) (Line [(gx g2, gY g2 (1 - fdDepth fd)), (gx g2 + gw g2, gY g2 (1 - fdDepth fd))])
  -- ---- 周期の探索の結果
     , plotAxes g3 "Period [days]" "Clearness of the dip (press B)" [1, 4, 7, 10] [] ]
  ++ [ Color (rgb255 220 140 255) (polyline 1 [ (gX g3 p, gY g3 s) | (p, s) <- sp ]) | Just sp <- [wSpec w] ]
  ++ [ Color (rgba255 255 255 255 120) (Line [(gX g3 trial, gy g3), (gX g3 trial, gy g3 + gh g3)])
  -- ---- 視線速度
     , plotAxes g4 "Phase" "Radial velocity of the star [m/s]" [0, 0.5, 1] [ roundTo 1 v | v <- [-kk, 0, kk] ]
     , Color (rgb255 140 230 180) (polyline 2 [ (gX g4 (fromIntegral i / 100), gY g4 (-kk * sin (2 * pi * (fromIntegral i / 100)))) | i <- [0 .. 100 :: Int] ])
  -- ---- 数字
     , say ("Trial P = " ++ fmt 4 trial ++ " d   depth = " ++ fmt 3 (fdDepth fd * 100) ++ "%   Rp/R* = sqrt(depth) = " ++ fmt 3 k
            ++ "   Rp = " ++ fmt 1 (k * rSunRE) ++ " R_earth = " ++ fmt 2 (k * rSunRJ) ++ " R_jup") 20 y0 15 (rgb255 250 225 140)
     , say ("Orbit a = (P/365.25)^(2/3) AU = " ++ fmt 4 (aOverR trial / auRsun) ++ " AU = " ++ fmt 1 (aOverR trial) ++ " R*   planet "
            ++ fmt 2 (wMpJ w) ++ " M_jup -> K = " ++ fmt 1 kk ++ " m/s") 20 (y0 + 24) 15 (rgb255 220 225 240) ]
  ++ [ say ("Auto search: P = " ++ fmt 4 fp ++ " d, depth " ++ fmt 3 (fdDepth ff * 100) ++ "% (clearness " ++ fmt 0 (fdSnr ff)
            ++ ")   answer: P = " ++ fmt 4 (trP tr) ++ ", Rp/R* = " ++ fmt 3 (trK tr)) 20 (y0 + 48) 15 (rgb255 140 230 180)
     | Just (fp, ff) <- [wFound w] ]
  ++ [ say "Exoplanets | Left/Right trial P (Shift: fine)  B auto search  Up/Down planet mass  R new star" 20 34 16 (rgb255 240 236 215)
     , say "When the folded dots line up into one dip, that period is the answer" 20 60 14 (rgb255 180 185 210)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env
    h = envH env
    tr = wTruth w
    dat = wData w
    trial = wTrialP w
    t = jsRem (realToFrac (envTime env) * 0.5) days
    sx = 150
    sy = 165
    sr = 60
    (px, py, front) = skyPos t tr
    g1 = Graph 320 100 (ww - 360) (h * 0.2) 0 days (1 - 0.03) (1 + 0.006) False False
    fd = fold dat trial
    g2 = Graph 70 (gy g1 + gh g1 + 70) (ww * 0.45) (h * 0.3) 0 1 (1 - 0.03) (1 + 0.006) False False
    k = sqrt (max 0 (fdDepth fd))
    g3 = Graph (gx g2 + gw g2 + 70) (gy g2) (ww - gx g2 - gw g2 - 110) (h * 0.13) 1 10 0
               (maybe 1 (\sp -> foldl' max (-1 / 0) (map snd sp) * 1.1) (wSpec w)) False False
    kk = rvK (trP tr) (wMpJ w * mJup)
    g4 = Graph (gx g3) (gy g3 + gh g3 + 70) (gw g3) (h * 0.13) 0 1 (-1.3 * kk) (1.3 * kk) False False
    y0 = max (gy g2 + gh g2) (gy g4 + gh g4) + 56

-- 小数点以下 n 桁に丸めた数（+v.toFixed(n)）
roundTo :: Int -> Double -> Double
roundTo n v = fromIntegral (round (v * 10 ^ n) :: Integer) / 10 ^ n

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down md _ -> pure w { wTrialP = min 10 (wTrialP w + st md) }
  EventKey (SpecialKey KeyLeft) Down md _ -> pure w { wTrialP = max 1 (wTrialP w - st md) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wMpJ = min 20 (wMpJ w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wMpJ = max 0.003 (wMpJ w / 1.25) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> let (sp, best) = searchPeriod (wData w) in pure w { wSpec = Just sp, wFound = Just best, wTrialP = fst best }
    | c `elem` "rR" -> do s <- freshSeed
                          pure (newStar s w)
  _ -> pure w
  where st md = if shift md == Down then 0.002 else 0.02

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("planet fully in front of the star: dimming = area ratio k^2", near (overlap 0 0.1 / pi) 0.01 1e-12 && near (overlap 0.5 0.1 / pi) 0.01 1e-12)
  , ("0 when apart, about half when centred on the star's edge", overlap 1.2 0.1 == 0 && near (overlap 1 0.1 / (pi * 0.01)) 0.5 0.05)
  , ("1 AU = 215 Rsun (a/R* for P = 1 year is 215)", near (aOverR 365.25) 215.03 1e-9)
  , ("Jupiter wobbles the Sun at K ~ 12.5 m/s, the Earth at 0.09 m/s", near (rvK (11.862 * 365.25) mJup) 12.5 0.15 && near (rvK 365.25 mEarth) 0.0894 0.002)
  , ("finds the period from noise-free data (P = 3.7 days, within 0.5%)", near (bp / 3.7) 1 0.005)
  , ("folded dip depth gives Rp/R* = 0.1 (within 10%)", near (sqrt (fdDepth bf)) 0.1 0.01) ]
  where
    tr = Truth 3.7 0.1 0.0 0.3
    d = U.fromList [ (t, fluxAt t tr) | t <- times ]
    (bp, bf) = snd (searchPeriod d)

-- ---------- やってみよう ----------
-- 1. ←→ で周期を変えて、折りたたんだ図の点がそろう周期を自分で探そう。B の答えと比べよう
-- 2. 周期の 2 倍や半分でもくぼみが見えることがある。それはなぜ？
-- 3. sigmaN を 0.005 にすると、地球くらいの惑星（Rp/R* ≈ 0.009）は見つかる？
-- 4. ↑↓ で質量を変えて K を見よう。地球の質量（0.00315 木星質量）だと何 m/s？
-- 5. 減光率と K の両方がわかると、惑星の半径と質量 → 密度（岩石か、ガスか）がわかる

-- ==================================================
-- ここから下は「天文の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 度 → ラジアン
degR :: Double
degR = pi / 180

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 0〜360 にそろえる（JavaScript の ((a % 360) + 360) % 360 と同じ）
wrap360 :: Double -> Double
wrap360 a = jsRem (jsRem a 360 + 360) 360

wrap180 :: Double -> Double
wrap180 a = wrap360 (a + 180) - 180

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 (fin 0 t)) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 (fin 0 t)) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgb255 80 90 120) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (rgb255 160 170 195) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 5) (gY g v + 6) 14 (rgb255 160 170 195) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 38) 14 (rgb255 205 210 225)
     , say yl (gx g) (gy g - 8) 14 (rgb255 205 210 225) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

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

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | abs v >= 1e-6 && abs v < 1e21 = showFFloat Nothing v ""
        | otherwise = show v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)

-- 有効数字 n 桁（toPrecision。末尾の 0 も残す）
toPrec :: Int -> Double -> String
toPrec n v
  | isNaN v || isInfinite v = "0"
  | v == 0 = showFFloat (Just (n - 1)) (0 :: Double) ""
  | e < -6 || e >= n = toExpo (n - 1) v
  | otherwise = showFFloat (Just (max 0 (n - 1 - e))) v ""
  where e = expOf v

-- 指数の形で小数点以下 n 桁（toExponential。例 1.20e+5）
toExpo :: Int -> Double -> String
toExpo n v
  | isNaN v || isInfinite v = "0"
  | v == 0 = showFFloat (Just n) (0 :: Double) "" ++ "e+0"
  | otherwise = showFFloat (Just n) m' "" ++ "e" ++ (if e' >= 0 then "+" else "") ++ show e'
  where
    e = expOf v
    m = v / 10 ^^ e
    r = fromIntegral (round (abs m * 10 ^ n) :: Integer) / 10 ^ n :: Double
    (m', e') = if r >= 10 then (signum m * r / 10, e + 1) else (signum m * r, e)

-- 10 の何乗の桁か
expOf :: Double -> Int
expOf v = let e = floor (logBase 10 (abs v)) :: Int
          in if abs v >= 10 ^^ (e + 1) then e + 1 else if abs v < 10 ^^ e then e - 1 else e

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

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Word32
freshSeed = randomRIO (1, maxBound)
