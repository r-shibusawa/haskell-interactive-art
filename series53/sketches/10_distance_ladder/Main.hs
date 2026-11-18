-- ============================================
-- 53-10 距離のはしご ─ 宇宙の距離は、近い所から順にはしごをかけて測る
-- 1 年周視差：地球が太陽のまわりを回ると、近い星は遠くの星に対して小さな円をえがく。その半径 p [秒角] から d = 1/p [pc]
-- 2 セファイド（脈動する星）：周期 P が長いほど本当に明るい。周期-光度関係 M_V = −2.43 (log₁₀P − 1) − 4.05（Benedict ほか 2007）
--   見かけの明るさ m と比べて、距離指数 m − M = 5 log₁₀ d − 5（星間の吸収は考えない）
-- 3 はしごの全体：レーダー → 年周視差 → セファイド → Ia 型超新星 → ハッブルの法則（13 へ）
-- 操作：1 2 3 = 段 / ↑↓ = 星を選ぶ・見かけの等級 / ←→ = 周期 / M = 測りなおす（視差）/ スペース = 再生/停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

auPerPc, auPerLy :: Double
auPerPc = 1 / tan (pi / 180 / 3600)   -- 1 pc = 1 AU ÷ tan(1″) ≈ 206265 AU
auPerLy = 63241.08                     -- 1 光年 [AU]

noise :: Double
noise = 0.01                           -- ★ 位置の測定の誤差 [秒角]

-- 近い星：名前、年周視差 [秒角]
nearby :: [(String, Double)]
nearby = [("Proxima Centauri", 0.7685), ("Sirius", 0.3792), ("Altair", 0.1949), ("Vega", 0.1302)]

-- 12 か月の測定（時刻 [年], 横の位置 [秒角]）と、当てはめた視差
data Meas = Meas { mTs, mXs :: [Double], mP :: !Double }

data World = World
  { wStage   :: !Int
  , wSi      :: !Int
  , wTYear   :: !Double
  , wPlaying :: !Bool
  , wMeas    :: Meas
  , wPeriod  :: !Double                -- ★ δ ケフェウスの周期 [日]
  , wMApp    :: !Double                -- ★ 平均の見かけの等級
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-10 Distance ladder" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      s <- freshSeed
      pure (measure world0 { wRng = seedRng s, wTest = selfTest })
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wTYear = wTYear w + 1 / 600 } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = measure (World 1 0 0 True (Meas [] [] 0) 5.366 3.95 (seedRng 1) [])

-- 遠くの星（動かない）。種 7 で 40 個
bgStars :: [(Double, Double)]
bgStars = fst (runRand (mapM (const ((,) <$> fmap (subtract 0.5) rnd <*> fmap (subtract 0.5) rnd)) [1 .. 40 :: Int]) (seedRng 7))

distPc :: Double -> Double
distPc p = 1 / max p 1e-9

-- 12 か月の測定：x(t) = p cos(2πt) + 誤差 → a cos + b sin を最小二乗で → p = √(a² + b²)
fitParallax :: [Double] -> [Double] -> Double
fitParallax ts xs
  | abs det < 1e-12 = 0
  | otherwise = sqrt (a * a + b * b)
  where
    (cc, ss, cs, xc, xsn) = foldl' acc (0, 0, 0, 0, 0) (zip ts xs)
    acc (q1, q2, q3, q4, q5) (t, x) = let c = cos (2 * pi * t); s = sin (2 * pi * t)
                                      in (q1 + c * c, q2 + s * s, q3 + c * s, q4 + x * c, q5 + x * s)
    det = cc * ss - cs * cs
    a = (xc * ss - xsn * cs) / det
    b = (xsn * cc - xc * cs) / det

measure :: World -> World
measure w = w { wMeas = Meas ts xs (fitParallax ts xs), wRng = r' }
  where
    p = snd (getAt ("", 0.7685) nearby (wSi w))
    ts = [ fromIntegral k / 12 | k <- [0 .. 11 :: Int] ]
    (es, r') = runRand (mapM (const rndNormal) ts) (wRng w)
    xs = [ p * cos (2 * pi * t) + noise * e | (t, e) <- zip ts es ]

-- 周期-光度関係
plM :: Double -> Double
plM p = -2.43 * (logBase 10 (max p 0.1) - 1) - 4.05

-- [pc]
distFromModulus :: Double -> Double -> Double
distFromModulus m mm = 10 ** ((m - mm + 5) / 5)

modulus :: Double -> Double
modulus d = 5 * logBase 10 d - 5

-- セファイドの光度曲線の形（すばやく明るくなり、ゆっくり暗くなる）位相 0〜1 → 等級の変化
cephShape :: Double -> Double
cephShape ph0 = if ph < 0.2 then 0.4 - (ph / 0.2) * 0.8 else -0.4 + ((ph - 0.2) / 0.8) * 0.8
  where ph = ph0 - fromIntegral (floor ph0 :: Int)

draw :: Env -> World -> Picture
draw env w = view env $
  (case wStage w of
     1 -> drawParallax env w
     2 -> drawCepheid env w
     _ -> drawLadder env)
  ++ [ say ("Distance ladder | 1 parallax  2 Cepheids  3 whole ladder" ++ getAt "" ["", "  | Up/Down star  M measure  Space play", "  | Left/Right period  Up/Down apparent mag", ""] (wStage w)) 20 34 16 (rgb255 240 236 215)
     , drawSelfTest env (wTest w) ]

drawParallax :: Env -> World -> [Picture]
drawParallax env w =
  -- 真上から：太陽・地球の軌道・近い星（距離は縮めて描く）・遠くの星
  [ Color (rgb255 70 110 200) (ringD cx cy (2 * rr))
  , Color (rgb255 255 210 90) (circleD cx cy 16)
  , Color (rgb255 90 150 240) (circleD ex ey 12)
  , Color (rgb255 255 240 200) (circleD cx sy 10)
  , say name (cx + 10) (sy + 5) 14 (rgb255 200 205 225)
  , say ("(really " ++ toExpo 2 (d * auPerPc) ++ " AU away)") (cx + 10) (sy + 23) 14 (rgb255 200 205 225)
  , Color (rgba255 255 230 160 120) (Line [(ex, ey), (cx, sy)])   -- 地球から星への視線（季節で向きが変わる）
  , say "Top view (distance to the star shrunk a lot)" (cx - 150) (cy + rr + 30) 14 (rgb255 160 170 200)
  -- 望遠鏡で見た位置（右）：遠くの星は動かず、近い星は半径 p の円をえがく
  , Color (rgb255 70 80 120) (frameTL 1 (vx - vr * 1.2) (vy - vr * 1.2) (vr * 2.4) (vr * 2.4)) ]
  ++ [ Color (rgb255 200 200 220) (circleD (vx + f2 bx * vr * 2.3) (vy + f2 by * vr * 2.3) 2.5) | (bx, by) <- bgStars ]
  ++ [ Color (rgba255 255 230 160 100) (ringD vx vy (2 * f2 p * sc))
     , Color (rgb255 255 240 200) (circleD (vx + f2 p * sc * cos ea) (vy - f2 p * sc * sin ea) 10) ]
  ++ [ Color (rgb255 255 140 110) (circleD (vx + f2 x * sc) (vy + vr * 1.1) 5) | x <- mXs (wMeas w) ]   -- 測るのは横方向（x）の位置
  ++ [ say "Telescope view (box = +-1.2\", red = monthly x)" (vx - vr * 1.2) (vy - vr * 1.2 - 8) 14 (rgb255 200 205 225)
  -- 数字
     , say (name ++ ": parallax p = " ++ showJ p ++ "\" -> d = 1/p = " ++ fmt 3 d ++ " pc = " ++ fmt 2 (d * auPerPc / auPerLy) ++ " ly") 20 yt 16 (rgb255 250 225 140)
     , say ("12 measurements (error " ++ showJ noise ++ "\") fit with cos/sin: p = " ++ fmt 4 (mP (wMeas w)) ++ "\" -> d = "
            ++ fmt 3 (distPc (mP (wMeas w))) ++ " pc (M: again)") 20 (yt + 26) 15 (rgb255 220 225 240)
     , say ("1 pc = 1 AU / tan 1\" = " ++ fmt 0 auPerPc ++ " AU = " ++ fmt 3 (auPerPc / auPerLy) ++ " ly. When p nears the error ("
            ++ showJ noise ++ "\"), d is lost") 20 (yt + 50) 15 (rgb255 220 225 240) ]
  where
    (name, p) = getAt ("", 0.7685) nearby (wSi w)
    d = distPc p
    h = envH env
    cx = envW env * 0.22
    cy = h * 0.5
    rr = min 90 (h * 0.12)
    ea = f2 (2 * pi * wTYear w)
    ex = cx + rr * cos ea
    ey = cy - rr * sin ea
    sy = cy - h * 0.4
    vx = envW env * 0.62
    vy = h * 0.4
    vr = min (envW env * 0.18) (h * 0.25)
    sc = vr / 1.0                      -- 1 秒角 = vr
    yt = vy + vr * 1.2 + 40

drawCepheid :: Env -> World -> [Picture]
drawCepheid env w =
  -- 光度曲線
  [ plotAxes g "day" "Apparent magnitude m (brighter = up)" [ roundTo 1 v | v <- [0, period, 2 * period, 3 * period] ] [ roundTo 2 v | v <- [mApp - 0.5, mApp, mApp + 0.5] ]
  , Color (rgb255 250 225 140) (polyline 2 [ (gX g t, gY g (mApp + cephShape (t / period))) | i <- [0 .. 300 :: Int], let t = (fromIntegral i / 300) * 3 * period ])
  , Color white (circleD (gX g tt) (gY g (mApp + cephShape (tt / period))) 10)
  -- 周期-光度関係
  , plotAxes g2 "Period P [days] (log)" "True brightness (absolute magnitude) M_V" [1, 3, 10, 30, 100] [-1, -3, -5, -7]
  , Color (rgb255 140 190 255) (polyline 2 [(gX g2 1, gY g2 (plM 1)), (gX g2 100, gY g2 (plM 100))])
  , Color (rgb255 255 140 110) (circleD (gX g2 period) (gY g2 mm) 12) ]
  -- 星の明るさのまる
  ++ [ Color (rgba255 255 235 190 20) (circleD sx sy (br * 2 + fromIntegral i * 12)) | i <- [5, 4 .. 1 :: Int] ]
  ++ [ Color (rgb255 255 240 210) (circleD sx sy (br * 2)) ]
  ++ concat [ [ say a x0 (y0 + fromIntegral i * lh) 15 (rgb255 200 205 225), say b (x0 + 250) (y0 + fromIntegral i * lh) 15 (rgb255 250 225 140) ]
            | (i, (a, b)) <- zip [0 :: Int ..]
                [ ("Period P", fmt 2 period ++ " days"), ("P-L relation -> M_V", fmt 2 mm), ("Mean apparent mag m", fmt 2 mApp)
                , ("Modulus m - M", fmt 2 (mApp - mm)), ("d = 10^((m-M+5)/5)", fmtD d) ] ]
  ++ [ say s x0 (y0 + 5 * lh + 14 + fromIntegral i * 20) 14 (rgb255 170 175 200)
     | (i, s) <- zip [0 :: Int ..]
         [ "Start: delta Cephei (P 5.366 d, m 3.95)", "No dust absorption -> a bit too far"
         , "Up/Down: larger m (fainter) = farther", "5 mag fainter = 1/100 light = 10x d" ] ]
  where
    period = wPeriod w
    mApp = wMApp w
    mm = plM period
    d = distFromModulus mApp mm
    ww = envW env
    h = envH env
    g = Graph 70 110 (ww * 0.5) (h * 0.32) 0 (3 * period) (mApp + 0.7) (mApp - 0.7) False False
    tt = jsRem (wTYear w * 365.25 * 0.2) (3 * period)
    g2 = Graph 70 (gy g + gh g + 80) (ww * 0.5) (h * 0.3) 1 100 (-1) (-7.5) True False
    sx = ww * 0.78
    sy = h * 0.3
    br = 30 + 8 * f2 (cephShape (tt / period)) * (-1)
    x0 = ww * 0.62
    y0 = h * 0.55
    lh = 26

-- 小数点以下 n 桁に丸めた数（+v.toFixed(n)）
roundTo :: Int -> Double -> Double
roundTo n v = fromIntegral (round (v * 10 ^ n) :: Integer) / 10 ^ n

fmtD :: Double -> String
fmtD pc | pc < 1000 = fmt 0 pc ++ " pc"
        | pc < 1e6 = fmt 1 (pc / 1000) ++ " kpc"
        | otherwise = fmt 2 (pc / 1e6) ++ " Mpc"

drawLadder :: Env -> [Picture]
drawLadder env =
  [ Color (rgb255 120 130 170) (Line [(x0, y), (x1, y)]) ]
  ++ [ sayC ("10^" ++ show e ++ " pc") (xp (10 ^^ e)) (y + 24) 14 (rgb255 160 170 195) | e <- [-8, -6 .. 10 :: Int] ]
  ++ concat [ [ Color (withA (160 / 255) c) (rectTL (xp a) yy (xp b - xp a) 18), say nm (xp a + 6) (yy - 6) 14 (gray 0.9) ]
            | (i, (nm, a, b, c)) <- zip [0 :: Int ..] rungs, let yy = y - 60 - fromIntegral i * 46 ]
  ++ concat [ [ Color white (circleD x y 8), sayC nm x (y + 48 + fromIntegral (i `mod` 2) * 18) 14 (rgb255 220 225 240) ]
            | (i, (nm, d)) <- zip [0 :: Int ..] objs, let x = xp d ]
  ++ [ say "Calibrate where neighbouring rungs overlap. Errors of a lower rung pass to every rung above." x0 (envH env * 0.2) 15 (rgb255 220 225 240)
     , say ("1 pc = " ++ fmt 0 auPerPc ++ " AU = 3.26 ly. From the Moon to the edge of the universe: about 19 digits") x0 (envH env * 0.2 + 26) 15 (rgb255 220 225 240) ]
  where
    x0 = 80
    x1 = envW env - 60
    y = envH env * 0.62
    lo = -9
    hi = 10.5
    xp pc = x0 + f2 ((logBase 10 pc - lo) / (hi - lo)) * (x1 - x0)      -- 横軸：log₁₀(距離 [pc])
    rungs = [ ("Radar / laser (solar system)", 2e-9, 3e-4, rgb255 140 190 255), ("Parallax (Gaia: to a few 1000 pc)", 1, 1e4, rgb255 250 225 140)
            , ("Cepheids", 1e3, 4e7, rgb255 255 170 120), ("Type Ia supernovae", 1e6, 3e9, rgb255 220 140 255), ("Hubble v = H0 d (13)", 1e7, 1e10, rgb255 140 230 180) ]
    objs = [ ("Moon", 384400e3 / 1.496e11 / auPerPc), ("Sun", 1 / auPerPc), ("Neptune", 30 / auPerPc), ("Proxima", 1.30), ("delta Cep", 270)
           , ("LMC", 5.0e4), ("Andromeda", 7.7e5), ("Virgo cluster", 1.65e7) ]

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wStage = fromEnum c - fromEnum '0' }
    | c `elem` "mM" && wStage w == 1 -> pure (measure w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey k) Down _ _
    | wStage w == 1 -> pure $ case k of
        KeyUp -> measure w { wSi = (wSi w + 1) `mod` n }
        KeyDown -> measure w { wSi = (wSi w + n - 1) `mod` n }
        _ -> w
    | wStage w == 2 -> pure $ case k of
        KeyRight -> w { wPeriod = min 100 (wPeriod w * 1.1) }
        KeyLeft -> w { wPeriod = max 1 (wPeriod w / 1.1) }
        KeyUp -> w { wMApp = max (-1) (wMApp w - 0.5) }
        KeyDown -> w { wMApp = min 30 (wMApp w + 0.5) }
        _ -> w
  _ -> pure w
  where n = length nearby

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("p = 0.1\" -> d = 10 pc, Proxima (0.7685\") -> 1.30 pc", near (distPc 0.1) 10 1e-9 && near (distPc 0.7685) 1.301 0.001)
  , ("1 pc = 206265 AU = 3.262 ly", near auPerPc 206264.8 0.5 && near (auPerPc / auPerLy) 3.2616 0.001)
  , ("noise-free measurements give the exact parallax (least squares with cos and sin)", near (fitParallax ts xs) 0.3792 1e-12)
  , ("distance modulus: m - M = 0 at 10 pc, 10 at 1 kpc, and back to the distance"
    , near (modulus 10) 0 1e-12 && near (modulus 1000) 10 1e-12 && near (distFromModulus (13.7 + plM 20) (plM 20)) (10 ** (13.7 / 5 + 1)) 1e-6)
  , ("P-L relation: M_V = -4.05 at P = 10 days, double the period = 0.73 mag brighter", near (plM 10) (-4.05) 1e-12 && near (plM 10 - plM 20) (2.43 * logBase 10 2) 1e-12)
  , ("5 magnitudes = 100x the brightness = 10x the distance", near (distFromModulus 15 0 / distFromModulus 10 0) 10 1e-9)
  , ("delta Cephei is roughly 200 to 350 pc away (no absorption)", dC > 200 && dC < 350) ]
  where
    ts = [ fromIntegral k / 12 | k <- [0 .. 11 :: Int] ]
    xs = [ 0.3792 * cos (2 * pi * t + 0.7) | t <- ts ]
    dC = distFromModulus 3.95 (plM 5.366)

-- ---------- やってみよう ----------
-- 1. 1 で ↑↓ を押して、遠い星ほど円（視差）が小さくなることを確かめよう
-- 2. noise を 0.1 にすると、ベガの距離はどれくらいばらつく？ M で何回か測りなおそう
-- 3. 2 で周期を 30 日、見かけの等級を 25 にすると、どれくらい遠い？（ほかの銀河まで届く）
-- 4. 3 のはしごで、自分が知っている天体の距離を objs に足してみよう
-- 5. 光は 1 年に 1 光年進む。アンドロメダ銀河の光は、何年前に出た光？

-- 円の線（直径 d）
ringD :: Float -> Float -> Float -> Picture
ringD x y d = at (x, y) (Circle (d / 2))

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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
