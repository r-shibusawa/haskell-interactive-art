-- ============================================
-- 45-05 遊星歯車
-- 太陽歯車・遊星歯車・内歯車（リング）・キャリアの 4 つ。どれを固定するかで速度比が変わる
-- ウィリスの式：(ω太陽 − ωキャリア) / (ωリング − ωキャリア) = − zリング / z太陽
-- 操作：1 = 内歯車を固定 / 2 = 太陽を固定 / 3 = キャリアを固定 / 4 = 直結 / ←→ = 太陽の歯数 / ↑↓ = 入力の速さ / スペース = 止める
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

sunList :: [Int]
sunList = [12, 18, 24, 30, 36]   -- ★ 太陽の歯数の候補（内歯車は 60 枚、遊星 3 個で組み立てられる組）

ringZ :: Int
ringZ = 60                       -- 内歯車の歯数

planetCount :: Int
planetCount = 3                  -- 遊星の数

gearM :: Float
gearM = 8                        -- ★ モジュール（px）

pa :: Float
pa = 20 * pi / 180

data World = World
  { wSunId   :: !Int
  , wFix     :: !Int             -- 1 リング固定 / 2 太陽固定 / 3 キャリア固定 / 4 直結
  , wSpeed   :: !Float           -- ★ 入力の速さ（ラジアン/フレーム）
  , wRotSun  :: !Float
  , wRotCar  :: !Float
  , wPlaying :: !Bool
  }

main :: IO ()
main = runSketch (sketch "45-05 Planetary gear" (World 2 1 0.02 0 0 True))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

bgC :: Color
bgC = hsb 212 45 13

sunZ :: World -> Int
sunZ w = case drop (wSunId w) sunList of
  (z : _) -> z
  [] -> 24

-- 遊星の歯数は zリング = z太陽 + 2 z遊星 から決まる
planetZ :: World -> Int
planetZ w = (ringZ - sunZ w) `div` 2

-- ★ ウィリスの式から、太陽とキャリアの角速度を決める（リングは式から出る）
speeds :: World -> (Float, Float)
speeds w = case wFix w of
  1 -> (v, v * zs / (zs + fromIntegral ringZ))   -- リング固定：太陽 → キャリア
  2 -> (0, v)                                     -- 太陽固定：キャリア → リング
  3 -> (v, 0)                                     -- キャリア固定：太陽 → リング（逆転）
  _ -> (v, v)                                     -- 直結：全体が一体で回る
  where v = wSpeed w
        zs = fromIntegral (sunZ w)

ringSpeed :: World -> Float
ringSpeed w = let (ws, wc) = speeds w in wc - (ws - wc) * fromIntegral (sunZ w) / fromIntegral ringZ

step :: World -> World
step w
  | wPlaying w = let (ws, wc) = speeds w in w { wRotSun = wRotSun w + ws, wRotCar = wRotCar w + wc }
  | otherwise = w

draw :: Env -> World -> Picture
draw env w = view env $
  -- 内歯車：大きな円を塗り、内側を「60 枚の歯車の形」にくりぬく
  [ dot ringC ccx ccy (fit * (2 * rr + 7 * gearM))
  , fillOutline bgC (hsbA 0 0 100 60) outRing tf (0, 0) rotRing
  , let (x, y) = tf ((rr + 2.4 * gearM) * cos rotRing, (rr + 2.4 * gearM) * sin rotRing) in dot (hsb 0 0 100) x y (9 * fit)
  -- 太陽
  , fillOutline sunC (hsbA 0 0 100 70) outSun tf (0, 0) rotSun
  , let (x, y) = tf ((rs - 2.5 * gearM) * cos rotSun, (rs - 2.5 * gearM) * sin rotSun) in dot (hsb 0 0 100) x y (7 * fit) ]
  -- 遊星
  ++ concat [ [ fillOutline (hsb 200 55 85) (hsbA 0 0 100 70) outPlanet tf (x, y) rotP
              , let (mx, my) = tf (x + (rp - 2.5 * gearM) * cos rotP, y + (rp - 2.5 * gearM) * sin rotP) in dot (hsb 0 0 100) mx my (6 * fit) ]
            | (x, y, rotP) <- planets ]
  -- キャリア（遊星の軸をつなぐ腕）
  ++ [ seg 10 carC (ccx, ccy) (tf (x, y)) | (x, y, _) <- planets ]
  ++ [ let (sx, sy) = tf (x, y) in dot (hsb 0 0 100) sx sy (12 * fit) | (x, y, _) <- planets ]
  ++ [ dot (hsb 0 0 100) ccx ccy (14 * fit) ]
  -- 文字
  ++ [ txt 20 40 18 (hsb 0 0 100) ("1-4: " ++ names !! (fixI - 1) ++ "   Left/Right: sun teeth " ++ show zs ++ "   Up/Down: input speed   Space: "
                                    ++ (if wPlaying w then "stop" else "run"))
     , txt 20 68 15 (hsb 0 0 60) "Gray = the fixed part. Same gears, different fixed part: reduction, step-up or reverse (automatic transmissions)."
     , Color (hsbA 0 0 100 8) (rect (ix - 16 + 230) (iy - 30 + 165) 460 330)
     , txt ix iy 16 (hsb 0 0 100) "Willis equation"
     , txt ix (iy + 26) 16 (hsb 0 0 100) ("(ws - wc) / (wr - wc) = - zr / zs = " ++ fmt 2 (negate (fromIntegral ringZ / fromIntegral zs)))
     , txt ix (iy + 54) 14 (hsb 0 0 70) ("Teeth: zs = " ++ show zs ++ "  zp = " ++ show zp ++ "  zr = " ++ show ringZ ++ "  (zr = zs + 2 zp)") ]
  -- 3 つの角速度のバー
  ++ concat [ [ txt ix (y + 5) 14 (hsb 0 0 100) nm
              , seg 1 (hsb 0 0 40) (ix + 250, y - 12) (ix + 250, y + 12)
              , Color (hsb hue 60 100) (let bw = clampF (-1.6) 1.6 (v / 0.06) * 110 in rect (ix + 250 + bw / 2) y (abs bw) 14)
              , txt (ix + 100) (y + 5) 14 (hsb 0 0 100) (if abs v < 1e-9 then "fixed" else fmt 3 (v / wSpeed w)) ]
            | (i, (nm, v, hue)) <- zip [0 :: Int ..] [("sun ws", ws, 40), ("carrier wc", wc, 320), ("ring wr", wr, 150)]
            , let y = iy + 96 + fromIntegral i * 40 ]
  ++ [ txt ix (iy + 228) 16 (hsb 35 90 100) ("Input: " ++ inName ++ " -> output: " ++ outName)
     , txt ix (iy + 254) 16 (hsb 35 90 100) ("Ratio i = w_in / w_out = " ++ fmt 3 ratio
         ++ (if ratio < 0 then " (reverse)" else if abs ratio > 1.001 then " (reduction)" else if abs ratio < 0.999 then " (step-up)" else " (1 : 1)"))
     , txt ix (iy + 280) 14 (hsb 0 0 60) (formulas !! (fixI - 1) ++ "     bars: speed with input = 1") ]
  where
    fixI = max 1 (min 4 (wFix w))
    zs = sunZ w
    zp = planetZ w
    (ws, wc) = speeds w
    wr = ringSpeed w
    ccx = envW env * 0.36
    ccy = envH env * 0.56
    rs = gearM * fromIntegral zs / 2
    rp = gearM * fromIntegral zp / 2
    rr = gearM * fromIntegral ringZ / 2
    fit = min 1 ((envH env - 190) / (2 * rr + 6 * gearM))
    tf (x, y) = (ccx + fit * x, ccy + fit * y)       -- 歯車の座標 → 画面
    outSun = makeGearOutline zs gearM pa 1 1.25
    outPlanet = makeGearOutline zp gearM pa 1 1.25
    outRing = makeGearOutline ringZ gearM pa 1.25 1   -- 内歯車は「歯のすき間」の形を描く
    -- かみ合いの角度：遊星 0 の向き g0 を基準に、遊星とリングの回転角を決める
    g0 = wRotCar w
    rotSun = wRotSun w
    zsF = fromIntegral zs
    zpF = fromIntegral zp
    rotRing = g0 + pi * (zpF + 1) / fromIntegral ringZ - (rotSun - g0) * zsF / fromIntegral ringZ
    planets = [ (x, y, g + pi + pi / zpF - (rotSun - g) * zsF / zpF)
              | i <- [0 .. planetCount - 1]
              , let g = g0 + fromIntegral i * tau / fromIntegral planetCount
                    x = (rs + rp) * cos g
                    y = (rs + rp) * sin g ]
    ringC = if wFix w == 1 then hsb 0 0 38 else hsb 150 45 60
    sunC = if wFix w == 2 then hsb 0 0 45 else hsb 40 75 95
    carC = if wFix w == 3 then hsbA 0 0 55 85 else hsbA 320 55 100 85
    names = ["ring fixed", "sun fixed", "carrier fixed", "locked (all turn together)"]
    inName = if wFix w == 2 then "carrier" else "sun"
    outName = if wFix w == 1 then "carrier" else "ring"
    wOut = if wFix w == 1 then wc else wr
    ratio = wSpeed w / (if abs wOut < 1e-12 then 1e-12 else wOut)
    formulas = ["i = 1 + zr / zs", "i = zr / (zs + zr)", "i = - zr / zs", "i = 1"]
    ix = envW env - 470
    iy = 130

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _ | k >= '1' && k <= '4' -> w { wFix = fromEnum k - fromEnum '0' }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSunId = max 0 (wSunId w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSunId = min (length sunList - 1) (wSunId w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = clampF 0.005 0.06 (wSpeed w + 0.005) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = clampF 0.005 0.06 (wSpeed w - 0.005) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（リング固定）で太陽の歯数を 12 → 36 と変える。速度比 1 + zr/zs は 6 から 2.67 まで変わる。確かめよう
-- 2. 3（キャリア固定）では遊星はその場で回るだけ（ふつうの歯車列）。リングが逆転する理由を説明しよう
-- 3. 2（太陽固定）は増速？減速？ 自転車の内装変速機はこれを使っている
-- 4. 遊星 3 個を等間隔に置ける条件は (zs + zr) / 3 が整数。sunList に 20 を足すとどうなる？（歯がぶつかる）
-- 5. ハイブリッド車の動力分割機構：太陽 = 発電機、キャリア = エンジン、リング = タイヤ。エンジン一定でリングを止めると太陽は？

-- ---------- 歯車の道具（インボリュート歯形の輪郭を作る） ----------
-- 輪郭：歯底円の半径、歯 1 枚の側面 [(半径, 歯の中心からの角度)]、歯数
data Outline = Outline { oRf :: !Float, oFlank :: ![(Float, Float)], oZ :: !Int }

-- 歯数 z、モジュール m、圧力角 pa（ラジアン）、歯先・歯元の高さ ha・hf（モジュールの何倍か）
--   歯 1 枚の中心が角度 0（+x 方向）を向く
--   インボリュート：基礎円 rb に巻いた糸をほどくときの糸の先。ほどいた角 t の点は
--   半径 rb√(1 + t²)、角度 inv = t − atan t
makeGearOutline :: Int -> Float -> Float -> Float -> Float -> Outline
makeGearOutline z m pa' ha hf = Outline rf flank z
  where
    zf = fromIntegral z
    r = m * zf / 2
    rb = r * cos pa'
    ra = r + ha * m
    rf = max (r - hf * m) m
    half = pi / (2 * zf) + tan pa' - pa'                  -- 基礎円の上での歯の半角
    t0 = if rf > rb then sqrt ((rf / rb) * (rf / rb) - 1) else 0
    t1 = sqrt (max ((ra / rb) * (ra / rb) - 1) (t0 * t0))
    flank = [(rf, half) | rf < rb]                         -- 基礎円より内側はまっすぐ
         ++ [ (rb * sqrt (1 + t * t), max (half - (t - atan t)) 0)
            | i <- [0 .. 8 :: Int], let t = t0 + (t1 - t0) * fromIntegral i / 8 ]

-- 輪郭を中心 c・角度 rot に置いて塗って縁取る（tf は歯車の座標 → 画面の変換）
--   歯車の形は凹んでいるので、歯底円の円板と、歯 1 枚ずつの凸な多角形に分けて塗る
fillOutline :: Color -> Color -> Outline -> (Point -> Point) -> Point -> Float -> Picture
fillOutline fc lc o tf c rot = Pictures
  [ Color fc (Polygon [tf (rotPt c rot (oRf o * cos a, oRf o * sin a)) | k <- [0 .. 4 * oZ o], let a = fromIntegral k * tau / fromIntegral (4 * oZ o)])
  , Color fc (Pictures [Polygon (tooth k) | k <- [0 .. oZ o - 1]])
  , Color lc (poly (concat [rootMid k : tooth k | k <- [0 .. oZ o - 1]])) ]
  where
    zf = fromIntegral (oZ o)
    ang k = fromIntegral k * tau / zf
    pt r a = tf (rotPt c rot (r * cos a, r * sin a))
    rootMid k = pt (oRf o) (ang k - pi / zf)                         -- 歯底の真ん中
    tooth k = [pt fr (ang k - fa) | (fr, fa) <- oFlank o] ++ [pt fr (ang k + fa) | (fr, fa) <- reverse (oFlank o)]

rotPt :: Point -> Float -> Point -> Point
rotPt (cx, cy) rot (x, y) = (cx + x * cos rot - y * sin rot, cy + x * sin rot + y * cos rot)

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
