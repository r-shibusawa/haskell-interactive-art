-- ============================================
-- 45-11 回転体の不つり合いと危険速度
-- 重心が軸の中心からわずかにずれた円板を回すと、遠心力で軸がたわみ、振れ回る
-- 回転数が軸の固有振動数に一致するところ（危険速度）で振れが最大。それを超えると重心が回転中心に寄って静かになる（自動調心）
-- 操作：↑↓（押し続ける）= 回転数 / スペース = 止めて調整 / 円板をクリック = つり合いおもりを置く / 1〜3 = おもりの重さ
--       Z = 1 つ戻す / C = 全部外す / N = 新しい不つり合い / H = 重い点を隠す
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

zeta, naturalHz, eccUnit, discR :: Float
zeta = 0.06               -- ★ 減衰比
naturalHz = 1.2           -- ★ 軸の固有振動数（Hz）。危険速度はこれと同じ回転数
eccUnit = 14              -- ★ 重さ 1 のおもりを円板のふちに付けたときの重心のずれ（px）
discR = 105               -- 円板の半径（px）

-- おもり：円板に固定した座標 (x, y) と重さ w
data Mass = Mass { mX :: !Float, mY :: !Float, mW :: !Float }

-- 軸中心の [x, vx, y, vy]
data S4 = S4 !Float !Float !Float !Float

data World = World
  { wHeavy   :: !Mass       -- もともとの不つり合い（重い点）
  , wWeights :: ![Mass]     -- 置いたつり合いおもり（新しい順）
  , wSize    :: !Float      -- 次に置くおもりの重さ
  , wGoal    :: !Float      -- 回転数 / 危険速度（目標）
  , wSpeed   :: !Float      -- 回転数 / 危険速度（現在）
  , wSpin    :: !Float      -- 回転角
  , wRs      :: !S4
  , wOrbit   :: ![Point]    -- 軌跡（新しい順、240 個まで）
  , wPaused  :: !Bool
  , wHide    :: !Bool
  }

main :: IO ()
main = runSketch (sketch "45-11 Rotor unbalance and critical speed" (World (Mass 0 0 1) [] 0.5 0.5 0 0 (S4 0 0 0 0) [] False False))
  { skBack  = hsb 212 45 13
  , skStart = \_ -> newUnbalance (World (Mass 0 0 1) [] 0.5 0.5 0 0 (S4 0 0 0 0) [] False False)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

newUnbalance :: World -> IO World
newUnbalance w = do
  a <- randomRIO (0, tau)
  pure w { wHeavy = Mass (discR * 0.8 * cos a) (discR * 0.8 * sin a) 1, wWeights = [], wOrbit = [] }

-- ★ 重心のずれ e（円板に固定した座標）：すべてのおもりの「重さ × 位置」の合計
eccentricity :: World -> Point
eccentricity w = (ex * eccUnit, ey * eccUnit)
  where
    ms = wHeavy w : wWeights w
    ex = sum [mW q * mX q / discR | q <- ms]
    ey = sum [mW q * mY q / discR | q <- ms]

-- ★ 運動方程式：x″ + 2ζωn x′ + ωn² x = Ω² (e を回転させたものの x 成分)。y も同じ
deriv :: S4 -> Float -> Float -> Point -> S4
deriv (S4 x vx y vy) ang om (ex, ey) =
  S4 vx (fx - 2 * zeta * wn * vx - wn * wn * x) vy (fy - 2 * zeta * wn * vy - wn * wn * y)
  where
    wn = 2 * pi * naturalHz
    fx = om * om * (ex * cos ang - ey * sin ang)
    fy = om * om * (ex * sin ang + ey * cos ang)

addS :: S4 -> S4 -> Float -> S4
addS (S4 a b c d) (S4 e f g h) k = S4 (a + e * k) (b + f * k) (c + g * k) (d + h * k)

rk4 :: Float -> Float -> Point -> World -> World
rk4 dt om e w = w { wRs = S4 (cl x1 k1a k2a k3a k4a) (cl x2 k1b k2b k3b k4b) (cl x3 k1c k2c k3c k4c) (cl x4 k1d k2d k3d k4d)
                  , wSpin = sp + om * dt }
  where
    s = wRs w
    sp = wSpin w
    k1 = deriv s sp om e
    k2 = deriv (addS s k1 (dt / 2)) (sp + om * dt / 2) om e
    k3 = deriv (addS s k2 (dt / 2)) (sp + om * dt / 2) om e
    k4 = deriv (addS s k3 dt) (sp + om * dt) om e
    S4 x1 x2 x3 x4 = s
    S4 k1a k1b k1c k1d = k1
    S4 k2a k2b k2c k2d = k2
    S4 k3a k3b k3c k3d = k3
    S4 k4a k4b k4c k4d = k4
    cl v a b c d = clampF (-1e4) 1e4 (v + (a + 2 * b + 2 * c + d) * dt / 6)

-- 理論の振れ回り半径：|e| r² / √((1 − r²)² + (2ζr)²)
whirl :: Float -> Float -> Float
whirl r eMag = eMag * r * r / sqrt ((1 - r * r) ^ (2 :: Int) + (2 * zeta * r) ^ (2 :: Int))

step :: Env -> World -> World
step env w0
  | wPaused w1 = w1
  | otherwise =
      let sp = wSpeed w1 + clampF (-0.004) 0.004 (wGoal w1 - wSpeed w1)   -- 回転数はゆっくり変わる
          om = sp * 2 * pi * naturalHz
          e = eccentricity w1
          w2 = iterate (rk4 (1 / 480) om e) w1 { wSpeed = sp } !! 8
          S4 x _ y _ = wRs w2
      in w2 { wOrbit = take 240 ((x, y) : wOrbit w2) }
  where
    g = wGoal w0 + (if isDown env (SpecialKey KeyUp) then 0.012 else 0) - (if isDown env (SpecialKey KeyDown) then 0.012 else 0)
    w1 = w0 { wGoal = clampF 0 3 g }

-- 軸の中心 S の画面位置
shaftPos :: Env -> World -> Point
shaftPos env w = let S4 x _ y _ = wRs w in (envW env * 0.27 + clampF (-170) 170 x, envH env * 0.55 + clampF (-170) 170 y)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：軸方向から見た図。O = 軸受の中心、S = 軸の中心、G = 重心
  [ seg 1 (hsb 0 0 40) (ocx - 220, ocy) (ocx + 220, ocy), seg 1 (hsb 0 0 40) (ocx, ocy - 220) (ocx, ocy + 220) ]
  ++ [ path 1.5 (hsbA 50 90 100 60) [ (ocx + clampF (-170) 170 x, ocy + clampF (-170) 170 y) | (x, y) <- wOrbit w ] | length (wOrbit w) > 1 ]
  ++ [ dot (hsbA 200 40 55 80) sx sy (discR * 2), circ 2 (hsb 200 50 100) sx sy (discR * 2) ]
  ++ [ seg 1 (hsbA 200 50 100 50) (tr (discR * 0.9 * cos a, discR * 0.9 * sin a)) (tr (discR * cos a, discR * sin a))
     | k <- [0 .. 11 :: Int], let a = fromIntegral k * pi / 6 ]
  ++ [ seg 1 (hsbA 200 50 100 50) (tr (0, 0)) (tr (discR * 0.9, 0)) ]
  ++ [ uncurry (dot (hsb 0 85 100)) (tr (mX hv, mY hv)) 20 | not (wHide w) ]
  ++ [ uncurry (dot (hsb 140 70 100)) (tr (mX q, mY q)) (10 + mW q * 12) | q <- wWeights w ]
  ++ [ uncurry (dot (hsb 50 90 100)) (tr e) 9                                 -- 重心 G
     , seg 2 (hsbA 0 0 100 70) (ocx, ocy) (sx, sy)
     , dot (hsb 0 0 100) ocx ocy 8, dot (hsb 0 0 100) sx sy 10
     , txt (ocx - 16) (ocy - 8) 14 (hsb 0 0 100) "O", txt (sx + 8) (sy - 8) 14 (hsb 0 0 100) "S" ]
  -- ---- 右：振れ回り半径と回転数
  ++ [ seg 1 (hsb 0 0 45) (gx, gy) (gx, gy + gh), seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh)
     , seg 1 (hsbA 0 80 100 50) (xR 1, gy) (xR 1, gy + gh)
     , path 1.5 (hsbA 0 0 100 30) [ (xR r, yV (whirl r (eccUnit * 0.8))) | i <- [0 .. 150 :: Int], let r = fromIntegral i / 50 ]   -- 修正前
     , path 2.5 (hsb 200 60 100) [ (xR r, yV (whirl r eMag)) | i <- [0 .. 150 :: Int], let r = fromIntegral i / 50 ]               -- いまの e
     , seg 1 (hsbA 140 60 100 60) (gx, yV eMag) (gx + gw, yV eMag)
     , dot (hsb 50 90 100) (xR (wSpeed w)) (yV radNow) 13 ]
  ++ [ txt (xR r - 10) (gy + gh + 18) 14 (hsb 0 0 60) (fmt 1 r) | r <- [0, 0.5 .. 3] ]
  ++ [ rtxt (gx + gw) (gy + gh + 36) 14 (hsb 0 0 60) "speed / critical speed"
     , txt (xR 1 + 6) (gy + 14) 14 (hsb 0 70 100) "critical speed"
     , rtxt (gx + gw) (yV eMag - 6) 14 (hsb 140 50 100) "high speed: whirl -> e (G centers)"
     , txt gx (gy - 10) 14 (hsb 200 50 100) "Whirl radius |OS| (gray: original, blue: now)"
  -- ---- 数値
     , txt 20 40 15 (hsb 0 0 100) ("Up/Down: " ++ fmt 0 rpmNow ++ " rpm  Space: " ++ (if wPaused w then "resume" else "pause")
                                   ++ "  Click: add weight (1-3: " ++ show (wSize w) ++ ")  Z: undo  C: clear  N: new  H: "
                                   ++ (if wHide w then "show" else "hide"))
     , txt 20 68 15 (hsb 0 0 60) "Red: heavy spot  Green: weights  Yellow: center of mass G -> bring G to the disc center"
     , Color (hsbA 0 0 100 8) (rect (ix - 14 + (gw + 28) / 2) (iy - 26 + 68) (gw + 28) 136)
     , txt ix iy 14 (hsb 0 0 100) "whirl = e r^2 / sqrt((1 - r^2)^2 + (2 zeta r)^2)"
     , txt ix (iy + 22) 14 (hsb 0 0 100) "r = speed / critical speed"
     , txt ix (iy + 46) 14 (hsb 35 90 100) ("e = " ++ fmt 2 eMag ++ " px (orig " ++ fmt 1 (eccUnit * 0.8) ++ ")   whirl now = " ++ fmt 1 radNow ++ " px")
     , txt ix (iy + 70) 14 (hsb (if eMag < 0.6 then 140 else if eMag < 3 then 50 else 0) 70 100) ("Balance: " ++ grade)
     , txt ix (iy + 94) 14 (hsb 0 0 60) regime ]
  where
    (ocx, ocy) = (envW env * 0.27, envH env * 0.55)
    (sx, sy) = shaftPos env w
    sp = wSpin w
    -- 円板に固定した座標 → 画面（p5.js の translate + rotate(spin)）
    tr (x, y) = (sx + x * cos sp - y * sin sp, sy + x * sin sp + y * cos sp)
    hv = wHeavy w
    e = eccentricity w
    eMag = norm2 e
    gx = envW env * 0.56
    gw = envW env - gx - 50
    gy = 150
    gh = envH env * 0.4
    peakTop = whirl 1 (max (eccUnit * 0.8) eMag) * 1.1
    xR r = gx + r / 3 * gw
    yV v = gy + gh - min v peakTop / peakTop * gh
    S4 rx _ ry _ = wRs w
    radNow = sqrt (rx * rx + ry * ry)
    rpmNow = wSpeed w * naturalHz * 60
    ix = envW env * 0.56
    iy = gy + gh + 80
    grade | eMag < 0.6 = "PASS! almost perfectly balanced"
          | eMag < 3 = "almost there"
          | otherwise = "large unbalance"
    regime | wSpeed w < 0.9 = "Low speed: the heavy spot faces outward"
           | wSpeed w < 1.1 = "Critical speed: max whirl (phase lags 90 deg)"
           | otherwise = "High speed: heavy spot faces inward (self-centering)"

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    -- マウスの位置を円板に固定した座標に直す
    let (sx, sy) = shaftPos env w
        (mx, my) = mouseP env
        dx = mx - sx
        dy = my - sy
        a = negate (wSpin w)
    in pure $ if sqrt (dx * dx + dy * dy) > discR || length (wWeights w) >= 8 then w
              else w { wWeights = Mass (dx * cos a - dy * sin a) (dx * sin a + dy * cos a) (wSize w) : wWeights w }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char k) Down _ _
    | k == '1' -> pure w { wSize = 0.25 }
    | k == '2' -> pure w { wSize = 0.5 }
    | k == '3' -> pure w { wSize = 1 }
    | k `elem` "zZ" -> pure w { wWeights = drop 1 (wWeights w) }
    | k `elem` "cC" -> pure w { wWeights = [] }
    | k `elem` "nN" -> newUnbalance w
    | k `elem` "hH" -> pure w { wHide = not (wHide w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ↑ を押し続けて危険速度を通過しよう。振れが最大になったあと、高速では逆に小さくなる（洗濯機の脱水と同じ）
-- 2. スペースで止め、重い点のちょうど反対側・同じ半径に重さ 1 のおもり（3）を置く。振れはどうなる？
-- 3. 重さ 0.5 のおもり 2 個でつり合わせるには、どこに置けばいい？（ベクトルの足し算）
-- 4. H で重い点を隠し、N で新しい不つり合いに。低速で「外側に振れる向き」をヒントに重い点を探そう（タイヤのバランス取り）
-- 5. zeta を 0.02 にすると危険速度の山は何倍？ 危険速度をすばやく通過すると振れは小さくてすむ。確かめよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 右端が x になるように書く文字（幅はおよその見積もり）
rtxt :: Float -> Float -> Float -> Color -> String -> Picture
rtxt x y size c s = txt (x - fromIntegral (length s) * size * 0.8) y size c s

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
