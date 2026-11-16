-- ============================================
-- 45-10 強制振動と共振
-- ばね・おもり・ダンパーを周期的な力でゆする：m x″ + c x′ + k x = F0 cos ωt（RK4 で積分）
-- ゆする速さが固有振動数に近いと振幅が何倍にもなる（共振）。振幅倍率 M = 1 / √((1 − r²)² + (2ζr)²)、r = ω / ωn
-- 操作：←→ = 周波数比 r / ↑↓ = 減衰比 ζ / A = 動吸振器（小さなおもりを追加）/ S = r をゆっくり自動で上げる / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

naturalHz, staticPx, massRatio, zeta2 :: Float
naturalHz = 1.0           -- ★ 固有振動数（Hz）
staticPx = 9              -- ★ 静たわみ F0 / k（px）。振幅はこの M 倍になる
massRatio = 0.15          -- ★ 動吸振器の質量比 μ = m2 / m1
zeta2 = 0.02              -- ★ 動吸振器の減衰

-- 状態 [x1, v1, x2, v2]
data S4 = S4 !Float !Float !Float !Float

data World = World
  { wR        :: !Float            -- 周波数比 r = ω / ωn
  , wZeta     :: !Float            -- ★ 減衰比 ζ
  , wAbsorber :: !Bool
  , wSweep    :: !Bool
  , wSt       :: !S4
  , wPhase    :: !Float            -- 力の位相 ωt
  , wAmp      :: !Float            -- 最後に測った振幅
  , wPeak     :: !Float            -- 1 周期の間の最大値
  , wMeasured :: ![(Float, Float)] -- シミュレーションから測った (r, 振幅倍率)。新しい順、500 個まで
  , wStrip    :: ![(Float, Float)] -- 時間波形 (x, 力)。新しい順、360 個まで
  }

world0 :: World
world0 = World 0.6 0.08 False False (S4 0 0 0 0) 0 0 0 [] []

main :: IO ()
main = runSketch (sketch "45-10 Forced vibration and resonance" world0)
  { skBack  = hsb 212 45 13
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 運動方程式（ωn² で割った形）。動吸振器は主系と同じ固有振動数にそろえてある
deriv :: World -> S4 -> Float -> S4
deriv w (S4 x1 v1 x2 v2) ph = S4 v1 (a1 + extra) v2 a2
  where
    wn = tau * naturalHz
    f = wn * wn * staticPx * cos ph
    a1 = f - 2 * wZeta w * wn * v1 - wn * wn * x1
    rel = wn * wn * (x2 - x1) + 2 * zeta2 * wn * (v2 - v1)   -- 吸振器のばね＋ダンパーの力 / m2
    (extra, a2) = if wAbsorber w then (massRatio * rel, -rel) else (0, 0)

addS :: S4 -> S4 -> Float -> S4
addS (S4 a b c d) (S4 e f g h) k = S4 (a + e * k) (b + f * k) (c + g * k) (d + h * k)

-- ★ RK4 で dt 秒進める
rk4 :: Float -> World -> World
rk4 dt w = w { wSt = S4 (cl x1 k1a k2a k3a k4a) (cl x2 k1b k2b k3b k4b) (cl x3 k1c k2c k3c k4c) (cl x4 k1d k2d k3d k4d)
             , wPhase = ph + om * dt }
  where
    om = tau * naturalHz * wR w
    s = wSt w
    ph = wPhase w
    k1 = deriv w s ph
    k2 = deriv w (addS s k1 (dt / 2)) (ph + om * dt / 2)
    k3 = deriv w (addS s k2 (dt / 2)) (ph + om * dt / 2)
    k4 = deriv w (addS s k3 dt) (ph + om * dt)
    S4 x1 x2 x3 x4 = s
    S4 k1a k1b k1c k1d = k1
    S4 k2a k2b k2c k2d = k2
    S4 k3a k3b k3c k3d = k3
    S4 k4a k4b k4c k4d = k4
    cl v a b c d = clampF (-1e5) 1e5 (v + (a + 2 * b + 2 * c + d) * dt / 6)

-- ★ 理論の応答（複素数で 2×2 の連立方程式を解く）：返り値は 振幅倍率 と 位相遅れ
response :: World -> Float -> (Float, Float)
response w r
  | not (wAbsorber w) = (1 / sqrt (fst d1 ^ (2 :: Int) + snd d1 ^ (2 :: Int)), atan2 (snd d1) (fst d1))
  | otherwise = (sqrt (xr * xr + xi * xi), atan2 (-xi) xr)
  where
    -- 主系：(1 − r²) + i 2ζr、吸振器：k2 項 = μ(1 + i 2ζ2 r)、(1 − r²) + i 2ζ2 r
    d1 = (1 - r * r, 2 * wZeta w * r)
    kc = (massRatio, massRatio * 2 * zeta2 * r)                  -- 吸振器のばね＋ダンパー
    d22 = (1 - r * r, 2 * zeta2 * r)                             -- 吸振器側（μ で割った形）
    cm (a, b) (c, d) = (a * c - b * d, a * d + b * c)
    -- X1 = F · D22 / ((D1 + kc) · D22·μ − kc²) を μ でそろえて計算
    dd11 = (fst d1 + fst kc, snd d1 + snd kc)
    dd22 = (massRatio * fst d22, massRatio * snd d22)
    (p, q) = cm dd11 dd22
    (kr, ki) = cm kc kc
    (dr, di) = (p - kr, q - ki)
    dm = dr * dr + di * di
    xr = (fst dd22 * dr + snd dd22 * di) / dm
    xi = (snd dd22 * dr - fst dd22 * di) / dm

step :: Env -> World -> World
step env w0 = w2 { wStrip = take 360 ((x1, cos (wPhase w2)) : wStrip w2) }
  where
    rL = if isDown env (SpecialKey KeyLeft) then -0.004 else 0
    rR = if isDown env (SpecialKey KeyRight) then 0.004 else 0
    r1 = wR w0 + rL + rR
    (r2, sw) = if wSweep w0 then (r1 + 0.0006, r1 + 0.0006 < 2.2) else (r1, False)
    w1 = w0 { wR = clampF 0.1 2.2 r2, wSweep = sw }
    -- 積分（1 フレーム = 1/60 秒を 8 分割）
    w2 = iterate sub w1 !! 8
    S4 x1 _ _ _ = wSt w2
    sub w =
      let S4 _ before _ _ = wSt w
          w' = rk4 (1 / 480) w
          S4 x v _ _ = wSt w'
          pk = max (wPeak w') (abs x)
      in if before > 0 && v <= 0          -- 速度が + → − に変わる瞬間 = 山。1 周期ごとに振幅を測る
           then w' { wAmp = pk, wPeak = 0, wMeasured = take 500 ((wR w', pk / staticPx) : wMeasured w') }
           else w' { wPeak = pk }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：ばね・おもり・ダンパー
  [ Color (hsb 0 0 40) (rect ccx (shakeY - 7) 220 14)
  , path 2.5 (hsb 140 50 90) [ (ccx - 40 + zig i 20 16, lerpF shakeY (y1 - 34) (fromIntegral i / 20)) | i <- [0 .. 20] ]
  , path 3 (hsb 0 0 70) [(ccx + 40, shakeY), (ccx + 40, midD + 14)], path 3 (hsb 0 0 70) [(ccx + 28, midD + 14), (ccx + 52, midD + 14)]
  , path 3 (hsb 0 0 70) [(ccx + 22, midD - 16), (ccx + 22, midD + 30), (ccx + 58, midD + 30), (ccx + 58, midD - 16)]
  , path 3 (hsb 0 0 70) [(ccx + 40, midD + 30), (ccx + 40, y1 - 34)]
  , Color (hsb 200 55 90) (rect ccx y1 140 68)
  , txt (ccx - 6) (y1 + 6) 15 (hsb 0 0 100) "m" ]
  ++ absorberPic
  ++ [ seg 1 (hsbA 0 0 100 30) (ccx - 150, restY) (ccx + 150, restY) ]
  -- 力の矢印
  ++ [ Pictures [ path 4 (hsb 0 80 100) [(ccx - 100, y1), (ccx - 100, y1 + fa)]
                , path 4 (hsb 0 80 100) [(ccx - 107, y1 + fa - signum fa * 9), (ccx - 100, y1 + fa), (ccx - 93, y1 + fa - signum fa * 9)] ]
     | abs fa > 4 ]
  -- ---- 右上：応答曲線（振幅倍率）、右中：位相
  ++ [ seg 1 (hsb 0 0 45) (gx, gy) (gx, gy + gh), seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh)
     , seg 1 (hsbA 0 0 100 25) (xR 1, gy) (xR 1, gy + gh)
     , path 2.5 (hsb 200 60 100) [ (xR r, yM (fst (response w r))) | i <- [1 .. 220 :: Int], let r = fromIntegral i / 100 ] ]
  ++ [ dot (hsbA 50 90 100 60) (xR r) (yM m) 5 | (r, m) <- wMeasured w ]   -- 測った点
  ++ [ dot (hsb 0 80 100) (xR (wR w)) (yM mag) 13 ]
  ++ [ txt (xR r - 10) (gy + gh + 18) 14 (hsb 0 0 60) (fmt 1 r) | r <- [0, 0.5, 1, 1.5, 2] ]
  ++ [ txt (gx - 20) (yM m + 5) 14 (hsb 0 0 60) (show (round m :: Int)) | m <- [0, 2, 4, 6, 8 :: Float] ]
  ++ [ txt (gx + gw - 170) (gy + gh + 36) 14 (hsb 0 0 60) "freq ratio r = w / wn"
     , txt gx (gy - 10) 14 (hsb 200 50 100) "Amplitude ratio M (blue = theory, yellow = simulation)"
     -- 位相
     , seg 1 (hsb 0 0 45) (gx, py) (gx, py + ph), seg 1 (hsb 0 0 45) (gx, py + ph) (gx + gw, py + ph)
     , path 2 (hsb 320 60 100) [ (xR r, py + min (posLag (snd (response w r))) pi / pi * ph) | i <- [1 .. 220 :: Int], let r = fromIntegral i / 100 ]
     , dot (hsb 0 80 100) (xR (wR w)) (py + min lagNow pi / pi * ph) 10
     , txt gx (py - 8) 14 (hsb 320 50 100) ("Phase lag (top 0 deg - bottom 180 deg): now " ++ fmt 0 (lagNow * 180 / pi) ++ " deg")
     -- 時間波形
     , seg 1 (hsb 0 0 40) (gx, sy0) (gx + gw, sy0) ]
  ++ (if length strip > 1
        then [ path 1.5 (hsbA 0 80 100 70) [ (gx + fromIntegral i / 360 * gw, sy0 - f * 14) | (i, (_, f)) <- zip [0 :: Int ..] strip ]
             , path 2 (hsb 200 60 100) [ (gx + fromIntegral i / 360 * gw, sy0 + clampF (-1.2) 1.2 (x / (staticPx * mTop)) * sAmp)
                                        | (i, (x, _)) <- zip [0 :: Int ..] strip ] ]
        else [])
  ++ [ txt gx (sy0 - 50) 14 (hsb 0 0 60) "red: force, blue: displacement (down +); peak shift = lag"
  -- ---- 文字
     , txt 20 40 16 (hsb 0 0 100) ("Left/Right: r = " ++ fmt 2 (wR w) ++ "   Up/Down: zeta = " ++ fmt 2 (wZeta w) ++ "   A: absorber "
                                   ++ onOff (wAbsorber w) ++ "   S: sweep " ++ onOff (wSweep w) ++ "   R: reset")
     , txt 20 68 15 (hsb 0 0 60) ("r = 1 (natural frequency) gives the biggest swing. Theory M = " ++ fmt 2 mag
                                  ++ ", measured " ++ fmt 2 (wAmp w / staticPx) ++ " (let it settle)")
     , txt 30 (envH env - 70) 14 (hsb 0 0 100) "m x'' + c x' + k x = F0 cos wt"
     , txt 30 (envH env - 46) 14 (hsb 0 0 65)
         (if wAbsorber w then "Absorber: the small mass swings instead," else "M = 1 / sqrt((1 - r^2)^2 + (2 zeta r)^2)")
     , txt 30 (envH env - 24) 14 (hsb 0 0 65)
         (if wAbsorber w then "cancelling r = 1 (the peak splits in two)" else "peak about 1 / (2 zeta) = " ++ fmt 1 (1 / (2 * wZeta w))) ]
  where
    S4 x1 _ x2 _ = wSt w
    ccx = envW env * 0.2
    restY = envH env * 0.5
    ceilY = 110
    y1 = restY + clampF (-230) 250 x1
    shakeY = ceilY + staticPx * cos (wPhase w) * 1.5      -- 力の向きを天井の動きで表す
    midD = (shakeY + y1 - 34) / 2
    zig :: Int -> Int -> Float -> Float
    zig i n a = if i == 0 || i == n then 0 else if odd i then a else -a
    absorberPic
      | wAbsorber w =
          let y2 = y1 + 110 + clampF (-70) 200 (x2 - x1)
          in [ path 2.5 (hsb 35 70 100) [ (ccx + zig i 12 12, lerpF (y1 + 34) (y2 - 20) (fromIntegral i / 12)) | i <- [0 .. 12] ]
             , Color (hsb 35 80 100) (rect ccx y2 68 40)
             , txt (ccx + 42) (y2 + 6) 14 (hsb 35 60 100) "absorber" ]
      | otherwise = []
    fa = cos (wPhase w) * 46
    gx = envW env * 0.44
    gw = envW env - gx - 50
    gy = 120
    gh = envH env * 0.36
    mTop = 8
    xR r = gx + r / 2.2 * gw
    yM m = gy + gh - min m mTop / mTop * gh
    (mag, lag) = response w (wR w)
    posLag l = if l < 0 then l + tau else l
    lagNow = posLag lag
    py = gy + gh + 60
    ph = 70
    sy0 = envH env - 70
    sAmp = 44
    strip = reverse (wStrip w)
    onOff b = if b then "ON" else "off"

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wZeta = clampF 0.03 1 (wZeta w + 0.01), wMeasured = [] }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wZeta = clampF 0.03 1 (wZeta w - 0.01), wMeasured = [] }
  EventKey (Char k) Down _ _
    | k `elem` "aA" -> let S4 a b _ _ = wSt w in w { wAbsorber = not (wAbsorber w), wSt = S4 a b a b, wMeasured = [] }
    | k `elem` "sS" -> if wSweep w then w { wSweep = False } else w { wSweep = True, wR = 0.2, wMeasured = [] }
    | k `elem` "rR" -> w { wSt = S4 0 0 0 0, wMeasured = [], wStrip = [], wR = 0.6, wZeta = 0.08, wSweep = False }
  _ -> w

-- ---------- やってみよう ----------
-- 1. S で自動スイープ。黄色の実測点が青い理論曲線をなぞる。ずれるのはどこ？（スイープが速いと共振が育ちきらない）
-- 2. r = 1 で位相の遅れはちょうど 90°。r が小さいと力と同じ向き、大きいと逆向きに動く。時間波形で確かめよう
-- 3. ζ を 0.03 まで下げると倍率は 1/2ζ ≈ 17。ブランコを「タイミングよく」押すと大きく揺れるのと同じ
-- 4. A で動吸振器を付けると r = 1 の山が消えて 2 つに割れる。massRatio を 0.3 にすると 2 つの山の間隔は？
-- 5. 洗濯機は脱水で回転が上がるとき r = 1 を通過する。r = 1.5 で使うなら ζ は大きい方がいい？小さい方がいい？

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

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
