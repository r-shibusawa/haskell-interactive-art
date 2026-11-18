-- ============================================
-- 53-07 スイングバイ ─ 惑星のそばを通るだけで、探査機が速くなる（遅くなる）のはなぜ？
-- 惑星から見ると：双曲線軌道で、入る速さと出る速さは同じ（向きが δ だけ変わる）sin(δ/2) = 1/e、e = √(1 + (b v∞²/μ)²)
-- 太陽から見ると：v = 惑星の速度 V + 惑星から見た速度。向きが変わると V との足し算の結果の長さが変わる
-- 1 kg あたりのエネルギーの変化 Δ(v²/2) = V · (v_出 − v_入)。軌道は運動方程式を RK4 で数値積分して描く
-- 操作：左の図をドラッグ = 通る位置 b / ↑↓ = 惑星から見た速さ v∞ / ←→ = 入ってくる向き / 1 木星 2 土星 3 地球
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

-- 惑星：名前、GM [km³/s²]、半径 [km]、公転の速さ [km/s]
data Pl = Pl { plName :: String, plMu, plR, plV :: !Double }

planets :: [Pl]
planets = [Pl "Jupiter" 1.26687e8 71492 13.07, Pl "Saturn" 3.7931e7 60268 9.68, Pl "Earth" 398600.4 6371 29.78]

plOf :: Int -> Pl
plOf k = getAt (Pl "Jupiter" 1.26687e8 71492 13.07) planets k

-- 数値積分の結果
data Fly = Fly
  { fPts  :: [(Double, Double, Double)]   -- 惑星のまわりの (x, y, 時刻)
  , fVin, fVout :: !(Double, Double)      -- 惑星から見た入る・出る速度（遠くでの値）
  , fRmin :: !Double
  , fHit  :: !Bool
  , fD    :: !Double                      -- 図の半分の幅（60 R）
  , fT    :: !Double }                    -- 最接近のころの時刻

data World = World
  { wPk   :: !Int
  , wVinf :: !Double                   -- ★ v∞ [km/s]
  , wIn   :: !Double                   -- ★ 入る向き [度]（惑星の進む向き = 0°）
  , wB    :: !Double                   -- ★ b [惑星の半径の何倍]
  , wPath :: !Fly
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-07 Gravity assist" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 8 160 20 (flyby 0 8 160 20) []

-- 解析解：離心率 e、曲がる角 δ、いちばん近づく距離 rp
hyper :: Double -> Double -> Double -> (Double, Double, Double)
hyper m v b = (e, 2 * asin (1 / e), (m / (v * v)) * (e - 1))
  where e = sqrt (1 + (b * v * v / m) ** 2)

-- 状態 (x, y, vx, vy)
data S = S !Double !Double !Double !Double

-- 数値積分：惑星を原点に、ずっと遠く（10 万 R）から入って、同じ距離まで出ていくまで（RK4、刻みは距離に合わせて変える）
-- 図に描くのは惑星のまわり ±60 R の部分だけ
flyby :: Int -> Double -> Double -> Double -> Fly
flyby k v al bR = Fly (reverse ptsR) (v * ux, v * uy) (vo * cos dir, vo * sin dir) rmin hit view' tMid
  where
    Pl _ m rp _ = plOf k
    ux = cos (al * degR)
    uy = sin (al * degR)
    (nx, ny) = (-uy, ux)
    d = 1e5 * rp
    view' = 60 * rp
    b = bR * rp
    v0 = sqrt (v * v + 2 * m / d)        -- 遠くで v∞ になるように、出発点ではエネルギー保存の分だけ速く
    s0 = S (-d * ux + b * nx) (-d * uy + b * ny) (v0 * ux) (v0 * uy)
    -- 運動方程式：(位置, 速度) → (速度, 加速度)
    deriv (S x y vx vy) = let r3 = (x * x + y * y) ** 1.5 in S vx vy (-m * x / r3) (-m * y / r3)
    addS (S a1 a2 a3 a4) h (S b1 b2 b3 b4) = S (a1 + h * b1) (a2 + h * b2) (a3 + h * b3) (a4 + h * b4)
    rk4 s h = let k1 = deriv s
                  k2 = deriv (addS s (h / 2) k1)
                  k3 = deriv (addS s (h / 2) k2)
                  k4 = deriv (addS s h k3)
                  S a1 a2 a3 a4 = k1
                  S b1 b2 b3 b4 = k2
                  S c1 c2 c3 c4 = k3
                  S d1 d2 d3 d4 = k4
              in addS s (h / 6) (S (a1 + 2 * b1 + 2 * c1 + d1) (a2 + 2 * b2 + 2 * c2 + d2) (a3 + 2 * b3 + 2 * c3 + d3) (a4 + 2 * b4 + 2 * c4 + d4))
    loop :: Int -> S -> Double -> [(Double, Double, Double)] -> Double -> (S, Double, [(Double, Double, Double)], Double, Bool)
    loop i s@(S x y vx vy) t acc rm
      | i >= 20000 = (s, t, acc, rm, False)
      | otherwise =
          let h = 0.01 * sqrt (x * x + y * y) / sqrt (vx * vx + vy * vy)
              s'@(S x' y' vx' vy') = rk4 s h
              t' = t + h
              r = sqrt (x' * x' + y' * y')
              rm' = min rm r
              acc' = if r < 1.5 * view' then (x', y', t') : acc else acc
          in if r < rp then (s', t', acc', rm', True)
             else if r > d && x' * vx' + y' * vy' > 0 then (s', t', acc', rm', False)
             else loop (i + 1) s' t' acc' rm'
    (S xe ye vxe vye, tEnd, ptsR, rmin, hit) = loop 0 s0 0 [] 1e99
    -- 遠くでの速さ v∞ は、エネルギー保存 v∞² = v² − 2μ/r で出口の値から求める
    vv = vxe * vxe + vye * vye
    vo = sqrt (max 0 (vv - 2 * m / sqrt (xe * xe + ye * ye)))
    dir = atan2 vye vxe
    tMid = case (ptsR, reverse ptsR) of        -- 最接近のころの時刻（図の中央）
      ((_, _, tl) : _, (_, _, tf) : _) -> (tf + tl) / 2
      _ -> tEnd / 2

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 惑星から見た図
  [ Color (rgb255 60 70 110) (frameTL 1 (cx - s / 2) (cy - s / 2) s s)
  , Color (rgb255 220 180 130) (circleD cx cy (max 4 (2 * f2 rp * sc)))
  , Color (rgba255 255 255 255 40) (clipLine (cx - f2 u0 * s, cy + f2 u1 * s) (cx + f2 u0 * s, cy - f2 u1 * s)) ]
  ++ [ Color (rgb255 250 225 140) (Pictures [ clipThick 2 a b | (a, b) <- zip scr (drop 1 scr) ]) ]
  ++ [ say ("Seen from " ++ plName pl ++ " (drag: where it passes, b)") (cx - s / 2) (cy - s / 2 - 8) 14 (rgb255 200 205 225)
     , say ("Speed: in " ++ fmt 2 (wVinf w) ++ " -> out " ++ fmt 4 (hyp (fVout p)) ++ " km/s (the same)") (cx - s / 2) (cy + s / 2 + 22) 14 (rgb255 200 205 225) ]
  -- ---- 太陽から見た速度の足し算
  ++ [ Color (rgba255 150 160 200 120) (at (vx + f2 vp * vs, vy) (Circle (f2 (wVinf w) * vs)))
     , arrowV vx vy vp 0 vs (rgb255 220 180 130)
     , arrowV (vx + f2 vp * vs) vy (fst (fVin p)) (snd (fVin p)) vs (rgb255 130 170 255)
     , arrowV (vx + f2 vp * vs) vy (fst (fVout p)) (snd (fVout p)) vs (rgb255 255 140 110)
     , arrowV vx vy (fst vin) (snd vin) vs (rgba255 130 170 255 140)
     , arrowV vx vy (fst vout) (snd vout) vs (rgba255 255 140 110 140)
     , say "Velocity from the sun = planet's velocity V (brown)" (vx - 20) 86 14 (rgb255 200 205 225)
     , say "+ velocity from the planet (blue = in, red = out). Circle radius = v_inf" (vx - 20) 104 14 (rgb255 200 205 225) ]
  -- ---- 太陽から見た通り道（惑星が右へ動く）
  ++ [ Color (rgb255 60 70 110) (frameTL 1 gx gy gw gh)
     , Color (rgba255 220 180 130 120) (Line [(ox - f2 xr * s2, oy), (ox + f2 xr * s2, oy)])
     , Color (rgb255 250 225 140) (polyline 2 [ (ox + f2 x * s2, oy - f2 y * s2) | (x, y) <- xs ])
     , Color (rgb255 220 180 130) (circleD ox oy 8)
     , say "Path seen from the sun (brown line = planet's motion, closest approach at the centre)" gx (gy - 8) 14 (rgb255 200 205 225) ]
  -- ---- 数字
  ++ [ say ("b = " ++ fmt 2 (wB w) ++ " R   e = " ++ fmt 3 he ++ "   closest rp = " ++ fmt 2 (hrp / rp) ++ " R" ++ (if fHit p then "   CRASH!" else "")) 20 y0 15 (rgb255 250 225 140)
     , say ("Bend angle delta = 2 asin(1/e) = " ++ fmt 1 (hd / degR) ++ " deg") 20 (y0 + 22) 15 (rgb255 250 225 140)
     , say ("Speed seen from the sun: " ++ fmt 2 (hyp vin) ++ " -> " ++ fmt 2 (hyp vout) ++ " km/s") 20 (y0 + 44) 15 (rgb255 220 225 240)
     , say ("Delta(v^2/2) = " ++ fmt 1 (eOut - eIn) ++ " km2/s2 (= V.Dv = " ++ fmt 1 dE ++ ") = " ++ fmt 2 ((eOut - eIn) * 1e6 / 1e9) ++ " GJ/kg") 20 (y0 + 66) 15 (rgb255 220 225 240)
     , say (if eOut > eIn then "Speed up: passed behind the planet and took" else "Slow down: crossing in front of the planet") 20 (y0 + 88) 15 gc
     , say (if eOut > eIn then "a little of its kinetic energy" else "gives energy away to it") 20 (y0 + 110) 15 gc
     , say "Gravity assist | drag left view = b  Up/Down v_inf  Left/Right direction  1 Jupiter 2 Saturn 3 Earth" 20 34 18 (rgb255 240 236 215)
     , say (plName pl ++ ": GM = " ++ fmtE 3 mu ++ " km3/s2, radius " ++ showJ rp ++ " km, orbital speed V = " ++ showJ vp ++ " km/s   v_inf = "
            ++ showJ (wVinf w) ++ " km/s   direction " ++ showJ (wIn w) ++ " deg") 20 60 16 (rgb255 250 225 140)
     , drawSelfTestAt (envW env - 190) (envH env - 12) (wTest w) ]
  where
    pl = plOf (wPk w)
    Pl _ mu rp vp = pl
    p = wPath w
    (he, hd, hrp) = hyper mu (wVinf w) (wB w * rp)
    s = min (envW env * 0.36) (envH env * 0.62)
    cx = 20 + s / 2
    cy = 110 + s / 2
    sc = s / 2 / f2 (fD p)
    (u0, u1) = let (a, b) = fVin p in (a / wVinf w, b / wVinf w)
    scr = [ (cx + f2 x * sc, cy - f2 y * sc) | (x, y, _) <- fPts p ]
    -- 枠の外は描かない
    box = (cx - s / 2, cy - s / 2, cx + s / 2, cy + s / 2)
    clipLine a b = maybe Blank (\(a', b') -> Line [a', b']) (clipSeg box a b)
    clipThick wt a b = maybe Blank (uncurry (thickLine wt)) (clipSeg box a b)
    hyp (a, b) = sqrt (a * a + b * b)
    vx = envW env * 0.5
    vy = envH env * 0.33
    vs = 0.9 * min (envW env * 0.45 / f2 (vp + wVinf w)) (envH env * 0.2 / f2 (wVinf w))
    vin = (vp + fst (fVin p), snd (fVin p))
    vout = (vp + fst (fVout p), snd (fVout p))
    gx = envW env * 0.47
    gy = envH env * 0.6
    gw = envW env - gx - 30
    gh = envH env * 0.3
    tm = fT p
    xs = [ (x + vp * (t - tm), y) | (x, y, t) <- fPts p ]
    xr = foldl' max 1 [ abs x | (x, _) <- xs ]
    yr = foldl' max 1 [ abs y | (_, y) <- xs ]
    s2 = min (gw / 2 / f2 xr) (gh / 2 / f2 yr) * 0.95
    ox = gx + gw / 2
    oy = gy + gh / 2
    eIn = (fst vin ^ (2 :: Int) + snd vin ^ (2 :: Int)) / 2
    eOut = (fst vout ^ (2 :: Int) + snd vout ^ (2 :: Int)) / 2
    dE = vp * (fst (fVout p) - fst (fVin p))
    y0 = cy + s / 2 + 46
    gc = if eOut > eIn then rgb255 130 230 160 else rgb255 255 150 130

-- 速度の矢印（倍率 s）
arrowV :: Float -> Float -> Double -> Double -> Float -> Color -> Picture
arrowV x y vx' vy' s c = Color c $ Pictures $
  [ thickLine 2 (x, y) (ex, ey) ]
  ++ (if l < 4 then [] else [ thickLine 1.2 (ex, ey) (ex - 8 * ux + 4 * uy, ey - 8 * uy - 4 * ux)
                             , thickLine 1.2 (ex, ey) (ex - 8 * ux - 4 * uy, ey - 8 * uy + 4 * ux) ])
  where
    ex = x + f2 vx' * s
    ey = y - f2 vy' * s
    l = sqrt ((ex - x) ^ (2 :: Int) + (ey - y) ^ (2 :: Int))
    ux = (ex - x) / max 1e-6 l
    uy = (ey - y) / max 1e-6 l

-- 線分を長方形 (x0, y0, x1, y1) の中だけに切る（リャン＝バースキー法）
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (bx0, by0, bx1, by1) (x1, y1) (x2, y2) = go [(-dx, x1 - bx0), (dx, bx1 - x1), (-dy, y1 - by0), (dy, by1 - y1)] 0 1
  where
    dx = x2 - x1
    dy = y2 - y1
    go [] t0 t1 = if t0 <= t1 then Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy)) else Nothing
    go ((pp, q) : rest) t0 t1
      | pp == 0 = if q < 0 then Nothing else go rest t0 t1
      | pp < 0 = go rest (max t0 (q / pp)) t1
      | otherwise = go rest t0 (min t1 (q / pp))

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (drag w)
  EventMotion _ | envDown env -> pure (drag w)
  EventKey k Down _ _ -> pure (redo (key k))
  _ -> pure w
  where
    redo w' = w' { wPath = flyby (wPk w') (wVinf w') (wIn w') (wB w') }
    key k = case k of
      SpecialKey KeyUp -> w { wVinf = min 25 (wVinf w + 1) }
      SpecialKey KeyDown -> w { wVinf = max 2 (wVinf w - 1) }
      SpecialKey KeyRight -> w { wIn = wrap360 (wIn w + 10) }
      SpecialKey KeyLeft -> w { wIn = wrap360 (wIn w - 10) }
      Char c | c >= '1' && c <= '3' -> w { wPk = fromEnum c - fromEnum '1' }
      _ -> w
    drag w0
      | abs (mx - cx) > s / 2 || abs (my - cy) > s / 2 = w0
      | otherwise = redo w0 { wB = if abs b < 0.2 then 0.2 else b }
      where
        (mx, my) = mouseP5 env
        s = min (envW env * 0.36) (envH env * 0.62)
        cx = 20 + s / 2
        cy = 110 + s / 2
        u = (cos (wIn w0 * degR), sin (wIn w0 * degR))
        (nx, ny) = (-(snd u), fst u)
        dx = realToFrac ((mx - cx) / (s / 2 / 60))
        dy = -(realToFrac ((my - cy) / (s / 2 / 60)))
        b = clampv (-40) 40 (dx * nx + dy * ny)

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("seen from the planet, speed out = speed in (numerical, within 0.01%)", near (hyp (fVout p) / 8) 1 1e-4)
  , ("numerical bend angle = 2 asin(1/e) (within 0.3 deg)", near turn (hd / degR) 0.3)
  , ("closest distance = (mu/v_inf^2)(e - 1) (within 0.5%)", near (fRmin p / hrp) 1 0.005)
  , ("energy change seen from the sun Delta(v^2/2) = V . (v_out - v_in)", near dE (vp * (fst (fVout p) - fst (fVin p)) + 0 * snd (fVin p)) (1e-6 * abs dE + 1e-9))
  , ("size of the velocity change |Dv| = 2 v_inf sin(delta/2)", near (hyp (fst (fVout p) - fst (fVin p), snd (fVout p) - snd (fVin p))) (2 * 8 * sin (hd / 2)) 0.02)
  , ("farther passes bend less: delta(400 R) < delta(20 R) / 5", let (_, d4, _) = hyper mu 8 (400 * rp) in d4 < hd / 5)
  , ("this example (passing behind Jupiter) speeds up as seen from the sun", dE > 0) ]
  where
    Pl _ mu rp vp = plOf 0
    p = flyby 0 8 160 20
    (_, hd, hrp) = hyper mu 8 (20 * rp)
    hyp (a, b) = sqrt (a * a + b * b)
    turn = abs (wrap180 ((atan2 (snd (fVout p)) (fst (fVout p)) - atan2 (snd (fVin p)) (fst (fVin p))) / degR))
    vin = (vp + fst (fVin p), snd (fVin p))
    vout = (vp + fst (fVout p), snd (fVout p))
    dE = (fst vout ^ (2 :: Int) + snd vout ^ (2 :: Int) - fst vin ^ (2 :: Int) - snd vin ^ (2 :: Int)) / 2

-- 指数表示（toExponential）："1.267e+8"
fmtE :: Int -> Double -> String
fmtE n v = let (m, e) = break (== 'e') (showEFloat (Just n) (fin 0 v) "")
           in m ++ case e of
                ('e' : '-' : ds) -> "e-" ++ ds
                ('e' : ds) -> "e+" ++ ds
                _ -> ""

-- 自己テストの結果を (x, y) に（07 は右下）
drawSelfTestAt :: Float -> Float -> [(String, Bool)] -> Picture
drawSelfTestAt x y rs = say (selfTestLine rs) x y 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)

-- ---------- やってみよう ----------
-- 1. 左の図で、惑星の上側と下側を通してみよう。加速するのはどちら？
-- 2. b を小さく（惑星すれすれ）すると δ は大きくなる。でも近づきすぎると衝突！
-- 3. ↑↓ で v∞ を大きくすると曲がりにくくなる（速すぎると重力が効く時間が短い）
-- 4. 1・2・3 で木星・土星・地球を比べよう。重い惑星ほど大きく曲げられる
-- 5. ボイジャーは木星と土星のスイングバイで太陽系の外へ。どれくらい加速できるか試そう


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
