-- ============================================
-- 03 凹面鏡・凸面鏡と球面収差
-- 平行光線が球面鏡に当たると、焦点 F（曲率半径の半分 R/2）に集まる（凹面）／ F から出たように広がる（凸面）
--   でも球面では、端のほうの光線ほど手前で軸と交わる = 球面収差。放物面鏡なら全部 1 点に集まる
-- ↑↓ = 曲率半径 R / ←→ = 鏡の大きさ（開口） / P = 球面 ⇄ 放物面 / C = 凹 ⇄ 凸 / マウス上下 = 光線の傾き
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sort)
import Numeric (showFFloat)

rayCount :: Int
rayCount = 25              -- ★ 光線の本数

data World = World
  { wR        :: !Float    -- ★ 曲率半径（px）
  , wAperture :: !Float    -- ★ 鏡の大きさ（R に対する割合 0.2〜0.95）
  , wConcave  :: !Bool
  , wParabola :: !Bool
  }

main :: IO ()
main = runSketch (sketch "36-03 Curved Mirrors and Aberration" (World 440 0.8 True False))
  { skBack  = hsb 230 40 8
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 光線（p, d）と鏡の交点までの距離 t と法線 n（なければ Nothing）
hitMirror :: World -> Point -> Point -> Float -> Float -> Float -> Maybe (Float, Float, Float)
hitMirror w (px, py) (dx, dy) v axis hh
  | wParabola w =
      -- (px + t dx − Vx) + s (py + t dy − ay)² / (4F) = 0 → t の 2 次方程式
      let yy = py - axis
          qa = s * dy * dy / (4 * f)
          qb = dx + s * 2 * yy * dy / (4 * f)
          qc = (px - v) + s * yy * yy / (4 * f)
          ok t = let y = py + t * dy - axis in t > 1e-3 && abs y <= hh
      in case filter ok (solveQuad qa qb qc) of
           (t : _) ->
             let y = py + t * dy - axis
                 ny = s * 2 * y / (4 * f)
                 len = norm2 (1, ny)
             in Just (t, 1 / len, ny / len)
           [] -> Nothing
  | otherwise =
      -- 球面：中心 C = (Vx − sR, axis)
      let cx = v - s * r
          cy = axis
          ox = px - cx
          oy = py - cy
          qb = 2 * (ox * dx + oy * dy)
          qc = ox * ox + oy * oy - r * r
          ok t =
            let x = px + t * dx
                y = py + t * dy
            in t > 1e-3 && abs (y - axis) <= hh && (if wConcave w then x > cx else x < cx)
      in case filter ok (solveQuad 1 qb qc) of
           (t : _) -> Just (t, (px + t * dx - cx) / r, (py + t * dy - cy) / r)
           [] -> Nothing
  where
    r = wR w
    f = r / 2
    s = if wConcave w then 1 else -1      -- 凹: x = Vx − y²/(4F) 側に曲がる

-- 2 次方程式（Float の桁落ちを防ぐため、q を経由する形で解く）
solveQuad :: Float -> Float -> Float -> [Float]
solveQuad a b c
  | abs a < 1e-9 = if abs b < 1e-9 then [] else [negate c / b]
  | d < 0 = []
  | abs q < 1e-12 = [0]
  | otherwise = sort [q / a, c / q]
  where
    d = b * b - 4 * a * c
    q = -0.5 * (b + (if b >= 0 then 1 else -1) * sqrt d)

mirrorX :: World -> Float -> Float -> Float
mirrorX w y v
  | wParabola w = v - s * y * y / (4 * f)
  | otherwise = (v - s * r) + s * sqrt (max 0 (r * r - y * y))
  where
    r = wR w
    f = r / 2
    s = if wConcave w then 1 else -1

-- 1 本の光線の結果
data Ray = Ray { rPic :: !Picture, rCross :: !(Maybe (Float, Float)) }   -- 絵と（軸と交わる x、端の度合い）

traceOne :: World -> Float -> Float -> Float -> Float -> Int -> Ray
traceOne w v axis hh tilt i =
  let y0 = negate hh + 2 * hh * (fromIntegral i + 0.5) / fromIntegral rayCount
      dx = cos tilt
      dy = sin tilt
      px = 0
      py = axis + y0 - v * tan tilt         -- 左端から出発（鏡の面で高さ y0 になる）
      edge = abs (fromIntegral i - (fromIntegral rayCount - 1) / 2) / (fromIntegral rayCount / 2)   -- 端の光線ほど 1
      incident = Color (hsbA 45 60 100 45)
  in case hitMirror w (px, py) (dx, dy) v axis hh of
       Nothing -> Ray (incident (thickLine 1.5 (px, py) (px + dx * 3000, py + dy * 3000))) Nothing
       Just (t, nx, ny) ->
         let hx = px + dx * t
             hy = py + dy * t
             -- ★ 反射： d' = d − 2(d·n)n
             dp = dx * nx + dy * ny
             rx = dx - 2 * dp * nx
             ry = dy - 2 * dp * ny
             hue = if edge > 0.8 then 0 else 170
             pic = Pictures
               [ incident (thickLine 1.5 (px, py) (hx, hy))
               , Color (hsbA hue 70 100 45) (thickLine 1.5 (hx, hy) (hx + rx * 3000, hy + ry * 3000))
               , if wConcave w then Blank
                 else Color (hsbA 170 70 100 18) (dashed (hx, hy) (hx - rx * 3000, hy - ry * 3000)) ]
             cross = if abs ry > 1e-6 then Just (hx + rx * ((axis - hy) / ry), edge) else Nothing
         in Ray pic cross

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      v = wd * 0.72
      axis = h * 0.55
      r = wR w
      hh = min (wAperture w * r) (h * 0.44)
      tilt = clampF (-0.12) 0.12 (remap (snd (mouseP5 env)) 0 h 0.12 (-0.12))   -- 入射光線の傾き（ラジアン）
      f = r / 2
      s = if wConcave w then 1 else -1
      rays = map (traceOne w v axis hh tilt) [0 .. rayCount - 1]
      crossings = [ c | Ray _ (Just c) <- rays ]
      fx = v - s * f
      centerX = v - s * r
      marg = foldl' (\b c -> case b of
                        Nothing -> Just c
                        Just b0 -> if snd c > snd b0 then Just c else b) Nothing crossings
      mirrorPts = [ (mirrorX w y v, axis + y)
                  | k <- [0 .. floor (2 * hh / 4) :: Int], let y = negate hh + 4 * fromIntegral k ]
      px = 20
      py = h - 110
  in Pictures
    [ onP5 env (Pictures
        ( [ Color (hsbA 0 0 100 30) (Line [(0, axis), (wd, axis)])
          , Color (hsb 190 20 100) (thickPath 5 mirrorPts) ]
          ++ map rPic rays ++
          -- 焦点と交点のずれ
          [ Color white (at (fx, axis) (disc 4))
          , Color (hsb 0 0 60) (at (centerX, axis) (disc 3)) ]
          ++ [ Color (hsbA (if e > 0.8 then 0 else 45) 80 100 70) (at (x, axis) (disc 2.5)) | (x, e) <- crossings ]
          ++ [ Color (hsbA 0 0 100 8) (box (px - 4) (py - 28) 740 100) ]))
    , txt env (fx - 4) (axis + 24) 14 white "F"
    , txt env (centerX - 4) (axis + 24) 14 (hsb 0 0 60) "C"
    , txt env 20 40 18 white
        ((if wConcave w then "Concave" else "Convex") ++ " (C)  " ++ (if wParabola w then "Parabolic" else "Spherical")
         ++ " (P)  R = " ++ nf 0 r ++ " (Up/Down)  f = R/2 = " ++ nf 0 f ++ "  Aperture " ++ nf 2 (wAperture w) ++ " (L/R)")
    , txt env 20 68 15 (hsb 0 0 60)
        "Mouse up/down tilts the rays. Yellow = incoming, cyan = reflected, red = outermost rays."
    , txt env 20 90 15 (hsb 0 0 60) "Dots on the axis: where each reflected ray crosses it."
    , case marg of
        Just (mx, _) | not (wParabola w) -> Pictures
          [ txt env (px + 10) py 16 white ("Edge ray crosses the axis " ++ nf 0 (abs (mx - fx)) ++ " px from F (spherical aberration)")
          , txt env (px + 10) (py + 28) 16 white "A smaller mirror (Left) reduces it. P (parabola) makes it 0." ]
        _ | wParabola w -> Pictures
          [ txt env (px + 10) py 16 white "Parabola: all parallel rays meet at F (dish antenna, telescope)"
          , txt env (px + 10) (py + 28) 16 white "But tilted light (mouse) shows coma." ]
        _ -> Blank
    , txt env (px + 10) (py + 56) 14 (hsb 0 0 70)
        (if wConcave w then "Concave: collects sunlight, flashlight reflector, reflecting telescope"
                       else "Convex: wide view looks small. Traffic mirrors, car side mirrors")
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wR = min 1200 (wR w + 20) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wR = max 120 (wR w - 20) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wAperture = min 0.95 (wAperture w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wAperture = max 0.2 (wAperture w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> w { wParabola = not (wParabola w) }
    | c `elem` "cC" -> w { wConcave = not (wConcave w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 開口を 0.95 にして、端の光線（赤）がどれだけ F より手前で交わるか見よう。開口 0.3 では？
-- 2. P を押して放物面に。全部 F に集まる。でもマウスで傾けると像がにじむ（コマ収差）
-- 3. 凸面鏡（C）：反射光の点線をたどると、鏡の裏の F から出たように見える（虚焦点）
-- 4. rayCount を 200 にして、半透明の線の重なりで「火線（カースティック）」を見よう → 05 へ


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字・破線）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- 点線
dashed :: Point -> Point -> Picture
dashed (x1, y1) (x2, y2) =
  let n = max 1 (floor (dist2 (x1, y1) (x2, y2) / 14)) :: Int
      f i = fromIntegral i / fromIntegral n
      pt t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Pictures [ Line [pt (f i), pt (f (i + 1))] | i <- [0, 2 .. n - 1] ]
