-- ============================================
-- 05 ガラス円柱・水滴の光線追跡と火線（カースティック）
-- たくさんの平行光線が丸いガラスに入り、屈折・反射をくり返す
--   境界ごとにフレネルの式で「反射する割合 R」と「通り抜ける割合 1−R」に分け、両方を追いかける
--   光線が集中する場所が明るい曲線 = 火線（コップの底やプールの底のゆらめく光）
-- マウス = 光の来る方向 / ↑↓ = 屈折率 n / ←→ = 光線の本数 / B = 反射の回数 / 1〜3 = 水・ガラス・ダイヤ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showFFloat)

radius :: Float
radius = 230               -- ★ 円の半径

-- 光を足し算するためのマス目（1280 × 800 の絵を cell ピクセルごとに 1 マス）
gw, gh :: Int
gw = 356
gh = 222

cell, canvasW, canvasH, lineW :: Float
cell = 3.6                 -- ★ マスの大きさ（px）
canvasW = 1280
canvasH = 800
lineW = 1.2                -- ★ 光線の太さ（px）

data World = World
  { wN      :: !Float      -- ★ 屈折率
  , wRays   :: !Int        -- ★ 光線の本数
  , wBounce :: !Int        -- ★ 中で反射する最大回数
  }

main :: IO ()
main = runSketch (sketch "36-05 Glass Cylinder and Caustics" (World 1.5 260 4))
  { skBack  = rgb255 8 10 18
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 光線の 1 区間。強さ w で色 (r, g, b) を足す
data Seg = Seg !Float !Float !Float !Float !Float !Float !Float !Float

-- 光線（p から向き d）と円（中心 c、半径 r）の交点までの距離。inside = 中から出る場合
hitCircle :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool -> Maybe Float
hitCircle px py dx dy cx cy r inside =
  let ox = px - cx
      oy = py - cy
      b = 2 * (ox * dx + oy * dy)
      c = ox * ox + oy * oy - r * r
      d = b * b - 4 * c
      q = sqrt d
      t = if inside then (negate b + q) / 2 else (negate b - q) / 2
  in if d < 0 || t <= 1e-3 then Nothing else Just t

-- ★ フレネルの反射率（s と p の平均）
fresnelR :: Float -> Float -> Float -> Float -> Float
fresnelR na nb cosI cosT =
  let rs = (na * cosI - nb * cosT) / max 1e-9 (na * cosI + nb * cosT)
      rp = (na * cosT - nb * cosI) / max 1e-9 (na * cosT + nb * cosI)
  in (rs * rs + rp * rp) / 2

-- ★ 1 本の光線を追跡して、区間の一覧を返す
traceRay :: World -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> (Float, Float, Float) -> Float -> Float -> Float -> [Seg]
traceRay world cx cy dx dy outCol inCol ex ey off0 = segs
  where
    n = wN world
    startDist = radius * 2.2
    off = off0
    px = cx - dx * startDist + ex * off
    py = cy - dy * startDist + ey * off
    seg x1 y1 x2 y2 w (r, g, b) = Seg x1 y1 x2 y2 (min 1 w) r g b
    segs = case hitCircle px py dx dy cx cy radius False of
      Nothing -> [seg px py (px + dx * 2500) (py + dy * 2500) 0.5 outCol]
      Just t ->
        let hx = px + dx * t
            hy = py + dy * t
            -- 外から中へ：法線は内向き
            nx = (cx - hx) / radius
            ny = (cy - hy) / radius
            cosI = dx * nx + dy * ny
            sinT2 = (1 / n) * (1 / n) * (1 - cosI * cosI)
            cosT = sqrt (max 0 (1 - sinT2))
            r = fresnelR 1 n cosI cosT
            -- 反射光（外へ）
            rx = dx - 2 * cosI * nx
            ry = dy - 2 * cosI * ny
            -- 屈折光（中へ）
            f = cosI / n - cosT
            d2x = dx / n - f * nx
            d2y = dy / n - f * ny
        in seg px py hx hy 0.5 outCol
           : seg hx hy (hx + rx * 2500) (hy + ry * 2500) r outCol
           : inner 0 hx hy d2x d2y (1 - r)
    inner :: Int -> Float -> Float -> Float -> Float -> Float -> [Seg]
    inner b qx0 qy0 ddx ddy w
      | b > wBounce world = []
      | otherwise = case hitCircle qx0 qy0 ddx ddy cx cy radius True of
          Nothing -> []
          Just t2 ->
            let qx = qx0 + ddx * t2
                qy = qy0 + ddy * t2
                -- 中から外へ：法線は内向き（光と逆向き）
                mx = (cx - qx) / radius
                my = (cy - qy) / radius
                c1 = negate (ddx * mx + ddy * my)     -- 入射角の cos
                s2 = n * n * (1 - c1 * c1)
                c2 = sqrt (max 0 (1 - s2))
                rb = if s2 < 1 then fresnelR n 1 c1 c2 else 1
                g = n * c1 - c2
                ox = n * ddx + g * mx                 -- 外へ出る光
                oy = n * ddy + g * my
                out = if s2 < 1 then [seg qx qy (qx + ox * 2500) (qy + oy * 2500) (w * (1 - rb)) outCol] else []
                -- 中で反射して続ける
                dp = ddx * mx + ddy * my
                w' = w * rb
                next = if w' < 0.01 then [] else inner (b + 1) qx qy (ddx - 2 * dp * mx) (ddy - 2 * dp * my) w'
            in seg qx0 qy0 qx qy w inCol : out ++ next

-- 線分をマス目に足す（p5.js の blendMode(ADD) の代わり）
plot :: MU.MVector s Float -> Seg -> ST s ()
plot buf (Seg x1 y1 x2 y2 w r g b) =
  case clipSeg x1 y1 x2 y2 of
    Nothing -> pure ()
    Just (ax, ay, bx, by) -> do
      let u0 = ax / cell
          v0 = ay / cell
          du = (bx - ax) / cell
          dv = (by - ay) / cell
          big = max (abs du) (abs dv)
          n = max 1 (ceiling big) :: Int
          -- 線がマスを覆う割合（太さ / マス × 斜めの長さ）
          gain = lineW / cell * (sqrt (du * du + dv * dv) / max 1e-6 big) * w
          ar = r * gain
          ag = g * gain
          ab = b * gain
          -- 1 マスに足す（画面の外なら何もしない）
          addCell !x !y !k
            | x >= 0 && x < gw && y >= 0 && y < gh = do
                let o = (y * gw + x) * 3
                MU.unsafeModify buf (+ ar * k) o
                MU.unsafeModify buf (+ ag * k) (o + 1)
                MU.unsafeModify buf (+ ab * k) (o + 2)
            | otherwise = pure ()
          -- 長い向きに 1 マスずつ進み、短い向きの隣り合う 2 マスに割合で分ける（縞模様を防ぐ）
          go !i
            | i > n = pure ()
            | otherwise = do
                let t = fromIntegral i / fromIntegral n
                    u = u0 + du * t
                    v = v0 + dv * t
                if abs du >= abs dv
                  then do
                    let fv = v - 0.5
                        j = floor fv :: Int
                        f = fv - fromIntegral j
                        x = floor u :: Int
                    addCell x j (1 - f)
                    addCell x (j + 1) f
                  else do
                    let fu = u - 0.5
                        j = floor fu :: Int
                        f = fu - fromIntegral j
                        y = floor v :: Int
                    addCell j y (1 - f)
                    addCell (j + 1) y f
                go (i + 1)
      go 0

-- 線分を画面の中だけに切る（画面の外の分を計算しないため）
clipSeg :: Float -> Float -> Float -> Float -> Maybe (Float, Float, Float, Float)
clipSeg x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
      step (t0, t1) (p, q)
        | abs p < 1e-9 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl step (0, 1) [(negate dx, x1), (dx, canvasW - x1), (negate dy, y1), (dy, canvasH - y1)]
  in if a > b then Nothing else Just (x1 + dx * a, y1 + dy * a, x1 + dx * b, y1 + dy * b)

drawAll :: Env -> World -> Picture
drawAll env w =
  let cx = canvasW * 0.5
      cy = canvasH * 0.55
      (gmx, gmy) = envMouse env
      mx = gmx + canvasW / 2
      my = canvasH / 2 - gmy
      ang = atan2 (my - cy) (mx - cx)        -- マウスの方向から光が来る
      dx = negate (cos ang)                  -- 光の向き（マウス→中心）
      dy = negate (sin ang)
      ex = negate dy                         -- 光に直角な向き
      ey = dx
      nRays = wRays w
      alpha = 3.2 / sqrt (fromIntegral nRays / 100)   -- 本数が増えたら 1 本を暗く
      outCol = (255 * 0.3 * alpha, 200 * 0.3 * alpha, 90 * 0.3 * alpha)
      inCol = (90 * 0.3 * alpha, 200 * 0.3 * alpha, 255 * 0.3 * alpha)
      offOf i = negate radius * 1.02 + radius * 2.04 * (fromIntegral i + 0.5) / fromIntegral nRays
      buf :: U.Vector Float
      buf = U.create (do
        b <- MU.replicate (gw * gh * 3) 0
        forM_ [0 .. nRays - 1] (\i ->
          forM_ (traceRay w cx cy dx dy outCol inCol ex ey (offOf i)) (plot b))
        pure b)
      px x y =
        let k = (y * gw + x) * 3
            q c v = fromIntegral (min 255 (floor (c + U.unsafeIndex buf v) :: Int))
        in (q 8 k, q 10 (k + 1), q 18 (k + 2))
  in Pictures
    [ pixelImage gw gh cell px
    , Translate (cx - canvasW / 2) (canvasH / 2 - cy) (Color (rgba255 255 255 255 90) (ring radius 2))
    , txt env 20 40 18 white
        ("n = " ++ nf 2 (wN w) ++ " (Up/Down)  Rays " ++ show nRays ++ " (L/R)  Bounces " ++ show (wBounce w)
         ++ " (B)  1: water 1.33  2: glass 1.5  3: diamond 2.42")
    , txt env 20 68 15 (gray 0.63)
        "Light comes from the mouse. Yellow = outside, cyan = inside. Bright curves are caustics (ray envelopes)."
    , txt env 20 90 15 (gray 0.63) "Reflectance comes from the Fresnel equations."
    , txt env 20 (envH env - 30) 14 (gray 0.63)
        "At n = 1.33, light that reflects once inside collects near 42 deg: the rainbow (sketch 11)."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wN = min 2.6 (wN w + 0.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wN = max 1 (wN w - 0.02) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRays = min 800 (wRays w + 40) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRays = max 40 (wRays w - 40) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> w { wBounce = (wBounce w + 1) `mod` 6 }
    | c == '1' -> w { wN = 1.33 }
    | c == '2' -> w { wN = 1.5 }
    | c == '3' -> w { wN = 2.42 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n を 1.0 から少しずつ上げる。火線の先（光が集まる点）はどう動く？ n = 2 では円の中に入る
-- 2. ダイヤ（n = 2.42）は反射率が高く、中で何度も反射する → きらめきの理由
-- 3. B で反射の回数を 0 にして、1 回目の屈折だけで火線（コップの底の三日月形）ができることを確かめよう
-- 4. 円を 2 つ並べて（hitCircle をもう 1 回）レンズのように光を集めよう


-- ==================================================
-- ここから下は「道具」（文字・数字）
-- ==================================================
-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""
