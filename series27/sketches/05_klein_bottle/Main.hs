-- ============================================
-- 05 クラインの壺と射影平面（3D）
-- 正方形の辺を矢印どおりに貼り合わせると曲面ができる：向きをそろえて貼るとトーラス、片方を逆にするとクラインの壺
--   右の正方形の中を歩くと、辺から出たとき貼り合わせた相手の辺から入ってくる。左は同じ点を 3D で見たもの
-- 1 = トーラス / 2 = クラインの壺 / 3 = 射影平面 / 4 = 球面 / ←→ = 向きを変える / ↑↓ = 速さ / A = 自動で歩く / C = 跡を消す / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art

width, height, s3, camD :: Float
width = 1280
height = 800
s3 = min width height * 0.2       -- 3D の大きさ
camD = 693                        -- カメラまでの距離（p5.js の標準と同じ）

trailMax :: Int
trailMax = 1500                   -- ★ 跡の長さ

-- | 跡の 1 点：正方形の座標と、3D の点。TBreak は辺を越えた切れ目
data TrailP = TP !Float !Float !V3 | TBreak

data World = World
  { wSurf :: !Int
  , wX, wY, wHead, wSpeed :: !Float   -- ★ 歩く人の位置・向き・速さ
  , wAuto :: !Bool
  , wTrail :: ![TrailP]               -- 新しい順
  , wRotX, wRotY :: !Float
  , wPrev :: !(Float, Float)
  }

main :: IO ()
main = runSketch (sketch "27-05 Klein Bottle and Projective Plane" (World 1 0.3 0.4 0.5 0.004 True [] (-0.5) 0.4 (0, 0)))
  { skBack  = rgb255 14 18 30
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | 正方形の場所の目印の色（0〜255）
squareColor :: Float -> Float -> (Float, Float, Float)
squareColor x y = (60 + 190 * x, 60 + 170 * y, 220 - 120 * x * y)

-- | ★ 正方形の点 (x, y)（0〜1）→ 3D の点
surfPoint :: Int -> Float -> Float -> V3
surfPoint surf x y =
  let u = x * tau
      v = y * tau
  in case surf of
       1 -> let r = 0.45 in V3 ((1 + r * cos v) * cos u) (r * sin v) ((1 + r * cos v) * sin u)
       2 -> -- クラインの壺の「8 の字」型のはめこみ（自分と交わる）
         let q = cos (u / 2) * sin v - sin (u / 2) * sin (2 * v)
         in V3 ((2 + q) * cos u / 2.6) ((sin (u / 2) * sin v + cos (u / 2) * sin (2 * v)) / 2.6) ((2 + q) * sin u / 2.6)
       3 -> -- 射影平面：正方形 → 半球 → ローマ曲面 (yz, xz, xy)
         let px = 2 * x - 1
             py = 2 * y - 1
             m = max (abs px) (abs py)
             rr = sqrt (px * px + py * py)
             dx = if rr > 1e-9 then px * m / rr else 0
             dy = if rr > 1e-9 then py * m / rr else 0
             dz = sqrt (max 0 (1 - dx * dx - dy * dy))
         in V3 (dy * dz * 2.2) (dx * dy * 2.2) (dx * dz * 2.2)
       _ -> -- 球面：となりあう辺どうしを貼る（左＝下、右＝上）
         let s = x + y
             d = x - y
             m = min s (2 - s)
             phi = if m > 1e-6 then pi * d / m else 0
             th = pi * s / 2
         in V3 (sin th * cos phi) (-cos th) (sin th * sin phi)

-- | ★ 辺から出たら、貼り合わせた相手の辺から入る。戻り値の Bool は「越えた」
wrap :: World -> (World, Bool)
wrap w
  | wSurf w == 4 =
      let x = wX w
          y = wY w
          vx = cos (wHead w)
          vy = sin (wHead w)
          (x', y', vx', vy', out)
            | x < 0 = (y, -x, vy, -vx, True)
            | y < 0 = (-y, x, -vy, vx, True)
            | x > 1 = (y, 2 - x, vy, -vx, True)
            | y > 1 = (2 - y, x, -vy, vx, True)
            | otherwise = (x, y, vx, vy, False)
      in (w { wX = clampF 0 1 x', wY = clampF 0 1 y', wHead = atan2 vy' vx' }, out)
  | otherwise =
      let flipY = wSurf w == 2 || wSurf w == 3
          flipX = wSurf w == 3
          (x1, y1, h1, o1)
            | wX w < 0 || wX w > 1 =
                let x0 = fract (wX w)
                in if flipY then (x0, 1 - wY w, -wHead w, True) else (x0, wY w, wHead w, True)
            | otherwise = (wX w, wY w, wHead w, False)
          (x2, y2, h2, o2)
            | y1 < 0 || y1 > 1 =
                let y0 = fract y1
                in if flipX then (1 - x1, y0, pi - h1, True) else (x1, y0, h1, True)
            | otherwise = (x1, y1, h1, False)
      in (w { wX = clampF 0 1 x2, wY = clampF 0 1 y2, wHead = h2 }, o1 || o2)

---------------------------------------------------------------- 動き

walk1 :: World -> World
walk1 w =
  let w1 = w { wX = wX w + cos (wHead w) * wSpeed w, wY = wY w + sin (wHead w) * wSpeed w }
      (w2, out) = wrap w1
      p = TP (wX w2) (wY w2) (scale3 s3 (surfPoint (wSurf w2) (wX w2) (wY w2)))
  in w2 { wTrail = p : (if out then TBreak : wTrail w2 else wTrail w2) }

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let m@(mx, my) = toP5 (envMouse env)
      (px, py) = wPrev w0
      w1 | envDown env && mx < width * 0.55 =
             w0 { wRotY = wRotY w0 + (mx - px) * 0.01, wRotX = clampF (-1.5) 1.5 (wRotX w0 - (my - py) * 0.01) }
         | otherwise = w0
      turn = (if isDown env (SpecialKey KeyLeft) then 0.04 else 0) - (if isDown env (SpecialKey KeyRight) then 0.04 else 0)
      wander = if wAuto w1 then (noise2 (fromIntegral (envFrame env) * 0.01) 0.5 - 0.5) * 0.06 else 0
      w2 = w1 { wHead = wHead w1 + turn + wander, wPrev = m }
      w3 = walk1 (walk1 w2)
  in w3 { wTrail = take trailMax (wTrail w3) }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> w { wSurf = fromEnum c - fromEnum '0', wTrail = [] }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "cC" -> w { wTrail = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 0.02 (wSpeed w + 0.001) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 0 (wSpeed w - 0.001) }
  _ -> w

---------------------------------------------------------------- 描画

viewFn :: World -> V3 -> V3
viewFn w =
  let cx = cos (wRotX w)
      sx = sin (wRotX w)
      cy = cos (wRotY w)
      sy = sin (wRotY w)
  in \(V3 x y z) ->
       let x1 = x * cy + z * sy
           z1 = z * cy - x * sy
       in V3 (x1 - width * 0.2) (y * cx - z1 * sx + 30) (y * sx + z1 * cx)

scr :: V3 -> Point
scr v = let (x, y) = project camD v in (x, -y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- | 曲面の網（曲面ごとに 1 回だけ計算する）。各点は (3D の点, 色)
meshes :: [[[(V3, (Float, Float, Float))]]]
meshes = map build [1, 2, 3, 4]
  where
    nx = 48 :: Int
    ny = 32 :: Int
    build s = [ [ (scale3 s3 (surfPoint s x y), squareColor x y)
                | i <- [0 .. nx], let x = fromIntegral i / fromIntegral nx ]
              | j <- [0 .. ny], let y = fromIntegral j / fromIntegral ny ]

lightDir :: V3
lightDir = unit3 (V3 0.4 0.6 (-0.7))

meshItems :: World -> (V3 -> V3) -> [(Float, Picture)]
meshItems w view =
  let rows = map (map (\(p, c) -> (view p, c))) (case drop (wSurf w - 1) meshes of
                                                   (m : _) -> m
                                                   [] -> [])
      quad (a, (r, g, b)) (b', _) (c', _) (d, _) =
        let n = unit3 (cross3 (sub3 c' a) (sub3 d a))
            s = (0.47 + 0.7 * abs (dot3 n lightDir)) / 255
            z = (zOf a + zOf b' + zOf c' + zOf d) / 4
        in (z, Color (makeColor (min 1 (r * s)) (min 1 (g * s)) (min 1 (b * s)) 1) (Polygon (map scr [a, b', c', d])))
      strip r1 r2 = zip4With quad r1 (drop 1 r1) (drop 1 r2) r2
  in concat (zipWith strip rows (drop 1 rows))

zip4With :: (a -> a -> a -> a -> b) -> [a] -> [a] -> [a] -> [a] -> [b]
zip4With f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zip4With f as bs cs ds
zip4With _ _ _ _ _ = []

drawWorld :: World -> Picture
drawWorld w =
  let view = viewFn w
      red' = rgb255 255 60 60
      -- 跡と歩く人
      trailSeg (TP _ _ a) (TP _ _ b) =
        let va = view a
            vb = view b
        in [((zOf va + zOf vb) / 2 + 4, Color red' (thickLine 3 (scr va) (scr vb)))]
      trailSeg _ _ = []
      trail3 = concat (zipWith trailSeg (wTrail w) (drop 1 (wTrail w)))
      p0 = view (scale3 s3 (surfPoint (wSurf w) (wX w) (wY w)))
      k0 = camD / max 1 (camD - zOf p0)
      walker = (zOf p0 + 12, at (scr p0) (Color (rgb255 255 230 60) (disc (9 * k0))))
      items = meshItems w view ++ trail3 ++ [walker]
  in Pictures (map snd (depthSort fst items) ++ [drawHud w])

-- | 右の正方形の色見本（1 回だけ作る）
sqImg :: Picture
sqImg = pixelImage 128 128 (qQ / 128) (\i j ->
  let (r, g, b) = squareColor (fromIntegral i / 127) (1 - fromIntegral j / 127)
      q v = fromIntegral (max 0 (min 255 (round v :: Int)))
  in (q r, q g, q b))

qQ, x0, y0 :: Float
qQ = min (width * 0.34) (height * 0.58)
x0 = width - qQ - 70
y0 = 150

drawHud :: World -> Picture
drawHud w =
  let surf = wSurf w
      p2s x y = (x0 + x * qQ - width / 2, height / 2 - (y0 + (1 - y) * qQ))
      red' = rgb255 255 60 60
      seg (TP ax ay _) (TP bx by _) = [Color red' (thickLine 2 (p2s ax ay) (p2s bx by))]
      seg _ _ = []
      notes = case surf of
        1 -> ["Orientation is kept. A donut with one hole"]
        2 -> ["Left and right edges glue upside down", "-> it passes through itself in 3D"]
        3 -> ["Both pairs glue upside down", "-> a closed surface with no orientation"]
        _ -> ["Neighboring edges glue together", "-> close the envelope, get a balloon (sphere)"]
      name = case surf of
        1 -> "Torus"
        2 -> "Klein bottle"
        3 -> "Projective plane"
        _ -> "Sphere"
      word = case surf of
        1 -> "a b a^-1 b^-1"
        2 -> "a b a b^-1"
        3 -> "a b a b"
        _ -> "a a^-1 b b^-1"
  in Pictures $
       [ txt 20 40 14 (gray 1) ("1: torus  2: Klein  3: proj. plane  4: sphere  <- ->: turn  Up/Down: speed  A: auto "
                                ++ (if wAuto w then "ON" else "off") ++ "  C: clear  Drag: view")
       , txt 20 68 14 (gray (170 / 255)) "Glue same letters so the arrows match. Colors mark places on the square (same as the 3D view)."
       , at (x0 + qQ / 2 - width / 2, height / 2 - (y0 + qQ / 2)) sqImg ]
       ++ concat (zipWith seg (wTrail w) (drop 1 (wTrail w)))
       ++ [ at (p2s (wX w) (wY w)) (Color (rgb255 255 230 60) (disc 7)) ]
       ++ concatMap edgeArrow (arrowsOf surf)
       ++ [ txt x0 (y0 + qQ + 62) 26 (rgb255 255 230 120) name
          , txt x0 (y0 + qQ + 88) 16 (gray 1) ("Edge word: " ++ word)
          , txt x0 (y0 + qQ + 108) 14 (gray (170 / 255)) "(read from bottom-left, counterclockwise)" ]
       ++ [ txt x0 (y0 + qQ + 130 + fromIntegral i * 20) 14 (gray (170 / 255)) s | (i, s) <- zip [0 :: Int ..] notes ]

-- | 各辺の矢印（辺, 向き, 文字）
arrowsOf :: Int -> [(Char, Int, Char)]
arrowsOf s = case s of
  1 -> [('B', 1, 'a'), ('T', 1, 'a'), ('L', 1, 'b'), ('R', 1, 'b')]
  2 -> [('B', 1, 'a'), ('T', 1, 'a'), ('L', 1, 'b'), ('R', -1, 'b')]
  3 -> [('B', 1, 'a'), ('T', -1, 'a'), ('L', 1, 'b'), ('R', -1, 'b')]
  _ -> [('L', 1, 'a'), ('B', 1, 'a'), ('R', 1, 'b'), ('T', 1, 'b')]

edgeArrow :: (Char, Int, Char) -> [Picture]
edgeArrow (e, d, lab) =
  let col = if lab == 'a' then rgb255 255 150 60 else rgb255 90 220 255
      (ax, ay, bx, by) = case e of
        'B' -> (x0, y0 + qQ, x0 + qQ, y0 + qQ)
        'T' -> (x0, y0, x0 + qQ, y0)
        'L' -> (x0, y0 + qQ, x0, y0)
        _   -> (x0 + qQ, y0 + qQ, x0 + qQ, y0)
      (x1, y1, x2, y2) = if d < 0 then (bx, by, ax, ay) else (ax, ay, bx, by)
      mx = (x1 + x2) / 2
      my = (y1 + y2) / 2
      ang = atan2 (y2 - y1) (x2 - x1)
      n = if lab == 'a' then 1 else 2 :: Int
      ln w (p, q) (r, s) = Color col (thickLine w (pt (p, q)) (pt (r, s)))
      chev k =
        let cx = mx + cos ang * fromIntegral k * 16
            cy = my + sin ang * fromIntegral k * 16
        in [ ln 6 (cx, cy) (cx - 16 * cos (ang - 0.6), cy - 16 * sin (ang - 0.6))
           , ln 6 (cx, cy) (cx - 16 * cos (ang + 0.6), cy - 16 * sin (ang + 0.6)) ]
      lx = mx + (if e == 'L' then -34 else if e == 'R' then 18 else 6)
      ly = my + (if e == 'T' then -14 else if e == 'B' then 30 else 6)
  in ln 6 (x1, y1) (x2, y2) : concatMap chev [0 .. n - 1] ++ [txt lx ly 22 col [lab]]

-- ---------- やってみよう ----------
-- 1. クラインの壺で、まっすぐ右へ歩き続けると、上下がさかさまになって戻ってくる。跡で確かめよう
-- 2. トーラスで向きを「右に 1、上に 2」の傾きにすると跡が閉じる。無理数の傾きだと？
-- 3. 射影平面の正方形で、中心を通ってまっすぐ歩くと何回で元に戻る？
-- 4. arrowsOf に自分の貼り方（たとえば上下だけ逆）を追加して、どの曲面になるか考えよう

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
