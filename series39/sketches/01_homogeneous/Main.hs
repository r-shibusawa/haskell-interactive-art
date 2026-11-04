-- ============================================
-- 39-01 同次座標：[x, y, w] と無限遠点
-- 平面の点 (X, Y) を 3 つの数 [x, y, w]（X = x/w, Y = y/w）で表す。[x, y, w] と [2x, 2y, 2w] は同じ点（スケール倍は同一視）
--   w → 0 にすると点は方向 (x, y) に飛んでいき、w = 0 で「無限遠点」になる。w が負になると反対側から帰ってくる！
--   右：射影平面の球面モデル。原点を通る直線（= 1 点）が球と交わる 2 点（対蹠点）を同じ点とみなす。赤道 = 無限遠直線
-- 左の図でドラッグ = (x, y) を決める / ←→ = w を変える / 0 = w を 0 に / 1 = w を 1 に / M = スケール倍を表示 / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- ★ 同次座標 [x, y, w] の最初の値
x0, y0, w0 :: Float
x0 = 1.4
y0 = 0.9
w0 = 1

data World = World
  { wX, wY, wW :: !Float
  , wMult  :: !Bool       -- スケール倍を表示
  , wTrail :: ![Point]    -- P の軌跡（新しい順、最大 150 点）
  , wHold  :: !Int        -- ←→ を押し続けているフレーム数
  }

type Rc = (Float, Float, Float, Float)

main :: IO ()
main = runSketch (sketch "39-01 Homogeneous coordinates [x, y, w]" (World x0 y0 w0 True [] 0))
  { skBack  = hsb 30 35 11
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

unitPx :: Env -> Float
unitPx env = min (envW env / 3) (envH env) * 0.16   -- 1 単位 = U px

-- 3 つの図の枠（左：平面 w=1、中：x–w 断面、右：球面モデル）
panels :: Env -> (Rc, Rc, Rc)
panels env =
  let w = envW env / 3
      top = 100
      h = envH env - 150
  in ((0, top, w - 10, top + h), (w, top, 2 * w - 10, top + h), (2 * w, top, 3 * w - 10, top + h))

step :: Env -> World -> World
step env w =
  let (l@(_, lt, lr, _), _, _) = panels env
      (mx, my) = mouseP5 env
      u = unitPx env
      (cx, cy) = center l
      w1 = if envDown env && mx < lr && my > lt
             then w { wX = clampF (-6) 6 ((mx - cx) / u), wY = clampF (-6) 6 ((cy - my) / u) }
             else w
      -- ←→ は押し続けると少しずつ w が動く
      l' = isDown env (SpecialKey KeyLeft)
      r' = isDown env (SpecialKey KeyRight)
      hold = if l' || r' then wHold w1 + 1 else 0
      w2 = if hold >= 20 && hold `mod` 2 == 0
             then (if l' then stepW (-0.04) else stepW 0.04) w1
             else w1
      trail = case toPlane w2 of
        Just p -> take 150 (p : wTrail w2)
        Nothing -> wTrail w2
  in w2 { wHold = hold, wTrail = trail }

stepW :: Float -> World -> World
stepW d w = w { wW = clampF (-2) 2 (wW w + d) }

toPlane :: World -> Maybe Point
toPlane w = if abs (wW w) < 1e-6 then Nothing else Just (wX w / wW w, wY w / wW w)

draw :: Env -> World -> Picture
draw env w = view env
  [ drawPlane env w l
  , drawSection env w m
  , drawSphere w r
  , txt 20 28 18 white ("[x, y, w] = [" ++ nf 2 hx ++ ", " ++ nf 2 hy ++ ", " ++ nf 2 hw ++ "]  ->  " ++ case toPlane w of
      Just (px, py) -> "(X, Y) = (x/w, y/w) = (" ++ nf 2 px ++ ", " ++ nf 2 py ++ ")"
      Nothing -> "point at infinity, direction (" ++ nf 2 hx ++ ", " ++ nf 2 hy ++ ")")
  , txt 20 50 14 (gray 0.8) "Left/Right: w (hold)   0/1: w = 0, 1   M: multiples   R: reset   Drag in left panel: (x, y)"
  , txt 20 70 14 (gray 0.65) "Small w: far away. w = 0: at infinity (arrow). w < 0: back from the other side - infinity is connected."
  ]
  where
    (l, m, r) = panels env
    hx = wX w; hy = wY w; hw = wW w

-- 左：ふつうの平面（w = 1 の切り口）。点 (x/w, y/w)
drawPlane :: Env -> World -> Rc -> Picture
drawPlane env w b@(b0, b1, b2, b3) = Pictures $
  [frame b "Plane (slice w = 1)"]
  ++ [seg 1 (hsba 0 0 100 0.15) (cx + i * u, b1) (cx + i * u, b3) | i <- [-20 .. 20], cx + i * u > b0, cx + i * u < b2]
  ++ [seg 1 (hsba 0 0 100 0.15) (b0, cy + i * u) (b2, cy + i * u) | i <- [-20 .. 20], cy + i * u > b1, cy + i * u < b3]
  ++ [seg 1 (hsba 0 0 100 0.4) (cx, b1) (cx, b3), seg 1 (hsba 0 0 100 0.4) (b0, cy) (b2, cy)]
  -- 軌跡
  ++ [Color (hsba 200 60 95 0.4) (thickPath 2 [s | p <- wTrail w, let s = toScreen p, inside s b])]
  -- (x, y) そのもの（薄い矢印）
  ++ [ seg 2 (hsba 45 80 100 0.6) (cx, cy) xy
     , dotAt (hsba 45 80 100 0.6) xy 10
     , txt (fst xy + 8) (snd xy + 20) 14 (hsb 45 80 100) "(x, y)" ]
  -- 点 P
  ++ [case toPlane w of
        Just p ->
          let s@(sx, sy) = toScreen p
          in if inside s b
               then Pictures [dotAt (hsb 15 80 100) s 16, txt (sx + 12) (sy - 10) 16 white "P = (x/w, y/w)"]
               else edgeArrow b (fst p) (snd p) (cx, cy) "far away"
        Nothing -> edgeArrow b hx hy (cx, cy) "at infinity"]
  ++ (if wMult w
        then txt (b0 + 16) (b3 - 94) 14 (gray 0.8) "Same point, multiplied coordinates:"
             : [ txt (b0 + 16) (b3 - 72 + fromIntegral i * 17) 14 (gray 0.8)
                   ("[" ++ nf 2 (hx * k) ++ ", " ++ nf 2 (hy * k) ++ ", " ++ nf 2 (wW w * k) ++ "]")
               | (i, k) <- zip [0 :: Int ..] [2, 0.5, -1] ]
             ++ [txt (b0 + 180) (b3 - 72) 14 (gray 0.8) "All have the", txt (b0 + 180) (b3 - 55) 14 (gray 0.8) "same x/w, y/w"]
        else [])
  where
    (cx, cy) = center b
    u = unitPx env
    hx = wX w
    hy = wY w
    xy = (cx + hx * u, cy - hy * u)
    toScreen (px, py) = (cx + px * u, cy - py * u)

-- 中：x–w 断面。原点からの直線が「画面 w = 1」に当たる所が X = x/w
drawSection :: Env -> World -> Rc -> Picture
drawSection env w b@(b0, b1, b2, b3) = Pictures $
  [ frame b "x-w section: line through 0 = 1 point"
  , seg 1 (hsba 0 0 100 0.4) (b0, cy) (b2, cy), seg 1 (hsba 0 0 100 0.4) (cx, b1) (cx, b3)
  , seg 2 blue (b0, cy - u) (b2, cy - u)
  , txt (b0 + 10) (cy - u - 8) 14 blue "w = 1 (screen)"
  , txt (cx + 8) (cy + 18) 14 (gray 0.7) "w = 0", txt (b2 - 20) (cy - 8) 14 (gray 0.7) "x", txt (cx + 8) (b1 + 20) 14 (gray 0.7) "w"
    -- 原点を通る直線（方向 (x, w)）
  , maybe Blank (uncurry (seg 2 (hsba 45 80 100 0.7))) (clipSeg b (cx - dx * 2000, cy + dw * 2000) (cx + dx * 2000, cy - dw * 2000))
  , seg 4 (hsb 45 80 100) (cx, cy) xw
  , dotAt (hsb 45 80 100) xw 10
  , txt (fst xw + 8) (snd xw + 20) 14 (hsb 45 80 100) "(x, w)"
  , if abs hw > 1e-6
      then let bigX = hx / hw
               sx = cx + bigX * u
           in if sx > b0 && sx < b2
                then Pictures [dotAt (hsb 15 80 100) (sx, cy - u) 14, txt (sx + 10) (cy - u - 12) 14 white ("X = x/w = " ++ nf 2 bigX)]
                else txt (b0 + 10) (cy - u + 22) 14 (hsb 15 80 100) ("Meet is outside the frame (X = " ++ nf 1 bigX ++ ")")
      else txt (b0 + 10) (cy - u + 22) 14 (hsb 15 80 100) "Line parallel to screen: at infinity"
  , txt (b0 + 16) (b3 - 40) 14 (gray 0.7) "(x, w), (2x, 2w), (-x, -w) on this line"
  , txt (b0 + 16) (b3 - 22) 14 (gray 0.7) "hit w = 1 at one place: the same point"
  ]
  where
    cx = (b0 + b2) / 2
    cy = b3 - 80     -- 原点（w = 0）
    u = unitPx env
    hx = wX w
    hw = wW w
    m = let v = sqrt (hx * hx + hw * hw) in if v == 0 then 1 else v
    dx = hx / m
    dw = hw / m
    xw = (cx + hx * u, cy - hw * u)
    blue = hsb 200 60 95

-- 右：球面モデル（上から見た図）。方向 (x, y, w) と球の交点とその対蹠点
drawSphere :: World -> Rc -> Picture
drawSphere w b@(b0, b1, b2, b3) = Pictures $
  [ frame b "Sphere: antipodes = 1 point"
  , ringAt (hsba 0 0 100 0.5) (cx, cy) (r * 2) 2 ]
  ++ [ringAt (hsba 0 0 100 0.15) (cx, cy) (r * 2 * sin (k * pi / 8)) 1 | k <- [1, 2, 3]]
  ++ [ seg 1 (hsba 45 80 100 0.5) s a
     , dotAt (hsb 15 80 100) (if nw >= 0 then s else a) 16
     , ringAt (hsb 15 80 100) (if nw >= 0 then a else s) 16 3
     , txt (min (fst s + 10) (b2 - 150)) (snd s - 8) 14 white ("w = " ++ nf 2 nw ++ (if nw >= 0 then " (upper)" else " (lower)"))
     , txt (min (fst a + 10) (b2 - 190)) (snd a + 18) 14 white "antipode (same point)"
     , txt (cx - r) (cy + r + 24) 14 (gray 0.7) "Equator (w = 0) = line at infinity"
     , txt (b0 + 16) (b3 - 40) 14 (gray 0.7) ("Small |w|: near equator. Radius = " ++ nf 2 (sqrt (nx * nx + ny * ny)))
     , txt (b0 + 16) (b3 - 22) 14 (gray 0.7) "Infinity is an ordinary point on the sphere"
     ]
  where
    (cx, cy) = center b
    r = min (b2 - b0) (b3 - b1) * 0.36
    hx = wX w; hy = wY w; hw = wW w
    m = let v = sqrt (hx * hx + hy * hy + hw * hw) in if v == 0 then 1 else v
    nx = hx / m; ny = hy / m; nw = hw / m
    -- 北半球側（w > 0）を実線の点、南半球側（w < 0）を白抜き。上から見ているので (nx, ny) の位置
    s = (cx + nx * r, cy - ny * r)
    a = (cx - nx * r, cy + ny * r)

center :: Rc -> Point
center (b0, b1, b2, b3) = ((b0 + b2) / 2, (b1 + b3) / 2)

inside :: Point -> Rc -> Bool
inside (x, y) (b0, b1, b2, b3) = x > b0 && x < b2 && y > b1 && y < b3

frame :: Rc -> String -> Picture
frame (b0, b1, b2, b3) title = Pictures
  [ strokeRect (hsba 0 0 100 0.3) b0 b1 (b2 - b0) (b3 - b1)
  , txt (b0 + 12) (b1 - 8) 14 (hsb 45 80 100) title ]

-- 枠のふちに方向の矢印
edgeArrow :: Rc -> Float -> Float -> Point -> String -> Picture
edgeArrow (b0, b1, b2, b3) dx0 dy0 (cx, cy) name =
  let m0 = sqrt (dx0 * dx0 + dy0 * dy0)
      m = if m0 == 0 then 1 else m0
      dx = dx0 / m
      dy = negate dy0 / m
      ts = [(b2 - 30 - cx) / dx | dx > 1e-9] ++ [(b0 + 30 - cx) / dx | dx < -1e-9]
        ++ [(b3 - 30 - cy) / dy | dy > 1e-9] ++ [(b1 + 30 - cy) / dy | dy < -1e-9]
      t = foldr min 1e6 ts
      x = if null ts then cx else cx + dx * t
      y = if null ts then cy else cy + dy * t
      a = atan2 dy dx
      rot (u, v) = (x + u * cos a - v * sin a, y + u * sin a + v * cos a)
      col = hsb 15 80 100
  in Pictures
       [ seg 3 col (rot (-30, 0)) (rot (10, 0)), seg 3 col (rot (10, 0)) (rot (0, -8)), seg 3 col (rot (10, 0)) (rot (0, 8))
       , txt (x - 40) (y - 14) 14 col name ]

-- 線分を枠の中に切りつめる（リャン＝バースキー法）
clipSeg :: Rc -> Point -> Point -> Maybe (Point, Point)
clipSeg (b0, b1, b2, b3) (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edge (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, c) = foldl edge (0, 1) [(-dx, x1 - b0), (dx, b2 - x1), (-dy, y1 - b1), (dy, b3 - y1)]
  in if a > c then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + c * dx, y1 + c * dy))

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> stepW (-0.04) w
  EventKey (SpecialKey KeyRight) Down _ _ -> stepW 0.04 w
  EventKey (Char '0') Down _ _ -> w { wW = 0 }
  EventKey (Char '1') Down _ _ -> w { wW = 1 }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> w { wMult = not (wMult w) }
    | c `elem` "rR" -> w { wX = x0, wY = y0, wW = w0, wTrail = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ← を押し続けて w を 1 → 0 → −1 にする。P はどう動く？ 球面モデルの点はどこを通る？
-- 2. (x, y) = (1, 0) で w = 0 のときの無限遠点と、(x, y) = (−1, 0) で w = 0 の無限遠点は同じ？（矢印と対蹠点を見る）
-- 3. 「同じ点の同次座標」は何通りある？ 0 倍だけはダメなのはなぜ？
-- 4. 直線 ax + by + c = 0 も [a, b, c] と書ける（02）。点 [x, y, w] が直線の上にある条件を内積で書いてみよう

-- ==================================================
-- ここから下は「道具」：p5.js と同じ座標（左上が原点、y は下向き）で描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- 図全体を「左上が原点、y は下向き」の座標に置く
view :: Env -> [Picture] -> Picture
view env ps = Translate (negate (envW env) / 2) (envH env / 2) (Scale 1 (-1) (Pictures ps))

-- gloss の座標 → p5.js の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = toP5 env (envMouse env)

-- 文字（(x, y) が左下。y が下向きなので文字だけ裏返して戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 塗りつぶした丸（d は直径。p5.js の circle と同じ）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 輪（d は直径、w は線の太さ）
ringAt :: Color -> Point -> Float -> Float -> Picture
ringAt c p d w
  | w <= 1 = Color c (at p (Circle (d / 2)))
  | otherwise = Color c (at p (ring (d / 2) w))

-- 線分（太さ 1 以下は細い線）
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b
  | w <= 0 = Blank
  | w <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine w a b)

-- 閉じた折れ線
closedPath :: Float -> Color -> [Point] -> Picture
closedPath _ _ [] = Blank
closedPath w c ps@(p : rest) = Pictures (zipWith (seg w c) ps (rest ++ [p]))

-- 左上 (x, y)、幅 w、高さ h の長方形
fillRect :: Color -> Float -> Float -> Float -> Float -> Picture
fillRect c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

strokeRect :: Color -> Float -> Float -> Float -> Float -> Picture
strokeRect c x y w h = closedPath 1 c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 四角形を塗る。gloss の Polygon はへこんだ形を正しく塗れないので、
-- へこみ・ねじれ（8 の字）のときは三角形に分ける
fillQuad :: Color -> [Point] -> Picture
fillQuad col [a, b, c, d]
  | sameSign [o a b c, o b c d, o c d a, o d a b] = Color col (Polygon [a, b, c, d])
  | o a b c * o a c d > 0 = tris [[a, b, c], [a, c, d]]
  | o b c d * o b d a > 0 = tris [[b, c, d], [b, d, a]]
  | otherwise = case (crossAt a b c d, crossAt b c d a) of
      (Just x, _) -> tris [[x, b, c], [x, d, a]]
      (_, Just x) -> tris [[a, b, x], [c, d, x]]
      _ -> Blank
  where
    o (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    sameSign vs = all (> 0) vs || all (< 0) vs
    tris ts = Color col (Pictures (map Polygon ts))
    -- 線分 pq と線分 rs の交点
    crossAt (px, py) (qx, qy) (rx, ry) (sx, sy) =
      let dx1 = qx - px; dy1 = qy - py; dx2 = sx - rx; dy2 = sy - ry
          den = dx1 * dy2 - dy1 * dx2
          t = ((rx - px) * dy2 - (ry - py) * dx2) / den
          u = ((rx - px) * dy1 - (ry - py) * dx1) / den
      in if abs den < 1e-6 || t < 0 || t > 1 || u < 0 || u > 1 then Nothing
         else Just (px + t * dx1, py + t * dy1)
fillQuad _ _ = Blank

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 小数 d 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf d v
  | isNaN v || isInfinite v = "-"
  | d <= 0 = show (round v :: Int)
  | otherwise = showFFloat (Just d) v ""
