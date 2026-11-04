-- ============================================
-- 39-12 ピンホールカメラの射影（3D → 2D）
-- 3 次元の点 X = [X, Y, Z, 1] は、カメラ行列 P = K [R | t]（3×4）で画像の点 x = P X（同次座標）に写る
--   K = 内部パラメータ（焦点距離 f、画像の中心）、R, t = カメラの向きと位置。x = [u, v, w] → 画像の (u/w, v/w)
--   左：世界（立方体・格子・カメラと画像面）。右上：カメラが撮った 2D 画像（自前で計算して描く）
-- ドラッグ = 世界の視点を回す / ←→ = カメラを回す / W S = カメラの高さ / ↑↓ = 焦点距離 f / O = 自動で回る / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

camR, imgW, imgH, planeD, camZ :: Float
camR = 420      -- ★ カメラから原点までの距離
imgW = 360      -- ★ 画像の大きさ（px）
imgH = 270
planeD = 110    -- 画像面までの距離（表示用）
camZ = 400 / tan (pi / 6)   -- 世界を見る視点の距離（p5.js の WEBGL の標準）

focal0, yaw0, pitch0, viewX0, viewY0 :: Float
focal0 = 300    -- ★ 焦点距離（px）
yaw0 = 0.6      -- カメラの向き（原点を見る）
pitch0 = 0.35
viewX0 = -0.5   -- 世界を見る視点（マウスで回す）
viewY0 = 0.5

data World = World
  { wYaw, wPitch, wFocal :: !Float
  , wViewX, wViewY :: !Float
  , wAuto :: !Bool
  , wPrev :: !Point     -- 1 フレーム前のマウス
  }

-- 世界の線分（両端と色相）
data Seg3 = Seg3 !V3 !V3 !Float

-- ★ 世界：立方体（辺 120）、地面の格子、小さな三角錐
segs :: [Seg3]
segs = cube ++ grid ++ pyramid
  where
    s = 60
    c = [ V3 (-s) (-s) (-s), V3 s (-s) (-s), V3 s s (-s), V3 (-s) s (-s)
        , V3 (-s) (-s) s, V3 s (-s) s, V3 s s s, V3 (-s) s s ]
    cube = [ Seg3 (c !! a) (c !! b) 200 | (a, b) <- [(0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (3, 7)] ]
    grid = concat [ [Seg3 (V3 (i * 60) 90 (-180)) (V3 (i * 60) 90 180) 45, Seg3 (V3 (-180) 90 (i * 60)) (V3 180 90 (i * 60)) 45] | i <- [-3 .. 3] ]
    t = [V3 150 90 (-100), V3 210 90 (-100), V3 180 90 (-50), V3 180 10 (-85)]
    pyramid = [ Seg3 (t !! a) (t !! b) 15 | (a, b) <- [(0, 1), (1, 2), (2, 0), (0, 3), (1, 3), (2, 3)] ]

data Cam = Cam
  { cP :: [[Float]]   -- P = K [R | t]（3×4）
  , cK :: [[Float]]
  , cR :: [V3]        -- 行 = カメラの軸（世界 → カメラの回転）
  , cT :: [Float]
  , cC :: V3          -- カメラの位置
  }

main :: IO ()
main = runSketch (sketch "39-12 Pinhole camera matrix" (World yaw0 pitch0 focal0 viewX0 viewY0 False (0, 0)))
  { skBack  = hsb 30 35 11
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

step :: Env -> World -> World
step env w =
  let m@(mx, my) = mouseP5 env
      (px, py) = wPrev w
      w1 = if envDown env
             then w { wViewY = wViewY w + (mx - px) * 0.01, wViewX = clampF (-1.5) 1.5 (wViewX w + (my - py) * 0.01) }
             else w
  in w1 { wPrev = m, wYaw = if wAuto w1 then wYaw w1 + 0.008 else wYaw w1 }

-- ★ カメラ行列 P = K [R | t] を作る
cameraMatrix :: World -> Cam
cameraMatrix w =
  let yaw = wYaw w
      pitch = wPitch w
      f = wFocal w
      c = V3 (camR * cos pitch * sin yaw) (negate camR * sin pitch) (camR * cos pitch * cos yaw)   -- カメラの位置（y は下向き）
      z = unit3 (scale3 (-1) c)                -- 前（原点を見る）
      x = unit3 (cross3 (V3 0 1 0) z)          -- 右
      y = cross3 z x                           -- 下
      t = [negate (dot3 x c), negate (dot3 y c), negate (dot3 z c)]    -- t = −R C
      k = [[f, 0, imgW / 2], [0, f, imgH / 2], [0, 0, 1]]
      list (V3 a b d) = [a, b, d]
      rt = zipWith (\r ti -> list r ++ [ti]) [x, y, z] t               -- 3×4
      p = [ [ sum (zipWith (\kv row -> kv * (row !! j)) krow rt) | j <- [0 .. 3] ] | krow <- k ]
  in Cam p k [x, y, z] t c

-- 3D の点を画像に写す → (u, v, w)
projectPt :: Cam -> V3 -> (Float, Float, Float)
projectPt cam (V3 x y z) =
  case [ a * x + b * y + c * z + d | [a, b, c, d] <- cP cam ] of
    [r0, r1, r2] -> (r0 / r2, r1 / r2, r2)
    _ -> (0, 0, 0)

-- 画像の点 (u, v) を、カメラから距離 d の画像面の上の世界の点に戻す（画像面を 3D に描くため）
imageToWorld :: World -> Cam -> Float -> Float -> V3
imageToWorld w cam u v =
  let f = wFocal w
      dx = (u - imgW / 2) / f              -- K⁻¹ [u, v, 1]
      dy = (v - imgH / 2) / f
  in case cR cam of
       [r0, r1, r2] -> add3 (cC cam) (scale3 planeD (add3 (add3 (scale3 dx r0) (scale3 dy r1)) r2))   -- Rᵀ dir
       _ -> cC cam

inImg :: (Float, Float, Float) -> Bool
inImg (u, v, _) = u > -imgW && u < 2 * imgW && v > -imgH && v < 2 * imgH

draw :: Env -> World -> Picture
draw env w = view env [world3D env w cam, hud env w cam]
  where cam = cameraMatrix w

-- ---- 3D の世界（奥から順に描く）
world3D :: Env -> World -> Cam -> Picture
world3D env w cam = Pictures (map snd (depthSort fst items))
  where
    turn v = rotX (wViewX w) (rotY (wViewY w) v)
    zOf (V3 _ _ z) = z
    scr v = let (x, y) = project camZ v in (envW env * 0.28 + x, envH env / 2 + y)   -- ★ 右のパネルにカメラが隠れないよう左に寄せる
    line3 sw c a b =
      let ta = turn a
          tb = turn b
      in ((zOf ta + zOf tb) / 2, seg sw c (scr ta) (scr tb))
    corners = [imageToWorld w cam u v | (u, v) <- [(0, 0), (imgW, 0), (imgW, imgH), (0, imgH)]]
    yellow = hsb 45 90 100
    tc = turn (cC cam)
    items =
      [ line3 2 (hsb hue 60 90) a b | Seg3 a b hue <- segs ]
      -- カメラ（点）と画像面（4 隅）と光線
      ++ concat (zipWith (\a b -> [line3 2 yellow a b, line3 2 yellow (cC cam) a]) corners (drop 1 corners ++ take 1 corners))
      ++ [ (zOf tc, Color yellow (at (scr tc) (disc (6 * camZ / max 1 (camZ - zOf tc))))) ]
      -- 画像面の上に像を描く（投影した線分を画像面の位置に置く）
      ++ [ line3 1.5 (hsb hue 70 100) (imageToWorld w cam ua va) (imageToWorld w cam ub vb)
         | Seg3 a b hue <- segs
         , let pa@(ua, va, wa) = projectPt cam a
         , let pb@(ub, vb, wb) = projectPt cam b
         , wa > 1, wb > 1, inImg pa, inImg pb ]
      -- 立方体の頂点 1 つからカメラへ向かう光線
      ++ [ line3 1 (hsba 0 0 100 0.4) (V3 60 (-60) 60) (cC cam) ]

-- ---- HUD（2D）
hud :: Env -> World -> Cam -> Picture
hud env w cam = Pictures $
  -- 右上に画像（自前で計算して描く）
  [ fillRect (hsb 30 20 18) px py imgW imgH
  , seg 1 (hsba 0 0 100 0.2) (px, py + imgH / 2) (px + imgW, py + imgH / 2)
  , seg 1 (hsba 0 0 100 0.2) (px + imgW / 2, py) (px + imgW / 2, py + imgH) ]
  ++ [ seg 2 (hsb hue 70 100) c d
     | Seg3 a b hue <- segs
     , let (ua, va, wa) = projectPt cam a
     , let (ub, vb, wb) = projectPt cam b
     , wa > 1, wb > 1
     , Just (c, d) <- [clipSeg (px, py, px + imgW, py + imgH) (px + ua, py + va) (px + ub, py + vb)] ]
  ++ [ closedPath 2 (hsb 45 90 100) [(px, py), (px + imgW, py), (px + imgW, py + imgH), (px, py + imgH)]
     , txt px (py - 8) 14 (hsb 45 90 100) ("Camera image (" ++ nf 0 imgW ++ "x" ++ nf 0 imgH ++ " px) = P X divided by w")
     -- 行列
     , fillRect (hsba 30 25 18 0.92) (px - 10) (py + imgH + 14) (imgW + 20) 212 ]
  ++ matrix 0 "K =" 0 (cK cam)
  ++ matrix 66 "[R|t] =" 2 (zipWith (\(V3 a b c) t -> [a, b, c, t]) (cR cam) (cT cam))
  ++ matrix 132 "P =" 0 (cP cam)
  ++ [ txt 20 28 18 white ("f = " ++ nf 0 (wFocal w) ++ " px (Up/Down)   Camera: Left/Right turn   W/S height   distance " ++ nf 0 camR)
     , txt 20 50 14 (gray 0.8) ("O: auto " ++ (if wAuto w then "ON" else "off") ++ "   Drag: rotate world view   R: reset")
     , txt 20 70 14 (gray 0.65) "Yellow box = image plane. A ray from a 3D point through the pinhole hits it. Large f = tele (big, flat)." ]
  where
    px = envW env - imgW - 24
    py = 96
    y0 = py + imgH + 36
    matrix dy name d rows = concat
      [ (if r == 0 then [txt px yy 14 white name] else [])
        ++ [ txt (px + 66 + fromIntegral c * 74) yy 14 white (nf (if c == 3 then 0 else d) v) | (c, v) <- zip [0 :: Int ..] row ]
      | (r, row) <- zip [0 :: Int ..] rows, let yy = y0 + dy + fromIntegral r * 19 ]

-- 線分を枠の中に切りつめる（リャン＝バースキー法）。p5.js 版の画像バッファのふちの代わり
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
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
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wYaw = wYaw w - 0.1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wYaw = wYaw w + 0.1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wFocal = min 1200 (wFocal w + 30) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wFocal = max 80 (wFocal w - 30) }
  EventKey (Char c) Down _ _
    | c `elem` "wW" -> w { wPitch = min 1.3 (wPitch w + 0.08) }
    | c `elem` "sS" -> w { wPitch = max (-0.3) (wPitch w - 0.08) }
    | c `elem` "oO" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> w { wYaw = yaw0, wPitch = pitch0, wFocal = focal0, wViewX = viewX0, wViewY = viewY0, wAuto = False }
  _ -> w

-- ---------- やってみよう ----------
-- 1. f を 80 にすると超広角：格子の遠くが急に小さくなる。f = 1200 なら望遠：ほぼ平行投影。P の数値もくらべよう
-- 2. 地面の格子の平行線は画像の中でどこに集まる？（消失点 = 方向 [1, 0, 0, 0] の像 = P の 1 列目！）
-- 3. camR を変えずに f を変えるのと、f を変えずに camR を変えるのでは、画像はどう違う？（ドリーズーム）
-- 4. 立方体の 1 つの面（4 点）は画像でも四角形。その 4 点の対応からホモグラフィ（03）が作れる = 平面のカメラ画像はホモグラフィ

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
