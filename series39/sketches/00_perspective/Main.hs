-- ============================================
-- 39-00 透視図法：地面の格子と線路
-- 地面の平行線（線路・格子）は、画面の上では 1 つの点 = 消失点に集まる。これが「射影」の出発点
--   目の高さ h、焦点距離 f のピンホールで地面の点 (X, Z) を写す：  sx = cx + f·X / Z,  sy = cy + f·h / Z
--   Z → ∞ で (cx, cy) = 消失点。地平線はすべての「地面の無限遠点」の集まり
-- 消失点をドラッグ / ↑↓ = 地平線の高さ / ←→ = 目の高さ h / 1 = 1 点透視（線路） / 2 = 2 点透視（箱） / G = 作図線 / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

focal, gauge, spread :: Float
focal = 700     -- ★ 焦点距離（px）。大きいほど望遠
gauge = 1.2     -- ★ 線路の幅（m）
spread = 560    -- ★ 2 点透視のときの 2 つの消失点の間隔（px）

eyeH0 :: Float
eyeH0 = 1.6     -- ★ 目の高さ（m）

-- 箱：手前の縦の辺（x, 底の y, 高さ）と、左右の消失点へ向かう割合、色相
data BoxDef = BoxDef !Float !Float !Float !Float !Float !Float

boxes :: [BoxDef]
boxes = [BoxDef 0.32 0.88 0.34 0.42 0.5 15, BoxDef 0.62 0.80 0.22 0.3 0.36 200, BoxDef 0.48 0.70 0.10 0.2 0.25 45]

data World = World
  { wVp    :: !Point   -- 消失点（地平線の上）
  , wEyeH  :: !Float   -- 目の高さ
  , wMode  :: !Int     -- 1 = 1 点透視, 2 = 2 点透視
  , wGuide :: !Bool
  }

main :: IO ()
main = runSketch (sketch "39-00 Perspective: ground grid and rails" (World (640, 336) eyeH0 1 True))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env (World (0, 0) eyeH0 1 True))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> World -> World
resetAll env w = w { wVp = (envW env * 0.5, envH env * 0.42), wEyeH = eyeH0 }

step :: Env -> World -> World
step env w =
  let (mx, my) = mouseP5 env
  in if envDown env && my > 90
       then w { wVp = (clampF 40 (envW env - 40) mx, clampF 120 (envH env - 120) my) }
       else w

-- ★ 地面の点 (X, Z) [m]（X = 左右、Z = 奥行き）と高さ hgt [m] を画面に写す（1 点透視 = ピンホールそのもの）
groundPt :: World -> Float -> Float -> Float -> Point
groundPt w x z hgt =
  let (vx, vy) = wVp w
  in (vx + focal * x / z, vy + focal * (wEyeH w - hgt) / z)

draw :: Env -> World -> Picture
draw env w = view env
  [ fillRect (hsb 220 45 22) 0 0 (envW env) vy
  , fillRect (hsb 35 40 14) 0 vy (envW env) (envH env - vy)
  , if wMode w == 1 then onePoint env w else twoPoint env w
    -- 地平線と消失点
  , dashed (hsba 45 60 100 0.6) (0, vy) (envW env, vy) 12
  , Pictures [dotAt (hsb 45 90 100) p 12 | p <- if wMode w == 1 then [wVp w] else [(vx - spread / 2, vy), (vx + spread / 2, vy)]]
  , txt 20 28 18 white ((if wMode w == 1 then "1-point (rails)" else "2-point (boxes)") ++ "  Drag: VP  Up/Down: horizon  Left/Right: eye h = " ++ nf 1 (wEyeH w) ++ " m")
  , txt 20 50 14 (gray 0.8) ("G: guides " ++ (if wGuide w then "ON" else "off") ++ "   1/2: switch   R: reset")
  , txt 20 72 14 (gray 0.65) (if wMode w == 1
      then "All ground parallels meet at the VP. sy = cy + f*h/Z: doubling Z halves the distance to the horizon."
      else "Both VPs lie on the horizon. Only vertical edges stay parallel. Back edges = meets of lines to the VPs.")
  ]
  where (vx, vy) = wVp w

-- 1 点透視：格子・線路・電柱
onePoint :: Env -> World -> Picture
onePoint env w = Pictures (depthLines ++ crossLines ++ sleepers ++ rails ++ poles ++ guide)
  where
    far = 400
    near = 0.6
    p = groundPt w
    -- 奥行き方向の線（X = 一定）→ すべて消失点へ
    depthLines = [seg 1 (hsba 45 30 70 (if wGuide w then 0.3 else 0.12)) (p x near 0) (p x far 0) | x <- [-12 .. 12]]
    -- 横方向の線（Z = 一定）→ 地平線に平行、間隔が 1/Z で詰まる
    crossLines = [seg 1 (hsba 45 30 70 0.25) (p (-12) z 0) (p 12 z 0) | z <- [1 .. 60]]
    -- 枕木（流れる）
    flow = wrapF 0.8 (envTime env * 1.5)
    sleepers = [ seg (max 1 (6 / z)) (hsb 25 50 55) (p (-gauge * 0.7) z 0) (p (gauge * 0.7) z 0)
               | z <- takeWhile (< 80) (iterate (+ 0.8) (near + flow)) ]
    -- レール
    rails = [seg 3 (hsb 200 30 95) (p x near 0) (p x far 0) | x <- [-gauge / 2, gauge / 2]]
    -- 電柱（高さ 4 m）：てっぺんも消失点に向かう線に乗る
    poles = [ seg (max 1 (8 / z)) (hsb 45 20 85) (p 3.5 z 0) (p 3.5 z 4)
            | z <- takeWhile (< 120) (iterate (+ 8) (3 + wrapF 8 (flow * 10))) ]
    guide = [seg 1 (hsba 45 90 100 0.5) (p 3.5 3 4) (wVp w) | wGuide w]

-- 2 点透視：箱。手前の縦辺から、左右の消失点へ向かう線で奥行きをつくる
twoPoint :: Env -> World -> Picture
twoPoint env w = Pictures (map one boxes)
  where
    (vx, vy) = wVp w
    vl = (vx - spread / 2, vy)
    vr = (vx + spread / 2, vy)
    one (BoxDef bx by bh bl br hue) =
      let fx = envW env * bx
          fy = envH env * by
          hpx = envH env * bh
          b = (fx, fy)
          t = (fx, fy - hpx)
          lb = lerpPt b vl bl
          tl = lerpPt t vl bl
          rb = lerpPt b vr br
          tr = lerpPt t vr br
          tb = meet tl vr tr vl     -- 奥の辺 = 作図線の交点
          faces = [[b, lb, tl, t], [b, rb, tr, t], [t, tl, tb, tr]]
          cols = [hsba hue 55 70 0.85, hsba hue 55 55 0.85, hsba hue 40 90 0.85]
          line1 = hsba 45 90 100 0.35
      in Pictures $
           zipWith fillQuad cols faces
           ++ map (closedPath 2 (hsb 0 0 10)) faces
           ++ (if wGuide w
                 then [seg 1 line1 q vl | q <- [b, t, rb, tr]] ++ [seg 1 line1 q vr | q <- [b, t, lb, tl]]
                 else [])

lerpPt :: Point -> Point -> Float -> Point
lerpPt (ax, ay) (bx, by) t = (ax + (bx - ax) * t, ay + (by - ay) * t)

-- 直線 ab と直線 cd の交点（2D の外積版。詳しくは 02）
meet :: Point -> Point -> Point -> Point -> Point
meet (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let l0 = ay - by; l1 = bx - ax; l2 = ax * by - bx * ay
      m0 = cy - dy; m1 = dx - cx; m2 = cx * dy - dx * cy
      x = l1 * m2 - l2 * m1
      y = l2 * m0 - l0 * m2
      w = l0 * m1 - l1 * m0
  in if abs w < 1e-9 then ((ax + cx) / 2, (ay + cy) / 2) else (x / w, y / w)

dashed :: Color -> Point -> Point -> Float -> Picture
dashed c a@(x1, y1) b@(x2, y2) d =
  let n = floor (dist2 a b / d) :: Int
      fn = fromIntegral n
      at' i = (lerpF x1 x2 (i / fn), lerpF y1 y2 (i / fn))
  in Pictures [seg 1 c (at' (fromIntegral i)) (at' (fromIntegral i + 1)) | i <- [0, 2 .. n - 1]]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wVp = (vx, max 120 (vy - 15)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wVp = (vx, min (envH env - 120) (vy + 15)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wEyeH = max 0.3 (wEyeH w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wEyeH = min 6 (wEyeH w + 0.1) }
  EventKey (Char '1') Down _ _ -> w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> w { wMode = 2 }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> w { wGuide = not (wGuide w) }
    | c `elem` "rR" -> resetAll env w
  _ -> w
  where (vx, vy) = wVp w

-- ---------- やってみよう ----------
-- 1. 目の高さ h を 0.3 m（猫の目線）と 6 m（2 階から）にして、線路の見え方を比べよう
-- 2. focal を 300 と 1500 に変える。広角と望遠では、同じ Z の枕木の間隔はどう変わる？
-- 3. 2 点透視で spread を小さくすると箱がゆがむ（消失点が近すぎる = 超広角）。自然に見える間隔は？
-- 4. 電柱のように「高さのあるもの」を線路の両側に並べて、街並みを描いてみよう（groundPt w X Z hgt を使う）

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
