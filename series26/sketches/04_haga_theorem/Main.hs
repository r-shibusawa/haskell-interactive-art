-- ============================================
-- 04 芳賀の定理（オリガミクス）
-- 上の辺の点 P に左下の角を重ねて折ると、折り返された下の辺が右の辺を切る点 Q ができる
--   上の辺の左から x の所に P を取ると、右上の角から Q までは 2x / (1 + x)。x = 1/2 なら 2/3（3 等分！）
-- ドラッグ／←→ = P を動かす / 1〜5 = x = 1/2, 1/3, 1/4, 1/5, 2/3 / F = 折る・広げる / T = 3 つの三角形
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

data World = World
  { wX      :: !Float      -- ★ P の位置（上の辺の左から。一辺 = 1）
  , wFolded :: !Bool
  , wTri    :: !Bool
  , wPhi    :: !Float      -- 折りのアニメーション
  , wDrag   :: !Bool
  }

presets :: [(Float, String, String)]   -- x、その表記、右上 → Q の分数
presets =
  [ (1 / 2, "1/2", "2/3"), (1 / 3, "1/3", "1/2"), (1 / 4, "1/4", "2/5")
  , (1 / 5, "1/5", "1/3"), (2 / 3, "2/3", "4/5") ]

main :: IO ()
main = runSketch (sketch "26-04 Haga's Theorem" (World 0.5 True True pi False))
  { skBack  = rgb255 34 32 40
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 折り線と、折った後の点を計算する（紙の座標：左下が原点、y は上向き）
data Haga = Haga { gCr :: !Ln, gP, gQ, gB2, gE, gG :: !Point }

haga :: Float -> Haga
haga x =
  let k = (0, 0)
      p = (x, 1)
      cr = bisector k p                 -- 角 K を P に重ねる折り線 = 垂直二等分線
      b2@(b2x, b2y) = refl (1, 0) cr    -- 右下の角の行き先
      -- 折り返された下の辺（P → B2）が右の辺 x = 1 と交わる点 Q
      t = (1 - x) / (b2x - x)
      q = (1, 1 + (b2y - 1) * t)
  in Haga cr p q b2 (atX cr 0) (atX cr 1)      -- 折り線が左の辺・右の辺と交わる点

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w =
  let (mx, _) = mouseXY env
      x' = if wDrag w then clampF 0.02 0.98 ((mx - px0) / paperSize env) else wX w
      target = if wFolded w then pi else 0
  in pure w { wX = x', wPhi = wPhi w + (target - wPhi w) * 0.1 }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let ps = paperSize env
      x = wX w
      g = haga x
      cr@(Ln _ (ux, uy)) = gCr g
      (_, qy) = gQ g
      dq = 1 - qy
      phi = wPhi w
      px = sp env
      (cx, cy) = px (0.5, 0.5)
      paper = Color (rgb255 246 240 226) (rect cx cy ps ps)
      -- 折り線
      seg = case clipSquare cr of
        Just (a, b) -> Color (rgb255 255 190 40) (thickLine 2 (px a) (px b))
        Nothing -> Blank
      -- 折り返す部分（左下の角の側）
      side = if sdist (0, 0) cr > 0 then 1 else -1
      flap = clipHalf [(0, 0), (1, 0), (1, 1), (0, 1)] cr side
      k = 1 - cos phi
      flapPts = [ px (fx + uy * d * k, fy - ux * d * k) | p@(fx, fy) <- flap, let d = sdist p cr ]
      flapPic = Pictures
        [ Color (if phi > pi / 2 then rgba255 226 84 72 235 else rgba255 255 250 235 235) (Polygon flapPts)
        , Color (rgb255 80 40 40) (poly flapPts) ]
      -- 3 つの直角三角形（折った後）
      tris
        | wTri w && phi > 3 =
            [ Color (rgba255 60 110 220 70) (Polygon [px (x, 1), px (1, 1), px (1, qy)])                 -- 右上
            , Color (rgba255 40 160 110 70) (Polygon [px (0, 1), px (x, 1), px (0, snd (gE g))]) ]        -- 左上
            ++ [ Color (rgba255 255 190 40 70) (Polygon [px (1, qy), px (gB2 g), px (1, snd (gG g))]) | snd (gG g) > 0 ]  -- 右下
        | otherwise = []
      -- Q の目盛り
      bar a = let (ax, ay) = px a in (ax + 14, ay)
      scaleBar =
        [ Color (rgb255 60 110 220) (thickLine 8 (bar (1, 1)) (bar (1, qy)))
        , Color (gray 0.55) (thickLine 8 (bar (1, qy)) (bar (1, 0)))
        , Color (gray 0.16) (at (px (x, 1)) (disc 6))
        , Color (gray 0.16) (at (px (1, qy)) (disc 6))
        , tp env (-4) (-14) (x, 1) 16 white "P"
        , tp env 26 5 (1, qy) 16 white "Q"
        , tp env (-30) (-14) (x / 2, 1) 16 white ("x = " ++ nf 3 x)
        , tp env 26 0 (1, (1 + qy) / 2) 16 (rgb255 110 160 255) (nf 3 dq)
        ]
      -- 説明と表
      py0 = 120
      tx = px0 + ps + 90
      boxW = envW env - tx - 20
      header = ("x", "corner -> Q", "fraction")
      row i (px', name, fr) =
        let y = py0 + 100 + fromIntegral (i + 1) * 32
            on = abs (px' - x) < 0.002
            c = if on then rgb255 255 190 40 else gray 0.59
        in [ txt env tx y 18 c (show (i + 1) ++ "  " ++ name)
           , txt env (tx + 120) y 18 c (nf 4 (2 * px' / (1 + px')))
           , txt env (tx + 260) y 18 c fr ]
      (h1, h2, h3) = header
      table =
        [ txt env tx (py0 + 100) 15 (gray 0.78) ("   " ++ h1)
        , txt env (tx + 120) (py0 + 100) 15 (gray 0.78) h2
        , txt env (tx + 260) (py0 + 100) 15 (gray 0.78) h3 ]
        ++ concat (zipWith row [0 :: Int ..] presets)
      y = py0 + 100 + 6 * 32
      p1 = 1 - x
      p2 = dq
      p3 = sqrt (p1 * p1 + p2 * p2)
      panel =
        [ txt env tx (py0 + 10) 24 (rgb255 255 190 40) "Corner -> Q = 2x / (1 + x)"
        , txt env tx (py0 + 44) 18 white ("measured " ++ nf 5 dq ++ "   formula " ++ nf 5 (2 * x / (1 + x)))
        ] ++ table
        ++ textBox env tx (y - 6) boxW 15 (gray 0.75) "With x = 1/(2n - 1), 2x/(1 + x) = 1/n. One fold makes any n-th part."
        ++ [ txt env tx (y + 80) 17 (rgb255 110 160 255) ("Blue triangle: " ++ nf 3 p1 ++ " : " ++ nf 3 p2 ++ " : " ++ nf 3 p3) ]
        ++ [ txt env tx (y + 108) 17 (rgb255 255 190 40) "= a 3 : 4 : 5 right triangle! (x6 gives 3, 4, 5)" | abs (x - 0.5) < 0.002 ]
        ++ textBox env tx (y + 124) boxW 14 (gray 0.6) "The 3 triangles (blue, green, yellow) are similar. Their side ratios give 2x/(1+x)."
      hud =
        [ txt env 20 40 18 white ("Drag / Left,Right: move P   1-5: pick x   F: " ++ (if wFolded w then "unfold" else "fold") ++ "   T: triangles " ++ (if wTri w then "ON" else "off"))
        , txt env 20 68 15 (gray 0.67) "Kazuo Haga's \"origamics\": one fold divides an edge in many useful ratios."
        ]
  in Pictures ([paper, seg, flapPic] ++ tris ++ scaleBar ++ panel ++ hud)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        ps = paperSize env
    in if abs (my - 120) < 40 && mx > px0 - 20 && mx < px0 + ps + 20 then w { wDrag = True } else w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wX = max 0.02 (wX w - 0.01) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wX = min 0.98 (wX w + 0.01) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' ->
        case drop (fromEnum c - fromEnum '1') presets of
          (v, _, _) : _ -> w { wX = v }
          [] -> w
    | toLower c == 'f' -> w { wFolded = not (wFolded w) }
    | toLower c == 't' -> w { wTri = not (wTri w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. x = 1/2 で 2/3、そこから 1/3 も手に入る。x をいくつにすれば 1/4、1/5 がつくれる？ 表で確かめよう
-- 2. 青・緑・黄の三角形が相似であることを使って、DQ = 2x/(1+x) を自分で導いてみよう
-- 3. 折り線が左の辺と交わる高さは (1 + x²)/2。緑の三角形の辺の長さを x で表してみよう
-- 4. 本物の紙で x = 1/2 を折って、右の辺の 2/3 の点ができるか確かめよう（芳賀の第 1 定理）

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（紙の座標：一辺 1）と画面の道具
-- ==================================================

data Ln = Ln !Point !Point

atX :: Ln -> Float -> Point
atX (Ln (px, py) (ux, uy)) xv = let t = (xv - px) / ux in (xv, py + uy * t)

nrm :: Float -> Float -> Point
nrm x y = let l0 = sqrt (x * x + y * y)
              l = if l0 == 0 then 1 else l0
          in (x / l, y / l)

bisector :: Point -> Point -> Ln
bisector (ax, ay) (bx, by) = Ln ((ax + bx) / 2, (ay + by) / 2) (nrm (-(by - ay)) (bx - ax))

sdist :: Point -> Ln -> Float
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

refl :: Point -> Ln -> Point
refl q@(qx, qy) ln@(Ln _ (ux, uy)) = let s = sdist q ln in (qx + 2 * s * uy, qy - 2 * s * ux)

edges :: [a] -> [(a, a)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

clipHalf :: [Point] -> Ln -> Float -> [Point]
clipHalf pts ln side = concatMap one (edges pts)
  where
    one (p@(px, py), q@(qx, qy)) =
      let s1 = sdist p ln * side
          s2 = sdist q ln * side
          t = s1 / (s1 - s2)
      in [p | s1 >= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | s1 * s2 < 0]

clipSquare :: Ln -> Maybe (Point, Point)
clipSquare ln =
  let sq = [(0, 0), (1, 0), (1, 1), (0, 1)]
      hits = [ (px + (qx - px) * t, py + (qy - py) * t)
             | (p@(px, py), q@(qx, qy)) <- edges sq
             , let s1 = sdist p ln
             , let s2 = sdist q ln
             , s1 * s2 <= 0 && s1 /= s2
             , let t = s1 / (s1 - s2) ]
  in case hits of
       a : b : _ -> Just (a, b)
       _ -> Nothing

px0 :: Float
px0 = 70

paperSize :: Env -> Float
paperSize env = min (envW env * 0.48) (envH env - 180)

-- 紙の座標 → gloss の座標（紙の左上は画面の (70, 120)）
sp :: Env -> Point -> Point
sp env (u, v) = let ps = paperSize env in sc env (px0 + u * ps, 120 + ps - v * ps)

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

tp :: Env -> Float -> Float -> Point -> Float -> Color -> String -> Picture
tp env dx dy p size c s = let (a, b) = sp env p in label (a + dx) (b - dy) size c s

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

textBox :: Env -> Float -> Float -> Float -> Float -> Color -> String -> [Picture]
textBox env x y w size c s =
  [ txt env x (y + size + fromIntegral i * size * 1.3) size c l | (i, l) <- zip [0 :: Int ..] (wrapW w size s) ]

wrapW :: Float -> Float -> String -> [String]
wrapW w size = go "" . words
  where
    maxC = floor (w / (size * 0.72)) :: Int
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= maxC = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs
