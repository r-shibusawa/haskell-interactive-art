-- ============================================
-- 05 ペンローズ・タイル（ひし形）
-- 太いひし形（72°）と細いひし形（36°）の 2 種類で、平面を「くり返しのない模様」で埋める（1974 年 ペンローズ）
--   作り方 = 分割（デフレーション）：三角形を黄金比 φ で切り分けるのをくり返す
--   円弧の模様 = マッチング規則。弧がとぎれないようにつなぐと、必ずくり返さない模様になる
-- ↑↓ = 分割の回数 / A = 円弧 / T = 三角形の切れ目 / ドラッグ = 移動 / Z・X = 拡大・縮小 / S = 保存
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- 三角形 (色, A, B, C)：色 0 = 細い、1 = 太い
data Tri = Tri !Int !Point !Point !Point

data World = World
  { wLevel    :: !Int      -- ★ 分割の回数（0〜8）
  , wShowArcs :: !Bool     -- ★ マッチング規則の円弧
  , wShowTri  :: !Bool     -- ★ 半分の三角形（ロビンソンの三角形）を見せる
  , wZoomF    :: !Float
  , wPanX     :: !Float
  , wPanY     :: !Float
  , wTris     :: [Tri]
  , wThin     :: Int
  , wThick    :: Int
  , wLayer    :: Picture
  , wArcsCut  :: Bool      -- 枚数が多すぎて円弧を省略しているか
  , wDragging :: !Bool
  , wMoved    :: !Point    -- ドラッグ中に動いた量
  , wLast     :: !Point
  }

world0 :: World
world0 = World 5 True False 1 0 0 [] 0 0 Blank False False (0, 0) (0, 0)

phi :: Float
phi = (1 + sqrt 5) / 2

-- ★ 色
thinCol, thickCol :: Color
thinCol = rgb255 250 190 80
thickCol = rgb255 70 120 200

main :: IO ()
main = runSketch (sketch "32-05 Penrose Tiling" world0)
  { skBack  = rgb255 18 20 28
  , skStart = \env -> pure (build env world0)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

build :: Env -> World -> World
build env w =
  let r = max (envW env) (envH env) * 0.75 * wZoomF w
      -- 最初は 10 枚の三角形でできた「車輪」
      start = map (wheel r) [0 .. 9 :: Int]
      tris = foldr (\_ t -> deflate t) start [1 .. wLevel w]
      count c = length [ () | Tri k _ _ _ <- tris, k == c ]
  in render env w { wTris = tris, wThin = (count 0 + 1) `div` 2, wThick = (count 1 + 1) `div` 2 }

wheel :: Float -> Int -> Tri
wheel r i =
  let pt k = (r * cos (fromIntegral k * pi / 10), r * sin (fromIntegral k * pi / 10))
      b = pt (2 * i - 1)
      c = pt (2 * i + 1)
  in if even i then Tri 0 (0, 0) c b else Tri 0 (0, 0) b c

-- ★ 分割（デフレーション）：細い三角形 → 2 枚、太い三角形 → 3 枚。切る位置は 1/φ
deflate :: [Tri] -> [Tri]
deflate = concatMap one
  where
    one (Tri 0 a b c) =
      let p = lerp2 a b (1 / phi)
      in [Tri 0 c p b, Tri 1 p c a]
    one (Tri _ a b c) =
      let q = lerp2 b a (1 / phi)
          r = lerp2 b c (1 / phi)
      in [Tri 1 r c a, Tri 1 q r b, Tri 0 r q a]

-- タイルを 1 枚の絵にしておく（レイヤーの代わり）
render :: Env -> World -> World
render env w =
  let cxw = wPanX w
      cyw = wPanY w
      hw = envW env / 2
      hh = envH env / 2
      visible (Tri _ (ax, ay) (bx, by) (cx, cy)) =
        let mx = (ax + bx + cx) / 3 + cxw
            my = (ay + by + cy) / 3 + cyw
        in abs mx <= hw + 80 && abs my <= hh + 80
      vis = filter visible (wTris w)
      -- ★ 図形の数の上限をこえるときは円弧を省略する
      cut = wShowArcs w && length vis * 3 > 7500
      arcsOn = wShowArcs w && not cut
      one t@(Tri c a b d) =
        let col = if c == 0 then thinCol else thickCol
            edge = dist2 b a
        in Pictures
          [ Color col (Polygon (insetTri 0.75 t))
          , if wShowTri w then Color (rgba255 255 255 255 90) (Line [b, d]) else Blank
          , if arcsOn
              then Pictures
                [ drawArc b a d (edge * (if c == 0 then 0.382 else 0.618)) (rgb255 230 60 60)
                , drawArc d a b (edge * 0.382) (rgb255 40 200 120) ]
              else Blank
          ]
  in w { wLayer = Translate cxw (negate cyw) (Scale 1 (-1) (Pictures (map one vis))), wArcsCut = cut }

drawAll :: Env -> World -> Picture
drawAll env w =
  let (dx, dy) = if wDragging w then wMoved w else (0, 0)
      thin = wThin w
      thick = wThick w
      ratio = if thin > 0 then nf4 (fromIntegral thick / fromIntegral thin) else "-"
  in Pictures
    [ Translate dx (negate dy) (wLayer w)
    , onP5 env (Color (rgba255 0 0 0 170) (box 10 10 700 104))
    , txt env 22 38 19 white ("Penrose tiling   " ++ show (wLevel w) ++ " subdivisions (Up/Down)")
    , txt env 22 64 15 (gray 0.82) ("Thick " ++ show thick ++ " / thin " ++ show thin ++ " -> ratio " ++ ratio ++ " (phi = 1.6180...)")
    , txt env 22 88 15 (gray 0.82)
        (if wArcsCut w then "Too many tiles: arcs are hidden here (Down or Z to see them)"
         else "Red arcs join red, green join green = matching rule (A: " ++ (if wShowArcs w then "ON" else "OFF") ++ ")")
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wDragging = True, wMoved = (0, 0), wLast = mouseOf env p }
  EventMotion p
    | wDragging w ->
        let m@(x, y) = mouseOf env p
            (lx, ly) = wLast w
            (ax, ay) = wMoved w
        in pure w { wMoved = (ax + x - lx, ay + y - ly), wLast = m }
    | otherwise -> pure w
  EventKey (MouseButton LeftButton) Up _ _
    | wDragging w ->
        let (mx, my) = wMoved w
            w1 = w { wDragging = False, wMoved = (0, 0) }
        in pure (if abs mx + abs my > 2 then render env w1 { wPanX = wPanX w + mx, wPanY = wPanY w + my } else w1)
  EventKey (SpecialKey KeyUp) Down _ _   -> pure (build env w { wLevel = min 8 (wLevel w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (build env w { wLevel = max 0 (wLevel w - 1) })
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> pure (render env w { wShowArcs = not (wShowArcs w) })
    | c == 't' || c == 'T' -> pure (render env w { wShowTri = not (wShowTri w) })
    | c == 'z' || c == 'Z' -> pure (build env w { wZoomF = min 8 (wZoomF w * phi), wPanX = wPanX w * phi, wPanY = wPanY w * phi })
    | c == 'x' || c == 'X' -> pure (build env w { wZoomF = max 0.3 (wZoomF w / phi), wPanX = wPanX w / phi, wPanY = wPanY w / phi })
    | c == 's' || c == 'S' -> saveSvg "penrose" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ↑ を押すたびに枚数が約 2.6 倍。太い ÷ 細い の比が φ に近づくのを確かめよう
-- 2. T で三角形の切れ目を表示。1 回の分割で三角形がどう切られるか、↑↓ を往復して観察しよう
-- 3. 同じ形の「星」や「太陽」が何度も出てくるのに、全体はくり返さない。ずらしても重ならない場所を探そう
-- 4. thinCol / thickCol を変えて、床や壁紙のデザインにしよう


-- ==================================================
-- ここから下は「描画の道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

mouseOf :: Env -> Point -> Point
mouseOf env (x, y) = (x + envW env / 2, envH env / 2 - y)

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

nf4 :: Float -> String
nf4 x = showFFloat (Just 4) x ""

lerp2 :: Point -> Point -> Float -> Point
lerp2 (ax, ay) (bx, by) t = (ax + (bx - ax) * t, ay + (by - ay) * t)

-- 辺 A−B と A−C を内側へ d だけ動かす（B−C は 2 枚の三角形の境目なので動かさない）。
-- 外側の辺に背景色の線を引く代わりになり、三角形 1 枚 = 図形 1 つで済む
insetTri :: Float -> Tri -> [Point]
insetTri d (Tri _ a@(ax, ay) b@(bx, by) c@(cx, cy)) =
  let unit (x, y) (u, v) =
        let dx = u - x
            dy = v - y
            l = max 1e-6 (sqrt (dx * dx + dy * dy))
        in (dx / l, dy / l)
      (abx, aby) = unit a b
      (acx, acy) = unit a c
      (bcx, bcy) = unit b c
      sinA = max 0.1 (abs (abx * acy - aby * acx))
      sinB = max 0.1 (abs (abx * bcy - aby * bcx))
      sinC = max 0.1 (abs (acx * bcy - acy * bcx))
  in [ (ax + (abx + acx) * d / sinA, ay + (aby + acy) * d / sinA)
     , (bx + bcx * d / sinB, by + bcy * d / sinB)
     , (cx - bcx * d / sinC, cy - bcy * d / sinC) ]

-- 頂点 V を中心に、V→P と V→Q の間に半径 r の円弧を描く
drawArc :: Point -> Point -> Point -> Float -> Color -> Picture
drawArc v@(vx, vy) (px, py) (qx, qy) r col =
  let a1 = atan2 (py - vy) (px - vx)
      a2 = atan2 (qy - vy) (qx - vx)
      d = wrapPi (a2 - a1)
      s = if d > 0 then a1 else a2
      e0 = if d > 0 then a2 else a1
      e = if e0 < s then e0 + tau else e0
      deg x = x * 180 / pi
      s' = wrapF 360 (deg s)
  in at v (Color col (ThickArc s' (s' + deg (e - s)) r (max 1.5 (r * 0.12))))

wrapPi :: Float -> Float
wrapPi d = wrapF tau (d + pi) - pi
