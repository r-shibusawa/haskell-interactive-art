-- ============================================
-- 43-02 ボロノイ図とドロネー三角形分割の双対
-- 同じ点群から作る 2 つの図は裏表の関係：
--   ボロノイの「面」 ↔ ドロネーの「頂点」（母点）、ボロノイの「頂点」 ↔ ドロネーの「三角形」（外接円の中心）
--   ボロノイの「辺」 ↔ ドロネーの「辺」（垂直二等分線どうし）
-- 操作：クリック = 点を追加 / ドラッグ = 点を動かす / V・D = 表示切替 / K = 外接円 / R = ランダム / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', partition)
import qualified Data.Vector.Unboxed as U

maxPts :: Int
maxPts = 60            -- ★ 点の上限（多いと重い）

startPts :: Int
startPts = 14          -- ★ R で作る点の数

data World = World
  { wPts  :: ![Point]  -- 母点（画面座標）
  , wV    :: !Bool
  , wD    :: !Bool
  , wCirc :: !Bool
  , wDrag :: !Int
  }

main :: IO ()
main = runSketch (sketch "43-02 Voronoi and Delaunay" (World [] True True False (-1)))
  { skBack  = hsb 250 40 10
  , skStart = \env -> do
      ps <- randomPts env startPts
      pure (World ps True True False (-1))
  , skDraw  = draw
  , skEvent = event
  }

fieldRect :: Env -> Panel
fieldRect env = Panel 20 95 (envW env - 40) (envH env - 130)

randomPts :: Env -> Int -> IO [Point]
randomPts env n = replicateM n $ do
  let a = fieldRect env
  x <- randomRIO (0, pw a - 120)
  y <- randomRIO (0, ph a - 120)
  pure (px a + 60 + x, py a + 60 + y)

draw :: Env -> World -> Picture
draw env w = view env $
  [ box (hsb 0 0 35) (px a) (py a) (pw a) (ph a) ]
  -- ボロノイの面（母点ごとに 1 つ）
  ++ concat [ [ Color (hsbA (180 + fromIntegral ((i * 23) `mod` 60)) 50 60 (if i == near then 55 else 14)) (Polygon c)
              , closed 2 (hsbA 180 70 100 80) c ]
            | wV w, (i, c) <- zip [0 :: Int ..] cells, length c >= 3 ]
  -- ドロネーの辺
  ++ [ if near >= 0 && (i == near || j == near)
         then seg 4 (hsb 10 90 100) (pt i) (pt j)
         else seg 1.5 (hsbA 10 80 100 60) (pt i) (pt j)
     | wD w, t <- tris, (i, j) <- [(ta t, tb t), (tb t, tc t), (tc t, ta t)] ]
  -- ボロノイの頂点 = 外接円の中心（三角形の数だけある）
  ++ [ dot (hsb 55 90 100) x y 6 | wV w && wD w, t <- tris, let (x, y) = center t
     , x > px a, x < px a + pw a, y > py a, y < py a + ph a ]
  -- 外接円（空円性：中に母点が入らない）
  ++ circles
  -- 母点
  ++ [ if i == near then dot (hsb 50 90 100) x y 12 else dot (hsb 0 0 100) x y 8 | (i, (x, y)) <- numbered ]
  -- 数える：面 = 母点、ボロノイ頂点 = 三角形
  ++ [ txt 20 32 18 (hsb 0 0 100) ("sites " ++ show (length pts) ++ " (= Voronoi faces)   triangles " ++ show (length tris) ++ " (= Voronoi vertices)")
     , txt 20 56 14 (hsb 0 0 100) ("V: Voronoi " ++ onOff (wV w) ++ "   D: Delaunay " ++ onOff (wD w) ++ "   K: circumcircles   R: random   C: clear")
     , txt 20 76 14 (hsb 0 0 60) "Hover a site: its face and the Delaunay edges crossing its walls light up. Both diagrams have the same edge count."
     , txt 700 56 14 (hsb 0 0 60) "Hover a yellow dot: the empty circle through 3 sites." ]
  where
    a = fieldRect env
    pts = wPts w
    numbered = zip [0 :: Int ..] pts
    pt i = case drop i pts of
      (p : _) -> p
      _ -> (0, 0)
    tris = if length pts >= 3 then delaunay pts else []
    cells = if length pts >= 2 then voronoiCells pts (px a) (py a) (px a + pw a) (py a + ph a) else []
    center t = (realToFrac (tcx t), realToFrac (tcy t)) :: Point
    m = mouseP env
    -- マウスに近い母点・近い外接円の中心
    lastOr d = foldl (\_ x -> x) d
    near0 = lastOr (-1) [i | (i, p) <- numbered, dist2 m p < 18]
    near = if wDrag w >= 0 then wDrag w else near0
    nearT = lastOr Nothing [Just t | t <- tris, dist2 m (center t) < 12]
    circles = case nearT of
      Just t -> let (x, y) = center t in
        [ circ 2 (hsbA 55 80 100 90) x y (2 * realToFrac (sqrt (tr2 t)))
        , Color (hsbA 55 80 100 35) (Polygon [pt (ta t), pt (tb t), pt (tc t)]) ]
      Nothing -> [ circ 1 (hsbA 55 80 100 25) x y (2 * realToFrac (sqrt (tr2 t)))
                 | wCirc w, t <- tris, let (x, y) = center t ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (press env w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let a = fieldRect env
        (mx, my) = mouseP env
        p = (clampF (px a + 2) (px a + pw a - 2) mx, clampF (py a + 2) (py a + ph a - 2) my)
    in pure w { wPts = [if i == wDrag w then p else q | (i, q) <- zip [0 ..] (wPts w)] }
  EventKey (Char k) Down _ _
    | k `elem` "vV" -> pure w { wV = not (wV w) }
    | k `elem` "dD" -> pure w { wD = not (wD w) }
    | k `elem` "kK" -> pure w { wCirc = not (wCirc w) }
    | k `elem` "rR" -> do
        ps <- randomPts env startPts
        pure w { wPts = ps, wDrag = -1 }
    | k `elem` "cC" -> pure w { wPts = [], wDrag = -1 }
  _ -> pure w

press :: Env -> World -> World
press env w
  | (i : _) <- near = w { wDrag = i }
  | mx > px a && mx < px a + pw a && my > py a && my < py a + ph a && length (wPts w) < maxPts =
      w { wPts = wPts w ++ [(mx, my)], wDrag = length (wPts w) }
  | otherwise = w
  where
    a = fieldRect env
    (mx, my) = mouseP env
    near = [i | (i, p) <- zip [0 ..] (wPts w), dist2 (mx, my) p < 14]

-- ---------- やってみよう ----------
-- 1. 点を 3 つだけにする。三角形 1 つ、ボロノイの頂点 1 つ。4 点目を外接円の中に入れると？
-- 2. 4 点を正方形に置くと外接円に 4 点が乗る（退化）。少しずらすと三角形の分け方が変わる瞬間を見よう
-- 3. 点をどれか 1 つだけ動かすと、ボロノイの面はいくつ変わる？ 隣の面だけ？
-- 4. 母点を格子状に並べるとボロノイは正方形、三角格子なら六角形（ハニカム）になる

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 太さ w の閉じた折れ線
closed :: Float -> Color -> [Point] -> Picture
closed w c ps = path w c (ps ++ take 1 ps)

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

-- 多角形を半平面 nx*x + ny*y <= c で切る（Sutherland–Hodgman）
clipHalf :: (Ord a, Fractional a) => [(a, a)] -> a -> a -> a -> [(a, a)]
clipHalf ps nx ny c = concat (zipWith piece ps (drop 1 ps ++ take 1 ps))
  where
    piece a@(ax, ay) (bx, by) =
      let fa = nx * ax + ny * ay - c
          fb = nx * bx + ny * by - c
          t = fa / (fa - fb)
      in [a | fa <= 0] ++ [(ax + (bx - ax) * t, ay + (by - ay) * t) | (fa < 0 && fb > 0) || (fa > 0 && fb < 0)]

-- 三角形（頂点の番号）と外接円（中心 tcx, tcy・半径² tr2）。
-- 超三角形の座標が大きいので、ここだけ桁の多い Double で計算する
data Tri = Tri { ta :: !Int, tb :: !Int, tc :: !Int, tcx :: !Double, tcy :: !Double, tr2 :: !Double }

makeTri :: U.Vector (Double, Double) -> Int -> Int -> Int -> Tri
makeTri ps a b c =
  let (ax, ay) = ps U.! a
      (bx, by) = ps U.! b
      (cx, cy) = ps U.! c
      d0 = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      d = if abs d0 < 1e-9 then 1e-9 else d0
      a2 = ax * ax + ay * ay
      b2 = bx * bx + by * by
      c2 = cx * cx + cy * cy
      ox = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
      oy = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d
  in Tri a b c ox oy ((ax - ox) * (ax - ox) + (ay - oy) * (ay - oy))

-- ドロネー三角形分割（Bowyer–Watson 法）：点を 1 つずつ入れ、外接円に含む三角形を壊して貼り直す
delaunay :: [Point] -> [Tri]
delaunay p0 = filter real (foldl' insert [makeTri ps n (n + 1) (n + 2)] [0 .. n - 1])
  where
    n = length p0
    big = 1e5
    ps = U.fromList ([(realToFrac x, realToFrac y) | (x, y) <- p0] ++ [(-big, -big), (big, -big), (0, big)])
    real t = ta t < n && tb t < n && tc t < n
    edgesOf t = [(ta t, tb t), (tb t, tc t), (tc t, ta t)]
    insert tris i =
      let (x, y) = ps U.! i
          inCircle t = (x - tcx t) * (x - tcx t) + (y - tcy t) * (y - tcy t) < tr2 t
          (bad, keep) = partition inCircle tris
          numbered = zip [0 :: Int ..] bad
          sameEdge a b (p, q) = (p == a && q == b) || (p == b && q == a)
          open = [ (a, b) | (k, t) <- numbered, (a, b) <- edgesOf t
                 , not (any (\(m, u) -> m /= k && any (sameEdge a b) (edgesOf u)) numbered) ]
      in keep ++ [makeTri ps a b i | (a, b) <- open]

-- ボロノイの面：枠を、各母点との垂直二等分線で切っていく（半平面の交わり）
voronoiCells :: [Point] -> Float -> Float -> Float -> Float -> [[Point]]
voronoiCells p0 x0 y0 x1 y1 = [map back (cell i p) | (i, p) <- numbered]
  where
    dbl :: Float -> Double
    dbl = realToFrac
    back (x, y) = (realToFrac x, realToFrac y) :: Point
    numbered = zip [0 :: Int ..] [(dbl x, dbl y) | (x, y) <- p0]
    frame = [(dbl x0, dbl y0), (dbl x1, dbl y0), (dbl x1, dbl y1), (dbl x0, dbl y1)]
    cell i (ax, ay) = foldl' cut frame [q | (j, q) <- numbered, j /= i]
      where
        cut poly (qx, qy) =
          let nx = qx - ax
              ny = qy - ay
          in clipHalf poly nx ny (nx * (ax + qx) / 2 + ny * (ay + qy) / 2)
