-- ============================================
-- 04 双対性：ボロノイとドロネー
-- ボロノイ図とドロネー三角形分割は「裏表」の関係（双対）
--   となり合うセルのサイト同士を結ぶ = ドロネーの辺。ボロノイの辺はその辺と直交する。三角形の外心 = ボロノイの頂点
-- V = ボロノイ / D = ドロネー / O = 外接円 / マウスを辺に近づける = 対応する辺 / ドラッグ = 動かす / クリック = 追加 / R = ランダム / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (find, findIndex, foldl', partition)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import GHC.Float (double2Float, float2Double)
import Text.Printf (printf)

data World = World
  { wSites :: ![Point]    -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wShowV :: !Bool
  , wShowD :: !Bool
  , wShowO :: !Bool
  , wCells :: ![[Point]]
  , wTris  :: ![(Int, Int, Int)]
  , wCirc  :: ![(Float, Float, Float)]    -- 外接円 (x, y, r2)。三角形と同じ順番
  , wEdges :: !(M.Map (Int, Int) [Int])   -- ドロネーの辺 (i, j) → その辺を持つ三角形の番号
  , wDirty :: !Bool
  , wDrag  :: !Int
  }

main :: IO ()
main = runSketch (sketch "22-04 Duality" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> randomSites 22 world0
  , skStep  = \_ _ w -> pure (if wDirty w then rebuild w else w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] True True False [] [] [] M.empty True (-1)

randomSites :: Int -> World -> IO World
randomSites n w = do
  ss <- replicateM n ((,) <$> randomRIO (80, width - 80) <*> randomRIO (140, height - 60))
  pure w { wSites = ss, wDirty = True }

rebuild :: World -> World
rebuild w = w { wCells = cells, wTris = tris, wCirc = circ, wEdges = em, wDirty = False }
  where
    ss = wSites w
    sv = U.fromList ss
    cells = map (\s -> voronoiCell ss s 0 90 width height) ss
    tris = delaunay ss
    circ = [ (double2Float x, double2Float y, double2Float r2)
           | (a, b, c) <- tris, let (x, y, r2) = circumcircle (toPD (sv U.! a)) (toPD (sv U.! b)) (toPD (sv U.! c)) ]
    em = foldl' addTri M.empty (zip [0 ..] tris)
    addTri m (ti, (a, b, c)) = foldl' (addEdge ti) m [(a, b), (b, c), (c, a)]
    addEdge ti m (a, b) = M.insertWith (flip (++)) (min a b, max a b) [ti] m

-- ドロネーの辺 (a, b) に対応するボロノイの辺：両側の三角形の外心を結ぶ。片側しかない（外周）なら外向きに半直線
dualEdge :: World -> U.Vector Point -> Int -> Int -> [Int] -> Maybe (Point, Point)
dualEdge w sv a b list = case map (nth (wCirc w)) list of
  [Just (x1, y1, _), Just (x2, y2, _)] -> Just ((x1, y1), (x2, y2))
  [Just (x1, y1, _)] ->
    let (ax, ay) = sv U.! a
        (bx, by) = sv U.! b
        mx = (ax + bx) / 2
        my = (ay + by) / 2
        nx0 = -(by - ay)
        ny0 = bx - ax
        l = max 1e-6 (sqrt (nx0 * nx0 + ny0 * ny0))
        third = case list of
          (ti : _) -> nth (wTris w) ti >>= \(p, q, r) -> find (\v -> v /= a && v /= b) [p, q, r]
          [] -> Nothing
        -- 3 番目の点と反対向き
        flipIt = case third of
          Just t -> let (tx, ty) = sv U.! t in (tx - mx) * nx0 + (ty - my) * ny0 > 0
          Nothing -> False
        (nx, ny) = if flipIt then (-nx0, -ny0) else (nx0, ny0)
    in Just ((x1, y1), (x1 + nx / l * 3000, y1 + ny / l * 3000))
  _ -> Nothing

segDist :: Point -> Point -> Point -> Float
segDist p@(px, py) (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
      t = if l2 > 0 then clampF 0 1 (((px - x1) * dx + (py - y1) * dy) / l2) else 0
  in dist2 p (x1 + t * dx, y1 + t * dy)

-- 一直線に近い 3 点の外心はとても遠くなる。遠すぎるものは描かない
near :: Float -> Float -> Bool
near x y = abs x < 1e5 && abs y < 1e5

drawAll :: Point -> World -> Picture
drawAll m w = Pictures
  [ view (voronoiPics ++ circlePics ++ delaunayPics ++ bestPics ++ concatMap siteDot ss
          ++ [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)])
  , txt 20 30 16 (gray 1) ("V: Voronoi " ++ onOff (wShowV w) ++ "  D: Delaunay " ++ onOff (wShowD w) ++ "  O: circles " ++ onOff (wShowO w)
      ++ "  Drag: move  Click: add  R: random  C: clear")
  , txt 20 52 14 (gray 0.8) ("Points " ++ show (length ss) ++ "  Edges " ++ show (M.size (wEdges w)) ++ "  Triangles " ++ show (length (wTris w))
      ++ "   Yellow dots = Voronoi vertices = circumcenters of Delaunay triangles.")
  , txt 20 74 14 (gray 0.8) ("Linking neighbor cells gives Delaunay. Move the mouse to a cyan edge." ++ info)
  ]
  where
    ss = wSites w
    sv = U.fromList ss
    voronoiPics
      | wShowV w =
          [ Pictures [Color (hsbA (wrapF 360 (fromIntegral i * 137.5)) 35 45 60) (Polygon c), Color (hsb 50 80 100) (outline 2 c)]
          | (i, c) <- zip [0 :: Int ..] (wCells w), length c > 2 ]
          ++ [dotAt (hsb 50 90 100) (x, y) 7 | (x, y, _) <- wCirc w, near x y]     -- ボロノイの頂点 = 外心
      | otherwise = []
    circlePics
      | wShowO w = [Color (hsbA 0 0 100 25) (at (x, y) (ring (sqrt r2) 1)) | (x, y, r2) <- wCirc w, r2 < 4e6]
      | otherwise = []
    delaunayPics
      | wShowD w = [Color (hsb 190 70 100) (thickLine 1.5 (sv U.! a) (sv U.! b)) | (a, b) <- M.keys (wEdges w)]
      | otherwise = []
    -- マウスにいちばん近いドロネーの辺と、その双対のボロノイの辺
    best = fst (M.foldlWithKey' pick (Nothing, 18) (wEdges w))
    pick (bv, bd) (a, b) list =
      let d = segDist m (sv U.! a) (sv U.! b)
      in if d < bd then (Just (a, b, list), d) else (bv, bd)
    bestPics = case best of
      Nothing -> []
      Just (a, b, list) ->
        [ Color (hsb 190 90 100) (thickLine 5 (sv U.! a) (sv U.! b)) ]
        ++ (case dualEdge w sv a b list of
              Just (e1@(x1, y1), e2@(x2, y2)) | near x1 y1 && near x2 y2 -> [Color (hsb 50 100 100) (thickLine 5 e1 e2)]
              _ -> [])
        ++ [ Color (hsbA 50 100 100 70) (at (x, y) (ring (sqrt r2) 2))
           | ti <- list, Just (x, y, r2) <- [nth (wCirc w) ti], near x y, r2 < 1e10 ]
    -- 直交の確認：2 本の向きの内積
    info = case best of
      Nothing -> ""
      Just (a, b, list) -> case dualEdge w sv a b list of
        Nothing -> ""
        Just ((x1, y1), (x2, y2)) ->
          let (ax, ay) = sv U.! a
              (bx, by) = sv U.! b
              ux = bx - ax
              uy = by - ay
              vx = float2Double x2 - float2Double x1
              vy = float2Double y2 - float2Double y1
              un = sqrt (float2Double (ux * ux + uy * uy))
              vn = sqrt (vx * vx + vy * vy)
              cosv = if un * vn > 0 then (float2Double ux * vx + float2Double uy * vy) / (un * vn) else 0
              deg = acos (max 0 (min 1 (abs cosv))) * 180 / pi :: Double
          in "  Angle between the two: " ++ printf "%.1f" deg ++ " deg" ++ (if length list == 1 then " (outer edge)" else "")
    siteDot s = [dotAt (hsb 0 0 10) s 14, dotAt (gray 1) s 10]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (press (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (x, y) = toP5 pos
    in pure w { wSites = setAt (wDrag w) (clampF 0 width x, clampF 90 height y) (wSites w), wDirty = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    'v' -> pure w { wShowV = not (wShowV w) }
    'd' -> pure w { wShowD = not (wShowD w) }
    'o' -> pure w { wShowO = not (wShowO w) }
    'r' -> do n <- randomRIO (10, 39)
              randomSites n w
    'c' -> pure w { wSites = [], wDirty = True }
    _ -> pure w
  _ -> pure w

press :: Point -> World -> World
press m@(_, my) w
  | my < 90 = w
  | otherwise = case findIndex (\s -> dist2 m s < 14) (wSites w) of
      Just i -> w { wDrag = i }
      Nothing -> w { wSites = wSites w ++ [m], wDirty = True }

-- ---------- やってみよう ----------
-- 1. サイトを 1 つドラッグして、ボロノイの頂点（黄）と外接円の中心がいつも一致していることを確かめよう
-- 2. セルの辺の数 = そのサイトから出ているドロネーの辺の数。いくつかのサイトで数えて確かめよう
-- 3. 外周のサイト（凸包の上）のセルは無限に広がっている（画面の端で切れているだけ）。どのサイトがそう？
-- 4. オイラーの公式：点 n、ドロネーの辺 e、三角形 t のあいだに n − e + t = 1 が成り立つ。表示の数で確かめよう

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- ==================================================
-- ボロノイ図の道具（ほかのスケッチにもそのままコピーして使える）
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく（O(n²)）
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = foldl' cut [(x0, y0), (x1, y0), (x1, y1), (x0, y1)] sites
  where
    -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
    cut poly b@(bx, by)
      | b == a || length poly <= 2 = poly
      | otherwise = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)

-- nx·x + ny·y ≦ c の側だけ残す（Sutherland–Hodgman の 1 本分）
clipHalfPlane :: [Point] -> Float -> Float -> Float -> [Point]
clipHalfPlane poly nx ny c = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    side (x, y) = nx * x + ny * y - c
    edge p@(px, py) q@(qx, qy) =
      let dp = side p
          dq = side q
          t = dp / (dp - dq)
      in [p | dp <= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | (dp < 0 && dq > 0) || (dp > 0 && dq < 0)]

-- 外接円 (x, y, r2) は桁が大きくなるので Double で計算する（r2 = 半径の 2 乗。3 点が一直線なら無限大）
type PD = (Double, Double)

circumcircle :: PD -> PD -> PD -> (Double, Double, Double)
circumcircle (ax, ay) (bx, by) (cx, cy)
  | abs d < 1e-9 = ((ax + bx + cx) / 3, (ay + by + cy) / 3, 1 / 0)
  | otherwise = (x, y, (ax - x) * (ax - x) + (ay - y) * (ay - y))
  where
    d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
    a2 = ax * ax + ay * ay
    b2 = bx * bx + by * by
    c2 = cx * cx + cy * cy
    x = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
    y = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d

-- ==================================================
-- ドロネーの道具（外接円の計算は桁が大きいので Double で行う）
--   delaunay pts … ドロネー三角形分割（Bowyer–Watson 法）。頂点番号の組を返す
-- ==================================================

toPD :: Point -> PD
toPD (x, y) = (float2Double x, float2Double y)

data Tri = Tri !Int !Int !Int !Double !Double !Double

-- Bowyer–Watson：点を 1 つずつ入れ、「外接円の中に新しい点がある三角形」を消して穴を新しい点とつなぐ
delaunay :: [Point] -> [(Int, Int, Int)]
delaunay pts
  | n < 3 = []
  | otherwise = [(a, b, c) | Tri a b c _ _ _ <- foldl' insert [mk n (n + 1) (n + 2)] [0 .. n - 1], a < n, b < n, c < n]
  where
    n = length pts
    ds = map toPD pts
    x0 = minimum (map fst ds)
    x1 = maximum (map fst ds)
    y0 = minimum (map snd ds)
    y1 = maximum (map snd ds)
    m = maximum [x1 - x0, y1 - y0, 1] * 20
    mx = (x0 + x1) / 2
    my = (y0 + y1) / 2
    -- 全部を囲む大きな三角形を後ろに足す
    pv = U.fromList (ds ++ [(mx - m, my - m), (mx + m, my - m), (mx, my + m)]) :: U.Vector PD
    mk i j k = let (x, y, r2) = circumcircle (pv U.! i) (pv U.! j) (pv U.! k) in Tri i j k x y r2
    insert tris i =
      let (px, py) = pv U.! i
          inside (Tri _ _ _ x y r2) = (px - x) * (px - x) + (py - y) * (py - y) < r2
          (bad, keep) = partition inside tris
          -- 2 回出た辺 = 穴の内側なので消す。1 回だけの辺が穴のふち
          toggle e (a, b) = let k = (min a b, max a b)
                            in if M.member k e then M.delete k e else M.insert k (a, b) e
          rim = foldl' toggle M.empty (concat [[(a, b), (b, c), (c, a)] | Tri a b c _ _ _ <- bad])
      in [mk a b i | (a, b) <- M.elems rim] ++ keep
