-- ============================================
-- 03 回転で作るタイル
-- 辺の真ん中のまわりに 180° 回したコピーを並べると、どんな三角形も・どんな四角形も平面を埋める
--   辺を「真ん中で点対称な曲線（C カーブ）」に変えても、まだぴったりかみ合う
-- 大きい点 = 頂点をドラッグ / 小さい点 = 辺の曲がりをドラッグ / 3・4 = 三角形・四角形 / C = 曲線 / L = 格子 / R = 戻す / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')

data Drag = DragV !Int | DragH !Int

data World = World
  { wSides   :: !Int              -- ★ 3 = 三角形、4 = 四角形
  , wCurves  :: !Bool             -- ★ 辺を C カーブにする
  , wP       :: [Point]           -- 頂点（p5.js と同じ画面の座標）
  , wH       :: [(Float, Float)]  -- 辺ごとの曲がり (s: 辺にそった位置, u: 横へのずれ)
  , wDrag    :: Maybe Drag
  , wLattice :: !Bool
  , wShape   :: [Point]           -- タイルの輪郭
  , wParts   :: [[Point]]         -- 輪郭を凸の部分に分けたもの
  }

world0 :: World
world0 = World 4 True [] [] Nothing False [] []

main :: IO ()
main = runSketch (sketch "32-03 Rotation Tiles" world0)
  { skBack  = backCol
  , skStart = \env -> pure (reset env world0)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

backCol :: Color
backCol = rgb255 22 24 32

-- ★ もとのタイルと 180° 回したタイルの色
colA, colB :: (Float, Float, Float)
colA = (255, 170, 80)
colB = (70, 150, 220)

reset :: Env -> World -> World
reset env w =
  let cx = envW env / 2
      cy = envH env / 2
      p | wSides w == 3 = [(cx - 90, cy + 60), (cx + 110, cy + 50), (cx - 10, cy - 100)]
        | otherwise = [(cx - 100, cy + 70), (cx + 90, cy + 80), (cx + 120, cy - 60), (cx - 60, cy - 90)]
      h = [ (0.28, if even i then 0.18 else -0.15) | i <- [0 .. length p - 1] ]
  in rebuild w { wP = p, wH = h }

rebuild :: World -> World
rebuild w = let s = outlinePts w in w { wShape = s, wParts = convexParts s }

pAt :: World -> Int -> Point
pAt w i = case drop (i `mod` max 1 (length (wP w))) (wP w) of
  (v : _) -> v
  [] -> (0, 0)

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      a@(ax, ay) = pAt w 0
      (bx, by) = pAt w 1
      (t1x, t1y) = vsub (pAt w 2) a
      (t2x, t2y) = if wSides w == 3 then vsub (pAt w 1) a else vsub (pAt w 3) (pAt w 1)
      det = t1x * t2y - t1y * t2x
      ok = abs det > 200 && isSimple (wP w)
      r = 14 :: Int
      offsets = [ (ox, oy, if even (i + j) then 1 else 0.82)
                | i <- [-r .. r], j <- [-r .. r]
                , let ox = fromIntegral i * t1x + fromIntegral j * t2x
                      oy = fromIntegral i * t1y + fromIntegral j * t2y
                , abs (ax + ox - wd / 2) <= wd * 0.75 && abs (ay + oy - h / 2) <= h * 0.8 ]
      -- 小さなタイルがたくさんあるときは、そのまま塗って軽くする
      light = length offsets * 2 * (length (wParts w) + 1) > 7000
      fillPic = if light then Polygon (wShape w) else Pictures (map Polygon (wParts w))
      shape = Pictures [fillPic, Color backCol (Line (closed (wShape w)))]
      tiles = concat
        [ [ Translate ox oy (Color (shade colA s) shape)
            -- 辺 AB の真ん中のまわりに 180° 回したタイル：点 p → (A + B) − p
          , Translate (ax + bx + ox) (ay + by + oy) (Scale (-1) (-1) (Color (shade colB s) shape)) ]
        | (ox, oy, s) <- offsets ]
      n = wSides w
      mids = [ at (mid2 (pAt w i) (pAt w (i + 1))) (Color (rgb255 255 80 80) (disc 3.5)) | i <- [0 .. n - 1] ]
      dots = concat
        [ dot 9 white (pAt w i) : (if wCurves w then [dot 5.5 (rgb255 255 230 120) (handlePos w i)] else [])
        | i <- [0 .. n - 1] ]
      lattice = if wLattice w && ok
        then Color (rgba255 255 255 255 160)
               (Pictures [thickLine 2 a (ax + t1x, ay + t1y), thickLine 2 a (ax + t2x, ay + t2y)])
        else Blank
      shapeName = if n == 3 then "triangle" else "quadrilateral"
      status | not ok = "Edges cross or collapse -> not a tile"
             | otherwise = (if n == 3 then "Angles: triangle 180 x 2 = 360" else "Angles: 4 corners of a quad meet = 360")
                           ++ "   C curves " ++ (if wCurves w then "ON" else "OFF") ++ "   L lattice"
  in Pictures
    [ onP5 env (Pictures
        [ if ok then Pictures tiles else Blank
        , Color white (thickPath 3 (closed (wShape w)))
        , Pictures mids, Pictures dots, lattice
        , Color (rgba255 0 0 0 170) (box 10 10 920 100) ])
    , txt env 22 38 19 white ("Any " ++ shapeName ++ " tiles: rotate 180 deg about the red edge midpoints")
    , txt env 22 64 15 (gray 0.78) "Orange = original, blue = rotated 180. Drag the vertices to change the shape"
    , txt env 22 90 15 (gray 0.78) status
    ]

-- 曲がりのハンドルの位置（辺 i の上）
handlePos :: World -> Int -> Point
handlePos w i =
  let a@(ax, ay) = pAt w i
      (dx, dy) = vsub (pAt w (i + 1)) a
      len = let l = sqrt (dx * dx + dy * dy) in if l == 0 then 1 else l
      nx = negate dy / len
      ny = dx / len
      (s, u) = case drop i (wH w) of
        (v : _) -> v
        [] -> (0.28, 0)
  in (ax + dx * s + nx * u * len, ay + dy * s + ny * u * len)

-- ★ タイルの輪郭：辺ごとに [始点, ハンドル, 点対称なハンドル, 終点] をなめらかにつなぐ
outlinePts :: World -> [Point]
outlinePts w = concatMap edge [0 .. wSides w - 1]
  where
    edge i =
      let a@(ax, ay) = pAt w i
          b@(bx, by) = pAt w (i + 1)
          h@(hx, hy) = handlePos w i
          h2 = (ax + bx - hx, ay + by - hy)   -- 辺の真ん中について点対称
      in if wCurves w then dropLast (smoothLine [a, h, h2, b] 8) else [a]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wDrag = pickDrag w (mouseOf env p) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion p -> case wDrag w of
    Nothing -> pure w
    Just d -> pure (rebuild (dragTo env w d (mouseOf env p)))
  EventKey (Char c) Down _ _
    | c == '3' || c == '4' -> pure (reset env w { wSides = fromEnum c - 48 })
    | c == 'c' || c == 'C' -> pure (rebuild w { wCurves = not (wCurves w) })
    | c == 'r' || c == 'R' -> pure (reset env w)
    | c == 'l' || c == 'L' -> pure w { wLattice = not (wLattice w) }
    | c == 's' || c == 'S' -> saveSvg "rotation_tile" env (drawAll env w) >> pure w
  _ -> pure w

pickDrag :: World -> Point -> Maybe Drag
pickDrag w m = fst (foldl' visit (Nothing, 22) [0 .. wSides w - 1])
  where
    visit acc i =
      let acc1 = closer acc (dist2 (pAt w i) m) (DragV i)
      in if wCurves w then closer acc1 (dist2 (handlePos w i) m) (DragH i) else acc1
    closer (best, bd) d x = if d < bd then (Just x, d) else (best, bd)

dragTo :: Env -> World -> Drag -> Point -> World
dragTo env w d (mx, my) = case d of
  DragV i -> w { wP = setAt i (clampF 20 (envW env - 20) mx, clampF 20 (envH env - 20) my) (wP w) }
  -- ハンドル：マウスの位置を「辺にそった位置 s」と「横へのずれ u」に直す
  DragH i ->
    let a@(ax, ay) = pAt w i
        (dx, dy) = vsub (pAt w (i + 1)) a
        len2 = let l = dx * dx + dy * dy in if l == 0 then 1 else l
        ux = mx - ax
        uy = my - ay
        s = clampF 0.05 0.45 ((ux * dx + uy * dy) / len2)
        u = clampF (-0.4) 0.4 ((ux * negate dy + uy * dx) / len2)
    in w { wH = setAt i (s, u) (wH w) }

-- ---------- やってみよう ----------
-- 1. 四角形をへこんだ形（矢じり形）にしても埋まる？ 交差させると？
-- 2. C を OFF にして、1 つの頂点に 4 つの角（四角形）/ 6 つの角（三角形）が集まっているのを確かめよう
-- 3. 曲がりを使って、くるっと回るとかみ合う「魚」や「鳥」をデザインしよう
-- 4. L で格子を表示。平行移動の向きが「対角線」になっているのはなぜ？


-- ==================================================
-- ここから下は「図形の道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

mouseOf :: Env -> Point -> Point
mouseOf env (x, y) = (x + envW env / 2, envH env / 2 - y)

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

shade :: (Float, Float, Float) -> Float -> Color
shade (r, g, b) s = makeColor (r * s / 255) (g * s / 255) (b * s / 255) 1

dot :: Float -> Color -> Point -> Picture
dot r c p = at p (Pictures [Color (gray 0.08) (disc (r + 1)), Color c (disc r)])

closed :: [a] -> [a]
closed p = p ++ take 1 p

dropLast :: [a] -> [a]
dropLast = reverse . drop 1 . reverse

setAt :: Int -> a -> [a] -> [a]
setAt i v l = take i l ++ [v] ++ drop (i + 1) l

vsub :: Point -> Point -> Point
vsub (a, b) (c, d) = (a - c, b - d)

mid2 :: Point -> Point -> Point
mid2 (a, b) (c, d) = ((a + c) / 2, (b + d) / 2)

-- 多角形の辺が交差していないか
isSimple :: [Point] -> Bool
isSimple pts =
  let n = length pts
      at' i = case drop (i `mod` n) pts of
        (v : _) -> v
        [] -> (0, 0)
      o (px, py) (qx, qy) (rx, ry) = signum ((qx - px) * (ry - py) - (qy - py) * (rx - px))
      crossing i j =
        let a = at' i
            b = at' (i + 1)
            c = at' j
            d = at' (j + 1)
        in o a b c * o a b d < 0 && o c d a * o c d b < 0
  in not (or [ crossing i j | i <- [0 .. n - 1], j <- [i + 2 .. n - 1], not (i == 0 && j == n - 1) ])

smoothLine :: [Point] -> Int -> [Point]
smoothLine pts seg = go (take 1 pts ++ pts ++ lastOf pts)
  where
    lastOf = take 1 . reverse
    go (p0 : p1 : p2 : p3 : rest) = spans p0 p1 p2 p3 ++ go (p1 : p2 : p3 : rest)
    go _ = lastOf pts
    spans (ax, ay) (bx, by) (cx, cy) (dx, dy) =
      [ (f ax bx cx dx t, f ay by cy dy t) | s <- [0 .. seg - 1], let t = fromIntegral s / fromIntegral seg ]
    f a b c d t = 0.5 * (2 * b + (c - a) * t + (2 * a - 5 * b + 4 * c - d) * t * t + (3 * b - a - 3 * c + d) * t * t * t)

-- gloss の Polygon は凸の形しか正しく塗れないので、
-- 三角形に切って（耳切り法）、くっつけられるものをくっつけて凸の部分に分ける
convexParts :: [Point] -> [[Point]]
convexParts pts0 = foldl' addTri [] (earClip [] (dropStraight (ccw pts0)))

ccw :: [Point] -> [Point]
ccw ps = if sum [ ax * by - bx * ay | ((ax, ay), (bx, by)) <- cyc ps ] < 0 then reverse ps else ps

cyc :: [a] -> [(a, a)]
cyc p = zip p (drop 1 p ++ take 1 p)

cross3p :: Point -> Point -> Point -> Float
cross3p (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - by) - (by - ay) * (cx - bx)

-- 一直線に並んだ点を取り除く
dropStraight :: [Point] -> [Point]
dropStraight ps =
  [ b | (a, b, c) <- zip3 (drop (n - 1) ps ++ ps) ps (drop 1 ps ++ ps), abs (cross3p a b c) > 1e-2 ]
  where n = length ps

earClip :: [[Point]] -> [Point] -> [[Point]]
earClip acc ps = case ps of
  (_ : _ : _ : _ : _) ->
    let n = length ps
        turned i = drop i ps ++ take i ps
        isEar (a : b : c : rest) = cross3p a b c > 0 && not (any (inTri a b c) rest)
        isEar _ = False
        ears = filter isEar (map turned [0 .. n - 1])
    in case (ears, ps) of
         ((a : b : c : rest) : _, _) -> earClip ([a, b, c] : acc) (a : c : rest)
         (_, a : b : c : rest) -> earClip ([a, b, c] : acc) (a : c : rest)   -- 線が交差しているときも止まらないように
         _ -> acc
  [_, _, _] -> ps : acc
  _ -> acc

inTri :: Point -> Point -> Point -> Point -> Bool
inTri a b c p = cross3p a b p >= 0 && cross3p b c p >= 0 && cross3p c a p >= 0

addTri :: [[Point]] -> [Point] -> [[Point]]
addTri polys tri = go polys
  where
    go [] = [tri]
    go (q : qs) = case mergeTri q tri of
      Just m -> m : qs
      Nothing -> q : go qs

mergeTri :: [Point] -> [Point] -> Maybe [Point]
mergeTri poly tri = case tri of
  [x, y, z] -> firstJust [try b a c | (a, b, c) <- [(x, y, z), (y, z, x), (z, x, y)]]
  _ -> Nothing
  where
    -- tri の辺 a→b が、poly の辺 b→a と同じなら、頂点 c をはさむ
    try a b c = firstJust
      [ if isConvex m then Just m else Nothing
      | (i, (p, q)) <- zip [1 ..] (cyc poly), p == a, q == b
      , let m = drop i poly ++ take i poly ++ [c] ]
    isConvex m = all (\(a, b, c) -> cross3p a b c >= -1e-3) (zip3 m (drop 1 m ++ take 1 m) (drop 2 m ++ take 2 m))
    firstJust xs = case [ v | Just v <- xs ] of
      (v : _) -> Just v
      [] -> Nothing
