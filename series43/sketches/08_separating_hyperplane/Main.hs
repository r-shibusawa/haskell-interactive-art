-- ============================================
-- 43-08 分離超平面とファルカスの補題
-- 交わらない 2 つの凸集合には、必ず間を通る直線（分離超平面）がある
--   作り方：いちばん近い 2 点を結び、その垂直二等分線を引く。交わっていると分離できない
--   ファルカス（F）：ベクトル b が a1, a2 の「錐」に入る か、錐と b を分ける直線がある か、どちらか一方だけ
-- 操作：頂点ドラッグ / 図形の中をドラッグ = 平行移動 / F = ファルカスのモード / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, find, foldl', group, sort, sortOn)
import Numeric (showFFloat)

-- ドラッグの対象：どの図形の、どの頂点か（−1 は図形全体）
data Drag = NoDrag | DragSet !Bool !Int

data World = World
  { wA     :: ![Point]    -- 図形 A（画面座標）
  , wB     :: ![Point]    -- 図形 B
  , wF     :: !Bool       -- ファルカスのモード
  , wDrag  :: !Drag
  , wVecs  :: ![Point]    -- ファルカス用 a1, a2, b（原点からのベクトル、px）
  , wDragV :: !Int
  , wLast  :: !Point      -- 前のマウス位置（movedX, movedY の代わり）
  }

-- ★ ファルカスの 3 本のベクトル
startVecs :: [Point]
startVecs = [(160, -40), (60, 150), (180, 120)]

main :: IO ()
main = runSketch (sketch "43-08 Separating hyperplane" (World [] [] False NoDrag startVecs (-1) (0, 0)))
  { skBack  = hsb 250 40 10
  , skStart = \env -> pure (resetSets env (World [] [] False NoDrag startVecs (-1) (0, 0)))
  , skDraw  = draw
  , skEvent = event
  }

resetSets :: Env -> World -> World
resetSets env w =
  let cx = envW env / 2
      cy = envH env / 2 + 30
  in w { wA = [(cx - 330, cy - 120), (cx - 160, cy - 170), (cx - 110, cy + 10), (cx - 260, cy + 90)]
       , wB = [(cx + 80, cy - 90), (cx + 300, cy - 60), (cx + 330, cy + 130), (cx + 120, cy + 150)] }

-- 点が多角形の中にあるか（交差数）
inPoly :: Point -> [Point] -> Bool
inPoly (x, y) ps = odd (length (filter crosses (zip ps (drop 1 ps ++ take 1 ps))))
  where
    crosses ((ax, ay), (bx, by)) =
      (ay > y) /= (by > y) && x < (bx - ax) * (y - ay) / (by - ay) + ax

-- 点 p と線分 ab の最近点
closestOnSeg :: Point -> Point -> Point -> Point
closestOnSeg (x, y) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l0 = dx * dx + dy * dy
      l2 = if l0 == 0 then 1e-9 else l0
      t = clampF 0 1 (((x - ax) * dx + (y - ay) * dy) / l2)
  in (ax + dx * t, ay + dy * t)

edgesOf :: [Point] -> [(Point, Point)]
edgesOf ps = zip ps (drop 1 ps ++ take 1 ps)

-- ★ 2 つの凸多角形の最近点対（頂点 vs 辺 を両方向で総当たり）
closestPair :: [Point] -> [Point] -> (Float, Point, Point)
closestPair ps qs = foldl' best (1 / 0, (0, 0), (0, 0)) cands
  where
    cands = [(p, closestOnSeg p a b) | p <- ps, (a, b) <- edgesOf qs]
         ++ [(closestOnSeg q a b, q) | q <- qs, (a, b) <- edgesOf ps]
    best acc@(d0, _, _) (p, q) = let d = dist2 p q in if d < d0 then (d, p, q) else acc

segsCross :: (Point, Point) -> (Point, Point) -> Bool
segsCross (a, b) (c, d) = cr a b c * cr a b d < 0 && cr c d a * cr c d b < 0
  where cr (px0, py0) (qx, qy) (rx, ry) = (qx - px0) * (ry - py0) - (qy - py0) * (rx - px0)

intersects :: [Point] -> [Point] -> Bool
intersects ps qs =
  any (`inPoly` qs) ps || any (`inPoly` ps) qs
    || or [segsCross e f | e <- edgesOf ps, f <- edgesOf qs]

drawPoly :: Float -> [Point] -> Float -> Picture
drawPoly hh ps hue = Pictures $
  [ Color (hsbA hue 70 100 20) (fillPoly hh ps), closed 3 (hsb hue 70 100) ps ]
  ++ [ dot (hsb hue 70 100) x y 10 | (x, y) <- ps ]

draw :: Env -> World -> Picture
draw env w
  | wF w = drawFarkas env w
  | otherwise = view env $
      [ drawPoly hh (wA w) 10, drawPoly hh (wB w) 180 ]
      ++ (if intersects (wA w) (wB w) then hit else apart)
      ++ [ txt 20 58 14 (hsb 0 0 60) "Convexity is what makes the bisector of the closest pair separate. A dent can make it fail."
         , txt 20 78 14 (hsb 0 0 60) "This theorem underlies LP duality, SVMs and Farkas' lemma." ]
  where
    hh = envH env
    hit = [ txt 20 32 18 (hsb 0 80 100) "X The shapes meet -> no line can separate them    F: Farkas' lemma    R: reset" ]
    apart =
      let (d, p@(ax, ay), q@(bx, by)) = closestPair (wA w) (wB w)
          mx = (ax + bx) / 2
          my = (ay + by) / 2
          nx = bx - ax
          ny = by - ay
          l0 = sqrt (nx * nx + ny * ny)
          l = if l0 == 0 then 1 else l0
          tx = -ny / l * 2000
          ty = nx / l * 2000
          ox = nx / l * 2000
          oy = ny / l * 2000
      in [ seg 2 (hsb 55 90 100) p q
         , dot (hsb 55 90 100) ax ay 10, dot (hsb 55 90 100) bx by 10
         -- 垂直二等分線 = 分離直線
         , seg 2 (hsb 0 0 100) (mx - tx, my - ty) (mx + tx, my + ty)
         -- 半平面を薄く塗る（A 側）
         , Color (hsbA 10 70 100 6) (Polygon [(mx - tx, my - ty), (mx + tx, my + ty), (mx + tx - ox, my + ty - oy), (mx - tx - ox, my - ty - oy)])
         , txt (mx + 14) (my - 30) 14 (hsb 0 0 100) ("closest pair distance " ++ fmt 1 d ++ " px")
         , txt 20 32 18 (hsb 0 0 100) "OK Disjoint -> a separating line exists (bisector of the closest pair)    F: Farkas    R: reset" ]

-- ★ ファルカスの補題（2D）：b ∈ cone(a1, a2) か、y·a1 ≥ 0, y·a2 ≥ 0, y·b < 0 の y があるか、ちょうど一方
drawFarkas :: Env -> World -> Picture
drawFarkas env w = view env $
  -- 錐を塗る
  [ Color (hsbA 10 70 100 15) (Polygon [(ox, oy), (ox + a1x * 20, oy + a1y * 20), (ox + a2x * 20, oy + a2y * 20)])
  , seg 1 (hsb 0 0 30) (0, oy) (envW env, oy)
  , seg 1 (hsb 0 0 30) (ox, 0) (ox, envH env)
  , vec a1 10 "a1", vec a2 10 "a2", vec bv 180 "b" ]
  ++ result
  ++ [ txt 20 58 14 (hsb 0 0 60) "Drag the arrow tips. 'A solution exists (b = A lam, lam >= 0)' and 'a certificate y of no solution' hold exactly one at a time."
     , txt 20 78 14 (hsb 0 0 60) "This is the heart of LP duality (proof of infeasibility = unbounded dual direction).    F: back" ]
  where
    ox = envW env / 2
    oy = envH env / 2 + 40
    (a1, a2, bv) = case wVecs w of
      [p, q, r] -> (p, q, r)
      _ -> ((1, 0), (0, 1), (1, 1))
    (a1x, a1y) = a1
    (a2x, a2y) = a2
    (bx, by) = bv
    vec (vx, vy) hue name = Pictures
      [ seg 4 (hsb hue 80 100) (ox, oy) (ox + vx, oy + vy)
      , dot (hsb hue 80 100) (ox + vx) (oy + vy) 14
      , txt (ox + vx + 10) (oy + vy - 10) 16 (hsb 0 0 100) name ]
    -- b = λ1 a1 + λ2 a2 を解く
    det = a1x * a2y - a1y * a2x
    dv = if det == 0 then 1e-9 else det
    l1 = (bx * a2y - by * a2x) / dv
    l2 = (a1x * by - a1y * bx) / dv
    inCone = abs det > 1e-6 && l1 >= 0 && l2 >= 0
    result
      | inCone =
          -- 分解を平行四辺形で描く
          [ dashPath 2 (hsbA 55 90 100 70) 6 6 [(ox + a1x * l1, oy + a1y * l1), (ox + bx, oy + by)]
          , dashPath 2 (hsbA 55 90 100 70) 6 6 [(ox + a2x * l2, oy + a2y * l2), (ox + bx, oy + by)]
          , txt 20 32 18 (hsb 0 0 100) ("(1) b = " ++ fmt 2 l1 ++ " a1 + " ++ fmt 2 l2 ++ " a2 (coefficients >= 0) -> b is in the cone. No separating line") ]
      | otherwise = case find good cands of
          Just (yx, yy) ->
            let l0 = sqrt (yx * yx + yy * yy)
                l = if l0 == 0 then 1 else l0
                tx = -yy / l * 2000
                ty = yx / l * 2000
                ex = ox + yx / l * 120
                ey = oy + yy / l * 120
            in [ seg 2 (hsb 0 0 100) (ox - tx, oy - ty) (ox + tx, oy + ty)
               , seg 3 (hsb 55 90 100) (ox, oy) (ex, ey)
               , dot (hsb 55 90 100) ex ey 12
               , txt (ex + 10) (ey - 8) 16 (hsb 0 0 100) "y"
               , txt 20 32 18 (hsb 0 0 100) "(2) y.a1 >= 0, y.a2 >= 0 but y.b < 0: the white line splits cone and b -> b is outside" ]
          Nothing -> [ txt 20 32 18 (hsb 0 0 100) "Degenerate (a1 and a2 are parallel)" ]
    -- y の候補：a1, a2 に直交するベクトル（錐の縁の法線）のうち条件を満たすもの
    cands = [(-a1y, a1x), (a1y, -a1x), (-a2y, a2x), (a2y, -a2x)]
    good (vx, vy) = vx * a1x + vy * a1y >= -1e-6 && vx * a2x + vy * a2y >= -1e-6 && vx * bx + vy * by < 0

event :: Env -> Event -> World -> IO World
event env ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = NoDrag, wDragV = -1 }
  EventMotion _ -> dragged env w0 w
  EventKey (Char k) Down _ _
    | k `elem` "fF" -> w { wF = not (wF w) }
    | k `elem` "rR" -> (resetSets env w) { wVecs = startVecs, wDrag = NoDrag }
  _ -> w
  where w = w0 { wLast = mouseP env }

press :: Env -> World -> World
press env w
  | wF w =
      let (ox, oy) = (envW env / 2, envH env / 2 + 40)
          hits = [i | (i, (vx, vy)) <- zip [0 ..] (wVecs w), dist2 m (ox + vx, oy + vy) < 16]
      in w { wDragV = foldl (\_ x -> x) (wDragV w) hits }
  | (d : _) <- vertexHits = w { wDrag = d }
  | inPoly m (wA w) = w { wDrag = DragSet True (-1) }
  | inPoly m (wB w) = w { wDrag = DragSet False (-1) }
  | otherwise = w
  where
    m = mouseP env
    hitsOf flag ps = [DragSet flag i | (i, p) <- zip [0 ..] ps, dist2 m p < 14]
    vertexHits = hitsOf True (wA w) ++ hitsOf False (wB w)

dragged :: Env -> World -> World -> World
dragged env w0 w
  | wF w =
      if wDragV w >= 0
        then w { wVecs = setAt (wDragV w) (clampF (-600) 600 (mx - envW env / 2), clampF (-360) 360 (my - envH env / 2 - 40)) (wVecs w) }
        else w
  | otherwise = case wDrag w of
      NoDrag -> w
      DragSet flag i
        | i >= 0 ->
            let p = (clampF 10 (envW env - 10) mx, clampF 90 (envH env - 10) my)
                moved = setAt i p (setOf flag)
                h = hull moved
            in if length h == length moved
                 then put flag h (DragSet flag (maybe i id (elemIndex p h)))
                 else put flag moved (wDrag w)
        | otherwise ->
            let (lx, ly) = wLast w0
            in put flag [(x + mx - lx, y + my - ly) | (x, y) <- setOf flag] (wDrag w)
  where
    (mx, my) = mouseP env
    setOf flag = if flag then wA w else wB w
    put flag ps d = if flag then w { wA = ps, wDrag = d } else w { wB = ps, wDrag = d }

-- ---------- やってみよう ----------
-- 1. 2 つの図形をぎりぎりまで近づける。分離直線は最後まで存在する？ 触れた瞬間は？
-- 2. 片方を凹の形にしたくなるが、頂点は凸包に直される。凹だと分離できない例を頭の中で描こう
-- 3. F で b を錐の縁ぎりぎりに置く。λ の片方が 0 になるとき y はどうなる？（相補性）
-- 4. a3 を増やして 3 本の錐にする。y の候補はどう探せばいい？（錐の各縁の法線）

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

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 太さ w の閉じた折れ線
closed :: Float -> Color -> [Point] -> Picture
closed w c ps = path w c (ps ++ take 1 ps)

-- 点線の折れ線（on だけ描いて off だけ空ける）
dashPath :: Float -> Color -> Float -> Float -> [Point] -> Picture
dashPath w c on off pts = Pictures (walk on True pts)
  where
    walk left drawing (a : b : rest)
      | l <= left = [seg w c a b | drawing] ++ walk (left - l) drawing (b : rest)
      | otherwise = [seg w c a m | drawing] ++ walk (if drawing then off else on) (not drawing) (m : b : rest)
      where
        l = dist2 a b
        t = left / l
        m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
    walk _ _ _ = []

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""

-- 凸包（Andrew の方法）
hull :: [Point] -> [Point]
hull ps
  | length s < 3 = s
  | otherwise = dropLast (chain s) ++ dropLast (chain (reverse s))
  where
    s = sort ps
    dropLast xs = take (length xs - 1) xs

-- 凸包の片側：左に曲がる点だけを残す
chain :: [Point] -> [Point]
chain = reverse . foldl' add []
  where
    add (b : a : rest) p | turn a b p <= 0 = add (a : rest) p
    add st p = p : st
    turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

-- リストの i 番目を入れかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 多角形を塗る。gloss の Polygon は凸の形しか塗れないので、
-- 凹んだ形や自己交差した形は横に細く切って台形の集まりにする（0〜hh の高さだけ）
fillPoly :: Float -> [Point] -> Picture
fillPoly hh ps
  | length ps < 3 = Blank
  | isConvex ps = Polygon ps
  | otherwise = Pictures (concat (zipWith strip ys (drop 1 ys)))
  where
    edges = [(a, b) | (a, b) <- zip ps (drop 1 ps ++ take 1 ps), snd a /= snd b]
    ys = map head' (group (sort (map (clampF 0 hh . snd) ps ++ [hh * fromIntegral k / 100 | k <- [0 .. 100 :: Int]])))
    head' = foldr const 0
    xAt y ((ax, ay), (bx, by)) = ax + (bx - ax) * (y - ay) / (by - ay)
    strip y0 y1
      | y1 - y0 < 0.01 = []
      | otherwise =
          let mid = (y0 + y1) / 2
              hits = sortOn (\(x, _, _, _) -> x)
                [ (xAt mid e, xAt y0 e, xAt y1 e, if snd a < snd b then 1 else -1 :: Int)
                | e@(a, b) <- edges, (snd a <= mid) /= (snd b <= mid) ]
          in spans y0 y1 0 hits
    -- 巻き数が 0 でないところを塗る（Canvas の nonzero と同じ）
    spans y0 y1 w ((_, l0, l1, d) : rest@((_, r0, r1, _) : _))
      | w + d /= 0 = Polygon [(l0, y0), (r0, y0), (r1, y1), (l1, y1)] : spans y0 y1 (w + d) rest
      | otherwise = spans y0 y1 (w + d) rest
    spans _ _ _ _ = []

-- 凸多角形か（同じ向きに曲がり続け、1 周だけする）
isConvex :: [Point] -> Bool
isConvex ps = (all (>= 0) crosses || all (<= 0) crosses) && abs (sum turns) < 7
  where
    ps1 = drop 1 ps ++ take 1 ps
    ps2 = drop 1 ps1 ++ take 1 ps1
    vec (ax, ay) (bx, by) = (bx - ax, by - ay)
    v1 = zipWith vec ps ps1
    v2 = zipWith vec ps1 ps2
    crosses = zipWith (\(a, b) (c, d) -> a * d - b * c) v1 v2
    turns = zipWith (\(a, b) (c, d) -> atan2 (a * d - b * c) (a * c + b * d)) v1 v2
