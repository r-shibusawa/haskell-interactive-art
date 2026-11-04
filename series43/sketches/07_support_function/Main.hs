-- ============================================
-- 43-07 支持関数と極集合
-- 凸体 K の支持関数  h(θ) = max{ ⟨x, u_θ⟩ : x ∈ K }（方向 u_θ に K がどこまで届くか）
--   支持直線 ⟨x, u_θ⟩ = h(θ) は K にぴったり接する。h を全方向で知れば K は復元できる
--   極集合 K° = { y : ⟨x, y⟩ ≤ 1 for all x ∈ K }：方向 θ での K° の半径は 1 / h(θ)
-- 操作：マウス = 方向 θ / 頂点ドラッグ / クリック = 頂点追加 / 1〜3 = 形 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, foldl', group, sort, sortOn)
import Numeric (showFFloat)

unitPx :: Float
unitPx = 90             -- ★ 単位円の半径（px）

data World = World
  { wVerts :: ![Point]
  , wDrag  :: !Int
  }

main :: IO ()
main = runSketch (sketch "43-07 Support function" (World (preset 1) (-1)))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

preset :: Int -> [Point]
preset 2 = [(1.4 * cos t, 1.4 * sin t) | i <- [0 .. 3 :: Int], let t = fromIntegral i * pi / 2 + pi / 4]
preset 3 = [(1.7 * cos t, 1.7 * sin t) | i <- [0 .. 2 :: Int], let t = fromIntegral i * tau / 3]
preset _ = [(1.6, 0.3), (0.4, 1.5), (-1.3, 0.9), (-1.1, -1.2), (0.9, -1.4)]

panels :: Env -> (Panel, Panel, Panel)
panels env =
  let w = (envW env - 80) / 3
      h = envH env - 150
      mk i = Panel (20 + i * (w + 20)) 100 w h
  in (mk 0, mk 1, mk 2)

centerOf :: Panel -> Point
centerOf p = (px p + pw p / 2, py p + ph p / 2)

toS :: Panel -> Point -> Point
toS p (x, y) = let (cx, cy) = centerOf p in (cx + x * unitPx, cy - y * unitPx)

toM :: Panel -> Point -> Point
toM p (sx, sy) = let (cx, cy) = centerOf p in ((sx - cx) / unitPx, (cy - sy) / unitPx)

-- ★ 支持関数：方向 θ に対して max ⟨v, u⟩ とその頂点
support :: [Point] -> Float -> (Float, Int)
support vs th =
  let ux = cos th
      uy = sin th
      best (bv, bi) (i, (x, y)) =
        let v = x * ux + y * uy in if v > bv then (v, i) else (bv, bi)
      (h, k) = foldl' best (-1 / 0, 0) (zip [0 ..] vs)
  in (if isInfinite h then 0 else h, k)

-- 極集合：箱を半平面 ⟨v_i, y⟩ ≤ 1 で切る。枠からはみ出さないよう枠でも切る
polarSet :: Panel -> [Point] -> [Point]
polarSet p vs = foldl' (\poly (x, y) -> clipHalf poly x y 1) frame vs
  where
    rx = min 4 (pw p / 2 / unitPx)
    ry = min 4 (ph p / 2 / unitPx)
    frame = [(-rx, -ry), (rx, -ry), (rx, ry), (-rx, ry)]

drawFrame :: Panel -> String -> Picture
drawFrame p title =
  let (cx, cy) = centerOf p
  in Pictures
       [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
       , seg 1 (hsb 0 0 30) (px p, cy) (px p + pw p, cy)
       , seg 1 (hsb 0 0 30) (cx, py p) (cx, py p + ph p)
       , dashCirc (hsb 0 0 45) cx cy (2 * unitPx)
       , txt (px p + 10) (py p + 22) 14 (hsb 0 0 70) title ]

drawPoly :: Float -> Panel -> [Point] -> Float -> Float -> Float -> Picture
drawPoly hh p ps hue fa w
  | length ps < 2 = Blank
  | otherwise =
      let sp = map (toS p) ps
      in Pictures [Color (hsbA hue 70 100 fa) (fillPoly hh sp), closed w (hsbA hue 70 100 85) sp]

draw :: Env -> World -> Picture
draw env w = view env $
  -- 左：K と支持直線
  [ drawFrame a "K and support line <x, u> = h(t)"
  , drawPoly hh a vs 10 18 3 ]
  ++ [ let (sx, sy) = toS a v in dot (hsb 10 70 100) sx sy (if i == si then 14 else 10) | (i, v) <- zip [0 ..] vs ]
  ++ [ seg 3 (hsb 180 70 100) (ax, ay) (ax + ux * h * unitPx, ay - uy * h * unitPx) ]
  ++ [ seg 2 (hsb 55 90 100) c1 c2 | Just (c1, c2) <- [clipSeg a (toS a (fx + tx * 5, fy + ty * 5)) (toS a (fx - tx * 5, fy - ty * 5))] ]
  ++ [ txt (px a + 10) (py a + ph a - 12) 14 (hsb 0 0 100) ("h(t) = " ++ fmt 2 h ++ "   t = " ++ fmt 0 (th * 180 / pi) ++ " deg")
  -- 中：h(θ) の極座標プロット（全方向を計算して描く）
     , drawFrame b "support function h(t), polar plot (r = h)"
     , path 2.5 (hsb 55 80 100) [(bx + cos t * hh' * unitPx, by - sin t * hh' * unitPx) | (t, hh') <- allH] ]
  -- 1/h（= K° の半径）も薄く
  ++ (if originInside
        then [ seg 1.5 (hsbA 180 70 100 50) c1 c2
             | (p1, p2) <- zip inv (drop 1 inv), Just (c1, c2) <- [clipSeg b p1 p2] ]
        else [])
  ++ [ seg 3 (hsb 180 70 100) (bx, by) (bx + cos th * h * unitPx, by - sin th * h * unitPx)
     , dot (hsb 55 90 100) (bx + cos th * h * unitPx) (by - sin th * h * unitPx) 10
     , txt (px b + 10) (py b + ph b - 12) 14 (hsb 0 0 70) "yellow: h(t)   faint aqua: 1/h(t) (outline of K^o)"
  -- 右：極集合 K°
     , drawFrame c "K^o = { y : <x, y> <= 1 for all x in K }"
     , drawPoly hh c pol 180 18 3 ]
  -- 頂点 v_i ↔ K° の辺（極線）
  ++ [ seg (if i == si then 3 else 1) (hsbA 10 60 100 (if i == si then 90 else 25)) c1 c2
     | (i, (vx, vy)) <- zip [0 ..] vs
     , let d2 = vx * vx + vy * vy, d2 >= 1e-6
     , let qx = vx / d2, let qy = vy / d2, let tux = -vy / sqrt d2, let tuy = vx / sqrt d2
     , Just (c1, c2) <- [clipSeg c (toS c (qx + tux * 5, qy + tuy * 5)) (toS c (qx - tux * 5, qy - tuy * 5))] ]
  ++ (if originInside
        then let rr = 1 / h
                 q = toS c (cos th * rr, sin th * rr)
             in [ seg 3 (hsb 180 70 100) p1 p2 | Just (p1, p2) <- [clipSeg c (centerOf c) q] ]
                ++ [ uncurry (dot (hsb 55 90 100)) q 10
                   , txt (px c + 10) (py c + ph c - 12) 14 (hsb 0 0 100) ("radius along t = 1/h(t) = " ++ fmt 2 rr) ]
        else [ txt (px c + 10) (py c + ph c - 12) 14 (hsb 0 80 100) "! origin outside K: K^o is unbounded" ])
  ++ [ txt 20 32 18 (hsb 0 0 100) ("vertices " ++ show (length vs) ++ "   mouse = direction t   " ++ (if originInside then "OK origin inside K" else "! put the origin inside K") ++ "   1-3: shape   R: reset")
     , txt 20 58 14 (hsb 0 0 60) "Vertices of K become edges of K^o (red polar lines), edges of K become vertices of K^o."
     , txt 20 78 14 (hsb 0 0 60) "Where h_K(t) is large, K^o is thin: 1/h. Turn t and follow the yellow dots." ]
  where
    (a, b, c) = panels env
    hh = envH env
    vs = wVerts w
    (ax, ay) = centerOf a
    (bx, by) = centerOf b
    (cx, cy) = centerOf c
    (mx, my)= mouseP env
    th | mx > px c = atan2 (-(my - cy)) (mx - cx)
       | mx > px b = atan2 (-(my - by)) (mx - bx)
       | otherwise = atan2 (-(my - ay)) (mx - ax)
    (h, si) = support vs th
    ux = cos th
    uy = sin th
    originInside = length vs >= 3 && all (\k -> fst (support vs (fromIntegral k * pi / 36)) > 0.05) [0 .. 71 :: Int]
    -- 支持直線
    fx = ux * h
    fy = uy * h
    tx = -uy
    ty = ux
    allH = [(t, fst (support vs t)) | k <- [0 .. 180 :: Int], let t = fromIntegral k * pi / 90]
    inv = [(bx + cos t / v * unitPx, by - sin t / v * unitPx) | (t, v) <- allH]
    pol = polarSet c vs

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (a, _, _) = panels env
        (mx, my) = mouseP env
        p = toM a (clampF (px a) (px a + pw a) mx, clampF (py a) (py a + ph a) my)
        moved = setAt (wDrag w) p (wVerts w)
        h = hull moved
    in if length h == length moved
         then w { wVerts = h, wDrag = maybe (wDrag w) id (elemIndex p h) }
         else w { wVerts = moved }
  EventKey (Char k) Down _ _
    | k `elem` "123" -> w { wVerts = preset (fromEnum k - fromEnum '0'), wDrag = -1 }
    | k `elem` "rR" -> w { wVerts = preset 1, wDrag = -1 }
  _ -> w

press :: Env -> World -> World
press env w
  | (i : _) <- near = w { wDrag = i }
  | mx > px a && mx < px a + pw a && my > py a && my < py a + ph a && length (wVerts w) < 12 =
      w { wVerts = hull (wVerts w ++ [toM a (mx, my)]) }
  | otherwise = w
  where
    (a, _, _) = panels env
    (mx, my) = mouseP env
    near = [i | (i, v) <- zip [0 ..] (wVerts w), dist2 (mx, my) (toS a v) < 14]

-- ---------- やってみよう ----------
-- 1. 正方形（2）の h(θ) は何の形？ 円（頂点をたくさん置く）なら h は定数
-- 2. 頂点を 1 つ遠くへ引っ張る。h の極座標プロットのどの方向がふくらみ、K° はどの方向にへこむ？
-- 3. K が単位円の中にすっぽり入ると K° は単位円より大きい。K = 単位円なら K° = 単位円
-- 4. h(θ) は「凸」な関数（h(u+v) ≤ h(u)+h(v)）。2 つの凸体の h を足すとミンコフスキー和の h になる

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

-- 点線の円（直径 d）
dashCirc :: Color -> Float -> Float -> Float -> Picture
dashCirc c x y d =
  dashPath 1 c 4 6 [(x + d / 2 * cos t, y + d / 2 * sin t) | k <- [0 .. 72 :: Int], let t = fromIntegral k * tau / 72]

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

-- 線分を枠で切る（Liang–Barsky）
clipSeg :: Panel -> Point -> Point -> Maybe (Point, Point)
clipSeg p (x1, y1) (x2, y2) = go 0 1 tests
  where
    dx = x2 - x1
    dy = y2 - y1
    tests = [(-dx, x1 - px p), (dx, px p + pw p - x1), (-dy, y1 - py p), (dy, py p + ph p - y1)]
    go t0 t1 [] = Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy))
    go t0 t1 ((a, q) : rest)
      | a == 0 = if q < 0 then Nothing else go t0 t1 rest
      | a < 0 = if r > t1 then Nothing else go (max t0 r) t1 rest
      | otherwise = if r < t0 then Nothing else go t0 (min t1 r) rest
      where r = q / a

-- 多角形を半平面 nx*x + ny*y <= c で切る（Sutherland–Hodgman）
clipHalf :: (Ord a, Fractional a) => [(a, a)] -> a -> a -> a -> [(a, a)]
clipHalf ps nx ny c = concat (zipWith piece ps (drop 1 ps ++ take 1 ps))
  where
    piece a@(ax, ay) (bx, by) =
      let fa = nx * ax + ny * ay - c
          fb = nx * bx + ny * by - c
          t = fa / (fa - fb)
      in [a | fa <= 0] ++ [(ax + (bx - ax) * t, ay + (by - ay) * t) | (fa < 0 && fb > 0) || (fa > 0 && fb < 0)]

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
