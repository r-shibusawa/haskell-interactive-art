-- ============================================
-- 43-01 多角形の極双対
-- 凸多角形 K の各「辺」の直線 ⟨n, x⟩ = d を、単位円に関する極で「頂点」n/d に写す
--   K° = { y : ⟨x, y⟩ ≤ 1 for all x ∈ K }　頂点の数 ↔ 辺の数 が入れ替わり、(K°)° = K
--   原点が K の内側にないと d ≤ 0 の辺が出て、双対は「壊れる」（有界でなくなる）
-- 操作：頂点ドラッグ / 原点（白）ドラッグ / クリック = 頂点を追加 / D = 双対の双対を重ねる / 1〜3 = 形 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, foldl', group, sort, sortOn)

unitPx :: Float
unitPx = 110             -- ★ 単位円の半径（px）

data World = World
  { wVerts  :: ![Point]  -- K の頂点（数学座標、単位円の半径 = 1）
  , wOrigin :: !Point    -- 極の中心（これを動かすと「壊れる」のが見える）
  , wDD     :: !Bool
  , wDragI  :: !Int
  , wDragO  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "43-01 Polar dual polygons" (preset 1 (World [] (0, 0) False (-1) False)))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

-- 1〜3 の形：正三角形・正方形・正六角形
preset :: Int -> World -> World
preset k w = w { wVerts = [(rr * cos t, rr * sin t) | i <- [0 .. n - 1], let t = fromIntegral i * tau / fromIntegral n + 0.3]
               , wOrigin = (0, 0) }
  where
    (n, rr) = case k of
      1 -> (3, 1.9)
      2 -> (4, 1.6)
      _ -> (6 :: Int, 1.5 :: Float)

-- 枠と中心
panels :: Env -> (Panel, Panel)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 150
  in (Panel 20 100 w h, Panel (40 + w) 100 w h)

toS :: Panel -> Point -> Point
toS p (x, y) = (px p + pw p / 2 + x * unitPx, py p + ph p / 2 - y * unitPx)

toM :: Panel -> Point -> Point
toM p (sx, sy) = ((sx - px p - pw p / 2) / unitPx, (py p + ph p / 2 - sy) / unitPx)

-- ★ 極双対：辺 (v_i, v_{i+1}) の直線 ⟨n, x − o⟩ = d → 双対の頂点 o + n / d
polarDual :: [Point] -> Point -> ([Point], Bool)
polarDual ps (ox, oy) =
  let one (ax, ay) (bx, by) =
        let ex = bx - ax
            ey = by - ay
            len0 = sqrt (ex * ex + ey * ey)
            len = if len0 == 0 then 1e-9 else len0
            nx = ey / len                       -- 外向き法線（頂点は反時計回り）
            ny = -ex / len
            d0 = nx * (ax - ox) + ny * (ay - oy) -- 原点から辺までの符号つき距離
            d | abs d0 < 0.02 = if d0 < 0 then -0.02 else 0.02
              | otherwise = d0
        in ((ox + nx / d, oy + ny / d), d0 <= 0.02)
      res = zipWith one ps (drop 1 ps ++ take 1 ps)
  in (map fst res, any snd res)

-- 多角形：辺（hiEdge だけ太く）と塗り
drawPoly :: Float -> Panel -> [Point] -> Float -> Float -> Float -> Int -> Picture
drawPoly hh p ps hue fillA w hiEdge = Pictures $
  [ if i == hiEdge then seg 7 (hsbA hue 70 100 100) a b else seg w (hsbA hue 70 100 80) a b
  | (i, a, b) <- zip3 [0 ..] sp (drop 1 sp ++ take 1 sp) ]
  ++ [ Color (hsbA hue 70 100 fillA) (fillPoly hh sp) | fillA > 0 ]
  where sp = map (toS p) ps

drawFrame :: Panel -> Point -> String -> Picture
drawFrame p o title =
  let (ox, oy) = toS p o
  in Pictures
       [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
       , dashCirc (hsb 0 0 45) ox oy (2 * unitPx)
       , txt (px p + 10) (py p + 22) 14 (hsb 0 0 70) title ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawFrame l o "K (coral) and the unit circle"
  , drawFrame r o "K^o (aqua): edge of K -> vertex of K^o, vertex -> edge"
  -- 左：K。頂点 i に対応する K° の辺は (w_{i-1}, w_i)
  , drawPoly hh l verts 10 18 3 hiE ]
  ++ [ let (sx, sy) = toS l v in dot (hsb 10 70 100) sx sy (if i == hiV then 16 else 11) | (i, v) <- zip [0 ..] verts ]
  -- 右：K°。K の頂点 hiV に対応する K° の辺 (hiV−1, hiV) を太く
  ++ [ drawPoly hh r dualP 180 18 3 (if hiV >= 0 then (hiV - 1 + n) `mod` n else -1) ]
  ++ [ let (sx, sy) = toS r v in dot (hsb 180 70 100) sx sy (if i == hiE then 16 else 11) | (i, v) <- zip [0 ..] dualP ]
  -- 選んだ頂点 ↔ 辺：左の頂点 hiV の極線を右に、右の頂点 hiE の極線を左に（点線）
  ++ [ polarLine r o v 10 | (i, v) <- zip [0 ..] verts, i == hiV ]
  ++ [ polarLine l o v 180 | (i, v) <- zip [0 ..] dualP, i == hiE ]
  -- 双対の双対
  ++ [ closedDash [toS l v | v <- dd] | wDD w ]
  -- 原点
  ++ [ circ 2 (hsb 0 0 100) o1x o1y 14, circ 2 (hsb 0 0 100) o2x o2y 8
     , seg 2 (hsb 0 0 100) (o1x - 10, o1y) (o1x + 10, o1y)
     , seg 2 (hsb 0 0 100) (o1x, o1y - 10) (o1x, o1y + 10)
     , txt 20 30 18 (hsb 0 0 100) ("K: " ++ show n ++ " verts, " ++ show n ++ " edges  ->  K^o: " ++ show n ++ " verts, " ++ show n ++ " edges    "
                                   ++ (if broken then "! origin outside K (or on an edge): the dual breaks" else "OK origin inside K"))
     , txt 20 54 14 (hsb 0 0 100) ("D: (K^o)^o " ++ (if wDD w then "ON (yellow dashes sit on K)" else "off") ++ "   1-3: shape   R: reset")
     , txt 20 74 14 (hsb 0 0 60) "Drag a vertex or the white origin. As K nears the origin (small d), K^o spreads far away."
     , txt 20 92 14 (hsb 0 0 60) "Hover a vertex to light up its partner edge: vertices <-> edges." ]
  where
    (l, r) = panels env
    hh = envH env
    verts = wVerts w
    o = wOrigin w
    n = max 1 (length verts)
    (dualP, broken) = polarDual verts o
    (dd, _) = polarDual dualP o
    m = mouseP env
    hits p ps = [i | (i, v) <- zip [0 :: Int ..] ps, dist2 m (toS p v) < 16]
    lastOr d xs = foldl (\_ x -> x) d xs
    hiV = if wDragI w >= 0 then wDragI w else lastOr (-1) (hits l verts)
    hiE = lastOr (-1) (hits r dualP)
    (o1x, o1y) = toS l o
    (o2x, o2y) = toS r o
    closedDash ps = dashPath 3 (hsbA 60 70 100 80) 8 6 (ps ++ take 1 ps)

-- 点 p の極線 ⟨p − o, x − o⟩ = 1 を描く
polarLine :: Panel -> Point -> Point -> Float -> Picture
polarLine p (ox, oy) (vx0, vy0) hue
  | d2 < 1e-6 = Blank
  | otherwise = case clipSeg p (toS p (fx + tx * 6, fy + ty * 6)) (toS p (fx - tx * 6, fy - ty * 6)) of
      Just (a, b) -> dashPath 2 (hsbA hue 60 100 60) 6 6 [a, b]
      Nothing -> Blank
  where
    vx = vx0 - ox
    vy = vy0 - oy
    d2 = vx * vx + vy * vy
    fx = ox + vx / d2            -- 足
    fy = oy + vy / d2
    tx = -vy / sqrt d2
    ty = vx / sqrt d2

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragI = -1, wDragO = False }
  EventMotion _ -> dragged env w
  EventKey (Char k) Down _ _
    | k `elem` "dD" -> w { wDD = not (wDD w) }
    | k `elem` "rR" -> preset 1 w
    | k `elem` "123" -> preset (fromEnum k - fromEnum '0') w
  _ -> w

press :: Env -> World -> World
press env w
  | dist2 m (toS l (wOrigin w)) < 14 = w { wDragO = True }
  | (i : _) <- near = w { wDragI = i }
  | inside l m && length (wVerts w) < 12 = w { wVerts = hull (wVerts w ++ [toM l m]) }
  | otherwise = w
  where
    (l, _) = panels env
    m = mouseP env
    near = [i | (i, v) <- zip [0 ..] (wVerts w), dist2 m (toS l v) < 14]

-- ドラッグ：凸のままなら凸包の順に並べ直す。凹むときは並びをそのままにする
dragged :: Env -> World -> World
dragged env w
  | wDragO w = w { wOrigin = p }
  | wDragI w >= 0 =
      let moved = setAt (wDragI w) p (wVerts w)
          h = hull moved
      in if length h == length moved
           then w { wVerts = h, wDragI = maybe (wDragI w) id (elemIndex p h) }
           else w { wVerts = moved }
  | otherwise = w
  where
    (l, _) = panels env
    (mx, my) = mouseP env
    p = toM l (clampF (px l) (px l + pw l) mx, clampF (py l) (py l + ph l) my)

-- ---------- やってみよう ----------
-- 1. 正方形（2）の双対はひし形（正方形を 45° 回したもの）。正三角形の双対は？ 正六角形は？
-- 2. 頂点を単位円の上に置くと、双対の辺はその頂点に接する。中に入れると双対は外へ、外に出すと内へ
-- 3. 原点を K の辺の近くへ持っていく。d → 0 のとき双対の頂点はどこへ飛ぶ？
-- 4. D を押して (K°)° = K を確認。壊れたとき（原点が外）はどうなる？

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

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 点線の円（直径 d）
dashCirc :: Color -> Float -> Float -> Float -> Picture
dashCirc c x y d =
  dashPath 1 c 4 6 [(x + d / 2 * cos t, y + d / 2 * sin t) | k <- [0 .. 72 :: Int], let t = fromIntegral k * tau / 72]

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

inside :: Panel -> Point -> Bool
inside p (x, y) = x >= px p && x <= px p + pw p && y >= py p && y <= py p + ph p

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
