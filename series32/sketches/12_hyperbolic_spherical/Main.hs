-- ============================================
-- 12 双曲平面と球面のタイリング
-- 正 p 角形を 1 つの頂点に q 枚ずつ集める {p, q} タイリング
--   (p − 2)(q − 2) < 4 → 角が余るので丸まって「球面」（正多面体 5 種類）
--   (p − 2)(q − 2) = 4 → ぴったり「平面」（00 の 3 種類）
--   (p − 2)(q − 2) > 4 → 角が足りないので波打って「双曲平面」（ポアンカレの円板に無限に描ける。エッシャー『円の極限』）
-- ←→ = p / ↑↓ = q / ドラッグ = 回す・動かす / R = 中心に戻す / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Numeric (showFFloat)

type D2 = (Double, Double)
data D3 = D3 !Double !Double !Double

data Geom = Sphere | Flat | Hyper deriving (Eq)

-- 平面・双曲の多角形（頂点, 中心, 深さ）
data Poly = Poly { pV :: [D2], pC :: !D2, pD :: !Int }

-- 球面の面（中心, 頂点）
data Face = Face { fC :: !D3, fV :: [D3] }

data World = World
  { wP, wQ  :: !Int         -- ★ 正 p 角形を q 枚ずつ
  , wGeom   :: !Geom
  , wPolys  :: [Poly]
  , wFaces  :: [Face]
  , wLayer  :: Picture      -- 平面・双曲の絵（変わったときだけ作る）
  , wRotX   :: !Float
  , wRotY   :: !Float
  , wLast   :: !Point       -- 1 つ前のマウス位置
  }

maxPolys :: Int
maxPolys = 2500             -- ★ 双曲のときに描く最大枚数

world0 :: World
world0 = World 7 3 Hyper [] [] Blank (-0.4) 0 (0, 0)

main :: IO ()
main = runSketch (sketch "32-12 Hyperbolic & Spherical" world0)
  { skBack  = rgb255 16 18 26
  , skStart = \env -> pure (build env world0)
  , skStep  = \env _ w -> pure (if wGeom w == Sphere && not (envDown env) then w { wRotY = wRotY w + 0.006 } else w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

build :: Env -> World -> World
build env w =
  let k = (wP w - 2) * (wQ w - 2)
      g | k < 4 = Sphere
        | k == 4 = Flat
        | otherwise = Hyper
      w1 = w { wGeom = g }
  in if g == Sphere then buildSphere w1 else buildPlane env Nothing w1

drawAll :: Env -> World -> Picture
drawAll env w =
  let p = wP w
      q = wQ w
      deg = 180 * fromIntegral (p - 2) / fromIntegral p :: Float
      name = case (p, q) of
        (3, 3) -> "tetrahedron"
        (4, 3) -> "cube"
        (3, 4) -> "octahedron"
        (5, 3) -> "dodecahedron"
        _ -> "icosahedron"
      title = case wGeom w of
        Sphere -> "Sphere: " ++ name ++ " (" ++ show (length (wFaces w)) ++ " faces)"
        Flat -> "Plane: exactly 360 deg"
        Hyper -> "Hyperbolic plane (Poincare disk)"
      compareMsg = case wGeom w of
        Sphere -> "< 360 -> the rest curls up into a ball"
        Flat -> "= 360"
        Hyper -> "> 360 -> does not fit, so tiles look smaller toward the rim"
      dragMsg = case wGeom w of
        Sphere -> "rotate. All faces are the same bulged regular polygon"
        Hyper -> "hyperbolic translation. In the disk every polygon is really the same size"
        Flat -> "move"
  in Pictures
    [ if wGeom w == Sphere then Translate 0 (-40) (drawSphere w) else wLayer w
    , onP5 env (Color (rgba255 0 0 0 180) (box 0 0 (envW env) 100))
    , txt env 20 36 20 white ("{" ++ show p ++ ", " ++ show q ++ "}  regular " ++ show p ++ "-gons, " ++ show q ++ " at each vertex -> " ++ title)
    , txt env 20 62 15 (gray 0.75) ("Flat " ++ show p ++ "-gon angle " ++ nf1 deg ++ " x " ++ show q ++ " = " ++ nf1 (deg * fromIntegral q) ++ " deg  " ++ compareMsg)
    , txt env 20 86 15 (gray 0.75) ("Left/Right p, Up/Down q. Drag: " ++ dragMsg)
    ]

-- ---------- 球面 ----------
buildSphere :: World -> World
buildSphere w =
  let p = wP w
      fp = fromIntegral p :: Double
      fq = fromIntegral (wQ w) :: Double
      rho = acos (1 / (tan (pi / fp) * tan (pi / fq)))   -- 面の中心から頂点までの角度
      first = Face (D3 0 0 1)
        [ D3 (sin rho * cos a) (sin rho * sin a) (cos rho)
        | i <- [0 .. p - 1], let a = 2 * pi * fromIntegral i / fp ]
      go done queue seen = case Q.viewl queue of
        Q.EmptyL -> reverse done
        f Q.:< rest
          | length done + Q.length queue >= 30 -> reverse done ++ (f : foldr (:) [] rest)
          | otherwise ->
              let (seen', new) = foldl' visit (seen, []) (cycPairs (fV f))
                  visit (s, out) (a, b) =
                    -- ★ 辺を含む「原点を通る平面」で鏡に映す
                    let nrm = unitD (crossD a b)
                        g = Face (mirror3 (fC f) nrm) (map (`mirror3` nrm) (fV f))
                        k = key3 (fC g)
                    in if S.member k s then (s, out) else (S.insert k s, g : out)
              in go (f : done) (foldl' (Q.|>) rest (reverse new)) seen'
  in w { wFaces = go [] (Q.singleton first) (S.singleton (key3 (fC first))) }

drawSphere :: World -> Picture
drawSphere w =
  let r = 230 :: Float
      camD = 519.6                 -- p5.js の WEBGL の標準カメラの距離（高さ 600 のとき）
      rx = wRotX w
      ry = wRotY w
      view (D3 x y z) = rotX rx (rotY ry (V3 (realToFrac x * r) (realToFrac y * r) (realToFrac z * r)))
      depth (V3 _ _ z) = z
      flat = project camD
      faces = zip [0 :: Int ..] (wFaces w)
      -- 面をふくらんだ三角形に分ける（中心から辺まで 2 段）。手前を向いた三角形だけ描く
      faceTris (fi, f) =
        let D3 _ _ cz = fC f
            h = wrapF 360 (fromIntegral fi * 137.5)
            shade = realToFrac (0.6 + 0.4 * max 0 (cz * 0.5 + 0.5)) :: Float
            col = hsb h 55 (95 * shade)
        in [ (zc, Color col (Polygon (map flat [a, b, c])))
           | (va, vb) <- cycPairs (fV f)
           , s <- [0 .. 5 :: Int]
           , let e1 = unitD (lerpD va vb (fromIntegral s / 6))
                 e2 = unitD (lerpD va vb (fromIntegral (s + 1) / 6))
                 m1 = unitD (lerpD (fC f) e1 0.5)
                 m2 = unitD (lerpD (fC f) e2 0.5)
           , [ta, tb, tc] <- [[fC f, m1, m2], [m1, e1, e2], [m1, e2, m2]]
           , let a = view ta
                 b = view tb
                 c = view tc
                 zc = (depth a + depth b + depth c) / 3
           , zc > 0 ]
      -- 辺（大円の弧）。カメラから見えるのは z > R² / カメラの距離 の部分だけ。面の後から描く
      horizon = r * r / camD
      edges = [ Color (gray 0.08) (thickLine 3 (flat u) (flat v))
              | f <- wFaces w
              , (va, vb) <- cycPairs (fV f)
              , s <- [0 .. 7 :: Int]
              , let u = view (scaleD 1.004 (unitD (lerpD va vb (fromIntegral s / 8))))
                    v = view (scaleD 1.004 (unitD (lerpD va vb (fromIntegral (s + 1) / 8))))
              , (depth u + depth v) / 2 > horizon ]
      sorted = depthSort fst (concatMap faceTris faces)
  in Scale 1 (-1) (Pictures (map snd sorted ++ edges))

-- ---------- 平面・双曲（ポアンカレ円板） ----------
buildPlane :: Env -> Maybe Poly -> World -> World
buildPlane env start w =
  let p = wP w
      fp = fromIntegral p :: Double
      fq = fromIntegral (wQ w) :: Double
      g = wGeom w
      d0 = if g == Hyper then sqrt (cos (pi / fp + pi / fq) / cos (pi / fp - pi / fq)) else 0.08
      first = case start of
        Just s -> s
        Nothing -> Poly [ (d0 * cos a, d0 * sin a) | i <- [0 .. p - 1], let a = 2 * pi * fromIntegral i / fp + 0.3 ] (0, 0) 0
      go :: Int -> [Poly] -> Q.Seq Poly -> S.Set (Int, Int) -> [Poly]
      go !cnt done queue seen = case Q.viewl queue of
        Q.EmptyL -> reverse done
        f Q.:< rest
          | cnt >= maxPolys -> reverse done ++ (f : foldr (:) [] rest)
          | g == Flat && hyp (pC f) > 1.1 -> go cnt (f : done) rest seen
          | otherwise ->
              let (seen', new, cnt') = foldl' (visit f) (seen, [], cnt) (cycPairs (pV f))
              in go cnt' (f : done) (foldl' (Q.|>) rest (reverse new)) seen'
      visit f acc@(s, out, c) (a, b) =
        let qc = reflectGeo g (pC f) a b
            k = key2 qc
        in if S.member k s then acc else
           let qv = map (\v -> reflectGeo g v a b) (pV f)
               tiny = case qv of
                 (v0 : _) -> g == Hyper && hyp (sub2 v0 qc) < 0.004   -- 小さすぎる所は省略
                 [] -> True
           in if tiny then acc
              else (S.insert k s, Poly qv qc (pD f + 1) : out, c + 1)
      polys = go 1 [] (Q.singleton first) (S.singleton (key2 (pC first)))
  in renderPlane env w { wPolys = polys }

radiusOf :: Env -> Geom -> Float
radiusOf env g
  | g == Flat = max (envW env) (envH env) * 0.6
  | otherwise = min (envW env) (envH env - 140) * 0.45

renderPlane :: Env -> World -> World
renderPlane env w =
  let g = wGeom w
      r = radiusOf env g
      rd = realToFrac r :: Double
      toPx (x, y) = (realToFrac x * r, realToFrac y * r)
      one poly =
        let sizePx = case pV poly of
              (v0 : _) -> hyp (sub2 v0 (pC poly)) * rd
              [] -> 0
            col = hsb (wrapF 360 (fromIntegral (pD poly) * 23)) 50 ((0.95 - min 0.4 (fromIntegral (pD poly) * 0.03)) * 100)
            seg = if g == Hyper && sizePx > 6 then 6 else 1
            pts = map toPx (concatMap (\(a, b) -> arcPts g a b seg) (cycPairs (pV poly)))
            c = toPx (pC poly)
            -- 双曲の辺は内側に曲がるので、大きい多角形は中心からの扇に分けて塗る
            body | g == Hyper && sizePx > 20 = Pictures [ Polygon [c, a, b] | (a, b) <- cycPairs pts ]
                 | otherwise = Polygon pts
        in Pictures [Color col body, Color (rgb255 16 18 26) (Line (pts ++ take 1 pts))]
      rim = if g == Hyper then Color (gray 0.78) (ring r 2) else Blank
  in w { wLayer = Translate 0 (-40) (Scale 1 (-1) (Pictures (map one (wPolys w) ++ [rim]))) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wLast = p }
  EventKey (MouseButton LeftButton) Up _ _ -> pure (reCenter env w)
  EventMotion p@(px, py)
    | envDown env ->
        let (lx, ly) = wLast w
            mvX = px - lx
            mvY = negate (py - ly)     -- p5.js の movedY は下向きが正
            w1 = w { wLast = p }
        in pure (if wGeom w == Sphere
                   then w1 { wRotY = wRotY w + mvX * 0.01, wRotX = wRotX w + mvY * 0.01 }
                   else dragPlane env mvX mvY w1)
    | otherwise -> pure w { wLast = p }
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure (build env w { wP = max 3 (wP w - 1) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (build env w { wP = min 10 (wP w + 1) })
  EventKey (SpecialKey KeyUp) Down _ _    -> pure (build env w { wQ = min 10 (wQ w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _  -> pure (build env w { wQ = max 3 (wQ w - 1) })
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> pure (build env w)
    | c == 's' || c == 'S' -> saveSvg "pq_tiling" env (drawAll env w) >> pure w
  _ -> pure w

dragPlane :: Env -> Float -> Float -> World -> World
dragPlane env mvX mvY w =
  let g = wGeom w
      r = radiusOf env g
      vec = ( realToFrac (clampF (-0.2) 0.2 (mvX / r)), realToFrac (clampF (-0.2) 0.2 (mvY / r)) ) :: D2
      move z = if g == Hyper then mobius z vec else (fst z + fst vec, snd z + snd vec)
      movePoly poly = poly { pV = map move (pV poly), pC = move (pC poly) }
  in renderPlane env w { wPolys = map movePoly (wPolys w) }

-- 中心にいちばん近い多角形から作り直す（端の欠けを埋める）
reCenter :: Env -> World -> World
reCenter env w
  | wGeom w == Sphere || null (wPolys w) = w
  | otherwise =
      let best = minimumBy (comparing (hyp . pC)) (wPolys w)
      in buildPlane env (Just best { pD = 0 }) w

-- ---------- やってみよう ----------
-- 1. q = 3 のまま p を 3, 4, 5, 6, 7 と増やそう。球 → 平面 → 双曲 と変わる境目は？
-- 2. 正多面体が 5 つしかない理由を (p − 2)(q − 2) < 4 から説明しよう
-- 3. {7, 3} をドラッグで動かすと、ふちの小さな七角形が真ん中に来て大きくなる。どれも同じ大きさの証拠
-- 4. 調べてみよう：エッシャー『円の極限』とコクセター、かぎ針編みの双曲平面


-- ==================================================
-- ここから下は「幾何の道具」（鏡映・反転・メビウス変換）
-- （中身は読まなくても OK）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

nf1 :: Float -> String
nf1 x = showFFloat (Just 1) x ""

cycPairs :: [a] -> [(a, a)]
cycPairs p = zip p (drop 1 p ++ take 1 p)

hyp :: D2 -> Double
hyp (a, b) = sqrt (a * a + b * b)

sub2 :: D2 -> D2 -> D2
sub2 (a, b) (c, d) = (a - c, b - d)

key2 :: D2 -> (Int, Int)
key2 (x, y) = (round (x * 1e5), round (y * 1e5))

key3 :: D3 -> (Int, Int, Int)
key3 (D3 x y z) = (round (x * 1e4), round (y * 1e4), round (z * 1e4))

crossD :: D3 -> D3 -> D3
crossD (D3 a b c) (D3 d e f) = D3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

unitD :: D3 -> D3
unitD (D3 a b c) =
  let l0 = sqrt (a * a + b * b + c * c)
      l = if l0 == 0 then 1 else l0
  in D3 (a / l) (b / l) (c / l)

scaleD :: Double -> D3 -> D3
scaleD k (D3 a b c) = D3 (k * a) (k * b) (k * c)

lerpD :: D3 -> D3 -> Double -> D3
lerpD (D3 a b c) (D3 d e f) t = D3 (a + (d - a) * t) (b + (e - b) * t) (c + (f - c) * t)

mirror3 :: D3 -> D3 -> D3
mirror3 (D3 x y z) (D3 a b c) =
  let d = 2 * (x * a + y * b + z * c)
  in D3 (x - d * a) (y - d * b) (z - d * c)

-- 点 a, b を通る測地線（双曲：単位円に直交する円、平面：直線）
geoCircle :: Geom -> D2 -> D2 -> Maybe (D2, Double)
geoCircle g (ax, ay) (bx, by)
  | g /= Hyper || abs det < 1e-9 = Nothing                 -- 原点を通る直径
  | otherwise = Just ((cx, cy), sqrt (max 0 (cx * cx + cy * cy - 1)))
  where
    -- c·a = (|a|² + 1)/2, c·b = (|b|² + 1)/2 を解く
    det = ax * by - ay * bx
    ra = (ax * ax + ay * ay + 1) / 2
    rb = (bx * bx + by * by + 1) / 2
    cx = (ra * by - rb * ay) / det
    cy = (ax * rb - bx * ra) / det

-- 測地線で折り返す（円なら反転、直線なら鏡映）
reflectGeo :: Geom -> D2 -> D2 -> D2 -> D2
reflectGeo g z@(zx, zy) a@(ax, ay) b@(bx, by) = case geoCircle g a b of
  Nothing ->
    let dx = bx - ax
        dy = by - ay
        t = ((zx - ax) * dx + (zy - ay) * dy) / max 1e-18 (dx * dx + dy * dy)
    in (2 * (ax + t * dx) - zx, 2 * (ay + t * dy) - zy)
  Just ((cx, cy), r) ->
    let dx = zx - cx
        dy = zy - cy
        d2 = max 1e-12 (dx * dx + dy * dy)
    in z `seq` (cx + r * r * dx / d2, cy + r * r * dy / d2)

-- 測地線の弧を点の列で
arcPts :: Geom -> D2 -> D2 -> Int -> [D2]
arcPts g a@(ax, ay) b@(bx, by) m = case geoCircle g a b of
  Just ((cx, cy), r) | m > 1 ->
    let t1 = atan2 (ay - cy) (ax - cx)
        t2 = atan2 (by - cy) (bx - cx)
        d0 = t2 - t1
        d | d0 > pi = d0 - 2 * pi
          | d0 < negate pi = d0 + 2 * pi
          | otherwise = d0
    in [ (cx + r * cos t, cy + r * sin t) | i <- [0 .. m - 1], let t = t1 + d * fromIntegral i / fromIntegral m ]
  _ -> [a]

-- 双曲の平行移動（メビウス変換）z → (z + w) / (1 + conj(w) z)
mobius :: D2 -> D2 -> D2
mobius (zx, zy) (wx, wy) =
  let nr = zx + wx
      ni = zy + wy
      dr = 1 + wx * zx + wy * zy
      di = wx * zy - wy * zx
      d2 = max 1e-12 (dr * dr + di * di)
      ox = (nr * dr + ni * di) / d2
      oy = (ni * dr - nr * di) / d2
      l = sqrt (ox * ox + oy * oy)
  in if l > 0.99999 then (ox * 0.99999 / l, oy * 0.99999 / l) else (ox, oy)
