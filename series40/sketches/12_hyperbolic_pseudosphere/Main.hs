-- ============================================
-- 40-12 負曲率の世界 ─ 擬球と双曲平面
-- 擬球 = トラクトリックス（牽引線）を回した曲面。どこでも K = −1（球面の K = +1 の逆）
--   擬球の上の幾何は双曲平面の一部：上半平面モデル {(x, y) : y ≥ 1}、ds² = (dx² + dy²)/y²、測地線は x 軸に中心を持つ半円
--   測地三角形の内角の和は π 未満。不足分 π − (A + B + C) = 三角形の面積（K = −1 なので）
-- 操作：下の上半平面で三角形の頂点をドラッグ / R = 元に戻す / 空いた所をドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

scaleF :: Double
scaleF = 150              -- ★ 擬球の大きさ（px）

tri0 :: (Point, Point, Point)
tri0 = ((1.2, 1.5), (4.6, 1.3), (3.1, 3.2))   -- ★ 上半平面の三角形の頂点 (x, y)、y ≥ 1

-- 上半平面の表示枠（p5.js の座標）
panelX, panelW, panelH, xMax, yMax :: Float
panelX = 30
panelW = 440
panelH = 250
xMax = 2 * pi
yMax = 4.5

discR :: Float
discR = 120               -- ポアンカレ円板の半径

data World = World
  { wTri :: !(Point, Point, Point)
  , wDrag :: !Int                 -- つかんでいる頂点（0, 1, 2、なければ −1）
  , wRx, wRy :: !Float
  , wPrev :: !Point
  , wMesh :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-12 Pseudosphere and hyperbolic plane" world0)
  { skBack  = hsb 250 30 9
  , skDraw  = draw
  , skEvent = event
  }

-- 擬球のメッシュ（K で色：全部 −1 なので青一色になるはず）
world0 :: World
world0 =
  let nu = 48
      nv = 40
      vAt i = fromIntegral i / fromIntegral nu * 2 * pi :: Double
      uAt j = 0.02 + 2.6 * fromIntegral j / fromIntegral nv :: Double
      pos i j = toV3 (pseudo (uAt j) (vAt i))
      col i j = kColor (realToFrac (fK (fundForms pseudo (uAt j) (vAt i)) * scaleF * scaleF)) 1
  in World tri0 (-1) (-0.3) 0.6 (0, 0) (gridMesh nu nv pos col)

-- 擬球 r(u, v) = (sech u cos v, sech u sin v, u − tanh u)（p5 の座標、z を上に）
pseudo :: Double -> Double -> P3
pseudo u v =
  let s = 1 / cosh u
  in P3 (scaleF * s * cos v) (negate scaleF * (u - tanh u) + 110) (scaleF * s * sin v)

-- 上半平面 (x, y) → 擬球（y = cosh u、x = v）
toSurface :: Point -> V3
toSurface (x, y) = toV3 (pseudo (acosh (realToFrac (max y 1))) (realToFrac x))

-- 曲面から少し浮かせる
lift :: V3 -> V3
lift (V3 x y z) = let l0 = norm2 (x, z)
                      l = if l0 == 0 then 1 else l0
                  in V3 (x + 2.5 * x / l) y (z + 2.5 * z / l)

-- | 測地線の弧：点の列と、両端での向き
data HArc = HArc [Point] !Point !Point

-- 上半平面の 2 点を結ぶ測地線（x 軸に中心をもつ半円）を n 個の点でサンプル。向きの接ベクトルも返す
hypArc :: Point -> Point -> Int -> HArc
hypArc (ax, ay) (bx, by) n
  | abs (ax - bx) < 1e-6 =
      HArc [(ax, ay + (by - ay) * frac i) | i <- [0 .. n]] (0, signum (by - ay)) (0, signum (ay - by))
  | otherwise =
      let c = (bx * bx + by * by - ax * ax - ay * ay) / (2 * (bx - ax))
          r = norm2 (ax - c, ay)
          t1 = atan2 ay (ax - c)
          t2 = atan2 by (bx - c)
          sg = signum (t2 - t1)
      in HArc [let th = t1 + (t2 - t1) * frac i in (c + r * cos th, r * sin th) | i <- [0 .. n]]
             (negate sg * sin t1, sg * cos t1)
             (sg * sin t2, negate sg * cos t2)
  where
    frac i = fromIntegral i / fromIntegral n

-- 三角形の 3 辺と内角（等角モデルなので、ふつうの角度で測ってよい）
triangleInfo :: (Point, Point, Point) -> ([HArc], [Float])
triangleInfo (p0, p1, p2) =
  let a0 = hypArc p0 p1 40
      a1 = hypArc p1 p2 40
      a2 = hypArc p2 p0 40
      ang (HArc _ out _) (HArc _ _ inn) = acos (clampF (-1) 1 (dot2 out inn))
  in ([a0, a1, a2], [ang a0 a2, ang a1 a0, ang a2 a1])

panelY :: Env -> Float
panelY env = envH env - panelH - 30

toPanel :: Env -> Point -> Point
toPanel env (x, y) = (panelX + x / xMax * panelW, panelY env + panelH - y / yMax * panelH)

fromPanel :: Env -> Point -> Point
fromPanel env (mx, my) =
  ( clampF 0.05 (xMax - 0.05) ((mx - panelX) / panelW * xMax)
  , clampF 1.02 yMax ((panelY env + panelH - my) / panelH * yMax) )

-- ケイリー変換 w = i(z − i)/(z + i)
toDisc :: Env -> Point -> Point
toDisc env (x0, y) =
  let x = x0 - pi
      d = x * x + (y + 1) * (y + 1)
  in (envW env - 170 + 2 * x / d * discR, envH env - 170 - (x * x + y * y - 1) / d * discR)

triList :: (Point, Point, Point) -> [Point]
triList (a, b, c) = [a, b, c]

hueList :: [Float]
hueList = [45, 130, 300]        -- ★ 3 辺の色相

draw :: Env -> World -> Picture
draw env w =
  let (arcs, angles) = triangleInfo (wTri w)
      vw = view (wRx w) (wRy w)
      Mesh pts faces = wMesh w
      vs = V.map vw pts
      -- 三角形を曲面に写す
      arcItems = concat [ pathItems (hsb h 85 100) 3 (map (vw . lift . toSurface) ps) | (h, HArc ps _ _) <- zip hueList arcs ]
      balls = [ ball white 5 (vw (toSurface p)) | p <- triList (wTri w) ]
  in Pictures [paint (meshItems 0.33 vs faces ++ arcItems ++ balls), hud env w arcs angles]

hud :: Env -> World -> [HArc] -> [Float] -> Picture
hud env w arcs angles =
  let g = hsb 0 0 65
      lab = hsb 0 0 70
      px = panelX
      py = panelY env
      y1 = snd (toPanel env (0, 1))
      total = sum angles
      kNow = realToFrac (fK (fundForms pseudo 1 1) * scaleF * scaleF) :: Float
      ix = envW env - 680
      iy = 110
      cx = envW env - 170
      cy = envH env - 170
      deg a = nf 1 (a * 180 / pi)
      names = ["A", "B", "C"]
      vertexMark k name p ang =
        let q@(qx, qy) = toPanel env p
        in [ dotAt env white (if wDrag w == k then 8 else 6) q
           , txt env (qx + 10) (qy - 8) 14 white (name ++ " = " ++ deg ang ++ " deg") ]
  in Pictures $
       [ txt env 20 40 18 white "drag a vertex in the half-plane below   R: reset   drag empty space: view"
       , txt env 20 66 14 g ("Pseudosphere: surface of revolution of the tractrix. Color = K (blue = negative). Computed K = " ++ nf 3 kNow ++ " (-1 everywhere)")
       , txt env 20 86 14 g "The strip y >= 1 maps isometrically (y = cosh u, x = v)"
       -- 上半平面モデル
       , box env (px - 10) (py - 10) (panelW + 20) (panelH + 20) (hsba 0 0 100 0.08)
       , box env px py panelW (y1 - py) (hsba 230 50 60 0.25)                -- y ≥ 1 の帯（擬球の部分）
       , seg2 env (hsba 0 0 100 0.3) 1 (px, y1) (px + panelW, y1) ] ++
       [ path2 env (hsb h 85 100) 2.5 (map (toPanel env) ps) | (h, HArc ps _ _) <- zip hueList arcs ] ++
       concat (zipWith4' vertexMark [0 ..] names (triList (wTri w)) angles) ++
       [ txt env px (py - 36) 14 lab "Half-plane: ds^2 = (dx^2 + dy^2)/y^2"
       , txt env px (py - 18) 14 lab "geodesics = semicircles on the x axis (y >= 1 = surface)"
       , txt env px (py + panelH + 18) 14 lab "x = 0"
       , txt env (px + panelW - 60) (py + panelH + 18) 14 lab "x = 2 pi"
       , txt env (px + panelW - 50) (y1 - 4) 14 lab "y = 1"
       -- ポアンカレ円板
       , dotAt env (hsba 0 0 100 0.08) (discR + 10) (cx, cy)
       , ringAt env (hsba 0 0 100 0.4) discR 1 (cx, cy) ] ++
       [ path2 env (hsb h 85 100) 2.5 (map (toDisc env) ps) | (h, HArc ps _ _) <- zip hueList arcs ] ++
       [ dotAt env white 4 (toDisc env p) | p <- triList (wTri w) ] ++
       [ txt env (cx - 110) (cy - discR - 22) 14 lab "Poincare disk (same triangle)"
       -- 内角の和
       , box env (ix - 16) (iy - 10) 680 126 (hsba 0 0 100 0.08)
       , txt env ix (iy + 26) 20 white ("Angle sum A + B + C = " ++ deg total ++ " deg < 180 deg")
       , txt env ix (iy + 54) 15 white ("Deficit pi - (A+B+C) = " ++ nf 4 (pi - total) ++ " rad = triangle area (K = -1)")
       , txt env ix (iy + 78) 14 g "Sphere (07): excess = area. Plane: 0. Hyperbolic plane: deficit = area."
       , txt env ix (iy + 98) 14 g "Three faces of Gauss-Bonnet."
       ]

zipWith4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4' f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4' f as bs cs ds
zipWith4' _ _ _ _ _ = []

-- k 番の頂点を p に動かす
setVertex :: Int -> Point -> (Point, Point, Point) -> (Point, Point, Point)
setVertex k p (a, b, c) = case k of
  0 -> (p, b, c)
  1 -> (a, p, c)
  _ -> (a, b, p)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(mx, my) = fromG env pos
        dists = zip [0 ..] (map (dist2 m . toPanel env) (triList (wTri w)))
        near = foldl (\d (k, v) -> if v < 18 then k else d) (-1) dists
        py = panelY env
        inPanel = mx > panelX && mx < panelX + panelW && my > py && my < py + panelH
        nearest = fst (foldl (\(bk, bd) (k, v) -> if v < bd then (k, v) else (bk, bd)) (0, 1e9) dists)
    in if near >= 0 then w { wDrag = near, wPrev = pos }
       else if inPanel then w { wDrag = nearest, wTri = setVertex nearest (fromPanel env m) (wTri w), wPrev = pos }
       else w { wDrag = -1, wPrev = pos }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos
    | envDown env && wDrag w >= 0 -> w { wTri = setVertex (wDrag w) (fromPanel env (fromG env pos)) (wTri w), wPrev = pos }
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  EventKey (Char c) Down _ _ | c == 'r' || c == 'R' -> w { wTri = tri0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 三角形を大きくするほど内角の和が小さくなる。頂点を上（y 大）に引っぱると曲面のどこに行く？（細い先のほう）
-- 2. 小さい三角形は内角の和がほぼ 180°（小さい範囲では平面とほぼ同じ）。どのくらい小さいと 179° になる？
-- 3. 頂点を x = 0 と x = 2π の近くに置くと、擬球の上では「裏で貼り合わさって」いる。曲面の上でつながるか見よう
-- 4. 2 つの頂点を同じ x にすると、その辺は垂直線（半径無限大の半円）。擬球の上では子午線になる

-- ==================================================
-- ここから下は「3D の道具」です（視点の回転・画家のアルゴリズム・面の陰影）
-- OpenGL の深度バッファの代わりに、奥の絵から順に重ねて描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | カメラまでの距離（p5.js の WEBGL の既定値 (height / 2) / tan(π/6)）
camZ :: Float
camZ = 400 / tan (pi / 6)

-- | 物体の点 → カメラの座標。p5.js の rotateX(rx); rotateY(ry) と同じ順番。y は下向き、z は手前向き
view :: Float -> Float -> V3 -> V3
view rx ry = rotX rx . rotY ry

-- | カメラの座標 → 画面（gloss は y が上向きなので反転する）
scr :: V3 -> Point
scr v = let (x, y) = project camZ v in (x, negate y)

-- | 奥行きつきの絵（奥行きは z、大きいほど手前）
type Item = (Float, Picture)

-- | 奥から順に並べて 1 枚の絵にする（画家のアルゴリズム）
paint :: [Item] -> Picture
paint = Pictures . map snd . depthSort fst

-- | 面より少し手前に描くためのずらし（線や点が面に隠れないように）
lineBias :: Float
lineBias = 3

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- | カメラの座標の線分（w は太さ、1 以下なら細い線）
seg :: Color -> Float -> V3 -> V3 -> Item
seg c w a b =
  ( (zOf a + zOf b) / 2 + lineBias
  , Color c (if w <= 1 then Line [scr a, scr b] else thickLine w (scr a) (scr b)) )

-- | 長い線分を n 個に切って描く（途中で面に隠れる線になる）
longSeg :: Int -> Color -> Float -> V3 -> V3 -> [Item]
longSeg n c w a b = pathItems c w [add3 a (scale3 (fromIntegral i / fromIntegral n) (sub3 b a)) | i <- [0 .. n]]

-- | カメラの座標の折れ線
pathItems :: Color -> Float -> [V3] -> [Item]
pathItems c w ps = zipWith (seg c w) ps (drop 1 ps)

-- | カメラの座標の小さな球（p5.js の sphere。画面では円）
ball :: Color -> Float -> V3 -> Item
ball c r v = (zOf v + lineBias, at (scr v) (Color c (disc (r * camZ / max 1 (camZ - zOf v)))))

-- | 面：4 つの頂点の番号（周の順、三角形なら最後を 2 回）と色
data Face = Face !Int !Int !Int !Int !Float !Float !Float

-- | 頂点の配列と面のリスト
data Mesh = Mesh !(V.Vector V3) [Face]

-- | 格子のメッシュ。pos i j が頂点の位置、col i j が頂点の色（面の色は 4 頂点の平均）
gridMesh :: Int -> Int -> (Int -> Int -> V3) -> (Int -> Int -> (Float, Float, Float)) -> Mesh
gridMesh nu nv pos col =
  Mesh (V.fromList [pos i j | i <- [0 .. nu], j <- [0 .. nv]])
       [ let (r1, g1, b1) = col i j
             (r2, g2, b2) = col (i + 1) j
             (r3, g3, b3) = col (i + 1) (j + 1)
             (r4, g4, b4) = col i (j + 1)
         in Face (ix i j) (ix (i + 1) j) (ix (i + 1) (j + 1)) (ix i (j + 1))
                 ((r1 + r2 + r3 + r4) / 4) ((g1 + g2 + g3 + g4) / 4) ((b1 + b2 + b3 + b4) / 4)
       | i <- [0 .. nu - 1], j <- [0 .. nv - 1] ]
  where ix i j = i * (nv + 1) + j

-- | 面を陰影つきの絵にする。vs は回転ずみの頂点、amb は環境光の強さ（両面に光が当たる）
faceItem :: Float -> V.Vector V3 -> Face -> Item
faceItem amb vs (Face ia ib ic id' r g b) =
  let pa = vs V.! ia
      pb = vs V.! ib
      pc = vs V.! ic
      pd = vs V.! id'
      cen = scale3 0.25 (add3 pa (add3 pb (add3 pc pd)))
      n0 = unit3 (cross3 (sub3 pc pa) (sub3 pd pb))
      n = if dot3 n0 (sub3 (V3 0 0 camZ) cen) < 0 then scale3 (-1) n0 else n0
      s = amb + 0.75 * max 0 (dot3 n light1) + 0.31 * max 0 (dot3 n light2)
      tone v = min 1 (v * s)
  in (zOf cen, Color (makeColor (tone r) (tone g) (tone b) 1) (Polygon [scr pa, scr pb, scr pc, scr pd]))

-- | 光の向き（p5.js の directionalLight の逆向き）
light1, light2 :: V3
light1 = unit3 (V3 0.4 (-0.6) 1)
light2 = unit3 (V3 (-0.5) 0.4 (-0.5))

-- | メッシュ全体を絵のリストにする
meshItems :: Float -> V.Vector V3 -> [Face] -> [Item]
meshItems amb vs = map (faceItem amb vs)

-- | ドラッグで視点を回す（p5.js の mouseDragged と同じ）
dragRot :: Point -> Point -> (Float, Float) -> (Float, Float)
dragRot (x0, y0) (x1, y1) (rx, ry) = (clampF (-1.6) 1.6 (rx - (y1 - y0) * 0.01), ry + (x1 - x0) * 0.01)

-- | 曲率 → 色（+ は赤、0 は白、− は青）
kColor :: Float -> Float -> (Float, Float, Float)
kColor v s =
  let t = tanh (v / s)
  in if t > 0 then (1, 1 - 0.85 * t, 1 - 0.85 * t) else (1 + 0.8 * t, 1 + 0.6 * t, 1)

-- | (r, g, b) → gloss の色
rgbC :: (Float, Float, Float) -> Color
rgbC (r, g, b) = makeColor r g b 1

-- ==================================================
-- ここから下は「曲面の道具」です（第一・第二基本形式の差分計算）
-- 差分は桁落ちしやすいので、ここだけ Double で計算する
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | Double の 3 次元ベクトル
data P3 = P3 !Double !Double !Double

padd, psub, pcross :: P3 -> P3 -> P3
padd (P3 a b c) (P3 d e f) = P3 (a + d) (b + e) (c + f)
psub (P3 a b c) (P3 d e f) = P3 (a - d) (b - e) (c - f)
pcross (P3 a b c) (P3 d e f) = P3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

pscale :: Double -> P3 -> P3
pscale k (P3 a b c) = P3 (k * a) (k * b) (k * c)

pdot :: P3 -> P3 -> Double
pdot (P3 a b c) (P3 d e f) = a * d + b * e + c * f

toV3 :: P3 -> V3
toV3 (P3 a b c) = V3 (realToFrac a) (realToFrac b) (realToFrac c)

-- | 基本形式の計算結果
data Forms = Forms
  { fP, fRu, fRv, fNrm :: !P3                  -- 点、r_u、r_v、単位法線
  , fE, fF, fG, fLL, fMM, fNN :: !Double       -- 第一基本形式 E F G、第二基本形式 L M N
  , fK, fH :: !Double                          -- ガウス曲率、平均曲率
  }

-- | 点 (u, v) で r_u, r_v, r_uu, r_uv, r_vv を中心差分で求め、第一・第二基本形式と K, H を返す
fundForms :: (Double -> Double -> P3) -> Double -> Double -> Forms
fundForms f u v =
  let h = 1e-3
      p = f u v
      pu1 = f (u + h) v
      pu0 = f (u - h) v
      pv1 = f u (v + h)
      pv0 = f u (v - h)
      ppp = f (u + h) (v + h)
      ppm = f (u + h) (v - h)
      pmp = f (u - h) (v + h)
      pmm = f (u - h) (v - h)
      ru = pscale (1 / (2 * h)) (psub pu1 pu0)
      rv = pscale (1 / (2 * h)) (psub pv1 pv0)
      ruu = pscale (1 / (h * h)) (padd (psub pu1 (pscale 2 p)) pu0)
      rvv = pscale (1 / (h * h)) (padd (psub pv1 (pscale 2 p)) pv0)
      ruv = pscale (1 / (4 * h * h)) (padd (psub (psub ppp ppm) pmp) pmm)
      c = pcross ru rv
      cl = let l = sqrt (pdot c c) in if l == 0 then 1e-12 else l
      n = pscale (1 / cl) c
      e' = pdot ru ru
      f' = pdot ru rv
      g' = pdot rv rv
      l' = pdot ruu n
      m' = pdot ruv n
      n' = pdot rvv n
      det = let d = e' * g' - f' * f' in if d == 0 then 1e-12 else d
  in Forms p ru rv n e' f' g' l' m' n' ((l' * n' - m' * m') / det) ((e' * n' + g' * l' - 2 * f' * m') / (2 * det))

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
