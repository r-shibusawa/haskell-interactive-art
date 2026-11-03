-- ============================================
-- 40-05 主曲率と主方向 ─ オイラーの公式
-- 曲面の 1 点で、接平面の方向 θ ごとに「その方向に切った断面の曲率」= 法曲率 κ(θ) を測る
--   いちばん大きい κ1 といちばん小さい κ2 が主曲率、その方向が主方向（互いに直交する）
--   オイラーの公式：κ(θ) = κ1 cos²θ + κ2 sin²θ　　K = κ1 κ2（ガウス曲率）　H = (κ1 + κ2)/2（平均曲率）
--   主曲率・主方向は、第一・第二基本形式から作る 2×2 行列（シェイプ作用素 I⁻¹ II）の固有値・固有ベクトル
-- 操作：1〜3 = 曲面 / マウス = 調べる点 / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

nu, nv :: Int
nu = 40                  -- ★ メッシュの分割数
nv = 40

scaleF :: Double
scaleF = 120             -- ★ 曲面の大きさ（px）

sf :: Float
sf = realToFrac scaleF

-- | 曲面：名前、u と v の範囲、f(u, v)（数学の座標、z が上）
data Surface = Surface String !Double !Double !Double !Double (Double -> Double -> P3)

-- ★ 曲面のカタログ
surfaces :: [Surface]
surfaces =
  [ Surface "Torus" 0 (2 * pi) 0 (2 * pi)
      (\u v -> P3 ((1.3 + 0.55 * cos v) * cos u) ((1.3 + 0.55 * cos v) * sin u) (0.55 * sin v))
  , Surface "Saddle (hyperbolic paraboloid)" (-1.5) 1.5 (-1.5) 1.5
      (\u v -> P3 u v (0.5 * (u * u - v * v)))
  , Surface "Ellipsoid" 0 (2 * pi) (-pi / 2 + 0.08) (pi / 2 - 0.08)
      (\u v -> P3 (1.8 * cos u * cos v) (1.1 * sin u * cos v) (0.8 * sin v))
  ]

data World = World
  { wSurf :: !Int
  , wRx, wRy :: !Float
  , wPrev :: !Point
  , wProbe :: !Point
  , wMesh :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-05 Principal curvatures" (build 1 world0))
  { skBack  = hsb 215 35 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 (-0.5) 0.6 (0, 0) (0.3, 0.3) (Mesh V.empty [])

surfOf :: Int -> Surface
surfOf no = case drop (no - 1) surfaces of
  (s : _) -> s
  [] -> Surface "" 0 1 0 1 (\_ _ -> P3 0 0 0)

pt :: Surface -> Double -> Double -> P3
pt (Surface _ _ _ _ _ f) u v = let P3 x y z = f u v in P3 (x * scaleF) (negate z * scaleF) (y * scaleF)

uvAt :: Surface -> Float -> Float -> (Double, Double)
uvAt (Surface _ u0 u1 v0 v1 _) a b = (u0 + (u1 - u0) * realToFrac a, v0 + (v1 - v0) * realToFrac b)

build :: Int -> World -> World
build no w =
  let s = surfOf no
      forms = V.fromList [ let (u, v) = uvAt s (fromIntegral i / fromIntegral nu) (fromIntegral j / fromIntegral nv)
                           in fundForms (pt s) u v
                         | i <- [0 .. nu], j <- [0 .. nv] ]
      ix i j = i * (nv + 1) + j
      pos i j = toV3 (fP (forms V.! ix i j))
      col i j = kColor (realToFrac (fK (forms V.! ix i j) * scaleF * scaleF)) 0.6
  in w { wSurf = no, wMesh = gridMesh nu nv pos col }

-- | 主曲率と主方向（シェイプ作用素 W = I⁻¹ II の固有値・固有ベクトル）
data Principal = Principal { prK1, prK2 :: !Double, prE1, prE2 :: !V3 }

principal :: Forms -> Principal
principal ff =
  let det0 = fE ff * fG ff - fF ff * fF ff
      det = if det0 == 0 then 1e-12 else det0
      -- W = [[a, b], [c, d]]（(u, v) 成分）
      a = (fG ff * fLL ff - fF ff * fMM ff) / det
      b = (fG ff * fMM ff - fF ff * fNN ff) / det
      c = (fE ff * fMM ff - fF ff * fLL ff) / det
      d = (fE ff * fNN ff - fF ff * fMM ff) / det
      h = (a + d) / 2
      k = a * d - b * c
      dsc = sqrt (max (h * h - k) 0)
      k1 = h + dsc
      k2 = h - dsc
      hyp x y = sqrt (x * x + y * y)
      dir kk =
        let (x1, y1) = (b, kk - a)
            (x2, y2) = (kk - d, c)
            (x0, y0) = if hyp x1 y1 > hyp x2 y2 then (x1, y1) else (x2, y2)
            (x, y) = if hyp x0 y0 < 1e-9 then (1, 0) else (x0, y0)
            e = padd (pscale x (fRu ff)) (pscale y (fRv ff))
            l0 = sqrt (pdot e e)
            l = if l0 == 0 then 1 else l0
        in pscale (1 / l) e
      e1 = dir k1
      e2 = pcross (fNrm ff) e1                   -- e2 は e1 と直交する接方向
  in Principal k1 k2 (toV3 e1) (toV3 e2)

step :: Env -> World -> World
step env w
  | envDown env = w
  | otherwise = let (mx, my) = fromG env (envMouse env)
                in w { wProbe = (clampF 0 1 (mx / envW env), clampF 0 1 (1 - my / envH env)) }

draw :: Env -> World -> Picture
draw env w =
  let s = surfOf (wSurf w)
      (pu, pv) = wProbe w
      (u, v) = uvAt s pu pv
      ff = fundForms (pt s) u v
      pr = principal ff
      k1d = realToFrac (prK1 pr) :: Float
      k2d = realToFrac (prK2 pr) :: Float
      Mesh pts faces = wMesh w
      vw = view (wRx w) (wRy w)
      vs = V.map vw pts
      p = toV3 (fP ff)
      n = toV3 (fNrm ff)
      e1 = prE1 pr
      e2 = prE2 pr
      arrow3 d l col = let tip = add3 p (scale3 l d)
                       in ball col 4 (vw tip) : longSeg 6 col 3 (vw p) (vw tip)
      -- 法曲率の「花」：接平面の中で方向 θ に長さ |κ(θ)| の線（赤 = +、青 = −）
      petals = [ seg (hsba (if kth > 0 then 0 else 215) 80 100 0.7) 1.5 (vw p) (vw (add3 p (scale3 l (add3 (scale3 (cos th) e1) (scale3 (sin th) e2)))))
               | i <- [0 .. 47 :: Int]
               , let th = fromIntegral i / 48 * tau
               , let kth = k1d * cos th * cos th + k2d * sin th * sin th
               , let l = min (abs kth * sf) 3 * 25 ]
      -- 主方向の断面の接触円（半径 1/κ、法線の方向に中心）
      oscCircle k e hue
        | abs k < 1e-4 || abs (1 / k) > 700 = []
        | otherwise =
            let r = 1 / k
                c = add3 p (scale3 r n)
            in pathItems (hsba hue 80 100 0.55) 1.5
                 [ vw (add3 c (scale3 (abs r) (add3 (scale3 (cos a) e) (scale3 (sin a) n))))
                 | i <- [0 .. 48 :: Int], let a = fromIntegral i / 48 * tau ]
      items = meshItems 0.33 vs faces ++ arrow3 e1 70 (hsb 0 80 100) ++ arrow3 e2 70 (hsb 215 80 100) ++ arrow3 n 50 white
           ++ petals ++ oscCircle k1d e1 0 ++ oscCircle k2d e2 215 ++ [ball white 6 (vw p)]
  in Pictures [paint items, hud env w (k1d * sf) (k2d * sf)]

hud :: Env -> World -> Float -> Float -> Picture
hud env w k1 k2 =
  let Surface name _ _ _ _ _ = surfOf (wSurf w)
      h = envH env
      cx = envW env - 190
      cy = h - 190
      r0 = 130
      kmax = max (max (abs k1) (abs k2)) 1e-6
      fth th = k1 * cos th * cos th + k2 * sin th * sin th
      polar th = let f = abs (fth th) / kmax in (cx + f * r0 * cos th, cy - f * r0 * sin th)
      ang i = fromIntegral (i :: Int) / 180 * tau
      msg | abs (k1 - k2) < 0.03 = "k1 ~ k2: same bending in every direction (umbilic). Every point of a sphere is one."
          | k1 * k2 < 0 = "k1, k2 opposite signs: up one way, down another (saddle point, K < 0)"
          | otherwise = "k1, k2 same sign: bends to the same side in every direction (elliptic, K > 0)"
  in Pictures $
       [ txt env 20 40 18 white ("1-3: surface (" ++ name ++ ")   mouse: probe   drag: view")
       , txt env 20 66 14 (hsb 0 0 65) "red arrow = principal direction e1 (k1)   blue arrow = e2 (k2)   white = normal"
       , txt env 20 86 14 (hsb 0 0 65) "petals = normal curvature k(theta)   faint circles = osculating circles of the sections (radius 1/k)"
       -- 極座標プロット（オイラーの公式）
       , box env (cx - r0 - 30) (cy - r0 - 40) (2 * r0 + 60) (2 * r0 + 70) (hsba 0 0 100 0.08)
       , ringAt env (hsba 0 0 100 0.2) r0 1 (cx, cy)
       , seg2 env (hsba 0 0 100 0.2) 1 (cx - r0, cy) (cx + r0, cy)
       , seg2 env (hsba 0 0 100 0.2) 1 (cx, cy - r0) (cx, cy + r0) ] ++
       [ seg2 env (hsb (if fth (ang i) > 0 then 0 else 215) 80 100) 2.5 (polar (ang i)) (polar (ang (i + 1))) | i <- [0 .. 179] ] ++
       [ txt env (cx - r0 - 24) (cy - r0 - 18) 14 white "k(th) = k1 cos^2 th + k2 sin^2 th"
       , txt env (cx + r0 - 90) (cy + 18) 14 (hsb 0 80 100) ("k1 = " ++ nf 3 k1)
       , txt env (cx - 32) (cy - r0 + 16) 14 (hsb 215 80 100) ("k2 = " ++ nf 3 k2)
       -- 数値パネル
       , box env 20 (h - 120) 880 96 (hsba 0 0 100 0.08)
       , txt env 36 (h - 84) 18 white ("k1 = " ++ nf 3 k1 ++ "  k2 = " ++ nf 3 k2 ++ "  ->  K = k1 k2 = " ++ nf 3 (k1 * k2) ++ "  H = " ++ nf 3 ((k1 + k2) / 2))
       , txt env 36 (h - 52) 14 (hsb 0 0 65) msg
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> build (fromEnum c - fromEnum '0') w
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. トーラス（1）の外側の点では κ1, κ2 が同符号、内側では逆符号。花びらの色が変わるのを見よう
-- 2. 鞍（2）の中心では κ1 = −κ2（H = 0）。花びらが 4 枚のきれいな形になる。中心からずれると？
-- 3. 楕円体（3）には κ1 = κ2 になる点（臍点）が 4 つある。マウスで探してみよう（花びらが円になる）
-- 4. 主方向 e1 と e2 はいつも直交している。曲面を変えても確かめよう（なぜ直交する？→対称行列の固有ベクトル）

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
