-- ============================================
-- 40-04 ガウス曲率 K と平均曲率 H で色をつける
-- 曲面の各点で、第一基本形式 (E, F, G) = (r_u·r_u, r_u·r_v, r_v·r_v) と
--   第二基本形式 (L, M, N) = (r_uu·n, r_uv·n, r_vv·n) を差分で計算し
--   K = (LN − M²)/(EG − F²)、H = (EN + GL − 2FM)/(2(EG − F²))
--   K > 0 = お椀のように丸い（赤）、K < 0 = 鞍のように反る（青）、K ≈ 0 = 平ら・筒（白）
-- 操作：1〜4 = 曲面 / C = K ↔ H で色分け / マウス = 調べる点 / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nu, nv :: Int
nu = 48                  -- ★ メッシュの分割数
nv = 48

scaleF :: Double
scaleF = 120             -- ★ 曲面の大きさ（px）

-- | 曲面：名前、u と v の範囲、f(u, v)（数学の座標 [x, y, z]、z が上）
data Surface = Surface String !Double !Double !Double !Double (Double -> Double -> P3)

-- ★ 曲面のカタログ
surfaces :: [Surface]
surfaces =
  [ Surface "Torus" 0 (2 * pi) 0 (2 * pi)
      (\u v -> P3 ((1.3 + 0.55 * cos v) * cos u) ((1.3 + 0.55 * cos v) * sin u) (0.55 * sin v))
  , Surface "Saddle (hyperbolic paraboloid)" (-1.5) 1.5 (-1.5) 1.5
      (\u v -> P3 u v (0.5 * (u * u - v * v)))
  , Surface "Hills and valleys" (-2) 2 (-2) 2
      (\u v -> P3 u v (0.55 * (sin (1.7 * u + 0.3) * cos (1.3 * v) + 0.6 * sin (2.6 * v + 1) * cos (0.9 * u))))
  , Surface "Sphere" 0 (2 * pi) (-pi / 2 + 0.05) (pi / 2 - 0.05)
      (\u v -> P3 (1.4 * cos u * cos v) (1.4 * sin u * cos v) (1.4 * sin v))
  ]

data World = World
  { wSurf :: !Int
  , wByK :: !Bool                  -- True なら K で色分け
  , wRx, wRy :: !Float
  , wPrev :: !Point
  , wProbe :: !Point               -- 調べる点（パラメータ平面の 0〜1）
  , wKmap, wHmap :: !(U.Vector Float)
  , wKref, wHref :: !Float
  , wMesh :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-04 Gauss and mean curvature" (build 1 True world0))
  { skBack  = hsb 215 35 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 True (-0.5) 0.6 (0, 0) (0.5, 0.5) U.empty U.empty 1 1 (Mesh V.empty [])

surfOf :: Int -> Surface
surfOf no = case drop (no - 1) surfaces of
  (s : _) -> s
  [] -> Surface "" 0 1 0 1 (\_ _ -> P3 0 0 0)

-- p5 の座標（y は下向きなので z を上にする）
pt :: Surface -> Double -> Double -> P3
pt (Surface _ _ _ _ _ f) u v = let P3 x y z = f u v in P3 (x * scaleF) (negate z * scaleF) (y * scaleF)

uvAt :: Surface -> Float -> Float -> (Double, Double)
uvAt (Surface _ u0 u1 v0 v1 _) a b = (u0 + (u1 - u0) * realToFrac a, v0 + (v1 - v0) * realToFrac b)

-- 80% 点（色の基準）
percentile :: U.Vector Float -> Float
percentile xs =
  let a = U.fromList (sort (map abs (U.toList xs)))
      i = floor (fromIntegral (U.length a) * (0.8 :: Float))
  in max (if U.null a then 0 else a U.! min i (U.length a - 1)) 1e-4

ix :: Int -> Int -> Int
ix i j = i * (nv + 1) + j

-- 全頂点で K, H を計算して色をつける
build :: Int -> Bool -> World -> World
build no byK w =
  let s = surfOf no
      forms = V.fromList [ let (u, v) = uvAt s (fromIntegral i / fromIntegral nu) (fromIntegral j / fromIntegral nv)
                           in fundForms (pt s) u v
                         | i <- [0 .. nu], j <- [0 .. nv] ]
      kmap = U.fromList (map (\f -> realToFrac (fK f * scaleF * scaleF)) (V.toList forms))
      hmap = U.fromList (map (\f -> realToFrac (fH f * scaleF)) (V.toList forms))
      kr = percentile kmap
      hr = percentile hmap
      col i j = if byK then kColor (kmap U.! ix i j) kr else kColor (hmap U.! ix i j) hr
      pos i j = toV3 (fP (forms V.! ix i j))
  in w { wSurf = no, wByK = byK, wKmap = kmap, wHmap = hmap, wKref = kr, wHref = hr, wMesh = gridMesh nu nv pos col }

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
      Mesh pts faces = wMesh w
      vw = view (wRx w) (wRy w)
      vs = V.map vw pts
      p = toV3 (fP ff)
      n = toV3 (fNrm ff)
      -- 調べる点：法線と接平面の小さな四角
      eu = unit3 (toV3 (fRu ff))
      ev = unit3 (toV3 (fRv ff))
      corner a b = vw (add3 p (scale3 40 (add3 (scale3 a eu) (scale3 b ev))))
      square = pathItems (hsba 45 90 100 0.8) 1.5 [corner (-1) (-1), corner 1 (-1), corner 1 1, corner (-1) 1, corner (-1) (-1)]
      probe = ball white 6 (vw p) : square ++ longSeg 6 white 2.5 (vw p) (vw (add3 p (scale3 60 n)))
  in Pictures [paint (meshItems 0.33 vs faces ++ probe), hud env w ff]

hud :: Env -> World -> Forms -> Picture
hud env w ff =
  let Surface name _ _ _ _ _ = surfOf (wSurf w)
      h = envH env
      wd = envW env
      sc = scaleF
      kNow = realToFrac (fK ff * sc * sc) :: Float
      hNow = realToFrac (fH ff * sc) :: Float
      disc' = max (hNow * hNow - kNow) 0
      k1 = hNow + sqrt disc'
      k2 = hNow - sqrt disc'
      unit1 v = nf 2 (realToFrac (v / sc / sc))
      unit2 v = nf 2 (realToFrac (v / sc))
      (bx, by, bw, bh) = (wd - 230, h - 230, 200, 200)
      mapCol i j =
        let a = ix (round ((fromIntegral i + 0.5) / 20 * fromIntegral nu :: Float)) (round ((fromIntegral j + 0.5) / 20 * fromIntegral nv :: Float))
        in if wByK w then kColor (wKmap w U.! a) (wKref w) else kColor (wHmap w U.! a) (wHref w)
      ref = if wByK w then wKref w else wHref w
      (pu, pv) = wProbe w
      fi k = fromIntegral (k :: Int) :: Float
  in Pictures $
       [ txt env 20 40 18 white ("1-4: surface (" ++ name ++ ")   C: color = " ++ (if wByK w then "Gauss K" else "mean H") ++ "   mouse: probe   drag: view")
       , txt env 20 66 14 (hsb 0 0 65) "K = (LN - M^2)/(EG - F^2)   H = (EN + GL - 2FM)/(2(EG - F^2))"
       , txt env 20 86 14 (hsb 0 0 65) "red = K > 0 (round)   blue = K < 0 (saddle)   white = K ~ 0 (flat, tube)"
       -- 数値パネル
       , box env 20 (h - 150) 760 126 (hsba 0 0 100 0.08)
       , txt env 36 (h - 118) 20 white ("K = " ++ nf 3 kNow ++ "   H = " ++ nf 3 hNow ++ (if abs kNow < 0.02 then "   (nearly flat or tube)" else if kNow > 0 then "   (elliptic point)" else "   (hyperbolic point)"))
       , txt env 36 (h - 88) 18 white ("principal k1 = " ++ nf 3 k1 ++ "  k2 = " ++ nf 3 k2 ++ "  (K = k1 k2, H = (k1+k2)/2)")
       , txt env 36 (h - 56) 14 (hsb 0 0 65) ("E = " ++ unit1 (fE ff) ++ "  F = " ++ unit1 (fF ff) ++ "  G = " ++ unit1 (fG ff) ++ "   L = " ++ unit2 (fLL ff) ++ "  M = " ++ unit2 (fMM ff) ++ "  N = " ++ unit2 (fNN ff) ++ "  (surface size = 1)")
       -- パラメータ平面と調べる点
       , box env (bx - 10) (by - 10) (bw + 20) (bh + 20) (hsba 0 0 100 0.08) ] ++
       [ box env (bx + fi i / 20 * bw) (by + bh - fi (j + 1) / 20 * bh) (bw / 20 + 0.5) (bh / 20 + 0.5) (rgbC (mapCol i j)) | i <- [0 .. 19], j <- [0 .. 19] ] ++
       [ ringAt env white 6 2 (bx + pu * bw, by + bh - pv * bh)
       , txt env bx (by - 16) 14 (hsb 0 0 70) "(u, v) plane (mouse)" ] ++
       -- 凡例
       [ box env (wd - 240 + fi i * 3.5) 100 4 14 (rgbC (kColor ((fi i / 59 - 0.5) * 2 * ref * 1.5) ref)) | i <- [0 .. 59] ] ++
       [ txt env (wd - 258) 112 14 (hsb 0 0 70) "-"
       , txt env (wd - 26) 112 14 (hsb 0 0 70) "+"
       , txt env (wd - 240) 94 14 (hsb 0 0 70) ((if wByK w then "K" else "H") ++ " color")
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> build (fromEnum c - fromEnum '0') (wByK w) w
    | c == 'c' || c == 'C' -> build (wSurf w) (not (wByK w)) w
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. トーラス（1）の外側は赤（K > 0）、内側は青（K < 0）、その境目（いちばん上と下の円）は白。なぜ？
-- 2. 球面（4）はどこでも K = 1/R²（半径 1.4 なら 0.51）。マウスをどこに置いても同じ値になるか確かめよう
-- 3. 鞍（2）は全部青。C で H に切り替えると中心で H = 0（極小曲面に近い点）
-- 4. surfaces に円柱 (cos u, sin u, v) を足すと K = 0 で真っ白、H ≠ 0 になる。平面と円柱は「内側から見ると同じ」

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
