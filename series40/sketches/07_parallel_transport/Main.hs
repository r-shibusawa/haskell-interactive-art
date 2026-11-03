-- ============================================
-- 40-07 平行移動とホロノミー ─ 球面を一周すると矢印が回る
-- 曲面の上でベクトルを「できるだけ向きを変えずに」運ぶ（平行移動）：一歩ごとに法線方向の成分を捨てるだけ
--   v_{k+1} = v_k − (v_k·n_{k+1}) n_{k+1}
--   平らな紙なら一周して戻ると元の向き。球面では戻ったとき回っている（ホロノミー）
--   回った角度 = 囲んだ領域の面積 / R²（= ∫K dA、球面過剰）。緯線なら 2π(1 − sin φ)、測地三角形なら内角の和 − π
-- 操作：1 = 緯線 / 2 = 測地三角形 / ↑↓ = 緯度 φ / ←→ = 三角形の角 α / スペース = 一時停止 / R = 最初から / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

radius :: Float
radius = 185              -- ★ 球の半径（px）

speed :: Int
speed = 2                 -- ★ 1 フレームに進む歩数

data World = World
  { wMode :: !Int                  -- 1 = 緯線、2 = 測地三角形
  , wPhi, wAng :: !Float           -- 緯度 φ、三角形の経度差 α
  , wPath :: !(V.Vector V3)        -- 経路
  , wVecs :: !(V.Vector V3)        -- 平行移動された矢印（経路の点数 + 1 個）
  , wK :: !Int                     -- 今の歩数
  , wPaused :: !Bool
  , wRx, wRy :: !Float
  , wPrev :: !Point
  , wMesh :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-07 Parallel transport" (rebuild world0))
  { skBack  = hsb 220 30 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 0.55 1.1 V.empty V.empty 0 False (-0.35) (-0.7) (0, 0) (Mesh V.empty [])

-- 球面の点（緯度 lat、経度 lon）→ p5 の座標（z を上に）
sph :: Float -> Float -> V3
sph lat lon = V3 (radius * cos lat * cos lon) (negate radius * sin lat) (radius * cos lat * sin lon)

-- 大円の弧（2 点を slerp でつなぐ）
arc3 :: V3 -> V3 -> Int -> [V3]
arc3 a b n =
  let w = acos (clampF (-1) 1 (dot3 a b / (radius * radius)))
  in [ add3 (scale3 (sin ((1 - t) * w) / sin w) a) (scale3 (sin (t * w) / sin w) b)
     | i <- [0 .. n - 1], let t = fromIntegral i / fromIntegral n ]

-- 領域の中かどうか（緯線：北側のキャップ、三角形：経度 0〜α の北半球）
inside :: World -> Float -> Float -> Bool
inside w lat lon0 =
  let lon = wrapF tau lon0
  in if wMode w == 1 then lat > wPhi w else lat > 0 && lon < wAng w

rebuild :: World -> World
rebuild w =
  let pathL
        | wMode w == 1 = [sph (wPhi w) (fromIntegral i / 600 * tau) | i <- [0 .. 599 :: Int]]
        | otherwise =
            let p = sph (pi / 2 - 1e-4) 0
                a = sph 0 0
                b = sph 0 (wAng w)
            in arc3 a b 240 ++ arc3 b p 200 ++ arc3 p a 200
      path = V.fromList pathL
      n = V.length path
      t0 = unit3 (sub3 (path V.! 1) (path V.! 0))
      -- 平行移動を全部先に計算しておく
      transport v i =
        let nrm = unit3 (path V.! (i `mod` n))
        in unit3 (sub3 v (scale3 (dot3 v nrm) nrm))
      vecs = V.fromList (scanl transport t0 [1 .. n])
      -- 球のメッシュ（囲んだ領域に色）
      nu = 48
      nv = 28
      lonAt i = fromIntegral i / fromIntegral nu * tau
      latAt j = -pi / 2 + 0.005 + (pi - 0.01) * fromIntegral j / fromIntegral nv
      col i j = if inside w (latAt j) (lonAt i) then (1, 0.78, 0.45) else (0.5, 0.58, 0.75)
  in w { wPath = path, wVecs = vecs, wK = 0, wMesh = gridMesh nu nv (\i j -> sph (latAt j) (lonAt i)) col }

step :: Env -> World -> World
step env w0 =
  let key k = isDown env (SpecialKey k)
      w1 = w0 { wPhi = if key KeyUp then min 1.4 (wPhi w0 + 0.01) else if key KeyDown then max (-1.4) (wPhi w0 - 0.01) else wPhi w0
              , wAng = if key KeyRight then min 3 (wAng w0 + 0.02) else if key KeyLeft then max 0.2 (wAng w0 - 0.02) else wAng w0 }
      w2 = if any key [KeyUp, KeyDown, KeyRight, KeyLeft] then rebuild w1 else w1
  in if wPaused w2 then w2 else w2 { wK = min (V.length (wPath w2)) (wK w2 + speed) }

lift :: Float -> V3 -> V3
lift d p = add3 p (scale3 d (unit3 p))

draw :: Env -> World -> Picture
draw env w =
  let Mesh pts faces = wMesh w
      vw = view (wRx w) (wRy w)
      vs = V.map vw pts
      path = wPath w
      vecs = wVecs w
      n = V.length path
      k = wK w
      -- 経路
      pathPts = [vw (lift 2 (path V.! (i `mod` n))) | i <- [0 .. n]]
      pathIt = pathItems (hsba 0 0 100 0.8) 2.5 pathPts
      arrowAt p v l hue sat wd =
        let q = lift 3 p
            tip = add3 q (scale3 l v)
            col = hsb hue sat 100
        in ball col (wd + 1.5) (vw tip) : longSeg 4 col wd (vw q) (vw tip)
      -- これまで運んだ矢印の軌跡
      trail = concat [arrowAt (path V.! i) (vecs V.! i) 34 45 55 1.5 | i <- [0, 15 .. k - 1], i < n]
      items = meshItems 0.33 vs faces ++ pathIt ++ trail
           ++ arrowAt (path V.! 0) (vecs V.! 0) 48 0 0 3                 -- 最初の矢印（白）
           ++ arrowAt (path V.! (k `mod` n)) (vecs V.! min k n) 48 45 95 3.5 -- 今の矢印（黄）
  in Pictures [paint items, hud env w]

degrees :: Float -> Float
degrees r = r * 180 / pi

hud :: Env -> World -> Picture
hud env w =
  let h = envH env
      n = V.length (wPath w)
      n0 = unit3 (wPath w V.! 0)
      v0 = wVecs w V.! 0
      vf = wVecs w V.! n
      -- ホロノミー（戻ってきた矢印と最初の矢印の角度）
      ang = atan2 (dot3 (cross3 v0 vf) n0) (dot3 v0 vf)
      measured = wrapF tau ang
      predicted = if wMode w == 1 then tau * (1 - sin (wPhi w)) else wAng w      -- 囲んだ領域の面積 / R²
      g = hsb 0 0 65
  in Pictures
       [ txt env 20 40 18 white ("1: latitude  2: geodesic triangle  " ++ (if wMode w == 1 then "Up/Down: phi = " ++ nf 0 (degrees (wPhi w)) ++ " deg" else "<- ->: alpha = " ++ nf 0 (degrees (wAng w)) ++ " deg"))
       , txt env 20 64 16 white ("Space: " ++ (if wPaused w then "resume" else "pause") ++ "   R: restart   drag: view")
       , txt env 20 88 14 g "white = first arrow   yellow = transported arrow (just drop the normal component)"
       , txt env 20 108 14 g ("orange = enclosed region   progress " ++ nf 0 (100 * fromIntegral (min (wK w) n) / fromIntegral n) ++ "%")
       , box env 20 (h - 150) 1100 126 (hsba 0 0 100 0.08)
       , txt env 36 (h - 116) 18 white ("Rotation of the returned arrow = " ++ nf 1 (degrees measured) ++ " deg" ++ (if wK w >= n then "" else "  (still going)"))
       , txt env 36 (h - 86) 18 white ("Area / R^2 = " ++ (if wMode w == 1 then "2 pi (1 - sin phi) = " else "angle sum - pi = alpha = ") ++ nf 3 predicted ++ " rad = " ++ nf 1 (degrees predicted) ++ " deg")
       , txt env 36 (h - 52) 14 g "Gauss-Bonnet: rotation = integral of K dA = area / R^2 (sphere: K = 1/R^2). Flat paper gives 0 deg."
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> rebuild w { wMode = fromEnum c - fromEnum '0' }
    | c == 'r' || c == 'R' -> w { wK = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 緯線（1）で φ = 0（赤道）にすると回転角は 0°。赤道は測地線だから。φ を上げると回転角はどう増える？
-- 2. 三角形（2）の α を 90° にすると、3 つの角がすべて直角の三角形。内角の和は 270°、回転角は 90°
-- 3. radius を変えても回転角は同じ（面積 / R² が同じ）。確かめよう
-- 4. 平行移動の式 v − (v·n)n を「v をいつも経路の接線に合わせる」に変えると何が起きる？（それは平行移動ではない）

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
