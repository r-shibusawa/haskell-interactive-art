-- ============================================
-- 40-03 パラメータ曲面のギャラリー
-- 曲面は 2 つのパラメータ (u, v) から 3D の点を作る関数 r(u, v)。u, v を細かく動かして格子（メッシュ）にする
--   球面・トーラス・鞍・モンキーサドル・エンネパー・カテノイド・らせん面（後ろ 3 つは「極小曲面」）
--   右下の四角が (u, v) の平面。オレンジの線 = u 一定、水色の線 = v 一定。曲面の上ではこれが曲がって写っている
-- 操作：1〜7 = 曲面 / W = ワイヤー ↔ シェード / G = 格子線 / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

nu, nv :: Int
nu = 48                  -- ★ メッシュの分割数（大きいほどなめらか、重くなる）
nv = 48

scaleF :: Float
scaleF = 120             -- ★ 曲面の大きさ（px）

-- | 曲面：名前、式、u と v の範囲、f(u, v) → (x, y, z)（数学の座標、z が上）
data Surface = Surface
  { sName, sFormula :: String
  , sU0, sU1, sV0, sV1 :: !Float
  , sF :: Float -> Float -> V3 }

-- ★ 曲面のカタログ
surfaces :: [Surface]
surfaces =
  [ Surface "Sphere" "r(u,v) = (cos u cos v, sin u cos v, sin v)" 0 tau (-pi / 2 + 0.005) (pi / 2 - 0.005)
      (\u v -> V3 (1.4 * cos u * cos v) (1.4 * sin u * cos v) (1.4 * sin v))
  , Surface "Torus" "r(u,v) = ((R + r cos v) cos u, (R + r cos v) sin u, r sin v)" 0 tau 0 tau
      (\u v -> V3 ((1.3 + 0.55 * cos v) * cos u) ((1.3 + 0.55 * cos v) * sin u) (0.55 * sin v))
  , Surface "Saddle (hyperbolic paraboloid)" "r(u,v) = (u, v, (u^2 - v^2)/2)" (-1.5) 1.5 (-1.5) 1.5
      (\u v -> V3 u v (0.5 * (u * u - v * v)))
  , Surface "Monkey saddle" "r(u,v) = (u, v, (u^3 - 3uv^2)/2)" (-1.3) 1.3 (-1.3) 1.3
      (\u v -> V3 u v (0.5 * (u * u * u - 3 * u * v * v)))
  , Surface "Enneper surface (minimal)" "r(u,v) = (u - u^3/3 + uv^2, v - v^3/3 + vu^2, u^2 - v^2)" (-1.7) 1.7 (-1.7) 1.7
      (\u v -> V3 (0.45 * (u - u * u * u / 3 + u * v * v)) (0.45 * (v - v * v * v / 3 + v * u * u)) (0.45 * (u * u - v * v)))
  , Surface "Catenoid (minimal)" "r(u,v) = (cosh v cos u, cosh v sin u, v)" 0 tau (-1.3) 1.3
      (\u v -> V3 (0.8 * cosh v * cos u) (0.8 * cosh v * sin u) (0.8 * v))
  , Surface "Helicoid (minimal)" "r(u,v) = (v cos u, v sin u, c u)" (-tau) tau (-1.3) 1.3
      (\u v -> V3 (0.8 * v * cos u) (0.8 * v * sin u) (0.3 * u))
  ]

data World = World
  { wSurf :: !Int
  , wWire, wGrid :: !Bool
  , wRx, wRy :: !Float
  , wPrev :: !Point
  , wMesh :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-03 Surface gallery" (build 1 world0))
  { skBack  = hsb 215 35 9
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 False True (-0.5) 0.6 (0, 0) (Mesh V.empty [])

surfOf :: Int -> Surface
surfOf no = case drop (no - 1) surfaces of
  (s : _) -> s
  [] -> Surface "" "" 0 1 0 1 (\_ _ -> V3 0 0 0)

-- (u, v) → p5 の座標 [x, −z, y]（p5 は y が下向きなので z を上にする）
ptAt :: Surface -> Int -> Int -> V3
ptAt s i j =
  let u = sU0 s + (sU1 s - sU0 s) * fromIntegral i / fromIntegral nu
      v = sV0 s + (sV1 s - sV0 s) * fromIntegral j / fromIntegral nv
      V3 x y z = sF s u v
  in V3 (x * scaleF) (negate z * scaleF) (y * scaleF)

build :: Int -> World -> World
build no w =
  let s = surfOf no
      col i _ = let (r, g, b, _) = rgbaOfColor (hsb (200 + fromIntegral i / fromIntegral nu * 120) 45 90) in (r, g, b)
  in w { wSurf = no, wMesh = gridMesh nu nv (ptAt s) col }

draw :: Env -> World -> Picture
draw env w =
  let Mesh pts faces = wMesh w
      vs = V.map (view (wRx w) (wRy w)) pts
      vert i j = vs V.! (i * (nv + 1) + j)
      wire = wWire w
      stepI = if wire then 2 else 4
      lw = if wire then 1.5 else 1
      al = if wire then 0.9 else 0.7
      uLines = [ seg (hsba 25 85 100 al) lw (vert i j) (vert i (j + 1)) | i <- [0, stepI .. nu], j <- [0 .. nv - 1] ]
      vLines = [ seg (hsba 190 80 100 al) lw (vert i j) (vert (i + 1) j) | j <- [0, stepI .. nv], i <- [0 .. nu - 1] ]
      items = (if wire then [] else meshItems 0.3 vs faces)
           ++ (if wGrid w || wire then uLines ++ vLines else [])
  in Pictures [paint items, hud env w]

hud :: Env -> World -> Picture
hud env w =
  let s = surfOf (wSurf w)
      (bx, by, bw, bh) = (envW env - 230, envH env - 230, 200, 200)
      fi k = fromIntegral (k :: Int) / 48
  in Pictures $
       [ txt env 20 40 18 white ("1-7: surface (" ++ show (wSurf w) ++ " " ++ sName s ++ ")   W: " ++ (if wWire w then "wire" else "shaded") ++ "   G: grid " ++ (if wGrid w then "ON" else "off") ++ "   drag: view")
       , txt env 20 66 14 (hsb 0 0 65) (sFormula s)
       , txt env 20 86 14 (hsb 0 0 65) ("u in [" ++ nf 2 (sU0 s) ++ ", " ++ nf 2 (sU1 s) ++ "]   v in [" ++ nf 2 (sV0 s) ++ ", " ++ nf 2 (sV1 s) ++ "]   " ++ show nu ++ " x " ++ show nv ++ " grid")
       -- パラメータ平面（u, v）
       , box env (bx - 10) (by - 10) (bw + 20) (bh + 20) (hsba 0 0 100 0.08) ] ++
       [ seg2 env (hsba 25 85 100 0.7) 1 (bx + fi i * bw, by) (bx + fi i * bw, by + bh) | i <- [0, 4 .. nu] ] ++
       [ seg2 env (hsba 190 80 100 0.7) 1 (bx, by + bh - fi j * bh) (bx + bw, by + bh - fi j * bh) | j <- [0, 4 .. nv] ] ++
       [ txt env (bx + bw - 34) (by + bh + 18) 14 (hsb 0 0 70) "u ->"
       , txt env (bx - 40) (by + 14) 14 (hsb 0 0 70) "v ^"
       , txt env bx (by - 16) 14 (hsb 0 0 70) "(u, v) parameter plane"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '7' -> build (fromEnum c - fromEnum '0') w
    | c == 'w' || c == 'W' -> w { wWire = not (wWire w) }
    | c == 'g' || c == 'G' -> w { wGrid = not (wGrid w) }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 球面（1）の北極・南極では u の線がぜんぶ集まる（パラメータの特異点）。格子線で確かめよう
-- 2. トーラス（2）の u, v の範囲をどちらも [0, π] にすると半分になる。「貼り合わせ」が見える
-- 3. surfaces に自分の曲面を足そう。例：(u, v, sin u cos v) の波の曲面、(u cos v, u sin v, u) の円錐
-- 4. カテノイド（6）とらせん面（7）は実は同じ曲面を「曲げた」もの（等長変形）。nu, nv を変えても形は同じ？

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
