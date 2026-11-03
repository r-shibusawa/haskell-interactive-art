-- ============================================
-- 40-15 カメラの明るさを高さにした曲面と、そのガウス曲率（カメラの許可が必要）
-- 映像を 48×36 に縮め、各画素の明るさを高さ z とする曲面 z = f(x, y)（モンジュのパッチ）を 3D に描く
--   K = (z_xx z_yy − z_xy²) / (1 + z_x² + z_y²)²　　H = ((1 + z_y²) z_xx − 2 z_x z_y z_xy + (1 + z_x²) z_yy) / (2 (1 + z_x² + z_y²)^{3/2})
--   明るい所は山（てっぺんは K > 0 赤）、山と谷の間の斜面は鞍（K < 0 青）。顔や手をかざすと曲率の地図になる
--   カメラが使えないときは、動くノイズの地形になる
-- 操作：K / H = 色分け / W = ワイヤー / ↑↓ = 高さの倍率 / B = 映像を表示 / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.List (sort)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nx, ny :: Int
nx = 48                  -- ★ 格子の分割（映像の縮小サイズ）
ny = 36

cs :: Float
cs = 13                  -- ★ 1 マスの大きさ（px）

blend :: Float
blend = 0.3              -- ★ 時間方向のなじませ

data World = World
  { wCam :: !(Maybe Camera)
  , wLive :: !Bool                -- 本物のカメラか
  , wVideo :: !B.ByteString       -- 最新の映像
  , wZ :: !(U.Vector Float)       -- 高さ（0〜1）
  , wAmp :: !Float                -- ★ 高さの倍率（↑↓）
  , wByK, wWire, wShowVideo :: !Bool
  , wFrame :: !Int
  , wRx, wRy :: !Float
  , wPrev :: !Point }

main :: IO ()
main = runSketch (sketch "40-15 Camera surface and curvature" world0)
  { skBack  = hsb 215 35 9
  , skStart = \_ -> do
      cam <- startCamera nx ny
      pure world0 { wCam = Just cam }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World Nothing False B.empty (U.replicate (nx * ny) 0) 160 True False True 0 (-0.8) 0 (0, 0)

ix :: Int -> Int -> Int
ix i j = i * ny + j

-- 3×3 のぼかし（端はそのまま）
blur :: U.Vector Float -> U.Vector Float
blur raw = U.generate (nx * ny) $ \k ->
  let (i, j) = k `divMod` ny
  in if i < 1 || i >= nx - 1 || j < 1 || j >= ny - 1 then raw U.! k
     else ( raw U.! ix (i - 1) (j - 1) + raw U.! ix (i - 1) j + raw U.! ix (i - 1) (j + 1)
          + raw U.! ix i (j - 1) + raw U.! k + raw U.! ix i (j + 1)
          + raw U.! ix (i + 1) (j - 1) + raw U.! ix (i + 1) j + raw U.! ix (i + 1) (j + 1) ) / 9

-- 高さの場を更新（カメラの明るさ、なければノイズ地形）。3×3 でぼかして時間方向にもなじませる
step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let key k = isDown env (SpecialKey k)
      a1 = if key KeyUp then min 400 (wAmp w0 + 3) else wAmp w0
      a2 = if key KeyDown then max 0 (a1 - 3) else a1
      frame = wFrame w0 + 1
  (live, video) <- case wCam w0 of
    Just cam -> do
      l <- camLive cam
      f <- if l then cameraFrame cam else pure B.empty
      pure (l, f)
    Nothing -> pure (False, B.empty)
  let t = fromIntegral frame * 0.01 :: Float
      noiseAt i j =
        let (fi, fj) = (fromIntegral i, fromIntegral j)
        in 0.5 + 0.25 * sin (fi * 0.25 + t * 1.3) * cos (fj * 0.3 - t) + 0.2 * sin ((fi + fj) * 0.18 + t * 0.7)
      value k =
        let (i, j) = k `divMod` ny
        in case wCam w0 of
             Just cam | live -> camGray cam video i j
             _ -> noiseAt i j
      smooth = blur (blur (U.generate (nx * ny) value))
      z' = U.zipWith (\z r -> z + (r - z) * blend) (wZ w0) smooth
  pure w0 { wAmp = a2, wFrame = frame, wLive = live, wVideo = video, wZ = z' }

-- 曲率（モンジュのパッチ）
curvatures :: World -> (U.Vector Float, U.Vector Float)
curvatures w =
  let z = wZ w
      amp = wAmp w
      one k =
        let (i, j) = k `divMod` ny
            i0 = max (i - 1) 0
            i1 = min (i + 1) (nx - 1)
            j0 = max (j - 1) 0
            j1 = min (j + 1) (ny - 1)
            zv = z U.! k
            zx = (z U.! ix i1 j - z U.! ix i0 j) * amp / (2 * cs)
            zy = (z U.! ix i j1 - z U.! ix i j0) * amp / (2 * cs)
            zxx = (z U.! ix i1 j - 2 * zv + z U.! ix i0 j) * amp / (cs * cs)
            zyy = (z U.! ix i j1 - 2 * zv + z U.! ix i j0) * amp / (cs * cs)
            zxy = (z U.! ix i1 j1 - z U.! ix i1 j0 - z U.! ix i0 j1 + z U.! ix i0 j0) * amp / (4 * cs * cs)
            g = 1 + zx * zx + zy * zy
            kk = (zxx * zyy - zxy * zxy) / (g * g)
            hh = ((1 + zy * zy) * zxx - 2 * zx * zy * zxy + (1 + zx * zx) * zyy) / (2 * g * sqrt g)
        in (kk, hh)
      both = U.generate (nx * ny) one
  in U.unzip both

draw :: Env -> World -> Picture
draw env w =
  let (kf, hf) = curvatures w
      field = if wByK w then kf else hf
      sorted = U.fromList (sort (map abs (U.toList field)))
      refI = floor (fromIntegral (U.length sorted) * (0.85 :: Float))
      ref = max (sorted U.! min refI (U.length sorted - 1)) 1e-6
      pos i j = V3 ((fromIntegral i - fromIntegral nx / 2) * cs) (negate (wZ w U.! ix i j) * wAmp w + 60) ((fromIntegral j - fromIntegral ny / 2) * cs)
      col i j = kColor (field U.! ix i j) ref
      Mesh pts faces = gridMesh (nx - 1) (ny - 1) pos col
      vs = V.map (view (wRx w) (wRy w)) pts
      vert i j = vs V.! ix i j
      wire = wWire w
      stepL = if wire then 2 else 4
      lc = hsba 0 0 100 (if wire then 0.75 else 0.2)
      lw = if wire then 1.2 else 0.7
      lines' = [ seg lc lw (vert i j) (vert i (j + 1)) | i <- [0, stepL .. nx - 1], j <- [0 .. ny - 2] ]
            ++ [ seg lc lw (vert i j) (vert (i + 1) j) | j <- [0, stepL .. ny - 1], i <- [0 .. nx - 2] ]
      items = (if wire then [] else meshItems 0.33 vs faces) ++ lines'
  in Pictures [paint items, hud env w]

hud :: Env -> World -> Picture
hud env w =
  let g = hsb 0 0 65
      name = if wByK w then "K" else "H"
      l2 = if wByK w
             then "bright = high. red = K > 0 (peaks, pits)  blue = K < 0 (saddles)  white = K ~ 0 (flat)"
             else "bright = high. red = H > 0 (hollow)  blue = H < 0 (bulge)  white = H ~ 0"
      (vx, vy, vw', vh) = (envW env - 212, 110, 192, 144)
      video = case wCam w of
        Just cam | wShowVideo w && wLive w && not (B.null (wVideo w)) ->
          let (cx, cy) = toG env (vx + vw' / 2, vy + vh / 2)
          in [ Translate cx cy (camImage cam (wVideo w) 4)
             , Color (hsb 45 90 100) (let (a, b) = toG env (vx + vw' / 2, vy + vh / 2) in rectLine a b vw' vh)
             , txt env vx (vy + vh + 18) 14 (hsb 0 0 70) ("video (" ++ show nx ++ "x" ++ show ny ++ ")") ]
        _ -> []
  in Pictures $
       [ txt env 20 40 16 white ("K / H: color (now " ++ (if wByK w then "Gauss K" else "mean H") ++ ")   W: wire   Up/Down: height x" ++ nf 0 (wAmp w) ++ "   B: video   drag: view")
       , txt env 20 62 16 white (if wLive w then "camera on" else "no camera -> noise terrain")
       , txt env 20 86 14 g l2
       , txt env 20 106 14 g ("color reference = 85th percentile of |" ++ name ++ "|")
       ] ++ video

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == 'k' || c == 'K' -> w { wByK = True }
    | c == 'h' || c == 'H' -> w { wByK = False }
    | c == 'w' || c == 'W' -> w { wWire = not (wWire w) }
    | c == 'b' || c == 'B' -> w { wShowVideo = not (wShowVideo w) }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 懐中電灯やスマホのライトをカメラに向けると鋭い山ができる。てっぺんは赤、まわりの斜面は青（鞍）になる
-- 2. 高さの倍率を 0 に近づけると全部白（平面）。高さを上げるほど曲率がはっきりする。式の分母 (1 + z_x² + z_y²) の役目は？
-- 3. 時間方向のなじませ blend を 1 にするとちらつく、0.05 にすると遅れる。曲率は 2 階微分なのでノイズに弱い
-- 4. 04 の曲面のカタログと見比べよう。山頂・谷底・鞍の 3 種類の点はどこにある？

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
