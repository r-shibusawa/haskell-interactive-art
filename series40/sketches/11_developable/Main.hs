-- ============================================
-- 40-11 可展面と地図 ─ 平らに広げられる曲面、広げられない曲面
-- 円柱・円錐・接線面は K = 0：伸ばしも縮めもせずに（距離を変えずに）平面に展開できる（可展面）
--   アニメーションの途中もすべて等長：格子の正方形の大きさが変わらないことに注目
-- 球面は K = 1/R² ≠ 0 なので展開できない（Gauss の驚異の定理）。無理に地図にすると距離や面積が歪む
--   → メルカトル図法：角は正しいが面積が 1/cos²φ 倍に（極に近いほど巨大に）
-- 操作：1〜4 = 曲面 / スペース = 広げる ↔ 巻く / ←→ = 手動で / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

ease :: Float
ease = 0.015              -- ★ 広がる速さ

names :: [String]
names = ["Cylinder", "Cone", "Tangent developable of a helix", "Sphere (not developable)"]

data World = World
  { wSurf :: !Int
  , wT, wTarget :: !Float        -- t = 0 で曲面、1 で平面
  , wRx, wRy :: !Float
  , wPrev :: !Point }

main :: IO ()
main = runSketch (sketch "40-11 Developable surfaces and maps" (World 1 0 1 (-0.5) 0.5 (0, 0)))
  { skBack  = hsb 210 30 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 内在座標 (s, w) と展開の進み具合 t から 3D の点（数学の座標、z が上）。どの t でも距離を保つ
devPt :: Int -> Float -> Float -> Float -> V3
devPt no s w t = case no of
  1 -> -- 円柱：曲率 k を (1−t)/R に減らす（弧長 s は保つ）
    let r = 110
        k = (1 - t) / r
    in if k < 1e-4 then V3 (s - pi * r) w r
       else V3 (sin (k * s) / k - t * pi * r) w ((cos (k * s) - 1) / k + r)
  2 -> -- 円錐：半頂角 α を広げる（扇形の角 σ = ψ sin α は保つ）
    let a0 = 0.5
        a = a0 + t * (pi / 2 - a0)
    in V3 (s * sin a * cos (w / sin a)) (s * sin a * sin (w / sin a)) (negate s * cos a + 120 * cos a)
  3 -> -- らせんの接線面：曲率 κ はそのまま、捩率 τ を 0 へ（内在距離は κ だけで決まる）
    let a0 = 60
        b0 = 30
        kap = a0 / (a0 * a0 + b0 * b0)
        tor = b0 / (a0 * a0 + b0 * b0) * (1 - t)
        a = kap / (kap * kap + tor * tor)
        b = tor / (kap * kap + tor * tor)
        c = sqrt (a * a + b * b)
        r1 = V3 (a * cos (s / c)) (a * sin (s / c)) (b * s / c)
        tv = V3 (negate a * sin (s / c) / c) (a * cos (s / c) / c) (b / c)
    in add3 (add3 r1 (scale3 w tv)) (V3 0 0 (negate b * 1.1 * pi))
  _ -> -- 球面（展開できない）：そのまま
    let r = 130 in V3 (r * cos w * cos s) (r * cos w * sin s) (r * sin w)

-- (s, w) の範囲と格子の分割
domain :: Int -> (Float, Float, Float, Float, Int, Int)
domain no = case no of
  1 -> (0, 2 * pi * 110, -120, 120, 48, 16)
  2 -> (40, 230, 0, 2 * pi * sin 0.5, 14, 40)
  3 -> (0, 2.2 * pi * sqrt (60 * 60 + 30 * 30), 0, 150, 48, 10)
  _ -> (0, 2 * pi, -1.45, 1.45, 48, 24)

areaColor :: Float -> (Float, Float, Float)
areaColor f = let v = min 1 (log f / log 30) in (0.95, 0.95 - 0.8 * v, 0.9 - 0.85 * v)

step :: Env -> World -> World
step env w0 =
  let key k = isDown env (SpecialKey k)
      w1 = if key KeyLeft then let v = max 0 (wT w0 - 0.01) in w0 { wT = v, wTarget = v } else w0
      w2 = if key KeyRight then let v = min 1 (wT w1 + 0.01) in w1 { wT = v, wTarget = v } else w1
  in if wSurf w2 < 4 then w2 { wT = wT w2 + (wTarget w2 - wT w2) * ease * 4 } else w2 { wT = 0 }

draw :: Env -> World -> Picture
draw env w =
  let no = wSurf w
      t = wT w
      (s0, s1, w0, w1, ns, nw) = domain no
      sAt i = s0 + (s1 - s0) * fromIntegral (i :: Int) / fromIntegral ns
      wAt j = w0 + (w1 - w0) * fromIntegral (j :: Int) / fromIntegral nw
      pos i j = let V3 x y z = devPt no (sAt i) (wAt j) t in V3 x (negate z) y
      col i j
        | no == 4 = areaColor (1 / max (cos (wAt j) * cos (wAt j)) 0.02)
        | odd (i `div` 4 + j `div` 4) = (0.55, 0.75, 0.95)
        | otherwise = (0.95, 0.85, 0.6)
      Mesh pts faces = gridMesh ns nw pos col
      vs = V.map (view (wRx w) (wRy w)) pts
      vert i j = vs V.! (i * (nw + 1) + j)
      lc = hsba 0 0 100 0.45
      lines' = [ seg lc 1 (vert i j) (vert i (j + 1)) | i <- [0, 4 .. ns], j <- [0 .. nw - 1] ]
            ++ [ seg lc 1 (vert i j) (vert (i + 1) j) | j <- [0, 4 .. nw], i <- [0 .. ns - 1] ]
  in Pictures [paint (meshItems 0.33 vs faces ++ lines'), hud env w]

hud :: Env -> World -> Picture
hud env w =
  let no = wSurf w
      name = case drop (no - 1) names of { (s : _) -> s; [] -> "" }
      g = hsb 0 0 65
      top = if no < 4
              then "Space: " ++ (if wTarget w >= 1 then "roll up" else "unroll") ++ "   <- ->: manual   unrolled " ++ nf 0 (wT w * 100) ++ "%"
              else "see the map on the right"
      (l1, l2) = case no of
        1 -> ("Cylinder: reduce the curvature. Every moment the grid squares keep their size (isometry), since K = 0", "")
        2 -> ("Cone: widen the tip angle to get a sector. Sector angle = 2 pi sin a (base circumference / slant length)", "")
        3 -> ("Tangent surface: tangents of a helix. Set the torsion to 0: a flat ring of tangents to a circle (same k)", "")
        _ -> ("Sphere has K != 0, so it cannot be flattened. Mercator keeps angles;", "areas grow by 1/cos^2 phi (redder = bigger). Why Greenland looks huge")
  in Pictures $
       [ txt env 20 40 16 white ("1-4: surface (" ++ name ++ ")   drag: view")
       , txt env 20 62 16 white top
       , txt env 20 86 14 g l1
       , txt env 20 106 14 g l2
       ] ++ [mercator env | no == 4]

-- メルカトル地図
mercator :: Env -> Picture
mercator env =
  let mx = envW env - 540
      my = 130
      mw = 400
      mh = 380
      lam = 1.45
      merc phi = log (tan (pi / 4 + phi / 2))
      yOf phi = my + mh / 2 - merc phi / merc lam * (mh / 2)
      latAt j = -lam + 2 * lam * fromIntegral (j :: Int) / 24
      strips = [ box env mx (yOf p1) mw (yOf p0 - yOf p1 + 0.5) (rgbC (areaColor (1 / max (cos pm * cos pm) 0.02)))
               | j <- [0 .. 23], let p0 = latAt j, let p1 = latAt (j + 1), let pm = (p0 + p1) / 2 ]
      lc = hsba 0 0 100 0.4
      vLines = [ seg2 env lc 1 (mx + x, my) (mx + x, my + mh) | i <- [0, 2 .. 24 :: Int], let x = fromIntegral i / 24 * mw ]
      hLines = [ seg2 env lc 1 (mx, yOf (latAt j)) (mx + mw, yOf (latAt j)) | j <- [0, 2 .. 24] ]
      tags = [ txt env (mx + mw + 12) (yOf phi + 4) 14 (hsb 0 0 70) (nf 0 (phi * 180 / pi) ++ " deg x" ++ nf 1 (1 / (cos phi * cos phi)))
             | phi <- [0, 0.5, 1.0, 1.3] ]
  in Pictures $
       [box env (mx - 10) (my - 10) (mw + 20) (mh + 20) (hsba 0 0 100 0.08)] ++ strips ++ vLines ++ hLines ++ tags ++
       [ txt env (mx + mw + 12) (yOf 0 + 22) 14 (hsb 0 0 70) "equator"
       , txt env mx (my + mh + 30) 14 white "Mercator: y = ln tan(pi/4 + phi/2)"
       , txt env mx (my + mh + 50) 14 white "color = area scale 1/cos^2 phi"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> w { wSurf = fromEnum c - fromEnum '0', wT = 0, wTarget = 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wTarget = if wTarget w == 1 then 0 else 1 }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 円錐（2）を展開した扇形の角は 2π sin α。α = 0.5 なら約 173°。扇形を見て確かめよう。α = π/6 なら半円
-- 2. 接線面（3）の途中の形はどれも同じ「内在的な形」。格子の正方形がずっと正方形なことを見よう
-- 3. 球面（4）の地図で、緯度 60° の正方形は赤道の何倍の面積に見える？（1/cos²60° = 4 倍）
-- 4. 円柱（1）の R を変えても展開図の幅は 2πR。R = 60 にして確かめよう。devPt にトーラスを足すと展開できる？（できない：K ≠ 0）

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
