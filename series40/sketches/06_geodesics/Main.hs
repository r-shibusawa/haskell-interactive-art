-- ============================================
-- 40-06 測地線 ─ 曲面の上の「まっすぐ」
-- 測地線 = 曲面に沿って進むとき、横に曲がらない（加速度が曲面の法線方向だけ）道。曲面の上では最短経路になる
--   曲面を F(p) = 0 で表し、p'' = −(v·∇²F·v / |∇F|²) ∇F を RK4 で数値積分（速さ 1 を保つ）
--   球面の測地線は大円（中心を通る平面で切った円）。トーラスでは巻きついたり、内側と外側を行き来したりする
-- 操作：クリック = 曲面の上の出発点 / ←→ = 出発の向き / スペース = 1 本打つ / N = 12 本の扇 / ↑↓ = 長さ / C = 消す / 1〜3 = 曲面 / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

maxLen0 :: Float
maxLen0 = 1400            -- ★ 測地線の長さ（px）

ds :: Double
ds = 2                    -- ★ RK4 の 1 歩（px）

scaleF :: Double
scaleF = 150

segBudget :: Int
segBudget = 200           -- ★ 測地線 1 本を描く線分の上限（多いと重くなる）

-- | 測地線：曲面から少し浮かせた点の列、描く折れ線、色相、生まれたフレーム
data Geo = Geo
  { gPts  :: !(V.Vector V3)
  , gLine :: !(V.Vector V3)
  , gHue  :: !Float
  , gBorn :: !Int }

data World = World
  { wSurf    :: !Int
  , wGeos    :: ![Geo]              -- 古い順
  , wStart   :: !P3
  , wTheta   :: !Float
  , wMaxLen  :: !Float
  , wRx, wRy :: !Float
  , wPrev    :: !Point
  , wMoved   :: !Bool
  , wFrame   :: !Int
  , wMesh    :: !Mesh }

main :: IO ()
main = runSketch (sketch "40-06 Geodesics" (build 1 world0))
  { skBack  = hsb 220 30 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 [] (P3 0 0 0) 0.6 maxLen0 (-0.5) 0.6 (0, 0) False 0 (Mesh V.empty [])

-- ★ 曲面のカタログ：陰関数 F(p) = 0（外側で +、数学の座標 z が上）
surfF :: Int -> Double -> Double -> Double -> Double
surfF no x y z = case no of
  1 -> x * x + y * y + z * z - 1.3 * 1.3
  2 -> let d = sqrt (x * x + y * y) - 1.25 in d * d + z * z - 0.55 * 0.55
  _ -> let (a, b, c) = (x / 1.8, y / 1.1, z / 0.85) in a * a + b * b + c * c - 1

-- ★ パラメータ表示（描画用）：f(u, v)
surfP :: Int -> Double -> Double -> P3
surfP no u v = case no of
  1 -> P3 (1.3 * cos u * cos v) (1.3 * sin u * cos v) (1.3 * sin v)
  2 -> P3 ((1.25 + 0.55 * cos v) * cos u) ((1.25 + 0.55 * cos v) * sin u) (0.55 * sin v)
  _ -> P3 (1.8 * cos u * cos v) (1.1 * sin u * cos v) (0.85 * sin v)

-- v の範囲
surfV :: Int -> (Double, Double)
surfV no = if no == 2 then (0, 2 * pi) else (-pi / 2 + 0.005, pi / 2 - 0.005)

surfName :: Int -> String
surfName no = case no of { 1 -> "Sphere"; 2 -> "Torus"; _ -> "Ellipsoid" }

-- 数学の座標（z が上）→ p5 の座標
toP5 :: P3 -> P3
toP5 (P3 x y z) = P3 (x * scaleF) (negate z * scaleF) (y * scaleF)

-- p5 の座標での F
fp5 :: Int -> P3 -> Double
fp5 no (P3 x y z) = surfF no (x / scaleF) (z / scaleF) (negate y / scaleF)

-- ∇F（差分）
gradF :: Int -> P3 -> P3
gradF no (P3 x y z) =
  let h = 0.5
      d a b = (a - b) / (2 * h)
  in P3 (d (fp5 no (P3 (x + h) y z)) (fp5 no (P3 (x - h) y z)))
        (d (fp5 no (P3 x (y + h) z)) (fp5 no (P3 x (y - h) z)))
        (d (fp5 no (P3 x y (z + h))) (fp5 no (P3 x y (z - h))))

orEps :: Double -> Double
orEps v = if v == 0 then 1e-9 else v

-- 曲面の上に戻す（ニュートン法を 3 回）
onSurface :: Int -> P3 -> P3
onSurface no p0 = once (once (once p0))
  where once p = let g = gradF no p
                     g2 = orEps (pdot g g)
                 in psub p (pscale (fp5 no p / g2) g)

punit :: P3 -> P3
punit a = let l = sqrt (pdot a a) in pscale (1 / (if l == 0 then 1 else l)) a

-- 測地線の加速度：曲面の法線方向に、曲面から離れないだけの分
accel :: Int -> P3 -> P3 -> P3
accel no p v =
  let g = gradF no p
      g2 = orEps (pdot g g)
      h = 0.5
      d2 = (fp5 no (padd p (pscale h v)) - 2 * fp5 no p + fp5 no (psub p (pscale h v))) / (h * h)
  in pscale (negate (d2 / g2)) g

isGood :: P3 -> Bool
isGood (P3 x y z) = not (isNaN x || isInfinite x || isNaN y || isNaN z)

-- RK4 で測地線を maxLen だけ積分（各歩で曲面に戻し、速さを 1 にそろえる）
integrate :: Int -> Float -> P3 -> P3 -> [P3]
integrate no maxLen p0 v0 = p0 : go (floor (realToFrac maxLen / ds) :: Int) p0 v0
  where
    go :: Int -> P3 -> P3 -> [P3]
    go 0 _ _ = []
    go n !p !v =
      let k1v = accel no p v
          k1p = v
          p2 = padd p (pscale (0.5 * ds) k1p)
          v2 = padd v (pscale (0.5 * ds) k1v)
          k2v = accel no p2 v2
          k2p = v2
          p3 = padd p (pscale (0.5 * ds) k2p)
          v3 = padd v (pscale (0.5 * ds) k2v)
          k3v = accel no p3 v3
          k3p = v3
          p4 = padd p (pscale ds k3p)
          v4 = padd v (pscale ds k3v)
          k4v = accel no p4 v4
          k4p = v4
          sum4 a b c d = padd a (padd (pscale 2 b) (padd (pscale 2 c) d))
          p' = onSurface no (padd p (pscale (ds / 6) (sum4 k1p k2p k3p k4p)))
          v' = padd v (pscale (ds / 6) (sum4 k1v k2v k3v k4v))
          nrm = punit (gradF no p')
          v'' = punit (psub v' (pscale (pdot v' nrm) nrm))
      in if isGood p' && isGood v'' then p' : go (n - 1) p' v'' else []

-- 測地線を 1 本作る
shoot :: Int -> Float -> Int -> P3 -> P3 -> Float -> Geo
shoot no maxLen frame p0 dir hue =
  let pts = integrate no maxLen p0 dir
      -- 曲面から少し浮かせて描く
      lifted = V.fromList (map (\p -> toV3 (padd p (pscale 2.5 (punit (gradF no p))))) pts)
      n = V.length lifted
      stride = max 2 (n `div` segBudget + 1)
      thin = V.fromList [lifted V.! i | i <- [0, stride .. n - 1]]
  in thin `seq` Geo lifted thin hue frame

-- 出発点での接ベクトル（向き th）
startDir :: Int -> P3 -> Float -> P3
startDir no p th =
  let n@(P3 _ ny _) = punit (gradF no p)
      ref = if abs ny < 0.9 then P3 0 1 0 else P3 1 0 0
      e1 = punit (pcross n ref)
      e2 = pcross n e1
      t = realToFrac th
  in padd (pscale (cos t) e1) (pscale (sin t) e2)

launch :: Float -> Float -> World -> World
launch th hue w =
  let g = shoot (wSurf w) (wMaxLen w) (wFrame w) (wStart w) (startDir (wSurf w) (wStart w) th) hue
      old = if length (wGeos w) > 24 then drop 1 (wGeos w) else wGeos w
  in w { wGeos = old ++ [g] }

build :: Int -> World -> World
build no w =
  let nu = 48
      nv = 32
      (v0, v1) = surfV no
      pos i j = toV3 (toP5 (surfP no (2 * pi * fromIntegral i / fromIntegral nu) (v0 + (v1 - v0) * fromIntegral j / fromIntegral nv)))
      col _ _ = (0.58, 0.64, 0.8)
  in w { wSurf = no, wMesh = gridMesh nu nv pos col, wGeos = []
       , wStart = onSurface no (toP5 (surfP no 0.9 (if no == 2 then 0.8 else 0.5))) }

step :: Env -> World -> World
step env w =
  let key k = isDown env (SpecialKey k)
      th = wTheta w + (if key KeyRight then 0.03 else 0) - (if key KeyLeft then 0.03 else 0)
      l1 = if key KeyUp then min 4000 (wMaxLen w + 20) else wMaxLen w
      l2 = if key KeyDown then max 200 (l1 - 20) else l1
  in w { wTheta = th, wMaxLen = l2, wFrame = wFrame w + 1 }

draw :: Env -> World -> Picture
draw env w =
  let Mesh pts faces = wMesh w
      vw = view (wRx w) (wRy w)
      vs = V.map vw pts
      geoItems g =
        let col = hsb (gHue g) 85 100
            n = V.length (gPts g)
            -- 点が測地線の上を走る
            tip = if n == 0 then [] else [ball col 4 (vw (gPts g V.! (((wFrame w - gBorn g) * 3) `mod` n)))]
        in tip ++ pathItems col 2.5 (map vw (V.toList (gLine g)))
      start = toV3 (wStart w)
      dirV = toV3 (startDir (wSurf w) (wStart w) (wTheta w))
      arrowItems = longSeg 5 (hsb 45 95 100) 3 (vw start) (vw (add3 start (scale3 50 dirV)))
      items = meshItems 0.33 vs faces ++ concatMap geoItems (wGeos w) ++ arrowItems ++ [ball white 6 (vw start)]
  in Pictures [paint items, hud env w]

hud :: Env -> World -> Picture
hud env w =
  let g = hsb 0 0 65
      deg = wTheta w * 180 / pi
      remDeg = deg - 360 * fromIntegral (truncate (deg / 360) :: Int)
      note = case wSurf w of
        1 -> "On a sphere geodesics are always great circles and come back to the start"
        2 -> "On a torus they wind around, dive inside, and often never close"
        _ -> "On an ellipsoid non-closing geodesics fill a band"
  in Pictures
       [ txt env 20 40 16 white ("1-3: surface (" ++ surfName (wSurf w) ++ ")   click: start point   <- ->: direction " ++ nf 0 remDeg ++ " deg")
       , txt env 20 62 16 white ("Space: shoot one   N: fan of 12   Up/Down: length " ++ nf 0 (wMaxLen w) ++ "   C: clear   drag: view")
       , txt env 20 86 14 g "yellow arrow = start direction   a geodesic goes on without turning the steering wheel"
       , txt env 20 106 14 g note
       , txt env 20 126 14 g (show (length (wGeos w)) ++ " geodesics   p'' = -(v.Hess(F).v / |grad F|^2) grad F, RK4 (step " ++ nf 0 (realToFrac ds) ++ " px)")
       ]

-- 画面（カメラ）の座標 → 物体の座標（rotateX, rotateY の逆）
invRot :: Float -> Float -> P3 -> P3
invRot rxF ryF (P3 x y z) =
  let rx = realToFrac rxF
      ry = realToFrac ryF
      (cy, sy, cx, sx) = (cos ry, sin ry, cos rx, sin rx)
      y1 = cx * y + sx * z
      z1 = negate sx * y + cx * z
  in P3 (cy * x - sy * z1) y1 (sy * x + cy * z1)

-- クリック：マウスの光線が曲面にぶつかる点を探す（F の符号が変わる所を二分法で）
pickSurface :: World -> Point -> Maybe P3
pickSurface w (mx, my) =
  let ez = realToFrac camZ :: Double
      no = wSurf w
      o = invRot (wRx w) (wRy w) (P3 0 0 ez)
      -- gloss の座標（中央が原点、y は上向き）→ カメラの座標（y は下向き）
      dir = punit (invRot (wRx w) (wRy w) (P3 (realToFrac mx) (negate (realToFrac my)) (negate ez)))
      at' t = padd o (pscale t dir)
      search :: Double -> Double -> Maybe (Double, Double)
      search tp fp
        | t >= ez + 450 = Nothing
        | fp > 0 && f <= 0 = Just (tp, t)
        | otherwise = search t f
        where t = tp + 3
              f = fp5 no (at' t)
      refine :: Int -> Double -> Double -> Double
      refine 0 a b = (a + b) / 2
      refine k a b = let m = (a + b) / 2
                     in if fp5 no (at' m) > 0 then refine (k - 1) m b else refine (k - 1) a m
      t0 = ez - 450
  in case search t0 (fp5 no (at' t0)) of
       Just (a, b) -> Just (onSurface no (at' (refine 20 a b)))
       Nothing -> Nothing

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos, wMoved = False }
  EventKey (MouseButton LeftButton) Up _ pos
    | wMoved w -> w
    | otherwise -> case pickSurface w pos of
        Just p -> launch (wTheta w) (nextHue w) w { wStart = p }
        Nothing -> w
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos, wMoved = True }
    | otherwise -> w { wPrev = pos }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> build (fromEnum c - fromEnum '0') w
    | c == 'n' || c == 'N' -> foldl (\wa k -> launch (wTheta w + fromIntegral k * tau / 12) (fromIntegral ((k * 30) `mod` 360)) wa) w [0 .. 11 :: Int]
    | c == 'c' || c == 'C' -> w { wGeos = [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> launch (wTheta w) (nextHue w) w
  _ -> w
  where nextHue wd = fromIntegral ((length (wGeos wd) * 47) `mod` 360)

-- ---------- やってみよう ----------
-- 1. 球面（1）で N を押すと 12 本の大円が反対側の 1 点（対蹠点）で全部交わる。確かめよう
-- 2. トーラス（2）の外側から内側へ向かう測地線は、内側で「跳ね返る」ものと、穴をくぐるものがある。向き（←→）で分けてみよう
-- 3. 楕円体（3）の測地線はほとんど閉じない。長さを 4000 にして、どの帯に閉じ込められるか見よう（クレローの関係）
-- 4. surfF と surfP に自分の曲面を足そう（F(p) = 0 の形で書けるもの。例：z − (x² − y²)/2 の鞍）

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
-- ここから下は「差分の道具」です（差分は桁落ちしやすいので Double で計算する）
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
