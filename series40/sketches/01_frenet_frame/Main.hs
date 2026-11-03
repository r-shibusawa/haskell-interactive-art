-- ============================================
-- 40-01 空間曲線のフレネ標構（T, N, B）
-- 曲線の上を動く「小さな座標系」。T = 進む向き（接線）、N = 曲がる向き（主法線）、B = T × N（従法線）
--   曲率 κ = |r' × r''| / |r'|³ …… どれだけ曲がるか（接触円の半径は 1/κ）
--   捩率 τ = (r' × r'')·r''' / |r' × r''|² …… 平面から「ねじれて」飛び出す速さ（平面曲線なら 0）
-- 操作：1〜5 = 曲線 / スペース = 一時停止 / ←→ = 速さ / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

speed0 :: Float
speed0 = 0.006             -- ★ 曲線の上を動く速さ

scaleF :: Double
scaleF = 130               -- ★ 曲線の大きさ（px）

names :: [String]
names = ["Helix", "Trefoil knot", "(3,5) torus knot", "Viviani curve (sphere & cylinder)", "Twisted figure 8"]

-- | 1 点のフレネ標構
data Frame = Frame { frP, frT, frN, frB :: !V3, frK, frTau :: !Float }

data World = World
  { wCurve  :: !Int
  , wT      :: !Float
  , wSpeed  :: !Float
  , wPaused :: !Bool
  , wTrail  :: ![(V3, V3)]            -- 主法線の軌跡（点と N、新しい順、最大 90 個）
  , wRx     :: !Float
  , wRy     :: !Float
  , wPrev   :: !Point
  , wPoly   :: !(V.Vector (V3, Float)) -- 曲線の折れ線（点と κ）。曲線を変えたときだけ計算する
  , wGraph  :: ![(Float, Float, Float)] -- 一周ぶんの (s, κ, τ)
  }

main :: IO ()
main = runSketch (sketch "40-01 Frenet frame" (setCurve 1 world0))
  { skBack  = hsb 230 30 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 0 speed0 False [] (-0.5) 0.6 (0, 0) V.empty []

-- ★ 曲線 r(s)（s は 0〜1 で一周）。戻り値は px
curvePt :: Int -> Double -> P3
curvePt no s =
  let a = s * 2 * pi
  in case no of
       1 -> pscale scaleF (P3 (1.2 * cos (2 * a)) (0.4 * (s - 0.5) * 6) (1.2 * sin (2 * a)))     -- らせん（2 巻き）
       2 -> pscale (scaleF * 0.5) (P3 (sin a + 2 * sin (2 * a)) (cos a - 2 * cos (2 * a)) (negate (sin (3 * a))))
       3 -> let r = 2 + cos (5 * a) in pscale (scaleF * 0.55) (P3 (r * cos (3 * a)) (r * sin (3 * a)) (sin (5 * a)))
       4 -> let b = 2 * a in pscale (scaleF * 0.8) (P3 (1 + cos b) (sin b) (2 * sin (b / 2)))
       _ -> pscale scaleF (P3 (1.6 * sin a) (0.9 * sin (2 * a)) (0.9 * cos (3 * a)))

-- 差分で r', r'', r''' を求め、フレネ標構と κ, τ を返す
frenet :: Int -> Double -> Frame
frenet no s =
  let d = 0.002
      p = curvePt no s
      p1 = curvePt no (s + d)
      m1 = curvePt no (s - d)
      p2 = curvePt no (s + 2 * d)
      m2 = curvePt no (s - 2 * d)
      v1 = pscale (1 / (2 * d)) (psub p1 m1)                                             -- r'
      v2 = pscale (1 / (d * d)) (padd (psub p1 (pscale 2 p)) m1)                         -- r''
      v3 = pscale (1 / (2 * d * d * d)) (psub (padd (psub p2 (pscale 2 p1)) (pscale 2 m1)) m2) -- r'''
      c = pcross v1 v2
      cl = sqrt (pdot c c)
      vl0 = sqrt (pdot v1 v1)
      vl = if vl0 == 0 then 1 else vl0
      t = pscale (1 / vl) v1
      b = if cl > 1e-9 then pscale (1 / cl) c else P3 0 0 1
      n = pcross b t
      kappa = cl / (vl * vl * vl)
      tor = if cl > 1e-9 then pdot c v3 / (cl * cl) else 0
  in Frame (toV3 p) (toV3 t) (toV3 n) (toV3 b) (realToFrac kappa) (realToFrac tor)

-- 曲線を選ぶ：折れ線とグラフを計算しておく
setCurve :: Int -> World -> World
setCurve no w =
  let n = 400 :: Int
      poly = V.fromList [ (toV3 (curvePt no s), frK (frenet no (s - 0.5 / fromIntegral n)))
                        | i <- [0 .. n], let s = fromIntegral i / fromIntegral n ]
      gr = [ (fromIntegral i / 120, frK g * sf, frTau g * sf)
           | i <- [0 .. 120 :: Int], let g = frenet no (fromIntegral i / 120) ]
  in w { wCurve = no, wTrail = [], wPoly = poly, wGraph = gr }

sf :: Float
sf = realToFrac scaleF

step :: Env -> World -> World
step env w =
  let t1 = if wPaused w then wT w else fract (wT w + wSpeed w)
      s1 = if isDown env (SpecialKey KeyLeft) then max 0.0005 (wSpeed w * 0.97) else wSpeed w
      s2 = if isDown env (SpecialKey KeyRight) then min 0.03 (s1 * 1.03) else s1
      f = frenet (wCurve w) (realToFrac t1)
  in w { wT = t1, wSpeed = s2, wTrail = take 90 ((frP f, frN f) : wTrail w) }

draw :: Env -> World -> Picture
draw env w =
  let f = frenet (wCurve w) (realToFrac (wT w))
      vw = view (wRx w) (wRy w)
      -- 曲線（曲率で色：κ 大 = 赤、小 = 水色）
      poly = V.toList (V.map (\(p, k) -> (vw p, k)) (wPoly w))
      curveItems = zipWith (\(a, _) (b, k) -> seg (hsba (200 - min 1 (k * sf / 2.5) * 200) 70 100 0.85) 2.5 a b) poly (drop 1 poly)
      -- これまでの主法線の軌跡（曲がる向きが回っていく様子）
      len = length (wTrail w)
      trailItems = [ seg (hsba 130 70 100 (fromIntegral (len - 1 - i) / fromIntegral (max len 1) * 0.45)) 1 (vw p) (vw (add3 p (scale3 28 nv)))
                   | (i, (p, nv)) <- zip [0 ..] (wTrail w) ]
      -- 接触円（半径 1/κ、T-N 平面の中、中心は p + N/κ）
      r = 1 / max (frK f) 1e-6
      c = add3 (frP f) (scale3 r (frN f))
      oscPts = [ vw (add3 c (scale3 r (add3 (scale3 (cos a) (frT f)) (scale3 (sin a) (frN f)))))
               | i <- [0 .. 64 :: Int], let a = fromIntegral i / 64 * tau ]
      oscItems = if r < 900 then pathItems (hsba 45 90 100 0.6) 1.5 oscPts else []
      -- 標構の矢印
      arrow3 d hue = let tip = add3 (frP f) (scale3 70 d) in
        ball (hsb hue 80 100) 4 (vw tip) : longSeg 8 (hsb hue 80 100) 3 (vw (frP f)) (vw tip)
      arrows = arrow3 (frT f) 0 ++ arrow3 (frN f) 130 ++ arrow3 (frB f) 215
      scene = paint (curveItems ++ trailItems ++ oscItems ++ arrows ++ [ball white 6 (vw (frP f))])
  in Pictures [scene, hud env w f r]

hud :: Env -> World -> Frame -> Float -> Picture
hud env w f r =
  let h = envH env
      kNow = frK f * sf
      tNow = frTau f * sf
      gx = envW env - 420
      gy = h - 150
      gw = 400
      gh = 120
      plot sel = [ (gx + s * gw, gy + gh / 2 - clampF (-1) 1 (sel k t / 3) * gh * 0.45) | (s, k, t) <- wGraph w ]
      nameNow = case drop (wCurve w - 1) names of { (s : _) -> s; [] -> "" }
  in Pictures
       [ txt env 20 40 18 white ("1-5: curve (" ++ nameNow ++ ")   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   <- ->: speed   drag: view")
       , txt env 20 66 14 (hsb 0 0 65) "red T = tangent   green N = principal normal   blue B = binormal   yellow = osculating circle (radius 1/k)"
       , txt env 20 86 14 (hsb 0 0 65) "faint green lines = history of N"
       , box env 20 (h - 120) 480 96 (hsba 0 0 100 0.08)
       , txt env 36 (h - 86) 20 white ("k = " ++ nf 3 kNow ++ "  (1/k = " ++ nf 0 r ++ " px)")
       , txt env 36 (h - 50) 20 white ("tau = " ++ nf 3 tNow ++ (if abs tNow < 0.02 then "  (~ 0: planar)" else if frTau f > 0 then "  (right-handed)" else "  (left-handed)"))
       -- κ・τ の小さなグラフ（曲線一周ぶん）
       , box env gx gy gw gh (hsba 0 0 100 0.08)
       , path2 env (hsb 0 70 100) 2 (plot (\k _ -> k))
       , path2 env (hsb 215 70 100) 2 (plot (\_ t -> t))
       , seg2 env (hsb 45 90 100) 1 (gx + wT w * gw, gy) (gx + wT w * gw, gy + gh)
       , txt env (gx + 8) (gy + 18) 14 (hsb 0 0 60) "one lap: k (red) and tau (blue)"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '5' -> setCurve (fromEnum c - fromEnum '0') w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. らせん（1）は κ も τ も一定。グラフが平らなのを確かめよう。半径や巻き数を変えると κ と τ はどう変わる？
-- 2. ビビアニ曲線（4）で τ の符号が変わる所を探そう。そこでは B がひっくり返りそうになる
-- 3. curvePt に自分の曲線を足してみよう（例：P3 (cos a) (sin a) 0 の平面曲線 → τ = 0 になる？）
-- 4. 接触円の半径 1/κ が大きい所と小さい所で、曲線の見た目がどう違うか

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
