-- ============================================
-- 07 平行移動とホロノミー
-- 矢印を「向きを変えないように」面に沿って運ぶ（平行移動）。平面なら 1 周しても元どおり
--   でも球面では、1 周して戻ると矢印が回っている！ 回った角度 = 囲んだ面積 × 曲率（半径 1 なら面積そのもの）
-- 1 = 緯線をまわる 2 = 測地三角形（北極 → 赤道 → 赤道 → 北極）/ ↑↓ = ループの大きさ / ←→ = 最初の矢印の向き / スペース = もう一度 / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector as V
import Numeric (showFFloat)

speedP :: Int
speedP = 3                  -- ★ アニメーションの速さ（1 フレームに進む点の数）

data World = World
  { wLoop :: !Int
  , wColat :: !D            -- ★ 緯線ループ：北極からの角度 θ0
  , wSweep :: !D            -- ★ 三角形ループ：赤道に沿って進む角度 α
  , wV0 :: !D               -- ★ 最初の矢印の向き
  , wProg :: !Int
  , wPath, wVecs :: !(V.Vector D3)
  , wHolo, wArea :: !D
  , wCap :: [Quad]
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-07 Parallel Transport" (rebuild (World 2 0.9 1.2 0.3 0 V.empty V.empty 0 0 [] cam0 { cRX = -0.6, cRY = -0.9 })))
  { skBack  = rgb255 14 16 26
  , skStep  = \_ _ w -> pure w { wProg = min (V.length (wPath w) - 1) (wProg w + speedP) }
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- 球面の点：u = 経度、v = 北極からの角度
sph :: D -> D -> D3
sph u v = D3 (sin v * cos u) (sin v * sin u) (cos v)

-- 面をつくるときは v を北極から南極へ逆にたどり、法線を外向きにする
sphOut :: Surf
sphOut u w = sph u (pi - w)

sphereMesh :: [Quad]
sphereMesh = makeSurface sphOut (0, 2 * pi) (0.001, pi - 0.001) (64, 40) (12, 12) (\_ _ -> (70 / 255, 90 / 255, 130 / 255, 1))

-- ループの点の列をつくる
makePath :: World -> [D3]
makePath w
  | wLoop w == 1 = [sph (fi i / 720 * 2 * pi) (wColat w) | i <- [0 .. 720]]
  | otherwise =
      -- 北極 → 経度 0 の子午線を赤道まで → 赤道を α まで → 子午線 α を北極まで（3 本とも大円 = 測地線）
      [sph 0 (0.0001 + fi i / 240 * (pi / 2 - 0.0001)) | i <- [0 .. 240]]
      ++ [sph (fi i / 240 * wSweep w) (pi / 2) | i <- [1 .. 240]]
      ++ [sph (wSweep w) (pi / 2 - fi i / 240 * (pi / 2 - 0.0001)) | i <- [1 .. 240]]
  where fi = fromIntegral :: Int -> D

rebuild :: World -> World
rebuild w =
  let path = V.fromList (makePath w)
      p0 = V.unsafeHead path
      e1 = vunit (vsub (path V.! 3) p0)
      e2 = vcross p0 e1
      v0 = vadd (vmul e1 (cos (wV0 w))) (vmul e2 (sin (wV0 w)))
      -- ★ 平行移動（離散版）：次の点の接平面に矢印を射影して、長さを 1 にもどす
      vecs = V.scanl' (\v n -> vunit (vsub v (vmul n (vdot v n)))) v0 (V.unsafeTail path)
      vf = V.unsafeLast vecs
      vi = V.unsafeHead vecs
      holo = atan2 (vdot (vcross vi vf) p0) (vdot vi vf)    -- 回った角度（法線まわり）
      area = if wLoop w == 1 then 2 * pi * (1 - cos (wColat w)) else wSweep w   -- 囲んだ面積（半径 1）
      -- 囲んだ部分を色ぬり
      lifted u t = vmul (sphOut u t) 1.004
      capCol _ _ = (1, 200 / 255, 90 / 255, 90 / 255)
      cap = if wLoop w == 1
              then makeSurface lifted (0, 2 * pi) (pi - wColat w, pi - 0.001) (48, 16) (0, 0) capCol
              else makeSurface lifted (0, wSweep w) (pi / 2, pi - 0.001) (32, 20) (0, 0) capCol
  in w { wPath = path, wVecs = vecs, wHolo = holo, wArea = area, wCap = cap, wProg = 0 }

event :: Event -> World -> World
event ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (Char '1') Down _ _ -> rebuild w { wLoop = 1 }
       EventKey (Char '2') Down _ _ -> rebuild w { wLoop = 2 }
       EventKey (SpecialKey KeyUp) Down _ _ ->
         rebuild (if wLoop w == 1 then w { wColat = min (pi - 0.1) (wColat w + 0.1) } else w { wSweep = min (2 * pi - 0.2) (wSweep w + 0.1) })
       EventKey (SpecialKey KeyDown) Down _ _ ->
         rebuild (if wLoop w == 1 then w { wColat = max 0.1 (wColat w - 0.1) } else w { wSweep = max 0.1 (wSweep w - 0.1) })
       EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wV0 = wV0 w - 0.3 }
       EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wV0 = wV0 w + 0.3 }
       EventKey (SpecialKey KeySpace) Down _ _ -> w { wProg = 0 }
       _ -> w

-- 矢印：長さ l、線の太さ w、先に丸
arrow3 :: View -> D3 -> D3 -> D -> Color -> Float -> [Item]
arrow3 view p v l c w =
  let a = vmul p 1.01
      b = vadd a (vmul v l)
  in segItem view c w (toV a) (toV b) ++ ballItem view c (w + 2) (toV b)

draw :: Env -> World -> Picture
draw env w =
  let c = wCam w
      view = mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.33)
      prog = wProg w
      surface = surfaceItems view (120 / 255) 0.83 True (rgb255 130 150 200) 0 sphereMesh
      capItems = surfaceItems view (120 / 255) 0.83 True black 3 (wCap w)
      -- 道（4 点に 1 本の線にまとめる）
      pathPts = [ toV (vmul (wPath w V.! i) 1.006) | i <- [0, 4 .. V.length (wPath w) - 1] ]
      pathLine = pathItems view (rgb255 255 230 120) 3 pathPts
      -- 運ばれた矢印（あと）
      trail = concat [ arrow3 view (wPath w V.! k) (wVecs w V.! k) 0.18 (rgb255 120 220 255) 2 | k <- [0, 30 .. prog] ]
      first' = arrow3 view (wPath w V.! 0) (wVecs w V.! 0) 0.3 white 3              -- 最初の矢印（白）
      now = arrow3 view (wPath w V.! prog) (wVecs w V.! prog) 0.3 (rgb255 255 90 120) 5  -- 今の矢印（赤）
  in Pictures [scene (surface ++ capItems ++ pathLine ++ trail ++ first' ++ now), drawHud env w]

drawHud :: Env -> World -> Picture
drawHud env w =
  let done = wProg w >= V.length (wPath w) - 1
      x = 20
      y = envH env - 190
      deg r = r * 180 / pi
      am = wrapD (deg (wArea w) + 180) - 180
      wrapD a = a - 360 * fromIntegral (floor (a / 360) :: Int) :: D
      loop1 = wLoop w == 1
  in Pictures
     [ txt env 20 34 16 white "1: latitude loop   2: geodesic triangle   U/D: size   L/R: first direction   Space: replay   Drag: view"
     , txt env 20 60 14 (gray 0.67) "White = start arrow, red = moving arrow, light blue = past. The arrow keeps direction inside each tangent plane."
     , box env (x - 10) (y - 30) 600 170 (withA (160 / 255) black)
     , txt env x y 16 white
         (if loop1 then "Latitude loop: angle from north pole th0 = " ++ nf 0 (deg (wColat w)) ++ " deg"
          else "Geodesic triangle: alpha along equator = " ++ nf 0 (deg (wSweep w)) ++ " deg (angle sum = 180 + alpha)")
     , txt env x (y + 32) 16 (rgb255 255 90 120)
         ("Turned after one loop = " ++ (if done then nf 1 (deg (wHolo w)) ++ " deg" else "(carrying...)"))
     , txt env x (y + 60) 16 (rgb255 255 200 90)
         ("Area / R^2 = " ++ nf 3 (wArea w) ++ " rad = " ++ nf 1 (deg (wArea w)) ++ " deg" ++ (if loop1 then "  (in -180..180: " ++ nf 1 am ++ ")" else ""))
     , txt env x (y + 90) 14 (gray 0.78)
         (if loop1 then "Cap area = 2 pi (1 - cos th0). Equator (th0 = 90): 2 pi = one full turn, looks unchanged"
          else "Triangle area = angle sum - 180 (see 08 Gauss-Bonnet). alpha = 90: 1/8 of the sphere")
     , txt env x (y + 114) 14 (gray 0.78) "Holonomy: turned angle = double integral of K dA. On a plane K = 0: no turn"
     ]

-- ---------- やってみよう ----------
-- 1. 三角形で α = 90° にする。矢印は何度回る？ 球面の 1/8 の面積（4π/8 = π/2）と比べよう
-- 2. 緯線ループで θ0 を小さく（北極のまわりの小さな円）。回る角度 ≈ 面積 = π θ0²（平らに近い）
-- 3. 最初の向き（←→）を変えても回る角度は同じ。回る角度はループだけで決まる
-- 4. 赤道（θ0 = 90°）は測地線。赤道に沿って運ぶと矢印は赤道に対して一定の角度を保つ。北の緯線では？

---------------------------------------------------------------- 道具
-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
px :: Env -> Float -> Float -> Point
px env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（p5 の text と同じく左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (a, b) = px env x y in label a b s c str

-- 左上が (x, y) の塗りつぶし長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (a, b) = px env (x + w / 2) (y + h / 2) in Color c (rect a b w h)

-- p5 の nf：小数点以下 d 桁の文字列
nf :: RealFloat a => Int -> a -> String
nf d x | isNaN x = "--"
       | isInfinite x = if x > 0 then "inf" else "-inf"
       | otherwise = showFFloat (Just d) x ""

degF :: Float -> Float
degF x = x * 180 / pi

-- JavaScript の % と同じ（符号は x に合わせる）
jsMod :: Float -> Float -> Float
jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)

---------------------------------------------------------------- 3D の道具（視点・ベクトル計算・面をつくる・画面への投影）
-- 計算は Double（差分の桁落ちを防ぐ）、描くときだけ Float の V3 に直す
type D = Double
data D3 = D3 !D !D !D

vadd, vsub, vcross :: D3 -> D3 -> D3
vadd (D3 a b c) (D3 d e f) = D3 (a + d) (b + e) (c + f)
vsub (D3 a b c) (D3 d e f) = D3 (a - d) (b - e) (c - f)
vcross (D3 a b c) (D3 d e f) = D3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

vmul :: D3 -> D -> D3
vmul (D3 a b c) s = D3 (a * s) (b * s) (c * s)

vdot :: D3 -> D3 -> D
vdot (D3 a b c) (D3 d e f) = a * d + b * e + c * f

vlen :: D3 -> D
vlen a = sqrt (vdot a a)

vunit :: D3 -> D3
vunit a = let l = vlen a in if l == 0 then a else vmul a (1 / l)

toV :: D3 -> V3
toV (D3 a b c) = V3 (realToFrac a) (realToFrac b) (realToFrac c)

f2 :: D -> Float
f2 = realToFrac

-- 視点の操作（ドラッグで回す）
data Cam = Cam { cRX :: !Float, cRY :: !Float, cDrag :: !Bool, cLast :: !Point }

cam0 :: Cam
cam0 = Cam (-0.5) 0.5 False (0, 0)

camEvent :: Event -> Cam -> Cam
camEvent ev c = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> c { cDrag = True, cLast = p }
  EventKey (MouseButton LeftButton) Up _ p -> c { cDrag = False, cLast = p }
  EventMotion p@(x, y)
    | cDrag c -> let (lx, ly) = cLast c
                 in c { cRY = cRY c + (x - lx) * 0.01
                      , cRX = clampF (-1.5) 1.5 (cRX c + (y - ly) * 0.01)
                      , cLast = p }
    | otherwise -> c { cLast = p }
  _ -> c

-- 視点：rotateX(rx); rotateY(ry) と同じ順で回す。数学の座標は z が上
data View = View !Float !Float !Float !Float !Float !Float

mkView :: Env -> Float -> Float -> Float -> View
mkView env rx ry sc = View (cos rx) (sin rx) (cos ry) (sin ry) (envH env / 2 / tan (pi / 6)) sc

-- 数学の座標 → 回したあとの p5 の座標（x 右、y 下、z 手前）
rotP :: View -> V3 -> V3
rotP (View cx sx cy sy _ _) (V3 a b c) =
  let y = -c
      x1 = a * cy + b * sy
      z1 = b * cy - a * sy
  in V3 x1 (y * cx - z1 * sx) (y * sx + z1 * cx)

-- 回したあとの点 → 画面の点と奥行き（大きいほど手前）
screenOf :: View -> V3 -> (Point, Float)
screenOf (View _ _ _ _ ez sc) (V3 x y z) =
  let d = z * sc
      s = ez / max 1 (ez - d)
  in ((x * sc * s, -(y * sc * s)), d)

look :: View -> V3 -> (Point, Float)
look v p = screenOf v (rotP v p)

-- 絵の部品：奥行きつき。奥から順に描く（画家のアルゴリズム）
type Item = (Float, Picture)

scene :: [Item] -> Picture
scene = Pictures . map snd . depthSort fst

-- 面より手前に見せたい線や点の下駄（px）
liftZ :: Float
liftZ = 10

nearOk :: View -> Float -> Bool
nearOk (View _ _ _ _ ez _) d = d < ez * 0.9

segItem :: View -> Color -> Float -> V3 -> V3 -> [Item]
segItem v c w a b =
  let (pa, za) = look v a
      (pb, zb) = look v b
  in [ ((za + zb) / 2 + liftZ, Color c (if w <= 1 then Line [pa, pb] else thickLine w pa pb))
     | nearOk v za && nearOk v zb ]

pathItems :: View -> Color -> Float -> [V3] -> [Item]
pathItems v c w ps = concat (zipWith (segItem v c w) ps (drop 1 ps))

-- p5 の sphere(r)：遠近で大きさが変わる丸
ballItem :: View -> Color -> Float -> V3 -> [Item]
ballItem v@(View _ _ _ _ ez _) c r p =
  let (q, z) = look v p
  in [ (z + liftZ + 1, at q (Color c (disc (r * ez / max 1 (ez - z))))) | nearOk v z ]

-- 面を四角形の集まりにする（4 隅、法線、色 r g b a、網目の辺）
data Quad = Quad !V3 !V3 !V3 !V3 !V3 !Float !Float !Float !Float [(V3, V3)]

type Surf = D -> D -> D3

normalAt :: Surf -> D -> D -> D3
normalAt f u v =
  let e = 0.001
  in vunit (vcross (vsub (f (u + e) v) (f (u - e) v)) (vsub (f u (v + e)) (f u (v - e))))

-- nu × nv の四角形。(gu, gv) 本の網目の線を、四角形の辺として持たせる
makeSurface :: Surf -> (D, D) -> (D, D) -> (Int, Int) -> (Int, Int) -> (D -> D -> (Float, Float, Float, Float)) -> [Quad]
makeSurface f (u0, u1) (v0, v1) (nu, nv) (gu, gv) col =
  [ Quad (toV a) (toV b) (toV c) (toV d) (toV (normalAt f um vm)) r g bl al edges
  | i <- [0 .. nu - 1], j <- [0 .. nv - 1]
  , let uu k = u0 + (u1 - u0) * fromIntegral k / fromIntegral nu
        vv k = v0 + (v1 - v0) * fromIntegral k / fromIntegral nv
        um = (uu i + uu (i + 1)) / 2
        vm = (vv j + vv (j + 1)) / 2
        a = f (uu i) (vv j)
        b = f (uu (i + 1)) (vv j)
        c = f (uu (i + 1)) (vv (j + 1))
        d = f (uu i) (vv (j + 1))
        (r, g, bl, al) = col um vm
        isU k = gu > 0 && k `elem` [(m * nu + gu `div` 2) `div` gu | m <- [0 .. gu]]
        isV k = gv > 0 && k `elem` [(m * nv + gv `div` 2) `div` gv | m <- [0 .. gv]]
        edges = [(toV a, toV d) | isU i] ++ [(toV b, toV c) | i == nu - 1 && isU nu]
             ++ [(toV a, toV b) | isV j] ++ [(toV d, toV c) | j == nv - 1 && isV nv]
  ]

-- 四角形を絵の部品に：環境光 amb + 平行光 dif、cull = True なら裏向きを描かない
surfaceItems :: View -> Float -> Float -> Bool -> Color -> Float -> [Quad] -> [Item]
surfaceItems v amb dif cull gridCol zb = concatMap one
  where
    lx = 0.3 / 1.2042
    ly = 0.6 / 1.2042
    lz = -1 / 1.2042
    View _ _ _ _ ez sc = v
    one (Quad a b c d n r g bl al es) =
      let ra@(V3 ax ay az) = rotP v a
          rb@(V3 bx by bz) = rotP v b
          rc@(V3 cx cy cz) = rotP v c
          rd@(V3 dx dy dz) = rotP v d
          V3 nx ny nz = rotP v n
          mx = (ax + bx + cx + dx) / 4
          my = (ay + by + cy + dy) / 4
          mz = (az + bz + cz + dz) / 4
          front = nz * (ez / sc - mz) - nx * mx - ny * my > 0
          (pa, _) = screenOf v ra
          (pb, _) = screenOf v rb
          (pc, _) = screenOf v rc
          (pd, _) = screenOf v rd
          lam = max 0 (-(nx * lx + ny * ly + nz * lz))
          k = amb + dif * lam
          z = mz * sc + zb
      in if cull && not front then [] else
         (z, Color (makeColor (r * k) (g * k) (bl * k) al) (Polygon [pa, pb, pc, pd]))
         : [ (z + 2, Color gridCol (Line (concat [[fst (look v e1), fst (look v e2)] | (e1, e2) <- es]))) | not (null es), length es == 1 ]
        ++ [ (z + 2, Color gridCol (Pictures [Line [fst (look v e1), fst (look v e2)] | (e1, e2) <- es])) | length es > 1 ]

-- 画面の点 m に見えている面の点 (u, v) を探す（なければ Nothing）
pickUV :: View -> Surf -> Point -> (D, D) -> (D, D) -> Maybe (D, D)
pickUV view f (mx, my) (u0, u1) (v0, v1) =
  let n = 70 :: Int
      fi = fromIntegral :: Int -> D
      scr a b = look view (toV (f a b))
      cands = [ (d, u, v) | i <- [0 .. n], j <- [0 .. n]
              , let u = u0 + (u1 - u0) * fi i / fi n
                    v = v0 + (v1 - v0) * fi j / fi n
                    ((sx, sy), d) = scr u v
              , (sx - mx) * (sx - mx) + (sy - my) * (sy - my) < 22 * 22 ]
      better best@(bd, _, _) c@(d, _, _) = if d > bd then c else best
      err a b = let ((sx, sy), _) = scr a b in (sx - mx) * (sx - mx) + (sy - my) * (sy - my)
      refine :: Int -> D -> D -> D -> D -> Float -> (D, D, Float)
      refine k u v du dv e
        | k >= 60 || du <= 1e-5 = (u, v, e)
        | otherwise =
            let try1 (uu, vv, ee, moved) (a, b) =
                  let u' = uu + a
                      v' = max v0 (min v1 (vv + b))
                      e' = err u' v'
                  in if e' < ee then (u', v', e', True) else (uu, vv, ee, moved)
                (u2, v2, e2, m) = foldl try1 (u, v, e, False) [(du, 0), (-du, 0), (0, dv), (0, -dv)]
            in if m then refine (k + 1) u2 v2 du dv e2 else refine (k + 1) u2 v2 (du / 2) (dv / 2) e2
  in case cands of
       [] -> Nothing
       (c0 : cs) ->
         let (_, u, v) = foldl better c0 cs
             (ru, rv, e) = refine 0 u v ((u1 - u0) / fi n) ((v1 - v0) / fi n) (err u v)
         in if e < 36 then Just (ru, rv) else Nothing

-- 色相 h（0〜1）・彩度 s・明るさ b → (r, g, b)（0〜1）
hsv :: D -> D -> D -> (Float, Float, Float)
hsv h0 s b =
  let h = h0 - fromIntegral (floor h0 :: Int)
      i = floor (h * 6) :: Int
      f = h * 6 - fromIntegral i
      p = b * (1 - s)
      q = b * (1 - f * s)
      t = b * (1 - (1 - f) * s)
      (r, g, bl) = case i `mod` 6 of
        0 -> (b, t, p)
        1 -> (q, b, p)
        2 -> (p, b, t)
        3 -> (p, q, b)
        4 -> (t, p, b)
        _ -> (b, p, q)
  in (f2 r, f2 g, f2 bl)
