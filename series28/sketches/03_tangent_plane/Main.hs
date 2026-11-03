-- ============================================
-- 03 接ベクトルと接平面
-- 面の上の点 p で、u だけ少し動かした向き ∂r/∂u（赤）と v だけ動かした向き ∂r/∂v（緑）が「接平面」を張る
--   面の上を動く点の速度は、いつもこの 2 本の組み合わせ γ' = a ∂r/∂u + b ∂r/∂v（黄）。法線 n（青）とは直角
-- クリック = 点 p を選ぶ / ←→ = 曲線の向き φ / スペース = 止める / 1 = 球面 2 = トーラス 3 = 鞍面 4 = 波 / P = 接平面の表示 / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

data SurfDef = SurfDef { sName :: String, sU, sV :: !(D, D), sF :: Surf, sClosed :: !Bool }

surfDef :: Int -> SurfDef
surfDef 1 = SurfDef "sphere" (-pi, pi) (-1.5, 1.5) (\u v -> D3 (cos v * cos u) (cos v * sin u) (sin v)) True
surfDef 2 = SurfDef "torus" (-pi, pi) (-pi, pi)
  (\u v -> D3 ((1 + 0.42 * cos v) * cos u) ((1 + 0.42 * cos v) * sin u) (0.42 * sin v)) True
surfDef 3 = SurfDef "saddle z = (x^2 - y^2)/3" (-1.2, 1.2) (-1.2, 1.2) (\u v -> D3 u v ((u * u - v * v) / 3)) False
surfDef _ = SurfDef "wave z = 0.3 sin 2x cos 2y" (-1.3, 1.3) (-1.3, 1.3) (\u v -> D3 u v (0.3 * sin (2 * u) * cos (2 * v))) False

meshOf :: Int -> [Quad]
meshOf k = let s = surfDef k
           in makeSurface (sF s) (sU s) (sV s) (64, 48) (16, 12) (\_ _ -> (80 / 255, 110 / 255, 160 / 255, 1))

mesh1, mesh2, mesh3, mesh4 :: [Quad]
mesh1 = meshOf 1
mesh2 = meshOf 2
mesh3 = meshOf 3
mesh4 = meshOf 4

data World = World
  { wKind :: !Int
  , wPu, wPv :: !D           -- 選んだ点 p の (u, v)
  , wPhi :: !D               -- ★ 曲線の向き（(u, v) の世界での角度）
  , wTT :: !D
  , wMoving, wPlane :: !Bool
  , wPress :: !(Maybe Point)
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-03 Tangent Plane" (World 4 0.5 0.4 0.6 0 True True Nothing cam0))
  { skBack  = rgb255 16 18 28
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- ★ 偏微分（接ベクトル）を差分で求める
partials :: Surf -> D -> D -> (D3, D3, D3)
partials f u v =
  let e = 0.0005
  in ( f u v
     , vmul (vsub (f (u + e) v) (f (u - e) v)) (1 / (2 * e))
     , vmul (vsub (f u (v + e)) (f u (v - e))) (1 / (2 * e)) )

step :: Env -> World -> World
step env w =
  let turn | isDown env (SpecialKey KeyLeft) = -0.03
           | isDown env (SpecialKey KeyRight) = 0.03
           | otherwise = 0
  in w { wPhi = wPhi w + turn, wTT = if wMoving w then wTT w + 0.012 else wTT w }

viewOf :: Env -> World -> View
viewOf env w = let c = wCam w in mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.3)

event :: Env -> Event -> World -> World
event env ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (MouseButton LeftButton) Down _ p -> w { wPress = Just p }
       EventKey (MouseButton LeftButton) Up _ p -> case wPress w of
         Just q | dist2 p q < 5 ->
           let s = surfDef (wKind w)
           in case pickUV (viewOf env w) (sF s) p (sU s) (sV s) of
                Just (u, v) -> w { wPu = u, wPv = v, wTT = 0, wPress = Nothing }
                Nothing -> w { wPress = Nothing }
         _ -> w { wPress = Nothing }
       EventKey (SpecialKey KeySpace) Down _ _ -> w { wMoving = not (wMoving w) }
       EventKey (Char c) Down _ _
         | c >= '1' && c <= '4' -> w { wKind = fromEnum c - fromEnum '0', wPu = 0.3, wPv = 0.3 }
         | toLower c == 'p' -> w { wPlane = not (wPlane w) }
       _ -> w

draw :: Env -> World -> Picture
draw env w =
  let view = viewOf env w
      s = surfDef (wKind w)
      f = sF s
      phi = wPhi w
      (tp, tru, trv) = partials f (wPu w) (wPv w)
      n = vunit (vcross tru trv)
      -- 曲線 γ(t) = r(pu + t cos φ, pv + t sin φ)
      l = 0.9
      sv = l * sin (wTT w)
      cu = wPu w + sv * cos phi
      cv = wPv w + sv * sin phi
      (cp, cru, crv) = partials f cu cv
      vel = vadd (vmul cru (cos phi)) (vmul crv (sin phi))   -- γ'(t)
      nC = vunit (vcross cru crv)
      fi = fromIntegral :: Int -> D
      curve = pathItems view (rgb255 255 230 90) 3
        [ toV (vadd (f (wPu w + a * cos phi) (wPv w + a * sin phi)) (vmul nC 0.004))
        | i <- [-40 .. 40], let a = fi i / 40 * l ]
      -- 接平面
      e1 = vunit tru
      e2 = vcross n e1
      h = 0.5
      plane = [ it | wPlane w, it <- plateItems view (withA (70 / 255) white) tp (vmul e1 h) (vmul e2 h) ]
      -- 接ベクトル（長いほうを 0.55 にそろえて表示）
      k = 0.55 / max (vlen tru) (vlen trv)
      kv = 0.4 / max (vlen vel) 0.4
      arrow3 p d c = pathItems view c 4 [toV (vadd p (vmul d (fi i / 6))) | i <- [0 .. 6]] ++ ballItem view c 5 (toV (vadd p d))
      arrows = arrow3 tp (vmul tru k) (rgb255 255 90 90)
            ++ arrow3 tp (vmul trv k) (rgb255 90 230 120)
            ++ arrow3 tp (vmul n 0.45) (rgb255 100 150 255)
            ++ arrow3 cp (vmul vel kv) (rgb255 255 230 90)
      balls = ballItem view (rgb255 255 230 90) 7 (toV cp) ++ ballItem view white 6 (toV tp)
      mesh = case wKind w of { 1 -> mesh1; 2 -> mesh2; 3 -> mesh3; _ -> mesh4 }
      surface = surfaceItems view (120 / 255) 0.83 (sClosed s) (rgb255 150 175 230) 0 mesh
  in Pictures [scene (surface ++ curve ++ plane ++ arrows ++ balls), drawHud env w s tp tru trv n vel nC]

v3 :: D3 -> String
v3 (D3 a b c) = "(" ++ nf 2 a ++ ", " ++ nf 2 b ++ ", " ++ nf 2 c ++ ")"

drawHud :: Env -> World -> SurfDef -> D3 -> D3 -> D3 -> D3 -> D3 -> D3 -> Picture
drawHud env w s tp tru trv n vel nC =
  let e = vdot tru tru
      f = vdot tru trv
      g = vdot trv trv
      x = 20
      y = envH env - 226
      phiDeg = jsMod (degF (f2 (wPhi w))) 360
  in Pictures
     [ txt env 20 28 16 white ("Click: pick point   L/R: curve direction phi = " ++ nf 0 phiDeg ++ " deg   Space: stop   P: plane   Drag: view")
     , txt env 20 50 16 white ("1-4: surface (" ++ sName s ++ ")")
     , txt env 20 72 14 (gray 0.67) "Red = dr/du, green = dr/dv, blue = normal n. The yellow dot's velocity always lies in the tangent plane."
     , box env (x - 10) (y - 30) 560 226 (withA 0.59 black)
     , txt env x y 15 white ("p = r(" ++ nf 2 (wPu w) ++ ", " ++ nf 2 (wPv w) ++ ") = " ++ v3 tp)
     , txt env x (y + 26) 15 (rgb255 255 120 120) ("dr/du = " ++ v3 tru)
     , txt env x (y + 52) 15 (rgb255 120 240 140) ("dr/dv = " ++ v3 trv)
     , txt env x (y + 78) 15 (rgb255 130 170 255) ("n = " ++ v3 n)
     , txt env x (y + 104) 15 white ("First form  E = " ++ nf 3 e ++ "  F = " ++ nf 3 f ++ "  G = " ++ nf 3 g)
     , txt env x (y + 130) 15 (rgb255 255 230 90) ("v = cos phi dr/du + sin phi dr/dv    n.v = " ++ nf 5 (vdot nC vel) ++ " (always 0)")
     , txt env x (y + 154) 14 (gray 0.67) "speed |v|^2 = E cos^2 phi + 2F cos phi sin phi + G sin^2 phi"
     , txt env x (y + 174) 14 (gray 0.67) ("  = " ++ nf 3 (vdot vel vel) ++ " (lengths are measured by the first form)")
     ]

-- 接平面の板：小さな四角に分けて、面との前後を場所ごとに決める
plateItems :: View -> Color -> D3 -> D3 -> D3 -> [Item]
plateItems view c p ax ay =
  [ (z + liftZ - 2, Color c (Polygon [fst (look view (toV (pt a b))) | (a, b) <- [(i, j), (i + 1, j), (i + 1, j + 1), (i, j + 1)]]))
  | i <- [0 .. n - 1], j <- [0 .. n - 1]
  , let z = snd (look view (toV (pt (i + 0.5) (j + 0.5)))) ]
  where n = 8 :: D
        pt a b = vadd p (vadd (vmul ax (2 * a / n - 1)) (vmul ay (2 * b / n - 1)))

-- ---------- やってみよう ----------
-- 1. 球面の北極の近く（v ≈ 1.5）をクリック。∂r/∂u がとても短くなるのはなぜ？（緯度経度の座標の弱点）
-- 2. 鞍面の原点で接平面は水平。少しずれた点では？ 面が接平面の上と下の両方にはみ出すのを横から見よう
-- 3. E, F, G から角度 cos θ = F / √(EG) を計算して表示しよう。F = 0 なら ∂r/∂u と ∂r/∂v は直角
-- 4. surfDef に自分の面（例：z = x³ − 3xy²「猿の鞍」）を追加しよう

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
