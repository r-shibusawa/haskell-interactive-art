-- ============================================
-- 00 局所的には平ら
-- 球面やトーラスの上を歩く小さな虫には、まわりが「平らな床」に見える。これが多様体の出発点
--   見える範囲 r を小さくするほど、面は接平面（平らな板）からはみ出さなくなる：はみ出し ≈ r² / (2R)
-- ↑↓ = 見える範囲 r（ズーム）/ ←→ = 向きを変える / スペース = 歩く・止まる / 1 = 球面 2 = トーラス / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

bigA, smallB :: D
bigA = 1.0                 -- ★ トーラスの大きい半径（球面の半径は 1）
smallB = 0.42              -- ★ トーラスの小さい半径

speed :: D
speed = 0.006              -- ★ 歩く速さ

data World = World
  { wKind    :: !Int       -- 1 = 球面、2 = トーラス
  , wBu      :: !D         -- 虫のいる場所 u
  , wBv      :: !D         -- 虫のいる場所 v
  , wHeading :: !D         -- 虫の向き
  , wR       :: !D         -- ★ 虫に見える範囲（半径 1 の球に対する長さ）
  , wWalking :: !Bool
  , wCam     :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-00 Locally Flat" (World 1 0.3 0.35 0.4 0.5 True cam0))
  { skBack  = rgb255 14 16 26
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- ★ 面の式（数学の座標、z が上）
surf :: Int -> Surf
surf 1 u v = D3 (cos v * cos u) (cos v * sin u) (sin v)
surf _ u v = D3 ((bigA + smallB * cos v) * cos u) ((bigA + smallB * cos v) * sin u) (smallB * sin v)

sphereMesh, torusMesh :: [Quad]
sphereMesh = makeSurface (surf 1) (0, 2 * pi) (-pi / 2, pi / 2) (72, 36) (24, 12) baseCol
torusMesh  = makeSurface (surf 2) (0, 2 * pi) (-pi, pi) (72, 36) (24, 12) baseCol

baseCol :: D -> D -> (Float, Float, Float, Float)
baseCol _ _ = (70 / 255, 90 / 255, 130 / 255, 1)

-- 虫のいる点での「床」の向き：e1 = 進む向き、e2 = 左、n = 上（法線）
data Frame = Frame { fP, fRu, fRv, fN, fE1, fE2 :: !D3 }

frame :: World -> D -> D -> Frame
frame w u v =
  let f = surf (wKind w)
      e = 0.001
      ru = vmul (vsub (f (u + e) v) (f (u - e) v)) (1 / (2 * e))
      rv = vmul (vsub (f u (v + e)) (f u (v - e))) (1 / (2 * e))
      n = vunit (vcross ru rv)
      h = wHeading w
      e1 = vunit (vadd (vmul (vunit ru) (cos h)) (vmul (vunit rv) (sin h)))
  in Frame (f u v) ru rv n e1 (vcross n e1)

step :: Env -> World -> World
step env w0 =
  let turn | isDown env (SpecialKey KeyLeft) = 0.04
           | isDown env (SpecialKey KeyRight) = -0.04
           | otherwise = 0
      w = w0 { wHeading = wHeading w0 + turn }
  in if not (wWalking w) then w else
     let fr = frame w (wBu w) (wBv w)
         h = wHeading w
         -- 進む向き e1 を (u, v) の変化に直す（u と v の方向が直交している面なので割り算でよい）
         bu = wBu w + speed * cos h / vlen (fRu fr)
         bv = wBv w + speed * sin h / vlen (fRv fr)
     in if wKind w == 1
          then if abs bv > 1.35   -- 極の近くははね返る
                 then w { wBu = bu, wBv = max (-1.35) (min 1.35 bv), wHeading = -h }
                 else w { wBu = bu, wBv = bv }
          else w { wBu = bu, wBv = wrapD (bv + pi) - pi }
  where wrapD x = x - 2 * pi * fromIntegral (floor (x / (2 * pi)) :: Int)

event :: Event -> World -> World
event ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (SpecialKey KeyUp) Down _ _ -> w { wR = max 0.01 (wR w / 1.25) }
       EventKey (SpecialKey KeyDown) Down _ _ -> w { wR = min 1.4 (wR w * 1.25) }
       EventKey (SpecialKey KeySpace) Down _ _ -> w { wWalking = not (wWalking w) }
       EventKey (Char '1') Down _ _ -> w { wKind = 1, wBv = 0.3 }
       EventKey (Char '2') Down _ _ -> w { wKind = 2, wBv = 0.3 }
       _ -> w

draw :: Env -> World -> Picture
draw env w =
  let c = wCam w
      view = mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.3)
      f = surf (wKind w)
      bu = wBu w
      bv = wBv w
      r = wR w
      fr = frame w bu bv
      du = r / vlen (fRu fr)
      dv = r / vlen (fRv fr)
      lift p = toV (vadd p (vmul (fN fr) 0.006))    -- 面より少し浮かせる
      yellow = rgb255 255 220 90
      netLine pts = pathItems view yellow 2 (map lift pts)
      fi = fromIntegral :: Int -> D
      -- 虫に見える範囲：面の網目（黄色）
      net = concat [ netLine [f (bu + fi i / 4 * du) (bv + fi k / 10 * dv) | k <- [-10 .. 10]]
                  ++ netLine [f (bu + fi k / 10 * du) (bv + fi i / 4 * dv) | k <- [-10 .. 10]]
                   | i <- [-4 .. 4] ]
      -- 接平面（白い板）
      plate = plateItems view (withA (60 / 255) white) (fP fr) (vmul (fE1 fr) r) (vmul (fE2 fr) r)
      red' = rgb255 255 90 90
      bug = ballItem view red' 6 (lift (fP fr))
         ++ segItem view red' 3 (toV (fP fr)) (toV (vadd (fP fr) (vmul (fE1 fr) 0.18)))
      mesh = if wKind w == 1 then sphereMesh else torusMesh
      surface = surfaceItems view (110 / 255) 0.85 True (rgb255 150 175 230) 0 mesh
  in Pictures [scene (surface ++ plate ++ net ++ bug), drawHud env w fr du dv]

-- 右の窓：虫から見た景色（接平面の座標で、見える範囲 r を同じ大きさに拡大）
drawHud :: Env -> World -> Frame -> D -> D -> Picture
drawHud env w fr du dv =
  let r = wR w
      f = surf (wKind w)
      s = 340 :: Float
      ox = envW env - s - 30
      oy = 100
      k = s * 0.42 / f2 r
      cx = ox + s / 2
      cy = oy + s / 2 + 10
      fi = fromIntegral :: Int -> D
      -- 1 本の線の点：画面の点と高さ h
      netPt a b =
        let d = vsub (f (wBu w + a * du) (wBv w + b * dv)) (fP fr)
        in (px env (cx + f2 (vdot d (fE1 fr)) * k) (cy - f2 (vdot d (fE2 fr)) * k), abs (vdot d (fN fr)))
      nets = [ [ if dir == (1 :: Int) then netPt (fi i / 4) (fi t / 10) else netPt (fi t / 10) (fi i / 4) | t <- [-10 .. 10] ]
             | dir <- [0, 1], i <- [-4 .. 4] ]
      hmax = foldl' max 0 (map snd (concat nets))
      segOf (p, _) (q, h) =
        let t = clampF 0 1 (f2 (h / r) * 4)
        in Color (makeColor (lerpF 120 255 t / 255) (lerpF 230 90 t / 255) (lerpF 255 90 t / 255) 1) (thickLine 1.5 p q)
      netPics = concatMap (\l -> zipWith segOf l (drop 1 l)) nets
      gy = oy + s + 70
      h0 = wHeading w
      sect = [ px env (cx + f2 (vdot d (fE1 fr)) * k) (gy - f2 (vdot d (fN fr)) * k)
             | t <- [-20 .. 20]
             , let a = fi t / 20
                   d = vsub (f (wBu w + a * du * cos h0) (wBv w + a * dv * sin h0)) (fP fr) ]
      (mx, my) = px env cx cy
      y0 = px env (ox + 20) gy
      y1 = px env (ox + s - 20) gy
      onOff b = if b then "ON" else "off"
  in Pictures $
     [ txt env 20 34 16 white ("Up/Down: range r = " ++ nf 3 r ++ "   Left/Right: turn   Space: walk " ++ onOff (wWalking w) ++ "   1: sphere  2: torus   Drag: view")
     , txt env 20 58 14 (gray 0.67) "Yellow = surface the bug sees, white = tangent plane. Small r: the two look the same (locally flat)."
     , box env ox oy s (s + 190) (withA 0.59 black)
     , txt env (ox + 12) (oy + 24) 15 white "View from the bug (r fills the window)"
     ] ++ netPics ++
     [ at (mx, my) (Color (rgb255 255 90 90) (disc 5))
     , Color (withA 0.47 white) (Line [y0, y1])
     , Color (rgb255 255 120 120) (Line sect)
     , txt env (ox + 12) (oy + s + 30) 14 (gray 0.86) "Side view (white = plane, red = surface)"
     , txt env (ox + 12) (gy + 50) 14 (gray 0.86)
         ("overshoot h/r = " ++ nf 4 (hmax / r) ++ (if wKind w == 1 then "  (~ r/2R = " ++ nf 4 (r / 2) ++ ")" else ""))
     , txt env (ox + 12) (gy + 74) 14 (gray 0.86)
         (if wKind w == 1 then "circle/(2 pi r) = sin r / r = " ++ nf 5 (sin r / r) ++ " (flat: 1)"
                          else "A torus bends differently by place")
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
-- 1. r を 0.01 まで小さくして、右の窓の網がまっすぐな方眼紙に見えることを確かめよう
-- 2. 球面で h / r と r / 2 を比べる。r を半分にすると、はみ出しの割合も半分になる？
-- 3. トーラスの外側（v = 0）と内側（v = π）で右の窓を比べる。内側は「鞍の形」にはみ出す（05 のガウス曲率）
-- 4. bigA と smallB を変えて細いトーラス・太いトーラスに。smallB を bigA より大きくすると何が起こる？

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
