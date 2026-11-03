-- ============================================
-- 02 円周・トーラス・球面のパラメータ表示
-- 2 つの数 (u, v) を 3D の点に送る式 r(u, v) で面をつくる。長方形の (u, v) を曲げて貼り合わせるイメージ
--   ←→ ↑↓ で u・v の範囲を広げると、長方形のどの辺とどの辺が貼り合わさるかが見える
-- 1 = 円周 2 = 円柱 3 = 球面 4 = トーラス 5 = 一葉双曲面 6 = らせん面 / ←→ = u の範囲 / ↑↓ = v の範囲 / A = 点を動かす / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

bigA, smallB :: D
bigA = 1.0                  -- ★ トーラスの半径
smallB = 0.4

data Shape = Shape { sName :: String, sU, sV :: !(D, D), sF :: Surf, sTxt :: String }

-- ★ 面の式。sU, sV がパラメータの範囲
shape :: Int -> Shape
shape 1 = Shape "circle S1" (0, 2 * pi) (0, 0) (\u _ -> D3 (cos u) (sin u) 0)
  "r(u) = (cos u, sin u). u and u + 2pi are the same point (glue the two ends of a segment)"
shape 2 = Shape "cylinder" (0, 2 * pi) (-0.8, 0.8) (\u v -> D3 (cos u * 0.8) (sin u * 0.8) v)
  "r(u, v) = (cos u, sin u, v). Glue only the left and right edges"
shape 3 = Shape "sphere S2" (0, 2 * pi) (-pi / 2, pi / 2) (\u v -> D3 (cos v * cos u) (cos v * sin u) (sin v))
  "r = (cos v cos u, cos v sin u, sin v). Glue left-right; top and bottom edges each collapse to a pole"
shape 5 = Shape "hyperboloid" (0, 2 * pi) (-1.1, 1.1) (\u v -> D3 (0.55 * cosh v * cos u) (0.55 * cosh v * sin u) (0.55 * sinh v))
  "r = (cosh v cos u, cosh v sin u, sinh v), x^2 + y^2 - z^2 = 1. Same topology as a cylinder"
shape 6 = Shape "helicoid" (0, 4 * pi) (-0.9, 0.9) (\u v -> D3 (v * cos u) (v * sin u) (0.12 * (u - 2 * pi)))
  "r = (v cos u, v sin u, c u). Nothing is glued: same topology as a plane (spiral stairs)"
shape _ = Shape "torus T2" (0, 2 * pi) (-pi, pi)
  (\u v -> D3 ((bigA + smallB * cos v) * cos u) ((bigA + smallB * cos v) * sin u) (smallB * sin v))
  "r = ((A + B cos v) cos u, (A + B cos v) sin u, B sin v). Glue left-right AND top-bottom"

data World = World
  { wKind  :: !Int
  , wUFrac :: !D          -- ★ u の範囲のうち何割を描くか（0.1〜1）
  , wVFrac :: !D          -- ★ v の範囲のうち何割を描くか（0.1〜1）
  , wAuto  :: !Bool
  , wT     :: !D
  , wMesh  :: [Quad]      -- 面（形や範囲を変えたときだけ作り直す）
  , wCam   :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-02 Parametric Surfaces" (rebuild (World 4 1 1 True 0 [] cam0)))
  { skBack  = rgb255 16 18 28
  , skStep  = \_ _ w -> pure (if wAuto w then w { wT = wT w + 0.01 } else w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

ranges :: World -> (D, D, D, D)
ranges w =
  let s = shape (wKind w)
      (a, b) = sU s
      (c, d) = sV s
  in (a, a + (b - a) * wUFrac w, c, c + (d - c) * wVFrac w)

-- 色：u で色相、v で明るさ（左下の長方形と同じ色）
colUV :: Shape -> D -> D -> (Float, Float, Float)
colUV s u v =
  let (u0, u1) = sU s
      (v0, v1) = sV s
      a = (u - u0) / (u1 - u0)
      b = if v1 > v0 then (v - v0) / (v1 - v0) else 0.5
  in hsv a 0.55 (0.45 + 0.5 * b)

rebuild :: World -> World
rebuild w
  | wKind w == 1 = w { wMesh = [] }
  | otherwise =
      let s = shape (wKind w)
          (u0, u1, v0, v1) = ranges w
          nu = max 4 (round (80 * wUFrac w))
          nv = max 2 (round (40 * wVFrac w))
          gu = max 1 (round (16 * wUFrac w))
          gv = max 1 (round (8 * wVFrac w))
          col u v = let (r, g, b) = colUV s u v in (r, g, b, 1)
      in w { wMesh = makeSurface (sF s) (u0, u1) (v0, v1) (nu, nv) (gu, gv) col }

event :: Event -> World -> World
event ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (Char c) Down _ _
         | c >= '1' && c <= '6' -> rebuild w { wKind = fromEnum c - fromEnum '0' }
         | toLower c == 'a' -> w { wAuto = not (wAuto w) }
       EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wUFrac = max 0.1 (wUFrac w - 0.1) }
       EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wUFrac = min 1 (wUFrac w + 0.1) }
       EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wVFrac = max 0.1 (wVFrac w - 0.1) }
       EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wVFrac = min 1 (wVFrac w + 0.1) }
       _ -> w

draw :: Env -> World -> Picture
draw env w =
  let c = wCam w
      view = mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.32)
      s = shape (wKind w)
      (u0, u1, v0, v1) = ranges w
      t = wT w
      -- 動く点 (u, v)：範囲の中を行ったり来たり
      pu = u0 + (u1 - u0) * (0.5 + 0.5 * sin (t * 0.9))
      pv = v0 + (v1 - v0) * (0.5 + 0.45 * sin (t * 2.3))
      closed = wUFrac w >= 0.999 && wVFrac w >= 0.999 && wKind w `elem` [3, 4]
      fi = fromIntegral :: Int -> D
      circleSegs = concat
        [ segItem view (makeColor r g b 1) 6 (toV (sF s a 0)) (toV (sF s a' 0))
        | i <- [0 .. 119]
        , let a = u0 + (u1 - u0) * fi i / 120
              a' = u0 + (u1 - u0) * fi (i + 1) / 120
              (r, g, b) = colUV s a 0 ]
      body = if wKind w == 1 then circleSegs
             else surfaceItems view (120 / 255) 0.83 closed (rgb255 20 24 40) 0 (wMesh w)
      axes = concat
        [ segItem view (rgba255 255 90 90 140) 1 (V3 0 0 0) (V3 1.5 0 0)
        , segItem view (rgba255 90 255 120 140) 1 (V3 0 0 0) (V3 0 1.5 0)
        , segItem view (rgba255 90 140 255 140) 1 (V3 0 0 0) (V3 0 0 1.5) ]
      ball = ballItem view (rgb255 255 230 90) 8 (toV (sF s pu pv))
  in Pictures [scene (body ++ axes ++ ball), drawHud env w s u1 v1 pu pv]

drawHud :: Env -> World -> Shape -> D -> D -> D -> D -> Picture
drawHud env w s u1 v1 pu pv =
  let kind = wKind w
      wd = 300 :: Float
      hg = if kind == 1 then 40 else 200 :: Float
      ox = 30
      oy = envH env - hg - 60
      nu = 30 :: Int
      nv = if kind == 1 then 1 else 15 :: Int
      fi = fromIntegral :: Int -> D
      ff = fromIntegral :: Int -> Float
      (su0, su1) = sU s
      (sv0, sv1) = sV s
      cells = [ box env (ox + ff i * wd / ff nu) (oy + hg - ff (j + 1) * hg / ff nv) (wd / ff nu + 0.5) (hg / ff nv + 0.5)
                  (makeColor r g b (if inside then 1 else 40 / 255))
              | i <- [0 .. nu - 1], j <- [0 .. nv - 1]
              , let a = su0 + (su1 - su0) * (fi i + 0.5) / fi nu
                    b' = sv0 + (sv1 - sv0) * (fi j + 0.5) / fi nv
                    inside = a <= u1 && (kind == 1 || b' <= v1)
                    (r, g, b) = colUV s a b' ]
      vEdge c x = Color c (thickLine 4 (px env x oy) (px env x (oy + hg)))
      hEdge c y = Color c (thickLine 4 (px env ox y) (px env (ox + wd) y))
      redC = rgb255 255 120 120
      edges = [e | kind <= 5, e <- [vEdge redC ox, vEdge redC (ox + wd)]]
           ++ [e | kind == 4, e <- [hEdge (rgb255 120 200 255) oy, hEdge (rgb255 120 200 255) (oy + hg)]]
           ++ [e | kind == 3, e <- [hEdge (rgb255 255 230 90) oy, hEdge (rgb255 255 230 90) (oy + hg)]]
      dotX = ox + f2 ((pu - su0) / (su1 - su0)) * wd
      dotY = if kind == 1 then oy + hg / 2 else oy + hg - f2 ((pv - sv0) / (sv1 - sv0)) * hg
      D3 rx ry rz = sF s pu pv
      gold = rgb255 255 230 90
  in Pictures $
     [ txt env 20 34 16 white ("1-6: shape (" ++ sName s ++ ")   L/R: u range " ++ show (round (wUFrac w * 100) :: Int) ++ "%   U/D: v range " ++ show (round (wVFrac w * 100) :: Int) ++ "%   A: dot   Drag: view")
     , txt env 20 60 14 (gray 0.67) (sTxt s)
     , box env (ox - 14) (oy - 40) (wd + 28) (hg + 80) (withA 0.59 black)
     , txt env ox (oy - 16) 14 white "The (u, v) world"
     ] ++ cells ++ edges ++
     [ at (px env dotX dotY) (Color gold (disc 6))
     , txt env ox (oy + hg + 24) 14 (gray 0.86)
         ("red: glue L/R" ++ (if kind == 4 then "  blue: glue top/bottom" else if kind == 3 then "  yellow: collapse" else ""))
     , txt env (ox + wd + 40) (envH env - 60) 15 gold
         ("(u, v) = (" ++ nf 2 pu ++ ", " ++ nf 2 pv ++ ")  ->  r = (" ++ nf 2 rx ++ ", " ++ nf 2 ry ++ ", " ++ nf 2 rz ++ ")")
     ]

-- ---------- やってみよう ----------
-- 1. トーラスで u の範囲を 10% → 100% へ。次に v の範囲も。長方形の 4 辺がどう貼られたか言葉で説明しよう
-- 2. 球面の v の範囲を半分にすると「半球」。上の辺が 1 点につぶれるのはどこ？
-- 3. 一葉双曲面と円柱は位相的に同じ（どちらも左右だけ貼る）。でも曲がり方はちがう（05 で比べる）
-- 4. shape に自分の式を追加しよう。例：(u, v) → (sin u cos v, sin u sin v, cos u + log(tan(u/2)) + v)…

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
