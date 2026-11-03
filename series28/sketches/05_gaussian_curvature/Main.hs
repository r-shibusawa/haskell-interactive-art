-- ============================================
-- 05 ガウス曲率
-- 面の各点で「いちばん曲がる向き」と「いちばん曲がらない向き」の曲率 κ1, κ2（主曲率）を掛けたもの = ガウス曲率 K
--   K > 0（赤）：お椀・球。K = 0（白）：円柱・平面。K < 0（青）：鞍（馬の鞍、ポテトチップス）
-- クリック = 点を調べる / 1 = 球面 2 = 円柱 3 = 鞍面 4 = トーラス 5 = 猿の鞍 6 = でこぼこ / H = 平均曲率で色 / ↑↓ = 色の強さ / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

data SurfDef = SurfDef { sName :: String, sU, sV :: !(D, D), sF :: Surf, sClosed :: !Bool }

surfDef :: Int -> SurfDef
surfDef 1 = SurfDef "sphere (K > 0 everywhere)" (-pi, pi) (-1.5, 1.5) (\u v -> D3 (cos v * cos u) (cos v * sin u) (sin v)) True
surfDef 2 = SurfDef "cylinder (K = 0 everywhere)" (-pi, pi) (-1, 1) (\u v -> D3 (0.7 * cos u) (0.7 * sin u) v) False
surfDef 3 = SurfDef "saddle (K < 0)" (-1.2, 1.2) (-1.2, 1.2) (\u v -> D3 u v ((u * u - v * v) / 2)) False
surfDef 5 = SurfDef "monkey saddle z = x^3 - 3xy^2" (-1.1, 1.1) (-1.1, 1.1) (\u v -> D3 u v (0.6 * (u * u * u - 3 * u * v * v))) False
surfDef 6 = SurfDef "bumps (peak, pit, pass)" (-1.3, 1.3) (-1.3, 1.3)
  (\u v -> D3 u v (0.45 * exp (-3 * ((u - 0.4) ^ (2 :: Int) + v * v)) - 0.35 * exp (-3 * ((u + 0.5) ^ (2 :: Int) + (v - 0.3) ^ (2 :: Int))))) False
surfDef _ = SurfDef "torus (outer +, inner -)" (-pi, pi) (-pi, pi)
  (\u v -> D3 ((1 + 0.45 * cos v) * cos u) ((1 + 0.45 * cos v) * sin u) (0.45 * sin v)) True

data World = World
  { wKind :: !Int
  , wUseH :: !Bool         -- False = ガウス曲率 K、True = 平均曲率 H で色をつける
  , wGain :: !D            -- ★ 色の強さ
  , wPu, wPv :: !D
  , wMesh :: [Quad]        -- 面（形や色を変えたときだけ作り直す）
  , wPress :: !(Maybe Point)
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-05 Gaussian Curvature" (rebuild (World 4 False 1.5 0.4 0.6 [] Nothing cam0)))
  { skBack  = rgb255 16 18 28
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- ★ 曲率の計算：第一基本形式 E, F, G と第二基本形式 L, M, N から
data Curv = Curv { cP, cRu, cRv, cNrm :: !D3, cE, cF, cG, cL, cM, cN, cK, cH, cK1, cK2 :: !D }

curv :: Surf -> D -> D -> Curv
curv f u v =
  let h = 0.002
      p = f u v
      ru = vmul (vsub (f (u + h) v) (f (u - h) v)) (1 / (2 * h))
      rv = vmul (vsub (f u (v + h)) (f u (v - h))) (1 / (2 * h))
      ruu = vmul (vadd (vsub (f (u + h) v) (vmul p 2)) (f (u - h) v)) (1 / (h * h))
      rvv = vmul (vadd (vsub (f u (v + h)) (vmul p 2)) (f u (v - h))) (1 / (h * h))
      ruv = vmul (vsub (vsub (f (u + h) (v + h)) (f (u + h) (v - h))) (vsub (f (u - h) (v + h)) (f (u - h) (v - h)))) (1 / (4 * h * h))
      n = vunit (vcross ru rv)
      e = vdot ru ru
      ff = vdot ru rv
      g = vdot rv rv
      l = vdot ruu n
      m = vdot ruv n
      nn = vdot rvv n
      w = e * g - ff * ff
      k = (l * nn - m * m) / w
      hh = (e * nn - 2 * ff * m + g * l) / (2 * w)
      dsc = sqrt (max 0 (hh * hh - k))
  in Curv p ru rv n e ff g l m nn k hh (hh + dsc) (hh - dsc)

-- 主曲率 k の向き（(II − k I) w = 0 を解く）
principalDir :: Curv -> D -> D3
principalDir c k =
  let a = (cM c - k * cF c, -(cL c - k * cE c))
      b = (cN c - k * cG c, -(cM c - k * cF c))
      hyp (x, y) = sqrt (x * x + y * y)
      w0 = if hyp a > hyp b then a else b
      (w1, w2) = if hyp w0 < 1e-9 then (1, 0) else w0
  in vunit (vadd (vmul (cRu c) w1) (vmul (cRv c) w2))

-- 曲率 → 色（+ 赤、0 白、− 青）
kRamp :: Float -> (Float, Float, Float)
kRamp x
  | x >= 0 = (235 / 255, (235 - 190 * x) / 255, (225 - 200 * x) / 255)
  | otherwise = ((235 + 200 * x) / 255, (235 + 120 * x) / 255, 240 / 255)

rebuild :: World -> World
rebuild w =
  let s = surfDef (wKind w)
      col u v = let c = curv (sF s) u v
                    (r, g, b) = kRamp (f2 (tanh ((if wUseH w then cH c else cK c) * wGain w)))
                in (r, g, b, 1)
  in w { wMesh = makeSurface (sF s) (sU s) (sV s) (64, 48) (16, 12) col }

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
                Just (u, v) -> w { wPu = u, wPv = v, wPress = Nothing }
                Nothing -> w { wPress = Nothing }
         _ -> w { wPress = Nothing }
       EventKey (Char c) Down _ _
         | c >= '1' && c <= '6' -> rebuild w { wKind = fromEnum c - fromEnum '0', wPu = 0.3, wPv = 0.3 }
         | toLower c == 'h' -> rebuild w { wUseH = not (wUseH w) }
       EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wGain = min 20 (wGain w * 1.4) }
       EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wGain = max 0.1 (wGain w / 1.4) }
       _ -> w

draw :: Env -> World -> Picture
draw env w =
  let view = viewOf env w
      s = surfDef (wKind w)
      c = curv (sF s) (wPu w) (wPv w)
      p = cP c
      n = cNrm c
      fi = fromIntegral :: Int -> D
      -- 2 つの主方向と、その向きの「法断面」の接触円（同じ側なら K > 0、反対側なら K < 0）
      dirItems (k, col) =
        let d = principalDir c k
            chord a = toV (vadd p (vmul d a))
            shaft = pathItems view col 4 [chord (-0.35 + 0.7 * fi i / 6) | i <- [0 .. 6]]
            circ | abs k > 0.05 =
                     let rho = 1 / k
                         ctr = vadd p (vmul n rho)
                     in pathItems view col 1.5
                          [ toV (vadd ctr (vadd (vmul n (-rho * cos a)) (vmul d (abs rho * sin a))))
                          | i <- [0 .. 80], let a = fi i / 80 * 2 * pi ]
                 | otherwise = pathItems view col 1.5 [chord (-1.2 + 2.4 * fi i / 12) | i <- [0 .. 12]]
        in shaft ++ circ
      overlays = concatMap dirItems [(cK1 c, rgb255 255 150 40), (cK2 c, rgb255 40 220 255)]
              ++ pathItems view white 3 [toV (vadd p (vmul n (0.4 * fi i / 4))) | i <- [0 .. 4]]
              ++ ballItem view (rgb255 255 230 90) 7 (toV p)
      surface = surfaceItems view (140 / 255) 0.65 (sClosed s) (rgb255 60 60 80) 0 (wMesh w)
  in Pictures [scene (surface ++ overlays), drawHud env w s c]

drawHud :: Env -> World -> SurfDef -> Curv -> Picture
drawHud env w s c =
  let x = 20
      y = envH env - 200
      ff = fromIntegral :: Int -> Float
      legend = [ box env (x + ff i * 2) (y + 110) 2 16 (let (r, g, b) = kRamp (ff i / 199 * 2 - 1) in makeColor r g b 1) | i <- [0 .. 199] ]
  in Pictures $
     [ txt env 20 28 16 white ("Click: inspect   1-6: surface (" ++ sName s ++ ")")
     , txt env 20 50 16 white ("H: color by " ++ (if wUseH w then "mean curvature H" else "Gaussian curvature K") ++ "   U/D: color strength   Drag: view")
     , txt env 20 72 14 (gray 0.67) "Orange = most curved dir (k1), cyan = least (k2). Circles on same side: K > 0, opposite: K < 0, one straight: K = 0"
     , box env (x - 10) (y - 30) 520 190 (withA (160 / 255) black)
     , txt env x y 15 (rgb255 255 170 80) ("k1 = " ++ nf 3 (cK1 c))
     , txt env (x + 160) y 15 (rgb255 80 230 255) ("k2 = " ++ nf 3 (cK2 c))
     , txt env x (y + 34) 19 white ("K = k1 k2 = " ++ nf 3 (cK c))
     , txt env (x + 270) (y + 34) 15 white ("H = (k1 + k2)/2 = " ++ nf 3 (cH c))
     , txt env x (y + 62) 14 (gray 0.78) "K = (LN - M^2) / (EG - F^2)   L, M, N = normal part of r''"
     , txt env x (y + 88) 14 (gray 0.78) ("E, F, G = " ++ nf 2 (cE c) ++ ", " ++ nf 2 (cF c) ++ ", " ++ nf 2 (cG c) ++ "   L, M, N = " ++ nf 2 (cL c) ++ ", " ++ nf 2 (cM c) ++ ", " ++ nf 2 (cN c))
     ] ++ legend ++
     [ txt env x (y + 146) 14 (gray 0.78) "- (saddle)"
     , txt env (x + 140) (y + 146) 14 (gray 0.78) "0 (flat, cylinder)"
     , txt env (x + 340) (y + 146) 14 (gray 0.78) "+ (bowl)"
     ]

-- ---------- やってみよう ----------
-- 1. 球面の K を半径を変えて確かめる（式の 1 を R にすると K = 1/R²）
-- 2. 円柱は曲がって見えるのに K = 0。紙を丸めると円柱になる = 紙（平面）と同じ K。紙で球は包めない理由
-- 3. トーラスの外側・内側・てっぺんで K を比べる。境目（K = 0）の線はどこ？
-- 4. H で色を変えると、鞍面の原点は H = 0（極小曲面：石けん膜と同じ）。K と H のちがいを説明しよう

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
