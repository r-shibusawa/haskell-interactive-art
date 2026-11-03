{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 15 カメラ映像を球に貼る（おまけ）
-- 平らなカメラ映像を、球面・トーラス・双曲円板に貼る。「どう貼るか」（写像）を変えると、同じ映像がまったくちがって見える
--   1 = 球面（正距円筒で巻く）2 = トーラス 3 = 球面（立体射影：北極から映像を投げる）4 = 双曲円板（ふちへ向かって無限にくり返す）
-- 1〜4 = 形と貼り方 / ←→ = 映像をずらす / ↑↓ = 映像の大きさ / スペース = 自動回転 / ドラッグ = 視点 / S = 保存
-- カメラが使えないときは、自動でつくった模様を貼る（カメラの許可が必要）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

vw, vh :: Int
vw = 160                   -- ★ カメラ映像の大きさ
vh = 120

imgW, imgH :: Int
imgW = 281
imgH = 250

imgK :: Float
imgK = 3.2

-- 点の表：位置（x, y, z の順）と、映像のどの画素を貼るか
newtype Cloud = Cloud (U.Vector (Float, Float, Float, Int))

data World = World
  { wCamera :: !(Maybe Camera)
  , wKind :: !Int
  , wZoom :: !D            -- ★ 映像の大きさ（3・4 で効く）
  , wShift :: !Float       -- 映像のずれ
  , wSpin :: !Bool
  , wLive :: !Bool
  , wFrame :: !B.ByteString
  , wCloud :: Cloud        -- 形を変えたときだけ作り直す
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-15 Webcam on a Manifold" (initial Nothing))
  { skBack  = rgb255 12 12 20
  , skStart = \_ -> do
      c <- startCamera vw vh
      pure (initial (Just c))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where initial c = World c 1 1 0 True False (fallback 0) (makeCloud 1 1) cam0

-- カメラがないときの模様（市松模様と動く円）
fallback :: Int -> B.ByteString
fallback frame =
  let t = fromIntegral frame :: Float
      cx = 80 + 50 * cos (t * 0.03)
      cy = 60 + 30 * sin (t * 0.05)
      pixel :: Int -> Word8
      pixel n =
        let (p, c) = n `divMod` 3
            (j, i) = p `divMod` vw
            fx = fromIntegral i - cx
            fy = fromIntegral j - cy
            inCircle = fx * fx + fy * fy < 12.5 * 12.5
            checker = even (i `div` 20 + j `div` 20)
            (r, g, b) | inCircle = (255, 80, 120)
                      | checker = (80, 140, 230)
                      | otherwise = (240, 200, 90)
        in case c of { 0 -> r; 1 -> g; _ -> b }
  in fst (B.unfoldrN (vw * vh * 3) (\n -> Just (pixel n, n + 1)) 0)

-- 0〜1 の外は鏡に映したようにくり返す
mirrorTex :: D -> D
mirrorTex s =
  let f = s - 2 * fromIntegral (floor (s / 2) :: Int)
  in if f > 1 then 2 - f else f

-- ★ 4 つの「貼り方」：面の点 (x, y, z) と、映像のどこ (s, t) を貼るか
shapeAt :: Int -> D -> D -> D -> (D3, D, D)
shapeAt kind zoom a b = case kind of
  1 -> let u = a * 2 * pi
           v = (b - 0.5) * pi
       in (D3 (cos v * cos u) (cos v * sin u) (sin v), a, 1 - b)
  2 -> let u = a * 2 * pi
           v = b * 2 * pi
       in (D3 ((1 + 0.45 * cos v) * cos u) ((1 + 0.45 * cos v) * sin u) (0.45 * sin v), a * 2, b)
  3 -> -- 球面の点を北極から平面に投げた位置 = 立体射影（01 の φN）で映像を読む
       let u = a * 2 * pi
           v = (b - 0.5) * (pi - 0.02)
           x = cos v * cos u
           y = cos v * sin u
           z = sin v
           w0 = x / (1 - z)
           w1 = y / (1 - z)
       in (D3 x y z, 0.5 + w0 / 2 * zoom, 0.5 - w1 / 2 * zoom)
  _ -> -- 双曲円板：円板の点 z を、双曲距離に比例した大きさの平面へ（ふちに近いほど映像が細かくくり返す）
       let r = 0.995 * (1 - (1 - a) * (1 - a))     -- ふちの近くほど細かく
           th = b * 2 * pi
           d = 2 * atanh r                          -- 中心からの双曲距離
           zx = r * cos th
           zy = r * sin th
           k = d / max r 1e-6 * 0.35 * zoom
       in (D3 (zx * 1.3) (zy * 1.3) 0, 0.5 + zx * k, 0.5 - zy * k)

makeCloud :: Int -> D -> Cloud
makeCloud kind zoom =
  let (na, nb) = if kind == 4 then (300, 480) else (480, 300) :: (Int, Int)
      sample n =
        let (i, j) = n `divMod` nb
            (p, s, t) = shapeAt kind zoom (fromIntegral i / fromIntegral na) (fromIntegral j / fromIntegral nb)
            V3 x y z = toV p
            tx = min (vw - 1) (floor (mirrorTex s * fromIntegral vw))
            ty = min (vh - 1) (floor (mirrorTex t * fromIntegral vh))
        in (x, y, z, (ty * vw + tx) * 3)
  in Cloud (U.generate (na * nb) sample)

step :: Env -> Float -> World -> IO World
step env _ w = do
  let c = wCam w
      left = isDown env (SpecialKey KeyLeft)
      right = isDown env (SpecialKey KeyRight)
      sh = wShift w + (if left then -0.01 else 0) + (if right then 0.01 else 0)
      cam' = if wSpin w && not (envDown env) then c { cRY = cRY c + 0.005 } else c
  (live, frame) <- case wCamera w of
    Nothing -> pure (False, fallback (envFrame env))
    Just camera -> do
      l <- camLive camera
      if l then (,) True <$> cameraFrame camera else pure (False, fallback (envFrame env))
  pure w { wShift = sh, wCam = cam', wLive = live, wFrame = frame }

event :: Env -> Event -> World -> IO World
event env ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
      rebuilt x = x { wCloud = makeCloud (wKind x) (wZoom x) }
  in case ev of
       EventKey (Char c) Down _ _
         | c >= '1' && c <= '4' -> pure (rebuilt w { wKind = fromEnum c - fromEnum '0' })
         | toLower c == 's' -> saveSvg "webcam_manifold" env (draw env w) >> pure w
       EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuilt w { wZoom = min 4 (wZoom w * 1.2) })
       EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuilt w { wZoom = max 0.2 (wZoom w / 1.2) })
       EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpin = not (wSpin w) }
       _ -> pure w

-- ★ 面を描く：点の表を投影して、いちばん手前の点の色を画素に残す（Z バッファ）
renderSurface :: View -> Cloud -> B.ByteString -> U.Vector Word8
renderSurface (View cx sx cy sy ez sc) (Cloud pts) frame = runST $ do
  zbuf <- MU.replicate (imgW * imgH) (-1e9 :: Float)
  img <- MU.new (imgW * imgH * 3)
  forM_ [0 .. imgW * imgH - 1] $ \i -> do
    MU.unsafeWrite img (i * 3) (12 :: Word8)
    MU.unsafeWrite img (i * 3 + 1) 12
    MU.unsafeWrite img (i * 3 + 2) 20
  let n = U.length pts
      ok = B.length frame >= vw * vh * 3
      go !i
        | i >= n = pure ()
        | otherwise = do
            let (a, b, c, t) = U.unsafeIndex pts i
                z1 = b * cy - a * sy
                x1 = a * cy + b * sy
                y2 = -c * cx - z1 * sx
                z2 = -c * sx + z1 * cx
                d = z2 * sc
                s = ez / max 1 (ez - d)
                ix = floor (x1 * sc * s / imgK + fromIntegral imgW / 2) :: Int
                iy = floor (y2 * sc * s / imgK + fromIntegral imgH / 2) :: Int
                r = BU.unsafeIndex frame t
                g = BU.unsafeIndex frame (t + 1)
                bl = BU.unsafeIndex frame (t + 2)
            splat zbuf img ix iy d r g bl
            splat zbuf img (ix + 1) iy d r g bl
            splat zbuf img ix (iy + 1) d r g bl
            splat zbuf img (ix + 1) (iy + 1) d r g bl
            go (i + 1)
  when ok (go 0)
  U.unsafeFreeze img

splat :: MU.STVector s Float -> MU.STVector s Word8 -> Int -> Int -> Float -> Word8 -> Word8 -> Word8 -> ST s ()
splat zbuf img ix iy d r g b
  | ix < 0 || iy < 0 || ix >= imgW || iy >= imgH = pure ()
  | otherwise = do
      let o = iy * imgW + ix
      z <- MU.unsafeRead zbuf o
      when (d > z) $ do
        MU.unsafeWrite zbuf o d
        MU.unsafeWrite img (o * 3) r
        MU.unsafeWrite img (o * 3 + 1) g
        MU.unsafeWrite img (o * 3 + 2) b

kindNote :: Int -> String
kindNote 1 = "Wraps the rectangle around (inverse equirectangular). Top and bottom edges collapse to the poles"
kindNote 2 = "Torus: glue left-right and top-bottom (02). Two copies side by side make one lap"
kindNote 3 = "Thrown from the north pole: the image grows without bound near the pole (01, 10 stereographic)"
kindNote _ = "Hyperbolic disk (09): copies of equal true size look smaller and repeat forever toward the rim"

draw :: Env -> World -> Picture
draw env w =
  let c = wCam w
      rx = if wKind w == 4 then cRX c * 0.4 - 0.9 else cRX c
      view = mkView env rx (cRY c + wShift w) (min (envW env) (envH env) * 0.32)
      img = renderSurface view (wCloud w) (wFrame w)
      surface = pixelImage imgW imgH imgK (\x y ->
        let i = (y * imgW + x) * 3
        in (U.unsafeIndex img i, U.unsafeIndex img (i + 1), U.unsafeIndex img (i + 2)))
  in Pictures
     [ surface
     , box env 0 0 (envW env) 72 (withA 0.55 black)
     , txt env 20 34 16 white ("1: sphere (wrap)  2: torus  3: sphere (stereographic)  4: hyperbolic disk   L/R: shift   U/D: size " ++ nf 2 (wZoom w) ++ "   Space: spin   S: save")
     , txt env 20 60 14 (gray 0.67) (kindNote (wKind w))
     , txt env 20 (envH env - 24) 14 (gray 0.67) (if wLive w then "Camera image" else "NO CAMERA: waiting or no permission, showing a pattern")
     ]

-- ---------- やってみよう ----------
-- 1. 1 と 3 で自分の顔がどう変わるか比べよう。3 の北極（上）では何が起きている？
-- 2. 4 の双曲円板で ↑↓。ふちの小さな映像は、双曲平面の中ではどれも中心と同じ大きさ（09 のタイル）
-- 3. shapeAt の s, t の式を変えて、自分だけの「貼り方」をつくろう（例：s = a * 3 で 3 回くり返す）
-- 4. 2 のトーラスで s = a * 2 を s = a に。左右のつなぎ目は見える？ 消すにはどうする？

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
