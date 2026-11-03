{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 14 多様体ブラシ
-- 球面やトーラスの表面に直接絵をかく。マウスの位置から「面のどの (u, v) か」を探して、(u, v) の画像（テクスチャ）に塗る
--   n 回対称・鏡映でコピーしながら描ける。描くと面の上の高さ v で音が鳴る。自由制作のベース
-- クリックで音をスタート / 面の上をドラッグ = 描く / 背景をドラッグ = 回す / 1 = 球面 2 = トーラス / ←→ = 対称の数 n / ↑↓ = 太さ / M = 鏡映 / H = 虹色 / スペース = 自動回転 / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

texW, texH :: Int
texW = 512                 -- ★ テクスチャの大きさ
texH = 256

-- 面を描く画像（小さな画素を k 倍に広げて、画面の中央に置く）
imgW, imgH :: Int
imgW = 281
imgH = 250

imgK :: Float
imgK = 3.2

data SurfDef = SurfDef { sName :: String, sU, sV :: !(D, D), sF :: Surf }

surfDef :: Int -> SurfDef
surfDef 1 = SurfDef "sphere" (0, 2 * pi) (-pi / 2, pi / 2) (\u v -> D3 (cos v * cos u) (cos v * sin u) (sin v))
surfDef _ = SurfDef "torus" (0, 2 * pi) (-pi, pi)
  (\u v -> D3 ((1 + 0.45 * cos v) * cos u) ((1 + 0.45 * cos v) * sin u) (0.45 * sin v))

-- 面の上の点の表：テクスチャの 1 画素ごとに 3D の位置と法線
data Cloud = Cloud !(U.Vector Float) !(U.Vector Float)

makeCloud :: Int -> Cloud
makeCloud k =
  let s = surfDef k
      f = sF s
      (u0, u1) = sU s
      (v0, v1) = sV s
      pts = [ (toV (f u v), toV (normalAt f u v))
            | j <- [0 .. texH - 1], i <- [0 .. texW - 1]
            , let u = u0 + (u1 - u0) * (fromIntegral i + 0.5) / fromIntegral texW
                  v = v0 + (v1 - v0) * (fromIntegral j + 0.5) / fromIntegral texH ]
      flat3 sel = U.fromList (concatMap (\p -> let V3 a b c = sel p in [a, b, c]) pts)
  in Cloud (flat3 fst) (flat3 snd)

cloud1, cloud2 :: Cloud
cloud1 = makeCloud 1
cloud2 = makeCloud 2

data World = World
  { wKind :: !Int
  , wSym :: !Int           -- ★ 回転対称の数（u 方向に 2π/n ずつコピー）
  , wMirror :: !Bool       -- ★ v → −v の鏡映もコピー
  , wBrush :: !D           -- ★ 筆の太さ（面の上の長さ）
  , wRainbow, wSpin, wAudio, wPainting :: !Bool
  , wTex :: !(U.Vector Word8)   -- テクスチャ（r, g, b の順）
  , wLastHit :: !(Maybe (D, D))
  , wHue :: !D
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-14 Manifold Brush" initial)
  { skBack  = rgb255 20 20 30
  , skStart = \_ -> cloudSize cloud1 `seq` cloudSize cloud2 `seq` pure initial
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where
    initial = World 1 6 True 0.05 True True False False cleanTex Nothing 0 cam0
    cloudSize (Cloud a _) = U.length a

-- まっさらな紙（薄い方眼つき）
cleanTex :: U.Vector Word8
cleanTex = U.generate (texW * texH * 3) $ \n ->
  let (p, c) = n `divMod` 3
      (j, i) = p `divMod` texW
      onLine = i `mod` (texW `div` 24) == 0 || j `mod` (texH `div` 12) == 0
      base = case c of { 0 -> 235; 1 -> 232; _ -> 222 } :: Float
  in truncate (if onLine then base * (1 - 25 / 255) else base)

-- 右下の画像
thumbnail :: U.Vector Word8 -> Picture
thumbnail t =
  let wd = 300 :: Int
      hd = 150 :: Int
  in pixelImage wd hd 1 (\x y ->
       let tx = x * texW `div` wd
           ty = y * texH `div` hd
           i = (ty * texW + tx) * 3
       in (U.unsafeIndex t i, U.unsafeIndex t (i + 1), U.unsafeIndex t (i + 2)))

viewOf :: Env -> World -> View
viewOf env w = let c = wCam w in mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.32)

pickNow :: Env -> World -> Point -> Maybe (D, D)
pickNow env w p = let s = surfDef (wKind w) in pickUV (viewOf env w) (sF s) p (sU s) (sV s)

-- (u, v) に筆を 1 回押す（対称なコピーもすべて）。返すのは塗る楕円の一覧
stamps :: World -> D -> D -> [(Float, Float, Float, Float)]
stamps w u v =
  let s = surfDef (wKind w)
      f = sF s
      e = 0.001
      (u0, u1) = sU s
      (v0, v1) = sV s
      ru = vlen (vsub (f (u + e) v) (f (u - e) v)) / (2 * e)
      rv = vlen (vsub (f u (v + e)) (f u (v - e))) / (2 * e)
      tw = fromIntegral texW :: D
      th = fromIntegral texH :: D
      -- 面の上で円になるように、画像の上では横 ew・縦 eh の楕円にする
      ew = min (tw * 0.5) (wBrush w * tw / (u1 - u0) / max ru 0.03)
      eh = wBrush w * th / (v1 - v0) / rv
      vs = if wMirror w then [v, -v] else [v]
      n = wSym w
  in [ (f2 (x + dx), f2 y, f2 ew, f2 eh)
     | k <- [0 .. n - 1], vv <- vs
     , let uu = u + fromIntegral k * (u1 - u0) / fromIntegral n
           a = (uu - u0) / (u1 - u0)
           x = (a - fromIntegral (floor a :: Int)) * tw
           y = (vv - v0) / (v1 - v0) * th
     , dx <- [-tw, 0, tw]
     , x + dx > -ew && x + dx < tw + ew ]

-- 楕円をテクスチャに塗る
paintStamps :: (Float, Float, Float) -> [(Float, Float, Float, Float)] -> U.Vector Word8 -> U.Vector Word8
paintStamps (r, g, b) es = U.modify $ \m ->
  forM_ es $ \(cx, cy, ew, eh) -> do
    let rx = ew / 2
        ry = eh / 2
        y0 = max 0 (floor (cy - ry))
        y1 = min (texH - 1) (ceiling (cy + ry))
        x0 = max 0 (floor (cx - rx))
        x1 = min (texW - 1) (ceiling (cx + rx))
        q v = truncate (v * 255) :: Word8
    forM_ [y0 .. y1] $ \j -> forM_ [x0 .. x1] $ \i -> do
      let dx = (fromIntegral i + 0.5 - cx) / max 0.5 rx
          dy = (fromIntegral j + 0.5 - cy) / max 0.5 ry
      when (dx * dx + dy * dy <= 1) $ do
        let o = (j * texW + i) * 3
        MU.unsafeWrite m o (q r)
        MU.unsafeWrite m (o + 1) (q g)
        MU.unsafeWrite m (o + 2) (q b)

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let c0 = wCam w0
      w1 = if wSpin w0 && not (envDown env) then w0 { wCam = c0 { cRY = cRY c0 + 0.004 } } else w0
  if not (wPainting w1 && envDown env) then pure w1 else
    case pickNow env w1 (envMouse env) of
      Nothing -> pure w1 { wLastHit = Nothing }
      Just hit@(hu, hv) -> do
        let hue0 = let h = wHue w1 + 0.004 in if h >= 1 then h - 1 else h
            col = if wRainbow w1 then hsv hue0 0.75 0.95 else (30 / 255, 40 / 255, 70 / 255)
            -- 前の点から今の点まで、すき間なく押していく
            (fu, fv) = maybe hit id (wLastHit w1)
            du0 = hu - fu
            du = if abs du0 > pi then du0 - signum du0 * 2 * pi else du0
            dv = hv - fv
            n = max 1 (min 60 (ceiling (sqrt (du * du + dv * dv) / (wBrush w1 * 0.3)))) :: Int
            fi = fromIntegral :: Int -> D
            es = concat [ stamps w1 (fu + du * fi i / fi n) (fv + dv * fi i / fi n) | i <- [1 .. n] ]
            w2 = w1 { wTex = paintStamps col es (wTex w1), wLastHit = Just hit, wHue = hue0 }
        -- 描いている間だけ、短い音をくり返し鳴らす
        when (wAudio w2 && envFrame env `mod` 8 == 0) $
          playTone (toFreq w2 hv) 0.12 0.16 Triangle
        pure w2

toFreq :: World -> D -> Float
toFreq w v =
  let s = surfDef (wKind w)
      (v0, v1) = sV s
      t = (v - v0) / (v1 - v0)
      k = floor (t * 14) :: Int
      penta = case k `mod` 5 of { 0 -> 0; 1 -> 2; 2 -> 4; 3 -> 7; _ -> 9 } :: Int
      semis = (k `div` 5) * 12 + penta
  in 196 * 2 ** (fromIntegral semis / 12)

event :: Env -> Event -> World -> IO World
event env ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (MouseButton LeftButton) Down _ p ->
         let hit = pickNow env w p
             painting = maybe False (const True) hit
             c = wCam w
         in pure w { wAudio = True, wPainting = painting, wLastHit = Nothing, wCam = c { cDrag = not painting } }
       EventKey (MouseButton LeftButton) Up _ _ -> pure w { wPainting = False, wLastHit = Nothing }
       EventKey (Char c) Down _ _
         | c == '1' -> pure w { wKind = 1 }
         | c == '2' -> pure w { wKind = 2 }
         | toLower c == 'm' -> pure w { wMirror = not (wMirror w) }
         | toLower c == 'h' -> pure w { wRainbow = not (wRainbow w) }
         | toLower c == 'c' -> pure w { wTex = cleanTex }
         | toLower c == 's' -> saveSvg "manifold_brush" env (draw env w) >> pure w
       EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpin = not (wSpin w) }
       EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wSym = max 1 (wSym w - 1) }
       EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wSym = min 16 (wSym w + 1) }
       EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wBrush = min 0.3 (wBrush w * 1.3) }
       EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wBrush = max 0.01 (wBrush w / 1.3) }
       _ -> pure w

-- ★ 面を描く：点の表を 1 つずつ画面に投影して、いちばん手前の点の色を画素に残す（Z バッファ）
renderSurface :: View -> Cloud -> U.Vector Word8 -> U.Vector Word8
renderSurface (View cx sx cy sy ez sc) (Cloud pos nrm) tex = runST $ do
  zbuf <- MU.replicate (imgW * imgH) (-1e9 :: Float)
  img <- MU.new (imgW * imgH * 3)
  fillBack img
  let n = U.length pos `div` 3
      lx = 0.3 / 1.2042 :: Float
      ly = 0.6 / 1.2042
      lz = -1 / 1.2042
      amb = 170 / 255 :: Float
      dif = 120 / 255 :: Float
      go !i
        | i >= n = pure ()
        | otherwise = do
            let k = i * 3
                a = U.unsafeIndex nrm k
                b = U.unsafeIndex nrm (k + 1)
                c = U.unsafeIndex nrm (k + 2)
                -- 法線を回す
                nz1 = b * cy - a * sy
                nx = a * cy + b * sy
                ny = -c * cx - nz1 * sx
                nz = -c * sx + nz1 * cx
                px0 = U.unsafeIndex pos k
                py0 = U.unsafeIndex pos (k + 1)
                pz0 = U.unsafeIndex pos (k + 2)
                z1 = py0 * cy - px0 * sy
                x1 = px0 * cy + py0 * sy
                y2 = -pz0 * cx - z1 * sx
                z2 = -pz0 * sx + z1 * cx
                d = z2 * sc
                s = ez / max 1 (ez - d)
                -- 手前を向いている点だけ描く
                facing = nz * (ez / sc - z2) - nx * x1 - ny * y2 > 0
            if not facing then go (i + 1) else do
              let sxp = x1 * sc * s / imgK + fromIntegral imgW / 2
                  syp = y2 * sc * s / imgK + fromIntegral imgH / 2
                  ix = floor sxp :: Int
                  iy = floor syp :: Int
                  lam = max 0 (-(nx * lx + ny * ly + nz * lz))
                  kl = amb + dif * lam
                  shade :: Word8 -> Word8
                  shade v = truncate (min 255 (fromIntegral v * kl))
                  r = shade (U.unsafeIndex tex k)
                  g = shade (U.unsafeIndex tex (k + 1))
                  bl = shade (U.unsafeIndex tex (k + 2))
              splat zbuf img ix iy d r g bl
              splat zbuf img (ix + 1) iy d r g bl
              splat zbuf img ix (iy + 1) d r g bl
              splat zbuf img (ix + 1) (iy + 1) d r g bl
              go (i + 1)
  go 0
  U.unsafeFreeze img

fillBack :: MU.STVector s Word8 -> ST s ()
fillBack img = forM_ [0 .. imgW * imgH - 1] $ \i -> do
  MU.unsafeWrite img (i * 3) 20
  MU.unsafeWrite img (i * 3 + 1) 20
  MU.unsafeWrite img (i * 3 + 2) 30

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

draw :: Env -> World -> Picture
draw env w =
  let view = viewOf env w
      cloud = if wKind w == 1 then cloud1 else cloud2
      img = renderSurface view cloud (wTex w)
      surface = pixelImage imgW imgH imgK (\x y ->
        let i = (y * imgW + x) * 3
        in (U.unsafeIndex img i, U.unsafeIndex img (i + 1), U.unsafeIndex img (i + 2)))
      wd = envW env
      hg = envH env
      tw = 300
      th = 150
      ox = wd - tw - 24
      oy = hg - th - 24
      onOff b = if b then "ON" else "off"
      s = surfDef (wKind w)
  in Pictures
     [ surface
     , box env 0 0 wd 84 (withA 0.55 black)
     , txt env 20 28 16 white ((if wAudio w then "Sound on" else "Click for sound") ++ "   Drag surface: draw   Drag background: turn   1/2: " ++ sName s ++ "   L/R: symmetry n = " ++ show (wSym w))
     , txt env 20 50 16 white ("U/D: brush   M: mirror " ++ onOff (wMirror w) ++ "   H: rainbow   Space: spin   C: clear   S: save")
     , txt env 20 74 14 (gray 0.67) "Bottom right = the (u, v) image (texture). Sphere and torus wear the same image: press 1/2 to compare"
     , box env (ox - 8) (oy - 30) (tw + 16) (th + 38) (withA (150 / 255) black)
     , at (px env (ox + tw / 2) (oy + th / 2)) (thumbnail (wTex w))
     , txt env ox (oy - 10) 14 (gray 0.86) "(u, v) image: across = u, vertical = v"
     ]

-- ---------- やってみよう ----------
-- 1. 球面の北極の近くに描くと、右下の画像では横に長く伸びる。でも球の上では丸い（stamps の ew の計算）
-- 2. トーラスで u 方向に 1 周する線と v 方向に 1 周する線を描こう。どちらも縮めて消せない輪（シリーズ 27）
-- 3. 球面で描いた絵を 2 でトーラスに貼ると、極のまわりの模様はどこへ行く？
-- 4. surfDef に自分の面を追加して描いてみよう（03 や 05 の面の式が使える）。音の決め方（toFreq）も変えよう

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
