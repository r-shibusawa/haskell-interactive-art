-- ============================================
-- 06 測地線
-- 面の上で「まっすぐ進む」線 = 測地線。左右にハンドルを切らずに歩くと、球面では大円になる
--   測地線の方程式 u″ᵏ + Γᵏᵢⱼ u′ⁱ u′ʲ = 0 を数値積分（RK4）。Γ（クリストッフェル記号）は E, F, G の微分から
-- クリック = 出発点 / ←→ = 出発の向き / ↑↓ = 長さ / F = 扇（9 本同時）/ L = 地図上の直線と比べる / 1 = 球面 2 = トーラス 3 = 円柱 4 = 丘 / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Numeric (showFFloat)

ds :: D
ds = 0.012                 -- ★ 積分の刻み（小さいほど正確）

stepsPerFrame :: Int
stepsPerFrame = 240        -- ★ 1 フレームに進める積分の回数（画面が止まらないように少しずつ計算する）

data SurfDef = SurfDef { sName :: String, sU, sV :: !(D, D), sWrapU, sWrapV :: !Bool, sF :: Surf, sClosed :: !Bool, sNote :: String }

surfDef :: Int -> SurfDef
surfDef 1 = SurfDef "sphere" (-pi, pi) (-1.5, 1.5) True False (\u v -> D3 (cos v * cos u) (cos v * sin u) (sin v)) True
  "Sphere: geodesic = great circle. Wavy on the map (it stops near a pole: the coordinates break there)"
surfDef 3 = SurfDef "cylinder" (-pi, pi) (-1.2, 1.2) True False (\u v -> D3 (0.6 * cos u) (0.6 * sin u) v) False
  "Cylinder: geodesics are helices. Cut it open and they are straight lines (straight on the map too)"
surfDef 4 = SurfDef "hill (Gaussian bump)" (-1.6, 1.6) (-1.6, 1.6) False False (\u v -> D3 u v (0.7 * exp (-4 * (u * u + v * v)))) False
  "Hill: geodesics bend around the peak (like a gravitational lens)"
surfDef _ = SurfDef "torus" (-pi, pi) (-pi, pi) True True
  (\u v -> D3 ((1 + 0.45 * cos v) * cos u) ((1 + 0.45 * cos v) * sin u) (0.45 * sin v)) True
  "Torus: circles the outside, gets turned back inside, winds around... many behaviors"

meshOf :: Int -> [Quad]
meshOf k = let s = surfDef k
           in makeSurface (sF s) (sU s) (sV s) (64, 48) (16, 12) (\_ _ -> (70 / 255, 95 / 255, 140 / 255, 1))

mesh1, mesh2, mesh3, mesh4 :: [Quad]
mesh1 = meshOf 1
mesh2 = meshOf 2
mesh3 = meshOf 3
mesh4 = meshOf 4

-- 測地線の 1 点：(u, v) と、面から少し浮かせた 3D の点
data Pt = Pt !D !D !V3

-- 積分中の測地線
data St = St !D !D !D !D

data Geo = Geo
  { gSt   :: !St
  , gLeft :: !Int          -- 残りの積分回数（止まったら 0）
  , gRev  :: [Pt]          -- ここまでの点（新しい順）
  , gN    :: !Int
  , gCol  :: !Color
  }

data World = World
  { wKind :: !Int
  , wSu, wSv :: !D         -- 出発点 (u, v)
  , wTheta :: !D           -- ★ 出発の向き（∂r/∂u からの角度）
  , wLen :: !D             -- ★ 測地線の長さ
  , wFan, wShowLine :: !Bool
  , wGeos :: [Geo]
  , wStraight :: !(Maybe [Pt])  -- 比べる線（測地線ができてから計算する）
  , wWalker :: !Int
  , wPress :: !(Maybe Point)
  , wCam :: !Cam
  }

main :: IO ()
main = runSketch (sketch "28-06 Geodesics" (computePaths (World 2 0 0.2 0.5 9 False True [] Nothing 0 Nothing cam0)))
  { skBack  = rgb255 16 18 28
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- 第一基本形式 E, F, G と接ベクトル
data Metric = Metric !D !D !D !D3 !D3

metric :: Surf -> D -> D -> Metric
metric f u v =
  let h = 0.001
      ru = vmul (vsub (f (u + h) v) (f (u - h) v)) (1 / (2 * h))
      rv = vmul (vsub (f u (v + h)) (f u (v - h))) (1 / (2 * h))
  in Metric (vdot ru ru) (vdot ru rv) (vdot rv rv) ru rv

-- ★ 測地線の方程式：加速度 = −Γ(速度, 速度)
accel :: Surf -> D -> D -> D -> D -> (D, D)
accel f u v du dv =
  let h = 0.002
      Metric e ff g _ _ = metric f u v
      Metric ae af ag _ _ = metric f (u + h) v
      Metric be bf bg _ _ = metric f (u - h) v
      Metric ce cf cg _ _ = metric f u (v + h)
      Metric de df dg _ _ = metric f u (v - h)
      eu = (ae - be) / (2 * h)
      fu = (af - bf) / (2 * h)
      gu = (ag - bg) / (2 * h)
      ev = (ce - de) / (2 * h)
      fv = (cf - df) / (2 * h)
      gv = (cg - dg) / (2 * h)
      w2 = 2 * (e * g - ff * ff)
      g1uu = (g * eu - 2 * ff * fu + ff * ev) / w2
      g1uv = (g * ev - ff * gu) / w2
      g1vv = (2 * g * fv - g * gu - ff * gv) / w2
      g2uu = (2 * e * fu - e * ev - ff * eu) / w2
      g2uv = (e * gu - ff * ev) / w2
      g2vv = (e * gv - 2 * ff * fv + ff * gu) / w2
  in ( -(g1uu * du * du + 2 * g1uv * du * dv + g1vv * dv * dv)
     , -(g2uu * du * du + 2 * g2uv * du * dv + g2vv * dv * dv) )

-- RK4 の 1 歩
rk4 :: Surf -> St -> St
rk4 f st@(St a b c d) =
  let fn (St u v du dv) = let (x, y) = accel f u v du dv in St du dv x y
      add (St p q r s) k (St x y z w) = St (p + k * x) (q + k * y) (r + k * z) (s + k * w)
      k1 = fn st
      k2 = fn (add st (ds / 2) k1)
      k3 = fn (add st (ds / 2) k2)
      k4 = fn (add st ds k3)
      St k1a k1b k1c k1d = k1
      St k2a k2b k2c k2d = k2
      St k3a k3b k3c k3d = k3
      St k4a k4b k4c k4d = k4
      w = ds / 6
  in St (a + w * (k1a + 2 * k2a + 2 * k3a + k4a))
        (b + w * (k1b + 2 * k2b + 2 * k3b + k4b))
        (c + w * (k1c + 2 * k2c + 2 * k3c + k4c))
        (d + w * (k1d + 2 * k2d + 2 * k3d + k4d))

-- 面から少し浮かせた点
lift :: Surf -> D -> D -> V3
lift f u v =
  let Metric _ _ _ ru rv = metric f u v
  in toV (vadd (f u v) (vmul (vunit (vcross ru rv)) 0.008))

-- 出発点から向き th の測地線を用意する（積分は step で少しずつ）
newGeo :: World -> Color -> D -> Geo
newGeo w col th =
  let f = sF (surfDef (wKind w))
      su = wSu w
      sv = wSv w
      Metric e ff g ru rv = metric f su sv
      e1 = vunit ru
      e2 = vunit (vsub rv (vmul e1 (vdot rv e1)))
      d = vadd (vmul e1 (cos th)) (vmul e2 (sin th))     -- 3D の単位ベクトル → (u′, v′) に直す
      p = vdot ru d
      q = vdot rv d
      wd = e * g - ff * ff
      n = round (wLen w / ds) :: Int
  in Geo (St su sv ((g * p - ff * q) / wd) ((e * q - ff * p) / wd)) n [Pt su sv (lift f su sv)] 1 col

computePaths :: World -> World
computePaths w =
  let hue k = let (r, g, b) = hsv k 0.65 1 in makeColor r g b 1
      geos = if wFan w
               then [ newGeo w (hue (fromIntegral j / 9)) (wTheta w + fromIntegral (j - 4) * 0.08) | j <- [0 .. 8 :: Int] ]
               else [ newGeo w (hue 0.13) (wTheta w) ]
  in w { wGeos = geos, wStraight = Nothing, wWalker = 0 }

-- 測地線を k 歩進める。端（球面なら極の近く）や数値の破綻で止まる
advance :: SurfDef -> Int -> Geo -> Geo
advance s k g
  | k <= 0 || gLeft g <= 0 = g
  | otherwise =
      let f = sF s
          st@(St u v du dv) = rk4 f (gSt g)
          bad = any (\x -> isNaN x || isInfinite x) [u, v, du, dv] || abs du + abs dv > 1e3
          outV = not (sWrapV s) && (v < fst (sV s) || v > snd (sV s))
          outU = not (sWrapU s) && (u < fst (sU s) || u > snd (sU s))
      in if bad || outV || outU then g { gLeft = 0 } else
         advance s (k - 1) g { gSt = st, gLeft = gLeft g - 1, gRev = Pt u v (lift f u v) : gRev g, gN = gN g + 1 }

step :: World -> World
step w =
  let s = surfDef (wKind w)
      active = length (filter ((> 0) . gLeft) (wGeos w))
      k = if active == 0 then 0 else stepsPerFrame `div` active
      geos = map (advance s k) (wGeos w)
      w1 = w { wGeos = geos }
      w2 = case wStraight w1 of
             Nothing | all ((<= 0) . gLeft) geos -> w1 { wStraight = Just (straightLine w1) }
             _ -> w1
      n0 = case geos of { (g : _) -> max 1 (gN g); [] -> 1 }
  in w2 { wWalker = (wWalker w2 + 3) `mod` n0 }

-- 比べる線：(u, v) の地図の上でまっすぐな線（同じ出発の向き）
straightLine :: World -> [Pt]
straightLine w =
  let s = surfDef (wKind w)
      su = wSu w
      sv = wSv w
      fanIdx = if wFan w then 4 else 0
      g0 = case drop fanIdx (wGeos w) of { (g : _) -> reverse (gRev g); [] -> [] }
      g1 = if length g0 < 2 then [Pt su sv (V3 0 0 0), Pt (su + 0.001) sv (V3 0 0 0)] else g0
      n = length g1
      k = max 1 (min (n - 1) 25)
      (ku, kv) = case drop k g1 of { (Pt a b _ : _) -> (a, b); [] -> (su, sv) }
      du = (ku - su) / fromIntegral k
      dv = (kv - sv) / fromIntegral k
      inRange v = sWrapV s || (v >= fst (sV s) && v <= snd (sV s))
      f = sF s
  in [ Pt u v (lift f u v)
     | i <- takeWhile (\i -> inRange (sv + dv * fromIntegral i)) [0 .. n - 1]
     , let u = su + du * fromIntegral i
           v = sv + dv * fromIntegral i ]

viewOf :: Env -> World -> View
viewOf env w = let c = wCam w in mkView env (cRX c) (cRY c) (min (envW env) (envH env) * 0.3)

event :: Env -> Event -> World -> World
event env ev w0 =
  let w = w0 { wCam = camEvent ev (wCam w0) }
  in case ev of
       EventKey (MouseButton LeftButton) Down _ p -> w { wPress = Just p }
       EventKey (MouseButton LeftButton) Up _ p@(mx, my) -> case wPress w of
         Just q | dist2 p q < 5 && not (mx + envW env / 2 > envW env - 360 && envH env / 2 - my > envH env - 330) ->
           let s = surfDef (wKind w)
           in case pickUV (viewOf env w) (sF s) p (sU s) (sV s) of
                Just (u, v) -> computePaths w { wSu = u, wSv = max (fst (sV s) + 0.05) (min (snd (sV s) - 0.05) v), wPress = Nothing }
                Nothing -> w { wPress = Nothing }
         _ -> w { wPress = Nothing }
       EventKey (Char c) Down _ _
         | c >= '1' && c <= '4' ->
             let k = fromEnum c - fromEnum '0'
             in computePaths w { wKind = k, wSu = if k == 4 then -1.3 else 0, wSv = if k == 4 then -0.25 else 0.2, wTheta = if k == 4 then 0.15 else 0.5 }
         | toLower c == 'f' -> computePaths w { wFan = not (wFan w) }
         | toLower c == 'l' -> computePaths w { wShowLine = not (wShowLine w) }
       EventKey (SpecialKey KeyLeft) Down _ _ -> computePaths w { wTheta = wTheta w + 0.08 }
       EventKey (SpecialKey KeyRight) Down _ _ -> computePaths w { wTheta = wTheta w - 0.08 }
       EventKey (SpecialKey KeyUp) Down _ _ -> computePaths w { wLen = min 20 (wLen w + 1) }
       EventKey (SpecialKey KeyDown) Down _ _ -> computePaths w { wLen = max 1 (wLen w - 1) }
       _ -> w

-- 間引いて n 点ごとに取り出す（最後の点は必ず残す）
every :: Int -> [a] -> [a]
every n xs = case xs of
  [] -> []
  (x : rest) -> x : go (n - 1) rest
  where
    go _ [] = []
    go _ [y] = [y]
    go 0 (y : ys) = y : go (n - 1) ys
    go k (_ : ys) = go (k - 1) ys

draw :: Env -> World -> Picture
draw env w =
  let view = viewOf env w
      s = surfDef (wKind w)
      f = sF s
      stride = if wFan w then 6 else 3
      ptV (Pt _ _ v) = v
      paths3 = concat [ pathItems view (gCol g) 3 (map ptV (every stride (reverse (gRev g)))) | g <- wGeos w ]
      straight3 = case wStraight w of
        Just pts | wShowLine w && length pts > 1 -> pathItems view (rgb255 200 200 210) 2 (map ptV (every stride pts))
        _ -> []
      walkers = concat
        [ ballItem view white 6 (toV (f u v))
        | g <- wGeos w
        , Just (Pt u v _) <- [listToMaybe (drop (max 0 (gN g - 1 - min (wWalker w) (gN g - 1))) (gRev g))] ]
      start = ballItem view (rgb255 255 90 90) 8 (toV (f (wSu w) (wSv w)))
      mesh = case wKind w of { 1 -> mesh1; 2 -> mesh2; 3 -> mesh3; _ -> mesh4 }
      surface = surfaceItems view (120 / 255) 0.83 (sClosed s) (rgb255 120 145 200) 0 mesh
  in Pictures [scene (surface ++ paths3 ++ straight3 ++ walkers ++ start), drawHud env w s stride]

-- 右下：(u, v) の地図
drawHud :: Env -> World -> SurfDef -> Int -> Picture
drawHud env w s stride =
  let wd = 320 :: Float
      hg = 240 :: Float
      ox = envW env - wd - 30
      oy = envH env - hg - 50
      (u0, u1) = sU s
      (v0, v1) = sV s
      wrapTo a b x = let r = b - a in (x - a) - r * fromIntegral (floor ((x - a) / r) :: Int)
      xOf u = ox + f2 (wrapTo u0 u1 u / (u1 - u0)) * wd
      yOf v = oy + hg - f2 ((if sWrapV s then wrapTo v0 v1 v else v - v0) / (v1 - v0)) * hg
      -- 飛び越す所（貼り合わせの辺）で切る
      pieces :: [Pt] -> [[Point]]
      pieces = go []
        where
          flush cur = [cur | length cur >= 2]
          go cur [] = flush cur
          go cur (Pt u v _ : rest) =
            let q@(x, y) = (xOf u, yOf v)
            in case cur of
                 ((lx, ly) : _) | abs (x - lx) >= wd / 2 || abs (y - ly) >= hg / 2 -> flush cur ++ go [q] rest
                 _ -> go (q : cur) rest
      mapPath c pts = Color c (Pictures (map (thickPath 2 . map (uncurry (px env))) (pieces (every (stride * 2) pts))))
      straightPic = case wStraight w of
        Just pts | wShowLine w -> [mapPath (rgb255 200 200 210) pts]
        _ -> []
      onOff b = if b then "ON" else "off"
      (rx, ry) = px env (ox + wd / 2) (oy + hg / 2)
  in Pictures $
     [ txt env 20 34 16 white ("Click: start   L/R: dir " ++ nf 0 (wTheta w * 180 / pi) ++ " deg   U/D: length " ++ nf 1 (wLen w) ++ "   F: fan " ++ onOff (wFan w) ++ "   L: map line " ++ onOff (wShowLine w) ++ "   1-4: " ++ sName s)
     , txt env 20 60 14 (gray 0.67) "Yellow = geodesic (straight on the surface). Gray = a straight line on the (u, v) map: it bends on the surface"
     , box env (ox - 10) (oy - 34) (wd + 20) (hg + 50) (withA (160 / 255) black)
     , txt env ox (oy - 12) 14 white "Seen on the (u, v) map (chart)"
     , Color (withA (60 / 255) white) (rectLine rx ry wd hg)
     ] ++ straightPic ++
     [ mapPath (gCol g) (reverse (gRev g)) | g <- wGeos w ] ++
     [ at (px env (xOf (wSu w)) (yOf (wSv w))) (Color (rgb255 255 90 90) (disc 5))
     , txt env 20 (envH env - 24) 14 (gray 0.78) (sNote s)
     ]

-- ---------- やってみよう ----------
-- 1. 球面で F（扇）。赤道から出た 9 本は反対側の 1 点で再び集まる（正の曲率は測地線を集める）
-- 2. 丘で F。山の両側を通った線は広がる？ 集まる？ 山のふもと（K < 0 の輪）に注目
-- 3. 円柱の測地線は (u, v) の地図でもまっすぐ。円柱は「平らな紙を丸めたもの」（05 で K = 0）だから
-- 4. ds を 0.05 にすると誤差でどうなる？ RK4 をオイラー法（1 段）に変えて比べよう

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
