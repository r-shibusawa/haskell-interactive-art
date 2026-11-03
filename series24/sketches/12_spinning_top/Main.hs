-- ============================================
-- 12 コマの歳差運動
-- 傾いたコマは倒れずに、軸がゆっくり円を描く（歳差運動）。軸の先はうねうねと上下もする（章動）
--   重力のトルクが角運動量の「向き」を少しずつ変える。オイラー角 (θ, φ, ψ) のラグランジュ方程式を RK4 で解く
-- ↑↓ = 回転の速さ / ←→ = 最初の傾き / 1 / 2 / 3 = 最初の横向きの速さ（とがる・なめらか・輪）/ ドラッグ = 見る向き / スペース = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Foldable (foldl', toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

moI1, moI3, mgl, lenP :: Float
moI1 = 1.0      -- ★ 支点のまわりの横向きの慣性モーメント
moI3 = 0.5      -- ★ 軸のまわりの慣性モーメント
mgl = 2.0       -- ★ 質量 × 重力 × 支点から重心までの距離
lenP = 200      -- コマの大きさ [px]

histLen :: Int
histLen = 600

data World = World
  { wW3     :: !Float       -- ★ 軸のまわりの回転の速さ ω₃ [rad/s]
  , wTilt   :: !Float       -- ★ 最初の傾き θ₀ [rad]
  , wPreset :: !Int
  , wTh, wThd, wPhi, wPsi :: !Float   -- 状態
  , wPphi   :: !Float       -- 保存量 p_φ
  , wTrail  :: !(Seq.Seq V3)
  , wViewX, wViewY :: !Float
  , wMouse  :: !Point
  , hTh, hPd :: !(Seq.Seq Float)
  , wFalls  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "24-12 Spinning Top" (restart world0))
  { skBack  = hsb 250 30 9
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = World 12 0.5 1 0 0 0 0 0 Seq.empty (-0.25) 0.3 (0, 0) Seq.empty Seq.empty False

slowPrecession :: Float -> Float
slowPrecession w3 = mgl / (moI3 * w3)   -- 速いコマの近似 Ω ≈ Mgl / (I₃ω₃)

restart :: World -> World
restart w =
  let sp = slowPrecession (wW3 w)
      pd0 = case wPreset w of
        1 -> 0
        2 -> sp
        _ -> -sp * 1.5
      th = wTilt w
      s = sin th
  in w { wTh = th, wThd = 0, wPhi = 0, wPsi = 0
       , wPphi = moI1 * pd0 * s * s + moI3 * wW3 w * cos th   -- ★ 鉛直軸まわりの角運動量（保存）
       , wTrail = Seq.empty, hTh = Seq.empty, hPd = Seq.empty, wFalls = False }

phidotOf :: Float -> Float -> Float -> Float
phidotOf pphi w3 t =
  let s = sin t
  in (pphi - moI3 * w3 * cos t) / (moI1 * max (s * s) 1e-6)

-- ★ θ の運動方程式（ψ' + φ' cos θ = ω₃ は一定）
thetaAcc :: Float -> Float -> Float -> Float
thetaAcc pphi w3 t =
  let pd = phidotOf pphi w3 t
  in (moI1 * pd * pd * sin t * cos t - moI3 * w3 * pd * sin t + mgl * sin t) / moI1

stepWorld :: World -> World
stepWorld w0' =
  let sub = 20 :: Int
      h = 1 / 60 / fromIntegral sub
      pphi = wPphi w0'
      w3 = wW3 w0'
      f = thetaAcc pphi w3
      loop :: Int -> Float -> Float -> Float -> Float -> (Float, Float, Float, Float)
      loop 0 !th !thd !phi !psi = (th, thd, phi, psi)
      loop k !th !thd !phi !psi =
        let k1t = thd
            k1d = f th
            k2t = thd + k1d * h / 2
            k2d = f (th + k1t * h / 2)
            k3t = thd + k2d * h / 2
            k3d = f (th + k2t * h / 2)
            k4t = thd + k3d * h
            k4d = f (th + k3t * h)
            th0 = th + h / 6 * (k1t + 2 * k2t + 2 * k3t + k4t)
            thd0 = thd + h / 6 * (k1d + 2 * k2d + 2 * k3d + k4d)
            th1 = if isBad th0 then wTilt w0' else clampF 0.01 1.75 th0      -- 床（θ ≈ 100°）より倒れない
            thd1 = if isBad thd0 then 0 else clampF (-50) 50 thd0
            pd = clampF (-200) 200 (phidotOf pphi w3 th1)
        in loop (k - 1) th1 thd1 (phi + pd * h) (psi + (w3 - pd * cos th1) * h)
      (th2, thd2, phi2, psi2) = loop sub (wTh w0') (wThd w0') (wPhi w0') (wPsi w0')
      -- 軸の向き（物理の座標：z が上）→ p5 の座標（y が下）
      st = sin th2
      tip = V3 (st * cos phi2 * lenP * 1.3) (-(cos th2) * lenP * 1.3) (st * sin phi2 * lenP * 1.3)
      tr = wTrail w0' Seq.|> tip
  in w0' { wTh = th2, wThd = thd2, wPhi = wrapF tau phi2, wPsi = wrapF tau psi2
         , wFalls = wFalls w0' || th2 >= 1.74
         , hTh = pushHist (hTh w0') (th2 * 180 / pi)
         , hPd = pushHist (hPd w0') (phidotOf pphi w3 th2)
         , wTrail = if Seq.length tr > 900 then Seq.drop 1 tr else tr }

isBad :: Float -> Bool
isBad v = isNaN v || isInfinite v

-- ---- 3D の面（Face）を作って描く
data Face = Face ![V3] !V3 !(Float, Float, Float)   -- 角、外向きの法線、HSB の色

-- y 軸に沿った円柱：半径 r、y0〜y1、側面の分割 n、長さ方向の分割 m（ふたも扇形に分ける）
cylinderFaces :: Float -> Float -> Float -> Int -> Int -> (Float, Float, Float) -> [Face]
cylinderFaces r y0 y1 n m col =
  [ Face [ V3 (r * cos a) ya (r * sin a), V3 (r * cos b) ya (r * sin b)
         , V3 (r * cos b) yb (r * sin b), V3 (r * cos a) yb (r * sin a) ]
         (V3 (cos ((a + b) / 2)) 0 (sin ((a + b) / 2))) col
  | i <- [0 .. n - 1], j <- [0 .. m - 1]
  , let a = ang i, let b = ang (i + 1)
  , let ya = lerpF y0 y1 (fromIntegral j / fromIntegral m)
  , let yb = lerpF y0 y1 (fromIntegral (j + 1) / fromIntegral m) ]
  ++ concat [ [ Face [V3 0 y1 0, V3 (r * cos a) y1 (r * sin a), V3 (r * cos b) y1 (r * sin b)] (V3 0 1 0) col
              , Face [V3 0 y0 0, V3 (r * cos a) y0 (r * sin a), V3 (r * cos b) y0 (r * sin b)] (V3 0 (-1) 0) col ]
            | i <- [0 .. n - 1], let a = ang i, let b = ang (i + 1) ]
  where
    ang :: Int -> Float
    ang i = fromIntegral i / fromIntegral n * tau

-- 中心 c、大きさ (sx, sy, sz) の箱
boxFaces :: V3 -> Float -> Float -> Float -> (Float, Float, Float) -> [Face]
boxFaces c sx sy sz col =
  [ face (V3 s 0 0) (V3 0 sy 0) (V3 0 0 sz) (s * sx / 2) | s <- [1, -1] ]
  ++ [ face (V3 0 s 0) (V3 sx 0 0) (V3 0 0 sz) (s * sy / 2) | s <- [1, -1] ]
  ++ [ face (V3 0 0 s) (V3 sx 0 0) (V3 0 sy 0) (s * sz / 2) | s <- [1, -1] ]
  where
    face n u v off =
      let pt a b = add3 c (add3 (scale3 off n) (add3 (scale3 a u) (scale3 b v)))
      in Face [pt (-0.5) (-0.5), pt 0.5 (-0.5), pt 0.5 0.5, pt (-0.5) 0.5] n col

-- 円すい（先が y = 0、底が y = -h）
coneFaces :: Float -> Float -> Int -> (Float, Float, Float) -> [Face]
coneFaces r h n col =
  [ Face [V3 0 0 0, V3 (r * cos a) (-h) (r * sin a), V3 (r * cos b) (-h) (r * sin b)]
         (unit3 (V3 (h * cos ((a + b) / 2)) r (h * sin ((a + b) / 2)))) col
  | i <- [0 .. n - 1]
  , let a = fromIntegral i / fromIntegral n * tau
  , let b = fromIntegral (i + 1) / fromIntegral n * tau ]

-- コマ本体（自転 ψ で回る前の形）
topFaces :: [Face]
topFaces =
  cylinderFaces 4 (-lenP * 1.3) 0 6 8 (0, 0, 80)                       -- 軸
  ++ cylinderFaces 75 (-lenP - 12) (-lenP + 12) 24 1 (30, 80, 100)     -- 円盤
  ++ boxFaces (V3 60 (-lenP - 14) 0) 26 6 26 (200, 80, 100)            -- 自転が見える目印
  ++ coneFaces 10 16 12 (0, 0, 60)

camD, offX, offY :: Float
camD = 400 / tan (pi / 6)  -- p5 の標準カメラの距離（高さ 800 の画面）
offX = -width * 0.2
offY = 140

lightDir :: V3
lightDir = unit3 (V3 (-0.4) (-0.7) 1)   -- 光が来る方向

drawAll :: World -> Picture
drawAll w = Pictures
  [ floorPic
  , Pictures (map snd (depthSort fst (trailItems ++ concatMap faceItem topFaces)))
  , drawHUD w
  , txt 20 30 17 (gray 1) ("w3 = " ++ nf 1 (wW3 w) ++ " rad/s (Up/Down)   Tilt = " ++ nf 0 (wTilt w * 180 / pi) ++ " deg (Left/Right)   " ++ show (wPreset w) ++ ": " ++ presetName)
  , txt 20 54 14 (gray 0.93) ("Precession rate = " ++ nf 2 (phidotOf (wPphi w) (wW3 w) (wTh w)) ++ " rad/s (fast-top estimate Mgl/(I3 w3) = " ++ nf 2 (slowPrecession (wW3 w)) ++ ")   theta = " ++ nf 1 (wTh w * 180 / pi) ++ " deg")
  , txt 20 76 14 (hsb 0 0 67) "Yellow = path of the axis tip. Drag: view   Space: restart"
  , if wFalls w then txt 560 76 14 (hsb 0 60 100) "It fell over! Raise w3" else Blank
  ]
  where
    presetName = case wPreset w of
      1 -> "released at rest: sharp cusps"
      2 -> "at precession speed: smooth circle"
      _ -> "pushed backward: loops"
    viewRot = rotX (wViewX w) . rotY (wViewY w)
    toView = add3 (V3 offX offY 0) . viewRot
    screen v = let (x, y) = project camD v in (x, -y)
    -- コマ本体：z-x-z のオイラー角で回す（rotY が鉛直軸まわり）
    bodyRot = rotY (-(wPhi w)) . rotZ (wTh w) . rotY (-(wPsi w))
    faceItem (Face corners n (h, s, br)) =
      let pts = map (toView . bodyRot) corners
          nv = viewRot (bodyRot n)
          centre = scale3 (1 / fromIntegral (length pts)) (foldr add3 (V3 0 0 0) pts)
          V3 _ _ zc = centre
          facing = dot3 nv (sub3 centre (V3 0 0 camD)) < 0
          f = 0.45 + max 0 (dot3 nv lightDir)
      in [(zc, Color (hsb h s (br * f)) (Polygon (map screen pts))) | facing]
    -- 床
    floorPic =
      let corners = [V3 (-350) 0 (-350), V3 350 0 (-350), V3 350 0 350, V3 (-350) 0 350]
          f = 0.45 + max 0 (dot3 (viewRot (V3 0 (-1) 0)) lightDir)
      in Color (hsb 220 20 (20 * f)) (Polygon (map (screen . toView) corners))
    -- 軸の先の軌跡
    trailPts = every2 (toList (wTrail w))
    trailItems =
      [ (zm, Color (hsb 55 90 100) (thickLine 2 (screen a) (screen b)))
      | (p, q) <- zip trailPts (drop 1 trailPts)
      , let a = toView p
      , let b = toView q
      , let V3 _ _ zm = scale3 0.5 (add3 a b) ]

drawHUD :: World -> Picture
drawHUD w = Pictures
  [ view [Color (hsba 0 0 12 0.9) (boxAt x0 y0 bw bh)]
  , plot (hTh w) tmin tmax 0 (bh / 2 - 10) 30 "Tilt theta [deg] (wobble = nutation)"
  , view [Color (hsb 0 0 35) (Line [(x0, yz), (x0 + bw, yz)])]
  , plot (hPd w) pmin pmax (bh / 2) (bh / 2 - 10) 200 "Precession rate [rad/s]"
  ]
  where
    x0 = width * 0.55
    y0 = height * 0.18
    bw = 537
    bh = 400
    d0 = wTilt w * 180 / pi
    tmin = foldl' min d0 (hTh w) - 2
    tmax = foldl' max d0 (hTh w) + 2
    pmin = foldl' min 0 (hPd w) - 0.2
    pmax = foldl' max 0 (hPd w) + 0.2
    yz = y0 + remap 0 pmin pmax (bh - 10) (bh / 2 + 24)
    plot d lo hi top hh hue title =
      let vals = toList d
          off = histLen - length vals
      in Pictures
           [ view [Color (hsb hue 70 100) (thickPath 2
               [ ( x0 + remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) 0 bw
                 , y0 + remap (clampF lo hi v) lo hi (top + hh) (top + 24) )
               | (i, v) <- zip [0 :: Int ..] vals, even i ])]
           , txt (x0 + 10) (y0 + top + 20) 14 (hsb hue 60 100) title ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> w { wMouse = pos }
  EventMotion pos@(mx, my)
    | envDown env ->
        let (px, py) = wMouse w
        in w { wViewY = wViewY w + (mx - px) * 0.01
             , wViewX = wViewX w + (my - py) * 0.01     -- y の向きが p5 と逆なので足す
             , wMouse = pos }
    | otherwise -> w { wMouse = pos }
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wW3 = min (wW3 w + 2) 40 }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wW3 = max (wW3 w - 2) 2 }
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wTilt = min (wTilt w + 0.1) 1.4 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wTilt = max (wTilt w - 0.1) 0.1 }
  EventKey (Char '1') Down _ _ -> restart w { wPreset = 1 }
  EventKey (Char '2') Down _ _ -> restart w { wPreset = 2 }
  EventKey (Char '3') Down _ _ -> restart w { wPreset = 3 }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ω₃ を上げると歳差はゆっくりに、章動は小さく速くなる（Ω ≈ Mgl / (I₃ω₃)）
-- 2. ω₃ を 2〜4 まで下げると？ 遅いコマは大きくよろめいて倒れる
-- 3. 1・2・3 で軸の先の軌跡の形（とがる・円・輪）を比べる。地球の自転軸も 2 万 6 千年で歳差運動している
-- 4. mgl をマイナスにすると「ぶら下がったコマ」になる。どう動く？


-- ==================================================
-- 道具（グラフの道具）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標（左上が原点、y は下向き）で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

every2 :: [a] -> [a]
every2 (a : _ : c : rest) = a : every2 (c : rest)
every2 xs = xs

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'
