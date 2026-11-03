-- ============================================
-- 11 剛体の回転：テニスラケットの定理（ジャニベコフ効果）
-- 3 つの軸で慣性モーメントが違う箱（I₁ < I₂ < I₃）。いちばん小さい軸・大きい軸のまわりの回転は安定、
--   まん中の軸のまわりの回転だけは不安定で、ときどきくるっと裏返る。オイラーの方程式を RK4 で解く
-- 1 / 2 / 3 = 回す軸 / ←→ = ずれの大きさ / ↑↓ = 時間の速さ / ドラッグ = 見る向き / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

dimX, dimY, dimZ, spin :: Float
dimX = 240      -- ★ 箱の大きさ [px]（x: 長い）
dimY = 140      -- ★ （y: まん中）
dimZ = 44       -- ★ （z: うすい）
spin = 4        -- ★ 回転の速さ [rad/s]

histLen :: Int
histLen = 600

-- 箱の慣性モーメント：I₁ ∝ b² + c² など（質量は 1 にそろえる）
i1, i2, i3 :: Float
i1 = (sq (dimY / 100) + sq (dimZ / 100)) / 12
i2 = (sq (dimX / 100) + sq (dimZ / 100)) / 12
i3 = (sq (dimX / 100) + sq (dimY / 100)) / 12

sq :: Float -> Float
sq v = v * v

-- 3 × 3 行列（行を並べたもの）
data M3 = M3 !V3 !V3 !V3

data World = World
  { wEps   :: !Float        -- ★ ほかの軸へのずれ（小さな揺らぎ）
  , wScale :: !Float
  , wOmega :: !V3           -- 体に固定した座標での角速度 ω
  , wR     :: !M3           -- 姿勢（体 → 世界の回転行列）
  , wAxis  :: !Int
  , wPaused :: !Bool
  , wViewX, wViewY :: !Float
  , wMouse :: !Point
  , h1, h2, h3 :: !(Seq.Seq Float)
  }

main :: IO ()
main = runSketch (sketch "24-11 Tennis Racket Theorem" (start 2 world0))
  { skBack  = hsb 225 35 9
  , skStep  = \_ _ w -> pure (if wPaused w then w else stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

identity :: M3
identity = M3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

world0 :: World
world0 = World 0.02 1 (V3 0 0 0) identity 2 False (-0.4) 0.5 (0, 0) Seq.empty Seq.empty Seq.empty

start :: Int -> World -> World
start k w =
  let e = wEps w * spin
      om = case k of
        1 -> V3 spin e e
        2 -> V3 e spin e
        _ -> V3 e e spin
  in w { wAxis = k, wOmega = om, wR = identity, h1 = Seq.empty, h2 = Seq.empty, h3 = Seq.empty }

-- ★ オイラーの方程式（外からのトルクなし）
eulerEq :: V3 -> V3
eulerEq (V3 a b c) = V3 ((i2 - i3) / i1 * b * c) ((i3 - i1) / i2 * c * a) ((i1 - i2) / i3 * a * b)

rk4 :: V3 -> Float -> V3
rk4 v h =
  let k1 = eulerEq v
      k2 = eulerEq (add3 v (scale3 (h / 2) k1))
      k3 = eulerEq (add3 v (scale3 (h / 2) k2))
      k4 = eulerEq (add3 v (scale3 h k3))
  in add3 v (scale3 (h / 6) (add3 k1 (add3 (scale3 2 k2) (add3 (scale3 2 k3) k4))))

stepWorld :: World -> World
stepWorld w =
  let sub = 20 :: Int
      h = wScale w / 60 / fromIntegral sub
      loop :: Int -> V3 -> M3 -> (V3, M3)
      loop 0 om r = (om, r)
      loop k om r =
        let wm = rk4 om (h / 2)              -- 刻みのまん中の ω で姿勢を回す
            !r' = mulM r (rodrigues wm h)    -- 姿勢の更新：R ← R · exp([ω]× h)
            !om' = rk4 om h
        in loop (k - 1) om' r'
      (om1, r1) = loop sub (wOmega w) (wR w)
      V3 a b c = om1
  in w { wOmega = om1, wR = orthonormalize r1
       , h1 = pushHist (h1 w) a, h2 = pushHist (h2 w) b, h3 = pushHist (h3 w) c }

-- ---- 3D の面（Face）を作って描く
data Face = Face ![V3] !V3 !(Float, Float, Float)   -- 角、外向きの法線、HSB の色

-- 中心 c、大きさ (sx, sy, sz) の箱。面を nx × ny × nz に分割すると、前後の順がくずれにくい
boxFaces :: V3 -> (Float, Float, Float) -> (Int, Int, Int) -> (Float, Float, Float) -> [Face]
boxFaces (V3 cx cy cz) (sx, sy, sz) (nx, ny, nz) col =
  concat [ grid (V3 s 0 0) (V3 0 sy 0) (V3 0 0 sz) ny nz (s * sx / 2) | s <- [1, -1] ]
  ++ concat [ grid (V3 0 s 0) (V3 sx 0 0) (V3 0 0 sz) nx nz (s * sy / 2) | s <- [1, -1] ]
  ++ concat [ grid (V3 0 0 s) (V3 sx 0 0) (V3 0 sy 0) nx ny (s * sz / 2) | s <- [1, -1] ]
  where
    grid n u v nu nv off =
      [ Face [ pt i j, pt (i + 1) j, pt (i + 1) (j + 1), pt i (j + 1) ] n col
      | i <- [0 .. nu - 1], j <- [0 .. nv - 1] ]
      where
        pt :: Int -> Int -> V3
        pt i j = add3 (V3 cx cy cz)
          (add3 (scale3 off n)
            (add3 (scale3 (fromIntegral i / fromIntegral nu - 0.5) u)
                  (scale3 (fromIntegral j / fromIntegral nv - 0.5) v)))

modelFaces :: [Face]
modelFaces =
  boxFaces (V3 0 0 0) (dimX, dimY, dimZ) (6, 4, 1) (210, 20, 85)
  -- 軸の棒（赤 = 1 軸、緑 = 2 軸、青 = 3 軸）と目印
  ++ boxFaces (V3 (dimX / 2 + 30) 0 0) (60, 8, 8) (2, 1, 1) (0, 80, 100)
  ++ boxFaces (V3 0 (dimY / 2 + 30) 0) (8, 60, 8) (1, 2, 1) (130, 80, 90)
  ++ boxFaces (V3 0 0 (dimZ / 2 + 30)) (8, 8, 60) (1, 1, 2) (220, 80, 100)
  ++ boxFaces (V3 (dimX * 0.3) (-dimY / 2 - 6) 0) (30, 12, 30) (1, 1, 1) (30, 90, 100)

camD, offX, offY :: Float
camD = 400 / tan (pi / 6)  -- p5 の標準カメラの距離（高さ 800 の画面）
offX = -width * 0.2
offY = 20

lightDir :: V3
lightDir = unit3 (V3 (-0.4) (-0.6) 1)   -- 光が来る方向（p5 の directionalLight の逆向き）

drawAll :: World -> Picture
drawAll w = Pictures
  [ Pictures (map snd (depthSort fst (lineItems ++ concatMap faceItem modelFaces)))
  , drawHUD w
  , txt 20 30 17 (gray 1) ("Spin axis: " ++ axisName ++ " (1/2/3)")
  , txt 20 54 14 (gray 0.93) ("eps = " ++ nf 3 (wEps w) ++ " (Left/Right)   Time x" ++ nf 2 (wScale w) ++ " (Up/Down)   Drag: view   Space: " ++ (if wPaused w then "resume" else "pause"))
  , txt 20 76 14 (hsb 0 0 67) ("I1 : I2 : I3 = 1.00 : " ++ nf 2 (i2 / i1) ++ " : " ++ nf 2 (i3 / i1) ++ "   Energy " ++ nf 4 en ++ "   |L| = " ++ nf 4 ln ++ " (both conserved)   Yellow = L (fixed direction)")
  ]
  where
    V3 a b c = wOmega w
    en = 0.5 * (i1 * a * a + i2 * b * b + i3 * c * c)
    -- 角運動量 L（世界座標では向きが変わらない）
    lb = V3 (i1 * a) (i2 * b) (i3 * c)
    lw = mulV (wR w) lb
    ln = norm3 lw
    lDir = unit3 lw
    axisName = case wAxis w of
      1 -> "1 (smallest I, long axis): stable"
      2 -> "2 (middle I): UNSTABLE, flips now and then"
      _ -> "3 (largest I, normal to the thin face): stable"
    viewRot = rotX (wViewX w) . rotY (wViewY w)
    toView = add3 (V3 offX offY 0) . viewRot
    screen v = let (x, y) = project camD v in (x, -y)
    faceItem (Face corners n (h, s, br)) =
      let pts = map (toView . mulV (wR w)) corners
          nv = viewRot (mulV (wR w) n)
          centre = scale3 (1 / fromIntegral (length pts)) (foldr add3 (V3 0 0 0) pts)
          V3 _ _ zc = centre
          facing = dot3 nv (sub3 centre (V3 0 0 camD)) < 0
          f = 0.45 + max 0 (dot3 nv lightDir)
      in [(zc, Color (hsb h s (br * f)) (Polygon (map screen pts))) | facing]
    -- L の線（少しずつ区切って、箱の前後に正しく重なるようにする）
    lineItems =
      [ (zm, Color (hsb 55 90 100) (thickLine 3 (screen p0) (screen p1)))
      | i <- [0 .. 12 :: Int]
      , let u0 = fromIntegral i / 13 * 260
      , let u1 = fromIntegral (i + 1) / 13 * 260
      , let p0 = toView (scale3 u0 lDir)
      , let p1 = toView (scale3 u1 lDir)
      , let V3 _ _ zm = scale3 0.5 (add3 p0 p1) ]

-- 右上：ω のグラフ
drawHUD :: World -> Picture
drawHUD w = Pictures
  [ view ( Color (hsba 0 0 12 0.9) (boxAt x0 y0 bw bh)
         : Color (hsb 0 0 35) (Line [(x0, y0 + bh / 2), (x0 + bw, y0 + bh / 2)])
         : zipWith curve [0, 130, 220] [h1 w, h2 w, h3 w] )
  , Pictures [txt (x0 + bw - 120 + k * 36) (y0 + 20) 14 (hsb h 70 100) n | (k, h, n) <- zip3 [0 ..] [0, 130, 220] ["w1", "w2", "w3"]]
  , txt (x0 + 10) (y0 + 20) 14 (hsb 0 0 75) "Body-frame angular velocity w (10 s)"
  , txt (x0 + 10) (y0 + bh - 12) 14 (hsb 0 0 75) "Spin axis 2: w2 swings between + and - = a flip"
  ]
  where
    x0 = width * 0.55
    y0 = height * 0.18
    bw = 537
    bh = 400
    lim = spin * 1.2
    curve hue d =
      let vals = toList d
          off = histLen - length vals
      in Color (hsb hue 70 100) (thickPath 2
           [ ( x0 + remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) 0 bw
             , y0 + remap (clampF (-lim) lim v) (-lim) lim (bh - 10) 30 )
           | (i, v) <- zip [0 :: Int ..] vals, even i ])

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
  EventKey (Char '1') Down _ _ -> start 1 w
  EventKey (Char '2') Down _ _ -> start 2 w
  EventKey (Char '3') Down _ _ -> start 3 w
  EventKey (SpecialKey KeyRight) Down _ _ -> start (wAxis w) w { wEps = min (wEps w * 2) 0.32 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> start (wAxis w) w { wEps = max (wEps w / 2) 0.0025 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wScale = min (wScale w * 2) 4 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wScale = max (wScale w / 2) 0.125 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 と 3 では箱はきれいに回り続ける。2 だけ、数秒ごとにくるっと裏返る（宇宙ステーションのナットで有名）
-- 2. ε を小さくすると、裏返るまでの時間はどう変わる？ 0 にはならない（不安定なので必ずいつか崩れる）
-- 3. dimY を 239 にして I₁ と I₂ を近づけると？
-- 4. スマホやテニスラケットを放り投げて、3 つの軸で回してみよう（落とさないように！）


-- ==================================================
-- 道具（3 × 3 行列と、グラフ）
-- ==================================================
mulV :: M3 -> V3 -> V3
mulV (M3 a b c) v = V3 (dot3 a v) (dot3 b v) (dot3 c v)

mulM :: M3 -> M3 -> M3
mulM (M3 a b c) m =
  let row r = V3 (dot3 r (colOf m 0)) (dot3 r (colOf m 1)) (dot3 r (colOf m 2))
  in M3 (row a) (row b) (row c)

colOf :: M3 -> Int -> V3
colOf (M3 (V3 a b c) (V3 d e f) (V3 g h i)) j = case j of
  0 -> V3 a d g
  1 -> V3 b e h
  _ -> V3 c f i

-- ロドリゲスの公式：軸 ω/|ω| のまわりに |ω|h 回す回転行列
rodrigues :: V3 -> Float -> M3
rodrigues v h
  | th < 1e-12 = identity
  | otherwise = M3 (V3 (c + x * x * k) (x * y * k - z * s) (x * z * k + y * s))
                   (V3 (y * x * k + z * s) (c + y * y * k) (y * z * k - x * s))
                   (V3 (z * x * k - y * s) (z * y * k + x * s) (c + z * z * k))
  where
    th = norm3 v * h
    V3 x y z = scale3 (h / th) v
    c = cos th
    s = sin th
    k = 1 - c

-- 計算誤差で行列がゆがまないように、列を直交に直す（グラム＝シュミット）
orthonormalize :: M3 -> M3
orthonormalize m =
  let c0@(V3 a d g) = unit3 (colOf m 0)
      c1raw = colOf m 1
      c1@(V3 b e h) = unit3 (sub3 c1raw (scale3 (dot3 c0 c1raw) c0))
      V3 c f i = cross3 c0 c1
  in M3 (V3 a b c) (V3 d e f) (V3 g h i)

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

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'
