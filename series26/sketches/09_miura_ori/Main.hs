-- ============================================
-- 09 ミウラ折り（3D）
-- 平行四辺形をジグザグに並べた折り目。1 か所を引くだけで全体が一度に開閉する（自由度 1・剛体折り）
--   三浦公亮さんが考え、1995 年の宇宙実験衛星 SFU の太陽電池パネルや、地図の折り方に使われた
-- ←→ = 折る量 / A = 自動で開閉 / ↑↓ = 平行四辺形の角 γ / 1・2 = 色 / ドラッグ = 視点 / ホイール = 近づく
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

nx, ny :: Int
nx = 12                  -- ★ 平行四辺形の数（横）
ny = 10                  -- ★ 平行四辺形の数（縦）

sideA, sideB :: Float
sideA = 40               -- ★ 平行四辺形の 2 辺（px）
sideB = 40

data World = World
  { wGamma  :: !Int      -- ★ 平行四辺形の角 γ（度）
  , wFold   :: !Float    -- 折る量 θ（0 = 平ら、π/2 = たたみきり）
  , wAuto   :: !Bool
  , wDir    :: !Float
  , wColor  :: !Int
  , wCurve  :: [Float]   -- 面積比のグラフ
  , wYaw    :: !Float
  , wPitch  :: !Float
  , wDist   :: !Float
  , wLast   :: !Point
  }

main :: IO ()
main = runSketch (sketch "26-09 Miura-ori" initial)
  { skBack  = rgb255 30 28 36
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

initial :: World
initial =
  let camX = 380
      camY = 480
      camZ = 520
  in World 60 0.5 True 1 0 (makeCurve 60)
       (atan2 (-camX) camZ)
       (atan2 camY (sqrt (camX * camX + camZ * camZ)))
       (sqrt (camX * camX + camY * camY + camZ * camZ))
       (0, 0)

-- ★ ミウラ折りの単位の大きさ（Schenk & Guest 2013 の式）
--   H = 高さ、S・L = 横・縦の間隔、V = ジグザグのずれ
data Cell = Cell { cH, cS, cL, cV :: !Float }

cell :: Int -> Float -> Cell
cell gammaDeg th =
  let g = fromIntegral gammaDeg * pi / 180
      tg = tan g
      q = sqrt (1 + cos th ^ (2 :: Int) * tg * tg)
  in Cell
       (sideA * sin th * sin g)
       (sideB * cos th * tg / q)
       (sideA * sqrt (max 0 (1 - sin th ^ (2 :: Int) * sin g ^ (2 :: Int))))
       (sideB / q)

-- 頂点 (i, j) の位置（y は上向き）
node :: Cell -> Int -> Int -> V3
node c i j =
  V3 (fromIntegral i * cS c)
     (fromIntegral (j `mod` 2) * cH c)
     (fromIntegral j * cL c + fromIntegral (i `mod` 2) * cV c)

areaOf :: Int -> Float -> Float
areaOf g th = let c = cell g th in (fromIntegral nx * cS c) * (fromIntegral ny * cL c + cV c)

makeCurve :: Int -> [Float]
makeCurve g =
  let a0 = areaOf g 0
  in [areaOf g (fromIntegral k / 60 * (pi / 2)) / a0 | k <- [0 .. 60 :: Int]]

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let w1 = if isDown env (SpecialKey KeyRight) then w0 { wFold = min (pi / 2 - 0.02) (wFold w0 + 0.01), wAuto = False } else w0
      w2 = if isDown env (SpecialKey KeyLeft) then w1 { wFold = max 0 (wFold w1 - 0.01), wAuto = False } else w1
      w3 | wAuto w2 =
             let f = wFold w2 + 0.006 * wDir w2
                 d | f > pi / 2 - 0.05 = -1
                   | f < 0.02 = 1
                   | otherwise = wDir w2
             in w2 { wFold = f, wDir = d }
         | otherwise = w2
  in pure w3

toView :: World -> V3 -> V3
toView w = rotX (wPitch w) . rotY (wYaw w)

persp :: World -> V3 -> Point
persp w v = let (x, y) = project (wDist w) v
                k = 800 / wDist w
            in (x * k, y * k)

-- 光の当たり方：環境光 + 2 つの平行光（面の表裏は区別しない）
shadeCol :: V3 -> (Float, Float, Float) -> Color
shadeCol n (r, g, b) =
  let u = unit3 n
      l1 = unit3 (V3 (-0.4) (-1) (-0.6))
      l2 = unit3 (V3 0.5 0.4 0.6)
      k = min 1 (110 / 255 + 0.7 * abs (dot3 u l1) + 0.3 * abs (dot3 u l2))
  in makeColor (r * k) (g * k) (b * k) 1

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let c = cell (wGamma w) (wFold w)
      mw = fromIntegral nx * cS c
      mh = fromIntegral ny * cL c + cV c
      off = V3 (-mw / 2) 0 (-mh / 2)
      quad i j =
        let vs = [node c i j, node c (i + 1) j, node c (i + 1) (j + 1), node c i (j + 1)]
            world = map (add3 off) vs
            view = map (toView w) world
            ptsv = map (persp w) view
            normal = case world of
              [p, q, _, s] -> cross3 (sub3 q p) (sub3 s p)
              _ -> V3 0 1 0
            base | wColor w == 0 = if odd (i + j) then (246 / 255, 240 / 255, 226 / 255) else (226 / 255, 84 / 255, 72 / 255)
                 | otherwise = ((80 + 140 * fromIntegral i / fromIntegral nx) / 255, (120 + 100 * fromIntegral j / fromIntegral ny) / 255, 220 / 255)
            depth = sum [z | V3 _ _ z <- view] / 4
        in (depth, Pictures [Color (shadeCol normal base) (Polygon ptsv), Color (rgba255 40 30 30 120) (poly ptsv)])
      scene = map snd (depthSort fst [quad i j | i <- [0 .. nx - 1], j <- [0 .. ny - 1]])
  in Pictures (scene ++ [drawHud env w c])

drawHud :: Env -> World -> Cell -> Picture
drawHud env w c =
  let c0 = cell (wGamma w) 0
      wr = cS c / cS c0
      lr = (fromIntegral ny * cL c + cV c) / (fromIntegral ny * cL c0 + cV c0)
      ar = wr * lr
      -- 面積比のグラフ
      gx = envW env - 360
      gy = envH env - 60
      gw = 320
      gh = 180
      (bx, by) = sc env (gx - 40 + (gw + 60) / 2, gy - gh - 40 + (gh + 80) / 2)
      curvePts = [sc env (gx + gw * fromIntegral k / 60, gy - gh * v) | (k, v) <- zip [0 :: Int ..] (wCurve w)]
      mx = gx + gw * wFold w / (pi / 2)
      my = gy - gh * ar
      graph =
        [ Color (rgba255 0 0 0 120) (rect bx by (gw + 60) (gh + 80))
        , Color (gray 0.47) (thickLine 1 (sc env (gx, gy)) (sc env (gx + gw, gy)))
        , Color (gray 0.47) (thickLine 1 (sc env (gx, gy)) (sc env (gx, gy - gh)))
        , Color (rgb255 255 190 40) (thickPath 2.5 curvePts)
        , Color white (at (sc env (mx, my)) (disc 5))
        , txt env (gx - 30) (gy - gh - 16) 14 (gray 0.78) "Area taken (flat = 100%)"
        , txt env (gx - 6) (gy + 18) 14 (gray 0.78) "0"
        , txt env (gx + gw - 70) (gy + 18) 14 (gray 0.78) "th -> 90"
        , txt env (gx - 36) (gy - gh + 4) 14 (gray 0.78) "100"
        ]
  in Pictures $
       [ txt env 20 40 18 white ("Left/Right: fold   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   Up/Down: angle gamma = " ++ show (wGamma w) ++ " deg   1, 2: color   Drag: view")
       , txt env 20 68 15 (gray 0.67) "No parallelogram bends; all fold angles change together. Shrink across and it also shrinks along."
       , txt env 20 110 20 (rgb255 255 190 40) ("Fold angle th = " ++ nf 1 (wFold w * 180 / pi) ++ " deg")
       , txt env 20 140 17 (gray 0.86) ("Width " ++ nf 1 (wr * 100) ++ "%   Length " ++ nf 1 (lr * 100) ++ "%")
       , txt env 20 166 17 (gray 0.86) ("Area " ++ nf 1 (ar * 100) ++ "%   Height " ++ nf 1 (cH c) ++ " px")
       ]
       ++ graph ++
       [ txt env 20 (envH env - 50) 14 (gray 0.67) "In space: fold it small for launch, then pull a single arm in orbit to open it."
       , txt env 20 (envH env - 30) 14 (gray 0.67) "The same arm folds it back (rigid folding: the paper never flexes)."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> w { wLast = p }
  EventKey (MouseButton WheelUp) Down _ _ -> w { wDist = max 300 (wDist w * 0.93) }
  EventKey (MouseButton WheelDown) Down _ _ -> w { wDist = min 2500 (wDist w * 1.07) }
  EventMotion p@(x, y) | envDown env ->
    let (lx, ly) = wLast w
    in w { wYaw = wYaw w + (x - lx) * 0.01
         , wPitch = clampF (-1.55) 1.55 (wPitch w - (y - ly) * 0.01)
         , wLast = p }
  EventKey (SpecialKey KeyUp) Down _ _ -> setGamma (min 85 (wGamma w + 5))
  EventKey (SpecialKey KeyDown) Down _ _ -> setGamma (max 20 (wGamma w - 5))
  EventKey (Char c) Down _ _
    | toLower c == 'a' -> w { wAuto = not (wAuto w) }
    | c == '1' -> w { wColor = 0 }
    | c == '2' -> w { wColor = 1 }
  _ -> w
  where setGamma g = w { wGamma = g, wCurve = makeCurve g }

-- ---------- やってみよう ----------
-- 1. γ を 85° にすると？ 20° にすると？ どちらがコンパクトにたためる？ グラフで比べよう
-- 2. sideA と sideB を変えて（例：50, 20）、細長い平行四辺形のミウラ折りをつくろう
-- 3. 1 つの頂点に集まる 4 本の折り目の角は γ, 180−γ, 180−γ, γ。08 の「ミウラ折りの頂点」と同じか確かめよう
-- 4. 紙で A4 をミウラ折りにしてみよう（ネットに折り方の型紙がある）。対角の 2 つの角を引くと一気に開く

-- ==================================================
-- ここから下は「画面の道具」
-- ==================================================

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
