-- ============================================
-- 09 距離センサーと障害物回避
-- ロボットは地図を持っていない。前方に扇形に飛ばした光線（LiDAR）の「距離」だけで動く
--   1 ブライテンベルク：左が近いと右へ、右が近いと左へ。反射だけで壁を避ける虫のような動き
--   2 ゴールへ：ゴールの方へ向かいつつ、前が詰まったら空いている側へ（bug 法のアイデア）
-- クリック = 障害物を置く / G = ゴールをマウスへ / 1・2 = 行動 / ↑↓ = センサーのノイズ / R = ロボットを戻す / C = 障害物を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import System.Random (StdGen, mkStdGen, randomR)
import Text.Printf (printf)

nRays :: Int
nRays = 15                -- ★ 光線の本数

fov, maxRange, vMax :: Float
fov = pi                  -- ★ 視野角（180°）
maxRange = 220            -- ★ 測れる最大距離
vMax = 2.2

data Obs = Obs !Float !Float !Float    -- x, y, 半径

data World = World
  { wX      :: !Float     -- ロボットの位置と向き
  , wY      :: !Float
  , wTh     :: !Float
  , wObs    :: ![Obs]
  , wGoal   :: !Point
  , wSigma  :: !Float     -- ★ 距離のノイズ（px）
  , wRanges :: ![Float]
  , wMode   :: !Int
  , wTrail  :: ![Point]   -- 軌跡（新しい順、1500 点まで）
  , wV      :: !Float     -- 画面表示用の速度
  }

main :: IO ()
main = runSketch (sketch "21-09 LiDAR Avoidance" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> step env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = resetRobot (World 0 0 0 startObs (width - 100, height - 100) 4 (replicate nRays maxRange) 2 [] 0)

resetRobot :: World -> World
resetRobot w = w { wX = 100, wY = 140, wTh = 0.5, wTrail = [] }

-- 乱数の種を固定した配置（p5 の randomSeed(11)）
startObs :: [Obs]
startObs = take 12 (go (mkStdGen 11))
  where
    go :: StdGen -> [Obs]
    go g0 =
      let (x, g1) = randomR (200, width - 200) g0
          (y, g2) = randomR (150, height - 60) g1
          (r, g3) = randomR (18, 50) g2
      in Obs x y r : go g3

rayAngle :: Float -> Int -> Float
rayAngle th i = th - fov / 2 + fov * fromIntegral i / fromIntegral (nRays - 1)

-- ★ 1 本の光線：角度 a で飛ばし、最初にぶつかる障害物（円）か画面の端までの距離
castRay :: [Obs] -> Float -> Float -> Float -> Float
castRay obs x y a =
  let dx = cos a
      dy = sin a
      hitCircle best (Obs ox oy r) =
        -- 円との交点（二次方程式）
        let fx = x - ox
            fy = y - oy
            b = 2 * (fx * dx + fy * dy)
            c = fx * fx + fy * fy - r * r
            d = b * b - 4 * c
            t = (-b - sqrt d) / 2
        in if d >= 0 && t > 0 && t < best then t else best
      -- 画面の端（壁）
      wallX | dx > 0 = (width - x) / dx
            | dx < 0 = -x / dx
            | otherwise = 1 / 0
      wallY | dy > 0 = (height - y) / dy
            | dy < 0 = (100 - y) / dy
            | otherwise = 1 / 0
      hitWall best t = if t > 0 && t < best then t else best
  in foldl' hitWall (foldl' hitCircle maxRange obs) [wallX, wallY]

-- 全部の光線を測る（ノイズつき）
sense :: World -> IO [Float]
sense w = do
  gs <- replicateM nRays gauss
  pure [ clampF 1 maxRange (castRay (wObs w) (wX w) (wY w) (rayAngle (wTh w) i) + g * wSigma w)
       | (i, g) <- zip [0 ..] gs ]

-- 真正面の光線の距離
frontOf :: [Float] -> Float
frontOf rs = case drop (nRays `div` 2) rs of
  (r : _) -> r
  [] -> maxRange

-- ★ 行動 1：ブライテンベルク。近い光線ほど強く反対側へ押す
braitenberg :: World -> (Float, Float)
braitenberg w =
  let rs = wRanges w
      half = fromIntegral (nRays - 1) / 2
      push turn (i, r) =
        let side = (fromIntegral i - half) / half    -- −1（左）〜 +1（右）
            closeness = 1 - r / maxRange
        in turn - side * closeness * closeness * 0.25    -- 右が近い → 左へ（マイナス）
      turn0 = foldl' push 0 (zip [0 :: Int ..] rs)
      front = frontOf rs
      turn = if front < 50 && abs turn0 < 0.02 then 0.1 else turn0   -- 真正面に壁 → とりあえず右へ
  in (vMax * min 1 (front / 120), turn)

-- ★ 行動 2：ゴールへ向かう＋避ける。ゴール方向に一番近い「空いている光線」を選ぶ
goToGoal :: World -> (Float, Float)
goToGoal w
  | dGoal < 15 = (0, 0)
  | otherwise = case best of
      (i, _) | i < 0 -> (0.3, 0.08)      -- 全部ふさがっている → ゆっくり回る
             | otherwise ->
                 let aBest = rayAngle th i
                     e = atan2 (sin (aBest - th)) (cos (aBest - th))
                 in (vMax * min 1 (frontOf rs / 100) * min 1 (dGoal / 50), 0.12 * e)
  where
    th = wTh w
    rs = wRanges w
    (gx, gy) = wGoal w
    dGoal = dist2 (gx, gy) (wX w, wY w)
    aGoal = atan2 (gy - wY w) (gx - wX w)
    pick b@(_, bs) (i, r)
      | r > 70 =                         -- ★ 70 px 以上空いていれば通れる
          let a = rayAngle th i
              diff = abs (atan2 (sin (a - aGoal)) (cos (a - aGoal)))
          in if diff < bs then (i, diff) else b
      | otherwise = b
    best = foldl' pick (-1 :: Int, 1 / 0) (zip [0 ..] rs)

step :: Env -> World -> IO World
step env w0 = do
  rs <- sense w0
  let w = w0 { wRanges = rs }
      (v, om0) = if wMode w == 1 then braitenberg w else goToGoal w
      th = wTh w + clampF (-0.15) 0.15 om0
      x = clampF 12 (width - 12) (wX w + v * cos th)
      y = clampF 112 (height - 12) (wY w + v * sin th)
      trail | even (envFrame env) = take 1500 ((x, y) : wTrail w)
            | otherwise = wTrail w
  pure w { wX = x, wY = y, wTh = th, wTrail = trail, wV = v }

drawAll :: World -> Picture
drawAll w =
  let robot@(rx, ry) = (wX w, wY w)
      th = wTh w
      rays = zip [0 :: Int ..] (wRanges w)
      hueOf d = remap d 0 maxRange 0 170
      ray (i, d) =
        let a = rayAngle th i
            tip = (rx + cos a * d, ry + sin a * d)
        in Pictures
           [ Color (hsbA (hueOf d) 80 100 60) (Line [robot, tip])
           , Color (hsb (hueOf d) 80 100) (at tip (disc 2.5)) ]
      gx = width - 330
      gy = 100
      bw = 300 / fromIntegral nRays
      bar (i, d) =
        let h = d / maxRange * 90
        in Color (hsb (hueOf d) 80 100) (boxAt (gx + fromIntegral i * bw) (gy + 100 - h) (bw - 2) h)
  in Pictures
  [ view
      ( [ Color (hsb 0 60 70) (Pictures [at (x, y) (disc r) | Obs x y r <- wObs w]) ]
        ++ [ Color (hsb 120 80 100) (at (wGoal w) (disc 11)) | wMode w == 2 ]
        ++ [ Color (hsbA 45 90 100 60) (thickPath 2 (wTrail w)) ]
        ++ map ray rays    -- 光線
        ++ [ Color (hsb 170 60 95) (bodyRect robot th (-16) (-12) 32 24)
           , Color (hsb 0 0 100) (bodyTri robot th (16, 0) (6, -7) (6, 7))
           -- 距離の棒グラフ（センサーから見た世界）
           , Color (hsbA 0 0 100 8) (boxAt (gx - 10) gy 320 110) ]
        ++ map bar rays )
  , txt gx (gy + 126) 14 (hsb 0 0 60) ("Left  <-  " ++ show nRays ++ " rays  ->  Right")
  , txt 20 34 16 (hsb 0 0 100) ("Behavior: " ++ (if wMode w == 1 then "Braitenberg (1)" else "to goal (2)") ++ "   Noise sigma = " ++ nf 0 (wSigma w)
                                ++ " px (Up/Down)   v = " ++ nf 1 (wV w))
  , txt 20 58 14 (hsb 0 0 100) "Click: obstacle   G: goal to mouse   R: back   C: clear"
  , txt 20 78 14 (hsb 0 0 60) "Ray color: red near, cyan far. No map, no memory, yet it avoids things. Dead ends (U shapes) still trap it."
  ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
    in if my > 90 then w { wObs = wObs w ++ [Obs mx my 30] } else w
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> w { wMode = 1 }
    '2' -> w { wMode = 2 }
    'g' -> let (mx, my) = toP5 (envMouse env) in w { wGoal = (mx, max 110 my) }
    'r' -> resetRobot w
    'c' -> w { wObs = [] }
    _ -> w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> w { wSigma = min 60 (wSigma w + 4) }
    KeyDown -> w { wSigma = max 0 (wSigma w - 4) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ノイズを 40 にすると急にふらつく。平均をとる（直前の値となめらかにする）と安定するか試そう
-- 2. nRays を 3 にしても避けられる？ 1 本では？（本物の安いロボットは 3〜5 個の超音波センサー）
-- 3. 障害物で「コ」の字をつくってゴールをその奥に置く。2 はどうなる？（06 の局所解と同じ問題）
-- 4. 行動 1 の「近いほど強く反対へ」の符号を逆にすると「光に寄る虫」になる。やってみよう


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])

-- 平均 0、標準偏差 1 の正規乱数（p5 の randomGaussian。ボックス・ミュラー法）
gauss :: IO Float
gauss = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))
