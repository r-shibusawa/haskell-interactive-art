-- ============================================
-- 06 ポテンシャル法
-- ゴールは「谷」、障害物は「山」。地形の傾き（勾配）にそって転がればゴールに着く
--   引力 = k_att × (ゴール − 位置)　斥力 = η × (1/d − 1/d0) / d²（d0 より近い障害物だけ）
-- でも、山に囲まれた「くぼみ」で止まってしまうことがある（局所解）
-- クリック = 障害物を置く / G = ゴールをマウスへ / 1 = ランダム配置 / 2 = 罠（局所解） / ↑↓ = 斥力の強さ / R = ロボットを戻す / C = 障害物を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import System.Random (StdGen, mkStdGen, randomR)
import Text.Printf (printf)

kAtt, d0, vMax :: Float
kAtt = 0.004              -- ★ 引力の強さ
d0 = 90                   -- ★ 斥力がはたらく距離（障害物の表面から）
vMax = 2.5

data Obs = Obs !Float !Float !Float    -- x, y, 半径

data World = World
  { wObs   :: ![Obs]
  , wGoal  :: !Point
  , wRobot :: !Point
  , wEta   :: !Float      -- ★ 斥力の強さ
  , wTrail :: ![Point]    -- 軌跡（新しい順、1500 点まで）
  , wStuck :: !Bool
  , wDist  :: !Float      -- ゴールまでの距離
  , wForce :: !Point      -- ロボットが受けている力
  }

main :: IO ()
main = runSketch (sketch "21-06 Potential Field" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = preset 1 (World [] (width - 140, height / 2) startPos 60000 [] False 0 (0, 0))

startPos :: Point
startPos = (120, height / 2)

preset :: Int -> World -> World
preset k w = resetRobot w { wObs = if k == 1 then randomObs else trapObs }

-- 乱数の種を固定した配置（p5 の randomSeed(3)）
randomObs :: [Obs]
randomObs = take 9 (go (mkStdGen 3))
  where
    go :: StdGen -> [Obs]
    go g0 =
      let (x, g1) = randomR (width * 0.3, width * 0.75) g0
          (y, g2) = randomR (130, height - 60) g1
          (r, g3) = randomR (20, 45) g2
      in Obs x y r : go g3

-- ゴールに向かって口を開けた「コ」の字：引力でくぼみに入って出られない
trapObs :: [Obs]
trapObs =
  [ Obs cx (cy + i * 34) 18 | i <- [-5 .. 5] ]
  ++ concat [ [Obs (cx - i * 34) (cy - 5 * 34) 18, Obs (cx - i * 34) (cy + 5 * 34) 18] | i <- [0 .. 5] ]
  where
    cx = width * 0.55
    cy = height / 2

resetRobot :: World -> World
resetRobot w = w { wRobot = startPos, wTrail = [], wStuck = False }

-- ★ 位置 (x, y) での力（＝ポテンシャルの勾配の逆向き）
force :: World -> Float -> Float -> Point
force w x y = foldl' add (kAtt * (gx - x), kAtt * (gy - y)) (wObs w)
  where
    (gx, gy) = wGoal w
    eta = wEta w
    add acc@(fx, fy) (Obs ox oy r) =
      let dx = x - ox
          dy = y - oy
          dc = sqrt (dx * dx + dy * dy)
          d = max (dc - r) 2             -- 表面からの距離
          m = eta * (1 / d - 1 / d0) / (d * d)
      in if d < d0 && dc > 1e-3 then (fx + m * dx / dc, fy + m * dy / dc) else acc

step :: Env -> World -> World
step env w =
  let (rx, ry) = wRobot w
      f@(fx, fy) = force w rx ry
      m = norm2 f
      dGoal = dist2 (wGoal w) (rx, ry)
      v = min vMax m
      (x1, y1) | m > 1e-6 && dGoal > 6 = (rx + fx / m * v, ry + fy / m * v)   -- 力の方向へ（速さに上限）
               | otherwise = (rx, ry)
      p = (clampF 10 (width - 10) x1, clampF 100 (height - 10) y1)
      trail | even (envFrame env) = take 1500 (p : wTrail w)
            | otherwise = wTrail w
  in w { wRobot = p, wStuck = dGoal > 30 && v < 0.05, wTrail = trail, wDist = dGoal, wForce = f }

-- 力の場を矢印で
fieldArrows :: World -> [Picture]
fieldArrows w =
  [ arrowAt gx gy
  | gx <- takeWhile (< width) (iterate (+ gs) gs)
  , gy <- takeWhile (< height) (iterate (+ gs) 110) ]
  where
    gs = 40
    arrowAt gx gy =
      let f@(fx, fy) = force w gx gy
          m = norm2 f
          l = min 14 (m * 6)
          hue = remap (min m 3) 0 3 200 0
          tip = (gx + fx / m * l, gy + fy / m * l)
      in if m < 1e-6 then Blank else Pictures
         [ Color (hsbA hue 70 100 45) (Line [(gx, gy), tip])
         , Color (hsbA hue 70 100 60) (at tip (disc 1.5)) ]

drawAll :: World -> Picture
drawAll w =
  let robot@(rx, ry) = wRobot w
      (fx, fy) = wForce w
      m = norm2 (fx, fy)
      obsPic (Obs x y r) = Pictures
        [ Color (hsbA 0 70 60 25) (at (x, y) (disc (r + d0)))
        , Color (hsb 0 70 80) (at (x, y) (disc r)) ]
      status | wStuck w = "   Stuck in a local minimum!"
             | wDist w <= 6 = "   Arrived"
             | otherwise = ""
  in Pictures
  [ view
      ( fieldArrows w
        ++ map obsPic (wObs w)
        ++ [ Color (hsb 120 80 100) (at (wGoal w) (disc 12))
           , Color (hsbA 45 90 100 80) (thickPath 2 (wTrail w))
           , Color (hsb 170 60 95) (at robot (disc 11))
           , if m > 1e-6 then Color (hsb 0 0 100) (thickLine 2 robot (rx + fx / m * 24, ry + fy / m * 24)) else Blank ] )
  , txt 20 34 16 (hsb 0 0 100) ("Click: obstacle   G: goal to mouse   1: random   2: trap   C: clear   R: back   eta = " ++ nf 0 (wEta w) ++ " (Up/Down)")
  , txt 20 58 14 (hsb 0 0 100) ("To goal " ++ nf 0 (wDist w) ++ " px" ++ status)
  , txt 20 78 14 (hsb 0 0 60) "Arrows = force here (blue weak, red strong). Pale ring = repulsion range d0. In the trap (2) the forces balance."
  ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
    in if my < 90 then w else w { wObs = wObs w ++ [Obs mx my 28] }
  EventKey (Char c) Down _ _ -> case toLower c of
    'g' -> let (mx, my) = toP5 (envMouse env) in w { wGoal = (mx, max 100 my), wStuck = False }
    '1' -> preset 1 w
    '2' -> preset 2 w
    'r' -> resetRobot w
    'c' -> resetRobot w { wObs = [] }
    _ -> w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> w { wEta = min 400000 (fromIntegral (round (wEta w * 1.5) :: Int)) }
    KeyDown -> w { wEta = max 5000 (fromIntegral (round (wEta w / 1.5) :: Int)) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 罠（2）から脱出する方法を考えよう：ランダムな力を少し足す？ 止まったら壁ぞいに歩く？（09 の bug 法）
-- 2. 2 つの障害物の間を通るとき、η が大きいと通れない。「通れる隙間」の最小値を探す
-- 3. d0 を 30 にすると障害物のすぐそばまで寄る。200 にすると？
-- 4. 複数のゴールを順に目指す（05 の経由点）と組み合わせてみよう


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

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")
