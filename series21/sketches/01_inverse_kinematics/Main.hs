-- ============================================
-- 01 逆運動学（IK）─ 解析解
-- 「手先をここへ」から関節の角度を逆算する。2 リンクなら余弦定理で式が解ける
--   cos q2 = (x² + y² − L1² − L2²) / (2 L1 L2)   → q2 には ±（ひじ上・ひじ下）の 2 つの解
-- クリック／ドラッグ = 目標 / E = ひじ上・ひじ下を切りかえ / 1〜3 = リンクの長さ / A = 自動で目標を動かす
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Text.Printf (printf)

smoothRate :: Float
smoothRate = 0.15         -- ★ 1 フレームで近づく割合（1 で瞬間移動）

data World = World
  { wL1     :: !Float     -- ★ リンクの長さ（px）
  , wL2     :: !Float
  , wTarget :: !Point     -- 目標（画面座標）
  , wUp     :: !Bool      -- ひじの向き
  , wQ1     :: !Float     -- いまの関節角（なめらかに目標の解へ近づける）
  , wQ2     :: !Float
  , wAuto   :: !Bool
  }

main :: IO ()
main = runSketch (sketch "21-01 Inverse Kinematics" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 180 140 (width * 0.6, height * 0.35) True 0.6 0.9 False

basePos :: Point
basePos = (width * 0.4, height * 0.6)

-- 逆運動学の解
data Sol = Sol { sA1 :: !Float, sA2 :: !Float, sOk :: !Bool }

-- ★ 逆運動学：目標 (tx, ty) → q1, q2（届かないときは届く方向へ伸ばした解を返す）
inverse :: World -> Point -> Bool -> Sol
inverse w (tx, ty) up =
  let (bx, by) = basePos
      l1 = wL1 w
      l2 = wL2 w
      x = tx - bx          -- 台座を原点、y 上向き
      y = by - ty
      r = sqrt (x * x + y * y)
      rMin = abs (l1 - l2) + 0.001
      rMax = l1 + l2 - 0.001
      rr = clampF rMin rMax r    -- 範囲外なら一番近い半径に丸める
      c2 = clampF (-1) 1 ((rr * rr - l1 * l1 - l2 * l2) / (2 * l1 * l2))
      a2 = acos c2 * (if up then 1 else -1)
      -- q1 = atan2(y, x) − atan2(L2 sin q2, L1 + L2 cos q2)
      a1 = atan2 y x - atan2 (l2 * sin a2) (l1 + l2 * cos a2)
  in Sol a1 a2 (r >= rMin && r <= rMax)

forward :: World -> Float -> Float -> (Point, Point)
forward w a1 a2 =
  let (bx, by) = basePos
      ex = bx + wL1 w * cos a1
      ey = by - wL1 w * sin a1
  in ((ex, ey), (ex + wL2 w * cos (a1 + a2), ey - wL2 w * sin (a1 + a2)))

-- 角度の差を −π〜π に折り返して少しずつ近づける
approach :: Float -> Float -> Float
approach cur goal =
  let d = goal - cur
  in cur + atan2 (sin d) (cos d) * smoothRate

step :: Env -> World -> World
step env w =
  let (bx, by) = basePos
      t = fromIntegral (envFrame env) * 0.02
      target
        | wAuto w = (bx + 240 * cos t + 60 * cos (3 * t), by - 150 - 120 * sin (2 * t))
        | envDown env = toP5 (envMouse env)
        | otherwise = wTarget w
      sol = inverse w target (wUp w)
  in w { wTarget = target, wQ1 = approach (wQ1 w) (sA1 sol), wQ2 = approach (wQ2 w) (sA2 sol) }

drawArm :: World -> Float -> Float -> Float -> Picture
drawArm w a1 a2 alpha =
  let (e, h) = forward w a1 a2
      b = basePos
  in Pictures
     [ Color (hsbA 200 60 95 alpha) (thickLine 14 b e)
     , Color (hsbA 320 60 95 alpha) (thickLine 14 e h)
     , Color (hsbA 0 0 100 alpha) (Pictures [at b (disc 11), at e (disc 9)])
     , Color (hsbA 45 90 100 alpha) (at h (disc 8))
     ]

drawAll :: World -> Picture
drawAll w =
  let b@(bx, by) = basePos
      target@(tx, ty) = wTarget w
      l1 = wL1 w
      l2 = wL2 w
      sol = inverse w target (wUp w)
      other = inverse w target (not (wUp w))    -- もう 1 つの解（ゴースト）
      ok = sOk sol
      rIn = abs (l1 - l2)
      x = tx - bx
      y = by - ty
      px = width - 360
      py = 110
      tcol = hsb (if ok then 120 else 0) 90 100
  in Pictures
  [ view
      [ -- 届く範囲（ドーナツ）
        Color (hsbA 45 80 100 10) (at b (disc (l1 + l2)))
      , if rIn > 0 then Color (hsb 215 35 12) (at b (disc rIn)) else Blank
      , Color (hsbA 45 80 100 40) (Pictures [at b (ring (l1 + l2) 1), at b (ring rIn 1)])
      , drawArm w (sA1 other) (sA2 other) 18
      , drawArm w (wQ1 w) (wQ2 w) 100
      -- 目標
      , Color tcol (Pictures
          [ at target (ring 13 2)
          , thickLine 2 (tx - 18, ty) (tx + 18, ty)
          , thickLine 2 (tx, ty - 18) (tx, ty + 18) ])
      -- 台座
      , Color (hsb 0 0 40) (boxAt (bx - 30) by 60 14)
      , Color (hsbA 0 0 100 8) (boxAt (px - 16) (py - 30) 350 120)
      ]
  , txt 20 32 16 (hsb 0 0 100) ("Click/drag: target   E: elbow " ++ (if wUp w then "up" else "down") ++ "   1-3: link lengths   A: auto "
                                ++ onOff (wAuto w) ++ "   " ++ (if ok then "Reachable" else "Out of reach (goes to nearest)"))
  , txt 20 56 14 (hsb 0 0 60) "q2 = +-acos((x^2 + y^2 - L1^2 - L2^2) / (2 L1 L2))     q1 = atan2(y, x) - atan2(L2 sin q2, L1 + L2 cos q2)"
  , txt 20 78 14 (hsb 0 0 60) "The faint arm is the other solution."
  , txt px py 16 (hsb 45 90 100) ("Target (" ++ nf 0 x ++ ", " ++ nf 0 y ++ ")  r = " ++ nf 0 (sqrt (x * x + y * y)))
  , txt px (py + 28) 16 (hsb 45 90 100) ("q1 = " ++ nf 1 (deg (sA1 sol)) ++ " deg   q2 = " ++ nf 1 (deg (sA2 sol)) ++ " deg")
  , txt px (py + 54) 14 (hsb 0 0 60) ("Reach radius: " ++ nf 0 rIn ++ " to " ++ nf 0 (l1 + l2))
  , txt px (py + 76) 14 (hsb 0 0 60) (if ok then "Both solutions are right. The designer picks." else "Unreachable: |D| > 1, acos is undefined")
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    'e' -> w { wUp = not (wUp w) }
    'a' -> w { wAuto = not (wAuto w) }
    '1' -> w { wL1 = 180, wL2 = 140 }
    '2' -> w { wL1 = 160, wL2 = 160 }
    '3' -> w { wL1 = 220, wL2 = 90 }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 目標を台座のすぐそばに置くと？（内側の穴）遠くに置くと？（外側）
-- 2. E でひじを切りかえ、腕が「バタン」と裏返るのを見る。ロボットが実際にやると危ないのはなぜ？
-- 3. smoothRate を 1 にすると瞬間移動。0.03 にするとゆっくり。本物のモーターはどちらに近い？
-- 4. 腕が届く範囲の境界ぎりぎりで目標を動かすと、q1 が急に変わる所がある（特異点）。探してみよう


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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

deg :: Float -> Float
deg a = a * 180 / pi
