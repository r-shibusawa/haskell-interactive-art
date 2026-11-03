-- ============================================
-- 13 100本の二重振り子
-- 最初の角度が 0.0001 ずつちがう振り子を 100 本、同時に離す
-- 最初は1本に見えるが、やがて虹色の扇のように広がり、ばらばらに
-- クリックでやり直し / マウス左右 = 本数
-- ============================================
module Main (main) where

import Art

-- わずかな差を追うので、角度と速さは Double で計算する
data Pend = Pend
  { pA1, pA2, pV1, pV2 :: !Double
  , pHue :: !Float
  }

data World = World
  { wPends :: ![Pend]
  , wCount :: !Int
  , wT     :: !Int
  }

angleStep :: Double
angleStep = 0.0001           -- ★ 振り子ごとの最初の角度のずれ

base1, base2 :: Double
base1 = pi * 0.75            -- ★ 最初の角度
base2 = pi * 0.5

main :: IO ()
main = runSketch (sketch "04-13 Pendulum Fan" (World [] 0 0))
  { skBack  = hsb 240 40 8
  , skStart = \env -> pure (reset env)
  , skStep  = \_ _ w -> pure w { wPends = map stepFrame (wPends w), wT = wT w + 1 }
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

reset :: Env -> World
reset env = World pends count 0
  where
    (mx, _) = mouseP5 env
    count = floor (remapC mx 0 (envW env) 10 200)
    pends =
      [ Pend (base1 + fromIntegral i * angleStep) base2 0 0 (fromIntegral i / fromIntegral count * 300)
      | i <- [0 .. count - 1] ]

-- 1 フレームに 6 回、細かく進める
stepFrame :: Pend -> Pend
stepFrame p = f (f (f (f (f (f p)))))
  where f = stepPend (1 / 6)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( concatMap drawPend (wPends w) ++
  [ txt env 20 40 16 white
      (show (wCount w) ++ " pendulums (start angles 0.0001 apart)   Frame " ++ show (wT w)
        ++ "   Click: restart (mouse X = count)")
  ])
  where
    (ox, oy) = sc env (envW env / 2) (envH env * 0.4)
    l = min (envW env) (envH env) * 0.2
    drawPend p =
      let x1 = ox + l * toF (sin (pA1 p))
          y1 = oy - l * toF (cos (pA1 p))
          x2 = x1 + l * toF (sin (pA2 p))
          y2 = y1 - l * toF (cos (pA2 p))
      in [ Color (hsba (pHue p) 70 100 0.7)
             (Pictures [thickLine 2 (ox, oy) (x1, y1), thickLine 2 (x1, y1) (x2, y2)])
         , Color (hsb (pHue p) 70 100) (at (x2, y2) (disc 4)) ]

-- 二重振り子の運動方程式（02 と同じ）
stepPend :: Double -> Pend -> Pend
stepPend dt p = p { pA1 = a1 + v1' * dt, pA2 = a2 + v2' * dt, pV1 = v1', pV2 = v2' }
  where
    g = 1; m1 = 1; m2 = 1; l1 = 1; l2 = 1
    a1 = pA1 p; a2 = pA2 p; v1 = pV1 p; v2 = pV2 p
    num1 = -g * (2 * m1 + m2) * sin a1 - m2 * g * sin (a1 - 2 * a2)
           - 2 * sin (a1 - a2) * m2 * (v2 * v2 * l2 + v1 * v1 * l1 * cos (a1 - a2))
    den1 = l1 * (2 * m1 + m2 - m2 * cos (2 * a1 - 2 * a2))
    num2 = 2 * sin (a1 - a2) * (v1 * v1 * l1 * (m1 + m2) + g * (m1 + m2) * cos a1 + v2 * v2 * l2 * m2 * cos (a1 - a2))
    den2 = l2 * (2 * m1 + m2 - m2 * cos (2 * a1 - 2 * a2))
    v1' = v1 + num1 / den1 * dt * 0.05
    v2' = v2 + num2 / den2 * dt * 0.05

onEvent :: Env -> Event -> World -> IO World
onEvent env (EventKey (MouseButton LeftButton) Down _ _) _ = pure (reset env)
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. 広がり始めるまでの時間を数えよう。ずれ 0.0001 を 0.00001 にすると、どれだけ遅くなる？
-- 2. base1 を pi * 0.3（低い位置から）にすると、ばらばらにならない。カオスには「エネルギー」が必要
-- 3. 先端の軌跡を残して描くと、きれいな絵になる（02 の pTrail を参考に）

-- ---------- 道具 ----------

toF :: Double -> Float
toF = realToFrac

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)
