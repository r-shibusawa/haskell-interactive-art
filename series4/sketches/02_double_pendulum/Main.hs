-- ============================================
-- 02 二重振り子（バタフライ効果）
-- 2本つないだ振り子は、物理法則どおりに動くのに予測できない
-- 2つの振り子を「ほんのわずか」ちがう角度から同時に離すと…
-- クリックでやり直し / マウス上下 = 最初のずれの大きさ / T で軌跡の ON/OFF
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- 振り子 1 本。わずかな差を追うので、角度と速さは Double で計算する
data Pend = Pend
  { pA1    :: !Double
  , pA2    :: !Double
  , pV1    :: !Double
  , pV2    :: !Double
  , pHue   :: !Float
  , pPrev  :: !(Maybe Point)   -- 前のフレームの先端の位置
  , pTrail :: ![Picture]       -- 軌跡の線分（新しいものが先頭）
  }

data World = World
  { wPends   :: ![Pend]
  , wDiff    :: !Float         -- 2つの振り子の最初の角度のちがい（ラジアン）
  , wShow    :: !Bool          -- 軌跡を描き足すか
  , wElapsed :: !Int
  }

trailMax :: Int
trailMax = 2500                -- ★ 1本あたり覚えておく軌跡の線分の数

main :: IO ()
main = runSketch (sketch "04-02 Double Pendulum" (World [] 0.001 True 0))
  { skBack  = hsb 230 40 10
  , skStart = \env -> reset env (World [] 0.001 True 0)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

reset :: Env -> World -> IO World
reset env w = do
  a1 <- randomRIO (pi / 2 - 0.3, pi / 2 + 0.3)
  a2 <- randomRIO (pi / 2 - 0.3, pi / 2 + 0.3)
  let (_, my) = mouseP5 env
      diff = remapC my 0 (envH env) 0.1 0.00001
  pure w
    { wDiff = diff
    , wElapsed = 0
    , wPends = [ makePend a1 a2 170
               , makePend (a1 + realToFrac diff) a2 10 ]   -- ★ ほんの少しだけ違う
    }

makePend :: Double -> Double -> Float -> Pend
makePend a1 a2 hue = Pend a1 a2 0 0 hue Nothing []

step :: Env -> World -> World
step env w = w { wPends = map one (wPends w), wElapsed = wElapsed w + 1 }
  where
    one p0 =
      -- 物理計算（1フレームに何回も細かく進めると正確になる）
      let p = applyN 6 (simulatePend (1 / 6)) p0
          (_, tip) = arms env p
          trail = case pPrev p of
            Just old | wShow w -> take trailMax (thickLine 2 old tip : pTrail p)
            _ -> pTrail p
      in p { pPrev = Just tip, pTrail = trail }

-- 振り子の位置：(1つ目のおもり, 先端)
arms :: Env -> Pend -> (Point, Point)
arms env p = ((x1, y1), (x2, y2))
  where
    (ox, oy) = origin env
    l = armLength env
    x1 = ox + l * toF (sin (pA1 p))
    y1 = oy - l * toF (cos (pA1 p))          -- gloss は y が上向きなので引く
    x2 = x1 + l * toF (sin (pA2 p))
    y2 = y1 - l * toF (cos (pA2 p))

origin :: Env -> Point
origin env = sc env (envW env / 2) (envH env * 0.35)

armLength :: Env -> Float
armLength env = min (envW env) (envH env) * 0.2   -- ★ 棒の長さ

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( map drawTrail (wPends w) ++ map drawBody (wPends w) ++
  [ txt env 20 40 16 white
      ("Start gap " ++ fmt 5 (wDiff w) ++ " rad (mouse Y at click)   Tip distance " ++ fmt 1 tipDist
        ++ " px   Frame " ++ show (wElapsed w))
  , txt env 20 68 16 white ("Click: restart   T: trail " ++ (if wShow w then "ON" else "off"))
  ])
  where
    drawTrail p = Color (hsba (pHue p) 70 100 0.6) (Pictures (pTrail p))
    drawBody p =
      let (a, b) = arms env p
      in Pictures
           [ Color (hsb (pHue p) 50 100) (Pictures [thickLine 4 (origin env) a, thickLine 4 a b])
           , Color (hsb (pHue p) 70 100) (Pictures [at a (disc 9), at b (disc 9)]) ]
    -- 2つの先端の距離
    tipDist = case wPends w of
      (p : q : _) -> dist2 (snd (arms env p)) (snd (arms env q))
      _ -> 0

-- 二重振り子の運動方程式（教科書どおり。読まなくてOK）
simulatePend :: Double -> Pend -> Pend
simulatePend dt p = p { pA1 = a1 + v1' * dt, pA2 = a2 + v2' * dt, pV1 = v1', pV2 = v2' }
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
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> reset env w
  EventKey (Char c) Down _ _ | c == 't' || c == 'T' -> pure w { wShow = not (wShow w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 最初はぴったり重なって動く2つが、何秒後に別れるか数えよう
-- 2. マウスを一番下にしてクリック（ずれ 0.00001）。それでも別れる？ 何秒で？
-- 3. wPends のリストを 5 本に増やして、虹色にしてみよう（→ 13 で）

-- ---------- 道具 ----------

-- 関数 f を n 回くり返す
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

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

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
