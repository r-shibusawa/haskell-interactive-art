-- ============================================
-- 09 ゆれる台の上のボール
-- 上下にゆれる台の上で跳ねるボール。台のゆれが小さいと規則的に跳ねるが、
-- 大きくすると「周期倍分岐」を経てカオスになる（00 の分岐と同じ道筋）
-- マウス左右 = 台のゆれの大きさ / クリックでボールをリセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- 位置は p5.js と同じく左上が原点、y は下向き
data Ball = Ball
  { bX, bY, bVY :: !Float
  , bHue        :: !Float
  }

-- 跳ねた瞬間の記録
data Hit = Hit !Float !Float     -- 速さ、色

data World = World
  { wBalls   :: ![Ball]
  , wT       :: !Float
  , wHistory :: ![Hit]           -- 新しいものが先頭
  }

freq :: Float
freq = 0.08                      -- ★ 台のゆれの速さ

bounce :: Float
bounce = 0.9                     -- ★ 跳ね返りの強さ

historyMax :: Int
historyMax = 400

main :: IO ()
main = runSketch (sketch "04-09 Bouncing Ball" (World [] 0 []))
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (reset env (World [] 0 []))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

reset :: Env -> World -> World
reset env w = w
  { wBalls = [ Ball (envW env * 0.2 + fromIntegral i * 60) (envH env * 0.3) 0 (fromIntegral i * 60 + 160)
             | i <- [0, 1, 2 :: Int] ]
  , wHistory = [] }

amplitude :: Env -> Float
amplitude env = remapC (fst (mouseP5 env)) 0 (envW env) 0 40

-- 台の位置
tableY :: Env -> Float -> Float
tableY env t = envH env * 0.7 + sin (t * freq) * amplitude env

step :: Env -> World -> World
step env w = w
  { wT = t
  , wBalls = balls
  , wHistory = take historyMax (concat (reverse hits) ++ wHistory w) }
  where
    t = wT w + 1
    ty = tableY env t
    tableVy = cos (t * freq) * amplitude env * freq     -- 台の速さ
    (balls, hits) = unzip (map (moveBall ty tableVy) (wBalls w))

moveBall :: Float -> Float -> Ball -> (Ball, [Hit])
moveBall ty tableVy b
  -- 台に当たったら跳ね返る（台の速さぶん、押し上げられる）
  | y > ty - 15 && vy > tableVy =
      let vy' = -bounce * (vy - tableVy) + tableVy
      in (b { bY = ty - 15, bVY = vy' }, [Hit (-vy') (bHue b)])
  | otherwise = (b { bY = y, bVY = vy }, [])
  where
    vy = bVY b + 0.5        -- 重力
    y = bY b + vy

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( [ Color (hsb (bHue b) 60 100) (at (sc env (bX b) (bY b)) (disc 15)) | b <- wBalls w ] ++
    -- 台
  [ Color (hsb 0 0 80) (rectP (envW env * 0.08) (tableY env (wT w)) (envW env * 0.42) 14)
    -- 右側：跳ねた瞬間の速さの記録（横軸 = 回数）
  , Color (hsb 0 0 40) (Pictures
      [ thickLine 1 (sc env gx gBot) (sc env (gx + gw) gBot)
      , thickLine 1 (sc env gx gTop) (sc env gx gBot) ])
  , Pictures (zipWith drawHit [n - 1, n - 2 ..] (wHistory w))
  , txt env 20 40 18 white ("Table shake " ++ fmt 1 (amplitude env) ++ " (mouse X)   Click: reset balls")
  , txt env gx 66 14 (hsb 0 0 70) "Speed at each bounce"
  , txt env gx 86 14 (hsb 0 0 70) "1 line: period 1   2: period 2   scatter: chaos"
  ])
  where
    gx = envW env * 0.56
    gw = envW env * 0.4
    gTop = 100
    gBot = envH env - 80
    n = length (wHistory w)
    drawHit :: Int -> Hit -> Picture
    drawHit i (Hit v hue) =
      Color (hsb hue 60 100)
        (at (sc env (gx + fromIntegral i / fromIntegral historyMax * gw) (remap v 0 40 gBot gTop)) (disc 2))
    rectP x y rw rh = let (cx, cy) = sc env (x + rw / 2) (y + rh / 2) in rect cx cy rw rh

onEvent :: Env -> Event -> World -> IO World
onEvent env (EventKey (MouseButton LeftButton) Down _ _) w = pure (reset env w)
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. ゆれ 0 → 規則的（右のグラフが1本の線）。ゆっくり増やして、2本・4本・散らばる、を探そう
-- 2. 3つのボールは最初ほぼ同じ場所。カオスのときはすぐ別々の動きになる
-- 3. 跳ね返り 0.9 を 0.5 にすると？（カオスになりにくい）

-- ---------- 道具 ----------

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
