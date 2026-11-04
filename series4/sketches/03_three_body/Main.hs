-- ============================================
-- 03 三体問題
-- 2つの星なら軌道はきれいな楕円。3つになると、もう誰にも予測できない
-- クリックで星を追加 / R でやり直し / スペースで止める
-- ============================================
module Main (main) where

import Art

data Body = Body
  { bX, bY   :: !Float
  , bVX, bVY :: !Float
  , bMass    :: !Float
  , bHue     :: !Float
  , bLast    :: !Point       -- 軌跡を最後に記録した位置
  }

data World = World
  { wBodies :: ![Body]
  , wTrail  :: ![Picture]    -- 軌跡の線分（新しいものが先頭）
  , wPaused :: !Bool
  }

g :: Float
g = 1.2                      -- ★ 引力の強さ

trailMax :: Int
trailMax = 6500              -- ★ 覚えておく線分の数（超えると古い線から消える）

segStep :: Float
segStep = 4                  -- ★ 何ピクセル動いたら線分を 1 本記録するか

maxBodies :: Int
maxBodies = 40               -- ★ 星の数の上限

main :: IO ()
main = runSketch (sketch "04-03 Three Body Problem" (World [] [] False))
  { skBack  = hsb 240 40 8
  , skStart = \_ -> reset (World [] [] False)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 正三角形に置いて、少し回転させる（それでもすぐ乱れる）
reset :: World -> IO World
reset w = do
  bodies <- mapM makeBody [0, 1, 2]
  pure w { wBodies = bodies, wTrail = [] }
  where
    makeBody :: Int -> IO Body
    makeBody i = do
      speed <- randomRIO (0.45 - 0.05, 0.45 + 0.05)   -- ★ ほんの少しランダム（速すぎると引力をふり切って飛び去る）
      let a = tau * fromIntegral i / 3
          r = 160
          pos = (cos a * r, -sin a * r)
      pure (Body (fst pos) (snd pos) (-sin a * speed) (-cos a * speed) 60 (fromIntegral i * 120 + 20) pos)

step :: Env -> World -> World
step env w
  | wPaused w = w
  | otherwise =
      let moved = applyN 4 (physics 0.25) (wBodies w)
          recorded = map (recordTrail env) moved
      in w { wBodies = map fst recorded
           , wTrail = take trailMax (concatMap snd recorded ++ wTrail w) }

-- 万有引力：すべての星のペアで、質量に比例・距離の2乗に反比例する力
physics :: Float -> [Body] -> [Body]
physics dt bodies = zipWith move [0 :: Int ..] bodies
  where
    numbered = zip [0 :: Int ..] bodies
    move i a =
      let (ax, ay) = foldr (addPull a) (0, 0) [b | (j, b) <- numbered, j /= i]
          vx = bVX a + ax * dt
          vy = bVY a + ay * dt
      in a { bVX = vx, bVY = vy, bX = bX a + vx * dt, bY = bY a + vy * dt }
    addPull a b (ax, ay) =
      let dx = bX b - bX a
          dy = bY b - bY a
          d2 = dx * dx + dy * dy
          d = sqrt d2
          f = g * bMass b / max d2 400        -- 近すぎると力が無限大になるので制限
      in if d < 1e-6 then (ax, ay) else (ax + dx / d * f, ay + dy / d * f)

-- 十分動いていたら、前の記録からの線分を 1 本残す（画面の外は残さない）
recordTrail :: Env -> Body -> (Body, [Picture])
recordTrail env b
  | dist2 pos (bLast b) < segStep = (b, [])
  | onScreen = (b { bLast = pos }, [Color (hsba (bHue b) 60 100 0.5) (thickLine 1.5 (bLast b) pos)])
  | otherwise = (b { bLast = pos }, [])
  where
    pos = (bX b, bY b)
    onScreen = abs (bX b) < envW env && abs (bY b) < envH env

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( Pictures (wTrail w) : concatMap drawBody (wBodies w) ++
  [ txt env 20 40 18 white
      ("Stars " ++ show (length (wBodies w)) ++ "   Click: add a star   R: restart   Space: "
        ++ (if wPaused w then "resume" else "pause"))
  ])
  where
    drawBody b =
      [ at (bX b, bY b) (Color (hsb (bHue b) 60 100) (disc (sqrt (bMass b) * 1.5)))
      , at (bX b, bY b) (Color (hsb (bHue b) 20 100) (disc (sqrt (bMass b) * 0.6))) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my)
    | snd (mouseP5 env) >= 50 && length (wBodies w) < maxBodies -> do
        mass <- randomRIO (20, 80)
        hue <- randomRIO (0, 360)
        pure w { wBodies = wBodies w ++ [Body mx my 0 0 mass hue (mx, my)] }
  EventKey (Char c) Down _ _ | c == 'r' || c == 'R' -> reset w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. R を何回も押そう。最初はほぼ同じなのに、毎回ちがう運命になる
-- 2. 星を 2 つだけにすると（reset の [0, 1, 2] を変える）、軌道は安定する？
-- 3. 太陽（質量 2000）を真ん中に置いて、まわりに小さい星を置くと惑星系になる

-- ---------- 道具 ----------

-- 関数 f を n 回くり返す
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s
