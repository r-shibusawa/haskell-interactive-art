-- ============================================
-- 01 クモの巣図（くり返しを目で見る）
-- 放物線 y = r x (1−x) と、対角線 y = x の間を、はしご状にたどる
-- 1つの点に吸い込まれる／四角くまわる／永遠にさまよう、が一目でわかる
-- マウス左右 = r / クリックで最初の値 / スペースで一時停止
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- 線分 1 本（0〜1 の座標で x1, y1, x2, y2）
data Seg = Seg !Float !Float !Float !Float

data World = World
  { wR      :: !Float
  , wX      :: !Float
  , wPath   :: ![Seg]    -- たどった線（新しいものが先頭）
  , wPaused :: !Bool
  }

maxSegs :: Int
maxSegs = 400            -- ★ 線の本数の上限

stepEvery :: Int
stepEvery = 6            -- ★ 何フレームごとに 1 ステップ進めるか

main :: IO ()
main = runSketch (sketch "04-01 Cobweb Diagram" (World 3.2 0.2 [] False))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> pure (advance env (followMouse env w))
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- r が変わったらやり直し
followMouse :: Env -> World -> World
followMouse env w
  | abs (newR - wR w) > 0.001 = w { wR = newR, wPath = [] }
  | otherwise = w
  where
    (mx, _) = mouseP5 env
    newR = remapC mx 0 (envW env) 2.5 4.0

-- ---- 1ステップ進める ----
advance :: Env -> World -> World
advance env w
  | not (wPaused w) && envFrame env `mod` stepEvery == 0 && length (wPath w) < maxSegs =
      let x = wX w
          y = wR w * x * (1 - x)         -- 上（下）へ：放物線で次の値を読む
      in w { wX = y
           , wPath = Seg x y y y         -- 横の線：対角線まで戻す（次の x になる）
                   : Seg x x x y         -- 縦の線
                   : wPath w }
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb 0 0 35) (Pictures
      [ thickPath 1 [scr 0 0, scr 1 0, scr 1 1, scr 0 1, scr 0 0]   -- 枠
      , thickLine 1 (scr 0 0) (scr 1 1) ])                          -- 対角線
  , Color (hsb 170 80 100) (thickPath 3 parabola)
  , Pictures (reverse (zipWith drawSeg [0 :: Int ..] (wPath w)))
  , Color (hsb 40 90 100) (at (scr (wX w) (wX w)) (disc 5))
  , txt env 20 40 16 white
      ("r = " ++ fmt 3 (wR w) ++ " (mouse X)   x = " ++ fmt 4 (wX w) ++ "   step " ++ show (length (wPath w) `div` 2)
        ++ "   Click: new start   Space: pause")
  , txt env (ox + s - 60) (oy + s + 24) 14 (hsb 0 0 60) "x(now)"
  , txt env (ox - 60) (oy - 12) 14 (hsb 0 0 60) "x(next)"
  ]
  where
    -- グラフの領域（正方形）
    s = min (envW env) (envH env) - 160
    ox = (envW env - s) / 2
    oy = 100
    scr vx vy = sc env (ox + vx * s) (oy + s - vy * s)   -- 0〜1 → 画面
    r = wR w
    parabola = [scr v (r * v * (1 - v)) | i <- [0 .. 100 :: Int], let v = fromIntegral i * 0.01]
    -- たどった線（新しいほど明るい。j は新しい順の番号）
    n = length (wPath w)
    drawSeg j (Seg a b c d) =
      let i = n - 1 - j
          alpha = remap (fromIntegral i) 0 (fromIntegral n) 15 100 / 100
      in Color (hsba 40 90 100 alpha) (thickLine 1.5 (scr a b) (scr c d))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    x <- randomRIO (0.05, 0.95)
    pure w { wX = x, wPath = [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. r = 2.8：らせんを描いて1点に吸い込まれる。 r = 3.3：四角をまわる（周期2）
-- 2. r = 3.9：どこにも落ち着かず、放物線の下を埋めつくす（カオス）
-- 3. 放物線を別の式（例：r * sin (pi * x)）にしても、同じことが起きる？
--    advance と parabola の 2 か所を書きかえよう

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
