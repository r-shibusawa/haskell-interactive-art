-- ============================================
-- 00 ロジスティック写像（1行の式が暴れだす）
-- 来年の個体数 = r × 今年の個体数 × (1 − 今年の個体数)
-- r を少し変えるだけで、落ち着く → 2つを行ったり来たり → 4つ → … → カオス
-- マウス左右 = r（2.5〜4.0） / クリックで最初の値をランダムに
-- ============================================
module Main (main) where

import Art
import Data.List (nub)
import Numeric (showFFloat)

newtype World = World { wX0 :: Float }   -- 最初の値

years :: Int
years = 120                  -- ★ 何年ぶん計算するか

main :: IO ()
main = runSketch (sketch "04-00 Logistic Map" (World 0.3))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ---- 計算：x を years 回更新する ----
series :: Float -> Float -> [Float]
series r x0 = take years (iterate (\x -> r * x * (1 - x)) x0)   -- ★ これが全部

-- 最後の 40 年の値が、いくつの値に落ち着いているか（周期）
stateText :: [Float] -> String
stateText xs
  | n == 1    = "Settles on one value"
  | n <= 2    = "Alternates between 2 values (period 2)"
  | n <= 4    = "Period 4"
  | n <= 8    = "Period 8"
  | otherwise = "Chaos (never returns to the same value)"
  where
    tailValues = drop (years - 40) xs
    n = length (nub (map (\v -> round (v * 1000) :: Int) tailValues))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb 0 0 40) (Pictures
      [ thickLine 1 (sc env left bottom) (sc env right bottom)
      , thickLine 1 (sc env left top) (sc env left bottom) ])
  , Color (hsb 170 80 100) (Pictures (thickPath 2.5 points : [at p (disc 2.5) | p <- points]))
  , txt env 20 40 16 white ("x(next) = r * x * (1 - x)   r = " ++ fmt 3 r ++ " (mouse X)   x0 = " ++ fmt 3 (wX0 w) ++ " (click to change)")
  , txt env 20 80 24 (hsb 40 80 100) (stateText xs)
  , txt env (right - 60) (bottom + 30) 14 (hsb 0 0 60) "year ->"
  , txt env (left - 70) (top - 16) 14 (hsb 0 0 60) "population (0-1)"
  ]
  where
    (mx, _) = mouseP5 env
    r = remapC mx 0 (envW env) 2.5 4.0
    xs = series r (wX0 w)
    left = 80
    right = envW env - 60
    top = 120
    bottom = envH env - 80
    points =
      [ sc env (remap (fromIntegral i) 0 (fromIntegral (years - 1)) left right) (remap x 0 1 bottom top)
      | (i, x) <- zip [0 :: Int ..] xs ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey (MouseButton LeftButton) Down _ _) w = do
  x0 <- randomRIO (0.01, 0.99)
  pure w { wX0 = x0 }
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. r をゆっくり右へ。2.5 → 3.0（落ち着く）→ 3.2（周期2）→ 3.5（周期4）→ 3.57…（カオス）
-- 2. r = 3.83 付近に、カオスの中に「周期3」の窓がある。探してみよう
-- 3. カオスのとき、クリックで x0 を変えると、グラフは全然違う形になる（初期値敏感性）

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js 風の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js 風の座標に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

-- p5.js の map(v, a, b, c, d, true)（範囲からはみ出さない）
remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- 小数点以下 n 桁の文字列（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
