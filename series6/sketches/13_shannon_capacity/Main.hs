-- ============================================
-- 13 シャノンの通信路容量（速さの上限）
-- 雑音のある通信路で、誤りなく送れる最大の速さには理論的な限界がある：
--   C = B × log₂(1 + S/N)   B = 帯域幅、S/N = 信号と雑音の比
-- マウス左右 = S/N（信号の強さ） / 上下 = 帯域幅 / 線の上の点をたどる
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

maxC :: Float
maxC = 55000              -- ★ グラフの上端（bit/s）

main :: IO ()
main = runSketch (sketch "06-13 Shannon Capacity" ())
  { skBack = hsb 230 40 10
  , skDraw = \env _ -> drawWorld env
  }

-- ★ シャノンの式
capacity :: Float -> Float -> Float
capacity bw db = bw * logBase 2 (1 + 10 ** (db / 10))   -- dB → 倍率

drawWorld :: Env -> Picture
drawWorld env = Pictures
  [ -- ---- 左：S/N を横軸にした容量の曲線 ----
    Color (hsb 0 0 30) (Pictures [Line [toG env left base, toG env right base], Line [toG env left base, toG env left top]])
  , Color (hsb 170 80 100) (thickPath 3
      [ toG env (xOf db) (yOf (capacity bw db)) | i <- [0 .. 100 :: Int], let db = -10 + fromIntegral i * 0.5 ])
  , Color (hsb 40 90 100) (at (toG env (xOf snrDb) (yOf c)) (disc 9))
  , Pictures [ txt env (xOf db - 18) (base + 22) 14 dim (show (round db :: Int) ++ " dB") | db <- [-10, 0, 10, 20, 30, 40] ]
  , Pictures [ txt env (left - 85) (yOf v + 5) 14 dim (show (round (v / 1000) :: Int) ++ " kbps") | v <- [0, 10000, 20000, 30000, 40000, 50000] ]
  , txt env ((left + right) / 2 - 90) (base + 46) 14 dim "Signal-to-noise ratio S/N ->"
    -- ---- 右：いまの値と「電話回線」の例 ----
  , txt env rx ry 22 (hsb 0 0 100) "C = B x log2(1 + S/N)"
  , txt env rx (ry + 50) 16 (hsb 0 0 100) ("Bandwidth B = " ++ show (round bw :: Int) ++ " Hz (mouse Y)")
  , txt env rx (ry + 85) 16 (hsb 0 0 100) ("S/N " ++ nf 1 snrDb ++ " dB = " ++ nf 1 (10 ** (snrDb / 10)) ++ "x (mouse X)")
  , txt env rx (ry + 150) 30 (hsb 45 90 100) ("C = " ++ nf 2 (c / 1000) ++ " kbps")
  , Pictures (zipWith (\k s -> txt env rx (ry + 200 + k * 21) 14 dim s) [0 ..] notes)
  , txt env 20 40 18 (hsb 0 0 100) "Shannon (1948): below this rate, error-free codes exist despite any noise"
  ]
  where
    dim = hsb 0 0 60
    (mx, my) = mouseXY env
    snrDb = clampF (-10) 40 (remap mx 0 (envW env) (-10) 40)
    bw = clampF 200 4000 (remap my 0 (envH env) 4000 200)
    c = capacity bw snrDb
    left = 90
    right = envW env * 0.6
    base = envH env * 0.78
    top = envH env * 0.2
    xOf db = remap db (-10) 40 left right
    yOf v = remap v 0 maxC base top
    rx = envW env * 0.62
    ry = envH env * 0.25
    notes =
      [ "Phone line (B ~ 3000 Hz, S/N ~ 30 dB)"
      , "-> about 30 kbps. That is why old modems"
      , "stopped at 28.8-56 kbps."
      , ""
      , "2x signal: C grows only a little (log)"
      , "2x bandwidth: C doubles too"
      , "(why 5G uses high frequencies)"
      ]

-- ---------- やってみよう ----------
-- 1. S/N を 0 dB（信号 = 雑音）にしても、容量はゼロではない！ 1 bit/s/Hz 送れる
-- 2. S/N が 10 dB 上がるごとに容量はどれだけ増える？（ほぼ一定量ずつ）
-- 3. Wi-Fi は B = 20〜160 MHz。容量はどのくらい？（capacity 160000000 30 を計算）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
