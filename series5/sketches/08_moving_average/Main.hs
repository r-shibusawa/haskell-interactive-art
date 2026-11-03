-- ============================================
-- 08 移動平均フィルタ（ノイズを取る）
-- ノイズだらけの信号を「近くの何点かの平均」に置きかえると、なめらかになる
-- でも窓を大きくしすぎると、本当の変化まで消えて、遅れが出る
-- マウス左右 = 窓の大きさ / 上下 = ノイズの強さ / クリックで信号を作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

newtype World = World
  { wDice :: U.Vector Float    -- 点ごとのサイコロ（-1〜1）。クリックで振り直す
  }

n :: Int
n = 400                  -- ★ 信号の点の数

-- 本当の信号：ゆっくりした波 ＋ 途中で段差
clean :: U.Vector Float
clean = U.generate n $ \i ->
  let t = fromIntegral i / fromIntegral n :: Float
  in 0.5 * sin (tau * 2 * t) + (if t > 0.6 && t < 0.75 then 0.6 else 0)

main :: IO ()
main = runSketch (sketch "05-08 Moving Average" (World (U.replicate n 0)))
  { skBack  = hsb 220 40 10
  , skStart = \_ -> World <$> rollDice
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

rollDice :: IO (U.Vector Float)
rollDice = U.replicateM n (randomRIO (-1, 1))

-- ★ 移動平均：各点を「その点までの windowSize 個の平均」に置きかえる
movingAverage :: Int -> U.Vector Float -> U.Vector Float
movingAverage windowSize x = U.generate (U.length x) $ \i ->
  let lo = max 0 (i - windowSize + 1)
      cnt = i - lo + 1
  in U.sum (U.slice lo cnt x) / fromIntegral cnt

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawSeries env noisy left right (h * 0.3) (h * 0.18) (0, 0, 70) 1 "Signal with noise"
  , drawSeries env clean left right (h * 0.3) (h * 0.18) (120, 70, 100) 2 ""
  , drawSeries env filtered left right (h * 0.72) (h * 0.18) (40, 90, 100) 3 "After the moving average"
  , drawSeries env clean left right (h * 0.72) (h * 0.18) (120, 70, 100) 1 ""
  , Color (hsba 220 40 10 0.85) (box env 0 0 ww 58)   -- 波が文字にかからないように帯を敷く
  , text' env 20 26 18 (hsb 0 0 100)
      ("Window " ++ show windowSize ++ " points (left/right)   Noise " ++ printf "%.2f" noiseAmp
       ++ " (up/down)   Click: new noise")
  , text' env 20 50 15 (hsb 0 0 60)
      "Green = true signal.  Bigger window: less noise, but the step gets blurred and late"
  ]
  where
    ww = envW env
    h = envH env
    (mx, my) = mouseP env
    windowSize = floor (mapC mx 0 ww 1 60) :: Int
    noiseAmp = mapC my 0 h 0.8 0
    noisy = U.zipWith (\v d -> v + d * noiseAmp) clean (wDice w)
    filtered = movingAverage windowSize noisy
    left = 60
    right = ww - 60

drawSeries :: Env -> U.Vector Float -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Float -> String -> Picture
drawSeries env arr left right mid amp (hue, sat, bri) weight name = Pictures [curve, title]
  where
    alpha = if null name then 0.6 else 1
    pts = [ pt env (remap (fromIntegral i) 0 (fromIntegral (U.length arr - 1)) left right) (mid - v * amp)
          | (i, v) <- zip [0 :: Int ..] (U.toList arr) ]
    curve = Color (hsba hue sat bri alpha) (if weight <= 1 then Line pts else thickPath weight pts)
    title | null name = Blank
          | otherwise = text' env left (mid - amp - 14) 14 (hsb hue sat bri) name

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> World <$> rollDice
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 窓 1（何もしない）→ 5 → 20 → 60 と変えて、ノイズと遅れのバランスを見る
-- 2. 平均の代わりに「中央値」にすると、段差が残りやすい（メディアンフィルタ）
-- 3. 窓の中で「真ん中ほど重く」平均する（重みつき）と、どう変わる？

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標（中央が原点、y は上向き）に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の map(v, a, b, c, d, true)：範囲からはみ出さない
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str

-- p5.js の rect(x, y, w, h)：左上の角と幅・高さで指定する
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y bw bh = let (gx, gy) = pt env (x + bw / 2) (y + bh / 2) in rect gx gy bw bh
