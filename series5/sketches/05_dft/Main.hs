-- ============================================
-- 05 フーリエ変換（波を周波数に分解する）
-- 2 つの正弦波を混ぜた信号を、フーリエ変換で「どの周波数がどれだけ」に分解する
-- 01 の逆：足した波から、材料をとり出す
-- マウス左右 = 2 つ目の波の周波数 / 上下 = その大きさ / N キーでノイズを足す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data World = World
  { wNoise    :: !Bool
  , wF2       :: !Int                -- 2 つ目の波の周波数
  , wA2       :: !Float              -- 2 つ目の波の大きさ
  , wSignal   :: !(U.Vector Float)   -- 信号
  , wSpectrum :: !(U.Vector Float)   -- 周波数ごとの大きさ
  }

n :: Int
n = 256                  -- ★ 信号の点の数

f1 :: Int
f1 = 5                   -- ★ 1 つ目の波の周波数

noiseAmount :: Float
noiseAmount = 0.8        -- ★ ノイズの大きさ

main :: IO ()
main = runSketch (sketch "05-05 Fourier Transform" (World False 20 0.5 (U.replicate n 0) (U.replicate (n `div` 2) 0)))
  { skBack  = hsb 220 40 10
  , skStep  = \env _ w -> step env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> World -> IO World
step env w = do
  let (mx, my) = mouseP env
      f2 = floor (mapC mx 0 (envW env) 1 60) :: Int
      a2 = mapC my 0 (envH env) 1 0
      wave f t = sin (tau * fromIntegral f * t)
      pure' = U.generate n $ \i ->
        let t = fromIntegral i / fromIntegral n
        in wave f1 t + a2 * wave f2 t
  -- ---- 信号をつくる ----
  noise <- if wNoise w
             then U.replicateM n (randomRIO (-noiseAmount, noiseAmount))
             else pure (U.replicate n 0)
  let signal = U.zipWith (+) pure' noise
  -- ---- フーリエ変換（DFT）----
  pure w { wF2 = f2, wA2 = a2, wSignal = signal, wSpectrum = dft signal }

-- ★ 離散フーリエ変換：周波数 k の成分の大きさを、全部の k について求める
dft :: U.Vector Float -> U.Vector Float
dft x = U.generate (n `div` 2) $ \k ->
  let -- 周波数 k の余弦波・正弦波と、どれだけ「似ているか」を足し合わせる
      re = U.sum (U.imap (\i v -> v * cosTable U.! ((k * i) `mod` n)) x)
      im = U.sum (U.imap (\i v -> v * sinTable U.! ((k * i) `mod` n)) x)
  in 2 * sqrt (re * re + im * im) / fromIntegral n   -- 大きさ（0〜1 くらい）

-- cos と sin は同じ角度を何度も使うので、表にしておくと速い
cosTable, sinTable :: U.Vector Float
cosTable = U.generate n (\i -> cos (tau * fromIntegral i / fromIntegral n))
sinTable = U.generate n (\i -> sin (tau * fromIntegral i / fromIntegral n))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([axis1, signalLine, label1, axis2] ++ bars ++ [label2] ++ ticks ++ [info])
  where
    ww = envW env
    h = envH env
    left = 60
    right = ww - 60
    -- ---- 上：信号 ----
    mid = h * 0.27
    amp = h * 0.17
    axis1 = Color (hsb 0 0 30) (Line [pt env left mid, pt env right mid])
    signalLine = Color (hsb 170 80 100) $ thickPath 2
      [ pt env (remap (fromIntegral i) 0 (fromIntegral n - 1) left right) (mid - v / 2.5 * amp)
      | (i, v) <- zip [0 :: Int ..] (U.toList (wSignal w)) ]
    label1 = text' env left (mid - amp - 10) 14 (hsb 0 0 60) "Signal (time)"
    -- ---- 下：スペクトル（周波数ごとの大きさ）----
    base = h - 80
    sh = h * 0.3
    axis2 = Color (hsb 0 0 30) (Line [pt env left base, pt env right base])
    xOf k = remap (fromIntegral k) 0 64 left right
    bars =
      [ Color (hsb (if k == f1 || k == wF2 w then 40 else 170) 80 100) $
          box env (xOf k) (base - mag * sh) ((right - left) / 64 - 2) (mag * sh)
      | k <- [0 .. 63 :: Int], let mag = wSpectrum w U.! k ]
    label2 = text' env left (base - sh - 10) 14 (hsb 0 0 60) "Spectrum (frequency 0-63)"
    ticks = [ text' env (xOf k) (base + 20) 14 (hsb 0 0 60) (show k) | k <- [0, 10 .. 60 :: Int] ]
    info = text' env 20 40 18 (hsb 0 0 100)
      ("Signal = sin(" ++ show f1 ++ " Hz) + " ++ printf "%.2f" (wA2 w) ++ " x sin(" ++ show (wF2 w)
       ++ " Hz)   (mouse: 2nd wave)   N: noise " ++ (if wNoise w then "ON" else "OFF"))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char 'n') Down _ _ -> pure w { wNoise = not (wNoise w) }
  EventKey (Char 'N') Down _ _ -> pure w { wNoise = not (wNoise w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. マウスを動かして、スペクトルの山が 2 つ目の周波数についてくるのを確認
-- 2. N でノイズを足しても、山ははっきり見える（ノイズは全周波数に薄く広がる）
-- 3. 信号を 3 つの波の和にして、山が 3 つ出るか確かめよう

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

-- p5.js の rect(x, y, w, h)：左上の角と幅・高さで指定する
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y bw bh = let (gx, gy) = pt env (x + bw / 2) (y + bh / 2) in rect gx gy bw bh

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str
