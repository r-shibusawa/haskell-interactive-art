-- ============================================
-- 04 量子化（値を「段」にする）
-- サンプリングで時間を点にしたら、今度は値（高さ）も段にする＝量子化
-- 段の数 = 2^ビット。ビットが少ないと、波がガタガタになり、ノイズが混ざる
-- マウス左右 = ビット数（1〜8） / クリックで音を聞く
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (foldl', sortOn)
import Text.Printf (printf)

data World = World
  { wPhase   :: !Float
  , wPlaying :: !Bool
  , wTimer   :: !Float   -- 次の音までの時間 [秒]
  }

noteLen, noteGap :: Float
noteLen = 0.3            -- ★ 1 音の長さ [秒]
noteGap = 0.28           -- ★ 音を鳴らす間隔 [秒]

baseFreq :: Float
baseFreq = 330           -- ★ 量子化する正弦波の周波数 [Hz]

main :: IO ()
main = runSketch (sketch "05-04 Quantization" (World 0 False 0))
  { skBack  = hsb 220 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 量子化：-1〜1 の値を、段の数 levels のどれかに丸める
quantize :: Float -> Float -> Float
quantize levels v = roundHalf ((v + 1) / 2 * (levels - 1)) / (levels - 1) * 2 - 1

-- マウスの左右 → ビット数
bitsOf :: Env -> Int
bitsOf env = floor (mapC (fst (mouseP env)) 0 (envW env) 1 9)

levelsOf :: Int -> Float
levelsOf b = 2 ^ b

step :: Env -> Float -> World -> IO World
step env dt w = do
  let timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  when sound (playQuantized (bitsOf env))
  pure w
    { wPhase = wrapF tau (wPhase w + 0.02)
    , wTimer = if sound then noteGap else max 0 timer
    }

-- 量子化した正弦波の音。自由な波形は鳴らせないので、波形を周波数に分解して
-- 大きい成分を 4 つまで同時に鳴らす（1 ビットは矩形波そのもの）
playQuantized :: Int -> IO ()
playQuantized 1 = playTone baseFreq 0.3 noteLen Square
playQuantized b =
  forM_ (partials b) $ \(k, a) ->
    playTone (baseFreq * fromIntegral k) (a * 0.3) noteLen Sine

-- 量子化した波の 1 周期をフーリエ係数に分解し、（倍音の番号, 大きさ）を大きい順に返す
partials :: Int -> [(Int, Float)]
partials b = take 4 (sortOn (negate . snd) (filter ((> 0.01) . snd) (map coef [1, 3 .. 21])))
  where
    n = 512 :: Int
    wave i = quantize (levelsOf b) (sin (tau * fromIntegral i / fromIntegral n))
    coef k =
      let s = foldl' (\acc i -> acc + wave i * sin (tau * fromIntegral (k * i) / fromIntegral n)) 0 [0 .. n - 1]
      in (k, abs (2 * s / fromIntegral n))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (steps ++ [original, quantized, noise, noiseLabel, info1, info2])
  where
    ww = envW env
    h = envH env
    bits = bitsOf env
    levels = levelsOf bits
    left = 60
    right = ww - 60
    mid = h * 0.45
    amp = h * 0.3
    -- 位置 x での元の波の値
    sig x = sin (tau * 2 * (x - left) / (right - left) + wPhase w)
    -- 段の線
    steps =
      [ Color (hsb 0 0 25) (Line [pt env left y, pt env right y])
      | levels <= 64
      , i <- [0 .. levels - 1]
      , let y = mid - (i / (levels - 1) * 2 - 1) * amp ]
    -- 元の波
    original = Color (hsba 170 60 100 0.5) $
      thickPath 2 [ pt env x (mid - sig x * amp) | x <- [left, left + 2 .. right] ]
    -- 量子化した波
    quantized = Color (hsb 40 90 100) $
      thickPath 3 [ pt env x (mid - quantize levels (sig x) * amp) | x <- [left .. right] ]
    errSum = foldl' (\s x -> s + abs (sig x - quantize levels (sig x))) 0 [left .. right]
    -- 誤差（ノイズ）
    noise = Color (hsba 0 90 100 0.7) $
      thickPath 1.5 [ pt env x (h * 0.85 - (sig x - quantize levels (sig x)) * amp * 2)
                    | x <- [left, left + 2 .. right] ]
    noiseLabel = text' env left (h * 0.85 - amp * 0.35) 14 (hsb 0 90 100) "Error (quantization noise) x2"
    info1 = text' env 20 40 18 (hsb 0 0 100)
      (show bits ++ " bits = " ++ show (round levels :: Int) ++ " levels (left/right)   Mean error "
       ++ printf "%.4f" (errSum / (right - left)) ++ "   Click: sound " ++ (if wPlaying w then "ON" else "OFF"))
    info2 = text' env 20 68 15 (hsb 0 0 60)
      "A CD uses 16 bits = 65536 levels. Each extra bit lowers the noise by about 6 dB"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 ビット（2 段）の音を聞こう。ブザーみたいになる
-- 2. 誤差のグラフを見ると、ビットが多いほど誤差が細かく・小さくなる
-- 3. quantize の roundHalf を「切り捨て」にすると、波が片側にずれる（バイアス）

-- ---------- 道具 ----------

-- JavaScript の Math.round と同じ四捨五入（Haskell の round は 0.5 を偶数に丸める）
roundHalf :: Float -> Float
roundHalf x = fromIntegral (floor (x + 0.5) :: Int)

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
