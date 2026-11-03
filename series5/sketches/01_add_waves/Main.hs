-- ============================================
-- 01 波を足す（フーリエ級数）
-- 正弦波を「1 倍・3 倍・5 倍…」の周波数で足していくと、矩形波になる！
-- どんな波も、正弦波の足し算で作れる（フーリエの定理）
-- マウス左右 = 足す波の数（1〜30） / 1〜3 キーで目標の波形 / クリックで音
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (foldl')

data World = World
  { wTarget  :: !Int     -- 1 = 矩形波, 2 = のこぎり波, 3 = 三角波
  , wPhase   :: !Float
  , wPlaying :: !Bool
  , wTimer   :: !Float   -- 次の音までの時間 [秒]
  }

noteLen, noteGap :: Float
noteLen = 0.25           -- ★ 1 音の長さ [秒]
noteGap = 0.24           -- ★ 音を鳴らす間隔 [秒]

maxPartials :: Int
maxPartials = 4          -- この数までは正弦波を 1 つずつ鳴らす（同時に鳴らせる音は 8 つまで）

main :: IO ()
main = runSketch (sketch "05-01 Adding Waves" (World 1 0 False 0))
  { skBack  = hsb 220 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- k 番目の正弦波の（周波数の倍率, 大きさ）を返す（★ ここが各波形のレシピ）
harmonic :: Int -> Int -> (Float, Float)
harmonic target k = case target of
  1 -> (odd', 1 / odd')                  -- 奇数倍、1/n
  2 -> (fromIntegral k, sign / fromIntegral k)   -- 全部、1/n、符号交互
  _ -> (odd', sign / (odd' * odd'))      -- 奇数倍、1/n²
  where
    odd' = fromIntegral (2 * k - 1) :: Float
    sign = if even k then -1 else 1

-- マウスの左右 → 足す波の数
countOf :: Env -> Int
countOf env = floor (mapC (fst (mouseP env)) 0 (envW env) 1 31)

step :: Env -> Float -> World -> IO World
step env dt w = do
  let timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  when sound (playSum (wTarget w) (min 30 (countOf env)))
  pure w
    { wPhase = wrapF tau (wPhase w + 0.03)
    , wTimer = if sound then noteGap else max 0 timer
    }

-- 少ないうちは正弦波を 1 つずつ鳴らし、多くなったら完成した波形の音で代用する
playSum :: Int -> Int -> IO ()
playSum target n
  | n <= maxPartials =
      forM_ [1 .. n] $ \k ->
        let (mult, amp) = harmonic target k
        in playTone (220 * mult) (abs amp * 0.15) noteLen Sine
  | otherwise = case target of
      1 -> playTone 220 0.12 noteLen Square
      2 -> playTone 220 0.22 noteLen Sawtooth
      _ -> playTone 220 0.18 noteLen Triangle

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (axis1 : parts ++ [total, axis2] ++ bars ++ labels)
  where
    ww = envW env
    h = envH env
    n = countOf env
    target = wTarget w
    harm = harmonic target
    -- ---- 上：足し合わせた波 ----
    mid = h * 0.38
    amp = h * 0.22
    axis1 = Color (hsb 0 0 30) (Line [pt env 0 mid, pt env ww mid])
    timeAt x = (x / ww) * tau * 2
    -- 足す前の各波（うすく）
    parts =
      [ Color (hsba (wrapF 360 (fromIntegral k * 40)) 60 100 0.35) $
          Line [ pt env x (mid - a * sin (m * timeAt x + wPhase w) * amp) | x <- [0, 3 .. ww] ]
      | k <- [1 .. min n 8], let (m, a) = harm k ]
    -- 合計
    terms = map harm [1 .. n]
    sumAt x = foldl' (\s (m, a) -> s + a * sin (m * timeAt x + wPhase w)) 0 terms  -- ★ 正弦波の足し算
    total = Color (hsb 170 80 100) $
      thickPath 3 [ pt env x (mid - sumAt x * amp * 0.8) | x <- [0, 2 .. ww] ]
    -- ---- 下：スペクトル（どの周波数がどれだけ入っているか）----
    base = h - 80
    sh = h * 0.25
    axis2 = Color (hsb 0 0 30) (Line [pt env 60 base, pt env (ww - 60) base])
    bars =
      [ Color (hsb (wrapF 360 (fromIntegral k * 40)) 60 100) $
          box env (remap m 0 62 60 (ww - 60) - 6) (base - abs a * sh) 12 (abs a * sh)
      | k <- [1 .. n], let (m, a) = harm k ]
    labels =
      [ text' env (ww - 280) (base + 24) 14 (hsb 0 0 60) "frequency (x base) ->"
      , text' env 20 (base - sh) 14 (hsb 0 0 60) "amp"
      , text' env 20 40 16 (hsb 0 0 100)
          ("Making a " ++ waveName target ++ ":  " ++ show n ++ " sines (left/right)   1-3: wave   Click: sound "
           ++ (if wPlaying w then "ON" else "OFF"))
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure w { wTarget = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wTarget = 2 }
  EventKey (Char '3') Down _ _ -> pure w { wTarget = 3 }
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n を 1 から増やして、角がだんだんできるのを見よう（30 でも完全な矩形にはならない）
-- 2. 自分のレシピを書く：harmonic で倍率と大きさを自由に決める
-- 3. 音を聞き比べる：矩形波はクラリネット、のこぎり波は弦楽器に似ている

-- ---------- 道具 ----------

waveName :: Int -> String
waveName target = case target of
  1 -> "square wave"
  2 -> "sawtooth wave"
  _ -> "triangle wave"

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
