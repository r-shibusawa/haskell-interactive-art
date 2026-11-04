-- ============================================
-- 15 うなり（おまけ）
-- 周波数がほんの少しちがう 2 つの音を同時に鳴らすと、「ワンワンワン」と大きさがゆれる
-- ゆれの速さ = 2 つの周波数の差。ギターのチューニングはこれを使う
-- マウス左右 = 2 つ目の周波数（440 ± 20 Hz） / クリックで音 ON/OFF
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Text.Printf (printf)

data World = World
  { wPhase   :: !Float
  , wPlaying :: !Bool
  , wTimer   :: !Float   -- 次の音までの時間 [秒]
  }

f1 :: Float
f1 = 440                 -- ★ 1 つ目の周波数 [Hz]

secondsOnScreen :: Float
secondsOnScreen = 1.0    -- ★ 画面の横幅 = 1 秒

noteLen, noteGap :: Float
noteLen = 1.0            -- ★ 1 音の長さ [秒]（うなりが聞こえるように長めにする）
noteGap = 0.9            -- ★ 音を鳴らす間隔 [秒]

main :: IO ()
main = runSketch (sketch "05-15 Beats" (World 0 False 0))
  { skBack  = hsb 220 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

freq2 :: Env -> Float
freq2 env = mapC (fst (mouseP env)) 0 (envW env) 420 460

step :: Env -> Float -> World -> IO World
step env dt w = do
  let timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
      -- 2 つ目の周波数は 0.1 Hz 単位にまるめて、同じ音のファイルを使いまわす
      f2 = fromIntegral (round (freq2 env * 10) :: Int) / 10
  when sound (playChord [f1, f2] 0.15 noteLen Sine)
  pure w
    { wPhase = wrapF tau (wPhase w + 0.5)
    , wTimer = if sound then noteGap else max 0 timer
    }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (twoWaves ++ [axis, summed, envelope 1, envelope (-1), info1, info2])
  where
    ww = envW env
    h = envH env
    f2 = freq2 env
    phase = wPhase w
    left = 60
    right = ww - 60
    timeAt x = (x - left) / (right - left) * secondsOnScreen
    xs1 = [left .. right]
    -- 2 つの波（上、うすく）
    twoWaves = concat
      [ [ Color (hsba hue 70 100 0.6) $ thickPath 1.5
            [ pt env x (mid - sin (tau * f * timeAt x + phase) * h * 0.06) | x <- xs1 ]
        , text' env left (mid - h * 0.07) 14 (hsb hue 70 100) (nf1 f ++ " Hz")
        ]
      | (f, mid, hue) <- [(f1, h * 0.22, 170), (f2, h * 0.38, 40)] ]
    -- 足した波（下、大きく）＝ うなり
    mid2 = h * 0.7
    amp = h * 0.2
    axis = Color (hsb 0 0 30) (Line [pt env left mid2, pt env right mid2])
    summed = Color (hsb 0 0 100) $ Line
      [ pt env x (mid2 - v * amp)
      | x <- xs1
      , let t = timeAt x
            v = (sin (tau * f1 * t + phase) + sin (tau * f2 * t + phase)) / 2 ]   -- ★ 足し算
    -- 包絡線（大きさのゆれ）
    envelope sgn = Color (hsb 0 90 100) $ thickPath 3
      [ pt env x (mid2 - sgn * abs (cos (pi * (f2 - f1) * timeAt x)) * amp)
      | x <- [left, left + 2 .. right] ]
    info1 = text' env 20 40 16 (hsb 0 0 100)
      ("f1 = 440 Hz   f2 = " ++ nf1 f2 ++ " Hz (left/right)   Beats " ++ nf1 (abs (f2 - f1))
       ++ " times a second   Click: sound " ++ (if wPlaying w then "ON" else "OFF"))
    info2 = text' env 20 68 15 (hsb 0 0 60)
      "Red = the loudness swing (envelope). It slows as the gap nears 0 and stops when they match"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. f2 を 440 にぴったり合わせて、うなりを消そう（チューニング！）
-- 2. 差を 20 Hz にすると、うなりは速すぎて「ざらざらした音」に聞こえる
-- 3. 3 つの音を鳴らすと、うなりはどうなる？

-- ---------- 道具 ----------

-- 小数 1 桁の文字列
nf1 :: Float -> String
nf1 = printf "%.1f"

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
