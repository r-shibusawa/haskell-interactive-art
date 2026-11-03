-- ============================================
-- 00 正弦波（サイン波）を見る・聞く
-- すべての音・信号のもとになる、いちばん単純な波
-- マウス左右 = 周波数（高さ） / 上下 = 振幅（大きさ） / クリックで音 ON/OFF
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Text.Printf (printf)

data World = World
  { wPhase   :: !Float   -- 波の進み具合
  , wPlaying :: !Bool    -- 音を鳴らしているか
  , wTimer   :: !Float   -- 次の音までの時間 [秒]
  }

msPerScreen :: Float
msPerScreen = 20         -- ★ 画面の横幅 = 20 ミリ秒ぶんの時間

noteLen, noteGap :: Float
noteLen = 0.2            -- ★ 1 音の長さ [秒]（連続音の代わりに短い音をくり返す）
noteGap = 0.13           -- ★ 音を鳴らす間隔 [秒]

main :: IO ()
main = runSketch (sketch "05-00 Sine Wave" (World 0 False 0))
  { skBack  = hsb 220 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- マウスの左右 → 周波数（Hz：1 秒間に何回ゆれるか）
freqOf :: Env -> Float
freqOf env = mapC (fst (mouseP env)) 0 (envW env) 100 1000

-- マウスの上下 → 振幅（0〜1）
ampOf :: Env -> Float
ampOf env = mapC (snd (mouseP env)) 0 (envH env) 1 0.05

step :: Env -> Float -> World -> IO World
step env dt w = do
  let timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  -- 周波数は 1 Hz 単位にまるめて、同じ音のファイルを使いまわす
  when sound $ playTone (fromIntegral (round (freqOf env) :: Int)) (ampOf env * 0.3) noteLen Sine
  pure w
    { wPhase = wrapF tau (wPhase w + 0.02)   -- 少しずつ流れる
    , wTimer = if sound then noteGap else max 0 timer
    }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [centerLine, wave, periodBar, info]
  where
    ww = envW env
    h = envH env
    freq = freqOf env
    amp = ampOf env
    -- ---- 波を描く ----
    waveAt x =
      let t = (x / ww) * (msPerScreen / 1000)     -- 秒
          -- ★ 正弦波の式： y = 振幅 × sin(2π × 周波数 × 時間)
          y = amp * sin (tau * freq * t + wPhase w)
      in pt env x (h / 2 - y * h * 0.35)
    wave = Color (hsb 170 80 100) (thickPath 3 [waveAt x | x <- [0, 2 .. ww]])
    -- 中心線
    centerLine = Color (hsb 0 0 30) (Line [pt env 0 (h / 2), pt env ww (h / 2)])
    -- 1 周期の長さを示す
    periodPx = (1 / freq / (msPerScreen / 1000)) * ww
    periodBar = Pictures
      [ Color (hsb 40 90 100) (thickLine 2 (pt env 60 (h * 0.85)) (pt env (60 + periodPx) (h * 0.85)))
      , text' env 60 (h * 0.85 - 10) 16 (hsb 40 90 100) ("1 period = " ++ nf2 (1000 / freq) ++ " ms")
      ]
    info = Pictures
      [ text' env 20 40 18 (hsb 0 0 100)
          ("y = " ++ nf2 amp ++ " x sin(2 pi x " ++ show (round freq :: Int) ++ " x t)")
      , text' env 20 68 16 (hsb 0 0 100)
          ("Frequency " ++ show (round freq :: Int) ++ " Hz (left/right)   Amplitude " ++ nf2 amp
           ++ " (up/down)   Click: sound " ++ (if wPlaying w then "ON" else "OFF"))
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 周波数を 2 倍にすると、1 周期の長さは？（音は 1 オクターブ上がる）
-- 2. 人の耳が聞こえるのは 20〜20000 Hz。freqOf の範囲を変えて試そう
-- 3. playTone の Sine を Square にして、y を「signum (sin ...)」にすると矩形波。音はどう変わる？

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

-- 小数 2 桁の文字列
nf2 :: Float -> String
nf2 = printf "%.2f"
