-- ============================================
-- 08 テルミン（マウスで演奏する電子楽器）
-- マウスを押している間だけ音が鳴る
-- 左右 = 音の高さ / 上下 = 音の大きさ
-- 1〜4 キーで音色を変更
-- ============================================
-- 音は「短い音のくり返し」で鳴らし、波形は同じ周波数と音量から計算して描く。
module Main (main) where

import Art
import Control.Monad (when)

data World = World
  { wPlaying :: !Bool
  , wWave    :: !Wave
  , wFreq    :: !Float      -- 今の周波数（なめらかに追いかける）
  , wAmp     :: !Float      -- 今の音量（なめらかに追いかける）
  , wPhase   :: !Float      -- 波形の位相
  , wTimer   :: !Float      -- 次の音までの時間 [秒]
  }

noteLen, noteGap :: Float
noteLen = 0.2               -- ★ 1 音の長さ [秒]
noteGap = 0.13              -- ★ 音を鳴らす間隔 [秒]

main :: IO ()
main = runSketch (sketch "01-08 Theremin" (World False Sine 440 0 0 0))
  { skBack  = rgb255 15 15 30
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- マウス位置 → 周波数（音の高さ）
mouseFreq :: Env -> Float
mouseFreq env = clampF 100 1200 (remap (fst (envMouse env)) (-envW env / 2) (envW env / 2) 100 1200)  -- ★ 周波数の範囲

-- マウス位置 → 音量（上ほど大きい）
mouseAmp :: Env -> Float
mouseAmp env = clampF 0 0.6 (remap (snd (envMouse env)) (-envH env / 2) (envH env / 2) 0 0.6)

step :: Env -> Float -> World -> IO World
step env dt w = do
  let freq = mouseFreq env
      amp = mouseAmp env
      -- 目標の値へ指数的に近づける（p5.js の setTargetAtTime と同じ考え方）
      approach tc cur target = target + (cur - target) * exp (-dt / tc)
      freq' = if wPlaying w then approach 0.02 (wFreq w) freq else wFreq w
      amp' = if wPlaying w then approach 0.02 (wAmp w) amp else approach 0.1 (wAmp w) 0
      timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  -- 周波数は 1 Hz 単位、音量は 0.02 単位にまるめて、同じ音のファイルを使いまわす
  when (sound && amp > 0.01) $
    playTone (fromIntegral (round freq :: Int)) (fromIntegral (round (amp * 50) :: Int) / 50) noteLen (wWave w)
  pure w
    { wFreq = freq'
    , wAmp = amp'
    , wPhase = fract (wPhase w + freq' * dt)
    , wTimer = if sound then noteGap else max 0 timer
    }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [waveLine, marker, info]
  where
    ww = envW env
    h = envH env
    freq = mouseFreq env
    amp = mouseAmp env
    -- ---- 音の波形を描く ----
    samples = 512 :: Int           -- 波形の点の数
    span_ = 1024 / 44100 :: Float  -- p5.js の analyser と同じ、約 23 ミリ秒ぶんを見る
    waveAt i =
      let t = fromIntegral i / fromIntegral samples
      in wAmp w * osc (wWave w) (wPhase w + wFreq w * span_ * t)
    waveLine = Color (rgb255 0 255 200) $ thickPath 4
      [ (-ww / 2 + ww * fromIntegral i / fromIntegral samples, waveAt i * h * 0.4)
      | i <- [0 .. samples] ]
    -- ---- マウスの位置に印 ----
    marker = Color (rgba255 255 80 150 (if wPlaying w then 255 else 80))
                   (at (envMouse env) (disc ((30 + amp * 60) / 2)))
    -- ---- 説明 ----
    line1 = "Hold the mouse to play   left/right = pitch   up/down = volume"
    line2 = "1:sine  2:triangle  3:sawtooth  4:square    now: " ++ waveName (wWave w)
    line3 = show (round freq :: Int) ++ " Hz"
    info = Pictures
      [ label (-ww / 2 + 20) (h / 2 - 34) 16 white line1
      , label (-ww / 2 + 20) (h / 2 - 60) 16 white line2
      , label (-ww / 2 + 20) (h / 2 - 86) 16 white line3
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = True, wTimer = 0 }
  EventKey (MouseButton LeftButton) Up _ _   -> pure w { wPlaying = False }
  EventKey (Char '1') Down _ _ -> pure w { wWave = Sine }
  EventKey (Char '2') Down _ _ -> pure w { wWave = Triangle }
  EventKey (Char '3') Down _ _ -> pure w { wWave = Sawtooth }
  EventKey (Char '4') Down _ _ -> pure w { wWave = Square }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 音色を 1〜4 で切り替えて、波の形と音の違いを比べよう
-- 2. 周波数の範囲 100〜1200 を変えてみよう（人の耳は約 20〜20000 Hz）
-- 3. 波形の線の色を、音の高さで変えてみよう（hsb (freq / 4) 80 100 など）

-- ---------- 道具 ----------

waveName :: Wave -> String
waveName wave = case wave of
  Sine -> "sine"
  Triangle -> "triangle"
  Sawtooth -> "sawtooth"
  Square -> "square"

-- 波の形：位相 p（1 で一周）での値を -1〜1 で返す
osc :: Wave -> Float -> Float
osc wave p0 =
  let p = fract p0
  in case wave of
       Sine -> sin (tau * p)
       Triangle -> 4 * abs (p - 0.5) - 1
       Sawtooth -> 2 * p - 1
       Square -> if p < 0.5 then 1 else -1
