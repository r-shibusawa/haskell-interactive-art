-- ============================================
-- 14 ドップラー効果（おまけ）
-- 救急車が通りすぎると音が「ピーポー」→ 低くなる。近づくと波が縮み、遠ざかると伸びる
-- 音源が動き、波紋が広がり、観測者（マウス）が聞く周波数が変わる
-- マウス = 観測者の位置 / クリックで音 ON/OFF / 上下キー = 音源の速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Text.Printf (printf)

data Ripple = Ripple { rX :: !Float, rY :: !Float, rR :: !Float }

data World = World
  { wSx      :: !Float      -- 音源の位置 x（y は固定）
  , wSpeed   :: !Float
  , wRipples :: ![Ripple]
  , wPlaying :: !Bool
  , wTimer   :: !Float      -- 次の音までの時間 [秒]
  }

baseFreq, soundSpeed :: Float
baseFreq = 440              -- ★ 音源の周波数 [Hz]
soundSpeed = 8              -- ★ 波の速さ（音速）

noteLen, noteGap :: Float
noteLen = 0.2               -- ★ 1 音の長さ [秒]
noteGap = 0.13              -- ★ 音を鳴らす間隔 [秒]

main :: IO ()
main = runSketch (sketch "05-14 Doppler Effect" (World 0 3 [] False 0))
  { skBack  = hsb 220 40 10
  , skStart = \env -> pure (World (-envW env / 2) 3 [] False 0)
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 音源の高さ
sourceY :: Env -> Float
sourceY env = envH env / 2 - envH env * 0.55

-- ★ ドップラーの式：聞こえる周波数 = f × 音速 / (音速 − 近づく速さ)
hear :: Env -> World -> (Float, Float)
hear env w =
  let (mx, my) = envMouse env
      dx = mx - wSx w
      dy = my - sourceY env
      d = max (sqrt (dx * dx + dy * dy)) 1
      approach = wSpeed w * dx / d    -- 音源が観測者に近づく速さ（遠ざかると負）
  in (clampF 100 2000 (baseFreq * soundSpeed / (soundSpeed - approach)), d)

step :: Env -> Float -> World -> IO World
step env dt w = do
  let ww = envW env
      -- 音源が左から右へ
      moved = wSx w + wSpeed w
      sx = if moved > ww / 2 + 100 then -ww / 2 - 100 else moved
      -- 一定間隔で波紋を出す（波の山）
      born = [Ripple sx (sourceY env) 0 | envFrame env `mod` 6 == 0]
      grown = [ r { rR = rR r + soundSpeed } | r <- wRipples w, rR r + soundSpeed <= ww ]
      w' = w { wSx = sx, wRipples = born ++ grown }
      (heard, d) = hear env w'
      timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  when sound $
    playTone (fromIntegral (round heard :: Int)) (mapC d 0 ww 0.3 0.02) noteLen Triangle
  pure w' { wTimer = if sound then noteGap else max 0 timer }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (ripples ++ [source, observer, info1, info2])
  where
    ww = envW env
    h = envH env
    ripples =
      [ Color (hsba 170 60 100 (remap (rR r) 0 ww 0.7 0)) (at (rX r, rY r) (ring (rR r) 2))
      | r <- wRipples w ]
    source = Color (hsb 0 90 100) (at (wSx w, sourceY env) (disc 15))
    observer = Color (hsb 40 90 100) (at (envMouse env) (disc 12))
    (heard, _) = hear env w
    x0 = 20 - ww / 2
    info1 = label x0 (h / 2 - 40) 16 (hsb 0 0 100)
      ("Source speed " ++ printf "%.1f" (wSpeed w) ++ " (up/down keys)   Observer = mouse   Heard "
       ++ show (round heard :: Int) ++ " Hz (source " ++ show (round baseFreq :: Int) ++ " Hz)")
    info2 = label x0 (h / 2 - 68) 15 (hsb 0 0 60)
      ("Ripples bunch up ahead (higher), stretch behind (lower)   Click: sound "
       ++ (if wPlaying w then "ON" else "OFF"))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 7 (wSpeed w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0 (wSpeed w - 0.5) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 観測者を音源の通り道の真上に置くと、通過の瞬間に音が急に下がる
-- 2. 速さを音速（8）に近づけると？（衝撃波・ソニックブーム）
-- 3. 宇宙の星の光も同じ（赤方偏移）。遠ざかる銀河の光は赤く見える

-- ---------- 道具 ----------

-- p5.js の map(v, a, b, c, d, true)：範囲からはみ出さない
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)
