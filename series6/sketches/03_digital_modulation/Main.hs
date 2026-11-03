-- ============================================
-- 03 デジタル変調（0 と 1 を波で送る）
-- ビット列を波にする 3 つの方法：
--   ASK：1 なら波あり、0 なら小さく   FSK：1 と 0 で周波数を変える   PSK：1 と 0 で波の向き（位相）を反転
-- 1〜3 キーで方式 / クリックでビットをランダムに / 上下キーで速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (digitToInt)

data World = World
  { wBits   :: ![Int]     -- 送るビット列
  , wMode   :: !Int       -- 1 = ASK, 2 = FSK, 3 = PSK
  , wScroll :: !Float     -- 走る点の位置
  , wSpeed  :: !Float     -- 走る点の速さ
  }

main :: IO ()
main = runSketch (sketch "06-03 Digital Modulation" (World [1, 0, 1, 1, 0, 0, 1, 0] 1 0 2))
  { skBack  = hsb 230 40 10
  , skStep  = \_ _ w -> pure w { wScroll = wScroll w + wSpeed w }
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 3 つの方式。b はビット、t はビット単位の時間
wave :: Int -> Int -> Float -> Float
wave 1 b t = (if b == 1 then 1 else 0.2) * sin (tau * 6 * t)              -- ASK
wave 2 b t = sin (tau * (if b == 1 then 9 else 4) * t)                     -- FSK
wave _ b t = sin (tau * 6 * t + (if b == 1 then 0 else pi))                -- PSK：0 のとき位相を 180° 反転

-- t の位置にあるビットで波の値を計算する
waveAt :: World -> Float -> Float
waveAt w t = wave (wMode w) (bitAt (wBits w) t) t

bitAt :: [Int] -> Float -> Int
bitAt bits t = case drop (min (floor t) (length bits - 1)) bits of
  (b : _) -> b
  [] -> 0

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Pictures (zipWith drawBit [0 ..] bits)
  , Color (hsb 0 0 25) (Line [toG env left mid, toG env right mid])
  , Color (hsb 40 90 100) (thickPath 2.5 [ toG env x (mid - waveAt w ((x - left) / bitW) * h)
                                         | k <- [0 .. n], let x = left + fromIntegral k ])
  , Color (hsb 0 0 100) (at (toG env px (mid - waveAt w ((px - left) / bitW) * h)) (disc 7))   -- 波の上を走る点
  , txt env 20 40 18 (hsb 0 0 100) (name ++ "   1-3: method   Click: random bits   Up/Down: speed")
  , txt env 20 68 15 (hsb 0 0 60) desc
  ]
  where
    bits = wBits w
    left = 60
    right = envW env - 60
    bitW = (right - left) / fromIntegral (length bits)    -- 1 ビットの横幅
    mid = envH env * 0.58
    h = envH env * 0.2
    n = floor (right - left) :: Int
    px = left + wrapF (right - left) (wScroll w)
    (name, desc) = case wMode w of
      1 -> ("ASK (amplitude)", "1 = big wave, 0 = small wave. Simple, but weak to noise (like fiber-optic ON/OFF)")
      2 -> ("FSK (frequency)", "1 = high frequency, 0 = low frequency. The old modem's beeping. Strong against noise")
      _ -> ("PSK (phase)", "1 and 0 flip the wave's direction. Wi-Fi and phones extend this to QPSK / QAM (see 04)")

    -- ---- ビット列 ----
    drawBit :: Float -> Int -> Picture
    drawBit i b =
      let x = left + i * bitW
          y = envH env * 0.18
      in Pictures
           [ roundBox env (x + 2) y (bitW - 4) 50 6 (if b == 1 then hsb 45 90 100 else hsb 230 30 35)
           , txtC env (x + bitW / 2) (y + 25) 24 (if b == 1 then hsb 230 40 20 else hsb 0 40 70) (show b)
           , Color (hsb 0 0 25) (Line [toG env x (envH env * 0.3), toG env x (envH env * 0.85)])
           ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure w { wMode = digitToInt c }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 8 (wSpeed w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0 (wSpeed w - 1) }
  EventKey (MouseButton LeftButton) Down _ _ -> do
    new <- replicateM (length (wBits w)) (randomRIO (0, 1 :: Int))
    pure w { wBits = new }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. PSK で、ビットが変わる境目の波をよく見る。急にひっくり返っている
-- 2. wave の 1 ビットの中の波の数（6）を増やすと、1 秒に送れるビット数は減る？増える？
-- 3. 4 種類の位相（0°, 90°, 180°, 270°）を使えば 1 回で 2 ビット送れる（QPSK）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 文字（中央が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 角の丸い長方形（左上が x, y、角の半径 r）
roundBox :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
roundBox env x y w h r c = Color c (Polygon (concat
  [ corner (x + w - r) (y + r) (-90), corner (x + w - r) (y + h - r) 0
  , corner (x + r) (y + h - r) 90, corner (x + r) (y + r) 180 ]))
  where
    corner cx cy a0 = [ toG env (cx + r * cos a) (cy + r * sin a)
                      | i <- [0 .. 4 :: Int], let a = (a0 + 22.5 * fromIntegral i) * pi / 180 ]
