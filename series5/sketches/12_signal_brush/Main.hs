-- ============================================
-- 12 波形を描いて鳴らす（自由制作のベース）
-- マウスで 1 周期ぶんの波形を描くと、それが音になる（ウェーブテーブル合成）
-- 右にはその波のスペクトル。ギザギザに描くほど倍音が増えて明るい音に
-- ドラッグで描く / マウス左右（描いていないとき）= 音の高さ / C で消す / 1〜3 でお手本
-- ============================================
-- ★ のついたところを変えて、自分だけの楽器をつくろう！
-- 音は描いた波形のスペクトルから大きい倍音を選び、短い音にして鳴らす。
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U

data World = World
  { wWave    :: !(U.Vector Float)   -- 1 周期の波形（-1〜1）
  , wSpec    :: !(U.Vector Float)   -- 波形のスペクトル
  , wPlaying :: !Bool
  , wFreq    :: !Float              -- 音の高さ [Hz]
  , wPrevX   :: !Float              -- 1 つ前のマウスの x
  , wTimer   :: !Float              -- 次の音までの時間 [秒]
  }

n :: Int
n = 256                  -- ★ 1 周期の点の数

maxK :: Int
maxK = 40                -- ★ スペクトルで見る倍音の数

noteLen, noteGap :: Float
noteLen = 0.3            -- ★ 1 音の長さ [秒]
noteGap = 0.28           -- ★ 音を鳴らす間隔 [秒]

main :: IO ()
main = runSketch (sketch "05-12 Signal Brush" (withWave (preset 1) (World U.empty U.empty False 220 0 0)))
  { skBack  = hsb 260 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- お手本の波形
preset :: Int -> U.Vector Float
preset k = U.generate n $ \i ->
  let t = fromIntegral i / fromIntegral n :: Float
  in case k of
       1 -> sin (tau * t)                 -- ★ 正弦波
       2 -> if t < 0.5 then 1 else -1     -- 矩形波
       _ -> 2 * t - 1                     -- のこぎり波

-- 波形を入れかえて、スペクトルも計算し直す
withWave :: U.Vector Float -> World -> World
withWave wave w = w { wWave = wave, wSpec = dft wave }

dft :: U.Vector Float -> U.Vector Float
dft x = U.generate maxK $ \k ->
  let re = U.sum (U.imap (\i v -> v * cosTable U.! ((k * i) `mod` n)) x)
      im = U.sum (U.imap (\i v -> v * sinTable U.! ((k * i) `mod` n)) x)
  in 2 * sqrt (re * re + im * im) / fromIntegral n

cosTable, sinTable :: U.Vector Float
cosTable = U.generate n (\i -> cos (tau * fromIntegral i / fromIntegral n))
sinTable = U.generate n (\i -> sin (tau * fromIntegral i / fromIntegral n))

step :: Env -> Float -> World -> IO World
step env dt w = do
  let freq | envDown env = wFreq w
           | otherwise = 2 ** mapC (fst (mouseP env)) 0 (envW env) (logBase 2 80) (logBase 2 880)
      timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  when sound (playWave freq (wSpec w))
  pure w { wFreq = freq, wTimer = if sound then noteGap else max 0 timer }

-- 大きい倍音を 4 つまで同時に鳴らす（1 倍音 = いま選んでいる高さ）
playWave :: Float -> U.Vector Float -> IO ()
playWave freq spec = do
  let harmonics = [ (k, a) | k <- [1 .. maxK - 1], let a = spec U.! k, a > 0.01
                           , freq * fromIntegral k < 10000 ]
      strongest = take 4 (sortOn (negate . snd) harmonics)
      total = sum (map snd strongest) * 0.25
      scale_ = if total > 0.8 then 0.8 / total else 1
  forM_ strongest $ \(k, a) ->
    playTone (fromIntegral (round (freq * fromIntegral k) :: Int)) (a * 0.25 * scale_) noteLen Sine

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([frame, axis, curve, axis2] ++ bars ++ [label1, label2, info1, info2])
  where
    ww = envW env
    h = envH env
    left = 60
    right = ww * 0.55
    mid = h * 0.5
    amp = h * 0.3
    -- ---- 左：波形（1 周期）----
    frame = Color (hsb 0 0 30) (uncurry rectLine (pt env ((left + right) / 2) mid) (right - left) (amp * 2))
    axis = Color (hsb 0 0 30) (Line [pt env left mid, pt env right mid])
    curve = Color (hsb 40 90 100) $ thickPath 3
      [ pt env (remap (fromIntegral i) 0 (fromIntegral n - 1) left right) (mid - v * amp)
      | (i, v) <- zip [0 :: Int ..] (U.toList (wWave w)) ]
    -- ---- 右：スペクトル（DFT）----
    sl = ww * 0.62
    sr = ww - 60
    base = mid + amp
    axis2 = Color (hsb 0 0 30) (Line [pt env sl base, pt env sr base])
    bars =
      [ Color (hsb (remap (fromIntegral k) 0 40 180 330) 70 100) $      -- ★ 色
          box env (remap (fromIntegral k) 0 (fromIntegral maxK) sl sr) (base - v * amp * 1.8)
                  ((sr - sl) / fromIntegral maxK - 3) (v * amp * 1.8)
      | k <- [1 .. maxK - 1], let v = wSpec w U.! k ]
    label1 = text' env sl (base + 24) 14 (hsb 0 0 60) "Harmonics (x1 to x39)"
    label2 = text' env left (mid - amp - 10) 14 (hsb 0 0 60) "Waveform (one cycle)"
    info1 = text' env 20 40 18 (hsb 0 0 100)
      ("Drag: draw the wave   Left/right: pitch " ++ show (round (wFreq w) :: Int) ++ " Hz   1-3: presets   C: clear")
    info2 = text' env 20 68 15 (hsb 0 0 60)
      ("Click outside the box: sound " ++ (if wPlaying w then "ON" else "OFF"))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < 60 -> w { wPrevX = mx }
    | mx > 60 && mx < envW env * 0.55 -> w { wPrevX = mx }
    | otherwise -> w { wPlaying = not (wPlaying w), wTimer = 0, wPrevX = mx }
  EventMotion _
    | envDown env -> (paint env w) { wPrevX = mx }
    | otherwise -> w { wPrevX = mx }
  EventKey (Char 'c') Down _ _ -> withWave (U.replicate n 0) w
  EventKey (Char 'C') Down _ _ -> withWave (U.replicate n 0) w
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> withWave (preset (fromEnum c - fromEnum '0')) w
  _ -> w
  where
    (mx, my) = mouseP env

-- ドラッグした場所の波形を書きかえる
paint :: Env -> World -> World
paint env w
  | i < 0 || i >= n = w
  -- 前の位置から今の位置まで線で埋める（飛びを防ぐ）
  | otherwise = withWave (wWave w U.// [ (k, v) | k <- [max 0 (min i pi') .. min (n - 1) (max i pi')] ]) w
  where
    left = 60
    right = envW env * 0.55
    mid = envH env * 0.5
    amp = envH env * 0.3
    (mx, my) = mouseP env
    index x = floor (remap x left right 0 (fromIntegral n - 1)) :: Int
    i = index mx
    pi' = index (wPrevX w)
    v = clampF (-1) 1 (remap my (mid - amp) (mid + amp) 1 (-1))

-- ---------- 自由制作のアイデア ----------
-- ・描いた波形を少しずつ変化させる（毎フレーム wWave を sin で揺らす）
-- ・キーボードで音階を弾けるようにする（シリーズ 1 の 09 を参考に）
-- ・スペクトルの色や形を作品にする

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
