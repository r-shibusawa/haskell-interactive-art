-- ============================================
-- 13 周波数でノイズを消す（スペクトル・フィルタリング）
-- 信号 → フーリエ変換 → 小さい成分を 0 にする → 逆変換 で、ノイズだけ消える
-- 08 の移動平均より賢い：「本物の信号」の周波数は残し、それ以外を消す
-- マウス左右 = しきい値 / 上下 = ノイズの量 / クリックで信号を作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data World = World
  { wClean :: !(U.Vector Float)   -- 本物の信号
  , wDice  :: !(U.Vector Float)   -- 点ごとのサイコロ（-1〜1）。クリックで振り直す
  }

n :: Int
n = 256                  -- ★ 信号の点の数

main :: IO ()
main = runSketch (sketch "05-13 Denoise" (World (U.replicate n 0) (U.replicate n 0)))
  { skBack  = hsb 220 40 10
  , skStart = \_ -> newSignal
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 2 つの正弦波を混ぜた信号と、ノイズのもとを作る
newSignal :: IO World
newSignal = do
  f1 <- randomRIO (2, 7 :: Int)
  f2 <- randomRIO (10, 24 :: Int)
  dice <- U.replicateM n (randomRIO (-1, 1))
  let clean = U.generate n $ \i ->
        let t = fromIntegral i / fromIntegral n :: Float
        in sin (tau * fromIntegral f1 * t) + 0.5 * sin (tau * fromIntegral f2 * t)
  pure (World clean dice)

-- cos と sin は同じ角度を何度も使うので、表にしておくと速い
cosTable, sinTable :: U.Vector Float
cosTable = U.generate n (\i -> cos (tau * fromIntegral i / fromIntegral n))
sinTable = U.generate n (\i -> sin (tau * fromIntegral i / fromIntegral n))

-- フーリエ変換：（実部, 虚部）を返す
dft :: U.Vector Float -> (U.Vector Float, U.Vector Float)
dft x =
  ( U.generate n (\k -> U.sum (U.imap (\i v -> v * cosTable U.! ((k * i) `mod` n)) x))
  , U.generate n (\k -> negate (U.sum (U.imap (\i v -> v * sinTable U.! ((k * i) `mod` n)) x))) )

-- 逆変換：成分から信号を組み立て直す
idft :: U.Vector Float -> U.Vector Float -> U.Vector Float
idft re im = U.generate n $ \i ->
  let part k = re U.! k * cosTable U.! ((k * i) `mod` n) - im U.! k * sinTable U.! ((k * i) `mod` n)
  in U.sum (U.generate n part) / fromIntegral n

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ([ drawSeries env noisy left right (h * 0.2) (h * 0.1) (0, 0, 70) 1 "With noise"
   , Color (hsb 0 0 30) (Line [pt env left base, pt env right base])
   , Color (hsb 40 90 100) (Line [pt env left (base - threshold * sh), pt env right (base - threshold * sh)])
   ] ++ bars ++
   [ text' env left (base - threshold * sh - 8) 14 (hsb 40 90 100)
       ("Threshold " ++ printf "%.2f" threshold ++ " (smaller parts are removed)")
   , text' env left (base - sh - 10) 14 (hsb 0 0 60) "Spectrum (green = keep, gray = remove)"
   , drawSeries env restored left right (h * 0.85) (h * 0.1) (40, 90, 100) 3 "Restored signal"
   , drawSeries env (wClean w) left right (h * 0.85) (h * 0.1) (120, 70, 100) 1 ""
   , Color (hsba 220 40 10 0.85) (box env 0 0 ww 58)   -- 波が文字にかからないように帯を敷く
   , text' env 20 28 16 (hsb 0 0 100)
       ("Threshold (left/right)   Noise " ++ printf "%.2f" noiseAmp ++ " (up/down)   Click: new signal")
   , text' env 20 50 16 (hsb 0 0 100)
       ("Kept " ++ show kept ++ " parts   Error from the true signal " ++ printf "%.3f" err)
   ])
  where
    ww = envW env
    h = envH env
    (mx, my) = mouseP env
    threshold = mapC mx 0 ww 0 0.6
    noiseAmp = mapC my 0 h 1.5 0
    noisy = U.zipWith (\v d -> v + d * noiseAmp) (wClean w) (wDice w)
    -- ★ 1. フーリエ変換
    (re, im) = dft noisy
    mag = U.zipWith (\r i -> 2 * sqrt (r * r + i * i) / fromIntegral n) re im
    -- ★ 2. しきい値より小さい成分を 0 にする
    keep = U.map (> threshold) mag
    onlyKept = U.zipWith (\k v -> if k then v else 0) keep
    -- ★ 3. 逆フーリエ変換で信号に戻す
    restored = idft (onlyKept re) (onlyKept im)
    kept = U.length (U.filter id keep)
    err = U.sum (U.zipWith (\a b -> abs (a - b)) restored (wClean w)) / fromIntegral n
    left = 60
    right = ww - 60
    -- スペクトル
    base = h * 0.6
    sh = h * 0.2
    half = n `div` 2
    bars =
      [ Color (if keep U.! k then hsb 120 70 100 else hsb 0 0 50) $
          box env (remap (fromIntegral k) 0 (fromIntegral half) left right) (base - m * sh)
                  ((right - left) / fromIntegral half - 1) (m * sh)
      | k <- [0 .. half - 1], let m = mag U.! k ]

drawSeries :: Env -> U.Vector Float -> Float -> Float -> Float -> Float -> (Float, Float, Float) -> Float -> String -> Picture
drawSeries env arr left right mid amp (hue, sat, bri) weight name = Pictures [curve, title]
  where
    alpha = if null name then 0.7 else 1
    pts = [ pt env (remap (fromIntegral i) 0 (fromIntegral (U.length arr - 1)) left right) (mid - v * amp)
          | (i, v) <- zip [0 :: Int ..] (U.toList arr) ]
    curve = Color (hsba hue sat bri alpha) (if weight <= 1 then Line pts else thickPath weight pts)
    title | null name = Blank
          | otherwise = text' env left (mid - amp - 10) 14 (hsb hue sat bri) name

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> newSignal
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. しきい値を上げていくと、ノイズが消えて 2 本の山だけ残る。上げすぎると信号も消える
-- 2. ノイズを最大にしても復元できる？（山がノイズに埋もれる限界）
-- 3. 「しきい値」ではなく「周波数 30 以上を消す」にすると（ローパス）、どう変わる？

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
