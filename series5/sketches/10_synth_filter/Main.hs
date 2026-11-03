-- ============================================
-- 10 ローパスフィルタで音を削る（シンセサイザーの基本）
-- 倍音たっぷりののこぎり波を、ローパスフィルタ（高い周波数を通さない）で削ると、
-- こもった音 ⇄ 明るい音 が自由に作れる。レゾナンスを上げると「ミョーン」
-- マウス左右 = カットオフ周波数 / 上下 = レゾナンス / クリックで音 ON/OFF / 1〜3 で波形
-- ============================================
-- 音は倍音ごとにフィルタの通り具合をかけて計算し、大きい成分を短い音にして鳴らす。
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (sortOn)
import Text.Printf (printf)

data Shape = Saw | Sq | Tri deriving Eq

data World = World
  { wPlaying :: !Bool
  , wShape   :: !Shape
  , wTimer   :: !Float    -- 次の音までの時間 [秒]
  }

baseFreq :: Float
baseFreq = 110            -- ★ 元の音の高さ [Hz]

noteLen, noteGap :: Float
noteLen = 0.3             -- ★ 1 音の長さ [秒]
noteGap = 0.3             -- ★ 音を鳴らす間隔 [秒]

gain :: Float
gain = 0.2                -- ★ 全体の音量

main :: IO ()
main = runSketch (sketch "05-10 Synth Filter" (World False Saw 0))
  { skBack  = hsb 230 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 対数で動かす（耳は対数で聞いている）
cutoffOf :: Env -> Float
cutoffOf env = 10 ** mapC (fst (mouseP env)) 0 (envW env) (logBase 10 80) (logBase 10 12000)

resonanceOf :: Env -> Float
resonanceOf env = mapC (snd (mouseP env)) 0 (envH env) 25 0.5

-- ★ 2 次ローパスの大きさ（だいたいの形）
response :: Float -> Float -> Float -> Float
response cutoff q f =
  let r = f / cutoff
  in 1 / sqrt ((1 - r * r) ^ (2 :: Int) + (r / q) ^ (2 :: Int))

-- 波形の k 倍音の大きさ（フーリエ級数）
harmonicAmp :: Shape -> Int -> Float
harmonicAmp shape k =
  let fk = fromIntegral k :: Float
  in case shape of
       Saw -> 2 / (pi * fk)                                  -- 全部の倍音、1/n
       Sq  -> if odd k then 4 / (pi * fk) else 0             -- 奇数倍、1/n
       Tri -> if odd k then 8 / (pi * pi * fk * fk) else 0   -- 奇数倍、1/n²（倍音が少ない）

-- フィルタを通ったあとの（周波数, 大きさ）。f の上限まで
filteredPartials :: Env -> Shape -> Float -> [(Float, Float)]
filteredPartials env shape fMax =
  [ (f, gain * harmonicAmp shape k * response (cutoffOf env) (resonanceOf env) f)
  | k <- [1 .. floor (fMax / baseFreq)]
  , let f = baseFreq * fromIntegral k ]

step :: Env -> Float -> World -> IO World
step env dt w = do
  let timer = wTimer w - dt
      sound = wPlaying w && timer <= 0
  when sound $ do
    -- 大きい成分を 4 つまで同時に鳴らす（合計が大きくなりすぎないように調整）
    let strongest = take 4 (sortOn (negate . snd) (filteredPartials env (wShape w) 10000))
        total = sum (map snd strongest)
        k = if total > 0.8 then 0.8 / total else 1
    forM_ strongest $ \(f, a) -> when (a > 0.003) (playTone f (a * k) noteLen Sine)
  pure w { wTimer = if sound then noteGap else max 0 timer }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([axis, curve] ++ scale_ ++ [spectrum, info1, info2])
  where
    ww = envW env
    h = envH env
    cutoff = cutoffOf env
    res = resonanceOf env
    left = 60
    right = ww - 60
    base = h * 0.55
    amp = h * 0.35
    logX f = remap (logBase 10 f) (logBase 10 20) (logBase 10 20000) left right
    -- ---- フィルタの通り具合（周波数応答）を描く ----
    axis = Color (hsb 0 0 30) (Line [pt env left base, pt env right base])
    curve = Color (hsb 40 90 100) $ thickPath 3
      [ pt env x (base - mapC db (-60) 30 0 amp)
      | x <- [left, left + 2 .. right]
      , let f = 10 ** remap x left right (logBase 10 20) (logBase 10 20000)
            db = 20 * logBase 10 (response cutoff res f) ]
    -- 目盛
    scale_ =
      [ text' env (logX f) (base + 20) 14 (hsb 0 0 60) (show (round f :: Int) ++ "Hz") | f <- [20, 100, 1000, 10000] ]
      ++ [ text' env (left - 50) (base - amp * 0.7) 14 (hsb 0 0 60) "pass"
         , text' env (left - 50) (base - 6) 14 (hsb 0 0 60) "cut" ]
    -- ---- 出力のスペクトル（倍音ごとに計算した大きさを棒で描く）----
    spectrum
      | wPlaying w = Color (hsba 170 80 100 0.8) $ Pictures
          [ thickLine 2 (pt env (logX f) (h - 40)) (pt env (logX f) (h - 40 - toLevel a * h * 0.22))
          | (f, a) <- filteredPartials env (wShape w) 20000, a > 0 ]
      | otherwise = Blank
    info1 = text' env 20 40 18 (hsb 0 0 100)
      ("Cutoff " ++ show (round cutoff :: Int) ++ " Hz (left/right)   Resonance Q = " ++ printf "%.1f" res
       ++ " (up/down)   Click: sound " ++ (if wPlaying w then "ON" else "OFF"))
    info2 = text' env 20 68 15 (hsb 0 0 60)
      ("Wave: " ++ shapeName (wShape w) ++ " (1-3)   Yellow = what the filter lets through   Cyan = output spectrum")

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPlaying = not (wPlaying w), wTimer = 0 }
  EventKey (Char '1') Down _ _ -> pure w { wShape = Saw }
  EventKey (Char '2') Down _ _ -> pure w { wShape = Sq }
  EventKey (Char '3') Down _ _ -> pure w { wShape = Tri }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. レゾナンスを最大にしてカットオフを左右に動かす → シンセの「ワウ」
-- 2. 三角波（倍音が少ない）だと、フィルタの効果が小さい。なぜ？
-- 3. response の式を変えて、高い音だけ通す「ハイパス」にすると？（r を 1 / r に）

-- ---------- 道具 ----------

shapeName :: Shape -> String
shapeName shape = case shape of
  Saw -> "sawtooth"
  Sq  -> "square"
  Tri -> "triangle"

-- 大きさを 0〜1 の棒の高さに直す（p5.js の analyser の -100〜-30 dB と同じ範囲）
toLevel :: Float -> Float
toLevel a =
  let db = 20 * logBase 10 (max 1e-6 (a * 0.21))
  in clampF 0 1 ((db + 100) / 70)

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
