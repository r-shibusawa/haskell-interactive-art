-- ============================================
-- 07 音の「明るさ」を測る（スペクトル重心）
-- スペクトルの「重心」が高いほど明るい音、低いほど暗い音。
-- 「あ」と「う」、シンバルとドラム、笛と太鼓。音色を数字 1 つで表す特徴量
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data World = World
  { wMic      :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive     :: !Bool               -- 本物のマイクの音が来ているか
  , wRaw      :: !(U.Vector Float)   -- なめらかにしたスペクトル（0〜1）
  , wCentroid :: !Float              -- スペクトル重心（Hz）
  , wFlatness :: !Float              -- 平坦さ（雑音っぽさ 0〜1）
  }

sampleRate, specBins :: Float
sampleRate = 22050         -- マイクのサンプリング周波数
specBins = 256             -- micSpectrum の個数

binHz :: Float
binHz = sampleRate / 2 / specBins

smoothing :: Float
smoothing = 0.6            -- ★ スペクトルをなめらかにする割合

main :: IO ()
main = runSketch (sketch "07-07 Brightness" (World Nothing False (U.replicate 256 0) 0 0))
  { skBack  = hsb 260 40 10
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: World -> IO World
stepWorld w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    raw <- micSpectrum mic
    let spec = U.zipWith (\o v -> smoothing * o + (1 - smoothing) * v) (wRaw w) (U.fromListN 256 (raw ++ repeat 0))
        ms = [ (fromIntegral i * binHz, toByte v / 255) | (i, v) <- drop 1 (zip [0 :: Int ..] (U.toList spec)) ]
        -- ★ スペクトル重心 = Σ(周波数 × 強さ) ÷ Σ(強さ)
        num = sum [f * m | (f, m) <- ms]
        den = sum (map snd ms)
        low = [m + 1e-4 | (f, m) <- ms, f < 8000]
        cnt = fromIntegral (length low)
        logSum = sum (map log low)
        lin = sum low
        -- ★ 平坦さ = 幾何平均 ÷ 算術平均（1 に近いほどノイズ、0 に近いほど「音程がある」）
        flat = exp (logSum / cnt) / (lin / cnt)
        w1 = w { wLive = live, wRaw = spec }
    pure (if den > 0.5     -- ビンの数が p5.js の 1/4 なので、しきい値も 2 の 1/4
            then w1 { wCentroid = lerpF (wCentroid w) (num / den) 0.2, wFlatness = lerpF (wFlatness w) flat 0.2 }
            else w1)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (spectrum ++ meter ++ hud)
  where
    wd = envW env
    ht = envH env
    centroid = wCentroid w
    flatness = wFlatness w
    -- スペクトルと重心
    base = ht * 0.8
    sh = ht * 0.3
    xOfF f = remap f 0 8000 60 (wd - 60)
    spectrum = case wMic w of
      Nothing -> []
      Just _ ->
        let cxp = xOfF centroid
            pts = [ toG env (xOfF f, base - toByte v / 255 * sh)
                  | (i, v) <- drop 1 (zip [0 :: Int ..] (U.toList (wRaw w)))
                  , let f = fromIntegral i * binHz
                  , f <= 8000 ]
        in [ Color (hsb 170 70 100) (thickPath 2 pts)
           , Color (hsb 45 90 100) (thickLine 4 (toG env (cxp, base)) (toG env (cxp, base - sh)))
           , txt env (min (wd - 190) (cxp + 8)) (base - sh + 20) 16 (hsb 45 90 100)
               ("Centroid " ++ show (round centroid :: Int) ++ " Hz")
           ]
    -- メーター
    level = clampF 0 1 (remap centroid 300 4000 0 1)
    my = ht * 0.2
    kind | flatness > 0.4 = "noise, drums, or a long 's'"
         | flatness > 0.15 = "a mixed sound"
         | otherwise = "pitched sound or vowel"
    meter =
      [ Color (hsb (remap level 0 1 230 50) 70 (40 + level * 60)) (box env 60 my ((wd - 120) * level) 40)
      , txt env 60 (my - 12) 20 white "dark  <-  brightness  ->  bright"
      , txt env 60 (my + 70) 16 (hsb 0 0 60) ("Noise-like (flatness) " ++ printf "%.2f" flatness ++ "     " ++ kind)
      ]
    hud =
      [ txt env 20 40 18 white (case wMic w of
          Nothing -> "Click to turn the mic ON"
          Just _  -> "Say 'ah', 'ee', 'oo', whistle, 'sss', clap. How do brightness and noisiness change?")
      , txt env 20 68 15 (hsb 0 0 60) "A feature that turns timbre into a number. Used for music tagging, instrument ID and similarity."
      , case wMic w of
          Just _ | not (wLive w) -> txt env 20 94 15 (hsb 0 0 70) "No mic found: demo signal"
          _ -> Blank
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「う」と「い」で重心を比べる（「い」は 2000 Hz 以上に成分がある）
-- 2. 「す〜」は雑音っぽさがほぼ 1。「あ〜」は 0 に近い
-- 3. 明るさ・雑音っぽさの 2 つで、音を 2 次元に並べる（12 の自由制作で）

-- ---------- 道具 ----------

-- 強さ（0〜1）を、p5.js の getByteFrequencyData と同じ -100〜-30 dB の目盛りで 0〜255 にする
toByte :: Float -> Float
toByte v =
  let magnitude = v * 40 / 512
      db = 20 * logBase 10 (max 1e-6 magnitude)
  in 255 * clampF 0 1 ((db + 100) / 70)

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str
