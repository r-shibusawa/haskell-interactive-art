-- ============================================
-- 06 クロマ（12 音のどれが鳴っているか）
-- スペクトルを「オクターブちがいは同じ音」として 12 個に折りたたむ ＝ クロマグラム
-- 歌う・楽器を鳴らすと、12 音の円のどこが光るかで、鳴っている音や和音がわかる
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U

data World = World
  { wMic    :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool               -- 本物のマイクの音が来ているか
  , wSpec   :: !(U.Vector Float)   -- なめらかにしたスペクトル
  , wChroma :: !(U.Vector Float)   -- 12 音の強さ（0〜1）
  , wHist   :: ![U.Vector Float]   -- クロマの履歴（新しいものが先頭、最大 120）
  }

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

sampleRate, specBins :: Float
sampleRate = 22050         -- マイクのサンプリング周波数
specBins = 256             -- micSpectrum の個数

smoothing :: Float
smoothing = 0.6            -- ★ スペクトルをなめらかにする割合

main :: IO ()
main = runSketch (sketch "07-06 Chroma" (World Nothing False (U.replicate 256 0) (U.replicate 12 0) []))
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
    let spec = smoothSpec smoothing (wSpec w) raw
        c = chromaOf (U.map toByte spec)
        m = U.maximum c
        scaleBy = if m > 0 then m else 1
        chroma = U.zipWith (\old v -> lerpF old (v / scaleBy) 0.3) (wChroma w) c
    pure w { wLive = live, wSpec = spec, wChroma = chroma, wHist = take 120 (chroma : wHist w) }

-- ★ 各周波数ビンを、いちばん近い音名（12 で割った余り）に加える
chromaOf :: U.Vector Float -> U.Vector Float
chromaOf spec = U.accum (+) (U.replicate 12 0)
  [ (pc, v * v)
  | (i, v) <- drop 2 (zip [0 :: Int ..] (U.toList spec))
  , let f = fromIntegral i * binHz
  , f >= 80 && f <= 4000
  , let midi = 69 + 12 * logBase 2 (f / 440)
        pc = (round midi :: Int) `mod` 12 ]
  where binHz = sampleRate / 2 / specBins

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (circles ++ [bestNote] ++ gram ++ gramLabels ++ hud)
  where
    wd = envW env
    ht = envH env
    -- ---- 円 ----
    cx = wd * 0.33
    cy = ht * 0.55
    r = min wd ht * 0.3
    chroma = wChroma w
    circles = concat
      [ [ Color (hsb (fromIntegral (i * 30)) 70 (30 + v * 70)) (at (toG env (x, y)) (disc ((40 + v * 50) / 2)))
        , txtM env x y 18 (hsb 0 0 (if v > 0.5 then 10 else 90)) name
        ]
      | (i, name) <- zip [0 :: Int ..] noteNames
      , let a = -pi / 2 + tau * fromIntegral i / 12
            v = chroma U.! i
            x = cx + cos a * r
            y = cy + sin a * r ]
    -- いちばん強い音
    bestNote =
      let best = U.maxIndex chroma
      in case wMic w of
           Just _ | chroma U.! best > 0.2 -> txtM env cx cy 72 white (noteNames !! best)
           _ -> Blank
    -- ---- クロマグラム（時間 × 12 音）----
    gx = wd * 0.62
    gw = wd - gx - 60
    gy = ht * 0.2
    gh = ht * 0.6
    gram =
      [ Color (hsb (fromIntegral (i * 30)) 70 (v * 100))
          (box env (gx + t / 120 * gw) (gy + gh - fromIntegral (i + 1) * (gh / 12)) (gw / 120 + 1) (gh / 12))
      | (t, frame) <- zip [0 ..] (reverse (wHist w))
      , i <- [0 .. 11 :: Int]
      , let v = frame U.! i ]
    gramLabels =
      txt env gx (gy - 12) 14 (hsb 0 0 60) "Chromagram (x: time, y: 12 notes)"
      : [ txt env (gx - 28) (gy + gh - fromIntegral i * (gh / 12) - 8) 14 (hsb 0 0 60) name
        | (i, name) <- zip [0 :: Int ..] noteNames ]
    hud =
      [ txt env 20 40 18 white (case wMic w of
          Nothing -> "Click to turn the mic ON"
          Just _  -> "Sing or play an instrument: the sounding notes light up. A chord lights several")
      , txt env 20 68 15 (hsb 0 0 60) "C3, C4, C5 all count as C (octaves are folded). The basis of chord recognition and key finding (-> 13)"
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
-- 1. 「ド」を歌う → C が光る。倍音のせいで G や E も少し光る（なぜ？）
-- 2. ピアノやギターで和音を弾く → 3 つ光る。長三和音と短三和音の形の違い
-- 3. 好きな曲を流して、クロマグラムの縞の変化（コード進行）を見る

-- ---------- 道具 ----------

-- 前のフレームと混ぜて、なめらかにする（p5.js の smoothingTimeConstant）
smoothSpec :: Float -> U.Vector Float -> [Float] -> U.Vector Float
smoothSpec k old raw = U.zipWith (\o v -> k * o + (1 - k) * v) old (U.fromListN 256 (raw ++ repeat 0))

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

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
