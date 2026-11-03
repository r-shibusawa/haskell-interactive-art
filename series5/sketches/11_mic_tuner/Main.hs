-- ============================================
-- 11 チューナー（声の高さを測る）
-- 音の波形が「どのくらいの周期でくり返しているか」を自己相関で調べ、
-- 周波数 → 音名（ドレミ）に変換する。楽器のチューナーと同じしくみ
-- クリックでマイク ON（マイクが使えないときはデモの信号）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U

data World = World
  { wMic   :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive  :: !Bool               -- 本物のマイクの音が来ているか
  , wFreq  :: !Float              -- なめらかにした周波数
  , wHist  :: ![Float]            -- 高さの履歴（新しいものが先頭）
  , wWave  :: !(U.Vector Float)   -- マイクの波形（最新の 512 点）
  }

sampleRate :: Float
sampleRate = 22050        -- Art.Device のマイクのサンプリング周波数

lagMin, lagMax :: Int
lagMin = floor (sampleRate / 1000)   -- ★ 調べる「ずれ」の範囲（約 1000 Hz まで）
lagMax = floor (sampleRate / 70)     -- ★ 約 70 Hz まで

historyLen :: Int
historyLen = 200

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

main :: IO ()
main = runSketch (sketch "05-11 Mic Tuner" (World Nothing False 0 [] U.empty))
  { skBack  = hsb 230 40 10
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    samples <- micSamples mic           -- 最新の波形
    let wave = U.fromList samples
        f = detectPitch wave
    pure w
      { wLive = live
      , wWave = wave
      , wFreq = if f > 0 then lerpF (wFreq w) f 0.4 else wFreq w
      , wHist = take historyLen (f : wHist w)
      }

-- ★ 自己相関で周期を見つける：波形を少しずつずらして重ね、いちばん似ている「ずれ」が周期
--   周期の 2 倍、3 倍のずれも同じくらい似るので、最大値に近い山のうち最初のものを選ぶ
detectPitch :: U.Vector Float -> Float
detectPitch buf
  | n <= lagMax || rms < 0.01 = -1      -- 点が足りない、または静かすぎる
  | best < 0.001 = -1
  | otherwise = case peaks of
      []    -> -1
      l : _ -> sampleRate / (fromIntegral l + refine l)
  where
    n = U.length buf
    rms = sqrt (U.sum (U.map (\v -> v * v) buf) / fromIntegral n)
    -- ずれ lag で重ねたときの「似ている度合い」
    corr = U.generate (lagMax + 1) $ \lag ->
      U.sum (U.zipWith (*) buf (U.drop lag buf)) / fromIntegral (n - lag)
    c = (corr U.!)
    best = U.maximum (U.slice lagMin (lagMax - lagMin) corr)
    peaks = [ l | l <- [lagMin .. lagMax - 1], c l >= 0.95 * best, c l >= c (l - 1), c l >= c (l + 1) ]
    -- 山の前後 3 点を放物線でつないで、1 サンプルより細かく頂点を求める
    refine l =
      let d = c (l - 1) - 2 * c l + c (l + 1)
      in if abs d < 1e-9 then 0 else clampF (-0.5) 0.5 (0.5 * (c (l - 1) - c (l + 1)) / d)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (body ++ [info] ++ demo)
  where
    ww = envW env
    h = envH env
    on = not (null (wMic w))
    body | on = [waveLine] ++ note ++ history
         | otherwise = []
    -- ---- 波形 ----
    count = max 1 (U.length (wWave w))
    waveLine = Color (hsba 170 60 100 0.6) $ thickPath 2
      [ pt env (remap (fromIntegral i) 0 (fromIntegral count) 0 ww) (h * 0.8 + v * h * 0.15)
      | (i, v) <- zip [0 :: Int ..] (U.toList (wWave w)), even i ]
    -- ---- 音名 ----
    freq = wFreq w
    note | freq > 0 = noteView env freq
         | otherwise = []
    -- 高さの履歴
    history =
      [ Color (hsba 40 80 100 0.6) $
          at (pt env (remap (fromIntegral i) 0 (fromIntegral historyLen) 0 ww)
                     (remap (log f) (log 80) (log 1000) (h * 0.75) (h * 0.1)))
             (disc 2)
      | (i, f) <- zip [0 :: Int ..] (reverse (wHist w)), f > 0 ]
    info = text' env 20 40 18 (hsb 0 0 100) $
      if on then "Sing or play a note: shows its name and Hz (A4 = 440 Hz)"
            else "Click to turn the mic ON"
    demo | on && not (wLive w) = [text' env 20 66 14 (hsb 40 80 100) "No mic: demo signal"]
         | otherwise = []

noteView :: Env -> Float -> [Picture]
noteView env freq =
  [ bold (centered env (ww / 2) (h * 0.42) 140 (hsb 0 0 100) name)
  , centered env (ww / 2) (h * 0.52) 28 (hsb 0 0 100) (show (round freq :: Int) ++ " Hz")
  -- ずれのメーター
  , Color (hsb 0 0 40) (thickLine 2 (pt env (ww * 0.25) (h * 0.6)) (pt env (ww * 0.75) (h * 0.6)))
  , Color (hsb 0 0 40) (thickLine 2 (pt env (ww / 2) (h * 0.58)) (pt env (ww / 2) (h * 0.62)))
  , Color (hsb (if abs cents < 10 then 120 else 40) 80 100) (at (pt env mx (h * 0.6)) (disc 12))
  , centered env mx (h * 0.6 + 40) 16 (hsb 0 0 100) (show (round cents :: Int) ++ " cents")
  ]
  where
    ww = envW env
    h = envH env
    -- ★ 周波数 → 半音の番号（A4 = 440 Hz = 69 番）
    midi = 69 + 12 * logBase 2 (freq / 440)
    nearest = floor (midi + 0.5) :: Int
    cents = (midi - fromIntegral nearest) * 100    -- ずれ（セント：半音の 1/100）
    name = (noteNames !! (nearest `mod` 12)) ++ show (nearest `div` 12 - 1)
    mx = remap cents (-50) 50 (ww * 0.25) (ww * 0.75)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMic w of
    Nothing -> do
      mic <- startMic
      pure w { wMic = Just mic }
    Just _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「あー」と言いながら、ゆっくり高くしていく。音名が C → C# → D と変わる
-- 2. ハミングで A（ラ）をぴったり当てる（メーターが緑になるまで）
-- 3. 低い声（70 Hz 以下）が測れない理由 → lagMax の意味を考えてみよう

-- ---------- 道具 ----------

-- 太い文字（少しずつずらして重ねる）
bold :: Picture -> Picture
bold pic = Pictures [ Translate dx dy pic | dx <- [-2, 0, 2], dy <- [-2, 0, 2] ]

-- p5.js の textAlign(CENTER) の文字：(x, y) は文字の下の中央
centered :: Env -> Float -> Float -> Float -> Color -> String -> Picture
centered env x y size col str = let (gx, gy) = pt env x y in labelC gx (gy + size / 2) size col str

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標（中央が原点、y は上向き）に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str
