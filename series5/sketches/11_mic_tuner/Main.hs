-- ============================================
-- 11 チューナー（声の高さを測る）
-- 音の波形が「どのくらいの周期でくり返しているか」を自己相関で調べ、
-- 周波数 → 音名（ドレミ）に変換する。楽器のチューナーと同じしくみ
-- クリックでマイク ON（マイクが使えないときはデモの信号）
-- ============================================
-- Art.Device から受け取れるのはスペクトルなので、自己相関はスペクトルから計算する
-- （ウィーナー＝ヒンチンの定理：パワースペクトルを逆フーリエ変換すると自己相関になる）。
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U

data World = World
  { wMic   :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive  :: !Bool               -- 本物のマイクの音が来ているか
  , wFreq  :: !Float              -- なめらかにした周波数
  , wHist  :: ![Float]            -- 高さの履歴（新しいものが先頭）
  , wCorr  :: !(U.Vector Float)   -- 自己相関（ずれ 0〜255）
  }

sampleRate :: Float
sampleRate = 22050        -- Art.Device のマイクのサンプリング周波数

fftN :: Int
fftN = 512                -- Art.Device のフーリエ変換の点の数

lagMin, lagMax :: Int
lagMin = 22               -- ★ 調べる「ずれ」の範囲（22 = 約 1000 Hz）
lagMax = 254              -- ★ 254 = 約 87 Hz

historyLen :: Int
historyLen = 200

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

main :: IO ()
main = runSketch (sketch "05-11 Mic Tuner" (World Nothing False 0 [] (U.replicate 256 0)))
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
    level <- micLevel mic
    spectrum <- micSpectrum mic
    let corr = autocorrelation (U.fromListN 256 (spectrum ++ repeat 0))
        f = if level < 0.01 then -1 else detectPitch corr   -- 静かすぎるときは -1
    pure w
      { wLive = live
      , wCorr = corr
      , wFreq = if f > 0 then lerpF (wFreq w) f 0.4 else wFreq w
      , wHist = take historyLen (f : wHist w)
      }

-- ★ スペクトルから自己相関を計算する：波形を lag だけずらして重ねたときの「似ている度合い」
autocorrelation :: U.Vector Float -> U.Vector Float
autocorrelation spec = U.generate 256 $ \lag ->
  let term k = power U.! k * cosTable U.! ((k * lag) `mod` fftN)
      r = 2 * U.sum (U.generate 255 (term . (+ 1))) / fromIntegral fftN
      -- 窓関数（ハン窓）をかけた分の目減りを元に戻す
      windowed = fromIntegral fftN * (0.25 + 0.125 * cosTable U.! lag)
  in r / windowed
  where
    power = U.map (\m -> (m * 40) * (m * 40)) spec    -- 40 は Art.Device の正規化の値

cosTable :: U.Vector Float
cosTable = U.generate fftN (\i -> cos (tau * fromIntegral i / fromIntegral fftN))

-- ★ 自己相関で周期を見つける：いちばん似ている「ずれ」が周期
--   周期の 2 倍のずれも同じくらい似るので、最大値に近い山のうち最初のものを選ぶ
detectPitch :: U.Vector Float -> Float
detectPitch corr
  | best < 0.001 = -1
  | otherwise = case peaks of
      []    -> -1
      l : _ -> sampleRate / (fromIntegral l + refine l)
  where
    c = (corr U.!)
    best = U.maximum (U.slice lagMin (lagMax - lagMin + 1) corr)
    peaks = [ l | l <- [lagMin .. lagMax], c l >= 0.95 * best, c l >= c (l - 1), c l >= c (l + 1) ]
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
    body | on = [corrLine, corrLabel] ++ note ++ history
         | otherwise = []
    -- ---- 自己相関（ずれごとの似ている度合い）----
    peak = max 1e-6 (U.maximum (U.map abs (U.slice lagMin (lagMax - lagMin + 1) (wCorr w))))
    corrLine = Color (hsba 170 60 100 0.6) $ thickPath 2
      [ pt env (remap (fromIntegral l) (fromIntegral lagMin) (fromIntegral lagMax) 0 ww)
               (h * 0.85 - (wCorr w U.! l) / peak * h * 0.1)
      | l <- [lagMin .. lagMax] ]
    corrLabel = text' env 20 (h - 16) 14 (hsb 170 60 100) "Autocorrelation (shift: small to large)"
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
-- 3. 低い声（87 Hz 以下）が測れない理由 → lagMax の意味を考えてみよう

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
