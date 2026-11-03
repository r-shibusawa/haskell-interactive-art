-- ============================================
-- 10 プッシュホンの音（DTMF）
-- 電話のボタンを押すと「ピ・ポ・パ」。実は 2 つの周波数の和。
-- 行と列で周波数が決まっていて、受信側はスペクトルの 2 つの山で「どのボタンか」を知る
-- 画面のボタンをクリック（音が鳴る＋スペクトル表示）
-- ============================================
-- スペクトルは、鳴らしている音と同じ 2 つの正弦波をプログラムの中で作り、
-- 窓をかけて周波数ごとの強さを計算したもの（ブラウザの AnalyserNode と同じ手順）。
module Main (main) where

import Art
import Control.Monad (when)
import qualified Data.Vector.Unboxed as U

data Tone = Tone !Float !Float !Float   -- 周波数 2 つと、鳴らし始めた時刻

data World = World
  { wPressed   :: !(Maybe (Int, Int))    -- 押したボタン（行、列）
  , wPressTime :: !Float
  , wDialed    :: !String
  , wTones     :: ![Tone]                -- いま鳴っている音
  , wSpec      :: !(Maybe (U.Vector Float))   -- なめらかにした各周波数の強さ（最初のクリックまで無し）
  }

rowsF, colsF :: [Float]
rowsF = [697, 770, 852, 941]
colsF = [1209, 1336, 1477, 1633]

labels :: [String]
labels = ["123A", "456B", "789C", "*0#D"]

sampleRate, smoothing, toneLen :: Float
sampleRate = 8000          -- 2000 Hz まで見るには十分
smoothing = 0.8            -- ★ スペクトルのなめらかさ（AnalyserNode の標準値）
toneLen = 0.4              -- 音の長さ（秒）

frameN, binCount :: Int
frameN = 682               -- 約 85 ms ぶんのサンプル
binCount = 171             -- 0〜2000 Hz の周波数の数

main :: IO ()
main = runSketch (sketch "06-10 DTMF Tones" (World Nothing 0 "" [] Nothing))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 音の大きさ：0.2 秒たったら素早く小さくなる
envelope :: Float -> Float
envelope a
  | a < 0 || a >= toneLen = 0
  | a < 0.2 = 0.15
  | otherwise = 0.15 * exp (negate (a - 0.2) / 0.03)

-- いま鳴っている音を、最近の frameN 個のサンプルにする（窓をかける）
signalFrame :: Float -> [Tone] -> U.Vector Float
signalFrame now tones = U.generate frameN sample
  where
    n = fromIntegral (frameN - 1) :: Float
    sample j =
      let fj = fromIntegral j :: Float
          back = (n - fj) / sampleRate
          value = sum [ envelope a * (sin (tau * f1 * a) + sin (tau * f2 * a))
                      | Tone f1 f2 t0 <- tones, let a = (now - t0) - back ]
          blackman = 0.42 - 0.5 * cos (tau * fj / n) + 0.08 * cos (2 * tau * fj / n)
      in value * blackman

-- ★ k 番目の周波数の強さ（ゲルツェル法：1 つの周波数だけの離散フーリエ変換）
binMagnitude :: U.Vector Float -> Int -> Float
binMagnitude x k =
  let nf' = fromIntegral (U.length x) :: Float
      c = 2 * cos (tau * fromIntegral k / nf')
      (s1, s2) = U.foldl' (\(!a, !b) v -> (v + c * a - b, a)) (0, 0) x
  in sqrt (max 0 (s1 * s1 + s2 * s2 - c * s1 * s2)) / nf'

step :: Env -> World -> World
step env w = case wSpec w of
  Nothing -> w
  Just old ->
    let now = envTime env
        alive = [t | t@(Tone _ _ t0) <- wTones w, now - t0 < toneLen + fromIntegral frameN / sampleRate]
        mags | null alive = U.replicate binCount 0
             | otherwise = let x = signalFrame now alive in U.generate binCount (binMagnitude x)
        new = U.zipWith (\p m -> smoothing * p + (1 - smoothing) * m) old mags
    in w { wTones = alive, wSpec = Just new }

-- 強さをデシベルにして 0〜1 にする（-100 dB〜-30 dB）
toLevel :: Float -> Float
toLevel m
  | m <= 1e-6 = 0
  | otherwise = clampF 0 1 ((20 * logBase 10 m + 100) / 70)

-- ボタンの大きさ
buttonSize :: Env -> Float
buttonSize env = min 90 ((envH env - 260) / 4)

bx, by :: Float
bx = 80
by = 120

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Pictures [ button r c ch | (r, line') <- zip [0 ..] labels, (c, ch) <- zip [0 ..] line' ]
  , Pictures [ txt env (bx - 70) (by + fromIntegral r * (bs + 10) + bs / 2 + 5) 14 dim (show (round f :: Int) ++ " Hz")
             | (r, f) <- zip [0 :: Int ..] rowsF ]
  , Pictures [ txtC env (bx + fromIntegral c * (bs + 10) + bs / 2) (by - 16) 14 dim (show (round f :: Int))
             | (c, f) <- zip [0 :: Int ..] colsF ]
    -- ---- スペクトル ----
  , Color (hsb 0 0 30) (Line [toG env sl base, toG env sr base])
  , Pictures (zipWith freqMark [0 :: Int ..] (rowsF ++ colsF))
  , case wSpec w of
      Nothing -> Blank
      Just sp -> Color (hsb 170 80 100) (thickPath 2
        [ toG env (remap f 0 2000 sl sr) (base - toLevel m * sh)
        | (k, m) <- zip [0 :: Int ..] (U.toList sp)
        , let f = fromIntegral k * sampleRate / fromIntegral frameN ])
  , txt env sl (base - sh - 14) 14 dim "Spectrum (0-2000 Hz)"
  , txt env 20 40 18 (hsb 0 0 100) ("Click a button: 2 tones (row + column) sound together.  Dialed: " ++ wDialed w)
  , txt env 20 68 15 dim "Example: 5 = 770 Hz + 1336 Hz. The receiver finds the 2 peaks to identify the button."
  ]
  where
    dim = hsb 0 0 60
    bs = buttonSize env
    sl = bx + 4 * (bs + 10) + 60
    sr = envW env - 60
    base = envH env - 100
    sh = envH env * 0.5
    button :: Int -> Int -> Char -> Picture
    button r c ch =
      let on = wPressed w == Just (r, c) && envTime env - wPressTime w < 0.3
          x = bx + fromIntegral c * (bs + 10)
          y = by + fromIntegral r * (bs + 10)
      in Pictures
           [ roundBox env x y bs bs 12 (if on then hsb 45 90 100 else hsb 230 30 30)
           , txtC env (x + bs / 2) (y + bs / 2) 28 (hsb 0 0 (if on then 10 else 90)) [ch] ]
    -- 周波数の目盛り。文字が重ならないように 2 段に分ける
    freqMark i f =
      let x = remap f 0 2000 sl sr
      in Pictures
           [ Color (hsb 0 0 25) (Line [toG env x base, toG env x (base - sh)])
           , txtC env x (base + (if even i then 16 else 36)) 14 dim (show (round f :: Int)) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    let bs = buttonSize env
        (mx, my) = mouseXY env
        c = floor ((mx - bx) / (bs + 10)) :: Int
        r = floor ((my - by) / (bs + 10)) :: Int
        started = w { wSpec = Just (maybe (U.replicate binCount 0) id (wSpec w)) }
    case (lookupAt r rowsF, lookupAt c colsF, lookupAt r labels >>= lookupAt c) of
      (Just f1, Just f2, Just ch) -> do
        playChord [f1, f2] 0.15 0.3 Sine
        pure started
          { wPressed = Just (r, c)
          , wPressTime = envTime env
          , wDialed = lastN 16 (wDialed w ++ [ch])
          , wTones = Tone f1 f2 (envTime env) : wTones w
          }
      _ -> pure started
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 電話機（スマホの通話画面）のボタンの音と聞き比べよう。同じ音！
-- 2. 「1」と「2」は行が同じ → 低い方の山は同じ位置、高い方だけ動く
-- 3. なぜ 1 つの周波数ではなく 2 つ使う？（声や雑音で誤って反応しないため）

-- ---------- 道具 ----------

-- i 番目を取り出す。範囲外なら Nothing
lookupAt :: Int -> [a] -> Maybe a
lookupAt i xs
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

-- 後ろの n 個
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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
