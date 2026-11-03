-- ============================================
-- 09 歌を楽譜にする（自動採譜）
-- 声の高さを測り（自己相関）、半音に丸めて「音符」にする
-- 録音 → 再生 で、自分の歌がピコピコ音で返ってくる
-- クリックでマイク ON → スペースで録音開始・停止 → P で再生 / C で消す
-- ============================================
-- Art.Device から受け取れるのはスペクトルだけなので、自己相関は
-- 「パワースペクトルをフーリエ変換すると自己相関になる」という関係（ウィーナー・ヒンチンの定理）で計算する。
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (foldl', partition)
import qualified Data.Vector.Unboxed as U

data Note = Note { nMidi :: !Int, nStart :: !Int, nLen :: !Int }

-- 予約した音：鳴らす時刻（秒）、周波数、長さ（秒）
data Booked = Booked !Float !Float !Float

data World = World
  { wMic       :: !(Maybe Mic)     -- まだ ON にしていなければ Nothing
  , wLive      :: !Bool            -- 本物のマイクの音が来ているか
  , wContour   :: ![Maybe Float]   -- 測った高さ（MIDI 番号、小数。新しいものが先頭）
  , wCount     :: !Int             -- wContour の長さ
  , wNotes     :: ![Note]          -- 丸めた音符
  , wRecording :: !Bool
  , wPlayBack  :: !Bool
  , wPlayStart :: !Float           -- 再生を始めた時刻（ミリ秒）
  , wBooked    :: ![Booked]        -- これから鳴らす音
  }

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

maxFrames, minNoteFrames :: Int
maxFrames = 600            -- 録音できるフレーム数
minNoteFrames = 4          -- ★ これより短い音符は捨てる

msPerFrame :: Float
msPerFrame = 33            -- 再生するときの 1 フレームの長さ（ミリ秒）

main :: IO ()
main = runSketch (sketch "07-09 Pitch Contour" (World Nothing False [] 0 [] False False 0 []))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  w1 <- case wMic w0 of
    Nothing -> pure w0
    Just mic -> do
      live <- micLive mic
      let wl = w0 { wLive = live }
      if not (wRecording wl) then pure wl else do
        level <- micLevel mic
        spec <- micSpectrum mic
        let f = detectPitch level spec
            m = if f > 0 then Just (69 + 12 * logBase 2 (f / 440)) else Nothing
            wr = wl { wContour = m : wContour wl, wCount = wCount wl + 1 }
        pure (if wCount wr > maxFrames then stopRecording wr else wr)
  let now = envTime env
      (due, later) = partition (\(Booked t _ _) -> t <= now) (wBooked w1)
  w2 <- foldM (\wa (Booked _ f d) -> wa <$ playTone f 0.3 d Square) w1 { wBooked = later } due
  pure (if wPlayBack w2 && now * 1000 - wPlayStart w2 > fromIntegral maxFrames * msPerFrame
          then w2 { wPlayBack = False } else w2)

stopRecording :: World -> World
stopRecording w = w { wRecording = False, wNotes = transcribe (reverse (wContour w)) }

-- ★ 採譜：半音に丸めて、同じ音が続く区間をまとめる（短すぎるものは捨てる）
transcribe :: [Maybe Float] -> [Note]
transcribe contour =
  let (done, cur) = foldl' go ([], Nothing) (zip [0 ..] contour)
  in reverse (keep cur done)
  where
    keep (Just n) acc | nLen n >= minNoteFrames = n : acc
    keep _ acc = acc
    go (acc, cur) (i, m) =
      let rounded = fmap (\v -> floor (v + 0.5) :: Int) m
      in case (cur, rounded) of
           (Just n, Just r) | r == nMidi n -> (acc, Just n { nLen = nLen n + 1 })
           (_, Nothing) -> (keep cur acc, Nothing)
           (_, Just r)  -> (keep cur acc, Just (Note r i 1))

-- 測る窓の長さ（micSpectrum のもとになるサンプル数）
windowN :: Int
windowN = 512

sampleRate :: Float
sampleRate = 22050

cosTable :: U.Vector Float
cosTable = U.generate windowN (\i -> cos (tau * fromIntegral i / fromIntegral windowN))

-- ★ 自己相関で高さを測る：いちばんよく重なるずらし幅（lag）が周期。見つからなければ -1
detectPitch :: Float -> [Float] -> Float
detectPitch level spec
  | level < 0.01 = -1
  | bestCorr < 0.001 = -1
  | otherwise = sampleRate / fromIntegral best
  where
    power = U.fromListN 256 (map (\s -> (s * 40) * (s * 40)) spec ++ repeat 0)
    corrAt :: Int -> Float
    corrAt lag =
      let s = U.ifoldl' (\acc k p -> acc + p * cosTable U.! ((k * lag) `mod` windowN)) 0 power
          n = fromIntegral windowN
          -- 窓をかけた分の補正（ハン窓の自己相関で割る）
      in 2 / n * s / (n * (0.25 + 0.125 * cosTable U.! lag))
    lagMin = floor (sampleRate / 1000) :: Int     -- 1000 Hz
    lagMax = floor (sampleRate / 110) :: Int      -- 110 Hz（窓が短いのでこれより低い音は測らない）
    (best, bestCorr) = foldl'
      (\b@(_, bc) lag -> let c = corrAt lag in if c > bc then (lag, c) else b)
      (-1 :: Int, 0 :: Float) [lagMin .. lagMax - 1]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (staff ++ points ++ noteBoxes ++ hud)
  where
    left = 60
    right = envW env - 60
    top = 110
    bottom = envH env - 80
    yOf m = remap m 48 84 bottom top       -- C3〜C6
    xOf :: Int -> Float
    xOf i = remap (fromIntegral i) 0 600 left right
    now = envTime env * 1000
    -- 五線風の横線（C の位置）
    staff = concat
      [ Color (hsb 0 0 (if isC then 40 else 18)) (Line [toG env (left, y), toG env (right, y)])
        : [ txt env (left - 34) (y + 5) 14 (hsb 0 0 60) ('C' : show (m `div` 12 - 1)) | isC ]
      | m <- [48 .. 84 :: Int]
      , let y = yOf (fromIntegral m)
            isC = m `mod` 12 == 0 ]
    -- 測った高さ（点）
    points =
      [ Color (hsba 170 60 100 0.6) (at (toG env (xOf i, yOf v)) (disc 2))
      | (i, Just v) <- zip [wCount w - 1, wCount w - 2 ..] (wContour w) ]
    -- 音符（四角）
    noteBoxes = concat
      [ Color (hsb 45 90 (if active then 100 else 70)) (box env x1 (y - 8) (max (x2 - x1) 4) 16)
        : [ txt env (x1 + 3) (y + 5) 14 (hsb 0 0 10) (noteNames !! (nMidi n `mod` 12)) | nLen n > 12 ]
      | n <- wNotes w
      , let x1 = xOf (nStart n)
            x2 = xOf (nStart n + nLen n)
            y = yOf (fromIntegral (nMidi n))
            t = now - wPlayStart w
            active = wPlayBack w && t > fromIntegral (nStart n) * msPerFrame
                                 && t < fromIntegral (nStart n + nLen n) * msPerFrame ]
    status = case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _
        | wRecording w -> "* Recording... sing! (Space: stop)"
        | not (null (wNotes w)) -> "P: play   Space: record again   C: clear   Notes: " ++ show (length (wNotes w))
        | otherwise -> "Space: start recording"
    hud =
      [ txt env 20 40 18 white status
      , txt env 20 68 15 (hsb 0 0 60) "Cyan dots = measured pitch. Yellow = rounded to semitones; a run of the same pitch becomes one note."
      , case wMic w of
          Just _ | not (wLive w) -> txt env 20 94 15 (hsb 0 0 70) "No mic found: demo signal"
          _ -> Blank
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  (EventKey (SpecialKey KeySpace) Down _ _, Just _)
    | wRecording w -> pure (stopRecording w)
    | otherwise -> pure w { wContour = [], wCount = 0, wNotes = [], wRecording = True }
  (EventKey (Char c) Down _ _, Just _)
    | (c == 'p' || c == 'P') && not (null (wNotes w)) ->
        pure w { wPlayBack = True, wPlayStart = envTime env * 1000
               , wBooked = wBooked w ++ map (book (envTime env)) (wNotes w) }
    | c == 'c' || c == 'C' -> pure w { wContour = [], wCount = 0, wNotes = [] }
  _ -> pure w
  where
    book now n = Booked (now + fromIntegral (nStart n) * msPerFrame / 1000)
                        (440 * 2 ** ((fromIntegral (nMidi n) - 69) / 12))
                        (fromIntegral (nLen n) * msPerFrame / 1000)

-- ---------- やってみよう ----------
-- 1. 「ドレミファソラシド」を歌って、階段になるか見る
-- 2. minNoteFrames を変える。短いと細切れ、長いと速いフレーズが消える
-- 3. 再生の音色を Square から Triangle や Sawtooth に

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str
