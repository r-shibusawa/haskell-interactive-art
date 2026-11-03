-- ============================================
-- 00 音の高さと周波数（ピアノの鍵盤）
-- 鍵盤を 1 つ右に進むと、周波数は 2 の 12 乗根（約 1.0595）倍。12 個進むと 2 倍（1 オクターブ）
-- 鍵盤をクリックして音を鳴らす / キーボード A W S E D F T G Y H U J K でも
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (elemIndex)

data World = World
  { wLast :: !Int     -- 最後に鳴らした鍵盤（-1 = まだ）
  , wTime :: !Float   -- 鳴らした時刻（秒）
  }

keyChars :: String
keyChars = "awsedftgyhujk"

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C"]

baseMidi :: Float
baseMidi = 60              -- ★ いちばん左の音（60 = C4）

main :: IO ()
main = runSketch (sketch "07-00 Notes and Frequency" (World (-1) 0))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 半音の番号（A4 = 69）→ 周波数
midiToFreq :: Float -> Float
midiToFreq m = 440 * 2 ** ((m - 69) / 12)

isBlack :: Int -> Bool
isBlack i = i `elem` [1, 3, 6, 8, 10]

-- i 番目より左にある白鍵の数
whitesBefore :: Int -> Int
whitesBefore i = length (filter (not . isBlack) [0 .. i - 1])

-- 鍵盤の位置：白鍵の幅、左端、上端、高さ（p5.js の座標）
layout :: Env -> (Float, Float, Float, Float)
layout env =
  let whiteW = min 110 ((envW env - 100) / 8)
  in (whiteW, (envW env - whiteW * 8) / 2, envH env * 0.4, envH env * 0.4)

-- i 番目の鍵盤の左端
keyLeft :: Env -> Int -> Float
keyLeft env i =
  let (whiteW, x0, _, _) = layout env
      x = x0 + fromIntegral (whitesBefore i) * whiteW
  in if isBlack i then x - whiteW * 0.3 else x

-- i 番目の鍵盤の中心
keyCenter :: Env -> Int -> Float
keyCenter env i =
  let (whiteW, _, _, _) = layout env
  in keyLeft env i + (if isBlack i then whiteW * 0.3 else whiteW / 2)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap whiteKey whites ++ concatMap blackKey blacks ++ graph ++ hud)
  where
    (whiteW, x0, y0, h) = layout env
    whites = filter (not . isBlack) [0 .. 12]
    blacks = filter isBlack [0 .. 12]
    isOn i = wLast w == i && envTime env - wTime w < 0.3
    hz i = midiToFreq (baseMidi + fromIntegral i)

    whiteKey i =
      let x = keyLeft env i
          c = keyCenter env i
          on = isOn i
      in [ Color (if on then hsb 45 90 100 else hsb 0 0 95) (box env x y0 whiteW h)
         , Color (hsb 0 0 20) (frame env x y0 whiteW h)
         , txtC env c (y0 + h - 64) 14 (hsb 0 0 55) [toUpperAscii (keyChars !! i)]
         , txtC env c (y0 + h - 40) 16 (hsb 0 0 30) (noteNames !! i ++ (if i == 12 then "5" else "4"))
         , txtC env c (y0 + h - 16) 16 (hsb 0 0 30) (show (round (hz i) :: Int) ++ " Hz")
         ]

    blackKey i =
      let x = keyLeft env i
          c = keyCenter env i
          on = isOn i
      in [ Color (if on then hsb 45 90 100 else hsb 0 0 12) (box env x y0 (whiteW * 0.6) (h * 0.6))
         , txtC env c (y0 + h * 0.6 - 48) 14 (hsb 0 0 70) [toUpperAscii (keyChars !! i)]
         , txtC env c (y0 + h * 0.6 - 30) 14 (hsb 0 0 70) (noteNames !! i)
         , txtC env c (y0 + h * 0.6 - 12) 14 (hsb 0 0 70) (show (round (hz i) :: Int))
         ]

    -- 周波数の対数グラフ
    gy = envH env * 0.3
    graph =
      Color (hsb 0 0 30) (Line [toG env (x0, gy), toG env (x0 + whiteW * 8, gy)])
      : [ Color (if isBlack i then hsb 0 0 40 else hsb 170 60 100)
                (box env (keyCenter env i - 8) (gy - bh) 16 bh)
        | i <- [0 .. 12]
        , let bh = remap (hz i) 261 523 20 100
        ]
      ++ [txt env x0 (gy - 110) 14 (hsb 0 0 60) "Frequency (x1.0595 per key: the bars grow by equal ratios)"]

    hud =
      [ txt env 20 40 18 white "Click a key, or press A W S E D F T G Y H U J K      f = 440 x 2^((n - 69) / 12)"
      , txt env 20 66 16 white "C4 = 261.6 Hz, A4 = 440 Hz, C5 = 523.3 Hz"
      , txt env 20 92 15 (hsb 0 0 60) "One octave up = double the frequency. 12 equal steps: 1 semitone = 2^(1/12). Ears hear ratios."
      ]

-- 鍵盤を鳴らす
pressKey :: Env -> Int -> World -> IO World
pressKey env i w = do
  playTone (midiToFreq (baseMidi + fromIntegral i)) 0.3 0.8 Triangle
  pure w { wLast = i, wTime = envTime env }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    case keyAt env (fromG env pos) of
      Just i  -> pressKey env i w
      Nothing -> pure w
  EventKey (Char c) Down _ _ ->
    case elemIndex (toLower c) keyChars of
      Just i  -> pressKey env i w
      Nothing -> pure w
  _ -> pure w

-- 黒鍵を優先して判定する
keyAt :: Env -> Point -> Maybe Int
keyAt env (mx, my)
  | my < y0 || my > y0 + h = Nothing
  | otherwise = case filter hitBlack (filter isBlack [0 .. 12]) ++ filter hitWhite (filter (not . isBlack) [0 .. 12]) of
      (i : _) -> Just i
      []      -> Nothing
  where
    (whiteW, _, y0, h) = layout env
    hitBlack i = let x = keyLeft env i in mx > x && mx < x + whiteW * 0.6 && my < y0 + h * 0.6
    hitWhite i = let x = keyLeft env i in mx > x && mx < x + whiteW

-- ---------- やってみよう ----------
-- 1. C4 と C5 の周波数を比べよう（ぴったり 2 倍）
-- 2. 「ドレミファソラシド」を弾いて、周波数の増え方を見る（等間隔ではなく等倍）
-- 3. baseMidi の 60 を 48 にすると 1 オクターブ下。72 にすると上

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 長方形の枠
frame :: Env -> Float -> Float -> Float -> Float -> Picture
frame env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rectLine gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（中央揃え。y は文字の下端）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx (gy + s / 2) s c str

toUpperAscii :: Char -> Char
toUpperAscii c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
