-- ============================================
-- 02 AM と FM（波に情報を乗せる）
-- 低い周波数のメッセージ（声）を、そのままでは遠くへ飛ばせない
-- 高い周波数の「搬送波」の 大きさ（AM）か 周波数（FM）を、メッセージで揺らして運ぶ
-- 1 = AM / 2 = FM / マウス左右 = メッセージの周波数 / クリックで音
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Numeric (showFFloat)

data Mode = AM | FM deriving (Eq, Show)

data World = World
  { wMode    :: !Mode
  , wPhase   :: !Float    -- メッセージの位相（少しずつ動く）
  , wPlaying :: !Bool     -- 音を出しているか
  }

carrierFreq :: Float
carrierFreq = 30          -- ★ 画面内に 30 周期（本物は数百 kHz〜）

main :: IO ()
main = runSketch (sketch "06-02 AM and FM" (World AM 0 False))
  { skBack  = hsb 230 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- メッセージの周波数はマウス左右で決める
msgFreqOf :: Env -> Float
msgFreqOf env = clampF 1 5 (remap (fst (mouseXY env)) 0 (envW env) 1 5)

-- メッセージ（-1〜1）
message :: Float -> Float -> Float -> Float
message f ph t = sin (tau * f * t + ph)

carrier :: Float -> Float
carrier t = sin (tau * carrierFreq * t)

-- ★ 変調
modulate :: Mode -> Float -> Float -> Float -> Float
modulate AM f ph t = (0.6 + 0.4 * message f ph t) * carrier t   -- 大きさを変える
modulate FM f ph t =                                             -- 周波数を変える。積分して位相にする
  sin (tau * carrierFreq * t + 8 * negate (cos (tau * f * t + ph)) / f)

step :: Env -> Float -> World -> IO World
step env _ w = do
  when (wPlaying w && envFrame env `mod` 6 == 0) (playModulated env w)
  pure w { wPhase = wPhase w + 0.01 }

-- 連続音の代わりに、短い音をくり返す（搬送波 440 Hz を、聞こえる速さで AM/FM）
playModulated :: Env -> World -> IO ()
playModulated env w = case wMode w of
  AM -> playTone 440 (0.2 * (0.6 + 0.4 * m)) 0.12 Sine   -- 大きさを ±0.4 揺らす
  FM -> playTone (fromIntegral (round (440 + 150 * m) :: Int)) 0.2 0.12 Sine   -- 周波数を ±150 Hz 揺らす
  where m = sin (tau * msgFreqOf env * envTime env)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ row 0 "Message (the signal to send)" (message f ph) 120 1.5
  , row 1 "Carrier (high-frequency wave)" carrier 0 1.5
  , row 2 (if wMode w == AM then "AM: message changes the carrier's SIZE" else "FM: message changes the carrier's FREQUENCY")
        (modulate (wMode w) f ph) 40 2.5
  , if wMode w == AM then envelope else Blank
  , txt env 20 40 18 (hsb 0 0 100)
      (show (wMode w) ++ " (1: AM  2: FM)  Message freq " ++ nf 1 f ++ " (mouse X)  Click: sound " ++ (if wPlaying w then "ON" else "OFF"))
  , txt env 20 68 15 (hsb 0 0 60) "AM radio 500-1600 kHz, FM radio 76-95 MHz. FM resists noise and sounds cleaner"
  ]
  where
    f = msgFreqOf env
    ph = wPhase w
    left = 60
    right = envW env - 60
    midOf i = envH env * (0.25 + i * 0.25)
    h = envH env * 0.09
    n = floor (right - left) :: Int
    tOf x = (x - left) / (right - left)

    row :: Float -> String -> (Float -> Float) -> Float -> Float -> Picture
    row i name fn hue weight =
      let mid = midOf i
          c = hueColor hue
      in Pictures
           [ Color (hsb 0 0 25) (Line [toG env left mid, toG env right mid])
           , Color c (thickPath weight [ toG env x (mid - fn (tOf x) * h)
                                       | k <- [0 .. n], let x = left + fromIntegral k ])
           , txt env left (mid - h - 10) 15 c name
           ]

    -- 包絡線 = メッセージの形
    envelope =
      let mid = midOf 2
          edge s = thickPath 2 [ toG env x (mid - s * (0.6 + 0.4 * message f ph (tOf x)) * h)
                               | k <- [0, 3 .. n], let x = left + fromIntegral k ]
      in Color (hsba 120 70 100 0.6) (Pictures [edge 1, edge (-1)])

-- 色相 0 は白っぽい灰色にする
hueColor :: Float -> Color
hueColor hue = if hue == 0 then hsb 0 0 80 else hsb hue 70 100

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (Char '1') Down _ _ -> w { wMode = AM }
  EventKey (Char '2') Down _ _ -> w { wMode = FM }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. AM の出力の「輪郭」がメッセージと同じ形になっているのを確認（受信側はこの輪郭を取り出す）
-- 2. FM の波が「詰まったり広がったり」しているのを見る
-- 3. modulate の AM で 0.4 を 1.0 にすると（過変調）、輪郭がつぶれて受信側で歪む

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
