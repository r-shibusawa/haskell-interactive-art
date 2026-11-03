-- ============================================
-- 11 タップテンポとメトロノーム（声・手拍子でテンポを決める）
-- スペースキーや手拍子でテンポを打つと BPM を測り、そのテンポでメトロノームが鳴る
-- 自分のテンポ感が安定しているか、揺らぎのグラフで見える
-- クリックでマイク ON（手拍子で打つ）/ スペースで打つ / M でメトロノーム ON/OFF / R でリセット
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

data World = World
  { wMic    :: !(Maybe Mic)  -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool         -- 本物のマイクの音が来ているか
  , wTaps   :: ![Float]      -- 打った時刻（ミリ秒、新しいものが先頭、最大 16）
  , wBpm    :: !Float
  , wMetro  :: !Bool         -- メトロノーム
  , wNext   :: !Float        -- 次に鳴らす時刻（秒）
  , wFlash  :: !Float
  , wDevs   :: ![Float]      -- 各打点の、理想からのずれ（ミリ秒、古い順）
  , wLast   :: !Float        -- 最後に手拍子を検出した時刻（ミリ秒）
  , wEnergy :: ![Float]      -- 音量の履歴（新しいものが先頭、最大 30）
  }

main :: IO ()
main = runSketch (sketch "07-11 Tempo Tap" (World Nothing False [] 0 False 0 0 [] 0 []))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 1 回打つ：間隔の平均から BPM = 60000 ÷ 平均（ミリ秒）
tapAt :: Float -> World -> World
tapAt now w =
  let old = case wTaps w of
        (t : _) | now - t > 2500 -> []     -- ★ 2.5 秒あいたら最初から
        ts -> ts
      taps = take 16 (now : old)
      gaps = reverse (zipWith (-) taps (drop 1 taps))
      mean = sum gaps / fromIntegral (length gaps)
  in if null gaps || mean <= 0
       then w { wTaps = taps }
       else w { wTaps = taps, wBpm = 60000 / mean, wDevs = map (subtract mean) gaps }

stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  let now = envTime env
      w1 = w0 { wFlash = wFlash w0 * 0.85 }
  -- 手拍子でも打てる（音量の立ち上がり）
  w2 <- case wMic w1 of
    Nothing -> pure w1
    Just mic -> do
      live <- micLive mic
      e <- micLevel mic
      let hist = take 30 (e : wEnergy w1)
          prev = drop 1 hist
          avg = sum prev / fromIntegral (max (length prev) 1)
          ms = now * 1000
          w' = w1 { wLive = live, wEnergy = hist }
      pure (if e > avg * 2 && e > 0.03 && ms - wLast w' > 200
              then tapAt ms w' { wLast = ms } else w')
  metronome now w2

-- 時刻が来たらメトロノームを鳴らす
metronome :: Float -> World -> IO World
metronome now w
  | not (wMetro w) || wBpm w <= 0 = pure w
  | wNext w < now - 1 = metronome now w { wNext = now }    -- 長く止まっていたらまとめて鳴らさない
  | wNext w <= now = do
      playTone 1200 0.3 0.05 Sine
      metronome now w { wNext = wNext w + 60 / wBpm w, wFlash = 1 }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([light, number, unit, axis] ++ bars ++ notes ++ hud)
  where
    wd = envW env
    ht = envH env
    -- ---- BPM ----
    cy = ht * 0.4
    light = Color (hsba 45 90 100 (wFlash w)) (at (toG env (wd / 2, cy)) (disc ((220 + wFlash w * 100) / 2)))
    number = txtM env (wd / 2) cy 130 white (if wBpm w > 0 then show (round (wBpm w) :: Int) else "--")
    unit = txtM env (wd / 2) (cy + 95) 26 white "BPM"
    -- ---- 揺らぎのグラフ ----
    gx = 100
    gw = wd - 200
    gy = ht * 0.8
    devs = wDevs w
    axis = Color (hsb 0 0 30) (Line [toG env (gx, gy), toG env (gx + gw, gy)])
    bars =
      [ Color (hsb (if abs d < 30 then 120 else 0) 80 100) (thickLine 6 (toG env (x, gy)) (toG env (x, gy - d)))
      | (i, dv) <- zip [0 :: Int ..] devs
      , let x = gx + fromIntegral i / 15 * gw
            d = clampF (-150) 150 dv ]
    sd = sqrt (sum (map (\d -> d * d) devs) / fromIntegral (max 1 (length devs)))
    verdict | sd < 20 = "like a machine!"
            | sd < 40 = "steady"
            | sd < 80 = "average"
            | otherwise = "try to keep it even"
    notes =
      txt env gx (gy - 90) 14 (hsb 0 0 60) "Deviation of each tap (ms): up = late, down = early. Green = within +-30 ms"
      : [ txt env gx (gy + 30) 14 (hsb 0 0 60) ("Spread (std dev) " ++ printf "%.0f" sd ++ " ms   " ++ verdict) | not (null devs) ]
    micOn = case wMic w of { Just _ -> True; Nothing -> False }
    hud =
      [ txt env 20 40 18 white
          ("Space: tap (" ++ (if micOn then "claps work too" else "click: mic on for claps") ++ ")   M: metronome "
           ++ (if wMetro w then "ON" else "OFF") ++ "   R: reset   Taps " ++ show (length (wTaps w)))
      , txt env 20 68 15 (hsb 0 0 60) "Same as the tap tempo in music software: BPM = 60000 / mean gap (ms)"
      , if micOn && not (wLive w) then txt env 20 94 15 (hsb 0 0 70) "No mic found: demo signal" else Blank
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> ensureMic w
  EventKey (SpecialKey KeySpace) Down _ _ -> tapAt (envTime env * 1000) <$> ensureMic w
  EventKey (Char c) Down _ _
    | c == 'm' || c == 'M' -> do
        w' <- ensureMic w
        pure w' { wMetro = not (wMetro w'), wNext = envTime env + 0.1 }
    | c == 'r' || c == 'R' -> do
        w' <- ensureMic w
        pure w' { wTaps = [], wBpm = 0, wDevs = [] }
    | otherwise -> ensureMic w
  _ -> pure w

-- マイクがまだなら ON にする
ensureMic :: World -> IO World
ensureMic w = case wMic w of
  Just _ -> pure w
  Nothing -> do
    mic <- startMic
    pure w { wMic = Just mic }

-- ---------- やってみよう ----------
-- 1. 120 BPM を目指して 16 回打つ。ばらつきは何 ms？ プロのドラマーは 10 ms 以下
-- 2. メトロノームを ON にして、それに合わせて打つと、ばらつきは減る？
-- 3. 「遅れる癖」「走る癖」がグラフに出る（上に偏る／下に偏る）

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
