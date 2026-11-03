-- ============================================
-- 04 ビート検出（音楽のテンポをコンピュータが聞き取る）
-- 音量が急に増える瞬間（オンセット）を見つけ、その間隔からテンポ（BPM）を推定する
-- スマホで音楽を流す、手を叩く、机をたたく → 光が点滅し、BPM が出る
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）/ マウス左右 = 感度
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import Text.Printf (printf)

data World = World
  { wMic    :: !(Maybe Mic)  -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool         -- 本物のマイクの音が来ているか
  , wEnergy :: ![Float]      -- 音量の履歴（新しいものが先頭、最大 300）
  , wOnsets :: ![Float]      -- 音が立ち上がった時刻（ミリ秒、新しいものが先頭、最大 16）
  , wBpm    :: !Float
  , wFlash  :: !Float
  , wLast   :: !Float        -- 最後のオンセットの時刻（ミリ秒）
  }

main :: IO ()
main = runSketch (sketch "07-04 Beat Tracking" (World Nothing False [] [] 0 0 0 ))
  { skBack  = hsb 260 40 8
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 感度：マウスの左右で 1.1〜3
sensitivity :: Env -> Float
sensitivity env = clampF 1.1 3 (remap (fst (fromG env (envMouse env))) 0 (envW env) 1.1 3)

stepWorld :: Env -> World -> IO World
stepWorld env w = do
  e <- maybe (pure 0) micLevel (wMic w)
  live <- maybe (pure False) micLive (wMic w)
  let now = envTime env * 1000
      -- ★ オンセット検出：直近の平均より sensitivity 倍以上大きく、前回から 0.15 秒以上たっていたら
      recent = take 29 (wEnergy w)
      avg = sum recent / fromIntegral (max (length recent) 1)
      isOnset = e > avg * sensitivity env && e > 0.02 && now - wLast w > 150
      w1 = w { wLive = live, wEnergy = take 300 (e : wEnergy w) }
      w2 | isOnset =
             let os = take 16 (now : wOnsets w1)
             in w1 { wOnsets = os, wLast = now, wFlash = 1, wBpm = estimateBpm os (wBpm w1) }
         | otherwise = w1
  pure w2 { wFlash = wFlash w2 * 0.85 }

-- ★ 間隔の中央値からテンポを求める（外れ値に強い）
estimateBpm :: [Float] -> Float -> Float
estimateBpm os old
  | length os < 4 = old
  | otherwise =
      let gaps = sort (zipWith (-) os (drop 1 os))     -- os は新しい順
          median = case drop (length gaps `div` 2) gaps of
                     (g : _) -> g / 1000
                     []      -> 0.5
          b = fold2 (60 / median)
      in if old == 0 then b else lerpF old b 0.3

-- 速すぎたら半分に、遅すぎたら 2 倍に
fold2 :: Float -> Float
fold2 b
  | b > 200 = fold2 (b / 2)
  | b < 50 = fold2 (b * 2)
  | otherwise = b

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([light, number, unit, axis, curve] ++ onsetLines ++ hud)
  where
    wd = envW env
    ht = envH env
    now = envTime env * 1000
    cy = ht * 0.42
    -- ---- 光 ----
    light = Color (hsba 45 90 100 (wFlash w)) (at (toG env (wd / 2, cy)) (disc ((200 + wFlash w * 200) / 2)))
    number = txtM env (wd / 2) cy 120 white (if wBpm w > 0 then show (round (wBpm w) :: Int) else "--")
    unit = txtM env (wd / 2) (cy + 90) 28 white "BPM"
    -- ---- 音量の波形とオンセット ----
    base = ht * 0.85
    h = ht * 0.2
    axis = Color (hsb 0 0 30) (Line [toG env (0, base), toG env (wd, base)])
    curve = Color (hsb 170 70 100) (thickPath 2
      [ toG env (remap i 0 300 0 wd, base - min e 0.5 * h * 2)
      | (i, e) <- zip [0 ..] (reverse (wEnergy w)) ])
    -- オンセットの縦線（画面の幅 = 5 秒）
    onsetLines =
      [ Color (hsb 45 90 100) (thickLine 2 (toG env (x, base)) (toG env (x, base - h)))
      | t <- wOnsets w
      , let x = wd - (now - t) / 1000 * (wd / 5)
      , x > 0 ]
    hud =
      [ txt env 20 40 18 white (case wMic w of
          Nothing -> "Click to turn the mic ON"
          Just _  -> "Clap or play music: finds onsets and estimates tempo   Sensitivity "
                     ++ printf "%.1f" (sensitivity env) ++ " (mouse X)")
      , txt env 20 68 15 (hsb 0 0 60) "Yellow lines = detected beats. BPM = 60 / median gap (s). The tempo can come out doubled or halved."
      , if micOff then txt env 20 94 15 (hsb 0 0 70) "No mic found: demo signal" else Blank
      ]
    micOff = case wMic w of { Just _ -> not (wLive w); Nothing -> False }

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. メトロノーム（スマホアプリ）を 120 BPM で鳴らして、120 と出るか試す
-- 2. 自分で手を叩いて、一定のテンポを保てるか（BPM の表示が安定するか）
-- 3. 間隔の「中央値」を「平均」にするとどうなる？（1 回の誤検出で大きくずれる）

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（上下左右とも中央揃え）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx gy s c str
