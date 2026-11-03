-- ============================================
-- 14 声でセットポイント（マイクの大きさ → ドローンの目標高度）
-- 声が大きいほど目標が高くなる。ドローンは PID でそれを追いかける
--   目標が急に動くと、ドローンは「遅れて」ついてくる。その遅れと行きすぎがグラフで見える
-- クリック = マイク ON（使えないときはマウスの高さが目標）/ 左右キー = Kp / 上下キー = Kd
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

kI, mass, grav, tMax, smooth, dt :: Float
kI = 2                -- ★ I ゲイン
mass = 1              -- ドローンの質量
grav = 9.8            -- 重力
tMax = 2 * mass * grav -- ★ 最大推力（ホバリングの 2 倍）
smooth = 0.15         -- ★ 声の大きさのなめらかさ（小さいほど目標がゆっくり動く）
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wKp, wKd :: !Float   -- ★ PID ゲイン（高度 [m] の誤差 → 推力の増減）
  , wMic     :: !(Maybe Mic)
  , wReady   :: !Bool    -- 本物のマイクから音が来ているか
  , wMouseY  :: !Float   -- マウスの高さ（p5.js と同じく最初は 0）
  , wY, wVy, wR, wThrust, wI, wE, wLvl :: !Float
  , hY, hR   :: !Hist
  }

world0 :: World
world0 = World
  { wKp = 8, wKd = 5, wMic = Nothing, wReady = False, wMouseY = 0
  , wY = 1, wVy = 0, wR = 1, wThrust = mass * grav, wI = 0, wE = 0, wLvl = 0
  , hY = Seq.empty, hR = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-14 Voice Setpoint" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \env _ w -> stepIO env w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 目標：マイクの音量（なければマウス）
stepIO :: Env -> World -> IO World
stepIO env w = do
  ready <- maybe (pure False) micLive (wMic w)
  lvl <- case wMic w of
    Just m | ready -> micLevel m
    _ -> pure 0
  let w1 = w { wReady = ready }
      w2 | ready =
             let s = wLvl w + (lvl - wLvl w) * smooth
             in w1 { wLvl = s, wR = remap (clampF 0.005 0.25 s) 0.005 0.25 0.3 4.5 }  -- ★ 声の大きさ → 目標高度 [m]
         | otherwise =
             let v = remap (wMouseY w) (envH env * 0.85) (envH env * 0.1) 0.3 4.5
             in w1 { wR = clampF 0.3 4.5 v }
  pure (step w2)

step :: World -> World
step w@World{..} =
  let e = wR - wY          -- ★ PID で推力を決める（微分は速度にかける）
      i = clampF (-5) 5 (wI + e * dt)
      t = clampF 0 tMax (mass * grav + wKp * e + kI * i - wKd * wVy)
      vy0 = wVy + (t / mass - grav) * dt
      y0 = wY + vy0 * dt
      vy1 = if y0 < 0 then max vy0 0 else vy0   -- 地面
      y = clampF 0 6 (max y0 0)
      vy = clampF (-20) 20 vy1
  in w { wE = e, wI = i, wThrust = t, wVy = vy, wY = y, hY = pushHist hY y, hR = pushHist hR wR }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ seg 3 (hsb 0 0 40) (gx0, groundY) (gx1, groundY) ]
  ++ concat [ [ seg 1 (hsb 0 0 25) (gx0, yOf h) (gx1, yOf h)
              , txt gx0 (if h >= 5 then yOf h + 18 else yOf h - 4) 14 (hsb 0 0 50) (show (round h :: Int) ++ " m") ]
            | h <- [1 .. 5] ]
  ++
  [ -- 目標の線
    seg 2 yel (gx0, yOf wR) (gx1, yOf wR)
  , txt (gx1 - 100) (yOf wR - 6) 14 yel ("Target " ++ nf 2 wR ++ " m")
    -- ドローン
  , seg 4 (hsb 0 0 80) (dx - 60, dy) (dx + 60, dy)
  , box (dx - 18) (dy - 12) 36 24 (hsb 30 80 100)
  , oval (dx - 60) (dy - 6) rotor 8 rotorC
  , oval (dx + 60) (dy - 6) rotor 8 rotorC
    -- 推力の矢印
  , seg 4 (hsb 120 80 100) (dx, dy + 14) (dx, dy + 14 + wThrust * 3)
  , txt (dx + 70) (dy + 22) 14 white ("Alt " ++ nf 2 wY ++ " m  Thrust " ++ nf 1 wThrust ++ " N")
    -- 音量メーター
  , box gx0 mY (gx1 - gx0) 12 (hsb 0 0 20)
  , box gx0 mY ((gx1 - gx0) * clampF 0 1 (wLvl / 0.25)) 12 (hsb 300 60 100)
  , txt gx0 (mY - 6) 14 (hsb 0 0 70) (if wReady then "Voice level" else "No mic (mouse height sets the target)")
    -- グラフ
  , drawPlot (wd * 0.52) (ht * 0.2) (wd * 0.44) (ht * 0.45)
      [serS hR 45 90 "Target (voice)", ser hY 30 "Drone altitude"] 0 5.5 "Altitude [m] (gap = lag)"
  , txt 20 40 18 white ((if wReady then "Use your voice to lift the drone" else "Click to turn the mic ON")
        ++ "   Kp = " ++ nf 1 wKp ++ " (L/R)   Kd = " ++ nf 1 wKd ++ " (Up/Down)   Error " ++ nf 2 wE ++ " m")
  , txt 20 68 15 c60 "The gap until altitude (orange) follows the target (yellow) is lag. More Kp: faster, overshoots. Kd brakes it."
  , txt 20 (ht - 30) 15 c60 ("Thrust limits: max " ++ nf 0 tMax ++ " N, min 0 (free fall). A sudden loud voice hits the limit and cannot catch up.")
  ]
  where
    wd = envW env
    ht = envH env
    gx0 = wd * 0.05
    gx1 = wd * 0.47
    groundY = ht * 0.85
    topY = ht * 0.1
    yOf h = remap h 0 5 groundY topY
    yel = hsb 45 90 100
    dx = (gx0 + gx1) / 2
    dy = yOf wY
    rotor = 44 + fromIntegral (envFrame env `mod` 3) * 4
    rotorC = hsba 200 40 100 ((30 + remap wThrust 0 tMax 0 100 * 0.5) / 100)
    mY = ht * 0.92
    c60 = hsb 0 0 60

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> do
    let w1 = w { wMouseY = snd (toP5 env p) }
    case wMic w of
      Just _ -> pure w1            -- マイクを開くのは 1 回だけ
      Nothing -> do
        m <- startMic
        pure w1 { wMic = Just m }
  EventMotion p -> pure w { wMouseY = snd (toP5 env p) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wKp = min (wKp w + 1) 40 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wKp = max (wKp w - 1) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wKd = min (wKd w + 1) 30 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wKd = max (wKd w - 1) 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「あー」と一定の声で止める。ドローンは止まる？ ふらつく？（声のゆれ = 目標のノイズ）
-- 2. Kd = 0 にすると？ Kp を 30 にすると？ 声で「ゆれ」を起こしてみよう
-- 3. smooth を 0.02 にすると目標がなめらかになる。遅れは増える？ 減る？（フィルタと制御のトレードオフ）
-- 4. 目標を「声の高さ」にしてみよう（micSpectrum の一番大きい山）

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- 中心 (x, y)、幅 w、高さ h のだ円
oval :: Float -> Float -> Float -> Float -> Color -> Picture
oval x y w h c = Color c (Translate x y (Scale (w / 2) (h / 2) (disc 1)))

-- 履歴：新しい値を足し、古い値を捨てる
type Hist = Seq Float

pushHist :: Hist -> Float -> Hist
pushHist h v =
  let v' = if isNaN v || isInfinite v then 0 else v
      h' = h |> v'
  in v' `seq` (if Seq.length h' > histMax then Seq.drop 1 h' else h')

-- グラフの線：データ、色、名前、太さ
data Series = Series !Hist !Color !String !Float

-- 色相 hue の線（彩度 70）
ser :: Hist -> Float -> String -> Series
ser h hue name = Series h (hsb hue 70 100) name 2

-- 彩度も指定する線
serS :: Hist -> Float -> Float -> String -> Series
serS h hue sat name = Series h (hsb hue sat 100) name 2

plotBg :: Color
plotBg = hsb 0 0 14

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x y w h series lo hi title = Pictures
  ( box x y w h plotBg
  : (if y0 > y && y0 < y + h then seg 1 (hsb 0 0 30) (x, y0) (x + w, y0) else Blank)
  : map curve series
  ++ [txt (x + 8) (y + 20) 14 (hsb 0 0 70) title]
  ++ legend (x + w - 8) series )
  where
    y0 = remap 0 lo hi (y + h) y
    px i = remap (fromIntegral i) 0 (fromIntegral histMax - 1) x (x + w)
    py v = remap (clampF lo hi v) lo hi (y + h) y
    curve (Series d c _ wt) =
      let pts = zipWith (\i v -> (px i, py v)) [0 :: Int ..] (toList d)
      in Color c (if wt <= 1 then Line pts else thickPath wt pts)
    legend _ [] = []
    legend lx (Series _ c name _ : rest) =
      let tw = textW 14 name
      in txt (lx - tw) (y + 20) 14 c name : legend (lx - tw - 14) rest
