-- ============================================
-- 15 ドローンのホバリング（ゲーム・自由制作のベース）
-- 2 つのローターの推力で飛ぶ 2D ドローン。手動で飛ばすと難しいが、自動（PID）なら風の中でも止まれる
--   自動：高度は PID で推力、横位置は「傾き」を決めてから、傾きを PID で追う（カスケード制御）
-- クリック = 目標の位置 / A = 手動⇔自動 / 手動：上下 = 推力、左右 = 傾き / W = 風 / S 保存 / C 軌跡を消す / R リセット
-- ============================================
-- ★ のついた数字を変えて、自分のドローンに！
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

windMax, mass, grav, inertia, thrustMax, tiltMax, dt, worldW, worldH :: Float
windMax = 4       -- ★ 風の強さ
mass = 1          -- ★ 質量
grav = 9.8        -- ★ 重力
inertia = 0.05    -- ★ 慣性モーメント
thrustMax = 25    -- ★ 最大推力
tiltMax = 0.6     -- ★ 自動のときの最大の傾き [rad]
dt = 1 / 60
worldW = 10       -- 世界の大きさ [m]
worldH = 6

altP, altI, altD, posP, posD, tiltP, tiltD :: Float
altP = 6          -- ★ 高度の PID
altI = 1.5
altD = 4
posP = 1.2        -- ★ 横位置 → 目標の傾き
posD = 1.4
tiltP = 60        -- ★ 傾き → トルク
tiltD = 8

histMax, trailMax :: Int
histMax = 300
trailMax = 3000   -- ★ 軌跡の線分を何本まで覚えておくか

-- 軌跡の線分 1 本：始点、終点（世界の座標）、そのときのモードの色相
data TrailSeg = TrailSeg !Float !Float !Float !Float !Float

data World = World
  { wAuto, wWindOn :: !Bool
  , wX, wY, wVx, wVy, wPhi, wOmega :: !Float
  , wTx, wTy, wThrust, wTorque, wI :: !Float
  , wManualT, wWind :: !Float
  , wCrashes, wScore :: !Int
  , wTrail :: !(Seq TrailSeg)
  , wLastX, wLastY :: !Float      -- 軌跡に最後に足した点
  , hY, hTy, hX, hTx :: !Hist
  }

world0 :: World
world0 = World
  { wAuto = True, wWindOn = True, wX = 0, wY = 2, wVx = 0, wVy = 0, wPhi = 0, wOmega = 0
  , wTx = 0, wTy = 3, wThrust = mass * grav, wTorque = 0, wI = 0, wManualT = mass * grav, wWind = 0
  , wCrashes = 0, wScore = 0, wTrail = Seq.empty, wLastX = 0, wLastY = 2
  , hY = Seq.empty, hTy = Seq.empty, hX = Seq.empty, hTx = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-15 Drone Hover" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w =
  let f = fromIntegral (envFrame env) :: Float
      -- 風（ゆっくり変わる + ときどき突風）
      wind | wWindOn w = (noise2 (f * 0.01) 0 - 0.5) * 2 * windMax + (if noise2 (f * 0.05 + 100) 0 > 0.8 then windMax else 0)
           | otherwise = 0
      control = if wAuto w then autoControl else manualControl env
  in addTrail (record (physics (control w { wWind = wind })))

-- 運動方程式（推力は機体の上向き）
physics :: World -> World
physics w@World{..} =
  let tq = clampF (-8) 8 wTorque
      sub (x, y, vx, vy, phi, om) _ =
        let ax = (wThrust * sin phi + wWind - 0.3 * vx) / mass
            ay = (wThrust * cos phi - mass * grav - 0.3 * vy) / mass
            vx' = vx + ax * dt / 2
            vy' = vy + ay * dt / 2
            om' = om + (tq / inertia) * dt / 2
        in (x + vx' * dt / 2, y + vy' * dt / 2, vx', vy', phi + om' * dt / 2, om')
      (x0, y0, vx0, vy0, phi0, om0) = foldl' sub (wX, wY, wVx, wVy, wPhi, wOmega) [1, 2 :: Int]
      om1 = clampF (-20) 20 om0
      vx1 = clampF (-30) 30 vx0
      vy1 = clampF (-30) 30 vy0
      halfW = worldW / 2
      (x2, vx2) | x0 < -halfW = (-halfW, abs vx1 * 0.3)
                | x0 > halfW = (halfW, negate (abs vx1) * 0.3)
                | otherwise = (x0, vx1)
      (y2, vy2) | y0 > worldH = (worldH, min vy1 0)
                | otherwise = (y0, vy1)
      w2 = w { wTorque = tq, wX = x2, wY = y2, wVx = vx2, wVy = vy2, wPhi = phi0, wOmega = om1 }
  in if y2 <= 0 && (abs vy2 > 2 || abs phi0 > 0.5) then (resetDrone w2) { wCrashes = wCrashes + 1 }
     else if y2 < 0 then w2 { wY = 0, wVy = 0 }
     else w2

-- 得点と履歴
record :: World -> World
record w@World{..} = w
  { wScore = if dist2 (wX, wY) (wTx, wTy) < 0.3 then wScore + 1 else wScore
  , hY = pushHist hY wY, hTy = pushHist hTy wTy, hX = pushHist hX wX, hTx = pushHist hTx wTx }

autoControl :: World -> World
autoControl w@World{..} =
  let -- ★ 高度：PID で推力
      ey = wTy - wY
      i = clampF (-3) 3 (wI + ey * dt)
      aCmd = altP * ey + altI * i - altD * wVy
      -- ★ 横位置：目標の加速度 → 目標の傾き φd = atan(ax / g)
      ex = wTx - wX
      axCmd = posP * ex - posD * wVx
      phiD = clampF (-tiltMax) tiltMax (atan2 axCmd grav)
  in w { wI = i
         -- ★ 傾き：PD でトルク
       , wTorque = tiltP * (phiD - wPhi) - tiltD * wOmega
       , wThrust = clampF 0 thrustMax (mass * (grav + aCmd) / max (cos wPhi) 0.3) }

-- 手動：キーを押しているあいだ推力と傾きを変える
manualControl :: Env -> World -> World
manualControl env w@World{..} =
  let t1 = if isDown env (SpecialKey KeyUp) then min (wManualT + 0.5) thrustMax else wManualT
      t2 = if isDown env (SpecialKey KeyDown) then max (t1 - 0.5) 0 else t1
      tq = (if isDown env (SpecialKey KeyRight) then 1.5 else 0) - (if isDown env (SpecialKey KeyLeft) then 1.5 else 0)
  in w { wManualT = t2, wThrust = t2, wTorque = tq - 2 * wOmega }   -- 少しだけ自然な減衰

resetDrone :: World -> World
resetDrone w = w { wX = 0, wY = 0.5, wVx = 0, wVy = 0, wPhi = 0, wOmega = 0, wI = 0, wManualT = mass * grav }

-- 軌跡を足す（ほとんど動いていないときは足さないので、長いあいだ覚えていられる）
addTrail :: World -> World
addTrail w@World{..}
  | dist2 (wX, wY) (wLastX, wLastY) < 0.02 = w
  | otherwise =
      let s = TrailSeg wLastX wLastY wX wY (if wAuto then 200 else 30)
          tr = wTrail |> s
      in w { wTrail = if Seq.length tr > trailMax then Seq.drop 1 tr else tr, wLastX = wX, wLastY = wY }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ -- 軌跡
    Pictures [ seg 2 (hsba hue 60 100 0.5) (sx a, sy b) (sx c, sy d) | TrailSeg a b c d hue <- toList wTrail ]
  , seg 3 (hsb 0 0 40) (40, sy 0) (wd - 40, sy 0)    -- 地面
    -- 目標
  , Color yel (at (sx wTx, sy wTy) (ring 20 2))
  , seg 2 yel (sx wTx - 30, sy wTy) (sx wTx + 30, sy wTy)
  , seg 2 yel (sx wTx, sy wTy - 30) (sx wTx, sy wTy + 30)
    -- 風の矢印
  , if abs wWind > 0.2
      then Pictures [ seg 2 (hsba 200 50 100 0.7) (xa, yy) (xa + wWind * 20, yy)
                    | i <- [0 .. 4], let yy = 120 + i * 60, let xa = wd / 2 - wWind * 10 ]
      else Blank
    -- ドローン（機体の傾きだけ回して描く）
  , Translate (sx wX) (sy wY) $ Rotate (negate (wPhi * 180 / pi)) $ Pictures
      [ seg 5 (hsb 0 0 80) (-40, 0) (40, 0)
      , box (-14) (-10) 28 20 (hsb (if wAuto then 200 else 30) 80 100)
      , oval (-40) (-5) rotor 7 rotorC
      , oval 40 (-5) rotor 7 rotorC
      , seg 3 (hsb 120 80 100) (0, 10) (0, 10 + wThrust * 2) ]   -- 推力
    -- 小さなグラフ（右下）
  , drawPlot (wd - 330) (ht - 220) 290 70 [serS hTy 45 90 "Target", ser hY 200 "Altitude"] 0 worldH "Altitude"
  , drawPlot (wd - 330) (ht - 140) 290 70 [serS hTx 45 90 "Target", ser hX 200 "Position"] (-worldW / 2) (worldW / 2) "Horiz."
  , txt 20 40 18 white ((if wAuto then "Auto (PID)" else "Manual (arrows)") ++ "   A: switch   Click: target   Wind "
        ++ (if wWindOn then "ON" else "OFF") ++ " (W)   Crashes " ++ show wCrashes ++ "   In target " ++ nf 1 (fromIntegral wScore / 60) ++ " s")
  , txt 20 68 15 (hsb 0 0 60) ((if wAuto then "" else "Up/Down: thrust  L/R: tilt   ") ++ "Thrust " ++ nf 1 wThrust ++ " N   Tilt " ++ nf 0 (wPhi * 180 / pi)
        ++ " deg   Wind " ++ nf 1 wWind ++ "   S: save   C: clear trail   R: reset")
  ]
  where
    wd = envW env
    ht = envH env
    sx v = remap v (-worldW / 2) (worldW / 2) 40 (wd - 40)
    sy v = remap v 0 worldH (ht - 60) 80
    yel = hsb 45 90 100
    rotor = 36 + fromIntegral (envFrame env `mod` 3) * 3
    rotorC = hsba 0 0 100 ((40 + remap wThrust 0 thrustMax 0 50) / 100)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        wd = envW env
        ht = envH env
    in pure $ if my < 90 then w else w
         { wTx = clampF (-worldW / 2 + 0.5) (worldW / 2 - 0.5) (remap mx 40 (wd - 40) (-worldW / 2) (worldW / 2))
         , wTy = clampF 0.5 (worldH - 0.3) (remap my (ht - 60) 80 0 worldH) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> pure w { wAuto = not (wAuto w), wManualT = mass * grav, wI = 0 }
    'w' -> pure w { wWindOn = not (wWindOn w) }
    'c' -> pure w { wTrail = Seq.empty }
    's' -> saveSvg "drone" env (drawAll env w) >> pure w
    'r' -> pure (resetDrone w) { wCrashes = 0, wScore = 0 }
    _ -> pure w
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・手動モードでどれだけ長く目標の中にいられるか、友だちと勝負（風 ON で）
-- ・目標を自動で動かす（円を描く、マウスを追う）。PID はどこまで追いつける？
-- ・風を「一方向にずっと」にする。自動の横位置に偏差が残る → 横位置にも I を足してみよう
-- ・14 の声で目標高度、06 の Z-N でゲインを決める、など合体
-- ・ドローンを 3 台にして、1 台を追いかける「編隊飛行」

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
plotBg = hsba 0 0 14 0.8

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
