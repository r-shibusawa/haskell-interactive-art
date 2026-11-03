-- ============================================
-- 06 PID 制御とチューニング（ボール・アンド・ビーム）
-- 板を傾けてボールを目標の位置に止める。P だけでは必ずゆれる → D（微分）でブレーキ、I で偏差を消す
--   u = Kp e + Ki ∫e dt − Kd x'　（微分は「ボールの速さ」にかける）
-- Ziegler–Nichols 法：Ki = Kd = 0 で Kp を上げ、ゆれが止まらなくなる Kp = Ku と周期 Tu を測る
--   → Kp = 0.6 Ku, Ki = 1.2 Ku/Tu, Kd = 0.075 Ku Tu
-- Q/A = Kp 上下 / W/S = Ki / E/D = Kd / Z = いまの Kp を Ku として Z-N で自動設定 / クリック = 目標
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

tauServo, thMax, cBall, grav, dt :: Float
tauServo = 0.3   -- ★ 板を傾けるサーボの遅れ [秒]
thMax = 0.4      -- ★ 板の最大の傾き [rad]
cBall = 1.0      -- ボールの転がり摩擦
grav = 9.8
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wKp, wKi, wKd :: !Float   -- ★ PID ゲイン
  , wX, wV, wTh, wR, wI, wCmd :: !Float
  , wFell :: !Int             -- 落ちた回数
  , hX, hR, hE, hTh :: !Hist
  }

world0 :: World
world0 = World
  { wKp = 0.3, wKi = 0, wKd = 0, wX = 0, wV = 0, wTh = 0, wR = 0.5, wI = 0, wCmd = 0, wFell = 0
  , hX = Seq.empty, hR = Seq.empty, hE = Seq.empty, hTh = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-06 PID Tuning" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

step :: World -> World
step w@World{..} =
  let e = wR - wX                 -- ★ PID
      i0 = clampF (-3) 3 (wI + e * dt)
      u = wKp * e + wKi * i0 - wKd * wV
      cmd = clampF (-thMax) thMax u
      sub (x, v, th) _ =
        let th' = th + ((cmd - th) / tauServo) * dt / 2      -- サーボ：板はすぐには傾かない
            a = (5 / 7) * grav * sin th' - cBall * v         -- 転がるボール
            v' = v + a * dt / 2
        in (x + v' * dt / 2, v', th')
      (x1, v1, th1) = foldl' sub (wX, wV, wTh) [1, 2 :: Int]
      fell = abs x1 > 1.1             -- 落ちたら中央にもどす
      x = if fell then 0 else x1
      v = if fell then 0 else v1
      th = if fell then 0 else th1
      i = if fell then 0 else i0
  in w { wX = x, wV = v, wTh = th, wI = i, wCmd = cmd, wFell = if fell then wFell + 1 else wFell
       , hX = pushHist hX x, hR = pushHist hR wR, hE = pushHist hE e, hTh = pushHist hTh th }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ seg 6 c50 (px, py) (px, py + 80)      -- 支柱
  , Color c50 (Polygon [(px - 30, py + 80), (px + 30, py + 80), (px, py + 60)])
    -- 板とボール（板の角度だけ回して描く）
  , Translate px py $ Rotate (negate (wTh * 180 / pi)) $ Pictures
      [ seg 8 (hsb 200 40 90) (-l, 0) (l, 0)
      , seg 2 yel (wR * l, -24) (wR * l, -4)
      , dot (wX * l) (-18) 30 (hsb 30 80 100) ]
  , txtC px (py + 110) 14 white ("Beam angle " ++ nf 1 (wTh * 180 / pi) ++ " deg")
  , txtC px (py - 100) 14 yel ("Target r = " ++ nf 2 wR ++ " (click)")
    -- サーボの絵
  , frameBox 3 (hsb 0 0 60) (px + l + 20) (py + 20) 40 50
  , seg 3 (hsb 0 0 60) (px + l + 40, py + 45) (px + l + 40 + 30 * cos (-wCmd * 3), py + 45 + 30 * sin (-wCmd * 3))
    -- グラフ
  , drawPlot gx (ht * 0.18) gw (ht * 0.36) [serS hR 45 90 "Target r", ser hX 30 "Ball x"] (-1.1) 1.1 "Ball position"
  , drawPlot gx (ht * 0.6) gw (ht * 0.25) [ser hTh 200 "Beam angle"] (-thMax * 1.2) (thMax * 1.2) "Control (beam angle [rad])"
  , txt 20 40 18 white ("Kp = " ++ nf 2 wKp ++ " (Q/A)   Ki = " ++ nf 2 wKi ++ " (W/S)   Kd = " ++ nf 2 wKd ++ " (E/D)   Z: Ziegler-Nichols   Falls " ++ show wFell)
  , txt 20 68 15 grayText ("Period Tu ~ " ++ (if period > 0 then nf 2 period ++ " s" else "-") ++ "   Amplitude: " ++ trend)
  , txt 20 90 15 grayText "Z-N method: set Ki = Kd = 0, raise Kp until the amplitude stays constant, then press Z."
  , txt 20 (ht - 30) 15 grayText "P: proportional  I: removes offset  D: brakes the speed. The servo lags, so raising Kp alone finally diverges."
  ]
  where
    wd = envW env
    ht = envH env
    px = wd * 0.26
    py = ht * 0.5
    l = wd * 0.2
    c50 = hsb 0 0 50
    yel = hsb 45 90 100
    gx = wd * 0.52
    gw = wd * 0.44
    grayText = hsb 0 0 60
    (period, trend) = measureOscillation hE

-- 誤差の履歴から、ゆれの周期と振幅の増減を推定（ゼロ交差から周期、山の高さから増減）
measureOscillation :: Hist -> (Float, String)
measureOscillation h =
  let xs = toList h
      idx = zip3 [1 :: Int ..] xs (drop 1 xs)
      crossings = reverse [ i | (i, a, b) <- idx, (a < 0 && b >= 0) || (a > 0 && b <= 0) ]
      peaks = reverse [ abs b | (a, b, c) <- zip3 xs (drop 1 xs) (drop 2 xs)
                              , abs b > abs a, abs b >= abs c, abs b > 0.01 ]
      period = case crossings of
        (c1 : _ : c3 : _) -> fromIntegral (c1 - c3) * dt   -- ゼロ交差 2 回分 = 1 周期
        _ -> 0
      trend = case peaks of
        (b : _ : a : _)
          | b > a * 1.1 -> "growing (Kp too big)"
          | b < a * 0.9 -> "decaying"
          | otherwise -> "constant (here are Ku, Tu)"
        _ -> "-"
  in (period, trend)

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, _) = toP5 env p
        wd = envW env
    in if mx < wd * 0.5 then w { wR = clampF (-0.9) 0.9 (remap mx (wd * 0.06) (wd * 0.46) (-1) 1) } else w
  EventKey (Char c) Down _ _ -> round3 $ case toLower c of
    'q' -> w { wKp = min (wKp w + 0.05) 5 }
    'a' -> w { wKp = max (wKp w - 0.05) 0 }
    'w' -> w { wKi = min (wKi w + 0.05) 5 }
    's' -> w { wKi = max (wKi w - 0.05) 0 }
    'e' -> w { wKd = min (wKd w + 0.05) 5 }
    'd' -> w { wKd = max (wKd w - 0.05) 0 }
    'z' -> let (period, _) = measureOscillation (hE w)
               ku = wKp w
               tu = if period > 0 then period else 3.4
           in w { wKp = 0.6 * ku, wKi = 1.2 * ku / tu, wKd = 0.075 * ku * tu, wI = 0 }  -- ★ Ziegler–Nichols の表
    _ -> w
  EventKey (SpecialKey _) Down _ _ -> round3 w
  _ -> w
  where
    round2 v = fromIntegral (round (v * 100) :: Int) / 100
    round3 x = x { wKp = round2 (wKp x), wKi = round2 (wKi x), wKd = round2 (wKd x) }

-- ---------- やってみよう ----------
-- 1. Ki = Kd = 0 で Kp を 0.3 → 0.5 → 0.7 と上げる。ゆれが「一定」になる Kp（Ku ≈ 0.6）を探して Z
-- 2. Z-N の値から Kd を 2 倍にする／半分にする。どちらが落ち着く？
-- 3. Kd を大きくしすぎると、サーボの遅れとぶつかってかえって細かくゆれる
-- 4. 目標を遠くにクリックしたとき、Ki が大きいと行きすぎる（05 のワインドアップ）

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
