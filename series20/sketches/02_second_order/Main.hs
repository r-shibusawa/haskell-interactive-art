-- ============================================
-- 02 二次系と減衰（バネ・マス・ダンパ）
-- おもりをバネで引っぱると、行きすぎて（オーバーシュート）ゆれながら落ち着く
--   y'' + 2ζωn y' + ωn² y = ωn² r
--   ζ（減衰比）が小さいとゆれる、1 でちょうど、1 より大きいとのんびり
-- 左右キー = ζ / 上下キー = ωn（固有角周波数）/ スペース = もう一度ステップ
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

period, dt :: Float
period = 6      -- ★ 目標を切り替える間隔 [秒]
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wZeta   :: !Float   -- ★ 減衰比 ζ（0 = ゆれ続ける、1 = 臨界減衰）
  , wWn     :: !Float   -- ★ 固有角周波数 ωn [rad/s]（大きいほど速い）
  , wY, wV, wR, wT, wTStep :: !Float
  , wPeak   :: !Float   -- 行きすぎの最大値
  , wSettle :: !Float   -- 2% の帯に入りきるまでの時間
  , hY, hR  :: !Hist
  }

world0 :: World
world0 = World 0.3 3 0 0 1 0 0 0 0 Seq.empty Seq.empty

main :: IO ()
main = runSketch (sketch "20-02 Second-Order System" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

step :: World -> World
step w@World{..} =
  let -- ★ 二次系の式を細かく 4 回に分けて積分（安定のため）
      sub (y, v) _ =
        let a = wWn * wWn * (wR - y) - 2 * wZeta * wWn * v
            v' = v + a * dt / 4
        in (y + v' * dt / 4, v')
      (y0, v0) = foldl' sub (wY, wV) [1 .. 4 :: Int]
      y = clampF (-5) 5 y0
      v = clampF (-100) 100 v0
      t = wT + dt
      e = wR - y
      peak = if wR > 0.5 && y - 1 > wPeak then y - 1 else wPeak
      settle = if abs e > 0.02 then t - wTStep else wSettle
      w1 = w { wY = y, wV = v, wT = t, wPeak = peak, wSettle = settle }
      next = t - wTStep >= period
      r = if next then 1 - wR else wR
      w2 = if next then flipTarget w1 else w1
  in w2 { hY = pushHist hY y, hR = pushHist hR r }

-- 目標を 0 ⇔ 1 に切り替える
flipTarget :: World -> World
flipTarget w = w { wR = 1 - wR w, wTStep = wT w, wPeak = 0, wSettle = 0 }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ seg 6 (hsb 0 0 50) (wallX, baseY - 120) (wallX, baseY + 120)   -- 壁
  , seg 2 (hsb 0 0 40) (wallX, baseY + 60) (wd * 0.46, baseY + 60) -- 床
    -- バネ（ジグザグ）
  , Color (hsb 200 60 100) (thickPath 3 spring)
    -- ダンパ（筒とピストン）
  , seg 3 c70 (wallX, baseY + 30) (wallX + 70, baseY + 30)
  , frameBox 3 c70 (wallX + 70) (baseY + 16) 60 28
  , seg 3 c70 (pistonX, baseY + 18) (pistonX, baseY + 42)
  , seg 3 c70 (pistonX, baseY + 30) (mx - 30, baseY + 30)
    -- 目標の線
  , seg 2 yel (rx, baseY - 100) (rx, baseY + 60)
    -- おもり
  , box (mx - 30) (baseY - 60) 60 120 (hsb 30 80 100)
  , txtC mx (baseY + 90) 14 white ("y = " ++ nf 2 wY)
  , txtC rx (baseY - 110) 14 yel ("Target r = " ++ show (round wR :: Int))
    -- グラフ
  , drawPlot gx gy gw gh [serS hR 45 90 "Target r", ser hY 30 "Output y"] (-0.6) 2 "Time (right = now, 10 s wide)"
    -- ±2% の帯
  , box gx (yOf 1.02) gw (yOf 0.98 - yOf 1.02) (hsba 45 90 100 0.12)
  , txt 20 40 18 white ("zeta = " ++ nf 2 wZeta ++ " (L/R)   wn = " ++ nf 1 wWn ++ " (Up/Down)   " ++ kind)
  , txt 20 68 15 grayText ("Overshoot " ++ nf 1 (wPeak * 100) ++ "% (theory " ++ nf 1 osTheory ++ "%)   Settling (+/-2%) "
        ++ nf 2 wSettle ++ " s (rule 4/(zeta wn) = " ++ nf 2 (4 / (wZeta * wWn + 1e-9)) ++ " s)")
  , txt 20 (ht - 30) 15 grayText "y'' + 2 zeta wn y' + wn^2 y = wn^2 r.  Car suspension, door closer, robot joint: two numbers set the motion."
  ]
  where
    wd = envW env
    ht = envH env
    wallX = wd * 0.06
    baseY = ht * 0.5
    scaleX = wd * 0.18
    mx = wallX + 60 + scaleX * (0.5 + wY)   -- おもりの位置
    rx = wallX + 60 + scaleX * (0.5 + wR)
    n = 12 :: Int
    spring = (wallX, baseY - 30)
      : [ (remap (fromIntegral i) 0 (fromIntegral n) wallX (mx - 30), baseY - 30 + (if odd i then -14 else 14)) | i <- [1 .. n - 1] ]
      ++ [(mx - 30, baseY - 30)]
    pistonX = wallX + 70 + clampF 5 55 (remap wY (-0.5) 1.5 10 50)
    c70 = hsb 0 0 70
    yel = hsb 45 90 100
    gx = wd * 0.5
    gy = ht * 0.2
    gw = wd * 0.46
    gh = ht * 0.45
    yOf v = remap v (-0.6) 2 (gy + gh) gy
    kind | wZeta < 1 = "Underdamped (oscillates)"
         | wZeta == 1 = "Critically damped (fastest settling)"
         | otherwise = "Overdamped (no shaking, but slow)"
    osTheory = if wZeta < 1 then exp (-pi * wZeta / sqrt (1 - wZeta * wZeta)) * 100 else 0
    grayText = hsb 0 0 60

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wZeta = min (round1 (wZeta w + 0.1)) 2.5 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wZeta = max (round1 (wZeta w - 0.1)) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wWn = min (wWn w + 0.5) 12 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wWn = max (wWn w - 0.5) 0.5 }
  EventKey (SpecialKey KeySpace) Down _ _ -> flipTarget w
  _ -> w
  where round1 v = fromIntegral (round (v * 10) :: Int) / 10

-- ---------- やってみよう ----------
-- 1. ζ = 0 にする。ゆれが止まらない（減衰なし）。ωn を変えるとゆれの速さだけ変わる
-- 2. ζ = 0.7 がよく使われる「ちょうどいい」値。オーバーシュートは何 % ？
-- 3. ωn を 2 倍にすると整定時間は何倍？（4/(ζωn) を確かめる）
-- 4. ζ = 1 と ζ = 2 で、どちらが先に ±2% に入る？ 「ゆれない = 速い」ではない

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
