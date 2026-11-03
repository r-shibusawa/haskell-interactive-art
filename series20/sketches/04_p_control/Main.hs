-- ============================================
-- 04 P 制御（比例制御）
-- 台車を目標の位置まで動かす。「ずれ（誤差）に比例した力」を出すだけの制御
--   u = Kp × (目標 − いまの位置)
--   風（一定の外乱）が吹くと、ずれが残ったまま止まる = 定常偏差
-- クリック = 目標の位置 / 左右キー = Kp / 上下キー = 風の強さ
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

mass, friction, uMax, dt :: Float
mass = 1          -- 台車の質量
friction = 1.5    -- 摩擦
uMax = 20         -- ★ モーターの最大の力
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wKp   :: !Float   -- ★ 比例ゲイン
  , wWind :: !Float   -- ★ 一定の外乱（風の力）
  , wX, wV, wR, wU, wE :: !Float
  , hX, hR, hU :: !Hist
  }

world0 :: World
world0 = World 2 0 0 0 2 0 2 Seq.empty Seq.empty Seq.empty

main :: IO ()
main = runSketch (sketch "20-04 P Control" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

step :: World -> World
step w@World{..} =
  let e = wR - wX                              -- ★ P 制御：誤差に比例した力
      u = clampF (-uMax) uMax (wKp * e)
      sub (x, v) _ =                           -- 台車の運動：m x'' = u − c x' + 風
        let a = (u - friction * v + wWind) / mass
            v' = v + a * dt / 2
        in (x + v' * dt / 2, v')
      (x0, v0) = foldl' sub (wX, wV) [1, 2 :: Int]
      x = clampF (-5) 5 x0
      v = clampF (-50) 50 v0
  in w { wX = x, wV = v, wU = u, wE = e
       , hX = pushHist hX x, hR = pushHist hR wR, hU = pushHist hU u }

railX0, railX1, railY :: Env -> Float
railX0 env = envW env * 0.05
railX1 env = envW env * 0.47
railY env = envH env * 0.5

wx :: Env -> Float -> Float
wx env v = remap v (-5) 5 (railX0 env) (railX1 env)

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ seg 4 (hsb 0 0 50) (x0, ry + 30) (x1, ry + 30) ]
  ++ [ seg 1 (hsb 0 0 30) (wx env i, ry + 30) (wx env i, ry + 40) | i <- [-5 .. 5] ]
  ++ [ txtC (wx env i) (ry + 56) 14 (hsb 0 0 60) (show (round i :: Int)) | i <- [-4, -2, 0, 2, 4] ]
  ++
  [ -- 目標
    seg 2 yel (wx env wR, ry - 80) (wx env wR, ry + 30)
  , txtC (wx env wR) (ry - 90) 14 yel ("Target r = " ++ nf 2 wR)
  , if abs wWind > 0.01 then windArrows else Blank
    -- 台車
  , box (cx - 40) (ry - 30) 80 50 (hsb 30 80 100)
  , dot (cx - 22) (ry + 24) 20 (hsb 0 0 30)
  , dot (cx + 22) (ry + 24) 20 (hsb 0 0 30)
    -- 力の矢印
  , seg 5 (hsb (if wU > 0 then 120 else 0) 80 100) (cx, ry - 5) (cx + wU * 5, ry - 5)
  , txtC cx (ry - 40) 14 white ("x = " ++ nf 2 wX)
    -- グラフ
  , drawPlot gx (ht * 0.18) gw (ht * 0.36) [serS hR 45 90 "Target r", ser hX 30 "Position x"] (-5) 5 "Position"
  , drawPlot gx (ht * 0.6) gw (ht * 0.25) [ser hU 120 "Force u"] (-uMax) uMax "Control input u = Kp (r - x)"
  , txt 20 40 18 white ("Kp = " ++ nf 2 wKp ++ " (L/R)   Wind = " ++ nf 1 wWind ++ " (Up/Down)   Click: target   Error " ++ nf 2 wE)
  , txt 20 68 15 grayText ("u = Kp x error. Force weakens near the target. Balancing wind needs an error: offset e = -wind/Kp = " ++ nf 2 (negate wWind / wKp))
  , txt 20 (ht - 30) 15 grayText "Higher Kp: smaller offset but more shaking (smaller zeta, see 02). To remove the offset: I control, see 05."
  ]
  where
    wd = envW env
    ht = envH env
    x0 = railX0 env
    x1 = railX1 env
    ry = railY env
    cx = wx env wX
    yel = hsb 45 90 100
    gx = wd * 0.52
    gw = wd * 0.44
    grayText = hsb 0 0 60
    -- 風
    windArrows = Pictures
      [ Pictures [ seg 2 c (xa, yy) (xa + len, yy)
                 , seg 2 c (xa + len, yy) (xa + len - 6 * s, yy - 5)
                 , seg 2 c (xa + len, yy) (xa + len - 6 * s, yy + 5) ]
      | i <- [0 .. 3]
      , let yy = ry - 60 + i * 25
            len = wWind * 12
            s = signum wWind
            xa = if wWind > 0 then x0 + 20 + i * 15 else x1 - 20 - i * 15
            c = hsba 200 50 100 0.7 ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        ry = railY env
    in if mx < railX1 env + 20 && my > ry - 150 && my < ry + 80
         then w { wR = clampF (-4.5) 4.5 (remap mx (railX0 env) (railX1 env) (-5) 5) }
         else w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wKp = min (wKp w * 1.3) 50 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wKp = max (wKp w / 1.3) 0.2 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wWind = min (wWind w + 1) 5 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wWind = max (wWind w - 1) (-5) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 風 0 で Kp を 0.5 → 2 → 10 → 30 と上げる。速くなる？ ゆれる？
-- 2. 風を 3 にする。Kp を上げると偏差は減るが、ゼロにはならないことを確かめる
-- 3. uMax（力の上限）を 3 にすると何が起きる？（飽和）
-- 4. 摩擦 friction を 0 にすると、Kp をどう選んでもゆれ続ける（減衰なし → D 制御が必要）

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
