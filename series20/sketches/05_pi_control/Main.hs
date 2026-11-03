-- ============================================
-- 05 PI 制御（積分で偏差を消す）
-- 04 の台車に「誤差をためる」項を足す
--   u = Kp e + Ki ∫e dt　　誤差が残っているかぎり積分が増え続け、風に負けなくなる
-- ただしモーターの力には上限がある。遠い目標を指すと積分がたまりすぎて行きすぎる = ワインドアップ
-- クリック = 目標 / 左右キー = Kp / 上下キー = Ki / D = 風 ON/OFF / W = アンチワインドアップ ON/OFF
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

mass, friction, uMax, dt :: Float
mass = 1
friction = 1.5
uMax = 5          -- ★ モーターの最大の力（小さめにして飽和を見やすく）
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wKp   :: !Float   -- ★ 比例ゲイン
  , wKi   :: !Float   -- ★ 積分ゲイン
  , wAnti :: !Bool    -- ★ アンチワインドアップ（W で切り替え）
  , wWind :: !Float
  , wX, wV, wR, wU, wI, wUP, wUI, wE :: !Float
  , wSat  :: !Bool    -- 飽和中か
  , hX, hR, hU, hI :: !Hist
  }

world0 :: World
world0 = World
  { wKp = 2, wKi = 1, wAnti = False, wWind = 0, wX = 0, wV = 0, wR = 2, wU = 0, wI = 0, wUP = 0, wUI = 0, wE = 2
  , wSat = False, hX = Seq.empty, hR = Seq.empty, hU = Seq.empty, hI = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-05 PI Control" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

step :: World -> World
step w@World{..} =
  let e = wR - wX                  -- ★ PI 制御
      uRaw = wKp * e + wKi * wI
      u = clampF (-uMax) uMax uRaw
      sat = abs uRaw > uMax
      -- 積分：アンチワインドアップ ON なら「飽和中に、さらに飽和させる向き」には積まない
      hold = wAnti && sat && signum e == signum uRaw
      i = clampF (-50) 50 (if hold then wI else wI + e * dt)
      uI = wKi * i
      sub (x, v) _ =
        let a = (u - friction * v + wWind) / mass
            v' = v + a * dt / 2
        in (x + v' * dt / 2, v')
      (x0, v0) = foldl' sub (wX, wV) [1, 2 :: Int]
      x = clampF (-5) 5 x0
      v = clampF (-50) 50 v0
  in w { wX = x, wV = v, wU = u, wI = i, wUP = wKp * e, wUI = uI, wE = e, wSat = sat
       , hX = pushHist hX x, hR = pushHist hR wR, hU = pushHist hU u, hI = pushHist hI uI }

railX0, railX1, railY :: Env -> Float
railX0 env = envW env * 0.05
railX1 env = envW env * 0.47
railY env = envH env * 0.5

wx :: Env -> Float -> Float
wx env v = remap v (-5) 5 (railX0 env) (railX1 env)

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ -- レールと台車
    seg 4 (hsb 0 0 50) (x0, ry + 30) (railX1 env, ry + 30)
  , seg 2 yel (wx env wR, ry - 80) (wx env wR, ry + 30)
  , txtC (wx env wR) (ry - 90) 14 yel ("Target r = " ++ nf 2 wR)
  , if abs wWind > 0.01
      then Pictures [ seg 2 (hsba 200 50 100 0.7) (x0 + 20 + i * 15, yy) (x0 + 20 + i * 15 + wWind * 12, yy)
                    | i <- [0 .. 3], let yy = ry - 60 + i * 25 ]
      else Blank
  , box (cx - 40) (ry - 30) 80 50 (hsb 30 80 100)
  , dot (cx - 22) (ry + 24) 20 (hsb 0 0 30)
  , dot (cx + 22) (ry + 24) 20 (hsb 0 0 30)
  , seg 5 (hsb (if wSat then 0 else 120) 80 100) (cx, ry - 5) (cx + wU * 12, ry - 5)
  , txtC cx (ry - 40) 14 white ("x = " ++ nf 2 wX)
    -- 制御入力の内訳（棒グラフ）
  , drawBar bx by bw "P term Kp*e" wUP 200
  , drawBar bx (by + 34) bw "I term Ki*int(e)" wUI 300
  , drawBar bx (by + 68) bw ("Output u (limit +/-" ++ nf 0 uMax ++ ")") wU (if wSat then 0 else 120)
  , if wSat
      then txt (bx + bw * 0.6) (by + 62) 14 (hsb 0 80 100) (if wAnti then "Saturated! (I is held)" else "Saturated! (I keeps growing)")
      else Blank
    -- グラフ
  , drawPlot gx (ht * 0.18) gw (ht * 0.36) [serS hR 45 90 "Target r", ser hX 30 "Position x"] (-5) 5 "Position"
  , drawPlot gx (ht * 0.6) gw (ht * 0.25) [ser hU 120 "Output u", ser hI 300 "I term"] (-uMax * 2) (uMax * 2) "Control input"
  , txt 20 40 18 white ("Kp = " ++ nf 2 wKp ++ " (L/R)   Ki = " ++ nf 2 wKi ++ " (Up/Down)   Wind " ++ (if wWind /= 0 then "ON" else "OFF")
        ++ " (D)   Anti-windup " ++ (if wAnti then "ON" else "OFF") ++ " (W)   Error " ++ nf 2 wE)
  , txt 20 68 15 grayText "The I term grows while any error remains, so it finds the force that balances the wind (zero offset)."
  , txt 20 90 15 grayText "A far target: I builds up during saturation, then the cart overshoots."
  , txt 20 (ht - 30) 15 grayText "W: anti-windup ON stops integrating while saturated. Compare: the overshoot disappears."
  ]
  where
    wd = envW env
    ht = envH env
    x0 = railX0 env
    ry = railY env
    cx = wx env wX
    yel = hsb 45 90 100
    bx = wd * 0.05
    by = ht * 0.72
    bw = wd * 0.4
    gx = wd * 0.52
    gw = wd * 0.44
    grayText = hsb 0 0 60

drawBar :: Float -> Float -> Float -> String -> Float -> Float -> Picture
drawBar x y w name val hue = Pictures
  [ box x y w 22 (hsb 0 0 20)
  , box (min mid (mid + len)) y (abs len) 22 (hsb hue 70 100)
  , seg 1 (hsb 0 0 60) (mid, y) (mid, y + 22)
  , txt (x + 6) (y - 5) 14 white (name ++ " = " ++ nf 2 val)
  ]
  where
    mid = x + w / 2
    len = clampF (-w / 2) (w / 2) (val / (uMax * 2) * (w / 2))

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
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wKi = min (wKi w + 0.25) 10 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wKi = max (wKi w - 0.25) 0 }
  EventKey (Char c) Down _ _
    | toLower c == 'd' -> w { wWind = if wWind w /= 0 then 0 else 3 }
    | toLower c == 'w' -> w { wAnti = not (wAnti w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 風 ON で Ki = 0（P だけ）→ 偏差が残る。Ki を 1 にすると I 項がじわじわ増えて偏差が消える
-- 2. 目標を一番遠くにクリック（W は OFF）→ 行きすぎる。I 項のグラフを見る。W を ON にして同じことを
-- 3. Ki を大きくしすぎるとどうなる？（積分はゆれを増やす。極が右へ寄る）
-- 4. uMax を 50 にするとワインドアップは起きる？

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
