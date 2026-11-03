-- ============================================
-- 00 開ループと閉ループ
-- 同じ部屋を 2 つ並べて暖める
--   左 = 開ループ：ヒーターの強さを「計算で決め打ち」して放っておく
--   右 = 閉ループ：温度を測って、目標より低ければ ON・高ければ OFF（サーモスタット）
-- 窓を開ける（外乱）と、2 つの差がはっきり出る
-- マウス左右 = 窓の開き具合 / 上下キー = 目標温度 / R でやり直し
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

outside, pMax, kWall, kWin, speed, hyst :: Float
outside = 5      -- ★ 外の気温 [℃]
pMax = 3.0       -- ★ ヒーターの最大出力
kWall = 0.1      -- 壁から逃げる熱（温度差 1 ℃ あたり）
kWin = 0.5       -- ★ 窓を全開にしたとき逃げる熱
speed = 3        -- ★ シミュレーションの速さ（1 = 実時間）
hyst = 0.3       -- ★ サーモスタットのあそび（この幅で ON/OFF）

histMax :: Int
histMax = 600

data World = World
  { wTarget :: !Float   -- 目標温度 [℃]
  , wOpen   :: !Float   -- 開ループの部屋の温度
  , wClosed :: !Float   -- 閉ループの部屋の温度
  , wPo     :: !Float   -- 開ループのヒーター出力
  , wPc     :: !Float   -- 閉ループのヒーター出力
  , wHeater :: !Bool    -- サーモスタットの ON/OFF
  , wWin    :: !Float   -- 窓の開き具合 0〜1
  , hOpen, hClosed, hTarget, hPo, hPc :: !Hist
  }

world0 :: World
world0 = World
  { wTarget = 22, wOpen = outside, wClosed = outside, wPo = 0, wPc = 0, wHeater = False, wWin = 0
  , hOpen = Seq.empty, hClosed = Seq.empty, hTarget = Seq.empty, hPo = Seq.empty, hPc = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-00 Open and Closed Loop" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

step :: World -> World
step w@World{..} =
  let dt = speed / 60
      -- 開ループ：目標に届くはずの出力を計算して固定（窓のことは知らない）
      po = clampF 0 pMax (kWall * (wTarget - outside))   -- ★ 計算で決めた一定値
      -- 閉ループ：測った温度で ON/OFF
      on1 = wHeater || wClosed < wTarget - hyst
      on2 = on1 && not (wClosed > wTarget + hyst)
      pc = if on2 then pMax else 0
      -- ★ 部屋の温度の変化：（ヒーター − 壁から逃げる熱 − 窓から逃げる熱）
      loss = kWall + kWin * wWin
      tO = clampF (-20) 60 (wOpen + (po - loss * (wOpen - outside)) * dt)
      tC = clampF (-20) 60 (wClosed + (pc - loss * (wClosed - outside)) * dt)
  in w { wPo = po, wPc = pc, wHeater = on2, wOpen = tO, wClosed = tC
       , hOpen = pushHist hOpen tO, hClosed = pushHist hClosed tC, hTarget = pushHist hTarget wTarget
       , hPo = pushHist hPo po, hPc = pushHist hPc pc }

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures
  [ drawRoom w (wd * 0.04) ry rw rh wOpen wPo "Open loop" 200
  , drawRoom w (wd * 0.28) ry rw rh wClosed wPc "Closed loop" 30
  , drawPlot gx (ht * 0.18) gw (ht * 0.36)
      [serS hTarget 0 0 "Target", ser hOpen 200 "Open", ser hClosed 30 "Closed"] 0 40 "Temperature [C]"
  , drawPlot gx (ht * 0.6) gw (ht * 0.25)
      [ser hPo 200 "Open", ser hPc 30 "Closed"] (-0.2) (pMax * 1.2) "Heater power"
  , txt 20 40 18 white ("Target " ++ nf 0 wTarget ++ " C (Up/Down)   Window " ++ nf 0 (wWin * 100) ++ "% (mouse L/R)   R: restart")
  , txt 20 68 15 grayText "Open loop fixes the heater power by calculation; an open window (disturbance) makes it fall short."
  , txt 20 90 15 grayText "Closed loop measures the temperature, so it stays near the target despite the disturbance."
  , txt (wd * 0.04) (ry + rh + 60) 15 (hsb 200 60 100) ("Open-loop error " ++ nf 1 (wTarget - wOpen) ++ " C")
  , txt (wd * 0.28) (ry + rh + 60) 15 (hsb 30 80 100) ("Closed-loop error " ++ nf 1 (wTarget - wClosed) ++ " C")
  ]
  where
    wd = envW env
    ht = envH env
    rw = wd * 0.2
    rh = ht * 0.4
    ry = ht * 0.28
    gx = wd * 0.54
    gw = wd * 0.43
    grayText = hsb 0 0 60

-- 部屋：壁・窓・ヒーター・温度計
drawRoom :: World -> Float -> Float -> Float -> Float -> Float -> Float -> String -> Float -> Picture
drawRoom World{..} x y w h t p name hue = Pictures
  [ frameBox 3 (hsb 0 0 50) x y w h
    -- 窓（右の壁に開く）
  , box (x + w - 8) (y + h * 0.2) 16 (h * 0.3) (hsba 200 30 90 0.4)
  , box (x + w - 8) (y + h * 0.2) 16 (h * 0.3 * wWin) (hsb 230 30 10)   -- 開いた分だけ穴
    -- ヒーター（出力に応じて赤く光る）
  , box (x + 20) (y + h - 40) (w - 40) 24 (hsba 10 90 100 (remap p 0 pMax 0 1))
  , frameBox 2 (hsb 0 0 60) (x + 20) (y + h - 40) (w - 40) 24
    -- 部屋の中の色（温度）
  , box (x + 3) (y + 3) (w - 6) (h - 6) (hsba (clampF 0 360 (remap t outside 30 220 0)) 60 80 0.2)
    -- 温度計
  , seg 8 (hsb 0 0 40) (tx, t0) (tx, t1)
  , seg 6 (hsb hue 80 100) (tx, t0) (tx, remap (clampF 0 40 t) 0 40 t0 t1)
  , seg 1 (hsb 0 0 70) (tx - 20, ty) (tx + 20, ty)
  , txtC tx (y + h + 30) 22 white (nf 1 t ++ " C")
  , txtC tx (y - 14) 18 (hsb hue 60 100) name
  ]
  where
    tx = x + w * 0.5
    t0 = y + h * 0.7
    t1 = y + 20
    ty = remap wTarget 0 40 t0 t1

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventMotion p -> setWin p
  EventKey (MouseButton LeftButton) Down _ p -> setWin p
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTarget = min (wTarget w + 1) 35 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTarget = max (wTarget w - 1) 10 }
  EventKey (Char c) Down _ _ | toLower c == 'r' -> world0 { wTarget = wTarget w, wWin = wWin w }
  _ -> w
  where setWin p = w { wWin = clampF 0 1 (fst (toP5 env p) / envW env) }

-- ---------- やってみよう ----------
-- 1. 窓を閉めたまま待つ。開ループもちゃんと目標に届く？（モデルが正しければ届く）
-- 2. 窓を半分開ける。開ループは何度まで下がる？ 閉ループは？
-- 3. hyst（あそび）を 2 にすると閉ループの温度はどうゆれる？ 0.05 にすると ON/OFF の回数は？
-- 4. 外気温 outside を 25 にして、目標 22 に「冷やす」には何が必要？

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
