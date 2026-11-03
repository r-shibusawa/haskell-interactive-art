-- ============================================
-- 01 一次遅れ系（ステップ応答と時定数 τ）
-- 水そうに水を入れると、水位は「すぐには」上がらず、だんだん目標に近づく
--   τ y' + y = K u　　→　入力を 1 にすると y は K に向かって指数関数で近づく
--   t = τ のとき、ちょうど最終値の 63.2% に届く
-- 左右キー = 時定数 τ / 上下キー = ゲイン K / 入力は 5 秒ごとに ON・OFF
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

period, dt :: Float
period = 5      -- ★ 入力を切り替える間隔 [秒]
dt = 1 / 60

histMax :: Int
histMax = 600

data World = World
  { wTau   :: !Float   -- ★ 時定数 [秒]（タンクなら 断面積 × 排水のしにくさ、RC 回路なら R × C）
  , wK     :: !Float   -- ★ ゲイン（入力 1 に対して最終的に y がいくつになるか）
  , wY     :: !Float   -- 出力（水位）
  , wU     :: !Float   -- 入力（0 か 1）
  , wT     :: !Float   -- 時刻
  , wTStep :: !Float   -- 入力を切り替えた時刻
  , wYStep :: !Float   -- そのときの出力
  , hY, hU, hT :: !Hist
  }

world0 :: World
world0 = World 1 1 0 1 0 0 0 Seq.empty Seq.empty Seq.empty

main :: IO ()
main = runSketch (sketch "20-01 First-Order System" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

step :: World -> World
step w@World{..} =
  let y = wY + ((wK * wU - wY) / wTau) * dt   -- ★ 一次遅れの式：y の変化 = (K u − y) / τ
      t = wT + dt
      flipNow = t - wTStep >= period          -- 入力を切り替え
      u = if flipNow then 1 - wU else wU
  in w { wY = y, wT = t, wU = u
       , wTStep = if flipNow then t else wTStep
       , wYStep = if flipNow then y else wYStep
       , hY = pushHist hY y, hU = pushHist hU u, hT = pushHist hT t }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ -- 入口のパイプとバルブ
    seg 10 (hsb 0 0 60) (mid, ty - 60) (mid, ty - 10)
  , box (mid - 20) (ty - 90) 40 24 (if on then hsb 200 80 100 else hsb 0 0 40)
  , txtC mid (ty - 100) 14 white ("Input u = " ++ show (round wU :: Int) ++ (if on then " (filling)" else " (stopped)"))
    -- 注ぐ水
  , if on then box (mid - 4) (ty - 10) 8 (th - remap wY 0 maxLevel 0 th + 4) (hsba 200 70 100 0.7) else Blank
    -- 水
  , box tx (ty + th - lvl) tw lvl (hsba 200 70 90 0.7)
    -- 最終値（K u）の線
  , seg 2 (hsb 45 90 100) (tx - 10, yFinal) (tx + tw + 10, yFinal)
    -- タンクの枠
  , Color (hsb 0 0 70) (thickPath 4 [(tx, ty), (tx, ty + th), (tx + tw, ty + th), (tx + tw, ty)])
    -- 排水
  , seg 10 (hsb 0 0 60) (tx + tw, ty + th - 20) (tx + tw + 50, ty + th - 20)
  , box (tx + tw + 50) (ty + th - 24) 8 (10 + wY * 20) (hsba 200 70 100 0.6)
  , txt (tx + 10) (ty + th - lvl - 10) 20 white ("y = " ++ nf 3 wY)
  , txt (tx + tw + 14) (yFinal + 5) 14 (hsb 45 90 100) ("K*u = " ++ nf 2 (wK * wU))
    -- グラフ（右）
  , drawPlot gx gy gw gh [serS hU 0 0 "Input u", ser hY 200 "Output y"] (-0.1) hi "Time (right = now, 10 s wide)"
  , marks
  , txt 20 40 18 white ("tau = " ++ nf 2 wTau ++ " s (L/R)   K = " ++ nf 2 wK ++ " (Up/Down)   Input switches every 5 s")
  , txt 20 68 15 grayText "tau y' + y = K u. Larger tau: slower. Larger K: higher final value. 3 tau: 95%, 4 tau: 98%."
  , txt 20 (ht - 30) 15 grayText "Tank: tau = area x drain resistance.  RC: tau = R x C.  Room heat, motor speed, cooling coffee: same equation."
  ]
  where
    wd = envW env
    ht = envH env
    tx = wd * 0.08
    ty = ht * 0.25
    tw = wd * 0.3
    th = ht * 0.5
    mid = tx + tw * 0.5
    maxLevel = max 1.3 (wK * 1.3)
    on = wU > 0.5
    lvl = remap (clampF 0 maxLevel wY) 0 maxLevel 0 th
    yFinal = ty + th - remap (clampF 0 maxLevel (wK * wU)) 0 maxLevel 0 th
    gx = wd * 0.5
    gy = ht * 0.2
    gw = wd * 0.46
    gh = ht * 0.45
    hi = max 1.3 (wK * 1.3)
    grayText = hsb 0 0 60
    -- τ の位置（切り替えから τ 秒後）に印
    xOf i = remap (fromIntegral i) 0 (fromIntegral histMax - 1) gx (gx + gw)
    yOf v = remap (clampF (-0.1) hi v) (-0.1) hi (gy + gh) gy
    marks = case Seq.findIndexL (>= wTStep) hT of
      Nothing -> Blank
      Just iStep -> Pictures
        [ seg 1 (hsba 45 90 100 0.6) (xOf iStep, gy) (xOf iStep, gy + gh)
        , case Seq.findIndexL (>= wTStep + wTau) hT of
            Nothing -> Blank
            Just iTau ->
              let y63 = wYStep + 0.632 * (wK * wU - wYStep)
                  c = hsb 45 90 100
              in Pictures
                   [ seg 2 c (xOf iTau, gy + gh) (xOf iTau, yOf y63)
                   , seg 2 c (xOf iStep, yOf y63) (xOf iTau, yOf y63)
                   , dot (xOf iTau) (yOf y63) 10 c
                   , txt (xOf iTau + 6) (yOf y63 - 8) 14 c "t = tau: 63.2%" ]
        ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTau = min (wTau w * 1.25) 10 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTau = max (wTau w / 1.25) 0.1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wK = min (wK w + 0.25) 3 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wK = max (wK w - 0.25) 0.25 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. τ を 0.2 と 4 で比べる。入力が切り替わった瞬間の「傾き」はどう変わる？（傾き = K u / τ）
-- 2. 63.2% の印が、どんな τ でもグラフの同じ「形」の場所につくことを確かめる
-- 3. period を 1 にして τ = 2 にする。追いつけずに三角波のようになる（→ 07 の周波数応答）
-- 4. 入力を 0/1 の切り替えではなく、マウスの高さにしてみよう（skEvent で wU を書きかえる）

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
