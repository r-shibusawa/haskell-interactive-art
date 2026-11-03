-- ============================================
-- 13 制御を聞く（PID で音の高さを追いかける）
-- 目標の音の高さが 2 秒ごとに飛ぶ。制御器の出力が音の高さになる
--   プラントは「遅れのあるモーター」のような 2 段の一次遅れ。追いつくまでの時間・行きすぎ・ゆれが「音」で聞こえる
--   P だけだと目標より低いまま（定常偏差）、I を足すと届く、上げすぎるとビブラートのようにゆれる
-- クリックで開始 / 左右キー = Kp / 上下キー = Ki / A・Z = Kd / 1〜3 = プリセット / T = 目標の音も鳴らす
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Control.Monad (when)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

tauPlant, interval, dt, noteLen, noteVol, targetVol :: Float
tauPlant = 0.12    -- ★ プラントの遅れ [秒]（2 段）
interval = 2.0     -- ★ 目標を変える間隔 [秒]
dt = 1 / 60
noteLen = 0.22     -- ★ 短い音 1 つの長さ [秒]（連続音の代わりに短い音を並べる）
noteVol = 0.2      -- ★ 出力の音の大きさ
targetVol = 0.1    -- ★ 目標の音の大きさ

noteEvery :: Int
noteEvery = 12     -- ★ 何フレームごとに短い音を鳴らすか

notes :: [Float]
notes = [262, 330, 392, 523, 440, 294, 659, 349]   -- ★ 目標の音の高さ [Hz]（ド・ミ・ソ・高いド…）

histMax :: Int
histMax = 600

data World = World
  { wKp, wKi, wKd :: !Float   -- ★ PID ゲイン
  , wStarted :: !Bool         -- クリックで音を開始
  , wTarget  :: !Bool         -- 目標の音も鳴らすか
  , wR, wF, wY1, wU, wI, wPrevF, wT, wE :: !Float
  , wNote :: !Int
  , hF, hR :: !Hist
  }

world0 :: World
world0 = World
  { wKp = 3, wKi = 8, wKd = 0.05, wStarted = False, wTarget = False
  , wR = 392, wF = 392, wY1 = 392, wU = 392, wI = 392 / 8, wPrevF = 392, wT = 0, wE = 0
  , wNote = 0, hF = Seq.empty, hR = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-13 Hear the Control" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \env _ w -> do
      let w' = step w
      when (wStarted w' && envFrame env `mod` noteEvery == 0) $ do
        playTone (hzOut w') noteVol noteLen Sawtooth
        when (wTarget w') (playTone (wR w') targetVol noteLen Sine)
      pure w'
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent ev w)
  }

-- 実際に鳴らす高さ
hzOut :: World -> Float
hzOut w = clampF 60 2000 (wF w)

step :: World -> World
step w@World{..} =
  let t0 = wT + dt
      next = t0 >= interval
      t = if next then 0 else t0
      idx = if next then (wNote + 1) `mod` length notes else wNote
      r = if next then noteAt idx else wR
      -- ★ PID（誤差は Hz のまま。ゲインは「Hz あたり」。微分は出力の変化にかける）
      e = r - wF
      i = clampF (-5000) 5000 (wI + e * dt)
      dF = (wF - wPrevF) / dt
      u = clampF 0 4000 (wKp * e + wKi * i - wKd * dF)   -- 指令（Hz）
      -- ★ プラント：2 段の一次遅れ（指令 u に遅れて周波数 f が追いつく）
      sub (y1, f) _ =
        let y1' = y1 + ((u - y1) / tauPlant) * dt / 2    -- 1 段目
        in (y1', f + ((y1' - f) / tauPlant) * dt / 2)    -- 2 段目 → 実際に鳴る周波数
      (y1n, f0) = foldl' sub (wY1, wF) [1, 2 :: Int]
      f = clampF 0 4000 f0
  in w { wT = t, wNote = idx, wR = r, wE = e, wI = i, wPrevF = wF, wU = u, wY1 = y1n, wF = f
       , hF = pushHist hF f, hR = pushHist hR r }
  where noteAt k = case drop k notes of { (n : _) -> n; [] -> 392 }

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures $
  [ -- 左：音の高さのメーター
    seg 14 (hsb 0 0 30) (mx, my0) (mx, my1)
  , seg 3 yel (mx - 40, yOf wR) (mx + 40, yOf wR)
  , txt (mx + 48) (yOf wR + 5) 14 yel ("Target " ++ nf 0 wR ++ " Hz")
  , dot mx (yOf wF) 26 (hsb 30 80 100)
  , txt (mx + 48) (yOf wF + 24) 14 (hsb 30 80 100) ("Now " ++ nf 0 fHz ++ " Hz")
    -- 回る円盤（モーターのイメージ）
  , Color (hsb 200 50 90) (at (dx, dy) (ring rad 3))
  ]
  ++ [ seg 3 (hsb 200 50 90) (dx, dy) (dx + rad * cos a, dy + rad * sin a)
     | i <- [1 .. 6 :: Int], let a = angle + fromIntegral i * pi / 3 ]
  ++
  [ txtC dx (dy + rad + 24) 14 (hsb 0 0 70) "Motor with lag (speed = pitch)"
    -- グラフ
  , drawPlot gx (ht * 0.2) gw (ht * 0.45) [serS hR 45 90 "Target", ser hF 30 "Output (sounding)"] 150 850 "Pitch [Hz]"
  , txt 20 40 18 white ((if wStarted then "" else "Click: start  ") ++ "Kp " ++ nf 2 wKp ++ " (L/R)  Ki " ++ nf 2 wKi ++ " (Up/Down)  Kd " ++ nf 3 wKd
        ++ " (A/Z)  1-3 presets  T target tone " ++ (if wTarget then "ON" else "OFF"))
  , txt 20 68 15 c60 ("Error " ++ nf 1 wE ++ " Hz. P only (Ki = 0): f = Kp/(1+Kp) x target, stays low. More Ki reaches it, then wobbles. Kd calms.")
  , txt 20 (ht - 30) 15 c60 "Wobble = pitch vibration, overshoot = brief high note, lag = portamento. Hear the controller's character."
  ]
  where
    wd = envW env
    ht = envH env
    mx = wd * 0.12
    my0 = ht * 0.82
    my1 = ht * 0.2
    yOf hz = remap (clampF 200 800 hz) 200 800 my0 my1
    yel = hsb 45 90 100
    fHz = hzOut w
    dx = wd * 0.33
    dy = ht * 0.5
    rad = min wd ht * 0.12
    angle = fromIntegral (envFrame env) * fHz / 4000
    gx = wd * 0.5
    gw = wd * 0.46
    c60 = hsb 0 0 60

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wStarted = True }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wKp = min (wKp w + 0.25) 8 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wKp = max (wKp w - 0.25) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wKi = min (wKi w + 2) 60 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wKi = max (wKi w - 2) 0 }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> w { wKd = min (wKd w + 0.02) 0.5 }
    'z' -> w { wKd = max (wKd w - 0.02) 0 }
    't' -> w { wTarget = not (wTarget w) }
    '1' -> preset 3 0 0        -- ★ P だけ：低いまま（f = 0.75 r）
    '2' -> preset 3 8 0.05     -- ★ ちょうどいい
    '3' -> preset 3 50 0       -- ★ ゆれる（限界に近い）
    _ -> w
  _ -> w
  where
    preset kp ki kd = w { wKp = kp, wKi = ki, wKd = kd, wI = if ki > 0 then wR w / ki else 0 }

-- ---------- やってみよう ----------
-- 1. 1（P だけ）で目標の音（T）と比べる。いつも少し低い。Kp を上げると近づくが届かない
-- 2. 2 から Ki を上げていく。どこから「ゆれ」が聞こえる？ Kd を足すと消える？
-- 3. tauPlant を 0.3（重いモーター）にすると同じゲインでも不安定になる。なぜ？（07・09 の位相の遅れ）
-- 4. notes を自分のメロディに。interval を短くすると「追いつけない」音楽になる

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
