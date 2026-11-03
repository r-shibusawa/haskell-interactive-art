-- ============================================
-- 12 オブザーバーとカルマンフィルタ
-- 測れるのは「ノイズまじりの位置」だけ。速度を知りたい
--   素直に差分すると、ノイズが何十倍にもなる
--   オブザーバー：モデルで予測して、測定とのずれで修正する（x̂' = A x̂ + B u + L (z − C x̂)）
--   カルマンフィルタ：そのゲイン L を「ノイズの大きさ」から最適に決める
-- 上下キー = ノイズの大きさ / 左右キー = オブザーバーの速さ ωo / K = カルマンフィルタに切替 / クリック = 押す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

qModel, cFric, dt :: Float
qModel = 0.5   -- ★ カルマンフィルタが想定する「モデルのあいまいさ」
cFric = 0.5    -- 摩擦
dt = 1 / 60

histMax :: Int
histMax = 600

-- 2×2 行列：[[a, b], [c, d]] を (a, b, c, d) で表す
type M2 = (Float, Float, Float, Float)

data World = World
  { wSigma :: !Float   -- ★ 測定ノイズの標準偏差
  , wWo    :: !Float   -- ★ オブザーバーの極（−ωo の重根）。大きいほど速く追うがノイズに弱い
  , wKalman :: !Bool
  , wP, wV, wT, wU, wPush :: !Float
  , wZ, wVDiff :: !Float
  , wPHat, wVHat :: !Float
  , wCov   :: !M2      -- カルマンフィルタの共分散
  , wK1, wK2 :: !Float -- ゲイン
  , hP, hZ, hPh, hV, hVd, hVh :: !Hist
  }

world0 :: World
world0 = World
  { wSigma = 0.2, wWo = 5, wKalman = False, wP = 0, wV = 0, wT = 0, wU = 0, wPush = 0
  , wZ = 0, wVDiff = 0, wPHat = 0, wVHat = 0, wCov = (1, 0, 0, 1), wK1 = 0, wK2 = 0
  , hP = Seq.empty, hZ = Seq.empty, hPh = Seq.empty, hV = Seq.empty, hVd = Seq.empty, hVh = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-12 Observer and Kalman" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> do
      n <- randomGaussian
      pure (step n w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- n は標準正規分布の乱数（測定ノイズのもと）
step :: Float -> World -> World
step n w@World{..} =
  let t = wT + dt
      -- 本当の台車（入力は正弦波 + クリックの押し）
      u = 3 * sin (0.8 * t) + wPush
      v0 = wV + (u - cFric * wV) * dt
      p = clampF (-10) 10 (wP + v0 * dt)
      v = clampF (-20) 20 v0
      -- 測定：位置にノイズ
      z = p + n * wSigma
      vDiff = (z - wZ) / dt    -- 素直な差分
      w1 = w { wT = t, wU = u, wPush = wPush * 0.9, wV = v, wP = p, wZ = z, wVDiff = vDiff }
  in record (if wKalman then kalman z u w1 else observer z u w1)

-- 推定値の範囲をおさえて、履歴に記録する
record :: World -> World
record w@World{..} =
  let pH = clampF (-20) 20 wPHat
      vH = clampF (-50) 50 wVHat
  in w { wPHat = pH, wVHat = vH
       , hP = pushHist hP wP, hZ = pushHist hZ wZ, hPh = pushHist hPh pH
       , hV = pushHist hV wV, hVd = pushHist hVd wVDiff, hVh = pushHist hVh vH }

-- ★ ルーエンバーガー・オブザーバー：極を −ωo（重根）に置く L
observer :: Float -> Float -> World -> World
observer z u w@World{..} =
  let l1 = 2 * wWo - cFric
      l2 = wWo * wWo - l1 * cFric
      err = z - wPHat
  in w { wPHat = wPHat + (wVHat + l1 * err) * dt
       , wVHat = wVHat + (u - cFric * wVHat + l2 * err) * dt
       , wK1 = l1 * dt, wK2 = l2 * dt }

-- ★ カルマンフィルタ（離散）：予測 → 共分散を更新 → ゲイン → 修正
kalman :: Float -> Float -> World -> World
kalman z u w@World{..} =
  let f = (1, dt, 0, 1 - cFric * dt)
      pH0 = wPHat + wVHat * dt
      vH0 = wVHat + (u - cFric * wVHat) * dt
      q = (qModel * dt * dt * dt / 3, qModel * dt * dt / 2, qModel * dt * dt / 2, qModel * dt)
      (p00, p01, p10, p11) = m2add (m2mul (m2mul f wCov) (m2T f)) q
      s = p00 + wSigma * wSigma + 1e-9
      k1 = p00 / s
      k2 = p10 / s
      err = z - pH0
  in w { wPHat = pH0 + k1 * err, wVHat = vH0 + k2 * err, wK1 = k1, wK2 = k2
       , wCov = ((1 - k1) * p00, (1 - k1) * p01, p10 - k2 * p00, p11 - k2 * p01) }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  -- 台車 3 台（本当・測定・推定）
  [ drawRow i val name hue | (i, val, name, hue) <- [(0, wP, "True position", 30), (1, wZ, "Measured (noisy)", 0), (2, wPHat, "Estimate", 200)] ]
  ++
  [ -- 速度の矢印（本当と推定）
    seg 4 (hsb 30 80 100) (xOf wP, rowY 0 + 40) (xOf wP + wV * 10, rowY 0 + 40)
  , seg 4 (hsb 200 70 100) (xOf wPHat, rowY 2 + 40) (xOf wPHat + wVHat * 10, rowY 2 + 40)
    -- グラフ
  , drawPlot gx (ht * 0.16) gw (ht * 0.3)
      [Series hZ (hsba 0 0 100 0.5) "Measured z" 1, ser hP 30 "True", ser hPh 200 "Estimate"] (-7) 7 "Position"
  , drawPlot gx (ht * 0.5) gw (ht * 0.3)
      [Series hVd (hsba 0 0 100 0.5) "Difference" 1, ser hV 30 "True", ser hVh 200 "Estimate"] (-8) 8 "Velocity (estimated, never measured)"
  , txt 20 40 18 white ((if wKalman then "Kalman filter" else "Observer wo = " ++ nf 1 wWo ++ " (L/R)")
        ++ "   Noise sigma = " ++ nf 2 wSigma ++ " (Up/Down)   K: switch   Click: push   Vel. RMS " ++ nf 2 vErr)
  , txt 20 68 15 c60 "Difference (z - zprev)/dt blows up the noise. The observer trusts its model and corrects a little at a time."
  , txt 20 90 15 c60 ("Gain = [" ++ nf 3 wK1 ++ ", " ++ nf 3 wK2 ++ "]")
  , txt 20 (ht - 30) 15 c60 (if wKalman
      then "Kalman filter: larger noise sigma means trusting the measurement less (smaller gain). Change sigma and watch."
      else "Higher wo: catches up fast but picks up noise. Lower: smooth but late. Kalman (K) picks the balance by itself.")
  ]
  where
    wd = envW env
    ht = envH env
    rx0 = wd * 0.05
    rx1 = wd * 0.47
    xOf v = remap v (-6) 6 rx0 rx1
    rowY :: Int -> Float
    rowY i = ht * 0.28 + fromIntegral i * ht * 0.17
    drawRow i val name hue =
      let ry = rowY i
          cx = xOf (clampF (-6) 6 val)
      in Pictures
           [ seg 2 (hsb 0 0 40) (rx0, ry + 20) (rx1, ry + 20)
           , if i == 2
               then frameBox 3 (hsb hue 70 100) (cx - 30) (ry - 16) 60 36
               else box (cx - 30) (ry - 16) 60 36 (hsba hue (if i == 1 then 0 else 80) 100 (if i == 1 then 0.5 else 1))
           , txt rx0 (ry - 26) 14 (hsb hue (if i == 1 then 0 else 70) 100) (name ++ "  " ++ nf 2 val) ]
    gx = wd * 0.52
    gw = wd * 0.44
    c60 = hsb 0 0 60
    -- 速度の誤差 RMS
    diffs = zipWith (-) (toList hV) (toList hVh)
    vErr = sqrt (sum (map (\d -> d * d) diffs) / fromIntegral (max (length diffs) 1))

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    w { wPush = if fst (toP5 env p) > envW env * 0.26 then 30 else -30 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSigma = min (wSigma w + 0.05) 1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSigma = max (wSigma w - 0.05) 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wWo = min (wWo w + 1) 40 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wWo = max (wWo w - 1) 1 }
  EventKey (Char c) Down _ _ | toLower c == 'k' -> w { wKalman = not (wKalman w), wCov = (1, 0, 0, 1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. σ = 0.5 にして、差分の速度（白）と推定（青）を比べる。推定はなぜ使えるレベルなのか
-- 2. ωo を 1 にする：なめらかだが遅れる。30 にする：ノイズを拾う。ちょうどいいのは？
-- 3. K でカルマンに切替、σ を 0 → 1 と変えてゲインの変化を見る。qModel（モデルのあいまいさ）を 10 にすると？
-- 4. 本当の摩擦を 1.0 にしてモデルは 0.5 のまま → モデルのずれがあると推定はどうなる？

-- 標準正規分布の乱数（ボックス・ミュラー法、p5.js の randomGaussian）
randomGaussian :: IO Float
randomGaussian = do
  a <- randomRIO (1e-6, 1)
  b <- randomRIO (0, 1)
  pure (sqrt (-2 * log a) * cos (tau * b))

m2mul, m2add :: M2 -> M2 -> M2
m2mul (a, b, c, d) (e, f, g, h) = (a * e + b * g, a * f + b * h, c * e + d * g, c * f + d * h)
m2add (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)

m2T :: M2 -> M2
m2T (a, b, c, d) = (a, c, b, d)

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
