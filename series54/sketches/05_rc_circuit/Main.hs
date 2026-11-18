-- ============================================
-- 54-05 RC 回路 ─ コンデンサの充電と放電、時定数 τ = RC
-- コンデンサの電圧 Vc は dVc/dt = (Vin − Vc) / (RC)。これをルンゲ・クッタ法（RK4）で少しずつ進める
-- 充電では Vc = V0(1 − e^(−t/τ))、τ たつと 63.2%。電池が出したエネルギーの半分は抵抗で熱になる
-- 操作：1 = スイッチ / 2 = 方形波 / スペース = スイッチ切りかえ / ↑↓ = R / ←→ = C（2 では周波数）/ R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

v0 :: Double
v0 = 5                                           -- ★ 電池の電圧 [V]

rList, cList, fList :: [Double]
rList = [1e3, 2e3, 5e3, 10e3, 20e3, 50e3]        -- ★ 抵抗 [Ω]
cList = [10e-6, 22e-6, 47e-6, 100e-6, 220e-6]    -- ★ コンデンサ [F]
fList = [0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10]       -- 方形波の周波数 [Hz]

span' :: Double
span' = 10                                       -- グラフの横幅 [秒]

-- 履歴の 1 点：時刻、入力の電圧、コンデンサの電圧、電流 [mA]
data H = H { hT, hVin, hVc, hI :: !Double }

data World = World
  { wRi, wCi, wFi, wMode :: !Int
  , wCharging :: !Bool
  , wVc, wT, wTSwitch, wVcSwitch :: !Double
  , wHist :: !(Seq.Seq H)
  , wBat, wR, wPpLo, wPpHi, wPpShow :: !Double
  , wLastPeriod :: !Int
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-05 RC circuit" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 3 3 3 1 False 0 0 0 0 Seq.empty 0 0 1e9 (-1e9) 0 0 []

resR, capC, freq :: World -> Double
resR w = getAt 1e4 rList (wRi w)
capC w = getAt 1e-4 cList (wCi w)
freq w = getAt 0.5 fList (wFi w)

rc :: World -> Double
rc w = resR w * capC w

vinAt :: World -> Double -> Double
vinAt w tt
  | wMode w == 1 = if wCharging w then v0 else 0
  | otherwise = if jsRem (tt * freq w) 1 < 0.5 then v0 else 0

-- 1 フレーム：1 歩は τ/50 より小さく
step :: Float -> World -> World
step dt w0 = record (foldl' one w0 [1 .. n])
  where
    dtF = realToFrac (min dt 0.05) :: Double
    n = max 1 (ceiling (dtF / (rc w0 / 50))) :: Int
    h = dtF / fromIntegral n
    one w _ =
      let vin = vinAt w (wT w)
          i = (vin - wVc w) / resR w
          vc = rk4Step (wVc w) vin (rc w) h
          t1 = wT w + h
          w1 = w { wVc = vc, wBat = wBat w + vin * i * h, wR = wR w + i * i * resR w * h, wT = t1 }
      in if wMode w /= 2 then w1 else
           let lo = min (wPpLo w1) vc
               hi = max (wPpHi w1) vc
               p = floor (t1 * freq w) :: Int
           in if p /= wLastPeriod w1 then w1 { wPpShow = hi - lo, wPpLo = 1e9, wPpHi = -1e9, wLastPeriod = p }
              else w1 { wPpLo = lo, wPpHi = hi }
    record w =
      let vin = vinAt w (wT w)
          i = (vin - wVc w) / resR w
          hs = wHist w Seq.|> H (wT w) vin (wVc w) (i * 1000)
          trim s = case Seq.viewl s of
            x Seq.:< rest | Seq.length s > 2 && (wT w - hT x > span' || Seq.length s > 2000) -> trim rest
            _ -> s
      in w { wHist = trim hs }

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ drawCircuit env w 110 120 vin i
  , plotFrame g1 ("Voltage [V]  orange = input Vin, cyan = capacitor Vc (last " ++ showJ span' ++ " s)") (-0.2) (v0 * 1.1)
  , Color (rgb255 255 170 80) (plotTime 2 w g1 (map hVin hs) (-0.2) (v0 * 1.1))
  , Color (rgb255 110 210 255) (plotTime 2.5 w g1 (map hVc hs) (-0.2) (v0 * 1.1)) ]
  ++ (if wMode w == 1 then tauMarks else [])
  ++ [ plotFrame g2 "Current I [mA] (+ = charging direction)" (-imax * 1.1) (imax * 1.1)
     , Color (rgb255 255 230 120) (plotTime 2 w g2 (map hI hs) (-imax * 1.1) (imax * 1.1)) ]
  ++ [ say s tx (128 + 28 * fromIntegral k) 16 c | (k, (s, c)) <- zip [0 :: Int ..] numbers ]
  ++ [ say s tx (yNote + 20 * fromIntegral k) 14 (gray 0.67) | (k, s) <- zip [0 :: Int ..] notes ]
  ++ [ say ("1 switch  2 square wave | Space switch (" ++ (if wCharging w then "charge" else "discharge") ++ ")  Up/Down R  Left/Right "
            ++ (if wMode w == 1 then "C" else "frequency") ++ "  R reset") 20 34 17 (gray 0.92)
     , say "dVc/dt = (Vin - Vc)/(RC) by RK4 (each frame split finer than tau/50). Time runs at real speed." 20 60 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    hs = toList (wHist w)
    vin = vinAt w (wT w)
    i = (vin - wVc w) / resR w
    g1 = (60, envH env * 0.5, envW env - 160, envH env * 0.2)
    g2 = (60, envH env * 0.76, envW env - 160, envH env * 0.13)
    imax = (v0 / resR w) * 1000
    tx = envW env * 0.5
    ec = 0.5 * capC w * wVc w * wVc w
    -- 63.2%（充電）または 36.8%（放電）の線と、切りかえから τ の縦線
    tauMarks =
      let (gx, gy, gw, gh) = g1
          lv = wVcSwitch w + (if wCharging w then v0 - wVcSwitch w else -(wVcSwitch w)) * (1 - exp (-1))
          yy = gy + gh - f2 ((lv + 0.2) / (v0 * 1.1 + 0.2)) * gh
          el = wT w - wTSwitch w
          xx = gx + gw - f2 ((el - rc w) / span') * gw
      in Color (rgba255 255 240 150 120) (dashed 6 6 [(gx, yy), (gx + gw, yy)])
         : (if el >= rc w && xx > gx
              then [ Color (rgb255 255 240 150) (Line [(xx, gy), (xx, gy + gh)]), say "tau after switch" (xx + 4) (gy + 16) 14 (rgb255 255 240 150) ]
              else [])
    numbers =
      [ ("R = " ++ sci 2 (resR w / 1000) ++ " kOhm  C = " ++ sci 2 (capC w * 1e6) ++ " uF  ->  tau = RC = " ++ sci 3 (rc w) ++ " s", gray 0.92)
      , ("Vc = " ++ fmt 3 (wVc w) ++ " V (" ++ fmt 1 (wVc w / v0 * 100) ++ "%)   I = " ++ fmt 3 (i * 1000) ++ " mA", gray 0.92) ]
      ++ if wMode w == 1
           then [ ("Since switch: " ++ fmt 2 (wT w - wTSwitch w) ++ " s = " ++ fmt 2 ((wT w - wTSwitch w) / rc w) ++ " tau", gray 0.92)
                , ("Energy: battery gave " ++ sci 3 (wBat w * 1000) ++ " mJ", rgb255 255 230 120)
                , ("  = capacitor " ++ sci 3 ((ec - 0.5 * capC w * wVcSwitch w * wVcSwitch w) * 1000) ++ " mJ + heat in R " ++ sci 3 (wR w * 1000) ++ " mJ", rgb255 255 230 120) ]
           else let fc = 1 / (2 * pi * rc w)
                    tp = 1 / freq w
                in [ ("Square wave f = " ++ showJ (freq w) ++ " Hz   cutoff fc = 1/(2 pi RC) = " ++ sci 3 fc ++ " Hz", gray 0.92)
                   , ("Vc swing (measured) = " ++ fmt 3 (wPpShow w) ++ " V", rgb255 255 230 120)
                   , ("formula V0 tanh(T/4tau) = " ++ fmt 3 (v0 * tanh (tp / (4 * rc w))) ++ " V", rgb255 255 230 120) ]
    yNote = 128 + 28 * 5 + 6
    notes
      | wMode w == 1 = [ "Charging from 0 V to full: exactly half of the battery's CV0^2", "becomes heat (whatever the size of R)." ]
      | otherwise = [ "f << fc: passes through (Vc ~ Vin). f >> fc: almost blocked,", "Vc becomes a triangle wave -> low-pass filter (removes high sounds)." ]

-- ---- 回路の図
drawCircuit :: Env -> World -> Float -> Float -> Double -> Double -> Picture
drawCircuit env w x y vin i = Pictures $
  [ Color (rgb255 200 205 215) (Pictures [thickLine 3 (x, y) (x + cw, y), thickLine 3 (x + cw, y) (x + cw, y + ch), thickLine 3 (x, y + ch) (x + cw, y + ch), thickLine 3 (x, y) (x, y + ch)])
  , Color (rgb255 14 18 26) (Pictures [rectTL (x - 25) (y + ch / 2 - 22) 50 44, rectTL (x + cw * 0.4) (y - 14) (cw * 0.25) 28, rectTL (x + cw - 30) (y + ch / 2 - 14) 60 28])
    -- 電池
  , Color (rgb255 120 200 255) (Pictures [thickLine 4 (x - 22, y + ch / 2 - 8) (x + 22, y + ch / 2 - 8), thickLine 3 (x - 11, y + ch / 2 + 8) (x + 11, y + ch / 2 + 8)])
  , say (if wMode w == 1 then showJ v0 ++ " V" else "square") (x - 80) (y + ch / 2 + 5) 14 (rgb255 160 220 255)
    -- 抵抗（ぎざぎざ）
  , Color (rgb255 255 170 110) (thickPath 3 [ (x + cw * 0.4 + cw * 0.25 * fromIntegral k / 8, y + (if k == 0 || k == 8 then 0 else if odd k then 9 else -9)) | k <- [0 .. 8 :: Int] ])
  , say "R" (x + cw * 0.5) (y - 18) 14 (rgb255 255 190 140)
    -- コンデンサの極板（ためた電荷）
  , Color (rgb255 200 205 215) (Pictures [thickLine 4 (x + cw - 26, y + ch / 2 - 7) (x + cw + 26, y + ch / 2 - 7), thickLine 4 (x + cw - 26, y + ch / 2 + 7) (x + cw + 26, y + ch / 2 + 7)])
  , Color (rgba255 240 90 70 alpha) (rectTL (x + cw - 26) (y + ch / 2 - 16) 52 6)
  , Color (rgba255 70 150 250 alpha) (rectTL (x + cw - 26) (y + ch / 2 + 10) 52 6)
  , say "C" (x + cw + 34) (y + ch / 2 + 5) 14 (rgb255 140 220 255) ]
  ++ [ say (if wCharging w then "Switch: battery connected (charging)" else "Switch: battery removed, shorted (discharging)") (x + 10) (y + ch + 28) 14
           (if wCharging w then rgb255 255 230 120 else gray 0.59) | wMode w == 1 ]
  -- 電流の点
  ++ [ Color (rgb255 255 230 120) (Pictures [ uncurry circleD (pos d) 6 | d <- [d0, d0 + 30 .. per - 1e-3] ]) | abs i > 1e-6 ]
  where
    cw = min 380 (envW env * 0.36)
    ch = 170
    q = clampv (-1) 1 (wVc w / v0)
    alpha = max 0 (min 255 (round (60 + 190 * q) :: Int))
    s = jsRem (fromIntegral (envFrame env) * i * 1000 * 2) 30
    d0 = f2 (jsRem (jsRem s 30 + 30) 30)
    per = 2 * (cw + ch)
    pos d | d < cw = (x + d, y)
          | d < cw + ch = (x + cw, y + d - cw)
          | d < 2 * cw + ch = (x + cw - (d - cw - ch), y + ch)
          | otherwise = (x, y + ch - (d - 2 * cw - ch))

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> resetRun w { wMode = if c == '1' then 1 else 2 }
    | c `elem` "rR" -> resetRun w { wRi = 3, wCi = 3, wFi = 3 }
  EventKey (SpecialKey KeySpace) Down _ _ | wMode w == 1 -> markSwitch w { wCharging = not (wCharging w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> markSwitch w { wRi = min (length rList - 1) (wRi w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> markSwitch w { wRi = max 0 (wRi w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _
    | wMode w == 1 -> markSwitch w { wCi = min (length cList - 1) (wCi w + 1) }
    | otherwise -> markSwitch w { wFi = min (length fList - 1) (wFi w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _
    | wMode w == 1 -> markSwitch w { wCi = max 0 (wCi w - 1) }
    | otherwise -> markSwitch w { wFi = max 0 (wFi w - 1) }
  _ -> w

resetRun :: World -> World
resetRun w = markSwitch w { wVc = 0, wT = 0, wCharging = False, wHist = Seq.empty }

markSwitch :: World -> World
markSwitch w = w { wTSwitch = wT w, wVcSwitch = wVc w, wBat = 0, wR = 0 }

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("charging: at t = tau, Vc = 63.2% (1 - e^-1)", near (run 0 1 1 1e-3) (1 - exp (-1)) 1e-9)
  , ("discharging: at t = tau, Vc = 36.8% (e^-1)", near (run 1 0 1 1e-3) (exp (-1)) 1e-9)
  , ("time to reach 63.2% = RC (within 0.1%)", nearRel tCross tau 0.001)
  , ("full charge: battery CV^2 = capacitor CV^2/2 + resistor CV^2/2 (within 0.5%)"
    , nearRel wb cc 0.005 && nearRel (0.5 * cc * vcE * vcE) (0.5 * cc) 0.005 && nearRel wr (0.5 * cc) 0.005)
  , ("half-life = tau ln2", near (run 1 0 (log 2) (log 2 / 1000)) 0.5 1e-9)
  , ("square wave: steady swing = V0 tanh(T/4tau) (within 0.5%)", nearRel (sqHi - sqLo) (tanh (tq / (4 * tau))) 0.005)
  , ("one RK4 step (h = tau/50) error < 1e-10 (vs e^(-h/tau))", near (rk4Step 0 1 tau (tau / 50)) (1 - exp (-1 / 50)) 1e-10) ]
  where
    tau = 1
    run vs vin tt dt = foldl' (\v _ -> rk4Step v vin tau dt) vs [1 .. (round (tt / dt) :: Int)]
    target = 1 - exp (-1)
    cross v tt prev dt | v < target = cross (rk4Step v 1 tau dt) (tt + dt) v dt
                       | otherwise = tt - (dt * (v - target)) / (v - prev)
    tCross = cross 0 0 0 1e-3
    rr = 1000
    cc = 1e-3
    hh = (rr * cc) / 1000
    (vcE, wb, wr) = foldl' (\(!vc, !b, !r) _ -> let i = (1 - vc) / rr in (rk4Step vc 1 (rr * cc) hh, b + 1 * i * hh, r + i * i * rr * hh)) (0, 0, 0) [1 .. 15000 :: Int]
    tq = 0.8
    (_, sqLo, sqHi) = foldl' (\(!x, !lo, !hi) k -> let ts = fromIntegral k * 1e-4
                                                       x1 = rk4Step x (if jsRem (ts / tq) 1 < 0.5 then 1 else 0) tau 1e-4
                                                   in if ts > 30 then (x1, min lo x1, max hi x1) else (x1, lo, hi))
                             (0, 9, -9) [0 .. 399999 :: Int]

-- ---------- やってみよう ----------
-- 1. スペースで充電し、τ たったとき（黄色の縦線）に Vc が 63.2% になっていることを確かめよう
-- 2. ↑ で R を大きくすると τ は？ 抵抗で熱になったエネルギーは変わる？（実は変わらない！）
-- 3. 2 の方形波で、f を fc より小さく・大きくして Vc の形を比べよう（ローパスフィルタ）
-- 4. カメラのフラッシュは、コンデンサにためたエネルギーを一気に使う。½CV² で何 J？
-- 5. v0 を変えて、τ が電圧によらないことを確かめよう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 時刻つきのグラフ：右はし = いま、左はし = span' 秒前
plotTime :: Float -> World -> G -> [Double] -> Double -> Double -> Picture
plotTime wt w (x, y, gw, gh) arr lo hi =
  polyline wt [ (x + gw - f2 ((wT w - hT p) / span') * gw, y + gh - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * gh) | (p, v) <- zip (toList (wHist w)) arr ]

-- ---- RK4 で 1 歩：dVc/dt = (vin − Vc) / τ（vin はこの 1 歩のあいだ一定）
rk4Step :: Double -> Double -> Double -> Double -> Double
rk4Step v vin tau h = v + (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
  where
    f x = (vin - x) / tau
    k1 = f v
    k2 = f (v + (h / 2) * k1)
    k3 = f (v + (h / 2) * k2)
    k4 = f (v + h * k3)

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: (RealFrac a) => a -> a -> a
jsRem a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where
    e0 = floor (logBase 10 (abs v)) :: Int
    e | 10 ^^ (e0 + 1) <= abs v = e0 + 1
      | 10 ^^ e0 > abs v = e0 - 1
      | otherwise = e0

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d / Nothing）
getAt :: a -> [a] -> Int -> a
getAt d xs i = maybe d id (getAtM xs i)

getAtM :: [a] -> Int -> Maybe a
getAtM xs i = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

-- n 回くり返した結果（iterate と組み合わせる。途中を正格に評価する）
(!!!) :: [a] -> Int -> a
xs !!! n = go xs n
  where go (y : ys) k | k <= 0 = y
                      | otherwise = y `seq` go ys (k - 1)
        go [] _ = error "empty"
infixl 9 !!!

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 線分（太さ w。1 以下なら細い線）
seg :: Float -> Point -> Point -> Picture
seg w a b = if w <= 1 then Line [a, b] else thickLine w a b

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- (x1, y1) → (x2, y2) の矢印。太さ w、hs = 矢じりの大きさ
arrowTo :: Float -> Float -> Point -> Point -> Picture
arrowTo w hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Pictures [ seg w (x1, y1) (x2, y2)
                         , seg w (x2, y2) (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux)
                         , seg w (x2, y2) (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)

-- ---- グラフの道具：枠 (x, y, w, h)、lo〜hi の範囲
type G = (Float, Float, Float, Float)

plotFrame :: G -> String -> Double -> Double -> Picture
plotFrame (x, y, w, h) title lo hi = Pictures $
  [ Color (rgb255 70 80 100) (frameTL 1 x y w h) ]
  ++ [ Color (rgba255 70 80 100 160) (Line [(x, y0), (x + w, y0)]) | lo < 0 && hi > 0, let y0 = y + h * f2 (hi / (hi - lo)) ]
  ++ [ say title (x + 4) (y - 6) 14 (gray 0.78)
     , say (sci 2 hi) (x + w + 4) (y + 12) 14 (gray 0.55)
     , say (sci 2 lo) (x + w + 4) (y + h) 14 (gray 0.55) ]

-- 数の列を左から右へ（cap 個ぶんの幅）
plotTrace :: Float -> G -> [Double] -> Double -> Double -> Int -> Picture
plotTrace wt (x, y, w, h) arr lo hi cap =
  polyline wt [ (x + fromIntegral i / fromIntegral (cap - 1) * w, y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h) | (i, v) <- zip [0 :: Int ..] arr ]

-- 長さ cap までの履歴（古いものから消す）
pushCap :: Int -> a -> Seq.Seq a -> Seq.Seq a
pushCap cap v s = let s1 = s Seq.|> v in if Seq.length s1 > cap then Seq.drop (Seq.length s1 - cap) s1 else s1

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h
