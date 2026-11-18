-- ============================================
-- 54-06 RLC 回路と共振 ─ 電気のふりこ
-- コイル L・コンデンサ C・抵抗 R の直列回路：L·di/dt + R·i + q/C = V(t)、dq/dt = i（RK4 で計算）
-- R = 0 なら電荷とエネルギーが C と L を行き来して振動し続ける（f0 = 1/(2π√LC)）。R があると減衰（Q 値）
-- 交流を加えて周波数を変えると、f0 で電流がいちばん大きい（共振）。スペースで周波数を掃引して共振曲線を測る
-- 操作：1 = 自由振動 / 2 = 交流で共振 / ↑↓ = R / ←→ = 周波数 / スペース = 1：充電しなおす 2：掃引 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

lH :: Double
lH = 0.1                               -- ★ コイル L [H]（100 mH）

cF :: Double
cF = 100e-6                            -- ★ コンデンサ C [F]（100 μF）

v0 :: Double
v0 = 10                                -- ★ 電圧 [V]（1：最初の充電、2：交流の振幅）

rList :: [Double]
rList = [0, 0.5, 1, 2, 5, 10, 20]      -- 抵抗 [Ω]

slow :: Double
slow = 0.01                            -- ★ スローモーション（1 秒で 0.01 秒ぶん）

-- 回路の状態：電荷 q [C] と電流 i [A]
data St = St { sQ, sI :: !Double }

data World = World
  { wMode, wRi :: !Int
  , wFDrive    :: !Double
  , wSt        :: !St
  , wTSim      :: !Double
  , wHeat      :: !Double
  , wHistQ     :: !(Seq.Seq Double)
  , wHistI     :: !(Seq.Seq Double)
  , wSweep     :: [(Double, Double)]   -- 掃引で測った (周波数, 振幅)
  , wSweepOn   :: !Bool
  , wSweepF    :: !Double
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-06 RLC resonance" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 3 40 (St (cF * v0) 0) 0 0 Seq.empty Seq.empty [] False 20 []

rNow :: World -> Double
rNow w = let r = getAt 0 rList (wRi w) in if wMode w == 2 then max 0.5 r else r

f0 :: Double
f0 = 1 / (2 * pi * sqrt (lH * cF))

vSrc :: World -> Double -> Double
vSrc w t = if wMode w == 2 then v0 * sin (2 * pi * wFDrive w * t) else 0

-- 1 フレームを 20 回に分けて進める
step :: Float -> World -> World
step dtF w0 = sweepStep (record (foldl' one w0 [1 .. 20 :: Int]))
  where
    dt = (realToFrac (min dtF 0.05) * slow) / 20
    r = rNow w0
    one w _ = let s = rlcStep (wSt w) (wTSim w) dt r lH cF (vSrc w)
              in w { wSt = s, wHeat = wHeat w + sI s * sI s * r * dt, wTSim = wTSim w + dt }
    -- 電荷の目盛り（2 は落ち着いたときの振幅 I/ω の 2 倍：始めのうなりも入る）
    record w = let qn = if wMode w == 1 then cF * v0 else (2 * ampFormula (wFDrive w) r) / (2 * pi * wFDrive w)
               in w { wHistQ = pushCap 500 (sQ (wSt w) / qn) (wHistQ w), wHistI = pushCap 500 (sI (wSt w)) (wHistI w) }
    -- 掃引：1 フレームに 1 つの周波数で、落ち着くまで計算して振幅を測る
    sweepStep w
      | wMode w == 2 && wSweepOn w =
          let f1 = wSweepF w * 1.03
          in w { wSweep = wSweep w ++ [(wSweepF w, steadyAmp (wSweepF w) r)], wSweepF = f1, wSweepOn = not (f1 > 120) }
      | otherwise = w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- グラフ：電荷と電流
  [ plotFrame g ((if wMode w == 1 then "cyan = charge q/q0, " else "cyan = q (steady = 0.5), ") ++ "yellow = current i (scale +-" ++ fmt 2 imax ++ " A)") (-1.1) 1.1
  , Color (rgb255 110 210 255) (plotTrace 2 g (toList (wHistQ w)) (-1.1) 1.1 500)
  , Color (rgb255 255 220 110) (plotTrace 2 g (map (/ imax) (toList (wHistI w))) (-1.1) 1.1 500) ]
  -- エネルギーの棒
  ++ concat [ [ say nm bx (by + fromIntegral k * 30 + 14) 14 (gray 0.78)
              , Color c (rectTL (bx + 180) (by + fromIntegral k * 30) (f2 (clampv 0 1.2 (v / max u0 (uc + ul + 1e-12))) * (bw - 290)) 18)
              , say (sci 3 (v * 1000) ++ " mJ") (bx + 188 + 1.2 * (bw - 290)) (by + fromIntegral k * 30 + 14) 14 (gray 0.78) ]
            | (k, (nm, v, c)) <- zip [0 :: Int ..] bars ]
  -- 右：共振曲線（式）と測った点
  ++ [ plotFrame rg "Resonance: current amplitude |I| [A] vs f [Hz]" 0 (imaxC * 1.05)
     , Color (rgb255 255 170 80) (polyline 2 [ (fx f, ay (ampFormula f r)) | k <- [0 .. 200 :: Int], let f = fmin + (fmax - fmin) * fromIntegral k / 200 ])
     , Color (rgb255 120 230 170) (Pictures [ circleD (fx f) (ay a) 6 | (f, a) <- wSweep w ])
     , Color (rgba255 255 255 255 90) (Line [(fx f0, ry), (fx f0, ry + rh)]) ]
  ++ [ Color (rgb255 255 220 110) (Line [(fx (wFDrive w), ry), (fx (wFDrive w), ry + rh)]) | wMode w == 2 ]
  ++ [ sayC (show (round f :: Int)) (fx f) (ry + rh + 18) 14 (gray 0.63) | f <- [20, 40, 60, 80, 100, 120] ]
  -- 数字
  ++ [ say s tx (ry + rh + 50 + 24 * fromIntegral k) 15 c | (k, (s, c)) <- zip [0 :: Int ..] numbers ]
  ++ [ say s tx (ry + rh + 50 + 24 * fromIntegral (length numbers) + 20 * fromIntegral k) 14 (gray 0.67) | (k, s) <- zip [0 :: Int ..] notes ]
  ++ [ say ("1 free oscillation  2 AC resonance | Up/Down R  " ++ (if wMode w == 2 then "Left/Right frequency  Space sweep" else "Space recharge") ++ "  R reset") 20 34 17 (gray 0.92)
     , say ("L di/dt + R i + q/C = V(t) by RK4. Time runs " ++ showJ slow ++ "x (f0 ~ 50 Hz -> about 0.5 swings/s on screen)") 20 60 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    r = rNow w
    St q i = wSt w
    uc = (q * q) / (2 * cF)
    ul = 0.5 * lH * i * i
    u0 = 0.5 * cF * v0 * v0
    g@(_, gy, _, gh) = (60, 120, envW env * 0.5, envH env * 0.24)
    imax = if wMode w == 1 then v0 * sqrt (cF / lH) else min (v0 / r) 40
    bx = 60
    by = gy + gh + 60
    bw = envW env * 0.5
    bars = [ ("Cap q^2/2C", uc, rgb255 110 210 255), ("Coil Li^2/2", ul, rgb255 255 220 110)
           , ("Total", uc + ul, gray 0.86), ("Heat int i^2R dt", wHeat w, rgb255 255 120 90) ]
    rg@(rx, ry, rw, rh) = (envW env * 0.6, 140, envW env * 0.36, envH env * 0.38)
    fmin = 20
    fmax = 120
    imaxC = v0 / r
    fx f = rx + f2 ((f - fmin) / (fmax - fmin)) * rw
    ay a = ry + rh - f2 (a / (imaxC * 1.05)) * rh
    qv = if r > 0 then sqrt (lH / cF) / r else 1 / 0
    tx = rx
    numbers =
      [ ("L = " ++ showJ (fromIntegral (round (lH * 1000) :: Int)) ++ " mH  C = " ++ showJ (fromIntegral (round (cF * 1e6) :: Int)) ++ " uF  R = " ++ showJ r ++ " Ohm", gray 0.92)
      , ("f0 = 1/(2 pi sqrt(LC)) = " ++ fmt 2 f0 ++ " Hz", rgb255 255 230 120)
      , ("Q = sqrt(L/C)/R = " ++ (if isInfinite qv then "inf" else fmt 1 qv), rgb255 255 230 120)
      , ("Width df = R/(2 pi L) = " ++ fmt 2 (r / (2 * pi * lH)) ++ " Hz = f0/Q", gray 0.92) ]
      ++ if wMode w == 2
           then let z = hyp r (2 * pi * wFDrive w * lH - 1 / (2 * pi * wFDrive w * cF))
                in [ ("Now f = " ++ fmt 1 (wFDrive w) ++ " Hz  |Z| = " ++ fmt 2 z ++ " Ohm", gray 0.92)
                   , ("|I| = V0/|Z| = " ++ fmt 3 (v0 / z) ++ " A", gray 0.92) ]
           else [ ("Total energy " ++ sci 4 ((uc + ul) * 1000) ++ " mJ (constant if R = 0)", gray 0.92) ]
    notes = [ "Orange = formula V0/sqrt(R^2 + (wL - 1/wC)^2),", "green = amplitude measured by RK4 (Space: sweep)" ]

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> restart w { wMode = if c == '1' then 1 else 2 }
    | c `elem` "rR" -> restart w { wRi = 3, wFDrive = 40, wSweep = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRi = min (length rList - 1) (wRi w + 1), wSweep = [] }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRi = max 0 (wRi w - 1), wSweep = [] }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wFDrive = min 120 (wFDrive w * 1.03) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wFDrive = max 20 (wFDrive w / 1.03) }
  EventKey (SpecialKey KeySpace) Down _ _
    | wMode w == 1 -> restart w
    | otherwise -> w { wSweep = [], wSweepF = 20, wSweepOn = True }
  _ -> w

restart :: World -> World
restart w = w { wSt = St (if wMode w == 1 then cF * v0 else 0) 0, wTSim = 0, wHeat = 0, wHistQ = Seq.empty, wHistI = Seq.empty }

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("f0 = 1/(2 pi sqrt(LC)) = 50.33 Hz", near f0 50.329 0.001)
  , ("R = 0: period of the oscillation = 2 pi sqrt(LC) (within 0.1%)"
    , case crosses of
        (c0 : _ : _) -> nearRel ((last' c0 crosses - c0) / fromIntegral (length crosses - 1)) t0 0.001
        _ -> False)
  , ("R = 0: energy is conserved (within 1e-6 over 20 periods)", eMax / e0 < 1e-6)
  , ("with R, each period shrinks by e^(-R T/2L) (within 0.5%)"
    , case peaks of
        (_ : p1 : p2 : _) -> nearRel (p2 / p1) (exp ((-rd * 2 * pi) / wd / (2 * lH))) 0.005
        _ -> False)
  , ("at resonance (f = f0) the steady current amplitude = V0/R (within 1%)", nearRel (steadyAmp f0 2) (v0 / 2) 0.01)
  , ("off resonance (0.8 f0) too: computed = formula V0/|Z| (within 1%)", nearRel (steadyAmp (0.8 * f0) 2) (ampFormula (0.8 * f0) 2) 0.01)
  , ("half-power width = R/(2 pi L) = f0/Q", nearRel (fb - fa) (2 / (2 * pi * lH)) 1e-6) ]
  where
    t0 = 2 * pi * sqrt (lH * cF)
    dt = t0 / 2000
    none _ = 0
    s0 = St 1e-3 0
    e0 = (sQ s0 * sQ s0) / (2 * cF)
    -- R = 0：q が + から 0 以下になる時刻と、エネルギーのずれの最大
    (_, _, crossR, eMax) = foldl' freeStep (s0, 0, [], 0) [1 .. 2000 * 20 :: Int]
    crosses = reverse crossR
    freeStep (s, t, cs, em) _ =
      let n = rlcStep s t dt 0 lH cF none
          cs' = if sQ s > 0 && sQ n <= 0 then (t + (dt * sQ s) / (sQ s - sQ n)) : cs else cs
          em' = max em (abs ((sQ n * sQ n) / (2 * cF) + 0.5 * lH * sI n * sI n - e0))
      in (n, t + dt, cs', em')
    -- R = 2：電荷の山
    rd = 2
    wd = sqrt (1 / (lH * cF) - (rd * rd) / (4 * lH * lH))
    (_, _, _, peaksR) = foldl' dampStep (s0, s0, 0, []) [1 .. 2000 * 5 :: Int]
    peaks = reverse peaksR
    dampStep (prev, s, t, ps) _ =
      let n = rlcStep s t dt rd lH cF none
          ps' = if sQ prev < sQ s && sQ n <= sQ s && sQ s > 0 then sQ s : ps else ps
      in (s, n, t + dt, ps')
    -- 電力が半分になる周波数を 2 分法で
    half lo hi up = go lo hi (0 :: Int)
      where go a b k | k >= 80 = (a + b) / 2
                     | otherwise = let m = (a + b) / 2
                                   in if ((ampFormula m 2 ^ (2 :: Int) > 0.5 * (v0 / 2) ^ (2 :: Int)) == up) then go a m (k + 1) else go m b (k + 1)
    fa = half 20 f0 True
    fb = half f0 120 False
    last' d xs = case reverse xs of { (x : _) -> x; [] -> d }

-- ---------- やってみよう ----------
-- 1. 1 で R = 0 にして、エネルギーが コンデンサ ⇔ コイル を行き来するのを見よう。合計は？
-- 2. ↑ で R を大きくすると、何回くらいで止まる？ Q 値との関係は？（Q ≈ 何回ふれるか × π）
-- 3. 2 で ←→ を押して f0 に近づけ、電流が大きくなるのを確かめよう。電圧と電流の山の時間差（位相）は？
-- 4. スペースで掃引し、緑の点（計算）と橙の線（式）が重なることを確かめよう。R を変えると幅は？
-- 5. ラジオのチューニングは共振。C を変えて 1 MHz に合わせるには C をいくつにする？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- RK4 で 1 歩：状態 (q, i)、dq/dt = i、di/dt = (V(t) − R·i − q/C) / L
rlcStep :: St -> Double -> Double -> Double -> Double -> Double -> (Double -> Double) -> St
rlcStep (St q i) t h r l c v = St (q + (h / 6) * (a1 + 2 * a2 + 2 * a3 + a4)) (i + (h / 6) * (b1 + 2 * b2 + 2 * b3 + b4))
  where
    f tt qq ii = (ii, (v tt - r * ii - qq / c) / l)
    (a1, b1) = f t q i
    (a2, b2) = f (t + h / 2) (q + (h / 2) * a1) (i + (h / 2) * b1)
    (a3, b3) = f (t + h / 2) (q + (h / 2) * a2) (i + (h / 2) * b2)
    (a4, b4) = f (t + h) (q + h * a3) (i + h * b3)

-- ---- 式：交流 V0 sin(2πft) のときの電流の振幅 V0 / |Z|
ampFormula :: Double -> Double -> Double
ampFormula f r = let w = 2 * pi * f in v0 / hyp r (w * lH - 1 / (w * cF))

-- ---- 計算：静かな状態から RK4 で落ち着くまで（減衰の時間 2L/R の 12 倍）進め、最後の 3 周期の電流の最大値
steadyAmp :: Double -> Double -> Double
steadyAmp f r = go (St 0 0) 0 0 (0 :: Int)
  where
    tp = 1 / f
    dt = tp / 60
    ttot = max (12 * ((2 * lH) / r)) (10 * tp)
    n = ceiling (ttot / dt) :: Int
    v tt = v0 * sin (2 * pi * f * tt)
    go !s !t !mx !k
      | k >= n = mx
      | otherwise = let s' = rlcStep s t dt r lH cF v
                        t' = t + dt
                    in go s' t' (if t' > ttot - 3 * tp then max mx (abs (sI s')) else mx) (k + 1)

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
