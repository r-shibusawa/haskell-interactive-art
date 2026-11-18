-- ============================================
-- 54-10 モーターと発電機 ─ 磁場の中で回るコイル
-- コイルを通る磁束 Φ = NBA cos θ。回すと起電力 V = −dΦ/dt = NBAω sin θ（交流）→ 発電機
-- 逆に電流を流すと力のモーメント τ = NBA·I·sin θ で回る → モーター。整流子は半回転ごとに電流の向きを切りかえる
-- 操作：1 = 発電機 / 2 = モーター / K = 整流子 あり・なし / ←→ = 回す速さ（2 は電池の電圧）/ ↑↓ = つなぐ抵抗 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nT, bF, area :: Double
nT = 50                                -- ★ 巻き数
bF = 0.2                               -- ★ 磁束密度 [T]
area = 0.01                            -- ★ コイルの面積 [m²]（10 cm × 10 cm）

nba :: Double
nba = nT * bF * area                   -- = 0.1 Wb

rCoil :: Double
rCoil = 2                              -- コイルの抵抗 [Ω]

jRot :: Double
jRot = 2e-4                            -- ★ 回る部分の慣性モーメント [kg·m²]

friction :: Double
friction = 2e-5                        -- ★ 軸のまさつ [N·m·s]

histCap :: Int
histCap = 400                          -- グラフに残す点の数

data World = World
  { wMode   :: !Int                    -- 1 = 発電機、2 = モーター
  , wComm   :: !Bool                   -- 整流子
  , wOmG    :: !Double                 -- 発電機を回す角速度 [rad/s]
  , wVbat   :: !Double                 -- 電池の電圧 [V]
  , wRload  :: !Double                 -- つないだ抵抗 [Ω]
  , wTh     :: !Double                 -- 角度 θ
  , wOm     :: !Double                 -- 角速度 ω
  , wHv     :: !(Seq Double)           -- グラフ：電圧
  , wHi     :: !(Seq Double)           -- グラフ：電流
  , wHw     :: !(Seq Double)           -- グラフ：回転数
  , wEIn    :: !Double                 -- 電池のエネルギー [J]
  , wEHeat  :: !Double                 -- 熱 [J]
  , wI      :: !Double                 -- いまの電流
  , wTau    :: !Double                 -- いまの力のモーメント
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-10 Motor and generator" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 False (2 * pi * 1) 6 10 0 0 Seq.empty Seq.empty Seq.empty 0 0 0 0 []

-- 1 フレームを 40 回に分けて進める
step :: Float -> World -> World
step dt w0 = w2
  where
    n = 40 :: Int
    h = realToFrac (min dt 0.05) / fromIntegral n
    (w1, emfOut) = foldl' sub (w0, 0) [1 .. n]
    sub (w, _) _
      | wMode w == 1 =
          let th = wTh w + wOmG w * h
              (vout, i, tau) = genState th (wOmG w) (wComm w) (wRload w)
          in (w { wTh = th, wOm = wOmG w, wI = i, wTau = tau }, vout)
      | otherwise =
          let (th, om) = motorStep (wTh w, wOm w) h (wVbat w) (wComm w)
              (vapp, i, tau) = motorState th om (wVbat w) (wComm w)
          in (w { wTh = th, wOm = om, wI = i, wTau = tau
                , wEIn = wEIn w + vapp * i * h, wEHeat = wEHeat w + i * i * rCoil * h }, vapp)
    th1 = jsRem (jsRem (wTh w1) (2 * pi) + 2 * pi) (2 * pi)
    w2 = w1 { wTh = th1
            , wHv = pushCap histCap (if wMode w1 == 1 then emfOut else nba * wOm w1 * sin th1) (wHv w1)
            , wHi = pushCap histCap (wI w1) (wHi w1)
            , wHw = pushCap histCap (wOm w1 / (2 * pi)) (wHw w1) }

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ drawMachine env w (envW env * 0.3) (envH env * 0.42)
  , plotFrame g1 (if gen then "Output voltage [V]" ++ (if wComm w then " (commutator: one direction)" else " (AC)") else "Back EMF NBA*omega*sin(theta) [V]") (-vmax) vmax
  , Color (rgb255 255 200 90) (plotTrace 2 g1 (toList (wHv w)) (-vmax) vmax histCap)
  , plotFrame g2 "Current I [A]" (-imax) imax
  , Color (rgb255 110 210 255) (plotTrace 2 g2 (toList (wHi w)) (-imax) imax histCap)
  , plotFrame g3 "Speed [rev/s]" 0 25
  , Color (rgb255 150 255 170) (plotTrace 2 g3 (toList (wHw w)) 0 25 histCap) ]
  -- ---- 数字
  ++ [ say s gx (570 + 24 * fromIntegral k) 15 (gray 0.92) | (k, s) <- zip [0 :: Int ..] (concatMap (wordWrap (envW env - gx - 10) 15) lns) ]
  ++ [ say ("1 generator  2 motor | K commutator (" ++ (if wComm w then "on" else "off") ++ ")  Left/Right "
            ++ (if gen then "turning speed  Up/Down load resistor" else "battery voltage") ++ "  R reset") 20 34 17 (gray 0.92) ]
  ++ [ say s 20 (58 + 18 * fromIntegral k) 14 (gray 0.67) | (k, s) <- zip [0 :: Int ..] (wordWrap (envW env - 40) 14 note) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    gen = wMode w == 1
    gx = envW env * 0.56
    gw = envW env * 0.38
    vmax = if gen then nba * wOmG w * 1.1 + 0.01 else wVbat w * 1.2
    imax = if gen then vmax / (rCoil + wRload w) else wVbat w / rCoil * 1.1
    g1 = (gx, 140, gw, 110)
    g2 = (gx, 290, gw, 110)
    g3 = (gx, 440, gw, 80)
    lns = [ "N = " ++ showJ nT ++ " turns, B = " ++ showJ bF ++ " T, A = " ++ showJ (area * 1e4) ++ " cm^2 -> NBA = " ++ fmt 2 nba ++ " Wb"
          , "Angle theta = " ++ show (round (wTh w * 180 / pi) :: Int) ++ " deg   speed " ++ fmt 2 (wOm w / (2 * pi)) ++ " rev/s" ]
          ++ if gen
               then [ "Peak voltage NBA*omega = " ++ fmt 3 (nba * wOmG w) ++ " V   load resistor " ++ showJ (wRload w) ++ " ohm"
                    , "Torque needed tau = NBA*I*sin(theta) = " ++ sci 3 (wTau w) ++ " N*m (Lenz: generating makes it harder to turn)" ]
               else [ "Battery " ++ showJ (wVbat w) ++ " V   current " ++ fmt 3 (wI w) ++ " A   tau = " ++ sci 3 (wTau w) ++ " N*m"
                    , "Battery energy " ++ fmt 2 (wEIn w) ++ " J -> heat " ++ fmt 2 (wEHeat w) ++ " J + rotation (and friction)" ]
    note = "Red on the left = N pole, blue on the right = S pole (field from left to right). The coil turns about the vertical axis. "
           ++ "theta = angle between the coil's normal and the field."
           ++ (if not gen && not (wComm w) then " No commutator: the torque reverses every half turn, so the coil stops at theta = 180 deg." else "")

-- 磁石・磁場・回るコイル・電流の点
drawMachine :: Env -> World -> Float -> Float -> Picture
drawMachine env w x y = Pictures $
  [ Color (rgb255 220 80 70) (rectTL (x - 280) (y - 120) 70 240)
  , Color (rgb255 70 130 230) (rectTL (x + 210) (y - 120) 70 240)
  , sayC "N" (x - 245) (y + 10) 28 white
  , sayC "S" (x + 245) (y + 10) 28 white ]
  ++ [ Color (rgba255 120 140 170 90) (arrowTo 1 6 (x - 205, yy) (x + 205, yy)) | yy <- [y - 100, y - 75 .. y + 100] ]
  -- コイルの 4 つの角（面の法線 n = (cos θ, 0, sin θ)、面の中の横の向き t = (−sin θ, 0, cos θ)）
  ++ [ Color (rgba255 200 130 60 (round (40 + 60 * lit))) (Polygon [a, b, c, d])
     , Color (rgb255 230 150 70) (thickPath 5 [a, b, c, d, a])
     , Color (gray 0.78) (thickLine 3 (x, y - hh / 2 - 50) (x, y + hh / 2 + 50)) ]
  -- 電流の点（向き = I の符号）
  ++ [ Color (rgb255 255 230 120) (circleD (fst p0 + (fst p1 - fst p0) * u) (snd p0 + (snd p1 - snd p0) * u) 6)
     | abs i > 1e-3, (p0, p1) <- zip path (drop 1 path), k <- [0 .. 3 :: Int]
     , let u = f2 (jsRem (jsRem ((fromIntegral k + ph) / 4) 1 + 1) 1) ]
  -- 整流子・ブラシ
  ++ [ say (if wComm w then "Commutator: swaps the connection every half turn" else "Slip rings: no swapping")
           (x - 90) (y + hh / 2 + 80) 14 (if wComm w then rgb255 240 200 120 else gray 0.63) ]
  where
    ww = 150
    hh = 170
    th = f2 (wTh w)
    tx = -(sin th)
    tz = cos th
    pt s v = (x + s * (ww / 2) * tx, y + v * (hh / 2) - s * (ww / 2) * tz * 0.18)
    a = pt (-1) (-1)
    b = pt 1 (-1)
    c = pt 1 1
    d = pt (-1) 1
    lit = abs (cos th)
    path = [a, b, c, d, a]
    i = wI w
    ph = jsRem (fromIntegral (envFrame env) * 0.12 * signum i * min 1 (abs i * 3)) 1

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12" -> let m = if c == '1' then 1 else 2
                       in w { wMode = m, wOm = 0, wTh = if m == 2 then 0.3 else 0
                            , wHv = Seq.empty, wHi = Seq.empty, wHw = Seq.empty, wEIn = 0, wEHeat = 0 }
    | c `elem` "kK" -> w { wComm = not (wComm w) }
    | c `elem` "rR" -> w { wComm = False, wOmG = 2 * pi, wVbat = 6, wRload = 10, wTh = 0.3, wOm = 0, wEIn = 0, wEHeat = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> arrowLR 1
  EventKey (SpecialKey KeyLeft) Down _ _ -> arrowLR (-1)
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRload = min 100 (wRload w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRload = max 1.25 (wRload w / 2) }
  _ -> w
  where
    arrowLR d
      | wMode w == 1 = w { wOmG = clampv 0 (2 * pi * 6) (wOmG w + d * pi) }
      | otherwise = w { wVbat = clampv 0 12 (wVbat w + d) }

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("generator: -dPhi/dt (numerical derivative) = NBA*omega*sin(theta)", ok1)
  , ("peak voltage = NBA*omega", nearRel mx (nba * w) 1e-4)
  , ("average power in the load = (NBA*omega)^2 R / (2 (r + R)^2) (within 0.1%)", nearRel pAvg (pTot * rl / (rCoil + rl)) 0.001)
  , ("work to turn it tau*omega = all the generated power (Lenz's law)", nearRel (tq * w) pTot 0.001)
  , ("without a commutator the battery's torque averages 0 over one turn", abs avg < 1e-12)
  , ("energy: battery = heat + rotational KE + friction (within 0.5%)", nearRel eIn (eH + ke + eF) 0.005)
  , ("motor with commutator: small friction -> omega -> 4V/(pi*NBA) (back EMF average = 8/pi^2 of battery), within 3%"
    , nearRel omEnd ((4 * 6) / (pi * nba)) 0.03) ]
  where
    w = 2 * pi * 3
    phi tt = nba * cos (w * tt)
    dd = 1e-7
    samples = [ (num, ex) | k <- [0 .. 359 :: Int], let t = fromIntegral k / (360 * 3)
                          , let num = -(phi (t + dd) - phi (t - dd)) / (2 * dd); ex = nba * w * sin (w * t) ]
    ok1 = all (\(num, ex) -> abs (num - ex) <= 1e-6 * nba * w) samples
    mx = foldl' max 0 (map fst samples)
    n = 10000 :: Int
    rl = 10
    (pAvg, tq) = foldl' (\(p, q) k -> let (vout, _, tau) = genState ((2 * pi * (fromIntegral k + 0.5)) / fromIntegral n) w False rl
                                      in (p + (vout * vout) / rl / fromIntegral n, q + tau / fromIntegral n)) (0, 0) [0 .. n - 1]
    pTot = ((nba * w) ** 2) / (2 * (rCoil + rl))
    avg = foldl' (\s k -> s + (nba * sin ((2 * pi * (fromIntegral k + 0.5)) / fromIntegral n) * 6) / rCoil / fromIntegral n) 0 [0 .. n - 1]
    hh = 1e-4
    motorRun (st@(th, om), ei, eh, ef) _ =
      let (vapp, i, _) = motorState th om 6 True
          st' = motorStep st hh 6 True
      in st' `seq` (st', ei + vapp * i * hh, eh + i * i * rCoil * hh, ef + friction * om * om * hh)
    ((_, omEnd), eIn, eH, eF) = foldl' motorRun ((0.3, 0), 0, 0, 0) [1 .. 40000 :: Int]
    ke = 0.5 * jRot * omEnd * omEnd

-- ---------- やってみよう ----------
-- 1. 発電機で ←→ を押して回す速さを 2 倍にすると、電圧の山は？ 周期は？
-- 2. ↑↓ でつなぐ抵抗を小さくすると電流が大きくなり、回すのが重くなる（τ）。自転車の発電機と同じ
-- 3. K で整流子をつけると、出てくる電圧の形はどう変わる？（直流の発電機）
-- 4. モーターで整流子をなしにすると、どこで止まる？ なぜ？
-- 5. モーターの回転数が上がると電流が減るのはなぜ？（逆起電力）。電圧を 2 倍にすると回転数は？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 発電機：角度 θ、角速度 ω、抵抗 Rl をつないだとき → (出力電圧, 電流, 回すのに必要な τ)
-- 起電力 e = NBAω sin θ（整流子ありなら |…|）、電流 I = e/(r + Rl)、出力電圧 vout = I·Rl、τ = NBA·I·sin θ
genState :: Double -> Double -> Bool -> Double -> (Double, Double, Double)
genState t w comm rl = (i * rl, i, nba * i * s * sw)
  where
    s = sin t
    sw = if comm then (if s >= 0 then 1 else -1) else 1
    e = nba * w * s * sw
    i = e / (rCoil + rl)

-- ---- モーター：電池 V（整流子ありなら sin θ の符号で向きを切りかえ）→ (かかる電圧, 電流, τ)
-- I = (V − 逆起電力)/r、τ = NBA·I·sin θ
motorState :: Double -> Double -> Double -> Bool -> (Double, Double, Double)
motorState t w v comm = (vapp, i, nba * i * s)
  where
    s = sin t
    sw = if comm then (if s >= 0 then 1 else -1) else 1
    vapp = v * sw
    i = (vapp - nba * w * s) / rCoil

-- ---- RK4 で 1 歩：dθ/dt = ω、J dω/dt = τ − まさつ·ω
motorStep :: (Double, Double) -> Double -> Double -> Bool -> (Double, Double)
motorStep (th, om) h v comm =
  ( th + (h / 6) * (k1a + 2 * k2a + 2 * k3a + k4a)
  , clampv (-400) 400 (om + (h / 6) * (k1b + 2 * k2b + 2 * k3b + k4b)) )
  where
    f t w = let (_, _, tau) = motorState t w v comm in (w, (tau - friction * w) / jRot)
    (k1a, k1b) = f th om
    (k2a, k2b) = f (th + (h / 2) * k1a) (om + (h / 2) * k1b)
    (k3a, k3b) = f (th + (h / 2) * k2a) (om + (h / 2) * k2b)
    (k4a, k4b) = f (th + h * k3a) (om + h * k3b)

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: RealFrac a => a -> a -> a
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

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

-- 最後に足して、cap 個をこえたら古いものを捨てる
pushCap :: Int -> Double -> Seq Double -> Seq Double
pushCap cap v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃えの文字（y は文字の下）
sayC :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c

-- 幅 maxW に収まるように単語の切れ目で折り返す
wordWrap :: Float -> Float -> String -> [String]
wordWrap maxW size s = reverse (go (words s) "" [])
  where
    go [] cur acc = if null cur then acc else cur : acc
    go (x : xs) cur acc
      | null cur = go xs x acc
      | fromIntegral (length cur + 1 + length x) * size * 0.62 <= maxW = go xs (cur ++ " " ++ x) acc
      | otherwise = go xs x (cur : acc)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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

-- 左上が (x, y) の長方形の枠（細い線）
frameTL :: Float -> Float -> Float -> Float -> Picture
frameTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- グラフの道具：枠 (x, y, w, h)。lo〜hi の範囲で、数字の列を左から右へ（cap 個ぶん）
plotFrame :: (Float, Float, Float, Float) -> String -> Double -> Double -> Picture
plotFrame (x, y, w, h) title lo hi = Pictures $
  [ Color (rgb255 70 80 100) (frameTL x y w h) ]
  ++ [ Color (rgba255 70 80 100 160) (Line [(x, y0), (x + w, y0)]) | lo < 0 && hi > 0, let y0 = y + h * f2 (hi / (hi - lo)) ]
  ++ [ say title (x + 4) (y - 6) 14 (gray 0.78)
     , say (sci 2 hi) (x + w + 4) (y + 12) 14 (gray 0.55)
     , say (sci 2 lo) (x + w + 4) (y + h) 14 (gray 0.55) ]

plotTrace :: Float -> (Float, Float, Float, Float) -> [Double] -> Double -> Double -> Int -> Picture
plotTrace wt (x, y, w, h) arr lo hi cap =
  polyline wt [ (x + fromIntegral i / fromIntegral (cap - 1) * w, y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h) | (i, v) <- zip [0 :: Int ..] arr ]

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
