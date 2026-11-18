-- ============================================
-- 55-06 冷凍機とヒートポンプ ─ サイクルを逆に回すと、低温から高温へ熱を運べる
-- 仕事 W を使って、外（低温 Tc）から熱 Qc をくみ上げ、部屋（高温 Th）へ Qh = Qc + W を出す
-- 成績係数 COP = Qh / W。カルノーの上限は Th/(Th − Tc)（逆カルノーサイクルを数値で回して確かめる）
-- 下：同じ家を「ヒートポンプ」と「電気ヒーター（COP = 1）」で温めて、使った電気を比べる
-- 操作：← → 外の気温 / ↑↓ 設定温度 / M 暖房 ⇔ 冷房 / スペース 一時停止 / R やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nMol, rGas :: Double
nMol = 1
rGas = 8.314

eff, dtHx, ua, cRoom, pIn, dtSim :: Double
eff = 0.45                 -- ★ 本物の機械はカルノーの何倍の COP か（0.4〜0.5 くらい）
dtHx = 5                   -- ★ 熱交換器の温度差 [K]（部屋より 5 K 熱く、外より 5 K 冷たくしないと熱が流れない）
ua = 50                    -- ★ 家の熱の逃げやすさ [W/K]
cRoom = 4e6                -- ★ 家の熱容量 [J/K]
pIn = 2000                 -- ★ ヒートポンプ・ヒーターの電力 [W]
dtSim = 30                 -- ★ 1 回の計算で進む時間 [秒]（1 コマ 20 回 = 10 分）

-- 家：温度 [°C]・スイッチ・使った電気 [kWh]・ヒートポンプか
data Room = Room { rT :: !Double, rOn :: !Bool, rKwh :: !Double, rPump :: !Bool }

-- 外の気温・設定温度・冷房か
data Setting = Setting { sOut, sSet :: !Double, sCool :: !Bool }

data World = World
  { wSet     :: !Setting
  , wPaused  :: !Bool
  , wRooms   :: !(Room, Room)
  , wLog     :: [(Double, Double, Double, Double)]   -- (温度 2 つ, 電気 2 つ)（新しい順、300 個まで）
  , wSimTime :: !Double
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-06 Refrigerator and heat pump" world0)
  { skBack  = rgbC 22 24 30
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetRooms (World (Setting 0 20 False) False (rm0, rm0) [] 0 [])
  where rm0 = Room 0 False 0 True

resetRooms :: World -> World
resetRooms w = w { wRooms = (Room t False 0 True, Room t False 0 False), wLog = [], wSimTime = 0 }
  where t = sOut (wSet w)

-- 逆カルノーサイクル（04・05 と同じ数値積分）。tc で等温膨張して熱を吸い、th で等温圧縮して熱を出す
legIso :: Double -> Double -> Double -> Int -> Double      -- 等温：Q = W
legIso v1 v2 t nsub = foldl' (\w i -> let a = v1 + ((v2 - v1) * fromIntegral i) / fromIntegral nsub
                                          b = v1 + ((v2 - v1) * fromIntegral (i + 1)) / fromIntegral nsub
                                      in w + 0.5 * nMol * rGas * t * (1 / a + 1 / b) * (b - a)) 0 [0 .. nsub - 1]

legAdi :: Double -> Double -> Double -> Double -> Int -> Double
legAdi v1 t1 v2 g nsub = foldl' (\w i -> let a = v1 + ((v2 - v1) * fromIntegral i) / fromIntegral nsub
                                             b = v1 + ((v2 - v1) * fromIntegral (i + 1)) / fromIntegral nsub
                                             ta = t1 * (v1 / a) ** (g - 1)
                                             tb = t1 * (v1 / b) ** (g - 1)
                                         in w + 0.5 * nMol * rGas * (ta / a + tb / b) * (b - a)) 0 [0 .. nsub - 1]

-- 戻り値 (Qc, Qh, W, COP)
reverseCarnot :: Double -> Double -> Int -> (Double, Double, Double, Double)
reverseCarnot th tc nsub = (qc, qh, wnet, qh / wnet)
  where
    g = 1.4
    e = 1 / (g - 1)
    va = 10; vb = 25
    vc = vb * (th / tc) ** e
    vd = va * (th / tc) ** e
    -- 逆回り：A → D（断熱膨張で冷える）→ C（tc で等温膨張、熱 Qc を吸う）→ B（断熱圧縮）→ A（th で等温圧縮、熱を出す）
    w1 = legAdi va th vd g nsub
    qc = legIso vd vc tc nsub
    w3 = legAdi vc tc vb g nsub
    qh = -(legIso vb va th nsub)
    wnet = -(w1 + qc + w3 - qh)        -- 外からされた仕事 = −（気体がした仕事の合計）

copHeat, copCool :: Double -> Double -> Double
copHeat tRoom tOut = let th = tRoom + 273.15 + dtHx; tc = tOut + 273.15 - dtHx in if th > tc then min 20 ((eff * th) / (th - tc)) else 20
copCool tRoom tOut = let tc = tRoom + 273.15 - dtHx; th = tOut + 273.15 + dtHx in if th > tc then min 20 ((eff * tc) / (th - tc)) else 20

-- 家の温度：C dT/dt = 入る熱 − UA (T − T外)。サーモスタットは ±0.5 °C で入り切り
roomStep :: Setting -> Double -> Room -> Room
roomStep st dt r = r { rT = clampv (-40) 60 t', rOn = on, rKwh = kwh }
  where
    tSet = sSet st; tOut = sOut st; cool = sCool st
    on | not cool = if rT r > tSet + 0.5 then False else if rT r < tSet - 0.5 then True else rOn r
       | otherwise = if rT r < tSet - 0.5 then False else if rT r > tSet + 0.5 then True else rOn r
    q | not on = 0
      | rPump r = pIn * (if cool then -(copCool (rT r) tOut) else copHeat (rT r) tOut)
      | cool = 0
      | otherwise = pIn
    kwh = if on && (rPump r || not cool) then rKwh r + (pIn * dt) / 3.6e6 else rKwh r
    t' = rT r + ((q - ua * (rT r - tOut)) * dt) / cRoom

step :: Env -> Float -> World -> IO World
step _ _ w
  | wPaused w = pure w
  | otherwise = pure w { wRooms = (a, b), wSimTime = st, wLog = take 300 ((rT a, rT b, rKwh a, rKwh b) : wLog w) }
  where
    ((a, b), st) = foldl' (\((!x, !y), !t) _ -> ((roomStep (wSet w) dtSim x, roomStep (wSet w) dtSim y), t + dtSim)) (wRooms w, wSimTime w) [1 .. 20 :: Int]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- エネルギーの流れの図（幅 = 量）：W = 1 として Qc = COP − 1、Qh = COP（暖房）
  [ Color (rgbaC 255 110 60 200) (rectTL fx fy fw 50), Color (rgbaC 70 140 255 200) (rectTL fx (fy + 230) fw 50)
  , say ((if cool then "outside" else "room") ++ " (hot Th = " ++ fmt 0 (th - 273.15) ++ " C)") (fx + 14) (fy + 31) 16 white
  , say ((if cool then "room" else "outside") ++ " (cold Tc = " ++ fmt 0 (tc - 273.15) ++ " C)") (fx + 14) (fy + 261) 16 white
  , Color (grayC 200) (at (mx, fy + 140) (disc 35)), sayC "pump" mx (fy + 145) 14 (grayC 30)
  , Color (rgbaC 90 170 255 180) (rectTL (mx - f2 qcF * sc / 2) (fy + 175) (f2 qcF * sc) 55)
  , Color (rgbaC 255 140 70 180) (rectTL (mx - f2 qhF * sc / 2) (fy + 50) (f2 qhF * sc) 55)
  , Color (rgbaC 240 220 120 200) (rectTL (mx + 40) (fy + 130) 120 sc)
  , say ("Qh = " ++ fmt 2 qhF) (mx + f2 qhF * sc / 2 + 8) (fy + 85) 15 (grayC 235)
  , say ("Qc = " ++ fmt 2 qcF) (mx + f2 qcF * sc / 2 + 8) (fy + 210) 15 (grayC 235)
  , say "W = 1 (electricity)" (mx + 168) (fy + 148) 15 (grayC 235)
  -- ---- COP と外の気温のグラフ
  , plotAxes g "outside temperature [C]" "COP" [-20, 0, 20, 40] [0, 5, 10]
  , Color (rgbC 255 150 70) (polyline 2 [ (gX g t, gY g (copHeat tSet t)) | k <- [0 .. 120 :: Int], let t = -20 + fromIntegral k / 2 ])
  , Color (rgbC 110 180 255) (polyline 2 [ (gX g t, gY g (copCool tSet t)) | k <- [0 .. 120 :: Int], let t = -20 + fromIntegral k / 2 ])
  , Color white (at (gX g tOut, gY g cop) (disc 5))
  , say "heating COP" (pX g + 8) (pY g + 18) 14 (rgbC 255 150 70)
  , say "cooling COP" (pX g + 8) (pY g + 36) 14 (rgbC 110 180 255)
  -- ---- 2 つの家の温度と電気
  , plotAxes g2 "time (1 point = 10 min)" "room temperature [C]" [] [-20, 0, 20, 40]
  , Color (rgbaC 255 255 255 60) (Line [(pX g2, gY g2 tSet), (pX g2 + pW g2, gY g2 tSet)])
  , Color (rgbaC 90 160 255 80) (Line [(pX g2, gY g2 tOut), (pX g2 + pW g2, gY g2 tOut)])
  , Color (rgbC 255 160 80) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 v) | (k, (v, _, _, _)) <- lg ])
  , Color (rgbC 230 230 120) (polyline 1.5 [ (gX g2 (fromIntegral k), gY g2 v) | (k, (_, v, _, _)) <- lg ])
  , plotAxes g3 "time" "electricity used [kWh]" [] [0, read1 (kmax * 1.1)]
  , Color (rgbC 255 160 80) (polyline 2 [ (gX g3 (fromIntegral k), gY g3 v) | (k, (_, _, v, _)) <- lg ])
  , Color (rgbC 230 230 120) (polyline 2 [ (gX g3 (fromIntegral k), gY g3 v) | (k, (_, _, _, v)) <- lg ])
  , say ("Heat pump " ++ fmt 2 (rKwh ra) ++ " kWh (orange)") (pX g3) (pY g3 + pH g3 + 50) 14 (grayC 235)
  , say ("Electric heater " ++ fmt 2 (rKwh rb) ++ " kWh (yellow)" ++ (if cool then "  - cannot cool" else "")) (pX g3) (pY g3 + pH g3 + 70) 14 (grayC 235)
  , say ("Elapsed " ++ fmt 1 (wSimTime w / 3600) ++ " hours") (pX g3) (pY g3 + pH g3 + 90) 14 (grayC 235)
  -- ---- 文字
  , say ((if cool then "Cooling (refrigerator)" else "Heating (heat pump)") ++ "   outside " ++ fmt 0 tOut ++ " C   set " ++ fmt 0 tSet ++ " C   COP = " ++ fmt 2 cop
         ++ " (Carnot " ++ fmt 2 (if cool then tc / (th - tc) else th / (th - tc)) ++ " x " ++ show eff ++ ")") 20 36 18 (grayC 240)
  , say "Left/Right outside temperature   Up/Down set temperature   M heating <-> cooling   Space pause   R restart" 20 62 15 (grayC 175)
  , say ("Numeric reverse Carnot: Qh = " ++ fmt 1 rqh ++ " J  Qc = " ++ fmt 1 rqc ++ " J  W = " ++ fmt 1 rw ++ " J  Qh/W = " ++ fmt 3 rcop
         ++ "  (Th/(Th - Tc) = " ++ fmt 3 (th / (th - tc)) ++ ")") 20 86 14 (rgbC 255 200 120)
  , drawSelfTest env (wTest w) ]
  where
    st = wSet w
    cool = sCool st
    tOut = sOut st
    tSet = sSet st
    (ra, rb) = wRooms w
    th = (if cool then tOut + dtHx else tSet + dtHx) + 273.15
    tc = (if cool then tSet - dtHx else tOut - dtHx) + 273.15
    (rqc, rqh, rw, rcop) = reverseCarnot th tc 200
    cop = if cool then copCool tSet tOut else copHeat tSet tOut
    fx = 40; fy = 130
    fw = envW env * 0.42
    qhF = if cool then cop + 1 else cop
    qcF = qhF - 1
    sc = 26
    mx = fx + fw / 2
    g = graph (fx + 50) (fy + 340) (fw - 50) (envH env - fy - 420) (-20) 40 0 10
    g2 = graph (envW env * 0.52 + 40) 130 (envW env * 0.44) (envH env * 0.32) 0 300 (-20) 40
    lg = zip [0 :: Int ..] (reverse (wLog w))
    kmax = foldl' (\m (_, _, a, b) -> max m (max a b)) 1 (wLog w)
    g3 = graph (pX g2) (pY g2 + pH g2 + 70) (pW g2) (envH env * 0.25) 0 300 0 (kmax * 1.1)
    read1 v = fromIntegral (floor (v * 10 + 0.5) :: Int) / 10      -- +toFixed(1)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSet = st { sOut = min (if cool then 38 else 16) (sOut st + 2) } }   -- 暖房は外が部屋より寒いときだけ、冷房は暑いときだけ
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSet = st { sOut = max (if cool then 24 else -20) (sOut st - 2) } }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSet = st { sSet = min (if cool then 28 else 24) (sSet st + 1) } }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSet = st { sSet = max (if cool then 20 else 16) (sSet st - 1) } }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> let c' = not cool
                           o | c' && sOut st < 25 = 32
                             | not c' && sOut st > 15 = 0
                             | otherwise = sOut st
                       in resetRooms w { wSet = st { sCool = c', sOut = o } }
    | c `elem` "rR" -> resetRooms w
  _ -> w
  where st = wSet w
        cool = sCool st

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("reverse Carnot (25 C <- -5 C): numeric Qh/W = Th/(Th - Tc) = " ++ fmt 3 (298 / 30) ++ " (1e-5)", near cop (298 / 30) 1e-5)
  , ("energy conservation Qh = Qc + W", near qh (qc + wn) (1e-6 * qh))
  , ("Carnot: heating COP - cooling COP = 1, heating COP = 1/eta (eta = 1 - Tc/Th)", near (298 / 30 - 268 / 30) 1 1e-12 && near cop (1 / (1 - 268 / 298)) 1e-5)
  , ("entropy: Carnot Qh/Th - Qc/Tc = 0, real machine (EFF < 1) > 0"
    , near (qh / 298 - qc / 268) 0 1e-6 && (let c = eff * 298 / 30 in c / 298 - (c - 1) / 268 > 0))
  , ("house with " ++ fmt 0 pIn ++ " W all the time: steady temperature = outside + P/UA = " ++ fmt 2 (pIn / ua) ++ " C", near (rT r) (pIn / ua) 0.01)
  , ("same house for 2 days: heater electricity / heat pump electricity = " ++ fmt 2 ratio ++ " ~ COP " ++ fmt 2 c0 ++ " (10%)", abs (ratio / c0 - 1) < 0.1) ]
  where
    (qc, qh, wn, cop) = reverseCarnot 298 268 2000
    r = foldl' (\x _ -> roomStep (Setting 0 100 False) 60 x) (Room 0 True 0 False) [1 .. 20000 :: Int]
    (a, b) = foldl' (\(!x, !y) _ -> (roomStep s20 30 x, roomStep s20 30 y)) (Room 20 False 0 True, Room 20 False 0 False) [1 .. 2880 * 2 :: Int]
    s20 = Setting 0 20 False
    ratio = rKwh b / rKwh a
    c0 = copHeat 20 0

-- ---------- やってみよう ----------
-- 1. ← で外の気温を下げよう。COP はどう変わる？ 寒い日にヒートポンプが苦手なのはなぜ？
-- 2. 2 つの家の電気の量を比べよう。COP が 3 なら、電気は何分の 1？
-- 3. M で冷房に。外が暑いほど COP は？ 冷蔵庫の後ろが熱いのはなぜ？
-- 4. dtHx を 0 にすると COP はカルノーの値に近づく。熱交換器を大きくする意味は？
-- 5. ua（断熱の悪さ）を半分にすると、電気はどれだけ減る？

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0'
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin 0 v0'

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
