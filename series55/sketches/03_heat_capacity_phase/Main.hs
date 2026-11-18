-- ============================================
-- 55-03 熱容量と相変化 ─ 氷を一定の熱で温めると、0 °C と 100 °C で温度が止まる（潜熱）
-- 加えた熱 Q を「エンタルピー」として積み上げ、qHeat から温度と状態（氷・水・水蒸気の割合）を計算する
-- 右：2 つのものを混ぜたときの平衡温度。熱の合計が変わらない（熱量保存）温度を二分法で求める
-- 操作：↑↓ ヒーターの強さ / スペース 加熱開始・停止 / R 最初から / 1〜4 混ぜる側の値を選ぶ → ← → で変更 / 5 B の物質（水・鉄）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mass, tStart :: Double
mass = 1                   -- ★ 温める H₂O の質量 [kg]
tStart = -30               -- ★ はじめの氷の温度 [°C]

cIce, cWater, cSteam, cIron :: Double
cIce = 2100                -- ★ 比熱 [J/(kg·K)]
cWater = 4186
cSteam = 2010
cIron = 450

lF, lV :: Double
lF = 334000                -- ★ 融解熱 [J/kg]
lV = 2257000               -- ★ 蒸発熱 [J/kg]

speed :: Double
speed = 4                  -- ★ 1 コマで進む時間 [秒]

data Phase = Ice | Water | Steam deriving Eq
data Mat = MWater | MIron deriving Eq

-- 混ぜるもの：質量 [kg]・温度 [°C]・物質
data Thing = Thing { tm :: !Double, tt :: !Double, tmat :: !Mat }

data World = World
  { wPower   :: !Double            -- ヒーター [W]
  , wQ       :: !Double            -- 加えた熱 [J]
  , wSimT    :: !Double            -- 時間 [秒]
  , wRunning :: !Bool
  , wTrace   :: [(Double, Double)] -- (Q, 温度)（新しい順）
  , wA, wB   :: !Thing
  , wSel     :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-03 Heat capacity and phase change" world0)
  { skBack  = rgbC 26 24 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2000 0 0 False [] (Thing 1 80 MWater) (Thing 1 20 MWater) 1 []

-- 1 kg あたりのエンタルピー h(T)（0 °C の氷 = 0 を基準）
hWater :: Double -> Phase -> Double
hWater t ph = case ph of
  Ice -> cIce * t
  Water -> lF + cWater * t
  Steam -> lF + cWater * 100 + lV + cSteam * (t - 100)

-- 状態：温度・とけた割合・蒸発した割合
data State = State { sT, sMelt, sVapor :: !Double }

-- h → 温度と、とけた割合・蒸発した割合
stateOf :: Double -> State
stateOf h
  | h < 0 = State (h / cIce) 0 0
  | h < lF = State 0 (h / lF) 0
  | h < h1 = State ((h - lF) / cWater) 1 0
  | h < h1 + lV = State 100 1 ((h - h1) / lV)
  | otherwise = State (100 + (h - h1 - lV) / cSteam) 1 1
  where h1 = lF + cWater * 100

-- 相変化の途中でない状態
hOfT :: Double -> Double
hOfT t | t < 0 = hWater t Ice
       | t <= 100 = hWater t Water
       | otherwise = hWater t Steam

hMat :: Mat -> Double -> Double
hMat m t = if m == MIron then cIron * t else hOfT (min t 100)

-- 混ぜる：物質ごとの h(T) を足した「全体の熱」が変わらない温度を二分法で求める
-- 戻り値は (平衡温度, とけた割合)
mixEquilibrium :: Thing -> Thing -> (Double, Double)
mixEquilibrium a b
  | abs t < 1e-6 = (0, clampv 0 1 (hh / (tm a * lF + (if tmat b == MWater then tm b * lF else 0))))   -- ちょうど 0 °C：氷と水がまざった状態
  | otherwise = (t, if t > 0 then 1 else 0)
  where
    hh = tm a * hMat MWater (tt a) + tm b * hMat (tmat b) (tt b)
    go :: Int -> Double -> Double -> (Double, Double)
    go 0 lo hi = (lo, hi)
    go k lo hi = let mid = (lo + hi) / 2
                 in if tm a * hMat MWater mid + tm b * hMat (tmat b) mid < hh then go (k - 1) mid hi else go (k - 1) lo mid
    (l, h) = go 80 (-60) 300
    t = (l + h) / 2

h0 :: Double
h0 = hWater tStart Ice

qMax :: Double
qMax = mass * (hWater 140 Steam - h0)

step :: Env -> Float -> World -> IO World
step env _ w = pure w { wQ = q, wSimT = st, wTrace = tr }
  where
    (q0, st) | wRunning w = foldl' (\(!a, !b) _ -> (a + wPower w * speed / 10, b + speed / 10)) (wQ w, wSimT w) [1 .. 10 :: Int]   -- 加えた熱 Q = P·t
             | otherwise = (wQ w, wSimT w)
    q = min q0 qMax
    s = stateOf (h0 + q / mass)
    tr | wRunning w && envFrame env `mod` 2 == 0 = take 3000 ((q, sT s) : wTrace w)
       | otherwise = wTrace w

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- ビーカーの絵
  [ Color (grayC 200) (thickPath 3 [(bx, by), (bx, by + bh), (bx + bw, by + bh), (bx + bw, by)])
  , Color (rgbaC 70 140 230 180) (rectTL (bx + 3) (by + bh - level) (bw - 6) level) ]
  ++ [ Color (rgbaC 220 240 255 230) (rectTL (bx + 20 + fromIntegral (k `mod` 3) * 66) (by + bh - 50 - fromIntegral (k `div` 3) * 46)
                                             (50 * sqrt' iceFrac + 6) (40 * sqrt' iceFrac + 4)) | k <- [0 .. nIce - 1] ]
  ++ (if sT s >= 100 && sVapor s < 1
        then [ Color (rgbaC 255 255 255 160) (at (bx + 20 + fromIntegral ((k * 37 + fc * 3) `mod` (round bw - 40)), by + bh - modF (fromIntegral (k * 53 + fc * 2)) (max 10 level)) (disc 3.5)) | k <- [0 .. 11] ]
        else [])
  ++ (if sVapor s > 0
        then [ Color (withA ((70 * f2 (sVapor s) + 20) / 255) (rgbC 230 230 240)) (at (bx + 40 + fromIntegral k * 32, by - 30 - 20 * sin (fromIntegral fc * 0.05 + fromIntegral k)) (Scale 1 0.6 (disc 25))) | k <- [0 .. 5 :: Int] ]
        else [])
  ++ [ Color (rgbaC 255 120 40 (if wRunning w then 230 else 90)) (rectTL (bx - 10) (by + bh + 8) (bw + 20) 16) ]
  -- ---- グラフ：温度と加えた熱
  ++ [ plotAxes g "heat added Q [kJ]" "temperature [C]" [0, 500, 1000, 1500, 2000, 2500, 3000] [-40, 0, 50, 100, 150]
     , Color (rgbaC 255 255 255 50) (polyline 2 [ (gX g (q / 1000), gY g (sT (stateOf (h0 + q / mass)))) | k <- [0 .. 300 :: Int], let q = fromIntegral k / 300 * qMaxK * 1000 ])
     , Color (rgbC 255 150 70) (polyline 3 [ (gX g (a / 1000), gY g b) | (a, b) <- reverse (wTrace w) ])
     , Color (rgbC 255 210 120) (at (gX g (wQ w / 1000), gY g (sT s)) (disc 5))
     , say ("melting " ++ fmt 0 (mass * lF / 1000) ++ " kJ") (gX g (mass * (0 - h0) / 1000) + 4) (gY g 0 - 8) 14 (grayC 170)
     , say ("boiling " ++ fmt 0 (mass * lV / 1000) ++ " kJ") (gX g (mass * (hWater 100 Water - h0) / 1000) + 4) (gY g 100 - 8) 14 (grayC 170) ]
  -- ---- 混ぜる計算
  ++ [ say "Mix: final temperature? (heat kept)" mx (my - 20) 15 (grayC 240) ]
  ++ [ say (nm ++ "  " ++ v) mx (my + 14 + fromIntegral k * 26) 15 (if wSel w == k + 1 then rgbC 255 210 120 else grayC 190) | (k, (nm, v)) <- zip [0 :: Int ..] items ]
  ++ [ say ("5 B material: " ++ (if tmat b == MIron then "iron (c = 450)" else "water / ice")) mx (my + 118) 15 (grayC 190)
     , say ("Equilibrium " ++ fmt 2 et ++ " C") mx (my + 160) 20 (rgbC 255 180 100) ]
  ++ [ say ("(melted " ++ fmt 0 (em * 100) ++ "%)") (mx + 230) (my + 160) 15 (rgbC 255 180 100) | et == 0 ]
  ++ [ say ("A gave " ++ fmt 2 (lostA / 1000) ++ " kJ = B got " ++ fmt 2 (gainB / 1000) ++ " kJ") mx (my + 190) 14 (grayC 190)
     , Color (rgbC 255 120 60) (rectTL mx (my + 205) (200 * f2 (clampv 0 1 (abs lostA / 400000))) 14)
     , Color (rgbC 90 160 255) (rectTL mx (my + 225) (200 * f2 (clampv 0 1 (abs gainB / 400000))) 14) ]
  -- ---- 文字
  ++ [ say ("Heating " ++ fmt 0 mass ++ " kg of ice at " ++ fmt 0 (wPower w) ++ " W   time " ++ fmt 0 (wSimT w) ++ " s   Q = " ++ fmt 1 (wQ w / 1000) ++ " kJ   T = " ++ fmt 1 (sT s) ++ " C") 20 36 18 (grayC 240)
     , say ("Space heat " ++ (if wRunning w then "(on)" else "(stopped)") ++ "   Up/Down heater   R restart   | Mix: 1-4 select, Left/Right change, 5 material") 20 62 15 (grayC 175)
     , say ("Melted " ++ fmt 0 (sMelt s * 100) ++ "%   evaporated " ++ fmt 0 (sVapor s * 100) ++ "%   | while T stops, heat breaks the bonds between molecules (latent heat)") 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    fc = envFrame env
    s = stateOf (h0 + wQ w / mass)
    bx = 40; by = 150; bw = 230; bh = 300
    liquid = (1 - sVapor s) * sMelt s
    iceFrac = 1 - sMelt s
    level = bh * 0.55 * f2 (liquid + iceFrac * 0.4)
    nIce = floor (9 * iceFrac + 0.5) :: Int
    sqrt' = f2 . sqrt
    qMaxK = qMax / 1000
    g = graph (bx + bw + 90) 130 (envW env * 0.36) (envH env - 300) 0 qMaxK (-40) 150
    mx = pX g + pW g + 60
    my = 150
    a = wA w
    b = wB w
    (et, em) = mixEquilibrium a b
    items = [ ("1 A: water temperature", fmt 0 (tt a) ++ " C"), ("2 A: water mass", fmt 2 (tm a) ++ " kg")
            , ("3 B: " ++ (if tmat b == MIron then "iron" else if tt b < 0 then "ice" else "water") ++ " temperature", fmt 0 (tt b) ++ " C")
            , ("4 B: mass", fmt 2 (tm b) ++ " kg") ]
    (lostA, gainB)
      | et == 0 = let l = tm a * hMat MWater (tt a) - tm a * lF * em in (l, -l)
      | otherwise = (tm a * (hMat MWater (tt a) - hMat MWater et), tm b * (hMat (tmat b) et - hMat (tmat b) (tt b)))
    modF v m = v - m * fromIntegral (floor (v / m) :: Int)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wPower = min 10000 (wPower w + 500) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wPower = max 500 (wPower w - 500) }
  EventKey (SpecialKey KeyRight) Down _ _ -> change 1
  EventKey (SpecialKey KeyLeft) Down _ _ -> change (-1)
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> w { wQ = 0, wSimT = 0, wTrace = [], wRunning = False }
    | c `elem` "1234" -> w { wSel = fromEnum c - fromEnum '0' }
    | c == '5' -> let m = if tmat (wB w) == MIron then MWater else MIron
                  in w { wB = (wB w) { tmat = m, tt = if m == MIron then 200 else 20 } }
  _ -> w
  where
    a = wA w
    b = wB w
    change d = case wSel w of
      1 -> w { wA = a { tt = clampv 0 100 (tt a + 5 * d) } }
      2 -> w { wA = a { tm = clampv 0.1 5 (tm a + 0.1 * d) } }
      3 -> w { wB = b { tt = clampv (-30) (if tmat b == MIron then 500 else 100) (tt b + 5 * d) } }
      4 -> w { wB = b { tm = clampv 0.1 5 (tm b + 0.1 * d) } }
      _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("ice at -30 C -> steam at 120 C: " ++ fmt 1 (need / 1000) ++ " kJ (sum of the parts)", near (hWater 120 Steam - hWater (-30) Ice) need 1e-6)
  , ("temperature -> heat -> temperature round trip (-30, -5, 37, 99, 110 C)", all (\t -> near (sT (stateOf (hOfT t))) t 1e-9) [-30, -5, 37, 99, 110])
  , ("while melting T stays 0 C; half the latent heat melts exactly half", sT half == 0 && near (sMelt half) 0.5 1e-12)
  , ("1000 W melts 1 kg of ice at 0 C in L/P = 334 s (step 0.5 s)", near tMelt 334 0.5)
  , ("1 kg water 80 C + 1 kg water 20 C -> 50 C", near (fst (mixEquilibrium (Thing 1 80 MWater) (Thing 1 20 MWater))) 50 1e-6)
  , ("1 kg water 50 C + 0.1 kg ice 0 C -> " ++ fmt 2 ei ++ " C (melting heat first, then mixing)", near (fst e2) ei 1e-3 && near (fst e) (50 / 1.1) 1e-6)
  , ("0.5 kg iron 200 C into 1 kg water 20 C: heat lost by iron = heat gained by water (" ++ fmt 2 fe ++ " C)"
    , near lost gain 1e-3 && near fe ((0.5 * 450 * 200 + 4186 * 20) / (225 + 4186)) 1e-6) ]
  where
    need = 2100 * 30 + 334000 + 4186 * 100 + 2257000 + 2010 * 20
    half = stateOf (lF / 2)
    hz = hWater 0 Ice
    melt (!q, !t) | sMelt (stateOf (hz + q)) < 1 = melt (q + 1000 * 0.5, t + 0.5)
                  | otherwise = t
    tMelt = melt (0, 0)
    e = mixEquilibrium (Thing 1 50 MWater) (Thing 0.1 0 MWater)      -- 0 °C の「水」として入れた場合
    ei = (1 * cWater * 50 - 0.1 * lF) / (1.1 * cWater)
    e2 = mixEquilibrium (Thing 1 50 MWater) (Thing 0.1 (-1e-9) MWater)
    fe = fst (mixEquilibrium (Thing 1 20 MWater) (Thing 0.5 200 MIron))
    lost = 0.5 * cIron * (200 - fe)
    gain = cWater * (fe - 20)

-- ---------- やってみよう ----------
-- 1. スペースで加熱。温度が止まる 2 か所はどこ？ 長いのはどちら？ なぜ？
-- 2. ↑ でヒーターを強くすると、グラフの形は変わる？（横軸は熱、時間ではない）
-- 3. cWater を小さく（1000 に）すると、グラフのどこが変わる？ 水の比熱が大きいことの意味は？
-- 4. 混ぜる：3 を選び B の温度を 0 °C 以下にすると B は氷。とけきらないのはどんなとき？
-- 5. 5 で鉄にして、熱い鉄球を水に入れる。水の温度はあまり上がらないのはなぜ？

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
