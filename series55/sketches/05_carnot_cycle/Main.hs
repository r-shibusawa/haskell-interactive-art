-- ============================================
-- 55-05 カルノーサイクル ─ 2 つの熱源の間で働くいちばん効率のよいエンジン
-- 等温膨張（高温 Th）→ 断熱膨張 → 等温圧縮（低温 Tc）→ 断熱圧縮。PV 図と TS 図を同時に描く
-- 各過程を細かく分けて W = ∫P dV と Q を数値で足し、効率 η = W/Q_in を計算 → 1 − Tc/Th と一致する
-- オットー（ガソリンエンジン）・スターリングと、同じ温度の範囲で効率を比べる
-- 操作：1 カルノー　2 オットー　3 スターリング / ↑↓ 高温 Th / ← → 低温 Tc / G 気体（γ）/ スペース 一時停止
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nMol, rGas :: Double
nMol = 1
rGas = 8.314

vA, vRatio :: Double
vA = 10                    -- ★ いちばん小さい体積 [L]
vRatio = 3                 -- ★ 等温膨張で何倍にするか（スターリングも同じ）

gammas :: [Double]
gammas = [5 / 3, 7 / 5]

names :: [String]
names = ["", "Carnot", "Otto (petrol engine)", "Stirling (no regenerator)"]

-- 気体の状態（体積 [L]、温度 [K]）
data St = St { sV, sT :: !Double }

-- 1 つの過程：終わりの状態、線の点 (V, T)、W・Q・ΔS、種類と名前
data Leg = Leg { lS :: !St, lPts :: [(Double, Double)], lW, lQ, lDS :: !Double, lK :: !Int, lName :: String }

data Cycle = Cycle { cLegs :: [Leg], cW, cQin, cQout, cEta :: !Double, cEnd, cStart :: !St, cR, cCv, cDS :: !Double }

data World = World
  { wTh, wTc :: !Double
  , wG       :: !Int
  , wKind    :: !Int
  , wPaused  :: !Bool
  , wPhase   :: !Double          -- 0〜4（いまの過程と、その中の位置）
  , wCyc     :: Cycle
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-05 Carnot cycle" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPaused w then w else w { wPhase = let p = wPhase w + 0.004 in if p >= 4 then p - 4 else p })
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 600 300 1 1 False 0 (makeCycle 1 600 300 (7 / 5) 400) []

-- 1 つの過程を細かく分けて進める（04 と同じ）。k：1 等温 2 断熱 4 定積、target：体積（定積は温度）
runLeg :: St -> Int -> Double -> Double -> Double -> Int -> Leg
runLeg s k target cv g nsub = Leg a (reverse ps) w q ds k ""
  where
    v1 = sV s; t1 = sT s
    go !i a0 !w0 !q0 !s0 pts
      | i > nsub = (a0, w0, q0, s0, pts)
      | otherwise =
          let b | k == 4 = St v1 (t1 + ((target - t1) * fromIntegral i) / fromIntegral nsub)
                | otherwise = let v = v1 + ((target - v1) * fromIntegral i) / fromIntegral nsub
                              in St v (if k == 1 then t1 else t1 * (v1 / v) ** (g - 1))
              dW = 0.5 * (nMol * rGas) * (sT a0 / sV a0 + sT b / sV b) * (sV b - sV a0)
              dQ = nMol * cv * (sT b - sT a0) + dW
              pts' = if i `mod` 5 == 0 then (sV b, sT b) : pts else pts
          in go (i + 1) b (w0 + dW) (q0 + dQ) (s0 + dQ / (0.5 * (sT a0 + sT b))) pts'
    (a, w, q, ds, ps) = go 1 s 0 0 0 [(sV s, sT s)]

-- サイクルを作る：各過程の終わりの状態を計算してつなぐ
makeCycle :: Int -> Double -> Double -> Double -> Int -> Cycle
makeCycle kd th tc g nsub = Cycle legs w qin qout (w / qin) end start r cv (foldl' (\a l -> a + lDS l) 0 legs)
  where
    cv = (nMol * rGas) / (g - 1)
    e = 1 / (g - 1)
    r = if kd == 2 then sqrt (th / tc) ** e else 0
    (start, plan)
      | kd == 2 = let v2 = vA * r in (St v2 tc, [(2, vA, "adiabatic compression"), (4, th, "isochoric heating (burning)"), (2, v2, "adiabatic expansion"), (4, tc, "isochoric cooling (exhaust)")])
      | kd == 3 = (St vA th, [(1, vA * vRatio, "isothermal expansion (Th)"), (4, tc, "isochoric cooling"), (1, vA, "isothermal compression (Tc)"), (4, th, "isochoric heating")])
      | otherwise = let vb = vA * vRatio
                        vc = vb * (th / tc) ** e
                        vd = vA * (th / tc) ** e
                    in (St vA th, [(1, vb, "isothermal expansion (Th)"), (2, vc, "adiabatic expansion"), (1, vd, "isothermal compression (Tc)"), (2, vA, "adiabatic compression")])
    run (s, acc) (k, tg, nm) = let l = (runLeg s k tg cv g nsub) { lName = nm } in (lS l, l : acc)
    (end, rl) = foldl' run (start, []) plan
    legs = reverse rl
    w = foldl' (\a l -> a + lW l) 0 legs
    qin = foldl' (\a l -> a + max 0 (lQ l)) 0 legs
    qout = foldl' (\a l -> a + min 0 (lQ l)) 0 legs

-- 基準は 10 L、300 K
entropy :: Double -> Double -> Double -> Double
entropy v t cv = nMol * cv * log (t / 300) + nMol * rGas * log (v / 10)

draw :: Env -> World -> Picture
draw env w = view env $
  [ plotAxes pv "volume V [L] (log)" "pressure P [kPa] (log)" (filter (\v -> v > x0 pv && v < x1 pv) [10, 20, 50, 100, 200])
                                                              (filter (\v -> v > y0 pv && v < y1 pv) [10, 20, 50, 100, 200, 500, 1000])
  , plotAxes ts "entropy S [J/K]" "temperature T [K]" [jsRound (x0 ts), jsRound (x1 ts)] [0, tc, th]
  , Color (rgbaC 255 120 60 70) (Line [(pX ts, gY ts th), (pX ts + pW ts, gY ts th)])
  , Color (rgbaC 90 160 255 70) (Line [(pX ts, gY ts tc), (pX ts + pW ts, gY ts tc)])
  -- 囲まれた面積（PV では仕事 W、TS では正味の熱 = W）
  , Color (rgbaC 255 210 120 40) (fan [ (gX pv v, gY pv (pr v t)) | (v, t) <- allPts ])
  , Color (rgbaC 255 210 120 40) (fan [ (gX ts (entropy v t cv), gY ts t) | (v, t) <- allPts ]) ]
  ++ concat [ let c | lQ l > 1e-6 = rgbC 255 120 60
                    | lQ l < -1e-6 = rgbC 90 160 255
                    | otherwise = grayC 200
                  wt = if k == li then 5 else 2.5
              in [ Color c (polyline wt [ (gX pv v, gY pv (pr v t)) | (v, t) <- lPts l ])
                 , Color c (polyline wt [ (gX ts (entropy v t cv), gY ts t) | (v, t) <- lPts l ]) ]
            | (k, l) <- zip [0 ..] (cLegs cyc) ]
  ++ [ Color white (at (gX pv v, gY pv (pr v t)) (disc 6.5)) | Just (v, t) <- [cur] ]
  ++ [ Color white (at (gX ts (entropy v t cv), gY ts t) (disc 6.5)) | Just (v, t) <- [cur] ]
  -- ---- 効率の比較（同じ Th, Tc）
  ++ concat [ [ say nm bx (by + fromIntegral k * 30 + 14) 14 (if wKind w == k + 1 then rgbC 255 210 120 else grayC 170)
              , Color (if wKind w == k + 1 then rgbC 255 150 70 else rgbC 120 120 140) (rectTL (bx + 340) (by + fromIntegral k * 30) (400 * f2 v) 18)
              , say (fmt 1 (v * 100) ++ "%") (bx + 350 + 400 * f2 v) (by + fromIntegral k * 30 + 14) 14 (grayC 235) ]
            | (k, (nm, v)) <- zip [0 :: Int ..] bars ]
  ++ [ say ("Carnot limit " ++ fmt 3 (carnot * 100) ++ "%   dS over one loop = " ++ showEFloat (Just 1) (cDS cyc) "") 820 (by + 14) 15 (rgbC 255 200 120)
     , say "red = heat in   blue = heat out   white = adiabatic" 820 (by + 44) 15 (grayC 175) ]
  -- ---- 文字
  ++ [ say (getAt "" names (wKind w) ++ "   Th = " ++ fmt 0 th ++ " K   Tc = " ++ fmt 0 tc ++ " K   gamma = " ++ fmt 3 g ++ "   | now: " ++ maybe "" lName curLeg) 20 36 18 (grayC 240)
     , say "1 Carnot   2 Otto   3 Stirling   Up/Down hot Th   Left/Right cold Tc   G gas   Space pause" 20 62 15 (grayC 175)
     , say ("Numerical integration: W = " ++ fmt 1 (cW cyc) ++ " J   Q_in = " ++ fmt 1 (cQin cyc) ++ " J   Q_out = " ++ fmt 1 (cQout cyc)
            ++ " J   eta = W/Q_in = " ++ fmt 3 (cEta cyc * 100) ++ "%") 20 86 15 (rgbC 255 200 120)
     , drawSelfTest env (wTest w) ]
  where
    cyc = wCyc w
    th = wTh w
    tc = wTc w
    g = getAt (7 / 5) gammas (wG w)
    cv = cCv cyc
    pr v t = (nMol * rGas * t) / v
    allPts = concatMap lPts (cLegs cyc)
    vs = map fst allPts
    ps = map (uncurry pr) allPts
    ss = map (\(v, t) -> entropy v t cv) allPts
    mn xs = if null xs then 1 else minimum xs
    mxl xs = if null xs then 2 else maximum xs
    gw = (envW env - 120) / 2 - 40
    gh = envH env - 330
    pv = (graph 70 120 gw gh (mn vs * 0.85) (mxl vs * 1.15) (mn ps * 0.85) (mxl ps * 1.15)) { logX = True, logY = True }   -- 両対数（細い形が見やすい）
    ts = graph (70 + gw + 90) 120 (gw - 40) gh (mn ss - 2) (mxl ss + 2) 0 (th * 1.12)
    li = floor (wPhase w) :: Int
    f = wPhase w - fromIntegral li
    curLeg = case drop li (cLegs cyc) of
      (l : _) -> Just l
      [] -> Nothing
    cur = case curLeg of
      Just l | not (null (lPts l)) -> let n = length (lPts l) in Just (getAt (0, 0) (lPts l) (min (n - 1) (floor (f * fromIntegral n))))
      _ -> Nothing
    bx = 70
    by = envH env - 160
    carnot = 1 - tc / th
    otto = makeCycle 2 th tc g 100
    stir = makeCycle 3 th tc g 100
    bars = [ ("Carnot 1 - Tc/Th", carnot), ("Otto (r = " ++ fmt 2 (cR otto) ++ ") 1 - r^(1-g)", cEta otto), ("Stirling (no regenerator)", cEta stir) ]

-- 閉じた形を、重心からの三角形で塗る
fan :: [(Float, Float)] -> Picture
fan [] = Blank
fan pts@(p0 : _) = Pictures [ Polygon [c, a, b] | (a, b) <- zip pts (drop 1 pts ++ [p0]) ]
  where n = fromIntegral (length pts)
        c = (sum (map fst pts) / n, sum (map snd pts) / n)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down _ _ -> let w' = key k in w' { wCyc = makeCycle (wKind w') (wTh w') (wTc w') (getAt (7 / 5) gammas (wG w')) 400 }
  _ -> w
  where
    key k = case k of
      Char c | c `elem` "123" -> w { wKind = fromEnum c - fromEnum '0' }
             | c `elem` "gG" -> w { wG = 1 - wG w }
      SpecialKey KeyUp -> w { wTh = min 1200 (wTh w + 50) }
      SpecialKey KeyDown -> w { wTh = max (wTc w + 50) (wTh w - 50) }
      SpecialKey KeyRight -> w { wTc = min (wTh w - 50) (wTc w + 25) }
      SpecialKey KeyLeft -> w { wTc = max 100 (wTc w - 25) }
      SpecialKey KeySpace -> w { wPaused = not (wPaused w) }
      _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Carnot: numeric W/Q_in = 1 - Tc/Th = 50% (1e-6)", near (cEta c) 0.5 1e-6)
  , ("back to the start after one loop (dU = 0), loop integral dQ/T = 0 (reversible)"
    , near (sV (cEnd c)) (sV (cStart c)) 1e-9 && near (sT (cEnd c)) (sT (cStart c)) 1e-9 && abs (cDS c) < 1e-6)
  , ("Q_in/Th = |Q_out|/Tc (entropy in and out of the reservoirs balances)", near (cQin c / 600) (-(cQout c) / 300) 1e-4)
  , ("Otto: eta = 1 - r^(1-gamma) = " ++ fmt 2 (100 * (1 - cR o ** (-0.4))) ++ "% < Carnot", near (cEta o) (1 - cR o ** (-0.4)) 1e-6 && cEta o < 0.5)
  , ("Stirling: below Carnot without a regenerator, equal to Carnot with an ideal one", cEta s < 0.5 && near regen 0.5 1e-5)
  , ("Otto between 600 K and 300 K reservoirs: loop sum dQ/T_reservoir = " ++ fmt 3 clausius ++ " < 0 (Clausius inequality)", clausius < 0) ]
  where
    c = makeCycle 1 600 300 1.4 2000
    o = makeCycle 2 600 300 1.4 2000
    s = makeCycle 3 600 300 1.4 2000
    regen = cW s / (cQin s - cCv s * 300)
    clausius = cQin o / 600 + cQout o / 300

-- ---------- やってみよう ----------
-- 1. ↑↓ ← → で Th と Tc を変え、効率がどう変わるか見よう。効率 100% にするには？
-- 2. TS 図ではカルノーサイクルは長方形。面積は何を表す？（PV 図の面積と同じ値になる）
-- 3. 2 のオットーと比べよう。同じ温度の範囲なのに効率が低いのはなぜ？（熱を受け取る温度が Th より低い）
-- 4. G で γ を変えると、カルノーの効率は変わる？ オットーは？
-- 5. vRatio を大きくすると、1 周の仕事 W と効率はそれぞれどう変わる？

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Int)

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
