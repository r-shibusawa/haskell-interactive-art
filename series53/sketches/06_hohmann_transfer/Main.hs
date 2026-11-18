-- ============================================
-- 53-06 ホーマン軌道で火星へ ─ いつ打ち上げれば、火星に会える？
-- 地球の軌道（半径 r₁）に接して出発し、火星の軌道（r₂）に接して着く楕円 = ホーマン移行軌道（a = (r₁ + r₂)/2）
-- 速さは vis-viva の式 v² = μ(2/r − 1/a)：出発の Δv₁ = v(近日点) − 地球の速さ、到着の Δv₂ = 火星の速さ − v(遠日点)
-- 所要時間 = 楕円の半周 = π√(a³/μ)。その間に火星も進むので、出発のとき火星は地球より約 44° 先にいないといけない
-- 円軌道・同じ平面の近似。操作：スペース = 打ち上げ / ←→ = 時間の速さ / W = 次の窓まで早送り / R = 最初から
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mu, au, day :: Double
mu = 1.32712440018e20                  -- 太陽の GM [m³/s²]
au = 1.495978707e11                    -- 1 AU [m]
day = 86400                            -- 1 日 [s]

r1, r2 :: Double
r1 = 1.0                               -- ★ 出発の軌道半径 [AU]（地球）
r2 = 1.5237                            -- ★ 到着の軌道半径 [AU]（火星）

hitDeg :: Double
hitDeg = 3                             -- ★ 「会えた」とする角度の差 [度]

speeds :: [Double]
speeds = [2, 5, 10, 20, 40]            -- 1 秒あたりの日数

-- 飛んでいる探査機：打ち上げた日と、近日点の向き θL
data Craft = Craft { cT0, cThL :: !Double }

data World = World
  { wT       :: !Double                -- 経過日数
  , wSp      :: !Int
  , wThE0    :: !Double
  , wThM0    :: !Double
  , wCraft   :: !(Maybe Craft)
  , wTries   :: !Int
  , wHits    :: !Int
  , wMsg     :: !String
  , wSkip    :: !Bool
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-06 Hohmann transfer to Mars" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      resetGame world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 0 0 1 Nothing 0 0 "" False []

resetGame :: World -> IO World
resetGame w = do
  e <- randomRIO (0, 2 * pi)
  m <- randomRIO (0, 2 * pi)
  pure w { wT = 0, wThE0 = e, wThM0 = m, wCraft = Nothing, wMsg = "", wSkip = False }

-- ---- ホーマン軌道の計算
-- 円軌道の角速度 [rad/日]、速さ [m/s]
nOf :: Double -> Double
nOf r = sqrt (mu / (r * au) ** 3) * day

vCirc :: Double -> Double
vCirc r = sqrt (mu / (r * au))

visViva :: Double -> Double -> Double
visViva r a = sqrt (mu * (2 / (r * au) - 1 / (a * au)))

-- ホーマン軌道の数字
data Ho = Ho { hoA, hoE, hoTH, hoDv1, hoDv2, hoPhase :: !Double }

hohmann :: Double -> Double -> Ho
hohmann ra rb = Ho a ((rb - ra) / (rb + ra)) tH (visViva ra a - vCirc ra) (vCirc rb - visViva rb a) (180 - nOf rb * tH / degR)
  where a = (ra + rb) / 2
        tH = pi * sqrt ((a * au) ** 3 / mu) / day

ho :: Ho
ho = hohmann r1 r2

thE, thM :: World -> Double -> Double
thE w t = wThE0 w + nOf r1 * t
thM w t = wThM0 w + nOf r2 * t

-- 火星が地球より何度先か
phaseNow :: World -> Double -> Double
phaseNow w t = wrap360 ((thM w t - thE w t) / degR)

daysToWindow :: World -> Double -> Double
daysToWindow w t = wrap360 (phaseNow w t - hoPhase ho) / ((nOf r1 - nOf r2) / degR)

-- 移行軌道の上の位置：近日点の向き θL、出発から dt 日（ケプラー方程式をニュートン法で）
craftPos :: Double -> Double -> (Double, Double)
craftPos thL dt = (x * cos thL - y * sin thL, x * sin thL + y * cos thL)
  where
    nT = sqrt (mu / (hoA ho * au) ** 3) * day
    m = nT * dt
    e = hoE ho
    go ea i | i >= (30 :: Int) = ea
            | otherwise = let d = (ea - e * sin ea - m) / (1 - e * cos ea)
                              ea' = ea - d
                          in if abs d < 1e-13 then ea' else go ea' (i + 1)
    ea = go m 0
    x = hoA ho * (cos ea - e)
    y = hoA ho * sqrt (1 - e * e) * sin ea

launch :: World -> World
launch w = case wCraft w of
  Just _ -> w
  Nothing -> w { wCraft = Just (Craft (wT w) (thE w (wT w))), wTries = wTries w + 1, wMsg = "" }

-- 1 フレームぶん進める
step :: World -> World
step w0 = arrive w1
  where
    w1 | wSkip w0 = let d = daysToWindow w0 (wT w0) in if d < 6 then w0 { wT = wT w0 + d, wSkip = False } else w0 { wT = wT w0 + 6 }
       | otherwise = w0 { wT = wT w0 + getAt 2 speeds (wSp w0) / 60 }
    -- 到着：火星との角度の差
    arrive w = case wCraft w of
      Just c | wT w - cT0 c >= hoTH ho ->
        let miss = wrap180 ((thM w (cT0 c + hoTH ho) - cThL c - pi) / degR)
        in if abs miss < hitDeg
             then w { wHits = wHits w + 1, wMsg = "Arrived! Off from Mars by " ++ fmt 1 miss ++ " deg (success)", wCraft = Nothing }
             else w { wMsg = "Mars was " ++ (if miss > 0 then fmt 0 miss ++ " deg ahead" else fmt 0 (-miss) ++ " deg behind") ++ " (missed)", wCraft = Nothing }
      _ -> w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgba255 60 110 200 120) (at (cx, cy) (Circle (f2 r1 * sc)))
  , Color (rgba255 220 110 80 120) (at (cx, cy) (Circle (f2 r2 * sc)))
  , Color (rgb255 255 210 90) (circleD cx cy 20)
    -- 移行軌道（いま打ち上げたら通る道）
  , case wCraft w of
      Just _ -> Color (rgba255 250 225 140 220) (polyline 2 pts)
      Nothing -> Color (rgba255 250 225 140 80) (dashed 5 6 pts)
    -- 到着の点と、そのとき火星がいる場所
  , Color (rgb255 250 225 140) (at (ax, ay) (Circle 11))
  , Color (rgba255 220 110 80 160) (at (mx, my) (Circle 8))
  , say "Mars on arrival day" (mx + 10) (my + 4) 14 (rgb255 220 150 130)
    -- 惑星と探査機
  , Color (rgb255 90 150 240) (circleD (cx + f2 r1 * sc * cos (f2 (thE w t))) (cy - f2 r1 * sc * sin (f2 (thE w t))) 16)
  , Color (rgb255 230 110 80) (circleD (cx + f2 r2 * sc * cos (f2 (thM w t))) (cy - f2 r2 * sc * sin (f2 (thM w t))) 14) ]
  ++ [ Color white (uncurry circleD (toScr (craftPos (cThL c) (t - cT0 c))) 9) | Just c <- [wCraft w] ]
  -- ---- 数字
  ++ concat [ [ say a (x0 + 0) (y0 + fromIntegral i * lh) 14 (rgb255 200 205 225), say b (x0 + 250) (y0 + fromIntegral i * lh) 14 (rgb255 250 225 140) ]
            | (i, (a, b)) <- zip [0 :: Int ..] rows ]
  ++ [ say ("Phase angle now " ++ fmt 1 ph ++ " deg" ++ (if ok then "   <- now!" else "")) x0 (yr + 24) 17 (if ok then rgb255 130 230 160 else rgb255 240 236 215)
     , say ("Next launch window in " ++ fmt 0 (daysToWindow w t) ++ " days") x0 (yr + 50) 15 (rgb255 220 225 240)
     , say ("Success " ++ show (wHits w) ++ " / " ++ show (wTries w)
            ++ maybe "" (\c -> "   flying " ++ fmt 0 (t - cT0 c) ++ " / " ++ fmt 0 (hoTH ho) ++ " days") (wCraft w)) x0 (yr + 76) 15 (rgb255 220 225 240)
     , say (wMsg w) x0 (yr + 102) 15 (rgb255 255 190 150)
     , say "Hohmann transfer to Mars | Space launch  Left/Right time speed  W skip to next window  R restart" 20 34 18 (rgb255 240 236 215)
     , say ("Day " ++ fmt 0 t ++ " (" ++ showJ (getAt 2 speeds (wSp w)) ++ " days/s)   dashed = path if launched now   circle = arrival point") 20 60 16 (rgb255 250 225 140)
     , drawSelfTest env (wTest w) ]
  where
    t = wT w
    cx = envW env * 0.33
    cy = envH env * 0.55
    sc = min (envW env * 0.28) (envH env * 0.4) / f2 r2
    toScr (x, y) = (cx + f2 x * sc, cy - f2 y * sc)
    thL = maybe (thE w t) cThL (wCraft w)
    pts = [ toScr (craftPos thL (fromIntegral i / 60 * hoTH ho)) | i <- [0 .. 60 :: Int] ]
    ax = cx + f2 r2 * sc * cos (f2 (thL + pi))
    ay = cy - f2 r2 * sc * sin (f2 (thL + pi))
    tA = maybe t cT0 (wCraft w) + hoTH ho
    mx = cx + f2 r2 * sc * cos (f2 (thM w tA))
    my = cy - f2 r2 * sc * sin (f2 (thM w tA))
    x0 = envW env * 0.66
    y0 = 110
    lh = 25
    ph = phaseNow w t
    rows =
      [ ("Transfer orbit a = (r1+r2)/2", fmt 4 (hoA ho) ++ " AU")
      , ("Earth's speed sqrt(mu/r1)", fmt 2 (vCirc r1 / 1000) ++ " km/s")
      , ("Departure v (vis-viva)", fmt 2 (visViva r1 (hoA ho) / 1000) ++ " km/s")
      , ("dv1 (speed up from Earth)", fmt 3 (hoDv1 ho / 1000) ++ " km/s")
      , ("Arrival v (aphelion)", fmt 2 (visViva r2 (hoA ho) / 1000) ++ " km/s")
      , ("dv2 (match Mars's orbit)", fmt 3 (hoDv2 ho / 1000) ++ " km/s")
      , ("Time pi sqrt(a^3/mu)", fmt 1 (hoTH ho) ++ " d = " ++ fmt 3 (hoTH ho / 365.25) ++ " yr")
      , ("Phase angle (Mars ahead)", fmt 1 (hoPhase ho) ++ " deg")
      , ("Synodic period (windows)", fmt 1 (2 * pi / (nOf r1 - nOf r2)) ++ " days") ]
    yr = y0 + fromIntegral (length rows) * lh
    ok = abs (wrap180 (ph - hoPhase ho)) < hitDeg

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go draw' left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go draw' left rest
      | l <= left = [ Line [a, b] | draw' ] ++ go draw' (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | draw' ] ++ go (not draw') (if draw' then off else on) (m : rest)
      where l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (launch w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wSp = min (length speeds - 1) (wSp w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wSp = max 0 (wSp w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "wW" -> pure (case wCraft w of { Nothing -> w { wSkip = True }; Just _ -> w })
    | c `elem` "rR" -> resetGame w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Earth's orbital speed sqrt(mu / 1 AU) ~ 29.78 km/s", near (vCirc 1 / 1000) 29.78 0.01)
  , ("Earth -> Mars Hohmann transfer takes ~ 259 days (0.709 yr)", near (hoTH ho) 258.9 1 && near (hoTH ho / 365.25) 0.709 0.003)
  , ("dv1 ~ 2.94 km/s, dv2 ~ 2.65 km/s", near (hoDv1 ho / 1000) 2.94 0.02 && near (hoDv2 ho / 1000) 2.65 0.02)
  , ("at departure Mars is about 44 deg ahead of Earth", near (hoPhase ho) 44.3 0.5)
  , ("launch windows every ~ 780 days (2 years 2 months)", near (2 * pi / (nOf r1 - nOf r2)) 779.9 2)
  , ("Kepler's equation: start at r1, half an orbit later exactly r2 on the opposite side"
    , near (hyp p0) r1 1e-9 && near (hyp p1) r2 1e-9 && near (abs (wrap180 (uncurry (flip atan2) p1 / degR - 0.7 / degR))) 180 1e-6)
  , ("energy v^2/2 - mu/r is the same at perihelion and aphelion (= -mu/2a)"
    , near (en r1) (en r2) 1 && near (en r1) (-mu / (2 * hoA ho * au)) 1)
  , ("launching on the window day meets Mars exactly (< 0.01 deg)", abs miss < 0.01) ]
  where
    p0 = craftPos 0.7 0
    p1 = craftPos 0.7 (hoTH ho)
    hyp (x, y) = sqrt (x * x + y * y)
    en r = visViva r (hoA ho) ** 2 / 2 - mu / (r * au)
    tw = world0 { wThE0 = 0, wThM0 = 1, wT = 0 }
    wd = daysToWindow tw 0
    miss = wrap180 ((thM tw (wd + hoTH ho) - thE tw wd - pi) / degR)

-- ---------- やってみよう ----------
-- 1. 位相角を見ながら、ちょうどいいときにスペースで打ち上げよう。何回で成功する？
-- 2. W で窓まで早送りしてから打ち上げると？ 次の窓までは何日？
-- 3. r2 を 5.2（木星）にすると、所要時間と Δv はどうなる？ 必要な位相角は？
-- 4. r2 を 0.723（金星）にすると、Δv はマイナス（減速）になる。内側へ行くにはブレーキ
-- 5. hitDeg を 1 にしてむずかしくしよう。実際の探査機は途中で軌道を細かく直している


-- ==================================================
-- ここから下は「天文の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 度 → ラジアン
degR :: Double
degR = pi / 180

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 0〜360 にそろえる（JavaScript の ((a % 360) + 360) % 360 と同じ）
wrap360 :: Double -> Double
wrap360 a = jsRem (jsRem a 360 + 360) 360

wrap180 :: Double -> Double
wrap180 a = wrap360 (a + 180) - 180

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 (fin 0 t)) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 (fin 0 t)) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgb255 80 90 120) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (rgb255 160 170 195) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 5) (gY g v + 6) 14 (rgb255 160 170 195) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 38) 14 (rgb255 205 210 225)
     , say yl (gx g) (gy g - 8) 14 (rgb255 205 210 225) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

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

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = show v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)
