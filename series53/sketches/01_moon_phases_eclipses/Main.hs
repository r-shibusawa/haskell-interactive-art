-- ============================================
-- 53-01 月の満ち欠けと日食・月食 ─ 新月のたびに日食が起きないのはなぜ？
-- 太陽と月の黄経の差（離角）E から、光って見える割合 k = (1 − cos E) / 2
-- 月の通り道（白道）は黄道から 5.1° 傾いている。交点の近くで新月 → 日食、満月 → 月食
-- 平均の速さで計算した近似（実際の食の時刻とは数時間ずれることがある）
-- 操作：スペース = 再生/停止 / ←→ = 速さ / N = 次の新月 / F = 次の満月 / E = 次の食 / R = 今日にもどる
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Foreign.C.Types (CTime(..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

inc :: Double
inc = 5.145                            -- ★ 白道の傾き [度]（0 にすると毎月 食が起こる）

solarLim, lunarLim :: Double
solarLim = 1.5                         -- ★ 食が起こる月の黄緯 β の目安 [度]（日食）
lunarLim = 1.0                         -- ★ 同じく月食

speeds :: [Double]
speeds = [0.05, 0.25, 1, 4, 15]        -- 1 秒あたりに進む日数

-- 新月・満月 1 つ分：時刻 JD、新月か、月の黄緯 β、食（"" / "solar" / "lunar"）
data Ev = Ev { evJd :: !Double, evNew :: !Bool, evBeta :: !Double, evEcl :: String }

data World = World
  { wJd      :: !Double                -- いまの時刻（JD、世界時）
  , wPlaying :: !Bool
  , wSp      :: !Int                   -- 速さの番号
  , wEvents  :: [Ev]                   -- これから 1 年の新月・満月
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-01 Moon phases and eclipses" world0)
  { skBack  = rgb255 10 14 32
  , skStart = \_ -> do
      printSelfTest selfTest
      j <- jdNowUT
      pure (makeEvents world0 { wJd = j, wTest = selfTest })
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2451545 True 2 [] []

-- ---- 月と太陽の平均の動き
-- 平均の黄経（2000-01-01 12:00 から d 日）と、白道の昇交点（18.6 年で 1 周、逆向きに回る）
lonSun, lonMoon, nodeLon, elong, moonBeta :: Double -> Double
lonSun jd = wrap360 (280.460 + 0.9856474 * (jd - 2451545))
lonMoon jd = wrap360 (218.316 + 13.176396 * (jd - 2451545))
nodeLon jd = wrap360 (125.044 - 0.0529538 * (jd - 2451545))
elong jd = wrap360 (lonMoon jd - lonSun jd)
moonBeta jd = asin (sin (inc * degR) * sin ((lonMoon jd - nodeLon jd) * degR)) / degR

litFrac :: Double -> Double
litFrac e = (1 - cos (e * degR)) / 2

-- 離角が増える速さ [度/日] → 朔望月 = 360 / rate
rate :: Double
rate = 13.176396 - 0.9856474

-- jd より後で、離角が target になる時刻（0 = 新月、180 = 満月）
nextSyzygy :: Double -> Double -> Double
nextSyzygy jd target = jd + dl / rate + (if dl < 1e-9 then 360 / rate else 0)
  where dl = wrap360 (target - elong jd)

eclipseAt :: Double -> Bool -> String
eclipseAt jd isNew
  | isNew = if b < solarLim then "solar" else ""
  | otherwise = if b < lunarLim then "lunar" else ""
  where b = abs (moonBeta jd)

-- これから 1 年の新月・満月の一覧（26 個）
makeEvents :: World -> World
makeEvents w = w { wEvents = go (wJd w - 1) (26 :: Int) }
  where
    go _ 0 = []
    go j k = let a = nextSyzygy j 0
                 b = nextSyzygy j 180
                 t = min a b
                 isNew = a < b
             in Ev t isNew (moonBeta t) (eclipseAt t isNew) : go (t + 0.5) (k - 1)

step :: World -> World
step w
  | wPlaying w = let w' = w { wJd = wJd w + getAt 1 speeds (wSp w) / 60 }   -- 1 フレームに 1/60 秒ぶん
                 in case drop 1 (wEvents w') of
                      (e1 : _) | wJd w' > evJd e1 -> makeEvents w'
                      _ -> w'
  | otherwise = w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- 真上から（太陽の光は左から）
  [ Color (rgba255 255 220 130 60) (Line [(20, cy + fromIntegral i * r * 0.2), (70, cy + fromIntegral i * r * 0.2)]) | i <- [-5 .. 5 :: Int] ]
  ++ [ say "Sunlight ->" 20 (cy - r - 12) 14 (rgb255 255 215 110) ]
  -- 白道：黄道より上（北）の半分は実線、下は点線
  ++ [ if up then Color (rgba255 200 210 255 200) (thickLine 2 (op a0) (op a1)) else Color (rgba255 200 210 255 70) (Line [op a0, op a1])
     | i <- [0 .. 71 :: Int]
     , let a0 = fromIntegral i / 72 * tau
     , let a1 = fromIntegral (i + 1) / 72 * tau
     , let up = sin (a0 - nd) > 0
     , up || even i ]
  ++ [ Color (rgb255 255 150 120) (thickLine 1.5 (cx - r * 1.15 * cos nd, cy + r * 1.15 * sin nd) (cx + r * 1.15 * cos nd, cy - r * 1.15 * sin nd))
     , say "Asc. node" (cx - r * 1.2 * cos nd) (cy + r * 1.2 * sin nd) 14 (rgb255 255 150 120)
     , Color (rgb255 60 120 200) (circleD cx cy 36), Color (rgb255 20 30 60) (halfDisc cx cy 18 (-pi / 2))   -- 地球（右半分が夜）
     , Color (rgb255 40 42 55) (circleD mx my 22), Color (rgb255 240 235 210) (halfDisc mx my 11 (pi / 2))     -- 月（左＝太陽側が明るい）
     , say "Top view (sizes and distances not to scale)" 20 (cy + r + 64) 14 (gray 0.86)
     , say "Bright part of the moon's orbit = north of the ecliptic" 20 (cy + r + 84) 14 (gray 0.86) ]
  -- 地球から見た月の形
  ++ [ drawPhase fx fy fr e
     , sayC ("Moon seen from Earth   lit fraction k = " ++ fmt 1 (k * 100) ++ "%") fx (fy + fr + 30) 15 (rgb255 240 236 215)
     , sayC (phaseName e) fx (fy + fr + 52) 15 (rgb255 240 236 215) ]
  -- 横から：月の黄緯 β（黄道からの高さ）を時間で
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx gy gw gh)
     , Color (rgba255 255 210 120 40) (rectTL gx (yv solarLim) gw (yv (-solarLim) - yv solarLim))
     , Color (rgba255 255 255 255 60) (Line [(gx, yv 0), (gx + gw, yv 0)])
     , Color (rgb255 200 210 255) (polyline 1.5 [ (xj j, yv (moonBeta j)) | i <- [0 .. 300 :: Int], let j = wJd w - 15 + fromIntegral i / 300 * span ]) ]
  ++ concat [ [ Color (gray 0.78) (circleD (xj (evJd ev)) (yv (evBeta ev)) (d + 2))
              , Color (if evNew ev then rgb255 30 30 40 else rgb255 245 240 215) (circleD (xj (evJd ev)) (yv (evBeta ev)) d) ]
              ++ [ sayC (evEcl ev) (xj (evJd ev)) (yv (evBeta ev) - 12) 14 (rgb255 255 140 110) | evEcl ev /= "" ]
            | ev <- wEvents w, xj (evJd ev) <= gx + gw, let d = if evEcl ev /= "" then 14 else 8 ]
  ++ [ Color (rgba255 255 255 255 200) (Line [(xj (wJd w), gy), (xj (wJd w), gy + gh)])
     , say "Moon latitude beta (height above the ecliptic), next year   dark = new moon, light = full moon" gx (gy - 28) 14 (rgb255 200 205 225)
     , say ("Only new moons inside the band (|beta| < " ++ showJ solarLim ++ ") give a solar eclipse") gx (gy - 8) 14 (rgb255 200 205 225)
     , sayR "+6" (gx - 6) (yv 6 + 12) 14 (rgb255 200 205 225)
     , sayR "-6" (gx - 6) (yv (-6)) 14 (rgb255 200 205 225)
     , sayR "0" (gx - 6) (yv 0 + 6) 14 (rgb255 200 205 225) ]
  -- 次の食の一覧
  ++ [ say ("Coming eclipses (approx.):" ++ if null ecl then " none" else "") gx (gy + gh + 28) 14 (rgb255 255 175 140) ]
  ++ [ say (dateStr (evJd ev) ++ "  " ++ evEcl ev ++ " eclipse  (beta = " ++ fmt 2 (evBeta ev) ++ ")") (gx + 20) (gy + gh + 48 + 20 * fromIntegral i) 14 (rgb255 255 175 140)
     | (i, ev) <- zip [0 :: Int ..] ecl ]
  ++ [ say "Moon phases and eclipses | Space play/stop  Left/Right speed  N new moon  F full moon  E eclipse  R today" 20 34 17 (rgb255 240 236 215)
     , say (dateStr (wJd w) ++ " (UT)   speed " ++ showJ (getAt 1 speeds (wSp w)) ++ " d/s   elongation E = " ++ fmt 1 e ++ "   beta = " ++ fmt 2 b
            ++ "   age ~ " ++ fmt 1 (e / rate) ++ " d   month = 360 / " ++ fmt 4 rate ++ " = " ++ fmt 3 (360 / rate) ++ " d") 20 62 15 (rgb255 250 225 140)
     , drawSelfTest env (wTest w) ]
  where
    wd = envW env
    ht = envH env
    e = elong (wJd w)
    k = litFrac e
    b = moonBeta (wJd w)
    cx = wd * 0.22
    cy = ht * 0.4
    r = min (wd * 0.17) (ht * 0.27)
    nd = f2 ((nodeLon (wJd w) - lonSun (wJd w)) * degR)   -- 太陽の方向から見た交点の向き
    op a = (cx - r * cos a, cy + r * sin a)
    mx = cx - r * cos (f2 (e * degR))                    -- 北から見て反時計回り
    my = cy + r * sin (f2 (e * degR))
    fx = wd * 0.55
    fy = ht * 0.33
    fr = min 110 (ht * 0.14)
    gx = wd * 0.4
    gw = wd - gx - 30
    gy = ht * 0.62
    gh = ht * 0.22
    span = 365
    yv v = gy + gh / 2 - f2 (v / 6) * (gh / 2)
    xj j = gx + f2 ((j - wJd w + 15) / span) * gw
    ecl = take 3 (filter (\ev -> evEcl ev /= "") (wEvents w))

-- 半円（p5.js の arc(x, y, 2r, 2r, a, a + π)：画面の角度 a から時計回りに半周）
halfDisc :: Float -> Float -> Float -> Float -> Picture
halfDisc x y r a = Polygon [ (x + r * cos t, y + r * sin t) | i <- [0 .. 32 :: Int], let t = a + pi * fromIntegral i / 32 ]

-- 月の形：明るい半円 ＋ 明暗の境目（だ円）。北半球から見て、満ちていくときは右が明るい
drawPhase :: Float -> Float -> Float -> Double -> Picture
drawPhase x y r e = Pictures
  [ Color dark (circleD x y (2 * r))
  , Color lit (halfDisc x y r (if waxing then -pi / 2 else pi / 2))
  , Color (if c > 0 then dark else lit) (Polygon [ (x + r * abs c * cos t, y + r * sin t) | i <- [0 .. 48 :: Int], let t = tau * fromIntegral i / 48 ]) ]
  where
    c = f2 (cos (e * degR))
    waxing = e < 180
    lit = rgb255 245 238 210
    dark = rgb255 45 47 60

phaseName :: Double -> String
phaseName e = getAt "" names (floor (wrap360 (e + 22.5) / 45) `mod` 8)
  where names = ["New moon", "Waxing crescent", "First quarter", "Waxing gibbous", "Full moon", "Waning gibbous", "Last quarter", "Waning crescent"]

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (makeEvents w { wPlaying = not (wPlaying w) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (makeEvents w { wSp = min (length speeds - 1) (wSp w + 1) })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (makeEvents w { wSp = max 0 (wSp w - 1) })
  EventKey (SpecialKey _) Down _ _ -> pure (makeEvents w)
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> pure (makeEvents w { wJd = nextSyzygy (wJd w + 0.01) 0, wPlaying = False })
    | c `elem` "fF" -> pure (makeEvents w { wJd = nextSyzygy (wJd w + 0.01) 180, wPlaying = False })
    | c `elem` "eE" -> pure $ makeEvents $ case filter (\v -> evEcl v /= "" && evJd v > wJd w + 0.01) (wEvents w) of
                          (v : _) -> w { wJd = evJd v, wPlaying = False }
                          [] -> w
    | c `elem` "rR" -> do j <- jdNowUT
                          pure (makeEvents w { wJd = j })
    | otherwise -> pure (makeEvents w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("lit fraction: new moon 0, first quarter 1/2, full moon 1", near (litFrac 0) 0 1e-12 && near (litFrac 90) 0.5 1e-12 && near (litFrac 180) 1 1e-12)
  , ("synodic month = 360 / (moon - sun speed) ~ 29.53 days", near (360 / rate) 29.5306 0.001)
  , ("eclipse seasons repeat every 180 / (sun + node speed) ~ 173.3 days", near (180 / (0.9856474 + 0.0529538)) 173.31 0.05)
  , ("max |beta| of the moon = orbit tilt 5.145 (0 at the nodes)", near (foldl' max 0 bs) inc 0.02 && near (foldl' min 1e9 bs) 0 0.2)
  , ("new moon of 2024-04-08 is near a node -> solar eclipse (a real total eclipse)", dateStr a == "2024-04-08" && eclipseAt a True == "solar")
  , ("new moon of 2024-06-06 is far from a node (|beta| > 4) -> no eclipse", dateStr c == "2024-06-06" && abs (moonBeta c) > 4 && eclipseAt c True == "")
  , ("full moon of 2025-03-14 is a lunar eclipse (a real total eclipse)", dateStr f == "2025-03-14" && eclipseAt f False == "lunar")
  , ("solar eclipses counted over 100 years: about 2.4 per year (close to the real mean)", fromIntegral ns / 100 > (2.1 :: Double) && fromIntegral ns / 100 < (2.6 :: Double)) ]
  where
    jn = jdFromDate 2000 1 1 0
    bs = [ abs (moonBeta (jn + fromIntegral i * 0.7)) | i <- [0 .. 399 :: Int] ]
    a = nextSyzygy (jdFromDate 2024 4 5 0) 0
    c = nextSyzygy (jdFromDate 2024 6 3 0) 0
    f = nextSyzygy (jdFromDate 2025 3 11 0) 180
    ns = countSolar (jdFromDate 2000 1 1 0) (1237 :: Int) (0 :: Int)
    countSolar _ 0 n = n
    countSolar j i n = let t = nextSyzygy j 0 in countSolar (t + 1) (i - 1) (if eclipseAt t True /= "" then n + 1 else n)

-- ---------- やってみよう ----------
-- 1. N を何回か押して新月ごとの β を見よう。帯（|β| < 1.5°）に入るのは何回に 1 回？
-- 2. E で次の食へ。そのとき「交点の線」と太陽の方向はどうなっている？
-- 3. inc を 0 にすると？ 毎月、日食と月食が起こってしまう
-- 4. 食は約 173 日ごとの「食の季節」にまとまって起こる。一覧の日付の間隔を計算しよう
-- 5. 月の形を描く drawPhase を使って、1 か月の月の形を 1 枚に並べたポスターを作ろう

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

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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
        | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 日付 ⇔ ユリウス日 JD（グレゴリオ暦、時刻は世界時 UT。2000-01-01 12:00 UT = 2451545.0）
jdFromDate :: Int -> Int -> Double -> Double -> Double
jdFromDate y0 m0 d h = fromIntegral (floorD (365.25 * fromIntegral (y + 4716)) + floorD (30.6001 * fromIntegral (m + 1)) + b) + d - 1524.5 + h / 24
  where
    (y, m) = if m0 <= 2 then (y0 - 1, m0 + 12) else (y0, m0)
    a = floorD (fromIntegral y / 100)
    b = 2 - a + floorD (fromIntegral a / 4)

-- JD → (年, 月, 日, 時)
dateFromJd :: Double -> (Int, Int, Int, Double)
dateFromJd jd = (yy, mm, dd, f * 24)
  where
    z = floorD (jd + 0.5)
    f = jd + 0.5 - fromIntegral z
    al = floorD ((fromIntegral z - 1867216.25) / 36524.25)
    a = z + 1 + al - floorD (fromIntegral al / 4)
    b = a + 1524
    c = floorD ((fromIntegral b - 122.1) / 365.25)
    d4 = floorD (365.25 * fromIntegral c)
    e = floorD (fromIntegral (b - d4) / 30.6001)
    dd = b - d4 - floorD (30.6001 * fromIntegral e)
    mm = if e < 14 then e - 1 else e - 13
    yy = if mm > 2 then c - 4716 else c - 4715

dateStr :: Double -> String
dateStr jd = let (y, m, d, _) = dateFromJd (fin 2451545 jd) in show y ++ "-" ++ pad2 m ++ "-" ++ pad2 d
  where pad2 n = (if n < 10 then "0" else "") ++ show n

floorD :: Double -> Int
floorD x = floor x

-- いまの JD（1970-01-01 0:00 UT = 2440587.5）。C の time() で秒を読む
foreign import ccall unsafe "time" c_time :: Ptr CTime -> IO CTime

jdNowUT :: IO Double
jdNowUT = do
  CTime t <- c_time nullPtr
  pure (2440587.5 + fromIntegral t / 86400)

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
