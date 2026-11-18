-- ============================================
-- 53-04 惑星の位置 ─ 軌道要素とケプラー方程式で「その日」の惑星の場所を計算する
-- 平均近点角 M = L − ϖ（日付とともに一定の速さで増える）→ ケプラー方程式 E − e sin E = M をニュートン法で解く
-- → 軌道面での位置 (a(cos E − e), a√(1−e²) sin E) → 3 回回転して黄道座標。真上（黄道の北）から見た図
-- 公転周期 T と軌道長半径 a から、ケプラーの第 3 法則 T² / a³ = 1（年、AU）を実データで確かめる
-- 操作：←→ = 日付を進める・もどす / 1 = 1 日 2 = 1 週 3 = 1 か月 単位 / スペース = 自動で進める / Z = 内惑星 ⇔ 全惑星 / T = 今日
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Foreign.C.Types (CTime(..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

colOf :: Planet -> Color
colOf p = case p of
  Mercury -> rgb255 190 180 170
  Venus   -> rgb255 240 210 140
  Earth   -> rgb255 90 150 240
  Mars    -> rgb255 230 110 80
  Jupiter -> rgb255 220 180 130
  Saturn  -> rgb255 230 210 150
  Uranus  -> rgb255 150 220 230
  Neptune -> rgb255 110 140 250

units :: [Double]
units = [1, 7, 30.4375]                -- ★ 1 日・1 週・1 か月

unitNames :: [String]
unitNames = ["1 day", "1 week", "1 month"]

data World = World
  { wJd      :: !Double                -- いまの日付（JD）
  , wUnit    :: !Int
  , wPlaying :: !Bool
  , wOuter   :: !Bool
  , wOrbits  :: [(Planet, [Point3])]   -- 1 周ぶんの軌道（要素の変化はゆっくりなので 1 回だけ計算）
  , wTest    :: [(String, Bool)] }

type Point3 = (Double, Double, Double)

main :: IO ()
main = runSketch (sketch "53-04 Planet positions" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      now <- jdNowUT
      let jd = today now
          orbs = [ (n, [ planetPos n (jd + fromIntegral k / 180 * t) | k <- [0 .. 180 :: Int] ]) | n <- allPlanets, let t = periodYr n * 365.25 ]
      printSelfTest selfTest
      pure world0 { wJd = jd, wOrbits = orbs, wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2451544.5 0 False False [] []

-- 今日の 0:00 UT
today :: Double -> Double
today now = ffloor (now - 0.5) + 0.5

step :: Env -> World -> World
step env w
  | wPlaying w && envFrame env `mod` 2 == 0 = w { wJd = wJd w + getAt 1 units (wUnit w) }
  | otherwise = w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgb255 40 50 90) (circleLine cx cy (f2 a * sc)) | a <- takeWhile (<= rmax) (iterate (+ stepA) stepA) ]
  ++ [ Color (rgba255 255 255 255 50) (Line [(cx, cy), (cx + rr, cy)])
     , sayR "Vernal equinox (longitude 0)" (cx + rr) (cy - 6) 14 (rgb255 160 170 200)
     , Color (rgb255 255 210 90) (circleD cx cy (if wOuter w then 10 else 22)) ]
  ++ concat [ [ Color (withA (110 / 255) (colOf n)) (polyline 1 [ toScr p | p <- orb ])
              , Color (colOf n) (uncurry circleD (toScr pn) (if wOuter w then 10 else 14))
              , say (show n) (fst (toScr pn) + 9) (snd (toScr pn) - 7) 14 (rgb255 225 228 240) ]
            | (n, orb) <- wOrbits w, n `elem` list, let pn = planetPos n jd ]
    -- 地球から火星への視線（04 → 05 逆行へのつなぎ）
  ++ [ Color (rgba255 230 110 80 90) (Line [toScr (planetPos Earth jd), toScr (planetPos Mars jd)]) | not (wOuter w) ]
  ++ [ sayR ("Circles every " ++ (if wOuter w then "10" else "0.5") ++ " AU") (cx + rr) (cy + rr + 4) 14 (rgb255 150 160 190) ]
    -- ---- 表
  ++ [ say hd (x0 + colX i) y0 14 (rgb255 240 236 215) | (i, hd) <- zip [0 ..] ["Planet", "a [AU]", "e", "T [yr]", "T^2/a^3", "dist r"] ]
  ++ concat [ [ say v (x0 + colX i) y 14 (if i == 0 then colOf n else if i == 4 then rgb255 250 225 140 else rgb255 215 220 235)
              | (i, v) <- zip [0 ..] cells ]
            | (j, n) <- zip [0 :: Int ..] allPlanets
            , let a = elA (fst (elemOf n))
                  t = periodYr n
                  y = y0 + 28 + fromIntegral j * 26
                  cells = [show n, fmt 3 a, fmt 3 (elE (fst (elemOf n))), fmt 3 t, fmt 4 (t * t / (a * a * a)), fmt 3 (hypot3 (planetPos n jd))] ]
  ++ [ drawLogLog x0 (y0 + 270) (envW env - x0 - 50) (max 100 (envH env - y0 - 360))
     , say "Planet positions | Left/Right date  1 day / 2 week / 3 month step  Space auto  Z inner <-> all  T today" 20 34 18 (rgb255 240 236 215)
     , say (dateStr jd ++ " (JD " ++ fmt 1 jd ++ ")   step " ++ getAt "" unitNames (wUnit w) ++ (if wPlaying w then "   auto" else "")
            ++ "   sun longitude " ++ fmt 1 (sunGeoLon jd) ++ "   Mars longitude seen from Earth " ++ fmt 1 (fst (geoLonLat Mars jd))) 20 60 16 (rgb255 250 225 140)
     , drawSelfTest env (wTest w) ]
  where
    jd = wJd w
    cx = envW env * 0.33
    cy = envH env * 0.55
    rr = min (envW env * 0.33) (envH env * 0.42)
    rmax = if wOuter w then 31 else 1.75 :: Double
    stepA = if wOuter w then 10 else 0.5
    sc = rr / f2 rmax
    list = if wOuter w then allPlanets else take 4 allPlanets
    toScr (x, y, _) = (cx + f2 x * sc, cy - f2 y * sc)
    x0 = envW env - 480
    y0 = 120
    cw = [86, 70, 64, 74, 82, 70]
    colX i = sum (take i cw)

-- 両対数のグラフ：log T = 1.5 log a の直線に 8 個が乗る
drawLogLog :: Float -> Float -> Float -> Float -> Picture
drawLogLog x y w h = Pictures $
  [ Color (rgb255 70 80 120) (frameTL 1 x y w h)
  , Color (rgba255 250 225 140 140) (Line [(gxA 0.3, gyT (0.3 ** 1.5)), (gxA 40, gyT (40 ** 1.5))]) ]
  ++ [ Color (colOf n) (circleD (gxA (elA (fst (elemOf n)))) (gyT (periodYr n)) 9) | n <- allPlanets ]
  ++ [ say "Log-log: across a, up T. Line: T = a^1.5" x (y - 8) 14 (rgb255 200 205 225)
     , say "0.3 AU" (x + 2) (y + h + 18) 14 (rgb255 200 205 225)
     , sayR "40 AU" (x + w - 2) (y + h + 18) 14 (rgb255 200 205 225) ]
  where
    gxA a = x + f2 ((logBase 10 a + 0.6) / 2.2) * w
    gyT t = y + h - f2 ((logBase 10 t + 0.8) / 3.2) * h

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey k Down _ _ -> clampJd <$> key k
  _ -> pure w
  where
    u = getAt 1 units (wUnit w)
    key k = case k of
      SpecialKey KeyRight -> pure w { wJd = wJd w + u }
      SpecialKey KeyLeft -> pure w { wJd = wJd w - u }
      SpecialKey KeySpace -> pure w { wPlaying = not (wPlaying w) }
      Char c
        | c >= '1' && c <= '3' -> pure w { wUnit = fromEnum c - fromEnum '1' }
        | c `elem` "zZ" -> pure w { wOuter = not (wOuter w) }
        | c `elem` "tT" -> do now <- jdNowUT
                              pure w { wJd = today now }
      _ -> pure w
    clampJd w' = w' { wJd = clampv 2378496.5 2470171.5 (wJd w') }   -- 軌道要素が使える 1800〜2050 年に制限

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Kepler's equation: |E - e sin E - M| < 1e-9 (e = 0 to 0.9)", worst < 1e-9)
  , ("2000-01-01 12:00 UT = JD 2451545.0, and back to the date", near (jdFromDate 2000 1 1 12) 2451545 1e-9 && dateStr (jdFromDate 2024 2 29 0) == "2024-02-29")
  , ("Earth-sun distance 0.983 to 1.017 AU, closest in early January", minimum' rs > 0.982 && maximum' rs < 1.018 && imin <= 7)
  , ("T^2 / a^3 = 1 +- 1% for all 8 planets (Kepler's third law)", all (\n -> near (periodYr n ** 2 / elA (fst (elemOf n)) ** 3) 1 0.01) allPlanets)
  , ("2020-10-13: Mars at opposition (180 +- 3 deg from the sun as seen from Earth)", abs el > 177)
  , ("2020-12-21: great conjunction of Jupiter and Saturn (longitudes differ < 1 deg)", abs (wrap180 (fst (geoLonLat Jupiter j2) - fst (geoLonLat Saturn j2))) < 1)
  , ("Mars's distance is always between a(1 - e) and a(1 + e)", r >= ma * (1 - me) - 0.001 && r <= ma * (1 + me) + 0.001) ]
  where
    worst = foldl' max 0 [ abs (ea - e * sin ea - m) | e <- [0, 0.2, 0.5, 0.9], k <- [0 .. 49 :: Int]
                         , let m = -pi + fromIntegral k / 49 * 2 * pi, let ea = solveKepler m e ]
    rs = [ hypot3 (planetPos Earth (jdFromDate 2025 1 1 0 + fromIntegral d)) | d <- [0 .. 364 :: Int] ]
    imin = fst (foldl' (\(bi, bv) (i, v) -> if v < bv then (i, v) else (bi, bv)) (0 :: Int, getAt 0 rs 0) (zip [0 ..] rs))
    j = jdFromDate 2020 10 13 0
    el = wrap180 (fst (geoLonLat Mars j) - sunGeoLon j)
    j2 = jdFromDate 2020 12 21 0
    r = hypot3 (planetPos Mars (jdFromDate 2010 5 5 0))
    ma = elA (fst (elemOf Mars))
    me = elE (fst (elemOf Mars))
    minimum' = foldl' min 1e99
    maximum' = foldl' max (-1e99)

-- 円の線（半径 r）
circleLine :: Float -> Float -> Float -> Picture
circleLine x y r = at (x, y) (Circle r)

-- ---------- やってみよう ----------
-- 1. T で今日にして、3（1 か月単位）とスペースで 1 年進めよう。いちばん速く回るのは？
-- 2. 表の T² / a³ がどれも 1 に近いことを確かめよう。両対数のグラフで 8 個が 1 本の直線に乗る
-- 3. 火星への赤い線を見ながら進め、地球が火星を追いこす日（衝）を探そう（2 年 2 か月ごと）
-- 4. 自分の誕生日に合わせると、その日の惑星の並びがわかる
-- 5. elemOf の e（離心率）を 0.5 にしてみると、軌道の形と速さの変わり方はどうなる？


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

-- ---- 日付 ⇔ ユリウス日 JD（グレゴリオ暦、時刻は世界時 UT。2000-01-01 12:00 UT = 2451545.0）
jdFromDate :: Double -> Double -> Double -> Double -> Double
jdFromDate y0 m0 d h = ffloor (365.25 * (y + 4716)) + ffloor (30.6001 * (m + 1)) + d + b - 1524.5 + h / 24
  where
    (y, m) = if m0 <= 2 then (y0 - 1, m0 + 12) else (y0, m0)
    a = ffloor (y / 100)
    b = 2 - a + ffloor (a / 4)

-- JD → (年, 月, 日, 時)
dateFromJd :: Double -> (Int, Int, Int, Double)
dateFromJd jd = (round yy, round mm, round dd, f * 24)
  where
    z = ffloor (jd + 0.5)
    f = jd + 0.5 - z
    al = ffloor ((z - 1867216.25) / 36524.25)
    a = z + 1 + al - ffloor (al / 4)
    b = a + 1524
    c = ffloor ((b - 122.1) / 365.25)
    dd0 = ffloor (365.25 * c)
    e = ffloor ((b - dd0) / 30.6001)
    dd = b - dd0 - ffloor (30.6001 * e)
    mm = if e < 14 then e - 1 else e - 13
    yy = if mm > 2 then c - 4716 else c - 4715

-- "2024-02-29" の形
dateStr :: Double -> String
dateStr jd = show y ++ "-" ++ pad2 m ++ "-" ++ pad2 d
  where (y, m, d, _) = dateFromJd jd
        pad2 k = (if k < 10 then "0" else "") ++ show k

ffloor :: Double -> Double
ffloor x = fromIntegral (floor x :: Integer)

-- いまの JD（1970-01-01 0:00 UT = 2440587.5）。C の time() で秒を読む
foreign import ccall unsafe "time" c_time :: Ptr CTime -> IO CTime

jdNowUT :: IO Double
jdNowUT = do
  CTime s <- c_time nullPtr
  pure (2440587.5 + fromIntegral s / 86400)

-- ---- 惑星の位置：J2000 の軌道要素（JPL の近似式、1800〜2050 年で誤差は数分角程度）
data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune
  deriving (Eq, Show, Enum, Bounded)

allPlanets :: [Planet]
allPlanets = [minBound .. maxBound]

-- a (AU), e, I (度), L 平均黄経 (度), ϖ 近日点黄経 (度), Ω 昇交点黄経 (度)
data El = El { elA, elE, elI, elL, elW, elO :: !Double }

-- J2000 の値と、それぞれの 1 世紀あたりの変化
elemOf :: Planet -> (El, El)
elemOf p = case p of
  Mercury -> (El 0.38709927 0.20563593 7.00497902 252.25032350 77.45779628 48.33076593, El 0.00000037 0.00001906 (-0.00594749) 149472.67411175 0.16047689 (-0.12534081))
  Venus   -> (El 0.72333566 0.00677672 3.39467605 181.97909950 131.60246718 76.67984255, El 0.00000390 (-0.00004107) (-0.00078890) 58517.81538729 0.00268329 (-0.27769418))
  Earth   -> (El 1.00000261 0.01671123 (-0.00001531) 100.46457166 102.93768193 0.0, El 0.00000562 (-0.00004392) (-0.01294668) 35999.37244981 0.32327364 0.0)
  Mars    -> (El 1.52371034 0.09339410 1.84969142 (-4.55343205) (-23.94362959) 49.55953891, El 0.00001847 0.00007882 (-0.00813131) 19140.30268499 0.44441088 (-0.29257343))
  Jupiter -> (El 5.20288700 0.04838624 1.30439695 34.39644051 14.72847983 100.47390909, El (-0.00011607) (-0.00013253) (-0.00183714) 3034.74612775 0.21252668 0.20469106)
  Saturn  -> (El 9.53667594 0.05386179 2.48599187 49.95424423 92.59887831 113.66242448, El (-0.00125060) (-0.00050991) 0.00193609 1222.49362201 (-0.41897216) (-0.28867794))
  Uranus  -> (El 19.18916464 0.04725744 0.77263783 313.23810451 170.95427630 74.01692503, El (-0.00196176) (-0.00004397) (-0.00242939) 428.48202785 0.40805281 0.04240589)
  Neptune -> (El 30.06992276 0.00859048 1.77004347 (-55.12002969) 44.96476227 131.78422574, El 0.00026291 0.00005105 0.00035372 218.45945325 (-0.32241464) (-0.01262724))

-- ケプラー方程式 E − e sin E = M をニュートン法で解く（M, E はラジアン）
solveKepler :: Double -> Double -> Double
solveKepler m e = go e0 (0 :: Int)
  where
    e0 = if e < 0.8 then m else (if m < 0 then -pi else pi)   -- 最初の値（M は −π〜π）
    go ea i
      | i >= 30 = ea
      | otherwise = let d = (ea - e * sin ea - m) / (1 - e * cos ea)
                        ea' = ea - d
                    in if abs d < 1e-13 then ea' else go ea' (i + 1)

-- 日付 jd の、太陽を中心とした黄道座標 (x, y, z)（AU）
planetPos :: Planet -> Double -> (Double, Double, Double)
planetPos name jd =
  ( (co * cO - so * sO * cI) * xp + (-so * cO - co * sO * cI) * yp
  , (co * sO + so * cO * cI) * xp + (-so * sO + co * cO * cI) * yp
  , so * sI * xp + co * sI * yp )
  where
    (p0, dp) = elemOf name
    t = (jd - 2451545.0) / 36525
    a = elA p0 + elA dp * t
    e = elE p0 + elE dp * t
    i = (elI p0 + elI dp * t) * degR
    l = elL p0 + elL dp * t
    w = elW p0 + elW dp * t
    o = (elO p0 + elO dp * t) * degR
    om = w * degR - o
    m = wrap180 (l - w) * degR
    ea = solveKepler m e
    xp = a * (cos ea - e)
    yp = a * sqrt (1 - e * e) * sin ea
    co = cos om
    so = sin om
    cO = cos o
    sO = sin o
    cI = cos i
    sI = sin i

-- 公転周期 [年] = 360° ÷ 平均黄経の変化（1 年あたり）
periodYr :: Planet -> Double
periodYr name = 36000 / elL (snd (elemOf name))

-- 地球から見た黄経・黄緯 [度]
geoLonLat :: Planet -> Double -> (Double, Double)
geoLonLat name jd = (wrap360 (atan2 y x / degR), atan2 z (sqrt (x * x + y * y)) / degR)
  where
    (px, py, pz) = planetPos name jd
    (qx, qy, qz) = planetPos Earth jd
    x = px - qx
    y = py - qy
    z = pz - qz

sunGeoLon :: Double -> Double
sunGeoLon jd = let (qx, qy, _) = planetPos Earth jd in wrap360 (atan2 (-qy) (-qx) / degR)

hypot3 :: (Double, Double, Double) -> Double
hypot3 (x, y, z) = sqrt (x * x + y * y + z * z)
