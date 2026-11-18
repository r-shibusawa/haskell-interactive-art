-- ============================================
-- 53-00 昼と夜・季節 ─ 地軸の傾き 23.44° が、昼の長さと日射量を変える
-- 日付から太陽の黄経 λ を計算し、赤緯 δ = asin(sin ε · sin λ)（ε = 地軸の傾き）
-- 緯度 φ での昼の長さ = 2·H₀ ÷ 15°（cos H₀ = −tan φ · tan δ）、南中高度 = 90° − |φ − δ|
-- 1 日の平均日射量 Q = (S₀/π)(1/r²)(H₀ sin φ sin δ + cos φ cos δ sin H₀)　← 大気の外での値
-- 操作：↑↓ = 緯度 / ←→ = 日付（Shift で 10 日）/ スペース = 1 年を早送り / T = 傾き 23.44° → 0° → 45° / グラフをドラッグ = 日付
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Environment (lookupEnv)

s0 :: Double
s0 = 1361                              -- ★ 太陽定数 [W/m²]（地球の位置で太陽に垂直な面が受ける光）

tilts :: [Double]
tilts = [23.44, 0, 45]                 -- ★ T キーで切りかえる地軸の傾き [度]

monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

-- 1 年分のグラフ（昼の長さ, 南中高度, 日射量）。緯度・傾きを変えたときだけ計算しなおす
type Curves = U.Vector (Double, Double, Double)

data World = World
  { wTilt    :: !Double
  , wLat     :: !Double                -- ★ 緯度（35° ≒ 東京）
  , wDoy     :: !Int                   -- ★ 日付（1 月 1 日 = 1）
  , wPlaying :: !Bool
  , wCurves  :: !Curves
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-00 Day, night and seasons" world0)
  { skBack  = rgb255 12 16 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = remake (World 23.44 35 172 False U.empty [])

-- ---- 太陽の位置と、昼の長さ・南中高度・日射量
-- 日付（1〜365）→ 太陽の黄経 λ と、地球と太陽の距離 r [AU]（簡単な式、誤差は 0.01° 程度）
sunOf :: Int -> (Double, Double)
sunOf d = (wrap360 lam, 1.00014 - 0.01671 * cos g - 0.00014 * cos (2 * g))
  where
    n = fromIntegral (d - 1 + 9132)    -- 2025 年のその日の正午（2000-01-01 12:00 からの日数）
    l = 280.460 + 0.9856474 * n
    g = (357.528 + 0.9856003 * n) * degR
    lam = l + 1.915 * sin g + 0.020 * sin (2 * g)

declination :: Int -> Double -> Double
declination d eps = asin (sin (eps * degR) * sin (fst (sunOf d) * degR)) / degR

-- 日の出から日の入りまでの時角の半分 H₀ [度]（0 = 極夜、180 = 白夜）
halfDay :: Double -> Double -> Double
halfDay phi dec = acos (clampv (-1) 1 c) / degR      -- |c| > 1 のときは 0° か 180° にする（NaN にしない）
  where c = negate (tan (clampv (-89.99) 89.99 phi * degR)) * tan (dec * degR)

-- 昼の長さ [時間]（太陽の中心で、大気の屈折なし）
dayLength :: Double -> Double -> Double
dayLength phi dec = 2 * halfDay phi dec / 15

noonAlt :: Double -> Double -> Double
noonAlt phi dec = 90 - abs (phi - dec)

insolation :: Double -> Double -> Double -> Double
insolation phi dec r = max 0 ((s0 / pi / (r * r)) * (h * sin p * sin q + cos p * cos q * sin h))
  where h = halfDay phi dec * degR
        p = phi * degR
        q = dec * degR

makeCurves :: Double -> Double -> Curves
makeCurves tl lt = U.generate 365 $ \i ->
  let d = i + 1
      q = declination d tl
  in (dayLength lt q, noonAlt lt q, insolation lt q (snd (sunOf d)))

remake :: World -> World
remake w = w { wCurves = makeCurves (wTilt w) (wLat w) }

step :: World -> World
step w | wPlaying w = w { wDoy = if wDoy w + 1 > 365 then 1 else wDoy w + 1 }   -- 1 フレームに 1 日
       | otherwise = w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ drawEarthSide w 240 (h * 0.36) (min 150 (h * 0.18)) dec
  , drawOrbitTop w 240 (h * 0.8) (min 130 (h * 0.15)) ]
  ++ concat [ graphOf i o | (i, o) <- zip [0 :: Int ..] gs ]
  ++ [ say "Seasons | Up/Down latitude  Left/Right date (Shift: 10 days)  Space play  T tilt  drag graph" 20 34 18 (rgb255 240 236 215)
     , say (dateLabel doy ++ "   latitude phi = " ++ showJ (wLat w) ++ "   tilt eps = " ++ showJ (wTilt w)
            ++ "   sun longitude = " ++ fmt 1 lam ++ "   declination = " ++ fmt 2 dec) 20 62 16 (rgb255 250 225 140)
     , say ("Day = 2 H0/15 = " ++ fmt 2 dl ++ " h (H0 = " ++ fmt 1 h0 ++ ")" ++ (if h0 >= 180 then " midnight sun" else if h0 <= 0 then " polar night" else "")
            ++ "   noon alt = " ++ fmt 1 alt ++ "   Q = " ++ fmt 0 q ++ " W/m2   r = " ++ fmt 4 r ++ " AU") 20 88 15 (rgb255 225 228 240) ]
  ++ [ say "Tilt 0: every day delta = 0 -> always 12 h of daylight, no seasons" gx (h - 34) 15 (rgb255 255 170 120) | wTilt w == 0 ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    h = envH env
    doy = wDoy w
    (lam, r) = sunOf doy
    dec = declination doy (wTilt w)
    dl = dayLength (wLat w) dec
    alt = noonAlt (wLat w) dec
    q = insolation (wLat w) dec r
    h0 = halfDay (wLat w) dec
    gx = 540
    gw = envW env - gx - 40
    gh = (h - 210) / 3 - 34
    sel k (a, b, c) = case k of { 0 -> a; 1 -> b; _ -> c :: Double }
    gs = [ (0 :: Int, 0, 24, "Day length [hours]", [0, 6, 12, 18, 24], rgb255 250 225 140)
         , (1, -30, 90, "Noon altitude of the sun [deg]", [-30, 0, 30, 60, 90], rgb255 230 150 90)
         , (2, 0, 600, "Daily mean insolation [W/m2] (top of the atmosphere)", [0, 200, 400, 600], rgb255 140 190 255) ]
    graphOf i (k, y0, y1, lab, ticks, col) =
      let g = Graph gx (120 + fromIntegral i * (gh + 52)) gw gh 1 365 y0 y1 False False
          cs = wCurves w
          cur = maybe 0 (sel k) (cs U.!? (doy - 1))
      in [ plotAxes g (if i == 2 then "Date" else "") lab [] ticks ]
         ++ [ sayC m (gX g (15 + fromIntegral j * 30.4)) (gy g + gh + 18) 14 (rgb255 120 130 160) | (j, m) <- zip [0 :: Int ..] monthNames ]
         ++ [ Color col (polyline 2.5 [ (gX g (fromIntegral (j + 1)), gY g (sel k c)) | (j, c) <- zip [0 :: Int ..] (U.toList cs) ])
            , Color (rgba255 255 255 255 120) (Line [(gX g (fromIntegral doy), gy g), (gX g (fromIntegral doy), gy g + gh)])
            , Color col (circleD (gX g (fromIntegral doy)) (gY g cur) 10) ]

-- 横から見た地球：太陽の光は右から。地軸は光の方向へ δ だけ傾いて見える
drawEarthSide :: World -> Float -> Float -> Float -> Double -> Picture
drawEarthSide w cx cy rr dec = Pictures $
  concat [ [ Color (rgba255 255 225 140 90) (Line [(cx + rr + 20, y), (cx + rr + 80, y)])
           , Color (rgba255 255 225 140 90) (Line [(cx + rr + 20, y), (cx + rr + 28, y - 4)]) ]
         | i <- [-4 .. 4 :: Int], let y = cy + fromIntegral i * rr * 0.24 ]
  ++ [ Color (rgb255 30 50 100) (circleD cx cy (2 * rr))
     , Color (rgb255 70 130 200) (Polygon [ (cx + rr * cos t, cy + rr * sin t) | k <- [0 .. 40 :: Int], let t = -pi / 2 + pi * fromIntegral k / 40 ])   -- 昼の側（右半分）
     , Color white (thickLine 2 (rp 0 (-rr * 1.25)) (rp 0 (rr * 1.25))) ]
  ++ [ Color (rgba255 200 210 240 120) (Line [rp (-x) y, rp x y])
     | la <- [66.56, 23.44, 0, -23.44, -66.56], let y = -rr * sin (la * pi / 180), let x = rr * cos (la * pi / 180) ]
  ++ [ Color (rgb255 250 225 140) (thickLine 3 (rp (-lx) ly) (rp lx ly))
     , Color (rgb255 255 200 90) (uncurry circleD (rp lx ly) 12)       -- 正午の地点
     , say "Side view: light from the right, left half = night" 20 (cy + rr + 44) 14 (rgb255 220 225 240)
     , say ("Yellow = path of latitude " ++ showJ (wLat w) ++ " in one day") 20 (cy + rr + 62) 14 (rgb255 220 225 240)
     , say "Part on the day side = day length / 24 h" 20 (cy + rr + 80) 14 (rgb255 220 225 240) ]
  where
    a = f2 (dec * degR)
    -- 地球の座標（上が北極）を δ だけ回して画面へ（δ > 0 なら北極が太陽のほうへ）
    rp x y = (cx + x * cos a - y * sin a, cy + x * sin a + y * cos a)
    lt = f2 (wLat w * degR)
    ly = -rr * sin lt
    lx = rr * cos lt

-- 真上から見た公転：地軸はいつも同じ向き（北極は図の右上へ傾く）
drawOrbitTop :: World -> Float -> Float -> Float -> Picture
drawOrbitTop w cx cy rr = Pictures
  [ Color (rgb255 90 100 140) (Line [ (cx + 1.2 * rr * cos t, cy + 0.62 * rr * sin t) | k <- [0 .. 72 :: Int], let t = tau * fromIntegral k / 72 ])
  , Color (rgb255 255 210 100) (circleD cx cy 24)
  , Color (rgb255 70 130 200) (circleD ex ey 18)
  , Color white (thickLine 2 (ex - t, ey + 22 * ct * 0.4) (ex + t, ey - 22 * ct * 0.4 - 10))
  , sayR "Mar eq." (cx - 1.2 * rr - 6) (cy + 5) 14 c
  , say "Sep eq." (cx + 1.2 * rr + 6) (cy + 5) 14 c
  , sayC "Dec sol." cx (cy - 0.62 * rr - 8) 14 c
  , sayC "Jun sol." cx (cy + 0.62 * rr + 20) 14 c
  , say "Top view (the axis points the same way all year)" 20 (cy + rr * 0.62 + 42) 14 c ]
  where
    a = f2 ((fst (sunOf (wDoy w)) + 180) * degR)
    ex = cx + 1.2 * rr * cos a
    ey = cy - 0.62 * rr * sin a
    t = sin (f2 (wTilt w * degR)) * 22
    ct = cos (f2 (wTilt w * degR))
    c = rgb255 200 205 225

dateLabel :: Int -> String
dateLabel d = getAt "" monthNames m ++ " " ++ show (d - getAt 0 cum m) ++ " (day " ++ show d ++ ")"
  where
    cum = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
    m = length (takeWhile (< d) (drop 1 cum))

-- ---- キーとマウス
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey k) Down md _ -> pure (remake (arrowKey k (if shift md == Down then 10 else 1)))
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> pure (remake w { wTilt = getAt 23.44 tilts ((idx + 1) `mod` length tilts) })
    | otherwise -> pure (remake w)
  EventKey (MouseButton LeftButton) Down _ _ -> pure (drag w)
  EventMotion _ | envDown env -> pure (drag w)
  _ -> pure w
  where
    idx = let k = length (takeWhile (/= wTilt w) tilts) in if k >= length tilts then -1 else k   -- indexOf（見つからなければ −1）
    arrowKey k st = case k of
      KeyUp -> w { wLat = min 90 (wLat w + 5) }
      KeyDown -> w { wLat = max (-90) (wLat w - 5) }
      KeyRight -> w { wDoy = ((wDoy w - 1 + st) `mod` 365) + 1 }
      KeyLeft -> w { wDoy = ((wDoy w - 1 - st + 3650) `mod` 365) + 1 }
      KeySpace -> w { wPlaying = not (wPlaying w) }
      _ -> w
    (mx, _) = mouseP5 env
    drag w0
      | mx > 540 && mx < envW env - 40 =
          w0 { wDoy = max 1 (min 365 (floor (1 + realToFrac ((mx - 540) / (envW env - 580)) * 364 + 0.5 :: Double))) }
      | otherwise = w0

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("equinox (delta = 0): 12 h of daylight at every latitude", all (\p -> near (dayLength p 0) 12 1e-9) [0, 35, 60, -45, 80])
  , ("noon altitude at the June solstice (35N) = 90 - 35 + 23.44 = 78.44", near (noonAlt 35 23.44) 78.44 1e-9)
  , ("arctic (70N): polar night (0 h) in December, midnight sun (24 h) in June", near (dayLength 70 (-23.44)) 0 1e-9 && near (dayLength 70 23.44) 24 1e-9)
  , ("tilt 0: 12 h of daylight all year even at 50N", all (\d -> near (dayLength 50 (declination d 0)) 12 1e-9) [1 .. 365])
  , ("insolation at the equator, equinox, r = 1 = S0/pi ~ 433 W/m2", near (insolation 0 0 1) (s0 / pi) 1e-9 && near (s0 / pi) 433.2 0.1)
  , ("max declination of the year ~ 23.44, around the June solstice (Jun 19-23)", near dmax 23.44 0.05 && imax + 1 >= 170 && imax + 1 <= 174)
  , ("closest to the sun (perihelion) in early January, r ~ 0.983 AU", rmin + 1 <= 8 && near rv 0.9833 0.001)
  , ("day lengths in the north and south add up to 24 h (also for delta /= 0)", near (dayLength 40 15 + dayLength (-40) 15) 24 1e-9) ]
  where
    ds = [ declination d 23.44 | d <- [1 .. 365] ]
    rs = [ snd (sunOf d) | d <- [1 .. 365] ]
    (imax, dmax) = argBest (>) ds
    (rmin, rv) = argBest (<) rs

-- 最初に見つかった「いちばん」の番号と値（indexOf(Math.max(...)) と同じ）
argBest :: (Double -> Double -> Bool) -> [Double] -> (Int, Double)
argBest better xs = foldl' (\(bi, bv) (i, v) -> if better v bv then (i, v) else (bi, bv)) (0, getAt 0 xs 0) (zip [0 ..] xs)

-- ---------- やってみよう ----------
-- 1. ↑↓ で緯度を 0°（赤道）、35°、66.6°（北極圏）、90° にして、昼の長さのグラフの形を比べよう
-- 2. T で傾きを 0° に。グラフが平らになる = 季節がなくなる。45° にすると？
-- 3. 夏至の日射量は、赤道と北極ではどちらが大きい？（北極は 1 日中日が当たる）
-- 4. 南半球（−35°）にして、季節が逆になることを確かめよう
-- 5. s0 を変えると、どのグラフが変わる？ 昼の長さは変わる？

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
        | otherwise = showFFloat Nothing v ""

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
