-- ============================================
-- 53-08 黒体放射と星の色 ─ 温度だけで、星の色と明るさが決まる
-- プランクの式 B(λ, T) = 2hc² / λ⁵ ÷ (e^(hc/λkT) − 1)　→ 山の位置 λmax = b / T（ウィーンの法則、b = 2.898 × 10⁻³ m·K）
-- 1 m² から出る光の量 = σT⁴（シュテファン-ボルツマンの法則。曲線の下の面積 × π を数値積分して確かめる）
-- 見える色 = スペクトルに人の目の色合わせ関数 x̄ ȳ z̄（CIE 1931 の近似式）を掛けて積分 → XYZ → sRGB
-- 操作：←→ = 温度（Shift で大きく）/ 1〜8 = 実在の星 / L = 縦軸を対数に / 星の温度はおおよその値
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (expm1, showFFloat)
import System.Environment (lookupEnv)

hP, cL, kB, sigmaSB, wienB :: Double
hP = 6.62607015e-34
cL = 2.99792458e8
kB = 1.380649e-23
sigmaSB = 5.670374e-8
wienB = 2.897771955e-3

-- ★ 実在の星：名前、表面温度 [K]（おおよその値）
starList :: [(String, Double)]
starList = [("Betelgeuse", 3600), ("Aldebaran", 3900), ("Arcturus", 4300), ("Sun", 5772), ("Procyon", 6530), ("Vega", 9600), ("Rigel", 12100), ("Spica", 25000)]

-- いまの温度で計算した値（温度を変えたときだけ計算しなおす）
data Cache = Cache { cT :: !Double, cRgb :: [Double], cX, cY :: !Double, cFlux :: !Double }

data World = World
  { wTemp  :: !Double
  , wLogY  :: !Bool
  , wSel   :: !Int
  , wCache :: Cache
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-08 Blackbody radiation and star colors" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = setTemp 5772 (World 5772 False 3 (makeCache 5772) [])

makeCache :: Double -> Cache
makeCache t = let (x, y) = chroma t in Cache t (rgbOf (xyzOf t)) x y (totalFlux t)

setTemp :: Double -> World -> World
setTemp t w = w { wTemp = t, wCache = if cT (wCache w) == t then wCache w else makeCache t }

-- ---- プランクの式と色の計算
-- [W / (m² · sr · m)]、lam [m]
planck :: Double -> Double -> Double
planck lam t
  | x > 700 = 0                         -- e^x があふれないように（とても短い波長では 0）
  | otherwise = (2 * hP * cL * cL) / (lam ** 5) / expm1 x
  where x = (hP * cL) / (lam * kB * t)

-- CIE 1931 の色合わせ関数の近似（区分的なガウス関数の和。Wyman・Sloan・Shirley 2013）λ [nm]
gpw :: Double -> Double -> Double -> Double -> Double
gpw x mu s1 s2 = let t = (x - mu) / (if x < mu then s1 else s2) in exp (-0.5 * t * t)

cmf :: Double -> (Double, Double, Double)
cmf l = ( 1.056 * gpw l 599.8 37.9 31.0 + 0.362 * gpw l 442.0 16.0 26.7 - 0.065 * gpw l 501.1 20.4 26.2
        , 0.821 * gpw l 568.8 46.9 40.5 + 0.286 * gpw l 530.9 16.3 31.1
        , 1.217 * gpw l 437.0 11.8 36.0 + 0.681 * gpw l 459.0 26.0 13.8 )

xyzOf :: Double -> (Double, Double, Double)
xyzOf t = foldl' add (0, 0, 0) [380, 382 .. 780 :: Int]
  where add (x, y, z) l = let b = planck (fromIntegral l * 1e-9) t
                              (cx, cy, cz) = cmf (fromIntegral l)
                          in (x + b * cx, y + b * cy, z + b * cz)

chroma :: Double -> (Double, Double)
chroma t = let (x, y, z) = xyzOf t; s = x + y + z in (x / s, y / s)

-- XYZ → sRGB（いちばん大きいチャンネルを 1 にそろえてから、ガンマをかける）
rgbOf :: (Double, Double, Double) -> [Double]
rgbOf (x, y, z) = map gam [r, g, b]
  where
    r = 3.2406 * x - 1.5372 * y - 0.4986 * z
    g = -0.9689 * x + 1.8758 * y + 0.0415 * z
    b = 0.0557 * x - 0.2040 * y + 1.0570 * z
    m = foldl' max 1e-30 [r, g, b]
    gam v0 = let v = clampv 0 1 (v0 / m) in 255 * (if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055)

-- 曲線の山を黄金分割探索で探す
peakNumeric :: Double -> Double
peakNumeric t = go 50e-9 2e-4 (200 :: Int)
  where
    gr = (sqrt 5 - 1) / 2
    go a b 0 = (a + b) / 2
    go a b n = let c = b - gr * (b - a)
                   d = a + gr * (b - a)
               in if planck c t > planck d t then go a d (n - 1) else go c b (n - 1)

-- π ∫ B dλ を対数の刻みで数値積分（台形公式）
totalFlux :: Double -> Double
totalFlux t = pi * fst (foldl' stepI (0, f 0) [1 .. n])
  where
    n = 4000 :: Int
    l0 = log 10e-9
    l1 = log 1e-2
    dl = (l1 - l0) / fromIntegral n
    f i = let lam = exp (l0 + ((l1 - l0) * fromIntegral i) / fromIntegral n) in planck lam t * lam
    stepI (s, prev) i = let v = f i in (s + ((v + prev) / 2) * dl, v)

-- 一覧の星の色（1 回だけ計算）
starRgb :: [[Double]]
starRgb = [ rgbOf (xyzOf t) | (_, t) <- starList ]

-- 見える光の帯（380〜780 nm、400 × 1 画素）
rainbow :: Picture
rainbow = pixelImage 400 1 1 $ \i _ ->
  let c@(_, cy, _) = cmf (380 + (400 * fromIntegral i) / 399)
      yy = clampv 0 1 (cy * 1.4)
      q v = fromIntegral (max 0 (min 255 (round (v * yy) :: Int)))
  in case rgbOf c of
       [r, g, b] -> (q r, q g, q b)
       _ -> (0, 0, 0)

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ at ((gX g 380 + gX g 780) / 2, gy g + gh g - 7) (Scale ((gX g 780 - gX g 380) / 400) 14 rainbow)
  , plotAxes g "Wavelength lambda [nm]" ("B(lambda, T)  (peak at this T = 1)" ++ (if wLogY w then "   log: one tick = x10" else "")) [100, 500, 1000, 1500, 2000, 2500, 3000] [] ]
  ++ [ sayR s (gx g - 5) (gY g (v * pk) + 6) 14 (rgb255 160 170 195)
     | (v, s) <- if wLogY w then [(1e-4, "0.0001"), (1e-3, "0.001"), (1e-2, "0.01"), (0.1, "0.1"), (1, "1")] else [(0, "0"), (0.5, "0.5"), (1, "1")] ]
  -- 実在の星の曲線（同じ縦軸。はみ出す部分は上で止まる）
  ++ [ Color (col3 c (70 / 255)) (polyline 1 (curvePts g t)) | ((_, t), c) <- zip starList starRgb ]
  ++ [ Color (col3 (cRgb ca) 1) (polyline 3 (curvePts g temp))
     , Color (withA (200 / 255) white) (Line [(gX g (lmax * 1e9), gy g), (gX g (lmax * 1e9), gy g + gh g)])
     , say ("peak = " ++ fmt 0 (lmax * 1e9) ++ " nm") (gX g (lmax * 1e9) + 6) (gy g + 18) 14 (rgb255 250 225 140)
     , say "Visible light (380-780 nm)" (gX g 380) (gy g + gh g - 20) 14 (rgb255 170 175 200) ]
  -- ---- 星の色
  ++ [ Color (col3 (cRgb ca) (18 / 255)) (circleD sx sy (sr * 2 + fromIntegral i * 14)) | i <- [6, 5 .. 1 :: Int] ]
  ++ [ Color (col3 (cRgb ca) 1) (circleD sx sy (2 * sr))
     , sayC ("Computed color RGB(" ++ commas (map (fmt 0) (cRgb ca)) ++ ")") sx (sy + sr + 30) 15 (rgb255 240 236 215)
     , sayC ("Chromaticity x = " ++ fmt 4 (cX ca) ++ ", y = " ++ fmt 4 (cY ca)) sx (sy + sr + 52) 15 (rgb255 240 236 215) ]
  -- ---- 星の一覧
  ++ concat [ [ Color (col3 c 1) (circleD (x0 + 8) (y - 5) 14)
              , say (show (i + 1) ++ " " ++ nm ++ " " ++ showJ t ++ " K, peak " ++ fmt 0 ((wienB / t) * 1e9) ++ " nm") (x0 + 24) y 14
                    (if i == wSel w && temp == t then rgb255 250 225 140 else rgb255 210 215 230) ]
            | (i, ((nm, t), c)) <- zip [0 :: Int ..] (zip starList starRgb), let y = y0 + fromIntegral i * 24 ]
  ++ [ say "Blackbody & star colors | Left/Right temperature (Shift: big)  1-8 real stars  L log axis" 20 34 18 (rgb255 240 236 215)
     , say ("T = " ++ fmt 0 temp ++ " K   Wien: peak = b / T = " ++ fmt 1 (lmax * 1e9) ++ " nm   sigma T^4 = "
            ++ fmt 2 (sigmaSB * temp ** 4 / 1e6) ++ " MW/m2 (integral " ++ fmt 2 (cFlux ca / 1e6) ++ ")") 20 60 16 (rgb255 250 225 140)
     , say "Double T -> half the peak wavelength, 2^4 = 16x the light. Cool = red, Sun = white, hot = blue-white" 20 (h - 54) 14 (rgb255 180 185 210)
     , say "(No green stars: even when the peak is green, all the colors mix.)" 20 (h - 34) 14 (rgb255 180 185 210)
     , drawSelfTest env (wTest w) ]
  where
    h = envH env
    temp = wTemp w
    ca = wCache w
    lmax = wienB / temp
    pk = planck lmax temp
    g = Graph 70 110 (envW env * 0.56) (h * 0.62) 100 3000 (if wLogY w then pk * 1e-4 else 0) (pk * 1.1) False (wLogY w)
    sx = envW env * 0.8
    sy = h * 0.3
    sr = min 110 (h * 0.15)
    x0 = envW env * 0.66
    y0 = sy + sr + 90

curvePts :: Graph -> Double -> [Point]
curvePts g t =
  [ (gX g l, gY g (max b (if glogy g then gy0 g else 0)))
  | i <- [0 .. 160 :: Int]
  , let l = gx0 g + ((gx1 g - gx0 g) * fromIntegral i) / 160
  , let b = planck (l * 1e-9) t
  , not (glogy g && b <= 0) ]

-- 0〜255 の [r, g, b] と透明度 → 色
col3 :: [Double] -> Float -> Color
col3 c a = case c of
  [r, g, b] -> makeColor (f2 r / 255) (f2 g / 255) (f2 b / 255) a
  _ -> makeColor 1 1 1 a

commas :: [String] -> String
commas = foldr1' (\a b -> a ++ ", " ++ b)
  where foldr1' _ [] = ""
        foldr1' f xs = foldr1 f xs

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down md _ -> pure (setTemp (min 40000 (roundJ (wTemp w * fac md))) w)
  EventKey (SpecialKey KeyLeft) Down md _ -> pure (setTemp (max 1000 (roundJ (wTemp w / fac md))) w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '8' -> let s = fromEnum c - fromEnum '1' in pure (setTemp (snd (getAt ("", 5772) starList s)) w { wSel = s })
    | c `elem` "lL" -> pure w { wLogY = not (wLogY w) }
  _ -> pure w
  where fac md = if shift md == Down then 1.25 else 1.04

-- JavaScript の Math.round（.5 は大きい方へ）
roundJ :: Double -> Double
roundJ x = fromIntegral (floor (x + 0.5) :: Int)

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Wien: numerical peak x T = 2.898e-3 m K (3000, 5772, 10000 K)", all (\t -> near ((peakNumeric t * t) / wienB) 1 1e-4) [3000, 5772, 10000])
  , ("Stefan-Boltzmann: pi * integral of B = sigma T^4 (within 1%)", all (\t -> near (totalFlux t / (sigmaSB * t ** 4)) 1 0.01) [3000, 5772, 20000])
  , ("Sun: L = 4 pi R^2 sigma T^4 (R = 6.957e8 m, T = 5772 K) = 3.83e26 W (within 2%)", near (lSun / 3.83e26) 1 0.02)
  , ("chromaticity of 6500 K: x ~ 0.313, y ~ 0.324 (white)", near c65x 0.3135 0.01 && near c65y 0.3236 0.01)
  , ("3000 K is reddish (R > G > B), 10000 K is blue-white (B > R)"
    , case (rgbOf (xyzOf 3000), rgbOf (xyzOf 10000)) of
        ([r3, g3, b3], [r10, _, b10]) -> r3 > g3 && g3 > b3 && b10 > r10
        _ -> False)
  , ("matching function y-bar (brightness) peaks near 555 nm at about 1", bl >= 550 && bl <= 562 && near best 1 0.04)
  , ("double the temperature -> 16x the light", near (totalFlux 8000 / totalFlux 4000) 16 0.16) ]
  where
    lSun = 4 * pi * 6.957e8 ** 2 * sigmaSB * 5772 ** 4
    (c65x, c65y) = chroma 6500
    ybar l = let (_, y, _) = cmf (fromIntegral l) in y
    (bl, best) = foldl' (\(bi, bv) l -> if ybar l > bv then (l, ybar l) else (bi, bv)) (0 :: Int, 0) [400 .. 700 :: Int]

-- ---------- やってみよう ----------
-- 1. 1〜8 で星を選んで、色と山の位置を比べよう。ベテルギウスの山は見える光の外（赤外線）
-- 2. ←→ で温度を変え、白く見えるのは何 K くらいからか探そう
-- 3. L で縦軸を対数に。温度が上がると、どの波長でも光が強くなる（曲線は交わらない）
-- 4. 電球（約 2800 K）やろうそく（約 1900 K）の色を作ってみよう
-- 5. 緑色の星が見つからないのはなぜ？ 山が 500 nm 付近の温度にして色を確かめよう

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
        | abs v >= 1e-6 && abs v < 1e21 = showFFloat Nothing v ""
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

-- 有効数字 n 桁（toPrecision。末尾の 0 も残す）
toPrec :: Int -> Double -> String
toPrec n v
  | isNaN v || isInfinite v = "0"
  | v == 0 = showFFloat (Just (n - 1)) (0 :: Double) ""
  | e < -6 || e >= n = toExpo (n - 1) v
  | otherwise = showFFloat (Just (max 0 (n - 1 - e))) v ""
  where e = expOf v

-- 指数の形で小数点以下 n 桁（toExponential。例 1.20e+5）
toExpo :: Int -> Double -> String
toExpo n v
  | isNaN v || isInfinite v = "0"
  | v == 0 = showFFloat (Just n) (0 :: Double) "" ++ "e+0"
  | otherwise = showFFloat (Just n) m' "" ++ "e" ++ (if e' >= 0 then "+" else "") ++ show e'
  where
    e = expOf v
    m = v / 10 ^^ e
    r = fromIntegral (round (abs m * 10 ^ n) :: Integer) / 10 ^ n :: Double
    (m', e') = if r >= 10 then (signum m * r / 10, e + 1) else (signum m * r, e)

-- 10 の何乗の桁か
expOf :: Double -> Int
expOf v = let e = floor (logBase 10 (abs v)) :: Int
          in if abs v >= 10 ^^ (e + 1) then e + 1 else if abs v < 10 ^^ e then e - 1 else e
