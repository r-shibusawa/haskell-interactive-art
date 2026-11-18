-- ============================================
-- 53-03 星空の回転 ─ 赤経・赤緯から「いま、どの高さ・方角に見えるか」を計算する
-- 地方恒星時 LST = グリニッジ恒星時 + 経度、時角 H = LST − 赤経 α
-- 高度：sin h = sin φ sin δ + cos φ cos δ cos H　方位：A = atan2(−cos δ sin H, sin δ cos φ − cos δ cos H sin φ)
-- 図は真上を見上げた全天（中心 = 天頂、円 = 地平線、上が北・左が東）。星の位置は J2000（誤差 1° 未満）
-- 操作：↑↓ = 緯度 / ←→ = 1 時間（Shift で 1 日）/ スペース = 早送り / T = 軌跡 / C = 星座の線 / R = いまの時刻
-- ============================================
module Main (main) where

import Art
import Data.List (find, foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import Foreign.C.Types (CTime(..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

lon, tz :: Double
lon = 139.7                            -- ★ 経度 [度、東が +]（東京）
tz = 9                                 -- ★ 時差 [時間]

-- 明るい星：名前、赤経 [時]、赤緯 [度]、等級（J2000、おおよその値）
data Star = Star { stName :: String, stRa, stDec, stMag :: !Double }

stars :: [Star]
stars = map (\(n, a, d, m) -> Star n a d m)
  [ ("Polaris", 2.530, 89.264, 2.0), ("Sirius", 6.7525, -16.716, -1.46), ("Canopus", 6.399, -52.696, -0.74), ("Arcturus", 14.261, 19.182, -0.05)
  , ("Vega", 18.6156, 38.784, 0.03), ("Capella", 5.2782, 45.998, 0.08), ("Rigel", 5.2423, -8.202, 0.13), ("Procyon", 7.655, 5.225, 0.34)
  , ("Betelgeuse", 5.9195, 7.407, 0.5), ("Achernar", 1.6286, -57.237, 0.46), ("Altair", 19.8464, 8.868, 0.77), ("Aldebaran", 4.5987, 16.509, 0.85)
  , ("Antares", 16.4901, -26.432, 1.0), ("Spica", 13.4199, -11.161, 0.97), ("Pollux", 7.7553, 28.026, 1.14), ("Fomalhaut", 22.9608, -29.622, 1.16)
  , ("Deneb", 20.6905, 45.280, 1.25), ("Regulus", 10.1395, 11.967, 1.35), ("Acrux", 12.4433, -63.099, 0.76), ("Hadar", 14.0637, -60.373, 0.61)
  , ("Rigil Kentaurus", 14.660, -60.835, -0.27), ("Mimosa", 12.7953, -59.689, 1.25), ("Gacrux", 12.5194, -57.113, 1.64), ("Imai", 12.2524, -58.749, 2.8)
  , ("Bellatrix", 5.4189, 6.350, 1.64), ("Alnitak", 5.6793, -1.943, 1.77), ("Alnilam", 5.6036, -1.202, 1.69), ("Mintaka", 5.5334, -0.299, 2.23)
  , ("Saiph", 5.7959, -9.670, 2.06), ("Castor", 7.5767, 31.888, 1.58), ("Dubhe", 11.0621, 61.751, 1.79), ("Merak", 11.0307, 56.383, 2.37)
  , ("Phecda", 11.8972, 53.695, 2.44), ("Megrez", 12.2571, 57.033, 3.31), ("Alioth", 12.9005, 55.960, 1.77), ("Mizar", 13.3988, 54.925, 2.23)
  , ("Alkaid", 13.7923, 49.313, 1.86), ("Caph", 0.1529, 59.150, 2.27), ("Schedar", 0.6751, 56.537, 2.24), ("Tsih", 0.9451, 60.717, 2.47)
  , ("Ruchbah", 1.4302, 60.235, 2.68), ("Segin", 1.9066, 63.670, 3.37), ("Sadr", 20.3705, 40.257, 2.23), ("Albireo", 19.512, 27.960, 3.1)
  , ("Kochab", 14.8451, 74.156, 2.08), ("Mirfak", 3.4054, 49.861, 1.79), ("Algol", 3.1361, 40.956, 2.1), ("Alpheratz", 0.1398, 29.091, 2.06)
  , ("Elnath", 5.4382, 28.608, 1.65), ("Denebola", 11.8177, 14.572, 2.14), ("Shaula", 17.5601, -37.104, 1.62), ("Alcyone (M45)", 3.7914, 24.105, 2.87) ]

-- 星座の線（stars の名前で）。True = 大三角などの目安の線
clines :: [(String, String, Bool)]
clines =
  [ ("Betelgeuse", "Bellatrix", False), ("Bellatrix", "Mintaka", False), ("Mintaka", "Alnilam", False), ("Alnilam", "Alnitak", False), ("Alnitak", "Saiph", False), ("Mintaka", "Rigel", False), ("Betelgeuse", "Alnitak", False)
  , ("Dubhe", "Merak", False), ("Merak", "Phecda", False), ("Phecda", "Megrez", False), ("Megrez", "Dubhe", False), ("Megrez", "Alioth", False), ("Alioth", "Mizar", False), ("Mizar", "Alkaid", False)
  , ("Caph", "Schedar", False), ("Schedar", "Tsih", False), ("Tsih", "Ruchbah", False), ("Ruchbah", "Segin", False), ("Deneb", "Sadr", False), ("Sadr", "Albireo", False)
  , ("Acrux", "Gacrux", False), ("Mimosa", "Imai", False), ("Castor", "Pollux", False)
  , ("Vega", "Deneb", True), ("Deneb", "Altair", True), ("Altair", "Vega", True), ("Betelgeuse", "Sirius", True), ("Sirius", "Procyon", True), ("Procyon", "Betelgeuse", True) ]

-- 軌跡の画像（p5.js の trailG の代わり）：全天の正方形を trailN × trailN のます目にして、明るさ 0〜1 をためる
trailN :: Int
trailN = 274                           -- 274² ≈ 7.5 万画素

data World = World
  { wLat    :: !Double
  , wJd     :: !Double
  , wFast   :: !Bool
  , wTrails :: !Bool
  , wLines  :: !Bool
  , wTrail  :: !(U.Vector Float)
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-03 Rotating starry sky" world0)
  { skBack  = rgb255 4 8 24
  , skStart = \_ -> do
      printSelfTest selfTest
      j <- jdNowUT
      pure world0 { wJd = j, wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 35.7 2451545 False False True blankTrail []

blankTrail :: U.Vector Float
blankTrail = U.replicate (trailN * trailN) 0

-- ---- 星の位置の計算
-- グリニッジ平均恒星時 [度]（J2000.0 で 280.46°、1 日に 360.9856°）
gmst :: Double -> Double
gmst jd = wrap360 (280.46061837 + 360.98564736629 * (jd - 2451545.0))

-- 赤経 α [時]・赤緯 δ [度] → (高度 h, 方位 A) [度]（A は北から東まわり）
altAz :: Double -> Double -> Double -> Double -> (Double, Double)
altAz raH dec phi lstDeg = (asin (clampv (-1) 1 sh) / degR, wrap360 (a / degR))
  where
    h = (lstDeg - raH * 15) * degR
    d = dec * degR
    p = phi * degR
    sh = sin p * sin d + cos p * cos d * cos h
    a = atan2 (negate (cos d) * sin h) (sin d * cos p - cos d * cos h * sin p)

-- 全天の図の座標（中心 = 天頂、半径 = 地平線。上が北、左が東）
skyXY :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
skyXY alt az cx cy rr = (cx - r * sin (az * degR), cy - r * cos (az * degR))
  where r = (90 - alt) / 90 * rr

-- 2 つの星の角距離 [度]
sepDeg :: Star -> Star -> Double
sepDeg a b = acos (clampv (-1) 1 (sin d1 * sin d2 + cos d1 * cos d2 * cos (r1 - r2))) / degR
  where
    (r1, d1) = (stRa a * 15 * degR, stDec a * degR)
    (r2, d2) = (stRa b * 15 * degR, stDec b * degR)

byName :: String -> Star
byName n = maybe (Star n 0 0 9) id (find ((== n) . stName) stars)

-- 星と、いまの (高度, 方位, 画面の位置)
data Pos = Pos { pStar :: Star, pH, pA :: !Double, pXY :: !(Float, Float) }

-- 全天の図の中心と半径
skyGeom :: Env -> (Double, Double, Double)
skyGeom env = (realToFrac (envW env) * 0.4, realToFrac (envH env) * 0.55, min (realToFrac (envW env) * 0.36) (realToFrac (envH env) * 0.41))

positions :: Env -> World -> [Pos]
positions env w = [ Pos s h a (f2 x, f2 y) | s <- stars, let (h, a) = altAz (stRa s) (stDec s) (wLat w) lst, let (x, y) = skyXY h a cx cy rr ]
  where
    lst = wrap360 (gmst (wJd w) + lon)
    (cx, cy, rr) = skyGeom env

-- 1 フレーム：早送りなら 4 分進め、軌跡がオンなら見えている星の点をためる
step :: Env -> World -> World
step env w0
  | wTrails w = w { wTrail = U.accum (\v a -> v + (1 - v) * a) (wTrail w) dots }
  | otherwise = w
  where
    w = if wFast w0 then w0 { wJd = wJd w0 + 1 / (24 * 60) * 4 } else w0   -- 1 フレームで 4 分（1 秒で 4 時間）
    (cx, cy, rr) = skyGeom env
    cell = 2 * rr / fromIntegral trailN
    dots = [ (j * trailN + i, 0.47) | p <- positions env w, pH p >= 0
           , let (x, y) = pXY p
           , let i = floor ((realToFrac x - (cx - rr)) / cell), let j = floor ((realToFrac y - (cy - rr)) / cell)
           , i >= 0, i < trailN, j >= 0, j < trailN ]

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ if wTrails w then Pictures [trailPic, Color (rgb255 4 8 24) (at (cx, cy) (ring (rr + 3) 6))]   -- 画素のぎざぎざを地平線の外で隠す
                 else Color (rgb255 14 22 52) (circleD cx cy (2 * rr)) ]
  ++ [ Color (rgb255 60 70 110) (Pictures [ circleLine cx cy (rr / 3), circleLine cx cy (2 * rr / 3)
                                          , Line [(cx - rr, cy), (cx + rr, cy)], Line [(cx, cy - rr), (cx, cy + rr)] ]) ]
  ++ (if wLines w then [ if big then Color (rgba255 255 210 120 90) (Line [pXY a, pXY b]) else Color (rgba255 130 160 230 150) (thickLine 1.5 (pXY a) (pXY b))
                       | (na, nb, big) <- clines, Just a <- [findP na], Just b <- [findP nb], pH a >= 0, pH b >= 0 ] else [])
  ++ concat [ Color (rgb255 255 250 225) (uncurry circleD (pXY p) (f2 (clampv 1.5 10 (7 - 1.6 * stMag (pStar p)))))
              : [ say (stName (pStar p)) (fst (pXY p) + 6) (snd (pXY p) - 4) 14 (rgb255 200 205 230) | stMag (pStar p) < 1.3 || stName (pStar p) == "Polaris" ]
            | p <- ps, pH p >= 0 ]
  ++ [ sayC "N" cx (cy - rr - 8) 16 c1, sayC "S" cx (cy + rr + 20) 16 c1, sayC "E" (cx - rr - 16) (cy + 6) 16 c1, sayC "W" (cx + rr + 16) (cy + 6) 16 c1 ]
  -- 右の表：明るい星の高度と方位
  ++ [ say "Visible bright stars (alt / az)" x0 (y0 - 14) 14 (rgb255 240 236 215) ]
  ++ concat [ [ say (stName (pStar p)) x0 (y0 + 10 + fromIntegral i * 22) 14 (rgb255 220 225 240)
              , say (fmt 1 (pH p) ++ " / " ++ fmt 0 (pA p)) (x0 + 140) (y0 + 10 + fromIntegral i * 22) 14 c1 ]
            | (i, p) <- zip [0 :: Int ..] vis ]
  ++ [ say ("Polaris altitude " ++ fmt 2 (maybe 0 pH (findP "Polaris"))) x0 (y0 + 22 * 14 + 30) 14 (rgb255 160 200 255)
     , say ("~ latitude " ++ fmt 1 (wLat w)) x0 (y0 + 22 * 14 + 50) 14 (rgb255 160 200 255)
     , say "Rotating sky | Up/Down latitude  Left/Right 1 hour (Shift: 1 day)  Space fast  T trails  C lines  R now" 20 34 17 (rgb255 240 236 215)
     , say (dateStr (wJd w + tz / 24) ++ " " ++ pad2 (floor lh) ++ ":" ++ pad2 (floor (jsRem lh 1 * 60)) ++ " (UTC+" ++ showJ tz ++ ")   latitude " ++ fmt 1 (wLat w)
            ++ "   longitude " ++ showJ lon ++ "   local sidereal time LST = " ++ fmt 3 (lst / 15) ++ " h" ++ (if wFast w then "   fast" else "")) 20 60 15 c1
     , drawSelfTest env (wTest w) ]
  where
    (cxd, cyd, rrd) = skyGeom env
    (cx, cy, rr) = (f2 cxd, f2 cyd, f2 rrd)
    ps = positions env w
    findP n = find ((== n) . stName . pStar) ps
    lst = wrap360 (gmst (wJd w) + lon)
    x0 = envW env * 0.78
    y0 = 120
    vis = take 14 (sortOn (stMag . pStar) (filter ((> 0) . pH) ps))
    (_, _, _, lh) = dateFromJd (wJd w + tz / 24)
    c1 = rgb255 250 225 140
    -- 軌跡の画像：円の中は空の色、外は背景の色に、ためた明るさで星の色を混ぜる
    trailPic = at (cx, cy) $ Scale 1 (-1) $ pixelImage trailN trailN (rr * 2 / fromIntegral trailN) $ \i j ->
      let v = getV (j * trailN + i)
          dx = (fromIntegral i + 0.5) / fromIntegral trailN * 2 - 1
          dy = (fromIntegral j + 0.5) / fromIntegral trailN * 2 - 1
          (br, bg, bb) = if dx * dx + dy * dy <= (1 + 3 / fromIntegral trailN) ^ (2 :: Int) then (14, 22, 52) else (4, 8, 24)
          mix a b = fromIntegral (round (a + (b - a) * v) :: Int)
      in (mix br 255, mix bg 240, mix bb 200)
    getV k = maybe 0 id (wTrail w U.!? k)
    pad2 :: Int -> String
    pad2 n = (if n < 10 then "0" else "") ++ show n

-- 円の線（半径 r）
circleLine :: Float -> Float -> Float -> Picture
circleLine x y r = Line [ (x + r * cos t, y + r * sin t) | k <- [0 .. 72 :: Int], let t = tau * fromIntegral k / 72 ]

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (clear w { wLat = min 90 (wLat w + 5) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (clear w { wLat = max (-90) (wLat w - 5) })
  EventKey (SpecialKey KeyRight) Down md _ -> pure (clear w { wJd = wJd w + (if shift md == Down then 1 else 1 / 24) })
  EventKey (SpecialKey KeyLeft) Down md _ -> pure (clear w { wJd = wJd w - (if shift md == Down then 1 else 1 / 24) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFast = not (wFast w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> do j <- jdNowUT
                          pure (clear w { wJd = j })
    | c `elem` "tT" -> pure (clear w { wTrails = not (wTrails w) })
    | c `elem` "cC" -> pure w { wLines = not (wLines w) }
  _ -> pure w
  where clear w' = w' { wTrail = blankTrail }

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Greenwich sidereal time at J2000.0 (2000-01-01 12:00 UT) = 18.697 h", near (gmst 2451545.0 / 15) 18.6973 0.001)
  , ("sidereal time gains 3 min 56 s per solar day (1 sidereal day = 23 h 56 min 4 s)", near (86400 * 360 / 360.98564736629) 86164.09 0.05)
  , ("Polaris altitude ~ latitude (35N, within 1 at any time)", all (\l -> near (fst (altAz (stRa pol) (stDec pol) 35 l)) 35 0.75) [0, 90, 180, 270])
  , ("altitude on the meridian (H = 0) = 90 - |phi - delta| (Vega, 86.2 at 35N, azimuth north)", near (fst mer) (90 - abs (35 - stDec v)) 1e-9 && near (snd mer) 0 1e-6)
  , ("at 80N Sirius (delta = -16.7) never rises", foldl' max (-999) hs < 0)
  , ("at 35N Dubhe (delta = 61.8 > 90 - 35) never sets (circumpolar)", foldl' min 999 hd > 0)
  , ("the pointers Dubhe and Merak are about 5.4 apart", near (sepDeg du (byName "Merak")) 5.4 0.15)
  , ("east horizon (azimuth 90) is the left edge of the chart, zenith is the center", near (fst (skyXY 0 90 0 0 100)) (-100) 1e-9 && near (fst (skyXY 90 0 0 0 100)) 0 1e-9) ]
  where
    pol = byName "Polaris"
    v = byName "Vega"
    mer = altAz (stRa v) (stDec v) 35 (stRa v * 15)
    si = byName "Sirius"
    hs = [ fst (altAz (stRa si) (stDec si) 80 l) | l <- [0, 5 .. 355] ]
    du = byName "Dubhe"
    hd = [ fst (altAz (stRa du) (stDec du) 35 l) | l <- [0, 5 .. 355] ]

-- ---------- やってみよう ----------
-- 1. スペースと T で軌跡を描こう。北極星のまわりを回る中心はどこ？ 1 時間で何度回る？（約 15°）
-- 2. ↑↓ で緯度を 90°（北極）、0°（赤道）、−35°（南半球）に。軌跡の形はどう変わる？
-- 3. Shift + →で 1 日ずつ進めると、同じ時刻の星空は 1 日に約 1° 西へずれる（1 か月で 2 時間ぶん）
-- 4. 南十字星（Acrux）が見えるのは緯度何度より南？（δ = −63° から考えよう）
-- 5. stars に好きな星を足して、自分の星座の線を clines に加えよう

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
