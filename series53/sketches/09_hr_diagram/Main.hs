-- ============================================
-- 53-09 HR 図 ─ 星の明るさと温度の図で、星の大きさと一生がわかる
-- 横 = 表面温度 T（左ほど熱い、対数）、縦 = 光度 L（太陽 = 1、対数）
-- シュテファン-ボルツマン L = 4πR²σT⁴ より、半径 R/R☉ = √(L/L☉) × (T☉/T)²。斜めの点線は半径が同じ線
-- 主系列の目安：L ∝ M^3.5、R ∝ M^0.8（近似）→ 寿命 τ ≈ 100 億年 × M / L = 100 億年 × M^−2.5
-- 操作：クリック = 星を選ぶ / ←→ = 主系列の星の質量 M / N = 次の星 / 星の値はおおよその値
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

tSun, lSunW, rSunM, sigmaSB :: Double
tSun = 5772
lSunW = 3.828e26
rSunM = 6.957e8
sigmaSB = 5.670374e-8

-- ★ 実在の星：名前、光度 [L☉]、表面温度 [K]（おおよその値）、種類
hrStars :: [(String, Double, Double, String)]
hrStars =
  [ ("Sun", 1, 5772, "main sequence"), ("Sirius A", 25, 9940, "main sequence"), ("Vega", 40, 9600, "main sequence"), ("Procyon A", 7, 6530, "main sequence")
  , ("Spica", 20000, 25000, "main sequence"), ("Proxima Centauri", 0.0017, 3040, "main seq., red dwarf"), ("Arcturus", 170, 4300, "red giant")
  , ("Aldebaran", 440, 3900, "red giant"), ("Betelgeuse", 100000, 3600, "red supergiant"), ("Rigel", 120000, 12100, "blue supergiant"), ("Sirius B", 0.056, 25000, "white dwarf") ]

data World = World
  { wSel  :: !Int
  , wMass :: !Double
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-09 HR diagram" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 1.0 []

-- [R☉]
radiusOf :: Double -> Double -> Double
radiusOf l t = sqrt l * (tSun / t) ** 2

-- [L☉]
lumOf :: Double -> Double -> Double
lumOf r t = r * r * (t / tSun) ** 4

-- 主系列の星（質量 M [M☉]）：L、R、T、寿命（単位は 10 億年）
data MS = MS { msL, msR, msT, msLife :: !Double }

msStar :: Double -> MS
msStar m = MS l r (tSun * (l / (r * r)) ** 0.25) (10 * m ** (-2.5))
  where l = m ** 3.5
        r = m ** 0.8

-- 図の枠
data Frame = Frame { fx, fy, fw, fh :: !Float }

frameOf :: Env -> Frame
frameOf env = Frame 80 100 (envW env * 0.6) (envH env - 190)

xOfT :: Frame -> Double -> Float
xOfT g t = fx g + f2 ((logBase 10 40000 - logBase 10 t) / (logBase 10 40000 - logBase 10 2500)) * fw g

yOfL :: Frame -> Double -> Float
yOfL g l = fy g + fh g - f2 ((logBase 10 l + 4.5) / 11) * fh g

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgb255 70 80 120) (frameTL 1 (fx g) (fy g) (fw g) (fh g)) ]
  -- 温度の色の帯（下）
  ++ [ Color (tColorA t (160 / 255)) (rectTL (xOfT g t - fw g / 120) (fy g + fh g + 2) (fw g / 59 + 1) 6)
     | i <- [0 .. 59 :: Int], let t = 40000 * (2500 / 40000) ** (fromIntegral i / 59) ]
  -- 半径が同じ線
  ++ concat [ [ Color (rgba255 120 130 170 90) (dashed 4 6 pts) ]
              ++ [ say (showJ r ++ " Rsun") (px - 40) (py - 4) 14 (rgb255 130 140 180) | Just (px, py) <- [lastM pts] ]
            | r <- [0.001, 0.01, 0.1, 1, 10, 100, 1000]
            , let pts = [ (xOfT g t, yOfL g l) | i <- [0 .. 40 :: Int], let t = 40000 * (2500 / 40000) ** (fromIntegral i / 40), let l = lumOf r t
                        , l > 10 ** (-4.5), l < 10 ** 6.5 ] ]
  -- 主系列の目安（M = 0.1〜30）。枠の外は描かない
  ++ [ Color (rgba255 250 225 140 150) (Pictures [ thickLine 6 a b | (p, q) <- zip ms (drop 1 ms), Just (a, b) <- [clipSeg g p q] ])
     , say "Main sequence (M = 0.1 to 30 Msun)" (xOfT g 16000) (yOfL g 3000 - 30) 14 (rgba255 250 225 140 200)
     , say "Giants" (xOfT g 4300) (yOfL g 300 - 22) 14 (rgb255 170 175 200)
     , say "Supergiants" (xOfT g 6000) (yOfL g 1e5 - 18) 14 (rgb255 170 175 200)
     , say "White dwarfs" (xOfT g 20000) (yOfL g 0.003) 14 (rgb255 170 175 200) ]
  -- 実在の星
  ++ concat [ [ Color (tColor t) (circleD x y (r * 2)) ]
              ++ [ Color white (ringD x y (r * 2 + 10)) | i == wSel w ]
              ++ [ say nm (x + r + 4) (y + 5) 14 (rgb255 215 220 235) ]
            | (i, (nm, l, t, _)) <- zip [0 :: Int ..] hrStars
            , let x = xOfT g t, let y = yOfL g l
            , let r = f2 (clampv 4 22 (6 + 3 * logBase 10 (radiusOf l t + 0.01))) ]
  -- 主系列の星（質量 M）
  ++ [ Color white (Line [(mx - 10, my), (mx + 10, my)]), Color white (Line [(mx, my - 10), (mx, my + 10)]) ]
  -- 目盛り
  ++ [ sayC (showJ t ++ " K") (xOfT g t) (fy g + fh g + 28) 14 (rgb255 160 170 195) | t <- [40000, 20000, 10000, 5000, 3000] ]
  ++ [ sayR ("10^" ++ show e) (fx g - 6) (yOfL g (10 ^^ e) + 6) 14 (rgb255 160 170 195) | e <- [-4, -2 .. 6 :: Int] ]
  ++ [ say "Luminosity L [Lsun]" (fx g) (fy g - 8) 14 (rgb255 205 210 225)
     , sayR "Surface temperature T (hotter to the left)" (fx g + fw g) (fy g + fh g + 50) 14 (rgb255 205 210 225) ]
  -- ---- 右の数字
  ++ [ say nm0 x0 y0 17 (rgb255 240 236 215)
     , say ("(" ++ kind0 ++ ")") x0 (y0 + 22) 14 (rgb255 200 205 225) ]
  ++ rows (y0 + 50) 205
       [ ("Luminosity L", fmtV l0 ++ " Lsun"), ("Temperature T", showJ t0 ++ " K"), ("R=sqrt(L)(Ts/T)^2", fmtV rr ++ " Rsun")
       , ("", "= " ++ fmtV (rr * 109.1) ++ " R_earth"), ("L=4pi R^2 sT^4", toExpo 2 (4 * pi * (rr * rSunM) ** 2 * sigmaSB * t0 ** 4) ++ " W") ]
  ++ [ say ("Main seq. M = " ++ fmt 2 (wMass w) ++ " Msun") x0 y1 16 (rgb255 240 236 215)
     , say "(Left/Right: mass)" x0 (y1 + 20) 14 (rgb255 200 205 225) ]
  ++ rows (y1 + 48) 205
       [ ("L = M^3.5", fmtV (msL m) ++ " Lsun"), ("R = M^0.8", fmtV (msR m) ++ " Rsun"), ("T = Ts(L/R^2)^1/4", fmt 0 (msT m) ++ " K")
       , ("Life=10Gyr*M^-2.5", fmtLife (msLife m)) ]
  -- 大きさくらべ（太陽と、選んだ星。円の半径は R の 1/3 乗で縮めて描く）
  ++ [ Color (tColor 5772) (circleD (x0 + 30) by (2 * 12))
     , Color (tColor t0) (circleD (x0 + 130) by (2 * f2 (clampv 1 70 (12 * rr ** (1 / 3)))))
     , sayC "Sun" (x0 + 30) (by + 90) 14 (rgb255 170 175 200)
     , say nm0 (x0 + 100) (by + 90) 14 (rgb255 170 175 200)
     , say "(size drawn as R^(1/3))" x0 (by + 110) 14 (rgb255 170 175 200) ]
  ++ [ say "HR diagram | click = pick a star   N next star   Left/Right main-sequence mass" 20 34 18 (rgb255 240 236 215)
     , say "Dotted = same radius (L ~ T^4). Upper right = big (giants), lower left = small (white dwarfs)" 20 60 15 (rgb255 250 225 140)
     , drawSelfTest env (wTest w) ]
  where
    g = frameOf env
    m = msStar (wMass w)
    mx = xOfT g (msT m)
    my = yOfL g (msL m)
    ms = [ (xOfT g (msT s), yOfL g (msL s)) | i <- [0 .. 60 :: Int], let s = msStar (0.1 * 300 ** (fromIntegral i / 60)) ]
    (nm0, l0, t0, kind0) = getAt (head' hrStars) hrStars (wSel w)
    rr = radiusOf l0 t0
    x0 = fx g + fw g + 30
    y0 = 120
    lh = 24
    y1 = y0 + 50 + 5 * lh + 24
    by = y1 + 48 + 4 * lh + 84
    rows yt dx rs = concat [ [ say a x0 (yt + fromIntegral i * lh) 14 (rgb255 200 205 225), say b (x0 + dx) (yt + fromIntegral i * lh) 14 (rgb255 250 225 140) ]
                           | (i, (a, b)) <- zip [0 :: Int ..] rs ]
    head' xs = getAt ("", 1, 5772, "") xs 0

-- 有効数字 3 桁か、指数の形
fmtV :: Double -> String
fmtV v = if v >= 1000 || v < 0.01 then toExpo 2 v else toPrec 3 v

-- 寿命（10 億年単位）
fmtLife :: Double -> String
fmtLife gyr = if gyr >= 1 then toPrec 3 gyr ++ " Gyr" else toPrec 3 (gyr * 1000) ++ " Myr"

-- 温度 → だいたいの色（08 の計算の結果を、簡単な表にして線形補間）
tcol :: [(Double, (Double, Double, Double))]
tcol = [ (2500, (255, 150, 70)), (3600, (255, 190, 120)), (5000, (255, 228, 196)), (5772, (255, 240, 225)), (7000, (245, 243, 255))
       , (10000, (205, 220, 255)), (20000, (170, 195, 255)), (40000, (155, 180, 255)) ]

tColorA :: Double -> Float -> Color
tColorA t0 a = go tcol
  where
    t = clampv 2500 40000 t0
    mk (r, g, b) = makeColor (f2 r / 255) (f2 g / 255) (f2 b / 255) a
    go ((ta, ca) : rest@((tb, cb) : _))
      | t <= tb = let s = (t - ta) / (tb - ta)
                      l u v = u + s * (v - u)
                      (r1, g1, b1) = ca
                      (r2, g2, b2) = cb
                  in mk (l r1 r2, l g1 g2, l b1 b2)
      | otherwise = go rest
    go _ = mk (155, 180, 255)

tColor :: Double -> Color
tColor t = tColorA t 1

-- 線分を枠の中だけに切る（リャン-バースキー法）
clipSeg :: Frame -> Point -> Point -> Maybe (Point, Point)
clipSeg g (x1, y1) (x2, y2) = go [(-dx, x1 - fx g), (dx, fx g + fw g - x1), (-dy, y1 - fy g), (dy, fy g + fh g - y1)] 0 1
  where
    dx = x2 - x1
    dy = y2 - y1
    go [] a b = if a <= b then Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy)) else Nothing
    go ((p, q) : rest) a b
      | p == 0 = if q < 0 then Nothing else go rest a b
      | p < 0 = go rest (max a (q / p)) b
      | otherwise = go rest a (min b (q / p))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off pts = Pictures (go 0 (zip pts (drop 1 pts)))
  where
    per = on + off
    go _ [] = []
    go s0 (((ax, ay), (bx, by)) : rest) =
      let l = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
          pt d = (ax + (bx - ax) * d / max 1e-6 l, ay + (by - ay) * d / max 1e-6 l)
          segs = [ Line [pt (max 0 d0), pt (min l (d0 + on))] | k <- [0 .. ceiling (l / per) + 1 :: Int]
                 , let d0 = fromIntegral k * per - s0, d0 + on > 0, d0 < l ]
          s' = let v = s0 + l in v - per * fromIntegral (floor (v / per) :: Int)
      in segs ++ go s' rest

-- 円の線（直径 d）
ringD :: Float -> Float -> Float -> Picture
ringD x y d = at (x, y) (Circle (d / 2))

lastM :: [a] -> Maybe a
lastM = foldl (\_ x -> Just x) Nothing

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        g = frameOf env
        pick (bi, bd) (i, (_, l, t, _)) = let d = sqrt ((mx - xOfT g t) ^ (2 :: Int) + (my - yOfL g l) ^ (2 :: Int)) in if d < bd then (i, d) else (bi, bd)
        (best, _) = foldl' pick (-1, 30) (zip [0 ..] hrStars)
    in pure (if best >= 0 then w { wSel = best } else w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wMass = min 30 (wMass w * 1.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wMass = max 0.1 (wMass w / 1.1) }
  EventKey (Char c) Down _ _ | c `elem` "nN" -> pure w { wSel = (wSel w + 1) `mod` length hrStars }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Sun: 4 pi Rsun^2 sigma Tsun^4 = 3.83e26 W (within 2%)", near ((4 * pi * rSunM ** 2 * sigmaSB * tSun ** 4) / 3.83e26) 1 0.02)
  , ("radius from the Sun's L and T = 1 Rsun", near (radiusOf 1 tSun) 1 1e-12)
  , ("radius and luminosity formulas are inverses (R -> L -> R)", near (radiusOf (lumOf 37 4100) 4100) 37 1e-9)
  , ("Sirius B is about Earth-sized (0.008 to 0.02 Rsun) = white dwarf", sb > 0.008 && sb < 0.02)
  , ("Betelgeuse is over 500x the Sun's size (supergiant)", radiusOf 100000 3600 > 500)
  , ("main-sequence guide M = 1 passes the Sun (L = 1, T = 5772 K)", near (msL (msStar 1)) 1 1e-12 && near (msT (msStar 1)) tSun 1e-9)
  , ("lifetime: M = 1 -> 10 Gyr, M = 2 -> ~1.8 Gyr, M = 10 -> ~32 Myr"
    , near (msLife (msStar 1)) 10 1e-12 && near (msLife (msStar 2)) 1.768 0.01 && near (msLife (msStar 10)) 0.0316 0.001)
  , ("lines of equal radius have slope 4 (L ~ T^4)", near (logBase 10 (lumOf 5 20000 / lumOf 5 2000)) 4 1e-12) ]
  where sb = radiusOf 0.056 25000

-- ---------- やってみよう ----------
-- 1. 星をクリックして半径を見よう。いちばん大きいのは？ いちばん小さいのは？
-- 2. ←→ で質量を変えて、主系列の上を動く星を見よう。重い星ほど明るく、熱く、短命
-- 3. 太陽の 10 倍の質量の星の寿命は？ 恐竜がいた時代（約 6600 万年前）と比べよう
-- 4. hrStars に知っている星を足そう（光度と温度は理科年表やウィキペディアで）
-- 5. 太陽は将来、赤色巨星（右上）を通って白色矮星（左下）になる。その道すじを線で描いてみよう

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
