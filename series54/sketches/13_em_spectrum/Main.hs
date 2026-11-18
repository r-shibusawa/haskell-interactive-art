-- ============================================
-- 54-13 電磁波のスペクトル ─ 電波から ガンマ線まで、みんな同じ電磁波
-- 波長 λ と周波数 f は c = fλ（c = 3.00×10⁸ m/s）、光子 1 個のエネルギーは E = hf = hc/λ
-- 波長を対数の目盛りで並べ、身近な例と比べる。下の図は電場 E（たて）と磁場 B（横）が直角にゆれながら進む波
-- 操作：帯をクリック・ドラッグ = 波長をえらぶ / ←→ = 少しずつ / 1〜7 = 帯へジャンプ / スペース = 波を止める
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cLight, hPl, qE :: Double
cLight = 2.998e8                       -- 光の速さ [m/s]
hPl = 6.626e-34                        -- プランク定数 [J·s]
qE = 1.602e-19                         -- 電気素量 [C]

lMax, lMin :: Double
lMax = 1e4                             -- 帯のはし：10 km
lMin = 1e-13                           --            0.1 pm

-- 帯：名前・波長の範囲 [m]・色
data Band = Band { bName :: String, bLo, bHi :: !Double, bCol :: (Int, Int, Int) }

-- ★ 境目はおおよそ。X 線とガンマ線は重なっている
bands :: [Band]
bands =
  [ Band "Radio (radio, TV)" 1 1e4 (200, 120, 70)
  , Band "Microwave" 1e-3 1 (220, 160, 80)
  , Band "Infrared" 780e-9 1e-3 (200, 70, 60)
  , Band "Visible light" 380e-9 780e-9 (255, 255, 255)
  , Band "Ultraviolet" 10e-9 380e-9 (150, 90, 230)
  , Band "X-ray" 1e-11 10e-9 (90, 160, 230)
  , Band "Gamma ray" 1e-13 1e-11 (80, 220, 200) ]

-- ★ 身近な例（波長 [m]）
examples :: [(String, Double)]
examples =
  [ ("AM radio 1 MHz", 300), ("FM radio 80 MHz", 3.75), ("Digital TV 500 MHz", 0.6), ("Microwave oven, Wi-Fi 2.45 GHz", 0.1224), ("5G mmWave 28 GHz", 0.0107)
  , ("Body heat", 10e-6), ("TV remote", 940e-9), ("Red light", 650e-9), ("Green light", 530e-9), ("Blue light", 450e-9)
  , ("Sunburn UV-B", 300e-9), ("Germicidal lamp", 254e-9), ("Medical X-ray", 2e-11), ("Co-60 gamma", 9.3e-13) ]

-- 大きさの例 [m]
sizes :: [(String, Double)]
sizes = [("soccer field", 100), ("person", 1.7), ("bee", 0.01), ("hair width", 1e-4), ("cell", 1e-5), ("virus", 1e-7), ("molecule", 1e-9), ("atom", 1e-10), ("nucleus", 1e-14)]

data World = World
  { wLam    :: !Double                 -- 選んだ波長 [m]
  , wPhase  :: !Double
  , wMoving :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-13 EM spectrum" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wMoving w then w { wPhase = wPhase w + 0.06 } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 530e-9 0 True []

-- 帯の枠 (x, y, w, h)
type Bar = (Double, Double, Double, Double)

barOf :: Env -> Bar
barOf env = (40, 150, realToFrac (envW env) - 80, 60)

-- 波長 → x、x → 波長（対数の目盛り）
xOf :: Bar -> Double -> Double
xOf (bx, _, bw, _) l = bx + (logBase 10 (lMax / l) / logBase 10 (lMax / lMin)) * bw

lOf :: Bar -> Double -> Double
lOf (bx, _, bw, _) x = lMax / 10 ** (((x - bx) / bw) * logBase 10 (lMax / lMin))

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- 帯
  concat [ [ Color (rgba255 r g b 70) (rectTL (xf (bHi bd)) by (xf (bLo bd) - xf (bHi bd)) bh)
           , say (bName bd) (xf (bHi bd) + 4) (by - 8 - (if i == 4 then 18 else 0)) 14 (gray 0.9) ]
         | (i, bd) <- zip [0 :: Int ..] bands, let (r, g, b) = bCol bd ]
  ++ [ Color (rgbD (wavelengthRGB (lOf bar x * 1e9))) (Line [(f2 x, by), (f2 x, by + bh)])
     | x <- takeWhile (< xOf bar 380e-9) (iterate (+ 1) (xOf bar 780e-9)) ]
  ++ [ Color (gray 0.35) (frameTL 1 (f2 bxD) by (f2 bwD) bh) ]
  ++ concat [ [ Color (gray 0.35) (Line [(x, by + bh), (x, by + bh + 6)])
              , sayC (lenLabel False (10 ^^ e)) x (by + bh + 22) 14 (gray 0.63) ]
            | e <- [4, 3 .. -13 :: Int], let x = xf (10 ^^ e) ]
  -- 例（7 段にずらして重ならないように）
  ++ concat [ [ Color (rgba255 200 200 200 90) (Line [(x, by + bh), (x, y - 12)])
              , if x + textW nm 14 > envW env - 10 then sayR nm (x + 4) y 14 (gray 0.78) else say nm (x - 4) y 14 (gray 0.78) ]
            | (i, (nm, l)) <- zip [0 :: Int ..] examples, let x = xf l; y = by + bh + 44 + fromIntegral (i `mod` 7) * 16 ]
  -- 選んだ波長
  ++ [ Color (rgb255 255 230 120) (thickLine 3 (xs, by - 24) (xs, by + bh + 4))
     -- 数字
     , say ("lambda = " ++ lenLabel True lam ++ "  ->  f = c/lambda = " ++ freqLabel f ++ "  ->  E = hf = " ++ sci 3 eJ ++ " J = " ++ sci 3 (eJ / qE) ++ " eV") 40 y0 20 (rgb255 255 230 120)
     , say ("[" ++ bandOf lam ++ "]  it oscillates " ++ sci 3 f ++ " times per second.  " ++ nearestExample lam) 40 (y0 + 30) 16 (gray 0.92)
     , say "Above about 1.6-3.3 eV (visible light), a single photon can break molecular bonds (UV gives sunburn)." 40 (y0 + 52) 14 (gray 0.67)
     , say "A microwave-oven photon has only 1e-5 eV; the oven heats water with a huge number of them." 40 (y0 + 70) 14 (gray 0.67)
     , drawWave (40, y0 + 104, envW env - 80, envH env - y0 - 164) w
     , say "Click/drag the band: choose wavelength  Left/Right fine  1-7 jump to band  Space stop the wave" 20 34 17 (gray 0.92)
     , say "Log scale (1 tick = 10x). Left: longer wavelength (lower frequency), right: shorter (higher energy)." 20 60 14 (gray 0.67)
     , say "c = 2.998e8 m/s, h = 6.626e-34 J s" 20 78 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    bar@(bxD, byD, bwD, bhD) = barOf env
    by = f2 byD
    bh = f2 bhD
    xf = f2 . xOf bar
    lam = wLam w
    xs = xf lam
    f = cLight / lam
    eJ = hPl * f
    y0 = by + bh + 166

-- 下：E（たて）と B（ななめ = 奥行き）がゆれながら進む波。横はばに 1 つの大きさの例をならべる
drawWave :: (Float, Float, Float, Float) -> World -> Picture
drawWave (x, y, w, h) wd = Pictures
  [ Color c (polyline 2 ptsE)
  , Color (rgba255 110 200 255 200) (polyline 1.5 ptsB)
  , Color (gray 0.4) (Line [(x, cyMid), (x + w, cyMid)])
  , say "Electric field E (vertical)" x (y + 4) 14 c
  , say "Magnetic field B (depth, |B| = |E|/c)" (x + 270) (y + 4) 14 (rgb255 110 200 255)
  , say ("Width = " ++ lenLabel False span ++ "   compare: " ++ joinC [ s ++ " " ++ lenLabel False v | (s, v) <- sizes, v <= span, v >= span / 1000 ]) x (y + h + 16) 14 (gray 0.78) ]
  where
    lam = wLam wd
    span = 10 ^^ (ceiling (logBase 10 (lam * 3)) :: Int)  -- 画面の横はばが表す長さ（10 のべき）
    cycles = span / lam
    cyMid = y + h / 2
    a = min 60 (h * 0.35)
    n = min 1200 (max 200 (ceiling (cycles * 20))) :: Int
    ss = [ (u, f2 (sin (2 * pi * cycles * realToFrac u - wPhase wd))) | i <- [0 .. n], let u = fromIntegral i / fromIntegral n :: Float ]
    ptsE = [ (x + u * w, cyMid - a * s) | (u, s) <- ss ]
    ptsB = [ (x + u * w + 0.5 * a * s, cyMid + 0.5 * a * s) | (u, s) <- ss ]
    c = if bandOf lam == "Visible light" then rgbD (wavelengthRGB (lam * 1e9)) else rgb255 255 200 90
    joinC xs = case xs of
      [] -> ""
      _ -> foldr1 (\p q -> p ++ ", " ++ q) xs

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (pick w)
  EventMotion _ | envDown env -> pure (pick w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wLam = max lMin (wLam w / 1.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wLam = min lMax (wLam w * 1.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wMoving = not (wMoving w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '7' -> pure (case getAtM bands (fromEnum c - fromEnum '1') of
                                      Just b -> w { wLam = sqrt (bLo b * bHi b) }
                                      Nothing -> w)
  _ -> pure w
  where
    bar@(_, by, _, bh) = barOf env
    (mx, my) = mouseP5 env
    pick w0 = if realToFrac my > by - 30 && realToFrac my < by + bh + 30
                then w0 { wLam = clampv lMin lMax (lOf bar (realToFrac mx)) } else w0

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("c = f lambda: 2.45 GHz -> lambda = 12.2 cm", nearRel (cLight / 2.45e9) 0.1224 0.001)
  , ("a 500 nm photon = 2.48 eV", nearRel ((hPl * cLight) / 500e-9 / qE) 2.48 0.002)
  , ("hc = 1240 eV nm", nearRel ((hPl * cLight) / qE / 1e-9) 1239.8 0.001)
  , ("lambda -> f -> lambda comes back", nearRel (cLight / (cLight / 3.7e-7)) 3.7e-7 1e-12)
  , ("band names: 550 nm visible, 10 um infrared, 0.1 nm X-ray, 10 m radio"
    , bandOf 550e-9 == "Visible light" && bandOf 10e-6 == "Infrared" && bandOf 1e-10 == "X-ray" && bandOf 10 == "Radio (radio, TV)")
  , ("colours: 650 nm red, 530 nm green, 450 nm blue is strongest"
    , r1 > r2 && r1 > r3 && g2 > g1 && g2 > g3 && b3 > b1 && b3 > b2)
  , ("EM wave |B| = |E|/c: E = 1 V/m -> B = 3.34 nT", nearRel (1 / cLight) 3.336e-9 0.001)
  , ("log scale: x -> lambda -> x comes back", near (xOf bar (lOf bar (40 + 123.4))) (40 + 123.4) 1e-6) ]
  where
    (r1, r2, r3) = wavelengthRGB 650
    (b1, b2, b3) = wavelengthRGB 450
    (g1, g2, g3) = wavelengthRGB 530
    bar = (40, 150, 1200, 60)

-- ---------- やってみよう ----------
-- 1. 自分のスマホの Wi-Fi（2.4 GHz と 5 GHz）の波長を調べ、どちらが壁を回りこみやすいか考えよう
-- 2. 可視光のはしからはしまで（380〜780 nm）は、全体の目盛りのうちどれくらい？
-- 3. 光子 1 個のエネルギーが 1 eV をこえるのは、どの波長より短いとき？
-- 4. examples に自分の知っている電磁波（GPS、ラジコン、体温計 など）を足そう
-- 5. 12 のアンテナの波長を、ここで何の電磁波に当たるか調べよう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
bandOf :: Double -> String
bandOf l = case [ bName b | b <- bands, l >= bLo b && l <= bHi b ] of
  (n : _) -> n
  [] -> if l > lMax then "Radio" else "Gamma ray"

-- 長さを m・cm・mm・um・nm・pm・fm で（fine = 細かく）
lenLabel :: Bool -> Double -> String
lenLabel fine l = case [ (s, n) | (s, n) <- units, l >= s * 0.999 ] of
  ((s, n) : _) -> let v = l / s
                  in (if fine then (if v >= 100 then fmt 0 v else precJ 3 v) else showJ (round2 v)) ++ " " ++ n
  [] -> sci 3 l ++ " m"
  where
    units = [(1e3, "km"), (1, "m"), (1e-2, "cm"), (1e-3, "mm"), (1e-6, "um"), (1e-9, "nm"), (1e-12, "pm"), (1e-15, "fm")]
    -- 有効数字 2 桁に丸めた数（+v.toPrecision(2)）
    round2 v = let e = expo10 v - 1
               in if e < 0 then fromIntegral (round (v * 10 ^^ (-e)) :: Integer) / 10 ^^ (-e)
                  else fromIntegral (round (v / 10 ^^ e) :: Integer) * 10 ^^ e

freqLabel :: Double -> String
freqLabel f = case [ (s, n) | (s, n) <- units, f >= s ] of
  ((s, n) : _) -> precJ 3 (f / s) ++ " " ++ n
  [] -> sci 3 f ++ " Hz"
  where units = [(1e18, "EHz"), (1e15, "PHz"), (1e12, "THz"), (1e9, "GHz"), (1e6, "MHz"), (1e3, "kHz"), (1, "Hz")]

-- いちばん近い例（対数で 0.3 以内）
nearestExample :: Double -> String
nearestExample l = case foldl pickB Nothing examples of
  Just (_, (n, v)) -> "Near: " ++ n ++ " (" ++ lenLabel True v ++ ")"
  Nothing -> ""
  where
    pickB acc e@(_, v) =
      let d = abs (logBase 10 (v / l))
          bd = maybe 0.3 fst acc
      in if d < bd then Just (d, e) else acc

-- ---- 波長 [nm] → 見える色の RGB（よく使われる区分的な近似。両はしは暗くする）
wavelengthRGB :: Double -> (Double, Double, Double)
wavelengthRGB nm = (255 * r * k, 255 * g * k, 255 * b * k)
  where
    (r, g, b)
      | nm >= 380 && nm < 440 = ((440 - nm) / 60, 0, 1)
      | nm < 490 = (0, (nm - 440) / 50, 1)
      | nm < 510 = (0, 1, (510 - nm) / 20)
      | nm < 580 = ((nm - 510) / 70, 1, 0)
      | nm < 645 = (1, (645 - nm) / 65, 0)
      | nm <= 780 = (1, 0, 0)
      | otherwise = (0, 0, 0)
    k0 | nm < 420 = 0.3 + (0.7 * (nm - 380)) / 40
       | nm > 700 = 0.3 + (0.7 * (780 - nm)) / 80
       | otherwise = 1
    k = if nm < 380 || nm > 780 then 0 else k0

rgbD :: (Double, Double, Double) -> Color
rgbD (r, g, b) = makeColor (q r) (q g) (q b) 1
  where q v = f2 (clampv 0 255 v) / 255

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 10 の何乗か（|v| の桁）
expo10 :: Double -> Int
expo10 v
  | 10 ^^ (e0 + 1) <= a = e0 + 1
  | 10 ^^ e0 > a = e0 - 1
  | otherwise = e0
  where a = abs v
        e0 = floor (logBase 10 a) :: Int

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where e = expo10 v

-- 有効数字 n 桁（JavaScript の toPrecision）
precJ :: Int -> Double -> String
precJ n v
  | isNaN v || isInfinite v = "-"
  | v == 0 = fmt (n - 1) 0
  | e < -6 || e >= n = fmt (n - 1) (v / 10 ^^ e) ++ "e" ++ (if e >= 0 then "+" else "") ++ show e
  | otherwise = fmt (max 0 (n - 1 - e)) v
  where
    e1 = expo10 v
    -- 丸めて 10 になるときは桁が 1 つ上がる
    e = if abs (read (fmt (n - 1) (abs v / 10 ^^ e1)) :: Double) >= 10 then e1 + 1 else e1

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

getAtM :: [a] -> Int -> Maybe a
getAtM xs i = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

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

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)
