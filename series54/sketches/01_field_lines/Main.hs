-- ============================================
-- 54-01 電場と電位 ─ 電気力線と等電位線を「計算」して描く
-- 電場 E と電位 V は、どの点でも全部の電荷の重ね合わせ（クーロンの法則）：E = Σ kq r̂ / r²、V = Σ kq / r
-- 電気力線 = E の向きに少しずつ進んだ道（ルンゲ・クッタ法 RK4）。等電位線 = V が同じ所（格子でマーチング・スクエア）
-- 操作：ドラッグ = 電荷を動かす / 1〜4 = 並べ方 / A・N = ＋・− を追加 / F = 符号 / ↑↓ = 大きさ / C = 色 / L = 線
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kc :: Double
kc = 8.988e9                           -- クーロン定数 k = 1/(4πε0) [N·m²/C²]

pxM :: Double
pxM = 0.001                            -- ★ 1 px = 1 mm（画面の横 1280 px = 1.28 m）

soft :: Double
soft = 0.004                           -- ★ 電荷のすぐ近くで無限大にしない「やわらげ」[m]（自己テストでは 0）

linesPerUC :: Double
linesPerUC = 12                        -- ★ 1 μC あたりの電気力線の本数

cell :: Int
cell = 8                               -- 電位の色地図の 1 マス [px]

levelsKV :: [Double]
levelsKV = [5, 10, 20, 40, 80, 160]    -- 等電位線を引く電位 [kV]（と、そのマイナスと 0）

-- 電荷 1 つ：位置 [px]（p5.js と同じ、左上が原点）、電荷 [μC]
data Charge = Charge { cX, cY, cQ :: !Double }

data World = World
  { wW, wH     :: !Double              -- 画面の大きさ
  , wCs        :: [Charge]
  , wGrab      :: !Int
  , wShowMap   :: !Bool
  , wShowLines :: !Bool
  , wDirty     :: !Bool
  , wPreset    :: !Int
  , wMap       :: Picture              -- 電位の色地図（作り直すのは電荷が動いたときだけ）
  , wLay       :: Picture              -- 等電位線と電気力線
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-01 Electric field and potential" (world0 1280 800))
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 (realToFrac (envW env)) (realToFrac (envH env))) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wDirty w then rebuild w else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Double -> Double -> World
world0 wd ht = setPreset 1 (World wd ht [] (-1) True True True 1 Blank Blank [])

nxOf, nyOf :: World -> Int
nxOf w = ceiling (wW w / fromIntegral cell) + 1
nyOf w = ceiling (wH w / fromIntegral cell) + 1

setPreset :: Int -> World -> World
setPreset n w = w { wPreset = n, wDirty = True, wCs = cs }
  where
    cx = wW w / 2
    cy = wH w / 2 + 30
    cs = case n of
      1 -> [Charge (cx - 120) cy 1, Charge (cx + 120) cy (-1)]                                  -- 双極子
      2 -> [Charge (cx - 120) cy 1, Charge (cx + 120) cy 1]                                     -- 同じ符号 2 つ
      3 -> [Charge (cx - 110) (cy - 110) 1, Charge (cx + 110) (cy - 110) (-1), Charge (cx + 110) (cy + 110) 1, Charge (cx - 110) (cy + 110) (-1)]   -- 四重極
      _ -> [Charge (cx - 160) (cy + 60) 2, Charge (cx + 140) (cy - 80) (-1), Charge (cx + 100) (cy + 140) (-0.5)]

-- 色地図・等電位線・電気力線を作り直す（電荷が動いたときだけ）
rebuild :: World -> World
rebuild w = w { wDirty = False, wMap = mapPic, wLay = lay }
  where
    nx = nxOf w
    ny = nyOf w
    cs = wCs w
    vgrid = U.generate (nx * ny) (\k -> let (j, i) = k `divMod` nx in potAt cs (fromIntegral (i * cell)) (fromIntegral (j * cell)) soft)
    col v = let s = signum v * clampv 0 1 (logBase 10 (1 + abs v / 2000) / 2.3)   -- 符号つきの対数で 0〜1
            in (byte (20 + (if s > 0 then 200 * s else 0)), byte (26 + 40 * abs s), byte (36 + (if s < 0 then -210 * s else 0)))
    c = fromIntegral cell :: Float
    mapPic = at (-c / 2 + fromIntegral nx * c / 2, -c / 2 + fromIntegral ny * c / 2)
               (Scale 1 (-1) (pixelImage nx ny c (\i j -> col (vgrid U.! (j * nx + i)))))
    lay | not (wShowLines w) = Blank
        | otherwise = Pictures
            [ Color (rgba255 255 220 90 190) (Pictures [ Line [(f2 x1 * c, f2 y1 * c), (f2 x2 * c, f2 y2 * c)]
                                                       | l <- levelsKV ++ map negate levelsKV ++ [0]
                                                       , (x1, y1, x2, y2) <- marchingSquares vgrid nx ny (l * 1000) ])
            , Color (rgba255 240 245 255 210) (Pictures [ Line [ (f2 x, f2 y) | (x, y) <- path ] | path <- allFieldLines (wW w, wH w) cs ]) ]

byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- 多いほうの符号から出発（＋は E の向き、− は逆向きにたどる）
allFieldLines :: (Double, Double) -> [Charge] -> [[(Double, Double)]]
allFieldLines dm cs =
  [ path
  | c <- cs, signum (cQ c) == dir
  , let n = max 4 (jsRound (linesPerUC * abs (cQ c)))
  , k <- [0 .. n - 1]
  , let a = (2 * pi * (fromIntegral k + 0.5)) / fromIntegral n
        path = traceLine dm cs (cX c + 10 * cos a) (cY c + 10 * sin a) dir 4 700
  , length (take 2 path) > 1 ]
  where
    pos = sum [ cQ c | c <- cs, cQ c > 0 ]
    neg = sum [ -(cQ c) | c <- cs, cQ c < 0 ]
    dir = if pos >= neg then 1 else -1

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ if wShowMap w then wMap w else Blank
  , wLay w ]
  ++ concat [ [ Color (if cQ c > 0 then rgb255 240 90 70 else rgb255 70 150 250) (circleD x y (14 + 6 * f2 (sqrt (abs (cQ c)))))
              , sayC (if cQ c > 0 then "+" else "-") x (y + 6) 16 white
              , say ((if cQ c > 0 then "+" else "") ++ fmt 1 (cQ c) ++ " uC") (x + 14) (y - 12) 14 (gray 0.9) ]
            | c <- wCs w, let x = f2 (cX c); y = f2 (cY c) ]
  ++ [ if em > 0 then Color (rgb255 255 230 120) (arrowTo 2 9 (mx, my) (mx + f2 (ex / em) * 40, my + f2 (ey / em) * 40)) else Blank
     , Color (rgba255 0 0 0 150) (rect (10 + bw / 2) (10 + 63) bw 126)
     , say "1 dipole  2 like  3 quadrupole  4 three | Drag move  A/N add  F sign  Up/Down q  C color  L lines" 20 34 16 (gray 0.92)
     , say ("At the mouse: |E| = " ++ sci 3 em ++ " N/C (= V/m)   V = " ++ sci 3 (vm / 1000) ++ " kV") 20 60 16 (rgb255 255 230 120)
     , say "Charges in uC, 1 px = 1 mm (slice through the 3D field, in the plane of the charges). Red = high V, blue = low." 20 84 14 (gray 0.67)
     , say ("White = field lines, yellow = equipotentials (" ++ joinC (map showJ levelsKV) ++ " kV, minus those, and 0)") 20 106 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    (mx, my) = mouseP5 env
    (ex, ey) = fieldAt (wCs w) (realToFrac mx) (realToFrac my) soft
    em = hyp ex ey
    vm = potAt (wCs w) (realToFrac mx) (realToFrac my) soft
    bw = min (envW env - 20) 1110
    joinC xs = case xs of
      [] -> ""
      (y : ys) -> y ++ concatMap (", " ++) ys

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wGrab = pickMin (-1) [ (d, i) | (i, c) <- zip [0 ..] (wCs w), let d = hyp (cX c - mx) (cY c - my), d < 22 ] }
  EventMotion _ | envDown env && wGrab w >= 0 ->
    pure w { wCs = updateAt (wGrab w) (\c -> c { cX = clampv 5 (wW w - 5) mx, cY = clampv 5 (wH w - 5) my }) (wCs w), wDirty = True }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wGrab = -1 }
  EventKey k Down _ _ -> pure (key k w) { wDirty = True }
  _ -> pure w
  where
    (mx0, my0) = mouseP5 env
    mx = realToFrac mx0
    my = realToFrac my0
    key k w0 =
      let w1 = case k of
            Char c | c >= '1' && c <= '4' -> setPreset (fromEnum c - fromEnum '0') w0
                   | c `elem` "aAnN" && length (wCs w0) < 8 ->
                       let x = clampv 20 (wW w0 - 20) mx
                           y = clampv 120 (wH w0 - 20) my
                       in if all (\o -> hyp (cX o - x) (cY o - y) > 20) (wCs w0)
                            then w0 { wCs = wCs w0 ++ [Charge x y (if c `elem` "aA" then 1 else -1)] } else w0
            _ -> w0
          i = pickMin (-1) [ (hyp (cX c - mx) (cY c - my), j) | (j, c) <- zip [0 ..] (wCs w1) ]
          upd f = w1 { wCs = updateAt i f (wCs w1) }
          w2 = if i < 0 then w1 else case k of
            Char c | c `elem` "fF" -> upd (\o -> o { cQ = cQ o * (-1) })
            SpecialKey KeyUp -> upd (\o -> o { cQ = signum (cQ o) * min 4 (abs (cQ o) + 0.5) })
            SpecialKey KeyDown -> upd (\o -> o { cQ = signum (cQ o) * max 0.5 (abs (cQ o) - 0.5) })
            _ -> w1
      in case k of
           Char c | c `elem` "cC" -> w2 { wShowMap = not (wShowMap w2) }
                  | c `elem` "lL" -> w2 { wShowLines = not (wShowLines w2) }
           _ -> w2

-- いちばん小さい d の番号（同じなら先のもの）
pickMin :: Int -> [(Double, Int)] -> Int
pickMin d xs = snd (foldl' (\(bd, bi) (e, i) -> if e < bd then (e, i) else (bd, bi)) (1e9, d) xs)

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("E on the dipole axis matches kq(1/(x-a)^2 - 1/(x+a)^2)", nearRel (fst (fieldAt dip x 0 0)) exact 1e-12)
  , ("far away E ~ 2kp/x^3 (dipole approximation, within 1%)", nearRel (fst (fieldAt dip xf 0 0)) ((2 * kc * p) / (xf * pxM) ** 3) 0.01)
  , ("E = -grad V (E from potential differences agrees)", all gradOk [(50, -40), (200, 200), (-150, 30)])
  , ("V = 0 on the perpendicular bisector of the dipole", all (\y -> abs (potAt dip 0 y 0) < 1e-6) [0, 50, 300])
  , ("a field line from + (RK4) reaches -", hyp (fst end + a) (snd end) < 14)
  , ("marching squares: the r^2 = 144 contour is a circle of radius 12 (within 1%)"
    , length segs > 20 && all (\(x1, y1, _, _) -> abs (hyp (x1 - 20) (y1 - 20) - 12) < 0.12 * 1.5) segs)
  , ("Gauss's law (3D): flux through a sphere = q/eps0, and 0 for a dipole"
    , nearRel (sphereFlux [Charge 30 10 1] 200) ((q * 1e-6) * 4 * pi * kc) 0.005 && abs (sphereFlux dip 200) < 0.005 * (q * 1e-6) * 4 * pi * kc) ]
  where
    a = 50
    q = 1
    dip = [Charge a 0 q, Charge (-a) 0 (-q)]
    x = 300
    exact = kc * q * 1e-6 * (1 / sqM ((x - a) * pxM) - 1 / sqM ((x + a) * pxM))
    xf = 20 * 2 * a
    p = q * 1e-6 * 2 * a * pxM
    cs = [Charge 0 0 1, Charge 140 60 (-2), Charge (-90) 120 0.5]
    gradOk (px, py) =
      let h = 0.01
          (ex, ey) = fieldAt cs px py 0
          gx = -(potAt cs (px + h) py 0 - potAt cs (px - h) py 0) / (2 * h * pxM)
          gy = -(potAt cs px (py + h) 0 - potAt cs px (py - h) 0) / (2 * h * pxM)
      in nearRel gx ex 1e-5 && nearRel gy ey 1e-5
    path = traceLine (1280, 800) dip (a - 10) 2 1 2 3000
    end = last' (0, 0) path
    nx = 41
    g = U.generate (nx * nx) (\k -> let (j, i) = k `divMod` nx in fromIntegral ((i - 20) * (i - 20) + (j - 20) * (j - 20)))
    segs = marchingSquares g nx nx 144
    sqM v = v * v
    last' d xs = foldl' (\_ y -> y) d xs

-- 原点を中心とする半径 r [px] の球面で E·n を数値積分（緯度 60 × 経度 120 のます目）。電荷は z = 0 の平面
sphereFlux :: [Charge] -> Double -> Double
sphereFlux cs r = foldl' (+) 0 [ term a b | a <- [0 .. nt - 1], b <- [0 .. np - 1] ]
  where
    nt = 60 :: Int
    np = 120 :: Int
    rr = r * pxM
    term a b =
      let th = (pi * (fromIntegral a + 0.5)) / fromIntegral nt
          ph = (2 * pi * (fromIntegral b + 0.5)) / fromIntegral np
          nx = sin th * cos ph
          ny = sin th * sin ph
          nz = cos th
          (ex, ey, ez) = foldl' (\(sx, sy, sz) c ->
                           let dx = (r * nx - cX c) * pxM
                               dy = (r * ny - cY c) * pxM
                               dz = r * nz * pxM
                               d = sqrt (dx * dx + dy * dy + dz * dz)
                               f = (kc * cQ c * 1e-6) / (d * d * d)
                           in (sx + f * dx, sy + f * dy, sz + f * dz)) (0, 0, 0) cs
      in (ex * nx + ey * ny + ez * nz) * rr * rr * sin th * (pi / fromIntegral nt) * ((2 * pi) / fromIntegral np)

-- ---------- やってみよう ----------
-- 1. 1 の双極子で、＋の電荷を ↑ で 2 μC にすると、− に入らない電気力線はどこへ行く？
-- 2. 2（同じ符号）のまん中の点で E はいくつ？ そこを通る等電位線はどんな形？
-- 3. 電気力線と等電位線は、どこでも直角に交わっている？ どうしてだろう
-- 4. linesPerUC を変えて、線の混み具合 = 電場の強さ になっていることを確かめよう
-- 5. A と N で電荷を 6 個並べて、好きな「場の模様」を作ろう（C で色を消すと線画になる）


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 点電荷の電場と電位（電荷の位置は px、q は μC、結果は N/C と V）
fieldAt :: [Charge] -> Double -> Double -> Double -> (Double, Double)
fieldAt cs x y sft = foldl' add (0, 0) cs
  where
    add (ex, ey) c =
      let dx = (x - cX c) * pxM
          dy = (y - cY c) * pxM
          r2 = dx * dx + dy * dy + sft * sft
          f = (kc * cQ c * 1e-6) / (r2 * sqrt r2)
      in if r2 == 0 then (ex, ey) else (ex + f * dx, ey + f * dy)

potAt :: [Charge] -> Double -> Double -> Double -> Double
potAt cs x y sft = foldl' add 0 cs
  where
    add v c =
      let dx = (x - cX c) * pxM
          dy = (y - cY c) * pxM
          r = sqrt (dx * dx + dy * dy + sft * sft)
      in if r > 0 then v + (kc * cQ c * 1e-6) / r else v

-- ---- 電気力線：単位ベクトル E/|E| の向きに RK4 で h [px] ずつ進む。dir = −1 なら逆向き。電荷に着くか画面の外で止まる
traceLine :: (Double, Double) -> [Charge] -> Double -> Double -> Double -> Double -> Int -> [(Double, Double)]
traceLine (wd, ht) cs x0 y0 dir h maxN = (x0, y0) : go x0 y0 0
  where
    u px py = let (ex, ey) = fieldAt cs px py soft
                  m = hyp ex ey
              in if m > 0 then ((dir * ex) / m, (dir * ey) / m) else (0, 0)
    go x y n
      | n >= maxN = []
      | isNaN x' || isInfinite x' || isNaN y' || isInfinite y' = []
      | x' < -200 || y' < -200 || x' > wd + 200 || y' > ht + 200 = [(x', y')]
      | any (\c -> signum (cQ c) == -dir && hyp (cX c - x') (cY c - y') < 6) cs = [(x', y')]
      | otherwise = (x', y') : go x' y' (n + 1)
      where
        (k1x, k1y) = u x y
        (k2x, k2y) = u (x + (h / 2) * k1x) (y + (h / 2) * k1y)
        (k3x, k3y) = u (x + (h / 2) * k2x) (y + (h / 2) * k2y)
        (k4x, k4y) = u (x + h * k3x) (y + h * k3y)
        x' = x + (h / 6) * (k1x + 2 * k2x + 2 * k3x + k4x)
        y' = y + (h / 6) * (k1y + 2 * k2y + 2 * k3y + k4y)

-- ---- マーチング・スクエア：格子の値 g（nx × ny）で、値 = level の線分を (x1, y1, x2, y2)（格子の単位）で返す
marchingSquares :: U.Vector Double -> Int -> Int -> Double -> [(Double, Double, Double, Double)]
marchingSquares g nx ny level = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
  where
    at' a b ia ja ib jb = let t = (level - a) / (b - a) in (ia + t * (ib - ia), ja + t * (jb - ja))
    cellSegs i j =
      let v0 = g U.! (j * nx + i)
          v1 = g U.! (j * nx + i + 1)
          v2 = g U.! ((j + 1) * nx + i + 1)
          v3 = g U.! ((j + 1) * nx + i)
          fi = fromIntegral i
          fj = fromIntegral j
          pts = [ at' v0 v1 fi fj (fi + 1) fj | (v0 > level) /= (v1 > level) ]
             ++ [ at' v1 v2 (fi + 1) fj (fi + 1) (fj + 1) | (v1 > level) /= (v2 > level) ]
             ++ [ at' v2 v3 (fi + 1) (fj + 1) fi (fj + 1) | (v2 > level) /= (v3 > level) ]
             ++ [ at' v3 v0 fi (fj + 1) fi fj | (v3 > level) /= (v0 > level) ]
      in case pts of
           [(a, b), (c, d)] -> [(a, b, c, d)]
           [(a, b), (c, d), (e, f), (gg, hh)] -> [(a, b, c, d), (e, f, gg, hh)]
           _ -> []

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

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

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where
    e0 = floor (logBase 10 (abs v)) :: Int
    e | 10 ^^ (e0 + 1) <= abs v = e0 + 1
      | 10 ^^ e0 > abs v = e0 - 1
      | otherwise = e0

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

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

-- 線分（太さ w。1 以下なら細い線）
seg :: Float -> Point -> Point -> Picture
seg w a b = if w <= 1 then Line [a, b] else thickLine w a b

-- (x1, y1) → (x2, y2) の矢印。太さ w、hs = 矢じりの大きさ
arrowTo :: Float -> Float -> Point -> Point -> Picture
arrowTo w hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Pictures [ seg w (x1, y1) (x2, y2)
                         , seg w (x2, y2) (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux)
                         , seg w (x2, y2) (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

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
