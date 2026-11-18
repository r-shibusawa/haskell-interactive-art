-- ============================================
-- 54-09 電磁誘導 ─ 磁石を動かすとコイルに電圧が生まれる
-- コイルを通る磁束 Φ が変わると、誘導起電力 V = −N dΦ/dt（ファラデーの法則）。マイナス = 変化をじゃまする向き（レンツの法則）
-- Φ は「ベクトルポテンシャル A をコイルにそって 1 周足す」∮A·dl で計算（磁石の中心がコイルを通りぬけても大丈夫）
-- 操作：磁石をドラッグ = 出し入れ / スペース = 自動で往復 / ↑↓ = 巻き数 / F = 磁石の向き / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mu0 :: Double
mu0 = 4 * pi * 1e-7

pxm :: Double
pxm = 2000                             -- ★ 1 m = 2000 px（1 cm = 20 px）

mdip :: Double
mdip = 1.0                             -- ★ 磁石の強さ（磁気モーメント）[A·m²]：小さなネオジム磁石くらい

aCoil :: Double
aCoil = 0.03                           -- ★ コイルの半径 [m]

rCoil :: Double
rCoil = 10                             -- コイルの抵抗 [Ω]（誘導電流 = 起電力 / R）

nseg :: Int
nseg = 96                              -- 1 周を何本に分けて足すか

data World = World
  { wTurns   :: !Int
  , wDir     :: !Double                -- 磁石の向き（+1 = N 極が右）
  , wMag     :: !(Double, Double)      -- 磁石の位置 [m]
  , wTarget  :: !(Double, Double)      -- 磁石が向かう位置
  , wDrag    :: !Bool
  , wAuto    :: !Bool
  , wTAuto   :: !Double
  , wPhiPrev :: !(Maybe Double)
  , wEmf     :: !Double
  , wPeak    :: !Double
  , wHPhi    :: !(Seq.Seq Double)
  , wHEmf    :: !(Seq.Seq Double)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-09 Electromagnetic induction" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 100 1 (-0.18, 0) (-0.18, 0) False False 0 Nothing 0 0 Seq.empty Seq.empty []

center :: Env -> (Double, Double)
center env = (realToFrac (envW env) * 0.5, realToFrac (envH env) * 0.36)

step :: Float -> World -> World
step dtF w0 = w2 { wPhiPrev = Just phi, wEmf = emf, wPeak = peak
                 , wHPhi = pushCap 360 (phi * 1000) (wHPhi w2), wHEmf = pushCap 360 (emf * 1000) (wHEmf w2) }
  where
    dt = clampv 0.005 0.05 (realToFrac dtF)
    w1 = if wAuto w0 then let ta = wTAuto w0 + dt in w0 { wTAuto = ta, wTarget = (-0.16 * cos (ta * 2.4), 0) } else w0
    (mx, my) = wMag w1
    (tx, ty) = wTarget w1
    w2 = w1 { wMag = (mx + (tx - mx) * 0.35, my + (ty - my) * 0.35) }     -- マウスの動きを少しなめらかに
    phi = fromIntegral (wTurns w2) * uncurry fluxLoop (wMag w2) (wDir w2)    -- N 巻き全部の磁束
    emf = case wPhiPrev w2 of
      Just pp -> 0.6 * wEmf w2 + 0.4 * (-(phi - pp) / dt)                  -- V = −dΦ/dt（少しならす）
      Nothing -> wEmf w2
    peak = max (wPeak w2 * 0.995) (abs emf)

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ drawCoil env w (wEmf w), drawMagnet env w
  , plotFrame g1 "Flux N Phi [mWb] (whole coil)" (-phiMax * 1.1) (phiMax * 1.1)
  , Color (rgb255 110 210 255) (plotTrace 2 g1 (toList (wHPhi w)) (-phiMax * 1.1) (phiMax * 1.1) 360)
  , plotFrame g2 "Induced emf V = -N dPhi/dt [mV]" (-emax) emax
  , Color (rgb255 255 200 90) (plotTrace 2 g2 (toList (wHEmf w)) (-emax) emax 360) ]
  ++ [ say s tx (ty + 26 * fromIntegral k) 15 (gray 0.92) | (k, s) <- zip [0 :: Int ..] nums ]
  ++ [ say s tx (ty + 26 * 5 + 6 + 18 * fromIntegral k) 14 (gray 0.67) | (k, s) <- zip [0 :: Int ..] notes ]
  ++ [ say ("Drag the magnet in and out  Space auto back-and-forth (" ++ (if wAuto w then "ON" else "OFF") ++ ")  Up/Down turns  F flip magnet  R reset") 20 34 17 (gray 0.92)
     , say ("Magnet = point magnetic dipole m = " ++ showJ mdip ++ " A m^2 (red = N pole).") 20 58 14 (gray 0.67)
     , say ("Phi = sum of A.dl at " ++ show nseg ++ " points around the coil, with A = (mu0/4pi) m x r / r^3.") 20 76 14 (gray 0.67)
     , say "Yellow (.)(x) = induced current direction (top/bottom wires), green = its field (opposes the change)." 20 94 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    phiMax = fromIntegral (wTurns w) * fluxLoop 0 0 1 * 1000
    emax = max 50 (wPeak w * 1000 * 1.1)
    g1 = (60, envH env * 0.64, envW env * 0.55, envH env * 0.12)
    g2 = (60, envH env * 0.82, envW env * 0.55, envH env * 0.12)
    tx = envW env * 0.66
    ty = envH env * 0.66
    phi = maybe 0 id (wPhiPrev w)
    emf = wEmf w
    nums = [ "Turns N = " ++ show (wTurns w) ++ "  radius " ++ showJ (aCoil * 100) ++ " cm"
           , "Flux N Phi = " ++ sci 3 (phi * 1000) ++ " mWb"
           , "emf V = " ++ sci 3 (emf * 1000) ++ " mV"
           , "Current I = V/R = " ++ sci 3 ((emf / rCoil) * 1000) ++ " mA (R = " ++ showJ rCoil ++ " Ohm)"
           , "Largest |V| so far ~ " ++ sci 3 (wPeak w * 1000) ++ " mV" ]
    notes = [ "Faster motion -> bigger dPhi/dt -> bigger voltage.", "Stop the magnet and V = 0", "(even with the magnet inside!)" ]

drawCoil :: Env -> World -> Double -> Picture
drawCoil env w v = Pictures $
  [ Color (rgb255 200 130 70) (thickPath 3 [ (cx + fromIntegral k * 7 + rx * cos a, cy + ry * sin a) | i <- [0 .. 48 :: Int], let a = tau * fromIntegral i / 48 ]) | k <- [-4 .. 4 :: Int] ]
  -- 誘導電流：起電力 > 0 は 右から見て反時計まわり（右ねじで +x 向きの磁場）
  ++ (if mag2 > 0.02
        then [ sym cx (cy - ry - 22) (s > 0), sym cx (cy + ry + 22) (s < 0)     -- 上と下の導線：⊙ 手前へ ⊗ 奥へ
             , Color (rgba255 120 255 170 al) (arrowTo 4 12 (cx - 30 * sf, cy) (cx + 30 * sf, cy)) ]
        else [])
  ++ [ say ("coil (" ++ show (wTurns w) ++ " turns)") (cx - 50) (cy + ry + 46) 14 (gray 0.78)
     -- 検流計
     , Color (gray 0.59) (Line [ (gx + 60 * cos a, gy + 60 * sin a) | i <- [0 .. 30 :: Int], let a = pi + 0.3 + (pi - 0.6) * fromIntegral i / 30 ])
     , Color (rgb255 255 230 120) (thickLine 3 (gx, gy) (gx + 55 * sin ang, gy - 55 * cos ang))
     , say "galvanometer" (gx - 50) (gy + 22) 14 (gray 0.78) ]
  where
    (cxD, cyD) = center env
    cx = f2 cxD
    cy = f2 cyD
    rx = f2 (aCoil * pxm * 0.35)
    ry = f2 (aCoil * pxm)
    s = signum v
    sf = f2 s
    mag2 = clampv 0 1 (abs v / 0.05)
    al = round (80 + 175 * mag2) :: Int
    yc = rgba255 255 230 120 al
    sym x y out = Color yc (Pictures ([ at (x, y) (ring 9 3) ]
                                      ++ if out then [ circleD x y 6 ]
                                         else [ thickLine 3 (x - 6, y - 6) (x + 6, y + 6), thickLine 3 (x - 6, y + 6) (x + 6, y - 6) ]))
    gx = cx + 260
    gy = cy + 40
    ang = f2 (clampv (-1.2) 1.2 ((v / 0.05) * 0.8))

drawMagnet :: Env -> World -> Picture
drawMagnet env w = Pictures
  [ Color (if wDir w > 0 then blueC else redC) (rectTL (x - l / 2) (y - h / 2) (l / 2) h)
  , Color (if wDir w > 0 then redC else blueC) (rectTL x (y - h / 2) (l / 2) h)
  , sayC (if wDir w > 0 then "S" else "N") (x - l / 4) (y + 8) 18 white
  , sayC (if wDir w > 0 then "N" else "S") (x + l / 4) (y + 8) 18 white ]
  where
    (cx, cy) = center env
    (mx, my) = wMag w
    x = f2 (cx + mx * pxm)
    y = f2 (cy - my * pxm)
    l = 120
    h = 34
    blueC = rgb255 70 140 240
    redC = rgb255 230 80 70

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let d = abs (px - (cx + fst (wMag w) * pxm)) < 80 && abs (py - (cy - snd (wMag w) * pxm)) < 40
    in w { wDrag = d, wAuto = if d then False else wAuto w }
  EventMotion _ | envDown env && wDrag w ->
    w { wTarget = (clampv (-0.3) 0.3 ((px - cx) / pxm), clampv (-0.1) 0.1 ((cy - py) / pxm)) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventKey k Down _ _ -> (key k) { wPhiPrev = Nothing }
  _ -> w
  where
    (cx, cy) = center env
    (mx0, my0) = mouseP5 env
    px = realToFrac mx0
    py = realToFrac my0
    key k = case k of
      SpecialKey KeySpace -> w { wAuto = not (wAuto w), wTAuto = acos (clampv (-1) 1 (-(fst (wMag w)) / 0.16)) / 2.4 }
      SpecialKey KeyUp -> w { wTurns = min 500 (wTurns w + 50) }
      SpecialKey KeyDown -> w { wTurns = max 50 (wTurns w - 50) }
      Char c | c `elem` "fF" -> w { wDir = -(wDir w) }
             | c `elem` "rR" -> w { wTurns = 100, wDir = 1, wAuto = False, wMag = (-0.18, 0), wTarget = (-0.18, 0), wPeak = 0 }
      _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("on the axis: loop int A.dl = mu0 m a^2/(2(a^2 + x^2)^(3/2)) (within 1e-4)", all (\x -> nearRel (fluxLoop x 0 1) (onAxis x) 1e-4) [0, 0.02, 0.1])
  , ("off the axis too: loop int A.dl = integral of B over the surface (within 0.5%)", nearRel (fluxLoop xs ys 1) sB 0.005)
  , ("symmetric: Phi(x) = Phi(-x)", nearRel (fluxLoop (-0.07) 0.01 1) (fluxLoop 0.07 0.01 1) 1e-9)
  , ("time integral of emf = -N dPhi (within 1e-4)", nearRel s1 (-dPhi1) 1e-4)
  , ("twice the speed -> twice the emf peak (same integral)", nearRel (mx2 / mx1) 2 1e-6 && nearRel s2 s1 1e-4)
  , ("Lenz: bringing the N pole closer raises Phi, the emf is negative (opposing field)", fluxLoop (-0.05) 0 1 > fluxLoop (-0.06) 0 1 && s1 < 0)
  , ("far away Phi ~ 0 (below 1e-4 of the center value)", fluxLoop (-1) 0 1 < 1e-4 * fluxLoop 0 0 1) ]
  where
    a = aCoil
    onAxis x = (mu0 * mdip * a * a) / (2 * (a * a + x * x) ** 1.5)
    -- 軸からはずれた所：面の上で B を積分した値と比べる（ストークスの定理）
    xs = 0.05
    ys = 0.04
    nr = 200 :: Int
    np = 200 :: Int
    sB = foldl' (\acc (i, j) -> let r = (a * (fromIntegral i + 0.5)) / fromIntegral nr
                                    ph = (2 * pi * (fromIntegral j + 0.5)) / fromIntegral np
                                    (bx, _, _) = dipoleB (0, r * cos ph, r * sin ph) (xs, ys, 0) 1
                                in acc + bx * r * (a / fromIntegral nr) * ((2 * pi) / fromIntegral np))
                0 [ (i, j) | i <- [0 .. nr - 1], j <- [0 .. np - 1] ]
    (s1, mx1, dPhi1) = pass 0.5
    (s2, mx2, _) = pass 1.0
    -- 一定の速さで近づける（シンプソンの公式で積分）
    pass v = (s, mx, dphi)
      where
        nT = 100
        x0 = -0.25
        x1 = 0.0
        tt = (x1 - x0) / v
        n = 2000 :: Int
        h = tt / fromIntegral n
        e t = let x = x0 + v * t; d = 1e-6 in (-nT * (fluxLoop (x + d) 0 1 - fluxLoop (x - d) 0 1) / (2 * d)) * v
        (s, mx) = foldl' (\(!sa, !ma) k -> let wt = if k == 0 || k == n then 1 else if odd k then 4 else 2
                                               ev = e (fromIntegral k * h)
                                           in (sa + (wt * ev * h) / 3, max ma (abs ev))) (0, 0) [0 .. n]
        dphi = nT * (fluxLoop x1 0 1 - fluxLoop x0 0 1)

-- ---------- やってみよう ----------
-- 1. 磁石をゆっくり・速く入れて、起電力のピークを比べよう。磁束のグラフの「傾き」との関係は？
-- 2. 磁石をコイルの中で止めると、Φ は大きいのに起電力は 0。なぜ？
-- 3. 入れるときと出すときで、起電力の符号はどうなる？ F で磁石を反対にすると？
-- 4. ↑ で巻き数を 2 倍にすると起電力は？（IH 調理器や変圧器はたくさん巻いている）
-- 5. 磁石を軸から上にずらして通すと、Φ のグラフの形はどう変わる？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
type V3d = (Double, Double, Double)

-- ---- 磁気双極子（位置 c、向き ±x、強さ mdip）のベクトルポテンシャル A = (μ0/4π) m × r / r³
dipoleA :: V3d -> V3d -> Double -> V3d
dipoleA (px, py, pz) (cx, cy, cz) dir = (0, -f * rz, f * ry)   -- m = (dir·mdip, 0, 0) なので m × r = (0, −m·rz, m·ry)
  where
    rx = px - cx
    ry = py - cy
    rz = pz - cz
    r = max 0.004 (sqrt (rx * rx + ry * ry + rz * rz))
    f = (1e-7 * mdip * dir) / (r * r * r)

-- ---- 磁気双極子の磁場 B = (μ0/4π)(3(m·r̂)r̂ − m)/r³（自己テスト用）
dipoleB :: V3d -> V3d -> Double -> V3d
dipoleB (px, py, pz) (cx, cy, cz) dir = (f * (3 * mr * (rx / r) - m), f * 3 * mr * (ry / r), f * 3 * mr * (rz / r))
  where
    rx = px - cx
    ry = py - cy
    rz = pz - cz
    r = sqrt (rx * rx + ry * ry + rz * rz)
    m = mdip * dir
    mr = (m * rx) / r
    f = 1e-7 / (r * r * r)

-- ---- コイル 1 巻き（x = 0 の面、半径 aCoil）を通る磁束 Φ = ∮A·dl（磁石は (mx, my, 0)）
fluxLoop :: Double -> Double -> Double -> Double
fluxLoop mx my dir = foldl' add 0 [0 .. nseg - 1]
  where
    add s k =
      let p0 = (2 * pi * fromIntegral k) / fromIntegral nseg
          p1 = (2 * pi * fromIntegral (k + 1)) / fromIntegral nseg
          pm = (p0 + p1) / 2
          dly = -aCoil * (p1 - p0) * sin pm                   -- 円にそった接線 × 弧の長さ
          dlz = aCoil * (p1 - p0) * cos pm
          (_, ay, az) = dipoleA (0, aCoil * cos pm, aCoil * sin pm) (mx, my, 0) dir
      in s + (ay * dly + az * dlz)

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

-- リストの i 番目（なければ d / Nothing）
getAt :: a -> [a] -> Int -> a
getAt d xs i = maybe d id (getAtM xs i)

getAtM :: [a] -> Int -> Maybe a
getAtM xs i = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

-- n 回くり返した結果（iterate と組み合わせる。途中を正格に評価する）
(!!!) :: [a] -> Int -> a
xs !!! n = go xs n
  where go (y : ys) k | k <= 0 = y
                      | otherwise = y `seq` go ys (k - 1)
        go [] _ = error "empty"
infixl 9 !!!

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

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

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

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

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

-- ---- グラフの道具：枠 (x, y, w, h)、lo〜hi の範囲
type G = (Float, Float, Float, Float)

plotFrame :: G -> String -> Double -> Double -> Picture
plotFrame (x, y, w, h) title lo hi = Pictures $
  [ Color (rgb255 70 80 100) (frameTL 1 x y w h) ]
  ++ [ Color (rgba255 70 80 100 160) (Line [(x, y0), (x + w, y0)]) | lo < 0 && hi > 0, let y0 = y + h * f2 (hi / (hi - lo)) ]
  ++ [ say title (x + 4) (y - 6) 14 (gray 0.78)
     , say (sci 2 hi) (x + w + 4) (y + 12) 14 (gray 0.55)
     , say (sci 2 lo) (x + w + 4) (y + h) 14 (gray 0.55) ]

-- 数の列を左から右へ（cap 個ぶんの幅）
plotTrace :: Float -> G -> [Double] -> Double -> Double -> Int -> Picture
plotTrace wt (x, y, w, h) arr lo hi cap =
  polyline wt [ (x + fromIntegral i / fromIntegral (cap - 1) * w, y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h) | (i, v) <- zip [0 :: Int ..] arr ]

-- 長さ cap までの履歴（古いものから消す）
pushCap :: Int -> a -> Seq.Seq a -> Seq.Seq a
pushCap cap v s = let s1 = s Seq.|> v in if Seq.length s1 > cap then Seq.drop (Seq.length s1 - cap) s1 else s1

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h
