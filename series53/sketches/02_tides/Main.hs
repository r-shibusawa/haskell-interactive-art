-- ============================================
-- 53-02 潮汐 ─ 月の引力の「差」が、海を 2 方向にふくらませる
-- 地球の各点の加速度から、地球の中心の加速度を引いたもの = 潮汐力 a = GM[(d − r)/|d − r|³ − d/|d|³]
-- 月の側も、反対側も外向き → ふくらみが 2 つ → 1 日に満潮が 2 回。太陽の潮汐は月の約 0.46 倍
-- 新月・満月（太陽と月が一直線）で大潮、上弦・下弦で小潮。海の形は「平衡潮汐」（大陸のない理想の海）
-- 操作：スペース = 再生/停止 / ←→ = 速さ / 1 月だけ 2 太陽だけ 3 両方 / ↑↓ = 月までの距離（×0.6〜×2）
-- ============================================
module Main (main) where

import Art
import Data.Bits ((.&.))
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Environment (lookupEnv)

grav, mMoon, mSun, mEarth :: Double
grav = 6.674e-11                       -- [m³/(kg s²)]
mMoon = 7.342e22                       -- [kg]
mSun = 1.989e30
mEarth = 5.972e24

dMoon, dSun, rEarth :: Double
dMoon = 3.844e8                        -- [m]
dSun = 1.496e11
rEarth = 6.371e6

exag :: Double
exag = 0.35                            -- ★ 海のふくらみを見やすく大げさにする倍率（R に対する割合 / 1 m）

speeds :: [Double]
speeds = [0.5, 2, 6, 24, 72]           -- 1 秒あたりに進む時間 [時間]

data World = World
  { wHours   :: !Double
  , wPlaying :: !Bool
  , wSp      :: !Int
  , wWhich   :: !Int                   -- 1 = 月、2 = 太陽、3 = 両方（ビット）
  , wDScale  :: !Double                -- 月までの距離の倍率
  , wHist    :: !(Seq (Double, Double))  -- 観測地点の (時刻, 潮位)。過去 72 時間
  , wMonth   :: !(U.Vector Double)     -- 30 日間の満潮の高さ（設定を変えたときだけ計算）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-02 Tides" world0)
  { skBack  = rgb255 8 14 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetCurves (World 0 True 2 3 1 Seq.empty U.empty [])

-- ---- 天体の向きと潮汐
-- 天体の向き（上から見た角度 [度]）。t = 0 は新月（太陽と月が同じ向き）
moonAng, sunAng, spinAng :: Double -> Double
moonAng t = 360 * t / (27.3217 * 24)
sunAng t = 360 * t / (365.2422 * 24)
spinAng t = 360 * t / 23.9345          -- 地球の自転（恒星日 23 時間 56 分）

-- 地球の中心から見た位置 r（m）での潮汐加速度 [m/s²]。m = 質量、d = 中心から天体までのベクトル
tidalAcc :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
tidalAcc rx ry m dx dy = (grav * m * (ex / e3 - dx / d3), grav * m * (ey / e3 - dy / d3))
  where
    ex = dx - rx
    ey = dy - ry
    e3 = (ex * ex + ey * ey) ** 1.5
    d3 = (dx * dx + dy * dy) ** 1.5

-- 天体 (質量, dx, dy, 距離)
bodies :: Int -> Double -> Double -> [(Double, Double, Double, Double)]
bodies which ds t =
  [ (mMoon, dm * cos m, dm * sin m, dm) | which .&. 1 /= 0 ]
  ++ [ (mSun, dSun * cos s, dSun * sin s, dSun) | which .&. 2 /= 0 ]
  where
    m = moonAng t * degR
    s = sunAng t * degR
    dm = dMoon * ds

-- 平衡潮汐の高さ [m]：h = H (3 cos²ψ − 1) / 2、H = (M/M地球)(R/d)³ R（ψ = 天体の方向からの角度）
eqH :: Double -> Double -> Double
eqH m d = (m / mEarth) * (rEarth / d) ^ (3 :: Int) * rEarth

tideHeight :: Int -> Double -> Double -> Double -> Double
tideHeight which ds ang t = foldl' (\h (m, dx, dy, d) -> let c = cos (ang - atan2 dy dx) in h + eqH m d * (3 * c * c - 1) / 2) 0 (bodies which ds t)

totalAcc :: Int -> Double -> Double -> Double -> Double -> (Double, Double)
totalAcc which ds rx ry t = foldl' (\(ax, ay) (m, dx, dy, _) -> let (qx, qy) = tidalAcc rx ry m dx dy in (ax + qx, ay + qy)) (0, 0) (bodies which ds t)

-- 30 日間の満潮の高さ（240 区間、各時刻で 72 方向の最大）
monthCurve :: Int -> Double -> U.Vector Double
monthCurve which ds = U.generate 241 $ \i ->
  let t = fromIntegral i / 240 * 30 * 24
  in foldl' (\hi k -> max hi (tideHeight which ds (fromIntegral k / 72 * pi) t)) (-9) [0 .. 71 :: Int]

resetCurves :: World -> World
resetCurves w = w { wHist = Seq.empty, wMonth = monthCurve (wWhich w) (wDScale w) }

obsHeight :: World -> Double
obsHeight w = tideHeight (wWhich w) (wDScale w) (spinAng (wHours w) * degR) (wHours w)

step :: World -> World
step w0 = w { wHist = trim (addH (wHist w)) }
  where
    w = if wPlaying w0 then w0 { wHours = wHours w0 + getAt 1 speeds (wSp w0) / 60 } else w0
    t = wHours w
    -- 止まっている間は同じ点を足さない（グラフは同じ）
    addH hs = case Seq.viewr hs of
      _ Seq.:> (t1, _) | t1 == t -> hs
      _ -> hs |> (t, obsHeight w)
    trim hs = case Seq.viewl hs of
      (t0, _) Seq.:< rest | Seq.length hs > 1 && t0 < t - 72 -> trim rest
      _ -> hs

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- 海（ふくらみを大げさに）
  [ Color (rgb255 40 100 190) (Polygon [ (cx + r * cos a, cy - r * sin a) | i <- [0 .. 180 :: Int], let a = fromIntegral i / 180 * tau
                                       , let r = rr * (1 + f2 (exag * tideHeight wh ds (realToFrac a) hrs)) ])
  , Color (rgb255 150 115 70) (circleD cx cy (2 * rr * 0.93)) ]   -- 地面
  -- 潮汐力の矢印（地球の中と表面）
  ++ [ arrowAt (cx + gxx * rr) (cy - gyy * rr) (totalAcc wh ds (realToFrac gxx * rEarth) (realToFrac gyy * rEarth) hrs) (realToFrac rr * 0.22 / amax)
     | gyy <- grid, gxx <- grid, gxx * gxx + gyy * gyy <= 1.0001 ]
  ++ [ arrowAt (cx + rr * cos a) (cy - rr * sin a) (totalAcc wh ds (rEarth * cos ad) (rEarth * sin ad) hrs) (realToFrac rr * 0.22 / amax)
     | i <- [0 .. 23 :: Int], let ad = fromIntegral i / 24 * 2 * pi, let a = f2 ad ]
  -- 月・太陽の方向と観測地点
  ++ [ Color (rgba255 230 230 220 (if wh .&. 1 /= 0 then 255 else 60)) (circleD mxp myp 26)
     , Color (rgba255 255 210 90 (if wh .&. 2 /= 0 then 255 else 60)) (circleD sxp syp 40)
     , sayC "Moon" mxp (myp + 30) 14 (gray 0.86)
     , sayC "Sun direction" sxp (syp + 36) 14 (gray 0.86)
     , Color (rgb255 255 90 90) (circleD (cx + rr * (1 + f2 (exag * oh)) * cos oa) (cy - rr * (1 + f2 (exag * oh)) * sin oa) 14)
     , sayC "Observer (turns with Earth)" (max 130 (cx + rr * 1.25 * cos oa)) (cy - rr * 1.25 * sin oa) 14 (rgb255 255 160 160) ]
  -- 観測地点の潮位のグラフ（過去 3 日）
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx gy gw gh)
     , Color (rgb255 255 120 120) (polyline 2 [ (gx + gw - f2 ((hrs - t) / 72) * gw, yv h) | (t, h) <- thin 600 (foldr (:) [] (wHist w)) ])
     , say "Sea level at the observer, last 72 h (+-0.6 m)" gx (gy - 8) 14 (rgb255 200 205 225) ]
  -- 1 か月の満潮の高さ（大潮・小潮）
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx gy2 gw gh2)
     , Color (rgb255 140 190 255) (polyline 2 [ (gx + fromIntegral i / 240 * gw, gy2 + gh2 - f2 (hi / 0.6) * gh2) | (i, hi) <- zip [0 :: Int ..] (U.toList (wMonth w)) ])
     , Color (rgba255 255 255 255 150) (Line [(tx, gy2), (tx, gy2 + gh2)])
     , say "High tide over 30 days (new -> full -> new moon)" gx (gy2 - 8) 14 (rgb255 200 205 225) ]
  -- 数字
  ++ [ say "Tides | Space play/stop  Left/Right speed  1 moon  2 sun  3 both  Up/Down moon distance" 20 34 18 (rgb255 240 236 215)
     , say ("Day " ++ fmt 2 (hrs / 24) ++ " (" ++ showJ (getAt 1 speeds (wSp w)) ++ " h/s)   moon distance = " ++ fmt 0 (dMoon * ds / 1000) ++ " km (x" ++ fmt 1 ds
            ++ ")   moon - sun angle = " ++ fmt 0 (wrap360 (moonAng hrs - sunAng hrs))) 20 60 15 (rgb255 250 225 140)
     , say ("Moon: 2GMR/d^3 = " ++ expo 3 am ++ " m/s2 (= " ++ fmt 2 (am / 9.81 * 1e7) ++ "e-7 g)") gx (gy2 + gh2 + 34) 14 c2
     , say ("Sun: " ++ expo 3 as ++ " m/s2   sun / moon = " ++ fmt 3 ratio) gx (gy2 + gh2 + 56) 14 c2
     , say ("Spring / neap = (1 + " ++ fmt 2 ratio ++ ") / (1 - " ++ fmt 2 ratio ++ ") = " ++ fmt 2 ((1 + ratio) / max 1e-6 (1 - ratio))) gx (gy2 + gh2 + 78) 14 c2
     , say ("Equilibrium tide of the moon H = " ++ fmt 3 (eqH mMoon (dMoon * ds)) ++ " m") gx (gy2 + gh2 + 100) 14 c2
     , say ("Arrows = tidal force (difference from the center). Ocean thickness exaggerated (" ++ showJ exag ++ " x R / m)") 20 (envH env - 34) 14 (rgb255 170 175 200)
     , drawSelfTest env (wTest w) ]
  where
    wd = envW env
    ht = envH env
    hrs = wHours w
    wh = wWhich w
    ds = wDScale w
    cx = wd * 0.28
    cy = ht * 0.53
    rr = min (wd * 0.17) (ht * 0.3)
    amax = 2 * grav * (mMoon / dMoon ^ (3 :: Int) + mSun / dSun ^ (3 :: Int)) * rEarth
    grid = [ -1 + 0.25 * fromIntegral i | i <- [0 .. 8 :: Int] ]
    ma = f2 (moonAng hrs * degR)
    sa = f2 (sunAng hrs * degR)
    (mxp, myp) = (cx + rr * 1.4 * cos ma, cy - rr * 1.4 * sin ma)
    (sxp, syp) = (cx + rr * 1.62 * cos sa, cy - rr * 1.62 * sin sa)
    oad = spinAng hrs * degR
    oa = f2 oad
    oh = obsHeight w
    gx = wd * 0.6
    gw = wd - gx - 30
    gy = ht * 0.16
    gh = ht * 0.26
    yv h = gy + gh / 2 - f2 (h / 0.6) * (gh / 2)
    gy2 = gy + gh + 70
    gh2 = ht * 0.2
    tx = gx + f2 (jsRem hrs (30 * 24) / (30 * 24)) * gw
    am = 2 * grav * mMoon * rEarth / (dMoon * ds) ^ (3 :: Int)
    as = 2 * grav * mSun * rEarth / dSun ^ (3 :: Int)
    ratio = as / am
    c2 = rgb255 225 228 240

-- 矢印（長さ 0.5 ピクセル以下は描かない）。y は上向きの加速度を画面の下向きに直す
arrowAt :: Float -> Float -> (Double, Double) -> Double -> Picture
arrowAt x y (ax, ay) k
  | not (l > 0.5) = Blank
  | otherwise = Color (rgb255 255 240 180) $ Pictures
      [ thickLine 1.5 (x, y) (x + vx, y + vy)
      , thickLine 1.5 (x + vx, y + vy) (x + vx - 5 * ux + 3 * uy, y + vy - 5 * uy - 3 * ux)
      , thickLine 1.5 (x + vx, y + vy) (x + vx - 5 * ux - 3 * uy, y + vy - 5 * uy + 3 * ux) ]
  where
    vx = f2 (ax * k)
    vy = f2 (-ay * k)
    l = sqrt (vx * vx + vy * vy)
    ux = vx / l
    uy = vy / l

-- 点が多すぎるときは間引く（形は同じ。最後の点は残す）
thin :: Int -> [a] -> [a]
thin n xs
  | len <= n = xs
  | otherwise = [ x | (i, x) <- zip [0 :: Int ..] xs, i `mod` k == 0 || i == len - 1 ]
  where
    len = length xs
    k = (len + n - 1) `div` n

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wSp = min (length speeds - 1) (wSp w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wSp = max 0 (wSp w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (resetCurves w { wDScale = min 2 (wDScale w + 0.1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (resetCurves w { wDScale = max 0.6 (wDScale w - 0.1) })
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (resetCurves w { wWhich = fromEnum c - fromEnum '0' })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("near side pulled toward the moon, far side away from it (two bulges)", near1 > 0 && far1 < 0)
  , ("mean size on the two sides = 2GMR/d^3 ~ 1.10e-6 m/s2 (within 0.1%)", near ((near1 - far1) / 2 / k2) 1 0.001 && near k2 1.10e-6 0.02e-6)
  , ("points at the side are pushed to the center: half the size (GMR/d^3)", side < 0 && near (-side / k2) 0.5 0.01)
  , ("sun tide / moon tide = (Msun/Mmoon)(dmoon/dsun)^3 ~ 0.46", near ratio 0.46 0.01)
  , ("spring / neap = (1 + 0.46) / (1 - 0.46) ~ 2.7", near ((1 + ratio) / (1 - ratio)) 2.7 0.1)
  , ("equilibrium tide of the moon: high +0.36 m, low -0.18 m (range 0.54 m)", near (eqH mMoon dMoon) 0.357 0.003 && near (1.5 * eqH mMoon dMoon) 0.535 0.005)
  , ("moon only: 2 high tides in 24 h 50 min", n == 2) ]
  where
    near1 = fst (tidalAcc rEarth 0 mMoon dMoon 0)
    far1 = fst (tidalAcc (-rEarth) 0 mMoon dMoon 0)
    side = snd (tidalAcc 0 rEarth mMoon dMoon 0)
    k2 = 2 * grav * mMoon * rEarth / dMoon ^ (3 :: Int)
    ratio = (mSun / mMoon) * (dMoon / dSun) ^ (3 :: Int)
    prev = U.generate 2485 (\i -> let t = 3 + fromIntegral i * 0.01 in tideHeight 1 1 (spinAng t * degR) t)
    n = length [ () | i <- [1 .. U.length prev - 2], prev U.! i > prev U.! (i - 1), prev U.! i >= prev U.! (i + 1) ]

-- ---------- やってみよう ----------
-- 1. 1（月だけ）と 2（太陽だけ）で、ふくらみの大きさを比べよう。太陽はずっと重いのに小さいのはなぜ？
-- 2. 3 で 30 日ぶん進めて、大潮（新月・満月）と小潮（上弦・下弦）を見つけよう
-- 3. ↑↓ で月の距離を半分に近づけると、潮汐は何倍？（1/d³ なので 8 倍）
-- 4. 観測地点のグラフで、満潮から次の満潮までの時間を測ろう（約 12 時間 25 分）
-- 5. 本当の海では大陸や海の深さのせいで時刻も高さも変わる。日本の港の潮汐表と比べてみよう

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

-- 指数の形で書く（JavaScript の toExponential(n)：1.100e-6）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = showFFloat (Just n) (0 :: Double) "" ++ "e+0"
  | otherwise = showFFloat (Just n) m' "" ++ "e" ++ (if e' >= 0 then "+" else "") ++ show e'
  where
    v = fin 0 v0
    e = floor (logBase 10 (abs v)) :: Int
    m = v / 10 ^^ e
    -- 丸めて 10.000 になったら桁を 1 つ上げる
    (m', e') = if abs (read (showFFloat (Just n) m "") :: Double) >= 10 then (m / 10, e + 1) else (m, e)

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
