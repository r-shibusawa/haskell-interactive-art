-- ============================================
-- 50-10 ファジィ制御で車庫入れ（トラックのバック）
-- 「車庫より少し右にいて、車の向きが真上なら、ハンドルを左にいっぱい」― 位置 x と向き φ の 5 × 7 = 35 規則でハンドル角を決める
--   車はバックで進む（自転車モデル：x' = −v cos φ、y' = −v sin φ、φ' = −v tan δ / L）を RK4 で解く
-- 操作：クリック = そこから出発 / ←→ = 出発の向き / A = お手本 8 台 / ↑↓ = 速さ / C = 消す / T = 表示する車を切りかえ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Fixed (mod')
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

lw, vel, dMax :: Double
lw = 2.5             -- ★ ホイールベース [m]
vel = 1.0            -- ★ バックの速さ [m/s]
dMax = 35            -- ★ ハンドルの最大 [度]

xN :: [String]
xN = ["far L", "near L", "center", "near R", "far R"]   -- 位置 x の集合
xC :: [Double]
xC = [-10, -2, 0, 2, 10]                                 -- ★ その中心 [m]

pN :: [String]
pN = ["0", "60", "80", "90", "100", "120", "180"]       -- 向き φ の集合（右向き・右ななめ・少し右・真上・少し左・左ななめ・左向き）
pC :: [Double]
pC = [0, 60, 80, 90, 100, 120, 180]                      -- ★ その中心 [度]

dN :: [String]
dN = ["NB", "NM", "NS", "ZE", "PS", "PM", "PB"]          -- ハンドル角 δ（正 = 左にきる）

-- ★ 規則の表：ruleTab[位置][向き] = ハンドルの番号（0 = NB … 6 = PB）
ruleTab :: [[Int]]
ruleTab =
  [ [0, 0, 0, 0, 0, 0, 6]   -- 左遠
  , [0, 0, 0, 0, 1, 4, 6]   -- 左近
  , [0, 0, 1, 3, 5, 6, 6]   -- 中央
  , [0, 2, 5, 6, 6, 6, 6]   -- 右近
  , [0, 6, 6, 6, 6, 6, 6] ] -- 右遠

-- ★ お手本の出発点 (x, y, 向き)
presets :: [(Double, Double, Double)]
presets = [(-8, 24, 0), (-6, 28, 90), (-4, 22, 150), (-2, 26, 45), (3, 24, 180), (5, 27, 90), (7, 22, 30), (8, 28, 135)]

d2r :: Double
d2r = pi / 180

hTruck :: Double
hTruck = 0.05        -- RK4 の時間きざみ [s]

sys :: FSys
sys = FSys [FVar (-10) 10 (triSets xC xN), FVar 0 180 (triSets pC pN)]
           (FVar (-dMax) dMax (evenTri (-dMax) dMax dN))
           [ Rule [i, j] (getAt 3 (getAt [] ruleTab i) j) | i <- [0 .. 4], j <- [0 .. 6] ] False False 141

data Truck = Truck
  { tS    :: [Double]            -- [x, y, φ]（φ はラジアン）
  , tPath :: [(Double, Double)]  -- 0.25 秒ごとの位置（新しい順）
  , tD    :: !Double             -- ハンドル角 [度]
  , tDone :: !Bool
  , tOk   :: !Bool
  , tT    :: !Double }

data World = World
  { wTrucks :: [Truck]
  , wSel    :: !Int
  , wSpeedK :: !Int     -- ★ 1 フレームに何回計算するか（↑↓。最初は 3）
  , wPhi0   :: !Int     -- ★ クリックで出発するときの向き [度]（90 = 前が上）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "50-10 Fuzzy truck parking" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest           -- 70 通りの出発点の成績もここで計算しておく
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World runPresets 0 3 90 []

-- 運動方程式（バック）：s = [x, y, φ]、δ はハンドル角 [度]
truckDeriv :: [Double] -> Double -> [Double]
truckDeriv s d = [-vel * cos phi, -vel * sin phi, (-vel * tan (d * d2r)) / lw]
  where phi = getAt 0 s 2

-- JavaScript の %（割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem a m = let r = mod' (abs a) m in if a < 0 then -r else r

-- 向きを 0〜180 度に入れる（下向きは端の集合へ）
headingDeg :: [Double] -> Double
headingDeg s = clampv 0 180 (if p > 270 then p - 360 else p)
  where p = jsRem (jsRem (getAt 0 s 2 / d2r) 360 + 360) 360

wrapDeg :: Double -> Double                     -- 0〜360 度に
wrapDeg a = jsRem (jsRem (a / d2r) 360 + 360) 360

controller :: [Double] -> MRes
controller s = mamdani sys [getAt 0 s 0, headingDeg s]

makeTruck :: Double -> Double -> Double -> Truck
makeTruck x y p = Truck [x, y, p * d2r] [(x, y)] 0 False False 0

advanceTruck :: Double -> Truck -> Truck
advanceTruck h tr
  | tDone tr = tr
  | otherwise = tr' { tDone = tDone tr' || abs x > 14 || y > 40 || t > 80 }
  where
    d = mY (controller (tS tr))
    s = rk4 truckDeriv (tS tr) d h
    t = tT tr + h
    x = getAt 0 s 0
    y = getAt 0 s 1
    path0 = if jsRem t 0.25 < h then take 400 ((x, y) : tPath tr) else tPath tr
    landed = y <= 0
    tr' = tr { tS = s, tD = d, tT = t
             , tPath = if landed then (x, y) : path0 else path0
             , tDone = landed
             , tOk = landed && abs x < 0.5 && abs (wrapDeg (getAt 0 s 2) - 90) < 5 }

-- 最後まで走らせる（自己テストと成績用）
runToEnd :: Double -> Double -> Double -> Truck
runToEnd x y p = go (makeTruck x y p)
  where go tr = if tDone tr then tr else go (advanceTruck hTruck tr)

runPresets :: [Truck]
runPresets = [makeTruck x y p | (x, y, p) <- presets]

step :: World -> World
step w = w { wTrucks = map (\tr -> iterate (advanceTruck hTruck) tr !! max 0 (wSpeedK w)) (wTrucks w) }

-- 駐車場の縮尺と原点
geom :: Env -> (Float, Float, Float)
geom env = (sc, 20 + 13 * sc, envH env - 85)
  where sc = min (envW env * 0.56 / 26) ((envH env - 170) / 32)

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 32 16 (hsb 0 0 95) ("Click: start there (heading Left/Right: " ++ show (wPhi0 w) ++ " deg)   A: 8 examples   Up/Down: speed x"
                               ++ show (wSpeedK w) ++ "   C: clear   T: select truck")
  , txt 20 58 15 (hsb 0 0 70) "Input = position x and heading phi, output = steering delta. Centroid -> one angle, then back up one step" ]
    -- 駐車場と車庫
  ++ [ Color (hsba 0 0 100 0.15) (Line [pt x 0, pt x 30]) | x <- [-12, -10 .. 12] ]
  ++ [ Color (hsba 0 0 100 0.15) (Line [pt (-12) y, pt 12 y]) | y <- [0, 2 .. 30] ]
  ++ [ Color (hsb 175 60 80) (Pictures [ thickLine 4 (pt (-1.5) 0) (pt (-1.5) (-1.5)), thickLine 4 (pt 1.5 0) (pt 1.5 (-1.5))
                                       , thickLine 4 (pt (-12) 0) (pt (-1.5) 0), thickLine 4 (pt 1.5 0) (pt 12 0) ])
     , txt (fst (pt 2 0)) (snd (pt 0 (-1))) 14 (hsb 175 60 80) "garage (x = 0, heading 90 deg)" ]
    -- 軌跡と車
  ++ concat [ [ Color (withA 0.6 (tcol tr)) (pathPic (map (uncurry pt) ((getAt 0 (tS tr) 0, getAt 0 (tS tr) 1) : tPath tr)))
              , drawTruck pt tr (if k == wSel w then hsb 20 45 100 else hsba 260 25 85 0.8) ]
            | (k, tr) <- zip [0 :: Int ..] (wTrucks w) ]
  ++ (case drop (wSel w) (wTrucks w) of
        (tr : _) | wSel w >= 0 -> panel tr
        _ -> [])
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    (sc, ox, oy) = geom env
    pt x y = (ox + f2 x * sc, oy - f2 y * sc)
    many = sum (map (length . tPath) (wTrucks w)) > 3000      -- 軌跡が多すぎるときは細い線で
    pathPic ps = if many then Line ps else thickPath 2 ps
    tcol tr = if tDone tr then (if tOk tr then hsb 160 50 90 else hsb 0 70 100) else hsb 20 45 100
    -- 選んだ車の推論
    panel tr =
      let r = controller (tS tr)
          tx = envW env * 0.6
          ty = 100
          cs = min 46 ((envW env - tx - 140) / 7.5)
          c0 = tx + 106
          yy = ty + 34 + 5 * cs + 16
          gx = tx
          gy = yy + 16
          gw = envW env - tx - 30
          gh = 90
          gX x = gx + f2 ((x + dMax) / (2 * dMax)) * gw
          gY a = gy + gh - f2 a * gh
          pts = zip (map gX (mOx r)) (map gY (mAgg r))
          xc = gX (mY r)
          trucks = wTrucks w
          done = [ (k, q) | (k, q) <- zip [1 :: Int ..] trucks, tDone q ]
          okN = length (filter (tOk . snd) done)
          (bn, bok, bt) = bench
      in [ txt tx ty 15 (hsb 0 0 95) ("Truck " ++ show (wSel w + 1) ++ ": x = " ++ fmt 2 (getAt 0 (tS tr) 0) ++ " m  phi = "
                                       ++ fmt 1 (headingDeg (tS tr)) ++ " deg  -> delta = " ++ fmt 1 (mY r) ++ " deg")
         ]
         ++ [ txt (c0 + fromIntegral j * cs + 4) (ty + 26) 14 (hsb 0 0 70) nm | (j, nm) <- zip [0 :: Int ..] pN ]
         ++ [ txt tx (ty + 26) 14 (hsb 0 0 55) "x \\ phi" ]
         ++ concat
            [ [ txt tx (ty + 34 + (fromIntegral i + 0.6) * cs) 14 (hsb 0 0 70) nm
              , txt (tx + 62) (ty + 34 + (fromIntegral i + 0.6) * cs) 14 (hsb 0 0 85) (fmt 2 (getAt 0 (getAt [] (mDeg r) 0) i)) ]
              ++ concat [ cell r c0 ty cs i j | j <- [0 .. 6] ]
            | (i, nm) <- zip [0 :: Int ..] xN ]
         ++ [ txt tx yy 14 (hsb 0 0 70) ("heading memberships: " ++ unwords [ nm ++ ": " ++ fmt 2 m ++ " " | (nm, m) <- zip pN (getAt [] (mDeg r) 1), m > 0 ]) ]
           -- 集約した出力と重心
         ++ [ Color (hsba 0 0 100 0.2) (frameTL gx gy gw gh)
            , Color (hsba 20 45 100 0.4) (Pictures [ Polygon [(x0, gy + gh), (x0, y0), (x1, y1), (x1, gy + gh)] | ((x0, y0), (x1, y1)) <- zip pts (drop 1 pts) ])
            , Color (hsb 20 45 100) (thickPath 2 pts)
            , Color (hsb 175 60 90) (thickLine 3 (xc, gy - 4) (xc, gy + gh + 4))
            , txt gx (gy + gh + 18) 14 (hsb 0 0 70) "-35 deg"
            , txt (gx + gw - 56) (gy + gh + 18) 14 (hsb 0 0 70) "+35 deg"
            , txtCB (gx + gw / 2) (gy + gh + 18) 14 (hsb 0 0 70) ("aggregated delta, centroid " ++ fmt 1 (mY r))
              -- 成績
            , txt tx (gy + gh + 48) 15 (hsb 0 0 90) ("Parked: " ++ show okN ++ " / " ++ show (length done) ++ "  (still driving: " ++ show (length trucks - length done) ++ ")") ]
         ++ [ txt tx (gy + gh + 72 + fromIntegral k * 20) 14 (if tOk q then hsb 160 50 90 else hsb 0 70 100)
                  ((if tOk q then "OK " else "NG ") ++ "truck " ++ show n ++ ": x error " ++ fmt 2 (abs (getAt 0 (tS q) 0)) ++ " m, heading error "
                   ++ fmt 1 (abs (wrapDeg (getAt 0 (tS q) 2) - 90)) ++ " deg, " ++ fmt 1 (tT q) ++ " s")
            | (k, (n, q)) <- zip [0 :: Int ..] (lastN 4 done) ]
         ++ [ txt tx (envH env - 44) 14 (hsb 0 0 70)
                ("Grid of " ++ show bn ++ " starts: " ++ show bok ++ " parked (" ++ fmt 0 (100 * fromIntegral bok / fromIntegral (max 1 bn)) ++ "%), mean " ++ fmt 1 bt ++ " s") ]
    cell r c0 ty cs i j =
      let wv = getAt 0 (mW r) (i * 7 + j)
          k = getAt 3 (getAt [] ruleTab i) j
          x = c0 + fromIntegral j * cs
          y = ty + 34 + fromIntegral i * cs
          hue = if k < 3 then 190 else if k > 3 then 20 else 260
          tc = hsb 0 0 (if wv > 0.6 then 10 else 92)
      in [ Color (hsb hue 30 (22 + 70 * f2 wv)) (rectTL x y (cs - 4) (cs - 4))
         , Color (hsba 0 0 100 (if wv > 0 then 0.8 else 0.15)) (frameTL x y (cs - 4) (cs - 4))
         , txt (x + 6) (y + cs * 0.42) 14 tc (getAt "" dN k) ]
         ++ [ txt (x + 3) (y + cs * 0.82) 14 tc (fmt 2 wv) | wv > 0 ]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- トラック：後輪の軸が (x, y)、前へ 4 m の車体
drawTruck :: (Double -> Double -> (Float, Float)) -> Truck -> Color -> Picture
drawTruck pt tr col = Pictures
  [ Color col (Polygon [ pt (x + a * c - b * s) (y + a * s + b * c) | (a, b) <- [(-0.8, -1), (3.6, -1), (3.6, 1), (-0.8, 1)] ])
  , Color (hsb 0 0 15) (thickLine 4 (pt (fx - 0.6 * cos d) (fy - 0.6 * sin d)) (pt (fx + 0.6 * cos d) (fy + 0.6 * sin d)))
  , Color (hsb 0 0 15) (at (pt x y) (disc 3)) ]
  where
    x = getAt 0 (tS tr) 0
    y = getAt 0 (tS tr) 1
    f = getAt 0 (tS tr) 2
    c = cos f
    s = sin f
    fx = x + 3 * c                    -- 前輪（ハンドルの向き）
    fy = y + 3 * s
    d = f + tD tr * d2r

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | not (x < -11 || x > 11 || y < 4 || y > 30) ->
        w { wTrucks = wTrucks w ++ [makeTruck x y (fromIntegral (wPhi0 w))], wSel = length (wTrucks w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPhi0 = (wPhi0 w + 15) `mod` 360 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPhi0 = (wPhi0 w + 345) `mod` 360 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeedK = min 12 (wSpeedK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeedK = max 1 (wSpeedK w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wTrucks = runPresets, wSel = 0 }
    | c `elem` "cC" -> w { wTrucks = [], wSel = 0 }
    | c `elem` "tT" && not (null (wTrucks w)) -> w { wSel = (wSel w + 1) `mod` length (wTrucks w) }
  _ -> w
  where
    (sc, ox, oy) = geom env
    (mx, my) = p5Mouse env
    x = realToFrac ((mx - ox) / sc) :: Double
    y = realToFrac ((oy - my) / sc) :: Double

-- 格子の出発点（x = −6〜6、向き 0〜180、y = 22 と 28 の 70 通り）での成績：(数, 成功, 平均の時間)
bench :: (Int, Int, Double)
bench = (n, ok, tt / fromIntegral (max 1 ok))
  where
    qs = [ runToEnd x y p | x <- [-6, -4 .. 6], p <- [0, 45 .. 180], y <- [22, 28] ]
    n = length qs
    oks = filter tOk qs
    ok = length oks
    tt = foldl' (+) 0 (map tT oks)

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("rule table is mirror-symmetric (mirror x and heading -> opposite steering)", anti)
  , ("x = 0, heading 90 -> only (center, up) fires -> delta = centroid of ZE = 0", nearTol 1e-9 (mY (controller [0, 10, 90 * d2r])) 0)
  , ("straight back from above -> stays x = 0, heading 90, into the garage", tOk st && abs (getAt 1 (tS st) 0) < 1e-9)
  , ("RK4: steering 20 deg for 2 s matches the circle x = -v(sin phi - sin phi0)/w etc. (error < 1e-6)"
    , nearTol 1e-6 (getAt 0 s40 0) ((-vel * (sin ph - 1)) / om) && nearTol 1e-6 (getAt 0 s40 1) ((vel * cos ph) / om) && nearTol 1e-9 (getAt 0 s40 2) ph)
  , ("mirror starts (6, 24, 60) and (-6, 24, 120) -> final x has opposite sign", nearTol 1e-6 (getAt 0 (tS a) 0) (-(getAt 0 (tS b) 0)))
  , ("all 8 examples park with |x| < 0.5 m and heading within 5 deg", all (\(x, y, p) -> tOk (runToEnd x y p)) presets)
  , ("grid of " ++ show bn ++ " starts: at least 90% park (" ++ show bok ++ " / " ++ show bn ++ ")", fromIntegral bok >= 0.9 * (fromIntegral bn :: Double))
  , ("start at heading 300 -> after a full turn (450) still judged as 90", tOk (runToEnd 0 25 300)) ]
  where
    anti = and [ getAt 0 (getAt [] ruleTab (4 - i)) (6 - j) == 6 - getAt 0 (getAt [] ruleTab i) j | i <- [0 .. 4], j <- [0 .. 6] ]
    st = runToEnd 0 25 90
    om = (-vel * tan (20 * d2r)) / lw
    s40 = iterate (\s -> rk4 truckDeriv s 20 0.05) [0, 0, pi / 2] !! 40
    ph = pi / 2 + om * 2
    a = runToEnd 6 24 60
    b = runToEnd (-6) 24 120
    (bn, bok, _) = bench

-- ---------- やってみよう ----------
-- 1. 車庫のすぐ上（y = 8 くらい）で、横に離れた所から出発すると？ 間に合わないのはなぜ？
-- 2. 表の「中央」の行の 1（NM）と 5（PM）を 2 と 4 にすると、ハンドルがおだやかになる。成功率は？
-- 3. xC の 2 と −2 を 5 と −5 にすると、車庫の近くで「まっすぐにする」力が弱くなる
-- 4. lw（車の長さ）を 5 にすると、小回りがきかない。規則はそのままで大丈夫？
-- 5. 向き 270°（前が下）で出発すると、端の集合（右向き・左向き）だけで回って戻ってくる

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 横は中央、y が文字の下の線（p5.js の textAlign(CENTER)）
txtCB :: Float -> Float -> Float -> Color -> String -> Picture
txtCB x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠
frameTL :: Float -> Float -> Float -> Float -> Picture
frameTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- 小数点以下 n 桁で書く（p5.js の toFixed / nf）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- Double を画面の Float に
f2 :: Double -> Float
f2 = realToFrac

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ==================================================
-- ここから下は「ファジィの道具」（中身は読まなくても OK。消さないでね）
-- 計算はすべて Double（倍精度）で行う
-- ==================================================
-- ---- メンバーシップ関数（所属度 μ を 0〜1 で返す）
trimf :: Double -> Double -> Double -> Double -> Double      -- 三角：a で 0、b で 1、c で 0
trimf x a b c
  | x < a || x > c = 0
  | x == b = 1
  | x < b = if b > a then (x - a) / (b - a) else 1
  | otherwise = if c > b then (c - x) / (c - b) else 1

-- メンバーシップ関数の形とパラメータ（このスケッチは三角だけ使う）
data MF = Tri !Double !Double !Double

evalMF :: MF -> Double -> Double
evalMF (Tri a b c) x = trimf x a b c

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 非ファジィ化：重心 = ∫x μ dx ÷ ∫μ dx（折れ線の区間ごとに正確に積分）。どの規則も発火しなければ Nothing
centroid :: [Double] -> [Double] -> Maybe Double
centroid xs ys = if a > 1e-12 then Just (m / a) else Nothing
  where
    (a, m) = foldl' add (0, 0) (zip4' xs (drop 1 xs) ys (drop 1 ys))
    add (!aa, !mm) (x0, x1, y0, y1) =
      let h = x1 - x0
      in (aa + h * (y0 + y1) / 2, mm + h * (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1)) / 6)
    zip4' (p : ps) (q : qs) (r : rs) (s : ss) = (p, q, r, s) : zip4' ps qs rs ss
    zip4' _ _ _ _ = []

-- ---- マムダニ型推論
-- 集合（名前とメンバーシップ関数）、変数（範囲と集合）、規則（入力ごとの集合の番号 → 出力の集合の番号。−1 は「使わない」）
data FSet = FSet { fsName :: String, fsMF :: MF }
data FVar = FVar { fvLo :: !Double, fvHi :: !Double, fvSets :: [FSet] }
data Rule = Rule { rIfs :: [Int], rThen :: !Int }
data FSys = FSys
  { syInputs :: [FVar], syOutput :: FVar, syRules :: [Rule]
  , syAndProd :: !Bool        -- かつ：False = min、True = 積
  , syImpProd :: !Bool        -- 含意：False = 切る（min）、True = 縮める（積）
  , syN :: !Int }             -- 出力を何点で調べるか

-- 推論の途中の値も全部返す（所属度・規則の強さ・後件ごとの強さ・集約した形・重心）
data MRes = MRes
  { mDeg :: [[Double]], mW :: [Double], mStr :: [Double]
  , mOx :: [Double], mAgg :: [Double], mY :: !Double, mFired :: !Bool }

mamdani :: FSys -> [Double] -> MRes
mamdani sys xin = MRes deg w str ox agg (maybe ((fvLo o + fvHi o) / 2) id c) (c /= Nothing)
  where
    deg = [ [evalMF (fsMF s) (clampv (fvLo v) (fvHi v) xi) | s <- fvSets v] | (v, xi) <- zip (syInputs sys) xin ]
    w = [ foldl' (\acc (i, si) -> if si < 0 then acc
                                   else let d = getAt 0 (getAt [] deg i) si
                                        in if syAndProd sys then acc * d else min acc d)
                 1 (zip [0 ..] (rIfs r))
        | r <- syRules sys ]
    o = syOutput sys
    nSets = length (fvSets o)
    -- 同じ後件は max でまとめる
    str = [ foldl' max 0 [wk | (r, wk) <- zip (syRules sys) w, rThen r == k] | k <- [0 .. nSets - 1] ]
    live = [ (s, fsMF f) | (s, f) <- zip str (fvSets o), s > 0 ]
    n = max 2 (syN sys)
    ox = [ fvLo o + (fvHi o - fvLo o) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
    agg = [ foldl' (\a (s, mf) -> let m = evalMF mf x in max a (if syImpProd sys then s * m else min s m)) 0 live | x <- ox ]
    c = centroid ox agg

-- 等間隔の三角 MF を n 個（両端は肩）
evenTri :: Double -> Double -> [String] -> [FSet]
evenTri lo hi names =
  [ FSet nm (Tri (lo + (i - 1) * d) (lo + i * d) (lo + (i + 1) * d)) | (i, nm) <- zip [0 ..] names ]
  where d = (hi - lo) / fromIntegral (length names - 1)

-- 中心の列から三角 MF を作る（両端は肩：端の集合は外側で 1）
triSets :: [Double] -> [String] -> [FSet]
triSets cs names =
  [ FSet nm (Tri (if i > 0 then getAt c cs (i - 1) else getAt c cs 0) c (if i < n - 1 then getAt c cs (i + 1) else c))
  | (i, (c, nm)) <- zip [0 ..] (zip cs names) ]
  where n = length cs

-- 4 次のルンゲ＝クッタ法：s' = f(s, u) を h だけ進める
rk4 :: ([Double] -> Double -> [Double]) -> [Double] -> Double -> Double -> [Double]
rk4 f s u h = zipWith5' (\v a b c d -> v + (h / 6) * (a + 2 * b + 2 * c + d)) s k1 k2 k3 k4
  where
    k1 = f s u
    k2 = f (zipWith (\v k -> v + (h / 2) * k) s k1) u
    k3 = f (zipWith (\v k -> v + (h / 2) * k) s k2) u
    k4 = f (zipWith (\v k -> v + h * k) s k3) u
    zipWith5' g (a : as) (b : bs) (c : cs) (d : ds) (e : es) = g a b c d e : zipWith5' g as bs cs ds es
    zipWith5' _ _ _ _ _ _ = []

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)
