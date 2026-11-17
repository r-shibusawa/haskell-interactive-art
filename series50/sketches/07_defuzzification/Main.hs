-- ============================================
-- 50-07 非ファジィ化の比較
-- 推論の結果は「形」（ファジィ集合）。最後に 1 つの数にする方法はいくつもある
--   重心法・面積二等分法・最大値の平均（MOM）・最大値の最小（SOM）・最大値の最大（LOM）
-- 操作：ドラッグ = 三角の頂点を動かす（横 = 位置、縦 = 切る高さ）/ 1 2 = 選ぶ / ←→ ↑↓ = 動かす / C = 切る ⇔ 縮める / R = もとに戻す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

loX, hiX :: Double
loX = 0               -- ★ 出力の範囲
hiX = 10

nPts :: Int
nPts = 1001           -- ★ 積分に使う点の数

-- 後件の三角 (a, b, c) と、切る高さ h
data Tri = Tri { ta :: !Double, tb :: !Double, tc :: !Double, th :: !Double }

-- ★ 2 つの後件
tris0 :: [Tri]
tris0 = [Tri 1 3 5 0.8, Tri 3.5 6.5 9.5 0.45]

-- 非ファジィ化の方法：名前・説明・色相
data Method = Method { mShort :: String, mName :: String, mDesc :: String, mHue :: Float }

methods :: [Method]
methods =
  [ Method "COG" "Centroid (COG)" "int x mu dx / int mu dx: the balance point" 175
  , Method "BIS" "Bisector of area" "the point that splits the area in half" 260
  , Method "MOM" "Mean of maxima MOM" "average of the points where mu is highest" 20
  , Method "SOM" "Smallest of maxima SOM" "leftmost point where mu is highest" 50
  , Method "LOM" "Largest of maxima LOM" "rightmost point where mu is highest" 330 ]

data World = World
  { wTris   :: [Tri]
  , wSel    :: !Int
  , wScaled :: !Bool       -- True = 縮める（積）、False = 切る（min）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-07 Defuzzification" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World tris0 0 False []

-- 2 つの「切った三角」の max（集約）を折れ線にする
aggregate :: World -> ([Double], [Double])
aggregate w = (xs, [ foldl' (\y t -> let m = trimf x (ta t) (tb t) (tc t) in max y (if wScaled w then th t * m else min (th t) m)) 0 (wTris w) | x <- xs ])
  where xs = [loX + ((hiX - loX) * fromIntegral i) / fromIntegral (nPts - 1) | i <- [0 .. nPts - 1]]

-- 5 つの方法の値（値がないときは Nothing）
data Defuzz = Defuzz { dCog, dBis, dMom, dSom, dLom :: Maybe Double, dHeight :: !Double }

defuzzAll :: [Double] -> [Double] -> Defuzz
defuzzAll xs ys = case maxima xs ys of
  Just (mom, som, lom, hgt) -> Defuzz (centroid xs ys) (bisector xs ys) (Just mom) (Just som) (Just lom) hgt
  Nothing -> Defuzz (centroid xs ys) (bisector xs ys) Nothing Nothing Nothing 0

valueOf :: Defuzz -> Int -> Maybe Double
valueOf d k = getAt Nothing [dCog d, dBis d, dMom d, dSom d, dLom d] k

-- グラフの枠
data Plot = Plot { pX, pY, pW, pH :: !Float }

plotOf :: Env -> Plot
plotOf env = Plot 40 175 (envW env * 0.6) (envH env - 300)

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 36 17 (hsb 0 0 95) ("Drag: peak (x = position, y = height)   1 2: select (now " ++ show (wSel w + 1) ++ ")   arrows: move   C: "
                               ++ (if wScaled w then "scale (product)" else "clip (min)") ++ "   R: reset")
  , txt 20 62 15 (hsb 0 0 70) "Same shape, different numbers depending on the method. Watch the flat top and the two peaks' sizes"
    -- 軸と目盛り
  , Color (hsba 0 0 100 0.25) (rectLineTL px py pw ph) ]
  ++ [ txt (xOf (fromIntegral x) - 4) (py + ph + 20) 14 (hsb 0 0 60) (show x) | x <- [round loX .. round hiX :: Int] ]
  ++ [ txt (px - 18) (py + 5) 14 (hsb 0 0 60) "1", txt (px - 18) (py + ph + 4) 14 (hsb 0 0 60) "0" ]
  -- もとの三角（うすく）
  ++ concat [ [ Color (hsba 0 0 100 (if k == wSel w then 0.55 else 0.25))
                  ((if k == wSel w then thickPath 2 else Line) [(xOf (ta t), yOf 0), (xOf (tb t), yOf 1), (xOf (tc t), yOf 0)])
              , Color (if k == wSel w then hsb 20 45 100 else hsb 0 0 70) (at (xOf (tb t), yOf (th t)) (disc 7)) ]
            | (k, t) <- zip [0 :: Int ..] (wTris w) ]
  -- 集約した形
  ++ [ fillUnder (hsba 260 30 90 0.3) [ (xOf x, yOf y) | (x, y) <- every 5 (zip xs ys) ]
     , Color (hsb 260 30 95) (thickPath 2.5 ([(xOf loX, yOf 0)] ++ [ (xOf x, yOf y) | (x, y) <- every 5 (zip xs ys) ] ++ [(xOf hiX, yOf 0)])) ]
  -- 面積二等分の左側を少し濃く
  ++ [ fillUnder (hsba 260 40 90 0.25) ([ (xOf x, yOf y) | (x, y) <- every 2 (takeWhile ((<= b) . fst) (zip xs ys)) ] ++ [(xOf b, yOf 0)])
     | Just b <- [dBis out] ]
  -- 各方法の位置
  ++ concat [ [ Color (hsb (mHue m) 60 100) (thickLine 2.5 (xOf v, py - 10 - fromIntegral k * 19) (xOf v, py + ph))
              , Color (hsb (mHue m) 60 100) (at (xOf v, py - 10 - fromIntegral k * 19) (disc 4.5))
              , txt (xOf v + 7) (py - 4 - fromIntegral k * 19) 14 (hsb (mHue m) 60 100) (mShort m) ]
            | (k, m) <- zip [0 :: Int ..] methods, Just v <- [valueOf out k] ]
  -- 右の表
  ++ [ txt tx ty 20 (hsb 0 0 95) ("Output values (range " ++ show (round loX :: Int) ++ " to " ++ show (round hiX :: Int) ++ ")") ]
  ++ concat [ let y = ty + 30 + fromIntegral k * 78 in
              [ Color (hsb (mHue m) 50 30) (rectTL tx y tw 66)
              , txt (tx + 14) (y + 26) 18 (hsb (mHue m) 60 100) (mName m)
              , txt (tx + tw - 110) (y + 28) 24 (hsb (mHue m) 60 100) (maybe "-" (fmt 3) (valueOf out k))
              , txt (tx + 14) (y + 52) 14 (hsb 0 0 80) (mDesc m) ]
            | (k, m) <- zip [0 :: Int ..] methods ]
  ++ [ txt tx yb 15 (hsb 0 0 80) ("area int mu dx = " ++ fmt 3 area ++ "   height = " ++ fmt 2 (dHeight out) ++ "  (" ++ show nPts ++ " points)") ]
  ++ [ txt tx (yb + 24) 15 (hsb 0 0 80) ("area left of the bisector = " ++ fmt 3 left ++ "   right = " ++ fmt 3 (area - left))
     | Just b <- [dBis out], let n = length (takeWhile (<= b) xs), let left = areaOf (take n xs) (take n ys) ]
  ++ [ txt tx (yb + 48) 14 (hsb 0 0 65) "The centroid is smooth and widely used."
     , txt tx (yb + 66) 14 (hsb 0 0 65) "MOM, SOM, LOM are fast but jump when"
     , txt tx (yb + 84) 14 (hsb 0 0 65) "the two peaks swap heights"
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    Plot px py pw ph = plotOf env
    xOf x = px + realToFrac ((x - loX) / (hiX - loX)) * pw
    yOf y = py + ph - realToFrac y * ph
    (xs, ys) = aggregate w
    out = defuzzAll xs ys
    area = areaOf xs ys
    tx = envW env * 0.66
    ty = 130
    tw = envW env - tx - 30
    yb = ty + 30 + 5 * 78 + 20
    -- 折れ線の下を塗る（gloss は凸の多角形しか塗れないので、細い台形を並べる）
    fillUnder col pts = Color col (Pictures [ Polygon [(x0, yOf 0), (x0, y0), (x1, y1), (x1, yOf 0)] | ((x0, y0), (x1, y1)) <- zip pts (drop 1 pts) ])

-- k 個おきに取り出す（最後の点は必ず入れる）
every :: Int -> [a] -> [a]
every k = go 0
  where
    go :: Int -> [a] -> [a]
    go _ [] = []
    go _ [z] = [z]
    go i (z : zs) = if i `mod` k == 0 then z : go (i + 1) zs else go (i + 1) zs

-- ---- 操作
-- 頂点 b を x へ（三角の幅はそのまま）、高さ h を y に
moveTo :: Double -> Double -> World -> World
moveTo x y w = w { wTris = [ if k == wSel w then mv t else t | (k, t) <- zip [0 ..] (wTris w) ] }
  where
    mv t = let wl = tb t - ta t
               wr = tc t - tb t
               b = clampv loX hiX x
           in Tri (b - wl) b (b + wr) (clampv 0.05 1 y)

selTri :: World -> Tri
selTri w = getAt (Tri 0 0 0 0) (wTris w) (wSel w)

dragTo :: Env -> World -> World
dragTo env w
  | mx < px || mx > px + pw || my < py - 20 || my > py + ph + 20 = w
  | otherwise = moveTo (loX + realToFrac ((mx - px) / pw) * (hiX - loX)) (realToFrac ((py + ph - my) / ph)) w
  where
    Plot px py pw ph = plotOf env
    (mx, my) = p5Mouse env

pressAt :: Env -> World -> World
pressAt env w = dragTo env w { wSel = pick }
  where
    Plot px py pw ph = plotOf env
    (mx, my) = p5Mouse env
    ds = [ (k, d) | (k, t) <- zip [0 ..] (wTris w)
                  , let d = sqrt ((mx - (px + realToFrac (tb t / hiX) * pw)) ^ (2 :: Int) + (my - (py + ph - realToFrac (th t) * ph)) ^ (2 :: Int)) ]
    pick = fst (foldl' (\(bk, bd) (k, d) -> if d < bd && d < 60 then (k, d) else (bk, bd)) (wSel w, 1e9) ds)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pressAt env w
  EventMotion _ | envDown env -> dragTo env w
  EventKey (Char '1') Down _ _ -> w { wSel = 0 }
  EventKey (Char '2') Down _ _ -> w { wSel = 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> moveTo (tb t - 0.1) (th t) w
  EventKey (SpecialKey KeyRight) Down _ _ -> moveTo (tb t + 0.1) (th t) w
  EventKey (SpecialKey KeyUp) Down _ _ -> moveTo (tb t) (th t + 0.05) w
  EventKey (SpecialKey KeyDown) Down _ _ -> moveTo (tb t) (th t - 0.05) w
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> w { wScaled = not (wScaled w) }
                             | c == 'r' || c == 'R' -> w { wTris = tris0 }
  _ -> w
  where t = selTri w

-- ---- 自己テスト：閉じた式（手で計算できる値）と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("symmetric triangle (2, 5, 8) clipped at 0.6 -> centroid = bisector = MOM = 5, SOM = 3.8, LOM = 6.2"
    , nm 1e-9 (dCog d1) 5 && nm 1e-9 (dBis d1) 5 && nm 1e-9 (dMom d1) 5 && nm 1e-9 (dSom d1) 3.8 && nm 1e-9 (dLom d1) 6.2)
  , ("centroid of triangle (0, 2, 10) = (0 + 2 + 10) / 3 = 4", nm 1e-9 (dCog d2) 4)
  , ("bisector of triangle (0, 2, 10) = 10 - sqrt 40 = 3.675 (right area (10 - x)^2 / 16 = 2.5)", nm 1e-6 (dBis d2) (10 - sqrt 40))
  , ("MOM = SOM = LOM of triangle (0, 2, 10) = the peak 2", nm 1e-9 (dMom d2) 2 && nm 1e-9 (dSom d2) 2 && nm 1e-9 (dLom d2) 2)
  , ("triangle (0, 2, 10) clipped at 0.5 = trapezoid (0, 1, 6, 10): centroid = polygon formula " ++ fmt 4 pc, nm 1e-9 (dCog d3) pc)
  , ("same trapezoid: MOM = 3.5, SOM = 1, LOM = 6", nm 1e-9 (dMom d3) 3.5 && nm 1e-9 (dSom d3) 1 && nm 1e-9 (dLom d3) 6)
  , ("all zero (no rule fired) -> no division by 0, 'no value'", isNothing' (dCog empty) && isNothing' (dBis empty) && isNothing' (dMom empty)) ]
  where
    gxs = [fromIntegral i / 100 | i <- [0 .. 1000 :: Int]]
    grid fn = defuzzAll gxs (map fn gxs)
    d1 = grid (\x -> min 0.6 (trimf x 2 5 8))
    d2 = grid (\x -> trimf x 0 2 10)
    d3 = grid (\x -> min 0.5 (trimf x 0 2 10))
    pc = polyCentroid [(0, 0), (1, 0.5), (6, 0.5), (10, 0)]
    empty = defuzzAll gxs (map (const 0) gxs)
    nm = nearM
    isNothing' = maybe True (const False)

-- 多角形（(x, y) の列、x 軸の上）の重心の x 座標：靴ひもの公式
polyCentroid :: [(Double, Double)] -> Double
polyCentroid ps = cx / (3 * a)
  where
    (a, cx) = foldl' (\(aa, cc) ((x0, y0), (x1, y1)) -> let cr = x0 * y1 - x1 * y0 in (aa + cr, cc + (x0 + x1) * cr))
                     (0, 0) (zip ps (drop 1 ps ++ take 1 ps))

-- ---------- やってみよう ----------
-- 1. 2 つの山の高さを同じにすると、MOM はどこへとぶ？ 重心は？
-- 2. 片方の山を左右にゆっくり動かして、5 つの値がなめらかに動くか見る
-- 3. C で「縮める」にすると、平らな部分がなくなる → SOM = MOM = LOM になる
-- 4. 山を離して置くと、重心が「どちらの山でもない谷」に来ることがある。これは良い？
-- 5. nPts を 101 に減らすと、重心の値はどれくらい変わる？

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

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠
rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

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

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 非ファジィ化（xs, ys は同じ長さ。ys は折れ線の高さ）
-- 重心 = ∫x μ dx ÷ ∫μ dx（折れ線の区間ごとに正確に積分）。どの規則も発火しなければ Nothing
centroid :: [Double] -> [Double] -> Maybe Double
centroid xs ys = if a > 1e-12 then Just (m / a) else Nothing
  where
    ps = zip xs ys
    (a, m) = foldl' step (0, 0) (zip ps (drop 1 ps))
    step (!aa, !mm) ((x0, y0), (x1, y1)) =
      let h = x1 - x0
      in (aa + (h * (y0 + y1)) / 2, mm + (h * (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1))) / 6)

areaOf :: [Double] -> [Double] -> Double
areaOf xs ys = foldl' (\acc ((x0, y0), (x1, y1)) -> acc + ((x1 - x0) * (y0 + y1)) / 2) 0 (zip ps (drop 1 ps))
  where ps = zip xs ys

-- 面積二等分：左右の面積が等しくなる x
bisector :: [Double] -> [Double] -> Maybe Double
bisector xs ys
  | aTot <= 1e-12 = Nothing
  | otherwise = Just (go 0 (zip ps (drop 1 ps)))
  where
    aTot = areaOf xs ys
    ps = zip xs ys
    lastX = foldl' (\_ x -> x) 0 xs
    go _ [] = lastX
    go acc (((x0, y0), (x1, y1)) : rest)
      | acc + a >= aTot / 2 =
          let need = aTot / 2 - acc
              k = (y1 - y0) / h          -- y0 t + k t²/2 = need を解く
              t | abs k < 1e-12 = if y0 > 0 then need / y0 else 0
                | otherwise = (-y0 + sqrt (max 0 (y0 * y0 + 2 * k * need))) / k
          in x0 + clampv 0 h t
      | otherwise = go (acc + a) rest
      where
        h = x1 - x0
        a = (h * (y0 + y1)) / 2

-- 最大値の点たち：平均（MOM）・いちばん左（SOM）・いちばん右（LOM）と高さ
maxima :: [Double] -> [Double] -> Maybe (Double, Double, Double, Double)
maxima xs ys
  | m <= 1e-12 = Nothing
  | otherwise = case top of
      (lo : _) -> Just (sum top / fromIntegral (length top), lo, foldl' (\_ x -> x) lo top, m)
      [] -> Nothing
  where
    m = foldl' max 0 ys
    top = [x | (x, y) <- zip xs ys, y >= m - 1e-9]

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 値がないこともある数（Nothing は不合格）
nearM :: Double -> Maybe Double -> Double -> Bool
nearM tol ma b = maybe False (\a -> nearTol tol a b) ma

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)
