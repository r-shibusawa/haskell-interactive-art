-- ============================================
-- 51-06 ブラックボックス
-- 中身の見えない箱に入力 u を入れ、出てくる出力 y を記録する。記録だけから箱の「式」を当てる（システム同定）
-- モデル（ARX）：y[k] = a₁y[k−1] + a₂y[k−2] + b₁u[k−1] + … + b₅u[k−5]。次数を小さい順に試し、係数は最小二乗法で。R で答え合わせ
-- 操作：左の帯でマウス上下 = 入力 u / A = 自動の入力（ランダムな高さの階段） / R = 正体を見る / N = 新しい箱
--       C = 記録を消す / W = 雑音
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import Data.List (find, foldl', sortOn)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nA, nB :: Int
nA = 2                     -- ★ モデルの次数：過去の出力 2 つ
nB = 5                     -- ★ 過去の入力 5 つ

maxRec :: Int
maxRec = 400               -- 記録する点の数

kinds :: [String]
kinds = ["1st-order lag", "2nd order (rings)", "dead time", "nonlinear"]

-- 箱の中身（4 種類のどれか）。θ = その箱を ARX で書いたときの本当の係数 [a1, a2, b1…b5]
data Box = Box { bKind :: !Int, bTheta :: Maybe [Double] }

-- 同定の結果
data Fit = Fit { fTh :: [Double], fNa, fNb, fN :: !Int, fRms, fYs, fPct :: !Double }

data World = World
  { wBox    :: !Box
  , wU      :: Seq Double        -- 記録した入力
  , wY      :: Seq Double        -- 記録した出力
  , wAutoIn :: !Bool
  , wReveal :: !Bool
  , wNoise  :: !Bool
  , wUNow   :: !Double
  , wRng    :: !Word32
  , wFit    :: Maybe Fit
  , wBoxN   :: !Int
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-06 Black box" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newBox (World (makeBox 0) Seq.empty Seq.empty True False False 0 2024 Nothing 0 [])

makeBox :: Int -> Box
makeBox k = case k of
  0 -> Box 0 (Just [0.9, 0, 0.2, 0, 0, 0, 0])
  1 -> Box 1 (Just [1.6, -0.81, 0.25, 0, 0, 0, 0])
  2 -> Box 2 (Just [0.8, 0, 0, 0, 0, 0.3, 0])
  _ -> Box 3 Nothing

-- 箱の式：hy !! i = y[k−i]、hu !! i = u[k−i]
boxF :: Int -> U.Vector Double -> U.Vector Double -> Double
boxF k hy hu = case k of
  0 -> 0.9 * y 1 + 0.2 * u 1
  1 -> 1.6 * y 1 - 0.81 * y 2 + 0.25 * u 1
  2 -> 0.8 * y 1 + 0.3 * u 4
  _ -> 0.85 * y 1 + 0.3 * tanh (3 * u 1)
  where y i = hy U.! i
        u i = hu U.! i

newBox :: World -> World
newBox w = clearRec w { wBoxN = wBoxN w + 1, wBox = makeBox k, wReveal = False, wRng = g }
  where (k, g) = randInt 4 (wRng w)

clearRec :: World -> World
clearRec w = w { wU = Seq.empty, wY = Seq.empty, wFit = Nothing }

-- 箱を 1 きざみ動かす
boxStep :: Box -> Seq Double -> Seq Double -> Double -> Double -> (Seq Double, Seq Double)
boxStep b us ys uNew noise = (us', ys |> clampv (-50) 50 (fin 0 y))
  where
    us' = us |> uNew
    k = Seq.length us' - 1
    hy = U.generate 6 (\i -> if k - i >= 0 && k - i < Seq.length ys then Seq.index ys (k - i) else 0)
    hu = U.generate 6 (\i -> if k - i >= 0 then Seq.index us' (k - i) else 0)
    y = boxF (bKind b) hy hu + noise

-- 入力の列をまとめて箱に通す（自己テスト・ステップ応答用）
runBox :: Box -> [Double] -> [Double]
runBox b us = toList (snd (foldl' (\(a, c) u -> boxStep b a c u 0) (Seq.empty, Seq.empty) us))

-- ---- 同定：記録から行（説明変数）を作って最小二乗。次数 (na, nb) は小さい順に試す
-- （1 次遅れの箱に 2 次のモデルを使うと、y[k−1] が y[k−2] と u[k−2] で書けてしまい式が 1 つに決まらない）
arxRows :: U.Vector Double -> U.Vector Double -> Int -> Int -> ([[Double]], [Double])
arxRows us ys na nb = unzip
  [ ([ ys U.! (k - i) | i <- [1 .. na] ] ++ [ us U.! (k - i) | i <- [1 .. nb] ], ys U.! k)
  | k <- [max nA nB .. U.length ys - 1] ]

fitOrder :: U.Vector Double -> U.Vector Double -> Int -> Int -> Maybe Fit
fitOrder us ys na nb
  | length xs < 20 = Nothing
  | otherwise = case lsqFit xs t of
      Nothing -> Nothing
      Just c ->
        let n = fromIntegral (length t)
            my = sum t / n
            res = foldl' (+) 0 [ (ti - sum (zipWith (*) r c)) ^ (2 :: Int) | (r, ti) <- zip xs t ]
            sy = foldl' (+) 0 [ (ti - my) ^ (2 :: Int) | ti <- t ]
            -- 7 個の並びにそろえる（使わない係数は 0）
            th = [ if i < na then getAt 0 c i else 0 | i <- [0 .. nA - 1] ] ++ [ if i < nb then getAt 0 c (na + i) else 0 | i <- [0 .. nB - 1] ]
        in Just (Fit th na nb (length t) (sqrt (res / n)) (sqrt (sy / n)) (if sy > 0 then 100 * (1 - sqrt (res / sy)) else 0))
  where (xs, t) = arxRows us ys na nb

identify :: U.Vector Double -> U.Vector Double -> Maybe Fit
identify us ys = case cands of
  [] -> Nothing
  (c0 : _) -> case find (\f -> fRms f <= 1e-9 * max (fYs f) 1e-12) cands of   -- 誤差がほぼ 0 になる一番小さいモデル
    Just f -> Just f
    Nothing -> Just (foldl' (\m f -> if aic f < aic m then f else m) c0 cands)  -- なければ AIC が最小のもの
  where
    cands = sortOn (\f -> fNa f + fNb f) [ f | na <- [1 .. nA], nb <- [1 .. nB], Just f <- [fitOrder us ys na nb] ]
    aic f = fromIntegral (fN f) * log (fRms f * fRms f + 1e-300) + 2 * fromIntegral (fNa f + fNb f)

-- モデルだけで動かした出力（入力は記録と同じ）
simulateModel :: [Double] -> [Double] -> [Double]
simulateModel th us = go 0 [] [] us
  where
    uv = U.fromList us
    go :: Int -> [Double] -> [Double] -> [Double] -> [Double]
    go _ _ _ [] = []
    go k ysPrev acc (_ : rest) =
      let y = sum [ getAt 0 th (i - 1) * getAt 0 ysPrev (i - 1) | i <- [1 .. nA], k - i >= 0 ]
            + sum [ getAt 0 th (nA + i - 1) * uv U.! (k - i) | i <- [1 .. nB], k - i >= 0 ]
          y' = clampv (-50) 50 (fin 0 y)
      in y' : go (k + 1) (y' : take 4 ysPrev) acc rest

-- 入力の帯の位置
stripAt :: Env -> (Float, Float, Float, Float)
stripAt env = (30, 110, 60, envH env - 200)

step :: Env -> World -> World
step env w
  | envFrame env `mod` 2 /= 0 = w
  | otherwise =
      let (sx, sy, sw, sh) = stripAt env
          (mx, my) = p5Mouse env
          (uN, g1)
            | wAutoIn w = let (a, g) = rand (wRng w)
                          in if a < 0.15 then let (b, g') = rand g in (2 * b - 1, g') else (wUNow w, g)
            | mx > sx - 20 && mx < sx + sw + 20 && my > sy && my < sy + sh = (clampv (-1) 1 (1 - 2 * realToFrac (my - sy) / realToFrac sh), wRng w)
            | otherwise = (wUNow w, wRng w)
          (noise, g2) = if wNoise w then let (c, g') = rand g1 in (0.02 * (2 * c - 1), g') else (0, g1)
          (us, ys) = boxStep (wBox w) (wU w) (wY w) uN noise
          (us', ys') = if Seq.length us > maxRec then (Seq.drop 1 us, Seq.drop 1 ys) else (us, ys)
          fit = if envFrame env `mod` 10 == 0 then identify (U.fromList (toList us')) (U.fromList (toList ys')) else wFit w
      in w { wUNow = uN, wRng = g2, wU = us', wY = ys', wFit = fit }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Black box - guess the equation from inputs and outputs only (box #" ++ show (wBoxN w) ++ ")") 20 34 20 cTxt
  , say ("Mouse up/down on the left strip = input   A auto input " ++ onOff (wAutoIn w)) 20 60 15 cSub
  , say ("R " ++ (if wReveal w then "hide" else "reveal") ++ " the truth   N new box   C clear record   W noise " ++ onOff (wNoise w)) 20 82 15 cSub
    -- ---- 入力の帯
  , Color (hsb 28 30 18) (rectTL sx sy sw sh)
  , Color (col cTube 100) (rectTL sx (uy - 6) sw 12)
  , say ("u = " ++ fmt 2 (wUNow w)) (sx - 6) (sy + sh + 24) 14 cTube
    -- ---- 箱
  , Color (if wReveal w then hsb 30 30 22 else hsb 0 0 4) (rectTL bx by bw bh)
  , Color (col cBrass 100) (frameTL 3 bx by bw bh)
  , if wReveal w then sayC (getAt "" kinds (bKind (wBox w))) (bx + bw / 2) (by + bh / 2) 18 cTxt
                 else sayC "?" (bx + bw / 2) (by + bh / 2) 40 cSub
  , flowArrow (sx + sw) uy bx (by + bh / 2) cTube ph (Just "u")
  , flowArrow (bx + bw) (by + bh / 2) (bx + bw + 50) (by + bh / 2) cScope ph (Just "y")
    -- ---- 記録と、モデルだけで動かした出力
  , plotFrame gx gy gw gh "Input u (+-1)"
  , plotLine uList gx gy gw gh (-1.2) 1.2 cTube maxRec 1.5
  , plotFrame gx gy2 gw gh "Output y (green) vs model run on u only (orange)"
  , plotLine yList gx gy2 gw gh (-ymax) ymax cScope maxRec 2 ]
  ++ [ plotLine (simulateModel (fTh f) uList) gx gy2 gw gh (-ymax) ymax cTube maxRec 1.5 | Just f <- [wFit w] ]
    -- ---- 推定した係数
  ++ [ say ("Least squares from " ++ show (Seq.length (wY w)) ++ " points") tx ty 17 cTxt ]
  ++ coefs
    -- ---- 階段の入力への応答（ステップ応答）
  ++ [ plotFrame tx sy2 sw2 sh2 ("Step u = 1: orange = model" ++ (if wReveal w then ", green = true" else "")) ]
  ++ [ plotLine (runBox (wBox w) (replicate 60 1)) tx sy2 sw2 sh2 (-0.5) 3 cScope 60 2.5 | wReveal w ]
  ++ [ plotLine (simulateModel (fTh f) (replicate 60 1)) tx sy2 sw2 sh2 (-0.5) 3 cTube 60 1.8 | Just f <- [wFit w] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    (sx, sy, sw, sh) = stripAt env
    uy = sy + f2 ((1 - wUNow w) / 2) * sh
    bx = sx + sw + 50
    by = sy
    bw = 200
    bh = 120
    ph = fract (fromIntegral (envFrame env) / 60)
    gx = bx
    gy = by + bh + 40
    gw = envW env * 0.55 - gx
    gh = (envH env - gy - 60) / 2 - 10
    gy2 = gy + gh + 20
    uList = toList (wU w)
    yList = toList (wY w)
    ymax = maximum (1 : map abs yList) * 1.1
    tx = envW env * 0.58
    ty = 110
    sy2 = ty + 300
    sw2 = envW env - tx - 30
    sh2 = envH env - sy2 - 50
    names = ["a1", "a2", "b1", "b2", "b3", "b4", "b5"]
    sgn v = (if v >= 0 then " " else "") ++ fmt 6 v
    coefs = case wFit w of
      Nothing -> [ say "Not enough input: move it more or press A" tx (ty + 30) 15 cAlarm
                 , say "(the same input all the time fixes nothing)" tx (ty + 52) 14 cAlarm ]
      Just f ->
        [ say ("chosen order na = " ++ show (fNa f) ++ ", nb = " ++ show (fNb f)) tx (ty + 24) 15 cTxt
        , say ("coef   estimate   " ++ (if wReveal w then "  true" else "(R: true values)")) tx (ty + 50) 15 cSub ]
        ++ [ say (nm ++ "  " ++ sgn e ++ (case tr of
                                             Just t -> "  " ++ sgn t ++ "  diff " ++ showEFloat (Just 1) (abs (e - t)) ""
                                             Nothing -> "")) tx (ty + 74 + fromIntegral i * 22) 14 (if abs e > 0.02 then cTube else cSub)
           | (i, nm, e) <- zip3 [0 :: Int ..] names (fTh f)
           , let tr = if wReveal w then fmap (\th -> getAt 0 th i) (bTheta (wBox w)) else Nothing ]
        ++ [ say ("1-step error RMS = " ++ showEFloat (Just 2) (fRms f) "" ++ "   fit " ++ fmt 2 (fPct f) ++ "%") tx (ty + 246) 15 cScope ]
        ++ [ say "Nonlinear: a linear ARX cannot describe it" tx (ty + 272) 14 cAlarm | wReveal w, bTheta (wBox w) == Nothing ]
        ++ [ say (if wNoise w then "With noise the estimate is a little off" else "No noise, model has the right form -> exact")
                 tx (ty + 272) 14 cScope | wReveal w, bTheta (wBox w) /= Nothing ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAutoIn = not (wAutoIn w) }
    | c `elem` "rR" -> w { wReveal = not (wReveal w) }
    | c `elem` "nN" -> newBox w
    | c `elem` "cC" -> clearRec w
    | c `elem` "wW" -> w { wNoise = not (wNoise w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 8 項目）
-- 種 seed の乱数で、ときどき高さを変える階段の入力を作って箱に通す（uFn があればそれを入力に）
record :: Int -> Int -> Word32 -> Maybe (Int -> Double) -> (Box, [Double], [Double])
record kind n seed uFn = (b, us, runBox b us)
  where
    b = makeBox kind
    us = go 0 seed 1
    go k g u
      | k >= n = []
      | otherwise = case uFn of
          Just f -> f k : go (k + 1) g u
          Nothing -> let (a, g1) = rand g
                         (u', g2) = if a < 0.3 then let (c, g') = rand g1 in (2 * c - 1, g') else (u, g1)
                     in u' : go (k + 1) g2 u'

identifyL :: [Double] -> [Double] -> Maybe Fit
identifyL us ys = identify (U.fromList us) (U.fromList ys)

maxErr :: Maybe Fit -> Box -> Double
maxErr mf b = case (mf, bTheta b) of
  (Just f, Just th) -> maximum (0 : zipWith (\v t -> abs (v - t)) (fTh f) th)
  _ -> 1 / 0

selfTest :: [(String, Bool)]
selfTest =
  [ (getAt "" kinds k ++ ": 200 clean points give coefficients with error " ++ showEFloat (Just 1) e "" ++ " (< 1e-6)", e < 1e-6)
  | k <- [0 .. 2], let (b, us, ys) = record k 200 (7 + fromIntegral k) Nothing, let e = maxErr (identifyL us ys) b ]
  ++
  [ ("nonlinear box: ARX keeps a 1-step error (RMS " ++ maybe "-" (\f -> showEFloat (Just 1) (fRms f) "") fn ++ " > 1e-3)", maybe False (\f -> fRms f > 1e-3) fn)
  , ("input always 0 (output 0) -> nothing to learn, Nothing (no NaN)", case identifyL u0 y0 of { Nothing -> True; Just _ -> False })
  , ("input repeating +-1: still exact for the 1st-order lag (< 1e-6)", maxErr (identifyL u1 y1) b1 < 1e-6)
  , ("model with the true coefficients = box output (diff < 1e-12)", and (zipWith (\a c -> abs (a - c) < 1e-12) ysim yt) && length ysim == length yt)
  , ("2nd-order step response settles at 0.25 / (1 - 1.6 + 0.81) = 1.1905", near (getAt 0 (simulateModel th2 (replicate 600 1)) 599) (0.25 / 0.21) 1e-6) ]
  where
    (_, nlU, nlY) = record 3 200 11 Nothing
    fn = identifyL nlU nlY
    (_, u0, y0) = record 2 200 1 (Just (const 0))
    (b1, u1, y1) = record 0 200 1 (Just (\k -> if odd k then 1 else -1))
    (b2, u2, _) = record 1 100 3 Nothing
    th2 = maybe [] id (bTheta b2)
    ysim = simulateModel th2 u2
    yt = runBox b2 u2

-- ---------- やってみよう ----------
-- 1. A を切って、左の帯で自分で入力を作る。ゆっくり動かすだけだと「入力が足りない」になる？ どんな入力だと当たりやすい？
-- 2. 当てた係数を見て、箱の種類を予想してから R で答え合わせ。むだ時間の箱では b のどこが大きい？
-- 3. W で雑音を入れると、推定はどれくらいずれる？ 記録が長いほど良くなる？
-- 4. 非線形の箱で、入力を小さく（±0.2 くらい）すると、ARX のあてはまりはどうなる？
-- 5. nB を 2 にすると、むだ時間の箱は当てられる？ モデルの「形」が合っていないとどうなるか

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- 信号の矢印：点が流れて「ループを回る」ようすを見せる（phase = 0〜1）
flowArrow :: Float -> Float -> Float -> Float -> HSB -> Float -> Maybe String -> Picture
flowArrow x1 y1 x2 y2 c phase lab = Pictures $
  [ Color (col c 100) (thickLine 2 (x1, y1) (x2, y2))
  , Color (col c 100) (Polygon [(x2, y2), (x2 - 11 * cos (a - 0.4), y2 - 11 * sin (a - 0.4)), (x2 - 11 * cos (a + 0.4), y2 - 11 * sin (a + 0.4))]) ]
  ++ [ Color (col c 100) (at (x1 + (x2 - x1) * f, y1 + (y2 - y1) * f) (disc 3)) | k <- [0, 1, 2], let f = fract (phase + k / 3) ]
  ++ [ say s ((x1 + x2) / 2 + 6) ((y1 + y2) / 2 - 6) 14 cSub | Just s <- [lab] ]
  where a = atan2 (y2 - y1) (x2 - x1)

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
rand :: Word32 -> (Double, Word32)
rand a0 = (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, a)
  where
    a = a0 + 0x6d2b79f5
    t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
    t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))

-- 0〜n−1 の整数（Math.floor(rng() * n)）
randInt :: Int -> Word32 -> (Int, Word32)
randInt n g = let (u, g') = rand g in (min (n - 1) (floor (u * fromIntegral n)), g')

-- ---- 最小二乗：X c ≈ y となる c（正規方程式 XᵀX c = Xᵀy を解く）
lsqFit :: [[Double]] -> [Double] -> Maybe [Double]
lsqFit xs ys = case xs of
  [] -> Nothing
  (r0 : _) ->
    let n = length r0
        a = [ [ foldl' (+) 0 [ getAt 0 r i * getAt 0 r j | r <- xs ] | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
        b = [ foldl' (+) 0 [ getAt 0 r i * y | (r, y) <- zip xs ys ] | i <- [0 .. n - 1] ]
    in solveLinear a b

-- 連立 1 次方程式 A x = b（部分ピボットのガウス消去）。解けない（情報が足りない）ときは Nothing
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a0 b0
  | not (scale0 > 0) = Nothing
  | otherwise = elim 0 (zipWith (\r v -> r ++ [v]) a0 b0)
  where
    n = length b0
    scale0 = maximum (0 : map abs (concat a0))
    -- 行は [係数…, 右辺]
    elim c m
      | c >= n = back m
      | otherwise =
          let rest = drop c m
              p = c + snd (foldl' (\(bv, bi) (i, r) -> if abs (getAt 0 r c) > bv then (abs (getAt 0 r c), i) else (bv, bi))
                                  (abs (getAt 0 (getAt [] rest 0) c), 0) (zip [0 ..] rest))
              piv = getAt [] m p
          in if abs (getAt 0 piv c) < 1e-11 * scale0 then Nothing else
             let m1 = [ if i == c then piv else if i == p then getAt [] m c else r | (i, r) <- zip [0 ..] m ]
                 m2 = [ if i <= c then r else
                          let f = getAt 0 r c / getAt 0 piv c
                          in [ if k < c then v else v - f * getAt 0 piv k | (k, v) <- zip [0 ..] r ]
                      | (i, r) <- zip [0 :: Int ..] m1 ]
             in elim (c + 1) m2
    back m =
      let x = foldr (\(r, row) acc -> let s = getAt 0 row n - sum [ getAt 0 row k * xk | (k, xk) <- zip [r + 1 ..] acc ]
                                      in s / getAt 0 row r : acc) [] (zip [0 ..] m)
      in if all (\v -> not (isNaN v || isInfinite v)) x then Just x else Nothing

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
