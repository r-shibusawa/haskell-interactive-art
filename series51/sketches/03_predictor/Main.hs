-- ============================================
-- 51-03 予測して狙う
-- 弾が届くまでに標的は動いてしまう。だから「過去の位置」から「未来の位置」を予測して、そこを狙う
-- 過去 N 個の位置に多項式を最小二乗であてはめ、弾の飛ぶ時間だけ先を計算する（ウィーナーが戦争中に取り組んだ対空射撃の予測の発想）
-- 操作：↑↓ = 予測の次数（0〜3） / ←→ = 使う過去の点の数 N / 1 2 = 弾の速さ（先読み時間が変わる） / M = 標的をマウスで動かす
--       スペース・クリック = 撃つ / A = 自動で撃つ / R = 数をリセット
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl', intercalate)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

tS :: Double
tS = 0.1             -- ★ 位置を記録する間隔 [秒]

hitR :: Double
hitR = 26            -- 標的の半径 [px]

-- 予測の設定：多項式の次数（0 = 平均、1 = 直線、2 = 放物線…）・使う過去の点の数・弾の速さ [px/s]
data Cfg = Cfg { cOrder, cN :: !Int, cV :: !Double }

cfg0 :: Cfg
cfg0 = Cfg 2 8 900   -- ★ はじめの設定（弾が遅いほど先まで予測しないといけない）

-- 飛んでいる弾（k = 0 予測あり、1 今の位置を狙う）
data Shell = Shell { sX, sY :: !Double, sVx, sVy :: !Double, sK :: !Int, sHit :: !Bool }

-- ゲームの状態：記録した点、飛んでいる弾、成績
data Game = Game
  { gW, gH    :: !Double
  , gT, gLastS :: !Double
  , gSamples  :: [(Double, Double)]   -- 記録した点（古い順、40 個まで）
  , gShells   :: [Shell]
  , gShots, gHits :: !(Int, Int)
  , gFireT    :: !Double
  , gTp       :: !(Double, Double)
  , gAim      :: Maybe (Double, Double) }

data World = World
  { wCfg   :: !Cfg
  , wGame  :: !Game
  , wMouse :: !Bool
  , wAuto  :: !Bool
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-03 Predictor" (World cfg0 (makeGame 1280 800) False True []))
  { skBack  = col (24, 35, 10) 100
  , skStart = \env -> do
      printSelfTest selfTest
      pure (World cfg0 (makeGame (realToFrac (envW env)) (realToFrac (envH env))) False True selfTest)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- ---- 予測：過去の点 (t = −N+1, …, −1, 0) に次数 p の多項式をあてはめて、t = ahead の値を出す
fitPoly :: [Double] -> Int -> Maybe [Double]
fitPoly vals p
  | n < p + 1 = Nothing
  | otherwise = lsqFit [ [ fromIntegral (i - (n - 1)) ^ k | k <- [0 .. p] ] | i <- [0 .. n - 1] ] vals
  where n = length vals

evalPoly :: [Double] -> Double -> Double
evalPoly c t = foldr (\ck s -> s * t + ck) 0 c

predictAt :: [(Double, Double)] -> Int -> Double -> Maybe (Double, Double)
predictAt samples p aheadSec = do
  cx <- fitPoly (map fst samples) p
  cy <- fitPoly (map snd samples) p
  pure (evalPoly cx (aheadSec / tS), evalPoly cy (aheadSec / tS))

-- 標的の自動の動き（リサジュー曲線）
autoTarget :: Double -> Double -> Double -> (Double, Double)
autoTarget t w h = (w * 0.5 + w * 0.33 * sin (0.45 * t), h * 0.38 + h * 0.2 * sin (0.75 * t + 0.5))

makeGame :: Double -> Double -> Game
makeGame w h = Game w h 0 (-1) [] [] (0, 0) (0, 0) 0.5 (w / 2, h / 3) Nothing

gunPos :: Game -> (Double, Double)
gunPos g = (gW g * 0.5, gH g - 70)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- 狙う点：飛ぶ時間 = 距離 ÷ 弾の速さ をくり返し計算（5 回でほぼ動かなくなる）
predictiveAim :: Cfg -> Game -> Maybe (Double, Double)
predictiveAim cfg g
  | length (gSamples g) < cN cfg = Nothing
  | otherwise = go (5 :: Int) 0 Nothing
  where
    s = lastN (cN cfg) (gSamples g)
    (gx, gy) = gunPos g
    since = gT g - gLastS g
    go 0 _ pp = pp
    go k fly _ = case predictAt s (cOrder cfg) (since + fly) of
      Nothing -> Nothing
      Just pt@(px, py) -> go (k - 1) (sqrt ((px - gx) ^ (2 :: Int) + (py - gy) ^ (2 :: Int)) / cV cfg) (Just pt)

fire :: Cfg -> Game -> Game
fire cfg g = g { gShells = gShells g ++ newShells, gShots = (s0 + length [() | Shell { sK = 0 } <- newShells], s1 + length [() | Shell { sK = 1 } <- newShells]), gAim = pa }
  where
    (gx, gy) = gunPos g
    pa = predictiveAim cfg g
    (s0, s1) = gShots g
    newShells = [ Shell gx gy (cV cfg * (ax - gx) / d) (cV cfg * (ay - gy) / d) k False
                | (k, Just (ax, ay)) <- zip [0, 1] [pa, Just (gTp g)]
                , let d0 = sqrt ((ax - gx) ^ (2 :: Int) + (ay - gy) ^ (2 :: Int)), let d = if d0 == 0 then 1 else d0 ]

gameStep :: Cfg -> Double -> (Double, Double) -> Bool -> Game -> Game
gameStep cfg dt tp auto g0 =
  let g1 = g0 { gT = gT g0 + dt, gTp = tp }
      g2 = if gT g1 - gLastS g1 >= tS - 1e-9 then g1 { gSamples = lastN 40 (gSamples g1 ++ [tp]), gLastS = gT g1 } else g1
      moved = [ let x = sX s + sVx s * dt
                    y = sY s + sVy s * dt
                    hit = not (sHit s) && sqrt ((x - fst tp) ^ (2 :: Int) + (y - snd tp) ^ (2 :: Int)) < hitR
                in s { sX = x, sY = y, sHit = sHit s || hit } | s <- gShells g2 ]
      newHits = [ sK s | (s, s') <- zip (gShells g2) moved, not (sHit s), sHit s' ]
      (h0, h1) = gHits g2
      g3 = g2 { gShells = [ s | s <- moved, not (sHit s), sX s > -50, sX s < gW g2 + 50, sY s > -50, sY s < gH g2 + 50 ]
              , gHits = (h0 + length (filter (== 0) newHits), h1 + length (filter (== 1) newHits)) }
  in if auto then (let ft = gFireT g3 - dt in if ft <= 0 then fire cfg g3 { gFireT = 0.6 } else g3 { gFireT = ft }) else g3

step :: Env -> World -> World
step env w = w { wGame = foldl' (\g _ -> gameStep (wCfg w) (1 / 240) tp (wAuto w) g) g0 [1 .. 4 :: Int] }
  where
    g0 = (wGame w) { gW = realToFrac (envW env), gH = realToFrac (envH env) }
    (mx, my) = p5Mouse env
    tp = if wMouse w then (realToFrac mx, realToFrac my) else autoTarget (gT g0) (gW g0) (gH g0)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Predict and aim - fit the past by least squares, aim at the future" 20 36 20 cTxt
  , say ("Up/Down order " ++ show ord ++ "   Left/Right points N = " ++ show nn ++ "   1 2 shell speed " ++ num (cV cfg)
         ++ "   M target " ++ (if wMouse w then "mouse" else "auto")) 20 60 15 cSub
  , say ("Space/click fire   A auto fire " ++ (if wAuto w then "ON" else "OFF") ++ "   R reset   orange = predicted, gray = aim at now") 20 82 15 cSub ]
  -- 記録した点と、あてはめた曲線の先
  ++ [ Color (col cScope 70) (at (f2 qx, f2 qy) (disc 4)) | (qx, qy) <- s ]
  ++ [ Color (col cTube 70) (thickPath 2 [ (f2 (clampv (-100) (sw + 100) px), f2 (clampv (-100) (sh + 100) py))
                                         | a <- curveTs, Just (px, py) <- [predictAt s ord a] ])
     | length s >= nn, length s >= ord + 1 ]
  -- 標的
  ++ [ Color (col cTxt 100) (Pictures [at (tx, ty) (ring (f2 hitR) 2), thickLine 2 (tx - 8, ty) (tx + 8, ty)]) ]
  ++ [ Color (col cTube 100) (Pictures [ Line [(ax - 10, ay - 10), (ax + 10, ay + 10)], Line [(ax - 10, ay + 10), (ax + 10, ay - 10)] ])
     | Just (ax0, ay0) <- [gAim g], let ax = f2 ax0, let ay = f2 ay0 ]
  -- 砲台と弾
  ++ [ Color (col cBrass 100) (rectTL (f2 gpx - 30) (f2 gpy) 60 30) ]
  ++ [ if sK sh == 1 then Color (hsb 0 0 70) (at (f2 (sX sh), f2 (sY sh)) (disc 4)) else Color (col cTube 100) (at (f2 (sX sh), f2 (sY sh)) (disc 5))
     | sh <- gShells g ]
  -- 成績
  ++ [ say ("Predicted (order " ++ show ord ++ ", N = " ++ show nn ++ "): " ++ show h0 ++ " / " ++ show s0 ++ " hits = " ++ rate h0 s0) hx hy 17 cTube
     , say ("Aim at now: " ++ show h1 ++ " / " ++ show s1 ++ " hits = " ++ rate h1 s1) hx (hy + 26) 17 (0, 0, 75)
     , say (case predictiveAim cfg g of
              Just (px, py) -> "Aim point = (" ++ fmt 0 px ++ ", " ++ fmt 0 py ++ ")   flight time " ++ fmt 2 (sqrt ((px - gpx) ^ (2 :: Int) + (py - gpy) ^ (2 :: Int)) / cV cfg) ++ " s"
              Nothing -> "Not enough points (wait for N samples)") hx (hy + 52) 15 cTxt ]
  ++ [ say ("x(t) ~ " ++ intercalate " + " [ fmt 1 c ++ (if k > 0 then " t" ++ (if k > 1 then "^" ++ show k else "") else "") | (k, c) <- zip [0 :: Int ..] cx ]
            ++ "   (t = sample index, now = 0)") hx (hy + 78) 14 cSub
     | length s >= ord + 1, Just cx <- [fitPoly (map fst s) ord] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    cfg = wCfg w
    ord = cOrder cfg
    nn = cN cfg
    g = wGame w
    s = lastN nn (gSamples g)
    sw = realToFrac (envW env)
    sh = realToFrac (envH env)
    curveTs = takeWhile (<= 1.2) (iterate (+ 0.05) (negate (fromIntegral (nn - 1)) * tS))
    (tx, ty) = (f2 (fst (gTp g)), f2 (snd (gTp g)))
    (gpx, gpy) = gunPos g
    (s0, s1) = gShots g
    (h0, h1) = gHits g
    rate h n = if n > 0 then fmt 1 (100 * fromIntegral h / fromIntegral n) ++ "%" else "-"
    hx = 20
    hy = envH env - 150

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> fixN w { wCfg = cfg { cOrder = min 3 (cOrder cfg + 1) } }
  EventKey (SpecialKey KeyDown) Down _ _ -> fixN w { wCfg = cfg { cOrder = max 0 (cOrder cfg - 1) } }
  EventKey (SpecialKey KeyRight) Down _ _ -> fixN w { wCfg = cfg { cN = min 20 (cN cfg + 1) } }
  EventKey (SpecialKey KeyLeft) Down _ _ -> fixN w { wCfg = cfg { cN = max (cOrder cfg + 1) (cN cfg - 1) } }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wGame = fire cfg (wGame w) }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wGame = fire cfg (wGame w) }
  EventKey (Char c) Down _ _
    | c == '1' -> w { wCfg = cfg { cV = max 300 (cV cfg - 100) } }
    | c == '2' -> w { wCfg = cfg { cV = min 1500 (cV cfg + 100) } }
    | c `elem` "mM" -> w { wMouse = not (wMouse w) }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> w { wGame = (wGame w) { gShots = (0, 0), gHits = (0, 0) } }
  _ -> w
  where
    cfg = wCfg w
    fixN v = let c = wCfg v in v { wCfg = c { cN = max (cN c) (cOrder c + 1) } }

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("order 1: line x = 3 + 2t predicted exactly as 13 at t = 5", maybe False (\c -> near (evalPoly c 5) 13 1e-9) (fitPoly line5 1))
  , ("order 2: parabola 1 - t + t^2/2 exactly 5 at t = 4", near (evalPoly q2 4) 5 1e-9)
  , ("order 1 misses the parabola (error " ++ fmt 2 (abs (evalPoly q1 4 - 5)) ++ ")", abs (evalPoly q1 4 - 5) > 1)
  , ("order 0 prediction = mean of the points", maybe False (\c -> near (evalPoly c 7) 5 1e-12) (fitPoly [2, 4, 9] 0))
  , ("too few points (order 3 on 3 points) -> nothing (no NaN)", case fitPoly [1, 2, 3] 3 of { Nothing -> True; _ -> False })
  , ("auto target for 40 s: predicted " ++ fmt 0 (100 * r0) ++ "% > aim at now " ++ fmt 0 (100 * r1) ++ "%, predicted >= 80%", r0 > r1 && r0 >= 0.8) ]
  where
    line5 = [ 3 + 2 * (i - 4) | i <- [0 .. 4] ]
    quad = [ 1 - t + 0.5 * t * t | i <- [0 .. 5], let t = i - 5 ]
    q2 = maybe [] id (fitPoly quad 2)
    q1 = maybe [] id (fitPoly quad 1)
    g = foldl' (\gg _ -> gameStep cfg0 (1 / 240) (autoTarget (gT gg) 1280 800) True gg) (makeGame 1280 800) [1 .. 240 * 40 :: Int]
    ratio (a, b) = if b > 0 then fromIntegral a / fromIntegral b else 0 :: Double
    r0 = ratio (fst (gHits g), fst (gShots g))
    r1 = ratio (snd (gHits g), snd (gShots g))

-- ---------- やってみよう ----------
-- 1. ↓ で次数 0（平均）・1（直線）・2（放物線）と変え、命中率を比べよう
-- 2. ←→ で N を変える。少なすぎ／多すぎると何が困る？（曲がり方が変わると古い点がじゃまになる）
-- 3. M でマウスで標的を動かし、急に向きを変えてみよう。予測は人の「気まぐれ」を読めるか？
-- 4. 1 で弾を遅くすると（先読み時間が長くなる）、命中率はどうなる？ 予測する時間が長いほど外れやすい理由は？
-- 5. 標的が「予測されている」と知っていたら、どう動けばよいか考えよう（12 の「互いを観察する系」へ）

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

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

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- ---- 最小二乗：X c ≈ y の c（正規方程式 XᵀX c = Xᵀy を解く）。解けないときは Nothing
lsqFit :: [[Double]] -> [Double] -> Maybe [Double]
lsqFit xs ys = case xs of
  [] -> Nothing
  (r0 : _) ->
    let n = length r0
        zero = replicate n (replicate n 0)
        addRow (!a, !b) (row, y) = ( zipWith (\ai xi -> zipWith (\aij xj -> aij + xi * xj) ai row) a row
                                   , zipWith (\bi xi -> bi + xi * y) b row )
        (am, bv) = foldl' addRow (zero, replicate n 0) (zip xs ys)
    in solveLinear am bv

-- 連立 1 次方程式 A x = b（部分ピボットのガウス消去）。解けない（情報が足りない）ときは Nothing
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a0 b0
  | not (scale0 > 0) = Nothing
  | otherwise = do
      tri <- elim (zipWith (\r v -> r ++ [v]) a0 b0)
      let xs = foldr back [] tri
      if all (\v -> not (isNaN v || isInfinite v)) xs then Just xs else Nothing
  where
    scale0 = maximum (0 : map abs (concat a0))
    -- 前進消去：行は [a_c, …, a_(n−1), b]。ピボットの行を残し、下の行から 1 列目を消して次へ
    elim [] = Just []
    elim rows =
      let hd r = case r of { (v : _) -> v; [] -> 0 }
          p = snd (foldl' (\(bv, bi) (i, r) -> if abs (hd r) > bv then (abs (hd r), i) else (bv, bi)) (abs (hd (head rows)), 0 :: Int) (zip [0 ..] rows))
          piv = rows !! p
          swapped = [ if i == p then head rows else r | (i, r) <- zip [0 ..] rows ]
          others = drop 1 swapped
          pv = hd piv
          reduced = [ zipWith (\x y -> x - f * y) (drop 1 r) (drop 1 piv) | r <- others, let f = hd r / pv ]
      in if abs pv < 1e-11 * scale0 then Nothing else (piv :) <$> elim reduced
    -- 後退代入：x_c = (b − Σ a_k x_k) / a_c
    back row xs = case row of
      (ac : rest) -> let as = take (length rest - 1) rest
                         b = last rest
                     in (foldl' (\s (a, x) -> s - a * x) b (zip as xs) / ac) : xs
      [] -> xs
