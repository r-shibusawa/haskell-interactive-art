-- ============================================
-- 51-02 遅れのあるフィードバックと企図振戦
-- 手（オレンジの点）は「目で見たずれ」に比例した速さで目標へ動く。でも目と神経には遅れ τ がある
-- K·τ が小さいとまっすぐ届く。大きくなると行きすぎてゆれ、π/2 を超えるとゆれが止まらない（ウィーナーが『サイバネティックス』で挙げた企図振戦の例）
-- 操作：マウス = 目標 / A = 目標を自動でジャンプ / ↑↓ = 遅れ τ / ←→ = ゲイン K / R = 手を戻す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Numeric (showFFloat)
import System.Environment (lookupEnv)

vmax :: Double
vmax = 2500          -- ★ 手の速さの上限 [px/s]（筋肉の限界。ゆれが無限に大きくならない）

dtH :: Double
dtH = 1 / 600        -- 計算のきざみ（1 フレームに 10 回）

-- 手の状態：位置と、過去の「見えたずれ」の記録（遅れのため、古い順）
data Hand = Hand { hX, hY :: !Double, hPast :: !(Seq (Double, Double)) }

data World = World
  { wK     :: !Double          -- ★ ゲイン：ずれ 1 px につき 1 秒で何 px 動くか [1/s]（最初は 8）
  , wTau   :: !Double          -- ★ 目と神経の遅れ [秒]（最初は 0.1）
  , wHand  :: !Hand
  , wAuto  :: !Bool
  , wTgt   :: !(Double, Double)
  , wTJump :: !Double
  , wHist  :: [Double]          -- 横のずれ（新しい順、360 個まで）
  , wTrail :: [(Double, Double)]   -- 手の軌跡（新しい順、120 個まで）
  , wTNow  :: !Double
  , wZc    :: [Double]          -- ずれの符号が変わった時刻（新しい順、3 秒以内）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-02 Purpose tremor" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetHand env world0) { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = (World 8 0.1 (makeHand (0, 0)) True (0, 0) 0 [] [] 0 [] [])

resetHand :: Env -> World -> World
resetHand env w = w { wHand = makeHand (realToFrac (envW env) * 0.3, realToFrac (envH env) * 0.5), wHist = [], wTrail = [], wZc = [] }

makeHand :: (Double, Double) -> Hand
makeHand (x, y) = Hand x y Seq.empty

-- 1 きざみ：τ 秒前に見えたずれ e(t − τ) に K をかけた速さで動く
handStep :: (Double, Double) -> Double -> Double -> Double -> Double -> Hand -> Hand
handStep (rx, ry) k delay vm dt h =
  let past0 = hPast h |> (rx - hX h, ry - hY h)
      n = round (delay / dt) :: Int
      past = Seq.drop (Seq.length past0 - (n + 1)) past0
      (ex, ey) = Seq.index past 0
      x = hX h + dt * clampv (-vm) vm (k * ex)
      y = hY h + dt * clampv (-vm) vm (k * ey)
  in Hand (clampv (-1e5) 1e5 (fin 0 x)) (clampv (-1e5) 1e5 (fin 0 y)) past

zone :: Double -> Int
zone kt | kt < 1 / exp 1 = 0
        | kt < pi / 2 = 1
        | otherwise = 2

zones :: [String]
zones = ["reaches directly", "overshoots, settles", "tremor, never stops"]

-- 手を動かす場所（左上の x, y、幅、高さ）
arena :: Env -> (Double, Double, Double, Double)
arena env = (20, 90, realToFrac (envW env) * 0.58, realToFrac (envH env) - 160)

step :: Env -> World -> World
step env w = w { wHand = hand, wTgt = tgt, wTJump = tj, wTNow = tn, wHist = hist, wZc = zc
               , wTrail = take 120 ((hX hand, hY hand) : wTrail w) }
  where
    (ax, ay, aw, ah) = arena env
    pts = [(0.25, 0.3), (0.75, 0.35), (0.5, 0.75), (0.2, 0.7)]
    (tgt, tj)
      | wAuto w = if wTNow w > wTJump w
                    then let (qx, qy) = getAt (0, 0) pts (floor (wTNow w / 2.5) `mod` 4) in ((ax + qx * aw, ay + qy * ah), wTNow w + 2.5)
                    else (wTgt w, wTJump w)
      | otherwise = let (mx, my) = p5Mouse env in ((clampv ax (ax + aw) (realToFrac mx), clampv ay (ay + ah) (realToFrac my)), wTJump w)
    (hand, tn) = foldl' (\(!h, !t) _ -> (handStep tgt (wK w) (wTau w) vmax dtH h, t + dtH)) (wHand w, wTNow w) [1 .. 10 :: Int]
    ex = fst tgt - hX hand
    hist = take 360 (ex : wHist w)
    zc0 = case wHist w of
            (prev : _) | signum ex /= signum prev && ex /= 0 -> tn : wZc w
            _ -> wZc w
    zc = takeWhile (>= tn - 3) zc0

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Delayed feedback - purpose tremor (hand shakes around the target)" 20 36 20 cTxt
  , say ((if wAuto w then "A: target jumps by itself (A for mouse)" else "Mouse = target (A for auto)")
         ++ "   Up/Down delay tau = " ++ fmt 2 (wTau w) ++ " s") 20 60 15 cSub
  , say ("Left/Right gain K = " ++ fmt 1 (wK w) ++ " /s   R reset hand") 20 82 15 cSub
  -- ---- 手と目標
  , Color (hsb 28 30 14) (rectTL axF ayF awF ahF)
  , Color (col cScope 80) (Pictures [ at (tx, ty) (ring 20 2), thickLine 2 (tx - 26, ty) (tx + 26, ty), thickLine 2 (tx, ty - 26) (tx, ty + 26) ])
  , Color (col cTube 50) (thickPath 2 [ (clampF axF (axF + awF) (f2 x), clampF ayF (ayF + ahF) (f2 y)) | (x, y) <- reverse (wTrail w) ])
  , Color (col cTube 100) (at (clampF axF (axF + awF) hx, clampF ayF (ayF + ahF) hy) (disc 11))
  , Color (col cBrass 60) (Line [(hx, hy), (hx + f2 sx * 0.2, hy + f2 sy * 0.2)])
  -- ---- ループの図
  , blockBox bx by half 50 "Target - hand" (Just ("e = " ++ fmt 1 ex ++ " px")) cBrass
  , blockBox (bx + half + 30) by half 50 "Brain: v = K e" (Just ("K = " ++ fmt 1 (wK w))) cTube
  , blockBox (bx + half + 30) (by + 90) half 50 "Muscle -> hand" (Just ("max " ++ num vmax ++ " px/s")) cTube
  , blockBox bx (by + 90) half 50 "Eye + nerve" (Just ("tau = " ++ fmt 2 (wTau w) ++ " s")) cScope
  , flowArrow (bx + half) (by + 25) (bx + half + 30) (by + 25) cTube ph Nothing
  , flowArrow (bx + half + 30 + half / 2) (by + 50) (bx + half + 30 + half / 2) (by + 90) cTube ph Nothing
  , flowArrow (bx + half + 30) (by + 115) (bx + half) (by + 115) cScope ph Nothing
  , flowArrow (bx + half / 2) (by + 90) (bx + half / 2) (by + 50) cScope ph Nothing
  -- ---- Kτ の地図と測った数
  , say ("K tau = " ++ fmt 3 kt ++ ": " ++ getAt "" zones z) bx my 15 (getAt cTxt [cScope, cBrass, cAlarm] z)
  , Color (col cScope 40) (rectTL (xm 0) (my + 14) (xm (1 / exp 1) - xm 0) 16)
  , Color (col cBrass 40) (rectTL (xm (1 / exp 1)) (my + 14) (xm (pi / 2) - xm (1 / exp 1)) 16)
  , Color (col cAlarm 40) (rectTL (xm (pi / 2)) (my + 14) (xm 3 - xm (pi / 2)) 16)
  , Color (col cTxt 100) (at (xm (min kt 3), my + 22) (disc 7))
  , say "0" (xm 0) (my + 48) 14 cSub, say "1/e" (xm (1 / exp 1) - 10) (my + 48) 14 cSub
  , say "pi/2" (xm (pi / 2) - 14) (my + 48) 14 cSub, say "3" (xm 3 - 10) (my + 48) 14 cSub
  , say ("Amplitude (last 2 s) = " ++ fmt 1 amp ++ " px") bx (my + 80) 15 cTxt
  , say ("Period (measured) = " ++ maybe "- (not shaking)" (\v -> fmt 3 v ++ " s") per) bx (my + 104) 15 cScope
  , say ("Early period guess 4 tau = " ++ fmt 3 (4 * wTau w) ++ " s") bx (my + 128) 15 cTube
  , say "With no delay, no K makes it shake" bx (my + 152) 14 cSub
  -- ---- ずれのグラフ
  , plotFrame bx gy bw gh "Horizontal error e_x (+-300 px)   6 s"
  , hLine 0 bx gy bw gh (-300) 300 cBrass
  , plotLine (reverse (wHist w)) bx gy bw gh (-300) 300 cTube 360 2
  , drawSelfTest env (wTest w) ]
  where
    (ax, ay, aw, ah) = arena env
    (axF, ayF, awF, ahF) = (f2 ax, f2 ay, f2 aw, f2 ah)
    (tx, ty) = (f2 (fst (wTgt w)), f2 (snd (wTgt w)))
    hand = wHand w
    (hx, hy) = (f2 (hX hand), f2 (hY hand))
    (sx, sy) = if Seq.null (hPast hand) then (0, 0) else Seq.index (hPast hand) 0
    ex = fst (wTgt w) - hX hand
    bx = axF + awF + 30
    bw = envW env - bx - 30
    by = 100
    half = (bw - 30) / 2
    ph = fract (fromIntegral (envFrame env) / 60)
    kt = wK w * wTau w
    my = by + 180
    z = zone kt
    xm v = bx + f2 (v / 3) * bw
    lastE = take 120 (wHist w)
    amp = if null lastE then 0 else (maximum lastE - minimum lastE) / 2
    per = case (wZc w, reverse (wZc w)) of
            (newest : _ : _ : _, oldest : _) -> Just (2 * (newest - oldest) / fromIntegral (length (wZc w) - 1))
            _ -> Nothing
    gy = my + 175
    gh = envH env - gy - 50

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> resetHand env w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTau = min 0.4 (round2 (wTau w + 0.02)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTau = max 0 (round2 (wTau w - 0.02)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wK = min 25 (wK w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wK = max 0.5 (wK w - 0.5) }
  _ -> w
  where round2 v = fromIntegral (round (v * 100) :: Int) / 100

-- ---- 自己テスト：1 次元で、目標を 0 → 1 にジャンプさせる（p5.js 版と同じ 6 項目）
stepRun :: Double -> Double -> Double -> [Double]
stepRun k d tEnd = go 0 (makeHand (0, 0)) []
  where go !t !h acc | t >= tEnd = reverse acc
                     | otherwise = let h' = handStep (1, 0) k d 50 dtH h in go (t + dtH) h' (hX h' : acc)

selfTest :: [(String, Bool)]
selfTest =
  [ ("K tau = 0.3 < 1/e: no overshoot (max " ++ fmt 4 (maxL a) ++ " <= 1), reaches the target in 6 s", maxL a <= 1 + 1e-9 && lastL a > 0.99)
  , ("K tau = 1.0: overshoots (max " ++ fmt 3 (maxL b) ++ ") but settles (last 1 s |error| < 1e-3)"
    , maxL b > 1.05 && all (\x -> abs (x - 1) < 1e-3) (drop (length b - 600) b))
  , ("K tau = 2.0 > pi/2: never stops (last 1 s amplitude " ++ fmt 2 late ++ " >= first 1 s " ++ fmt 2 early ++ ")", late >= early * 0.99 && late > 0.5)
  , ("K tau = 1.6 (just above pi/2): period " ++ fmt 3 per ++ " s ~ 4 tau = 0.4 s (5%)", abs (per - 0.4) / 0.4 < 0.05)
  , ("no delay: even K = 200 does not overshoot", maxL e <= 1 + 1e-9)
  , ("K tau = 16: speed limit keeps values finite (no NaN)", all (\x -> not (isNaN x || isInfinite x)) f && maxL (map abs f) < 1e3) ]
  where
    maxL xs = if null xs then 0 else maximum xs
    lastL xs = if null xs then 0 else last xs
    a = stepRun 3 0.1 6
    b = stepRun 10 0.1 10
    c = stepRun 20 0.1 6
    nc = length c
    early = maxL [ abs (x - 1) | (i, x) <- zip [0 :: Int ..] c, i < 600 ]
    late = maxL [ abs (x - 1) | (i, x) <- zip [0 :: Int ..] c, i > nc - 600 ]
    d = stepRun 16 0.1 8
    z = [ fromIntegral i * dtH | (i, x0, x1) <- zip3 [1 :: Int ..] d (drop 1 d), (x1 - 1) * (x0 - 1) < 0 ]
    per = if length z > 3 then 2 * (last z - getAt 0 z 2) / fromIntegral (length z - 3) else 0
    e = stepRun 200 0 1
    f = stepRun 40 0.4 5

-- ---------- やってみよう ----------
-- 1. ↑↓ で τ を 0 にすると、K をどんなに上げてもゆれない。τ = 0.1 にして K を上げていくと、どこからゆれ始める？
-- 2. 測った周期と 4τ を比べよう。τ を 2 倍にすると周期は？
-- 3. A を切ってマウスで目標を動かす。ゆれているときに目標を速く動かすとどうなる？
-- 4. vmax を小さくすると、ゆれの振幅はどう変わる？（筋肉が弱いと…）
-- 5. 「よく見てから動く」（τ が大きい）人ほど、ゆっくり（K を小さく）動くべき理由を、Kτ の地図で説明しよう

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

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

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

-- グラフの中の水平線（目標値など、点線）
hLine :: Double -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Picture
hLine v x y w h lo hi c =
  let yy = y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h
  in Color (col c 70) (Pictures [ Line [(x + s, yy), (x + min w (s + 6), yy)] | s <- [0, 11 .. w] ])

-- ブロック図の箱（名前と今の値）
blockBox :: Float -> Float -> Float -> Float -> String -> Maybe String -> HSB -> Picture
blockBox x y w h name val c = Pictures $
  [ Color (hsb 25 40 16) (rectTL x y w h), Color (col c 100) (frameTL 2 x y w h)
  , sayC name (x + w / 2) (y + h / 2 - (if val == Nothing then 0 else 10)) 15 cTxt ]
  ++ [ sayC v (x + w / 2) (y + h / 2 + 12) 14 c | Just v <- [val] ]

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

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
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
