-- ============================================
-- 51-01 ワットの遠心調速機（ガバナー）
-- 回転が速くなると 2 つの球が遠心力で開き、てこが蒸気弁をしぼる → 回転が下がる。負荷が変わっても回転数がほぼ一定
-- 摩擦（球の動きのブレーキ）が小さすぎたりゲインが大きすぎると「ハンチング」（ゆれ続ける）。マクスウェル「調速機について」（1868）の安定条件を数で表示
-- 操作：1 2 3 = 変える量（負荷・摩擦・ゲイン） / ↑↓ = 値を変える / スペース = 負荷を急に増やす・戻す / F = 調速機を外す（弁を固定） / R = 回転をゆらす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

gL, kT, bE :: Double
gL = 30              -- ★ 重力 ÷ 腕の長さ（g / l）[1/s²]
kT = 10              -- ★ 蒸気のトルクの強さ
bE = 0.5             -- ★ エンジンの摩擦（回るほどブレーキ）

u0, phi0 :: Double
u0 = 0.5             -- 腕の角度が PHI0 のとき弁の開き U0
phi0 = 0.6

hStep :: Double
hStep = 1 / 240

-- 調速機の設定：負荷 L・球の摩擦 c・ゲイン kv（角度 1 rad で弁がどれだけ閉じるか）・調速機あり／なし
data Par = Par { pL, pC, pKv :: !Double, pFb :: !Bool }

-- 特性方程式 s³ + a2 s² + a1 s + a0 の係数
data CP = CP { a2, a1, a0 :: !Double }

data World = World
  { wPar   :: !Par
  , wSt    :: [Double]                    -- [腕の角度 φ, 角速度 φ', エンジンの回転 ω]
  , wTheta :: !Double                     -- 絵の中の回転の角度
  , wHist  :: [(Double, Double, Double)]  -- (ω, ω*, u) の履歴（新しい順、720 個まで）
  , wSel   :: !Int
  , wLbase :: !Double
  , wTest  :: [(String, Bool)] }

par0 :: Par
par0 = Par 2 5 0.6 True   -- ★ はじめの設定

main :: IO ()
main = runSketch (sketch "51-01 Watt governor" (World par0 (equilibrium par0) 0 [] 1 2 []))
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (World par0 (equilibrium par0) 0 [] 1 2 selfTest)
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

-- 弁の開き u（0〜1）：腕が開くほど閉じる。F で外すと U0 のまま
valve :: Double -> Par -> Double
valve phi p = if pFb p then clampv 0 1 (u0 - pKv p * (phi - phi0)) else u0

-- 状態 s = [腕の角度 φ, 角速度 φ', エンジンの回転 ω]
govDeriv :: [Double] -> Par -> [Double]
govDeriv s p = case s of
  [ph, pd, w] -> [ pd
                 , w * w * sin ph * cos ph - gL * sin ph - pC p * pd    -- 遠心力 − 重力 − 摩擦
                 , kT * valve ph p - pL p - bE * w ]                   -- 蒸気 − 負荷 − 摩擦（慣性 1）
  _ -> [0, 0, 0]

govStep :: [Double] -> Par -> Double -> [Double]
govStep s p h = case rk4 govDeriv s p h of
  [n0, n1, n2] ->
    let (m0, m1) | n0 < 0.05 = (0.05, max 0 n1)          -- 腕のストッパー
                 | n0 > 1.45 = (1.45, min 0 n1)
                 | otherwise = (n0, n1)
    in [m0, clampv (-50) 50 (fin 0 m1), clampv 0 40 (fin 0 n2)]
  _ -> s

-- つり合い：cos φ = GL / ω²、KT·u(φ) = L + BE·ω を 2 分法で
equilibrium :: Par -> [Double]
equilibrium p
  | not (pFb p) = let w = max 0 ((kT * u0 - pL p) / bE)                 -- 弁が固定なら式で
                  in [if w * w > gL then acos (gL / (w * w)) else 0.05, 0, w]
  | otherwise = let w = go (200 :: Int) (sqrt gL + 1e-9) 40 in [acos (gL / (w * w)), 0, w]
  where
    go 0 lo hi = (lo + hi) / 2
    go k lo hi = let w = (lo + hi) / 2
                     ph = acos (gL / (w * w))
                 in if kT * valve ph p - pL p - bE * w > 0 then go (k - 1) w hi else go (k - 1) lo w

-- つり合いのまわりで線形化（数値微分のヤコビ行列）→ 特性方程式 s³ + a2 s² + a1 s + a0
charPoly :: Par -> CP
charPoly p = CP (-tr) m2 (-det)
  where
    s = equilibrium p
    e = 1e-6
    bump j d = [ if i == j then v + d else v | (i, v) <- zip [0 :: Int ..] s ]
    colJ j = zipWith (\fp fm -> (fp - fm) / (2 * e)) (govDeriv (bump j e) p) (govDeriv (bump j (-e)) p)
    cols = map colJ [0, 1, 2]
    a i j = getAt 0 (getAt [] cols j) i
    tr = a 0 0 + a 1 1 + a 2 2
    m2 = a 0 0 * a 1 1 - a 0 1 * a 1 0 + a 0 0 * a 2 2 - a 0 2 * a 2 0 + a 1 1 * a 2 2 - a 1 2 * a 2 1
    det = a 0 0 * (a 1 1 * a 2 2 - a 1 2 * a 2 1) - a 0 1 * (a 1 0 * a 2 2 - a 1 2 * a 2 0) + a 0 2 * (a 1 0 * a 2 1 - a 1 1 * a 2 0)

-- 安定の条件（3 次のラウス＝フルビッツ）：a2, a1, a0 > 0 かつ a2·a1 > a0
isStable :: CP -> Bool
isStable cp = a2 cp > 0 && a1 cp > 0 && a0 cp > 0 && a2 cp * a1 cp > a0 cp

step :: World -> World
step w = w { wSt = st, wTheta = wTheta w + getAt 0 st 2 / 60
           , wHist = take 720 ((getAt 0 st 2, getAt 0 eq 2, valve (getAt 0 st 0) p) : wHist w) }
  where
    p = wPar w
    st = foldl' (\s _ -> govStep s p hStep) (wSt w) [1 .. 4 :: Int]
    eq = equilibrium p

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Watt governor - faster spin opens the balls, which close the valve" 20 36 20 cTxt
  , say (concat [ (if i == wSel w then "> " else "  ") ++ show i ++ " " ++ s ++ "   "
                | (i, s) <- zip [1 :: Int ..] ["load L = " ++ fmt 1 (pL p), "ball friction c = " ++ fmt 1 (pC p), "gain kv = " ++ fmt 2 (pKv p)] ]) 20 60 15 cSub
  , say ("Up/Down change   Space load +1   F governor " ++ (if pFb p then "ON" else "removed") ++ "   R shake") 20 82 15 cSub ]
  -- ---- 調速機の絵
  ++ [ Color (col cBrass 100) (thickLine 6 (cx, top - 20) (cx, top + lp * 1.6)) ]
  ++ concat [ let c3 = cos th * sg
                  bx = cx + lp * sin phF * c3
                  by = top + lp * cos phF
              in [ Color (col cBrass 100) (thickLine 4 (cx, top) (bx, by))
                 , Color (col cBrass 100) (thickLine 4 ((cx + bx) / 2, (top + by) / 2) (cx, sleeveY))
                 , Color (col cTube (70 + 30 * sin th * sg)) (at (bx, by) (disc ((34 + 8 * sin th * sg) / 2))) ]
            | sg <- [-1, 1] ]
  ++ [ Color (col cBrass 100) (rectTL (cx - 16) (sleeveY - 8) 32 16)
     -- てこと弁
     , Color (col cSub 100) (thickLine 4 (cx + 16, sleeveY) (vx, vy - 60 * f2 (u0 - u)))
     , Color (hsb 28 20 30) (rectTL (vx + 20) (top + 120) 220 50)
     , Color (col cTube (30 + 60 * f2 u)) (rectTL (vx + 20) (top + 120) (220 * f2 u) 50)
     , say ("Steam valve u = " ++ fmt 3 u) (vx + 20) (top + 110) 15 cTube
     , say ("Engine w = " ++ fmt 3 om ++ " rad/s (" ++ fmt 1 (om * 60 / (2 * pi)) ++ " rpm)") (vx + 20) (top + 200) 16 cScope
     , say ("Balance w* = " ++ fmt 3 (getAt 0 eq 2) ++ "   arm phi = " ++ fmt 1 (getAt 0 st 0 * 180 / pi) ++ " deg") (vx + 20) (top + 224) 15 cTxt ]
  -- ---- 安定の判定（数で）
  ++ [ say "Linearized: s^3+a2 s^2+a1 s+a0 = 0" mx my 16 cTxt
     , say ("a2 = " ++ fmt 3 (a2 cp) ++ "  a1 = " ++ fmt 3 (a1 cp) ++ "  a0 = " ++ fmt 3 (a0 cp)) mx (my + 28) 14 cTube
     , say ("Stable if a2*a1 > a0: " ++ fmt 2 (a2 cp * a1 cp) ++ (if ok then " > " else " <= ") ++ fmt 2 (a0 cp)) mx (my + 56) 15 (if ok then cScope else cAlarm)
     , say (if pFb p then (if ok then "-> stable: the wobble dies" else "-> unstable: hunting") else "No governor: valve fixed")
           mx (my + 84) 18 (if pFb p then (if ok then cScope else cAlarm) else cSub)
     , say ("Measured amplitude (4 s) = " ++ fmt 3 amp ++ " rad/s") mx (my + 112) 14 cTxt
     , say ("Load +1 -> w: gov " ++ (if pFb p then fmt 3 d1 else "-") ++ ", none " ++ fmt 3 d0) mx (my + 140) 15 cTxt
     , say "Low c or high kv breaks the condition" mx (my + 168) 14 cSub
     , say "(Maxwell 1868: the coefficients decide)" mx (my + 190) 14 cSub ]
  -- ---- 時間グラフ
  ++ [ plotFrame gx gy gwid ght "Speed w (green), balance w* (dotted) +-3 rad/s, valve u (orange, 0 to 1)   12 s"
     , hLine (getAt 0 eq 2) gx gy gwid ght lo hi cBrass
     , plotLine [ u' | (_, _, u') <- hist ] gx gy gwid ght 0 1 cTube 720 1.5
     , plotLine [ o | (o, _, _) <- hist ] gx gy gwid ght lo hi cScope 720 2.5
     , drawSelfTest env (wTest w) ]
  where
    p = wPar w
    st = wSt w
    eq = equilibrium p
    om = getAt 0 st 2
    u = valve (getAt 0 st 0) p
    cp = charPoly p
    ok = isStable cp
    ws = [ o | (o, _, _) <- take 240 (wHist w) ]
    amp = if null ws then 0 else (maximum ws - minimum ws) / 2
    d1 = getAt 0 (equilibrium p { pL = pL p + 1 }) 2 - getAt 0 eq 2
    d0 = (kT * u0 - pL p - 1) / bE - (kT * u0 - pL p) / bE
    th = f2 (wTheta w)
    phF = f2 (getAt 0 st 0)
    cx = envW env * 0.22
    top = 130
    lp = min 150 (envH env * 0.2)
    sleeveY = top + 2 * lp * cos phF * 0.55
    vx = cx + 230
    vy = sleeveY
    mx = envW env * 0.62
    my = 110
    gx = 40
    gy = envH env * 0.55
    gwid = envW env - 80
    ght = envH env - gy - 50
    lo = getAt 0 eq 2 - 3
    hi = getAt 0 eq 2 + 3
    hist = reverse (wHist w)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wSel = fromEnum c - fromEnum '0' }
    | c `elem` "fF" -> w { wPar = p { pFb = not (pFb p) } }
    | c `elem` "rR" -> w { wSt = [ if i == 2 then clampv 0 40 (v + 1) else v | (i, v) <- zip [0 :: Int ..] (wSt w) ] }
  EventKey (SpecialKey KeyUp) Down _ _ -> change 1
  EventKey (SpecialKey KeyDown) Down _ _ -> change (-1)
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPar = p { pL = if pL p >= wLbase w + 1 then wLbase w else wLbase w + 1 } }
  _ -> w
  where
    p = wPar w
    change d = case wSel w of
      1 -> let l = clampv 0 4 (pL p + 0.5 * d) in w { wPar = p { pL = l }, wLbase = l }
      2 -> w { wPar = p { pC = clampv 0.5 12 (pC p + 0.5 * d) } }
      _ -> w { wPar = p { pKv = clampv 0.1 2 (pKv p + 0.1 * d) } }

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("at the balance point ds/dt = 0 (|ds/dt| < 1e-9)", all (\v -> abs v < 1e-9) (govDeriv e p))
  , ("load +1: governor change " ++ fmt 3 dClosed ++ " <= 1/5 of none " ++ fmt 1 dOpen, dClosed < dOpen / 5 && near dOpen (1 / bE) 1e-6) ]
  ++ map cTest [2, 5]
  ++ [ ("raising gain kv raises a0 and breaks it (kv = 0.3 stable, 1.5 unstable)", isStable small && not (isStable big) && a0 big > a0 small)
     , ("extreme wobble (c = 0.5, from w = 30) gives no NaN", not bad) ]
  where
    p = par0
    e = equilibrium p
    pLd = p { pL = 3 }
    dClosed = abs (getAt 0 (equilibrium pLd) 2 - getAt 0 e 2)
    dOpen = abs (getAt 0 (equilibrium pLd { pFb = False }) 2 - getAt 0 (equilibrium p { pFb = False }) 2)
    cTest c =
      let q = p { pC = c }
          ok = isStable (charPoly q)
          eqw = getAt 0 (equilibrium q) 2
          s0 = case equilibrium q of { [x, y, z] -> [x, y, z + 0.05]; s -> s }
          (_, late) = foldl' (\(!s, !l) k -> let s' = govStep s q hStep
                                              in (s', if k > 25 * 240 then max l (abs (getAt 0 s' 2 - eqw)) else l))
                             (s0, 0) [0 .. 30 * 240 - 1 :: Int]
      in ("c = " ++ num c ++ ": a2 a1 > a0 " ++ (if ok then "holds" else "fails") ++ ", wobble (25-30 s) = " ++ showEFloat (Just 1) late ""
            ++ (if ok then " dies" else " stays"), if ok then late < 1e-3 else late > 0.5)
    big = charPoly p { pKv = 1.5 }
    small = charPoly p { pKv = 0.3 }
    bad = snd (foldl' (\(!s, !b) _ -> let s' = govStep s p { pC = 0.5 } hStep in (s', b || any (\v -> isNaN v || isInfinite v) s'))
                      (case e of { [_, y, _] -> [1.4, y, 30]; s -> s }, False) [1 .. 20 * 240 :: Int])

-- ---------- やってみよう ----------
-- 1. 2 で摩擦を選び、↓ で少しずつ下げる。a2·a1 と a0 が入れかわる所でゆれ方はどう変わる？
-- 2. スペースで負荷を増やす。調速機あり／なし（F）で回転数の落ち方を比べよう
-- 3. 3 でゲイン kv を上げると、負荷による回転数のずれは小さくなる。でも安定は？（性能と安定のかけひき）
-- 4. gL（腕の長さ）を変えると、つり合いの回転数はどう変わる？
-- 5. 1868 年のマクスウェルの論文は、この「a2·a1 > a0」のような条件を調速機について調べたもの。理由を考えてみよう

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

-- ---- 4 次のルンゲ＝クッタ法（f s p = ds/dt、s はリスト）
rk4 :: ([Double] -> p -> [Double]) -> [Double] -> p -> Double -> [Double]
rk4 f s p h =
  let k1 = f s p
      k2 = f (zipWith (\v k -> v + h / 2 * k) s k1) p
      k3 = f (zipWith (\v k -> v + h / 2 * k) s k2) p
      k4 = f (zipWith (\v k -> v + h * k) s k3) p
  in zipWith5 (\v a b c d -> v + h / 6 * (a + 2 * b + 2 * c + d)) s k1 k2 k3 k4
  where zipWith5 g (a : as) (b : bs) (c : cs) (d : ds) (e : es) = g a b c d e : zipWith5 g as bs cs ds es
        zipWith5 _ _ _ _ _ _ = []

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
