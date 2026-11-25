-- ============================================
-- 67-06 ピタゴラスの定理と射影 ─ KL ダイバージェンスでも「直角三角形」なら D(p‖r) = D(p‖q) + D(q‖r) がぴったり成り立つ
-- 左（e-射影）：平均が c の分布の集合 E（三角形の中の直線 = m-平坦）。r から E へ e-測地線で降ろした足 q は q_i ∝ r_i e^(λ x_i)
--   E の中のどの p でも D(p‖r) = D(p‖q) + D(q‖r)（p→q は m-測地線、q→r は e-測地線、q で直交）
-- 右（m-射影）：2×2 の表 p(x, y) から「独立な表」の集合 M（e-平坦）へ。足 q = 周辺分布の積 p(x)p(y) = 最尤推定
--   M の中のどの r でも D(p‖r) = D(p‖q) + D(q‖r)
-- 操作：左 ドラッグ r（紫）・p（直線の上）、↑↓ 平均 c / 右 棒をドラッグで表 p、正方形の中をクリックで独立な r / R でたらめ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

xv :: [Double]
xv = [0, 1, 2]                                -- ★ 3 つの結果の値 x（平均 = Σ x_i p_i）

data Grab = GNone | GR | GP | GBar | GSq deriving Eq

data World = World
  { wC      :: !Double                        -- 平均 c
  , wR      :: [Double]                       -- 左の r
  , wS      :: !Double                        -- 直線 E の上の p の位置（p3 = s）
  , wTab    :: [Double]                       -- 右の表 p = [p00, p01, p10, p11]
  , wAB     :: !(Double, Double)              -- 独立な r の (P(x=1), P(y=1))
  , wGrab   :: !Grab
  , wField  :: Maybe Field                    -- 正方形の等高線（表 p と画面の幅が変わったら作り直す）
  , wKey    :: String
  , wRng    :: !Rng
  , wTest   :: [Check] }

main :: IO ()
main = runSketch (sketch "67-06 Pythagorean theorem and projections" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (refresh env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (refresh env w)
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあとの乱数の列をそのまま使う（R キー）
world0 :: World
world0 = World 1.3 [0.6, 0.3, 0.1] 0.3 [0.4, 0.1, 0.15, 0.35] (0.3, 0.7) GNone Nothing "" rngAfterTest []

frameOf :: Env -> SFrame
frameOf env = let side = min (d2 (envW env) * 0.4) ((d2 (envH env) - 330) / 0.91)
              in simplexFrame (50 + side / 2) (130 + side * 0.62) side

barBox :: Env -> Box
barBox env = Box (d2 (envW env) * 0.56) 140 (d2 (envW env) * 0.18) 130

sqBox :: Env -> PBox
sqBox env = let s = min (d2 (envW env) * 0.18) 200 in plotBox (d2 (envW env) * 0.79) 120 s s 0 1 0 1

-- ---- 左：平均 c の集合 E と e-射影
lineEnds :: Double -> (Double, Double)        -- p3 = s の範囲
lineEnds c = (max 0 (c - 1), c / 2)

onLine :: Double -> Double -> [Double]        -- p2 + 2 p3 = c、合計 1
onLine c s = [1 - c + s, c - 2 * s, s]

tilt :: [Double] -> Double -> [Double]        -- r_i e^(λ x_i) を正規化
tilt r lam = normP (zipWith (\v x -> v * exp (lam * x)) r xv)

meanOf :: [Double] -> Double
meanOf p = sumOf (zipWith (*) p xv)

-- ★ ニュートン法：f(λ) = 平均 − c、f'(λ) = 分散
--   平均は λ について増える → [lo, hi] で答えをはさみ、外へ跳んだら二分法
solveLambda :: [Double] -> Double -> Double
solveLambda r c = go (0 :: Int) 0 (-80) 80
  where
    go it lam lo hi
      | it >= 100 = lam
      | otherwise =
          let q = tilt r lam
              m = meanOf q
              v = sumOf (zipWith (\w x -> w * (x - m) ^ (2 :: Int)) q xv)
              (lo', hi') = if m < c then (lam, hi) else (lo, lam)
              nx0 = lam - (m - c) / max v 1e-300
              nx = if nx0 > lo' && nx0 < hi' then nx0 else (lo' + hi') / 2
          in if abs (nx - lam) < 1e-15 * (1 + abs lam) then nx else go (it + 1) nx lo' hi'

eProject :: [Double] -> Double -> [Double]
eProject r c = tilt r (solveLambda r c)

-- ---- 右：2×2 の表 [p00, p01, p10, p11] と独立な表
margins :: [Double] -> (Double, Double)       -- (P(x=1), P(y=1))
margins t = (at' t 2 0 + at' t 3 0, at' t 1 0 + at' t 3 0)

indep :: Double -> Double -> [Double]
indep a b = [(1 - a) * (1 - b), (1 - a) * b, a * (1 - b), a * b]

mProject :: [Double] -> [Double]
mProject t = uncurry indep (margins t)

-- 正方形の等高線は、表 p（小数 3 桁）と画面の幅が変わったときだけ作り直す
refresh :: Env -> World -> World
refresh env w =
  let k = intercalate "," (map (fmtN 3) (wTab w)) ++ show (envW env)
      sq = sqBox env
      fld = gridField sq (\a b -> min (divKL (wTab w) (indep (clampv a 1e-4 (1 - 1e-4)) (clampv b 1e-4 (1 - 1e-4)))) 3)
                      fieldOpt { foLevel = 0.05, foVmax = 1.5, foCap = 0.6, foC0 = (40, 60, 95), foC1 = violet, foRes = 2 }
  in if k == wKey w then w else w { wKey = k, wField = Just fld }

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawField (wField w)
  , hudTitle "Pythagorean theorem and projections: D(p||r) = D(p||q) + D(q||r)"
             ("Left: drag r (violet) and p (on the line)   Up/Down mean c = " ++ fmtN 2 (wC w)
              ++ "\nRight: drag the bars = table p, click the square = independent r   R random")
  -- ===== 左
  , drawSimplexFrame fr ["x = 0", "x = 1", "x = 2"]
  , simplexPath fr [onLine c s0, onLine c s1] (colA 200 teal) 2
  , simplexPath fr mg (col orange) 3.5, simplexPath fr eg (col violet) 3.5
  , simplexDot fr (wR w) violet 16 "r", simplexDot fr pE orange 16 "p", simplexDot fr qE ink 13 "q"
  , say ("Green line E = distributions with mean " ++ fmtN 2 c ++ " (m-flat)") 40 y0 14 (col teal)
  , say ("D(p||r) = " ++ fmtN 6 dpr) 40 (y0 + 28) 16 (col ink)
  , say ("D(p||q) + D(q||r) = " ++ fmtN 6 dpq ++ " + " ++ fmtN 6 dqr) 40 (y0 + 52) 16 (col ink)
  , say ("  = " ++ fmtN 6 (dpq + dqr)) 40 (y0 + 74) 16 (col ink)
  , say ("gap = " ++ jsExp1 (dpr - dpq - dqr) ++ "   orthogonal: sum (p-q)(log r - log q) = " ++ jsExp1 orth) 40 (y0 + 98) 14 (col lime)
  , say "q = point of E closest to r (min D(.||r)); r on E -> q = r" 40 (y0 + 120) 14 (col dimc)
  -- ===== 右
  , drawBars (wTab w) br barOpt { boNames = ["x0 y0", "x0 y1", "x1 y0", "x1 y1"], boGhost = qM, boVmax = 0.8 }
  , say "Table p" (f2 (bxX br)) (f2 (bxY br) - 36) 15 (col orange)
  , say "(white frames = p(x)p(y) = q)" (f2 (bxX br)) (f2 (bxY br) - 18) 14 (col orange)
  , drawAxes sq "P(x = 1)" "independent tables M" ]
  ++ [ Pictures [Color (col c') (circleAt x y 13), say nm (x + 9) (y - 8) 14 (col c')]
     | ((a, b), c', nm) <- [(margins qM, ink, "q"), (wAB w, violet, "r")], let (x, y) = toScrF sq a b ]
  ++ [ say "(contours of D(p||r))" (f2 (pbX sq)) (f2 (pbY sq + pbH sq) + 48) 14 (col dimc)
     , say ("D(p||r) = " ++ fmtN 6 bDpr) (f2 (bxX br)) ty 16 (col ink)
     , say ("D(p||q) + D(q||r) = " ++ fmtN 6 bDpq ++ " + " ++ fmtN 6 bDqr) (f2 (bxX br)) (ty + 26) 16 (col ink)
     , say ("  = " ++ fmtN 6 (bDpq + bDqr)) (f2 (bxX br)) (ty + 50) 16 (col ink)
     , say ("gap = " ++ jsExp1 (bDpr - bDpq - bDqr)) (f2 (bxX br)) (ty + 76) 16 (col lime)
     , say "D(p||q) = mutual information (more in 12)." (f2 (bxX br)) (ty + 104) 14 (col dimc)
     , say "Reading p as data shares, q = MLE of the" (f2 (bxX br)) (ty + 124) 14 (col dimc)
     , say "independent model: MLE = m-projection" (f2 (bxX br)) (ty + 144) 14 (col dimc)
     , drawSelfTest env (wTest w) ]
  where
    fr = frameOf env
    br = barBox env
    sq = sqBox env
    c = wC w
    (s0, s1) = lineEnds c
    pE = onLine c (sOf w)
    qE = eProject (wR w) c
    eg = [ mixE qE (wR w) (i2d k / 40) | k <- [0 .. 40 :: Int] ]
    mg = [ mixM pE qE (i2d k / 40) | k <- [0 .. 40 :: Int] ]
    dpr = divKL pE (wR w)
    dpq = divKL pE qE
    dqr = divKL qE (wR w)
    orth = sumOf (zipWith3 (\pi' v ri -> (pi' - v) * (safeLog ri - safeLog v)) pE qE (wR w))
    (_, vy) = vtx fr 0
    y0 = f2 vy + 34
    qM = mProject (wTab w)
    rM = uncurry indep (wAB w)
    bDpr = divKL (wTab w) rM
    bDpq = divKL (wTab w) qM
    bDqr = divKL qM rM
    ty = f2 (max (bxY br + bxH br + 60) (pbY sq + pbH sq + 60)) + 16

-- 直線 E の上の p の位置（端から少し離す。p5.js 版は draw の中で sL を書きかえる）
sOf :: World -> Double
sOf w = let (s0, s1) = lineEnds (wC w) in clampv (wS w) (s0 + 0.004) (s1 - 0.004)

event :: Env -> Event -> World -> IO World
event env ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (rx, ry) = baryToXY fr (wR w)
        (px, py) = baryToXY fr (onLine (wC w) (wS w))
        g | dd rx ry < 22 = GR
          | dd px py < 22 = GP
          | inRect mx my (Box (bxX br) (bxY br - 30) (bxW br) (bxH br + 50)) = GBar
          | inRect mx my (pbBox sq) = GSq
          | otherwise = GNone
    in dragged w { wGrab = g }
  EventMotion _ -> dragged w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = GNone }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wC = clampv (wC w + 0.05) 0.1 1.9 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wC = clampv (wC w - 0.05) 0.1 1.9 }
  EventKey (Char ch) Down _ _
    | ch == 'r' || ch == 'R' ->
        let ((r, t, ab), g) = runR (do r' <- rndDist 3 0.02
                                       t' <- rndDist 4 0.01
                                       a <- rnd
                                       b <- rnd
                                       pure (r', t', (0.1 + 0.8 * a, 0.1 + 0.8 * b))) (wRng w)
        in w { wR = r, wTab = t, wAB = ab, wRng = g }
  _ -> w
  where
    w = w0 { wS = sOf w0 }
    fr = frameOf env
    br = barBox env
    sq = sqBox env
    (mx, my) = mouseD env
    dd x y = sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int))
    dragged v = case wGrab v of
      GR -> v { wR = clampSimplex (xyToBary fr mx my) 0.004 }
      GP -> let (s0, s1) = lineEnds (wC v)
                (ax, ay) = baryToXY fr (onLine (wC v) s0)
                (bx, by) = baryToXY fr (onLine (wC v) s1)
                u = ((mx - ax) * (bx - ax) + (my - ay) * (by - ay)) / ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int) + 1e-9)
            in v { wS = s0 + clampv u 0 1 * (s1 - s0) }
      -- 縦軸の上限 0.8 に合わせる
      GBar -> v { wTab = barsDrag (wTab v) (Box (bxX br) (bxY br + bxH br - bxH br / 0.8) (bxW br) (bxH br / 0.8)) mx my 0.003 }
      GSq -> let (a, b) = fromScr sq mx my in v { wAB = (clampv a 0.01 0.99, clampv b 0.01 0.99) }
      GNone -> v

-- ---------- やってみよう ----------
-- 1. 左で r を動かすと、q は直線 E の上をどう動く？ 紫の e-測地線はいつも E と直角？
-- 2. 右の表を「x と y が同じになりやすい」形（x0 y0 と x1 y1 が大きい）にすると D(p‖q) は？ 独立な表にすると？
-- 3. 右の正方形で r を q に重ねると D(q‖r) = 0。等高線の一番低い所が q になっているか確かめよう
-- 4. ふつうのピタゴラス a² = b² + c² との対応：KL は「距離の 2 乗」の仲間（01：近い所では ½ × 距離²）
-- 5. EM 法（09）はこの 2 つの射影を交互にくり返す

-- ---- 自己テスト
-- (a) m-射影：200 個の表（種 606）。最初の 20 個は 100 個のでたらめな独立表とも比べる
--   戻り値 = (最大誤差, 最小か, 最大誤差 (b), 平均が c か, 最小か (b), 直交か)、そのあとの乱数の列
testRun :: ((Double, Bool, Double, Bool, Bool, Bool), Rng)
testRun = runR body (rngOf 606)
  where
    body = do
      (wA, okMinA) <- loopA 0 0 True
      (wB, okLam, okMinB, okOrth) <- loopB 0 0 True True True
      pure (wA, okMinA, wB, okLam, okMinB, okOrth)
    loopA :: Int -> Double -> Bool -> R (Double, Bool)
    loopA k !wA !ok
      | k >= 200 = pure (wA, ok)
      | otherwise = do
          p <- rndDist 4 0.002
          a <- rnd
          b <- rnd
          let q = mProject p
              r = indep (0.02 + 0.96 * a) (0.02 + 0.96 * b)
              wA' = max wA (abs (divKL p r - divKL p q - divKL q r))
          ok' <- if k < 20
                   then loopR 100 (\o -> do x <- rnd
                                            y <- rnd
                                            pure (o && not (divKL p (indep x y) < divKL p q - 1e-12))) ok
                   else pure ok
          loopA (k + 1) wA' ok'
    loopB :: Int -> Double -> Bool -> Bool -> Bool -> R (Double, Bool, Bool, Bool)
    loopB k !wB !okL !okM !okO
      | k >= 200 = pure (wB, okL, okM, okO)
      | otherwise = do
          r <- rndDist 3 0.002
          u <- rnd
          let c = 0.15 + 1.7 * u
              q = eProject r c
              (s0, s1) = lineEnds c
          v <- rnd
          let p = onLine c (s0 + (s1 - s0) * (0.01 + 0.98 * v))
              wB' = max wB (abs (divKL p r - divKL p q - divKL q r))
              okL' = okL && not (abs (meanOf q - c) > 1e-12)
              okO' = okO && not (abs (sumOf (zipWith3 (\pi' qi ri -> (pi' - qi) * (safeLog ri - safeLog qi)) p q r)) > 1e-10)
          okM' <- if k < 20
                    then loopR 100 (\o -> do x <- rnd
                                             pure (o && not (divKL (onLine c (s0 + (s1 - s0) * x)) r < divKL q r - 1e-12))) okM
                    else pure okM
          loopB (k + 1) wB' okL' okM' okO'

rngAfterTest :: Rng
rngAfterTest = snd testRun

selfTest :: [Check]
selfTest =
  [ ("m-射影：200 個の表 p と独立な r で D(p‖r) = D(p‖q) + D(q‖r)（最大誤差 " ++ jsExp1 wA ++ " ≤ 1e-10）", wA < 1e-10)
  , ("q = 周辺分布の積は、独立な表の中で D(p‖·) が最小（2000 個のでたらめな独立表に勝つ）", okMinA)
  , ("e-射影：200 組の r と E の中の p で D(p‖r) = D(p‖q) + D(q‖r)（最大誤差 " ++ jsExp1 wB ++ " ≤ 1e-10）", wB < 1e-10)
  , ("ニュートン法の q は平均がちょうど c（誤差 1e-12）、E の中で D(·‖r) が最小", okLam && okMinB)
  , ("直交：Σ (p_i − q_i)(log r_i − log q_i) = 0（m の向きと e の向きがフィッシャー計量で直角）", okOrth)
  , ("ニュートン法の λ = 二分法の λ（誤差 1e-10）", abs (solveLambda r0 1.4 - bis) < 1e-10) ]
  where
    ((wA, okMinA, wB, okLam, okMinB, okOrth), _) = testRun
    r0 = [0.5, 0.3, 0.2]
    bis = let (lo, hi) = foldl' (\(lo', hi') _ -> let m = (lo' + hi') / 2 in if meanOf (tilt r0 m) < 1.4 then (m, hi') else (lo', m)) (-50, 50) [1 .. 200 :: Int]
          in (lo + hi) / 2

-- v.toExponential(1)：JavaScript と同じ書き方（1.2e-15、0.0e+0）
jsExp1 :: Double -> String
jsExp1 v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = headOr e0 ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 確率の三角形（3 つの結果）：頂点 1 = 左下、2 = 右下、3 = 上。点 = 確率で重みをつけた頂点の平均（重心座標）
data SFrame = SFrame { sfCx :: !Double, sfCy :: !Double, sfSide :: !Double, sfV :: [(Double, Double)] }

simplexFrame :: Double -> Double -> Double -> SFrame
simplexFrame cx cy side =
  let h = side * sqrt 3 / 2
  in SFrame cx cy side [(cx - side / 2, cy + h / 3), (cx + side / 2, cy + h / 3), (cx, cy - 2 * h / 3)]

vtx :: SFrame -> Int -> (Double, Double)      -- i 番目の頂点
vtx fr i = at' (sfV fr) i (0, 0)

baryToXY :: SFrame -> [Double] -> (Double, Double)
baryToXY fr p = foldl' (\(x, y) (w, (vx, vy)) -> (x + w * vx, y + w * vy)) (0, 0) (zip p (sfV fr))

-- 画面の点 → (p1, p2, p3)（三角形の外なら負の値も出る）
xyToBary :: SFrame -> Double -> Double -> [Double]
xyToBary fr x y =
  let (a0, a1) = vtx fr 0
      (b0, b1) = vtx fr 1
      (c0, c1) = vtx fr 2
      det = (b1 - c1) * (a0 - c0) + (c0 - b0) * (a1 - c1)
      l1 = ((b1 - c1) * (x - c0) + (c0 - b0) * (y - c1)) / det
      l2 = ((c1 - a1) * (x - c0) + (a0 - c0) * (y - c1)) / det
  in [l1, l2, 1 - l1 - l2]

-- 三角形の中へ寄せる（どの確率も f 以上。p5.js 版の既定は 0.002）
clampSimplex :: [Double] -> Double -> [Double]
clampSimplex p f = mixFloor (normP p) f

sxy :: SFrame -> [Double] -> (Float, Float)   -- 描くときの Float の点
sxy fr p = let (x, y) = baryToXY fr p in (f2 x, f2 y)

vxy :: SFrame -> Int -> (Float, Float)
vxy fr i = let (x, y) = vtx fr i in (f2 x, f2 y)

-- 三角形のふちと頂点の名前（names が空なら「outcome 1」…）
drawSimplexFrame :: SFrame -> [String] -> Picture
drawSimplexFrame fr names = Pictures $
  Color (colA 170 ink) (thickPath 1.5 [vxy fr 0, vxy fr 1, vxy fr 2, vxy fr 0])
  : [ sayM (if null names then "outcome " ++ show (i + 1) else at' names i "") (x + ox) (y + oy) 14 (col (outC i))
    | (i, (ox, oy)) <- zip [0 ..] [(-16, 14), (16, 14), (0, -16)], let (x, y) = vxy fr i ]

-- 分布の列を三角形の上の折れ線に（2 点未満なら何もしない）
simplexPath :: SFrame -> [[Double]] -> Color -> Float -> Picture
simplexPath fr ps c w = Color c (polyline w (map (sxy fr) ps))

-- 点（直径 r）と名前
simplexDot :: SFrame -> [Double] -> RGB -> Float -> String -> Picture
simplexDot fr p c r name =
  let (x, y) = sxy fr p
  in Pictures [ Color (col bgC) (circleAt x y (r + 4)), Color (col c) (circleAt x y r)
              , if null name then Blank else sayLM name (x + 10) (y - 10) 15 (col c) ]

-- ★ 等高線の地図：三角形の中の各点で fn(p) を計算し、level の間隔で色の帯と線を描いた画像を作る（半分の解像度）
simplexField :: SFrame -> ([Double] -> Double) -> FieldOpt -> Field
simplexField fr fn opt =
  let pad = 4
      (v0x, _) = vtx fr 0
      (v2x, v2y) = vtx fr 2
      (_, v0y) = vtx fr 0
      x0 = min v0x v2x - pad
      y0 = v2y - pad
      w = sfSide fr + 2 * pad
      h = v0y - v2y + 2 * pad
      res = if foRes opt > 0 then foRes opt else 2
      gw = ceiling (w / i2d res) :: Int
      gh = ceiling (h / i2d res) :: Int
      lv = foLevel opt
      vmax = foVmax opt
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            b = xyToBary fr (x0 + (i2d i + 0.5) * i2d res) (y0 + (i2d j + 0.5) * i2d res)
        in if any (< 0) b then 0 / 0 else fn (map (max 1e-12) b)
      bandOf v = floor (min v (vmax * 4) / lv) :: Int
      pic = bandImage gw gh vals bandOf (/ vmax) opt 245 200
  in vals `seq` Field pic (f2 x0) (f2 y0) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 棒グラフ（ドラッグで高さを変えられる）
data BarOpt = BarOpt { boCols :: Int -> RGB, boNames :: [String], boVmax :: !Double, boGhost :: [Double] }

barOpt :: BarOpt                              -- 既定：結果の色、名前は 1 2 3 …、上限 1、重ねる分布なし
barOpt = BarOpt outC [] 1 []

drawBars :: [Double] -> Box -> BarOpt -> Picture
drawBars p r opt = Pictures $
  Color (col wire) (lineW 1 (f2 (bxX r), f2 (bxY r + bxH r)) (f2 (bxX r + bxW r), f2 (bxY r + bxH r)))
  : concat [ [ Color (colA 210 (boCols opt i)) (rectTL (x + i2f i * bw + 3) (y + h - hh) (bw - 6) hh)
             , if null (boGhost opt) then Blank
               else let gh = f2 (clampv (at' (boGhost opt) i 0) 0 vmax / vmax) * h
                    in Color (col ink) (frameTL 1.5 (x + i2f i * bw + 3) (y + h - gh) (bw - 6) gh)
             , if bw >= 34 then sayC (fmtN 2 v) (x + (i2f i + 0.5) * bw) (y + h - hh - 4) 14 (col ink) else Blank   -- 細い棒では数字を省く
             , sayC (if null (boNames opt) then show (i + 1) else at' (boNames opt) i "") (x + (i2f i + 0.5) * bw) (y + h + 17) 14 (col dimc) ]
           | (i, v) <- zip [0 ..] p, let hh = f2 (clampv v 0 vmax / vmax) * h ]
  where k = length p
        x = f2 (bxX r)
        y = f2 (bxY r)
        h = f2 (bxH r)
        bw = f2 (bxW r) / i2f (max 1 k)
        vmax = boVmax opt

-- 棒をドラッグ：i 番目の高さを変えて、残りを比例で縮める（合計 1 のまま）
barsDrag :: [Double] -> Box -> Double -> Double -> Double -> [Double]
barsDrag p r mx my fl0 =
  let k = length p
      fl = if fl0 == 0 then 0.001 else fl0
      i = floor ((mx - bxX r) / (bxW r / i2d k)) :: Int
      v = clampv ((bxY r + bxH r - my) / bxH r) fl (1 - fl * i2d (k - 1))
      rest = 1 - at' p i 0
      q = [ if j == i then v else if rest > 1e-9 then (pj / rest) * (1 - v) else (1 - v) / i2d (k - 1) | (j, pj) <- zip [0 ..] p ]
  in if i < 0 || i >= k || my < bxY r - 30 || my > bxY r + bxH r + 20 then p else mixFloor q fl

-- ---- 2 次元の図の枠（データ座標 ↔ 画面）
data PBox = PBox { pbX :: !Double, pbY :: !Double, pbW :: !Double, pbH :: !Double, pbX0 :: !Double, pbX1 :: !Double, pbY0 :: !Double, pbY1 :: !Double }

plotBox :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PBox
plotBox = PBox

pbBox :: PBox -> Box                          -- 枠の長方形（inRect 用）
pbBox b = Box (pbX b) (pbY b) (pbW b) (pbH b)

toScr :: PBox -> Double -> Double -> (Double, Double)
toScr b u v = (pbX b + (u - pbX0 b) / (pbX1 b - pbX0 b) * pbW b, pbY b + pbH b - (v - pbY0 b) / (pbY1 b - pbY0 b) * pbH b)

toScrF :: PBox -> Double -> Double -> (Float, Float)
toScrF b u v = let (x, y) = toScr b u v in (f2 x, f2 y)

fromScr :: PBox -> Double -> Double -> (Double, Double)
fromScr b sx sy = (pbX0 b + (sx - pbX b) / pbW b * (pbX1 b - pbX0 b), pbY0 b + (pbY b + pbH b - sy) / pbH b * (pbY1 b - pbY0 b))

drawAxes :: PBox -> String -> String -> Picture
drawAxes b xl yl = Pictures
  [ Color (col wire) (frameTL 1 x y w h)
  , sayT xl (x + w / 2) (y + h + 16) 14 (col dimc) True
  , sayT (fmtN 1 (pbX0 b)) x (y + h + 2) 14 (col dimc) False
  , sayR (fmtN 1 (pbX1 b)) (x + w) (y + h + 14) 14 (col dimc)
  , sayRM (fmtN 1 (pbY0 b)) (x - 4) (y + h) 14 (col dimc)
  , sayRM (fmtN 1 (pbY1 b)) (x - 4) y 14 (col dimc)
  , say yl x (y - 5) 14 (col dimc) ]
  where x = f2 (pbX b)
        y = f2 (pbY b)
        w = f2 (pbW b)
        h = f2 (pbH b)

-- データ座標の点列を折れ線に（枠の外の部分は枠のふちで切る）
plotLine :: PBox -> [(Double, Double)] -> Color -> Float -> Picture
plotLine b pts c w = Color c (Pictures (map (polyline w . map (\(x, y) -> (f2 x, f2 y))) (clipRuns b pts)))

-- 枠の中に入る部分の折れ線の列（リャン-バースキー法で線分を切る）
clipRuns :: PBox -> [(Double, Double)] -> [[(Double, Double)]]
clipRuns b pts = finish (foldl' stepP ([], [], Nothing) pts)
  where
    x0 = pbX b - 1
    x1 = pbX b + pbW b + 1
    y0 = pbY b - 1
    y1 = pbY b + pbH b + 1
    flush run done = if length run >= 2 then reverse run : done else done
    finish (run, done, _) = reverse (flush run done)
    clip (ax, ay) (cx, cy) =
      let dx = cx - ax
          dy = cy - ay
          go (t0, t1) (p, q)
            | p == 0 = if q < 0 then Nothing else Just (t0, t1)
            | otherwise = let r = q / p in Just (if p < 0 then (max t0 r, t1) else (t0, min t1 r))
          res = foldl' (\acc pq -> acc >>= \tt -> go tt pq) (Just (0, 1)) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      in case res of
           Just (t0, t1) | t0 <= t1 -> Just (t0, t1)
           _ -> Nothing
    stepP (run, done, prev) (u, v) =
      let s@(sx, sy) = toScr b u v
          good = not (isNaN sx || isNaN sy || isInfinite sx || isInfinite sy)
      in if not good then ([], flush run done, Nothing) else
         case prev of
           Nothing -> (run, done, Just s)
           Just pv@(px, py) -> case clip pv s of
             Nothing -> ([], flush run done, Just s)
             Just (t0, t1) ->
               let pa = (px + t0 * (sx - px), py + t0 * (sy - py))
                   pb = (px + t1 * (sx - px), py + t1 * (sy - py))
                   (run1, done1) = if t0 > 0 then ([pa], flush run done) else if null run then ([pa], done) else (run, done)
                   run2 = pb : run1
               in if t1 < 1 then ([], flush run2 done1, Just s) else (run2, done1, Just s)

-- 長方形の上の等高線の地図（fn(u, v)）
gridField :: PBox -> (Double -> Double -> Double) -> FieldOpt -> Field
gridField b fn opt =
  let res = if foRes opt > 0 then foRes opt else 3
      gw = ceiling (pbW b / i2d res) :: Int
      gh = ceiling (pbH b / i2d res) :: Int
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            (u, v) = fromScr b (pbX b + (i2d i + 0.5) * i2d res) (pbY b + (i2d j + 0.5) * i2d res)
        in fn u v
      lv = foLevel opt
      vmin = foVmin opt
      vmax = foVmax opt
      bandOf v = floor (clampv ((v - vmin) / lv) (-1e6) 1e6) :: Int
      pic = bandImage gw gh (U.map (\v -> if isNaN v then -1e300 else v) vals) bandOf (\v -> (v - vmin) / (vmax - vmin)) opt 235 170
  in vals `seq` Field pic (f2 (pbX b)) (f2 (pbY b)) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
