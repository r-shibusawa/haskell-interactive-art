-- ============================================
-- 67-11 α-ダイバージェンスの家族 ─ D_α(p‖q) = 4/(1−α²)·(1 − Σ p_i^((1−α)/2) q_i^((1+α)/2))
-- α → −1 で KL(p‖q)、α → +1 で KL(q‖p)、α = 0 で 2Σ(√p − √q)²（ヘリンジャー距離の仲間、左右対称）
-- α-測地線：u^((1−α)/2) の座標で混ぜて正規化。α = −1 は m-測地線、α = +1 は e-測地線、その間をなめらかに変わる
-- 右下：2 山の分布 p を 1 山の正規分布 q で近似。α = −1（KL(p‖q)）は「ゼロを避ける」= 両方の山をおおう、α = +1（KL(q‖p)）は「ゼロを許す」= 片方の山だけ
-- 操作：↑↓ α / ドラッグ p・q（三角形）/ スペース α を自動で往復
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Control.Exception (evaluate)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

alphas :: [Double]
alphas = [-1, -0.5, 0, 0.5, 1]                -- ★ うすく描く α-測地線

xN :: Int
xN = 240                                      -- ★ 近似のための x の区切り

xLo, xHi :: Double
xLo = -8
xHi = 8

-- 近似の結果：μ, σ, D_α
data Fit = Fit { ftM :: !Double, ftS :: !Double, ftV :: !Double }

data World = World
  { wAlpha :: !Double
  , wP     :: [Double]
  , wQ     :: [Double]
  , wGrab  :: !Int                            -- 0 なし、1 p、2 q
  , wAuto  :: !Bool
  , wTa    :: !Double                         -- 自動で往復するときの角度
  , wFits  :: M.Map Int Fit                   -- α（10 倍した整数）ごとの近似（一度計算したら覚えておく）
  , wTest  :: [Check] }

main :: IO ()
main = runSketch (sketch "67-11 Alpha-divergence family" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      -- α は 0.1 きざみの 21 通りだけなので、近似は最初に全部計算しておく（動かしている途中で止まらないように）
      let fits = M.fromList [ (k, fitGauss (i2d k / 10)) | k <- [-10 .. 10] ]
      _ <- evaluate (sum [ ftM f + ftS f + ftV f | f <- M.elems fits ])
      pure world0 { wTest = selfTest, wFits = fits }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World (-1) [0.7, 0.2, 0.1] [0.1, 0.25, 0.65] 0 False 0 M.empty []

frameOf :: Env -> SFrame
frameOf env = let side = min (d2 (envW env) * 0.38) ((d2 (envH env) - 300) / 0.91)
              in simplexFrame (50 + side / 2) (130 + side * 0.62) side

xAt :: Int -> Double
xAt i = xLo + (xHi - xLo) * (i2d i + 0.5) / i2d xN

-- 合計が 1 になるように（normP と同じ計算を unboxed で）
normU :: U.Vector Double -> U.Vector Double
normU v = let s = U.foldl' (\a x -> a + (if x > 0 then x else 0)) 0 v
          in if not (s > 0) then U.map (const (1 / i2d (U.length v))) v else U.map (\x -> (if x > 0 then x else 0) / s) v

-- ★ 2 山の分布
target :: U.Vector Double
target = normU (U.generate xN (\i -> 0.35 * gaussPdf (xAt i) (-2.2) 0.7 + 0.65 * gaussPdf (xAt i) 2 0.9 + 1e-12))

gaussBins :: Double -> Double -> U.Vector Double
gaussBins m s = normU (U.generate xN (\i -> gaussPdf (xAt i) m s + 1e-300))

-- alphaDiv と同じ計算を unboxed で（近似を探すときにたくさん呼ぶので）
alphaDivU :: U.Vector Double -> U.Vector Double -> Double -> Double
alphaDivU p q a
  | abs (a + 1) < 1e-12 = klU p q
  | abs (a - 1) < 1e-12 = klU q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = U.foldl' (+) 0 (U.zipWith (\x y -> pos x ** ea * pos y ** eb) p q)
                    v = (4 / (1 - a * a)) * (1 - s)
                in if v > 0 then v else 0
  where pos x = if x > 0 then x else 0

klU :: U.Vector Double -> U.Vector Double -> Double
klU p q = go 0 0
  where n = U.length p
        go !i !d
          | i >= n = if d > 0 then d else 0
          | otherwise = let a = U.unsafeIndex p i
                            b = U.unsafeIndex q i
                        in if a <= 0 then go (i + 1) d else if b <= 0 then posInf else go (i + 1) (d + a * log (a / b))

-- ★ q = N(μ, σ) の中で D_α(p‖q) が最小のものを探す：粗い格子 → 細かく（パターン探索）
fitGauss :: Double -> Fit
fitGauss a = search 0.2 (grid (0, 1, posInf) [ (-4 + 8 * i2d i / 40, 0.3 + 3.7 * i2d j / 30) | i <- [0 .. 40 :: Int], j <- [0 .. 30 :: Int] ])
  where
    obj m s = alphaDivU target (gaussBins m s) a
    grid = foldl' (\b@(_, _, bv) (m, s) -> let v = obj m s in if v < bv then (m, s, v) else b)
    search :: Double -> (Double, Double, Double) -> Fit
    search h best@(bm, bs, bv)
      | not (h > 1e-5) = Fit bm bs bv
      | otherwise =
          let try1 (b@(m0, s0, v0), mv) (dm, ds) =
                let m = m0 + dm
                    s = max 0.05 (s0 + ds)
                    v = obj m s
                in if v < v0 - 1e-15 then ((m, s, v), True) else (b, mv)
              (best', moved) = foldl' try1 (best, False) [(h, 0), (-h, 0), (0, h), (0, -h)]
          in search (if moved then h else h / 2) best'

fitOf :: World -> Fit
fitOf w = M.findWithDefault (Fit 0 1 0) (jsRound (wAlpha w * 10)) (wFits w)

step :: World -> World
step w = let w1 = if wAuto w then let t = wTa w + 0.006 in w { wTa = t, wAlpha = i2d (jsRound (-(cos t) * 10)) / 10 } else w
             k = jsRound (wAlpha w1 * 10)
         in if M.member k (wFits w1) then w1 else w1 { wFits = M.insert k (fitGauss (wAlpha w1)) (wFits w1) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("alpha-divergence: from KL(p||q) to KL(q||p)   alpha = " ++ fmtN 1 al)
             ("Up/Down alpha   Drag p, q   Space auto sweep of alpha " ++ (if wAuto w then "ON" else "OFF"))
  -- ---- 左：三角形と α-測地線
  , drawSimplexFrame fr ["#1", "#2", "#3"] ]   -- 頂点の名前（p5.js 版は「結果 1」…）
  ++ [ simplexPath fr [ mixAlpha (wP w) (wQ w) (i2d k / 50) a | k <- [0 .. 50 :: Int] ]
                   (if a == -1 then colA 110 orange else if a == 1 then colA 110 violet else colA 60 ink) 1.5 | a <- alphas ]
  ++ [ simplexPath fr [ mixAlpha (wP w) (wQ w) (i2d k / 60) al | k <- [0 .. 60 :: Int] ] (col cc) 4
     , simplexDot fr (wP w) ink 16 "p", simplexDot fr (wQ w) ink 16 "q"
     , say "alpha = -1: m-geodesic (straight)" 40 (f2 v0y + 34) 14 (col orange)
     , say "alpha = +1: e-geodesic" 40 (f2 v0y + 56) 14 (col violet)
  -- ---- 右上：α と D_α(p‖q)
     , drawAxes b1 "alpha" "D_alpha(p||q)"
     , plotLine b1 cv (col teal) 2.5
     , let (ax, ay) = toScrF b1 al dNow in Color (col cc) (circleAt ax ay 12)
     , say ("D_alpha = " ++ fmtN 5 dNow ++ "   KL(p||q) = " ++ fmtN 5 (divKL (wP w) (wQ w)) ++ " (alpha = -1)") (f2 rx) (ty0 + 0) 14 (col ink)
     , say ("KL(q||p) = " ++ fmtN 5 (divKL (wQ w) (wP w)) ++ " (alpha = +1)") (f2 rx) (ty0 + 19) 14 (col ink)
     , say ("alpha = 0: 2 sum (sqrt p - sqrt q)^2 = " ++ fmtN 5 (2 * hellingerSq (wP w) (wQ w))) (f2 rx) (ty0 + 38) 14 (col ink)
     , say ("dual: D_alpha(p||q) = D_-alpha(q||p) = " ++ fmtN 5 (alphaDiv (wQ w) (wP w) (-al))) (f2 rx) (ty0 + 57) 14 (col ink)
  -- ---- 右下：2 山を 1 山で近似（確率 ÷ 幅 = 密度で描く）
     , drawAxes b2 "x" "p with 2 peaks (white) vs best N(mu, sigma)   density"
     , plotLine b2 [ (xAt i, (target U.! i) / dx) | i <- [0 .. xN - 1] ] (col ink) 2.5
     , plotLine b2 [ (xAt i, (q U.! i) / dx) | i <- [0 .. xN - 1] ] (col cc) 3
     , say ("best q: mu = " ++ fmtN 3 (ftM ft) ++ ", sigma = " ++ fmtN 3 (ftS ft) ++ "   D_alpha = " ++ fmtN 4 (ftV ft)) (f2 rx) (f2 (pbY b2 + pbH b2) + 54) 14 (col cc)
     , say (if al < -0.3 then "-> covers the whole p" else if al > 0.3 then "-> picks one peak" else "-> in between") (f2 rx) (f2 (pbY b2 + pbH b2) + 73) 14 (col cc)
     , drawSelfTest env (wTest w) ]
  where
    fr = frameOf env
    al = wAlpha w
    (_, v0y) = vtx fr 0
    t = f2 ((al + 1) / 2)
    cc = let (a0, a1, a2) = orange
             (b0, b1', b2') = violet
         in (a0 * (1 - t) + b0 * t, a1 * (1 - t) + b1' * t, a2 * (1 - t) + b2' * t)
    rx = d2 (envW env) * 0.5
    cv = [ (a, alphaDiv (wP w) (wQ w) a) | k <- [0 .. 100 :: Int], let a = -1 + i2d k / 50 ]
    b1 = plotBox rx 110 (d2 (envW env) * 0.44) (d2 (envH env) * 0.27) (-1) 1 0 (maxOr 0 (map snd cv) * 1.15 + 1e-6)
    dNow = alphaDiv (wP w) (wQ w) al
    ty0 = f2 (pbY b1 + pbH b1) + 52
    ft = fitOf w
    dx = (xHi - xLo) / i2d xN
    q = gaussBins (ftM ft) (ftS ft)
    b2 = plotBox rx (d2 (envH env) * 0.58) (d2 (envW env) * 0.44) (d2 (envH env) * 0.28) xLo xHi 0 (max (U.maximum target) (U.maximum q) / dx * 1.1)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let near p = let (x, y) = baryToXY fr p in sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int)) < 22
    in w { wGrab = if near (wQ w) then 2 else if near (wP w) then 1 else 0 }
  EventMotion _
    | wGrab w == 1 -> w { wP = clampSimplex (xyToBary fr mx my) 0.003 }
    | wGrab w == 2 -> w { wQ = clampSimplex (xyToBary fr mx my) 0.003 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAuto = False, wAlpha = clampv (i2d (jsRound ((wAlpha w + 0.1) * 10)) / 10) (-1) 1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAuto = False, wAlpha = clampv (i2d (jsRound ((wAlpha w - 0.1) * 10)) / 10) (-1) 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w), wTa = acos (clampv (-(wAlpha w)) (-1) 1) }
  _ -> w
  where fr = frameOf env
        (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. α を −1 から +1 まで動かして、右下の近似がどこで「両方おおう」から「片方」に切りかわるか探そう
-- 2. 2 つの山の重み（0.35 と 0.65）を同じにすると、α = +1 の近似はどちらの山を選ぶ？
-- 3. 三角形で p を辺の近くに置くと、α によって測地線の形はどれくらいちがう？
-- 4. 機械学習の「変分推論」は KL(q‖p) を、「最尤推定」は KL(p‖q) を小さくする。どちらが 1 つの山にしぼる？
-- 5. α = 3 など範囲の外も式では計算できる。どんな近似になる？（alphas と ↑↓ の範囲を変えて）

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("α → −1 で KL(p‖q)、α → +1 で KL(q‖p)（α = ±(1 − 10⁻⁶)、100 組、相対誤差 1e-4）", all okLim pairs)
  , ("α = 0 は 2Σ(√p − √q)²（誤差 1e-12）", all (\(p, q) -> not (abs (alphaDiv p q 0 - 2 * hellingerSq p q) > 1e-12)) pairs)
  , ("双対 D_α(p‖q) = D_−α(q‖p)、D_α ≥ 0 で p = q のときだけ 0"
    , and [ not (abs (alphaDiv p q a - alphaDiv q p (-a)) > 1e-12) && alphaDiv p q a > 0 && not (alphaDiv p p a > 1e-12) | (p, q) <- pairs, a <- [-0.7, 0.3, 0.9] ])
  , ("α-測地線：α = −1 は m-測地線、α = +1 は e-測地線、α → 1 で e に近づく", okG && and (zipWith (\v e -> abs (v - e) < 1e-6) nearE e5))
  , ("KL(p‖q) 最小の正規分布は p と平均・標準偏差が同じ（モーメント合わせ：" ++ fmtN 3 qm ++ ", " ++ fmtN 3 qs ++ "、誤差 1e-3）"
    , abs (qm - mu) < 1e-3 && abs (qs - sd) < 1e-3)
  , ("KL(q‖p) 最小は片方の山へ（σ = " ++ fmtN 2 (ftS fz) ++ " ＜ " ++ fmtN 2 (ftS fm) ++ "、μ = " ++ fmtN 2 (ftM fz) ++ " は大きい山の近く）"
    , ftS fz < 0.6 * ftS fm && abs (ftM fz - 2) < 0.4) ]
  where
    pairs = withSeed 111 (mapM (\_ -> do p <- rndDist 4 0.01
                                         q <- rndDist 4 0.01
                                         pure (p, q)) [1 .. 100 :: Int])
    e = 1e-6
    okLim (p, q) = not (abs (alphaDiv p q (-1 + e) - divKL p q) > 1e-4 * (1 + divKL p q) || abs (alphaDiv p q (1 - e) - divKL q p) > 1e-4 * (1 + divKL q p))
    p3 = [0.6, 0.3, 0.1]
    q3 = [0.1, 0.2, 0.7]
    okG = and [ and (zipWith (\x y -> not (abs (x - y) > 1e-12)) (mixAlpha p3 q3 t (-1)) (mixM p3 q3 t))
                && and (zipWith (\x y -> not (abs (x - y) > 1e-12)) (mixAlpha p3 q3 t 1) (mixE p3 q3 t)) | t <- [0.25, 0.5, 0.8] ]
    nearE = mixAlpha p3 q3 0.5 (1 - 1e-7)
    e5 = mixE p3 q3 0.5
    mom d = let m = sumOf [ (d U.! i) * xAt i | i <- [0 .. xN - 1] ]
            in (m, sqrt (sumOf [ (d U.! i) * (xAt i - m) ^ (2 :: Int) | i <- [0 .. xN - 1] ]))
    fm = fitGauss (-1)
    (mu, sd) = mom target
    (qm, qs) = mom (gaussBins (ftM fm) (ftS fm))
    fz = fitGauss 1

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

-- ---- 正規分布 N(μ, σ) の道具。フィッシャー計量 ds² = (dμ² + 2dσ²) / σ²
gaussPdf :: Double -> Double -> Double -> Double
gaussPdf x m s = exp (-((x - m) * (x - m)) / (2 * s * s)) / (s * sqrt (2 * pi))

-- フィッシャー-ラオ距離（閉じた式）
gaussFR :: Double -> Double -> Double -> Double -> Double
gaussFR m1 s1 m2 s2 =
  let a = (m1 - m2) * (m1 - m2) / 2 + (s1 - s2) * (s1 - s2)
  in sqrt 2 * acosh (1 + a / (2 * s1 * s2))

-- D(N1‖N2)
gaussKL :: Double -> Double -> Double -> Double -> Double
gaussKL m1 s1 m2 s2 = log (s2 / s1) + (s1 * s1 + (m1 - m2) * (m1 - m2)) / (2 * s2 * s2) - 0.5

-- ★ 測地線：u = μ/√2 と置くと (u, σ) では半円（中心は σ = 0 の上）→ (μ, σ) では横に √2 倍の半楕円
gaussGeodesic :: Double -> Double -> Double -> Double -> Int -> [(Double, Double)]
gaussGeodesic m1 s1 m2 s2 n
  | abs (u1 - u2) < 1e-9 * (1 + abs u1) = [ (m1, s1 * (s2 / s1) ** (i2d k / i2d n)) | k <- [0 .. n] ]
  | otherwise = [ ((c + rr * cos t) * sqrt 2, rr * sin t) | k <- [0 .. n], let t = t1 + (t2 - t1) * i2d k / i2d n ]
  where u1 = m1 / sqrt 2
        u2 = m2 / sqrt 2
        c = (u2 * u2 + s2 * s2 - u1 * u1 - s1 * s1) / (2 * (u2 - u1))
        rr = sqrt ((u1 - c) * (u1 - c) + s1 * s1)
        t1 = atan2 s1 (u1 - c)
        t2 = atan2 s2 (u2 - c)

-- 折れ線の長さをフィッシャー計量で（区間の真ん中の σ を使う）
metricLength :: [(Double, Double)] -> Double
metricLength path = sumOf [ sqrt (dm * dm + 2 * ds * ds) / sm
                          | ((a0, b0), (a1, b1)) <- zip path (drop 1 path), let dm = a1 - a0, let ds = b1 - b0, let sm = (b1 + b0) / 2 ]

-- 測地線の方程式（クリストッフェル記号から）：μ'' = 2μ'σ'/σ、σ'' = −μ'²/(2σ) + σ'²/σ を RK4 で解く
geodesicRK4 :: Double -> Double -> Double -> Double -> Double -> Int -> [(Double, Double)]
geodesicRK4 m s vm vs tT n = go n (m, s, vm, vs) [(m, s)]
  where
    h = tT / i2d n
    fd (_, b, c, d) = (c, d, 2 * c * d / b, -(c * c) / (2 * b) + d * d / b)
    ad (a, b, c, d) k (e, f, g, hh) = (a + k * e, b + k * f, c + k * g, d + k * hh)
    go 0 _ acc = reverse acc
    go j st acc =
      let k1 = fd st
          k2 = fd (ad st (h / 2) k1)
          k3 = fd (ad st (h / 2) k2)
          k4 = fd (ad st h k3)
          (a, b, c, d) = st
          (e1, f1, g1, h1) = k1
          (e2, f2', g2, h2) = k2
          (e3, f3, g3, h3) = k3
          (e4, f4, g4, h4) = k4
          a' = a + (h / 6) * (e1 + 2 * e2 + 2 * e3 + e4)
          b0' = b + (h / 6) * (f1 + 2 * f2' + 2 * f3 + f4)
          c' = c + (h / 6) * (g1 + 2 * g2 + 2 * g3 + g4)
          d' = d + (h / 6) * (h1 + 2 * h2 + 2 * h3 + h4)
          b' = if b0' > 1e-6 then b0' else 1e-6
      in a' `seq` b' `seq` c' `seq` d' `seq` go (j - 1) (a', b', c', d') ((a', b') : acc)

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
