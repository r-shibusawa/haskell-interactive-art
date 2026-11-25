-- ============================================
-- 67-02 フィッシャー情報とクラメール-ラオの限界 ─ 表の確率 p のコインを n 回投げて p を推定する（p_hat = 表の数 / n）
-- スコア = 対数尤度の傾き s(x) = d/dp log P(x; p)。平均は 0、分散 = フィッシャー情報 I(p) = 1/(p(1−p))
-- どんな偏りのない推定でも分散 ≥ 1/(n I(p)) = p(1−p)/n（クラメール-ラオ）。実験をくり返すと p_hat の分散はこの限界にぴったり
-- I(p) は KL の 2 次の項 D(p‖p+dp) ≈ ½ I(p) dp²：「近くの p とどれだけ区別しやすいか」= 分布の空間の曲がり方
-- 操作：← → p / ↑↓ 投げる回数 n / スペース 実験を止める・動かす / R やり直し / マウスを下のグラフに乗せる = その p の I(p)
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

repsPerFrame :: Int
repsPerFrame = 300                            -- ★ 1 フレームでくり返す実験の数

data World = World
  { wP        :: !Double                      -- 本当の p
  , wN        :: !Int                         -- 投げる回数 n
  , wHist     :: U.Vector Int                 -- 表の数 h = 0..n ごとの実験の回数
  , wReps     :: !Int
  , wSumE     :: !Double
  , wSumE2    :: !Double
  , wRunning  :: !Bool
  , wScoreN   :: !Int                         -- スコアを数えた回数（表と裏の合計）
  , wScoreSum :: !Double
  , wScoreSum2 :: !Double
  , wRng      :: !Rng
  , wTest     :: [Check] }

main :: IO ()
main = runSketch (sketch "67-02 Fisher information" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあとに種 2025 を入れ直す
world0 :: World
world0 = resetExp (World 0.3 20 U.empty 0 0 0 True 0 0 0 (rngOf 2025) [])

resetExp :: World -> World
resetExp w = w { wHist = U.replicate (wN w + 1) 0, wReps = 0, wSumE = 0, wSumE2 = 0, wScoreN = 0, wScoreSum = 0, wScoreSum2 = 0 }

-- ★ スコア：表(x=1) なら 1/p、裏なら −1/(1−p)
scoreOf :: Bool -> Double -> Double
scoreOf x p = if x then 1 / p else -1 / (1 - p)

-- 分散 = Σ P(x) s(x)²（平均は 0）
fisherExact :: Double -> Double
fisherExact p = let a = scoreOf True p
                    b = scoreOf False p
                in p * (a * a) + (1 - p) * (b * b)

binKL :: Double -> Double -> Double
binKL a b = divKL [a, 1 - a] [b, 1 - b]

-- 1 回の実験：n 回投げて (表の数, スコアの和, スコアの 2 乗の和, 次の乱数)。rnd を投げた順に引く
oneExperiment :: Double -> Int -> Double -> Double -> Rng -> (Int, Double, Double, Rng)
oneExperiment p n = go 0 0
  where
    go !k !h !s1 !s2 !g
      | k >= n = (h, s1, s2, g)
      | otherwise = let (u, g') = rndStep g
                        x = u < p
                        s = scoreOf x p
                    in go (k + 1) (if x then h + 1 else h) (s1 + s) (s2 + s * s) g'
    go :: Int -> Int -> Double -> Double -> Rng -> (Int, Double, Double, Rng)

step :: World -> World
step w0
  | not (wRunning w0) || wReps w0 >= 200000 = w0
  | otherwise = let (w1, idx) = go repsPerFrame w0 [] in w1 { wHist = U.accum (+) (wHist w1) [ (i, 1) | i <- idx ] }
  where
    n = wN w0
    go :: Int -> World -> [Int] -> (World, [Int])
    go 0 w acc = (w, acc)
    go k w acc =
      let (h, s1, s2, g) = oneExperiment (wP w) n (wScoreSum w) (wScoreSum2 w) (wRng w)
          e = i2d h / i2d n
          i = max 0 (min n (jsRound (e * i2d n)))
          !w' = w { wSumE = wSumE w + e, wSumE2 = wSumE2 w + e * e, wReps = wReps w + 1
                  , wScoreSum = s1, wScoreSum2 = s2, wScoreN = wScoreN w + n, wRng = g }
      in go (k - 1) w' (i : acc)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Fisher information and the Cramer-Rao bound: estimating a coin's p"
             ("Left/Right p = " ++ fmtN 2 p ++ "   Up/Down tosses n = " ++ show n ++ "   Space " ++ (if wRunning w then "running" else "paused")
              ++ "   R restart   hover the lower-left graph")
  -- ---- 左：p_hat のヒストグラムと理論の正規分布（分散 = 限界）
  , drawAxes b ("p_hat = heads / n  (" ++ show reps ++ " experiments)") "count" ]
  ++ [ Color (colA 200 orange) (rectTL (x - bwid / 2 + 0.5) (f2 (pbY b + pbH b) - hh) (max 1 (bwid - 1)) hh)
     | i <- [0 .. n], let hh = i2f (wHist w `idx` i) / i2f mx * f2 (pbH b) * 0.95, let x = f2 (pbX b) + i2f i / i2f n * f2 (pbW b) ]
  ++ [ plotLine b pk (col violet) 2.5
     , Color (col ink) (lineW 1.5 (tx, f2 (pbY b)) (tx, f2 (pbY b + pbH b)))
     -- ---- 右：数値
     , say ("mean of p_hat = " ++ fmtN 4 mean ++ "  (true p = " ++ fmtN 4 p ++ ")") rx 140 16 (col ink)
     , say "  -> no bias" rx 160 14 (col dimc)
     , say ("var of p_hat (experiment) = " ++ fmtN 6 vr) rx 184 16 (col ink)
     , say ("bound 1/(n I(p)) = p(1-p)/n = " ++ fmtN 6 bound) rx 210 16 (col violet)
     , say ("ratio = " ++ fmtN 4 (vr / bound) ++ "  (-> 1: right at the bound)") rx 236 16 (col (if abs (vr / bound - 1) < 0.05 then lime else sand))
     , say ("score s(x): heads 1/p = " ++ fmtN 3 (scoreOf True p) ++ ",") rx 276 16 (col ink)
     , say ("            tails -1/(1-p) = " ++ fmtN 3 (scoreOf False p)) rx 298 16 (col ink)
     , say ("mean score (experiment) = " ++ fmtN 4 sm ++ "  -> 0") rx 326 16 (col ink)
     , say ("var of score (experiment) = " ++ fmtN 4 sv) rx 352 16 (col ink)
     , say ("exact sum P(x) s(x)^2 = " ++ fmtN 4 (fisherExact p)) rx 376 16 (col ink)
     , say ("I(p) = 1/(p(1-p)) = " ++ fmtN 4 (1 / (p * (1 - p)))) rx 404 16 (col teal)
     -- ---- 下：I(p) のグラフと KL の 2 次近似
     , drawAxes b2 "p" "I(p) = 1/(p(1-p)): big near 0 and 1 (easy to tell)"
     , plotLine b2 ip (col teal) 2.5
     , Color (col orange) (circleAt (f2 hx) (f2 hy) 10)
     , drawAxes b3 ("q  (around p = " ++ fmtN 2 ph ++ ")") "D(p||q) (orange) and 1/2 I(p)(q-p)^2 (green)"
     , plotLine b3 kl (col orange) 2.5, plotLine b3 q2 (col teal) 1.5
     , drawSelfTest env (wTest w) ]
  where
    p = wP w
    n = wN w
    reps = wReps w
    idx v i = if i >= 0 && i < U.length v then v U.! i else 0
    mean = if reps > 0 then wSumE w / i2d reps else 0
    vr = if reps > 1 then (wSumE2 w - i2d reps * mean * mean) / i2d (reps - 1) else 0
    bound = p * (1 - p) / i2d n
    ww = d2 (envW env)
    hh0 = d2 (envH env)
    b = plotBox 60 120 (ww * 0.5) (hh0 * 0.42) 0 1 0 1
    mx = max 1 (U.foldl' max 0 (wHist w))
    bwid = f2 (pbW b) / i2f n                 -- 表の数 h = 0, 1, …, n ごとに 1 本
    sc = (i2d reps / i2d mx) * (1 / i2d n) * 0.95   -- 正規分布の高さ = 回数 × 密度 × 幅 1/n
    pk = [ (x, min 1 (gaussPdf x p (sqrt bound) * sc)) | k <- [0 .. 200 :: Int], let x = i2d k / 200 ]
    tx = fst (toScrF b p 0)
    rx = f2 (ww * 0.6)
    nS = wScoreN w
    sm = if nS > 0 then wScoreSum w / i2d nS else 0
    sv = if nS > 0 then wScoreSum2 w / i2d nS - sm * sm else 0
    b2 = plotBox 60 (hh0 * 0.66) (ww * 0.38) (hh0 * 0.25) 0 1 0 25
    ip = [ (x, min 25 (1 / (x * (1 - x)))) | k <- [1 .. 199 :: Int], let x = i2d k / 200 ]
    (mxp, myp) = mouseD env
    ph = if inRect mxp myp (pbBox b2) then clampv (fst (fromScr b2 mxp myp)) 0.02 0.98 else p
    (hx, hy) = toScr b2 ph (min 25 (1 / (ph * (1 - ph))))
    b3 = plotBox (ww * 0.52) (hh0 * 0.66) (ww * 0.4) (hh0 * 0.25) (ph - 0.25) (ph + 0.25) 0 0.3
    xs3 = [ x | k <- [0 .. 200 :: Int], let x = ph - 0.25 + 0.5 * i2d k / 200, x > 0.001 && x < 0.999 ]
    kl = [ (x, min 0.3 (binKL ph x)) | x <- xs3 ]
    q2 = [ (x, min 0.3 (0.5 * (x - ph) * (x - ph) / (ph * (1 - ph)))) | x <- xs3 ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetExp w { wP = clampv (i2d (jsRound ((wP w - 0.05) * 100)) / 100) 0.05 0.95 }
  EventKey (SpecialKey KeyRight) Down _ _ -> resetExp w { wP = clampv (i2d (jsRound ((wP w + 0.05) * 100)) / 100) 0.05 0.95 }
  EventKey (SpecialKey KeyUp) Down _ _ -> resetExp w { wN = min 200 (wN w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetExp w { wN = max 5 (jsRound (i2d (wN w) / 2)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (Char c) Down _ _ | c == 'r' || c == 'R' -> resetExp w
  _ -> w

-- ---------- やってみよう ----------
-- 1. p を 0.5 と 0.9 にして、同じ n でヒストグラムの幅を比べよう。どちらが推定しやすい？ I(p) の値と比べよう
-- 2. n を 4 倍にすると、ヒストグラムの幅（標準偏差）は何倍になる？
-- 3. 推定を「(表の数 + 1) / (n + 2)」に変えると分散は限界より小さくなる？ 平均は p からずれる？（偏りのある推定）
-- 4. 下右のグラフで、D(p‖q) と 2 次近似がずれ始めるのはどのくらい離れたとき？
-- 5. 次の 03 では、正規分布 N(μ, σ) の 2 つのパラメータで同じことをすると、曲がった平面（双曲平面）が現れる

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("スコアの分散（厳密な和）= 1/(p(1−p))、p = 0.1, 0.3, 0.5, 0.77, 0.9 で誤差 1e-12", and [ abs (fisherExact q * q * (1 - q) - 1) <= 1e-12 | q <- ps ])
  , ("スコアの平均 = 0（どの p でも）", and [ abs (q * scoreOf True q + (1 - q) * scoreOf False q) <= 1e-12 | q <- ps ])
  , ("KL の 2 階微分 d²/dq² D(p‖q)|q=p = I(p)（数値微分、相対誤差 1e-4）", and [ abs (d2KL q / fisherExact q - 1) <= 1e-4 | q <- ps ])
  , ("モンテカルロ 4 万回：スコアの分散 " ++ fmtN 3 mv ++ " ≈ I(0.3) = " ++ fmtN 3 (1 / 0.21) ++ "（3% 以内）", abs (mv * 0.21 - 1) < 0.03)
  , ("n = 50 回の実験を 2 万回：p_hat の分散 " ++ fmtN 6 vv ++ " は限界 p(1−p)/n = " ++ fmtN 6 0.0042 ++ " の 5% 以内", abs (vv / 0.0042 - 1) < 0.05)
  , ("p_hat の平均は p（偏りなし）：" ++ fmtN 4 (e1 / i2d nR), abs (e1 / i2d nR - p0) < 0.003) ]
  where
    ps = [0.1, 0.3, 0.5, 0.77, 0.9]
    d2KL q = let h = 1e-3 in (binKL q (q + h) - 2 * binKL q q + binKL q (q - h)) / (h * h)   -- KL の 2 階微分（数値）
    p0 = 0.3
    nMC = 40000 :: Int
    (_, s1, s2, _) = oneExperiment p0 nMC 0 0 (rngOf 99)
    mv = s2 / i2d nMC - (s1 / i2d nMC) * (s1 / i2d nMC)
    nR = 20000 :: Int
    nn = 50 :: Int
    (e1, e2) = goR nR 0 0 (rngOf 123)
    goR :: Int -> Double -> Double -> Rng -> (Double, Double)
    goR 0 !a !b _ = (a, b)
    goR k !a !b g = let (h, _, _, g') = oneExperiment p0 nn 0 0 g
                        e = i2d h / i2d nn
                    in goR (k - 1) (a + e) (b + e * e) g'
    vv = (e2 - i2d nR * (e1 / i2d nR) * (e1 / i2d nR)) / i2d (nR - 1)

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
