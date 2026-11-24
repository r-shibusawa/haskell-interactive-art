-- ============================================
-- 65-01 n ステップ後の分布 ─ 「いまどこにいそうか」の確率（分布）に、遷移行列 P を 1 回掛けるごとに 1 歩すすむ
-- μ_n = μ_0 P^n。P^n の (i, j) 成分 = i から始めて n 歩後に j にいる確率。
-- n を大きくすると P^n のどの行も同じになっていく（どこから始めても同じ分布へ = 始まりを忘れる）。
-- チャップマン-コルモゴロフ方程式 P^(m+n) = P^m P^n：「m 歩してから n 歩」= 「m + n 歩」を数値で確かめる
-- 操作：スペース 1 歩 / A 自動 / R n = 0 に戻す / 1〜4 スタートの場所 / 5 均等にスタート / ↑↓ m（チャップマン-コルモゴロフの m）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

placeNames :: [String]
placeNames = ["Home", "School", "Park", "Shop"]   -- 家・学校・公園・店

transP :: Mat                                 -- ★ 遷移行列（4 つの場所を 1 時間ごとに移動する人、各行の合計 = 1）
transP =
  [ [0.5, 0.3, 0.1, 0.1]
  , [0.2, 0.5, 0.2, 0.1]
  , [0.3, 0.0, 0.4, 0.3]
  , [0.4, 0.1, 0.2, 0.3] ]

nMax :: Int
nMax = 40                                     -- ★ グラフに描く n の上限

-- P^0, P^1, …, P^40（くり返し掛ける）と、各 P^n の「行どうしのちがい」
powers :: [Mat]
powers = take (nMax + 1) (iterate (`matMul` transP) (eyeMat 4))

spreads :: [Double]
spreads = map colSpread powers

piVec :: Vec
piVec = maybe (replicate 4 0.25) id (stationaryExact transP)

-- P^n の各列で「いちばん大きい行 − いちばん小さい行」の最大：0 なら全部の行が同じ
colSpread :: Mat -> Double
colSpread a = foldl' max 0 [ maxOr 0 c - minOr 0 c | j <- [0 .. length a - 1], let c = [ at' r j 0 | r <- a ] ]

data World = World
  { wMu0   :: Vec                             -- はじめの分布
  , wN     :: !Int                            -- いまの n
  , wDists :: [Vec]                           -- μ_0, μ_1, …, μ_n
  , wShown :: Vec                             -- 画面の棒（なめらかに動かす）
  , wAuto  :: !Bool
  , wCkM   :: !Int                            -- チャップマン-コルモゴロフの m
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-01 n-step distribution" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart [1, 0, 0, 0] (World [] 0 [] [] True 3 [])

restart :: Vec -> World -> World
restart m w = w { wMu0 = m, wN = 0, wDists = [m], wShown = m }

-- ★ μ_(n+1) = μ_n P
oneStep :: World -> World
oneStep w
  | wN w >= nMax = w
  | otherwise = w { wDists = wDists w ++ [vecMat (lastOr [] (wDists w)) transP], wN = wN w + 1 }

step :: Env -> World -> World
step env w0 =
  let w = if wAuto w0 && envFrame env `mod` 40 == 0 then oneStep w0 else w0
      target = at' (wDists w) (wN w) []
  in w { wShown = forceL (zipWith (\s t -> s + (t - s) * 0.15) (wShown w) target) }   -- 棒をなめらかに動かす

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle ("n-step distribution: n = " ++ show n ++ "   mu_n = mu_0 P^n")
             ("Space 1 step   A auto (" ++ (if wAuto w then "ON" else "OFF") ++ ")   R n = 0   1-4 start place   5 uniform start   Up/Down m = " ++ show (wCkM w))
  -- ---- いまの分布（棒）と定常分布（線）
  , say ("mu_" ++ show n ++ " (bars), its limit pi (white lines)") bx (by - 12) 15 (col cream)
  , drawBars bx by bw bh (wShown w) stateCol 1 (Just placeNames)
  , Pictures [ Pictures [ Color (col cream) (lineW 2 (x - bw / 8, y) (x + bw / 8, y))
                        , sayC (fmtN 3 (at' target k 0)) x (by + bh - f2 (at' (wShown w) k 0) * bh - 4) 14 (col cream) ]
             | k <- [0 .. 3], let x = bx + (i2f k + 0.5) * (bw / 4), let y = by + bh - f2 (at' piVec k 0) * bh ]
  -- ---- これまでの分布を積み上げた帯（左 = n 0）
  , say "mu_n for n = 0, 1, 2, ... (height = prob.)" sx (sy - 10) 14 (col cream)
  , Pictures [ Color (col (stateCol k)) (rectTL (sx + i2f m * cw) (sy + f2 y0 * sh) (max 1 (cw - 1)) (f2 p * sh))
             | (m, d) <- zip [0 :: Int ..] (wDists w), (k, p, y0) <- zip3 [0 ..] d (scanl (+) 0 d) ]
  , Color (col (84, 76, 70)) (frameTL 1 sx sy sw sh)
  -- ---- P^n の表
  , say ("P^" ++ show n ++ " (row = start, column = after n steps)") mx (my - 30) 15 (col cream)
  , drawMatrix (at' powers n []) (mx + 64) my 62 30 (Just placeNames) 14 3 1
  , say ("Large n: every row -> pi = (" ++ commaList (map (fmtN 3) piVec) ++ ")") mx (my + 4 * 30 + 24) 14 (col sub)
  -- ---- 行のちがい（対数）
  , plotAxes g "n" "log10 (largest difference between rows of P^n)" [0, 10, 20, 30, 40] [-16, -8, 0]
  , drawCurve g (map (\d -> logBase 10 (max d 1e-16)) spreads) teal 2.5 0
  , Color (col dice) (circleAt (gX g (i2d n)) (gY g (logBase 10 (max (at' spreads n 0) 1e-16))) 10)
  -- ---- チャップマン-コルモゴロフ
  , say ("Chapman-Kolmogorov: P^(" ++ show m ++ "+" ++ show ckN ++ ") = P^" ++ show m ++ " x P^" ++ show ckN) mx cy 16 (col cream)
  , say ("largest difference = " ++ fmtE (maxAbsDiff lhs rhs) ++ " (rounding error only)") mx (cy + 26) 16 (col mustard)
  , say "\"m steps to some k, then n steps\" summed over every k = m + n steps" mx (cy + 50) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    n = wN w
    m = wCkM w
    target = at' (wDists w) n []
    bx = 40
    by = 120
    bw = min 380 (envW env * 0.3)
    bh = 220
    sx = bx
    sy = by + bh + 70
    sw = bw
    sh = 120
    cw = sw / i2f (nMax + 1)
    mx = bx + bw + 70
    my = 124
    g = Graph (mx + 40) (my + 200) (min 440 (envW env - mx - 90)) 170 0 (i2d nMax) (-16) 0
    ckN = max 1 n
    lhs = if m + ckN <= nMax then at' powers (m + ckN) [] else at' powers nMax []   -- p5.js 版と同じく、表にある P^40 まで
    rhs = matMul (at' powers m (matPow transP m)) (at' powers ckN [])
    cy = grY g + grH g + 64

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> oneStep w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wCkM = min 20 (wCkM w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wCkM = max 0 (wCkM w - 1) }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'r' || c == 'R' -> restart (wMu0 w) w
    | c >= '1' && c <= '4' -> restart (setAt (fromEnum c - fromEnum '1') 1 [0, 0, 0, 0]) w
    | c == '5' -> restart [0.25, 0.25, 0.25, 0.25] w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1〜4 でスタートを変えても、n = 20 くらいで棒はいつも同じ高さ（π）になる？
-- 2. transP の 1 行を [0, 0, 0, 1] にしたり、家 ⇔ 学校 だけを行き来する行列にすると、行はそろう？（03 の分類へ）
-- 3. ログのグラフはほぼ直線 = ちがいが毎回ほぼ同じ割合で縮む。その割合は P の 2 番目に大きい固有値の大きさ
-- 4. ↑↓ で m を変えて、P^(m+n) と P^m P^n のずれがいつも 10⁻¹⁶ くらいなのを見よう

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("遷移行列の各行の合計 = 1", rowSumErr transP 1 < 1e-12)
  , ("P^n（n = 1〜60）の各行の合計 = 1", all (\a -> rowSumErr a 1 < 1e-12) pw)
  , ("チャップマン-コルモゴロフ P^(m+n) = P^m P^n（m, n ≤ 10、ずれ " ++ fmtE ck ++ "）", ck < 1e-12)
  , ("μ に P を 12 回掛ける = μ P^12", maxAbsDiffV v12 (vecMat mu (pw !! 12)) < 1e-14)
  , ("P^60 のどの行も定常分布 π（ずれ < 10⁻¹⁰）", all (\r -> maxAbsDiffV r pv < 1e-10) (pw !! 60))
  , ("行どうしのちがいは n とともに増えない（列の幅が縮む）", and [ colSpread (pw !! k) <= colSpread (pw !! (k - 1)) + 1e-15 | k <- [1 .. 60] ])
  , ("家から 5 歩のシミュレーション（4 万回）≈ P^5 の 1 行目（ずれ < 0.01）", maxAbsDiffV (map (\c -> i2d c / i2d runs) cnt) (head (pw !! 5)) < 0.01) ]
  where
    pw = take 61 (iterate (`matMul` transP) (eyeMat 4))
    ck = foldl' maxN 0 [ maxAbsDiff (pw !! (m + n)) (matMul (pw !! m) (pw !! n)) | m <- [0 .. 10], n <- [0 .. 10] ]
    mu = [0.1, 0.2, 0.3, 0.4]
    v12 = iterate (`vecMat` transP) mu !! 12
    pv = maybe [] id (stationaryExact transP)
    runs = 40000 :: Int
    -- シミュレーション：家から 5 歩後にどこにいる？
    cnt = withSeed 11 (loopR runs walk5 [0, 0, 0, 0])
    walk5 c = do
      s <- loopR 5 (\s0 -> sampleFrom (transP !! s0)) 0
      pure (forceI (modifyAt s (+ 1) c))

forceI :: [Int] -> [Int]
forceI xs = foldl' (flip seq) () xs `seq` xs

commaList :: [String] -> String
commaList xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ ", " ++ b) xs

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

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

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
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

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

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

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- 遷移図を描く道具：矢印の太さ = 確率
-- 少しふくらんだ曲線の点（両はしの円 r の中は除く）
bezPts :: (Float, Float) -> (Float, Float) -> Float -> Float -> [(Float, Float)]
bezPts (ax, ay) (bx, by) bend r =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      dx = bx - ax
      dy = by - ay
      l0 = sqrt (dx * dx + dy * dy)
      l = if l0 == 0 then 1 else l0
      cx = mx - (dy / l) * bend * l
      cy = my + (dx / l) * bend * l
  in [ (x, y) | k <- [0 .. 30 :: Int], let t = i2f k / 30
                                           x = (1 - t) * (1 - t) * ax + 2 * t * (1 - t) * cx + t * t * bx
                                           y = (1 - t) * (1 - t) * ay + 2 * t * (1 - t) * cy + t * t * by
     , hyp (x - ax) (y - ay) > r + 2, hyp (x - bx) (y - by) > r + 6 ]

-- 折れ線の最後に三角の矢じり
arrowHeadAt :: [(Float, Float)] -> Float -> Picture
arrowHeadAt pts sz = case reverse pts of
  ((x2, y2) : (x1, y1) : _) ->
    let l0 = hyp (x2 - x1) (y2 - y1)
        l = if l0 == 0 then 1 else l0
        ux = (x2 - x1) / l
        uy = (y2 - y1) / l
    in Polygon [(x2 + ux * sz, y2 + uy * sz), (x2 - uy * sz * 0.6, y2 + ux * sz * 0.6), (x2 + uy * sz * 0.6, y2 - ux * sz * 0.6)]
  _ -> Blank

-- 矢印のラベルやクリック判定に使う点：i → j の矢印の真ん中 (hx, hy)
data Hit = Hit { hI :: !Int, hJ :: !Int, hX :: !Float, hY :: !Float }

data GraphOpts = GraphOpts { goNames :: Maybe [String], goHl :: !Int, goNameSize :: !Float, goLabels :: !Bool, goAlpha :: Maybe Float }

defOpts :: GraphOpts
defOpts = GraphOpts Nothing (-1) 18 True Nothing

-- nodes = [(x, y), …]、a = 遷移行列、r = 円の半径、cols i = 状態 i の色。戻り値：絵と、矢印ごとの Hit
drawChainGraph :: [(Float, Float)] -> Mat -> Float -> (Int -> RGB) -> GraphOpts -> (Picture, [Hit])
drawChainGraph nodes a r cols opts = (Pictures (edgePics ++ nodePics), hits)
  where
    nn = length nodes
    cx = realToFrac (meanOf (map (realToFrac . fst) nodes)) :: Float
    cy = realToFrac (meanOf (map (realToFrac . snd) nodes)) :: Float
    edges = [ (i, j, p, c, w, pts, h)
            | i <- [0 .. nn - 1], j <- [0 .. nn - 1]
            , let p = at' (at' a i []) j 0
            , p > 1e-9
            , let c = cols i
                  w = 1 + 11 * f2 p
                  (pts, h) = edgeShape i j ]
    hits = [ h | (_, _, _, _, _, _, Just h) <- edges ]
    edgeShape i j
      | i == j = let (nx, ny) = at' nodes i (0, 0)
                     ox = nx - cx
                     oy = ny - cy
                     l0 = hyp ox oy
                     l = if l0 == 0 then 1 else l0
                     ux = if l <= 1 then 0 else ox / l
                     uy = if l > 1 then oy / l else -1
                     lx = nx + ux * r * 1.55
                     ly = ny + uy * r * 1.55
                     rr = r * 0.62
                     base = atan2 (negate uy) (negate ux)
                     pts = [ (lx + rr * cos t, ly + rr * sin t) | k <- [0 .. 32 :: Int], let t = base + 0.9 + (i2f k / 32) * (2 * pi - 1.8) ]
                 in (pts, Just (Hit i j (lx + ux * rr) (ly + uy * rr)))
      | otherwise = let pts = bezPts (at' nodes i (0, 0)) (at' nodes j (0, 0)) 0.14 r
                        (hx, hy) = at' pts (length pts `div` 2) (0, 0)
                    in (pts, if null pts then Nothing else Just (Hit i j hx hy))
    aE = maybe 210 id (goAlpha opts)
    aH = maybe 230 id (goAlpha opts)
    edgePics = [ Pictures [ Color (colA aE c) (polyline w pts), Color (colA aH c) (arrowHeadAt pts (8 + w * 0.8))
                          , case h of
                              Just hh | goLabels opts -> sayM (fmtN 2 p) (hX hh) (hY hh - 12) 14 (col cream)
                              _ -> Blank ]
               | (_, _, p, c, w, pts, h) <- edges ]
    nodePics = [ Pictures [ Color (col cream) (circleAt x y (2 * r + sw)), Color (col (scaleRGB 0.55 c)) (circleAt x y (2 * r - sw))
                          , sayM (maybe (show i) (\ns -> at' ns i (show i)) (goNames opts)) x y (goNameSize opts) (col cream) ]
               | (i, (x, y)) <- zip [0 ..] nodes, let c = cols i, let sw = if goHl opts == i then 4 else 1.5 ]

-- 行列を色つきの表で（濃い = 大きい）。左上 (x, y)、マスの大きさ cw × ch、d = 小数の桁、hi = いちばん濃い値
drawMatrix :: Mat -> Float -> Float -> Float -> Float -> Maybe [String] -> Float -> Int -> Double -> Picture
drawMatrix a x y cw ch names ts d hi = Pictures $
  [ Pictures [ Color (col (48 + t * (tr - 48), 42 + t * (tg - 42), 40 + t * (tb - 40))) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
             , sayM (fmtN d v) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col cream) ]
  | (i, row) <- zip [0 ..] a, (j, v) <- zip [0 ..] row, let t = f2 (clampv (abs v / hi) 0 1) ]
  ++ case names of
       Nothing -> []
       Just ns -> [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col sub) | (i, nm) <- zip [0 .. length a - 1] ns ]
                  ++ [ sayC nm (x + i2f j * cw + cw / 2) (y - 5) ts (col sub) | (j, nm) <- zip [0 .. nCols a - 1] ns ]
  where (tr, tg, tb) = teal

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

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

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
