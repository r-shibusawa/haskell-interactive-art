-- ============================================
-- 65-09 隠れマルコフモデル ─ いかさまカジノ：ふつうのサイコロ（どの目も 1/6）と、6 が出やすいいかさまサイコロを、
-- 店がこっそり切りかえている。どちらを使っているか（隠れた状態）はマルコフ連鎖で動き、見えるのは出た目だけ。
-- 前向きアルゴリズム α と後ろ向きアルゴリズム β を掛けると、各時刻の「いかさまだった確率」P(いかさま | 全部の目) が出る。
-- 掛け算をくり返すと数が小さくなりすぎるので、毎回合計で割る（スケーリング）。最もありそうな 1 本の道はビタビ（シリーズ 59 の 13）
-- 操作：クリック 目を変える / R 新しい列 / スペース 前向きだけ ⇔ 前向き後ろ向き / ↑↓ いかさまサイコロの 6 の確率
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nT :: Int
nT = 150                                      -- ★ サイコロを振る回数

toLoaded, toFair :: Double
toLoaded = 0.05                               -- ★ 切りかえの確率（ふつう → いかさま）
toFair = 0.1                                  -- ★ （いかさま → ふつう）

-- 状態 0 = ふつう、1 = いかさま。emit !! 状態 !! (目 − 1)
data Hmm = Hmm { hInit :: Vec, hTrans :: Mat, hEmit :: Mat }

-- 前向き後ろ向きの結果：post !! t = P(状態 | 全部の目)、filt !! t = P(状態 | 1〜t 回目の目)、logLik = log P(全部の目)
data FB = FB { fbPost :: [Vec], fbFilt :: [Vec], fbLogLik :: !Double }

data World = World
  { wSix    :: !Double                        -- ★ いかさまサイコロで 6 が出る確率（ほかの目は等しく残りを分ける）
  , wHmm    :: Hmm
  , wRolls  :: [Int]
  , wTruth  :: [Int]
  , wFB     :: FB
  , wSmooth :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-09 Hidden Markov model" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = newSequence (World 0.5 (makeHmm toLoaded toFair 0.5) [] [] (FB [] [] 0) True (rngOf 9) [])

makeHmm :: Double -> Double -> Double -> Hmm
makeHmm a b p6 = let q = (1 - p6) / 5
                 in Hmm [b / (a + b), a / (a + b)] [[1 - a, a], [b, 1 - b]] [map (/ 6) [1, 1, 1, 1, 1, 1], [q, q, q, q, q, p6]]

-- 隠れた状態と目を 1 つずつ作る。戻り値 (状態の列, 目の列)
sampleHmm :: Hmm -> Int -> R ([Int], [Int])
sampleHmm h n = do
  z0 <- sampleFrom (hInit h)
  let go t z zs os
        | t >= n = pure (reverse zs, reverse os)
        | otherwise = do
            z' <- if t > 0 then sampleFrom (at' (hTrans h) z []) else pure z
            o <- sampleFrom (at' (hEmit h) z' [])
            go (t + 1) z' (z' : zs) ((o + 1) : os)
  go 0 z0 [] []

emitP :: Hmm -> Int -> Int -> Double
emitP h j o = at' (at' (hEmit h) j []) (o - 1) 0

trP :: Hmm -> Int -> Int -> Double
trP h i j = at' (at' (hTrans h) i []) j 0

-- ★ 前向き・後ろ向き（スケーリングつき）
forwardBackward :: Hmm -> [Int] -> FB
forwardBackward h os = FB post als logL
  where
    k = 2
    -- 前向き：α_t(j) = Σ_i α_(t−1)(i) P_ij × 出る確率、毎回合計 c_t で割る
    (alsR, scsR, logL) = foldl' fwd ([], [], 0) (zip [0 :: Int ..] os)
    fwd (as, cs, ll) (t, o) =
      let prev = headOr [] as
          a = forceL [ s * emitP h j o | j <- [0 .. k - 1]
                     , let s = if t == 0 then at' (hInit h) j 0 else foldl' (\acc i -> acc + at' prev i 0 * trP h i j) 0 [0 .. k - 1] ]
          c = sumOf a
      in (map (/ c) a : as, c : cs, ll + log c)
    als = reverse alsR
    scs = reverse scsR
    -- 後ろ向き：β_t(i) = Σ_j P_ij × 出る確率(t+1) × β_(t+1)(j) / c_(t+1)
    bes = foldr bwd [replicate k 1] (zip (drop 1 os) (drop 1 scs))
    bwd (o, c) acc = let nb = headOr [] acc
                     in map (/ c) [ foldl' (\s j -> s + trP h i j * emitP h j o * at' nb j 0) 0 [0 .. k - 1] | i <- [0 .. k - 1] ] : acc
    post = [ let g = zipWith (*) a b; s = sumOf g in map (/ s) g | (a, b) <- zip als bes ]

recompute :: World -> World
recompute w = w { wFB = forwardBackward (wHmm w) (wRolls w) }

newSequence :: World -> World
newSequence w = let ((zs, os), g) = runR (sampleHmm (wHmm w) nT) (wRng w)
                in recompute w { wRolls = os, wTruth = zs, wRng = g }

accuracy :: [Vec] -> [Int] -> Double
accuracy ps zs = i2d (length [ () | (p, z) <- zip ps zs, (if at' p 1 0 > 0.5 then 1 else 0) == z ]) / i2d (max 1 (length zs))

-- ---- 画面の配置（draw とクリックで同じものを使う）
perRow :: Int
perRow = 50

cellLayout :: Env -> (Float, Float, Float, Float)   -- (cw, x0, y0, rowH)
cellLayout env = let nRows = (nT + perRow - 1) `div` perRow
                     y0 = 150
                 in (min 22 ((envW env - 80) / i2f perRow), 40, y0, min 150 ((envH env - y0 - 110) / i2f nRows))

cellXY :: Env -> Int -> (Float, Float)
cellXY env t = let (cw, x0, y0, rowH) = cellLayout env in (x0 + i2f (t `mod` perRow) * cw, y0 + i2f (t `div` perRow) * rowH)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Hidden Markov model: dishonest casino   " ++ (if wSmooth w then "forward-backward P(loaded | all rolls)" else "forward only P(loaded | rolls so far)"))
             ("Click: change a roll   R new sequence   Space forward only <-> forward-backward   Up/Down P(6) of loaded die = " ++ fmtN 2 (wSix w))
  , say "Square = roll (6 in red)   strip = die really used (red = loaded)   bar = computed P(loaded)" x0 (y0 - 40) 14 (col sub)
  , Pictures [ Pictures [ Color (if r == 6 then col dice else col (70, 63, 58)) (rectTL (x + 1) y (cw - 2) (cw - 2))
                        , sayM (show r) (x + cw / 2) (y + cw / 2 - 1) 14 (col cream)
                        , Color (col (if z == 1 then dice else teal)) (rectTL x (y + cw + 2) cw 6)
                        , Color (col (60, 54, 50)) (rectTL (x + 1) (y + cw + 12) (cw - 2) bh)
                        , Color (col mustard) (rectTL (x + 1) (y + cw + 12 + bh * (1 - p)) (cw - 2) (bh * p))
                        , if t `mod` perRow == 0 then sayRM (show (t + 1)) (x - 4) (y + cw / 2) 14 (col sub) else Blank ]
             | (t, r, z, pv) <- zip4' [0 ..] (wRolls w) (wTruth w) shownP
             , let (x, y) = cellXY env t, let p = f2 (at' pv 1 0) ]
  -- 0.5 の線
  , Color (colA 90 cream) (Pictures [ lineW 1 (x0, y) (x0 + i2f perRow * cw, y) | r <- [0 .. nRows - 1], let y = y0 + i2f r * rowH + cw + 12 + (rowH - cw - 40) / 2 ])
  -- ---- 数字
  , say ("Accuracy (P > 0.5 = loaded): forward-backward " ++ show (jsRoundI (accuracy (fbPost fb) (wTruth w) * 100)) ++ "%  /  forward only "
         ++ show (jsRoundI (accuracy (fbFilt fb) (wTruth w) * 100)) ++ "%") x0 ty 15 (col cream)
  , say ("log P(all rolls) = " ++ fmtN 2 (fbLogLik fb) ++ "   (fair die only: " ++ fmtN 2 (i2d nT * log (1 / 6)) ++ ")") x0 (ty + 24) 15 (col cream)
  , say ("Switch: fair -> loaded " ++ showD toLoaded ++ ", loaded -> fair " ++ showD toFair ++ ". The backward pass also uses later rolls, so switches line up better.") x0 (ty + 48) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    (cw, x0, y0, rowH) = cellLayout env
    nRows = (nT + perRow - 1) `div` perRow
    bh = rowH - cw - 40
    fb = wFB w
    shownP = if wSmooth w then fbPost fb else fbFilt fb
    ty = y0 + i2f nRows * rowH + 10

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        (cw, _, _, _) = cellLayout env
        hit = headOr (-1) [ t | t <- [0 .. length (wRolls w) - 1], let (x, y) = cellXY env t, mx >= x, mx < x + cw, my >= y, my < y + cw ]
    in if hit < 0 then w else recompute w { wRolls = modifyAt hit (\r -> r `mod` 6 + 1) (wRolls w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSmooth = not (wSmooth w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> setSix (min 0.95 (round2 (wSix w + 0.05)))
  EventKey (SpecialKey KeyDown) Down _ _ -> setSix (max 0.2 (round2 (wSix w - 0.05)))
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> newSequence w
  _ -> w
  where setSix s = recompute w { wSix = s, wHmm = makeHmm toLoaded toFair s }

-- ---------- やってみよう ----------
-- 1. 6 の確率を 0.2（ほぼふつう）にすると、計算した確率の棒はどうなる？ 見分けにくいのはなぜ？
-- 2. 6 が続くところをクリックで別の目に変えると、まわりの確率はどう変わる？（後ろ向きの効果）
-- 3. スペースで「前向きだけ」にすると、切りかえに気づくのが少し遅れるのはなぜ？
-- 4. toLoaded を 0.3 にして、切りかえが多い店にすると、当たりの割合は？

-- ---- 自己テスト
-- 2^n 通りの隠れた道を全部足す（短い列だけ）。戻り値 (尤度, 各時刻の周辺確率)
bruteForce :: Hmm -> [Int] -> (Double, [Vec])
bruteForce h os =
  let n = length os
      (tot, marg) = foldl' (\(tt, mg) m ->
                       let p = foldl' (\acc (t, o) -> let z = bitOf m t in acc * ((if t == 0 then at' (hInit h) z 0 else trP h (bitOf m (t - 1)) z) * emitP h z o)) 1 (zip [0 ..] os)
                       in (tt + p, forceM [ if bitOf m t == 0 then [a + p, b] else [a, b + p] | (t, [a, b]) <- zip [0 ..] mg ]))
                     (0, replicate n [0, 0]) [0 .. (2 :: Int) ^ n - 1]
  in (tot, [ map (/ tot) r | r <- marg ])
  where bitOf m t = if testBit m t then 1 else 0
        forceM xs = foldl' (\u r -> sumOf r `seq` u) () xs `seq` xs

selfTest :: [(String, Bool)]
selfTest =
  [ ("遷移と出る目の確率の行の合計 = 1", rowSumErr (hTrans h) 1 < 1e-12 && rowSumErr (hEmit h) 1 < 1e-12 && near (sumOf (hInit h)) 1 1e-12)
  , ("はじめの分布 = 切りかえの連鎖の定常分布（2/3, 1/3）", maybe False (\pe -> maxAbsDiffV pe (hInit h) < 1e-12) (stationaryExact (hTrans h)) && near (at' (hInit h) 1 0) (1 / 3) 1e-12)
  , ("尤度：前向き（スケーリング）= 4096 通りの総当たり（12 回）", near (fbLogLik fb) (log bLik) 1e-10)
  , ("各時刻の P(いかさま | 目) = 総当たりの周辺確率", maxAbsDiff (fbPost fb) bPost < 1e-12)
  , ("どの時刻でも P(ふつう) + P(いかさま) = 1", all (\p -> near (sumOf p) 1 1e-12) (fbPost fb))
  , ("最後の時刻では 前向きだけ = 前向き後ろ向き", maxAbsDiffV (lastOr [] (fbFilt fb)) (lastOr [] (fbPost fb)) < 1e-12)
  , ("3000 回：当たりの割合 " ++ show (jsRoundI (accS * 100)) ++ "%（> 80%）、前向きだけ以上", accS > 0.8 && accS >= accF - 0.005 && noNaN [fbLogLik big])
  , ("6 ばかりなら いかさま > 0.9、6 がなければ < 0.1", at' (at' (fbPost all6) 10 []) 1 0 > 0.9 && at' (at' (fbPost no6) 10 []) 1 0 < 0.1) ]
  where
    h = makeHmm 0.05 0.1 0.5
    os = [6, 6, 2, 6, 5, 6, 6, 1, 3, 4, 6, 2]
    fb = forwardBackward h os
    (bLik, bPost) = bruteForce h os
    (zs, bigOs) = withSeed 90 (sampleHmm h 3000)
    big = forwardBackward h bigOs
    accS = accuracy (fbPost big) zs
    accF = accuracy (fbFilt big) zs
    all6 = forwardBackward h (replicate 20 6)
    no6 = forwardBackward h (concat (replicate 4 [1, 2, 3, 4, 5]))

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
