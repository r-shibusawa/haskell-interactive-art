-- ============================================
-- 65-07 トランプは何回切ればよい？ ─ リフルシャッフル（山を 2 つに分けて、ぱらぱらと交互に落とす）を何回すれば「よく混ざる」？
-- GSR モデル：分ける枚数は 2 項分布、落とすときは「残りの枚数に比例した確率」で左右の山から 1 枚ずつ。
-- k 回切った後に並びが σ になる確率は「上昇列の数 r」だけで決まる：C(n + 2^k − r, n) / 2^(kn)
--   上昇列 = 1, 2, 3, … と番号順に拾っていくとき、何回目でぜんぶ拾えるか（1 回 = 切る前のまま）
-- これで「完全にばらばら（どの並びも 1/n!）」との全変動距離を 52 枚でぴったり計算できる → 7 回あたりで急に小さくなる（カットオフ）
-- 操作：スペース 1 回切る / R 新しいデッキ / ↑↓ グラフで強調する枚数 n / 1 2 3 小さいデッキ（4・5・6 枚）の総当たりの表
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

deckN :: Int
deckN = 52                                    -- ★ 画面のデッキの枚数

maxK :: Int
maxK = 14                                     -- ★ グラフの最大の回数

nOpts :: [Int]
nOpts = [4, 8, 13, 20, 26, 52, 104]           -- ↑↓ で選べる枚数

data World = World
  { wDeck   :: [Int]
  , wDecks  :: [[Int]]                        -- 最近 16 回ぶんの並び（古い順）
  , wNShuf  :: !Int                           -- 切った回数
  , wNShow  :: !Int
  , wBruteN :: !Int
  , wBrute  :: [Double]                       -- 総当たりの全変動距離（1〜8 回）
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-07 How many shuffles?" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = makeBrute 5 (newDeck (World [] [] 0 52 5 [] (rngOf 7) []))

newDeck :: World -> World
newDeck w = w { wDeck = [0 .. deckN - 1], wDecks = [[0 .. deckN - 1]], wNShuf = 0 }

makeBrute :: Int -> World -> World
makeBrute n w = w { wBruteN = n, wBrute = bruteTV n 8 }

-- ★ GSR のリフルシャッフル 1 回
riffle :: [Int] -> R [Int]
riffle d = do
  c <- foldl' (\m _ -> do k <- m; u <- rnd; pure (if u < 0.5 then k + 1 else k)) (pure 0) d   -- 上の山の枚数 ~ 2 項分布
  let (left, right) = splitAt c d
      drop' ls rs acc = case (ls, rs) of
        ([], []) -> pure (reverse acc)
        _ -> do
          let l = i2d (length ls)
              r = i2d (length rs)
          u <- rnd
          if u < l / (l + r)                    -- 残りの枚数に比例して落とす
            then drop' (drop 1 ls) rs (headOr 0 ls : acc)
            else drop' ls (drop 1 rs) (headOr 0 rs : acc)
  drop' left right []

-- 上昇列の数：カード i + 1 がカード i より前にあるたびに 1 つ増える
risingSeqs :: [Int] -> Int
risingSeqs d = let wh = IM.fromList (zip d [0 ..])
                   at i = IM.findWithDefault 0 i wh
               in 1 + length [ () | i <- [0 .. length d - 2], at (i + 1) < (at i :: Int) ]

-- ★ オイラー数の割合 e(n, r) = （上昇列が r 個の並びの数）/ n!：e(n, r) = [r e(n−1, r) + (n − r + 1) e(n−1, r−1)] / n
eulerFrac :: Int -> [Double]
eulerFrac n = foldl' nextRow [1] [2 .. n]
  where nextRow e m = forceL [ ((if r <= m - 1 then i2d r * at' e (r - 1) 0 else 0) + (if r >= 2 then i2d (m - r + 1) * at' e (r - 2) 0 else 0)) / i2d m | r <- [1 .. m] ]

-- ★ k 回後の全変動距離（ぴったり）：½ Σ_r e(n, r) |n! × 確率 − 1|、n! × 確率 = Π_{j=1..n} (a − r + j) / a、a = 2^k
tvExactCalc :: Int -> Int -> Double
tvExactCalc n k =
  let e = eulerFrac n
      a = 2 ^ k :: Double
      s = foldl' (\acc (r, er) -> let prod = foldl' (\p j -> p * ((a - i2d r + i2d j) / a)) 1 [1 .. n] in acc + er * abs (prod - 1)) 0 (zip [1 .. n] e)
  in s / 2

-- 一度計算した値はとっておく（p5.js 版の tvCache）
tvCache :: M.Map (Int, Int) Double
tvCache = M.fromList [ ((n, k), tvExactCalc n k) | n <- [4, 5, 6, 8, 13, 20, 26, 52, 104], k <- [0 .. maxK] ]

tvExact :: Int -> Int -> Double
tvExact n k = M.findWithDefault (tvExactCalc n k) (n, k) tvCache

-- 総当たり（小さいデッキ）：n! 通りの並びの確率を、1 回ごとに 2^n 通りの落とし方で全部計算して更新する
permsOf :: Int -> [[Int]]
permsOf n = foldl' (\out m -> [ take k p ++ [m] ++ drop k p | p <- out, k <- [0 .. length p] ]) [[]] [0 .. n - 1]

bruteTV :: Int -> Int -> [Double]
bruteTV n kMax = go 1 dist0
  where
    ps = permsOf n
    np = length ps
    idx = M.fromList (zip ps [0 ..])
    look p = M.findWithDefault 0 p idx
    dist0 = U.generate np (\i -> if i == look [0 .. n - 1] then 1 else 0)   -- 最初は番号順
    two = 2 ^ n :: Int
    nxt = V.fromList [ [ look (out bits) | bits <- [0 .. two - 1] ] | p <- ps
                     , let out bits = let c = length [ () | i <- [0 .. n - 1], not (testBit bits i) ]
                                          (l, r) = splitAt c p
                                      in pickBits bits 0 l r ]
    pickBits bits i l r
      | i >= n = []
      | testBit bits i = headOr 0 r : pickBits bits (i + 1) l (drop 1 r)
      | otherwise = headOr 0 l : pickBits bits (i + 1) (drop 1 l) r
    -- 2^n 通りの 0/1 の列（0 = 上の山から、1 = 下の山から）がどれも確率 1/2^n = GSR モデル
    go k dist
      | k > kMax = []
      | otherwise = let nd = U.accum (+) (U.replicate np 0) [ (m, dist U.! i / i2d two) | i <- [0 .. np - 1], dist U.! i > 0, m <- nxt V.! i ]
                    in tvDist (U.toList nd) (replicate np (1 / i2d np)) : go (k + 1) nd

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("How many shuffles? " ++ show (wNShuf w) ++ " so far, " ++ show (risingSeqs (wDeck w)) ++ " rising sequences")
             ("Space shuffle once   R new deck   Up/Down graph n = " ++ show (wNShow w) ++ "   1 2 3 brute-force table (4, 5, 6 cards)")
  -- ---- デッキの帯（1 段 = 1 回切った後の並び。色 = 最初の位置）
  , say "Row = deck after each shuffle (colour = original position)" x0 (y0 - 8) 14 (col cream)
  , Pictures [ Color (hsb (i2f c / i2f deckN * 300) 70 95) (rectTL (x0 + i2f i * cw) (y0 + i2f row * rh) (cw - 1) (rh - 3))
             | (row, d) <- zip [0 :: Int ..] (wDecks w), (i, c) <- zip [0 :: Int ..] d ]
  , Pictures [ sayLM (show (base + row) ++ "x  rs " ++ show (risingSeqs d)) (x0 + i2f deckN * cw + 8) (y0 + i2f row * rh + rh / 2 - 1) 14 (col sub)
             | (row, d) <- zip [0 :: Int ..] (wDecks w) ]
  , say "(rs = rising sequences)" x0 (y0 + i2f (length (wDecks w)) * rh + 18) 14 (col sub)
  -- ---- 全変動距離のグラフ（ぴったり計算）
  , plotAxes g "riffle shuffles k" "total variation distance from uniform (exact)" (map i2d [0, 2 .. 14]) [0, 0.5, 1]
  , Pictures [ Color (col c) (polyline (if n == wNShow w then 4 else 1.5) (curve n)) | (n, c) <- curves ]
  , if wNShow w `notElem` map fst curves then Color (col cream) (polyline 3 (curve (wNShow w))) else Blank
  , legend (grX g + grW g - 110) (grY g + 20) [ (c, show n ++ " cards") | (n, c) <- curves ]
  , Color (col cream) (circleAt (gX g (i2d kNow)) (gY g (tvExact (wNShow w) kNow)) 10)
  -- 数の表
  , say (show (wNShow w) ++ " cards:") gx ty 14 (col cream)
  , Pictures [ say (show k ++ "x " ++ fmtN 3 v) (gx + 80 + i2f ((k - 1) `mod` 5) * 92) (ty + i2f ((k - 1) `div` 5) * 22) 14 (col (if v < 0.25 then teal else if v > 0.9 then dice else mustard))
             | k <- [1 .. 10], let v = tvExact (wNShow w) k ]
  -- 総当たりとの比較
  , say (show (wBruteN w) ++ " cards (" ++ show (at' [1, 1, 2, 6, 24, 120, 720 :: Int] (wBruteN w) 0) ++ " orders): brute force vs formula") gx by 14 (col cream)
  , Pictures [ say (show k ++ "x   brute " ++ fmtN 6 (at' (wBrute w) (k - 1) 0) ++ "   formula " ++ fmtN 6 (tvExact (wBruteN w) k)) gx (by + i2f k * 20) 14 (col sub) | k <- [1 .. 5] ]
  , drawSelfTest env (wTest w) ]
  where
    x0 = 40
    y0 = 100
    cw = min 10 (envW env * 0.45 / i2f deckN)
    rh = min 26 (envH env * 0.62 / 16)
    base = if wNShuf w + 1 > 16 then wNShuf w + 1 - 16 else 0
    gx = envW env * 0.55
    g = Graph gx 120 (min 520 (envW env - gx - 40)) (envH env * 0.4) 0 (i2d maxK) 0 1
    curves = [(13, plum), (26, teal), (52, dice), (104, mustard)]
    curve n = [ (gX g (i2d k), gY g (tvExact n k)) | k <- [0 .. maxK] ]
    kNow = min maxK (wNShuf w)
    ty = grY g + grH g + 60
    by = ty + 70

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let (d, g) = runR (riffle (wDeck w)) (wRng w)
    in w { wDeck = d, wDecks = lastN 16 (wDecks w ++ [d]), wNShuf = wNShuf w + 1, wRng = g }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wNShow = at' nOpts (min (length nOpts - 1) (optIdx + 1)) 52 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wNShow = at' nOpts (max 0 (optIdx - 1)) 52 }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> newDeck w
    | c >= '1' && c <= '3' -> makeBrute (3 + fromEnum c - fromEnum '0') w
  _ -> w
  where optIdx = maybe (-1) id (elemIndex (wNShow w) nOpts)
        lastN k xs = drop (length xs - k) xs

-- ---------- やってみよう ----------
-- 1. スペースを 7 回押して、虹色がどれくらい崩れるか見よう。3 回だとまだ色のかたまりが見える
-- 2. 上昇列の数は 1 回切るごとに最大 2 倍。52 枚がばらばら（上昇列 ≈ 26）になるには何回？
-- 3. 枚数を 2 倍（104 枚）にすると、必要な回数は 1 回くらいしか増えない（約 1.5 log₂ n）
-- 4. 本物のリフルは GSR モデルより「交互に落ちやすい」。riffle の l / (l + r) を変えると？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("n = 2〜6 枚、1〜6 回：総当たりの全変動距離 = 上昇列の式（ずれ " ++ fmtE worst ++ "）", worst < 1e-12)
  , ("52 枚：全部の並びの確率の合計 = 1（k = 0〜12）", okSum && near (sumOf e52) 1 1e-12)
  , ("52 枚：全変動距離は回数とともに減る", and [ tvExact 52 k <= tvExact 52 (k - 1) + 1e-15 | k <- [1 .. 14] ])
  , ("52 枚：7 回 = " ++ fmtN 3 (tvExact 52 7) ++ "（ベイヤー-ダイアコニスの表 0.334）、6 回 0.614、8 回 0.167", near (tvExact 52 7) 0.334 6e-4 && near (tvExact 52 6) 0.614 6e-4 && near (tvExact 52 8) 0.167 6e-4)
  , ("52 枚：4 回まではほぼ 1（> 0.99）、10 回で < 0.05", tvExact 52 4 > 0.99 && tvExact 52 10 < 0.05)
  , ("4 枚を 1 回（シミュレーション 4 万回）：上昇列 1 個 ≈ 5/16、2 個 ≈ 11/16", near (frac 1) (5 / 16) 0.01 && near (frac 2) (11 / 16) 0.01 && IM.findWithDefault 0 3 cnt == 0) ]
  where
    worst = foldl' maxN 0 [ abs (b - tvExact n k) | n <- [2 .. 6], (k, b) <- zip [1 .. 6] (bruteTV n 6) ]
    e52 = eulerFrac 52
    okSum = and [ abs (s - 1) <= 1e-9 | k <- [0 .. 12 :: Int], let a = 2 ^ k :: Double
                , let s = foldl' (\acc (r, er) -> acc + er * foldl' (\p j -> p * ((a - i2d r + i2d j) / a)) 1 [1 .. 52 :: Int]) 0 (zip [1 :: Int ..] e52) ]
    runs = 40000 :: Int
    cnt = withSeed 77 (go runs IM.empty)
    go :: Int -> IM.IntMap Int -> R (IM.IntMap Int)
    go 0 m = pure m
    go k m = do
      d <- riffle [0, 1, 2, 3]
      go (k - 1) $! IM.insertWith (+) (risingSeqs d) 1 m
    frac r = i2d (IM.findWithDefault 0 r cnt) / i2d runs

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
