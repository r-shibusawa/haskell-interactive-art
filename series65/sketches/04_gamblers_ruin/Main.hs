-- ============================================
-- 65-04 ギャンブラーの破産 ─ 所持金 i から 1 回ごとに確率 p で +1、1 − p で −1。0 になったら破産、目標 goal で終わり
-- 0 と goal は吸収状態（03 の例 2）。破産する確率と、終わるまでの平均の回数を 3 つの方法で求める
--   ① 公式（p ≠ 1/2 と p = 1/2 で形がちがう）② 基本行列 N = (I − Q)⁻¹（05 と同じ方法）③ 1 万回のシミュレーション
-- p = 0.5 から少し不利になるだけで、目標が遠いと破産がほぼ確実になる
-- 操作：← → 勝つ確率 p / ↑↓ はじめの所持金 / T 目標（10・20・50）/ スペース シミュレーションをやり直す
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

games :: Int
games = 10000                                 -- ★ シミュレーションの回数

stepBudget :: Int
stepBudget = 30000                            -- 1 フレームに進める歩数の目安（p5.js 版は 1 フレームに 6 ミリ秒ずつ）

-- 基本行列の答え：N、平均の回数 = N 1、破産の確率
data Fund = Fund { fdN :: Mat, fdSteps :: Vec, fdRuin :: Vec }

data World = World
  { wPWin    :: !Int                          -- ★ 勝つ確率（%、整数で持つ → 0.48）
  , wStart   :: !Int                          -- ★ はじめの所持金
  , wGoal    :: !Int                          -- ★ 目標
  , wRuinF   :: !Double                       -- ① 公式の破産の確率
  , wLenF    :: !Double                       -- ① 公式の平均の回数
  , wFund    :: Maybe Fund                    -- ② 基本行列
  , wSimRuin :: !Int
  , wSimLen  :: !Int
  , wSimDone :: !Int
  , wPaths   :: [[Int]]                       -- 最初の 40 ゲームの所持金の道のり
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-04 Gambler's ruin" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (simFrame w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = recompute (World 48 10 20 0 0 Nothing 0 0 0 [] (rngOf 4) [])

pOf :: World -> Double
pOf w = i2d (wPWin w) / 100

-- ★ ① 公式：r = q/p として、破産の確率 = (r^i − r^N) / (1 − r^N)、p = 1/2 なら 1 − i/N
ruinFormula :: Double -> Int -> Int -> Double
ruinFormula p i nG
  | abs (p - 0.5) < 1e-6 = 1 - i2d i / i2d nG
  | otherwise = let r = (1 - p) / p in (r ** i2d i - r ** i2d nG) / (1 - r ** i2d nG)

-- 平均の回数：p = 1/2 なら i (N − i)、そうでなければ i/(q − p) − N/(q − p) × (1 − r^i)/(1 − r^N)
durationFormula :: Double -> Int -> Int -> Double
durationFormula p i nG
  | abs (p - 0.5) < 1e-6 = i2d i * i2d (nG - i)
  | otherwise = let q = 1 - p
                    r = q / p
                in i2d i / (q - p) - (i2d nG / (q - p)) * (1 - r ** i2d i) / (1 - r ** i2d nG)

-- ★ ② 基本行列：一時的な状態 1..N−1 の間の遷移 Q、N = (I − Q)⁻¹。平均回数 = N 1、破産確率 = N × (状態 1 から 0 へ行く確率 q)
fundamental :: Double -> Int -> Maybe Fund
fundamental p nG = do
  let m = nG - 1
      iq = [ [ if j == k then 1 else if j == k + 1 then 0 - p else if j == k - 1 then 0 - (1 - p) else 0 | j <- [0 .. m - 1] ] | k <- [0 .. m - 1] ]
  fm <- matInv iq
  pure (Fund fm (map sumOf fm) (map (\r -> headOr 0 r * (1 - p)) fm))

-- 1 ゲーム：戻り値 (破産したか, 回数, 道のり)。keep なら道のり（最初の 2000 回）を残す
playGame :: Double -> Int -> Int -> Bool -> R (Bool, Int, [Int])
playGame p i nG keep = go i 0 [i]
  where
    go !x !t path
      | x > 0 && x < nG && t < 1000000 = do
          u <- rnd
          let x' = if u < p then x + 1 else x - 1
              t' = t + 1
          go x' t' (if keep && t' < 2000 then x' : path else path)
      | otherwise = pure (x == 0, t, reverse path)

recompute :: World -> World
recompute w =
  let p = pOf w
      s = max 1 (min (wGoal w - 1) (wStart w))
  in w { wStart = s, wRuinF = ruinFormula p s (wGoal w), wLenF = durationFormula p s (wGoal w), wFund = fundamental p (wGoal w)
       , wSimRuin = 0, wSimLen = 0, wSimDone = 0, wPaths = [] }

-- 1 フレームぶんのシミュレーション（歩数の目安 stepBudget まで、ゲームをまるごと進める）
simFrame :: World -> World
simFrame w0 = go w0 0
  where
    go w used
      | wSimDone w >= games || used >= stepBudget = w
      | otherwise =
          let keep = length (wPaths w) < 40
              ((ru, len, path), g) = runR (playGame (pOf w) (wStart w) (wGoal w) keep) (wRng w)
              w' = w { wSimRuin = wSimRuin w + (if ru then 1 else 0), wSimLen = wSimLen w + len, wSimDone = wSimDone w + 1
                     , wPaths = if keep then wPaths w ++ [path] else wPaths w, wRng = g }
          in go w' (used + len + 1)

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle ("Gambler's ruin: p = " ++ fmtN 2 p ++ "   start with " ++ show (wStart w) ++ "   goal " ++ show (wGoal w))
             ("<- -> p   Up/Down starting money   T goal   Space simulate again (" ++ show (wSimDone w) ++ " / " ++ show games ++ " games)")
  -- ---- 所持金の道のり（最初の 40 ゲーム）
  , plotAxes g "bets (rounds)" "money (red = ruined, green = reached the goal)" [0, fromIntegral (jsRound (grX1 g / 2)), grX1 g] [0, i2d (wStart w), i2d (wGoal w)]
  , Pictures [ Color (colA 120 c) (Line pts)          -- 細い線（1 本 = 1 つの図形で軽い）
             | path <- wPaths w
             , let lastV = lastOr 0 path
                   c = if lastV <= 0 then dice else if lastV >= wGoal w then teal else sub
                   len = length path
                   stride = max 1 (len `div` 300)
                   pts = [ gPt g (i2d k) (i2d v) | (k, v) <- zip [0 ..] path, k `mod` stride == 0 ] ++ [gPt g (i2d (len - 1)) (i2d lastV)] ]
  -- ---- 3 つの方法の表
  , say ("Answers starting from " ++ show (wStart w)) tx ty 16 (col cream)
  , Pictures [ Pictures [ say a tx yy 15 (col c), say b (tx + 170) yy 15 (col cream), say d (tx + 290) yy 15 (col cream) ]
             | (k, (a, b, d)) <- zip [0 :: Int ..] rowsT, let yy = ty + 34 + i2f k * 28, let c = at' [sub, mustard, teal, dice] k sub ]
  -- ---- 破産の確率 と はじめの所持金（p をいくつか）
  , plotAxes g2 "starting money i" "P(ruin) (formula)" [0, i2d (wGoal w) / 2, i2d (wGoal w)] [0, 0.5, 1]
  , Pictures [ Color (col c) (polyline (if pp == p then 3 else 1.5) [ gPt g2 (i2d i) (ruinFormula pp i (wGoal w)) | i <- [0 .. wGoal w] ])
             | (pp, c) <- [(0.5, sub), (0.45, plum), (p, mustard)] ]
  , if wSimDone w > 0 then Color (col dice) (circleAt (gX g2 (i2d (wStart w))) (gY g2 simR) 10) else Blank
  , legend (grX g2 + 10) (grY g2 + grH g2 + 50) [(mustard, "p = " ++ fmtN 2 p ++ " (now)"), (sub, "p = 0.50"), (plum, "p = 0.45")]
  -- ---- 少し不利なだけで
  , say "Just a little disadvantage... (start with 100, want 200)" 70 by 16 (col cream)
  , Pictures [ Pictures [ say ("p = " ++ fmtN 2 pp) 70 (by + 30 + i2f k * 26) 15 (col sub)
                        , Color (col dice) (rectTL 160 (by + 18 + i2f k * 26) (f2 r * 300) 16)
                        , say ("ruin " ++ fmtN 2 (100 * r) ++ "%") 470 (by + 30 + i2f k * 26) 15 (col cream) ]
             | (k, pp) <- zip [0 :: Int ..] [0.5, 0.49, 0.48, 0.45], let r = ruinFormula pp 100 200 ]
  , drawSelfTest env (wTest w) ]
  where
    p = pOf w
    g = Graph 70 110 (envW env * 0.5) (envH env * 0.38) 0 (i2d (max 50 (min 2000 (ceiling (wLenF w * 2.5) :: Int)))) 0 (i2d (wGoal w))
    tx = grX g + grW g + 60
    ty = 130
    idx = wStart w - 1
    simR = i2d (wSimRuin w) / i2d (max 1 (wSimDone w))
    rowsT = [ ("", "P(ruin)", "mean bets")
            , ("(1) formula", fmtN 4 (wRuinF w), fmtN 1 (wLenF w))
            , ("(2) matrix N", maybe "-" (\f -> fmtN 4 (at' (fdRuin f) idx 0)) (wFund w), maybe "-" (\f -> fmtN 1 (at' (fdSteps f) idx 0)) (wFund w))
            , ("(3) simulation", if wSimDone w > 0 then fmtN 4 simR else "...", if wSimDone w > 0 then fmtN 1 (i2d (wSimLen w) / i2d (wSimDone w)) else "...") ]
    g2 = Graph tx (ty + 190) (min 400 (envW env - tx - 50)) (envH env * 0.26) 0 (i2d (wGoal w)) 0 1
    by = grY g + grH g + 70

-- どのキーでも recompute（p5.js 版と同じく、シミュレーションはやり直し）
event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey k) Down _ _ -> recompute (special k)
  EventKey (Char c) Down _ _
    | c == 't' || c == 'T' -> let g = if wGoal w == 10 then 20 else if wGoal w == 20 then 50 else 10
                              in recompute w { wGoal = g, wStart = jsRoundI (i2d g / 2) }
    | otherwise -> recompute w
  _ -> w
  where
    special k = case k of
      KeyRight -> w { wPWin = min 70 (wPWin w + 1) }
      KeyLeft -> w { wPWin = max 30 (wPWin w - 1) }
      KeyUp -> w { wStart = min (wGoal w - 1) (wStart w + 1) }
      KeyDown -> w { wStart = max 1 (wStart w - 1) }
      _ -> w

-- ---------- やってみよう ----------
-- 1. p = 0.50 と 0.49 で、目標 50 の破産の確率をくらべよう。差はたった 0.01 なのに？
-- 2. p > 0.5（有利）にすると、破産の確率の曲線はどんな形？
-- 3. p = 0.5 のときの平均の回数 i (N − i) は、真ん中から始めると最大。なぜ？
-- 4. カジノは「目標を持たずに続ける人」に強い。goal を大きくすると破産の確率は 1 に近づく

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("破産の確率：公式 = 基本行列（p 6 通り × 目標 3 通り、ずれ " ++ fmtE worstR ++ "）", worstR < 1e-9)
  , ("平均の回数：公式 = 基本行列（相対ずれ " ++ fmtE worstT ++ "）", worstT < 1e-9)
  , ("p = 0.5 + 10⁻⁷ でも公式は p = 0.5 の値につながる（0/0 にならない）", near (ruinFormula (0.5 + 1e-7) 3 10) 0.7 1e-5 && near (durationFormula (0.5 + 1e-7) 3 10) 21 1e-3)
  , ("p = 0.5：破産 = 1 − i/N、平均の回数 = i (N − i)", near (ruinFormula 0.5 10 20) 0.5 1e-9 && near (durationFormula 0.5 10 20) 100 1e-9)
  , ("シミュレーション 2 万回（p = 0.48）：破産の確率 " ++ fmtN 3 (i2d ru / i2d nGm) ++ " ≈ 公式 " ++ fmtN 3 (ruinFormula 0.48 10 20) ++ "（2% 以内）"
    , abs (i2d ru / i2d nGm - ruinFormula 0.48 10 20) < 0.02)
  , ("シミュレーション：平均の回数 " ++ fmtN 1 (i2d len / i2d nGm) ++ " ≈ " ++ fmtN 1 (durationFormula 0.48 10 20) ++ "（3% 以内）"
    , abs (i2d len / i2d nGm / durationFormula 0.48 10 20 - 1) < 0.03)
  , ("少し不利（p = 0.49）で 100 → 200：破産の確率 > 98%", ruinFormula 0.49 100 200 > 0.98) ]
  where
    cases = [ (pp, nG, fd) | pp <- [0.3, 0.45, 0.49, 0.5, 0.51, 0.6], nG <- [5, 10, 20], Just fd <- [fundamental pp nG] ]
    worstR = foldl' maxN 0 [ abs (at' (fdRuin fd) (i - 1) 0 - ruinFormula pp i nG) | (pp, nG, fd) <- cases, i <- [1 .. nG - 1] ]
    worstT = foldl' maxN 0 [ abs (at' (fdSteps fd) (i - 1) 0 - durationFormula pp i nG) / durationFormula pp i nG | (pp, nG, fd) <- cases, i <- [1 .. nG - 1] ]
    nGm = 20000 :: Int
    (ru, len) = withSeed 41 (loopR nGm (\(!a, !b) -> do (r, t, _) <- playGame 0.48 10 20 False
                                                        pure (a + (if r then 1 else 0), b + t)) (0, 0))

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
