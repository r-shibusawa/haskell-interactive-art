-- ============================================
-- 65-00 天気のマルコフ連鎖 ─ 「明日の天気は、今日の天気だけで決まる」と考えるモデル
-- 晴れ・くもり・雨の 3 つの状態と、矢印の確率（遷移確率）。同じ状態から出る矢印の確率の合計はいつも 1。
-- 1 日ごとにサイコロ（0〜1 の乱数 u）を振り、今日の行の区切りのどこに u が落ちたかで明日が決まる。
-- 長く続けると、晴れ・くもり・雨の日の割合は、決まった値（定常分布 π、02 で詳しく）に近づく
-- 操作：矢印のラベルをドラッグ（上下）で確率を変える / スペース 1 日すすめる / A 自動 / R 最初から / 1 2 3 今日の天気
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

stateNames :: [String]
stateNames = ["Sunny", "Cloudy", "Rain"]       -- 晴れ・くもり・雨

transP0 :: Mat                                -- ★ 遷移行列：transP !! 今日 !! 明日（各行の合計 = 1）
transP0 =
  [ [0.6, 0.3, 0.1]
  , [0.3, 0.4, 0.3]
  , [0.2, 0.4, 0.4] ]

showDays :: Int
showDays = 42                                 -- ★ カレンダーに並べる日数（6 週間）

data World = World
  { wP       :: Mat                           -- いまの遷移行列
  , wToday   :: !Int
  , wDays    :: [Int]                         -- これまでの天気（新しい順。最大 4000 日）
  , wNDays   :: !Int                          -- wDays の長さ
  , wCounts  :: [Int]                         -- 晴れ・くもり・雨の日の数
  , wLastU   :: !Double                       -- 最後に振ったサイコロ（まだなら −1）
  , wAuto    :: !Bool
  , wDrag    :: Maybe Hit                     -- ドラッグ中の矢印
  , wDragY   :: !Float
  , wPi      :: Vec                           -- 定常分布（連立方程式で）
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-00 Weather Markov chain" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 15 == 0 then advanceDay w else w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart (World transP0 0 [] 0 [] (-1) True Nothing 0 [] (rngOf 2024) [])

restart :: World -> World
restart w = w { wDays = [wToday w], wNDays = 1, wCounts = setAt (wToday w) 1 [0, 0, 0], wLastU = -1
              , wPi = maybe [1 / 3, 1 / 3, 1 / 3] id (stationaryExact (wP w)) }

-- ★ サイコロで明日を決める：u が [0, p0) なら 0、[p0, p0+p1) なら 1、残りは 2
nextWeather :: [Double] -> Double -> Int
nextWeather row u = go 0 0 row
  where go j _ [] = max 0 (j - 1)
        go j c (p : ps) = let c' = c + p in if u < c' then j else go (j + 1) c' ps

advanceDay :: World -> World
advanceDay w =
  let (u, g) = runR rnd (wRng w)
      t = nextWeather (at' (wP w) (wToday w) []) u
      n = wNDays w + 1
  in w { wLastU = u, wToday = t, wDays = take 4000 (t : wDays w), wNDays = min 4000 n
       , wCounts = modifyAt t (+ 1) (wCounts w), wRng = g }

-- 1 つの確率を v にして、同じ行の残りを「元の比のまま」縮める（行の合計 = 1 を保つ）
setProb :: Mat -> Int -> Int -> Double -> Mat
setProb a i j v0 = modifyAt i fixRow a
  where
    v = clampv v0 0 1
    fixRow r = let n = length r
                   rest = sumOf [ x | (k, x) <- zip [0 ..] r, k /= j ]
               in [ if k == j then v else if rest > 1e-12 then (x / rest) * (1 - v) else (1 - v) / i2d (n - 1) | (k, x) <- zip [0 ..] r ]

-- ---- 画面の配置（draw とクリックで同じものを使う）
nodesOf :: Env -> [(Float, Float)]
nodesOf env = [ (gcx + rad * cos t, gcy + rad * sin t) | k <- [0 .. 2 :: Int], let t = -pi / 2 + i2f k * 2 * pi / 3 ]
  where gcx = envW env * 0.22
        gcy = envH env * 0.4
        rad = min (envW env) (envH env) * 0.16

graphOf :: Env -> World -> (Picture, [Hit])
graphOf env w = drawChainGraph (nodesOf env) (wP w) 34 stateCol defOpts { goNames = Just stateNames, goHl = wToday w, goNameSize = 15 }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Weather Markov chain: day " ++ show (wNDays w) ++ "   today is " ++ nm (wToday w))
             ("Drag labels up/down: change probability   Space 1 day   A auto (" ++ (if wAuto w then "ON" else "OFF") ++ ")   R restart   1 2 3 today")
  -- ---- 遷移図（矢印の太さ = 確率）
  , fst (graphOf env w)
  , case wDrag w of
      Just h -> Color (col mustard) (ringAt (hX h) (hY h - 12) 34 2)
      Nothing -> Blank
  -- ---- 遷移行列とサイコロ
  , say "Transition matrix P (row = today, column = tomorrow)" mx (my - 30) 15 (col cream)
  , drawMatrix (wP w) (mx + 80) my 70 30 (Just stateNames) 14 2 1
  , say ("Dice: today's row (" ++ nm yest ++ ") on 0..1, u picks tomorrow") mx dy 15 (col cream)
  , Pictures [ Pictures [ Color (col (stateCol j)) (rectTL (mx + f2 c * bw) (dy + 12) (f2 p * bw) 26)
                        , if p > 0.08 then sayM (nm j) (mx + f2 (c + p / 2) * bw) (dy + 25) 14 (grayC 20) else Blank ]
             | (j, p, c) <- zip3 [0 ..] row (scanl (+) 0 row) ]
  , if wLastU w >= 0
      then Pictures [ Color (col cream) (lineW 3 (mx + f2 (wLastU w) * bw, dy + 6) (mx + f2 (wLastU w) * bw, dy + 44))
                    , sayT ("u = " ++ fmtN 3 (wLastU w)) (mx + f2 (wLastU w) * bw) (dy + 46) 14 (col cream) True ]
      else Blank
  -- ---- 割合：シミュレーション と 定常分布
  , say ("Share of days (" ++ show tot ++ " days)   bar = simulation   line = stationary pi") mx ty 15 (col cream)
  , Pictures [ Pictures [ Color (col (stateCol k)) (rectTL (mx + 70) y (f2 f * bw) 20)
                        , Color (col cream) (lineW 2 (mx + 70 + f2 pk * bw, y - 3) (mx + 70 + f2 pk * bw, y + 23))
                        , sayLM (nm k) mx (y + 10) 14 (col cream)
                        , sayLM (fmtN 3 f ++ " / pi = " ++ fmtN 3 pk) (mx + 80 + bw) (y + 10) 14 (col cream) ]
             | k <- [0 .. 2], let y = ty + 16 + i2f k * 30, let f = i2d (at' (wCounts w) k 0) / i2d (max 1 tot), let pk = at' (wPi w) k 0 ]
  -- ---- カレンダー（最近の 6 週間）
  , say ("Calendar (left to right, last " ++ show showDays ++ " days)") cx0 (cy0 - 10) 14 (col sub)
  , Pictures [ Pictures [ Color (col (48, 43, 40)) (rectTL (x + 2) (y + 2) (cs - 4) (cs - 4))
                        , weatherIcon d (x + cs / 2) (y + cs / 2 + 2) (cs * 0.5)
                        , sayT (show (k + 1)) (x + 5) (y + 4) 14 (col sub) False ]
             | (p, (k, d)) <- zip [0 :: Int ..] (zip [start ..] recent)
             , let x = cx0 + i2f (p `mod` 14) * cs
             , let y = cy0 + i2f (p `div` 14) * cs ]
  , drawSelfTest env (wTest w) ]
  where
    nm k = at' stateNames k "?"
    mx = envW env * 0.47
    my = 124
    dy = my + 3 * 30 + 40
    bw = min 380 (envW env * 0.3)
    yest = if wNDays w > 1 then at' (wDays w) 1 (wToday w) else wToday w
    row = at' (wP w) yest []
    ty = dy + 90
    tot = sum (wCounts w)
    cs = min 64 ((envW env - 80) / 14)
    cx0 = 40
    cy0 = envH env - cs * 3 - 40
    start = max 0 (wNDays w - showDays)
    recent = reverse (take (wNDays w - start) (wDays w))

-- 晴れ = 太陽、くもり = 雲、雨 = 雲としずく
weatherIcon :: Int -> Float -> Float -> Float -> Picture
weatherIcon wt x y s
  | wt == 0 = Color (col dice) (Pictures (circleAt x y (s * 0.6) : [ lineW 2 (x + cos t * s * 0.42, y + sin t * s * 0.42) (x + cos t * s * 0.6, y + sin t * s * 0.6) | k <- [0 .. 7 :: Int], let t = i2f k * pi / 4 ]))
  | otherwise = Pictures $
      [ Color (col (if wt == 1 then (200, 196, 186) else (150, 160, 175))) (Pictures [circleAt (x - s * 0.22) y (s * 0.45), circleAt (x + s * 0.18) (y - s * 0.05) (s * 0.55), rectTL (x - s * 0.44) y (s * 0.86) (s * 0.22)]) ]
      ++ [ Color (col teal) (Pictures [ lineW 2 (x + k * s * 0.25, y + s * 0.3) (x + k * s * 0.25 - 3, y + s * 0.48) | k <- [-1, 0, 1] ]) | wt == 2 ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        cands = [ (d, h) | h <- snd (graphOf env w), let d = hyp (mx - hX h) (my - (hY h - 12)), d < 26 ]
        best = foldl' (\b c -> case b of
                                 Just (bd, _) | bd <= fst c -> b
                                 _ -> Just c) Nothing cands
    in w { wDrag = fmap snd best, wDragY = my }
  EventMotion pos | Just h <- wDrag w ->
    let (_, my) = toP5 env pos
        p = setProb (wP w) (hI h) (hJ h) (round2 (at' (at' (wP w) (hI h) []) (hJ h) 0 + realToFrac (wDragY w - my) / 250))
    in w { wP = p, wDragY = my, wPi = maybe (wPi w) id (stationaryExact p) }
  EventKey (MouseButton LeftButton) Up _ _ -> case wDrag w of
    Just _ -> let p = fixRows (wP w) in w { wP = p, wPi = maybe (wPi w) id (stationaryExact p), wDrag = Nothing }
    Nothing -> w
  EventKey (SpecialKey KeySpace) Down _ _ -> advanceDay w
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'r' || c == 'R' -> restart w
    | c >= '1' && c <= '3' -> restart w { wToday = fromEnum c - fromEnum '1' }
  _ -> w

-- ---------- やってみよう ----------
-- 1. transP0 の 雨 → 雨 を 0.9 にすると、カレンダーに「梅雨」のような長い雨の続きが現れる。割合はどう変わる？
-- 2. 晴れ → 晴れ = 1 にすると、一度晴れたらずっと晴れ（吸収状態、04・05 で詳しく）
-- 3. 1 2 3 でちがう天気から始めても、長く続けると割合は同じ π に近づく？
-- 4. skStep の `mod` 15 を `mod` 1 にすると 1 秒に 60 日。割合が π に近づく速さを見よう

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("遷移行列の各行の合計 = 1", rowSumErr pt 1 < 1e-12)
  , ("定常分布：連立方程式 = べき乗法（" ++ commaList (map (fmtN 4) pe) ++ "）", maxAbsDiffV pe pw < 1e-10)
  , ("π P = π、Σ π = 1", maxAbsDiffV (vecMat pe pt) pe < 1e-12 && near (sumOf pe) 1 1e-12)
  -- 手計算：π = (24, 22, 15) / 61 … (π0 = 0.6π0 + 0.3π1 + 0.2π2 などを解く)
  , ("定常分布 = (24, 22, 15) / 61（手計算）", maxAbsDiffV pe [24 / 61, 22 / 61, 15 / 61] < 1e-12)
  , ("20 万日のシミュレーションの割合 ≈ π（ずれ < 0.01）", maxAbsDiffV (map (\c -> i2d c / i2d nD) cnt) pe < 0.01)
  , ("確率を変えても行の合計 = 1（0.6 : 0.3 の比 → 0.333 : 0.167）", rowSumErr pc 1 < 1e-12 && near (pc !! 0 !! 0) (1 / 3) 1e-12 && near (pc !! 0 !! 1) (1 / 6) 1e-12)
  , ("残りが 0 の行でも NaN にならず合計 = 1", noNaN (concat pc2) && rowSumErr pc2 1 < 1e-12) ]
  where
    pt = transP0
    pe = maybe [] id (stationaryExact pt)
    pw = fst (stationaryPower pt 1e-14 100000 Nothing)
    nD = 200000 :: Int
    cnt = withSeed 7 (go nD 0 (0, 0, 0))
    go :: Int -> Int -> (Int, Int, Int) -> R [Int]
    go 0 _ (a, b, c) = pure [a, b, c]
    go k s (!a, !b, !c) = do
      u <- rnd
      let s' = nextWeather (pt !! s) u
      go (k - 1) s' (if s' == 0 then (a + 1, b, c) else if s' == 1 then (a, b + 1, c) else (a, b, c + 1))
    pc = setProb pt 0 2 0.5
    pc2 = setProb (setProb pc 1 1 1) 1 0 0.4

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
