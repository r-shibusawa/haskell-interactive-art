-- ============================================
-- 64-01 コンドルセの陪審定理 ─ 正解率 p の人が n 人、それぞれ独立に投票して多数決。多数決の正解率は？
-- 正解する人の数は 2 項分布 → 多数決の正解率 = Σ_{k > n/2} C(n, k) p^k (1 − p)^(n − k) を正確に計算（左のグラフ）
--   p > 0.5 なら n を増やすほど 1 に近づき、p < 0.5 なら 0 に近づく
--   右：「まねっこ」を入れる。各人は確率 c でリーダーの意見をまね、残りは自分で考える（1 人の正解率は p のまま、2 人の相関 = c²）
--   相関が強いと、人数を増やしても多数決の正解率は p 付近から上がらない（点 = シミュレーション、線 = 正確な計算）
-- 操作：スライダー（ドラッグ）p・まねる確率 c・人数 n / スペース シミュレーションをやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)

nMax :: Int
nMax = 101                     -- ★ グラフの人数の最大（奇数）

simNs :: [Int]
simNs = [1, 3, 5, 9, 15, 25, 41, 61, 81, 101]   -- シミュレーションする人数

simBudget :: Double
simBudget = 6                  -- ★ 1 フレームにシミュレーションするミリ秒

faintPs :: [Double]
faintPs = [0.4, 0.45, 0.55, 0.7, 0.8]

-- 1 回の投票：各人が正しいか（0/1）と、リーダーが正しいか
data Vote = Vote { vtVotes :: ![Int], vtLead :: !Int }

-- グラフの線（スライダーが変わったときだけ計算し直す）
data Curves = Curves { cvInd :: ![(Double, Double)], cvCp :: ![(Double, Double)], cvFaint :: ![[(Double, Double)]] }

data World = World
  { wSl    :: ![Slider]          -- p・c・n
  , wDrag  :: !(Maybe Int)
  , wSimN  :: !(U.Vector Int)    -- シミュレーションの回数
  , wSimOk :: !(U.Vector Int)    -- 多数決が正しかった回数
  , wLast  :: !(Maybe Vote)      -- 30 フレームごとの 1 回の投票
  , wCv    :: !Curves
  , wRng   :: !Rng               -- 乱数の列（自己テストの続きから）
  , wTest  :: [(String, Bool)] }

sliders0 :: [Slider]
sliders0 =
  [ Slider 70 0 240 0.3 0.9 0.6 "one person's accuracy p" 3 False   -- ★
  , Slider 70 0 240 0 1 0.4 "copy probability c" 2 False            -- ★ 0 = みんな独立
  , Slider 70 0 240 1 101 25 "people n (odd)" 0 True ]

main :: IO ()
main = runSketch (sketch "64-01 Condorcet jury theorem" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (length (cvCp (wCv world0)))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = recurve (World sliders0 Nothing z z Nothing (Curves [] [] []) testRng [])
  where z = U.replicate (length simNs) 0

pOf, cOf :: World -> Double
pOf w = slV (headOr (Slider 0 0 0 0 1 0.6 "" 0 False) (wSl w))
cOf w = slV (headOr (Slider 0 0 0 0 1 0.4 "" 0 False) (drop 1 (wSl w)))

nOf :: World -> Int                     -- 人数は奇数にする（n | 1）
nOf w = let k = round (slV (headOr (Slider 0 0 0 0 1 25 "" 0 True) (drop 2 (wSl w)))) :: Int in if odd k then k else k + 1

-- ★ 独立な n 人の多数決の正解率（2 項分布の和。n が偶数なら同数はコイン投げで半分）
majorityExact :: Int -> Double -> Double
majorityExact n p
  | n <= 0 = 0.5
  | otherwise = clampv (go 0 0 0) 0 1
  where
    logp = log (max 1e-300 p)
    logq = log (max 1e-300 (1 - p))
    go k lc0 s
      | k > n = s
      | otherwise =
          let lc = if k > 0 then lc0 + (log (fromIntegral (n - k + 1)) - log (fromIntegral k)) else lc0   -- lc = log C(n, k)
              pk = exp (lc + fromIntegral k * logp + fromIntegral (n - k) * logq)
              s' = if 2 * k > n then s + pk else if 2 * k == n then s + pk / 2 else s
          in go (k + 1) lc s'

-- 2 項分布の確率 P(k), k = 0..n を全部（対数で計算してあふれないように）
binomRow :: Int -> Double -> [Double]
binomRow n p = go 0 0
  where
    lp = log (max 1e-300 p)
    lq = log (max 1e-300 (1 - p))
    go k lc0
      | k > n = []
      | otherwise =
          let lc = if k > 0 then lc0 + (log (fromIntegral (n - k + 1)) - log (fromIntegral k)) else lc0
              v | p <= 0 = if k == 0 then 1 else 0
                | p >= 1 = if k == n then 1 else 0
                | otherwise = exp (lc + fromIntegral k * lp + fromIntegral (n - k) * lq)
          in v : go (k + 1) lc

-- ★ まねっこモデルの正確な値：まねる人の数 m ~ 2 項(n, c)、残り n − m 人は独立。リーダーが正しいか（確率 p）で場合分け
majorityCopy :: Int -> Double -> Double -> Double
majorityCopy n p c = clampv (foldl' one 0 (zip [0 ..] (binomRow n c))) 0 1
  where
    need = n `div` 2 + 1                -- 過半数
    one s (m, pm)
      | pm < 1e-15 = s
      | otherwise =
          let row = zip [0 ..] (binomRow (n - m) p)
              okL = foldl' (\a (k, r) -> if k + m >= need then a + r else a) 0 row
              okW = foldl' (\a (k, r) -> if k >= need then a + r else a) 0 row
          in s + pm * (p * okL + (1 - p) * okW)

recurve :: World -> World
recurve w = w { wCv = Curves [ (fromIntegral n, majorityExact n p) | n <- [1, 3 .. nMax] ]
                             [ (fromIntegral n, majorityCopy n p c) | n <- [1, 5 .. nMax] ]
                             [ [ (fromIntegral n, majorityExact n pp) | n <- [1, 3 .. nMax] ] | pp <- faintPs ] }
  where p = pOf w
        c = cOf w

-- 1 回の投票（シミュレーション）
voteOnce :: Int -> Double -> Double -> R Vote
voteOnce n p c = do
  a <- rnd
  let lead = if a < p then 1 else 0
  vs <- mapM (const (do u <- rnd
                        if u < c then pure lead else do b <- rnd; pure (if b < p then 1 else 0))) [1 .. n]
  pure (Vote vs lead)

resetSim :: World -> World
resetSim w = w { wSimN = U.map (const 0) (wSimN w), wSimOk = U.map (const 0) (wSimOk w) }

-- 全部の人数で 1 回ずつ投票する
simOnce :: Double -> Double -> World -> World
simOnce p c w =
  let (oks, g) = evalR (wRng w) (mapM (\k -> do v <- voteOnce k p c; pure (if 2 * sum (vtVotes v) > k then 1 else 0)) simNs)
  in w { wRng = g, wSimN = U.map (+ 1) (wSimN w), wSimOk = U.zipWith (+) (wSimOk w) (U.fromList oks) }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w <- sliced simBudget (const True) (simOnce (pOf w0) (cOf w0)) w0
  pure $ if envFrame env `mod` 30 == 1
    then let (v, g) = evalR (wRng w) (voteOnce (nOf w) (pOf w) (cOf w)) in w { wLast = Just v, wRng = g }
    else w

draw :: Env -> World -> Picture
draw env w = view env
  [ -- ---- 左：独立な投票者の正確な曲線
    plotAxes g "people n (odd)" "majority accuracy of n independent people (exact binomial)" [1, 25, 51, 75, 101] [0, 0.25, 0.5, 0.75, 1]
  , Pictures [ Pictures [ Color (rgbC 120 95 100) (polyline 1 (map (uncurry (gPt g)) ln))
                        , if pp < 0.6 then sayLM ("p=" ++ fmtNum 2 pp) (grX g + grW g + 4) (gY g (majorityExact nMax pp)) 14 (rgbC 150 130 130) else Blank ]
             | (pp, ln) <- zip faintPs (cvFaint cv) ]
  , Color (colOf gold) (polyline 3 (map (uncurry (gPt g)) (cvInd cv)))
  , Color (colOf ink) (dashed 1 4 4 [(grX g, gY g 0.5), (grX g + grW g, gY g 0.5)])
  , Color (colOf gold) (circleAt (gX g (fromIntegral n)) (gY g (majorityExact n p)) 10)
  -- ---- 右：まねっこがいるとき
  , plotAxes g2 "people n" "copying model: line = exact, dots = simulation" [1, 25, 51, 75, 101] [0, 0.25, 0.5, 0.75, 1]
  , Color (colOf gold) (dashed 1.5 5 4 (map (uncurry (gPt g2)) (cvInd cv)))
  , Color (colOf teal) (polyline 3 (map (uncurry (gPt g2)) (cvCp cv)))
  , Color (colOf ink) (Pictures [ circleAt (gX g2 (fromIntegral k)) (gY g2 (fromIntegral ok / fromIntegral sn)) 9
                                | (k, sn, ok) <- zip3 simNs (U.toList (wSimN w)) (U.toList (wSimOk w)), sn > 0 ])
  , legend (grX g2 + 12) (grY g2 + 18)
      [ (gold, "independent (c = 0)")
      , (teal, "copy prob. c = " ++ fmtNum 2 c ++ " (correlation c^2 = " ++ fmtNum 2 (c * c) ++ ")")
      , (ink, "simulation (" ++ show (U.head (wSimN w)) ++ " runs each)") ]
  -- ---- 下：いまの n 人の 1 回の投票（金 = 正解、赤 = まちがい）
  , Pictures [ drawSlider (wDrag w == Just k) s | (k, s) <- zip [0 ..] (placed env w) ]
  , case wLast w of
      Just v | length (vtVotes v) == n ->
        let k = sum (vtVotes v)
        in Pictures [ Pictures [ Color (colOf (if x == 1 then gold else wine)) (circleAt (420 + fromIntegral (i `mod` cols) * cs) (vy + 10 + fromIntegral (i `div` cols) * cs) (cs * 0.8))
                               | (i, x) <- zip [0 :: Int ..] (vtVotes v) ]
                    , say ("One vote (each 30 frames): " ++ show k ++ " of " ++ show n ++ " right -> majority " ++ (if 2 * k > n then "right" else "wrong")
                           ++ " (leader " ++ (if vtLead v == 1 then "right" else "wrong") ++ ")") 420 (vy + 10 + 3 * cs + 20) 15 (rgbC 220 210 195) ]
      _ -> Blank
  , say ("n = " ++ show n ++ ": independent " ++ pctStr (majorityExact n p) ++ ", with copying " ++ pctStr (majorityCopy n p c) ++ " (one person " ++ pctStr p ++ ")")
      420 (vy + 10 + 3 * cs + 50) 16 (colOf ink)
  -- ---- 文字
  , say "Condorcet jury theorem: p > 0.5 and independent -> majority wins" 40 40 20 (colOf ink)
  , say "Drag sliders p, c, n   Space restart the simulation" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    p = pOf w
    c = cOf w
    n = nOf w
    cv = wCv w
    half = (envW env - 160) / 2
    g = Graph 70 120 (half - 40) (envH env * 0.42) 1 (fromIntegral nMax) 0 1
    g2 = Graph (120 + half) 120 (half - 60) (envH env * 0.42) 1 (fromIntegral nMax) 0 1
    vy = grY g + grH g + 70
    cols = min n 51
    cs = min 22 ((envW env - 520) / 51)

-- スライダーの位置（x = 70、y は下の段から 56 ずつ）
placed :: Env -> World -> [Slider]
placed env w = let vy = 120 + envH env * 0.42 + 70
               in [ s { slX = 70, slY = vy + 20 + 56 * fromIntegral k } | (k, s) <- zip [0 :: Int ..] (wSl w) ]

setSl :: Env -> Int -> Float -> World -> World
setSl env k mx w = recurve (resetSim w { wSl = [ if j == k then sliderSet s mx else s | (j, s) <- zip [0 ..] (placed env w) ] })

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pt ->
    let (mx, my) = toP5 env pt
    in case sliderHit (placed env w) (mx, my) of
         Just k -> setSl env k mx w { wDrag = Just k }
         Nothing -> w
  EventMotion pt | Just k <- wDrag w -> setSl env k (fst (toP5 env pt)) w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> resetSim w
  _ -> w

-- ---- 自己テスト
-- シミュレーション 2 万回（n = 21, p = 0.6, c = 0.4）：(多数決が正しかった回数, 1 人目の正解, 1 人目と 2 人目がともに正解)
simTest :: ((Int, Int, Int), Rng)
simTest = evalR (rngOf 77) (foldM (\(!ok, !o1, !two) _ -> do
                                      v <- voteOnce 21 0.6 0.4
                                      let vs = vtVotes v
                                          a = headOr 0 vs
                                          b = headOr 0 (drop 1 vs)
                                      pure (if 2 * sum vs > 21 then ok + 1 else ok, o1 + a, two + a * b)) (0, 0, 0) [1 .. 20000 :: Int])

testRng :: Rng
testRng = snd simTest

selfTest :: [(String, Bool)]
selfTest =
  [ ("2 項分布の和 = 総当たり（2^n 通り）、n = 1〜15 の奇数 × p = 0.3, 0.55, 0.6, 0.8（1e-12）", okB)
  , ("奇数 n で：p = 0.55 なら n とともに増え（n = 201 で " ++ fmtNum 3 (majorityExact 201 0.55) ++ "）、p = 0.45 なら減る", up && down)
  , ("n = 1 なら p そのもの、p = 0.5 ならどの n でも 0.5", near (majorityExact 1 0.7) 0.7 1e-12 && near (majorityExact 51 0.5) 0.5 1e-12)
  , ("まねっこモデル：c = 0 なら独立の式と同じ、c = 1 なら全員リーダーと同じで p", near (majorityCopy 21 0.6 0) (majorityExact 21 0.6) 1e-12 && near (majorityCopy 21 0.6 1) 0.6 1e-12)
  , ("シミュレーション 2 万回（n = 21, p = 0.6, c = 0.4）の多数決の正解率 " ++ fmtNum 4 (fromIntegral ok / tt) ++ " = 正確な値 " ++ fmtNum 4 ex ++ "（±4 標準誤差）"
    , abs (fromIntegral ok / tt - ex) < 4 * se)
  , ("まねっこでも 1 人の正解率は p（" ++ fmtNum 3 pa ++ "）、2 人の相関 ≈ c² = 0.16（" ++ fmtNum 3 (cov / (pa * (1 - pa))) ++ "）"
    , abs (pa - 0.6) < 0.015 && abs (cov / (pa * (1 - pa)) - 0.16) < 0.03) ]
  where
    -- 総当たり：2^n 通りの「誰が正解か」を全部数える
    brute n p = foldl' (\s m -> let k = popCount' m in if 2 * k > n then s + p ** fromIntegral k * (1 - p) ** fromIntegral (n - k) else s) 0 [0 .. 2 ^ n - 1 :: Int]
    popCount' m = length (filter odd (takeWhile (> 0) (iterate (`div` 2) m)))
    okB = and [ abs (brute n p - majorityExact n p) <= 1e-12 | n <- [1, 3 .. 15], p <- [0.3, 0.55, 0.6, 0.8] ]
    up = and [ majorityExact (n + 2) 0.55 > majorityExact n 0.55 | n <- [1, 3 .. 199] ]
    down = and [ majorityExact (n + 2) 0.45 < majorityExact n 0.45 | n <- [1, 3 .. 199] ]
    ((ok, o1, two), _) = simTest
    tt = 20000 :: Double
    ex = majorityCopy 21 0.6 0.4
    se = sqrt (ex * (1 - ex) / tt)
    pa = fromIntegral o1 / tt
    cov = fromIntegral two / tt - pa * pa

-- ---------- やってみよう ----------
-- 1. p = 0.51 でも、n = 101 なら多数決の正解率は何 %？ わずかな差が人数で大きくなる
-- 2. p = 0.45 にすると、人数を増やすほど多数決はまちがう。「みんなで決める」が危ないとき
-- 3. c を 0 → 1 に動かそう。c = 0.5 だと、n を増やしても正解率は何 % 付近で止まる？
-- 4. まねっこの多数決は、ほとんど「リーダー 1 人の答え」になる。アンサンブルで「ちがうモデル」を集める理由
-- 5. 03 では、投票ではなく数の「平均」で、相関があるとどこまで誤差が下がるかを式で確かめる

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

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

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
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

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

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
  [ Color (rgbC 95 70 75) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (rgbC 175 160 150) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gY g v + 6) 14 (rgbC 175 160 150) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (rgbC 215 205 190), say yl (grX g) (grY g - 8) 14 (rgbC 215 205 190) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.2 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashed wd on off pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
              k = max 1 (ceiling (l / (on + off))) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
              fr = on / (on + off)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + fr) / fromIntegral k)) | i <- [0 .. k - 1] ]

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (colOf c) (rectTL x (yy - 5) 14 10), say s (x + 22) (yy + 5) 14 (rgbC 220 210 195)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ]

-- ---- 画面の中のスライダー：Slider x y w lo hi v name d（表示の桁）int（整数か）。マウスでドラッグ
data Slider = Slider { slX :: !Float, slY :: !Float, slW :: !Float, slLo :: !Double, slHi :: !Double, slV :: !Double
                     , slName :: !String, slD :: !Int, slInt :: !Bool }

drawSlider :: Bool -> Slider -> Picture
drawSlider active s =
  let t = f2 ((slV s - slLo s) / (slHi s - slLo s))
  in Pictures [ Color (rgbC 110 80 85) (lineW 4 (slX s, slY s) (slX s + slW s, slY s))
              , Color (colOf gold) (lineW 4 (slX s, slY s) (slX s + t * slW s, slY s))
              , Color (if active then rgbC 255 233 214 else colOf ink) (circleAt (slX s + t * slW s) (slY s) 16)
              , say (slName s ++ " = " ++ fmtNum (slD s) (slV s)) (slX s) (slY s - 12) 14 (rgbC 215 205 190) ]

sliderHit :: [Slider] -> (Float, Float) -> Maybe Int
sliderHit sl (mx, my) = headOr Nothing [ Just k | (k, s) <- zip [0 ..] sl, mx > slX s - 10, mx < slX s + slW s + 10, abs (my - slY s) < 14 ]

sliderSet :: Slider -> Float -> Slider
sliderSet s mx = let v = slLo s + clampv (realToFrac ((mx - slX s) / slW s)) 0 1 * (slHi s - slLo s)
                 in s { slV = if slInt s then fromIntegral (jsRoundI v) else v }

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)

-- ---- 1 フレームの計算を budget ミリ秒までにする（p5.js の while (performance.now() − t0 < budget && more)）
--      step は必ず 1 回は動く
sliced :: Double -> (w -> Bool) -> (w -> w) -> w -> IO w
sliced budget more step w0 = if not (more w0) then pure w0 else do
  t0 <- getMonotonicTime
  let go w = do
        w' <- evaluate (step w)
        t <- getMonotonicTime
        if (t - t0) * 1000 < budget && more w' then go w' else pure w'
  go w0
