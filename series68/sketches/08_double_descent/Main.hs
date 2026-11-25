-- ============================================
-- 68-08 二重降下（ダブルディセント）─ ランダムな特徴 p 個の線形回帰を、ノイズ入りのデータ n 個に当てはめる
-- 特徴 = 折れ目の位置 t と向き s をランダムに決めた ReLU(s (x − t))（隠れ層を固定して、出力の重みだけ学ぶネット）
-- グラフは 40 組のデータの平均（1 組の誤差は 10 で頭打ち）
--   p < n：最小 2 乗。p を増やすとテスト誤差が下がり、やがて上がる（ここまでは昔ながらの U 字）
--   p = n：データをちょうど通る解が 1 つだけ → ノイズまで無理に通すので、テスト誤差が跳ね上がる
--   p > n：通る解は無数 → いちばん重みが小さい解（最小ノルム解 w = Φᵀ(ΦΦᵀ)⁻¹y）をえらぶと、テスト誤差がまた下がる
-- リッジ正則化（λ‖w‖² を足す）を入れると山が消える。U 字のバイアス-バリアンスの話（series64/02）の「その先」
-- 操作：← → 特徴の数 p / ↑↓ リッジの強さ λ / N 左のデータを作り直す / R 平均の種を変える
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

nTrain :: Int
nTrain = 12                -- ★ データの数

noise :: Double
noise = 0.2                -- ★ ノイズ

pMax, nSeeds :: Int
pMax = 200                 -- ★ 特徴の最大数
nSeeds = 40                -- ★ 平均する回数

pList :: [Int]
pList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 25, 30, 40, 50, 70, 100, 140, 200]

lams :: [Double]
lams = [1e-4, 1e-3, 1e-2, 1e-1]

minNorm :: Double
minNorm = 1e-10            -- 「λ → 0」の代わりのごく小さな λ（割り算 0 を防ぐだけ）

errCap :: Double
errCap = 10                -- ★ 1 組の誤差は 10 で頭打ち（ひどく暴れた 1 組だけで平均が決まらないように）

-- データ 1 組：x, y と、特徴の折れ目 t・向き s（pMax 個まとめて作り、前から p 個使う）
data DSet = DSet { dXs :: !Vec, dYs :: !Vec, dTs :: !Vec, dSs :: !Vec }

-- 平均の曲線：最小ノルム解のテスト誤差、リッジのテスト誤差、訓練誤差（pList の順）、何組ぶんか
data Curve = Curve { cMn :: ![Double], cRg :: ![Double], cTr :: ![Double], cK :: !Int }

data World = World
  { wP       :: !Int                  -- pList の番号
  , wLam     :: !Int                  -- lams の番号
  , wDataSeed :: !Int
  , wSeedBase :: !Int
  , wCurve   :: !Curve
  , wDemo    :: !DSet
  , wW0      :: !Vec                  -- 左のデータの最小ノルム解
  , wWr      :: !Vec                  -- 左のデータのリッジ解
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-08 Double descent" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (U.sum (wW0 world0) + U.sum (wWr world0))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = refit (makeDemo (World 7 2 1 1 (startCurve) e U.empty U.empty []))
  where e = DSet U.empty U.empty U.empty U.empty

trueF :: Double -> Double
trueF x = sin (pi * x)

-- x は少しずらした等間隔、y = sin(πx) + ノイズ
makeSet :: Int -> DSet
makeSet seed = withSeed seed $ do
  pts <- mapM (\i -> do u <- rnd
                        let x = -1 + (2 * (i2d i + 0.5)) / i2d nTrain + (0.6 / i2d nTrain) * (2 * u - 1)
                        g <- randn
                        pure (x, trueF x + noise * g)) [0 .. nTrain - 1]
  fs <- mapM (const (do g <- randn
                        u <- rnd
                        pure (if g > 0 then 1 else -1, 0.9 * (-1 + 2 * u)))) [1 .. pMax]
  pure (DSet (U.fromList (map fst pts)) (U.fromList (map snd pts)) (U.fromList (map snd fs)) (U.fromList (map fst fs)))

-- 特徴 ReLU(s_j (x − t_j))、j = 0〜p−1
feats :: DSet -> Int -> Double -> Vec
feats d p x = U.generate p (\j -> let v = dSs d U.! j * (x - dTs d U.! j) in if v > 0 then v else 0)

-- ★ 最小ノルム解（lam → 0）とリッジ解：p ≤ n なら (ΦᵀΦ + λI) w = Φᵀy、p > n なら w = Φᵀ (ΦΦᵀ + λI)⁻¹ y（どちらもリッジ解と同じ式）
fitRF :: DSet -> Int -> Double -> Vec
fitRF d p lam
  | p <= n = ridgeSolve phi (dYs d) lam
  | otherwise =
      let kk = V.generate n (\i -> U.generate n (\k -> let s = dotV (phi V.! min i k) (phi V.! max i k) in if i == k then s + lam else s))
          al = fromMaybe (U.replicate n 0) (solveLinear kk (dYs d))
      in foldl' (\w i -> U.zipWith (\wj fj -> wj + fj * (al U.! i)) w (phi V.! i)) (U.replicate p 0) [0 .. n - 1]
  where
    phi = V.map (feats d p) (V.convert (dXs d))
    n = V.length phi

predictRF :: DSet -> Vec -> Double -> Double
predictRF d w x = dotV w (feats d (U.length w) x)

testErr :: DSet -> Vec -> Double
testErr d w = foldl' (\s t -> let x = -1 + (2 * (i2d t + 0.5)) / 100
                                  e = predictRF d w x - trueF x
                              in s + e * e) 0 [0 .. 99 :: Int] / 100

trainErr :: DSet -> Vec -> Double
trainErr d w = foldl' (\s i -> let e = predictRF d w (dXs d U.! i) - dYs d U.! i in s + e * e) 0 [0 .. U.length (dXs d) - 1] / i2d (U.length (dXs d))

-- 1 組ぶんを平均に足す（k 組目）
curveAdd :: Double -> Curve -> DSet -> Int -> Curve
curveAdd lam c d k = Curve (zipWith upd (cMn c) mns) (zipWith upd (cRg c) rgs) (zipWith upd (cTr c) trs) (cK c)
  where
    res = [ let w0 = fitRF d p minNorm
                wr = fitRF d p lam
            in (min errCap (testErr d w0), testErr d wr, trainErr d w0) | p <- pList ]
    mns = [ a | (a, _, _) <- res ]
    rgs = [ b | (_, b, _) <- res ]
    trs = [ t | (_, _, t) <- res ]
    upd acc v = acc + (v - acc) / i2d k

startCurve :: Curve
startCurve = let z = map (const 0) pList in Curve z z z 0

makeDemo :: World -> World
makeDemo w = w { wDemo = makeSet (900 + wDataSeed w) }

refit :: World -> World
refit w = let p = at' pList (wP w) 1
          in w { wW0 = fitRF (wDemo w) p minNorm, wWr = fitRF (wDemo w) p (at' lams (wLam w) 0) }

-- p5.js 版は 1 フレームに 8 ミリ秒ぶん。ここでは 1 フレームに 1 組
step :: Env -> Float -> World -> IO World
step _ _ w
  | cK c < nSeeds = let k = cK c + 1
                        c' = curveAdd (at' lams (wLam w) 0) c { cK = k } (makeSet (wSeedBase w * 1000 + k)) k
                    in pure w { wCurve = c' }
  | otherwise = pure w
  where c = wCurve w

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Double descent: features p = " ++ show p ++ ", data n = " ++ show nTrain
              ++ (if p < nTrain then " (p < n: least squares)" else if p == nTrain then " (p = n: one exact fit)" else " (p > n: min-norm solution)")
              ++ "   ridge lambda = " ++ jsNum lam)
             ("Left/Right number of features p   Up/Down ridge strength lambda   N new data on the left   R new seeds for the average   (average " ++ show (cK c) ++ " / " ++ show nSeeds ++ " sets)")
  -- ---- 左：当てはめ
  , plotAxes g "x" "white = true sin(pi x), magenta = min-norm, lime = ridge" [-1, 0, 1] [-2, 0, 2]
  , plotXY g xs (map trueF xs) (colA 150 ink) 2
  , plotXY g xs (map (predictRF d (wWr w)) xs) (col lime) 2
  , plotXY g xs (map (predictRF d (wW0 w)) xs) (col mag) 3
  , Color (col amber) (Pictures [ circleAt (gX g x) (gY g y) 10 | (x, y) <- zip (U.toList (dXs d)) (U.toList (dYs d)) ])
  , Color (col sub) (Pictures [ Line [(gX g t, grY g + grH g - 14), (gX g t, grY g + grH g - 2)] | t <- take (min p 60) (U.toList (dTs d)) ])
  , say ("this data: test error " ++ fmtN (testErr d (wW0 w)) 3 ++ " (ridge " ++ fmtN (testErr d (wWr w)) 3 ++ ")") 60 ty 14 (col ink)
  , say ("train error " ++ fmtE (trainErr d (wW0 w)) ++ "   weight size |w| = " ++ fmtN (normV (wW0 w)) 2) 60 (ty + 20) 14 (col ink)
  , say "orange = training data (noisy), ticks = feature kinks" 60 (ty + 42) 14 (col sub)
  , say "near p = n: huge weights to hit every point -> wild" 60 (ty + 62) 14 (col sub)
  -- ---- 右：p とテスト誤差
  , plotAxes g2 "features p (log)" ("log10 test error (average of " ++ show (cK c) ++ " sets, capped at 10 each)") [] [-2, -1, 0, 1]
  , Pictures [ sayC (show v) (gX g2 (logBase 10 (i2d v))) (grY g2 + grH g2 + 18) 14 (col sub) | v <- [1, 2, 5, 12, 20, 50, 100, 200 :: Int] ]
  , Color (colA 160 amber) (Line [(gX g2 ln12, grY g2), (gX g2 ln12, grY g2 + grH g2)])
  , say ("p = n = " ++ show nTrain) (gX g2 ln12 + 4) (grY g2 + 18) 14 (col amber)
  , if cK c > 0
      then Pictures [ plotXY g2 lx (lg (cTr c)) (colA 160 sub) 1.5
                    , plotXY g2 lx (lg (cRg c)) (col lime) 2.5
                    , plotXY g2 lx (lg (cMn c)) (col mag) 3
                    , Color (col ink) (circleAt (gX g2 (at' lx (wP w) 0)) (gY g2 (logBase 10 (max 1e-6 (at' (cMn c) (wP w) 0)))) 11) ]
      else Blank
  , legend (grX g2 + grW g2 - 250) (grY g2 + 30) [(mag, "min-norm (lambda -> 0)"), (lime, "ridge lambda = " ++ jsNum lam), (sub, "train error (0 for p >= n)")]
  , say ("peak at p = " ++ show (at' pList im 1) ++ "   error: p = 8 " ++ fmtN (at' (cMn c) 7 0) 3 ++ ", peak " ++ fmtN (at' (cMn c) im 0) 2 ++ ", p = 200 " ++ fmtN (lastOr 0 (cMn c)) 3) rx ty 14 (col ink)
  , say "\"Too many features overfit\" only holds near p = n. With even more," rx (ty + 22) 14 (col sub)
  , say "the smallest-weight solution (smooth) is chosen and it improves again." rx (ty + 42) 14 (col sub)
  , say "Ridge stops the weights from growing -> no blow-up at p = n, peak gone." rx (ty + 62) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    c = wCurve w
    d = wDemo w
    p = at' pList (wP w) 1
    lam = at' lams (wLam w) 0
    lw = envW env * 0.42
    g = Graph 60 110 (lw - 60) (envH env - 330) (-1) 1 (-2.5) 2.5
    xs = [ -1 + i2d i / 150 | i <- [0 .. 300 :: Int] ]
    ty = grY g + grH g + 56
    rx = lw + 60
    g2 = Graph rx 110 (envW env - rx - 40) (envH env - 330) 0 (logBase 10 (i2d pMax)) (-2.5) 1.5
    ln12 = logBase 10 (i2d nTrain)
    lx = map (logBase 10 . i2d) pList
    lg = map (logBase 10 . max 1e-6)
    im = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 (cMn c)) (zip [0 ..] (cMn c)))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> refit w { wP = min (length pList - 1) (wP w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> refit w { wP = max 0 (wP w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> refit w { wLam = min (length lams - 1) (wLam w + 1), wCurve = startCurve }
  EventKey (SpecialKey KeyDown) Down _ _ -> refit w { wLam = max 0 (wLam w - 1), wCurve = startCurve }
  EventKey (Char ch) Down _ _
    | ch `elem` "nN" -> refit (makeDemo w { wDataSeed = wDataSeed w + 1 })
    | ch `elem` "rR" -> w { wSeedBase = wSeedBase w + 1, wCurve = startCurve }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ← → で p を 8 → 12 → 200 と動かし、左の当てはめの暴れ方を見る。‖w‖ はどうなっている？
-- 2. noise を 0 にすると山はどうなる？ 0.5 にしたら？
-- 3. nTrain を 20 にすると、山の位置は動く？
-- 4. λ を ↑↓ で変える。山を消すのにちょうどよい λ は？ 大きすぎると何が悪くなる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("最小ノルム解のテスト誤差の山は p ≈ n（p = " ++ show pk ++ "、n = " ++ show nTrain ++ "）", abs (pk - nTrain) <= 2)
  , ("山は両側の 5 倍以上（山 " ++ fmtN mnIm 2 ++ "、p ≤ 8 の最小 " ++ fmtN low 3 ++ "、p = 200 で " ++ fmtN lst 3 ++ "）", mnIm > 5 * low && mnIm > 5 * lst)
  , ("二重降下：p = 200 は p ≤ 8 のいちばん良い所よりさらに良い", lst < low)
  , ("リッジ λ = 0.01 で山が消える（p = n で " ++ fmtN (at' (cRg acc) iN 0) 3 ++ " < 最小ノルム解の 1/5）"
    , at' (cRg acc) iN 0 < at' (cMn acc) iN 0 / 5 && maximum (drop 8 (cRg acc)) < 2 * minimum (cRg acc))
  , ("p = 60 > n：最小ノルム解は全部の点を通り、通る別の解 w + v（Φv = 0）より ‖w‖ が小さい"
    , trainErr d w < 1e-12 && trainErr d w2 < 1e-12 && normV w < normV w2) ]
  where
    acc = foldl' (\cc k -> curveAdd 1e-2 cc (makeSet (5000 + k)) k) startCurve [1 .. nSeeds]
    mns = cMn acc
    im = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 mns) (zip [0 ..] mns))
    pk = at' pList im 0
    mnIm = at' mns im 0
    iN = length (takeWhile (/= nTrain) pList)
    lst = lastOr 0 mns
    low = minimum (take 8 mns)
    d = makeSet 77
    w = fitRF d 60 minNorm
    phi = V.map (feats d 60) (V.convert (dXs d))
    v = withSeed 78 (randnVec 60)
    kk = V.generate nTrain (\i -> U.generate nTrain (\k -> dotV (phi V.! i) (phi V.! k)))
    al = fromMaybe (U.replicate nTrain 0) (solveLinear kk (U.generate nTrain (\i -> dotV (phi V.! i) v)))
    nul = U.imap (\j x -> x - foldl' (\s i -> s + (phi V.! i) U.! j * al U.! i) 0 [0 .. nTrain - 1]) v
    w2 = U.zipWith (\x n -> x + 0.3 * n) w nul

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

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

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

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

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

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
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
