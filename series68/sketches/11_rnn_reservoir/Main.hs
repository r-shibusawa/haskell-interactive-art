-- ============================================
-- 68-11 再帰網の力学とリザバー計算 ─ ニューロン 100 個がランダムにつながった再帰網：x(t+1) = tanh(W x(t) + w_in u(t))
-- W の大きさは「スペクトル半径」ρ（固有値の絶対値の最大）で決まる。べき乗法：v ← W v / ‖W v‖ をくり返し、1 回あたりの伸び率の平均
--   ρ < 1：昔の状態はだんだん忘れる（ちがう初期状態でも、同じ入力ならやがて同じ状態に = エコー状態性）
--   ρ > 1：自分で動き続け、小さな差が消えない（混沌）
-- リザバー計算：W はランダムのまま固定し、出力の重みだけをリッジ回帰（連立 1 次方程式 1 回）で学ぶ。次の値を予測させる
-- 操作：↑↓ スペクトル半径 ρ / T 課題（サイン波 ⇔ マッキー-グラス）/ R 別のランダムな網 / スペース 一時停止
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

nR, wash, nFit, nTest :: Int
nR = 100                   -- ★ ニューロンの数
wash = 100                 -- ★ 捨てる長さ
nFit = 1000                -- ★ 学習の長さ
nTest = 400                -- ★ テストの長さ

ridge :: Double
ridge = 1e-6               -- ★ 出力の重みのリッジ

-- まばらな行列（2 割しかつながっていない）：行ごとに、0 でない列の番号と値（左から順）。
-- 0 を足しても和は変わらないので、ふつうの行列の計算とぴったり同じ答えになる
-- 行 i の値は spVal の spStart[i]〜spStart[i+1]−1 番目、列の番号は spCol に
data Sparse = Sparse { spN :: !Int, spStart :: !(U.Vector Int), spCol :: !(U.Vector Int), spVal :: !Vec }

-- 読み出しの学習の結果
data Fit = Fit { fWo :: !Vec, fPred :: ![Double], fTestS :: ![Double], fNmse :: !Double, fPhi :: !Mat, fY :: !Vec }

data World = World
  { wRho     :: !Double             -- 目標のスペクトル半径
  , wTask    :: !Int                -- 0 サイン波 / 1 マッキー-グラス
  , wSeed    :: !Int
  , wRes     :: !Sparse
  , wInp     :: !Vec                -- 入力の重み w_in
  , wA       :: !Vec                -- 初期状態だけちがう 2 つの網
  , wB       :: !Vec
  , wDist    :: ![Double]           -- 距離の記録（新しい順、400 個まで）
  , wTraces  :: ![Vec]              -- ニューロン 6 個の記録（新しい順、300 個まで）
  , wT       :: !Int
  , wSeries  :: !Vec
  , wFit     :: !Fit
  , wEst     :: !Double             -- べき乗法で測り直した ρ
  , wPaused  :: !Bool
  , wStage   :: !Int                -- 作り直しの残りの段（0 = 終わり）
  , wFeats   :: !Mat                -- 途中の計算（全部の時刻の状態）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-11 RNN reservoir" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (fNmse (wFit world0) + wEst world0)
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = finish (rebuildRes (World 0.9 0 1 (Sparse 0 (U.singleton 0) U.empty U.empty) U.empty U.empty U.empty [] [] 0 U.empty (Fit U.empty [] [] 0 V.empty U.empty) 0 False 0 V.empty []))

fromDense :: [[Double]] -> Sparse
fromDense rows = Sparse (length rows) (U.fromList (scanl (+) 0 [ length (filter (/= 0) r) | r <- rows ]))
                        (U.fromList [ j | r <- rows, (j, v) <- zip [0 ..] r, v /= 0 ])
                        (U.fromList [ v | r <- rows, v <- r, v /= 0 ])

spMatVec :: Sparse -> Vec -> Vec
spMatVec m x = U.generate (spN m) row
  where
    st = spStart m
    cs = spCol m
    vs = spVal m
    row i = go (U.unsafeIndex st i) (U.unsafeIndex st (i + 1)) 0
    go !k !e !s = if k >= e then s else go (k + 1) e (s + U.unsafeIndex vs k * U.unsafeIndex x (U.unsafeIndex cs k))

-- ★ スペクトル半径：べき乗法。複素数の固有値の組（回転）でも、長い区間の伸び率の平均（対数）なら正しく出る（注意：種を決め直す）
spectralRadius :: Sparse -> Int -> Int -> R Double
spectralRadius m iters seed = do
  seedRng seed
  v0 <- randnVec (spN m)
  let nv = normV v0
      go k v acc cnt
        | k >= iters = exp (acc / i2d cnt)
        | otherwise =
            let w = spMatVec m v
                nw = normV w
            in if not (nw > 0) then 0
               else if i2d k >= i2d iters / 3 then go (k + 1) (U.map (/ nw) w) (acc + log nw) (cnt + 1 :: Int)
               else go (k + 1) (U.map (/ nw) w) acc cnt
  pure (go 0 (U.map (/ nv) v0) 0 0)

-- 2 割だけつながったランダムな網を、スペクトル半径 rho にそろえる
randomReservoir :: Int -> Double -> Int -> R Sparse
randomReservoir n rho seed = do
  seedRng seed
  rows <- mapM (const (mapM (const (do u <- rnd; if u < 0.2 then randn else pure 0)) [1 .. n])) [1 .. n]
  let m = fromDense rows
  r0 <- spectralRadius m 600 (seed + 1)
  pure m { spVal = U.map (\v -> (v * rho) / r0) (spVal m) }

stepRes :: Sparse -> Vec -> Vec -> Double -> Vec
stepRes w inp x u = U.zipWith (\y wi -> tanh (y + wi * u)) (spMatVec w x) inp

-- 課題の時系列：サイン波、またはマッキー-グラス方程式 dx/dt = 0.2 x(t−17)/(1 + x(t−17)^10) − 0.1 x（オイラー法 dt = 0.1、1 ごとに記録）
makeSeries :: Int -> Int -> Vec
makeSeries kind len
  | kind == 0 = U.generate len (\t -> 0.8 * sin ((2 * pi * i2d t) / 25) * sin ((2 * pi * i2d t) / 210 + 1))
  | otherwise = U.take len (runST $ do
      let dt = 0.1
          lag = 170
      h <- MU.replicate (lag + 1) 1.2
      out <- MU.replicate len 0
      let go s x cnt
            | s >= (len + 300) * 10 = pure cnt
            | otherwise = do
                xl <- MU.read h (s `mod` (lag + 1))
                let x' = x + dt * ((0.2 * xl) / (1 + xl ** 10) - 0.1 * x)
                MU.write h (s `mod` (lag + 1)) x'
                cnt' <- if s `mod` 10 == 0 && s >= 3000 && cnt < len then MU.write out cnt (x' - 0.9) >> pure (cnt + 1)
                        else pure (if s `mod` 10 == 0 && s >= 3000 then cnt + 1 else cnt)
                go (s + 1) x' cnt'
      _ <- go 0 1.2 (0 :: Int)
      U.freeze out)

-- ★ 出力の重みの学習：状態 [x, u, 1] から次の値 u(t+1) をリッジ回帰で。テスト区間の正規化 2 乗誤差（NMSE）を返す
fitReadout :: Sparse -> Vec -> Vec -> Fit
fitReadout w inp ser = fitFrom (featsOf w inp ser) ser

-- 状態 [x, u, 1] を全部の時刻で（網に入力を流す）
featsOf :: Sparse -> Vec -> Vec -> Mat
featsOf w inp ser = V.fromList [ U.snoc (U.snoc x (ser U.! t)) 1 | (t, x) <- zip [0 ..] states ]
  where states = take (U.length ser - 1) (tail (scanl (\x t -> stepRes w inp x (ser U.! t)) (U.replicate nR 0) [0 ..]))

fitFrom :: Mat -> Vec -> Fit
fitFrom feats ser = Fit wo pred test (err / max 1e-12 vr) phi y
  where
    phi = V.slice wash nFit feats
    y = U.slice (wash + 1) nFit ser
    wo = ridgeSolve phi y ridge
    test = U.toList (U.slice (wash + nFit + 1) nTest ser)
    pred = [ dotV wo (feats V.! (wash + nFit + t)) | t <- [0 .. nTest - 1] ]
    m = meanOf test
    err = foldl' (\s (p, v) -> s + (p - v) ** 2) 0 (zip pred test)
    vr = foldl' (\s v -> s + (v - m) ** 2) 0 test

-- 網を作り直す。重い計算は 3 フレームに分ける（キーを押したフレームで網、次で状態、その次で出力の重み、最後に ρ の測り直し）
rebuildRes :: World -> World
rebuildRes w =
  let ((res, inp, a, b), _) = runR (do
        r <- randomReservoir nR (wRho w) (50 + wSeed w)
        seedRng (60 + wSeed w)
        i <- mapM (const ((\u -> u - 0.5) <$> rnd)) [1 .. nR]
        ab <- mapM (const (do p <- randn; q <- randn; pure (p * 0.8, q * 0.8))) [1 .. nR]
        pure (r, U.fromList i, U.fromList (map fst ab), U.fromList (map snd ab))) (rngOf 1)
  in w { wRes = res, wInp = inp, wA = a, wB = b, wDist = [], wTraces = [], wT = 0
       , wSeries = makeSeries (wTask w) (wash + nFit + nTest + 2), wStage = 1 }

-- 残りの計算を 1 段ずつ
advance :: World -> World
advance w = case wStage w of
  1 -> w { wFeats = featsOf (wRes w) (wInp w) (wSeries w), wStage = 2 }
  2 -> w { wFit = fitFrom (wFeats w) (wSeries w), wFeats = V.empty, wStage = 3 }
  3 -> w { wEst = withSeed 1 (spectralRadius (wRes w) 300 9), wStage = 0 }
  _ -> w

finish :: World -> World
finish w = if wStage w == 0 then w else finish (advance w)

step :: Env -> Float -> World -> IO World
step _ _ w00 = pure (if wPaused w0 then w0 else one (one w0))
  where
    w0 = advance w00
    one w = let ser = wSeries w
                u = ser U.! (wT w `mod` U.length ser)
                a = stepRes (wRes w) (wInp w) (wA w) u
                b = stepRes (wRes w) (wInp w) (wB w) u
                d = normV (U.zipWith (-) a b)
            in w { wA = a, wB = b, wT = wT w + 1, wDist = take 400 (d : wDist w), wTraces = take 300 (U.take 6 a : wTraces w) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Reservoir: spectral radius rho = " ++ fmtN rho 2 ++ (if rho < 1 then " (< 1: forgets)" else if rho == 1 then " (= 1: border)" else " (> 1: runs by itself)")
              ++ "   task " ++ (if wTask w == 1 then "Mackey-Glass" else "sine wave") ++ "   test NMSE = " ++ (if wStage w `elem` [1, 2] then "..." else fmtE (fNmse fr)))
             ("Up/Down rho   T task   R new random network   Space " ++ (if wPaused w then "resume" else "pause") ++ "   (" ++ show nR ++ " neurons, only the " ++ show (nR + 2) ++ " output weights are learned)")
  -- (1) 2 つの初期状態の距離
  , plotAxes g1 "time (last 400)" "(1) distance log10 |x_A - x_B| of two nets that differ only in the start" [0, 400] [-12, -6, 0]
  , plotXY g1 (map i2d [0 :: Int ..]) (map (\d -> logBase 10 (max 1e-13 d)) dists) (col mag) 2
  -- (2) ニューロン 6 個
  , plotAxes g2 "time" "(2) outputs x_i(t) of 6 neurons (tanh: -1 to 1)" [0, 300] [-1, 0, 1]
  , Pictures [ plotXY g2 (map i2d [0 :: Int ..]) [ tr U.! c | tr <- traces, U.length tr > c ] (colA 200 cc) 1.5 | (c, cc) <- zip [0 ..] [mag, lime, sky, amber, Main.violet, ink] ]
  -- (3) 予測
  , plotAxes g3 "time in the test part (not used for learning)" "(3) next-value prediction: white = real, magenta = reservoir (only output weights learned)" [0, 200] [grY0 g3, 0, grY1 g3]
  , plotXY g3 ts (take 200 (fTestS fr)) (colA 200 ink) 3
  , plotXY g3 ts (take 200 (fPred fr)) (col mag) 1.5
  -- 右上：説明と数値
  , say ("rho measured by power iteration = " ++ fmtN (wEst w) 4 ++ " (target " ++ fmtN rho 2 ++ ")") rx 120 15 (col ink)
  , say ("current distance |x_A - x_B| = " ++ fmtE (headOr 0 (wDist w))) rx 146 15 (col ink)
  , Pictures [ say s rx (180 + 20 * i2f i) 14 (col sub) | (i, s) <- zip [0 :: Int ..] notes ]
  , drawSelfTest env (wTest w) ]
  where
    rho = wRho w
    fr = wFit w
    lw = envW env * 0.46
    h3 = (envH env - 220) / 3
    g1 = Graph 70 105 (lw - 70) (h3 - 40) 0 400 (-12) 2
    g2 = Graph 70 (grY g1 + h3 + 20) (lw - 70) (h3 - 40) 0 300 (-1) 1
    g3 = let lim = if wTask w == 1 then 0.6 else 1 in Graph 70 (grY g2 + h3 + 20) (envW env - 110) (h3 - 30) 0 200 (-lim) lim
    dists = reverse (wDist w)
    traces = reverse (wTraces w)
    ts = map i2d [0 .. 199 :: Int]
    rx = lw + 40
    notes = [ "rho < 1: the distance falls like a line -> state = recent input"
            , "rho near 1: forgets slowly = longer memory, but if too big"
            , "it moves by itself, ignoring the input (try rho = 1.3-1.6)"
            , "only the output weights: solve (P^T P + lambda I) w = P^T y once"
            , "no backprop through time (vanishing/exploding: 01, 07)"
            , "NMSE = squared error / variance of the real series (0 = best)" ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuildRes w { wRho = min 1.6 (i2d (jsRound ((wRho w + 0.1) * 10)) / 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuildRes w { wRho = max 0.1 (i2d (jsRound ((wRho w - 0.1) * 10)) / 10) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char ch) Down _ _
    | ch `elem` "tT" -> rebuildRes w { wTask = 1 - wTask w }
    | ch `elem` "rR" -> rebuildRes w { wSeed = wSeed w + 1 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ρ = 0.5、0.9、1.2、1.5 で (1) の距離のグラフをくらべる。忘れる速さは ρ とどう関係する？
-- 2. マッキー-グラス（T）で、ρ を変えて NMSE がいちばん小さくなるところを探す
-- 3. nR を 20 や 300 にすると予測はどう変わる？
-- 4. ridge を 1 にすると？ 1e-12 にすると？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("べき乗法：回転を含む行列（固有値が複素数の組、ρ = 0.9）で " ++ fmtN e1 5, abs (e1 - 0.9) < 0.009)
  , ("ランダムな網を ρ = 0.8 にそろえ、別の初期ベクトルで測り直しても " ++ fmtN e2 4, abs (e2 - 0.8) < 0.016)
  , ("ρ = 0.8：ちがう初期状態でも 300 歩で同じ状態に（距離 " ++ fmtE dist ++ "、エコー状態性）", dist < 1e-6)
  , ("ρ = 1.5、入力 0：300 歩たっても動き続ける（‖x‖ = " ++ fmtN (normV c) 2 ++ "）", normV c > 0.5)
  , ("ρ = 0.9：サイン波の次の値をテスト区間で予測、NMSE = " ++ fmtE (fNmse fr) ++ " < 1e−3", fNmse fr < 1e-3)
  , ("出力の重みは正規方程式 Φᵀ(Φw − y) + λw = 0 を満たす（残り " ++ fmtE res ++ "）", res < 1e-6) ]
  where
    rot r a = [[r * cos a, -r * sin a], [r * sin a, r * cos a]]
    blockRow k i = concat [ if q == k then rot r a !! i else [0, 0] | (q, (r, a)) <- zip [0 :: Int ..] [(0.9, 0.7), (0.5, 2.1), (0.3, 0.2)] ]
    blk = fromDense [ blockRow k i | k <- [0, 1, 2], i <- [0, 1] ]
    e1 = withSeed 1 (spectralRadius blk 600 4)
    (e2, inp, a0, b0) = withSeed 1 $ do
      wr <- randomReservoir nR 0.8 51
      e <- spectralRadius wr 800 77
      seedRng 61
      i <- U.fromList <$> mapM (const ((\u -> u - 0.5) <$> rnd)) [1 .. nR]
      ab <- mapM (const ((,) <$> randn <*> randn)) [1 .. nR]
      pure (e, i, U.fromList (map fst ab), U.fromList (map snd ab))
    w08 = withSeed 1 (randomReservoir nR 0.8 51)
    (a3, b3) = foldl' (\(a, b) t -> let u = sin (i2d t * 0.3) in (stepRes w08 inp a u, stepRes w08 inp b u)) (a0, b0) [0 .. 299 :: Int]
    dist = normV (U.zipWith (-) a3 b3)
    (w15, c0) = withSeed 1 $ do
      wr <- randomReservoir nR 1.5 52
      cs <- mapM (const ((0.5 *) <$> randn)) [1 .. nR]
      pure (wr, U.fromList cs)
    c = foldl' (\x _ -> stepRes w15 inp x 0) c0 [1 .. 300 :: Int]
    w09 = withSeed 1 (randomReservoir nR 0.9 53)
    fr = fitReadout w09 inp (makeSeries 0 (wash + nFit + nTest + 2))
    gW = V.foldl' (\g (f, yi) -> let e = dotV (fWo fr) f - yi in U.zipWith (\gj fj -> gj + fj * e) g f) (U.replicate (nR + 2) 0) (V.zip (fPhi fr) (V.convert (fY fr)))
    res = U.maximum (U.zipWith (\g wv -> abs (g + ridge * wv)) gW (fWo fr))

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
