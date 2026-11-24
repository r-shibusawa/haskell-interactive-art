-- ============================================
-- 64-02 バイアスとバリアンス ─ sin の曲線 + ノイズから訓練データを 100 回作り直し、そのたびに多項式をあてはめる
-- うすい線 = 100 本の当てはめ、金 = その平均、白 = 本当の曲線。期待誤差 = バイアス² + バリアンス + ノイズ
--   バイアス² = 平均の曲線と本当の曲線のずれ、バリアンス = 100 本のばらつき、ノイズ = σ²（どんなモデルでも消せない）
--   次数が低いとバイアスが大きく、高いとバリアンスが大きい。B でバギング（ブートストラップ 25 回の平均）を重ねる
--   バギングが効くのは「不安定な」モデル（次数 11〜12）。安定なモデルではかえって悪くなることも（深い木の例は series62/08）
-- 操作：← → 多項式の次数 / B バギング / ↑↓ 点の数 / R やり直す
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

mSets, bBag :: Int
mSets = 100                    -- ★ 訓練データを作る回数
bBag = 25                      -- ★ バギングの回数

noiseSd, ridgeL :: Double
noiseSd = 0.3                  -- ★ ノイズ
ridgeL = 1e-3                  -- ★ ごく小さな正則化

gxs :: [Double]                -- 誤差を測る x（0〜1 を 100 等分）
gxs = [ fromIntegral i / 100 | i <- [0 .. 100 :: Int] ]

ftrue :: Double -> Double      -- ★ 本当の関数
ftrue x = sin (2 * pi * x)

-- 訓練データ 1 組と、その当てはめ（gxs の上の値）、誤差を測るための新しいノイズ、バギングの平均
data DSet = DSet { sXs :: ![Double], sYs :: ![Double], sFit :: !(U.Vector Double), sNoise :: !(U.Vector Double), sBag :: !(Maybe (U.Vector Double)) }

-- バイアス²・バリアンス・期待誤差
data BV = BV { bvMean :: !(U.Vector Double), bvB2 :: !Double, bvV :: !Double, bvMseNF :: !Double, bvMse :: !Double }

data World = World
  { wDeg      :: !Int              -- ★ 多項式の次数
  , wN        :: !Int              -- ★ 1 回の点の数
  , wBag      :: !Bool
  , wSeedD    :: !Int
  , wSets     :: ![DSet]           -- 新しい順
  , wDegStats :: ![BV]             -- 次数 0, 1, 2, … の順
  , wSt       :: !(Maybe BV)       -- いまの 100 本の統計
  , wSb       :: !(Maybe BV)       -- バギングの統計
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-02 Bias and variance" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 3 20 False 1 [] [] Nothing Nothing []

restart :: World -> World
restart w = w { wSets = [], wDegStats = [], wSt = Nothing, wSb = Nothing }

-- ★ 訓練データを 1 組作って当てはめる（bag = True ならバギングも）
makeSet :: Int -> Int -> Int -> Int -> Bool -> DSet
makeSet seedD k d n bag = withSeed (seedD * 100003 + k * 7 + n) $ do
  pts <- mapM (const (do x <- rnd; z <- rndGauss; pure (x, ftrue x + noiseSd * z))) [1 .. n]
  let xs = map fst pts
      ys = map snd pts
      f = polyFit xs ys d ridgeL
      xv = V.fromList xs
      yv = V.fromList ys
  nz <- mapM (const ((noiseSd *) <$> rndGauss)) gxs
  bg <- if not bag then pure Nothing else do
    let one acc _ = do
          js <- mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]
          let g = polyFit (map (xv V.!) js) (map (yv V.!) js) d ridgeL
          pure (U.zipWith (\a x -> a + polyEval g x / fromIntegral bBag) acc (U.fromList gxs))
    Just <$> foldM one (U.replicate (length gxs) 0) [1 .. bBag]
  pure (DSet xs ys (U.fromList (map (polyEval f) gxs)) (U.fromList nz) bg)

-- ★ バイアス²・バリアンス・期待誤差（fs = 当てはめの列、それぞれ gxs の上の値）
bvStats :: [U.Vector Double] -> Maybe [U.Vector Double] -> BV
bvStats fs noiseArr = BV mean (b2 / gg) (v / (gg * mm)) (mseNF / (gg * mm)) (mse / (gg * mm))
  where
    mm = fromIntegral (length fs)
    gg = fromIntegral (length gxs)
    mean = U.generate (length gxs) (\q -> foldl' (\s f -> s + f U.! q) 0 fs / mm)
    nzs = fromMaybe [] noiseArr
    (b2, v, mseNF, mse) = foldl' perQ (0, 0, 0, 0) (zip [0 ..] gxs)
    perQ (!a, !b, !c, !e) (q, x) =
      let mq = mean U.! q
          t = ftrue x
          (b', c', e') = foldl' (\(!bb, !cc, !ee) (m, f) ->
                                   let fq = f U.! q
                                       ee' = case drop m nzs of
                                               (nz : _) -> let r = fq - t - nz U.! q in ee + r * r
                                               [] -> ee
                                   in (bb + (fq - mq) * (fq - mq), cc + (fq - t) * (fq - t), ee')) (b, c, e) (zip [0 ..] fs)
      in (a + (mq - t) * (mq - t), b', c', e')

-- 1 歩：まず 100 組を作り、そのあと次数ごとの比較（裏で 40 組ずつ）
moreWork :: World -> Bool
moreWork w = length (wSets w) < mSets || length (wDegStats w) <= 12

workOne :: World -> World
workOne w
  | length (wSets w) < mSets =
      let !s = makeSet (wSeedD w) (length (wSets w)) (wDeg w) (wN w) (wBag w)
      in maybe () (`seq` ()) (sBag s) `seq` w { wSets = s : wSets w }   -- ここで計算しきる（時間を正しく測るため）
  | otherwise =
      let d = length (wDegStats w)
          ss = [ makeSet (wSeedD w) k d (wN w) False | k <- [0 .. 39] ]
          !st = bvStats (map sFit ss) (Just (map sNoise ss))
      in w { wDegStats = wDegStats w ++ [st] }

step :: Env -> Float -> World -> IO World
step _ _ w0 = do
  w <- sliced 6 moreWork workOne w0                -- 1 フレームに 6 ミリ秒だけ計算（画面は止まらない）
  let ss = reverse (wSets w)
      changed = length (wSets w) /= length (wSets w0)
  pure $ if not changed then w else
    w { wSt = if null ss then Nothing else Just (bvStats (map sFit ss) (Just (map sNoise ss)))
      , wSb = case ss of
                (s0 : _) | wBag w, Just _ <- sBag s0 -> Just (bvStats [ fromMaybe (sFit s) (sBag s) | s <- ss ] (Just (map sNoise ss)))
                _ -> Nothing }

draw :: Env -> World -> Picture
draw env w = view env
  [ -- ---- 左：当てはめの束
    plotAxes g "x" ("thin: " ++ show (length ss) ++ " fits, gold: mean, white: true sin" ++ (if wBag w then ", teal: bagging" else "")) [0, 0.5, 1] [-2, -1, 0, 1, 2]
  , Color (colA (if wBag w then 18 else 40) teal) (Pictures [ curve (sFit s) | s <- ss ])
  , case wSb w of
      Just _ -> Color (colA 30 teal) (Pictures [ maybe Blank curve (sBag s) | s <- ss ])
      Nothing -> Blank
  , Color (colOf ink) (polyline 2.5 [ gPt g x (ftrue x) | x <- gxs ])
  , maybe Blank (\st -> Color (colOf gold) (polyline 3 (zipWith (gPt g) gxs (U.toList (bvMean st))))) (wSt w)
  , maybe Blank (\sb -> Color (colOf teal) (dashed 3 7 5 (zipWith (gPt g) gxs (U.toList (bvMean sb))))) (wSb w)
  , case reverse ss of                     -- 最初の 1 組の訓練データ
      (s0 : _) -> Color (colOf wine) (Pictures [ circleAt (gX g x) (gY g y) 7 | (x, y) <- zip (sXs s0) (sYs s0) ])
      [] -> Blank
  -- ---- 右上：棒グラフ（バイアス² + バリアンス + ノイズ と 測った期待誤差）
  , case wSt w of
      Nothing -> Blank
      Just st ->
        let rows = [ ("bias^2", bvB2 st, gold), ("variance", bvV st, teal), ("noise sigma^2", noiseSd * noiseSd, (150, 130, 130))
                   , ("sum of the 3", bvB2 st + bvV st + noiseSd * noiseSd, ink), ("measured error", bvMse st, wine) ]
                   ++ maybe [] (\sb -> [("bagging: bias^2", bvB2 sb, gold), ("bagging: variance", bvV sb, teal), ("bagging: error", bvMse sb, wine)]) (wSb w)
            top = maximum (0.5 : [ v | (_, v, _) <- rows ])
            bw = realToFrac (rw - 230) / top
        in Pictures [ Pictures [ Color (colOf c) (rectTL (rx + 160) yy (max 1 (min (rw - 230) (f2 (v * bw)))) 18)
                               , sayRM nm (rx + 150) (yy + 9) 14 (rgbC 220 210 195)
                               , sayLM (fmtNum 4 v) (rx + 166 + min (rw - 230) (f2 (v * bw))) (yy + 9) 14 (rgbC 220 210 195) ]
                    | (k, (nm, v, c)) <- zip [0 :: Int ..] rows, let yy = 110 + 28 * fromIntegral k ]
  -- ---- 右下：次数とバイアス²・バリアンス
  , plotAxes g2 "polynomial degree" "by degree: gold bias^2, teal var, white error" [0, 3, 6, 9, 12] [0, 0.4, 0.8]
  , Color (colOf gold) (polyline 2.5 [ gPt g2 (fromIntegral d) (bvB2 s) | (d, s) <- dd ])
  , Color (colOf teal) (polyline 2.5 [ gPt g2 (fromIntegral d) (bvV s) | (d, s) <- dd ])
  , Color (colOf ink) (polyline 2.5 [ gPt g2 (fromIntegral d) (bvMse s) | (d, s) <- dd ])
  , Color (colOf wine) (lineW 1 (gX g2 (fromIntegral (wDeg w)), grY g2) (gX g2 (fromIntegral (wDeg w)), grY g2 + grH g2))
  -- ---- 文字
  , say ("Bias and variance   degree " ++ show (wDeg w) ++ "   points " ++ show (wN w) ++ "   "
         ++ (if wBag w then "bagging ON (mean of " ++ show bBag ++ ")" else "bagging OFF")) 40 40 20 (colOf ink)
  , say "Left/Right degree   B bagging   Up/Down points   R restart" 40 66 15 (rgbC 170 155 145)
  , maybe Blank (\st -> say ("noise-free error " ++ fmtNum 5 (bvMseNF st) ++ " = bias^2 + variance " ++ fmtNum 5 (bvB2 st + bvV st) ++ " (always exact)")
                             (grX g) (grY g + grH g + 50) 14 (rgbC 200 185 170)) (wSt w)
  , drawSelfTest env (wTest w) ]
  where
    ss = wSets w
    side = min (envH env - 190) (envW env * 0.48)
    g = Graph 60 110 side (side * 0.82) 0 1 (-2) 2
    curve f = polyline 1 (zipWith (gPt g) gxs (U.toList f))
    rx = grX g + grW g + 90
    rw = envW env - rx - 40
    g2 = Graph rx (110 + 28 * 8 + 50) (rw - 20) (max 80 (envH env - 110 - 28 * 8 - 140)) 0 12 0 0.8
    dd = zip [0 :: Int ..] (wDegStats w)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> clearSets w { wDeg = min 12 (wDeg w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> clearSets w { wDeg = max 0 (wDeg w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wN = min 60 (wN w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wN = max 15 (wN w - 5) }
  EventKey (Char c) Down _ _
    | c == 'b' || c == 'B' -> clearSets w { wBag = not (wBag w) }
    | c == 'r' || c == 'R' -> restart w { wSeedD = wSeedD w + 1 }
  _ -> w
  where clearSets x = x { wSets = [], wSt = Nothing, wSb = Nothing }

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("ノイズなしの誤差 = バイアス² + バリアンス（次数 5、60 組、1e-9 の恒等式）", near (bvMseNF s5) (bvB2 s5 + bvV s5) 1e-9)
  , ("バイアス² + バリアンス + σ² " ++ fmtNum 4 (bvB2 s5 + bvV s5 + 0.09) ++ " ≈ 新しいノイズで測った期待誤差 " ++ fmtNum 4 (bvMse s5) ++ "（8% 以内）"
    , abs (bvMse s5 / (bvB2 s5 + bvV s5 + 0.09) - 1) < 0.08)
  , ("次数 12・20 点（不安定）：バギングでバリアンス " ++ fmtNum 3 (bvV a) ++ " → " ++ fmtNum 3 (bvV b) ++ "（半分未満）", bvV b < 0.5 * bvV a)
  , ("次数 0（定数）：バイアス² ≈ sin² の平均 ≈ 0.5（" ++ fmtNum 3 (bvB2 s0b) ++ "）", abs (bvB2 s0b - 0.495) < 0.03)
  , ("同じ訓練データなら、次数を上げるほど訓練誤差は下がる（0〜12）", mono)
  , ("ノイズのない 3 次式は次数 3 でぴったり当たる（チェビシェフ多項式の最小 2 乗法）", all (\x -> near (polyEval fc x) (cub x) 1e-9) [0, 0.37, 1]) ]
  where
    sets5 = [ makeSet 1 k 5 20 False | k <- [0 .. 59] ]
    s5 = bvStats (map sFit sets5) (Just (map sNoise sets5))
    sets12 = [ makeSet 1 k 12 20 True | k <- [0 .. 59] ]
    a = bvStats (map sFit sets12) Nothing
    b = bvStats [ fromMaybe (sFit s) (sBag s) | s <- sets12 ] Nothing
    s0b = bvStats [ sFit (makeSet 1 k 0 20 False) | k <- [0 .. 59] ] Nothing
    s0 = makeSet 1 3 0 20 False
    errs = [ let f = polyFit (sXs s0) (sYs s0) d ridgeL in meanOf [ (polyEval f x - y) * (polyEval f x - y) | (x, y) <- zip (sXs s0) (sYs s0) ] | d <- [0 .. 12] ]
    mono = and (zipWith (\prev e -> not (e > prev + 1e-3)) (posInf : errs) errs)
    xs = [0.1, 0.3, 0.45, 0.6, 0.8, 0.95]
    cub x = 2 * x ** 3 - x + 0.5
    fc = polyFit xs (map cub xs) 3 0

-- ---------- やってみよう ----------
-- 1. ← → で次数を 0 から 12 まで動かそう。うすい線の束が「細くずれている」→「太くまとまって合っている」→「暴れる」
-- 2. 右下のグラフで期待誤差（白）がいちばん小さい次数は？ ↑↓ で点の数を増やすと、その次数はどう変わる？
-- 3. 次数 12 で B を押そう。バギングの平均（青緑の点線）はおとなしくなる。次数 6 だと？（バリアンスが増えることも）
-- 4. noiseSd を 0 にすると、ノイズの棒は消える。バイアスとバリアンスは？
-- 5. 03 で「平均してもばらつきが消えないとき」を調べる。バギングの木どうしは似ている（相関がある）

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

-- ---- 連立 1 次方程式 A w = b（部分ピボットつきのガウスの消去法）。ridge = 対角に足す小さな数（解けないのを防ぐ）
solveLin :: [[Double]] -> [Double] -> Double -> [Double]
solveLin a b ridge = [ if abs (r U.! i) < 1e-14 then 0 else (r U.! n) / (r U.! i) | (i, r) <- zip [0 ..] (V.toList final) ]
  where
    n = length b
    m0 = V.fromList [ U.fromList (zipWith (\k v -> if k == i then v + ridge else v) [0 ..] row ++ [bi]) | (i, row, bi) <- zip3 [0 :: Int ..] a b ]
    final = foldl' elim m0 [0 .. n - 1]
    elim m c =
      let p = foldl' (\pp r -> if abs ((m V.! r) U.! c) > abs ((m V.! pp) U.! c) then r else pp) c [c + 1 .. n - 1]
      in if abs ((m V.! p) U.! c) < 1e-14 then m else
         let ms = m V.// [(c, m V.! p), (p, m V.! c)]
             pr = ms V.! c
         in V.imap (\r row -> if r == c then row else
                      let f = (row U.! c) / (pr U.! c)
                      in if f /= 0 then U.imap (\k v -> if k >= c then v - f * (pr U.! k) else v) row else row) ms

-- ---- 多項式の最小 2 乗法。x ∈ [0, 1] を [−1, 1] に直し、チェビシェフ多項式 T0, T1, … を特徴量にする（次数が高くても計算が安定）
chebFeat :: Double -> Int -> [Double]
chebFeat x deg = take (deg + 1) (go 1 t)
  where t = 2 * x - 1
        go a b = a : go b (2 * t * b - a)

data Poly = Poly { pDeg :: !Int, pC :: ![Double] }

-- (ΦᵀΦ + λI) c = Φᵀy
polyFit :: [Double] -> [Double] -> Int -> Double -> Poly
polyFit xs ys deg ridge =
  let d = deg + 1
      fs = map (\x -> U.fromList (chebFeat x deg)) xs
      bv = [ foldl' (\s (f, y) -> s + (f U.! p) * y) 0 (zip fs ys) | p <- [0 .. d - 1] ]
      am = [ [ foldl' (\s f -> s + (f U.! p) * (f U.! q)) 0 fs | q <- [0 .. d - 1] ] | p <- [0 .. d - 1] ]
  in Poly deg (solveLin am bv ridge)

polyEval :: Poly -> Double -> Double
polyEval m x = foldl' (+) 0 (zipWith (*) (pC m) (chebFeat x (pDeg m)))

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
