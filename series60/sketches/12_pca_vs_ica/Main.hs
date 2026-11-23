-- ============================================
-- 60-12 PCA と独立成分分析（ICA）─ 2 つの音が混ざった録音から、元の音を取り出せる？
-- 元の信号 s1, s2（のこぎり波・正弦波など）を行列 A で混ぜた観測 x = A s だけを使う
--   PCA：x を回して「無相関」にするところまで（白色化 10）。でも無相関にする回し方はいくらでもある → 元にはもどらない
--   FastICA：白色化したあと、出力が「いちばん正規分布らしくない」回転を探す（非線形 g = tanh、両方同時に求めて対称に直交化）
--   取り出した信号の「順番」と「符号（上下）」と「大きさ」は決まらない（どれでも x = A s をみたす）
-- 操作：← → 混ぜ方 / 1 のこぎり＋正弦 2 矩形＋正弦 3 のこぎり＋矩形 / R ICA をやり直す（はじめの向きを変える）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8, Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nS :: Int
nS = 600                         -- ★ 時間のサンプル数

data World = World
  { wMix    :: !Double            -- ★ 混ぜ方
  , wPair   :: !Int               -- 1 のこぎり＋正弦 2 矩形＋正弦 3 のこぎり＋矩形
  , wSeed   :: !Int
  , wSrc, wObs, wPca, wIca :: !Mat
  , wIters  :: !Int
  , wCorrP, wCorrI :: [[Double]]
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-12 PCA vs independent component analysis" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sumMat (wIca w) + sumMat (wPca w) + sum (concat (wCorrI w ++ wCorrP w)))
      pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.6 1 21 V.empty V.empty V.empty V.empty 0 [] [] []

-- ★ 元の信号（音のような周期的な波）
makeSources :: Int -> Mat
makeSources k = V.generate nS (\i -> let t = fromIntegral i / fromIntegral nS * 8 in vec [fa t, fb t])
  where
    frac v = v - fromIntegral (floor v :: Int)
    saw t = 2 * frac (t * 1.3) - 1
    sq t = if sin (2 * pi * 0.7 * t) >= 0 then 1 else -1
    sn t = sin (2 * pi * 0.9 * t + 0.3)
    (fa, fb) = case k of
      1 -> (saw, sn)
      2 -> (sq, sn)
      _ -> (saw, sq)

mixMatrix :: Double -> Mat
mixMatrix t = mat [[1, t], [0.4 + 0.5 * t, 1]]

colOf :: Mat -> Int -> [Double]
colOf y k = map (`vix` k) (V.toList y)

absCorrTable :: Mat -> Mat -> [[Double]]
absCorrTable y s = [ [ abs (corrOf (colOf y k) (colOf s j)) | j <- [0, 1] ] | k <- [0, 1] ]

-- 順番の入れかえを許した「元との相関」の小さいほう
bestMatch :: [[Double]] -> Double
bestMatch t = max (min (at' 0 0) (at' 1 1)) (min (at' 0 1) (at' 1 0))
  where at' i j = nth 0 (nth [] t i) j

-- ★ 全部の計算：混ぜる → PCA → 白色化 → FastICA
rebuild :: World -> World
rebuild w = w { wSrc = src, wObs = obs, wPca = pcaOut, wIca = icaOut, wIters = its
              , wCorrP = absCorrTable pcaOut src, wCorrI = absCorrTable icaOut src }
  where
    src = makeSources (wPair w)
    am = mixMatrix (wMix w)
    obs = V.map (matVec am) src
    e = jacobiEigen (covMatrix obs)
    pcaOut = projectData obs (colMeans obs) (eVecs e) 2
    wh = whitenData obs 1e-12
    (wm, its) = withSeed (wSeed w) (fastICA (whZ wh) 300)
    icaOut = V.map (matVec wm) (whZ wh)

-- 波形（はじめの 300 サンプル）
wave :: Mat -> Int -> Float -> Float -> Float -> Float -> Color -> String -> [Picture]
wave y k x y0' w h col title =
  [ Color (rgbC 55 66 80) (lineW 1 (x, y0' + h / 2) (x + w, y0' + h / 2))
  , Color col (polyline 1.5 [ (x + fromIntegral i / 299 * w, y0' + h / 2 - f2 (v / m) * h * 0.45) | (i, v) <- zip [0 :: Int ..] (take 300 vals) ])
  , say title x (y0' + 2) 14 (rgbC 190 198 210) ]
  where vals = colOf y k
        m = foldl' max 1e-12 (map abs vals)

scatter :: Mat -> Float -> Float -> Float -> String -> [Picture]
scatter y x y0' s title =
  [ Color (rgbC 60 72 88) (frameTL 1 x y0' s s) ]
  ++ [ Color (rgbaC 225 232 242 120) (circleAt (x + s / 2 + f2 (vix v 0 / m) * s * 0.46) (y0' + s / 2 - f2 (vix v 1 / m) * s * 0.46) 3) | v <- V.toList y ]
  ++ [ say title x (y0' - 6) 14 (rgbC 205 212 222) ]
  where m = V.foldl' (\a v -> U.foldl' (\b q -> max b (abs q)) a v) 1e-12 y

draw :: Env -> World -> Picture
draw env w = view env $
  concat [ scatter yy x 120 sz (nm ++ " (scatter)")
           ++ wave yy 0 x (160 + sz) cw 70 (rgbC 255 225 140) "1st"
           ++ wave yy 1 x (250 + sz) cw 70 (rgbC 90 165 240) "2nd"
         | (c, nm, yy) <- zip3 [0 :: Int ..] ["Sources s", "Observed x = A s", "PCA output", "ICA output"] [wSrc w, wObs w, wPca w, wIca w]
         , let x = 40 + fromIntegral c * (cw + 24) ]
  ++ [ say ("A = [[" ++ fmtN (mix am 0 0) 2 ++ ", " ++ fmtN (mix am 0 1) 2 ++ "], [" ++ fmtN (mix am 1 0) 2 ++ ", " ++ fmtN (mix am 1 1) 2 ++ "]]") 40 y 15 (grayC 220)
     , say ("Correlation between PCA outputs " ++ fmtN (cr pc) 4 ++ " (uncorrelated)   between ICA outputs " ++ fmtN (cr ic) 4) 40 (y + 26) 15 (grayC 220)
     , say ("|corr| with the sources (best order)   PCA: " ++ fmtN (bestMatch (wCorrP w)) 3) 40 (y + 56) 15 (rgbC 255 125 105)
     , say ("ICA: " ++ fmtN (bestMatch (wCorrI w)) 3 ++ " (FastICA converged in " ++ show (wIters w) ++ " steps)") (40 + 520) (y + 56) 15 (rgbC 150 220 160)
     , sayWrap ("PCA only takes orthogonal axes in order of spread. The mixing axes need not be orthogonal, so the PCA outputs are still mixed. "
                ++ "ICA aims at 'independent' (knowing one tells nothing about the other), so the parallelogram in the scatter lines up with the axes.")
               40 (y + 72) (envW env - 80) 14 (rgbC 165 175 190)
     , say "PCA vs independent component analysis" 40 44 22 (grayC 240)
     , say ("<- -> mixing (" ++ fmtN (wMix w) 2 ++ ")   1 saw+sine   2 square+sine   3 saw+square   R redo ICA") 40 72 15 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    cw = (envW env - 80 - 3 * 24) / 4
    sz = min cw (envH env * 0.3)
    y = 350 + sz
    am = mixMatrix (wMix w)
    pc = covMatrix (wPca w)
    ic = covMatrix (wIca w)
    cr c = mix c 0 1 / sqrt (mix c 0 0 * mix c 1 1)

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey k) Down _ _
    | k == KeyLeft || k == KeyRight ->
        pure (rebuild w { wMix = clampv (jsRound ((wMix w + (if k == KeyRight then 0.1 else -0.1)) * 10) / 10) (-0.9) 0.9 })
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (rebuild w { wPair = fromEnum c - fromEnum '0' })
    | c `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("whitened observations have covariance I (3 pairs x 3 mixings, 1e-9)", all (\(a, _, _, _, _) -> a) rs)
  , ("FastICA recovers the sources (|corr| >= 0.98 up to order, all 9 cases)", all (\(_, a, _, _, _) -> a) rs)
  , ("FastICA unmixing matrix W is orthogonal (W W^T = I, 1e-9)", all (\(_, _, a, _, _) -> a) rs)
  , ("PCA outputs are uncorrelated (off-diagonal covariance ~ 0)", all (\(_, _, _, a, _) -> a) rs)
  , ("but PCA does not recover the sources in some case (worst |corr| < 0.95)", foldl' min 1 (map (\(_, _, _, _, p) -> p) rs) < 0.95)
  , ("the two sources are almost uncorrelated (|corr| < 0.1): unmixed, PCA and ICA keep them", abs (mix cs 0 1 / sqrt (mix cs 0 0 * mix cs 1 1)) < 0.1) ]
  where
    rs = [ trial k t | k <- [1 .. 3], t <- [-0.6, 0.2, 0.7] ]
    trial k t =
      let s = makeSources k
          x = V.map (matVec (mixMatrix t)) s
          wh = whitenData x 1e-12
          (wm, _) = withSeed (200 + k) (fastICA (whZ wh) 300)
          y = V.map (matVec wm) (whZ wh)
          e = jacobiEigen (covMatrix x)
          p = projectData x (colMeans x) (eVecs e) 2
          cp = covMatrix p
      in ( maxAbsDiff (covMatrix (whZ wh)) (eyeMat 2) <= 1e-9
         , bestMatch (absCorrTable y s) >= 0.98
         , maxAbsDiff (matMul wm (matT wm)) (eyeMat 2) <= 1e-9
         , abs (mix cp 0 1) <= 1e-9 * mix cp 0 0
         , bestMatch (absCorrTable p s) )
    cs = covMatrix (makeSources 1)

-- ---------- やってみよう ----------
-- 1. ← → で混ぜ方を変えよう。PCA の出力が元にもどるのはどんな混ぜ方のとき？
-- 2. ICA の出力の順番や上下は、R を押すたびに変わることがある。なぜ決まらない？
-- 3. 散布図の形に注目：元の信号（四角っぽい）→ 観測（平行四辺形）→ PCA（回った平行四辺形）→ ICA（四角）
-- 4. 2 つとも正規分布の雑音だったら ICA でも分けられない。なぜ？（どの回転でも同じ丸い雲）
-- 5. 3 つの信号を 3 つのマイクで録音したら？ fastICA は何次元でも同じ式で動く

-- ==================================================
-- ここから下は「主成分分析の道具」（中身は読まなくても OK。消さないでね）
-- 約束：データ X は「n 行 × d 列」（1 行 = 1 個のデータ）。共分散は n − 1 で割る（C = Xcᵀ Xc / (n − 1)）
-- ベクトル Vec = Double の並び（unboxed）、行列 Mat = Vec（行）の並び。計算は Double、描くときだけ Float にする
-- ==================================================
-- ---- 数の表示と安全のための道具
clampv :: Double -> Double -> Double -> Double      -- clampv v a b：v を a〜b に（p5.js 版と同じ順）
clampv v a b = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double                   -- NaN・∞ を fb に
fin v fb = if isNaN v || isInfinite v then fb else v

f2 :: Double -> Float                               -- 描くときの Float（NaN は 0）
f2 v = realToFrac (fin v 0)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 数を小数 d 桁で（NaN は「?」、"-0.00" は "0.00"）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v || isInfinite v = "?"
  | otherwise = let s = showFFloat (Just d) v ""
                in if take 1 s == "-" && all (`elem` "0.") (drop 1 s) then drop 1 s else s

fmtVec :: Vec -> Int -> String
fmtVec v d = "(" ++ joinWith ", " (map (`fmtN` d) (U.toList v)) ++ ")"

-- 指数の形で小数 d 桁（toExponential）
fmtE :: Double -> Int -> String
fmtE v d = if isNaN v || isInfinite v then "?" else showEFloat (Just d) v ""

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin v0 0

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Integer)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- リストの k 番目（なければ d）
nth :: a -> [a] -> Int -> a
nth d xs k = if k < 0 then d else case drop k xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Int -> Rng                               -- (s >>> 0) || 1
seedRng s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- seedRng(s) してから計算し、結果だけ取り出す
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double                                     -- 0〜1 の一様乱数
rnd = R rndStep

rndRange :: Double -> Double -> R Double            -- a〜b の一様乱数
rndRange a b = do u <- rnd; pure (a + (b - a) * u)

gaussRnd :: R Double                                -- 標準正規分布（ボックス-ミュラー法。u, v の順に 2 つ引く）
gaussRnd = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- 行列の道具。データ X は「n 行 × d 列」（1 行 = 1 個のデータ）
type Vec = U.Vector Double
type Mat = V.Vector Vec

vec :: [Double] -> Vec
vec = U.fromList

mat :: [[Double]] -> Mat
mat = V.fromList . map U.fromList

vix :: Vec -> Int -> Double                         -- 成分（範囲の外は 0）
vix v i = maybe 0 id (v U.!? i)

mrow :: Mat -> Int -> Vec                           -- 行（範囲の外は空）
mrow m i = maybe U.empty id (m V.!? i)

mix :: Mat -> Int -> Int -> Double                  -- (i, j) 成分
mix m i j = vix (mrow m i) j

nRows, nCols :: Mat -> Int
nRows = V.length
nCols m = U.length (mrow m 0)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = V.generate n (\i -> U.generate n (\j -> if i == j then 1 else 0))

matT :: Mat -> Mat
matT a = V.generate (nCols a) (\j -> U.generate (nRows a) (\i -> mix a i j))

matMul :: Mat -> Mat -> Mat
matMul a b = V.map row a
  where m = nCols b
        row r = U.ifoldl' (\acc k x -> if x == 0 then acc else U.zipWith (\c bk -> c + x * bk) acc (mrow b k)) (U.replicate m 0) r

matVec :: Mat -> Vec -> Vec
matVec a x = V.convert (V.map (`dotv` x) a)

dotv :: Vec -> Vec -> Double                       -- 内積（左から順に足す）
dotv a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

vlen :: Vec -> Double
vlen a = sqrt (dotv a a)

unitv :: Vec -> Vec                                 -- 長さ 1 に（0 ベクトルは (1, 0, …)）
unitv a = let l = vlen a in if l > 1e-300 then U.map (/ l) a else U.imap (\i _ -> if i == 0 then 1 else 0) a

scaleV :: Double -> Vec -> Vec
scaleV k = U.map (k *)

addV, subV :: Vec -> Vec -> Vec
addV = U.zipWith (+)
subV = U.zipWith (-)

maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = V.foldl' max 0 (V.zipWith (\r s -> U.foldl' max 0 (U.zipWith (\x y -> abs (x - y)) r s)) a b)

colMeans :: Mat -> Vec                              -- 平均ベクトル（列ごとの平均）
colMeans x = U.map (/ fromIntegral (max 1 (nRows x))) (V.foldl' addV (U.replicate (nCols x) 0) x)

centerCols :: Mat -> Vec -> Mat                     -- 平均を引く（中心化）
centerCols x mu = V.map (`subV` mu) x

-- ★ 共分散行列 C = Xcᵀ Xc / (n − 1)（n − 1 で割る「不偏」の約束。点が 1 個以下ならゼロ行列）
covMatrix :: Mat -> Mat
covMatrix x
  | n < 2 = zerosMat d d
  | otherwise = toMat d $ runST $ do
      c <- UM.replicate (d * d) 0
      V.forM_ x $ \r -> forM_ [0 .. d - 1] $ \i -> do
        let a = vix r i - vix mu i
        forM_ [i .. d - 1] $ \j -> UM.modify c (+ a * (vix r j - vix mu j)) (i * d + j)
      forM_ [0 .. d - 1] $ \i -> forM_ [i .. d - 1] $ \j -> do
        v <- UM.read c (i * d + j)
        let v' = v / fromIntegral (n - 1)
        UM.write c (i * d + j) v'
        UM.write c (j * d + i) v'
      U.freeze c
  where n = nRows x
        d = nCols x
        mu = colMeans x

-- 長さ d·d の並びを d×d の行列に
toMat :: Int -> Vec -> Mat
toMat d flat = V.generate (if d > 0 then U.length flat `div` d else 0) (\i -> U.slice (i * d) d flat)

-- 固有値分解の結果：vals = 固有値（大きい順）、vecs の k 行目 = k 番目の固有ベクトル
data Eig = Eig { eVals :: !Vec, eVecs :: !Mat }

-- ★ ヤコビ法（対称行列の固有値分解）：非対角を 1 つずつ回転で 0 にしていく。d ≤ 64 くらいまで
-- 固有ベクトルは長さ 1、いちばん大きい成分を正に
jacobiEigen :: Mat -> Eig
jacobiEigen = jacobiEigenN 80

jacobiEigenN :: Int -> Mat -> Eig
jacobiEigenN maxSweeps c = Eig (U.fromList (map (\k -> vix diag k) idx)) (V.fromList (map vecOf idx))
  where
    d = nRows c
    (af, vf) = runST $ do
      a <- U.thaw (U.concat (V.toList (V.map (\r -> U.generate d (vix r)) c)))
      v <- U.thaw (U.generate (d * d) (\k -> if k `div` d == k `mod` d then 1 else 0))
      let rd i j = UM.unsafeRead a (i * d + j)
          wr i j x = UM.unsafeWrite a (i * d + j) x
          sweep sw = when (sw < maxSweeps) $ do
            (off, tot) <- foldM (\(!o, !t) k -> do
                                   x <- UM.unsafeRead a k
                                   let s = x * x
                                   pure (if k `div` d /= k `mod` d then o + s else o, t + s)) (0, 0) [0 .. d * d - 1]
            when (off > 1e-30 * tot) $ do
              forM_ [0 .. d - 2] $ \p -> forM_ [p + 1 .. d - 1] $ \q -> do
                apq <- rd p q
                when (abs apq >= 1e-300) $ do
                  app <- rd p p
                  aqq <- rd q q
                  let th = (aqq - app) / (2 * apq)
                      t = if th == 0 then 1 else signum th / (abs th + sqrt (th * th + 1))
                      cs = 1 / sqrt (t * t + 1)
                      sn = t * cs
                  forM_ [0 .. d - 1] $ \k -> do
                    akp <- rd k p
                    akq <- rd k q
                    wr k p (cs * akp - sn * akq)
                    wr k q (sn * akp + cs * akq)
                  forM_ [0 .. d - 1] $ \k -> do
                    apk <- rd p k
                    aqk <- rd q k
                    wr p k (cs * apk - sn * aqk)
                    wr q k (sn * apk + cs * aqk)
                  forM_ [0 .. d - 1] $ \k -> do
                    vkp <- UM.unsafeRead v (k * d + p)
                    vkq <- UM.unsafeRead v (k * d + q)
                    UM.unsafeWrite v (k * d + p) (cs * vkp - sn * vkq)
                    UM.unsafeWrite v (k * d + q) (sn * vkp + cs * vkq)
              sweep (sw + 1)
      sweep 0
      af' <- U.freeze a
      vf' <- U.freeze v
      pure (af', vf')
    diag = U.generate d (\i -> vix af (i * d + i))
    idx = sortBy (\x y -> compare (vix diag y) (vix diag x)) [0 .. d - 1]
    vecOf k = fixSign (U.generate d (\i -> vix vf (i * d + k)))

-- いちばん大きい成分（同じくらいなら前のほう）を正にそろえる
fixSign :: Vec -> Vec
fixSign v = let big = U.ifoldl' (\b i x -> if abs x > abs (vix v b) + 1e-12 then i else b) 0 v
            in if vix v big < 0 then U.map negate v else v

traceOf :: Mat -> Double
traceOf c = sum [ mix c i i | i <- [0 .. nRows c - 1] ]

-- C V = V Λ の残り（いちばん大きいずれ）
eigResidual :: Mat -> Eig -> Double
eigResidual c eg = foldl' max 0 [ abs (vix cv i - vix (eVals eg) k * vix v i)
                                | (k, v) <- zip [0 ..] (V.toList (eVecs eg)), let cv = matVec c v, i <- [0 .. U.length v - 1] ]

-- 固有ベクトルどうしの直交のずれ
orthoError :: Mat -> Double
orthoError vs = foldl' max 0 [ abs (dotv a b - (if i == j then 1 else 0)) | (i, a) <- zip [0 :: Int ..] l, (j, b) <- zip [0 ..] l ]
  where l = V.toList vs

-- 射影（主成分得点）：score_k = (x − μ)·v_k（k 個まで）
projectData :: Mat -> Vec -> Mat -> Int -> Mat
projectData x mu vecs k = V.map (\r -> V.convert (V.map (\v -> U.sum (U.zipWith3 (\a m b -> (a - m) * b) r mu v)) (V.take k vecs))) x

-- 復元：x_hat = μ + Σ score_k v_k
reconstructRow :: Vec -> Vec -> Mat -> Vec
reconstructRow score mu vecs = U.ifoldl' (\x k s -> U.zipWith (\a b -> a + s * b) x (mrow vecs k)) mu score

-- 向きのちがい（符号は無視、0〜90°）。atan2(‖v − (v·u)u‖, |v·u|)
angleBetween :: Vec -> Vec -> Double
angleBetween u v = let a = unitv u
                       b = unitv v
                       c = dotv a b
                       r = U.sum (U.zipWith (\ai bi -> (bi - c * ai) ^ (2 :: Int)) a b)
                   in atan2 (sqrt r) (abs c) * 180 / pi

-- 相関係数
corrOf :: [Double] -> [Double] -> Double
corrOf a b = if saa > 0 && sbb > 0 then sab / sqrt (saa * sbb) else 0
  where n = fromIntegral (length a)
        ma = foldl' (+) 0 a / n
        mb = foldl' (+) 0 b / n
        (sab, saa, sbb) = foldl' (\(!p, !q, !r) (x, y) -> (p + (x - ma) * (y - mb), q + (x - ma) ^ (2 :: Int), r + (y - mb) ^ (2 :: Int))) (0, 0, 0) (zip a b)

-- 計算を全部すませる（起動や作り直しのあと、最初のコマが重くならないように）
sumMat :: Mat -> Double
sumMat = V.foldl' (\s r -> s + U.sum r) 0
-- ---- 2×2 対称行列 [[a, b], [b, c]] の固有値・固有ベクトル（解の公式）
data E2 = E2 { e2L1, e2L2 :: !Double, e2V1, e2V2 :: !Vec, e2Angle :: !Double }

eig2sym :: Double -> Double -> Double -> E2
eig2sym a b c = E2 l1 l2 v1 (vec [-(vix v1 1), vix v1 0]) (atan2 (vix v1 1) (vix v1 0) * 180 / pi)
  where
    tr = a + c
    h = sqrt (((a - c) / 2) ^ (2 :: Int) + b * b)
    l1 = tr / 2 + h
    l2 = tr / 2 - h
    v0 = if abs b > 1e-14 * (abs a + abs c + 1e-300) then unitv (vec [l1 - c, b]) else if a >= c then vec [1, 0] else vec [0, 1]
    v1 = if vix v0 0 < 0 || (vix v0 0 == 0 && vix v0 1 < 0) then U.map negate v0 else v0

-- 2×2 の行列 C の固有分解
eig2of :: Mat -> E2
eig2of c = eig2sym (mix c 0 0) (mix c 0 1) (mix c 1 1)
-- ---- グラフと図の道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 80 92 108) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 160 170 185) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 160 170 185) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 212 222), say yl (pX g) (pY g - 8) 14 (rgbC 205 212 222) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 共分散 C の「k σ の楕円」の点（データの単位）。中心 μ
covEllipsePts :: Vec -> Mat -> Double -> Int -> [(Double, Double)]
covEllipsePts mu c k segs =
  [ (vix mu 0 + u * vix (e2V1 e) 0 + w * vix (e2V2 e) 0, vix mu 1 + u * vix (e2V1 e) 1 + w * vix (e2V2 e) 1)
  | i <- [0 .. segs], let t = 2 * pi * fromIntegral i / fromIntegral segs, let u = a * cos t, let w = b * sin t ]
  where e = eig2of c
        a = k * sqrt (max 0 (e2L1 e))
        b = k * sqrt (max 0 (e2L2 e))

-- 矢印（太さ wt、矢じりの大きさ hs）
arrow2 :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
arrow2 wt hs (xa, ya) (xb, yb) = Pictures [thickLine wt (xa, ya) (xb, yb), Polygon [(xb, yb), (xb - hs * cos (a - 0.4), yb - hs * sin (a - 0.4)), (xb - hs * cos (a + 0.4), yb - hs * sin (a + 0.4))]]
  where a = atan2 (yb - ya) (xb - xa)
-- ---- 標準化と白色化
-- 各列を平均 0・標準偏差 1 に（標準偏差 0 の列は 0 のまま）。戻り値 (Z, μ, sd)
standardizeCols :: Mat -> (Mat, Vec, Vec)
standardizeCols x = (V.map (\r -> U.imap (\j v -> if vix sd j > 1e-12 then (v - vix mu j) / vix sd j else 0) r) x, mu, sd)
  where mu = colMeans x
        c = covMatrix x
        sd = U.generate (nRows c) (\i -> sqrt (max 0 (mix c i i)))

data White = White { whZ :: !Mat, whW :: !Mat, whMu :: !Vec, whEg :: !Eig }

-- ★ 白色化（PCA 白色化）：z = Λ^(−1/2) Vᵀ (x − μ)。共分散が単位行列 I になる（eps で 0 割りを防ぐ）
whitenData :: Mat -> Double -> White
whitenData x e = White (V.map (\r -> matVec wm (subV r mu)) x) wm mu eg
  where mu = colMeans x
        eg = jacobiEigen (covMatrix x)
        wm = V.imap (\k v -> U.map (/ sqrt (max (vix (eVals eg) k) 0 + e)) v) (eVecs eg)
-- ---- FastICA（非線形 g = tanh、全部の成分を同時に求めて「対称な直交化」W ← (W Wᵀ)^(−1/2) W）
symDecorrelate :: Mat -> Mat
symDecorrelate wm = matMul mh wm
  where
    eg = jacobiEigen (matMul wm (matT wm))
    m = nRows wm
    mh = V.generate m (\i -> U.generate m (\j ->
           foldl' (\acc k -> let e = mrow (eVecs eg) k
                                 s = 1 / sqrt (max (vix (eVals eg) k) 1e-15)
                             in acc + s * vix e i * vix e j) 0 [0 .. m - 1]))

-- Z は白色化したデータ（n 行 × m 列）。戻り値 (W（行 = 分離の向き）, 反復回数)
fastICA :: Mat -> Int -> R (Mat, Int)
fastICA z iters = do
  w0 <- V.replicateM m (U.replicateM m gaussRnd)
  pure (loop 0 (symDecorrelate w0))
  where
    n = fromIntegral (nRows z)
    m = nCols z
    newW w = let ys = V.convert (V.map (\r -> tanh (dotv w r)) z) :: Vec
                 gp = U.foldl' (\s y -> s + (1 - y * y)) 0 ys
                 acc j = V.ifoldl' (\s i r -> s + vix r j * vix ys i) 0 z
             in U.generate m (\j -> acc j / n - (gp / n) * vix w j)
    loop it wm
      | it >= iters = (wm, it)
      | otherwise =
          let wn = symDecorrelate (V.map newW wm)
              lim = foldl' max 0 [ abs (abs (dotv (mrow wn i) (mrow wm i)) - 1) | i <- [0 .. m - 1] ]
          in if lim < 1e-12 then (wn, it) else loop (it + 1) wn
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
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 破線（線 dash、すき間 gap）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine wt dash gap (xa, ya) (xb, yb) = Pictures [ thickLine wt (at' t0) (at' (min l (t0 + dash))) | t0 <- takeWhile (< l) [0, dash + gap ..] ]
  where l = sqrt ((xb - xa) ^ (2 :: Int) + (yb - ya) ^ (2 :: Int))
        at' t = if l <= 0 then (xa, ya) else (xa + (xb - xa) * t / l, ya + (yb - ya) * t / l)

-- 幅 w × 高さ h の画素の絵を k 倍にして、左上が (x, y) になるように置く（f x y は左上が (0, 0)）
imageTL :: Float -> Float -> Int -> Int -> Float -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
imageTL x y w h k f = Translate (x + fromIntegral w * k / 2) (y + fromIntegral h * k / 2) (Scale 1 (-1) (pixelImage w h k f))

-- 0〜255 に丸めた画素の値
byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round (fin v 0) :: Int)))
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any (\v -> isNaN v || isInfinite v)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs && not (null rs) then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

selfTestOk :: [(String, Bool)] -> Bool
selfTestOk rs = not (null rs) && all snd rs

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if selfTestOk rs then rgbC 120 210 170 else rgbC 255 90 90)
