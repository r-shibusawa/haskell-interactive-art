-- ============================================
-- 60-11 カーネル PCA ─ まっすぐな軸では分けられないデータを、「似ている度」の行列で PCA する
-- 同心円（内側と外側）は、どの直線に射影しても混ざってしまう → ふつうの PCA では分かれない
--   RBF カーネル k(x, y) = exp(−γ ‖x − y‖²)（近いほど 1、遠いほど 0）で n×n のグラム行列 K を作り、
--   特徴空間で中心化 Kc = K − 1K − K1 + 1K1 してから固有ベクトル α を求める（部分空間反復法）。第 k 成分の得点 = √λ_k α_k
--   γ がちょうどよいと、第 1 成分だけで内側と外側が分かれる。背景の色 = 平面の各点の第 1 成分の値
--   200×200 の計算は少し重いので、キーを押したあとは裏で計算し、できたら入れかえる（そのあいだは computing...）
-- 操作：← → γ（カーネルの幅）/ 1 同心円 2 三日月 / R 新しいデータ / スペース 背景の表示
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

nPts :: Int
nPts = 200                       -- ★ 点の数（グラム行列は nPts×nPts）

bgW, bgH :: Int
bgW = 70                         -- 背景の絵の画素数（横・縦）
bgH = 56

-- カーネル PCA の結果と、そこから作るもの
data KRes = KRes
  { krKp     :: !KPca
  , krRowM   :: !Vec              -- K の各行の平均（新しい点の得点に使う）
  , krAccK   :: !Double           -- 第 1 成分の符号で分けた正解率
  , krBg     :: Picture }         -- 背景（平面の各点の第 1 成分の値）

data World = World
  { wGam     :: !Double           -- ★ γ
  , wKind    :: !Int              -- 1 同心円 2 三日月
  , wSeed    :: !Int
  , wPts     :: !Mat
  , wLabs    :: [Int]
  , wLinS    :: [Double]          -- ふつうの PCA の第 1 成分
  , wAccL    :: !Double
  , wRes     :: KRes
  , wRng     :: Rng               -- 乱数の続き（p5.js 版と同じく、γ を変えたときは続きから引く）
  , wShowBg  :: !Bool
  , wPending :: Maybe (Later (KRes, Mat))
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-11 Kernel PCA" world0)
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuildNow world0 { wTest = selfTest }
      _ <- evaluate (forceRes (wRes w, wPts w))
      pure w
  , skStep  = \_ _ w -> step w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 5 1 11 V.empty [] [] 0.5 (KRes (KPca U.empty V.empty V.empty) U.empty 0 Blank) (seedRng 1) True Nothing []

-- データを作る（種から）。戻り値 = データ、ふつうの PCA の第 1 成分、乱数の続き
makeData :: Int -> Int -> ((Mat, [Int], [Double]), Rng)
makeData kind seed = runR (do
  d <- if kind == 1 then makeCircles nPts 0.05 else makeMoons nPts 0.06
  let p = V.fromList (map fst d)
      lin = jacobiEigen (covMatrix p)
      ls = map (`vix` 0) (V.toList (projectData p (colMeans p) (eVecs lin) 1))
  pure (p, map snd d, ls)) (seedRng seed)

-- ★ γ を変えたら計算し直す（毎フレームではなく）。乱数は rng から引き、続きも返す
recompute :: Mat -> [Int] -> Double -> Rng -> (KRes, Rng)
recompute p labs gam rng = (KRes kp rowM (signAccuracy (U.toList (mrow (kpComps kp) 0)) labs) (makeBg p gam kp rowM), rng')
  where
    (kp, rng') = runR (kernelPCA p gam 2) rng
    rowM = V.convert (V.map (\r -> U.sum r / fromIntegral (U.length r)) (rbfGram p gam))

forceRes :: (KRes, Mat) -> Double
forceRes (r, p) = U.sum (kpVals (krKp r)) + sumMat (kpComps (krKp r)) + U.sum (krRowM r) + krAccK r + sumMat p
                  + (case krBg r of Blank -> 0; _ -> 1)

-- 新しい点 x の第 1 成分 = Σ_i α_i k~(x, x_i) / √λ（k~ = 中心化したカーネル）
kpcaScore :: Mat -> Double -> KPca -> Vec -> (Double, Double) -> Int -> Double
kpcaScore p gam kp rowM (x, y) comp = s / sq
  where
    n = fromIntegral (nRows p)
    kx = V.convert (V.map (\q -> exp (-gam * ((vix q 0 - x) ^ (2 :: Int) + (vix q 1 - y) ^ (2 :: Int)))) p) :: Vec
    mx = U.sum kx / n
    sq = sqrt (max (vix (kpVals kp) comp) 1e-300)
    al = mrow (kpComps kp) comp
    -- k~ = k − (x の行の平均) − (x_i の行の平均) + 全体の平均（最後の項は Σα = 0 で消える）
    s = U.ifoldl' (\acc i kxi -> acc + (vix al i / sq) * (kxi - mx - vix rowM i)) 0 kx

-- 背景の絵（青 = 負、赤 = 正）
makeBg :: Mat -> Double -> KPca -> Vec -> Picture
makeBg p gam kp rowM = pixelImage bgW bgH 1 (\i j -> col (vix vals (i + bgW * j)))
  where
    vals = U.generate (bgW * bgH) (\k -> let i = k `mod` bgW
                                             j = k `div` bgW
                                         in kpcaScore p gam kp rowM (-1.6 + 3.2 * fromIntegral i / fromIntegral (bgW - 1), 1.3 - 2.6 * fromIntegral j / fromIntegral (bgH - 1)) 0)
    m = U.foldl' (\a v -> max a (abs v)) 1e-9 vals
    col v = let t = clampv (v / m) (-1) 1
            in (byte (if t > 0 then 40 + 60 * t else 40), byte (50 + 30 * abs t), byte (if t < 0 then 60 - 70 * t else 60))

-- データを作り直して、すぐに全部計算する（起動のとき）
rebuildNow :: World -> World
rebuildNow w = w { wPts = p, wLabs = labs, wLinS = ls, wAccL = splitAccuracy ls labs, wRes = res, wRng = rng2 }
  where ((p, labs, ls), rng1) = makeData (wKind w) (wSeed w)
        (res, rng2) = recompute p labs (wGam w) rng1

-- 裏で計算を始める（前の計算は止める）
startJob :: World -> Mat -> [Int] -> Rng -> IO World
startJob w p labs rng = do
  mapM_ cancelLater (wPending w)
  let (res, rng') = recompute p labs (wGam w) rng
  job <- startLater forceRes (res, p)
  pure w { wPending = Just job, wRng = rng' }

step :: World -> IO World
step w = case wPending w of
  Nothing -> pure w
  Just job -> do
    r <- pollLater job
    pure (case r of
            Just (res, _) -> w { wRes = res, wPending = Nothing }
            Nothing -> w)

-- 1 次元に並べて、ラベルの色で
strip :: [Int] -> [Double] -> Float -> Float -> Float -> String -> Double -> [Picture]
strip labs vals x y w title acc =
  [ Color (rgbC 70 82 98) (lineW 1 (x, y) (x + w, y)), Color (rgbC 70 82 98) (lineW 1 (x + w / 2, y - 30) (x + w / 2, y + 30)) ]
  ++ [ Color (if l == 1 then rgbaC 255 125 105 200 else rgbaC 90 165 240 200)
             (circleAt (x + w / 2 + f2 (v / m) * (w / 2) * 0.95) (y + (if l == 1 then 10 else -10) + fromIntegral ((i * 7) `mod` 9) - 4) 6)
     | (i, v, l) <- zip3 [0 :: Int ..] vals labs ]
  ++ [ say (title ++ "  split " ++ fmtN (100 * acc) 1 ++ "%") x (y - 40) 14 (rgbC 205 212 222) ]
  where m = foldl' max 1e-12 (map abs vals)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate cx cy (Scale (3.2 * s / fromIntegral bgW) (-2.6 * s / fromIntegral bgH) (krBg res)) | wShowBg w ]
  ++ [ Color (rgbC 60 72 88) (frameTL 1 (cx - 1.6 * s) (cy - 1.3 * s) (3.2 * s) (2.6 * s)) ]
  ++ [ Color (labCol l) (circleAt (cx + f2 (vix p 0) * s) (cy - f2 (vix p 1) * s) 6) | (p, l) <- zip (V.toList (wPts w)) (wLabs w) ]
  ++ [ say "Data (colour = true group, background = kernel PC1 value)" (cx - 1.6 * s) (cy - 1.3 * s - 10) 14 (rgbC 205 212 222) ]
  -- ---- 1 次元に射影して比べる
  ++ strip (wLabs w) (wLinS w) x 170 wd "Ordinary PCA, PC1 (best threshold)" (wAccL w)
  ++ strip (wLabs w) comp0 x 290 wd "Kernel PCA, PC1 (split at 0)" (krAccK res)
  -- ---- カーネル PCA の第 1・第 2 成分
  ++ [ Color (rgbC 60 72 88) (frameTL 1 x gy gw gh) ]
  ++ [ Color (labCol l) (circleAt (x + gw / 2 + f2 (a / m1) * gw * 0.46) (gy + gh / 2 - f2 (b / m2) * gh * 0.46) 5) | (a, b, l) <- zip3 comp0 comp1 (wLabs w) ]
  ++ [ say "Kernel PCA scores (x PC1, y PC2)" x (gy - 8) 14 (rgbC 205 212 222)
     , say ("gamma = " ++ fmtN (wGam w) 2) tx (gy + 20) 15 (grayC 220)
     , say ("l1 = " ++ fmtN (vix (kpVals kp) 0) 3 ++ "  l2 = " ++ fmtN (vix (kpVals kp) 1) 3) tx (gy + 46) 15 (grayC 220)
     , say ("Gram matrix " ++ show nPts ++ "x" ++ show nPts) tx (gy + 72) 15 (grayC 220)
     , say (if krAccK res >= 0.95 then "Split by PC1!" else "Not split yet") tx (gy + 104) 15 (if krAccK res >= 0.95 then rgbC 150 220 160 else rgbC 255 225 140)
     , sayWrap "Too small gamma: everything is 'similar'. Too large: nothing but itself is similar. Neither splits." tx (gy + 120) (envW env - tx - 30) 14 (rgbC 165 175 190)
     , say "Kernel PCA" 40 44 22 (grayC 240)
     , say ("<- -> gamma   1 circles 2 moons (now " ++ (if wKind w == 1 then "circles" else "moons") ++ ")   R new data   Space background") 40 72 15 (rgbC 165 175 190) ]
  ++ [ say "computing..." (x + wd - 130) 72 16 (rgbC 255 225 140) | maybe False (const True) (wPending w) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    res = wRes w
    kp = krKp res
    s = min (envW env * 0.12) (envH env * 0.2)
    cx = 40 + 1.6 * s
    cy = 140 + 1.3 * s
    x = envW env * 0.5
    wd = envW env * 0.44
    gy = 370
    gw = wd * 0.5
    gh = min (wd * 0.5) (envH env - 430)
    tx = x + gw + 30
    comp0 = U.toList (mrow (kpComps kp) 0)
    comp1 = U.toList (mrow (kpComps kp) 1)
    m1 = foldl' max 1e-12 (map abs comp0)
    m2 = foldl' max 1e-12 (map abs comp1)
    labCol l = if l == 1 then rgbC 255 125 105 else rgbC 90 165 240

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey k) Down _ _
    | k == KeyLeft || k == KeyRight ->
        let w1 = w { wGam = clampv (wGam w * (if k == KeyRight then 1.4 else 1 / 1.4)) 0.1 200 }
        in startJob w1 (wPts w1) (wLabs w1) (wRng w1)
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> newData w { wKind = if c == '1' then 1 else 2 }
    | c `elem` "rR" -> newData w { wSeed = wSeed w + 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wShowBg = not (wShowBg w) }
  _ -> pure w
  where
    newData w1 = do
      let ((p, labs, ls), rng1) = makeData (wKind w1) (wSeed w1)
      startJob w1 { wPts = p, wLabs = labs, wLinS = ls, wAccL = splitAccuracy ls labs } p labs rng1

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("RBF Gram matrix: diagonal = 1, entries in (0, 1], symmetric",
      and [ near (vix r i) 1 1e-15 && U.all (\v -> v > 0 && v <= 1) r && and [ vix r j == mix k j i | j <- [0 .. nPts - 1] ] | (i, r) <- zip [0 ..] (V.toList k) ])
  , ("centered Gram matrix: every row sum is 0 (1e-12)", V.all (\r -> abs (U.sum r) < 1e-12) kc)
  , ("Kc a = lambda a (components 1, 2, 1e-8 x lambda1)",
      and [ U.and (U.zipWith (\v ai -> abs (v - vix (kpVals r) c * ai) < 1e-8 * vix (kpVals r) 0) (matVec (kpKc r) a) a)
          | c <- [0, 1], let a = unitv (mrow (kpComps r) c) ])
  , ("new-point formula on training points = score sqrt(lambda) alpha",
      and [ near (kpcaScore pp 5 r rowM (vix q 0, vix q 1) 0) (vix (mrow (kpComps r) 0) i) 1e-8 | (i, q) <- zip [0 .. 19] (V.toList pp) ])
  , ("circles (gamma = 5): sign of kernel PC1 splits >= 95%", signAccuracy (U.toList (mrow (kpComps r) 0)) labs >= 0.95)
  , ("circles: ordinary PC1 splits < 80% even with the best threshold", splitAccuracy ls labs < 0.8) ]
  where
    (d, r) = withSeed 11 (do d0 <- makeCircles nPts 0.05
                             r0 <- kernelPCA (V.fromList (map fst d0)) 5 2
                             pure (d0, r0))
    pp = V.fromList (map fst d)
    labs = map snd d
    k = rbfGram pp 5
    kc = centerGram k
    rowM = V.convert (V.map (\row -> U.sum row / fromIntegral (U.length row)) k)
    lin = jacobiEigen (covMatrix pp)
    ls = map (`vix` 0) (V.toList (projectData pp (colMeans pp) (eVecs lin) 1))

-- ---------- やってみよう ----------
-- 1. ← → で γ を変えて、第 1 成分で分かれる範囲を探そう。小さすぎ・大きすぎると何が起きる？
-- 2. 背景の色の境目は、どんな形？（円になっていれば成功）
-- 3. 2 で三日月にすると、第 1 成分だけでは分かれにくい。第 2 成分も見ると？
-- 4. カーネルを k(x, y) = (x·y)² にすると、同心円は分かれる？（rbfGram を書きかえて試そう）
-- 5. 点の数を 2 倍にすると、計算時間はどれくらい増える？（グラム行列は n×n）

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
-- ---- 部分空間反復法とグラム行列（大きいほうから k 個の固有ベクトルだけ）
-- グラム-シュミット（修正版）。長さがほぼ 0 になったら乱数で作り直す
orthonormalize :: [Vec] -> R [Vec]
orthonormalize = go []
  where
    go done [] = pure (reverse done)
    go done (q : qs) = do
      let q' = foldl' (\acc b -> let p = dotv acc b in U.zipWith (\x y -> x - p * y) acc b) q (reverse done)
          l = vlen q'
      if l < 1e-12
        then do fresh <- U.replicateM (U.length q) gaussRnd
                go done (fresh : qs)
        else go (U.map (/ l) q' : done) qs

-- ★ 部分空間反復法：べき乗法を k 本まとめて回し、毎回直交化。applyM v = M v（M を作らなくてよい）
-- 最後に k×k の小さい行列をヤコビ法で（レイリー-リッツ）
subspaceEigen :: (Vec -> Vec) -> Int -> Int -> Int -> Double -> R Eig
subspaceEigen applyM dim k iters tol = do
  q0 <- orthonormalize =<< replicateM k (U.replicateM dim gaussRnd)
  q <- loop 0 q0
  let mq = map applyM q
      t0 = [ [ dotv qa mb | mb <- mq ] | qa <- q ]
      t = [ [ if b < a || b > a then (nth 0 (nth [] t0 a) b + nth 0 (nth [] t0 b) a) / 2 else nth 0 (nth [] t0 a) b
            | b <- [0 .. k - 1] ] | a <- [0 .. k - 1] ]
      eg = jacobiEigen (mat t)
      comb w = fixSign (U.ifoldl' (\v a c -> U.zipWith (\x y -> x + c * y) v (nth U.empty q a)) (U.replicate dim 0) w)
  pure (Eig (eVals eg) (V.map comb (eVecs eg)))
  where
    loop it q
      | it >= iters = pure q
      | otherwise = do
          z <- orthonormalize (map applyM q)
          let diff = foldl' max 0 (zipWith (\za qa -> 1 - abs (dotv za qa)) z q)
          if tol > 0 && diff < tol && it > 5 then pure z else loop (it + 1) z

-- PCA の結果（グラム行列のやり方）：vals = 固有値、vecs = 主成分（d 次元、長さ 1）、gramTrace = 全分散
data PcaG = PcaG { pgVals :: !Vec, pgVecs :: !Mat, pgTrace :: !Double }

-- ★ 次元 d が点の数 n より大きいとき：グラム行列 G = Xc Xcᵀ / (n − 1)（n×n）の固有ベクトル u から
-- 主成分 v = Xcᵀ u / ‖Xcᵀ u‖ を作る（G と C は 0 でない固有値が同じ）。u は部分空間反復法で上から k 個
pcaGram :: Mat -> Int -> Int -> R PcaG
pcaGram xc k iters = do
  eg <- subspaceEigen (matVec g) n (min (k + 2) n) iters 1e-13      -- 2 本多めに回すと k 本目までの収束が速い
  let vecs = V.map (\u -> unitv (U.ifoldl' (\v i ui -> U.zipWith (\a x -> a + ui * x) v (mrow xc i)) (U.replicate d 0) u)) (V.take k (eVecs eg))
  pure (PcaG (U.map (max 0) (U.take k (eVals eg))) vecs (traceOf g))
  where
    n = nRows xc
    d = nCols xc
    half = V.generate n (\i -> U.generate (n - i) (\t -> dotv (mrow xc i) (mrow xc (i + t))))   -- 上半分だけ計算（対称）
    g = V.generate n (\i -> U.generate n (\j -> (if j >= i then vix (mrow half i) (j - i) else vix (mrow half j) (i - j)) / fromIntegral (max 1 (n - 1))))
-- ---- カーネル PCA：K_ij = exp(−γ ‖x_i − x_j‖²) を「特徴空間で中心化」して固有ベクトル α を求める
rbfGram :: Mat -> Double -> Mat
rbfGram p gam = V.map (\a -> V.convert (V.map (\b -> exp (-gam * U.sum (U.zipWith (\x y -> (x - y) ^ (2 :: Int)) a b))) p)) p

centerGram :: Mat -> Mat                            -- Kc = K − 1K − K1 + 1K1（1 = すべて 1/n の行列）
centerGram k = V.imap (\i r -> U.imap (\j v -> v - vix rm i - vix rm j + allM) r) k
  where n = fromIntegral (nRows k)
        rm = V.convert (V.map (\r -> U.sum r / n) k) :: Vec
        allM = U.sum rm / n

data KPca = KPca { kpVals :: !Vec, kpComps :: !Mat, kpKc :: !Mat }

-- 第 k 成分の得点 = Kc α_k / √λ_k = √λ_k α_k（α_k は長さ 1 の固有ベクトル）
kernelPCA :: Mat -> Double -> Int -> R KPca
kernelPCA p gam k = do
  let kc = centerGram (rbfGram p gam)
  eg <- subspaceEigen (matVec kc) (nRows p) (k + 2) 400 1e-14
  let comps = V.imap (\c a -> U.map (* sqrt (max 0 (vix (eVals eg) c))) a) (V.take k (eVecs eg))
  pure (KPca (U.take k (eVals eg)) comps kc)

-- 「普通の PCA では分かれない」データ：同心円と 2 つの三日月（ラベル 0 / 1 つき）
makeCircles :: Int -> Double -> R [(Vec, Int)]
makeCircles n noise = forM [0 .. n - 1] $ \i -> do
  let lab = i `mod` 2
      r = if lab == 1 then 0.35 else 1
  t <- (* (2 * pi)) <$> rnd
  gx <- gaussRnd
  gy <- gaussRnd
  pure (vec [r * cos t + noise * gx, r * sin t + noise * gy], lab)

makeMoons :: Int -> Double -> R [(Vec, Int)]
makeMoons n noise = forM [0 .. n - 1] $ \i -> do
  let lab = i `mod` 2
  t <- (* pi) <$> rnd
  let (px, py) = if lab == 1 then (1 - cos t, 0.5 - sin t) else (cos t, sin t)
  gx <- gaussRnd
  gy <- gaussRnd
  pure (vec [px - 0.5 + noise * gx, py - 0.25 + noise * gy], lab)

-- 1 次元の値で 2 つのラベルをどれだけ分けられるか：いちばん良いしきい値での正解率（0.5〜1）
splitAccuracy :: [Double] -> [Int] -> Double
splitAccuracy vals labs = go 0 0 0 sortedLabs
  where
    n = fromIntegral (length vals)
    tot1 = fromIntegral (sum labs)
    sortedLabs = map snd (sortBy (\a b -> compare (fst a) (fst b)) (zip vals labs))
    go :: Int -> Double -> Double -> [Int] -> Double
    go k c1 best rest =
      let acc = (fromIntegral k - c1 + (tot1 - c1)) / n
          best' = maximum [best, acc, 1 - acc]
      in case rest of
           [] -> best'
           (l : ls) -> go (k + 1) (c1 + fromIntegral l) best' ls

signAccuracy :: [Double] -> [Int] -> Double
signAccuracy vals labs = let a = fromIntegral (length (filter id (zipWith (\v l -> (if v > 0 then 1 else 0) == l) vals labs))) / fromIntegral (max 1 (length vals))
                         in max a (1 - a)
-- ---- 重い計算を裏で（別のスレッドで計算し、できたら受け取る。そのあいだも画面は止まらない）
data Later a = Later ThreadId (MVar a)

-- force x で x を全部計算してから渡す（force は全部の数を足すなど、中身を最後まで使う関数）
startLater :: (a -> Double) -> a -> IO (Later a)
startLater force x = do
  mv <- newEmptyMVar
  tid <- forkIO (do r <- try (evaluate (force x)) :: IO (Either SomeException Double)
                    case r of
                      Right _ -> putMVar mv x
                      Left _ -> pure ())
  pure (Later tid mv)

pollLater :: Later a -> IO (Maybe a)          -- できていれば Just
pollLater (Later _ mv) = tryTakeMVar mv

cancelLater :: Later a -> IO ()               -- いらなくなった計算を止める
cancelLater (Later tid _) = killThread tid
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
