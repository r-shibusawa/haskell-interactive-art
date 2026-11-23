-- ============================================
-- 60-14 音色の主成分 ─ 倍音 12 個の強さで表した音色を PCA して、2 次元の「音色マップ」を作る
-- 音色 = 基本の高さ f の 1〜12 倍の音（倍音）をどれだけ混ぜるか。のこぎり波・矩形波・三角波・フルート風・金管風の
--   5 つの「家族」を少しずつ変えた 150 個の音色（合成）→ 12 次元の点。ヤコビ法で PCA して、PC1・PC2 の平面に並べる
--   マップの上をマウスで押すと、その位置を逆に写した倍音 μ + s1 v1 + s2 v2（負は 0 にする）で音が鳴る
--   Art.Sound は音を続けて鳴らせないので、強い倍音 5 つを 0.25 秒ごとに短い音として鳴らしなおす
-- 音量は小さめから（全体の音量 master は 0.2 以下）。ヘッドホンは特に注意
-- 操作：マップを押す・なぞる 音を鳴らす / スペース 鳴らしっぱなし ON/OFF / ↑↓ 音の高さ / R 新しい音色
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

nH, nT :: Int
nH = 12                          -- ★ 倍音の数
nT = 150                         -- ★ 音色の数

master :: Double
master = 0.1                     -- ★ 全体の音量（0.2 以下にしておく）

famNames :: [String]
famNames = ["saw", "square", "triangle", "flute-like", "brass-like"]

famCol :: Int -> Int -> Color
famCol a k = case k of
  0 -> rgbaC 255 225 140 a
  1 -> rgbaC 90 165 240 a
  2 -> rgbaC 150 220 160 a
  3 -> rgbaC 220 170 255 a
  _ -> rgbaC 255 125 105 a

data World = World
  { wX      :: !Mat               -- 音色（150 行 × 12 列）
  , wFams   :: [Int]              -- 家族の番号
  , wMu     :: !Vec
  , wEg     :: !Eig
  , wScores :: !Mat               -- PC1・PC2 の得点
  , wSeed   :: !Int
  , wHold   :: !Bool              -- 鳴らしっぱなし
  , wF0     :: !Double            -- ★ 基本の高さ（Hz）
  , wCur    :: !Vec               -- マップの上の位置 (s1, s2)
  , wNext   :: !Float             -- 次の音までの時間（秒）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-14 Principal components of timbre" (rebuild world0))
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = rebuild world0 { wTest = selfTest }
      _ <- evaluate (sumMat (wScores w) + U.sum (eVals (wEg w)))
      pure w
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World V.empty [] U.empty (Eig U.empty V.empty) V.empty 14 False 220 (vec [0, 0]) 0 []

-- ★ 家族ごとの倍音の形（k = 1〜12）に、傾きと山の位置のばらつきを足す。最大を 1 にそろえる
makeTimbre :: Int -> R Vec
makeTimbre fam = do
  tilt <- rndRange 0.8 1.2
  pk <- rndRange 4.6 5.4
  out <- forM [1 .. nH] $ \k -> do
    let kd = fromIntegral k :: Double
        a = case fam of
          0 -> 1 / kd ** tilt
          1 -> if odd k then 1 / kd ** tilt else 0.02
          2 -> if odd k then 1 / kd ** (2 * tilt) else 0.01
          3 -> if k == 1 then 1 else 0.35 * exp (-(kd - 2) * tilt)
          _ -> exp (-((kd - pk) ^ (2 :: Int)) / 4) + 0.2 / kd
    g <- gaussRnd
    pure (max 0 (a * (1 + 0.08 * g)))
  let m = foldl' max 1e-9 out
  pure (vec (map (/ m) out))

rebuild :: World -> World
rebuild w = w { wX = x, wFams = fams, wMu = mu, wEg = eg, wScores = projectData x mu (eVecs eg) 2 }
  where
    fams = [ i `mod` 5 | i <- [0 .. nT - 1] ]
    x = V.fromList (withSeed (wSeed w) (mapM makeTimbre fams))
    mu = colMeans x
    eg = jacobiEigen (covMatrix x)

-- ★ マップの位置 (s1, s2) → 倍音（逆射影）。負の強さは 0 に
backProject :: World -> Vec -> Vec
backProject w s = U.map (max 0) (reconstructRow s (wMu w) (eVecs (wEg w)))

-- マップの場所（左上 x, y と大きさ s）
mapGeo :: Env -> (Float, Float, Float)
mapGeo env = (40, 120, min (envW env * 0.45) (envH env - 220))

mapRange :: World -> Double
mapRange w = V.foldl' (\a v -> U.foldl' (\b q -> max b (abs q)) a v) 1e-9 (wScores w) * 1.15

toMap :: Env -> World -> Vec -> (Float, Float)
toMap env w sc = let (gx, gy, s) = mapGeo env
                     r = mapRange w
                 in (gx + s / 2 + f2 (vix sc 0 / r) * s / 2, gy + s / 2 - f2 (vix sc 1 / r) * s / 2)

fromMap :: Env -> World -> (Float, Float) -> Vec
fromMap env w (x, y) = let (gx, gy, s) = mapGeo env
                           r = mapRange w
                       in vec [realToFrac ((x - gx - s / 2) / (s / 2)) * r, realToFrac (-(y - gy - s / 2) / (s / 2)) * r]

inMap :: Env -> Bool
inMap env = let (mx, my) = mouseP env
                (gx, gy, s) = mapGeo env
            in mx > gx && mx < gx + s && my > gy && my < gy + s

-- 毎コマ：押している間は位置を動かし、0.25 秒ごとに強い倍音 5 つを短く鳴らす
step :: Env -> Float -> World -> IO World
step env dt w0 = do
  let pressed = envDown env && inMap env
      w = if pressed then w0 { wCur = fromMap env w0 (mouseP env) } else w0
      on = wHold w || pressed
  if on && wNext w - dt <= 0
    then do
      let amps = backProject w (wCur w)
          tot = let t = U.sum amps in if t > 0 then t else 1
          parts = take 5 (sortBy (\a b -> compare (snd b) (snd a)) [ (fromIntegral k * wF0 w, a) | (k, a) <- zip [1 :: Int ..] (U.toList amps), fromIntegral k * wF0 w < 18000 ])
      forM_ parts $ \(f, a) -> when (a > 0) (playTone (realToFrac f) (realToFrac (master * 0.8 * a / tot)) 0.26 Sine)
      pure w { wNext = 0.25 }
    else pure w { wNext = if on then wNext w - dt else 0 }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 音色マップ
  [ Color (rgbC 60 72 88) (frameTL 1 gx gy s s), Color (rgbC 60 72 88) (lineW 1 (gx, gy + s / 2) (gx + s, gy + s / 2)), Color (rgbC 60 72 88) (lineW 1 (gx + s / 2, gy) (gx + s / 2, gy + s)) ]
  ++ [ Color (famCol 200 f) (uncurry circleAt (toMap env w sc) 8) | (sc, f) <- zip (V.toList (wScores w)) (wFams w) ]
  ++ [ Color white (ringAt qx qy 24 2), Color white (lineW 2 (qx - 16, qy) (qx + 16, qy)), Color white (lineW 2 (qx, qy - 16) (qx, qy + 16)) ]
  ++ concat [ [ Color (famCol 255 k) (circleAt (gx + fromIntegral k * 112 + 6) (gy + s + 19) 10), say nm (gx + fromIntegral k * 112 + 16) (gy + s + 24) 14 (famCol 255 k) ] | (k, nm) <- zip [0 ..] famNames ]
  -- ---- いまの倍音（棒）と波形
  ++ [ say "Current partial strengths (harmonics 1-12)" bx (by - 10) 14 (rgbC 205 212 222) ]
  ++ concat [ [ Color (rgbC 255 225 140) (rectTL (bx + fromIntegral k * bw / fromIntegral nH + 3) (by + bh - h) (bw / fromIntegral nH - 6) h)
              , sayC (show (k + 1)) (bx + (fromIntegral k + 0.5) * bw / fromIntegral nH) (by + bh + 18) 14 (grayC 170) ]
            | (k, a) <- zip [0 :: Int ..] (U.toList amps), let h = f2 (clampv a 0 1.2) * bh / 1.2 ]
  ++ [ say "Waveform (2 periods)" bx (wy - 10) 14 (rgbC 205 212 222)
     , Color (rgbC 90 165 240) (polyline 2 [ (bx + fromIntegral i / 299 * bw, wy + 60 - f2 v * 55)
                                           | i <- [0 .. 299 :: Int], let t = fromIntegral i / 299 * 4 * pi
                                           , let v = U.ifoldl' (\acc k a -> acc + a * sin (fromIntegral (k + 1) * t)) 0 amps / tot ])
  -- ---- 主成分の形
     , say "PC v1 (yellow), v2 (blue): how partial strengths change" bx (py - 10) 14 (rgbC 205 212 222)
     , Color (rgbC 70 82 98) (lineW 1 (bx, py + 50) (bx + bw, py + 50)) ]
  ++ [ Color c (polyline 2.5 [ (bx + (fromIntegral j + 0.5) * bw / fromIntegral nH, py + 50 - f2 v * 70) | (j, v) <- zip [0 :: Int ..] (U.toList (mrow (eVecs (wEg w)) k)) ])
     | (k, c) <- [(0, rgbC 255 225 140), (1, rgbC 90 165 240)] ]
  ++ [ say ("Explained PC1 " ++ fmtN (100 * l 0 / tr) 1 ++ "%, PC2 " ++ fmtN (100 * l 1 / tr) 1 ++ "% (two: " ++ fmtN (100 * (l 0 + l 1) / tr) 1 ++ "%)") bx yt 15 (grayC 220)
     , say ("Map position (s1, s2) = " ++ fmtVec (wCur w) 2) bx (yt + 24) 15 (grayC 220)
     , say "-> partials = mu + s1 v1 + s2 v2 (negative set to 0)" bx (yt + 48) 15 (grayC 220)
     , say "Principal components of timbre" 40 44 22 (grayC 240)
     , say ("Press/drag map to play   Space hold (" ++ (if wHold w then "ON" else "OFF") ++ ")   Up/Down pitch (" ++ show (round (wF0 w) :: Int) ++ " Hz)   R new timbres   keep volume low")
           40 72 15 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    (gx, gy, s) = mapGeo env
    (qx, qy) = toMap env w (wCur w)
    amps = backProject w (wCur w)
    tot = let t = U.sum amps in if t /= 0 then t else 1
    bx = gx + s + 50
    bw = envW env - bx - 40
    bh = 150
    by = 140
    wy = by + bh + 60
    py = wy + 145
    yt = py + 128
    l k = vix (eVals (wEg w)) k
    tr = U.sum (eVals (wEg w))

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wHold = not (wHold w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wF0 = clampv (wF0 w * 2 ** (1 / 12)) 110 880 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wF0 = clampv (wF0 w * 2 ** (-1 / 12)) 110 880 }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> pure (rebuild w { wSeed = wSeed w + 1 })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("12x12 Jacobi: C V = V Lambda, orthonormal, sum of eigenvalues = trace",
      eigResidual c e < 1e-9 * tr && orthoError (eVecs e) < 1e-12 && near (U.sum (eVals e)) tr (1e-12 * tr))
  , ("back-projecting with all 12 components restores the timbre (1e-12)",
      and [ U.and (U.zipWith (\v xj -> near v xj 1e-12) (reconstructRow (mrow (projectData (V.singleton xr) mu (eVecs e) nH) 0) mu (eVecs e)) xr) | xr <- take 10 (V.toList x) ])
  , ("map position -> partials -> projection returns the same position (before clipping)", okRT)
  , ("on the 2D map the nearest family centre is right for >= 80%", fromIntegral hit / fromIntegral nT >= (0.8 :: Double))
  , ("anywhere on the map the partials are >= 0, no NaN",
      and [ noNaN a && all (>= 0) a | sv <- [[-9, -9], [9, 9], [0, 0], [3, -7]], let a = map (max 0) (U.toList (reconstructRow (vec sv) mu (eVecs e))) ])
  , ("volume limit: master <= 0.2", master <= 0.2) ]
  where
    fs = [ i `mod` 5 | i <- [0 .. nT - 1] ]
    x = V.fromList (withSeed 14 (mapM makeTimbre fs))
    mu = colMeans x
    c = covMatrix x
    e = jacobiEigen c
    tr = traceOf c
    okRT = and (withSeed 5 (replicateM 30 (do
             a <- rndRange (-1) 1
             b <- rndRange (-1) 1
             let xr = reconstructRow (vec [a, b]) mu (eVecs e)
                 s2 = mrow (projectData (V.singleton xr) mu (eVecs e) 2) 0
             pure (near (vix s2 0) a 1e-12 && near (vix s2 1) b 1e-12))))
    sc = V.toList (projectData x mu (eVecs e) 2)
    cent = [ (sum (map (`vix` 0) m) / fromIntegral (length m), sum (map (`vix` 1) m) / fromIntegral (length m))
           | f <- [0 .. 4], let m = [ q | (q, fi) <- zip sc fs, fi == f ] ]
    dist q (cx, cy) = hypot (vix q 0 - cx) (vix q 1 - cy)
    hit = length [ () | (q, fi) <- zip sc fs
                      , let best = foldl' (\b (k, ce) -> if dist q ce < dist q (nth (0, 0) cent b) then k else b) 0 (zip [0 ..] cent)
                      , best == fi ]

-- ---------- やってみよう ----------
-- 1. マップの上を、のこぎり → 矩形 → 三角 となぞって、音色の変わり方を聞こう
-- 2. 家族と家族の「あいだ」は、どんな音？ どこにも家族がいない所は？
-- 3. 下の v1・v2 の折れ線を見て、PC1 が「何を変える向き」か言葉にしよう（高い倍音の量？ 偶数倍音？）
-- 4. makeTimbre に新しい家族（例：偶数倍音だけ）を足して R。マップはどう変わる？
-- 5. 倍音の強さを log（デシベル）にしてから PCA すると、マップは変わる？

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
