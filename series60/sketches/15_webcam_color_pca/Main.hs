-- ============================================
-- 60-15 カメラ映像の色の主成分 ─ 画素の色 (R, G, B) を 3 次元の点群とみなして PCA する
-- 160×120 = 19200 個の点の 3×3 共分散行列を、毎フレーム ヤコビ法で固有値分解
--   第 1 主成分だけで復元すると、色が 1 本の直線の上にのる → ほとんど「白黒」（ただし灰色の向きとは限らない）
--   2 本で色の平面、3 本で元どおり。下の色の帯 = 平均の色から ±2√λ だけ主成分の向きに動かした色
--   復元誤差（1 画素あたり、n − 1 で割る）= 捨てた固有値の和
--   画素は 19200 個あるので、点の列（Mat）は作らず、unboxed の数の並び（R, G, B, R, G, B, …）のまま計算する
-- 操作：1 2 3 使う成分の数 / V カメラ ⇔ テスト画像 / S 保存（SVG）/ スペース 映像を止める
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
import qualified Data.ByteString as B

cw, ch :: Int
cw = 160                         -- ★ 計算に使う映像の大きさ
ch = 120

nPix :: Int
nPix = cw * ch

-- 画素の並び：長さ nPix × 3 の (R, G, B, R, G, B, …)（0〜255 の数）
type Pix = U.Vector Double

data World = World
  { wCam    :: Maybe Camera
  , wUseCam :: !Bool
  , wLive   :: !Bool              -- いまカメラの映像を使っているか
  , wK      :: !Int               -- 使う成分の数
  , wFrozen :: !Bool
  , wSrc    :: !Pix               -- 元の映像
  , wRec    :: !(U.Vector Word8)  -- 復元した映像
  , wRes    :: CRes
  , wSrcPic, wRecPic :: Picture   -- 元の映像と復元した映像の絵（1 倍）
  , wCloud  :: [(Color, Double, Double)]   -- 13 画素に 1 つの (色, PC1, PC2)
  , wIsTest :: !Bool              -- いまの wSrc がテスト画像か
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-15 Colour PCA of the camera image" world0)
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch
      let w = update testPix world0 { wCam = Just cam, wTest = selfTest }
      _ <- evaluate (crErr (wRes w) + fromIntegral (U.sum (U.map fromIntegral (wRec w) :: U.Vector Int)) + sum [ a | (_, a, _) <- wCloud w ])
      _ <- evaluate (wSrcPic w)
      _ <- evaluate (wRecPic w)
      pure w
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = update testPix (World Nothing True False 1 False U.empty U.empty (CRes U.empty V.empty (Eig U.empty V.empty) 0) Blank Blank [] True [])

-- JavaScript の Uint8ClampedArray に入れたときと同じ丸め（0〜255 に切って、ちょうど半分は偶数へ）
clamp8 :: Double -> Double
clamp8 v = fromIntegral (round (clampv (fin v 0) 0 255) :: Int)

-- テスト画像：なめらかな色のグラデーションと、いくつかの色の丸（カメラがないとき用）
testPix :: Pix
testPix = U.concatMap (\i -> let (r, g, b) = px (i `mod` cw) (i `div` cw) in U.fromList [clamp8 r, clamp8 g, clamp8 b]) (U.enumFromN 0 nPix)
  where
    px x y =
      let u = fromIntegral x / fromIntegral cw
          v = fromIntegral y / fromIntegral ch
          base = (60 + 150 * u, 70 + 120 * v, 170 - 90 * u + 40 * sin (6 * v))
      in foldl' (\(r, g, b) (cx, cy, cr, (cr', cg, cb)) ->
                   let d = hypot (fromIntegral x - cx) (fromIntegral y - cy)
                       t = clampv ((cr - d) / 4) 0 1
                   in if d < cr then (r + t * (cr' - r), g + t * (cg - g), b + t * (cb - b)) else (r, g, b))
                base [(40, 40, 22, (240, 200, 60)), (110, 70, 30, (60, 160, 230)), (70, 95, 18, (230, 80, 90))]

-- PCA の結果：平均の色、共分散、固有値分解、復元誤差
data CRes = CRes { crMu :: !Vec, crC :: !Mat, crE :: !Eig, crErr :: !Double }

-- ★ 画素 → 点群 → 平均と共分散 → ヤコビ法 → k 本で復元（誤差は 0〜255 にそろえる前）
colorPCA :: Pix -> Int -> CRes
colorPCA p k = CRes mu c e (err / fromIntegral (n - 1))
  where
    n = U.length p `div` 3
    ch' i j = U.unsafeIndex p (3 * i + j)
    sumTo f = let go !i !s = if i >= n then s else go (i + 1) (s + f i) in go 0 0
    mu = U.generate 3 (\j -> sumTo (\i -> ch' i j) / fromIntegral (max 1 n))
    (m0, m1, m2) = (vix mu 0, vix mu 1, vix mu 2)
    -- 共分散：画素の順に足す（covMatrix と同じ順）
    cov a b = sumTo (\i -> (ch' i a - vix mu a) * (ch' i b - vix mu b)) / fromIntegral (n - 1)
    c00 = cov 0 0
    c01 = cov 0 1
    c02 = cov 0 2
    c11 = cov 1 1
    c12 = cov 1 2
    c22 = cov 2 2
    c = mat [[c00, c01, c02], [c01, c11, c12], [c02, c12, c22]]
    e = jacobiEigen c
    rp = reconPix p mu e k
    err = sumTo (\i -> let (x0', x1', x2') = rp i
                       in (ch' i 0 - x0') ^ (2 :: Int) + (ch' i 1 - x1') ^ (2 :: Int) + (ch' i 2 - x2') ^ (2 :: Int))

-- 画素 i を k 本で復元した色 x = μ + Σ ((x − μ)·v_c) v_c
reconPix :: Pix -> Vec -> Eig -> Int -> Int -> (Double, Double, Double)
reconPix p mu e k i = go 0 (vix mu 0, vix mu 1, vix mu 2)
  where
    d0 = U.unsafeIndex p (3 * i) - vix mu 0
    d1 = U.unsafeIndex p (3 * i + 1) - vix mu 1
    d2 = U.unsafeIndex p (3 * i + 2) - vix mu 2
    go c (!a, !b, !cc)
      | c >= k = (a, b, cc)
      | otherwise = let v = mrow (eVecs e) c
                        s = d0 * vix v 0 + d1 * vix v 1 + d2 * vix v 2
                    in go (c + 1) (a + s * vix v 0, b + s * vix v 1, cc + s * vix v 2)

-- 復元した映像（0〜255 に切って丸める）
reconImage :: Pix -> CRes -> Int -> U.Vector Word8
reconImage p r k = U.create $ do
  out <- UM.new (U.length p)
  forM_ [0 .. U.length p `div` 3 - 1] $ \i -> do
    let (a, b, c) = reconPix p (crMu r) (crE r) k i
        q v = fromIntegral (round (clamp8 v) :: Int)
    UM.unsafeWrite out (3 * i) (q a)
    UM.unsafeWrite out (3 * i + 1) (q b)
    UM.unsafeWrite out (3 * i + 2) (q c)
  pure out

update :: Pix -> World -> World
update p w = w { wSrc = p, wRes = r, wRec = rec, wSrcPic = pic (\i -> byte (U.unsafeIndex p i)), wRecPic = pic (U.unsafeIndex rec), wCloud = cloud }
  where
    r = colorPCA p (wK w)
    rec = reconImage p r (wK w)
    pic f = pixelImage cw ch 1 (\x y -> let i = 3 * (x + cw * y) in (f i, f (i + 1), f (i + 2)))
    mu = crMu r
    vecs = eVecs (crE r)
    cloud = [ (makeColorI (q 0) (q 1) (q 2) 255, dotv d (mrow vecs 0), dotv d (mrow vecs 1))
            | i <- [0, 13 .. nPix - 1]
            , let q j = round (vix p (3 * i + j)) :: Int
            , let d = U.generate 3 (\j -> vix p (3 * i + j) - vix mu j) ]

-- 毎コマ：止めていなければ、カメラ（なければテスト画像）を読んで計算し直す
step :: Env -> Float -> World -> IO World
step _ _ w
  | wFrozen w = pure w
  | otherwise = do
      live <- maybe (pure False) camLive (wCam w)
      let useLive = wUseCam w && live
      p <- case wCam w of
        Just cam | useLive -> do
          f <- cameraFrame cam
          pure (if B.length f >= nPix * 3 then Just (U.generate (nPix * 3) (\i -> fromIntegral (B.index f i))) else Nothing)
        _ -> pure Nothing
      -- テスト画像は変わらないので、すでに計算してあればそのまま（結果は毎コマ計算し直すのと同じ）
      pure (case p of
              Just px -> update px w { wLive = True, wIsTest = False }
              Nothing -> if wIsTest w then w { wLive = False } else update testPix w { wLive = False, wIsTest = True })

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (40 + iw / 2) (120 + ih / 2) (Scale k (-k) (wSrcPic w))
  , Translate (80 + iw + iw / 2) (120 + ih / 2) (Scale k (-k) (wRecPic w))
  , say ("Source " ++ (if wLive w then "(camera)" else "(test image)")) 40 110 15 (rgbC 205 212 222)
  , say ("Rebuilt from " ++ show (wK w) ++ " component" ++ (if wK w > 1 then "s" else "")) (80 + iw) 110 15 (rgbC 205 212 222) ]
  -- ---- 右：PC1-PC2 平面の点群（各点の色で）
  ++ (if pw > 100
        then [ Color (rgbC 60 72 88) (frameTL 1 px0 120 pw pw) ]
             ++ [ Color col (circleAt (px0 + pw / 2 + f2 (clampv (a / rr) (-0.5) 0.5) * pw) (120 + pw / 2 - f2 (clampv (b / rr) (-0.5) 0.5) * pw) 3)
                | (col, a, b) <- wCloud w ]
             ++ [ say "Colour cloud (x PC1, y PC2)" px0 110 15 (rgbC 205 212 222) ]
        else [])
  -- ---- 主成分の色の帯
  ++ concat [ [ Color (makeColorI (round (lerp lo hi j t)) (round (lerp lo hi (1 :: Int) t)) (round (lerp lo hi 2 t)) 255) (rectTL (x + fromIntegral ti * 11) y 11 26)
              | ti <- [0 .. 20 :: Int], let t = fromIntegral ti / 20, let j = 0 :: Int ]
              ++ [ Color (if k' < wK w then rgbC 255 225 140 else rgbC 80 92 108) (frameTL 2 x y 231 26)
                 , say ("PC" ++ show (k' + 1) ++ " = " ++ fmtVec (mrow vecs k') 2) x (y + 46) 14 (grayC (if k' < wK w then 240 else 150))
                 , say ("l = " ++ fmtN (vix vals k') 1 ++ " (" ++ fmtN (100 * vix vals k' / max tot 1e-9) 1 ++ "%)") x (y + 66) 14 (grayC (if k' < wK w then 240 else 150)) ]
            | k' <- [0 .. 2]
            , let x = 40 + fromIntegral k' * max 320 ((envW env - 80) / 3)
            , let sd = 2 * sqrt (max 0 (vix vals k'))
            , let lo = U.generate 3 (\j -> clampv (vix mu j - sd * vix (mrow vecs k') j) 0 255)
            , let hi = U.generate 3 (\j -> clampv (vix mu j + sd * vix (mrow vecs k') j) 0 255) ]
  ++ [ say ("Mean colour = " ++ fmtVec mu 1 ++ "   error per pixel = " ++ fmtN (crErr r) 2 ++ "   sum of dropped eigenvalues = " ++ fmtN (U.sum (U.drop (wK w) vals)) 2) 40 (y + 100) 15 (grayC 220)
     , sayWrap ("Angle between the grey direction (1, 1, 1)/sqrt3 and PC1 = " ++ fmtN (angleBetween (mrow vecs 0) (vec [1, 1, 1])) 1
                ++ " deg. A black-and-white photo projects onto the grey direction; rebuilding from PC1 only projects onto 'the direction in which the colours of this image change most'.")
               40 (y + 112) (envW env - 80) 14 (rgbC 165 175 190)
     , say "Colour PCA of the camera image" 40 44 22 (grayC 240)
     , say ("1 2 3 components   V camera <-> test image (now " ++ (if wUseCam w then "camera" else "test image") ++ ")   S save   Space freeze (" ++ (if wFrozen w then "ON" else "OFF") ++ ")   no camera: test image")
           40 72 15 (rgbC 165 175 190)
     , drawSelfTest env (wTest w) ]
  where
    iw = min ((envW env - 160) / 2.75) ((envH env - 320) * 4 / 3)
    ih = iw * 3 / 4
    k = iw / fromIntegral cw
    r = wRes w
    mu = crMu r
    vals = eVals (crE r)
    vecs = eVecs (crE r)
    tot = U.sum vals
    px0 = 120 + 2 * iw
    pw = min ih (envW env - px0 - 40)
    rr = 2.6 * sqrt (max (vix vals 0) 1)
    y = 150 + ih
    lerp lo hi j t = vix lo j + (vix hi j - vix lo j) * t

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> let w1 = w { wK = fromEnum c - fromEnum '0' }
                        in pure (update (wSrc w1) w1)
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> do saveSvg "color_pca" env (draw env w); pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFrozen = not (wFrozen w) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目、テスト画像で）
selfTest :: [(String, Bool)]
selfTest =
  [ ("colour covariance is symmetric and PSD (smallest eigenvalue >= 0)",
      mix c 0 1 == mix c 1 0 && mix c 0 2 == mix c 2 0 && mix c 1 2 == mix c 2 1 && vix (eVals e) 2 > -1e-9 * tr)
  , ("Jacobi: C V = V Lambda (<= 1e-9 x trace), orthonormal", eigResidual c e < 1e-9 * tr && orthoError (eVecs e) < 1e-12)
  , ("rebuilding from 3 components gives the original (error ~ 0)", crErr r3 < 1e-12 * tr)
  , ("k = 0, 1, 2: error sum |x - x_hat|^2/(n - 1) = sum of dropped eigenvalues (before clipping to 0-255)",
      and [ near (crErr (colorPCA testPix k)) (U.sum (U.drop k (eVals e))) (1e-9 * tr) | k <- [0 .. 2] ])
  , ("colours rebuilt from 1 component lie on one line (l2/l1 < 0.01; clipping to 0-255 shifts a little)", vix (eVals e1) 1 / vix (eVals e1) 0 < 0.01)
  , ("for a grey image PC1 = grey direction (1, 1, 1)/sqrt3, l2 = l3 = 0",
      angleBetween (mrow (eVecs (crE rg)) 0) (vec [1, 1, 1]) < 1e-6 && vix (eVals (crE rg)) 1 < 1e-9 * vix (eVals (crE rg)) 0) ]
  where
    r3 = colorPCA testPix 3
    c = crC r3
    e = crE r3
    tr = traceOf c
    out = reconImage testPix (colorPCA testPix 1) 1
    p1 = V.fromList [ vec [ fromIntegral (out U.! (3 * i + j)) | j <- [0 .. 2] ] | i <- [0, 7 .. nPix - 1] ]
    e1 = jacobiEigen (covMatrix p1)
    gray = U.generate (nPix * 3) (\idx -> fromIntegral (((idx `div` 3) * 37) `mod` 256))
    rg = colorPCA gray 1

-- ---------- やってみよう ----------
-- 1. 1 → 2 → 3 で復元を比べよう。1 本だけでも「白黒」よりきれいに見える？
-- 2. 赤い服・青い空など、色の多い場面と、白い壁の前で、PC1 の向きはどう変わる？
-- 3. PC1 が灰色の向きから大きくずれるのはどんな映像？
-- 4. 2 本だけ使うと、どの色がうまく出ない？（捨てた PC3 の色の帯を見よう）
-- 5. PC1 の得点だけを白黒で表示する「自分の映像専用の白黒写真」を作ってみよう

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
