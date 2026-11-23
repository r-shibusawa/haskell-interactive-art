-- ============================================
-- 61-13 ベクトル量子化と画像の減色 ─ 画素の色 (r, g, b) を 3 次元の点とみなして k-means → k 色のパレット（中心）に置きかえる
--   各画素は「パレットの番号」だけ覚えればよい → 1 画素 24 ビットが ⌈log2 k⌉ ビットに（データ量が減る）
--   画質は PSNR = 10·log10(255² / MSE)（MSE = 色の差の 2 乗の平均）で測る。大きいほどもとの画像に近い（同じなら ∞）
--   k + 1 色は「k 色の答え + いちばん合っていない画素の色」から始めるので、k を増やすと PSNR は必ず上がるか同じ
--   画素は 12288 個あるので、k-means は Data.Vector.Unboxed の平らな配列（r, g, b, r, g, b, …）で計算し、1 フレームに少しずつ進める
-- 操作：↑↓ 色の数 k / E 誤差の画像 / スペース もとの画像 ⇔ 減色 / R 新しい画像
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import qualified Data.Vector as V
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

imgW, imgH :: Int
imgW = 128                 -- ★ 画像の大きさ（大きいと計算が重い）
imgH = 96

ks :: [Int]
ks = [1, 2, 3, 4, 6, 8, 12, 16, 24, 32]   -- ★ 調べる色の数

maxIter :: Int
maxIter = 30               -- ★ k-means のくり返しの上限

budget :: Int
budget = 300000            -- ★ 1 フレームに計算する「画素 × 色」の数（大きいと速く終わるが重い）

-- 1 つの k の答え：中心（パレット、平らな配列 3k）、ラベル、MSE、PSNR、ビット数
data QR = QR { qK :: !Int, qCs :: !(U.Vector Double), qLab :: !(U.Vector Int), qMse :: !Double, qPsnr :: !Double, qBits :: !Int }

-- 計算中の仕事：中心を 1 つずつ足しているところ（目標の k、中心、ラベル）か、k-means のくり返し（k、中心、ラベル、くり返した回数）
data Job = Grow !Int !(U.Vector Double) !(U.Vector Int)
         | Iter !Int !(U.Vector Double) !(U.Vector Int) !Int

data World = World
  { wPix     :: !(U.Vector Double)    -- 画素の色（r, g, b, r, g, b, …）
  , wResults :: ![QR]                 -- ks の順に、できたぶんだけ
  , wJob     :: !(Maybe Job)
  , wKSel    :: !Int
  , wShowErr :: !Bool
  , wShowOrig :: !Bool
  , wSeedI   :: !Word32
  , wImgO    :: Picture               -- もとの画像（1 倍）
  , wImgQ    :: Maybe (Int, Bool, Picture)   -- 減色した画像（k、誤差か）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-13 Vector quantization" (newImage 131 (World U.empty [] Nothing 4 False False 131 Blank Nothing [])))
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = newImage 131 (World U.empty [] Nothing 4 False False 131 Blank Nothing selfTest)
      U.sum (wPix w) `seq` pure w
  , skStep  = \_ _ w -> pure (refreshQ (advance w))
  , skDraw  = draw
  , skEvent = event
  }

-- ★ テスト用の合成画像：グラデーションの空、太陽、山、しま模様、色のついた丸
makeImage :: Int -> Int -> Word32 -> U.Vector Double
makeImage w h seed = U.concat [ U.fromList (pixel x y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
  where
    fw = fromIntegral w :: Double
    fh = fromIntegral h :: Double
    disks = withSeed seed $ replicateM 6 $ do
      dx <- rnd
      dy <- rnd
      dr <- rnd
      c <- replicateM 3 rnd
      pure (dx * fw, fh * (0.55 + 0.4 * dy), 5 + 9 * dr, map (* 255) c)
    pixel xi yi =
      let x = fromIntegral xi
          y = fromIntegral yi
          u = y / fh
          sky = [40 + 180 * u, 80 + 120 * u, 200 - 60 * u]                                     -- 空のグラデーション
          c1 = if sq (x - fw * 0.75) + sq (y - fh * 0.25) < sq (fh * 0.12) then [255, 210 - 40 * u, 90] else sky   -- 太陽
          hill = fh * (0.6 + 0.12 * sin (x * 0.07 + fromIntegral seed))
          c2 = if y > hill then [60 + 40 * sin (x * 0.3), 140 - 50 * ((y - hill) / fh), 70] else c1   -- 山
          c3 = if y > fh * 0.85 then (if odd (xi `div` 6) then [230, 230, 220] else [70, 60, 90]) else c2   -- しま
          c4 = L.foldl' (\c (ddx, ddy, dr, dc) -> if sq (x - ddx) + sq (y - ddy) < dr * dr then dc else c) c3 disks
      in map (clampv 0 255 . jsRound) c4
    sq v = v * v

-- ---- 平らな配列の k-means（クラスタリングの道具の lloyd と同じ計算・同じ順番）
nPix :: U.Vector Double -> Int
nPix px = U.length px `div` 3

sqd3 :: U.Vector Double -> Int -> U.Vector Double -> Int -> Double
sqd3 a !i b !j = let d0 = ix a (3 * i) - ix b (3 * j); d1 = ix a (3 * i + 1) - ix b (3 * j + 1); d2 = ix a (3 * i + 2) - ix b (3 * j + 2)
                 in d0 * d0 + d1 * d1 + d2 * d2
  where ix = U.unsafeIndex    -- 範囲の確認をしない（i, j は必ず範囲内）
{-# INLINE sqd3 #-}

assign3 :: U.Vector Double -> U.Vector Double -> U.Vector Int   -- ① いちばん近い中心
assign3 px cs = U.generate (nPix px) near1
  where
    k = U.length cs `div` 3
    near1 i = go 0 0 (1 / 0)
      where go !j !b !bd | j >= k = b
                         | otherwise = let d = sqd3 px i cs j in if d < bd then go (j + 1) j d else go (j + 1) b bd

inertia3 :: U.Vector Double -> U.Vector Double -> U.Vector Int -> Double
inertia3 px cs lab = L.foldl' (\s i -> s + sqd3 px i cs (lab U.! i)) 0 [0 .. nPix px - 1]

update3 :: U.Vector Double -> U.Vector Int -> U.Vector Double -> U.Vector Double   -- ② 仲間の平均（仲間 0 なら動かさない）
update3 px lab cs = U.generate (U.length cs) (\jd -> let c = cnt U.! (jd `div` 3) in if c > 0 then sums U.! jd / fromIntegral c else cs U.! jd)
  where
    k = U.length cs `div` 3
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) lab)
    sums = U.accumulate (+) (U.replicate (3 * k) 0) (U.generate (U.length px) (\id' -> (3 * (lab U.! (id' `div` 3)) + id' `mod` 3, px U.! id')))

pixAt :: U.Vector Double -> Int -> U.Vector Double
pixAt px i = U.slice (3 * i) 3 px

plusPlus3 :: U.Vector Double -> Int -> R (U.Vector Double)   -- k-means++（道具の initPlusPlus と同じ乱数の使い方）
plusPlus3 px k = do
  u <- rnd
  let c0 = pixAt px (min (n - 1) (floor (u * fromIntegral n)))
  loop [c0] (U.generate n (\i -> sqd3 px i c0 0))
  where
    n = nPix px
    loop out d2
      | length out >= k = pure (U.concat (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pixAt px pick
          loop (c : out) (U.imap (\i v -> min v (sqd3 px i c 0)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

startJob :: U.Vector Double -> U.Vector Double -> Job
startJob px c0 = Iter (U.length c0 `div` 3) c0 (assign3 px c0) 0

-- 仕事を 1 回ぶん進める（中心を 1 つ足す、または k-means を 1 回：更新 → 割り当て）。止まったら Right
stepJob :: U.Vector Double -> Job -> Either Job QR
stepJob px (Grow k c0 lab)
  | U.length c0 `div` 3 >= k = Left (Iter k c0 lab 0)
  | otherwise =
      -- ★ いちばん合っていない画素の色を新しい中心にする
      let (far, _) = L.foldl' (\(b, bd) i -> let d = sqd3 px i c0 (lab U.! i) in if d > bd then (i, d) else (b, bd)) (0, -1) [0 .. nPix px - 1]
          c1 = c0 U.++ pixAt px far
          lab1 = assign3 px c1
      in Left (if U.length c1 `div` 3 >= k then Iter k c1 lab1 0 else Grow k c1 lab1)
stepJob px (Iter k cs lab it) =
  let nc = update3 px lab cs
      nl = assign3 px nc
  in if nl == lab || it + 1 >= maxIter then Right (finish px k nc nl (inertia3 px nc nl)) else Left (Iter k nc nl (it + 1))

jobK :: Job -> Int                           -- いまの中心の数（計算の手間の目安）
jobK (Grow _ c _) = U.length c `div` 3 + 1
jobK (Iter k _ _ _) = k

runJob :: U.Vector Double -> Job -> QR
runJob px j = either (runJob px) id (stepJob px j)

-- ★ k 色の答え：MSE・PSNR・ビット数をまとめる
finish :: U.Vector Double -> Int -> U.Vector Double -> U.Vector Int -> Double -> QR
finish px k cs lab j = QR k cs lab mse (if mse > 0 then 10 * logBase 10 (255 * 255 / mse) else 1 / 0) (bitsOf k)
  where mse = j / fromIntegral (nPix px * 3)

bitsOf :: Int -> Int                         -- ⌈log2 k⌉
bitsOf k = length (takeWhile (< k) (iterate (* 2) 1))

-- ★ 前の答え + いちばん合っていない画素の色 から始める → J は増えないので PSNR は下がらない
growStart :: QR -> Int -> Job
growStart prev k = Grow k (qCs prev) (qLab prev)

quantizeChain :: U.Vector Double -> [Int] -> Word32 -> [QR]
quantizeChain px kks seed = case kks of
  [] -> []
  (k0 : rest) -> let r0 = runJob px (startJob px (withSeed seed (plusPlus3 px k0)))
                 in scanl (\prev k -> runJob px (growStart prev k)) r0 rest

-- ---- 1 フレームに少しずつ計算を進める（p5.js 版は 1 フレームに 1 つの k）
advance :: World -> World
advance w0 = go budget w0
  where
    px = wPix w0
    n = nPix px
    go b w
      | b <= 0 || length (wResults w) >= length ks = w
      | otherwise = case wJob w of
          Nothing -> case wResults w of
            [] -> go (b - n) w { wJob = Just (startJob px (withSeed 3 (plusPlus3 px (headOr 1 ks)))) }
            rs -> go b w { wJob = Just (growStart (last rs) (headOr 1 (drop (length rs) ks))) }
          Just j -> case stepJob px j of
            Left j' -> go (b - n * jobK j) w { wJob = Just j' }
            Right r -> go (b - n * jobK j) w { wJob = Nothing, wResults = wResults w ++ [r] }

current :: World -> Maybe QR
current w = headOr Nothing (map Just (drop (wKSel w) (wResults w)))

-- 減色した画像（または誤差 × 4）を作りなおす（k か表示が変わったときだけ）
refreshQ :: World -> World
refreshQ w = case current w of
  Nothing -> w
  Just e
    | fmap (\(k, er, _) -> (k, er)) (wImgQ w) == Just (qK e, wShowErr w) -> w
    | otherwise ->
        let px = wPix w
            cs = qCs e
            lab = qLab e
            img = pixelImage imgW imgH 1 $ \x y ->
              let i = y * imgW + x
                  l = lab U.! i
                  ch d = let v = px U.! (3 * i + d); q = cs U.! (3 * l + d)
                         in if wShowErr w then clampv 0 255 (4 * abs (v - q)) else jsRound q
              in (toW8 (ch 0), toW8 (ch 1), toW8 (ch 2))
        in img `seq` w { wImgQ = Just (qK e, wShowErr w, img) }

toW8 :: Double -> Word8
toW8 v = fromIntegral (max 0 (min 255 (floorI v)))

newImage :: Word32 -> World -> World
newImage s w = w { wPix = px, wResults = [], wJob = Nothing, wSeedI = s, wImgQ = Nothing
                 , wImgO = pixelImage imgW imgH 1 (\x y -> let i = 3 * (y * imgW + x) in (toW8 (px U.! i), toW8 (px U.! (i + 1)), toW8 (px U.! (i + 2)))) }
  where px = makeImage imgW imgH s

psnrText :: Double -> String
psnrText v = if isInfinite v then "inf" else fmt 2 v ++ " dB"

draw :: Env -> World -> Picture
draw env w = view env $
  [ imageAt (wImgO w) 40 110 ]
  ++ [ imageAt (if wShowOrig w then wImgO w else img) (80 + iw) 110 | Just _ <- [e], Just (_, _, img) <- [wImgQ w] ]
  ++ [ say "original (24 bits per pixel)" 40 102 14 (grayC 205) ]
  ++ [ say (if wShowErr w then "error x 4 (brighter = colour moved more)" else if wShowOrig w then "original (Space: back to reduced)" else show (qK r) ++ " colours (" ++ show (qBits r) ++ " bits per pixel)") (80 + iw) 102 14 (grayC 205) | Just r <- [e] ]
  -- ---- パレット
  ++ concat [ [ Color (rgbC (round' (cs U.! (3 * j))) (round' (cs U.! (3 * j + 1))) (round' (cs U.! (3 * j + 2)))) (rectTL (40 + fromIntegral r * sw) py (sw - 3) 30) | (r, j) <- zip [0 :: Int ..] order ]
              ++ [ say "palette (most used first)" 40 (py + 48) 14 (grayC 170) ]
            | Just r <- [e]
            , let cs = qCs r
            , let cnt = U.accumulate (+) (U.replicate (qK r) (0 :: Int)) (U.map (\l -> (l, 1)) (qLab r))
            , let order = L.sortBy (\a b -> compare (cnt U.! b) (cnt U.! a)) [0 .. qK r - 1]
            , let sw = min 36 (envW env * 0.5 / fromIntegral (qK r)) ]
  -- ---- PSNR のグラフ
  ++ [ plotAxes g "log2 k" "PSNR (dB)" [0, 1, 2, 3, 4, 5] [10, 20, 30, 40]
     , Color (rgbC 110 230 190) (polyline 2 [ (gX g (lg2 (qK r)), gY g (qPsnr r)) | r <- wResults w, not (isInfinite (qPsnr r)) ]) ]
  ++ [ Color (if i == wKSel w then white else rgbC 110 230 190) (circleAt (gX g (lg2 (qK r))) (gY g (min 45 (qPsnr r))) (if i == wKSel w then 12 else 6)) | (i, r) <- zip [0 ..] (wResults w) ]
  -- ---- 数
  ++ [ case e of
         Just r -> let bytes = (imgW * imgH * qBits r + 7) `div` 8 + 3 * qK r
                       orig = imgW * imgH * 3
                   in Pictures [ say ("k = " ++ show (qK r) ++ "   PSNR = " ++ psnrText (qPsnr r) ++ "   MSE = " ++ fmt 2 (qMse r)) 40 (py + 90) 16 (grayC 225)
                               , sayWrap ("Data: " ++ show (imgW * imgH) ++ " pixels x " ++ show (qBits r) ++ " bits + palette " ++ show (3 * qK r) ++ " bytes = " ++ show bytes ++ " bytes (" ++ fmt 1 (100 * fromIntegral bytes / fromIntegral orig) ++ "% of the original)") 40 (py + 96) (envW env * 0.55) 16 (grayC 225) ]
         Nothing -> say "computing..." 40 (py + 90) 16 (grayC 225)
     -- ---- 文字
     , say ("Vector quantization and colour reduction   k = " ++ show (headOr 1 (drop (wKSel w) ks)) ++ maybe "   (computing)" (\r -> "   PSNR " ++ psnrText (qPsnr r)) e) 40 40 20 (grayC 240)
     , say ("Up/Down number of colours k   E error image" ++ (if wShowErr w then " (on)" else "") ++ "   Space original <-> reduced   R new image") 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    e = current w
    sc = min ((envW env - 120) / (2 * fromIntegral imgW)) ((envH env - 330) / fromIntegral imgH)
    iw = fromIntegral imgW * sc
    ih = fromIntegral imgH * sc
    py = 130 + ih
    imageAt img x y = at (x + iw / 2, y + ih / 2) (Scale sc (-sc) img)
    g = G (envW env * 0.6) (py + 10) (envW env * 0.4 - 60) (envH env - py - 110) 0 5 10 45
    lg2 k = logBase 2 (fromIntegral k)
    round' v = max 0 (min 255 (jsRoundI v))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wKSel = min (length ks - 1) (wKSel w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wKSel = max 0 (wKSel w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> w { wShowErr = not (wShowErr w) }
    | c `elem` "rR" -> newImage (wSeedI w + 1) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShowOrig = not (wShowOrig w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("more colours -> higher PSNR: " ++ L.intercalate " -> " (map (psnrText . qPsnr) ch)
    , and (zipWith (\a b -> qPsnr b >= qPsnr a - 1e-9) ch (drop 1 ch)) && qPsnr (ch !!! 4) > qPsnr (ch !!! 0) + 10)
  , ("at convergence each palette colour = mean of the pixels replaced by it", cen)
  , ("every pixel is replaced by the nearest palette colour (brute force)", near1)
  , ("MSE = J (inertia) / (pixels x 3), PSNR = 10 log10(255^2 / MSE)"
    , near (qMse e * fromIntegral (nPix px) * 3) (inertia3 px (qCs e) (qLab e)) 1e-6 && near (qPsnr e) (10 * logBase 10 (65025 / qMse e)) 1e-12)
  , ("an image with only 5 colours reduced to 5 colours has error 0, PSNR = inf (shown as \"inf\")", qMse q5 == 0 && isInfinite (qPsnr q5) && psnrText (qPsnr q5) == "inf")
  , ("bits ceil(log2 k): k = 1 -> 0, 2 -> 1, 16 -> 4, 17 -> 5, 32 -> 5", map bitsOf [1, 2, 16, 17, 32] == [0, 1, 4, 5, 5]) ]
  where
    px = makeImage 48 36 7
    ch = quantizeChain px [1, 2, 4, 8, 16] 3
    xs !!! i = headOr (QR 0 U.empty U.empty 0 0 0) (drop i xs)
    e = ch !!! 3
    n = nPix px
    cen = and [ abs (meanOf [ px U.! (3 * i + d) | i <- mem ] - qCs e U.! (3 * j + d)) <= 1e-9
              | j <- [0 .. qK e - 1], let mem = [ i | i <- [0 .. n - 1], qLab e U.! i == j ], not (null mem), d <- [0 .. 2] ]
    near1 = and [ not (any (\j -> sqd3 px i (qCs e) j < sqd3 px i (qCs e) (qLab e U.! i) - 1e-9) [0 .. qK e - 1]) | i <- [0 .. n - 1] ]
    five = U.concat [ U.fromList (headOr [] (drop (i `mod` 5) [[10, 20, 30], [200, 0, 0], [0, 200, 0], [0, 0, 200], [250, 250, 250]])) | i <- [0 .. 199 :: Int] ]
    q5 = headOr (QR 0 U.empty U.empty 1 0 0) (quantizeChain five [5] 2)

-- ---------- やってみよう ----------
-- 1. ↑↓ で k を 1 → 32 に。何色くらいから「もとの画像と区別がつかない」？ PSNR は何 dB？
-- 2. E で誤差の画像を見よう。どこの色がいちばんずれる？（グラデーションの空はなかなか消えない）
-- 3. k = 2 のパレットは何色と何色？ 明るい・暗いに分かれる？
-- 4. データ量の % と PSNR のグラフを比べて、「ちょうどよい k」を決めよう
-- 5. budget を小さくすると計算の進み方がゆっくり見える。12 の SOM の色の地図も、色のパレットを作っている（しかも似た色がとなりに並ぶ）

-- ==================================================
-- ここから下は「クラスタリングの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点は U.Vector Double（[x, y] や [r, g, b]）、
-- 点の集まりは V.Vector（p5.js 版の配列の配列）、ラベルは U.Vector Int（−1 = ノイズ）
-- ==================================================
-- ---- 共通：クラスタの色・距離（点は [x, y] や [r, g, b] のような U.Vector Double）
type P = U.Vector Double
type Pts = V.Vector P

pt2 :: Double -> Double -> P                -- 2 次元の点 [x, y]
pt2 x y = U.fromList [x, y]

ptX, ptY :: P -> Double                     -- x 座標・y 座標（なければ 0）
ptX p = if U.length p > 0 then U.unsafeIndex p 0 else 0
ptY p = if U.length p > 1 then U.unsafeIndex p 1 else 0

palette :: [(Int, Int, Int)]
palette = [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130), (200, 240, 110), (255, 150, 220), (90, 210, 220), (255, 150, 70), (190, 190, 255)]

palRGB :: Int -> (Int, Int, Int)            -- クラスタ k の色（k < 0 = ノイズ = 灰色）
palRGB k | k < 0 = (120, 120, 135)
         | otherwise = headOr (0, 0, 0) (drop (k `mod` length palette) palette)

palC :: Int -> Color
palC k = palCA k 255

palCA :: Int -> Int -> Color                -- 不透明度 a（0〜255）つき
palCA k a = let (r, g, b) = palRGB k in rgbaC r g b a

sqd :: P -> P -> Double                     -- 距離の 2 乗
sqd a b = go 0 0
  where
    n = min (U.length a) (U.length b)
    go !d !s | d >= n = s
             | otherwise = let e = U.unsafeIndex a d - U.unsafeIndex b d in go (d + 1) (s + e * e)
{-# INLINE sqd #-}

edist :: P -> P -> Double                   -- ユークリッド距離
edist a b = sqrt (sqd a b)

meanOf :: [Double] -> Double
meanOf [] = 0
meanOf xs = foldl' (+) 0 xs / fromIntegral (length xs)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ。gauss の 2 つ目の値もとっておく）
data Rng = Rng !Word32 !(Maybe Double)

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s) Nothing

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s sp) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, Rng a sp)   -- Int を通すと速い

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

seedR :: Word32 -> R ()                     -- 計算の途中で seedRng(s)
seedR s = R (\_ -> ((), seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int               -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

gauss :: R Double                           -- 標準正規分布（ボックス-ミュラー法）
gauss = R $ \(Rng s sp) -> case sp of
  Just g -> (g, Rng s Nothing)
  Nothing ->
    let (u0, r1) = rndStep (Rng s Nothing)
        (v, Rng s2 _) = rndStep r1
        u = 1 - u0
        r = sqrt (-2 * log u)
    in (r * cos (2 * pi * v), Rng s2 (Just (r * sin (2 * pi * v))))

-- ---- グラフと図の道具：G = 画面の四角（左上 x, y、幅、高さ）とデータの範囲（p5.js と同じ、左上が原点）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

invX :: G -> Float -> Double                -- 画面の x → データの x
invX g sx = x0 g + realToFrac ((sx - pX g) / pW g) * (x1 g - x0 g)

invY :: G -> Float -> Double
invY g sy = y0 g + realToFrac ((pY g + pH g - sy) / pH g) * (y1 g - y0 g)

inBox :: G -> Float -> Float -> Bool
inBox g sx sy = sx >= pX g && sx <= pX g + pW g && sy >= pY g && sy <= pY g + pH g

-- 縦横の縮尺をそろえる（span = 短い辺に入るデータの幅、(cx, cy) = まん中）
fitView :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> G
fitView x y w h cx cy sp = G x y w h (cx - hw) (cx + hw) (cy - hh) (cy + hh)
  where
    s = realToFrac (min w h) / sp
    hw = realToFrac w / 2 / s
    hh = realToFrac h / 2 / s

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- クラスタの色で点を描く（ラベルがなければ白）。r = 直径、al = 不透明度（0〜255）
drawPts :: G -> Pts -> Maybe (U.Vector Int) -> Float -> Int -> Picture
drawPts g pts labels r al = Pictures
  [ Color (maybe (rgbaC 230 230 240 al) (\ls -> palCA (if i < U.length ls then ls U.! i else 0) al) labels) (circleAt (gX g (ptX p)) (gY g (ptY p)) r)
  | (i, p) <- zip [0 ..] (V.toList pts) ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double   -- clampv 下限 上限 値
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = let s = showFFloat (Just n) (fin 0 v) "" in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下に小さく（view の中で使う）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
