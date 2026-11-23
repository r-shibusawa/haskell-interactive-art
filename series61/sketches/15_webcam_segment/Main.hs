-- ============================================
-- 61-15 カメラ映像の色でクラスタリング ─ 画素を 5 次元の点 (r, g, b, w·x, w·y) にして k-means → ポスターのように塗り分け
--   w = 位置の重み。0 なら色だけで分ける（離れた場所の同じ色も同じクラスタ）、大きいと近い場所どうしがまとまる（領域分割）
--   毎フレーム最初からやり直すと、中心の番号や位置が変わって画面がちらつく
--   → 前のフレームの中心から始めて、1 フレームに割り当て + 更新を 1 回だけ（少しずつ追いかける）
--   右下の「ちらつき」= 前のフレームから色（クラスタ）が変わった画素の割合
--   特徴は Data.Vector.Unboxed の平らな配列（画素ごとに 5 つずつ）。p5.js 版と同じく Float32 の精度にそろえて入れる
-- 操作：↑↓ k / ← → 位置の重み w / I 中心を選び直す（k-means++）/ V カメラ ⇔ テスト画像 / S 保存
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32, Word8)
import Data.List (foldl', sort, sortBy)
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch, nf :: Int
cw = 160                   -- カメラの大きさ
ch = 120
nf = 5                     -- 特徴の次元 (r, g, b, w·x, w·y)

data World = World
  { wCam     :: Maybe Camera
  , wUseCam  :: !Bool
  , wLive    :: !Bool
  , wK       :: !Int                  -- ★ 色の数
  , wPos     :: !Double               -- ★ 位置の重み
  , wFeat    :: !(U.Vector Double)    -- 特徴（画素ごとに 5 つ）
  , wLab     :: !(U.Vector Int)
  , wCs      :: !(U.Vector Double)    -- 中心（5 つずつ、k 個）
  , wSrc     :: !(U.Vector Word8)     -- いまの画像（r, g, b, …）
  , wFlicker :: !Double
  , wJ       :: !Double
  , wTestT   :: !Int
  , wRng     :: !Rng                  -- 乱数の状態（p5.js 版と同じく testImage が種を入れ直す）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-15 Camera colour clustering" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch          -- 左右反転した映像（p5.js の flipped: true）
      let w = world0 { wCam = Just cam, wTest = selfTest }
      U.sum (wCs w) `seq` pure w
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- setup：テスト画像（0 番）の特徴から k-means++ で中心を選ぶ
world0 :: World
world0 = World Nothing True False 6 0.6 feat (U.replicate (cw * ch) 0) cs px 0 0 0 r2 []
  where
    (px, r1) = testImage 0 0
    feat = fillFeatures px 0.6
    (cs, r2) = runR (segInit feat 6) r1

-- Float32Array に入れたときと同じ値にする（p5.js 版の特徴は 32 ビットの小数）
f32 :: Double -> Double
f32 v = realToFrac (realToFrac v :: Float)

-- ★ 画素 → 特徴 (r, g, b, w·x, w·y)。位置は 0〜255 にそろえてから w 倍
fillFeatures :: U.Vector Word8 -> Double -> U.Vector Double
fillFeatures px w = U.create $ do
  f <- UM.new (cw * ch * nf)
  forM_ [0 .. cw * ch - 1] $ \i -> do
    let q = i * nf
    UM.unsafeWrite f q (fromIntegral (U.unsafeIndex px (3 * i)))
    UM.unsafeWrite f (q + 1) (fromIntegral (U.unsafeIndex px (3 * i + 1)))
    UM.unsafeWrite f (q + 2) (fromIntegral (U.unsafeIndex px (3 * i + 2)))
    UM.unsafeWrite f (q + 3) (f32 (w * fromIntegral (255 * (i `mod` cw)) / fromIntegral cw))
    UM.unsafeWrite f (q + 4) (f32 (w * fromIntegral (255 * (i `div` cw)) / fromIntegral ch))
  pure f

d2 :: U.Vector Double -> Int -> U.Vector Double -> Int -> Double   -- 画素 i と中心 j の距離²（5 次元）
d2 f !i c !j = e 0 * e 0 + e 1 * e 1 + e 2 * e 2 + e 3 * e 3 + e 4 * e 4
  where e d = U.unsafeIndex f (i * nf + d) - U.unsafeIndex c (j * nf + d)
{-# INLINE d2 #-}

-- ★ 割り当て（いちばん近い中心）。J（慣性）も返す
segAssign :: U.Vector Double -> U.Vector Double -> (U.Vector Int, Double)
segAssign f cs = runST $ do
  let k = U.length cs `div` nf
      n = U.length f `div` nf
  out <- UM.new n
  let loop !i !jSum
        | i >= n = pure jSum
        | otherwise = nearest i 0 0 (1 / 0) jSum
      nearest !i !j !b !bd !jSum
        | j >= k = do UM.unsafeWrite out i b; loop (i + 1) (jSum + bd)
        | otherwise = let v = d2 f i cs j in if v < bd then nearest i (j + 1) j v jSum else nearest i (j + 1) b bd jSum
  jTot <- loop 0 0
  lab <- U.unsafeFreeze out
  pure (lab, jTot)

-- ★ 更新（仲間の平均。仲間 0 の中心は動かさない）
segUpdate :: U.Vector Double -> U.Vector Int -> U.Vector Double -> U.Vector Double
segUpdate f lb cs = runST $ do
  let k = U.length cs `div` nf
  sums <- UM.replicate (k * nf) 0
  cnt <- UM.replicate k (0 :: Int)
  forM_ [0 .. U.length lb - 1] $ \i -> do
    let l = lb U.! i
    UM.unsafeModify cnt (+ 1) l
    forM_ [0 .. nf - 1] $ \d -> UM.unsafeModify sums (+ U.unsafeIndex f (i * nf + d)) (l * nf + d)
  s <- U.freeze sums
  c <- U.freeze cnt
  pure (U.generate (k * nf) (\jd -> let n = c U.! (jd `div` nf) in if n > 0 then s U.! jd / fromIntegral n else cs U.! jd))

-- 仲間 0 の中心は、いちばん合っていない画素へ移す（J は増えない）。ラベルも書きかえる
reviveEmpty :: U.Vector Double -> U.Vector Int -> U.Vector Double -> (U.Vector Double, U.Vector Int)
reviveEmpty f lb0 cs0 = L.foldl' revive (cs0, lb0) [ j | j <- [0 .. k - 1], not (used U.! j) ]
  where
    k = U.length cs0 `div` nf
    used = U.create (do { u <- UM.replicate k False; U.forM_ lb0 (\l -> UM.unsafeWrite u l True); pure u })   -- 仲間がいるか
    revive (cs, lb) j =
      let (far, _) = L.foldl' (\(b, bd) i -> let v = d2 f i cs (lb U.! i) in if v > bd then (i, v) else (b, bd)) (0, -1) [0, 3 .. U.length lb - 1]
      in (cs U.// [ (j * nf + d, f U.! (far * nf + d)) | d <- [0 .. nf - 1] ], lb U.// [(far, j)])

segInit :: U.Vector Double -> Int -> R (U.Vector Double)   -- k-means++（1000 画素だけ見て選ぶ）
segInit f k = do
  let n = U.length f `div` nf
  sample <- V.replicateM 1000 (do u <- rnd; let i = floor (u * fromIntegral n) in pure (U.slice (i * nf) nf f))
  cs <- initPlusPlus sample k
  pure (U.concat (V.toList cs))

-- 位置の重みを変えたら、中心の位置の成分も同じ割合で直す
setPositionWeight :: Double -> World -> World
setPositionWeight w wd = wd { wPos = w, wCs = U.imap fix (wCs wd) }
  where
    old = wPos wd
    fix jd c | jd `mod` nf >= 3 = if old > 0 then c * w / old else w * 255 / 2
             | otherwise = c

-- カメラがないときのテスト画像：空と地面のグラデーション、動く太陽、色の四角（ノイズ少し）。種を入れ直すので乱数の状態も返す
testImage :: Int -> Int -> (U.Vector Word8, Rng)
testImage t seed = runST $ do
  p <- UM.new (cw * ch * 3)
  let sx = 40 + 30 * sin (fromIntegral t * 0.03) :: Double
      put !i !r !g !b = do
        UM.unsafeWrite p (3 * i) (clamp8 r)
        UM.unsafeWrite p (3 * i + 1) (clamp8 g)
        UM.unsafeWrite p (3 * i + 2) (clamp8 b)
      loop !i !rg
        | i >= cw * ch = pure rg
        | otherwise = do
            let (y, x) = i `divMod` cw
                fx = fromIntegral x :: Double
                fy = fromIntegral y :: Double
                (u, rg') = mulberry rg
                nz = 8 * (u - 0.5)
            if x > 110 && x < 145 && y > 85 && y < 112 then put i (70 + nz) (80 + nz) (200 + nz)           -- 青い四角
            else if x > 20 && x < 55 && y > 80 && y < 110 then put i (200 + nz) (60 + nz) (70 + nz)       -- 赤い四角
            else if (fx - sx - 60) * (fx - sx - 60) + (fy - 30) * (fy - 30) < 300 then put i (255 + nz) (210 + nz) (80 + nz)   -- 太陽
            else if y < 70 then put i (80 + fy + nz) (140 + fy + nz) (230 + nz)                          -- 空
            else put i (90 + nz) (150 - (fy - 70) + nz) (60 + nz)                                        -- 地面
            loop (i + 1) rg'
  sEnd <- loop 0 (let s0 = 1000 + fromIntegral seed in if s0 == 0 then 1 else s0)
  img <- U.unsafeFreeze p
  pure (img, Rng sEnd Nothing)

-- 道具の rnd と同じ mulberry32（状態を Word32 のまま回して速くする）
mulberry :: Word32 -> (Double, Word32)
mulberry s =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, a)   -- Int を通すと速い
{-# INLINE mulberry #-}

clamp8 :: Double -> Word8                  -- Uint8ClampedArray と同じ（0〜255、ちょうど半分は偶数へ丸める）
clamp8 v | isNaN v || v <= 0 = 0
         | v >= 255 = 255
         | otherwise = fromIntegral (round v :: Int)

-- 1 フレーム：画像 → 特徴 → 前の中心から 割り当て + 更新 を 1 回だけ
step :: Env -> Float -> World -> IO World
step _ _ w = do
  live <- case wCam w of
    Just cam | wUseCam w -> camLive cam
    _ -> pure False
  camPx <- case wCam w of
    Just cam | live -> do
      fr <- cameraFrame cam
      pure (if B.length fr >= cw * ch * 3 then Just (U.generate (cw * ch * 3) (B.index fr)) else Nothing)
    _ -> pure Nothing
  let (px, rng, tt) = case camPx of
        Just p -> (p, wRng w, wTestT w)
        Nothing -> let t = wTestT w + 1; (p, r) = testImage t t in (p, r, t)
      feat = fillFeatures px (wPos w)
      (lab1, _) = segAssign feat (wCs w)
      (csR, labR) = reviveEmpty feat lab1 (wCs w)
      cs2 = segUpdate feat labR csR
      (lab2, j) = segAssign feat cs2
      changed = U.length (U.filter id (U.zipWith (/=) lab2 (wLab w)))
      fl = 0.9 * wFlicker w + 0.1 * (fromIntegral changed / fromIntegral (U.length lab2))
  pure $! w { wLive = live && camPx /= Nothing, wFeat = feat, wLab = lab2, wCs = cs2, wSrc = px, wFlicker = fl, wJ = j, wTestT = tt, wRng = rng }

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (40 + 160, 110 + 120) (Scale 2 (-2) (pixelImage cw ch 1 (\x y -> let i = 3 * (x + cw * y) in (U.unsafeIndex src i, U.unsafeIndex src (i + 1), U.unsafeIndex src (i + 2)))))
  , at (400 + fromIntegral cw * big / 2, 110 + fromIntegral ch * big / 2)
       (Scale big (-big) (pixelImage cw ch 1 (\x y -> let l = U.unsafeIndex lab (x + cw * y) in (pal U.! (3 * l), pal U.! (3 * l + 1), pal U.! (3 * l + 2))))) ]
  -- ---- パレット
  ++ concat [ [ Color (rgbC (toI (cs U.! (j * nf))) (toI (cs U.! (j * nf + 1))) (toI (cs U.! (j * nf + 2)))) (rectTL px0 py0 44 44)
              , say (fmt 0 (100 * fromIntegral (cnt U.! j) / fromIntegral (U.length lab)) ++ "%") px0 (py0 + 60) 14 (grayC 200) ]
            | j <- [0 .. k - 1], let px0 = 40 + fromIntegral (j `mod` 6) * 52, let py0 = 390 + fromIntegral (j `div` 6) * 66 ]
  ++ [ say ("J (per pixel) = " ++ fmt 1 (wJ w / fromIntegral (max 1 (U.length lab)))) 40 545 15 (grayC 220)
     , say ("flicker = " ++ fmt 2 (100 * wFlicker w) ++ "% (pixels that changed colour)") 40 566 15 (grayC 220)
     , sayWrap (if wPos w == 0 then "w = 0: colour only. The same colour on the left and right is the same cluster"
                else "w = " ++ fmt 1 (wPos w) ++ ": position is used too. A larger w splits the picture into tiles (the idea of superpixels)") 40 590 330 14 (grayC 150)
  -- ---- 文字
     , say ("Clustering camera colours   k = " ++ show k ++ "   position weight w = " ++ fmt 1 (wPos w) ++ (if wLive w then "" else "   (test image)")) 40 40 20 (grayC 240)
     , say "Up/Down k   <- -> position weight w   I re-pick centres   V camera <-> test image   S save" 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    src = wSrc w
    lab = wLab w
    cs = wCs w
    k = U.length cs `div` nf
    big = min ((envW env - 420) / fromIntegral cw) ((envH env - 170) / fromIntegral ch)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) lab)
    pal = U.generate (3 * k) (\q -> clamp8 (cs U.! ((q `div` 3) * nf + q `mod` 3)))   -- 中心の色（Uint8ClampedArray に入れるときと同じ丸め）
    toI v = fromIntegral (clamp8 v) :: Int

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (reinit (min 12 (wK w + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (reinit (max 2 (wK w - 1)))
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (setPositionWeight (min 3 (round1 (wPos w + 0.2))) w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (setPositionWeight (max 0 (round1 (wPos w - 0.2))) w)
  EventKey (Char c) Down _ _
    | c `elem` "iI" -> pure (reinit (wK w))
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> do saveSvg "cluster_camera" env (draw env w); pure w
  _ -> pure w
  where
    reinit k = let (cs, r) = runR (segInit (wFeat w) k) (wRng w) in w { wK = k, wCs = cs, wRng = r }
    round1 v = jsRound (v * 10) / 10        -- +v.toFixed(1)

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("starting from the previous centres, one step at a time (assign + update), J never increases (8 times)", mono)
  , ("updated centre = mean of its pixels (checked on the red component)", cenOk)
  , ("with w = 0 position is ignored: same as the nearest centre by colour (r, g, b) only", colOnly && f0 U.! 3 == 0 && U.last f0 == 0)
  , ("starting from the previous centres flickers less: changed pixels " ++ fmt 1 (100 * fromIntegral chW / nPix) ++ "% < restart " ++ fmt 1 (100 * fromIntegral chC / nPix) ++ "%", chW < chC)
  , ("a larger position weight w groups clusters by place (mean squared offset from the centre's place drops from w = 0.5 to 3)", spread 3 < spread 0.5 && noNaN (U.toList (U.take 10 (fillFeatures (fst (testImage 0 5)) 1))))
  , ("an image with only 2 colours and k = 12: centres with no pixels just stay, no NaN", noNaN (U.toList c12) && U.length c12 `div` nf == 12) ]
  where
    nPix = fromIntegral (cw * ch) :: Double
    -- 1, 2：前の中心から続けても J は増えない
    (img1, r1) = testImage 0 1
    f = fillFeatures img1 0.6
    cs0 = fst (runR (segInit f 6) r1)
    (lb0, j0) = segAssign f cs0
    (mono, csE, lbE, _) = L.foldl' (\(ok, cs, lb, prev) _ -> let cs' = segUpdate f lb cs; (lb', jj) = segAssign f cs' in (ok && not (jj > prev * (1 + 1e-6)), cs', lb', jj)) (True, cs0, lb0, j0) [1 .. 8 :: Int]
    cs2 = segUpdate f lbE csE
    cenOk = and [ abs (s / fromIntegral n - cs2 U.! (j * nf)) <= 1e-3
                | j <- [0 .. U.length csE `div` nf - 1]
                , let mem = [ i | i <- [0 .. U.length lbE - 1], lbE U.! i == j ], let n = length mem, n > 0
                , let s = L.foldl' (\a i -> a + f U.! (i * nf)) 0 mem ]
    -- 3：w = 0 なら色だけ
    (img0, r0) = testImage 0 1
    f0 = fillFeatures img0 0
    c0 = fst (runR (segInit f0 4) r0)
    (l0, _) = segAssign f0 c0
    colOnly = and [ b == l0 U.! i
                  | i <- [0, 37 .. U.length l0 - 1]
                  , let b = fst (L.foldl' (\(bb, bd) j -> let v = sq (f0 U.! (i * nf) - c0 U.! (j * nf)) + sq (f0 U.! (i * nf + 1) - c0 U.! (j * nf + 1)) + sq (f0 U.! (i * nf + 2) - c0 U.! (j * nf + 2))
                                                         in if v < bd then (j, v) else (bb, bd)) (0, 1 / 0) [0 .. U.length c0 `div` nf - 1]) ]
    -- 4：ちらつき（前の中心から vs やり直し）
    fa = fillFeatures (fst (testImage 10 2)) 0.6
    fb = fillFeatures (fst (testImage 11 3)) 0.6
    iter4 ff cs = fst (segAssign ff (L.foldl' (\c _ -> segUpdate ff (fst (segAssign ff c)) c) cs [1 .. 4 :: Int]))
    caFinal = L.foldl' (\c _ -> segUpdate fa (fst (segAssign fa c)) c) (withSeed 4 (segInit fa 6)) [1 .. 4 :: Int]
    la = fst (segAssign fa caFinal)
    lw = fst (segAssign fb (segUpdate fb (fst (segAssign fb caFinal)) caFinal))
    lc = iter4 fb (withSeed 99 (segInit fb 6))
    chW = U.length (U.filter id (U.zipWith (/=) lw la))
    chC = U.length (U.filter id (U.zipWith (/=) lc la))
    -- 5：位置の重み
    spread wt =
      let ff = fillFeatures (fst (testImage 0 5)) wt
          c = L.foldl' (\cc _ -> segUpdate ff (fst (segAssign ff cc)) cc) (withSeed 6 (segInit ff 6)) [1 .. 5 :: Int]
          ll = fst (segAssign ff c)
          s = L.foldl' (\acc i -> let l = ll U.! i
                                      x = fromIntegral (255 * (i `mod` cw)) / fromIntegral cw
                                      y = fromIntegral (255 * (i `div` cw)) / fromIntegral ch
                                  in acc + (if wt > 0 then sq (x - c U.! (l * nf + 3) / wt) + sq (y - c U.! (l * nf + 4) / wt) else 0)) 0 [0 .. U.length ll - 1]
      in s / fromIntegral (U.length ll)
    -- 6：2 色だけの画像で k = 12
    two = fillFeatures (U.generate (cw * ch * 3) (\q -> if odd (q `div` 3) then 200 else 20)) 0
    c12 = L.foldl' (\c _ -> segUpdate two (fst (segAssign two c)) c) (withSeed 7 (segInit two 12)) [1 .. 3 :: Int]
    sq v = v * v

-- ---------- やってみよう ----------
-- 1. ← → で位置の重み w を 0 → 3 に。0 では色だけ、大きいとタイルのような領域に分かれる
-- 2. ↑↓ で k を 2 にすると、画面は何と何に分かれる？（明るい・暗い？ 肌と背景？）
-- 3. I で中心を選び直すと、一瞬ちらついてから落ち着く。毎フレーム I を押すとどうなるか想像しよう
-- 4. 服の色を変えたり手を動かしたりして、クラスタが追いかけてくる様子を見よう
-- 5. S で保存して、自分のポスター風の写真を作ろう。13 の減色と同じことを動画でやっている

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

-- ---- k-means（ロイドのアルゴリズム）。目的関数 = 慣性 = Σ |x_i − 中心|²
nearestCenter :: P -> Pts -> Int
nearestCenter p cs = go 0 0 (1 / 0)
  where
    k = V.length cs
    go !j !b !bd
      | j >= k = b
      | otherwise = let d = sqd p (V.unsafeIndex cs j) in if d < bd then go (j + 1) j d else go (j + 1) b bd

assignLabels :: Pts -> Pts -> U.Vector Int  -- ① 割り当て
assignLabels pts cs = U.generate (V.length pts) (\i -> nearestCenter (V.unsafeIndex pts i) cs)

inertiaOf :: Pts -> Pts -> U.Vector Int -> Double
inertiaOf pts cs labels = U.ifoldl' (\s i l -> s + sqd (pts V.! i) (cs V.! l)) 0 (U.take (V.length pts) labels)

-- ② 更新（仲間の平均。仲間 0 なら動かさない）
updateCenters :: Pts -> U.Vector Int -> Pts -> Pts
updateCenters pts labels cs
  | V.null cs = cs
  | otherwise = V.generate k (\j -> let c = cnt U.! j in if c > 0 then U.generate dim (\d -> sums U.! (j * dim + d) / fromIntegral c) else cs V.! j)
  where
    k = V.length cs
    dim = U.length (V.head cs)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) labels)
    sums = runST $ do
      s <- UM.replicate (k * dim) 0
      forM_ [0 .. V.length pts - 1] $ \i -> do
        let l = labels U.! i
            p = pts V.! i
        forM_ [0 .. min dim (U.length p) - 1] $ \d -> UM.unsafeModify s (+ U.unsafeIndex p d) (l * dim + d)
      U.freeze s

initRandomCenters :: Pts -> Int -> R Pts    -- ランダムに k 点えらぶ（重ならないように）
initRandomCenters pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = go 0 (U.enumFromN 0 n) []
  where
    n = V.length pts
    go j idx out
      | j >= min k n = pure (V.fromList (reverse out ++ replicate (k - length out) (V.head pts)))
      | otherwise = do
          u <- rnd
          let r = j + floor (u * fromIntegral (n - j))
              idx' = idx U.// [(j, idx U.! r), (r, idx U.! j)]
          go (j + 1) idx' (pts V.! (idx' U.! j) : out)

-- k-means++：今ある中心から遠い点ほど（距離² に比例して）えらばれやすい
initPlusPlus :: Pts -> Int -> R Pts
initPlusPlus pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = do
      u <- rnd
      let c0 = pts V.! min (n - 1) (floor (u * fromIntegral n))
      loop [c0] (U.generate n (\i -> sqd (pts V.! i) c0))
  where
    n = V.length pts
    loop out d2
      | length out >= k = pure (V.fromList (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pts V.! pick
          loop (c : out) (U.imap (\i v -> min v (sqd (pts V.! i) c)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

-- k-means の結果。hist = 半ステップ（割り当て・更新）ごとの慣性、all = やり直しごとの慣性
data KM = KM { kmCenters :: !Pts, kmLabels :: !(U.Vector Int), kmInertia :: !Double, kmHist :: ![Double], kmIters :: !Int, kmAll :: ![Double] }

-- 最後まで回す（maxIter 回まで。0 なら 100 回）
lloyd :: Pts -> Pts -> Int -> KM
lloyd pts c0 maxIter = go 0 c0 l0 [inertiaOf pts c0 l0]
  where
    mi = if maxIter <= 0 then 100 else maxIter
    l0 = assignLabels pts c0
    go it cs ls hist
      | it >= mi = finish it cs ls hist
      | otherwise =
          let nc = updateCenters pts ls cs
              h1 = inertiaOf pts nc ls
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              hist' = h2 : h1 : hist
          in if nl == ls then finish (it + 1) nc nl hist' else go (it + 1) nc nl hist'
    finish it cs ls hist = KM cs ls (headOr 0 hist) (reverse hist) it []

-- restarts 回やり直して慣性がいちばん小さいものを選ぶ（usePP = k-means++ で始める）
kmeansBest :: Pts -> Int -> Int -> Bool -> Int -> R KM
kmeansBest pts k restarts usePP maxIter = do
  rs <- forM [1 .. max 1 restarts] $ \_ -> do
    c0 <- if usePP then initPlusPlus pts k else initRandomCenters pts k
    pure (lloyd pts c0 maxIter)
  let best = foldl' (\b r -> if kmInertia r < kmInertia b then r else b) (headOr (KM V.empty U.empty 0 [] 0 []) rs) (drop 1 rs)
  pure best { kmAll = map kmInertia rs }

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
