-- ============================================
-- 65-15 カメラ映像のマルコフ模様 📷 ─ 映像の色を 8 色に減らし（k 平均法）、
--   「この色の右どなりは何色か」（横の遷移）と「この色の下は何色か」（縦の遷移）を数えて 2 つの遷移行列にする。
-- 左上から 1 マスずつ、左と上の色から次の色をサイコロで決めていくと、元の映像の「色の並び方の雰囲気」を持つ新しい模様になる。
-- 左と上の両方を使うときは P(c | 左 a, 上 b) ∝ 横[a][c] × (縦[b][c] / π_c)^w で近似する（w = 縦の強さ）
-- 操作：スペース 新しい模様 / ↑↓ 縦の強さ w / C カメラ ⇔ テスト画像 / F 学習を止める / S 画面を保存（SVG）
-- カメラが使えないときは、自動でテスト画像（空・山・しま模様）を使う
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch :: Int
cw = 64                                       -- ★ 学習に使う映像の大きさ（小さいほど速い）
ch = 48

gw, gh :: Int
gw = 96                                       -- ★ 作る模様のマスの数
gh = 72

nCol :: Int
nCol = 8                                      -- 色の数

rowsPerFrame :: Int
rowsPerFrame = 4                              -- ★ 1 フレームに作る行の数（ゆっくり描かれる様子が見える）

type RGBd = (Double, Double, Double)
type Pix = U.Vector Int                       -- 画素：r, g, b, r, g, b, …（左上から、1 画素 3 つ）

data World = World
  { wCam     :: Maybe Camera
  , wUseCam  :: !Bool
  , wLive    :: !Bool                         -- いまカメラの映像を使っているか
  , wFrozen  :: !Bool
  , wPx      :: !Pix                          -- いまの映像
  , wPal     :: [RGBd]                        -- 8 色（明るさの順）
  , wSrcLab  :: !(U.Vector Int)               -- 映像の画素ごとの色の番号
  , wHorT    :: Mat                           -- 横の遷移行列
  , wVertT   :: Mat                           -- 縦の遷移行列
  , wFreq    :: Vec                           -- 色の割合
  , wGenLab  :: !(U.Vector Int)               -- 作っている模様（色の番号）
  , wGenPix  :: !(U.Vector Word8)             -- 画面に出す模様の画素（p5.js の genImg と同じく、まだ作っていない行は前の模様が残る）
  , wGenRow  :: !Int
  , wWait    :: !Int
  , wVMix    :: !Double                       -- ★ 縦の強さ w（0 = 横だけ → 横じま、1 = 横と縦）
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-15 Webcam Markov texture" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch                -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 =
  let w0 = World Nothing True False False testPix (initPalette testPix) U.empty [] [] [] U.empty
                 (U.concat (replicate (gw * gh) (U.fromList [32, 28, 26]))) 0 0 1 (rngOf 15) []
  in startPattern (learnFrom testPix w0)

-- テスト画像：空のグラデーション、太陽、山、しま模様の畑
testPix :: Pix
testPix = makeTestPixels cw ch

makeTestPixels :: Int -> Int -> Pix
makeTestPixels w h = U.fromList (concat [ pxAt x y | y <- [0 .. h - 1], x <- [0 .. w - 1] ])
  where
    fw = i2d w
    fh = i2d h
    pxAt x y =
      let xd = i2d x
          yd = i2d y
          ridge = fh * 0.55 - 10 * sin (xd * 0.12) - 6 * sin (xd * 0.31)
          base = [120 + y * 2, 170 + y, 235]
          sun = if sqrt ((xd - fw * 0.75) ^ (2 :: Int) + (yd - fh * 0.22) ^ (2 :: Int)) < fh * 0.12 then [250, 200, 70] else base
          hill = if yd > ridge then [70, 110, 80] else sun
          s = (floor ((xd + (yd - fh * 0.72) * 0.8) / 5) :: Int) `mod` 2
      in if yd > fh * 0.72 then (if s == 1 then [200, 160, 70] else [140, 90, 50]) else hill

nPix :: Pix -> Int
nPix px = U.length px `div` 3

initPalette :: Pix -> [RGBd]
initPalette px = [ pixRGB px (floor ((i2d k + 0.5) / i2d nCol * i2d (nPix px))) | k <- [0 .. nCol - 1] ]

pixRGB :: Pix -> Int -> RGBd
pixRGB px i = (i2d (px U.! (3 * i)), i2d (px U.! (3 * i + 1)), i2d (px U.! (3 * i + 2)))

nearestCol :: [RGBd] -> Double -> Double -> Double -> Int
nearestCol pal r g b = fst (foldl' pick (0, posInf) (zip [0 ..] pal))
  where pick (bk, bd) (k, (pr, pg, pb)) = let d = (pr - r) ^ (2 :: Int) + (pg - g) ^ (2 :: Int) + (pb - b) ^ (2 :: Int) in if d < bd then (k, d) else (bk, bd)

labelsOf :: [RGBd] -> Pix -> U.Vector Int
labelsOf pal px = U.generate (nPix px) (\i -> let (r, g, b) = pixRGB px i in nearestCol pal r g b)

-- ★ k 平均法：各画素を一番近い色に分け、色をその平均に動かす（を its 回）。最後に明るさの順に並べる
kmeansStep :: [RGBd] -> Pix -> Int -> R ([RGBd], U.Vector Int)
kmeansStep pal0 px its = do
  let n = nPix px
      oneIt pal = do
        let lab = labelsOf pal px
            sums = U.accum (+) (U.replicate (4 * nCol) 0) (concat [ [(4 * k, r), (4 * k + 1, g), (4 * k + 2, b), (4 * k + 3, 1)] | i <- [0 .. n - 1], let k = lab U.! i, let (r, g, b) = pixRGB px i ]) :: U.Vector Double
        mapM (\k -> let c = sums U.! (4 * k + 3) in
                      if c > 0 then pure (sums U.! (4 * k) / c, sums U.! (4 * k + 1) / c, sums U.! (4 * k + 2) / c)
                      else do u <- rnd; pure (pixRGB px (floor (u * i2d n))))   -- 使われない色は、映像のどこかの画素の色にし直す
             [0 .. nCol - 1]
  pal1 <- loopR its oneIt pal0
  let pal = sortOn (\(r, g, b) -> r + g + b) pal1
  pure (pal, labelsOf pal px)

-- ★ 遷移を数える：横 = (x, y) → (x+1, y)、縦 = (x, y) → (x, y+1)。画像は輪っかとして数える
--   → 下のほうの色からも上のほうの色へ戻れる（閉じたクラスに閉じこめられない、03 を参照）。数が 0 の行は全部同じ確率に
data Trans = Trans { tHc :: Mat, tVc :: Mat, tHorP :: Mat, tVertP :: Mat, tFreq :: Vec }

countTransitions :: U.Vector Int -> Int -> Int -> Trans
countTransitions lab w h =
  let cells = [ (y * w + x, x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1] ]
      hcv = U.accum (+) (U.replicate (nCol * nCol) 0) [ (a * nCol + lab U.! (y * w + (x + 1) `mod` w), 1) | (i, x, y) <- cells, let a = lab U.! i ] :: U.Vector Double
      vcv = U.accum (+) (U.replicate (nCol * nCol) 0) [ (a * nCol + lab U.! (((y + 1) `mod` h) * w + x), 1) | (i, x, y) <- cells, let a = lab U.! i ] :: U.Vector Double
      frv = U.accum (+) (U.replicate nCol 0) [ (lab U.! i, 1) | (i, _, _) <- cells ] :: U.Vector Double
      toM v = [ [ v U.! (i * nCol + j) | j <- [0 .. nCol - 1] ] | i <- [0 .. nCol - 1] ]
      toP c = [ let s = sumOf r in if s > 0 then map (/ s) r else replicate nCol (1 / i2d nCol) | r <- c ]
      hc = toM hcv
      vc = toM vcv
  in Trans hc vc (toP hc) (toP vc) [ v / i2d (w * h) | v <- U.toList frv ]

learnFrom :: Pix -> World -> World
learnFrom px w =
  let ((pal, lab), g) = runR (kmeansStep (wPal w) px 3) (wRng w)
      t = countTransitions lab cw ch
  in w { wPal = pal, wSrcLab = lab, wHorT = tHorP t, wVertT = tVertP t, wFreq = tFreq t, wRng = g }

-- ★ 1 マスの色：左 a と上 b（ないときは −1）から
nextColor :: Mat -> Mat -> Vec -> Int -> Int -> Double -> R Int
nextColor hp vp fr a b w =
  let wts = [ if a < 0 && b < 0 then frc
              else if b < 0 then at' (at' hp a []) c 0
              else if a < 0 then at' (at' vp b []) c 0
              else at' (at' hp a []) c 0 * (at' (at' vp b []) c 0 / max frc 1e-9) ** w
            | c <- [0 .. nCol - 1], let frc = at' fr c 0 ]
  in if not (sumOf wts > 0)
       then (if a >= 0 then sampleFrom (at' hp a []) else sampleFrom (map (+ 1e-9) fr))   -- 両方と合わないときは横だけで
       else sampleFrom wts

-- 行 y0〜y1−1 を作る（幅 gwid の模様 lab に書きこむ）
generateRows :: U.Vector Int -> Int -> Int -> Int -> Mat -> Mat -> Vec -> Double -> R (U.Vector Int)
generateRows lab0 gwid y0 y1 hp vp fr w = loopRow y0 lab0
  where
    loopRow y !lab
      | y >= y1 = pure lab
      | otherwise = do
          let cellAt x left = nextColor hp vp fr left (if y > 0 then lab U.! ((y - 1) * gwid + x) else -1) w
              goX x left acc
                | x >= gwid = pure (reverse acc)
                | otherwise = do c <- cellAt x left; goX (x + 1) c (c : acc)
          row <- goX 0 (-1) []
          loopRow (y + 1) (lab U.// zip [y * gwid ..] row)

startPattern :: World -> World
startPattern w = w { wGenLab = U.replicate (gw * gh) 0, wGenRow = 0, wWait = 0 }

-- 色の番号 → 画素（行 0〜rows−1 をいまの 8 色で塗り直す。ほかの行は前のまま）
paintGen :: World -> World
paintGen w =
  let n = gw * wGenRow w
      pal = U.fromList (concat [ [q r, q g, q b] | (r, g, b) <- wPal w ]) :: U.Vector Word8
      upd = [ (3 * i + c, pal U.! (3 * (wGenLab w U.! i) + c)) | i <- [0 .. n - 1], c <- [0, 1, 2] ]
  in w { wGenPix = wGenPix w U.// upd }

q :: Double -> Word8                          -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）
q v = fromIntegral (max 0 (min 255 (round (fin 0 v) :: Int)))

-- カメラのフレーム（RGB のバイト列）→ 画素
camPix :: B.ByteString -> Pix
camPix bs = U.generate (cw * ch * 3) (\i -> fromIntegral (B.index bs i))

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  (live, px) <- case wCam w0 of
    Just cam | wUseCam w0 -> do
      ok <- camLive cam
      if ok then do
        f <- cameraFrame cam
        pure (if B.length f >= cw * ch * 3 then (True, camPix f) else (True, testPix))
      else pure (False, testPix)
    _ -> pure (False, testPix)
  let w1 = w0 { wLive = live, wPx = px }
      w2 = if not (wFrozen w1) && envFrame env `mod` 20 == 0 then learnFrom px w1 else w1
      w3 = if wGenRow w2 < gh
             then let y1 = min gh (wGenRow w2 + rowsPerFrame)
                      (lab, g) = runR (generateRows (wGenLab w2) gw (wGenRow w2) y1 (wHorT w2) (wVertT w2) (wFreq w2) (wVMix w2)) (wRng w2)
                  in w2 { wGenLab = lab, wGenRow = y1, wRng = g }
             else if wWait w2 + 1 > 180 then startPattern w2 else w2 { wWait = wWait w2 + 1 }   -- 3 秒ながめたら次の模様
  pure (paintGen w3)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Webcam Markov texture: learning from the " ++ (if wLive w then "camera" else "test image") ++ (if wFrozen w then " (learning stopped)" else ""))
             ("Space new pattern   Up/Down vertical strength w = " ++ fmtN 1 (wVMix w) ++ "   C camera <-> test image   F stop learning   S save")
  -- ---- 元の映像と 8 色にした映像
  , imageBox x0 y0 sw sh cw ch (pixelImage cw ch 1 (\x y -> let i = 3 * (y * cw + x) in (w8 (px U.! i), w8 (px U.! (i + 1)), w8 (px U.! (i + 2)))))
  , imageBox x0 (y0 + sh + 30) sw sh cw ch (pixelImage cw ch 1 (\x y -> palQ (wSrcLab w U.! (y * cw + x))))
  , say "Original video" x0 (y0 - 6) 14 (col sub)
  , say "Reduced to 8 colours" x0 (y0 + sh + 24) 14 (col sub)
  , Pictures [ Color (rgbCol c) (rectTL (x0 + i2f k * (sw / 8)) (y0 + 2 * sh + 40) (sw / 8 - 3) 18) | (k, c) <- zip [0 :: Int ..] (wPal w) ]
  , say "8 colours, share pi in %" x0 (y0 + 2 * sh + 76) 14 (col sub)
  , Pictures [ Pictures [ Color (rgbCol c) (rectTL (x0 + i2f k * (sw / 8)) (y0 + 2 * sh + 86 + 70 - hh) (sw / 8 - 3) hh)
                        , say (show (jsRoundI (f * 100))) (x0 + i2f k * (sw / 8) + 2) (y0 + 2 * sh + 174) 14 (rgbCol c) ]
             | (k, c, f) <- zip3 [0 :: Int ..] (wPal w) (wFreq w), let hh = f2 (clampv (f / maxOr 1 (wFreq w)) 0 1) * 70 ]
  -- ---- 作った模様
  , imageBox gx y0 gwid ghei gw gh (pixelImage gw gh 1 (\x y -> let i = 3 * (y * gw + x) in (wGenPix w U.! i, wGenPix w U.! (i + 1), wGenPix w U.! (i + 2))))
  , say ("Markov chain pattern (" ++ show gw ++ " x " ++ show gh ++ " cells, from top left)") gx (y0 - 6) 14 (col sub)
  , if wGenRow w < gh then Color (col cream) (lineW 1 (gx, y0 + i2f (wGenRow w) / i2f gh * ghei) (gx + gwid, y0 + i2f (wGenRow w) / i2f gh * ghei)) else Blank
  -- ---- 遷移行列のヒートマップ（横と縦）
  , heatmap (wPal w) (wHorT w) hx (y0 + 20) cell "Horizontal: left -> right"
  , heatmap (wPal w) (wVertT w) hx (y0 + 60 + 8 * cell + 30) cell "Vertical: top -> bottom"
  , say "Bright diagonal = same colour" hx ty 14 (col sub)
  , say "continues" hx (ty + 18) 14 (col sub)
  , say "w = 0: no vertical link," hx (ty + 42) 14 (col sub)
  , say "so horizontal stripes" hx (ty + 60) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    px = wPx w
    w8 v = fromIntegral (max 0 (min 255 v)) :: Word8
    palQ k = let (r, g, b) = at' (wPal w) k (0, 0, 0) in (q r, q g, q b)
    sw = min 256 (envW env * 0.2)
    sh = sw * i2f ch / i2f cw
    x0 = 40
    y0 = 104
    gx = x0 + sw + 40
    gwid = min (envW env * 0.42) ((envH env - 130) * i2f gw / i2f gh)
    ghei = gwid * i2f gh / i2f gw
    hx = gx + gwid + 40
    cell = max 10 (min 22 ((envW env - hx - 50) / 8))
    ty = y0 + 2 * (8 * cell) + 150

rgbCol :: RGBd -> Color
rgbCol (r, g, b) = col (realToFrac r, realToFrac g, realToFrac b)

-- 行 = 今の色、列 = 次の色（白いほど確率が大きい）
heatmap :: [RGBd] -> Mat -> Float -> Float -> Float -> String -> Picture
heatmap pal m x y c title = Pictures $
  say title x (y - 8) 14 (col cream)
  : concat [ [ Color (rgbCol pc) (rectTL x (y + 14 + i2f i * c) (c - 2) (c - 2)), Color (rgbCol pc) (rectTL (x + 14 + i2f (i + 1) * c) y (c - 2) (c - 2 - 6)) ]
             ++ [ Color (col (40 + 210 * v, 36 + 200 * v, 34 + 180 * v)) (rectTL (x + 14 + i2f (j + 1) * c) (y + 14 + i2f i * c) (c - 2) (c - 2))
                | j <- [0 .. nCol - 1], let v = f2 (sqrt (max 0 (at' (at' m i []) j 0))) ]
           | i <- [0 .. nCol - 1], let pc = at' pal i (0, 0, 0) ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (startPattern w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (startPattern w { wVMix = min 2 (round1 (wVMix w + 0.2)) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (startPattern w { wVMix = max 0 (round1 (wVMix w - 0.2)) })
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wUseCam = not (wUseCam w) }
    | c == 'f' || c == 'F' -> pure w { wFrozen = not (wFrozen w) }
    | c == 's' || c == 'S' -> do saveSvg "markov_texture" env (draw env w); pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. しま模様のシャツや本棚をカメラに映すと、模様はどう変わる？ F で学習を止めてじっくり見よう
-- 2. w を 0（横だけ）にすると、なぜ横じまになる？ 2 にすると？
-- 3. nCol を 4 や 16 に変えると、模様の細かさはどう変わる？
-- 4. 遷移を「右どなり」ではなく「2 つ右」で数えると？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest = withSeed 150 $ do
  let px = testPix
      n = cw * ch
  (pal, lab) <- kmeansStep (initPalette px) px 6
  let okN = and [ lab U.! i == (let (r, g, b) = pixRGB px i in nearestCol pal r g b) | i <- [0 .. n - 1] ]
      t = countTransitions lab cw ch
      -- しま模様（横に 0, 1, 2 のくり返し、縦は同じ）→ 模様をそのまま作り直す
      sw = 30
      sh = 20
      sl = U.fromList [ x `mod` 3 | _y <- [0 .. sh - 1], x <- [0 .. sw - 1 :: Int] ]
      ts = countTransitions sl sw sh
      hP i j = at' (at' (tHorP ts) i []) j 0
      vP i j = at' (at' (tVertP ts) i []) j 0
  gl <- generateRows (U.replicate (sw * sh) 0) sw 0 sh (tHorP ts) (tVertP ts) (tFreq ts) 1
  let okS = and [ not (gl U.! (y * sw + x) /= gl U.! x || (x > 0 && gl U.! x /= (gl U.! (x - 1) + 1) `mod` 3)) | y <- [1 .. sh - 1], x <- [0 .. sw - 1] ]
  -- 横の連鎖だけで長い列を作ると、遷移の割合 ≈ 横の遷移行列
  cnt <- let go :: Int -> Int -> U.Vector Int -> R (U.Vector Int)
             go 0 _ c = pure c
             go k cur !c = do nx <- sampleFrom (at' (tHorP t) cur []); go (k - 1) nx (U.accum (+) c [(cur * nCol + nx, 1)])
         in go 60000 0 (U.replicate (nCol * nCol) 0)
  let worst = foldl' max 0 [ abs (i2d (cnt U.! (i * nCol + j)) / s - at' (at' (tHorP t) i []) j 0)
                           | i <- [0 .. nCol - 1], let s = i2d (sum [ cnt U.! (i * nCol + j2) | j2 <- [0 .. nCol - 1] ]), s > 2000, j <- [0 .. nCol - 1] ]
  -- 模様の中のとなりどうしは、元の映像で起きたつながりだけ（確率 0 のつながりは作らない）
  g0 <- generateRows (U.replicate (gw * gh) 0) gw 0 gh (tHorP t) (tVertP t) (tFreq t) 0
  g2 <- generateRows (U.replicate (gw * gh) 0) gw 0 gh (tHorP t) (tVertP t) (tFreq t) 1
  let pairStats g =
        let hs = [ at' (at' (tHorP t) (g U.! (y * gw + x - 1)) []) (g U.! (y * gw + x)) 0 > 0 | y <- [0 .. gh - 1], x <- [1 .. gw - 1] ]
            vs = [ at' (at' (tVertP t) (g U.! ((y - 1) * gw + x)) []) (g U.! (y * gw + x)) 0 > 0 | y <- [1 .. gh - 1], x <- [0 .. gw - 1] ]
            frac bs = i2d (length (filter id bs)) / i2d (length bs)
        in (frac hs, frac vs)
      (s0h, s0v) = pairStats g0
      (s2h, s2v) = pairStats g2
      one = U.replicate 100 3
      t1 = countTransitions one 10 10
  g1 <- generateRows (U.replicate 100 0) 10 0 10 (tHorP t1) (tVertP t1) (tFreq t1) 1
  pure
    [ ("8 色に減らす：どの画素も一番近い色に分けられている", okN && length pal == nCol)
    , ("数えた数（輪っか）：横も縦も " ++ show n ++ " 組", totalOf (tHc t) == i2d n && totalOf (tVc t) == i2d n)
    , ("輪っかなので、色の割合は横の連鎖の定常分布：π 横 = π", maxAbsDiffV (vecMat (tFreq t) (tHorP t)) (tFreq t) < 1e-12 && maxAbsDiffV (vecMat (tFreq t) (tVertP t)) (tFreq t) < 1e-12)
    , ("横・縦の遷移行列の各行の合計 = 1、色の割合の合計 = 1", rowSumErr (tHorP t) 1 < 1e-12 && rowSumErr (tVertP t) 1 < 1e-12 && near (sumOf (tFreq t)) 1 1e-12)
    , ("しま模様：横は 0→1→2→0 が確率 1、縦は同じ色が確率 1", hP 0 1 == 1 && hP 1 2 == 1 && hP 2 0 == 1 && all (\k -> vP k k == 1) [0, 1, 2])
    , ("しま模様から作った模様は、きちんと同じしま模様", okS)
    , ("横の連鎖で 6 万歩：つながりの割合 ≈ 横の遷移行列（ずれ < 0.02）", worst < 0.02)
    , ("w = 0 と 1：横のとなりは全部、元の映像にあるつながり", s0h == 1 && s2h == 1)
    , ("w = 1：縦のとなりも " ++ show (jsRoundI (s2v * 100)) ++ "% が元の映像にあるつながり（w = 0 は " ++ show (jsRoundI (s0v * 100)) ++ "%）", s2v > 0.95 && s2v >= s0v)
    , ("1 色だけの映像でも NaN にならず、模様も 1 色", noNaN (concat (tHorP t1)) && noNaN (concat (tVertP t1)) && U.all (== 3) g1) ]

totalOf :: Mat -> Double
totalOf m = sumOf (map sumOf m)

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

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

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
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

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
