-- ============================================
-- 15 カメラで回折（おまけ）
-- 光（や電子）を穴（開口）に通して遠くのスクリーンで見ると、穴の形の 2 次元フーリエ変換の |·|² が映る（フラウンホーファー回折）
--   カメラの映像を白黒にして「穴」とみなし、2D FFT（128×128）で回折像を計算する。カメラがなければマウスで穴を描く
-- V = カメラ ⇔ 手描き / 1〜5 = 穴の例（スリット・二重スリット・円・格子・正方形） / ドラッグ = 穴を描く（E = 消しゴム） / C = 全部ふさぐ
-- ←→ = カメラのしきい値 / ↑↓ = 回折像の明るさ / I = 白黒反転 / L = 光の色
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)

gN :: Int
gN = 128             -- ★ FFT の大きさ（2 のべき。128 で 60fps）

camH' :: Int
camH' = gN * 3 `div` 4

type Field = U.Vector Float

data World = World
  { wCam     :: !(Maybe Camera)
  , wCamMode :: !Bool
  , wUsing   :: !Bool       -- いまカメラの映像を使っているか
  , wMask    :: !Field      -- 穴（1 = 開いている、0 = ふさがっている）
  , wThr     :: !Float      -- ★ しきい値：画面の平均の明るさ + thr より明るい所 = 穴
  , wGain    :: !Float      -- ★ 回折像の明るさ（対数の強さ）
  , wInvert  :: !Bool
  , wEraser  :: !Bool
  , wTint    :: !Int
  , wDirty   :: !Bool
  , wImgA    :: !Picture
  , wImgF    :: !Picture
  }

main :: IO ()
main = runSketch (sketch "25-15 Webcam Diffraction" world0)
  { skBack  = hsb 220 25 6
  , skStart = \_ -> do
      cam <- startCamera gN camH'
      pure world0 { wCam = Just cam }
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = (preset 2 (World Nothing True False U.empty 25 3 False False 0 True Blank Blank)) { wCamMode = True }   -- カメラが動きだすまでは手描きの穴を表示

tints :: [(Float, Float, Float)]
tints = [(255, 70, 60), (90, 255, 120), (90, 150, 255), (255, 255, 255)]

-- 穴の例を描く（1 = 開いている、0 = ふさがっている）
preset :: Int -> World -> World
preset k w =
  let c = gN `div` 2
      hole dx dy = case k of
        1 -> abs dx < 2 && abs dy < 30
        2 -> abs (abs dx - 8) < 2 && abs dy < 30
        3 -> dx * dx + dy * dy < 64
        4 -> abs dy < 30 && abs dx < 24 && (dx + 64) `mod` 8 < 2
        5 -> abs dx < 6 && abs dy < 6
        _ -> False
      val i = let (y, x) = i `divMod` gN in if hole (x - c) (y - c) then 1 else 0
  in w { wMask = U.generate (gN * gN) val, wCamMode = False, wDirty = True }

-- カメラの画像 → 穴（明るい所 = 1）
maskFromCam :: World -> B.ByteString -> Maybe Field
maskFromCam w f
  | B.length f < gN * camH' * 3 = Nothing
  | otherwise =
      let off = (gN - camH') `div` 2
          bright x y =
            let i = (y * gN + x) * 3
                ch k = fromIntegral (BU.unsafeIndex f (i + k)) :: Float
            in (ch 0 + ch 1 + ch 2) / 3
          -- 平均の明るさを求めて、それより thr だけ明るい所を「穴」にする（部屋の明るさが変わっても使える）
          total = sum [ bright x y | y <- [0 .. camH' - 1], x <- [0 .. gN - 1] ]
          mean = total / fromIntegral (gN * camH')
          val i =
            let (y0, x) = i `divMod` gN
                y = y0 - off
            in if y < 0 || y >= camH' then 0
               else if (bright x y > mean + wThr w) /= wInvert w then 1 else 0
      in Just (U.generate (gN * gN) val)

-- ★ 2 次元 FFT = 横の各行を FFT → 縦の各列を FFT
fft2 :: Field -> (Field, Field)
fft2 mask = runST $ do
  re <- U.thaw mask
  im <- MV.replicate (gN * gN) 0
  rowR <- MV.new gN
  rowI <- MV.new gN
  forM_ [0 .. gN - 1] $ \y -> do
    forM_ [0 .. gN - 1] $ \x -> do
      MV.unsafeRead re (y * gN + x) >>= MV.unsafeWrite rowR x
      MV.unsafeRead im (y * gN + x) >>= MV.unsafeWrite rowI x
    fft False rowR rowI
    forM_ [0 .. gN - 1] $ \x -> do
      MV.unsafeRead rowR x >>= MV.unsafeWrite re (y * gN + x)
      MV.unsafeRead rowI x >>= MV.unsafeWrite im (y * gN + x)
  forM_ [0 .. gN - 1] $ \x -> do
    forM_ [0 .. gN - 1] $ \y -> do
      MV.unsafeRead re (y * gN + x) >>= MV.unsafeWrite rowR y
      MV.unsafeRead im (y * gN + x) >>= MV.unsafeWrite rowI y
    fft False rowR rowI
    forM_ [0 .. gN - 1] $ \y -> do
      MV.unsafeRead rowR y >>= MV.unsafeWrite re (y * gN + x)
      MV.unsafeRead rowI y >>= MV.unsafeWrite im (y * gN + x)
  (,) <$> U.unsafeFreeze re <*> U.unsafeFreeze im

render :: World -> World
render w =
  let (re, im) = fft2 (wMask w)
      power = U.zipWith (\a b -> a * a + b * b) re im
      imax = U.foldl' max 1e-12 power
      (tr, tg, tb) = case drop (wTint w) tints of
        (t : _) -> t
        [] -> (255, 255, 255)
      big = 10 ** wGain w
      imgA = pixelImage gN gN 1 $ \x y ->
        let a = byte (wMask w `U.unsafeIndex` (y * gN + x) * 255) in (a, a, a)
      imgF = pixelImage gN gN 1 $ \x y ->
        -- 真ん中が周波数 0 になるよう半分ずらす（fftshift）
        let sx = (x + gN `div` 2) `mod` gN
            sy = (y + gN `div` 2) `mod` gN
            i = power `U.unsafeIndex` (sy * gN + sx) / imax
            b = clampF 0 1 (log (1 + i * big) / log (1 + big))   -- 対数で暗い縞も見えるように
        in (byte (tr * b), byte (tg * b), byte (tb * b))
  in w { wImgA = imgA, wImgF = imgF, wDirty = False }

byte :: Float -> Word8
byte v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- 画面の配置
sizeS :: Env -> Float
sizeS env = min ((envW env - 120) / 2) (envH env - 190)

ax0, ay0 :: Float
ax0 = 40
ay0 = 120

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  live <- maybe (pure False) camLive (wCam w0)
  let using = wCamMode w0 && live
  w1 <- case wCam w0 of
    Just cam | using -> do
      f <- cameraFrame cam
      pure (case maskFromCam w0 f of
              Just m -> w0 { wMask = m, wDirty = True }
              Nothing -> w0)
    _ -> pure w0
  -- マウスで穴を描く
  let (mx, my) = mouseP env
      s = sizeS env
      inside = mx > ax0 && mx < ax0 + s && my > ay0 && my < ay0 + s
      w2 | envDown env && not using && inside =
             let cx = floor ((mx - ax0) / s * fromIntegral gN) :: Int
                 cy = floor ((my - ay0) / s * fromIntegral gN) :: Int
                 val = if wEraser w1 then 0 else 1
                 spots = [ (y * gN + x, val)
                         | y <- [cy - 2 .. cy + 2], x <- [cx - 2 .. cx + 2]
                         , x >= 0, y >= 0, x < gN, y < gN
                         , (x - cx) * (x - cx) + (y - cy) * (y - cy) <= 5 ]
             in w1 { wMask = wMask w1 U.// spots, wDirty = True }
         | otherwise = w1
      w3 = w2 { wUsing = using }
  pure (if wDirty w3 then render w3 else w3)

drawW :: Env -> World -> Picture
drawW env w =
  let s = sizeS env
      fx = ax0 + s + 40
      k = s / fromIntegral gN
      placed x img = at (pt env (x + s / 2) (ay0 + s / 2)) (Scale k k img)
      apNote | wUsing w = ": bright parts of the camera"
             | wCamMode w = ": waiting for camera (hand drawn)"
             | otherwise = ": hand drawn"
  in Pictures
       [ placed ax0 (wImgA w)
       , placed fx (wImgF w)
       , frameBox env (gray 0.3) ax0 ay0 s s
       , frameBox env (gray 0.3) fx ay0 s s
       , txt env ax0 (ay0 - 12) 16 white ("Aperture" ++ apNote)
       , txt env fx (ay0 - 12) 14 white "Diffraction |2D FFT|^2 (log; center = straight on)"
       , txt env 20 26 16 white ("V: " ++ (if wCamMode w then "camera" else "hand drawn") ++ "   1-5: apertures   Drag: draw (E: " ++ (if wEraser w then "eraser" else "pen") ++ ")   C: close all")
       , txt env 20 48 16 white ("Left/Right: threshold mean+" ++ show (round (wThr w) :: Int) ++ "   Up/Down: brightness " ++ show (round (wGain w) :: Int) ++ "   I: invert   L: color")
       , txt env 20 70 14 (gray 0.6) "Narrow holes spread wide (like dx dp). Double slit: fringes. Circle: rings (Airy). Grating: dots."
       ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> preset (fromEnum c - fromEnum '0') w
    | otherwise -> case toLower c of
        'v' -> w { wCamMode = not (wCamMode w), wDirty = True }
        'e' -> w { wEraser = not (wEraser w) }
        'c' -> w { wMask = U.replicate (gN * gN) 0, wCamMode = False, wDirty = True }
        'i' -> w { wInvert = not (wInvert w), wDirty = True }
        'l' -> w { wTint = (wTint w + 1) `mod` length tints, wDirty = True }
        _ -> w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wThr = min 200 (wThr w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wThr = max (-100) (wThr w - 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wGain = min 8 (wGain w + 1), wDirty = True }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wGain = max 0 (wGain w - 1), wDirty = True }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（スリット）を手描きで太くしたり細くしたりして、回折像の横の広がりがどう変わるか観察しよう
-- 2. 2 の二重スリットの間隔を広げると縞が細かくなる（00 の λL/d と同じ）
-- 3. カメラの前で指を 2 本立てる・手を丸くする。どんな回折像になる？（暗い部屋なら I で反転）
-- 4. 4 の格子の間隔（mod 8）を変えると点の間隔はどうなる？ これが結晶の X 線回折で原子の並びを調べる仕組み

-- ==================================================
-- ここから下は「道具」（複素数の FFT：高速フーリエ変換）
-- ==================================================
twC, twS :: Field
twC = U.generate (gN `div` 2) (\k -> cos (2 * pi * fromIntegral k / fromIntegral gN))
twS = U.generate (gN `div` 2) (\k -> sin (2 * pi * fromIntegral k / fromIntegral gN))

-- re, im：長さ gN（2 のべき）の配列をその場で変換する。inv = True で逆変換（1/n で割る）
fft :: Bool -> MV.STVector s Float -> MV.STVector s Float -> ST s ()
fft inv re im = swaps 1 0 >> stage 2 >> when inv (divide 0)
  where
    n = gN
    next !j !bit | j .&. bit /= 0 = next (j `xor` bit) (bit `shiftR` 1)
                 | otherwise = j `xor` bit
    swaps !i !j                                     -- ビット反転の並べ替え
      | i >= n = pure ()
      | otherwise = do
          let j' = next j (n `shiftR` 1)
          when (i < j') (MV.unsafeSwap re i j' >> MV.unsafeSwap im i j')
          swaps (i + 1) j'
    stage !len                                      -- バタフライ演算
      | len > n = pure ()
      | otherwise = block len 0 >> stage (len * 2)
    block !len !i
      | i >= n = pure ()
      | otherwise = wing len i 0 >> block len (i + len)
    wing !len !i !k
      | k >= len `shiftR` 1 = pure ()
      | otherwise = do
          let a = i + k
              b = a + len `shiftR` 1
              t = k * (n `div` len)
              wr = twC `U.unsafeIndex` t
              wi = if inv then twS `U.unsafeIndex` t else negate (twS `U.unsafeIndex` t)
          rb <- MV.unsafeRead re b
          ib <- MV.unsafeRead im b
          ra <- MV.unsafeRead re a
          ia <- MV.unsafeRead im a
          let xr = rb * wr - ib * wi
              xi = rb * wi + ib * wr
          MV.unsafeWrite re b (ra - xr)
          MV.unsafeWrite im b (ia - xi)
          MV.unsafeWrite re a (ra + xr)
          MV.unsafeWrite im a (ia + xi)
          wing len i (k + 1)
    scaleBy = 1 / fromIntegral n :: Float
    divide !i
      | i >= n = pure ()
      | otherwise = MV.unsafeModify re (* scaleBy) i >> MV.unsafeModify im (* scaleBy) i >> divide (i + 1)

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)
