-- ============================================
-- 15 声で動くジュリア集合（おまけ）
-- 09 の c の値を、マウスではなく声で動かす
-- 声の大きさで c が円の上を回り、集合の形が変わり続ける
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）/ 1〜3 で配色
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

imgW, imgH :: Int
imgW = 240
imgH = 135

maxIter :: Int
maxIter = 50

radius :: Float
radius = 0.7885     -- ★ c の大きさ（この値で円を回ると、きれいに変化する）

data World = World
  { wMic     :: !(Maybe Mic)
  , wLive    :: !Bool
  , wTheta   :: !Float    -- c の角度
  , wVol     :: !Float
  , wPalette :: !(U.Vector (Word8, Word8, Word8))
  }

main :: IO ()
main = runSketch (sketch "02-15 Voice Julia" (World Nothing False 0 0 (makePalette 1)))
  { skBack  = black
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = do
  (level, live) <- case wMic w of
    Nothing -> pure (0, False)
    Just mic -> (,) <$> micLevel mic <*> micLive mic
  let vol = lerpF (wVol w) level 0.2
  -- 声が大きいほど速く回る
  pure w { wVol = vol, wLive = live, wTheta = wrapF tau (wTheta w + 0.003 + vol * 0.6) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( Scale (ww / fromIntegral imgW) (wh / fromIntegral imgH) (pixelImage imgW imgH 1 pixel)
  : Color (withA 0.7 black) (rect 0 (wh / 2 - 22) ww 44)
  : [label (-ww / 2 + 16) (wh / 2 - 28) 16 white message] )
  where
    ww = envW env
    wh = envH env
    -- 声が大きいほど半径が少し大きくなる（形がちぎれる）
    r = radius + wVol w * 0.3
    cr = r * cos (wTheta w)
    ci = r * sin (wTheta w)
    pal = wPalette w
    pixel px py = U.unsafeIndex pal (julia cr ci px py)
    message = case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _ -> "Your voice turns c   c = " ++ nf3 cr ++ " + " ++ nf3 ci ++ "i   1-3: colors"
                  ++ (if wLive w then "" else "   (no mic: demo signal)")

-- この点を z のスタート地点にして、飛んでいくまでの回数を数える
julia :: Float -> Float -> Int -> Int -> Int
julia cr ci px py = go zr0 zi0 0
  where
    zr0 = -1.8 + 3.6 * fromIntegral px / fromIntegral imgW
    zi0 = -1 + 2 * fromIntegral py / fromIntegral imgH
    go :: Float -> Float -> Int -> Int
    go !zr !zi !n
      | n < maxIter && zr * zr + zi * zi < 4 =
          go (zr * zr - zi * zi + cr) (2 * zr * zi + ci) (n + 1)
      | otherwise = n

makePalette :: Int -> U.Vector (Word8, Word8, Word8)
makePalette scheme = U.generate (maxIter + 1) entry
  where
    entry n
      | n == maxIter = (0, 0, 0)
      | otherwise =
          let t = fromIntegral n / fromIntegral maxIter :: Float
          in bytes $ case scheme of
               1 -> hsb (wrapF 360 (320 + t * 160)) 70 (30 + sqrt t * 70)
               2 -> hsb (200 - t * 80) 80 (sqrt t * 100)
               _ -> hsb 0 0 (fromIntegral (n `mod` 2) * 70 + t * 30)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ | Nothing <- wMic w -> do
    mic <- startMic
    pure w { wMic = Just mic }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wPalette = makePalette (fromEnum c - fromEnum '0') }
    | c == 's' || c == 'S' -> do
        saveSvg "julia-sound" env (drawWorld env w)
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. radius を 0.5 や 1.0 にすると？
-- 2. 声の高さ（micSpectrum）で配色を変えてみよう
-- 3. 09 のように、マウスと声の両方で c を動かしてみよう

-- ---------- 道具 ----------

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 小数 3 桁の文字列（p5.js の nf(x, 1, 3)）
nf3 :: Float -> String
nf3 x = showFFloat (Just 3) x ""
