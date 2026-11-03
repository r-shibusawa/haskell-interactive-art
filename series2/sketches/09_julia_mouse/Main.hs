-- ============================================
-- 09 ジュリア集合（マウスで形が変わる）
-- マンデルブロと同じ式 z = z² + c。でも c をマウスで決めて、
-- 全部の点を z のスタート地点にする → マウスを動かすと形がリアルタイムに変わる
-- クリックで止める／動かす  1〜3 で配色
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

-- 計算する画像の大きさ（小さいほど速い）
imgW, imgH :: Int
imgW = 240   -- ★
imgH = 135   -- ★

maxIter :: Int
maxIter = 60 -- ★

data World = World
  { wFrozen  :: !Bool
  , wCr, wCi :: !Float
  , wPalette :: !(U.Vector (Word8, Word8, Word8))
  }

main :: IO ()
main = runSketch (sketch "02-09 Julia Set" (World False (-0.7) 0.27 (makePalette 1)))
  { skBack  = black
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w
  | wFrozen w = w
  | otherwise =
      -- マウスの位置 → c の値（横: -1.5〜1.5, 縦: -1〜1）
      w { wCr = remap mx (-ww / 2) (ww / 2) (-1.5) 1.5
        , wCi = remap (wh / 2 - my) 0 wh (-1) 1 }
  where
    ww = envW env
    wh = envH env
    (mx, my) = envMouse env

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 小さい画像を画面いっぱいに拡大
    Scale (ww / fromIntegral imgW) (wh / fromIntegral imgH) (pixelImage imgW imgH 1 pixel)
  , Color (withA 0.7 black) (rect 0 (wh / 2 - 22) ww 44)
  , label (-ww / 2 + 16) (wh / 2 - 28) 16 white
      ( "c = " ++ nf3 (wCr w) ++ " + " ++ nf3 (wCi w) ++ "i   Mouse: change c   Click: "
        ++ (if wFrozen w then "resume" else "freeze") ++ "   1-3: colors" )
  ]
  where
    ww = envW env
    wh = envH env
    pal = wPalette w
    pixel px py = U.unsafeIndex pal (julia (wCr w) (wCi w) px py)

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
               1 -> hsb (wrapF 360 (280 + t * 200)) 70 (30 + sqrt t * 70)
               2 -> hsb (180 - t * 60) 80 (sqrt t * 100)
               _ -> hsb 0 0 (fromIntegral (n `mod` 2) * 70 + t * 30)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (_, my) | my < envH env / 2 - 44 ->
    pure w { wFrozen = not (wFrozen w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wPalette = makePalette (fromEnum c - fromEnum '0') }
    | c == 's' || c == 'S' -> do
        saveSvg "julia" env (drawWorld env w)
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. マウスをゆっくり動かして、c によって「つながる／ちぎれる」境目を探そう
-- 2. imgW, imgH を 320, 180 にすると細かくなる（重くなったら戻す）
-- 3. maxIter を 20 や 200 にすると？

-- ---------- 道具 ----------

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 小数 3 桁の文字列（p5.js の nf(x, 1, 3)）
nf3 :: Float -> String
nf3 x = showFFloat (Just 3) x ""
