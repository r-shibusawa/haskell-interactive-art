-- ============================================
-- 08 マンデルブロ集合（どこまでズームしても終わらない）
-- 点ごとに z = z² + c をくり返し、無限に飛んでいくまでの回数で色をつける
-- クリック = その場所に 2 倍ズーム / Z = ズームアウト / R = リセット
-- 1〜3 キーで配色
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word16, Word8)

-- 計算する画像の大きさ（小さいほど速い）
imgW, imgH :: Int
imgW = 356
imgH = 222

rowsPerFrame :: Int
rowsPerFrame = 24   -- ★ 1 フレームに計算する行数（画面を止めないよう、少しずつ計算する）

-- ズームしても計算がくずれないよう、座標はこのスケッチだけ Double を使う
data World = World
  { wCenterX, wCenterY :: !Double         -- 見ている中心（数学の座標）
  , wZoom    :: !Double                   -- 拡大率
  , wMaxIter :: !Int                      -- 最大くり返し回数（増やすと細かくなるが遅くなる）
  , wScheme  :: !Int
  , wPalette :: !(U.Vector (Word8, Word8, Word8))
  , wIters   :: !(U.Vector Word16)        -- ピクセルごとの回数
  , wRow     :: !Int                      -- 次に計算する行
  }

initial :: World
initial = World (-0.6) 0 1 120 1 (makePalette 1 120) (U.replicate (imgW * imgH) 0) 0

main :: IO ()
main = runSketch (sketch "02-08 Mandelbrot" initial)
  { skBack  = black
  , skStep  = \env _ w -> pure (renderRows env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- まだ計算していない行を少しだけ計算する
renderRows :: Env -> World -> World
renderRows env w
  | wRow w >= imgH = w
  | otherwise = w { wIters = U.modify write (wIters w), wRow = end }
  where
    end = min imgH (wRow w + rowsPerFrame)
    write m = forM_ [wRow w .. end - 1] $ \py ->
      forM_ [0 .. imgW - 1] $ \px ->
        MU.unsafeWrite m (py * imgW + px) (fromIntegral (pixelIter env w px py))

-- ピクセル → 数学の座標 c = (cr, ci) → 飛んでいくまでの回数
pixelIter :: Env -> World -> Int -> Int -> Int
pixelIter env w px py = iter (wMaxIter w) cr ci
  where
    ww = realToFrac (envW env) :: Double
    wh = realToFrac (envH env) :: Double
    size = 3.5 / wZoom w / ww     -- 画面に映る数学的な範囲（横幅 3.5 を zoom で割る）
    sx = (fromIntegral px + 0.5) * ww / fromIntegral imgW
    sy = (fromIntegral py + 0.5) * wh / fromIntegral imgH
    cr = wCenterX w + (sx - ww / 2) * size
    ci = wCenterY w + (sy - wh / 2) * size

-- z = 0 からスタートして、z = z² + c をくり返す
iter :: Int -> Double -> Double -> Int
iter maxIter cr ci = go 0 0 0
  where
    go :: Double -> Double -> Int -> Int
    go !zr !zi !n
      | n < maxIter && zr * zr + zi * zi < 4 =
          go (zr * zr - zi * zi + cr) (2 * zr * zi + ci) (n + 1)
      | otherwise = n

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Scale (ww / fromIntegral imgW) (wh / fromIntegral imgH) (pixelImage imgW imgH 1 pixel)
  , Color (withA 0.7 black) (rect 0 (wh / 2 - 22) ww 44)
  , label (-ww / 2 + 16) (wh / 2 - 28) 16 white
      ( "Click: zoom (" ++ show (round (wZoom w) :: Integer) ++ "x)   Z: out   R: reset   1-3: colors   Iterations "
        ++ show (wMaxIter w) )
  ]
  where
    ww = envW env
    wh = envH env
    iters = wIters w
    pal = wPalette w
    top = U.length pal - 1
    -- n 回で飛んでいった → その回数で色をつける（最後まで残った点は黒）
    pixel x y =
      let n = fromIntegral (U.unsafeIndex iters (y * imgW + x)) :: Int
      in U.unsafeIndex pal (min top n)

-- 回数 → 色 の表をあらかじめ作っておく（速くするため）
makePalette :: Int -> Int -> U.Vector (Word8, Word8, Word8)
makePalette scheme maxIter = U.generate (maxIter + 1) entry
  where
    entry n
      | n == maxIter = (0, 0, 0)
      | otherwise = bytes (colorOf n)
    colorOf n =
      let t = fromIntegral n / fromIntegral maxIter :: Float
      in case scheme of        -- ★ 配色の式
           1 -> hsb (wrapF 360 (200 + sqrt t * 300)) 80 (100 * min 1 (t * 6 + 0.2))
           2 -> hsb (30 + t * 40) 90 (sqrt t * 100)
           _ -> hsb 0 0 (fromIntegral (n `mod` 2) * 60 + sqrt t * 40)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) | my <= wh / 2 - 44 ->
    -- クリックした場所を新しい中心にして 2 倍ズーム
    let size = 3.5 / wZoom w / realToFrac ww
        zoom = wZoom w * 2
        maxIter = min 600 (120 + floor (logBase 2 zoom + 1e-9) * 25)  -- 深くズームしたら回数も増やす
    in pure w
         { wCenterX = wCenterX w + realToFrac mx * size
         , wCenterY = wCenterY w - realToFrac my * size    -- y は上下が逆
         , wZoom = zoom
         , wMaxIter = maxIter
         , wPalette = makePalette (wScheme w) maxIter
         , wRow = 0
         }
  EventKey (Char c) Down _ _
    | c == 'z' || c == 'Z' -> pure w { wZoom = max 1 (wZoom w / 2), wRow = 0 }
    | c == 'r' || c == 'R' ->
        pure w { wCenterX = -0.6, wCenterY = 0, wZoom = 1, wMaxIter = 120
               , wPalette = makePalette (wScheme w) 120, wRow = 0 }
    | c >= '1' && c <= '3' ->
        let s = fromEnum c - fromEnum '0'
        in pure w { wScheme = s, wPalette = makePalette s (wMaxIter w) }
    | c == 's' || c == 'S' -> do
        saveSvg "mandelbrot" env (drawWorld env w)
        pure w
  _ -> pure w
  where
    ww = envW env
    wh = envH env

-- ---------- やってみよう ----------
-- 1. 黒い部分のふち（境界）を何回もクリックしてズームしよう。小さなマンデルブロが見つかる！
-- 2. z = z² + c の「2 乗」を「3 乗」にすると別の集合になる
--    実部: zr*zr*zr - 3*zr*zi*zi + cr   虚部: 3*zr*zr*zi - zi*zi*zi + ci
-- 3. 配色の式を変えて、自分だけの色にしよう

-- ---------- 道具 ----------

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
