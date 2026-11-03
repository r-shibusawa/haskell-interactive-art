-- ============================================
-- 09 画像フィルタ（2 次元の信号処理）
-- 画像も「信号」。まわりの画素と重みをかけて足す（畳み込み）だけで、
-- ぼかし・輪郭・シャープ・エンボスができる
-- 1〜5 キーでフィルタを切りかえ（カメラが使えないときはデモ映像）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

data World = World
  { wCam   :: !(Maybe Camera)
  , wFrame :: !B.ByteString    -- 最新の映像（R, G, B, R, G, B, ... と並んでいる）
  , wLive  :: !Bool
  , wMode  :: !Int
  }

camW, camH :: Int
camW = 160               -- ★ 小さく処理（速くするため）
camH = 120

-- ★ 3×3 のカーネル（重みの表）
kernelOf :: Int -> (String, [[Float]])
kernelOf mode = case mode of
  2 -> ("Blur (average)", [[1 / 9, 1 / 9, 1 / 9], [1 / 9, 1 / 9, 1 / 9], [1 / 9, 1 / 9, 1 / 9]])
  3 -> ("Edges (Laplacian)", [[-1, -1, -1], [-1, 8, -1], [-1, -1, -1]])
  4 -> ("Sharpen", [[0, -1, 0], [-1, 5, -1], [0, -1, 0]])
  5 -> ("Emboss", [[-2, -1, 0], [-1, 1, 1], [0, 1, 2]])
  _ -> ("Original", [[0, 0, 0], [0, 1, 0], [0, 0, 0]])

main :: IO ()
main = runSketch (sketch "05-09 Image Filter" (World Nothing B.empty False 1))
  { skBack  = gray (20 / 255)
  , skStart = \_ -> do
      cam <- startCamera camW camH
      pure (World (Just cam) B.empty False 1)
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = case wCam w of
  Nothing -> pure w
  Just cam -> do
    frame <- cameraFrame cam
    live <- camLive cam
    pure w { wFrame = frame, wLive = live }

-- ★ 畳み込み：まわり 3×3 の画素 × 重み を足す（ch は 0 = 赤, 1 = 緑, 2 = 青）
--   startCamera の映像はもう左右反転してあるので、横の向きを逆にして p5.js 版と同じ結果にする
convolve :: U.Vector Float -> B.ByteString -> Int -> Int -> Int -> Float
convolve kernel frame x y ch = U.ifoldl' add 0 kernel
  where
    add s n wgt =
      let (j, i) = n `divMod` 3
          idx = ((x - (i - 1)) + (y + j - 1) * camW) * 3 + ch
      in s + wgt * fromIntegral (BU.unsafeIndex frame idx)

-- 結果の画素（端の 1 列は計算できないので黒）
filtered :: Int -> U.Vector Float -> B.ByteString -> Int -> Int -> (Word8, Word8, Word8)
filtered mode kernel frame x y
  | x < 1 || y < 1 || x >= camW - 1 || y >= camH - 1 = (0, 0, 0)
  | otherwise = (out 0, out 1, out 2)
  where
    offset = if mode == 5 then 128 else 0        -- エンボスは灰色を足す
    out ch = round (clampF 0 255 (convolve kernel frame x y ch + offset))

drawWorld :: Env -> World -> Picture
drawWorld env w = case wCam w of
  Just cam | B.length (wFrame w) >= camW * camH * 3 -> Pictures
    [ at (pt env (ww / 2) (100 + fh * scaleF / 2)) result      -- 結果（大）
    , at (pt env (10 + fw / 2) (10 + fh / 2)) (camImage cam (wFrame w) 1)   -- 元の映像（小）
    , text' env (fw + 40) 40 18 white ("1-5: filter   Now: " ++ name)
    , text' env (fw + 40) 70 14 (gray (180 / 255)) ("Kernel: " ++ showKernel rows_)
    , if wLive w then Blank else text' env (fw + 40) 96 14 (gray (180 / 255)) "No camera: demo video"
    ]
  _ -> Blank
  where
    ww = envW env
    h = envH env
    fw = fromIntegral camW :: Float
    fh = fromIntegral camH :: Float
    (name, rows_) = kernelOf (wMode w)
    kernel = U.fromList (concat rows_)
    scaleF = min ((ww - 40) / fw) ((h - 120) / fh)
    result = pixelImage camW camH scaleF (filtered (wMode w) kernel (wFrame w))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '5' -> pure w { wMode = fromEnum c - fromEnum '0' }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 輪郭フィルタの重みを全部足すと 0。だから平らな場所は黒くなる
-- 2. ぼかしは 08 の移動平均の 2 次元版。5×5 にするともっとぼける
-- 3. 自分のカーネルを 6 として追加しよう（例：横の線だけ出す [[-1,-1,-1],[2,2,2],[-1,-1,-1]]）

-- ---------- 道具 ----------

-- カーネルを文字にする（整数はそのまま、小数は 2 桁）
showKernel :: [[Float]] -> String
showKernel rows' = "[" ++ intercalate "," (map showRow rows') ++ "]"
  where
    showRow r = "[" ++ intercalate "," (map showNum r) ++ "]"
    showNum v
      | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = printf "%.2f" v

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標（中央が原点、y は上向き）に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str
