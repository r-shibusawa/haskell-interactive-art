-- ============================================
-- 09 XOR 暗号（鍵の良し悪しが絵でわかる）
-- 画像の各ビットを「鍵」と XOR すると暗号になる。もう一度 XOR すると元に戻る
-- 鍵が短くてくり返しだと、絵の形が透けて見える（有名な「ECB ペンギン」問題）
-- 1 = 短い鍵 / 2 = 長い鍵 / 3 = ランダムな鍵（ワンタイムパッド） / クリックで復号⇄暗号
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (xor)
import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

data World = World
  { wKeyMode :: !Int
  , wShowDec :: !Bool             -- 復号を見せるか
  , wKey     :: !(U.Vector Int)   -- 鍵（絵と同じ長さ）
  }

imgW, imgH :: Int
imgW = 64
imgH = 48

main :: IO ()
main = runSketch (sketch "06-09 XOR Cipher" (World 1 False (repeatKey shortPattern)))
  { skBack  = background
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 元の絵：ハート
plain :: U.Vector Int
plain = U.generate (imgW * imgH) $ \i ->
  let x = fromIntegral (i `mod` imgW) :: Float
      y = fromIntegral (i `div` imgW) :: Float
      nx = (x - fromIntegral imgW / 2) / 14
      ny = (fromIntegral imgH / 2 + 2 - y) / 14   -- 画面の y は下向きなので、符号を逆にして上下を合わせる
      heart = (nx * nx + ny * ny - 1) ** 3 - nx * nx * ny * ny * ny < 0
  in if heart then 1 else 0

shortPattern :: [Int]
shortPattern = [1, 0, 1, 1, 0, 0, 1, 0]   -- ★ 8 ビットをくり返す

-- パターンを絵の長さまでくり返す
repeatKey :: [Int] -> U.Vector Int
repeatKey pat = U.fromList (take (imgW * imgH) (cycle pat))

-- ★ 鍵をつくる
makeKey :: Int -> IO (U.Vector Int)
makeKey 1 = pure (repeatKey shortPattern)
makeKey 2 = repeatKey <$> randomBits 100           -- 100 ビットをくり返す
makeKey _ = U.fromList <$> randomBits (imgW * imgH) -- 絵と同じ長さの乱数

randomBits :: Int -> IO [Int]
randomBits n = replicateM n (randomRIO (0, 1))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawPanels env cell [plain, key, if wShowDec w then decrypted else cipher]
  , title 0 "Original picture"
  , title 1 "Key"
  , title 2 (if wShowDec w then "Decrypted (XOR with key again)" else "Ciphertext (picture XOR key)")
  , txt env 20 40 18 (hsb 0 0 100)
      ("Key: " ++ keyName ++ " (1-3)  Click: show " ++ (if wShowDec w then "ciphertext" else "decrypted"))
  , txt env 20 68 15 (hsb 0 0 60) "XOR: same = 0, different = 1. a ^ k ^ k = a, so the same key restores the picture."
  , txt env 20 90 15 (hsb 0 0 60) "With a repeating key, the picture's pattern stays visible in the ciphertext."
  ]
  where
    key = wKey w
    cipher = U.zipWith xor plain key      -- ★ 暗号化：XOR
    decrypted = U.zipWith xor cipher key  -- ★ 復号：もう一度 XOR
    cell = min ((envW env - 160) / fromIntegral (imgW * 3 + 8)) ((envH env - 220) / fromIntegral imgH)
    title :: Float -> String -> Picture
    title i = txt env (panelX + i * fromIntegral (imgW + panelGap) * cell) (panelY - 10) 15 (hsb 0 0 100)
    keyName = case wKeyMode w of
      1 -> "8-bit repeat"
      2 -> "100-bit repeat"
      _ -> "random, picture length (one-time pad)"

-- 3 枚の絵を、すきまつきで 1 枚のピクセル画像にまとめて描く
drawPanels :: Env -> Float -> [U.Vector Int] -> Picture
drawPanels env cell imgs =
  at (toG env (panelX + fromIntegral totalW * cell / 2) (panelY + fromIntegral imgH * cell / 2))
     (pixelImage totalW imgH cell pixel)
  where
    n = length imgs
    totalW = n * imgW + (n - 1) * panelGap
    pixel x y =
      let (panel, px) = x `divMod` (imgW + panelGap)
      in if px >= imgW then backRGB else
         case drop panel imgs of
           (img : _) | img U.! (y * imgW + px) == 1 -> onRGB
           _ -> offRGB

panelX, panelY :: Float
panelX = 40
panelY = 130

panelGap :: Int
panelGap = 4               -- 絵と絵のすきま（マス）

onRGB, offRGB, backRGB :: (Word8, Word8, Word8)
onRGB = rgb8 (hsb 45 90 100)
offRGB = rgb8 (hsb 230 30 20)
backRGB = rgb8 background

background :: Color
background = hsb 230 40 10

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> do
    let m = digitToInt c
    k <- makeKey m
    pure w { wKeyMode = m, wKey = k }
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wShowDec = not (wShowDec w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 鍵 1 の暗号文を見る。ハートが縞模様ごしに見える＝暗号として失格
-- 2. 鍵 3 の暗号文は、どう見ても乱数。これが「解読不可能」と証明された唯一の暗号
-- 3. でも鍵 3 は「絵と同じ長さの鍵」を安全に相手に渡す必要がある。どうやって？（鍵配送問題）

-- ---------- 道具 ----------

-- 色をピクセル画像用の 0〜255 に直す
rgb8 :: Color -> (Word8, Word8, Word8)
rgb8 c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (round (v * 255) :: Int)

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
