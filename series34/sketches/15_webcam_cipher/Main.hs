-- ============================================
-- 15 カメラ映像を暗号化 📷
-- カメラの映像を 160×120 に縮め、横に並んだ 4 画素（各色 4 ビット × 4 = 16 ビット）を 1 ブロックとして
-- 05 のおもちゃの暗号で毎フレーム暗号化する。ECB だと動く輪郭が見えてしまい、CTR だと砂嵐になる
-- 1 = ECB / 2 = CTR / D = 表示の切りかえ（暗号文 → 正しい鍵で復号 → 違う鍵で復号）/ ←→ = 鍵を変える
-- カメラが使えないときは、自動で動く絵を暗号化する
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showHex)

vw, vh, nBlocks :: Int
vw = 160          -- ★ 暗号化する映像の大きさ
vh = 120
nBlocks = vw `div` 4 * vh * 3

sbox :: U.Vector Int
sbox = U.fromList [0xC, 0x5, 0x6, 0xB, 0x9, 0x0, 0xA, 0xD, 0x3, 0xE, 0xF, 0x8, 0x4, 0x7, 0x1, 0x2]

-- 変換表（S ボックス 4 個ぶんと P ボックス）を先に作っておくと、鍵を変えたときの計算が速い
substT, permT :: U.Vector Int
substT = U.generate 65536 $ \x ->
  foldl' (\y i -> y .|. ((sbox U.! ((x `shiftR` (4 * i)) .&. 0xF)) `shiftL` (4 * i))) 0 [0 .. 3]
permT = U.generate 65536 $ \x ->
  foldl' (\y b -> if testBit x (15 - b) then y .|. (1 `shiftL` (15 - ((b `mod` 4) * 4 + (b `shiftR` 2)))) else y) 0 [0 .. 15]

-- 鍵ごとの表
data Tables = Tables
  { encT        :: !(U.Vector Int)
  , decT        :: !(U.Vector Int)
  , decWrong    :: !(U.Vector Int)
  , stream      :: !(U.Vector Int)   -- CTR 用の鍵の列：カウンタ i を暗号化したもの
  , streamWrong :: !(U.Vector Int)
  }

data World = World
  { cipherKey :: !Int
  , tables    :: !Tables
  , modeNo    :: !Int        -- 1 = ECB、2 = CTR
  , view      :: !Int        -- 0 暗号文 1 正しい鍵で復号 2 違う鍵で復号
  , camera    :: !(Maybe Camera)
  , live      :: !Bool
  , source    :: !B.ByteString
  , output    :: !(U.Vector Int)  -- ブロックごとの表示する値
  }

main :: IO ()
main = runSketch (sketch "34-15 Encrypting the Camera" world0)
  { skBack  = rgb255 10 12 16
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { camera = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }
  where
    world0 = World 0x6C1F (makeTables 0x6C1F) 1 0 Nothing False B.empty U.empty

-- ★ 鍵が決まったら、65536 通りの入力すべての暗号文を表にしておく（毎フレーム計算しなくて済む）
makeTables :: Int -> Tables
makeTables k =
  let wrongK = k `xor` 1                  -- 違う鍵は 1 ビットだけ違う
      e = U.generate 65536 (enc k)
      w = U.generate 65536 (enc wrongK)
      invert t = U.update (U.replicate 65536 0) (U.imap (\x c -> (c, x)) t)
      counters t = U.generate nBlocks (\i -> t U.! (i .&. 0xFFFF))
  in Tables e (invert e) (invert w) (counters e) (counters w)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  isLive <- maybe (pure False) camLive (camera w)
  frame <- case camera w of
    Just cam | isLive -> cameraFrame cam
    _ -> pure B.empty
  -- 元の映像（カメラか、代わりの絵）
  let usable = isLive && B.length frame >= vw * vh * 3
      src = if usable then frame else fakeFrame (envTime env)
  pure w { live = usable, source = src, output = encryptFrame w src }

-- ★ 暗号化（と復号）：4 画素 × 4 ビットを 1 ブロックに
encryptFrame :: World -> B.ByteString -> U.Vector Int
encryptFrame w src = U.generate nBlocks $ \b ->
  let (j, ch) = b `divMod` 3
      (y, bx) = j `divMod` (vw `div` 4)
      px k = fromIntegral (B.index src (3 * (y * vw + bx * 4 + k) + ch) `shiftR` 4) :: Int
      block = (px 0 `shiftL` 12) .|. (px 1 `shiftL` 8) .|. (px 2 `shiftL` 4) .|. px 3
      t = tables w
      ecb = modeNo w == 1
      c = if ecb then encT t U.! block else block `xor` (stream t U.! b)   -- 1 = ECB、2 = CTR
  in case view w of
       1 -> if ecb then decT t U.! c else c `xor` (stream t U.! b)
       2 -> if ecb then decWrong t U.! c else c `xor` (streamWrong t U.! b)  -- 違う鍵の列
       _ -> c

-- カメラがないときの代わり：動く円と四角と文字
fakeFrame :: Float -> B.ByteString
fakeFrame t = fst (B.unfoldrN (vw * vh * 3) gen 0)
  where
    cx = 80 + 50 * cos t
    cy = 60 + 30 * sin (t * 1.3)
    rx = 20 + 30 * sin (t * 0.7)
    gen :: Int -> Maybe (Word8, Int)
    gen i =
      let (j, ch) = i `divMod` 3
          (yi, xi) = j `divMod` vw
          x = fromIntegral xi + 0.5 :: Float
          y = fromIntegral yi + 0.5 :: Float
          d = (x - cx) * (x - cx) + (y - cy) * (y - cy)
          (r, g, b)
            | helloMask U.! j = (20, 20, 20)
            | x >= rx && x < rx + 40 && y >= 80 && y < 110 = (200, 40, 40)
            | d < 625 = (40, 60, 160)
            | otherwise = (230, 220, 200)
          v = case ch of { 0 -> r; 1 -> g; _ -> b }
      in Just (v, i + 1)

-- 「HELLO」の形（5×7 のドット絵を 4 倍にしたもの）
helloMask :: U.Vector Bool
helloMask = U.generate (vw * vh) $ \j ->
  let (y, x) = j `divMod` vw
      gx = (x - 30) `div` 4
      gy = (y - 12) `div` 4
      (letterNo, col) = gx `divMod` 4
  in x >= 30 && y >= 12 && gy < 7 && letterNo < 5 && col < 3
     && glyphBit (nth "" helloGlyphs letterNo) col gy

helloGlyphs :: [String]
helloGlyphs =
  [ "X.XX.XX.XXXXX.XX.XX.X"   -- H
  , "XXXX..X..XXXX..X..XXX"   -- E
  , "X..X..X..X..X..X..XXX"   -- L
  , "X..X..X..X..X..X..XXX"   -- L
  , ".X.X.XX.XX.XX.XX.X.X."   -- O
  ]

glyphBit :: String -> Int -> Int -> Bool
glyphBit g col row = nth '.' g (row * 3 + col) == 'X'

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (images ++ texts)
  where
    sw = envW env
    sh = envH env
    fw = fromIntegral vw :: Float
    fh = fromIntegral vh :: Float
    bigW = min (sw * 0.62) ((sh - 150) * fw / fh)
    bigH = bigW * fh / fw
    smallW = min (sw - bigW - 80) (bigW * 0.5)
    smallH = smallW * fh / fw
    place x y pw ph pic =
      let (a, b) = toS env (x + pw / 2, y + ph / 2)
      in Translate a b (Scale (pw / fw) (pw / fw) pic)
    images
      | U.length (output w) < nBlocks || B.length (source w) < vw * vh * 3 = []
      | otherwise =
          [ place 30 100 bigW bigH (outputImage (output w))
          , place (bigW + 60) 100 smallW smallH (sourceImage (source w)) ]
    viewName = case view w of
      0 -> "cipher"
      1 -> "decrypted with the right key"
      _ -> "decrypted with a key 1 bit off"
    tx = bigW + 60
    ty = 130 + smallH
    msgs
      | view w == 2 = ["A key just 1 bit off makes", "the decrypted video unreadable", "(avalanche effect from sketch 05)"]
      | modeNo w == 1 = ["ECB: same 4 pixels -> same cipher.", "Flat background and clothes share a", "pattern, so outlines show (bad)"]
      | otherwise = ["CTR: a different key stream per block.", "Same color, other place: other cipher", "-> only static (good; change the IV)"]
    texts =
      [ txt env 24 34 18 (gray 0.94) ("Encrypting the camera  1 ECB  2 CTR  D switch view  Left/Right key " ++ hex4 (cipherKey w))
      , txt env 24 56 14 (gray 0.6)
          ((if live w then "Camera on" else "No camera -> moving picture instead") ++ "   1 block = 4 pixels x 4 bits (per color) | educational")
      , txt env 30 88 20 (if modeNo w == 1 && view w == 0 then rgb255 255 120 120 else rgb255 120 230 150)
          ((if modeNo w == 1 then "ECB: " else "CTR: ") ++ viewName)
      , txt env tx 90 15 (gray 0.78) "Original video"
      ]
      ++ [ txt env tx (ty + fromIntegral i * 22) 14 (gray 0.67) s | (i, s) <- zip [0 :: Int ..] msgs ]

-- ブロックの値を画素に戻して絵にする
outputImage :: U.Vector Int -> Picture
outputImage out = pixelImage vw vh 1 $ \x y ->
  let (bx, k) = x `divMod` 4
      base = (y * (vw `div` 4) + bx) * 3
      get ch = fromIntegral (((out U.! (base + ch)) `shiftR` (12 - 4 * k)) .&. 0xF) * 17
  in (get 0, get 1, get 2)

sourceImage :: B.ByteString -> Picture
sourceImage f = pixelImage vw vh 1 $ \x y ->
  let i = (y * vw + x) * 3
  in (B.index f i, B.index f (i + 1), B.index f (i + 2))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure w { modeNo = 1 }
  EventKey (Char '2') Down _ _ -> pure w { modeNo = 2 }
  EventKey (Char c) Down _ _ | c `elem` "dD" -> pure w { view = (view w + 1) `mod` 3 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> newKey
  EventKey (SpecialKey KeyRight) Down _ _ -> newKey
  _ -> pure w
  where newKey = (\k -> w { cipherKey = k, tables = makeTables k }) <$> randomRIO (0, 65535)

-- ---------- やってみよう ----------
-- 1. ECB で手をふると、暗号文の中でも手が動くのが見える。CTR では？
-- 2. 単色の服・壁の前と、模様のある服の前では、ECB の「透け方」はどう違う？
-- 3. vw, vh を 200, 150 にすると細かくなる。ECB の輪郭はもっとはっきりする？（重くなったら戻す）
-- 4. 作品のアイデア：映像の一部だけ（顔のあたりだけ）を暗号化する「モザイク」をつくってみよう

-- ==================================================
-- ここから下は「道具」（05 と同じおもちゃの 16 ビット SPN 暗号）
-- ==================================================
roundKey :: Int -> Int -> Int
roundKey k r =
  let s = (3 * r) `mod` 16
  in (((k `shiftL` s) .|. (k `shiftR` (16 - s))) `xor` (0x1357 * (r + 1))) .&. 0xFFFF

enc :: Int -> Int -> Int
enc k x0 =
  let r0 = substT U.! (x0 `xor` roundKey k 0)
      r1 = substT U.! ((permT U.! r0) `xor` roundKey k 1)
      r2 = substT U.! ((permT U.! r1) `xor` roundKey k 2)
      r3 = substT U.! ((permT U.! r2) `xor` roundKey k 3)
  in r3 `xor` roundKey k 4

hex4 :: Int -> String
hex4 v = let s = map toUpper (showHex v "") in "0x" ++ replicate (4 - length s) '0' ++ s

nth :: a -> [a] -> Int -> a
nth d xs i = case drop i xs of
  (x : _) -> x
  [] -> d

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str
