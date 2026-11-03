-- ============================================
-- 06 暗号利用モード（ECB・CBC・CTR）
-- ブロック暗号は 16 ビットずつしか暗号化できない。長いデータ（画像）をどうつなぐか = 利用モード
--   ECB：ブロックごとにそのまま暗号化 → 同じ色のブロックは同じ暗号文 → 絵の輪郭が透けて見える！
--   CBC：1 つ前の暗号文を XOR してから暗号化 → 連鎖して模様が消える
--   CTR：カウンタ（0, 1, 2, …）を暗号化した列を XOR → ストリーム暗号のように使える
-- 1〜3 = 画像 / ←→ = 鍵を変える / I = 初期値（IV）を変える / D = 復号して見せる
-- ※ 教育用（05 と同じおもちゃの 16 ビット暗号）。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showHex)

sbox, invBox :: U.Vector Int
sbox = U.fromList [0xC, 0x5, 0x6, 0xB, 0x9, 0x0, 0xA, 0xD, 0x3, 0xE, 0xF, 0x8, 0x4, 0x7, 0x1, 0x2]
invBox = U.replicate 16 0 U.// [(sbox U.! i, i) | i <- [0 .. 15]]

imgW, imgH, blocksPerRow :: Int
imgW = 160                -- ★ 画像の大きさ（4 画素 × 4 ビット = 1 ブロック）
imgH = 112
blocksPerRow = imgW `div` 4

data World = World
  { cipherKey   :: !Int
  , iv          :: !Int
  , pictureNo   :: !Int
  , showDecrypt :: !Bool
  , plainBlocks :: !(U.Vector Int)
  , layers      :: ![Picture]       -- 平文・ECB・CBC・CTR の画像
  }

main :: IO ()
main = runSketch (sketch "34-06 Block Cipher Modes" (encryptAll (usePicture 1 world0)))
  { skBack  = rgb255 16 18 24
  , skDraw  = drawWorld
  , skEvent = onEvent
  }
  where world0 = World 0x5A3C 0x0F0F 1 False U.empty []

-- 画像の明るさ（赤の値 0〜255）。上から重ねた順に調べる
shade :: Int -> Float -> Float -> Int
shade 1 x y            -- ペンギン風
  | tri (74, 30) (86, 30) (80, 40) x y || oval 66 108 22 8 x y || oval 94 108 22 8 x y = 250
  | oval 80 70 48 70 x y || oval 72 22 12 14 x y || oval 88 22 12 14 x y = 240
  | oval 80 62 74 92 x y || oval 80 22 46 40 x y = 20
  | otherwise = 255
shade 2 x y            -- 文字
  | x >= 10 && x < 150 && y >= 96 && y < 104 = 120
  | glyphP5 x y = 30
  | otherwise = 255
shade _ x y            -- しま模様と円
  | oval 80 56 60 60 x y = 0
  | odd (floor (x / 20) :: Int) = 60
  | otherwise = 200

-- 「p5」の字。gloss では画像の中に文字を描けないので、棒と輪で作る
glyphP5 :: Float -> Float -> Bool
glyphP5 x y = stemP || bowlP || top5 || neck5 || bowl5
  where
    inside x0 y0 x1 y1 = x >= x0 && x < x1 && y >= y0 && y < y1
    bowl cx cy = let d = sqrt ((x - cx) * (x - cx) + (y - cy) * (y - cy)) in d < 20 && d > 9
    stemP = inside 30 32 41 86
    bowlP = bowl 52 52 && x >= 41
    top5 = inside 88 18 124 28
    neck5 = inside 88 18 98 48
    bowl5 = bowl 103 62 && not (x < 100 && y < 62 && y > 48 && x < 92) && (x >= 96 || y >= 62)

-- 画像を描いて、1 画素 = 4 ビット（16 段階の明るさ）にする
usePicture :: Int -> World -> World
usePicture n w = w { pictureNo = n, plainBlocks = U.generate (imgH * blocksPerRow) block }
  where
    block b =
      let (y, bx) = b `divMod` blocksPerRow
          px k = shade n (fromIntegral (bx * 4 + k) + 0.5) (fromIntegral y + 0.5) `div` 16  -- 明るさ 0〜255 → 0〜15
      in foldl' (\v k -> (v `shiftL` 4) .|. px k) 0 [0 .. 3]

-- ★ 3 つのモードで暗号化（showDecrypt なら、そのあと復号もする）
encryptAll :: World -> World
encryptAll w = w { layers = map paint [plainBlocks w, ecbOut, cbcOut, ctrOut] }
  where
    k = cipherKey w
    ps = plainBlocks w
    counter i = enc k ((iv w + i) .&. 0xFFFF)
    ecb = U.map (enc k) ps                                -- ECB：そのまま
    cbc = U.postscanl' (\prev p -> enc k (p `xor` prev)) (iv w) ps  -- CBC：前の暗号文を混ぜる
    ctr = U.imap (\i p -> p `xor` counter i) ps           -- CTR：カウンタを暗号化して XOR
    (ecbOut, cbcOut, ctrOut)
      | showDecrypt w =
          ( U.map (dec k) ecb
          , U.zipWith (\c prev -> dec k c `xor` prev) cbc (U.cons (iv w) cbc)
          , U.imap (\i c -> c `xor` counter i) ctr )
      | otherwise = (ecb, cbc, ctr)

paint :: U.Vector Int -> Picture
paint blocks = pixelImage imgW imgH 1 $ \x y ->
  let (bx, k) = x `divMod` 4
      v = fromIntegral (((blocks U.! (y * blocksPerRow + bx)) `shiftR` (12 - 4 * k)) .&. 0xF) * 17
  in (v, v, v)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ panels ++ formulas)
  where
    sw = envW env
    sh = envH env
    ratio = fromIntegral imgH / fromIntegral imgW
    pw0 = (sw - 100) / 4
    (pw, ph) = if pw0 * ratio > sh - 220 then ((sh - 220) / ratio, sh - 220) else (pw0, pw0 * ratio)
    k = pw / fromIntegral imgW
    header =
      [ txt env 24 34 18 (gray 0.94)
          ("Block cipher modes  1-3 image  Left/Right key " ++ hex4 (cipherKey w) ++ "  I IV " ++ hex4 (iv w)
            ++ "  D decrypt " ++ (if showDecrypt w then "ON" else "off"))
      , txt env 24 56 14 (gray 0.6) "1 block = 4 pixels in a row (16 bits). Same toy cipher as sketch 05 | educational"
      ]
    names =
      [ ("Plain (original)", ["-"])
      , ("ECB", ["Same block -> same cipher.", "The outline shows (bad)"])
      , ("CBC", ["XOR previous cipher block.", "Needs an IV"])
      , ("CTR", ["Encrypt a counter and XOR.", "Can run in parallel"])
      ]
    panels = concat
      [ [ Translate (a + pw / 2) (b - ph / 2) (Scale k k img)
        , txt env x (y - 12) 18 (if i == 1 then rgb255 255 120 120 else gray 0.9)
            (nm ++ (if showDecrypt w && i > 0 then " -> decrypted" else ""))
        ]
        ++ [ txt env x (y + ph + 22 + fromIntegral j * 20) 14 (gray 0.63) s | (j, s) <- zip [0 :: Int ..] note ]
      | (i, (nm, note), img) <- zip3 [0 :: Int ..] names (layers w)
      , let x = 20 + fromIntegral i * (pw + 20)
            y = 100
            (a, b) = toS env (x, y) ]

    -- 図：3 つのモードのつなぎ方
    dy = 100 + ph + 90
    formulas =
      [ txt env 24 dy 15 (gray 0.78) "ECB: C[i] = E(P[i])"
      , txt env 24 (dy + 26) 15 (gray 0.78) "CBC: C[i] = E(P[i] xor C[i-1]),  C[-1] = IV"
      , txt env 24 (dy + 52) 15 (gray 0.78) "CTR: C[i] = P[i] xor E(IV + i)"
      , txt env 24 (dy + 86) 14 (gray 0.6) "Encrypting two images with the same key and IV in CTR repeats the key-reuse failure of sketch 03."
      , txt env 24 (dy + 108) 14 (gray 0.6) "Always change the IV."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c == '1' -> pure (encryptAll (usePicture 1 w))
    | c == '2' -> pure (encryptAll (usePicture 2 w))
    | c == '3' -> pure (encryptAll (usePicture 3 w))
    | c `elem` "iI" -> (\v -> encryptAll w { iv = v }) <$> randomRIO (0, 65535)
    | c `elem` "dD" -> pure (encryptAll w { showDecrypt = not (showDecrypt w) })
  EventKey (SpecialKey KeyLeft) Down _ _  -> newKey
  EventKey (SpecialKey KeyRight) Down _ _ -> newKey
  _ -> pure w
  where newKey = (\k -> encryptAll w { cipherKey = k }) <$> randomRIO (0, 65535)

-- ---------- やってみよう ----------
-- 1. 画像 1〜3 で ECB を見比べよう。どんな画像だと輪郭がいちばんよく見える？
-- 2. 鍵を変えても ECB の輪郭は消えない。なぜ？（鍵ではなく「モード」の問題）
-- 3. encryptAll の CBC で、暗号文の 1 ブロックだけ壊してから復号すると、何ブロック壊れる？ CTR では？
-- 4. imgW, imgH を大きくすると細かい絵になる。ブロックを縦 2 × 横 2 画素にするとどう見える？

-- ==================================================
-- ここから下は「道具」（05 と同じおもちゃの 16 ビット SPN 暗号と、その逆）
-- ==================================================
subst :: U.Vector Int -> Int -> Int
subst bx x = foldl' (\y i -> y .|. ((bx U.! ((x `shiftR` (4 * i)) .&. 0xF)) `shiftL` (4 * i))) 0 [0 .. 3]

pboxIndex :: Int -> Int
pboxIndex b = (b `mod` 4) * 4 + (b `shiftR` 2)

permute :: Int -> Int
permute x = foldl' (\y b -> if testBit x (15 - b) then y .|. (1 `shiftL` (15 - pboxIndex b)) else y) 0 [0 .. 15]

roundKey :: Int -> Int -> Int
roundKey k r =
  let s = (3 * r) `mod` 16
  in (((k `shiftL` s) .|. (k `shiftR` (16 - s))) `xor` (0x1357 * (r + 1))) .&. 0xFFFF

cipherRounds :: Int
cipherRounds = 4

enc :: Int -> Int -> Int
enc k x0 = foldl' oneRound x0 [0 .. cipherRounds - 1] `xor` roundKey k cipherRounds
  where
    oneRound x r =
      let b = subst sbox (x `xor` roundKey k r)
      in if r < cipherRounds - 1 then permute b else b

-- 逆順にたどる（P ボックスは 4×4 の転置なので、もう一度かけると元に戻る）
dec :: Int -> Int -> Int
dec k x0 = foldl' oneRound (x0 `xor` roundKey k cipherRounds) [cipherRounds - 1, cipherRounds - 2 .. 0]
  where
    oneRound x r =
      let a = if r < cipherRounds - 1 then permute x else x
      in subst invBox a `xor` roundKey k r

hex4 :: Int -> String
hex4 v = let s = map toUpper (showHex v "") in "0x" ++ replicate (4 - length s) '0' ++ s

-- 楕円（中心・幅・高さ）の内側か
oval :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
oval cx cy w h x y =
  let u = (x - cx) / (w / 2)
      v = (y - cy) / (h / 2)
  in u * u + v * v <= 1

-- 三角形の内側か
tri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> Float -> Bool
tri (ax, ay) (bx, by) (cx, cy) x y =
  let s1 = (bx - ax) * (y - ay) - (by - ay) * (x - ax)
      s2 = (cx - bx) * (y - by) - (cy - by) * (x - bx)
      s3 = (ax - cx) * (y - cy) - (ay - cy) * (x - cx)
  in (s1 >= 0 && s2 >= 0 && s3 >= 0) || (s1 <= 0 && s2 <= 0 && s3 <= 0)

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str
