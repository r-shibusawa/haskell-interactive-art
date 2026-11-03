-- | Picture を SVG 文字列にする（画面の保存と、テスト時の見た目の確認に使う）
module Art.Svg (pictureToSvg, pictureStats, png) where

import Data.Bits (shiftR, testBit, xor, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl')
import Data.Word (Word32, Word8)
import Graphics.Gloss

import Graphics.Gloss.Data.Bitmap (BitmapData, bitmapSize)
import Art.Runtime (recallBitmap)
import Text.Printf (printf)

-- | (描画要素の数, NaN や無限大の数) を数える。絵全体を評価することにもなる。
pictureStats :: Picture -> (Int, Int)
pictureStats = go (0, 0)
  where
    bad :: Float -> Int
    bad x = if isNaN x || isInfinite x then 1 else 0
    pts = foldl' (\s (x, y) -> s + bad x + bad y) 0
    add (!a, !b) p n = (a + p, b + n)
    go !acc pic = case pic of
      Blank -> acc
      Polygon ps -> add acc 1 (pts ps)
      Line ps -> add acc 1 (pts ps)
      Circle r -> add acc 1 (bad r)
      ThickCircle r w -> add acc 1 (bad r + bad w)
      Arc a b r -> add acc 1 (bad a + bad b + bad r)
      ThickArc a b r w -> add acc 1 (bad a + bad b + bad r + bad w)
      Text s -> add acc (1 + length s `div` 4) 0
      Bitmap _ -> add acc 1 0
      BitmapSection _ _ -> add acc 1 0
      Color c p -> c `seq` go acc p
      Translate x y p -> go (add acc 0 (bad x + bad y)) p
      Rotate a p -> go (add acc 0 (bad a)) p
      Scale x y p -> go (add acc 0 (bad x + bad y)) p
      Pictures ps -> foldl' go acc ps

num :: Float -> String
num = printf "%.2f"

css :: Color -> String
css c = let (r, g, b, _) = rgbaOfColor c
            q x = round (255 * max 0 (min 1 x)) :: Int
        in printf "rgb(%d,%d,%d)" (q r) (q g) (q b)

alphaOf :: Color -> Float
alphaOf c = let (_, _, _, a) = rgbaOfColor c in a

pictureToSvg :: Float -> Float -> Color -> Picture -> String
pictureToSvg w h back pic = concat
  [ printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" viewBox=\"%s %s %s %s\">" (num w) (num h) (num (-w/2)) (num (-h/2)) (num w) (num h)
  , printf "<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"%s\"/>" (num (-w/2)) (num (-h/2)) (num w) (num h) (css back)
  , "<g transform=\"scale(1,-1)\">", go white pic, "</g></svg>" ]
  where
    pointsAttr ps = unwords [num x ++ "," ++ num y | (x, y) <- ps]
    st c = printf "stroke=\"%s\" stroke-opacity=\"%.3f\" fill=\"none\" vector-effect=\"non-scaling-stroke\"" (css c) (alphaOf c) :: String
    fl c = printf "fill=\"%s\" fill-opacity=\"%.3f\"" (css c) (alphaOf c) :: String
    arc a b r = let f t = (r * cos (t*pi/180), r * sin (t*pi/180))
                    n = max 2 (ceiling (abs (b - a) / 6)) :: Int
                in [f (a + (b - a) * fromIntegral i / fromIntegral n) | i <- [0 .. n]]
    go :: Color -> Picture -> String
    go c p = case p of
      Blank -> ""
      Polygon ps -> printf "<polygon points=\"%s\" %s/>" (pointsAttr ps) (fl c)
      Line ps -> printf "<polyline points=\"%s\" %s/>" (pointsAttr ps) (st c)
      Circle r -> printf "<circle r=\"%s\" %s/>" (num (abs r)) (st c)
      ThickCircle r t
        | t <= 0 -> go c (Circle r)
        | otherwise -> printf "<circle r=\"%s\" fill=\"none\" stroke=\"%s\" stroke-opacity=\"%.3f\" stroke-width=\"%s\"/>" (num (abs r)) (css c) (alphaOf c) (num t)
      Arc a b r -> go c (Line (arc a b r))
      ThickArc a b r t -> printf "<polyline points=\"%s\" fill=\"none\" stroke=\"%s\" stroke-opacity=\"%.3f\" stroke-width=\"%s\"/>" (pointsAttr (arc a b r)) (css c) (alphaOf c) (num (max 1 t))
      Text s -> printf "<g transform=\"scale(1,-1)\"><text font-family=\"Helvetica,Arial,sans-serif\" font-size=\"120\" %s>%s</text></g>" (fl c) (concatMap esc s)
      Bitmap bd -> image bd
      BitmapSection _ bd -> image bd
      Color c' q -> go c' q
      Translate x y q -> printf "<g transform=\"translate(%s,%s)\">%s</g>" (num x) (num y) (go c q)
      Rotate a q -> printf "<g transform=\"rotate(%s)\">%s</g>" (num (negate a)) (go c q)
      Scale x y q -> printf "<g transform=\"scale(%s,%s)\">%s</g>" (printf "%.4f" x :: String) (printf "%.4f" y :: String) (go c q)
      Pictures ps -> concatMap (go c) ps
    esc '<' = "&lt;"
    esc '&' = "&amp;"
    esc '>' = "&gt;"
    esc x = [x]

image :: BitmapData -> String
image bd =
  let (bw, bh) = bitmapSize bd
      x0 = negate bw `div` 2
      y0 = negate bh `div` 2
  in case recallBitmap bd of
       Just raw -> printf "<g transform=\"scale(1,-1)\"><image x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" style=\"image-rendering:pixelated\" href=\"data:image/png;base64,%s\"/></g>" x0 y0 bw bh (BC.unpack (base64 (png bw bh raw)))
       Nothing -> printf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"gray\"/>" x0 y0 bw bh

-- 非圧縮の PNG（テスト用なので速さと単純さを優先）
png :: Int -> Int -> B.ByteString -> B.ByteString
png w h rgba = BL.toStrict . BB.toLazyByteString $ mconcat
  [ BB.byteString (B.pack [137, 80, 78, 71, 13, 10, 26, 10])
  , chunk "IHDR" (be w <> be h <> B.pack [8, 6, 0, 0, 0])
  , chunk "IDAT" zlib
  , chunk "IEND" B.empty ]
  where
    be = BL.toStrict . BB.toLazyByteString . BB.word32BE . fromIntegral
    chunk t d = let body = BC.pack t <> d
                in BB.word32BE (fromIntegral (B.length d)) <> BB.byteString body <> BB.word32BE (crc body)
    rows = B.concat [B.cons 0 (B.take (w * 4) (B.drop (y * w * 4) rgba)) | y <- [0 .. h - 1]]
    zlib = B.pack [0x78, 1] <> stored rows <> (BL.toStrict . BB.toLazyByteString . BB.word32BE $ adler rows)
    stored d | B.length d <= 65535 = block 1 d
             | otherwise = let (a, b) = B.splitAt 65535 d in block 0 a <> stored b
    block f d = let n = B.length d
                    w8 = fromIntegral :: Int -> Word8
                in B.pack [f, w8 (n .&. 255), w8 (n `shiftR` 8), w8 (negate n - 1 .&. 255), w8 ((negate n - 1) `shiftR` 8 .&. 255)] <> d

adler :: B.ByteString -> Word32
adler d = let (a, b) = B.foldl' (\(!x, !y) v -> let x' = (x + fromIntegral v) `mod` 65521 in (x', (y + x') `mod` 65521)) (1, 0) d
          in b * 65536 + a

crc :: B.ByteString -> Word32
crc = xor 0xffffffff . B.foldl' (\c v -> foldl' (\k i -> if testBit (k `xor` (if testBit v i then 1 else 0)) 0 then (k `shiftR` 1) `xor` 0xedb88320 else k `shiftR` 1) c [0 .. 7]) 0xffffffff

base64 :: B.ByteString -> B.ByteString
base64 s = BC.pack (go (B.unpack s))
  where
    tbl = BC.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    e i = BC.index tbl (fromIntegral i)
    go (a:b:c:r) = e (a `shiftR` 2) : e (((a .&. 3) * 16) + (b `shiftR` 4)) : e (((b .&. 15) * 4) + (c `shiftR` 6)) : e (c .&. 63) : go r
    go [a, b] = [e (a `shiftR` 2), e (((a .&. 3) * 16) + (b `shiftR` 4)), e ((b .&. 15) * 4), '=']
    go [a] = [e (a `shiftR` 2), e ((a .&. 3) * 16), '=', '=']
    go [] = []
