-- | 描画の小道具（色、文字、線、数学、ノイズ、ピクセル画像、3D の投影）
module Art.Draw
  ( -- * 色
    hsb, hsba, gray, rgb255, rgba255, withA
    -- * 配置と図形
  , at, disc, ring, rect, rectLine, thickLine, thickPath, poly, arrow
    -- * 文字（ASCII のみ）
  , label, labelC
    -- * 数学
  , tau, clampF, lerpF, remap, fract, dist2, dot2, norm2, wrapF
    -- * ノイズ
  , noise2, noise3, fbm2
    -- * ピクセル画像
  , pixelImage, pixelImageF, grayImage
    -- * 3D
  , V3(..), rotX, rotY, rotZ, add3, sub3, scale3, dot3, cross3, norm3, unit3, project, depthSort
  ) where

import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Foreign.Storable (pokeByteOff)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Graphics.Gloss

import Art.Runtime (rememberBitmap)

---------------------------------------------------------------- 色

-- | HSB：色相 0〜360、彩度 0〜100、明度 0〜100（p5.js の colorMode(HSB, 360, 100, 100)）
hsb :: Float -> Float -> Float -> Color
hsb h s b = hsba h s b 1

-- | 透明度つき HSB（透明度は 0〜1）
hsba :: Float -> Float -> Float -> Float -> Color
hsba h0 s0 v0 a =
  let h = wrapF 360 h0 / 60
      s = clampF 0 1 (s0 / 100)
      v = clampF 0 1 (v0 / 100)
      i = floor h :: Int
      f = h - fromIntegral i
      p = v * (1 - s)
      q = v * (1 - s * f)
      t = v * (1 - s * (1 - f))
  in case i `mod` 6 of
       0 -> makeColor v t p a
       1 -> makeColor q v p a
       2 -> makeColor p v t a
       3 -> makeColor p q v a
       4 -> makeColor t p v a
       _ -> makeColor v p q a

gray :: Float -> Color
gray v = makeColor v v v 1

rgb255 :: Int -> Int -> Int -> Color
rgb255 r g b = makeColorI r g b 255

rgba255 :: Int -> Int -> Int -> Int -> Color
rgba255 = makeColorI

-- | 色の透明度だけ変える
withA :: Float -> Color -> Color
withA a c = let (r, g, b, _) = rgbaOfColor c in makeColor r g b a

---------------------------------------------------------------- 図形

at :: (Float, Float) -> Picture -> Picture
at (x, y) = Translate x y

-- | 塗りつぶした円（半径 r）
disc :: Float -> Picture
disc r = ThickCircle (r / 2) r

-- | 太さ w の輪
ring :: Float -> Float -> Picture
ring r w = ThickCircle r w

-- | 中心 (x, y)、幅 w、高さ h の塗りつぶした長方形
rect :: Float -> Float -> Float -> Float -> Picture
rect x y w h = Translate x y (rectangleSolid w h)

-- | 枠線だけの長方形
rectLine :: Float -> Float -> Float -> Float -> Picture
rectLine x y w h = Translate x y (rectangleWire w h)

-- | 太さのある線分
thickLine :: Float -> Point -> Point -> Picture
thickLine w (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      l = sqrt (dx * dx + dy * dy)
  in if l < 1e-6 then Blank else
     let nx = -dy / l * w / 2
         ny = dx / l * w / 2
     in Polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]

-- | 太さのある折れ線
thickPath :: Float -> [Point] -> Picture
thickPath w ps = Pictures (zipWith (thickLine w) ps (drop 1 ps))

-- | 閉じた折れ線の枠
poly :: [Point] -> Picture
poly [] = Blank
poly ps@(p : _) = Line (ps ++ [p])

-- | 矢印（太さ w）
arrow :: Float -> Point -> Point -> Picture
arrow w a@(x1, y1) b@(x2, y2) =
  let ang = atan2 (y2 - y1) (x2 - x1)
      s = 3 * w + 4
      f d = (x2 - s * cos (ang + d), y2 - s * sin (ang + d))
  in Pictures [thickLine w a b, Polygon [b, f 0.45, f (-0.45)]]

---------------------------------------------------------------- 文字

-- | 左下を (x, y) にして文字を書く。size はおよその文字の高さ（ピクセル）。
--   gloss の文字は ASCII のみ。日本語は '?' になる。
label :: Float -> Float -> Float -> Color -> String -> Picture
label x y size c s =
  Translate x y (Scale k k (Color c (Text (map asciiOnly s))))
  where k = size / 100

-- | 中央揃えの文字
labelC :: Float -> Float -> Float -> Color -> String -> Picture
labelC x y size c s = label (x - fromIntegral (length s) * size * 0.36) (y - size / 2) size c s

asciiOnly :: Char -> Char
asciiOnly ch = if ch >= ' ' && ch <= '~' then ch else '?'

---------------------------------------------------------------- 数学

tau :: Float
tau = 2 * pi

clampF :: Float -> Float -> Float -> Float
clampF lo hi = max lo . min hi

lerpF :: Float -> Float -> Float -> Float
lerpF a b t = a + (b - a) * t

-- | p5.js の map()
remap :: Float -> Float -> Float -> Float -> Float -> Float
remap v a b c d = c + (d - c) * (v - a) / (b - a)

fract :: Float -> Float
fract x = x - fromIntegral (floor x :: Int)

-- | 0 以上 m 未満に折り返す
wrapF :: Float -> Float -> Float
wrapF m x = x - m * fromIntegral (floor (x / m) :: Int)

dist2 :: Point -> Point -> Float
dist2 (a, b) (c, d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))

dot2 :: Point -> Point -> Float
dot2 (a, b) (c, d) = a * c + b * d

norm2 :: Point -> Float
norm2 (a, b) = sqrt (a * a + b * b)

---------------------------------------------------------------- ノイズ

perm :: U.Vector Int
perm = U.fromList (take 512 (cycle base))
  where base = map (\i -> (i * 167 + 13) `xor'` ((i * 31) `div` 7)) [0 .. 255 :: Int]
        xor' a b = (a + b * 3 + a * b) .&. 255

fade :: Float -> Float
fade t = t * t * t * (t * (t * 6 - 15) + 10)

grad3 :: Int -> Float -> Float -> Float -> Float
grad3 h x y z =
  let k = h .&. 15
      u = if k < 8 then x else y
      v | k < 4 = y
        | k == 12 || k == 14 = x
        | otherwise = z
  in (if k .&. 1 == 0 then u else -u) + (if k .&. 2 == 0 then v else -v)

-- | パーリンノイズ（値はおよそ 0〜1、p5.js の noise に相当）
noise3 :: Float -> Float -> Float -> Float
noise3 x y z =
  let fl v = floor v :: Int
      xi = fl x .&. 255
      yi = fl y .&. 255
      zi = fl z .&. 255
      xf = x - fromIntegral (fl x)
      yf = y - fromIntegral (fl y)
      zf = z - fromIntegral (fl z)
      p i = perm U.! i
      a = p xi + yi
      aa = p a + zi
      ab = p (a + 1) + zi
      b = p (xi + 1) + yi
      ba = p b + zi
      bb = p (b + 1) + zi
      u = fade xf
      v = fade yf
      w = fade zf
      l1 = lerpF (grad3 (p aa) xf yf zf) (grad3 (p ba) (xf - 1) yf zf) u
      l2 = lerpF (grad3 (p ab) xf (yf - 1) zf) (grad3 (p bb) (xf - 1) (yf - 1) zf) u
      l3 = lerpF (grad3 (p (aa + 1)) xf yf (zf - 1)) (grad3 (p (ba + 1)) (xf - 1) yf (zf - 1)) u
      l4 = lerpF (grad3 (p (ab + 1)) xf (yf - 1) (zf - 1)) (grad3 (p (bb + 1)) (xf - 1) (yf - 1) (zf - 1)) u
  in clampF 0 1 (0.5 + 0.5 * lerpF (lerpF l1 l2 v) (lerpF l3 l4 v) w)

noise2 :: Float -> Float -> Float
noise2 x y = noise3 x y 0.5

-- | 重ねたノイズ
fbm2 :: Int -> Float -> Float -> Float
fbm2 n x y =
  let go :: Int -> Float -> Float -> Float -> Float -> (Float, Float)
      go 0 _ _ s t = (s, t)
      go k f a s t = go (k - 1) (f * 2) (a * 0.5) (s + a * noise2 (x * f) (y * f)) (t + a)
      (v, tot) = go n 1 0.5 0 0
  in v / tot

---------------------------------------------------------------- ピクセル画像

-- | 幅 w × 高さ h のピクセル画像を k 倍にして画面中央に置く。
--   f x y は左上が (0, 0) で (赤, 緑, 青) を 0〜255 で返す。
--   60 fps を保つには w × h を 8 万画素くらいまでにする。
pixelImage :: Int -> Int -> Float -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
pixelImage w h k f =
  let bs = BI.unsafeCreate (w * h * 4) $ \ptr ->
        let goY !y | y >= h = pure ()
                   | otherwise = goX y 0 >> goY (y + 1)
            goX !y !x
              | x >= w = pure ()
              | otherwise = do
                  let (a, b, c) = f x y
                      o = (y * w + x) * 4
                  pokeByteOff ptr o a
                  pokeByteOff ptr (o + 1) b
                  pokeByteOff ptr (o + 2) c
                  pokeByteOff ptr (o + 3) (255 :: Word8)
                  goX y (x + 1)
        in goY 0
      fmt = BitmapFormat TopToBottom PxRGBA
  in bs `seq` Scale k k (Bitmap (rememberBitmap (bitmapDataOfByteString w h fmt bs False) bs))

-- | 0〜1 の小数で色を返す版
pixelImageF :: Int -> Int -> Float -> (Int -> Int -> (Float, Float, Float)) -> Picture
pixelImageF w h k f = pixelImage w h k (\x y -> let (a, b, c) = f x y in (q a, q b, q c))
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- | グレースケール版（0〜255 が入った長さ w × h のバイト列）
grayImage :: Int -> Int -> Float -> B.ByteString -> Picture
grayImage w h k d = pixelImage w h k (\x y -> let v = B.index d (y * w + x) in (v, v, v))

---------------------------------------------------------------- 3D

data V3 = V3 !Float !Float !Float deriving (Eq, Show)

add3, sub3, cross3 :: V3 -> V3 -> V3
add3 (V3 a b c) (V3 d e f) = V3 (a + d) (b + e) (c + f)
sub3 (V3 a b c) (V3 d e f) = V3 (a - d) (b - e) (c - f)
cross3 (V3 a b c) (V3 d e f) = V3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

scale3 :: Float -> V3 -> V3
scale3 k (V3 a b c) = V3 (k * a) (k * b) (k * c)

dot3 :: V3 -> V3 -> Float
dot3 (V3 a b c) (V3 d e f) = a * d + b * e + c * f

norm3 :: V3 -> Float
norm3 v = sqrt (dot3 v v)

unit3 :: V3 -> V3
unit3 v = let n = norm3 v in if n < 1e-9 then V3 0 0 0 else scale3 (1 / n) v

rotX, rotY, rotZ :: Float -> V3 -> V3
rotX t (V3 x y z) = V3 x (y * cos t - z * sin t) (y * sin t + z * cos t)
rotY t (V3 x y z) = V3 (x * cos t + z * sin t) y (z * cos t - x * sin t)
rotZ t (V3 x y z) = V3 (x * cos t - y * sin t) (x * sin t + y * cos t) z

-- | 透視投影。カメラは z = +d にあり原点を見る。戻り値は画面の座標。
project :: Float -> V3 -> Point
project d (V3 x y z) = let k = d / max 1 (d - z) in (x * k, y * k)

-- | 奥（z が小さい）から順に並べる。OpenGL の深度バッファを使わず、画家のアルゴリズムで描く。
depthSort :: (a -> Float) -> [a] -> [a]
depthSort = sortOn
