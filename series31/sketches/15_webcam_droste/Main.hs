-- ============================================
-- 15 カメラのフィードバック（ビデオの不動点）📷
-- 前のフレームを、縮めて回して今の映像の上に貼る、を毎フレームくり返す → 映像の中に映像の中に映像…（ドロステ効果）
--   貼り方は相似変換 T(p) = c + sR(p − c)。どこまで潜っても動かない点 c が不動点 = うずの中心（02・04 と同じ）
-- クリック = うずの中心 c / ←→ = 回転 / ↑↓ = 縮小率 / F = 映像を止める / B = 枠 / S = 保存（カメラがないときは模様で動く）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)

fw, fh, camW', camH' :: Int
fw = 320                    -- ★ フィードバック用の画像の大きさ（小さいほど速い）
fh = 200
camW' = 320                 -- カメラから受け取る大きさ
camH' = 240

data World = World
  { wCam    :: !(Maybe Camera)
  , wLive   :: !Bool
  , wS      :: !Float              -- ★ 縮小率
  , wTheta  :: !Float              -- ★ 回転角
  , wC      :: !Point              -- 不動点（画像の座標）
  , wSrc    :: !(U.Vector Word32)  -- 今の映像
  , wFb     :: !(U.Vector Word32)  -- 前のフレーム（貼ったあと）
  , wFrozen :: !Bool
  , wBorder :: !Bool
  }

main :: IO ()
main = runSketch (sketch "31-15 Camera Droste" world0)
  { skBack  = black
  , skStart = \_ -> do
      cam <- startCamera camW' camH'
      pure world0 { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World Nothing False 0.78 0.18 (fromIntegral fw * 0.62, fromIntegral fh * 0.45) e e False True
  where e = U.replicate (fw * fh) 0

-- カメラの映像を、縦横比を保ったまま画面いっぱいに切り出す
fromCamera :: B.ByteString -> U.Vector Word32
fromCamera f
  | B.length f < camW' * camH' * 3 = U.replicate (fw * fh) 0
  | otherwise = U.generate (fw * fh) $ \k ->
      let (y, x) = k `divMod` fw
          i = ((y + (camH' - fh) `div` 2) * camW' + x) * 3
      in pack (BU.unsafeIndex f i) (BU.unsafeIndex f (i + 1)) (BU.unsafeIndex f (i + 2))

-- カメラがないときの模様
pattern :: Int -> U.Vector Word32
pattern frame = runST $ do
  v <- MU.replicate (fw * fh) (packC (hsb 230 30 15))
  let t = fromIntegral frame * 0.02 :: Float
      ffw = fromIntegral fw
      ffh = fromIntegral fh
  forM_ [0 .. 11 :: Int] $ \i -> do
    let fi = fromIntegral i
        cx = ffw / 2 + cos (t + fi) * ffw * 0.35
        cy = ffh / 2 + sin (t * 1.3 + fi * 2) * ffh * 0.35
        r = (40 + 20 * sin (t + fi)) / 4        -- p5 版の半分の解像度なので半径は 1/4
        col = packC (hsb (wrapF 360 (fi * 30 + fromIntegral frame)) 70 95)
        x0 = max 0 (floor (cx - r))
        x1 = min (fw - 1) (ceiling (cx + r))
        y0 = max 0 (floor (cy - r))
        y1 = min (fh - 1) (ceiling (cy + r))
    forM_ [y0 .. y1] $ \y -> forM_ [x0 .. x1] $ \x -> do
      let dx = fromIntegral x + 0.5 - cx
          dy = fromIntegral y + 0.5 - cy
      when (dx * dx + dy * dy <= r * r) (MU.unsafeWrite v (y * fw + x) col)
  U.unsafeFreeze v

-- 今の映像の上に、前のフレームを T で縮めて回して貼る（★ T(p) = c + sR(p − c)）
feedback :: World -> U.Vector Word32 -> U.Vector Word32
feedback w src = U.generate (fw * fh) pixel
  where
    (cx, cy) = wC w
    s = wS w
    cs = cos (wTheta w) / s     -- 逆変換：画面の点 → 前のフレームの点
    sn = sin (wTheta w) / s
    ffw = fromIntegral fw :: Float
    ffh = fromIntegral fh :: Float
    fb = wFb w
    bw = 1.5 :: Float           -- 枠の太さの半分
    frameCol = pack 255 220 90
    border = wBorder w
    pixel k =
      let (y, x) = k `quotRem` fw
          dx = fromIntegral x + 0.5 - cx
          dy = fromIntegral y + 0.5 - cy
          qx = cx + cs * dx + sn * dy
          qy = cy - sn * dx + cs * dy
          inside = qx >= 0 && qx < ffw && qy >= 0 && qy < ffh
          onFrame = border && qx >= negate bw && qx <= ffw + bw && qy >= negate bw && qy <= ffh + bw
                    && (qx < bw || qx > ffw - bw || qy < bw || qy > ffh - bw)
      in if onFrame then frameCol
         else if inside then bilinear fb qx qy
         else U.unsafeIndex src k

-- なめらかに読む（4 つの画素の重み付き平均）
bilinear :: U.Vector Word32 -> Float -> Float -> Word32
bilinear v x y =
  let x' = x - 0.5
      y' = y - 0.5
      xi = floor x' :: Int
      yi = floor y' :: Int
      a = round ((x' - fromIntegral xi) * 256) :: Int
      b = round ((y' - fromIntegral yi) * 256) :: Int
      x0 = max 0 xi
      y0 = max 0 yi
      x1 = min (fw - 1) (xi + 1)
      y1 = min (fh - 1) (yi + 1)
      p00 = fromIntegral (U.unsafeIndex v (y0 * fw + x0)) :: Int
      p10 = fromIntegral (U.unsafeIndex v (y0 * fw + x1)) :: Int
      p01 = fromIntegral (U.unsafeIndex v (y1 * fw + x0)) :: Int
      p11 = fromIntegral (U.unsafeIndex v (y1 * fw + x1)) :: Int
      ch sh = let c00 = (p00 `shiftR` sh) .&. 255
                  c10 = (p10 `shiftR` sh) .&. 255
                  c01 = (p01 `shiftR` sh) .&. 255
                  c11 = (p11 `shiftR` sh) .&. 255
                  top = c00 * (256 - a) + c10 * a
                  bot = c01 * (256 - a) + c11 * a
              in (top * (256 - b) + bot * b) `shiftR` 16
  in fromIntegral ((ch 16 `shiftL` 16) .|. (ch 8 `shiftL` 8) .|. ch 0)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  -- ←→↑↓ は押している間ずっと効く
  let th = wTheta w0 + (if isDown env (SpecialKey KeyRight) then 0.01 else 0) - (if isDown env (SpecialKey KeyLeft) then 0.01 else 0)
      s1 = if isDown env (SpecialKey KeyUp) then min 0.97 (wS w0 + 0.004) else wS w0
      s2 = if isDown env (SpecialKey KeyDown) then max 0.2 (s1 - 0.004) else s1
      w1 = w0 { wTheta = th, wS = s2 }
  -- 1. 今の映像（止めていなければ更新）
  (live, src) <- case wCam w1 of
    Just cam | not (wFrozen w1) -> do
      l <- camLive cam
      if l then (,) True . fromCamera <$> cameraFrame cam
           else pure (False, pattern (envFrame env))
    Just cam -> do
      l <- camLive cam
      pure (l, wSrc w1)
    Nothing -> pure (False, if wFrozen w1 then wSrc w1 else pattern (envFrame env))
  -- 2. 今の映像の上に、前のフレームを T で縮めて回して貼る
  pure w1 { wLive = live, wSrc = src, wFb = feedback w1 src }

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Translate (ww / 2) (hh / 2) (Scale k (negate ky)
      (pixelImage fw fh 1 (\x y -> unpack (U.unsafeIndex (wFb w) (y * fw + x))))) ]
  -- 不動点と、四隅の点が T をくり返すとどこへ行くか
  ++ [ Color (hsba 0 0 100 0.7) (thickPath 1.5 [ toS q | q <- take 14 (iterate (mapT w) p) ]) | p <- corners ]
  ++ [ hoop 3 (hsb 0 90 100) (cxs, cys) 30
     , ln 3 (hsb 0 90 100) (cxs - 22, cys) (cxs + 22, cys)
     , ln 3 (hsb 0 90 100) (cxs, cys - 22) (cxs, cys + 22)
     , fillBox (makeColor 0 0 0 0.55) 0 0 ww 84
     , txt 20 34 15 white ("scale s = " ++ nf 2 (wS w) ++ " (Up/Down)  rotation " ++ nf 0 (wTheta w * 180 / pi) ++ " deg (Left/Right)  click: center  F: freeze " ++ (if wFrozen w then "ON" else "off") ++ "  B: border  S: save" ++ (if wLive w then "" else "  (no camera: pattern)"))
     , txt 20 60 14 (hsb 0 0 75) "Red cross = fixed point c (T does not move it). White lines = the 4 corners under T, T^2, T^3 ... sucked into c."
     , txt 20 78 14 (hsb 0 0 75) "s near 1: slow pull. Adding rotation makes a spiral."
     ]
  where
    ww = envW env
    hh = envH env
    k = ww / fromIntegral fw
    ky = hh / fromIntegral fh
    toS (x, y) = (x * k, y * ky)
    (cxs, cys) = toS (wC w)
    ffw = fromIntegral fw
    ffh = fromIntegral fh
    corners = [(0, 0), (ffw, 0), (ffw, ffh), (0, ffh)]

mapT :: World -> Point -> Point
mapT w (px, py) =
  let (cx, cy) = wC w
      dx = px - cx
      dy = py - cy
      cs = cos (wTheta w)
      sn = sin (wTheta w)
      s = wS w
  in (cx + s * (cs * dx - sn * dy), cy + s * (sn * dx + cs * dy))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my > 84 -> pure w { wC = (mx / envW env * fromIntegral fw, my / envH env * fromIntegral fh) }
  EventKey (Char c) Down _ _
    | toLower c == 'f' -> pure w { wFrozen = not (wFrozen w) }
    | toLower c == 'b' -> pure w { wBorder = not (wBorder w) }
    | toLower c == 's' -> saveSvg "droste" env (drawWorld env w) >> pure w
  _ -> pure w
  where (mx, my) = mouseP env

-- ---------- やってみよう ----------
-- 1. θ = 0 にすると、ただの「合わせ鏡」のトンネル。c をクリックで動かして、トンネルの奥がどこを向くか見よう
-- 2. 手をカメラの前で動かすと、動きが渦を巻いて奥へ流れていく。s を 0.95 にすると？
-- 3. F で止めてから中心を動かすと、絵がどう変わる？ 止めても不動点は c のまま
-- 4. feedback の sn の符号を片方だけ変えると（裏返し）、不動点はどうなる？ 04 の地図と比べよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

pack :: Word8 -> Word8 -> Word8 -> Word32
pack r g b = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

packC :: Color -> Word32
packC c =
  let (r, g, b, _) = rgbaOfColor c
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int))) :: Word8
  in pack (q r) (q g) (q b)

unpack :: Word32 -> (Word8, Word8, Word8)
unpack v = (fromIntegral (v `shiftR` 16), fromIntegral ((v `shiftR` 8) .&. 255), fromIntegral (v .&. 255))
