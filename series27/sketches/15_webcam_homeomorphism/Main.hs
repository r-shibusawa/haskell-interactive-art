-- ============================================
-- 15 カメラ映像を同相変形
-- 自分の顔をゴムのように連続に変形する。ねじっても、うずまきにしても、円で裏返しても「つながり方」は変わらない ＝ 同相写像
--   どの変形にも「元に戻す変形（逆写像）」がある。画面の各点は、逆写像で元の映像のどこを見ればいいかを計算して色を決めている
-- マウス = 変形の中心 / 1 = ねじり / 2 = うずまき / 3 = 円の反転 / 4 = メビウス変換 / 5 = ふくらみ / ↑↓ = 強さ / G = 方眼 / P = 映像 ↔ 模様 / S = 保存
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

sW, sH, oW, oH :: Int
sW = 160             -- カメラの解像度
sH = 120
oW = 320             -- ★ 変形した画像の解像度（大きいほどきれいで重い）
oH = 240

width, height, dispH, dispW, dispX, dispY :: Float
width = 1280
height = 800
dispH = min (height - 140) ((width - 40) * 0.75)
dispW = dispH / 0.75
dispX = (width - dispW) / 2
dispY = 100

data World = World
  { wCam :: !(Maybe Camera)
  , wFrame :: !B.ByteString     -- 元の映像（RGB）
  , wFromCam :: !Bool           -- 元の映像がカメラか
  , wMode :: !Int
  , wStrength :: !Float         -- ★ 変形の強さ
  , wGrid, wUsePattern :: !Bool
  , wCx, wCy :: !Float          -- 変形の中心（正規化座標）
  -- ↓ 変形や映像が変わったときに作り直す（必要になるまで計算されない）
  , wLut :: U.Vector Int        -- 出力の各画素が見る元の映像の位置（−1 = 映像の外）
  , wOut :: Picture
  , wSrcPic :: Picture
  , wGridPic :: Picture
  }

main :: IO ()
main = runSketch (sketch "27-15 Homeomorphic Webcam" (rewarp world0))
  { skBack  = gray (20 / 255)
  , skStart = \_ -> do
      cam <- startCamera sW sH
      pure (rewarp world0 { wCam = Just cam })
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

world0 :: World
world0 = World Nothing patternBytes False 1 2.5 True False 0 0 U.empty Blank (srcImage patternBytes) Blank

-- | ★ 変形 f（dir = 1）と、その逆（dir = −1）。座標は画面の中心が (0, 0)、上下の端が ±1
warp :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
warp mode strength cx cy dir x y =
  let dx = x - cx
      dy = y - cy
      r = sqrt (dx * dx + dy * dy)
  in case mode of
       _ | mode == 1 || mode == 2 ->
         let a0 = if mode == 1
                    then let u = max 0 (1 - r / 0.9) in strength * u * u
                    else strength * exp (-(r * r) / 0.12)
             a = a0 * dir
             ca = cos a
             sa = sin a
         in (cx + dx * ca - dy * sa, cy + dx * sa + dy * ca)
       3 -> -- 円の反転：中心からの距離 r → ρ²/r（内と外が入れかわる。自分自身が逆写像）
         let rho = 0.15 + strength * 0.08
             s = rho * rho / max (r * r) 1e-6
         in (cx + dx * s, cy + dy * s)
       4 -> -- メビウス変換 w = (z − a)/(1 − ā z)：円板を円板にうつし、点 a を中心へ
         let ax = cx * 0.9
             ay = cy * 0.9
             sgn = if dir > 0 then -1 else 1
             nr = x + sgn * ax                         -- z ∓ a
             ni = y + sgn * ay
             dr = 1 + sgn * (ax * x + ay * y)          -- 1 ∓ ā z
             di = sgn * (ax * y - ay * x)
             m0 = dr * dr + di * di
             m = if m0 == 0 then 1e-9 else m0
         in ((nr * dr + ni * di) / m, (ni * dr - nr * di) / m)
       _ -> -- ふくらみ：r → r^γ（γ < 1 で中心がふくらむ）
         let g = (1 / (1 + strength * 0.3)) ** dir
             rR = 0.9
         in if r >= rR || r < 1e-9 then (x, y)
            else let s = rR * (r / rR) ** g / r in (cx + dx * s, cy + dy * s)

-- | 出力の各画素について、逆写像で元の映像のどこを見るかを表にする
buildLut :: World -> U.Vector Int
buildLut w =
  let inv = warp (wMode w) (wStrength w) (wCx w) (wCy w) (-1)
      halfH = fromIntegral oH / 2 :: Float
      halfW = fromIntegral oW / 2 :: Float
      sh2 = fromIntegral sH / 2 :: Float
      sw2 = fromIntegral sW / 2 :: Float
      look k =
        let (j, i) = k `divMod` oW
            x = (fromIntegral i - halfW) / halfH
            y = (fromIntegral j - halfH) / halfH
            (qx, qy) = inv x y
            fx = qx * sh2 + sw2
            fy = qy * sh2 + sh2
        in if fx >= 0 && fx < fromIntegral sW && fy >= 0 && fy < fromIntegral sH
             then (floor fy * sW + floor fx) * 3
             else -1
  in U.generate (oW * oH) look

outImage :: U.Vector Int -> B.ByteString -> Picture
outImage lut frame = pixelImage oW oH (dispW / fromIntegral oW) (\i j ->
  let o = U.unsafeIndex lut (j * oW + i)
  in if o < 0 then (30, 30, 40)
     else (BU.unsafeIndex frame o, BU.unsafeIndex frame (o + 1), BU.unsafeIndex frame (o + 2)))

srcImage :: B.ByteString -> Picture
srcImage frame = pixelImage sW sH 1 (\i j ->
  let o = (j * sW + i) * 3
  in (BU.unsafeIndex frame o, BU.unsafeIndex frame (o + 1), BU.unsafeIndex frame (o + 2)))

toScr :: (Float, Float) -> (Float, Float)
toScr (x, y) = (dispX + dispW / 2 + x * dispH / 2, dispY + dispH / 2 + y * dispH / 2)

-- | 方眼：元の映像のまっすぐな線を前向きの変形で写す
buildGrid :: World -> Picture
buildGrid w =
  let fwd = warp (wMode w) (wStrength w) (wCx w) (wCy w) 1
      ok (px, py) = not (isNaN px || isNaN py || isInfinite px || isInfinite py) && abs px < 3 && abs py < 3
      lineOf :: Int -> Int -> [Picture]
      lineOf k d =
        let fk = fromIntegral k / 6 :: Float
            ps = [ if d == 0 then fwd (fk * 1.33) s else fwd (s * 1.33 / 1.4) fk
                 | n <- [0 .. 140 :: Int], let s = -1.4 + fromIntegral n * 0.02 ]
            -- 説明文の上には線を描かない
            above p = snd (toScr p) < 90
            seg a b = if ok a && ok b && not (above a || above b) then [thickLine 1.5 (pt (toScr a)) (pt (toScr b))] else []
        in concat (zipWith seg ps (drop 1 ps))
  in Color (rgba255 255 255 120 150) (Pictures (concat [lineOf k d | k <- [-6 .. 6], d <- [0, 1]]))

-- | 変形を変えたときに作り直す
rewarp :: World -> World
rewarp w =
  let lut = buildLut w
  in w { wLut = lut, wOut = outImage lut (wFrame w), wGridPic = buildGrid w }

---------------------------------------------------------------- 動き

stepWorld :: World -> IO World
stepWorld w = do
  live <- maybe (pure False) camLive (wCam w)
  let useCam = live && not (wUsePattern w)
  if useCam
    then do
      frame <- maybe (pure B.empty) cameraFrame (wCam w)
      if B.length frame < sW * sH * 3 then pure w
        else pure w { wFrame = frame, wFromCam = True, wOut = outImage (wLut w) frame, wSrcPic = srcImage frame }
    else if wFromCam w
      then pure w { wFrame = patternBytes, wFromCam = False, wOut = outImage (wLut w) patternBytes, wSrcPic = srcImage patternBytes }
      else pure w

setCenter :: Point -> World -> World
setCenter pos w =
  let (mx, my) = toP5 pos
      cx0 = clampF (-1.3) 1.3 ((mx - dispX - dispW / 2) / (dispH / 2))
      cy0 = clampF (-1) 1 ((my - dispY - dispH / 2) / (dispH / 2))
      r = sqrt (cx0 * cx0 + cy0 * cy0)
      (cx, cy) = if wMode w == 4 && r > 0.95 then (cx0 * 0.95 / r, cy0 * 0.95 / r) else (cx0, cy0)
  in w { wCx = cx, wCy = cy }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion pos -> pure (rewarp (setCenter pos w))
  EventKey (Char c) Down _ _
    | c `elem` "12345" -> pure (rewarp (setCenter (envMouse env) w { wMode = fromEnum c - fromEnum '0' }))
    | c `elem` "gG" -> pure w { wGrid = not (wGrid w) }
    | c `elem` "pP" -> pure w { wUsePattern = not (wUsePattern w) }
    | c `elem` "sS" -> saveSvg "homeomorphism" env (drawWorld w) >> pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rewarp w { wStrength = min 8 (wStrength w + 0.5) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rewarp w { wStrength = max 0 (wStrength w - 0.5) })
  _ -> pure w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let ready = wFromCam w
      m = toScr (wCx w, wCy w)
      name = case wMode w of
        1 -> "Twist: turn more near the center"
        2 -> "Swirl: a whirl only around the center"
        3 -> "Inversion: swap inside and outside of a circle (the center goes to infinity)"
        4 -> "Mobius: carry point a of the disk to the center (a 'shift' inside the disk)"
        _ -> "Bulge: puff up the center"
      fH = fromIntegral sH :: Float
      strengthS = let s10 = round (wStrength w * 10) :: Int in show (s10 `div` 10) ++ "." ++ show (s10 `mod` 10)
  in Pictures
       [ at (pt (dispX + dispW / 2, dispY + dispH / 2)) (wOut w)
       , if wGrid w then wGridPic w else Blank
       , at (pt m) (Color (rgb255 255 80 80) (ring 8 2))
       -- 元の映像（小さく）
       , at (pt (20 + fromIntegral sW / 2, height - fH - 20 + fH / 2)) (wSrcPic w)
       , txt 20 (height - fH - 28) 14 (gray 1) "Source"
       , txt 20 26 16 (gray 1) ("Mouse: center   1 twist   2 swirl   3 inversion   4 Mobius   5 bulge   Up/Down: strength " ++ strengthS)
       , txt 20 50 16 (gray 1) ("G: grid   P: " ++ (if ready then "camera" else "pattern") ++ "   S: save"
                                ++ (if ready then "" else "   (no camera -> pattern)"))
       , txt 20 74 14 (gray (160 / 255)) (name ++ ". Yellow = straight lines")
       ]

-- ---------- やってみよう ----------
-- 1. ねじりの強さを 8 にしても、顔はちぎれない（つながり方は同じ）。何回ねじったら元に戻ると思う？
-- 2. 円の反転で、自分の顔を円の中に閉じこめよう。円の上の点は動かない（不動点）
-- 3. warp に自分の変形を足そう。「逆写像」も書けないと画面は描けない。逆が書けない変形はある？
-- 4. 方眼をよく見ると、変形しても直角に交わったまま（メビウス変換・反転は角度を保つ「等角写像」）

-- ==================================================
-- ここから下は「道具」（カメラがないときの模様、座標の変換）
-- ==================================================

patternBytes :: B.ByteString
patternBytes = B.pack (concat [ patternAt x y | y <- [0 .. sH - 1], x <- [0 .. sW - 1] ])

-- | 市松模様の上にニコニコの顔
patternAt :: Int -> Int -> [Word8]
patternAt ix iy =
  let x = fromIntegral ix + 0.5 :: Float
      y = fromIntegral iy + 0.5 :: Float
      inC cx cy d = dist2 (x, y) (cx, cy) <= d / 2
      nx = (x - 80) / 18
      ny = (y - 64) / 13
      an = atan2 ny nx
      smile = abs (sqrt (nx * nx + ny * ny) - 1) * 15 < 1.5 && an > 0.2 && an < pi - 0.2
      odd' = odd (ix `div` 10 + iy `div` 10)
  in if smile || inC 68 52 10 || inC 92 52 10 then [40, 40, 40]
     else if inC 80 60 70 then [255, 220, 80]
     else if odd' then [235, 200, 120]
     else [60, 90, 180]

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
