-- ============================================
-- 15 カメラ映像をレンズで見る（おまけ）
-- 画面の各点について「どこから光が来たか」を計算し、カメラ映像のその場所の色を持ってくる（逆向きの光線追跡）
--   1 = 凸レンズ（中心ほど拡大）  2 = 魚眼（視野を押し込む）  3 = 水面の波紋（屈折がゆらぐ）
--   カメラが使えないときはカラーバーの画像で動く
-- マウス = レンズの中心 / 1〜3 = モード / ↑↓ = 強さ / ←→ = レンズの大きさ
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Numeric (showFFloat)

vw, vh, ow, oh :: Int
vw = 160                   -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 120
ow = ceiling (canvasW / cell)
oh = ceiling (canvasH / cell)

cell, canvasW, canvasH :: Float
cell = 4                   -- ★ 出力の粗さ（px）
canvasW = 1280
canvasH = 800

data World = World
  { wCam      :: !(Maybe Camera)
  , wFrame    :: !B.ByteString     -- いまの映像（RGB）
  , wLive     :: !Bool
  , wKind     :: !Int
  , wStrength :: !Float            -- ★ 効果の強さ
  , wR        :: !Float            -- ★ レンズの半径（px）
  , wTime     :: !Float
  }

main :: IO ()
main = runSketch (sketch "36-15 Webcam Through a Lens" (World Nothing fallback False 1 1 260 0))
  { skBack  = black
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure (World (Just cam) fallback False 1 1 260 0)
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- カメラが使えないときの代わりの画像（カラーバー + 格子）
fallback :: B.ByteString
fallback = fst (B.unfoldrN (vw * vh * 3) (\i -> Just (px i, i + 1)) 0)
  where
    bar :: Int -> (Float, Float, Float)
    bar 0 = (255, 255, 255)
    bar 1 = (255, 220, 0)
    bar 2 = (0, 220, 220)
    bar 3 = (0, 200, 0)
    bar 4 = (220, 0, 220)
    bar 5 = (220, 0, 0)
    bar _ = (0, 0, 220)
    px :: Int -> Word8
    px i =
      let (j, ch) = i `divMod` 3
          (y, x) = j `divMod` vw
          (r, g, b) = bar (x * 7 `div` vw)
          k = if x `mod` 16 < 2 || y `mod` 16 < 2 then 0.35 else 1
          v = case ch of { 0 -> r; 1 -> g; _ -> b }
      in round (v * k)

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w0 = do
  let w = w0 { wTime = wTime w0 + 0.016 }
  case wCam w of
    Nothing -> pure w
    Just cam -> do
      live <- camLive cam
      if live
        then do
          f <- cameraFrame cam
          pure w { wLive = True, wFrame = if B.length f >= vw * vh * 3 then f else fallback }
        else pure w { wLive = False, wFrame = fallback }

-- ★ 画面の点（レンズ中心からの相対位置）→ 光が来た元の位置（相対）
sourceOffset :: World -> Float -> Float -> Point
sourceOffset w dx dy =
  let lensR = wR w
      strength = wStrength w
      r = sqrt (dx * dx + dy * dy)
  in case wKind w of
       1 | r >= lensR -> (dx, dy)
         | otherwise ->
             -- 凸レンズ（虫めがね）：中心で倍率 1 + strength、縁で 1 に戻る
             let u = r / lensR
                 mag = 1 + strength * (1 - u * u)
             in (dx / mag, dy / mag)
       2 | r >= lensR -> (dx, dy)
         | otherwise ->
             -- 魚眼：角度 θ に比例した距離に写る（等距離射影）。縁ほど押し込まれる
             let u = r / lensR
                 k = 0.3 + strength * 1.2
                 rr = tan (u * atan k) / k          -- 0〜1 → 0〜1（外側ほど急に伸びる）
                 s = rr * lensR / max r 1e-6
             in (dx * s, dy * s)
       _ | r < 1e-6 -> (dx, dy)
         | otherwise ->
             -- 水面の波紋：高さ h = sin(k r − ωt) の傾きで光が曲がる（屈折）
             let slope = cos (r * 0.08 - wTime w * 4) * exp (negate r / (lensR * 1.2)) * 0.08
                 shift' = slope * 220 * strength
             in (dx + dx / r * shift', dy + dy / r * shift')

kindName :: Int -> String
kindName 1 = "Convex lens (magnifier)"
kindName 2 = "Fisheye lens"
kindName _ = "Water ripples"

drawAll :: Env -> World -> Picture
drawAll env w =
  let (gmx, gmy) = envMouse env
      cx = gmx + canvasW / 2
      cy = canvasH / 2 - gmy
      f = wFrame w
      pixel i j =
        let x = fromIntegral i * cell + cell / 2
            y = fromIntegral j * cell + cell / 2
            (ox, oy) = sourceOffset w (x - cx) (y - cy)
            -- 画面の座標 → カメラ画像の座標（画面いっぱいに引き伸ばしてある）
            u = max 0 (min (vw - 1) (floor ((cx + ox) / canvasW * fromIntegral vw)))
            v = max 0 (min (vh - 1) (floor ((cy + oy) / canvasH * fromIntegral vh)))
            si = (v * vw + u) * 3
        in (BU.unsafeIndex f si, BU.unsafeIndex f (si + 1), BU.unsafeIndex f (si + 2))
      imgW = fromIntegral ow * cell
      imgH = fromIntegral oh * cell
  in Pictures
    [ Translate 0 ((canvasH - imgH) / 2) (Translate ((imgW - canvasW) / 2) 0 (pixelImage ow oh cell pixel))
    -- レンズの縁
    , at (gmx, gmy) (Color (rgba255 255 255 255 120) (ring (wR w) 2))
    , Color (rgba255 0 0 0 140) (rect (negate (envW env) / 2 + 10 + 620) (envH env / 2 - 14 - 35) 1240 70)
    , txt env 20 40 18 white
        (kindName (wKind w) ++ " (1-3)  Strength " ++ nf 1 (wStrength w) ++ " (Up/Down)  Radius " ++ nf 0 (wR w)
         ++ " (L/R)  " ++ (if wLive w then "Camera ON" else "No camera: test image"))
    , txt env 20 68 15 (gray 0.78)
        "Mouse = lens center. For each pixel we trace back where the light came from and take that color."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wStrength = min 3 (wStrength w + 0.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wStrength = max 0.2 (wStrength w - 0.2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wR = min 600 (wR w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wR = max 60 (wR w - 20) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wKind = fromEnum c - 48 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 凸レンズの倍率の式 mag を「1 − strength × (1 − u²)」（1 より小さく）にすると凹レンズ（縮小）になる
-- 2. 魚眼で strength を最大に。ドアののぞき穴・車のバックカメラと同じ見え方
-- 3. 波紋の中心を複数（マウス + 固定点）にして、波が重なる干渉（06）を見よう
-- 4. sourceOffset に「波長で変わるずれ」を足すと、色がにじむ（色収差）。RGB を別々にサンプリングしてみよう


-- ==================================================
-- ここから下は「道具」（文字・数字）
-- ==================================================
-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""
