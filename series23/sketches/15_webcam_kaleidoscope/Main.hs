-- ============================================
-- 15 カメラで万華鏡（おまけ）
-- カメラ映像の三角形を切り取り、鏡に映すように 2n 枚並べる = 二面体群 Dn の対称性
--   W = 壁紙モード：三角形を平面にしきつめる（3 枚の鏡の万華鏡 = 壁紙群 p3m1）
-- マウス = 映像のどこを切り取るか / ←→ = n（鏡の数）/ ↑↓ = 大きさ / W = 壁紙モード / スペース = 回転 / S = 保存
-- カメラが使えないときは、自動で動く模様を映す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word8)

vw, vh :: Int
vw = 320                 -- ★ カメラ画像の大きさ
vh = 240

patW, patH :: Int
patW = 160               -- カメラがないときの模様の大きさ（軽くするためカメラの半分）
patH = 120

imgW, imgH :: Int
imgW = 320               -- ★ 画面の絵の細かさ（4 倍に拡大して表示）
imgH = 200

data World = World
  { wCam   :: !(Maybe Camera)
  , wFrame :: !B.ByteString
  , wLive  :: !Bool
  , wN     :: !Int       -- ★ 鏡の数（Dn の n）
  , wSrcR  :: !Float     -- ★ 切り取る三角形の大きさ（映像の高さに対する割合）
  , wPaper :: !Bool      -- 壁紙モード
  , wSpin  :: !Bool
  , wSpinA :: !Float
  }

main :: IO ()
main = runSketch (sketch "23-15 Webcam Kaleidoscope" (World Nothing B.empty False 6 0.35 False False 0))
  { skBack  = black
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure (World (Just cam) B.empty False 6 0.35 False False 0)
  , skStep  = \env _ w -> step (envFrame env) w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Int -> World -> IO World
step frame w0 = do
  let w = if wSpin w0 then w0 { wSpinA = wSpinA w0 + 0.01 } else w0
  case wCam w of
    Nothing -> pure w
    Just cam -> do
      live <- camLive cam
      f <- if live then cameraFrame cam else pure (fallback frame)
      pure w { wFrame = f, wLive = live }

-- カメラがないときの、動く模様（24 個の丸と 1 本の白い線）
fallback :: Int -> B.ByteString
fallback frame = B.pack (U.toList (U.modify paint base))
  where
    base = U.generate (patW * patH * 3) (\i -> if i `mod` 3 == 2 then 40 else 20)
    fw = fromIntegral patW :: Float
    fh = fromIntegral patH :: Float
    fc = fromIntegral frame :: Float
    paint buf = do
      forM_ [0 .. 23 :: Int] $ \i -> do
        let t = fc * 0.01 + fromIntegral i
            ch a = 128 + 127 * sin a
            col = (ch (t * 1.3), ch (t * 0.7 + 2), ch (t + 4))
            cx = fw / 2 + sin (t * 0.9) * fw * 0.4
            cy = fh / 2 + cos (t * 1.1) * fh * 0.4
            d = (18 + 12 * sin (t * 2)) / 2      -- 模様が半分の大きさなので丸も半分
        fillCircle buf col (200 / 255) cx cy (d / 2)
      -- 白い線
      let y1 = fh * (0.5 + 0.4 * sin (fc * 0.02))
          y2 = fh * 0.3
      forM_ [0 .. patW - 1] $ \x -> do
        let yc = y1 + (y2 - y1) * fromIntegral x / fw
        forM_ [floor yc, floor yc + 1] $ \y ->
          when (y >= 0 && y < patH) (blendPx buf (255, 255, 255) 1 x y)

fillCircle :: M.MVector s Word8 -> (Float, Float, Float) -> Float -> Float -> Float -> Float -> ST s ()
fillCircle buf col a cx cy r =
  forM_ [max 0 (floor (cy - r)) .. min (patH - 1) (ceiling (cy + r))] $ \y ->
    forM_ [max 0 (floor (cx - r)) .. min (patW - 1) (ceiling (cx + r))] $ \x ->
      when (dist2 (fromIntegral x + 0.5, fromIntegral y + 0.5) (cx, cy) <= r) (blendPx buf col a x y)

blendPx :: M.MVector s Word8 -> (Float, Float, Float) -> Float -> Int -> Int -> ST s ()
blendPx buf (r, g, b) a x y = do
  let i = (y * patW + x) * 3
      mix k c = do
        old <- M.unsafeRead buf (i + k)
        M.unsafeWrite buf (i + k) (round (fromIntegral old * (1 - a) + c * a :: Float))
  mix 0 r
  mix 1 g
  mix 2 b

-- 絵の各画素の、中心からの角度（毎回 atan2 を計算しないように先に表にしておく）
angleTable :: U.Vector Float
angleTable = U.generate (imgW * imgH) $ \i ->
  let (y, x) = i `divMod` imgW
      (px, py) = pixelXY x y
  in wrapF tau (atan2 py px)

-- 画素 (x, y) の座標（中心が原点、y は下向き、画面のピクセル単位）
pixelXY :: Int -> Int -> (Float, Float)
pixelXY x y = ((fromIntegral x + 0.5) * 4 - width / 2, (fromIntegral y + 0.5) * 4 - height / 2)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (image ++ hud)
  where
    -- 切り取る三角形（映像の座標 0〜1）：中心はマウス
    (mx, my) = toP5 (envMouse env)
    u0 = clampF 0.2 0.8 (mx / width)
    v0 = clampF 0.2 0.8 (my / height)
    r = wSrcR w
    n = wN w
    half = pi / fromIntegral n
    spinA = wSpinA w
    aspect = fromIntegral vh / fromIntegral vw
    second = spinA + (if wPaper w then pi / 3 else half)
    vA = (u0, v0)
    vB = clampUV (u0 + r * cos spinA * aspect, v0 + r * sin spinA)
    vC = clampUV (u0 + r * cos second * aspect, v0 + r * sin second)
    frame = wFrame w
    (sw, sh) = if wLive w then (vw, vh) else (patW, patH)   -- 今の映像の大きさ
    ready = B.length frame >= sw * sh * 3
    sample (u, v) =
      let x = min (sw - 1) (max 0 (floor (u * fromIntegral sw)))
          y = min (sh - 1) (max 0 (floor (v * fromIntegral sh)))
          i = (y * sw + x) * 3
      in (BU.unsafeIndex frame i, BU.unsafeIndex frame (i + 1), BU.unsafeIndex frame (i + 2))
    image | not ready = []
          | wPaper w = [pixelImage imgW imgH 4 (wallpaperPixel sample r vA vB vC)]
          | otherwise = [pixelImage imgW imgH 4 (kaleidoPixel sample n half vA vB vC)]
    hud =
      [ box 0 0 width 84 (rgba255 0 0 0 150)
      , txt 20 34 18 white ("Mouse: where to cut   Left/Right: n = " ++ show n ++ "   Up/Down: size   W: "
          ++ (if wPaper w then "wallpaper (p3m1)" else "kaleidoscope (D" ++ show n ++ ")") ++ "   Space: spin   S: save")
      , txt 20 62 15 (gray 0.75) ((if wLive w then "Camera. " else "No camera -> moving pattern. ")
          ++ (if wPaper w then "A triangle bounded by 3 mirrors fills the whole plane (translations appear too)"
                          else "Symmetries: " ++ show n ++ " rotations + " ++ show n ++ " mirrors = " ++ show (2 * n)))
      ]

clampUV :: Point -> Point
clampUV (a, b) = (clampF 0 1 a, clampF 0 1 b)

-- 3 点の重み（w1, w2, w3）で映像の座標を混ぜる
mixUV :: Float -> Point -> Float -> Point -> Float -> Point -> Point
mixUV w1 (a1, b1) w2 (a2, b2) w3 (a3, b3) = (w1 * a1 + w2 * a2 + w3 * a3, w1 * b1 + w2 * b2 + w3 * b3)

-- Dn：2n 枚の三角形。となりどうしは鏡映（B と C を入れかえる）
kaleidoPixel :: (Point -> (Word8, Word8, Word8)) -> Int -> Float -> Point -> Point -> Point -> Int -> Int -> (Word8, Word8, Word8)
kaleidoPixel sample n half vA vB vC x y =
  let (px, py) = pixelXY x y
      th = U.unsafeIndex angleTable (y * imgW + x)
      i = min (2 * n - 1) (floor (th / half)) :: Int
      a1 = fromIntegral i * half
      -- 三角形の 1 辺が x 軸に重なるように回す
      lx = px * cos a1 + py * sin a1
      ly = py * cos a1 - px * sin a1
      rr = height * 0.48          -- ★ 万華鏡の半径
      t = ly / (rr * sin half)
      s = (lx - t * rr * cos half) / rr
      (p0, p1) = if even i then (vB, vC) else (vC, vB)
  in if s + t > 1 then (0, 0, 0) else sample (mixUV (1 - s - t) vA s p0 t p1)

-- 正三角形のしきつめ：格子点 (i, j) に 3 色の番号 (i − j) mod 3 をつけ、番号 → 映像の頂点 A, B, C
wallpaperPixel :: (Point -> (Word8, Word8, Word8)) -> Float -> Point -> Point -> Point -> Int -> Int -> (Word8, Word8, Word8)
wallpaperPixel sample srcR vA vB vC x y =
  let (px, py) = pixelXY x y
      l = height * 0.22 * (srcR / 0.35)
      by = l * sqrt 3 / 2
      fj = (py + height) / by
      fi = (px + width - fj * l / 2) / l
      i = floor fi :: Int
      j = floor fj :: Int
      a = fi - fromIntegral i
      b = fj - fromIntegral j
      lab ii jj = case (ii - jj) `mod` 3 of
        0 -> vA
        1 -> vB
        _ -> vC
      uv | a + b <= 1 = mixUV (1 - a - b) (lab i j) a (lab (i + 1) j) b (lab i (j + 1))
         | otherwise = mixUV (1 - b) (lab (i + 1) j) (a + b - 1) (lab (i + 1) (j + 1)) (1 - a) (lab i (j + 1))
  in sample uv

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wN = min 16 (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wN = max 2 (wN w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSrcR = min 0.6 (wSrcR w + 0.03) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSrcR = max 0.1 (wSrcR w - 0.03) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpin = not (wSpin w) }
  EventKey (Char c) Down _ _
    | c == 'w' || c == 'W' -> pure w { wPaper = not (wPaper w) }
    | c == 's' || c == 'S' -> do
        saveSvg "kaleidoscope" env (drawWorld env w)
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 2, 3, 4… と変えて、鏡の数と模様の「花びら」の数の関係を見よう（花びら n 枚、三角形 2n 枚）
-- 2. 隣どうしを鏡映でなく「同じ向き」に並べると（kaleidoPixel の (p0, p1) を常に (vB, vC) に）巡回群 Cn の模様になる。違いは？
-- 3. 壁紙モードの三角形を 60° でなく 90°-45°-45° にすると正方形の万華鏡（p4m）になる。映像の座標の作り方を変えてみよう
-- 4. 顔を近づけたり手を動かしたりして、気に入った模様を S で保存しよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)
