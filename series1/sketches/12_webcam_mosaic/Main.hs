-- ============================================
-- 12 Webカメラ・ドットアート（おまけ）
-- カメラの映像を、色つきの丸の集まりで表現する
-- 明るいところほど丸が大きくなる
-- 1〜3 キーでモード切り替え / S で保存（カメラが使えないときはデモ映像）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B

data World = World
  { wCam   :: !(Maybe Camera)
  , wFrame :: !B.ByteString    -- 最新の映像（R, G, B, R, G, B, ... と並んでいる）
  , wLive  :: !Bool
  , wMode  :: !Int
  }

cols, rows :: Int
cols = 64                 -- ★ 横の粒の数
rows = 48                 -- ★ 縦の粒の数

background :: Color
background = gray (10 / 255)

main :: IO ()
main = runSketch (sketch "01-12 Webcam Mosaic" (World Nothing B.empty False 1))
  { skBack  = background
  , skStart = \_ -> do
      cam <- startCamera cols rows     -- 小さいサイズで取り込む（=粗いモザイク）
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

drawWorld :: Env -> World -> Picture
drawWorld env w
  | B.length frame < cols * rows * 3 = Blank     -- カメラの準備ができるまで待つ
  | otherwise = Pictures (dots ++ [hint])
  where
    frame = wFrame w
    -- 1 マスの大きさ（画面に収まるように決める）
    cell = min (envW env / fromIntegral cols) (envH env / fromIntegral rows)
    px x = (fromIntegral x + 0.5 - fromIntegral cols / 2) * cell
    py y = (fromIntegral rows / 2 - fromIntegral y - 0.5) * cell
    -- startCamera の映像はもう左右反転してあるので、そのまま並べると鏡になる
    dots = [ dot (wMode w) cell (px x, py y) (rgbAt frame x y)
           | y <- [0 .. rows - 1], x <- [0 .. cols - 1] ]
    hint | wLive w = Blank
         | otherwise = Pictures
             [ Color (rgba255 0 0 0 170) (rect (-envW env / 2 + 130) (envH env / 2 - 30) 240 32)
             , label (-envW env / 2 + 20) (envH env / 2 - 36) 16 white "No camera: demo video" ]

-- 1 マスぶんの絵
dot :: Int -> Float -> Point -> (Int, Int, Int) -> Picture
dot mode cell pos@(x, y) (r, g, b) = case mode of
  -- 色つきの丸（明るいほど大きい）
  1 -> Color col (at pos (disc (remap bright 0 255 1 (cell * 1.3) / 2)))
  -- 白黒の丸
  2 -> Color white (at pos (disc (remap bright 0 255 0 cell / 2)))
  -- 明るさで回転する線（y は上向きなので、向きの符号を反対にする）
  _ -> let a = remap bright 0 255 0 pi
           dx = cos a * cell / 2
           dy = sin a * cell / 2
       in Color col (thickLine (cell * 0.2) (x - dx, y + dy) (x + dx, y - dy))
  where
    bright = fromIntegral (r + g + b) / 3 :: Float   -- 明るさ
    col = rgb255 r g b

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wMode = 2 }
  EventKey (Char '3') Down _ _ -> pure w { wMode = 3 }
  EventKey (Char c) Down _ _
    | c == 's' || c == 'S' -> do
        saveSvg "webcam-art" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), drawWorld env w])
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. cols = 32, rows = 24 にすると？（もっと粗くなる）
-- 2. 丸の代わりに rect 0 0 s s や文字 label で描いてみよう
-- 3. 自分だけのモード 4 をつくろう

-- ---------- 道具 ----------

-- (x, y) の色（赤, 緑, 青）。0〜255
rgbAt :: B.ByteString -> Int -> Int -> (Int, Int, Int)
rgbAt f x y =
  let i = (x + y * cols) * 3
  in (fromIntegral (B.index f i), fromIntegral (B.index f (i + 1)), fromIntegral (B.index f (i + 2)))
