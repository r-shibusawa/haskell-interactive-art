-- ============================================
-- 14 タイル・デザイナー 🔊（自由制作のベース）
-- 左のマス目に 1 枚のタイルを描くと、右に並べた模様がその場で変わる
--   並べ方：平行移動・回転・鏡映・すべり鏡映・ハーフドロップ。描くときの対称（ミラー）も選べる
--   スペース = タイルを左から右へ「楽譜」として演奏（上ほど高い音）
-- ドラッグ = 描く / 下の色をクリック = 色 / E = 消しゴム / M = 描くときの対称 / 1〜5 = 並べ方 / P = 配色 / R = ランダム / C = 消す / ↑↓ = 大きさ / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

gG :: Int
gG = 12                 -- ★ タイルのマス数（G × G）

-- 左のエディタの位置と 1 マスの大きさ
es, ex, ey :: Float
es = 26
ex = 30
ey = 110

data World = World
  { wTile    :: !Float           -- ★ 右側に並べるタイルの大きさ
  , wCells   :: !(U.Vector Int)  -- 色番号（0 = 背景）
  , wBrush   :: !Int
  , wEraser  :: !Bool
  , wSym     :: !Int             -- 描くときの対称 0〜4
  , wArrange :: !Int             -- 並べ方 1〜5
  , wPal     :: !Int
  , wPlaying :: !Bool
  , wPlayCol :: !Int
  , wLastPaint :: !Int
  , wAudio   :: !Bool
  }

world0 :: World
world0 = World 72 (U.replicate (gG * gG) 0) 1 False 1 2 0 False 0 (-1) False

main :: IO ()
main = runSketch (sketch "32-14 Tile Designer" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> randomTile world0
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 配色（先頭が背景）
palettes :: [[(Word8, Word8, Word8)]]
palettes =
  [ [(245, 238, 225), (30, 60, 110), (220, 80, 60), (240, 180, 50), (70, 150, 120)]
  , [(20, 22, 30), (250, 210, 90), (90, 200, 230), (240, 100, 150), (240, 240, 240)]
  , [(230, 225, 210), (120, 80, 50), (190, 140, 90), (60, 90, 70), (30, 30, 30)]
  ]

palOf :: World -> Int -> (Word8, Word8, Word8)
palOf w k = case drop k (concat (take 1 (drop (wPal w) palettes))) of
  (c : _) -> c
  [] -> (0, 0, 0)

toColor :: (Word8, Word8, Word8) -> Color
toColor (r, g, b) = rgb255 (fromIntegral r) (fromIntegral g) (fromIntegral b)

symName :: Int -> String
symName i = case i of
  0 -> "none"
  1 -> "left-right"
  2 -> "up-down-left-right"
  3 -> "4-fold rotation"
  _ -> "diagonal"

arrName :: Int -> String
arrName i = case i of
  1 -> "translation"
  2 -> "rotation (4 make a set)"
  3 -> "mirror"
  4 -> "glide mirror"
  _ -> "half drop"

-- ★ 演奏：8 フレームごとに 1 列進み、色のあるマスを鳴らす
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wPlaying w && envFrame env `mod` 8 == 0 = do
      forM_ [0 .. gG - 1] $ \r -> do
        let v = wCells w U.! (r * gG + wPlayCol w)
        when (v > 0 && wAudio w) (noteFor r v 0.06)
      pure w { wPlayCol = (wPlayCol w + 1) `mod` gG }
  | otherwise = pure w

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      x0 = 420
      ts = wTile w
      g = fromIntegral gG :: Float
      -- タイルの画像を作る（1 マス = 1 画素）。同じ画像を置き方を変えて使い回す
      tileImg = pixelImage gG gG 1 (\x y -> palOf w (wCells w U.! (y * gG + x)))
      unit = Scale (ts / g) (ts / g) tileImg
      nx = ceiling ((wd - x0) / ts) + 1 :: Int
      ny = ceiling (h / ts) + 1 :: Int
      -- 画面の中央が原点の座標に直して置く
      place x y = Translate (x + ts / 2 - wd / 2) (h / 2 - (y + ts / 2))
      copies = [ place (x0 + fromIntegral i * ts) (fromIntegral j * ts) (drawCopy w ts unit i j)
               | j <- [-1 .. ny - 1], i <- [0 .. nx - 1] ]
      gridY = ey + g * es
      editor = [ Pictures
                   [ Color (toColor (palOf w (wCells w U.! i))) (box (ex + cx * es) (ey + cy * es) es es)
                   , Color (gray 0.24) (rectLine (ex + cx * es + es / 2) (ey + cy * es + es / 2) es es) ]
               | i <- [0 .. gG * gG - 1]
               , let cx = fromIntegral (i `mod` gG)
                     cy = fromIntegral (i `div` gG) ]
      playBar = if wPlaying w
        then Color (rgb255 255 60 60) (thickBox (ex + fromIntegral (wPlayCol w) * es) ey es (g * es) 3)
        else Blank
      swatches = [ Pictures
                     [ Color (toColor (palOf w k)) (box sx (gridY + 20) 40 40)
                     , if selected then Color white (thickBox sx (gridY + 20) 40 40 4)
                       else Color (gray 0.31) (rectLine (sx + 20) (gridY + 40) 40 40) ]
                 | k <- [1 .. 4]
                 , let sx = ex + fromIntegral (k - 1) * 50
                       selected = k == wBrush w && not (wEraser w) ]
  in Pictures
    [ Pictures copies
    , -- 左の部分は上から塗って、模様を隠す
      onP5 env (Pictures (Color (rgb255 30 32 40) (box 0 0 x0 h) : editor ++ [playBar] ++ swatches))
    , txt env 30 40 20 white "Tile Designer"
    , txt env 30 68 15 (gray 0.78) ("Draw symmetry: " ++ symName (wSym w) ++ " (M)" ++ (if wEraser w then "  ERASER" else ""))
    , txt env 30 90 15 (gray 0.78) ("Layout: " ++ arrName (wArrange w) ++ " (1-5)")
    , txt env 30 (gridY + 90) 15 (gray 0.78) ("Space: play " ++ (if wPlaying w then "[stop]" else "[>]") ++ "  P colors  R random")
    , txt env 30 (gridY + 114) 15 (gray 0.78) "Up/Down size  E eraser  C clear"
    , txt env 30 (gridY + 138) 15 (gray 0.78) "S save  Click also makes sound"
    ]

-- ★ (i, j) 番目のタイルの置き方
drawCopy :: World -> Float -> Picture -> Int -> Int -> Picture
drawCopy w ts unit i j = case wArrange w of
  2 -> let turns = case (odd j, odd i) of        -- 4 枚で 1 周
             (False, False) -> 0
             (False, True) -> 1
             (True, False) -> 3
             (True, True) -> 2 :: Float
       in Rotate (turns * 90) unit
  3 -> Scale (if odd i then -1 else 1) (if odd j then -1 else 1) unit
  4 | odd j -> Translate (negate ts / 2) 0 (Scale (-1) 1 unit)
  5 | odd i -> Translate 0 (negate ts / 2) unit
  _ -> unit

paintAt :: Float -> Float -> World -> IO World
paintAt mx my w = do
  let cx = floor ((mx - ex) / es) :: Int
      cy = floor ((my - ey) / es) :: Int
  if cx < 0 || cy < 0 || cx >= gG || cy >= gG then pure w else do
    let v = if wEraser w then 0 else wBrush w
        cell = cy * gG + cx
    when (wLastPaint w /= cell && v > 0 && wAudio w) (noteFor cy v 0.08)
    pure w { wCells = wCells w U.// [ (b * gG + a, v) | (a, b) <- symCells (wSym w) cx cy ], wLastPaint = cell }

-- ★ 描くときの対称：1 マス塗ると、対応するマスも塗る
symCells :: Int -> Int -> Int -> [(Int, Int)]
symCells sym x y =
  let m = gG - 1
  in (x, y) : case sym of
       1 -> [(m - x, y)]
       2 -> [(m - x, y), (x, m - y), (m - x, m - y)]
       3 -> [(m - y, x), (m - x, m - y), (y, m - x)]
       4 -> [(y, x)]
       _ -> []

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let w = w0 { wAudio = True }
        (mx, my) = mouseOf env p
        y0 = ey + fromIntegral gG * es + 20
        hits = [ k | k <- [1 .. 4], let sx = ex + fromIntegral (k - 1) * 50, mx > sx, mx < sx + 40, my > y0, my < y0 + 40 ]
    in case hits of
         (k : _) -> pure w { wBrush = k, wEraser = False }
         [] -> paintAt mx my w
  EventMotion p | envDown env -> let (mx, my) = mouseOf env p in paintAt mx my w0
  EventKey (MouseButton LeftButton) Up _ _ -> pure w0 { wLastPaint = -1 }
  EventKey k Down _ _ -> keyDown env k w0 { wAudio = True }
  _ -> pure w0

keyDown :: Env -> Key -> World -> IO World
keyDown env k w = case k of
  Char c
    | c >= '1' && c <= '5' -> pure w { wArrange = fromEnum c - 48 }
    | c == 'm' || c == 'M' -> pure w { wSym = (wSym w + 1) `mod` 5 }
    | c == 'e' || c == 'E' -> pure w { wEraser = not (wEraser w) }
    | c == 'p' || c == 'P' -> pure w { wPal = (wPal w + 1) `mod` length palettes }
    | c == 'r' || c == 'R' -> randomTile w
    | c == 'c' || c == 'C' -> pure w { wCells = U.replicate (gG * gG) 0 }
    | c == 's' || c == 'S' -> saveSvg "my_tile" env (drawAll env w) >> pure w
  SpecialKey KeySpace -> pure w { wPlaying = not (wPlaying w), wPlayCol = 0 }
  SpecialKey KeyUp -> pure w { wTile = min 200 (wTile w + 12) }
  SpecialKey KeyDown -> pure w { wTile = max 24 (wTile w - 12) }
  _ -> pure w

randomTile :: World -> IO World
randomTile w = do
  let add cells _ = do
        x <- randomRIO (0, gG - 1)
        y <- randomRIO (0, gG - 1)
        v <- randomRIO (1, 4)
        pure (cells U.// [ (b * gG + a, v) | (a, b) <- symCells (wSym w) x y ])
  cells <- foldM add (U.replicate (gG * gG) 0) [1 .. 18 :: Int]
  pure w { wCells = cells }

-- ---------- やってみよう ----------
-- 1. 角に 4 分の 1 円を描いて 2（回転）にすると、円がつながる。トルシェ（11）と同じ考え
-- 2. 端のマスに描いた模様が、となりのタイルとつながるようにデザインしよう（左端と右端をそろえる）
-- 3. 4（すべり鏡映）や 5（ハーフドロップ）は壁紙やテキスタイルでよく使われる並べ方
-- 4. 演奏しながら描き足して、模様と音を同時にデザインしよう。gG を 16 にすると細かく描ける


-- ==================================================
-- ここから下は「道具」（座標・枠・音）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

mouseOf :: Env -> Point -> Point
mouseOf env (x, y) = (x + envW env / 2, envH env / 2 - y)

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y)、太さ t の枠
thickBox :: Float -> Float -> Float -> Float -> Float -> Picture
thickBox x y w h t = thickPath t [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- 行（上ほど高い）と色（音色）から音を作る
noteFor :: Int -> Int -> Float -> IO ()
noteFor row colorIdx vol = do
  let deg = gG - 1 - row
      penta = case deg `mod` 5 of { 0 -> 0; 1 -> 2; 2 -> 4; 3 -> 7; _ -> 9 } :: Int
      semis = 12 * (deg `div` 5) + penta
      f = 196 * 2 ** (fromIntegral semis / 12)
      wave = case colorIdx `mod` 5 of { 1 -> Triangle; 2 -> Square; 3 -> Sawtooth; _ -> Sine }
  playTone f vol 0.3 wave
