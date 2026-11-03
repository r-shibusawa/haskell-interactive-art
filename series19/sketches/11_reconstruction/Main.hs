-- ============================================
-- 11 再構成と穴埋め（測地的膨張）
-- マーカー（種）を「マスク（元の画像）の中だけで」膨張しつづける = 測地的膨張。止まったものが「再構成」
-- 種を置いた塊だけ取り出す ／ 穴を埋める ／ 画面のふちに触れた物体を消す ── 全部この 1 つの道具で
-- クリックで種を置く（モード 1） / 1〜3 でモード / スペースでやり直し / R で別の画像
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

data World = World
  { wMask    :: !Grid
  , wRecMask :: !Grid           -- 育ってよい場所
  , wRec     :: !Grid           -- 育った部分
  , wArrival :: !(U.Vector Int) -- 届いた時刻（-1 = まだ）
  , wMarker  :: !(Maybe Grid)   -- クリックで置いた種
  , wMode    :: !Int            -- ★ 1 = 種から再構成 2 = 穴埋め 3 = ふちに触れた物体を消す
  , wIter    :: !Int
  , wDone    :: !Bool
  , wSeed    :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-11 Reconstruction and Hole Filling" (makeMask world0))
  { skBack  = hsb 170 45 9
  , skStep  = \env _ w -> pure (if not (wDone w) && envFrame env `mod` 3 == 0 then stepGeo w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e e (U.replicate (cols * rows) (-1)) Nothing 1 0 False 1
  where e = U.replicate (cols * rows) False

se :: [(Int, Int)]
se = makeSE Box 1  -- ★ 測地的膨張の SE（3×3 = 8 近傍）

-- ★ 画像：穴のある塊、ふちに触れる塊、小さな塊
makeMask :: World -> World
makeMask w = startMode w { wMask = gridFrom inBlob }
  where
    us = randomRs (0, 1 :: Float) (mkStdGen (wSeed w))
    fourAt n = take 4 (drop (4 * n) us)
    blob n = case fourAt n of
      [a, b, c, d] -> (8 + a * (fi cols - 16), 8 + b * (fi rows - 16), 6 + c * 8, d < 0.6)
      _ -> (0, 0, 0, False)
    blobs = map blob [0 .. 6]
      ++ [ (2, fi rows - 12, 9, False)    -- 左下のふちに触れる
         , (fi cols - 3, 12, 8, True) ]   -- 右上のふちに触れる
    inBlob x y = any (\(cx, cy, r, hole) ->
      let d = distI x y cx cy in d <= r && not (hole && d < r * 0.45)) blobs

isBorder :: Int -> Bool
isBorder i =
  let x = i `rem` cols
      y = i `quot` cols
  in x == 0 || y == 0 || x == cols - 1 || y == rows - 1

-- ★ モードごとに「マスク」と「種」を決める
startMode :: World -> World
startMode w = w { wRecMask = recMask, wRec = rec0, wArrival = U.map (\v -> if v then 0 else -1) rec0, wIter = 0, wDone = False }
  where
    mask = wMask w
    onBorder m = U.imap (\i v -> v && isBorder i) m
    (recMask, rec0) = case wMode w of
      1 -> (mask, case wMarker w of                      -- マスクの中で種を育てる
                    Just m -> U.zipWith (&&) m mask
                    Nothing -> U.map (const False) mask)
      2 -> let bg = U.map not mask in (bg, onBorder bg)  -- 背景の中で、ふちから育てる
      _ -> (mask, onBorder mask)                         -- 前景の中で、ふちから育てる

-- ★ 測地的膨張を 1 回：膨張してからマスクと AND。変わらなくなったら終わり
stepGeo :: World -> World
stepGeo w
  | wDone w = w
  | otherwise = w
      { wRec = next
      , wArrival = U.imap arrival (wArrival w)
      , wIter = wIter w + 1
      , wDone = U.and (U.zipWith (==) next rec0)
      }
  where
    rec0 = wRec w
    next = U.zipWith (&&) (dilateImg se rec0) (wRecMask w)
    arrival i a = if next U.! i && not (rec0 U.! i) then wIter w + 1 else a

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor
  , txt env px (ty + 20) 34 (hsb 45 90 100) ("Dilate x" ++ show (wIter w) ++ (if wDone w then " done" else ""))
  , txt env px (ty + 54) 15 (lum 100) ("Mode " ++ show mode ++ ": " ++ modeName mode)
  , txt env px (ty + 80) 15 (lum 100) ("Grown (colored): " ++ show (areaOf rec) ++ " cells")
  , txt env px (ty + 104) 15 (lum 100) ("Result foreground: " ++ show nOut ++ " cells")
  ]
  ++ map (\(dy, s) -> txt env px (ty + dy) 14 (lum 60) s) (zip [144, 168 ..] (modeNotes mode))
  ++ [ txt env px (ty + 280) 14 (lum 60) "Color = order of arrival"
     , txt env 20 40 17 (lum 100) "Click: place a seed (mode 1)   1-3: mode   Space: restart   R: new image"
     , txt env 20 68 14 (lum 60)
         "Plain dilation spreads forever. Limited to a mask (geodesic dilation) and repeated, only what connects to the seed remains."
     ]
  where
    c = cellOf env
    mode = wMode w
    mask = wMask w
    rec = wRec w
    arrival = wArrival w
    result i = case mode of
      1 -> rec U.! i
      2 -> mask U.! i || not (rec U.! i)
      _ -> mask U.! i && not (rec U.! i)
    nOut = length (filter result [0 .. cols * rows - 1])
    maskRGB = rgbOf (hsb 150 40 (if mode == 1 then 45 else 90))
    cellColor x y
      | rec U.! i =
          let a = arrival U.! i
          in if a == 0 then seedRGB  -- 種 = 黄、到達順で色
             else rgbOf (hsb (fi ((200 + a * 4) `mod` 360)) 80 85)
      | mask U.! i = maskRGB
      | otherwise = offRGB
      where i = idx x y
    px = ox + fi cols * c + 40
    ty = oy + 10

modeName :: Int -> String
modeName m = case m of
  1 -> "grow from seed"
  2 -> "fill holes"
  _ -> "remove border objects"

modeNotes :: Int -> [String]
modeNotes m = case m of
  1 -> [ "Mask = original image (dark green)"
       , "Seed = where you clicked (yellow)"
       , "R = (R (+) B) and mask, repeated"
       , "-> takes out just the seeded blob"
       , "(labeling, object selection)" ]
  2 -> [ "Mask = background (complement)"
       , "Seed = the image border"
       , "Only background reachable from"
       , "the border grows. Rest = holes"
       , "Result = image + holes (filled)" ]
  _ -> [ "Mask = original image"
       , "Seed = foreground on the border"
       , "Grown = objects on the border"
       , "Result = image - grown blobs"
       , "(drops cut cells in microscopy)" ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> click env w
  EventKey (SpecialKey KeySpace) Down _ _ ->
    startMode (if wMode w == 1 then w { wMarker = Nothing } else w)
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> startMode w { wMode = 1 }
    '2' -> startMode w { wMode = 2 }
    '3' -> startMode w { wMode = 3 }
    'r' -> makeMask w { wSeed = wSeed w + 1, wMarker = Nothing }
    _   -> w
  _ -> w

-- クリックで種を置く（モード 1 だけ）
click :: Env -> World -> World
click env w
  | wMode w /= 1 || not (inGrid gx gy) = w
  | wMask w U.! i = w' { wRec = wRec w U.// [(i, True)], wArrival = wArrival w U.// [(i, 0)], wDone = False }
  | otherwise = w'
  where
    (gx, gy) = cellAt env (cellOf env) ox oy
    i = idx gx gy
    marker = maybe (U.map (const False) (wMask w)) id (wMarker w)
    w' = w { wMarker = Just (marker U.// [(i, True)]) }

-- ---------- やってみよう ----------
-- 1. モード 1 で、穴のある塊の「穴の中」をクリックしてみる。何も育たないのはなぜ？
-- 2. モード 2 で、ふちに触れた塊の穴は埋まる？（ふちから流れ込める穴は「穴」ではない）
-- 3. se を makeSE Cross 1（4 近傍）にすると、斜めにしかつながっていない所で止まる。8 近傍と 4 近傍の「つながり」のちがい

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

offRGB, seedRGB :: (Float, Float, Float)
offRGB = rgbOf panelC
seedRGB = rgbOf (hsb 45 80 100)

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

fi :: Int -> Float
fi = fromIntegral

-- 色を pixelImageF 用の (赤, 緑, 青) にする
rgbOf :: Color -> (Float, Float, Float)
rgbOf c = let (r, g, b, _) = rgbaOfColor c in (r, g, b)

-- マス目全体を 1 枚の画像にする。f x y がマスの色。1 マスずつ長方形を描くより速い
imagePic :: Env -> Float -> Float -> Float -> (Int -> Int -> (Float, Float, Float)) -> Picture
imagePic env c x0 y0 f =
  at (pt env (x0 + fi cols * c / 2) (y0 + fi rows * c / 2)) (pixelImageF cols rows c f)

-- 1 ピクセルのすき間のあるマス目（p5.js の rect(x, y, cell - 1, cell - 1)）。
-- 画像の上に、すき間の色の線を重ねて描く
cellsPic :: Env -> Float -> Float -> Float -> Color -> (Int -> Int -> (Float, Float, Float)) -> Picture
cellsPic env c x0 y0 gapC f = Pictures
  [ imagePic env c x0 y0 f
  , Color gapC (Pictures
      ( [Line [pt env (x0 + fi i * c - 0.5) y0, pt env (x0 + fi i * c - 0.5) (y0 + fi rows * c)] | i <- [1 .. cols - 1]]
     ++ [Line [pt env x0 (y0 + fi j * c - 0.5), pt env (x0 + fi cols * c) (y0 + fi j * c - 0.5)] | j <- [1 .. rows - 1]]))
  ]

-- 二値画像：長さ cols × rows の Bool の並び（True = 前景）
type Grid = U.Vector Bool

idx :: Int -> Int -> Int
idx x y = x + y * cols

gridFrom :: (Int -> Int -> Bool) -> Grid
gridFrom f = U.generate (cols * rows) (\i -> f (i `rem` cols) (i `quot` cols))

-- 前景のマスの数
areaOf :: Grid -> Int
areaOf = U.length . U.filter id

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- マウスのいるマスの番号
cellAt :: Env -> Float -> Float -> Float -> (Int, Int)
cellAt env c x0 y0 =
  let (mx, my) = mouseXY env in (floor ((mx - x0) / c), floor ((my - y0) / c))

inGrid :: Int -> Int -> Bool
inGrid x y = x >= 0 && y >= 0 && x < cols && y < rows

-- 構造要素：原点からのずれ (dx, dy) のリスト
data Kind = Box | Cross | Disk deriving (Eq)

makeSE :: Kind -> Int -> [(Int, Int)]
makeSE kind r =
  [ (dx, dy) | dy <- [-r .. r], dx <- [-r .. r], inSE dx dy ]
  where
    inSE dx dy = case kind of
      Box -> True
      Cross -> dx == 0 || dy == 0
      Disk -> fi (dx * dx + dy * dy) <= fi (r * r) + fi r * 0.5

-- 膨張と収縮。画像の外側は端のマスと同じ値とみなす
--   膨張：SE のどれか 1 マスでも前景に当たれば ON
--   収縮：SE の全部のマスが前景に入れば ON
dilateImg, erodeImg :: [(Int, Int)] -> Grid -> Grid
dilateImg se a = U.generate (U.length a) (\i -> any (onAt a i) se)
erodeImg se a = U.generate (U.length a) (\i -> all (onAt a i) se)

onAt :: Grid -> Int -> (Int, Int) -> Bool
onAt a i (dx, dy) =
  let nx = max 0 (min (cols - 1) (i `rem` cols + dx))
      ny = max 0 (min (rows - 1) (i `quot` cols + dy))
  in a U.! (nx + ny * cols)
