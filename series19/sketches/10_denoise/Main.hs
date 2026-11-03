-- ============================================
-- 10 ノイズ除去（ごま塩ノイズ）
-- 文字や図形に「白い点（塩）」と「黒い点（ごま）」をまぜて、モルフォロジーで元にもどす
-- オープン→クローズ、クローズ→オープン、メディアン（多数決）を比べて、まちがいの数で勝負
-- 1〜4 で方法 / 上下でノイズの量 / 左右で SE の大きさ / P で元の絵 / スペースで別のノイズ
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 96
rows = 64

oy :: Float
oy = 130

data World = World
  { wClean   :: !Grid
  , wNoisy   :: !Grid
  , wResult  :: !Grid
  , wMethod  :: !Int    -- ★ 1 なし 2 オープン→クローズ 3 クローズ→オープン 4 メディアン（3×3 多数決）
  , wNoise   :: !Float  -- ★ ノイズの割合
  , wR       :: !Int    -- ★ SE の半径
  , wSeed    :: !Int
  , wPicture :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-10 Noise Removal" (recompute (makePicture world0)))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e e 2 0.08 1 1 1
  where e = U.replicate (cols * rows) False

makePicture :: World -> World
makePicture w = w { wClean = img }
  where
    img | wPicture w == 1 = textToGrid "p5" 62
        | otherwise = gridFrom $ \x y ->
            distI x y 26 30 < 15
            || (x > 50 && x < 88 && y > 10 && y < 52 && not (x > 60 && x < 78 && y > 20 && y < 42))
            || (y > 54 && y < 58 && x > 6 && x < 46)

recompute :: World -> World
recompute w = w { wNoisy = noisy, wResult = result }
  where
    se = makeSE Disk (wR w)
    us = U.fromListN (cols * rows) (randomRs (0, 1 :: Float) (mkStdGen (wSeed w)))
    noisy = U.zipWith (\v u -> if u < wNoise w then not v else v) (wClean w) us  -- ★ ごま塩
    result = case wMethod w of
      1 -> noisy
      2 -> closeImg se (openImg se noisy)
      3 -> openImg se (closeImg se noisy)
      _ -> median3 noisy

-- ★ 3×3 の多数決（二値画像のメディアンフィルタ）
median3 :: Grid -> Grid
median3 a = gridFrom $ \x y ->
  length [ () | dy <- [-1, 0, 1], dx <- [-1, 0, 1]
         , a U.! idx (max 0 (min (cols - 1) (x + dx))) (max 0 (min (rows - 1) (y + dy))) ] >= 5

errorsOf :: Grid -> Grid -> (Int, Int)
errorsOf clean a =
  ( U.length (U.filter id (U.zipWith (\v t -> v && not t) a clean))
  , U.length (U.filter id (U.zipWith (\v t -> not v && t) a clean)) )

drawPanel :: Env -> World -> Grid -> Float -> String -> Picture
drawPanel env w a px title = Pictures
  [ cellsPic env c px oy panelC cellColor
  , txt env px (oy - 30) 17 (lum 100) title
  , txt env px (oy - 10) 14 (hsb 350 85 100) ("Extra white (salt) " ++ show fp)
  , txt env (px + 210) (oy - 10) 14 (hsb 200 70 90) ("Missing (pepper) " ++ show fn)
  , txt env (px + 420) (oy - 10) 14 (lum 100) ("Total errors " ++ show (fp + fn))
  ]
  where
    c = cellOf env
    clean = wClean w
    (fp, fn) = errorsOf clean a
    cellColor x y
      | a U.! i = if clean U.! i then onRGB else saltRGB  -- 赤 = 余計な白
      | clean U.! i = pepperRGB                           -- 青 = 足りない白
      | otherwise = offRGB
      where i = idx x y

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawPanel env w (wNoisy w) 20 ("Noisy (rate " ++ fmt 2 (wNoise w) ++ ")")
  , drawPanel env w (wResult w) (20 + pw + gap)
      (methodName (wMethod w) ++ (if wMethod w /= 4 then "   SE = Disk " ++ show n ++ "x" ++ show n else ""))
  , txt env 20 (by + 10) 28 (hsb 45 90 100)
      ("Errors " ++ show e1 ++ " -> " ++ show e2
       ++ (if e2 < e1 then "   (" ++ fmt 0 ((1 - fi e2 / fi (max e1 1)) * 100) ++ "% fewer)" else ""))
  , txt env 20 (by + 44) 14 (lum 60)
      "Opening removes salt, closing removes pepper. Order matters (both alter the shape). The median is an order statistic."
  , txt env 20 (by + 68) 14 (lum 60)
      "Switch methods and find the one with the fewest errors for each noise level and SE size."
  , txt env 20 40 16 (lum 100)
      "1-4: method (none, open-close, close-open, median)   Up/Down: noise   Left/Right: SE   P: picture   Space: new noise"
  , txt env 20 68 14 (lum 60)
      "Red = extra white, blue = missing white. Errors are counted against the correct picture (before noise)."
  ]
  where
    c = cellOf env
    pw = fi cols * c
    gap = 30
    n = 2 * wR w + 1
    total a = let (p, q) = errorsOf (wClean w) a in p + q
    e1 = total (wNoisy w)
    e2 = total (wResult w)
    by = oy + fi rows * c + 28

methodName :: Int -> String
methodName m = case m of
  1 -> "As is"
  2 -> "Open -> close (A o B) . B"
  3 -> "Close -> open (A . B) o B"
  _ -> "Median (3x3 majority)"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> recompute w { wNoise = min 0.3 (wNoise w + 0.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> recompute w { wNoise = max 0 (wNoise w - 0.02) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wR = min 3 (wR w + 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> recompute w { wSeed = wSeed w + 1 }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'p' -> recompute (makePicture w { wPicture = 3 - wPicture w })
    d | d >= '1' && d <= '4' -> recompute w { wMethod = fromEnum d - fromEnum '0' }
    _   -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ノイズ 0.10 でどの方法が一番まちがいが少ない？ 0.20 では？（ノイズが多いと 3×3 では足りなくなる）
-- 2. SE を大きくすると、ノイズは消えるが「p5」の細い部分も消える。まちがいが増え始めるのはどこ？
-- 3. 方法 5 を作る：メディアンを 2 回。または「オープン→クローズ」を 5×5 の多数決の後にかける

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 70) / 2 / fi cols) ((envH env - 250) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, saltRGB, pepperRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC
saltRGB = rgbOf (hsb 350 85 95)
pepperRGB = rgbOf (hsb 200 70 60)

openImg, closeImg :: [(Int, Int)] -> Grid -> Grid
openImg se = dilateImg se . erodeImg se
closeImg se = erodeImg se . dilateImg se

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

fi :: Int -> Float
fi = fromIntegral

-- 小数 d 桁の文字列（p5.js の nf）
fmt :: Int -> Float -> String
fmt d v = showFFloat (Just d) v ""

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

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

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

-- 文字をマス目の画像にする。gloss には文字を画像に焼く機能がないので、
-- 文字の形を太い折れ線で作ってある（px は文字の大きさ）
textToGrid :: String -> Float -> Grid
textToGrid str px = gridFrom (\x y -> any (near (fi x, fi y)) segs)
  where
    k = px / 56
    (halfW, strokes) = glyphs str
    place (ax, ay) = (fi cols / 2 + k * ax, fi rows / 2 + k * ay)
    segs = [ (place a, place b) | p <- strokes, (a, b) <- zip p (drop 1 p) ]
    near q (a, b) = segDist q a b < halfW * k

-- 点 q と線分 ab の距離
segDist :: Point -> Point -> Point -> Float
segDist (qx, qy) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l2 = max 1e-6 (dx * dx + dy * dy)
      t = clampF 0 1 (((qx - ax) * dx + (qy - ay) * dy) / l2)
  in dist2 (qx, qy) (ax + t * dx, ay + t * dy)

-- 円弧を折れ線にする（中心、半径 rx, ry、開始角、終了角。角度は度で、y は下向き）
arcPts :: Point -> Float -> Float -> Float -> Float -> [Point]
arcPts (cx, cy) rx ry a0 a1 =
  [ (cx + rx * cos t, cy + ry * sin t)
  | i <- [0 .. 16 :: Int], let t = (a0 + (a1 - a0) * fi i / 16) * pi / 180 ]

shiftX :: Float -> [[Point]] -> [[Point]]
shiftX d = map (map (\(x, y) -> (x + d, y)))

-- 文字ごとの線の太さの半分と、折れ線（中心が原点、大きさ 56 のときの座標）
glyphs :: String -> (Float, [[Point]])
glyphs s = case s of
  "p5" -> (4, shiftX (-17) glyphP ++ shiftX 17 glyph5)
  "ab" -> (4, shiftX (-14) glyphA ++ shiftX 14 glyphB)
  _    -> (2.6, glyphOn)
  where
    glyphP = [[(-10, -11), (-10, 24)], arcPts (1, -0.5) 10.5 10.5 0 360]
    glyph5 = [ [(11, -21), (-6, -21), (-8, -7)]
             , arcPts (-1, 0) 12 10.5 (-125) 150 ]
    glyphA = [ arcPts (0, -4) 9 9 (-160) 0, [(9, -4), (9, 11)]
             , arcPts (0, 4.5) 9 6 0 360 ]
    glyphB = [[(-10, -22), (-10, 11)], arcPts (1, -0.5) 10.5 10.5 0 360]
    -- 「音」= 立 + 日
    glyphOn = [ [(0, -24), (0, -19)], [(-15, -18), (15, -18)]
              , [(-8, -14), (-5, -6)], [(8, -14), (5, -6)], [(-23, -5), (23, -5)]
              , [(-13, 1), (13, 1), (13, 24), (-13, 24), (-13, 1)], [(-13, 12.5), (13, 12.5)] ]
