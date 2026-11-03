-- ============================================
-- 08 モルフォロジー勾配（エッジ検出）
-- 膨張 − 収縮 = 形のふちだけが残る（太さ 2）。元 − 収縮 = 内側のふち、膨張 − 元 = 外側のふち
-- 二値画像でもグレースケール画像でも同じ式で動く
-- 1〜3 で勾配の種類 / 4〜6 でお手本（図形・文字・グレー） / 左右で SE の大きさ / ドラッグで描く / E で消しゴム / C で全部消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U

cols, rows :: Int
cols = 96
rows = 64

oy :: Float
oy = 130

type Gray = U.Vector Float

data World = World
  { wImg    :: !Gray
  , wResult :: !Gray
  , wMode   :: !Int   -- ★ 1 = 対称 (f⊕B)−(f⊖B)  2 = 内部 f−(f⊖B)  3 = 外部 (f⊕B)−f
  , wR      :: !Int   -- ★ SE の半径
  , wEraser :: !Bool
  , wShape  :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-08 Morphological Gradient" (recompute (preset 4 world0)))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e 1 1 False 4
  where e = U.replicate (cols * rows) 0

preset :: Int -> World -> World
preset k w = w { wShape = k, wImg = img }
  where
    fromBools g = U.map (\v -> if v then 1 else 0) g
    img = case k of
      4 -> fromBools $ gridFrom $ \x y ->
             distI x y 26 30 < 16 || (x > 50 && x < 86 && y > 10 && y < 50 && not (x > 60 && x < 76 && y > 22 && y < 38))
      5 -> fromBools (textToGrid "ab" 60)
      _ -> U.generate (cols * rows) $ \i ->
             let x = i `rem` cols
                 y = i `quot` cols
                 v0 = 0.2 + 0.4 * fbm2 4 (fi x * 0.04 + 40) (fi y * 0.04)
                 v1 = if distI x y 30 32 < 16 then v0 + 0.35 else v0
                 v2 = if x > 56 && x < 86 && y > 14 && y < 50 then v1 + 0.25 + 0.15 * sin (fi x * 0.5) else v1
             in clampF 0 1 v2

recompute :: World -> World
recompute w = w { wResult = result }
  where
    se = makeSE (wR w)
    f = wImg w
    d = grayDilate se f
    e = grayErode se f
    result = case wMode w of
      1 -> U.zipWith (-) d e
      2 -> U.zipWith (-) f e
      _ -> U.zipWith (-) d f

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ cellsPic env c 20 oy panelC leftColor
  , cellsPic env c px2 oy panelC rightColor
  , txt env 20 (oy - 30) 17 (lum 100) "Original f (drag to draw)"
  , txt env px2 (oy - 30) 17 (lum 100) (gradName (wMode w))
  , txt env 20 (oy - 10) 14 (lum 60) (if wShape w == 6 then "Grayscale" else "Binary (0 / 1)")
  , txt env px2 (oy - 10) 14 (lum 60)
      ("SE = Disk " ++ show n ++ "x" ++ show n ++ "   Strong edge cells: " ++ show nEdge ++ "   Brightness = gradient")
  , txt env 20 by 15 (lum 100)
      "Symmetric: both sides of the edge (width 2r)   Internal: inside only   External: outside only"
  , txt env 20 (by + 24) 14 (lum 60)
      "In grayscale this is the range (max - min) inside the SE: how much the image changes. A larger SE also finds slow changes."
  , txt env 20 (by + 48) 14 (lum 60)
      "Unlike Sobel it has no direction and is not linear. It resists noise and gives closed outlines (input to 12, watershed)."
  , txt env 20 40 16 (lum 100)
      ("1-3: gradient type   4-6: samples (shapes, text, gray)   Left/Right: SE size   Drag: draw (" ++ (if wEraser w then "eraser" else "pen") ++ ", E to switch)   C: clear")
  , txt env 20 68 14 (lum 60)
      "Subtract the eroded shape from the dilated shape and only the edge remains. 'Fatter minus thinner' is edge detection."
  ]
  where
    c = cellOf env
    n = 2 * wR w + 1
    pw = fi cols * c
    px2 = 20 + pw + 30
    img = wImg w
    result = wResult w
    nEdge = U.length (U.filter (> 0.5) result)
    leftColor x y =
      let v = img U.! idx x y
      in if v > 0.02 then rgbOf (hsb 150 30 (20 + v * 75)) else offRGB
    rightColor x y =
      let g = result U.! idx x y
      in if g > 0.02 then rgbOf (hsb (45 - g * 20) 90 (40 + g * 60)) else offRGB
    by = oy + fi rows * c + 30

gradName :: Int -> String
gradName m = case m of
  1 -> "Symmetric (f (+) B) - (f (-) B)"
  2 -> "Internal f - (f (-) B)"
  _ -> "External (f (+) B) - f"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env -> paint env w
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wR = min 4 (wR w + 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> recompute w { wMode = 1 }
    '2' -> recompute w { wMode = 2 }
    '3' -> recompute w { wMode = 3 }
    '4' -> recompute (preset 4 w)
    '5' -> recompute (preset 5 w)
    '6' -> recompute (preset 6 w)
    'e' -> w { wEraser = not (wEraser w) }
    'c' -> recompute w { wImg = U.map (const 0) (wImg w) }
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w = recompute w { wImg = wImg w U.// [(i, if wEraser w then 0 else 1) | i <- brush (cellAt env c 20 oy) 2 5] }
  where c = cellOf env

-- ---------- やってみよう ----------
-- 1. 内部勾配と外部勾配を足すと対称勾配になる。4 の四角でふちの太さを数えて確かめる
-- 2. グレー画像（6）で SE を大きくする。細かい縞（sin）と大きな丸、どちらが先に消える？
-- 3. 「勾配 > しきい値」で二値化してから 05 の細線化にかけると、太さ 1 の輪郭線になる

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、グレースケールの膨張と収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 70) / 2 / fi cols) ((envH env - 220) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

offRGB :: (Float, Float, Float)
offRGB = rgbOf panelC

-- 構造要素（円）
makeSE :: Int -> [(Int, Int)]
makeSE r =
  [ (dx, dy) | dy <- [-r .. r], dx <- [-r .. r], fi (dx * dx + dy * dy) <= fi (r * r) + fi r * 0.5 ]

-- グレースケールの膨張（max）／収縮（min）。画像の外側は端の値とみなす
grayDilate, grayErode :: [(Int, Int)] -> Gray -> Gray
grayDilate se a = U.generate (U.length a) (\i -> foldr (max . sampleAt a i) (-1) se)
grayErode se a = U.generate (U.length a) (\i -> foldr (min . sampleAt a i) 2 se)

sampleAt :: Gray -> Int -> (Int, Int) -> Float
sampleAt a i (dx, dy) =
  let nx = max 0 (min (cols - 1) (i `rem` cols + dx))
      ny = max 0 (min (rows - 1) (i `quot` cols + dy))
  in a U.! (nx + ny * cols)

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

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- マウスのいるマスの番号
cellAt :: Env -> Float -> Float -> Float -> (Int, Int)
cellAt env c x0 y0 =
  let (mx, my) = mouseXY env in (floor ((mx - x0) / c), floor ((my - y0) / c))

inGrid :: Int -> Int -> Bool
inGrid x y = x >= 0 && y >= 0 && x < cols && y < rows

-- (gx, gy) のまわり半径 r の、dx² + dy² <= lim を満たすマスの番号
brush :: (Int, Int) -> Int -> Int -> [Int]
brush (gx, gy) r lim =
  [ idx x y | dy <- [-r .. r], dx <- [-r .. r], dx * dx + dy * dy <= lim
  , let x = gx + dx, let y = gy + dy, inGrid x y ]

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
