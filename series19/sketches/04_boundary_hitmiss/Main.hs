-- ============================================
-- 04 境界抽出とヒット・オア・ミス
-- 境界 = A − (A ⊖ B)：収縮して消えた部分 = 形のふち（1 マスの太さ）
-- ヒット・オア・ミス：「ここは 1、ここは 0」という 3×3 のパターンにぴったり合う場所だけを見つける（角・孤立点・線の端）
-- B で境界の種類（内側／外側／なし） / 1〜4 でパターン / ドラッグで描く / E で消しゴム / R でお手本 / C で全部消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (transpose)
import qualified Data.Vector.Unboxed as U

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

-- ★ パターン：'1' = 前景, '0' = 背景, '.' = どちらでもよい（3 行 × 3 文字）
cornerP, isolatedP, endP :: [String]
cornerP = ["00.", "011", ".11"]    -- 左上の角
isolatedP = ["000", "010", "000"]  -- 孤立した 1 点
endP = ["000", "010", ".1."]       -- 線の端（下につながる）

data World = World
  { wImg      :: !Grid
  , wBoundary :: !Int     -- 0 = なし, 1 = 内側 A−(A⊖B), 2 = 外側 (A⊕B)−A
  , wPatNo    :: !Int
  , wEraser   :: !Bool
  , wErode    :: !Grid    -- 画像が変わったときに計算する
  , wDilate   :: !Grid
  , wHits     :: !Grid
  }

main :: IO ()
main = runSketch (sketch "19-04 Boundary and Hit-or-Miss" (refresh (World presetImg 1 1 False presetImg presetImg presetImg)))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

presetImg :: Grid
presetImg = base U.// [(idx x y, True) | (x, y) <- [(60, 52), (88, 50), (20, 58), (40, 60)]]  -- 孤立点
  where
    base = gridFrom $ \x y ->
      (x > 8 && x < 36 && y > 8 && y < 30)                                           -- 四角
      || (x > 46 && x < 56 && y > 8 && y < 40) || (x > 46 && x < 80 && y > 30 && y < 40) -- L
      || (y == 50 && x > 10 && x < 50)                                               -- 横線
      || (x == 70 && y > 44 && y < 60)                                               -- 縦線
      || distI x y 84 16 < 7                                                         -- 円

-- パターンを 90° 回す
rot :: [String] -> [String]
rot = map reverse . transpose

patternsOf :: Int -> [[String]]
patternsOf n = case n of
  2 -> take 4 (iterate rot cornerP)
  3 -> [isolatedP]
  4 -> take 4 (iterate rot endP)
  _ -> [cornerP]

-- ★ ヒット・オア・ミス：パターンが完全に一致するマスを 1 にする（画像の外は 0 とみなす）
hitOrMiss :: Grid -> [[String]] -> Grid
hitOrMiss a pats = gridFrom (\x y -> any (matches x y) compiled)
  where
    compiled = [ [ (dx, dy, ch == '1') | (dy, row) <- zip [-1, 0, 1] p, (dx, ch) <- zip [-1, 0, 1] row, ch /= '.' ] | p <- pats ]
    value x y = inGrid x y && a U.! idx x y
    matches x y = all (\(dx, dy, want) -> value (x + dx) (y + dy) == want)

refresh :: World -> World
refresh w = w { wErode = erodeImg se (wImg w), wDilate = dilateImg se (wImg w), wHits = hitOrMiss (wImg w) (patternsOf (wPatNo w)) }

se :: [(Int, Int)]
se = makeSE Box 1

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor
  , Color (hsb 45 90 100) (Pictures
      [ at (pt env (ox + fi x * c + c / 2) (oy + fi y * c + c / 2)) (disc (c * 0.8))
      | i <- U.toList (U.findIndices id hits), let x = i `rem` cols, let y = i `quot` cols ])
  , txt env px (py - 10) 16 (lum 100)
      ("Pattern " ++ show (wPatNo w) ++ ": " ++ patName (wPatNo w) ++ " (" ++ show (length pats) ++ " dir)") ]
  ++ concat (zipWith drawPattern [0 ..] pats)
  ++ [ txt env px ty 14 (hsb 150 60 90) "1 = must be foreground (B1)"
     , txt env px (ty + 24) 14 (hsb 350 60 90) "0 = must be background (B2)"
     , txt env px (ty + 48) 14 (lum 60) ". = either"
     , txt env px (ty + 84) 15 (hsb 45 90 100) ("Matches: " ++ show (areaOf hits))
     , txt env px (ty + 108) 15 (hsb 350 80 100) ("Boundary cells: " ++ show nB)
     , txt env px (ty + 130) 14 (hsb 350 80 100) ("(" ++ boundaryName ++ ")")
     , txt env px (ty + 166) 14 (lum 60) "Hit-or-miss: (A (-) B1) and"
     , txt env px (ty + 190) 14 (lum 60) "(A^c (-) B2)"
     , txt env px (ty + 214) 14 (lum 60) "B1 fits A, B2 fits the background"
     , txt env 20 40 17 (lum 100)
         ("B: boundary type   1-4: pattern   Drag: draw (" ++ (if wEraser w then "eraser" else "pen") ++ ", E to switch)   R: sample   C: clear")
     , txt env 20 68 14 (lum 60)
         "Red = boundary (cells erosion removes). Yellow = exact pattern match. Target corners, isolated points and line ends."
     ]
  where
    c = cellOf env
    img = wImg w
    hits = wHits w
    pats = patternsOf (wPatNo w)
    isBoundary i = case wBoundary w of
      1 -> img U.! i && not (wErode w U.! i)
      2 -> wDilate w U.! i && not (img U.! i)
      _ -> False
    nB = length (filter isBoundary [0 .. cols * rows - 1])
    cellColor x y
      | isBoundary i = edgeRGB
      | img U.! i = if wBoundary w /= 0 then dimRGB else onRGB
      | otherwise = offRGB
      where i = idx x y
    boundaryName = case wBoundary w of
      1 -> "inner: A - (A (-) B)"
      2 -> "outer: (A (+) B) - A"
      _ -> "none"
    px = ox + fi cols * c + 40
    py = oy
    big = 24
    ty = py + (if length pats > 2 then 2 else 1) * 90 + 30
    -- 右：パターンの図
    drawPattern :: Int -> [String] -> [Picture]
    drawPattern k p =
      let qx = px + fi (k `mod` 2) * 90
          qy = py + fi (k `div` 2) * 90
      in concat
        [ [ box env (patColor ch) (qx + fi x * big) (qy + fi y * big) (big - 2) (big - 2)
          , txt env (qx + fi x * big + 6) (qy + fi y * big + 17) 14 (lum 100) [ch] ]
        | (y, row) <- zip [0 :: Int ..] p, (x, ch) <- zip [0 :: Int ..] row ]
    patColor ch = case ch of
      '1' -> hsb 150 60 90
      '0' -> hsb 350 60 90
      _   -> hsb 170 20 30

patName :: Int -> String
patName n = case n of
  1 -> "top-left corner"
  2 -> "4 corners"
  3 -> "isolated point"
  _ -> "line end"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env -> paint env w
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'b' -> w { wBoundary = (wBoundary w + 1) `mod` 3 }
    'e' -> w { wEraser = not (wEraser w) }
    'r' -> refresh w { wImg = presetImg }
    'c' -> refresh w { wImg = U.map (const False) (wImg w) }
    d | d >= '1' && d <= '4' -> refresh w { wPatNo = fromEnum d - fromEnum '0' }
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w
  | inGrid gx gy = refresh w { wImg = wImg w U.// [(idx gx gy, not (wEraser w))] }
  | otherwise = w
  where (gx, gy) = cellAt env (cellOf env) ox oy

-- ---------- やってみよう ----------
-- 1. パターン 2（4 つの角）で四角は 4 か所。L 字はいくつ？ 円には角があるか？
-- 2. 1 マスずつ描いて、「孤立点」が一致する条件を確かめる（となりに 1 つでも点があるとダメ）
-- 3. cornerP のような自分のパターンを足してみる。例：["011", "111", "111"] で「左上だけ欠けた場所」

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, dimRGB, edgeRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 50 95)
offRGB = rgbOf panelC
dimRGB = rgbOf (hsb 150 50 55)
edgeRGB = rgbOf (hsb 350 80 100)

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 塗りつぶした長方形（左上が (x, y)。p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

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
