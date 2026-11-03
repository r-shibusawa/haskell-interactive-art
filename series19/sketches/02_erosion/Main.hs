-- ============================================
-- 02 収縮（erosion）A ⊖ B
-- 構造要素 B が「すっぽり入る」マスだけを残す → 形がやせる。細い所は切れ、小さなゴミは消える
-- 双対性：前景を収縮することは、背景を膨張すること（C で補集合の表示に切り替え）
-- スペースで 1 回収縮（赤 = 消えたマス） / A で自動 / C で補集合 / 1〜3 でお手本 / 左右で SE の大きさ / K で SE の形 / D で描くモード / R でもどす
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
  { wOrig    :: !Grid
  , wImg     :: !Grid
  , wRemoved :: !Grid   -- 今回消えたマス
  , wKind    :: !Kind   -- ★ Box Cross Disk
  , wR       :: !Int    -- ★ SE の半径
  , wSteps   :: !Int
  , wAuto    :: !Bool
  , wDraw    :: !Bool
  , wComp    :: !Bool   -- 補集合の表示
  , wDiff    :: !Int    -- 双対性の両辺の差（画像か SE が変わったときに計算する）
  }

main :: IO ()
main = runSketch (sketch "19-02 Erosion" (preset 1 world0))
  { skBack  = hsb 170 45 9
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 20 == 0 then stepErode w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e e Disk 1 0 False False False 0
  where e = U.replicate (cols * rows) False

preset :: Int -> World -> World
preset k w = reset w { wOrig = img }
  where
    img = case k of
      1 -> -- 細いくびれでつながった 2 つの塊、細い線、ゴミ
        let base = gridFrom $ \x y ->
              distI x y 24 30 < 14 || distI x y 50 30 < 12
              || (x >= 30 && x <= 44 && abs (y - 30) <= 1)
              || (x > 66 && x < 90 && y > 8 && y < 48)
              || (y > 52 && y < 55 && x > 8 && x < 60)
            specks = take 40 (randomRs (0, cols * rows - 1) (mkStdGen 5))
        in base U.// [(i, True) | i <- specks]
      2 -> textToGrid "p5" 56
      _ -> gridFrom (\x y -> fbm2 4 (fi x * 0.09) (fi y * 0.09) > 0.5)

reset :: World -> World
reset w = refresh w { wImg = wOrig w, wRemoved = U.map (const False) (wOrig w), wSteps = 0 }

-- 双対性チェック：A ⊖ B と (Aᶜ ⊕ B)ᶜ は同じか
refresh :: World -> World
refresh w = w { wDiff = U.length (U.filter id (U.zipWith (==) e d)) }
  where
    se = makeSE (wKind w) (wR w)
    e = erodeImg se (wImg w)
    d = dilateImg se (U.map not (wImg w))

-- ★ 1 回の収縮。消えたマスを wRemoved に記録
stepErode :: World -> World
stepErode w = refresh w { wImg = next, wRemoved = U.zipWith (\o n -> o && not n) (wImg w) next, wSteps = wSteps w + 1 }
  where next = erodeImg (makeSE (wKind w) (wR w)) (wImg w)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor
  , txt env px (py - 10) 16 (lum 100) ("SE B: " ++ kindName (wKind w) ++ " " ++ show n ++ "x" ++ show n) ]
  ++ sePanel env px py big r se
  ++ [ txt env px ty 36 (hsb 350 80 100) ("Erode x" ++ show (wSteps w))
     , txt env px (ty + 36) 15 (lum 100)
         (if comp then "Background: " ++ show area ++ " (yellow = grew)"
                  else "Foreground: " ++ show area ++ " (red = lost " ++ show nRemoved ++ ")")
     , txt env px (ty + 70) 14 (lum 60) "A (-) B = {p | B at p fits in A}"
     , txt env px (ty + 94) 14 (lum 60) "Thin parts break, specks vanish,"
     , txt env px (ty + 112) 14 (lum 60) "holes grow"
     , txt env px (ty + 140) 14 (lum 100) "Duality: A (-) B = (A^c (+) B)^c"
     , txt env px (ty + 164) 14 (lum 60) "Erode fg = dilate background"
     , txt env px (ty + 188) 14 (lum 60) ("Difference now: " ++ show (wDiff w) ++ " cells")
     , txt env px (ty + 212) 14 (lum 60) "C: show background (complement)"
     , txt env 20 40 16 (lum 100)
         ("Space: erode once   A: auto (" ++ onOff (wAuto w) ++ ")   C: complement (" ++ onOff comp ++ ")   1-3: samples   Left/Right: SE size   K: shape   D: draw   R: reset")
     , txt env 20 68 14 (lum 60)
         (if comp then "Blue = background. Dilating the background (yellow grows) thins the foreground: the same operation seen from behind."
                  else "Erosion keeps only cells where B fits. When does the neck break? On which step do all the specks vanish?")
     ]
  where
    c = cellOf env
    r = wR w
    n = 2 * r + 1
    se = makeSE (wKind w) r
    comp = wComp w
    img = wImg w
    removed = wRemoved w
    nRemoved = areaOf removed
    area = if comp then U.length (U.filter not img) else areaOf img
    cellColor x y
      | removed U.! i = if comp then rgbOf (hsb 45 90 100) else goneRGB
      | (if comp then not v else v) = if comp then bgRGB else onRGB
      | otherwise = offRGB
      where i = idx x y
            v = img U.! i
    px = ox + fi cols * c + 40
    py = oy
    big = 20
    ty = py + fi n * big + 40

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env -> paint env w
  EventKey (SpecialKey KeySpace) Down _ _ -> stepErode w
  EventKey (SpecialKey KeyLeft) Down _ _ -> refresh w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> refresh w { wR = min 4 (wR w + 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'a' -> w { wAuto = not (wAuto w) }
    'c' -> w { wComp = not (wComp w) }
    '1' -> preset 1 w
    '2' -> preset 2 w
    '3' -> preset 3 w
    'k' -> refresh w { wKind = nextKind (wKind w) }
    'd' -> w { wDraw = not (wDraw w) }
    'r' -> reset w
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w
  | not (wDraw w) = w
  | otherwise = refresh w { wImg = wImg w U.// on, wOrig = wOrig w U.// on }
  where on = [(i, True) | i <- brush (cellAt env (cellOf env) ox oy) 1 2]

-- ---------- やってみよう ----------
-- 1. お手本 1 で、くびれが切れるのは何回目？ ゴミが全部消えるのは？（SE の大きさを変えて比べる）
-- 2. 収縮を続けると最後に残るのは「いちばん太い場所」。どこが最後まで残る？
-- 3. 01 の膨張を n 回 → 収縮を n 回 しても元には戻らない。何が変わる？（→ 03 のクロージング）

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, goneRGB, bgRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC
goneRGB = rgbOf (hsb 350 90 100)
bgRGB = rgbOf (hsb 200 60 95)

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

-- (gx, gy) のまわり半径 r の、dx² + dy² <= lim を満たすマスの番号
brush :: (Int, Int) -> Int -> Int -> [Int]
brush (gx, gy) r lim =
  [ idx x y | dy <- [-r .. r], dx <- [-r .. r], dx * dx + dy * dy <= lim
  , let x = gx + dx, let y = gy + dy, inGrid x y ]

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

kindName :: Kind -> String
kindName k = case k of { Box -> "Square"; Cross -> "Cross"; Disk -> "Disk" }

nextKind :: Kind -> Kind
nextKind k = case k of { Box -> Cross; Cross -> Disk; Disk -> Box }

-- 構造要素の拡大図（big はマスの大きさ）
sePanel :: Env -> Float -> Float -> Float -> Int -> [(Int, Int)] -> [Picture]
sePanel env px py big r se =
  [ box env (if (dx, dy) `elem` se then hsb 150 60 95 else hsb 170 30 20)
      (px + fi (dx + r) * big) (py + fi (dy + r) * big) (big - 2) (big - 2)
  | dy <- [-r .. r], dx <- [-r .. r] ]

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
