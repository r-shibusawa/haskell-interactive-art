-- ============================================
-- 05 細線化とスケルトン
-- 細線化（Zhang–Suen）：形のふちを「つながりを壊さないように」少しずつ削って、太さ 1 の線にする
-- モルフォロジー・スケルトン（Lantuéjoul）：収縮を k 回 → それをオープニングして引く、を全部足す
-- スペースで 1 ステップ / A で自動 / M で方式を切替 / 1〜3 でお手本 / ドラッグで描く / R でもどす
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

data Mode = Thin | Skel deriving (Eq)  -- ★ Thin = Zhang–Suen, Skel = モルフォロジー・スケルトン

data World = World
  { wOrig    :: !Grid
  , wImg     :: !Grid
  , wRemoved :: !Grid
  , wSkel    :: !Grid
  , wMode    :: !Mode
  , wPass    :: !Int
  , wIter    :: !Int
  , wKStep   :: !Int
  , wDone    :: !Bool
  , wAuto    :: !Bool
  }

main :: IO ()
main = runSketch (sketch "19-05 Thinning and Skeleton" (preset 1 world0))
  { skBack  = hsb 170 45 9
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 6 == 0 then stepOnce w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e e e Thin 0 0 0 False False
  where e = U.replicate (cols * rows) False

seB :: [(Int, Int)]
seB = makeSE Box 1  -- ★ スケルトン用の SE（3×3）

preset :: Int -> World -> World
preset k w = reset w { wOrig = img }
  where
    img = case k of
      1 -> textToGrid "p5" 60
      2 -> gridFrom $ \x y ->
             (x > 10 && x < 40 && y > 10 && y < 54) || (x > 30 && x < 86 && y > 24 && y < 40) || distI x y 76 16 < 9
      _ -> gridFrom (\x y -> fbm2 4 (fi x * 0.07) (fi y * 0.07) > 0.52)

reset :: World -> World
reset w = w { wImg = wOrig w, wRemoved = e, wSkel = e, wPass = 0, wIter = 0, wKStep = 0, wDone = False }
  where e = U.map (const False) (wOrig w)

stepOnce :: World -> World
stepOnce w = if wMode w == Thin then stepThin w else stepSkel w

-- ★ Zhang–Suen の 1 サブイテレーション（pass 0 と 1 を交互に）
stepThin :: World -> World
stepThin w
  | wDone w = w
  | otherwise = w
      { wImg = img U.// [(i, False) | i <- del]
      , wRemoved = U.map (const False) img U.// [(i, True) | i <- del]
      , wIter = if pass == 1 then wIter w + 1 else wIter w
      , wDone = null del && pass == 1
      , wPass = 1 - pass
      }
  where
    img = wImg w
    pass = wPass w
    pix :: Int -> Int -> Int
    pix x y = if inGrid x y && img U.! idx x y then 1 else 0
    del = [ idx x y | y <- [0 .. rows - 1], x <- [0 .. cols - 1], img U.! idx x y, canDelete x y ]
    canDelete x y =
      let p2 = pix x (y - 1); p3 = pix (x + 1) (y - 1); p4 = pix (x + 1) y; p5 = pix (x + 1) (y + 1)
          p6 = pix x (y + 1); p7 = pix (x - 1) (y + 1); p8 = pix (x - 1) y; p9 = pix (x - 1) (y - 1)
          n = [p2, p3, p4, p5, p6, p7, p8, p9]
          b = sum n                                                   -- 近傍の 1 の数
          a = length [ () | (u, v) <- zip n (drop 1 n ++ take 1 n), u == 0, v == 1 ]  -- 0 → 1 に変わる回数（一周）
          c1 = if pass == 0 then p2 * p4 * p6 == 0 && p4 * p6 * p8 == 0
                            else p2 * p4 * p8 == 0 && p2 * p6 * p8 == 0
      in b >= 2 && b <= 6 && a == 1 && c1

-- ★ スケルトンの k 番目の部分 S_k = (A ⊖ kB) − ((A ⊖ kB) ∘ B)
stepSkel :: World -> World
stepSkel w
  | wDone w = w
  | otherwise = w
      { wRemoved = found
      , wSkel = U.zipWith (||) (wSkel w) found
      , wImg = erodeImg seB img
      , wKStep = wKStep w + 1
      , wDone = not (U.or img) || wKStep w + 1 > 40
      }
  where
    img = wImg w
    o = dilateImg seB (erodeImg seB img)
    found = U.zipWith (\v p -> v && not p) img o

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor ]
  ++ (if isThin then thinText else skelText)
  ++ [ txt env 20 40 16 (lum 100)
         ("Space: 1 step   A: auto (" ++ (if wAuto w then "ON" else "OFF") ++ ")   M: method ("
          ++ (if isThin then "Zhang-Suen thinning" else "morphological skeleton") ++ ")   1-3: samples   Drag: draw   R: reset")
     , txt env 20 68 14 (lum 60)
         (if isThin then "Goal: a connected center line of width 1 (OCR, fingerprints, maps). Plain erosion cuts thin parts, so add conditions."
                    else "A skeleton built only from morphology. Close to the center line, but it can break into pieces. Compare with thinning.")
     ]
  where
    c = cellOf env
    isThin = wMode w == Thin
    img = wImg w
    removed = wRemoved w
    skel = wSkel w
    orig = wOrig w
    shownSkel i = not isThin && skel U.! i && not (removed U.! i)
    nSkel = length (filter shownSkel [0 .. cols * rows - 1])
    area = length [ () | i <- [0 .. cols * rows - 1], img U.! i, not (removed U.! i), not (shownSkel i) ]
    cellColor x y
      | removed U.! i = if isThin then redRGB else yellowRGB
      | shownSkel i = skelRGB
      | img U.! i = onRGB
      | not isThin && orig U.! i = shadeRGB  -- 削られた元の形
      | otherwise = offRGB
      where i = idx x y
    px = ox + fi cols * c + 40
    ty = oy + 10
    note dy s = txt env px (ty + dy) 14 (lum 60) s
    thinText =
      [ txt env px (ty + 20) 34 (hsb 45 90 100) ("Thinning " ++ show (wIter w))
      , txt env px (ty + 50) 15 (lum 100) ("Next: sub-step " ++ show (wPass w + 1) ++ (if wDone w then " (done)" else ""))
      , txt env px (ty + 76) 15 (lum 100) ("Cells left: " ++ show area)
      , note 116 "Delete edge cell p when:"
      , note 140 "- 2 to 6 of its neighbors are 1"
      , note 164 "  (not a line end, not interior)"
      , note 188 "- exactly one 0->1 around it"
      , note 212 "  (deleting keeps connectivity)"
      , note 236 "- pass 1: right or bottom is 0"
      , note 260 "  pass 2: left or top is 0"
      , note 296 "Red = removed in this step"
      , note 320 "Shaved evenly from all sides, so"
      , note 344 "the center line remains"
      ]
    skelText =
      [ txt env px (ty + 20) 34 (hsb 45 90 100) ("Erode x" ++ show (wKStep w))
      , txt env px (ty + 50) 15 (lum 100) ("Skeleton cells: " ++ show nSkel ++ (if wDone w then " (done)" else ""))
      , note 90 "S_k = E_k - (E_k o B)"
      , note 114 "E_k = A eroded k times by B"
      , note 138 "S_k = tips that opening removes"
      , note 162 "Skeleton = S_0 + S_1 + ... (yellow)"
      , note 198 "Yellow = S_k found now"
      , note 222 "Dark green = shaved original"
      , note 258 "Dilate S_k k times, add: A again"
      , note 282 "Connectivity is not guaranteed"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env -> paint env w
  EventKey (SpecialKey KeySpace) Down _ _ -> stepOnce w
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'a' -> w { wAuto = not (wAuto w) }
    'm' -> reset w { wMode = if wMode w == Thin then Skel else Thin }
    '1' -> preset 1 w
    '2' -> preset 2 w
    '3' -> preset 3 w
    'r' -> reset w
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w = w { wImg = wImg w U.// on, wOrig = wOrig w U.// on, wDone = False }
  where on = [(i, True) | i <- brush (cellAt env (cellOf env) ox oy) 2 5]

-- ---------- やってみよう ----------
-- 1. 太い線を描いて細線化。何周で終わる？（太さの半分くらい）
-- 2. 「p5」の細線化とスケルトンを見比べる。どちらが文字として読みやすい？
-- 3. お手本 2 の四角：細線化すると線の端に「ひげ」が出る。ひげを消すには？（端点から数回削る = 枝刈り）

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, redRGB, yellowRGB, skelRGB, shadeRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC
redRGB = rgbOf (hsb 350 85 100)
yellowRGB = rgbOf (hsb 45 85 100)
skelRGB = rgbOf (hsb 45 90 100)
shadeRGB = rgbOf (hsb 150 40 35)

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
