-- ============================================
-- 03 オープニングとクロージング
-- オープニング A∘B = 収縮してから膨張 → 小さなゴミ・細い突起が消える（形はほぼ元のまま）
-- クロージング A•B = 膨張してから収縮 → 小さな穴・すき間が埋まる（形はほぼ元のまま）
-- 上下でノイズの量 / 左右で SE の大きさ / 1〜3 でお手本 / K で SE の形 / スペースで別のノイズ
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 72
rows = 48

oy :: Float
oy = 130

data World = World
  { wClean  :: !Grid
  , wNoisy  :: !Grid
  , wOpened :: !Grid
  , wClosed :: !Grid
  , wNoise  :: !Float  -- ★ ノイズの割合（0〜0.15）
  , wR      :: !Int    -- ★ SE の半径
  , wKind   :: !Kind   -- ★ Box Cross Disk
  , wSeed   :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-03 Opening and Closing" (recompute (preset 1 world0)))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e e e 0.03 1 Disk 1
  where e = U.replicate (cols * rows) False

preset :: Int -> World -> World
preset k w = w { wClean = img }
  where
    img = case k of
      1 -> gridFrom $ \x y ->
             distI x y 22 22 < 14 || (x > 42 && x < 66 && y > 8 && y < 40) || (y > 38 && y < 44 && x > 6 && x < 40)
      2 -> textToGrid "ab" 48
      _ -> gridFrom (\x y -> fbm2 4 (fi x * 0.1) (fi y * 0.1) > 0.52)

-- ★ ゴミ（背景に白点）と穴（前景に黒点）をランダムに加えて、両方の操作をかける
recompute :: World -> World
recompute w = w { wNoisy = noisy
                , wOpened = dilateImg se (erodeImg se noisy)   -- オープニング
                , wClosed = erodeImg se (dilateImg se noisy) } -- クロージング
  where
    se = makeSE (wKind w) (wR w)
    us = U.fromListN (cols * rows) (randomRs (0, 1 :: Float) (mkStdGen (wSeed w)))
    noisy = U.zipWith (\v u -> if u < wNoise w then not v else v) (wClean w) us

-- 正解とのちがいを数える：ゴミ（余計な 1）と穴（足りない 1）
errors :: Grid -> Grid -> (Int, Int)
errors clean a =
  ( U.length (U.filter id (U.zipWith (\v t -> v && not t) a clean))
  , U.length (U.filter id (U.zipWith (\v t -> not v && t) a clean)) )

drawPanel :: Env -> World -> Grid -> Float -> String -> String -> Picture
drawPanel env w a px title sub = Pictures
  [ cellsPic env c px oy panelC cellColor
  , txt env px (oy - 32) 17 (lum 100) title
  , txt env px (oy - 12) 14 (lum 60) sub
  , txt env px bottom 14 (hsb 350 80 100) ("Specks (extra) " ++ show s)
  , txt env (px + 195) bottom 14 (hsb 200 70 100) ("Holes (missing) " ++ show h)
  ]
  where
    c = cellOf env
    bottom = oy + fi rows * c + 22
    clean = wClean w
    (s, h) = errors clean a
    -- 正解と違うマスは色を変える（赤 = 余計、足りないマスは描けないので下で数える）
    cellColor x y
      | not (a U.! i) = offRGB
      | clean U.! i = onRGB
      | otherwise = badRGB
      where i = idx x y

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawPanel env w (wNoisy w) 20 "Noisy A" "Random specks and holes added"
  , drawPanel env w (wOpened w) (20 + pw + gap) "Opening A o B = (A (-) B) (+) B" "Erode, then dilate: specks vanish"
  , drawPanel env w (wClosed w) (20 + 2 * (pw + gap)) "Closing A . B = (A (+) B) (-) B" "Dilate, then erode: holes fill"
  , txt env 20 40 17 (lum 100)
      ("Up/Down: noise " ++ fmt 2 (wNoise w) ++ "   Left/Right: SE = " ++ kindName (wKind w) ++ " " ++ show n ++ "x" ++ show n
       ++ " (K: shape)   1-3: samples   Space: new noise")
  , txt env 20 68 14 (lum 60)
      "Erosion alone thins the shape. Dilating with the same B restores what survived: that is opening. Closing is the reverse."
  , txt env 20 by 14 (lum 60)
      "Properties: A o B <= A <= A . B (opening only removes, closing only adds). Repeating changes nothing (idempotent)."
  , txt env 20 (by + 24) 14 (lum 60) "Nothing smaller than B can survive opening."
  , txt env 20 (by + 48) 14 (lum 60) "Try: opening then closing (A o B) . B removes both kinds of noise. Compare in sketch 10."
  ]
  where
    c = cellOf env
    pw = fi cols * c
    gap = 30
    n = 2 * wR w + 1
    by = oy + fi rows * c + 60

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> recompute w { wNoise = min 0.15 (wNoise w + 0.01) }
  EventKey (SpecialKey KeyDown) Down _ _ -> recompute w { wNoise = max 0 (wNoise w - 0.01) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wR = min 4 (wR w + 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> recompute w { wSeed = wSeed w + 1 }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'k' -> recompute w { wKind = nextKind (wKind w) }
    '1' -> recompute (preset 1 w)
    '2' -> recompute (preset 2 w)
    '3' -> recompute (preset 3 w)
    _   -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ノイズを増やしていくと、オープニングでも取れないゴミが出てくる。なぜ？（ゴミが集まって B より大きくなる）
-- 2. SE を大きくすると、お手本 1 の細い横棒はオープニングで消える。棒の太さと SE の大きさの関係は？
-- 3. お手本 2 の文字で、クロージングの SE を大きくすると「a」の穴が埋まる。文字がつぶれない限界は？

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 100) / 3 / fi cols) ((envH env - 260) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, badRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC
badRGB = rgbOf (hsb 350 80 95)

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

kindName :: Kind -> String
kindName k = case k of { Box -> "Square"; Cross -> "Cross"; Disk -> "Disk" }

nextKind :: Kind -> Kind
nextKind k = case k of { Box -> Cross; Cross -> Disk; Disk -> Box }

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
