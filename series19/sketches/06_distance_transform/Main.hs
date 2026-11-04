-- ============================================
-- 06 距離変換（distance transform）
-- 形の中の各マスに「いちばん近い背景までの距離」を書き込む → 熱い色ほど奥
-- 「距離 > t」のマスを残す = 半径 t の円で収縮。背景側の距離 ≤ t を足す = 半径 t の円で膨張
-- 1〜3 で距離の種類（ユークリッド・市街地・チェス盤） / 上下で t / D で内側⇄外側 / ドラッグで描く / E で消しゴム / R でお手本 / C で全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showFFloat)

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

data World = World
  { wImg     :: !Grid
  , wDt      :: !(U.Vector Float)
  , wMaxD    :: !Float
  , wMetric  :: !Int    -- ★ 1 = ユークリッド 2 = 市街地（4 近傍） 3 = チェス盤（8 近傍）
  , wT       :: !Int    -- ★ しきい値（半径）
  , wOutside :: !Bool
  , wEraser  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "19-06 Distance Transform" (recompute world0))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World presetImg (U.replicate (cols * rows) 0) 1 1 4 False False

presetImg :: Grid
presetImg = gridFrom $ \x y ->
  distI x y 26 30 < 17 || distI x y 44 40 < 11
  || (x > 60 && x < 90 && y > 10 && y < 54)
  || (y > 56 && y < 60 && x > 6 && x < 50)

-- ★ 距離変換：fg が True のマスについて、fg が False のいちばん近いマスまでの距離
distanceTransform :: Int -> Grid -> U.Vector Float
distanceTransform metric fg
  -- 背景が 1 マスもないと距離² は 1e9 のまま。市街地／チェス盤と同じ「遠すぎる」値にする
  | metric == 1 = U.map (\v -> if v >= 1e8 then 1e6 else realToFrac (sqrt v)) (edtSquared (\i -> not (fg U.! i)))
  | otherwise = twoPass nb fg
  where
    nb = if metric == 2 then [(-1, 0), (0, -1)] else [(-1, 0), (0, -1), (-1, -1), (1, -1)]

-- 2 回の走査（前向き・後ろ向き）で市街地／チェス盤の距離が正確に求まる
twoPass :: [(Int, Int)] -> Grid -> U.Vector Float
twoPass nb fg = runST $ do
  d <- U.thaw (U.map (\v -> if v then 1e6 else 0) fg)
  forM_ [0 .. rows - 1] $ \y -> forM_ [0 .. cols - 1] $ \x -> relax d x y nb
  let nb2 = [(-dx, -dy) | (dx, dy) <- nb]
  forM_ [rows - 1, rows - 2 .. 0] $ \y -> forM_ [cols - 1, cols - 2 .. 0] $ \x -> relax d x y nb2
  U.freeze d

relax :: MU.MVector s Float -> Int -> Int -> [(Int, Int)] -> ST s ()
relax d x y nb = forM_ nb $ \(dx, dy) -> do
  let nx = x + dx
      ny = y + dy
  if not (inGrid nx ny) then pure () else do
    a <- MU.read d (idx nx ny)
    b <- MU.read d (idx x y)
    if a + 1 < b then MU.write d (idx x y) (a + 1) else pure ()

recompute :: World -> World
recompute w = w { wDt = dt, wMaxD = U.foldl' (\m v -> if v < 1e5 && v > m then v else m) 1 dt }
  where
    fg = if wOutside w then U.map not (wImg w) else wImg w
    dt = distanceTransform (wMetric w) fg

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor
  , txt env px ty 15 (lum 100) ("Distance: " ++ metricName (wMetric w))
  , txt env px (ty + 26) 15 (lum 60) ("Max " ++ fmt 1 maxD ++ "   At mouse " ++ mouseText) ]
  ++ [ box env (heat (fi k / 40)) (px + fi k * 5) (ty + 40) 5 14 | k <- [0 .. 39 :: Int] ] -- カラーバー
  ++ [ txt env px (ty + 72) 14 (lum 60) "0"
     , txt env (px + 180) (ty + 72) 14 (lum 60) (fmt 0 maxD)
     , txt env px (ty + 120) 34 (hsb 45 90 100) ("t = " ++ show t)
     , txt env px (ty + 150) 15 (lum 100)
         (if outside then "Yellow: bg d <= t (" ++ show nSel ++ " cells)" else "Yellow: fg d > t (" ++ show nSel ++ " cells)") ]
  ++ (if outside
        then [ note 176 "Green + yellow: dilated by disk t"
             , note 200 "Background distance = contours"
             , note 224 "around the shape; also used for"
             , note 248 "Voronoi and nearest neighbors" ]
        else [ note 176 "= A eroded by a disk of radius t"
             , note 200 "One transform gives any t"
             , note 224 "(faster than t erosions)"
             , note 260 "Hottest cells = medial axis" ])
  ++ [ note 300 "City, chess: 2 passes, O(N)"
     , note 324 "Euclidean: 1D parabolas, O(N)"
     , txt env 20 40 16 (lum 100)
         ("1-3: metric   Up/Down: t   D: inside/outside (" ++ (if outside then "outside, dilate" else "inside, erode")
          ++ ")   Drag: draw (" ++ (if wEraser w then "eraser" else "pen") ++ ", E)   R: sample   C: clear")
     , txt env 20 68 14 (lum 60)
         "A 'circle' is a diamond in city-block, a square in chessboard, a true circle in Euclidean. This sets the corner shapes."
     ]
  where
    c = cellOf env
    img = wImg w
    dt = wDt w
    t = wT w
    tf = fi t
    maxD = wMaxD w
    outside = wOutside w
    heat v = hsb (200 - v * 200) 80 (40 + v * 60)
    selected i = if outside then not (img U.! i) && dt U.! i <= tf
                            else img U.! i && dt U.! i > tf
    nSel = length (filter selected [0 .. cols * rows - 1])
    cellColor x y
      | outside =
          if img U.! i then onRGB
          else if d <= tf then rgbOf (hsb 45 70 70)
          else if d > 3 * tf + 2 then offRGB
          else rgbOf (hsb 200 60 (60 - (d / maxD) * 40))
      | not (img U.! i) = offRGB
      | d > tf = rgbOf (hsb 45 90 100)    -- 収縮：距離 > t が残る
      | otherwise = rgbOf (heat (d / maxD))
      where i = idx x y
            d = dt U.! i
    -- マウス位置の距離
    (mx, my) = cellAt env c ox oy
    mouseText
      | inGrid mx my && dt U.! idx mx my <= 1e5 = fmt 1 (dt U.! idx mx my)
      | otherwise = "-"
    px = ox + fi cols * c + 40
    ty = oy + 10
    note dy s = txt env px (ty + dy) 14 (lum 60) s

metricName :: Int -> String
metricName m = case m of
  1 -> "Euclid sqrt(dx^2+dy^2)"
  2 -> "City block |dx|+|dy|"
  _ -> "Chess max(|dx|,|dy|)"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env -> paint env w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wT = min 30 (wT w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wT = max 0 (wT w - 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> recompute w { wMetric = 1 }
    '2' -> recompute w { wMetric = 2 }
    '3' -> recompute w { wMetric = 3 }
    'd' -> recompute w { wOutside = not (wOutside w) }
    'e' -> w { wEraser = not (wEraser w) }
    'r' -> recompute w { wImg = presetImg }
    'c' -> recompute w { wImg = U.map (const False) (wImg w) }
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w = recompute w { wImg = wImg w U.// [(i, not (wEraser w)) | i <- brush (cellAt env (cellOf env) ox oy) 2 5] }

-- ---------- やってみよう ----------
-- 1. 1 点だけ描いて外側モードにする。1〜3 で「円」の形がどう変わる？（ひし形・正方形・円）
-- 2. 内側モードで t を上げていく。2 つの円がくっついた形は、どの t で 2 つに分かれる？（→ 12 のウォーターシェッド）
-- 3. 距離がいちばん大きいマスだけ残すと「中心線」になる。05 の細線化と比べてみよう

-- ==================================================
-- ここから下は「道具」（座標、描画、ユークリッド距離変換）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC

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

-- 2 乗ユークリッド距離変換（Felzenszwalb & Huttenlocher）：
-- isSite i が True のマスまでの距離²。縦に 1 回、横に 1 回、1 次元の処理をする
edtSquared :: (Int -> Bool) -> U.Vector Double
edtSquared isSite =
  U.concat [ edt1d (U.generate cols (\x -> byCol U.! (x * rows + y))) | y <- [0 .. rows - 1] ]
  where
    big = 1e9
    f = U.generate (cols * rows) (\i -> if isSite i then 0 else big)
    byCol = U.concat [ edt1d (U.generate rows (\y -> f U.! (x + y * cols))) | x <- [0 .. cols - 1] ]

-- 1 次元：放物線の下側包絡線
edt1d :: U.Vector Double -> U.Vector Double
edt1d f = runST $ do
  let n = U.length f
      sq :: Int -> Double
      sq q = fromIntegral (q * q)
      cross q p = ((f U.! q + sq q) - (f U.! p + sq p)) / fromIntegral (2 * q - 2 * p)
  v <- MU.replicate n (0 :: Int)
  z <- MU.replicate (n + 1) (0 :: Double)
  MU.write z 0 (-1e20)
  MU.write z 1 1e20
  let insert k q = do
        p <- MU.read v k
        zk <- MU.read z k
        let s = cross q p
        if s <= zk then insert (k - 1) q else do
          MU.write v (k + 1) q
          MU.write z (k + 1) s
          MU.write z (k + 2) 1e20
          pure (k + 1)
  _ <- foldM insert 0 [1 .. n - 1]
  d <- MU.replicate n (0 :: Double)
  let query k q
        | q >= n = pure ()
        | otherwise = do
            zn <- MU.read z (k + 1)
            if zn < fromIntegral q then query (k + 1) q else do
              p <- MU.read v k
              MU.write d q (sq (q - p) + f U.! p)
              query k (q + 1)
  query 0 0
  U.freeze d
