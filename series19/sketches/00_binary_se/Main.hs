-- ============================================
-- 00 二値画像と構造要素（モルフォロジーの材料）
-- 画像を「0 と 1 のマス目」として扱い、小さな形 = 構造要素（SE）を画像の上で動かす
-- SE が「すっぽり入る」か「少しでも当たる」か ── これが収縮と膨張の正体
-- ドラッグで描く / E で消しゴム / 1〜3 で SE の形（正方形・十字・円） / 左右で SE の大きさ / R でお手本 / C で全部消す
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

data World = World
  { wImg    :: !Grid   -- 0 / 1 の画像（True = 前景 = 白いマス）
  , wKind   :: !Kind   -- ★ Box（正方形） Cross（十字） Disk（円）
  , wR      :: !Int    -- ★ SE の半径（1 なら 3×3）
  , wEraser :: !Bool
  }

main :: IO ()
main = runSketch (sketch "19-00 Binary Images and Structuring Elements" world0)
  { skBack  = hsb 170 45 9
  , skStart = \_ -> preset world0
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World (U.replicate (cols * rows) False) Box 1 False

-- お手本：リング、穴あき四角、小さなゴミ
preset :: World -> IO World
preset w = do
  let base = gridFrom $ \x y ->
        let d = distI x y 30 32
            ringOn = d < 19 && d > 8
            square = x > 56 && x < 86 && y > 12 && y < 52
            hole = x > 66 && x < 74 && y > 26 && y < 40
        in ringOn || (square && not hole)
  specks <- mapM (const (randomRIO (0, cols * rows - 1))) [1 .. 30 :: Int] -- ★ ゴミの数
  pure w { wImg = base U.// [(i, True) | i <- specks] }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC (\x y -> if img U.! idx x y then onRGB else offRGB) ]
  ++ marker ++ [ txt env px (py - 10) 16 (lum 100)
      ("B: " ++ kindName (wKind w) ++ " " ++ show n ++ "x" ++ show n ++ ", " ++ show (length se) ++ " cells") ]
  ++ sePanel env px py big r se
  ++ [ box env (hsb 45 90 100) (px + fi r * big + big * 0.3) (py + fi r * big + big * 0.3) (big * 0.4) (big * 0.4)
     , txt env px ty 15 (lum 100) "With B at the mouse..."
     , txt env px (ty + 26) 14 (if fits then hsb 45 90 100 else lum 40)
         ((if fits then "(*) " else "( ) ") ++ "fits: stays after erosion")
     , txt env px (ty + 52) 14 (if hits then hsb 20 90 100 else lum 40)
         ((if hits then "(*) " else "( ) ") ++ "hits: ON after dilation")
     , txt env px (ty + 90) 15 (lum 60) ("Foreground: " ++ show (areaOf img) ++ " / " ++ show (cols * rows))
     , txt env px (ty + 124) 14 (lum 60) "Erosion A (-) B = cells that fit"
     , txt env px (ty + 148) 14 (lum 60) "Dilation A (+) B = cells that hit"
     , txt env 20 40 17 (lum 100)
         ("Drag: draw (" ++ (if wEraser w then "eraser" else "pen") ++ ", E to switch)   1-3: SE shape   Left/Right: SE size   R: sample   C: clear")
     , txt env 20 68 14 (lum 60)
         "A binary image is a grid of 0s and 1s. Slide a small shape B (structuring element) over it: does it fit? does it hit?"
     ]
  where
    c = cellOf env
    img = wImg w
    r = wR w
    n = 2 * r + 1
    se = makeSE (wKind w) r
    (mx, my) = cellAt env c ox oy
    inside = inGrid mx my
    onCount = length [ () | (dx, dy) <- se
                     , img U.! idx (clampI (cols - 1) (mx + dx)) (clampI (rows - 1) (my + dy)) ]
    fits = inside && onCount == length se
    hits = inside && onCount > 0
    strokeC | fits = hsb 45 90 100
            | hits = hsb 20 90 100
            | otherwise = hsb 0 0 100
    -- マウスの位置に SE を重ねる
    marker | inside =
               [ outline env strokeC 2 (ox + fi (mx + dx) * c) (oy + fi (my + dy) * c) (c - 1) (c - 1) | (dx, dy) <- se ]
               ++ [ box env (hsb 45 90 100) (ox + fi mx * c + c * 0.3) (oy + fi my * c + c * 0.3) (c * 0.4) (c * 0.4) ]
           | otherwise = []
    px = ox + fi cols * c + 40
    py = oy
    big = 22
    ty = py + fi n * big + 40

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paint env w)
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wR = min 5 (wR w + 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> pure w { wKind = Box }
    '2' -> pure w { wKind = Cross }
    '3' -> pure w { wKind = Disk }
    'e' -> pure w { wEraser = not (wEraser w) }
    'r' -> preset w
    'c' -> pure w { wImg = U.map (const False) (wImg w) }
    _   -> pure w
  _ -> pure w

paint :: Env -> World -> World
paint env w = w { wImg = wImg w U.// [(i, not (wEraser w)) | i <- brush (cellAt env (cellOf env) ox oy) 1 2] }

-- ---------- やってみよう ----------
-- 1. SE を大きくして、リングの上でも「すっぽり入る」場所を探す。入らなくなるのは半径いくつから？
-- 2. 小さなゴミの上では「当たる」が「入らない」。収縮するとゴミは消える、と予想できる
-- 3. 十字と円では「入る」範囲がどう違う？ 四角の角で比べてみよう

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC

clampI :: Int -> Int -> Int
clampI hi v = max 0 (min hi v)

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

-- 枠だけの長方形（太さ t）
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (uncurry (pt env))
  [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]))

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

-- 構造要素の拡大図（big はマスの大きさ）
sePanel :: Env -> Float -> Float -> Float -> Int -> [(Int, Int)] -> [Picture]
sePanel env px py big r se =
  [ box env (if (dx, dy) `elem` se then hsb 150 60 95 else hsb 170 30 20)
      (px + fi (dx + r) * big) (py + fi (dy + r) * big) (big - 2) (big - 2)
  | dy <- [-r .. r], dx <- [-r .. r] ]
