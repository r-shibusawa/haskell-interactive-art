-- ============================================
-- 14 形を育てるブラシ（自由制作のベース）
-- 描いた形が、ランダムな構造要素の膨張・収縮・オープン・クローズで「育って」いく
-- 新しく生えたマスほど新しい色。育つたびに小さな音。描きながら生き物を飼うように
-- ドラッグで描く / 1〜4 で育ち方 / 上下で速さ / スペースで一時停止 / M で音 / S で保存 / C で消す
-- ============================================
-- ★ のついた数字を変えて、自分の作品に！
module Main (main) where

import Art
import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

cols, rows :: Int
cols = 96
rows = 64

oy :: Float
oy = 110

brushR :: Int
brushR = 2        -- ★ ブラシの半径

maxFill :: Float
maxFill = 0.6     -- ★ 画面のこれ以上を占めたら自動でしぼむ

data Kind = Box | Cross | Disk | HLine | VLine | Diag deriving (Eq, Show)

allKinds :: [Kind]
allKinds = [Box, Cross, Disk, HLine, VLine, Diag]

data World = World
  { wImg       :: !Grid
  , wAge       :: !(U.Vector Int)
  , wMode      :: !Int     -- ★ 1 ふくらむ 2 しぼむ 3 ふくらむ⇄しぼむ 4 なめらかに（オープン／クローズ）
  , wEvery     :: !Int     -- ★ 何フレームごとに育つか
  , wGen       :: !Int
  , wPaused    :: !Bool
  , wSound     :: !Bool
  , wAudio     :: !Bool    -- 最初のクリックで音が出るようになる（p5.js 版と同じ）
  , wLastOp    :: !String
  }

main :: IO ()
main = runSketch (sketch "19-14 Growth Brush" world0)
  { skBack  = hsb 170 45 7
  , skStep  = \env _ w ->
      if not (wPaused w) && envFrame env `mod` wEvery w == 0 then grow w else pure w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World start (U.replicate (cols * rows) 0) 3 12 0 False True False ""
  where start = gridFrom (\x y -> distI x y (fi cols / 2) (fi rows / 2) < 5)

data Op = Dilate | Erode | Open | Close deriving (Eq)

opName :: Op -> String
opName o = case o of { Dilate -> "dilate"; Erode -> "erode"; Open -> "open"; Close -> "close" }

-- ★ 1 回の成長：ランダムな SE とランダムな操作
grow :: World -> IO World
grow w = do
  ki <- randomRIO (0, length allKinds - 1)
  r <- randomRIO (1, 2)
  u <- randomRIO (0, 1 :: Float)
  let kind = case drop ki allKinds of
        (k : _) -> k
        [] -> Box
      se = makeSE kind r
      img = wImg w
      area = areaOf img
      frac = fi area / fi (U.length img)
      op0 = case wMode w of
        1 -> Dilate
        2 -> Erode
        3 -> if u < 0.55 then Dilate else Erode
        _ -> if u < 0.5 then Open else Close
      op1 = if frac > maxFill then Erode else op0                 -- ふえすぎたらしぼむ
      op = if area < 20 && op1 /= Dilate then Dilate else op1     -- 消えそうなら育てる
      next = case op of
        Dilate -> dilateImg se img
        Erode -> erodeImg se img
        Open -> dilateImg se (erodeImg se img)
        Close -> erodeImg se (dilateImg se img)
      gen = wGen w + 1
      born i a = if next U.! i && not (img U.! i) then gen else a
      delta = areaOf next - area
  -- ★ 音：ふえたら上がる音、へったら下がる音
  when (wSound w && wAudio w && delta /= 0) $
    playTone (fi (round (220 * 2 ** clampF (-1.5) 1.5 (fi delta / 400) :: Float))) 0.12 0.35
      (if delta > 0 then Triangle else Sine)
  pure w { wImg = next, wAge = U.imap born (wAge w), wGen = gen
         , wLastOp = opName op ++ " (" ++ show kind ++ ")" }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ cellsPic env c x0 oy panelC cellColor
  , txt env 20 40 16 (lum 100)
      ("Drag: draw   1-4: growth (" ++ modeName (wMode w) ++ ")   Up/Down: speed " ++ show (wEvery w)
       ++ "   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   M: sound " ++ (if wSound w then "ON" else "OFF") ++ "   S: save   C: clear")
  , txt env 20 68 14 (lum 60)
      ("Generation " ++ show gen ++ "   Foreground " ++ fmt 0 (fi (areaOf img) / fi (U.length img) * 100)
       ++ "%   Last: " ++ wLastOp w ++ "   (click to enable sound)")
  ]
  where
    c = cellOf env
    x0 = xOf env
    img = wImg w
    gen = wGen w
    cellColor x y
      | not (img U.! i) = offRGB
      | otherwise =
          let age = wAge w U.! i
              a = gen - age  -- 何世代前に生えたか
          in rgbOf (hsb (fi ((140 + age * 7) `mod` 360)) 70 (if a < 2 then 100 else max 45 (95 - fi a * 2))) -- ★ 色：世代で色相、古いほど暗く
      where i = idx x y

modeName :: Int -> String
modeName m = case m of
  1 -> "swell"
  2 -> "shrink"
  3 -> "swell/shrink"
  _ -> "smooth"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paint env w { wAudio = True })
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEvery = max 2 (wEvery w - 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEvery = min 60 (wEvery w + 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'm' -> pure w { wSound = not (wSound w) }
    's' -> saveSvg "morphology" env (drawWorld env w) >> pure w
    'c' -> pure w { wImg = U.map (const False) (wImg w), wGen = 0 }
    d | d >= '1' && d <= '4' -> pure w { wMode = fromEnum d - fromEnum '0' }
    _ -> pure w
  _ -> pure w

paint :: Env -> World -> World
paint env w = w
  { wImg = wImg w U.// [(i, True) | i <- cells]
  , wAge = wAge w U.// [(i, wGen w) | i <- cells, not (wImg w U.! i)] }
  where
    cells = brush (cellAt env (cellOf env) (xOf env) oy) brushR (brushR * brushR + 1)

-- ---------- やってみよう ----------
-- 1. 細い線を 1 本描いて「なめらかに」で育てる。線はどうなる？ 太い丸は？
-- 2. allKinds を [Disk] だけにすると丸く育ち、[HLine] だけにすると横に伸びる。育ち方を自分で設計する
-- 3. 色を age ではなく「その位置の収縮回数」で決める。または背景を黒い紙に、形を白いインクに

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 40) / fi cols) ((envH env - 130) / fi rows)

-- マス目の左端（画面の中央に置く）
xOf :: Env -> Float
xOf env = (envW env - fi cols * cellOf env) / 2

panelC :: Color
panelC = hsb 170 30 12

offRGB :: (Float, Float, Float)
offRGB = rgbOf panelC

-- 構造要素：正方形・十字・円・横線・縦線・斜め線
makeSE :: Kind -> Int -> [(Int, Int)]
makeSE kind r =
  [ (dx, dy) | dy <- [-r .. r], dx <- [-r .. r], inSE dx dy ]
  where
    inSE dx dy = case kind of
      Box -> True
      Cross -> dx == 0 || dy == 0
      Disk -> fi (dx * dx + dy * dy) <= fi (r * r) + fi r * 0.5
      HLine -> dy == 0
      VLine -> dx == 0
      Diag -> dx == dy

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
