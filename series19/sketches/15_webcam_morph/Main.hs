-- ============================================
-- 15 カメラでモルフォロジー（おまけ）
-- カメラの映像を小さく縮めて 2 値化し、膨張・収縮・オープン・クローズ・勾配をリアルタイムでかける
-- オープニングのあとに塊を数えると「ノイズを除いた物体の数」になる（カメラが使えないときはデモの映像）
-- 1〜6 で操作 / 左右で SE の大きさ / マウス左右 = しきい値 / I で白黒反転 / 上下で数える塊の最小サイズ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

cols, rows :: Int
cols = 96
rows = 72

oy :: Float
oy = 130

data World = World
  { wCam    :: !(Maybe Camera)
  , wLive   :: !Bool
  , wBin    :: !Grid
  , wResult :: !Grid
  , wLabels :: !(U.Vector Int)  -- 0 = なし, -1 = 小さくて数えない塊, 1〜 = 塊の番号
  , wBlobs  :: !Int
  , wOp     :: !Int     -- ★ 1 なし 2 膨張 3 収縮 4 オープン 5 クローズ 6 勾配
  , wR      :: !Int     -- ★ SE の半径
  , wMin    :: !Int     -- ★ 数える塊の最小マス数
  , wInvert :: !Bool
  }

main :: IO ()
main = runSketch (sketch "19-15 Webcam Morphology" world0)
  { skBack  = hsb 170 45 9
  , skStart = \_ -> do
      cam <- startCamera cols rows
      pure world0 { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World Nothing False e e (U.replicate (cols * rows) 0) 0 4 1 10 False
  where e = U.replicate (cols * rows) False

-- ★ しきい値（0〜255 の明るさ）
thresholdOf :: Env -> Float
thresholdOf env = clampF 30 220 (remap (fst (mouseXY env)) 0 (envW env) 30 220)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = case wCam w of
  Nothing -> pure w
  Just cam -> do
    frame <- cameraFrame cam
    live <- camLive cam
    let th = thresholdOf env
        brightness i
          | B.length frame < cols * rows * 3 = 0
          | otherwise =
              (fromIntegral (B.index frame (i * 3)) + fromIntegral (B.index frame (i * 3 + 1))
               + fromIntegral (B.index frame (i * 3 + 2))) / 3 :: Float
        bin = U.generate (cols * rows) (\i ->
          let b = brightness i in if wInvert w then b > th else b < th)
        se = makeSE Disk (wR w)
        result = case wOp w of
          1 -> bin
          2 -> dilateImg se bin
          3 -> erodeImg se bin
          4 -> dilateImg se (erodeImg se bin)
          5 -> erodeImg se (dilateImg se bin)
          _ -> U.zipWith (\d e -> d && not e) (dilateImg se bin) (erodeImg se bin)
        (labels, n) = labelComponents (wMin w) result
    pure w { wLive = live, wBin = bin, wResult = result, wLabels = labels, wBlobs = n }

-- ★ 連結成分のラベリング（8 近傍）。minSize 未満の塊は数えない
labelComponents :: Int -> Grid -> (U.Vector Int, Int)
labelComponents minSize a = runST $ do
  labels <- MU.replicate (U.length a) (0 :: Int)
  let flood [] comp = pure comp
      flood (i : stack) comp = do
        let x = i `rem` cols
            y = i `quot` cols
        (stack', comp') <- foldM (\(st, cs) j -> do
                    l <- MU.read labels j
                    if a U.! j && l == 0
                      then MU.write labels j (-1) >> pure (j : st, j : cs)
                      else pure (st, cs))
                  (stack, comp)
                  [ idx nx ny | dy <- [-1, 0, 1], dx <- [-1, 0, 1]
                  , let nx = x + dx, let ny = y + dy, inGrid nx ny ]
        flood stack' comp'
      start n s = do
        l <- MU.read labels s
        if not (a U.! s) || l /= 0 then pure n else do
          MU.write labels s (-1)
          comp <- flood [s] [s]
          if length comp >= minSize
            then forM_ comp (\i -> MU.write labels i (n + 1)) >> pure (n + 1)
            else pure n
  n <- foldM start (0 :: Int) [0 .. U.length a - 1]
  final <- U.freeze labels
  pure (final, n)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ imagePic env c 20 oy leftColor
  , imagePic env c px2 oy rightColor
  , txt env 20 (oy - 30) 17 (lum 100)
      ("Binary (threshold " ++ show (round (thresholdOf env) :: Int) ++ ", " ++ (if wInvert w then "bright" else "dark") ++ " = ON)")
  , txt env px2 (oy - 30) 17 (lum 100) (opName (wOp w) ++ "   SE = Disk " ++ show n ++ "x" ++ show n)
  , txt env 20 (oy - 10) 14 (lum 60) ("Camera -> shrink to " ++ show cols ++ "x" ++ show rows ++ " -> mean brightness -> 0/1")
  , txt env px2 (oy - 10) 14 (lum 60) ("Color = blob (gray: under " ++ show (wMin w) ++ " cells, not counted)")
  , txt env px2 by 36 (hsb 45 90 100) ("Blobs " ++ show (wBlobs w))
  , txt env 20 (by + 36) 14 (lum 60)
      "Show a hand, a face, things on a desk. Watch the count change. Opening removes noise dots; closing fills holes in objects."
  , txt env 20 40 16 (lum 100)
      "1-6: op (none, dilate, erode, open, close, gradient)   Left/Right: SE   Mouse X: threshold   I: invert   Up/Down: min size"
  , txt env 20 68 14 (lum 60)
      (if wLive w then "Use a plain background. Adjust the threshold so only the objects turn white, then switch operations."
                  else "No camera: showing a demo video.")
  ]
  where
    c = cellOf env
    n = 2 * wR w + 1
    pw = fi cols * c
    px2 = 20 + pw + 30
    by = oy + fi rows * c + 40
    leftColor x y = if wBin w U.! idx x y then binRGB else offRGB
    rightColor x y
      | not (wResult w U.! i) = offRGB
      | l > 0 = rgbOf (hsb (fi ((l * 47) `mod` 360)) 70 95)
      | otherwise = smallRGB
      where i = idx x y
            l = wLabels w U.! i

opName :: Int -> String
opName k = case k of
  1 -> "As is"
  2 -> "Dilation"
  3 -> "Erosion"
  4 -> "Opening"
  5 -> "Closing"
  _ -> "Gradient (dilate - erode)"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wR = min 4 (wR w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wMin = min 200 (wMin w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wMin = max 1 (wMin w - 5) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'i' -> w { wInvert = not (wInvert w) }
    d | d >= '1' && d <= '6' -> w { wOp = fromEnum d - fromEnum '0' }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 机に消しゴムやペンを並べて、オープニングで「物の数」が正しく出るしきい値と SE を探す
-- 2. 勾配（6）で輪郭だけにする。SE を大きくすると輪郭は太くなる
-- 3. 塊の数を音の高さにする（playTone を使う）。手を広げると音が変わる楽器に

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 70) / 2 / fi cols) ((envH env - 220) / fi rows)

offRGB, binRGB, smallRGB :: (Float, Float, Float)
offRGB = rgbOf (hsb 170 30 16)
binRGB = rgbOf (lum 85)
smallRGB = rgbOf (lum 35)

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

-- 二値画像：長さ cols × rows の Bool の並び（True = 前景）
type Grid = U.Vector Bool

idx :: Int -> Int -> Int
idx x y = x + y * cols

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
