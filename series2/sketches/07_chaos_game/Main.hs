-- ============================================
-- 07 カオスゲーム（ランダムからフラクタルが生まれる）
-- 「頂点をランダムに 1 つ選んで、そこへ半分だけ近づく」をくり返すと…？
-- 3〜8 キーで頂点の数 / マウス上下 = 近づく割合 / クリックで頂点を置く
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, randomR)

-- 点を描きためるキャンバス。頂点が入る範囲だけをピクセル画像で持つ
data Canvas = Canvas
  { cLeft, cTop :: !Float         -- 左上の画面座標
  , cK          :: !Float         -- 1 ピクセルが画面の何ドットか
  , cW, cH      :: !Int
  , cData       :: !(U.Vector Word8)  -- 赤緑青を 3 バイトずつ
  }

data World = World
  { wVerts  :: !(U.Vector (Float, Float))  -- 頂点
  , wPx, wPy :: !Float                     -- 今の点の位置
  , wRatio  :: !Float                      -- どれだけ近づくか（0.5 = 半分）
  , wLast   :: !Int                        -- 前に選んだ頂点
  , wGen    :: !StdGen
  , wCanvas :: !Canvas
  }

itersPerFrame :: Int
itersPerFrame = 2000     -- ★ 1 フレームのくり返し回数

dotAlpha :: Float
dotAlpha = 0.35          -- ★ 点の濃さ

maxPixels :: Float
maxPixels = 78000        -- キャンバスのピクセル数の上限

background :: Color
background = hsb 0 0 6

main :: IO ()
main = runSketch (sketch "02-07 Chaos Game" (setPolygon 1280 800 3 (blankWorld 1)))
  { skBack  = background
  , skStart = \env -> do
      s <- randomRIO (0, 1000000)
      pure (setPolygon (envW env) (envH env) 3 (blankWorld s))
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

blankWorld :: Int -> World
blankWorld s = World U.empty 0 0 0.5 (-1) (mkStdGen s) (Canvas 0 0 1 1 1 (U.replicate 3 0))

-- 正 n 角形の頂点を置く
setPolygon :: Float -> Float -> Int -> World -> World
setPolygon ww wh n w = reset ww wh w { wVerts = U.generate n vertex }
  where
    r = min ww wh * 0.42
    vertex i =
      let a = -pi / 2 + tau * fromIntegral i / fromIntegral n
      in (cos a * r, -40 - sin a * r)     -- p5.js の height / 2 + 40 + sin(a) * r

-- キャンバスを消して、点を画面のどこかに置きなおす
reset :: Float -> Float -> World -> World
reset ww wh w =
  let (px, g1) = randomR (-ww / 2, ww / 2) (wGen w)
      (py, g2) = randomR (-wh / 2, wh / 2) g1
  in w { wPx = px, wPy = py, wGen = g2, wCanvas = fitCanvas (wVerts w) }

step :: Env -> Float -> World -> IO World
step env _ w0 = pure (playGame w)
  where
    wh = envH env
    -- マウスで近づく割合を変える（変わったら描き直す）
    r = clampF 0.3 0.7 (remap (wh / 2 - snd (envMouse env)) 0 wh 0.7 0.3)
    w | abs (r - wRatio w0) > 0.003 = reset (envW env) wh w0 { wRatio = r }
      | otherwise = w0

-- 1 フレームぶんのくり返し
playGame :: World -> World
playGame w =
  let (pts, x, y, lastK, g) = go itersPerFrame [] (wPx w) (wPy w) (wLast w) (wGen w)
      c = wCanvas w
  in w { wPx = x, wPy = y, wLast = lastK, wGen = g
       , wCanvas = c { cData = U.modify (plotAll c colors pts) (cData c) } }
  where
    verts = wVerts w
    n = U.length verts
    ratio = wRatio w
    colors = U.generate n (\k -> bytes (hsb (fromIntegral k * 360 / fromIntegral n) 60 100))
    go :: Int -> [(Int, Float, Float)] -> Float -> Float -> Int -> StdGen
       -> ([(Int, Float, Float)], Float, Float, Int, StdGen)
    go 0 acc !x !y !l g = (acc, x, y, l, g)
    go i acc !x !y !l g =
      let (k, g') = randomR (0, n - 1) g   -- 頂点をランダムに選ぶ
      in if n >= 4 && k == l               -- 頂点が 4 つ以上のとき：同じ頂点を続けて選ばない
           then go (i - 1) acc x y l g'
           else
             let (vx, vy) = verts U.! k
                 x' = lerpF x vx ratio     -- 選んだ頂点に向かって ratio だけ近づく
                 y' = lerpF y vy ratio
             in go (i - 1) ((k, x', y') : acc) x' y' k g'

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( image
  : Color white (Pictures [at v (disc 5) | v <- U.toList (wVerts w)])   -- 頂点を描く
  : [ label (-ww / 2 + 20) (wh / 2 - 30) 16 white
        ( "Vertices " ++ show (U.length (wVerts w)) ++ " (keys 3-8)   Ratio " ++ nf2 (wRatio w)
          ++ " (mouse up/down)   Click: add vertex" ) ] )
  where
    ww = envW env
    wh = envH env
    c = wCanvas w
    d = cData c
    fw = fromIntegral (cW c) * cK c
    fh = fromIntegral (cH c) * cK c
    pixel x y =
      let i = (y * cW c + x) * 3
      in (U.unsafeIndex d i, U.unsafeIndex d (i + 1), U.unsafeIndex d (i + 2))
    image = at (cLeft c + fw / 2, cTop c - fh / 2) (pixelImage (cW c) (cH c) (cK c) pixel)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '3' && c <= '8' -> pure (setPolygon ww wh (fromEnum c - fromEnum '0') w)
    | c == 'r' || c == 'R' -> pure (setPolygon ww wh 3 w)
  -- 自分で好きな場所に頂点を置く
  EventKey (MouseButton LeftButton) Down _ (mx, my)
    | U.length (wVerts w) < 8 && my <= wh / 2 - 50 ->
        pure (reset ww wh w { wVerts = U.snoc (wVerts w) (mx, my) })
  _ -> pure w
  where
    ww = envW env
    wh = envH env

-- ---------- やってみよう ----------
-- 1. 3 つの頂点・割合 0.5 → シェルピンスキーの三角形になる！（05 と同じ形）
-- 2. 頂点 4 つ、5 つ、6 つで、きれいに見える割合を探そう
-- 3. 「同じ頂点を続けて選ばない」ルールを消すと、四角形はどうなる？

-- ---------- 道具 ----------

-- 頂点がぜんぶ入る大きさのキャンバスを作る（ピクセル数が上限に収まるように粗さ k を決める）
fitCanvas :: U.Vector (Float, Float) -> Canvas
fitCanvas verts = Canvas x0 y1 k bw bh (U.generate (bw * bh * 3) fill)
  where
    xs = U.map fst verts
    ys = U.map snd verts
    x0 = U.minimum xs - 6
    x1 = U.maximum xs + 6
    y0 = U.minimum ys - 6
    y1 = U.maximum ys + 6
    k = max 1 (sqrt ((x1 - x0) * (y1 - y0) / maxPixels))
    bw = ceiling ((x1 - x0) / k)
    bh = ceiling ((y1 - y0) / k)
    (r, g, b) = bytes background
    fill i = case i `mod` 3 of
      0 -> r
      1 -> g
      _ -> b

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 点を色つきでキャンバスに重ねる
plotAll :: Canvas -> U.Vector (Word8, Word8, Word8) -> [(Int, Float, Float)] -> MU.MVector s Word8 -> ST s ()
plotAll c colors pts m = forM_ pts $ \(k, x, y) -> do
  let px = floor ((x - cLeft c) / cK c) :: Int
      py = floor ((cTop c - y) / cK c) :: Int
  if px < 0 || py < 0 || px >= cW c || py >= cH c then pure () else do
    let i = (py * cW c + px) * 3
        (r, g, b) = colors U.! k
    blend m i r
    blend m (i + 1) g
    blend m (i + 2) b

blend :: MU.MVector s Word8 -> Int -> Word8 -> ST s ()
blend m i v = do
  old <- MU.unsafeRead m i
  let o = fromIntegral old :: Float
      n = o + (fromIntegral v - o) * dotAlpha
  MU.unsafeWrite m i (fromIntegral (round n :: Int))

-- 小数 2 桁の文字列（p5.js の nf(x, 1, 2)）
nf2 :: Float -> String
nf2 x = showFFloat (Just 2) x ""
