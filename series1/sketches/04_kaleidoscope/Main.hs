-- ============================================
-- 04 万華鏡ペイント
-- 描いた線が回転＆反転してコピーされる
-- 数字キー 2〜9 で分割数を変更 / C で消す / S で保存
-- ============================================
-- 線 1 本が (分割数 × 2) 本に増えるので、全部を線のまま覚えると重くなる。
-- 新しい線は線のまま描き、古い線はピクセル画像（wCanvas）に焼きつけて残す。
module Main (main) where

import Art
import Control.Monad (forM_, when)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)

-- 描いた線 1 本
data Seg = Seg
  { sFrom   :: !Point
  , sTo     :: !Point
  , sWeight :: !Float
  , sColor  :: !Color
  , sPic    :: Picture      -- この線の絵（一度つくれば使いまわせる）
  }

data World = World
  { wSym     :: !Int                  -- いくつに分けてコピーするか
  , wHue     :: !Float
  , wRecent  :: ![Seg]                -- 線のまま描く新しい線（新しいものが先頭）
  , wCanvas  :: !(U.Vector Word8)     -- 焼きつけた古い線（RGB が順に入ったバイト列）
  , wBaked   :: Picture               -- wCanvas の絵（焼きつけたときだけ作りなおす）
  , wPrev    :: !Point
  , wWasDown :: !Bool
  }

-- 焼きつけ用の画像の大きさ
canvasW, canvasH :: Int
canvasW = 356
canvasH = 222

canvasK :: Float
canvasK = 3.6              -- 画面では 3.6 倍に拡大する

initialSym :: Int
initialSym = 8             -- ★ 最初の分割数

background :: Color
background = hsb 0 0 8

blankCanvas :: U.Vector Word8
blankCanvas = U.replicate (canvasW * canvasH * 3) 20     -- 20 ≒ 明るさ 8%

initialWorld :: World
initialWorld = World initialSym 0 [] blankCanvas Blank (0, 0) False

main :: IO ()
main = runSketch (sketch "01-04 Kaleidoscope" initialWorld)
  { skBack  = background
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w
  | envDown env = bakeIfNeeded w
      { wRecent = if moved then seg : wRecent w else wRecent w
      , wHue = wrapF 360 (wHue w + 1)      -- 360 を超えると 0 に戻る
      , wPrev = m
      , wWasDown = True
      }
  | otherwise = w { wPrev = m, wWasDown = False }
  where
    m = envMouse env
    moved = m /= wPrev w || not (wWasDown w)
    speed = dist2 (wPrev w) m
    weight = clampF 2 12 (remap speed 0 40 2 12)      -- ★ 速さ 0〜40 を 太さ 2〜12 に
    col = hsba (wHue w) 70 100 0.7                    -- ★ 彩度 70・透明度 0.7
    seg = Seg (wPrev w) m weight col (Color col (capsule weight (wPrev w) m))

-- 線の絵を、回転と反転で増やして描く
drawWorld :: World -> Picture
drawWorld w = Pictures (wBaked w : copies)
  where
    n = wSym w
    wedge = Pictures (reverse (map sPic (wRecent w)))
    copies =
      [ Rotate (360 * fromIntegral i / fromIntegral n)
               (Pictures [wedge, Scale 1 (-1) wedge])     -- もとの線と、上下反転（鏡）
      | i <- [1 .. n] ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '2' && c <= '9' -> pure (clearAll w) { wSym = fromEnum c - fromEnum '0' }
    | c == 'c' || c == 'C' -> pure (clearAll w)
    | c == 's' || c == 'S' -> do
        saveSvg "kaleidoscope" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), drawWorld w])
        pure w
  _ -> pure w

clearAll :: World -> World
clearAll w = w { wRecent = [], wCanvas = blankCanvas, wBaked = Blank }

-- ---------- やってみよう ----------
-- 1. 数字キーで分割数を変えて、模様の違いを見てみよう
-- 2. capsule の代わりに at (sTo seg) (disc 5) にすると？
-- 3. background と blankCanvas を白（hsb 0 0 100 と 255）にして描いてみよう

-- ---------- 道具 ----------

-- 線のまま描く本数の上限（分割数が多いほど少なくする）
maxRecent :: Int -> Int
maxRecent sym = 7000 `div` (2 * sym)

-- 線が増えすぎたら、古い 1/4 をピクセル画像に焼きつける
bakeIfNeeded :: World -> World
bakeIfNeeded w
  | count <= limit = w
  | otherwise = w { wRecent = keep, wCanvas = canvas, wBaked = imageOf canvas }
  where
    limit = maxRecent (wSym w)
    count = length (wRecent w)
    (keep, old) = splitAt (limit * 3 `div` 4) (wRecent w)
    canvas = bake (wSym w) (reverse old) (wCanvas w)

imageOf :: U.Vector Word8 -> Picture
imageOf v = pixelImage canvasW canvasH canvasK $ \x y ->
  let i = (y * canvasW + x) * 3 in (v U.! i, v U.! (i + 1), v U.! (i + 2))

-- 線ぜんぶを、回転・反転したコピーもふくめてバイト列に書きこむ
bake :: Int -> [Seg] -> U.Vector Word8 -> U.Vector Word8
bake sym segs = U.modify $ \mv ->
  forM_ segs $ \s ->
    forM_ [1 .. sym] $ \i -> do
      let a = tau * fromIntegral i / fromIntegral sym
      stamp mv s (turn a (sFrom s)) (turn a (sTo s))
      stamp mv s (turn a (mirror (sFrom s))) (turn a (mirror (sTo s)))
  where
    turn a (x, y) = (x * cos a + y * sin a, y * cos a - x * sin a)
    mirror (x, y) = (x, -y)

-- 1 本の線を、ふちをなめらかにしてバイト列に重ねる
stamp :: MU.PrimMonad m => MU.MVector (MU.PrimState m) Word8 -> Seg -> Point -> Point -> m ()
stamp mv s a b =
  forM_ [max 0 (floor (min ay by - r)) .. min (canvasH - 1) (ceiling (max ay by + r))] $ \y ->
    forM_ [max 0 (floor (min ax bx - r)) .. min (canvasW - 1) (ceiling (max ax bx + r))] $ \x -> do
      let d = distToSegment (fromIntegral x + 0.5, fromIntegral y + 0.5) (ax, ay) (bx, by)
          cover = clampF 0 1 (r - d + 0.5) * ca
          i = (y * canvasW + x) * 3
      when (cover > 0) $ do
        blend i cr cover
        blend (i + 1) cg cover
        blend (i + 2) cb cover
  where
    (ax, ay) = toCanvas a
    (bx, by) = toCanvas b
    r = sWeight s / 2 / canvasK
    (cr, cg, cb, ca) = rgbaOfColor (sColor s)
    blend i c t = MU.modify mv (\old -> round (lerpF (fromIntegral old) (c * 255) t)) i

-- 画面の座標 → 焼きつけ用の画像の座標（左上が原点、y は下向き）
toCanvas :: Point -> Point
toCanvas (x, y) = (x / canvasK + fromIntegral canvasW / 2, fromIntegral canvasH / 2 - y / canvasK)

-- 点 p と線分 ab の距離
distToSegment :: Point -> Point -> Point -> Float
distToSegment (px, py) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      len2 = dx * dx + dy * dy
      t = if len2 < 1e-6 then 0 else clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / len2)
  in dist2 (px, py) (ax + t * dx, ay + t * dy)

-- 両はしが丸い太い線を、1 つの多角形で描く（太さ w）
capsule :: Float -> Point -> Point -> Picture
capsule w (x1, y1) (x2, y2) = Polygon (cap x2 y2 (ang - pi / 2) ++ cap x1 y1 (ang + pi / 2))
  where
    ang = if x1 == x2 && y1 == y2 then 0 else atan2 (y2 - y1) (x2 - x1)
    cap cx cy a0 =
      [ (cx + w / 2 * cos a, cy + w / 2 * sin a)
      | i <- [0 .. 4 :: Int], let a = a0 + pi * fromIntegral i / 4 ]
