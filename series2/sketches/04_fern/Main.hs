-- ============================================
-- 04 バーンズリーのシダ（点を打つだけで葉ができる）
-- 4 つの「縮めて動かす」変換をランダムに選んで、点を何万個も打つ
-- マウスを左右に動かすと、葉の形が変わる（変換の角度を変えている）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import System.Random (StdGen, mkStdGen, randomR)

-- 点を描きためるキャンバス。シダのまわりだけをピクセル画像で持つ
bufW, bufH :: Int
bufW = 220
bufH = 350

data World = World
  { wX, wY :: !Float            -- 今の点（数学の座標）
  , wTwist :: !Float            -- 葉のねじれ（マウスで変える）
  , wGen   :: !StdGen           -- 乱数の状態
  , wBuf   :: !(U.Vector Word8) -- 赤緑青を 3 バイトずつ
  }

pointsPerFrame :: Int
pointsPerFrame = 3000           -- ★ 1 フレームに打つ点の数

dotAlpha :: Float
dotAlpha = 0.25                 -- ★ 点の濃さ（1 ピクセルに何度も重なって明るくなる）

main :: IO ()
main = runSketch (sketch "02-04 Barnsley Fern" (World 0 0 0 (mkStdGen 1) blankBuf))
  { skBack  = hsb 140 30 6
  , skStart = \_ -> do
      s <- randomRIO (0, 1000000)
      pure (World 0 0 0 (mkStdGen s) blankBuf)
  , skStep  = step
  , skDraw  = drawWorld
  }

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w { wX = x, wY = y, wGen = g, wBuf = U.modify (plotAll pts) (wBuf w) }
  where
    -- マウスが動いたら、形を変えて描き直す
    t = clampF (-0.25) 0.25 (remap (fst (envMouse env) + envW env / 2) 0 (envW env) (-0.25) 0.25)
    w | abs (t - wTwist w0) > 0.002 = w0 { wTwist = t, wBuf = blankBuf }
      | otherwise = w0
    (pts, x, y, g) = fernPoints pointsPerFrame (wTwist w) (wX w) (wY w) (wGen w)

-- 点を n 個ぶん進める
fernPoints :: Int -> Float -> Float -> Float -> StdGen -> ([(Float, Float)], Float, Float, StdGen)
fernPoints n twist = go n []
  where
    go :: Int -> [(Float, Float)] -> Float -> Float -> StdGen -> ([(Float, Float)], Float, Float, StdGen)
    go 0 acc !x !y g = (acc, x, y, g)
    go k acc !x !y g =
      let (r, g') = randomR (0, 1) g   -- 0〜1 のサイコロで、どの変換を使うか決める
          (nx, ny) = transform twist r x y
      in go (k - 1) ((nx, ny) : acc) nx ny g'

-- 4 つの変換
transform :: Float -> Float -> Float -> Float -> (Float, Float)
transform twist r x y
  -- 1%：茎
  | r < 0.01 = (0, 0.16 * y)
  -- 85%：葉全体を少し縮めて上へ（ここをマウスでねじる）
  | r < 0.86 = let a = -0.04 + twist in (0.85 * x + a * y, -a * x + 0.85 * y + 1.6)
  -- 7%：左の小葉
  | r < 0.93 = (0.2 * x - 0.26 * y, 0.23 * x + 0.22 * y + 1.6)
  -- 7%：右の小葉
  | otherwise = (-0.15 * x + 0.28 * y, 0.26 * x + 0.24 * y + 0.44)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ pixelImage bufW bufH (envH env / fromIntegral bufH) pixel
  , label (-envW env / 2 + 20) (envH env / 2 - 30) 16 white
      ("Move the mouse left/right to bend the leaf   Points " ++ show (envFrame env * pointsPerFrame))
  ]
  where
    buf = wBuf w
    pixel x y =
      let i = (y * bufW + x) * 3
      in (U.unsafeIndex buf i, U.unsafeIndex buf (i + 1), U.unsafeIndex buf (i + 2))

-- ---------- やってみよう ----------
-- 1. 0.85 を 0.8 や 0.9 にすると、葉の長さが変わる
-- 2. 確率（0.01 / 0.86 / 0.93）を変えると、点の集まり方が変わる
-- 3. 色を y ではなく、どの変換を使ったかで変えてみよう

-- ---------- 道具 ----------

-- 背景色で塗ったキャンバス
blankBuf :: U.Vector Word8
blankBuf = U.generate (bufW * bufH * 3) (\i -> if i `mod` 3 == 0 then r else if i `mod` 3 == 1 then g else b)
  where (r, g, b) = bytes (hsb 140 30 6)

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 数学の座標（x: -3〜3, y: 0〜10）→ キャンバスの座標 に点を重ねる
plotAll :: [(Float, Float)] -> MU.MVector s Word8 -> ST s ()
plotAll pts m = forM_ pts $ \(x, y) -> do
  let bh = fromIntegral bufH :: Float
      px = floor (fromIntegral bufW / 2 + x / 3 * 0.3 * bh) :: Int
      py = floor (bh * 0.025 + (10 - y) / 10 * bh * 0.95) :: Int
  if px < 0 || py < 0 || px >= bufW || py >= bufH then pure () else do
    let i = (py * bufW + px) * 3
        (r, g, b) = bytes (hsb (remap y 0 10 90 150) 60 100)
    blend m i r
    blend m (i + 1) g
    blend m (i + 2) b

blend :: MU.MVector s Word8 -> Int -> Word8 -> ST s ()
blend m i v = do
  old <- MU.unsafeRead m i
  let o = fromIntegral old :: Float
      n = o + (fromIntegral v - o) * dotAlpha
  MU.unsafeWrite m i (fromIntegral (round n :: Int))
