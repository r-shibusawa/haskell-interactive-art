-- ============================================
-- 05 ストレンジ・アトラクター画集
-- 2つの数 (x, y) を、sin と cos だけの式でくり返し更新して点を打つ。
-- 何十万回打っても、点は決まった「かたち（アトラクター）」の上にしか落ちない
-- マウス = 式のパラメータ a, b を変える / 1〜3 キーで式の種類 / S で保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)

data World = World
  { wA, wB :: !Float            -- マウスで変えるパラメータ
  , wX, wY :: !Float            -- いまの点
  , wType  :: !Int              -- 式の種類（1〜3）
  , wBuf   :: !(U.Vector Float) -- 点を打ちためる画像（1 ピクセルに R, G, B の 3 つ）
  }

c, d :: Float
c = 1.0                         -- ★ 固定のパラメータ
d = 0.7

pointsPerFrame :: Int
pointsPerFrame = 6000           -- ★ 1フレームに打つ点の数

bufSize :: Int
bufSize = 280                   -- 画像の 1 辺（280 × 280 = 78400 画素）

viewRange :: Float
viewRange = 2.1                 -- 画像に映す x, y の範囲（−2.1〜2.1）

pointAlpha :: Float
pointAlpha = 0.05               -- ★ 点 1 つの濃さ

main :: IO ()
main = runSketch (sketch "04-05 Attractor Gallery" (World (-1.4) 1.6 0.1 0.1 1 blankBuf))
  { skBack  = hsb 0 0 5
  , skStep  = \_ _ w -> pure (plotPoints w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

blankBuf :: U.Vector Float
blankBuf = U.replicate (3 * bufSize * bufSize) 0.05

-- ★ 式はこれだけ。sin と cos が点を「折りたたむ」
formula :: Int -> Float -> Float -> Float -> Float -> (Float, Float)
formula 1 a b x y = (sin (a * y) + c * cos (a * x), sin (b * x) + d * cos (b * y))   -- クリフォード
formula 2 a b x y = (sin (a * y) - cos (b * x), sin (c * x) - cos (d * y))           -- デ・ヨング
formula _ a b x y = (d * sin (a * x) - sin (b * y), c * cos (a * x) + cos (b * y))   -- スヴェンソン

typeName :: Int -> String
typeName 1 = "Clifford"
typeName 2 = "De Jong"
typeName _ = "Svensson"

-- 画像をコピーして点を打ち足す
plotPoints :: World -> World
plotPoints w = runST $ do
  mv <- U.thaw (wBuf w)
  (x, y) <- loop mv (formula (wType w) (wA w) (wB w)) pointsPerFrame (wX w) (wY w)
  buf <- U.unsafeFreeze mv
  pure w { wX = x, wY = y, wBuf = buf }

loop :: M.MVector s Float -> (Float -> Float -> (Float, Float)) -> Int -> Float -> Float -> ST s (Float, Float)
loop mv f !n !x !y
  | n <= 0 = pure (x, y)
  | otherwise = do
      let (nx, ny) = f x y
      plot mv nx ny
      loop mv f (n - 1) nx ny

-- 点を 1 つ打つ。点の「向き」で色を変える
plot :: M.MVector s Float -> Float -> Float -> ST s ()
plot mv x y = when (bx >= 0 && bx < bufSize && by >= 0 && by < bufSize) $ do
  blend i cr
  blend (i + 1) cg
  blend (i + 2) cb
  where
    toBuf v = floor ((v + viewRange) / (2 * viewRange) * fromIntegral bufSize) :: Int
    bx = toBuf x
    by = toBuf y
    i = 3 * (by * bufSize + bx)
    (cr, cg, cb, _) = rgbaOfColor (hsb (atan2 y x * 60 + 200) 60 100)
    blend k v = M.modify mv (\old -> old + (v - old) * pointAlpha) k

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ pixelImage bufSize bufSize k pixel
  , txt env 20 40 18 white
      (typeName (wType w) ++ " attractor   a = " ++ fmt 2 (wA w) ++ "  b = " ++ fmt 2 (wB w)
        ++ " (mouse)   1-3: formula   S: save")
  ]
  where
    s = min (envW env) (envH env) * 0.2             -- ★ 縮尺
    k = 2 * viewRange * s / fromIntegral bufSize
    buf = wBuf w
    pixel x y =
      let i = 3 * (y * bufSize + x)
      in (byte (U.unsafeIndex buf i), byte (U.unsafeIndex buf (i + 1)), byte (U.unsafeIndex buf (i + 2)))
    byte v = fromIntegral (truncate (v * 255 + 0.5) :: Int)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion _ -> pure (followMouse env w)   -- 動かすまでは最初の a, b のまま
  EventKey (Char ch) Down _ _
    | ch >= '1' && ch <= '3' -> pure w { wType = fromEnum ch - fromEnum '0', wBuf = blankBuf }
    | ch == 's' || ch == 'S' -> saveSvg "attractor" env (drawWorld env w) >> pure w
  _ -> pure w

-- マウスで a, b を決める（動いたら描き直す）
followMouse :: Env -> World -> World
followMouse env w
  | abs (na - wA w) > 0.004 || abs (nb - wB w) > 0.004 = w { wA = na, wB = nb, wBuf = blankBuf }
  | otherwise = w
  where
    (mx, my) = mouseP5 env
    na = remapC mx 0 (envW env) (-2.5) 2.5
    nb = remapC my 0 (envH env) (-2.5) 2.5

-- ---------- やってみよう ----------
-- 1. マウスをゆっくり動かして、お気に入りの形を見つけたら S で保存
-- 2. c, d も変えてみよう（固定されている値）
-- 3. 色を「向き」ではなく「何回目の点か」で変えると？

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b e f = clampF (min e f) (max e f) (remap v a b e f)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
