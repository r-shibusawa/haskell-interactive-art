-- ============================================
-- 04 反応拡散（チューリング・パターン）
-- 2種類の化学物質 A と B が「広がる」「反応する」だけで、
-- ヒョウの斑点やシマウマの縞、サンゴのような模様が自然にできる
-- クリック・ドラッグで B をたらす / 1〜4 キーで模様のタイプ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Text.Printf (printf)

data World = World
  { wA    :: !(U.Vector Float)
  , wB    :: !(U.Vector Float)
  , wFeed :: !Float             -- 反応のパラメータ（これで模様が変わる）
  , wKill :: !Float
  , wName :: !String
  }

gw, gh :: Int
gw = 160                   -- ★ 計算する大きさ（小さいほど速い）
gh = 90

dA, dB :: Float
dA = 1.0                   -- ★ 広がる速さ
dB = 0.5

stepsPerFrame :: Int
stepsPerFrame = 8          -- ★ 1フレームに何回計算を進めるか

-- ★ 模様のタイプ（名前, feed, kill）
presets :: [(Char, String, Float, Float)]
presets =
  [ ('1', "Spots (leopard)", 0.035, 0.065)
  , ('2', "Coral", 0.055, 0.062)
  , ('3', "Mitochondria", 0.03, 0.06)
  , ('4', "Waves", 0.014, 0.045)
  ]

start :: World
start = World (U.replicate (gw * gh) 1) (U.replicate (gw * gh) 0) 0.055 0.062 "Coral"

main :: IO ()
main = runSketch (sketch "03-04 Reaction Diffusion" start)
  { skStart = \_ -> do
      -- B をひとたらし
      spots <- replicateM 300 (randomRIO (0, gw * gh - 1))
      pure start { wB = wB start U.// [(i, 1) | i <- spots] }
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: World -> World
stepWorld w =
  let go :: Int -> U.Vector Float -> U.Vector Float -> (U.Vector Float, U.Vector Float)
      go 0 a b = (a, b)
      go n a b = let (a', b') = update (wFeed w) (wKill w) a b in go (n - 1) a' b'
      (a2, b2) = go stepsPerFrame (wA w) (wB w)
  in w { wA = a2, wB = b2 }

-- ★ グレイ＝スコット・モデル：A と B のルール
update :: Float -> Float -> U.Vector Float -> U.Vector Float -> (U.Vector Float, U.Vector Float)
update feed kill a b = runST $ do
  na <- MU.unsafeNew (gw * gh)
  nb <- MU.unsafeNew (gw * gh)
  let row !y
        | y >= gh = pure ()
        | otherwise = cell y 0 >> row (y + 1)
      cell !y !x
        | x >= gw = pure ()
        | otherwise = do
            let i = x + y * gw
                av = U.unsafeIndex a i
                bv = U.unsafeIndex b i
                xm = if x == 0 then gw - 1 else x - 1
                xp = if x == gw - 1 then 0 else x + 1
                ym = (if y == 0 then gh - 1 else y - 1) * gw
                yp = (if y == gh - 1 then 0 else y + 1) * gw
                yy = y * gw
                -- まわりとの差（ラプラシアン）＝「広がる」のもと
                la = laplace a x xm xp yy ym yp
                lb = laplace b x xm xp yy ym yp
                abb = av * bv * bv             -- A と B が出会うと A が B に変わる（反応）
            MU.unsafeWrite na i (clampF 0 1 (av + (dA * la - abb + feed * (1 - av))))
            MU.unsafeWrite nb i (clampF 0 1 (bv + (dB * lb + abb - (kill + feed) * bv)))
            cell y (x + 1)
  row 0
  a' <- U.unsafeFreeze na
  b' <- U.unsafeFreeze nb
  pure (a', b')

laplace :: U.Vector Float -> Int -> Int -> Int -> Int -> Int -> Int -> Float
laplace v !x !xm !xp !yy !ym !yp =
  let at' i = U.unsafeIndex v i
  in negate (at' (x + yy))
     + 0.2 * (at' (xm + yy) + at' (xp + yy) + at' (x + ym) + at' (x + yp))
     + 0.05 * (at' (xm + ym) + at' (xp + ym) + at' (xm + yp) + at' (xp + yp))
{-# INLINE laplace #-}

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Scale (envW env / fromIntegral gw) (envH env / fromIntegral gh) (pixelImage gw gh 1 pixel)
  , Color (makeColor 0 0 0 0.7) (rect 0 (envH env / 2 - 22) (envW env) 44)
  , hud env 28 16 white
      ("Click/drag: drop B   1-4: " ++ wName w ++ " (f=" ++ printf "%.3f" (wFeed w) ++ ", k=" ++ printf "%.3f" (wKill w) ++ ")")
  ]
  where
    -- B の濃さで色をつける（★ 色の式）
    pixel :: Int -> Int -> (Word8, Word8, Word8)
    pixel x y =
      let v = clampF 0 1 (U.unsafeIndex (wB w) (x + y * gw) * 3)
      in (truncate (20 + v * 220), truncate (30 + v * 120), truncate (60 + (1 - v) * 80))

dropB :: Env -> World -> World
dropB env w =
  let (mx, my) = mouseP5 env
      cx = floor (remap mx 0 (envW env) 0 (fromIntegral gw)) :: Int
      cy = floor (remap my 0 (envH env) 0 (fromIntegral gh)) :: Int
      spots = [ (((cx + i) `mod` gw) + ((cy + j) `mod` gh) * gw, 1) | i <- [-2 .. 2], j <- [-2 .. 2] ]
  in w { wB = wB w U.// spots }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (dropB env w)
  EventMotion _ | envDown env -> pure (dropB env w)
  EventKey (Char c) Down _ _ ->
    pure (foldr (\(k, name, f, kl) acc -> if k == c then w { wFeed = f, wKill = kl, wName = name } else acc) w presets)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1〜4 で模様を切りかえて、しばらく待とう。模様が「育つ」
-- 2. presets の feed と kill を自分で少しずつ変えてみる（0.01 ずらすだけで別の模様）
-- 3. pixel の色の式を変えて、本物の動物の柄に近づけよう

-- ---------- 道具 ----------

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
