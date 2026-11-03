-- ============================================
-- 15 粘菌（スライムモールド）シミュレーション（おまけ）
-- 数千の粒が「前の3方向のにおいをかいで、強い方へ進み、においを残す」だけ。
-- 本物の粘菌が電車の路線図のようなネットワークをつくるのと同じしくみ
-- クリックでエサ（においの源）を置く / 1〜3 で動きのタイプ / C でリセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- 粒（x, y, 向き）
type Agent = (Float, Float, Float)

-- ★ 粘菌の性格
data Mood = Mood
  { mAngle :: !Float      -- においをかぐ角度
  , mDist  :: !Float      -- においをかぐ距離
  , mTurn  :: !Float      -- 曲がる速さ
  }

data World = World
  { wTrail  :: !(U.Vector Float)   -- においの地図
  , wTrail2 :: !(U.Vector Float)   -- ぼかした結果を入れる2枚目
  , wAgents :: !(U.Vector Agent)
  , wFoods  :: ![Int]              -- エサのあるマス
  , wMood   :: !Mood
  }

gridScale :: Int
gridScale = 3              -- ★ においマップの粗さ（大きいほど速い）

gw, gh :: Int
gw = 1280 `div` gridScale
gh = 800 `div` gridScale

agentCount :: Int
agentCount = 4000          -- ★ 粒の数

fade :: Float
fade = 0.95                -- ★ においの消える速さ

-- 表示用の画像の大きさ（8 万画素までに収めるため、地図より少し粗くする）
imgW, imgH :: Int
imgW = 355
imgH = 222

noTrail :: U.Vector Float
noTrail = U.replicate (gw * gh) 0

main :: IO ()
main = runSketch (sketch "03-15 Slime Mold" (World noTrail noTrail U.empty [] (Mood 0.4 9 0.3)))
  { skBack  = black
  , skStart = \_ -> do
      agents <- U.replicateM agentCount newAgent
      pure (World noTrail noTrail agents [] (Mood 0.4 9 0.3))
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newAgent :: IO Agent
newAgent = do
  a <- randomRIO (0, tau)
  r <- randomRIO (0, fromIntegral (min gw gh) * 0.3)
  ang <- randomRIO (0, tau)
  pure (fromIntegral gw / 2 + cos a * r, fromIntegral gh / 2 + sin a * r, ang)

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w = do
  trail <- U.thaw (wTrail w)
  -- エサはにおいを出し続ける
  forM_ (wFoods w) $ \i -> MU.write trail i 5
  -- ---- 粒を動かす ----
  agents <- U.mapM (moveAgent (wMood w) trail) (wAgents w)
  frozen <- U.unsafeFreeze trail
  -- ---- においを広げて（ぼかす）、少し消す ----
  pure w { wTrail = blur frozen (wTrail2 w), wTrail2 = frozen, wAgents = agents }

moveAgent :: Mood -> MU.IOVector Float -> Agent -> IO Agent
moveAgent mood trail (x, y, ang) = do
  -- 前・左前・右前 のにおいを調べる
  f <- sense mood trail x y ang 0
  l <- sense mood trail x y ang (mAngle mood)
  r <- sense mood trail x y ang (negate (mAngle mood))
  ang1 <- if f > l && f > r then pure ang            -- まっすぐ
          else if l > r then pure (ang + mTurn mood)
          else if r > l then pure (ang - mTurn mood)
          else (ang +) <$> randomRIO (-0.3, 0.3)
  let x1 = x + cos ang1
      y1 = y + sin ang1
      -- 端で跳ね返る
      wide = fromIntegral gw
      high = fromIntegral gh
      ang2 = if x1 < 0 || x1 >= wide then pi - ang1 else ang1
      x2 = if x1 < 0 || x1 >= wide then clampF 0 (wide - 1) x1 else x1
      ang3 = if y1 < 0 || y1 >= high then negate ang2 else ang2
      y2 = if y1 < 0 || y1 >= high then clampF 0 (high - 1) y1 else y1
      -- においを残す
      i = floor x2 + floor y2 * gw
  v <- MU.unsafeRead trail i
  MU.unsafeWrite trail i (min (v + 1) 5)
  pure (x2, y2, ang3)

-- 角度 offset の方向、mDist 先のにおいの強さ
sense :: Mood -> MU.IOVector Float -> Float -> Float -> Float -> Float -> IO Float
sense mood trail x y ang offset =
  let a = ang + offset
      sx = floor (x + cos a * mDist mood) :: Int
      sy = floor (y + sin a * mDist mood) :: Int
  in if sx < 0 || sy < 0 || sx >= gw || sy >= gh then pure 0 else MU.unsafeRead trail (sx + sy * gw)

-- 端のマス以外をぼかす（端のマスは 2 枚目の地図の値がそのまま残る）
blur :: U.Vector Float -> U.Vector Float -> U.Vector Float
blur t other = U.modify paint other
  where
    paint m =
      let row !y
            | y >= gh - 1 = pure ()
            | otherwise = cell y 1 >> row (y + 1)
          cell !y !x
            | x >= gw - 1 = pure ()
            | otherwise = do
                let i = x + y * gw
                    at' = U.unsafeIndex t
                    s = at' (i - 1) + at' (i + 1) + at' (i - gw) + at' (i + gw) + at' i * 4
                MU.unsafeWrite m i (s / 8 * fade)
                cell y (x + 1)
      in row 1

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Scale (envW env / fromIntegral imgW) (envH env / fromIntegral imgH) (pixelImage imgW imgH 1 pixel)
  , Color (makeColor 0 0 0 0.7) (rect 0 (envH env / 2 - 22) (envW env) 44)
  , hud env 28 16 white
      ("Click: place food   1: paths   2: fine mesh   3: waves   C: reset   Particles " ++ show agentCount)
  ]
  where
    -- ★ 色の式
    pixel :: Int -> Int -> (Word8, Word8, Word8)
    pixel x y =
      let gx = x * gw `div` imgW
          gy = y * gh `div` imgH
          v = min (U.unsafeIndex (wTrail w) (gx + gy * gw) / 2) 1
      in (truncate (v * 255), truncate (v * v * 230), truncate (v * 80))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        x = floor (mx / envW env * fromIntegral gw) :: Int
        y = floor (my / envH env * fromIntegral gh) :: Int
    in pure $ if my > 44 && x >= 0 && y >= 0 && x < gw && y < gh
         then w { wFoods = (x + y * gw) : wFoods w }
         else w
  EventKey (Char '1') Down _ _ -> pure w { wMood = Mood 0.4 9 0.3 }
  EventKey (Char '2') Down _ _ -> pure w { wMood = Mood 0.8 4 0.6 }
  EventKey (Char '3') Down _ _ -> pure w { wMood = Mood 0.2 20 0.1 }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wTrail = noTrail, wFoods = [] }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. エサを3〜4か所に置いて、粘菌がそれらをつなぐ道をつくるのを待とう
-- 2. 消える速さ fade の 0.95 を 0.9 や 0.99 に
-- 3. pixel の色の式を変えて、本物の粘菌（黄色）や、電気回路のような見た目に

-- ---------- 道具 ----------

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
