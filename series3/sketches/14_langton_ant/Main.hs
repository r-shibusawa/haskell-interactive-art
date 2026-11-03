-- ============================================
-- 14 ラングトンのアリ（おまけ）
-- ルールは2つだけ：白マスなら右に曲がって黒くする、黒マスなら左に曲がって白くする
-- 1万歩ほどはめちゃくちゃ。でもそのあと突然「高速道路」をつくり始める
-- マウス左右 = 速さ / クリックでアリを追加 / R でリセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Word (Word16, Word8)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

data Ant = Ant
  { aX   :: !Int
  , aY   :: !Int
  , aDir :: !Int      -- 0 = 上, 1 = 右, 2 = 下, 3 = 左
  , aHue :: !Int
  }

data World = World
  { wGrid  :: !(U.Vector Word16)   -- 0 = 白、それ以外 = 黒（アリの色相 + 1 を覚えておく）
  , wAnts  :: ![Ant]
  , wSteps :: !Int
  , wSpeed :: !Int
  }

cellSize :: Int
cellSize = 4               -- ★ マスの大きさ

cols, rows :: Int
cols = 1280 `div` cellSize
rows = (800 - 50) `div` cellSize

maxSpeed :: Float
maxSpeed = 400             -- ★ 1フレームに進む歩数の上限

start :: World
start = World (U.replicate (cols * rows) 0) [Ant (cols `div` 2) (rows `div` 2) 0 0] 0 1

main :: IO ()
main = runSketch (sketch "03-14 Langton's Ant" start)
  { skBack  = white
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: Env -> World -> World
stepWorld env w =
  let speed = floor (clampF 1 maxSpeed (remap (fst (mouseP5 env)) 0 (envW env) 1 maxSpeed)) :: Int
      (ants, grid) = runAnts speed (wAnts w) (wGrid w)
  in w { wAnts = ants, wGrid = grid, wSteps = wSteps w + speed, wSpeed = speed }

runAnts :: Int -> [Ant] -> U.Vector Word16 -> ([Ant], U.Vector Word16)
runAnts speed ants grid = runST $ do
  g <- U.thaw grid
  ants' <- foldM (\as _ -> mapM (stepAnt g) as) ants [1 .. speed]
  g' <- U.unsafeFreeze g
  pure (ants', g')

-- ★ ここがルール
stepAnt :: MU.MVector s Word16 -> Ant -> ST s Ant
stepAnt g (Ant x y dir hue) = do
  let i = x + y * cols
  v <- MU.unsafeRead g i
  let dir' = if v == 0
        then (dir + 1) `mod` 4     -- 白 → 右に曲がる
        else (dir + 3) `mod` 4     -- 黒 → 左に曲がる
  MU.unsafeWrite g i (if v == 0 then fromIntegral hue + 1 else 0)   -- 色を反転する
  let (dx, dy) = direction dir'
  pure (Ant ((x + dx + cols) `mod` cols) ((y + dy + rows) `mod` rows) dir' hue)

direction :: Int -> (Int, Int)
direction d = case d of
  0 -> (0, -1)
  1 -> (1, 0)
  2 -> (0, 1)
  _ -> (-1, 0)

-- マスの値 → 色（黒くするときはアリごとの色）
palette :: U.Vector (Word8, Word8, Word8)
palette = U.generate 362 $ \v ->
  if v == 0 then (255, 255, 255) else toRgb (hsb (fromIntegral (v - 1)) 60 30)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Translate 0 (envH env / 2 - 50 - fromIntegral (rows * cellSize) / 2) $
      pixelImage cols rows (fromIntegral cellSize) $ \x y ->
        U.unsafeIndex palette (fromIntegral (U.unsafeIndex (wGrid w) (x + y * cols)))
  , p5Space env $ Pictures
      -- アリの現在地
      [ Color (hsb 0 90 100) (box (fromIntegral (aX a * cellSize)) (50 + fromIntegral (aY a * cellSize)) size size)
      | a <- wAnts w ]
  , Color white (rect 0 (envH env / 2 - 25) (envW env) 50)
  , hud env 20 15 (hsb 0 0 10) ("Steps " ++ show (wSteps w) ++ "   Ants " ++ show (length (wAnts w)))
  , hud env 40 15 (hsb 0 0 10) ("Mouse X: speed (" ++ show (wSpeed w) ++ " steps/frame)   Click: add ant   R: reset")
  ]
  where size = fromIntegral cellSize

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my >= 50 -> do
        d <- randomRIO (0, 3)
        h <- randomRIO (0, 359)
        let x = max 0 (min (cols - 1) (floor (mx / fromIntegral cellSize)))
            y = max 0 (min (rows - 1) (floor ((my - 50) / fromIntegral cellSize)))
        pure w { wAnts = wAnts w ++ [Ant x y d h] }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> pure start
  _ -> pure w
  where (mx, my) = mouseP5 env

-- ---------- やってみよう ----------
-- 1. 速さを最大にして待つ。約 10,000 歩で「高速道路」が始まる
-- 2. アリを2匹にすると、高速道路はできる？ 衝突すると？
-- 3. ルールを「白→左、黒→右」に変えると？（鏡になるだけ？）
--    3色にして「右・右・左」にすると、別の模様が出る（ターマイトという拡張）

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の rect と同じ、左上の角 (x, y) と幅・高さで決める長方形
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size

toRgb :: Color -> (Word8, Word8, Word8)
toRgb c = let (r, g, b, _) = rgbaOfColor c in (byte r, byte g, byte b)
  where byte v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
