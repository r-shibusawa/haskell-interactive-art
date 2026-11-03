-- ============================================
-- 03 森林火災モデル
-- 空き地に木が生え、まれに雷が落ち、火はとなりの木に広がる
-- 3つの確率だけで、森が「呼吸」するように燃えて再生する
-- クリックで火をつける / マウス左右 = 木の生える確率 / 上下 = 雷の確率
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, newStdGen, randomR)
import Text.Printf (printf)

data World = World
  { wGrid  :: !(U.Vector Word8)   -- x + y * cols 番目のマス
  , wGen   :: !StdGen             -- マスの数だけ乱数を使うので、速い乱数を持っておく
  , wGrow  :: !Float              -- 空き地に木が生える確率
  , wBolt  :: !Float              -- 木に雷が落ちる確率
  }

emptyCell, tree, fire :: Word8
emptyCell = 0
tree = 1
fire = 2

cellSize :: Int
cellSize = 6               -- ★ マスの大きさ

cols, rows :: Int
cols = 1280 `div` cellSize
rows = (800 - 50) `div` cellSize

main :: IO ()
main = runSketch (sketch "03-03 Forest Fire" (World (U.replicate (cols * rows) emptyCell) (mkStdGen 1) 0.01 0.00005))
  { skBack  = hsb 30 30 10
  , skStart = \_ -> do
      g <- newStdGen
      -- 最初は半分が木
      pure (World (U.map (\r -> if r < 0.5 then tree else emptyCell) (randoms (cols * rows) g)) g 0.01 0.00005)
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randoms :: Int -> StdGen -> U.Vector Float
randoms n = U.unfoldrExactN n (randomR (0, 1))

stepWorld :: Env -> World -> World
stepWorld env w =
  let (mx, my) = mouseP5 env
      -- ★ マウスで確率を変える
      pGrow = clampF 0.001 0.03 (remap mx 0 (envW env) 0.001 0.03)
      pBolt = clampF 0.000005 0.0005 (remap my 0 (envH env) 0.0005 0.000005)
      (seed, g2) = randomR (0, maxBound :: Int) (wGen w)
      rs = randoms (cols * rows) (mkStdGen seed)
      grid = wGrid w
      rule i c r
        | c == fire = emptyCell                              -- 燃えた木は空き地になる
        | c == tree = if r < pBolt || neighborOnFire grid i then fire else tree  -- 燃え移る、または雷
        | r < pGrow = tree                                   -- 空き地に木が生える
        | otherwise = emptyCell
  in w { wGrid = U.izipWith rule grid rs, wGen = g2, wGrow = pGrow, wBolt = pBolt }

-- まわり8マスに火があるか（端はつながっていない）
neighborOnFire :: U.Vector Word8 -> Int -> Bool
neighborOnFire grid i = any burning [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where
    x = i `mod` cols
    y = i `div` cols
    burning (dx, dy) =
      let nx = x + dx
          ny = y + dy
      in nx >= 0 && ny >= 0 && nx < cols && ny < rows && U.unsafeIndex grid (nx + ny * cols) == fire

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Translate (fromIntegral (cols * cellSize) / 2 - envW env / 2)
              (envH env / 2 - 50 - fromIntegral (rows * cellSize) / 2) $
      pixelImage cols rows (fromIntegral cellSize) (\x y -> cellColor (envFrame env) x y (U.unsafeIndex grid (x + y * cols)))
  , hud env 20 15 white
      ("Trees " ++ show (count tree) ++ "   Fire " ++ show (count fire)
        ++ "   Grow " ++ printf "%.3f" (wGrow w)
        ++ "   Lightning " ++ printf "%.2f" (wBolt w * 10000) ++ " per 10000")
  , hud env 40 15 white "Click: set fire   Mouse X: grow chance   Mouse Y: lightning chance"
  ]
  where
    grid = wGrid w
    count c = U.length (U.filter (== c) grid)

type Rgb = (Word8, Word8, Word8)

cellColor :: Int -> Int -> Int -> Word8 -> Rgb
cellColor frame x y c
  | c == tree = treeColor
  | c == fire = U.unsafeIndex fireColors ((x * 73 + y * 179 + frame * 283) `mod` 31)   -- 火の色はマスごとにゆらす
  | otherwise = groundColor

treeColor, groundColor :: Rgb
treeColor = toRgb (hsb 110 70 55)
groundColor = toRgb (hsb 30 30 10)

fireColors :: U.Vector Rgb
fireColors = U.generate 31 (\i -> toRgb (hsb (10 + fromIntegral i) 90 100))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        x = floor (mx / fromIntegral cellSize) :: Int
        y = floor ((my - 50) / fromIntegral cellSize) :: Int
        i = x + y * cols
    in pure $ if x >= 0 && x < cols && y >= 0 && y < rows && wGrid w U.! i == tree
         then w { wGrid = wGrid w U.// [(i, fire)] }
         else w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 木が密集するまで待ってクリック → 大火災。すぐクリック → 小さな火。「密度」が大事
-- 2. 雷をゼロにして、木が生える確率を最大に。全部木になったらクリック
-- 3. 火が「斜め」に広がらないようにすると？（neighborOnFire のリストを4方向にする）

-- ---------- 道具 ----------

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size

toRgb :: Color -> Rgb
toRgb c = let (r, g, b, _) = rgbaOfColor c in (byte r, byte g, byte b)
  where byte v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
