-- ============================================
-- 14 パーコレーション（染み込み）（おまけ）
-- 格子の各マスを確率 p で「通れる」にする。p がある値（約 0.593）を超えると、
-- 突然、上から下まで通れる道ができる（相転移）。04 の巨大成分と同じ現象を格子で
-- マウス左右 = p / クリックで配置をやり直す / 上から染み込んだ水が青く広がる
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, forM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, listArray, (!))
import Numeric (showFFloat)
import System.Random (mkStdGen, randoms)

-- 同じ seed なら同じ乱数の列になるので、p を上げると通れるマスは「増える」だけ
newtype World = World { wSeed :: Int }

cols, rows, total :: Int
cols = 60                  -- ★ マスの数
rows = 36
total = cols * rows

originX, originY :: Float
originX = 20
originY = 100

main :: IO ()
main = runSketch (sketch "08-14 Percolation" (World 1))
  { skBack  = hsb 220 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 各マスを確率 p で「通れる」にする
openCells :: Int -> Float -> UArray Int Bool
openCells seed p = listArray (0, total - 1) (map (< p) (randoms (mkStdGen seed)))

-- ★ 上の段から「通れるマス」をたどって染み込ませる（BFS：同じ歩数のマスをまとめて広げる）
wetCells :: UArray Int Bool -> UArray Int Bool
wetCells open = runSTUArray $ do
  wet <- newArray (0, total - 1) False
  let tops = [x | x <- [0 .. cols - 1], open ! x]
  forM_ tops (\x -> writeArray wet x True)
  spread open wet tops
  pure wet

spread :: UArray Int Bool -> STUArray s Int Bool -> [Int] -> ST s ()
spread _ _ [] = pure ()
spread open wet level = do
  next <- mapM soak level
  spread open wet (concat next)
  where
    soak i = do
      new <- filterM (\j -> not <$> readArray wet j) [j | j <- around i, open ! j]
      forM_ new (\j -> writeArray wet j True)
      pure new

around :: Int -> [Int]
around i =
  [ nx + ny * cols
  | (dx, dy) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
  , let nx = x + dx
  , let ny = y + dy
  , nx >= 0, ny >= 0, nx < cols, ny < rows ]
  where (y, x) = i `divMod` cols

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb 0 0 20) (Pictures [box i | i <- [0 .. total - 1], not (open ! i)])
  , Color (hsb 200 80 100) (Pictures [box i | i <- [0 .. total - 1], wet ! i])
  , Color (hsb 0 0 70) (Pictures [box i | i <- [0 .. total - 1], open ! i, not (wet ! i)])
  , txt env 20 34 18 (hsb 0 0 100)
      ("p = " ++ nf 3 p ++ " (mouse X)   " ++ (if percolates then "OK: water reached the bottom!" else "x: stopped on the way") ++ "   Click: new layout")
  , txt env 20 58 15 (hsb 0 0 60) "White = open, black = closed, blue = soaked from the top."
  , txt env 20 80 15 (hsb 0 0 60) "Threshold p = 0.593 (square lattice). Above it water nearly always gets through, below it nearly never."
  ]
  where
    p = clampF 0.3 0.8 (remap (fst (mouseXY env)) 0 (envW env) 0.3 0.8)
    open = openCells (wSeed w) p
    wet = wetCells open
    percolates = or [wet ! ((rows - 1) * cols + x) | x <- [0 .. cols - 1]]
    c = min ((envW env - 40) / fromIntegral cols) ((envH env - 140) / fromIntegral rows)
    box i =
      let (iy, ix) = i `divMod` cols
          (gx, gy) = toG env (originX + fromIntegral ix * c + (c - 1) / 2) (originY + fromIntegral iy * c + (c - 1) / 2)
      in rect gx gy (c - 1) (c - 1)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure w { wSeed = s }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. p を 0.55 → 0.60 → 0.65 と動かし、クリックを何回もして「通る確率」を体感する
-- 2. 格子を大きくすると（cols, rows）、境目はもっと鋭くなる（無限なら完全な階段）
-- 3. 森林火災（シリーズ3の 03）で木の密度がこれを超えると全焼する。同じ数学

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
