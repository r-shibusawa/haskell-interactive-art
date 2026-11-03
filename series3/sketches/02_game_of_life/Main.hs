-- ============================================
-- 02 ライフゲーム（Conway's Game of Life）
-- 2次元のマス目。各セルは「生きている(1)」か「死んでいる(0)」
-- ルールはたった3つ。でも、動く・増える・くり返す「生き物」が現れる
-- ドラッグでセルを描く / スペース = 再生・停止 / R = ランダム / C = 全部消す
-- G = グライダー銃 / N = 1世代だけ進める
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

data World = World
  { wGrid    :: !(U.Vector Word8)   -- x + y * cols 番目のセル（0 か 1）
  , wAge     :: !(U.Vector Int)     -- 何世代生きているか（色に使う）
  , wRunning :: !Bool
  , wGen     :: !Int
  }

cellSize :: Int
cellSize = 10              -- ★ セルの大きさ

cols, rows :: Int
cols = 1280 `div` cellSize
rows = (800 - 50) `div` cellSize

everyFrames :: Int
everyFrames = 4            -- ★ 何フレームごとに世代を進めるか

main :: IO ()
main = runSketch (sketch "03-02 Game of Life" (World emptyGrid (U.replicate (cols * rows) 0) False 0))
  { skBack  = hsb 0 0 8
  , skStart = \_ -> do
      g <- randomGrid
      pure (World g (U.replicate (cols * rows) 0) False 0)
  , skStep  = \env _ w -> pure (if wRunning w && envFrame env `mod` everyFrames == 0 then nextGeneration w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

emptyGrid :: U.Vector Word8
emptyGrid = U.replicate (cols * rows) 0

randomGrid :: IO (U.Vector Word8)
randomGrid = U.replicateM (cols * rows) ((\r -> if r < (0.25 :: Float) then 1 else 0) <$> randomRIO (0, 1))

-- ★ ライフゲームのルール（これだけ！）
nextGeneration :: World -> World
nextGeneration w =
  let grid = wGrid w
      age = wAge w
      neighbors i = countNeighbors grid (i `mod` cols) (i `div` cols)
      -- 生存：となりが 2 か 3 なら生き続ける / 誕生：となりがちょうど 3 なら生まれる / それ以外は死ぬ
      rule :: Int -> (Word8, Int)
      rule i =
        let n = neighbors i
            alive = U.unsafeIndex grid i == 1
            old = U.unsafeIndex age i
        in if alive && (n == 2 || n == 3) then (1, old + 1)
           else if not alive && n == 3 then (1, 0)
           else (0, old)
      (grid', age') = U.unzip (U.generate (cols * rows) rule)
  in w { wGrid = grid', wAge = age', wGen = wGen w + 1 }

-- まわり8マスの生きているセルの数（端は反対側とつながっている）
countNeighbors :: U.Vector Word8 -> Int -> Int -> Int
countNeighbors grid x y = sum
  [ fromIntegral (U.unsafeIndex grid (((x + i + cols) `mod` cols) + ((y + j + rows) `mod` rows) * cols))
  | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0 ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env (Pictures (U.ifoldr cell [] (wGrid w)))
  , hud env 20 15 white ((if wRunning w then "PLAYING" else "PAUSED") ++ "   Generation " ++ show (wGen w))
  , hud env 40 15 white "Space: play/pause   Drag: draw   R: random   C: clear   G: glider gun   N: one step"
  ]
  where
    size = fromIntegral cellSize
    cell i v rest
      | v /= 1 = rest
      | otherwise =
          let x = fromIntegral (i `mod` cols) * size
              y = 50 + fromIntegral (i `div` cols) * size
              a = fromIntegral (min (U.unsafeIndex (wAge w) i) 30)
              -- 若いセルは黄色、長生きのセルは青に
          in Color (hsb (remap a 0 30 50 220) 70 100) (box x y (size - 1) (size - 1)) : rest

paint :: Env -> World -> World
paint env w =
  let (mx, my) = mouseP5 env
      x = floor (mx / fromIntegral cellSize) :: Int
      y = floor ((my - 50) / fromIntegral cellSize) :: Int
      i = x + y * cols
  in if x >= 0 && x < cols && y >= 0 && y < rows
       then w { wGrid = wGrid w U.// [(i, 1)], wAge = wAge w U.// [(i, 0)] }
       else w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paint env w)
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> do
        g <- randomGrid
        pure w { wGrid = g, wGen = 0 }
    | c `elem` "cC" -> pure w { wGrid = emptyGrid, wGen = 0 }
    | c `elem` "gG" -> pure w { wGrid = wGrid w U.// gliderGun 5 5 }
    | c `elem` "nN" -> pure (nextGeneration w)        -- 1世代だけ進める
  _ -> pure w

-- ゴスパーのグライダー銃（グライダーを次々と発射する有名なパターン）
gliderGun :: Int -> Int -> [(Int, Word8)]
gliderGun ox oy =
  [ ((ox + i) + (oy + j) * cols, 1)
  | (j, line') <- zip [0 ..] pattern'
  , (i, ch) <- zip [0 ..] line'
  , ch == 'O', ox + i < cols, oy + j < rows ]

pattern' :: [String]
pattern' =
  [ "........................O..........."
  , "......................O.O..........."
  , "............OO......OO............OO"
  , "...........O...O....OO............OO"
  , "OO........O.....O...OO.............."
  , "OO........O...O.OO....O.O..........."
  , "..........O.....O.......O..........."
  , "...........O...O...................."
  , "............OO......................"
  ]

-- ---------- やってみよう ----------
-- 1. C で消して、横に3つ並べて描く → スペース。「ブリンカー」が点滅する
-- 2. 斜めに階段状に5つ描く（グライダー）→ 動いていく！
-- 3. ルールの数字（2, 3, 3）を変えると、まったく別の世界になる（→ 12 で本格的に）

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
