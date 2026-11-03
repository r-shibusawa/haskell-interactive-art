-- ============================================
-- 12 自分の生命ルールをつくる（自由制作のベース）
-- ライフゲームのルールは B3/S23（となりが3で誕生、2か3で生存）。
-- この数字を変えると、まったく別の「宇宙」が生まれる。セルが生まれると音が鳴る
-- ドラッグで描く / スペース = 再生・停止 / R = ランダム / C = 消す / 1〜5 で有名なルール
-- ============================================
-- ★ のついたところを変えて、自分だけの宇宙をつくろう！
module Main (main) where

import Art
import Control.Monad (replicateM_, when)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

data World = World
  { wGrid    :: !(U.Vector Word8)   -- x + y * cols 番目のセル（0 か 1）
  , wAge     :: !(U.Vector Int)
  , wBirth   :: ![Int]              -- となりが何個のとき生まれるか
  , wSurvive :: ![Int]              -- となりが何個のとき生き残るか
  , wName    :: !String
  , wRunning :: !Bool
  , wAudioOn :: !Bool               -- クリックするまで音は鳴らさない
  }

cellSize :: Int
cellSize = 12              -- ★ セルの大きさ

cols, rows :: Int
cols = 1280 `div` cellSize
rows = (800 - 50) `div` cellSize

everyFrames :: Int
everyFrames = 6            -- ★ 6 を小さくすると速い

-- ★ 誕生の音
notes :: [String]
notes = ["C3", "E3", "G3", "B3", "D4", "F#4", "A4", "C5"]

-- ★ 有名なルール（キー, 誕生, 生存, 名前）
presets :: [(Char, [Int], [Int], String)]
presets =
  [ ('1', [3], [2, 3], "B3/S23 Game of Life")
  , ('2', [3, 6], [2, 3], "B36/S23 HighLife (self-replicating)")
  , ('3', [3], [1, 2, 3, 4, 5], "B3/S12345 Maze")
  , ('4', [3, 6, 7, 8], [3, 4, 6, 7, 8], "B3678/S34678 Day and Night")
  , ('5', [2], [], "B2/S Seeds (explosive)")
  ]

emptyGrid :: U.Vector Word8
emptyGrid = U.replicate (cols * rows) 0

start :: World
start = World emptyGrid (U.replicate (cols * rows) 0) [3] [2, 3] "B3/S23 Game of Life" True False

main :: IO ()
main = runSketch (sketch "03-12 My Life Rules" start)
  { skBack  = hsb 0 0 6
  , skStart = \_ -> (\g -> start { wGrid = g }) <$> randomGrid 0.15
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randomGrid :: Float -> IO (U.Vector Word8)
randomGrid p = U.replicateM (cols * rows) ((\r -> if r < p then 1 else 0) <$> randomRIO (0, 1))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRunning w && envFrame env `mod` everyFrames == 0 = do
      let (w', births) = nextGeneration w
      -- 生まれた数で音を鳴らす（多すぎると鳴らしすぎるので上限）
      when (wAudioOn w && births > 0) $ replicateM_ (min births 4) $ do
        i <- randomRIO (0, length notes - 1)
        mapM_ (\n -> playNote n 0.08 0.3 Sine) (take 1 (drop i notes))
      pure w'
  | otherwise = pure w

nextGeneration :: World -> (World, Int)
nextGeneration w =
  let grid = wGrid w
      age = wAge w
      rule :: Int -> (Word8, Int)
      rule i =
        let n = countNeighbors grid (i `mod` cols) (i `div` cols)
            alive = U.unsafeIndex grid i == 1
            old = U.unsafeIndex age i
        in if alive && n `elem` wSurvive w then (1, old + 1)
           else if not alive && n `elem` wBirth w then (1, 0)
           else (0, old)
      (grid', age') = U.unzip (U.generate (cols * rows) rule)
      births = U.length (U.filter id (U.zipWith (\a b -> a == 0 && b == 1) grid grid'))
  in (w { wGrid = grid', wAge = age' }, births)

-- まわり8マスの生きているセルの数（端は反対側とつながっている）
countNeighbors :: U.Vector Word8 -> Int -> Int -> Int
countNeighbors grid x y = sum
  [ fromIntegral (U.unsafeIndex grid (((x + i + cols) `mod` cols) + ((y + j + rows) `mod` rows) * cols))
  | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0 ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env (Pictures (U.ifoldr cell [] (wGrid w)))
  , hud env 20 15 white (wName w ++ "   " ++ (if wRunning w then "PLAYING" else "PAUSED"))
  , hud env 40 15 white "Space: play/pause   Drag: draw   R: random   C: clear   1-5: rules   Click: sound ON"
  ]
  where
    size = fromIntegral cellSize
    cell i v rest
      | v /= 1 = rest
      | otherwise =
          let x = fromIntegral (i `mod` cols) * size
              y = 50 + fromIntegral (i `div` cols) * size
              -- ★ 色：年齢で変わる
              hue = fromIntegral ((U.unsafeIndex (wAge w) i * 8 + 180) `mod` 360)
          -- ★ 図形
          in Color (hsb hue 60 100) (roundBox x y (size - 1) (size - 1) 3) : rest

paint :: Env -> World -> World
paint env w =
  let (mx, my) = mouseP5 env
      x = floor (mx / fromIntegral cellSize) :: Int
      y = floor ((my - 50) / fromIntegral cellSize) :: Int
  in if x >= 0 && x < cols && y >= 0 && y < rows
       then w { wGrid = wGrid w U.// [(x + y * cols, 1)] }
       else w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paint env w { wAudioOn = True })
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> (\g -> w { wGrid = g }) <$> randomGrid 0.15
    | c `elem` "cC" -> pure w { wGrid = emptyGrid }
    | otherwise ->
        pure (foldr (\(k, b, s, name) acc -> if k == c then w { wBirth = b, wSurvive = s, wName = name } else acc) w presets)
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・wBirth と wSurvive を自分で決めて、面白い宇宙を探す（B3/S23 以外に 2^18 通りある）
-- ・セルを roundBox から disc や文字にする。年齢で大きさを変える
-- ・11 のマイクと組み合わせて、声でセルをランダムに生み出す
-- ・10 のように、再生バーで音を鳴らす

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 角を r だけ落とした長方形（p5.js の rect の角丸の代わり）
roundBox :: Float -> Float -> Float -> Float -> Float -> Picture
roundBox x y w h r = Polygon
  [ (x + r, y), (x + w - r, y), (x + w, y + r), (x + w, y + h - r)
  , (x + w - r, y + h), (x + r, y + h), (x, y + h - r), (x, y + r) ]

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
