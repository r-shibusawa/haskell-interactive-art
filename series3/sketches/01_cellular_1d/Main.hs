-- ============================================
-- 01 1次元セル・オートマトン
-- 1列のマス（セル）が「自分と両どなり」だけを見て、次の世代の色を決める
-- 8通りのルールを 0/1 で書くと「ルール番号」になる（0〜255）
-- ←→ でルール変更 / 数字キー 1〜4 で有名なルール / クリックで最初の1列をランダムに
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, (.&.))
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

data World = World
  { wRule        :: !Int                -- ルール番号（有名：30 = カオス、90 = フラクタル、110 = 複雑、184 = 交通）
  , wCells       :: !(U.Vector Word8)   -- 今の世代（0 か 1）
  , wPaper       :: !(U.Vector Word8)   -- これまでに描いた行（画面の代わり）
  , wRow         :: !Int                -- いま描いている行
  , wRunning     :: !Bool               -- 画面の下まで来たら止まる
  , wRandomStart :: !Bool
  }

cellSize :: Int
cellSize = 4               -- ★ セルの大きさ

cols, rows :: Int
cols = 1280 `div` cellSize
rows = (800 - 50) `div` cellSize + 1

initialRule :: Int
initialRule = 30           -- ★ 最初のルール

main :: IO ()
main = runSketch (sketch "03-01 1D Cellular Automaton" start)
  { skBack  = white
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

start :: World
start = World initialRule centerCell (U.replicate (cols * rows) 0) 0 True False

centerCell :: U.Vector Word8
centerCell = U.generate cols (\i -> if i == cols `div` 2 then 1 else 0)   -- 真ん中に1つだけ

reset :: World -> IO World
reset w = do
  cells <- if wRandomStart w
    then U.replicateM cols ((\r -> if r < (0.5 :: Float) then 1 else 0) <$> randomRIO (0, 1))
    else pure centerCell
  pure w { wCells = cells, wPaper = U.replicate (cols * rows) 0, wRow = 0, wRunning = True }

stepWorld :: World -> World
stepWorld w
  | not (wRunning w) = w
  | otherwise =
      let row = wRow w
          -- 今の世代を1行描く
          paper = U.update (wPaper w) (U.imap (\i c -> (row * cols + i, c)) (wCells w))
          next = nextCells (wRule w) (wCells w)
          row' = row + 1
      in w { wPaper = paper, wCells = next, wRow = row'
           , wRunning = row' < rows }     -- 画面の下まで来たら止まる（←→ やクリックで再開）

-- 次の世代を計算する（端は反対側とつなぐ）
nextCells :: Int -> U.Vector Word8 -> U.Vector Word8
nextCells rule cells = U.generate cols $ \i ->
  applyRule rule (cells U.! ((i - 1 + cols) `mod` cols)) (cells U.! i) (cells U.! ((i + 1) `mod` cols))

-- ★ ルール：左・自分・右 の3つ（000〜111 = 0〜7）→ ルール番号のそのビットが 0 か 1 か
applyRule :: Int -> Word8 -> Word8 -> Word8 -> Word8
applyRule rule a b c =
  let index = fromIntegral a * 4 + fromIntegral b * 2 + fromIntegral c  -- 3つの 0/1 を 0〜7 の数にする
  in fromIntegral ((rule `shiftR` index) .&. 1)                         -- index 番目のビットを取り出す

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Translate 0 (envH env / 2 - 50 - fromIntegral (rows * cellSize) / 2) $
      pixelImage cols rows (fromIntegral cellSize) $ \x y ->
        let v = if wPaper w U.! (x + y * cols) == 1 then 20 else 255 in (v, v, v)
  -- 説明
  , Color white (rect 0 (envH env / 2 - 23) (envW env) 46)
  , hud env 20 15 black
      ("Rule " ++ show (wRule w) ++ "   binary " ++ binary (wRule w))
  , hud env 40 15 black "Left/Right: change rule   1-4: famous rules   Click: random first row on/off"
  ]

binary :: Int -> String
binary r = [if testBit r b then '1' else '0' | b <- [7, 6 .. 0]]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> reset w { wRule = (wRule w + 1) `mod` 256 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> reset w { wRule = (wRule w + 255) `mod` 256 }
  EventKey (SpecialKey _) Down _ _ -> reset w       -- p5.js と同じく、どのキーでも最初から
  EventKey (Char c) Down _ _ -> reset w { wRule = famous c (wRule w) }
  EventKey (MouseButton LeftButton) Down _ _ -> reset w { wRandomStart = not (wRandomStart w) }
  _ -> pure w

famous :: Char -> Int -> Int
famous c r = case c of
  '1' -> 30
  '2' -> 90
  '3' -> 110
  '4' -> 184
  _   -> r

-- ---------- やってみよう ----------
-- 1. ルール 90 → シェルピンスキーの三角形（シリーズ2！）が出てくる
-- 2. ルール 30 の真ん中の列を上から読むと、ほぼランダム。実は乱数に使われたことがある
-- 3. 256 個のルールを ←→ で全部見て、お気に入りを見つけよう

-- ---------- 道具 ----------

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
