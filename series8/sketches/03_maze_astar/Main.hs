-- ============================================
-- 03 迷路を解く（A* 探索）
-- マス目も「隣どうしがつながったグラフ」。壁を描いて、A* で左上から右下への道を探す
-- A* = ダイクストラ ＋「ゴールまでの見込み距離」のヒント。無駄な方向を調べない
-- ドラッグで壁を描く / Shift + ドラッグで壁を消す / スペースで探索 / C で消す / R でランダムな壁
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U

data World = World
  { wWall      :: !(U.Vector Bool)
  , wOpen      :: ![Int]                -- 候補（入れた順）
  , wClosed    :: !IS.IntSet            -- 調べたマス
  , wG         :: !(IM.IntMap Int)      -- ここまでの実コスト
  , wF         :: !(IM.IntMap Int)      -- 実コスト + 見込み
  , wCame      :: !(IM.IntMap Int)      -- どのマスから来たか
  , wPath      :: !IS.IntSet            -- 見つけた道
  , wPathLen   :: !Int
  , wSearching :: !Bool
  , wErase     :: !Bool                 -- Shift を押しながらドラッグを始めた
  }

cols, rows, total, goal :: Int
cols = 40                  -- ★ マスの数
rows = 24
total = cols * rows
goal = total - 1

wallRate :: Float
wallRate = 0.28            -- ★ ランダムな壁の割合

stepsPerFrame :: Int
stepsPerFrame = 3          -- ★ 1 フレームに進める回数

originX, originY :: Float
originX = 20
originY = 100

main :: IO ()
main = runSketch (sketch "08-03 Maze with A*" (cleared (U.replicate total False) False))
  { skBack  = hsb 220 40 10
  , skStart = \_ -> randomWalls False
  , skStep  = \_ _ w -> pure (if wSearching w then iterateN stepsPerFrame stepAStar w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

cleared :: U.Vector Bool -> Bool -> World
cleared wall erase = World wall [] IS.empty IM.empty IM.empty IM.empty IS.empty 0 False erase

reset :: World -> World
reset w = cleared (wWall w) (wErase w)

randomWalls :: Bool -> IO World
randomWalls erase = do
  rs <- replicateM total (randomRIO (0, 1 :: Float))
  let wall = U.fromList (map (< wallRate) rs) U.// [(0, False), (goal, False)]
  pure (cleared wall erase)

-- ★ ゴールまでのマンハッタン距離（見込み）
heuristic :: Int -> Int
heuristic i = abs (cols - 1 - x) + abs (rows - 1 - y)
  where (y, x) = i `divMod` cols

startSearch :: World -> World
startSearch w = (reset w)
  { wG = IM.singleton 0 0, wF = IM.singleton 0 (heuristic 0), wOpen = [0], wSearching = True }

-- ★ A* の 1 ステップ
stepAStar :: World -> World
stepAStar w = case wOpen w of
  [] -> w { wSearching = False }
  (o : os)
    | not (wSearching w) -> w
    | otherwise ->
        -- f = g（ここまでの実コスト）+ h（見込み）がいちばん小さい候補を選ぶ
        let fOf i = IM.findWithDefault inf i (wF w)
            cur = foldl' (\b k -> if fOf k < fOf b then k else b) o os
            w1 = w { wOpen = filter (/= cur) (wOpen w), wClosed = IS.insert cur (wClosed w) }
        in if cur == goal
             then let path = tracePath w1 cur
                  in w1 { wPath = IS.fromList path, wPathLen = length path, wSearching = False }
             else foldl' (relax cur) w1 (neighbours cur)

neighbours :: Int -> [Int]
neighbours i =
  [ nx + ny * cols
  | (dx, dy) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
  , let nx = x + dx
  , let ny = y + dy
  , nx >= 0, ny >= 0, nx < cols, ny < rows ]
  where (y, x) = i `divMod` cols

relax :: Int -> World -> Int -> World
relax cur w nb
  | wWall w U.! nb || IS.member nb (wClosed w) = w
  | g < IM.findWithDefault inf nb (wG w) =
      w { wG = IM.insert nb g (wG w)
        , wF = IM.insert nb (g + heuristic nb) (wF w)
        , wCame = IM.insert nb cur (wCame w)
        , wOpen = if nb `elem` wOpen w then wOpen w else wOpen w ++ [nb] }
  | otherwise = w
  where g = IM.findWithDefault inf cur (wG w) + 1

tracePath :: World -> Int -> [Int]
tracePath w v = case IM.lookup v (wCame w) of
  Just u -> v : tracePath w u
  Nothing -> [v]

inf :: Int
inf = maxBound `div` 2

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map cellPic [0 .. total - 1] ++ [startGoal 0 (hsb 120 90 100), startGoal goal (hsb 0 90 100)] ++ hud)
  where
    c = cellSize env
    openSet = IS.fromList (wOpen w)
    box i col =
      let (iy, ix) = i `divMod` cols
          (gx, gy) = toG env (originX + fromIntegral ix * c + (c - 1) / 2) (originY + fromIntegral iy * c + (c - 1) / 2)
      in Color col (rect gx gy (c - 1) (c - 1))
    cellPic i
      | wWall w U.! i = box i (hsb 0 0 70)
      | IS.member i (wPath w) = box i (hsb 45 90 100)
      | IS.member i (wClosed w) = box i (hsb 200 60 45)
      | IS.member i openSet = box i (hsb 120 70 70)
      | otherwise = box i (hsb 220 30 15)
    startGoal = box
    status
      | wPathLen w > 0 = "Path length " ++ show (wPathLen w - 1) ++ "   Cells searched " ++ show (IS.size (wClosed w))
      | wSearching w = "Searching..."
      | null (wOpen w) && not (IS.null (wClosed w)) = "No path"
      | otherwise = ""
    hud =
      [ txt env 20 28 18 (hsb 0 0 100) ("Drag: wall (Shift: erase)   Space: search   C: clear   R: random   " ++ status)
      , txt env 20 52 15 (hsb 0 0 60) "Blue = searched, green = candidates, yellow = the path."
      , txt env 20 74 15 (hsb 0 0 60) "The estimated distance is a hint, so cells toward the goal are searched first."
      ]

cellSize :: Env -> Float
cellSize env = min ((envW env - 40) / fromIntegral cols) ((envH env - 120) / fromIntegral rows)

-- マウスの位置のマスに壁を描く（消す）
paint :: Env -> World -> World
paint env w
  | my <= 90 = w
  | x < 0 || y < 0 || x >= cols || y >= rows = w
  | i == 0 || i == goal = w
  | otherwise = reset w { wWall = wWall w U.// [(i, not erasing)] }
  where
    (mx, my) = mouseXY env
    c = cellSize env
    x = floor ((mx - originX) / c) :: Int
    y = floor ((my - originY) / c) :: Int
    i = x + y * cols
    erasing = wErase w || isDown env (SpecialKey KeyShiftL) || isDown env (SpecialKey KeyShiftR)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down m _ -> pure (paint env w { wErase = shift m == Down })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wErase = False }
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (startSearch w)
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure (reset w { wWall = U.replicate total False })
    | c `elem` "rR" -> randomWalls (wErase w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. heuristic を「heuristic _ = 0」にするとダイクストラ（02）と同じ。調べたマスの数を比べよう
-- 2. 斜め移動を許す（(1, 1) などを足す）と道はどう変わる？
-- 3. 壁で完全に囲むと「No path」。調べたマスは全部青になる

-- ---------- 道具 ----------

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x = foldl' (\v _ -> f v) x [1 .. n]

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
