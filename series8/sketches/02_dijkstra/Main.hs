-- ============================================
-- 02 最短経路（ダイクストラ法）
-- 線に「長さ（コスト）」があるとき、いちばん安い道を見つける
-- 「確定した点から、いちばん近い未確定の点を確定する」をくり返すだけ。カーナビや乗換案内の中身
-- クリック = 出発点（1 回目）→ 目的地（2 回目） / R で新しい地図
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import qualified Data.Vector as V

data World = World
  { wNodes   :: !(V.Vector Point)
  , wEdges   :: ![(Int, Int, Int)]            -- (a, b, コスト)
  , wAdj     :: !(V.Vector [(Int, Int)])      -- 点 → [(行き先, コスト)]
  , wStart   :: !Int                          -- 出発点（-1 = まだ）
  , wGoal    :: !Int                          -- 目的地（-1 = まだ）
  , wDist    :: !(IM.IntMap Int)              -- 出発点からの最小コスト（ない = 無限大）
  , wPrev    :: !(IM.IntMap Int)              -- どの点から来たか
  , wDone    :: !IS.IntSet                    -- 確定した点
  , wPath    :: ![Int]
  , wRunning :: !Bool
  , wTimer   :: !Float
  }

nodeCount, nearCount :: Int
nodeCount = 22             -- ★ 点の数
nearCount = 3              -- ★ 各点から近い何点とつなぐか

stepTime :: Float
stepTime = 0.2             -- ★ 1 ステップの時間（秒）

main :: IO ()
main = runSketch (sketch "08-02 Dijkstra" (World V.empty [] V.empty (-1) (-1) IM.empty IM.empty IS.empty [] False 0))
  { skBack  = hsb 220 40 10
  , skStart = makeGraph
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 近い点どうしをつなぐ（各点から近い 3 点）
makeGraph :: Env -> IO World
makeGraph env = do
  pts <- replicateM nodeCount $ do
    x <- randomRIO (100, envW env - 100)
    y <- randomRIO (130, envH env - 60)
    pure (x, y)
  let nodes = V.fromList pts
      (edges, adj) = foldl' (connect nodes) ([], V.replicate nodeCount []) [0 .. nodeCount - 1]
  pure (World nodes (reverse edges) adj (-1) (-1) IM.empty IM.empty IS.empty [] False 0)

connect :: V.Vector Point -> ([(Int, Int, Int)], V.Vector [(Int, Int)]) -> Int -> ([(Int, Int, Int)], V.Vector [(Int, Int)])
connect nodes acc i = foldl' link acc others
  where
    pi' = nodes V.! i
    others = take nearCount (sortOn snd [(j, dist2 pi' (nodes V.! j)) | j <- [0 .. nodeCount - 1], j /= i])
    link (es, adj) (j, d)
      | any ((== j) . fst) (adj V.! i) = (es, adj)
      | otherwise =
          let c = roundJs (d / 20)             -- ★ 長さ = コスト
          in ((i, j, c) : es, V.accum (\l x -> l ++ [x]) adj [(i, (j, c)), (j, (i, c))])

-- ★ ダイクストラの 1 ステップ
stepDijkstra :: World -> World
stepDijkstra w = case candidates of
  [] -> w { wRunning = False }
  (c : cs) ->
    let best = foldl' (\b i -> if distOf w i < distOf w b then i else b) c cs     -- いちばん近い未確定の点
        done = IS.insert best (wDone w)                                           -- を確定
    in if best == wGoal w
         then w { wDone = done, wRunning = False, wPath = tracePath w best }
         else foldl' (relax best) w { wDone = done } (wAdj w V.! best)
  where
    candidates = [i | i <- IM.keys (wDist w), not (IS.member i (wDone w))]

-- より安く行けるなら更新
relax :: Int -> World -> (Int, Int) -> World
relax best w (to, cost)
  | distOf w best + cost < distOf w to =
      w { wDist = IM.insert to (distOf w best + cost) (wDist w), wPrev = IM.insert to best (wPrev w) }
  | otherwise = w

-- 来た道をたどる
tracePath :: World -> Int -> [Int]
tracePath w = go []
  where go acc v = case IM.lookup v (wPrev w) of
          Just u -> go (v : acc) u
          Nothing -> v : acc

distOf :: World -> Int -> Int
distOf w i = IM.findWithDefault infinity i (wDist w)

infinity :: Int
infinity = maxBound `div` 2

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRunning w && envTime env - wTimer w > stepTime = pure (stepDijkstra w) { wTimer = envTime env }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap edgePic (wEdges w) ++ concatMap nodePic [0 .. V.length (wNodes w) - 1] ++ hud)
  where
    pos i = let (x, y) = wNodes w V.! i in toG env x y
    pathSet = Set.fromList (concat (zipWith (\a b -> [(a, b), (b, a)]) (wPath w) (drop 1 (wPath w))))
    edgePic (a, b, c) =
      let onPath = Set.member (a, b) pathSet
          (ax, ay) = pos a
          (bx, by) = pos b
      in [ if onPath then Color (hsb 45 90 100) (thickLine 5 (ax, ay) (bx, by))
                     else Color (hsb 0 0 35) (thickLine 1.5 (ax, ay) (bx, by))
         , label ((ax + bx) / 2 + 4) ((ay + by) / 2 + 4) 14 (hsb 0 0 55) (show c) ]
    nodePic i =
      let isEnd = i == wStart w || i == wGoal w
          lit = isEnd || IS.member i (wDone w)
          hue | i == wStart w = 120
              | i == wGoal w = 0
              | IS.member i (wDone w) = 200
              | otherwise = 220
          (gx, gy) = pos i
      in Color (hsb hue (if lit then 80 else 30) (if lit then 100 else 40)) (at (gx, gy) (disc (if isEnd then 18 else 13)))
         : [labelC gx gy 14 (hsb 0 0 10) (show d) | Just d <- [IM.lookup i (wDist w)]]
    msg | wStart w < 0 = "Click the start node"
        | wGoal w < 0 = "Click the goal node"
        | wRunning w = "Searching..."
        | not (null (wPath w)) = "Shortest path: cost " ++ show (distOf w (wGoal w)) ++ ", through " ++ show (length (wPath w)) ++ " nodes"
        | otherwise = "Goal cannot be reached"
    hud =
      [ txt env 20 40 18 (hsb 0 0 100) (msg ++ "   R: new map")
      , txt env 20 64 15 (hsb 0 0 60) "Edge number = cost. Node number = lowest cost from the start (light blue once fixed)."
      , txt env 20 86 15 (hsb 0 0 60) "From the fixed nodes, the nearest unfixed node is fixed next, again and again."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        hits = [i | (i, p) <- zip [0 ..] (V.toList (wNodes w)), dist2 (mx, my) p < 20]
    in pure (foldl' clickNode w hits)
  EventKey (Char c) Down _ _ | c `elem` "rR" -> makeGraph env
  _ -> pure w

clickNode :: World -> Int -> World
clickNode w i
  | wStart w < 0 || wGoal w >= 0 =
      w { wStart = i, wGoal = -1, wPath = [], wDist = IM.empty, wPrev = IM.empty, wDone = IS.empty, wRunning = False }
  | i /= wStart w =
      w { wGoal = i, wDist = IM.singleton (wStart w) 0, wPrev = IM.empty, wDone = IS.empty, wPath = [], wRunning = True }
  | otherwise = w

-- ---------- やってみよう ----------
-- 1. 「線の本数がいちばん少ない道」と「コストがいちばん小さい道」はちがう。例を探そう
-- 2. コストを「線の長さ」ではなく全部 1 にすると、BFS（01）と同じ結果になる
-- 3. 目的地までの直線距離をヒントに使うと速くなる（A*、→ 03）

-- ---------- 道具 ----------

-- JavaScript の Math.round（0.5 は切り上げ。Haskell の round とは少しちがう）
roundJs :: Float -> Int
roundJs x = floor (x + 0.5)

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
