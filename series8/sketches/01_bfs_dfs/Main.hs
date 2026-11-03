-- ============================================
-- 01 グラフをたどる（幅優先探索と深さ優先探索）
-- BFS：近い点から順に、波のように広がる（最短経路が見つかる）
-- DFS：一本道をどんどん奥へ、行き止まりで戻る（迷路の探索）
-- クリックした点から探索開始 / 1 = BFS / 2 = DFS / R で新しいグラフ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IM
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

data World = World
  { wNodes    :: !(V.Vector Point)
  , wEdges    :: ![(Int, Int)]
  , wAdj      :: !(V.Vector [Int])
  , wMode     :: !Int                  -- 1 = BFS、2 = DFS
  , wCount    :: !Int                  -- 訪れた点の数
  , wFrontier :: !(Seq Int)            -- 次に見る候補（BFS：キュー、DFS：スタック）
  , wVisited  :: !(IM.IntMap Int)      -- 点の番号 → 何番目に訪れたか
  , wTimer    :: !Float
  }

stepTime :: Float
stepTime = 0.15            -- ★ 1 ステップの時間（秒）

cols, rows :: Int
cols = 8                   -- ★ 格子の大きさ
rows = 5

main :: IO ()
main = runSketch (sketch "08-01 BFS and DFS" (World V.empty [] V.empty 1 0 Seq.empty IM.empty 0))
  { skBack  = hsb 220 40 10
  , skStart = \env -> makeGraph env 1
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 格子状にゆがんだグラフをつくる
makeGraph :: Env -> Int -> IO World
makeGraph env mode = do
  nodes <- mapM place [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  edges <- foldM addEdges [] [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
  let es = reverse edges
      n = cols * rows
      adj = V.accum (\l x -> l ++ [x]) (V.replicate n []) (concat [[(a, b), (b, a)] | (a, b) <- es])
  pure (World (V.fromList nodes) es adj mode 0 Seq.empty IM.empty 0)
  where
    place (r, c) = do
      dx <- randomRIO (-25, 25)
      dy <- randomRIO (-25, 25)
      pure ( remap (fromIntegral c) 0 (fromIntegral cols - 1) 120 (envW env - 120) + dx
           , remap (fromIntegral r) 0 (fromIntegral rows - 1) 150 (envH env - 80) + dy )
    addEdges acc (r, c) = do
      let i = r * cols + c
      a <- chance 0.85
      b <- chance 0.85
      let acc1 = if c < cols - 1 && a then (i, i + 1) : acc else acc
      pure (if r < rows - 1 && b then (i, i + cols) : acc1 else acc1)

chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- ★ 1 ステップ進める。BFS は先頭から、DFS は末尾から取り出す（違いはこの 1 行だけ）
popNext :: Int -> Seq Int -> Maybe (Int, Seq Int)
popNext 1 q = case viewl q of
  x :< rest -> Just (x, rest)
  EmptyL -> Nothing
popNext _ q = case viewr q of
  rest :> x -> Just (x, rest)
  EmptyR -> Nothing

visit :: World -> World
visit w = case popNext (wMode w) (wFrontier w) of
  Nothing -> w
  Just (cur, rest)
    | IM.member cur (wVisited w) -> w { wFrontier = rest }
    | otherwise ->
        let cnt = wCount w + 1
            visited = IM.insert cur cnt (wVisited w)
            new = [nb | nb <- wAdj w V.! cur, not (IM.member nb visited)]
        in w { wFrontier = foldl (|>) rest new, wVisited = visited, wCount = cnt }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | not (Seq.null (wFrontier w)) && envTime env - wTimer w > stepTime = pure (visit w) { wTimer = envTime env }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic (wEdges w) ++ concatMap nodePic [0 .. n - 1] ++ hud)
  where
    n = V.length (wNodes w)
    pos i = let (x, y) = wNodes w V.! i in toG env x y
    seen i = IM.member i (wVisited w)
    inFrontier = IM.fromList [(i, ()) | i <- toList (wFrontier w)]
    edgePic (a, b)
      | seen a && seen b = Color (hsb 0 0 80) (thickLine 3 (pos a) (pos b))
      | otherwise = Color (hsb 0 0 30) (thickLine 1.5 (pos a) (pos b))
    nodePic i = case IM.lookup i (wVisited w) of
      Just v ->
        let (gx, gy) = pos i
        in [ Color (hsb (remap (fromIntegral v) 1 (fromIntegral n) 180 330) 80 100) (at (gx, gy) (disc 17))
           , labelC gx gy 14 (hsb 0 0 10) (show v) ]
      Nothing
        | IM.member i inFrontier -> [Color (hsb 45 80 100) (at (pos i) (disc 13))]
        | otherwise -> [Color (hsb 220 30 35) (at (pos i) (disc 13))]
    title | wMode w == 1 = "BFS (queue: first in, first out)"
          | otherwise = "DFS (stack: last in, first out)"
    hud =
      [ txt env 20 34 18 (hsb 0 0 100) (title ++ "   Visited " ++ show (wCount w) ++ " / " ++ show n)
      , txt env 20 58 15 (hsb 0 0 100) "1 / 2: switch   Click a node: start   R: new graph"
      , txt env 20 80 15 (hsb 0 0 60) "Number = visit order. Yellow = next candidates. BFS spreads in rings, DFS dives deep along one path."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        hits = [i | (i, p) <- zip [0 ..] (V.toList (wNodes w)), dist2 (mx, my) p < 20]
    in pure (case reverse hits of
               (i : _) -> w { wCount = 0, wVisited = IM.empty, wFrontier = Seq.singleton i }
               [] -> w)
  EventKey (Char c) Down _ _
    | c `elem` "12" -> pure w { wMode = if c == '1' then 1 else 2, wCount = 0, wVisited = IM.empty, wFrontier = Seq.empty }
    | c `elem` "rR" -> makeGraph env (wMode w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ点から BFS と DFS を始めて、訪れる順番の違いを見る
-- 2. グラフが 2 つに分かれているとき（R で作り直すと時々そうなる）、片方からは全部訪れない
-- 3. BFS で「何歩目か」を記録すれば最短経路になる。wVisited に歩数を入れてみよう

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
