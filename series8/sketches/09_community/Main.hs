-- ============================================
-- 09 コミュニティ検出（仲良しグループを見つける）
-- 「自分の隣で多数派のラベルに合わせる」を全員がくり返すと、密につながった仲間が同じ色になる
-- （ラベル伝播法）。SNS のグループ分け、タンパク質の機能分類などに使われる
-- スペースで 1 ステップ / R で新しいネットワーク / 自動で力学レイアウト
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Node = Node { nx, ny, nvx, nvy :: !Float }

data World = World
  { wNodes :: !(V.Vector Node)
  , wEdges :: ![(Int, Int)]
  , wAdj   :: !(V.Vector [Int])
  , wLabel :: !(U.Vector Int)
  , wSteps :: !Int
  , wAuto  :: !Bool
  , wTimer :: !Float
  }

n :: Int
n = 90                     -- ★ 点の数（30 人ずつ 3 グループ）

pSame, pOther :: Float
pSame = 0.18               -- ★ 同じグループ内でつながる確率
pOther = 0.008             -- ★ グループ間でつながる確率

main :: IO ()
main = runSketch (sketch "08-09 Community Detection" (World V.empty [] V.empty U.empty 0 True 0))
  { skBack  = hsb 220 40 10
  , skStart = build
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 3 つのグループが「中は密、外は疎」につながったネットワーク
build :: Env -> IO World
build env = do
  ns <- replicateM n $ do
    x <- randomRIO (0, envW env)
    y <- randomRIO (100, envH env)
    pure (Node x y 0 0)
  let pairs = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
  rs <- mapM (const (randomRIO (0, 1 :: Float))) pairs
  let same i j = i `div` 30 == j `div` 30
      edges = [(i, j) | ((i, j), r) <- zip pairs rs, r < (if same i j then pSame else pOther)]
      adj = V.accum (flip (:)) (V.replicate n []) (concat [[(a, b), (b, a)] | (a, b) <- edges])
  pure (World (V.fromList ns) edges adj (U.enumFromN 0 n) 0 True 0)  -- ★ 最初は全員ちがうラベル

-- ★ ラベル伝播の 1 ステップ：隣でいちばん多いラベルに乗りかえる
propagate :: World -> IO World
propagate w = do
  keys <- replicateM n (randomRIO (0, 1 :: Float))
  let order = map snd (sortOn fst (zip keys [0 .. n - 1]))
      update (labels, changed) i = case majority labels (wAdj w V.! i) of
        Just best | best /= labels U.! i -> (labels U.// [(i, best)], changed + 1 :: Int)
        _ -> (labels, changed)
      (labels1, changed1) = foldl' update (wLabel w, 0) order
  pure w { wLabel = labels1, wSteps = wSteps w + 1, wAuto = wAuto w && changed1 /= 0 }

-- 隣でいちばん多いラベル（同数なら番号の小さいほう）
majority :: U.Vector Int -> [Int] -> Maybe Int
majority labels nbs =
  case IM.toAscList (IM.fromListWith (+) [(labels U.! nb, 1 :: Int) | nb <- nbs]) of
    [] -> Nothing
    (x : xs) -> Just (fst (foldl' (\a b -> if snd a >= snd b then a else b) x xs))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  w1 <- if wAuto w && envTime env - wTimer w > 0.7 && envFrame env > 60
          then (\v -> v { wTimer = envTime env }) <$> propagate w
          else pure w
  pure w1 { wNodes = forces env (wEdges w1) (wNodes w1) }

-- 力学レイアウト：中心への力 ＋ 反発 ＋ バネ
forces :: Env -> [(Int, Int)] -> V.Vector Node -> V.Vector Node
forces env edges ns = V.map move (V.accum addV (V.map repel ns) (concatMap spring edges))
  where
    w = envW env
    h = envH env
    repel a =
      let F2 fx fy = V.foldl' (push a) (F2 ((w / 2 - nx a) * 0.002) ((h / 2 + 40 - ny a) * 0.002)) ns
      in a { nvx = (nvx a + fx) * 0.5, nvy = (nvy a + fy) * 0.5 }
    push a (F2 sx sy) b =
      let dx = nx a - nx b
          dy = ny a - ny b
          d2 = max (dx * dx + dy * dy) 100
      in F2 (sx + dx / d2 * 600) (sy + dy / d2 * 600)
    spring (a, b) =
      let na = ns V.! a
          nb = ns V.! b
          dx = nx nb - nx na
          dy = ny nb - ny na
          d = max (sqrt (dx * dx + dy * dy)) 1
          f = (d - 50) * 0.01
          ux = dx / d * f
          uy = dy / d * f
      in [(a, (ux, uy)), (b, (-ux, -uy))]
    addV nd (dx, dy) = nd { nvx = nvx nd + dx, nvy = nvy nd + dy }
    move nd = nd { nx = clampF 20 (w - 20) (nx nd + nvx nd), ny = clampF 90 (h - 20) (ny nd + nvy nd) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic (wEdges w) ++ nodePics ++ hud)
  where
    labels = wLabel w
    pos i = let nd = wNodes w V.! i in toG env (nx nd) (ny nd)
    hueOf l = fromIntegral ((l * 37) `mod` 360) :: Float
    edgePic (a, b)
      | labels U.! a == labels U.! b = Color (hsba (hueOf (labels U.! a)) 50 70 0.6) (Line [pos a, pos b])
      | otherwise = Color (hsba 0 0 35 0.6) (Line [pos a, pos b])
    nodePics = [Color (hsb (hueOf l) 70 100) (at (pos i) (disc 7)) | (i, l) <- zip [0 .. V.length (wNodes w) - 1] (U.toList labels)]
    distinct = IS.size (IS.fromList (U.toList labels))
    hud =
      [ txt env 20 28 18 (hsb 0 0 100)
          ("Step " ++ show (wSteps w) ++ "   Labels " ++ show distinct ++ "   " ++ (if wAuto w then "Running..." else "Converged (no more changes)"))
      , txt env 20 50 15 (hsb 0 0 100) "Space: one step   R: new network"
      , txt env 20 72 15 (hsb 0 0 60) "All colors differ at first. Just following the neighbors' majority merges dense groups into one color."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> propagate w
  EventKey (Char c) Down _ _ | c `elem` "rR" -> build env
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 収束したとき、色の数はいくつ？ 作ったグループ（3 つ）と一致した？
-- 2. グループ間のつながり確率 pOther を 0.05 にすると、分けにくくなる
-- 3. 同じネットワークでも、順番のランダムさで結果が少し変わる（R で何度も）

-- ---------- 道具 ----------

-- 力を足し合わせるための組（正格なので速い）
data F2 = F2 !Float !Float

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
