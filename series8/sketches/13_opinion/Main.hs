-- ============================================
-- 13 意見はどう広がるか（投票者モデル）
-- 各点は 2 つの意見（青／橙）のどちらか。毎回ランダムな点が「隣の 1 人の意見をまねる」だけ
-- ネットワークの形によって、まとまる・分かれる・ずっと揺れる
-- 1 = 格子 / 2 = ランダム / 3 = 2 つのグループ / クリックで点の意見を反転 / R でやり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wNodes    :: !(V.Vector Point)
  , wAdj      :: !(V.Vector [Int])
  , wEdges    :: ![(Int, Int)]
  , wOpinion  :: !(U.Vector Int)    -- 0 = 青、1 = 橙
  , wTopology :: !Int
  , wHistory  :: !(Seq Float)       -- 橙の割合の推移
  }

n :: Int
n = 150                    -- ★ 点の数

copiesPerFrame :: Int
copiesPerFrame = 20        -- ★ 1 フレームにまねる回数

historyLen :: Int
historyLen = 300

main :: IO ()
main = runSketch (sketch "08-13 Opinion Dynamics" (World V.empty V.empty [] (U.replicate n 0) 1 Seq.empty))
  { skBack  = hsb 220 40 10
  , skStart = \env -> build env 1
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 線を足す（同じ線・自分への線は足さない）
data G = G !(IM.IntMap IS.IntSet) ![(Int, Int)]

link :: G -> (Int, Int) -> G
link g@(G adj es) (a, b)
  | a == b || IS.member b (IM.findWithDefault IS.empty a adj) = g
  | otherwise = G (add a b (add b a adj)) ((a, b) : es)
  where add x y = IM.insertWith IS.union x (IS.singleton y)

build :: Env -> Int -> IO World
build env topology = do
  (pts, pairs) <- case topology of
    1 -> pure (latticePoints env, latticeLinks)
    2 -> do
      pts <- replicateM n ((,) <$> randomRIO (100, w * 0.62) <*> randomRIO (130, h - 60))
      ts <- replicateM (2 * n) (randomRIO (0, n - 1))
      pure (pts, zip (concatMap (replicate 2) [0 .. n - 1]) ts)
    _ -> do
      pts <- mapM groupPoint [0 .. n - 1]
      inner <- mapM groupLink (concatMap (replicate 3) [0 .. n - 1])
      bridges <- replicateM 3 ((,) <$> randomRIO (0, half - 1) <*> ((+ half) <$> randomRIO (0, half - 1)))  -- 2 グループを細くつなぐ
      pure (pts, inner ++ bridges)
  let G adj es = foldl link (G IM.empty []) pairs
      adjList = V.generate n (\i -> IS.toList (IM.findWithDefault IS.empty i adj))
  ops <- replicateM n (randomRIO (0, 1))
  pure (World (V.fromList pts) adjList (reverse es) (U.fromList ops) topology Seq.empty)
  where
    w = envW env
    h = envH env
    half = n `div` 2
    groupPoint i = do
      dx <- randomRIO (-110, 110)
      dy <- randomRIO (-200, 200)
      pure ((if i < half then w * 0.2 else w * 0.5) + dx, h * 0.55 + dy)
    groupLink i = do
      j <- randomRIO (0, half - 1)
      pure (i, if i < half then j else j + half)

latticeCols :: Int
latticeCols = 15

latticePoints :: Env -> [Point]
latticePoints env =
  [ ( remap (fromIntegral (i `mod` latticeCols)) 0 (fromIntegral latticeCols - 1) 100 (envW env * 0.62)
    , remap (fromIntegral (i `div` latticeCols)) 0 9 130 (envH env - 60) )
  | i <- [0 .. n - 1] ]

latticeLinks :: [(Int, Int)]
latticeLinks = concat
  [ [(i, i + 1) | i `mod` latticeCols < latticeCols - 1] ++ [(i, i + latticeCols) | i + latticeCols < n]
  | i <- [0 .. n - 1] ]

-- ★ 投票者モデル：ランダムな点が、ランダムな隣の意見をコピー
copyOnce :: V.Vector [Int] -> U.Vector Int -> IO (U.Vector Int)
copyOnce adj ops = do
  i <- randomRIO (0, n - 1)
  case adj V.! i of
    [] -> pure ops
    nbs -> do
      k <- randomRIO (0, length nbs - 1)
      pure (case drop k nbs of
              (nb : _) -> ops U.// [(i, ops U.! nb)]
              [] -> ops)

orangeFraction :: U.Vector Int -> Float
orangeFraction ops = fromIntegral (U.sum ops) / fromIntegral n

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  ops <- foldM (\o _ -> copyOnce (wAdj w) o) (wOpinion w) [1 .. copiesPerFrame]
  let hist | envFrame env `mod` 5 == 0 = Seq.drop (Seq.length (wHistory w) + 1 - historyLen) (wHistory w |> orangeFraction ops)
           | otherwise = wHistory w
  pure w { wOpinion = ops, wHistory = hist }

opinionHue :: Int -> Float
opinionHue 0 = 200
opinionHue _ = 30

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic (wEdges w) ++ map nodePic [0 .. n - 1] ++ graph ++ hud)
  where
    ops = wOpinion w
    pos i = let (x, y) = wNodes w V.! i in toG env x y
    edgePic (a, b)
      | ops U.! a == ops U.! b = Color (hsb (opinionHue (ops U.! a)) 50 60) (Line [pos a, pos b])
      | otherwise = Color (hsb 0 0 30) (Line [pos a, pos b])
    nodePic i = Color (hsb (opinionHue (ops U.! i)) 80 100) (at (pos i) (disc 6))
    -- 右：橙の割合の推移
    gx = envW env * 0.7
    gw = envW env - gx - 40
    gy = envH env * 0.25
    gh = envH env * 0.5
    frac = orangeFraction ops
    orangeCount = U.sum ops
    unified = orangeCount == 0 || orangeCount == n
    curve = [ toG env (gx + (fromIntegral t / fromIntegral historyLen) * gw) (gy + gh - v * gh)
            | (t, v) <- zip [0 :: Int ..] (toList (wHistory w)) ]
    graph =
      [ Color (hsb 0 0 30) (Line [toG env gx (gy + gh), toG env (gx + gw) (gy + gh)])
      , Color (hsb 0 0 30) (Line [toG env gx (gy + gh / 2), toG env (gx + gw) (gy + gh / 2)])
      , Color (hsb 30 80 100) (thickPath 2.5 curve)
      , txt env gx (gy - 10) 14 (hsb 0 0 60) "Orange share (100% or 0% = unified)"
      , txt env gx (gy + gh + 36) 22 (hsb 0 0 100) ("Orange " ++ nf 0 (frac * 100) ++ " %  Blue " ++ nf 0 ((1 - frac) * 100) ++ " %")
      ]
    hud =
      [ txt env 20 32 18 (hsb 0 0 100)
          ("1: lattice   2: random   3: two groups   Click: flip   R: restart" ++ (if unified then "   -> Unified!" else ""))
      , txt env 20 60 15 (hsb 0 0 60) "The only rule: copy one neighbor. Lattice grows big blobs, random unifies fast, two groups stay split."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        hits = [(i, 1 - wOpinion w U.! i) | (i, p) <- zip [0 ..] (V.toList (wNodes w)), dist2 m p < 10]
    in pure w { wOpinion = wOpinion w U.// hits }
  EventKey (Char '1') Down _ _ -> build env 1
  EventKey (Char '2') Down _ _ -> build env 2
  EventKey (Char '3') Down _ _ -> build env 3
  EventKey (Char c) Down _ _ | c `elem` "rR" -> build env (wTopology w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 つのトポロジーで、統一されるまでの時間を比べる
-- 2. 2 グループ（3）で、グループ間の線を増やすと（replicateM 3 → 20）どうなる？
-- 3. 「まねない頑固者」を 1 人入れると（その点は更新しない）、全体はどうなる？

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
