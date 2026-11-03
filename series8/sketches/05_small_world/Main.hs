-- ============================================
-- 05 スモールワールド（6 次の隔たり）
-- 隣としかつながっていない輪は「遠い」。少しだけランダムにつなぎかえると、
-- 世界が一気に小さくなる（平均の距離が急に減る）のに、ご近所づきあい（クラスタ）は残る
-- マウス左右 = つなぎかえ確率 β / 点をクリック = その点からの距離を色で表示 / R で乱数を変える
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Numeric (showFFloat)
import System.Random (mkStdGen, randoms)

data World = World
  { wSource :: !Int        -- 距離を測る出発点
  , wSeed   :: !Int
  }

n, k :: Int
n = 60                     -- ★ 点の数
k = 4                      -- ★ 各点が隣とつなぐ数

-- グラフ（つくる途中で乱数の列を消費していく）
data Graph = Graph
  { gRand  :: [Float]
  , gAdj   :: !(IM.IntMap IS.IntSet)
  , gEdges :: ![(Int, Int)]
  }

main :: IO ()
main = runSketch (sketch "08-05 Small World" (World 0 3))
  { skBack  = hsb 220 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ ワッツ＝ストロガッツ：輪をつくって、各エッジを確率 β でランダムにつなぎかえる
build :: Float -> Int -> Graph
build beta seed = foldl' addEdge g0 [(i, j) | i <- [0 .. n - 1], j <- [1 .. k `div` 2]]
  where
    g0 = Graph (randoms (mkStdGen seed)) (IM.fromList [(i, IS.empty) | i <- [0 .. n - 1]]) []
    addEdge g (i, j) =
      let (r, rs1) = nextR (gRand g)
          (b, rs2) | r < beta = pick g i rs1 1
                   | otherwise = ((i + j) `mod` n, rs1)
      in if not (linked g i b) && i /= b
           then g { gRand = rs2
                  , gAdj = IM.adjust (IS.insert i) b (IM.adjust (IS.insert b) i (gAdj g))
                  , gEdges = (i, b) : gEdges g }
           else g { gRand = rs2 }

-- つなぎかえ先をランダムに選ぶ（自分自身や、すでにつながった点はやめる。最大 50 回）
pick :: Graph -> Int -> [Float] -> Int -> (Int, [Float])
pick g a rs tries =
  let (r, rs1) = nextR rs
      b = floor (r * fromIntegral n)
  in if (b == a || linked g a b) && tries < (50 :: Int)
       then pick g a rs1 (tries + 1)
       else (b, rs1)

linked :: Graph -> Int -> Int -> Bool
linked g a b = IS.member b (neighbours g a)

neighbours :: Graph -> Int -> IS.IntSet
neighbours g a = IM.findWithDefault IS.empty a (gAdj g)

nextR :: [Float] -> (Float, [Float])
nextR (r : rs) = (r, rs)
nextR [] = (0, [])

-- 距離（BFS）：同じ歩数の点をまとめて広げていく
bfs :: Graph -> Int -> IM.IntMap Int
bfs g source = go 1 [source] (IM.singleton source 0)
  where
    go _ [] seen = seen
    go d level seen =
      let next = IS.toList (IS.unions (map (neighbours g) level))
          new = [v | v <- next, not (IM.member v seen)]
      in go (d + 1 :: Int) new (foldl' (\m v -> IM.insert v d m) seen new)

-- クラスタ係数：自分の友だちどうしが友だちである割合
clustering :: Graph -> Float
clustering g = sum (map local [0 .. n - 1]) / fromIntegral n
  where
    local i =
      let nb = IS.toList (neighbours g i)
          m = length nb
          links = length [() | (a, ia) <- zip nb [0 :: Int ..], (b, ib) <- zip nb [0 ..], ia < ib, linked g a b]
      in if m < 2 then 0 else fromIntegral links / (fromIntegral (m * (m - 1)) / 2)

-- 輪の上の位置
ringPos :: Env -> Int -> Point
ringPos env i =
  let cx = envW env * 0.4
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.36
      a = tau * fromIntegral i / fromIntegral n - pi / 2
  in (cx + cos a * r, cy + sin a * r)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic (gEdges g) ++ map nodePic [0 .. n - 1] ++ stats ++ hud)
  where
    beta = clampF 0 1 (remap (fst (mouseXY env)) 0 (envW env) 0 1)
    g = build beta (wSeed w)
    dists = bfs g (wSource w)
    maxD = maximum (0 : IM.elems dists)
    pos i = let (x, y) = ringPos env i in toG env x y
    edgePic (a, b) =
      let d = abs (a - b)
          far = d > k && d < n - k           -- 輪の近所ではない線 = 近道
      in if far then Color (hsba 45 90 100 0.8) (thickLine 1.5 (pos a) (pos b))
                else Color (hsba 200 40 60 0.5) (Line [pos a, pos b])
    nodePic i =
      let hue = case IM.lookup i dists of
            Nothing -> 0
            Just d -> remap (fromIntegral d) 0 (fromIntegral (max maxD 1)) 120 0
      in Color (hsb hue 80 100) (at (pos i) (disc (if i == wSource w then 11 else 6)))
    reached = IM.elems dists
    avg = fromIntegral (sum reached) / fromIntegral (max 1 (length reached)) :: Float
    rx = envW env * 0.75
    ry = envH env * 0.3
    stats =
      [ txt env rx ry 22 (hsb 0 0 100) ("beta = " ++ nf 2 beta)
      , txt env rx (ry + 44) 18 (hsb 0 0 100) ("Avg distance " ++ nf 2 avg ++ " steps")
      , txt env rx (ry + 76) 18 (hsb 0 0 100) ("Max distance " ++ show maxD ++ " steps")
      , txt env rx (ry + 108) 18 (hsb 0 0 100) ("Clustering " ++ nf 2 (clustering g))
      ] ++
      zipWith (\i s -> txt env rx (ry + 160 + i * 22) 14 (hsb 0 0 60) s) [0 ..]
      [ "beta = 0: ring"
      , "   (far, clustered)"
      , "small beta: few shortcuts"
      , "   (near, clustered)"
      , "   <- small world"
      , "beta = 1: random"
      , "   (near, not clustered)"
      ]
    hud =
      [ txt env 20 40 16 (hsb 0 0 100) "Mouse X: rewiring beta   Click node: distance from it (green near, red far)   Yellow: shortcut"
      , txt env 20 68 15 (hsb 0 0 60) "A tiny beta (0.05) slashes the avg distance: this is how 'six degrees of separation' works."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure (case [i | i <- [0 .. n - 1], dist2 (mouseXY env) (ringPos env i) < 14] of
            [] -> w
            hits -> w { wSource = last hits })
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    s <- randomRIO (0, 9999)
    pure w { wSeed = s }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. β = 0 → 0.05 → 0.2 → 1 で、平均距離とクラスタ係数の変わり方を比べる
-- 2. n を 200 にすると、β = 0 の平均距離は？（n / (2k) くらい）
-- 3. 近道が 1 本あるだけで、どれだけ距離が縮むか

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
