-- ============================================
-- 06 Mapper（データの「地図」をつくる）
-- 点群を 1 つの値（ここでは x 座標）で輪切りにし、各輪切りの中で近い点をまとめて 1 つの丸にする
-- 輪切りを少し重ねると、隣の丸どうしがつながってグラフになる。高次元データの形を見る定番の方法
-- マウス左右 = 輪切りの数 / 上下 = 重なり / 1〜3 で形 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import Numeric (showFFloat)
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts   :: ![Point]
  , wShape :: !Int
  , wSeed  :: !Int
  }

-- Mapper の丸：通し番号、輪切りの番号、中の点、重心の y
data Node = Node { ndId :: !Int, ndK :: !Int, ndMembers :: !IS.IntSet, ndY :: !Float }

eps :: Float
eps = 45                -- ★ この距離より近い点は同じクラスタ

initialWorld :: World
initialWorld = World [] 1 3

main :: IO ()
main = runSketch (sketch "09-06 Mapper" initialWorld)
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env initialWorld)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

shapeName :: Int -> String
shapeName k = case k of
  1 -> "Circle"
  2 -> "Y shape"
  _ -> "Figure 8"

makePoints :: Env -> World -> World
makePoints env w = w { wPts = map sample (take 160 (randomChunks (wSeed w) 7)) }
  where
    cx = envW env * 0.28
    cy = envH env / 2 + 30
    bigR = min (envW env) (envH env) * 0.22
    sample (ua : ut : ub : g1 : g2 : g3 : g4 : _) =
      let a = ua * tau
          (x, y) = case wShape w of
            1 -> (cx + cos a * bigR, cy + sin a * bigR)
            2 -> let ang | ub < 1 / 3 = -pi / 2      -- Y 字の 3 本の枝
                         | ub < 2 / 3 = pi * 0.25
                         | otherwise = pi * 0.75
                 in (cx + cos ang * bigR * ut, cy + sin ang * bigR * ut)
            _ -> (cx + bigR * 1.2 * sin a, cy + bigR * 0.8 * sin a * cos a)
      in (x + gaussian g1 g2 * 6, y + gaussian g3 g4 * 6)
    sample _ = (cx, cy)

-- ★ Mapper：丸と、点を共有する丸どうしの線
runMapper :: [Point] -> Int -> Float -> ([Node], [(Node, Node)])
runMapper pts intervals overlap = (nodes, edges)
  where
    indexed = zip [0 ..] pts
    (lo, hi) = xRange pts
    w = (hi - lo) / fromIntegral intervals
    nodes = zipWith (\i nd -> nd { ndId = i }) [0 ..] (concatMap sliceNodes [0 .. intervals - 1])
    -- 1. 輪切り（重なりあり）
    sliceNodes k =
      let a = lo + fromIntegral k * w - w * overlap
          b = lo + fromIntegral (k + 1) * w + w * overlap
          inside = [(i, p) | (i, p) <- indexed, fst p >= a, fst p <= b]
      in map (toNode k) (filter ((>= 2) . length) (clusters inside))   -- 3. クラスタ = 丸
    -- 2. 輪切りの中でクラスタリング（距離 eps 以内はつなぐ：単連結）
    clusters inside =
      let uf = foldl' (\u (i, j) -> ufUnion u i j) (ufNew (map fst inside))
                 [(i, j) | (i, p) <- inside, (j, q) <- inside, i < j, dist2 p q < eps]
      in map reverse (IM.elems (IM.fromListWith (++) [(ufFind uf i, [(i, p)]) | (i, p) <- inside]))
    toNode k g = Node 0 k (IS.fromList (map fst g)) (sum (map (snd . snd) g) / fromIntegral (length g))
    -- 4. 点を共有する丸どうしをつなぐ
    edges = [ (a, b) | a <- nodes, b <- nodes, ndId a < ndId b
            , not (IS.disjoint (ndMembers a) (ndMembers b)) ]

-- x 座標の最小と最大（点がなければ 0, 1）
xRange :: [Point] -> (Float, Float)
xRange [] = (0, 1)
xRange ps = (minimum (map fst ps), maximum (map fst ps))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 左：点群と輪切り
    Pictures [ Color (hsba (hueOf k) 50 60 0.12)
                 (rectTL (lo + fromIntegral k * bandW - bandW * overlap) 100 (bandW * (1 + 2 * overlap)) (h - 140))
             | k <- [0 .. intervals - 1] ]
  , Color (hsba 0 0 100 0.7) (Pictures [at (g x y) (disc 2.5) | (x, y) <- pts])
  -- 右：Mapper グラフ（輪切りの順に左から並べ、縦はクラスタの位置）
  , Color (hsb 0 0 60) (Pictures [thickLine 2 (nodePos a) (nodePos b) | (a, b) <- edges])
  , Pictures [ Color (hsb (hueOf (ndK nd)) 70 100)
                 (at (nodePos nd) (disc ((14 + sqrt (fromIntegral (IS.size (ndMembers nd))) * 4) / 2)))
             | nd <- nodes ]
  , txt env 20 40 18 (hsb 0 0 100)
      ("Slices " ++ show intervals ++ " (X)  Overlap " ++ nf 2 overlap ++ " (Y)  Shape: " ++ shapeName (wShape w)
        ++ " (1-3)  Nodes " ++ show nNodes ++ "  Edges " ++ show nEdges ++ "  Loops " ++ show loops)
  , txt env 20 68 15 (hsb 0 0 60)
      "Left: slices by x (colour bands). Right: each blob in a band is a node; nodes sharing points are linked."
  , txt env 20 90 15 (hsb 0 0 60)
      "A circle gives a loop, Y gives branches, a figure 8 gives two loops.  Click: new points"
  ]
  where
    g = toG env
    h = envH env
    pts = wPts w
    (mx, my) = mouseXY env
    intervals = floor (mapC mx 0 (envW env) 3 14) :: Int
    overlap = mapC my 0 h 0.6 0.05
    (nodes, edges) = runMapper pts intervals overlap
    (lo, hi) = xRange pts
    bandW = (hi - lo) / fromIntegral intervals
    hueOf k = fromIntegral ((k * 50) `mod` 360) :: Float
    rectTL x y rw rh = let (cx, cy) = g (x + rw / 2) (y + rh / 2) in rect cx cy rw rh
    gx = envW env * 0.58
    gw = envW env - gx - 60
    gy = 130
    gh = h - 200
    nodePos nd = g (gx + fromIntegral (ndK nd) / fromIntegral (intervals - 1) * gw)
                   (remap (ndY nd) 100 (h - 40) gy (gy + gh))
    -- 輪の数（グラフの独立なループ = E − V + 連結成分）
    nNodes = length nodes
    nEdges = length edges
    ufGraph = foldl' (\u (a, b) -> ufUnion u (ndId a) (ndId b)) (ufNew [0 .. nNodes - 1]) edges
    comps = IS.size (IS.fromList (map (ufFind ufGraph) [0 .. nNodes - 1]))
    loops = nEdges - nNodes + comps

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 輪切りを 3 にすると形が壊れ、14 にすると細かすぎて切れる。ちょうどいい数は？
-- 2. 重なりを 0.05 にすると線が消える（隣と点を共有しないから）
-- 3. 輪切りの基準を x から「中心からの距離」に変えると、円はどんなグラフに？

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

-- 範囲からはみ出さない remap（p5.js の map(v, a, b, c, d, true)）
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- 種から決まる 0〜1 の乱数を k 個ずつ区切った列（p5.js の randomSeed + random）。
-- 同じ種なら毎回同じ点群になる
randomChunks :: Int -> Int -> [[Float]]
randomChunks seed k = go (randomRs (0, 1) (mkStdGen seed))
  where go us = let (a, b) = splitAt k us in a : go b

-- 0〜1 の乱数 2 つから正規分布の乱数を作る（p5.js の randomGaussian）
gaussian :: Float -> Float -> Float
gaussian u v = sqrt (-2 * log (max 1e-6 u)) * cos (tau * v)

-- union-find（つながったグループを管理する）
newtype UnionFind = UnionFind (IM.IntMap Int)

ufNew :: [Int] -> UnionFind
ufNew is = UnionFind (IM.fromList [(i, i) | i <- is])

ufFind :: UnionFind -> Int -> Int
ufFind uf@(UnionFind m) x = case IM.lookup x m of
  Just p | p /= x -> ufFind uf p
  _               -> x

ufUnion :: UnionFind -> Int -> Int -> UnionFind
ufUnion uf@(UnionFind m) a b =
  let ra = ufFind uf a
      rb = ufFind uf b
  in if ra == rb then uf else UnionFind (IM.insert ra rb m)
