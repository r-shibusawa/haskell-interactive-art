-- ============================================
-- 42-01 グラフの森
-- グラフの辺をいくつか選ぶ。選んだ辺が「閉路を含まない」（森になる）なら独立、閉路ができたら従属
--   判定は union-find：辺の両端がもう同じ木にいれば、その辺で閉路ができる
--   これが「グラフ的マトロイド」。00 のベクトルと同じ「独立／従属」の構造になっている
-- 操作：辺をクリック = 選ぶ・外す / ノードをドラッグ = 動かす / R = ランダムなグラフ / C = 全部外す / N = ノード数
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import Control.Monad (foldM)
import Data.List (foldl', nub, sortOn)
import qualified Data.IntMap.Strict as IM

nNode0 :: Int
nNode0 = 6               -- ★ ノードの数（4〜8）

extraEdges :: Int
extraEdges = 4           -- ★ 木より何本多く辺を張るか

data Edge = Edge { eA, eB :: !Int, eOn :: !Bool }

data World = World
  { wN     :: !Int
  , wNodes :: ![Point]
  , wEdges :: ![Edge]
  , wDrag  :: !Int
  , wCycle :: ![Int]     -- 閉路になっている辺の番号
  , wFam   :: ![Int]     -- 大きさごとの独立集合の数
  , wBases :: !Int
  }

main :: IO ()
main = runSketch (sketch "42-01 Graph forests" world0)
  { skBack  = hsb 150 25 10
  , skStart = \env -> randomGraph env world0
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World nNode0 [] [] (-1) [] [] 0

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

randomGraph :: Env -> World -> IO World
randomGraph env w = do
  let n = wN w
      cx = envW env * 0.4
      cy = envH env * 0.55
      rad = min (envW env) (envH env) * 0.3
      node i = do
        let a = tau * fromIntegral i / fromIntegral n - pi / 2
        jx <- randomRIO (-30, 30)
        jy <- randomRIO (-30, 30)
        pure (cx + rad * cos a + jx, cy + rad * sin a + jy)
  ns <- mapM node [0 .. n - 1]
  -- まず全域木をランダムに張り、そこに辺を足す
  order <- shuffleIO [0 .. n - 1]
  treeE <- mapM (\i -> do k <- randomRIO (0, i - 1)
                          pure (Edge (pick 0 order i) (pick 0 order k) False)) [1 .. n - 1]
  es <- addExtra n (n - 1 + extraEdges) (200 :: Int) treeE
  pure (update w { wNodes = ns, wEdges = es })

-- 同じ組を避けて、辺を target 本になるまで足す
addExtra :: Int -> Int -> Int -> [Edge] -> IO [Edge]
addExtra n target tries es
  | length es >= target || tries <= 0 = pure es
  | otherwise = do
      a <- randomRIO (0, n - 1)
      b <- randomRIO (0, n - 1)
      let dup = any (\e -> ends e == (a, b) || ends e == (b, a)) es
      addExtra n target (tries - 1) (if a == b || dup then es else es ++ [Edge a b False])

-- ★ 選んだ辺の判定：union-find で閉路を探し、閉路の辺を集める
update :: World -> World
update w =
  let n = wN w
      es = wEdges w
      on = [ i | (i, e) <- zip [0 ..] es, eOn e ]
      prs is = map (ends . pick (Edge 0 0 False) es) is
      -- 閉路の辺：その辺を外しても「残りだけでまだ両端がつながっている」辺
      cyc | isForest n (prs on) = []
          | otherwise = [ i | i <- on
                        , let (a, b) = ends (pick (Edge 0 0 False) es i)
                        , connected n (prs (filter (/= i) on)) a b ]
      -- 全部の部分集合を数える（辺 <= 12 本なら一瞬）
      nE = length es
      rankAll = forestRank n (map ends es)
      forests = [ s | m <- [0 .. (2 :: Int) ^ nE - 1]
                , let sub = [ ends e | (i, e) <- zip [0 ..] es, testBit m i ]
                , isForest n sub
                , let s = length sub ]
      fam = [ length (filter (== s) forests) | s <- [0 .. nE] ]
  in w { wCycle = cyc, wFam = fam, wBases = length (filter (== rankAll) forests) }

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      ns = wNodes w
      n = wN w
      node = pick (0, 0) ns
      onCount = length (filter eOn es)
      indep = null (wCycle w)
      hueSel = if indep then 130 else 5
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            inCyc = i `elem` wCycle w
            c | eOn e = hsb (if inCyc then 5 else 130) 85 100
              | otherwise = gray 0.45
            wgt | eOn e = if inCyc then 9 else 6
                | otherwise = 2.5
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures [ seg env wgt c p q
                    , txtC env (mx + 10) (my - 10) 14 (gray (if eOn e then 1 else 0.6)) ("e" ++ show (i + 1 :: Int)) ]
      -- ノード（同じ木は同じ色）
      uf = ufAll n (map ends (filter eOn es))
      roots = map (ufFind uf) [0 .. length ns - 1]
      comps = length (nub roots)
      nodePic i p =
        let root = pick 0 roots i
        in Pictures [ ball env 15 (hsb (fromIntegral (root * 57 `mod` 360)) 55 85) p
                    , txtC env (fst p) (snd p) 14 (gray 0.1) (show (i + 1 :: Int)) ]
      px = envW env - 440
      py = 110
      title | onCount == 0 = "Nothing chosen"
            | indep = "Independent (forest)"
            | otherwise = "Dependent (has cycle)"
      famRow (s, c) =
        ( 15, if s == onCount && onCount > 0 then hsb hueSel 70 100 else white
        , "|A| = " ++ show s ++ ": " ++ show c ++ (if c == 0 then "  (no larger forest)" else "") )
  in Pictures $
       zipWith edgePic [0 ..] es ++ zipWith nodePic [0 ..] ns ++
       [ txt env 20 40 16 white ("Click edge: select  Drag node  R: new graph  C: clear all  N: nodes " ++ show n)
       , txt env 20 66 14 (gray 0.6) "Same color = connected by chosen edges (one tree). Both ends in one tree = this edge makes a cycle."
       , box env (withA 0.08 white) (px - 20) (py - 40) 450 400
       , stack env px py
           ([ (24, if onCount == 0 then white else hsb hueSel 80 100, title)
            , (18, white, "|A| = " ++ show onCount ++ "   rank r(A) = " ++ show (n - comps))
            , (14, gray 0.6, "rank = nodes - parts = " ++ show n ++ " - " ++ show comps)
            , (14, gray 0.6, "A tree: rank equals its edge count")
            , (8, white, "")
            , (16, white, "Forests by size. Bases (trees): " ++ show (wBases w)) ]
            ++ map famRow (limitRows (zip [0 :: Int ..] (wFam w))))
       ]

-- 0 の行か |A| = 7 の行で止める
limitRows :: [(Int, Int)] -> [(Int, Int)]
limitRows [] = []
limitRows (x@(s, c) : r) = if c == 0 || s >= 7 then [x] else x : limitRows r

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestIn 20 (map (dist2 (mouseP env)) (wNodes w)) }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env - 20) mx, clampF 90 (envH env - 20) my)
    in pure w { wNodes = setAt (wDrag w) (const p) (wNodes w) }
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w < 0 ->
        let node = pick (0, 0) (wNodes w)
            i = nearestIn 14 [ segDist (mouseP env) (node (eA e)) (node (eB e)) | e <- wEdges w ]
        in pure (if i >= 0 then update w { wEdges = setAt i (\e -> e { eOn = not (eOn e) }) (wEdges w) } else w)
    | otherwise -> pure w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomGraph env w { wDrag = -1 }
    | c `elem` "cC" -> pure (update w { wEdges = map (\e -> e { eOn = False }) (wEdges w) })
    | c `elem` "nN" -> randomGraph env w { wN = if wN w >= 8 then 4 else wN w + 1, wDrag = -1 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 辺を 1 本ずつ足していき、初めて赤くなる瞬間を見る。赤い辺はいつも「輪」になっている？
-- 2. 独立集合の最大の大きさ（階数）はいつも「ノード数 − 1」？ 離れ小島があるグラフだとどうなる？
-- 3. 「基（全域木）」の数を R を押して何回か比べる。辺が多いほど多い？
-- 4. 00 と見比べる：ベクトルの「同じ直線に乗る」とグラフの「閉路になる」は同じ役目

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 文字（中央を指定）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (a, b) = sp env (x, y) in labelC a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 白いふちつきの丸（半径 r）
ball :: Env -> Float -> Color -> Point -> Picture
ball env r c p = at (sp env p) (Pictures [Color white (disc (r + 1)), Color c (disc r)])

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- リストの i 番目だけ f で変える
setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- ランダムに並べかえる（p5.js の shuffle）
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  ks <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortOn fst (zip ks xs)))

-- 点と線分の距離
segDist :: Point -> Point -> Point -> Float
segDist m@(mx, my) (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
      t = if l2 > 0 then clampF 0 1 (((mx - x1) * dx + (my - y1) * dy) / l2) else 0
  in dist2 m (x1 + t * dx, y1 + t * dy)

-- limit より近いものの中でいちばん近い番号（なければ -1）
nearestIn :: Float -> [Float] -> Int
nearestIn limit ds = fst (foldl' pickNear (-1, limit) (zip [0 ..] ds))
  where pickNear (bi, bd) (i, d) = if d < bd then (i, d) else (bi, bd)

-- union-find：親の表を IntMap で持つ
ufNew :: Int -> IM.IntMap Int
ufNew n = IM.fromDistinctAscList [ (i, i) | i <- [0 .. n - 1] ]

ufFind :: IM.IntMap Int -> Int -> Int
ufFind uf x = case IM.lookup x uf of
  Just p | p /= x -> ufFind uf p
  _ -> x

-- 2 つの木をつなぐ。もう同じ木なら Nothing（この辺で閉路ができる）
ufUnion :: IM.IntMap Int -> (Int, Int) -> Maybe (IM.IntMap Int)
ufUnion uf (a, b) =
  let ra = ufFind uf a
      rb = ufFind uf b
  in if ra == rb then Nothing else Just (IM.insert ra rb uf)

-- 辺を全部つないだ union-find
ufAll :: Int -> [(Int, Int)] -> IM.IntMap Int
ufAll n = foldl' (\uf e -> maybe uf id (ufUnion uf e)) (ufNew n)

-- 辺のリストが森か（閉路がないか）
isForest :: Int -> [(Int, Int)] -> Bool
isForest n es = case foldM ufUnion (ufNew n) es of
  Just _ -> True
  Nothing -> False

-- 辺のリストの階数（閉路を作らずに入れられる本数）
forestRank :: Int -> [(Int, Int)] -> Int
forestRank n es = snd (foldl' stepR (ufNew n, 0) es)
  where stepR (uf, r) e = case ufUnion uf e of
          Just uf' -> (uf', r + 1)
          Nothing -> (uf, r)

-- 辺のリストだけで a と b がつながっているか
connected :: Int -> [(Int, Int)] -> Int -> Int -> Bool
connected n es a b = let uf = ufAll n es in ufFind uf a == ufFind uf b
