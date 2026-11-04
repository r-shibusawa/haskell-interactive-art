-- ============================================
-- 42-02 同じ構造：グラフと GF(2) のベクトル
-- 左はグラフ、右はその「接続行列」（行 = ノード、列 = 辺。辺の両端に 1）。計算は GF(2)（1 + 1 = 0 の世界）
--   辺集合を選ぶと、左では「閉路がない？」、右では「列ベクトルが独立？（GF(2) の階数 = 本数？）」を判定
--   答えはいつも一致する。グラフ的マトロイドは GF(2) 上で「表現できる」
-- 操作：辺または列をクリック = 選ぶ・外す / ノードをドラッグ / R = 新しいグラフ / C = 全部外す
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, testBit, xor, (.|.))
import Control.Monad (foldM)
import Data.Bits (xor)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM

nNode :: Int
nNode = 5                -- ★ ノードの数（4〜6）

nEdge :: Int
nEdge = 7                -- ★ 辺の数

cw, ch :: Float
cw = 54                  -- ★ 行列の 1 マスの幅
ch = 44                  -- ★ 行列の 1 マスの高さ

data Edge = Edge { eA, eB :: !Int, eOn :: !Bool }

data World = World
  { wNodes :: ![Point]
  , wEdges :: ![Edge]
  , wDrag  :: !Int
  , wCycle :: ![Int]
  }

main :: IO ()
main = runSketch (sketch "42-02 Graph and GF(2) vectors" world0)
  { skBack  = hsb 260 25 10
  , skStart = \env -> randomGraph env world0
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] (-1) []

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

matrixOrigin :: Env -> Point
matrixOrigin env = (envW env * 0.5, envH env * 0.32)

randomGraph :: Env -> World -> IO World
randomGraph env w = do
  let cx = envW env * 0.25
      cy = envH env * 0.55
      rad = min (envW env * 0.18) (envH env * 0.3)
      node i = do
        let a = tau * fromIntegral i / fromIntegral nNode - pi / 2
        jx <- randomRIO (-20, 20)
        jy <- randomRIO (-20, 20)
        pure (cx + rad * cos a + jx, cy + rad * sin a + jy)
  ns <- mapM node [0 .. nNode - 1]
  order <- shuffleIO [0 .. nNode - 1]
  treeE <- mapM (\i -> do k <- randomRIO (0, i - 1)
                          pure (Edge (pick 0 order i) (pick 0 order k) False)) [1 .. nNode - 1]
  es <- addExtra (200 :: Int) treeE
  let sorted = map (\e -> if eA e > eB e then e { eA = eB e, eB = eA e } else e) es
  pure (update w { wNodes = ns, wEdges = sorted })

addExtra :: Int -> [Edge] -> IO [Edge]
addExtra tries es
  | length es >= nEdge || tries <= 0 = pure es
  | otherwise = do
      a <- randomRIO (0, nNode - 1)
      b <- randomRIO (0, nNode - 1)
      let dup = any (\e -> ends e == (a, b) || ends e == (b, a)) es
      addExtra (tries - 1) (if a == b || dup then es else es ++ [Edge a b False])

-- ★ 辺の列ベクトル：両端のノードの行に 1（ビットで持つ）
column :: Edge -> Int
column e = (1 `shiftL` eA e) .|. (1 `shiftL` eB e)

update :: World -> World
update w =
  let on = [ (i, e) | (i, e) <- zip [0 :: Int ..] (wEdges w), eOn e ]
      prs = map (ends . snd)
      cyc | isForest nNode (prs on) = []
          | otherwise = [ i | (i, e) <- on, connected nNode (prs (filter ((/= i) . fst) on)) (eA e) (eB e) ]
  in w { wCycle = cyc }

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      node = pick (0, 0) (wNodes w)
      onE = filter eOn es
      nOn = length onE
      forest = null (wCycle w)
      r2 = rankGF2 (map column onE)
      indep2 = r2 == nOn
      hueOf b = if nOn == 0 then white else hsb (if b then 130 else 5) 80 100
      inCyc i = i `elem` wCycle w
      -- ---- 左：グラフ
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            c = if eOn e then hsb (if inCyc i then 5 else 130) 85 100 else gray 0.45
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures [ seg env (if eOn e then 7 else 2.5) c p q
                    , txtC env (mx + 10) (my - 10) 14 (gray (if eOn e then 1 else 0.6)) ("e" ++ show (i + 1 :: Int)) ]
      nodePic i p = Pictures [ ball env 15 (hsb 260 30 80) p, txtC env (fst p) (snd p) 14 (gray 0.1) ("v" ++ show (i + 1 :: Int)) ]
      -- ---- 右：接続行列（GF(2)）
      (mx0, my0) = matrixOrigin env
      nf = fromIntegral nNode :: Float
      colPic j e =
        let x = mx0 + fromIntegral j * cw
            h = if eOn e then (if inCyc j then 5 else 130) else 0
            cell i =
              let v = i == eA e || i == eB e
                  y = my0 + fromIntegral i * ch + ch / 2
                  c | v = hsb (if eOn e then h else 200) 60 100
                    | otherwise = gray 0.4
              in txtC env x y (if v then 20 else 16) c (if v then "1" else "0")
        in Pictures $
             [ box env (hsba h 70 60 0.35) (x - cw / 2 + 2) (my0 - ch - 6) (cw - 4) (ch * (nf + 1) + 4) | eOn e ] ++
             [ txtC env x (my0 - ch / 2 - 4) 14 (gray (if eOn e then 1 else 0.65)) ("e" ++ show (j + 1)) ] ++
             map cell [0 .. nNode - 1]
      rowLabels = [ txtC env (mx0 - cw / 2 - 20) (my0 + fromIntegral i * ch + ch / 2) 14 (gray 0.7) ("v" ++ show (i + 1))
                  | i <- [0 .. nNode - 1] ]
      nCol = fromIntegral (length es) :: Float
      bars = [ seg env 1 (gray 0.4) (x, my0 - 2) (x, my0 + nf * ch + 2)
             | x <- [mx0 - cw / 2 - 2, mx0 + nCol * cw - cw / 2 + 2] ]
      -- 選んだ列の和（GF(2)）
      total = foldl' xor 0 (map column onE)
      bits = commaList [ if testBit total i then "1" else "0" | i <- [0 .. nNode - 1] ]
      sy = my0 + nf * ch + 40
      py = envH env - 120
  in Pictures $
       zipWith edgePic [0 ..] es ++ zipWith nodePic [0 ..] (wNodes w) ++
       zipWith colPic [0 ..] es ++ rowLabels ++ bars ++
       [ stack env (mx0 - cw / 2) sy
           [ (16, white, "Sum of chosen columns in GF(2): (" ++ bits ++ ")")
           , (15, hsb 5 80 100, if nOn > 0 && total == 0 then "<- zero vector = dependent!" else "")
           , (14, gray 0.6, "Sum over a cycle: each node appears twice,")
           , (14, gray 0.6, "1 + 1 = 0, so every entry becomes 0.") ]
       , txt env 20 40 16 white "Click edge or column: select  Drag node  R: new graph  C: clear all"
       , txt env 20 66 14 (gray 0.6) "Left: no cycle = independent.  Right: GF(2) rank = count = independent.  They always agree."
       , stack env 20 py
           [ (20, hueOf forest, "Graph: " ++ (if nOn == 0 then "-" else if forest then "forest (indep.)" else "cycle (dependent)")
                                ++ "  rank " ++ show (forestRank nNode (map ends onE)))
           , (20, hueOf indep2, "GF(2): " ++ (if nOn == 0 then "-" else if indep2 then "independent" else "dependent")
                                ++ "  rank " ++ show r2 ++ " / count " ++ show nOn)
           , (16, white, if nOn == 0 then "Choose some edges"
                         else if forest == indep2 then "-> Same answer: same independence = same matroid" else "-> ?") ]
       ]

commaList :: [String] -> String
commaList [] = ""
commaList [x] = x
commaList (x : r) = x ++ ", " ++ commaList r

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestIn 20 (map (dist2 (mouseP env)) (wNodes w)) }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env * 0.45) mx, clampF 90 (envH env - 140) my)
    in pure w { wNodes = setAt (wDrag w) (const p) (wNodes w) }
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w >= 0 -> pure w { wDrag = -1 }
    | otherwise ->
        let m@(mx, my) = mouseP env
            node = pick (0, 0) (wNodes w)
            hit = nearestIn 14 [ segDist m (node (eA e)) (node (eB e)) | e <- wEdges w ]
            (mx0, my0) = matrixOrigin env
            j = floor ((mx - mx0) / cw + 0.5) :: Int      -- 列をクリック？
            inCols = j >= 0 && j < length (wEdges w) && my > my0 - ch && my < my0 + fromIntegral nNode * ch
            i | hit >= 0 = hit
              | inCols = j
              | otherwise = -1
        in pure (if i >= 0 then update w { wEdges = setAt i (\e -> e { eOn = not (eOn e) }) (wEdges w) } else w)
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomGraph env w { wDrag = -1 }
    | c `elem` "cC" -> pure (update w { wEdges = map (\e -> e { eOn = False }) (wEdges w) })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 閉路になる辺だけを選んで、右の「列の和」が全部 0 になるのを確かめる
-- 2. 木（全域木）を選ぶと階数はノード数 − 1。行列の階数も同じ？（行が 1 つ余分 = 全部足すと 0）
-- 3. 実数で計算したらどうなる？ 接続行列の 1 を「出る側 −1、入る側 +1」にすると実数でも同じ構造になる
-- 4. GF(2) で表現できないマトロイドもある（11 の非ファノ）。逆にグラフでは作れないマトロイドも（10 の U2,4）

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

-- GF(2) 上の階数：ベクトルをビット列（整数）で持ち、XOR で消去する
rankGF2 :: [Int] -> Int
rankGF2 = length . foldl' add []
  where
    add basis v0 =
      let v = foldl' (\v1 b -> min v1 (v1 `xor` b)) v0 basis
      in if v /= 0 then insertDesc v basis else basis
    insertDesc v bs = takeWhile (> v) bs ++ [v] ++ dropWhile (> v) bs
