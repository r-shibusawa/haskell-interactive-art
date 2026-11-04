-- ============================================
-- 42-09 双対マトロイド
-- 平面グラフ（3×3 の格子）とその双対グラフ（面 = 頂点、辺を横切る辺）を重ねて描く
--   辺集合 T が全域木 ⇔ 残りの辺（補集合）の双対辺が双対グラフの全域木。基の補集合 = 双対の基
--   T に閉路（サーキット）があると、その双対辺は双対グラフを切る「カット」になる
-- 操作：辺をクリック = T に入れる・外す / ノードをドラッグ / スペース = ランダムな全域木 / C = 全部外す / D = 双対の表示 / R = 位置を戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import Numeric (showFFloat)
import qualified Data.IntMap.Strict as IM

nF :: Int
nF = 5                   -- 面の数（4 + 外側）

outerFace :: Int
outerFace = 4            -- ★ 外側の面の番号

-- f1, f2 = 辺の両側の面の番号
data Edge = Edge { eA, eB, eF1, eF2 :: !Int, eOn :: !Bool }

data World = World
  { wNodes :: ![Point]
  , wEdges :: ![Edge]
  , wFaces :: ![[Int]]   -- 内側の面の頂点番号
  , wDrag  :: !Int
  , wDual  :: !Bool
  , wCycle :: ![Int]
  }

main :: IO ()
main = runSketch (sketch "42-09 Dual matroid" world0)
  { skBack  = hsb 280 25 9
  , skStart = \env -> pure (buildLattice env world0)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] [] (-1) True []

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

buildLattice :: Env -> World -> World
buildLattice env w =
  let cell = min (envW env * 0.18) (envH env * 0.24)
      ox = envW env * 0.3 - cell
      oy = envH env * 0.56 - cell
      ns = [ (ox + fromIntegral i * cell, oy + fromIntegral j * cell) | j <- [0 .. 2 :: Int], i <- [0 .. 2 :: Int] ]
      vid i j = i + j * 3
      cellId i j = if i < 0 || j < 0 || i > 1 || j > 1 then outerFace else i + j * 2
      horiz = [ Edge (vid i j) (vid (i + 1) j) (cellId i (j - 1)) (cellId i j) False | j <- [0 .. 2], i <- [0, 1] ]
      vert = [ Edge (vid i j) (vid i (j + 1)) (cellId (i - 1) j) (cellId i j) False | j <- [0, 1], i <- [0 .. 2] ]
      faces = [ [vid i j, vid (i + 1) j, vid (i + 1) (j + 1), vid i (j + 1)] | j <- [0, 1], i <- [0, 1] ]
  in update w { wNodes = ns, wEdges = horiz ++ vert, wFaces = faces }

-- 面（双対の頂点）の位置：内側は重心、外側は格子の右下の外
facePos :: World -> Int -> Point
facePos w f
  | f == outerFace =
      ( foldl' max (-1e9) (map fst (wNodes w)) + 90
      , foldl' max (-1e9) (map snd (wNodes w)) + 70 )
  | otherwise =
      let vs = map (pick (0, 0) (wNodes w)) (pick [] (wFaces w) f)
      in (sum (map fst vs) / 4, sum (map snd vs) / 4)

-- 辺の中点から外向きの点（外側の面へ向かう双対辺の折れ曲がり）
outward :: World -> Edge -> Point
outward w e =
  let (px, py) = pick (0, 0) (wNodes w) (eA e)
      (qx, qy) = pick (0, 0) (wNodes w) (eB e)
      mx = (px + qx) / 2
      my = (py + qy) / 2
      (cx, cy) = facePos w (if eF1 e == outerFace then eF2 e else eF1 e)
      dx = mx - cx
      dy = my - cy
      l = let v = sqrt (dx * dx + dy * dy) in if v > 0 then v else 1
  in (mx + dx / l * 50, my + dy / l * 50)

update :: World -> World
update w =
  let n = length (wNodes w)
      on = [ (i, e) | (i, e) <- zip [0 :: Int ..] (wEdges w), eOn e ]
      prs = map (ends . snd)
      cyc | isForest n (prs on) = []
          | otherwise = [ i | (i, e) <- on, connected n (prs (filter ((/= i) . fst) on)) (eA e) (eB e) ]
  in w { wCycle = cyc }

randomTree :: World -> IO World
randomTree w = do
  order <- shuffleIO [0 .. length (wEdges w) - 1]
  let es = wEdges w
      addE (uf, acc) i = case ufUnion uf (ends (pick (Edge 0 0 0 0 False) es i)) of
        Just uf' -> (uf', i : acc)
        Nothing -> (uf, acc)
      (_, chosen) = foldl' addE (ufNew (length (wNodes w)), []) order
  pure (update w { wEdges = [ e { eOn = i `elem` chosen } | (i, e) <- zip [0 ..] es ] })

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      node = pick (0, 0) (wNodes w)
      n = length (wNodes w)
      onE = filter eOn es
      offE = filter (not . eOn) es
      nOn = length onE
      hasCycle = not (null (wCycle w))
      rankT = forestRank n (map ends onE)
      isTree = not hasCycle && rankT == n - 1
      -- 双対の判定：補集合の双対辺が双対グラフで森か・全域木か
      dualPairs = map (\e -> (eF1 e, eF2 e)) offE
      dualForest = isForest nF dualPairs
      dualRank = forestRank nF dualPairs
      dualTree = dualForest && dualRank == nF - 1
      -- 面を塗る
      facePic vs = Color (hsb 280 30 20) (fillQuad (map (sp env . node) vs))
      edgePic i e =
        let c = if eOn e then hsb (if i `elem` wCycle w then 5 else 130) 85 100 else gray 0.4
        in seg env (if eOn e then 8 else 2) c (node (eA e)) (node (eB e))
      -- 双対グラフ：T に入っていない辺の双対辺を描く（T の辺の双対はうすく）
      dualEdge i e =
        let inCut = i `elem` wCycle w
            drawIt p q
              | not (eOn e) = seg env 5 (hsb 35 90 100) p q
              | inCut = dashSeg env 3 6 6 (hsba 5 80 100 0.7) p q
              | otherwise = dashSeg env 1.5 4 6 (hsba 35 40 60 0.4) p q
        in if eF1 e == outerFace || eF2 e == outerFace
             then let o = outward w e
                      inner = facePos w (if eF1 e == outerFace then eF2 e else eF1 e)
                      outer = facePos w outerFace
                  in Pictures [drawIt inner o, drawIt o outer]
             else drawIt (facePos w (eF1 e)) (facePos w (eF2 e))
      facePt f =
        let (cx, cy) = facePos w f
            hw = if f == outerFace then 21 else 13
        in Pictures
             [ box env white (cx - hw - 1) (cy - 14) (2 * hw + 2) 28
             , box env (hsb 35 80 95) (cx - hw) (cy - 13) (2 * hw) 26
             , txtC env cx cy 14 (gray 0.1) (if f == outerFace then "Out" else "F" ++ show (f + 1)) ]
      dualPics = if wDual w then zipWith dualEdge [0 ..] es ++ map facePt [0 .. nF - 1] else []
      px = envW env - 460
      py = 120
      status | nOn == 0 = ""
             | hasCycle = "has cycle (dependent)"
             | isTree = "spanning tree (basis)"
             | otherwise = "forest (not yet a tree)"
      note | hasCycle = [ "Dual edges (red dashes) of T's cycle are"
                        , "a cut: inside faces apart from outside."
                        , "Circuit <-> cut" ]
           | not dualForest = [ "Complement's duals contain a cycle:"
                              , "T is split (does not reach all nodes)." ]
           | isTree = [ "Remove an edge of T: T splits (a cut),"
                      , "and its dual closes a cycle over there." ]
           | otherwise = []
  in Pictures $
       map facePic (wFaces w) ++ zipWith edgePic [0 ..] es ++ dualPics ++
       map (ball env 12 (hsb 280 40 85)) (wNodes w) ++
       [ txt env 20 40 16 white ("Click edge: T in/out  Space: random tree  C: clear  D: dual " ++ (if wDual w then "ON" else "off") ++ "  Drag node  R: reset")
       , txt env 20 66 14 (gray 0.6) "Green = T. Orange = duals of edges not in T (they join faces). The outer face Out is one vertex."
       , box env (withA 0.08 white) (px - 20) (py - 40) 470 (envH env - py + 20)
       , stack env px py
           ([ (20, hsb 130 70 100, "T: " ++ show nOn ++ " edges")
            , (15, hsb 130 70 100, status)
            , (14, gray 0.7, "9 nodes, 12 edges, 5 faces (4 + Out).")
            , (14, gray 0.7, "A spanning tree has 8, complement 4.")
            , (6, white, "")
            , (20, hsb 35 80 100, "Dual: complement " ++ show (length offE))
            , (15, hsb 35 80 100, if dualForest then (if dualTree then "dual spanning tree" else "dual forest") else "dual has a cycle")
            , (14, gray 0.7, "Dual: 5 vertices, 12 edges. Trees: 4.")
            , (6, white, "")
            , (15, white, if isTree && dualTree then "Complement of basis = dual basis"
                          else if nOn == 0 then "Choose some edges" else "") ]
            ++ map (\s -> (14, gray 0.8, s)) note ++
            [ (8, white, "")
            , (16, white, "Dual matroid M*")
            , (14, gray 0.7, "M* bases = complements of M bases.")
            , (14, gray 0.7, "r*(E) = |E| - r(E) = 12 - 8 = 4")
            , (14, gray 0.7, "Circuits of M = cocircuits (cuts) of M*.")
            , (14, gray 0.7, "Planar: M* is graphic (the dual graph).")
            , (14, gray 0.7, "Nonplanar (K5, K3,3): it is not.") ])
       ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestIn 18 (map (dist2 (mouseP env)) (wNodes w)) }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env - 480) mx, clampF 90 (envH env - 100) my)
    in pure w { wNodes = setAt (wDrag w) (const p) (wNodes w) }
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w < 0 ->
        let node = pick (0, 0) (wNodes w)
            i = nearestIn 14 [ segDist (mouseP env) (node (eA e)) (node (eB e)) | e <- wEdges w ]
        in pure (if i >= 0 then update w { wEdges = setAt i (\e -> e { eOn = not (eOn e) }) (wEdges w) } else w)
    | otherwise -> pure w { wDrag = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> randomTree w
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure (update w { wEdges = map (\e -> e { eOn = False }) (wEdges w) })
    | c `elem` "dD" -> pure w { wDual = not (wDual w) }
    | c `elem` "rR" ->
        let w1 = buildLattice env w { wDrag = -1 }
        in pure (update w1 { wEdges = zipWith (\e old -> e { eOn = eOn old }) (wEdges w1) (wEdges w) })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. スペースで全域木を作り、オレンジの双対辺が「外」を含む 5 つの面をちょうど木でつなぐのを確かめる
-- 2. 全域木から辺を 1 本外す → 双対側に閉路ができる。1 本足す → 元のグラフに閉路、双対側はカット
-- 3. 格子を 2×2 や 4×4 に変える（buildLattice の 3 を変える。面の番号 cellId も直す）
-- 4. 双対の双対は元に戻る（M** = M）。表の式 r*(E) = |E| − r(E) をもう一度当てはめると？

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

-- 破線（on 描いて off あける）
dashSeg :: Env -> Float -> Float -> Float -> Color -> Point -> Point -> Picture
dashSeg env w on off c p@(x1, y1) q@(x2, y2) =
  let l = dist2 p q
      n = floor (l / (on + off)) :: Int
      pt d = (x1 + (x2 - x1) * d / max 1e-6 l, y1 + (y2 - y1) * d / max 1e-6 l)
  in Pictures [ seg env w c (pt d) (pt (min l (d + on))) | k <- [0 .. n], let d = fromIntegral k * (on + off), d < l ]

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

-- 小数 1 桁の文字列
f1 :: Float -> String
f1 x = showFFloat (Just 1) x ""

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

-- 四角形を塗る。gloss は凹んだ多角形を塗れないので、凹みの頂点を通る対角線で 2 つの三角形に分ける
fillQuad :: [Point] -> Picture
fillQuad [a, b, c, d] =
  let cross (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)
      area = cross a b c + cross a c d
      reflex p q r = cross p q r * area < 0
  in if reflex d a b || reflex b c d
       then Pictures [Polygon [a, b, c], Polygon [a, c, d]]
       else Pictures [Polygon [a, b, d], Polygon [b, c, d]]
fillQuad _ = Blank

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
