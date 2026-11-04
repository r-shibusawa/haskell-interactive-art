-- ============================================
-- 42-06 貪欲法 = クラスカル法
-- 重みつきグラフで「軽い辺から順に、閉路ができなければ採用」を繰り返すと最小全域木になる
--   これがマトロイド上の貪欲法：独立なら入れる、従属なら捨てる。それだけで最適
--   右の表は重み順の辺のリスト。全列挙で求めた本当の最小値とも比べる（いつも一致）
-- 操作：スペース = 1 ステップ / A = 自動 / ノードをドラッグ = 重み（長さ）が変わる / R = 最初から / N = 新しいグラフ
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, testBit)
import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM

nNode :: Int
nNode = 7                -- ★ ノードの数

targetEdges :: Int
targetEdges = 12         -- ★ 辺の本数

autoEvery :: Int
autoEvery = 25           -- ★ 自動のとき何フレームに 1 本試すか

data Edge = Edge { eA, eB, eW :: !Int }

data St = Untried | Taken | Dropped deriving (Eq)

data World = World
  { wNodes  :: ![Point]
  , wEdges  :: ![Edge]
  , wOrder  :: ![Int]        -- 重み順の辺番号
  , wStepI  :: !Int          -- 次に試す辺（wOrder の添字）
  , wStatus :: ![St]         -- 辺ごとの状態
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wBest   :: !(Maybe Int)  -- 全列挙で求めた最小全域木の重み
  , wDrag   :: !Int
  }

main :: IO ()
main = runSketch (sketch "42-06 Greedy = Kruskal" world0)
  { skBack  = hsb 100 25 9
  , skStart = \env -> newGraph env world0
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] [] 0 [] False 0 Nothing (-1)

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

newGraph :: Env -> World -> IO World
newGraph env w = do
  let placeNodes :: Int -> [Point] -> IO [Point]
      placeNodes tries ps
        | length ps >= nNode || tries <= 0 = pure ps
        | otherwise = do
            x <- randomRIO (80, envW env - 480)
            y <- randomRIO (130, envH env - 60)
            let ok = all (\q -> dist2 (x, y) q > 110) ps
            placeNodes (tries - 1) (if ok then ps ++ [(x, y)] else ps)
  ns <- placeNodes 500 []
  let n = length ns
      nodeOf = pick (0, 0) ns
      -- 近い順に辺を張る（各ノードから 2 本）＋ つながるまで足す
      nearOf i = take 2 (sortOn (\j -> dist2 (nodeOf i) (nodeOf j)) (filter (/= i) [0 .. n - 1]))
      es1 = foldl' addEdge [] [ (i, j) | i <- [0 .. n - 1], j <- nearOf i ]
      linkUp (uf, es) i
        | ufFind uf i /= ufFind uf 0 = (maybe uf id (ufUnion uf (0, i)), addEdge es (0, i))
        | otherwise = (uf, es)
      (_, es2) = foldl' linkUp (ufAll n es1, es1) [1 .. n - 1]
      fill :: Int -> [(Int, Int)] -> IO [(Int, Int)]
      fill guard es
        | length es >= targetEdges || guard <= 0 || n < 2 = pure es
        | otherwise = do
            a <- randomRIO (0, n - 1)
            b <- randomRIO (0, n - 1)
            fill (guard - 1) (addEdge es (a, b))
  es3 <- fill 100 es2
  pure (reset w { wNodes = ns, wEdges = map (\(a, b) -> Edge a b 0) es3 })

addEdge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addEdge es (a, b)
  | a == b || (a, b) `elem` es || (b, a) `elem` es = es
  | otherwise = es ++ [(a, b)]

updateWeights :: World -> World
updateWeights w =
  let node = pick (0, 0) (wNodes w)
  in w { wEdges = map (\e -> e { eW = roundI (dist2 (node (eA e)) (node (eB e)) / 10) }) (wEdges w) }

reset :: World -> World
reset w0 =
  let w = updateWeights w0
      es = wEdges w
  in w { wOrder = sortOn (\i -> eW (pick (Edge 0 0 0) es i)) [0 .. length es - 1]
       , wStatus = map (const Untried) es
       , wStepI = 0
       , wAuto = False
       , wBest = bruteForce w }

-- ★ 貪欲法の 1 ステップ：次に軽い辺を、閉路ができないなら採用
kruskalStep :: World -> World
kruskalStep w
  | wStepI w >= length (wOrder w) = w
  | otherwise =
      let es = wEdges w
          i = pick 0 (wOrder w) (wStepI w)
          edgeAt = ends . pick (Edge 0 0 0) es
          chosen = [ j | j <- take (wStepI w) (wOrder w), pick Untried (wStatus w) j == Taken ]
          ok = isForest (length (wNodes w)) (map edgeAt (chosen ++ [i]))
      in w { wStatus = setAt i (const (if ok then Taken else Dropped)) (wStatus w), wStepI = wStepI w + 1 }

-- 全列挙：すべての全域木の中で最小の重み（辺 <= 16 本くらいまで）
bruteForce :: World -> Maybe Int
bruteForce w =
  let n = length (wNodes w)
      es = wEdges w
      nE = length es
      trees = [ total
              | nE <= 18
              , m <- [0 .. (2 :: Int) ^ nE - 1]
              , popCount m == n - 1
              , let sub = [ e | (i, e) <- zip [0 ..] es, testBit m i ]
              , isForest n (map ends sub)
              , let total = sum (map eW sub) ]
  in case trees of
       [] -> Nothing
       _ -> Just (minimum trees)

step :: World -> World
step w
  | wAuto w =
      let t = wTimer w + 1
          w1 = w { wTimer = t }
      in if t `mod` autoEvery == 0 then kruskalStep w1 else w1
  | otherwise = w

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      status = pick Untried (wStatus w)
      node = pick (0, 0) (wNodes w)
      taken = [ e | (i, e) <- zip [0 ..] es, status i == Taken ]
      total = sum (map eW taken)
      nOk = length taken
      n = length (wNodes w)
      done = nOk == n - 1
      nOrder = length (wOrder w)
      current = if wStepI w < nOrder && not done then pick (-1) (wOrder w) (wStepI w) else -1
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            st = status i
            linePic = case st of
              Taken -> seg env 7 (hsb 130 85 100) p q
              Dropped -> dashSeg env 2 6 6 (hsba 5 70 70 0.6) p q
              Untried | i == current -> seg env 4 (hsb 50 90 100) p q
                      | otherwise -> seg env 2 (gray 0.4) p q
            tc | st == Taken = hsb 130 70 100
               | i == current = hsb 50 70 100
               | st == Dropped = gray 0.45
               | otherwise = white
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures [linePic, txtC env (mx + 8) (my - 8) 14 tc (show (eW e))]
      nodePic i p = Pictures [ ball env 14 (hsb 100 40 85) p, txtC env (fst p) (snd p) 14 (gray 0.1) (show (i + 1 :: Int)) ]
      px = envW env - 400
      py = 120
      bestStr = maybe "-" show (wBest w)
      same = wBest w == Just total
      row k i =
        let e = pick (Edge 0 0 0) es i
            st = status i
            isCur = k == wStepI w && not done
            c | st == Taken = hsb 130 75 100
              | st == Dropped = hsb 5 75 70
              | isCur = hsb 50 75 100
              | k > wStepI w = gray 0.6
              | otherwise = white
        in txt env px (py + 104 + fromIntegral k * 22) 15 c
             ((if isCur then "> " else "   ") ++ pad2 (eW e) ++ "   " ++ show (eA e + 1) ++ "-" ++ show (eB e + 1) ++ "   "
               ++ (case st of { Taken -> "taken"; Dropped -> "cycle: dropped"; Untried -> "" }))
  in Pictures $
       zipWith edgePic [0 ..] es ++ zipWith nodePic [0 ..] (wNodes w) ++
       [ txt env 20 40 16 white ("Space: 1 step  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  Drag node (weight = length)  R: restart  N: new graph")
       , txt env 20 66 14 (gray 0.6) "Lightest first. No cycle: take it (green). Cycle: drop it (red dashes). Only independence decides."
       , box env (withA 0.08 white) (px - 20) (py - 40) 390 (envH env - py + 20)
       , stack env px py
           [ (20, hsb (if done then 130 else 50) 80 100,
               if done then "Minimum tree done!" else "By weight (" ++ show (wStepI w) ++ " / " ++ show (length es) ++ ")")
           , (14, white, "Taken total " ++ show total ++ "  (" ++ show nOk ++ " / " ++ show (n - 1) ++ " edges)")
           , (14, if done then hsb (if same then 130 else 5) 80 100 else gray 0.7, "Brute-force best: " ++ bestStr)
           , (14, if done then hsb (if same then 130 else 5) 80 100 else gray 0.7,
               if done then (if same then "Match: greedy is optimal" else "?") else "") ] ] ++
       zipWith row [0 ..] (wOrder w)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestIn 20 (map (dist2 (mouseP env)) (wNodes w)) }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env - 430) mx, clampF 90 (envH env - 20) my)
    in pure (updateWeights w { wNodes = setAt (wDrag w) (const p) (wNodes w) })
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w >= 0 -> pure ((reset w) { wDrag = -1 })
    | otherwise -> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (kruskalStep w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> pure (reset w)
    | c `elem` "nN" -> newGraph env w { wDrag = -1 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 全部終えたら、捨てられた辺を 1 本選んで「この辺を木に入れるなら、代わりにどの辺を外すか」を考える。外す辺は必ず重い？
-- 2. ノードをドラッグして重みを変え、木がどう変わるかを見る。1 つの辺の重みだけが変わると、木の変化は 1 本ずつ
-- 3. 「重い辺から順に、閉路ができないなら採用」にすると最大全域木。コードの sortOn を逆にしてみよう
-- 4. 貪欲法が最適になる理由を 07・08 で確かめる。「独立かどうか」で決めてよいのはマトロイドのときだけ

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

-- 2 桁にそろえる（p5.js の nf(n, 2)）
pad2 :: Int -> String
pad2 n = (if n < 10 then "0" else "") ++ show n

-- 四捨五入（Math.round）
roundI :: Float -> Int
roundI x = floor (x + 0.5)

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
