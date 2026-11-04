-- ============================================
-- 42-04 基とサーキット
-- グラフ的マトロイドの「基」（= 全域木、最大の独立集合）と「サーキット」（= 閉路、最小の従属集合）を全部列挙して順に見る
--   基交換公理：基 B1 から辺 e を抜いても、別の基 B2 の辺 f を入れれば また基になる（スペースでアニメーション）
--   基はどれも同じ大きさ（= 階数）。サーキットから 1 本抜くと必ず独立
-- 操作：←→ = 次の基／サーキット / B・K = 基 ⇄ サーキットの表示 / スペース = 基の交換 / ノードをドラッグ / R = ランダムなグラフ / 1 = K4
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.List (elemIndex, foldl', intercalate, nub, sort, sortOn)
import qualified Data.IntMap.Strict as IM

data Edge = Edge { eA, eB :: !Int }

-- 交換アニメーション
data Anim = Anim { anTo, anE, anF :: !Int, anT :: !Float, anCands :: ![Int] }

data World = World
  { wNodes    :: ![Point]
  , wEdges    :: ![Edge]
  , wBases    :: ![[Int]]   -- 基（辺番号のリスト）のリスト
  , wCircuits :: ![[Int]]
  , wView     :: !Char      -- 'B' = 基, 'K' = サーキット
  , wIdx      :: !Int
  , wDrag     :: !Int
  , wAnim     :: !(Maybe Anim)
  }

main :: IO ()
main = runSketch (sketch "42-04 Bases and circuits" world0)
  { skBack  = hsb 30 30 9
  , skStart = \env -> pure (makeK4 env world0)
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] [] [] 'B' 0 (-1) Nothing

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

makeK4 :: Env -> World -> World
makeK4 env w =
  let cx = envW env * 0.33
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.26
      a i = tau * fromIntegral i / 4 - pi / 2 + pi / 4
      ns = [ (cx + r * cos (a i), cy + r * sin (a i)) | i <- [0 .. 3 :: Int] ]
      es = [ Edge p q | p <- [0 .. 3], q <- [p + 1 .. 3] ]
  in enumerateAll w { wNodes = ns, wEdges = es }

randomGraph :: Env -> World -> IO World
randomGraph env w = do
  let n = 5 :: Int
      m = 7 :: Int
      cx = envW env * 0.33
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.27
      node i = do
        let a = tau * fromIntegral i / fromIntegral n - pi / 2
        jx <- randomRIO (-20, 20)
        jy <- randomRIO (-20, 20)
        pure (cx + r * cos a + jx, cy + r * sin a + jy)
      more :: Int -> [Edge] -> IO [Edge]
      more tries es
        | length es >= m || tries <= 0 = pure es
        | otherwise = do
            a <- randomRIO (0, n - 1)
            b <- randomRIO (0, n - 1)
            let dup = any (\e -> ends e == (a, b) || ends e == (b, a)) es
            more (tries - 1) (if a == b || dup then es else es ++ [Edge a b])
  ns <- mapM node [0 .. n - 1]
  order <- shuffleIO [0 .. n - 1]
  treeE <- mapM (\i -> do k <- randomRIO (0, i - 1)
                          pure (Edge (pick 0 order i) (pick 0 order k))) [1 .. n - 1]
  es <- more 200 treeE
  pure (enumerateAll w { wNodes = ns, wEdges = es })

-- ★ 全部の部分集合を調べて、基とサーキットを集める
enumerateAll :: World -> World
enumerateAll w =
  let n = length (wNodes w)
      es = wEdges w
      nE = length es
      prs sub = map (ends . pick (Edge 0 0) es) sub
      rk = forestRank n (map ends es)
      subsets = [ [ i | i <- [0 .. nE - 1], testBit m i ] | m <- [1 .. (2 :: Int) ^ nE - 1] ]
      bases = [ s | s <- subsets, length s == rk, isForest n (prs s) ]
      -- 最小の従属集合 = どの 1 本を抜いても独立
      circuits = [ s | s <- subsets, not (isForest n (prs s))
                     , all (\i -> isForest n (prs (filter (/= i) s))) s ]
  in w { wBases = bases, wCircuits = circuits, wIdx = 0, wAnim = Nothing }

currentList :: World -> [[Int]]
currentList w = if wView w == 'B' then wBases w else wCircuits w

-- ★ 基の交換：B1 から e を抜き、入れると基になる f を探す
startExchange :: World -> IO World
startExchange w = case (wView w, pick [] (wBases w) (wIdx w)) of
  ('B', b1@(_ : _)) -> do
    ei <- randomRIO (0, length b1 - 1)
    let e = pick 0 b1 ei
        rest = filter (/= e) b1
        n = length (wNodes w)
        es = wEdges w
        prs sub = map (ends . pick (Edge 0 0) es) sub
        cands = [ f | f <- [0 .. length es - 1], f `notElem` b1, isForest n (prs (rest ++ [f])) ]
    case cands of
      [] -> pure w
      _ -> do
        fi <- randomRIO (0, length cands - 1)
        let f = pick 0 cands fi
            b2 = sort (rest ++ [f])
            to = fromMaybe (wIdx w) (elemIndex b2 (wBases w))
        pure w { wAnim = Just (Anim to e f 0 cands) }
  _ -> pure w

step :: World -> World
step w = case wAnim w of
  Just a | anT a + 1 / 90 >= 1 -> w { wIdx = anTo a, wAnim = Nothing }
         | otherwise -> w { wAnim = Just a { anT = anT a + 1 / 90 } }
  Nothing -> w

draw :: Env -> World -> Picture
draw env w =
  let list = currentList w
      nList = length list
      idx = clampI 0 (max 0 (nList - 1)) (wIdx w)
      cur = pick [] list idx
      isB = wView w == 'B'
      hueV = if isB then 130 else 5
      node = pick (0, 0) (wNodes w)
      -- 辺
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            on0 = i `elem` cur
            base = (if on0 then hsb hueV 85 100 else gray 0.45, if on0 then 7 else 2.5, on0)
            (c, wgt, on) = case wAnim w of
              Just a
                | i == anE a -> (hsb 5 90 100, lerpF 7 1 (anT a), on0)
                | i == anF a -> (hsb 50 90 100, lerpF 1 7 (anT a), True)
                | i `elem` anCands a && not on0 -> (hsb 50 50 70, 3, on0)
              _ -> base
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures [ seg env wgt c p q
                    , txtC env (mx + 10) (my - 10) 14 (gray (if on then 1 else 0.6)) (eName i) ]
      nodePic i p = Pictures [ ball env 15 (hsb 30 40 85) p, txtC env (fst p) (snd p) 14 (gray 0.1) (show (i + 1 :: Int)) ]
      -- 右のリスト
      px = envW env - 400
      py = 120
      rowH = 22
      maxRows = floor ((envH env - py - 140) / rowH) :: Int
      listRow k s =
        let on = k == idx
        in txt env px (py + 120 + fromIntegral k * rowH) 15
             (if on then hsb hueV 80 100 else gray 0.65)
             ((if on then "> " else "   ") ++ unwords (map eName s))
      sizes = sort (nub (map length (wCircuits w)))
      animText = case wAnim w of
        Just a ->
          [ (15, hsb 50 90 100, "Drop " ++ eName (anE a) ++ ", add " ++ eName (anF a))
          , (14, gray 0.8, "Candidates: " ++ unwords (map eName (anCands a)))
          , (14, gray 0.7, "B1 - e + f is a basis again") ]
        Nothing -> []
  in Pictures $
       zipWith edgePic [0 ..] (wEdges w) ++ zipWith nodePic [0 ..] (wNodes w) ++
       [ txt env 20 40 16 white "<- ->: next  B: bases  K: circuits  Space: exchange  Drag node  R: random  1: K4"
       , txt env 20 66 14 (gray 0.6) "Basis = cannot add more (all same size).  Circuit = cannot remove any (drop one edge: a forest)."
       , box env (withA 0.08 white) (px - 20) (py - 40) 390 (envH env - py + 20)
       , stack env px py
           ([ (20, hsb hueV 80 100, (if isB then "Bases (trees)  " else "Circuits (cycles)  ") ++ show (idx + 1) ++ " / " ++ show nList)
            , (14, white, if isB then "Rank = " ++ show (length (pick [] (wBases w) 0)) ++ " (same for all bases)"
                                 else "Circuit sizes: " ++ intercalate ", " (map show sizes)) ]
            ++ animText) ] ++
       zipWith listRow [0 ..] (take maxRows list) ++
       [ txt env px (py + 120 + fromIntegral maxRows * rowH) 15 (gray 0.5) ("... " ++ show (nList - maxRows) ++ " more") | nList > maxRows ]

eName :: Int -> String
eName i = "e" ++ show (i + 1)

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestIn 20 (map (dist2 (mouseP env)) (wNodes w)) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env - 420) mx, clampF 90 (envH env - 20) my)
    in pure w { wNodes = setAt (wDrag w) (const p) (wNodes w) }
  EventKey (SpecialKey KeyRight) Down _ _ | n > 0 -> pure w { wIdx = (wIdx w + 1) `mod` n, wAnim = Nothing }
  EventKey (SpecialKey KeyLeft) Down _ _ | n > 0 -> pure w { wIdx = (wIdx w + n - 1) `mod` n, wAnim = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> startExchange w
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> pure w { wView = 'B', wIdx = 0, wAnim = Nothing }
    | c `elem` "kK" -> pure w { wView = 'K', wIdx = 0, wAnim = Nothing }
    | c `elem` "rR" -> randomGraph env w { wDrag = -1 }
    | c == '1' -> pure (makeK4 env w { wDrag = -1 })
  _ -> pure w
  where n = length (currentList w)

-- ---------- やってみよう ----------
-- 1. K4 の基は 16 個、サーキットは 7 個（三角形 4 つと四角形 3 つ）。全部めくって確かめよう
-- 2. スペースを何回も押す。抜いた辺によって「入れられる候補」の数が変わる。候補が 1 本だけのときはどんなとき？
-- 3. R で辺が 1 本だけのグラフや、ぶら下がりの辺があるグラフが出たら、その辺はどの基にも入っている（コループ）
-- 4. 基の補集合（入っていない辺）を全部並べると、それも「別のマトロイド」の基になっている（09 の双対）

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
