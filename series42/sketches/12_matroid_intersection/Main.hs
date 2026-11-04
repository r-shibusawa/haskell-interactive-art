-- ============================================
-- 42-12 マトロイド交差：二部グラフのマッチング
-- 二部グラフの辺集合に 2 つの分割マトロイドをのせる：M1 = 「左の各頂点から 1 本まで」、M2 = 「右の各頂点から 1 本まで」
--   両方で独立な集合（共通独立集合）= マッチング。最大のものを探すのが「マトロイド交差問題」
--   増加路：使っていない左頂点から、未使用辺 → 使用辺 → 未使用辺 … と交互にたどって未使用の右頂点に着く道。裏返すと 1 本増える
-- 操作：辺をクリック = 入れる・外す（両方のマトロイドで検査） / スペース = 増加路を探す → もう一度で裏返す / A = 自動 / C = 全部外す / R = 新しいグラフ
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.List (foldl', intercalate, nub, sortOn)

nL, nR :: Int
nL = 4                   -- ★ 左の頂点数
nR = 4                   -- ★ 右の頂点数

data Edge = Edge { eA, eB :: !Int, eOn :: !Bool }   -- a = 左, b = 右

data World = World
  { wEdges :: ![Edge]
  , wPath  :: !(Maybe [Int])   -- 見つけた増加路（辺番号の列）
  , wAuto  :: !Bool
  , wTimer :: !Int
  , wMax   :: !Int             -- 全列挙で求めた最大マッチング
  }

main :: IO ()
main = runSketch (sketch "42-12 Matroid intersection" world0)
  { skBack  = hsb 190 30 9
  , skStart = \_ -> randomGraph world0
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] Nothing False 0 0

noEdge :: Edge
noEdge = Edge 0 0 False

posL, posR :: Env -> Int -> Point
posL env i = (envW env * 0.18, remap (fromIntegral i) 0 (fromIntegral (nL - 1)) (envH env * 0.3) (envH env * 0.85))
posR env j = (envW env * 0.46, remap (fromIntegral j) 0 (fromIntegral (nR - 1)) (envH env * 0.3) (envH env * 0.85))

randomGraph :: World -> IO World
randomGraph w = do
  firstE <- concat <$> mapM (\i -> do js <- shuffleIO [0 .. nR - 1]
                                      pure [ Edge i j False | j <- take 2 js ]) [0 .. nL - 1]
  extra <- concat <$> mapM (\j -> if any ((== j) . eB) firstE then pure []
                                  else do a <- randomRIO (0, nL - 1)
                                          pure [Edge a j False]) [0 .. nR - 1]
  let es = sortOn (\e -> (eA e, eB e)) (firstE ++ extra)
      n = length es
      -- 全列挙で最大
      best = foldl' max 0
        [ length sub | m <- [0 .. (2 :: Int) ^ n - 1]
        , let sub = [ e | (i, e) <- zip [0 ..] es, testBit m i ]
        , distinct (map eA sub), distinct (map eB sub) ]
  pure w { wEdges = es, wPath = Nothing, wAuto = False, wMax = best }

distinct :: [Int] -> Bool
distinct xs = length (nub xs) == length xs

-- ★ 2 つのマトロイド（どちらも分割マトロイド）：2 本以上ある頂点
badOf :: (Edge -> Int) -> [Edge] -> [Int]
badOf f on = [ v | v <- nub (map f on), length (filter ((== v) . f) on) > 1 ]

-- ★ 増加路を BFS で探す（マッチングのときだけ）
findPath :: World -> Maybe [Int]
findPath w =
  let es = zip [0 ..] (wEdges w)
      on = filter (eOn . snd) es
      matchedL = IM.fromList [ (eA e, i) | (i, e) <- on ]
      matchedR = IM.fromList [ (eB e, i) | (i, e) <- on ]
      edge = pick noEdge (wEdges w)
      -- 左の頂点 s から出発する BFS
      bfs s = go [s] (IM.singleton s (-1)) IM.empty
      -- prev[左] = そこへ来た使用辺（出発点は -1）、seenR[右] = そこへ着いた未使用辺
      go [] _ _ = Nothing
      go (u : queue) prev seenR = visit u queue prev seenR [ (i, e) | (i, e) <- es, eA e == u, not (eOn e) ]
      visit _ queue prev seenR [] = go queue prev seenR
      visit u queue prev seenR ((i, e) : rest)
        | IM.member (eB e) seenR = visit u queue prev seenR rest
        | otherwise =
            let seen' = IM.insert (eB e) i seenR
            in case IM.lookup (eB e) matchedR of
                 Nothing -> Just (reverse (i : back prev seen' u))   -- 未使用の右頂点に着いた：道を復元
                 Just m ->
                   let nxt = eA (edge m)
                   in if IM.member nxt prev
                        then visit u queue prev seen' rest
                        else visit u (queue ++ [nxt]) (IM.insert nxt m prev) seen' rest
      -- 使用辺 → それに着いた未使用辺 → … と戻る
      back prev seenR v = case IM.lookup v prev of
        Just m | m >= 0 ->
          let i2 = IM.findWithDefault 0 (eB (edge m)) seenR
          in m : i2 : back prev seenR (eA (edge i2))
        _ -> []
      starts = [ s | s <- [0 .. nL - 1], not (IM.member s matchedL) ]
      matching = distinct (map (eA . snd) on) && distinct (map (eB . snd) on)
  in if matching then listToMaybe (mapMaybe bfs starts) else Nothing

applyPath :: World -> World
applyPath w = case wPath w of
  Nothing -> w
  Just p -> w { wEdges = [ if i `elem` p then e { eOn = not (eOn e) } else e | (i, e) <- zip [0 ..] (wEdges w) ], wPath = Nothing }

stepAlgo :: World -> World
stepAlgo w = if isJust (wPath w) then applyPath w else w { wPath = findPath w }

step :: World -> World
step w
  | wAuto w =
      let t = wTimer w + 1
          w1 = w { wTimer = t }
      in if t `mod` 30 /= 0 then w1
         else if not (isJust (wPath w1)) && not (isJust (findPath w1)) then w1 { wAuto = False }
         else stepAlgo w1
  | otherwise = w

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      on = filter eOn es
      b1 = badOf eA on
      b2 = badOf eB on
      ok1 = null b1
      ok2 = null b2
      path = maybe [] id (wPath w)
      edgePic i e =
        let p = posL env (eA e)
            q = posR env (eB e)
            inPath = i `elem` path
            bad = (eA e `elem` b1 || eB e `elem` b2) && eOn e
        in if inPath then seg env (if eOn e then 10 else 6) (hsb 50 90 100) p q
           else if eOn e then seg env 8 (hsb (if bad then 5 else 130) 85 100) p q
           else seg env 2.5 (gray 0.4) p q
      leftPic i =
        let p = posL env i
            h | i `elem` b1 = 5
              | any ((== i) . eA) on = 130
              | otherwise = 190
        in Pictures [ ball env 17 (hsb h 60 85) p, txtC env (fst p) (snd p) 14 (gray 0.1) ("L" ++ show (i + 1)) ]
      rightPic j =
        let q@(x, y) = posR env j
            h | j `elem` b2 = 5
              | any ((== j) . eB) on = 130
              | otherwise = 300
        in Pictures [ box env white (x - 18) (y - 18) 36 36, box env (hsb h 60 85) (x - 17) (y - 17) 34 34
                    , txtC env x y 14 (gray 0.1) ("R" ++ show (j + 1)) ]
      px = envW env - 470
      py = 120
      nOn = length on
      isBest = nOn == wMax w && ok1 && ok2
      edgeName i = let e = pick noEdge es i in "L" ++ show (eA e + 1) ++ "-R" ++ show (eB e + 1)
      pathRows = case wPath w of
        Just p -> [ (14, hsb 50 90 100, "Augmenting path:") ]
                  ++ map (\s -> (14, hsb 50 90 100, s)) (chunk3 (map edgeName p))
                  ++ [ (14, hsb 50 90 100, "(space again to flip)") ]
        Nothing
          | ok1 && ok2 && nOn < wMax w -> [ (15, hsb 50 90 100, "Space: find an augmenting path") ]
          | ok1 && ok2 -> [ (15, hsb 50 90 100, "No path -> this is a maximum") ]
          | otherwise -> []
      badNames pre vs = intercalate ", " (map (\v -> pre ++ show (v + 1)) vs)
  in Pictures $
       zipWith edgePic [0 ..] es ++ map leftPic [0 .. nL - 1] ++ map rightPic [0 .. nR - 1] ++
       [ txt env 20 40 16 white ("Click edge: in/out  Space: find path, then flip  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  C: clear  R: new graph")
       , txt env 20 66 14 (gray 0.6) "M1: at most 1 per left circle. M2: at most 1 per right square. Both = matching. Yellow = augmenting path."
       , box env (withA 0.08 white) (px - 20) (py - 40) 480 (envH env - py + 20)
       , stack env px py
           ([ (14, hsb (if ok1 then 130 else 5) 80 100, "M1 (left): " ++ (if ok1 then "independent" else "dep, 2+ at " ++ badNames "L" b1))
            , (14, hsb (if ok2 then 130 else 5) 80 100, "M2 (right): " ++ (if ok2 then "independent" else "dep, 2+ at " ++ badNames "R" b2))
            , (18, white, (if ok1 && ok2 then "Common independent set" else "Not common independent"))
            , (18, white, "|I| = " ++ show nOn)
            , (16, if isBest then hsb 130 80 100 else white, "Maximum = " ++ show (wMax w) ++ (if isBest then "  -> largest set!" else "")) ]
            ++ pathRows ++
            [ (6, white, "")
            , (14, gray 0.7, "A path alternates unused, used, unused...")
            , (14, gray 0.7, "Flip it: one more used edge, and both")
            , (14, gray 0.7, "independences are kept.")
            , (6, white, "")
            , (16, white, "Intersection theorem (Edmonds)")
            , (14, gray 0.7, "max |I| = min_A r1(A) + r2(E\\A)")
            , (14, gray 0.7, "Bipartite: Konig (max matching =")
            , (14, gray 0.7, "min vertex cover). Two general matroids:")
            , (14, gray 0.7, "polynomial time. Three: NP-hard.")
            , (14, gray 0.7, "Uses: arborescences, splitting into two")
            , (14, gray 0.7, "trees, assignment problems.") ])
       ]

-- 3 つずつに分けて 1 行にする
chunk3 :: [String] -> [String]
chunk3 [] = []
chunk3 xs = intercalate " -> " (take 3 xs) : chunk3 (drop 3 xs)

nearestEdge :: Env -> World -> Int
nearestEdge env w =
  let m@(mx, my) = mouseP env
      d e =
        let (x1, y1) = posL env (eA e)
            (x2, y2) = posR env (eB e)
            dx = x2 - x1
            dy = y2 - y1
            l2 = dx * dx + dy * dy
            t = if l2 > 0 then clampF 0.05 0.95 (((mx - x1) * dx + (my - y1) * dy) / l2) else 0
        in dist2 m (x1 + t * dx, y1 + t * dy)
  in nearestIn 12 (map d (wEdges w))

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let i = nearestEdge env w
    in pure (if i >= 0 then w { wEdges = setAt i (\e -> e { eOn = not (eOn e) }) (wEdges w), wPath = Nothing } else w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (stepAlgo w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "cC" -> pure w { wEdges = map (\e -> e { eOn = False }) (wEdges w), wPath = Nothing }
    | c `elem` "rR" -> randomGraph w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 手で「これ以上足せない」マッチングを作ってから、スペースで増加路を探す。極大でも最大とは限らない
-- 2. 増加路の長さ（辺の数）はいつも奇数。1 本の道（長さ 1）はただの「空いている辺」
-- 3. 左右の頂点数を 5 にする（nL, nR）。全列挙の最大（2^辺数）が重くなるのはどのへん？
-- 4. 1 つのマトロイドだけなら貪欲法で最大（06）。2 つの共通部分になると貪欲は失敗する例を作ってみよう（07 のマッチングと同じ）

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
