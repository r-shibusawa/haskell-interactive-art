-- ============================================
-- 42-10 削除と縮約（マイナー）
-- 辺 e を「削除」（M∖e：辺を消す）または「縮約」（M/e：辺を縮めて両端をひとつの点にする）して、小さなマトロイドを作る
--   削除と縮約を繰り返してできるものが「マイナー」。縮約すると平行な辺はループ（1 本で従属）になる
--   グラフ的マトロイドは GF(2) で表現できる = U2,4 をマイナーに持たない（Tutte の定理）
-- 操作：辺をクリック = 操作する / D・K = 削除モード・縮約モード / U = 元に戻す / R = 最初から / ノードをドラッグ
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import qualified Data.Map.Strict as M
import Control.Monad (foldM)
import Data.List (foldl', intercalate, mapAccumL)
import qualified Data.IntMap.Strict as IM

data Edge = Edge { eA, eB :: !Int, eName :: String }

-- 縮約のアニメーション
data Anim = Anim { anFrom, anTo, anEdge :: !Int, anT :: !Float, anStart :: !Point }

-- 元に戻す用の記録
data Snap = Snap [Point] [Edge] [String]

data Stats = Stats { sRank, sBases, sLoops, sParallel :: !Int, sFam :: ![Int] }

data World = World
  { wNodes :: ![Point]
  , wEdges :: ![Edge]
  , wMode  :: !Char      -- 'D' = 削除, 'K' = 縮約
  , wStack :: ![Snap]
  , wOps   :: ![String]  -- 操作の記録（表示用）
  , wAnim  :: !(Maybe Anim)
  , wDrag  :: !Int
  , wStats :: !Stats
  }

main :: IO ()
main = runSketch (sketch "42-10 Deletion and contraction" world0)
  { skBack  = hsb 20 30 9
  , skStart = \env -> pure (resetGraph env world0)
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] 'D' [] [] Nothing (-1) (Stats 0 0 0 0 [])

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

resetGraph :: Env -> World -> World
resetGraph env w =
  -- ★ 最初のグラフ：5 頂点、7 辺（四角形 ＋ 対角線 ＋ ぶら下がり）
  let cx = envW env * 0.3
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.24
      a i = tau * fromIntegral i / 4 + pi / 4
      ns = [ (cx + r * cos (a i), cy + r * sin (a i)) | i <- [0 .. 3 :: Int] ] ++ [(cx + r * 1.9, cy)]
      es = [ Edge p q ("e" ++ show k)
           | (k, (p, q)) <- zip [1 :: Int ..] [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (1, 3), (1, 4)] ]
  in computeStats w { wNodes = ns, wEdges = es, wStack = [], wOps = [], wAnim = Nothing, wDrag = -1 }

snapshot :: World -> World
snapshot w = w { wStack = take 30 (Snap (wNodes w) (wEdges w) (wOps w) : wStack w) }

removeAt :: Int -> [a] -> [a]
removeAt i xs = [ x | (k, x) <- zip [0 ..] xs, k /= i ]

-- ★ 削除：辺を消すだけ
deleteEdge :: Int -> World -> World
deleteEdge i w0 =
  let w = snapshot w0
      name = maybe "" eName (safeEdge w i)
  in computeStats w { wOps = wOps w ++ ["\\" ++ name], wEdges = removeAt i (wEdges w) }

safeEdge :: World -> Int -> Maybe Edge
safeEdge w i = case drop i (wEdges w) of
  (e : _) | i >= 0 -> Just e
  _ -> Nothing

-- ★ 縮約：両端をひとつにする（アニメーションの後で finishContract）
contractEdge :: Int -> World -> World
contractEdge i w = case safeEdge w i of
  Nothing -> w
  Just e
    | eA e == eB e -> deleteEdge i w        -- ループの縮約は削除と同じ
    | otherwise ->
        let w1 = snapshot w
        in w1 { wOps = wOps w1 ++ ["/" ++ eName e]
              , wAnim = Just (Anim (eB e) (eA e) i 0 (pick (0, 0) (wNodes w) (eB e))) }

finishContract :: Anim -> World -> World
finishContract a w =
  let from = anFrom a
      to = anTo a
      merge v = if v == from then to else v
      shiftV v = if v > from then v - 1 else v
      fix e = e { eA = shiftV (merge (eA e)), eB = shiftV (merge (eB e)) }
  in computeStats w { wEdges = map fix (removeAt (anEdge a) (wEdges w))
                    , wNodes = removeAt from (wNodes w)
                    , wAnim = Nothing }

computeStats :: World -> World
computeStats w =
  let n = length (wNodes w)
      es = wEdges w
      nE = length es
      rk = forestRank n (map ends es)
      forests = [ length sub | m <- [0 .. (2 :: Int) ^ nE - 1]
                , let sub = [ ends e | (i, e) <- zip [0 ..] es, testBit m i ]
                , isForest n sub ]
      fam = [ length (filter (== s) forests) | s <- [0 .. nE] ]
      loops = length (filter (\e -> eA e == eB e) es)
      pairs = [ (p, q) | (i, p) <- zip [0 :: Int ..] es, (j, q) <- zip [0 ..] es, j > i ]
      par (p, q) = eA p /= eB p && (ends p == ends q || ends p == (eB q, eA q))
  in w { wStats = Stats rk (length (filter (== rk) forests)) loops (length (filter par pairs)) fam }

step :: World -> World
step w = case wAnim w of
  Nothing -> w
  Just a ->
    let t = anT a + 1 / 40
        k = min 1 t
        (sx, sy) = anStart a
        (tx, ty) = pick (0, 0) (wNodes w) (anTo a)
        w1 = w { wNodes = setAt (anFrom a) (const (lerpF sx tx k, lerpF sy ty k)) (wNodes w) }
    in if t >= 1 then finishContract a w1 else w1 { wAnim = Just a { anT = t } }

-- 平行辺を少しずらす量（同じ組の何本目か × 18）
edgeOffsets :: [Edge] -> [Float]
edgeOffsets = snd . mapAccumL count M.empty
  where
    count seen e =
      let k = (min (eA e) (eB e), max (eA e) (eB e))
          c = M.findWithDefault 0 k seen + 1 :: Int
      in (M.insert k c seen, fromIntegral (c - 1) * 18)

bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  [ (b x0 x1 x2 x3 t, b y0 y1 y2 y3 t) | k <- [0 .. 16 :: Int], let t = fromIntegral k / 16 ]
  where b a c d e t = let u = 1 - t in u * u * u * a + 3 * u * u * t * c + 3 * u * t * t * d + t * t * t * e

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      node = pick (0, 0) (wNodes w)
      hov = case wAnim w of { Just _ -> -1; Nothing -> nearestEdge env w }
      st = wStats w
      edgePic i e off =
        let p@(px, py) = node (eA e)
            q@(qx, qy) = node (eB e)
            isH = hov == i
            c = if isH then hsb (if wMode w == 'D' then 5 else 50) 90 100 else hsb 20 25 75
            wgt = if isH then 7 else 4
            nx = -(qy - py)
            ny = qx - px
            l = let v = sqrt (nx * nx + ny * ny) in if v > 0 then v else 1
            cp1 = (px + nx / l * off * 2, py + ny / l * off * 2)
            cp2 = (qx + nx / l * off * 2, qy + ny / l * off * 2)
            shape | eA e == eB e = Color c (at (sp env (px + 26, py - 26)) (ring 20 wgt))   -- ループ
                  | otherwise = Color c (thickPath wgt (map (sp env) (bezierPts p cp1 cp2 q)))
            mx = if eA e == eB e then px + 26 else (px + qx) / 2
            my = if eA e == eB e then py - 54 else (py + qy) / 2
        in Pictures [shape, txtC env (mx + 10) (my - 10) 14 white (eName e)]
      animLine = case wAnim w of
        Just a -> seg env 8 (hsb 50 90 100) (node (anFrom a)) (node (anTo a))
        Nothing -> Blank
      nodePic i p = Pictures [ ball env 13 (hsb 20 40 85) p, txtC env (fst p) (snd p) 14 (gray 0.1) (show (i + 1 :: Int)) ]
      px0 = envW env - 460
      py0 = 110
      is4 = length es == 4 && sRank st == 2
      opsStr = if null (wOps w) then "M" else "M " ++ unwords (wOps w)
      fam2 = pick 0 (sFam st) 2
  in Pictures $
       zipWith3 edgePic [0 ..] es (edgeOffsets es) ++ [animLine] ++ zipWith nodePic [0 ..] (wNodes w) ++
       [ txt env 20 40 16 white ("Click edge: " ++ (if wMode w == 'D' then "delete M\\e" else "contract M/e") ++ "  D: delete mode  K: contract mode  U: undo  R: reset  Drag node")
       , txt env 20 66 14 (gray 0.6) "Delete = remove the edge. Contract = merge both ends (parallel edges become loops). Still a matroid."
       , box env (withA 0.08 white) (px0 - 20) (py0 - 40) 470 (envH env - py0 + 20)
       , stack env px0 py0
           [ (16, hsb 50 80 100, opsStr)
           , (16, white, "|E| = " ++ show (length es) ++ "  rank " ++ show (sRank st) ++ "  bases " ++ show (sBases st))
           , (16, white, "loops " ++ show (sLoops st) ++ "  parallel pairs " ++ show (sParallel st))
           , (14, gray 0.7, "Indep. sets by size: " ++ intercalate ", " (map show (take (sRank st + 1) (sFam st))))
           , (14, gray 0.7, "Delete: rank same or -1.")
           , (14, gray 0.7, "Contract: rank -1 (unless a loop).")
           , (8, white, "")
           , (16, white, "Minors and representability")
           , (14, gray 0.7, "U2,4: 4 elements, any 2 independent")
           , (14, gray 0.7, "(4 points on a line). GF(2) lines hold")
           , (14, gray 0.7, "only 3 points, so U2,4 is not binary,")
           , (14, gray 0.7, "and minors keep that property.")
           , (14, gray 0.7, "Tutte: M binary <=> no U2,4 minor.")
           , (14, gray 0.7, "Graphic matroids are binary (02): U2,4")
           , (14, gray 0.7, "never appears. Try 4 elements, rank 2:")
           , (14, if is4 then hsb 50 80 100 else gray 0.6,
               if is4 then "|E| = 4, rank 2: indep. pairs " ++ show fam2 ++ " / 6"
                      else "(now |E| = " ++ show (length es) ++ ", rank " ++ show (sRank st) ++ ")")
           , (14, if is4 then hsb 50 80 100 else gray 0.6, if is4 then "(U2,4 would give 6)" else "")
           , (14, gray 0.7, "Graphic <=> no minor U2,4, F7, F7*,")
           , (14, gray 0.7, "M*(K5), M*(K3,3)")
           ]
       ]

nearestEdge :: Env -> World -> Int
nearestEdge env w =
  let m = mouseP env
      node = pick (0, 0) (wNodes w)
      d e off =
        let p@(px, py) = node (eA e)
            q@(qx, qy) = node (eB e)
            nx = -(qy - py)
            ny = qx - px
            l = let v = sqrt (nx * nx + ny * ny) in if v > 0 then v else 1
            c = ((px + qx) / 2 + nx / l * off * 1.5, (py + qy) / 2 + ny / l * off * 1.5)
        in if eA e == eB e then abs (dist2 m (px + 26, py - 26) - 20)
           else if off > 0 then dist2 m c
           else segDist m p q
  in nearestIn 16 (zipWith d (wEdges w) (edgeOffsets (wEdges w)))

noAnim :: World -> Bool
noAnim w = case wAnim w of { Nothing -> True; Just _ -> False }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = nearestIn 18 (map (dist2 (mouseP env)) (wNodes w)) }
  EventMotion _ | wDrag w >= 0 && noAnim w ->
    let (mx, my) = mouseP env
        p = (clampF 20 (envW env - 480) mx, clampF 90 (envH env - 20) my)
    in w { wNodes = setAt (wDrag w) (const p) (wNodes w) }
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w < 0 && noAnim w ->
        let i = nearestEdge env w
        in if i < 0 then w
           else if wMode w == 'D' then deleteEdge i w
           else contractEdge i w
    | otherwise -> w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> w { wMode = 'D' }
    | c `elem` "kK" -> w { wMode = 'K' }
    | c `elem` "uU" && noAnim w -> case wStack w of
        (Snap ns es ops : rest) -> computeStats w { wNodes = ns, wEdges = es, wOps = ops, wStack = rest, wDrag = -1 }
        [] -> w
    | c `elem` "rR" -> resetGraph env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. e5（対角線）を縮約すると、四角形の 4 辺が 2 組の平行辺になる。さらに縮約してループを作ってみよう
-- 2. 辺が 4 本・階数 2 のマイナーをいろいろ作り、「独立な 2 個組」が 6 にならないことを確かめる（U2,4 は出ない）
-- 3. 削除と縮約の順番を入れかえても結果は同じ（M∖e/f = M/f∖e）。U で戻して試す
-- 4. 双対（09）との関係：M∖e の双対 = M*/e。削除と縮約は双対で入れかわる

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
