-- ============================================
-- 07 最小全域木の考え方：カットと安全な辺
-- 全域木 ＝ すべての点を、閉路なしで V−1 本の線でつないだもの。最小全域木 ＝ 重みの合計が最小
-- 基本の作り方：「A に足しても最小全域木の一部のままの辺（安全な辺）」を 1 本ずつ足す
-- カットの性質：A の辺をまたがないカット (S, V−S) を横切る辺のうち、いちばん軽い辺は安全
-- 操作：点をクリック＝S に入れる/出す（黄色が S）/ スペース＝そのカットの軽い辺を A に足す
--       A＝自動（A の木の 1 つを S にする）/ 線の数字をクリック＝重みを変える / 1・2＝お手本 / R＝最初から
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (find, foldl', nub, sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import Data.Word (Word32)

data Vars = Vars
  { vInS :: !(Set.Set Int)  -- S に入っている点
  , vInA :: !(Set.Set Int)  -- A に入った辺（線の名札）
  , vMsg :: !String
  , vT   :: !(Set.Set Int)  -- 比べるための最小全域木 T（クルスカルで求めておく）
  }

type World = App Vars

main :: IO ()
main = runSketch (sketch "14-07 Cuts and Safe Edges" (preset screen0 1))
  { skBack  = cBg
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

preset :: Size -> Int -> World
preset (sw, sh) k = resetAll (App g area ed0 (run0 "") (Vars Set.empty Set.empty "" Set.empty))
  where
    area = Area 70 160 (sw - 680) (sh - 360)
    rs = seeded 7
    wiggle a b r = r * (b - a) + a
    g | k == 1 = -- 9 点・14 本（同じ重みの辺もある）
          addEdgesW [(0, 1, 6), (1, 2, 3), (0, 3, 2), (1, 4, 9), (2, 5, 5), (3, 4, 4), (4, 5, 8), (3, 6, 7), (4, 7, 1), (5, 8, 6), (6, 7, 3), (7, 8, 10), (0, 4, 11), (4, 8, 5)]
            (placeGraph [(0, 0), (0.5, 0), (1, 0), (0, 0.5), (0.45, 0.55), (1, 0.5), (0, 1), (0.5, 1), (1, 1)] area (letters 9) (makeGraph False))
      | otherwise =
          let pos = [ (clampF 0 1 (fromIntegral (i `mod` 4) / 3 + wiggle (-0.08) 0.08 (ixOr 0 rs (2 * i))),
                       clampF 0 1 (fromIntegral (i `div` 4) / 2 + wiggle (-0.1) 0.1 (ixOr 0 rs (2 * i + 1))))
                    | i <- [0 .. 9 :: Int] ]
              pairs = [(0, 1), (1, 2), (2, 3), (0, 4), (1, 5), (2, 6), (3, 7), (4, 5), (5, 6), (6, 7), (4, 8), (5, 8), (5, 9), (6, 9), (8, 9), (1, 4), (2, 7)]
              weights = [floor (wiggle 1 15 r) | r <- drop 20 rs]
          in addEdgesW (zipWith (\(u, v) wt -> (u, v, wt)) pairs weights)
               (placeGraph pos area (letters 10) (makeGraph False))

resetAll :: World -> World
resetAll w = w { aV = Vars Set.empty Set.empty "Click points to build the cut S" (computeT (aG w)) }

-- 素集合（根を探す）
findRoot :: IM.IntMap Int -> Int -> Int
findRoot p x = case IM.lookup x p of
  Just y | y /= x -> findRoot p y
  _ -> x

unionAll :: Graph -> [Edge] -> IM.IntMap Int
unionAll g = foldl' link (IM.fromList [(i, i) | i <- [0 .. nCount g - 1]])
  where link p e = IM.insert (findRoot p (eU e)) (findRoot p (eV e)) p

-- クルスカル（比べる用）
computeT :: Graph -> Set.Set Int
computeT g = snd (foldl' step (IM.fromList [(i, i) | i <- [0 .. nCount g - 1]], Set.empty) (sortOn eW (gEdges g)))
  where
    step (p, t) e =
      let a = findRoot p (eU e)
          b = findRoot p (eV e)
      in if a /= b then (IM.insert a b p, Set.insert (eId e) t) else (p, t)

inSet :: Vars -> Int -> Bool
inSet v i = Set.member i (vInS v)

crossing :: Graph -> Vars -> [Edge]
crossing g v = filter (\e -> inSet v (eU e) /= inSet v (eV e)) (gEdges g)

-- いちばん軽い横切る辺（同じ重みなら先に見つけた方）
lightEdge :: Graph -> Vars -> Maybe Edge
lightEdge g v = case crossing g v of
  [] -> Nothing
  (e : es) -> Just (foldl' (\a b -> if eW b < eW a then b else a) e es)

edgesOf :: Graph -> Set.Set Int -> [Edge]
edgesOf g s = filter (\e -> Set.member (eId e) s) (gEdges g)

respects :: Graph -> Vars -> Bool
respects g v = all (\e -> inSet v (eU e) == inSet v (eV e)) (edgesOf g (vInA v))

-- 木 T の中の u から v への道（辺の列）
treePath :: Graph -> Vars -> Int -> Int -> [Edge]
treePath g v u t = walk t
  where
    tree = edgesOf g (vT v)
    prev = bfs (IM.singleton u Nothing) [u]
    bfs seen [] = seen
    bfs seen (x : q) =
      let new = [(y, e) | e <- tree, let y = if eU e == x then eV e else if eV e == x then eU e else -1, y >= 0]
          (seen1, q1) = foldl' (\(s, acc) (y, e) -> if IM.member y s then (s, acc) else (IM.insert y (Just e) s, acc ++ [y])) (seen, []) new
      in bfs seen1 (q ++ q1)
    walk x = case IM.lookup x prev of
      Just (Just e) -> e : walk (if eU e == x then eV e else eU e)
      _ -> []

addLight :: World -> World
addLight w
  | null (gNodes g) = w
  | Set.null (vInS v) || Set.size (vInS v) == nCount g = say' "S is empty or everything: no crossing edge. Click points to build S"
  | not (respects g v) = say' "! An edge of A crosses the cut: it does not respect A, the cut property fails"
  | otherwise = case lightEdge g v of
      Nothing -> say' "No crossing edge (S and V-S are not connected)"
      Just e ->
        let a = Set.insert (eId e) (vInA v)
        in w { aV = v { vInA = a, vMsg = "Light edge " ++ labelOf g (eU e) ++ "-" ++ labelOf g (eV e) ++ " (" ++ show (eW e) ++ ") is safe: added to A. |A| = " ++ show (Set.size a) ++ " / " ++ show (nCount g - 1) } }
  where
    g = aG w
    v = aV w
    say' m = w { aV = v { vMsg = m } }

-- 自動：A の森の木を 1 つ選び、それを S にする（必ず A を尊重する）
autoCut :: World -> IO World
autoCut w
  | Set.size (vInA v) >= nCount g - 1 || null (gNodes g) = pure w { aV = v { vMsg = "Complete! A is a minimum spanning tree" } }
  | otherwise = do
      let p = unionAll g (edgesOf g (vInA v))
          roots = nub (map (findRoot p) [0 .. nCount g - 1])
      k <- randomRIO (0, length roots - 1)
      let r = ixOr 0 roots k
          s = Set.fromList [i | i <- [0 .. nCount g - 1], findRoot p i == r]
      pure (addLight w { aV = v { vInS = s } })
  where
    g = aG w
    v = aV w

drawWorld :: Env -> World -> Picture
drawWorld env w = view env (concat [[title], halos, edges, nodes, texts, proof])
  where
    g = aG w
    v = aV w
    sw = envW env
    ga = aGA w
    bottom = aY ga + aH ga
    le = lightEdge g v
    hvp = case le of
      Just e | Set.notMember (eId e) (vT v) && respects g v -> treePath g v (eU e) (eV e)
      _ -> []
    hvIds = map eId hvp
    swapE = find (\e -> inSet v (eU e) /= inSet v (eV e)) hvp
    crossIds = map eId (crossing g v)
    title = hud "07 Cuts and Safe Edges - how to build an MST"
      [ "Click point=in/out of S  Space=add light edge to A  A=auto cut  Click edge number=weight  1,2=presets  R=restart"
      , "Yellow=S  Thick red=A (MST in progress)  Dashed=crosses the cut  *=light edge (lightest crossing edge)" ]
    -- カットの境界線っぽく、S を薄く囲む
    halos = [dot (alpha255 28 cYel) (xyOf g i) 45 | i <- Set.toList (vInS v), i < nCount g]
    edges = map edgePic (gEdges g)
    isLe e = fmap eId le == Just (eId e)
    edgePic e =
      let inCross = eId e `elem` crossIds
          (c, wd, ds)
            | isLe e = (cYel, 6, Nothing)
            | Set.member (eId e) (vInA v) = (cAccent, 6, Nothing)
            | eId e `elem` hvIds = (cViolet, 4, Nothing)
            | inCross = (cCyan, 2.5, Just (6, 5))
            | otherwise = (cEdge, 2, Nothing)
      in drawEdge (xyOf g (eU e)) (xyOf g (eV e)) edgeO
           { oCol = c, oW = wd, oDash = ds, oLabel = (if isLe e then "*" else "") ++ show (eW e) }
    nodes = [drawNode (nX nd, nY nd) (nLabel nd) (if inSet v i then cYel else cWhite) nodeO | (i, nd) <- zip [0 ..] (gNodes g)]
    weightOf s = sum (map eW (edgesOf g s))
    wA = weightOf (vInA v)
    wT = weightOf (vT v)
    resp = respects g v
    texts =
      [ txt 40 (bottom + 66) 17 (if take 1 (vMsg v) == "!" then cAccent else cGreen) (vMsg v)
      , txt 40 (bottom + 94) 15 cText
          ("Weight of A = " ++ show wA ++ "   MST weight = " ++ show wT ++ (if Set.size (vInA v) == nCount g - 1 && wA == wT then "   OK, they match!" else ""))
      , txt 40 (bottom + 118) 15 (if resp then cGreen else cAccent)
          (if resp then "This cut respects A (no edge of A crosses it)" else "This cut does not respect A") ]
    -- 証明の図解パネル
    px = sw - 540
    proofLines =
      [ "T = some MST (here: the one Kruskal builds)"
      , "If the light edge (u, v) is not in T..."
      , " 1. T + (u, v) has a cycle (purple path + *)"
      , " 2. it crosses the cut twice: so (x, y) in T does"
      , " 3. (u, v) is light, so w(u, v) <= w(x, y)"
      , " 4. T' = T - (x, y) + (u, v) is not heavier"
      , " so T' is an MST containing A + (u, v): safe!" ]
    nm e = labelOf g (eU e) ++ "-" ++ labelOf g (eV e)
    proof =
      panelBox px 110 520 330 "Proof of the cut property (exchange argument)"
      : zipWith (\i t -> txt (px + 16) (158 + fromIntegral i * 24) 14 (if i >= (2 :: Int) && null hvp then cDim else cText) t) [0 ..] proofLines
      ++ [ txt (px + 16) 330 15 cYel $ case (le, swapE) of
             (Just e, _) | Set.member (eId e) (vT v) -> "* " ++ nm e ++ " is in T (safe as it is)"
             (Just e, Just s) -> "* " ++ nm e ++ "(" ++ show (eW e) ++ ") can swap with " ++ nm s ++ "(" ++ show (eW s) ++ ")"
             _ -> "Click points to build S and see the swap here"
         , txt (px + 16) 370 14 cDim "Kruskal (08) and Prim (09) are special cases of this"
         , txt (px + 16) 392 14 cDim "Prim: S = the tree.  Kruskal: S = one forest tree" ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventResize _ -> pure (preset (sizeOf env) 1)
  EventKey (MouseButton LeftButton) Down mods _ -> pure $
    case weightClick (shift mods == Down) 1 15 m g of
      Just g1 -> w { aG = g1, aV = v { vT = computeT g1 } }
      Nothing ->
        let i = nodeAt g m
        in if i < 0 then w else
             let s = if Set.member i (vInS v) then Set.delete i (vInS v) else Set.insert i (vInS v)
                 names = [nLabel nd | (j, nd) <- zip [0 ..] (gNodes g), Set.member j s]
             in w { aV = v { vInS = s, vMsg = "S = { " ++ unwords names ++ " }  Space adds the light edge" } }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (addLight w)
  _ -> case charKey ev of
    Just 'a' -> autoCut w
    Just '1' -> pure (preset (sizeOf env) 1)
    Just '2' -> pure (preset (sizeOf env) 2)
    Just 'r' -> pure (resetAll w)
    _ -> pure w
  where
    m = mouseOf env
    g = aG w
    v = aV w

-- ---------- やってみよう ----------
-- 1. A を何回も押して完成させ、A の重みが最小全域木の重みと一致するか確かめよう
-- 2. わざと A の辺をまたぐカットを作ってスペース → なぜカットの性質が使えない？
-- 3. 同じ重みの辺があると、最小全域木は 1 つとは限らない。重みを変えて試そう

-- ==================================================
-- ここから下は「グラフを描く・動かすための道具」です（シリーズ14 共通）
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- ---------- 座標 ----------
-- スケッチの中は p5.js と同じ「左上が原点、y は下向き」で考える。最後に view で gloss の座標に変える
type Size = (Float, Float)

sizeOf :: Env -> Size
sizeOf env = (envW env, envH env)

screen0 :: Size
screen0 = (1280, 800)

-- マウスの位置（左上が原点）
mouseOf :: Env -> Point
mouseOf env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

view :: Env -> [Picture] -> Picture
view env ps = Translate (negate (envW env) / 2) (envH env / 2) (Scale 1 (-1) (Pictures ps))

-- ---------- 色（未訪問＝白、発見済み＝灰、完了＝黒 ＋ 強調色）----------
hex :: Int -> Color
hex v = rgb255 (v `div` 65536) (v `div` 256 `mod` 256) (v `mod` 256)

cBg, cPanel, cEdge, cText, cDim, cWhite, cGray, cBlack, cAccent, cYel, cCyan, cGreen, cViolet, cOrange :: Color
cBg = hex 0x0d1020
cPanel = hex 0x161a2e
cEdge = hex 0x4a5275
cText = hex 0xe8ebf5
cDim = hex 0x8a92b0
cWhite = hex 0xf4f4f6
cGray = hex 0x9a9cab
cBlack = hex 0x22242e
cAccent = hex 0xff5e7e
cYel = hex 0xffd166
cCyan = hex 0x5ee6ff
cGreen = hex 0x7cf4a6
cViolet = hex 0xc4b5fd
cOrange = hex 0xffa25e

-- 透明度を 0〜255 で指定する（p5.js と同じ）
alpha255 :: Float -> Color -> Color
alpha255 a = withA (a / 255)

-- 距離の表示。無限大は inf
inf :: Float
inf = 1 / 0

fmt :: Float -> String
fmt d
  | isNaN d = "-"
  | isInfinite d = if d > 0 then "inf" else "-inf"
  | otherwise = show (round d :: Int)

-- JavaScript の Math.round と同じ丸め
roundJS :: Float -> Int
roundJS x = floor (x + 0.5)

-- p5.js の randomSeed + random と同じ数列（お手本のグラフを同じ形にするため）
seeded :: Word32 -> [Float]
seeded = map toUnit . drop 1 . iterate (\z -> 1664525 * z + 1013904223)
  where toUnit z = realToFrac (fromIntegral z / 4294967296 :: Double)

-- リストの安全な取り出し（範囲外なら代わりの値）
ixOr :: a -> [a] -> Int -> a
ixOr d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- ---------- グラフ ----------
data Node = Node { nX, nY :: !Float, nLabel :: !String }

-- eId は線の名札（JavaScript の「同じオブジェクトか」の代わり）
data Edge = Edge { eId, eU, eV, eW :: !Int }

data Graph = Graph
  { gNodes     :: ![Node]
  , gEdges     :: ![Edge]
  , gDirected  :: !Bool
  , gMulti     :: !Bool   -- 同じ所に何本でも線を引ける
  , gKeepOrder :: !Bool   -- 隣を「線を足した順」に見る
  }

-- 描く範囲
data Area = Area { aX, aY, aW, aH :: !Float }

makeGraph :: Bool -> Graph
makeGraph d = Graph [] [] d False False

-- 位置を 0〜1 の割合で書いたお手本を、画面の範囲 area に置く
placeGraph :: [Point] -> Area -> [String] -> Graph -> Graph
placeGraph pos a labels g =
  g { gNodes = zipWith (\(px, py) l -> Node (aX a + px * aW a) (aY a + py * aH a) l) pos labels }

letters :: Int -> [String]
letters n = map (: []) (take n ['a' .. 'z'])

addEdge :: Int -> Int -> Int -> Graph -> Graph
addEdge u v w g
  | u == v || u < 0 || v < 0 = g
  | not (gMulti g) && any same (gEdges g) =
      g { gEdges = map (\e -> if same e then e { eW = w } else e) (gEdges g) }
  | otherwise = g { gEdges = gEdges g ++ [Edge newId u v w] }
  where
    same e = (eU e == u && eV e == v) || (not (gDirected g) && eU e == v && eV e == u)
    newId = 1 + foldl' max (-1) (map eId (gEdges g))

addEdges :: [(Int, Int)] -> Graph -> Graph
addEdges es g = foldl' (\h (u, v) -> addEdge u v 1 h) g es

addEdgesW :: [(Int, Int, Int)] -> Graph -> Graph
addEdgesW es g = foldl' (\h (u, v, w) -> addEdge u v w h) g es

nodeOf :: Graph -> Int -> Node
nodeOf g = ixOr (Node 0 0 "?") (gNodes g)

labelOf :: Graph -> Int -> String
labelOf g = nLabel . nodeOf g

xyOf :: Graph -> Int -> Point
xyOf g i = let n = nodeOf g i in (nX n, nY n)

nCount :: Graph -> Int
nCount = length . gNodes

-- 隣接リスト Adj[u]（相手のラベル順。gKeepOrder なら線を足した順）
data Adj = Adj { adjV, adjW, adjE :: !Int }

adjList :: Graph -> IM.IntMap [Adj]
adjList g = IM.map order (foldl' push start (gEdges g))
  where
    start = IM.fromList [(i, []) | i <- [0 .. nCount g - 1]]
    push m e =
      let m1 = IM.insertWith (flip (++)) (eU e) [Adj (eV e) (eW e) (eId e)] m
      in if gDirected g then m1 else IM.insertWith (flip (++)) (eV e) [Adj (eU e) (eW e) (eId e)] m1
    order as = if gKeepOrder g then as else sortOn (labelOf g . adjV) as

adjOf :: IM.IntMap [Adj] -> Int -> [Adj]
adjOf a u = IM.findWithDefault [] u a

hasReverse :: Graph -> Edge -> Bool
hasReverse g e = gDirected g && any (\f -> eU f == eV e && eV f == eU e) (gEdges g)

bendRev :: Graph -> Edge -> Float
bendRev g e = if hasReverse g e then 14 else 0

findEdge :: Graph -> Int -> Int -> Maybe Edge
findEdge g u v = find (\e -> (eU e == u && eV e == v) || (not (gDirected g) && eU e == v && eV e == u)) (gEdges g)

-- 辺 e は u と v を結んでいるか
touches :: Graph -> Edge -> Int -> Int -> Bool
touches g e u v = (eU e == u && eV e == v) || (not (gDirected g) && eU e == v && eV e == u)

-- ---------- 文字と四角 ----------
-- 文字の幅（gloss のフォントの実際の幅。空白や = はかなり広い）
textW :: Float -> String -> Float
textW size s = sum (map charW s) * size / 100

charW :: Char -> Float
charW c = ixOr 80 romanWidths (fromEnum c - 32)

romanWidths :: [Float]
romanWidths =
  [ 104, 26, 51, 79, 76, 96, 101, 13, 47, 47, 59, 97, 26, 100, 26, 82
  , 77, 66, 77, 77, 80, 77, 73, 77, 77, 74, 26, 26, 81, 97, 81, 73
  , 74, 80, 83, 84, 85, 78, 78, 89, 89, 21, 59, 79, 71, 97, 88, 88
  , 85, 88, 82, 80, 71, 89, 81, 100, 72, 79, 73, 46, 78, 46, 90, 104
  , 83, 66, 70, 68, 70, 68, 38, 70, 71, 28, 36, 62, 19, 123, 70, 71
  , 70, 70, 49, 62, 39, 71, 60, 80, 56, 66, 61, 41, 23, 41, 91 ]

-- 左下を (x, y) にして書く
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- (x, y) を中心にして書く
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - textW size s / 2) (y + size / 2) size c s

-- 左上を (x, y) にした四角
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (y + h / 2) (max 0 w) (max 0 h))

-- 太さ sw の枠
frameBox :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox c sw x y w h = Color c (thickPath sw [(x - sw / 2, y), (x + w + sw / 2, y), (x + w + sw / 2, y + h), (x + w, y + h), (x - sw / 2, y + h), (x, y + h), (x, y)])

dot :: Color -> Point -> Float -> Picture
dot c p r = Color c (at p (disc r))

seg :: Color -> Float -> Point -> Point -> Picture
seg c w a b = Color c (thickLine w a b)

-- 点線（on 描いて off 休む）
dashed :: Float -> Float -> Float -> [Point] -> Picture
dashed w on off ps = Pictures (go 0 ps)
  where
    period = on + off
    go :: Float -> [Point] -> [Picture]
    go phase (a : rest@(b : _)) =
      let len = dist2 a b
          pointAt d = let t = d / max 1e-3 len in (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
          starts = takeWhile (< len) (iterate (+ period) (negate phase))
          piece s = let s0 = max 0 s; s1 = min len (s + on)
                    in if s1 > s0 then [thickLine w (pointAt s0) (pointAt s1)] else []
      in concatMap piece starts ++ go (wrapF period (phase + len)) rest
    go _ _ = []

-- ---------- 描く ----------
nodeAt :: Graph -> Point -> Int
nodeAt = nodeAtR 26

nodeAtR :: Float -> Graph -> Point -> Int
nodeAtR r g p = foldl' pick (-1) (zip [0 ..] (gNodes g))
  where pick best (i, n) = if dist2 p (nX n, nY n) < r then i else best

edgePoint :: Point -> Point -> Float -> Float -> Point
edgePoint (ax, ay) (bx, by) bend t =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      dx = bx - ax
      dy = by - ay
      l = max 1 (sqrt (dx * dx + dy * dy))
      cx = mx - (dy / l) * bend * 2
      cy = my + (dx / l) * bend * 2
      u = 1 - t
  in (u * u * ax + 2 * u * t * cx + t * t * bx, u * u * ay + 2 * u * t * cy + t * t * by)

-- 線（矢印）のオプション
data EdgeO = EdgeO
  { oDirected :: !Bool
  , oCol      :: !Color
  , oW        :: !Float
  , oBend     :: !Float
  , oLabel    :: !String
  , oLabelCol :: !Color
  , oLabelT   :: !Float
  , oR        :: !Float
  , oDash     :: !(Maybe (Float, Float))
  }

edgeO :: EdgeO
edgeO = EdgeO False cEdge 2 0 "" cText 0.5 22 Nothing

drawEdge :: Point -> Point -> EdgeO -> Picture
drawEdge a b o
  | a == b = Blank
  | otherwise = Pictures [Color col body, tip, tagLabel]
  where
    bend = oBend o
    r = oR o
    col = oCol o
    w = oW o
    pt = edgePoint a b bend
    ts = [fromIntegral i / 50 | i <- [0 .. 50 :: Int]]
    t0 = case filter (\t -> dist2 (pt t) a >= r) ts of
      (t : _) -> t
      [] -> 0
    t1 = case filter (\t -> dist2 (pt t) b >= r + 2) (reverse ts) of
      (t : _) -> t
      [] -> 1
    pieces = if bend == 0 then 1 else 16 :: Int
    pts = [pt (t0 + (t1 - t0) * fromIntegral i / fromIntegral pieces) | i <- [0 .. pieces]]
    body = case oDash o of
      Just (on, off) -> dashed w on off pts
      Nothing -> thickPath w pts
    end = pt t1
    tip
      | oDirected o =
          let (qx, qy) = pt (t1 - 0.05)
              ang = atan2 (snd end - qy) (fst end - qx)
              s = 9 + w
              f d = (fst end - cos (ang + d) * s, snd end - sin (ang + d) * s)
          in Color col (Polygon [end, f (-0.42), f 0.42])
      | otherwise = Blank
    tagLabel
      | null (oLabel o) = Blank
      | otherwise =
          let (mx, my) = pt (oLabelT o)
              tw = textW 14 (oLabel o) + 12
          in Pictures
               [ box col (mx - tw / 2 - 1.5) (my - 12.5) (tw + 3) 25
               , box cBg (mx - tw / 2) (my - 11) tw 22
               , txtC mx my 14 (oLabelCol o) (oLabel o) ]

edgeLabelAt :: Point -> Point -> Float -> Point -> Bool
edgeLabelAt a b bend p = dist2 p (edgePoint a b bend 0.5) < 16

-- 点のオプション
data NodeO = NodeO
  { nR        :: !Float
  , nRing     :: !(Maybe Color)
  , nBelow    :: !(Maybe String)
  , nAbove    :: !(Maybe String)
  , nBelowCol :: !Color
  , nAboveCol :: !Color
  }

nodeO :: NodeO
nodeO = NodeO 22 Nothing Nothing Nothing cYel cCyan

-- 点。f は塗る色（cWhite / cGray / cBlack や強調色）
drawNode :: Point -> String -> Color -> NodeO -> Picture
drawNode (x, y) s f o = Pictures
  [ case nRing o of
      Just c -> Color c (at (x, y) (ring (r + 6) 4))
      Nothing -> Blank
  , dot (if f == cBlack then cDim else cBg) (x, y) (r + 1)
  , dot f (x, y) (r - 1)
  , txtC x y size textCol s
  , maybe Blank (\t -> tagText t x (y + r + 13) (nBelowCol o)) (nBelow o)
  , maybe Blank (\t -> tagText t x (y - r - 12) (nAboveCol o)) (nAbove o)
  ]
  where
    r = nR o
    n = length s
    size = max 14 (if n > 3 then r * 0.45 else if n > 2 then r * 0.55 else r * 0.8)
    (cr, cg, cb, _) = rgbaOfColor f
    textCol = if maximum [cr, cg, cb] > 0.6 then cBg else cText

-- 背景つきの小さな文字（線と重なっても読める）
tagText :: String -> Float -> Float -> Color -> Picture
tagText s x y c =
  let tw = textW 14 s + 8
  in Pictures [box (alpha255 210 cBg) (x - tw / 2) (y - 9) tw 18, txtC x y 14 c s]

-- 見出しと説明
hud :: String -> [String] -> Picture
hud title ls = Pictures
  ( txt 20 36 22 cAccent title
  : zipWith (\i l -> txt 20 (60 + i * 20) 14 cDim l) [0 ..] ls )

-- 枠
panelBox :: Float -> Float -> Float -> Float -> String -> Picture
panelBox x y w h title = Pictures
  [box cPanel x y w h, if null title then Blank else txt (x + 12) (y + 20) 14 cDim title]

-- 疑似コードの高さ
codeH :: [String] -> Float
codeH ls = fromIntegral (length ls) * 19 + 16

-- 疑似コード。cur 行を強調
drawCode :: [String] -> Int -> Float -> Float -> Float -> Picture
drawCode ls cur x y w = Pictures (panelBox x y w (codeH ls) "" : zipWith row [0 ..] ls)
  where
    lh = 19
    row i l =
      let fy = fromIntegral (i :: Int)
          now = i == cur
          -- 字下げは空白 1 つを 6 ピクセルに（フォントの空白は幅が広すぎる）
          (indent, rest) = span (== ' ') l
      in Pictures
           [ if now then Pictures [box (alpha255 60 cAccent) (x + 4) (y + 8 + fy * lh) (w - 8) lh, box cAccent (x + 4) (y + 8 + fy * lh) 3 lh] else Blank
           , txt (x + 12 + fromIntegral (length indent) * 6) (y + 22 + fy * lh) 14 (if now then cText else cDim) rest ]

-- 箱の列（キュー・スタック・リスト）
data Item = Item { iT :: !String, iCol :: !(Maybe Color) }

plainItem :: String -> Item
plainItem s = Item s Nothing

colItem :: Color -> String -> Item
colItem c s = Item s (Just c)

drawBoxes :: String -> [Item] -> Float -> Float -> Float -> Picture
drawBoxes name items x y maxW = Pictures (txt x (y + 21) 14 cDim name : go (x + textW 14 name + 10) items)
  where
    go bx [] = [txt bx (y + 21) 14 cDim "(empty)" | null items]
    go bx (it : rest)
      | maxW > 0 && bx + bw > x + maxW = [txt (bx + 4) (y + 21) 14 cDim "..."]
      | otherwise =
          [ box cEdge (bx - 1) (y - 1) (bw + 2) 34
          , box (maybe cPanel id (iCol it)) bx y bw 32
          , txtC (bx + bw / 2) (y + 16) 14 (maybe cText (const cBg) (iCol it)) (iT it) ]
          ++ go (bx + bw + 6) rest
      where bw = max 34 (textW 14 (iT it) + 14)

-- ---------- アルゴリズムを 1 手ずつ動かす（JavaScript のジェネレータの代わり）----------
-- Algo s a は「状態 s を書きかえながら、ときどき止まる計算」。do 記法で書いて、yield で 1 手休む
newtype Algo s a = Algo { unAlgo :: s -> Res s a }

data Res s a
  = Done !s a
  | Pause !s !Int !(Maybe String) (Algo s a)  -- 行番号（負なら変えない）とメッセージ
  | Say !s String (Algo s a)                  -- 止まらずにメッセージだけ変える

instance Functor (Algo s) where
  fmap f m = m >>= pure . f

instance Applicative (Algo s) where
  pure a = Algo (\s -> Done s a)
  mf <*> ma = mf >>= \f -> fmap f ma

instance Monad (Algo s) where
  m >>= f = Algo $ \s -> case unAlgo m s of
    Done s1 a -> unAlgo (f a) s1
    Pause s1 l t k -> Pause s1 l t (k >>= f)
    Say s1 t k -> Say s1 t (k >>= f)

-- 1 手休む（行番号とメッセージ）
yield :: Int -> String -> Algo s ()
yield l t = Algo (\s -> Pause s l (Just t) (pure ()))

yieldL :: Int -> Algo s ()
yieldL l = Algo (\s -> Pause s l Nothing (pure ()))

yieldM :: String -> Algo s ()
yieldM t = Algo (\s -> Pause s (-1) (Just t) (pure ()))

-- 休まずにメッセージだけ変える
say :: String -> Algo s ()
say t = Algo (\s -> Say s t (pure ()))

getS :: Algo s s
getS = Algo (\s -> Done s s)

gets :: (s -> a) -> Algo s a
gets f = Algo (\s -> Done s (f s))

modify :: (s -> s) -> Algo s ()
modify f = Algo (\s -> let s1 = f s in s1 `seq` Done s1 ())

-- 実行の様子
data Run s = Run
  { rGen      :: !(Maybe (Algo s ()))
  , rPlaying  :: !Bool
  , rFinished :: !Bool
  , rSpeed    :: !Float
  , rLine     :: !Int
  , rMsg      :: String
  }

-- スケッチ全体の入れ物。aV はスケッチごとの変数
data App s = App
  { aG   :: !Graph
  , aGA  :: !Area
  , aEd  :: !Ed
  , aRun :: !(Run s)
  , aV   :: !s
  }

run0 :: String -> Run s
run0 = Run Nothing False False 4 (-1)

-- 速さは残して、最初の状態に戻す
stopRun :: String -> Run s -> Run s
stopRun m r = (run0 m) { rSpeed = rSpeed r }

hasRunner :: App s -> Bool
hasRunner w = case rGen (aRun w) of
  Just _ -> True
  Nothing -> False

startRun :: Algo s () -> App s -> App s
startRun gen w = w { aRun = (aRun w) { rGen = Just gen, rFinished = False, rLine = -1, rMsg = "" } }

stepRun :: App s -> App s
stepRun w = case rGen r of
  Just g | not (rFinished r) -> go (aV w) g r
  _ -> w
  where
    r = aRun w
    go s g q = case unAlgo g s of
      Done s1 _ -> w { aV = s1, aRun = q { rFinished = True, rPlaying = False, rGen = Just (pure ()) } }
      Pause s1 l t k -> w { aV = s1, aRun = q { rGen = Just k, rLine = if l >= 0 then l else rLine q, rMsg = maybe (rMsg q) id t } }
      Say s1 t k -> go s1 k q { rMsg = t }

autoRun :: Env -> App s -> App s
autoRun env w
  | rPlaying r && hasRunner w && not (rFinished r) && envFrame env `mod` max 1 (roundJS (60 / rSpeed r)) == 0 = stepRun w
  | otherwise = w
  where r = aRun w

-- 押されたキー（文字は小文字にする）
charKey :: Event -> Maybe Char
charKey ev = case ev of
  EventKey (Char c) Down _ _ -> Just (toLower c)
  _ -> Nothing

-- スペース＝再生/停止、→＝1 手、↑↓＝速さ、S＝画像を保存。扱ったら Just
--   reset は最初に戻す関数、mkGen はアルゴリズムを作る関数（作れないときは Nothing）
runKeys :: Env -> (App s -> App s) -> (App s -> Maybe (Algo s ())) -> Picture -> Event -> App s -> IO (Maybe (App s))
runKeys env reset mkGen pic ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _
    | not (hasRunner w) -> pure (Just (begin True))
    | rFinished r -> pure Nothing
    | otherwise -> pure (Just w { aRun = r { rPlaying = not (rPlaying r) } })
  EventKey (SpecialKey KeyRight) Down _ _
    | not (hasRunner w) -> pure (Just (stepRun (begin False)))
    | otherwise -> pure (Just (stepRun w { aRun = r { rPlaying = False } }))
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (Just w { aRun = r { rSpeed = min 60 (rSpeed r * 2) } })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (Just w { aRun = r { rSpeed = max 0.5 (rSpeed r / 2) } })
  _ | charKey ev == Just 's' -> saveSvg "graph" env pic >> pure (Just w)
  _ -> pure Nothing
  where
    r = aRun w
    begin play =
      let w0 = reset w
      in case mkGen w0 of
           Nothing -> w
           Just gen ->
             let w1 = startRun gen w0
             in w1 { aRun = (aRun w1) { rPlaying = play } }

statusLine :: Float -> Float -> Run s -> Picture
statusLine x y r = Pictures
  [ txt x y 15 (if rFinished r then cGreen else cYel)
      ((if rFinished r then "DONE  " else if rPlaying r then "PLAY  " else "PAUSE  ") ++ rMsg r)
  , txt x (y + 19) 14 cDim ("Space play/pause   Right step   Up/Down speed x" ++ speedText (rSpeed r)) ]

speedText :: Float -> String
speedText v = if v < 1 then "0.5" else show (roundJS v)

-- ---------- マウスでグラフを編集 ----------
-- クリック＝点を追加、点から点へドラッグ＝線を追加、点から空白へドラッグ＝点を移動
data Ed = Ed { edFrom :: !Int, edSx, edSy :: !Float, edMoved :: !Bool, edClicked :: !Int }

ed0 :: Ed
ed0 = Ed (-1) 0 0 False (-1)

data Edit = NoEdit | AddedNode | AddedEdge | MovedNode | ClickedNode deriving Eq

inArea :: Area -> Point -> Bool
inArea a (x, y) = x > aX a - 20 && x < aX a + aW a + 20 && y > aY a - 20 && y < aY a + aH a + 20

editPress :: Point -> App s -> App s
editPress p@(x, y) w = w { aEd = (aEd w) { edFrom = if inArea (aGA w) p then nodeAt (aG w) p else -2, edSx = x, edSy = y, edMoved = False } }

-- 別の所で押されたことにする（編集しない）
editCancel :: App s -> App s
editCancel w = w { aEd = (aEd w) { edFrom = -2 } }

editDrag :: Point -> App s -> App s
editDrag p w
  | dist2 p (edSx e, edSy e) > 6 = w { aEd = e { edMoved = True } }
  | otherwise = w
  where e = aEd w

editRelease :: Int -> Point -> App s -> (App s, Edit)
editRelease newW p@(x, y) w0
  | from == -2 || not (inArea (aGA w) p) = (w, NoEdit)
  | from < 0 && not moved && to < 0 && nCount g < 26 =
      (w { aG = g { gNodes = gNodes g ++ [Node x y (nextLabel g)] } }, AddedNode)
  | from >= 0 && not moved = (w { aEd = (aEd w) { edClicked = from } }, ClickedNode)
  | from >= 0 && to >= 0 && to /= from = (w { aG = addEdge from to newW g }, AddedEdge)
  | from >= 0 && to < 0 && moved =
      (w { aG = g { gNodes = zipWith (\i n -> if i == from then n { nX = x, nY = y } else n) [0 ..] (gNodes g) } }, MovedNode)
  | otherwise = (w, NoEdit)
  where
    from = edFrom (aEd w0)
    moved = edMoved (aEd w0)
    w = w0 { aEd = (aEd w0) { edFrom = -1 } }
    g = aG w
    to = nodeAt g p

nextLabel :: Graph -> String
nextLabel g = case filter (`notElem` map nLabel (gNodes g)) (letters 26) of
  (l : _) -> l
  [] -> show (nCount g)

-- ドラッグ中のゴムひも
editRubber :: Env -> App s -> Picture
editRubber env w
  | edFrom e >= 0 && edMoved e && envDown env && edFrom e < nCount (aG w) =
      Color cYel (dashed 2 6 6 [xyOf (aG w) (edFrom e), mouseOf env])
  | otherwise = Blank
  where e = aEd w

-- 線の数字（重み）をクリック → 重みを 1 つ増やす（Shift で減らす）。範囲 lo〜hi で一周
weightClick :: Bool -> Int -> Int -> Point -> Graph -> Maybe Graph
weightClick isShift lo hi p g = case find hit (gEdges g) of
  Nothing -> Nothing
  Just e -> Just g { gEdges = map (\f -> if eId f == eId e then f { eW = turn (eW f) } else f) (gEdges g) }
  where
    hit e = edgeLabelAt (xyOf g (eU e)) (xyOf g (eV e)) (bendRev g e) p
    turn v =
      let v1 = if isShift then v - 1 else v + 1
      in if v1 > hi then lo else if v1 < lo then hi else v1
