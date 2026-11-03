-- ============================================
-- 10 ベルマン–フォード法と差分制約
-- 負の重みがあっても使える最短経路。全部の辺を緩和（d を短く書きかえられるか試す）することを |V|−1 回くり返す
-- そのあともう 1 周して、まだ短くなる辺があれば「負の閉路」がある（最短経路が存在しない）
-- 差分制約 xⱼ − xᵢ ≤ b は、辺 vᵢ → vⱼ（重み b）に置きかえるとベルマン–フォードで解ける
-- 操作：スペース＝再生・停止 / →＝1 手 / 1＝負の重みあり / 2＝負の閉路 / 3＝差分制約 / 4＝解なしの制約
--       点をクリック＝始点 / 線の数字クリック＝重み（Shift で減らす）/ ドラッグ＝矢印 / R＝最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, unless)
import Data.Char (toLower)
import Data.List (find, foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word32)

data Vars = Vars
  { vMode     :: !Int
  , vSrc      :: !Int
  , vD        :: !(IM.IntMap Float)
  , vPi       :: !(IM.IntMap Int)
  , vPass     :: !Int
  , vCurE     :: !Int   -- いま見ている辺（-1 = なし）
  , vImproved :: !Int   -- いま d が小さくなった辺
  , vBadE     :: !Int   -- 負の閉路を見つけた辺
  , vResult   :: !(Maybe Bool)
  , vCons     :: ![(Int, Int, Int)]  -- 差分制約 (j, i, b) : x_j − x_i ≤ b
  }

type World = App Vars

code :: [String]
code =
  [ "Bellman-Ford(G, s)"
  , "  all d = inf, pi = none, s.d = 0"
  , "  repeat (number of points - 1) passes"
  , "      for every edge u -> v"
  , "          Relax(u -> v)"
  , "  one last pass: every edge u -> v"
  , "      if u.d + weight < v.d (still improves)"
  , "          stop: \"negative cycle\""
  , "  stop: \"shortest paths found\""
  , "Relax(u -> v)"
  , "  if u.d + weight < v.d: v.d = u.d + w, v.pi = u"
  ]

main :: IO ()
main = runSketch (sketch "14-10 Bellman-Ford and Difference Constraints" (preset screen0 1))
  { skBack  = cBg
  , skStep  = \env _ w -> pure (autoRun env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

preset :: Size -> Int -> World
preset (sw, sh) k = resetRun (App g area ed0 (run0 "") (Vars k 0 IM.empty IM.empty 0 (-1) (-1) (-1) Nothing cons))
  where
    area = Area 70 160 (sw - 680) (sh - 380)
    base = (makeGraph True) { gKeepOrder = True }
    cons0 = [(5, 4, 2), (4, 3, -1), (3, 2, -2), (2, 1, -1), (1, 5, 3), (3, 1, 0), (5, 2, -1), (4, 1, 1)]
    cons | k == 3 = cons0
         | k == 4 = cons0 ++ [(1, 4, -2), (2, 5, 0)]
         | otherwise = []
    g | k == 1 = -- 負の重みあり（負の閉路はなし）。辺の順番がわざと悪いので 3 周目まで変化が続く
          addEdgesW [(1, 3, 5), (3, 4, 3), (2, 1, -3), (1, 4, 7), (4, 2, -2), (2, 3, 7), (0, 1, 4), (0, 2, 6)]
            (placeGraph [(0, 0.5), (0.4, 0), (0.4, 1), (0.95, 0), (0.95, 1)] area ["s", "a", "b", "c", "d"] base)
      | k == 2 = -- 負の閉路
          addEdgesW [(0, 1, 4), (0, 2, 5), (1, 3, 3), (3, 2, -6), (2, 1, 2), (3, 4, 1)]
            (placeGraph [(0, 0.5), (0.35, 0), (0.35, 1), (0.7, 0.5), (1, 0)] area ["s", "a", "b", "c", "d"] base)
      | otherwise = -- 差分制約 → 制約グラフ（v0 から全部へ 0）
          addEdgesW ([(0, i, 0) | i <- [1 .. 5]] ++ [(i, j, b) | (j, i, b) <- cons])
            (placeGraph [(0.5, 0.5), (0.5, 0), (1, 0.3), (0.85, 1), (0.15, 1), (0, 0.3)] area ["v0", "v1", "v2", "v3", "v4", "v5"] base)

resetRun :: World -> World
resetRun w = w
  { aRun = stopRun "Space to start" (aRun w)
  , aV = (aV w)
      { vD = IM.fromList [(i, inf) | i <- ids], vPi = IM.fromList [(i, -1) | i <- ids]
      , vPass = 0, vCurE = -1, vImproved = -1, vBadE = -1, vResult = Nothing
      , vSrc = if vSrc (aV w) >= n then 0 else vSrc (aV w) }
  }
  where
    n = nCount (aG w)
    ids = [0 .. n - 1]

dOf :: Vars -> Int -> Float
dOf v i = IM.findWithDefault inf i (vD v)

piOf :: Vars -> Int -> Int
piOf v i = IM.findWithDefault (-1) i (vPi v)

bellmanFord :: Graph -> Int -> Algo Vars ()
bellmanFord g s = do
  let lab = labelOf g
      n = nCount g
      ids = [0 .. n - 1]
      wf e = fromIntegral (eW e) :: Float
      arrow' e = lab (eU e) ++ "->" ++ lab (eV e)
  modify (\v -> v { vD = IM.insert s 0 (IM.fromList [(i, inf) | i <- ids]), vPi = IM.fromList [(i, -1) | i <- ids] })
  yield 1 ("d = inf, " ++ lab s ++ ".d = 0")
  forM_ [1 .. n - 1] $ \pass -> do
    modify (\v -> v { vPass = pass })
    yield 2 ("Pass " ++ show pass ++ " of " ++ show (n - 1))
    changes <- mapM (\e -> do
      modify (\v -> v { vCurE = eId e, vImproved = -1 })
      v0 <- getS
      let du = dOf v0 (eU e)
          dv = dOf v0 (eV e)
          nd = du + wf e
      if dv > nd
        then do
          modify (\v -> v { vD = IM.insert (eV e) nd (vD v), vPi = IM.insert (eV e) (eU e) (vPi v), vImproved = eId e })
          yield 10 ("Relax " ++ arrow' e ++ ": " ++ lab (eV e) ++ ".d " ++ fmt dv ++ " -> " ++ fmt du ++ " + (" ++ show (eW e) ++ ") = " ++ fmt nd)
          pure True
        else do
          yield 4 ("Relax " ++ arrow' e ++ ": " ++ fmt du ++ " + (" ++ show (eW e) ++ ") >= " ++ fmt dv ++ ": no change")
          pure False) (gEdges g)
    modify (\v -> v { vCurE = -1, vImproved = -1 })
    unless (or changes) (yield 2 ("Pass " ++ show pass ++ " changed nothing (we could stop here)"))
  modify (\v -> v { vPass = n })
  mode <- gets vMode
  let check [] = do
        modify (\v -> v { vCurE = -1, vResult = Just True })
        yield 8 "No negative cycle"
        say ("Success: shortest paths found" ++ (if mode >= 3 then ": x_i = dist(v0, v_i) solves the constraints" else ""))
      check (e : rest) = do
        modify (\v -> v { vCurE = eId e })
        yield 6 ("Check: " ++ lab (eV e) ++ ".d > " ++ lab (eU e) ++ ".d + w ?")
        v0 <- getS
        if dOf v0 (eV e) > dOf v0 (eU e) + wf e
          then do
            modify (\v -> v { vBadE = eId e, vResult = Just False, vCurE = -1 })
            yield 7 "Still improves: there is a negative cycle!"
            say ("Failed: negative cycle (each lap is shorter)" ++ (if mode >= 3 then ": no solution" else ""))
          else check rest
  check (gEdges g)

drawWorld :: Env -> World -> Picture
drawWorld env w = view env (concat [[title], edges, [editRubber env w], nodes, [statusLine 40 (sh - 34) r, drawCode code (rLine r) (sw - 540) 110 520], side])
  where
    g = aG w
    v = aV w
    r = aRun w
    sw = envW env
    sh = envH env
    y = 110 + codeH code
    mode = vMode v
    pass = vPass v
    title = hud ("10 Bellman-Ford" ++ (if mode >= 3 then " solves difference constraints" else ""))
      [ "Space=play  Right=step  1=neg. weights  2=neg. cycle  3=constraints  4=no solution  Click=start  Click number=weight"
      , "Below a point: d  Thick red=pi (tree)  Green=d just got smaller  Shift+click number=-1  R=restart" ++ (if pass > 0 && pass < nCount g then "  Pass " ++ show pass else "") ]
    edges =
      [ drawEdge (xyOf g (eU e)) (xyOf g (eV e)) edgeO
          { oDirected = True, oBend = bendRev g e, oCol = c, oW = wd
          , oLabel = show (eW e), oLabelCol = if eW e < 0 then cAccent else cText }
      | e <- gEdges g
      , let (c, wd)
              | eId e == vBadE v = (hex 0xff2d55, 7)
              | eId e == vImproved v = (cGreen, 6)
              | eId e == vCurE v = (cYel, 5)
              | piOf v (eV e) == eU e = (cAccent, 4)
              | otherwise = (cEdge, 2) ]
    improvedTo = case find (\e -> eId e == vImproved v) (gEdges g) of
      Just e -> eV e
      Nothing -> -1
    nodes =
      [ drawNode (nX nd, nY nd) (nLabel nd) (if i == vSrc v then cAccent else if i == improvedTo then cGreen else cWhite) nodeO
          { nBelow = Just ("d=" ++ fmt (dOf v i)) }
      | (i, nd) <- zip [0 ..] (gNodes g) ]
    side
      | mode >= 3 = drawConstraints env w (sw - 540) (y + 14) 520
      | otherwise =
          drawBoxes "Edge order"
            [ Item (labelOf g (eU e) ++ labelOf g (eV e)) (if eId e == vCurE v then Just cYel else if eId e == vImproved v then Just cGreen else Nothing) | e <- gEdges g ]
            (sw - 540) (y + 14) 520
          : zipWith (\i t -> txt (sw - 540) (y + 76 + i * 20) 14 cDim t) [0 ..]
              [ "Why |V|-1 passes? A shortest path has at most"
              , "|V|-1 edges. After pass i, every shortest path"
              , "with at most i edges is correct (path relaxation)"
              , "Theta(VE): slower than Dijkstra, but handles w < 0" ]

drawConstraints :: Env -> World -> Float -> Float -> Float -> [Picture]
drawConstraints env w x y pw =
  panelBox x y pw ph "x_j - x_i <= b  gives edge v_i -> v_j (weight b)"
  : zipWith consLine [0 :: Int ..] (vCons v)
  ++ result
  where
    v = aV w
    ph = envH env - y - 30
    dv = dOf v
    fi = fromIntegral :: Int -> Float
    consLine k (j, i, b) =
      let diff = dv j - dv i
          ok = diff <= fromIntegral b
          solved = vResult v == Just True
          c | not solved = cText
            | ok = cGreen
            | otherwise = cAccent
      in txt (x + 16 + fi (k `mod` 2) * 250) (y + 44 + fi (k `div` 2) * 22) 15 c
           ("x" ++ show j ++ " - x" ++ show i ++ " <= " ++ show b ++ (if solved then "  " ++ fmt diff ++ (if ok then " ok" else " NG") else ""))
    n = nCount (aG w)
    xs = map dv [1 .. n - 1]
    finite = not (null xs) && not (any (\t -> isInfinite t || isNaN t) xs)
    result = case vResult v of
      Just True | finite -> numberLine
      Just False -> [txt (x + 16) (y + ph - 30) 15 cAccent "Negative cycle = the sum says 0 <= (negative)"]
      _ -> []
    -- 数直線に解を並べる
    numberLine =
      let ly = y + ph - 50
          lo = foldl' min inf xs - 1
          hi = max (foldl' max (negate inf) xs) 0 + 1
          mapX t = remap t lo hi (x + 20) (x + pw - 20)
          ticks = [ceiling lo .. floor hi] :: [Int]
          stacked = go IM.empty (zip [1 :: Int ..] xs)
          go _ [] = []
          go seen ((i, t) : rest) =
            let key = round t :: Int
                k = IM.findWithDefault 0 key seen + 1
            in (i, t, k) : go (IM.insert key k seen) rest
      in [seg cDim 2 (x + 20, ly) (x + pw - 20, ly)]
         ++ concat [ [seg cDim 1.5 (mapX (fi t), ly - 4) (mapX (fi t), ly + 4), txtC (mapX (fi t)) (ly + 16) 14 cDim (show t)] | t <- ticks ]
         ++ [ drawNode (mapX t, ly - 18 - fi (k - 1) * 30) ("x" ++ show i) cCyan nodeO { nR = 13 } | (i, t, k) <- stacked ]
         ++ [txt (x + 16) (y + 150) 14 cGreen ("Solution x = (" ++ joinWith ", " (map fmt xs) ++ ")  Adding a constant also solves it")]

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith sep (x : xs) = x ++ concatMap (sep ++) xs

begin :: World -> Maybe (Algo Vars ())
begin w
  | nCount (aG w) == 0 = Nothing
  | otherwise = Just (bellmanFord (aG w) (vSrc (aV w)))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventResize _ -> pure (preset (sizeOf env) (vMode (aV w)))
  EventKey (MouseButton LeftButton) Down mods _ -> pure $
    case weightClick (shift mods == Down) (-5) 10 m (aG w) of
      Just g1 -> editCancel (resetRun w { aG = g1 })
      Nothing -> editPress m w
  EventKey (MouseButton LeftButton) Up _ _ -> pure $ case editRelease 3 m w of
    (w1, ClickedNode) -> resetRun w1 { aV = (aV w1) { vSrc = edClicked (aEd w1) } }
    (w1, NoEdit) -> w1
    (w1, _) -> resetRun w1
  EventMotion _ | envDown env -> pure (editDrag m w)
  _ -> do
    r <- runKeys env resetRun begin (drawWorld env w) ev w
    pure $ case r of
      Just w1 -> w1
      Nothing -> case charKey ev of
        Just c | c `elem` "1234" -> preset (sizeOf env) (fromEnum c - fromEnum '0')
        Just 'r' -> resetRun w
        Just 'c' -> resetRun w { aG = (aG w) { gNodes = [], gEdges = [] }, aV = (aV w) { vCons = [] } }
        _ -> w
  where m = mouseOf env

-- ---------- やってみよう ----------
-- 1. お手本 1 で d = (0, 3, 6, 8, 10) になるか確かめよう。何周目で変化が止まる？
-- 2. 2 の負の閉路の重みを 1 つ大きくして（Shift なしでクリック）閉路の合計を 0 以上にすると？
-- 3. 差分制約は「時間割」「作業の開始時刻」の問題。制約を 1 つ厳しくすると解が消える（4）

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
