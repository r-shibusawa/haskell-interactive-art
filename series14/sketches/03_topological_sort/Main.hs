-- ============================================
-- 03 トポロジカルソート
-- 「A は B より先に」という矢印をすべて守る一列の順番を作る（閉路のない有向グラフ DAG だけ）
-- DFS 版：点が終わる（黒になる）たびに、リストの「先頭」に入れる → 終了時刻 f の大きい順
-- Kahn 版：入ってくる矢印が 0 本の点から順に取り出し、その点の矢印を消していく
-- 操作：スペース＝再生・停止 / →＝1 手 / A＝DFS 版 ⇔ Kahn 版 / 1＝カレー作り / 2＝履修の順番
--       ドラッグ＝矢印を追加（閉路を作るとどうなる？）/ クリック＝点を追加 / R＝最初から / C＝全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.List (elemIndex, find, foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word32)

data Mark = White | Grayed | Blacked deriving Eq

data St = St { sColor :: !Mark, sD, sF :: !Int }

data Vars = Vars
  { vKahn  :: !Bool
  , vSt    :: !(IM.IntMap St)
  , vClock :: !Int              -- 発見・終了時刻をつけるための時計
  , vList  :: ![Int]
  , vIn    :: !(IM.IntMap Int)  -- 入ってくる矢印の本数
  , vQ     :: ![Int]
  , vCurU  :: !Int
  , vCurV  :: !Int
  , vCycle :: !Bool
  , vLineT :: !Float            -- 終わったら一列に並べるアニメーション
  }

type World = App Vars

st0 :: St
st0 = St White 0 0

codeDfs, codeKahn :: [String]
codeDfs =
  [ "TopologicalSort(G)"
  , "  run DFS to get each finish time f"
  , "  when a point finishes, put it at the list front"
  , "  return the list"
  , "Visit(u)"
  , "  clock + 1, u.d = clock, u turns gray"
  , "  for each neighbour v of u"
  , "      if v is white, Visit(v)"
  , "      if v is gray: cycle! (back edge)"
  , "  u turns black, clock + 1, u.f = clock"
  , "  put u at the front of the list"
  ]
codeKahn =
  [ "Kahn(G)"
  , "  each u: in[u] = number of incoming arrows"
  , "  queue Q = all points with in = 0"
  , "  repeat until Q is empty"
  , "      u = take the front of Q, append to the list"
  , "      for each arrow u -> v"
  , "          in[v] - 1     (erase the arrow)"
  , "          if in[v] is now 0, put v in Q"
  , "  points left over => there is a cycle"
  ]

main :: IO ()
main = runSketch (sketch "14-03 Topological Sort" (preset screen0 False 1))
  { skBack  = cBg
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let w = autoRun env w0
  in if rFinished (aRun w) then w { aV = (aV w) { vLineT = min 1 (vLineT (aV w) + 0.015) } } else w

preset :: Size -> Bool -> Int -> World
preset (sw, sh) kahnMode k = resetRun (App g area ed0 (run0 "") (Vars kahnMode IM.empty 0 [] IM.empty [] (-1) (-1) False 0))
  where
    area = Area 70 150 (sw - 680) (sh - 420)
    base = (makeGraph True) { gKeepOrder = True }
    g | k == 1 = -- カレー作り（お茶はいつでもよい）
          addEdges [(0, 1), (1, 7), (2, 4), (3, 4), (4, 5), (5, 6), (6, 7), (2, 5)]
            (placeGraph [(0.05, 0.1), (0.5, 0.05), (0.05, 0.55), (0.05, 0.95), (0.35, 0.75), (0.6, 0.75), (0.8, 0.95), (0.95, 0.4), (0.3, 0.35)] area
              ["Wash", "Rice", "Veg", "Meat", "Fry", "Stew", "Roux", "Serve", "Tea"] base)
      | otherwise = -- 履修の順番
          addEdges [(0, 2), (0, 4), (1, 3), (2, 7), (3, 5), (3, 6), (4, 7), (5, 7), (5, 8), (6, 8), (2, 5)]
            (placeGraph [(0, 0.1), (0, 0.9), (0.33, 0.1), (0.33, 0.6), (0.66, 0), (0.66, 0.5), (0.66, 1), (1, 0.3), (1, 0.85)] area
              ["Math", "Prog", "LinAl", "Data", "Prob", "Algo", "OS", "ML", "Comp"] base)

resetRun :: World -> World
resetRun w = w
  { aRun = stopRun "Space to start (A switches the method)" (aRun w)
  , aV = (aV w)
      { vSt = IM.fromList [(i, st0) | i <- [0 .. n - 1]], vClock = 0, vList = [], vQ = []
      , vIn = IM.fromList [(i, 0) | i <- [0 .. n - 1]], vCurU = -1, vCurV = -1, vCycle = False, vLineT = 0 }
  }
  where n = nCount (aG w)

stOf :: Vars -> Int -> St
stOf v u = IM.findWithDefault st0 u (vSt v)

editSt :: Int -> (St -> St) -> Algo Vars ()
editSt u f = modify (\v -> v { vSt = IM.adjust f u (vSt v) })

topoDfs :: Graph -> Algo Vars ()
topoDfs g = do
  let adj = adjList g
  yield 1 "Start the DFS"
  forM_ [0 .. nCount g - 1] $ \u -> do
    c <- gets (sColor . (`stOf` u))
    when (c == White) (visit g adj u)
  modify (\v -> v { vCurU = -1, vCurV = -1 })
  cyc <- gets vCycle
  say (if cyc then "There is a cycle: no valid order exists" else "Finished: larger f first = a topological order")

visit :: Graph -> IM.IntMap [Adj] -> Int -> Algo Vars ()
visit g adj u = do
  let lab = labelOf g
  modify (\v -> v { vCurU = u, vClock = vClock v + 1 })
  t <- gets vClock
  editSt u (\s -> s { sD = t, sColor = Grayed })
  yield 5 ("Found " ++ lab u ++ " (d = " ++ show t ++ ")")
  forM_ (adjOf adj u) $ \a -> do
    let x = adjV a
    modify (\v -> v { vCurU = u, vCurV = x })
    yield 6 (lab u ++ " -> " ++ lab x)
    c <- gets (sColor . (`stOf` x))
    if c == White
      then visit g adj x >> modify (\v -> v { vCurU = u })
      else when (c == Grayed) $ do
        modify (\v -> v { vCycle = True })
        yield 8 (lab x ++ " is gray: back edge = a cycle!")
  modify (\v -> v { vCurV = -1, vClock = vClock v + 1 })
  t2 <- gets vClock
  editSt u (\s -> s { sColor = Blacked, sF = t2 })
  yield 9 (lab u ++ " finished (f = " ++ show t2 ++ ")")
  modify (\v -> v { vList = u : vList v })
  yield 10 ("Put " ++ lab u ++ " at the front of the list")

kahn :: Graph -> Algo Vars ()
kahn g = do
  let adj = adjList g
      lab = labelOf g
      n = nCount g
      counts = foldl' (\m e -> IM.insertWith (+) (eV e) 1 m) (IM.fromList [(i, 0) | i <- [0 .. n - 1]]) (gEdges g)
  modify (\v -> v { vIn = counts })
  yield 1 "Count the incoming arrows"
  modify (\v -> v { vQ = [u | u <- [0 .. n - 1], IM.findWithDefault 0 u counts == 0] })
  yield 2 "Points with 0 arrows (ready now) go in the queue"
  let loop = do
        q <- gets vQ
        case q of
          [] -> pure ()
          (u : rest) -> do
            yield 3 "The queue is not empty"
            modify (\v -> v { vQ = rest, vCurU = u, vList = vList v ++ [u] })
            editSt u (\s -> s { sColor = Blacked })
            yield 4 ("Take " ++ lab u ++ ", append it to the list")
            forM_ (adjOf adj u) $ \a -> do
              let x = adjV a
              modify (\v -> v { vCurV = x, vIn = IM.adjust (subtract 1) x (vIn v) })
              left <- gets (IM.findWithDefault 0 x . vIn)
              yield 6 ("Erase " ++ lab u ++ " -> " ++ lab x ++ " (" ++ show left ++ " left)")
              when (left == 0) $ do
                modify (\v -> v { vQ = vQ v ++ [x] })
                editSt x (\s -> s { sColor = Grayed })
                yield 7 (lab x ++ " has 0 arrows: into the queue")
            modify (\v -> v { vCurV = -1 })
            loop
  loop
  modify (\v -> v { vCurU = -1, vCycle = length (vList v) < n })
  cyc <- gets vCycle
  yield 8 (if cyc then "Points are left over: there is a cycle" else "All points taken out")
  say (if cyc then "There is a cycle: no valid order exists" else "Finished: a line where all arrows point right")

markColor :: Mark -> Color
markColor White = cWhite
markColor Grayed = cGray
markColor Blacked = cBlack

-- 一列に並べたときの位置
posOf :: Env -> World -> Int -> Point
posOf env w i =
  let g = aG w
      v = aV w
      n = nCount g
      p@(px, py) = xyOf g i
  in case elemIndex i (vList v) of
       Just k | rFinished (aRun w) && not (vCycle v) && length (vList v) == n ->
         let tx = 60 + (fromIntegral k + 0.5) * ((envW env - 120) / fromIntegral n)
             ty = envH env - 205
             l = vLineT v
             t = l * l * (3 - 2 * l)
         in (lerpF px tx t, lerpF py ty t)
       _ -> p

drawWorld :: Env -> World -> Picture
drawWorld env w = view env (concat [[title], edges, [editRubber env w], nodes, lists, [cycleText, statusLine 40 (sh - 34) r, drawCode code (rLine r) (sw - 540) 110 520], notes])
  where
    g = aG w
    v = aV w
    r = aRun w
    sw = envW env
    sh = envH env
    useK = vKahn v
    code = if useK then codeKahn else codeDfs
    pos = posOf env w
    title = hud ("03 Topological Sort - " ++ (if useK then "Kahn (in-degree)" else "DFS (finish time)"))
      [ "Space=play  Right=step  A=DFS <-> Kahn  1=cooking curry  2=courses  Drag=arrow  Click=point  R=restart  C=clear"
      , "Arrow u -> v means \"u before v\". When finished, all points move into a line with every arrow pointing right" ]
    lined = rFinished r && not (vCycle v) && length (vList v) == nCount g && vLineT v > 0
    indexIn x = maybe 0 fromIntegral (elemIndex x (vList v)) :: Float
    edges =
      [ drawEdge (pos (eU e)) (pos (eV e)) edgeO
          { oDirected = True, oR = 30
          , oBend = if lined then negate (abs (indexIn (eV e) - indexIn (eU e))) * 9 * vLineT v else 0
          , oCol = if look then cYel else if gone then alpha255 60 cEdge else cEdge
          , oW = if look then 4 else 2 }
      | e <- gEdges g
      , let look = eU e == vCurU v && eV e == vCurV v
            gone = useK && sColor (stOf v (eU e)) == Blacked && not lined ]
    nodes =
      [ drawNode (pos i) (nLabel nd) (markColor (sColor s)) nodeO
          { nR = 30
          , nRing = if i == vCurU v then Just cYel else Nothing
          , nBelow = if useK
              then (if hasRunner w then Just ("in=" ++ show (IM.findWithDefault 0 i (vIn v))) else Nothing)
              else if sD s > 0 then Just (show (sD s) ++ "/" ++ (if sF s > 0 then show (sF s) else "")) else Nothing }
      | (i, nd) <- zip [0 ..] (gNodes g), let s = stOf v i ]
    -- リスト
    ly = sh - 120
    lists =
      drawBoxes (if useK then "List (append)" else "List (prepend)")
        [colItem cAccent (labelOf g u ++ (if useK then "" else " " ++ show (sF (stOf v u)))) | u <- vList v]
        40 ly (if useK then sw / 2 - 60 else sw - 80)
      : [drawBoxes "Queue Q" [colItem cGray (labelOf g u) | u <- vQ v] (sw / 2 + 20) ly (sw / 2 - 60) | useK]
    cycleText = if vCycle v then txt 40 (ly - 16) 20 cAccent "Cycle found: in any order, some arrow points backwards" else Blank
    y = 110 + codeH code
    noteLines = if useK
      then [ "0 incoming arrows = everything required is done"
           , "Another way of taking from Q: another valid order"
           , "Running time: Theta(V + E)" ]
      else [ "Why larger f first? u -> v means v.f < u.f"
           , "White v ends first (descendant). Black v already ended"
           , "Gray is impossible (that is a cycle). Theta(V + E)" ]
    notes = zipWith (\i t -> txt (sw - 540) (y + 24 + i * 20) 14 cDim t) [0 ..] noteLines

begin :: World -> Maybe (Algo Vars ())
begin w
  | nCount (aG w) == 0 = Nothing
  | vKahn (aV w) = Just (kahn (aG w))
  | otherwise = Just (topoDfs (aG w))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventResize _ -> pure (preset (sizeOf env) (vKahn (aV w)) 1)
  EventKey (MouseButton LeftButton) Down _ _ -> pure (editPress m w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure $ case editRelease 1 m w of
    (w1, NoEdit) -> w1
    (w1, _) -> resetRun w1
  EventMotion _ | envDown env -> pure (editDrag m w)
  _ -> do
    r <- runKeys env resetRun begin (drawWorld env w) ev w
    pure $ case r of
      Just w1 -> w1
      Nothing -> case charKey ev of
        Just 'a' -> resetRun w { aV = (aV w) { vKahn = not (vKahn (aV w)) } }
        Just '1' -> preset (sizeOf env) (vKahn (aV w)) 1
        Just '2' -> preset (sizeOf env) (vKahn (aV w)) 2
        Just 'r' -> resetRun w
        Just 'c' -> resetRun w { aG = (aG w) { gNodes = [], gEdges = [] } }
        _ -> w
  where m = mouseOf env

-- ---------- やってみよう ----------
-- 1. DFS 版と Kahn 版で順番は同じ？　違っても両方とも正しいことを確かめよう
-- 2. Serve（盛り付け）→ Wash（米研ぎ）の矢印を足すと閉路ができる。両方の方法で何が起きる？
-- 3. Tea（お茶）はどこに入ってもよい。正しい順番は全部で何通りある？

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
