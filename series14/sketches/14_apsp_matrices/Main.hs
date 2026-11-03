-- ============================================
-- 14 全点対最短路：行列で考える
-- すべての点の組 (i, j) の最短距離を表（行列）にする
--  1：L(m) ＝ 辺 m 本以内の最短距離。⊗（min と + の「行列のかけ算」）で 1 本ずつ伸ばす  Θ(V⁴)
--  2：くり返し 2 乗 L(2m) ＝ L(m) ⊗ L(m) で倍々に伸ばす  Θ(V³ log V)
--  3：フロイド–ワーシャル D(k) ＝ 途中で通ってよい点を 1〜k に増やしていく  Θ(V³)
--  4：推移的閉包 T(k) ＝ 「行けるか行けないか」だけを OR と AND で
-- 操作：スペース＝再生・停止 / →＝1 手 / 1〜4＝方法 / 線の数字クリック＝重み / R＝最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, unless)
import Data.Char (toLower)
import Data.List (find, foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

-- 行列は (行, 列) → 値 の表
type Mat = Map.Map (Int, Int) Float

data Vars = Vars
  { vMode   :: !Int
  , vM      :: !Mat      -- 右端（いま作っている）行列
  , vWm     :: !Mat      -- 重み行列 W
  , vP      :: !(Maybe Mat)  -- ⊗ の右側
  , vLprev  :: !Mat      -- ⊗ の左側、または 1 つ前の D
  , vPi     :: !Mat      -- 直前の点の表 Π（-1 = なし）
  , vCi     :: !Int
  , vCj     :: !Int
  , vCk     :: !Int
  , vUpd    :: !Bool
  , vMLabel :: !String
  , vKLabel :: !Int
  }

type World = App Vars

codeOf :: Int -> [String]
codeOf 1 =
  [ "Extend one edge at a time(W)"
  , "  L(1) = W (table of paths with at most 1 edge)"
  , "  for m from 2 to n - 1"
  , "      L(m) = L(m-1) (x) W"
  , "A (x) B  (matrix product with min and +)"
  , "  start each cell (i, j) at inf"
  , "      try every middle point k from 1 to n"
  , "          (i, j) = min((i, j), A[i][k] + B[k][j])" ]
codeOf 2 =
  [ "Repeated squaring(W)"
  , "  L(1) = W, m = 1"
  , "  while m < n - 1"
  , "      L(2m) = L(m) (x) L(m) (multiply by itself)"
  , "      double m"
  , "(x) is the same as in 1"
  , "  (i, j) = min over k of (A[i][k] + A[k][j])"
  , "  (+ -> times, min -> sum gives the usual product)" ]
codeOf 3 =
  [ "Floyd-Warshall(W)"
  , "  D(0) = W (no middle points)"
  , "  for k from 1 to n (allow one more middle point)"
  , "      for row i from 1 to n"
  , "          for column j from 1 to n"
  , "              d[i][j] = min(d[i][j], d[i][k] + d[k][j])"
  , "              (if shorter: pi[i][j] = pi[k][j])"
  , "  D(n) is the answer" ]
codeOf _ =
  [ "Transitive closure(G)"
  , "  t[i][j] = 1 if i = j or edge i -> j, else 0"
  , "  for k from 1 to n (allow one more middle point)"
  , "      for row i from 1 to n"
  , "          for column j from 1 to n"
  , "              t[i][j] = t[i][j] OR (t[i][k] AND t[k][j])"
  , "  T(n) is the answer"
  , "  (bit operations make this fast)" ]

main :: IO ()
main = runSketch (sketch "14-14 All-Pairs Shortest Paths" (preset screen0 3))
  { skBack  = cBg
  , skStep  = \env _ w -> pure (autoRun env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

preset :: Size -> Int -> World
preset (_, sh) k = resetRun (App g area ed0 (run0 "") vars0)
  where
    vars0 = Vars k Map.empty Map.empty Nothing Map.empty Map.empty (-1) (-1) (-1) False "" 0
    area = Area 70 (sh - 280) 380 200
    g | k == 4 = -- 4 点：4 からは全部へ行けるが、4 へはだれも行けない
          addEdgesW [(0, 1, 1), (1, 2, 1), (2, 1, 1), (3, 0, 1)]
            (placeGraph [(0.5, 0), (1, 0.5), (0.5, 1), (0, 0.5)] area ["1", "2", "3", "4"] (makeGraph True))
      | otherwise = -- 5 点・負の重みあり（負の閉路はなし）
          addEdgesW [(0, 1, 4), (0, 2, -2), (1, 3, 3), (2, 1, 5), (2, 4, 6), (3, 0, 1), (3, 2, 7), (4, 3, -3), (1, 4, 2), (4, 0, 8)]
            (placeGraph [(0, 0.2), (0.55, 0), (1, 0.5), (0.6, 1), (0.05, 0.9)] area ["1", "2", "3", "4", "5"] (makeGraph True))

weightMatrix :: Graph -> Mat
weightMatrix g = Map.fromList
  [ ((i, j), if i == j then 0 else maybe inf (fromIntegral . eW) (find (\f -> eU f == i && eV f == j) (gEdges g)))
  | i <- ids, j <- ids ]
  where ids = [0 .. nCount g - 1]

boolMatrix :: Mat -> Mat
boolMatrix = Map.mapWithKey (\(i, j) x -> if i == j || x < inf then 1 else 0)

resetRun :: World -> World
resetRun w = w
  { aRun = stopRun "Space to start" (aRun w)
  , aV = (aV w)
      { vWm = wm, vM = m0, vLprev = m0, vP = Nothing
      , vPi = Map.mapWithKey (\(i, j) x -> if i /= j && x < inf then fromIntegral i else -1) wm
      , vCi = -1, vCj = -1, vCk = -1, vUpd = False
      , vMLabel = if mode == 3 then "D(0)" else if mode == 4 then "T(0)" else "L(1)", vKLabel = 0 }
  }
  where
    mode = vMode (aV w)
    wm = weightMatrix (aG w)
    m0 = if mode == 4 then boolMatrix wm else wm

cell :: Mat -> Int -> Int -> Float
cell m i j = Map.findWithDefault inf (i, j) m

-- A ⊗ B（min と + のかけ算）。1 マスごとに休む
extend :: Int -> Mat -> Mat -> String -> String -> String -> Algo Vars Mat
extend n a b nameL nameR nameOut = do
  mode <- gets vMode
  let ids = [0 .. n - 1]
      start = Map.fromList [((i, j), inf) | i <- ids, j <- ids]
  modify (\v -> v { vLprev = a, vP = Just b, vM = start, vMLabel = nameOut })
  forM_ [(i, j) | i <- ids, j <- ids] $ \(i, j) -> do
    let val = foldl' min inf [cell a i k + cell b k j | k <- ids]
    modify (\v -> v { vCi = i, vCj = j, vCk = -1, vUpd = True, vM = Map.insert (i, j) val (vM v) })
    yield (if mode == 1 then 7 else 6)
      (nameOut ++ "[" ++ show (i + 1) ++ "][" ++ show (j + 1) ++ "] = min_k(" ++ nameL ++ "[" ++ show (i + 1) ++ "][k] + " ++ nameR ++ "[k][" ++ show (j + 1) ++ "]) = " ++ fmt val)
  modify (\v -> v { vCi = -1, vCj = -1 })
  gets vM

runAlgo :: Graph -> Algo Vars ()
runAlgo g = do
  mode <- gets vMode
  let n = nCount g
      w0 = weightMatrix g
      ids = [0 .. n - 1]
      name s m = s ++ "(" ++ show (m :: Int) ++ ")"
  case mode of
    1 -> do
      modify (\v -> v { vMLabel = "L(1)", vM = w0 })
      yield 1 "L(1) = W (at most 1 edge)"
      let loop m l
            | m > n - 1 = pure ()
            | otherwise = do
                yield 3 (name "L" m ++ " = " ++ name "L" (m - 1) ++ " (x) W (at most " ++ show m ++ " edges)")
                l1 <- extend n l w0 (name "L" (m - 1)) "W" (name "L" m)
                loop (m + 1) l1
      loop 2 w0
      say ("Finished: L(n-1) gives shortest distances (" ++ show (n - 2) ++ " products, each Theta(n^3))")
    2 -> do
      modify (\v -> v { vM = w0 })
      yield 1 "L(1) = W"
      let loop m l
            | m >= n - 1 = pure ()
            | otherwise = do
                yield 3 (name "L" (2 * m) ++ " = " ++ name "L" m ++ " (x) " ++ name "L" m ++ " (square)")
                l1 <- extend n l l (name "L" m) (name "L" m) (name "L" (2 * m))
                loop (2 * m) l1
      loop 1 w0
      say "Finished: log n squarings are enough (L(m) stops changing at m >= n-1)"
    _ -> do
      let bool = mode == 4
          d0 = if bool then boolMatrix w0 else w0
          nm = if bool then "T" else "D"
          tag = if bool then "t" else "d"
      modify (\v -> v { vM = d0, vP = Nothing, vMLabel = name nm 0 })
      yield 1 (if bool then "T(0): 1 if there is an edge or i = j" else "D(0) = W (no middle points)")
      forM_ ids $ \k -> do
        modify (\v -> v { vCk = k, vKLabel = k + 1, vLprev = vM v, vMLabel = name nm (k + 1) })
        yield 2 ("k = " ++ show (k + 1) ++ ": point " ++ show (k + 1) ++ " may now be used as a middle point")
        prev <- gets vLprev
        forM_ [(i, j) | i <- ids, j <- ids] $ \(i, j) -> do
          let pij = cell prev i j
              pik = cell prev i k
              pkj = cell prev k j
              via = if bool then (if pik /= 0 && pkj /= 0 then 1 else 0) else pik + pkj
              upd = if bool then pij == 0 && via /= 0 else via < pij
          modify (\v ->
            let v1 = v { vCi = i, vCj = j, vUpd = upd }
            in if upd
                 then v1 { vM = Map.insert (i, j) (if bool then 1 else via) (vM v1)
                         , vPi = if bool then vPi v1 else Map.insert (i, j) (cell (vPi v1) k j) (vPi v1) }
                 else v1)
          unless (i == k || j == k) $ do  -- 行 k・列 k は変わらない
            now <- gets (\v -> cell (vM v) i j)
            yield 5 (tag ++ show (i + 1) ++ show (j + 1) ++ " = "
              ++ (if bool
                    then "OR(" ++ fmt pij ++ ", " ++ fmt pik ++ " AND " ++ fmt pkj ++ ")"
                    else "min(" ++ fmt pij ++ ", " ++ fmt pik ++ " + " ++ fmt pkj ++ ")")
              ++ " = " ++ fmt now ++ (if upd then "  updated" else ""))
      modify (\v -> v { vCi = -1, vCj = -1, vCk = -1 })
      say (if bool then "Finished: 1 in T(n) means reachable" else "Finished: D(n) = shortest distances, Pi rebuilds the paths (3 loops!)")

-- 行列を 1 つ描く
data MatO = MatO
  { mCellHot :: !Bool
  , mRow     :: !Int
  , mCol     :: !Int
  , mShow    :: Float -> String
  , mColor   :: Float -> Color
  }

matO :: MatO
matO = MatO False (-1) (-1) fmt (\x -> if x < 0 then cAccent else cText)

drawMatrix :: Vars -> Int -> Mat -> Float -> Float -> Float -> String -> MatO -> [Picture]
drawMatrix v n a x y cs title o =
  txt x (y - 10) 18 cText title
  : concat
      [ [ txtC (x + cs * (fi i + 1.5)) (y + cs * 0.5) 14 cDim (show (i + 1))
        , txtC (x + cs * 0.5) (y + cs * (fi i + 1.5)) 14 cDim (show (i + 1)) ]
      | i <- [0 .. n - 1] ]
  ++ concat
      [ [ box cBg px py cs cs
        , box fillC (px + 1) (py + 1) (cs - 2) (cs - 2)
        , txtC (px + cs / 2) (py + cs / 2) (max 14 (cs * 0.36)) (if hot then cBg else mColor o val) (mShow o val) ]
      | i <- [0 .. n - 1], j <- [0 .. n - 1]
      , let px = x + cs * (fi j + 1)
            py = y + cs * (fi i + 1)
            val = cell a i j
            hot = mCellHot o && i == vCi v && j == vCj v
            lit = i == mRow o || j == mCol o
            fillC | hot = if vUpd v then cGreen else cYel
                  | lit = hex 0x3a2f55
                  | otherwise = hex 0x1c2036 ]
  where fi = fromIntegral :: Int -> Float

-- ⊗ の記号（丸にバツ）
otimes :: Float -> Float -> Picture
otimes x y = Pictures
  [ Color cYel (at (x, y) (ring 14 3))
  , seg cYel 3 (x - 10, y - 10) (x + 10, y + 10)
  , seg cYel 3 (x - 10, y + 10) (x + 10, y - 10) ]

drawWorld :: Env -> World -> Picture
drawWorld env w = view env (concat [[title], matrices, edges, nodes, [hint, statusLine 40 (sh - 34) r, drawCode (codeOf mode) (rLine r) (sw - 560) (if mode <= 2 then sh - 300 else 150) 540]])
  where
    g = aG w
    v = aV w
    r = aRun w
    sw = envW env
    sh = envH env
    mode = vMode v
    n = nCount g
    nf = fromIntegral n :: Float
    ci = vCi v
    cj = vCj v
    ck = vCk v
    gy = aY (aGA w)
    modeName = case mode of
      1 -> "1 (x) n-2 times"
      2 -> "2 repeated squaring"
      3 -> "3 Floyd-Warshall"
      _ -> "4 transitive closure"
    title = hud ("14 All-Pairs Shortest Paths - " ++ modeName)
      [ "Space=play  Right=step  1=one edge at a time  2=squaring  3=Floyd-Warshall  4=closure  Click number=weight  R=restart"
      , if mode >= 3
          then "Purple row and column = values used through point k. Green = a cell got shorter (connected)"
          else "Add the left row i to the right column j and take the minimum: that makes one cell on the far right" ]
    x = 40
    y = 150
    matrices
      | mode <= 2 =
          let cs = minimum [54, (sw - 160) / (3 * (nf + 1) + 3), (sh - 470) / (nf + 1)]
          in drawMatrix v n (vLprev v) x y cs (if mode == 1 then "L (before)" else "L(m)") matO { mRow = ci }
             ++ [otimes (x + cs * (nf + 1) + cs * 0.6) (y + cs * (nf + 2) / 2)]
             ++ drawMatrix v n (maybe (vWm v) id (vP v)) (x + cs * (nf + 2.2)) y cs (if mode == 1 then "W" else "L(m)") matO { mCol = cj }
             ++ [txtC (x + cs * (2 * nf + 3.4)) (y + cs * (nf + 2) / 2) 30 cYel "="]
             ++ drawMatrix v n (vM v) (x + cs * (2 * nf + 4.4)) y cs (vMLabel v) matO { mCellHot = True }
      | otherwise =
          let cs = minimum [58, (sw - 640) / (2 * (nf + 1) + 1), (sh - 470) / (nf + 1)]
              bool = mode == 4
              main' = matO { mCellHot = True, mRow = ck, mCol = ck }
          in drawMatrix v n (vM v) x y cs (vMLabel v)
               (if bool then main' { mColor = \t -> if t /= 0 then cCyan else cDim } else main')
             ++ (if bool then [] else
                   drawMatrix v n (vPi v) (x + cs * (nf + 2)) y cs ("Pi(" ++ show (vKLabel v) ++ ") (previous point)")
                     matO { mShow = \t -> if t < 0 then "-" else show (round t + 1 :: Int), mColor = const cViolet })
    edges =
      [ drawEdge (xyOf g (eU e)) (xyOf g (eV e)) edgeO
          { oDirected = True, oR = 20, oBend = if hasReverse g e then 12 else 0
          , oCol = if on then cYel else cEdge, oW = if on then 4 else 2
          , oLabel = if mode == 4 then "" else show (eW e), oLabelCol = if eW e < 0 then cAccent else cText }
      | e <- gEdges g
      , let on = mode >= 3 && ck >= 0 && ci >= 0 && ((eU e == ci && eV e == ck) || (eU e == ck && eV e == cj)) ]
    nodes =
      [ drawNode (nX nd, nY nd) (nLabel nd) (if i == ck then cViolet else if i == ci then cYel else if i == cj then cGreen else cWhite) nodeO { nR = 20 }
      | (i, nd) <- zip [0 ..] (gNodes g) ]
    hint = if mode >= 3 && ci >= 0 && ck >= 0
      then txt (aX (aGA w)) (gy - 30) 14 cYel ("Yellow " ++ show (ci + 1) ++ " -> purple " ++ show (ck + 1) ++ " -> green " ++ show (cj + 1) ++ ": is this detour shorter?")
      else Blank

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventResize _ -> pure (preset (sizeOf env) (vMode (aV w)))
  EventKey (MouseButton LeftButton) Down mods _
    | vMode (aV w) /= 4 -> pure $ case weightClick (shift mods == Down) (-5) 10 (mouseOf env) (aG w) of
        Just g1 -> resetRun w { aG = g1 }
        Nothing -> w
  _ -> do
    r <- runKeys env resetRun (Just . runAlgo . aG) (drawWorld env w) ev w
    pure $ case r of
      Just w1 -> w1
      Nothing -> case charKey ev of
        Just c | c `elem` "1234" -> preset (sizeOf env) (fromEnum c - fromEnum '0')
        Just 'r' -> resetRun w
        _ -> w

-- ---------- やってみよう ----------
-- 1. 3 の結果の 1 行目が 0 3 −2 1 4 になるか確かめよう（1 → 3 → 5 → 4 の回り道に注目）
-- 2. 1 と 2 で同じ答えになる？ かかった ⊗ の回数は？
-- 3. 負の閉路があると、対角成分 dᵢᵢ が負になる。重みを変えて作ってみよう

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
