-- ============================================
-- 04 V-パス ─ 矢印をたどる、閉路があったら勾配場ではない
-- V-パス = 臨界胞体から矢印を「渡っては降りる」をくり返す列（面 → 辺 → 面 → 辺 … / 辺 → 頂点 → 辺 → 頂点 …）
--   矢印の集まりが「離散勾配ベクトル場」と呼べるのは、閉じた V-パス（閉路）がないときだけ
--   A のランダムな矢印にはたいてい閉路がある（赤く点滅）。G の高さ関数から作った矢印には絶対にない
-- A = ランダムな矢印 / G = 高さ関数からの勾配場 / クリック = 臨界胞体を選んで V-パスを見る / P = 全部のパス
-- ============================================
module Main (main) where

import Art
import Art
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', mapAccumL, nub, sort, sortBy, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showFFloat)

nx, ny :: Int
nx = 12             -- ★ 格子の大きさ
ny = 7

cs, ox, oy :: Float
cs = 76
ox = 60
oy = 120

cx :: Cx
cx = makeGridComplex nx ny Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx ox oy cs

data World = World
  { wPair     :: !(U.Vector Int)
  , wCycles   :: [[Int]]
  , wPaths    :: [(Int, [Int])]   -- (出発する臨界胞体, V-パス)。矢印が変わったときだけ計算する
  , wSelected :: !Int
  , wShowAll  :: !Bool
  , wMsg      :: !String
  }

main :: IO ()
main = runSketch (sketch "37-04 V-Paths" (setPair (U.replicate (nCells cx) (-1)) ""))
  { skBack  = hsb 225 30 8
  , skStart = \_ -> fromHeight True
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

setPair :: U.Vector Int -> String -> World
setPair pair msg = World pair (findCycles pair) paths (-1) True msg
  where
    paths = [ (c, p) | c <- [0 .. nCells cx - 1], pair U.! c < 0, cDim (cell cx c) > 0
                     , p <- take 400 (descendPaths cx pair 300 c) ]

fromHeight :: Bool -> IO World
fromHeight showAll = do
  s <- newSeed
  let h = heightFrom cx (\i j -> noiseP s (fi i * 0.45) (fi j * 0.45) + 0.0001 * (fi i * 3 + fi j))
  pure (setPair (gradientFromHeight cx h) "From height: arrows always go down, so no loops") { wShowAll = showAll }

randomField :: Bool -> IO World
randomField showAll = do
  let links = [(a, b) | a <- [0 .. nCells cx - 1], b <- cCofaces (cell cx a)]
  keyed <- mapM (\l -> (,,) <$> randomRIO (0, 1 :: Float) <*> randomRIO (0, 1 :: Float) <*> pure l) links
  let place (used, acc) (_, r, (a, b))
        | not (IS.member a used) && not (IS.member b used) && r < 0.9 = (IS.insert a (IS.insert b used), (a, b) : (b, a) : acc)
        | otherwise = (used, acc)
      (_, ups) = foldl' place (IS.empty, []) (sortOn (\(k, _, _) -> k) keyed)
      w = setPair (U.replicate (nCells cx) (-1) U.// ups) ""
  pure w { wMsg = "Random arrows: " ++ show (length (wCycles w)) ++ " loops", wShowAll = showAll }

-- ★ 閉路を探す：胞体 c から「矢印の先の胞体の、c 以外の面」へ進むグラフで深さ優先探索
findCycles :: U.Vector Int -> [[Int]]
findCycles pair = reverse (thd (foldl' start (IS.empty, IS.empty, []) [0 .. nCells cx - 1]))
  where
    thd (_, _, c) = c
    next c = let p = pair U.! c
             in if p >= 0 && cDim (cell cx p) > cDim (cell cx c) then filter (/= c) (cFaces (cell cx p)) else []
    start st@(gray', black, _) c
      | IS.member c gray' || IS.member c black = st
      | otherwise = dfs [] st c
    dfs stack (g, bl, cy) c =
      let stack' = c : stack
          visit st@(g1, b1', cy1) d
            | IS.member d b1' = st
            | IS.member d g1 =                              -- 戻る辺 = 閉路
                let cyc = reverse (d : takeWhile (/= d) stack')
                in (g1, b1', concatMap (\e -> [e, pair U.! e]) cyc : cy1)
            | otherwise = dfs stack' st d
          (g2, b2', cy2) = foldl' visit (IS.insert c g, bl, cy) (next c)
      in (IS.delete c g2, IS.insert c b2', cy2)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  drawGridComplex cx pair ox oy cs True False
  ++ [ polyL (if from == wSelected w then 6 else 2.5) (hsba (if cDim (cell cx from) == 1 then 190 else 30) 80 100 0.85) (map (pos U.!) p)
     | (from, p) <- shown ]
  ++ (if not (null (wCycles w)) && envFrame env `mod` 40 < 24            -- 閉路（赤く点滅）
        then [ polyL 7 (hsb 0 90 100) (map (pos U.!) (cyc ++ take 1 cyc)) | cyc <- wCycles w ] else [])
  ++ [ ringC white (pos U.! wSelected w) (cs * 0.5) 3 | wSelected w >= 0 ]
  ++ [ if null (wCycles w)
         then txt ox 44 20 white ("OK: no loops, a gradient field.  m0 = " ++ show m0 ++ "  m1 = " ++ show m1 ++ "  m2 = " ++ show m2 ++ "  V-paths " ++ show (length shown))
         else txt ox 44 20 white (show (length (wCycles w)) ++ " loops: not a gradient field (critical m = " ++ showM m ++ " means little)")
     , txt ox 72 15 (gray 0.65) ("A: random arrows  G: from height  Click: choose a critical cell  P: all paths " ++ (if wShowAll w then "ON" else "off"))
     , txt ox 96 15 (hsb 48 60 100) (wMsg w)
     , txt ox (oy + fi ny * cs + 36) 14 (gray 0.55) "Blue = paths from a critical edge (saddle) to vertices.  Orange = paths from a critical face (max).  Red = closed V-path."
     ]
  where
    pair = wPair w
    m@(m0, m1, m2) = countCritical cx pair
    shown = [ q | q@(from, _) <- wPaths w, wShowAll w || from == wSelected w ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let mp = mouseP env
        pick (b, bd) c = let d = dist2 mp (pos U.! c) in if d < bd then (c, d) else (b, bd)
        (best, _) = foldl' pick (-1, cs * 0.4) [c | c <- [0 .. nCells cx - 1], wPair w U.! c < 0]
    in pure w { wSelected = best
              , wMsg = if best >= 0 then "V-paths from a critical cell of dimension " ++ show (cDim (cell cx best)) else wMsg w }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> randomField (wShowAll w)
    | c `elem` "gG" -> fromHeight (wShowAll w)
    | c `elem` "pP" -> pure w { wShowAll = not (wShowAll w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. A を押して閉路を見つけよう。閉路は「面 → 辺 → 面 → 辺」型と「辺 → 頂点 → 辺 → 頂点」型のどちら？
-- 2. G の勾配場で、1 つの鞍点（臨界な辺）から出る V-パスは必ず 2 本。なぜ？
-- 3. 極大（臨界な面）から出る V-パスは何本？ 枝分かれするのはどこ？
-- 4. 閉路がないとき、矢印を全部たどると必ず臨界胞体で止まる。この性質を使うのが 05〜08

-- ---------- 離散モース理論の道具 ----------

-- p5.js と同じ座標（左上が原点、y は下向き）で描くための小道具。screen で全体を画面に合わせる
scrW, scrH :: Float
scrW = 1280
scrH = 800

screen :: Env -> Picture -> Picture
screen env = Translate (-envW env / 2) (envH env / 2)

toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = toP5 env (envMouse env)

fl :: Point -> Point
fl (x, y) = (x, -y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (-y)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (thickLine w (fl a) (fl b))

-- 太さ w の折れ線
polyL :: Float -> Color -> [Point] -> Picture
polyL w c ps = Color c (thickPath w (map fl ps))

-- 左上 (x, y)、幅 w、高さ h の長方形
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (-(y + h / 2)) w h)

-- 直径 d の円（p5.js の circle）
dotC :: Color -> Point -> Float -> Picture
dotC c p d = Color c (at (fl p) (disc (d / 2)))

-- 直径 d、線の太さ w の輪
ringC :: Color -> Point -> Float -> Float -> Picture
ringC c p d w = Color c (at (fl p) (ring (d / 2) w))

fmt :: Int -> Float -> String
fmt n x = showFFloat (Just n) x ""

fi :: Int -> Float
fi = fromIntegral

-- p5.js の noise に近いノイズ。oct = 重ねる回数、fall = 減衰（noiseDetail）、s = ずらす量（noiseSeed の代わり）
pnoise :: Int -> Float -> Float -> Float -> Float -> Float
pnoise oct fall s x y = go oct 1 fall 0
  where
    go :: Int -> Float -> Float -> Float -> Float
    go 0 _ _ acc = acc
    go k f a acc = go (k - 1) (f * 2) (a * fall) (acc + a * clampF 0 1 (0.5 + 2.2 * (noise3 (x * f + s) (y * f + s * 0.61) (0.5 + s * 0.37) - 0.5)))   -- 2.2 倍：p5.js の noise と同じくらいの幅にする

-- noiseDetail を変えていないときの noise
noiseP :: Float -> Float -> Float -> Float
noiseP = pnoise 4 0.5

newSeed :: IO Float
newSeed = randomRIO (0, 200)

-- 格子（nx × ny マス）の胞体複体。Ident は辺の同一視、mask i j が False のマス（面）は入れない
--   胞体は 頂点 → 辺 → 面 の順に番号がつく。cxV・cxEh（横の辺）・cxEv（縦の辺）・cxF = 格子の位置 → 胞体番号（なければ -1）
data Ident = Plain | TorusGlue | SphereGlue | KleinGlue deriving Eq

data Cell = Cell
  { cDim     :: !Int
  , cVerts   :: ![Int]
  , cFaces   :: ![Int]
  , cCofaces :: ![Int]
  , cNcof    :: !Int     -- 面に接する回数（折り返しは 2 回）
  }

data Cx = Cx
  { cxNx, cxNy :: !Int
  , cxCells :: !(V.Vector Cell)
  , cxV, cxEh, cxEv, cxF :: !(U.Vector Int)
  }

type CellKey = (Char, Int, Int)
data Build = Build !(M.Map CellKey Int) [(Int, [Int])]

cell :: Cx -> Int -> Cell
cell cx c = cxCells cx V.! c

nCells :: Cx -> Int
nCells = V.length . cxCells

gridAt :: Cx -> U.Vector Int -> Int -> Int -> Int
gridAt cx t i j = t U.! (i * (cxNy cx + 1) + j)

vid, ehAt, evAt, fid :: Cx -> Int -> Int -> Int
vid cx = gridAt cx (cxV cx)
ehAt cx = gridAt cx (cxEh cx)
evAt cx = gridAt cx (cxEv cx)
fid cx = gridAt cx (cxF cx)

getCell :: CellKey -> Int -> [Int] -> Build -> (Build, Int)
getCell k d vs b@(Build m cs) = case M.lookup k m of
  Just c  -> (b, c)
  Nothing -> let c = M.size m in (Build (M.insert k c m) ((d, vs) : cs), c)

-- 起動のときに複体を作りきっておく（最初のフレームで止まらないように）
forceCx :: Cx -> Int
forceCx = V.foldl' (\a c -> a + cDim c + length (cVerts c) + length (cFaces c) + length (cCofaces c)) 0 . cxCells

makeGridComplex :: Int -> Int -> Ident -> (Int -> Int -> Bool) -> Cx
makeGridComplex nx ny ident mask = Cx nx ny (V.fromList cells) vT ehT evT fT
  where
    grid = [(i, j) | i <- [0 .. nx], j <- [0 .. ny]]
    keep i j = i >= 0 && j >= 0 && i < nx && j < ny && mask i j
    canon i j = case ident of
      TorusGlue -> (i `mod` nx, j `mod` ny)
      KleinGlue -> let (a, b) = if i == nx then (0, ny - j) else (i, j) in (a, if b == ny then 0 else b)
      SphereGlue | j == 0 -> (0, i)
                 | i == nx -> (j, ny)
      _ -> (i, j)
    -- 辺は「格子のどの位置の辺か」で同一視する（同じ 2 頂点をもつ別の辺もあるので頂点の組では区別しない）
    canonE i j d = case ident of
      TorusGlue -> (d, i `mod` nx, j `mod` ny)
      KleinGlue | d == 'h' && j == ny -> (d, i, 0)
                | d == 'v' && i == nx -> (d, 0, ny - j - 1)
      SphereGlue | d == 'h' && j == 0 -> ('v', 0, i)
                 | d == 'v' && i == nx -> ('h', j, ny)
      _ -> (d, i, j)
    addV b (i, j)
      | keep i j || keep (i - 1) j || keep i (j - 1) || keep (i - 1) (j - 1) =
          let (a, c) = canon i j in getCell ('p', a, c) 0 [] b
      | otherwise = (b, -1)
    (b1, vids) = mapAccumL addV (Build M.empty []) grid
    vT = U.fromList vids
    ix i j = i * (ny + 1) + j
    vAt i j = vT U.! ix i j
    edge k a c b
      | a < 0 || c < 0 || a == c = (b, -1)
      | otherwise = getCell k 1 [min a c, max a c] b
    addE b (i, j) =
      let (b', h) | i < nx && (keep i j || keep i (j - 1)) = edge (canonE i j 'h') (vAt i j) (vAt (i + 1) j) b
                  | otherwise = (b, -1)
          (b'', e) | j < ny && (keep i j || keep (i - 1) j) = edge (canonE i j 'v') (vAt i j) (vAt i (j + 1)) b'
                   | otherwise = (b', -1)
      in (b'', (h, e))
    (b2, es) = mapAccumL addE b1 grid
    ehT = U.fromList (map fst es)
    evT = U.fromList (map snd es)
    addF b (i, j)
      | i < nx && j < ny && keep i j =
          getCell ('f', i, j) 2 (sort (nub [vAt i j, vAt (i + 1) j, vAt i (j + 1), vAt (i + 1) (j + 1)])) b
      | otherwise = (b, -1)
    (Build _ rev, fs) = mapAccumL addF b2 grid
    fT = U.fromList fs
    faceEdges i j = filter (>= 0) [ehT U.! ix i j, ehT U.! ix i (j + 1), evT U.! ix i j, evT U.! ix (i + 1) j]
    faceList = [(f, faceEdges i j) | ((i, j), f) <- zip grid fs, f >= 0]
    faceMap = IM.fromList [(f, nub e) | (f, e) <- faceList]
    ncof = IM.fromListWith (+) [(e, 1 :: Int) | (_, l) <- faceList, e <- l]
    raw = zip [0 ..] (reverse rev)
    facesOf (c, (d, vs)) = case d of
      0 -> []
      1 -> vs
      _ -> IM.findWithDefault [] c faceMap
    cof = IM.fromListWith (\new old -> old ++ new) [(f, [c]) | r@(c, _) <- raw, f <- facesOf r]
    cells = [ Cell d (if d == 0 then [c] else vs) (facesOf r) (IM.findWithDefault [] c cof) (IM.findWithDefault 0 c ncof)
            | r@(c, (d, vs)) <- raw ]

-- 頂点の数
nVerts :: Cx -> Int
nVerts cx = V.length (V.filter ((== 0) . cDim) (cxCells cx))

-- 格子の位置 → 高さ から、頂点ごとの高さ h をつくる（穴の頂点はとばす）
heightFrom :: Cx -> (Int -> Int -> Float) -> U.Vector Float
heightFrom cx f = U.replicate (nVerts cx) 0 U.// assocs
  where assocs = [ (v, f i j) | i <- [0 .. cxNx cx], j <- [0 .. cxNy cx], let v = vid cx i j, v >= 0 ]

-- 高さ関数 h[頂点] から離散勾配場（ペアリング）をつくる（lower star 法：Robins ら 2011）
--   返り値 pair[胞体] = 矢印の相手の胞体番号、臨界なら -1
gradientFromHeight :: Cx -> U.Vector Float -> U.Vector Int
gradientFromHeight cx h = U.create $ do
  p <- MU.replicate (nCells cx) (-1)
  forM_ [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0] $ \x ->
    forM_ (lowerStarPairs cx h x) $ \(a, b) -> MU.write p a b >> MU.write p b a
  pure p

-- 胞体の「値」= 頂点の高さを大きい順に並べた列（同じ高さは頂点番号で区別）
gval :: Cx -> U.Vector Float -> Int -> [(Float, Int)]
gval cx h c = sortBy (flip compare) [(h U.! v, v) | v <- cVerts (cell cx c)]

-- いちばん高い頂点
topVert :: Cx -> U.Vector Float -> Int -> Int
topVert cx h c = snd (maximum ((-1 / 0, -1) : [(h U.! v, v) | v <- cVerts (cell cx c)]))

data Star = Star { stDone :: !IS.IntSet, stPq0 :: [Int], stPq1 :: [Int], stPairs :: [(Int, Int)] }

-- x の lower star（x がいちばん高い頂点である胞体）の中だけで、矢印のペアを決める
lowerStarPairs :: Cx -> U.Vector Float -> Int -> [(Int, Int)]
lowerStarPairs cx h x = case sortOn gv (filter ((== 1) . cDim . cell cx) star) of
  [] -> []                                             -- 臨界な頂点（極小）
  (delta : rest) ->                                    -- いちばん低い辺と x をペアに
    stPairs (run (addCof delta (Star (IS.singleton delta) rest [] [(x, delta)])))
  where
    gv = gval cx h
    star = nub (concat [ e : [f | f <- cCofaces (cell cx e), topVert cx h f == x]
                       | e <- cCofaces (cell cx x), topVert cx h e == x ])
    inL = IS.fromList star
    unpaired s a = [b | b <- cFaces (cell cx a), IS.member b inL, not (IS.member b (stDone s))]
    addCof a s0 = foldl' add s0 (cCofaces (cell cx a))
      where add s b
              | IS.member b inL && not (IS.member b (stDone s)) && length (unpaired s b) == 1 && b `notElem` stPq1 s =
                  s { stPq1 = stPq1 s ++ [b] }
              | otherwise = s
    run s = case (stPq0 s, stPq1 s) of
      ([], []) -> s
      _ -> run (popCrit (drain s))
    drain s = case sortOn gv (stPq1 s) of
      [] -> s
      (a : more) -> case unpaired s a of
        [] -> drain s { stPq1 = more, stPq0 = stPq0 s ++ [a] }
        (b : _) -> drain (addCof b (addCof a s
                     { stPq1 = more, stPq0 = filter (/= b) (stPq0 s)
                     , stDone = IS.insert a (IS.insert b (stDone s)), stPairs = (a, b) : stPairs s }))
    popCrit s = case sortOn gv (stPq0 s) of            -- g は臨界
      [] -> s
      (g : more) -> addCof g s { stPq0 = more, stDone = IS.insert g (stDone s) }

-- 臨界胞体の数 (m0, m1, m2)
countCritical :: Cx -> U.Vector Int -> (Int, Int, Int)
countCritical cx pair = (cnt 0, cnt 1, cnt 2)
  where cnt d = length [() | c <- [0 .. nCells cx - 1], pair U.! c < 0, cDim (cell cx c) == d]

showM :: (Int, Int, Int) -> String
showM (a, b, c) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

-- 臨界胞体 c（次元 k）から矢印をたどって下る V-パスをすべて集める（次元 k-1 の臨界胞体で終わる）
descendPaths, ascendPaths :: Cx -> U.Vector Int -> Int -> Int -> [[Int]]
descendPaths = walkPaths cFaces
-- 臨界胞体 c から矢印を逆にたどって上る V-パス（次元 k+1 の臨界胞体で終わる）
ascendPaths = walkPaths cCofaces

-- 閉路に出会ったパスはそこで打ち切る。maxLen = パスの長さの上限（p5.js の省略時は 4000）
walkPaths :: (Cell -> [Int]) -> Cx -> U.Vector Int -> Int -> Int -> [[Int]]
walkPaths next cx pair maxLen c0 = walk c0 [c0] (IS.singleton c0) 1
  where
    walk a rev onPath len = concatMap step (next (cell cx a))
      where
        step b
          | b == pair U.! a = []
          | p < 0 = [reverse (b : rev)]
          | cDim (cell cx p) /= cDim (cell cx a) = []
          | IS.member p onPath = [reverse (p : b : rev)]
          | len < maxLen = walk p (p : b : rev) (IS.insert p onPath) (len + 2)
          | otherwise = []
          where p = pair U.! b

-- パスの最後の胞体
pathEnd :: [Int] -> Int
pathEnd = foldl' (\_ x -> x) (-1)

-- キャンセル：パス c0, c1, c2, … の矢印を (c0,c1), (c2,c3), … に組み替える（向きが逆になる）
cancelUpdates :: [Int] -> [(Int, Int)]
cancelUpdates (a : b : rest) = (a, b) : (b, a) : cancelUpdates rest
cancelUpdates _ = []

cancelPath :: U.Vector Int -> [Int] -> U.Vector Int
cancelPath pair path = pair U.// cancelUpdates path

-- 各胞体の描く位置（頂点 = 格子点、辺 = 中点、面 = 中心。同一視された胞体は最初の位置）
gridPositions :: Cx -> Float -> Float -> Float -> U.Vector Point
gridPositions cx ox oy cs = U.replicate (nCells cx) (0, 0) U.// reverse puts
  where
    f = fromIntegral :: Int -> Float
    puts = concat
      [ [ (c, p) | (c, p) <- [ (vid cx i j, (ox + f i * cs, oy + f j * cs))
                             , (ehAt cx i j, (ox + (f i + 0.5) * cs, oy + f j * cs))
                             , (evAt cx i j, (ox + f i * cs, oy + (f j + 0.5) * cs))
                             , (fid cx i j, (ox + (f i + 0.5) * cs, oy + (f j + 0.5) * cs)) ], c >= 0 ]
      | i <- [0 .. cxNx cx], j <- [0 .. cxNy cx] ]

drawArrow :: Float -> Point -> Point -> Picture
drawArrow cs (x1, y1) (x2, y2) = Pictures [seg (max 1.5 (cs * 0.06)) c (x1, y1) (x2, y2), Color c (Polygon (map fl [(x2, y2), side (-0.5), side 0.5]))]
  where
    a = atan2 (y2 - y1) (x2 - x1)
    hd = min 8 (cs * 0.22)
    c = hsb 48 70 100
    side d = (x2 - hd * cos (a + d), y2 - hd * sin (a + d))

-- 格子複体を描く（左上 ox, oy・1 マス cs）。臨界：頂点 = 紫、辺 = ライム、面 = コーラル
--   showArrows で矢印も描く。onlyCrit なら臨界でない面は塗らない
drawGridComplex :: Cx -> U.Vector Int -> Float -> Float -> Float -> Bool -> Bool -> [Picture]
drawGridComplex cx pair ox oy cs showArrows onlyCrit = faces ++ edges ++ verts ++ (if showArrows then arrows else [])
  where
    nx = cxNx cx
    ny = cxNy cx
    gx i = ox + fromIntegral i * cs
    gy j = oy + fromIntegral j * cs
    crit c = pair U.! c < 0
    half = cs / 2
    faces = [ box col (gx i + 1) (gy j + 1) (cs - 2) (cs - 2)
            | i <- [0 .. nx - 1], j <- [0 .. ny - 1], let f = fid cx i j, f >= 0
            , col <- [hsba 12 80 95 0.75 | crit f] ++ [hsb 225 25 22 | not (crit f), not onlyCrit] ]
    edgeLine e a b | crit e = seg 5 (hsb 85 90 95) a b
                   | otherwise = seg 1.5 (gray 0.42) a b
    edges = concat [ [edgeLine e (gx i, gy j) (gx (i + 1), gy j) | let e = ehAt cx i j, e >= 0]
                     ++ [edgeLine e (gx i, gy j) (gx i, gy (j + 1)) | let e = evAt cx i j, e >= 0]
                   | i <- [0 .. nx], j <- [0 .. ny] ]
    verts = [ if crit v then dotC (hsb 272 70 100) (gx i, gy j) (cs * 0.34) else dotC (gray 0.72) (gx i, gy j) (max 3 (cs * 0.14))
            | i <- [0 .. nx], j <- [0 .. ny], let v = vid cx i j, v >= 0 ]
    arrows = concat [ arrowsAt i j (gx i) (gy j) | i <- [0 .. nx], j <- [0 .. ny] ]
    arrowsAt i j x y =
      let v = vid cx i j
          p = if v >= 0 then pair U.! v else -1
          e1 = ehAt cx i j
          e2 = evAt cx i j
          toFace e = e >= 0 && pair U.! e >= 0 && cDim (cell cx (pair U.! e)) == 2
          ar = drawArrow cs
      in [ ar (x, y) (x + half, y) | p >= 0, i < nx, e1 == p ]                               -- 頂点 → 辺
         ++ [ ar (x, y) (x - half, y) | p >= 0, i > 0, ehAt cx (i - 1) j == p ]
         ++ [ ar (x, y) (x, y + half) | p >= 0, j < ny, e2 == p ]
         ++ [ ar (x, y) (x, y - half) | p >= 0, j > 0, evAt cx i (j - 1) == p ]
         ++ [ ar (x + half, y) (x + half, y + half) | toFace e1, j < ny, fid cx i j == pair U.! e1 ]          -- 横の辺 → 面
         ++ [ ar (x + half, y) (x + half, y - half) | toFace e1, j > 0, fid cx i (j - 1) == pair U.! e1 ]
         ++ [ ar (x, y + half) (x + half, y + half) | toFace e2, i < nx, fid cx i j == pair U.! e2 ]          -- 縦の辺 → 面
         ++ [ ar (x, y + half) (x - half, y + half) | toFace e2, i > 0, fid cx (i - 1) j == pair U.! e2 ]
