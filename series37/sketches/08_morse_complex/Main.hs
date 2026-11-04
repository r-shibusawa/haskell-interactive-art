-- ============================================
-- 08 モース複体 ─ 臨界胞体だけでできた小さな複体
-- 臨界胞体を頂点にして、鞍点 → 極小、極大 → 鞍点 の V-パスの本数（mod 2）が 1 なら辺で結ぶ
--   これが「モース複体」。元の複体よりずっと小さいのに、ホモロジー（ベッチ数）は同じ ─ 離散モース理論の主定理
--   境界行列を Z2 で掃き出してモース複体のベッチ数を計算し、元の b_k とくらべる
-- N = 新しい高さ関数 / 1〜3 = 円盤・円環・穴 2 つ / P = V-パスの表示 / K = キャンセルして小さくする
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
nx = 14             -- ★ 格子の大きさ
ny = 9

cs, ox, oy :: Float
cs = 56
ox = 50
oy = 110

-- モース複体の辺：a → b の V-パスが n 本
data MEdge = MEdge { eA, eB, eN :: !Int, ePaths :: [[Int]] }

-- モース複体 { 節点, 辺, 各次元の数, ベッチ数 }
data Morse = Morse { mNodes :: [Int], mEdges :: [MEdge], mM :: !(Int, Int, Int), mBetti :: !(Int, Int, Int) }

data World = World
  { wCx    :: !Cx
  , wPos   :: !(U.Vector Point)
  , wBetti :: !Betti
  , wH     :: !(U.Vector Float)
  , wPair  :: !(U.Vector Int)
  , wMc    :: Morse
  , wPaths :: !Bool
  }

main :: IO ()
main = runSketch (sketch "37-08 Morse Complex" (buildComplex 2 True))
  { skBack  = hsb 225 30 8
  , skStart = \_ -> newHeight (buildComplex 2 True)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

buildComplex :: Int -> Bool -> World
buildComplex shapeNo sp =
  World cx (gridPositions cx ox oy cs) (bettiNumbers cx) U.empty pair0 (buildMorse cx pair0) sp
  where
    holes = case shapeNo of
      1 -> []
      2 -> [(5, 3, 4, 3)]
      _ -> [(3, 3, 3, 3), (8, 3, 3, 3)]
    inHole i j (x, y, w, h) = i >= x && i < x + w && j >= y && j < y + h
    cx = makeGridComplex nx ny Plain (\i j -> not (any (inHole i j) holes))
    pair0 = U.generate (nCells cx) id

newHeight :: World -> IO World
newHeight w = do
  s <- newSeed
  let cx = wCx w
      h = heightFrom cx (\i j -> noiseP s (fi i * 0.35) (fi j * 0.35) + 0.0001 * (fi i * 3 + fi j))
      pair = gradientFromHeight cx h
  pure w { wH = h, wPair = pair, wMc = buildMorse cx pair }

-- ★ モース複体：臨界胞体の間の V-パスを数える（mod 2 が境界係数）
buildMorse :: Cx -> U.Vector Int -> Morse
buildMorse cx pair = Morse nodes edges (len 0, len 1, len 2) (len 0 - r1, len 1 - r1 - r2, len 2 - r2)
  where
    nodes = [c | c <- [0 .. nCells cx - 1], pair U.! c < 0]
    edges = [ MEdge c e (length ps) ps
            | c <- nodes, cDim (cell cx c) > 0
            , (e, ps) <- IM.toAscList (IM.fromListWith (flip (++)) [(pathEnd p, [p]) | p <- descendPaths cx pair 4000 c]) ]
    byDim d = [c | c <- nodes, cDim (cell cx c) == d]
    len = length . byDim
    -- Z2 の境界行列 ∂1（鞍点 → 極小）、∂2（極大 → 鞍点）の階数。行 = 係数 1 の列の集合
    rows d = [ IS.fromList [eB e | e <- edges, eA e == c, odd (eN e)] | c <- byDim d ]
    r1 = rankZ2 (rows 1)
    r2 = rankZ2 (rows 2)

-- 掃き出し法：先頭の行の最小の列で、残りの行を消していく（足し算は対称差）
rankZ2 :: [IS.IntSet] -> Int
rankZ2 [] = 0
rankZ2 (r : rs) = case IS.lookupGE minBound r of
  Nothing -> rankZ2 rs
  Just c -> 1 + rankZ2 [ if IS.member c x then IS.union x r `IS.difference` IS.intersection x r else x | x <- rs ]

-- K：V-パスが 1 本のペアを 1 つキャンセル（07 と同じ）
cancelOne :: World -> World
cancelOne w = case [p | MEdge _ _ 1 [p] <- mEdges (wMc w)] of
  (p : _) -> let pair = cancelPath (wPair w) p in w { wPair = pair, wMc = buildMorse (wCx w) pair }
  [] -> w

-- 点線
dashed :: Float -> Color -> Point -> Point -> [Picture]
dashed wd c (x1, y1) (x2, y2) =
  [ seg wd c (at' (t * 14)) (at' (min l (t * 14 + 6))) | t <- map fromIntegral [0 .. floor (l / 14) :: Int] ]
  where
    l = dist2 (x1, y1) (x2, y2)
    at' d = if l < 1e-6 then (x1, y1) else (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  -- 元の複体（うすく）
  [ box (hsb (200 - v * 150) 30 (15 + v * 25)) (ox + fi i * cs) (oy + fi j * cs) cs cs
  | i <- [0 .. nx - 1], j <- [0 .. ny - 1], let f = fid cx i j, f >= 0
  , let vs = cVerts (cell cx f), let v = sum (map (wH w U.!) vs) / fi (length vs) ]
  ++ [ seg 1 (gray 0.28) (pos U.! a) (pos U.! b) | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 1, [a, b] <- [cVerts (cell cx c)] ]
  -- モース複体の辺（V-パスまたは直線）
  ++ concatMap drawEdge (mEdges mc)
  -- 節点 = 臨界胞体
  ++ [ dotC (hsb (dimHue d) 80 100) (pos U.! c) (if d == 1 then 16 else 22) | c <- mNodes mc, let d = cDim (cell cx c) ]
  ++ [ txt ox 44 20 white ("Complex: V = " ++ show (nV b) ++ " E = " ++ show (nE b) ++ " F = " ++ show (nF b) ++ " b = " ++ showM orig
         ++ "  ->  Morse: m = " ++ showM (mM mc) ++ " b = " ++ showM (mBetti mc))
     , txt ox 72 15 (gray 0.65) ("N: new height  1-3: shape  P: V-paths " ++ (if wPaths w then "ON" else "off") ++ "  K: cancel   solid = odd # of paths (coef 1)  dashed = even (coef 0)")
     , txt ox (oy + fi ny * cs + 36) 17 (if same then hsb 85 80 100 else hsb 0 80 100)
         ((if same then "OK " else "NG ") ++ "A complex of " ++ show (nV b + nE b + nF b) ++ " cells and a Morse complex of " ++ show (tot (mM mc)) ++ " cells have the same homology")
     , txt ox (oy + fi ny * cs + 60) 14 (gray 0.55) "d(saddle) = sum of mins (V-paths mod 2), d(max) = sum of saddles.  b_k = m_k - rank d_k - rank d_(k+1)"
     ]
  where
    cx = wCx w
    pos = wPos w
    b = wBetti w
    mc = wMc w
    orig = (b0 b, b1 b, b2 b)
    same = mBetti mc == orig
    tot (x, y, z) = x + y + z
    dimHue :: Int -> Float
    dimHue d = case d of { 0 -> 272; 1 -> 85; _ -> 12 }
    drawEdge e =
      let odd' = odd (eN e)
          hue = if cDim (cell cx (eA e)) == 1 then 190 else 30
      in (if wPaths w
            then [ polyL (if odd' then 3 else 1.5) (hsba hue 60 90 (if odd' then 0.6 else 0.25)) (map (pos U.!) p) | p <- ePaths e ]
            else [])
         ++ (if odd' then [seg 4 (hsb hue 80 100) (pos U.! eA e) (pos U.! eB e)]
                     else dashed 2 (gray 0.6) (pos U.! eA e) (pos U.! eB e))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> newHeight w
    | c `elem` "pP" -> pure w { wPaths = not (wPaths w) }
    | c `elem` "kK" -> pure (cancelOne w)
    | c `elem` "123" -> newHeight (buildComplex (fromEnum c - fromEnum '0') (wPaths w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 点線（V-パスが 2 本）の辺を見つけよう。2 本のパスは極小のどちら側を通っている？
-- 2. K を押し続けると節点が減っていく。最後にはモース複体に何が残る？ b はずっと同じ？
-- 3. 円環（2）で鞍点が 1 つだけになったとき、その鞍点の ∂ はなぜ 0 になる？（2 本のパスが同じ極小に）
-- 4. モース複体の辺を「鞍点 → 極小」だけにすると、残った成分の数が b0。その理由を考えよう

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

-- Union-Find：n 個の要素を組 (a, b) で結び、各要素の根を返す
components :: Int -> [(Int, Int)] -> U.Vector Int
components n links = U.create $ do
  p <- U.thaw (U.enumFromN 0 n)
  let find a = do
        q <- MU.read p a
        if q == a then pure a else do
          r <- MU.read p q
          MU.write p a r
          find r
      unite (a, b) = do
        x <- find a
        y <- find b
        if x /= y then MU.write p x y else pure ()
  mapM_ unite links
  mapM_ (\i -> find i >>= MU.write p i) [0 .. n - 1]
  pure p

-- ベッチ数：b0 = つながった塊の数（Union-Find）、b2 = 閉じた曲面の数、b1 = b0 + b2 − χ（Z2 係数）
data Betti = Betti { b0, b1, b2, chi, nV, nE, nF :: !Int }

bettiNumbers :: Cx -> Betti
bettiNumbers cx = Betti bb0 (bb0 + bb2 - x) bb2 x v e f
  where
    cs = [0 .. nCells cx - 1]
    ofDim d = [c | c <- cs, cDim (cell cx c) == d]
    v = length (ofDim 0)
    e = length (ofDim 1)
    f = length (ofDim 2)
    x = v - e + f
    links = [ (a, b) | c <- ofDim 1, [a, b] <- [cVerts (cell cx c)] ]
         ++ [ (a, b) | c <- ofDim 2, ed <- cFaces (cell cx c), [a, b] <- [cCofaces (cell cx ed)] ]   -- 辺を共有する面をつなぐ
    root = components (nCells cx) links
    bb0 = length [c | c <- ofDim 0, root U.! c == c]
    closed = IM.fromListWith (&&) [ (root U.! c, all (\ed -> cNcof (cell cx ed) == 2) (cFaces (cell cx c))) | c <- ofDim 2 ]
    bb2 = length (filter id (IM.elems closed))
