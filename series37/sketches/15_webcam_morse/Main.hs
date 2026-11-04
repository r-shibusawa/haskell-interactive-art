-- ============================================
-- 15 ウェブカメラ輝度のモース理論（おまけ）
-- カメラの映像を小さく縮めて、明るさを高さ関数にする。離散勾配場を 3 フレームごとに計算して
--   極小（暗い点）・鞍点・極大（明るい点）と、流域の境界（分水嶺）を映像の上に重ねる
--   ↑↓ のしきい値 ε で持続の短い臨界点を隠す（簡略化）。カメラが使えないときは動くノイズ画像で代用
-- ↑↓ = しきい値 ε / B = 流域の境界 / I = 明暗を反転 / M = 記号の大きさ
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
import Data.Word (Word8)
import Numeric (showFFloat)

vw, vh :: Int
vw = 64             -- ★ 映像の解像度
vh = 48

ox, oy, cs :: Float
ox = 30
oy = 100
cs = min ((scrW - 60) / fi (vw - 1)) ((scrH - 150) / fi (vh - 1))

cx :: Cx
cx = makeGridComplex (vw - 1) (vh - 1) Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx ox oy cs

data World = World
  { wCam   :: !(Maybe Camera)
  , wLive  :: !Bool
  , wH     :: !(U.Vector Float)
  , wPair  :: !(U.Vector Int)
  , wImg   :: Picture
  , wEps   :: !Float
  , wShowB :: !Bool
  , wInv   :: !Bool
  , wBig   :: !Bool
  , wMins, wSads, wMaxs :: [Int]
  , wBound :: [Int]
  }

empty :: World
empty = World Nothing False (U.replicate (vw * vh) 0.5) (U.replicate (nCells cx) (-1)) Blank 0.06 True False False [] [] [] []

main :: IO ()
main = runSketch (sketch "37-15 Webcam Morse" empty)
  { skBack  = hsb 225 30 8
  , skStart = \_ -> do
      cam <- forceCx cx `seq` pos `seq` startCamera vw vh
      pure empty { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- p5.js の noise(x, y, z)
noise3P :: Float -> Float -> Float -> Float
noise3P x y z = sum [ 0.5 ^ (k + 1) * clampF 0 1 (0.5 + 2.2 * (noise3 (x * f) (y * f) (z * f) - 0.5)) | k <- [0 .. 3 :: Int], let f = 2 ^ k ]

-- ★ 高さ関数を更新：カメラの明るさ（なければノイズ）を少しなめらかにして、前のフレームと混ぜる
updateHeights :: (Int -> Int -> Float) -> World -> U.Vector Float
updateHeights camV w = wH w U.// news
  where
    raw k = let (j, i) = k `divMod` vw
                v = camV i j
            in if wInv w then 1 - v else v
    src = U.generate (vw * vh) raw
    news = [ (v, wH w U.! v * 0.5 + target * 0.5)
           | j <- [0 .. vh - 1], i <- [0 .. vw - 1]
           , let vals = [ src U.! (x + y * vw) | dj <- [-1, 0, 1], di <- [-1, 0, 1], let x = i + di, let y = j + dj, x >= 0, y >= 0, x < vw, y < vh ]
           , let target = sum vals / fi (length vals) + 0.00001 * (fi i * 3 + fi j)
           , let v = vid cx i j ]

-- ★ 勾配場 → 臨界点の持続（パーシステンスの近似）→ ε 以上のものだけ表示。流域の境界も求める
analyze :: World -> World
analyze w = w { wPair = pair, wImg = image, wSads = sads, wMins = mins, wMaxs = maxs, wBound = boundary }
  where
    h = wH w
    pair = gradientFromHeight cx h
    bs = basins cx pair
    eps = wEps w
    inf = 1 / 0 :: Float
    ch c = foldl' (\m v -> max m (h U.! v)) (-inf) (cVerts (cell cx c))
    critOf d = [c | c <- [0 .. nCells cx - 1], pair U.! c < 0, cDim (cell cx c) == d]
    saddleInfo =
      [ (c, young, pd, oldMax, pu)
      | c <- critOf 1
      , let down = map pathEnd (descendPaths cx pair 4000 c)
      , let up = map pathEnd (ascendPaths cx pair 4000 c)
      , let (young, pd) = case down of
              [a, b] | a /= b -> let y = if h U.! a > h U.! b then a else b in (y, ch c - h U.! y)
              _ -> (-1, inf)
      , let distinct = IS.size (IS.fromList up) >= 2
      , let oldMax = if distinct then foldl' (\a b -> if ch a < ch b then a else b) (headOr up) up else -1
      , let pu = if distinct then ch oldMax - ch c else inf ]
    headOr l = case l of
      (x : _) -> x
      [] -> -1
    minPers = IM.fromListWith min [(y, pd) | (_, y, pd, _, _) <- saddleInfo, y >= 0]
    maxPers = IM.fromListWith min [(y, pu) | (_, _, _, y, pu) <- saddleInfo, y >= 0]
    sads = [c | (c, _, pd, _, pu) <- saddleInfo, min pd pu >= eps]
    mins = [c | c <- critOf 0, IM.findWithDefault inf c minPers >= eps]
    maxs = [c | c <- critOf 2, IM.findWithDefault inf c maxPers >= eps]
    boundary = [ a | i <- [0 .. vw - 2], j <- [0 .. vh - 2], let a = vid cx i j
                   , bs U.! a /= bs U.! vid cx (i + 1) j || bs U.! a /= bs U.! vid cx i (j + 1) ]
    -- 映像（高さ）を描く
    image = stretch ox oy (fi (vw - 1) * cs) (fi (vh - 1) * cs) (vw - 1) (vh - 1) $ \i j ->
      bytes (hsb 220 15 (10 + h U.! vid cx i j * 85))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | envFrame env `mod` 3 /= 0 = pure w
  | otherwise = do
      let fr = fi (envFrame env)
          noiseV i j = noise3P (fi i * 0.08 + fr * 0.01) (fi j * 0.08) (fr * 0.005)
      (live, camV) <- case wCam w of
        Just cam -> do
          l <- camLive cam
          if l then do
            f <- cameraFrame cam
            pure (True, camGray cam f)
          else pure (False, noiseV)
        Nothing -> pure (False, noiseV)
      pure (analyze w { wLive = live, wH = updateHeights camV w })

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  [ wImg w ]
  ++ (if wShowB w then [ box (hsba 85 60 100 0.35) (x - cs / 2) (y - cs / 2) cs cs | a <- wBound w, let (x, y) = pos U.! a ] else [])
  ++ [ dotC (hsb 272 80 100) (pos U.! c) (9 * r) | c <- wMins w ]
  ++ [ dotC (hsb 12 85 100) (pos U.! c) (9 * r) | c <- wMaxs w ]
  ++ concat [ [ seg (2 * r) col (x - 4 * r, y - 4 * r) (x + 4 * r, y + 4 * r), seg (2 * r) col (x - 4 * r, y + 4 * r) (x + 4 * r, y - 4 * r) ]
            | c <- wSads w, let (x, y) = pos U.! c ]
  ++ [ txt ox 40 20 white (source ++ "  m = " ++ showM (countCritical cx (wPair w)) ++ "  eps " ++ fmt 2 (wEps w) ++ ": min " ++ show (length (wMins w))
         ++ "  saddle " ++ show (length (wSads w)) ++ "  max " ++ show (length (wMaxs w)))
     , txt ox 66 15 (gray 0.65) ("Up/Dn: eps  B: basins " ++ (if wShowB w then "ON" else "off") ++ "  I: invert  M: big marks   purple = min (dark)  x = saddle  coral = max (bright)  lime = watershed")
     ]
  where
    r = if wBig w then 2 else 1
    col = hsb 85 90 100
    source | wLive w = "Camera"
           | envTime env < 3 = "Waiting for camera..."
           | otherwise = "No camera: noise"

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wEps = min 0.6 (wEps w + 0.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wEps = max 0 (wEps w - 0.02) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> w { wShowB = not (wShowB w) }
    | c `elem` "iI" -> w { wInv = not (wInv w) }
    | c `elem` "mM" -> w { wBig = not (wBig w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 顔を映して ε を上げる。最後まで残る極大は？（たいてい額や鼻先）残る極小は？（目・鼻の穴）
-- 2. 手を広げると指ごとに極大が出て、指の間に鞍点が出る。何本の鞍点が出る？ 極大 − 鞍点 は？
-- 3. 懐中電灯やスマホのライトを動かして、極大が流域（分水嶺）をどう動かすか見よう
-- 4. 反転（I）すると極大と極小が入れかわる。分水嶺（流域の境界）はどう変わる？

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

-- 流域：各頂点を矢印（頂点 → 辺 → 反対側の頂点）にそって流して、着いた極小の番号を入れる
basins :: Cx -> U.Vector Int -> U.Vector Int
basins cx pair = U.create $ do
  bs <- MU.replicate (nCells cx) (-1)
  let other e c = case cVerts (cell cx e) of
        [a, b] | a == c -> b
               | otherwise -> a
        _ -> c
      flow c trail = do
        b <- MU.read bs c
        if b >= 0 then pure (b, trail)
        else if pair U.! c >= 0 then flow (other (pair U.! c) c) (c : trail)
        else pure (c, c : trail)
  forM_ [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0] $ \v -> do
    (b, trail) <- flow v []
    forM_ trail (\t -> MU.write bs t b)
  pure bs

-- ピクセル画像を左上 (x, y)、幅 ww、高さ hh に引きのばして置く
stretch :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
stretch x y ww hh w h f =
  Translate (x + ww / 2) (-(y + hh / 2)) (Scale (ww / fromIntegral w) (hh / fromIntegral h) (pixelImage w h 1 f))

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
