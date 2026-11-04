-- ============================================
-- 05 単体的崩壊（collapse）─ 自由面をつぶしていく
-- 自由面（free face）= ちょうど 1 つの余面にしか含まれていない胞体。その胞体と余面を一緒に消しても「形」は変わらない
--   面 → 辺、辺 → 頂点と順につぶして、つぶせなくなった残りが「芯（core）」。円盤は点に、円環は円周に
--   崩壊の 1 歩 = 勾配場の矢印 1 本。崩壊を記録すると勾配場ができる（残った芯 = 臨界胞体）
-- スペース = 1 歩 / Enter = 自動 / 1〜3 = 円盤・円環・穴 2 つ / R = ランダムな形 / N = やり直し
-- ============================================
module Main (main) where

import Art
import Art
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL, nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showFFloat)

nx, ny :: Int
nx = 14             -- ★ 格子の大きさ
ny = 9

cs, ox, oy :: Float
cs = 62
ox = 60
oy = 120

data World = World
  { wCx    :: !Cx
  , wPos   :: !(U.Vector Point)
  , wAlive :: !(U.Vector Bool)
  , wPair  :: !(U.Vector Int)    -- 崩壊の記録 = 勾配の矢印
  , wSteps :: !Int
  , wAuto  :: !Bool
  , wCand  :: !(Maybe (Int, Int))
  , wMsg   :: !String
  }

main :: IO ()
main = runSketch (sketch "37-05 Collapse" (buildComplex (holeMask 1)))
  { skBack  = hsb 225 30 8
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

holeMask :: Int -> Int -> Int -> Bool
holeMask shapeNo i j = not (any inHole holes)
  where
    holes = case shapeNo of
      1 -> []
      2 -> [(5, 3, 4, 3)]
      _ -> [(3, 3, 3, 3), (8, 3, 3, 3)]
    inHole (x, y, w, h) = i >= x && i < x + w && j >= y && j < y + h

buildComplex :: (Int -> Int -> Bool) -> World
buildComplex mask = reset (World cx (gridPositions cx ox oy cs) U.empty U.empty 0 False Nothing "")
  where cx = makeGridComplex nx ny Plain mask

reset :: World -> World
reset w = w { wAlive = U.replicate n True, wPair = U.replicate n (-1), wSteps = 0, wCand = Nothing, wAuto = False, wMsg = "" }
  where n = nCells (wCx w)

-- ★ 自由面を探す：生きている余面がちょうど 1 つの胞体。まず辺（面をつぶす）、なければ頂点（辺をつぶす）
findFree :: World -> IO (Maybe (Int, Int))
findFree w = case filter (not . null) [freeOf 1, freeOf 0] of
  (l : _) -> do
    k <- randomRIO (0, length l - 1)
    pure (Just (l !! k))
  [] -> pure Nothing
  where
    cx = wCx w
    alive c = wAlive w U.! c
    freeOf d = [ (c, t) | c <- [0 .. nCells cx - 1], alive c, cDim (cell cx c) == d
                        , [t] <- [filter alive (cCofaces (cell cx c))] ]

collapseStep :: World -> IO World
collapseStep w = do
  fr <- maybe (findFree w) (pure . Just) (wCand w)
  pure $ case fr of
    Nothing -> w { wAuto = False, wMsg = "No free faces left: this is the core", wCand = Nothing }
    Just (a, b) -> w { wAlive = wAlive w U.// [(a, False), (b, False)], wPair = wPair w U.// [(a, b), (b, a)]
                     , wSteps = wSteps w + 1, wCand = Nothing }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  w1 <- if wAuto w && envFrame env `mod` 3 == 0 then collapseStep w else pure w
  case wCand w1 of
    Nothing | not (wAuto w1) -> do
      c <- findFree w1
      pure w1 { wCand = c }
    _ -> pure w1

-- 生き残った複体の V, E, F とベッチ数（Union-Find、平面なので b2 = 0）
bettiAlive :: World -> (Int, Int, Int, Int, Int)
bettiAlive w = (v, e, f, b0, b0 - (v - e + f))
  where
    cx = wCx w
    live d = [c | c <- [0 .. nCells cx - 1], wAlive w U.! c, cDim (cell cx c) == d]
    v = length (live 0)
    e = length (live 1)
    f = length (live 2)
    root = components (nCells cx) [(a, b) | c <- live 1, [a, b] <- [cVerts (cell cx c)]]
    b0 = length [c | c <- live 0, root U.! c == c]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  -- 消えた胞体はうすく、生きている胞体ははっきり
  [ box (if alive f then hsb 12 60 75 else hsb 225 15 14) (ox + fi i * cs + 1) (oy + fi j * cs + 1) (cs - 2) (cs - 2)
  | i <- [0 .. nx - 1], j <- [0 .. ny - 1], let f = fid cx i j, f >= 0 ]
  ++ [ if alive c then seg 4 (hsb 85 70 95) (pos U.! a) (pos U.! b) else seg 1 (gray 0.22) (pos U.! a) (pos U.! b)
     | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 1, [a, b] <- [cVerts (cell cx c)] ]
  ++ [ if alive c then dotC (hsb 272 70 100) (pos U.! c) 14 else dotC (gray 0.28) (pos U.! c) 5
     | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0 ]
  ++ candidate
  ++ [ txt ox 44 20 white ("Steps " ++ show (wSteps w) ++ "   left V = " ++ show v ++ "  E = " ++ show e ++ "  F = " ++ show f
         ++ "   chi = " ++ show (v - e + f) ++ "  b0 = " ++ show b0 ++ "  b1 = " ++ show b1 ++ " (unchanged)")
     , txt ox 72 15 (gray 0.65) ("Space: 1 step  Enter: auto " ++ (if wAuto w then "ON" else "off") ++ "  1-3: disc, annulus, 2 holes  R: random  N: restart   " ++ wMsg w)
     , txt ox (oy + fi ny * cs + 36) 14 (gray 0.55) "Yellow = next free face (exactly 1 coface) and that coface, removed together. Core: a point = disc, a circle = annulus."
     , txt ox (oy + fi ny * cs + 56) 14 (gray 0.55) "The core is the critical cells: one min plus one saddle per hole."
     ]
  where
    cx = wCx w
    pos = wPos w
    alive c = wAlive w U.! c
    (v, e, f, b0, b1) = bettiAlive w
    yellow = hsb 48 90 100
    -- 次につぶす自由面（黄色）
    candidate = case wCand w of
      Nothing -> []
      Just (a, b) ->
        [ ringC yellow (pos U.! a) (cs * 0.45) 3 ]
        ++ (if cDim (cell cx b) == 2
              then let (x, y) = pos U.! b
                       l = x - cs / 2 + 4
                       r = x + cs / 2 - 4
                       t = y - cs / 2 + 4
                       u = y + cs / 2 - 4
                   in [polyL 3 yellow [(l, t), (r, t), (r, u), (l, u), (l, t)]]
              else [ seg 3 yellow (pos U.! p) (pos U.! q) | [p, q] <- [cVerts (cell cx b)] ])
        ++ [ drawArrow cs (pos U.! a) (pos U.! b) ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> collapseStep w
  EventKey (SpecialKey KeyEnter) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> pure (reset w)
    | c `elem` "rR" -> do
        rnd <- mapM (const (randomRIO (0, 1 :: Float))) [1 .. nx * ny]
        let t = U.fromList (map (< 0.78) rnd)
        pure (buildComplex (\i j -> t U.! (i + j * nx)))
    | c `elem` "123" -> pure (buildComplex (holeMask (fromEnum c - fromEnum '0')))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 円盤（1）を最後までつぶすと点が 1 つ残る。頂点 1 つ = 極小 1 つ、m = (1, 0, 0)
-- 2. 円環（2）の芯は円周。頂点と辺が同じ数だけ残る。辺のうち「臨界」と呼べるのは何本？（ヒント：頂点 → 辺の矢印をさらに置くと）
-- 3. R のランダムな形で、芯がどんな形になるか予想してから自動で走らせよう
-- 4. findFree で「頂点から先に」つぶすと途中で止まることがある。順番を入れかえて試そう

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
