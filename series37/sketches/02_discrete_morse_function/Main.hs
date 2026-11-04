-- ============================================
-- 02 離散モース関数 ─ 胞体に数を割りあてる
-- フォルマンの離散モース関数 f：頂点・辺・面のそれぞれに数を与える。条件は 2 つだけ
--   (a) 自分より「値が小さいか等しい余面（1 つ上の次元で自分を含む胞体）」は高々 1 つ
--   (b) 自分より「値が大きいか等しい面（1 つ下の次元で自分に含まれる胞体）」は高々 1 つ
--   条件を破る胞体は赤。等号になったペアが矢印（勾配）、矢印のない胞体が臨界胞体
-- クリック = 値を +1 / Shift+クリック = −1 / G = 高さ関数からつくる / D = 次元そのもの（全部臨界）/ R = ランダム
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
nx = 6              -- ★ 格子の大きさ
ny = 4

cs, ox, oy :: Float
cs = 118
ox = 50
oy = 120

cx :: Cx
cx = makeGridComplex nx ny Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx ox oy cs

data World = World
  { wF    :: !(U.Vector Int)   -- 胞体の値 f
  , wSeed :: !Int
  , wMsg  :: !String
  }

world0 :: World
world0 = fromHeight (World U.empty 3 "")

main :: IO ()
main = runSketch (sketch "37-02 Discrete Morse Function" world0)
  { skBack  = hsb 225 30 8
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ G：高さ関数 → lower star 法で勾配場 → ペアが同じ値になるように番号をふる（正しい離散モース関数になる）
fromHeight :: World -> World
fromHeight w = w { wF = U.generate n (\c -> IM.findWithDefault 0 (rep c) levels), wSeed = wSeed w + 1
                 , wMsg = "f from a height function (arrow = gradient, pair with equal values)" }
  where
    s = fromIntegral (wSeed w) * 17.3
    h = heightFrom cx (\i j -> noiseP s (fi i * 0.6) (fi j * 0.6) + 0.001 * (fi i + fi j * 7))
    pair = gradientFromHeight cx h
    n = nCells cx
    rep c = let p = pair U.! c in if p >= 0 then min c p else c   -- ペアは 1 つの「かたまり」として扱う
    links = [ (rep v, rep c) | c <- [0 .. n - 1], v <- cFaces (cell cx c), pair U.! v /= c, rep v /= rep c ]
    succs = IM.fromListWith (flip (++)) [(a, [b]) | (a, b) <- links]
    indeg0 = IM.fromListWith (+) [(b, 1 :: Int) | (_, b) <- links]
    start = [c | c <- [0 .. n - 1], rep c == c, IM.findWithDefault 0 c indeg0 == 0]
    levels = kahn start indeg0 IM.empty
    -- トポロジカル順にたどって「いちばん長い道のり」を値にする
    kahn [] _ lv = lv
    kahn (a : queue) deg lv =
      let la = IM.findWithDefault 0 a lv
          (queue', deg', lv') = foldl' visit ([], deg, lv) (IM.findWithDefault [] a succs)
          visit (q, d, l) b =
            let l2 = IM.insertWith max b (la + 1) l
                nd = IM.findWithDefault 0 b d - 1
                d2 = IM.insert b nd d
            in (if nd == 0 then q ++ [b] else q, d2, l2)
      in kahn (queue ++ queue') deg' lv'

-- ★ 条件 (a)(b) を全部の胞体で調べる。等号・逆転のペアを矢印にする
checkMorse :: U.Vector Int -> (IS.IntSet, [(Int, Int)])
checkMorse f = (IS.fromList [c | (c, _, True) <- info], [(c, t) | (c, [t], _) <- info])
  where
    fv c = f U.! c
    info = [ (c, up, length up > 1 || length down > 1)
           | c <- [0 .. nCells cx - 1]
           , let up = filter (\t -> fv t <= fv c) (cCofaces (cell cx c))       -- 自分より小さいか等しい余面
           , let down = filter (\v -> fv v >= fv c) (cFaces (cell cx c)) ]     -- 自分より大きいか等しい面

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  [ box (if bad c then hsb 0 80 60 else hsb 225 25 22) (x - cs / 2 + 2) (y - cs / 2 + 2) (cs - 4) (cs - 4)
  | c <- cellsOf 2, let (x, y) = pos U.! c ]
  ++ concat [ [ seg (if bad c then 5 else 2) (if bad c then hsb 0 85 95 else gray 0.45) (pos U.! a) (pos U.! b) ]
            | c <- cellsOf 1, [a, b] <- [cVerts (cell cx c)] ]
  ++ [ drawArrow (cs * 0.4) (ax, ay) ((ax + bx) / 2 + (bx - ax) * 0.25, (ay + by) / 2 + (by - ay) * 0.25)
     | (a, b) <- arrows, let (ax, ay) = pos U.! a, let (bx, by) = pos U.! b ]
  ++ concatMap mark [0 .. nCells cx - 1]
  ++ [ txt ox 44 20 white
         (if nViol == 0
            then "OK: discrete Morse function.  critical m0 = " ++ show (nCrit 0) ++ "  m1 = " ++ show (nCrit 1) ++ "  m2 = " ++ show (nCrit 2)
            else "NG: " ++ show nViol ++ " cells break the rules (red)")
     , txt ox 72 15 (gray 0.65) "Click: +1  Shift+click: -1  G: from height  D: f = dimension (all critical)  R: random"
     , txt ox 96 14 (hsb 48 60 100) (wMsg w) ]
  ++ [ txt hx (oy + 10 + fromIntegral k * 26) 15 (gray 0.8) r | (k, r) <- zip [0 :: Int ..] rules ]
  where
    (viol, arrows) = checkMorse (wF w)
    bad c = IS.member c viol
    inArrow = IS.fromList (concat [[a, b] | (a, b) <- arrows])
    critical c = not (IS.member c inArrow) && not (bad c)
    cellsOf d = [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == d]
    nCrit d = length (filter critical (cellsOf d))
    nViol = IS.size viol
    hx = ox + fi nx * cs + 50
    -- 臨界胞体の印と値
    mark c =
      let p@(x, y) = pos U.! c
          d = cDim (cell cx c)
          crit = critical c
      in [ dotC (if bad c then hsb 0 70 100 else hsb 272 70 100) p (if crit then 30 else 18) | d == 0 ]
         ++ [ dotC (hsb 85 90 95) p 22 | crit, d == 1 ]
         ++ [ dotC (hsb 12 80 95) p 30 | crit, d == 2 ]
         ++ [ box (hsba 225 30 8 0.8) (x - 13) (y - 10) 26 20 | d /= 0 ]
         ++ [ Color white (at (fl p) (labelC 0 0 (if d == 0 then 14 else 15) white (show (wF w U.! c)))) ]
    rules =
      [ "Rules for each cell s:"
      , "(a) at most one coface t with f(t) <= f(s)"
      , "(b) at most one face v with f(v) >= f(s)"
      , ""
      , "An equal or reversed pair is an arrow s -> t"
      , "A cell without an arrow is critical:"
      , "  purple = min, lime = saddle, coral = max"
      , ""
      , "Edge <= both of its vertices: breaks (b)"
      , "Face <= two or more edges: breaks (b)"
      , "Vertex >= two of its edges: breaks (a)"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down md _ -> pure (click (shift md == Down))
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> pure (fromHeight w)
    | c `elem` "dD" -> pure w { wF = U.generate (nCells cx) (cDim . cell cx), wMsg = "f = dimension: no arrows, everything is critical" }
    | c `elem` "rR" -> do
        vals <- mapM (const (randomRIO (0, 9))) [1 .. nCells cx]
        pure w { wF = U.fromList vals, wMsg = "Random values (usually break the rules)" }
  _ -> pure w
  where
    m = mouseP env
    click shifted =
      let pick (b, bd) c = let d = dist2 m (pos U.! c) in if d < bd then (c, d) else (b, bd)
          (best, _) = foldl' pick (-1, cs * 0.3) [0 .. nCells cx - 1]
      in if best < 0 then w else
         let v = max 0 (min 99 (wF w U.! best + (if shifted then -1 else 1)))
             name = ["vertex", "edge", "face"] 
         in w { wF = wF w U.// [(best, v)]
              , wMsg = concat (take 1 (drop (cDim (cell cx best)) name)) ++ " value is now " ++ show v }

-- ---------- やってみよう ----------
-- 1. D のあと、ある辺の値を両端の片方の頂点と同じにしてみよう。矢印が 1 本できて臨界胞体が 2 つ減る
-- 2. 同じ辺の値を両端の両方以下にすると赤になる。なぜ「高々 1 つ」でないといけない？
-- 3. G で作った f の値を 1 か所だけ変えて、何か所に影響が出るか見よう
-- 4. 全部の胞体がペアになる（臨界胞体が頂点 1 つだけ）f を手で作れる？（円盤なら必ずできる）

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
