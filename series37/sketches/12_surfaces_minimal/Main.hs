-- ============================================
-- 12 曲面と最小の臨界胞体 ─ トーラス・球面・クラインの壺
-- 正方格子の向かい合う辺を貼り合わせると閉じた曲面になる（辺の色と矢印が「貼り方」）
--   トーラス：上下・左右を同じ向きに　球面：上と左、右と下を折るように　クラインの壺：左右を逆向きに
--   高さ関数から勾配場を作ると、臨界胞体の数が最小（m_k = b_k、Z2 係数）になる：トーラス (1,2,1)・球面 (1,0,1)・クラインの壺 (1,2,1)
-- 1〜3 = 曲面 / T = トーラスを 3D に貼って見る（ドラッグで回転）/ A = 矢印 / N = 高さ関数を変える
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

n :: Int
n = 16              -- ★ 格子の大きさ（n × n マス）

cs, side, x0, y0 :: Float
cs = 30
side = fi n * cs
x0 = (scrW - side) / 2             -- 平らな格子を置く位置
y0 = (scrH - side) / 2 + 50

-- 格子の上の絵（p5.js の pg にあたる）。同じ絵を平面にも 3D のトーラスにも貼れるように、図形を一度リストにする
data Prim
  = PFace !Float !Color !Float !Float !Float      -- 持ち上げる量、色、左上 x, y、一辺
  | PSeg !Float !Color !Point !Point
  | PDisc !Color !Point !Float
  | PTri !Color !Point !Point !Point

data World = World
  { wSurf   :: !Int
  , wCx     :: !Cx
  , wBetti  :: !Betti
  , wPair   :: !(U.Vector Int)
  , wH      :: !(U.Vector Float)
  , wPrims  :: [Prim]
  , wPic3   :: Picture          -- 3D のトーラス（回したときだけ描きなおす）
  , w3D     :: !Bool
  , wArrows :: !Bool
  , wPhase  :: !Float
  , wRx     :: !Float
  , wRy     :: !Float
  , wMouse  :: !Point
  }

world0 :: World
world0 = build (World 1 cx0 (bettiNumbers cx0) U.empty U.empty [] Blank False True 0 (-0.7) 0.6 (0, 0))
  where cx0 = makeGridComplex n n TorusGlue (\_ _ -> True)

main :: IO ()
main = runSketch (sketch "37-12 Surfaces Minimal" world0)
  { skBack  = hsb 225 30 8
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 曲面と高さ関数。貼り合わせた頂点は同じ高さでないといけないので、cos で作る（球面は i + j）
build :: World -> World
build w = refresh w { wCx = cx, wBetti = bettiNumbers cx, wH = h, wPair = gradientFromHeight cx h }
  where
    ident = case wSurf w of { 1 -> TorusGlue; 2 -> SphereGlue; _ -> KleinGlue }
    cx = makeGridComplex n n ident (\_ _ -> True)
    ph = wPhase w
    value i j
      | wSurf w == 2 = fi (i + j)
      -- クラインの壺は (n, j) と (0, n − j) を貼るので、j の cos は位相をずらせない（ずらすと貼り目で高さが飛んで臨界胞体が増える）
      | otherwise = cos (2 * pi * fi i / fi n + ph) + 1.3 * cos (2 * pi * fi j / fi n + (if wSurf w == 3 then 0 else ph * 0.7))
    -- 同じ頂点には最初の位置の値を使う
    table = IM.fromListWith (\_ old -> old) [ (vid cx i j, value i j) | i <- [0 .. n], j <- [0 .. n] ]
    h = U.fromList [ val + 0.0001 * fi v | (v, val) <- IM.toAscList table ]

refresh :: World -> World
refresh w = w1 { wPic3 = torusPicture w1 }
  where w1 = w { wPrims = gridPrims w }

-- 格子を描く（貼り合わせる辺に色と矢印をつける）
gridPrims :: World -> [Prim]
gridPrims w = faces ++ critFaces ++ edges ++ verts ++ (if wArrows w then arrows else []) ++ marks
  where
    cx = wCx w
    pair = wPair w
    h = wH w
    lo = U.minimum h
    hi = U.maximum h
    crit c = pair U.! c < 0
    gp i = fi i * cs
    half = cs / 2
    faces = [ PFace 0 (hsb (200 - v * 150) 40 (25 + v * 45)) (gp i) (gp j) cs
            | i <- [0 .. n - 1], j <- [0 .. n - 1]
            , let vs = cVerts (cell cx (fid cx i j))
            , let v = (sum (map (h U.!) vs) / fi (length vs) - lo) / (hi - lo + 1e-9) ]
    critFaces = [ PFace 10 (hsba 12 80 95 0.75) (gp i + 1) (gp j + 1) (cs - 2)
                | i <- [0 .. n - 1], j <- [0 .. n - 1], crit (fid cx i j) ]
    edgeP e a b | crit e = PSeg 5 (hsb 85 90 95) a b
                | otherwise = PSeg 1.5 (gray 0.42) a b
    edges = concat [ [edgeP e (gp i, gp j) (gp (i + 1), gp j) | let e = ehAt cx i j, e >= 0]
                     ++ [edgeP e (gp i, gp j) (gp i, gp (j + 1)) | let e = evAt cx i j, e >= 0]
                   | i <- [0 .. n], j <- [0 .. n] ]
    verts = [ if crit v then PDisc (hsb 272 70 100) (gp i, gp j) (cs * 0.34) else PDisc (gray 0.72) (gp i, gp j) (max 3 (cs * 0.14))
            | i <- [0 .. n], j <- [0 .. n], let v = vid cx i j, v >= 0 ]
    arrowP a@(ax, ay) b@(bx, by) =
      let t = atan2 (by - ay) (bx - ax)
          hd = min 8 (cs * 0.22)
          c = hsb 48 70 100
          sd d = (bx - hd * cos (t + d), by - hd * sin (t + d))
      in [PSeg (max 1.5 (cs * 0.06)) c a b, PTri c b (sd (-0.5)) (sd 0.5)]
    arrows = concat [ arrowsAt i j (gp i) (gp j) | i <- [0 .. n], j <- [0 .. n] ]
    arrowsAt i j x y =
      let v = vid cx i j
          p = if v >= 0 then pair U.! v else -1
          e1 = ehAt cx i j
          e2 = evAt cx i j
          toFace e = e >= 0 && pair U.! e >= 0 && cDim (cell cx (pair U.! e)) == 2
      in concat $
         [ arrowP (x, y) (x + half, y) | p >= 0, i < n, e1 == p ]
         ++ [ arrowP (x, y) (x - half, y) | p >= 0, i > 0, ehAt cx (i - 1) j == p ]
         ++ [ arrowP (x, y) (x, y + half) | p >= 0, j < n, e2 == p ]
         ++ [ arrowP (x, y) (x, y - half) | p >= 0, j > 0, evAt cx i (j - 1) == p ]
         ++ [ arrowP (x + half, y) (x + half, y + half) | toFace e1, j < n, fid cx i j == pair U.! e1 ]
         ++ [ arrowP (x + half, y) (x + half, y - half) | toFace e1, j > 0, fid cx i (j - 1) == pair U.! e1 ]
         ++ [ arrowP (x, y + half) (x + half, y + half) | toFace e2, i < n, fid cx i j == pair U.! e2 ]
         ++ [ arrowP (x, y + half) (x - half, y + half) | toFace e2, i > 0, fid cx (i - 1) j == pair U.! e2 ]
    a = 5
    b = side - 5                       -- 少し内側に描く（三角が切れないように）
    lime = 85
    purple = 272
    marks = concat $ case wSurf w of
      1 -> [mark a a b a lime, mark a b b b lime, mark a a a b purple, mark b a b b purple]
      2 -> [mark a a b a lime, mark a a a b lime, mark b a b b purple, mark a b b b purple]
      _ -> [mark a a b a lime, mark a b b b lime, mark a a a b purple, mark b b b a purple]

-- 貼り合わせる辺の印：線と、向きを示す三角 2 つ
mark :: Float -> Float -> Float -> Float -> Float -> [Prim]
mark x1 y1 x2 y2 hue = PSeg 6 c (x1, y1) (x2, y2) : map tri [0.3, 0.7]
  where
    c = hsb hue 90 100
    t = atan2 (y2 - y1) (x2 - x1)
    tri k = let x = x1 + (x2 - x1) * k
                y = y1 + (y2 - y1) * k
                pt d = (x + 9 * cos (t + d), y + 9 * sin (t + d))
            in PTri c (pt 0) (pt 2.4) (pt (-2.4))

-- 平らな格子を描く
flatPicture :: [Prim] -> Picture
flatPicture = Pictures . map draw
  where
    o (x, y) = (x + x0, y + y0)
    draw p = case p of
      PFace _ c x y s -> box c (x + x0) (y + y0) s s
      PSeg w c a b -> seg w c (o a) (o b)
      PDisc c q d -> dotC c (o q) d
      PTri c a b d -> Color c (Polygon (map (fl . o) [a, b, d]))

-- ★ トーラスの大きさ。格子の (x, y) をトーラスの上の点にうつす（p5.js の texture の代わり）
torusR, torusS, camD :: Float
torusR = 175
torusS = 78
camD = 693

-- 回したあとの点と法線
onTorus :: World -> Point -> (V3, V3)
onTorus w (x, y) = (add3 (view p) (V3 0 60 0), view nrm)
  where
    th = x / side * tau
    ph = y / side * tau
    k = torusR + torusS * cos ph
    p = V3 (k * cos th) (k * sin th) (torusS * sin ph)
    nrm = V3 (cos ph * cos th) (cos ph * sin th) (sin ph)
    view = rotX (wRx w) . rotY (wRy w)

toScreen :: V3 -> Point
toScreen v = let (x, y) = project camD v in (scrW / 2 + x, scrH / 2 + y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- 画家のアルゴリズム：裏を向いた図形を捨て、奥から順に描く。線や点は面より少し手前にずらす
torusPicture :: World -> Picture
torusPicture w = Pictures (map snd (depthSort fst (concatMap item (wPrims w))))
  where
    place = onTorus w
    front (p, q) = dot3 q (sub3 (V3 0 0 camD) p) > 0
    lift = 14
    item prim = case prim of
      PFace up c x y s ->
        [ (avg + up, Color c (Polygon (map (fl . toScreen . fst) pts)))
        | a <- [0 .. 2 :: Int], b <- [0 .. 1 :: Int]
        , let sx = s / 3
        , let sy = s / 2
        , let qx = x + fi a * sx
        , let qy = y + fi b * sy
        , let pts = map place [(qx, qy), (qx + sx, qy), (qx + sx, qy + sy), (qx, qy + sy)]
        , let mid = place (qx + sx / 2, qy + sy / 2)
        , front mid
        , let avg = zOf (fst mid) ]
      PSeg wd c a@(ax, ay) (bx, by) ->
        let steps = max 1 (ceiling (dist2 a (bx, by) / 30)) :: Int
            pt k = (ax + (bx - ax) * fi k / fi steps, ay + (by - ay) * fi k / fi steps)
        in [ (zOf (fst mid) + lift, seg (wd * 1.4) c (toScreen (fst (place (pt k)))) (toScreen (fst (place (pt (k + 1))))))
           | k <- [0 .. steps - 1]
           , let mid = place (pt k), front mid ]
      PDisc c q d -> [ (zOf (fst m) + lift + 1, dotC c (toScreen (fst m)) d) | let m = place q, front m ]
      PTri c a b d ->
        [ (zOf (fst m) + lift + 1, Color c (Polygon (map (fl . toScreen . fst . place) [a, b, d]))) | let m = place a, front m ]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures
  [ if w3D w && wSurf w == 1 then wPic3 w else flatPicture (wPrims w)
  , txt 30 40 20 white (name ++ "  V = " ++ show (nV b) ++ "  E = " ++ show (nE b) ++ "  F = " ++ show (nF b) ++ "  chi = " ++ show (chi b)
      ++ "   m = " ++ showM m ++ "  Z2 b = " ++ showM bs ++ (if m == bs then "  minimal" else ""))
  , txt 30 68 15 (gray 0.65) ("1-3: surface  T: 3D torus " ++ (if w3D w then "ON (drag to rotate)" else "off") ++ "  A: arrows  N: change height   purple = min  lime = saddle  coral = max")
  , txt 30 96 14 (gray 0.55) note
  , txt 30 118 14 (gray 0.55) "Glue edges of the same color so that their arrows agree."
  ]
  where
    b = wBetti w
    m = countCritical (wCx w) (wPair w)
    bs = (b0 b, b1 b, b2 b)
    (name, note) = case wSurf w of
      1 -> ("Torus", "A torus has a loop through the hole and a loop around it, so b1 = 2. Each of the 2 saddles makes one loop.")
      2 -> ("Sphere", "Fold and glue top with left, right with bottom. b1 = 0, so no saddle is needed (1 min and 1 max).")
      _ -> ("Klein bottle", "With Z2 coefficients b = (1, 2, 1). (With integers b1 = 1 plus torsion.) A non-orientable surface.")

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _
    | envDown env && w3D w ->
        let (mx, my) = mouseP env
            (px, py) = wMouse w
            w1 = w { wRy = wRy w + (mx - px) * 0.01, wRx = wRx w + (my - py) * 0.01, wMouse = (mx, my) }
        in w1 { wPic3 = torusPicture w1 }
    | otherwise -> w { wMouse = mouseP env }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wMouse = mouseP env }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> build w { wSurf = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> w { w3D = not (w3D w) }
    | c `elem` "aA" -> refresh w { wArrows = not (wArrows w) }
    | c `elem` "nN" -> build w { wPhase = wPhase w + 0.9 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. トーラスを 3D（T）で回して、2 つの鞍点（ライムの辺）がそれぞれ「穴をくぐる輪」「穴をまわる輪」をふさいでいるのを確かめよう
-- 2. 球面（2）の極大（コーラルの面）は右下の角にある。折りたたみの貼り方で角がどこに行くか指でたどろう
-- 3. N は位相をずらすだけで高さ関数の山と谷の数は同じなので、m は変わらない。b_k は下限なので m はこれより減らせないが、増やすことはできる（cos を 2 周期にすると？）
-- 4. クラインの壺（3）とトーラスは m も b（Z2）も同じ。違いはどこに出る？（辺の貼り方 → 向き付け）

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
