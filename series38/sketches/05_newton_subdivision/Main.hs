-- ============================================
-- 38-05 ニュートン多角形の正則細分と双対グラフ
-- 次数 d の曲線：格子点 (i, j), i + j ≤ d の高さ a_ij が係数。高さをノイズでゆらす／ドラッグで変形
--   細分の各辺の「格子長」（辺の上にある格子点の間隔の数）= 曲線の対応する辺の 重み
--   重みは辺の太さと数字で表示。重み 2 以上の辺は、細分で格子点を「とばす」辺から生まれる
-- 操作：↑↓ = 次数 d（2〜4）/ 格子点をドラッグ = 高さ / スペース = ノイズを流す / R = ランダム / 0 = 全部 0 / W = 重み 2 の例
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

amp, sc :: Float
amp = 1.5                  -- ★ ノイズの大きさ
sc = 55

data World = World
  { wD :: !Int             -- ★ 次数（2〜4）
  , wTerms :: !(V.Vector Term)
  , wOff :: !(U.Vector Double)   -- ドラッグで足した分
  , wFlow :: !Bool         -- スペースでノイズを流す
  , wTT :: !Float
  , wDrag :: !Int
  , wDragY :: !Float
  , wDragA :: !Double }

-- 格子点 (i, j) のノイズの高さ
noiseH :: Float -> Term -> Double
noiseH tt t = realToFrac (noise3 (fromIntegral (tI t) * 0.9 + 10) (fromIntegral (tJ t) * 0.9 + 20) tt * amp * 2 - amp)

rebuild :: Int -> Float -> World -> World
rebuild d tt w =
  let ts = V.map (\t -> t { tA = noiseH tt t }) (makeTerms d)
  in w { wD = d, wTT = tt, wTerms = ts, wOff = U.replicate (V.length ts) 0, wDrag = -1 }

world0 :: World
world0 = rebuild 3 0 (World 3 V.empty U.empty False 0 (-1) 0 0)

main :: IO ()
main = runSketch (sketch "38-05 Regular Subdivision of the Newton Polygon" world0)
  { skBack  = bgCol
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = draw
  , skEvent = onEvent
  }

stepWorld :: World -> World
stepWorld w
  | wFlow w =
      let tt = wTT w + 0.004
      in w { wTT = tt, wTerms = V.imap (\k t -> t { tA = noiseH tt t + wOff w U.! k }) (wTerms w) }
  | otherwise = w

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.3 + x * sc
toY env y = envH env * 0.52 - y * sc

nsOf :: Env -> Int -> Float
nsOf env d = min 100 ((envH env - 220) / fromIntegral d)

latX, latY :: Env -> Int -> Float -> Float
latX env d i = envW env * 0.62 + i * nsOf env d
latY env d j = envH env * 0.84 - j * nsOf env d

latP :: Env -> Int -> Term -> Point
latP env d t = (latX env d (fromIntegral (tI t)), latY env d (fromIntegral (tJ t)))

hueOf :: Int -> Float
hueOf ei = fromIntegral ((ei * 37 + 20) `mod` 360)

edgeCol :: Int -> Edge -> Color
edgeCol ei e = if eW e > 1 then hsb 55 95 100 else hsb (hueOf ei) 65 100

-- セルの重心（格子の座標）
centroid :: V.Vector Term -> Cell -> Point
centroid ts c =
  let ids = cIds c
      m = fromIntegral (length ids) :: Float
  in (sum [fromIntegral (tI (ts V.! q)) / m | q <- ids], sum [fromIntegral (tJ (ts V.! q)) / m | q <- ids])

-- 三角形の面積の 2 倍
triArea2 :: V.Vector Term -> Cell -> Int
triArea2 ts c = case map (ts V.!) (cIds c) of
  (p : q : r : _) -> (tI q - tI p) * (tJ r - tJ p) - (tJ q - tJ p) * (tI r - tI p)
  _ -> 0

draw :: Env -> World -> Picture
draw env w = Pictures (
  -- 左：曲線
  [ seg env 1 gridC (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 gridC (0, toY env g) (envW env * 0.55, toY env g) | g <- grid ]
  ++ concat (zipWith curveEdge [0 ..] edges)
  ++ [ let (x, y) = us (cellXY c) in dotP env x y 12 (gray 1) | c <- cellList ]
  -- 右：ニュートン多角形と細分
  ++ [ box env (latX env d 0 - 50) (latY env d 0 - fd * ns - 50) (fd * ns + 140) (fd * ns + 100) (hsba 0 0 100 0.06) ]
  ++ [ Color (hsba 190 40 40 0.4) (Polygon [ let (x, y) = latP env d (ts V.! q) in pt env x y | q <- cIds c ]) | c <- cellList ]
  ++ concat (zipWith subEdge [0 ..] edges)
  -- 双対グラフ：セルの重心どうしを結ぶ（曲線の骨格）
  ++ [ seg env 1.5 (hsba 0 0 100 0.6) (latC (cells V.! eC1 e)) (latC (cells V.! eC2 e)) | e <- edges, eC2 e >= 0 ]
  ++ [ let (x, y) = latC c in dotP env x y 9 (gray 1) | c <- cellList ]
  ++ concat (zipWith termDot [0 ..] (V.toList ts))
  ++ [ txt env 20 30 16 (gray 1) ("Up/Down: d = " ++ show d ++ "   Drag points: height   Space: noise " ++ (if wFlow w then "flowing" else "stopped") ++ "   R: random   0: all zero   W: weight 2")
     , txt env 20 52 14 (gray 0.65) ("Terms " ++ show (V.length ts) ++ "   Cells " ++ show (length cellList) ++ " (curve vertices)   Edges " ++ show (length edges) ++ "   Edges with weight >= 2: " ++ show heavy)
     , txt env 20 74 14 (gray 0.65) ((if unit then "All unit triangles = smooth (all vertices 3-valent)" else "Big cells exist = not smooth") ++ ". White lines = dual graph")
     , txt env 20 (envH env - 44) 14 (gray 0.65) "Subdivision edge is perpendicular to curve edge. Lattice length of edge = weight of curve edge."
     , txt env 20 (envH env - 24) 14 (gray 0.65) "If smooth: vertices = d^2, bounded edges = 3d(d-1)/2, rays = 3d" ] )
  where
    d = wD w
    fd = fromIntegral d :: Float
    ns = nsOf env d
    ts = wTerms w
    cv = tropCurve ts
    cells = cvCells cv
    edges = cvEdges cv
    cellList = V.toList cells
    grid = [fromIntegral g | g <- [-10 .. 10 :: Int]] :: [Float]
    gridC = hsba 0 0 100 0.18
    us (x, y) = (toX env x, toY env y)
    latC c = let (i, j) = centroid ts c in (latX env d i, latY env d j)
    heavy = length (filter ((> 1) . eW) edges)
    unit = all (\c -> length (cIds c) == 3 && abs (triArea2 ts c) == 1) cellList
    wid e = 2 + fromIntegral (eW e) * 2.5
    curveEdge ei e =
      let (a, b) = edgeEnds cv e
          (ax, ay) = a
          (bx, by) = b
          (mx, my) | eC2 e >= 0 = ((ax + bx) / 2, (ay + by) / 2)
                   | otherwise = (ax + fromIntegral (eDx e) * 1.2, ay + fromIntegral (eDy e) * 1.2)
      in seg env (wid e) (edgeCol ei e) (us a) (us b)
         : [ txt env (toX env mx + 8) (toY env my - 8) 16 (hsb 55 95 100) ("w = " ++ show (eW e)) | eW e > 1 ]
    subEdge ei e =
      let a@(ax, ay) = latP env d (ts V.! eP e)
          b@(bx, by) = latP env d (ts V.! eQ e)
      in seg env (wid e) (edgeCol ei e) a b
         : [ txt env ((ax + bx) / 2 + 8) ((ay + by) / 2 - 8) 16 (hsb 55 95 100) ("len " ++ show (eW e)) | eW e > 1 ]
    termDot k t =
      let (x, y) = latP env d t
      in [ dotO env x y 20 (if wDrag w == k then hsb 55 90 100 else hsb 190 20 100)
         , txt env (x + 12) (y - 10) 14 (gray 0.85) (nfd 1 (tA t)) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [k | (k, t) <- zip [0 ..] (V.toList (wTerms w)), dist2 m (latP env (wD w) t) < 18]
    in pure (case reverse hit of
         (k : _) -> w { wDrag = k, wDragY = snd m, wDragA = tA (wTerms w V.! k) }
         [] -> w { wDrag = -1 })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let k = wDrag w
        t = wTerms w V.! k
        na = max (-6) (min 6 (wDragA w + realToFrac (wDragY w - snd (mouseP env)) * 0.02))
    in pure w { wTerms = wTerms w V.// [(k, t { tA = na })]
              , wOff = wOff w U.// [(k, wOff w U.! k + (na - tA t))] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild (min 4 (wD w + 1)) (wTT w) w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild (max 2 (wD w - 1)) (wTT w) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFlow = not (wFlow w) }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> pure (rebuild (wD w) (wTT w + 7) w)
    | ch == '0' -> pure w { wOff = U.map (const 0) (wOff w), wTerms = V.map (\t -> t { tA = 0 }) (wTerms w) }
    -- お椀形 + (1,0) だけ持ち上げる → 辺 (0,0)–(2,0) が格子長 2
    | ch `elem` "wW" -> pure w
        { wFlow = False, wOff = U.map (const 0) (wOff w)
        , wTerms = V.map (\t -> t { tA = 0.3 * fromIntegral (tI t * tI t + tJ t * tJ t) + (if tI t == 1 && tJ t == 0 then 1 else 0) }) (wTerms w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 辺の太さ（重み）が変わる瞬間を探そう。細分で「格子点をとばす」辺が出たときだ
-- 2. 全部 0（0 キー）の細分は「分けない」= 曲線は 1 点に 3 本の太い半直線。重みはいくつ？
-- 3. d = 4 で smooth なとき、頂点は 16 個、半直線は 12 本。数えてみよう
-- 4. ノイズの amp を大きくすると、どんな細分が出やすい？ 小さくすると？


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str


-- ==================================================
-- ここから下は「トロピカル曲線の道具」
--   Term i j a … 単項 a ⊗ x^i ⊗ y^j（a = 係数 = 格子点 (i, j) の高さ）
--   F(x, y) = min (a + i x + j y)。tropCurve は曲線を「頂点と辺」で返す
--     Cell … 細分のセル（高さの下側凸包の面）⇔ 曲線の頂点 (x, y)。cIds は項番号
--     Edge … 細分の辺（項 p–q）⇔ 曲線の辺。eW = 格子長 = 重み
--       eC2 >= 0 なら頂点 eC1 → eC2 の線分。eC2 < 0 なら頂点 eC1 から方向 (eDx, eDy) の半直線
--   同じ平面の上にあるかを 1e-7 の精度で調べるので、高さと頂点は Double で持つ
-- ==================================================
data Term = Term { tI :: !Int, tJ :: !Int, tA :: !Double }

data Cell = Cell { cIds :: [Int], cX :: !Double, cY :: !Double }

data Edge = Edge
  { eP, eQ :: !Int      -- 細分の辺の両端（項番号）
  , eW :: !Int          -- 重み
  , eC1, eC2 :: !Int    -- 両側のセル（eC2 < 0 なら半直線）
  , eDx, eDy :: !Int }  -- 原始方向ベクトル

data Curve = Curve { cvCells :: V.Vector Cell, cvEdges :: [Edge] }

tropCurve :: V.Vector Term -> Curve
tropCurve ts = Curve cells edges
  where
    n' = V.length ts
    eps' = 1e-7 :: Double
    term k = ts V.! k
    fi :: Int -> Double
    fi = fromIntegral
    -- 3 点を通る平面 z = al + be*i + ga*j。すべての点がその上側なら「下側凸包の面」
    face p q r =
      let tp = term p
          tq = term q
          tr = term r
          di1 = tI tq - tI tp
          dj1 = tJ tq - tJ tp
          di2 = tI tr - tI tp
          dj2 = tJ tr - tJ tp
          det = di1 * dj2 - dj1 * di2
          da1 = tA tq - tA tp
          da2 = tA tr - tA tp
          be = (da1 * fi dj2 - fi dj1 * da2) / fi det
          ga = (fi di1 * da2 - fi di2 * da1) / fi det
          al = tA tp - be * fi (tI tp) - ga * fi (tJ tp)
          collect :: Int -> [Int] -> Maybe [Int]
          collect !k acc
            | k >= n' = Just (reverse acc)
            | dd < negate eps' = Nothing
            | dd < eps' = collect (k + 1) (k : acc)
            | otherwise = collect (k + 1) acc
            where t = term k
                  dd = tA t - (al + be * fi (tI t) + ga * fi (tJ t))
      in if det == 0 then Nothing else fmap (\ids -> (ids, negate be, negate ga)) (collect 0 [])
    faces = [f | p <- [0 .. n' - 1], q <- [p + 1 .. n' - 1], r <- [q + 1 .. n' - 1], Just f <- [face p q r]]
    -- 同じ面は 1 回だけ。面の傾き → 曲線の頂点
    cells = V.fromList (reverse (snd (foldl' addFace (Set.empty, []) faces)))
    addFace (seen, acc) (ids, x, y)
      | Set.member ids seen = (seen, acc)
      | length hull >= 3 = (seen', Cell hull x y : acc)
      | otherwise = (seen', acc)
      where seen' = Set.insert ids seen
            hull = hull2d ts ids
    -- セルのまわりの辺を集める。2 つのセルが共有する辺は有界な辺、1 つだけなら半直線
    (_, rawEdges, _) = V.ifoldl' addCell (Map.empty, IntMap.empty, 0 :: Int) cells
    addCell st ci c =
      let ids = cIds c
      in foldl' (addEdge ci) st (zip ids (drop 1 ids ++ take 1 ids))
    addEdge ci (mp, es, next) (p, q) =
      let key = (min p q, max p q)
      in case Map.lookup key mp of
           Just ei -> (mp, IntMap.adjust (\(a, b, c1, _) -> (a, b, c1, ci)) ei es, next)
           Nothing -> (Map.insert key next mp, IntMap.insert next (p, q, ci, -1 :: Int) es, next + 1)
    total = fromIntegral (max n' 1) :: Double
    ci0 = sum [fi (tI t) / total | t <- V.toList ts]
    cj0 = sum [fi (tJ t) / total | t <- V.toList ts]
    edges = map finish (IntMap.elems rawEdges)
    finish (p, q, c1, c2) =
      let tp = term p
          tq = term q
          di = tI tq - tI tp
          dj = tJ tq - tJ tp
          g = gcdInt (abs di) (abs dj)
          ux' = dj `quot` g                -- 細分の辺に垂直な原始ベクトル
          uy' = negate di `quot` g
          ca = cells V.! c1
          s | c2 >= 0 = let cb = cells V.! c2 in fi ux' * (cX cb - cX ca) + fi uy' * (cY cb - cY ca)
            | otherwise = fi ux' * (ci0 - fi (tI tp)) + fi uy' * (cj0 - fi (tJ tp))   -- 半直線はニュートン多角形の内向き（min 流儀）
          sg = if s < 0 then -1 else 1
      in Edge p q g c1 c2 (sg * ux') (sg * uy')

gcdInt :: Int -> Int -> Int
gcdInt a b = let g = gcd a b in if g == 0 then 1 else g

-- 格子点の集合の凸包（反時計回り。一直線上の点は除く）
hull2d :: V.Vector Term -> [Int] -> [Int]
hull2d ts ids
  | length pts < 3 = pts
  | otherwise = reverse (drop 1 (chain pts)) ++ reverse (drop 1 (chain (reverse pts)))
  where
    pts = sortOn (\k -> let t = ts V.! k in (tI t, tJ t)) ids
    turn o a b =
      let to = ts V.! o
          ta = ts V.! a
          tb = ts V.! b
      in (tI ta - tI to) * (tJ tb - tJ to) - (tJ ta - tJ to) * (tI tb - tI to)
    chain = foldl' push []
    push (b : a : rest) p | turn a b p <= 0 = push (a : rest) p
    push st p = p : st

-- 次数 d のすべての項 (i + j <= d) を高さ 0 でつくる
makeTerms :: Int -> V.Vector Term
makeTerms d = V.fromList [Term i j 0 | i <- [0 .. d], j <- [0 .. d - i]]

-- 頂点の座標
cellXY :: Cell -> Point
cellXY c = (realToFrac (cX c), realToFrac (cY c))

-- 辺の両端（半直線は長さ 100 で切る）
edgeEnds :: Curve -> Edge -> (Point, Point)
edgeEnds cv e =
  let a@(ax, ay) = cellXY (cvCells cv V.! eC1 e)
      b | eC2 e >= 0 = cellXY (cvCells cv V.! eC2 e)
        | otherwise = (ax + fromIntegral (eDx e) * 100, ay + fromIntegral (eDy e) * 100)
  in (a, b)
