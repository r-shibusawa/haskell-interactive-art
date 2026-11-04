-- ============================================
-- 38-04 トロピカル 2 次曲線とニュートン多角形の双対性
-- F = min(a00, a10 + x, a01 + y, a20 + 2x, a11 + x + y, a02 + 2y)：6 つの係数 = 6 つの格子点の「高さ」
--   高さを 3 次元に持ち上げて下から見た凸包 = ニュートン三角形の「正則細分」
--   細分の 辺 ⇔ 曲線の 辺（垂直）， 三角形 ⇔ 曲線の 頂点， 境界の辺 ⇔ 無限に伸びる半直線（同じ色で対応）
-- 操作：右の格子点を上下にドラッグ = 高さを変える / R = ランダム / 0 = 全部 0 / S = なめらかな例
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Numeric (showFFloat)

sc, ns :: Float
sc = 60                    -- ★ 曲線側：1 単位 = 何 px
ns = 120                   -- ニュートン多角形の間隔

data World = World
  { wTerms :: !(V.Vector Term)   -- ★ 6 つの項
  , wDrag :: !Int
  , wDragY :: !Float
  , wDragA :: !Double }

-- ★ 4 つの三角形に割れる例
example :: V.Vector Term
example = V.map (\t -> t { tA = h (tI t) (tJ t) }) (makeTerms 2)
  where h :: Int -> Int -> Double
        h 0 0 = 1
        h 2 0 = 1
        h 0 2 = 1
        h _ _ = 0

world0 :: World
world0 = World example (-1) 0 0

main :: IO ()
main = runSketch (sketch "38-04 Tropical Conic and Duality" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = onEvent
  }

toX, toY, latX, latY :: Env -> Float -> Float
toX env x = envW env * 0.3 + x * sc
toY env y = envH env * 0.55 - y * sc
latX env i = envW env * 0.66 + i * ns
latY env j = envH env * 0.78 - j * ns

latP :: Env -> Term -> Point
latP env t = (latX env (fromIntegral (tI t)), latY env (fromIntegral (tJ t)))

cellHue, edgeHue :: Int -> Float
cellHue ci = fromIntegral ((ci * 73 + 30) `mod` 360)
edgeHue ei = fromIntegral ((ei * 41 + 200) `mod` 360)

draw :: Env -> World -> Picture
draw env w = Pictures (
  -- 左：曲線
  [ seg env 1 gridC (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 gridC (0, toY env g) (envW env * 0.55, toY env g) | g <- grid ]
  ++ zipWith curveEdge [0 ..] edges
  ++ concat (zipWith cellDot [0 ..] cellList)
  -- 右：ニュートン多角形と正則細分
  ++ [ box env (latX env 0 - 60) (latY env 0 - 2 * ns - 60) (2 * ns + 180) (2 * ns + 120) (hsba 0 0 100 0.06) ]
  ++ concat (zipWith cellFace [0 ..] cellList)
  ++ zipWith subEdge [0 ..] edges
  ++ concat (zipWith termDot [0 ..] (V.toList ts))
  -- テキスト
  ++ [ txt env 20 30 16 (gray 1) "Drag lattice points up/down = heights   R: random   0: all zero   S: smooth example"
     , txt env 20 52 14 (gray 0.65) "Left = tropical conic (min attained twice).  Right = regular subdivision. Same colour = dual."
     , txt env 20 74 14 (gray 0.65) ("Cells " ++ show nc ++ " (= curve vertices)   Edges " ++ show (length edges) ++ " (rays " ++ show (length (filter ((< 0) . eC2) edges)) ++ " = boundary edges)")
     , txt env 20 96 14 (gray 0.65) status
     , txt env 20 (envH env - 24) 14 (gray 0.65) ("F(x, y) = min(" ++ intercalate ", " (map termStr (V.toList ts)) ++ ")") ] )
  where
    ts = wTerms w
    cv = tropCurve ts
    edges = cvEdges cv
    cellList = V.toList (cvCells cv)
    nc = length cellList
    tri = length (filter ((== 3) . length . cIds) cellList)
    status
      | tri == nc && nc == 4 = "smooth: 4 unit triangles -> 4 vertices"
      | nc == 1 = "No subdivision: one point with rays (weighted)"
      | otherwise = "Some cells are big -> fewer vertices (not smooth)"
    grid = [fromIntegral g | g <- [-8 .. 8 :: Int]] :: [Float]
    gridC = hsba 0 0 100 0.2
    us (x, y) = (toX env x, toY env y)
    curveEdge ei e =
      let (a, b) = edgeEnds cv e
      in seg env (3 + fromIntegral (eW e) * 2) (hsb (edgeHue ei) 70 100) (us a) (us b)
    cellDot :: Int -> Cell -> [Picture]
    cellDot ci c =
      let (x, y) = us (cellXY c)
      in [ dotP env x y 18 (hsb (cellHue ci) 70 100)
         , txt env (x + 12) (if odd ci then y - 10 else y + 24) 14 (gray 1) ("(" ++ nfd 1 (cX c) ++ ", " ++ nfd 1 (cY c) ++ ")") ]
    cellFace ci c =
      let ids = cIds c
          m = fromIntegral (length ids) :: Float
          mi = sum [fromIntegral (tI (ts V.! q)) / m | q <- ids]
          mj = sum [fromIntegral (tJ (ts V.! q)) / m | q <- ids]
      in [ Color (hsba (cellHue ci) 60 60 0.45) (Polygon [ let (x, y) = latP env (ts V.! q) in pt env x y | q <- ids ])
         , dotP env (latX env mi) (latY env mj) 14 (hsb (cellHue ci) 70 100) ]
    subEdge ei e =
      seg env (3 + fromIntegral (eW e) * 2) (hsb (edgeHue ei) 70 100) (latP env (ts V.! eP e)) (latP env (ts V.! eQ e))
    termDot k t =
      let (x, y) = latP env t
          on = wDrag w == k
      in [ dotO env x y 26 (if on then hsb 55 90 100 else gray 1)
         , txt env (x + 16) (y - 14) 14 (gray 1) ("a" ++ show (tI t) ++ show (tJ t) ++ " = " ++ nfd 1 (tA t))
         , txt env (x + 16) (y + 26) 14 (gray 0.6) ("x^" ++ show (tI t) ++ " y^" ++ show (tJ t)) ]
    termStr t = nfd 1 (tA t) ++ part (tI t) "x" ++ part (tJ t) "y"
    part :: Int -> String -> String
    part k v | k == 0 = ""
             | k == 1 = " + " ++ v
             | otherwise = " + " ++ show k ++ v

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [k | (k, t) <- zip [0 ..] (V.toList (wTerms w)), dist2 m (latP env t) < 20]
    in pure (case reverse hit of
         (k : _) -> w { wDrag = k, wDragY = snd m, wDragA = tA (wTerms w V.! k) }
         [] -> w { wDrag = -1 })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let v = max (-6) (min 6 (wDragA w + realToFrac (wDragY w - snd (mouseP env)) * 0.02))
        k = wDrag w
    in pure w { wTerms = wTerms w V.// [(k, (wTerms w V.! k) { tA = v })] }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> do
        ts <- V.mapM (\t -> do r <- randomRIO (-3, 3 :: Double)
                               pure t { tA = fromIntegral (floor (r * 2 + 0.5) :: Int) / 2 }) (wTerms w)
        pure w { wTerms = ts }
    | ch == '0' -> pure w { wTerms = V.map (\t -> t { tA = 0 }) (wTerms w) }
    | ch `elem` "sS" -> pure w { wTerms = example }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 全部 0 にしてから a11（真ん中）だけ下げる／上げる。細分と曲線はどう変わる？
-- 2. 曲線の辺は、対応する細分の辺と必ず垂直。色を見て確かめよう
-- 3. 角の点 a20 をうんと上げると、x^2 の項は「どこでも最小にならない」。曲線の半直線は何本になる？
-- 4. 06 のように次数 3 にすると（makeTerms 3）、真ん中の点が「サイクル」をつくる


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
