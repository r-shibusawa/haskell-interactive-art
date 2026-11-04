-- ============================================
-- 38-12 バランス条件
-- トロピカル曲線のどの頂点でも、出ていく辺の（重み × 原始方向ベクトル）の和は 0 になる
--   例：直線の頂点では (1,0) + (0,1) + (−1,−1) = 0。重み 2 の辺は 2 倍で数える
--   逆に「バランスした折れ線グラフ」は必ずどれかのトロピカル多項式の曲線になる（Mikhalkin の定理）
-- 操作：頂点をクリック = ベクトルを展開 / ↑↓ = 次数 / R = ランダム / W = 重み 2 の辺がある例 / 0 = 全部 0
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Numeric (showFFloat)

sc :: Float
sc = 50

data World = World
  { wD :: !Int
  , wTerms :: !(V.Vector Term)
  , wPicked :: !Int }

randomize :: World -> IO World
randomize w = do
  ts <- V.mapM (\t -> do r <- randomRIO (-2, 2 :: Double)
                         pure t { tA = r + bowl t }) (makeTerms (wD w))
  pure w { wTerms = ts, wPicked = -1 }

bowl :: Term -> Double
bowl t = 0.3 * fromIntegral (tI t * tI t + tJ t * tJ t)

world0 :: World
world0 = World 3 (makeTerms 3) (-1)

main :: IO ()
main = runSketch (sketch "38-12 Balancing Condition" world0)
  { skBack  = bgCol
  , skStart = \_ -> randomize world0
  , skDraw  = draw
  , skEvent = onEvent
  }

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.38 + x * sc
toY env y = envH env * 0.52 - y * sc

-- ★ 頂点 ci から出ていく辺：(w, ux, uy)（原始方向ベクトルは外向き）
outgoing :: Curve -> Int -> [(Int, Int, Int)]
outgoing cv ci = concatMap f (cvEdges cv)
  where
    f e | eC1 e == ci = [(eW e, eDx e, eDy e)]
        | eC2 e == ci = [(eW e, negate (eDx e), negate (eDy e))]
        | otherwise = []

sumVec :: [(Int, Int, Int)] -> (Int, Int)
sumVec vs = (sum [w * x | (w, x, _) <- vs], sum [w * y | (w, _, y) <- vs])

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ seg env 1 gridC (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 gridC (0, toY env g) (envW env * 0.7, toY env g) | g <- grid ]
  ++ concatMap edgePic (cvEdges cv)
  -- 全頂点でのバランスを確認（小さく表示）
  ++ zipWith cellPic [0 ..] cellList
  ++ (if validPick then pickedPic else [])
  ++ [ txt env 20 30 16 (gray 1) ("Click a vertex: expand vectors   Up/Down: degree d = " ++ show (wD w) ++ "   R: random   W: weight 2 example   0: all zero")
     , txt env 20 52 14 (gray 0.65) ("Vertices " ++ show (length cellList) ++ "   Balanced everywhere: " ++ (if allOk then "yes" else "NO") ++ ".  Green = balanced. Edge thickness = weight w.")
     , txt env 20 72 14 (gray 0.65) "Direction vectors = shortest integer vectors (primitive)."
     , txt env 20 (envH env - 44) 14 (gray 0.65) "Why 0? Walking around the dual cell (polygon) of a vertex returns to the start:"
     , txt env 20 (envH env - 24) 14 (gray 0.65) "curve edge is perpendicular to cell edge, and weight = lattice length of the cell edge." ] )
  where
    cv = tropCurve (wTerms w)
    cellList = V.toList (cvCells cv)
    picked = wPicked w
    validPick = picked >= 0 && picked < length cellList
    grid = [fromIntegral g | g <- [-12 .. 12 :: Int]] :: [Float]
    gridC = hsba 0 0 100 0.15
    us (x, y) = (toX env x, toY env y)
    edgePic e =
      let (a@(ax, ay), b@(bx, by)) = edgeEnds cv e
          (mx, my) | eC2 e >= 0 = ((ax + bx) / 2, (ay + by) / 2)
                   | otherwise = (ax + fromIntegral (eDx e), ay + fromIntegral (eDy e))
      in seg env (2 + fromIntegral (eW e) * 2) (hsba 35 70 100 0.8) (us a) (us b)
         : [ txt env (toX env mx + 6) (toY env my - 6) 14 (hsb 55 90 100) ("w=" ++ show (eW e)) | eW e > 1 ]
    balanced ci = sumVec (outgoing cv ci) == (0, 0)
    allOk = all balanced [0 .. length cellList - 1]
    cellPic ci c =
      let (x, y) = us (cellXY c)
      in dotO env x y (if ci == picked then 22 else 14)
           (hsb (if ci == picked then 330 else if balanced ci then 90 else 0) 80 100)
    -- 選んだ頂点：ベクトルを矢印で（そこから出る向きに）、さらに頭と尻をつないで閉じた多角形に
    pickedPic =
      let c = cvCells cv V.! picked
          (cx, cy) = cellXY c
          vs = sortOn (\(_, ux, uy) -> atan2 (fromIntegral uy) (fromIntegral ux :: Float)) (outgoing cv picked)
          l = 1.3 :: Float
          col = hsb 330 80 100
          fi :: Int -> Float
          fi = fromIntegral
          px = envW env * 0.78
          py = envH env * 0.62
          k = 36
          chain = scanl (\(ax, ay) (vw, ux, uy) -> (ax + fi (vw * ux), ay + fi (vw * uy))) (0, 0) vs
          tx = envW env * 0.62
          ty = 92
          (sumX, sumY) = sumVec vs
      in [ Color col (arrow 3 (pt env (toX env cx) (toY env cy))
                               (pt env (toX env (cx + fi ux * fi vw * l)) (toY env (cy + fi uy * fi vw * l))))
         | (vw, ux, uy) <- vs ]
         -- 右側に「つないで一周」の図
         ++ [ box env (px - 160) (py - 170) 320 340 (hsba 0 0 100 0.06)
            , txt env (px - 150) (py - 180) 14 (gray 1) "Chain head to tail: back to start" ]
         ++ [ Color col (arrow 3 (pt env (px + ax * k) (py - ay * k)) (pt env (px + bx * k) (py - by * k)))
            | ((ax, ay), (bx, by)) <- zip chain (drop 1 chain) ]
         ++ [ dotP env px py 10 (hsb 90 80 100)
            -- 式
            , txt env tx ty 16 (gray 1) ("Vertex (" ++ nfd 2 (cX c) ++ ", " ++ nfd 2 (cY c) ++ ")   " ++ show (length vs) ++ " edges") ]
         ++ zipWith (\idx (vw, ux, uy) -> txt env tx (ty + 26 + idx * 22) 15 (hsb 330 60 100)
                       (show vw ++ " x (" ++ show ux ++ ", " ++ show uy ++ ")")) [0 ..] vs
         ++ [ txt env tx (ty + 36 + fromIntegral (length vs) * 22) 16 (hsb 90 80 100)
                ("Sum = (" ++ show sumX ++ ", " ++ show sumY ++ ")" ++ (if sumX == 0 && sumY == 0 then "  OK balanced" else "  NO")) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        cv = tropCurve (wTerms w)
        near (x, y) = dist2 m (toX env x, toY env y) < 16
        hit = [ci | (ci, c) <- zip [0 ..] (V.toList (cvCells cv)), near (cellXY c)]
    in pure w { wPicked = if null hit then -1 else last hit }
  EventKey (SpecialKey KeyUp) Down _ _ -> randomize w { wD = min 4 (wD w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomize w { wD = max 1 (wD w - 1) }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> randomize w
    | ch == '0' -> pure w { wTerms = V.map (\t -> t { tA = 0 }) (wTerms w), wPicked = -1 }
    | ch `elem` "wW" -> pure w
        { wTerms = V.map (\t -> t { tA = bowl t + (if tI t == 1 && tJ t == 0 then 1.5 else 0) }) (wTerms w), wPicked = -1 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 0 キー：全部 0 なら頂点は 1 つで、d 倍の重みの半直線が 3 本。d × ((1,0) + (0,1) + (−1,−1)) = 0 を確かめよう
-- 2. 重み 2 の辺を持つ頂点（W）では、他の辺の向きがどう変わる？ 展開図で見よう
-- 3. 4 本以上の辺が出る頂点（細分のセルが四角形）を作ろう：ランダムを繰り返すか 04 の要領で高さを調整
-- 4. 自分で「バランスした折れ線」を紙に描いてみよう。半直線の方向の和が 0 なら、必ずどれかの曲線になる


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
