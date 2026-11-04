-- ============================================
-- 38-06 トロピカル 3 次曲線と種数
-- 3 次曲線のニュートン多角形には内部の格子点 (1, 1) が 1 つある。
--   その点が細分の頂点になる ⇔ 曲線に「サイクル」（有界な面）が 1 つできる → 種数 g = 1（楕円曲線！）
--   一般に 種数 = 細分の頂点になった内部格子点の数。smooth（単位三角形だけ）なら g = (d−1)(d−2)/2
-- 操作：格子点をドラッグ = 高さ / ↑↓ = 次数（2〜4）/ R = ランダム / 0 = 全部 0 / G = サイクルが出る例 / N = 消える例
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
  { wD :: !Int             -- ★ 次数
  , wTerms :: !(V.Vector Term)
  , wDrag :: !Int
  , wDragY :: !Float
  , wDragA :: !Double }

isInterior :: Int -> Term -> Bool
isInterior d t = tI t > 0 && tJ t > 0 && tI t + tJ t < d

-- ★ 内部の点を低く（サイクルあり）／高く（サイクルなし）。お椀形（smooth になる）+ 内部点を下げる／上げる
preset :: Int -> Bool -> V.Vector Term
preset d cycleOn = V.map h (makeTerms d)
  where
    h t = let i = fromIntegral (tI t) :: Double
              j = fromIntegral (tJ t) :: Double
          in t { tA = 0.3 * (i * i + j * j) + 0.07 * i * j + (if isInterior d t then (if cycleOn then -0.6 else 3) else 0) }

world0 :: World
world0 = World 3 (preset 3 True) (-1) 0 0

main :: IO ()
main = runSketch (sketch "38-06 Tropical Cubic and Genus" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = onEvent
  }

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.3 + x * sc
toY env y = envH env * 0.5 - y * sc

nsOf :: Env -> Int -> Float
nsOf env d = min 100 ((envH env - 220) / fromIntegral d)

latP :: Env -> Int -> Term -> Point
latP env d t = (envW env * 0.62 + fromIntegral (tI t) * nsOf env d, envH env * 0.84 - fromIntegral (tJ t) * nsOf env d)

-- 三角形の面積の 2 倍
triArea2 :: V.Vector Term -> Cell -> Int
triArea2 ts c = case map (ts V.!) (cIds c) of
  (p : q : r : _) -> (tI q - tI p) * (tJ r - tJ p) - (tJ q - tJ p) * (tI r - tI p)
  _ -> 0

draw :: Env -> World -> Picture
draw env w = Pictures (
  -- 左：曲線。サイクル（有界な面）を塗る
  [ seg env 1 gridC (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 gridC (0, toY env g) (envW env * 0.55, toY env g) | g <- grid ]
  ++ concatMap cyclePic used
  ++ [ let (a, b) = edgeEnds cv e in seg env (wid e) edgeC (us a) (us b) | e <- edges ]
  ++ [ let (x, y) = us (cellXY c) in dotP env x y 12 (gray 1) | c <- cellList ]
  -- 右：ニュートン多角形
  ++ [ box env (lx0 - 50) (ly0 - fd * ns - 50) (fd * ns + 140) (fd * ns + 100) (hsba 0 0 100 0.06) ]
  ++ map cellFace cellList
  ++ [ seg env (wid e) edgeC (latP env d (ts V.! eP e)) (latP env d (ts V.! eQ e)) | e <- edges ]
  ++ concat (zipWith termDot [0 ..] (V.toList ts))
  ++ [ txt env 20 30 16 (gray 1) ("Drag lattice points   Up/Down: degree d = " ++ show d ++ "   R: random   0: all zero   G: cycle   N: no cycle")
     , txt env 20 62 20 (hsb 330 70 100) ("Genus g = " ++ show (length used) ++ "  (of " ++ show interior ++ " interior points, those in the subdivision)")
     , txt env 20 90 16 (if smooth then hsb 90 80 100 else hsb 35 80 100)
         (if smooth then "smooth: only unit triangles -> g = (d-1)(d-2)/2 = " ++ show ((d - 1) * (d - 2) `div` 2) ++ " (maximum)"
                    else "not smooth: a big cell exists (red) -> genus may be below the maximum")
     , txt env 20 114 14 (gray 0.65) "Pink face = cycle (bounded region where an interior term is lowest)."
     , txt env 20 134 14 (gray 0.65) "A raised interior point leaves the lower hull and the cycle disappears."
     , txt env 20 (envH env - 24) 14 (gray 0.65) "Over C a cubic is an elliptic curve (genus 1 = a donut). Here it is one visible loop." ] )
  where
    d = wD w
    fd = fromIntegral d :: Float
    ns = nsOf env d
    lx0 = envW env * 0.62
    ly0 = envH env * 0.84
    ts = wTerms w
    cv = tropCurve ts
    edges = cvEdges cv
    cellList = V.toList (cvCells cv)
    -- 種数：内部格子点のうち細分の頂点になったもの
    used = [k | (k, t) <- zip [0 ..] (V.toList ts), isInterior d t, any ((k `elem`) . cIds) cellList]
    interior = length (filter (isInterior d) (V.toList ts))
    isUnit c = length (cIds c) == 3 && abs (triArea2 ts c) == 1
    smooth = all isUnit cellList
    grid = [fromIntegral g | g <- [-10 .. 10 :: Int]] :: [Float]
    gridC = hsba 0 0 100 0.18
    edgeC = hsb 35 80 100
    wid e = 2 + fromIntegral (eW e) * 2
    us (x, y) = (toX env x, toY env y)
    cyclePic k = case regionPoly cv k ts 100 of
      Nothing -> []
      Just poly' ->
        let ps = [ (realToFrac x, realToFrac y) | (x, y) <- poly' ] :: [Point]
            m = fromIntegral (length ps) :: Float
            mx = sum (map fst ps) / m
            my = sum (map snd ps) / m
            t = ts V.! k
        in [ Color (hsba 330 70 100 0.35) (Polygon [ let (x, y) = us p in pt env x y | p <- ps ])
           , txt env (toX env mx - 36) (toY env my + 5) 14 (hsb 330 60 100) ("x^" ++ show (tI t) ++ " y^" ++ show (tJ t) ++ " lowest") ]
    cellFace c =
      let col = if isUnit c then hsba 90 50 50 0.4 else hsba 0 70 60 0.4
      in Color col (Polygon [ let (x, y) = latP env d (ts V.! q) in pt env x y | q <- cIds c ])
    termDot k t =
      let (x, y) = latP env d t
          inner = isInterior d t
          isUsed = k `elem` used
      in [ dotO env x y (if inner then 26 else 20)
             (hsb (if inner then 330 else 190) (if inner then (if isUsed then 90 else 30) else 20) 100)
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
    let v = max (-6) (min 6 (wDragA w + realToFrac (wDragY w - snd (mouseP env)) * 0.02))
        k = wDrag w
    in pure w { wTerms = wTerms w V.// [(k, (wTerms w V.! k) { tA = v })] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (setDegree (min 4 (wD w + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (setDegree (max 2 (wD w - 1)))
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> do
        ts <- V.mapM (\t -> do r <- randomRIO (-2, 2 :: Double)
                               pure t { tA = r }) (wTerms w)
        pure w { wTerms = ts }
    | ch == '0' -> pure w { wTerms = V.map (\t -> t { tA = 0 }) (wTerms w) }
    | ch `elem` "gG" -> pure w { wTerms = preset (wD w) True }
    | ch `elem` "nN" -> pure w { wTerms = preset (wD w) False }
  _ -> pure w
  where
    setDegree d = w { wD = d, wTerms = preset d True, wDrag = -1 }

-- ---------- やってみよう ----------
-- 1. 内部の点 (1,1) をゆっくり上げていって、サイクルが消える瞬間の高さを見つけよう（他の 3 点が作る平面の高さ）
-- 2. d = 4 にして g = 3 を作ろう。内部の 3 点をすべて低くするとどうなる？
-- 3. サイクルの辺の重みを 2 にできる？（細分の内部の辺が格子点をとばすように）
-- 4. 種数 g の曲線の「サイクルの長さ」（辺の格子長の和）を計算して表示してみよう（j 不変量と関係がある）


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

-- 項 k が最小になる領域（曲線の頂点を結んだ多角形）。無限に伸びる領域は far だけ先で切る
regionPoly :: Curve -> Int -> V.Vector Term -> Double -> Maybe [(Double, Double)]
regionPoly cv k ts far
  | null cs0 = Nothing
  | length rays < 2 = Just [(cX c, cY c) | ci <- sorted, let c = cell ci]
  | otherwise = Just (tipOf rA ++ [(cX c, cY c) | ci <- rotated, let c = cell ci] ++ tipOf rB)
  where
    cell ci = cvCells cv V.! ci
    cs0 = [ci | (ci, c) <- zip [0 ..] (V.toList (cvCells cv)), k `elem` cIds c]
    tk = ts V.! k
    ang :: Int -> Double
    ang ci =
      let ids = cIds (cell ci)
          m = fromIntegral (length ids) :: Double
          mi = sum [fromIntegral (tI (ts V.! q)) / m | q <- ids]
          mj = sum [fromIntegral (tJ (ts V.! q)) / m | q <- ids]
      in atan2 (mj - fromIntegral (tJ tk)) (mi - fromIntegral (tI tk))
    sorted = sortOn ang cs0
    rays = [e | e <- cvEdges cv, eC2 e < 0, eP e == k || eQ e == k]
    -- 境界の点：いちばん大きい角度のすき間のあとから始める
    m0 = length sorted
    gaps = [ (g, (i + 1) `mod` m0)
           | (i, c, nxt) <- zip3 [0 :: Int ..] sorted (drop 1 sorted ++ take 1 sorted)
           , let g = ang nxt - ang c + (if i == m0 - 1 then 2 * pi else 0) ]
    bi = snd (foldl' (\b x -> if fst x > fst b then x else b) (-1, 0) gaps)
    rotated = drop bi sorted ++ take bi sorted
    firstC = take 1 rotated
    lastC = take 1 (reverse rotated)
    sameEdge a b = eP a == eP b && eQ a == eQ b
    rA = take 1 (filter (\e -> [eC1 e] == firstC) rays ++ rays)
    notA e = not (any (sameEdge e) rA)
    rB = take 1 (filter (\e -> notA e && [eC1 e] == lastC) rays ++ filter notA rays ++ rA)
    tipOf es = [ (cX c + fromIntegral (eDx e) * far, cY c + fromIntegral (eDy e) * far)
               | e <- es, let c = cell (eC1 e) ]
