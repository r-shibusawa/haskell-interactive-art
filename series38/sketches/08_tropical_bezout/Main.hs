-- ============================================
-- 38-08 トロピカル・ベズーの定理
-- 一般の位置にある次数 d と次数 e のトロピカル曲線の交点数（重み付き）= d × e
--   交点の重み = w1 · w2 · |det(u1, u2)|（w = 辺の重み，u = 原始方向ベクトル）。ちょうど d·e になる
-- 操作：←→ = 次数 d / ↑↓ = 次数 e / R = 係数をランダムに / スペース = 曲線 2 を動かす
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
sc = 40

data World = World
  { wD, wE :: !Int         -- ★ 2 つの曲線の次数（1〜4）
  , wT1, wT2 :: !(V.Vector Term)
  , wMoving :: !Bool
  , wShift :: !Double }

-- お椀形 + ランダム → smooth になりやすい
randomTerms :: Int -> IO (V.Vector Term)
randomTerms d = V.mapM f (makeTerms d)
  where f t = do r <- randomRIO (-3, 3 :: Double)
                 pure t { tA = r + 0.3 * fromIntegral (tI t * tI t + tJ t * tJ t) }

randomize :: World -> IO World
randomize w = do
  t1 <- randomTerms (wD w)
  t2 <- randomTerms (wE w)
  pure w { wT1 = t1, wT2 = t2 }

world0 :: World
world0 = World 2 3 (makeTerms 2) (makeTerms 3) False 0

main :: IO ()
main = runSketch (sketch "38-08 Tropical Bezout" world0)
  { skBack  = bgCol
  , skStart = \_ -> randomize world0
  , skStep  = \_ _ w -> pure (if wMoving w then w { wShift = wShift w + 0.01 } else w)
  , skDraw  = draw
  , skEvent = \_ ev w -> onEvent ev w
  }

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.42 + x * sc
toY env y = envH env * 0.52 - y * sc

-- 曲線の辺を「線分」のリストにする（半直線は長さ 100 で切る）。方向ベクトルと重みつき
data Seg = Seg { x1, y1, x2, y2 :: !Double, ux, uy, sw :: !Int }

segments :: Curve -> [Seg]
segments cv = map f (cvEdges cv)
  where
    cell k = cvCells cv V.! k
    f e =
      let a = cell (eC1 e)
          (bx, by) | eC2 e >= 0 = let b = cell (eC2 e) in (cX b, cY b)
                   | otherwise = (cX a + fromIntegral (eDx e) * 100, cY a + fromIntegral (eDy e) * 100)
      in Seg (cX a) (cY a) bx by (eDx e) (eDy e) (eW e)

data Hit = Hit { hx, hy :: !Double, hm :: !Int }

-- ★ 線分どうしの交点（あれば）と重み
intersect :: Seg -> Seg -> Maybe Hit
intersect s u
  | abs den < 1e-9 = Nothing                -- 平行
  | ta < -1e-9 || ta > 1 + 1e-9 || tb < -1e-9 || tb > 1 + 1e-9 = Nothing
  | otherwise = Just (Hit (x1 s + ta * rx) (y1 s + ta * ry) (sw s * sw u * abs (ux s * uy u - uy s * ux u)))   -- 重み w1 w2 |det(u1, u2)|
  where
    rx = x2 s - x1 s
    ry = y2 s - y1 s
    qx = x2 u - x1 u
    qy = y2 u - y1 u
    den = rx * qy - ry * qx
    dx = x1 u - x1 s
    dy = y1 u - y1 s
    ta = (dx * qy - dy * qx) / den
    tb = (dx * ry - dy * rx) / den

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ seg env 1 gridC (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 gridC (0, toY env g) (envW env, toY env g) | g <- grid ]
  ++ map (segPic 35 80) s1
  ++ map (segPic 330 70) s2
  ++ concatMap hitPic pts
  ++ [ txt env 20 34 16 (gray 1) ("<- ->: d = " ++ show d ++ " (mango)   Up/Down: e = " ++ show e ++ " (pink)   R: random   Space: move curve 2 " ++ (if wMoving w then "ON" else "off"))
     , txt env 20 72 22 (hsb 90 90 100) ("Sum of weights = " ++ show total ++ " = d x e = " ++ show d ++ " x " ++ show e ++ " = " ++ show (d * e) ++ (if total == d * e then "  OK" else ""))
     , txt env 760 72 14 (hsb 90 90 100) (if total == d * e then "" else "(not general: hits a vertex)")
     , txt env 20 98 14 (gray 0.65) (show (length pts) ++ " crossing(s) (green; big dot, number = weight >= 2). Weight = w1 w2 |det(u1, u2)|.")
     , txt env 20 118 14 (gray 0.65) "Example: directions (1,0), (1,1) give |det| = 1;  (1,0), (1,2) give 2."
     , txt env 20 (envH env - 44) 14 (gray 0.65) "The usual Bezout theorem (d and e curves over C meet in d e points) still holds."
     , txt env 20 (envH env - 24) 14 (gray 0.65) "Proof idea: translate a curve far away and count only the rays at infinity." ] )
  where
    d = wD w
    e = wE w
    sh = wShift w
    t2s = V.map (\t -> t { tA = tA t + 2 * sin sh * fromIntegral (tI t - tJ t) }) (wT2 w)
    s1 = segments (tropCurve (wT1 w))
    s2 = segments (tropCurve t2s)
    -- 同じ点は 1 回だけ数える
    pts = reverse (foldl' addHit [] [h | s <- s1, u <- s2, Just h <- [intersect s u], hm h /= 0])
    addHit acc p
      | any (\q -> abs (hx q - hx p) < 1e-6 && abs (hy q - hy p) < 1e-6) acc = acc
      | otherwise = p : acc
    total = sum (map hm pts)
    grid = [fromIntegral g | g <- [-14 .. 14 :: Int]] :: [Float]
    gridC = hsba 0 0 100 0.15
    sp x y = (toX env (realToFrac x), toY env (realToFrac y))
    segPic h s sg = seg env (1.5 + fromIntegral (sw sg) * 1.5) (hsb h s 100) (sp (x1 sg) (y1 sg)) (sp (x2 sg) (y2 sg))
    hitPic p =
      let (x, y) = sp (hx p) (hy p)
      in dotP env x y (10 + fromIntegral (hm p) * 6) (hsb 90 90 100)
         : [ txtC env x y 14 (gray 0.1) (show (hm p)) | hm p > 1 ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> randomize w { wD = max 1 (wD w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> randomize w { wD = min 4 (wD w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomize w { wE = max 1 (wE w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> randomize w { wE = min 4 (wE w + 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wMoving = not (wMoving w) }
  EventKey (Char ch) Down _ _ | ch `elem` "rR" -> randomize w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. d = e = 1：2 本の直線は 1 点で交わる（03）。d = 1, e = 2 なら 2 点。実際に数えよう
-- 2. スペースで動かしているとき、交点が一瞬 d·e からずれる瞬間がある？（頂点を通るとき = 一般の位置でない）
-- 3. 重み 2 の交点が出る配置を探そう（重み 2 の辺が交わる、または方向 (1,0) と (1,2) のように斜めに交わる）
-- 4. 同じ次数の曲線を 2 本重ねると（wT2 = wT1）交点は無限個。03 のように「安定交点」を考えるとやはり d² 個


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
