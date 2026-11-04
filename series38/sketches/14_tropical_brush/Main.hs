-- ============================================
-- 38-14 トロピカル曲線ブラシ
-- マウスの軌跡の点を「係数（高さ）」にして、次数 d のトロピカル曲線をリアルタイムに計算し直す
--   各領域（どの項が最小か）を色分けし、曲線を鉛の線にした「ステンドグラス」。軌跡が変わると曲線が動く
--   自由制作のベース：高さの決め方（heightOf）・色（hueOf）・次数・線の太さを変えよう
-- 操作：ドラッグ = 描く / ↑↓ = 次数 d / 1〜3 = 配色 / S = 画像を保存（SVG）/ C = クリア
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

sc, scale4 :: Float
sc = 60
scale4 = 5

data World = World
  { wD :: !Int             -- ★ 次数（2〜6）
  , wTrail :: [Point]      -- マウスの軌跡（新しい順、最大 60 点）
  , wPalette :: !Int
  , wImg :: Picture }      -- 色分けの画像（軌跡・次数・配色が変わったときだけ作り直す）

termCount :: Int -> Int
termCount d = (d + 1) * (d + 2) `div` 2

randomTrail :: Env -> Int -> IO [Point]
randomTrail env k = replicateM k $ do
  a <- randomRIO (0, 1)
  b <- randomRIO (0, 1)
  pure (envW env * (0.3 + 0.4 * a), envH env * (0.3 + 0.4 * b))

main :: IO ()
main = runSketch (sketch "38-14 Tropical Curve Brush" (World 4 [] 1 Blank))
  { skBack  = bgCol
  , skStart = \env -> do
      tr <- randomTrail env (termCount 4)
      pure (repaint env (World 4 tr 1 Blank))
  , skDraw  = draw
  , skEvent = onEvent
  }

ox, oy :: Env -> Float
ox env = envW env * 0.5
oy env = envH env * 0.5

toX, toY, fromX, fromY :: Env -> Float -> Float
toX env x = ox env + x * sc
toY env y = oy env - y * sc
fromX env s = (s - ox env) / sc
fromY env s = (oy env - s) / sc

-- ★ 軌跡の点（画面座標）→ 項 (i, j) の高さ。縦位置で全体の高さ、横位置で x–y の偏り
heightOf :: Env -> Point -> Term -> Double
heightOf env (px, py) t =
  let i = fromIntegral (tI t) :: Float
      j = fromIntegral (tJ t) :: Float
  in realToFrac ((py / envH env) * 4 + (px / envW env - 0.5) * 2 * (i - j) + 0.15 * (i * i + j * j))

-- ★ 項 k の色（配色 1〜3）
hueOf :: Int -> Int -> Term -> Float
hueOf palette k t = case palette of
  1 -> fromIntegral ((k * 47 + 20) `mod` 360)
  2 -> 20 + fromIntegral (tI t + tJ t) * 25
  _ -> fromIntegral ((tI t * 90 + tJ t * 150) `mod` 360)

-- 0..360, 0..1, 0..1 → (r, g, b)（0〜255）
hsb2rgb :: Float -> Float -> Float -> (Float, Float, Float)
hsb2rgb h s v =
  let c = v * s
      x = c * (1 - abs (wrapF 2 (h / 60) - 1))
      m = v - c
      (r, g, b) | h < 60 = (c, x, 0)
                | h < 120 = (x, c, 0)
                | h < 180 = (0, c, x)
                | h < 240 = (0, x, c)
                | h < 300 = (x, 0, c)
                | otherwise = (c, 0, x)
  in ((r + m) * 255, (g + m) * 255, (b + m) * 255)

-- 画像を作り直す
repaint :: Env -> World -> World
repaint env w = w { wImg = stained env w (currentTerms env w) }

-- 軌跡 → 高さ：新しい点から順に項へ割り当てる（足りなければいちばん古い点）
currentTerms :: Env -> World -> V.Vector Term
currentTerms env w =
  let tr = wTrail w
      oldest = case reverse tr of
        (p : _) -> p
        [] -> (envW env / 2, envH env / 2)
      ps = V.fromList (take (termCount (wD w)) (tr ++ repeat oldest))
  in V.zipWith (\p t -> t { tA = heightOf env p t }) ps (makeTerms (wD w))

-- 領域を塗る：粗い画像で各ピクセルの最小の項を探す。1 位と 2 位の差が小さいところは暗く（鉛の線のにじみ）
stained :: Env -> World -> V.Vector Term -> Picture
stained env w ts = pixelImage bw bh scale4 pixel
  where
    bw = ceiling (envW env / scale4)
    bh = ceiling (envH env / scale4)
    n = V.length ts
    as = U.generate n (\k -> realToFrac (tA (ts V.! k))) :: U.Vector Float
    is = U.generate n (\k -> fromIntegral (tI (ts V.! k))) :: U.Vector Float
    js = U.generate n (\k -> fromIntegral (tJ (ts V.! k))) :: U.Vector Float
    cols = U.generate n (\k -> hsb2rgb (hueOf (wPalette w) k (ts V.! k)) 0.6 0.95) :: U.Vector (Float, Float, Float)
    pixel qx py =
      let x = fromX env (fromIntegral qx * scale4)
          y = fromY env (fromIntegral py * scale4)
          go :: Int -> Float -> Float -> Int -> (Float, Float, Int)
          go !k !m1 !m2 !best
            | k >= n = (m1, m2, best)
            | v < m1 = go (k + 1) v m1 k
            | v < m2 = go (k + 1) m1 v best
            | otherwise = go (k + 1) m1 m2 best
            where v = U.unsafeIndex as k + U.unsafeIndex is k * x + U.unsafeIndex js k * y
          (a1, a2, b) = go 0 (1 / 0) (1 / 0) 0
          sh = 0.45 + 0.55 * min 1 ((a2 - a1) / 0.8)
          (r, g, bl) = U.unsafeIndex cols b
      in (truncate (r * sh), truncate (g * sh), truncate (bl * sh))

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ wImg w ]
  -- 鉛の線（曲線の辺）
  ++ [ let (a, b) = edgeEnds cv e in seg env (4 + fromIntegral (eW e) * 3) lead (us a) (us b) | e <- cvEdges cv ]
  ++ [ let (x, y) = us (cellXY c) in dotP env x y 10 lead | c <- V.toList (cvCells cv) ]
  -- 軌跡
  ++ [ Color (hsba 0 0 100 0.5) (thickPath 1.5 [ pt env x y | (x, y) <- wTrail w ]) ]
  ++ [ box env 0 0 (envW env) 72 (rgba255 0 0 0 130)
     , txt env 20 30 16 (gray 1) ("Drag: draw   Up/Down: degree d = " ++ show (wD w) ++ " (" ++ show (V.length ts) ++ " terms)   1-3: colours   S: save   C: clear")
     , txt env 20 56 14 (gray 0.85) ("Latest " ++ show (V.length ts) ++ " trail points = coefficients. Region = lowest term. Lead lines = tropical curve (" ++ show (V.length (cvCells cv)) ++ " vertices)") ] )
  where
    ts = currentTerms env w
    cv = tropCurve ts
    lead = hsb 0 0 8
    us (x, y) = (toX env x, toY env y)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion _ | envDown env ->
    let m = mouseP env
    in pure (case wTrail w of
         (l : _) | dist2 m l <= 12 -> w
         tr -> repaint env w { wTrail = take 60 (m : tr) })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (repaint env w { wD = min 6 (wD w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (repaint env w { wD = max 2 (wD w - 1) })
  EventKey (Char '1') Down _ _ -> pure (repaint env w { wPalette = 1 })
  EventKey (Char '2') Down _ _ -> pure (repaint env w { wPalette = 2 })
  EventKey (Char '3') Down _ _ -> pure (repaint env w { wPalette = 3 })
  EventKey (Char ch) Down _ _
    | ch `elem` "sS" -> do
        saveSvg "tropical_brush" env (draw env w)
        pure w
    | ch `elem` "cC" -> do
        tr <- randomTrail env (termCount (wD w))
        pure (repaint env w { wTrail = tr })
  EventResize _ -> pure (repaint env w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. heightOf を変える：py だけにすると？ 距離（点と画面中央）にすると？ 曲線の動きがどう変わる？
-- 2. 鉛の線の太さを重み w に比例させている。太さを一定にする／頂点に丸をつけない、で印象を比べよう
-- 3. 0.8（明暗の差のしきい値）を 3 にすると領域の中がグラデーションになる。「谷」が見える
-- 4. 軌跡をファイルに書き出して、同じ絵を再現できるようにしよう。S で保存した画像と比べる


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
