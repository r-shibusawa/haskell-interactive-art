-- ============================================
-- 43-12 双対多面体（3D）
-- 面の真ん中に点を打ってつなぐと、別の多面体ができる：立方体 ↔ 正八面体、正十二面体 ↔ 正二十面体、正四面体 ↔ 正四面体
--   頂点の数と面の数が入れ替わり、辺の数は同じ。オイラーの式 V − E + F = 2 は双対でも成り立つ
--   面は「3 点を通る平面の片側に全部の頂点がある」で自動的に見つける（凸包）
-- 操作：ドラッグ = 回転 / 1〜3 = 組 / A・B = 表示切替 / D = 双対の双対 / スペース = 自動回転
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', nub, sortOn)
import qualified Data.Set as Set
import qualified Data.Vector as V

unitPx :: Float
unitPx = 150              -- ★ 1 単位 = 何 px

-- 頂点は Double の組で計算する（面の判定の許しの幅 1e-6 が効くように）
type D3 = (Double, Double, Double)

-- 多面体：頂点、面（頂点の番号を順に）、辺
data Poly = Poly { pVerts :: !(V.Vector D3), pFaces :: ![[Int]], pEdges :: ![(Int, Int)] }

data World = World
  { wNo   :: !Int
  , wA    :: !Poly         -- 元
  , wB    :: !Poly         -- 双対
  , wC    :: !Poly         -- 双対の双対
  , wRx   :: !Float
  , wRy   :: !Float
  , wSpin :: !Bool
  , wShA  :: !Bool
  , wShB  :: !Bool
  , wShC  :: !Bool
  , wLast :: !Point        -- 前のマウス位置（movedX, movedY の代わり）
  }

main :: IO ()
main = runSketch (sketch "43-12 Dual polyhedra" (makePair 1 (World 1 emptyP emptyP emptyP (-0.45) 0.6 True True True False (0, 0))))
  { skBack  = hsb 250 40 10
  , skStep  = \_ _ w -> pure (if wSpin w then w { wRy = wRy w + 0.004 } else w)
  , skDraw  = draw
  , skEvent = event
  }

emptyP :: Poly
emptyP = Poly V.empty [] []

pairName :: Int -> String
pairName k = case k of
  1 -> "cube <-> octahedron"
  2 -> "dodecahedron <-> icosahedron"
  _ -> "tetrahedron <-> tetrahedron"

-- ★ 頂点の座標（正多面体）
baseVerts :: Int -> [D3]
baseVerts k = map shrink vs
  where
    phi = (1 + sqrt 5) / 2
    cube = [(x, y, z) | x <- [-1, 1], y <- [-1, 1], z <- [-1, 1]]
    vs = case k of
      1 -> cube
      2 -> cube ++ concat [ [(0, s / phi, t * phi), (s / phi, t * phi, 0), (t * phi, 0, s / phi)] | s <- [-1, 1], t <- [-1, 1] ]
      _ -> [(1, 1, 1), (1, -1, -1), (-1, 1, -1), (-1, -1, 1)]
    r = sqrt 3
    shrink (a, b, c) = (a / r * 1.3, b / r * 1.3, c / r * 1.3)

makePair :: Int -> World -> World
makePair k w =
  let a = buildPoly (baseVerts k)
      b = dualOf a
  in w { wNo = k, wA = a, wB = b, wC = dualOf b }

-- 回転してから透視投影する（y は p5.js と同じ下向きのまま計算し、最後に上向きへ）
camD :: Env -> Float
camD env = envH env / 2 / tan (pi / 6)

rotated :: World -> D3 -> V3
rotated w (a, b, c) = rotX (wRx w) (rotY (wRy w) (V3 (f a) (f b) (f c)))
  where f v = realToFrac v * unitPx

toScreen :: Env -> V3 -> Point
toScreen env v = let (x, y) = project (camD env) v in (x, -y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- ★ 面・辺・頂点を奥から順に描くための一覧（画家のアルゴリズム）
polyItems :: Env -> World -> Poly -> Float -> Float -> Float -> [(Float, Picture)]
polyItems env w q hue faceA edgeW = faces ++ edges ++ verts
  where
    vs = V.map (rotated w) (pVerts q)
    look i = case vs V.!? i of
      Just v -> v
      Nothing -> V3 0 0 0
    light = unit3 (V3 (-0.3) (-0.5) 1)    -- 面から光へ向かう向き
    faces =
      [ (avg, Color (hsba hue 60 (100 * shade) (faceA / 100)) (Polygon (map (toScreen env) ps)))
      | f <- pFaces q
      , let ps = map look f
      , length ps >= 3
      , let avg = sum (map zOf ps) / fromIntegral (length ps)
      , let center = scale3 (1 / fromIntegral (length ps)) (foldl' add3 (V3 0 0 0) ps)
      , let n0 = case ps of
              (a : b : c : _) -> unit3 (cross3 (sub3 b a) (sub3 c a))
              _ -> V3 0 0 1
      , let n = if dot3 n0 center < 0 then scale3 (-1) n0 else n0
      , let shade = clampF 0.25 1 (60 / 255 + max 0 (dot3 n light)) ]
    edges =
      [ ((zOf a + zOf b) / 2 + 1, thickLine edgeW' (toScreen env a) (toScreen env b) `colored` hsb hue 70 100)
      | (i, j) <- pEdges q, let a = look i, let b = look j ]
    edgeW' = edgeW
    verts =
      [ (zOf v + 2, Color (hsb hue 40 100) (Translate x y (disc (5 * k))))
      | v <- V.toList vs
      , let (x, y) = toScreen env v
      , let k = camD env / max 1 (camD env - zOf v) ]
    colored p c = Color c p

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Pictures (map snd (depthSort fst items))
  , view env
      [ txt 20 34 18 (hsb 0 0 100) (pairName (wNo w) ++ " (1-3)   A: " ++ onOff (wShA w) ++ "   B: " ++ onOff (wShB w) ++ "   D: dual of dual " ++ (if wShC w then "ON (yellow)" else "off") ++ "   space: spin   drag: turn")
      , txt 20 60 14 (hsb 10 60 100) ("coral  " ++ counts (wA w))
      , txt 20 82 14 (hsb 180 60 100) ("aqua   " ++ counts (wB w) ++ "   (V and F swap, E stays)")
      , txt 20 104 14 (hsb 0 0 60) "Dual vertex = center of a face. Dual edge crosses the original edge at right angles."
      , txt 20 124 14 (hsb 0 0 60) "The dual of the dual (D) has the original shape, smaller." ]
  ]
  where
    items = concat
      ( [polyItems env w (wA w) 10 30 3 | wShA w]
     ++ [polyItems env w (wB w) 180 30 3 | wShB w]
     ++ [polyItems env w (wC w) 55 20 2 | wShC w] )

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

counts :: Poly -> String
counts q =
  let v = V.length (pVerts q)
      e = length (pEdges q)
      f = length (pFaces q)
  in "V = " ++ show v ++ "   E = " ++ show e ++ "   F = " ++ show f ++ "   V - E + F = " ++ show (v - e + f)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  -- ドラッグで回す：右へ動かすと rotY、下へ動かすと rotX が増える
  EventMotion (x, y)
    | envDown env ->
        let (lx, ly) = wLast w
        in w { wRy = wRy w + (x - lx) * 0.01, wRx = wRx w - (y - ly) * 0.01, wSpin = False, wLast = (x, y) }
    | otherwise -> w { wLast = (x, y) }
  EventKey (MouseButton LeftButton) Down _ p -> w { wLast = p }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpin = not (wSpin w) }
  EventKey (Char k) Down _ _
    | k `elem` "123" -> makePair (fromEnum k - fromEnum '0') w
    | k `elem` "aA" -> w { wShA = not (wShA w) }
    | k `elem` "bB" -> w { wShB = not (wShB w) }
    | k `elem` "dD" -> w { wShC = not (wShC w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 立方体の面 6 個の中心をつなぐと正八面体（頂点 6）。正八面体の面 8 個の中心をつなぐと立方体（頂点 8）
-- 2. 正四面体は自分自身と双対（自己双対）。双対の双対は向きが元に戻る？ 大きさは？
-- 3. baseVerts に自分の凸多面体（例：三角柱の 6 点）を足す。双対は何角錐？（n 角柱 ↔ n 角両錐）
-- 4. 辺の数が変わらない理由を考えよう：元の辺 1 本 ↔ 隣り合う 2 面 ↔ 双対の辺 1 本

-- ---------- 多面体の道具（面の検出・双対の計算。読まなくても OK） ----------
subD, crossD :: D3 -> D3 -> D3
subD (a, b, c) (d, e, f) = (a - d, b - e, c - f)
crossD (a, b, c) (d, e, f) = (b * f - c * e, c * d - a * f, a * e - b * d)

dotD :: D3 -> D3 -> Double
dotD (a, b, c) (d, e, f) = a * d + b * e + c * f

normD :: D3 -> Double
normD v = sqrt (dotD v v)

scaleD :: Double -> D3 -> D3
scaleD k (a, b, c) = (k * a, k * b, k * c)

-- 頂点の配列から面（凸包）と辺を作る
buildPoly :: [D3] -> Poly
buildPoly vs = Poly vv faces edges
  where
    vv = V.fromList vs
    n = V.length vv
    eps = 1e-6
    numbered = zip [0 :: Int ..] vs
    faces = reverse (snd (foldl' tryFace (Set.empty, []) triples))
    triples = [(i, j, k) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], k <- [j + 1 .. n - 1]]
    tryFace acc@(seen, out) (i, j, k) =
      let vi = vv V.! i
          cr = crossD (subD (vv V.! j) vi) (subD (vv V.! k) vi)
          l = normD cr
          nm0 = scaleD (1 / l) cr
          d0 = dotD nm0 vi
          sides = [(m, dotD nm0 v - d0) | (m, v) <- numbered]
          pos = length [() | (_, s) <- sides, s > eps]
          neg = length [() | (_, s) <- sides, s < -eps]
          on = [m | (m, s) <- sides, abs s <= eps]
          nm = if pos > 0 then scaleD (-1) nm0 else nm0     -- 外向きにそろえる
      in if l < eps || (pos > 0 && neg > 0) || Set.member on seen
           then acc
           else (Set.insert on seen, orderFace nm on : out)
    -- 面の頂点を法線のまわりの角度で並べる
    orderFace nm on =
      let pts = [vv V.! m | m <- on]
          c = centroid pts
          u0 = subD (firstOr c pts) c
          u = scaleD (1 / max 1e-9 (normD u0)) u0
          w = crossD nm u
          ang m = let d = subD (vv V.! m) c in atan2 (dotD d w) (dotD d u)
      in sortOn ang on
    firstOr d xs = case xs of
      (x : _) -> x
      _ -> d
    edges = nub
      [ (min a b, max a b)
      | f <- faces, (a, b) <- zip f (drop 1 f ++ take 1 f) ]

centroid :: [D3] -> D3
centroid ps =
  let (a, b, c) = foldl' (\(x, y, z) (p, q, r) -> (x + p, y + q, z + r)) (0, 0, 0) ps
      k = fromIntegral (max 1 (length ps))
  in (a / k, b / k, c / k)

-- 辺の中点までの距離の平均
midR :: Poly -> Double
midR q =
  let es = pEdges q
      one (a, b) = let (x1, y1, z1) = pVerts q V.! a
                       (x2, y2, z2) = pVerts q V.! b
                   in normD ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)
  in sum (map one es) / fromIntegral (max 1 (length es))

-- ★ 双対：各面の中心を頂点にする。辺の中点の半径がそろうように大きさを合わせる
dualOf :: Poly -> Poly
dualOf q =
  let centers = [centroid [pVerts q V.! m | m <- f] | f <- pFaces q]
      d = buildPoly centers
      r = midR d
      k = midR q / (if r == 0 then 1 else r)
  in buildPoly (map (scaleD k) centers)

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))
