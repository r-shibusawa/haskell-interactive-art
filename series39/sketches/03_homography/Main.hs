-- ============================================
-- 39-03 ホモグラフィ（射影変換）
-- 正方形の 4 頂点を好きな 4 点に写す 3×3 行列 H を、8 本の連立方程式から求める（道具の homographyFrom4）
--   点 [x, y, 1] → H [x, y, 1] = [x', y', w']、画面では (x'/w', y'/w')。直線は直線のまま、平行は保たれない
--   A でアフィン変換に制限：H の 3 行目が [0, 0, 1] になり、平行線は平行のまま（平行四辺形）
-- 4 つの角をドラッグ / A = アフィンに制限 / G = 格子の数 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

src :: [Point]
src = [(0, 0), (1, 0), (1, 1), (0, 1)]   -- 元の正方形

nGrid0 :: Int
nGrid0 = 8                               -- ★ チェッカーのマス目の数

data World = World
  { wDst    :: ![Point]   -- 4 つの角（画面座標）
  , wDrag   :: !Int
  , wAffine :: !Bool
  , wGrid   :: !Int
  }

main :: IO ()
main = runSketch (sketch "39-03 Homography" (World [] (-1) False nGrid0))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env (World [] (-1) False nGrid0))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> World -> World
resetAll env w =
  let s = min (envW env) (envH env) * 0.5
      cx = envW env * 0.42
      cy = envH env * 0.52
  in w { wDst = [(cx - s / 2, cy - s / 2), (cx + s / 2, cy - s / 2), (cx + s / 2, cy + s / 2), (cx - s / 2, cy + s / 2)] }

-- 対角の点を平行四辺形になる位置に置く
opposite :: Int -> [Point] -> [Point]
opposite k ds = case map (\i -> ds !! (i `mod` 4)) [k, k + 1, k + 3] of
  [(ax, ay), (bx, by), (cx, cy)] -> setAt ((k + 2) `mod` 4) (bx + cx - ax, by + cy - ay) ds
  _ -> ds

step :: Env -> World -> World
step env w
  | wDrag w < 0 = w
  | otherwise =
      let ds = setAt (wDrag w) (mouseP5 env) (wDst w)
      in w { wDst = if wAffine w then opposite (wDrag w) ds else ds }

-- 絵：顔（円と弧）と矢印。0〜1 の座標で作っておく
figure :: [[Point]]
figure =
  [ arc 0.5 0.5 0.32 0 (pi * 2) 48
  , arc 0.38 0.42 0.04 0 (pi * 2) 12, arc 0.62 0.42 0.04 0 (pi * 2) 12
  , arc 0.5 0.5 0.2 (pi * 0.15) (pi * 0.85) 16
  , [(0.1, 0.9), (0.3, 0.9), (0.25, 0.85), (0.3, 0.9), (0.25, 0.95)] ]
  where
    arc :: Float -> Float -> Float -> Float -> Float -> Int -> [Point]
    arc cx cy r a0 a1 n =
      [ (cx + r * cos a, cy + r * sin a)
      | i <- [0 .. n], let a = a0 + (a1 - a0) * fromIntegral i / fromIntegral n ]

draw :: Env -> World -> Picture
draw env w = case homographyFrom4 src (wDst w) of
  Nothing -> view env [txt 20 40 18 white "The 4 points are degenerate (press R to reset)"]
  Just h ->
    let n = wGrid w
        fn = fromIntegral n :: Float
        -- 点 (x, y) を H で写して [x', y', w'] を返す（w' の符号も見たいので割らない）
        mapH (x, y) = mat3Vec h (hPt (x, y))
        ok (H3 _ _ c) = c > 1e-6
        flat (H3 a b c) = (d2f (a / c), d2f (b / c))
        -- チェッカーボード：マスの 4 角を写して四角形で描く（w' > 0 のときだけ）
        cells =
          [ fillQuad (checker i j 0.7) (map flat c)
          | i <- [0 .. n - 1], j <- [0 .. n - 1]
          , let fi = fromIntegral i / fn
                fj = fromIntegral j / fn
                d = 1 / fn
                c = map mapH [(fi, fj), (fi + d, fj), (fi + d, fj + d), (fi, fj + d)]
          , all ok c ]
        -- 格子線（直線は直線に写る：両端を写すだけでよい）
        gridC = hsba 0 0 100 0.35
        mapped a b =
          let pa = mapH a
              pb = mapH b
          in if ok pa && ok pb then [seg 1 gridC (flat pa) (flat pb)] else sliced a b
        sliced (x0, y0) (x1, y1) =
          let ps = [ if ok q then Just (flat q) else Nothing
                   | i <- [0 .. 24 :: Int], let t = fromIntegral i / 24
                   , let q = mapH (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t) ]
          in [seg 1 gridC a b | (Just a, Just b) <- zip ps (drop 1 ps)]
        gridLines = concat [ mapped (k, 0) (k, 1) ++ mapped (0, k) (1, k) | i <- [0 .. n], let k = fromIntegral i / fn ]
        -- 絵
        pics = [ Color (hsb 15 80 100) (thickPath 3 [flat q | p <- ln, let q = mapH p, ok q]) | ln <- figure ]
        -- 角
        corners = [ Pictures [ dotAt (hsb 45 90 100) d 16, txt (fst d + 10) (snd d - 10) 15 white name ]
                  | (d, name) <- zip (wDst w) ["(0, 0)", "(1, 0)", "(1, 1)", "(0, 1)"] ]
    in view env $ cells ++ gridLines ++ pics ++ corners ++
         [ matrixPanel env w h
         , sourcePanel env w h
         , txt 20 28 18 white ("Drag corners   A: affine only " ++ (if wAffine w then "ON (parallelogram)" else "off") ++ "   G: cells " ++ show n ++ "   R: reset")
         , txt 20 50 14 (gray 0.65) "Lines stay lines, but parallels now meet (far cells shrink)."
         , txt 20 70 14 (gray 0.65) "Lower right: the mouse sent back through the inverse of H gives the source coordinates."
         ]

checker :: Int -> Int -> Float -> Color
checker i j a = if odd (i + j) then hsba 45 40 80 a else hsba 200 40 45 a

-- 行列の数値
matrixPanel :: Env -> World -> M3 -> Picture
matrixPanel env w (M3 r0 r1 r2) = Pictures $
  [ fillRect (hsba 0 0 100 0.08) (px - 16) (py - 30) 390 165
  , txt px (py + 30) 16 white "H =" ]
  ++ concat [ [ txt (px + 40 + fromIntegral c * 108) (py + fromIntegral r * 26) 15 col (nf d (d2f v))
              | (c, v) <- zip [0 :: Int ..] (vals row) ]
            | (r, row, d) <- [(0 :: Int, r0, 2), (1, r1, 2), (2, r2, 5)]
            , let col = if r == 2 && wAffine w then hsb 45 90 100 else white ]
  ++ [ txt px (py + 100) 14 (gray 0.7) "Square 0..1 -> screen px"
     , txt px (py + 120) 14 (gray 0.7) "Row 3 = [0, 0, 1]: affine, parallels stay" ]
  where
    px = envW env - 400
    py = 110
    vals (H3 a b c) = [a, b, c]

-- 右下：元の正方形とマウスの対応点
sourcePanel :: Env -> World -> M3 -> Picture
sourcePanel env w h = Pictures $
  [ fillRect (hsba 0 0 100 0.08) (px - 10) (py - 10) (s + 20) (s + 20) ]
  ++ [ fillRect (checker i j 0.5) (px + fromIntegral i * s / fn) (py + fromIntegral j * s / fn) (s / fn) (s / fn)
     | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  ++ [ Color (hsb 15 80 100) (thickPath 2 [(px + x * s, py + y * s) | (x, y) <- ln]) | ln <- figure ]
  ++ case mat3Inv h >>= \inv -> applyH inv mouse of
       Just (qx, qy) | qx >= 0 && qx <= 1 && qy >= 0 && qy <= 1 ->
         [ dotAt (hsb 120 80 100) (px + qx * s, py + qy * s) 10
         , txt px (py - 16) 14 (hsb 120 80 100) ("(" ++ nf 2 qx ++ ", " ++ nf 2 qy ++ ")")
         , ringAt (hsb 120 80 100) mouse 14 1 ]
       _ -> []
  where
    s = 160
    px = envW env - 200
    py = envH env - 200
    n = wGrid w
    fn = fromIntegral n :: Float
    mouse = mouseP5 env

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = nearestHandle (wDst w) (mouseP5 env) 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "aA" ->
        let on = not (wAffine w)
        in w { wAffine = on, wDst = if on then opposite 0 (wDst w) else wDst w }
    | c `elem` "gG" -> w { wGrid = if wGrid w >= 16 then 4 else wGrid w * 2 }
    | c `elem` "rR" -> resetAll env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 上の 2 角を近づけて「地面を見下ろした格子」を作る。00 の透視図と同じ形になるか
-- 2. 角を 1 つだけ内側に動かして、3 行目の数値（h31, h32）がどう変わるか見る。0 から離れるほど「遠近感」が強い
-- 3. 顔の絵を自分の絵（figure の線のリスト）に変えて、ゆがませてみよう
-- 4. 2 つのホモグラフィを続けて使うと？ H2 × H1（mat3Mul）も 1 つのホモグラフィになることを確かめよう

-- ==================================================
-- ここから下は「道具」：p5.js と同じ座標（左上が原点、y は下向き）で描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- 図全体を「左上が原点、y は下向き」の座標に置く
view :: Env -> [Picture] -> Picture
view env ps = Translate (negate (envW env) / 2) (envH env / 2) (Scale 1 (-1) (Pictures ps))

-- gloss の座標 → p5.js の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = toP5 env (envMouse env)

-- 文字（(x, y) が左下。y が下向きなので文字だけ裏返して戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 塗りつぶした丸（d は直径。p5.js の circle と同じ）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 輪（d は直径、w は線の太さ）
ringAt :: Color -> Point -> Float -> Float -> Picture
ringAt c p d w
  | w <= 1 = Color c (at p (Circle (d / 2)))
  | otherwise = Color c (at p (ring (d / 2) w))

-- 線分（太さ 1 以下は細い線）
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b
  | w <= 0 = Blank
  | w <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine w a b)

-- 閉じた折れ線
closedPath :: Float -> Color -> [Point] -> Picture
closedPath _ _ [] = Blank
closedPath w c ps@(p : rest) = Pictures (zipWith (seg w c) ps (rest ++ [p]))

-- 左上 (x, y)、幅 w、高さ h の長方形
fillRect :: Color -> Float -> Float -> Float -> Float -> Picture
fillRect c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

strokeRect :: Color -> Float -> Float -> Float -> Float -> Picture
strokeRect c x y w h = closedPath 1 c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 四角形を塗る。gloss の Polygon はへこんだ形を正しく塗れないので、
-- へこみ・ねじれ（8 の字）のときは三角形に分ける
fillQuad :: Color -> [Point] -> Picture
fillQuad col [a, b, c, d]
  | sameSign [o a b c, o b c d, o c d a, o d a b] = Color col (Polygon [a, b, c, d])
  | o a b c * o a c d > 0 = tris [[a, b, c], [a, c, d]]
  | o b c d * o b d a > 0 = tris [[b, c, d], [b, d, a]]
  | otherwise = case (crossAt a b c d, crossAt b c d a) of
      (Just x, _) -> tris [[x, b, c], [x, d, a]]
      (_, Just x) -> tris [[a, b, x], [c, d, x]]
      _ -> Blank
  where
    o (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    sameSign vs = all (> 0) vs || all (< 0) vs
    tris ts = Color col (Pictures (map Polygon ts))
    -- 線分 pq と線分 rs の交点
    crossAt (px, py) (qx, qy) (rx, ry) (sx, sy) =
      let dx1 = qx - px; dy1 = qy - py; dx2 = sx - rx; dy2 = sy - ry
          den = dx1 * dy2 - dy1 * dx2
          t = ((rx - px) * dy2 - (ry - py) * dx2) / den
          u = ((rx - px) * dy1 - (ry - py) * dx1) / den
      in if abs den < 1e-6 || t < 0 || t > 1 || u < 0 || u > 1 then Nothing
         else Just (px + t * dx1, py + t * dy1)
fillQuad _ _ = Blank

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 小数 d 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf d v
  | isNaN v || isInfinite v = "-"
  | d <= 0 = show (round v :: Int)
  | otherwise = showFFloat (Just d) v ""

-- ==================================================
-- ここから下は「射影幾何の道具」（同次座標・外積・行列・ホモグラフィ）
-- 小さな差（行列式 ≈ 0 など）を見るので、計算は Double で行う
-- ==================================================

-- 同次座標の点 [x, y, w]、または直線 [a, b, c]（ax + by + cw = 0）
data H3 = H3 !Double !Double !Double

f2d :: Float -> Double
f2d = realToFrac

d2f :: Double -> Float
d2f = realToFrac

-- w = 1 のふつうの点
hPt :: Point -> H3
hPt (x, y) = H3 (f2d x) (f2d y) 1

-- 外積。2 点 → それを通る直線 ／ 2 直線 → その交点。どちらも同じ式！
hCross :: H3 -> H3 -> H3
hCross (H3 a b c) (H3 d e f) = H3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

-- 内積。点と直線で 0 なら「点が直線の上にある」
hDot :: H3 -> H3 -> Double
hDot (H3 a b c) (H3 d e f) = a * d + b * e + c * f

-- 3 つの点（または直線）の行列式。0 なら共線（共点）
hDet :: H3 -> H3 -> H3 -> Double
hDet p q r = hDot p (hCross q r)

-- 長さを 1 にそろえる（表示用。射影的には同じ点・同じ直線）
hUnit :: H3 -> H3
hUnit p@(H3 a b c) =
  let n = sqrt (hDot p p)
      m = if n == 0 then 1 else n
  in H3 (a / m) (b / m) (c / m)

-- 無限遠（または画面からはるか遠く）にあるか
isFar :: H3 -> Bool
isFar (H3 x y w) = abs w * 1e5 < sqrt (x * x + y * y)

-- 同次座標 → 画面の点（無限遠なら Nothing）
toXY :: H3 -> Maybe Point
toXY p@(H3 x y w)
  | isFar p || w == 0 = Nothing
  | otherwise = Just (d2f (x / w), d2f (y / w))

-- 2 点を通る直線
lineThrough :: Point -> Point -> H3
lineThrough a b = hCross (hPt a) (hPt b)

-- 1 直線上の 4 点の複比 (P1,P2;P3,P4)。a, b はその直線上の基準 2 点（無限遠点があっても計算できる）
crossRatioH :: H3 -> H3 -> H3 -> H3 -> H3 -> H3 -> Maybe Double
crossRatioH p1 p2 p3 p4 a b =
  let n = hCross a b
      nn = let v = hDot n n in if v == 0 then 1 else v
      lm p = (hDot (hCross p b) n / nn, hDot (hCross a p) n / nn)
      (l1, m1) = lm p1
      (l2, m2) = lm p2
      (l3, m3) = lm p3
      (l4, m4) = lm p4
      num = (l1 * m3 - l3 * m1) * (l2 * m4 - l4 * m2)
      den = (l2 * m3 - l3 * m2) * (l1 * m4 - l4 * m1)
  in if abs den < 1e-12 then Nothing else Just (num / den)

-- 枠（左, 上, 右, 下）
type Frame = (Float, Float, Float, Float)

screenBox :: Env -> Frame
screenBox env = (0, 0, envW env, envH env)

-- 無限遠点の矢印を置く枠（上の説明文と下の文字にかからないようにする）
arrowBox :: Env -> Frame
arrowBox env = (0, 90, envW env, envH env - 30)

-- 直線 [a, b, c] が枠を横切る両端
hLineEnds :: Frame -> H3 -> Maybe (Point, Point)
hLineEnds (x0, y0, x1, y1) l =
  let edges = [H3 1 0 (f2d (-x0)), H3 1 0 (f2d (-x1)), H3 0 1 (f2d (-y0)), H3 0 1 (f2d (-y1))]
      ok (x, y) = x > x0 - 1 && x < x1 + 1 && y > y0 - 1 && y < y1 + 1
      cand = [p | e <- edges, Just p <- [toXY (hCross l e)], ok p]
      pairs = [(p, q) | (i, p) <- zip [0 :: Int ..] cand, (j, q) <- zip [0 ..] cand, i < j]
  in case pairs of
       [] -> Nothing
       _ -> Just (maximumBy (comparing (uncurry dist2)) pairs)

-- 直線を枠いっぱいに描く
hLine :: Float -> Color -> Frame -> H3 -> Picture
hLine w c box l = maybe Blank (uncurry (seg w c)) (hLineEnds box l)

-- 点を描く。ふつうの点なら丸、無限遠点なら枠のふちに「方向の矢印」（向かい合う 2 か所 = 同じ 1 点）
hPoint :: Frame -> Float -> String -> Color -> Color -> Float -> H3 -> Picture
hPoint (x0, y0, x1, y1) r lab fillC strokeC sw p@(H3 hx hy _) = case toXY p of
  Just q@(qx, qy) -> Pictures
    [ dotAt fillC q r
    , if sw > 0 then ringAt strokeC q r sw else Blank
    , if null lab then Blank else txt (qx + r * 0.7) (qy - r * 0.7) 15 strokeC lab ]
  Nothing
    | m == 0 -> Blank
    | otherwise -> Pictures (concatMap arrowAt [1, -1])
  where
    m = sqrt (hx * hx + hy * hy)
    dx = d2f (hx / m)
    dy = d2f (hy / m)
    cx = (x0 + x1) / 2
    cy = (y0 + y1) / 2
    arrowAt s =
      let ux = dx * s
          uy = dy * s
          ts = [(x1 - 30 - cx) / ux | ux > 1e-9] ++ [(x0 + 30 - cx) / ux | ux < -1e-9]
            ++ [(y1 - 30 - cy) / uy | uy > 1e-9] ++ [(y0 + 30 - cy) / uy | uy < -1e-9]
          t = foldr min 1e6 ts
          x = cx + ux * t
          y = cy + uy * t
          rot (u, v) = (x + u * ux - v * uy, y + u * uy + v * ux)
          tip = rot (r, 0)
      in [ seg sw strokeC (rot (-r * 2.5, 0)) tip
         , seg sw strokeC tip (rot (0, -r * 0.8))
         , seg sw strokeC tip (rot (0, r * 0.8))
         , if null lab then Blank else txt (x - r * 2) (y - r) 15 strokeC (lab ++ " inf") ]

-- 3×3 行列（行を 3 つ並べたもの）の道具
data M3 = M3 !H3 !H3 !H3

mat3Vec :: M3 -> H3 -> H3
mat3Vec (M3 a b c) v = H3 (hDot a v) (hDot b v) (hDot c v)

mat3T :: M3 -> M3
mat3T (M3 (H3 a b c) (H3 d e f) (H3 g h i)) = M3 (H3 a d g) (H3 b e h) (H3 c f i)

mat3Mul :: M3 -> M3 -> M3
mat3Mul (M3 a b c) m =
  let M3 p q r = mat3T m
      row v = H3 (hDot v p) (hDot v q) (hDot v r)
  in M3 (row a) (row b) (row c)

mat3Inv :: M3 -> Maybe M3
mat3Inv (M3 (H3 a b c) (H3 d e f) (H3 g h i)) =
  let ca = e * i - f * h
      cb = negate (d * i - f * g)
      cc = d * h - e * g
      det = a * ca + b * cb + c * cc
  in if abs det < 1e-12 then Nothing else Just $ M3
       (H3 (ca / det) (negate (b * i - c * h) / det) ((b * f - c * e) / det))
       (H3 (cb / det) ((a * i - c * g) / det) (negate (a * f - c * d) / det))
       (H3 (cc / det) (negate (a * h - b * g) / det) ((a * e - b * d) / det))

-- ガウスの消去法（部分ピボット）で A x = b を解く。解けなければ Nothing
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a b = go 0 (zipWith (\r v -> r ++ [v]) a b)
  where
    n = length b
    go c m
      | c >= n = Just (zipWith (\i r -> (r !! n) / (r !! i)) [0 ..] m)
      | abs (m !! p !! c) < 1e-12 = Nothing
      | otherwise = go (c + 1) [if i == c then pivot else elim i r | (i, r) <- zip [0 ..] swapped]
      where
        p = maximumBy (comparing (\r -> abs (m !! r !! c))) [c .. n - 1]
        swapped = setAt c (m !! p) (setAt p (m !! c) m)
        pivot = swapped !! c
        elim i r =
          let f = (r !! c) / (pivot !! c)
          in zipWith3 (\k x y -> if k < c then x else x - f * y) [0 :: Int ..] r pivot

-- 4 点の対応 src[i] → dst[i] からホモグラフィ 3×3 を求める（h33 = 1 に固定、8 本の連立方程式）
homographyFrom4 :: [Point] -> [Point] -> Maybe M3
homographyFrom4 src dst =
  let rows = concat (zipWith eqs (map dbl src) (map dbl dst))
      dbl (x, y) = (f2d x, f2d y)
      eqs (x, y) (u, v) =
        [ ([x, y, 1, 0, 0, 0, -u * x, -u * y], u)
        , ([0, 0, 0, x, y, 1, -v * x, -v * y], v) ]
  in case solveLinear (map fst rows) (map snd rows) of
       Just [h0, h1, h2, h3, h4, h5, h6, h7] -> Just (M3 (H3 h0 h1 h2) (H3 h3 h4 h5) (H3 h6 h7 1))
       _ -> Nothing

-- ホモグラフィで点を写す（無限遠へ飛ぶなら Nothing）
applyH :: M3 -> Point -> Maybe Point
applyH h p = toXY (mat3Vec h (hPt p))

-- つかめる点のリストからマウスに近いものの番号。半径 r 以内になければ -1
nearestHandle :: [Point] -> Point -> Float -> Int
nearestHandle ps m r = fst (foldl pick (-1, r) (zip [0 ..] ps))
  where pick (bi, bd) (i, p) = let d = dist2 p m in if d < bd then (i, d) else (bi, bd)
