-- ============================================
-- 39-04 複比の不変性
-- 直線 l 上の 4 点 A, B, C, D を、点 O から別の直線 m に射影して A', B', C', D' を作る
--   長さ AC や比 AC/BC は変わるのに、複比 (A,B;C,D) = (AC·BD)/(BC·AD) だけは同じ値のまま ─ 射影変換の不変量
-- 点 A〜D・O・直線の端をドラッグ / P = m を l と平行に / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

tt0 :: [Float]
tt0 = [0.15, 0.4, 0.55, 0.85]   -- ★ A, B, C, D の l 上の位置（0〜1）

data World = World
  { wLa, wLb :: !Point   -- 直線 l の端（ドラッグ可）
  , wMa, wMb :: !Point   -- 直線 m の端
  , wO   :: !Point       -- 射影の中心
  , wTT  :: ![Float]
  , wDrag :: !Int
  }

main :: IO ()
main = runSketch (sketch "39-04 Cross-ratio is invariant" (blank0 (-1)))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

blank0 :: Int -> World
blank0 = World (0, 0) (1, 0) (0, 1) (1, 1) (0, 0) tt0

resetAll :: Env -> World
resetAll env =
  let sw = envW env
      sh = envH env
  in World (sw * 0.12, sh * 0.78) (sw * 0.88, sh * 0.78) (sw * 0.2, sh * 0.45) (sw * 0.8, sh * 0.25) (sw * 0.5, sh * 0.12) tt0 (-1)

onL :: World -> Float -> Point
onL w t = let (ax, ay) = wLa w; (bx, by) = wLb w in (ax + (bx - ax) * t, ay + (by - ay) * t)

handles :: World -> [Point]
handles w = map (onL w) (wTT w) ++ [wO w, wLa w, wLb w, wMa w, wMb w]

step :: Env -> World -> World
step env w
  | i < 0 = w
  | i < 4 =   -- A〜D は直線 l の上を動く（マウスを l に射影）
      let (ax, ay) = wLa w
          (bx, by) = wLb w
          vx = bx - ax
          vy = by - ay
          l2 = let v = vx * vx + vy * vy in if v == 0 then 1 else v
      in w { wTT = setAt i (clampF (-0.3) 1.3 (((mx - ax) * vx + (my - ay) * vy) / l2)) (wTT w) }
  | i == 4 = w { wO = m }
  | i == 5 = w { wLa = m }
  | i == 6 = w { wLb = m }
  | i == 7 = w { wMa = m }
  | otherwise = w { wMb = m }
  where
    i = wDrag w
    m@(mx, my) = mouseP5 env

draw :: Env -> World -> Picture
draw env w = view env $
  -- 直線
  [ hLine 2 (hsba 200 60 95 0.7) full l
  , hLine 2 (hsba 15 75 95 0.7) full m ]
  -- 射影の線
  ++ [ seg 1 (hsba 45 80 100 0.4) o (px + (px - ox) * 3, py + (py - oy) * 3) | (px, py) <- ps ]
  -- 点
  ++ [ Pictures [dotAt (hsb 200 60 100) p 14, txt (fst p - 5) (snd p + 28) 16 white n] | (p, n) <- zip ps names ]
  ++ [ hPoint (arrowBox env) 14 (n ++ "'") (hsb 15 75 100) (hsb 15 75 100) 2 q | (q, n) <- zip qs names ]
  ++ [ dotAt (hsb 45 90 100) o 16, txt (ox + 12) (oy - 8) 16 white "O" ]
  ++ [ dotAt (gray 0.6) h 8 | h <- [wLa w, wLb w, wMa w, wMb w] ]
  ++ [ txt 20 28 18 white ("(A,B;C,D) = (AC*BD)/(BC*AD):   on l = " ++ fmt crL ++ "   on m = " ++ fmt crM ++ "   -> " ++ (if same then "same!" else "..."))
     , txt 20 50 14 (gray 0.8) "Drag A-D, O and line ends   P: m parallel to l   R: reset"
     , txt 20 70 14 (gray 0.65) "Lengths and ratios change, only the cross-ratio stays (even when a point flies to infinity)."
     -- 表
     , fillRect (hsba 0 0 100 0.08) (tx - 16) (ty - 30) 420 150 ]
  ++ row ty white ["length", "AC", "BC", "AC/BC", "cross"]
  ++ row (ty + 30) (hsb 200 60 100) ["on l", nf 0 (ld 0 2), nf 0 (ld 1 2), nf 2 (ld 0 2 / nz (ld 1 2)), fmt crL]
  ++ row (ty + 60) (hsb 15 75 100) ["on m", fmtD (md 0 2), fmtD (md 1 2), ratioM, fmt crM]
  ++ [ txt tx (ty + 92) 14 (gray 0.7) "AC/BC changes (affine, not projective)."
     , txt tx (ty + 110) 14 (gray 0.7) "Only the 'ratio of ratios' stays the same." ]
  where
    full = screenBox env
    names = ["A", "B", "C", "D"]
    o@(ox, oy) = wO w
    tt = wTT w
    ps = map (onL w) tt
    m = lineThrough (wMa w) (wMb w)
    l = lineThrough (wLa w) (wLb w)
    -- 射影：O と各点を通る直線と m の交点
    qs = map (\p -> hCross (lineThrough o p) m) ps
    -- 複比：l 上は位置 t で、m 上は同次座標の式で（無限遠点があっても計算できる）
    crL = case map f2d tt of
      [a, b, c, d] -> crossRatio4 a b c d
      _ -> Nothing
    crM = case qs of
      [a, b, c, d] -> crossRatioH a b c d (hPt (wMa w)) (hPt (wMb w))
      _ -> Nothing
    same = case (crL, crM) of
      (Just a, Just b) -> abs (a - b) < 1e-3 * (1 + abs a)
      _ -> False
    -- 比較用：ふつうの長さと比
    ld i j = abs (tt !! j - tt !! i) * dist2 (wLa w) (wLb w)
    xy = map toXY qs
    md i j = case (xy !! i, xy !! j) of
      (Just a, Just b) -> Just (dist2 a b)
      _ -> Nothing
    ratioM = case (md 0 2, md 1 2) of
      (Just a, Just b) -> nf 2 (a / nz b)
      _ -> "inf"
    nz v = if v == 0 then 1 else v
    tx = envW env - 430
    ty = 110
    row y c cols = [ txt (tx + dx) y 15 c s | (dx, s) <- zip [0, 80, 150, 220, 310] cols ]

crossRatio4 :: Double -> Double -> Double -> Double -> Maybe Double
crossRatio4 a b c d =
  let den = (c - b) * (d - a)
  in if abs den < 1e-12 then Nothing else Just ((c - a) * (d - b) / den)

fmt :: Maybe Double -> String
fmt Nothing = "-"
fmt (Just v) = if abs v > 1e4 then "inf" else nf 3 (d2f v)

fmtD :: Maybe Float -> String
fmtD = maybe "inf" (nf 0)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = nearestHandle (handles w) (mouseP5 env) 26 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "pP" ->
        let (ax, ay) = wLa w
            (bx, by) = wLb w
            ma@(mx, my) = (envW env * 0.2, envH env * 0.45)
        in w { wMa = ma, wMb = (mx + (bx - ax) * 0.7, my + (by - ay) * 0.7) }
    | c `elem` "rR" -> resetAll env
  _ -> w

-- ---------- やってみよう ----------
-- 1. m を l と平行にする（P）と A'B'C'D' は ABCD の相似な縮小版になる。比 AC/BC も同じになることを確かめよう
-- 2. m の端を動かして D' を無限遠に飛ばす（O と D を結ぶ線と m を平行に）。複比は？（AD → ∞ でも (AC·BD)/(BC·AD) は極限で有限）
-- 3. 複比が −1 になる位置（調和列点）を探そう。C が AB の中点なら D はどこ？（→ 09）
-- 4. A, B, C, D の順番を入れかえると複比はどう変わる？（1 − λ、1/λ などの 6 種類）

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
