-- ============================================
-- 39-06 デザルグの定理
-- 2 つの三角形 ABC と A'B'C' が「点配景」（AA', BB', CC' が 1 点 O を通る）なら、
--   対応する辺の交点 AB∩A'B', BC∩B'C', CA∩C'A' の 3 つは必ず 1 直線（配景の軸）の上にある
--   双対：3 つの交点が共線（線配景）なら AA', BB', CC' は共点。定理の文の「点」と「線」を入れかえても正しい
-- O・A・B・C・A'・B'・C' をドラッグ（A' は直線 OA の上を動く） / D = 双対の説明 / 1 = A'B'C' を ABC の相似形に / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

kk0 :: [Float]
kk0 = [1.8, 1.45, 2.3]   -- ★ A' = O + k (A − O) の k

data World = World
  { wO    :: !Point      -- 中心
  , wTri  :: ![Point]    -- 三角形 ABC
  , wK    :: ![Float]
  , wDrag :: !Int
  , wDual :: !Bool
  }

main :: IO ()
main = runSketch (sketch "39-06 Desargues' theorem" (World (0, 0) [] kk0 (-1) False))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env False)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> Bool -> World
resetAll env dual =
  let sw = envW env
      sh = envH env
  in World (sw * 0.12, sh * 0.5) [(sw * 0.38, sh * 0.3), (sw * 0.42, sh * 0.72), (sw * 0.3, sh * 0.55)] kk0 (-1) dual

primed :: World -> [Point]
primed w = let (ox, oy) = wO w in zipWith (\(x, y) k -> (ox + k * (x - ox), oy + k * (y - oy))) (wTri w) (wK w)

step :: Env -> World -> World
step env w
  | i < 0 = w
  | i == 0 = w { wO = m }
  | i <= 3 = w { wTri = setAt (i - 1) m (wTri w) }
  | otherwise =   -- A' などは直線 OA の上に拘束：k = 射影
      let j = i - 4
          (ox, oy) = wO w
          (ax, ay) = wTri w !! j
          vx = ax - ox
          vy = ay - oy
          l2 = let v = vx * vx + vy * vy in if v == 0 then 1 else v
          k = ((mx - ox) * vx + (my - oy) * vy) / l2
      in w { wK = setAt j (clampF (-4) 4 (if abs k < 0.05 then 0.05 else k)) (wK w) }
  where
    i = wDrag w
    m@(mx, my) = mouseP5 env

draw :: Env -> World -> Picture
draw env w = view env $
  -- 配景の線（O から）
  [ seg 1 (hsba 45 80 100 0.35) (ox - dx * 5, oy - dy * 5) (ox + dx * 8, oy + dy * 8)
  | (ax, ay) <- t, let dx = ax - ox, let dy = ay - oy ]
  -- 辺の延長（薄く）
  ++ concat [ [hLine 1 faint full a, hLine 1 faint full b] | (a, b) <- zip (sides t) (sides tp) ]
  -- 軸
  ++ [ hLine (if dual then 4 else 2) (hsba 15 80 100 (if dual then 1 else 0.8)) full axis
     -- 三角形
     , closedPath 3 (hsb 200 60 100) t
     , closedPath 3 (hsb 140 60 95) tp ]
  -- 点
  ++ concat [ [ dotAt (hsb 200 60 100) a 14, txt (fst a + 10) (snd a - 8) 16 white n
              , dotAt (hsb 140 60 95) b 14, txt (fst b + 10) (snd b - 8) 16 white (n ++ "'") ]
            | (a, b, n) <- zip3 t tp ["A", "B", "C"] ]
  ++ [ dotAt (hsb 45 90 100) (ox, oy) (if dual then 12 else 18)
     , txt (ox + 12) (oy - 10) 16 white "O (center)" ]
  ++ [ hPoint (arrowBox env) 16 n (hsba 15 80 100 0.6) (hsb 15 80 100) 2 x | (x, n) <- zip xs ["AB^A'B'", "BC^B'C'", "CA^C'A'"] ]
  ++ [ txt 20 28 16 white ("det of the 3 meets = " ++ nf 6 (d2f det) ++ " -> " ++ (if abs det < 1e-4 then "collinear OK (always 0)" else "...")
                           ++ (if axisFar then "  Axis at infinity (similar)" else ""))
     , txt 20 50 14 (gray 0.8) ("Drag O, A, B, C, A', B', C'   D: dual " ++ (if dual then "ON" else "off") ++ "   1: similar   R: reset")
     , txt 20 70 14 (gray 0.65) "Even off-screen or at infinity, the 3 meets stay on 1 line." ]
  ++ (if dual then
       [ fillRect (hsba 0 0 100 0.08) px 90 500 170
       , txt (px + 20) 118 16 (hsb 15 80 100) "Dual (point <-> line, through <-> meet)"
       , t' 146 "Theorem: AA', BB', CC' pass through 1 point O"
       , t' 166 "  => AB^A'B', BC^B'C', CA^C'A' are on 1 line"
       , t' 196 "Dual: if the side meets are on 1 line,"
       , t' 216 "  => AA', BB', CC' meet at 1 point (converse)"
       , t' 244 "Self-dual. Think of a shadow of 3D triangles." ]
      else [])
  where
    full = screenBox env
    dual = wDual w
    faint = hsba 0 0 100 0.12
    (ox, oy) = wO w
    t = wTri w
    tp = primed w
    px = envW env - 520
    t' y s = txt (px + 20) y 14 (gray 0.9) s
    -- 対応する辺の交点
    sides ps = zipWith lineThrough ps (drop 1 ps ++ take 1 ps)
    xs = zipWith hCross (sides t) (sides tp)
    (axis, det, axisFar) = case xs of
      [a, b, c] -> (hCross a b, hDet (hUnit a) (hUnit b) (hUnit c), isFar a && isFar b)
      _ -> (H3 0 0 0, 0, False)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = nearestHandle (wO w : wTri w ++ primed w) (mouseP5 env) 26 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (Char '1') Down _ _ -> w { wK = [1.8, 1.8, 1.8] }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> w { wDual = not (wDual w) }
    | c `elem` "rR" -> resetAll env (wDual w)
  _ -> w

-- ---------- やってみよう ----------
-- 1. A' を O の反対側（k < 0）へ動かす。2 つの三角形が O をはさんでも定理は成り立つ？
-- 2. 1 で相似形にすると 3 つの交点はすべて無限遠（矢印が 3 組）。それでも「1 直線上」= 無限遠直線上
-- 3. O を無限遠に持っていきたい。AA', BB', CC' を平行にするにはどうすれば？（O をずっと遠くへ）
-- 4. この図を「2 枚の三角形の板に光を当てた影」と考えると、軸は 2 枚の板の面の交線。立体で説明してみよう

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
