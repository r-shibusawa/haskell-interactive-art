-- ============================================
-- 39-05 円錐曲線の射影像：円が楕円・放物線・双曲線になる
-- 左の正方形の中の円を、右の四角形へのホモグラフィ H で写す。写した先は必ず円錐曲線（2 次曲線）
--   H の 3 行目 [h31, h32, h33] は「無限遠直線に写る直線」（w' = h31 x + h32 y + h33 = 0）。左に点線で描く
--   この点線と円が 交わらない → 楕円 / 接する → 放物線 / 2 点で交わる → 双曲線（2 つの交点 = 双曲線の無限遠点 = 漸近線の方向）
-- 右の 4 角をドラッグ / 左の円の中心をドラッグ / ↑↓ = 半径 / T = 放物線にする（半径を点線にぴったり合わせる） / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

src :: [Point]
src = [(0, 0), (1, 0), (1, 1), (0, 1)]

cc0 :: Point
cc0 = (0.5, 0.5)   -- ★ 円の中心（0〜1 の座標）

cr0 :: Float
cr0 = 0.28         -- ★ 円の半径

data World = World
  { wDst  :: ![Point]   -- 右の四角形の角（画面座標）
  , wCC   :: !Point
  , wCR   :: !Float
  , wDrag :: !Int
  , wDragC :: !Bool
  }

main :: IO ()
main = runSketch (sketch "39-05 Projective images of a circle" (World [] cc0 cr0 (-1) False))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> World
resetAll env =
  let x0 = envW env * 0.5
      y0 = envH env * 0.18
      sw = envW env * 0.42
      sh = envH env * 0.66
  in World [ (x0 + sw * 0.1, y0 + sh * 0.98), (x0 + sw * 0.9, y0 + sh * 0.98)
           , (x0 + sw * 0.58, y0 + sh * 0.25), (x0 + sw * 0.42, y0 + sh * 0.25) ] cc0 cr0 (-1) False

-- 左の正方形の大きさと位置
layout :: Env -> (Float, Float, Float)
layout env = (min (envW env * 0.36) (envH env * 0.6), envW env * 0.06, envH env * 0.27)

srcToScreen :: Env -> Point -> Point
srcToScreen env (x, y) = let (s0, ox, oy) = layout env in (ox + x * s0, oy + y * s0)

step :: Env -> World -> World
step env w =
  let m@(mx, my) = mouseP5 env
      (s0, ox, oy) = layout env
      w1 = if wDrag w >= 0 then w { wDst = setAt (wDrag w) m (wDst w) } else w
  in if wDragC w1
       then w1 { wCC = (clampF 0.05 0.95 ((mx - ox) / s0), clampF 0.05 0.95 ((my - oy) / s0)) }
       else w1

-- 中心 c から直線 l までの距離
distTo :: Point -> H3 -> Float
distTo (cx, cy) (H3 a b c) =
  let n = sqrt (a * a + b * b)
  in d2f (abs (a * f2d cx + b * f2d cy + c) / (if n == 0 then 1 else n))

draw :: Env -> World -> Picture
draw env w = case homographyFrom4 src (wDst w) of
  Nothing -> view env [txt 20 40 18 white "The 4 points are degenerate (press R)"]
  Just h@(M3 _ _ linf@(H3 la lb lc)) ->
    let cc = wCC w
        cr = wCR w
        (s0, ox, oy) = layout env
        -- 無限遠直線の元 l∞ = H の 3 行目（元の正方形の座標で）
        d = distTo cc linf
        kind :: Int
        kind = if d > cr * 1.01 then 0 else if d < cr * 0.99 then 2 else 1
        kindName = case kind of
          0 -> "ellipse (0 meets)"
          1 -> "parabola (tangent: 1 meet)"
          _ -> "hyperbola (2 meets)"
        c0 = srcToScreen env cc
        ks = [fromIntegral k / 8 | k <- [0 .. 8 :: Int]] :: [Float]
        -- l∞ の元（点線）：a x + b y + c = 0 を画面座標に
        s0d = f2d s0
        lscr = H3 (la / s0d) (lb / s0d) (lc - la * f2d ox / s0d - lb * f2d oy / s0d)
        inter = circleLine cc cr linf
        gridC = hsba 0 0 100 0.25
        pts = map imagePt [0 .. 720 :: Int]
        imagePt i =
          let a = fromIntegral i / 720 * pi * 2
              H3 x y q = mat3Vec h (hPt (fst cc + cr * cos a, snd cc + cr * sin a))
          in if abs q > 1e-9 then Just (d2f (x / q), d2f (y / q), signum q) else Nothing
        fine v = abs v < 1e5
        curve = [ seg 3 (hsb 200 60 100) (x1, y1) (x2, y2)
                | (Just (x1, y1, s1), Just (x2, y2, s2)) <- zip pts (drop 1 pts)
                , s1 == s2, fine x1, fine y1, fine x2, fine y2 ]
        mapped (x0, y0) (x1, y1) =
          let ps = [ if q > 1e-6 then Just (d2f (x / q), d2f (y / q)) else Nothing
                   | i <- [0 .. 16 :: Int], let t = fromIntegral i / 16
                   , let H3 x y q = mat3Vec h (hPt (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)) ]
          in [seg 1 gridC a b | (Just a, Just b) <- zip ps (drop 1 ps)]
        red = hsb 15 80 100
        rightBox = (envW env * 0.46, 90, envW env - 10, envH env - 10)
    in view env $
         -- ---- 左：元の正方形
         [ fillRect (hsba 0 0 100 0.06) ox oy s0 s0 ]
         ++ concat [ [ seg 1 gridC (ox + k * s0, oy) (ox + k * s0, oy + s0), seg 1 gridC (ox, oy + k * s0) (ox + s0, oy + k * s0) ] | k <- ks ]
         ++ [ ringAt (hsb 200 60 100) c0 (cr * s0 * 2) 3
            , case hLineEnds (ox - 130, oy - 130, ox + s0 + 130, oy + s0 + 130) lscr of
                Just (a, b) -> dashed8 2 (hsb 45 90 100) a b
                Nothing -> Blank ]
         -- 交点（円と l∞）→ 右では無限遠点（漸近線の方向）
         ++ [ dotAt red (srcToScreen env p) 12 | p <- inter ]
         ++ [ dotAt (hsb 45 90 100) c0 10
            , txt ox (oy - 12) 14 white "Source square (0..1)   dashed = maps to line at infinity" ]
         -- ---- 右：写した先
         ++ concat [ mapped (k, 0) (k, 1) ++ mapped (0, k) (1, k) | k <- ks ]
         ++ [ closedPath 1 (hsba 0 0 100 0.6) (wDst w) ]
         -- 円の像：細かく刻んで写す。w' の符号が変わる所（無限遠を通る所）で線を切る
         ++ curve
         -- 無限遠点の方向（矢印）
         ++ [ hPoint rightBox 14 "" red red 3 (mat3Vec h (hPt p)) | p <- inter ]
         ++ [ dotAt (hsb 45 90 100) p 16 | p <- wDst w ]
         ++ [ txt 20 28 18 white ("Image: " ++ kindName ++ "   center-to-line d = " ++ nf 3 d ++ "   radius r = " ++ nf 3 cr)
            , txt 20 50 14 (gray 0.8) "Drag right corners and left circle center   Up/Down: radius   T: make parabola   R: reset"
            , txt 20 70 14 (gray 0.65) "When the circle crosses the dashed line, the curve passes infinity and returns from the other side."
            , txt 20 (envH env - 24) 14 (gray 0.8) "Ellipse, parabola, hyperbola differ only in how they meet the line at infinity. All are images of a circle."
            ]

dashed8 :: Float -> Color -> Point -> Point -> Picture
dashed8 w c a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      n = floor (l / 8) :: Int
      p i = let t = fromIntegral i * 8 / l in (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in if l < 1 then Blank else Pictures [seg w c (p i) (p (i + 1)) | i <- [0, 2 .. n - 1]]

-- 円（中心 c、半径 r）と直線 [a, b, c] の交点（0〜2 個）
circleLine :: Point -> Float -> H3 -> [Point]
circleLine (cx, cy) r (H3 la lb lc) =
  let n0 = sqrt (la * la + lb * lb)
      n = if n0 == 0 then 1 else n0
      a = d2f (la / n)
      b = d2f (lb / n)
      k = d2f (lc / n)
      d = a * cx + b * cy + k          -- 符号つき距離
      fx = cx - a * d                  -- 垂線の足
      fy = cy - b * d
      h = sqrt (max 0 (r * r - d * d))
  in if abs d > r then []
     else if h < 1e-9 then [(fx, fy)]
     else [(fx - b * h, fy + a * h), (fx + b * h, fy - a * h)]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP5 env
        i = nearestHandle (wDst w) m 30
    in w { wDrag = i, wDragC = i < 0 && dist2 m (srcToScreen env (wCC w)) < 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1, wDragC = False }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wCR = min 0.9 (wCR w + 0.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wCR = max 0.03 (wCR w - 0.02) }
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> case homographyFrom4 src (wDst w) of
        Just (M3 _ _ l) -> w { wCR = clampF 0.03 0.9 (distTo (wCC w) l) }
        Nothing -> w
    | c `elem` "rR" -> resetAll env
  _ -> w

-- ---------- やってみよう ----------
-- 1. 右の四角形を長方形にすると点線は正方形からどこまで離れる？（アフィンなら l∞ は無限遠のまま = 楕円しかできない）
-- 2. 双曲線のとき、矢印の方向（無限遠点）が漸近線の方向。曲線が本当にその方向に伸びているか確かめよう
-- 3. 放物線（T）のときの矢印は 1 つ。放物線は「無限遠直線に接する」と言える。接点の方向は軸の方向？
-- 4. 円のかわりに正方形の中の別の図形（三角形）を写してみよう。角が無限遠を通るとどう見える？

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
