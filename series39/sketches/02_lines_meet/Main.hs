-- ============================================
-- 39-02 平行線も交わる：外積で直線と交点
-- 2 点 P, Q（同次座標）を通る直線は外積 P × Q。2 直線 l, m の交点も外積 l × m。式はまったく同じ！
--   平行な 2 直線の交点は w = 0 の点 = 無限遠点。画面のふちに「方向の矢印」で描く
--   D で双対：点 (a, b, c) ↔ 直線 ax + by + cw = 0 を入れかえても、同じ定理が成り立つ
-- 4 つの点をドラッグ / P = 2 本目を 1 本目と平行にする / D = 双対を表示 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

data World = World
  { wPts  :: ![Point]   -- 4 つの端点（直線 1 = 0, 1 番、直線 2 = 2, 3 番）
  , wDrag :: !Int
  , wDual :: !Bool
  }

main :: IO ()
main = runSketch (sketch "39-02 Lines meet: cross products" (World [] (-1) False))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env (World [] (-1) False))
  , skStep  = \env _ w -> pure (if wDrag w >= 0 then w { wPts = setAt (wDrag w) (mouseP5 env) (wPts w) } else w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> World -> World
resetAll env w =
  let sw = envW env
      sh = envH env
  in w { wPts = [(sw * 0.25, sh * 0.7), (sw * 0.55, sh * 0.35), (sw * 0.3, sh * 0.3), (sw * 0.7, sh * 0.6)] }

-- 数値表示用の単位（px）。画面の中心が原点
unitPx :: Env -> Double
unitPx env = f2d (min (envW env) (envH env) / 4)

-- 画面の座標 ↔ 中心が原点・U px = 1 の座標（数値を見やすく、双対を意味のある形にするため）
toUnit :: Env -> Point -> H3
toUnit env (x, y) = let u = unitPx env in H3 ((f2d x - f2d (envW env) / 2) / u) ((f2d (envH env) / 2 - f2d y) / u) 1

unitLineToScreen :: Env -> H3 -> H3
unitLineToScreen env (H3 a b c) =
  let u = unitPx env
      sw = f2d (envW env)
      sh = f2d (envH env)
  in H3 (a / u) (negate b / u) (c - a * sw / (2 * u) + b * sh / (2 * u))

unitPtToScreen :: Env -> H3 -> H3
unitPtToScreen env (H3 x y w) =
  let u = unitPx env
  in H3 (x * u + w * f2d (envW env) / 2) (negate y * u + w * f2d (envH env) / 2) w

draw :: Env -> World -> Picture
draw env w = case wPts w of
  [pa, pb, pc, pd] ->
    let dual = wDual w
        full = screenBox env
        -- 単位座標で計算（表示する数値）
        ua = toUnit env pa
        ub = toUnit env pb
        uc = toUnit env pc
        ud = toUnit env pd
        l1 = hCross ua ub           -- 直線 = 点 × 点
        l2 = hCross uc ud
        x = hCross l1 l2            -- 交点 = 直線 × 直線
        n1 = hUnit l1
        n2 = hUnit l2
        nX = hUnit x
        far = isFar x
        fade = if dual then 0.4 else 1
        lineA = if dual then 0.35 else 0.9
        names = ["P", "Q", "R", "S"]
    in view env $
         [ hLine 3 (hsba 200 60 95 lineA) full (unitLineToScreen env l1)
         , hLine 3 (hsba 15 75 95 lineA) full (unitLineToScreen env l2) ]
         ++ [ Pictures [dotAt c p 14, txt (fst p + 10) (snd p - 10) 16 c n]
            | (i, p, n) <- zip3 [0 :: Int ..] (wPts w) names
            , let c = hsba (if i < 2 then 200 else 15) 60 100 fade ]
         ++ [ hPoint (arrowBox env) 22 "X" (hsba 45 90 100 0.4) (hsb 45 90 100) 3 (unitPtToScreen env x) ]
         ++ [ drawDual env [ua, ub, uc, ud] [l1, l2] x | dual ]
         ++ [ txt 20 28 16 white ("l1 = PxQ = [" ++ v3 n1 ++ "]  l2 = RxS = [" ++ v3 n2 ++ "]  X = l1xl2 = [" ++ v3 nX ++ "]")
            , txt 20 50 14 (if far then hsb 45 90 100 else gray 0.8)
                ("Drag points   P: make parallel   D: dual " ++ (if dual then "ON" else "off") ++ "   R: reset" ++ (if far then "     <- w ~ 0: point at infinity!" else ""))
            , txt 20 70 14 (gray 0.65) "Parallel lines: w of X is 0 -> arrows at the screen edge (both ends are the same 1 point)."
            , txt 20 (envH env - 24) 14 (gray 0.8)
                ("Check (dot): l1.X = " ++ nf 4 (d2f (hDot n1 nX)) ++ "   l2.X = " ++ nf 4 (d2f (hDot n2 nX)) ++ "   -> X lies on both lines")
            ]
  _ -> Blank

-- 双対：点 [a, b, c] → 直線 ax + by + cw = 0、直線 → 点。元の図の「点と線」が入れかわる
drawDual :: Env -> [H3] -> [H3] -> H3 -> Picture
drawDual env ps ls x = Pictures $
  [ ringAt (hsba 0 0 100 0.2) (cx, cy) (u * 2) 1
  , seg 1 (hsba 0 0 100 0.25) (cx, 0) (cx, envH env)
  , seg 1 (hsba 0 0 100 0.25) (0, cy) (envW env, cy) ]
  -- 4 点 → 4 直線
  ++ [ hLine 1.5 (hsba (if i < 2 then 200 else 15) 60 100 0.7) full (unitLineToScreen env p) | (i, p) <- zip [0 :: Int ..] ps ]
  -- 2 直線 → 2 点（= 対応する 2 直線の交点になっている）
  ++ [ let c = hsb (if i == 0 then 200 else 15) 60 100
       in hPoint (arrowBox env) 16 ("l" ++ show (i + 1) ++ "*") c c 0 (unitPtToScreen env l)
     | (i, l) <- zip [0 :: Int ..] ls ]
  -- 交点 X → 直線（2 つの双対点を通る）
  ++ [ hLine 2 (hsba 45 90 100 0.9) full (unitLineToScreen env x)
     , txt 20 92 14 (hsb 45 90 100) "Dual: points P, Q -> lines P*, Q*. Line l1 = PxQ -> point l1* = P*^Q*. '2 points on 1 line' <-> '2 lines meet at 1 point'"
     ]
  where
    full = screenBox env
    u = d2f (unitPx env)
    cx = envW env / 2
    cy = envH env / 2

v3 :: H3 -> String
v3 (H3 a b c) = nf 2 (d2f a) ++ ", " ++ nf 2 (d2f b) ++ ", " ++ nf 2 (d2f c)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = nearestHandle (wPts w) (mouseP5 env) 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> case wPts w of   -- S を R + (Q − P) にして平行に
        [(ax, ay), (bx, by), r@(rx, ry), _] -> w { wPts = [(ax, ay), (bx, by), r, (rx + bx - ax, ry + by - ay)] }
        _ -> w
    | c `elem` "dD" -> w { wDual = not (wDual w) }
    | c `elem` "rR" -> resetAll env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. P を押して平行にし、S をほんの少しだけ動かす。交点は画面のどちら側に現れる？ 矢印の向きと比べよう
-- 2. 直線 [a, b, c] の a, b, c を 2 倍しても同じ直線。hUnit で長さをそろえている理由を考えよう
-- 3. 双対（D）で P, Q, R, S を動かし、「3 点が一直線」のとき双対の 3 直線がどうなるか観察
-- 4. 3 本目の直線を足して、3 直線が 1 点で交わる条件を行列式 hDet(l1, l2, l3) = 0 で判定してみよう

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
