-- ============================================
-- 39-10 双対性の遊び場
-- 左に点と直線を置くと、右にその双対（点 ↔ 直線）がリアルタイムで描かれる
--   双対は単位円に関する「極と極線」：点 (u, v) ↔ 直線 uX + vY = 1。円の内側の点 ↔ 円の外の直線、円周上の点 ↔ 接線
--   左で「3 点が 1 直線」なら右で「3 直線が 1 点」。「2 点を通る直線」は「2 直線の交点」になる
-- 左の空いた所をクリック = 点を置く / 点をドラッグ / 点を続けて 2 つクリック = その 2 点を通る直線を引く（もう一度で消す） / X = 選んだ点を消す / 1〜3 = お手本 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Data.List (findIndex, maximumBy, sort)
import Data.Ord (comparing)
import Numeric (showFFloat)

maxPoints :: Int
maxPoints = 12   -- ★ 置ける点の数

data World = World
  { wPts   :: ![Point]        -- 左の点（画面座標）
  , wLines :: ![(Int, Int)]   -- 左の直線 = (点の番号, 点の番号)
  , wSel   :: ![Int]          -- 選択中の点（最大 2）
  , wDrag  :: !Int
  , wMoved :: !Bool
  }

main :: IO ()
main = runSketch (sketch "39-10 Duality playground" (World [] [] [] (-1) False))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (preset env 1 (World [] [] [] (-1) False))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- 左右の枠
boxL, boxR :: Env -> Frame
boxL env = (0, 90, envW env / 2 - 5, envH env)
boxR env = (envW env / 2 + 5, 90, envW env, envH env)

center :: Frame -> Point
center (a, b, c, d) = ((a + c) / 2, (b + d) / 2)

-- 単位円の半径（px）
unitPx :: Env -> Float
unitPx env = min (envW env / 4) ((envH env - 90) / 2) * 0.5

-- 画面 ↔ 単位円の座標
toUnit :: Env -> Point -> H3
toUnit env (x, y) =
  let (cx, cy) = center (boxL env)
      u = f2d (unitPx env)
  in H3 ((f2d x - f2d cx) / u) ((f2d cy - f2d y) / u) 1

fromUnit :: Env -> Point -> Point
fromUnit env (x, y) =
  let (cx, cy) = center (boxL env)
      u = unitPx env
  in (cx + x * u, cy - y * u)

unitLineToScreen :: Env -> Point -> H3 -> H3
unitLineToScreen env (cx, cy) (H3 a b c) =
  let u = f2d (unitPx env)
  in H3 (a / u) (negate b / u) (c - a * f2d cx / u + b * f2d cy / u)

-- 双対：点 [u, v, 1] → 直線 uX + vY − 1 = 0。直線 [a, b, c] → 点 [a, b, −c]
dualOf :: H3 -> H3
dualOf (H3 a b c) = H3 a b (negate c)

preset :: Env -> Int -> World -> World
preset env n w =
  let (ps, ls) = case n of
        1 -> ([(-1.2, -0.5), (0.1, 0.2), (1.4, 0.9), (0.6, -1.1)], [(0, 1), (1, 2), (0, 2)])
        2 -> ([(-1, -0.8), (1.1, -0.6), (0.2, 1.2)], [(0, 1), (1, 2), (2, 0)])
        _ -> ([(-1.2, -0.9), (1.2, -0.9), (0.8, 1.1), (-0.5, 0.7)], [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (1, 3)])
  in w { wPts = map (fromUnit env) ps, wLines = ls, wSel = [], wDrag = -1 }

step :: Env -> World -> World
step env w
  | i < 0 || i >= length (wPts w) = w
  | otherwise =
      let (b0, b1, b2, b3) = boxL env
          (mx, my) = mouseP5 env
          n = (clampF (b0 + 10) (b2 - 10) mx, clampF (b1 + 10) (b3 - 10) my)
      in w { wPts = setAt i n (wPts w), wMoved = wMoved w || dist2 n (wPts w !! i) > 2 }
  where i = wDrag w

draw :: Env -> World -> Picture
draw env w = view env $
  -- 枠と単位円
  concat [ let (a, b, c', d) = bx in
           [ strokeRect (hsba 0 0 100 0.25) a b (c' - a) (d - b)
           , ringAt (hsba 0 0 100 0.3) c (u * 2) 1
           , seg 1 (hsba 0 0 100 0.3) (fst c - 8, snd c) (fst c + 8, snd c)
           , seg 1 (hsba 0 0 100 0.3) (fst c, snd c - 8) (fst c, snd c + 8)
           , txt (a + 12) (b + 24) 15 (hsb 45 80 100) t ]
         | (bx, c, t) <- [(bl, cl, "Points and lines (edit here)"), (br, cr, "Dual (point <-> line)")] ]
  -- 左：直線
  ++ [ hLine 2 (hsba 200 60 95 0.8) bl (unitLineToScreen env cl l) | l <- hl ]
  -- 右：点の双対 = 直線
  ++ [ hLine 2 (hsba 15 75 100 0.85) br (unitLineToScreen env cr (dualOf p)) | p <- hp ]
  -- 右：直線の双対 = 点
  ++ [ let H3 a b c = dualOf l
           ud = f2d u
       in hPoint br 14 ("l" ++ show k ++ "*") blue blue 2
            (H3 (a * ud + c * f2d (fst cr)) (negate b * ud + c * f2d (snd cr)) c)
     | (k, l) <- zip [1 :: Int ..] hl ]
  -- 左：点
  ++ concat [ let s = i `elem` wSel w in
              [ dotAt (hsb 15 75 100) p (if s then 20 else 14)
              , if s then ringAt (hsb 45 90 100) p 28 2 else Blank
              , txt (fst p + 10) (snd p - 8) 14 white ("P" ++ show (i + 1)) ]
            | (i, p) <- zip [0 ..] pts ]
  -- 左：直線のラベル（2 点の中点に）
  ++ [ let (ax, ay) = pts !! i
           (bx, by) = pts !! j
       in txt ((ax + bx) / 2 + 6) ((ay + by) / 2 - 6) 14 blue ("l" ++ show k)
     | (k, (i, j)) <- zip [1 :: Int ..] (wLines w) ]
  ++ [ txt 20 28 16 white ("Left: " ++ show np ++ " points, " ++ show nl ++ " lines -> Right: " ++ show np ++ " lines, " ++ show nl ++ " points"
                           ++ (if null collin then "" else "   Collinear " ++ unwords (take 3 collin) ++ ": 3 lines meet"))
     , txt 20 50 14 (gray 0.8) "Click: add point   Drag: move   Click 2 points in a row: draw/erase line"
     , txt 20 70 14 (gray 0.65) "X: delete selected   1-3: examples   C: clear all   Point at circle center -> dual is the line at infinity" ]
  where
    bl = boxL env
    br = boxR env
    cl = center bl
    cr = center br
    u = unitPx env
    pts = wPts w
    np = length pts
    nl = length (wLines w)
    blue = hsb 200 60 95
    hp = map (toUnit env) pts
    hl = [hCross (hp !! i) (hp !! j) | (i, j) <- wLines w]
    -- 共線・共点のチェック：3 点が共線 ↔ 右で 3 直線が共点
    idx = zip [1 :: Int ..] hp
    collin = [ "P" ++ show i ++ "P" ++ show j ++ "P" ++ show k
             | (i, a) <- idx, (j, b) <- idx, i < j, (k, c) <- idx, j < k
             , abs (hDet (hUnit a) (hUnit b) (hUnit c)) < 2e-3 ]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m@(mx, my) = mouseP5 env
        (_, b1, b2, _) = boxL env
        i = nearestHandle (wPts w) m 22
    in if mx > b2 || my < b1 then w
       else if i < 0 && length (wPts w) < maxPoints then w { wDrag = i, wMoved = False, wPts = wPts w ++ [m], wSel = [] }
       else w { wDrag = i, wMoved = False }
  EventKey (MouseButton LeftButton) Up _ _ -> (if wDrag w >= 0 && not (wMoved w) then clickPoint (wDrag w) w else w) { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> foldl removePoint w { wSel = [], wDrag = -1 } (reverse (sort (wSel w)))
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
    | c `elem` "cC" -> w { wPts = [], wLines = [], wSel = [], wDrag = -1 }
  _ -> w

-- クリック = 選択。2 点そろったら直線を引く（もう引いてあれば消す）
clickPoint :: Int -> World -> World
clickPoint i w =
  let sel = if i `elem` wSel w then filter (/= i) (wSel w) else wSel w ++ [i]
  in case sel of
       [a, b] ->
         let same (p, q) = (p == a && q == b) || (p == b && q == a)
             ls = case findIndex same (wLines w) of
               Just _ -> filter (not . same) (wLines w)
               Nothing -> wLines w ++ [(a, b)]
         in w { wLines = ls, wSel = [] }
       _ -> w { wSel = sel }

removePoint :: World -> Int -> World
removePoint w i =
  let down k = if k > i then k - 1 else k
  in w { wPts = [p | (k, p) <- zip [0 ..] (wPts w), k /= i]
       , wLines = [(down a, down b) | (a, b) <- wLines w, a /= i, b /= i] }

-- ---------- やってみよう ----------
-- 1. お手本 1 で P1, P2, P3 が 1 直線に並ぶようにドラッグ。右の 3 直線が 1 点で交わる瞬間を見よう
-- 2. 点を単位円の上に置くと、双対の直線はその点での接線になる。円の中心に置くと？（無限遠直線 = 描けない）
-- 3. お手本 3（完全四辺形：4 点 6 直線）の双対は「完全四角形：4 直線 6 点」。右の図を数えて確かめよう
-- 4. デザルグ（06）やパップス（07）の図をここで作って、双対の図で「双対定理」を目で確かめよう

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
