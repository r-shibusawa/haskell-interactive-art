-- ============================================
-- 39-09 調和共役と完全四辺形
-- 直線上の 3 点 A, B, C から、複比 (A,B;C,D) = −1 になる第 4 点 D（C の調和共役）を「定規だけ」で作図する
--   ① 直線の外に点 P、PA 上に点 Q　② 直線 QC と PB の交点 R　③ 直線 AR と BQ の交点 S　④ 直線 PS と AB の交点 = D
--   P, Q をどこに取っても D は同じ所に決まる（完全四辺形 PQRS の性質）。C が AB の中点なら D は無限遠
-- スペース = 次の手順（作図線がアニメーション） / A, B, C を直線上でドラッグ / P, Q をドラッグ / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

speed :: Float
speed = 0.03    -- ★ 作図線が伸びる速さ

data World = World
  { wYL :: !Float             -- 直線 AB の高さ
  , wXa, wXb, wXc :: !Float   -- A, B, C の x 座標
  , wP  :: !Point             -- P の位置
  , wSQ :: !Float             -- Q = P + sQ (A − P)
  , wStage :: !Int            -- 手順
  , wProg  :: !Float          -- その手順のアニメーションの進み（0〜1）
  , wDrag  :: !Int
  }

main :: IO ()
main = runSketch (sketch "39-09 Harmonic conjugate" (World 416 218 806 678 (947, 168) 0.43 0 0 (-1)))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetAll :: Env -> World
resetAll env =
  let sw = envW env
      sh = envH env
  in World (sh * 0.52) (sw * 0.17) (sw * 0.63) (sw * 0.53) (sw * 0.74, sh * 0.21) 0.43 0 0 (-1)

qPt :: World -> Point
qPt w =
  let (px, py) = wP w
  in (px + wSQ w * (wXa w - px), py + wSQ w * (wYL w - py))

step :: Env -> World -> World
step env w =
  let (mx, my) = mouseP5 env
      (px, py) = wP w
      w1 = case wDrag w of
        0 -> w { wXa = mx }
        1 -> w { wXb = mx }
        2 -> w { wXc = mx }
        3 -> w { wP = (mx, clampF 40 (max 40 (wYL w - 40)) my) }
        4 -> let vx = wXa w - px
                 vy = wYL w - py
                 l2 = let v = vx * vx + vy * vy in if v == 0 then 1 else v
             in w { wSQ = clampF 0.1 0.9 (((mx - px) * vx + (my - py) * vy) / l2) }
        _ -> w
  in w1 { wProg = min 1 (wProg w1 + speed) }

-- a から b の方向へ、長さ k 倍まで、進み f で線を伸ばす
grow :: Color -> Point -> Point -> Float -> Float -> Picture
grow c a@(ax, ay) (bx, by) f k = seg 2 c a (ax + (bx - ax) * k * f, ay + (by - ay) * k * f)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 直線 AB
  [ seg 2 (hsba 200 60 95 0.7) (0, y) (envW env, y) ]
  -- 作図（手順ごとに描く。今の手順はアニメーション）
  ++ (if f 1 > 0 then [grow (hsba 45 70 100 0.7) p a (f 1) 2.2, grow (hsba 45 70 100 0.7) p b (f 1) 2.2] else [])
  ++ [ grow (hsba 140 60 95 0.8) q c (f 2) 1.6 | f 2 > 0, Just _ <- [mr] ]
  ++ concat [ [grow (hsba 15 75 100 0.8) a r (f 3) 1.5, grow (hsba 15 75 100 0.8) b q (f 3) 1.6] | f 3 > 0, Just r <- [mr], Just _ <- [ms] ]
  ++ [ case md of
         Just (_, dy) -> grow (hsba 300 50 100 0.9) p s (f 4) ((dy - snd p) / nz (snd s - snd p))
         Nothing -> grow (hsba 300 50 100 0.9) p s (f 4) 3
     | f 4 > 0, Just s <- [ms] ]
  -- 点
  ++ concat [ [dotAt (hsb col 60 100) pt 14, txt (fst pt - 5) (snd pt + 26) 16 white nm]
            | (pt, nm, col) <- [(a, "A", 200), (b, "B", 200), (c, "C", 45)] ]
  ++ (if stage >= 1 then [dotAt (hsb 45 70 100) p 14, txt (fst p + 10) (snd p - 8) 16 white "P"] else [])
  ++ (if stage >= 1 && f 1 >= 1 then [dotAt (hsb 45 70 100) q 12, txt (fst q - 24) (snd q) 16 white "Q"] else [])
  ++ [ Pictures [dotAt (hsb 140 60 95) r 12, txt (fst r + 10) (snd r) 16 white "R"] | stage >= 2, f 2 >= 1, Just r <- [mr] ]
  ++ [ Pictures [dotAt (hsb 15 75 100) s 12, txt (fst s + 10) (snd s - 8) 16 white "S"] | stage >= 3, f 3 >= 1, Just s <- [ms] ]
  ++ [ hPoint (arrowBox env) 16 "D" (hsb 300 50 100) (hsb 300 50 100) 2 dh | stage >= 4, f 4 >= 1 ]
  ++ (if stage >= 5
        then
          -- 複比の図示：A, B に対して C が内分、D が外分。同じ比
          let py = envH env - 140
          in [ fillRect (hsba 0 0 100 0.08) 20 (py - 30) 540 120
             , txt 40 py 16 white ("(A,B;C,D) = (AC/BC) / (AD/BD) = " ++ maybe "-" (nf 4 . d2f) cr)
             , txt 40 (py + 28) 14 (gray 0.8)
                 ("AC/CB = " ++ nf 3 ((xc - xa) / nz9 (xb - xc)) ++ " (inner)   AD/DB = "
                  ++ maybe "inf (at infinity)" (\(dx, _) -> nf 3 ((dx - xa) / nz9 (xb - dx))) md ++ " (outer)")
             , txt 40 (py + 52) 14 (gray 0.8) "Inner ratio = outer ratio: harmonic. C at midpoint -> D at infinity"
             , txt 40 (py + 76) 14 (gray 0.8) "Move P and Q anywhere: D does not move" ]
        else [])
  ++ [ txt 20 28 17 white ("Step " ++ show stage ++ "/5: " ++ msg)
     , txt 20 50 14 (gray 0.8) "Space: next   Drag A, B, C along the line   Drag P, Q   R: restart"
     , txt 20 70 14 (gray 0.65) "Complete quadrilateral PQRS: A, B and the 2 other side meets C, D are harmonic." ]
  where
    y = wYL w
    xa = wXa w
    xb = wXb w
    xc = wXc w
    stage = wStage w
    p = wP w
    a = (xa, y)
    b = (xb, y)
    c = (xc, y)
    q = qPt w
    ab = lineThrough a b
    mr = toXY (hCross (lineThrough q c) (lineThrough p b))
    ms = mr >>= \r -> toXY (hCross (lineThrough r a) (lineThrough q b))
    dh = case ms of
      Just s -> hCross (lineThrough p s) ab
      Nothing -> H3 1 0 0
    md = toXY dh
    cr = crossRatioH (hPt a) (hPt b) (hPt c) dh (hPt a) (hPt b)
    f :: Int -> Float
    f k = if k < stage then 1 else if k == stage then wProg w else 0
    nz v = if v == 0 then 1 else v
    nz9 v = if v == 0 then 1e-9 else v
    msg = case stage of
      0 -> "Construct the harmonic conjugate D of C with a ruler only -> Space"
      1 -> "(1) Take P off the line and Q on PA (anywhere)"
      2 -> "(2) R = line QC ^ line PB"
      3 -> "(3) S = line AR ^ line BQ"
      4 -> "(4) Line PS meets AB at D"
      _ -> "(5) Cross-ratio (A,B;C,D) = -1. Move P and Q: D stays!"

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let i = nearestHandle [(wXa w, wYL w), (wXb w, wYL w), (wXc w, wYL w), wP w, qPt w] (mouseP5 env) 26
    in w { wDrag = if i >= 3 && wStage w < 1 then -1 else i }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (SpecialKey KeySpace) Down _ _
    | wStage w < 5 -> w { wStage = wStage w + 1, wProg = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> resetAll env
  _ -> w

-- ---------- やってみよう ----------
-- 1. 手順 5 まで進めてから C を AB の中点へ。D はどこへ行く？（矢印 = 無限遠）
-- 2. C を AB の外側に置くと D は AB の内側に来る。(A,B;C,D) = (A,B;D,C) であることを確かめよう
-- 3. P と Q をいろいろ動かして D が本当に動かないことを確かめる。定規だけ（長さを測らない）で決まる点
-- 4. 複比 −1 は射影で保たれる（04）。この図全体をホモグラフィ（03）で写しても D は D のまま

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
