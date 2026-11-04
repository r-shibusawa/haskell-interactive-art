-- ============================================
-- 39-08 パスカルの定理とブリアンションの定理
-- 円錐曲線（楕円）上の 6 点 P1〜P6 を順に結んだ六角形：対辺の交点 P1P2∩P4P5, P2P3∩P5P6, P3P4∩P6P1 は 1 直線（パスカル線）上
--   双対（B）：楕円の 6 本の接線が作る六角形の対角線 3 本は 1 点（ブリアンション点）で交わる
--   パスカルの定理の「点 ↔ 直線」「〜上の点 ↔ 〜に接する直線」を入れかえるとブリアンションになる
-- 6 点をドラッグ（楕円の上を動く） / B = パスカル ↔ ブリアンション / ↑↓ = 楕円の太さ / ←→ = 回転 / S = 順番をシャッフル / R = リセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

th0 :: [Float]
th0 = [0.3, 1.2, 2.3, 3.3, 4.2, 5.4]   -- 6 点の楕円上の角度

phi0, ratio0 :: Float
phi0 = 0.3      -- ★ 楕円の回転
ratio0 = 0.6    -- ★ 短い半軸 = 長い半軸 × この値

data World = World
  { wTh    :: ![Float]
  , wEb    :: !Float     -- 短い半軸（長い半軸 ea は画面の大きさで決まる）
  , wPhi   :: !Float
  , wOrder :: ![Int]     -- 六角形の順番
  , wBrian :: !Bool
  , wDrag  :: !Int
  }

main :: IO ()
main = runSketch (sketch "39-08 Pascal and Brianchon" (World th0 180 phi0 [0 .. 5] False (-1)))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (World th0 (ea env * ratio0) phi0 [0 .. 5] False (-1))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

ea :: Env -> Float
ea env = min (envW env) (envH env) * 0.38

ctr :: Env -> Point
ctr env = (envW env * 0.5, envH env * 0.54)

-- 楕円上の点（角度 t）と、その点での接線の方向
ePt :: Env -> World -> Float -> Point
ePt env w t =
  let (cx, cy) = ctr env
      x = ea env * cos t
      y = wEb w * sin t
      p = wPhi w
  in (cx + x * cos p - y * sin p, cy + x * sin p + y * cos p)

eTan :: Env -> World -> Float -> Point
eTan env w t =
  let x = negate (ea env) * sin t
      y = wEb w * cos t
      p = wPhi w
  in (x * cos p - y * sin p, x * sin p + y * cos p)

angleOf :: Env -> World -> Point -> Float
angleOf env w (mx, my) =
  let (cx, cy) = ctr env
      dx = mx - cx
      dy = my - cy
      p = wPhi w
      x = dx * cos p + dy * sin p
      y = negate dx * sin p + dy * cos p
  in atan2 (y / wEb w) (x / ea env)

step :: Env -> World -> World
step env w
  | wDrag w >= 0 = w { wTh = setAt (wDrag w) (angleOf env w (mouseP5 env)) (wTh w) }
  | otherwise = w

hexThetas :: World -> [Float]
hexThetas w = map (wTh w !!) (wOrder w)

rotList :: [a] -> [a]
rotList xs = drop 1 xs ++ take 1 xs

draw :: Env -> World -> Picture
draw env w = view env $
  -- 楕円
  [ closedPath 2 (hsba 200 50 90 0.7) [ePt env w (fromIntegral i * tau / 96) | i <- [0 .. 95 :: Int]] ]
  ++ (if wBrian w then brianchon else pascal)
  -- 6 点
  ++ concat [ [dotAt (hsb 200 60 100) p 14, txt (fst p + 10) (snd p - 8) 15 white ("P" ++ show k)]
            | (k, p) <- zip [1 :: Int ..] ps ]
  ++ [ txt 20 50 14 (gray 0.8) ("Drag the 6 points (on the ellipse)   B: " ++ (if wBrian w then "to Pascal" else "to Brianchon") ++ "   Up/Down: width   Left/Right: turn   S: shuffle   R: reset")
     , txt 20 (envH env - 24) 14 (gray 0.8) (if wBrian w
         then "Point <-> tangent, side <-> vertex, collinear <-> concurrent. Pascal (1640, age 16), dual by Brianchon (1806)."
         else "Works for self-crossing hexagons too. Merge 2 points and that side becomes a tangent (tangent from 5 points).") ]
  where
    full = screenBox env
    ths = hexThetas w
    ps = map (ePt env w) ths           -- 六角形の順に並べた点
    red = hsb 15 80 100
    okLabel d word = if abs d < 1e-4 then word ++ " OK" else "..."
    -- パスカル：辺 i = P_i P_{i+1}。対辺は i と i+3
    pascal =
      let sides = zipWith lineThrough ps (rotList ps)
          xs = zipWith hCross (take 3 sides) (drop 3 sides)
          (pl, det) = case xs of
            [a, b, c] -> (hCross a b, hDet (hUnit a) (hUnit b) (hUnit c))
            _ -> (H3 0 0 0, 0)
      in [ hLine 1.5 (hsba 45 70 100 0.5) full s | s <- sides ]
         ++ zipWith (seg 3 (hsb 140 60 95)) ps (rotList ps)
         ++ [ hLine 3 red full pl ]
         ++ [ hPoint (arrowBox env) 16 n (hsba 15 80 100 0.6) red 2 x | (x, n) <- zip xs ["12^45", "23^56", "34^61"] ]
         ++ [ txt 20 28 18 white ("Pascal: det of the 3 meets of opposite sides = " ++ nf 6 (d2f det) ++ " -> " ++ okLabel det "collinear") ]
    -- ブリアンション：接線 t_i。六角形の頂点 V_i = t_i ∩ t_{i+1}。対角線 V_i V_{i+3}
    brianchon =
      let tg = zipWith (\p@(px, py) t -> let (dx, dy) = eTan env w t in lineThrough p (px + dx, py + dy)) ps ths
          vs = zipWith hCross tg (rotList tg)
          diag = zipWith hCross (take 3 vs) (drop 3 vs)
          (bp, det) = case diag of
            [a, b, c] -> (hCross a b, hDet (hUnit a) (hUnit b) (hUnit c))
            _ -> (H3 0 0 0, 0)
          yellow = hsb 45 90 100
      in [ hLine 1.5 (hsba 45 70 100 0.6) full t | t <- tg ]
         ++ [ seg 2 (hsb 140 60 95) a b | (Just a, Just b) <- zip (map toXY vs) (map toXY (rotList vs)) ]
         ++ [ hLine 3 red full d | d <- diag ]
         ++ [ hPoint (arrowBox env) 12 ("V" ++ show i) (hsba 45 90 100 0.6) yellow 2 v | (i, v) <- zip [1 :: Int ..] vs ]
         ++ [ hPoint (arrowBox env) 20 "B" red red 2 bp
            , txt 20 28 18 white ("Brianchon: det of the 3 diagonals = " ++ nf 6 (d2f det) ++ " -> " ++ okLabel det "concurrent") ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let i = nearestHandle (map (ePt env w) (hexThetas w)) (mouseP5 env) 26
    in pure w { wDrag = if i >= 0 then wOrder w !! i else -1 }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEb = min (ea env) (wEb w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEb = max 40 (wEb w - 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wPhi = wPhi w - 0.05 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wPhi = wPhi w + 0.05 }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> pure w { wBrian = not (wBrian w) }
    | c `elem` "sS" -> do    -- フィッシャー＝イェーツのシャッフル
        order <- foldM swapRandom (wOrder w) [5, 4 .. 1]
        pure w { wOrder = order }
    | c `elem` "rR" -> pure w { wTh = th0, wOrder = [0 .. 5], wPhi = phi0, wEb = ea env * ratio0 }
  _ -> pure w
  where
    swapRandom xs i = do
      j <- randomRIO (0, i)
      pure (setAt i (xs !! j) (setAt j (xs !! i) xs))

-- ---------- やってみよう ----------
-- 1. S で順番を変えて星形の六角形にしても、パスカル線はちゃんと 3 点を通る。順番は 60 通り（線は 60 本）
-- 2. 2 点を近づけて重ねる（P1 ≈ P2）。辺 P1P2 は接線に近づく。「5 点と 1 本の接線」でもパスカルは成り立つ
-- 3. ブリアンションで、六角形の頂点 V が無限遠に飛ぶ配置（接線が平行 = 2 点が楕円の反対側）を作る。対角線はまだ 1 点で交わる？
-- 4. 楕円の eb を ea と同じにして円にする。円の場合のパスカル線を 6 点を動かして観察し、07 のパップスと見比べよう

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
