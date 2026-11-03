-- ============================================
-- 01 1 回折りでできること（藤田・羽鳥の公理）
-- 「点や線を、別の点や線に重ねる」1 回の折りでつくれる折り線は 7 種類しかない（O1〜O7）
--   O6 は 3 次方程式を解くのと同じで、定規とコンパスにはできない作図ができる（02・03 で使う）
-- 1〜7 = 公理を選ぶ / ドラッグ = 点 P1・P2 と直線の端（四角）を動かす / P = 放物線を表示（O5・O6）/ R = 配置を戻す
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit, toLower, digitToInt)
import Data.List (foldl')

gridN :: Int
gridN = 1500                -- ★ O6 で答えを探すときの細かさ

-- つかめる点（P1, P2 と、直線 L1・L2 を決める 2 点ずつ）
data Grip = P1 | P2 | A1 | B1 | A2 | B2 deriving (Eq, Show)

data Handles = Handles { hP1, hP2, hA1, hB1, hA2, hB2 :: !Point }

data World = World
  { wAxiom :: !Int
  , wPara  :: !Bool             -- 放物線を見せるか
  , wH     :: !Handles
  , wDrag  :: !(Maybe Grip)
  }

-- 公理の名前・説明 1・説明 2
info :: Int -> (String, String, String)
info k = case k of
  1 -> ("O1", "Fold through 2 points P1, P2", "Exactly 1 line. Same as drawing with a ruler.")
  2 -> ("O2", "Put point P1 onto point P2", "Perpendicular bisector of P1 and P2. Exactly 1 line.")
  3 -> ("O3", "Put line L1 onto line L2", "Angle bisectors: 2 lines if they cross, 1 if parallel.")
  4 -> ("O4", "Through P1, perpendicular to L1", "Puts L1 onto itself. Exactly 1 line.")
  5 -> ("O5", "Through P2, put P1 onto L1", "Circle (center P2, radius |P1P2|) meets L1: 0 to 2 lines (quadratic).")
  6 -> ("O6", "Put P1 onto L1 and P2 onto L2", "Common tangents of 2 parabolas: 0 to 3 lines (a CUBIC equation!).")
  _ -> ("O7", "Put P1 onto L1, perpendicular to L2", "0 or 1 line.")

uses :: Int -> [Grip]
uses k = case k of
  1 -> [P1, P2]
  2 -> [P1, P2]
  3 -> [A1, B1, A2, B2]
  4 -> [P1, A1, B1]
  5 -> [P1, P2, A1, B1]
  6 -> [P1, P2, A1, B1, A2, B2]
  _ -> [P1, A1, B1, A2, B2]

main :: IO ()
main = runSketch (sketch "26-01 Huzita-Hatori Axioms" (World 1 True (layout 1280 800) Nothing))
  { skBack  = rgb255 34 32 40
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 紙の左上と大きさ
paperOf :: Float -> Float -> (Float, Float, Float)
paperOf w h = let ps = min (w * 0.62) (h - 160) in (w * 0.36 - ps / 2, 110, ps)

layout :: Float -> Float -> Handles
layout w h =
  let (px0, py0, ps) = paperOf w h
      p u v = (px0 + u * ps, py0 + v * ps)
  in Handles (p 0.3 0.25) (p 0.62 0.55) (p 0.05 0.85) (p 0.95 0.8) (p 0.15 0.1) (p 0.4 0.95)

getH :: Handles -> Grip -> Point
getH h g = case g of { P1 -> hP1 h; P2 -> hP2 h; A1 -> hA1 h; B1 -> hB1 h; A2 -> hA2 h; B2 -> hB2 h }

setH :: Grip -> Point -> Handles -> Handles
setH g v h = case g of
  P1 -> h { hP1 = v }
  P2 -> h { hP2 = v }
  A1 -> h { hA1 = v }
  B1 -> h { hB1 = v }
  A2 -> h { hA2 = v }
  B2 -> h { hB2 = v }

-- 解：折り線、点の行き先 [P, P の行き先]、円（O5）
data Sol = Sol [Ln] [(Point, Point)] (Maybe (Point, Float))

-- ★ 公理ごとの折り線を計算する
solve :: World -> Sol
solve w =
  let h = wH w
      p1 = hP1 h
      p2 = hP2 h
      l1@(Ln l1p l1u) = lineOf (hA1 h) (hB1 h)
      l2@(Ln l2p l2u) = lineOf (hA2 h) (hB2 h)
  in case wAxiom w of
    1 -> Sol [lineOf p1 p2] [] Nothing
    2 -> Sol [bisector p1 p2] [(p1, p2)] Nothing
    3 -> case intersect l1 l2 of
      Just x -> Sol [Ln x (nrm (fst l1u + fst l2u) (snd l1u + snd l2u)), Ln x (nrm (fst l1u - fst l2u) (snd l1u - snd l2u))] [] Nothing
      Nothing -> Sol [Ln ((fst l1p + fst l2p) / 2, (snd l1p + snd l2p) / 2) l1u] [] Nothing
    4 -> Sol [Ln p1 (-(snd l1u), fst l1u)] [] Nothing
    5 ->
      let rad = dist2 p1 p2
          t0 = (fst p2 - fst l1p) * fst l1u + (snd p2 - snd l1p) * snd l1u   -- P2 から L1 へ下ろした足
          d = sdist p2 l1
          xs | rad * rad >= d * d =
                 let hh = sqrt (rad * rad - d * d)
                 in [onLine l1 (t0 + s) | s <- if hh < 1e-6 then [0] else [-hh, hh]]
             | otherwise = []
      in Sol [bisector p1 x | x <- xs] [(p1, x) | x <- xs] (Just (p2, rad))
    6 ->
      let xs = solveO6 p1 p2 l1 l2
          crs = [bisector p1 x | x <- xs]
      in Sol crs (concat [[(p1, x), (p2, refl p2 cr)] | (x, cr) <- zip xs crs]) Nothing
    _ ->
      let n1 = (-(snd l1u), fst l1u)
          den = dot2 l2u n1
      in if abs den > 1e-6
           then
             let s = -(((fst p1 - fst l1p) * fst n1 + (snd p1 - snd l1p) * snd n1)) / den
                 x = (fst p1 + fst l2u * s, snd p1 + snd l2u * s)
             in if s * s > 1e-6 then Sol [bisector p1 x] [(p1, x)] Nothing else Sol [] [] Nothing
           else Sol [] [] Nothing

-- O6：L1 の上の点 X(t) に P1 を乗せる折り線のうち、P2 の行き先が L2 に乗るものを探す
--     （符号が変わる所を二分法で）
solveO6 :: Point -> Point -> Ln -> Ln -> [Point]
solveO6 p1 p2 l1 l2 = reverse (snd (foldl' stepT ((lo, f lo), []) [1 .. gridN]))
  where
    lo = -3000 :: Float
    hi = 3000 :: Float
    f t = let x = onLine l1 t
          in if dist2 x p1 < 1e-6 then 0 / 0 else sdist (refl p2 (bisector p1 x)) l2
    good v = not (isNaN v || isInfinite v)
    stepT ((pt, pf), acc) i =
      let t = lo + (hi - lo) * fromIntegral i / fromIntegral gridN
          v = f t
          found = good v && good pf && pf * v <= 0 && abs (pf - v) < 2000
          acc' = if found then onLine l1 (bisect pt t pf (40 :: Int)) : acc else acc
      in acc' `seq` ((t, v), acc')
    bisect a b _ 0 = (a + b) / 2
    bisect a b fa k =
      let m = (a + b) / 2
          fm = f m
      in if fa * fm <= 0 then bisect a m fa (k - 1) else bisect m b fm (k - 1)

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let (px0, py0, ps) = paperOf (envW env) (envH env)
      h = wH w
      ax = wAxiom w
      Sol lns imgs circ = solve w
      use = uses ax
      (cx, cy) = sc env (px0 + ps / 2, py0 + ps / 2)
      paper = Color (rgb255 246 240 226) (rect cx cy ps ps)
      -- 放物線（焦点 P・準線 L の点の集まり）：P を L に乗せる折り線は、この放物線の接線
      paras | wPara w && ax == 5 = [parabola env (hP1 h) (lineOf (hA1 h) (hB1 h)) (rgba255 230 120 90 120)]
            | wPara w && ax == 6 = [ parabola env (hP1 h) (lineOf (hA1 h) (hB1 h)) (rgba255 230 120 90 120)
                                   , parabola env (hP2 h) (lineOf (hA2 h) (hB2 h)) (rgba255 80 140 230 120) ]
            | otherwise = []
      circPic = case circ of
        Just (c, r) -> [Color (rgba255 120 120 120 150) (at (sc env c) (ring r 1))]
        Nothing -> []
      -- 与えられた直線
      given = [Color (rgb255 200 70 50) (infLine env 3 (lineOf (hA1 h) (hB1 h))) | A1 `elem` use]
           ++ [Color (rgb255 50 100 200) (infLine env 3 (lineOf (hA2 h) (hB2 h))) | A2 `elem` use]
      -- 折り線（紙の中だけ太く）
      cols = cycle [(255, 190, 40), (80, 210, 140), (200, 110, 255)]
      folds = concat
        [ Color (rgba255 r g b 90) (infLine env 1.5 l)
          : [Color (rgb255 r g b) (thickLine 5 (sc env a) (sc env c)) | Just (a, c) <- [clipToPaper (px0, py0, ps) l]]
        | ((r, g, b), l) <- zip cols lns ]
      -- 点の行き先（折ると P → X）
      images = concat
        [ [ Color (rgba255 60 60 60 160) (dashed 1.5 [5, 5] (sc env a) (sc env b))
          , Color (rgb255 255 190 40) (at (sc env b) (disc 5)) ]
        | (a, b) <- imgs ]
      -- つかめる点
      grips = map (grip env h) use
      -- 説明パネル
      (nm, d1, d2) = info ax
      tx = px0 + ps + 40
      boxW = envW env - tx - 20
      panel = Pictures $
        [ txt env tx (py0 + 40) 44 (rgb255 255 190 40) nm ]
        ++ textBox env tx (py0 + 66) boxW 20 white d1
        ++ textBox env tx (py0 + 130) boxW 16 (gray 0.75) d2
        ++ [ txt env tx (py0 + 230) 26 white ("Fold lines: " ++ show (length lns)) ]
        ++ [ let (n, d, _) = info k
             in txt env tx (py0 + 280 + fromIntegral (k - 1) * 24) 14 (if k == ax then white else gray 0.55) (n ++ "  " ++ d)
           | k <- [1 .. 7] ]
        ++ textBox env tx (py0 + 280 + 7 * 24) boxW 14 (gray 0.6) "O1-O6: Humiaki Huzita (1989). O7 added by Koshiro Hatori (2001)."
      hud = Pictures
        [ txt env 20 40 18 white ("1-7: axiom   Drag: P1, P2, line ends (squares)   P: parabola " ++ (if wPara w then "ON" else "off") ++ "   R: reset")
        , txt env 20 68 15 (gray 0.67) "Only 7 ways to put points and lines onto points and lines. They construct more than ruler and compass."
        ]
  in Pictures ([paper] ++ paras ++ circPic ++ given ++ folds ++ images ++ grips ++ [panel, hud])

grip :: Env -> Handles -> Grip -> Picture
grip env h g =
  let p@(x, y) = getH h g
      (a, b) = sc env p
  in case g of
    P1 -> pointPic "P1"
    P2 -> pointPic "P2"
    A1 -> Color (rgb255 200 70 50) (rect a b 14 14)
    B1 -> Color (rgb255 200 70 50) (rect a b 14 14)
    _  -> Color (rgb255 50 100 200) (rect a b 14 14)
  where
    pointPic s =
      let p@(x, y) = getH h g
      in Pictures [Color (gray 0.12) (at (sc env p) (disc 8)), txt env (x + 10) (y - 10) 14 (gray 0.12) s]

parabola :: Env -> Point -> Ln -> Color -> Picture
parabola env f@(fx, fy) ln@(Ln (lx, ly) (ux, uy)) c =
  -- 準線 L の上の点 X から垂直に h だけ離れた点：|Y − F| = h  →  h = (t² + d²) / (2d)
  let d = sdist f ln
      t0 = (fx - lx) * ux + (fy - ly) * uy
      pt t = let hh = (t * t + d * d) / (2 * d)
                 (xx, xy) = onLine ln (t0 + t)
             in sc env (xx - uy * hh, xy + ux * hh)
  in if abs d < 1 then Blank else Color c (thickPath 2 (map pt [-900, -888 .. 900]))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        pick (best, bd) g = let d = dist2 m (getH (wH w) g) in if d < bd then (Just g, d) else (best, bd)
    in w { wDrag = fst (foldl pick (Nothing, 22) (uses (wAxiom w))) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventMotion _ -> case wDrag w of
    Just g ->
      let (mx, my) = mouseXY env
      in w { wH = setH g (clampF 0 (envW env) mx, clampF 90 (envH env) my) (wH w) }
    Nothing -> w
  EventKey (Char c) Down _ _
    | isDigit c && c >= '1' && c <= '7' -> w { wAxiom = digitToInt c }
    | toLower c == 'p' -> w { wPara = not (wPara w) }
    | toLower c == 'r' -> w { wH = layout (envW env) (envH env) }
  EventResize _ -> w { wH = layout (envW env) (envH env) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. O5 で P1 を円の外へ動かすと折り線が 0 本になる。2 本 → 1 本 → 0 本の境目はどんなとき？
-- 2. O6 で折り線が 3 本になる配置を探そう。3 次方程式の解が 3 つある状態
-- 3. O6 で P を動かすと放物線も動く。折り線が「2 つの放物線の両方に接する線」になっていることを確かめよう
-- 4. O3 で L1 と L2 を平行にすると？ O7 で L1 と L2 を平行にすると？

-- ==================================================
-- ここから下は「直線と鏡映の道具」と画面の道具
-- ==================================================

-- 直線：通る点と向き（長さ 1）。座標は p5.js と同じ「左上が原点、y は下向き」
data Ln = Ln !Point !Point

nrm :: Float -> Float -> Point
nrm x y = let l0 = sqrt (x * x + y * y)
              l = if l0 == 0 then 1 else l0
          in (x / l, y / l)

lineOf :: Point -> Point -> Ln
lineOf a@(ax, ay) (bx, by) = Ln a (nrm (bx - ax) (by - ay))

onLine :: Ln -> Float -> Point
onLine (Ln (px, py) (ux, uy)) t = (px + ux * t, py + uy * t)

bisector :: Point -> Point -> Ln
bisector (ax, ay) (bx, by) = Ln ((ax + bx) / 2, (ay + by) / 2) (nrm (-(by - ay)) (bx - ax))

sdist :: Point -> Ln -> Float
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

refl :: Point -> Ln -> Point
refl q@(qx, qy) ln@(Ln _ (ux, uy)) = let s = sdist q ln in (qx + 2 * s * uy, qy - 2 * s * ux)

intersect :: Ln -> Ln -> Maybe Point
intersect l1@(Ln (ax, ay) (ux, uy)) (Ln (bx, by) (vx, vy)) =
  let den = ux * vy - uy * vx
  in if abs den < 1e-6 then Nothing
     else Just (onLine l1 (((bx - ax) * vy - (by - ay) * vx) / den))

infLine :: Env -> Float -> Ln -> Picture
infLine env w (Ln (px, py) (ux, uy)) = thickLine w (sc env (px - ux * 3000, py - uy * 3000)) (sc env (px + ux * 3000, py + uy * 3000))

clipToPaper :: (Float, Float, Float) -> Ln -> Maybe (Point, Point)  -- 紙（正方形）と直線が交わる線分
clipToPaper (x0, y0, ps) ln =
  let sq = [(x0, y0), (x0 + ps, y0), (x0 + ps, y0 + ps), (x0, y0 + ps)]
      es = zip sq (drop 1 sq ++ take 1 sq)
      hits = [ (ax + (bx - ax) * t, ay + (by - ay) * t)
             | (a@(ax, ay), b@(bx, by)) <- es
             , let sa = sdist a ln
             , let sb = sdist b ln
             , sa * sb <= 0 && sa /= sb
             , let t = sa / (sa - sb) ]
  in case hits of
       a : b : _ -> Just (a, b)
       _ -> Nothing

-- p5.js の座標（左上が原点、y は下向き）→ gloss の座標
sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

-- 幅に合わせて折り返した文字（y は箱の上端）
textBox :: Env -> Float -> Float -> Float -> Float -> Color -> String -> [Picture]
textBox env x y w size c s =
  [ txt env x (y + size + fromIntegral i * size * 1.3) size c l | (i, l) <- zip [0 :: Int ..] (wrapW w size s) ]

wrapW :: Float -> Float -> String -> [String]
wrapW w size = go "" . words
  where
    maxC = floor (w / (size * 0.72)) :: Int
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= maxC = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

dashed :: Float -> [Float] -> Point -> Point -> Picture
dashed w pat a@(ax, ay) (bx, by) =
  let l = dist2 a (bx, by)
      pt s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
      go s (on : off : rest)
        | s >= l = []
        | otherwise = thickLine w (pt s) (pt (min l (s + on))) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then Blank else Pictures (go 0 (cycle pat))
