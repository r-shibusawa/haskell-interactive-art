-- ============================================
-- 43-00 点と直線の双対
-- 左の点 (a, b) と右の直線 y = ax − b を入れ替える「双対変換」
--   点を動かすと右の直線が動く。右にマウスを置くと、その点の双対直線が左に現れる
--   3 点が一直線に並ぶ ↔ 3 本の直線が 1 点で交わる、上側の凸包 ↔ 下側の包絡線
-- 操作：ドラッグ = 点を動かす / クリック = 点を追加 / C = 一直線の 3 点 / H = 凸包と包絡線 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sort)
import Numeric (showFFloat)

rangeR :: Float
rangeR = 3              -- ★ 表示する範囲（−rangeR 〜 rangeR）

tol :: Float
tol = 0.03              -- ★ 「一直線」と判定する許しの幅

data World = World
  { wPts  :: ![Point]   -- 左の点 (a, b)（数学座標）
  , wHull :: !Bool
  , wDrag :: !Int       -- ドラッグ中の点の番号（なければ −1）
  }

startPts, linePts :: [Point]
startPts = [(-1.5, 1), (0.5, -0.8), (2, 1.6), (-0.6, -2)]
linePts = [(-2, -1.5), (0, -0.5), (2, 0.5), (1, 2)]

main :: IO ()
main = runSketch (sketch "43-00 Point-line duality" (World startPts False (-1)))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

-- パネル：左と右の枠（画面座標）
panels :: Env -> (Panel, Panel)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 150
  in (Panel 20 100 w h, Panel (40 + w) 100 w h)

-- 数学座標 ↔ 画面座標
toS :: Panel -> Float -> Float -> Point
toS p x y = (px p + (x + rangeR) / (2 * rangeR) * pw p, py p + (rangeR - y) / (2 * rangeR) * ph p)

toM :: Panel -> Point -> Point
toM p (sx, sy) = ((sx - px p) / pw p * 2 * rangeR - rangeR, rangeR - (sy - py p) / ph p * 2 * rangeR)

-- 直線 y = m x + k を枠の中に描く
drawLineIn :: Panel -> Float -> Float -> Float -> Float -> Float -> Picture
drawLineIn p m k hue alphaV w =
  case clipSeg p (toS p (-rangeR) (m * (-rangeR) + k)) (toS p rangeR (m * rangeR + k)) of
    Just (a, b) -> seg w (hsbA hue 70 100 alphaV) a b
    Nothing -> Blank

drawFrame :: Panel -> String -> Picture
drawFrame p title =
  let (ox, oy) = toS p 0 0
  in Pictures
       [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
       , seg 1 (hsb 0 0 30) (px p, oy) (px p + pw p, oy)
       , seg 1 (hsb 0 0 30) (ox, py p) (ox, py p + ph p)
       , txt (px p + 10) (py p + 22) 14 (hsb 0 0 70) title ]

hueOf :: Int -> Float
hueOf i = 10 + fromIntegral ((i * 37) `mod` 360)

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawFrame l "point (a, b)", drawFrame r "line y = a x - b" ]
  -- ★ 双対：点 i → 右の直線
  ++ [ drawLineIn r a (-b) (hueOf i) 80 (if i == wDrag w then 4 else 2) | (i, (a, b)) <- numbered ]
  ++ [ hullEnvelope l r pts | wHull w && length pts >= 3 ]
  ++ concatMap collinearMarks triples
  -- 点を描く
  ++ concat [ [ dot (hsb (hueOf i) 70 100) sx sy (if i == wDrag w then 16 else 12)
              , txt (sx + 10) (sy - 8) 14 (hsb 0 0 100) ("(" ++ fmt 1 a ++ ", " ++ fmt 1 b ++ ")") ]
            | (i, (a, b)) <- numbered, let (sx, sy) = toS l a b ]
  ++ mouseDual
  ++ [ txt 20 34 18 (hsb 0 0 100) ("(a, b) <-> y = ax - b   points " ++ show (length pts) ++ "   collinear triples " ++ show found
                                   ++ "   H: hull " ++ (if wHull w then "ON" else "off") ++ "   C: line   R: reset")
     , txt 20 60 14 (hsb 0 0 60) "Incidence survives duality: (a, b) lies on y = mx + k  <=>  (m, -k) lies on y = ax - b."
     , txt 20 82 14 (hsb 0 0 60) "So 3 collinear points <=> 3 lines through one point." ]
  where
    (l, r) = panels env
    pts = wPts w
    numbered = zip [0 :: Int ..] pts
    (mx, my) = mouseP env
    -- 一直線の 3 点 ↔ 共点の 3 直線（印は 3 組まで）
    triples = take 3
      [ (p, q)
      | (i, p) <- numbered, (j, q) <- numbered, (k, o) <- numbered, i < j, j < k
      , abs ((fst q - fst p) * (snd o - snd p) - (snd q - snd p) * (fst o - fst p)) < tol * 3 ]
    found = length triples
    collinearMarks ((pa, pb), (qa, qb)) =
      let dA = qa - pa
          m = (qb - pb) / (if dA == 0 then 1e-9 else dA)
          kk = pb - m * pa
          (sx, sy) = toS r m (-kk)
          left = case clipSeg l (toS l (-rangeR) (m * (-rangeR) + kk)) (toS l rangeR (m * rangeR + kk)) of
                   Just (a, b) -> dashPath 2 (hsbA 50 70 100 70) 6 6 [a, b]
                   Nothing -> Blank
      in [left, circ 3 (hsb 50 90 100) sx sy 22]
    -- マウスの双対：右の点 (x, y) → 左の直線 b = x a − y
    mouseDual
      | inside r (mx, my) =
          let (x, y) = toM r (mx, my)
              s = "(" ++ fmt 1 x ++ ", " ++ fmt 1 y ++ ") -> b = " ++ fmt 1 x ++ " a - (" ++ fmt 1 y ++ ")"
              tx = min (mx + 12) (envW env - 12 - 10 * fromIntegral (length s))
          in [ drawLineIn l x (-y) 180 90 2, dot (hsb 180 70 100) mx my 10
             , txt tx (my - 8) 14 (hsb 180 50 100) s ]
      | inside l (mx, my) && wDrag w < 0 =
          let (x, y) = toM l (mx, my) in [drawLineIn r x (-y) 180 60 1.5]
      | otherwise = []

-- ★ 上側の凸包（左）↔ 下側の包絡線（右）、下側の凸包 ↔ 上側の包絡線
hullEnvelope :: Panel -> Panel -> [Point] -> Picture
hullEnvelope l r pts = Pictures
  [ hullLine 10 (chain (reverse sorted)), hullLine 180 (chain sorted)
  , envelope 10 minimum, envelope 180 maximum
  , txt (px l + 10) (py l + ph l - 10) 14 (hsb 0 0 70) "coral: upper hull <-> lower envelope   aqua: lower hull <-> upper envelope" ]
  where
    sorted = sort pts
    colorOf hue = hsbA hue 80 100 80
    hullLine hue c = path 5 (colorOf hue) [toS l a b | (a, b) <- c]
    -- 包絡線：x を細かく動かして min / max を取る
    envelope hue pick = path 5 (colorOf hue)
      [ toS r x (clampF (-rangeR) rangeR (pick [a * x - b | (a, b) <- pts]))
      | k <- [0 .. 120 :: Int], let x = -rangeR + fromIntegral k * 0.05 ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (l, _) = panels env
        (mx, my) = mouseP env
        m = toM l (clampF (px l) (px l + pw l) mx, clampF (py l) (py l + ph l) my)
    in w { wPts = setAt (wDrag w) m (wPts w) }
  EventKey (Char k) Down _ _
    | k `elem` "hH" -> w { wHull = not (wHull w) }
    | k `elem` "rR" -> w { wPts = startPts }
    | k `elem` "cC" -> w { wPts = linePts }
  _ -> w

press :: Env -> World -> World
press env w
  | not (inside l m) = w
  | (i : _) <- near = w { wDrag = i }
  | length (wPts w) < 12 = w { wPts = wPts w ++ [toM l m], wDrag = length (wPts w) }
  | otherwise = w
  where
    (l, _) = panels env
    m = mouseP env
    near = [i | (i, (a, b)) <- zip [0 ..] (wPts w), dist2 m (toS l a b) < 14]

-- ---------- やってみよう ----------
-- 1. C で一直線の 3 点を出し、右で 3 本の直線が 1 点で交わるのを確認。1 点だけ少し動かすと？
-- 2. 右の交点 (m, −k) の m は左の直線の傾き。左の直線を立てていくと交点はどこへ行く？
-- 3. H を押し、点を 1 つ上に引っ張る。左の凸包が変わるとき右の包絡線はどう変わる？
-- 4. 双対変換を (a, b) → y = ax + b に変えると（drawLineIn の符号を変える）、上下の対応はどうなる？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 点線の折れ線（on だけ描いて off だけ空ける）
dashPath :: Float -> Color -> Float -> Float -> [Point] -> Picture
dashPath w c on off pts = Pictures (walk on True pts)
  where
    walk left drawing (a : b : rest)
      | l <= left = [seg w c a b | drawing] ++ walk (left - l) drawing (b : rest)
      | otherwise = [seg w c a m | drawing] ++ walk (if drawing then off else on) (not drawing) (m : b : rest)
      where
        l = dist2 a b
        t = left / l
        m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
    walk _ _ _ = []

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

inside :: Panel -> Point -> Bool
inside p (x, y) = x >= px p && x <= px p + pw p && y >= py p && y <= py p + ph p

-- 線分を枠で切る（Liang–Barsky）
clipSeg :: Panel -> Point -> Point -> Maybe (Point, Point)
clipSeg p (x1, y1) (x2, y2) = go 0 1 tests
  where
    dx = x2 - x1
    dy = y2 - y1
    tests = [(-dx, x1 - px p), (dx, px p + pw p - x1), (-dy, y1 - py p), (dy, py p + ph p - y1)]
    go t0 t1 [] = Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy))
    go t0 t1 ((a, q) : rest)
      | a == 0 = if q < 0 then Nothing else go t0 t1 rest
      | a < 0 = if r > t1 then Nothing else go (max t0 r) t1 rest
      | otherwise = if r < t0 then Nothing else go t0 (min t1 r) rest
      where r = q / a

-- 凸包の片側：左に曲がる点だけを残す
chain :: [Point] -> [Point]
chain = reverse . foldl' add []
  where
    add (b : a : rest) p | turn a b p <= 0 = add (a : rest) p
    add st p = p : st
    turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

-- リストの i 番目を入れかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]
