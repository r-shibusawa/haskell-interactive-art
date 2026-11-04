-- ============================================
-- 43-04 弱双対性
-- (P) の実行可能な x と (D) の実行可能な y をどう選んでも、必ず  c·x ≤ b·y
--   証明は 1 行：c·x ≤ (Aᵀy)·x = y·(Ax) ≤ y·b　（x ≥ 0, Aᵀy ≥ c, y ≥ 0, Ax ≤ b を使う）
--   ギャップ b·y − c·x が 0 になる組を見つけたら、それが両方の最適解
-- 操作：マウス = x（左の領域の中） / ←→ ↑↓ = y / O = y を最適解へ / 1〜3 = 例題
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

-- LP の係数（03 と同じ）
data Prob = Prob
  { c1, c2, a11, a12, a21, a22, b1, b2 :: !Double }

-- ★ 例題
preset :: Int -> Prob
preset 2 = Prob 2 5 1 2 3 1 6 7
preset 3 = Prob 4 1 2 1 1 4 5 8
preset _ = Prob 3 2 1 1 1 3 4 6

rangeR :: Double
rangeR = 8

ySpeed :: Double
ySpeed = 0.03           -- ★ 矢印キーで y が動く速さ

data World = World
  { wProb    :: !Prob
  , wX       :: !(Double, Double)
  , wY       :: !(Double, Double)
  , wBestGap :: !Double
  }

main :: IO ()
main = runSketch (sketch "43-04 Weak duality" (World (preset 1) (1, 1) (1, 1) (1 / 0)))
  { skBack  = hsb 250 40 10
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

panels :: Env -> (Panel, Panel)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 300
  in (Panel 20 100 w h, Panel (40 + w) 100 w h)

toS :: Panel -> Double -> Double -> Point
toS p x y = (px p + realToFrac (x / rangeR) * pw p, py p + ph p - realToFrac (y / rangeR) * ph p)

toM :: Panel -> Point -> (Double, Double)
toM p (sx, sy) = (realToFrac ((sx - px p) / pw p) * rangeR, realToFrac ((py p + ph p - sy) / ph p) * rangeR)

consP, consD :: Prob -> [Con]
consP q = [Con (a11 q) (a12 q) (b1 q) True, Con (a21 q) (a22 q) (b2 q) True]
consD q = [Con (a11 q) (a21 q) (c1 q) False, Con (a12 q) (a22 q) (c2 q) False]

feasibleP, feasibleD :: Prob -> (Double, Double) -> Bool
feasibleP q (x, y) = x >= -1e-9 && y >= -1e-9
  && a11 q * x + a12 q * y <= b1 q + 1e-9 && a21 q * x + a22 q * y <= b2 q + 1e-9
feasibleD q (x, y) = x >= -1e-9 && y >= -1e-9
  && a11 q * x + a21 q * y >= c1 q - 1e-9 && a12 q * x + a22 q * y >= c2 q - 1e-9

-- 3 つの値：c·x ≤ y·(Ax) ≤ b·y
values :: Prob -> (Double, Double) -> (Double, Double) -> (Double, Double, Double)
values q (x1, x2) (y1, y2) =
  ( c1 q * x1 + c2 q * x2
  , y1 * (a11 q * x1 + a12 q * x2) + y2 * (a21 q * x1 + a22 q * x2)
  , b1 q * y1 + b2 q * y2 )

-- 矢印キーで y を動かし、マウスが左の枠の中なら x を動かす
step :: Env -> Float -> World -> IO World
step env _ w =
  let (y1, y2) = wY w
      key k = isDown env (SpecialKey k)
      d a b = (if a then -ySpeed else 0) + (if b then ySpeed else 0)
      y' = ( clampD (y1 + d (key KeyLeft) (key KeyRight))
           , clampD (y2 + d (key KeyDown) (key KeyUp)) )
      (l, _) = panels env
      (mx, my) = mouseP env
      x' = if mx > px l && mx < px l + pw l && my > py l && my < py l + ph l then toM l (mx, my) else wX w
      q = wProb w
      (cx, _, by) = values q x' y'
      best = if feasibleP q x' && feasibleD q y' then min (wBestGap w) (by - cx) else wBestGap w
  in pure w { wX = x', wY = y', wBestGap = best }
  where clampD = max 0 . min rangeR

drawRegion :: Panel -> [Con] -> Float -> String -> (Double, Double) -> Bool -> String -> Picture
drawRegion p cons hue title (a, b) ok name = Pictures $
  [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
  , txt (px p + 10) (py p + 22) 14 (hsb 0 0 70) title ]
  ++ (if length region >= 3
        then [ Color (hsbA hue 70 100 22) (Polygon sp), closed 2 (hsbA hue 60 100 80) sp ]
        else [])
  ++ [ circ 3 (hsb (if ok then hue else 0) 90 100) sx sy 18 ]
  ++ [ dot (hsb hue 90 100) sx sy 10 | ok ]
  ++ [ txt tx (sy - 10) 14 (hsb 0 0 100) s ]
  where
    box0 = [(0, 0), (rangeR, 0), (rangeR, rangeR), (0, rangeR)]
    cut poly (Con a1 a2 r le) = if le then clipHalf poly a1 a2 r else clipHalf poly (-a1) (-a2) (-r)
    region = foldl' cut box0 cons
    sp = [toS p x y | (x, y) <- region]
    (sx, sy) = toS p (max 0 (min rangeR a)) (max 0 (min rangeR b))
    s = name ++ " = (" ++ fmt 2 a ++ ", " ++ fmt 2 b ++ ")" ++ (if ok then "" else "  X not feasible")
    tx = min (sx + 14) (px p + pw p - 10 * fromIntegral (length s))

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawRegion l (consP q) 10 "(P) feasible region: A x <= b, x >= 0. Mouse picks x" (wX w) okP "x"
  , drawRegion r (consD q) 180 "(D) feasible region: A^T y >= c, y >= 0. Arrows move y" (wY w) okD "y"
  -- 数直線
  , seg 2 (hsb 0 0 50) (nx0, ny) (nx1, ny) ]
  ++ concat [ [ seg 2 (hsb 0 0 40) (xOf v, ny - 5) (xOf v, ny + 5)
              , txt (xOf v - 4) (ny + 22) 14 (hsb 0 0 50) (show k) ]
            | k <- [0, 2 .. floor vmax :: Int], let v = fromIntegral k ]
  ++ (if lpOk opt
        then [ seg 2 (hsb 50 90 100) (xOf (lpVal opt), ny - 26) (xOf (lpVal opt), ny + 8)
             , txt (xOf (lpVal opt) - 30) (ny - 32) 14 (hsb 50 90 100) ("optimum " ++ fmt 2 (lpVal opt)) ]
        else [])
  ++ [ solid (hsbA (if gap < 0.2 then 120 else 300) 70 100 30) (xOf cx) (ny - 14) (xOf by - xOf cx) 28 | okP && okD ]
  ++ [ dot (if okP then hsb 10 90 100 else hsb 0 0 50) (xOf cx) ny 18
     , txt (xOf cx - 30) (ny + 42) 14 (if okP then hsb 10 90 100 else hsb 0 0 50) ("c.x = " ++ fmt 2 cx)
     , dot (if okD then hsb 180 90 100 else hsb 0 0 50) (xOf by) ny 18
     , txt (xOf by - 30) (ny + 62) 14 (if okD then hsb 180 90 100 else hsb 0 0 50) ("b.y = " ++ fmt 2 by)
     , dot (hsb 0 0 70) (xOf mid) ny 8
     , txt (xOf mid - 30) (ny - 52) 14 (hsb 0 0 70) ("y.Ax = " ++ fmt 2 mid)
     , txt 20 32 18 (hsb 0 0 100) (msg ++ "    O: y to optimum   1-3: examples")
     , txt 20 58 14 (hsb 0 0 60) "c.x <= (A^T y).x = y.(Ax) <= y.b  Left uses A^T y >= c and x >= 0, right uses Ax <= b and y >= 0."
     , txt 20 78 14 (hsb 0 0 60) "So the primal value never exceeds the dual value." ]
  where
    (l, r) = panels env
    q = wProb w
    okP = feasibleP q (wX w)
    okD = feasibleD q (wY w)
    (cx, mid, by) = values q (wX w) (wY w)
    gap = by - cx
    opt = solveLP2 (consP q) (c1 q, c2 q) True
    ny = envH env - 110
    nx0 = 60
    nx1 = envW env - 60
    vmax = max 10 (if lpOk opt then lpVal opt * 2.2 else 20)
    xOf v = nx0 + realToFrac (max 0 (min vmax v) / vmax) * (nx1 - nx0)
    msg | okP && okD && gap < 0.2 = "Gap " ++ fmt 2 gap ++ ": both are optimal!"
        | okP && okD = "Gap b.y - c.x = " ++ fmt 2 gap ++ " (best so far " ++ fmt 2 (wBestGap w) ++ ")"
        | otherwise = "Compare feasible points only (red ring = outside the region)"

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "123" -> w { wProb = preset (fromEnum k - fromEnum '0'), wBestGap = 1 / 0, wY = (1, 1) }
    | k `elem` "oO" ->
        let q = wProb w
            res = solveLP2 (consD q) (b1 q, b2 q) False
        in if lpOk res then w { wY = (lpX res, lpY res) } else w
  _ -> w

-- ---------- やってみよう ----------
-- 1. x を領域の頂点に置き、y を矢印で動かしてギャップを 0 にする。ギャップ 0 の y は 1 つだけ？
-- 2. わざと x を領域の外に出すと c·x が b·y を超えることがある。弱双対性は「実行可能」が前提
-- 3. 真ん中の y·Ax は必ず 2 つの値の間にある。等号になるのはどんなとき？（→ 05 相補性）
-- 4. 例題 2 と 3 でも試す。最適値に届かない y はどの制約がゆるい？

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

-- 左上が (x, y) の塗りつぶした長方形
solid :: Color -> Float -> Float -> Float -> Float -> Picture
solid c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 太さ w の閉じた折れ線
closed :: Float -> Color -> [Point] -> Picture
closed w c ps = path w c (ps ++ take 1 ps)

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

-- 多角形を半平面 nx*x + ny*y <= c で切る（Sutherland–Hodgman）
clipHalf :: (Ord a, Fractional a) => [(a, a)] -> a -> a -> a -> [(a, a)]
clipHalf ps nx ny c = concat (zipWith piece ps (drop 1 ps ++ take 1 ps))
  where
    piece a@(ax, ay) (bx, by) =
      let fa = nx * ax + ny * ay - c
          fb = nx * bx + ny * by - c
          t = fa / (fa - fb)
      in [a | fa <= 0] ++ [(ax + (bx - ax) * t, ay + (by - ay) * t) | (fa < 0 && fb > 0) || (fa > 0 && fb < 0)]

-- 制約 a1 x + a2 y <= b（True）または >= b（False）
data Con = Con !Double !Double !Double !Bool

-- LP の答え：実行可能か、頂点、値、非有界か
data LP = LP { lpOk :: !Bool, lpX :: !Double, lpY :: !Double, lpVal :: !Double, lpUnb :: !Bool }

-- 2 変数の頂点列挙（x >= 0, y >= 0 は自動で追加）。maximize = True なら最大化。
-- 判定の許しの幅 1e-7 が効くよう、Double で計算する
solveLP2 :: [Con] -> (Double, Double) -> Bool -> LP
solveLP2 cons (c1, c2) maximize = case foldl' pick Nothing cands of
  Nothing -> LP False 0 0 0 False
  Just (x, y, v) -> LP True x y v (x > big - 1 || y > big - 1)
  where
    big = 1e3
    allC = cons ++ [Con 1 0 0 False, Con 0 1 0 False]
    lns = zip [0 :: Int ..] ([(a, b, r) | Con a b r _ <- allC] ++ [(1, 0, big), (0, 1, big)])
    sat x y (Con a b r le) = let s = a * x + b * y in if le then s <= r + 1e-7 else s >= r - 1e-7
    cands = [ (x, y, c1 * x + c2 * y)
            | (i, (a1, a2, p)) <- lns, (j, (b1, b2, q)) <- lns, i < j
            , let det = a1 * b2 - a2 * b1, abs det >= 1e-9
            , let x = (p * b2 - q * a2) / det, let y = (a1 * q - b1 * p) / det
            , all (sat x y) allC, x <= big + 1e-7, y <= big + 1e-7 ]
    pick Nothing c = Just c
    pick best@(Just (_, _, bv)) c@(_, _, v)
      | if maximize then v > bv + 1e-9 else v < bv - 1e-9 = Just c
      | otherwise = best
