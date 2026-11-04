-- ============================================
-- 43-03 2 変数 LP の主問題と双対問題
-- 主問題 (P)：max c·x  s.t. A x ≤ b, x ≥ 0　　双対問題 (D)：min b·y  s.t. Aᵀ y ≥ c, y ≥ 0
--   左に (P) の実行可能領域と目的関数の等高線、右に (D) の同じもの。最適値はぴったり一致する（強双対性）
--   制約の係数（b や A）を変えると両方の領域が同時に変わる
-- 操作：←→ = 係数を選ぶ / ↑↓ = 値を変える / 1〜3 = 例題 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

-- ★ 係数の並び：c1 c2 a11 a12 b1 a21 a22 b2
preset :: Int -> [Double]
preset 2 = [2, 5, 1, 2, 6, 3, 1, 7]
preset 3 = [4, 1, 2, 1, 5, 1, 4, 8]
preset _ = [3, 2, 1, 1, 4, 1, 3, 6]

names :: [String]
names = ["c1", "c2", "a11", "a12", "b1", "a21", "a22", "b2"]

rangeR :: Double
rangeR = 8                 -- ★ 各パネルの表示範囲（0〜rangeR）

data World = World
  { wP   :: ![Double]      -- 8 個の係数
  , wSel :: !Int           -- いま選んでいる係数（0〜7）
  }

main :: IO ()
main = runSketch (sketch "43-03 LP primal and dual" (World (preset 1) 0))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

panels :: Env -> (Panel, Panel)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 230
  in (Panel 20 100 w h, Panel (40 + w) 100 w h)

toS :: Panel -> Double -> Double -> Point
toS p x y = (px p + realToFrac (x / rangeR) * pw p, py p + ph p - realToFrac (y / rangeR) * ph p)

-- 主問題と双対問題の制約
consOf :: [Double] -> ([Con], [Con])
consOf [_, _, a11, a12, b1, a21, a22, b2] =
  ([Con a11 a12 b1 True, Con a21 a22 b2 True], [])
consOf _ = ([], [])

dualOf :: [Double] -> [Con]
dualOf [c1, c2, a11, a12, _, a21, a22, _] = [Con a11 a21 c1 False, Con a12 a22 c2 False]
dualOf _ = []

-- パネルに LP を描く：制約、目的関数、最適解
drawLP :: Panel -> [Con] -> (Double, Double) -> LP -> Float -> [String] -> String -> Picture
drawLP p cons cc res hue titles varName = Pictures $
  [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p) ]
  ++ [ txt (px p + 10) (py p + 22 + 18 * k) 14 (hsb 0 0 70) t | (k, t) <- zip [0 ..] titles ]
  -- 実行可能領域：箱を半平面で切る
  ++ [ Color (hsbA hue 70 100 22) (Polygon [toS p x y | (x, y) <- region]) | length region >= 3 ]
  -- 制約の直線
  ++ [ seg 2 (hsbA hue 60 100 80) (s a) (s b) | Con a1 a2 r _ <- cons, Just (a, b) <- [lineInBox (a1, a2) r] ]
  -- 目的関数の等高線（点線）と最適の等高線
  ++ (if lpOk res then optimum else [])
  ++ concat [ [ txt (sx - 3) (py p + ph p + 16) 14 (hsb 0 0 50) (show v)
              , txt (px p - 15) (ty + 5) 14 (hsb 0 0 50) (show v) ]
            | v <- [0, 2, 4, 6, 8 :: Int]
            , let (sx, _) = toS p (fromIntegral v) 0
            , let (_, ty) = toS p 0 (fromIntegral v) ]
  where
    s (x, y) = toS p x y
    box0 = [(0, 0), (rangeR, 0), (rangeR, rangeR), (0, rangeR)]
    cut poly (Con a1 a2 r le) = if le then clipHalf poly a1 a2 r else clipHalf poly (-a1) (-a2) (-r)
    region = foldl' cut box0 cons
    vmax = max 1e-6 (fst cc * rangeR + snd cc * rangeR)
    optimum =
      [ dashPath 1 (hsbA 50 60 100 50) 4 6 [s a, s b]
      | k <- [1 .. 5 :: Int], Just (a, b) <- [lineInBox cc (vmax * fromIntegral k / 6)] ]
      ++ [ seg 3 (hsb 50 90 100) (s a) (s b) | Just (a, b) <- [lineInBox cc (lpVal res)] ]
      ++ [ dot (hsb 50 90 100) ox oy 16
         , txt (ox + 12) (oy - 10) 14 (hsb 0 0 100)
             (varName ++ " = (" ++ fmt 2 (lpX res) ++ ", " ++ fmt 2 (lpY res) ++ ")  value " ++ fmt 2 (lpVal res)) ]
    (ox, oy) = toS p (lpX res) (lpY res)

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawLP l consP (c1, c2) rp 10 titleP "x", drawLP r consD (b1, b2) rd 180 titleD "y"
  -- 係数パネル
  , solid (hsbA 0 0 100 8) 20 (yy - 30) (envW env - 40) 85 ]
  ++ [ txt (40 + fromIntegral i * 120) yy 16
         (if i == wSel w then hsb 50 90 100 else hsb 0 0 70) (nm ++ " = " ++ num v)
     | (i, nm, v) <- zip3 [0 :: Int ..] names ps ]
  ++ [ txt 40 (yy + 30) 14 (hsb 0 0 60) "Left/Right picks a coefficient, Up/Down changes it. Larger b widens (P) and weighs more on the objective of (D)."
     , txt 20 32 18 (hsb 0 0 100) (msg ++ "    1-3: examples   R: reset")
     , txt 20 58 14 (hsb 0 0 60) "Constraints of (P) = variables of (D), variables of (P) = constraints of (D)."
     , txt 20 78 14 (hsb 0 0 60) "A is transposed, c and b swap. max <-> min, <= <-> >=" ]
  where
    (l, r) = panels env
    yy = envH env - 95
    ps = wP w
    (c1, c2, a11, a12, b1, a21, a22, b2) = case ps of
      [a, b, c, d, e, f, g, h] -> (a, b, c, d, e, f, g, h)
      _ -> (0, 0, 0, 0, 0, 0, 0, 0)
    consP = fst (consOf ps)
    consD = dualOf ps
    rp = solveLP2 consP (c1, c2) True
    rd = solveLP2 consD (b1, b2) False
    titleP = [ "(P) max " ++ num c1 ++ "x1 + " ++ num c2 ++ "x2"
             , "s.t. " ++ num a11 ++ "x1 + " ++ num a12 ++ "x2 <= " ++ num b1 ++ ",  " ++ num a21 ++ "x1 + " ++ num a22 ++ "x2 <= " ++ num b2 ++ ",  x >= 0" ]
    titleD = [ "(D) min " ++ num b1 ++ "y1 + " ++ num b2 ++ "y2"
             , "s.t. " ++ num a11 ++ "y1 + " ++ num a21 ++ "y2 >= " ++ num c1 ++ ",  " ++ num a12 ++ "y1 + " ++ num a22 ++ "y2 >= " ++ num c2 ++ ",  y >= 0" ]
    msg | not (lpOk rp) = "(P) has no feasible point -> (D) is unbounded (or infeasible)"
        | lpUnb rp = "(P) is unbounded -> (D) has no feasible point"
        | not (lpOk rd) = "(D) has no feasible point"
        | otherwise = "Optimum: (P) " ++ fmt 2 (lpVal rp) ++ " = (D) " ++ fmt 2 (lpVal rd)
                      ++ (if abs (lpVal rp - lpVal rd) < 1e-4 then "  OK strong duality" else "")

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` 8 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w + 7) `mod` 8 }
  EventKey (SpecialKey KeyUp) Down _ _ -> bump 1
  EventKey (SpecialKey KeyDown) Down _ _ -> bump (-1)
  EventKey (Char k) Down _ _
    | k `elem` "123" -> w { wP = preset (fromEnum k - fromEnum '0') }
    | k `elem` "rR" -> w { wP = preset 1 }
  _ -> w
  where
    bump d = w { wP = [if i == wSel w then max 0 (min 12 (v + d)) else v | (i, v) <- zip [0 ..] (wP w)] }

-- ---------- やってみよう ----------
-- 1. b1 を 1 ずつ増やすと最適値はいくつずつ増える？ その増え方が双対の最適解 y1（影の価格）
-- 2. c1 を大きくしていくと (P) の最適な頂点が移る。そのとき (D) の最適な頂点も移る？
-- 3. a11 = a12 = 0 にして b1 = 0 にすると？ (P) の実行可能領域と (D) の非有界を観察
-- 4. 例題 2 の最適解で、きつい制約（等号で成り立つ）と y の正負の関係を見よう（→ 05）

-- ---------- 道具 ----------
-- 整数なら小数点なしで書く
num :: Double -> String
num v = let k = round v :: Int in if fromIntegral k == v then show k else fmt 2 v

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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

-- 直線 a·x = v を箱 [0, rangeR]² で切った 2 端点
lineInBox :: (Double, Double) -> Double -> Maybe ((Double, Double), (Double, Double))
lineInBox (a1, a2) v = case xs ++ ys of
  (p : rest@(_ : _)) -> Just (p, last rest)
  _ -> Nothing
  where
    xs = if abs a2 > 1e-9 then [(x, y) | x <- [0, rangeR], let y = (v - a1 * x) / a2, y >= -1e-9, y <= rangeR + 1e-9] else []
    ys = if abs a1 > 1e-9 then [(x, y) | y <- [0, rangeR], let x = (v - a2 * y) / a1, x > 1e-9, x < rangeR - 1e-9] else []
