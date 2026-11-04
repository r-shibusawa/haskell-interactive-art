-- ============================================
-- 43-05 相補性条件と影の価格
-- 最適解では「ゆるい制約（余りがある）の双対変数は 0」「きつい制約（等号）の双対変数は正」
--   y_i × (b_i − a_i·x*) = 0　　y_i = 制約 i の右辺 b_i を 1 増やしたときの最適値の増え方（影の価格）
--   3 本の制約（資源）をドラッグして平行移動すると、どの資源が「足りない」かが y で見える
-- 操作：制約の線をドラッグ = b_i を変える / 1〜2 = 例題 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

-- ★ 目的関数（利益）、3 本の制約（資源の使い方）、資源の量（ドラッグで変わる）
data Prob = Prob { pC :: !(Double, Double), pA :: ![(Double, Double)], pB :: ![Double] }

preset :: Int -> Prob
preset 2 = Prob (1, 4) [(2, 1), (1, 2), (1, 5)] [8, 7, 12]
preset _ = Prob (3, 2) [(1, 1), (1, 3), (3, 1)] [4, 6, 9]

rangeR :: Double
rangeR = 8

data World = World { wProb :: !Prob, wDrag :: !Int }

main :: IO ()
main = runSketch (sketch "43-05 Complementary slackness" (World (preset 1) (-1)))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

panel0 :: Env -> Panel
panel0 env = Panel 20 100 (envW env * 0.55) (envH env - 150)

toS :: Panel -> Double -> Double -> Point
toS p x y = (px p + realToFrac (x / rangeR) * pw p, py p + ph p - realToFrac (y / rangeR) * ph p)

toM :: Panel -> Point -> (Double, Double)
toM p (sx, sy) = (realToFrac ((sx - px p) / pw p) * rangeR, realToFrac ((py p + ph p - sy) / ph p) * rangeR)

consOf :: [(Double, Double)] -> [Double] -> [Con]
consOf as bs = [Con a1 a2 r True | ((a1, a2), r) <- zip as bs]

hues :: [Float]
hues = [10, 180, 300]

draw :: Env -> World -> Picture
draw env w = view env $
  [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p) ]
  -- 実行可能領域
  ++ [ Color (hsbA 10 70 100 22) (Polygon [toS p x y | (x, y) <- region]) | length region >= 3 ]
  -- 制約の直線（ゆるい = 細い、きつい = 太い）
  ++ concat [ conLine i a r s h | (i, a, r, s, h) <- zip5' [0 ..] as bs slack hues ]
  ++ (if lpOk rp
        then [ dashPath 2 (hsb 50 90 100) 6 6 [toS p x1 y1, toS p x2 y2] | Just ((x1, y1), (x2, y2)) <- [lineInBox (pC q) (lpVal rp)] ]
             ++ [ dot (hsb 50 90 100) ox oy 16
                , txt (ox + 12) (oy - 10) 14 (hsb 0 0 100)
                    ("x* = (" ++ fmt 2 (lpX rp) ++ ", " ++ fmt 2 (lpY rp) ++ ")  profit " ++ fmt 2 (lpVal rp)) ]
        else [])
  ++ concat [ [ txt (sx - 3) (py p + ph p + 16) 14 (hsb 0 0 50) (show v)
              , txt (px p - 15) (ty + 5) 14 (hsb 0 0 50) (show v) ]
            | v <- [0, 2, 4, 6, 8 :: Int]
            , let (sx, _) = toS p (fromIntegral v) 0
            , let (_, ty) = toS p 0 (fromIntegral v) ]
  -- 右：双対変数のバーと相補性のチェック
  ++ [ txt bx (by0 - 10) 16 (hsb 0 0 100) "Dual y (shadow price) and slack" ]
  ++ concat [ priceBar i y s d h | (i, y, s, d, h) <- zip5' [0 ..] yd slack diffs hues ]
  ++ [ txt bx (by0 + 30 + 3 * 150) 14 (hsb 0 0 70)
         ("dual value b.y = " ++ fmt 2 (sum (zipWith (*) bs yd)) ++ (if lpOk rp then "  = primal optimum " ++ fmt 2 (lpVal rp) else ""))
     , txt 20 32 18 (hsb 0 0 100) "Drag a constraint line to change resource b    1-2: examples    R: reset"
     , txt 20 58 14 (hsb 0 0 60) "Only lines through the optimum (tight) get a price y. A spare resource adds no profit (price 0)."
     , txt 20 78 14 (hsb 0 0 60) "y_i x slack_i = 0 is complementary slackness." ]
  where
    p = panel0 env
    q = wProb w
    as = pA q
    bs = pB q
    rp = solveLP2 (consOf as bs) (pC q) True
    yd = solveDual3 as bs (pC q)        -- 双対変数 y（影の価格）
    -- 影の価格を差分でも確かめる：b_i を 1 増やしたときの最適値の差
    diffs = [ let r2 = solveLP2 (consOf as [if k == i then v + 1 else v | (k, v) <- zip [0 :: Int ..] bs]) (pC q) True
              in if lpOk r2 && lpOk rp then lpVal r2 - lpVal rp else 0
            | i <- [0 .. length bs - 1] ]
    box0 = [(0, 0), (rangeR, 0), (rangeR, rangeR), (0, rangeR)]
    region = foldl' (\poly ((a1, a2), r) -> clipHalf poly a1 a2 r) box0 (zip as bs)
    slack = if lpOk rp then [r - (a1 * lpX rp + a2 * lpY rp) | ((a1, a2), r) <- zip as bs] else map (const 0) bs
    (ox, oy) = toS p (lpX rp) (lpY rp)
    conLine i a r s hue = case lineInBox a r of
      Nothing -> []
      Just ((x1, y1), (x2, y2)) ->
        let tight = s < 1e-6
            s1@(ax, ay) = toS p x1 y1
            s2@(cx, cy) = toS p x2 y2
            wt | i == wDrag w = 7
               | tight = 5
               | otherwise = 2
        in [ seg wt (hsbA hue 70 100 (if tight then 100 else 45)) s1 s2
           , txt (ax + (cx - ax) * 0.3 + 8) (ay + (cy - ay) * 0.3 - 8) 14 (hsb hue 60 100)
               ("R" ++ show (i + 1) ++ ": " ++ num (fst a) ++ "x1 + " ++ num (snd a) ++ "x2 <= " ++ fmt 1 r) ]
    bx = px p + pw p + 30
    bw = envW env - bx - 30
    by0 = 130
    ymax = maximum (1 : map abs yd)
    priceBar i y s d hue =
      let yy = by0 + 30 + fromIntegral i * 150
          tight = s < 1e-6
          nm = show (i + 1 :: Int)
      in [ txt bx yy 14 (hsb hue 60 100)
             ("Resource " ++ nm ++ "  " ++ (if tight then "tight (equality)" else "slack (spare " ++ fmt 2 s ++ ")"))
         , solid (hsb 0 0 25) bx (yy + 10) bw 24
         , solid (hsb hue 80 100) bx (yy + 10) (bw * realToFrac (max 0 (min 1 (y / ymax)))) 24
         , txt (bx + 6) (yy + 28) 14 (hsb 0 0 100) ("y" ++ nm ++ " = " ++ fmt 2 y)
         , txt bx (yy + 56) 14 (hsb 0 0 70)
             ("y" ++ nm ++ " x slack = " ++ fmt 2 y ++ " x " ++ fmt 2 s ++ " = " ++ fmt 2 (y * s) ++ (if abs (y * s) < 1e-3 then "  OK" else ""))
         , txt bx (yy + 78) 14 (hsb 0 0 70)
             ("b" ++ nm ++ " +1: profit +" ++ fmt 2 d ++ (if abs (d - y) < 0.05 then "  = y" ++ nm ++ " OK" else "  (vertex moved)")) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = toM p (mouseP env)
        cands = [ (d, i)
                | (i, (a1, a2), r) <- zip3 [0 ..] (pA q) (pB q)
                , let d = abs (a1 * mx + a2 * my - r) / sqrt (a1 * a1 + a2 * a2), d < 0.35 ]
    in w { wDrag = snd (foldl' (\b c -> if fst c < fst b then c else b) (1e9, wDrag w) cands) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = toM p (mouseP env)
        upd i r (a1, a2)
          | i == wDrag w = max 0.5 (min 30 (fromIntegral (round ((a1 * mx + a2 * my) * 2) :: Int) / 2))
          | otherwise = r
    in w { wProb = q { pB = zipWith3 upd [0 ..] (pB q) (pA q) } }
  EventKey (Char k) Down _ _
    | k `elem` "12" -> w { wProb = preset (fromEnum k - fromEnum '0') }
    | k `elem` "rR" -> w { wProb = preset 1 }
  _ -> w
  where
    p = panel0 env
    q = wProb w

-- ---------- やってみよう ----------
-- 1. 資源 3（紫）を遠くへ動かすと y3 = 0。近づけて最適解に触れた瞬間に y3 > 0 になる
-- 2. 3 本すべてが最適点を通るように調整（退化）。y は一通りに決まる？ 差分とのずれを見よう
-- 3. 価格の合計 b·y はいつも利益と同じ。資源の「価値」の総和 = 利益、という読み方
-- 4. preset の c を (1, 1) に変えると（★）、きつい制約の組み合わせはどう変わる？

-- ---------- 道具 ----------
-- 5 つのリストをまとめて並べる
zip5' :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5' (a : as) (b : bs) (c : cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5' as bs cs ds es
zip5' _ _ _ _ _ = []

-- 整数なら小数点なしで書く
num :: Double -> String
num v = let k = round v :: Int in if fromIntegral k == v then show k else fmt 2 v

-- 平面 a y1 + b y2 + c y3 = d
type Plane = (Double, Double, Double, Double)

-- 双対 min b·y s.t. Aᵀy ≥ c, y ≥ 0（3 変数）：5 枚の平面から 3 枚ずつ選んで頂点を列挙
solveDual3 :: [(Double, Double)] -> [Double] -> (Double, Double) -> [Double]
solveDual3 as bs (cc1, cc2) = case foldl' pick Nothing cands of
  Just ((y1, y2, y3), _) -> [if v > 0 then v else 0 | v <- [y1, y2, y3]]
  Nothing -> [0, 0, 0]
  where
    (u1, u2, u3) = case map fst as of
      [a, b, c] -> (a, b, c)
      _ -> (0, 0, 0)
    (v1, v2, v3) = case map snd as of
      [a, b, c] -> (a, b, c)
      _ -> (0, 0, 0)
    (r1, r2, r3) = case bs of
      [a, b, c] -> (a, b, c)
      _ -> (0, 0, 0)
    pl1 = (u1, u2, u3, cc1)
    pl2 = (v1, v2, v3, cc2)
    planes = zip [0 :: Int ..] [pl1, pl2, (1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0)]
    holds (y1, y2, y3) (a, b, c, d) = a * y1 + b * y2 + c * y3 >= d - 1e-7
    cands = [ (y, r1 * y1 + r2 * y2 + r3 * y3)
            | (i, p) <- planes, (j, q) <- planes, (k, o) <- planes, i < j, j < k
            , Just y@(y1, y2, y3) <- [solve3 p q o]
            , y1 >= -1e-7, y2 >= -1e-7, y3 >= -1e-7, holds y pl1, holds y pl2 ]
    pick Nothing c = Just c
    pick best@(Just (_, bv)) c@(_, v) = if v < bv - 1e-9 then Just c else best

-- 3 元連立 1 次方程式（クラメルの公式）
solve3 :: Plane -> Plane -> Plane -> Maybe (Double, Double, Double)
solve3 (a1, b1, c1, d1) (a2, b2, c2, d2) (a3, b3, c3, d3)
  | abs dt < 1e-9 = Nothing
  | otherwise = Just ( det d1 b1 c1 d2 b2 c2 d3 b3 c3 / dt
                     , det a1 d1 c1 a2 d2 c2 a3 d3 c3 / dt
                     , det a1 b1 d1 a2 b2 d2 a3 b3 d3 / dt )
  where
    dt = det a1 b1 c1 a2 b2 c2 a3 b3 c3
    det a b c d e f g h i = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

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
