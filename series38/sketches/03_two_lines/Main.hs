-- ============================================
-- 38-03 2 本のトロピカル直線は 1 点で交わる
-- 一般の位置にある 2 本のトロピカル直線はちょうど 1 点で交わる（ふつうの直線と同じ）
--   重なってしまう（半直線が共有される）ときは「安定交点」：少しずらした交点の極限。
--   安定交点はトロピカル・クラメルの公式で一発で求まる：
--   X = (b1⊗c2) ⊕ (b2⊗c1),  Y = (a1⊗c2) ⊕ (a2⊗c1),  Z = (a1⊗b2) ⊕ (a2⊗b1)  → 交点 (X − Z, Y − Z)
-- 操作：2 つの頂点をドラッグ / ←→ = 摂動の大きさ ε / G = 頂点を格子にスナップ / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import Numeric (showFFloat)

-- 同じ直線の上かを 1e-6 の精度で調べるので Double を使う
data L = L { la, lb, lc :: !Double }

line1, line2 :: L
line1 = L 0 0 0            -- ★ 直線 1 の係数 min(a + x, b + y, c)
line2 = L (-3) (-1) 0      -- ★ 直線 2

sc :: Float
sc = 70

data World = World
  { wL1, wL2 :: !L
  , wEps :: !Double        -- ★ 摂動の大きさ（←→）
  , wSnap :: !Bool
  , wDrag :: !Int }        -- 0 / 1 = ドラッグ中の直線、-1 = なし

world0 :: World
world0 = World line1 line2 0.6 False (-1)

main :: IO ()
main = runSketch (sketch "38-03 Two Tropical Lines" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.45 + x * sc
toY env y = envH env * 0.55 - y * sc

sp :: Env -> (Double, Double) -> Point
sp env (x, y) = (toX env (realToFrac x), toY env (realToFrac y))

vert :: L -> (Double, Double)
vert l = (lc l - la l, lc l - lb l)

-- ★ トロピカル・クラメルの公式（min 流儀）→ 安定交点
cramer :: L -> L -> (Double, Double)
cramer p q =
  let x = min (lb p + lc q) (lb q + lc p)
      y = min (la p + lc q) (la q + lc p)
      z = min (la p + lb q) (la q + lb p)
  in (x - z, y - z)

-- 点が直線 l の上にあるか（最小が 2 回とられる）
onLine :: L -> (Double, Double) -> Bool
onLine l (x, y) = case sort [la l + x, lb l + y, lc l] of
  (a : b : _) -> b - a < 1e-6
  _ -> False

-- 2 本が半直線を共有しているか（頂点が同じ x, 同じ y, または同じ対角線上）
overlapInfo :: World -> Maybe String
overlapInfo w
  | abs (x1 - x2) < 1e-6 = Just "Same x -> vertical rays overlap"
  | abs (y1 - y2) < 1e-6 = Just "Same y -> horizontal rays overlap"
  | abs ((x1 - y1) - (x2 - y2)) < 1e-6 = Just "Same diagonal -> diagonal rays overlap"
  | otherwise = Nothing
  where (x1, y1) = vert (wL1 w)
        (x2, y2) = vert (wL2 w)

rayDirs :: [(Double, Double)]
rayDirs = [(1, 0), (0, 1), (-1, -1)]

drawLine :: Env -> L -> Float -> Float -> Picture
drawLine env l hue wgt = Pictures (
  [ seg env wgt (hsba hue 70 100 0.9) (sp env (x, y)) (sp env (x + dx * 100, y + dy * 100)) | (dx, dy) <- rayDirs ]
  ++ [ let (px, py) = sp env (x, y) in dotP env px py 18 (hsb hue 70 100) ] )
  where (x, y) = vert l

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ seg env 1 (hsba 0 0 100 0.25) (toX env g, 0) (toX env g, envH env) | g <- grid ]
  ++ [ seg env 1 (hsba 0 0 100 0.25) (0, toY env g) (envW env, toY env g) | g <- grid ]
  ++ [ seg env 1 (hsba 0 0 100 0.5) (toX env 0, 0) (toX env 0, envH env)
     , seg env 1 (hsba 0 0 100 0.5) (0, toY env 0) (envW env, toY env 0) ]
  ++ overlapPic
  ++ [ drawLine env l1 35 4, drawLine env l2 330 4 ]
  ++ dashed
  ++ [ let (qx, qy) = sp env pPert in Color (hsb 55 90 100) (at (pt env qx qy) (ring 11 2)) ]
  ++ [ dotP env sx sy 20 (hsb 90 80 100), txtC env sx sy 14 (gray 0.1) "P" ]
  ++ [ txt env 20 36 16 (gray 1) ("Drag the vertices   <- ->: shift eps = " ++ nfd 2 (wEps w) ++ "   G: snap to grid " ++ (if wSnap w then "ON" else "off") ++ "   R: reset")
     , txt env 20 62 14 (gray 0.65) ("Line 1 (a,b,c) = " ++ triple l1 ++ "   Line 2 = " ++ triple l2)
     , box env (px - 16) (py - 30) 515 268 (hsba 0 0 100 0.08)
     , txt env px py 16 (gray 1) "Tropical Cramer's rule"
     , txt env px (py + 30) 14 green' ("X = min(b1 + c2, b2 + c1) = " ++ nfd 2 (min (lb l1 + lc l2) (lb l2 + lc l1)))
     , txt env px (py + 52) 14 green' ("Y = min(a1 + c2, a2 + c1) = " ++ nfd 2 (min (la l1 + lc l2) (la l2 + lc l1)))
     , txt env px (py + 74) 14 green' ("Z = min(a1 + b2, a2 + b1) = " ++ nfd 2 (min (la l1 + lb l2) (la l2 + lb l1)))
     , txt env px (py + 104) 14 (gray 1) ("Stable = (X - Z, Y - Z) = (" ++ nfd 2 (fst p) ++ ", " ++ nfd 2 (snd p) ++ ")")
     , txt env px (py + 128) 14 (gray 1) ("On line 1? " ++ yn (onLine l1 p) ++ "   On line 2? " ++ yn (onLine l2 p))
     , txt env px (py + 158) 14 (maybe (gray 0.65) (const (hsb 55 90 100)) ov) (maybe "General position: exactly 1 point" id ov)
     , txt env px (py + 182) 14 (gray 0.65) "Yellow ring = crossing when line 2 is shifted"
     , txt env px (py + 202) 14 (gray 0.65) "by (eps, 2 eps). As eps -> 0 it reaches the"
     , txt env px (py + 222) 14 (gray 0.65) "green stable point, however you shift." ] )
  where
    grid = [fromIntegral g | g <- [-12 .. 12 :: Int]] :: [Float]
    l1 = wL1 w
    l2 = wL2 w
    eps = wEps w
    ov = overlapInfo w
    green' = hsb 90 70 100
    yn b = if b then "yes" else "no"
    triple l = "(" ++ nfd 1 (la l) ++ ", " ++ nfd 1 (lb l) ++ ", " ++ nfd 1 (lc l) ++ ")"
    -- 重なっているときは、重なった部分を黄色で強調
    overlapPic = case ov of
      Nothing -> []
      Just _ ->
        let v1@(x1, y1) = vert l1
            v2@(x2, y2) = vert l2
            dxs = signum (x2 - x1)
            dys = signum (y2 - y1)
            (ex, ey) | abs (x1 - x2) < 1e-6 = (0, 1)
                     | abs (y1 - y2) < 1e-6 = (1, 0)
                     | otherwise = (-1, -1)
            far@(fx, fy) = if ex * dxs + ey * dys >= 0 then v2 else v1   -- 方向 e で遠いほう
            c = hsba 55 90 100 0.6
        in [ seg env 14 c (sp env far) (sp env (fx + ex * 100, fy + ey * 100))
           , seg env 14 c (sp env v1) (sp env v2) ]
    -- 摂動した直線 2（点線）と、その交点
    p2 = L (la l2 - eps) (lb l2 - 2 * eps) (lc l2)
    (vpx, vpy) = vert p2
    dashed = [ seg env 1.5 (hsba 330 40 100 0.5) (sp env (vpx + dx * s, vpy + dy * s)) (sp env (vpx + dx * (s + 0.25), vpy + dy * (s + 0.25)))
             | (dx, dy) <- rayDirs, k <- [0 .. 59 :: Int], let s = fromIntegral k * 0.5 ]
    pPert = cramer l1 p2
    -- 安定交点（クラメル）
    p = cramer l1 l2
    (sx, sy) = sp env p
    px = envW env - 530
    py = 110

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [k | (k, l) <- [(0, wL1 w), (1, wL2 w)], dist2 m (sp env (vert l)) < 24]
    in w { wDrag = if null hit then -1 else last hit }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        x0 = realToFrac ((mx - envW env * 0.45) / sc) :: Double
        y0 = realToFrac ((envH env * 0.55 - my) / sc) :: Double
        rnd v = fromIntegral (floor (v + 0.5) :: Int) :: Double
        x = if wSnap w then rnd x0 else x0
        y = if wSnap w then rnd y0 else y0
        move l = l { la = clampD (-10) 10 (lc l - x), lb = clampD (-10) 10 (lc l - y) }
    in if wDrag w == 0 then w { wL1 = move (wL1 w) } else w { wL2 = move (wL2 w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wEps = max 0.05 (wEps w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wEps = min 2 (wEps w + 0.1) }
  EventKey (Char ch) Down _ _
    | ch `elem` "gG" -> w { wSnap = not (wSnap w) }
    | ch `elem` "rR" -> w { wL1 = line1, wL2 = line2 }
  _ -> w

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- ---------- やってみよう ----------
-- 1. 頂点を動かして、交点が「必ず 1 つ」になることを確かめよう。交点が頂点に来るのはどんなとき？
-- 2. G でスナップして 2 つの頂点を同じ x に置く（半直線が重なる）。安定交点はどこ？ ε を変えても同じ？
-- 3. 直線 2 のずらし方を (ε, 2ε) でなく (2ε, ε) に変えても極限が同じか試そう
-- 4. 2 点を通るトロピカル直線もクラメルで求まる（係数 = 2 点の座標の 2×2 トロピカル行列式）。書いてみよう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str
