-- ============================================
-- 02 角の三等分（阿部の方法）
-- 「定規とコンパスで任意の角を 3 等分することはできない」（1837 年に証明）。でも折り紙ならできる
--   決め手は O6（2 点を 2 直線に同時に乗せる折り）。3 次方程式を 1 回の折りで解いている
-- →／スペース = 次の手順 / ← = 前の手順 / ↑↓ = 角 θ を変える / A = 自動で進む
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

-- 紙の座標（左下が原点、一辺 1、y は上向き）。桁を見せるので Double で計算する
type P = (Double, Double)

initialTheta, a0 :: Double
initialTheta = 66        -- ★ 3 等分したい角（度）。0〜90
a0 = 0.22                -- ★ 平行線の間隔（紙の一辺 = 1）。小さいほど大きな角まで折れる

data World = World
  { wTheta :: !Double
  , wStep  :: !Int
  , wPhi   :: !Float     -- 折りのアニメーション（0 = 広げた、π = 折った）
  , wAuto  :: !Bool
  , wAutoT :: !Int
  }

steps :: [String]
steps =
  [ "Angle theta is given (at corner Q, between the bottom edge and line l)"
  , "Fold a line L2 parallel to the bottom edge (height 2a, any height works)"
  , "Bring the bottom edge to L2 to fold L1 (height a). Mark M and P on the left edge"
  , "O6: fold so corner Q lands on L1 and point P lands on line l, at the same time"
  , "While folded, mark where Q and M land (Q', M'), then unfold"
  , "Draw lines from corner Q to Q' and M': theta is trisected!"
  ]

main :: IO ()
main = runSketch (sketch "26-02 Angle Trisection" (World initialTheta 0 0 False 0))
  { skBack  = rgb255 34 32 40
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 解：折り線、Q・P・M の行き先
data Sol = Sol { sCrease :: !Ln, sQp :: !P, sPp :: !P, sMp :: !P }

-- ★ O6 で折り線を求める：Q を L1（y = a）に、P(0, 2a) を直線 l に乗せる
solveFold :: Double -> Maybe Sol
solveFold thetaDeg = go 1 (-3) (f (-3)) Nothing
  where
    r = thetaDeg * pi / 180
    l = Ln (0, 0) (cos r, sin r)
    q = (0, 0)
    p = (0, 2 * a0)
    m = (0, a0)
    nN = 3000 :: Int
    f t = sdist (refl p (bisector q (t, a0))) l
    go :: Int -> Double -> Double -> Maybe Sol -> Maybe Sol
    go !i !pt !pf best
      | i > nN = best
      | otherwise =
          let t = -3 + 6 * fromIntegral i / fromIntegral nN
              v = f t
              best' = if pf * v <= 0 then check (bisect pt t pf (50 :: Int)) best else best
          in go (i + 1) t v best'
    bisect lo hi _ 0 = (lo + hi) / 2
    bisect lo hi flo k =
      let md = (lo + hi) / 2
          fm = f md
      in if flo * fm <= 0 then bisect lo md flo (k - 1) else bisect md hi fm (k - 1)
    -- 紙の中で、P′ が直線 l の正の側にある解を選ぶ
    check x best =
      let cr = bisector q (x, a0)
          pp = refl p cr
      in if x > 0 && x < 1 && fst pp * cos r + snd pp * sin r > 0
           then Just (Sol cr (x, a0) pp (refl m cr))
           else best

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let w1 | wAuto w0 =
             let t = wAutoT w0 + 1
             in w0 { wAutoT = t, wStep = if t `mod` 120 == 0 then (wStep w0 + 1) `mod` length steps else wStep w0 }
         | otherwise = w0
      target :: Float
      target | wStep w1 == 3 = pi
             | wStep w1 == 4 = if envFrame env `mod` 160 < 80 then pi else 0   -- 折って、広げて
             | otherwise = 0
  in pure w1 { wPhi = wPhi w1 + (target - wPhi w1) * 0.08 }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let ps = paperSize env
      st = wStep w
      th = wTheta w
      sol = solveFold th
      px = sp env
      r = th * pi / 180
      lx = cos r
      ly = sin r
      lt = min (1 / max lx 1e-6) (1 / max ly 1e-6)
      (cx, cy) = px (0.5, 0.5)
      grayLine = rgb255 120 120 120
      -- 紙と平行線
      paper = Color (rgb255 246 240 226) (rect cx cy ps ps)
      parallels =
        [ Pictures [Color grayLine (dashed 2 [8, 6] (px (0, 2 * a0)) (px (1, 2 * a0))), lab env "L2" (1.01, 2 * a0)] | st >= 1 ]
        ++ [ Pictures [Color grayLine (dashed 2 [8, 6] (px (0, a0)) (px (1, a0))), lab env "L1" (1.01, a0)] | st >= 2 ]
      -- 角 θ の直線 l
      lineL =
        [ Color (rgb255 200 70 50) (thickLine 3 (px (0, 0)) (px (lx * lt, ly * lt)))
        , lab env "l" (lx * lt * 0.9 + 0.02, ly * lt * 0.9)
        , Color (rgba255 200 70 50 120) (thickPath 2
            [ px (0.25 * cos (r * fromIntegral k / 40), 0.25 * sin (r * fromIntegral k / 40)) | k <- [0 .. 40 :: Int] ])
        ]
      foldPic = case sol of
        Just s | st >= 3 -> [foldLayer env w s]
        _ -> []
      -- 左の辺の点
      leftDots =
        [ Color (gray 0.16) (at (px (0, 0)) (disc 5)), tp env (-24) 5 (0, 0) 16 (gray 0.9) "Q" ]
        ++ concat [ [ Color (gray 0.16) (at (px (0, a0)) (disc 5)), Color (gray 0.16) (at (px (0, 2 * a0)) (disc 5))
                    , tp env (-26) 5 (0, a0) 16 (gray 0.9) "M", tp env (-24) 5 (0, 2 * a0) 16 (gray 0.9) "P" ] | st >= 2 ]
      -- 三等分線
      trisect = case sol of
        Just s | st >= 5 ->
          let (qx, qy) = sQp s
              (mx, my) = sMp s
              k1 = 1 / max qx qy
              k2 = 1 / max mx my   -- 紙の端まで伸ばす
          in [ Color (rgb255 40 160 110) (thickLine 3 (px (0, 0)) (px (qx * k1, qy * k1)))
             , Color (rgb255 60 110 220) (thickLine 3 (px (0, 0)) (px (mx * k2, my * k2))) ]
        _ -> []
      -- 説明
      tx = 60 + ps + 60
      boxW = envW env - tx - 30
      py0 = 110
      y = py0 + 200 + fromIntegral (length steps) * 46
      stepText = case drop st steps of { s : _ -> s; [] -> "" }
      panel =
        [ txt env tx (py0 + 30) 30 (rgb255 255 190 40) ("Step " ++ show st ++ " / " ++ show (length steps - 1)) ]
        ++ textBox env tx (py0 + 50) boxW 19 white stepText
        ++ concat [ textBox env tx (py0 + 190 + fromIntegral i * 46) boxW 15 (if i == st then white else gray 0.47) (show i ++ ". " ++ s)
                  | (i, s) <- zip [0 :: Int ..] steps ]
        ++ result
      result = case sol of
        Nothing -> [txt env tx (y + 10) 16 (rgb255 255 120 100) "Cannot fold inside the paper at this angle: make a smaller"]
        Just s | st >= 5 ->
          let deg (a, b) = atan2 b a * 180 / pi
          in [ txt env tx (y + 20) 20 (rgb255 40 200 140) ("angle(bottom, QQ') = " ++ nf 3 (deg (sQp s)) ++ " deg")
             , txt env tx (y + 50) 20 (rgb255 110 160 255) ("angle(bottom, QM') = " ++ nf 3 (deg (sMp s)) ++ " deg")
             , txt env tx (y + 80) 20 white ("theta/3 = " ++ nf 3 (th / 3) ++ "   2 theta/3 = " ++ nf 3 (2 * th / 3))
             ]
        _ -> []
      hud =
        [ txt env 20 40 18 white ("Right/Space: next   Left: back   Up/Down: theta = " ++ nf 0 th ++ " deg   A: auto " ++ (if wAuto w then "ON" else "off"))
        , txt env 20 68 15 (gray 0.67) "Ruler and compass stop at quadratics (circle meets line). Origami O6 solves cubics, so it can trisect."
        ]
  in Pictures ([paper] ++ parallels ++ lineL ++ foldPic ++ leftDots ++ trisect ++ panel ++ hud)

-- 折り線と、折り返される部分
foldLayer :: Env -> World -> Sol -> Picture
foldLayer env w s =
  let cr@(Ln _ (ux, uy)) = sCrease s
      px = sp env
      phi = wPhi w
      seg = case clipSquare cr of
        Just (a, b) -> Color (rgb255 255 190 40) (thickLine 3 (px a) (px b))
        Nothing -> Blank
      sq = [(0, 0), (1, 0), (1, 1), (0, 1)]
      side = if sdist (0, 0) cr > 0 then 1 else -1
      flap = clipHalf sq cr side
      k = realToFrac (1 - cos phi) :: Double
      -- 折り返される部分（Q の側）をアニメーション：折り線からの距離 s が s·cos φ になる
      flapPts = [ px (x + uy * d * k, y - ux * d * k) | p@(x, y) <- flap, let d = sdist p cr ]
      flapPic
        | phi > 0.02 && length flapPts >= 3 = Pictures
            [ Color (if phi > pi / 2 then rgba255 226 84 72 220 else rgba255 255 250 235 230) (Polygon flapPts)
            , Color (rgb255 80 40 40) (poly flapPts) ]
        | otherwise = Blank
      mark (name, p) = Pictures
        [ Color (gray 0.16) (at (px p) (disc 5))
        , tp env 8 (-8) p 15 (gray 0.08) name ]
  in Pictures [seg, flapPic, mark ("Q'", sQp s), mark ("M'", sMp s), mark ("P'", sPp s)]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> next
  EventKey (SpecialKey KeySpace) Down _ _ -> next
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wStep = max (wStep w - 1) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTheta = min (wTheta w + 2) 88 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTheta = max (wTheta w - 2) 4 }
  EventKey (Char c) Down _ _ | toLower c == 'a' -> w { wAuto = not (wAuto w), wAutoT = 0 }
  _ -> w
  where next = w { wStep = min (wStep w + 1) (length steps - 1) }

-- ---------- やってみよう ----------
-- 1. θ = 90° を 3 等分すると 30°。これは定規とコンパスでもできる。θ = 60° → 20° はなぜ定規とコンパスでは無理？
-- 2. a0 を 0.1 や 0.4 に変えると、折れる角の範囲はどう変わる？
-- 3. なぜ 3 等分になるのか：Q・M′・Q′ と、P′ の関係（合同な三角形が 3 つ）を図に描いて考えよう
-- 4. この手順を本物の紙で折ってみよう。分度器で測ると何度ずれる？

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（紙の座標：一辺 1）と画面の道具
-- ==================================================

data Ln = Ln !P !P     -- 通る点と向き（長さ 1）

nrm :: Double -> Double -> P
nrm x y = let l0 = sqrt (x * x + y * y)
              l = if l0 == 0 then 1 else l0
          in (x / l, y / l)

bisector :: P -> P -> Ln
bisector (ax, ay) (bx, by) = Ln ((ax + bx) / 2, (ay + by) / 2) (nrm (-(by - ay)) (bx - ax))

sdist :: P -> Ln -> Double
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

refl :: P -> Ln -> P
refl q@(qx, qy) ln@(Ln _ (ux, uy)) = let s = sdist q ln in (qx + 2 * s * uy, qy - 2 * s * ux)

edges :: [a] -> [(a, a)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

clipHalf :: [P] -> Ln -> Double -> [P]
clipHalf pts ln side = concatMap one (edges pts)
  where
    one (p@(px, py), q@(qx, qy)) =
      let s1 = sdist p ln * side
          s2 = sdist q ln * side
          t = s1 / (s1 - s2)
      in [p | s1 >= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | s1 * s2 < 0]

clipSquare :: Ln -> Maybe (P, P)
clipSquare ln =
  let sq = [(0, 0), (1, 0), (1, 1), (0, 1)]
      hits = [ (px + (qx - px) * t, py + (qy - py) * t)
             | (p@(px, py), q@(qx, qy)) <- edges sq
             , let s1 = sdist p ln
             , let s2 = sdist q ln
             , s1 * s2 <= 0 && s1 /= s2
             , let t = s1 / (s1 - s2) ]
  in case hits of
       a : b : _ -> Just (a, b)
       _ -> Nothing

paperSize :: Env -> Float
paperSize env = min (envW env * 0.5) (envH env - 170)

-- 紙の座標 → gloss の座標（紙の左上は画面の (60, 110)）
sp :: Env -> P -> Point
sp env (u, v) =
  let ps = paperSize env
  in sc env (60 + realToFrac u * ps, 110 + ps - realToFrac v * ps)

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

-- 紙の点 p から画面で (dx, dy) ずらした所に文字（dy は下向きが正）
tp :: Env -> Float -> Float -> P -> Float -> Color -> String -> Picture
tp env dx dy p size c s = let (a, b) = sp env p in label (a + dx) (b - dy) size c s

lab :: Env -> String -> P -> Picture
lab env s p = tp env 4 5 p 15 (gray 0.6) s

nf :: Int -> Double -> String
nf d v = showFFloat (Just d) v ""

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
