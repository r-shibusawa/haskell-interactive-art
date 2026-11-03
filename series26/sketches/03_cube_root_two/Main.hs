-- ============================================
-- 03 立方根 2 の作図（メッサーの方法）
-- 体積が 2 倍の立方体の一辺 ∛2 を作図せよ（デロス島の問題）── 定規とコンパスでは不可能
--   折り紙なら O6 を 1 回使うだけ。紙を 3 等分して、角と点を同時に 2 本の線へ乗せる
-- →／スペース = 次の手順 / ← = 前の手順 / A = 自動で進む / G = 3 等分の線の高さを動かして一般化
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

-- 紙の座標（左下が原点、一辺 1、y は上向き）。桁を見せるので Double で計算する
type P = (Double, Double)

h0 :: Double
h0 = 1 / 3                 -- ★ 下の線 l1 の高さ（3 等分なら 1/3）

data World = World
  { wStep    :: !Int
  , wPhi     :: !Float
  , wAuto    :: !Bool
  , wAutoT   :: !Int
  , wGeneral :: !Bool      -- G：l1 の高さ h を動かす
  , wH       :: !Double
  }

steps :: [String]
steps =
  [ "Square paper (side 1). Goal: divide the left edge in the ratio cbrt(2) : 1"
  , "Fold lines l1 (1/3) and l2 (2/3), dividing the paper into 3 equal strips"
  , "Point P = l1 meets the right edge. C = the lower right corner"
  , "O6: fold so corner C lands on the left edge and P lands on l2, at the same time"
  , "C' (where C lands) divides the left edge: upper : lower = cbrt(2) : 1 !"
  ]

main :: IO ()
main = runSketch (sketch "26-03 Cube Root of 2" (World 0 0 False 0 False h0))
  { skBack  = rgb255 34 32 40
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

data Sol = Sol { sCrease :: !Ln, sCp :: !P, sPp :: !P }

-- ★ O6：C(1, 0) を左の辺 x = 0 に、P(1, h) を l2（y = 2h）に乗せる折り線
solveFold :: Double -> Maybe Sol
solveFold h = go 1 0.001 (f 0.001) Nothing
  where
    c = (1, 0)
    p = (1, h)
    l2 = Ln (0, 2 * h) (1, 0)
    nN = 2000 :: Int
    f t = sdist (refl p (bisector c (0, t))) l2
    go :: Int -> Double -> Double -> Maybe Sol -> Maybe Sol
    go !i !pt !pf best
      | i > nN = best
      | otherwise =
          let t = 0.001 + 0.998 * fromIntegral i / fromIntegral nN
              v = f t
              best' = if pf * v <= 0 then Just (mk (bisect pt t pf (50 :: Int))) else best
          in go (i + 1) t v best'
    bisect lo hi _ 0 = (lo + hi) / 2
    bisect lo hi flo k =
      let md = (lo + hi) / 2
          fm = f md
      in if flo * fm <= 0 then bisect lo md flo (k - 1) else bisect md hi fm (k - 1)
    mk y = let cr = bisector c (0, y) in Sol cr (0, y) (refl p cr)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let ps = realToFrac (paperSize env) :: Double
      (_, my) = mouseXY env
      w1 | wAuto w0 =
             let t = wAutoT w0 + 1
             in w0 { wAutoT = t, wStep = if t `mod` 120 == 0 then (wStep w0 + 1) `mod` length steps else wStep w0 }
         | otherwise = w0
      w2 | wGeneral w1 =
             let m = realToFrac my :: Double
                 v = 0.15 + (0.48 - 0.15) * (m - (110 + ps)) / (110 - (110 + ps))
             in w1 { wH = max 0.15 (min 0.48 v) }
         | otherwise = w1
      target :: Float
      target | wStep w2 == 3 = pi
             | wStep w2 == 4 && envFrame env `mod` 160 < 80 = pi
             | otherwise = 0
  in pure w2 { wPhi = wPhi w2 + (target - wPhi w2) * 0.08 }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let ps = paperSize env
      st = wStep w
      h = wH w
      sol = solveFold h
      px = sp env
      (cx, cy) = px (0.5, 0.5)
      paper = Color (rgb255 246 240 226) (rect cx cy ps ps)
      strips = concat
        [ [ Color (rgb255 120 120 120) (dashed 2 [8, 6] (px (0, h)) (px (1, h)))
          , Color (rgb255 120 120 120) (dashed 2 [8, 6] (px (0, 2 * h)) (px (1, 2 * h)))
          , tp env 6 5 (1, h) 15 (gray 0.6) "l1"
          , tp env 6 5 (1, 2 * h) 15 (gray 0.6) "l2" ]
        | st >= 1 ]
      pointsPC = concat
        [ [ Color (gray 0.16) (at (px (1, h)) (disc 5)), Color (gray 0.16) (at (px (1, 0)) (disc 5))
          , tp env 10 (-10) (1, h) 16 (gray 0.9) "P", tp env 10 4 (1, 0) 16 (gray 0.9) "C" ]
        | st >= 2 ]
      foldPic = case sol of
        Just s | st >= 3 -> [foldLayer env w s]
        _ -> []
      -- 左の辺を 2 色で塗り分ける
      edgeBars = case sol of
        Just s | st >= 4 ->
          let shift14 (x, y) = (x - 14, y)
              cpY = snd (sCp s)
          in [ Color (rgb255 60 110 220) (thickLine 8 (shift14 (px (0, 1))) (shift14 (px (0, cpY))))
             , Color (rgb255 40 160 110) (thickLine 8 (shift14 (px (0, cpY))) (shift14 (px (0, 0)))) ]
        _ -> []
      -- 説明
      py0 = 110
      tx = 60 + ps + 60
      boxW = envW env - tx - 30
      y = py0 + 170 + fromIntegral (length steps) * 44
      stepText = case drop st steps of { s : _ -> s; [] -> "" }
      panel =
        [ txt env tx (py0 + 30) 30 (rgb255 255 190 40) ("Step " ++ show st ++ " / " ++ show (length steps - 1)) ]
        ++ textBox env tx (py0 + 50) boxW 19 white stepText
        ++ concat [ textBox env tx (py0 + 160 + fromIntegral i * 44) boxW 15 (if i == st then white else gray 0.47) (show i ++ ". " ++ s)
                  | (i, s) <- zip [0 :: Int ..] steps ]
        ++ result
      result = case sol of
        Nothing -> [txt env tx (y + 20) 16 (rgb255 255 120 100) "Cannot fold inside the paper with this h"]
        Just s | st >= 4 ->
          let lo = snd (sCp s)
              up = 1 - lo
          in [ txt env tx (y + 20) 20 (rgb255 110 160 255) ("up = " ++ nf 5 up)
             , txt env (tx + 180) (y + 20) 20 (rgb255 40 200 140) ("low = " ++ nf 5 lo)
             , txt env tx (y + 54) 20 white ("up / low = " ++ nf 6 (up / lo))
             , txt env tx (y + 84) 20 (rgb255 255 190 40) ("cbrt(2)  = " ++ nf 6 (2 ** (1 / 3)))
             , drawCubes env (tx + 300) (y + 150) 50 (up / lo)
             ]
        _ -> []
      hud =
        [ txt env 20 40 18 white ("Right/Space: next   Left: back   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   G: move l1 with mouse " ++ (if wGeneral w then "ON (h = " ++ nf 3 h ++ ")" else "off"))
        , txt env 20 68 15 (gray 0.67) "Doubling a cube needs side cbrt(2) = 1.2599... It solves x^3 = 2, so ruler and compass cannot draw it."
        ]
  in Pictures ([paper] ++ strips ++ pointsPC ++ foldPic ++ edgeBars ++ panel ++ hud)

foldLayer :: Env -> World -> Sol -> Picture
foldLayer env w s =
  let cr@(Ln _ (ux, uy)) = sCrease s
      px = sp env
      phi = wPhi w
      seg = case clipSquare cr of
        Just (a, b) -> Color (rgb255 255 190 40) (thickLine 3 (px a) (px b))
        Nothing -> Blank
      side = if sdist (1, 0) cr > 0 then 1 else -1
      flap = clipHalf [(0, 0), (1, 0), (1, 1), (0, 1)] cr side
      k = realToFrac (1 - cos phi) :: Double
      flapPts = [ px (x + uy * d * k, y - ux * d * k) | p@(x, y) <- flap, let d = sdist p cr ]
      flapPic
        | phi > 0.02 && length flapPts >= 3 = Pictures
            [ Color (if phi > pi / 2 then rgba255 226 84 72 220 else rgba255 255 250 235 230) (Polygon flapPts)
            , Color (rgb255 80 40 40) (poly flapPts) ]
        | otherwise = Blank
  in Pictures
       [ seg, flapPic
       , Color (gray 0.16) (at (px (sCp s)) (disc 6)), Color (gray 0.16) (at (px (sPp s)) (disc 5))
       , tp env 10 5 (sCp s) 15 (gray 0.08) "C'"
       , tp env 8 (-8) (sPp s) 15 (gray 0.08) "P'"
       ]

-- 一辺 1 と 一辺 r の立方体を並べる（体積 1 と r³）
drawCubes :: Env -> Float -> Float -> Float -> Double -> Picture
drawCubes env x y s r = Pictures (zipWith one [0, 1] [(1, rgb255 40 160 110), (r, rgb255 60 110 220)])
  where
    one :: Float -> (Double, Color) -> Picture
    one k (len, c) =
      let l = s * clampF 0.2 1.6 (realToFrac len)    -- 画面からはみ出さないように描く大きさだけ制限
          cxx = x + k * s * 2.6
          d = l * 0.45
          q ps = let qs = map (sc env) ps
                 in Pictures [Color (withA 0.63 c) (Polygon qs), Color white (poly qs)]
      in Pictures
           [ q [(cxx, y), (cxx + l, y), (cxx + l, y - l), (cxx, y - l)]
           , q [(cxx, y - l), (cxx + l, y - l), (cxx + l + d, y - l - d), (cxx + d, y - l - d)]
           , q [(cxx + l, y), (cxx + l + d, y - d), (cxx + l + d, y - l - d), (cxx + l, y - l)]
           , txt env cxx (y + 22) 14 white ("vol " ++ nf 3 (len * len * len))
           ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> next
  EventKey (SpecialKey KeySpace) Down _ _ -> next
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wStep = max (wStep w - 1) 0 }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> w { wAuto = not (wAuto w), wAutoT = 0 }
    'g' -> if wGeneral w then w { wGeneral = False, wH = h0 } else w { wGeneral = True }
    _ -> w
  _ -> w
  where next = w { wStep = min (wStep w + 1) (length steps - 1) }

-- ---------- やってみよう ----------
-- 1. 手順 4 の比が ∛2 と何桁まで合っているか見よう。本物の紙で折るとどのくらいずれる？
-- 2. G で l1 の高さ h を変えると比はどう変わる？ h = 1/3 のときだけ ∛2 になる理由を考えよう
-- 3. ∛2 があれば「体積 2 倍の立方体」が折れる。∛3 をつくるにはどうすればいい？（ヒント：O6 の条件を変える）
-- 4. 02 の角の三等分と比べよう。どちらも O6 が 3 次方程式を解いている

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（紙の座標：一辺 1）と画面の道具
-- ==================================================

data Ln = Ln !P !P

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

sp :: Env -> P -> Point
sp env (u, v) =
  let ps = paperSize env
  in sc env (60 + realToFrac u * ps, 110 + ps - realToFrac v * ps)

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

tp :: Env -> Float -> Float -> P -> Float -> Color -> String -> Picture
tp env dx dy p size c s = let (a, b) = sp env p in label (a + dx) (b - dy) size c s

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
