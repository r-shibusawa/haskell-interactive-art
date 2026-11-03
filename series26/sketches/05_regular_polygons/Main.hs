-- ============================================
-- 05 正多角形を折る
-- 正方形の紙から正三角形・正六角形・正五角形を「折りだけ」でつくる
--   主役は O5（点 A を通り、点 B を直線に乗せる折り）＝「中心 A・半径 |AB| の円」をコンパスなしで使う技
-- 1 = 正三角形 / 2 = 正六角形 / 3 = 正五角形 / →／スペース = 次の手順 / ← = 前へ / A = 自動
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (intercalate)
import Numeric (showFFloat)

data World = World
  { wShape :: !Int
  , wStep  :: !Int
  , wAnim  :: !Float      -- 今の手順のアニメーション 0〜1
  , wAuto  :: !Bool
  , wAutoT :: !Int
  }

-- 手順の中身：折り目、O5 の動き（中心 A、動く点 B、行き先 X）、多角形、印
data Item
  = ICrease !Ln
  | IMove !Point !Point !Point
  | IPoly [Point]
  | IPt !Point String

data StepP = StepP String [Item]
data Plan = Plan String [StepP]

main :: IO ()
main = runSketch (sketch "26-05 Folding Regular Polygons" (World 1 0 0 False 0))
  { skBack  = rgb255 34 32 40
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

vert, horiz :: Float -> Ln
vert x = Ln (x, 0) (0, 1)     -- 縦の線 x = 一定
horiz y = Ln (0, y) (1, 0)    -- 横の線 y = 一定

-- ★ 折り方の手順を組み立てる（紙の座標：左下が原点、一辺 1）
planOf :: Int -> Plan
planOf 1 = planTri
planOf 2 = planHex
planOf _ = planPent

-- 正三角形：下の辺を 1 辺にする
planTri :: Plan
planTri =
  let a = (0, 0)
      b = (1, 0)
      (t1x, t1cr) = o5 a b (vert 0.5) snd
  in Plan "Triangle"
       [ StepP "Fold in half vertically to make a center line" [ICrease (vert 0.5)]
       , StepP "O5: through corner A, put corner B on the center line. It lands at T" [IMove a b t1x, ICrease t1cr, IPt t1x "T"]
       , StepP "AB = AT = BT (each is the paper's side), so it is equilateral" [IPoly [a, b, t1x]]
       ]

-- 正六角形：中心 O、半径 1/2
planHex :: Plan
planHex =
  let o = (0.5, 0.5)
      e = (1, 0.5)
      w = (0, 0.5)
      (h1x, h1c) = o5 o e (vert 0.75) snd
      (h2x, h2c) = o5 o e (vert 0.75) (negate . snd)
      (h3x, h3c) = o5 o w (vert 0.25) snd
      (h4x, h4c) = o5 o w (vert 0.25) (negate . snd)
  in Plan "Hexagon"
       [ StepP "Fold in half both ways: center O, and midpoints W, E of left and right edges"
           [ICrease (vert 0.5), ICrease (horiz 0.5), IPt o "O", IPt e "E", IPt w "W"]
       , StepP "Fold left and right edges to the center line (the 1/4 lines)" [ICrease (vert 0.25), ICrease (vert 0.75)]
       , StepP "O5: through O, put E on the right 1/4 line (2 ways: up and down)" [IMove o e h1x, ICrease h1c, IMove o e h2x, ICrease h2c]
       , StepP "O5: through O, put W on the left 1/4 line" [IMove o w h3x, ICrease h3c, IMove o w h4x, ICrease h4c]
       , StepP "All 6 points are 1/2 from O and 1/2 from their neighbors: a regular hexagon" [IPoly [e, h1x, h3x, w, h4x, h2x]]
       ]

-- 正五角形：黄金比 φ = 1.618… を折りでつくる
planPent :: Plan
planPent =
  let m = (0.5, 0)
      c = (1, 1)
      bq = (1, 0)
      mc = Ln m (nrm (fst c - fst m) (snd c - snd m))
      (p1x, p1c) = o5 m bq mc snd                 -- |B′C| = (√5 − 1)/2 = 0.618
      (f, p2c) = o5 c p1x (horiz 1) (negate . fst)  -- 上の辺に 0.618 を移す
      v1 = (fst f / 2, 0)
      v2 = (1 - fst f / 2, 0)
      (p3x, p3c) = o5 v1 v2 (vert 0) snd
      (p4x, p4c) = o5 v2 v1 (vert 1) snd
      (p5x, p5c) = o5 p3x v1 (vert 0.5) snd
  in Plan "Pentagon"
       [ StepP "Fold in half vertically: M is the midpoint of the bottom edge" [ICrease (vert 0.5), IPt m "M"]
       , StepP "Fold the line from M to the upper right corner C (length sqrt5 / 2)" [ICrease mc]
       , StepP "O5: through M, put lower right corner on MC: B'. B'C = 0.618 (golden ratio)" [IMove m bq p1x, ICrease p1c, IPt p1x "B'"]
       , StepP "O5: through C, put B' on the top edge: F. The top edge now has 0.618" [IMove c p1x f, ICrease p2c, IPt f "F"]
       , StepP "Vertical line through F, then half of it: V1, V2 on the bottom (0.618 apart)"
           [ICrease (vert (fst f)), ICrease (vert (fst v1)), ICrease (vert (fst v2)), IPt v1 "V1", IPt v2 "V2"]
       , StepP "O5: through V1 put V2 on the left edge; through V2 put V1 on the right edge" [IMove v1 v2 p3x, ICrease p3c, IMove v2 v1 p4x, ICrease p4c]
       , StepP "O5: through the left vertex, put V1 on the center line: the top vertex" [IMove p3x v1 p5x, ICrease p5c]
       , StepP "5 sides of 0.618, diagonals of 1 (phi times): a regular pentagon" [IPoly [v1, v2, p4x, p5x, p3x]]
       ]

-- プランは定数なので 1 回だけ計算する
plans :: (Plan, Plan, Plan)
plans = (planOf 1, planOf 2, planOf 3)

curPlan :: World -> Plan
curPlan w = let (a, b, c) = plans in case wShape w of { 1 -> a; 2 -> b; _ -> c }

stepsOf :: Plan -> [StepP]
stepsOf (Plan _ ss) = ss

nSteps :: World -> Int
nSteps = length . stepsOf . curPlan

next :: Bool -> World -> World
next loop w
  | wStep w < nSteps w - 1 = w { wStep = wStep w + 1, wAnim = 0 }
  | loop = w { wShape = wShape w `mod` 3 + 1, wStep = 0, wAnim = 0 }
  | otherwise = w { wAnim = 0 }

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w0 =
  let w1 | wAuto w0 =
             let t = wAutoT w0 + 1
                 w' = w0 { wAutoT = t }
             in if t `mod` 110 == 0 then next True w' else w'
         | otherwise = w0
  in pure w1 { wAnim = min 1 (wAnim w1 + 0.02) }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let ps = paperSize env
      plan@(Plan name ss) = curPlan w
      st = wStep w
      (cx, cy) = sp env (0.5, 0.5)
      paper = Color (rgb255 246 240 226) (rect cx cy ps ps)
      items = concat
        [ map (drawItem env (i == st) (if i == st then wAnim w else 1)) its
        | (i, StepP _ its) <- zip [0 ..] ss, i <= st ]
      -- 説明
      py0 = 110
      tx = 60 + ps + 60
      boxW = envW env - tx - 30
      y = py0 + 70 + fromIntegral (length ss) * 58
      list = concat
        [ textBox env tx (py0 + 54 + fromIntegral i * 58) boxW 16 (if i == st then white else if i < st then gray 0.63 else gray 0.4) (show (i + 1) ++ ". " ++ t)
        | (i, StepP t _) <- zip [0 :: Int ..] ss ]
      lastPoly = case [ps' | StepP _ its <- ss, IPoly ps' <- its] of
        p : _ -> p
        [] -> []
      final
        | st == length ss - 1 && not (null lastPoly) =
            let sides = [dist2 a b | (a, b) <- edges lastPoly]
                n = length lastPoly
            in textBox env tx (y - 6) boxW 17 (rgb255 255 190 40) ("Sides: " ++ intercalate "  " (map (nf 4) sides))
               ++ [ txt env tx (y + 60) 17 (rgb255 255 190 40) ("Interior angle: " ++ nf 0 (180 - 360 / fromIntegral n) ++ " deg (" ++ show n ++ "-gon)") ]
        | otherwise = []
      hud =
        [ txt env 20 40 18 white ("1: triangle   2: hexagon   3: pentagon   Right/Space: next   Left: back   A: auto " ++ (if wAuto w then "ON" else "off"))
        , txt env 20 68 15 (gray 0.67) "Blue arc = path of a point during an O5 fold: it swings around a point on the crease, like a compass."
        , txt env tx (py0 + 30) 30 (rgb255 255 190 40) (name ++ "  step " ++ show (st + 1) ++ " / " ++ show (length ss))
        ]
  in plan `seq` Pictures ([paper] ++ items ++ list ++ final ++ hud)

drawItem :: Env -> Bool -> Float -> Item -> Picture
drawItem env now t item = case item of
  ICrease ln -> case clipSquare ln of
    Just (a, b) ->
      Color (if now then rgb255 255 170 30 else rgb255 150 140 130)
            (dashed (if now then 2.5 else 1.5) [8, 6] (sp env a) (sp env b))
    Nothing -> Blank
  -- B が A のまわりの円をたどって X へ（O5 = コンパスの代わり）
  IMove ac@(acx, acy) bp@(bx, by) (xx, xy) ->
    let r = dist2 ac bp
        a0 = atan2 (by - acy) (bx - acx)
        a1 = atan2 (xy - acy) (xx - acx)
        da = atan2 (sin (a1 - a0)) (cos (a1 - a0))
        pt a = sp env (acx + r * cos a, acy + r * sin a)
    in Pictures
         [ Color (rgba255 80 120 220 (if now then 200 else 70))
             (thickPath 1.5 [pt (a0 + da * t * fromIntegral k / 30) | k <- [0 .. 30 :: Int]])
         , Color (rgb255 80 120 220) (at (pt (a0 + da * t)) (disc (if now then 6 else 3.5)))
         , Color (gray 0.16) (at (sp env ac) (disc 3.5))
         ]
  IPoly ps ->
    let qs = map (sp env) ps
    in Pictures
         [ Color (rgba255 226 84 72 (round (170 * t))) (Polygon qs)
         , Color (rgb255 170 40 40) (thickPath 3 (qs ++ take 1 qs)) ]
  IPt p@(_, y) name ->
    let (a, b) = sp env p
    in Pictures
         [ Color (gray 0.12) (at (a, b) (disc 4.5))
         , label (a + 8) (b - (if y > 0.9 then 20 else -8)) 15 (gray 0.12) name ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> next False w
  EventKey (SpecialKey KeySpace) Down _ _ -> next False w
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wStep = max 0 (wStep w - 1), wAnim = 1 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> w { wShape = fromEnum c - fromEnum '0', wStep = 0, wAnim = 0 }
    | toLower c == 'a' -> w { wAuto = not (wAuto w), wAutoT = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 正三角形の手順を本物の紙で。分度器で 60° になっているか測ろう
-- 2. 正六角形の手順をまねして、正六角形の中に正三角形を 2 つ（六芒星）描き足してみよう
-- 3. 正五角形の手順 3 で出てくる 0.618… は黄金比 φ の逆数。φ = 1 / 0.618… = 1.618… を確かめよう
-- 4. 正八角形はどう折る？（ヒント：45° の折りは「辺を対角線に重ねる」O3 でできる）

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（紙の座標：一辺 1）と画面の道具
-- ==================================================

data Ln = Ln !Point !Point

nrm :: Float -> Float -> Point
nrm x y = let l0 = sqrt (x * x + y * y)
              l = if l0 == 0 then 1 else l0
          in (x / l, y / l)

bisector :: Point -> Point -> Ln
bisector (ax, ay) (bx, by) = Ln ((ax + bx) / 2, (ay + by) / 2) (nrm (-(by - ay)) (bx - ax))

sdist :: Point -> Ln -> Float
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

-- O5：点 A を通り、点 B を直線 L に乗せる折り。2 つの解から pick の値が大きい方を選ぶ
o5 :: Point -> Point -> Ln -> (Point -> Float) -> (Point, Ln)
o5 a@(ax, ay) b ln@(Ln (lx, ly) (ux, uy)) pick =
  let r = dist2 a b
      t0 = (ax - lx) * ux + (ay - ly) * uy
      d = sdist a ln
      hh = sqrt (max 0 (r * r - d * d))
      at' s = (lx + ux * (t0 + s), ly + uy * (t0 + s))
      c0 = at' (-hh)
      c1 = at' hh
      x = if pick c0 >= pick c1 then c0 else c1
  in (x, bisector b x)

edges :: [a] -> [(a, a)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

clipSquare :: Ln -> Maybe (Point, Point)
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

sp :: Env -> Point -> Point
sp env (u, v) = let ps = paperSize env in sc env (60 + u * ps, 110 + ps - v * ps)

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

nf :: Int -> Float -> String
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
