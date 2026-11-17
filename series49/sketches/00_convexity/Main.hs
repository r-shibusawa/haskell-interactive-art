-- ============================================
-- 49-00 凸関数と凸集合
-- 凸関数 ⇔ グラフの上の部分（エピグラフ）が凸集合 ⇔ どの 2 点を結ぶ弦もグラフより上 ⇔ f'' ≥ 0（2 次元ならヘッセ行列が半正定値）
--   凸なら「局所最小 ＝ 大域最小」。たくさんの初期点から坂を下ると、全部同じ場所に着く
-- 操作：D = 1 次元 / 2 次元 / ←→ = 関数 / ドラッグ = 2 点 a・b を動かす / G = 坂を下る点 / T = 接線
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (log1p, showFFloat)
import System.Environment (lookupEnv)

nTh :: Int
nTh = 120                      -- ★ 弦の上で調べる点の数

-- 1 次元の関数：f, d1 = f', d2 = f''
data Fn1 = Fn1 { f1Name :: String, f1F, f1D1, f1D2 :: Double -> Double, f1Kink :: Bool }

fns1 :: [Fn1]
fns1 =
  [ Fn1 "x^2" (\x -> x * x) (\x -> 2 * x) (const 2) False
  , Fn1 "|x| (has a kink)" abs signum (const 0) True
  , Fn1 "e^x / 5" (\x -> exp x / 5) (\x -> exp x / 5) (\x -> exp x / 5) False
  , Fn1 "log(1 + e^x)" (\x -> log1p (exp x)) (\x -> 1 / (1 + exp (-x))) (\x -> let s = 1 / (1 + exp (-x)) in s * (1 - s)) False
  , Fn1 "x^4/4 - x^2" (\x -> x ^ (4 :: Int) / 4 - x * x) (\x -> x ^ (3 :: Int) - 2 * x) (\x -> 3 * x * x - 2) False
  , Fn1 "x^2/4 + sin(3x)/2" (\x -> x * x / 4 + sin (3 * x) / 2) (\x -> x / 2 + 1.5 * cos (3 * x)) (\x -> 0.5 - 4.5 * sin (3 * x)) False
  , Fn1 "x^3/6 (no minimum)" (\x -> x ^ (3 :: Int) / 6) (\x -> x * x / 2) id False
  ]

fns2 :: [Fn]
fns2 = [quadFn 5 0.5, lse, rosen, himmel]

data World = World
  { wDim    :: !Int            -- 1 か 2
  , wFi     :: !Int            -- 関数の番号
  , wA, wB  :: !Double         -- 1 次元の 2 点
  , wA2, wB2 :: !V2            -- 2 次元の 2 点
  , wDrag   :: !Int            -- つかんでいる点（−1 = なし）
  , wShowGD :: !Bool
  , wShowTan :: !Bool
  , wScan   :: !(Double, Double)          -- （max（グラフ − 弦）、f'' または λ_min の最小）
  , wMins1  :: [(Double, Double)]         -- 1 次元：（初期点、行き先）行き先 NaN = 発散
  , wMins2  :: [(V2, V2)]
  , wView   :: !View
  , wCI     :: Picture                    -- 等高線 ＋ ピンク（λ_min(H) < 0 の場所）
  , wTests  :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-00 Convex functions and convex sets" (rebuild env0' world0))
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }
  where env0' = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 1 0 (-2.2) 1.6 (0, 0) (0, 0) (-1) True True (0, 0) [] [] (makeView 16 74 704 676 (0, 0, 1)) Blank []

-- 関数を変えたときだけ計算しなおす（等高線・ランダムな弦の検査・坂を下った行き先）
rebuild :: Env -> World -> World
rebuild env w
  | wDim w == 1 =
      let f = getAt (head1 fns1) fns1 (wFi w)
      in w { wScan = scan1 f 400 (rngList 1), wMins1 = localMins1 f }
  | otherwise =
      let f = getAt (quadFn 5 0.5) fns2 (wFi w)
          (cx, cy, hw) = fnView f
          v = makeView 16 74 (realToFrac (envW env) * 0.55) (realToFrac (envH env) - 124) (fnView f)
          neg x y = if eL1 (eig2 (fnH f x y)) < 0 then Just (255, 80, 120, 0.28) else Nothing
      in w { wView = v, wCI = fst (contourImageT v (fnF f) 18 neg)
           , wA2 = (cx - hw * 0.6, cy - hw * 0.3), wB2 = (cx + hw * 0.5, cy + hw * 0.35)
           , wScan = scan2 f 400 (rngList 1), wMins2 = localMins2 f }
  where head1 xs = case xs of { (x : _) -> x; [] -> Fn1 "" id id id False }

-- 弦と関数の差：max_θ f(θa + (1−θ)b) − (θf(a) + (1−θ)f(b))。正なら弦がグラフより下 → 凸でない
jensenGap :: (Double -> Double) -> (Double, Double)
jensenGap f = foldl' pick (-1 / 0, 0) [fromIntegral i / fromIntegral nTh | i <- [0 .. nTh]]
  where
    f0 = f 0
    f1 = f 1
    pick (worst, at') t = let g = f t - ((1 - t) * f0 + t * f1) in if g > worst then (g, t) else (worst, at')

-- ランダムな n 組の a, b で弦を検査し、f'' の最小値も調べる
scan1 :: Fn1 -> Int -> [Double] -> (Double, Double)
scan1 f n rs = (gap, d2)
  where
    pairs = take n (pairUp rs)
    gap = foldl' max (-1 / 0) [fst (jensenGap (\t -> f1F f (a + t * (b - a)))) | (r1, r2) <- pairs, let a = 6 * r1 - 3, let b = 6 * r2 - 3]
    d2 = foldl' min (1 / 0) [f1D2 f (-3 + 6 * fromIntegral k / fromIntegral (n - 1)) | k <- [0 .. n - 1]]
    pairUp (a : b : xs) = (a, b) : pairUp xs
    pairUp _ = []

scan2 :: Fn -> Int -> [Double] -> (Double, Double)
scan2 f n rs = go n rs (-1 / 0) (1 / 0)
  where
    (cx, cy, hw) = fnView f
    go 0 _ g l = (g, l)
    go k (r1 : r2 : r3 : r4 : xs) g l =
      let a = (cx + hw * (2 * r1 - 1), cy + hw * 0.6 * (2 * r2 - 1))
          b = (cx + hw * (2 * r3 - 1), cy + hw * 0.6 * (2 * r4 - 1))
          gp = fst (jensenGap (\t -> uncurry (fnF f) (vaxpy t (vsub b a) a)))
      in go (k - 1) xs (max g gp) (min l (eL1 (eig2 (uncurry (fnH f) a))))
    go _ _ g l = (g, l)

-- 坂を下る（1 次元：x ← x − t f'(x)、t は半分ずつ縮めて f が十分下がるまで）。|x| > 50 なら「発散」（NaN）
descend1 :: Fn1 -> Double -> Double
descend1 f = go (0 :: Int)
  where
    go k x
      | k >= 2000 = x
      | abs d < 1e-12 = x
      | abs x' > 50 = 0 / 0
      | otherwise = go (k + 1) x'
      where
        d = f1D1 f x
        t = shrink 1
        shrink t0 = if t0 > 1e-14 && f1F f (x - t0 * d) > f1F f x - 0.5 * t0 * d * d then shrink (t0 / 2) else t0
        x' = x - t * d

localMins1 :: Fn1 -> [(Double, Double)]
localMins1 f = [(s, descend1 f s) | i <- [0 .. 12 :: Int], let s = -3 + 0.5 * fromIntegral i]

-- 2 次元：H が正定値ならニュートン方向、そうでなければ −∇f。どちらもアルミホで
descend2 :: Fn -> V2 -> V2
descend2 f = go (0 :: Int)
  where
    go k p
      | k >= 400 = p
      | vlen g < 1e-12 = p
      | otherwise = go (k + 1) (vaxpy (arT (armijo0 f p d)) d p)
      where
        g = uncurry (fnG f) p
        h = uncurry (fnH f) p
        d = if eL1 (eig2 h) > 1e-8 then vscale (-1) (solve2 h g) else vscale (-1) g

localMins2 :: Fn -> [(V2, V2)]
localMins2 f = [(s, descend2 f s) | i <- [-3 .. 3 :: Int], j <- [-2 .. 2 :: Int], let s = (cx + hw * fromIntegral i / 3.5, cy + hw * fromIntegral j / 4)]
  where (cx, cy, hw) = fnView f

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  body
  ++ [ hud [(title, cInk), ("D: " ++ (if wDim w == 1 then "1D" else "2D") ++ "   Left/Right: function   drag a, b   G: descent   T: tangent", cDim)] 16 14 16 1.45
     , hud panel x0 90 14 1.45
     , drawSelfTest env (wTests w) ]
  where
    (body, gapNow) = if wDim w == 1 then draw1 env w else draw2 env w
    title = "Convex functions and convex sets -- " ++ (if wDim w == 1 then f1Name (getAt (Fn1 "" id id id False) fns1 (wFi w)) else fnName (getAt rosen fns2 (wFi w)))
    x0 = envW env * 0.6
    (sGap, sD2) = wScan w
    isConvex = sGap <= 1e-12
    ends = if wDim w == 1 then [(e, 0) | (_, e) <- wMins1 w, not (isNaN e)] else [e | (_, e) <- wMins2 w, not (isNaN (fst e))]
    nAll = if wDim w == 1 then length (wMins1 w) else length (wMins2 w)
    distinct = foldl' (\acc p -> if any (\q -> vlen (vsub p q) < 1e-4) acc then acc else acc ++ [p]) [] ends
    showP (a, b) = if wDim w == 1 then "(" ++ fmt 4 a ++ ")" else "(" ++ fmt 4 a ++ ", " ++ fmt 4 b ++ ")"
    panel =
      [ ("Chord test: is the chord a-b above the graph?", cInk)
      , ("Now max(graph - chord) = " ++ fmt 3 gapNow, if gapNow > 1e-12 then cPk else cGr)
      , (if gapNow > 1e-12 then "-> chord goes below the graph (red): not convex" else "-> chord is above the graph (or touches it)", cDim)
      , ("", cInk)
      , ("400 random pairs a, b: max(graph - chord) = " ++ fmt 3 sGap, cInk)
      , ((if wDim w == 1 then "min f'' = " else "min of smallest Hessian eigenvalue = ") ++ fmt 3 sD2, cInk) ]
      ++ [ ("(pink = where it is negative)", cInk) | wDim w == 2 ]
      ++ [ ("Verdict: " ++ (if isConvex then "convex (chord always above, curvature >= 0)" else "not convex"), if isConvex then cGr else cPk)
         , ("", cInk)
         , ("Where descent ends: " ++ (if length ends < nAll then show (nAll - length ends) ++ " diverged, " else "") ++ show (length distinct) ++ " distinct point(s)", cYe)
         , (unwords (map showP (take 4 distinct)), cDim)
         , (if isConvex then "Convex: every local min is the global min (one place)"
            else if length distinct > 1 then "Not convex: different starts stop in different valleys"
            else "Not convex, but only one valley (convex is sufficient)", cDim)
         , ("", cInk) ]
      ++ (if wDim w == 1
            then [ ("Definition: for 0 <= th <= 1", cCy)
                 , ("  f(th a + (1-th) b) <= th f(a) + (1-th) f(b)", cCy)
                 , ("Convex set: the segment between any two", cCy)
                 , ("  points of the set stays inside it", cCy)
                 , ("f convex <=> epigraph {(x, z): z >= f(x)} convex", cCy)
                 , ("First order: f(y) >= f(x) + f'(x)(y - x)", cCy)
                 , ("  (the tangent stays below)", cCy) ]
            else [ ("2D: convex <=> every slice phi(th) along a", cCy)
                 , ("segment is convex <=> Hessian is PSD", cCy) ])

-- 1 次元：グラフ、エピグラフ、弦、接線
draw1 :: Env -> World -> ([Picture], Double)
draw1 env w = (pics, fst (jensenGap (\t -> ff (a + t * (b - a)))))
  where
    f = getAt (Fn1 "" id id id False) fns1 (wFi w)
    ff = f1F f
    (px, py, pw, ph) = plot1Rect env
    xs = [-3 + 6 * fromIntegral i / 200 | i <- [0 .. 200 :: Int]] :: [Double]
    vals = map ff xs
    lo = foldl' min (1 / 0) vals
    hi = foldl' max (-1 / 0) vals
    pad = (hi - lo) * 0.15 + 0.1
    xS x = px + d2f ((x + 3) / 6) * pw
    yS v = py + ph - d2f ((v - lo + pad) / (hi - lo + 2 * pad)) * ph
    a = wA w
    b = wB w
    -- エピグラフ（グラフより上）を縦の細い帯で塗る（gloss の多角形は凸しか塗れないため）
    epi = [ Color (rgba255 95 215 240 40) (Polygon [(xS x0, py), (xS x0, max py (yS v0)), (xS x1, max py (yS v1)), (xS x1, py)])
          | ((x0, v0), (x1, v1)) <- zip (zip xs vals) (drop 1 (zip xs vals)) ]
    curve = [ Color (if f1D2 f ((x0 + x1) / 2) < 0 then cPk else cCy) (thickLine 3 (xS x0, yS (ff x0)) (xS x1, yS (ff x1)))
            | i <- [0 .. 199 :: Int], let x0 = -3 + 6 * fromIntegral i / 200, let x1 = x0 + 0.03 ]
    tangent = let s = f1D1 f a
                  l = (xS (-3), yS (ff a + s * (-3 - a)))
                  r = (xS 3, yS (ff a + s * (3 - a)))
              in case clipY py (py + ph) l r of
                   Just (p, q) -> [Color cYe (thickLine 1.5 p q)]
                   Nothing -> []
    reds = [ Color cRd (at (xS x, yS (ff x)) (disc 3.5))
           | i <- [0 .. 40 :: Int], let t = fromIntegral i / 40, let x = a + t * (b - a), let c = (1 - t) * ff a + t * ff b, ff x > c + 1e-12 ]
    ends = [ Pictures [ dot (xS x, yS (ff x)) 7 cOr, txt (xS x + 9) (yS (ff x) - 8) 14 cInk nm ] | (x, nm) <- [(a, "a"), (b, "b")] ]
    gd = if wShowGD w
           then concat [ [ Color cGr (Line [(xS s, py + ph - 6), (xS e, yS (ff e))]) | not (isNaN e) ]
                         ++ [ Color cGr (at (xS s, py + ph - 6) (disc 3)) ]
                         ++ [ Color cGr (at (xS e, yS (ff e)) (disc 4.5)) | not (isNaN e) ]
                       | (s, e) <- wMins1 w ]
           else []
    pics = epi
      ++ [ Color cLine (rectLineTL px py pw ph), Color cLine (Line [(xS 0, py), (xS 0, py + ph)]) ]
      ++ curve
      ++ [ txt (px + 8) (py + 20) 14 cDim "Epigraph (above the graph) = light blue.  Pink: f'' < 0" ]
      ++ (if wShowTan w then tangent else [])
      ++ [ Color cOr (thickLine 2.5 (xS a, yS (ff a)) (xS b, yS (ff b))) ]
      ++ reds ++ ends ++ gd
      ++ [ txt (px + 8) (py + ph - 14) 14 cPk "Diverges (goes down forever)" | any (isNaN . snd) (wMins1 w) ]

plot1Rect :: Env -> (Float, Float, Float, Float)
plot1Rect env = (40, 80, envW env * 0.55, envH env - 150)

-- 2 次元：等高線、線分 a→b、その上の断面 φ(θ)
draw2 :: Env -> World -> ([Picture], Double)
draw2 env w = (pics, gapNow)
  where
    f = getAt rosen fns2 (wFi w)
    v = wView w
    a = toS v (wA2 w)
    b = toS v (wB2 w)
    phi t = uncurry (fnF f) (vaxpy t (vsub (wB2 w) (wA2 w)) (wA2 w))
    gapNow = fst (jensenGap phi)
    gd = if wShowGD w
           then concat [ [Color cGr (Line [p, q]), Color cGr (at p (disc 2.5)), Color cGr (at q (disc 5))]
                       | (s, e) <- wMins2 w, let p = toS v s, let q = toS v e ]
           else []
    ends = [ Pictures [ dot p 7 cOr, txt (fst p + 9) (snd p - 8) 14 cInk nm ] | (p, nm) <- [(a, "a"), (b, "b")] ]
    gx = envW env * 0.6
    gy = envH env * 0.62
    gw = envW env * 0.36
    gh = envH env * 0.26
    vs = [phi (fromIntegral i / 100) | i <- [0 .. 100 :: Int]]
    v0 = getAt 0 vs 0
    v100 = getAt 0 vs 100
    lo = foldl' min (1 / 0) vs
    hi = foldl' max (-1 / 0) vs + 1e-9
    yS z = gy + gh - d2f ((z - lo) / (hi - lo)) * gh
    xI i = gx + gw * fromIntegral i / 100
    slice = [ Color cLine (rectLineTL gx gy gw gh)
            , Color cCy (fatLine 2 [(xI i, yS z) | (i, z) <- zip [0 :: Int ..] vs])
            , Color cOr (fatLine 2 [(gx, yS v0), (gx + gw, yS v100)]) ]
            ++ [ Color cRd (at (xI i, yS z) (disc 3)) | (i, z) <- zip [0 :: Int ..] vs, z > v0 + (v100 - v0) * fromIntegral i / 100 + 1e-12, i `mod` 3 == 0 ]
            ++ [ txt gx (gy - 8) 14 cInk "Slice phi(th) = f(a + th(b - a)) along a->b, and the chord" ]
    pics = [wCI w, Color cOr (thickLine 2.5 a b)] ++ gd ++ ends ++ slice

---------------------------------------------------------------- 操作

-- 近いほうの点（0 = a、1 = b）
pickPt :: Env -> World -> Point -> Int
pickPt env w (mx, my)
  | wDim w == 1 = let (px, _, pw, _) = plot1Rect env
                      xw = -3 + realToFrac ((mx - px) / pw) * 6
                  in if abs (xw - wA w) < abs (xw - wB w) then 0 else 1
  | otherwise = let a = toS (wView w) (wA2 w)
                    b = toS (wView w) (wB2 w)
                in if dist2 (mx, my) a < dist2 (mx, my) b then 0 else 1

dragTo :: Env -> Point -> World -> World
dragTo env m@(mx, _) w
  | wDrag w < 0 = w
  | wDim w == 1 = let (px, _, pw, _) = plot1Rect env
                      x = max (-3) (min 3 (-3 + realToFrac ((mx - px) / pw) * 6))
                  in if wDrag w == 0 then w { wA = x } else w { wB = x }
  | inside (wView w) m = let p = toW (wView w) m in if wDrag w == 0 then w { wA2 = p } else w { wB2 = p }
  | otherwise = w

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> let m = toP5 env p in dragTo env m w { wDrag = pickPt env w m }
  EventMotion p | envDown env -> dragTo env (toP5 env p) w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wFi = (wFi w + 1) `mod` n }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wFi = (wFi w + n - 1) `mod` n }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> rebuild env w { wDim = 3 - wDim w, wFi = 0 }
    | c `elem` "gG" -> w { wShowGD = not (wShowGD w) }
    | c `elem` "tT" -> w { wShowTan = not (wShowTan w) }
  _ -> w
  where n = if wDim w == 1 then length fns1 else length fns2

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  concat [ test1 f c | (f, c) <- zip fns1 [True, True, True, True, False, False, False] ]
  ++ [ stOk "x^2: all 13 starts go to 0 (local min = global min)" (all (\(_, e) -> abs e < 1e-6) (localMins1 (fns1 !!! 0))) ""
     , stOk "x^4/4 - x^2: the ends split between +sqrt2 and -sqrt2" (any (\e -> abs (e - sqrt 2) < 1e-6) m4 && any (\e -> abs (e + sqrt 2) < 1e-6) m4) ""
     , stOk "x^3/6: keeps going down and diverges (no minimum)" (any (isNaN . snd) (localMins1 (fns1 !!! 6))) "" ]
  ++ concat [ test2 f c | (f, c) <- zip fns2 [True, True, False, False] ]
  ++ [ stOk "log-sum-exp: all 35 starts go to (-ln2/2, 0), f* = (3/2)ln2 - 0.1"
         (all (\(_, e) -> vlen (vsub e (-log 2 / 2, 0)) < 1e-6) (localMins2 lse) && abs (fnF lse (-log 2 / 2) 0 - fnFs lse) < 1e-14) ""
     , stOk "Himmelblau: the ends split among the 4 minima (4 local minima)" (hit == 4) (show hit ++ " / 4")
     , stOk "Rosenbrock: Hessian at (0, 1) has a positive and a negative eigenvalue (not convex)" (eL1 e01 < 0 && eL2 e01 > 0) "" ]
  where
    (!!!) xs i = getAt (Fn1 "" id id id False) xs i
    m4 = map snd (localMins1 (fns1 !!! 4))
    hm = map snd (localMins2 himmel)
    hit = length [q | q <- fnXs himmel, any (\p -> vlen (vsub p q) < 1e-6) hm]
    e01 = eig2 (fnH rosen 0 1)
    test1 f convex =
      let h = 1e-5
          e = foldl' max 0 [ max (abs (f1D1 f x - (f1F f (x + h) - f1F f (x - h)) / (2 * h))) (abs (f1D2 f x - (f1D1 f (x + h) - f1D1 f (x - h)) / (2 * h)))
                           | x <- [-2.3, -0.7, 0.4, 1.9] ]
          (sg, sd) = scan1 f 200 (rngList 5)
          tang = and [ f1F f y >= f1F f x + f1D1 f x * (y - x) - 1e-12
                     | (x, y) <- take 200 (pairs (rngList 9)), not (f1Kink f && abs x < 1e-9) ]
      in [ stOk (f1Name f ++ ": f', f'' = finite differences") (e < 1e-6) (fmt 3 e)
         , stOk (f1Name f ++ ": chord test says " ++ (if convex then "convex (no violation), tangent always below" else "not convex (some pair violates)") ++ ", agrees with sign of f''")
             ((sg <= 1e-12) == convex && (sd >= 0) == convex && (not convex || tang)) ("max(graph - chord) " ++ fmt 3 sg) ]
    pairs (a : b : xs) = (6 * a - 3, 6 * b - 3) : pairs xs
    pairs _ = []
    test2 f convex =
      let (eg, eh) = derivErr f [(0.3, -0.4), (-1.1, 0.8), (1.7, 1.2)]
          (sg, sl) = scan2 f 200 (rngList 3)
      in [ stOk (fnName f ++ ": gradient, Hessian = finite differences") (eg < 1e-5 && eh < 1e-5) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
         , stOk (fnName f ++ ": " ++ (if convex then "convex (no chord violation, lambda_min(H) >= 0)" else "not convex (chord violation, lambda_min(H) < 0 somewhere)"))
             ((sg <= 1e-10) == convex && (sl >= -1e-12) == convex) ("max(graph - chord) " ++ fmt 3 sg ++ ", lambda_min " ++ fmt 3 sl) ]

-- ---------- やってみよう ----------
-- 1. x^4/4 − x^2 で a と b を ±2 にすると弦は上。a = −1.5, b = 1.5 にすると？ 「どの 2 点でも」が大事
-- 2. |x| はとがっていて f'' が定義できない点があるが凸。弦の検査は微分を使わないので、そのまま使える
-- 3. T の接線（黄色）：凸なら接線はいつもグラフの下（1 次の条件 f(y) ≥ f(x) + f'(x)(y − x)）
-- 4. 2 次元のローゼンブロックはピンク（ヘッセ行列が負の固有値をもつ所）がある → 凸ではないが、局所最小は 1 つだけ
-- 5. 自分の関数を fns1 に足してみよう（f, d1 = f', d2 = f'' を書く。自己テストが微分をチェックしてくれる）

-- ---------- 道具 ----------
-- 線分 p→q を y の範囲 [y0, y1] に切りつめる
clipY :: Float -> Float -> Point -> Point -> Maybe (Point, Point)
clipY y0 y1 (x1, ya) (x2, yb)
  | t0 > t1 = Nothing
  | otherwise = Just (pt t0, pt t1)
  where
    dy = yb - ya
    (t0, t1) | abs dy < 1e-9 = if ya >= y0 && ya <= y1 then (0, 1) else (1, 0)
             | otherwise = let ta = (y0 - ya) / dy
                               tb = (y1 - ya) / dy
                           in (max 0 (min ta tb), min 1 (max ta tb))
    pt t = (x1 + t * (x2 - x1), ya + t * dy)

-- ==================================================
-- ここから下は「最適化の道具」（中身は読まなくても OK。消さないでね）
-- テスト関数（値・勾配・ヘッセ行列）、等高線、対数グラフ、2×2 行列、直線探索、自己テスト
-- 計算はすべて Double（倍精度）で行い、描くときだけ Float にする（1e-10 より小さい誤差まで測るため）
-- ==================================================

-- ---- 色（p5.js 版の PAL）
cBg, cInk, cDim, cLine, cOr, cCy, cGr, cYe, cPk, cVi, cRd :: Color
cBg = rgb255 12 36 69
cInk = rgb255 232 242 251
cDim = rgb255 142 178 207
cLine = rgb255 44 90 133
cOr = rgb255 255 122 47
cCy = rgb255 95 216 240
cGr = rgb255 141 224 122
cYe = rgb255 255 210 74
cPk = rgb255 255 111 168
cVi = rgb255 183 156 255
cRd = rgb255 255 90 90

-- ---- 2 次元のベクトルと 2×2 行列
type V2 = (Double, Double)
data M2 = M2 !Double !Double !Double !Double     -- [[a, b], [c, d]]

vadd, vsub :: V2 -> V2 -> V2
vadd (a, b) (c, d) = (a + c, b + d)
vsub (a, b) (c, d) = (a - c, b - d)

vscale :: Double -> V2 -> V2
vscale k (a, b) = (k * a, k * b)

vaxpy :: Double -> V2 -> V2 -> V2    -- a·x + y
vaxpy k (a, b) (c, d) = (c + k * a, d + k * b)

vdot :: V2 -> V2 -> Double
vdot (a, b) (c, d) = a * c + b * d

vlen :: V2 -> Double
vlen (a, b) = hypot a b

hypot :: Double -> Double -> Double
hypot a b = let m = max (abs a) (abs b) in if m == 0 || isInfinite m then m else m * sqrt ((a / m) ^ (2 :: Int) + (b / m) ^ (2 :: Int))

mvec :: M2 -> V2 -> V2
mvec (M2 a b c d) (x, y) = (a * x + b * y, c * x + d * y)

-- 描くために Float へ（NaN は 0、とても大きい値は ±1e5 に切りつめる）
d2f :: Double -> Float
d2f v = if isNaN v then 0 else realToFrac (max (-1e5) (min 1e5 v))

-- 発散したか（NaN・無限大・とても大きい）
bad :: Double -> Bool
bad v = isNaN v || isInfinite v || abs v > 1e12

finite :: Double -> Bool
finite v = not (isNaN v || isInfinite v)

-- 対称行列の固有値（小さい順 l1 ≤ l2）と固有ベクトル
data Eig = Eig { eL1, eL2 :: !Double, eV1, eV2 :: !V2 }

eig2 :: M2 -> Eig
eig2 (M2 a b _ d) = Eig l1 (t + r) v (negate (snd v), fst v)
  where
    t = (a + d) / 2
    r = hypot ((a - d) / 2) b
    l1 = t - r
    v0 | abs b > 1e-14 * (abs a + abs d + 1) = (l1 - d, b)
       | a <= d = (1, 0)
       | otherwise = (0, 1)
    v = vscale (1 / vlen v0) v0

solve2 :: M2 -> V2 -> V2
solve2 (M2 a b c d) (r0, r1) = let det = a * d - b * c in ((d * r0 - b * r1) / det, (a * r1 - c * r0) / det)

-- ---- テスト関数：値 f、勾配 g、ヘッセ行列 h、最小点 xs、最小値 fs、表示範囲 view（中心 x, 中心 y, 横の半幅）
data Fn = Fn
  { fnName  :: String
  , fnF     :: Double -> Double -> Double
  , fnG     :: Double -> Double -> V2
  , fnH     :: Double -> Double -> M2
  , fnXs    :: [V2]
  , fnFs    :: Double
  , fnView  :: (Double, Double, Double)
  , fnStart :: V2
  , fnMu    :: Double            -- 強凸性 μ（0 = わからない）
  , fnL     :: Double            -- 勾配のリプシッツ定数 L（0 = わからない）
  , fnConvex :: Bool
  , fnTh    :: Double            -- 2 次関数の回転角
  }

fn0 :: Fn
fn0 = Fn "" (\_ _ -> 0) (\_ _ -> (0, 0)) (\_ _ -> M2 0 0 0 0) [] 0 (0, 0, 1) (0, 0) 0 0 False 0

-- f = ½ xᵀAx、A の固有値は μ = 1 と L = κ（θ だけ回転）
quadFn :: Double -> Double -> Fn
quadFn kap th = fn0
  { fnName = "quadratic kappa = " ++ fmt 3 kap
  , fnF = \x y -> 0.5 * (a * x * x + 2 * b * x * y + d * y * y)
  , fnG = \x y -> (a * x + b * y, b * x + d * y)
  , fnH = \_ _ -> M2 a b b d
  , fnMu = 1, fnL = kap, fnXs = [(0, 0)], fnFs = 0, fnView = (0, 0, 3.2), fnStart = (-2.6, 1.6), fnConvex = True, fnTh = th }
  where
    c = cos th
    s = sin th
    a = c * c + kap * s * s
    b = (1 - kap) * c * s
    d = s * s + kap * c * c

rosen :: Fn
rosen = fn0
  { fnName = "Rosenbrock"
  , fnF = \x y -> (1 - x) ^ (2 :: Int) + 100 * (y - x * x) ^ (2 :: Int)
  , fnG = \x y -> (-2 * (1 - x) - 400 * x * (y - x * x), 200 * (y - x * x))
  , fnH = \x y -> M2 (2 - 400 * (y - x * x) + 800 * x * x) (-400 * x) (-400 * x) 200
  , fnXs = [(1, 1)], fnFs = 0, fnView = (-0.2, 0.9, 2.1), fnStart = (-1.2, 1) }

himmel :: Fn
himmel = h { fnXs = map (\p -> fromMaybe p (criticalPoint h p 60)) (fnXs h) }   -- 4 つの最小点を ∇f = 0 のニュートン法で精密に
  where
    h = fn0
      { fnName = "Himmelblau"
      , fnF = \x y -> (x * x + y - 11) ^ (2 :: Int) + (x + y * y - 7) ^ (2 :: Int)
      , fnG = \x y -> (4 * x * (x * x + y - 11) + 2 * (x + y * y - 7), 2 * (x * x + y - 11) + 4 * y * (x + y * y - 7))
      , fnH = \x y -> M2 (12 * x * x + 4 * y - 42) (4 * x + 4 * y) (4 * x + 4 * y) (4 * x + 12 * y * y - 26)
      , fnXs = [(3, 2), (-2.8051, 3.1313), (-3.7793, -3.2832), (3.5844, -1.8481)], fnFs = 0, fnView = (0, 0, 6.2), fnStart = (-0.3, -0.9) }

beale :: Fn
beale = fn0
  { fnName = "Beale"
  , fnF = \x y -> sum [r * r | (r, _, _, _, _) <- terms x y]
  , fnG = \x y -> foldl' (\(p, q) (r, rx, ry, _, _) -> (p + 2 * r * rx, q + 2 * r * ry)) (0, 0) (terms x y)
  , fnH = \x y -> let (a, b, d) = foldl' (\(a0, b0, d0) (r, rx, ry, rxy, ryy) -> (a0 + 2 * rx * rx, b0 + 2 * (rx * ry + r * rxy), d0 + 2 * (ry * ry + r * ryy))) (0, 0, 0) (terms x y)
                  in M2 a b b d
  , fnXs = [(3, 0.5)], fnFs = 0, fnView = (0.6, 0, 4.6), fnStart = (-1.5, 1.8) }
  where
    terms x y = [ (c - x + x * y ^ n, y ^ n - 1, fromIntegral n * x * y ^ (n - 1), fromIntegral n * y ^ (n - 1)
                  , if n > 1 then fromIntegral (n * (n - 1)) * x * y ^ (n - 2) else 0)
                | (c, n) <- zip [1.5, 2.25, 2.625] [1 :: Int ..] ]

-- log(e^(x+3y−0.1) + e^(x−3y−0.1) + e^(−x−0.1))：凸。最小点 (−ln2 / 2, 0)、最小値 (3/2)ln2 − 0.1
lse :: Fn
lse = fn0
  { fnName = "log-sum-exp"
  , fnF = \x y -> fst (pr x y)
  , fnG = \x y -> let (p0, p1, p2) = snd (pr x y) in (p0 + p1 - p2, 3 * p0 - 3 * p1)
  , fnH = \x y -> let (p0, p1, p2) = snd (pr x y)
                      g0 = p0 + p1 - p2
                      g1 = 3 * p0 - 3 * p1
                  in M2 (1 - g0 * g0) (g1 - g0 * g1) (g1 - g0 * g1) (9 * (p0 + p1) - g1 * g1)
  , fnXs = [(-log 2 / 2, 0)], fnFs = 1.5 * log 2 - 0.1, fnView = (-0.4, 0, 2.4), fnStart = (-2, 1.1), fnConvex = True, fnL = 10 }
  where
    pr x y =
      let z0 = x + 3 * y - 0.1
          z1 = x - 3 * y - 0.1
          z2 = -x - 0.1
          m = max z0 (max z1 z2)
          e0 = exp (z0 - m)
          e1 = exp (z1 - m)
          e2 = exp (z2 - m)
          s = e0 + e1 + e2
      in (m + log s, (e0 / s, e1 / s, e2 / s))

-- 最小点までの距離（最小点が複数なら近いほう）
errTo :: Fn -> V2 -> Double
errTo f p = foldl' min (1 / 0) [vlen (vsub p q) | q <- fnXs f]

-- ---- 微分のチェック（中心差分）。H は「解析的な勾配」の差分と比べる
fdGrad :: (Double -> Double -> Double) -> Double -> Double -> Double -> V2
fdGrad fn x y h = ((fn (x + h) y - fn (x - h) y) / (2 * h), (fn x (y + h) - fn x (y - h)) / (2 * h))

derivErr :: Fn -> [V2] -> (Double, Double)
derivErr f pts = foldl' one (0, 0) pts
  where
    h = 1e-6
    rel a b = abs (a - b) / max 1 (abs b)
    one (eg, eh) (x, y) =
      let (g0, g1) = fnG f x y
          (n0, n1) = fdGrad (fnF f) x y h
          M2 ha hb hc hd = fnH f x y
          (ax1, ay1) = fnG f (x + h) y
          (ax0, ay0) = fnG f (x - h) y
          (bx1, by1) = fnG f x (y + h)
          (bx0, by0) = fnG f x (y - h)
          nm = [(ax1 - ax0) / (2 * h), (bx1 - bx0) / (2 * h), (ay1 - ay0) / (2 * h), (by1 - by0) / (2 * h)]
      in (maximum [eg, rel g0 n0, rel g1 n1], maximum (eh : zipWith rel [ha, hb, hc, hd] nm))

-- ∇f = 0 をニュートン法で解く（停留点を探す）。見つからなければ Nothing
criticalPoint :: Fn -> V2 -> Int -> Maybe V2
criticalPoint f p0 it = go 0 p0
  where
    go k p
      | k >= it = done p
      | not (finite (fst d + snd d)) = Nothing
      | vlen d < 1e-14 = done p'
      | otherwise = go (k + 1) p'
      where
        d = solve2 (uncurry (fnH f) p) (uncurry (fnG f) p)
        p' = vsub p d
    done p = if vlen (uncurry (fnG f) p) < 1e-8 then Just p else Nothing

-- アルミホの条件 f(x + t d) ≤ f(x) + c₁ t ∇fᵀd になるまで t を β 倍（試した点も返す）
data Armijo = Armijo { arT :: !Double, arTries :: [(Double, Double)], arSlope :: !Double }

armijo :: Fn -> V2 -> V2 -> Double -> Double -> Double -> Armijo
armijo f p d c1 beta t0 = go (0 :: Int) t0 []
  where
    f0 = uncurry (fnF f) p
    sl = vdot (uncurry (fnG f) p) d
    go k t acc
      | k >= 60 = Armijo t (reverse acc) sl
      | fq <= f0 + c1 * t * sl = Armijo t (reverse acc') sl
      | otherwise = go (k + 1) (t * beta) acc'
      where
        fq = uncurry (fnF f) (vaxpy t d p)
        acc' = (t, fq) : acc

armijo0 :: Fn -> V2 -> V2 -> Armijo
armijo0 f p d = armijo f p d 1e-4 0.5 1

-- ---- 画面と世界の座標（縦横の縮尺は同じ）。画面は p5.js と同じ（左上が原点、y は下向き）
data View = View { vPx, vPy, vPw, vPh, vCx, vCy, vHw, vSc, vHh :: !Double }

makeView :: Double -> Double -> Double -> Double -> (Double, Double, Double) -> View
makeView px py pw ph (cx, cy, hw) = View px py pw' ph' cx cy hw sc (ph' / (2 * sc))
  where
    pw' = fromIntegral (floor (pw + 0.5) :: Int)
    ph' = fromIntegral (floor (ph + 0.5) :: Int)
    sc = pw' / (2 * hw)

toSD :: View -> V2 -> V2
toSD v (x, y) = (vPx v + vPw v / 2 + (x - vCx v) * vSc v, vPy v + vPh v / 2 - (y - vCy v) * vSc v)

toS :: View -> V2 -> Point
toS v p = let (a, b) = toSD v p in (d2f a, d2f b)

toWD :: View -> V2 -> V2
toWD v (sx, sy) = (vCx v + (sx - vPx v - vPw v / 2) / vSc v, vCy v - (sy - vPy v - vPh v / 2) / vSc v)

toW :: View -> Point -> V2
toW v (sx, sy) = toWD v (realToFrac sx, realToFrac sy)

inside :: View -> Point -> Bool
inside v (sx, sy) = let x = realToFrac sx; y = realToFrac sy in x >= vPx v && x <= vPx v + vPw v && y >= vPy v && y <= vPy v + vPh v

-- 等高線の画像：格子で値を計算 → 分位点で段を決め、色ぬり ＋ マーチングスクエアで線（関数を変えたときだけ呼ぶ）
--   返すのは（絵、段の値）。絵は view の中（p5.js の座標）でそのまま使う
contourImage :: View -> (Double -> Double -> Double) -> Int -> (Picture, [Double])
contourImage v fz nLev = contourImageT v fz nLev (\_ _ -> Nothing)

-- tint x y = Just (r, g, b, 透明度 0〜1) なら、その場所の色に重ねる
contourImageT :: View -> (Double -> Double -> Double) -> Int -> (Double -> Double -> Maybe (Double, Double, Double, Double)) -> (Picture, [Double])
contourImageT v fz nLev tint = (Pictures ([image, Color (rgba255 120 200 240 31) (Pictures gridLines)] ++ map contourOf levIx), levs)
  where
    cs = 5 :: Double
    nx = ceiling (vPw v / cs) + 1 :: Int
    ny = ceiling (vPh v / cs) + 1 :: Int
    zs = U.generate (nx * ny) $ \q ->
      let (j, i) = q `divMod` nx
          (x, y) = toWD v (vPx v + fromIntegral i * cs, vPy v + fromIntegral j * cs)
          z = fz x y
      in if finite z then z else 1e300
    zAt i j = zs U.! (j * nx + i)
    srt = U.fromList (sort (filter (< 1e300) (U.toList zs)))
    ns = U.length srt
    levs = if ns == 0 then [] else
      [ srt U.! min (ns - 1) (floor (fromIntegral ns * (fromIntegral k / fromIntegral (nLev + 1)) ** (1.7 :: Double))) | k <- [1 .. nLev] ]
    bands = U.map (\z -> if z >= 1e300 then 255 else length (takeWhile (< z) levs)) zs :: U.Vector Int
    -- 色ぬり（8 万画素以下になるように拡大率 k を選ぶ）
    k = getAt 5 [kk | kk <- [1, 2, 2.5, 4, 5], fromIntegral (iw kk * ih kk) <= (80000 :: Double)] 0
    iw kk = max 1 (floor (vPw v / kk)) :: Int
    ih kk = max 1 (floor (vPh v / kk)) :: Int
    w' = iw k
    h' = ih k
    pix x y =
      let sx = (fromIntegral x + 0.5) * k
          sy = (fromIntegral y + 0.5) * k
          i = min (nx - 1) (floor (sx / cs + 0.5))
          j = min (ny - 1) (floor (sy / cs + 0.5))
          b = bands U.! (j * nx + i)
          t = if b == 255 then 1.25 else fromIntegral b / fromIntegral nLev
          base = (14 + 34 * (1 - t), 40 + 62 * (1 - t), 72 + 70 * (1 - t))
          (cr, cg, cb) = case uncurry tint (toWD v (vPx v + sx, vPy v + sy)) of
            Nothing -> base
            Just (r, g, bb, a) -> let (r0, g0, b0) = base in (r0 * (1 - a) + r * a, g0 * (1 - a) + g * a, b0 * (1 - a) + bb * a)
          q c = fromIntegral (max 0 (min 255 (floor c :: Int)))
      in (q cr, q cg, q cb)
    image = Translate (realToFrac (vPx v + fromIntegral w' * k / 2)) (realToFrac (vPy v + fromIntegral h' * k / 2))
              (Scale 1 (-1) (pixelImage w' h' (realToFrac k) pix))
    -- 方眼（製図用紙）
    gs0 = 10 ** fromIntegral (floor (logBase 10 (vHw v / 2) + 1e-12) :: Int)
    gs1 = if vHw v / gs0 > 10 then gs0 * 2 else gs0
    gs = if vHw v / gs1 > 10 then gs1 * 2.5 else gs1
    gridLines =
      [ Line [(sx, realToFrac (vPy v)), (sx, realToFrac (vPy v + vPh v))]
      | x <- takeWhile (< vCx v + vHw v) [fromIntegral (ceiling ((vCx v - vHw v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sx = fst (toS v (x, 0)) ]
      ++
      [ Line [(realToFrac (vPx v), sy), (realToFrac (vPx v + vPw v), sy)]
      | y <- takeWhile (< vCy v + vHh v) [fromIntegral (ceiling ((vCy v - vHh v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sy = snd (toS v (0, y)) ]
    -- マーチングスクエア：ます目の 4 隅の最小〜最大にかかる段だけ調べ、線分を辺の番号でつないで長い折れ線にする
    hid i j = 2 * (j * nx + i)
    vid i j = 2 * (j * nx + i) + 1
    levIx = zip [0 :: Int ..] levs
    segMap = IM.fromListWith (++) (concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ])
    cellSegs i j =
      let a = zAt i j
          b = zAt (i + 1) j
          c = zAt (i + 1) (j + 1)
          d = zAt i (j + 1)
          lo = min (min a b) (min c d)
          hi = max (max a b) (max c d)
          fi = fromIntegral i :: Double
          fj = fromIntegral j :: Double
          one lv =
            let up z = z > lv
                ps = [ (hid i j, (fi + (lv - a) / (b - a), fj)) | up a /= up b ]
                  ++ [ (vid (i + 1) j, (fi + 1, fj + (lv - b) / (c - b))) | up b /= up c ]
                  ++ [ (hid i (j + 1), (fi + (lv - d) / (c - d), fj + 1)) | up d /= up c ]
                  ++ [ (vid i j, (fi, fj + (lv - a) / (d - a))) | up a /= up d ]
            in pairUp ps
      in [ (li, ss) | (li, lv) <- takeWhile ((< hi) . snd) (dropWhile ((< lo) . snd) levIx), let ss = one lv, not (null ss) ]
    pairUp (p : q : rest) = (p, q) : pairUp rest
    pairUp _ = []
    scr (gx, gy) = (d2f (vPx v + gx * cs), d2f (vPy v + gy * cs)) :: Point
    contourOf (li, _) = Color (rgba255 110 215 250 140) (Pictures [Line (map scr c) | c <- chains (IM.findWithDefault [] li segMap)])

-- 線分（端の番号つき）をつないで折れ線のリストにする
chains :: [((Int, V2), (Int, V2))] -> [[V2]]
chains ss = go starts IS.empty
  where
    adj = IM.fromListWith (++) (concat [[(ka, [kb]), (kb, [ka])] | ((ka, _), (kb, _)) <- ss])
    pos = IM.fromList (concat [[(ka, pa), (kb, pb)] | ((ka, pa), (kb, pb)) <- ss])
    starts = [k | (k, ns) <- IM.toList adj, length ns == 1] ++ IM.keys adj
    nbrs k = IM.findWithDefault [] k adj
    go [] _ = []
    go (s : rest) seen
      | IS.member s seen = go rest seen
      | otherwise = let (path, seen') = walk s s (IS.insert s seen) [s]
                    in map (\k -> IM.findWithDefault (0, 0) k pos) path : go rest seen'
    walk s cur seen acc = case [n | n <- nbrs cur, not (IS.member n seen)] of
      (n : _) -> walk s n (IS.insert n seen) (n : acc)
      [] -> (if length acc > 2 && s `elem` nbrs cur then s : acc else acc, seen)

-- ---- 対数グラフ：0 以下・NaN は描かない
data Series = Series { sV :: [Double], sC :: Color, sName :: String, sDash :: Bool, sWt :: Float }

series :: [Double] -> Color -> String -> Series
series vs c nm = Series vs c nm False 2

data PlotOpt = PlotOpt { poLo, poHi :: Maybe Double, poLogx :: Bool, poTitle :: String, poKmax :: Maybe Int, poXlabel :: String }

plotOpt :: PlotOpt
plotOpt = PlotOpt Nothing Nothing False "" Nothing "iteration k"

logPlot :: Float -> Float -> Float -> Float -> [Series] -> PlotOpt -> Picture
logPlot x y w h ss opt = Pictures $
  [ Color (rgba255 8 26 50 230) (rectTL x y w h), Color cLine (rectLineTL x y w h) ]
  ++ concat [ [ Color (rgb255 40 80 120) (Line [(x, yy), (x + w, yy)]), txtR (x - 4) (yy + 5) 14 cDim ("1e" ++ show e) ]
            | e <- takeWhile (<= hi) [lo, lo + de ..], let yy = yOf (10 ^^ e) ]
  ++ [ txtC (xOf k) (y + h + 12) 14 cDim (show k) | k <- kt ]
  ++ [ txtC (x + w / 2) (y + h + 30) 14 cDim (poXlabel opt) ]
  ++ map curve ss
  ++ concat [ [ txtR (x + w - 34) (yy + 12) 14 (sC s) (sName s)
              , Color (sC s) (if sDash s then dashLine 2 4 3 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)] else fatLine 2 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)]) ]
            | (i, s) <- zip [0 :: Int ..] (filter (not . null . sName) ss), let yy = y + 6 + 17 * fromIntegral i ]
  ++ [ txt x (y - 6) 14 cInk (poTitle opt) | not (null (poTitle opt)) ]
  where
    good v = v > 0 && finite v
    allV = [logBase 10 v | s <- ss, v <- sV s, good v]
    mn = foldl' min (1 / 0) allV
    mx = foldl' max (-1 / 0) allV
    kmax = case poKmax opt of
      Just km | km > 0 -> km
      _ -> maximum (1 : [length (sV s) - 1 | s <- ss])
    lo = case poLo opt of
      Just l -> floor l
      Nothing -> if null allV then -16 else max (-17) (floor mn) :: Int
    hi0 = case poHi opt of
      Just hv -> ceiling hv
      Nothing -> if null allV then 2 else ceiling mx :: Int
    hi = if hi0 <= lo then lo + 1 else hi0
    de = max 1 (ceiling (fromIntegral (hi - lo) / 8 :: Double)) :: Int
    xOf :: Int -> Float
    xOf k = x + w * (if poLogx opt then realToFrac (log (1 + fromIntegral k) / log (1 + fromIntegral kmax) :: Double) else fromIntegral k / fromIntegral kmax)
    yOf :: Double -> Float
    yOf v = y + h - h * d2f ((logBase 10 v - fromIntegral lo) / fromIntegral (hi - lo))
    kt = if poLogx opt then filter (<= kmax) [1, 10, 100, 1000, 10000] else [0, (kmax + 1) `div` 2, kmax]
    curve s = Color (sC s) (Pictures [ case run of
                                         [p] -> at p (disc (sWt s / 2 + 0.5))     -- 1 点だけなら点を打つ
                                         _ -> if sDash s then dashLine (sWt s) 6 5 run else fatLine (sWt s) run
                                     | run <- runs s ])
    runs s = filter (not . null) (splitRuns [ if good v then Just (xOf k, max y (min (y + h) (yOf v))) else Nothing | (k, v) <- zip [0 .. kmax] (sV s) ])
    splitRuns ps = case break (== Nothing) ps of
      (a, []) -> [[p | Just p <- a]]
      (a, _ : rest) -> [p | Just p <- a] : splitRuns rest

-- ---- 描く道具
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss の座標 → p5.js の座標（左上が原点、y は下向き）
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

p5Mouse :: Env -> Point
p5Mouse env = toP5 env (envMouse env)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c s

-- 右端が x、下が y の文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size c s = txt (x - fromIntegral (length s) * size * 0.72) y size c s

-- 左上が (x, y) の長方形（p5.js の rect）：塗り・枠
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = rectLine (x + w / 2) (y + h / 2) w h

-- 太さ w の折れ線（細い線を少しずつずらして重ねる。点が多くても描く要素が少なくてすむ）
fatLine :: Float -> [Point] -> Picture
fatLine w ps
  | w <= 1.5 = Line ps
  | otherwise = Pictures [Translate dx dy (Line ps) | (dx, dy) <- (0, 0) : concat [[(o, 0), (-o, 0), (0, o), (0, -o)] | o <- offs]]
  where offs = takeWhile (<= (w - 1) / 2 + 1e-3) [0.5, 1 ..]

-- 点線（on 描いて off あける）
dashLine :: Float -> Float -> Float -> [Point] -> Picture
dashLine w on off ps = Pictures (map (fatLine w) (go True on (zip ps (drop 1 ps)) []))
  where
    go _ _ [] cur = [reverse cur | length cur >= 2]
    go drawing left ((a, b) : rest) cur
      | l < 1e-6 = go drawing left rest cur
      | left >= l = go drawing (left - l) rest (if drawing then b : start a cur else [])
      | otherwise =
          let m = (fst a + (fst b - fst a) * left / l, snd a + (snd b - snd a) * left / l)
          in if drawing then reverse (m : start a cur) : go False off ((m, b) : rest) []
             else go True on ((m, b) : rest) [m]
      where l = dist2 a b
    start a cur = if null cur then [a] else cur

-- 矢印（色 c、太さ w）
arrowP :: Point -> Point -> Color -> Float -> Picture
arrowP (x1, y1) (x2, y2) c w = Color c (Pictures ([thickLine w (x1, y1) (x2, y2)] ++ [Polygon [(x2, y2), hd (-0.4), hd 0.4] | l > 2]))
  where
    a = atan2 (y2 - y1) (x2 - x1)
    l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    hl = min 12 (l * 0.4)
    hd d = (x2 - hl * cos (a + d), y2 - hl * sin (a + d))

-- 白いふちの丸（半径 r）
dot :: Point -> Float -> Color -> Picture
dot p r c = at p (Pictures [Color white (disc r), Color c (disc (r - 1.2))])

-- 反復点の列を折れ線と点で
drawPath :: View -> [V2] -> Color -> Float -> Bool -> Picture
drawPath _ [] _ _ _ = Blank
drawPath v ps c w dots = Pictures ([Color c (fatLine w ss) | length ss >= 2] ++ ds ++ [dot (last ss) 5.5 c])
  where
    ss = map (toS v) ps
    stepN = max 1 (length ss `div` 60)
    ds = if dots then [Color c (at s (disc 2.5)) | (i, s) <- zip [0 :: Int ..] ss, i `mod` stepN == 0] else []

-- 文字の列（左上が (x, y)、大きさ sz、行の間隔 lh）
hud :: [(String, Color)] -> Float -> Float -> Float -> Float -> Picture
hud ls x y sz lh = Pictures [txt x (y + fromIntegral i * sz * lh + sz) sz c s | (i, (s, c)) <- zip [0 :: Int ..] ls, not (null s)]

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 数の表示（発散・とても小さい数も）：有効数字 d 桁
fmt :: Int -> Double -> String
fmt d v
  | bad v = "diverged"
  | v == 0 = "0"
  | a < 1e-3 || a >= 1e5 = showFFloat (Just (d - 1)) (v / 10 ^^ e) "" ++ "e" ++ show e
  | otherwise =
      let r = fromIntegral (round (v / 10 ^^ (e - d + 1)) :: Integer) * 10 ^^ (e - d + 1) :: Double
      in if e - d + 1 >= 0 then show (round r :: Integer) else stripZ (showFFloat (Just (d - 1 - e)) r "")
  where
    a = abs v
    e = log10i a
    stripZ s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last t == '.' then init t else t else s

-- floor(log10 a)（10 のべきでも正確に）
log10i :: Double -> Int
log10i a = let e0 = floor (logBase 10 a) :: Int in if 10 ^^ (e0 + 1) <= a then e0 + 1 else if 10 ^^ e0 > a then e0 - 1 else e0

-- 決まった種から同じ乱数の列（p5.js 版と同じ mulberry32）
rngList :: Int -> [Double]
rngList seed = go (fromIntegral seed :: Word32)
  where
    go s0 =
      let s = s0 + 0x6d2b79f5
          t1 = (s `xor` (s `shiftR` 15)) * (1 .|. s)
          t2 = (t1 + ((t1 `xor` (t1 `shiftR` 7)) * (61 .|. t1))) `xor` t1
          r = t2 `xor` (t2 `shiftR` 14)
      in fromIntegral r / 4294967296 : go s

-- ---- 自己テスト（起動時に、知られている答えと照合する）：（項目, 合格か, 実測値など）
type Test = (String, Bool, String)

stOk :: String -> Bool -> String -> Test
stOk name ok info = (name, ok, info)

-- 結果を端末に出す（テストモードでは失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Test] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else [r | r@(_, ok, _) <- rs, not ok]
  mapM_ (\(nm, ok, info) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm ++ (if null info then "" else "  [" ++ info ++ "]"))) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [Test] -> String
selfTestLine rs = "Self-test " ++ (if allOk then "OK " else "NG ") ++ show (length [() | (_, True, _) <- rs]) ++ " / " ++ show (length rs)
  where allOk = and [ok | (_, ok, _) <- rs]

drawSelfTest :: Env -> [Test] -> Picture
drawSelfTest env rs = txt 16 (envH env - 12) 14 (if and [ok | (_, ok, _) <- rs] then cGr else cRd) (selfTestLine rs)
