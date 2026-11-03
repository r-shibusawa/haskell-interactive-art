-- ============================================
-- 06 多項式の 2 つの表し方：係数か、点の値か
-- 次数 n−1 の多項式は「n 個の係数」でも「n 個の点（x, y）」でも 1 つに決まる
-- 係数 → 点：ホーナー法で 1 点 Θ(n)。点 → 係数：ラグランジュ補間。
-- かけ算は点の値なら点ごとに掛けるだけ（Θ(n)）。だから「速く変換する方法」= FFT（07〜09）が欲しい
-- ドラッグ：点を動かす / クリック：点を追加 / D：マウスに近い点を消す / マウスの横位置：ホーナー法で計算する x
-- L：ラグランジュの基底を表示 / M：かけ算モード / N：ランダム / C：2 点に戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.Char (toLower)
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

hornerSpeed :: Int
hornerSpeed = 40 -- ★ ホーナー法の 1 段の表示時間（フレーム）

type V2 = (Double, Double)

data World = World
  { wPts    :: ![V2]
  , wCoef   :: ![Double]
  , wOk     :: !Bool
  , wDrag   :: !Int
  , wShowL  :: !Bool
  , wMul    :: !Bool
  , wHornT  :: !Int
  , wLastX0 :: !(Maybe Double)
  }

-- かけ算モードで掛ける相手 B(x) の点
bPts :: [V2]
bPts = [(-1, 0.6), (0, -0.4), (1, 0.8)]

main :: IO ()
main = runSketch (sketch "16-06 Polynomials" initialWorld)
  { skBack  = bgC
  , skStep  = \env _ w -> pure (stepW env w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = interp (World [(-0.9, -0.3), (-0.3, 0.6), (0.3, -0.2), (0.9, 0.5)] [] True (-1) False False 0 Nothing)

polyMul :: [Double] -> [Double] -> [Double]
polyMul p q =
  [ sum [pi' * qj | (i, pi') <- zip [0 ..] p, (j, qj) <- zip [0 ..] q, i + j == k]
  | k <- [0 .. length p + length q - 2 :: Int] ]

-- k 番目の基底の分子（の係数）と分母：Π_{j≠k} (x − x_j) / (x_k − x_j)
basis :: [V2] -> Int -> ([Double], Double)
basis ps k =
  let xk = fst (ps !! k)
      others = [x | (j, (x, _)) <- zip [0 ..] ps, j /= k]
  in (foldl (\num x -> polyMul num [negate x, 1]) [1] others, product [xk - x | x <- others])

-- ラグランジュ補間：Σ y_k · Π_{j≠k} (x − x_j)/(x_k − x_j) を展開して係数にする
lagrange :: [V2] -> Maybe [Double]
lagrange ps =
  let n = length ps
      parts = map (basis ps) [0 .. n - 1]
  in if any (\(_, d) -> abs d < 1e-9) parts then Nothing
     else Just (foldl (zipWith (+)) (replicate n 0)
                  [ map (\v -> y * v / d) num | ((_, y), (num, d)) <- zip ps parts ])

interp :: World -> World
interp w = case lagrange (wPts w) of
  Just c -> w { wCoef = c, wOk = True }
  Nothing -> w { wOk = False }

horner :: [Double] -> Double -> Double
horner c x = foldr (\k v -> v * x + k) 0 c

-- グラフの枠
rectOf :: Env -> (Float, Float, Float, Float)
rectOf env = (30, 110, envW env * 0.58, envH env - 140)

sx, sy :: Env -> Double -> Float
sx env x = let (rx, _, rw, _) = rectOf env in rx + (realToFrac x + 1.2) / 2.4 * rw
sy env y = let (_, ry, _, rh) = rectOf env in ry + rh / 2 - (realToFrac y / 1.6) * (rh / 2)

wx, wy :: Env -> Float -> Double
wx env px = let (rx, _, rw, _) = rectOf env in realToFrac ((px - rx) / rw * 2.4 - 1.2)
wy env py = let (_, ry, _, rh) = rectOf env in realToFrac (negate ((py - ry - rh / 2) / (rh / 2)) * 1.6)

insideR :: Env -> Bool
insideR env =
  let (rx, ry, rw, rh) = rectOf env
      (mx, my) = mouseP env
  in mx > rx && mx < rx + rw && my > ry && my < ry + rh

curve :: Env -> [Double] -> Color -> Float -> Picture
curve env c col t =
  let xs = [-1.2 + 2.4 * fromIntegral i / 240 | i <- [0 .. 240 :: Int]]
      vals = [(x, horner c x) | x <- xs]
  in Pictures [ seg env col t (sx env x1, sy env v1) (sx env x2, sy env v2)
              | ((x1, v1), (x2, v2)) <- zip vals (drop 1 vals), abs v1 < 1.7, abs v2 < 1.7 ]

-- マウスが動いたらホーナー法を最初から見せる
stepW :: Env -> World -> World
stepW env w
  | wOk w && insideR env =
      let x0 = wx env (fst (mouseP env))
          moved = case wLastX0 w of
            Nothing -> True
            Just l -> abs (x0 - l) > 0.02
          w1 = if moved then w { wHornT = 0, wLastX0 = Just x0 } else w
      in w1 { wHornT = min 100000 (wHornT w1 + 1) }
  | otherwise = w

drawW :: Env -> World -> Picture
drawW env w =
  let (rx, ry, rw, rh) = rectOf env
      coef = wCoef w
      ps = wPts w
      axisC = rgb255 50 42 68
      lCurves
        | wOk w && wShowL w =
            [ curve env (map (\v -> v * y / d) num) (colorL k) 1.5
            | (k, (_, y)) <- zip [0 ..] ps, let (num, d) = basis ps k ]
        | otherwise = []
      colorL k = rgba255 (if (k * 67) `mod` 360 < 180 then 120 else 200) (120 + k * 15) 255 120
      mulPics = case (wMul w && wOk w, lagrange bPts) of
        (True, Just bc) ->
          let cc = polyMul coef bc
              nn = length cc
              marks = [ let x = if nn <= 1 then -1 else -1 + 2 * fromIntegral j / fromIntegral (nn - 1)
                            vc = horner coef x * horner bc x
                            yc = sy env (max (-1.6) (min 1.6 vc))
                        in Pictures [ seg env (rgba255 255 95 109 120) 1 (sx env x, sy env 0) (sx env x, yc)
                                    , dot env coral (sx env x) yc 10 ]
                      | j <- [0 .. nn - 1] ]
          in curve env bc amber 3 : curve env cc coral 4 : marks
        _ -> []
      dots = [ dot env (if i == wDrag w then white else mint) (sx env x) (sy env y) 18 | (i, (x, y)) <- zip [0 ..] ps ]
      px = rx + rw + 30
      pw = envW env - px - 30
      polyStr = "A(x) = " ++ concat (zipWith polyTerm [0 :: Int ..] coef)
      polyTerm k c
        | k == 0 = fmt c
        | otherwise = (if c >= 0 then " + " else " - ") ++ fmt (abs c) ++ "x" ++ (if k > 1 then "^" ++ show k else "")
      ptsStr = intercalate "  " ["(" ++ f2d x ++ ", " ++ f2d y ++ ")" | (x, y) <- ps]
      y1 = 112 + 28
      h1 = max 40 (paraH 15 pw polyStr)
      y2 = y1 + h1 + 10
      y3 = y2 + 28
      h3 = paraH 15 pw ptsStr
      y4 = y3 + h3 + 16
      -- ホーナー法：マウスの x で値を計算する（線と点はグラフの中、文字は右）
      hornerOn = wOk w && insideR env && not (null coef)
      x0 = wx env (fst (mouseP env))
      n = length coef
      stage = min (n - 1) (wHornT w `div` hornerSpeed)
      top = foldr (\c _ -> c) 0 (reverse coef)
      lower = drop 1 (reverse coef)   -- 上の次数から順に
      vNow = foldl (\a k -> a * x0 + k) top (take stage lower)
      hornerPlot
        | hornerOn = Pictures
            [ seg env (rgba255 255 200 90 150) 1 (sx env x0, ry) (sx env x0, ry + rh)
            , if stage == n - 1 then dot env amber (sx env x0) (sy env (max (-1.6) (min 1.6 vNow))) 16 else Blank ]
        | otherwise = Blank
      hornerText
        | hornerOn =
            let accs = drop 1 (scanl (\v k -> v * x0 + k) top lower)
                formula = foldl (\s k -> "(" ++ s ++ ")*x " ++ (if k < 0 then "- " else "+ ") ++ fmt (abs k)) (fmt top) lower
                yf = y4 + 30
                ys = yf + paraH 14 pw formula + 8
            in Pictures
                 [ txtT env px y4 18 (g8 235) ("Horner: compute A(" ++ f2d x0 ++ ")")
                 , para env px yf 14 (g8 170) pw formula
                 , txtT env px ys 16 amber ("Start: " ++ fmt top)
                 , Pictures [ txtT env px (ys + 24 + fromIntegral i * 24) 16 (if i < stage then amber else g8 90)
                                ("x " ++ f2d x0 ++ " + " ++ fmt k ++ " -> " ++ fmt a)
                            | (i, k, a) <- zip3 [0 :: Int ..] lower accs ]
                 , para env px (ys + 30 + fromIntegral n * 24) 14 (g8 150) pw
                     (show (n - 1) ++ " mults and " ++ show (n - 1) ++ " adds only (Theta(n)). No separate x^2, x^3.")
                 ]
        | otherwise = para env px y4 15 (g8 150) pw "Put the mouse on the graph to see Horner's method at that x"
      noteTxt
        | wMul w = "Yellow B(x) x green A(x) = red C(x). Multiplying coefficients is Theta(n^2); with point values just multiply at each x. The degree grows, so 2n points are needed."
        | otherwise = "n points fix exactly one polynomial of degree n-1 (the Vandermonde matrix is regular). Points to coefficients is Theta(n^2). FFT picks special x (roots of unity): Theta(n log n) both ways."
  in Pictures
       [ box env plotC rx ry rw rh
       , seg env axisC 1 (rx, sy env 0) (rx + rw, sy env 0)
       , seg env axisC 1 (sx env 0, ry) (sx env 0, ry + rh)
       , if wOk w then Blank
         else txt env (rx + 20) (ry + 30) 18 (rgb255 255 110 110) "Two points share an x: no single polynomial passes through them"
       , Pictures lCurves
       , if wOk w then curve env coef mint 4 else Blank
       , Pictures mulPics
       , Pictures dots
       , hornerPlot
       , maskOut env bgC rx ry rw rh
       , txt env 30 50 26 (g8 235) ("Polynomials: coefficients or point values" ++ (if wMul w then " [multiply]" else ""))
       , txt env 30 80 15 (g8 150) ("Drag: move  Click: add  D: delete  L: basis " ++ (if wShowL w then "ON" else "OFF")
                                     ++ "  M: multiply  N: random  C: 2 points  Mouse x: Horner")
       , txtT env px 112 18 (g8 235) "Coefficients"
       , if wOk w then para env px y1 15 mint pw polyStr else Blank
       , txtT env px y2 18 (g8 235) "Point values"
       , para env px y3 15 (g8 200) pw ptsStr
       , hornerText
       , footer env px pw 14 (g8 150) noteTxt
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | insideR env ->
        let m = mouseP env
            found = foldl (\b (i, (x, y)) -> if dist2 m (sx env x, sy env y) < 14 then i else b)
                          (-1) (zip [0 ..] (wPts w))
            (mx, my) = m
        in pure $
             if found >= 0 then w { wDrag = found }
             else if length (wPts w) < 9
               then interp w { wPts = wPts w ++ [(wx env mx, wy env my)], wDrag = length (wPts w) }
               else w { wDrag = -1 }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _
    | wDrag w >= 0 ->
        let (mx, my) = mouseP env
            p = (clampD (-1.15) 1.15 (wx env mx), clampD (-1.5) 1.5 (wy env my))
        in pure (interp w { wPts = [if i == wDrag w then p else q | (i, q) <- zip [0 ..] (wPts w)] })
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'l' -> pure w { wShowL = not (wShowL w) }
    'm' -> pure w { wMul = not (wMul w) }
    'n' -> do
      k <- randomRIO (3, 6 :: Int)
      ps <- forM [0 .. k - 1] $ \i -> do
        dx <- randomRIO (-0.08, 0.08)
        y <- randomRIO (-0.8, 0.8)
        pure (-1 + 2 * fromIntegral i / fromIntegral (k - 1) + dx, y)
      pure (interp w { wPts = ps, wDrag = -1 })
    'c' -> pure (interp w { wPts = [(-0.6, -0.4), (0.6, 0.4)], wDrag = -1 })
    'd' | length (wPts w) > 1 ->
      let m = mouseP env
          (bi, _) = minimumBy (comparing (\(_, (x, y)) -> dist2 m (sx env x, sy env y))) (zip [0 :: Int ..] (wPts w))
      in pure (interp w { wPts = [q | (i, q) <- zip [0 ..] (wPts w), i /= bi], wDrag = -1 })
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 点を 1 つ追加すると、曲線全体が変わる。係数は全部変わるか？
-- 2. L で基底を出す：基底 ℓₖ は自分の点で yₖ、ほかの点で 0。足すと緑の曲線
-- 3. 点を左右の端に近づけると、曲線が端で暴れる（ルンゲ現象）

-- ---------- 道具 ----------
fmt :: Double -> String
fmt v = if abs v < 0.005 then "0" else showFFloat (Just 2) v ""

f2d :: Double -> String
f2d v = showFFloat (Just 2) v ""

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- (x, y, w, h) の外側を塗りつぶす（描いたものを枠で切り取る）
maskOut :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
maskOut env c x y w h =
  let far = 3000
  in Pictures
       [ box env c (x - far) (y - far) far (h + 2 * far)
       , box env c (x + w) (y - far) far (h + 2 * far)
       , box env c x (y - far) w far
       , box env c x (y + h) w far ]

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v
