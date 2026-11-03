-- ============================================
-- 02 最小二乗法：点にいちばん近い曲線
-- 点が多すぎて全部を通る曲線はない → 縦のずれ（残差）の 2 乗の和が最小の多項式を選ぶ
-- 正規方程式 AᵀA c = Aᵀy を解く。AᵀA は「対称正定値」なのでコレスキー分解で安全に解ける
-- クリック：点を追加 / ドラッグ：点を動かす / 1〜6：多項式の次数 / C：全部消す / N：ランダムな点 / S：残差の正方形
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.Array
import Data.Char (digitToInt, toLower)
import Numeric (showFFloat)

-- 当てはめの結果
data Fit = Fit
  { fCoef :: ![Double]
  , fATA  :: ![[Double]]
  , fL    :: ![[Double]]
  , fSse  :: !Double
  }

data World = World
  { wDegree :: !Int           -- ★ 最初の次数（1 = 直線）
  , wSquare :: !Bool          -- ★ 残差を「正方形」で描く（面積の合計を最小にしている）
  , wPts    :: ![(Double, Double)]
  , wFit    :: !(Maybe Fit)
  , wDragI  :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-02 Least Squares" (World 1 True [] Nothing (-1)))
  { skBack  = bgC
  , skStart = \_ -> randomPoints (World 1 True [] Nothing (-1))
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomPoints :: World -> IO World
randomPoints w = do
  ps <- forM [0 .. 11 :: Int] $ \i -> do
    let x = -0.9 + 1.8 * fromIntegral i / 11 :: Double
    e <- randomRIO (-0.15, 0.15)
    pure (x, 0.6 * sin (x * 2.4) + e)
  pure (refit w { wPts = ps })

refit :: World -> World
refit w = w { wFit = fit (wDegree w) (wPts w) }

-- 正規方程式を作ってコレスキー分解（AᵀA = LLᵀ）で解く
fit :: Int -> [(Double, Double)] -> Maybe Fit
fit degree ps
  | length ps < m = Nothing
  | not (all (\j -> dg j > 1e-12) idx) = Nothing -- 正定値でない（点の x が重なりすぎ）
  | otherwise = Just (Fit coefL ataL lL sse)
  where
    m = degree + 1
    idx = [0 .. m - 1]
    pw x = [x ^ k | k <- idx]
    ata = listArray ((0, 0), (m - 1, m - 1))
            [sum [pw x !! i * pw x !! j | (x, _) <- ps] | i <- idx, j <- idx] :: Array (Int, Int) Double
    aty = listArray (0, m - 1) [sum [pw x !! i * y | (x, y) <- ps] | i <- idx] :: Array Int Double
    -- L を遅延配列で作る（自分自身を参照してよい）
    l = listArray ((0, 0), (m - 1, m - 1)) [entry i j | i <- idx, j <- idx] :: Array (Int, Int) Double
    dg j = ata ! (j, j) - sum [l ! (j, k) * l ! (j, k) | k <- [0 .. j - 1]]
    entry i j
      | j > i = 0
      | i == j = sqrt (dg j)
      | otherwise = (ata ! (i, j) - sum [l ! (i, k) * l ! (j, k) | k <- [0 .. j - 1]]) / l ! (j, j)
    -- 前進代入 Lz = Aᵀy → 後退代入 Lᵀc = z
    z = listArray (0, m - 1)
          [(aty ! i - sum [l ! (i, k) * z ! k | k <- [0 .. i - 1]]) / l ! (i, i) | i <- idx] :: Array Int Double
    c = listArray (0, m - 1)
          [(z ! i - sum [l ! (k, i) * c ! k | k <- [i + 1 .. m - 1]]) / l ! (i, i) | i <- idx] :: Array Int Double
    coefL = elems c
    ataL = [[ata ! (i, j) | j <- idx] | i <- idx]
    lL = [[l ! (i, j) | j <- idx] | i <- idx]
    sse = sum [(y - evalP coefL x) ^ (2 :: Int) | (x, y) <- ps]

-- ホーナー法
evalP :: [Double] -> Double -> Double
evalP cs x = foldr (\c v -> v * x + c) 0 cs

-- 描画の枠
data R = R !Float !Float !Float !Float

plotRect :: Env -> R
plotRect env = R 30 110 (min (envW env * 0.58) (envH env - 150)) (envH env - 140)

sx, sy :: R -> Double -> Float
sx (R x _ w _) v = x + (realToFrac v + 1.1) / 2.2 * w
sy (R _ y _ h) v = y + h / 2 - (realToFrac v / 1.6) * (h / 2)

wx, wy :: R -> Float -> Double
wx (R x _ w _) px = realToFrac ((px - x) / w * 2.2 - 1.1)
wy (R _ y _ h) py = realToFrac (negate ((py - y - h / 2) / (h / 2)) * 1.6)

drawW :: Env -> World -> Picture
drawW env w =
  let r@(R rx ry rw rh) = plotRect env
      grid = [ Pictures [ seg env (rgb255 50 42 68) 1 (sx r v, ry) (sx r v, ry + rh)
                        , seg env (rgb255 50 42 68) 1 (rx, sy r v) (rx + rw, sy r v) ]
             | k <- [-4 .. 4 :: Int], let v = fromIntegral k / 4.4 ]
      fitPics = case wFit w of
        Nothing -> []
        Just f ->
          let cs = fCoef f
              resid (x, y) =
                let e = y - evalP cs x
                    px = sx r x
                    py = sy r y
                    qy = sy r (evalP cs x)
                    s = abs (qy - py)
                    x0 = if e > 0 then px else px - s
                in Pictures
                     [ if wSquare w then box env (rgba255 255 200 90 40) x0 (min py qy) s s else Blank
                     , seg env amber 2 (px, py) (px, qy) ]
              xs = [-1 + 2 * fromIntegral i / 200 | i <- [0 .. 200 :: Int]] :: [Double]
              vals = [(x, evalP cs x) | x <- xs]
              curve = [ seg env mint 4 (sx r x1, sy r v1) (sx r x2, sy r v2)
                      | ((x1, v1), (x2, v2)) <- zip vals (drop 1 vals), abs v1 < 1.6, abs v2 < 1.6 ]
          in map resid (wPts w) ++ curve
      dots = [ dot env (if i == wDragI w then white else coral) (sx r x) (sy r y) 16
             | (i, (x, y)) <- zip [0 ..] (wPts w) ]
      px = rx + rw + 30
      pw = envW env - px - 30
      panel = case wFit w of
        Nothing ->
          [ para env px 130 18 (rgb255 255 120 120) pw $
              if length (wPts w) < wDegree w + 1
                then "Degree " ++ show (wDegree w) ++ " needs at least " ++ show (wDegree w + 1)
                       ++ " points (now " ++ show (length (wPts w)) ++ ")"
                else "A^T A is not positive definite (points share the same x): no unique answer" ]
        Just f ->
          let eq = "y = " ++ unwords (zipWith term [0 :: Int ..] (fCoef f))
              term k c
                | k == 0 = fmt c
                | otherwise = (if c >= 0 then "+ " else "- ") ++ fmt (abs c) ++ "x" ++ (if k > 1 then "^" ++ show k else "")
              m = wDegree w + 1
              mf = fromIntegral m :: Float
              big = m >= 4   -- 次数が高いと縦に入らないので、2 つの行列を横に並べる
              cs1 = if big then min 48 ((pw - 16) / 2 / mf) else min 48 (pw / mf)
              y1 = 270
              l2x = if big then px + cs1 * mf + 16 else px
              l2y = if big then y1 else y1 + mf * cs1 + 45
              ltitle = if big then y1 - 25 else y1 + mf * cs1 + 20
          in [ txtT env px 120 18 (g8 235) "The closest polynomial"
             , para env px 150 16 mint pw eq
             , txtT env px 215 18 amber ("Sum of squared residuals = " ++ fixed4 (fSse f))
             , txtT env px 245 15 (g8 200) (if big then "A^T A (symmetric)" else "A^T A (symmetric: mirrored across the diagonal)")
             , drawMat env (fATA f) px y1 cs1 True
             , txtT env l2x ltitle 15 (g8 200) (if big then "L (A^T A = L L^T)" else "Cholesky factor L (A^T A = L L^T)")
             , drawMat env (fL f) l2x l2y cs1 False
             ]
  in Pictures
       [ txt env 30 50 26 (g8 235) "Least squares: the curve closest to the points"
       , txt env 30 80 15 (g8 150) ("Click: add  Drag: move  1-6: degree (now " ++ show (wDegree w)
                                     ++ ")  C: clear  N: random  S: squares " ++ (if wSquare w then "ON" else "OFF"))
       , box env plotC rx ry rw rh
       , Pictures grid
       , Pictures fitPics
       , Pictures dots
       , Pictures panel
       , footer env px pw 14 (g8 150)
           "Higher degree: smaller residuals, but the curve goes wild between points (overfitting).\nn points fit exactly with degree n-1 (interpolation, see 06).\nA^T A is symmetric positive definite: no pivoting and never a divide by 0."
       ]

drawMat :: Env -> [[Double]] -> Float -> Float -> Float -> Bool -> Picture
drawMat env mx x0 y0 cs heat =
  let top = foldl max 0 (map abs (concat mx))
      hiV = if top == 0 then 1 else top
      digits = if cs >= 44 then 1 else 0 -- マスが小さいときは小数を省く
      one (i, j, v) =
        let t = realToFrac (abs v / hiV) :: Float
            c | heat = mixC cellC coral t
              | j > i = plotC
              | otherwise = rgb255 30 90 80
            x = x0 + fromIntegral j * cs
            y = y0 + fromIntegral i * cs
        in Pictures
             [ box env c (x + 0.5) (y + 0.5) (cs - 1) (cs - 1)
             , if j > i && not heat then Blank
               else txtC env (x + cs / 2) (y + cs / 2) 14 (g8 235) (noMinusZero (showFFloat (Just digits) v "")) ]
  in Pictures (map one [(i, j, v) | (i, r) <- zip [0 :: Int ..] mx, (j, v) <- zip [0 :: Int ..] r])

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let r@(R rx ry rw rh) = plotRect env
        (mx, my) = mouseP env
        inside = mx >= rx && mx <= rx + rw && my >= ry && my <= ry + rh
        found = foldl (\b (i, (x, y)) -> if dist2 (mx, my) (sx r x, sy r y) < 14 then i else b)
                      (-1) (zip [0 ..] (wPts w))
    in pure $
         if not inside then w
         else if found >= 0 then refit w { wDragI = found }
         else refit w { wPts = wPts w ++ [(clampD (-1) 1 (wx r mx), wy r my)], wDragI = length (wPts w) }
  EventMotion _
    | wDragI w >= 0 ->
        let r = plotRect env
            (mx, my) = mouseP env
            p = (clampD (-1) 1 (wx r mx), clampD (-1.5) 1.5 (wy r my))
        in pure (refit w { wPts = [if i == wDragI w then p else q | (i, q) <- zip [0 ..] (wPts w)] })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragI = -1 }
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    c | c `elem` "123456" -> pure (refit w { wDegree = digitToInt c })
    'c' -> pure (refit w { wPts = [], wDragI = -1 })
    'n' -> randomPoints w { wDragI = -1 }
    's' -> pure w { wSquare = not (wSquare w) }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 点だけ大きく外す（外れ値）。直線はどれくらい引っぱられる？ 2 乗なので強く引っぱられる
-- 2. 点を 4 つにして次数 3 → 残差 0（全部の点を通る）
-- 3. x が全部同じ点を 2 つだけ置いて 1 次 → 正定値でなくなる（縦の直線は y = 式で書けない）

-- ---------- 道具 ----------
noMinusZero :: String -> String
noMinusZero s = if s == "-0" then "0" else s

fmt :: Double -> String
fmt v = if abs v < 0.005 then "0" else showFFloat (Just 2) v ""

fixed4 :: Double -> String
fixed4 v = showFFloat (Just 4) v ""

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- 2 つの色を混ぜる（p5.js の lerpColor）
mixC :: Color -> Color -> Float -> Color
mixC a b t =
  let (r1, g1, b1, _) = rgbaOfColor a
      (r2, g2, b2, _) = rgbaOfColor b
  in makeColor (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t) 1

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
