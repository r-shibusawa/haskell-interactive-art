-- ============================================
-- 01 ポアソン到着と指数分布
-- 「いつ来るかまったく予想できない」到着を、時間の上の点としてならべる
--   到着と到着の間隔 → 指数分布 λe^(−λx)　　1 秒ごとに数えた人数 → ポアソン分布 e^(−λ) λ^k / k!
-- ←→ = 到着率 λ / ↑↓ = 時間の速さ / スペース = 一時停止 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Array.Unboxed (UArray, accumArray, (!))
import Data.Foldable (foldl', toList)
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

showSpan :: Double
showSpan = 12     -- ★ 上の帯に表示する時間の長さ

data World = World
  { wLambda :: !Double           -- ★ 単位時間あたりの平均到着数
  , wSpeed :: !Float             -- ★ 1 秒で進む時間
  , wPaused :: !Bool
  , wT, wNext, wLast :: !Double
  , wArrivals :: !(Seq Double)   -- 最近の到着時刻
  , wGaps :: !(Seq Float)        -- 到着の間隔（すべて）
  , wCounts :: !(Seq Int)        -- 1 単位時間ごとの到着数（すべて）
  , wCurCount, wCurBin :: !Int
  }

main :: IO ()
main = runSketch (sketch "29-01 Poisson Arrivals" world0)
  { skBack  = hsb 250 25 12
  , skStart = \_ -> resetAll world0
  , skStep  = \_ _ w -> if wPaused w then pure w else advance (wT w + realToFrac (wSpeed w) / 60) w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 3 2 False 0 0 0 Seq.empty Seq.empty Seq.empty 0 0

resetAll :: World -> IO World
resetAll w = do
  n <- expRand (wLambda w)
  pure w { wT = 0, wLast = 0, wNext = n, wArrivals = Seq.empty, wGaps = Seq.empty
         , wCounts = Seq.empty, wCurCount = 0, wCurBin = 0 }

-- 区切り（整数時刻）をまたいだら、その区間の人数を記録
closeBins :: Double -> World -> World
closeBins t w
  | floor t > wCurBin w = closeBins t w { wCounts = wCounts w |> wCurCount w, wCurCount = 0, wCurBin = wCurBin w + 1 }
  | otherwise = w

advance :: Double -> World -> IO World
advance tEnd w0
  | wNext w0 <= tEnd = do
      let a = wNext w0
          w = closeBins a w0
      gap <- expRand (wLambda w)       -- ★ 次の到着までの時間は指数分布
      advance tEnd w { wArrivals = wArrivals w |> a, wGaps = wGaps w |> fl (a - wLast w)
                     , wCurCount = wCurCount w + 1, wLast = a, wNext = a + gap }
  | otherwise = do
      let w = closeBins tEnd w0
          recent = Seq.dropWhileL (< tEnd - showSpan) (wArrivals w)
          trim n s = if Seq.length s > n then Seq.drop (Seq.length s - n) s else s
      pure w { wT = tEnd, wArrivals = recent, wGaps = trim 20000 (wGaps w), wCounts = trim 5000 (wCounts w) }

drawAll :: World -> Picture
drawAll w = Pictures [drawBand w, drawGaps w, drawCounts w, hud]
  where
    hud = Pictures
      [ txt 20 40 18 (gray 1) ("lambda = " ++ nf 1 (wLambda w) ++ " (<- ->)  Time " ++ nf 0 (wT w) ++ "  Arrivals " ++ show (Seq.length (wGaps w))
          ++ "  Bins " ++ show (Seq.length (wCounts w)) ++ "  Speed x" ++ num (wSpeed w) ++ " (Up/Down)  Space  R")
      , txt 20 68 15 (hsb 0 0 60) "Short gaps are the most common. More data, closer to theory. Random, yet mean and variance both obey lambda." ]

-- 時間の帯：到着 = 縦線、区切り = 単位時間
drawBand :: World -> Picture
drawBand w = Pictures
  ( box x0 by (x1 - x0) bh (hsba 0 0 100 0.06)
  : concatMap tick [ceiling (t - showSpan) .. floor t]
 ++ [ seg 3 (hsb 190 70 100) (xOf a, by + 10) (xOf a, by + bh - 10) | a <- toList (wArrivals w) ]
 ++ [ txt x0 (by - 8) 14 (hsb 0 0 60) "<- past    Arrivals on the time line (bars). Yellow = arrivals per unit of time    now ->" ] )
  where
    t = wT w
    x0 = 40
    x1 = width - 40
    by = 110
    bh = 70
    xOf :: Double -> Float
    xOf a = remap (fl (a - t)) (fl (negate showSpan)) 0 x0 x1
    tick :: Int -> [Picture]
    tick k =
      let kd = fromIntegral k
          n = length (filter (\a -> a >= kd && a < kd + 1) (toList (wArrivals w)))
      in seg 1 (hsba 0 0 100 0.25) (xOf kd, by) (xOf kd, by + bh)
         : [ txtC (xOf (kd + 0.5)) (by + bh + 20) 14 (hsb 45 80 100) (show n) | k >= 0, kd + 1 <= t ]

gy, gh :: Float
gy = height - 60
gh = height - 340

-- 左：間隔のヒストグラム vs 指数分布
drawGaps :: World -> Picture
drawGaps w = Pictures
  ( axes gx gy gw gh "Gap x" "0" (nf 1 xmax)
  : [ let dens = fromIntegral (bins ! i) / total / bw
          h = min (dens / ymax) 1 * gh
      in box (gx + fromIntegral i * gw / fromIntegral nb + 1) (gy - h) (gw / fromIntegral nb - 2) h (hsba 190 60 90 0.7)
    | i <- [0 .. nb - 1] ]
 ++ [ path 3 (hsb 45 90 100)
        [ (gx + gw * fromIntegral i / 80, gy - lam * exp (negate lam * (xmax * fromIntegral i / 80)) / ymax * gh) | i <- [0 .. 80 :: Int] ]
    , txt gx (gy - gh - 34) 16 (gray 1) "Histogram of gaps (blue) and lambda e^(-lambda x) (yellow)"
    , txt gx (gy - gh - 12) 16 (hsb 45 90 100) ("Mean gap " ++ nf 3 mg ++ " (theory 1/lambda = " ++ nf 3 (1 / lam) ++ ")") ] )
  where
    lam = fl (wLambda w)
    gx = 60
    gw = width * 0.42
    xmax = 5 / lam
    nb = 30 :: Int
    bw = xmax / fromIntegral nb
    gaps = toList (wGaps w)
    bins :: UArray Int Int
    bins = accumArray (+) 0 (0, nb - 1) [ (b, 1) | g <- gaps, let b = floor (g / bw), b < nb ]
    total = fromIntegral (max (Seq.length (wGaps w)) 1) :: Float
    ymax = lam * 1.15
    mg = foldl' (+) 0 (wGaps w) / total

-- 右：1 単位時間の人数 vs ポアソン分布
drawCounts :: World -> Picture
drawCounts w = Pictures
  ( axes hx gy hw gh "Arrivals per unit k" "0" (show kmax)
  : concat [ [ let h = min ((fromIntegral (freq ! k) / total) / pmax) 1 * gh
               in box (hx + fromIntegral k * cw + 2) (gy - h) (cw - 4) h (hsba 320 50 90 0.7)
             , dot (hx + fromIntegral k * cw + cw / 2) (gy - p / pmax * gh) 10 (hsb 45 90 100) ]
           | (k, p) <- zip [0 .. kmax] pm ]
 ++ [ txt hx (gy - gh - 34) 16 (gray 1) "Counts (pink) and Poisson distribution (yellow)"
    , txt hx (gy - gh - 12) 16 (hsb 45 90 100) ("Mean " ++ nf 2 m ++ "  Variance " ++ nf 2 v ++ " (theory: both " ++ nf 1 lam ++ ")") ] )
  where
    lam = fl (wLambda w)
    hx = width * 0.55
    hw = width * 0.4
    kmax = max 10 (ceiling (lam + 4 * sqrt lam)) :: Int
    counts = toList (wCounts w)
    freq :: UArray Int Int
    freq = accumArray (+) 0 (0, kmax) [ (c, 1) | c <- counts, c <= kmax ]
    pm = poissonPmf lam kmax
    pmax = maximum (0 : pm) * 1.3
    cw = hw / fromIntegral (kmax + 1)
    n = Seq.length (wCounts w)
    total = fromIntegral (max n 1) :: Float
    m = fromIntegral (sum counts) / total
    v = sum [ (fromIntegral c - m) * (fromIntegral c - m) | c <- counts ] / fromIntegral (max (n - 1) 1)

axes :: Float -> Float -> Float -> Float -> String -> String -> String -> Picture
axes x y w h lab a b = Pictures
  [ seg 1 c (x, y) (x + w, y), seg 1 c (x, y) (x, y - h)
  , txt (x - 4) (y + 18) 14 (hsb 0 0 60) a
  , txtR (x + w) (y + 18) 14 (hsb 0 0 60) b
  , txtC (x + w / 2) (y + 32) 14 (hsb 0 0 60) lab ]
  where c = hsba 0 0 100 0.4

-- ポアソン分布 P(k) = e^(−λ) λ^k / k!（k = 0..kmax）
poissonPmf :: Float -> Int -> [Float]
poissonPmf lam kmax = take (kmax + 1) (scanl (\p k -> p * lam / fromIntegral k) (exp (negate lam)) [1 :: Int ..])

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetAll w { wLambda = min 8 (wLambda w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetAll w { wLambda = max 0.5 (wLambda w - 0.5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 64 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0.25 (wSpeed w / 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> resetAll w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ↑ で速くして 1000 回分集める。ヒストグラムは曲線にどれくらい近づく？
-- 2. λ = 0.5 と 8 で人数の分布の形を比べる。λ が大きいと何の形に近づく？（中心極限定理）
-- 3. advance の expRand (wLambda w) を pure (1 / wLambda w)（等間隔）に変えると、分散はどうなる？「ランダム」とは何かを考える
-- 4. expRand を 2 回足した間隔（λ を 2 倍に）にすると、間隔の分布は指数分布でなくなる（アーラン分布）。形を見る

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- 右ぞろえの文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- 速さの表示：整数なら小数点なし
num :: Float -> String
num v = if v == fromIntegral (round v :: Int) then show (round v :: Int) else show v

-- シミュレーションの時刻は Double、描くときは Float
fl :: Double -> Float
fl = realToFrac

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)
