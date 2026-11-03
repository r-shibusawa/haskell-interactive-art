-- ============================================
-- 05 出生死滅過程と定常分布
-- 系内人数を「状態 0, 1, 2, …, K」として、到着で右へ（率 λ）、サービス終了で左へ（率 μ）1 つずつ動く
--   600 人の「分身」を同時に動かすと、どこから始めても人数の分布が定常分布 π_n ∝ (λ/μ)^n に落ち着く
--   落ち着いた状態では、となりどうしの流れがつり合う：π_n λ = π_(n+1) μ（つり合いの式）
-- ←→ = λ / ↑↓ = μ / R = 全員を状態 0 から / T = 全員を状態 K から / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.Vector.Unboxed as U
import Text.Printf (PrintfArg, printf)

kCap, nWalk :: Int
kCap = 8          -- ★ 定員（状態は 0〜K）。M/M/1/K 待ち行列
nWalk = 600       -- ★ 分身の数

speed :: Float
speed = 1.5       -- ★ 1 秒で進む時間

-- 流れの粒（from から dir の方向へ、f = 0〜1 が進み具合）
data Spark = Spark !Int !Int !Float

data World = World
  { wLambda, wMu :: !Float         -- ★ 到着率とサービス率
  , wPaused :: !Bool
  , wWalkers :: !(U.Vector Int)    -- 分身がいる状態
  , wOcc, wFluxR, wFluxL :: !(U.Vector Float)
  , wSparks :: ![Spark]
  , wT :: !Float
  }

main :: IO ()
main = runSketch (sketch "29-05 Birth-Death Process" (startAt 0 (World 0.8 1 False U.empty U.empty U.empty U.empty [] 0)))
  { skBack  = hsb 150 25 11
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \env w -> drawAll env w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

startAt :: Int -> World -> World
startAt s w = w { wWalkers = U.replicate nWalk s, wOcc = U.replicate (kCap + 1) 0
                , wFluxR = U.replicate kCap 0, wFluxL = U.replicate kCap 0, wT = 0, wSparks = [] }

nodeX :: Int -> Float
nodeX n = remap (fromIntegral n) 0 (fromIntegral kCap) 110 (width - 110)

-- ★ 小さな時間 dt の間に、確率 λdt で右へ、μdt で左へ
stepAll :: Float -> World -> IO World
stepAll dt w = do
  us <- U.replicateM nWalk (randomRIO (0, 1 :: Float))
  let old = wWalkers w
      move n u
        | n < kCap && u < wLambda w * dt = n + 1
        | n > 0 && u > 1 - wMu w * dt = n - 1
        | otherwise = n
      new = U.zipWith move old us
      tally sel = U.accumulate (+) (U.replicate kCap (0 :: Int)) (U.map (\i -> (i, 1)) sel)
      cR = tally (U.map fst (U.filter (\(a, b) -> b == a + 1) (U.zip old new)))
      cL = tally (U.map snd (U.filter (\(a, b) -> b == a - 1) (U.zip old new)))
      -- 流れ（1 人・単位時間あたり）をなめらかに記録
      smooth c f = U.zipWith (\k v -> v + (fromIntegral k / fromIntegral nWalk / dt - v) * 0.02) c f
  sparks <- foldM (addSparks cR cL) (wSparks w) [0 .. kCap - 1]
  pure w { wWalkers = new, wFluxR = smooth cR (wFluxR w), wFluxL = smooth cL (wFluxL w), wSparks = sparks
         , wOcc = U.accum (+) (wOcc w) [(U.head new, dt)]   -- 分身 1 号が各状態にいた時間
         , wT = wT w + dt }

addSparks :: U.Vector Int -> U.Vector Int -> [Spark] -> Int -> IO [Spark]
addSparks cR cL s0 j = do
  u1 <- randomRIO (0, 1 :: Float)
  u2 <- randomRIO (0, 1 :: Float)
  let s1 = if cR U.! j > 0 && length s0 < 300 && u1 < 0.3 then Spark j 1 0 : s0 else s0
      s2 = if cL U.! j > 0 && length s1 < 300 && u2 < 0.3 then Spark (j + 1) (-1) 0 : s1 else s1
  pure s2

stationary :: World -> [Float]
stationary w =
  let r = wLambda w / wMu w
      p = take (kCap + 1) (iterate (* r) 1)
      s = sum p
  in map (/ s) p

stepWorld :: World -> IO World
stepWorld w0 = do
  let sub = ceiling (speed / 60 / 0.02) :: Int
  w1 <- if wPaused w0 then pure w0 else foldM (\w _ -> stepAll (speed / 60 / fromIntegral sub) w) w0 [1 .. sub]
  let adv (Spark a d f) = Spark a d (f + 0.04)
      alive (Spark _ _ f) = f < 1
  pure w1 { wSparks = filter alive (map adv (wSparks w1)) }

drawAll :: Env -> World -> Picture
drawAll _ w = Pictures
  ( concatMap arcs [0 .. kCap - 1]
 ++ map drawSpark (wSparks w)
 ++ concatMap node [0 .. kCap]
 ++ [ hoop (nodeX (U.head (wWalkers w))) cy 72 4 (hsb 45 90 100)   -- 分身 1 号
    , txt (nodeX 0 + 30) (cy - 50) 15 (hsb 190 70 100) "lambda (arrival)"
    , txt (nodeX 0 + 30) (cy + 62) 15 (hsb 330 70 100) "mu (service end)"
    , seg 1 (hsba 0 0 100 0.3) (60, gy) (width - 60, gy) ]
 ++ concatMap bar (zip [0 .. kCap] pis)
 ++ [ txt 60 (gy - gh - 14) 14 (hsb 0 0 60) "Green = share of 600 clones now. White = stationary pi_n. Yellow = time share of clone 1. Small: flow to neighbor"
    , txt 20 38 16 (gray 1) ("lambda = " ++ nf 2 (wLambda w) ++ " (<- ->)  mu = " ++ nf 2 (wMu w) ++ " (Up/Down)  rho = " ++ nf 2 (wLambda w / wMu w)
        ++ "  K = " ++ show kCap ++ "  Mean " ++ nf 2 lBar ++ "  Full pi_K = " ++ nf 3 (last' pis))
    , txt 20 66 15 (hsb 0 0 60) ("Time " ++ nf 1 (wT w) ++ ".  R / T: start at 0 / K.  Space: pause.  With capacity K it settles even if rho > 1; pi_K = chance of refusal") ] )
  where
    cy = 190
    gy = height - 70
    gh = height - 420
    bw = (nodeX 1 - nodeX 0) * 0.6
    pis = stationary w
    lBar = sum (zipWith (\n p -> fromIntegral n * p) [0 :: Int ..] pis)
    last' = foldl (\_ x -> x) 0
    cnt = U.accumulate (+) (U.replicate (kCap + 1) (0 :: Int)) (U.map (\i -> (i, 1)) (wWalkers w))
    share n = fromIntegral (cnt U.! n) / fromIntegral nWalk :: Float
    -- 状態の図と矢印（半円の弧）
    arcs n =
      let a = nodeX n + 34
          b = nodeX (n + 1) - 34
          mx = (a + b) / 2
          half = (b - a) / 2
          arcPts cy1 t0 = [ (mx + half * cos t, cy1 + 30 * sin t) | i <- [0 .. 16 :: Int], let t = t0 + pi * fromIntegral i / 16 ]
      in [ path 3 (hsba 190 70 100 0.7) (arcPts (cy - 6) pi), path 3 (hsba 330 70 100 0.7) (arcPts (cy + 6) 0) ]
    drawSpark (Spark from d f) =   -- 流れの粒
      let x = lerpF (nodeX from) (nodeX (from + d)) f
          y = if d > 0 then cy - 6 - sin (f * pi) * 30 else cy + 6 + sin (f * pi) * 30
      in dot x y 6 (if d > 0 then hsb 190 60 100 else hsb 330 60 100)
    node n =
      [ dot (nodeX n) cy 60 (hsba 0 0 100 ((10 + 60 * share n * 3) / 100))
      , hoop (nodeX n) cy 60 2 (hsba 0 0 100 0.6)
      , txtC (nodeX n) (cy + 8) 22 (gray 1) (show n) ]
    -- 棒グラフ：分身の分布（色）・定常分布（白枠）・1 号の滞在時間の割合（黄の点）
    bar (n, p) =
      let x = nodeX n
          h1 = min (share n * gh * 1.6) gh
          h2 = min (p * gh * 1.6) gh
          o = min ((wOcc w U.! n) / max (wT w) 1e-9 * gh * 1.6) gh
          frameC = gray 1
          lx = x - bw / 2
          rx = x + bw / 2
          ty = gy - h2
      in [ box lx (gy - h1) bw h1 (hsba 160 60 85 0.8)
         , path 2 frameC [(lx, gy), (lx, ty), (rx, ty), (rx, gy), (lx, gy)]
         , dot x (gy - o) 10 (hsb 45 90 100)
         , txtC x (gy + 22) 14 (hsb 0 0 80) (nf 3 p) ]
         ++ (if n < kCap
              then [ txtC ((x + nodeX (n + 1)) / 2) (gy - gh + 12) 14 (hsb 0 0 60) ("->" ++ nf 3 (wFluxR w U.! n))
                   , txtC ((x + nodeX (n + 1)) / 2) (gy - gh + 30) 14 (hsb 0 0 60) ("<-" ++ nf 3 (wFluxL w U.! n)) ]
              else [])

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLambda = min 3 (wLambda w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLambda = max 0.1 (wLambda w - 0.1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wMu = min 3 (wMu w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wMu = max 0.1 (wMu w - 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> startAt 0 w
    | c `elem` "tT" -> startAt kCap w
  _ -> w

-- ---------- やってみよう ----------
-- 1. R と T で始め方を変えても、最後は同じ分布になることを確かめる。何秒くらいで落ち着く？
-- 2. λ = μ のとき定常分布はどんな形？ λ = 2μ のときは？（π_n ∝ ρ^n）
-- 3. サービス終了の率を μ × min(n, 2) に変えると窓口 2 つ（M/M/2/K）になる。stepAll の move を書き換える
-- 4. 到着の率を λ × (K − n) にすると「人数が限られた客」（機械の修理など）。分布はどう変わる？

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

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

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

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v
