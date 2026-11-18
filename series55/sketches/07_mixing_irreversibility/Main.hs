-- ============================================
-- 55-07 混合と不可逆性 ─ 仕切りをとると 2 種類の分子は混ざり、もう元にはもどらない
-- 分子はやわらかい円板（ばね状の反発）で、運動方程式を「速度ベルレ法」で解く。この解き方は時間を逆にしても同じ式
-- → 速度を全部反対にすると（V）、本当に元の分かれた状態にもどる。でも 1 粒子を 100 万分の 1 だけずらすと（P）もどらない
-- 全部の赤が左にもどる確率は (1/2)^N。混合エントロピー ΔS = 2N k ln 2
-- 操作：スペース 仕切りをとる / V 時間を逆回し / P 少しずらして逆回し / 1 2 3 粒子の数 / ↑↓ 速さ / R 最初から
-- ============================================
{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main (main) where

import Art
import Control.Monad (forM_, replicateM_, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

sigma, eps, epsWall, dt :: Double
sigma = 10                 -- ★ 粒子の直径 [px]
eps = 30                   -- ★ 粒子どうしの反発の強さ（kT = 1 の何倍か）
epsWall = 80               -- ★ 壁・仕切りの反発の強さ
dt = 0.05                  -- ★ 1 ステップの時間

sizes :: [Int]
sizes = [4, 20, 150]       -- ★ 1 〜 3 で選ぶ、片側の粒子の数

-- やわらかい円板の気体（左半分が赤、右半分が青）
data Sim = Sim
  { sN :: !Int, sW, sH :: !Double, sWall :: !Bool
  , sX, sY, sVX, sVY, sAX, sAY :: !(U.Vector Double)
  , sSteps :: !Int }

data World = World
  { wSim     :: !Sim
  , wSizeIdx :: !Int
  , wSpf     :: !Int               -- 1 コマのステップ数
  , wLogL    :: [Int]              -- 左にいる赤の数（新しい順、400 個まで）
  , wCount   :: !(U.Vector Int)    -- その回数（仕切りをとってから）
  , wMsg     :: String
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-07 Mixing and irreversibility" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetSim env world0 { wTest = selfTest })
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeMix 1 100 100 (seedRng 5))) 2 20 [] (U.replicate 2 0) "" []

resetSim :: Env -> World -> World
resetSim env w =
  let n = getAt 150 sizes (wSizeIdx w)
      (s, _) = makeMix n (fromIntegral (floor (envW env * 0.48) :: Int)) (realToFrac (envH env) - 220) (seedRng 5)
  in w { wSim = s, wLogL = [], wCount = U.replicate (n + 1) 0, wMsg = "" }

-- 左に赤 n 個、右に青 n 個
makeMix :: Int -> Double -> Double -> Rng -> (Sim, Rng)
makeMix n w h r0 = (forces (Sim (2 * n) w h True (col pa) (col pb) (col pc) (col pd) z z 0), rEnd)
  where
    cols = max 1 (ceiling (sqrt ((fromIntegral n * w) / 2 / h)) :: Int)
    rows = ceiling (fromIntegral n / fromIntegral cols :: Double) :: Int
    one (acc, rg) i =
      let k = i `mod` n
          side = if i < n then 0 else 1
          x = side * (w / 2) + (fromIntegral (k `mod` cols) + 0.5) * (w / 2 / fromIntegral cols)
          y = (fromIntegral (k `div` cols) + 0.5) * (h / fromIntegral rows)
          (vx, rg1) = rndNormal rg
          (vy, rg2) = rndNormal rg1
      in ((x, y, vx, vy) : acc, rg2)
    (ps, rEnd) = foldl' one ([], r0) [0 .. 2 * n - 1]
    qs = reverse ps
    pa (a, _, _, _) = a
    pb (_, b, _, _) = b
    pc (_, _, c, _) = c
    pd (_, _, _, d) = d
    col f = U.fromList (map f qs)
    z = U.replicate (2 * n) 0

-- 力：粒子どうしは U = ε(1 − r/σ)²（r < σ のときだけ）、壁と仕切りも同じ形
wallF :: Double -> Double -> Double
wallF d e = let rr = sigma / 2 in if d < rr then ((2 * e) / rr) * (1 - max d 0 / rr) else 0

-- 動かす間だけ書きかえられる配列
data MS s = MS { mX, mY, mVX, mVY, mAX, mAY :: !(UM.MVector s Double) }

withSim :: Sim -> (forall s. MS s -> ST s ()) -> Sim
withSim s act = runST $ do
  m <- MS <$> U.thaw (sX s) <*> U.thaw (sY s) <*> U.thaw (sVX s) <*> U.thaw (sVY s) <*> U.thaw (sAX s) <*> U.thaw (sAY s)
  act m
  x <- U.freeze (mX m); y <- U.freeze (mY m); vx <- U.freeze (mVX m); vy <- U.freeze (mVY m)
  ax <- U.freeze (mAX m); ay <- U.freeze (mAY m)
  pure s { sX = x, sY = y, sVX = vx, sVY = vy, sAX = ax, sAY = ay }

rd :: UM.MVector s Double -> Int -> ST s Double
rd = UM.unsafeRead

wr :: UM.MVector s Double -> Int -> Double -> ST s ()
wr = UM.unsafeWrite

forcesM :: Sim -> MS s -> ST s ()
forcesM s m = do
  let n = sN s
      cs = sigma
      nx = max 1 (floorI (sW s / cs))
      ny = max 1 (floorI (sH s / cs))
  UM.set (mAX m) 0
  UM.set (mAY m) 0
  hd <- UM.replicate (nx * ny) (-1 :: Int)
  nxt <- UM.replicate (max 1 n) (-1 :: Int)
  forM_ [0 .. n - 1] $ \i -> do
    x <- rd (mX m) i; y <- rd (mY m) i
    let c = cellOf (x / cs) nx + nx * cellOf (y / cs) ny
    h <- UM.unsafeRead hd c
    UM.unsafeWrite nxt i h
    UM.unsafeWrite hd c i
  let walk !j f = if j >= 0 then f j >> UM.unsafeRead nxt j >>= \j' -> walk j' f else pure ()
      pair i j = unless (j <= i) $ do
        xi <- rd (mX m) i; yi <- rd (mY m) i; xj <- rd (mX m) j; yj <- rd (mY m) j
        let dx = xj - xi; dy = yj - yi; r2 = dx * dx + dy * dy
        unless (r2 >= sigma * sigma || r2 == 0) $ do
          let r = sqrt r2
              f = ((2 * eps) / sigma) * (1 - r / sigma)
          a1 <- rd (mAX m) i; wr (mAX m) i (a1 - (f * dx) / r)
          a2 <- rd (mAY m) i; wr (mAY m) i (a2 - (f * dy) / r)
          a3 <- rd (mAX m) j; wr (mAX m) j (a3 + (f * dx) / r)
          a4 <- rd (mAY m) j; wr (mAY m) j (a4 + (f * dy) / r)
  forM_ [0 .. ny - 1] $ \cy -> forM_ [0 .. nx - 1] $ \cx -> do
    h0 <- UM.unsafeRead hd (cx + nx * cy)
    walk h0 $ \i -> forM_ [-1, 0, 1] $ \oy -> forM_ [-1, 0, 1] $ \ox -> do
      let ax = cx + ox; ay = cy + oy
      unless (ax < 0 || ay < 0 || ax >= nx || ay >= ny) $ do
        hj <- UM.unsafeRead hd (ax + nx * ay)
        walk hj (pair i)
  forM_ [0 .. n - 1] $ \i -> do
    x <- rd (mX m) i; y <- rd (mY m) i
    a1 <- rd (mAX m) i
    let a1' = a1 + (wallF x epsWall - wallF (sW s - x) epsWall)
    a2 <- rd (mAY m) i
    wr (mAY m) i (a2 + (wallF y epsWall - wallF (sH s - y) epsWall))
    -- 仕切り
    wr (mAX m) i (if sWall s then a1' + (if 2 * i < n then -(wallF (sW s / 2 - x) epsWall) else wallF (x - sW s / 2) epsWall) else a1')

-- 速度ベルレ法（時間を逆にしても同じ手順）
verletM :: Sim -> MS s -> ST s ()
verletM s m = do
  forM_ [0 .. sN s - 1] $ \i -> do
    ax <- rd (mAX m) i; ay <- rd (mAY m) i
    vx <- rd (mVX m) i; vy <- rd (mVY m) i
    let vx' = vx + 0.5 * dt * ax; vy' = vy + 0.5 * dt * ay
    wr (mVX m) i vx'; wr (mVY m) i vy'
    x <- rd (mX m) i; y <- rd (mY m) i
    wr (mX m) i (x + dt * vx'); wr (mY m) i (y + dt * vy')
  forcesM s m
  forM_ [0 .. sN s - 1] $ \i -> do
    ax <- rd (mAX m) i; ay <- rd (mAY m) i
    vx <- rd (mVX m) i; vy <- rd (mVY m) i
    wr (mVX m) i (vx + 0.5 * dt * ax); wr (mVY m) i (vy + 0.5 * dt * ay)

forces :: Sim -> Sim
forces s = withSim s (forcesM s)

-- k ステップ進める
verlets :: Int -> Sim -> Sim
verlets k s = (withSim s (\m -> replicateM_ k (verletM s m))) { sSteps = sSteps s + k }

energy :: Sim -> Double
energy s = foldl' one 0 [0 .. n - 1]
  where
    n = sN s
    x = sX s; y = sY s
    one e0 i =
      let xi = x U.! i; yi = y U.! i
          e1 = e0 + 0.5 * (sVX s U.! i ^ (2 :: Int) + sVY s U.! i ^ (2 :: Int))
          ds = [xi, sW s - xi, yi, sH s - yi] ++ (if sWall s then [abs (xi - sW s / 2)] else [])
          e2 = foldl' (\e d -> if d < sigma / 2 then e + epsWall * (1 - d / (sigma / 2)) ^ (2 :: Int) else e) e1 ds
      in foldl' (\e j -> let r = hypot (x U.! j - xi) (y U.! j - yi) in if r < sigma then e + eps * (1 - r / sigma) ^ (2 :: Int) else e) e2 [i + 1 .. n - 1]

reverseV :: Sim -> Sim
reverseV s = s { sVX = U.map negate (sVX s), sVY = U.map negate (sVY s) }

redLeft :: Sim -> Int
redLeft s = length [ () | i <- [0 .. sN s `div` 2 - 1], sX s U.! i < sW s / 2 ]

lnFact :: Int -> Double
lnFact n = foldl' (\s k -> s + log (fromIntegral k)) 0 [2 .. n]

lnChoose :: Int -> Int -> Double
lnChoose n k = lnFact n - lnFact k - lnFact (n - k)

step :: World -> World
step w = w { wSim = s, wLogL = take 400 (nl : wLogL w), wCount = if sWall s then wCount w else U.accum (+) (wCount w) [(nl, 1)] }
  where s = verlets (wSpf w) (wSim w)
        nl = redLeft s

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱
  [ Color (grayC 150) (frameTL 2 bx by (f2 (sW s)) (f2 (sH s)))
  , if sWall s then Color (grayC 230) (thickLine 4 (midX, by) (midX, by + f2 (sH s)))
    else Color (rgbaC 255 255 255 40) (Line [(midX, by), (midX, by + f2 (sH s))]) ]
  ++ [ Color (if i < nh then rgbC 255 100 70 else rgbC 80 160 255) (at (bx + f2 (sX s U.! i), by + f2 (sY s U.! i)) (disc (f2 sigma * 0.45)))
     | i <- [0 .. sN s - 1] ]
  -- ---- グラフ 1：左にいる赤の数
  ++ [ plotAxes g1 "time ->" "red molecules on the left" [] [0, fromIntegral nh / 2, fromIntegral nh]
     , Color (rgbC 255 120 80) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 (fromIntegral v)) | (k, v) <- zip [0 :: Int ..] (reverse (wLogL w)) ])
     , Color (rgbaC 255 255 255 60) (Line [(pX g1, gY g1 (fromIntegral nh / 2)), (pX g1 + pW g1, gY g1 (fromIntegral nh / 2))]) ]
  -- ---- グラフ 2：その分布（理論 = 二項分布 C(N, k)/2^N）
  ++ [ plotAxes g2 "red on the left, k" "fraction" [0, fromIntegral nh / 2, fromIntegral nh] [0, roundSig 2 (pmax * 1.4)] ]
  ++ [ Color (rgbaC 255 150 80 160) (rectTL (gX g2 (fromIntegral k - 0.4)) y (max 1 (gX g2 (fromIntegral k + 0.4) - gX g2 (fromIntegral k - 0.4))) (pY g2 + pH g2 - y))
     | tot > 0, (k, c) <- zip [0 :: Int ..] (U.toList (wCount w)), let y = gY g2 (fromIntegral c / fromIntegral tot) ]
  ++ [ Color (rgbC 130 200 255) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 (exp (lnChoose nh k - fromIntegral nh * log 2))) | k <- [0 .. nh] ]) ]
  -- ---- 文字
  ++ [ say ("Mixing and irreversibility   N = " ++ show nh ++ " per side   " ++ (if sWall s then "wall in (Space removes it)" else "no wall")
            ++ "   step " ++ show (sSteps s) ++ "   energy " ++ fmt 2 (energy s)) 20 36 18 (grayC 240)
     , say "Space open wall   V reverse time   P nudge 1e-6 and reverse   1 2 3 N = 4/20/150   Up/Down speed   R restart" 20 62 15 (grayC 175)
     , say ("All red back left: (1/2)^N = " ++ expo 2 (0.5 ^^ nh) ++ "   mixing dS = 2N ln 2 = " ++ fmt 1 (2 * fromIntegral nh * log 2)
            ++ " k   now ln W = " ++ fmt 1 sCount ++ " (max " ++ fmt 1 sMax ++ ")") 20 86 15 (rgbC 255 200 120)
     , say (wMsg w) bx (by + f2 (sH s) + 28) 15 (rgbC 255 200 120)
     , say "Blue = binomial C(N,k)/2^N (molecules put left/right at random)" (pX g2) (pY g2 + pH g2 + 52) 14 (rgbC 130 200 255) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    s = wSim w
    nh = sN s `div` 2
    nl = redLeft s
    bl = length [ () | i <- [nh .. sN s - 1], sX s U.! i < sW s / 2 ]
    bx = 20
    by = 120
    midX = bx + f2 (sW s / 2)
    gx = bx + f2 (sW s) + 70
    gw = envW env - gx - 30
    g1 = graph gx by gw ((f2 (sH s) - 80) / 2) 0 400 0 (fromIntegral nh)
    tot = U.sum (wCount w)
    pmax = exp (lnChoose nh (nh `div` 2) - fromIntegral nh * log 2)
    g2 = graph gx (pY g1 + pH g1 + 80) gw (pH g1) (-0.5) (fromIntegral nh + 0.5) 0 (pmax * 1.4)
    sCount = lnChoose nh nl + lnChoose nh bl
    sMax = 2 * lnChoose nh (nh `div` 2)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _
    | sWall s -> w { wSim = forces s { sWall = False }, wMsg = "", wCount = U.map (const 0) (wCount w) }
  EventKey (Char c) Down _ _
    | c `elem` "vV" -> w { wSim = forces (reverseV s), wMsg = "Running backwards: reversed at step " ++ show (sSteps s) }
    | c `elem` "pP" -> w { wSim = forces (reverseV s { sX = sX s U.// [(0, sX s U.! 0 + 1e-6)] })
                         , wMsg = "Nudged by 1e-6 and reversed at step " ++ show (sSteps s) }
    | c `elem` "123" -> resetSim env w { wSizeIdx = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> resetSim env w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpf = min 160 (wSpf w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpf = max 5 (wSpf w `div` 2) }
  _ -> w
  where s = wSim w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("velocity Verlet: energy change after 1500 steps < 0.5%", abs (energy s1 / e0 - 1) < 5e-3)
  , ("2800 steps, reverse the velocities -> back to the start in 2800 steps (error " ++ expo 1 err ++ " < 1e-3 px)", err < 1e-3)
  , ("nudge 1 particle by 1e-6 px and reverse -> error grows to " ++ fmt 1 errP ++ " px (no return)", errP > 1)
  , ("binomial sums to 1, N = 20 all left: 2^-20 = 9.54e-7"
    , near (sum [ exp (lnChoose 20 k) | k <- [0 .. 20] ] / 2 ^^ (20 :: Int)) 1 1e-9 && near (exp (lnChoose 20 20 - 20 * log 2)) 9.5367e-7 1e-10)
  , ("ways: ln C(2N, N) ~ 2N ln 2 - (1/2) ln(pi N) (N = 150, Stirling)", near (lnChoose 300 150) (300 * log 2 - 0.5 * log (pi * 150)) 0.01)
  , ("after the wall is removed, mean share of red on the left ~ 1/2 (" ++ fmt 2 (sm / fromIntegral cnt) ++ ")", abs (sm / fromIntegral cnt - 0.5) < 0.12) ]
  where
    (sa, r1) = makeMix 30 200 160 (seedRng 21)
    s0 = forces sa { sWall = False }
    e0 = energy s0
    s1 = verlets 1500 s0
    s2 = verlets 2800 s1
    (ta, r2) = makeMix 30 200 160 r1
    t0 = forces ta { sWall = False, sX = sX s2, sY = sY s2, sVX = sVX s2, sVY = sVY s2 }
    s3 = verlets 2800 (forces (reverseV s2))
    maxErr a = U.maximum (U.zipWith4 (\x y x0 y0 -> hypot (x - x0) (y - y0)) (sX a) (sY a) (sX s1) (sY s1))
    err = maxErr s3
    t1 = verlets 2800 (forces (reverseV t0 { sX = sX t0 U.// [(0, sX t0 U.! 0 + 1e-6)] }))
    errP = maxErr t1
    (ma, _) = makeMix 20 160 160 r2
    m0 = forces ma { sWall = False }
    run (!mm, !sm', !c) k = let mm' = verlets 1 mm
                            in if k > 2000 && k `mod` 20 == 0 then (mm', sm' + fromIntegral (redLeft mm') / 20, c + 1) else (mm', sm', c)
    (_, sm, cnt) = foldl' run (m0, 0 :: Double, 0 :: Int) [0 .. 5999 :: Int]

-- ---------- やってみよう ----------
-- 1. スペースで混ぜてから、すぐ V を押そう。本当に分かれた状態にもどる？ 長く待ってから V では？（計算の丸め誤差も広がる）
-- 2. P（少しずらして逆回し）では？ 10⁻⁶ のずれが、衝突のたびに何倍にもなる（カオス）
-- 3. 1 で粒子を 4 個ずつにすると、全部の赤が左にもどることがある。何回に 1 回くらい？ (1/2)^4 = 1/16
-- 4. 150 個ずつでは 2⁻¹⁵⁰ ≈ 7 × 10⁻⁴⁶。宇宙の年齢の間に一度も起きないくらい小さい
-- 5. 下の分布は二項分布に近い？ 粒子の数を増やすと、山は細くなる？

-- マスの番号（0〜n−1 におさめる）
cellOf :: Double -> Int -> Int
cellOf q n | not (q >= 0) = 0
           | q >= fromIntegral (n - 1) = n - 1
           | otherwise = floor q

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を n 個まとめて
rndList :: Int -> Rng -> ([Double], Rng)
rndList n r0 = go n r0 []
  where go 0 r acc = (reverse acc, r)
        go k r acc = let (v, r') = rnd r in go (k - 1) r' (v : acc)

-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

-- 指数表記（toExponential）
expo :: Int -> Double -> String
expo n v = let s = showEFloat (Just n) (fin 0 v) ""
               (a, b) = break (== 'e') s
           in case b of
                ('e' : '-' : _) -> s
                ('e' : rest) -> a ++ "e+" ++ rest
                _ -> s

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
