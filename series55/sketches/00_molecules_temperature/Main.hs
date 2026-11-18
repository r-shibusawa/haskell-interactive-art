-- ============================================
-- 55-00 温度は分子の運動 ─ 箱の中の剛体円板と、マクスウェル-ボルツマン分布
-- はじめは全部の粒子が「同じ速さ・ばらばらの向き」。ぶつかり合ううちに速さがばらつき、
-- 速さのヒストグラムが 2 次元のマクスウェル-ボルツマン分布 f(v) = (mv/kT)·e^(−mv²/2kT) に近づく
-- 温度 = 1 粒子あたりの平均運動エネルギー（2 次元では ⟨½mv²⟩ = kT。換算単位 k = m = 1）
-- 操作：↑ 加熱　↓ 冷却 / スペース 一時停止 / C 色（速さ ⇔ 1 色）/ クリック 近くの粒子を追いかける / R やり直し
-- ============================================
{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main (main) where

import Art
import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPart :: Int
nPart = 320                -- ★ 粒子の数

rad, v0, timePerFrame :: Double
rad = 4                    -- ★ 粒子の半径 [px]
v0 = 1.5                   -- ★ はじめの速さ（全員同じ）
timePerFrame = 2           -- ★ 1 コマで進める時間

bins :: Int
bins = 32                  -- ★ ヒストグラムの区間の数

data World = World
  { wGas     :: !Gas
  , wHist    :: !(U.Vector Double)   -- 少しずつ平均したヒストグラム
  , wPaused  :: !Bool
  , wColor   :: !Bool                -- 速さで色をつけるか
  , wTracked :: !Int                 -- 追いかけている粒子（−1 = なし）
  , wDev     :: [Double]             -- 理論とのずれ（新しい順、200 個まで）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-00 Temperature is molecular motion" world0)
  { skBack  = rgbC 24 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetGas env 1 world0 { wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeGas 1 10 10 rad 1 1 (seedRng 1))) (U.replicate bins 0) False True (-1) [] []

-- 粒子を作り直す：全員同じ速さ、ばらばらの向き
resetGas :: Env -> Word32 -> World -> World
resetGas env seed w =
  let (g0, r1) = makeGas nPart (fromIntegral (floor (envW env * 0.5) :: Int)) (realToFrac (envH env) - 150) rad 1 1 (seedRng seed)
      (vs, _) = rndList nPart r1
      ang = U.fromList (map (* (2 * pi)) vs)
      g = g0 { gsVX = U.map (\a -> v0 * cos a) ang, gsVY = U.map (\a -> v0 * sin a) ang }
  in w { wGas = g, wHist = U.replicate bins 0, wDev = [], wTracked = -1 }

-- いまの速さの分布（区間ごとの割合 ÷ 区間の幅 = 確率密度）
speedHist :: Gas -> Int -> Double -> U.Vector Double
speedHist g nb vmax = U.accum (+) (U.replicate nb 0)
  [ (b, 1 / (fromIntegral n * dv)) | i <- [0 .. n - 1]
  , let b = floorI (hypot (gsVX g U.! i) (gsVY g U.! i) / dv), b >= 0, b < nb ]
  where dv = vmax / fromIntegral nb
        n = gsN g

-- 分布のずれ = Σ |ヒストグラム − 理論| × 幅（0 なら完全に一致、最大 2）
mbDeviation :: U.Vector Double -> Double -> Double -> Double
mbDeviation h kT vmax = U.ifoldl' (\s b v -> s + abs (v - mb2d ((fromIntegral b + 0.5) * dv) kT 1) * dv) 0 h
  where dv = vmax / fromIntegral (U.length h)

step :: Env -> Float -> World -> IO World
step env _ w = pure w { wGas = g, wHist = hist', wDev = dev' }
  where
    g = if wPaused w then wGas w else gasAdvance timePerFrame (wGas w)
    kT = gasTemp g
    vmax = 4.2 * sqrt kT
    now = speedHist g bins vmax
    hist' = U.zipWith (\h v -> 0.96 * h + 0.04 * v) (wHist w) now     -- 少しずつ平均（ゆれを小さく）
    dev' | not (wPaused w) && envFrame env `mod` 3 == 0 = take 200 (mbDeviation now kT vmax : wDev w)
         | otherwise = wDev w

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱と粒子
  [ Color (grayC 150) (frameTL 2 bx by gw ght) ]
  ++ [ Color (if wColor w then lerpC (80, 150, 255) (255, 90, 40) (clampv 0 1 (s i / (2 * vRef))) else rgbC 200 200 210)
             (at (bx + f2 (gsX g U.! i), by + f2 (gsY g U.! i)) (disc (f2 (gsR g U.! i))))
     | i <- [0 .. gsN g - 1] ]
  ++ [ Color (rgbC 255 240 120) (at (bx + f2 (gsX g U.! t), by + f2 (gsY g U.! t)) (ring 11 2)) | t <- tracked ]
  -- ---- 速さのヒストグラムと理論曲線
  ++ [ plotAxes g1 "speed v" "probability density" [0, vMaxPlot / 2, vMaxPlot] [0, gy1 / 2, gy1] ]
  ++ [ Color (rgbaC 255 150 80 170) (rectTL (gX g1 (fromIntegral b * dv) + 1) y (pW g1 / fromIntegral bins - 2) (pY g1 + pH g1 - y))
     | b <- [0 .. bins - 1], let y = gY g1 (min (wHist w U.! b) gy1) ]
  ++ [ Color (rgbC 130 200 255) (polyline 2.5 [ (gX g1 v, gY g1 (mb2d v kT 1)) | k <- [0 .. 120 :: Int], let v = fromIntegral k / 120 * vMaxPlot ])
     , Color (rgbaC 255 255 255 120) (Line [(gX g1 vp, pY g1), (gX g1 vp, pY g1 + pH g1)]) ]
  -- ---- ずれのグラフ
  ++ [ plotAxes g2 "time ->" "distance from theory  sum |f - f_MB| dv" [] [0, 0.8, 1.6]
     , Color (rgbC 255 210 120) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 d) | (k, d) <- zip [0 :: Int ..] (reverse (wDev w)) ]) ]
  -- ---- 文字
  ++ [ say ("Temperature kT = <mv^2/2> = " ++ fmt 3 kT ++ "  (if 1 = 300 K: " ++ fmt 0 (kT * 300) ++ " K)   total energy " ++ fmt 1 (gasKE g)) 20 36 18 (grayC 240)
     , say "Up heat   Down cool   Space pause   C color   Click follow a particle   R restart (all same speed)" 20 62 15 (grayC 175)
     , say ("Mean speed " ++ fmt 3 mean ++ " (theory sqrt(pi kT/2m) = " ++ fmt 3 (sqrt (pi * kT / 2)) ++ ")   most likely sqrt(kT/m) = " ++ fmt 3 vp ++ " (white)") 20 86 15 (grayC 175)
     , say "Blue = theory (mv/kT) e^(-mv^2/2kT)   orange = measured" (pX g1) (pY g1 + pH g1 + 52) 15 (rgbC 130 200 255) ]
  ++ [ say ("Speed of the followed particle " ++ fmt 2 (s t) ++ " (changes at every collision)") bx (by + ght + 26) 15 (rgbC 255 240 120) | t <- tracked ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = wGas w
    bx = 20
    by = 110
    gw = f2 (gsW g)
    ght = f2 (gsH g)
    kT = gasTemp g
    vRef = sqrt (2 * kT)
    vMaxPlot = 4.2 * sqrt kT
    vp = sqrt kT
    dv = vMaxPlot / fromIntegral bins
    s i = hypot (gsVX g U.! i) (gsVY g U.! i)
    tracked = [ t | let t = wTracked w, t >= 0, t < gsN g ]
    mean = foldl' (\a i -> a + s i / fromIntegral (gsN g)) 0 [0 .. gsN g - 1]
    sw = envW env
    gy1 = 1.1 / sqrt kT * 0.62 + 0.05
    g1 = graph (bx + gw + 70) 120 (sw - gw - 120) (envH env * 0.4) 0 vMaxPlot 0 gy1
    g2y = pY g1 + pH g1 + 80
    g2 = graph (pX g1) g2y (pW g1) (envH env - g2y - 80) 0 200 0 1.6

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wGas = gasSetTemp (min 6 (gasTemp g * 1.15)) g }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGas = gasSetTemp (max 0.05 (gasTemp g / 1.15)) g }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wColor = not (wColor w) }
    | c `elem` "rR" -> do
        seed <- randomRIO (1, 99999)      -- Date.now() % 100000 の代わり
        pure (resetGas env seed w)
  EventKey (MouseButton LeftButton) Down _ (x, y) ->
    let mx = realToFrac (x + envW env / 2) - 20
        my = realToFrac (envH env / 2 - y) - 110
        pick (best, t) i = let d = (gsX g U.! i - mx) ^ (2 :: Int) + (gsY g U.! i - my) ^ (2 :: Int)
                           in if d < best then (d, i) else (best, t)
    in pure w { wTracked = snd (foldl' pick (900, -1) [0 .. gsN g - 1]) }
  _ -> pure w
  where g = wGas w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("head-on collision (equal mass): velocities swap", near (gsVX a1 U.! 0) (-0.5) 1e-12 && near (gsVX a1 U.! 1) 1 1e-12)
  , ("oblique collision (mass 1 and 3): kinetic energy and momentum conserved"
    , near (gasKE h1) e0 1e-12 && near (fst p1) (fst p0) 1e-12 && near (snd p1) (snd p0) 1e-12 && gsVX h1 U.! 0 /= 1.2)
  , ("120 particles, 600 steps: total energy constant (relative 1e-9)", abs (gasKE w1 / gasKE w0 - 1) < 1e-9)
  , ("theory: area = 1, <v^2> = 2kT/m (kT = 1.3)", near ii 1 1e-6 && near v2 2.6 1e-4)
  , ("all same speed (dev " ++ fmt 2 d0 ++ ") -> collisions give Maxwell (dev " ++ fmt 3 d1 ++ " < 0.16)", d0 > 1 && d1 < 0.16)
  , ("heating: scale speeds by sqrt(2/kT) -> <mv^2/2> = kT = 2", near (gasTemp (gasSetTemp 2 q2)) 2 1e-12) ]
  where
    (ga, r1) = makeGas 2 100 100 5 1 1 (seedRng 55)
    a0 = setP 0 40 50 1 0 (setP 1 49 50 (-0.5) 0 ga)
    a1 = gasPairP 0 1 a0
    (hb, r2) = makeGas 2 100 100 5 1 1 r1
    h0 = setP 0 40 50 1.2 0.3 (setP 1 47 56 (-0.4) 0.1 hb { gsM = gsM hb U.// [(1, 3)] })
    e0 = gasKE h0
    p0 = gasMomentum h0
    h1 = gasPairP 0 1 h0
    p1 = gasMomentum h1
    (w0, _) = makeGas 120 260 260 4 1 1 r2
    w1 = gasSteps 600 0.5 w0
    -- 理論の分布を細かい台形で足す（v は dv ずつ足していく：p5.js 版と同じ）
    integ !i !m2 !v | v >= 20 = (i, m2)
                    | otherwise = integ (i + mb2d v 1.3 1 * 0.001) (m2 + v * v * mb2d v 1.3 1 * 0.001) (v + 0.001)
    (ii, v2) = integ 0 0 (0.001 / 2)
    (qa, r3) = makeGas 150 300 300 4 1 1 (seedRng 7)
    (angs, _) = rndList 150 r3
    q = qa { gsVX = U.fromList [cos (a * 2 * pi) | a <- angs], gsVY = U.fromList [sin (a * 2 * pi) | a <- angs] }
    kq = gasTemp q
    vm = 4.2 * sqrt kq
    d0 = mbDeviation (speedHist q 16 vm) kq vm
    run (!gg, !acc) k = let g' = gasStep 0.5 gg
                            acc' = if k >= 300 && k `mod` 10 == 0 then U.zipWith (\a hh -> a + hh / 60) acc (speedHist g' 16 vm) else acc
                        in (g', acc')
    (q2, accH) = foldl' run (q, U.replicate 16 0) [0 .. 899 :: Int]
    d1 = mbDeviation accH kq vm

-- 粒子 i の位置と速度を決める（自己テスト用）
setP :: Int -> Double -> Double -> Double -> Double -> Gas -> Gas
setP i x y vx vy g = g { gsX = gsX g U.// [(i, x)], gsY = gsY g U.// [(i, y)], gsVX = gsVX g U.// [(i, vx)], gsVY = gsVY g U.// [(i, vy)] }

-- ---------- やってみよう ----------
-- 1. R を押して、はじめの「全員同じ速さ」の山が、何秒でなめらかな分布になるか見よう
-- 2. ↑ で加熱すると、分布の山はどう動く？ 横に広がる？ 高さは？
-- 3. 粒子を 1 つクリックして追いかけよう。1 つの粒子の速さはどれくらい変わる？
-- 4. nPart を 60 や 800 に。分布はなめらかになる？ ずれのグラフはどう変わる？
-- 5. rad を 1 にすると衝突が減る。分布に近づくまでの時間はどう変わる？

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

-- ---- 剛体円板の気体（2 次元、換算単位 k = 1）。粒子どうしは弾性衝突、壁ではね返る
-- 近くのペアだけ調べる「セルリスト」：箱をマス目に分け、同じマスととなりのマスの粒子とだけ比べる
data Gas = Gas
  { gsN :: !Int, gsW, gsH :: !Double
  , gsWallX, gsWallU, gsWallMin :: !Double                -- ピストン（右の壁）の位置・速さ・いちばん左
  , gsX, gsY, gsVX, gsVY, gsR, gsM :: !(U.Vector Double)
  , gsImp, gsImpX, gsMeterT, gsTime :: !Double           -- 壁が受けた力積（全部・ピストンだけ）、測った時間
  , gsCS :: !Double, gsNX, gsNY :: !Int                   -- マス目の大きさと数
  , gsBig :: !(U.Vector Int) }                            -- 大きな粒子（セルリストに入れない）

makeGas :: Int -> Double -> Double -> Double -> Double -> Double -> Rng -> (Gas, Rng)
makeGas n w h r kT m r0 = (gasCells g, rEnd)
  where
    cols = max 1 (ceiling (sqrt (fromIntegral n * w / h)) :: Int)
    rows = ceiling (fromIntegral n / fromIntegral cols :: Double) :: Int
    s = sqrt (kT / m)
    one (acc, rg) i =                    -- 格子に少しずらして並べる（重ならない）
      let (a, rg1) = rnd rg
          (b, rg2) = rnd rg1
          (c, rg3) = rndNormal rg2
          (d, rg4) = rndNormal rg3
          x = (fromIntegral (i `mod` cols) + 0.5 + 0.2 * (a - 0.5)) * (w / fromIntegral cols)
          y = (fromIntegral (i `div` cols) + 0.5 + 0.2 * (b - 0.5)) * (h / fromIntegral rows)
      in ((x, y, s * c, s * d) : acc, rg4)
    (ps, rEnd) = foldl' one ([], r0) [0 .. n - 1]
    qs = reverse ps
    g = Gas n w h w 0 (4 * r) (U.fromList [a | (a, _, _, _) <- qs]) (U.fromList [b | (_, b, _, _) <- qs])
            (U.fromList [c | (_, _, c, _) <- qs]) (U.fromList [d | (_, _, _, d) <- qs])
            (U.replicate n r) (U.replicate n m) 0 0 0 0 1 1 1 U.empty

-- マス目の大きさ = ふつうの粒子の直径の最大（大きな粒子は別あつかい）
gasCells :: Gas -> Gas
gasCells g = g { gsCS = cs, gsNX = max 1 (floorI (gsW g / cs)), gsNY = max 1 (floorI (gsH g / cs)), gsBig = big }
  where
    r0 = if gsN g > 0 then gsR g U.! 0 else 1
    big = U.findIndices (> 2 * r0) (gsR g)
    rs = U.foldl' max 0 (U.filter (<= 2 * r0) (gsR g))
    cs = 2 * rs + 1e-9

-- 動かす間だけ書きかえられる配列にする
data MGas s = MGas { mX, mY, mVX, mVY :: !(UM.MVector s Double) }

withGas :: Gas -> (forall s. MGas s -> ST s Gas) -> Gas
withGas g act = runST $ do
  m <- MGas <$> U.thaw (gsX g) <*> U.thaw (gsY g) <*> U.thaw (gsVX g) <*> U.thaw (gsVY g)
  g' <- act m
  x <- U.freeze (mX m); y <- U.freeze (mY m); vx <- U.freeze (mVX m); vy <- U.freeze (mVY m)
  pure g' { gsX = x, gsY = y, gsVX = vx, gsVY = vy }

rd :: UM.MVector s Double -> Int -> ST s Double
rd = UM.unsafeRead

wr :: UM.MVector s Double -> Int -> Double -> ST s ()
wr = UM.unsafeWrite

-- 重なっていて近づいている 2 粒子を弾性衝突させる（運動量とエネルギーを保存）
pairM :: Gas -> MGas s -> Int -> Int -> ST s ()
pairM g v i j = do
  xi <- rd (mX v) i; yi <- rd (mY v) i; xj <- rd (mX v) j; yj <- rd (mY v) j
  let dx = xj - xi; dy = yj - yi
      ri = U.unsafeIndex (gsR g) i; rj = U.unsafeIndex (gsR g) j
      rr = ri + rj
      d2 = dx * dx + dy * dy
  unless (d2 >= rr * rr || d2 == 0) $ do
    let d = sqrt d2; nx = dx / d; ny = dy / d
        mi = U.unsafeIndex (gsM g) i; mj = U.unsafeIndex (gsM g) j
    vxi <- rd (mVX v) i; vyi <- rd (mVY v) i; vxj <- rd (mVX v) j; vyj <- rd (mVY v) j
    let vn = (vxi - vxj) * nx + (vyi - vyj) * ny
    when (vn > 0) $ do
      let jj = ((2 * mi * mj) / (mi + mj)) * vn
      wr (mVX v) i (vxi - (jj / mi) * nx); wr (mVY v) i (vyi - (jj / mi) * ny)
      wr (mVX v) j (vxj + (jj / mj) * nx); wr (mVY v) j (vyj + (jj / mj) * ny)
    let ov = rr - d; wi = mj / (mi + mj); wj = mi / (mi + mj)   -- 重なりを押しもどす（速度は変えない）
    wr (mX v) i (xi - nx * ov * wi); wr (mY v) i (yi - ny * ov * wi)
    wr (mX v) j (xj + nx * ov * wj); wr (mY v) j (yj + ny * ov * wj)

-- となりのマス（半分だけ = 二重に数えない）
neigh :: [(Int, Int)]
neigh = [(1, 0), (-1, 1), (0, 1), (1, 1)]

collideM :: Gas -> MGas s -> ST s ()
collideM g v = do
  let nx = gsNX g; ny = gsNY g; cs = gsCS g; n = gsN g
      isBig = U.accum (\_ b -> b) (U.replicate n False) [ (b, True) | b <- U.toList (gsBig g) ]
  hd <- UM.replicate (nx * ny) (-1 :: Int)
  nxt <- UM.replicate (max 1 n) (-1 :: Int)
  forM_ [0 .. n - 1] $ \i -> unless (U.unsafeIndex isBig i) $ do
    x <- rd (mX v) i; y <- rd (mY v) i
    let c = cellOf (x / cs) nx + nx * cellOf (y / cs) ny
    h <- UM.unsafeRead hd c
    UM.unsafeWrite nxt i h
    UM.unsafeWrite hd c i
  let walk !j f = when (j >= 0) (f j >> UM.unsafeRead nxt j >>= \j' -> walk j' f)
  forM_ [0 .. ny - 1] $ \cy -> forM_ [0 .. nx - 1] $ \cx -> do
    h0 <- UM.unsafeRead hd (cx + nx * cy)
    walk h0 $ \i -> do
      ni <- UM.unsafeRead nxt i
      walk ni (pairM g v i)                                         -- 同じマス
      forM_ neigh $ \(ox, oy) -> do
        let ax = cx + ox; ay = cy + oy
        unless (ax < 0 || ax >= nx || ay >= ny) $ do
          hj <- UM.unsafeRead hd (ax + nx * ay)
          walk hj (pairM g v i)
  U.forM_ (gsBig g) $ \b -> forM_ [0 .. n - 1] $ \j -> when (j /= b) (pairM g v b j)

-- マスの番号（0〜n−1 におさめる）
cellOf :: Double -> Int -> Int
cellOf q n | not (q >= 0) = 0
           | q >= fromIntegral (n - 1) = n - 1
           | otherwise = floor q

-- 壁ではね返る。戻り値は (壁が受けた速度の変化 hit、ピストンが受けた力積)
-- 右の壁はピストン（速さ wallU で動く。動く壁ではね返ると気体は仕事をする / される）
wallsM :: Gas -> MGas s -> Int -> ST s (Double, Double)
wallsM g v i = do
  let ri = U.unsafeIndex (gsR g) i; mi = U.unsafeIndex (gsM g) i
      xr = gsWallX g - ri; hh = gsH g; u = gsWallU g
  x0 <- rd (mX v) i
  h1 <- if x0 < ri then do wr (mX v) i (min (2 * ri - x0) xr); flipNeg (mVX v) 0 else pure 0
  x1 <- rd (mX v) i
  (h2, ix) <- if x1 > xr
    then do wr (mX v) i (max (2 * xr - x1) ri)
            vx <- rd (mVX v) i
            if vx > u then do let nv = 2 * u - vx
                              wr (mVX v) i nv
                              pure (vx - nv, mi * (vx - nv))
                      else pure (h1, 0)
    else pure (h1, 0)
  y0 <- rd (mY v) i
  h3 <- if y0 < ri then do wr (mY v) i (min (2 * ri - y0) (hh - ri)); flipNeg (mVY v) h2 else pure h2
  y1 <- rd (mY v) i
  h4 <- if y1 > hh - ri
    then do wr (mY v) i (max (2 * (hh - ri) - y1) ri)
            vy <- rd (mVY v) i
            if vy > 0 then wr (mVY v) i (-vy) >> pure (2 * vy) else pure h3
    else pure h3
  pure (h4, ix)
  where flipNeg arr hOld = do
          vv <- rd arr i
          if vv < 0 then wr arr i (-vv) >> pure (-2 * vv) else pure hOld

-- 1 回分（時間 dt）進める
stepM :: Gas -> MGas s -> Double -> ST s Gas
stepM g0 v dt = do
  let g = g0 { gsWallX = clampv (gsWallMin g0) (gsW g0) (gsWallX g0 + gsWallU g0 * dt) }
      go !i !imp !impX
        | i >= gsN g = pure (imp, impX)
        | otherwise = do
            x <- rd (mX v) i; y <- rd (mY v) i; vx <- rd (mVX v) i; vy <- rd (mVY v) i
            wr (mX v) i (x + vx * dt); wr (mY v) i (y + vy * dt)
            (hit, ix) <- wallsM g v i
            let imp' = if hit > 0 then imp + U.unsafeIndex (gsM g) i * hit else imp
            go (i + 1) imp' (impX + ix)
  (imp, impX) <- go 0 (gsImp g) (gsImpX g)
  collideM g v
  pure g { gsImp = imp, gsImpX = impX, gsTime = gsTime g + dt, gsMeterT = gsMeterT g + dt }

-- k 回まとめて進める
gasSteps :: Int -> Double -> Gas -> Gas
gasSteps k dt g = withGas g (\v -> let go 0 gg = pure gg
                                       go j gg = stepM gg v dt >>= go (j - 1 :: Int)
                                   in go k g)

gasStep :: Double -> Gas -> Gas
gasStep = gasSteps 1

-- 1 コマ分（時間 tt）を進める。いちばん速い粒子でも 1 回に半径の 1/3 しか動かないように細かく分ける
gasAdvance :: Double -> Gas -> Gas
gasAdvance tt g = gasSteps nsub (tt / fromIntegral nsub) g
  where
    v2 = U.foldl' max 0 (U.zipWith (\a b -> a * a + b * b) (gsVX g) (gsVY g))
    rmin = U.foldl' min (1 / 0) (gsR g)
    q = (sqrt v2 + abs (gsWallU g)) * tt / (0.33 * rmin)
    nsub = if isNaN q || isInfinite q then 60 else max 1 (min 60 (ceiling q))

-- 2 粒子だけ衝突させる（自己テスト用）
gasPairP :: Int -> Int -> Gas -> Gas
gasPairP i j g = withGas g (\v -> pairM g v i j >> pure g)

gasKE :: Gas -> Double
gasKE g = U.ifoldl' (\e i m -> e + 0.5 * m * (gsVX g U.! i * gsVX g U.! i + gsVY g U.! i * gsVY g U.! i)) 0 (gsM g)

-- 2 次元：平均運動エネルギー ⟨½mv²⟩ = kT
gasTemp :: Gas -> Double
gasTemp g = gasKE g / fromIntegral (max 1 (gsN g))

gasMomentum :: Gas -> (Double, Double)
gasMomentum g = U.ifoldl' (\(!px, !py) i m -> (px + m * gsVX g U.! i, py + m * gsVY g U.! i)) (0, 0) (gsM g)

gasSetTemp :: Double -> Gas -> Gas
gasSetTemp kT g = g { gsVX = U.map (* f) (gsVX g), gsVY = U.map (* f) (gsVY g) }
  where f = sqrt (clampv 1e-6 1e6 kT / max 1e-12 (gasTemp g))

-- 2 次元のマクスウェル-ボルツマン分布（速さ）
mb2d :: Double -> Double -> Double -> Double
mb2d v kT m = ((m * v) / kT) * exp ((-m * v * v) / (2 * kT))

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

-- 有効数字 3 桁（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0'
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin 0 v0'

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
