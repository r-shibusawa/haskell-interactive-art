-- ============================================
-- 55-01 理想気体の状態方程式 ─ 壁にぶつかる分子から圧力を測る
-- 圧力 = 壁が受けた力積（運動量の変化の合計）÷（壁の長さ × 時間）。2 次元なので「体積」は面積 A
-- 1.5 秒ごとに P を測って点を打つ。理想気体なら PA = NkT（PA/(NkT) = 1）。粒子に大きさがあるぶん少しだけ 1 より大きい
-- 熱浴 ON = 温度を一定に保つ（ボイルの法則）。OFF でピストンを押し込むと、気体が仕事をされて温まる（断熱圧縮）
-- 操作：← → / ドラッグ ピストンを動かす / ↑ ↓ 温度 / N 粒子 +20  M 粒子 −20 / H 熱浴 ON/OFF / P 定圧モード / C グラフを消す
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
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

rad :: Double
rad = 3                    -- ★ 粒子の半径

window :: Int
window = 90                -- ★ 何コマごとに圧力を測るか（60 コマ ≈ 1 秒）

frameT :: Double
frameT = 2                 -- 1 コマで進める時間

bx, by :: Float
bx = 20
by = 120

-- 1 回の測定
data Pt = Pt { ptP, ptA :: !Double, ptN :: !Int, ptKT, ptZ :: !Double }

data World = World
  { wGas     :: !Gas
  , wRng     :: !Rng               -- 粒子を作り直すときに使う乱数（種は最初に 1 回だけ）
  , wNPart   :: !Int               -- ★ 粒子の数
  , wTargetT :: !Double            -- ★ 熱浴の温度
  , wBath    :: !Bool
  , wConstP  :: !Bool
  , wPSet    :: !Double
  , wPoints  :: [Pt]               -- 測定の点（新しい順、120 個まで）
  , wLast    :: Maybe Pt
  , wPEst    :: !Double            -- その場の圧力（なめらかに）
  , wImpPrev :: !Double
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-01 Ideal gas law" world0)
  { skBack  = rgbC 22 24 30
  , skStart = \env -> do
      printSelfTest selfTest
      pure (makeBox env 0.7 world0 { wTest = selfTest, wRng = seedRng 3 })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeGas 1 10 10 rad 1 1 (seedRng 1))) (seedRng 3) 150 1 True False 0 [] Nothing 0 0 []

-- 箱を作り直す：粒子はピストンの内側（左から frac の割合）にまとめる
makeBox :: Env -> Double -> World -> World
makeBox env frac w =
  let (g0, r') = makeGas (wNPart w) (fromIntegral (floor (envW env * 0.45) :: Int)) (realToFrac (envH env) - 220) rad (wTargetT w) 1 (wRng w)
      g = g0 { gsWallMin = gsW g0 * 0.2, gsX = U.map (* frac) (gsX g0), gsWallX = gsW g0 * frac }
  in w { wGas = gasResetMeter g, wRng = r', wImpPrev = 0
       , wPEst = fromIntegral (wNPart w) * wTargetT w / (gsWallX g * gsH g) }

-- マウスの位置（p5.js と同じ、左上が原点）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

step :: Env -> Float -> World -> IO World
step env _ w = pure w2
  where
    g = wGas w
    (mx, my) = mouseP env
    -- ---- ピストンの動き：キー・ドラッグ・定圧モード
    u0 | isDown env (SpecialKey KeyRight) = 0.4
       | isDown env (SpecialKey KeyLeft) = -0.4
       | otherwise = 0
    u1 | envDown env && mx > bx && mx < bx + f2 (gsW g) + 30 && my > by && my < by + f2 (gsH g)
         = clampv (-0.4) 0.4 ((realToFrac mx - realToFrac bx - gsWallX g) * 0.02)
       | otherwise = u0
    u2 | wConstP w = clampv (-0.3) 0.3 (2 * (wPEst w / wPSet w - 1))   -- 圧力が高すぎたら広げ、低すぎたら縮める
       | otherwise = u1
    ga = gasAdvance frameT g { gsWallU = u2 }
    gb = if wBath w then gasSetTemp (wTargetT w) ga else ga
    dImp = gsImp gb - wImpPrev w
    pEst = 0.95 * wPEst w + 0.05 * (dImp / (2 * (gsWallX gb + gsH gb) * frameT))
    w1 = w { wGas = gb, wImpPrev = gsImp gb, wPEst = pEst }
    w2 | envFrame env `mod` window == 0 =
           let pp = gasPressure gb
               a = gsWallX gb * gsH gb
               kT = gasTemp gb
               pt = Pt pp a (gsN gb) kT (pp * a / (fromIntegral (gsN gb) * kT))
           in w1 { wLast = Just pt, wPoints = take 120 (pt : wPoints w1), wGas = gasResetMeter gb, wImpPrev = 0 }
       | otherwise = w1

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱・粒子・ピストン
  [ Color (grayC 150) (Pictures [ thickLine 2 (bx, by) (bx + gw + 20, by), thickLine 2 (bx, by + ght) (bx + gw + 20, by + ght), thickLine 2 (bx, by) (bx, by + ght) ]) ]
  ++ [ Color (rgbC 255 150 90) (Pictures [ at (bx + f2 (gsX g U.! i), by + f2 (gsY g U.! i)) (disc (f2 (gsR g U.! i))) | i <- [0 .. gsN g - 1] ]) ]
  ++ [ Color (rgbC 170 180 200) (Pictures [ rectTL (bx + wx) (by + 2) 12 (ght - 4), rectTL (bx + wx + 12) (by + ght / 2 - 6) (gw - wx + 30) 12 ]) ]
  ++ [ Color (rgbaC 255 120 60 60) (rectTL (bx - 12) by 8 ght) | wBath w ]
  -- ---- グラフ 1：P と A（ボイルの法則）、グラフ 2：PA/N と kT（傾き 1 の直線）
  ++ [ plotAxes g1 "area A" "pressure P" [0, x1 g1 / 2, x1 g1] (map prec2 [0, y1 g1 / 2, y1 g1])
     , Color (rgbC 130 200 255) (polyline 2 [ (gX g1 a, gY g1 (fromIntegral (gsN g) * wTargetT w / a)) | k <- [0 .. 79 :: Int], let a = x1 g1 * (0.08 + 0.92 * fromIntegral k / 79) ])
     , plotAxes g2 "temperature kT" "PA / N" [0, 1, 2, 3] [0, 1, 2, 3]
     , Color (rgbC 130 200 255) (thickLine 1 (gX g2 0, gY g2 0) (gX g2 3, gY g2 3)) ]
  ++ concat [ [ Color c (at (gX g1 (ptA p), gY g1 (ptP p)) (disc 4)), Color c (at (gX g2 (ptKT p), gY g2 (ptP p * ptA p / fromIntegral (ptN p))) (disc 4)) ]
            | p <- reverse (wPoints w), let c = lerpC (90, 160, 255) (255, 90, 50) (clampv 0 1 (ptKT p / 3)) ]
  -- ---- 文字と表
  ++ [ say "Check the ideal gas law PA = NkT from molecular collisions  (2D, reduced units k = m = 1)" 20 36 18 (grayC 240)
     , say ("Left/Right or drag piston   Up/Down temperature   N / M particles +-20   H bath " ++ (if wBath w then "ON (isothermal)" else "OFF (adiabatic)")
            ++ "   P const. P " ++ (if wConstP w then "ON" else "OFF") ++ "   C clear") 20 62 15 (grayC 175)
     , say "Blue line = theory P = NkT/A (current N and temperature). Dot colour = temperature (blue low -> red high)" 20 86 15 (grayC 175) ]
  ++ concat [ [ say ("Latest:  P = " ++ expo 3 (ptP p) ++ "   A = " ++ fmt 0 (ptA p) ++ "   N = " ++ show (ptN p) ++ "   kT = " ++ fmt 3 (ptKT p)) bx (by + ght + 28) 15 (grayC 235)
              , say ("PA / (NkT) = " ++ fmt 3 (ptZ p) ++ "   (area fraction eta = " ++ fmt 1 (eta * 100) ++ "%, 1 + 2 eta = " ++ fmt 3 (1 + 2 * eta) ++ ")") bx (by + ght + 50) 15 (rgbC 255 210 120)
              , say "Above 1 because the particles have a size (more so at small volume)" bx (by + ght + 70) 14 (grayC 170) ]
            | Just p <- [wLast w], let eta = fromIntegral (gsN g) * pi * rad * rad / ptA p ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = wGas w
    gw = f2 (gsW g)
    ght = f2 (gsH g)
    wx = f2 (gsWallX g)
    gx = bx + gw + 110
    gwid = envW env - gx - 40
    gh = (envH env - 260) / 2
    g1 = graph gx by gwid gh 0 (gsW g * gsH g) 0 (max 0.004 (3 * fromIntegral (gsN g) * wTargetT w / (gsW g * gsH g)))
    g2 = graph gx (by + gh + 70) gwid gh 0 3 0 3.6

event :: Env -> Event -> World -> IO World
event env ev w = pure (limitT (case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTargetT = min 3 (wTargetT w + 0.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTargetT = max 0.25 (wTargetT w - 0.25) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> makeBox env (gsWallX g / gsW g) w { wNPart = min 400 (wNPart w + 20) }
    | c `elem` "mM" -> makeBox env (gsWallX g / gsW g) w { wNPart = max 20 (wNPart w - 20) }
    | c `elem` "hH" -> let b = not (wBath w)
                       in w { wBath = b, wTargetT = if b then clampv 0.25 3 (jsRound (gasTemp g * 4) / 4) else wTargetT w }
    | c `elem` "pP" -> w { wConstP = not (wConstP w), wPSet = max 1e-5 (wPEst w) }
    | c `elem` "cC" -> w { wPoints = [] }
  _ -> w))
  where
    g = wGas w
    -- 熱浴なしで熱くなりすぎたら kT = 4 におさえる（キーを押したとき）
    limitT v = case ev of
      EventKey (MouseButton _) _ _ _ -> v
      EventKey _ Down _ _ | not (wBath v) && gasTemp (wGas v) > 4 -> v { wGas = gasSetTemp 4 (wGas v) }
      _ -> v

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("1 particle between the walls: right-wall pressure = mv^2/((L - 2r) H) (1%)", near (gsImpX s1 / (gsH s1 * gsTime s1) / (4 / (99 * 50))) 1 0.01)
  , ("100 particles: PA/(NkT) = " ++ fmt 3 z1 ++ " (within 6% of 1 + 2 eta = " ++ fmt 3 (1 + 2 * eta) ++ ")", abs (z1 / (1 + 2 * eta) - 1) < 0.06)
  , ("double the temperature -> pressure about doubles (ratio " ++ fmt 2 (gasPressure w3 / p1) ++ ", +-12%)", abs (gasPressure w3 / p1 / 2 - 1) < 0.12)
  , ("bounce off a piston approaching at 0.5: speed 1 -> 2 (elastic in the wall frame)", near (gsVX m1 U.! 0) (-2) 1e-12)
  , ("no bath, slowly compress to half: 2D gas (gamma = 2) keeps T*A constant (ratio " ++ fmt 3 (ta1 / ta0) ++ ", +-8%)", abs (ta1 / ta0 - 1) < 0.08 && gasTemp a3 > 1.5) ]
  where
    (s0, r1) = makeGas 1 100 50 0.5 1 1 (seedRng 11)
    s1 = gasSteps 20000 0.25 s0 { gsX = U.fromList [30], gsY = U.fromList [25], gsVX = U.fromList [2], gsVY = U.fromList [0] }
    (w0, r2) = makeGas 100 400 400 3 1 1 r1
    w1 = gasResetMeter (gasSteps 400 0.5 w0)
    w2 = gasSteps 1600 0.5 w1
    z1 = gasPressure w2 * 400 * 400 / (100 * gasTemp w2)
    eta = 100 * pi * 9 / 160000
    p1 = gasPressure w2
    w3 = gasSteps 1600 0.5 (gasResetMeter (gasSetTemp (2 * gasTemp w2) w2))
    (m0, r3) = makeGas 1 100 50 0.5 1 1 r2
    m1 = gasWallsP 0 m0 { gsX = U.fromList [99.7], gsVX = U.fromList [1], gsVY = U.fromList [0], gsWallU = -0.5 }
    (a0, _) = makeGas 60 300 300 2 1 1 r3
    a1 = gasSteps 400 0.5 a0
    ta0 = gasTemp a1 * gsWallX a1 * gsH a1
    compress gg | gsWallX gg > 150.5 = compress (gasStep 0.5 gg)
                | otherwise = gg
    a3 = (compress a1 { gsWallU = -0.03 }) { gsWallU = 0 }
    ta1 = gasTemp a3 * gsWallX a3 * gsH a3

-- 有効数字 2 桁（目盛り用）
prec2 :: Double -> Double
prec2 v | v == 0 = 0
        | otherwise = let k = 10 ^^ (1 - (floor (logBase 10 (abs v)) :: Int)) in fromIntegral (round (v * k) :: Integer) / k

-- 指数表記（toExponential）
expo :: Int -> Double -> String
expo n v = showEFloat (Just n) (fin 0 v) ""

-- JavaScript の Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Int)

-- ---------- やってみよう ----------
-- 1. 熱浴 ON のまま、ピストンを少しずつ動かして点を打とう。点は青い曲線 P = NkT/A に乗る？（ボイルの法則）
-- 2. ↑ で温度を上げると、点は下のグラフの直線（傾き 1）の上を右へ動く？
-- 3. P で定圧モードにして ↑ で加熱。ピストンはどう動く？ A と T の関係は？（シャルルの法則）
-- 4. H で熱浴を切り、ピストンを押し込むと温度は上がる？ 自転車の空気入れが熱くなるのと同じ
-- 5. N で粒子を 400 にして小さく押し込むと、PA/(NkT) は 1 からどれくらいずれる？

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

-- 1 粒子だけ壁ではね返す（自己テスト用）
gasWallsP :: Int -> Gas -> Gas
gasWallsP i g = withGas g (\v -> do
  (hit, ix) <- wallsM g v i
  pure g { gsImp = gsImp g + (if hit > 0 then gsM g U.! i * hit else 0), gsImpX = gsImpX g + ix })

-- 力積 ÷（壁の長さ × 時間）
gasPressure :: Gas -> Double
gasPressure g = gsImp g / max 1e-9 (2 * (gsWallX g + gsH g) * gsMeterT g)

gasResetMeter :: Gas -> Gas
gasResetMeter g = g { gsImp = 0, gsImpX = 0, gsMeterT = 0 }

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
