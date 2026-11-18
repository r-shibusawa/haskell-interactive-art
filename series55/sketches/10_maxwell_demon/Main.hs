-- ============================================
-- 55-10 マクスウェルの悪魔 ─ 速い分子だけを右へ通すと、温度差が生まれる？
-- 仕切りの扉の前に来た分子の速さを悪魔が測り、速い分子は左→右、遅い分子は右→左だけ通す（ほかははね返す）
-- 右が熱く、左が冷たくなり、気体のエントロピーは減る。でも悪魔は 1 回測るごとに 1 ビットを記憶する
-- 記憶を消すには 1 ビットあたり kT ln 2 の熱が出る（ランダウアーの原理）→ 合計のエントロピーは減らない（第二法則）
-- 操作：スペース 悪魔 ON/OFF / A 自動 ⇔ 手動（手動はマウスを押している間だけ扉が開く）/ ↑↓ 速さのしきい値 / R 最初から
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
nPart = 220                -- ★ 分子の数

rad, door :: Double
rad = 3                    -- ★ 分子の半径
door = 70                  -- ★ 扉の高さ [px]

bx, by :: Float
bx = 20
by = 120

data World = World
  { wGas    :: !Gas
  , wDemon  :: !Bool               -- 悪魔がいるか
  , wAuto   :: !Bool               -- 自動 ⇔ 手動
  , wThr    :: !Double             -- ★ しきい値（平均の速さの何倍か）
  , wS0     :: !Double             -- はじめのエントロピー
  , wT0     :: !Double             -- はじめの温度
  , wLogT   :: [(Double, Double)]  -- 左右の温度（新しい順、300 個まで）
  , wLogS   :: [(Double, Double)]  -- (−ΔS_気体, ビット × ln 2)
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-10 Maxwell's demon" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetDemon env 4 world0 { wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeDemonGas 2 10 10 (seedRng 1))) True True 1 0 1 [] [] []

resetDemon :: Env -> Word32 -> World -> World
resetDemon env seed w =
  let (g, _) = makeDemonGas nPart (fromIntegral (floor (envW env * 0.5) :: Int)) (realToFrac (envH env) - 230) (seedRng seed)
  in w { wGas = g, wT0 = gasTemp g, wS0 = gasEntropy g, wLogT = [], wLogS = [] }

-- 仕切りつきの箱：はじめにどちらの側にいるかを覚える
makeDemonGas :: Int -> Double -> Double -> Rng -> (Gas, Rng)
makeDemonGas n w h r0 =
  let (g, r1) = makeGas n w h rad 1 1 r0
      side = U.map (\x -> if x < w / 2 then 0 else 1) (gsX g)
  in (g { gsDemon = Demon True True False False 1.4 0 0 side }, r1)

-- 左右それぞれの数・温度
sides :: Gas -> ((Int, Int), (Double, Double))
sides g = ((n0, n1), (e0 / fromIntegral (max 1 n0), e1 / fromIntegral (max 1 n1)))
  where
    (n0, n1, e0, e1) = foldl' add (0, 0, 0, 0) [0 .. gsN g - 1]
    add (!a, !b, !c, !d) i =
      let e = 0.5 * (gsVX g U.! i ^ (2 :: Int) + gsVY g U.! i ^ (2 :: Int))
      in if gsX g U.! i < gsW g / 2 then (a + 1, b, c + e, d) else (a, b + 1, c, d + e)

-- 2 次元の理想気体のエントロピー S = N[ln(A/N) + ln T]（k = 1、定数は省く）
sIdeal :: Int -> Double -> Double -> Double
sIdeal n a t | n > 0 = fromIntegral n * (log (a / fromIntegral n) + log (max t 1e-9))
             | otherwise = 0

gasEntropy :: Gas -> Double
gasEntropy g = let ((n0, n1), (t0, t1)) = sides g
                   a = (gsW g / 2) * gsH g
               in sIdeal n0 a t0 + sIdeal n1 a t1

step :: Env -> Float -> World -> IO World
step env _ w = pure w { wGas = g, wLogT = logT, wLogS = logS }
  where
    g0 = wGas w
    dm = (gsDemon g0) { dmOn = wDemon w, dmAuto = wAuto w, dmThr = wThr w * sqrt (2 * wT0 w) * 0.886, dmManual = envDown env }
    g = gasAdvance 2 g0 { gsDemon = dm }
    (_, (tl, tr)) = sides g
    dS = gasEntropy g - wS0 w
    bits = dmBits (gsDemon g)
    (logT, logS) | envFrame env `mod` 3 == 0 = (take 300 ((tl, tr) : wLogT w), take 300 ((-dS, fromIntegral bits * log 2) : wLogS w))
                 | otherwise = (wLogT w, wLogS w)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱
  [ Color (rgbaC 60 90 160 50) (rectTL bx by (gw / 2) ght), Color (rgbaC 200 80 50 50) (rectTL mid by (gw / 2) ght)
  , Color (grayC 150) (frameTL 2 bx by gw ght)
  , Color (grayC 230) (Pictures ([thickLine 4 (mid, by) (mid, d0), thickLine 4 (mid, d0 + dr) (mid, by + ght)] ++ [ thickLine 4 (mid, d0) (mid, d0 + dr) | not (wDemon w) ])) ]
  ++ [ Color (if s i > dmThr dm then rgbC 255 120 70 else rgbC 110 170 255) (at (bx + f2 (gsX g U.! i), by + f2 (gsY g U.! i)) (disc (f2 rad))) | i <- [0 .. gsN g - 1] ]
  -- 悪魔
  ++ [ Color (rgbC 170 90 220) (Pictures [ at (mid, d0 - 24) (disc 13), Polygon [(mid - 12, d0 - 30), (mid - 6, d0 - 34), (mid - 14, d0 - 44)], Polygon [(mid + 12, d0 - 30), (mid + 6, d0 - 34), (mid + 14, d0 - 44)] ])
     , Color white (Pictures [ at (mid - 5, d0 - 26) (disc 3), at (mid + 5, d0 - 26) (disc 3) ])
     , say "demon" (mid + 18) (d0 - 20) 14 (grayC 240) ]
  -- ---- グラフ：左右の温度、エントロピーの収支
  ++ [ plotAxes g1 "time ->" "temperature kT (left blue, right red)" [] (map (\v -> fromIntegral (floor (v * 100 + 0.5) :: Int) / 100) [0, wT0 w, 2 * wT0 w])
     , Color (rgbC 110 170 255) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 a) | (k, (a, _)) <- zip [0 :: Int ..] logT ])
     , Color (rgbC 255 120 70) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 b) | (k, (_, b)) <- zip [0 :: Int ..] logT ])
     , plotAxes g2 "time ->" "entropy (k = 1)" [] [0, fromIntegral (floor (smax + 0.5) :: Int)]
     , Color (rgbC 255 200 100) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 a) | (k, (a, _)) <- zip [0 :: Int ..] logS ])
     , Color (rgbC 200 140 255) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 b) | (k, (_, b)) <- zip [0 :: Int ..] logS ])
     , say "decrease of gas entropy -dS_gas" (pX g2 + 8) (pY g2 + 18) 14 (rgbC 255 200 100)
     , say "cost of erasing memory: bits x ln 2" (pX g2 + 8) (pY g2 + 38) 14 (rgbC 200 140 255) ]
  -- ---- 文字
  ++ [ say ("Maxwell's demon   " ++ (if wDemon w then (if wAuto w then "auto (fast -> right, slow -> left only)" else "manual (hold the mouse to open)") else "door closed (Space: demon ON)")
            ++ "   threshold speed " ++ fmt 2 (dmThr dm)) 20 36 18 (grayC 240)
     , say "Space demon ON/OFF   A auto / manual   Up/Down threshold   R restart  |  red = faster than threshold, blue = slower" 20 62 15 (grayC 175)
     , say ("Left " ++ show nl ++ "  kT = " ++ fmt 3 tl ++ "   Right " ++ show nr ++ "  kT = " ++ fmt 3 tr ++ "   dS_gas = " ++ fmt 2 dS ++ "   memory " ++ show bits
            ++ " bits -> erase " ++ fmt 1 cost ++ " k   total " ++ fmt 1 (dS + cost) ++ (if -dS <= cost then " >= 0 OK" else "")) 20 86 15 (rgbC 255 210 120)
     , say "Bits and entropy: see series 6 on information theory (1 bit = ln 2)" bx (by + ght + 28) 14 (grayC 170) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = wGas w
    dm = gsDemon g
    gw = f2 (gsW g)
    ght = f2 (gsH g)
    dr = f2 door
    mid = bx + gw / 2
    d0 = by + ght / 2 - dr / 2
    s i = hypot (gsVX g U.! i) (gsVY g U.! i)
    ((nl, nr), (tl, tr)) = sides g
    dS = gasEntropy g - wS0 w
    bits = dmBits dm
    cost = fromIntegral bits * log 2
    logT = reverse (wLogT w)
    logS = reverse (wLogS w)
    gx = bx + gw + 70
    gwid = envW env - gx - 30
    gh = (ght - 80) / 2
    g1 = graph gx by gwid gh 0 300 0 (2.5 * wT0 w)
    smax = maximum (5 : concat [ [a, b] | (a, b) <- logS ])
    g2 = graph gx (by + gh + 80) gwid gh 0 300 (minimum (0 : map fst logS)) (smax * 1.1)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wDemon = not (wDemon w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wThr = min 2 (wThr w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wThr = max 0.3 (wThr w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> do
        seed <- randomRIO (1, 999)          -- Date.now() % 1000 の代わり
        pure (resetDemon env seed w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("door closed: numbers on each side and total energy stay the same", fst (sides a0) == fst (sides a1) && abs (gasKE a1 / gasKE a0 - 1) < 1e-9)
  , ("entropy is extensive: S(2N, 2A, T) = 2 S(N, A, T); moving heat to the hot side lowers S"
    , near (sIdeal 200 2000 1.3) (2 * sIdeal 100 1000 1.3) 1e-9 && sIdeal 100 1000 0.8 + sIdeal 100 1000 1.2 < 2 * sIdeal 100 1000 1)
  , ("auto demon: right gets hot, left cold (right " ++ fmt 2 tr ++ " / left " ++ fmt 2 tl ++ " > 1.15)", tr / tl > 1.15)
  , ("gas entropy decreases (dS = " ++ fmt 2 dS ++ "), but memory " ++ show bits ++ " bits x ln 2 = " ++ fmt 1 (fromIntegral bits * log 2) ++ " is larger"
    , dS < 0 && fromIntegral bits * log 2 >= -dS)
  , ("Landauer limit: kT ln 2 at 300 K = 2.87 x 10^-21 J", near (1.380649e-23 * 300 * log 2) 2.871e-21 1e-24) ]
  where
    (ga, r1) = makeDemonGas 120 300 240 (seedRng 10)
    a0 = ga { gsDemon = (gsDemon ga) { dmOn = False } }
    a1 = gasSteps 1500 0.5 a0
    (d0, _) = makeDemonGas 120 300 240 r1
    sd0 = gasEntropy d0
    d1 = gasSteps 6000 0.5 d0 { gsDemon = (gsDemon d0) { dmThr = sqrt (2 * gasTemp d0) * 0.886 } }
    (_, (tl, tr)) = sides d1
    dS = gasEntropy d1 - sd0
    bits = dmBits (gsDemon d1)

-- ---------- やってみよう ----------
-- 1. しばらく待って、左右の温度の差を見よう。冷蔵庫なしで温度差ができた！？
-- 2. 下のグラフで、気体のエントロピーの減少と、記憶を消すコストを比べよう。どちらが大きい？
-- 3. A で手動にして、自分で悪魔になろう（赤い分子が来たときだけマウスを押す）。うまく分けられる？
-- 4. ↑↓ でしきい値を変えると、温度差のでき方は？ 測る回数（ビット）は？
-- 5. 情報を消すとなぜ熱が出るのか、シリーズ 6（通信理論）のエントロピーとくらべて考えよう

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
  , gsBig :: !(U.Vector Int)                              -- 大きな粒子（セルリストに入れない）
  , gsDemon :: !Demon }                                   -- 仕切りと悪魔

-- 仕切り（x = W/2）の扉の前の悪魔
data Demon = Demon
  { dmOn, dmAuto, dmManual, dmFree :: !Bool               -- 悪魔がいるか・自動か・手動で開けているか・扉が開けっぱなしか
  , dmThr :: !Double                                      -- 速さのしきい値
  , dmBits, dmPassed :: !Int                              -- 測った回数（= 記憶したビット数）、通した数
  , dmSide :: !(U.Vector Int) }                           -- それぞれの分子が左（0）か右（1）か

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
            (U.replicate n r) (U.replicate n m) 0 0 0 0 1 1 1 U.empty (Demon False False False False 0 0 0 (U.replicate n 0))

-- マス目の大きさ = ふつうの粒子の直径の最大（大きな粒子は別あつかい）
gasCells :: Gas -> Gas
gasCells g = g { gsCS = cs, gsNX = max 1 (floorI (gsW g / cs)), gsNY = max 1 (floorI (gsH g / cs)), gsBig = big }
  where
    r0 = if gsN g > 0 then gsR g U.! 0 else 1
    big = U.findIndices (> 2 * r0) (gsR g)
    rs = U.foldl' max 0 (U.filter (<= 2 * r0) (gsR g))
    cs = 2 * rs + 1e-9

-- 動かす間だけ書きかえられる配列にする
data MGas s = MGas { mX, mY, mVX, mVY :: !(UM.MVector s Double), mSide :: !(UM.MVector s Int) }

withGas :: Gas -> (forall s. MGas s -> ST s Gas) -> Gas
withGas g act = runST $ do
  m <- MGas <$> U.thaw (gsX g) <*> U.thaw (gsY g) <*> U.thaw (gsVX g) <*> U.thaw (gsVY g) <*> U.thaw (dmSide (gsDemon g))
  g' <- act m
  x <- U.freeze (mX m); y <- U.freeze (mY m); vx <- U.freeze (mVX m); vy <- U.freeze (mVY m); sd <- U.freeze (mSide m)
  pure g' { gsX = x, gsY = y, gsVX = vx, gsVY = vy, gsDemon = (gsDemon g') { dmSide = sd } }

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

-- 仕切り（x = W/2）を横切ろうとしたら、扉の所だけ悪魔が判断する。戻り値は (測った回数, 通した数) の増え方
barrierM :: Gas -> MGas s -> Int -> Double -> ST s (Int, Int)
barrierM g v i xo = do
  let mid = gsW g / 2; dm = gsDemon g
  x <- rd (mX v) i; y <- rd (mY v) i
  if (xo < mid) == (x < mid) then pure (0, 0) else do
    let inDoor = abs (y - gsH g / 2) < door / 2
    (bit, open) <- if inDoor && dmOn dm
      then do vx <- rd (mVX v) i; vy <- rd (mVY v) i
              let fast = hypot vx vy > dmThr dm                -- 1 回測った = 1 ビット記憶
              pure (1, if dmAuto dm then (if xo < mid then fast else not fast) else dmManual dm)
      else pure (0, inDoor && dmFree dm)
    if open then do UM.unsafeWrite (mSide v) i (if x < mid then 0 else 1)
                    pure (bit, 1)
            else do wr (mX v) i (2 * mid - x)
                    vx <- rd (mVX v) i
                    wr (mVX v) i (-vx)
                    pure (bit, 0)

-- 衝突の押しもどしで仕切りをすりぬけた分子は、元の側へもどす
afterM :: Gas -> MGas s -> ST s ()
afterM g v = forM_ [0 .. gsN g - 1] $ \i -> do
  x <- rd (mX v) i
  sd <- UM.unsafeRead (mSide v) i
  when ((if x < gsW g / 2 then 0 else 1) /= sd) (wr (mX v) i (gsW g - x))

-- 1 回分（時間 dt）進める
stepM :: Gas -> MGas s -> Double -> ST s Gas
stepM g0 v dt = do
  let g = g0 { gsWallX = clampv (gsWallMin g0) (gsW g0) (gsWallX g0 + gsWallU g0 * dt) }
      go !i !imp !impX !bits !passed
        | i >= gsN g = pure (imp, impX, bits, passed)
        | otherwise = do
            x <- rd (mX v) i; y <- rd (mY v) i; vx <- rd (mVX v) i; vy <- rd (mVY v) i
            wr (mX v) i (x + vx * dt); wr (mY v) i (y + vy * dt)
            (hit, ix) <- wallsM g v i
            (b, p) <- barrierM g v i x
            let imp' = if hit > 0 then imp + U.unsafeIndex (gsM g) i * hit else imp
            go (i + 1) imp' (impX + ix) (bits + b) (passed + p)
      dm = gsDemon g
  (imp, impX, bits, passed) <- go 0 (gsImp g) (gsImpX g) (dmBits dm) (dmPassed dm)
  collideM g v
  afterM g v
  pure g { gsImp = imp, gsImpX = impX, gsTime = gsTime g + dt, gsMeterT = gsMeterT g + dt
         , gsDemon = dm { dmBits = bits, dmPassed = passed } }

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
