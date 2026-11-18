-- ============================================
-- 55-13 ブラウン運動と拡散 ─ 大きな粒子が、見えない小さな分子に押されてふらつく
-- 左：大きな円板（花粉の粒のつもり）と、たくさんの小さな分子の剛体円板シミュレーション。大きな粒子の道すじを記録
-- 右：ランダムウォーク（毎回ランダムな向きに 1 歩）400 人。平均二乗変位 MSD = ⟨r²⟩ は時間に比例：MSD = 2dDt（d = 2 次元）
-- 直線を当てはめて拡散係数 D を求める。大きな粒子の MSD も、短い時間は t²（まっすぐ進む）、長い時間は t（拡散）
-- 操作：↑↓ 温度 / M 大きな粒子の重さ / T 道すじ表示 / スペース 一時停止 / R やり直し
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

nSmall :: Int
nSmall = 420               -- ★ 小さな分子の数

rSmall, rBig :: Double
rSmall = 3                 -- ★ 半径
rBig = 16

masses :: [Double]
masses = [5, 25, 100]      -- ★ 大きな粒子の質量（小さな分子 = 1）

walkSigma :: Double
walkSigma = 1.5            -- ★ ランダムウォークの 1 歩の大きさ（各方向の標準偏差 px）

nWalk :: Int
nWalk = 400

bx, by :: Float
bx = 20
by = 110

data World = World
  { wGas     :: !Gas
  , wRng     :: !Rng               -- ランダムウォークに使う乱数（毎コマ続けて使う）
  , wMassIdx :: !Int
  , wPaused  :: !Bool
  , wTrail   :: !Bool
  , wPathX   :: !(U.Vector Double) -- 大きな粒子の道すじ（古い順、1500 個まで）
  , wPathY   :: !(U.Vector Double)
  , wWalk    :: !(U.Vector Double) -- ランダムウォークの位置 (x0, y0, x1, y1, ...)
  , wWalkT   :: !Int
  , wMsd     :: [Double]           -- ランダムウォークの MSD（新しい順）
  , wKT      :: !Double            -- ★ 温度
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-13 Brownian motion and diffusion" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetAll env world0 { wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeGas 1 10 10 rSmall 1 1 (seedRng 1))) (seedRng 13) 1 False True U.empty U.empty U.empty 0 [] 1 []

massOf :: World -> Double
massOf w = getAt 25 masses (wMassIdx w)

resetAll :: Env -> World -> World
resetAll env w =
  let (g, r') = makeBrown nSmall (fromIntegral (floor (envW env * 0.42) :: Int)) (realToFrac (envH env) - 230) (massOf w) (wKT w) (seedRng 13)
  in w { wGas = g, wRng = r', wPathX = U.empty, wPathY = U.empty, wWalk = U.replicate (2 * nWalk) 0, wWalkT = 0, wMsd = [] }

-- 小さな分子 n 個と、真ん中の大きな粒子（番号 n）
makeBrown :: Int -> Double -> Double -> Double -> Double -> Rng -> (Gas, Rng)
makeBrown n w h bigM t r0 =
  let (g0, r1) = makeGas (n + 1) w h rSmall t 1 r0
      upd v x = v U.// [(n, x)]
      g1 = g0 { gsR = upd (gsR g0) rBig, gsM = upd (gsM g0) bigM, gsX = upd (gsX g0) (w / 2), gsY = upd (gsY g0) (h / 2)
              , gsVX = upd (gsVX g0) 0, gsVY = upd (gsVY g0) 0 }
      -- 大きな粒子に重なった分子はどける
      move (gg, r) i
        | hypot (gsX gg U.! i - w / 2) (gsY gg U.! i - h / 2) < rBig + rSmall + 1 =
            let (a, ra) = rnd r
                (b, rb) = rnd ra
            in (gg { gsX = gsX gg U.// [(i, 10 + a * 30)], gsY = gsY gg U.// [(i, 10 + b * (h - 20))] }, rb)
        | otherwise = (gg, r)
      (g2, r2) = foldl' move (g1, r1) [0 .. n - 1]
  in (gasCells g2, r2)

-- 平均二乗変位：道すじ（一定時間ごと）から、ずれ lag ごとの ⟨|r(t + lag) − r(t)|²⟩
msdOfPath :: U.Vector Double -> U.Vector Double -> Int -> Double
msdOfPath px py lag
  | c > 0 = s / fromIntegral c
  | otherwise = 0
  where
    c = max 0 (U.length px - lag)
    s = foldl' (\a k -> a + (px U.! (k + lag) - px U.! k) ^ (2 :: Int) + (py U.! (k + lag) - py U.! k) ^ (2 :: Int)) 0 [0 .. c - 1]

-- 最小二乗の直線 y = a + b x
fitLine :: [Double] -> [Double] -> Maybe (Double, Double)
fitLine xs ys
  | n < 2 || d == 0 = Nothing
  | otherwise = Just ((sy - b * sx) / fromIntegral n, b)
  where
    n = length xs
    (sx, sy, sxx, sxy) = foldl' (\(!a, !b', !c, !e) (x, y) -> (a + x, b' + y, c + x * x, e + x * y)) (0, 0, 0, 0) (zip xs ys)
    d = fromIntegral n * sxx - sx * sx
    b = (fromIntegral n * sxy - sx * sy) / d

-- 全員が 1 歩（各方向に標準偏差 sg の正規分布）
walkStep :: Double -> U.Vector Double -> Rng -> (U.Vector Double, Rng)
walkStep sg w r0 = let (z, r1) = rndNormals (U.length w) r0 in (U.zipWith (\a b -> a + sg * b) w z, r1)

walkMSD :: U.Vector Double -> Double
walkMSD w = foldl' (\s k -> s + (w U.! k) ^ (2 :: Int) + (w U.! (k + 1)) ^ (2 :: Int)) 0 [0, 2 .. U.length w - 2] / fromIntegral (U.length w `div` 2)

step :: Env -> Float -> World -> IO World
step _ _ w
  | wPaused w = pure w
  | otherwise =
      let g = gasAdvance 2 (wGas w)
          big = gsN g - 1
          keep v = if U.length v > 1500 then U.drop (U.length v - 1500) v else v
          (wk, r') = walkStep walkSigma (wWalk w) (wRng w)
          t = wWalkT w + 1
          msd = walkMSD wk : wMsd w
          w1 = w { wGas = g, wRng = r', wPathX = keep (U.snoc (wPathX w) (gsX g U.! big)), wPathY = keep (U.snoc (wPathY w) (gsY g U.! big))
                 , wWalk = wk, wWalkT = t, wMsd = msd }
      in pure (if t >= 300 then w1 { wWalk = U.map (const 0) wk, wWalkT = 0, wMsd = [] } else w1)

-- 大きな粒子の MSD を測るずれの時間（1, 2, 3, 4, 5, 7, ... と 1.25 倍ずつ）
lagsOf :: Int -> [Int]
lagsOf len = takeWhile (\l -> fromIntegral l < min 400 (fromIntegral len / 3 :: Double)) (iterate (\l -> ceiling (fromIntegral l * 1.25 :: Double)) 1)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 分子の箱
  [ Color (grayC 150) (frameTL 2 bx by gw ght)
  , Color (rgbaC 150 160 190 150) (Pictures [ at (bx + f2 (gsX g U.! i), by + f2 (gsY g U.! i)) (disc (f2 rSmall)) | i <- [0 .. big - 1] ]) ]
  ++ [ Color (rgbC 255 200 100) (polyline 1.5 [ (bx + f2 x, by + f2 y) | (x, y) <- zip (U.toList (wPathX w)) (U.toList (wPathY w)) ]) | wTrail w ]
  ++ [ Color (rgbC 255 140 70) (at (bx + f2 (gsX g U.! big), by + f2 (gsY g U.! big)) (disc (f2 rBig))) ]
  -- ---- ランダムウォーク
  ++ [ Color (grayC 70) (frameTL 1 wx by (ww * 0.5) wh)
     , Color (rgbaC 130 200 255 160) (Pictures [ at (x, y) (disc 1.5) | k <- [0, 2 .. U.length wk - 2]
                                                  , let x = cx + f2 (wk U.! k) * 0.6, let y = cy + f2 (wk U.! (k + 1)) * 0.6
                                                  , abs (x - cx) < ww * 0.25, abs (y - cy) < wh / 2 ])
     , Color (rgbaC 255 255 255 90) (at (cx, cy) (ring (f2 (sqrt (walkMSD wk)) * 0.6) 1)) ]
  -- MSD のグラフ（ランダムウォーク）
  ++ [ plotAxes gw' "steps t" "MSD <r^2> [px^2]" [0, 150, 300] [0, jsRoundD (y1 gw' / 2), jsRoundD (y1 gw')]
     , Color (rgbC 130 200 255) (polyline 2 [ (gX gw' (fromIntegral k), gY gw' v) | (k, v) <- zip [1 :: Int ..] msd ]) ]
  ++ [ Color (rgbC 255 230 120) (thickLine 1 (gX gw' 0, gY gw' a) (gX gw' 300, gY gw' (a + b * 300))) | Just (a, b) <- [fw] ]
  -- MSD のグラフ（大きな粒子、両対数）
  ++ [ plotAxes gb "lag tau (frames)" "MSD of the big particle [px^2] (log-log)" [1, 10, 100, 400] [0.1, 10, 1000, 1e5]
     , Color (rgbaC 255 255 255 70) (polyline 1 [ (gX gb t, gY gb (v2 * (2 * t) ^ (2 :: Int))) | t <- [1, 400] ])
     , Color (rgbC 255 160 80) (Pictures [ at (gX gb (fromIntegral l), gY gb m) (disc 3.5) | (l, m) <- zip lags ms, m > 0 ]) ]
  ++ [ Color (rgbC 255 230 120) (polyline 1.5 [ (gX gb t, gY gb (max 0.1 (b * t))) | t <- [10, 300] ]) | Just (_, b) <- [fb], b > 0 ]
  -- ---- 文字
  ++ [ say ("Brownian motion and diffusion   kT = " ++ fmt 2 (gasTemp g) ++ "   big particle mass " ++ num (massOf w) ++ " (" ++ num (massOf w) ++ " x a molecule), radius " ++ num rBig) 20 36 18 (grayC 240)
     , say "Up/Down temperature  M mass  T trail  Space pause  R restart | white = straight flight <v^2>t^2, yellow = diffusion 4D tau" 20 62 15 (grayC 175)
     , say ("Random walk: slope " ++ maybe "..." (fmt 3 . snd) fw ++ " -> D = slope/4 = " ++ maybe "..." (\(_, b) -> fmt 3 (b / 4)) fw
            ++ " (theory sigma^2/2 = " ++ fmt 3 (walkSigma ^ (2 :: Int) / 2) ++ ")   big particle: D ~ "
            ++ (case fb of Just (_, b) | b > 0 -> fmt 3 (b / 4 / 2) ++ " px^2/time"; _ -> "computing...")) 20 86 15 (rgbC 255 220 120) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = wGas w
    big = gsN g - 1
    gw = f2 (gsW g)
    ght = f2 (gsH g)
    wk = wWalk w
    wx = bx + gw + 50
    ww = envW env - wx - 30
    wh = (envH env - 270) / 2
    cx = wx + ww * 0.25
    cy = by + wh / 2
    gw' = graph (wx + ww * 0.5 + 60) by (ww * 0.5 - 70) wh 0 300 0 (4 * 0.5 * walkSigma ^ (2 :: Int) * 300 * 1.2)
    msd = reverse (wMsd w)
    fw = if length msd > 10 then fitLine (map fromIntegral [1 .. length msd]) msd else Nothing
    lags = lagsOf (U.length (wPathX w))
    ms = map (msdOfPath (wPathX w) (wPathY w)) lags
    gb = (graph (wx + 60) (by + wh + 80) (ww - 70) wh 1 400 0.1 1e5) { logX = True, logY = True }
    v2 = 2 * wKT w / massOf w          -- 2 次元：⟨v²⟩ = 2kT/M、1 コマ = 時間 2
    mids = [ (fromIntegral l, m) | (l, m) <- zip lags ms, l >= 20, l <= 120, m > 0 ]
    fb = if length mids >= 3 then fitLine (map fst mids) (map snd mids) else Nothing

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> let t = min 4 (wKT w * 1.4) in w { wKT = t, wGas = gasSetTemp t (wGas w) }
  EventKey (SpecialKey KeyDown) Down _ _ -> let t = max 0.1 (wKT w / 1.4) in w { wKT = t, wGas = gasSetTemp t (wGas w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> resetAll env w { wMassIdx = (wMassIdx w + 1) `mod` length masses }
    | c `elem` "tT" -> w { wTrail = not (wTrail w) }
    | c `elem` "rR" -> resetAll env w
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("molecule (mass 1) hits the big particle (25): energy and momentum conserved, the big one starts moving"
    , near (gasKE c1) e0 1e-12 && near (fst p1) (fst p0) 1e-12 && near (snd p1) (snd p0) 1e-12 && gsVX c1 U.! 1 > 0)
  , ("random walk 2000 x 200 steps (sigma = 1): MSD = 2dDt, D = sigma^2/2 = 0.5 (fit D = " ++ fmt 3 (fb / 4) ++ ", 5%)", abs (fb / 4 / 0.5 - 1) < 0.05)
  , ("least squares: y = 1 + 2x", case fitLine [1, 2, 3, 4] [3, 5, 7, 9] of Just (a, b) -> near a 1 1e-12 && near b 2 1e-12; Nothing -> False)
  , ("molecule MSD: t^2 at short times (ratio " ++ fmt 2 (m2 / m1) ++ " ~ 4), t at long times (ratio " ++ fmt 2 (m200 / m100) ++ " ~ 2)"
    , abs (m2 / m1 - 4) < 0.3 && abs (m200 / m100 - 2) < 0.35)
  , ("with collisions the long-time MSD = " ++ fmt 0 m200 ++ " is far below straight flight <v^2>t^2 = " ++ fmt 0 (v2 * 100 * 100), m200 < 0.3 * v2 * 100 * 100) ]
  where
    (ga, r1) = makeGas 2 100 100 3 1 1 (seedRng 130)
    c0 = gasCells ga { gsR = U.fromList [3, 16], gsM = U.fromList [1, 25], gsX = U.fromList [30, 48], gsY = U.fromList [50, 53]
                     , gsVX = U.fromList [2, 0], gsVY = U.fromList [0.5, 0] }
    e0 = gasKE c0
    p0 = gasMomentum c0
    c1 = gasCollideP c0
    p1 = gasMomentum c1
    walk (!wv, !r, acc) _ = let (wv', r') = walkStep 1 wv r in (wv', r', walkMSD wv' : acc)
    (_, r2, ys) = foldl' walk (U.replicate (2 * 2000) 0, r1, []) [1 .. 200 :: Int]
    fb = maybe 0 snd (fitLine (map fromIntegral [1 .. 200 :: Int]) (reverse ys))
    (q0, _) = makeGas 300 240 240 3 1 1 r2
    qa = gasSteps 200 0.5 q0
    n = fromIntegral (gsN qa)
    msdFrom q = U.sum (U.zipWith4 (\x y x0' y0' -> (x - x0') ^ (2 :: Int) + (y - y0') ^ (2 :: Int)) (gsX q) (gsY q) (gsX qa) (gsY qa)) / n
    v2 = 2 * gasKE qa / n
    q1 = gasStep 0.5 qa
    q2 = gasStep 0.5 q1
    q100 = gasSteps 98 0.5 q2
    q200 = gasSteps 100 0.5 q100
    (m1, m2, m100, m200) = (msdFrom q1, msdFrom q2, msdFrom q100, msdFrom q200)

-- ---------- やってみよう ----------
-- 1. 大きな粒子の道すじを見よう。分子は小さくて速いのに、大きな粒子はゆっくりふらつく
-- 2. 右下の両対数グラフ：短い τ では傾き 2（白い線に沿う）、長い τ では傾き 1 になる？
-- 3. M で重くすると D はどう変わる？ ↑ で温度を上げると？（アインシュタインの関係 D = kT/γ）
-- 4. ランダムウォークの walkSigma を 2 倍にすると D は何倍？
-- 5. インクが水にじわじわ広がるのも同じ。広がりの半径は √t に比例する

-- 正規分布の乱数を k 個（順番どおりに）
rndNormals :: Int -> Rng -> (U.Vector Double, Rng)
rndNormals k r0 = runST $ do
  mv <- UM.new k
  let go !i r | i >= k = pure r
              | otherwise = let (v, r') = rndNormal r in UM.unsafeWrite mv i v >> go (i + 1) r'
  r <- go 0 r0
  v <- U.unsafeFreeze mv
  pure (v, r)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- Math.round（.5 は上へ）
jsRoundD :: Double -> Double
jsRoundD v = fromIntegral (floor (v + 0.5) :: Int)

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

-- 衝突だけ 1 回調べる（自己テスト用）
gasCollideP :: Gas -> Gas
gasCollideP g = withGas g (\v -> collideM g v >> pure g)

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
