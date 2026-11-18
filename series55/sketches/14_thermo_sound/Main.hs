-- ============================================
-- 55-14 熱の音 🔊 ─ 分子が壁に当たる音を聞く。音の数 = 衝突の頻度 = 圧力
-- 分子が壁に当たるたびに（一部を選んで）小さな「コツ」を鳴らす。速くぶつかるほど高い音
-- 温度を上げると音が細かく・高くなる。C でピストンを動かしカルノーサイクルを回すと、4 つの過程で持続音の音色が変わる
-- 2 次元の分子の気体は γ = 2 なので、断熱の過程では T × 面積 が一定
-- 操作：クリックで音 ON / ↑↓ 温度 / ← → 音の数（間引きの割合）/ C カルノーサイクル / M 消音 / R やり直し
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
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
nPart = 160                -- ★ 分子の数

rad :: Double
rad = 4

maxClick :: Int
maxClick = 4               -- ★ 1 コマに鳴らす音の上限

tH, tC :: Double
tH = 2                     -- ★ カルノーサイクルの高温・低温（kT）
tC = 1

master :: Double
master = 0.12              -- ★ 全体の音量（0.2 以下にしておく）

legs :: [String]
legs = ["isothermal expansion (Th)", "adiabatic expansion", "isothermal compression (Tc)", "adiabatic compression"]

waves :: [Wave]
waves = [Sine, Triangle, Sine, Square]

bx, by :: Float
bx = 20
by = 110

data World = World
  { wGas     :: !Gas
  , wRng     :: !Rng               -- 鳴らす衝突を選ぶ乱数
  , wClickP  :: !Double            -- 衝突を鳴らす割合
  , wTargetT :: !Double
  , wCarnot  :: !Bool
  , wLeg     :: !Int               -- カルノーサイクルのどの過程か（0〜3）
  , wBath    :: !Bool              -- いま熱浴につながっているか
  , wTBath   :: !Double
  , wMuted   :: !Bool
  , wAudio   :: !Bool              -- 音を始めたか（クリックかキーで ON）
  , wHitsNow :: !Int               -- このコマに壁に当たった回数
  , wLogPA   :: [(Double, Double, Double)]   -- (面積, 圧力, 温度)（新しい順、200 個まで）
  , wRate    :: !Double
  , wA1      :: !Double
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-14 Heat sound" world0)
  { skBack  = rgbC 20 20 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetGas env world0 { wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (fst (makeGas 1 10 10 rad 1 1 (seedRng 1))) (seedRng 14) 0.5 1 False 0 True 1 False False 0 [] 0 0 []

resetGas :: Env -> World -> World
resetGas env w =
  let (g0, r') = makeGas nPart (fromIntegral (floor (envW env * 0.5) :: Int)) (realToFrac (envH env) - 230) rad (wTargetT w) 1 (seedRng 14)
      g = g0 { gsWallMin = gsW g0 * 0.25, gsX = U.map (* 0.45) (gsX g0), gsWallX = gsW g0 * 0.45 }
  in w { wGas = g, wRng = r', wA1 = gsWallX g, wCarnot = False, wLeg = 0, wLogPA = [] }

-- 何個の衝突を鳴らすか：それぞれ確率 p で選び、上限 maxc（大きな音にならないように）
pickClicks :: [Double] -> Double -> Int -> Rng -> ([Double], Rng)
pickClicks hits p maxc r0 = let (out, r) = foldl' pick ([], r0) hits in (reverse out, r)
  where pick (acc, r) v = let (a, r') = rnd r in (if a < p && length acc < maxc then v : acc else acc, r')

-- カルノーサイクル（面積 A = ピストンの位置）：Th で A1 → A2、断熱で Tc まで（T·A 一定 → A3 = A2·Th/Tc）、Tc で A4 = A1·Th/Tc まで、断熱で A1 へ
carnotTargets :: Double -> Double -> Double -> Double -> (Double, Double, Double)
carnotTargets a1 a2 th tc = (a2, a2 * th / tc, a1 * th / tc)

step :: Env -> Float -> World -> IO World
step env _ w = do
  let g0 = (wGas w) { gsHits = [], gsHitN = 0 }
      -- ---- ピストンと熱浴（カルノーモード）
      x = gsWallX g0
      kT0 = gasTemp g0
      a2' = min (gsW g0 * 0.95 * tC / tH) (wA1 w * 1.6)
      (ta2, ta3, ta4) = carnotTargets (wA1 w) a2' tH tC
      (u, bath, tBath, leg)
        | not (wCarnot w) = (0, True, wTargetT w, wLeg w)
        | otherwise = case wLeg w of
            0 -> (0.25, True, tH, if x >= ta2 then 1 else 0)
            1 -> (0.25, False, wTargetT w, if kT0 <= tC || x >= ta3 || x >= gsW g0 - 1 then 2 else 1)
            2 -> (-0.25, True, tC, if x <= ta4 then 3 else 2)
            _ -> (-0.25, False, wTargetT w, if x <= wA1 w then 0 else 3)
      g1 = gasAdvance 2 g0 { gsWallU = u }
      g2 | bath = gasSetTemp (if wCarnot w then tBath else wTargetT w) g1
         | gasTemp g1 > 6 = gasSetTemp 6 g1
         | otherwise = g1
      hits = reverse (gsHits g2)
      rate = 0.95 * wRate w + 0.05 * fromIntegral (gsHitN g2)
      p = gasPressure g2
      (g3, logPA) | envFrame env `mod` 10 == 0 = (gasResetMeter g2, take 200 ((gsWallX g2 * gsH g2, p, gasTemp g2) : wLogPA w))
                  | otherwise = (g2, wLogPA w)
      sound = wAudio w && not (wMuted w)
      (clicks, r') = if sound then pickClicks hits (wClickP w) maxClick (wRng w) else ([], wRng w)
  -- ---- 音（コツは 2 コマに 1 回まで、持続音は 0.25 秒の音を 4 回/秒）
  when sound $ do
    forM_ (take 1 clicks) $ \v -> when (even (envFrame env)) (click (250 + 220 * v) (0.25 * min 1 (v / 2)))
    when (wCarnot w && envFrame env `mod` 15 == 0) (drone (80 + 70 * gasTemp g3) (getAt Sine waves leg))
  pure w { wGas = g3, wRng = r', wLeg = leg, wBath = bath, wTBath = tBath, wHitsNow = gsHitN g2, wRate = rate, wLogPA = logPA }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱
  [ Color (grayC 150) (Pictures [ thickLine 2 (bx, by) (bx + gw, by), thickLine 2 (bx, by + ght) (bx + gw, by + ght), thickLine 2 (bx, by) (bx, by + ght) ]) ]
  ++ [ Color (if wCarnot w && wTBath w == tC then rgbC 80 150 255 else rgbC 255 110 60) (rectTL (bx - 12) by 8 ght) | wBath w ]
  ++ [ Color (rgbC 255 160 90) (Pictures [ at (bx + f2 (gsX g U.! i), by + f2 (gsY g U.! i)) (disc (f2 rad)) | i <- [0 .. gsN g - 1] ])
     , Color (rgbC 180 185 200) (rectTL (bx + f2 (gsWallX g)) (by + 2) 12 (ght - 4)) ]
  -- 鳴った衝突の印
  ++ [ Color (rgbaC 255 255 255 120) (Pictures [ at (bx + gw + 30 + fromIntegral (k `mod` 10) * 10, by + 10 + fromIntegral (k `div` 10) * 10) (disc 3) | k <- [0 .. min 30 (wHitsNow w) - 1] ]) ]
  -- ---- PA 図（カルノー）と温度
  ++ [ plotAxes g1 "area A" "pressure P (measured)" [] [] ]
  ++ [ Color (lerpC (80, 150, 255) (255, 110, 60) (clampv 0 1 ((t - 0.8) / 1.4))) (at (gX g1 a, gY g1 p) (disc 3)) | (a, p, t) <- logPA ]
  ++ [ Color (rgbaC 255 255 255 50) (polyline 1 [ (gX g1 a, gY g1 (fromIntegral nPart * t / a)) | k <- [0 .. 59 :: Int], let a = x1 g1 * (0.2 + 0.8 * fromIntegral k / 59) ]) | t <- [tC, tH] ]
  ++ [ plotAxes g2 "time ->" "temperature kT" [] [0, 1, 2, 3]
     , Color (rgbC 255 200 100) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 t) | (k, (_, _, t)) <- zip [0 :: Int ..] logPA ]) ]
  -- ---- 文字
  ++ [ say ("Heat sound   kT = " ++ fmt 2 kT ++ "   wall hits " ++ fmt 0 (wRate w * 60) ++ " /s (playing " ++ show (floor (wClickP w * 100 + 0.5) :: Int) ++ "%)"
            ++ (if wCarnot w then "   Carnot: " ++ getAt "" legs (wLeg w) else "")) 20 36 18 (grayC 240)
     , say ((if wAudio w then (if wMuted w then "muted (M)" else "sound ON") else "click for sound") ++ "   Up/Down temperature   Left/Right number of clicks   C Carnot cycle "
            ++ (if wCarnot w then "ON" else "OFF") ++ "   M mute   R restart") 20 62 15 (grayC 175)
     , say "White = ideal-gas isotherms P = NkT/A (Tc and Th). Dot colour = temperature. Adiabatic: T*A constant (2D, gamma = 2)" 20 86 15 (grayC 175) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = wGas w
    gw = f2 (gsW g)
    ght = f2 (gsH g)
    kT = gasTemp g
    logPA = reverse (wLogPA w)
    gx = bx + gw + 150
    gwid = envW env - gx - 30
    gh = (envH env - 270) / 2
    g1 = graph gx by gwid gh 0 (gsW g * gsH g) 0 (fromIntegral nPart * tH / (gsW g * 0.25 * gsH g) * 1.3)
    g2 = graph gx (by + gh + 70) gwid gh 0 200 0 3

event :: Env -> Event -> World -> IO World
event env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w
  EventKey (SpecialKey KeyUp) Down _ _ | not (wCarnot w) -> pure w { wTargetT = min 4 (wTargetT w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ | not (wCarnot w) -> pure w { wTargetT = max 0.2 (wTargetT w / 1.25) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wClickP = min 1 (wClickP w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wClickP = max 0.1 (wClickP w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> let on = not (wCarnot w)
                           g = wGas w
                           a1 = gsW g * 0.3
                       in pure (if on then w { wCarnot = True, wLeg = 0, wA1 = a1, wGas = g { gsWallX = a1, gsX = U.map (\x -> min x (a1 - rad - 1)) (gsX g) } }
                                      else w { wCarnot = False, wLeg = 0 })
    | c `elem` "mM" -> pure w { wMuted = not (wMuted w) }
    | c `elem` "rR" -> pure (resetGas env w)
  _ -> pure w
  where
    -- キーかクリックで音を始める（startAudio）
    w = case ev of
          EventKey _ Down _ _ -> w0 { wAudio = True }
          _ -> w0

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("molecules hitting the walls: total energy constant over 800 steps", abs (gasKE a1 / gasKE a0 - 1) < 1e-9)
  , ("4x temperature -> 2x speed -> about 2x wall hits (" ++ fmt 2 ratio ++ ", +-15%)", abs (ratio / 2 - 1) < 0.15)
  , ("limit on clicks: even 100 hits play only " ++ show maxClick, length (fst (pickClicks (replicate 100 1) 1 maxClick r1)) == maxClick)
  , ("2D Carnot: T*A constant on adiabats -> both isotherms have the same area ratio (A2/A1 = A3/A4)"
    , let (b2, b3, b4) = carnotTargets 100 150 2 1 in near (b2 / 100) (b3 / b4) 1e-12 && near (2 * b2) (1 * b3) 1e-12)
  , ("volume limit: master <= 0.2", master <= 0.2) ]
  where
    (a0, r1) = makeGas 100 300 300 4 1 1 (seedRng 140)
    a1 = gasSteps 800 0.5 a0
    h1 = gsHitN a1
    a2 = gasSteps 800 0.5 (gasSetTemp (4 * gasTemp a1) a1 { gsHitN = 0 })
    ratio = fromIntegral (gsHitN a2) / fromIntegral (max 1 h1)

-- ---------- やってみよう ----------
-- 1. ↑ で温度を上げよう。音の細かさ（回数）と高さはどう変わる？ 回数は √T に比例するはず
-- 2. C でカルノーサイクルを回し、4 つの過程の音色のちがいを聞こう。断熱のときは温度（音の高さ）が変わる
-- 3. PA 図の点は、Th と Tc の等温線の間を 1 周する？
-- 4. nPart を 2 倍にすると、衝突の回数（= 圧力）は何倍？
-- 5. 音の高さの式 250 + 220·v を変えて、自分の「気体の音」を作ろう

-- ---- 音の道具（Art.Sound の playTone で鳴らす）。音量の上限：全体の音量 master は 0.2 以下
-- 短い「コツ」
click :: Double -> Double -> IO ()
click freq vol = playTone (realToFrac (clampv 100 3000 freq)) (realToFrac (min 0.2 master * clampv 0 1 vol * 0.5)) 0.03 Sine

-- カルノーサイクル中の持続音（過程ごとに音色）。続く音の代わりに短い音をくり返す
drone :: Double -> Wave -> IO ()
drone freq wave = playTone (realToFrac (clampv 50 800 freq)) (realToFrac (min 0.2 master * 0.12)) 0.25 wave

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

-- ---- 剛体円板の気体（2 次元、換算単位 k = 1）。粒子どうしは弾性衝突、壁ではね返る
-- 近くのペアだけ調べる「セルリスト」：箱をマス目に分け、同じマスととなりのマスの粒子とだけ比べる
data Gas = Gas
  { gsN :: !Int, gsW, gsH :: !Double
  , gsWallX, gsWallU, gsWallMin :: !Double                -- ピストン（右の壁）の位置・速さ・いちばん左
  , gsX, gsY, gsVX, gsVY, gsR, gsM :: !(U.Vector Double)
  , gsImp, gsImpX, gsMeterT, gsTime :: !Double           -- 壁が受けた力積（全部・ピストンだけ）、測った時間
  , gsCS :: !Double, gsNX, gsNY :: !Int                   -- マス目の大きさと数
  , gsBig :: !(U.Vector Int)                              -- 大きな粒子（セルリストに入れない）
  , gsHitN :: !Int, gsHits :: [Double] }                  -- 壁に当たった回数と、そのときの速さ（新しい順）

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
            (U.replicate n r) (U.replicate n m) 0 0 0 0 1 1 1 U.empty 0 []

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

-- 1 回分（時間 dt）進める。壁に当たるたびに、その速さ（hit/2）を記録する
stepM :: Gas -> MGas s -> Double -> ST s Gas
stepM g0 v dt = do
  let g = g0 { gsWallX = clampv (gsWallMin g0) (gsW g0) (gsWallX g0 + gsWallU g0 * dt) }
      go !i !imp !impX !hn hs
        | i >= gsN g = pure (imp, impX, hn, hs)
        | otherwise = do
            x <- rd (mX v) i; y <- rd (mY v) i; vx <- rd (mVX v) i; vy <- rd (mVY v) i
            wr (mX v) i (x + vx * dt); wr (mY v) i (y + vy * dt)
            (hit, ix) <- wallsM g v i
            if hit > 0 then go (i + 1) (imp + U.unsafeIndex (gsM g) i * hit) (impX + ix) (hn + 1) (hit / 2 : hs)
                       else go (i + 1) imp (impX + ix) hn hs
  (imp, impX, hn, hs) <- go 0 (gsImp g) (gsImpX g) (gsHitN g) (gsHits g)
  collideM g v
  pure g { gsImp = imp, gsImpX = impX, gsTime = gsTime g + dt, gsMeterT = gsMeterT g + dt, gsHitN = hn, gsHits = hs }

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
