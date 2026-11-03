-- ============================================
-- 02 波束と不確定性
-- 平面波（01）をたくさん重ねると「かたまり」= 波束になる。位置の広がり Δx と運動量の広がり Δp は
--   フーリエ変換で結ばれていて、Δx · Δp ≥ ħ/2（ħ = 1 の単位なので 0.5）。ガウス型のときだけ等号
-- ←→ = 幅 σ / ↑↓ = 平均の運動量 k0 / スペース = 時間を進める（自由粒子は広がっていく） / G = ガウス ⇔ 四角 / R = 時刻を 0 に
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.&.))
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Numeric (showFFloat)

nN :: Int
nN = 512                  -- ★ 格子点の数（2 のべき）

lL :: Float
lL = 200                  -- 計算する x の範囲の長さ（両端はつながっている）

dx :: Float
dx = lL / fromIntegral nN

type Field = U.Vector Float

data World = World
  { wSigma :: !Float      -- ★ 位置の広がり σ
  , wK0    :: !Float      -- ★ 平均の波数（ħ = 1 なので運動量 p = k）
  , wGauss :: !Bool
  , wT     :: !Float
  , wRun   :: !Bool
  , wPhiR  :: !Field      -- t = 0 の運動量空間の波動関数 φ(k)
  , wPhiI  :: !Field
  , wRe    :: !Field      -- 時刻 t の位置の波動関数 ψ(x)
  , wIm    :: !Field
  }

main :: IO ()
main = runSketch (sketch "25-02 Wave Packet" (makePacket (World 6 1.5 True 0 False U.empty U.empty U.empty U.empty)))
  { skBack  = hsb 260 30 10
  , skStep  = \_ _ w -> pure (evolve (if wRun w then w { wT = wT w + 0.25 } else w))
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

ks :: Field
ks = U.generate nN (\j -> 2 * pi / lL * fromIntegral (if j < nN `div` 2 then j else j - nN))

xAt :: Int -> Float
xAt j = -lL / 2 + fromIntegral j * dx

-- 波束をつくって、FFT で運動量の分布 φ(k) を求めておく
makePacket :: World -> World
makePacket w =
  let x0 = -40
      s0 = wSigma w
      k0 = wK0 w
      amp x | wGauss w = exp (-(x - x0) * (x - x0) / (4 * s0 * s0))
            | abs (x - x0) < s0 * sqrt 3 = 1      -- 四角も Δx = σ になる幅
            | otherwise = 0
      as = U.generate nN (amp . xAt)
      nrm = 1 / sqrt (U.sum (U.map (\a -> a * a * dx) as))   -- ∫|ψ|² dx = 1 に正規化
      re = U.imap (\j a -> nrm * a * cos (k0 * xAt j)) as
      im = U.imap (\j a -> nrm * a * sin (k0 * xAt j)) as
      (pr, pi') = fftPure False re im
  in evolve w { wPhiR = pr, wPhiI = pi', wT = 0 }

-- ★ 自由粒子の時間発展：運動量空間では各成分の位相が e^{-i k² t / 2} で回るだけ
evolve :: World -> World
evolve w =
  let t = wT w
      th j = let k = ks `U.unsafeIndex` j in -(k * k * t) / 2
      pr = wPhiR w
      pi' = wPhiI w
      re = U.generate nN (\j -> let a = th j in pr `U.unsafeIndex` j * cos a - pi' `U.unsafeIndex` j * sin a)
      im = U.generate nN (\j -> let a = th j in pr `U.unsafeIndex` j * sin a + pi' `U.unsafeIndex` j * cos a)
      (r, i) = fftPure True re im     -- 逆変換で位置の波動関数へ
  in w { wRe = r, wIm = i }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      re = wRe w
      im = wIm w
      probs = U.zipWith (\a b -> a * a + b * b) re im
      -- 位置の期待値と広がり
      mx = U.sum (U.imap (\j p -> xAt j * p * dx) probs)
      mx2 = U.sum (U.imap (\j p -> xAt j * xAt j * p * dx) probs)
      pmax = U.maximum probs
      dX = sqrt (max 0 (mx2 - mx * mx))
      -- 運動量の期待値と広がり（|φ|² で重みづけ）
      phiP = U.zipWith (\a b -> a * a + b * b) (wPhiR w) (wPhiI w)
      sk = U.sum phiP
      mk = U.sum (U.zipWith (*) ks phiP) / sk
      mk2 = U.sum (U.zipWith (\k p -> k * k * p) ks phiP) / sk
      kmax = U.maximum phiP
      dP = sqrt (max 0 (mk2 - mk * mk))
      left = 60
      wd = width - 120
      fN = fromIntegral nN :: Float
      -- 位置の分布 |ψ(x)|²（色 = 位相）
      y1 = height * 0.48
      h1 = height * 0.3
      bars = [ seg env (wd / fN + 0.6) (hsb ((atan2 (im U.! j) (re U.! j) + pi) / tau * 360) 80 100)
                   (x, y1) (x, y1 - p / max pmax 1e-9 * h1)
             | j <- [0 .. nN - 1]
             , let p = probs U.! j
             , p / max pmax 1e-9 * h1 > 0.3
             , let x = left + fromIntegral j / fN * wd ]
      xm = left + (mx + lL / 2) / lL * wd
      xd = dX / lL * wd
      -- 運動量の分布 |φ(k)|²
      y2 = height * 0.92
      h2 = height * 0.25
      kShow = 4
      kCurve = [ (left + (k + kShow) / (2 * kShow) * wd, y2 - p / kmax * h2)
               | j <- [0 .. nN - 1]
               , let jj = (j + nN `div` 2) `mod` nN      -- fftshift：負の k を左に並べ直す
               , let k = ks U.! jj
               , abs k <= kShow
               , let p = phiP U.! jj ]
      km = left + (mk + kShow) / (2 * kShow) * wd
      kd = dP / (2 * kShow) * wd
      -- 数値
      prod' = dX * dP
      s0 = wSigma w
      t = wT w
      px = width - 420
      py = 110
  in Pictures $
       [ txt env left (y1 - h1 - 10) 16 white "Position |psi(x)|^2 (color = phase)" ]
       ++ bars
       ++ [ seg env 1 (hsb 0 0 50) (left, y1) (left + wd, y1)
          , seg env 3 (hsb 55 90 100) (xm - xd, y1 + 14) (xm + xd, y1 + 14)   -- Δx の矢印
          , txt env left (y2 - h2 - 10) 16 white ("Momentum |phi(k)|^2 (from FFT)   k = -" ++ nf 0 kShow ++ " to " ++ nf 0 kShow)
          , curve env 2 (hsb 190 80 100) kCurve
          , seg env 1 (hsb 0 0 50) (left, y2) (left + wd, y2)
          , seg env 1 (hsb 0 0 50) (left + wd / 2, y2) (left + wd / 2, y2 + 6)
          , seg env 3 (hsb 55 90 100) (km - kd, y2 + 14) (km + kd, y2 + 14)
          , txt env 20 26 16 white ("Left/Right: sigma = " ++ nf 1 s0 ++ "   Up/Down: k0 = " ++ nf 2 (wK0 w) ++ "   G: " ++ (if wGauss w then "Gaussian" else "square"))
          , txt env 20 48 16 white ("Space: time " ++ (if wRun w then "running" else "paused") ++ "   R: t = 0   t = " ++ nf 1 t)
          , txt env 20 70 14 (gray 0.6) "A narrower packet needs a wider spread of momenta. In time fast and slow parts separate, so it spreads."
          , box env (hsba 0 0 100 0.08) (px - 16) (py - 28) 410 130
          , txt env px py 16 (hsb 55 90 100) ("dx = " ++ nf 2 dX ++ "   (theory " ++ nf 2 (s0 * sqrt (1 + sq (t / (2 * s0 * s0)))) ++ ")")
          , txt env px (py + 28) 16 (hsb 55 90 100) ("dp = " ++ nf 3 dP ++ "   (1/(2 sigma) = " ++ nf 3 (1 / (2 * s0)) ++ ")")
          , txt env px (py + 62) 20 (if prod' < 0.52 then hsb 120 70 100 else white) ("dx * dp = " ++ nf 3 prod' ++ "  >= 0.5")
          , txt env px (py + 88) 14 (gray 0.6) "hbar = m = 1. Free particle: dp is constant"
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    'g' -> makePacket w { wGauss = not (wGauss w) }
    'r' -> w { wT = 0 }
    _ -> w
  EventKey (SpecialKey KeyLeft) Down _ _ -> makePacket w { wSigma = max 1 (wSigma w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> makePacket w { wSigma = min 20 (wSigma w + 0.5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> makePacket w { wK0 = min 3 (wK0 w + 0.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> makePacket w { wK0 = max (-3) (wK0 w - 0.25) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. σ を 1 と 20 にして、下のグラフの幅を比べよう。Δx · Δp はいつも 0.5 のまま？
-- 2. G で四角い波束に。Δx は同じなのに Δp は大きくなる → 角ばった形には高い波数が要る
-- 3. スペースで時間を進めると細い波束ほど早く広がる。広がりが 2 倍になる時刻 t = 2√3 σ² を確かめよう
-- 4. k0 を 0 にすると波束は止まったまま広がるだけ。k0 を変えると中心の速さ（= k0）がどう変わる？

-- ==================================================
-- ここから下は「道具」（複素数の FFT：高速フーリエ変換）
-- ==================================================
twC, twS :: Field
twC = U.generate (nN `div` 2) (\k -> cos (2 * pi * fromIntegral k / fromIntegral nN))
twS = U.generate (nN `div` 2) (\k -> sin (2 * pi * fromIntegral k / fromIntegral nN))

-- re, im：長さ nN（2 のべき）の配列をその場で変換する。inv = True で逆変換（1/n で割る）
fft :: Bool -> MV.STVector s Float -> MV.STVector s Float -> ST s ()
fft inv re im = swaps 1 0 >> stage 2 >> when inv (divide 0)
  where
    n = nN
    next !j !bit | j .&. bit /= 0 = next (j `xor` bit) (bit `shiftR` 1)
                 | otherwise = j `xor` bit
    swaps !i !j                                     -- ビット反転の並べ替え
      | i >= n = pure ()
      | otherwise = do
          let j' = next j (n `shiftR` 1)
          when (i < j') (MV.unsafeSwap re i j' >> MV.unsafeSwap im i j')
          swaps (i + 1) j'
    stage !len                                      -- バタフライ演算
      | len > n = pure ()
      | otherwise = block len 0 >> stage (len * 2)
    block !len !i
      | i >= n = pure ()
      | otherwise = wing len i 0 >> block len (i + len)
    wing !len !i !k
      | k >= len `shiftR` 1 = pure ()
      | otherwise = do
          let a = i + k
              b = a + len `shiftR` 1
              t = k * (n `div` len)
              wr = twC `U.unsafeIndex` t
              wi = if inv then twS `U.unsafeIndex` t else negate (twS `U.unsafeIndex` t)
          rb <- MV.unsafeRead re b
          ib <- MV.unsafeRead im b
          ra <- MV.unsafeRead re a
          ia <- MV.unsafeRead im a
          let xr = rb * wr - ib * wi
              xi = rb * wi + ib * wr
          MV.unsafeWrite re b (ra - xr)
          MV.unsafeWrite im b (ia - xi)
          MV.unsafeWrite re a (ra + xr)
          MV.unsafeWrite im a (ia + xi)
          wing len i (k + 1)
    scaleBy = 1 / fromIntegral n :: Float
    divide !i
      | i >= n = pure ()
      | otherwise = MV.unsafeModify re (* scaleBy) i >> MV.unsafeModify im (* scaleBy) i >> divide (i + 1)

fftPure :: Bool -> Field -> Field -> (Field, Field)
fftPure inv re im = runST $ do
  r <- U.thaw re
  i <- U.thaw im
  fft inv r i
  (,) <$> U.unsafeFreeze r <*> U.unsafeFreeze i

sq :: Float -> Float
sq x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)
