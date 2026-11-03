-- ============================================
-- 04 トンネル効果
-- エネルギー E が壁の高さ V0 より低くても、波束の一部は壁を「すり抜ける」
--   透過率（理論, E < V0）：T = 1 / (1 + V0² sinh²(κa) / (4E(V0 − E)))、κ = √(2(V0 − E))   （ħ = m = 1）
-- ↑↓ = 壁の高さ V0 / ←→ = 壁の幅 a / W/S = 波束のエネルギー（k0） / スペース = 撃ち直す / P = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.&.))
import Data.Char (toLower)
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Numeric (showFFloat)

nN :: Int
nN = 512              -- ★ 格子点の数

lL :: Float
lL = 240              -- x の範囲 −120 〜 120

dt :: Float
dt = 0.05

subSteps :: Int
subSteps = 8          -- ★ 1 フレームに進める歩数

dx :: Float
dx = lL / fromIntegral nN

sigma :: Float
sigma = 12            -- 波束の幅（大きいほどエネルギーがそろう）

histMax :: Int
histMax = 400

type Field = U.Vector Float

data World = World
  { wV0    :: !Float        -- ★ 壁の高さ
  , wCells :: !Int          -- ★ 壁の幅（格子のマス数）。幅 a = マス数 × dx
  , wK0    :: !Float        -- ★ 波数（E = k0²/2）
  , wRe    :: !Field
  , wIm    :: !Field
  , wVc    :: !Field
  , wVs    :: !Field
  , wLostL :: !Float        -- 端で吸い込んだ確率（左 = 反射、右 = 透過 として数える）
  , wLostR :: !Float
  , wT     :: !Float
  , wPause :: !Bool
  , wHist  :: ![(Float, Float)]   -- R と T の時間変化（新しいものが先頭）
  , wHistN :: !Int
  , wPL    :: !Float        -- 壁の左にある確率
  , wPR    :: !Float        -- 壁の右にある確率
  , wPmax  :: !Float
  }

main :: IO ()
main = runSketch (sketch "25-04 Tunneling" world0)
  { skBack  = hsb 280 25 10
  , skStep  = \env _ w -> pure (stepW env w)
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = shoot (World 1.3 4 1.5 U.empty U.empty U.empty U.empty 0 0 0 False [] 0 0 0 0)

ks, kc, kS, mask :: Field
ks = U.generate nN (\j -> 2 * pi / lL * fromIntegral (if j < nN `div` 2 then j else j - nN))
kc = U.map (\k -> cos (-(k * k * dt) / 2)) ks
kS = U.map (\k -> sin (-(k * k * dt) / 2)) ks
mask = U.generate nN $ \j ->
  let d = min j (nN - 1 - j)
      wd = 50 :: Int
  in if d < wd then 1 - 0.04 * sq (fromIntegral (wd - d) / fromIntegral wd) else 1

xAt :: Int -> Float
xAt j = -lL / 2 + fromIntegral j * dx

barrierW :: World -> Float
barrierW w = fromIntegral (wCells w) * dx

-- 壁が始まるマス
barrierJ0 :: World -> Int
barrierJ0 w = nN `div` 2 - wCells w `div` 2

-- 壁を置き直して波束を撃つ
shoot :: World -> World
shoot w =
  let j0 = barrierJ0 w
      -- 真ん中の wCells マスだけ V0（幅 a = マス数 × dx。理論の式もこの a を使う）
      vs = U.generate nN (\j -> if j >= j0 && j < j0 + wCells w then wV0 w else 0)
      x0 = -55
      as = U.generate nN (\j -> exp (-sq (xAt j - x0) / (4 * sigma * sigma)))
      s = U.sum (U.map (\a -> a * a * dx) as)
      nrm = 1 / sqrt s
  in w { wVc = U.map (\a -> cos (-a * dt / 2)) vs
       , wVs = U.map (\a -> sin (-a * dt / 2)) vs
       , wRe = U.imap (\j a -> nrm * a * cos (wK0 w * xAt j)) as
       , wIm = U.imap (\j a -> nrm * a * sin (wK0 w * xAt j)) as
       , wLostL = 0, wLostR = 0, wT = 0, wHist = [], wHistN = 0 }

-- ★ 長方形の壁の透過率（解析解）
transmission :: World -> Float -> Float
transmission w e
  | e <= 0 = 0
  | abs (e - v0) < 1e-6 = 1 / (1 + a * a * v0 / 2)
  | e < v0 =
      let kap = sqrt (2 * (v0 - e))
          sh = sinh (min (kap * a) 30)
      in 1 / (1 + v0 * v0 * sh * sh / (4 * e * (v0 - e)))
  | otherwise =
      let kk = sqrt (2 * (e - v0))
          sn = sin (kk * a)
      in 1 / (1 + v0 * v0 * sn * sn / (4 * e * (e - v0)))
  where v0 = wV0 w
        a = barrierW w

-- 波束がもつエネルギーの分布で T(E) を平均（運動量の分布はガウス、幅 1/(2σ)）
tAverage :: World -> Float
tAverage w =
  let sk = 1 / (2 * sigma)
      qs = [ fromIntegral i * 0.1 | i <- [-30 .. 30 :: Int] ]
      wgt q = exp (-q * q / 2)
      val q = let k = wK0 w + q * sk in wgt q * transmission w (k * k / 2)
  in sum (map val qs) / sum (map wgt qs)

-- 分割演算子法を subSteps 歩進める。吸い込んだ分を左右別に記録してから減らす
advance :: World -> World
advance w = runST $ do
  re <- U.thaw (wRe w)
  im <- U.thaw (wIm w)
  lostL <- newSTRef (wLostL w)
  lostR <- newSTRef (wLostR w)
  replicateM_ subSteps $ do
    rotate2 re im (wVc w) (wVs w)
    fft False re im
    rotate2 re im kc kS
    fft True re im
    rotate2 re im (wVc w) (wVs w)
    absorb re im lostL lostR
  r <- U.unsafeFreeze re
  i <- U.unsafeFreeze im
  l <- readSTRef lostL
  q <- readSTRef lostR
  pure w { wRe = r, wIm = i, wLostL = l, wLostR = q, wT = wT w + fromIntegral subSteps * dt }

absorb :: MV.STVector s Float -> MV.STVector s Float -> STRef s Float -> STRef s Float -> ST s ()
absorb re im lostL lostR = go 0
  where
    go !j
      | j >= nN = pure ()
      | otherwise = do
          let m = mask `U.unsafeIndex` j
          when (m < 1) $ do
            a <- MV.unsafeRead re j
            b <- MV.unsafeRead im j
            let p = (a * a + b * b) * (1 - m * m) * dx
            modifySTRef' (if j < nN `div` 2 then lostL else lostR) (+ p)
            MV.unsafeWrite re j (a * m)
            MV.unsafeWrite im j (b * m)
          go (j + 1)

stepW :: Env -> World -> World
stepW env w0 =
  let w = if wPause w0 then w0 else advance w0
      probs = U.zipWith (\a b -> a * a + b * b) (wRe w) (wIm w)
      -- 壁の左（反射）・右（透過）にある確率
      half = nN `div` 2       -- xAt half = 0
      pL = wLostL w + U.sum (U.take half probs) * dx
      pR = wLostR w + U.sum (U.drop half probs) * dx
      record = envFrame env `mod` 3 == 0 && wHistN w < histMax
  in w { wPL = pL, wPR = pR, wPmax = U.maximum probs
       , wHist = if record then (pL, pR) : wHist w else wHist w
       , wHistN = if record then wHistN w + 1 else wHistN w }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      e0 = wK0 w * wK0 w / 2
      v0 = wV0 w
      tAvg = tAverage w
      -- 波束と壁
      left = 40
      wd = width - 80
      base = height * 0.6
      eS = height * 0.18
      fN = fromIntegral nN :: Float
      px j = left + fromIntegral j / (fN - 1) * wd
      j0 = fromIntegral (barrierJ0 w) :: Float
      bx1 = left + (j0 - 0.5) / (fN - 1) * wd
      bx2 = left + (j0 + fromIntegral (wCells w) - 0.5) / (fN - 1) * wd
      hs = height * 0.32 / max (wPmax w) 0.01
      bars = [ seg env (wd / fN + 0.8) (hsb ((atan2 b a + pi) / tau * 360) 75 100) (px j, base) (px j, base - p * hs)
             | j <- [0 .. nN - 1]
             , let a = wRe w U.! j
             , let b = wIm w U.! j
             , let p = a * a + b * b
             , p * hs >= 0.4 ]
      -- R と T のバーと時間変化
      gx = left
      gy = height * 0.95
      gh = height * 0.25
      gw = wd * 0.5
      histAsc = reverse (wHist w)
      histX i = gx + fromIntegral i / fromIntegral histMax * gw
      rCurve = curve env 2 (hsb 200 80 100) [ (histX i, gy - r * gh) | (i, (r, _)) <- zip [0 :: Int ..] histAsc ]
      tCurve = curve env 2 (hsb 30 80 100) [ (histX i, gy - t * gh) | (i, (_, t)) <- zip [0 :: Int ..] histAsc ]
      tx = gx + gw + 30
      ty = gy - gh
      -- T(E) の小さなグラフ
      cx = width - 300
      cy = 150
      cw = 250
      ch = 120
      emax = 3
      teCurve = [ (cx + e / emax * cw, cy + ch - transmission w e * ch)
                | i <- [0 .. 100 :: Int], let e = fromIntegral i / 100 * emax ]
  in Pictures $
       [ box env (hsb 0 0 35) bx1 (base - v0 * eS) (max 2 (bx2 - bx1)) (v0 * eS)
       , seg env 1 (hsba 55 80 100 0.7) (left, base - e0 * eS) (left + wd, base - e0 * eS) ]
       ++ bars
       ++ [ seg env 1 (hsb 0 0 50) (left, base) (left + wd, base)
          , frameBox env (hsb 0 0 35) gx (gy - gh) gw gh
          , rCurve, tCurve
          , seg env 1 (hsba 30 80 100 0.5) (gx, gy - tAvg * gh) (gx + gw, gy - tAvg * gh)
          , txt env tx (ty + 20) 16 (hsb 200 80 100) ("Reflected R = " ++ nf 3 (wPL w))
          , txt env tx (ty + 46) 16 (hsb 30 80 100) ("Transmitted T = " ++ nf 3 (wPR w))
          , txt env tx (ty + 82) 16 white ("Theory T at E0 = " ++ nf 3 (transmission w e0))
          , txt env tx (ty + 108) 16 white ("Theory T, packet average = " ++ nf 3 tAvg)
          , txt env tx (ty + 134) 14 (gray 0.6) "Thin line = average theory. T nears it after the pass."
          , txt env tx (ty + 158) 14 (gray 0.6) ("R + T = " ++ nf 3 (wPL w + wPR w) ++ " (conserved). Coarse grid: a few % off.")
          , frameBox env (hsb 0 0 35) cx cy cw ch
          , curve env 2 (hsb 30 80 100) teCurve
          , seg env 1 (hsb 0 0 70) (cx + v0 / emax * cw, cy) (cx + v0 / emax * cw, cy + ch)
          , seg env 1 (hsb 55 90 100) (cx + min emax e0 / emax * cw, cy) (cx + min emax e0 / emax * cw, cy + ch)
          , txt env cx (cy - 8) 14 (gray 0.7) "T(E)  white = V0  yellow = E0"
          , txt env cx (cy + ch + 18) 14 (gray 0.7) "E = 0"
          , txt env (cx + cw - 10) (cy + ch + 18) 14 (gray 0.7) "3"
          , txt env 20 26 16 white ("Up/Down: V0 = " ++ nf 2 v0 ++ "   Left/Right: width a = " ++ nf 2 (barrierW w) ++ "   W/S: E0 = k0^2/2 = " ++ nf 3 e0)
          , txt env 20 48 16 white ("Space: shoot again   P: " ++ (if wPause w then "paused" else "running") ++ "   t = " ++ nf 1 (wT w))
          , txt env 20 70 14 (gray 0.6) (if e0 < v0
              then "Classically E0 < V0 means all bounces back. The wave decays inside the wall and leaks through."
              else "E0 > V0, yet part bounces back (also not classical). Some widths give a resonance with T = 1.")
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    'w' -> shoot w { wK0 = min 2.4 (wK0 w + 0.05) }
    's' -> shoot w { wK0 = max 0.5 (wK0 w - 0.05) }
    'p' -> w { wPause = not (wPause w) }
    _ -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> shoot w { wV0 = min 4 (wV0 w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> shoot w { wV0 = max 0.1 (wV0 w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> shoot w { wCells = min 20 (wCells w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> shoot w { wCells = max 1 (wCells w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> shoot w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 幅 a を 1 マスずつ増やすと T はどれだけ速く減る？（ほぼ e^{−2κa} に比例）
-- 2. V0 を E0 より少し低くして、T が 1 にならないことを確かめる。幅を変えて T ≈ 1 になる共鳴を探そう
-- 3. sigma を 4 にすると波束のエネルギーの幅が広がり、「E0 での理論値」と合わなくなる。平均の理論値とは？
-- 4. 壁を 2 枚にして（shoot の vs の式を変える）、間で波が行き来する「共鳴トンネル」を作ってみよう

-- ==================================================
-- ここから下は「道具」（複素数の FFT：高速フーリエ変換）
-- ==================================================
-- 複素数の配列に位相 (c + is) をかける
rotate2 :: MV.STVector s Float -> MV.STVector s Float -> Field -> Field -> ST s ()
rotate2 re im c s = go 0
  where
    go !j
      | j >= nN = pure ()
      | otherwise = do
          a <- MV.unsafeRead re j
          b <- MV.unsafeRead im j
          let cj = c `U.unsafeIndex` j
              sj = s `U.unsafeIndex` j
          MV.unsafeWrite re j (a * cj - b * sj)
          MV.unsafeWrite im j (a * sj + b * cj)
          go (j + 1)

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

-- 枠線だけの長方形（x, y は左上）
frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)
