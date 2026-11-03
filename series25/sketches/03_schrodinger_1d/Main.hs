-- ============================================
-- 03 シュレーディンガー方程式（1 次元）
-- i ∂ψ/∂t = −(1/2) ∂²ψ/∂x² + V(x) ψ   （ħ = m = 1）を「分割演算子法」で解く
--   V の半歩（位置空間で位相を回す）→ FFT → 運動エネルギーの 1 歩（運動量空間で e^{−ik²dt/2}）→ 逆 FFT → V の半歩
-- ドラッグ = 波束を置いて投げる（向きと長さ = 運動量） / 1〜5 = ポテンシャル（自由・段差・壁・調和・二重井戸） / ↑↓ = V の強さ / ←→ = 幅 σ / スペース = 停止 / R = 投げ直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.&.))
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Numeric (showFFloat)

nN :: Int
nN = 512              -- ★ 格子点の数（2 のべき）

lL :: Float
lL = 200              -- x の範囲 −100 〜 100

dt :: Float
dt = 0.05             -- ★ 1 歩の時間

subSteps :: Int
subSteps = 6          -- ★ 1 フレームに進める歩数（大きいほど速い）

dx :: Float
dx = lL / fromIntegral nN

type Field = U.Vector Float

data World = World
  { wRe    :: !Field
  , wIm    :: !Field
  , wV     :: !Field
  , wVc    :: !Field            -- ポテンシャルの半歩 e^{−iV dt/2}
  , wVs    :: !Field
  , wPot   :: !Int
  , wAmp   :: !Float
  , wX0    :: !Float
  , wK0    :: !Float
  , wSigma :: !Float
  , wRun   :: !Bool
  , wDrag  :: !(Maybe Point)    -- ドラッグを始めた場所
  , wE     :: !Float            -- ⟨E⟩
  , wNorm  :: !Float            -- 全確率
  , wPmax  :: !Float
  }

main :: IO ()
main = runSketch (sketch "25-03 Schrodinger 1D" world0)
  { skBack  = hsb 220 30 10
  , skStep  = \_ _ w -> pure (measure (if wRun w && noDrag w then advance w else w))
  , skDraw  = drawW
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }
  where noDrag w = case wDrag w of { Nothing -> True; _ -> False }

world0 :: World
world0 = launch (makePotential (World U.empty U.empty U.empty U.empty U.empty 3 1 (-50) 2 6 True Nothing 0 1 0))

ks, kc, kS, mask :: Field
ks = U.generate nN (\j -> 2 * pi / lL * fromIntegral (if j < nN `div` 2 then j else j - nN))
kc = U.map (\k -> cos (-(k * k * dt) / 2)) ks     -- 運動エネルギーの位相 e^{−ik²dt/2}
kS = U.map (\k -> sin (-(k * k * dt) / 2)) ks
mask = U.generate nN $ \j ->                       -- 端で波を吸い込む（反射させない）
  let d = min j (nN - 1 - j)
      wd = 40 :: Int
  in if d < wd then 1 - 0.03 * sq (fromIntegral (wd - d) / fromIntegral wd) else 1

xAt :: Int -> Float
xAt j = -lL / 2 + fromIntegral j * dx

-- ★ ポテンシャル V(x) の形
makePotential :: World -> World
makePotential w =
  let v j = wAmp w * shape (wPot w) (xAt j)
      vs = U.generate nN v
  in w { wV = vs
       , wVc = U.map (\a -> cos (-a * dt / 2)) vs
       , wVs = U.map (\a -> sin (-a * dt / 2)) vs }

shape :: Int -> Float -> Float
shape p x = case p of
  2 -> if x > 0 then 2.5 else 0
  3 -> if abs x < 2 then 2.5 else 0
  4 -> 0.5 * sq 0.04 * x * x
  5 -> 7.7e-6 * sq (x * x - 625)      -- 底は x = ±25、真ん中の山の高さ ≈ 3
  _ -> 0

-- ガウス波束 ψ = exp(−(x−x0)²/4σ²) e^{ik0 x} を置く
launch :: World -> World
launch w =
  let as = U.generate nN (\j -> exp (-sq (xAt j - wX0 w) / (4 * wSigma w * wSigma w)))
      nrm = 1 / sqrt (U.sum (U.map (\a -> a * a * dx) as))
  in w { wRe = U.imap (\j a -> nrm * a * cos (wK0 w * xAt j)) as
       , wIm = U.imap (\j a -> nrm * a * sin (wK0 w * xAt j)) as }

-- 分割演算子法を subSteps 歩進める
advance :: World -> World
advance w = runST $ do
  re <- U.thaw (wRe w)
  im <- U.thaw (wIm w)
  replicateM_ subSteps $ do
    rotate2 re im (wVc w) (wVs w)   -- V の半歩
    fft False re im
    rotate2 re im kc kS             -- 運動エネルギーの 1 歩（運動量空間）
    fft True re im
    rotate2 re im (wVc w) (wVs w)   -- V の半歩
    scaleBoth re im mask
  r <- U.unsafeFreeze re
  i <- U.unsafeFreeze im
  pure w { wRe = r, wIm = i }

-- エネルギーの期待値 ⟨E⟩ = ⟨T⟩ + ⟨V⟩
measure :: World -> World
measure w =
  let probs = U.zipWith (\a b -> a * a + b * b) (wRe w) (wIm w)
      norm = U.sum probs * dx
      eV = U.sum (U.zipWith (*) (wV w) probs) * dx
      (tr, ti) = fftPure False (wRe w) (wIm w)
      kp = U.zipWith (\a b -> a * a + b * b) tr ti
      eT = U.sum (U.zipWith (\k p -> k * k / 2 * p) ks kp)
      sk = U.sum kp
      e = (if sk > 0 then eT / sk else 0) + (if norm > 1e-9 then eV / norm else 0)
  in w { wE = e, wNorm = norm, wPmax = U.maximum probs }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      left = 40
      wd = width - 80
      base = height * 0.88
      eS = height * 0.11           -- エネルギー 1 あたりの px
      fN = fromIntegral nN :: Float
      px j = left + fromIntegral j / (fN - 1) * wd
      vTop j = base - min (wV w U.! j) 6.5 * eS
      -- ポテンシャル（凹んだ形も塗れるよう、細い台形に分ける）
      potPic = Color (hsb 0 0 30) $ Pictures
        [ Polygon (map (uncurry (pt env)) [(px j, base), (px (j + 1), base), (px (j + 1), vTop (j + 1)), (px j, vTop j)])
        | j <- [0 .. nN - 2], vTop j < base - 0.3 || vTop (j + 1) < base - 0.3 ]
      yE = base - clampF 0 6.5 (wE w) * eS
      hs = height * 0.3 / max (wPmax w) 0.02
      -- |ψ|²（⟨E⟩ の高さから上に、色 = 位相）
      bars = [ seg env (wd / fN + 0.8) (hsb ((atan2 b a + pi) / tau * 360) 80 100) (px j, yE) (px j, yE - p * hs)
             | j <- [0 .. nN - 1]
             , let a = wRe w U.! j
             , let b = wIm w U.! j
             , let p = a * a + b * b
             , p * hs >= 0.5 ]
      edgeW = 40 / fN * wd
      dragPic = case wDrag w of
        Nothing -> []
        Just (fx, fy) ->
          let (mx, _) = mouseP env
          in [ seg env 3 (hsb 55 90 100) (fx, fy) (mx, fy), dotAt env (hsb 55 90 100) fx fy 12 ]
  in Pictures $
       [ potPic
       , seg env 1 (hsba 55 80 100 0.6) (left, yE) (left + wd, yE) ]
       ++ bars
       ++ [ box env (hsba 0 0 100 0.06) left 100 edgeW (base - 100)     -- 吸収する端
          , box env (hsba 0 0 100 0.06) (left + wd - edgeW) 100 edgeW (base - 100) ]
       ++ dragPic
       ++ [ txt env 20 26 16 white ("1-5: " ++ potName (wPot w) ++ "   Drag: throw a packet   Up/Down: V x " ++ nf 2 (wAmp w) ++ "   Left/Right: sigma = " ++ nf 1 (wSigma w))
          , txt env 20 48 16 white ("Space: " ++ (if wRun w then "running" else "paused") ++ "   R: throw again   k0 = " ++ nf 2 (wK0 w) ++ "   <E> = " ++ nf 3 (wE w) ++ "   Total prob = " ++ nf 3 (wNorm w))
          , txt env 20 70 14 (gray 0.6) "Gray = V(x), yellow line = <E>, colored hill = |psi|^2 (color = phase)."
          , txt env 20 90 14 (gray 0.6) "The wave seeps in even where <E> is below V. Total prob falls when the edges absorb it."
          ]

potName :: Int -> String
potName p = case p of
  1 -> "free"
  2 -> "step"
  3 -> "barrier"
  4 -> "harmonic"
  _ -> "double well"

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m@(_, my) = mouseP env in if my > 110 then w { wDrag = Just m } else w
  EventKey (MouseButton LeftButton) Up _ _ -> case wDrag w of
    Nothing -> w
    Just (fx, _) ->
      let (mx, _) = mouseP env
          left = 40
          wd = envW env - 80
          x0 = clampF (-lL / 2 + 25) (lL / 2 - 25) ((fx - left) / wd * lL - lL / 2)
          d = (mx - fx) / 60
          k0 = if abs d > 0.1 then clampF (-4) 4 d else wK0 w
      in launch w { wX0 = x0, wK0 = k0, wDrag = Nothing }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> makePotential w { wPot = fromEnum c - fromEnum '0' }
    | toLower c == 'r' -> launch w
  EventKey (SpecialKey KeyUp) Down _ _ -> makePotential w { wAmp = min 3 (wAmp w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> makePotential w { wAmp = max 0.1 (wAmp w / 1.25) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> launch w { wSigma = max 2 (wSigma w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> launch w { wSigma = min 20 (wSigma w + 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 2（段差）で ⟨E⟩ が段差より高いのに一部が跳ね返る。古典的な粒子ならありえない！
-- 2. 4（調和）で波束を投げると形を保って往復する？ σ を変えると「息をする」ように幅が変わる
-- 3. 5（二重井戸）の片方の底に k0 ≈ 0 で置くと、山を越えられないはずなのに少しずつ反対側へ移る（トンネル）
-- 4. shape に自分の V（たとえば周期的な山 sin x）を足して、結晶の中の電子のようすを見よう

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

scaleBoth :: MV.STVector s Float -> MV.STVector s Float -> Field -> ST s ()
scaleBoth re im m = go 0
  where
    go !j
      | j >= nN = pure ()
      | otherwise = do
          let k = m `U.unsafeIndex` j
          MV.unsafeModify re (* k) j
          MV.unsafeModify im (* k) j
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

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
