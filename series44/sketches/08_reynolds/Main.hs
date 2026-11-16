{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-08 レイノルズ数　Re = U L / ν
-- ナヴィエ・ストークス方程式の「移流（慣性）」と「粘性」の強さの比。流れの性格はこの 1 つの数で決まる
--   ∂u/∂t + (u·∇)u = −∇p/ρ + ν∇²u ,　∇·u = 0　　（07 と同じソルバーで解いている）
--   Re が小さい：粘性が勝ち、乱れはすぐ消える → 層流（なめらか）
--   Re が大きい：慣性が勝ち、小さな乱れが育つ → 渦ができ、やがて乱流へ
-- 左のノズルから速さ U・幅 L の噴流を出し、ν を変えて Re を変える
-- 操作：上下キー = Re / 1〜3 = Re 10・100・1000 / V = 渦度／染料 / R = やり直し
-- ============================================
module Main (main) where

import Art hiding (project)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

ujet, lNozzle, coflow, wobble :: Float
ujet = 1.0          -- ★ 噴流の速さ U [マス/ステップ]
lNozzle = 10        -- ★ ノズルの幅 L [マス]
coflow = 0.12       -- ★ まわりのゆっくりした流れ（U に対する割合）
wobble = 0.03       -- ★ ノズルの出口に入れる小さなゆれ（自然界にいつもある「乱れのたね」）

nx, ny :: Int
nx = 160            -- ★ 格子の数（横）
ny = 100            --   縦は画面（1280 × 800）の形に合わせる

w2, cells :: Int
w2 = nx + 2
cells = w2 * (ny + 2)

dt :: Float
dt = 1

data World = World
  { wRe    :: !Float
  , wView  :: !Int                -- 0 = 渦度, 1 = 染料
  , wU     :: !(U.Vector Float)
  , wV     :: !(U.Vector Float)
  , wP     :: !(U.Vector Float)
  , wDye   :: !(U.Vector Float)
  , wProbe :: ![Float]            -- 観測点での縦の速さ（新しい順、400 個まで）
  }

-- ノズルの速度分布：まん中の幅 L だけ速い（角は少し丸める）
inflow :: U.Vector Float
inflow = U.generate (ny + 2) $ \j ->
  ujet * (coflow + (1 - coflow) * 0.5 * (1 + tanh ((lNozzle / 2 - abs (fromIntegral j - fromIntegral (ny + 1) / 2)) / 1.2)))

world0 :: World
world0 = restart (World 100 0 U.empty U.empty U.empty U.empty [])

restart :: World -> World
restart w = let z = U.replicate cells 0
            in w { wU = U.replicate cells (ujet * coflow), wV = z, wP = z, wDye = z, wProbe = [] }

viscosity :: World -> Float
viscosity w = ujet * lNozzle / wRe w   -- ★ Re = U L / ν　→　ν = U L / Re

main :: IO ()
main = runSketch (sketch "44-08 Reynolds number" world0)
  { skBack  = hsb 215 60 8
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ナヴィエ・ストークス方程式を 1 歩進める（07 と同じ：粘性 → 移流 → 圧力）
stepFluid :: Int -> World -> World
stepFluid frame w = runST $ do
  u <- U.thaw (wU w)
  v <- U.thaw (wV w)
  p <- U.thaw (wP w)
  dye <- U.thaw (wDye w)
  u0 <- MU.new cells
  v0 <- MU.new cells
  dvg <- MU.new cells
  dye0 <- MU.new cells
  let nu = viscosity w
      bd = frame
  MU.copy u0 u
  MU.copy v0 v
  diffuse bd 1 u u0 nu
  diffuse bd 2 v v0 nu
  MU.copy u0 u
  MU.copy v0 v
  advect bd 1 u u0 u0 v0
  advect bd 2 v v0 u0 v0
  project bd u v p dvg
  MU.copy dye0 dye
  advect bd 0 dye dye0 u v
  for 0 (cells - 1) $ \k -> do
    modify' u k (safe 8)
    modify' v k (safe 8)
  u' <- U.freeze u
  v' <- U.freeze v
  p' <- U.freeze p
  d' <- U.freeze dye
  pure w { wU = u', wV = v', wP = p', wDye = d' }

-- 観測点（ノズルから 0.55 画面ぶん下流）
probeI, probeJ :: Int
probeI = floor (fromIntegral nx * 0.55 :: Float)
probeJ = (ny + 1) `div` 2

step :: Env -> Float -> World -> IO World
step env _ w = pure $
  let w1 = stepFluid (envFrame env) w
  in w1 { wProbe = take 400 (wV w1 U.! ix probeI probeJ : wProbe w1) }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) (pixelImage nx ny 1 (render w))
  , view env $
      [ Color (hsb 42 80 100) (Translate px py (ring 7 2))
      , rectTL (hsbA 215 60 8 80) gx gy gw gh
      , Color (hsb 0 0 40) (Line [(gx, gy + gh / 2), (gx + gw, gy + gh / 2)])
      , path 2 (hsb 42 80 100) [ (gx + fromIntegral k / 400 * gw, gy + gh / 2 - clampF (-gh / 2) (gh / 2) (q / ujet * gh))
                               | (k, q) <- zip [0 :: Int ..] (reverse (wProbe w)) ]
      , txt (gx + 8) (gy + 20) 14 (hsb 42 80 100) ("v at the yellow circle (swing " ++ fx 1 (amp / ujet * 100) ++ "% of U)")
      -- 文字
      , rectTL (hsbA 215 60 8 78) 0 0 (envW env) 104
      , txt 20 38 20 (hsb 0 0 100)
          ("Reynolds number Re = U L / nu = " ++ fx 1 ujet ++ " x " ++ show (round lNozzle :: Int) ++ " / " ++ fx 4 nu
           ++ " = " ++ show (round re :: Int) ++ " (Up/Down, 1-3)")
      , txt 20 66 15 (hsb 0 0 80)
          ("Navier-Stokes:  du/dt + (u.grad)u = -grad p/rho + nu lap u ,  div u = 0      view: "
           ++ (if wView w == 0 then "vorticity" else "dye") ++ " (V)   R = reset")
      , txt 20 90 15 (hsb (if re < 150 then 185 else if re < 700 then 42 else 12) 60 100)
          (if re < 150 then "Viscosity wins: the jet spreads smoothly and weakens (laminar). Disturbances die out."
           else if re < 700 then "Inertia and viscosity compete: the jet starts to wave, vortices line up above and below."
           else "Inertia wins: small disturbances grow fast, vortices collide irregularly (toward turbulence).") ]
  ]
  where
    re = wRe w
    nu = viscosity w
    px = fromIntegral probeI / fromIntegral nx * envW env
    py = fromIntegral probeJ / fromIntegral ny * envH env
    gx = envW env - 440
    gy = envH env - 150
    gw = 420
    gh = 130
    amp = maximum (0 : map abs (wProbe w))

-- 格子の値を色にする
render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w x y =
  let n = ix (x + 1) (y + 1)
      u = wU w
      v = wV w
  in if wView w == 1
       then let d = min (wDye w U.! n) 1.2
            in (b8 (8 + 150 * d * d * 0.6), b8 (16 + 190 * d), b8 (34 + 220 * d))
       else -- 渦度：反時計回り = 赤、時計回り = 青
         let s = -0.5 * (v U.! (n + 1) - v U.! (n - 1) - u U.! (n + w2) + u U.! (n - w2)) * 5 / ujet
             a = min (abs s) 1
         in if s > 0 then (b8 (8 + 247 * a), b8 (16 + 110 * a), b8 (34 + 70 * a))
                     else (b8 (8 + 20 * a), b8 (16 + 200 * a), b8 (34 + 221 * a))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRe = min 5000 (wRe w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRe = max 2 (wRe w / 1.5) }
  EventKey (Char '1') Down _ _ -> w { wRe = 10 }
  EventKey (Char '2') Down _ _ -> w { wRe = 100 }
  EventKey (Char '3') Down _ _ -> w { wRe = 1000 }
  EventKey (Char k) Down _ _
    | k `elem` "vV" -> w { wView = 1 - wView w }
    | k `elem` "rR" -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 → 2 → 3 と押して比べる。噴流がうねり始める Re はどのくらい？（上下キーで細かくさがす）
-- 2. 右下のグラフ：Re が小さいとゆれ幅はほぼ 0、大きいと不規則にゆれる。「規則的」にゆれる Re はある？
-- 3. wobble を 0 にする。Re = 1000 でもなめらかなまま？ → 乱れの「たね」がないと育たない
-- 4. lNozzle を 20 にすると Re は 2 倍。U を 2 倍にしても 2 倍。同じ Re なら同じ流れになる？（相似則）
-- 5. 身のまわりの Re を計算しよう：ν（水）≈ 10⁻⁶ m²/s、ν（空気）≈ 1.5 × 10⁻⁵ m²/s。泳ぐ人、飛ぶ蚊、歩く人のまわりの空気
-- ※ 格子があらいので、計算そのものがもつ「にせの粘性（数値粘性）」があり、本当の Re は表示より少し低い

-- ---------- 道具（解くための道具。07 とほぼ同じで、ちがうのは境界だけ） ----------
-- 格子は (nx + 2) × (ny + 2)。まわり 1 マスは境界
type Grid s = MU.MVector s Float

ix :: Int -> Int -> Int
ix i j = i + w2 * j

{-# INLINE rd #-}
rd :: Grid s -> Int -> ST s Float
rd = MU.unsafeRead

{-# INLINE wr #-}
wr :: Grid s -> Int -> Float -> ST s ()
wr = MU.unsafeWrite

{-# INLINE modify' #-}
modify' :: Grid s -> Int -> (Float -> Float) -> ST s ()
modify' g n f = rd g n >>= \x -> wr g n $! f x

{-# INLINE for #-}
for :: Int -> Int -> (Int -> ST s ()) -> ST s ()
for lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

safe :: Float -> Float -> Float
safe m x | isNaN x = 0
         | otherwise = max (-m) (min m x)

diffuse :: Int -> Int -> Grid s -> Grid s -> Float -> ST s ()
diffuse bd b x x0 k = let a = dt * k in linSolve bd b x x0 a (1 + 4 * a) 14

advect :: Int -> Int -> Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
advect bd b !d !d0 !uu !vv = do
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    un <- rd uu n
    vn <- rd vv n
    let x = max 0.5 (min (fromIntegral nx + 0.5) (fromIntegral i - dt * un))
        y = max 0.5 (min (fromIntegral ny + 0.5) (fromIntegral j - dt * vn))
        i0 = floor x
        j0 = floor y
        s = x - fromIntegral i0
        t = y - fromIntegral j0
        m = ix i0 j0
    a <- rd d0 m
    b' <- rd d0 (m + w2)
    c <- rd d0 (m + 1)
    e <- rd d0 (m + 1 + w2)
    wr d n ((1 - s) * ((1 - t) * a + t * b') + s * ((1 - t) * c + t * e))
  setBnd bd b d

project :: Int -> Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
project bd !u !v !p !dvg = do
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    a <- rd u (n + 1)
    b <- rd u (n - 1)
    c <- rd v (n + w2)
    d <- rd v (n - w2)
    wr dvg n (-0.5 * (a - b + c - d))
  linSolve bd 3 p dvg 1 4 40
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    pr <- rd p (n + 1)
    pl <- rd p (n - 1)
    pd <- rd p (n + w2)
    pu <- rd p (n - w2)
    modify' u n (subtract (0.5 * (pr - pl)))
    modify' v n (subtract (0.5 * (pd - pu)))
  setBnd bd 1 u
  setBnd bd 2 v

linSolve :: Int -> Int -> Grid s -> Grid s -> Float -> Float -> Int -> ST s ()
linSolve bd b !x !x0 !a !c iters = for 1 iters $ \_ -> do
  for 1 ny $ \j -> do
    l0 <- rd x (ix 0 j)
    row j 1 l0
  setBnd bd b x
  where
    !ic = 1 / c
    !k = a * ic
    -- 左どなりの値 l は、いま書いたばかりの値をそのまま持ち回る（読み直すより速い）
    row !j !i !l
      | i > nx = pure ()
      | otherwise = do
          let n = ix i j
          s0 <- rd x0 n
          r <- rd x (n + 1)
          up <- rd x (n - w2)
          dn <- rd x (n + w2)
          let new = (s0 + a * (r + up + dn)) * ic + k * l
          wr x n new
          row j (i + 1) new

-- 境界条件：左 = 流入（ノズル）、右 = 流出（そのまま出ていく）、上下 = すべる壁
-- b = 1：u、2：v、3：圧力、0：染料。frame はノズルのゆれに使うフレーム番号
setBnd :: Int -> Int -> Grid s -> ST s ()
setBnd frame b x = do
  for 1 nx $ \i -> do
    a <- rd x (ix i 1)
    c <- rd x (ix i ny)
    wr x (ix i 0) (if b == 2 then -a else a)
    wr x (ix i (ny + 1)) (if b == 2 then -c else c)
  for 0 (ny + 1) $ \j -> do
    let inj = inflow U.! j
        jet = inj > ujet * (coflow + 1) / 2
    case b of
      1 -> wr x (ix 0 j) inj >> wr x (ix 1 j) inj
      2 -> let wv = if jet then wobble * ujet * (sin (fromIntegral frame * 1.57 * ujet / lNozzle) + rand11 (frame * 131 + j)) else 0
           in wr x (ix 0 j) wv >> wr x (ix 1 j) wv
      0 -> let d = if jet then 1 else 0 in wr x (ix 0 j) d >> wr x (ix 1 j) d
      _ -> rd x (ix 1 j) >>= wr x (ix 0 j)
    c <- rd x (ix nx j)
    wr x (ix (nx + 1) j) (if b == 3 then -c else c)   -- 出口：圧力 0、ほかは「こう配 0」

-- 整数から決まる −1〜1 の乱数（Math.random() * 2 − 1 の代わり）
rand11 :: Int -> Float
rand11 k0 =
  let k1 = (k0 `xor` (k0 `shiftR` 7)) * 1103515245 + 12345
      k2 = (k1 `xor` (k1 `shiftR` 13)) * 2654435761
  in fromIntegral ((k2 `shiftR` 8) `mod` 65536) / 32768 - 1

-- ---------- 道具（描く） ----------
fx :: Int -> Float -> String
fx d v = showFFloat (Just d) v ""

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

rectTL :: Color -> Float -> Float -> Float -> Float -> Picture
rectTL c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))
