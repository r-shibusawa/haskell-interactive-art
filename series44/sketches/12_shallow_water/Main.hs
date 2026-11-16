{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-12 浅水波 ─ 水面を伝わる波
-- 水深にくらべて波長が長い波（津波、浴そうの波、池の波紋の大きなもの）は「浅水方程式」にしたがう
--   ∂η/∂t = −∂(H u)/∂x − ∂(H v)/∂y　　（水が集まると水面 η が上がる ＝ 連続の式）
--   ∂u/∂t = −g ∂η/∂x,　∂v/∂t = −g ∂η/∂y　（水面のかたむきが水を押す ＝ 運動方程式）
-- 波の速さは c = √(g H)。深いほど速い（津波は深さ 4000 m で時速 700 km、岸で遅くなり高くなる）
-- 操作：クリック・ドラッグ = 水滴 / W = 道具（水滴・壁を描く・壁を消す）/ 1〜4 = 場面 / P = 造波機
--       上下キー = 水深 H / R = 水面を静める
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

grav, damp, dropSize, dt :: Float
grav = 1.0          -- ★ 重力加速度 g
damp = 0.002        -- ★ 波が弱まる速さ
dropSize = 3        -- ★ 水滴の大きさ [マス]
dt = 0.5

subSteps :: Int
subSteps = 2

nx, ny :: Int
nx = 160            -- ★ 格子の数（横）
ny = 100            --   縦は画面（1280 × 800）の形に合わせる

data World = World
  { wDepth0 :: !Float              -- ★ 水深 H（上下キー。波の速さ c = √(gH)）
  , wEta    :: !(U.Vector Float)   -- 水面の高さ η（nx × ny）
  , wU      :: !(U.Vector Float)   -- 横の速さ（マスの左右の境目、(nx + 1) × ny）
  , wV      :: !(U.Vector Float)   -- 縦の速さ（マスの上下の境目、nx × (ny + 1)）
  , wWall   :: !(U.Vector Bool)
  , wDepth  :: !(U.Vector Float)   -- 場所ごとの水深の割合
  , wTool   :: !Int
  , wScene  :: !Int
  , wMaker  :: !Bool
  , wT      :: !Float
  }

toolNames, sceneNames, sceneNotes :: [String]
toolNames = ["drop water", "draw walls", "erase walls"]
sceneNames = ["", "1 empty pool", "2 one gap (diffraction)", "3 two gaps (interference)", "4 shallow lens (refraction)"]
sceneNotes =
  [ ""
  , "Drop water: ripples spread and bounce off the walls (screen edges)."
  , "Waves through the gap do not go straight: they fan out (diffraction)."
  , "Waves from two gaps overlap: bands that add up and bands that cancel (interference)."
  , "Waves slow down over the shallow part (bright circle) and bend to one point (refraction), like tsunamis at a cape." ]

main :: IO ()
main = runSketch (sketch "44-12 Shallow water waves" (setScene 1 world0))
  { skBack  = hsb 215 60 8
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1.0 U.empty U.empty U.empty U.empty U.empty 0 1 False 0

calm :: World -> World
calm w = w { wEta = U.replicate (nx * ny) 0, wU = U.replicate ((nx + 1) * ny) 0, wV = U.replicate (nx * (ny + 1)) 0 }

setScene :: Int -> World -> World
setScene sc w =
  let wx = floor (fromIntegral nx * 0.35 :: Float)
      mid = ny `div` 2
      wallAt i j = (i == wx || i == wx + 1) &&
                   ((sc == 2 && abs (j - mid) > 3) || (sc == 3 && abs (abs (j - mid) - 9) > 2))
      -- 浅瀬：まん中の円の中だけ浅い（波が遅くなり、レンズのように曲がって集まる）
      depthAt i j =
        let r = sqrt ((fromIntegral i - fromIntegral nx * 0.42) ^ (2 :: Int) + fromIntegral (j - mid) ^ (2 :: Int)) / (fromIntegral ny * 0.3)
        in if sc == 4 && r < 1 then 0.35 + 0.65 * r * r else 1
  in calm w { wScene = sc
            , wWall = U.generate (nx * ny) (\k -> let (j, i) = k `divMod` nx in wallAt i j)
            , wDepth = U.generate (nx * ny) (\k -> let (j, i) = k `divMod` nx in depthAt i j)
            , wMaker = sc > 1 }   -- 場面 2〜4 は左の造波機から平らな波を送る

step :: Env -> Float -> World -> IO World
step env _ w = pure $ runST $ do
  eta <- U.thaw (wEta w)
  uu <- U.thaw (wU w)
  vv <- U.thaw (wV w)
  wall <- U.thaw (wWall w)
  let (mx, my) = mouseP env
      ci = mx / envW env * fromIntegral nx
      cj = my / envH env * fromIntegral ny
  -- マウス：水滴 または 壁
  when (envDown env && my > 100) $
    if wTool w == 0
      then forM_ [max 0 (floor (cj - 3 * dropSize)) .. min ny (ceiling (cj + 3 * dropSize)) - 1] $ \j ->
             forM_ [max 0 (floor (ci - 3 * dropSize)) .. min nx (ceiling (ci + 3 * dropSize)) - 1] $ \i -> do
               let k = i + j * nx
                   add = 0.12 * exp (-((fromIntegral i - ci) ^ (2 :: Int) + (fromIntegral j - cj) ^ (2 :: Int)) / (dropSize * dropSize))
               e <- MU.unsafeRead eta k
               MU.unsafeWrite eta k (e + add)
      else forM_ [-1, 0, 1] $ \dj -> forM_ [-1, 0, 1] $ \di -> do
             let i = floor ci + di
                 j = floor cj + dj
             when (i > 1 && i < nx - 1 && j >= 0 && j < ny) $ do
               MU.unsafeWrite wall (i + j * nx) (wTool w == 1)
               MU.unsafeWrite eta (i + j * nx) 0
  let go !s !t
        | s >= subSteps = pure t
        | otherwise = stepWater w eta uu vv wall t >>= go (s + 1)
  t' <- go 0 (wT w)
  e' <- U.freeze eta
  u' <- U.freeze uu
  v' <- U.freeze vv
  wl <- U.freeze wall
  pure w { wEta = e', wU = u', wV = v', wWall = wl, wT = t' }

-- ★ 浅水方程式を 1 歩進める（速度は「マスの境目」に置く：スタガード格子）
stepWater :: World -> Grid s -> Grid s -> Grid s -> MU.MVector s Bool -> Float -> ST s Float
stepWater w !eta !uu !vv !wall !t = do
  let !g = grav * dt
      !h0 = wDepth0 w
      depth = wDepth w
      maker = wMaker w
  -- 運動方程式：水面の高さの差が、境目の水を押す。壁の面は通さない
  for 0 (ny - 1) $ \j -> for 1 (nx - 1) $ \i -> do
    let k = i + j * nx
        f = i + j * (nx + 1)
    a <- MU.unsafeRead wall k
    b <- MU.unsafeRead wall (k - 1)
    if a || b then wr uu f 0 else do
      u <- rd uu f
      e1 <- rd eta k
      e0 <- rd eta (k - 1)
      wr uu f ((u - g * (e1 - e0)) * (1 - damp))
  for 1 (ny - 1) $ \j -> for 0 (nx - 1) $ \i -> do
    let k = i + j * nx
    a <- MU.unsafeRead wall k
    b <- MU.unsafeRead wall (k - nx)
    if a || b then wr vv k 0 else do
      v <- rd vv k
      e1 <- rd eta k
      e0 <- rd eta (k - nx)
      wr vv k ((v - g * (e1 - e0)) * (1 - damp))
  -- 連続の式：入ってくる水 − 出ていく水 のぶんだけ水面が上下する
  for 0 (ny - 1) $ \j -> for 0 (nx - 1) $ \i -> do
    let k = i + j * nx
        f = i + j * (nx + 1)
        hh = h0 * U.unsafeIndex depth k
    e <- rd eta k
    ur <- rd uu (f + 1)
    ul <- rd uu f
    vd <- rd vv (k + nx)
    vu <- rd vv k
    let e1 = max (-4) (min 4 (e - dt * hh * (ur - ul + vd - vu)))
        e2 = if maker && i > nx - 14 then e1 * (1 - 0.012 * fromIntegral (i - (nx - 14))) else e1   -- 右端で波を吸収する
    wr eta k (if isNaN e2 then 0 else e2)
  -- 造波機：左端の水面を上下にゆらす
  let t' = t + dt
  when maker $ for 0 (ny - 1) $ \j -> wr eta (1 + j * nx) (0.22 * sin (t' * 0.32))
  pure t'

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) (pixelImage nx ny 1 (render w))
  , view env
      [ rectTL (hsbA 215 60 8 78) 0 0 (envW env) 100
      , txt 20 38 18 (hsb 0 0 100)
          ((sceneNames !! wScene w) ++ " (1-4)   tool: " ++ (toolNames !! wTool w) ++ " (W)   maker "
           ++ (if wMaker w then "ON" else "OFF") ++ " (P)   H = " ++ fx 2 (wDepth0 w) ++ " (Up/Down)   R = calm")
      , txt 20 66 15 (hsb 0 0 80)
          ("Shallow water equations  d(eta)/dt = -div(H u) ,  du/dt = -g grad(eta)    wave speed c = sqrt(gH) = "
           ++ fx 2 (sqrt (grav * wDepth0 w)) ++ " cells/step")
      , txt 20 90 15 (hsb 0 0 65) (sceneNotes !! wScene w) ]
  ]

-- 水面を色に：高い所は明るく、低い所は暗く。かたむきで光の反射をつける
render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w i j
  | wWall w U.! k = (222, 196, 140)
  | otherwise =
      let eta = wEta w
          e = eta U.! k
          slope = if i > 0 && i < nx - 1 then eta U.! (k + 1) - eta U.! (k - 1) else 0
          a = clampF 0 1 (0.5 + e * 1.8)
          shine = clampF (-1) 1 (slope * 6)
          shallow = 1 - wDepth w U.! k
          hi = max shine 0
          dark = 1 + 0.3 * min shine 0   -- 光が当たる側は明るく、反対側は暗く
      in ( b8 ((10 + 50 * a + 110 * hi + 60 * shallow) * dark)
         , b8 ((40 + 140 * a + 70 * hi + 45 * shallow) * dark)
         , b8 ((80 + 150 * a + 30 * hi + 10 * shallow) * dark) )
  where k = i + j * nx

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k >= '1' && k <= '4' -> setScene (fromEnum k - fromEnum '0') w
    | k `elem` "wW" -> w { wTool = (wTool w + 1) `mod` 3 }
    | k `elem` "pP" -> w { wMaker = not (wMaker w) }
    | k `elem` "rR" -> calm w
  -- これ以上深いと計算がこわれる（c Δt < 0.7 マス）
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wDepth0 = min 1.6 (wDepth0 w + 0.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wDepth0 = max 0.2 (wDepth0 w - 0.2) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 で水滴を 1 つ。波紋が端ではね返る。2 か所に続けて落とすと、重なった所はどうなる？
-- 2. 水深 H を 0.2 と 1.6 で比べる。波が画面を横切る時間はどのくらいちがう？（c = √(gH)）
-- 3. 2 のすき間：W で壁を消して、すき間を広げていく。広いと波はまっすぐ進む。波長と比べてどのくらいから？
-- 4. 3 の干渉：すき間の間かく（9 マス）を変えると、筋の本数はどう変わる？
-- 5. 4 の浅瀬：波が集まる「焦点」はどこ？ depthAt の 0.35 を 0.7 にすると焦点は遠くなる？ 近くなる？
-- 6. W で壁を描いて、自分の港（防波堤）を作ろう。港の中を静かに保つには？

-- ---------- 道具 ----------
type Grid s = MU.MVector s Float

{-# INLINE rd #-}
rd :: Grid s -> Int -> ST s Float
rd = MU.unsafeRead

{-# INLINE wr #-}
wr :: Grid s -> Int -> Float -> ST s ()
wr = MU.unsafeWrite

{-# INLINE for #-}
for :: Int -> Int -> (Int -> ST s ()) -> ST s ()
for lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

fx :: Int -> Float -> String
fx d v = showFFloat (Just d) v ""

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

rectTL :: Color -> Float -> Float -> Float -> Float -> Picture
rectTL c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)
