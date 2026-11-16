{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-15 カメラで流体をかき混ぜる 📷
-- カメラの映像の「動いた所」を力に変えて、07 のナヴィエ・ストークス方程式のソルバーに入れる
--   明るさの時間変化 ∂I/∂t と、明るさのこう配 ∇I から、動きの向きを見積もる（オプティカルフロー）
--     動き ≈ −(∂I/∂t) ∇I / |∇I|²
--   動いた所には、映像の色がそのまま染料として入る → 手をふると自分の色のけむりが流れる
-- カメラが使えないとき・M を押したときは、マウスのドラッグでかき混ぜる
-- 操作：V = 映像を表示 / 上下キー = 力の強さ / 左右キー = 感度 / M = マウスに切替 / C = 消す
-- ============================================
module Main (main) where

import Art hiding (project)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

nu0, dt, fadeK :: Float
nu0 = 0.002         -- ★ 動粘性係数 ν
dt = 1.0
fadeK = 0.985       -- ★ 染料が消える速さ

camW, camH :: Int
camW = 160          -- カメラ画像の大きさ
camH = 120

nx, ny :: Int
nx = 112            -- ★ 格子の数（横）
ny = 70             --   縦は画面（1280 × 800）の形に合わせる

w2, cells :: Int
w2 = nx + 2
cells = w2 * (ny + 2)

data World = World
  { wCam     :: !(Maybe Camera)
  , wFrame   :: !B.ByteString       -- 最新のカメラ映像
  , wCamOK   :: !Bool
  , wShowCam :: !Bool
  , wMouse   :: !Bool               -- マウスでかき混ぜる
  , wGain    :: !Float              -- ★ 動きを力にする強さ（上下キー）
  , wSense   :: !Float              -- ★ この明るさの変化より小さい動きは無視（左右キー。小さいほど敏感）
  , wMoved   :: !Int                -- 動いたマスの数
  , wU       :: !(U.Vector Float)
  , wV       :: !(U.Vector Float)
  , wP       :: !(U.Vector Float)
  , wDyeR    :: !(U.Vector Float)
  , wDyeG    :: !(U.Vector Float)
  , wDyeB    :: !(U.Vector Float)
  , wLum     :: !(U.Vector Float)   -- 各マスの明るさ（いま）
  , wPrev    :: !Point
  }

world0 :: World
world0 = wipe (World Nothing B.empty False True False 0.5 18 0 U.empty U.empty U.empty U.empty U.empty U.empty (U.replicate cells 0) (0, 0))

wipe :: World -> World
wipe w = let z = U.replicate cells 0 in w { wU = z, wV = z, wP = z, wDyeR = z, wDyeG = z, wDyeB = z }

main :: IO ()
main = runSketch (sketch "44-15 Stir the fluid with a webcam" world0)
  { skBack  = hsb 215 60 6
  , skStart = \_ -> do
      cam <- startCamera camW camH
      pure world0 { wCam = Just cam }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 格子のマス (i, j) に対応するカメラの画素の番号
camIndex :: Int -> Int -> Int
camIndex i j =
  let cx = min (camW - 1) (floor ((fromIntegral i - 0.5) / fromIntegral nx * fromIntegral camW :: Float))
      cy = min (camH - 1) (floor ((fromIntegral j - 0.5) / fromIntegral ny * fromIntegral camH :: Float))
  in (cx + cy * camW) * 3

step :: Env -> Float -> World -> IO World
step env _ w = do
  live <- maybe (pure False) camLive (wCam w)
  frame <- case wCam w of
    Just c | live -> cameraFrame c
    _ -> pure B.empty
  let camOK = live && B.length frame >= camW * camH * 3
      (mx, my) = mouseP env
      (pmx, pmy) = wPrev w
      byte k = fromIntegral (BU.unsafeIndex frame k) :: Float
      -- 格子の各マスに対応する画素の明るさを読む
      lumNew = if camOK
                 then U.generate cells (\n -> let (j, i) = n `divMod` w2
                                              in if i >= 1 && i <= nx && j >= 1 && j <= ny
                                                   then let q = camIndex i j in (byte q + byte (q + 1) + byte (q + 2)) / 3
                                                   else 0)
                 else wLum w
      lumOld = wLum w
      active = camOK && not (wMouse w) && envFrame env >= 20   -- 最初の数フレームは明るさが安定しないので待つ
      -- カメラがないとき：マウスのドラッグで力と色（場所で色が変わる）
      useMouseNow = (not camOK || wMouse w) && envDown env
      hue = fromIntegral ((envFrame env * 2) `mod` 360) :: Float
      mcol = (0.5 + 0.5 * cos (hue / 57.3), 0.5 + 0.5 * cos (hue / 57.3 - 2.1), 0.5 + 0.5 * cos (hue / 57.3 + 2.1))
      (u', v', p', r', g', b', moved) = runST $ do
        u <- U.thaw (wU w)
        v <- U.thaw (wV w)
        p <- U.thaw (wP w)
        dr <- U.thaw (wDyeR w)
        dg <- U.thaw (wDyeG w)
        db <- U.thaw (wDyeB w)
        u0 <- MU.new cells
        v0 <- MU.new cells
        dvg <- MU.new cells
        tmp <- MU.new cells
        cnt <- MU.replicate 1 (0 :: Int)
        -- ★ カメラの動きを力と染料に変える
        when active $ for 2 (ny - 1) $ \j -> for 2 (nx - 1) $ \i -> do
          let n = ix i j
              it = lumNew U.! n - lumOld U.! n
          when (abs it >= wSense w) $ do
            let gx = (lumNew U.! (n + 1) - lumNew U.! (n - 1)) / 2
                gy = (lumNew U.! (n + w2) - lumNew U.! (n - w2)) / 2
                g2 = gx * gx + gy * gy + 40
                fx = clampF (-2) 2 (-it * gx / g2)   -- 動きの向き
                fy = clampF (-2) 2 (-it * gy / g2)
                q = camIndex i j
            modify' u n (+ fx * wGain w)
            modify' v n (+ fy * wGain w)
            modify' dr n (\d -> min (d + byte q / 255 * 0.35) 1.5)
            modify' dg n (\d -> min (d + byte (q + 1) / 255 * 0.35) 1.5)
            modify' db n (\d -> min (d + byte (q + 2) / 255 * 0.35) 1.5)
            MU.modify cnt (+ 1) 0
        when useMouseNow $ do
          let ci = mx / envW env * fromIntegral nx + 1
              cj = my / envH env * fromIntegral ny + 1
              fx = clampF (-4) 4 ((mx - pmx) / envW env * fromIntegral nx * 0.5)
              fy = clampF (-4) 4 ((my - pmy) / envH env * fromIntegral ny * 0.5)
              (c0, c1, c2) = mcol
          forM_ [max 1 (floor (cj - 7)) .. min ny (floor (cj + 7))] $ \j ->
            forM_ [max 1 (floor (ci - 7)) .. min nx (floor (ci + 7))] $ \i -> do
              let wt = exp (-((fromIntegral i - ci) ^ (2 :: Int) + (fromIntegral j - cj) ^ (2 :: Int)) / 10)
                  n = ix i j
              modify' u n (+ fx * wt)
              modify' v n (+ fy * wt)
              modify' dr n (\d -> min (d + c0 * wt * 0.5) 1.5)
              modify' dg n (\d -> min (d + c1 * wt * 0.5) 1.5)
              modify' db n (\d -> min (d + c2 * wt * 0.5) 1.5)
        stepVelocity nu0 u v u0 v0 p dvg   -- ナヴィエ・ストークス方程式を 1 歩
        forM_ [dr, dg, db] $ \d -> do
          MU.copy tmp d
          advect 0 d tmp u v
          for 0 (cells - 1) $ \k -> modify' d k (* fadeK)
        c <- MU.read cnt 0
        (,,,,,,) <$> U.freeze u <*> U.freeze v <*> U.freeze p <*> U.freeze dr <*> U.freeze dg <*> U.freeze db <*> pure c
  pure w { wFrame = frame, wCamOK = camOK, wLum = lumNew, wU = u', wV = v', wP = p'
         , wDyeR = r', wDyeG = g', wDyeB = b', wMoved = moved, wPrev = (mx, my) }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) (pixelImage nx ny 1 compose)
  , view env
      [ rectTL (hsbA 215 60 8 78) 0 0 (envW env) 100
      , txt 20 34 18 (hsb 0 0 100)
          ((if wCamOK w && not (wMouse w)
              then "Move your hands in front of the camera (moving cells " ++ show (wMoved w) ++ ")"
              else "Drag the mouse to stir" ++ (if wCamOK w then "" else " (no camera: allow camera access)"))
           ++ "   V video   M mouse   C clear")
      , txt 20 62 15 (hsb 0 0 75)
          "The force f in the Navier-Stokes equations  du/dt + (u.grad)u = -grad p/rho + nu lap u + f  comes from the video motion."
      , txt 20 86 15 (hsb 0 0 75)
          ("force " ++ showFFloat (Just 2) (wGain w) "" ++ " (Up/Down)   sensitivity " ++ show (round (wSense w) :: Int) ++ " (Left/Right)") ]
  ]
  where
    f = wFrame w
    camOn = wCamOK w && wShowCam w && B.length f >= camW * camH * 3
    -- 背景 → 映像（30%）→ 染料（染料のない所は透明）の順に重ねる
    compose x y =
      let i = x + 1
          j = y + 1
          n = ix i j
          q = camIndex i j
          cam k bg = if camOn then bg + 0.3 * (fromIntegral (BU.unsafeIndex f (q + k)) - bg) else bg
          r0 = cam 0 6
          g0 = cam 1 10
          b0 = cam 2 15
          dr = wDyeR w U.! n
          dg = wDyeG w U.! n
          db = wDyeB w U.! n
          a = min (max dr (max dg db) * 320) 255 / 255
          mix base d = base + a * (min (255 * d) 255 - base)
      in (b8 (mix r0 dr), b8 (mix g0 dg), b8 (mix b0 db))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "vV" -> w { wShowCam = not (wShowCam w) }
    | k `elem` "mM" -> w { wMouse = not (wMouse w) }
    | k `elem` "cC" -> wipe w   -- 明るさの記録は残す
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wGain = min 2 (wGain w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wGain = max 0.1 (wGain w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSense = min 60 (wSense w + 3) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSense = max 6 (wSense w - 3) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 手をゆっくり横に動かす／すばやくふる。けむりの流れる向きと勢いはどう変わる？
-- 2. 色のついた物（ペン、スマホの画面）をふる。その色の染料が出る
-- 3. 感度（左右キー）を下げるとノイズでも動く。上げると大きな動きだけに反応する
-- 4. nu0 を 0.05 にすると、ねばい液体の中で動いているようになる
-- 5. fadeK を 0.999 にして絵を残す。14 の墨流しのように、体で模様を描こう
-- 6. カメラの力を足す所で v から 0.05 を引くと、動いた所から上へ「炎」のように立ちのぼる

-- ---------- 道具（流体ソルバー。07 と同じ：粘性 → 移流 → 圧力 の順に解く） ----------
-- 格子は (nx + 2) × (ny + 2)。まわり 1 マスは壁（境界）
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

-- 大きすぎる値と NaN をおさえる
safe :: Float -> Float -> Float
safe m x | isNaN x = 0
         | otherwise = max (-m) (min m x)

-- ナヴィエ・ストークス方程式を 1 歩：粘性 ν∇²u → 移流 −(u·∇)u → 圧力 −∇p と ∇·u = 0
stepVelocity :: Float -> Grid s -> Grid s -> Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
stepVelocity !nu u v u0 v0 p dvg = do
  when (nu > 0) $ do
    MU.copy u0 u
    MU.copy v0 v
    diffuse 1 u u0 nu
    diffuse 2 v v0 nu
  MU.copy u0 u
  MU.copy v0 v
  advect 1 u u0 u0 v0
  advect 2 v v0 u0 v0
  project u v p dvg
  for 0 (cells - 1) $ \k -> do
    modify' u k (safe 20)
    modify' v k (safe 20)

diffuse :: Int -> Grid s -> Grid s -> Float -> ST s ()
diffuse b x x0 k = let a = dt * k in linSolve b x x0 a (1 + 4 * a) 10

-- 移流：各マスで「Δt 前にここへ来たはずの場所」へさかのぼり、そこの値をもらう
advect :: Int -> Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
advect b !d !d0 !uu !vv = do
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
  setBnd b d

-- 射影：∇²p = ∇·u を解き、u ← u − ∇p
project :: Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
project !u !v !p !dvg = do
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    a <- rd u (n + 1)
    b <- rd u (n - 1)
    c <- rd v (n + w2)
    d <- rd v (n - w2)
    wr dvg n (-0.5 * (a - b + c - d))
  setBnd 0 dvg
  linSolve 0 p dvg 1 4 25
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    pr <- rd p (n + 1)
    pl <- rd p (n - 1)
    pd <- rd p (n + w2)
    pu <- rd p (n - w2)
    modify' u n (subtract (0.5 * (pr - pl)))
    modify' v n (subtract (0.5 * (pd - pu)))
  setBnd 1 u
  setBnd 2 v

-- ガウス・ザイデル法：x = (x0 + a × まわり 4 マスの合計) / c をくり返す
linSolve :: Int -> Grid s -> Grid s -> Float -> Float -> Int -> ST s ()
linSolve b !x !x0 !a !c iters = for 1 iters $ \_ -> do
  for 1 ny $ \j -> do
    l0 <- rd x (ix 0 j)
    row j 1 l0
  setBnd b x
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

-- 境界条件：壁は通りぬけない。b = 1 は u（左右の壁で反転）、b = 2 は v（上下の壁で反転）、0 はとなりをコピー
setBnd :: Int -> Grid s -> ST s ()
setBnd b x = do
  for 1 nx $ \i -> do
    a <- rd x (ix i 1)
    c <- rd x (ix i ny)
    wr x (ix i 0) (if b == 2 then -a else a)
    wr x (ix i (ny + 1)) (if b == 2 then -c else c)
  for 0 (ny + 1) $ \j -> do
    a <- rd x (ix 1 j)
    c <- rd x (ix nx j)
    wr x (ix 0 j) (if b == 1 then -a else a)
    wr x (ix (nx + 1) j) (if b == 1 then -c else c)

-- ---------- 道具（描く） ----------
b8 :: Float -> Word8
b8 x = fromIntegral (max 0 (min 255 (round x :: Int)))

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
