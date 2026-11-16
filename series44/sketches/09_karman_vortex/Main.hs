{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-09 カルマン渦列
-- 流れの中に円柱を置くと、後ろに「右回り・左回り」の渦が交互にはがれて 2 列に並ぶ
-- 電線が風でヒューと鳴る、旗がはためく、橋がゆれる ── みんなこの渦のしわざ
--   はがれる周波数 f は ストローハル数 St = f D / U ≈ 0.2 でほぼ決まる（D = 円柱の直径）
-- ここでは格子ボルツマン法（LBM, D2Q9）で解く：流体を「9 方向に動く粒子の集まり」として
-- ぶつかる → 動く をくり返すと、全体としてナヴィエ・ストークス方程式にしたがう流れになる
-- 操作：ドラッグ = 物体を動かす / 上下キー = レイノルズ数 / 1〜3 = 形 / V = 表示 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

u0 :: Float
u0 = 0.1            -- ★ 流れの速さ（格子の単位。0.15 より大きくすると計算がこわれやすい）

dBody :: Int
dBody = 12          -- ★ 物体の大きさ [マス]

stepsPerFrame :: Int
stepsPerFrame = 8   -- ★ 1 フレームに進める回数

nx, ny, nn :: Int
nx = 160            -- ★ 格子の数（横）
ny = 100            --   縦は画面（1280 × 800）の形に合わせる
nn = nx * ny

-- 9 方向の粒子の分布と、密度・速度を 1 本の配列にまとめて持つ。
-- 番号：0 = 止まっている, 1 = N, 2 = S, 3 = E, 4 = W, 5 = NE, 6 = SE, 7 = NW, 8 = SW, 9 = 密度 ρ, 10 = ux, 11 = uy
cN, cS, cE, cW, cNE, cSE, cNW, cSW, cRho, cUx, cUy :: Int
cN = 1; cS = 2; cE = 3; cW = 4; cNE = 5; cSE = 6; cNW = 7; cSW = 8; cRho = 9; cUx = 10; cUy = 11

data World = World
  { wRe      :: !Float            -- ★ レイノルズ数 Re = U D / ν
  , wShape   :: !Int              -- 1 = 円柱, 2 = 平板, 3 = 四角柱
  , wView    :: !Int              -- 0 = 渦度, 1 = 速さ
  , wF       :: !(U.Vector Float) -- 12 × nn
  , wBar     :: !(U.Vector Bool)  -- 物体のマス
  , wBx      :: !Int
  , wBy      :: !Int
  , wSteps   :: !Int
  , wLastX   :: !Int              -- 最後に「負 → 正」になったステップ
  , wPeriods :: ![Int]
  , wLastV   :: !Float
  , wDots    :: ![Point]          -- 流れに乗る粒（格子の座標）
  }

main :: IO ()
main = runSketch (sketch "44-09 Karman vortex street" (restart world0))
  { skBack  = hsb 215 60 8
  , skStart = \_ -> do
      ds <- replicateM 400 ((,) <$> randomRIO (0, fromIntegral nx) <*> randomRIO (0, fromIntegral ny))
      pure (restart world0 { wDots = ds })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 100 1 0 U.empty (U.replicate nn False)
  (round (fromIntegral nx * 0.22 :: Float)) (round (fromIntegral ny * 0.5 :: Float) + 1)   -- まん中から 1 マスずらす（渦が早く出始める）
  0 0 [] 0 []

restart :: World -> World
restart w =
  let f = runST $ do
        mv <- MU.new (12 * nn)
        for 0 (nn - 1) $ \k -> setEquil mv k u0 0 1
        U.freeze mv
  in placeBody (w { wF = f, wBar = U.replicate nn False, wSteps = 0, wPeriods = [], wLastX = 0 })

viscosity :: World -> Float
viscosity w = max (u0 * fromIntegral dBody / wRe w) 0.004   -- ν = U D / Re

-- 物体の形をマス目に書きこむ（物体がどいた所は、止まった流体でうめる）
placeBody :: World -> World
placeBody w =
  let d = fromIntegral dBody :: Float
      inBody x y =
        let dx = x - wBx w
            dy = y - wBy w
            body = case wShape w of
              1 -> fromIntegral (dx * dx + dy * dy) <= d * d / 4
              2 -> dx == 0 && fromIntegral (abs dy) <= d / 2
              _ -> fromIntegral (abs dx) <= d / 2 && fromIntegral (abs dy) <= d / 2
        in body && x > 1 && x < nx - 2 && y > 1 && y < ny - 2
      bar = U.generate nn (\k -> let (y, x) = k `divMod` nx in inBody x y)
      f = runST $ do
        mv <- U.thaw (wF w)
        for 0 (nn - 1) $ \k -> when (wBar w U.! k && not (bar U.! k)) (setEquil mv k 0 0 1)
        U.freeze mv
  in w { wBar = bar, wF = f }

-- ---------- 格子ボルツマン法の 1 フレーム ----------
step :: Env -> Float -> World -> IO World
step _ _ w = do
  let omega = 1 / (3 * viscosity w + 0.5)
      pk = min (wBx w + 3 * dBody) (nx - 3) + wBy w * nx   -- 物体の後ろの観測点
      (f', steps, lastX, periods, lastV, broken) = runST $ do
        mv <- U.thaw (wF w)
        let go !s !st !lx ps !lv !bad
              | s >= stepsPerFrame = pure (st, lx, ps, lv, bad)
              | otherwise = do
                  -- 外わくは一様流
                  for 0 (nx - 1) $ \x -> setEquil mv x u0 0 1 >> setEquil mv (x + (ny - 1) * nx) u0 0 1
                  for 0 (ny - 1) $ \y -> setEquil mv (y * nx) u0 0 1 >> setEquil mv (nx - 1 + y * nx) u0 0 1
                  collide mv omega
                  stream mv (wBar w)
                  let st' = st + 1
                  -- 物体の後ろで、縦の速さ uy が「負 → 正」になる瞬間を数える（= 渦が 1 組はがれた）
                  pv <- rd mv (cUy * nn + pk)
                  r <- rd mv (cRho * nn + pk)
                  let crossed = lv < 0 && pv >= 0
                      ps' = if crossed && lx > 0 && st' - lx > 40 then lastN 6 (ps ++ [st' - lx]) else ps
                      lx' = if crossed then st' else lx
                  go (s + 1) st' lx' ps' pv (bad || not (r > 0.3 && r < 3))
        (st, lx, ps, lv, bad) <- go 0 (wSteps w) (wLastX w) (wPeriods w) (wLastV w) False
        f2 <- U.freeze mv
        pure (f2, st, lx, ps, lv, bad)
      w1 = w { wF = f', wSteps = steps, wLastX = lastX, wPeriods = periods, wLastV = lastV }
  if broken
    then pure (restart w1)   -- 計算がこわれたら最初から
    else do
      ds <- mapM (moveDot w1) (wDots w1)
      pure w1 { wDots = ds }

-- 流れに乗る粒
moveDot :: World -> Point -> IO Point
moveDot w (x0, y0) = do
  let k = clampI 1 (nx - 2) (floor x0) + clampI 1 (ny - 2) (floor y0) * nx
      f = wF w
      x = x0 + f U.! (cUx * nn + k) * fromIntegral stepsPerFrame
      y = y0 + f U.! (cUy * nn + k) * fromIntegral stepsPerFrame
  if x > fromIntegral (nx - 1) || x < 0 || y < 0 || y > fromIntegral ny || wBar w U.! k || isNaN x || isNaN y
    then (,) 1 <$> randomRIO (0, fromIntegral ny)
    else pure (x, y)

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale cw ch (pixelImage nx ny 1 (render w))
  , view env $
      [ dot (hsbA 0 0 100 60) (x * cw) (y * ch) 3 | (x, y) <- wDots w ]
      ++ [ Color (hsb 42 80 100) (Translate (fromIntegral (min (wBx w + 3 * dBody) (nx - 3)) * cw) (fromIntegral (wBy w) * ch) (ring 6 2))
         , rectTL (hsbA 215 60 8 78) 0 0 (envW env) 100
         , txt 20 38 18 (hsb 0 0 100)
             ("Re = U D / nu = " ++ show (round (u0 * fromIntegral dBody / viscosity w) :: Int) ++ " (Up/Down)   shape: "
              ++ (["", "cylinder", "flat plate", "square"] !! wShape w) ++ " (1-3)   view: " ++ (if wView w == 1 then "speed" else "vorticity")
              ++ " (V)   drag = move   R = reset")
         , txt 20 66 15 (hsb 0 0 80)
             (if tt > 0
                then "Vortex period at the yellow circle T = " ++ show (round tt :: Int) ++ " steps  ->  Strouhal number St = D / (U T) = "
                     ++ showFFloat (Just 3) (fromIntegral dBody / (u0 * tt)) "" ++ "   (experiments: 0.16-0.21 for Re 100-1000)"
                else "Waiting for vortices to shed... (below Re ~ 50 they stay attached behind the body)")
         , txt 20 90 15 (hsb 0 0 65) "Red = counter-clockwise, blue = clockwise. They shed alternately from top and bottom, in a zigzag." ]
  ]
  where
    cw = envW env / fromIntegral nx
    ch = envH env / fromIntegral ny
    ps = wPeriods w
    tt = if null ps then 0 else fromIntegral (sum ps) / fromIntegral (length ps) :: Float

-- 色をつける（上が y = 0 の行）
render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w x y
  | wBar w U.! k = (222, 196, 140)
  | wView w == 0 =   -- 渦度（画面で反時計回り = 赤）。外わくのすぐそばは描かない
      let s = if edge then 0 else -(uy (k + 1) - uy (k - 1) - ux (k + nx) + ux (k - nx)) * 0.5 / u0 * 6
          a = min (abs s) 1
      in if s > 0 then (b8 (8 + 247 * a), b8 (16 + 110 * a), b8 (34 + 70 * a))
                  else (b8 (8 + 20 * a), b8 (16 + 200 * a), b8 (34 + 221 * a))
  | otherwise =
      let sp = min (sqrt (ux k * ux k + uy k * uy k) / u0 / 1.6) 1
      in (b8 (8 + 240 * sp * sp), b8 (16 + 220 * sp), b8 (60 + 150 * sp))
  where
    k = x + y * nx
    f = wF w
    ux i = f U.! (cUx * nn + i)
    uy i = f U.! (cUy * nn + i)
    edge = x < 3 || x > nx - 4 || y < 3 || y > ny - 4

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventMotion _ | envDown env ->
    let (mx, my) = mouseP env
        bx = clampI dBody (nx - 4 * dBody) (round (mx / envW env * fromIntegral nx))
        by = clampI dBody (ny - dBody) (round (my / envH env * fromIntegral ny))
    in placeBody w { wBx = bx, wBy = by, wPeriods = [], wLastX = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRe = min 300 (wRe w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRe = max 10 (wRe w / 1.25) }
  EventKey (Char k) Down _ _
    | k `elem` "123" -> placeBody w { wShape = fromEnum k - fromEnum '0', wPeriods = [], wLastX = 0 }
    | k `elem` "vV" -> w { wView = 1 - wView w }
    | k `elem` "rR" -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 渦がはがれ始めるまで待つ。St はいくつ？ Re を上下キーで変えると St はどう変わる？
-- 2. Re を 30 まで下げる。渦は 2 つ並んで円柱の後ろにくっつく（双子渦）。はがれ始める Re は？
-- 3. 円柱をドラッグでゆっくり動かす／壁の近くに置く。渦の並びはどう変わる？
-- 4. 2 の平板・3 の四角柱と比べる。St が大きい（速くはがれる）のはどれ？
-- 5. 直径 5 mm の電線に風速 10 m/s：f = St U / D = 0.2 × 10 / 0.005 = 400 Hz。これが「風の音」
-- 6. dBody を 6 や 20 にして St を測る。直径が変わっても St がほぼ同じなら、相似則が成り立っている

-- ---------- 道具（格子ボルツマン法） ----------
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

-- 大きい方から小さい方へ（逆向きの for 文）
{-# INLINE forDown #-}
forDown :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forDown hi lo f = go hi
  where go !i | i < lo = pure ()
              | otherwise = f i >> go (i - 1)

-- 分布 c の k 番目
{-# INLINE at' #-}
at' :: Int -> Int -> Int
at' c k = c * nn + k

-- 平衡分布：速度 (vx, vy)・密度 r の流体なら、9 方向の粒子はこの割合になる
setEquil :: Grid s -> Int -> Float -> Float -> Float -> ST s ()
setEquil !mv !k !vx !vy !r = do
  let u2 = 1 - 1.5 * (vx * vx + vy * vy)
      a = r / 9
      b = r / 36
      s = vx + vy
      d = vx - vy
  wr mv (at' 0 k) (4 / 9 * r * u2)
  wr mv (at' cE k) (a * (u2 + 3 * vx + 4.5 * vx * vx))
  wr mv (at' cW k) (a * (u2 - 3 * vx + 4.5 * vx * vx))
  wr mv (at' cN k) (a * (u2 + 3 * vy + 4.5 * vy * vy))
  wr mv (at' cS k) (a * (u2 - 3 * vy + 4.5 * vy * vy))
  wr mv (at' cNE k) (b * (u2 + 3 * s + 4.5 * s * s))
  wr mv (at' cSW k) (b * (u2 - 3 * s + 4.5 * s * s))
  wr mv (at' cSE k) (b * (u2 + 3 * d + 4.5 * d * d))
  wr mv (at' cNW k) (b * (u2 - 3 * d + 4.5 * d * d))
  wr mv (at' cRho k) r
  wr mv (at' cUx k) vx
  wr mv (at' cUy k) vy

-- ★ 衝突：各マスで密度と速度を求め、粒子の分布を平衡分布へ近づける（近づく速さ ω が粘性を決める）
collide :: Grid s -> Float -> ST s ()
collide !mv !omega =
  for 1 (ny - 2) $ \y -> for 1 (nx - 2) $ \x -> do
    let k = x + y * nx
        relax c target = do
          old <- rd mv (at' c k)
          wr mv (at' c k) (old + omega * (target - old))
    f0 <- rd mv (at' 0 k)
    fN <- rd mv (at' cN k)
    fS <- rd mv (at' cS k)
    fE <- rd mv (at' cE k)
    fW <- rd mv (at' cW k)
    fNE <- rd mv (at' cNE k)
    fSE <- rd mv (at' cSE k)
    fNW <- rd mv (at' cNW k)
    fSW <- rd mv (at' cSW k)
    let r = f0 + fN + fS + fE + fW + fNW + fNE + fSW + fSE
        vx = (fE + fNE + fSE - fW - fNW - fSW) / r
        vy = (fN + fNE + fNW - fS - fSE - fSW) / r
        u2 = 1 - 1.5 * (vx * vx + vy * vy)
        a = r / 9
        b = r / 36
        s = vx + vy
        d = vx - vy
    wr mv (at' cRho k) r
    wr mv (at' cUx k) vx
    wr mv (at' cUy k) vy
    relax 0 (4 / 9 * r * u2)
    relax cE (a * (u2 + 3 * vx + 4.5 * vx * vx))
    relax cW (a * (u2 - 3 * vx + 4.5 * vx * vx))
    relax cN (a * (u2 + 3 * vy + 4.5 * vy * vy))
    relax cS (a * (u2 - 3 * vy + 4.5 * vy * vy))
    relax cNE (b * (u2 + 3 * s + 4.5 * s * s))
    relax cSW (b * (u2 - 3 * s + 4.5 * s * s))
    relax cSE (b * (u2 + 3 * d + 4.5 * d * d))
    relax cNW (b * (u2 - 3 * d + 4.5 * d * d))

-- ★ 並進：粒子をそれぞれの向きへ 1 マス動かす。物体に当たった粒子は来た向きへはね返る
-- （同じ配列の中でずらすので、まだ動かしていない値を先に読む順番で回す）
stream :: Grid s -> U.Vector Bool -> ST s ()
stream !mv !bar = do
  let copy c to from = rd mv (at' c from) >>= wr mv (at' c to)
  forDown (ny - 2) 1 $ \y -> for 1 (nx - 2) $ \x -> do
    let k = x + y * nx
    copy cN k (k - nx)
    copy cNW k (k + 1 - nx)
  forDown (ny - 2) 1 $ \y -> forDown (nx - 2) 1 $ \x -> do
    let k = x + y * nx
    copy cE k (k - 1)
    copy cNE k (k - 1 - nx)
  for 1 (ny - 2) $ \y -> forDown (nx - 2) 1 $ \x -> do
    let k = x + y * nx
    copy cS k (k + nx)
    copy cSE k (k - 1 + nx)
  for 1 (ny - 2) $ \y -> for 1 (nx - 2) $ \x -> do
    let k = x + y * nx
    copy cW k (k + 1)
    copy cSW k (k + 1 + nx)
  for 2 (ny - 3) $ \y -> for 2 (nx - 3) $ \x -> do
    let k = x + y * nx
        back cTo to cFrom = rd mv (at' cFrom k) >>= wr mv (at' cTo to)
    when (U.unsafeIndex bar k) $ do
      back cE (k + 1) cW
      back cW (k - 1) cE
      back cN (k + nx) cS
      back cS (k - nx) cN
      back cNE (k + nx + 1) cSW
      back cNW (k + nx - 1) cSE
      back cSE (k - nx + 1) cNW
      back cSW (k - nx - 1) cNE

-- ---------- 道具 ----------
clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs

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

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
