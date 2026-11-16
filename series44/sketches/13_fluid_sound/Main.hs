{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-13 流れを聴く 🔊
-- ナヴィエ・ストークス方程式（07 のソルバー）で作った流れの中に「観測点」を置き、音に変える
--   渦が観測点を通りすぎる（渦度が大きくなる）→ ポーンと 1 音。音の高さはその場所の流速で決まる
--   まん中の観測点の流速 → 低く続く音（ドローン）の高さと大きさ
-- 風の音、川のせせらぎ、笛の音は、どれも流れの中の渦が作っている（09 の St = fD/U を思い出そう）
-- 操作：ドラッグ = かき混ぜる / A = 自動でかき混ぜる / D = ドローン / V = 表示切替
--       上下キー = 粘性 ν / R = やり直し
-- ============================================
module Main (main) where

import Art hiding (project)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

dt :: Float
dt = 1.0            -- 時間刻み

threshold :: Float
threshold = 0.05    -- ★ 渦と見なす渦度の大きさ（小さいほどよく鳴る）

scaleNotes :: [Int]
scaleNotes = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21]   -- ★ 音階（半音の数。これはペンタトニック）

baseFreq :: Float
baseFreq = 196      -- ★ いちばん低い音 [Hz]

nProbe :: Int
nProbe = 6          -- ★ 観測点の数

stirPower :: Float
stirPower = 0.2     -- ★ 自動のかき混ぜ棒の強さ

droneEvery :: Float
droneEvery = 0.5    -- ★ ドローンを鳴らし直す間かく [秒]（続く音の代わりに短い音をくり返す）

nx, ny :: Int
nx = 112            -- ★ 格子の数（横）
ny = 70             --   縦は画面（1280 × 800）の形に合わせる

w2, cells :: Int
w2 = nx + 2
cells = w2 * (ny + 2)

-- 観測点：場所、鳴らせる状態か、光っている強さ、渦度、流速
data Probe = Probe { pI :: !Int, pJ :: !Int, pArmed :: !Bool, pFlash :: !Float, pW :: !Float, pSp :: !Float }

data World = World
  { wNu     :: !Float             -- ★ 動粘性係数 ν（大きくすると渦がすぐ消えて静かになる）
  , wU      :: !(U.Vector Float)
  , wV      :: !(U.Vector Float)
  , wP      :: !(U.Vector Float)
  , wDye    :: !(U.Vector Float)
  , wProbes :: ![Probe]
  , wAuto   :: !Bool
  , wDrone  :: !Bool
  , wView   :: !Int               -- 0 = 渦度, 1 = 染料
  , wStirT  :: !Float
  , wPrev   :: !Point
  , wDroneT :: !Float             -- 最後にドローンを鳴らした時刻
  }

world0 :: World
world0 = restart (World 0.005 U.empty U.empty U.empty U.empty [] True True 0 0 (0, 0) (-10))

restart :: World -> World
restart w =
  let z = U.replicate cells 0
      -- 観測点を輪のように並べる
      ps = [ Probe (round (fromIntegral nx / 2 + fromIntegral nx * 0.28 * cos a :: Float))
                   (round (fromIntegral ny / 2 + fromIntegral ny * 0.3 * sin a :: Float)) True 0 0 0
           | k <- [0 .. nProbe - 1], let a = fromIntegral k * 2 * pi / fromIntegral nProbe ]
  in w { wU = z, wV = z, wP = z, wDye = z, wProbes = ps }

main :: IO ()
main = runSketch (sketch "44-13 Listening to the flow" world0)
  { skBack  = hsb 215 60 8
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- かき混ぜる：(ci, cj) のまわりに速度 (fx, fy) と染料 amount を足す
type Stir = (Float, Float, Float, Float, Float)

stir :: Grid s -> Grid s -> Grid s -> Stir -> ST s ()
stir u v dye (ci, cj, fx, fy, amount) =
  forM_ [max 1 (floor (cj - 6)) .. min ny (floor (cj + 6))] $ \j ->
    forM_ [max 1 (floor (ci - 6)) .. min nx (floor (ci + 6))] $ \i -> do
      let wt = exp (-((fromIntegral i - ci) ^ (2 :: Int) + (fromIntegral j - cj) ^ (2 :: Int)) / 9)
          n = ix i j
      modify' u n (+ fx * wt)
      modify' v n (+ fy * wt)
      modify' dye n (\d -> min (d + amount * wt) 2)

step :: Env -> Float -> World -> IO World
step env _ w = do
  let (mx, my) = mouseP env
      (pmx, pmy) = wPrev w
      ww = envW env
      hh = envH env
      -- 外力：マウスと、自動のかき混ぜ棒（8 の字に動く）
      st = wStirT w + (if wAuto w then 0.02 else 0)
      mouseStir = [ ( mx / ww * fromIntegral nx + 1, my / hh * fromIntegral ny + 1
                    , clampF (-4) 4 ((mx - pmx) / ww * fromIntegral nx * 0.5)
                    , clampF (-4) 4 ((my - pmy) / hh * fromIntegral ny * 0.5), 0.8 ) | envDown env ]
      autoStir = [ ( fromIntegral nx * (0.5 + 0.36 * sin st), fromIntegral ny * (0.5 + 0.34 * sin (2 * st + 0.6))
                   , 0.36 * fromIntegral nx * 0.02 * cos st * stirPower           -- 棒の速度
                   , 0.34 * fromIntegral ny * 0.04 * cos (2 * st + 0.6) * stirPower, 0.25 ) | wAuto w ]
      (u', v', p', d') = runST $ do
        u <- U.thaw (wU w)
        v <- U.thaw (wV w)
        p <- U.thaw (wP w)
        dye <- U.thaw (wDye w)
        u0 <- MU.new cells
        v0 <- MU.new cells
        dvg <- MU.new cells
        dye0 <- MU.new cells
        forM_ (mouseStir ++ autoStir) (stir u v dye)
        stepVelocity (wNu w) u v u0 v0 p dvg
        MU.copy dye0 dye
        advect 0 dye dye0 u v
        for 0 (cells - 1) $ \k -> modify' dye k (* 0.995)
        (,,,) <$> U.freeze u <*> U.freeze v <*> U.freeze p <*> U.freeze dye
      -- ★ 観測点：渦度 ω と流速を読み、音にする
      readProbe q =
        let n = ix (pI q) (pJ q)
            om = 0.5 * (v' U.! (n + 1) - v' U.! (n - 1) - u' U.! (n + w2) + u' U.! (n - w2))
            sp = sqrt (u' U.! n * u' U.! n + v' U.! n * v' U.! n)
            hit = pArmed q && abs om > threshold   -- 渦が来た！
            armed = if abs om < threshold * 0.5 then True else pArmed q && not hit   -- 渦が去ったら次を待つ
        in (q { pW = om, pSp = sp, pArmed = armed, pFlash = (if hit then 1 else pFlash q) * 0.92 }, hit)
      probes = map readProbe (wProbes w)
  forM_ probes $ \(q, hit) -> when hit $ do
    let idx = floor (clampF 0 0.999 (pSp q * 6) * fromIntegral (length scaleNotes))   -- 速いほど高い音
        note = case drop idx scaleNotes of
          (s : _) -> s
          _ -> 0
    -- ω > 0 の渦（画面では時計回り、青）はやわらかいサイン波、ω < 0 の渦（赤）は三角波
    playTone (baseFreq * 2 ** (fromIntegral note / 12)) (clampF 0.15 0.6 (abs (pW q) * 4))
             (if pW q > 0 then 1.2 else 0.7) (if pW q > 0 then Sine else Triangle)
  -- まん中の流速 → ドローン（短い音をくり返す）
  let c = ix (round (fromIntegral nx / 2 :: Float)) (round (fromIntegral ny / 2 :: Float))
      cs = sqrt (u' U.! c * u' U.! c + v' U.! c * v' U.! c)
      now = envTime env
      dVol = clampF 0 0.25 (cs * 1.5)
      due = wDrone w && now - wDroneT w >= droneEvery && dVol > 0.01
  when due $
    playTone (fromIntegral (round (clampF 30 2000 (55 + cs * 400)) :: Int))
             (fromIntegral (round (dVol * 20) :: Int) / 20) (droneEvery + 0.1) Triangle
  pure w { wU = u', wV = v', wP = p', wDye = d', wProbes = map fst probes, wStirT = st, wPrev = (mx, my)
         , wDroneT = if due then now else wDroneT w }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) (pixelImage nx ny 1 (render w))
  , view env $
      concat [ [ Color (hsbA 42 80 100 (50 + 50 * pFlash q)) (Translate x y (ring (11 + 25 * pFlash q) (2 + 4 * pFlash q)))
               , Color (hsb 42 80 100) (thickLine 2 (x, y) (x + clampF (-50) 50 (u U.! n * 60), y + clampF (-50) 50 (v U.! n * 60))) ]
             | q <- wProbes w
             , let n = ix (pI q) (pJ q)
             , let x = (fromIntegral (pI q) - 0.5) / fromIntegral nx * envW env
             , let y = (fromIntegral (pJ q) - 0.5) / fromIntegral ny * envH env ]
      ++ [ Color (hsb 185 70 100) (Translate (envW env / 2) (envH env / 2) (ring (8 + cs * 100) 2))
         , rectTL (hsbA 215 60 8 78) 0 0 (envW env) 100
         , txt 20 38 18 (hsb 0 0 100)
             ("drag = stir   A = auto " ++ onOff (wAuto w) ++ "   D = drone " ++ onOff (wDrone w) ++ "   V = view ("
              ++ (if wView w == 1 then "dye" else "vorticity") ++ ")   nu = " ++ showFFloat (Just 4) (wNu w) "" ++ " (Up/Down)   R")
         , txt 20 66 15 (hsb 0 0 80) "Listen to a solution of the Navier-Stokes equations  du/dt + (u.grad)u = -grad p/rho + nu lap u ,  div u = 0"
         , txt 20 90 15 (hsb 0 0 65)
             ("yellow circles = probes (ring when a vortex passes; red and blue vortices sound different)   cyan = center speed (drone "
              ++ show (round (55 + cs * 400) :: Int) ++ " Hz)") ]
  ]
  where
    u = wU w
    v = wV w
    c = ix (round (fromIntegral nx / 2 :: Float)) (round (fromIntegral ny / 2 :: Float))
    cs = sqrt (u U.! c * u U.! c + v U.! c * v U.! c)
    onOff b = if b then "ON" else "OFF"

render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w x y =
  let n = ix (x + 1) (y + 1)
      u = wU w
      v = wV w
  in if wView w == 1
       then let d = min (wDye w U.! n) 1.6
            in (b8 (8 + 150 * d * d * 0.6), b8 (16 + 190 * d), b8 (34 + 220 * d))
       else let s = -0.5 * (v U.! (n + 1) - v U.! (n - 1) - u U.! (n + w2) + u U.! (n - w2)) * 7
                a = min (abs s) 1
            in if s > 0 then (b8 (8 + 247 * a), b8 (16 + 110 * a), b8 (34 + 70 * a))
                        else (b8 (8 + 20 * a), b8 (16 + 200 * a), b8 (34 + 221 * a))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "aA" -> w { wAuto = not (wAuto w) }
    | k `elem` "dD" -> w { wDrone = not (wDrone w) }
    | k `elem` "vV" -> w { wView = 1 - wView w }
    | k `elem` "rR" -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wNu = min 0.32 (wNu w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wNu = max 0.00125 (wNu w / 2) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. A で自動を止め、自分でゆっくり・すばやくかき混ぜる。鳴り方（数・高さ）はどう変わる？
-- 2. ν を上げる（上キー）。渦がすぐ消えて静かになる。はちみつの中は静か？
-- 3. threshold を 0.02 にするとにぎやかに、0.15 にすると強い渦だけが鳴る
-- 4. scaleNotes を [0, 3, 5, 6, 7, 10, 12]（ブルース）や [0, 2, 3, 7, 8, 12]（日本の音階）に変える
-- 5. nProbe を 12 にして、渦が輪をひとまわりするとメロディーになるようにしてみよう
-- 6. 音の高さを「流速」ではなく「観測点の番号」で決めると？（restart で番号を Probe に持たせる）

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
