{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-07 ナヴィエ・ストークス方程式を解く
-- 水や空気の動きを決める式（非圧縮性の流体）：
--
--   ∂u/∂t = −(u·∇)u − (1/ρ)∇p + ν∇²u + f　　　∇·u = 0
--            └ 移流 ┘  └ 圧力 ┘  └粘性┘  └外力
--
--   移流：流れが自分自身（速度）を運ぶ。うずが流されるのはこの項のはたらき
--   圧力：流体がつぶれない（∇·u = 0）ように、押し返す
--   粘性：速さの差をならす（06 の拡散と同じ）
--   外力：かき混ぜる力、重力など
-- 格子の上で、1 フレームごとに「① 外力 → ② 粘性（拡散）→ ③ 移流 → ④ 圧力（射影）」の順に解く
-- （Jos Stam の Stable Fluids という方法。どんな Δt でも発散しない）
-- 操作：ドラッグ = かき混ぜる / 1〜4 = 各ステップを ON/OFF / V = 表示切替 / A = 矢印
--       上下キー = 粘性 ν / 左右キー = 時間刻み Δt / F = ファン / R = やり直し
-- ============================================
module Main (main) where

import Art hiding (project)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

forceGain, fanPower, fadeK :: Float
forceGain = 0.5     -- ★ マウスで押す強さ
fanPower = 0.12     -- ★ ファン（左から吹く外力）の強さ
fadeK = 0.997       -- ★ 染料がうすくなる速さ（1 = 消えない）

nx, ny :: Int
nx = 128            -- ★ 格子の数（横）
ny = 80             --   縦は画面（1280 × 800）の形に合わせる

w2, cells :: Int
w2 = nx + 2
cells = w2 * (ny + 2)

data World = World
  { wNu     :: !Float             -- ★ 動粘性係数 ν（大きいほどねばっこい）
  , wDt     :: !Float             -- ★ 時間刻み Δt
  , wOn     :: ![Bool]            -- ① 外力 ② 粘性 ③ 移流 ④ 圧力
  , wView   :: !Int
  , wArrows :: !Bool
  , wFan    :: !Bool
  , wU      :: !(U.Vector Float)  -- 速度 u（横）
  , wV      :: !(U.Vector Float)  -- 速度 v（縦、下向きが正）
  , wP      :: !(U.Vector Float)  -- 圧力 p
  , wDye    :: !(U.Vector Float)  -- 染料
  , wDivB   :: !Float             -- 圧力のステップの前の |∇·u| の最大値
  , wDivA   :: !Float             --                   後
  , wPrev   :: !Point             -- 1 フレーム前のマウス（pmouseX, pmouseY）
  }

viewNames :: [String]
viewNames = ["dye", "vorticity w = curl u", "pressure p", "divergence div u (0 = incompressible)"]

world0 :: World
world0 = restart (World 0.02 1.0 [True, True, True, True] 0 True True U.empty U.empty U.empty U.empty 0 0 (0, 0))

restart :: World -> World
restart w = let z = U.replicate cells 0 in w { wU = z, wV = z, wP = z, wDye = z }

main :: IO ()
main = runSketch (sketch "44-07 Solving the Navier-Stokes equations" world0)
  { skBack  = hsb 215 60 8
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

on :: World -> Int -> Bool
on w k = case drop k (wOn w) of
  (b : _) -> b
  _ -> False

-- 速度と染料を足す場所：(ci, cj, fx, fy, 染料の量, 半径)
type Splat = (Float, Float, Float, Float, Float, Float)

-- ① 外力：マウスのドラッグと、左のファン
forces :: Env -> World -> [Splat]
forces env w =
  [ ( mx / ww * fromIntegral nx + 1, my / hh * fromIntegral ny + 1
    , clampF (-4) 4 ((mx - pmx) / ww * fromIntegral nx * forceGain)
    , clampF (-4) 4 ((my - pmy) / hh * fromIntegral ny * forceGain), 1.0, 4 )
  | envDown env ]
  ++ [ (fromIntegral nx * 0.12, fromIntegral ny * 0.5 + 1, fanPower * cos a * wDt w, fanPower * sin a * wDt w, 0.12, 3)
     | wFan w ]
  where
    ww = envW env
    hh = envH env
    (mx, my) = mouseP env
    (pmx, pmy) = wPrev w
    a = 0.5 * sin (fromIntegral (envFrame env) * 0.012)   -- ゆっくり首をふる

-- ===== ナヴィエ・ストークス方程式を 1 歩（Δt）進める =====
stepFluid :: [Splat] -> World -> World
stepFluid splats w = runST $ do
  u <- U.thaw (wU w)
  v <- U.thaw (wV w)
  p <- U.thaw (wP w)
  dye <- U.thaw (wDye w)
  u0 <- MU.new cells
  v0 <- MU.new cells
  dvg <- MU.new cells
  dye0 <- MU.new cells
  let dt = wDt w
      nu = wNu w
  -- ① 外力 f：u ← u + Δt f
  when (on w 0) $ forM_ splats (splat u v dye)
  -- ② 粘性 ν∇²u：速度を「拡散」させる（06 と同じ式を、こわれない陰解法で解く）
  when (on w 1 && nu > 0) $ do
    MU.copy u0 u
    MU.copy v0 v
    diffuse dt 1 u u0 nu
    diffuse dt 2 v v0 nu
  -- ③ 移流 −(u·∇)u：速度を、速度自身で運ぶ
  when (on w 2) $ do
    MU.copy u0 u
    MU.copy v0 v
    advect dt 1 u u0 u0 v0
    advect dt 2 v v0 u0 v0
  -- ④ 圧力 −∇p：∇·u = 0 になるような p を求めて、そのこう配を引く
  db <- maxDivergence u v
  when (on w 3) $ project u v p dvg 30
  da <- maxDivergence u v
  -- 染料（流れを見るための色）は速度に運ばれるだけ
  MU.copy dye0 dye
  advect dt 0 dye dye0 u v
  for 0 (cells - 1) $ \k -> do
    modify' dye k (* fadeK)
    modify' u k (safe 30)   -- 念のための安全装置
    modify' v k (safe 30)
  u' <- U.freeze u
  v' <- U.freeze v
  p' <- U.freeze p
  d' <- U.freeze dye
  pure w { wU = u', wV = v', wP = p', wDye = d', wDivB = db, wDivA = da }

-- 場所 (ci, cj) のまわり半径 r に、速度 (fx, fy) と染料 amount を足す
splat :: Grid s -> Grid s -> Grid s -> Splat -> ST s ()
splat u v dye (ci, cj, fx, fy, amount, r) =
  forM_ [max 1 (floor (cj - 2 * r)) .. min ny (floor (cj + 2 * r))] $ \j ->
    forM_ [max 1 (floor (ci - 2 * r)) .. min nx (floor (ci + 2 * r))] $ \i -> do
      let wt = exp (-((fromIntegral i - ci) ^ (2 :: Int) + (fromIntegral j - cj) ^ (2 :: Int)) / (r * r))
          n = ix i j
      modify' u n (+ fx * wt)
      modify' v n (+ fy * wt)
      modify' dye n (\d -> min (d + amount * wt) 3)

maxDivergence :: Grid s -> Grid s -> ST s Float
maxDivergence u v = go 2 2 0
  where
    go !i !j !big
      | j >= ny = pure big
      | i >= nx = go 2 (j + 1) big
      | otherwise = do
          let n = ix i j
          a <- rd u (n + 1)
          b <- rd u (n - 1)
          c <- rd v (n + w2)
          d <- rd v (n - w2)
          go (i + 1) j (max big (abs (0.5 * (a - b + c - d))))

step :: Env -> Float -> World -> IO World
step env _ w = pure (stepFluid (forces env w) w) { wPrev = mouseP env }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) (pixelImage nx ny 1 (render w))
  , view env $
      -- 速度の矢印
      [ Color (hsbA 0 0 100 45) (Pictures
          [ Line [(x, y), (x + clampF (-30) 30 (wU w U.! n * 14), y + clampF (-30) 30 (wV w U.! n * 14))]
          | wArrows w, j <- [4, 10 .. ny], i <- [4, 10 .. nx]
          , let n = ix i j, let x = (fromIntegral i - 0.5) * cw, let y = (fromIntegral j - 0.5) * ch ]) ]
      ++ hud env w
  ]
  where
    cw = envW env / fromIntegral nx
    ch = envH env / fromIntegral ny

-- 式を、項ごとに色分けして書く（OFF の項は暗くする）
hud :: Env -> World -> [Picture]
hud env w =
  [ rectTL (hsbA 215 60 8 78) 0 0 (envW env) 128
  , txt 20 42 22 (hsb 0 0 100) "du/dt =" ]
  ++ concat
     [ [ txt x 42 22 (hsb hue 60 (if on w k then 100 else 35)) term
       , txt x 64 14 (hsb hue 60 (if on w k then 100 else 35))
           (show (k + 1) ++ " " ++ (names !! k) ++ (if on w k then " ON" else " OFF")) ]
     | (n, (term, k, hue)) <- zip [0 :: Int ..] terms, let x = 125 + fromIntegral n * 175 ]
  ++ [ txt 770 42 22 (hsb 12 60 (if on w 3 then 100 else 35)) "div u = 0"
     , txt 20 94 16 (hsb 0 0 100)
         ("Navier-Stokes equations   nu = " ++ fx 3 (wNu w) ++ " (Up/Down)   dt = " ++ fx 2 (wDt w)
          ++ " (Left/Right)   view: " ++ (viewNames !! wView w) ++ " (V)")
     , txt 20 116 14 (hsb 0 0 70)
         ("max |div u|: before pressure step " ++ fx 4 (wDivB w) ++ " -> after " ++ fx 4 (wDivA w)
          ++ "    drag = stir   F = fan   A = arrows   R = reset") ]
  where
    terms = [("-(u.grad)u", 2, 185), ("- grad p/rho", 3, 12), ("+ nu lap u", 1, 42), ("+ f", 0, 130)] :: [(String, Int, Float)]
    names = ["force", "viscosity", "advection", "pressure"]

-- 格子の値を色にする
render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w x y =
  let n = ix (x + 1) (y + 1)
      u = wU w
      v = wV w
  in case wView w of
       0 -> let d = min (wDye w U.! n) 1.6   -- 染料：紺 → 水色 → 白
            in (b8 (8 + 150 * d * d * 0.6), b8 (16 + 190 * d), b8 (34 + 220 * d))
       vn -> -- 正 = 赤（さんご色）、負 = 青（水色）
         let s0 = case vn of
               1 -> 0.5 * (v U.! (n + 1) - v U.! (n - 1) - u U.! (n + w2) + u U.! (n - w2)) * 6
               2 -> wP w U.! n * 1.5
               _ -> -0.5 * (u U.! (n + 1) - u U.! (n - 1) + v U.! (n + w2) - v U.! (n - w2)) * 12
             a = min (abs s0) 1
             s = if vn == 1 then -s0 else s0   -- 画面は y が下向きなので、反時計回りを赤にする
         in if s > 0 then (b8 (8 + 247 * a), b8 (16 + 110 * a), b8 (34 + 70 * a))
                     else (b8 (8 + 20 * a), b8 (16 + 200 * a), b8 (34 + 221 * a))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k >= '1' && k <= '4' -> let m = fromEnum k - fromEnum '1'
                              in w { wOn = [ if i == m then not b else b | (i, b) <- zip [0 ..] (wOn w) ] }
    | k `elem` "vV" -> w { wView = (wView w + 1) `mod` 4 }
    | k `elem` "aA" -> w { wArrows = not (wArrows w) }
    | k `elem` "fF" -> w { wFan = not (wFan w) }
    | k `elem` "rR" -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wNu = min 2.56 (if wNu w == 0 then 0.005 else wNu w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wNu = if wNu w <= 0.005 then 0 else wNu w / 2 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wDt = min 4 (wDt w * 1.25) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wDt = max 0.2 (wDt w / 1.25) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ドラッグしてうずを作る。V で渦度に切り替えると、うずが赤（反時計回り）と青のペアで生まれるのが見える
-- 2. 4 を押して圧力を OFF。V で「発散」を見ると、流体がたまる所・うすくなる所ができる（つぶれる流体）
-- 3. 3 を押して移流を OFF。うずが動かなくなり、ただ広がって消える（ストークス流れ ＝ とてもねばい流れ）
-- 4. ν を 0 → 0.64 と変える。水 → はちみつ。うずの寿命はどう変わる？
-- 5. Δt を 4 にしてもこわれない（無条件安定）。でも動きはぼやける。なぜ？（さかのぼる距離が長い）
-- 6. stepFluid に浮力を足そう：染料のある所を上へ押す（v の n 番目から 0.02 × dye の n 番目を引く）

-- ---------- 道具（解くための道具） ----------
-- 格子は (nx + 2) × (ny + 2)。まわり 1 マスは壁（境界）
type Grid s = MU.MVector s Float

ix :: Int -> Int -> Int
ix i j = i + w2 * j

rd :: Grid s -> Int -> ST s Float
rd = MU.unsafeRead

wr :: Grid s -> Int -> Float -> ST s ()
wr = MU.unsafeWrite

modify' :: Grid s -> Int -> (Float -> Float) -> ST s ()
modify' g n f = rd g n >>= \x -> wr g n $! f x

-- lo から hi まで順に（for 文）
for :: Int -> Int -> (Int -> ST s ()) -> ST s ()
for lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

-- 大きすぎる値と NaN をおさえる
safe :: Float -> Float -> Float
safe m x | isNaN x = 0
         | otherwise = max (-m) (min m x)

-- ② 拡散：(1 − νΔt∇²) x = x0 を解く（未来の値どうしで平均するので、ν が大きくても安定）
diffuse :: Float -> Int -> Grid s -> Grid s -> Float -> ST s ()
diffuse dt b x x0 k = let a = dt * k in linSolve b x x0 a (1 + 4 * a) 12

-- ③ 移流：各マスで「Δt 前にここへ来たはずの場所」へさかのぼり、そこの値をもらう（セミ・ラグランジュ法）
advect :: Float -> Int -> Grid s -> Grid s -> Grid s -> Grid s -> ST s ()
advect !dt b !d !d0 !uu !vv = do
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

-- ④ 射影：∇²p = ∇·u を解き、u ← u − ∇p。これで ∇·u = 0（入った分だけ出ていく）になる
project :: Grid s -> Grid s -> Grid s -> Grid s -> Int -> ST s ()
project !u !v !p !dvg iters = do
  for 1 ny $ \j -> for 1 nx $ \i -> do
    let n = ix i j
    a <- rd u (n + 1)
    b <- rd u (n - 1)
    c <- rd v (n + w2)
    d <- rd v (n - w2)
    wr dvg n (-0.5 * (a - b + c - d))
  setBnd 0 dvg
  linSolve 0 p dvg 1 4 iters   -- 前のフレームの p を初期値にして反復
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

-- 連立方程式を反復で解く（ガウス・ザイデル法）：x = (x0 + a × まわり 4 マスの合計) / c をくり返す
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
