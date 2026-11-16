{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-14 墨流し（マーブリング）ブラシ
-- 水面に墨や絵の具を落とし、竹串でそっと引くと、流れの形がそのまま模様になる（日本の「墨流し」、トルコの「エブル」）
-- ここでは 07 のナヴィエ・ストークス方程式のソルバーに色インクを流して描く
--   インクは紙の色から光を「吸う」ので、重ねるほど暗くなる（絵の具の混色）
-- 操作：ドラッグ = インクを流す / 1〜5 = 色 / T = 竹串（インクなしで流れだけ作る）/ スペース = くし（櫛）を通す
--       上下キー = 筆の太さ / 左右キー = 粘性 / S = 画像を保存 / C = 全部消す
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
dt = 1.0

push1, settle, inkAmount :: Float
push1 = 0.45        -- ★ ドラッグで水を押す強さ
settle = 0.985      -- ★ 水が静まる速さ（1 = いつまでも動く、0.9 = すぐ止まる）
inkAmount = 0.5     -- ★ 1 フレームに出るインクの量

paper :: (Float, Float, Float)
paper = (244, 238, 224)   -- ★ 紙の色

-- ★ インク：(赤を吸う量, 緑を吸う量, 青を吸う量)。全部吸えば黒、青だけ吸えば黄色に見える
inks :: [(Float, Float, Float)]
inks = [(1.0, 1.0, 1.0), (1.0, 0.65, 0.15), (0.05, 0.9, 0.85), (0.0, 0.25, 0.95), (0.85, 0.1, 0.75)]

inkNames :: [String]
inkNames = ["sumi (black ink)", "ai (indigo)", "shu (vermilion)", "yamabuki (yellow)", "wakatake (green)"]

nx, ny :: Int
nx = 160            -- ★ 格子の数（横）
ny = 100            --   縦は画面（1280 × 800）の形に合わせる

w2, cells :: Int
w2 = nx + 2
cells = w2 * (ny + 2)

data World = World
  { wNu    :: !Float              -- ★ 動粘性係数 ν（左右キー。大きいと模様が動かない）
  , wBrush :: !Float              -- ★ 筆の太さ [マス]
  , wInk   :: !Int
  , wStick :: !Bool               -- 竹串（インクなし）
  , wComb  :: !Float              -- くしの位置（-1 = 使っていない）
  , wU     :: !(U.Vector Float)
  , wV     :: !(U.Vector Float)
  , wP     :: !(U.Vector Float)
  , wDyeR  :: !(U.Vector Float)
  , wDyeG  :: !(U.Vector Float)
  , wDyeB  :: !(U.Vector Float)
  , wPrev  :: !Point
  }

world0 :: World
world0 = wipe (World 0.0005 2.5 0 False (-1) U.empty U.empty U.empty U.empty U.empty U.empty (0, 0))

wipe :: World -> World
wipe w = let z = U.replicate cells 0 in w { wU = z, wV = z, wP = z, wDyeR = z, wDyeG = z, wDyeB = z }

main :: IO ()
main = runSketch (sketch "44-14 Suminagashi marbling brush" world0)
  { skBack  = rgb255 244 238 224
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 筆の 1 押し：(ci, cj, fx, fy, インク（なければ Nothing）, 量)
type Splat = (Float, Float, Float, Float, Maybe (Float, Float, Float), Float)

-- 場所 (ci, cj) に、速度 (fx, fy) とインク（色 ink、量 amount）を足す
splat :: Float -> Grid s -> Grid s -> Grid s -> Grid s -> Grid s -> Splat -> ST s ()
splat r u v dr dg db (ci, cj, fx, fy, ink, amount) =
  forM_ [max 1 (floor (cj - 2.5 * r)) .. min ny (floor (cj + 2.5 * r))] $ \j ->
    forM_ [max 1 (floor (ci - 2.5 * r)) .. min nx (floor (ci + 2.5 * r))] $ \i -> do
      let wt = exp (-((fromIntegral i - ci) ^ (2 :: Int) + (fromIntegral j - cj) ^ (2 :: Int)) / (r * r))
          n = ix i j
      modify' u n (+ fx * wt)
      modify' v n (+ fy * wt)
      forM_ ink $ \(a, b, c) -> do
        modify' dr n (\d -> min (d + a * amount * wt) 4)
        modify' dg n (\d -> min (d + b * amount * wt) 4)
        modify' db n (\d -> min (d + c * amount * wt) 4)

step :: Env -> Float -> World -> IO World
step env _ w = pure $
  let (mx, my) = mouseP env
      (pmx, pmy) = wPrev w
      ww = envW env
      hh = envH env
      br = wBrush w
      -- 筆（マウス）：動かした向きに水を押し、インクを置く
      dx = (mx - pmx) / ww * fromIntegral nx
      dy = (my - pmy) / hh * fromIntegral ny
      steps = max 1 (ceiling (sqrt (dx * dx + dy * dy) / br)) :: Int   -- 速く動かしても線がとぎれないように
      ink = if wStick w then Nothing else Just (inks !! wInk w)
      brush = [ ( lerpF pmx mx s / ww * fromIntegral nx + 1, lerpF pmy my s / hh * fromIntegral ny + 1
                , clampF (-6) 6 dx * push1 / fromIntegral steps, clampF (-6) 6 dy * push1 / fromIntegral steps
                , ink, inkAmount / fromIntegral steps )
              | envDown env && my > 0, k <- [1 .. steps], let s = fromIntegral k / fromIntegral steps ]
      -- くし：等間かくの歯が左から右へ通りぬける
      comb = [ (wComb w, fromIntegral j, 0.5, 0, Nothing, 0) | wComb w >= 0, j <- [6, 18 .. ny - 1] ]
      combX = if wComb w >= 0 then (let c = wComb w + 1.5 in if c > fromIntegral nx then -1 else c) else -1
      (u', v', p', r', g', b') = runST $ do
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
        forM_ (brush ++ comb) (splat br u v dr dg db)
        stepVelocity (wNu w) u v u0 v0 p dvg   -- ナヴィエ・ストークス方程式を 1 歩
        for 0 (cells - 1) $ \k -> modify' u k (* settle) >> modify' v k (* settle)
        -- インク 3 色ぶんを流れで運ぶ
        forM_ [dr, dg, db] $ \d -> MU.copy tmp d >> advect 0 d tmp u v
        (,,,,,) <$> U.freeze u <*> U.freeze v <*> U.freeze p <*> U.freeze dr <*> U.freeze dg <*> U.freeze db
  in w { wU = u', wV = v', wP = p', wDyeR = r', wDyeG = g', wDyeB = b', wComb = combX, wPrev = (mx, my) }

-- 紙の色 × インクが吸った残り
canvasPic :: Env -> World -> Picture
canvasPic env w =
  Scale (envW env / fromIntegral nx) (envH env / fromIntegral ny) $ pixelImage nx ny 1 $ \x y ->
    let n = ix (x + 1) (y + 1)
        (pr, pg, pb) = paper
    in (b8 (pr * exp (-1.6 * wDyeR w U.! n)), b8 (pg * exp (-1.6 * wDyeG w U.! n)), b8 (pb * exp (-1.6 * wDyeB w U.! n)))

draw :: Env -> World -> Picture
draw env w = Pictures
  [ canvasPic env w
  , view env $
      [ txt 16 26 15 (hsbA 0 0 15 70)
          ("1-5 color: " ++ (inkNames !! wInk w) ++ "   T stick " ++ (if wStick w then "ON (flow only)" else "OFF")
           ++ "   space = comb   brush " ++ showFFloat (Just 1) (wBrush w) "" ++ " (Up/Down)   nu = "
           ++ showFFloat (Just 4) (wNu w) "" ++ " (Left/Right)   S save   C clear") ]
      ++ concat [ [ dot (swatch ink) (30 + fromIntegral k * 34) 52 24
                  , Color (hsb 0 0 (if k == wInk w then 10 else 70)) (Translate (30 + fromIntegral k * 34) 52 (ring 12 (if k == wInk w then 3 else 1))) ]
                | (k, ink) <- zip [0 :: Int ..] inks ]
  ]
  where
    swatch (a, b, c) = let (pr, pg, pb) = paper
                       in makeColor (pr * exp (-1.6 * a) / 255) (pg * exp (-1.6 * b) / 255) (pb * exp (-1.6 * c) / 255) 1

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char k) Down _ _
    | k >= '1' && k <= '5' -> pure w { wInk = fromEnum k - fromEnum '1', wStick = False }
    | k `elem` "tT" -> pure w { wStick = not (wStick w) }
    | k `elem` "sS" -> saveSvg "suminagashi" env (canvasPic env w) >> pure w   -- 文字は入れずに保存
    | k `elem` "cC" -> pure (wipe w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wComb = 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wBrush = min 6 (wBrush w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wBrush = max 1 (wBrush w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wNu = min 0.256 (wNu w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wNu = max 0.0005 (wNu w / 2) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 墨で大きく円を描き、藍・朱を中に落とす。T で竹串にして、外から中へ・中から外へ引いてみよう
-- 2. 色の帯を横に何本か引いてからスペース（くし）。もう一度、こんどは縦にマウスで引くと「矢羽根」模様
-- 3. ν を大きくすると（右キー）筆のあとがすぐ止まる。小さいと長く流れ続ける。好みの「水」を見つけよう
-- 4. inks に自分の色を足す。(0, 0, 1) は何色？ (1, 0, 0) は？（吸う色の反対の色に見える）
-- 5. paper を (20, 24, 40) にして、inks を負の数（光る絵の具）にすると夜空のような絵になる
-- 6. settle を 1 にして、step の中で v から 0.002 × dyeR を引く（インクのある所を上へ）と、けむりのような絵になる

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
