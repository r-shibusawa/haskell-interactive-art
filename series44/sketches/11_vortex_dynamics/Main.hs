{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-11 点渦の力学
-- 渦を「強さ Γ をもつ 1 点」で表す。点渦は自分では動かず、ほかの渦が作る流れに流される
--   渦 j が距離 r の所に作る速さ = Γ_j / (2π r)（向きは渦のまわりを回る向き）
-- 粘性のない流体では渦は消えない（ヘルムホルツの渦定理）ので、渦どうしの追いかけっこが続く
--   同じ向きの 2 つ → たがいのまわりを回る　　逆向きの 2 つ（渦対）→ 並んでまっすぐ進む
--   渦対が 2 組 → 後ろの組が前の組をくぐり抜ける「蛙飛び」（煙の輪でも起きる）
-- 操作：クリック = 渦を置く / 1 = ＋（反時計回り）・2 = −（時計回り）/ 3〜6 = お手本 / スペース = 停止 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, replicateM)
import Control.Monad.ST (runST)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

gam :: Float
gam = 40000         -- ★ 渦の強さ Γ [px²/秒]

core :: Float
core = 12           -- ★ 渦の芯の大きさ [px]（近づきすぎたとき速さが無限大にならないように）

nTracer :: Int
nTracer = 1400      -- ★ 流れを見る粒の数

fadeA :: Float
fadeA = 0.14        -- ★ 1 フレームで軌跡が背景色に近づく割合（p5 の fill(..., 14) と同じ）

-- 軌跡を残す画像（画面の 1/4 の大きさで持ち、4 倍にして表示する）
tk :: Int
tk = 4
bw, bh :: Int
bw = 1280 `div` tk
bh = 800 `div` tk

-- 背景の色 hsb(215, 60, 10) の RGB（0〜255）
bgR, bgG, bgB :: Float
bgR = 10; bgG = 16; bgB = 26

data Vortex = Vortex { vX :: !Float, vY :: !Float, vG :: !Float }

data World = World
  { wSign   :: !Float
  , wPaused :: !Bool
  , wVort   :: ![Vortex]
  , wDots   :: ![Point]
  , wName   :: !String
  , wTrail  :: !(U.Vector Float)   -- bw × bh × 3 の色（0〜255）
  }

main :: IO ()
main = runSketch (sketch "44-11 Point vortex dynamics" (preset 1280 800 5 world0))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ds <- replicateM nTracer ((,) <$> randomRIO (0, envW env) <*> randomRIO (0, envH env))
      pure (preset (envW env) (envH env) 5 world0 { wDots = ds })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 False [] [] "" (U.generate (bw * bh * 3) (\i -> [bgR, bgG, bgB] !! (i `mod` 3)))

preset :: Float -> Float -> Int -> World -> World
preset w h k wd =
  let cx = w / 2
      cy = h / 2 + 30
  in case k of
       3 -> wd { wVort = [Vortex (cx - 70) cy 1, Vortex (cx + 70) cy 1]
               , wName = "Two of the same sign: they keep circling their common center" }
       4 -> wd { wVort = [Vortex (cx - 300) (cy - 55) 1, Vortex (cx - 300) (cy + 55) (-1)]
               , wName = "Vortex pair (opposite signs): each carries the other the same way, so they move straight" }
       5 -> wd { wVort = concat [ [Vortex (cx - 380 + j * 110) (cy - 70) 1, Vortex (cx - 380 + j * 110) (cy + 70) (-1)] | j <- [0, 1] ]
               , wName = "Leapfrogging: the rear pair narrows, speeds up and passes through the front pair" }
       _ -> wd { wVort = concat [ [Vortex (cx - 400 + j * 200) (cy - 28) 1, Vortex (cx - 300 + j * 200) (cy + 28) (-1)] | j <- [0 .. 4] ]
               , wName = "Point-vortex model of a Karman street (09): two staggered rows drift together" }

-- ★ 場所 (x, y) での流れの速度：すべての点渦が作る速度の足し算（skip 番の渦は自分なので除く）
velocityAt :: [Vortex] -> Int -> Float -> Float -> Point
velocityAt vs skip x y = foldl' add (0, 0) (zip [0 ..] vs)
  where
    add (!vx, !vy) (k, Vortex wx wy g)
      | k == skip = (vx, vy)
      | otherwise =
          let dx = x - wx
              dy = y - wy
              s = g * gam / (2 * pi * (dx * dx + dy * dy + core * core))
          in (vx + s * dy, vy - s * dx)   -- 画面では y が下向きなので、g = +1 が反時計回りに見える

-- 渦を dt だけ進める（4 次のルンゲ・クッタ法：長く回してもずれにくい）
moveVortices :: Float -> Float -> Float -> [Vortex] -> [Vortex]
moveVortices w h dt vs =
  let vel ps = [ velocityAt ps k (vX p) (vY p) | (k, p) <- zip [0 ..] ps ]
      shift c ks = [ v { vX = vX v + c * dt * a, vY = vY v + c * dt * b } | (v, (a, b)) <- zip vs ks ]
      k1 = vel vs
      k2 = vel (shift 0.5 k1)
      k3 = vel (shift 0.5 k2)
      k4 = vel (shift 1 k3)
      comb (a1, b1) (a2, b2) (a3, b3) (a4, b4) = ((a1 + 2 * a2 + 2 * a3 + a4) / 6, (b1 + 2 * b2 + 2 * b3 + b4) / 6)
  in [ v { vX = wrapF w (vX v + dt * a), vY = wrapF h (vY v + dt * b) }   -- 画面の端から出たら反対側へ
     | (v, (a, b)) <- zip vs (zipWith4' comb k1 k2 k3 k4) ]

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let dt = if wPaused w0 then 0 else 1 / 60
      ww = envW env
      hh = envH env
      vs = iterate (moveVortices ww hh (dt / 4)) (wVort w0) !! 4
  -- 流れに乗る粒（中点法）
  ds <- mapM (\(x0, y0) -> do
      let (ax, ay) = velocityAt vs (-1) x0 y0
          (bx, by) = velocityAt vs (-1) (x0 + ax * dt / 2) (y0 + ay * dt / 2)
          x = x0 + bx * dt
          y = y0 + by * dt
      r <- randomRIO (0, 1 :: Float)
      if x < 0 || x > ww || y < 0 || y > hh || r < 0.002 || isNaN x || isNaN y
        then (,) <$> randomRIO (0, ww) <*> randomRIO (0, hh)
        else pure (x, y)) (wDots w0)
  pure w0 { wVort = vs, wDots = ds, wTrail = fadeAndStamp env ds vs (wTrail w0) }

-- 前のフレームをうすく残して、粒の軌跡（尾）を見せる：背景色へ少し近づけてから、粒と渦を書きこむ
fadeAndStamp :: Env -> [Point] -> [Vortex] -> U.Vector Float -> U.Vector Float
fadeAndStamp env ds vs old = runST $ do
  buf <- U.thaw old
  let bg = U.fromList [bgR, bgG, bgB]
  forM_ [0 .. bw * bh * 3 - 1] $ \i -> do
    c <- MU.unsafeRead buf i
    MU.unsafeWrite buf i (c + fadeA * (bg U.! (i `mod` 3) - c))
  let sx = fromIntegral bw / envW env
      sy = fromIntegral bh / envH env
      put (r, g, b) a i j =
        if i >= 0 && i < bw && j >= 0 && j < bh
          then forM_ (zip [0, 1, 2] [r, g, b]) $ \(ch, v) -> do
                 let k = (i + j * bw) * 3 + ch
                 c <- MU.unsafeRead buf k
                 MU.unsafeWrite buf k (c + a * (v - c))
          else pure ()
  -- 粒：hsb(190, 40, 100) を透明度 80%
  forM_ ds $ \(x, y) -> put (153, 230, 255) 0.8 (floor (x * sx)) (floor (y * sy))
  -- 渦の円（直径 22 px）
  forM_ vs $ \v -> do
    let ci = vX v * sx
        cj = vY v * sy
        rr = 11 * sx
        col = if vG v > 0 then (255, 107, 64) else (64, 210, 255)
    forM_ [floor (cj - rr) .. ceiling (cj + rr)] $ \j ->
      forM_ [floor (ci - rr) .. ceiling (ci + rr)] $ \i ->
        if (fromIntegral i + 0.5 - ci) ^ (2 :: Int) + (fromIntegral j + 0.5 - cj) ^ (2 :: Int) <= rr * rr
          then put col 1 i j else pure ()
  U.freeze buf

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral bw) (envH env / fromIntegral bh)
      (pixelImage bw bh 1 (\i j -> let k = (i + j * bw) * 3 in (b8 (tr U.! k), b8 (tr U.! (k + 1)), b8 (tr U.! (k + 2)))))
  , view env $
      concat [ [ dot (hsb (if vG v > 0 then 12 else 195) 75 100) (vX v) (vY v) 22
               , Color (hsb 0 0 100) (Translate (vX v) (vY v) (ring 11 1.5))
               , txtC (vX v) (vY v + 6) 16 (hsb 0 0 10) (if vG v > 0 then "+" else "-") ]
             | v <- vs ]
      ++ [ rectTL (hsb 215 60 10) 0 0 (envW env) 104
         , txt 20 40 18 (hsb 0 0 100)
             ("click = place a vortex (now: " ++ (if wSign w > 0 then "+ counter-clockwise" else "- clockwise")
              ++ ", 1/2)   3-6 = examples   space = " ++ (if wPaused w then "play" else "pause") ++ "   C = clear")
         , txt 20 68 15 (hsb 42 70 100) (wName w)
         , txt 20 92 15 (hsb 0 0 65)
             (show (length vs) ++ " vortices   sum of Gamma " ++ show (round sg :: Int)
              ++ "   energy H = -sum Gi Gj ln rij / 2 pi = " ++ showFFloat (Just 3) energy "" ++ " (constant unless wrapping)"
              ++ (if sg /= 0 then "   center (" ++ show (round (mx / sg) :: Int) ++ ", " ++ show (round (my / sg) :: Int) ++ ")" else "")) ]
  ]
  where
    tr = wTrail w
    vs = wVort w
    -- 保存される量：渦どうしのエネルギー H と、渦の重心
    sg = sum (map vG vs)
    mx = sum [ vG v * vX v | v <- vs ]
    my = sum [ vG v * vY v | v <- vs ]
    energy = negate (sum [ vG a * vG b * log (dist2 (vX a, vY a) (vX b, vY b) + 1) / (2 * pi)
                         | (i, a) <- zip [0 :: Int ..] vs, (j, b) <- zip [0 ..] vs, j > i ])

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in if my < 104 || length (wVort w) >= 16 then w
       else w { wVort = wVort w ++ [Vortex mx my (wSign w)], wName = "Your own vortices. Try mixing + and -." }
  EventKey (Char '1') Down _ _ -> w { wSign = 1 }
  EventKey (Char '2') Down _ _ -> w { wSign = -1 }
  EventKey (Char k) Down _ _
    | k >= '3' && k <= '6' -> preset (envW env) (envH env) (fromEnum k - fromEnum '0') w
    | k `elem` "cC" -> w { wVort = [], wName = "No vortices. Click to place some." }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 3：同じ向きの 2 つ。回る速さは距離でどう変わる？（C で消して、近く・遠くに置き直す）
-- 2. 4：渦対。進む速さは Γ / (2π × 間かく)。間かくをせまくすると速くなる。確かめよう
-- 3. 5：蛙飛び。前の組が広がって遅くなり、後ろの組が縮んで速くなる。何回くり返す？
-- 4. 3 つの渦を三角形に置く。4 つ以上をでたらめに置くと動きは予測できなくなる（カオス）
-- 5. 6：渦列。1 つだけ渦を足して列を乱すと、どのくらい持ちこたえる？
-- 6. 強さがちがう渦を作ろう：event のクリックで Vortex mx my (wSign w * 2) にすると 2 倍の渦になる

-- ---------- 道具 ----------
zipWith4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4' f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4' f as bs cs ds
zipWith4' _ _ _ _ _ = []

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

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
