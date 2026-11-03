{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 11 ベクトル場と流れ
-- 平面の各点に矢印（ベクトル）を置いたものがベクトル場。矢印に沿って流すと流線ができる
--   場が 0 になる点（特異点）には「指数」がある：わき出し・吸い込み・渦 = +1、鞍点 = −1。小さな円を 1 周したとき矢印が何回まわるか
-- クリック = +1 の点 / Shift + クリック = −1 の点（鞍点）/ ←→ = 場を回す（わき出し → 渦）/ ↑↓ = 調べる円の半径 / 1〜4 = 例 / A = 矢印 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

np :: Int
np = 1800                  -- ★ 流れる粒子の数

-- 軌跡をためる画像の大きさ（小さな画素を k 倍して画面いっぱいに広げる）
bufW, bufH :: Int
bufW = 355
bufH = 222

bgR, bgG, bgB :: Float
bgR = 14
bgG = 16
bgB = 26

-- 特異点（p5 と同じ画面座標：左上が原点、y は下向き）
data Sing = Sing !Float !Float !Float

data World = World
  { wSing :: [Sing]
  , wRot :: !Float         -- ★ 場全体を回す角度（0 = わき出し、π/2 = 渦、π = 吸い込み）
  , wProbe :: !Float       -- ★ 指数を調べる円の半径（px）
  , wArrows :: !Bool
  , wParts :: !(U.Vector (Float, Float, Float))   -- 粒子：x, y, 寿命
  , wBuf :: !(U.Vector Float)                     -- 消えていく軌跡（r, g, b の順）
  }

main :: IO ()
main = runSketch (sketch "28-11 Vector Fields" (World [] 0 80 True U.empty clearBuf))
  { skBack  = rgb255 14 16 26
  , skStart = start
  , skStep  = \env _ w -> step env w
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

clearBuf :: U.Vector Float
clearBuf = U.generate (bufW * bufH * 3) (\i -> case i `rem` 3 of { 0 -> bgR; 1 -> bgG; _ -> bgB })

start :: Env -> IO World
start env = do
  ps <- mapM (const (newPart env)) [1 .. np]
  pure (preset env 1 (World [] 0 80 True (U.fromList ps) clearBuf))

newPart :: Env -> IO (Float, Float, Float)
newPart env = do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env)
  l <- randomRIO (40, 160)
  pure (x, y, l)

-- ★ ベクトル場：複素数で f(z) = e^{i rot} × Π (z − cₖ) または (z̄ − c̄ₖ)。+1 の点は z − c、−1 の点は共役
field :: World -> Float -> Float -> (Float, Float)
field w x y = foldl' one (cos (wRot w), sin (wRot w)) (wSing w)
  where
    one (!re, !im) (Sing sx sy s) =
      let a = (x - sx) / 100
          b0 = (y - sy) / 100
          b = if s < 0 then -b0 else b0
      in (re * a - im * b, re * b + im * a)

preset :: Env -> Int -> World -> World
preset env k w =
  let cx = envW env / 2
      cy = envH env / 2
      sing = case k of
        1 -> Just [Sing cx cy 1]
        2 -> Just [Sing (cx - 180) cy 1, Sing (cx + 180) cy 1]
        3 -> Just [Sing (cx - 200) cy 1, Sing cx cy (-1), Sing (cx + 200) cy 1]
        4 -> Just [Sing cx cy (-1)]
        _ -> Nothing
  in w { wSing = maybe (wSing w) id sing, wBuf = clearBuf }

-- 円に沿って 1 周したときに、矢印が何回まわるか（回転数 = 指数の合計）
windingNumber :: World -> Float -> Float -> Float -> Int
windingNumber w cx cy r =
  let n = 360 :: Int
      angs = [ atan2 vy vx
             | i <- [0 .. n]
             , let a = fromIntegral i / fromIntegral n * tau
                   (vx, vy) = field w (cx + r * cos a) (cy + r * sin a) ]
      wrap d | d > pi = d - tau
             | d < -pi = d + tau
             | otherwise = d
      total = foldl' (+) 0 (zipWith (\a b -> wrap (b - a)) angs (drop 1 angs))
  in round (total / tau)

-- 流れ：粒子を場の向きに動かし、薄く消えていく軌跡を画像にためる
step :: Env -> World -> IO World
step env w = do
  let wd = envW env
      hg = envH env
      fade = 18 / 255        -- ★ 軌跡の消える速さ
      faded = U.imap (\i c -> let bg = case i `rem` 3 of { 0 -> bgR; 1 -> bgG; _ -> bgB } in c + (bg - c) * fade) (wBuf w)
      move (x, y, l) =
        let (vx, vy) = field w x y
            m = sqrt (vx * vx + vy * vy)
        in if m < 1e-9 || isInfinite m || isNaN m then Nothing else
           let sp = 1.6 * min 1.5 (m ** 0.3)     -- 速さは控えめに
           in Just (x + vx / m * sp, y + vy / m * sp, l - 1, min 1 (m / 3))
  results <- mapM (\p -> case move p of
                      Nothing -> (\q -> (q, Nothing)) <$> newPart env
                      Just (nx, ny, l, t)
                        | l < 0 || nx < 0 || nx > wd || ny < 0 || ny > hg -> (\q -> (q, Just (nx, ny, t))) <$> newPart env
                        | otherwise -> pure ((nx, ny, l), Just (nx, ny, t)))
                  (U.toList (wParts w))
  let plots = [ (cellOf wd hg x y, t) | (_, Just (x, y, t)) <- results, x >= 0, x < wd, y >= 0, y < hg ]
      buf = U.modify (\m -> forM_ plots $ \(i, t) -> do
              let a = 140 / 255
                  mix k col = do
                    c <- MU.unsafeRead m (i * 3 + k)
                    MU.unsafeWrite m (i * 3 + k) (c + (col - c) * a)
              mix 0 (120 + 100 * t)
              mix 1 180
              mix 2 255) faded
  pure w { wParts = U.fromList (map fst results), wBuf = buf }

cellOf :: Float -> Float -> Float -> Float -> Int
cellOf wd hg x y =
  let i = min (bufW - 1) (floor (x / wd * fromIntegral bufW))
      j = min (bufH - 1) (floor (y / hg * fromIntegral bufH))
  in j * bufW + i

draw :: Env -> World -> Picture
draw env w =
  let wd = envW env
      hg = envH env
      k = max (wd / fromIntegral bufW) (hg / fromIntegral bufH)
      b = wBuf w
      q :: Float -> Word8
      q = truncate . max 0 . min 255
      trails = pixelImage bufW bufH k (\x y ->
        let i = (y * bufW + x) * 3
        in (q (U.unsafeIndex b i), q (U.unsafeIndex b (i + 1)), q (U.unsafeIndex b (i + 2))))
      -- 矢印の格子
      arrowAt x y =
        let (vx, vy) = field w x y
            m0 = sqrt (vx * vx + vy * vy)
            m = if m0 == 0 then 1 else m0
            dx = vx / m * 14
            dy = vy / m * 14
            a = atan2 dy dx
        in Line [ px env (x - dx / 2) (y - dy / 2), px env (x + dx / 2) (y + dy / 2)
                , px env (x + dx / 2 - 5 * cos (a - 0.5)) (y + dy / 2 - 5 * sin (a - 0.5)) ]
      arrows = if wArrows w
        then [Color (withA (90 / 255) white) (Pictures [arrowAt x y | x <- [20, 60 .. wd - 1], y <- [100, 140 .. hg - 1]])]
        else []
      -- 特異点
      singPic (Sing x y s) =
        let (a, c) = px env x y
        in Pictures [ at (a, c) (Color (if s > 0 then rgb255 255 120 100 else rgb255 100 200 255) (disc 8))
                    , labelC a c 14 black (if s > 0 then "+" else "-") ]
      -- マウスの位置の円で指数を調べる
      (gx, gy) = envMouse env
      mx = gx + wd / 2
      my = hg / 2 - gy
      r = wProbe w
      wn = windingNumber w mx my r
      gold = rgb255 255 230 90
      probeArrows = [ Color gold (thickLine 1.5 (px env x y) (px env (x + vx / m * 22) (y + vy / m * 22)))
                    | i <- [0 .. 11 :: Int]
                    , let a = fromIntegral i / 12 * tau
                          x = mx + r * cos a
                          y = my + r * sin a
                          (vx, vy) = field w x y
                          m0 = sqrt (vx * vx + vy * vy)
                          m = if m0 == 0 then 1 else m0 ]
      total = round (sum [s | Sing _ _ s <- wSing w]) :: Int
      rotDeg = jsMod (degF (wRot w)) 360
  in Pictures $
     [trails] ++ arrows ++ map singPic (wSing w) ++
     [ at (gx, gy) (Color gold (ring r 2)) ] ++ probeArrows ++
     [ txt env (mx + r + 8) my 17 gold ("index " ++ (if wn > 0 then "+" else "") ++ show wn)
     , box env 0 0 wd 90 (withA (170 / 255) black)
     , txt env 20 34 16 white ("Click: +1 point   Shift+click: -1 (saddle)   L/R: turn field " ++ nf 0 rotDeg ++ " deg   U/D: circle size   1-4: examples   A: arrows   C: clear")
     , txt env 20 62 14 (gray 0.67) "Turns of the arrow around the yellow circle = sum of indices inside. L/R turns a source into a vortex: index stays +1"
     , box env 0 (hg - 70) wd 70 (withA (170 / 255) black)
     , txt env 20 (hg - 40) 16 white ("Sum of indices = " ++ show total ++ "    On a sphere (stereographic, see 01), the point at infinity has index 2 - (" ++ show total ++ ") = " ++ show (2 - total))
     , txt env 20 (hg - 16) 14 (gray 0.67) "Poincare-Hopf: indices on a sphere always sum to chi = 2. A hairy ball always has a cowlick."
     ]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods (gx, gy) ->
    let mx = gx + envW env / 2
        my = envH env / 2 - gy
        isNear (Sing x y _) = dist2 (mx, my) (x, y) < 12
        (before, rest) = break isNear (wSing w)
        s = if shift mods == Down then -1 else 1
    in if my < 90 || my > envH env - 70 then w
       else case rest of
         (_ : after) -> w { wSing = before ++ after }      -- 近くの点をクリックしたら消す
         [] | length (wSing w) < 8 -> w { wSing = wSing w ++ [Sing mx my s] }
            | otherwise -> w
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRot = wRot w - pi / 8 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRot = wRot w + pi / 8 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wProbe = min 400 (wProbe w + 20) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wProbe = max 20 (wProbe w - 20) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> preset env (fromEnum c - fromEnum '0') w
    | toLower c == 'a' -> w { wArrows = not (wArrows w) }
    | toLower c == 'c' -> w { wSing = [], wBuf = clearBuf }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 例 3（+1, −1, +1）：大きな円で 3 つ全部を囲むと指数は？ 1 つずつ囲むと？（足し算になっている）
-- 2. ←→ で 90° 回すと、わき出しは渦（台風）に、鞍点は向きの変わった鞍点になる。指数は変わらない
-- 3. 特異点を 1 つも置かないと、場はどこでも同じ向き。球面に貼ると北極の指数は 2（双極子のつむじ）
-- 4. field を「(z − c)²」にすると指数 +2 の点が作れる。どんな模様になる？

---------------------------------------------------------------- 道具
-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
px :: Env -> Float -> Float -> Point
px env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（p5 の text と同じく左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (a, b) = px env x y in label a b s c str

-- 左上が (x, y) の塗りつぶし長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (a, b) = px env (x + w / 2) (y + h / 2) in Color c (rect a b w h)

-- p5 の nf：小数点以下 d 桁の文字列
nf :: RealFloat a => Int -> a -> String
nf d x | isNaN x = "--"
       | isInfinite x = if x > 0 then "inf" else "-inf"
       | otherwise = showFFloat (Just d) x ""

degF :: Float -> Float
degF x = x * 180 / pi

-- JavaScript の % と同じ（符号は x に合わせる）
jsMod :: Float -> Float -> Float
jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)
