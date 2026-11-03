-- ============================================
-- 06 スタンダード写像（島とカオスの海）
-- 「回転しながら、周期的にキックされる」コマの動きを 2 つの数で表す
-- クリックした場所から軌道を追うと、きれいな輪（島）か、散らばる点（カオスの海）になる
-- クリックで軌道を追加 / マウス上下 = キックの強さ K / C で消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)

-- 追っている軌道（色は R, G, B で持っておく）
data Orbit = Orbit
  { oX, oP :: !Float
  , oR, oG, oB :: !Float
  }

data World = World
  { wK      :: !Float            -- キックの強さ（0 = 規則的、大きいほどカオス）
  , wOrbits :: ![Orbit]
  , wBuf    :: !(U.Vector Float) -- 点を打ちためる画像（1 ピクセルに R, G, B の 3 つ）
  }

stepsPerFrame :: Int
stepsPerFrame = 200              -- ★ 軌道 1 本を 1 フレームに進める回数

bufW, bufH :: Int
bufW = 360                       -- 画像の大きさ（360 × 220 = 79200 画素）
bufH = 220

pointAlpha :: Float
pointAlpha = 0.2                 -- ★ 点 1 つの濃さ

maxOrbits :: Int
maxOrbits = 200                  -- ★ 軌道の数の上限

backColor :: Color
backColor = hsb 240 40 8

main :: IO ()
main = runSketch (sketch "04-06 Standard Map" (World 0.9 [] blankBuf))
  { skBack  = backColor
  , skStart = \_ -> do
      -- 最初にいくつかの軌道をランダムに
      orbits <- replicateM 25 (do x <- randomRIO (0, tau); p <- randomRIO (0, tau); makeOrbit x p)
      pure (World 0.9 orbits blankBuf)
  , skStep  = \env _ w -> pure (plotOrbits (followMouse env w))
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

makeOrbit :: Float -> Float -> IO Orbit
makeOrbit x p = do
  hue <- randomRIO (0, 360)
  let (r, g, b, _) = rgbaOfColor (hsb hue 60 100)
  pure (Orbit x p r g b)

blankBuf :: U.Vector Float
blankBuf = U.generate (3 * bufW * bufH) (\i -> [br, bg, bb] `pick` (i `mod` 3))
  where
    (br, bg, bb, _) = rgbaOfColor backColor
    pick xs n = case drop n xs of
      (v : _) -> v
      [] -> 0

followMouse :: Env -> World -> World
followMouse env w
  | abs (nk - wK w) > 0.01 = w { wK = nk, wBuf = blankBuf }
  | otherwise = w
  where
    (_, my) = mouseP5 env
    nk = remapC my 0 (envH env) 2.0 0.0

-- それぞれの軌道を進めて点を打つ
plotOrbits :: World -> World
plotOrbits w = runST $ do
  mv <- U.thaw (wBuf w)
  orbits <- mapM (runOrbit mv (wK w) stepsPerFrame) (wOrbits w)
  buf <- U.unsafeFreeze mv
  pure w { wOrbits = orbits, wBuf = buf }

runOrbit :: M.MVector s Float -> Float -> Int -> Orbit -> ST s Orbit
runOrbit mv k !n o
  | n <= 0 = pure o
  | otherwise = do
      -- ★ スタンダード写像：運動量がキックされ、角度が運動量ぶん進む
      let p' = wrapF tau (oP o + k * sin (oX o))
          x' = wrapF tau (oX o + p')
          o' = o { oX = x', oP = p' }
      plot mv o'
      runOrbit mv k (n - 1) o'

plot :: M.MVector s Float -> Orbit -> ST s ()
plot mv o = when (bx >= 0 && bx < bufW && by >= 0 && by < bufH) $ do
  blend i (oR o)
  blend (i + 1) (oG o)
  blend (i + 2) (oB o)
  where
    bx = floor (oX o / tau * fromIntegral bufW) :: Int
    by = floor ((1 - oP o / tau) * fromIntegral bufH) :: Int   -- p が大きいほど上
    i = 3 * (by * bufW + bx)
    blend k v = M.modify mv (\old -> old + (v - old) * pointAlpha) k

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Scale (envW env / fromIntegral bufW) (envH env / fromIntegral bufH) (pixelImage bufW bufH 1 pixel)
  , Color (withA 0.75 backColor) (rect 0 (envH env / 2 - 40) (envW env) 80)   -- 文字を読みやすくする帯
  , txt env 20 40 18 white
      ("K = " ++ fmt 2 (wK w) ++ " (mouse Y)   Click: add orbit   C: clear   Orbits " ++ show (length (wOrbits w)))
  , txt env 20 66 14 (hsb 0 0 60) "Across: angle x   Up: momentum p      p <- p + K sin(x),  x <- x + p"
  ]
  where
    buf = wBuf w
    pixel x y =
      let i = 3 * (y * bufW + x)
      in (byte (U.unsafeIndex buf i), byte (U.unsafeIndex buf (i + 1)), byte (U.unsafeIndex buf (i + 2)))
    byte v = fromIntegral (truncate (v * 255 + 0.5) :: Int)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my >= 50 && length (wOrbits w) < maxOrbits -> do
        o <- makeOrbit (remap mx 0 (envW env) 0 tau) (remap my (envH env) 0 0 tau)
        pure w { wOrbits = wOrbits w ++ [o] }
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> pure w { wOrbits = [], wBuf = blankBuf }
  _ -> pure w
  where (mx, my) = mouseP5 env

-- ---------- やってみよう ----------
-- 1. K を 0 にすると全部まっすぐな線（規則的）。0.5、1.0、1.5 と上げていこう
-- 2. 島（輪）の中をクリック → 輪の上に乗る。島の外をクリック → 海に散らばる
-- 3. 島の「中の島」を探してズーム…はできないが、島の中を細かくクリックしてみよう

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
