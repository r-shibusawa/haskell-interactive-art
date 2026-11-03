-- ============================================
-- 11 声におどろく群れ
-- 05 のボイドに耳をつける。静かなときは群れ、大きな声で散る。
-- 声が大きいほど色が赤く、動きが速くなる
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')
import Text.Printf (printf)

data Boid = Boid
  { bX  :: !Float
  , bY  :: !Float
  , bVx :: !Float
  , bVy :: !Float
  }

-- 残像のための、前のフレームの群れ
data Snap = Snap ![Boid] !Float

data World = World
  { wBoids :: ![Boid]
  , wPast  :: ![Snap]
  , wMic   :: !(Maybe Mic)    -- まだ ON にしていなければ Nothing
  , wLive  :: !Bool           -- 本物のマイクの音が来ているか
  , wVol   :: !Float
  , wScare :: !Float          -- 0 = 静か、1 = うるさい
  }

boidCount :: Int
boidCount = 150            -- ★ 鳥の数

loudness :: Float
loudness = 0.2             -- ★ どのくらいの声で最大になるか（感度）

trailFrames :: Int
trailFrames = 5            -- ★ 残像の長さ

trailFade :: Float
trailFade = 0.65           -- 残像の薄くなり方（p5.js の background の透明度 35 に相当）

main :: IO ()
main = runSketch (sketch "03-11 Mic Flock" (World [] [] Nothing False 0 0))
  { skBack  = hsb 220 40 8
  , skStart = \env -> do
      bs <- replicateM boidCount (newBoid env)
      pure (World bs [] Nothing False 0 0)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newBoid :: Env -> IO Boid
newBoid env = do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env)
  a <- randomRIO (0, tau)
  pure (Boid x y (cos a * 2) (sin a * 2))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  level <- maybe (pure 0) micLevel (wMic w)
  live <- maybe (pure False) micLive (wMic w)
  let vol = lerpF (wVol w) level 0.2
      scare = clampF 0 1 (remap vol 0 loudness 0 1)
      boids = wBoids w
  pure w { wBoids = map (stepBoid env scare boids) boids
         , wPast = take trailFrames (Snap boids (scareHue (wScare w)) : wPast w)
         , wVol = vol, wScare = scare, wLive = live }

scareHue :: Float -> Float
scareHue scare = remap scare 0 1 200 10

data Sum = Sum !Float !Float !Float !Float !Float !Float !Int

stepBoid :: Env -> Float -> [Boid] -> Boid -> Boid
stepBoid env scare others b =
  let keepAway = 25 + scare * 60               -- うるさいと距離をとる
      Sum cx cy ax ay sx sy count = foldl' gather (Sum 0 0 0 0 0 0 0) others
      gather acc@(Sum c1 c2 a1 a2 s1 s2 n) o =
        let dx = bX b - bX o
            dy = bY b - bY o
            d2 = dx * dx + dy * dy
        in if d2 == 0 || d2 >= 70 * 70 then acc
           else if d2 < keepAway * keepAway
             then Sum (c1 + bX o) (c2 + bY o) (a1 + bVx o) (a2 + bVy o) (s1 + dx / d2) (s2 + dy / d2) (n + 1)
             else Sum (c1 + bX o) (c2 + bY o) (a1 + bVx o) (a2 + bVy o) s1 s2 (n + 1)
      k = fromIntegral (max 1 count)
      rules = if count == 0 then [] else
        [ setMag (0.03 * (1 - scare)) (cx / k - bX b, cy / k - bY b)   -- うるさいと集まらない
        , setMag 0.05 (ax / k, ay / k)
        , (sx * (30 + scare * 100), sy * (30 + scare * 100))
        ]
      -- 中心から逃げる（大声のとき）
      fromCenter = setMag (scare * 0.3) (bX b - envW env / 2, bY b - envH env / 2)
      (vx, vy) = limit (3 + scare * 6) (foldl' addP (bVx b, bVy b) (fromCenter : rules))
  in Boid (wrapEdge (envW env) (bX b + vx)) (wrapEdge (envH env) (bY b + vy)) vx vy

-- 画面の端は反対側につながる
wrapEdge :: Float -> Float -> Float
wrapEdge size v
  | v < 0 = v + size
  | v > size = v - size
  | otherwise = v

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures $
      [ Pictures (map (boidPic hue (trailFade ^ age)) bs)
      | (age, Snap bs hue) <- reverse (zip [1 :: Int ..] (wPast w)) ]
      ++ map (boidPic (scareHue (wScare w)) 1) (wBoids w)
  , hud env 28 16 white message
  ]
  where
    volume = "Volume " ++ printf "%.3f" (wVol w)
    message = case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _ | wLive w -> "Stay quiet: the flock gathers. Loud voice: it scatters!   " ++ volume
             | otherwise -> "No mic found: using a demo signal.   " ++ volume

boidPic :: Float -> Float -> Boid -> Picture
boidPic hue alpha b =
  let ang = atan2 (bVy b) (bVx b)
      c = cos ang
      s = sin ang
      place (x, y) = (bX b + x * c - y * s, bY b + x * s + y * c)
  in Color (hsba hue 70 100 alpha) (Polygon (map place [(10, 0), (-6, 5), (-6, -5)]))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMic w of
    Nothing -> (\m -> w { wMic = Just m }) <$> startMic
    Just _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 手を叩いたときと、「あー」と言い続けたときの違いを見よう
-- 2. loudness の 0.2（どのくらいの声で最大になるか）を変えて感度を調整
-- 3. 声の高さ（micSpectrum）で色を変えてみよう

-- ---------- 道具 ----------

addP :: Point -> Point -> Point
addP (a, b) (c, d) = (a + c, b + d)

-- ベクトルの長さを m にする（長さ 0 ならそのまま）
setMag :: Float -> Point -> Point
setMag m p@(x, y) =
  let n = norm2 p
  in if n == 0 then p else (x / n * m, y / n * m)

-- ベクトルの長さを m までにする
limit :: Float -> Point -> Point
limit m p = if norm2 p > m then setMag m p else p

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
