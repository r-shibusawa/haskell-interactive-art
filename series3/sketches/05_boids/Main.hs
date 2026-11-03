-- ============================================
-- 05 ボイド（鳥の群れ）
-- 1匹1匹は「近づく・そろえる・離れる」の3つのルールだけ
-- リーダーはいないのに、群れとして動く
-- マウス = 群れが寄ってくる / クリック = 散らす / 1・2・3 キーでルールの ON/OFF
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')

data Boid = Boid
  { bX   :: !Float
  , bY   :: !Float
  , bVx  :: !Float
  , bVy  :: !Float
  , bHue :: !Float
  }

data World = World
  { wBoids :: ![Boid]
  , wPast  :: ![[Boid]]    -- 残像のために、前のフレームの群れを少し覚えておく
  , wCoh   :: !Bool
  , wAli   :: !Bool
  , wSep   :: !Bool
  }

boidCount :: Int
boidCount = 120            -- ★ 鳥の数

viewRange, tooClose :: Float
viewRange = 60             -- ★ 見える範囲
tooClose = 25              -- ★ 近すぎる距離

trailFrames :: Int
trailFrames = 4            -- ★ 残像の長さ

trailFade :: Float
trailFade = 0.6            -- 残像の薄くなり方（p5.js の background の透明度 40 に相当）

main :: IO ()
main = runSketch (sketch "03-05 Boids" (World [] [] True True True))
  { skBack  = hsb 230 40 10
  , skStart = \env -> do
      bs <- replicateM boidCount (newBoid env)
      pure (World bs [] True True True)
  , skStep  = \env _ w -> pure w
      { wBoids = map (stepBoid env w (wBoids w)) (wBoids w)
      , wPast = take trailFrames (wBoids w : wPast w)
      }
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newBoid :: Env -> IO Boid
newBoid env = do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env)
  a <- randomRIO (0, tau)
  h <- randomRIO (180, 260)
  pure (Boid x y (cos a * 2) (sin a * 2) h)

-- 近くの仲間について足し合わせた結果
data Sum = Sum !Float !Float !Float !Float !Float !Float !Int

-- ★ 3つのルール
stepBoid :: Env -> World -> [Boid] -> Boid -> Boid
stepBoid env w others b =
  let Sum cx cy ax ay sx sy count = foldl' gather (Sum 0 0 0 0 0 0 0) others
      gather acc@(Sum c1 c2 a1 a2 s1 s2 n) o =
        let dx = bX b - bX o
            dy = bY b - bY o
            d2 = dx * dx + dy * dy
        in if d2 == 0 || d2 >= viewRange * viewRange then acc    -- 自分（と同じ場所の仲間）は数えない
           else if d2 < tooClose * tooClose
             then Sum (c1 + bX o) (c2 + bY o) (a1 + bVx o) (a2 + bVy o) (s1 + dx / d2) (s2 + dy / d2) (n + 1) -- 近いほど強く離れる
             else Sum (c1 + bX o) (c2 + bY o) (a1 + bVx o) (a2 + bVy o) s1 s2 (n + 1)
      k = fromIntegral (max 1 count)
      none = (0, 0)
      cohesion = setMag 0.03 (cx / k - bX b, cy / k - bY b)   -- 中心に向かう力
      alignment = setMag 0.05 (ax / k, ay / k)                -- 向きをそろえる力
      separation = (sx * 30, sy * 30)                         -- 離れる力
      rules = if count == 0 then [] else
        [if wCoh w then cohesion else none, if wAli w then alignment else none, if wSep w then separation else none]
      -- マウスに引き寄せられる（弱く）
      (mx, my) = mouseP5 env
      toMouse = (mx - bX b, my - bY b)
      pull = if norm2 toMouse < 300 then setMag 0.02 toMouse else none
      (vx, vy) = limit 3.5 (foldl' addP (bVx b, bVy b) (pull : rules))
  in b { bX = wrapEdge (envW env) (bX b + vx), bY = wrapEdge (envH env) (bY b + vy), bVx = vx, bVy = vy }

-- 画面の端は反対側につながる
wrapEdge :: Float -> Float -> Float
wrapEdge size v
  | v < 0 = v + size
  | v > size = v - size
  | otherwise = v

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures $
      [ Pictures (map (boidPic (trailFade ^ age)) bs)
      | (age, bs) <- reverse (zip [1 :: Int ..] (wPast w)) ]
      ++ map (boidPic 1) (wBoids w)
  , hud env 28 16 white
      ("1: cohesion " ++ onOff (wCoh w) ++ "   2: alignment " ++ onOff (wAli w) ++ "   3: separation " ++ onOff (wSep w) ++ "   Click: scatter")
  ]

onOff :: Bool -> String
onOff v = if v then "ON" else "off"

-- 進む方向を向いた三角形
boidPic :: Float -> Boid -> Picture
boidPic alpha b =
  let ang = atan2 (bVy b) (bVx b)
      c = cos ang
      s = sin ang
      place (x, y) = (bX b + x * c - y * s, bY b + x * s + y * c)
  in Color (hsba (bHue b) 60 100 alpha) (Polygon (map place [(10, 0), (-6, 5), (-6, -5)]))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wBoids = map (scatter (mouseP5 env)) (wBoids w) }
  EventKey (Char '1') Down _ _ -> pure w { wCoh = not (wCoh w) }
  EventKey (Char '2') Down _ _ -> pure w { wAli = not (wAli w) }
  EventKey (Char '3') Down _ _ -> pure w { wSep = not (wSep w) }
  _ -> pure w

-- クリックした場所から逃げる
scatter :: Point -> Boid -> Boid
scatter (mx, my) b =
  let away = (bX b - mx, bY b - my)
      (px, py) = setMag 6 away
  in if norm2 away < 250 then b { bVx = bVx b + px, bVy = bVy b + py } else b

-- ---------- やってみよう ----------
-- 1. 1〜3 でルールを1つずつ切って、どのルールが何をしているか見よう
-- 2. 見える範囲 viewRange を 150 にすると？（大きな群れに）
-- 3. 色を「近くの仲間の数」で変えると、群れの密度が見える

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

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
