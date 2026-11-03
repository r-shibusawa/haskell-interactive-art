-- ============================================
-- 06 アリの道（フェロモン）
-- アリは地図を持っていない。エサを見つけたアリが「におい（フェロモン）」を
-- 落としながら巣に帰るだけで、群れ全体が最短に近い道をつくる
-- クリックでエサを置く / C でフェロモンを消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U

data Ant = Ant
  { aX     :: !Float
  , aY     :: !Float
  , aAngle :: !Float
  , aFood  :: !Bool
  }

data World = World
  { wAnts      :: ![Ant]
  , wFoods     :: ![Point]            -- エサの位置
  , wPheromone :: !(U.Vector Float)   -- マスごとの「におい」の強さ
  , wDelivered :: !Int
  }

antCount :: Int
antCount = 300             -- ★ アリの数

gridSize :: Int
gridSize = 8               -- ★ フェロモンのマス目の大きさ

cols, rows :: Int
cols = 1280 `div` gridSize
rows = 800 `div` gridSize

evaporation :: Float
evaporation = 0.995        -- ★ 蒸発（1 に近いほど長く残る）

noPheromone :: U.Vector Float
noPheromone = U.replicate (cols * rows) 0

main :: IO ()
main = runSketch (sketch "03-06 Ants" (World [] [] noPheromone 0))
  { skBack  = hsb 40 20 10
  , skStart = \env -> do
      let (nx, ny) = nest env
      ants <- replicateM antCount ((\a -> Ant nx ny a False) <$> randomRIO (0, tau))
      pure (World ants [(envW env * 0.2, envH env * 0.3), (envW env * 0.8, envH env * 0.7)] noPheromone 0)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

nest :: Env -> Point
nest env = (envW env / 2, envH env / 2)     -- 巣の位置

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  -- エサを持っているアリがフェロモンを落とし、全体が少しずつ蒸発する
  let drops = [(i, ()) | a <- wAnts w, aFood a, Just i <- [cellIndex (aX a, aY a)]]
      ph = U.accum (\v _ -> min (v + 0.3) 3) (U.map (* evaporation) (wPheromone w)) drops
  ants <- mapM (moveAnt env (wFoods w) ph) (wAnts w)
  let arrived = length (filter id (zipWith (\old new -> aFood old && not (aFood new)) (wAnts w) ants))
  pure w { wAnts = ants, wPheromone = ph, wDelivered = wDelivered w + arrived }

moveAnt :: Env -> [Point] -> U.Vector Float -> Ant -> IO Ant
moveAnt env foods ph a = do
  turned <- if aFood a then pure (homing env a) else searching foods ph a
  let ang = aAngle turned
      x = aX turned + cos ang * 2
      y = aY turned + sin ang * 2
      -- 壁で跳ね返る
      ang1 = if x < 0 || x > envW env then pi - ang else ang
      ang2 = if y < 0 || y > envH env then negate ang1 else ang1
  pure turned { aX = clampF 0 (envW env) x, aY = clampF 0 (envH env) y, aAngle = ang2 }

-- エサを持っている：巣に向かう
homing :: Env -> Ant -> Ant
homing env a =
  let (nx, ny) = nest env
      dx = nx - aX a
      dy = ny - aY a
      ang = lerpAngle (aAngle a) (atan2 dy dx) 0.2
  in if norm2 (dx, dy) < 20
       then a { aFood = False, aAngle = ang + pi }   -- 巣についたら反転
       else a { aAngle = ang }

-- 探している：前方3方向のにおいをかいで、強い方へ。あとはランダム
searching :: [Point] -> U.Vector Float -> Ant -> IO Ant
searching foods ph a = do
  let smell d =
        let ang = aAngle a + d
            v = maybe 0 (ph U.!) (cellIndex (aX a + cos ang * 24, aY a + sin ang * 24))
        in (v, ang)
      (best, bestAngle) = foldl' (\p q -> if fst q > fst p then q else p) (-1, aAngle a) (map smell [-0.6, 0, 0.6])
      ang1 = if best > 0.05 then lerpAngle (aAngle a) bestAngle 0.3 else aAngle a
  wobble <- randomRIO (-0.3, 0.3)                    -- ふらふら歩く
  -- エサを見つけたら持つ
  let found = length (filter (\f -> dist2 (aX a, aY a) f < 20) foods)
  pure a { aAngle = ang1 + wobble + fromIntegral found * pi, aFood = found > 0 }

cellIndex :: Point -> Maybe Int
cellIndex (x, y) =
  let cx = floor (x / fromIntegral gridSize) :: Int
      cy = floor (y / fromIntegral gridSize) :: Int
  in if cx < 0 || cy < 0 || cx >= cols || cy >= rows then Nothing else Just (cx + cy * cols)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ pixelImageF cols rows (fromIntegral gridSize) $ \x y ->
      -- においが強いほど水色を濃く
      let v = U.unsafeIndex (wPheromone w) (x + y * cols)
          a = if v > 0.02 then min v 1 * 0.7 else 0
      in (mixTo gr ph' a, mixTo gg pg a, mixTo gb pb a)
  , p5Space env $ Pictures $
      [Color (hsb 100 80 90) (at f (disc 18)) | f <- wFoods w]
      ++ [Color (hsb 30 80 90) (at (nest env) (disc 20))]
      ++ map antPic (wAnts w)
  , hud env 28 16 white ("Food carried " ++ show (wDelivered w) ++ "   Click: place food   C: clear scent")
  ]
  where
    (gr, gg, gb, _) = rgbaOfColor (hsb 40 20 10)
    (ph', pg, pb, _) = rgbaOfColor (hsb 180 80 100)
    mixTo p q t = p + (q - p) * t

antPic :: Ant -> Picture
antPic a = Color (if aFood a then hsb 100 80 100 else white) (at (aX a, aY a) (disc 2))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd (mouseP5 env) > 44 -> pure w { wFoods = mouseP5 env : wFoods w }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wPheromone = noPheromone }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 蒸発 0.995 を 0.98（すぐ消える）や 0.999（ずっと残る）にすると道はどうなる？
-- 2. エサを遠くと近くに置いて、どちらの道が太くなるか見よう
-- 3. アリの数 300 を 1000 にすると？

-- ---------- 道具 ----------

-- 角度をなめらかに近づける（360° のつなぎ目を正しく扱う）
lerpAngle :: Float -> Float -> Float -> Float
lerpAngle a b t = a + (wrapF tau (b - a + pi) - pi) * t

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
