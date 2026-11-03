-- ============================================
-- 09 進化するロケット（遺伝的アルゴリズム）
-- ロケットは「毎フレームどの方向に噴射するか」の遺伝子を持つ
-- ゴールに近いロケットほど選ばれて、子を残す → 世代を重ねると壁をよけてゴールへ
-- クリックでゴールを動かす / ドラッグで壁を動かす
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Rocket = Rocket
  { rX       :: !Float
  , rY       :: !Float
  , rVx      :: !Float
  , rVy      :: !Float
  , rGenes   :: !(U.Vector (Float, Float))   -- lifespan 個の「噴射ベクトル」
  , rAlive   :: !Bool
  , rReached :: !Bool
  }

data World = World
  { wRockets :: ![Rocket]
  , wCount   :: !Int        -- 今のフレーム
  , wGen     :: !Int
  , wTarget  :: !Point
  , wWall    :: !Point      -- 壁の左上の角
  }

popSize, lifespan :: Int
popSize = 150              -- ★ ロケットの数
lifespan = 300             -- ★ 1世代のフレーム数

wallW, wallH :: Float
wallW = 300
wallH = 16

mutationRate :: Float
mutationRate = 0.01        -- ★ 突然変異の確率

main :: IO ()
main = runSketch (sketch "03-09 Smart Rockets" (World [] 0 1 (0, 0) (0, 0)))
  { skBack  = hsb 230 40 10
  , skStart = \env -> do
      rs <- replicateM popSize (makeRocket env <$> randomGenes)
      pure (World rs 0 1 (envW env / 2, 80) (envW env / 2 - wallW / 2, envH env / 2))
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randomGene :: IO (Float, Float)
randomGene = (\a -> (cos a * 0.25, sin a * 0.25)) <$> randomRIO (0, tau)

randomGenes :: IO (U.Vector (Float, Float))
randomGenes = U.replicateM lifespan randomGene

makeRocket :: Env -> U.Vector (Float, Float) -> Rocket
makeRocket env genes = Rocket (envW env / 2) (envH env - 30) 0 0 genes True False

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  let moved = map (moveRocket env w) (wRockets w)
      count = wCount w + 1
  if count >= lifespan
    then do
      next <- nextGeneration env (wTarget w) moved
      pure w { wRockets = next, wCount = 0, wGen = wGen w + 1 }
    else pure w { wRockets = moved, wCount = count }

moveRocket :: Env -> World -> Rocket -> Rocket
moveRocket env w r
  | not (rAlive r) || rReached r = r
  | otherwise =
      let (gx, gy) = rGenes r U.! wCount w          -- 遺伝子どおりに噴射
          (vx, vy) = limit 4 (rVx r + gx, rVy r + gy)
          x = rX r + vx
          y = rY r + vy
          (wx, wy) = wWall w
          -- 壁や画面外に当たったら死ぬ
          hitWall = x > wx && x < wx + wallW && y > wy && y < wy + wallH
          outside = x < 0 || x > envW env || y < 0 || y > envH env
      in r { rX = x, rY = y, rVx = vx, rVy = vy
           , rAlive = not (hitWall || outside)
           , rReached = dist2 (x, y) (wTarget w) < 12 }

-- ★ 進化：評価 → 選択 → 交叉 → 突然変異
nextGeneration :: Env -> Point -> [Rocket] -> IO [Rocket]
nextGeneration env target rockets = do
  let -- 1. 評価：ゴールに近いほど高得点
      fitness r =
        let f = 1 / (dist2 (rX r, rY r) target + 1)
            f1 = if rReached r then f * 10 else f   -- ゴールしたら大ボーナス
        in if rAlive r then f1 else f1 * 0.1        -- ぶつかったら減点
      maxFit = foldl' max 0 (map fitness rockets)
      -- 2. 選択：得点が高いほど「くじ」に多く入れる
      pool = V.fromList (concatMap (\r -> replicate (floor (fitness r / maxFit * 100)) r) rockets)
      pick = (pool V.!) <$> randomRIO (0, V.length pool - 1)
      -- 3. 交叉と突然変異で次の世代をつくる
      child = do
        a <- pick
        b <- pick
        mid <- randomRIO (0, lifespan - 1)
        genes <- U.generateM lifespan $ \j -> do
          m <- randomRIO (0, 1)
          if m < mutationRate                       -- 1% で突然変異
            then randomGene
            else pure (rGenes (if j < mid then a else b) U.! j)   -- 前半は親A、後半は親B
        pure (makeRocket env genes)
  if V.null pool then pure rockets else replicateM popSize child

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures $
      [ Color (hsb 100 80 100) (at (wTarget w) (disc 12))
      , Color (hsb 0 0 60) (box (fst (wWall w)) (snd (wWall w)) wallW wallH)
      ] ++ map rocketPic (wRockets w)
  , hud env 20 15 white
      ("Generation " ++ show (wGen w) ++ "   Frame " ++ show (wCount w) ++ "/" ++ show lifespan
        ++ "   Reached goal " ++ show (length (filter rReached (wRockets w))) ++ "/" ++ show popSize)
  , hud env 40 15 white "Click: move goal   Drag: move wall"
  ]

rocketPic :: Rocket -> Picture
rocketPic r =
  let ang = atan2 (rVy r) (rVx r)
      c = cos ang
      s = sin ang
      place (x, y) = (rX r + x * c - y * s, rY r + x * s + y * c)
      col | rReached r = hsba 100 60 100 0.8
          | rAlive r = hsba 200 60 100 0.8
          | otherwise = hsba 0 0 40 0.8
  in Color col (Polygon (map place [(-8, -2), (8, -2), (8, 2), (-8, 2)]))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my > 44 && not (mx > wx - 20 && mx < wx + wallW + 20 && abs (my - wy) < 40) ->
        pure w { wTarget = (mx, my) }
  EventMotion _ | envDown env -> pure w { wWall = (mx - wallW / 2, my) }
  _ -> pure w
  where
    (mx, my) = mouseP5 env
    (wx, wy) = wWall w

-- ---------- やってみよう ----------
-- 1. 10世代くらい待とう。壁をよけてゴールするロケットが増えてくる
-- 2. 突然変異 0.01 を 0.2 にすると？（ランダムすぎて学べない） 0 にすると？
-- 3. 壁を動かすと、せっかく進化した遺伝子はどうなる？

-- ---------- 道具 ----------

-- ベクトルの長さを m までにする
limit :: Float -> Point -> Point
limit m p@(x, y) =
  let n = norm2 p
  in if n > m then (x / n * m, y / n * m) else p

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の rect と同じ、左上の角 (x, y) と幅・高さで決める長方形
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
