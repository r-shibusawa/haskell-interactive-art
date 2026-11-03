-- ============================================
-- 07 生態系シミュレーション（草・ウサギ・キツネ）
-- 草は生える。ウサギは草を食べて増える。キツネはウサギを食べて増える。
-- 食べられないと死ぬ。それだけで、数が波のように増えたり減ったりする
-- クリックでウサギを追加 / F キーでキツネを追加
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

data Animal = Animal
  { nX      :: !Float
  , nY      :: !Float
  , nEnergy :: !Float
  , nAngle  :: !Float
  }

data World = World
  { wGrass   :: !(U.Vector Float)   -- 草のマス（0〜1 の量）
  , wRabbits :: ![Animal]
  , wFoxes   :: ![Animal]
  , wHistory :: ![(Int, Int)]       -- 数の記録（グラフ用、新しいものが先頭）
  }

gw, gh :: Int
gw = 48                    -- ★ 草のマスの数
gh = 27

graphH :: Float
graphH = 120               -- 下のグラフの高さ

grassGrow :: Float
grassGrow = 0.004          -- ★ 草の回復

main :: IO ()
main = runSketch (sketch "03-07 Predator and Prey" (World (U.replicate (gw * gh) 1) [] [] []))
  { skBack  = hsb 100 30 15
  , skStart = \env -> do
      rs <- replicateM 80 (randomAnimal env)
      fs <- replicateM 8 (randomAnimal env)
      pure (World (U.replicate (gw * gh) 1) rs fs [])
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randomAnimal :: Env -> IO Animal
randomAnimal env = do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env - graphH)
  newAnimal x y

newAnimal :: Float -> Float -> IO Animal
newAnimal x y = Animal x y 50 <$> randomRIO (0, tau)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  -- ---- 草：少しずつ生える ----
  grass <- U.thaw (U.map (\g -> min 1 (g + grassGrow)) (wGrass w))
  -- ---- ウサギ ----
  rabbits <- foldM (stepRabbit env grass) [] (wRabbits w)
  grass' <- U.unsafeFreeze grass
  -- ---- キツネ ----
  (rabbits', foxes) <- foldM (stepFox env) (rabbits, []) (wFoxes w)
  let history
        | envFrame env `mod` 4 == 0 =
            take (floor (envW env / 2)) ((length rabbits', length foxes) : wHistory w)
        | otherwise = wHistory w
  pure w { wGrass = grass', wRabbits = rabbits', wFoxes = foxes, wHistory = history }

-- ウサギ 1 匹ぶん。生き残ったウサギと子どもを done に足していく
stepRabbit :: Env -> MU.IOVector Float -> [Animal] -> Animal -> IO [Animal]
stepRabbit env grass done r0 = do
  r1 <- move env 1.5 r0
  let cw = envW env / fromIntegral gw
      ch = (envH env - graphH) / fromIntegral gh
      cx = max 0 (min (gw - 1) (floor (nX r1 / cw)))
      cy = max 0 (min (gh - 1) (floor (nY r1 / ch)))
      gi = cx + cy * gw
  g <- MU.read grass gi
  -- 草を食べる（生きているだけでお腹が減る）
  ate <- if g > 0.3 then MU.write grass gi (g - 0.3) >> pure 6 else pure 0
  let r2 = r1 { nEnergy = nEnergy r1 - 0.3 + ate }
  -- ★ 十分食べたら子を産む
  if nEnergy r2 > 100
    then do
      kid <- newAnimal (nX r2) (nY r2)
      pure (r2 { nEnergy = 50 } : kid : done)
    else pure (if nEnergy r2 <= 0 then done else r2 : done)   -- エネルギーが尽きたら餓死

-- キツネ 1 匹ぶん
stepFox :: Env -> ([Animal], [Animal]) -> Animal -> IO ([Animal], [Animal])
stepFox env (rabbits, done) f0 = do
  -- いちばん近いウサギを追う
  let nearest = foldl' closer (150, Nothing) rabbits
      closer best@(bd, _) r =
        let d = dist2 (nX f0, nY f0) (nX r, nY r)
        in if d < bd then (d, Just r) else best
      f1 = case snd nearest of
        Just t -> f0 { nAngle = atan2 (nY t - nY f0) (nX t - nX f0) }
        Nothing -> f0
  f2 <- move env 2.2 f1
  -- ウサギを食べる（1 フレームに 1 匹まで）
  let isNear r = dist2 (nX f2, nY f2) (nX r, nY r) < 10
      (rabbits', eaten) = removeFirst isNear rabbits
      f3 = f2 { nEnergy = nEnergy f2 - 0.5 + (if eaten then 40 else 0) }
  if nEnergy f3 > 150
    then do
      kid <- newAnimal (nX f3) (nY f3)
      pure (rabbits', f3 { nEnergy = 60 } : kid : done)
    else pure (rabbits', if nEnergy f3 <= 0 then done else f3 : done)

-- 条件に合う最初の 1 つを取りのぞく
removeFirst :: (a -> Bool) -> [a] -> ([a], Bool)
removeFirst _ [] = ([], False)
removeFirst p (x : xs)
  | p x = (xs, True)
  | otherwise = let (ys, found) = removeFirst p xs in (x : ys, found)

-- ふらふら歩いて、端で跳ね返る
move :: Env -> Float -> Animal -> IO Animal
move env speed a = do
  d <- randomRIO (-0.4, 0.4)
  let ang = nAngle a + d
      x = nX a + cos ang * speed
      y = nY a + sin ang * speed
      w' = envW env
      h' = envH env - graphH
      ang1 = if x < 0 || x > w' then pi - ang else ang
      ang2 = if y < 0 || y > h' then negate ang1 else ang1
  pure a { nX = clampF 0 (w' - 1) x, nY = clampF 0 (h' - 1) y, nAngle = ang2 }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures
      [ Pictures (grassCells env w)
      , Pictures [Color (hsb 0 0 95) (at (nX r, nY r) (disc 4)) | r <- wRabbits w]
      , Pictures [Color foxColor (at (nX f, nY f) (disc 7)) | f <- wFoxes w]
      -- ---- グラフ ----
      , Color (hsb 0 0 5) (box 0 (h - graphH) (envW env) graphH)
      , graphLine h (map fst hist) 400 (hsb 0 0 95)
      , graphLine h (map snd hist) 40 foxColor
      ]
  , hud env (h - 96) 16 white
      ("Rabbits " ++ show (length (wRabbits w)) ++ " (white)   Foxes " ++ show (length (wFoxes w))
        ++ " (red)   Click: add rabbits   F: add foxes")
  ]
  where
    h = envH env
    hist = reverse (wHistory w)

foxColor :: Color
foxColor = hsb 20 90 95

grassCells :: Env -> World -> [Picture]
grassCells env w =
  let cw = envW env / fromIntegral gw
      ch = (envH env - graphH) / fromIntegral gh
      cell i g = Color (hsb 110 60 (30 + g * 40))
                   (box (fromIntegral (i `mod` gw) * cw) (fromIntegral (i `div` gw) * ch) (cw + 0.5) (ch + 0.5))
  in zipWith cell [0 ..] (U.toList (wGrass w))

graphLine :: Float -> [Int] -> Float -> Color -> Picture
graphLine h vals maxVal col =
  Color col (thickPath 2 (zipWith (\i v -> (i * 2, h - 10 - min (fromIntegral v / maxVal) 1 * 80)) [0 ..] vals))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < envH env - graphH -> do
        kids <- replicateM 10 (newAnimal mx my)
        pure w { wRabbits = kids ++ wRabbits w }
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> do
        kids <- replicateM 3 (newAnimal mx my)
        pure w { wFoxes = kids ++ wFoxes w }
  _ -> pure w
  where (mx, my) = mouseP5 env

-- ---------- やってみよう ----------
-- 1. グラフを見よう。ウサギが増えた少しあとにキツネが増え、ウサギが減る…波になる
-- 2. キツネを全部いなくすると（F を押さず待つ）、ウサギと草はどうなる？
-- 3. 数字（草の回復 0.004、繁殖 100 など）を変えて、絶滅しないバランスを探そう

-- ---------- 道具 ----------

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
