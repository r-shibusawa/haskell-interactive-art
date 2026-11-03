-- ============================================
-- 08 進化する生き物
-- 生き物は「遺伝子」（速さ・大きさ・色）を持つ。エサを食べて増え、
-- 子は親の遺伝子を少しだけ変えて（突然変異）受けつぐ。
-- 誰も設計していないのに、環境に合った生き物が残っていく
-- 1・2 キーで環境を変える（エサの量） / クリックでエサを置く
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.List (foldl')
import Text.Printf (printf)

data Genes = Genes
  { gSpeed :: !Float
  , gSize  :: !Float
  , gHue   :: !Float
  }

data Creature = Creature
  { cX      :: !Float
  , cY      :: !Float
  , cGenes  :: !Genes
  , cEnergy :: !Float
  , cAngle  :: !Float
  }

data Food = Food !Float !Float

data World = World
  { wCreatures :: ![Creature]
  , wFoods     :: ![Food]
  , wFoodRate  :: !Int      -- 1フレームに生まれるエサの数
  , wBirths    :: !Int
  , wAvgSpeed  :: !Float
  , wAvgSize   :: !Float
  }

topMargin :: Float
topMargin = 60             -- 上の説明の帯

maxFoods :: Int
maxFoods = 2000            -- ★ エサの上限（増えすぎて重くならないように）

main :: IO ()
main = runSketch (sketch "03-08 Evolution" (World [] [] 2 0 0 0))
  { skBack  = hsb 220 30 10
  , skStart = \env -> do
      cs <- firstCreatures env
      pure (World cs [] 2 0 0 0)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

firstCreatures :: Env -> IO [Creature]
firstCreatures env = replicateM 40 $ do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env)
  g <- Genes <$> randomRIO (1, 3) <*> randomRIO (8, 20) <*> randomRIO (0, 360)
  makeCreature x y g

makeCreature :: Float -> Float -> Genes -> IO Creature
makeCreature x y g = Creature x y g 60 <$> randomRIO (0, tau)

-- 1 フレームのあいだ受けわたす状態
data Pass = Pass ![Food] ![Creature] !Int !Float !Float

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  -- エサが生まれる
  born <- replicateM (wFoodRate w) (newFood env)
  let foods0 = take maxFoods (concat born ++ wFoods w)
  Pass foods cs births sumSpeed sumSize <-
    foldM (stepCreature env) (Pass foods0 [] (wBirths w) 0 0) (wCreatures w)
  -- 全滅したら新しく始める
  cs' <- if null cs then firstCreatures env else pure cs
  let n = fromIntegral (length cs')
  pure w { wCreatures = cs', wFoods = foods, wBirths = births
         , wAvgSpeed = sumSpeed / n, wAvgSize = sumSize / n }

newFood :: Env -> IO [Food]
newFood env = do
  r <- randomRIO (0, 1 :: Float)
  if r < 0.5
    then (\x y -> [Food x y]) <$> randomRIO (0, envW env) <*> randomRIO (topMargin, envH env)
    else pure []

stepCreature :: Env -> Pass -> Creature -> IO Pass
stepCreature env (Pass foods done births sumSpeed sumSize) c = do
  let g = cGenes c
      -- いちばん近いエサへ向かう（大きいほど遠くまで見える）
      (_, target) = foldl' closer (60 + gSize g * 4, Nothing) foods
      closer best@(bd, _) f@(Food fx fy) =
        let d = dist2 (cX c, cY c) (fx, fy)
        in if d < bd then (d, Just f) else best
  angle <- case target of
    Just (Food fx fy) -> pure (lerpF (cAngle c) (atan2 (fy - cY c) (fx - cX c)) 0.2)
    Nothing -> (cAngle c +) <$> randomRIO (-0.3, 0.3)
  let x = wrapF (envW env) (cX c + cos angle * gSpeed g)
      y = clampF topMargin (envH env) (cY c + sin angle * gSpeed g)
      -- ★ コスト：速くて大きいほど、たくさんエネルギーを使う
      cost = 0.1 + gSpeed g * 0.08 + gSize g * 0.015
      -- エサを食べる
      reach = gSize g / 2 + 4
      (foods', eaten) = foldr (\f@(Food fx fy) (keep, k) ->
        if dist2 (x, y) (fx, fy) < reach then (keep, k + 1) else (f : keep, k)) ([], 0 :: Int) foods
      energy = cEnergy c - cost + 25 * fromIntegral eaten
      me = c { cX = x, cY = y, cAngle = angle, cEnergy = energy }
  -- ★ 繁殖：遺伝子をコピーして、少しだけ変える（突然変異）
  (me', kids, births') <- if energy > 120
    then do
      kid <- mutate g >>= makeCreature x y
      pure (me { cEnergy = 60 }, [kid], births + 1)
    else pure (me, [], births)
  pure $ if cEnergy me' <= 0
    then Pass foods' (kids ++ done) births' sumSpeed sumSize     -- 死ぬ
    else Pass foods' (me' : kids ++ done) births' (sumSpeed + gSpeed g) (sumSize + gSize g)

mutate :: Genes -> IO Genes
mutate g = do
  ds <- randomRIO (-0.4, 0.4)
  dz <- randomRIO (-3, 3)
  dh <- randomRIO (-20, 20)
  pure (Genes (clampF 0.3 6 (gSpeed g + ds)) (clampF 4 40 (gSize g + dz)) (wrapF 360 (gHue g + dh)))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures
      [ Color (hsb 100 70 90) (Pictures [Translate x y (disc 2.5) | Food x y <- wFoods w])
      , Pictures (map creaturePic (wCreatures w))
      ]
  , hud env 20 15 white
      ("Creatures " ++ show (length (wCreatures w)) ++ "   Avg speed " ++ printf "%.2f" (wAvgSpeed w)
        ++ "   Avg size " ++ printf "%.1f" (wAvgSize w) ++ "   Births " ++ show (wBirths w))
  , hud env 40 15 white ("1: little food   2: lots of food (now " ++ show (wFoodRate w) ++ ")   Click: drop food")
  ]

creaturePic :: Creature -> Picture
creaturePic c =
  let g = cGenes c
      s = gSize g
  in Pictures
       [ Color (hsb (gHue g) 70 100) (at (cX c, cY c) (disc (s / 2)))
       , Color (hsb (gHue g) 70 60)                                        -- 目
           (at (cX c + cos (cAngle c) * s * 0.3, cY c + sin (cAngle c) * s * 0.3) (disc (s * 0.15)))
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    let (mx, my) = mouseP5 env
    fs <- replicateM 30 (Food <$> ((mx +) <$> randomRIO (-60, 60)) <*> ((my +) <$> randomRIO (-60, 60)))
    pure w { wFoods = take maxFoods (fs ++ wFoods w) }
  EventKey (Char '1') Down _ _ -> pure w { wFoodRate = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wFoodRate = 6 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. しばらく眺めて「平均の速さ」「大きさ」がどう変わるか見よう
-- 2. 1（エサ少ない）と 2（エサ多い）で、残る生き物の特徴はどう変わる？
-- 3. コストの式を変えると（例：速いほど有利に）、進化の方向が変わる

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
