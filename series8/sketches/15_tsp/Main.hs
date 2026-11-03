-- ============================================
-- 15 巡回セールスマン問題（おまけ）
-- 全部の都市を 1 回ずつ回って戻る、いちばん短い道は？ 都市が 20 個でも道の数は 10^17 通り
-- 「交差している 2 本をほどく」（2-opt）をくり返すだけで、かなり良い道が見つかる
-- クリックで都市を追加 / スペースで改善開始 / R でランダムな都市 / C で消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.List (sortOn)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wCities    :: !(V.Vector Point)
  , wTour      :: !(U.Vector Int)
  , wImproving :: !Bool
  , wBest      :: !Float
  , wStartLen  :: !Float
  , wSwaps     :: !Int
  }

cityCount :: Int
cityCount = 25             -- ★ 都市の数

triesPerStep :: Int
triesPerStep = 200         -- ★ 1 回の改善で試す組の数

stepsPerFrame :: Int
stepsPerFrame = 5          -- ★ 1 フレームに行う改善の回数

main :: IO ()
main = runSketch (sketch "08-15 Traveling Salesman" (World V.empty U.empty False 0 0 0))
  { skBack  = hsb 220 40 10
  , skStart = randomCities
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

randomCities :: Env -> IO World
randomCities env = do
  pts <- replicateM cityCount ((,) <$> randomRIO (80, envW env - 80) <*> randomRIO (130, envH env - 60))
  resetTour (V.fromList pts)

-- 都市を回る順番をランダムにする
resetTour :: V.Vector Point -> IO World
resetTour cities = do
  let m = V.length cities
  keys <- replicateM m (randomRIO (0, 1 :: Float))
  let tour = U.fromList (map snd (sortOn fst (zip keys [0 .. m - 1])))
      len = tourLength cities tour
  pure (World cities tour False len len 0)

tourLength :: V.Vector Point -> U.Vector Int -> Float
tourLength cities tour = sum [dist2 (city i) (city ((i + 1) `mod` m)) | i <- [0 .. m - 1]]
  where
    m = U.length tour
    city i = cities V.! (tour U.! i)

-- ★ 2-opt：区間 [i+1 … j] を逆向きにすると短くなるなら、ひっくり返す
twoOptStep :: World -> IO (World, Bool)
twoOptStep w
  | m < 4 = pure (w, False)            -- 都市が少なすぎる
  | otherwise = go triesPerStep
  where
    m = U.length (wTour w)
    tour = wTour w
    city i = wCities w V.! (tour U.! i)
    go 0 = pure (w, False)
    go tries = do
      i <- randomRIO (0, m - 2)
      if i + 2 >= m then go (tries - 1 :: Int) else do   -- 区間がつくれない組は試さない
        j <- randomRIO (i + 2, m - 1)
        let a = city i
            b = city (i + 1)
            c = city j
            d = city ((j + 1) `mod` m)
            before = dist2 a b + dist2 c d
            after = dist2 a c + dist2 b d
        if (i == 0 && j == m - 1) || after >= before - 0.01
          then go (tries - 1)
          else do
            let tour' = U.concat [U.take (i + 1) tour, U.reverse (U.slice (i + 1) (j - i) tour), U.drop (j + 1) tour]
            pure (w { wTour = tour', wBest = tourLength (wCities w) tour', wSwaps = wSwaps w + 1 }, True)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | not (wImproving w) = pure w
  | otherwise = do
      (w1, changed) <- foldM improve (w, False) [1 .. stepsPerFrame]
      -- しばらく改善なし → 終了
      pure (if not changed && envFrame env `mod` 30 == 0 then w1 { wImproving = False } else w1)
  where
    improve (v, ch) _ = do
      (v1, c) <- twoOptStep v
      pure (v1, ch || c)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (tourPic ++ cityPics ++ hud)
  where
    gpos (x, y) = toG env x y
    pts = map (\i -> gpos (wCities w V.! i)) (U.toList (wTour w))
    tourPic = case pts of
      (p : _ : _) -> [Color (hsb 45 90 100) (thickPath 2.5 (pts ++ [p]))]
      _ -> []
    cityPics = [Color (hsb 170 70 100) (at (gpos p) (disc 7)) | p <- V.toList (wCities w)]
    saved = 100 * (1 - wBest w / max (wStartLen w) 1)
    hud =
      [ txt env 20 32 18 (hsb 0 0 100)
          ("Cities " ++ show (V.length (wCities w)) ++ "  Length " ++ show (roundJs (wBest w))
            ++ " (start " ++ show (roundJs (wStartLen w)) ++ ", " ++ nf 0 saved ++ " % shorter)  Untangled " ++ show (wSwaps w))
      , txt env 20 56 15 (hsb 0 0 100)
          ("Space: " ++ (if wImproving w then "stop" else "start improving") ++ "   Click: add city   R: random   C: clear")
      , txt env 20 78 15 (hsb 0 0 60)
          "Checking all routes is impossible (10^23 for 25 cities), but untangling crossings gets within a few %."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd m > 90 -> resetTour (V.snoc (wCities w) m)
    | otherwise -> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wImproving = not (wImproving w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomCities env
    | c `elem` "cC" -> pure (World V.empty U.empty False 0 0 0)
  _ -> pure w
  where m = mouseXY env

-- ---------- やってみよう ----------
-- 1. 改善が止まったら、もう一度 R → スペース。毎回ちがう道になる？ 長さは？（局所最適）
-- 2. 都市を円形に並べて置く。正解（円周）にたどり着ける？
-- 3. 「いちばん近い都市へ行く」（最近傍法）で初期の道を作ると、どれだけ良くなる？

-- ---------- 道具 ----------

-- JavaScript の Math.round
roundJs :: Float -> Int
roundJs x = floor (x + 0.5)

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
