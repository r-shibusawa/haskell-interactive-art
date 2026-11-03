-- ============================================
-- 07 ネットワーク上の感染シミュレーション（SIR モデル）
-- S（未感染）→ I（感染中）→ R（回復・免疫）。感染者の隣は確率 β でうつる
-- つながり方で流行の広がり方が全然ちがう。点をクリックすると「ワクチン」（最初から R）
-- マウス左右 = 感染率 β / 1 = 格子 / 2 = ランダム / 3 = ハブあり / スペースで開始 / クリック = ワクチン
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wNodes    :: !(V.Vector Point)
  , wAdj      :: !(IM.IntMap IS.IntSet)
  , wState    :: !(U.Vector Int)            -- 0 = S, 1 = I, 2 = R
  , wRunning  :: !Bool
  , wHistory  :: ![(Int, Int, Int)]         -- S, I, R の人数（新しいものが先頭）
  , wTopology :: !Int
  }

n :: Int
n = 120                    -- ★ 点の数

gamma :: Float
gamma = 0.05               -- ★ 回復率

graphSteps :: Int
graphSteps = 200           -- グラフの横幅に入るステップ数

main :: IO ()
main = runSketch (sketch "08-07 Epidemic (SIR)" (World V.empty IM.empty (U.replicate n 0) False [] 2))
  { skBack  = hsb 220 40 10
  , skStart = \env -> build env 2
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

link :: IM.IntMap IS.IntSet -> (Int, Int) -> IM.IntMap IS.IntSet
link adj (a, b)
  | a == b = adj
  | otherwise = IM.insertWith IS.union a (IS.singleton b) (IM.insertWith IS.union b (IS.singleton a) adj)

randomPoints :: Env -> IO [Point]
randomPoints env = replicateM n $ do
  x <- randomRIO (100, envW env * 0.6)
  y <- randomRIO (130, envH env - 60)
  pure (x, y)

build :: Env -> Int -> IO World
build env topology = do
  (pts, adj) <- case topology of
    1 -> pure (latticePoints env, foldl link IM.empty latticeLinks)
    2 -> do
      pts <- randomPoints env
      targets <- replicateM (2 * n) (randomRIO (0, n - 1))
      pure (pts, foldl link IM.empty (zip (concatMap (replicate 2) [0 .. n - 1]) targets))
    _ -> do
      pts <- randomPoints env
      adj <- growHubs
      pure (pts, adj)
  first <- randomRIO (0, n - 1)                 -- 最初の感染者
  pure (World (V.fromList pts) adj (U.replicate n 0 U.// [(first, 1)]) False [] topology)

-- 格子
latticeCols :: Int
latticeCols = 12

latticePoints :: Env -> [Point]
latticePoints env =
  [ ( remap (fromIntegral (i `mod` latticeCols)) 0 (fromIntegral latticeCols - 1) 100 (envW env * 0.6)
    , remap (fromIntegral (i `div` latticeCols)) 0 lastRow 130 (envH env - 60) )
  | i <- [0 .. n - 1] ]
  where lastRow = fromIntegral (ceiling (fromIntegral n / fromIntegral latticeCols :: Float) - 1 :: Int)

latticeLinks :: [(Int, Int)]
latticeLinks = concat
  [ [(i, i + 1) | i `mod` latticeCols < latticeCols - 1] ++ [(i, i + latticeCols) | i + latticeCols < n]
  | i <- [0 .. n - 1] ]

-- ハブあり：優先的選択で成長
growHubs :: IO (IM.IntMap IS.IntSet)
growHubs = fst <$> foldM addLink (link IM.empty (0, 1), IM.fromList [(0, 1), (1, 1)]) [i | i <- [2 .. n - 1], _ <- [0, 1 :: Int]]
  where
    addLink (adj, deg) i = do
      let weight t = IM.findWithDefault 0 t deg
          total = sum (map weight [0 .. i - 1])
      r <- randomRIO (0, fromIntegral total) :: IO Float
      let t = lottery weight i r
      pure (link adj (i, t), IM.insertWith (+) i 1 (IM.insertWith (+) t 1 deg))

lottery :: (Int -> Int) -> Int -> Float -> Int
lottery weight i = go 0
  where
    go t left
      | t >= i = i - 1
      | left - fromIntegral (weight t) < 0 = t
      | otherwise = go (t + 1) (left - fromIntegral (weight t))

betaOf :: Env -> Float
betaOf env = clampF 0.02 0.4 (remap (fst (mouseXY env)) 0 (envW env) 0.02 0.4)

-- ★ 1 ステップ：感染者の隣に確率 β でうつす。感染者は確率 γ で回復
spread :: Float -> World -> IO (U.Vector Int)
spread beta w = foldM person st [i | i <- [0 .. n - 1], st U.! i == 1]
  where
    st = wState w
    person next i = do
      next1 <- foldM infect next (IS.toList (IM.findWithDefault IS.empty i (wAdj w)))
      r <- randomRIO (0, 1)
      pure (if r < gamma then next1 U.// [(i, 2)] else next1)
    infect next nb
      | st U.! nb /= 0 = pure next
      | otherwise = do
          r <- randomRIO (0, 1)
          pure (if r < beta then next U.// [(nb, 1)] else next)

counts :: U.Vector Int -> (Int, Int, Int)
counts st = (c 0, c 1, c 2)
  where c v = U.length (U.filter (== v) st)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRunning w && envFrame env `mod` 6 == 0 = do
      next <- spread (betaOf env) w
      let c@(_, infected, _) = counts next
      pure w { wState = next
             , wHistory = take (graphSteps + 40) (c : wHistory w)     -- 画面の外に出る分は持たない
             , wRunning = infected /= 0 }
  | otherwise = pure w

stateColor :: Int -> Color
stateColor 0 = hsb 200 80 100
stateColor 1 = hsb 0 80 100
stateColor _ = hsb 120 80 100

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ nodePics ++ graph ++ hud)
  where
    st = wState w
    pos i = let (x, y) = wNodes w V.! i in toG env x y
    edgePics =
      [ Color (hsb 0 0 35) (Line [pos i, pos j])
      | (i, s) <- IM.toList (wAdj w), j <- IS.toList s, j > i ]
    nodePics = map nodePic [0 .. n - 1]
    nodePic i = case st U.! i of
      0 -> Color (hsb 200 30 60) (at (pos i) (disc 5.5))
      1 -> Color (stateColor 1) (at (pos i) (disc 8))
      _ -> Color (stateColor 2) (at (pos i) (disc 5.5))
    -- ---- 右：S/I/R の推移 ----
    gx = envW env * 0.68
    gw = envW env - gx - 40
    gy = envH env * 0.25
    gh = envH env * 0.5
    hist = reverse (wHistory w)
    curve pick = [ toG env (gx + (fromIntegral t / fromIntegral graphSteps) * gw) (gy + gh - (fromIntegral (pick c) / fromIntegral n) * gh)
                 | (t, c) <- zip [0 :: Int ..] hist ]
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c
    (cs, ci, cr) = counts st
    pct = 100 * fromIntegral (ci + cr) / fromIntegral n :: Float
    graph =
      [ Color (hsb 0 0 30) (Line [toG env gx (gy + gh), toG env (gx + gw) (gy + gh)])
      , Color (stateColor 0) (thickPath 2.5 (curve fst3))
      , Color (stateColor 1) (thickPath 2.5 (curve snd3))
      , Color (stateColor 2) (thickPath 2.5 (curve thd3))
      , txt env gx (gy - 10) 14 (hsb 200 30 80) "S healthy"
      , txt env (gx + 110) (gy - 10) 14 (hsb 0 80 100) "I infected"
      , txt env (gx + 220) (gy - 10) 14 (hsb 120 80 100) "R recovered"
      , txt env gx (gy + gh + 24) 14 (hsb 0 0 70)
          ("S " ++ show cs ++ "  I " ++ show ci ++ "  R " ++ show cr ++ "  Infected so far " ++ nf 0 pct ++ " %")
      ]
    hud =
      [ txt env 20 32 18 (hsb 0 0 100) ("Infection rate beta = " ++ nf 2 (betaOf env) ++ " (mouse X)   Space: " ++ (if wRunning w then "stop" else "start"))
      , txt env 20 56 15 (hsb 0 0 100) "1: lattice   2: random   3: hubs   R: rebuild   Click node: vaccinate"
      , txt env 20 78 15 (hsb 0 0 60) "Hub networks flare up even at low beta. Vaccinating the hubs stops it best: who gets the vaccine matters."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        hits = [(i, 2) | (i, p) <- zip [0 ..] (V.toList (wNodes w)), dist2 m p < 12, wState w U.! i == 0]
    in pure w { wState = wState w U.// hits }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  EventKey (Char '1') Down _ _ -> build env 1
  EventKey (Char '2') Down _ _ -> build env 2
  EventKey (Char '3') Down _ _ -> build env 3
  EventKey (Char c) Down _ _ | c `elem` "rR" -> build env (wTopology w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ β で 1・2・3 を比べる。最終的に感染した割合は？
-- 2. ハブあり（3）で、いちばん線の多い点にワクチンを打ってから開始すると？
-- 3. 回復率 gamma を上げる（早く治る）と、流行の山はどう変わる？

-- ---------- 道具 ----------

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
