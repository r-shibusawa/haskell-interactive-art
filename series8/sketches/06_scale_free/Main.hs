-- ============================================
-- 06 スケールフリー・ネットワーク（金持ちはますます金持ちに）
-- 新しい点が来るたび「すでにつながりの多い点」に優先的につながる（優先的選択）と、
-- 少数の「ハブ」と大多数の「末端」ができる。Web、SNS、航空路線、タンパク質の相互作用…
-- 自動で成長 / マウス左右 = 速さ / 1 = 優先的選択 / 2 = ランダムにつなぐ（比較） / R でやり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Node = Node { nx, ny, nvx, nvy :: !Float }

data World = World
  { wNodes :: !(V.Vector Node)
  , wEdges :: ![(Int, Int)]
  , wDeg   :: !(U.Vector Int)
  , wPref  :: !Bool             -- True = 優先的選択、False = ランダム
  }

maxN :: Int
maxN = 150                 -- ★ 点の数の上限

main :: IO ()
main = runSketch (sketch "08-06 Scale-Free Network" (World V.empty [] U.empty True))
  { skBack  = hsb 220 40 10
  , skStart = \env -> reset env True
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

reset :: Env -> Bool -> IO World
reset env pref = do
  ns <- replicateM 3 (newNode env 50)
  pure (World (V.fromList ns) [(0, 1), (1, 2), (2, 0)] (U.replicate 3 2) pref)

newNode :: Env -> Float -> IO Node
newNode env spread = do
  dx <- randomRIO (-spread, spread)
  dy <- randomRIO (-spread, spread)
  pure (Node (envW env * 0.35 + dx) (envH env * 0.55 + dy) 0 0)

-- ★ 新しい点を 2 本のエッジで追加
addNode :: Env -> World -> IO World
addNode env w = do
  node <- newNode env 200
  targets <- pickTargets IS.empty
  let ts = IS.toList targets
      i = V.length (wNodes w)
      deg1 = U.accum (+) (U.snoc (wDeg w) 0) (concat [[(i, 1), (t, 1)] | t <- ts])
  pure w { wNodes = V.snoc (wNodes w) node, wEdges = wEdges w ++ [(i, t) | t <- ts], wDeg = deg1 }
  where
    pickTargets s
      | IS.size s >= 2 = pure s
      | otherwise = do
          t <- if wPref w then lottery (wDeg w) else randomRIO (0, U.length (wDeg w) - 1)
          pickTargets (IS.insert t s)

-- 次数に比例した確率で選ぶ（くじ：次数ぶんの札を入れる）
lottery :: U.Vector Int -> IO Int
lottery deg = do
  r <- randomRIO (0, fromIntegral (U.sum deg)) :: IO Float
  let go i left
        | i >= U.length deg = U.length deg - 1
        | left - fromIntegral (deg U.! i) < 0 = i
        | otherwise = go (i + 1) (left - fromIntegral (deg U.! i))
  pure (go 0 r)

-- マウス左右 = 速さ（1〜5）
speedOf :: Env -> Int
speedOf env = floor (clampF 1 6 (remap (fst (mouseXY env)) 0 (envW env) 1 6))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  let grow = V.length (wNodes w) < maxN && envFrame env `mod` max 1 (7 - speedOf env) == 0
  w1 <- if grow then addNode env w else pure w
  pure w1 { wNodes = forces env (wEdges w1) (wNodes w1) }

-- 力学レイアウト：中心への力 ＋ 反発 ＋ バネ
forces :: Env -> [(Int, Int)] -> V.Vector Node -> V.Vector Node
forces env edges ns = V.map move (V.accum addV (V.map repel ns) (concatMap spring edges))
  where
    repel a =
      let F2 fx fy = V.foldl' (push a) (F2 ((envW env * 0.35 - nx a) * 0.003) ((envH env * 0.55 - ny a) * 0.003)) ns
      in a { nvx = (nvx a + fx) * 0.5, nvy = (nvy a + fy) * 0.5 }
    push a (F2 sx sy) b =
      let dx = nx a - nx b
          dy = ny a - ny b
          d2 = max (dx * dx + dy * dy) 64
      in F2 (sx + dx / d2 * 400) (sy + dy / d2 * 400)
    spring (a, b) =
      let na = ns V.! a
          nb = ns V.! b
          dx = nx nb - nx na
          dy = ny nb - ny na
          d = max (sqrt (dx * dx + dy * dy)) 1
          f = (d - 40) * 0.01
          ux = dx / d * f
          uy = dy / d * f
      in [(a, (ux, uy)), (b, (-ux, -uy))]
    addV n (dx, dy) = n { nvx = nvx n + dx, nvy = nvy n + dy }
    move n = n { nx = nx n + nvx n, ny = ny n + nvy n }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ nodePics ++ histogram ++ hud)
  where
    deg = wDeg w
    count = V.length (wNodes w)
    maxDeg = U.foldl' max 0 deg
    zoom = viewScale env (wNodes w)
    cx = envW env * 0.35
    cy = envH env * 0.55
    -- 力の計算はそのままで、表示だけ縮めて全部の点を画面に入れる
    pos i =
      let n = wNodes w V.! i
      in toG env (cx + (nx n - cx) * zoom) (cy + (ny n - cy) * zoom)
    edgePics = [Color (hsba 0 0 40 0.6) (Line [pos a, pos b]) | (a, b) <- wEdges w]
    nodeHue d | maxDeg <= 2 = 200
              | otherwise = remap (fromIntegral d) 2 (fromIntegral maxDeg) 200 40
    nodePics =
      [ Color (hsb (nodeHue d) 80 100) (at (pos i) (disc (3 + sqrt (fromIntegral d) * 2.5)))
      | (i, d) <- zip [0 ..] (U.toList deg), i < count ]
    -- ---- 次数分布（右）：両対数で描く ----
    hist = IM.fromListWith (+) [(d, 1 :: Int) | d <- U.toList deg]
    rx = envW env * 0.68
    rw = envW env - rx - 50
    base = envH env * 0.8
    top = envH env * 0.25
    logF :: Int -> Float
    logF = log . fromIntegral
    histPoint (d, c) =
      let x = remap (logF d) 0 (logF (max maxDeg 2 + 1)) rx (rx + rw)
          y = remap (logF c) 0 (logF count) base top
      in Color (hsb 45 90 100) (at (toG env x y) (disc 4))
    histogram =
      [ Color (hsb 0 0 30) (Line [toG env rx base, toG env (rx + rw) base])
      , Color (hsb 0 0 30) (Line [toG env rx base, toG env rx top])
      ] ++ map histPoint (IM.toList hist) ++
      [ txt env (rx + rw - 130) (base + 22) 14 (hsb 0 0 60) "Degree -> (log)"
      , txt env rx (top - 10) 14 (hsb 0 0 60) "Node count (log)"
      , txt env rx (top + 20) 14 (hsb 0 0 60) (if wPref w then "Near a line = power law (scale-free)" else "A compact hump = random graph")
      ]
    hud =
      [ txt env 20 32 18 (hsb 0 0 100)
          ((if wPref w then "Preferential: popular nodes get picked more" else "Random: every node equally likely")
            ++ "   Nodes " ++ show count ++ "   Max degree " ++ show maxDeg)
      , txt env 20 56 15 (hsb 0 0 100) "1: preferential   2: random   R: restart   Mouse X: speed"
      , txt env 20 78 15 (hsb 0 0 60) "Barabasi-Albert (1999). Hubs (red) form because new nodes choose the popular ones, like top search results."
      ]

-- 反発が強く、点が増えると広がって画面の外へ出る。全部が左側の領域に入る倍率（最大 1）を求める
viewScale :: Env -> V.Vector Node -> Float
viewScale env ns = min 1 (min (400 / max 1 far) (340 / max 1 tall))
  where
    far = V.foldl' (\m n -> max m (abs (nx n - envW env * 0.35))) 0 ns
    tall = V.foldl' (\m n -> max m (abs (ny n - envH env * 0.55))) 0 ns

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char '1') Down _ _ -> reset env True
  EventKey (Char '2') Down _ _ -> reset env False
  EventKey (Char c) Down _ _ | c `elem` "rR" -> reset env (wPref w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 と 2 で、右のグラフの形を比べる。優先的選択だけが「直線」になる
-- 2. ハブを 1 つ壊すと（エッジを消す）ネットワークはどうなる？ 末端を壊すと？（攻撃への弱さ）
-- 3. 新しい点のエッジ数 2 を 1 にすると木（ツリー）になる

-- ---------- 道具 ----------

-- 力を足し合わせるための組（正格なので速い）
data F2 = F2 !Float !Float

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
