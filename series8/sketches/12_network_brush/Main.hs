-- ============================================
-- 12 ネットワークを育てる（自由制作のベース）
-- クリックで点を置くと、近くの点と自動でつながり、バネで動きながら形をつくる
-- つながった瞬間に音が鳴り、次数が大きい点ほど大きく・明るく
-- クリック／ドラッグで点を置く / マウスを押したまま：引力 / C で消す / S で保存
-- ============================================
-- ★ のついた数字を変えて、自分のネットワーク・アートに！
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.List (sortOn)
import qualified Data.Vector as V

data Node = Node { nx, ny, nvx, nvy, nHue :: !Float, nDeg :: !Int }

data World = World
  { wNodes :: !(V.Vector Node)
  , wEdges :: ![(Int, Int)]
  , wTrail :: ![V.Vector Node]     -- 少し前のフレームの点（残像用。新しいものが先頭）
  }

linkDist :: Float
linkDist = 110             -- ★ この距離より近い点どうしはつながる

maxLinks :: Int
maxLinks = 4               -- ★ 1 点あたりの最大のつながり

maxNodes :: Int
maxNodes = 300             -- ★ 点の数の上限

trailFrames :: Int
trailFrames = 3            -- ★ 残像のフレーム数（前のフレームほど薄くなる）

main :: IO ()
main = runSketch (sketch "08-12 Network Brush" (World V.empty [] []))
  { skBack  = hsb 230 40 8
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 新しい点を置いて、近い点とつなぐ
addNode :: Env -> Float -> Float -> World -> IO World
addNode env x y w
  | V.length (wNodes w) >= maxNodes = pure w
  | otherwise = do
      let i = V.length (wNodes w)
          ns = wNodes w
          near = take maxLinks (sortOn snd
            [ (j, d)
            | (j, p) <- zip [0 ..] (V.toList ns)
            , let d = dist2 (x, y) (nx p, ny p)
            , d < linkDist, nDeg p < maxLinks ])
          targets = map fst near
          hue = fromIntegral ((envFrame env * 2) `mod` 360)
          new = Node x y 0 0 hue (length targets)
          ns1 = V.accum (\p k -> p { nDeg = nDeg p + k }) ns [(j, 1) | j <- targets]
      forM_ targets $ \j ->                                  -- ★ つながると音
        playTone (220 * 2 ** (fromIntegral (nDeg (ns1 V.! j) `mod` 8) * (2 / 12))) 0.12 0.4 Triangle
      pure w { wNodes = V.snoc ns1 new, wEdges = wEdges w ++ [(i, j) | j <- targets] }

-- ★ 力：反発 ＋ バネ ＋（押している間）マウスへの引力
forces :: Env -> [(Int, Int)] -> V.Vector Node -> V.Vector Node
forces env edges ns = V.map move (V.accum addV (V.map repel ns) (concatMap spring edges))
  where
    (mx, my) = mouseXY env
    repel a =
      let F2 fx fy = V.foldl' (push a) (F2 0 0) ns
          ax | envDown env = (mx - nx a) * 0.004
             | otherwise = 0
          ay | envDown env = (my - ny a) * 0.004
             | otherwise = 0
      in a { nvx = (nvx a + fx + ax) * 0.85, nvy = (nvy a + fy + ay) * 0.85 }  -- ★ 摩擦
    push a (F2 sx sy) b =
      let dx = nx a - nx b
          dy = ny a - ny b
          d2 = max (dx * dx + dy * dy) 64
      in F2 (sx + dx / d2 * 500) (sy + dy / d2 * 500)                           -- ★ 反発の強さ
    spring (i, j) =
      let a = ns V.! i
          b = ns V.! j
          dx = nx b - nx a
          dy = ny b - ny a
          d = max (sqrt (dx * dx + dy * dy)) 1
          f = (d - 70) * 0.02                                                   -- ★ バネの自然長 70
          ux = dx / d * f
          uy = dy / d * f
      in [(i, (ux, uy)), (j, (-ux, -uy))]
    addV a (dx, dy) = a { nvx = nvx a + dx, nvy = nvy a + dy }
    move a = a { nx = clampF 10 (envW env - 10) (nx a + nvx a), ny = clampF 60 (envH env - 10) (ny a + nvy a) }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = pure w
  { wNodes = forces env (wEdges w) (wNodes w)
  , wTrail = take trailFrames (wNodes w : wTrail w) }

-- ★ 残像：p5.js の background(…, 50) は 1 フレームごとに前の絵を半分の濃さにする。
--   ここでは前のフレームの点と線を、1/8、1/4、1/2 の濃さで重ねて同じ見た目にする
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (ghosts ++ layer 1 (wNodes w) ++ hud)
  where
    ghosts = concat (reverse (zipWith layer (iterate (/ 2) 0.5) (wTrail w)))
    layer k ns = map (edgePic k ns) (wEdges w) ++ map (nodePic k) (V.toList ns)
    pos a = toG env (nx a) (ny a)
    edgePic k ns (i, j) = case (ns V.!? i, ns V.!? j) of
      (Just a, Just b) -> Color (hsba ((nHue a + nHue b) / 2) 50 80 (0.6 * k)) (thickLine 1.5 (pos a) (pos b))  -- ★ 線の太さ
      _ -> Blank
    nodePic k a =
      let d = fromIntegral (nDeg a)
      in Color (hsba (nHue a) 70 (60 + d * 10) k) (at (pos a) (disc (4 + d * 2)))                             -- ★ 次数で大きさ
    hud =
      [ txt env 16 28 16 (hsb 0 0 100)
          ("Click/drag: add nodes (auto-link)   Hold: attract   C: clear   S: save   Nodes "
            ++ show (V.length (wNodes w)) ++ "   Edges " ++ show (length (wEdges w))) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my > 44 -> addNode env mx my w
    | otherwise -> pure w
  EventMotion _ | envDown env && envFrame env `mod` 4 == 0 -> do
    dx <- randomRIO (-10, 10)
    dy <- randomRIO (-10, 10)
    addNode env (mx + dx) (my + dy) w
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wNodes = V.empty, wEdges = [], wTrail = [] }
    | c `elem` "sS" -> do
        saveSvg "network" env (drawWorld env w)
        pure w
  _ -> pure w
  where (mx, my) = mouseXY env

-- ---------- 自由制作のアイデア ----------
-- ・linkDist を大きく／小さく、maxLinks を 2（鎖）や 8（密）に
-- ・線を 10 の「音楽グラフ」にして、点を置くたびにメロディが伸びる
-- ・11 のマイクで、声で連鎖を走らせる（色が波のように広がる）
-- ・06 のように、置いた点が「人気の点」を選んでつながる（ハブができる）

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
