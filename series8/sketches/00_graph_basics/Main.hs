-- ============================================
-- 00 グラフをつくる（点と線）
-- グラフ = 点（ノード）と、点をつなぐ線（エッジ）。友だち関係、路線図、Web のリンク、全部グラフ
-- クリックで点を置く / 点から点へドラッグで線を引く / 点の大きさ = つながりの数（次数）
-- C で消す / F で力学レイアウト ON/OFF（線でつながった点が引き合う）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V

data Node = Node { nx, ny, nvx, nvy :: !Float }

data World = World
  { wNodes    :: !(V.Vector Node)
  , wEdges    :: ![(Int, Int)]
  , wDragFrom :: !Int          -- ドラッグを始めた点（-1 = なし）
  , wForce    :: !Bool         -- 力学レイアウト
  }

main :: IO ()
main = runSketch (sketch "08-00 Make a Graph" (World V.empty [] (-1) False))
  { skBack  = hsb 220 40 10
  , skStart = \env -> pure (initial env)
  , skStep  = \env _ w -> pure (if wForce w then w { wNodes = applyForces env (wEdges w) (wNodes w) } else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- お手本：小さな友だちネットワーク
initial :: Env -> World
initial env = World (V.fromList (map place [0 .. 6])) sampleEdges (-1) False
  where
    place :: Int -> Node
    place i =
      let a = tau * fromIntegral i / 7
      in Node (envW env / 2 + cos a * 200) (envH env / 2 + sin a * 200) 0 0

sampleEdges :: [(Int, Int)]
sampleEdges = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (3, 4), (4, 5), (5, 6)]

degree :: World -> Int -> Int
degree w i = length [() | (a, b) <- wEdges w, a == i || b == i]

-- ★ 力学レイアウト：つながった点はバネで引き合い、全部の点は反発する
applyForces :: Env -> [(Int, Int)] -> V.Vector Node -> V.Vector Node
applyForces env edges ns = V.map move (V.accum addV (V.map repel ns) (concatMap spring edges))
  where
    w = envW env
    h = envH env
    repel a =
      let F2 fx fy = V.foldl' (push a) (F2 0 0) ns          -- 自分自身は dx = dy = 0 なので力は 0
          cx = (w / 2 - nx a) * 0.005                        -- 中心へ
          cy = (h / 2 - ny a) * 0.005
      in a { nvx = (nvx a + fx + cx) * 0.6, nvy = (nvy a + fy + cy) * 0.6 }
    push a (F2 sx sy) b =
      let dx = nx a - nx b
          dy = ny a - ny b
          d2 = max (dx * dx + dy * dy) 100
      in F2 (sx + dx / d2 * 3000) (sy + dy / d2 * 3000)      -- 反発
    spring (a, b) =
      let na = ns V.! a
          nb = ns V.! b
          dx = nx nb - nx na
          dy = ny nb - ny na
          d = max (sqrt (dx * dx + dy * dy)) 1
          f = (d - 120) * 0.02                               -- バネ（自然長 120）
          ux = dx / d * f
          uy = dy / d * f
      in [(a, (ux, uy)), (b, (-ux, -uy))]
    addV n (dx, dy) = n { nvx = nvx n + dx, nvy = nvy n + dy }
    move n = n { nx = clampF 30 (w - 30) (nx n + nvx n), ny = clampF 90 (h - 30) (ny n + nvy n) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ dragPic ++ nodePics ++ hud)
  where
    pos i = let n = wNodes w V.! i in toG env (nx n) (ny n)
    edgePics = [Color (hsb 0 0 60) (thickLine 2 (pos a) (pos b)) | (a, b) <- wEdges w]
    dragPic
      | wDragFrom w >= 0 = [Color (hsb 45 90 100) (thickLine 2 (pos (wDragFrom w)) (envMouse env))]
      | otherwise = []
    nodePics = concat (zipWith nodePic [0 ..] (V.toList (wNodes w)))
    nodePic i n =
      let d = degree w i
          df = fromIntegral d
          (gx, gy) = toG env (nx n) (ny n)
      in [ Color (hsb (min 360 (remap df 0 6 200 320)) 70 100) (at (gx, gy) (disc (12 + df * 4)))
         , labelC gx gy 14 (hsb 0 0 10) (show d) ]
    total = sum (map (degree w) [0 .. V.length (wNodes w) - 1])
    hud =
      [ txt env 20 30 18 (hsb 0 0 100)
          ("Nodes " ++ show (V.length (wNodes w)) ++ "  Edges " ++ show (length (wEdges w))
            ++ "  Degree sum " ++ show total ++ " (= edges x 2)")
      , txt env 20 54 15 (hsb 0 0 100)
          ("Click: node   Drag node to node: edge   F: force layout " ++ (if wForce w then "ON" else "off") ++ "   C: clear")
      , txt env 20 76 15 (hsb 0 0 60)
          "Number = degree (edges at the node). Handshake lemma: degrees always sum to 2 x edges."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < 80 -> pure w
    | otherwise ->
        let i = nearest (wNodes w) mx my
        in pure (if i < 0
                   then w { wDragFrom = i, wNodes = V.snoc (wNodes w) (Node mx my 0 0) }
                   else w { wDragFrom = i })
  EventKey (MouseButton LeftButton) Up _ _ -> pure (release w mx my)
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wNodes = V.empty, wEdges = [], wDragFrom = -1 }
    | c `elem` "fF" -> pure w { wForce = not (wForce w) }
  _ -> pure w
  where (mx, my) = mouseXY env

-- 離した点が別の点なら線を引く（同じ線は 2 本引かない）
release :: World -> Float -> Float -> World
release w mx my
  | from >= 0 && to >= 0 && to /= from && not exists = w { wDragFrom = -1, wEdges = wEdges w ++ [(from, to)] }
  | otherwise = w { wDragFrom = -1 }
  where
    from = wDragFrom w
    to = nearest (wNodes w) mx my
    exists = (from, to) `elem` wEdges w || (to, from) `elem` wEdges w

-- (x, y) の近くの点の番号（なければ -1）
nearest :: V.Vector Node -> Float -> Float -> Int
nearest ns x y = maybe (-1) id (V.findIndex (\n -> dist2 (x, y) (nx n, ny n) < 25) ns)

-- ---------- やってみよう ----------
-- 1. 自分のクラスの友だち関係を描いてみよう。いちばん次数が大きい人は誰？
-- 2. 全部の点を全部とつなぐ（完全グラフ）。n 個の点なら線は何本？ n(n−1)/2
-- 3. F で力学レイアウトにすると、つながりの強いグループが自然に集まる

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
