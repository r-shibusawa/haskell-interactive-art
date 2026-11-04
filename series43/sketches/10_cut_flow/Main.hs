-- ============================================
-- 43-10 最大流 = 最小カット
-- 水源 s から出口 t へ流せる最大の量（最大流）は、s と t を切り離すのに切るパイプの容量の最小（最小カット）に等しい
--   Ford–Fulkerson：残余グラフで s→t の道（増加路）を見つけては流す。道がなくなったら終わり
--   そのとき s から残余グラフで届く頂点の集合 S が最小カット。これは LP 双対の有名な例
-- 操作：スペース = 1 ステップ（道を探す → 流す） / A = 自動 / 辺の数字をドラッグ上下 = 容量 / R = 流れをリセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM

-- 辺：始点、終点、容量、流れ
data Edge = Edge { eU :: !Int, eV :: !Int, eC :: !Int, eF :: !Int }

-- 増加路の 1 歩：辺の番号と向き（+1 = 順方向、−1 = 戻す）
type Hop = (Int, Int)

data World = World
  { wEdges :: ![Edge]
  , wTotal :: !Int
  , wPath  :: !(Maybe [Hop])  -- いま光っている増加路
  , wDone  :: !Bool
  , wCut   :: !(Maybe [Bool]) -- s から届く頂点
  , wAuto  :: !Bool
  , wDragE :: !Int
  , wDragY :: !Float
  , wDragC :: !Int
  }

-- ★ 辺と容量（from, to, 容量）
edges0 :: [Edge]
edges0 = [ Edge u v c 0 | (u, v, c) <- [(0, 1, 7), (0, 2, 5), (1, 2, 3), (1, 3, 6), (2, 4, 8), (3, 2, 2), (3, 5, 5), (4, 3, 4), (4, 5, 7)] ]

nodeN :: Int
nodeN = 6

main :: IO ()
main = runSketch (sketch "43-10 Max flow = min cut" (World edges0 0 Nothing False Nothing False (-1) 0 0))
  { skBack  = hsb 250 40 10
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 45 == 0 then stepOnce w else w)
  , skDraw  = draw
  , skEvent = event
  }

-- 頂点の位置
nodePos :: Env -> Int -> Point
nodePos env i =
  let cx = envW env / 2
      cy = envH env / 2 + 20
      wd = min (envW env * 0.38) 480
      hg = min (envH env * 0.3) 230
  in case i of
       0 -> (cx - wd, cy)
       1 -> (cx - wd / 3, cy - hg)
       2 -> (cx - wd / 3, cy + hg)
       3 -> (cx + wd / 3, cy - hg)
       4 -> (cx + wd / 3, cy + hg)
       _ -> (cx + wd, cy)

nodeName :: Int -> String
nodeName i = case i of
  0 -> "s"
  1 -> "a"
  2 -> "b"
  3 -> "c"
  4 -> "d"
  _ -> "t"

resetFlow :: World -> World
resetFlow w = w { wEdges = [e { eF = 0 } | e <- wEdges w], wTotal = 0, wPath = Nothing, wDone = False, wCut = Nothing }

-- ★ 残余グラフで BFS：s から t への道を探す（順方向は残り容量、逆方向は流れている分だけ戻せる）
findPath :: [Edge] -> (Maybe [Hop], [Bool])
findPath es = (route, [IM.member i prev || i == 0 | i <- [0 .. nodeN - 1]])
  where
    numbered = zip [0 ..] es
    prev = bfs [0] (IM.singleton 0 (-1, 0))
    bfs [] seen = IM.delete 0 seen
    bfs (u : q) seen =
      let (seen', new) = foldl' visit (seen, []) numbered
          visit (s, acc) (i, e) =
            let (s1, acc1) = if eU e == u && eC e - eF e > 0 && not (IM.member (eV e) s)
                               then (IM.insert (eV e) (i, 1) s, eV e : acc) else (s, acc)
                (s2, acc2) = if eV e == u && eF e > 0 && not (IM.member (eU e) s1)
                               then (IM.insert (eU e) (i, -1) s1, eU e : acc1) else (s1, acc1)
            in (s2, acc2)
      in bfs (q ++ reverse new) seen'
    t = nodeN - 1
    route = if IM.member t prev then Just (walk t []) else Nothing
    walk v acc
      | v == 0 = acc
      | otherwise = case IM.lookup v prev of
          Just hop@(i, dir) -> case drop i es of
            (e : _) -> walk (if dir == 1 then eU e else eV e) (hop : acc)
            _ -> acc
          Nothing -> acc

stepOnce :: World -> World
stepOnce w
  | wDone w = w
  | otherwise = case wPath w of
      Nothing -> case findPath (wEdges w) of
        (Nothing, seen) -> w { wDone = True, wCut = Just seen, wAuto = False }
        (p, _) -> w { wPath = p }
      Just p ->
        let es = wEdges w
            room (i, dir) = case drop i es of
              (e : _) -> if dir == 1 then eC e - eF e else eF e
              _ -> 0
            delta = minimum (map room p)
            apply i e = e { eF = eF e + sum [dir * delta | (k, dir) <- p, k == i] }
        in w { wEdges = zipWith apply [0 ..] es, wTotal = wTotal w + delta, wPath = Nothing }

-- 辺の数字の位置
labelPos :: Env -> Edge -> Point
labelPos env e =
  let (ax, ay) = nodePos env (eU e)
      (bx, by) = nodePos env (eV e)
      ang = atan2 (by - ay) (bx - ax)
  in ((ax + bx) / 2 - 6 * sin ang, (ay + by) / 2 + 6 * cos ang)

draw :: Env -> World -> Picture
draw env w = view env $
  -- カットの塗り分け
  [ dot (hsbA (if s then 10 else 180) 70 100 12) x y 150
  | Just cut <- [wCut w], (i, s) <- zip [0 ..] cut, let (x, y) = nodePos env i ]
  ++ concat (zipWith edgePic [0 ..] (wEdges w))
  -- 頂点
  ++ concat [ [ dot (hsb (if i == 0 then 10 else if i == nodeN - 1 then 180 else 250) 60 (if i == 0 || i == nodeN - 1 then 100 else 40)) x y 44
              , circ 2 (hsb 0 0 100) x y 44
              , txtC x y 18 (hsb 0 0 100) (nodeName i) ]
            | i <- [0 .. nodeN - 1], let (x, y) = nodePos env i ]
  ++ [ txt 20 32 18 (hsb 0 0 100) (msg ++ "    A: auto " ++ (if wAuto w then "ON" else "off") ++ "   R: reset")
     , txt 20 58 14 (hsb 0 0 60) "Drag an edge number up/down to change its capacity (flow resets). At the end, the pipes between"
     , txt 20 78 14 (hsb 0 0 60) "vertices s can reach (coral) and cannot reach (aqua) sum to exactly the flow."
     , txt 20 (envH env - 40) 14 (hsb 0 0 50) "Primal: max flow s.t. f <= c on each edge, in = out at each vertex.   Dual: min sum c_e d_e s.t. d_e >= p_u - p_v (cut LP)."
     , txt 20 (envH env - 20) 14 (hsb 0 0 50) "Weak duality: any flow <= any cut.   Strong duality: they are equal." ]
  where
    isCutEdge e = case wCut w of
      Just cut -> reach cut (eU e) && not (reach cut (eV e))
      Nothing -> False
    reach cut i = or (take 1 (drop i cut))
    cutCap = sum [eC e | e <- wEdges w, isCutEdge e]
    msg | wDone w = "OK no augmenting path: max flow " ++ show (wTotal w) ++ " = min cut capacity " ++ show cutCap ++ " (red marks)"
        | Just _ <- wPath w = "Augmenting path found (yellow dashes) -> space to push flow"
        | otherwise = "Flow " ++ show (wTotal w) ++ "    space: find an augmenting path"
    onPath i = case wPath w of
      Just p -> any ((== i) . fst) p
      Nothing -> False
    edgePic :: Int -> Edge -> [Picture]
    edgePic i e =
      let (ax, ay) = nodePos env (eU e)
          (bx, by) = nodePos env (eV e)
          ang = atan2 (by - ay) (bx - ax)
          ox = cos ang * 26
          oy = sin ang * 26
          p1 = (ax + ox, ay + oy)
          p2@(tipX, tipY) = (bx - ox, by - oy)
          cut = isCutEdge e
          (mx, my) = ((ax + bx) / 2, (ay + by) / 2)
          (lx, ly) = labelPos env e
          -- 矢じり：先端から後ろへ 12、横へ ±6
          back d s = (tipX - 12 * cos ang + d * s * sin ang, tipY - 12 * sin ang - d * s * cos ang)
      in -- 容量 = 太さ（薄い）、流れ = 太さ（濃い）
         [ seg (2 + fromIntegral (eC e) * 1.6) (hsb 0 0 30) p1 p2 ]
         ++ [ seg (2 + fromIntegral (eF e) * 1.6) (hsbA (if cut then 55 else 180) 80 100 85) p1 p2 | eF e > 0 ]
         ++ [ dashPath 3 (hsb 55 90 100) 8 8 [p1, p2] | onPath i ]
         ++ [ seg 3 (hsb 0 90 100) (lx - 26 * sin ang, ly + 26 * cos ang) (lx + 26 * sin ang, ly - 26 * cos ang) | cut ]
         ++ [ Color (hsb 0 0 60) (Polygon [p2, back 6 1, back 6 (-1)])
            , solid (hsbA 250 40 10 85) (lx - 28) (ly - 12) 56 24
            , txtC lx ly 14 (if i == wDragE w then hsb 50 90 100 else hsb 0 0 100) (show (eF e) ++ " / " ++ show (eC e)) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> stepOnce w
  EventKey (Char k) Down _ _
    | k `elem` "aA" -> w { wAuto = not (wAuto w) }
    | k `elem` "rR" -> resetFlow w
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m@(_, my) = mouseP env
        grab acc (i, e) = if dist2 m (labelPos env e) < 22 then acc { wDragE = i, wDragY = my, wDragC = eC e } else acc
    in foldl' grab w (zip [0 ..] (wEdges w))
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragE = -1 }
  EventMotion _ | wDragE w >= 0 ->
    let (_, my) = mouseP env
        nc = max 1 (min 12 (round (fromIntegral (wDragC w) + (wDragY w - my) / 14))) :: Int
        changed = or [i == wDragE w && eC e /= nc | (i, e) <- zip [0 ..] (wEdges w)]
    in if changed
         then resetFlow w { wEdges = [if i == wDragE w then e { eC = nc } else e | (i, e) <- zip [0 ..] (wEdges w)] }
         else w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 終わったあとカットの辺を 1 本太く（容量を増やす）してもう一度。最大流は増える？ 増えないなら別の辺がボトルネック
-- 2. s から出る辺を全部細くすると、カットは s のすぐそばになる
-- 3. 逆向きに戻す辺（残余グラフの −1 方向）を使う増加路が出る容量を探そう
-- 4. 2 人のプレイヤー：片方が容量 1 を足し、片方がカットを当てるゲームにしてみよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = Translate x y (Scale 1 (-1) (labelC 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の塗りつぶした長方形
solid :: Color -> Float -> Float -> Float -> Float -> Picture
solid c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 点線の折れ線（on だけ描いて off だけ空ける）
dashPath :: Float -> Color -> Float -> Float -> [Point] -> Picture
dashPath w c on off pts = Pictures (walk on True pts)
  where
    walk left drawing (a : b : rest)
      | l <= left = [seg w c a b | drawing] ++ walk (left - l) drawing (b : rest)
      | otherwise = [seg w c a m | drawing] ++ walk (if drawing then off else on) (not drawing) (m : b : rest)
      where
        l = dist2 a b
        t = left / l
        m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
    walk _ _ _ = []

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
