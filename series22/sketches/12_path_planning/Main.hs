-- ============================================
-- 12 経路計画：ボロノイ辺に沿って
-- 障害物と壁のふちに点を並べてボロノイ図を作ると、その辺は「左右の障害物からちょうど真ん中」を通る道になる
--   = いちばん安全な（障害物から遠い）道の網。その網の上をダイクストラ法で探すと、ぶつかりにくい経路が見つかる
-- クリック = 障害物を置く / S・G = マウスの位置にスタート・ゴール / ↑↓ = ロボットの半径 / スペース = 走らせる / V = 網の表示 / R = ランダム / C = 障害物を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl', partition, sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Float (double2Float, float2Double)
import Text.Printf (printf)

-- 動ける範囲
bx0, by0, bx1, by1 :: Float
bx0 = 20
by0 = 100
bx1 = width - 20
by1 = height - 20

data Ob = Ob { oX, oY, oR :: !Float }       -- 障害物

data Node = Node { nX, nY, nC :: !Float }   -- 道の網のノード = ボロノイの頂点（nC はすき間の広さ）

data Edge = Edge { eU, eV :: !Int, eOk :: !Bool, eC :: !Float }

data World = World
  { wObs    :: ![Ob]
  , wStart  :: !Point
  , wGoal   :: !Point
  , wRobotR :: !Float     -- ★ ロボットの半径（これより狭いすき間は通れない）
  , wNodes  :: !(V.Vector Node)
  , wEdges  :: ![Edge]
  , wPath   :: ![Point]
  , wLen    :: !Float
  , wHits   :: ![(Float, Float, Int)]   -- まっすぐの線がぶつかる所（x, y, 何番目の点か）
  , wDirty  :: !Bool
  , wNet    :: !Bool
  , wRobot  :: !(Maybe Float)   -- 走っているロボットの進んだ距離
  }

main :: IO ()
main = runSketch (sketch "22-12 Path Planning" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> randomObstacles world0
  , skStep  = \_ _ w -> pure (moveRobot (if wDirty w then rebuild w else w))
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World [] (bx0 + 50, (by0 + by1) / 2) (bx1 - 50, (by0 + by1) / 2) 18 V.empty [] [] 0 [] True True Nothing

randomObstacles :: World -> IO World
randomObstacles w = do
  let go :: Int -> [Ob] -> IO [Ob]
      go guard os
        | length os >= 14 || guard >= 500 = pure os
        | otherwise = do
            x <- randomRIO (bx0 + 60, bx1 - 60)
            y <- randomRIO (by0 + 40, by1 - 40)
            r <- randomRIO (25, 70)
            let p = (x, y)
                clash = dist2 p (wStart w) < r + 60 || dist2 p (wGoal w) < r + 60
                        || any (\q -> dist2 p (oX q, oY q) < r + oR q + 15) os
            go (guard + 1) (if clash then os else os ++ [Ob x y r])
  os <- go 0 []
  pure w { wObs = os, wDirty = True }

-- 障害物・壁からの距離（すき間の広さ）
clearance :: [Ob] -> Float -> Float -> Float
clearance obs x y = foldl' (\c o -> min c (dist2 (x, y) (oX o, oY o) - oR o)) walls obs
  where walls = min (x - bx0) (min (bx1 - x) (min (y - by0) (by1 - y)))

segClear :: [Ob] -> Float -> Point -> Point -> Bool
segClear obs robotR a@(ax, ay) b@(bx, by) =
  let n = max 1 (ceiling (dist2 a b / 6)) :: Int
      ok k = let t = fromIntegral k / fromIntegral n
             in clearance obs (lerpF ax bx t) (lerpF ay by t) >= robotR
  in all ok [0 .. n]

-- ① 障害物のふちと壁に点を並べる（持ち主の番号つき）
borderPoints :: [Ob] -> [(Point, Int)]
borderPoints obs = concat (zipWith ring' [0 ..] obs) ++ walls
  where
    ring' k o =
      let m = max 8 (ceiling (tau * oR o / 18)) :: Int
      in [ ((oX o + oR o * cos a, oY o + oR o * sin a), k)
         | i <- [0 .. m - 1], let a = tau * fromIntegral i / fromIntegral m ]
    walls =
      concat [ [((x, by0), -1), ((x + 15, by1), -3)] | x <- [bx0, bx0 + 30 .. bx1] ]
      ++ concat [ [((bx0, y), -4), ((bx1, y + 7), -2)] | y <- takeWhile (< by1) [by0 + 15, by0 + 45 ..] ]

rebuild :: World -> World
rebuild w = findPath w { wNodes = nodes, wEdges = edges, wHits = hits, wDirty = False }
  where
    obs = wObs w
    robotR = wRobotR w
    both = borderPoints obs
    pts = map fst both
    pv = U.fromList pts
    owner = U.fromList (map snd both)
    -- ② ドロネー → となりの三角形の外心どうしを結ぶ = ボロノイの辺
    tris = delaunay pts
    nodes = V.fromList
      [ Node x y (clearance obs x y)
      | (a, b, c) <- tris
      , let (cx, cy, _) = circumcircle (toPD (pv U.! a)) (toPD (pv U.! b)) (toPD (pv U.! c))
            x = double2Float cx
            y = double2Float cy ]
    em = foldl' addTri M.empty (zip [0 ..] tris)
    addTri m (ti, (a, b, c)) = foldl' (\mm (p, q) -> M.insertWith (++) (min p q, max p q) [ti] mm) m [(a, b), (b, c), (c, a)]
    edges =
      [ Edge u v ok (min (nC na) (min (nC nb) mc))
      | ((a, b), [v, u]) <- M.toList em
      , owner U.! a /= owner U.! b                -- 同じ障害物どうし → 障害物の中の辺なので使わない
      , let na = nodes V.! u
            nb = nodes V.! v
            mc = clearance obs ((nX na + nX nb) / 2) ((nY na + nY nb) / 2)
            ok = nC na >= robotR && nC nb >= robotR && mc >= robotR ]
    -- まっすぐの線がぶつかる所
    (sx, sy) = wStart w
    (gx, gy) = wGoal w
    nHit = ceiling (dist2 (wStart w) (wGoal w) / 10) :: Int
    hits =
      [ (x, y, k)
      | k <- [0 .. nHit]
      , let t = fromIntegral k / fromIntegral (max 1 nHit)
            x = lerpF sx gx t
            y = lerpF sy gy t
      , clearance obs x y < robotR ]

-- ③ スタート・ゴールを近くのノードにつないで、ダイクストラ法
findPath :: World -> World
findPath w
  | clearance obs (fst st) (snd st) < robotR || clearance obs (fst gl) (snd gl) < robotR = w { wPath = [], wLen = 0 }
  | otherwise = case IM.lookup g dists of
      Nothing -> w { wPath = [], wLen = 0 }
      Just (dg, _) -> w { wPath = walk g [], wLen = dg }
  where
    obs = wObs w
    robotR = wRobotR w
    st = wStart w
    gl = wGoal w
    nodes = wNodes w
    n = V.length nodes
    s = n
    g = n + 1
    posOf i = let nd = nodes V.! i in (nX nd, nY nd)
    link p = take 3 (filter (\(i, _) -> segClear obs robotR p (posOf i)) (take 12 (sortOn snd
      [ (i, dist2 p (nX nd, nY nd)) | (i, nd) <- zip [0 ..] (V.toList nodes), nC nd >= robotR ])))
    fromS = link st
    toG = link gl
    -- 通れる辺だけの隣接リスト
    adj = IM.fromListWith (++) (concat
      [ [(eU e, [(eV e, l)]), (eV e, [(eU e, l)])]
      | e <- wEdges w, eOk e, let l = dist2 (posOf (eU e)) (posOf (eV e)) ]
      ++ [(i, [(g, l)]) | (i, l) <- toG]
      ++ [(s, fromS)])
    -- dists：ノード → (距離, ひとつ前のノード)
    dists = dijkstra (S.singleton (0, s)) (IM.singleton s (0, -1))
    dijkstra frontier best = case S.minView frontier of
      Nothing -> best
      Just ((d, u), rest)
        | u == g -> best
        | d > maybe (1 / 0) fst (IM.lookup u best) -> dijkstra rest best
        | otherwise ->
            let relax (fr, bs) (v, l) =
                  let nd = d + l
                  in if nd < maybe (1 / 0) fst (IM.lookup v bs)
                       then (S.insert (nd, v) fr, IM.insert v (nd, u) bs)
                       else (fr, bs)
                (fr', bs') = foldl' relax (rest, best) (IM.findWithDefault [] u adj)
            in dijkstra fr' bs'
    walk v acc
      | v < 0 = acc
      | otherwise =
          let p | v == s = st
                | v == g = gl
                | otherwise = posOf v
              prev = maybe (-1) snd (IM.lookup v dists)
          in walk prev (p : acc)

pointAt :: World -> Float -> Point
pointAt w dd0 = go dd0 (wPath w)
  where
    go dd (a@(ax, ay) : rest@(b@(bx, by) : _)) =
      let l = dist2 a b
      in if dd <= l then (if l > 0 then (lerpF ax bx (dd / l), lerpF ay by (dd / l)) else a)
         else go (dd - l) rest
    go _ [p] = p
    go _ _ = wStart w

moveRobot :: World -> World
moveRobot w = case wRobot w of
  Just d | not (null (wPath w)) ->
    let d' = d + 3
    in w { wRobot = if d' > wLen w + 30 then Nothing else Just d' }
  _ -> w

drawAll :: World -> Picture
drawAll w = Pictures
  [ view ([Color (hsb 0 0 60) (outline 2 [(bx0, by0), (bx1, by0), (bx1, by1), (bx0, by1)])]
          ++ net
          ++ [dotAt (hsb 230 15 50) (oX o, oY o) (oR o * 2) | o <- wObs w]
          ++ [Color (hsbA 0 70 100 70) (dashed 2 8 8 st gl)]
          ++ crosses
          ++ pathPic
          ++ robotPic
          ++ [dotAt (hsb 130 80 100) st 18, dotAt (hsb 0 80 100) gl 18])
  , labelAtP st "S"
  , labelAtP gl "G"
  , txt 20 30 16 (gray 1) ("Click: obstacle  S,G: start,goal  Up/Down: robot radius " ++ show (round (wRobotR w) :: Int) ++ "  Space: run  V: net " ++ onOff (wNet w) ++ "  R: random  C: clear")
  , txt 20 52 14 (hsb 0 0 65) ("Yellow = path along Voronoi edges " ++ (if null (wPath w) then "(not found: gaps too narrow)" else printf "%.0f px" (wLen w))
      ++ ".  Red dashes = straight " ++ printf "%.0f" (dist2 st gl) ++ " px (" ++ show (length (wHits w)) ++ " hits).")
  , txt 20 74 14 (hsb 0 0 65) "Net color = gap width (greener is wider). It may detour, but it stays far from obstacles."
  ]
  where
    st = wStart w
    gl = wGoal w
    labelAtP (x, y) s = txt (x - 5) (y + 7) 14 (hsb 0 0 10) s
    nodes = wNodes w
    -- 一直線に近い 3 点の外心はとても遠くなるので描かない
    near nd = abs (nX nd) < 1e5 && abs (nY nd) < 1e5
    -- 道の網：色 = すき間の広さ（赤 = 狭い → 緑 = 広い）。通れない辺は薄く
    net | wNet w = concatMap edgePic (wEdges w)
        | otherwise = []
    edgePic e =
      let a = nodes V.! eU e
          b = nodes V.! eV e
          pa = (nX a, nY a)
          pb = (nX b, nY b)
      in if not (near a && near b) then []
         else if eOk e
           then [Color (hsbA (clampF 0 140 (remap (eC e) (wRobotR w) 120 0 140)) 70 90 80) (thickLine 2 pa pb)]
           else [Color (hsbA 0 0 100 12) (Line [pa, pb])]
    -- まっすぐの線（ぶつかる所に ×）
    crosses = concat
      [ [ Color (hsb 0 90 100) (thickLine 2 (x - 4, y - 4) (x + 4, y + 4))
        , Color (hsb 0 90 100) (thickLine 2 (x - 4, y + 4) (x + 4, y - 4)) ]
      | (x, y, k) <- wHits w, even k ]       -- × は 1 つおき
    -- 見つけた道
    pathPic = [Color (hsb 50 90 100) (thickPath 5 (wPath w)) | not (null (wPath w))]
    -- ロボット
    robotPic = case wRobot w of
      Just d | not (null (wPath w)) ->
        let p = pointAt w d
        in [dotAt (hsbA 190 80 100 50) p (wRobotR w * 2), dotAt (hsb 190 80 100) p 10]
      _ -> []

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let (x, y) = toP5 pos
    if inBox (x, y)
      then do
        r <- randomRIO (25, 60)
        pure w { wObs = wObs w ++ [Ob x y r], wDirty = True }
      else pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wRobotR = min 60 (wRobotR w + 3), wDirty = True }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wRobotR = max 3 (wRobotR w - 3), wDirty = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRobot = Just 0 }
  EventKey (Char c) Down _ _ -> case toLower c of
    's' | inBox m -> pure w { wStart = m, wDirty = True }
    'g' | inBox m -> pure w { wGoal = m, wDirty = True }
    'v' -> pure w { wNet = not (wNet w) }
    'r' -> randomObstacles w
    'c' -> pure w { wObs = [], wDirty = True }
    _ -> pure w
  _ -> pure w
  where
    m = toP5 (envMouse env)
    inBox (x, y) = x > bx0 && x < bx1 && y > by0 && y < by1

-- ---------- やってみよう ----------
-- 1. ロボットの半径を大きくしていくと、狭いすき間の辺が消えて道が遠回りになる。道がなくなる半径は？
-- 2. 辺の重みを「長さ」でなく「長さ ÷ すき間」にすると、もっと安全側を通る道になる。試そう
-- 3. シリーズ 21 の A*（格子）や RRT と比べると？ ボロノイの道は「最短」ではないが「いちばん安全」
-- 4. 障害物を四角形や壁の線にしても、ふちに点を並べれば同じ方法が使える（一般化ボロノイ図）

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 外接円 (x, y, r2) は桁が大きくなるので Double で計算する（r2 = 半径の 2 乗。3 点が一直線なら無限大）
type PD = (Double, Double)

circumcircle :: PD -> PD -> PD -> (Double, Double, Double)
circumcircle (ax, ay) (bx, by) (cx, cy)
  | abs d < 1e-9 = ((ax + bx + cx) / 3, (ay + by + cy) / 3, 1 / 0)
  | otherwise = (x, y, (ax - x) * (ax - x) + (ay - y) * (ay - y))
  where
    d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
    a2 = ax * ax + ay * ay
    b2 = bx * bx + by * by
    c2 = cx * cx + cy * cy
    x = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
    y = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d

-- ==================================================
-- ドロネーの道具（外接円の計算は桁が大きいので Double で行う）
--   delaunay pts … ドロネー三角形分割（Bowyer–Watson 法）。頂点番号の組を返す
-- ==================================================

toPD :: Point -> PD
toPD (x, y) = (float2Double x, float2Double y)

data Tri = Tri !Int !Int !Int !Double !Double !Double

-- Bowyer–Watson：点を 1 つずつ入れ、「外接円の中に新しい点がある三角形」を消して穴を新しい点とつなぐ
delaunay :: [Point] -> [(Int, Int, Int)]
delaunay pts
  | n < 3 = []
  | otherwise = [(a, b, c) | Tri a b c _ _ _ <- foldl' insert [mk n (n + 1) (n + 2)] [0 .. n - 1], a < n, b < n, c < n]
  where
    n = length pts
    ds = map toPD pts
    x0 = minimum (map fst ds)
    x1 = maximum (map fst ds)
    y0 = minimum (map snd ds)
    y1 = maximum (map snd ds)
    m = maximum [x1 - x0, y1 - y0, 1] * 20
    mx = (x0 + x1) / 2
    my = (y0 + y1) / 2
    -- 全部を囲む大きな三角形を後ろに足す
    pv = U.fromList (ds ++ [(mx - m, my - m), (mx + m, my - m), (mx, my + m)]) :: U.Vector PD
    mk i j k = let (x, y, r2) = circumcircle (pv U.! i) (pv U.! j) (pv U.! k) in Tri i j k x y r2
    insert tris i =
      let (px, py) = pv U.! i
          inside (Tri _ _ _ x y r2) = (px - x) * (px - x) + (py - y) * (py - y) < r2
          (bad, keep) = partition inside tris
          -- 2 回出た辺 = 穴の内側なので消す。1 回だけの辺が穴のふち
          toggle e (a, b) = let k = (min a b, max a b)
                            in if M.member k e then M.delete k e else M.insert k (a, b) e
          rim = foldl' toggle M.empty (concat [[(a, b), (b, c), (c, a)] | Tri a b c _ _ _ <- bad])
      in [mk a b i | (a, b) <- M.elems rim] ++ keep

-- 点線（太さ t、線の長さ on、すき間 off）
dashed :: Float -> Float -> Float -> Point -> Point -> Picture
dashed t on off a@(ax, ay) b@(bx, by) =
  let l = dist2 a b
      n = floor (l / (on + off)) :: Int
      p s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
  in if l < 1 then Blank else Pictures
       [thickLine t (p s) (p (min l (s + on))) | k <- [0 .. n], let s = fromIntegral k * (on + off)]
