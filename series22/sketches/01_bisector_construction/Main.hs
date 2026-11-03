-- ============================================
-- 01 垂直二等分線から作る
-- 2 点 A・B から同じ距離の点の集まり = 線分 AB の「垂直二等分線」。A に近い側は半平面（平面の半分）
--   サイト A のセル ＝ 「A が B より近い半平面」を、ほかの全部のサイトについて重ねた（共通部分をとった）もの
-- スペース = 次の二等分線で切る / B = 1 つ戻る / ←→ = 注目するサイト / A = 全員のセル / ドラッグ = 動かす / クリック = 追加 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (findIndex, foldl', sortOn)
import Data.Maybe (mapMaybe)

maxSites :: Int
maxSites = 12             -- ★ クリックで増やせるサイトの上限

data World = World
  { wSites :: ![Point]    -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wFocus :: !Int        -- 注目しているサイト
  , wCut   :: !Int        -- 何本目の二等分線まで切ったか
  , wOrder :: ![Int]      -- 切る順番（近いサイトから）
  , wAll   :: !Bool       -- 全員のセルを見せる
  , wDrag  :: !Int        -- 動かしているサイト（なければ -1）
  , wAnim  :: !Float      -- 新しい線が出てくるアニメーション 0〜1
  }

main :: IO ()
main = runSketch (sketch "22-01 Bisector Construction" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> randomSites 5 world0
  , skStep  = \_ _ w -> pure w { wAnim = min 1 (wAnim w + 0.06) }
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] 0 0 [] False (-1) 0

randomSites :: Int -> World -> IO World
randomSites n w = do
  ss <- replicateM n ((,) <$> randomRIO (width * 0.2, width * 0.8) <*> randomRIO (180, height - 80))
  pure (makeOrder w { wSites = ss, wFocus = 0, wCut = 0 })

-- ★ 近いサイトから順に切ると、早くセルの形に近づく
makeOrder :: World -> World
makeOrder w = case nth ss (wFocus w) of
  Nothing -> w { wOrder = [], wCut = 0 }
  Just (ax, ay) ->
    let d2 j = case nth ss j of
          Just (x, y) -> (x - ax) * (x - ax) + (y - ay) * (y - ay)
          Nothing -> 0
        order = sortOn d2 [j | j <- [0 .. length ss - 1], j /= wFocus w]
    in w { wOrder = order, wCut = max 0 (min (length order) (wCut w)) }
  where ss = wSites w

hueOf :: Int -> Float
hueOf i = wrapF 360 (fromIntegral i * 137.5)

screenRect :: [Point]
screenRect = [(0, 90), (width, 90), (width, height), (0, height)]

-- a の側だけ残す（a と b の垂直二等分線で切る）
cutBy :: Point -> [Point] -> Point -> [Point]
cutBy (ax, ay) poly (bx, by) = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)

-- 線分 ab の垂直二等分線を長くかく
bisector :: Float -> Point -> Point -> Float -> Picture
bisector t (ax, ay) (bx, by) anim =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      dx = -(by - ay)
      dy = bx - ax
      l = max 1e-6 (sqrt (dx * dx + dy * dy))
      len = 2000 * anim
  in thickLine t (mx - dx / l * len, my - dy / l * len) (mx + dx / l * len, my + dy / l * len)

drawAll :: World -> Picture
drawAll w = Pictures [view shapes, Pictures (siteTexts), hud w]
  where
    ss = wSites w
    n = length ss
    focusI = max 0 (min (n - 1) (wFocus w))
    orderSites = mapMaybe (nth ss) (take (wCut w) (wOrder w))
    lastB = if wCut w > 0 then nth (wOrder w) (wCut w - 1) else Nothing
    shapes = case nth ss focusI of
      Nothing -> []
      Just a -> (if wAll w then allCells else construction a) ++ concat (zipWith siteDots [0 ..] ss)
    -- 全員のセル（道具の voronoiCell）
    allCells =
      [ Pictures [Color (hsbA (hueOf i) 45 60 70) (Polygon poly), Color (gray 1) (outline 2 poly)]
      | (i, s) <- zip [0 ..] ss, let poly = voronoiCell ss s 0 90 width height ]
    construction a =
      let hueF = hueOf focusI
          -- これまでに切った半平面の共通部分 = セルの途中経過
          poly = foldl' (cutBy a) screenRect orderSites
          lastSite = lastB >>= nth ss
          -- いま切っている二等分線：A の側を薄く塗る（半平面）
          half = case lastSite of
            Just b -> [Color (hsbA hueF 40 70 (18 * wAnim w)) (Polygon (cutBy a screenRect b))]
            Nothing -> []
          k1 = length orderSites - 1
          lines' = zipWith (\k b -> let isLast = k == k1 in
                     Color (hsbA 0 0 100 (if isLast then 90 else 25))
                           (bisector (if isLast then 2 else 1) a b (if isLast then wAnim w else 1))) [0 ..] orderSites
          -- 二等分線と、A と B を結ぶ点線
          mark = case lastSite of
            Just b -> [Color (hsb 50 80 100) (Pictures (dashed 2 6 6 a b : [rightAngle a b]))]
            Nothing -> []
      in half ++ [Color (hsbA hueF 55 75 45) (Polygon poly), Color (hsb hueF 30 100) (outline 4 poly)] ++ lines' ++ mark
    siteDots i s =
      let isF = i == focusI && not (wAll w)
          isB = not (wAll w) && lastB == Just i
          c | isF = hsb 50 90 100
            | isB = hsb 190 70 100
            | otherwise = gray 1
          d = if isF then 20 else 14
      in [dotAt (hsb 0 0 10) s (d + 2), dotAt c s (d - 2)]
    siteTexts =
      [ txt (x + 12) (y - 10) 16 (gray 1) (if i == focusI && not (wAll w) then "A"
                                           else if not (wAll w) && lastB == Just i then "B" else show i)
      | (i, (x, y)) <- zip [0 :: Int ..] ss ]

-- 中点の直角マーク（線分の向きに回した小さな正方形）
rightAngle :: Point -> Point -> Picture
rightAngle (ax, ay) (bx, by) =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      ang = atan2 (by - ay) (bx - ax)
      c = cos ang
      s = sin ang
      r (u, v) = (mx + u * c - v * s, my + u * s + v * c)
  in outline 1.5 (map r [(0, 0), (12, 0), (12, 12), (0, 12)])

hud :: World -> Picture
hud w = Pictures
  [ txt 20 30 16 (gray 1) (msg ++ "  A: all cells  Drag: move  Click: add  R: random")
  , txt 20 52 14 (hsb 0 0 65) tip1
  , txt 20 74 14 (hsb 0 0 65) tip2
  ]
  where
    total = length (wOrder w)
    done = wCut w >= total
    msg | wAll w = "All cells (A: back)"
        | otherwise = "Space: next cut (" ++ show (wCut w) ++ " / " ++ show total ++ ")  B: back  Left/Right: focus site"
    (tip1, tip2)
      | wAll w = ("Every cell is a convex polygon (no dents),", "because an intersection of half-planes is always convex.")
      | done = ("Done! This is the cell of yellow site A. Far sites may not change it,", "so the number of edges is at most (number of sites - 1).")
      | otherwise = ("Yellow = A, cyan = B. The line through the middle of dashed AB at a right angle is the bisector.", "The pale color is the side closer to A. The overlap of all those sides becomes the cell.")

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (press (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (x, y) = toP5 pos
    in pure (makeOrder w { wSites = setAt (wDrag w) (clampF 0 width x, clampF 90 height y) (wSites w) })
  EventKey (SpecialKey KeySpace) Down _ _
    | wCut w < length (wOrder w) -> pure w { wCut = wCut w + 1, wAnim = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ | n > 0 ->
    pure (makeOrder w { wFocus = (wFocus w + 1) `mod` n, wCut = 0 })
  EventKey (SpecialKey KeyLeft) Down _ _ | n > 0 ->
    pure (makeOrder w { wFocus = (wFocus w + n - 1) `mod` n, wCut = 0 })
  EventKey (Char c) Down _ _ -> case toLower c of
    'b' -> pure w { wCut = max 0 (wCut w - 1) }
    'a' -> pure w { wAll = not (wAll w) }
    'r' -> do k <- randomRIO (3, 8)
              randomSites k w
    _ -> pure w
  _ -> pure w
  where n = length (wSites w)

press :: Point -> World -> World
press m@(_, my) w
  | my < 90 = w
  | otherwise = case findIndex (\s -> dist2 m s < 14) (wSites w) of
      Just i -> w { wDrag = i }
      Nothing | length (wSites w) < maxSites -> makeOrder w { wSites = wSites w ++ [m] }
              | otherwise -> w

-- ---------- やってみよう ----------
-- 1. サイトを 2 個にすると、セルは「半平面」そのもの。3 個のとき、3 本の二等分線は 1 点で交わる（外心）。確かめよう
-- 2. makeOrder の並べかえをやめる（番号順に切る）と、途中経過がどう変わる？ 最後の形は同じ？
-- 3. A を囲むようにサイトを 6 個並べると、セルは何角形？ 遠くにもう 1 個置くとセルは変わる？
-- 4. 全員のセル（A）で、セルの辺の数を数えよう。平均すると何角形くらい？（06 で答え合わせ）

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

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- ==================================================
-- ボロノイ図の道具（ほかのスケッチにもそのままコピーして使える）
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく（O(n²)）
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = foldl' cut [(x0, y0), (x1, y0), (x1, y1), (x0, y1)] sites
  where
    -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
    cut poly b@(bx, by)
      | b == a || length poly <= 2 = poly
      | otherwise = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)

-- nx·x + ny·y ≦ c の側だけ残す（Sutherland–Hodgman の 1 本分）
clipHalfPlane :: [Point] -> Float -> Float -> Float -> [Point]
clipHalfPlane poly nx ny c = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    side (x, y) = nx * x + ny * y - c
    edge p@(px, py) q@(qx, qy) =
      let dp = side p
          dq = side q
          t = dp / (dp - dq)
      in [p | dp <= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | (dp < 0 && dq > 0) || (dp > 0 && dq < 0)]

-- 点線（太さ t、線の長さ on、すき間 off）
dashed :: Float -> Float -> Float -> Point -> Point -> Picture
dashed t on off a@(ax, ay) b@(bx, by) =
  let l = dist2 a b
      n = floor (l / (on + off)) :: Int
      p s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
  in if l < 1 then Blank else Pictures
       [thickLine t (p s) (p (min l (s + on))) | k <- [0 .. n], let s = fromIntegral k * (on + off)]
