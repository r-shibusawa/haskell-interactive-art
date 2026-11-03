-- ============================================
-- 01 感度分析とシャドウプライス
-- 00 の生産計画で、資源の量を少し増やすと最適な利益はいくら増える？ その「1 単位あたりの値打ち」がシャドウプライス（影の価格）
--   余っている資源のシャドウプライスは 0。値打ちが変わらない資源量の範囲を「レンジ」という
-- ←→ = 木材 / ↑↓ = 作業時間 / Z・X = 塗装ライン / 1〜3 = 右のグラフに出す資源 / R = 元に戻す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Ord (comparing)
import Data.List (foldl', sortBy)
import Data.Char (toLower)

profitA, profitB :: Double
profitA = 30                           -- ★ 1 個あたりの利益（千円）
profitB = 40

-- | 資源：a·x + b·y ≤ cap（cap0 は最初の量）
data Res = Res { rName :: String, rA, rB, rCap0 :: !Double, rUnit :: String, rHue :: !Float }

resources :: [Res]
resources =                            -- ★ 資源
  [ Res "Wood" 2 3 120 "kg" 30
  , Res "Hours" 2 1 80 "h" 200
  , Res "Paint" 0 1 35 "units" 300
  ]

xMax, yMax :: Double
xMax = 80
yMax = 60

-- | 資源量が変わったときだけ計算し直す結果
data Solved = Solved
  { sPoly :: [(Double, Double)]        -- 実行可能領域
  , sBest :: !(Maybe (Double, Double, Double))   -- 最適解 (x, y, z)
  , sShadow :: [Double]                -- 各資源のシャドウプライス
  , sRange :: [(Double, Double)]       -- 各資源のレンジ
  , sCurve :: [(Double, Double)]       -- 右のグラフ：資源量 → 最適な利益
  }

data World = World
  { wCaps :: ![Double]                 -- 今の資源量
  , wSel :: !Int                       -- グラフに出す資源
  , wTrail :: ![(Double, Double)]      -- 最適解の通った跡（新しい順、200 点まで）
  , wS :: !Solved
  }

world0 :: World
world0 = refresh (World (map rCap0 resources) 0 [] (solveAll (map rCap0 resources) 0))

main :: IO ()
main = runSketch (sketch "30-01 Sensitivity and Shadow Price" world0)
  { skBack  = hsb 225 30 12
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 資源量 caps のときの最適解（頂点を全部調べる）
solve :: [Double] -> ([(Double, Double)], Maybe (Double, Double, Double))
solve caps =
  let cons = zipWith (\r c -> (rA r, rB r, c)) resources caps ++ [(-1, 0, 0), (0, -1, 0)]
      poly = feasibleVertices cons
  in (poly, bestVertex profitA profitB poly)

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

optValue :: [Double] -> Int -> Double -> Double
optValue caps i c = case snd (solve (setAt i c caps)) of
  Just (_, _, z) -> z
  Nothing -> 0

-- ★ シャドウプライス：資源 i を少しだけ増やしたときの利益の増え方（微分）
shadow :: [Double] -> Int -> Double -> Double
shadow caps i c =
  let d = 0.01
      lo = max 0 (c - d)
  in (optValue caps i (c + d) - optValue caps i lo) / (c + d - lo)

-- シャドウプライスが変わらない資源量の範囲（レンジ）
ranging :: [Double] -> Int -> (Double, Double)
ranging caps i =
  let c = caps !!! i
      limit = rCap0 (resources !!! i) * 3
      s0 = shadow caps i c
      same t = abs (shadow caps i t - s0) < 1e-3
      walkLo v = if v > 0 && same (v - 0.25) then walkLo (v - 0.25) else v
      walkHi v = if v < limit && same (v + 0.25) then walkHi (v + 0.25) else v
      lo0 = walkLo c
      hi0 = walkHi c
      -- 境界を二分法で細かく探す（変わる点は ±0.01 くらいの精度）
      bisectLo :: Int -> Double -> Double -> Double
      bisectLo 0 _ b = b
      bisectLo n a b = let m = (a + b) / 2 in if same m then bisectLo (n - 1) a m else bisectLo (n - 1) m b
      bisectHi :: Int -> Double -> Double -> Double
      bisectHi 0 a _ = a
      bisectHi n a b = let m = (a + b) / 2 in if same m then bisectHi (n - 1) m b else bisectHi (n - 1) a m
      lo = if lo0 > 0 then bisectLo 12 (lo0 - 0.25) lo0 - 0.01 else lo0
      hi = if hi0 < limit then bisectHi 12 hi0 (hi0 + 0.25) + 0.01 else hi0
  in (max 0 lo, hi)

-- 結果を全部求める
solveAll :: [Double] -> Int -> Solved
solveAll caps sel =
  let (poly, best) = solve caps
      n = length resources
      cMax = rCap0 (resources !!! sel) * 3
  in Solved
       { sPoly = poly
       , sBest = best
       , sShadow = [shadow caps i (caps !!! i) | i <- [0 .. n - 1]]
       , sRange = [ranging caps i | i <- [0 .. n - 1]]
       , sCurve = [ let c = cMax * fromIntegral k / 160 in (c, optValue caps sel c) | k <- [0 .. 160 :: Int] ]
       }

-- 資源量・選択が変わったら計算し直し、最適解の跡をのばす
refresh :: World -> World
refresh w =
  let s = solveAll (wCaps w) (wSel w)
      trail = case sBest s of
        Just (x, y, _) | moved x y (wTrail w) -> take 200 ((x, y) : wTrail w)
        _ -> wTrail w
      moved x y tr = case tr of
        (px, py) : _ -> sqrt ((x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)) > 0.01
        [] -> True
  in w { wS = s, wTrail = trail }

-- グラフの位置と大きさ
data Layout = Layout { pl, pt, pw, ph :: !Float }

layout :: Env -> Layout
layout env =
  let h = min (envH env - 330) (envW env * 0.42 * fl (yMax / xMax))
  in Layout 70 110 (h * fl (xMax / yMax)) h

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures [left, table, graph, hud]
  where
    Layout{..} = layout env
    Solved{..} = wS
    wd = envW env
    sx x = pl + fl (x / xMax) * pw
    sy y = pt + ph - fl (y / yMax) * ph
    sp2 (x, y) = (sx x, sy y)

    -- 左：実行可能領域
    left = Pictures $
      [seg 1 (hsba 0 0 100 0.1) (sx x, pt) (sx x, pt + ph) | x <- [0, 10 .. xMax]]
      ++ [seg 1 (hsba 0 0 100 0.1) (pl, sy y) (pl + pw, sy y) | y <- [0, 10 .. yMax]]
      ++ [fillPoly (hsba 120 60 70 0.22) (map sp2 sPoly)]
      ++ [limit i r c | (i, r, c) <- zip3 [0 ..] resources wCaps]
      ++ [path 2 (hsba 50 80 100 0.5) (map sp2 wTrail)]
      ++ [case sBest of
            Just (x, y, _) -> Pictures
              [ dot (hsb 50 90 100) (sx x) (sy y) 16
              , txt (sx x + 12) (sy y - 10) 15 white ("Best (" ++ nf 1 x ++ ", " ++ nf 1 y ++ ")") ]
            Nothing -> Blank]
      ++ [txt pl (pt + ph + 22) 14 (gr 60) "x = chairs, y = tables. Yellow line = path of the best point"]
    limit i r c = case clipLine (rA r) (rB r) c of
      Nothing -> Blank
      Just (a, b) ->
        seg (if i == wSel then 3 else 1.5) (hsba (rHue r) 70 100 (if i == wSel then 1 else 0.55)) (sp2 a) (sp2 b)

    -- 下：感度分析の表
    ty = pt + ph + 60
    cols = [pl, pl + 130, pl + 270, pl + 380, pl + 500, pl + 650]
    table = Pictures $
      zipWith (\x h -> txt x ty 14 (gr 70) h) cols ["Resource", "Amount", "Used", "Slack", "Shadow price", "Range (same value)"]
      ++ concat (zipWith row [0 ..] (zip3 resources wCaps (zip sShadow sRange)))
    row :: Int -> (Res, Double, (Double, (Double, Double))) -> [Picture]
    row i (r, cap, (sp, (rg0, rg1))) =
      let used = case sBest of
            Just (x, y, _) -> rA r * x + rB r * y
            Nothing -> 0
          y = ty + 28 + fromIntegral i * 26
          at' k = cols !!! k
      in [ txt (at' 0) y 14 (hsba (rHue r) 60 100 (if i == wSel then 1 else 0.75)) ((if i == wSel then "> " else "  ") ++ rName r)
         , txt (at' 1) y 14 (gr 90) (nf 0 cap ++ " " ++ rUnit r)
         , txt (at' 2) y 14 (gr 90) (nf 1 used)
         , txt (at' 3) y 14 (gr 90) (nf 1 (cap - used))
         , txt (at' 4) y 14 (if sp > 1e-6 then hsb 50 90 100 else gr 55) (nf 2 sp ++ " k/" ++ rUnit r)
         , txt (at' 5) y 14 (gr 75) (nf 1 rg0 ++ " - " ++ (if rg1 >= rCap0 r * 3 then "inf" else nf 1 rg1))
         ]

    -- 右：選んだ資源の量 → 最適な利益 のグラフ（傾き = シャドウプライス）
    gx = pl + pw + 90
    gy = pt + 130
    gw = wd - gx - 50
    gh = ph - 150
    r = resources !!! wSel
    cap = wCaps !!! wSel
    cMax = rCap0 r * 3
    zMax = max 1 (maximum (0 : map snd sCurve)) * 1.1
    gxs c = gx + fl (c / cMax) * gw
    gys z = gy + gh - fl (z / zMax) * gh
    (rg0, rg1) = sRange !!! wSel
    sp = sShadow !!! wSel
    z0 = optValue wCaps wSel cap
    -- グラフからはみ出さない長さに
    fitDc :: Int -> Double -> Double
    fitDc k dc
      | k < 8 && (z0 + sp * dc > zMax || z0 - sp * dc < 0 || cap - dc < 0) = fitDc (k + 1) (dc / 2)
      | otherwise = dc
    dc = fitDc 0 (cMax * 0.25)
    graph = Pictures
      [ box (hsba 50 80 100 0.12) (gxs rg0) gy (gxs (min rg1 cMax) - gxs rg0) gh
      , seg 1 (hsba 0 0 100 0.4) (gx, gy + gh) (gx + gw, gy + gh)
      , seg 1 (hsba 0 0 100 0.4) (gx, gy) (gx, gy + gh)
      , path 3 (hsb (rHue r) 70 100) [(gxs c, gys z) | (c, z) <- sCurve]
      , dash 2 (hsb 50 90 100) (gxs (cap - dc), gys (z0 - sp * dc)) (gxs (cap + dc), gys (z0 + sp * dc))   -- 接線（傾き = シャドウプライス）
      , dot (hsb 50 90 100) (gxs cap) (gys z0) 14
      , txt gx pt 16 white (rName r ++ " amount -> best profit")
      , txt gx (pt + 26) 14 (gr 80) ("Now: " ++ rName r ++ " " ++ nf 0 cap ++ " " ++ rUnit r ++ " -> profit " ++ nf 0 z0 ++ "k")
      , txt gx (pt + 48) 14 (gr 80) ("Slope (shadow price) = " ++ nf 2 sp ++ "k / " ++ rUnit r)
      , txt gx (pt + 70) 14 (gr 60) "Band = range: outside it the active constraint changes"
      , txt gx (pt + 92) 14 (gr 60) "Flat graph = surplus resource (shadow price 0)"
      , txtC (gx + gw / 2) (gy + gh + 14) 14 (gr 60) (rName r ++ " (" ++ rUnit r ++ ") ->")
      ]

    hud = Pictures
      [ txt 20 40 16 white ("L/R wood " ++ capS 0 ++ "  U/D hours " ++ capS 1 ++ "  Z/X paint " ++ capS 2 ++ "  1-3: graph  R: reset    Best profit "
          ++ maybe "-" (\(_, _, z) -> nf 0 z) sBest ++ "k")
      , txt 20 68 14 (gr 60) "How much to pay for 1 more kg of wood? Up to its shadow price. More of a surplus resource adds nothing."
      ]
    capS i = nf 0 (wCaps !!! i)

-- 安全な添字（範囲外なら先頭）
(!!!) :: [a] -> Int -> a
xs !!! i = case drop i xs of
  v : _ -> v
  [] -> case xs of
    v : _ -> v
    [] -> error "empty list"

-- キー
onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> addCap 0 2 360 w
  EventKey (SpecialKey KeyLeft) Down _ _ -> addCap 0 (-2) 360 w
  EventKey (SpecialKey KeyUp) Down _ _ -> addCap 1 2 240 w
  EventKey (SpecialKey KeyDown) Down _ _ -> addCap 1 (-2) 240 w
  EventKey (Char c) Down _ _ -> case toLower c of
    'x' -> addCap 2 1 105 w
    'z' -> addCap 2 (-1) 105 w
    '1' -> refresh w { wSel = 0 }
    '2' -> refresh w { wSel = 1 }
    '3' -> refresh w { wSel = 2 }
    'r' -> refresh w { wCaps = map rCap0 resources, wTrail = [] }
    _ -> w
  _ -> w

addCap :: Int -> Double -> Double -> World -> World
addCap i d hi w =
  let c = max 0 (min hi (wCaps w !!! i + d))
  in refresh w { wCaps = setAt i c (wCaps w) }

-- ---------- やってみよう ----------
-- 1. 作業時間（↑↓）を増やし続けると、どこかでシャドウプライスが下がる。そこで何が起きている？（左の図で確認）
-- 2. 塗装ライン（Z・X）のシャドウプライスが 0 になるのはなぜ？「Slack」（余り）の列と見比べよう
-- 3. 3 つのシャドウプライス × 資源量 を足すと最適な利益と同じになる？（双対定理 → シリーズ 16）
-- 4. profitA を変えると、どの資源の値打ちが上がる？


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 塗りつぶした凸多角形
fillPoly :: Color -> [Point] -> Picture
fillPoly c ps = Color c (Polygon [(x, negate y) | (x, y) <- ps])

-- 点線（8 描いて 6 空ける）
dash :: Float -> Color -> Point -> Point -> Picture
dash t c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      pt d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      ds = takeWhile (< l) [0, 14 ..]
  in if l < 1 then Blank else Pictures [seg t c (pt d) (pt (min l (d + 8))) | d <- ds]

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- Double → Float（計算は Double、描くのは Float）
fl :: Double -> Float
fl = realToFrac

-- 制約 (a, b, c)（a·x + b·y ≤ c）の境界線どうしの交点のうち、全部の制約を満たすもの = 多角形の頂点
feasibleVertices :: [(Double, Double, Double)] -> [(Double, Double)]
feasibleVertices cons =
  let ic = zip [0 :: Int ..] cons
      cands = [ (x, y)
              | (i, (a1, b1, c1)) <- ic, (j, (a2, b2, c2)) <- ic, i < j
              , let det = a1 * b2 - a2 * b1
              , abs det >= 1e-9
              , let x = (c1 * b2 - c2 * b1) / det
              , let y = (a1 * c2 - a2 * c1) / det
              , all (\(a, b, c) -> a * x + b * y <= c + 1e-7) cons ]
      addNew ps p@(x, y) = if any (\(u, v) -> abs (u - x) + abs (v - y) < 1e-6) ps then ps else ps ++ [p]
      pts = foldl' addNew [] cands
      n = fromIntegral (length pts)
      cx = sum (map fst pts) / max 1 n
      cy = sum (map snd pts) / max 1 n
  in sortBy (comparing (\(x, y) -> atan2 (y - cy) (x - cx))) pts

-- 頂点の中で目的関数 p·x + q·y が一番大きいもの (x, y, z)
bestVertex :: Double -> Double -> [(Double, Double)] -> Maybe (Double, Double, Double)
bestVertex p q = foldl' pick Nothing
  where
    pick b (x, y) =
      let z = p * x + q * y
      in case b of
           Just (_, _, bz) | z <= bz + 1e-9 -> b
           _ -> Just (x, y, z)

-- 直線 a·x + b·y = c のうち、グラフの枠の中に入る部分
clipLine :: Double -> Double -> Double -> Maybe ((Double, Double), (Double, Double))
clipLine a b c =
  let fromX = [(x, y) | abs b > 1e-9, x <- [0, xMax], let y = (c - a * x) / b, y >= 0, y <= yMax]
      fromY = [(x, y) | abs a > 1e-9, y <- [0, yMax], let x = (c - b * y) / a, x >= 0, x <= xMax]
      pts = sortBy (comparing fst <> comparing (negate . snd)) (fromX ++ fromY)
  in case pts of
       (p : _ : _) -> Just (p, last pts)
       _ -> Nothing

