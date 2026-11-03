-- ============================================
-- 02 整数計画と分枝限定法
-- 「椅子 3.75 脚」は作れない。変数を整数に限ると、LP の最適解を四捨五入しても最適とは限らない
--   分枝限定法：LP を解く → 小数の変数で「x ≤ 3」「x ≥ 4」に分ける（分枝）→ 今の最良の整数解より良くなれない枝は切る（限定）
-- スペース = 1 ステップ / A = 自動 / B = 深さ優先 ⇄ 最良優先 / 1〜3 = 問題 / R = やり直し
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Ord (comparing)
import Numeric (showFFloat)
import Data.List (foldl', sortBy)
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe)

-- | 最大化 c·(x, y)、制約 a·x + b·y ≤ cap
data Problem = Problem { pC :: !(Double, Double), pCons :: [(Double, Double, Double)] }

problems :: [Problem]
problems =                             -- ★ 問題
  [ Problem (5, 4) [(6, 4, 24), (1, 2, 6)]
  , Problem (8, 5) [(1, 1, 6), (9, 5, 45)]
  , Problem (2, 3) [(1, 3, 10.5), (3, 1, 11)]
  ]

xMax, yMax :: Int
xMax = 8
yMax = 7

data Status = Wait | NoSol | Infeasible | Pruned | IntegerSol | Branched deriving Eq

-- | 節（部分問題）
data Node = Node
  { nLb, nUb :: !(Double, Double)      -- x, y の下限・上限
  , nCond :: String
  , nChildren :: [Int]
  , nDepth :: !Int
  , nStatus :: !Status
  , nSol :: !(Maybe (Double, Double, Double))   -- LP の解 (x, y, z)
  }

data World = World
  { wProb :: !Int
  , wNodes :: !(IM.IntMap Node)        -- 節（番号 → 節）。0 番が根
  , wActive :: ![Int]                  -- まだ調べていない節
  , wCurrent, wIncumbent :: !(Maybe Int)
  , wBestFirst, wAuto, wDone :: !Bool
  , wExplored :: !Int
  }

inf :: Double
inf = 1 / 0

world0 :: World
world0 = reset 0 False False

reset :: Int -> Bool -> Bool -> World
reset prob best auto =
  let root = makeNode prob (0, 0) (inf, inf) 0 "root"
  in World prob (IM.singleton 0 root) [0] Nothing Nothing best auto False 0

main :: IO ()
main = runSketch (sketch "30-02 Branch and Bound" world0)
  { skBack  = hsb 225 30 12
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 30 == 0 then stepBB w else w)
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

problemAt :: Int -> Problem
problemAt i = case drop i problems of
  p : _ -> p
  [] -> Problem (1, 1) []

-- 節をつくって、すぐ LP を解いておく
makeNode :: Int -> (Double, Double) -> (Double, Double) -> Int -> String -> Node
makeNode prob lb ub depth cond =
  let s = solveLP prob lb ub
  in Node lb ub cond [] depth (maybe NoSol (const Wait) s) s

consOf :: Int -> (Double, Double) -> (Double, Double) -> [(Double, Double, Double)]
consOf prob (l0, l1) (u0, u1) =
  pCons (problemAt prob) ++ [(-1, 0, -l0), (0, -1, -l1)]
    ++ [(1, 0, u0) | u0 < inf] ++ [(0, 1, u1) | u1 < inf]

solveLP :: Int -> (Double, Double) -> (Double, Double) -> Maybe (Double, Double, Double)
solveLP prob lb ub =
  let (c0, c1) = pC (problemAt prob)
  in bestVertex c0 c1 (feasibleVertices (consOf prob lb ub))

isInt :: Double -> Bool
isInt v = abs (v - fromIntegral (round v :: Int)) < 1e-6

nodeZ :: World -> Int -> Double
nodeZ w k = case IM.lookup k (wNodes w) >>= nSol of
  Just (_, _, z) -> z
  Nothing -> -1

-- ★ 分枝限定法の 1 ステップ
stepBB :: World -> World
stepBB w = case wActive w of
  [] -> w { wDone = True, wAuto = False, wCurrent = Nothing }
  active ->
    let lastK = last active
        -- 深さ優先：最後に入れた節 ／ 最良優先：上界が一番大きい節
        pick = if wBestFirst w
                 then foldl (\b i -> if nodeZ w i > nodeZ w b then i else b) lastK active
                 else lastK
        rest = filter (/= pick) active
        w1 = w { wActive = rest, wCurrent = Just pick, wExplored = wExplored w + 1 }
    in maybe w1 (process w1 pick) (IM.lookup pick (wNodes w))

process :: World -> Int -> Node -> World
process w k n = case nSol n of
  Nothing -> mark Infeasible w                                 -- 解なし
  Just (x, y, z)
    | Just iz <- incumbentZ, z <= iz + 1e-9 -> mark Pruned w   -- 限定：これ以上良くならない
    | isInt x && isInt y ->                                    -- 整数解が見つかった
        let better = maybe True (z >) incumbentZ
        in mark IntegerSol (if better then w { wIncumbent = Just k } else w)
    | otherwise -> branch x y                                  -- 分枝
  where
    mark s wa = wa { wNodes = IM.adjust (\m -> m { nStatus = s }) k (wNodes wa) }
    incumbentZ = do
      i <- wIncumbent w
      m <- IM.lookup i (wNodes w)
      (_, _, z) <- nSol m
      pure z
    branch x y =
      let useX = not (isInt x)
          val = if useX then x else y
          name = if useX then "x" else "y"
          fl' = fromIntegral (floor val :: Int) :: Double
          ce' = fromIntegral (ceiling val :: Int) :: Double
          (u0, u1) = nUb n
          (l0, l1) = nLb n
          ubA = if useX then (fl', u1) else (u0, fl')
          lbB = if useX then (ce', l1) else (l0, ce')
          ia = IM.size (wNodes w)
          ib = ia + 1
          d = nDepth n + 1
          a = makeNode (wProb w) (nLb n) ubA d (name ++ " <= " ++ nf 0 fl')
          b = makeNode (wProb w) lbB (nUb n) d (name ++ " >= " ++ nf 0 ce')
          nodes = IM.insert ib b (IM.insert ia a (IM.adjust (\m -> m { nStatus = Branched, nChildren = [ia, ib] }) k (wNodes w)))
      in w { wNodes = nodes, wActive = wActive w ++ [ia, ib] }

statusColor :: Status -> Color
statusColor s = case s of
  Wait -> gr 60
  NoSol -> gr 60
  Branched -> hsb 210 50 80
  IntegerSol -> hsb 140 75 90
  Pruned -> hsb 30 70 90
  Infeasible -> hsb 0 60 70

-- 葉の数
countLeaves :: IM.IntMap Node -> Int -> Int
countLeaves nodes k = case IM.lookup k nodes of
  Just n | not (null (nChildren n)) -> sum (map (countLeaves nodes) (nChildren n))
  _ -> 1

-- 木の配置：葉を左から順に並べ、親は子の真ん中に置く（戻り値は次の葉の番号）
placeNode :: IM.IntMap Node -> Float -> Float -> Float -> Float -> Int -> Int -> (IM.IntMap Point, Int) -> (IM.IntMap Point, Int)
placeNode nodes x0 sw y0 gap _ k (pos, leaf) = case IM.lookup k nodes of
  Nothing -> (pos, leaf)
  Just n ->
    let y = y0 + fromIntegral (nDepth n) * gap
    in case nChildren n of
         [] -> (IM.insert k (x0 + (fromIntegral leaf + 0.5) * sw, y) pos, leaf + 1)
         cs ->
           let (pos', leaf') = foldl (\acc c -> placeNode nodes x0 sw y0 gap 0 c acc) (pos, leaf) cs
               xs = mapMaybe (\c -> fst <$> IM.lookup c pos') cs
               mx = sum xs / fromIntegral (max 1 (length xs))
           in (IM.insert k (mx, y) pos', leaf')

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [left, tree, legend, hud]
  where
    wd = envW env
    ht = envH env
    pl = 70
    pt = 120
    ph = min (ht - 220) (wd * 0.36 * fromIntegral yMax / fromIntegral xMax)
    pw = ph * fromIntegral xMax / fromIntegral yMax
    sx x = pl + fl x / fromIntegral xMax * pw
    sy y = pt + ph - fl y / fromIntegral yMax * ph
    prob = problemAt wProb
    (c0, c1) = pC prob
    nodeOf k = IM.lookup k wNodes
    root = nodeOf 0 >>= nSol
    cur = wCurrent >>= nodeOf
    incum = wIncumbent >>= nodeOf >>= nSol

    -- 左：整数の格子点と実行可能領域
    xsI = map fromIntegral [0 .. xMax] :: [Double]
    ysI = map fromIntegral [0 .. yMax] :: [Double]
    left = Pictures $
      [seg 1 (hsba 0 0 100 0.1) (sx x, pt) (sx x, pt + ph) | x <- xsI]
      ++ [seg 1 (hsba 0 0 100 0.1) (pl, sy y) (pl + pw, sy y) | y <- ysI]
      ++ [txtC (sx x) (pt + ph + 16) 14 (gr 60) (nf 0 x) | x <- xsI]
      ++ [txtR (pl - 8) (sy y + 5) 14 (gr 60) (nf 0 y) | y <- drop 1 ysI]
      ++ [fillPoly (hsba 120 50 70 0.15) [(sx x, sy y) | (x, y) <- feasibleVertices (pCons prob ++ [(-1, 0, 0), (0, -1, 0)])]]
      ++ [ case cur of
             Just n | nStatus n /= Infeasible ->
               fillPoly (hsba 190 70 90 0.35) [(sx x, sy y) | (x, y) <- feasibleVertices (consOf wProb (nLb n) (nUb n))]
             _ -> Blank ]
      ++ [ let ok = all (\(a, b, c) -> a * x + b * y <= c + 1e-9) (pCons prob)
           in dot (hsba 0 0 100 (if ok then 0.85 else 0.15)) (sx x) (sy y) (if ok then 8 else 4)
         | x <- xsI, y <- ysI ]
      ++ [ case root of
             Just (x, y, z) -> Pictures
               [ dot (hsb 50 90 100) (sx x) (sy y) 12
               , txt (sx x + 10) (sy y - 8) 14 (hsb 50 60 100) ("LP best " ++ nf 2 z) ]
             Nothing -> Blank ]
      ++ [ case cur >>= nSol of
             Just (x, y, _) -> ringAt 3 (hsb 190 80 100) (sx x) (sy y) 22
             Nothing -> Blank ]
      ++ [ case incum of
             Just (x, y, _) -> star (hsb 140 80 100) (sx x) (sy y) 13
             Nothing -> Blank ]
      ++ [ txt pl (pt + ph + 46) 15 (gr 85) ("Maximize " ++ nf 0 c0 ++ "x + " ++ nf 0 c1 ++ "y") ]
      ++ zipWith (\i (a, b, c) -> txt (pl + i * 170) (pt + ph + 70) 15 (gr 85) (nf 0 a ++ "x + " ++ nf 0 b ++ "y <= " ++ num c)) [0 ..] (pCons prob)
      ++ [ txt pl (pt + ph + 94) 15 (gr 85) "x, y are integers >= 0" ]
    num c = if isInt c then nf 0 c else nf 1 c

    -- 右：分枝限定の木
    tx = pl + pw + 60
    tw = wd - tx - 30
    ty = pt + 10
    leaves = countLeaves wNodes 0
    maxD = maximum (1 : map nDepth (IM.elems wNodes))
    gap = min 78 ((ht - ty - 110) / fromIntegral maxD)
    sw = tw / fromIntegral (max 1 leaves)
    (pos, _) = placeNode wNodes tx sw ty gap 0 0 (IM.empty, 0)
    tree = Pictures $
      [ Pictures
          [ seg 2 (hsba 0 0 100 0.35) a b
          , txtC (lerpF ax bx 0.62 + (if bx < ax then -34 else 34)) (lerpF ay by 0.62) 14 (gr 80) (nCond cn) ]
      | (k, n) <- IM.toList wNodes, c <- nChildren n
      , Just a@(ax, ay) <- [IM.lookup k pos], Just b@(bx, by) <- [IM.lookup c pos], Just cn <- [IM.lookup c wNodes] ]
      ++ concatMap nodePic (IM.toList wNodes)
    nodePic (k, n) = case IM.lookup k pos of
      Nothing -> []
      Just (x, y) ->
        let col = statusColor (nStatus n)
            waiting = nStatus n == Wait
            isCur = wCurrent == Just k
        in [ dot (if waiting then hsb 225 30 18 else col) x y 44
           , ringAt (if isCur then 4 else 2) (if isCur then hsb 190 80 100 else col) x y 44
           , txtC x y 14 (if waiting then gr 90 else gr 8) (maybe "none" (\(_, _, z) -> nf 1 z) (nSol n))
           , case nSol n of
               Just (sxv, syv, _)
                 | gap < 62 -> Blank        -- 段がせまいときは座標を省く
                 | sw >= 125 -> txtC x (y + 32) 14 (gr 70) ("(" ++ nf 2 sxv ++ ", " ++ nf 2 syv ++ ")")
                 | sw >= 72 -> txtC x (y + 32) 14 (gr 70) (nf 1 sxv ++ "," ++ nf 1 syv)
               _ -> Blank
           ]

    -- 凡例
    ly = ht - 40
    legend = Pictures
      [ Pictures
          [ dot (if s == Wait then hsb 225 30 30 else statusColor s) (tx + i * 125 + 8) (ly - 5) 14
          , txt (tx + i * 125 + 20) ly 14 (gr 80) t ]
      | (i, (s, t)) <- zip [0 ..] [(Wait, "Waiting"), (Branched, "Branched"), (IntegerSol, "Integer"), (Pruned, "Pruned"), (Infeasible, "No sol")] ]

    ub = maximum (maybe (-inf) (\(_, _, z) -> z) incum : [z | k <- wActive, Just n <- [nodeOf k], Just (_, _, z) <- [nSol n]])
    hud = Pictures
      [ txt 20 40 16 white ("Space: 1 step  A: auto " ++ (if wAuto then "ON" else "off") ++ "  B: " ++ (if wBestFirst then "best-first" else "depth-first")
          ++ "  1-3: problem " ++ show (wProb + 1) ++ "  R: restart")
      , txt 20 68 14 (gr 60) ("Nodes " ++ show wExplored ++ "  Incumbent (*) "
          ++ maybe "none" (\(x, y, z) -> nf 1 z ++ " at (" ++ nf 0 x ++ ", " ++ nf 0 y ++ ")") incum
          ++ "  Bound " ++ (if ub > -inf then nf 2 ub else "-")
          ++ (if wDone then "  OPTIMAL PROVEN" else "") ++ "   Number in a node = its LP value (upper bound)")
      ]

-- キー
onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> stepBB w
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> w { wAuto = not (wAuto w) }
    'b' -> reset (wProb w) (not (wBestFirst w)) (wAuto w)
    '1' -> reset 0 (wBestFirst w) (wAuto w)
    '2' -> reset 1 (wBestFirst w) (wAuto w)
    '3' -> reset 2 (wBestFirst w) (wAuto w)
    'r' -> reset (wProb w) (wBestFirst w) (wAuto w)
    _ -> w
  _ -> w

-- 星（へこんだ形なので三角形を中心のまわりに並べて描く）
star :: Color -> Float -> Float -> Float -> Picture
star c x y r =
  let pts = [ let a = -pi / 2 + fromIntegral i * pi / 5
                  rr = if odd i then r * 0.45 else r
              in (x + cos a * rr, y + sin a * rr)
            | i <- [0 .. 9 :: Int] ]
  in Pictures (zipWith (\p q -> fillPoly c [(x, y), p, q]) pts (drop 1 pts ++ take 1 pts))

-- ---------- やってみよう ----------
-- 1. 問題 1 の LP 最適解 (3, 1.5) を四捨五入すると (3, 2)。それは実行可能？ 整数の最適解はどこだった？
-- 2. 深さ優先と最良優先（B）で「Nodes」（調べた節）の数を比べよう。どちらが早く暫定解を見つける？
-- 3. 分枝する変数を「x が先」から「y が先」に変えると木の形はどう変わる？（process の useX）
-- 4. problems に自分の問題を足そう。変数が整数でないといけない現実の例は？（人数、トラックの台数…）


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

-- 文字：右端を x にそろえる
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size = \c s -> label (x - fromIntegral (length s) * size * 0.72) (negate y) size c s

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 塗りつぶした凸多角形
fillPoly :: Color -> [Point] -> Picture
fillPoly c ps = Color c (Polygon [(x, negate y) | (x, y) <- ps])

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 輪：中心 (x, y)、直径 d、太さ t
ringAt :: Float -> Color -> Float -> Float -> Float -> Picture
ringAt t c x y d = Color c (at (x, negate y) (ring (d / 2) t))

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

