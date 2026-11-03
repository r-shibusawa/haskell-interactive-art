-- ============================================
-- 03 輸送問題 ─ 北西隅法と MODI 法（飛び石法）
-- 3 つの工場から 4 つの店へ商品を運ぶ。供給と需要を満たして、運賃の合計を最小にする
--   北西隅法：表の左上から順に詰めて「とりあえずの解」をつくる → MODI 法：まだ使っていないマスの「得になる度合い」を計算し、
--   損しないループ（飛び石）に沿って荷物を付け替える。得になるマスがなくなったら最適
-- スペース = 1 ステップ / A = 自動 / R = 新しい問題 / N = 北西隅からやり直し
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Array
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

mN, nN :: Int
mN = 3                                 -- ★ 工場の数
nN = 4                                 -- ★ 店の数

data Phase = NW | Modi | Cycle | Opt deriving Eq

type Grid a = Array (Int, Int) a

data World = World
  { wCost :: !(Grid Int)               -- 1 個あたりの運賃
  , wSupply, wDemand :: ![Int]
  , wAlloc :: !(Grid Int)              -- 輸送量
  , wBasic :: !(Grid Bool)             -- 「基底」（使っているマス）
  , wPhase :: !Phase
  , wNwI, wNwJ :: !Int
  , wRemS, wRemD :: ![Int]
  , wU, wV :: ![Int]
  , wRed :: !(Maybe (Grid Int))        -- 得になる度合い
  , wEnter :: !(Maybe (Int, Int))
  , wLoop :: ![(Int, Int)]
  , wTheta :: !Int
  , wAuto :: !Bool
  , wIter :: !Int
  , wCostLog :: !(Seq.Seq Int)
  }

main :: IO ()
main = runSketch (sketch "30-03 Transportation Problem" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newProblem
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 40 == 0 then stepAll w else w)
  , skDraw  = drawAll
  , skEvent = \_ ev w -> onEvent ev w
  }

cells :: [(Int, Int)]
cells = [(i, j) | i <- [0 .. mN - 1], j <- [0 .. nN - 1]]

gridBounds :: ((Int, Int), (Int, Int))
gridBounds = ((0, 0), (mN - 1, nN - 1))

newProblem :: IO World
newProblem = do
  costs <- replicateM (mN * nN) (randomRIO (2, 12))      -- ★ 1 個あたりの運賃
  demand <- replicateM nN ((* 5) <$> randomRIO (4, 12))
  let total = sum demand
      go i left
        | i >= mN - 1 = pure [left]
        | otherwise = do
            r <- randomRIO (-2, 3 :: Float)
            let s0 = floor (fromIntegral total / fromIntegral mN / 5 + r) * 5 :: Int
                s = max 5 (min (left - 5 * (mN - 1 - i)) s0)
            (s :) <$> go (i + 1) (left - s)
  supply <- go 0 total
  pure (restart (emptyWorld (listArray gridBounds costs) supply demand))

emptyWorld :: Grid Int -> [Int] -> [Int] -> World
emptyWorld cost supply demand = World
  { wCost = cost, wSupply = supply, wDemand = demand
  , wAlloc = fill 0, wBasic = fill False, wPhase = NW, wNwI = 0, wNwJ = 0
  , wRemS = supply, wRemD = demand, wU = [], wV = [], wRed = Nothing, wEnter = Nothing
  , wLoop = [], wTheta = 0, wAuto = False, wIter = 0, wCostLog = Seq.empty }

fill :: a -> Grid a
fill v = listArray gridBounds (replicate (mN * nN) v)

restart :: World -> World
restart w = (emptyWorld (wCost w) (wSupply w) (wDemand w)) { wAuto = wAuto w }

totalCost :: World -> Int
totalCost w = sum [wAlloc w ! c * wCost w ! c | c <- cells]

idx :: [a] -> Int -> a -> a
idx xs i d = case drop i xs of
  v : _ -> v
  [] -> d

setL :: Int -> a -> [a] -> [a]
setL i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- ★ 北西隅法の 1 マス：供給と需要の小さい方を詰める
stepNW :: World -> World
stepNW w@World{..} =
  let q = min (idx wRemS wNwI 0) (idx wRemD wNwJ 0)
      remS = setL wNwI (idx wRemS wNwI 0 - q) wRemS
      remD = setL wNwJ (idx wRemD wNwJ 0 - q) wRemD
      w1 = w { wAlloc = wAlloc // [((wNwI, wNwJ), q)], wBasic = wBasic // [((wNwI, wNwJ), True)], wRemS = remS, wRemD = remD }
  in if idx remS wNwI 0 == 0 && wNwI < mN - 1 then w1 { wNwI = wNwI + 1 }
     else if wNwJ < nN - 1 then w1 { wNwJ = wNwJ + 1 }
     else computeModi w1 { wPhase = Modi }

-- ★ MODI：使っているマスで cost = u_i + v_j となる u, v を決め、他のマスの「得になる度合い」= cost − u − v
computeModi :: World -> World
computeModi w@World{..} =
  let sweep (us, vs) (i, j)
        | not (wBasic ! (i, j)) = (us, vs)
        | otherwise =
            let vs' = case (idx us i Nothing, idx vs j Nothing) of
                  (Just u, Nothing) -> setL j (Just (wCost ! (i, j) - u)) vs
                  _ -> vs
                us' = case (idx vs' j Nothing, idx us i Nothing) of
                  (Just v, Nothing) -> setL i (Just (wCost ! (i, j) - v)) us
                  _ -> us
            in (us', vs')
      start = (Just 0 : replicate (mN - 1) Nothing, replicate nN Nothing)
      (usM, vsM) = foldl (\st _ -> foldl sweep st cells) start [1 .. mN + nN]
      us = map (fromMaybe 0) usM
      vs = map (fromMaybe 0) vsM
      redOf (i, j) = if wBasic ! (i, j) then 0 else wCost ! (i, j) - idx us i 0 - idx vs j 0
      red = listArray gridBounds (map redOf cells) :: Grid Int
      pick (best, e) c
        | not (wBasic ! c) && red ! c < best = (red ! c, Just c)
        | otherwise = (best, e)
      (_, enter) = foldl pick (0, Nothing) cells
      w1 = w { wU = us, wV = vs, wRed = Just red, wEnter = enter, wCostLog = wCostLog Seq.|> totalCost w }
  in case enter of
       Nothing -> w1 { wPhase = Opt, wAuto = False }
       Just _ -> w1

-- 入るマスから、使っているマスだけを縦横に飛んで戻るループ（飛び石）を探す
findLoop :: Grid Bool -> (Int, Int) -> [(Int, Int)]
findLoop basic (ei, ej) =
  let total = mN + nN
      -- 行 i と列 j を点、使っているマスを辺とみなした木の上で、行 ei → 列 ej の道を幅優先で探す
      next a = [ b | b <- [0 .. total - 1]
               , if a < mN then b >= mN && basic ! (a, b - mN) else b < mN && basic ! (b, a - mN) ]
      bfs [] seen = seen
      bfs (a : q) seen =
        let new = [b | b <- next a, lookup b seen == Nothing]
        in bfs (q ++ new) (seen ++ [(b, a) | b <- new])
      prevs = bfs [ei] [(ei, -1)]
      walk b
        | b == ei || b < 0 = []
        | otherwise = case lookup b prevs of
            Just a | a >= 0 -> (if a < mN then (a, b - mN) else (b, a - mN)) : walk a
            _ -> []
  in (ei, ej) : reverse (walk (mN + ej))      -- 0 番目が入るマス（+）、あとは − + − … と交互

stepModi :: World -> World
stepModi w@World{..} = case (wPhase, wEnter) of
  (Modi, Just e) ->
    let lp = findLoop wBasic e
        minus = [c | (k, c) <- zip [0 :: Int ..] lp, odd k]
    in w { wLoop = lp, wTheta = minimum (maxBound : map (wAlloc !) minus), wPhase = Cycle }
  (Cycle, Just e) ->
    let changes = [ (c, wAlloc ! c + (if even k then wTheta else negate wTheta)) | (k, c) <- zip [0 :: Int ..] wLoop ]
        alloc = wAlloc // changes
        leaving = [c | (k, c) <- zip [0 :: Int ..] wLoop, odd k, alloc ! c == 0]
        basic = wBasic // ((e, True) : take 1 [(c, False) | c <- leaving])
    in computeModi w { wAlloc = alloc, wBasic = basic, wLoop = [], wIter = wIter + 1, wPhase = Modi }
  _ -> w

stepAll :: World -> World
stepAll w = case wPhase w of
  NW -> stepNW w
  Opt -> w
  _ -> stepModi w

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [table, loopPic, explain, mapPic, costGraph, hud]
  where
    wd = envW env
    ht = envH env
    cw = min 120 ((wd * 0.55 - 160) / fromIntegral nN)
    chh = min 80 ((ht - 330) / fromIntegral mN)
    tx = 140
    ty = 160
    fI = fromIntegral :: Int -> Float
    cx j = tx + fI j * cw
    cy i = ty + fI i * chh
    name i = take 1 (drop i "ABCDE")
    showDuals = wPhase /= NW && not (null wU)

    -- 表
    table = Pictures $
      concat [ [ txtC (cx j + cw / 2) (ty - 40) 16 (gr 90) ("Shop " ++ show (j + 1))
               , txtC (cx j + cw / 2) (ty - 18) 14 (gr 60) ("Need " ++ show (idx wDemand j 0)) ]
               ++ [ txtC (cx j + cw / 2) (ty + fI mN * chh + 16) 14 (hsb 190 70 100) ("v=" ++ show (idx wV j 0)) | showDuals ]
             | j <- [0 .. nN - 1] ]
      ++ concat [ [ txtC (tx - 70) (cy i + chh / 2 - 10) 16 (gr 90) ("Plant " ++ name i)
                  , txtC (tx - 70) (cy i + chh / 2 + 12) 14 (gr 60) ("Have " ++ show (idx wSupply i 0)) ]
                  ++ [ txtC (tx + fI nN * cw + 34) (cy i + chh / 2) 14 (hsb 190 70 100) ("u=" ++ show (idx wU i 0)) | showDuals ]
                | i <- [0 .. mN - 1] ]
      ++ map cell cells
    cell (i, j) =
      let x = cx j
          y = cy i
          basic = wBasic ! (i, j)
          isEnter = wEnter == Just (i, j) && wPhase /= Opt
          bg | wPhase == NW && i == wNwI && j == wNwJ = hsb 50 60 45
             | basic = hsb 210 40 30
             | otherwise = hsb 225 25 17
      in Pictures
        [ box bg x y cw chh
        , boxL 1 (hsba 0 0 100 0.3) x y cw chh
        , txt (x + 6) (y + 18) 14 (gr 65) ("$" ++ show (wCost ! (i, j)))
        , if basic then txtC (x + cw / 2) (y + chh / 2 + 4) 22 white (show (wAlloc ! (i, j)))
          else case wRed of
            Just red | wPhase /= NW ->
              let r = red ! (i, j)
              in txtC (x + cw / 2) (y + chh / 2 + 4) 15 (if r < 0 then hsb 0 75 100 else hsb 140 50 80) ((if r > 0 then "+" else "") ++ show r)
            _ -> Blank
        , if isEnter then boxL 3 (hsb 0 80 100) (x + 3) (y + 3) (cw - 6) (chh - 6) else Blank
        ]

    -- ループ（飛び石）
    loopPic
      | null wLoop = Blank
      | otherwise = Pictures $
          loopPath 3 (hsb 50 90 100) [(cx j + cw / 2, cy i + chh / 2) | (i, j) <- wLoop]
          : concat [ [ dot (if even k then hsb 140 80 100 else hsb 0 80 100) (cx j + cw - 16) (cy i + chh - 16) 22
                     , txtC (cx j + cw - 16) (cy i + chh - 16) 16 (gr 10) (if even k then "+" else "-") ]
                   | (k, (i, j)) <- zip [0 :: Int ..] wLoop ]

    -- 説明
    ey = ty + fI mN * chh + 60
    msgs = case wPhase of
      NW -> [ "Northwest corner: fill the yellow cell with the smaller of"
            , "remaining supply and need, then move right or down." ]
      Modi -> [ "Empty cells: cost change per unit if we start using them"
              , "(red = saves money). The red frame cell enters next." ]
      Cycle -> [ "Along the loop, alternate + and - by theta = " ++ show wTheta ++ " units"
               , "(plant and shop totals stay the same)." ]
      Opt -> [ "No cell lowers the cost -> optimal. Improvements: " ++ show wIter ]
    cycleMsg = case (wPhase, wEnter, wRed) of
      (Cycle, Just e, Just red) -> [show (red ! e) ++ " per unit x theta " ++ show wTheta ++ " -> change " ++ show (red ! e * wTheta)]
      _ -> []
    explain = Pictures $
      txt (tx - 120) ey 20 white ("Total cost $" ++ show (totalCost w) ++ (if wPhase == Opt then "  OPTIMAL" else ""))
      : [ txt (tx - 120) (ey + 30 + k * 24) 15 (gr 70) s | (k, s) <- zip [0 ..] (msgs ++ cycleMsg) ]

    -- 右：地図と運賃の推移
    mx = tx + fI nN * cw + 110
    mw = wd - mx - 40
    mh = (ht - 240) * 0.55
    my = 140
    rowY i = my + (fI i + 0.5) * mh / fI mN
    colY j = my + (fI j + 0.5) * mh / fI nN
    mapPic = Pictures $
      [ seg (1 + fI (wAlloc ! (i, j)) / 6) (hsba (200 + fI (wCost ! (i, j)) * 10) 60 100 0.7) (mx + 30, rowY i) (mx + mw - 30, colY j)
      | (i, j) <- cells, wAlloc ! (i, j) > 0 ]
      ++ concat [ [box (hsb 30 70 100) (mx + 10) (rowY i - 18) 40 36, txtC (mx + 30) (rowY i) 14 (gr 10) (name i)] | i <- [0 .. mN - 1] ]
      ++ concat [ [dot (hsb 190 60 100) (mx + mw - 30) (colY j) 38, txtC (mx + mw - 30) (colY j) 14 (gr 10) (show (j + 1))] | j <- [0 .. nN - 1] ]

    gy = my + mh + 50
    gh = ht - gy - 40
    logL = toList wCostLog
    costGraph
      | null logL = title
      | otherwise =
          let hi = fI (maximum logL)
              lo = fI (minimum logL) * 0.9
              n = max 1 (length logL - 1)
              pts = [ (mx + fI k * mw / fI n, gy + gh - (fI c - lo) / max 1 (hi - lo) * gh) | (k, c) <- zip [0 ..] logL ]
          in Pictures (title : path 2 (hsb 50 90 100) pts : [dot (gr 80) x y 6 | (x, y) <- pts])
    title = txt mx (gy - 10) 14 (gr 70) "Total cost after each MODI improvement"

    hud = Pictures
      [ txt 20 40 16 white ("Space: 1 step   A: auto " ++ (if wAuto then "ON" else "off") ++ "   R: new problem   N: restart from the northwest corner")
      , txt 20 68 14 (gr 60) "Cell: top left = cost per unit, center = units shipped. Map: line width = amount, color = cost (blue low, red high)."
      ]

-- キー
onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (stepAll w)
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> do
      n <- newProblem
      pure n { wAuto = wAuto w }
    'n' -> pure (restart w)
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 北西隅法の解と最適解で運賃がどれだけ違う？ 北西隅法は運賃を見ていないことに注目
-- 2. 「運賃が一番安いマスから詰める」（最小費用法）に stepNW を書きかえると、改善の回数は減る？
-- 3. 供給の合計が需要より多いときは「ダミーの店（運賃 0）」を足すとこの方法が使える。nN = 5 にして試そう
-- 4. ループの + と − で、なぜ供給・需要の合計が変わらないのか説明してみよう


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

-- 枠だけの長方形（線の太さ t）
boxL :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
boxL t c x y w h = loopPath t c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

