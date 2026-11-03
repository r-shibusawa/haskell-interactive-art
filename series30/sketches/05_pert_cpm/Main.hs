-- ============================================
-- 05 プロジェクト管理 PERT/CPM
-- 家を建てる作業には「これが終わらないと始められない」順序がある。全体は最短で何日？ どの作業が遅れると全体が遅れる？
--   前から：最早開始 ES・最早終了 EF ／ 後ろから：最遅開始 LS・最遅終了 LF ／ 余裕 = LS − ES。余裕 0 の作業の列がクリティカルパス
-- クリック = 作業を選ぶ / ↑↓ = 選んだ作業の日数 / ←→ = 納期 / M = 日数がばらつくときのシミュレーション（PERT）/ R = 元に戻す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (foldM)
import Data.Array
import Data.Char (toLower)
import Data.List (foldl')

-- | 作業：記号、名前、最初の日数、前の作業（番号）
data Act = Act { aId :: Char, aName :: String, aD0 :: !Double, aPre :: [Int] }

acts :: [Act]
acts =                                 -- ★ 作業：前の作業が先に並ぶようにする
  [ Act 'A' "Foundation" 4 []
  , Act 'B' "Frame" 6 [0]
  , Act 'C' "Roof" 4 [1]
  , Act 'D' "Wiring" 3 [1]
  , Act 'E' "Plumbing" 5 [1]
  , Act 'F' "Interior" 6 [3, 4]
  , Act 'G' "Exterior" 5 [2]
  , Act 'H' "Garden" 3 [0]
  , Act 'I' "Inspection" 2 [5, 6, 7]
  ]

nActs :: Int
nActs = length acts

actAt :: Array Int Act
actAt = listArray (0, nActs - 1) acts

-- 次の作業（後ろから計算するときに使う）
succsOf :: Array Int [Int]
succsOf = accumArray (flip (:)) [] (0, nActs - 1) [(p, i) | (i, a) <- zip [0 ..] acts, p <- aPre a]

data World = World
  { wDur :: ![Double]                  -- 各作業の日数
  , wSel :: !Int
  , wDeadline :: !Int                  -- ★ 納期（日）
  , wSpans :: ![Double]                -- シミュレーションの工期（3000 回あまりで止まる）
  , wCrit :: ![Int]                    -- クリティカルになった回数
  , wRuns :: !Int
  }

world0 :: World
world0 = World (map aD0 acts) 4 30 [] (replicate nActs 0) 0

main :: IO ()
main = runSketch (sketch "30-05 PERT / CPM" world0)
  { skBack  = hsb 225 30 12
  , skStep  = \_ _ w -> if wRuns w > 0 && wRuns w < 3000 then foldM (\a _ -> simulateOnce a) w [1 .. 40 :: Int] else pure w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

data Cpm = Cpm { cES, cEF, cLS, cLF :: !(Array Int Double), cT :: !Double }

-- ★ CPM：前から ES/EF、後ろから LS/LF を計算（acts は前の作業が先に並んでいる）
cpm :: [Double] -> Cpm
cpm dur =
  let d = listArray (0, nActs - 1) dur :: Array Int Double
      es = listArray (0, nActs - 1) [ maximum (0 : map (ef !) (aPre a)) | a <- acts ] :: Array Int Double
      ef = listArray (0, nActs - 1) [ es ! i + d ! i | i <- [0 .. nActs - 1] ] :: Array Int Double
      t = maximum (elems ef)
      lf = listArray (0, nActs - 1) [ minimum (t : map (ls !) (succsOf ! i)) | i <- [0 .. nActs - 1] ] :: Array Int Double
      ls = listArray (0, nActs - 1) [ lf ! i - d ! i | i <- [0 .. nActs - 1] ] :: Array Int Double
  in Cpm es ef ls lf t

-- 作業の段（前の作業の段 + 1）で横の位置を決める
levels :: Array Int Int
levels = lv
  where lv = listArray (0, nActs - 1) [ maximum (0 : map (\p -> lv ! p + 1) (aPre a)) | a <- acts ]

-- ★ PERT のシミュレーション：各作業の日数を「楽観 0.7 倍 〜 最可能 〜 悲観 1.8 倍」の三角分布でばらつかせて 1 回分の工期
simulateOnce :: World -> IO World
simulateOnce w = do
  dur <- mapM (\a -> triangular (a * 0.7) a (a * 1.8)) (wDur w)
  let r = cpm dur
      crit = [ if cLS r ! i - cES r ! i < 1e-6 then c + 1 else c | (i, c) <- zip [0 ..] (wCrit w) ]
  pure $! sumCrit crit `seq` w { wSpans = cT r : wSpans w, wRuns = wRuns w + 1, wCrit = crit }

sumCrit :: [Int] -> Int
sumCrit = foldl' (+) 0

resetSim :: World -> World
resetSim w = w { wSpans = [], wCrit = replicate nActs 0, wRuns = 0 }

-- 箱の位置（中心）
nodePos :: Env -> Int -> Point
nodePos env i =
  let nx = 80
      nw = envW env - 160
      ny = 110
      nh = envH env * 0.42
      maxLv = maximum (elems levels)
      col = [k | k <- [0 .. nActs - 1], levels ! k == levels ! i]
      k = length (takeWhile (/= i) col)
  in ( nx + fromIntegral (levels ! i) / fromIntegral (max 1 maxLv) * nw
     , ny + fromIntegral (k + 1) / fromIntegral (length col + 1) * nh )

boxHW, boxHH :: Float
boxHW = 66                             -- 箱の幅の半分
boxHH = 40                             -- 箱の高さの半分

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures [arrows, boxes, gantt, simPic, hud]
  where
    wd = envW env
    ht = envH env
    r@Cpm{..} = cpm wDur
    is = [0 .. nActs - 1]
    posOf = listArray (0, nActs - 1) (map (nodePos env) is) :: Array Int Point
    durA = listArray (0, nActs - 1) wDur :: Array Int Double
    crit = listArray (0, nActs - 1) [cLS ! i - cES ! i < 1e-6 | i <- is] :: Array Int Bool
    n0 v = nf 0 v
    sel = actAt ! wSel

    -- 矢印（前の作業 → 次の作業）
    arrows = Pictures
      [ let c = crit ! i && crit ! j && abs (cEF ! j - cES ! i) < 1e-6
            (ax, ay) = posOf ! j
            (bx, by) = posOf ! i
        in arrowP (if c then 4 else 2) (if c then hsb 0 80 100 else hsba 0 0 100 0.3) (ax + boxHW, ay) (bx - boxHW, by)
      | i <- is, j <- aPre (actAt ! i) ]

    -- 作業の箱：ES EF / LS LF
    boxes = Pictures (map actBox is)
    actBox i =
      let (x, y) = posOf ! i
          a = actAt ! i
          isSel = i == wSel
      in Pictures
        [ box (if crit ! i then hsb 0 50 35 else hsb 220 30 25) (x - boxHW) (y - boxHH) (boxHW * 2) (boxHH * 2)
        , boxL (if isSel then 4 else 2) (if isSel then hsb 50 90 100 else if crit ! i then hsb 0 80 100 else gr 50) (x - boxHW) (y - boxHH) (boxHW * 2) (boxHH * 2)
        , txtC x (y - 27) 14 white (aId a : ' ' : aName a)
        , txtC x (y - 9) 14 (hsb 50 70 100) (n0 (durA ! i) ++ " days")
        , txtC x (y + 9) 14 (gr 70) ("ES/EF " ++ n0 (cES ! i) ++ "/" ++ n0 (cEF ! i))
        , txtC x (y + 27) 14 (gr 70) ("LS/LF " ++ n0 (cLS ! i) ++ "/" ++ n0 (cLF ! i))
        ]

    -- ガントチャート（余裕 = 薄い部分）
    ny = 110
    nh = ht * 0.42
    gx = 200
    gy = ny + nh + 40
    gw = wd * 0.55 - gx
    rowH = min 22 ((ht - gy - 40) / fromIntegral nActs)
    dead = fromIntegral wDeadline :: Double
    unit = gw / fl (max cT (dead + 2))
    tx v = gx + fl v * unit
    gBottom = gy + fromIntegral nActs * rowH
    gantt = Pictures $
      concatMap ganttRow is
      ++ [ seg 2 (hsb 50 90 100) (tx cT, gy - 10) (tx cT, gBottom)
         , dash 2 (hsba 0 0 100 0.6) (tx dead, gy - 10) (tx dead, gBottom)
         , txt (tx cT + 4) (gy - 12) 14 (hsb 50 90 100) ("Project " ++ n0 cT ++ " d")
         , txtR (tx dead + 4) (gBottom + 16) 14 (gr 80) ("Deadline " ++ show wDeadline) ]
    ganttRow i =
      let y = gy + fromIntegral i * rowH
          a = actAt ! i
      in [ txt 40 (y + rowH * 0.75) 14 (gr 80) (aId a : ' ' : aName a)
         , box (if crit ! i then hsb 0 75 95 else hsb 200 60 85) (tx (cES ! i)) (y + 2) (fl (durA ! i) * unit) (rowH - 5) ]
         ++ (if crit ! i then [] else
              [ box (hsba 200 40 85 0.3) (tx (cEF ! i)) (y + 2) (fl (cLF ! i - cEF ! i) * unit) (rowH - 5)
              , txt (tx (cLF ! i) + 4) (y + rowH * 0.75) 14 (gr 60) ("slack " ++ n0 (cLS ! i - cES ! i)) ])

    -- 右下：シミュレーション（M）
    hx = wd * 0.6
    hw = wd - hx - 40
    hy = gy
    hh = ht - hy - 90
    simPic = Pictures $
      txt hx (hy - 12) 14 (gr 90) "PERT simulation (M): what if durations vary?"
      : if null wSpans
          then [txt hx (hy + 20) 14 (gr 55) "Press M: random durations, 3000 project runs"]
          else simBody
    simBody =
      let lo = floor (minimum wSpans) :: Int
          hi = ceiling (maximum wSpans) :: Int
          nb = max 1 (hi - lo)
          cnt = accumArray (+) 0 (0, nb) [(max 0 (min nb (floor s - lo)), 1) | s <- wSpans] :: Array Int Int
          cm = maximum (1 : elems cnt)
          nS = length wSpans
          late = length (filter (> dead) wSpans)
          bars = (hh - 60)
          bw = hw / fromIntegral (nb + 1)
          mean = sum wSpans / fromIntegral nS
          runs = fromIntegral (max 1 wRuns) :: Float
          critS i c = aId (actAt ! i) : ' ' : nf 0 (100 * fromIntegral c / runs) ++ "%"
          critL = zipWith critS [0 ..] wCrit
      in [ let h = fromIntegral c / fromIntegral cm * bars
           in box (if fromIntegral (lo + b + 1) > dead then hsb 0 70 90 else hsb 140 50 80) (hx + fromIntegral b * bw) (hy + bars - h) (bw - 2) h
         | (b, c) <- assocs cnt ]
         ++ [ txt hx (hy + hh - 44) 14 (gr 60) (show lo ++ " d")
            , txtR (hx + hw) (hy + hh - 44) 14 (gr 60) (show hi ++ " d")
            , txt hx (hy + hh - 24) 14 (gr 85) (show wRuns ++ " runs  mean " ++ nf 1 mean ++ " (CPM " ++ n0 cT ++ ")  P(late) " ++ nf 1 (100 * fromIntegral late / fromIntegral nS :: Float) ++ "%")
            , txt hx (hy + hh - 4) 14 (gr 85) ("Critical: " ++ unwords (take 5 critL))
            , txt hx (hy + hh + 16) 14 (gr 85) ("          " ++ unwords (drop 5 critL)) ]

    hud = Pictures
      [ txt 20 40 16 white ("Click: select   U/D: " ++ aId sel : ' ' : aName sel ++ " days " ++ n0 (durA ! wSel) ++ "   L/R: deadline " ++ show wDeadline ++ "   M: simulate   R: reset")
      , txt 20 68 14 (gr 60) ("Project " ++ n0 cT ++ " days. Red = critical path (slack 0): shorten it and the project shortens. Blue may slip by its slack.")
      ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        hit i = let (x, y) = nodePos env i in abs (mx - x) < boxHW && abs (my - y) < boxHH
    in pure (foldl' (\a i -> if hit i then addDays i 1 a { wSel = i } else a) w [0 .. nActs - 1])
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (addDays (wSel w) 1 w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (addDays (wSel w) (-1) w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wDeadline = min 60 (wDeadline w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wDeadline = max 5 (wDeadline w - 1) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> simulateOnce (resetSim w)
    'r' -> pure (resetSim w { wDur = map aD0 acts })
    _ -> pure w
  _ -> pure w

addDays :: Int -> Double -> World -> World
addDays i d w = resetSim w { wDur = [ if k == i then max 1 (min 20 (v + d)) else v | (k, v) <- zip [0 ..] (wDur w) ] }

-- ---------- やってみよう ----------
-- 1. 青い作業（余裕あり）を ↑ で伸ばしていくと、何日でクリティカルパスが入れ替わる？
-- 2. 予算で 1 つだけ 2 日縮められるなら、どの作業を縮める？ 縮めても工期が変わらない作業は？
-- 3. M のシミュレーション：平均の工期は CPM の工期より長くなる。なぜ？（合流点で「遅いほう」を待つから）
-- 4. 作業を足そう（例：Act 'J' "Furniture" 2 [5] を I の前に）。前の作業の番号に注意


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

-- 文字：右端を x にそろえる
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size = \c s -> label (x - fromIntegral (length s) * size * 0.72) (negate y) size c s

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

-- 点線（8 描いて 6 空ける）
dash :: Float -> Color -> Point -> Point -> Picture
dash t c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      pt d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      ds = takeWhile (< l) [0, 14 ..]
  in if l < 1 then Blank else Pictures [seg t c (pt d) (pt (min l (d + 8))) | d <- ds]

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

-- 矢印（線 3 本）
arrowP :: Float -> Color -> Point -> Point -> Picture
arrowP t c a@(x1, y1) b@(x2, y2) =
  let ang = atan2 (y2 - y1) (x2 - x1)
      wing d = (x2 - 10 * cos (ang + d), y2 - 10 * sin (ang + d))
  in Pictures [seg t c a b, seg t c b (wing (-0.4)), seg t c b (wing 0.4)]

-- 三角分布（最小 lo、最頻 md、最大 hi）の乱数
triangular :: Double -> Double -> Double -> IO Double
triangular lo md hi = do
  u <- randomRIO (0, 1)
  let f = (md - lo) / (hi - lo)
  pure $ if u < f
    then lo + sqrt (u * (hi - lo) * (md - lo))
    else hi - sqrt ((1 - u) * (hi - lo) * (hi - md))
