-- ============================================
-- 06 在庫管理 ─ 経済的発注量 EOQ
-- 毎日売れていく商品を、1 回に何個ずつ注文する？ たくさん頼むと保管費がかさみ、少しずつ頼むと発注の手間（送料）がかさむ
--   年間費用 = 発注費 K × D / Q ＋ 保管費 h × Q / 2 → 一番安い Q* = √(2 D K / h)（ハリスの公式、1913）
-- ←→ = 1 回の発注量 Q / ↑↓ = 発注費 K / Z・X = 保管費 h / O = Q* にする / N = 需要のばらつき / B = 安全在庫 / スペース = 一時停止
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

demandD, leadL, daysN :: Float
demandD = 2400                         -- ★ 年間の需要（個/年）
leadL = 8                              -- ★ リードタイム（注文してから届くまでの日数）
daysN = 360

data EvType = EvIn | EvOrder deriving Eq

data World = World
  { wK, wH, wQ :: !Float               -- ★ 1 回の発注費（円）、1 個を 1 年保管する費用（円）、1 回の発注量
  , wNoisy, wSafety, wPaused :: !Bool
  , wDay :: !Int
  , wStock :: !Float
  , wOnOrder :: ![(Float, Int)]        -- 届く予定（個数、到着日）
  , wLog :: !(Seq.Seq (Int, Float))    -- 在庫の記録（400 日分）
  , wEvents :: ![(Int, EvType)]        -- 注文と入荷（新しい順、400 日分）
  , wStockouts :: !Int
  , wLost :: !Float
  }

world0 :: World
world0 = resetSim (World 3000 40 300 False False False 0 0 [] Seq.empty [] 0 0)

main :: IO ()
main = runSketch (sketch "30-06 EOQ Inventory" world0)
  { skBack  = hsb 225 30 12
  , skStep  = \_ _ w -> if wPaused w then pure w else stepDay w >>= stepDay
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

resetSim :: World -> World
resetSim w = w { wDay = 0, wStock = wQ w, wOnOrder = [], wLog = Seq.empty, wEvents = [], wStockouts = 0, wLost = 0 }

qStar :: World -> Float
qStar World{..} = sqrt (2 * demandD * wK / wH)

annualCost :: World -> Float -> Float
annualCost World{..} q = wK * demandD / q + wH * q / 2

sigmaDaily :: Float
sigmaDaily = (demandD / daysN) * 0.6   -- ばらつきの大きさ（1 日あたり）

safetyStock :: World -> Float
safetyStock w = if wSafety w then 1.65 * sigmaDaily * sqrt leadL else 0   -- ★ 95% 欠品しない安全在庫

reorderPoint :: World -> Float
reorderPoint w = demandD / daysN * leadL + safetyStock w   -- 発注点：届くまでの間に売れる量（＋安全在庫）

-- 1 日分進める
stepDay :: World -> IO World
stepDay w@World{..} = do
  z <- gauss
  let mean = demandD / daysN
      dmd = if wNoisy then max 0 (mean + z * sigmaDaily) else mean
      (arrived, waiting) = span' (\(_, a) -> a <= wDay) wOnOrder
      stock0 = wStock + sum (map fst arrived)
      evIn = [(wDay, EvIn) | _ <- arrived]
      lastV = case Seq.viewr wLog of
        _ Seq.:> (_, v) -> v
        Seq.EmptyR -> 0
      (stock1, lost, outs)
        | stock0 >= dmd = (stock0 - dmd, wLost, wStockouts)
        | otherwise = (0, wLost + dmd - stock0, if stock0 > 0 || lastV > 0 then wStockouts + 1 else wStockouts)
      pos = stock1 + sum (map fst waiting)      -- 在庫ポジション（届く予定も含む）
      order = pos <= reorderPoint w
      onOrder = if order then waiting ++ [(wQ, wDay + round leadL)] else waiting
      evOrd = [(wDay, EvOrder) | order]
      logN = wLog Seq.|> (wDay, stock1)
      logT = if Seq.length logN > 400 then Seq.drop 1 logN else logN
      events = takeWhile (\(d, _) -> d > wDay - 400) (evOrd ++ evIn ++ wEvents)
  pure w { wStock = stock1, wLost = lost, wStockouts = outs, wOnOrder = onOrder, wLog = logT, wEvents = events, wDay = wDay + 1 }

-- 条件を満たすものと満たさないものに分ける
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (filter p xs, filter (not . p) xs)

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [saw, curves, numbers, hud]
  where
    wd = envW env
    ht = envH env

    -- 上：在庫の推移（のこぎり形）
    cx = 80
    cy = 110
    cw = wd - 160
    chh = ht * 0.32
    yMax = maximum [wQ * 1.25 + safetyStock w, qStar w * 1.25, 100]
    x0 = wDay - 200
    xS d = cx + fromIntegral (d - x0) / 200 * cw
    yS v = cy + chh - v / yMax * chh
    recent = filter (\(d, _) -> d >= x0) (toList wLog)
    rp = reorderPoint w
    saw = Pictures $
      [ seg 1 (hsba 0 0 100 0.25) (cx, cy + chh) (cx + cw, cy + chh)
      , seg 1 (hsba 0 0 100 0.25) (cx, cy) (cx, cy + chh)
      , dash 1 (hsba 30 70 100 0.6) (cx, yS rp) (cx + cw, yS rp)
      , if wSafety then seg 1 (hsba 140 60 90 0.6) (cx, yS (safetyStock w)) (cx + cw, yS (safetyStock w)) else Blank
      , path 2 (hsb 190 70 100) [(xS d, yS v) | (d, v) <- recent]
      ]
      ++ [ if t == EvOrder
             then seg 2 (hsb 50 90 100) (xS d, cy + chh) (xS d, cy + chh + 10)
             else fillPoly (hsb 140 80 100) [(xS d, cy + chh + 4), (xS d - 6, cy + chh + 14), (xS d + 6, cy + chh + 14)]
         | (d, t) <- wEvents, d >= x0 ]
      ++ [ dot (hsb 0 80 100) (xS d) (cy + chh) 6 | (d, v) <- recent, v <= 0 ]
      ++ [ txt cx (cy - 8) 14 (gr 70) "Stock (units)"
         , txt (cx + cw - 400) (yS rp - 6) 14 (hsb 30 70 100) ("Reorder point " ++ nf 0 rp ++ " (order when stock gets here)")
         , txt cx (cy + chh + 32) 14 (gr 60) ("Yellow tick = order, green triangle = arrival, red dot = stockout  (day " ++ show wDay ++ ")")
         ]

    -- 下：費用の曲線
    gx = 80
    gy = cy + chh + 70
    gw = wd * 0.58
    gh = ht - gy - 60
    qMax = max 1500 (wQ * 1.2)
    cMax = annualCost w (qMax * 0.05) * 0.7
    gX q = gx + q / qMax * gw
    gY c = gy + gh - min c cMax / cMax * gh
    qs = qStar w
    curveQs = [qMax * 0.03 + fromIntegral k * qMax / 150 | k <- [0 .. 145 :: Int]]
    curve f col name isTotal = Pictures
      [ path (if isTotal then 3 else 2) col [(gX q, gY (f q)) | q <- curveQs]
      , txt (gX (qMax * 0.86)) (gY (f (qMax * 0.86)) + (if isTotal then -10 else 20)) 14 col name ]
    curves = Pictures
      [ seg 1 (hsba 0 0 100 0.25) (gx, gy + gh) (gx + gw, gy + gh)
      , seg 1 (hsba 0 0 100 0.25) (gx, gy) (gx, gy + gh)
      , curve (\q -> wK * demandD / q) (hsb 30 70 100) "Order K*D/Q" False
      , curve (\q -> wH * q / 2) (hsb 190 70 100) "Hold h*Q/2" False
      , curve (annualCost w) (hsb 50 90 100) "Total" True
      , dash 1 (hsb 140 70 100) (gX qs, gy) (gX qs, gy + gh)
      , dot (hsb 140 70 100) (gX qs) (gY (annualCost w qs)) 10
      , txt (gX qs + 6) (gy + 14) 14 (hsb 140 70 100) ("Q* = " ++ nf 0 qs)
      , dot white (gX wQ) (gY (annualCost w wQ)) 14
      , txtC (gx + gw / 2) (gy + gh + 14) 14 (gr 60) "Order size Q ->"
      ]

    -- 右：数字
    rx = gx + gw + 50
    infoLines =
      [ "Orders per year D/Q = " ++ nf 1 (demandD / wQ)
      , "Ordering " ++ nf 0 (wK * demandD / wQ) ++ " yen/yr"
      , "Holding " ++ nf 0 (wH * wQ / 2) ++ " yen/yr"
      , "Total " ++ nf 0 (annualCost w wQ) ++ " yen/yr"
      , ""
      , "Q* = sqrt(2*" ++ nf 0 demandD ++ "*" ++ nf 0 wK ++ "/" ++ nf 0 wH ++ ") = " ++ nf 0 qs
      , "Total at Q*: " ++ nf 0 (annualCost w qs) ++ " yen/yr"
      , "Now " ++ nf 1 (100 * (annualCost w wQ / annualCost w qs - 1)) ++ "% above the best"
      , ""
      , "Stockouts " ++ show wStockouts ++ " (lost " ++ nf 0 wLost ++ " units)"
      , if wNoisy then "Noisy demand: stock may run out" else "N: make the demand vary"
      ]
    numbers = Pictures $
      txt rx (gy + 10) 20 white ("Q = " ++ nf 0 wQ ++ " units")
      : [ txt rx (gy + 44 + i * 24) 14 (gr 85) s | (i, s) <- zip [0 ..] infoLines ]

    hud = Pictures
      [ txt 20 40 16 white ("L/R Q " ++ nf 0 wQ ++ "  U/D order cost K " ++ nf 0 wK ++ "  Z/X holding h " ++ nf 0 wH ++ "  O: use Q*  N: noise " ++ onOff wNoisy
          ++ "  B: safety " ++ onOff wSafety ++ "  Space: " ++ (if wPaused then "resume" else "pause"))
      , txt 20 68 14 (gr 60) "The bottom of the total curve is Q*. It is flat there: small errors in Q barely change the cost (EOQ is robust)."
      ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- キー
onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wQ = min 3000 (wQ w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wQ = max 20 (wQ w - 20) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wK = min 20000 (wK w + 500) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wK = max 500 (wK w - 500) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'x' -> w { wH = min 400 (wH w + 10) }
    'z' -> w { wH = max 10 (wH w - 10) }
    'o' -> w { wQ = fromIntegral (round (qStar w) :: Int) }
    'n' -> resetSim w { wNoisy = not (wNoisy w) }
    'b' -> resetSim w { wSafety = not (wSafety w) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. Q を Q* の 2 倍にすると費用は何 % 増える？ 半分にすると？（どちらも約 25%）
-- 2. 発注費 K を 4 倍にすると Q* は何倍？ 式の √ を確かめよう
-- 3. N でばらつかせて品切れを起こし、B（安全在庫）で減るか見よう。安全在庫の代わりに何が増える？
-- 4. 「まとめて買うと 5% 引き」（数量割引）を annualCost に入れると最適な Q はどう変わる？


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

-- 正規分布の乱数（ボックス＝ミュラー法、p5.js の randomGaussian）
gauss :: IO Float
gauss = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

