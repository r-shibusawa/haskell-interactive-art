-- ============================================
-- 07 新聞売り子問題 ─ 不確かな需要と発注量
-- 明日の弁当を何個仕入れる？ 売れ残りは捨て値、足りなければ売りそこね。需要は毎日ランダム
--   最適な Q は「需要が Q 以下である確率 = (p − c) / (p − s)」を満たす点（クリティカル・フラクタイル）
-- ←→ = 仕入れ数 Q / ↑↓ = 仕入れ値 c / O = 最適な Q / スペース = 毎日のシミュレーション ON/OFF / C = 記録を消す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Array.Unboxed (UArray, accumArray, assocs, elems)
import Data.Char (toLower)
import Data.Foldable (foldl', toList)
import qualified Data.Sequence as Seq

price, salvage, mu, sigma :: Float
price = 600                            -- ★ 売り値 p（円）
salvage = 50                           -- ★ 売れ残りの処分価格 s（円）
mu = 100                               -- ★ 需要の平均
sigma = 25                             -- ★ 需要の標準偏差（正規分布）

data World = World
  { wCost :: !Float                    -- ★ 仕入れ値 c（円）
  , wQ :: !Float
  , wRunning :: !Bool
  , wLog :: !(Seq.Seq (Float, Float))  -- シミュレーション：需要と利益の記録（3000 日分）
  , wCurve :: [(Float, Float)]         -- 期待利益の曲線（c が変わったら計算し直す）
  }

world0 :: World
world0 = World 300 100 True Seq.empty (expectedCurve 300)

main :: IO ()
main = runSketch (sketch "30-07 Newsvendor" world0)
  { skBack  = hsb 225 30 12
  , skStep  = \_ _ w -> if wRunning w then simDay w >>= simDay >>= simDay else pure w
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

simDay :: World -> IO World
simDay w = do
  z <- gauss
  let d = max 0 (fromIntegral (round (mu + sigma * z) :: Int))
      l = wLog w Seq.|> (d, profitOf (wCost w) (wQ w) d)
  pure w { wLog = if Seq.length l > 3000 then Seq.drop 1 l else l }

profitOf :: Float -> Float -> Float -> Float
profitOf c q d = price * min q d + salvage * max 0 (q - d) - c * q

-- 需要 0, 1, 2, ... とその確率の表
demandTable :: [(Float, Float)]
demandTable = [(d, normPdf d mu sigma) | k <- [0 .. round (mu + 5 * sigma) :: Int], let d = fromIntegral k]

-- ★ 期待利益：需要 d ごとの利益 × その確率 を足す
expectedProfit :: Float -> Float -> Float
expectedProfit c q =
  let (s, tot) = foldl' (\(!a, !b) (d, pr) -> (a + pr * profitOf c q d, b + pr)) (0, 0) demandTable
  in s / tot

dMax :: Float
dMax = mu + 4 * sigma

expectedCurve :: Float -> [(Float, Float)]
expectedCurve c = [ (q, expectedProfit c q) | k <- [0 .. floor (dMax / 2) :: Int], let q = fromIntegral k * 2 ]

criticalFractile :: Float -> Float
criticalFractile c = (price - c) / (price - salvage)

optimalQ :: Float -> Float
optimalQ c = mu + sigma * invNorm (criticalFractile c)

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures [dist, expCurve, hist, numbers, hud]
  where
    wd = envW env
    ht = envH env
    cf = criticalFractile wCost
    qOpt = optimalQ wCost

    -- 左上：需要の分布と Q
    ax = 80
    ay = 130
    aw = wd * 0.42
    ah = ht * 0.28
    pMax = normPdf mu mu sigma * 1.15
    xD d = ax + d / dMax * aw
    yP p = ay + ah - p / pMax * ah
    dist = Pictures $
      [ box (if d <= wQ then hsba 190 60 85 0.7 else hsba 30 70 95 0.7) (xD d) (yP (normPdf d mu sigma)) (aw / dMax) (ay + ah - yP (normPdf d mu sigma))
      | k <- [0 .. round dMax :: Int], let d = fromIntegral k ]
      ++ [ seg 3 white (xD wQ, ay - 10) (xD wQ, ay + ah)
         , dash 2 (hsb 140 80 100) (xD qOpt, ay - 10) (xD qOpt, ay + ah)
         , txt (xD wQ + 6) ay 14 white ("Q = " ++ nf 0 wQ)
         , txt (xD qOpt + 6) (ay + 18) 14 (hsb 140 70 100) ("Q* = " ++ nf 1 qOpt)
         , txt ax (ay - 40) 14 (gr 70) ("Demand: mean " ++ nf 0 mu ++ ", sd " ++ nf 0 sigma)
         , txt ax (ay - 20) 14 (gr 70) ("Blue: enough stock (prob " ++ nf 3 (normCdf wQ mu sigma) ++ "), orange: short")
         , txtC (ax + aw / 2) (ay + ah + 14) 14 (gr 70) "Demand (units) ->"
         ]

    -- 右上：期待利益の曲線
    bx = ax + aw + 70
    bw = wd - bx - 50
    es = map snd wCurve
    eMax = maximum (0 : es)
    eMin = minimum (0 : es)
    xQ q = bx + q / dMax * bw
    yE e = ay + ah - (e - eMin) / max 1 (eMax - eMin) * ah
    eq = expectedProfit wCost wQ
    expCurve = Pictures
      [ seg 1 (hsba 0 0 100 0.25) (bx, yE 0) (bx + bw, yE 0)
      , path 3 (hsb 50 90 100) [(xQ q, yE e) | (q, e) <- wCurve]
      , dot white (xQ wQ) (yE eq) 12
      , dot (hsb 140 80 100) (xQ qOpt) (yE (expectedProfit wCost qOpt)) 10
      , txt bx (ay - 20) 14 (gr 70) "Order size Q -> expected profit per day"
      , txt (min (xQ wQ + 10) (wd - 200)) (yE eq + 20) 14 white ("Expected " ++ nf 0 eq ++ " yen")
      ]

    -- 下左：シミュレーションの利益のヒストグラム
    hx = 80
    hy = ay + ah + 80
    hw = wd * 0.42
    hh = ht - hy - 70
    lo = profitOf wCost wQ 0
    hi = profitOf wCost wQ wQ
    nb = 30 :: Int
    binOf p = max 0 (min (nb - 1) (floor ((p - lo) / max 1 (hi - lo) * fromIntegral (nb - 1))))
    cnt = accumArray (+) 0 (0, nb - 1) [(binOf p, 1) | (_, p) <- toList wLog] :: UArray Int Int
    cm = maximum (1 : elems cnt)
    hist = Pictures $
      [ let h = fromIntegral c / fromIntegral cm * hh
        in box (if b == nb - 1 then hsb 190 60 85 else hsb 30 60 90) (hx + fromIntegral b * hw / fromIntegral nb) (hy + hh - h) (hw / fromIntegral nb - 2) h
      | (b, c) <- assocs cnt ]
      ++ [ txt hx (hy - 12) 14 (gr 70) ("Profit over " ++ show (Seq.length wLog) ++ " days (blue = sold out: " ++ nf 0 hi ++ " yen)")
         , txt hx (hy + hh + 16) 14 (gr 55) (nf 0 lo ++ " yen")
         , txtR (hx + hw) (hy + hh + 16) 14 (gr 55) (nf 0 hi ++ " yen")
         ]

    -- 下右：数字
    nLog = Seq.length wLog
    avg = if nLog == 0 then 0 else foldl' (\a (_, p) -> a + p) 0 wLog / fromIntegral nLog
    soldOut = foldl' (\a (d, _) -> if d >= wQ then a + 1 else a) (0 :: Int) wLog
    rows =
      [ "Price p = " ++ nf 0 price ++ "  Cost c = " ++ nf 0 wCost ++ "  Salvage s = " ++ nf 0 salvage
      , "One unit short loses p - c = " ++ nf 0 (price - wCost) ++ " yen"
      , "One unit left over loses c - s = " ++ nf 0 (wCost - salvage) ++ " yen"
      , "Critical fractile (p-c)/(p-s) = " ++ nf 3 cf
      , "-> P(demand <= Q*) = " ++ nf 1 (cf * 100) ++ "% at Q* = " ++ nf 1 qOpt
      , ""
      , "Simulated mean profit " ++ nf 0 avg ++ " yen (expected " ++ nf 0 eq ++ ")"
      , "Sold-out days " ++ (if nLog > 0 then nf 1 (100 * fromIntegral soldOut / fromIntegral nLog :: Float) else "-") ++ "%"
      ]
    numbers = Pictures
      [ txt bx (hy + i * 26) 15 (if i == 4 then hsb 140 70 100 else gr 85) s | (i, s) <- zip [0 ..] rows ]

    hud = Pictures
      [ txt 20 40 16 white ("L/R order Q " ++ nf 0 wQ ++ "   U/D cost c " ++ nf 0 wCost ++ "   O: best Q   Space: simulation " ++ (if wRunning then "ON" else "OFF") ++ "   C: clear log")
      , txt 20 68 14 (gr 60) ("Ordering the mean demand (" ++ nf 0 mu ++ ") is not always best: it depends on the balance of the two losses.")
      ]

-- キー
onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> clearLog w { wQ = min 250 (wQ w + 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> clearLog w { wQ = max 0 (wQ w - 2) }
  EventKey (SpecialKey KeyUp) Down _ _ -> setCost (min (price - 10) (wCost w + 10)) w
  EventKey (SpecialKey KeyDown) Down _ _ -> setCost (max (salvage + 10) (wCost w - 10)) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'o' -> clearLog w { wQ = fromIntegral (round (optimalQ (wCost w)) :: Int) }
    'c' -> clearLog w
    _ -> w
  _ -> w

clearLog :: World -> World
clearLog w = w { wLog = Seq.empty }

setCost :: Float -> World -> World
setCost c w = clearLog w { wCost = c, wCurve = expectedCurve c }

-- ---------- やってみよう ----------
-- 1. 仕入れ値 c を上げると Q* は平均より少なく、下げると多くなる。フラクタイルが 0.5 になるのはいつ？
-- 2. Q を Q* から ±20 動かすと期待利益はどれくらい下がる？ 曲線の形で説明しよう
-- 3. sigma（ばらつき）を 50 にすると Q* と期待利益はどう変わる？「需要予測の精度」の値打ちを考えよう
-- 4. 売れ残りを翌日に回せる（パン → ラスク）なら？ salvage を上げてみよう


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

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

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

-- 正規分布の道具
normPdf :: Float -> Float -> Float -> Float
normPdf x m s = exp (-0.5 * ((x - m) / s) ^ (2 :: Int)) / (s * sqrt (2 * pi))

normCdf :: Float -> Float -> Float -> Float
normCdf x m s = 0.5 * (1 + erf ((x - m) / (s * sqrt 2)))

-- 近似式（誤差 1e-7 程度）
erf :: Float -> Float
erf x =
  let t = 1 / (1 + 0.3275911 * abs x)
      y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-x * x)
  in if x >= 0 then y else negate y

-- 標準正規分布の逆関数（二分法）
invNorm :: Float -> Float
invNorm p = go (60 :: Int) (-8) 8
  where
    go 0 lo hi = (lo + hi) / 2
    go n lo hi =
      let m = (lo + hi) / 2
      in if normCdf m 0 1 < p then go (n - 1) m hi else go (n - 1) lo m
