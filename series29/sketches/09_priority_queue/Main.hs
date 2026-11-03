-- ============================================
-- 09 優先度つき待ち行列
-- 客が 2 種類：赤 = 優先客（救急・VIP）、青 = ふつうの客。窓口は 1 つ（平均サービス 1 分、指数分布）
--   1 = 先着順（FCFS）　2 = 優先・割り込みなし（いまのサービスは最後まで）　3 = 優先・割り込みあり（青を中断して赤を先に）
--   優先すると赤はほとんど待たないが、そのぶん青が待つ。全体の平均はほぼ変わらない（保存則）
-- 1・2・3 = ルール / ↑↓ = 赤の到着率 λ1 / ←→ = 青の到着率 λ2 / F = 早送り / R = 測り直す
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr, (<|), (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

mu :: Double
mu = 1            -- ★ サービス率

ruleNames :: [String]
ruleNames = ["", "First come first served (FCFS)", "Priority, no preemption", "Priority with preemption"]

-- 客：クラス（1 = 赤、2 = 青）、到着時刻、残りのサービス時間、割り込まれたか
data Cust = Cust { cCls :: !Int, cArr, cRem :: !Double, cPre :: !Bool }

data World = World
  { wLam1, wLam2 :: !Float   -- ★ 到着率（赤・青）
  , wMode :: !Int            -- 1, 2, 3
  , wFast :: !Bool
  , wT, wNA1, wNA2, wDep :: !Double
  , wCur :: !(Maybe Cust)
  , wQ1, wQ2 :: !(Seq Cust)
  , wSum1, wSum2 :: !Double
  , wCnt1, wCnt2, wPre, wFlash :: !Int
  }

main :: IO ()
main = runSketch (sketch "29-09 Priority Queue" world0)
  { skBack  = hsb 345 20 12
  , skStart = \_ -> resetSim world0
  , skStep  = \_ _ w -> do
      w1 <- advance (wT w + (if wFast w then 200 else 15) / 60) w
      pure w1 { wFlash = max 0 (wFlash w1 - 1) }
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 0.3 0.5 2 False 0 0 0 infinity Nothing Seq.empty Seq.empty 0 0 0 0 0 0

lam1, lam2 :: World -> Double
lam1 = realToFrac . wLam1
lam2 = realToFrac . wLam2

resetSim :: World -> IO World
resetSim w = do
  a <- expRand (lam1 w)
  b <- expRand (lam2 w)
  pure w { wT = 0, wNA1 = a, wNA2 = b, wDep = infinity, wCur = Nothing, wQ1 = Seq.empty, wQ2 = Seq.empty
         , wSum1 = 0, wSum2 = 0, wCnt1 = 0, wCnt2 = 0, wPre = 0, wFlash = 0 }

begin :: Cust -> World -> World
begin c w = w { wCur = Just c, wDep = wT w + cRem c }

-- 次にサービスする客を選ぶ
pickNext :: World -> (Maybe Cust, World)
pickNext w
  | wMode w == 1 = case (viewl (wQ1 w), viewl (wQ2 w)) of   -- 先着順：2 つの列の先頭のうち、早く来たほう
      (a :< r1, b :< _) | cArr a < cArr b -> (Just a, w { wQ1 = r1 })
      (a :< r1, EmptyL) -> (Just a, w { wQ1 = r1 })
      (_, b :< r2) -> (Just b, w { wQ2 = r2 })
      _ -> (Nothing, w)
  | otherwise = case (viewl (wQ1 w), viewl (wQ2 w)) of
      (a :< r1, _) -> (Just a, w { wQ1 = r1 })
      (_, b :< r2) -> (Just b, w { wQ2 = r2 })
      _ -> (Nothing, w)

advance :: Double -> World -> IO World
advance tEnd w0
  | tn > tEnd = pure w0 { wT = tEnd }
  | tn == wDep w0 = do                              -- サービス終了
      let w1 = case wCur w of
            Just c | cCls c == 1 -> w { wSum1 = wSum1 w + tn - cArr c, wCnt1 = wCnt1 w + 1 }
            Just c -> w { wSum2 = wSum2 w + tn - cArr c, wCnt2 = wCnt2 w + 1 }
            Nothing -> w
          (nx, w2) = pickNext w1 { wCur = Nothing, wDep = infinity }
      advance tEnd (maybe w2 (`begin` w2) nx)
  | otherwise = do                                  -- 到着
      let cls = if tn == wNA1 w0 then 1 else 2 :: Int
      s <- expRand mu
      g <- expRand (if cls == 1 then lam1 w0 else lam2 w0)
      let c = Cust cls tn s False
          w1 = if cls == 1 then w { wNA1 = tn + g } else w { wNA2 = tn + g }
          w2 = case wCur w1 of
            Nothing -> begin c w1
            Just cur
              | wMode w1 == 3 && cls == 1 && cCls cur == 2 ->   -- ★ 割り込み：青の残り時間を覚えて列の先頭へ戻す
                  begin c w1 { wQ2 = cur { cRem = wDep w1 - tn, cPre = True } <| wQ2 w1, wPre = wPre w1 + 1, wFlash = 20 }
              | cls == 1 -> w1 { wQ1 = wQ1 w1 |> c }
              | otherwise -> w1 { wQ2 = wQ2 w1 |> c }
          w3 = if Seq.length (wQ2 w2) > 600 then w2 { wQ2 = dropLast (wQ2 w2) } else w2
      advance tEnd w3
  where
    tn = minimum [wNA1 w0, wNA2 w0, wDep w0]
    w = w0 { wT = tn }
    dropLast q = case viewr q of
      rest :> _ -> rest
      EmptyR -> q

-- ★ 理論値：各クラスの平均滞在時間 T（待ち＋サービス）
theory :: World -> (Double, Double)
theory w
  | r >= 1 = (infinity, infinity)
  | wMode w == 1 = let t = 1 / (mu - lam1 w - lam2 w) in (t, t)
  | wMode w == 2 = (bigR / (1 - r1) + 1 / mu, bigR / ((1 - r1) * (1 - r)) + 1 / mu)
  | otherwise = (1 / (mu * (1 - r1)), 1 / (mu * (1 - r1)) + (r / mu) / ((1 - r1) * (1 - r)))
  where
    r1 = lam1 w / mu
    r = (lam1 w + lam2 w) / mu
    bigR = (lam1 w + lam2 w) / (mu * mu)

showT :: Double -> String
showT v = if isInfinite v then "inf" else nf 2 v

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawServer w
  , drawBars w
  , drawText w
  , txt 20 40 18 (gray 1) ("1-3: rule  lambda1 = " ++ nf 2 (wLam1 w) ++ " (Up/Down)  lambda2 = " ++ nf 2 (wLam2 w) ++ " (<- ->)  F: fast "
      ++ (if wFast w then "ON" else "off") ++ "  R: again  Time " ++ nf 0 (wT w))
  , txt 20 68 15 (hsb 0 0 60) "ER, network traffic (voice first), OS scheduling. Priority is a design choice: who should wait?"
  ]

-- 窓口と 2 本の列
drawServer :: World -> Picture
drawServer w = Pictures
  ( box sx (sy - 40) 60 80 (hsb 200 50 (maybe 30 (const 75) (wCur w)))
  : [ box (sx - 8) (sy - 48) 76 96 (hsba 0 80 100 (fromIntegral (wFlash w) * 4 / 100)) | wFlash w > 0 ]
 ++ [ dot (sx - 20) sy 26 (if cCls c == 1 then hsb 0 75 100 else hsb 210 70 100) | Just c <- [wCur w] ]
 ++ [ dot (sx - 56 - fromIntegral k * 16) (sy - 26) 13 (hsb 0 75 100) | k <- [0 .. min (Seq.length (wQ1 w)) 40 - 1] ]
 ++ concat [ let x = sx - 56 - fromIntegral k * 16
             in dot x (sy + 26) 13 (hsb 210 70 (if cPre c then 60 else 100)) : [ hoop x (sy + 26) 16 1 (gray 1) | cPre c ]
           | (k, c) <- zip [0 :: Int ..] (take 40 (toList (wQ2 w))) ]
 ++ [ txt (sx - 140) (sy - 44) 14 (hsb 0 0 70) "Priority line"
    , txt (sx - 360) (sy + 56) 14 (hsb 0 0 70) "Normal line (white ring = was preempted)" ] )
  where
    sx = width * 0.55
    sy = 190

-- 結果
drawBars :: World -> Picture
drawBars w = Pictures
  ( seg 1 (hsba 0 0 100 0.3) (bx, by) (bx + bw * 2 + 60, by)
  : bar (1 :: Int) (wSum1 w) (wCnt1 w) th1 ++ bar 2 (wSum2 w) (wCnt2 w) th2
 ++ [ txt bx (by - bh - 10) 14 (hsb 0 0 60) "Mean time in system T (color = measured, white line = theory)" ] )
  where
    (th1, th2) = theory w
    bx = 80
    by = height - 80
    bh = height * 0.4
    bw = 120
    tmax = 12 :: Float
    bar k total cnt th =
      let m = fl (total / fromIntegral (max cnt 1))
          x = bx + fromIntegral (k - 1) * (bw + 60)
          h = min (m / tmax) 1 * bh
          h2 = min (fl th / tmax) 1 * bh
      in [ box x (by - h) bw h (if k == 1 then hsb 0 70 95 else hsb 210 65 95)
         , seg 3 (gray 1) (x - 8, by - h2) (x + bw + 8, by - h2)
         , txtC (x + bw / 2) (by - h - 10) 16 (gray 1) (nf 2 m ++ " min")
         , txtC (x + bw / 2) (by + 22) 14 (hsb 0 0 60) (if k == 1 then "Red (priority)" else "Blue (normal)")
         , txtC (x + bw / 2) (by + 40) 14 (hsb 0 0 60) ("Theory " ++ showT th) ]

drawText :: World -> Picture
drawText w = Pictures
  [ txt tx ty 20 (gray 1) ("Rule: " ++ concat (take 1 (drop (wMode w) ruleNames)))
  , txt tx (ty + 34) 16 (gray 1) ("rho1 = " ++ nf 2 (lam1 w / mu) ++ "  rho2 = " ++ nf 2 (lam2 w / mu) ++ "  rho = " ++ nf 2 r ++ (if r >= 1 then " -> unstable" else ""))
  , txt tx (ty + 62) 16 (gray 1) ("Mean time, everyone: measured " ++ nf 2 allM ++ "  theory " ++ showT allTh)
  , txt tx (ty + 90) 16 (gray 1) ("Served: red " ++ show (wCnt1 w) ++ "  blue " ++ show (wCnt2 w) ++ "  preempted " ++ show (wPre w))
  , txt tx (ty + 120) 14 (hsb 0 0 60) "The overall mean barely changes with the rule (with exponential"
  , txt tx (ty + 140) 14 (hsb 0 0 60) "service, even with preemption). The rule only changes who"
  , txt tx (ty + 160) 14 (hsb 0 0 60) "carries the waiting."
  ]
  where
    tx = width * 0.55
    ty = height * 0.45
    r = (lam1 w + lam2 w) / mu
    (th1, th2) = theory w
    allM = (wSum1 w + wSum2 w) / fromIntegral (max (wCnt1 w + wCnt2 w) 1)
    allTh = (lam1 w * th1 + lam2 w * th2) / (lam1 w + lam2 w)

-- ★ λ1 + λ2 ≦ 0.95 に保つ（ρ < 1）
limitLams :: Bool -> World -> World
limitLams upKey w =
  let w1 | wLam1 w + wLam2 w > 0.95 =
             if upKey then w { wLam1 = 0.95 - wLam2 w } else w { wLam2 = 0.95 - wLam1 w }
         | otherwise = w
  in w1 { wLam1 = roundTo 100 (wLam1 w1), wLam2 = roundTo 100 (wLam2 w1) }

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> resetSim (limitLams True w { wLam1 = min 0.9 (wLam1 w + 0.05) })
  EventKey (SpecialKey KeyDown) Down _ _ -> resetSim (limitLams False w { wLam1 = max 0.05 (wLam1 w - 0.05) })
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim (limitLams False w { wLam2 = min 0.9 (wLam2 w + 0.05) })
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim (limitLams False w { wLam2 = max 0.05 (wLam2 w - 0.05) })
  EventKey (Char c) Down _ _
    | c `elem` "123" -> resetSim w { wMode = digitToInt c }
    | c `elem` "fF" -> pure w { wFast = not (wFast w) }
    | c `elem` "rR" -> resetSim w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 → 2 → 3 と切り替えて、赤と青の待ち時間がどう入れかわるかを見る（F で早送り）
-- 2. 赤の割合を増やしていく（↑）。優先が多すぎると「優先」の意味がなくなるのはなぜ？
-- 3. 3 クラスに増やす（wQ3 を追加）。いちばん下のクラスの待ち時間はどうなる？
-- 4. サービス時間の短い客を優先する（SPT ルール）と全員の平均が減る。pickNext を cRem の小さい順に変えて試す

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- シミュレーションの時刻は Double、描くときは Float
fl :: Double -> Float
fl = realToFrac

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)

infinity :: Double
infinity = 1 / 0

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
