-- ============================================
-- 03 M/M/1 待ち行列
-- M（ポアソン到着）/ M（指数分布のサービス）/ 1（窓口 1 つ）。待ち行列理論のいちばん基本の模型
--   利用率 ρ = λ/μ　平均の系内人数 L = ρ/(1−ρ)　平均の滞在時間 W = 1/(μ−λ)
--   ρ → 1 で L も W も無限大に吹き上がる。「9 割しか使っていないのに大渋滞」
-- ←→ = ρ / ↑↓ = 時間の速さ / スペース = 一時停止 / R = 測り直す / C = グラフの点を消す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

rhoList :: [Double]
rhoList = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.98]   -- ★ 選べる ρ

mu :: Double
mu = 1            -- ★ サービス率（平均サービス時間 = 1/μ）

data World = World
  { wRi :: !Int
  , wSpeed :: !Float         -- ★ 1 秒で進む時間
  , wPaused :: !Bool
  , wT, wNextArr, wNextDep, wArea, wSumW, wSumWq :: !Double
  , wSys :: !(Seq (Double, Double))   -- 系内の客（到着時刻, サービス開始時刻）
  , wDone :: !Int
  , wRec :: !(Seq Int)
  , wMeasured :: !(M.Map Int Float)   -- ρ の番号 → 測った L
  }

main :: IO ()
main = runSketch (sketch "29-03 M/M/1 Queue" world0)
  { skBack  = hsb 220 30 12
  , skStart = \_ -> resetSim world0
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 6 40 False 0 0 infinity 0 0 0 Seq.empty 0 Seq.empty M.empty

rhoOf :: World -> Double
rhoOf w = case drop (wRi w) rhoList of
  (r : _) -> r
  [] -> 0.7

lam :: World -> Double
lam w = rhoOf w * mu

resetSim :: World -> IO World
resetSim w = do
  n <- expRand (lam w)
  pure w { wT = 0, wNextArr = n, wNextDep = infinity, wSys = Seq.empty, wArea = 0
         , wDone = 0, wSumW = 0, wSumWq = 0, wRec = Seq.empty }

advance :: Double -> World -> IO World
advance tEnd w
  | tn > tEnd = pure w { wArea = wArea w + n * (tEnd - wT w), wT = tEnd }
  | wNextArr w <= wNextDep w = do        -- 到着
      s <- expRand mu
      a <- expRand (lam w)
      let first = Seq.null (wSys w)
          sys1 = wSys w |> (tn, if first then tn else -1)
          sys2 = if Seq.length sys1 > 3000 then Seq.take 3000 sys1 else sys1   -- 念のための上限
      advance tEnd moved { wSys = sys2, wNextArr = tn + a, wNextDep = if first then tn + s else wNextDep w }
  | otherwise = case viewl (wSys w) of   -- サービス終了
      (arr, start) :< rest -> do
        s <- expRand mu
        let w1 = moved { wDone = wDone w + 1, wSumW = wSumW w + tn - arr, wSumWq = wSumWq w + start - arr }
        advance tEnd $ if Seq.null rest
          then w1 { wSys = rest, wNextDep = infinity }
          else w1 { wSys = Seq.adjust' (\(a, _) -> (a, tn)) 0 rest, wNextDep = tn + s }
      EmptyL -> advance tEnd moved { wNextDep = infinity }
  where
    tn = min (wNextArr w) (wNextDep w)
    n = fromIntegral (Seq.length (wSys w))
    moved = w { wArea = wArea w + n * (tn - wT w), wT = tn }

stepWorld :: World -> IO World
stepWorld w0 = do
  w1 <- if wPaused w0 then pure w0 else do
    w <- advance (wT w0 + realToFrac (wSpeed w0) / 60) w0
    let r = wRec w |> Seq.length (wSys w)
    pure w { wRec = if fromIntegral (Seq.length r) > width * 0.55 then Seq.drop 1 r else r }
  pure $ if wT w1 > 200 then w1 { wMeasured = M.insert (wRi w1) (fl (measuredL w1)) (wMeasured w1) } else w1

measuredL :: World -> Double
measuredL w = wArea w / max (wT w) 1e-9

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawLine w
  , drawHistory w
  , drawNumbers w
  , drawCurve w
  , txt 20 40 18 (gray 1) ("rho = " ++ nf 2 (rhoOf w) ++ " (<- ->)  Speed x" ++ num (wSpeed w) ++ " (Up/Down)  "
      ++ (if wPaused w then "PAUSED" else "Space: pause") ++ "  R: measure again  C: clear points")
  , txt 20 68 15 (hsb 0 0 60) "Raise rho: 0.5, 0.8, 0.9, 0.95. From 0.9 to 0.95, L doubles from 9 to 19. Spare capacity matters a lot."
  ]

-- 行列を点で
drawLine :: World -> Picture
drawLine w = Pictures
  ( box (qx + 10) (qy - 22) 40 44 (hsb 200 60 80)
  : txt (qx + 10) (qy + 40) 14 (gray 1) "Desk"
  : [ if i == 0 then dot (qx + 30) qy 16 (hsb 140 70 95)
      else dot (qx - 10 - fromIntegral (i - 1) * 9) qy 8 (hsb 30 70 100)
    | i <- [0 .. min n 60 - 1] ]
 ++ [ txt 40 (qy + 5) 14 (hsb 0 80 100) ("+" ++ show (n - 60)) | n > 60 ] )
  where
    qx = width * 0.55
    qy = 130
    n = Seq.length (wSys w)

gx, gy, gw, gh :: Float
gx = 40
gy = height * 0.55
gw = width * 0.55
gh = height * 0.3

-- 系内人数 N(t) の変化
drawHistory :: World -> Picture
drawHistory w = Pictures
  [ seg 1 ax (gx, gy) (gx + gw, gy), seg 1 ax (gx, gy) (gx, gy - gh)
  , seg 1 (hsba 45 90 100 0.6) (gx, yL) (gx + gw, yL)
  , path 1.5 (hsb 30 80 100) [ (gx + fromIntegral i, gy - fromIntegral v / top * gh) | (i, v) <- zip [0 :: Int ..] recs ]
  , txt gx (gy - gh - 8) 14 (hsb 0 0 60) ("N(t), people in system (max " ++ show (round top :: Int) ++ ")  Yellow = theoretical mean L")
  ]
  where
    ax = hsba 0 0 100 0.3
    recs = toList (wRec w)
    top = fromIntegral (maximum (10 : recs)) :: Float
    rho = fl (rhoOf w)
    yL = gy - min (rho / (1 - rho) / top) 1 * gh

-- 数字
drawNumbers :: World -> Picture
drawNumbers w = Pictures
  ( txt tx ty 18 (gray 1) ("rho = lambda/mu = " ++ nf 2 rho ++ "  (lambda = " ++ nf 2 (lam w) ++ ", mu = " ++ num (fl mu) ++ ")")
  : concat [ [ txt (tx + cx) (ty + 30 + fromIntegral i * 26) 16 (cellColor i j) s | (j, cx, s) <- zip3 [0 :: Int ..] [0, 220, 380] row ]
           | (i, row) <- zip [0 :: Int ..] rows ]
 ++ [ txt tx (ty + 30 + 4 * 26 + 6) 14 (hsb 0 0 60)
        ("Time " ++ nf 0 (wT w) ++ "  Served " ++ show (wDone w) ++ "  (near rho = 1 the mean takes very long to settle)") ] )
  where
    tx = 40
    ty = gy + 50
    rho = rhoOf w
    done = fromIntegral (max (wDone w) 1) :: Double
    rows = [ ["", "Simulation", "Theory"]
           , ["L (mean in system)", nf 2 (measuredL w), nf 2 (rho / (1 - rho))]
           , ["W (mean time in sys)", nf 2 (wSumW w / done), nf 2 (1 / (mu - lam w))]
           , ["Wq (mean wait)", nf 2 (wSumWq w / done), nf 2 (rho / (mu - lam w))] ]
    cellColor :: Int -> Int -> Color
    cellColor i j | i == 0 = hsb 0 0 60
                  | j == 2 = hsb 45 90 100
                  | otherwise = gray 1

-- 右：L と ρ の関係
drawCurve :: World -> Picture
drawCurve w = Pictures
  ( [ seg 1 ax (px, py) (px + pw, py), seg 1 ax (px, py) (px, py - ph)
    , seg 1 (hsba 0 80 100 0.5) (px + pw, py) (px + pw, py - ph)
    , path 3 (hsb 45 90 100) curve ]
 ++ [ dot (px + fl r * pw) (py - min l lmax / lmax * ph) 12 (hsb 190 70 100)
    | (i, l) <- M.toList (wMeasured w), r <- take 1 (drop i rhoList) ]
 ++ [ dot (px + fl (rhoOf w) * pw) (py - min (fl (measuredL w)) lmax / lmax * ph) 8 (gray 1)
    , txt px (py - ph - 10) 14 (hsb 0 0 60) "L = rho/(1-rho) (yellow), measured L (cyan)"
    , txt (px - 4) (py + 18) 14 (hsb 0 0 60) "0"
    , txtR (px + pw) (py + 18) 14 (hsb 0 0 60) "rho = 1"
    , txt (px + 6) (py - ph + 14) 14 (hsb 0 0 60) ("L = " ++ show (round lmax :: Int)) ] )
  where
    ax = hsba 0 0 100 0.4
    px = width * 0.65
    py = height - 70
    pw = width * 0.31
    ph = height - 250
    lmax = 20 :: Float
    curve = takeWhile (\(_, y) -> y >= py - ph)
      [ (px + r * pw, py - v / lmax * ph) | i <- [0 .. 199 :: Int], let r = fromIntegral i / 200 * 0.99, let v = r / (1 - r) ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim w { wRi = min (length rhoList - 1) (wRi w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim w { wRi = max 0 (wRi w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 2000 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> resetSim w
    | c `elem` "cC" -> pure w { wMeasured = M.empty }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. すべての ρ で ↑ を押して速く回し、水色の点を集める。理論の曲線に乗る？
-- 2. ρ = 0.98 で N(t) のグラフを見る。平均 49 人のはずなのに、0 人の時間もある。なぜ？
-- 3. 窓口の人件費を減らすため ρ を 0.7 → 0.9 にしたら、待ち時間は何倍になる？（W = 1/(μ−λ) で計算）
-- 4. mu = 2 にして同じ ρ で比べる。L は同じで W は半分になるのはなぜ？（04 リトルの法則）

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

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 右ぞろえの文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- 速さの表示：整数なら小数点なし
num :: Float -> String
num v = if v == fromIntegral (round v :: Int) then show (round v :: Int) else show v

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
