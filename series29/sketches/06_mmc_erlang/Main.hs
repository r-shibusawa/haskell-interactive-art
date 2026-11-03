-- ============================================
-- 06 M/M/c と M/M/c/K ─ アーランの公式
-- 窓口が c 個。全部ふさがっていたら並ぶ（M/M/c）か、定員 K を超えたら断られる（M/M/c/K）
--   アーラン C 式：来た客が待たされる確率　　アーラン B 式（K = c）：来た客が断られる確率（電話回線の設計で生まれた）
--   右の表で「待たされる確率 20% 以下にするには何人いればいい？」を計画する
-- ←→ = 到着率 λ / ↑↓ = 窓口の数 c / K = 定員（∞ → c → c+5）/ スペース = 一時停止 / R = 測り直す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

mu, target :: Double
mu = 1            -- ★ 1 窓口のサービス率
target = 0.2      -- ★ 目標：待たされる（または断られる）確率をこれ以下に

speed :: Float
speed = 8         -- ★ 1 秒で進む時間

kNames :: [String]
kNames = ["inf", "c", "c+5"]

-- 断られた客のアニメーション
data Lost = Lost !Float !Float !Float   -- x, y, vy

data World = World
  { wLambda :: !Double       -- ★ 到着率（1 分あたり）
  , wC, wKm :: !Int          -- 窓口の数、定員のモード
  , wPaused :: !Bool
  , wT, wNextArr, wSumWq :: !Double
  , wDesk :: ![Double]       -- 窓口ごとの終了時刻（空きは −1）
  , wQ :: !(Seq Double)      -- 並んでいる客の到着時刻
  , wNArr, wNWait, wNLost, wNStart :: !Int
  , wLost :: ![Lost]
  }

main :: IO ()
main = runSketch (sketch "29-06 M/M/c and Erlang Formulas" world0)
  { skBack  = hsb 10 25 12
  , skStart = \_ -> resetSim world0
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 4 5 0 False 0 0 0 [] Seq.empty 0 0 0 0 []

-- 定員（Nothing = 無限）
capK :: Int -> Int -> Maybe Int
capK km c = case km of
  0 -> Nothing
  1 -> Just c
  _ -> Just (c + 5)

resetSim :: World -> IO World
resetSim w = do
  n <- expRand (wLambda w)
  pure w { wT = 0, wNextArr = n, wDesk = replicate (wC w) (-1), wQ = Seq.empty, wNArr = 0
         , wNWait = 0, wNLost = 0, wSumWq = 0, wNStart = 0, wLost = [] }

busy :: World -> Int
busy w = length (filter (>= 0) (wDesk w))

advance :: Double -> World -> IO World
advance tEnd w
  | tn > tEnd = pure w { wT = tEnd }
  | wNextArr w <= tD = do                     -- 到着
      a <- expRand (wLambda w)
      let w1 = w { wT = tn, wNextArr = tn + a, wNArr = wNArr w + 1 }
          full = maybe False (\k -> busy w1 + Seq.length (wQ w1) >= k) (capK (wKm w) (wC w)) || Seq.length (wQ w1) > 500
          w2 | full = w1 { wNLost = wNLost w1 + 1
                         , wLost = if length (wLost w1) < 40 then Lost (width * 0.3) 200 (-2) : wLost w1 else wLost w1 }
             | otherwise = w1 { wNWait = wNWait w1 + (if busy w1 == wC w1 then 1 else 0), wQ = wQ w1 |> tn }
      fillDesks w2 >>= advance tEnd
  | otherwise = fillDesks w { wT = tn, wDesk = setAt di (-1) (wDesk w) } >>= advance tEnd   -- サービス終了
  where
    (tD, di) = minimum ((infinity, -1) : [ (d, i) | (i, d) <- zip [0 ..] (wDesk w), d >= 0 ])
    tn = min (wNextArr w) tD

fillDesks :: World -> IO World
fillDesks w0 = go 0 w0
  where
    go i w
      | i >= wC w = pure w
      | otherwise = case (drop i (wDesk w), viewl (wQ w)) of
          (d : _, arr :< rest) | d < 0 -> do
            s <- expRand mu
            go (i + 1) w { wSumWq = wSumWq w + wT w - arr, wQ = rest, wNStart = wNStart w + 1
                         , wDesk = setAt i (wT w + s) (wDesk w) }
          _ -> go (i + 1) w

-- 理論の結果：待たされる確率、断られる確率、平均の待ち時間
data Theory = Theory { thPw, thLoss, thWq :: !Double }

-- ★ 理論値：人数 n の定常確率から計算（K = ∞ はアーラン C 式）
theory :: Int -> Double -> Maybe Int -> Theory
theory cc lam mk =
  let a = lam / mu
      fc = fromIntegral cc
      r = a / fc
  in case mk of
    Nothing
      | r >= 1 -> Theory 1 0 infinity
      | otherwise ->
          let (s, term) = foldl' (\(!s0, !t0) k -> (s0 + t0, t0 * a / fromIntegral (k + 1))) (0, 1) [0 .. cc - 1]   -- 終わると term = a^c / c!
              top = term / (1 - r)
              erl = top / (s + top)
          in Theory erl 0 (erl / (fc * mu - lam))
    Just kk ->
      let ps = scanl (\p n -> p * a / fromIntegral (min n cc)) 1 [1 .. kk]
          tot = sum ps
          indexed = zip [0 :: Int ..] ps
          loss = foldl' (\_ p -> p) 1 ps / tot
          pw = sum [ p / tot | (n, p) <- indexed, n >= cc, n < kk ]
          lq = sum [ fromIntegral (n - cc) * p / tot | (n, p) <- indexed, n >= cc + 1 ]
      in Theory (pw / (1 - loss)) loss (lq / (lam * (1 - loss)))

stepWorld :: World -> IO World
stepWorld w0 = do
  w1 <- if wPaused w0 then pure w0 else advance (wT w0 + realToFrac speed / 60) w0
  let move (Lost x y vy) = Lost (x - 2) (y + vy) (vy + 0.15)
      inside (Lost _ y _) = y < height
  pure w1 { wLost = filter inside (map move (wLost w1)) }

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawDesks w
  , drawNumbers w
  , drawPlan w
  , txt 20 40 18 (gray 1) ("lambda = " ++ nf 1 (wLambda w) ++ " (<- ->)  Desks c = " ++ show (wC w) ++ " (Up/Down)  Cap " ++ kName w
      ++ " (K)  Speed x" ++ num speed ++ "  Space: pause  R: again")
  , txt 20 68 15 (hsb 0 0 60) "At equal rho, more desks wait less (economy of scale). With cap c the line vanishes, and people are refused (Erlang B)."
  ]

kName :: World -> String
kName w = concat (take 1 (drop (wKm w) kNames))

-- 窓口と行列
drawDesks :: World -> Picture
drawDesks w = Pictures
  ( concat [ box dx y 50 (dh - 6) (hsb 200 50 (if d >= 0 then 75 else 30))
             : [ dot (dx - 14) (y + dh / 2 - 3) (min 18 (dh - 10)) (hsb 140 70 95) | d >= 0 ]
           | (i, d) <- zip [0 :: Int ..] (wDesk w), let y = top + fromIntegral i * dh ]
 ++ [ dot (dx - 44 - fromIntegral i * 14) qy 11 (hsb 30 75 100) | i <- [0 .. min n maxDots - 1] ]
 ++ [ txt 30 (qy + 24) 14 (hsb 0 80 100) ("+" ++ show (n - maxDots)) | n > maxDots ]
 ++ concat [ let kx = dx - 44 - fromIntegral (k - wC w) * 14 + 7
             in [ seg 2 (hsb 0 70 100) (kx, qy - 16) (kx, qy + 16), txtC kx (qy - 20) 14 (hsb 0 70 100) "Cap" ]
           | Just k <- [capK (wKm w) (wC w)] ]
 ++ [ dot x y 10 (hsba 0 80 100 0.7) | Lost x y _ <- wLost w ] )
  where
    dx = width * 0.36
    top = 120
    c = wC w
    dh = min 44 ((height * 0.5) / fromIntegral c)
    qy = top + (fromIntegral c * dh) / 2
    n = Seq.length (wQ w)
    maxDots = 29   -- 1280 の画面に収まる点の数

-- 測定と理論
drawNumbers :: World -> Picture
drawNumbers w = Pictures
  ( txt tx ty 17 (gray 1) ("a = lambda/mu = " ++ nf 1 a ++ "  rho = a/c = " ++ nf 2 (a / fromIntegral (wC w)) ++ "  K = " ++ maybe "inf" show cap)
  : concat [ [ txt (tx + cx) (ty + 32 + fromIntegral i * 26) 16 (cellColor i j) s | (j, cx, s) <- zip3 [0 :: Int ..] [0, 190, 340] row ]
           | (i, row) <- zip [0 :: Int ..] rows ]
 ++ [ txt tx (ty + 32 + 4 * 26 + 4) 14 (hsb 0 0 60) ("Time " ++ nf 0 (wT w) ++ " min  Arrived " ++ show (wNArr w)) ] )
  where
    tx = 30
    ty = height * 0.68
    a = wLambda w / mu
    cap = capK (wKm w) (wC w)
    th = theory (wC w) (wLambda w) cap
    mPw = fromIntegral (wNWait w) / fromIntegral (max (wNArr w - wNLost w) 1) :: Double
    mLoss = fromIntegral (wNLost w) / fromIntegral (max (wNArr w) 1) :: Double
    mWq = wSumWq w / fromIntegral (max (wNStart w) 1)
    rows = [ ["", "Simulation", "Theory"]
           , ["Chance of waiting", nf 3 mPw, nf 3 (thPw th)]
           , ["Chance of refusal", nf 3 mLoss, nf 3 (thLoss th)]
           , ["Mean wait Wq", nf 3 mWq, if isInfinite (thWq th) then "inf (unstable)" else nf 3 (thWq th)] ]
    cellColor :: Int -> Int -> Color
    cellColor i j | i == 0 = hsb 0 0 60
                  | j == 2 = hsb 45 90 100
                  | otherwise = gray 1

-- 右：何人いればいい？（人員計画の表）
drawPlan :: World -> Picture
drawPlan w = Pictures
  ( txt px (py - 14) 17 (gray 1) ("Staffing plan: lambda = " ++ nf 1 (wLambda w) ++ ", cap " ++ kName w)
  : txt px (py + 10) 14 (hsb 0 0 60) "Desks c"
  : txt (px + 80) (py + 10) 14 (hsb 0 0 60) (if wKm w == 1 then "Chance of refusal" else "Chance of waiting")
  : txt (px + 340) (py + 10) 14 (hsb 0 0 60) "Mean wait"
  : concatMap row table
 ++ [ txt px (py + 14 + 16 * rh + 8) 15 (hsb 140 70 100)
        (case best of
           Just b -> "Fewest desks for " ++ show (round (target * 100) :: Int) ++ "% or less = " ++ show b
           Nothing -> "Even 15 desks miss the target") ] )
  where
    px = width * 0.58
    py = 120
    rh = min 30 ((height - 200) / 16)
    table = [ (cc, theory cc (wLambda w) (capK (wKm w) cc)) | cc <- [1 .. 15] ]
    valueOf t = if wKm w == 1 then thLoss t else thPw t
    best = case [ cc | (cc, t) <- table, valueOf t <= target, not (isInfinite (thWq t)) ] of
      (b : _) -> Just b
      [] -> Nothing
    row :: (Int, Theory) -> [Picture]
    row (cc, t) =
      let v = fl (valueOf t)
          y = py + 14 + fromIntegral cc * rh
      in [ box (px - 6) (y - rh + 6) (min 520 (width - px - 10)) (rh - 2) (if cc == wC w then hsb 45 60 40 else hsba 0 0 100 0.04)
         , txt (px + 10) y 14 (if Just cc == best then hsb 140 70 100 else gray 1) (show cc)
         , box (px + 80) (y - rh + 10) (v * 170) (rh - 10) (if valueOf t <= target then hsb 140 60 80 else hsb 0 70 85)
         , txt (px + 80 + v * 170 + 6) y 14 (gray 1) (nf 3 v)
         , txt (px + 340) y 14 (gray 1) (if isInfinite (thWq t) then "inf" else nf 3 (thWq t)) ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim w { wLambda = min 14 (wLambda w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim w { wLambda = max 0.5 (wLambda w - 0.5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> resetSim w { wC = min 15 (wC w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetSim w { wC = max 1 (wC w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "kK" -> resetSim w { wKm = (wKm w + 1) `mod` 3 }
    | c `elem` "rR" -> resetSim w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. λ = 4 で c を 4 → 5 → 6 と増やす。窓口 1 つ増やすだけで待ち時間は何分の 1 になる？
-- 2. λ = 1.5, c = 2 と λ = 11.5, c = 15（どちらも ρ は 0.75 くらい）で待たされる確率を比べる（λ と c を一緒に変える）
-- 3. K を押して定員 c（アーラン B）に。コールセンターの回線数を決めるとき、断られる確率 1% にするには？（target = 0.01）
-- 4. 平方根の法則：必要な窓口 ≒ a + β√a。表の答えと a + √a を比べてみる（12 で使う）

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

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

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

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]
