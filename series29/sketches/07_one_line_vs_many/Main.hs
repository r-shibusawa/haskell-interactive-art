-- ============================================
-- 07 一列 vs 複数列
-- 窓口 4 つ。左：全員が 1 本の列に並び、空いた窓口へ（フォーク型・銀行や空港）
--            右：窓口ごとに列があり、来た客はいちばん短い列を選ぶ。途中で列は変えない（スーパーのレジ）
--   同じ客（同じ到着時刻・同じサービス時間）を両方に流して比べる。平均はあまり変わらないのに「ばらつき」が大きくちがう
-- ←→ = 到着率 λ / ↑↓ = 時間の速さ / C = 右の列の選び方（短い列 ↔ でたらめ）/ R = 測り直す
-- ============================================
module Main (main) where

import Art
import Data.Array.Unboxed (UArray, accumArray, elems)
import Data.Foldable (foldl', toList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

nDesk :: Int
nDesk = 4         -- ★ 窓口の数

mu :: Double
mu = 1            -- ★ 1 窓口のサービス率

data Cust = Cust { cArr, cS, cR :: !Double }   -- 到着時刻、サービス時間、列を選ぶ乱数

-- 片方の仕組み：窓口の終了時刻と待ち時間の記録。一列なら sQ、複数列なら sQs を使う
data Side = Side
  { sDesk :: ![Maybe Double]
  , sQ :: !(Seq Cust)
  , sQs :: ![Seq Cust]
  , sWaits :: !(Seq Float)
  }

data World = World
  { wLambda :: !Double     -- ★ 到着率（ρ = λ / (4μ)）
  , wSpeed :: !Float       -- ★ 1 秒で進む時間
  , wShortest :: !Bool
  , wT :: !Double
  , wNext :: !Cust
  , wA, wB :: !Side
  }

main :: IO ()
main = runSketch (sketch "29-07 One Line vs Many Lines" world0)
  { skBack  = hsb 260 25 12
  , skStart = \_ -> resetAll world0
  , skStep  = \_ _ w -> advance (wT w + realToFrac (wSpeed w) / 60) w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 3.4 6 True 0 (Cust 0 1 0) sideA sideB

sideA, sideB :: Side
sideA = Side (replicate nDesk Nothing) Seq.empty [] Seq.empty                        -- 一列
sideB = Side (replicate nDesk Nothing) Seq.empty (replicate nDesk Seq.empty) Seq.empty   -- 複数列

newCustomer :: Double -> IO Cust
newCustomer t = do
  s <- expRand mu
  r <- randomRIO (0, 1)
  pure (Cust t s r)

resetAll :: World -> IO World
resetAll w = do
  a <- expRand (wLambda w)
  c <- newCustomer a
  pure w { wT = 0, wNext = c, wA = sideA, wB = sideB }

-- いちばん早く終わる窓口（なければ無限大）
firstDone :: Side -> (Double, Int)
firstDone s = minimum ((infinity, -1) : [ (e, i) | (i, Just e) <- zip [0 ..] (sDesk s) ])

-- 窓口 i で客 p のサービスを時刻 t に始める
start :: Int -> Cust -> Double -> Side -> Side
start i p t s = s { sWaits = sWaits s |> fl (t - cArr p), sDesk = setAt i (Just (t + cS p)) (sDesk s) }

-- 一列：時刻 t までのサービス終了を処理
runA :: Double -> Side -> Side
runA t s
  | tD > t = s
  | otherwise =
      let s1 = s { sDesk = setAt di Nothing (sDesk s) }
      in runA t $ case viewl (sQ s1) of
           p :< rest -> start di p tD s1 { sQ = rest }
           EmptyL -> s1
  where (tD, di) = firstDone s

arriveA :: Cust -> Side -> Side
arriveA p s = case [ i | (i, Nothing) <- zip [0 ..] (sDesk s) ] of
  (free : _) -> start free p (cArr p) s
  [] -> s { sQ = sQ s |> p }

-- 複数列
runB :: Double -> Side -> Side
runB t s
  | tD > t = s
  | otherwise =
      let s1 = s { sDesk = setAt di Nothing (sDesk s) }
      in runB t $ case drop di (sQs s1) of
           (q : _) | p :< rest <- viewl q -> start di p tD s1 { sQs = setAt di rest (sQs s1) }
           _ -> s1
  where (tD, di) = firstDone s

arriveB :: Bool -> Cust -> Side -> Side
arriveB shortest p s
  | free = start pick p (cArr p) s
  | otherwise = s { sQs = [ if i == pick then q |> p else q | (i, q) <- zip [0 ..] (sQs s) ] }
  where
    -- ★ 人数（サービス中を含む）がいちばん少ない列。同じなら左から
    sizes = [ (Seq.length q + maybe 0 (const 1) d, i) | (i, q, d) <- zip3 [0 :: Int ..] (sQs s) (sDesk s) ]
    pick | shortest = snd (minimum ((maxBound, 0) : sizes))
         | otherwise = floor (cR p * fromIntegral nDesk)
    free = case drop pick (sDesk s) of
      (Nothing : _) -> True
      _ -> False

advance :: Double -> World -> IO World
advance tEnd w
  | cArr c <= tEnd = do
      let a1 = arriveA c (runA (cArr c) (wA w))
          b1 = arriveB (wShortest w) c (runB (cArr c) (wB w))
          a2 = if Seq.length (sQ a1) > 400 then a1 { sQ = dropLast (sQ a1) } else a1
      g <- expRand (wLambda w)
      n <- newCustomer (cArr c + g)
      advance tEnd w { wA = a2, wB = b1, wNext = n }
  | otherwise = pure w { wA = trim (runA tEnd (wA w)), wB = trim (runB tEnd (wB w)), wT = tEnd }
  where
    c = wNext w
    trim s = if Seq.length (sWaits s) > 20000 then s { sWaits = Seq.drop 10000 (sWaits s) } else s
    dropLast q = case viewr q of
      rest :> _ -> rest
      EmptyR -> q

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawPanel (wA w) 0 hw "One line (fork)" 190
  , drawPanel (wB w) hw hw ("One line per desk (" ++ (if wShortest w then "pick the shortest" else "pick at random") ++ ")") 330
  , txt 20 40 18 (gray 1) ("lambda = " ++ nf 1 (wLambda w) ++ " (<- ->)  rho = " ++ nf 2 (wLambda w / (fromIntegral nDesk * mu)) ++ "  Speed x" ++ num (wSpeed w)
      ++ " (Up/Down)  C: how to pick  R: again  Time " ++ nf 0 (wT w))
  , txt 20 68 15 (hsb 0 0 60) "On the right, 'the next line was faster' happens. One line keeps first-come first-served: smaller max and spread."
  ]
  where hw = width / 2

-- 平均、標準偏差、最大、3 分以上の割合
stats :: Seq Float -> (Float, Float, Float, Float)
stats ws =
  let n = fromIntegral (max (Seq.length ws) 1) :: Float
      (s, mx, over) = foldl' (\(!a, !b, !c) x -> (a + x, max b x, if x > 3 then c + 1 else c)) (0, 0, 0 :: Int) ws
      m = s / n
      v = foldl' (\a x -> a + (x - m) * (x - m)) 0 ws
  in (m, sqrt (v / n), mx, fromIntegral over / n)

drawPanel :: Side -> Float -> Float -> String -> Float -> Picture
drawPanel s x0 w title hue = Pictures
  ( txt (x0 + 30) (top - 6) 18 (hsb hue 60 100) title
  : concat [ box dx y 44 (dh - 8) (hsb 200 50 (maybe 30 (const 75) d))
             : [ dot (dx - 14) (y + dh / 2 - 4) 16 (hsb 140 70 95) | Just _ <- [d] ]
           | (i, d) <- zip [0 :: Int ..] (sDesk s), let y = rowY i ]
 ++ concat [ [ dot (dx - 36 - fromIntegral k * 14) (rowY i + dh / 2 - 4) 11 (hsb 30 75 100) | k <- [0 .. min (Seq.length q) 25 - 1] ]
           | (i, q) <- zip [0 :: Int ..] (sQs s) ]
    -- 一列はジグザグに並べる
 ++ [ let col = k `div` 6
          row = if even col then k `mod` 6 else 5 - (k `mod` 6)
      in dot (cx - fromIntegral col * 18) (cy - 40 + fromIntegral row * 16) 11 (hsb 30 75 100)
    | k <- [0 .. min (Seq.length (sQ s)) 60 - 1] ]
    -- 待ち時間のヒストグラム
 ++ [ seg 1 (hsba 0 0 100 0.3) (gx, gy) (gx + gw, gy) ]
 ++ [ let h = fromIntegral b / peak * gh
      in box (gx + fromIntegral i * gw / fromIntegral nb + 1) (gy - h) (gw / fromIntegral nb - 2) h (hsba hue 60 90 0.8)
    | (i, b) <- zip [0 :: Int ..] (elems bins) ]
 ++ [ txt gx (gy + 18) 14 (hsb 0 0 60) "0"
    , txtR (gx + gw) (gy + 18) 14 (hsb 0 0 60) (show (round xmax :: Int) ++ "+ min")
    , txt gx (gy - gh - 6) 14 (hsb 0 0 60) "Histogram of waits"
    , txt gx ty 16 (gray 1) ("Mean wait  " ++ nf 3 m ++ " min")
    , txt gx (ty + 24) 16 (gray 1) ("Std dev  " ++ nf 3 sd ++ " min")
    , txt gx (ty + 48) 16 (gray 1) ("Max  " ++ nf 2 mx ++ " min   Waited 3+ min  " ++ nf 1 (over * 100) ++ "%")
    , txt gx (ty + 72) 16 (hsb 0 0 60) ("Served " ++ show (Seq.length (sWaits s))) ] )
  where
    top = 110
    dh = 46
    dx = x0 + w - 110
    rowY i = top + 16 + fromIntegral i * dh
    cx = x0 + w - 150
    cy = top + 16 + fromIntegral nDesk * dh / 2 - 4
    (m, sd, mx, over) = stats (sWaits s)
    gx = x0 + 40
    gy = height - 60
    gw = w - 80
    gh = height - top - fromIntegral nDesk * dh - 260
    nb = 30 :: Int
    xmax = 6 :: Float
    bins :: UArray Int Int
    bins = accumArray (+) 0 (0, nb - 1) [ (min (floor (v / xmax * fromIntegral nb)) (nb - 1), 1) | v <- toList (sWaits s) ]
    peak = fromIntegral (maximum (1 : elems bins)) :: Float
    ty = gy - gh - 110

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetAll w { wLambda = min 3.9 (wLambda w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetAll w { wLambda = max 0.4 (wLambda w - 0.1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 400 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0.5 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> resetAll w { wShortest = not (wShortest w) }
    | c `elem` "rR" -> resetAll w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ↑ で速くして 1 万人分集める。平均・標準偏差・最大を左右で比べる。どちらが「公平」？
-- 2. C で「でたらめに選ぶ」に。窓口 4 つの M/M/1 が 4 つ並んだのと同じ → 平均の待ち時間も大きく悪化する
-- 3. λ を 2 にすると差は縮む？ 3.8 だと？ 混んでいるときほど一列の良さが効く
-- 4. 右に「列の乗り換え（ジョッキー）」を加える：サービス終了で列が空いたら、他の列の最後尾の人を移す

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

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]
