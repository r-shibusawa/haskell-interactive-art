-- ============================================
-- 04 リトルの法則 L = λW
-- どんな待ち行列でも「平均の系内人数 L = 到着率 λ × 平均の滞在時間 W」が成り立つ
--   窓口の数・サービス時間の分布・並び方に関係なし。下の「滞在の帯」の面積 = N(t) のグラフの面積 だから
--   毎回ちがう待ち行列をつくって L と λW を測り、右の散布図に打っていく
-- N = 新しい待ち行列 / F = 20 回まとめて測る / ↑↓ = 時間の速さ / R = 散布図を消す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (foldl', toList)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

runT, showSpan :: Double
runT = 300        -- ★ 1 回の測定の長さ
showSpan = 30     -- 下の帯に表示する時間

distNames :: [String]
distNames = ["Exponential", "Constant", "Uniform"]

-- ★ でたらめにつくった待ち行列の設定
data Cfg = Cfg { cC :: !Int, cDist :: !Int, cMu, cLam :: !Double }

-- 窓口でサービス中の客
data Busy = Busy { bId :: !Int, bArr, bEnd :: !Double }

-- 一人ひとりの滞在（到着 → 退出、dep < 0 はまだ店内）
data Stay = Stay { sArr, sDep :: !Double }

data Sim = Sim
  { sT, sNextArr, sArea, sSumW :: !Double
  , sDesk :: ![Maybe Busy]
  , sQ :: !(Seq (Int, Double))     -- 並んでいる客（番号, 到着時刻）
  , sNArr, sNDone :: !Int
  , sStays :: !(IM.IntMap Stay)    -- 番号の順に並ぶ
  , sRec :: !(Seq (Double, Int))   -- (時刻, 人数)
  }

data Point3 = Point3 !Float !Float !Float   -- L, λW, 色相

data World = World
  { wCfg :: !Cfg
  , wSim :: !Sim
  , wPts :: !(Seq Point3)
  , wSpeed :: !Float       -- ★ 1 秒で進む時間
  , wPending :: !Int       -- F で残っている測定の回数
  }

main :: IO ()
main = runSketch (sketch "29-04 Little's Law" (World cfg0 (emptySim 1 0) Seq.empty 30 0))
  { skBack  = hsb 280 20 12
  , skStart = \_ -> newRun (World cfg0 (emptySim 1 0) Seq.empty 30 0)
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

cfg0 :: Cfg
cfg0 = Cfg 1 0 1 0.5

emptySim :: Int -> Double -> Sim
emptySim c next = Sim 0 next 0 0 (replicate c Nothing) Seq.empty 0 0 IM.empty Seq.empty

-- ★ でたらめな待ち行列を 1 つつくる
newRun :: World -> IO World
newRun w = do
  c <- randomRIO (1, 3)
  rho <- randomRIO (0.3, 0.92)
  dist <- randomRIO (0, 2)
  m <- randomRIO (0.5, 2)
  let lam = rho * fromIntegral c * m
  next <- expRand lam
  pure w { wCfg = Cfg c dist m lam, wSim = emptySim c next }

serviceTime :: Cfg -> IO Double
serviceTime cfg = case cDist cfg of
  0 -> expRand (cMu cfg)
  1 -> pure (1 / cMu cfg)
  _ -> randomRIO (0, 2 / cMu cfg)

nIn :: Sim -> Int
nIn s = Seq.length (sQ s) + length [() | Just _ <- sDesk s]

advance :: Cfg -> Double -> Sim -> IO Sim
advance cfg tEnd s
  | tn > tEnd = do
      let s1 = s { sArea = sArea s + n * (tEnd - sT s), sT = tEnd }
      pure s1 { sStays = trimStays tEnd (sStays s1) }
  | sNextArr s <= tD = do                     -- 到着
      a <- expRand (cLam cfg)
      let k = sNArr s
          s1 = moved { sQ = sQ s |> (k, tn), sStays = IM.insert k (Stay tn (-1)) (sStays s)
                     , sNArr = k + 1, sNextArr = tn + a }
      startDesks cfg s1 >>= advance cfg tEnd
  | otherwise = case drop di (sDesk s) of     -- サービス終了
      (Just b : _) -> do
        let s1 = moved { sStays = IM.adjust (\y -> y { sDep = tn }) (bId b) (sStays s)
                       , sDesk = setAt di Nothing (sDesk s), sNDone = sNDone s + 1
                       , sSumW = sSumW s + tn - bArr b }
        startDesks cfg s1 >>= advance cfg tEnd
      _ -> advance cfg tEnd moved
  where
    (tD, di) = minimum ((infinity, -1) : [ (bEnd b, i) | (i, Just b) <- zip [0 ..] (sDesk s) ])
    tn = min (sNextArr s) tD
    n = fromIntegral (nIn s)
    moved = s { sArea = sArea s + n * (tn - sT s), sT = tn }

-- 空いた窓口に先頭の客を入れる
startDesks :: Cfg -> Sim -> IO Sim
startDesks cfg = go 0
  where
    go i s
      | i >= cC cfg = pure s
      | otherwise = case (drop i (sDesk s), viewl (sQ s)) of
          (Nothing : _, (k, arr) :< rest) -> do
            st <- serviceTime cfg
            go (i + 1) s { sQ = rest, sDesk = setAt i (Just (Busy k arr (sT s + st))) (sDesk s) }
          _ -> go (i + 1) s

-- 表示の範囲より前に帰った人の帯を捨てる
trimStays :: Double -> IM.IntMap Stay -> IM.IntMap Stay
trimStays t m = case IM.lookupMin m of
  Just (k, y) | sDep y >= 0 && sDep y < t - showSpan -> trimStays t (IM.delete k m)
  _ -> m

finishRun :: World -> IO World
finishRun w = do
  let s = wSim w
      l = sArea s / sT s
      lamHat = fromIntegral (sNArr s) / sT s
      wbar = sSumW s / fromIntegral (max (sNDone s) 1)
      hue = case cDist (wCfg w) of { 0 -> 190; 1 -> 45; _ -> 320 }
      pts = wPts w |> Point3 (fl l) (fl (lamHat * wbar)) hue
  newRun w { wPts = if Seq.length pts > 400 then Seq.drop 1 pts else pts }

stepWorld :: World -> IO World
stepWorld w0 = do
  -- F で頼まれた測定：1 フレームに 1 回ずつ終わらせる
  w1 <- if wPending w0 > 0
          then do
            s <- advance (wCfg w0) runT (wSim w0)
            finishRun w0 { wSim = s, wPending = wPending w0 - 1 }
          else pure w0
  let s0 = wSim w1
  s1 <- advance (wCfg w1) (min runT (sT s0 + realToFrac (wSpeed w1) / 60)) s0
  let rec1 = Seq.dropWhileL (\(t, _) -> t < sT s1 - showSpan) (sRec s1 |> (sT s1, nIn s1))
      w2 = w1 { wSim = s1 { sRec = rec1 } }
  if sT s1 >= runT then finishRun w2 else pure w2

gx, gw, gy, gh :: Float
gx = 40
gw = width * 0.55
gy = height * 0.42
gh = height * 0.24

drawAll :: World -> Picture
drawAll w = Pictures [drawArea s, drawStays s, drawScatter w, drawInfo w, hud]
  where
    s = wSim w
    hud = Pictures
      [ txt 20 40 18 (gray 1) ("N: new queue  F: measure 20 at once  Speed x" ++ num (wSpeed w) ++ " (Up/Down)  R: clear the plot")
      , txt 20 68 15 (hsb 0 0 60) "Every queue lands on the yellow diagonal L = lambda W. Hard-to-measure W follows from easy L and lambda." ]

xOf :: Sim -> Double -> Float
xOf s t = gx + fl ((t - (sT s - showSpan)) / showSpan) * gw

-- 左上：N(t)（階段の形は凸でないので、細い棒を並べて塗る）
drawArea :: Sim -> Picture
drawArea s = Pictures
  ( seg 1 (hsba 0 0 100 0.3) (gx, gy) (gx + gw, gy)
  : [ let h = fromIntegral n / top * gh in box (xOf s a) (gy - h) (xOf s b - xOf s a + 0.5) h (hsba 30 80 100 0.5)
    | ((a, n), b) <- zip recs (drop 1 (map fst recs) ++ [sT s]) ]
 ++ [ txt gx (gy - gh - 8) 14 (hsb 0 0 60) "People in system N(t). Orange area = people x time" ] )
  where
    recs = toList (sRec s)
    top = fromIntegral (maximum (4 : map snd recs)) :: Float

-- 左下：一人ひとりの滞在の帯（到着 → 退出）
drawStays :: Sim -> Picture
drawStays s = Pictures
  ( [ let a = max (sArr p) (sT s - showSpan)
          e = if sDep p >= 0 then sDep p else sT s
          y = by + wrapF bh (fromIntegral i * rowH)
      in box (xOf s a) y (max 1 (xOf s e - xOf s a)) (rowH - 1)
             (if sDep p >= 0 then hsb 190 60 90 else hsb 30 80 100)
    | (i, p) <- zip [0 :: Int ..] (IM.elems (sStays s)) ]
 ++ [ txt gx (by - 6) 14 (hsb 0 0 60) "Each person's stay (bar length = W). Total bar length = area above" ] )
  where
    by = gy + 30
    bh = height - by - 40
    rowH = max 3 (min 10 (bh / fromIntegral (max (IM.size (sStays s)) 1)))

-- 右：散布図 L vs λW
drawScatter :: World -> Picture
drawScatter w = Pictures
  ( [ seg 1 ax (px, py) (px + ps, py), seg 1 ax (px, py) (px, py - ps)
    , seg 2 (hsba 45 90 100 0.7) (px, py) (px + ps, py - ps) ]
 ++ [ dot (px + min lw vmax / vmax * ps) (py - min l vmax / vmax * ps) 9 (hsba hue 70 100 0.8) | Point3 l lw hue <- toList (wPts w) ]
 ++ [ txt px (py - ps - 10) 14 (hsb 0 0 60) "x = lambda W, y = L (mean in system)"
    , txt (px - 4) (py + 18) 14 (hsb 0 0 60) "0"
    , txtR (px + ps) (py + 18) 14 (hsb 0 0 60) (show (round vmax :: Int))
    , txt px (py + 36) 14 (hsb 0 0 60) "Service: cyan exp, yellow const, pink uniform" ] )
  where
    ax = hsba 0 0 100 0.4
    px = width * 0.64
    py = height - 70
    ps = min (width * 0.32) (height - 320)
    vmax = 10 :: Float

-- 今の待ち行列
drawInfo :: World -> Picture
drawInfo w = Pictures
  [ txt tx ty 16 (gray 1) ("Now: " ++ show (cC cfg) ++ " desks, " ++ concat (take 1 (drop (cDist cfg) distNames)) ++ ", mean " ++ nf 2 (1 / cMu cfg))
  , txt tx (ty + 26) 16 (gray 1) ("Run " ++ nf 0 (sT s) ++ " / " ++ nf 0 runT)
  , txt tx (ty + 52) 16 (hsb 45 90 100) ("L = " ++ nf 3 l ++ "   lambda = " ++ nf 3 lamHat ++ "   W = " ++ nf 3 wbar)
  , txt tx (ty + 78) 16 (hsb 45 90 100) ("lambda W = " ++ nf 3 (lamHat * wbar))
  , txt tx (ty + 104) 16 (gray 1) ("Queues measured: " ++ show (Seq.length (wPts w)))
  ]
  where
    cfg = wCfg w
    s = wSim w
    tx = width * 0.64
    ty = 120
    l = sArea s / max (sT s) 1e-9
    lamHat = fromIntegral (sNArr s) / max (sT s) 1e-9
    wbar = sSumW s / fromIntegral (max (sNDone s) 1)

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 600 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 2 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> newRun w
    | c `elem` "fF" -> pure w { wPending = wPending w + 20 }
    | c `elem` "rR" -> pure w { wPts = Seq.empty }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. F を何回か押す。点が対角線から外れるのはどんなとき？（runT を 50 にすると？）
-- 2. ラーメン屋に平均 6 人並んでいて、1 分に 2 人来る。平均何分待つ？（W = L/λ）
-- 3. serviceTime の分布を変えても対角線から外れないことを確かめる（例：一様乱数 2 つのかけ算）
-- 4. 大学の在籍者 4000 人、毎年 1000 人入学。平均の在籍年数は？ リトルの法則は「流れ」があればどこでも使える

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
