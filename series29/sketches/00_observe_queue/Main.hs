-- ============================================
-- 00 行列を観察する
-- お店に客がランダムにやってきて、窓口で 1 人ずつサービスを受けて帰っていく
--   行列の長さと待ち時間は「平均の到着の速さ λ」と「1 窓口の平均のサービスの速さ μ」で決まる。まずはじっくり眺めよう
-- ↑↓ = 時間の速さ / ←→ = 到着の速さ λ / 1・2・3 = 窓口の数 / スペース = 一時停止 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

mu :: Double
mu = 1.0          -- ★ 1 つの窓口が 1 分あたりにさばく平均人数

maxLine :: Int
maxLine = 120     -- ★ これ以上並んでいたら入店をあきらめる

data Stage = Waiting | Serving | Finished deriving Eq

-- 画面に出ている客
data Person = Person
  { pArr, pStart, pEnd :: !Double   -- 到着・サービス開始・サービス終了の時刻
  , pStage :: !Stage
  , pDesk :: !Int
  , pX, pY :: !Float                -- いま描いている位置
  }

data World = World
  { wLambda :: !Double              -- ★ 1 分あたりの平均到着人数
  , wDesks :: !Int                  -- 窓口の数
  , wSpeed :: !Float                -- ★ シミュレーションの速さ（1 秒で何分進むか）
  , wPaused :: !Bool
  , wT, wNextArr :: !Double
  , wPeople :: !(IM.IntMap Person)  -- 番号 → 客
  , wNextId :: !Int
  , wDesk :: ![Maybe Int]           -- 窓口ごとにサービス中の客の番号
  , wLine :: !(Seq Int)             -- 並んでいる客（先頭が左）
  , wServed, wMaxQ, wBalked :: !Int
  , wSumWait, wArea :: !Double
  , wLenRec :: !(Seq Int)
  }

main :: IO ()
main = runSketch (sketch "29-00 Watching a Queue" world0)
  { skBack  = hsb 30 20 13
  , skStart = \_ -> resetAll world0
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 0.8 1 2 False 0 0 IM.empty 0 [Nothing] Seq.empty 0 0 0 0 0 Seq.empty

resetAll :: World -> IO World
resetAll w = do
  n <- expRand (wLambda w)
  pure w { wT = 0, wNextArr = n, wPeople = IM.empty, wLine = Seq.empty
         , wDesk = replicate (wDesks w) Nothing, wServed = 0, wSumWait = 0
         , wMaxQ = 0, wBalked = 0, wArea = 0, wLenRec = Seq.empty }

deskPos :: Int -> Int -> Point
deskPos nDesk i = (width * 0.66, height * 0.42 + (fromIntegral i - fromIntegral (nDesk - 1) / 2) * 90)

linePos :: Int -> Point
linePos i = let k = fromIntegral i
            in (width * 0.58 - k * 28, height * 0.42 + (if i > 0 then sin (k * 0.9) * 3 else 0))

nBusy, inSystem :: World -> Int
nBusy w = length [() | Just _ <- wDesk w]
inSystem w = Seq.length (wLine w) + nBusy w

-- ★ 1 つの事件（到着・サービス終了）ずつ時間を進める
advance :: Double -> World -> IO World
advance tEnd w
  | tn > tEnd = pure w { wArea = wArea w + fromIntegral (inSystem w) * (tEnd - wT w), wT = tEnd }
  | wNextArr w <= tDone = do                                  -- 到着
      next <- expRand (wLambda w)
      let w1 = moved { wNextArr = tn + next }
      if Seq.length (wLine w1) >= maxLine
        then advance tEnd w1 { wBalked = wBalked w1 + 1 }
        else do
          let k = wNextId w1
              p = Person tn (-1) (-1) Waiting 0 (-20) (height * 0.42)
          afterEvent w1 { wPeople = IM.insert k p (wPeople w1), wNextId = k + 1, wLine = wLine w1 |> k }
  | otherwise = afterEvent (finishDesk di moved)              -- サービス終了
  where
    busy = [ (pEnd p, i) | (i, Just k) <- zip [0 ..] (wDesk w), Just p <- [IM.lookup k (wPeople w)] ]
    (tDone, di) = minimum ((infinity, -1) : busy)
    tn = min (wNextArr w) tDone
    moved = w { wArea = wArea w + fromIntegral (inSystem w) * (tn - wT w), wT = tn }
    afterEvent w1 = do
      w2 <- fillDesks w1
      advance tEnd w2 { wMaxQ = max (wMaxQ w2) (Seq.length (wLine w2)) }

finishDesk :: Int -> World -> World
finishDesk di w = case drop di (wDesk w) of
  (Just k : _) -> w { wPeople = IM.adjust (\p -> p { pStage = Finished }) k (wPeople w)
                    , wDesk = setAt di Nothing (wDesk w), wServed = wServed w + 1 }
  _ -> w

-- 空いた窓口に先頭の客を入れる
fillDesks :: World -> IO World
fillDesks w0 = go 0 w0
  where
    go i w
      | i >= wDesks w = pure w
      | otherwise = case (drop i (wDesk w), viewl (wLine w)) of
          (Nothing : _, k :< rest) -> do
            s <- expRand mu
            let t = wT w
                arr = maybe t pArr (IM.lookup k (wPeople w))
                serve p = p { pStart = t, pEnd = t + s, pStage = Serving, pDesk = i }
            go (i + 1) w { wPeople = IM.adjust serve k (wPeople w), wLine = rest
                         , wDesk = setAt i (Just k) (wDesk w), wSumWait = wSumWait w + t - arr }
          _ -> go (i + 1) w

stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  w1 <- if wPaused w0 then pure w0 else advance (wT w0 + realToFrac (wSpeed w0) / 60) w0
  let rec1 | envFrame env `mod` 3 == 0 && not (wPaused w1) =
               let r = wLenRec w1 |> Seq.length (wLine w1)
               in if Seq.length r > round width - 80 then Seq.drop 1 r else r
           | otherwise = wLenRec w1
  pure (moveAll w1 { wLenRec = rec1 })

-- 客の目標位置を決めて、なめらかに動かす
moveAll :: World -> World
moveAll w = w { wPeople = IM.filter alive (IM.mapWithKey move (wPeople w)) }
  where
    slots = IM.fromList (zip (toList (wLine w)) [0 ..])
    target k p = case pStage p of
      Waiting -> linePos (IM.findWithDefault 0 k slots)
      Serving -> let (dx, dy) = deskPos (wDesks w) (pDesk p) in (dx - 22, dy)
      Finished -> (width + 40, height * 0.42)
    move k p = let (tx, ty) = target k p
               in p { pX = pX p + (tx - pX p) * 0.25, pY = pY p + (ty - pY p) * 0.25 }
    alive p = not (pStage p == Finished && pX p > width + 20)

drawAll :: World -> Picture
drawAll w = Pictures
  ( box 40 (height * 0.2) (width * 0.75) (height * 0.44) (hsb 30 15 20)   -- 店の床
  : concatMap drawDesk (zip [0 ..] (wDesk w))
 ++ map drawPerson (IM.elems (wPeople w))
 ++ [ seg 1 (hsba 0 0 100 0.2) (40, gy) (width - 40, gy)                  -- 行列の長さの時間変化
    , path 2 (hsb 30 80 100) [ (40 + fromIntegral i, gy - min (fromIntegral v * 6) gh)
                             | (i, v) <- zip [0 :: Int ..] (toList (wLenRec w)) ]
    , txt 44 (gy - gh - 6) 14 (hsb 0 0 70) "Length of the line over time" ]
 ++ zipWith panelRow [0 ..] rows
 ++ [ txt 20 40 18 (gray 1) ("lambda = " ++ nf 2 (wLambda w) ++ " (<- ->)  mu = " ++ num (fl mu) ++ "  Desks " ++ show (wDesks w)
        ++ " (1-3)  Speed x" ++ num (wSpeed w) ++ " (Up/Down)  " ++ (if wPaused w then "PAUSED" else "Space: pause") ++ "  R: reset")
    , txt 20 68 15 (hsb 0 0 60) "Green = just came, red = waited long. As lambda nears mu the line swells and shrinks: the theme of queueing." ]
  )
  where
    gy = height - 40
    gh = height * 0.22
    t = wT w
    rho = wLambda w / (fromIntegral (wDesks w) * mu)
    drawDesk (i, cur) =
      let (dx, dy) = deskPos (wDesks w) i
          bar = case cur >>= \k -> IM.lookup k (wPeople w) of   -- 残りのサービス時間のバー
            Nothing -> []
            Just p ->
              let f = fl (clamp01 ((t - pStart p) / max (pEnd p - pStart p) 1e-6))
              in [ box (dx + 52) (dy - 30) 8 60 (hsba 0 0 100 0.25)
                 , box (dx + 52) (dy + 30 - 60 * f) 8 (60 * f) (hsb 140 70 95) ]
      in [ box dx (dy - 32) 46 64 (hsb 200 50 (maybe 35 (const 70) cur))
         , txtC (dx + 23) (dy + 50) 14 (gray 1) ("Desk " ++ show (i + 1 :: Int)) ] ++ bar
    drawPerson p =
      let waited = fl ((if pStart p < 0 then t else pStart p) - pArr p)   -- 待った時間で色を変える
      in dot (pX p) (pY p) 20 (hsba (clampF 0 140 (140 - waited * 25)) 70 95 (if pStage p == Finished then 0.5 else 1))
    rows =
      [ "Elapsed  " ++ nf 1 t ++ " min"
      , "In line  " ++ show (Seq.length (wLine w))
      , "In shop  " ++ show (inSystem w)
      , "Mean in shop  " ++ nf 2 (wArea w / max t 1e-6)
      , "Served  " ++ show (wServed w)
      , "Mean wait  " ++ nf 2 (wSumWait w / fromIntegral (max (wServed w + nBusy w) 1)) ++ " min"
      , "Longest line  " ++ show (wMaxQ w)
      , "Gave up  " ++ show (wBalked w)
      , "rho = lambda/(c mu)"
      , "    = " ++ nf 2 rho ]
    panelRow :: Int -> String -> Picture
    panelRow i s = txt (width * 0.8 + 10) (height * 0.24 + fromIntegral i * 26) 16
      (if i >= 8 then (if rho >= 1 then hsb 0 80 100 else hsb 45 90 100) else gray 1) s

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 32 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0.25 (wSpeed w / 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wLambda = round1 (min 3 (wLambda w + 0.1)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wLambda = round1 (max 0.1 (wLambda w - 0.1)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> resetAll w { wDesks = digitToInt c }
    | c `elem` "rR" -> resetAll w
  _ -> pure w
  where round1 v = fromIntegral (round (v * 10) :: Int) / 10

-- ---------- やってみよう ----------
-- 1. λ = 0.5, 0.8, 0.95 で 5 分ずつ眺める。平均の待ち時間はどう変わる？（予想 → 03 で式と比べる）
-- 2. λ を 1.2 にすると μ = 1 を超える。窓口 1 つでは行列はどうなる？ 2 を押して窓口を増やすと？
-- 3. mu を 2 にして窓口 1 つと、mu = 1 で窓口 2 つはどちらが空いている？（06 で答え合わせ）
-- 4. fillDesks の expRand mu を pure (1 / mu)（いつも同じ時間）に変えると行列はどう変わる？（08 のテーマ）

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
