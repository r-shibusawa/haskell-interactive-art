-- ============================================
-- 12 コールセンターの人員計画（応用）
-- 9 時〜18 時のコールセンター。電話の数は時間で変わる（昼前と午後にピーク）。1 本の通話は平均 4 分
--   待たされすぎた客は電話を切る（あきらめ）。オペレーターを増やせば人件費、減らせば待ち時間とクレームのコスト
--   1 = 1 日同じ人数　2 = 平方根の法則 c = a + β√a（a = λ/μ = その時間に必要な平均人数）　3 = 自分で決める（棒をクリック）
-- 1〜3 = 方針 / ←→ = 人数（1）または β（2）/ クリック・ドラッグ = 時間ごとの人数（3）/ ↑↓ = 速さ / スペース = 一時停止 / R = 1 日をやり直す
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (delete)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Text.Printf (PrintfArg, printf)

meanCall, patience, wage, waitCost, abandonCost :: Double
meanCall = 4        -- ★ 平均通話時間（分）
patience = 5        -- ★ 平均でこれだけ待つと電話を切る（分）
wage = 2000         -- ★ オペレーター 1 人 1 時間の人件費（円）
waitCost = 40       -- ★ 待たせた 1 分あたりのコスト（円）
abandonCost = 500   -- ★ 切られた電話 1 本のコスト（円）

dayEnd :: Double
dayEnd = 540        -- 9 時間 = 540 分

-- 待っている電話：かかってきた時刻、あきらめる時刻
data Call = Call { cArr, cGive :: !Double } deriving Eq

-- 1 日の結果（表の 1 行）
data Day = Day { dPol :: String, dHours :: !Int, dWait, dLost :: !Double, dTotal :: !Double }

data World = World
  { wPolicy, wFixed :: !Int
  , wBeta :: !Float
  , wCustom :: !(U.Vector Int)
  , wSpeed :: !Float      -- ★ 1 秒で進む分
  , wPaused :: !Bool
  , wT, wNextArr :: !Double
  , wAgents :: ![Double]  -- 話し中のオペレーターの終了時刻
  , wQ :: ![Call]
  , wStaff :: !(U.Vector Int)
  , wRec :: !(Seq Int)
  , wCalls, wServed, wAb :: !Int
  , wSumW :: !Double
  , wHourW :: !(U.Vector Double)
  , wHourN :: !(U.Vector Int)
  , wDone :: !Bool
  , wDays :: ![Day]
  }

main :: IO ()
main = runSketch (sketch "29-12 Call Center Staffing" world0)
  { skBack  = hsb 25 25 12
  , skStart = \_ -> resetDay world0
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World 2 12 1.0 (U.fromList [8, 12, 16, 12, 9, 12, 15, 13, 9]) 16 False 0 0 [] []
  (U.replicate 9 12) Seq.empty 0 0 0 0 (U.replicate 9 0) (U.replicate 9 0) False []

-- ★ 時刻 t（分、0 = 9:00）の到着率（1 分あたり）
lamAt :: Double -> Double
lamAt t =
  let h = 9 + t / 60
      sq x = x * x
  in 1.2 + 2.6 * exp (negate (sq ((h - 11) / 1.1))) + 2.0 * exp (negate (sq ((h - 15) / 1.4)))

-- a = λ/μ
loadAt :: Int -> Double
loadAt hr = sum [ lamAt (fromIntegral (hr * 60 + k * 10 + 5)) | k <- [0 .. 5] ] / 6 * meanCall

plan :: World -> U.Vector Int
plan w = U.generate 9 $ \h -> case wPolicy w of
  1 -> wFixed w
  2 -> let a = loadAt h in ceiling (a + realToFrac (wBeta w) * sqrt a)   -- ★ 平方根の法則
  _ -> wCustom w U.! h

resetDay :: World -> IO World
resetDay w = do
  n <- nextArrival 0
  pure w { wT = 0, wStaff = plan w, wNextArr = n, wAgents = [], wQ = [], wRec = Seq.empty
         , wCalls = 0, wServed = 0, wAb = 0, wSumW = 0, wHourW = U.replicate 9 0, wHourN = U.replicate 9 0, wDone = False }

-- 時間で変わるポアソン到着（間引き法：最大の率で候補を出し、確率 λ(t)/λmax で採用）
nextArrival :: Double -> IO Double
nextArrival t0 = do
  let lmax = 6
  g <- expRand lmax
  u <- randomRIO (0, 1)
  let t = t0 + g
  if t >= dayEnd || u < lamAt t / lmax then pure t else nextArrival t

hourOf :: Double -> Int
hourOf t = min 8 (floor (t / 60))

staffNow :: World -> Int
staffNow w = wStaff w U.! hourOf (wT w)

addHour :: Int -> Double -> World -> World
addHour h v w = w { wHourW = U.accum (+) (wHourW w) [(h, v)], wHourN = U.accum (+) (wHourN w) [(h, 1)] }

-- 空いたオペレーターに先頭の電話をつなぐ
startCalls :: World -> IO World
startCalls w = case wQ w of
  (p : rest) | length (wAgents w) < staffNow w -> do
    d <- expRand (1 / meanCall)
    let wt = wT w - cArr p
        w1 = addHour (hourOf (cArr p)) wt w
    startCalls w1 { wQ = rest, wSumW = wSumW w1 + wt, wServed = wServed w1 + 1, wAgents = wT w + d : wAgents w1 }
  _ -> pure w

advance :: Double -> World -> IO World
advance tEnd0 w
  | tn > tEnd = finishDay w { wT = tEnd }
  | tn == tD = startCalls w1 { wAgents = delete tD (wAgents w1) } >>= advance tEnd
  | tn == tB = case [ c | c <- wQ w1, cGive c == tB ] of
      (p : _) -> do
        let wt = tn - cArr p
            w2 = addHour (hourOf (cArr p)) wt w1
        startCalls w2 { wQ = delete p (wQ w2), wAb = wAb w2 + 1, wSumW = wSumW w2 + wt } >>= advance tEnd
      [] -> advance tEnd w1
  | tn == tA = do
      g <- expRand (1 / patience)
      n <- nextArrival tn
      startCalls w1 { wCalls = wCalls w1 + 1, wQ = wQ w1 ++ [Call tn (tn + g)], wNextArr = n } >>= advance tEnd
  | otherwise = startCalls w1 >>= advance tEnd   -- 時間の区切り（人数が変わる）
  where
    tEnd = min tEnd0 dayEnd
    tA = if wNextArr w < dayEnd then wNextArr w else infinity
    tD = minimum (infinity : wAgents w)
    tB = minimum (infinity : map cGive (wQ w))
    tH = fromIntegral ((floor (wT w / 60) + 1) * 60 :: Int)
    tn = minimum [tA, tD, tB, tH]
    w1 = w { wT = tn }

-- 1 日の終わり：残った客は待ち時間だけ数える
finishDay :: World -> IO World
finishDay w
  | wT w >= dayEnd && not (wDone w) =
      let w1 = w { wSumW = wSumW w + sum [ dayEnd - cArr p | p <- wQ w ], wDone = True }
          (hours, _, _, _, tot) = cost w1
          pol = case wPolicy w1 of
            1 -> "Fixed " ++ show (wFixed w1)
            2 -> "Sqrt b=" ++ nf 1 (wBeta w1)
            _ -> "Own plan"
          day = Day pol hours (wSumW w1 / fromIntegral (max (wServed w1 + wAb w1) 1))
                    (fromIntegral (wAb w1) / fromIntegral (max (wCalls w1) 1)) tot
      in pure w1 { wDays = take 6 (day : wDays w1) }
  | otherwise = pure w

-- 人時、人件費、待たせたコスト、あきらめのコスト、合計
cost :: World -> (Int, Double, Double, Double, Double)
cost w =
  let hours = U.sum (wStaff w)
      a = fromIntegral hours * wage
      b = wSumW w * waitCost
      c = fromIntegral (wAb w) * abandonCost
  in (hours, a, b, c, a + b + c)

stepWorld :: World -> IO World
stepWorld w0 = do
  w1 <- if wPaused w0 then pure w0 else advance (wT w0 + realToFrac (wSpeed w0) / 60) w0
  pure $ if floor (wT w1) > Seq.length (wRec w1) - 1 && wT w1 < dayEnd
    then w1 { wRec = wRec w1 |> length (wQ w1) }
    else w1

gx, gw, gy, gh, cmax :: Float
gx = 70
gw = width * 0.6
gy = height * 0.48
gh = height * 0.32
cmax = 26

xOf :: Float -> Float
xOf t = gx + t / 540 * gw

yOf :: Float -> Float
yOf c = gy - c / cmax * gh

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawStaff w
  , drawLine w
  , drawResults w
  , txt 20 40 18 (gray 1) ("Policy " ++ show (wPolicy w) ++ ": " ++ policyText ++ "  Speed x" ++ num (wSpeed w) ++ " (Up/Down)  Space: pause  R: redo")
  , txt 20 68 15 (hsb 0 0 60) "After changing policy, press R and compare total cost in the table. Moving staff to the peaks avoids most of the waiting."
  ]
  where
    policyText = case wPolicy w of
      1 -> "same " ++ show (wFixed w) ++ " all day (<- ->)"
      2 -> "square root rule b = " ++ nf 1 (wBeta w) ++ " (<- ->)"
      _ -> "your own plan (click, drag bars)"

-- 人数の棒と、必要な平均人数 a(t)
drawStaff :: World -> Picture
drawStaff w = Pictures
  ( concat [ let s = fromIntegral (wStaff w U.! h)
                 x60 = fromIntegral (h * 60)
             in [ box (xOf x60 + 2) (yOf s) (gw / 9 - 4) (gy - yOf s) (if wPolicy w == 3 then hsb 200 50 70 else hsb 200 40 50)
                , txtC (xOf (x60 + 30)) (yOf s - 6) 14 (gray 1) (show (wStaff w U.! h))
                , txtC (xOf (x60 + 30)) (gy + 20) 14 (gray 1) (show (9 + h) ++ ":00") ]
           | h <- [0 .. 8] ]
 ++ [ path 3 (hsb 45 90 100) [ (xOf (fromIntegral t), yOf (fl (lamAt (fromIntegral t) * meanCall))) | t <- [0, 5 .. 540 :: Int] ]
    , seg 2 (gray 1) (xOf (fl (wT w)), gy) (xOf (fl (wT w)), gy - gh)
    , txt gx (gy - gh - 8) 14 (hsb 0 0 60) "Blue = staff. Yellow = mean load a = lambda(t) x call length" ] )

-- 行列の長さ
drawLine :: World -> Picture
drawLine w = Pictures
  ( [ seg 1 (hsba 0 0 100 0.3) (gx, ly) (gx + gw, ly)
    , path 2 (hsb 30 80 100) [ (xOf (fromIntegral i), ly - min (fromIntegral v / 30) 1 * lh) | (i, v) <- zip [0 :: Int ..] (toList (wRec w)) ] ]
 ++ [ let v = fl (wHourW w U.! h) / fromIntegral (wHourN w U.! h)
          bar = min (v / 5) 1 * lh
      in box (xOf (fromIntegral (h * 60)) + 10) (ly - bar) (gw / 9 - 20) bar (hsba 0 70 100 0.5)
    | h <- [0 .. 8], wHourN w U.! h > 0 ]
 ++ [ txt gx (ly - lh - 8) 14 (hsb 0 0 60) "Orange = calls waiting (max 30). Red = mean wait of that hour (max 5 min)" ] )
  where
    ly = height - 50
    lh = height * 0.2

-- 結果とこれまでの日
drawResults :: World -> Picture
drawResults w = Pictures
  ( txt tx ty 20 (gray 1) ("Now " ++ show hh ++ ":" ++ printf "%02d" mm ++ (if wDone w then "  (day over)" else ""))
  : zipWith (\i s -> txt tx (ty + 32 + i * 24) 16 (gray 1) s) [0 ..] rows
 ++ [ txt tx (ty + 32 + fromIntegral (length rows) * 24 + 6) 18 (hsb 45 90 100) ("Total  " ++ commas (round tot) ++ " yen")
    , txt tx dy 14 (hsb 0 0 60) "Past days: policy/hours/wait/lost/total" ]
 ++ [ txt tx (dy + 22 + fromIntegral i * 20) 14 (hsb 0 0 (if i == 0 then 100 else 70))
        (dPol d ++ " / " ++ show (dHours d) ++ " / " ++ nf 2 (dWait d) ++ " / " ++ nf 1 (dLost d * 100) ++ "% / " ++ commas (round (dTotal d)))
    | (i, d) <- zip [0 :: Int ..] (wDays w) ] )
  where
    tx = width * 0.665
    ty = 120
    dy = ty + 300
    t = wT w
    hh = 9 + floor (t / 60) :: Int
    mm = floor t `mod` 60 :: Int
    (hours, costA, costB, costC, tot) = cost w
    rows =
      [ "Calls  " ++ show (wCalls w)
      , "Served  " ++ show (wServed w) ++ "   Hung up  " ++ show (wAb w) ++ " (" ++ nf 1 (fromIntegral (wAb w) / fromIntegral (max (wCalls w) 1) * 100 :: Float) ++ "%)"
      , "Mean wait  " ++ nf 2 (wSumW w / fromIntegral (max (wServed w + wAb w) 1)) ++ " min"
      , "Agents on calls  " ++ show (length (wAgents w)) ++ " / " ++ show (staffNow w)
      , ""
      , "Staff " ++ show hours ++ " h x " ++ show (round wage :: Int) ++ " = " ++ commas (round costA) ++ " yen"
      , "Waiting cost  " ++ nf 0 costB ++ " yen"
      , "Hang-up cost  " ++ commas (round costC) ++ " yen" ]

-- 方針 3：棒をクリック・ドラッグして人数を決める
editBar :: Point -> World -> World
editBar pos w
  | wPolicy w /= 3 || mx < gx || mx > gx + gw || my < gy - gh - 20 || my > gy + 10 = w
  | otherwise =
      let h = max 0 (min 8 (floor ((mx - gx) / (gw / 9))))
          v = max 1 (min 26 (round ((gy - my) / gh * 26)))
          w1 = w { wCustom = wCustom w U.// [(h, v)] }
      in w1 { wStaff = plan w1 }
  where (mx, my) = toP5 pos

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (editBar pos w)
  EventMotion pos | envDown env -> pure (editBar pos w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (replan w { wFixed = if wPolicy w == 1 then min 26 (wFixed w + 1) else wFixed w
                                                            , wBeta = if wPolicy w == 2 then roundTo 10 (min 3 (wBeta w + 0.1)) else wBeta w })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (replan w { wFixed = if wPolicy w == 1 then max 1 (wFixed w - 1) else wFixed w
                                                           , wBeta = if wPolicy w == 2 then roundTo 10 (max (-1) (wBeta w - 0.1)) else wBeta w })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 256 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (replan w { wPolicy = digitToInt c })
    | c `elem` "rR" -> resetDay w
  _ -> pure w
  where replan w1 = w1 { wStaff = plan w1 }

-- ---------- やってみよう ----------
-- 1. 方針 1 で、合計コストが最小になる人数を探す（毎回 R）。何人のとき？
-- 2. 方針 2 で β を 0, 0.5, 1, 2 と変えて比べる。同じ人時なら方針 1 より安くなる？
-- 3. waitCost を 10 円（待たせても気にしない会社）と 200 円（高級ホテル）にすると、最適な β はどう動く？
-- 4. 方針 3 で自分の計画をつくって、方針 2 に勝てるか挑戦。ヒント：a(t) は 1 時間の途中でも変わる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- マウスの位置（gloss の座標）を p5 の座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

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

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k

-- 3 けたごとにコンマを入れる（toLocaleString の代わり）
commas :: Int -> String
commas n
  | n < 0 = '-' : commas (negate n)
  | n < 1000 = show n
  | otherwise = commas (n `div` 1000) ++ "," ++ printf "%03d" (n `mod` 1000)
