-- ============================================
-- 51-11 存続可能システムモデル（ビア 1972）の考え方
-- 4 つの現場（システム 1）が、同じ倉庫（共有の資源）から品物を取り寄せて在庫を目標に保とうとする。届くまでに遅れがあるので、
-- 各自がばらばらに注文すると在庫が大きくゆれる。調整（システム 2）＝「注文中の分も数える」を入れるとゆれが減る
-- 在庫が尽きそうになると、まっすぐ上（システム 3）へ「痛み」の信号（アルゲドニック信号）が飛び、倉庫から緊急に届ける
-- 操作：2 = 調整 ON/OFF / 3 = 緊急信号 ON/OFF / ↑↓ = 届くまでの遅れ / ←→ = 注文の強さ k / 現場をクリック = 注文が急に増える / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nUnit :: Int
nUnit = 4

target, prodRate, pMax, alarmQ, dtSim :: Double
target = 20          -- ★ 在庫の目標（システム 5 の方針）
prodRate = 22        -- ★ 倉庫に入ってくる量（1 日あたり）。需要の平均は 4 × 5 = 20
pMax = 120           -- 倉庫の大きさ（あふれた分は捨てる）
alarmQ = 3           -- ★ これより在庫が減ると緊急信号
dtSim = 0.1          -- 1 きざみ = 0.1 日

speed :: Int
speed = 4            -- 1 フレームに何きざみ進めるか

-- 設定：調整（システム 2）、緊急信号（システム 3）、遅れ [日]、注文の強さ k
data Par = Par { pS2, pS3 :: !Bool, pDelay :: !Int, pK :: !Double }

-- 現場：在庫、注文中の品物（量、届く時刻）、需要の位相、急な注文、成績
data Unit = Unit { uQ :: !Double, uPipe :: [(Double, Double)], uPh, uShock :: !Double
                 , uAlarms :: !Int, uEmerg, uDev2 :: !Double, uN :: !Int }

-- 組織の状態
data Sim = Sim { sT, sP :: !Double, sUnits :: [Unit], sProduced, sWasted, sUsed, sQ0 :: !Double, sSignal :: !Int }

data World = World
  { wPar  :: !Par
  , wSim  :: !Sim
  , wHist :: [[Double]]       -- 4 つの在庫の履歴（新しい順、600 個まで）
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-11 Viable system" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (Par False True 4 0.5) (makeSim 1) [] []

-- ---- 組織の状態
makeSim :: Word32 -> Sim
makeSim seed = Sim 0 60 us 0 0 0 (fromIntegral nUnit * target + 60) (-1)
  where
    phs = take nUnit (rngList (makeRng seed))
    us = [ Unit target [] (6.28 * ph) 0 0 0 0 0 | ph <- phs ]

demand :: Unit -> Double -> Double
demand u t = 5 + 1.5 * sin (0.35 * t + uPh u) + uShock u

inPipe :: Unit -> Double
inPipe u = foldl' (\a (amt, _) -> a + amt) 0 (uPipe u)

-- ---- 1 きざみ
simStep :: Par -> Sim -> Sim
simStep p s = sFinal
  where
    t = sT s
    dl = fromIntegral (pDelay p)
    req = [ max 0 want * dtSim
          | u <- sUnits s
          , let d = demand u t
                want = if pS2 p
                         then d + pK p * (target + d * dl - uQ u - inPipe u)   -- 調整あり：注文中（まだ届いていない分）も数える
                         else d + pK p * (target - uQ u) ]                     -- 調整なし：目の前の在庫だけを見る
    tot = foldl' (+) 0 req
    f = if tot > sP s then sP s / tot else 1                                   -- 倉庫が足りなければ全員を同じ割合でへらす
    -- 現場を 1 つずつ順に（倉庫の量 P は順に減っていく）
    (pAfter, used, sig, usRev) = foldl' one (sP s, sUsed s, -1, []) (zip3 [0 ..] (sUnits s) req)
    one (!pp, !us, !sg, acc) (i, u, r) =
      let g = r * f
          pA = pp - g
          pipe0 = uPipe u ++ [(g, t + dl)]
          (arrived, pipe1) = span (\(_, at') -> at' <= t + 1e-9) pipe0
          q1 = foldl' (\a (amt, _) -> a + amt) (uQ u) arrived
          d = demand u t * dtSim
          q2 = q1 - d
          alarm = pS3 p && q2 < alarmQ && pA > 0                                -- 痛みの信号 → システム 3 が直接届ける
          e = if alarm then min pA ((target / 2 - q2) * 0.2) else 0
          q3 = q2 + e
          late = t > 30
          u' = u { uQ = q3, uPipe = pipe1, uEmerg = uEmerg u + e, uAlarms = uAlarms u + (if alarm then 1 else 0)
                 , uShock = max 0 (uShock u - 0.6 * dtSim)
                 , uDev2 = if late then uDev2 u + (q3 - target) * (q3 - target) else uDev2 u
                 , uN = if late then uN u + 1 else uN u }
      in (pA - e, us + d, if alarm then i else sg, u' : acc)
    p1 = pAfter + prodRate * dtSim
    sFinal = s { sT = t + dtSim, sP = min p1 pMax, sUnits = reverse usRev, sUsed = used, sSignal = sig
               , sProduced = sProduced s + prodRate * dtSim
               , sWasted = sWasted s + (if p1 > pMax then p1 - pMax else 0) }

total :: Sim -> Double
total s = sP s + foldl' (\a u -> a + uQ u + inPipe u) 0 (sUnits s)

rmsDev :: Sim -> Double
rmsDev s = let a = sum (map uDev2 (sUnits s))
               n = sum (map uN (sUnits s))
           in if n > 0 then sqrt (a / fromIntegral n) else 0

step :: World -> World
step w = w { wSim = sim, wHist = take 600 (map uQ (sUnits sim) : wHist w) }
  where sim = foldl' (\t _ -> simStep (wPar w) t) (wSim w) [1 .. speed]

-- 現場の箱の位置
unitsX :: Env -> (Float, Float)
unitsX env = let ux = 30 + 230 + 50 in (ux, (envW env * 0.66 - ux) / fromIntegral nUnit - 12)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Viable system (Beer): coordination and pain signals calm the swings" 20 36 20 cTxt
  , say ("2 coordination " ++ onOff (pS2 p) ++ "   3 alarm signal " ++ onOff (pS3 p) ++ "   Up/Down delay " ++ show (pDelay p) ++ " days   Left/Right order gain k = " ++ fmt 2 (pK p)) 20 62 15 cSub
  , say "Click a unit: sudden orders   R reset" 20 84 15 cSub
    -- ---- 図：5 つのシステム
  , blockBox x0 y0 bw bh "System 5: policy" (Just ("target stock " ++ num target)) cBrass
  , blockBox x0 (y0 + 70) bw bh "System 4: outside" (Just ("demand " ++ fmt 1 (sum [ demand u (sT sim) | u <- sUnits sim ]) ++ "/day")) cBrass
  , blockBox x0 (y0 + 140) bw bh "System 3: control" (Just ("store P = " ++ fmt 1 (sP sim))) (if sSignal sim >= 0 then cAlarm else cTube)
  , blockBox x0 (y0 + 210) bw bh "System 2: coord." (Just (if pS2 p then "counts orders" else "none")) (if pS2 p then cScope else cSub) ]
  ++ concat (zipWith unitBox [0 ..] (sUnits sim))
  ++ [ say ("Units 1-4 = System 1   red line = pain signal (alarms " ++ show (sum (map uAlarms (sUnits sim)))
            ++ ", delivered " ++ fmt 0 (sum (map uEmerg (sUnits sim))) ++ ")") ux (y0 + 290) 14 cAlarm ]
    -- 数
  ++ [ say "Stock deviation from target" mx my 15 cTxt
     , say "(RMS, from day 30)" mx (my + 20) 14 cSub
     , say (fmt 2 rms) mx (my + 64) 40 (if rms < 3 then cScope else cAlarm)
     , say ("time " ++ fmt 0 (sT sim) ++ " days") (mx + 160) (my + 60) 15 cSub
     , say ("No coordination: delay x k = " ++ fmt 2 dk) mx (my + 96) 14 cSub
     , say (if dk > pi / 2 then "  > pi/2 -> keeps swinging (as in 02)" else "  <= pi/2") mx (my + 116) 14 cSub
     , say "Balance: store + stock + ordered - in" mx (my + 140) 14 cSub
     , say ("  + used + wasted = " ++ fmt 6 (total sim - sProduced sim + sUsed sim + sWasted sim)) mx (my + 160) 14 cSub
     , say ("  (stays at the start value " ++ num (sQ0 sim) ++ ")") mx (my + 180) 14 cSub ]
  ++ [ plotFrame gx gy gwid ght ("Stock of the 4 units (0 to 50, dashed = target " ++ num target ++ ", red = alarm line)")
     , hLine target gx gy gwid ght 0 50 cBrass
     , hLine alarmQ gx gy gwid ght 0 50 cAlarm ]
  ++ [ plotLine (map (\h -> getAt 0 h i) hist) gx gy gwid ght 0 50 c 600 1.6 | (i, c) <- zip [0 ..] [cTube, cScope, cBrass, (200, 50, 95)] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    p = wPar w
    sim = wSim w
    x0 = 30
    y0 = 112
    bw = 230
    bh = 52
    (ux, uw) = unitsX env
    unitBox i u =
      let x = ux + fromIntegral i * (uw + 12)
          y = y0 + 140
          low = uQ u < alarmQ
          hq = f2 (clampv 0 1 (uQ u / (2 * target))) * 60
      in [ Color (col cSub 50) (thickLine 1.5 (x0 + bw, y0 + 165) (x, y + 30))
         , Color (hsb 25 40 16) (rectTL x y uw 120)
         , Color (col (if low then cAlarm else cTube) 100) (frameTL 2 x y uw 120)
         , Color (hsba 0 0 100 0.08) (rectTL (x + 10) (y + 34) (uw - 20) 60)
         , Color (col (if low then cAlarm else cScope) 100) (rectTL (x + 10) (y + 94 - hq) (uw - 20) hq)
         , say ("Unit " ++ show (i + 1 :: Int)) (x + 10) (y + 24) 14 cTxt
         , say ("q = " ++ fmt 1 (uQ u)) (x + 8) (y + 112) 14 cTxt
         , say ("pipe " ++ fmt 1 (inPipe u)) x (y - 8) 14 cSub ]
         ++ [ Color (col cAlarm 100) (thickLine 4 (x + uw / 2, y) (x0 + bw, y0 + 166)) | sSignal sim == i ]
    mx = envW env * 0.68
    my = 122
    rms = rmsDev sim
    dk = fromIntegral (pDelay p) * pK p
    gx = 30
    gy = y0 + 320
    gwid = envW env - 60
    ght = envH env - gy - 40
    hist = reverse (wHist w)

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '2' -> resetSim w { wPar = p { pS2 = not (pS2 p) } }
    | c == '3' -> w { wPar = p { pS3 = not (pS3 p) } }
    | c `elem` "rR" -> resetSim w
  EventKey (SpecialKey KeyUp) Down _ _ -> resetSim w { wPar = p { pDelay = min 8 (pDelay p + 1) } }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetSim w { wPar = p { pDelay = max 1 (pDelay p - 1) } }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPar = p { pK = min 1.5 (round2 (pK p + 0.1)) } }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPar = p { pK = max 0.1 (round2 (pK p - 0.1)) } }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        (ux, uw) = unitsX env
        i = floor ((mx - ux) / (uw + 12)) :: Int
        sim = wSim w
    in if i >= 0 && i < nUnit && my > 112 + 88 && my < 112 + 288
         then w { wSim = sim { sUnits = [ if k == i then u { uShock = 4 } else u | (k, u) <- zip [0 ..] (sUnits sim) ] } }
         else w
  _ -> w
  where
    p = wPar w
    round2 v = fromIntegral (round (v * 100) :: Int) / 100

resetSim :: World -> World
resetSim w = w { wSim = makeSim 1, wHist = [] }

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
runFor :: Par -> Double -> Sim
runFor p days = foldl' (\t _ -> simStep p t) (makeSim 1) [1 .. ceiling (days / dtSim) :: Int]

selfTest :: [(String, Bool)]
selfTest =
  [ ("swing RMS without coordination " ++ fmt 2 (rmsDev off) ++ " -> with " ++ fmt 2 (rmsDev on) ++ " (1/3 or less)", rmsDev on < rmsDev off / 3)
  , ("goods conserved: store + stock + ordered - in + used + wasted = start " ++ num (sQ0 s) ++ " (diff < 1e-6)", near bal (sQ0 s) 1e-6)
  , ("alarm signal raises the minimum stock (" ++ fmt 1 m0 ++ " -> " ++ fmt 1 m1 ++ ")", m1 > m0)
  , ("short delay (1 day x k 0.5 < pi/2): small swing even without coordination (RMS " ++ fmt 2 (rmsDev low) ++ " < 3)", rmsDev low < 3)
  , ("extreme setting (delay 8 days, k = 1.5) gives no NaN", ok (total ex) && all (ok . uQ) (sUnits ex)) ]
  where
    off = runFor (Par False False 4 0.5) 200
    on = runFor (Par True False 4 0.5) 200
    s = runFor (Par False True 4 0.5) 200
    bal = total s - sProduced s + sUsed s + sWasted s
    minQ pr = snd (foldl' (\(!t, !m) _ -> let t' = simStep pr t
                                          in (t', if sT t' > 30 then minimum (m : map uQ (sUnits t')) else m))
                          (makeSim 1, 1e9) [1 .. 2000 :: Int])
    m0 = minQ (Par False False 4 0.5)
    m1 = minQ (Par False True 4 0.5)
    low = runFor (Par False False 1 0.5) 200
    ex = runFor (Par False True 8 1.5) 200
    ok v = not (isNaN v || isInfinite v)

-- ---------- やってみよう ----------
-- 1. 2 で調整を入れたり切ったりして、在庫のゆれと RMS を比べよう（切りかえるとやり直しになる）
-- 2. ↑ で遅れを長くすると？ 調整なしの「遅れ × k」が π/2 を超える所に注目（02 の企図振戦と同じ式）
-- 3. 3 で緊急信号を切ると、在庫がマイナス（品切れの借り）になる現場が出る？
-- 4. 現場をクリックして急な注文を入れる。ほかの現場にも影響が出るのはなぜ？（倉庫を分け合っているから）
-- 5. 学校やバイト先で「システム 1〜5」にあたるものは何か考えよう

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- グラフの中の水平線（目標値など、点線）
hLine :: Double -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Picture
hLine v x y w h lo hi c =
  let yy = y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h
  in Color (col c 70) (Pictures [ Line [(x + s, yy), (x + min w (s + 6), yy)] | s <- [0, 11 .. w] ])

-- ブロック図の箱（名前と今の値）
blockBox :: Float -> Float -> Float -> Float -> String -> Maybe String -> HSB -> Picture
blockBox x y w h name val c = Pictures $
  [ Color (hsb 25 40 16) (rectTL x y w h), Color (col c 100) (frameTL 2 x y w h)
  , sayC name (x + w / 2) (y + h / 2 - (if val == Nothing then 0 else 10)) 15 cTxt ]
  ++ [ sayC v (x + w / 2) (y + h / 2 + 12) 14 c | Just v <- [val] ]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
newtype Rng = Rng Word32

makeRng :: Word32 -> Rng
makeRng = Rng

nextR :: Rng -> (Double, Rng)
nextR (Rng a0) = (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)
  where
    a = a0 + 0x6d2b79f5
    t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
    t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))

rngList :: Rng -> [Double]
rngList r = let (v, r') = nextR r in v : rngList r'

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
