-- ============================================
-- 55-08 アインシュタイン固体 ─ エネルギーの「場合の数」を正確に数える
-- 固体 = N 個の振動子。エネルギーは ε のかたまり（量子）でしか持てない。q 個の量子を N 個に分ける方法は
-- Ω(N, q) = C(q + N − 1, q) 通り（Integer = 桁数に上限のない整数で正確に数える）。2 つの固体 A・B で量子をやり取りすると、
-- いちばん場合の数が多い分け方 = エントロピー S = k ln Ω が最大 = 熱平衡。そこで 2 つの温度（1/T = dS/dq）が等しい
-- 操作：← → 振動子 NA / ↑↓ 量子の数 q / 1 2 3 例（小・中・大）/ スペース やり取り開始・停止 / L 縦軸を対数に / R 全部 A に
--       （スペースと L 以外のキーを押すと、量子は全部 A にもどる）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nTotalMax :: Int
nTotalMax = 500            -- ★ 振動子の最大数

-- qA = 0〜q のそれぞれの Ω_A·Ω_B、エントロピー、確率
data Table = Table
  { tNA, tNB, tQ :: !Int
  , tLnA, tLnB, tLnT, tP :: !(U.Vector Double)
  , tTotal :: !Integer, tLnAll :: !Double, tPeak :: !Int }

data World = World
  { wNA, wNB, wQ :: !Int
  , wTab     :: !Table
  , wOsc     :: !(U.Vector Int)    -- 振動子ごとの量子の数（0〜NA−1 が A）
  , wRunning :: !Bool
  , wLogY    :: !Bool
  , wQaLog   :: [Int]              -- qA の記録（新しい順、300 個まで）
  , wQaHist  :: !(U.Vector Int)    -- 出た qA の回数
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-08 Einstein solid" world0)
  { skBack  = rgbC 24 22 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuild (World 300 200 100 (makeTable 1 1 1) U.empty False False [] U.empty (seedRng 8) [])
           { wRunning = True }

-- ---- 場合の数（Integer で正確に）
omega :: Int -> Int -> Integer        -- C(q+N−1, q)
omega n q = foldl' (\r i -> (r * fromIntegral (q + n - i)) `div` fromIntegral i) 1 [1 .. q]

-- Ω(N, 0), Ω(N, 1), … を順に：Ω(N, q+1) = Ω(N, q)·(q + N)/(q + 1)
omegaRow :: Int -> Int -> [Integer]
omegaRow n qmax = scanl (\r q -> (r * fromIntegral (q + n)) `div` fromIntegral (q + 1)) 1 [0 .. qmax - 1]

-- 大きな整数の ln（上の 15 桁 + 桁数）
bigLn :: Integer -> Double
bigLn b = let s = show b
          in if length s <= 15 then log (fromIntegral b)
             else log (fromIntegral (read (take 15 s) :: Integer)) + fromIntegral (length s - 15) * log 10

makeTable :: Int -> Int -> Int -> Table
makeTable na nb q = Table na nb q (U.fromList lnA) (U.fromList lnB) lnTv (U.map (\l -> exp (l - lnAll)) lnTv) total lnAll peak
  where
    ra = V.fromList (omegaRow na q)
    rb = V.fromList (omegaRow nb q)
    ws = [ ra V.! qa * rb V.! (q - qa) | qa <- [0 .. q] ]
    total = foldl' (+) 0 ws
    lnA = [ bigLn (ra V.! qa) | qa <- [0 .. q] ]
    lnB = [ bigLn (rb V.! (q - qa)) | qa <- [0 .. q] ]
    lnTv = U.fromList (map bigLn ws)
    lnAll = bigLn total
    peak = U.ifoldl' (\pk i l -> if l > lnTv U.! pk then i else pk) 0 lnTv

-- 温度：1/T = dS/dq（となりとの差で）。S = ln Ω、k = ε = 1 の単位
tempA, tempB :: Table -> Int -> Double
tempA t qa = let a = max 0 (qa - 1); b = min (tQ t) (qa + 1)
             in if b > a then fromIntegral (b - a) / max 1e-9 (tLnA t U.! b - tLnA t U.! a) else 0 / 0
tempB t qa = let a = max 0 (qa - 1); b = min (tQ t) (qa + 1)
             in if b > a then fromIntegral (b - a) / max 1e-9 (tLnB t U.! a - tLnB t U.! b) else 0 / 0

-- 表を作り直し、量子を全部 A に
rebuild :: World -> World
rebuild w = w { wTab = makeTable (wNA w) (wNB w) (wQ w), wOsc = osc, wQaLog = [], wQaHist = U.replicate (wQ w + 1) 0 }
  where osc = U.accum (+) (U.replicate (wNA w + wNB w) 0) [ (k `mod` wNA w, 1) | k <- [0 .. wQ w - 1] ]

-- ランダムに選んだ振動子 i から j へ量子を 1 つ（i が持っていれば）
exchange :: Int -> (U.Vector Int, Rng) -> (U.Vector Int, Rng)
exchange n (o, r0) = runST $ do
  m <- U.thaw o
  let len = fromIntegral (U.length o) :: Double
      go 0 !r = pure r
      go k !r = do
        let (a, r1) = rnd r
            (b, r2) = rnd r1
            i = floor (a * len)
            j = floor (b * len)
        vi <- UM.read m i
        when (vi > 0) $ do
          UM.write m i (vi - 1)
          vj <- UM.read m j
          UM.write m j (vj + 1)
        go (k - 1 :: Int) r2
  r' <- go n r0
  o' <- U.freeze m
  pure (o', r')

qaNow :: Int -> U.Vector Int -> Int
qaNow na o = U.sum (U.take na o)

step :: World -> World
step w
  | not (wRunning w) = w
  | otherwise = let (o, r) = exchange 400 (wOsc w, wRng w)
                    qa = qaNow (wNA w) o
                in w { wOsc = o, wRng = r, wQaLog = take 300 (qa : wQaLog w), wQaHist = U.accum (+) (wQaHist w) [(qa, 1)] }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 振動子のマス目（量子の数で明るさ）
  [ Color (if i < na then rgbC (jsRoundI (60 + 195 * v)) (jsRoundI (40 + 80 * v)) 30 else rgbC 30 (jsRoundI (50 + 100 * v)) (jsRoundI (70 + 185 * v)))
          (rectTL (ox + fromIntegral (k `mod` cols) * cs) (oy + fromIntegral rr * cs) (cs - 1) (cs - 1))
  | i <- [0 .. U.length osc - 1]
  , let k = if i < na then i else i - na
        rr = k `div` cols + (if i < na then 0 else (na + cols - 1) `div` cols + 1)
        v = clampv 0 1 (fromIntegral (osc U.! i) / 4) ]
  ++ [ say ("A: " ++ show na ++ " oscillators, " ++ show qa ++ " quanta") ox (oy - 8) 14 (rgbC 255 160 90)
     , say ("B: " ++ show (wNB w) ++ " oscillators, " ++ show (wQ w - qa) ++ " quanta") ox (oy + fromIntegral ((na + cols - 1) `div` cols + 1) * cs - 4) 14 (rgbC 120 180 255) ]
  -- ---- グラフ 1：Ω_A·Ω_B の確率と、やり取りで出た qA のヒストグラム
  ++ [ plotAxes g1 "quanta in A, qA" (if wLogY w then "probability (log)" else "probability Omega_A Omega_B / Omega_total")
         [0, jsRound (fromIntegral q / 2), fromIntegral q] (if wLogY w then [1e-12, 1e-8, 1e-4, 1] else [0, roundSig 2 (pm * 1.2)]) ]
  ++ [ Color (rgbaC 255 160 90 130) (rectTL (gX g1 (fromIntegral k - 0.45)) y (max 1 (gX g1 (fromIntegral k + 0.45) - gX g1 (fromIntegral k - 0.45))) (pY g1 + pH g1 - y))
     | tot > 0, (k, c) <- zip [0 :: Int ..] (U.toList (wQaHist w)), c > 0
     , let y = gY g1 (max (y0 g1) (fromIntegral c / fromIntegral tot)) ]
  ++ [ Color (rgbC 130 200 255) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 (max (y0 g1) p)) | (k, p) <- zip [0 :: Int ..] (U.toList (tP tab)) ])
     , Color (rgbaC 255 255 255 120) (Line [(gX g1 pk, pY g1), (gX g1 pk, pY g1 + pH g1)])
     , Color (rgbC 255 220 120) (Line [(gX g1 (fromIntegral qa), pY g1 + pH g1 - 12), (gX g1 (fromIntegral qa), pY g1 + pH g1)]) ]
  -- ---- グラフ 2：エントロピー S_A、S_B、S_全体
  ++ [ plotAxes g2 "quanta in A, qA" "entropy S = ln Omega (k = 1)" [0, jsRound (fromIntegral q / 2), fromIntegral q] [0, jsRound smax]
     , Color (rgbC 255 150 80) (curve (tLnA tab))
     , Color (rgbC 110 170 255) (curve (tLnB tab))
     , Color (grayC 240) (curve (tLnT tab))
     , Color (rgbaC 255 255 255 120) (Line [(gX g2 pk, pY g2), (gX g2 pk, pY g2 + pH g2)]) ]
  -- ---- 文字
  ++ [ say ("Einstein solid   NA = " ++ show na ++ "   NB = " ++ show (wNB w) ++ "   q = " ++ show q ++ "   total ways Omega = "
            ++ (if length (show (tTotal tab)) > 12 then "about 10^" ++ fmt 1 (tLnAll tab / log 10) else show (tTotal tab))) 20 36 18 (grayC 240)
     , say ("<- -> NA  Up/Down q  1 2 3 examples  Space exchange " ++ (if wRunning w then "(on)" else "(off)")
            ++ "  L log  R all in A  | white: peak qA = " ++ show (tPeak tab)) 20 62 15 (grayC 175)
     , say ("At the peak kT_A = " ++ fmtN 3 ta ++ ", kT_B = " ++ fmtN 3 tb ++ " (1/T = dS/dq equal)   q NA/(NA+NB) = "
            ++ fmt 1 (fromIntegral (q * na) / fromIntegral (na + wNB w)) ++ "   now qA = " ++ show qa) 20 86 15 (rgbC 255 200 120)
     , say "Blue = exact probability, orange = share of qA from random exchanges" (pX g1) (pY g1 + pH g1 + 52) 14 (grayC 170)
     , say "Orange S_A, blue S_B, white S_A + S_B (peak = equilibrium)" (pX g2) (pY g2 + pH g2 + 52) 14 (grayC 170) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    na = wNA w
    q = wQ w
    tab = wTab w
    osc = wOsc w
    qa = qaNow na osc
    cols = 25
    cs = fromIntegral (min 14 (floor (envW env * 0.3 / fromIntegral cols) :: Int)) :: Float
    ox = 20
    oy = 120
    gx = ox + fromIntegral cols * cs + 70
    gw = envW env - gx - 30
    gh = (envH env - 270) / 2
    pm = U.maximum (tP tab)
    g1 = (graph gx 120 gw gh (-0.5) (fromIntegral q + 0.5) (if wLogY w then 1e-12 else 0) (if wLogY w then 1 else pm * 1.2)) { logY = wLogY w }
    tot = U.sum (wQaHist w)
    pk = fromIntegral (tPeak tab)
    smax = U.maximum (tLnT tab)
    g2 = graph gx (120 + gh + 80) gw gh 0 (fromIntegral q) 0 (smax * 1.1 + 1)
    curve vs = polyline 2 [ (gX g2 (fromIntegral k), gY g2 s) | (k, s) <- zip [0 :: Int ..] (U.toList vs) ]
    ta = tempA tab (tPeak tab)
    tb = tempB tab (tPeak tab)

-- NaN は "NaN" と書く（toFixed と同じ）
fmtN :: Int -> Double -> String
fmtN n v = if isNaN v then "NaN" else fmt n v

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (Char c) Down _ _ | c `elem` "lL" -> w { wLogY = not (wLogY w) }
  EventKey (MouseButton _) _ _ _ -> w
  EventKey k Down _ _ -> rebuild (key k w)      -- スペースと L 以外のキーは、変えてから全部 A にもどす
  _ -> w
  where
    na = wNA w
    q = wQ w
    key k v = case k of
      SpecialKey KeyRight -> v { wNA = min (nTotalMax - wNB v) (na + (if na < 20 then 1 else 10)) }
      SpecialKey KeyLeft -> v { wNA = max 1 (na - (if na <= 20 then 1 else 10)) }
      SpecialKey KeyUp -> v { wQ = min 400 (q + (if q < 20 then 1 else 10)) }
      SpecialKey KeyDown -> v { wQ = max 1 (q - (if q <= 20 then 1 else 10)) }
      Char '1' -> v { wNA = 3, wNB = 3, wQ = 6 }
      Char '2' -> v { wNA = 30, wNB = 20, wQ = 50 }
      Char '3' -> v { wNA = 300, wNB = 200, wQ = 100 }
      _ -> v

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Omega(N = 3, q = 3) = 10, Omega(N, 0) = 1, Omega(1, q) = 1", omega 3 3 == 10 && omega 50 0 == 1 && omega 1 40 == 1)
  , ("sum Omega_A(qA) Omega_B(q - qA) = Omega(NA + NB, q) (Vandermonde, exact integers)", tTotal t == omega 50 50)
  , ("most likely split qA = " ++ show (tPeak big) ++ " ~ q NA/(NA+NB) = 60 (+-1)", abs (tPeak big - 60) <= 1)
  , ("there the two temperatures are equal (kT_A = " ++ fmt 3 ta ++ ", kT_B = " ++ fmt 3 tb ++ ", within 2%)", abs (ta / tb - 1) < 0.02)
  , ("1/kT = dS/dq ~ ln(1 + N/q) (large N, q formula, within 1%)", abs (slope / log (1 + 300 / 100) - 1) < 0.01)
  , ("long random exchanges: share of qA -> exact probability (distance " ++ fmt 3 l1 ++ " < 0.05)", l1 < 0.05) ]
  where
    t = makeTable 30 20 50
    big = makeTable 300 200 100
    ta = tempA big (tPeak big)
    tb = tempB big (tPeak big)
    slope = (bigLn (omega 300 101) - bigLn (omega 300 99)) / 2
    s = makeTable 6 4 10
    run (!h, !st) _ = let st'@(o, _) = exchange 5 st in (U.accum (+) h [(qaNow 6 o, 1 :: Int)], st')
    (hh, _) = foldl' run (U.replicate 11 0, (U.fromList (10 : replicate 9 0), seedRng 80)) [1 .. 40000 :: Int]
    l1 = U.ifoldl' (\a k c -> a + abs (fromIntegral c / 40000 - tP s U.! k)) 0 hh

-- ---------- やってみよう ----------
-- 1. 1 を押して NA = NB = 3、q = 6。Ω_A·Ω_B を手で数えて、グラフと比べよう（全部で 462 通り）
-- 2. 3 にして R。全部 A にあった量子が B へ流れ、山の所で止まる。もう一度全部 A にもどることはある？
-- 3. L で対数にすると、山からはずれた分け方がどれだけめずらしいか見える
-- 4. NA を大きくすると、A の温度はどうなる？ 熱は温度の高いほうから低いほうへ流れる？
-- 5. 1/T = dS/dq：量子 1 個を渡すときのエントロピーの増え方が、温度を決める

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を n 個まとめて
rndList :: Int -> Rng -> ([Double], Rng)
rndList n r0 = go n r0 []
  where go 0 r acc = (reverse acc, r)
        go k r acc = let (v, r') = rnd r in go (k - 1) r' (v : acc)

-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

-- 指数表記（toExponential）
expo :: Int -> Double -> String
expo n v = let s = showEFloat (Just n) (fin 0 v) ""
               (a, b) = break (== 'e') s
           in case b of
                ('e' : '-' : _) -> s
                ('e' : rest) -> a ++ "e+" ++ rest
                _ -> s

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
