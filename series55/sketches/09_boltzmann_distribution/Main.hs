-- ============================================
-- 55-09 ボルツマン分布 ─ ランダムにエネルギーをやり取りすると、分布は指数関数 e^(−E/kT) になる
-- 2500 個の粒子が、エネルギーのかたまり（量子 ε）を 1 つずつランダムに渡し合う（全体のエネルギーは一定）
-- はじめは全員同じエネルギーでも、やがて「エネルギー 0 がいちばん多く、高いほど指数関数的に少ない」分布に
-- 片対数グラフでは直線になり、その傾き = −1/kT。理論：p(n) = (1 − a)aⁿ、a = e^(−ε/kT) = ⟨n⟩/(1 + ⟨n⟩)
-- 操作：スペース 一時停止 / ↑↓ エネルギーを足す・引く / ← → 速さ / R 全員同じにもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32, Word8)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

gridN :: Int
gridN = 50                 -- ★ 粒子を gridN × gridN に並べる

eMean :: Int
eMean = 3                  -- ★ はじめの 1 粒子あたりの量子の数

data World = World
  { wEn     :: !(U.Vector Int)   -- 粒子ごとの量子の数
  , wRng    :: !Rng              -- 乱数の状態（コマをまたいで使う）
  , wPaused :: !Bool
  , wSpeed  :: !Int              -- 1 コマのやり取りの回数
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-09 Boltzmann distribution" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (U.replicate (gridN * gridN) eMean) (seedRng 9) False 20000 []

resetAll :: World -> World
resetAll w = w { wEn = U.replicate (gridN * gridN) eMean }

-- ランダムに選んだ粒子 i から j へ量子を 1 つ（i が持っていれば）
exchangeE :: Int -> (U.Vector Int, Rng) -> (U.Vector Int, Rng)
exchangeE n (e, r0) = runST $ do
  m <- U.thaw e
  let nn = U.length e
      go 0 !r = pure r
      go k !r = do
        let (a, r1) = rnd r
            (b, r2) = rnd r1
            i = floor (a * fromIntegral nn)
            j = floor (b * fromIntegral nn)
        vi <- UM.read m i
        when (vi > 0) $ do
          UM.write m i (vi - 1)
          vj <- UM.read m j
          UM.write m j (vj + 1)
        go (k - 1 :: Int) r2
  r' <- go n r0
  e' <- U.freeze m
  pure (e', r')

meanE :: U.Vector Int -> Double
meanE e = fromIntegral (U.sum e) / fromIntegral (U.length e)

-- 割合のヒストグラム（0〜nmax）
histogram :: U.Vector Int -> Int -> [Double]
histogram e nmax = map (/ fromIntegral (U.length e)) (U.toList counts)
  where counts = U.accumulate (+) (U.replicate (nmax + 1) 0 :: U.Vector Double)
                   (U.map (\v -> (v, 1)) (U.filter (<= nmax) e))

-- 理論（⟨n⟩ = m）
geomP :: Int -> Double -> Double
geomP n m = let a = m / (1 + m) in (1 - a) * a ** fromIntegral n

-- e^(−ε/kT) = m/(1+m)、ε = 1
kTof :: Double -> Double
kTof m = 1 / log (1 + 1 / m)

-- 直線の当てはめ（最小二乗）：y = a + b·x
fitLine :: [Double] -> [Double] -> Maybe (Double, Double)
fitLine xs ys
  | n < 2 || d == 0 = Nothing
  | otherwise = Just ((sy - b * sx) / fromIntegral n, b)
  where
    n = length xs
    (sx, sy, sxx, sxy) = foldl' (\(!a, !b', !c, !e) (x, y) -> (a + x, b' + y, c + x * x, e + x * y)) (0, 0, 0, 0) (zip xs ys)
    d = fromIntegral n * sxx - sx * sx
    b = (fromIntegral n * sxy - sx * sy) / d

fitSemilog :: [Double] -> Double -> Maybe (Double, Double)
fitSemilog h minP = let pts = [ (fromIntegral n, log p) | (n, p) <- zip [0 :: Int ..] h, p >= minP ]
                    in fitLine (map fst pts) (map snd pts)

step :: World -> World
step w | wPaused w = w
       | otherwise = let (e, r) = exchangeE (wSpeed w) (wEn w, wRng w) in w { wEn = e, wRng = r }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 粒子のマス目
  [ at (ox + gs / 2, oy + gs / 2) (Scale 1 (-1) (pixelImage gridN gridN (fromIntegral cs) cellCol)) ]
  -- ---- 分布（ふつうの目盛り）と片対数
  ++ [ plotAxes g1 "energy n (quanta)" "fraction p(n)" [0, jsRound (fromIntegral nmax / 2), fromIntegral nmax] [0, y1v] ]
  ++ [ Color (rgbaC 255 150 80 170) (rectTL (gX g1 (fromIntegral n - 0.4)) y (gX g1 (fromIntegral n + 0.4) - gX g1 (fromIntegral n - 0.4)) (pY g1 + pH g1 - y))
     | (n, p) <- zip [0 :: Int ..] h, let y = gY g1 p ]
  ++ [ Color (rgbC 130 200 255) (polyline 2 [ (gX g1 (fromIntegral n), gY g1 (geomP n m)) | n <- [0 .. nmax] ]) ]
  ++ [ plotAxes g2 "energy n" "fraction (log)" [0, jsRound (fromIntegral nmax / 2), fromIntegral nmax] [1e-4, 1e-3, 1e-2, 0.1, 1] ]
  ++ [ Color (rgbC 255 150 80) (at (gX g2 (fromIntegral n), gY g2 p) (disc 4)) | (n, p) <- zip [0 :: Int ..] h, p > 0 ]
  ++ [ Color (rgbC 130 200 255) (polyline 2 [ (gX g2 (fromIntegral n), gY g2 (geomP n m)) | n <- [0 .. nmax] ]) ]
  ++ [ Color (rgbC 255 230 120) (polyline 1.5 [ (gX g2 (fromIntegral n), gY g2 (exp (a + b * fromIntegral n))) | n <- [0, nmax] ]) | Just (a, b) <- [fit] ]
  -- ---- 文字
  ++ [ say ("Boltzmann distribution   " ++ show (U.length en) ++ " particles   " ++ show (jsRoundI (m * fromIntegral (U.length en)))
            ++ " quanta (constant)   <n> = " ++ fmt 2 m ++ " -> kT = " ++ fmt 3 kT ++ " e") 20 36 18 (grayC 240)
     , say ("Space pause   Up/Down add/remove energy   Left/Right speed (" ++ show (wSpeed w) ++ " per frame)   R all equal again") 20 62 15 (grayC 175)
     , say (case fit of
              Just (_, b) -> "Semilog slope (least squares) = " ++ fmt 4 b ++ " -> kT = " ++ fmt 3 (-1 / b) ++ " e   (theory -1/kT = "
                             ++ fmt 4 (-1 / kT) ++ ")   largest energy " ++ show mx
              Nothing -> "fitting...") 20 86 15 (rgbC 255 220 120)
     , say "Blue = theory (1 - a) a^n, yellow = fitted line" (pX g2) (pY g2 + pH g2 + 52) 14 (rgbC 130 200 255) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    en = wEn w
    m = meanE en
    nmax = max 12 (ceiling (m * 6))
    h = histogram en nmax
    kT = kTof m
    fit = fitSemilog h (20 / fromIntegral (U.length en))
    mx = U.maximum en
    cs = floor (min (envW env * 0.36 / fromIntegral gridN) ((envH env - 180) / fromIntegral gridN)) :: Int
    gs = fromIntegral (cs * gridN)
    ox = 20
    oy = 120
    cellCol x y =
      let u = clampv 0 1 (fromIntegral (en U.! (y * gridN + x)) / (4 * m + 1))
      in if u < 0.5 then (q (30 + 200 * u), q (40 + 120 * u), q (120 - 60 * u))
                    else (q (130 + 125 * u), q (100 + 155 * (u - 0.5)), q (60 + 140 * (u - 0.5)))
    q v = fromIntegral (max 0 (min 255 (jsRoundI v))) :: Word8
    gx = ox + gs + 70
    gw = envW env - gx - 30
    gh = (envH env - 270) / 2
    y1raw = max (geomP 0 m) (headOr 0 h) * 1.15
    y1v = roundSig 2 y1raw
    g1 = graph gx 120 gw gh (-0.5) (fromIntegral nmax + 0.5) 0 y1raw
    g2 = (graph gx (120 + gh + 80) gw gh (-0.5) (fromIntegral nmax + 0.5) 1e-4 1) { logY = True }

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> addE w
  EventKey (SpecialKey KeyDown) Down _ _ -> subE w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSpeed = min 200000 (wSpeed w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSpeed = max 100 (wSpeed w `div` 2) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> resetAll w
  _ -> w

-- ↑：ランダムな粒子に量子を 1 つずつ（粒子の数の半分だけ）
addE :: World -> World
addE w = w { wEn = U.accum (+) en [ (i, 1) | i <- is ], wRng = r' }
  where
    en = wEn w
    nn = U.length en
    (vs, r') = rndList ((nn + 1) `div` 2) (wRng w)
    is = [ floor (v * fromIntegral nn) | v <- vs ]

-- ↓：ランダムな粒子から量子を 1 つずつ取る（半分の回数まで、平均が 0.6 より大きい間）
subE :: World -> World
subE w = runST $ do
  m <- U.thaw (wEn w)
  let nn = U.length (wEn w)
      go !k !n !s !r
        | not (k < nn * 4 && 2 * n < nn && fromIntegral s / fromIntegral nn > (0.6 :: Double)) = pure r
        | otherwise = do
            let (a, r1) = rnd r
                i = floor (a * fromIntegral nn)
            v <- UM.read m i
            if v > 0 then UM.write m i (v - 1) >> go (k + 1) (n + 1) (s - 1) r1
                     else go (k + 1) n s r1
  r' <- go (0 :: Int) (0 :: Int) (U.sum (wEn w)) (wRng w)
  e <- U.freeze m
  pure w { wEn = e, wRng = r' }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("exchanges keep the total number of quanta (" ++ show sum0 ++ ")", jsRoundI (meanE e1 * 2000) == sum0)
  , ("theory: sum = 1, mean <n> = 3", near ps 1 1e-12 && near pm 3 1e-9)
  , ("all start at n = 3 -> exponential (distance " ++ fmt 3 l1 ++ " < 0.05)", l1 < 0.05)
  , ("semilog slope " ++ fmt 3 fb ++ " = -1/kT = -ln(4/3) = " ++ fmt 3 (-(log (4 / 3))) ++ " (5%)", abs (fb / (-(log (4 / 3))) - 1) < 0.05)
  , ("least squares: points of y = 1 + 2x give a = 1, b = 2", maybe False (\(a, b) -> near a 1 1e-12 && near b 2 1e-12) (fitLine [0, 1, 2, 3] [1, 3, 5, 7]))
  , ("high temperature (<n> = 10): kT ~ <n> + 1/2 (classical equipartition)", near (kTof 10) 10.5 0.01) ]
  where
    sum0 = 3 * 2000
    (e1, r1) = exchangeE 300000 (U.replicate 2000 3, seedRng 90)
    (ps, pm) = foldl' (\(!a, !b) n -> (a + geomP n 3, b + fromIntegral n * geomP n 3)) (0, 0) [0 .. 399]
    rounds (!acc, !st) _ = let st'@(e, _) = exchangeE 20000 st
                           in (zipWith (\a p -> a + p / 10) acc (histogram e 30), st')
    (accH, _) = foldl' rounds (replicate 31 0, (e1, r1)) [1 .. 10 :: Int]
    l1 = foldl' (\a (n, p) -> a + abs (p - geomP n 3)) 0 (zip [0 ..] accH)
    fb = maybe 0 snd (fitSemilog accH 0.005)

-- ---------- やってみよう ----------
-- 1. R を押して、全員同じエネルギーから分布が指数関数になるまでを見よう。何コマくらいかかる？
-- 2. ↑ でエネルギーを足すと、片対数の直線の傾きはどうなる？（温度が上がる = 傾きがゆるやか）
-- 3. エネルギー 0 の粒子がいちばん多いのはなぜ？「場合の数」で考えよう（08 とつながる）
-- 4. gridN を 10 にすると、分布はどれくらいゆれる？
-- 5. 大気の高さと気圧（e^(−mgh/kT)）、化学反応の速さ（e^(−E/kT)）も同じ形

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
