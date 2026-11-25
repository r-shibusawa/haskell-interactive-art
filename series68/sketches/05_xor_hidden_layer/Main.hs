-- ============================================
-- 68-05 隠れ層が XOR を解く ─ 入力 2・隠れ 2（tanh）・出力 1（シグモイド）の小さなネット（2-2-1）
-- XOR（(0,1) と (1,0) が 1、(0,0) と (1,1) が 0）は 1 本の直線では分けられない（00・02）。
-- でも隠れ層は 4 点を新しい場所 h = (h1, h2) に写す。真ん中のパネル（隠れ層の空間）で 4 点が直線で分けられるようになれば、
-- 出力のニューロン（ただの直線 = パーセプトロン）で答えられる。学習は交差エントロピーの勾配降下（誤差逆伝播は 04 と同じ）
-- 初期値によっては「2 点だけ正しい」谷（損失 ≈ 0.347 = ln2 / 2）にはまって抜け出せない → 種を変えて試す
-- 操作：← → 種（初期値）を変える / スペース 一時停止 / F 速さ / R 同じ種でやり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

lr :: Double
lr = 1.0                   -- ★ 学習率

xorX :: [(Double, Double)]
xorX = [(0, 0), (0, 1), (1, 0), (1, 1)]

xorY :: [Int]
xorY = [0, 1, 1, 0]

speedList :: [Int]
speedList = [1, 4, 20]

-- パラメータ（9 個）：w1[j][k] = 番号 2j + k、b1[j] = 4 + j、w2[j] = 6 + j、b2 = 8
type Prm = Vec

w1Of :: Prm -> Int -> Int -> Double
w1Of p j k = p U.! (2 * j + k)

b1Of, w2Of :: Prm -> Int -> Double
b1Of p j = p U.! (4 + j)
w2Of p j = p U.! (6 + j)

b2Of :: Prm -> Double
b2Of p = p U.! 8

-- 種ごとに 3000 歩で解けたか
data SeedStats = SeedStats { ssOk :: !Int, ssList :: ![Bool] }

data World = World
  { wSeed    :: !Int
  , wPrm     :: !Prm
  , wSteps   :: !Int
  , wLossLog :: ![Double]          -- 新しい順（1500 個まで）
  , wPaused  :: !Bool
  , wSpd     :: !Int
  , wInImg   :: !Picture           -- 入力の空間の色（4 フレームに 1 回描き直す）
  , wHidImg  :: !Picture           -- 隠れ層の空間の色
  , wPainted :: !Bool
  , wStats   :: !SeedStats
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-05 XOR with a hidden layer" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      let st = surveySeeds 40 3000
      _ <- evaluate (ssOk st)
      pure world0 { wStats = st, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart (World 2 (U.replicate 9 0) 0 [] False 1 Blank Blank False (SeedStats 0 []) [])

initXor :: Int -> Prm
initXor seed = withSeed seed (U.fromList <$> mapM (const randn) [1 .. 9 :: Int])

hiddenOf :: Prm -> (Double, Double) -> (Double, Double)
hiddenOf p (x0, x1) = (hj 0, hj 1)
  where hj j = tanh (w1Of p j 0 * x0 + w1Of p j 1 * x1 + b1Of p j)

outFromH :: Prm -> (Double, Double) -> Double
outFromH p (h0, h1) = sigm (w2Of p 0 * h0 + w2Of p 1 * h1 + b2Of p)

xorLoss :: Prm -> Double
xorLoss p = foldl' (\s (x, y) -> let o = clampv (outFromH p (hiddenOf p x)) 1e-15 (1 - 1e-15)
                                 in s - (if y == 1 then log o else log (1 - o))) 0 (zip xorX xorY) / 4

-- ★ 誤差逆伝播：出力の誤差 d = (o − y)/4 → w2, b2。隠れへは d × w2 × (1 − h²)
xorGrad :: Prm -> Prm
xorGrad p = runST $ do
  g <- MU.replicate 9 0
  forM_ (zip xorX xorY) $ \(x@(x0, x1), y) -> do
    let (h0, h1) = hiddenOf p x
        d = (outFromH p (h0, h1) - i2d y) / 4
    MU.modify g (+ d) 8
    forM_ [(0, h0), (1, h1)] $ \(j, hj) -> do
      MU.modify g (+ d * hj) (6 + j)
      let dz = d * w2Of p j * (1 - hj * hj)
      MU.modify g (+ dz) (4 + j)
      MU.modify g (+ dz * x0) (2 * j)
      MU.modify g (+ dz * x1) (2 * j + 1)
  U.freeze g

xorStep :: Prm -> Double -> Prm
xorStep p rate = U.imap upd p
  where
    g = xorGrad p
    upd i v
      | i == 8 = clampv (v - rate * g U.! 8) (-60) 60
      | i >= 6 = clampv (v - rate * g U.! i) (-60) 60
      | otherwise = v - rate * g U.! i

-- 損失 < 0.01 になるまで（または maxSteps 歩まで）
trainXor :: Int -> Int -> (Prm, Int, Double)
trainXor seed maxSteps = go (initXor seed) 0
  where go p k = let l = xorLoss p
                 in if k >= maxSteps || l < 0.01 then (p, k, l) else go (xorStep p lr) (k + 1)

surveySeeds :: Int -> Int -> SeedStats
surveySeeds n maxSteps = let list = [ let (_, _, l) = trainXor s maxSteps in l < 0.01 | s <- [1 .. n] ]
                         in SeedStats (length (filter id list)) list

-- 4 点が直線で分けられるか（バイアスつきパーセプトロンが 2000 周以内に止まるか）
linSep4 :: [(Double, Double)] -> [Int] -> Bool
linSep4 hs ys = go 0 (0, 0, 0)
  where
    go :: Int -> (Double, Double, Double) -> Bool
    go e w | e >= 2000 = False
           | otherwise = let (w', bad) = foldl' visit (w, 0 :: Int) (zip hs ys) in bad == 0 || go (e + 1) w'
    visit ((a, b, c), bad) ((h0, h1), yv) =
      let y = if yv /= 0 then 1 else -1
      in if y * (a * h0 + b * h1 + c * 1) <= 0 then ((a + y * h0, b + y * h1, c + y * 1), bad + 1) else ((a, b, c), bad)

restart :: World -> World
restart w = let p = initXor (wSeed w) in w { wPrm = p, wSteps = 0, wLossLog = [xorLoss p] }

-- 出力の確率 → 色（0.5 の境目で切りかわる）
probCol :: Double -> RGB
probCol p = let t = 2 * p - 1 in if t >= 0 then mixCol panel mag (0.12 + 0.5 * t) else mixCol panel lime (0.12 - 0.5 * t)

step :: Env -> Float -> World -> IO World
step env _ w0 = pure (paint w1)
  where
    w1 = if wPaused w0 then w0 else run (at' speedList (wSpd w0) 1) w0
    run 0 w = w
    run k w | wSteps w >= 20000 = w
            | otherwise = let p = xorStep (wPrm w) lr
                              s = wSteps w + 1
                              lg = if s `mod` 4 == 0 then take 1500 (xorLoss p : wLossLog w) else wLossLog w
                          in run (k - 1 :: Int) w { wPrm = p, wSteps = s, wLossLog = lg }
    paint w
      | envFrame env `mod` 4 == 1 || not (wPainted w) =
          let p = wPrm w
          in w { wInImg = paintImage 90 90 (\u v -> probCol (outFromH p (hiddenOf p (-0.5 + 2 * u, -0.5 + 2 * v))))
               , wHidImg = paintImage 90 90 (\u v -> probCol (outFromH p (-1.1 + 2.2 * u, -1.1 + 2.2 * v)))
               , wPainted = True }
      | otherwise = w

-- w1 u + w2 v + c = 0 をパネルの中に（toVal：0〜1 → パネルの値）
drawLineIn :: Float -> Float -> Float -> Double -> Double -> Double -> (Double -> Double) -> RGB -> Picture
drawLineIn bx by sz a b c toVal cl =
  let pts0 = [ (x, -(a * x + c) / b) | k <- [0, 1], let x = toVal k, abs b > 1e-9 ]
      pts = if length pts0 < 2 && abs a > 1e-9 then let x = -c / a in [(x, toVal 0), (x, toVal 1)] else pts0
      lo = toVal 0
      spn = toVal 1 - lo
      scr (p0, p1) = (bx + f2 ((p0 - lo) / spn) * sz, by + f2 (1 - (p1 - lo) / spn) * sz)
  in case pts of
       [p, q] -> case clipSeg bx by sz sz (scr p) (scr q) of
         Just (s, e) -> Color (col cl) (lineW 2.5 s e)
         Nothing -> Blank
       _ -> Blank

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("XOR with a hidden layer (2-2-1): seed " ++ show (wSeed w) ++ "   " ++ show (wSteps w) ++ " steps   loss " ++ fmtN l 4
              ++ (if solved then "  -> all 4 correct" else if l > 0.3 && wSteps w > 3000 then "  -> stuck in a valley (Left/Right: other seed)" else ""))
             ("Left/Right seed (initial values)   Space " ++ (if wPaused w then "resume" else "pause") ++ "   F speed x" ++ show (at' speedList (wSpd w) 1)
              ++ "   R restart   |   " ++ show (ssOk st) ++ " of 40 seeds solve it within 3000 steps")
  , imageBox ax y0 sz sz 90 90 (wInImg w), imageBox bxx y0 sz sz 90 90 (wHidImg w)
  , Pictures [ drawLineIn ax y0 sz (w1Of p j 0) (w1Of p j 1) (b1Of p j) (\u -> -0.5 + 2 * u) c | (j, c) <- [(0, sky), (1, amber)] ]
  , drawLineIn bxx y0 sz (w2Of p 0) (w2Of p 1) (b2Of p) (\u -> -1.1 + 2.2 * u) ink
  , Pictures [ let (pa, pb) = toA x
                   (qa, qb) = toB h
                   cl = col (if y == 1 then mag else lime)
                   lb = "(" ++ show (round (fst x) :: Int) ++ "," ++ show (round (snd x) :: Int) ++ ")"
               in Pictures [ Color (grayC 20) (circleAt pa pb 21), Color cl (circleAt pa pb 18)
                           , Color (grayC 20) (circleAt qa qb 21), Color cl (circleAt qa qb 18)
                           , say lb (pa + 10) (pb - 6) 14 (col ink), say lb (qa + 10) (qb - 6) 14 (col ink) ]
             | (x, h, y) <- zip3 xorX hs xorY ]
  , say "input space (x1, x2): color = net output" ax (y0 - 10) 15 (col ink)
  , say "hidden space (h1, h2): where the 4 points go" bxx (y0 - 10) 15 (col ink)
  , say "blue / orange lines = borders of hidden" ax (y0 + sz + 22) 14 (col sub)
  , say "neurons 1, 2 (tanh = 0): 2 lines cut the plane" ax (y0 + sz + 42) 14 (col sub)
  , say "white line = border of the output neuron" bxx (y0 + sz + 22) 14 (col sub)
  , say ("a line here solves XOR" ++ (if linSep4 hs xorY then "   OK separable now" else "   x not separable yet")) bxx (y0 + sz + 42) 14 (col sub)
  -- ---- 右：損失
  , plotAxes g "steps (x4)" "log10 loss (cross-entropy)" [0, grX1 g] [-3, -2, -1, 0]
  , Color (col wire) (Line [(grX g, gY g valley), (grX g + grW g, gY g valley)])
  , say "valley 0.347 = (ln 2)/2" (grX g + 4) (gY g valley - 4) 14 (col sub)
  , plotXY g (map i2d [0 .. length logs - 1]) (map (\v -> logBase 10 (max 1e-6 v)) logs) (col amber) 2
  , say ("w1 = [" ++ fmtN (w1Of p 0 0) 2 ++ ", " ++ fmtN (w1Of p 0 1) 2 ++ " | " ++ fmtN (w1Of p 1 0) 2 ++ ", " ++ fmtN (w1Of p 1 1) 2 ++ "]") rx ty 14 (col ink)
  , say ("b1 = [" ++ fmtN (b1Of p 0) 2 ++ ", " ++ fmtN (b1Of p 1) 2 ++ "]   b2 = " ++ fmtN (b2Of p) 2) rx (ty + 22) 14 (col ink)
  , say ("w2 = [" ++ fmtN (w2Of p 0) 2 ++ ", " ++ fmtN (w2Of p 1) 2 ++ "]") rx (ty + 44) 14 (col ink)
  -- 種ごとの成否
  , say "seeds 1-40 in 3000 steps:" rx (ty + 76) 14 (col sub)
  , say "dot = solved, ring = valley" rx (ty + 94) 14 (col sub)
  , Pictures [ let cx = rx + 8 + i2f (i `mod` 20) * 16
                   cy = ty + 112 + i2f (i `div` 20) * 18
                   cur = i + 1 == wSeed w
               in Pictures [ if okv then Color (col lime) (circleAt cx cy 11) else Color (col mag) (ringAt cx cy 11 1.5)
                           , if cur then Color (col ink) (ringAt cx cy 13 2.5) else Blank ]
             | (i, okv) <- zip [0 :: Int ..] (ssList st) ]
  , drawSelfTest env (wTest w) ]
  where
    p = wPrm w
    st = wStats w
    l = xorLoss p
    hs = map (hiddenOf p) xorX
    solved = and [ (outFromH p h > 0.5) == (y == 1) | (h, y) <- zip hs xorY ]
    sz = min ((envW env - 140) / 3) (envH env - 260)
    y0 = 110
    ax = 40
    bxx = 40 + sz + 50
    toA (x0, x1) = (ax + f2 ((x0 + 0.5) / 2) * sz, y0 + f2 (1 - (x1 + 0.5) / 2) * sz)
    toB (h0, h1) = (bxx + f2 ((h0 + 1.1) / 2.2) * sz, y0 + f2 (1 - (h1 + 1.1) / 2.2) * sz)
    rx = bxx + sz + 50
    logs = reverse (wLossLog w)
    g = Graph rx y0 (envW env - rx - 40) (sz * 0.62) 0 (max 10 (i2d (length logs - 1))) (-3) 0.5
    valley = logBase 10 0.347
    ty = grY g + grH g + 60

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wSeed = wSeed w `mod` 40 + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wSeed = if wSeed w == 1 then 40 else wSeed w - 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> w { wSpd = (wSpd w + 1) `mod` length speedList }
    | c `elem` "rR" -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 種 2 で、隠れ層の空間の 4 点がどう動いて直線で分けられるようになるかを見る
-- 2. ← → で谷にはまる種（輪）を探す。そのとき隠れ層の空間で 4 点はどうなっている？
-- 3. lr を 0.3 や 3 にすると、解ける種の数は変わる？
-- 4. 隠れ層を 3 個にしたら谷にはまりにくくなる？（initXor と hiddenOf を書きかえる）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("種 2：" ++ show k2 ++ " 歩で損失 < 0.01（" ++ fmtE l2 ++ "）", l2 < 0.01)
  , ("学習後、隠れ層の空間の 4 点は直線で分けられる", linSep4 (map (hiddenOf p2) xorX) xorY)
  , ("入力の空間の XOR 4 点は直線で分けられない", not (linSep4 xorX xorY))
  , ("種 1：谷にはまる（3000 歩でも損失 " ++ fmtN l1 3 ++ " ≈ (ln 2)/2、全問正解できない）", abs (l1 - log 2 / 2) < 0.01 && not fOk)
  , ("逆伝播の勾配 = 中心差分（相対誤差 " ++ fmtE worst ++ "）", worst < 1e-6) ]
  where
    (p2, k2, l2) = trainXor 2 3000
    (p1, _, l1) = trainXor 1 3000
    fOk = and [ (outFromH p1 (hiddenOf p1 x) > 0.5) == (y == 1) | (x, y) <- zip xorX xorY ]
    p7 = initXor 7
    g7 = xorGrad p7
    hh = 1e-6
    probe i = let a = p7 U.// [(i, p7 U.! i + hh)]
                  b = p7 U.// [(i, p7 U.! i - hh)]
              in (xorLoss a - xorLoss b) / (2 * hh)
    worst = foldl' (\m i -> max m (relErr (g7 U.! i) (probe i))) 0 [0 .. 8]

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

-- ---- 画像の道具：幅 iw × 高さ ih の画像の全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((i2d px + 0.5) / i2d iw) (1 - (i2d py + 0.5) / i2d ih)
  in (w8 r, w8 g, w8 b))

-- マス目の画像：fn i j（i = 左から、j = 上から）→ (r, g, b)
gridImage :: Int -> Int -> (Int -> Int -> RGB) -> Picture
gridImage iw ih fn = pixelImage iw ih 1 (\i j -> let (r, g, b) = fn i j in (w8 r, w8 g, w8 b))

w8 :: Double -> Word8                         -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）
w8 v = fromIntegral (c8 v)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / i2f iw) (negate h / i2f ih) pic)

mixCol :: RGB -> RGB -> Double -> RGB
mixCol (a0, a1, a2) (b0, b1, b2) t0 = let t = clampv t0 0 1 in (a0 + (b0 - a0) * t, a1 + (b1 - a1) * t, a2 + (b2 - a2) * t)

divCol :: Double -> RGB                       -- −1〜1 → ライム・暗い・マゼンタ
divCol v = if v >= 0 then mixCol panel mag v else mixCol panel lime (negate v)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
