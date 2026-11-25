-- ============================================
-- 68-03 万能近似 ─ 隠れ層が 1 層でも、ニューロンを増やせばどんな連続関数にもいくらでも近づける（シベンコ 1989 ほか）
-- 出力 = b + Σ v_k f(w_k x + c_k)。ここでは学習せず、近似を「組み立てる」（構成的な証明）：
--   ① シグモイドの段：急なシグモイド σ(s(x − m)) は「段」。段の高さを関数の差にして n 段のかいだん
--   ② ReLU の折れ線：ReLU(x − t) は t で折れる。傾きの変化を重みにすれば、n 個で n 区間の折れ線（各点を通る）
--   ③ こぶ：シグモイド 2 個の差 σ(s(x − a)) − σ(s(x − b)) は a〜b の「こぶ」。こぶを並べて関数をつくる
-- 誤差は n を 2 倍にすると、段・こぶは約 1/2、折れ線は約 1/4（なめらかな関数なら）
-- 操作：ドラッグ 関数を描く / 1 2 3 作り方 / ↑↓ ニューロンの数を 2 倍・半分 / ← → 1 つずつ / R 別の関数 / D お手本の関数
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

ns :: Int
ns = 400                   -- 関数を調べる点の数

steep :: Double
steep = 6                  -- ★ シグモイドの急さ（段の幅の何分の 1 で立ち上がるか）

modeNames :: [String]
modeNames = ["sigmoid steps", "ReLU broken line", "sigmoid-pair bumps"]

nList :: [Int]
nList = [2, 4, 8, 16, 32, 64, 128]

-- 1 個のニューロン：v f(w x + c)
data Term = Term { tF :: !Act, tW :: !Double, tC :: !Double, tV :: !Double }

-- 近似：出力 = b + Σ v f(w x + c)
data Approx = Approx { apB :: !Double, apTerms :: ![Term] }

data World = World
  { wTarget :: !Vec                   -- 描いた関数（401 点）
  , wUnits  :: !Int
  , wMode   :: !Int
  , wSeed   :: !Int
  , wErrTab :: ![[Double]]            -- 作り方ごと × nList の誤差（RMS）
  , wLast   :: !(Maybe (Double, Double))
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-03 Universal approximation" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (concat (wErrTab world0)))
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = updateErr (setDefault (World U.empty 8 0 1 [] Nothing []))

demoFn :: Double -> Double
demoFn x = 0.55 * sin (2 * pi * x) + 0.3 * cos (9 * x) * x - 0.1

setDefault :: World -> World
setDefault w = w { wTarget = U.generate (ns + 1) (\i -> demoFn (i2d i / i2d ns)) }

-- ランダムな sin の和（種つき）
setRandom :: World -> World
setRandom w = w { wTarget = U.generate (ns + 1) val }
  where
    (as, fs, phs) = withSeed (40 + wSeed w) $ do
      ks <- mapM (\k -> do g <- randn; u <- rnd; v <- rnd; pure ((0.5 * g) / (i2d k + 1), 1 + i2d k * 2 + 2 * u, 6.28 * v)) [0 .. 3 :: Int]
      pure (map (\(a, _, _) -> a) ks, map (\(_, f, _) -> f) ks, map (\(_, _, p) -> p) ks)
    val i = let x = i2d i / i2d ns in clampv (foldl' (\v (a, f, ph) -> v + a * sin (f * x * 3.14 + ph)) 0 (zip3 as fs phs)) (-1) 1

-- 描いた関数の x での値（点の間は直線でつなぐ）
tAt :: Vec -> Double -> Double
tAt fn x = let u = clampv x 0 1 * i2d ns
               i = min (ns - 1) (floor u)
               t = u - i2d i
           in fn U.! i * (1 - t) + fn U.! (i + 1) * t

-- ★ 近似を組み立てる
buildApprox :: (Double -> Double) -> Int -> Int -> Approx
buildApprox fn n kind = case kind of
  0 -> let s = steep * 2 * i2d n             -- かいだん：区間の中央 m_k で段、高さ = 関数の差
       in Approx (fn 0) [ Term Sigmoid s (-s * ((x0 + x1) / 2)) (fn x1 - fn x0) | k <- [1 .. n], let x0 = (i2d k - 1) / i2d n, let x1 = i2d k / i2d n ]
  1 -> let slopes = [ (fn ((i2d k + 1) / i2d n) - fn (i2d k / i2d n)) * i2d n | k <- [0 .. n - 1] ]   -- 折れ線：t_k = k/n で傾きが変わる
       in Approx (fn 0) [ Term Relu 1 (-(i2d k / i2d n)) (sl - prev) | (k, sl, prev) <- zip3 [0 .. n - 1] slopes (0 : slopes) ]
  _ -> let nb = max 1 (n `div` 2)            -- こぶ：2 個 1 組で n/2 個
           s = steep * 2 * i2d nb
       in Approx 0 (concat [ [Term Sigmoid s (-s * a) h, Term Sigmoid s (-s * b) (-h)]
                           | k <- [0 .. nb - 1], let a = i2d k / i2d nb, let b = (i2d k + 1) / i2d nb, let h = fn ((a + b) / 2) ])

evalTerm :: Term -> Double -> Double
evalTerm t x = tV t * actF (tF t) (tW t * x + tC t)

evalApprox :: Approx -> Double -> Double
evalApprox ap x = foldl' (\o t -> o + evalTerm t x) (apB ap) (apTerms ap)

-- 2 乗平均の誤差（RMS）と最大の誤差
approxErr :: (Double -> Double) -> Approx -> (Double, Double)
approxErr fn ap = (sqrt (s / i2d (ns + 1)), mx)
  where
    (s, mx) = foldl' acc (0, 0) [0 .. ns]
    acc (a, m) i = let x = i2d i / i2d ns
                       e = evalApprox ap x - fn x
                   in (a + e * e, max m (abs e))

updateErr :: World -> World
updateErr w = let fn = tAt (wTarget w) in w { wErrTab = [ [ fst (approxErr fn (buildApprox fn n k)) | n <- nList ] | k <- [0 .. 2] ] }

graphMain :: Env -> Graph
graphMain env = let lw = envW env * 0.58 in Graph 60 100 (lw - 60) ((envH env - 230) * 0.6) 0 1 (-1.2) 1.2

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Universal approximation: " ++ at' modeNames (wMode w) "" ++ "   " ++ show (length (apTerms ap)) ++ " neurons   error RMS " ++ fmtN rms 4 ++ "   max " ++ fmtN mxe 3)
             "drag draw a function   1 2 3 construction   Up/Down neurons x2 / half   Left/Right one by one   R another function   D example"
  , plotAxes g "x" "magenta = drawn function   white = sum of the neurons" [0, 0.5, 1] [-1, 0, 1]
  , plotXY g xs (map fn xs) (col mag) 4
  , plotXY g xs (map (evalApprox ap) xs) (col ink) 2
  -- ニューロン 1 個ずつの出力（下の段）
  , plotAxes g2 "x" ("each neuron's output v_k f(w_k x + c_k) (" ++ (if wMode w == 1 then "pieces of the broken line" else if wMode w == 0 then "steps" else "bumps = difference of 2") ++ ")") [0, 0.5, 1] [-1, 0, 1]
  , Pictures [ plotXY g2 xs [ (evalTerm t x + (if wMode w == 2 then evalTerm t2 x else 0)) * sc | x <- xs ] (colA 170 (at' [sky, lime, amber] (k `mod` 3) sky)) 1.5
             | (k, t, t2) <- zip3 [0 :: Int ..] showT (drop 1 showT ++ [Term Relu 0 0 0]), not (wMode w == 2 && odd k) ]
  -- ---- 右：ニューロンの数と誤差
  , plotAxes g3 "number of neurons n (doubling)" "log10 error (RMS)" [] [-4, -3, -2, -1, 0]
  , Pictures [ sayC (show n) (gX g3 (log2 n)) (grY g3 + grH g3 + 18) 14 (col sub) | n <- nList ]
  , Pictures [ Pictures (plotXY g3 (map log2 nList) (map (logBase 10 . max 1e-6) errs) (col c) (if k == wMode w then 4 else 1.5)
                         : [ Color (col c) (circleAt (gX g3 (log2 n)) (gY g3 (logBase 10 (max 1e-6 e))) (if k == wMode w then 8 else 5)) | (n, e) <- zip nList errs ])
             | (k, errs, c) <- zip3 [0 ..] (wErrTab w) [mag, sky, lime] ]
  , legend (grX g3 + 12) (grY g3 + grH g3 - 60) (zip [mag, sky, lime] modeNames)
  , say ("n = 16 -> 32: steps / " ++ fmtN s16 1 ++ ", broken line / " ++ fmtN r16 1) rx ty 14 (col ink)
  , Pictures [ say ln rx (ty + 24 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] notes ]
  , drawSelfTest env (wTest w) ]
  where
    fn = tAt (wTarget w)
    ap = buildApprox fn (wUnits w) (wMode w)
    (rms, mxe) = approxErr fn ap
    g = graphMain env
    xs = [ i2d i / 300 | i <- [0 .. 300 :: Int] ]
    g2 = Graph 60 (grY g + grH g + 50) (grW g) ((envH env - 230) * 0.3) 0 1 (-1) 1
    showT = take 64 (apTerms ap)
    sc = if wMode w == 1 then 1 / maximum (1e-9 : map (abs . tV) (apTerms ap)) else 1
    rx = envW env * 0.58 + 60
    g3 = Graph rx 110 (envW env - rx - 40) (min 360 (envH env - 330)) 1 7 (-4) 0
    log2 n = logBase 2 (i2d n)
    errAt k i = at' (at' (wErrTab w) k []) i 0
    r16 = errAt 1 3 / max 1e-12 (errAt 1 4)
    s16 = errAt 0 3 / max 1e-12 (errAt 0 4)
    ty = grY g3 + grH g3 + 60
    notes = [ "steps: a height error (slope x width)", "  remains -> about 1/2 each time"
            , "broken line: only the bend (f'' x width^2)", "  remains -> about 1/4 each time"
            , "corners / zigzags: error falls more slowly"
            , "theorem: 'as close as you like' - not how", "many neurons, nor whether learning finds them"
            , "(output = b + sum of v_k f(w_k x + c_k))" ]

-- 前の位置との間を直線でうめる
dragTo :: Env -> (Float, Float) -> World -> World
dragTo env pos w =
  let (mx, my) = toP5 env pos
      g = graphMain env
      x = d2 ((mx - grX g) / grW g)
      y0 = 1.2 - d2 ((my - grY g) / grH g) * 2.4
  in if x < 0 || x > 1 || my < grY g || my > grY g + grH g then w { wLast = Nothing } else
     let y = clampv y0 (-1.15) 1.15
         (p0, p1) = fromMaybe (x, y) (wLast w)
         i0 = jsRound (min p0 x * i2d ns)
         i1 = jsRound (max p0 x * i2d ns)
         upd = [ (i, p1 + (y - p1) * clampv t 0 1) | i <- [i0 .. i1], let t = if abs (x - p0) > 1e-9 then (i2d i / i2d ns - p0) / (x - p0) else 1 ]
     in w { wTarget = wTarget w U.// upd, wLast = Just (x, y) }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventMotion pos | envDown env -> dragTo env pos w
  EventKey (MouseButton LeftButton) Up _ _ -> updateErr w { wLast = Nothing }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> w { wMode = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> updateErr (setRandom w { wSeed = wSeed w + 1 })
    | c `elem` "dD" -> updateErr (setDefault w)
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wUnits = min 128 (wUnits w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wUnits = max 1 (wUnits w `div` 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wUnits = min 128 (wUnits w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wUnits = max 1 (wUnits w - 1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ニューロン 4 個・8 個・16 個で、3 つの作り方を見比べる。どれがいちばん少ない数で近い？
-- 2. 階段のような関数（とびのある関数）を描く。シグモイドの段と ReLU の折れ線、どちらが得意？
-- 3. steep を 1 や 30 にする。ゆるい段だと何が起きる？
-- 4. 学習（04〜05 の誤差逆伝播）でも同じくらい少ない数で近づけるか、考えてみよう

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("ニューロン 4 → 8 → 16 → 32 で誤差が下がる（" ++ intercalate "、" msg ++ "）", dec)
  , ("ReLU 10 個の折れ線は 11 個の節点で関数とぴったり一致（ずれ " ++ fmtE knotErr ++ "）", knotErr < 1e-12)
  , ("折れ線の誤差は n を 2 倍で約 1/4（比 " ++ fmtN r 2 ++ "）", r > 3.2 && r < 4.8)
  , ("シグモイド 2 個の差は「こぶ」：中で " ++ fmtN (bump 0.5) 4 ++ "、外で " ++ fmtE (bump 0.2), bump 0.5 > 0.99 && bump 0.2 < 1e-3 && bump 0.8 < 1e-3)
  , ("シグモイド 64 個の段で、なめらかな関数の最大誤差 < 0.1", snd (approxErr fn (buildApprox fn 64 0)) < 0.1) ]
  where
    fn x = sin (2 * pi * x) + 0.5 * cos (5 * x)
    errs = [ [ fst (approxErr fn (buildApprox fn n k)) | n <- [4, 8, 16, 32] ] | k <- [0 .. 2] ]
    dec = and [ and (zipWith (>) e (drop 1 e)) | e <- errs ]
    msg = [ fmtN (at' e 0 0) 3 ++ "→" ++ fmtN (at' e 3 0) 4 | e <- errs ]
    ap10 = buildApprox fn 10 1
    knotErr = maximum [ abs (evalApprox ap10 (i2d k / 10) - fn (i2d k / 10)) | k <- [0 .. 10 :: Int] ]
    r = fst (approxErr fn (buildApprox fn 16 1)) / fst (approxErr fn (buildApprox fn 32 1))
    bump x = sigm (200 * (x - 0.4)) - sigm (200 * (x - 0.6))

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

-- ---- 活性化関数とその微分
data Act = Step | Sigmoid | Tanh | Relu | Softplus deriving (Eq, Show)

actNames :: [Act]
actNames = [Step, Sigmoid, Tanh, Relu, Softplus]

actF :: Act -> Double -> Double
actF a z = case a of
  Step -> if z >= 0 then 1 else 0
  Sigmoid -> sigm z
  Tanh -> tanh z
  Relu -> if z > 0 then z else 0
  Softplus -> if z > 30 then z else log (1 + exp z)   -- softplus = log(1 + eᶻ)

-- 微分 f'(z)（ステップは 0、ReLU の 0 での微分は 0 とする）
actD :: Act -> Double -> Double
actD a z = case a of
  Step -> 0
  Sigmoid -> let s = sigm z in s * (1 - s)
  Tanh -> let t = tanh z in 1 - t * t
  Relu -> if z > 0 then 1 else 0
  Softplus -> sigm z                          -- softplus の微分 = シグモイド

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
