-- ============================================
-- 68-09 正則化 ─ 少ないデータ（30 点、ラベルに少しまちがいあり）に、大きめのネット（隠れ 32 個）をそのまま学ばせると、
-- まちがいの点まで丸く囲む「過学習」になる。3 つのふせぎ方を同時に学習して並べる：
--   ① 重み減衰：損失に (λ/2)‖w‖² を足す → 重みが小さく、境界がなめらか
--   ② 早期終了：正則化なしで学習し、検証用の別のデータの誤差がいちばん小さかった時点で止める（その時の重みを保存）
--   ③ ドロップアウト：学習のたびに隠れニューロンを確率 p で休ませる（残りは 1/(1 − p) 倍）→ 1 つの素子に頼れない
-- 下のグラフ：訓練データの損失（細い）とテストデータの損失（太い）。テストの損失が上がり始めたら過学習
-- （同じ形の小さなネットを「道具」として使う例：Q 学習 series63/11、アンサンブルで自信を測る series64/13）
-- 操作：↑↓ 重み減衰 λ / ← → ドロップアウト p / E 早期終了の重み ⇔ 最後の重み / N 新しいデータ / スペース 一時停止 / R やり直す
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

nTr, nVal, nTe, hidden :: Int
nTr = 30                   -- ★ 訓練・検証・テストの数、隠れ層の大きさ
nVal = 30
nTe = 400
hidden = 32

flipP, lrR :: Double
flipP = 0.1                -- ★ ラベルのまちがいの割合
lrR = 0.005                -- ★ 学習率（Adam）

maxSteps, stepsPerFrame :: Int
maxSteps = 4000            -- ★ 学習の長さ
stepsPerFrame = 8

wdList, dropList :: [Double]
wdList = [0.001, 0.002, 0.003, 0.004, 0.005, 0.01, 0.03]
dropList = [0.2, 0.35, 0.5, 0.65]

-- データ：点 x と クラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Vec), dsY :: !(U.Vector Int) }

-- 早期終了：検証の損失がいちばん小さかった時点の重み
data Best = Best { bVal :: !Double, bStep :: !Int, bNet :: !MLP }

data World = World
  { wWd      :: !Int
  , wDrop    :: !Int
  , wSeed    :: !Int
  , wTrain   :: !DataSet
  , wValid   :: !DataSet
  , wTestD   :: !DataSet
  , wNets    :: ![MLP]                -- ① 重み減衰 ② 正則化なし ③ ドロップアウト
  , wHist    :: ![[(Int, Double, Double)]]   -- ネットごとに（歩数, 訓練の損失, テストの損失）新しい順
  , wStep    :: !Int
  , wBest    :: !Best
  , wShowBest :: !Bool
  , wPaused  :: !Bool
  , wBufs    :: ![Picture]            -- 3 つの境界の絵（10 フレームに 1 回ずつ描き直す）
  , wBufOk   :: ![Bool]
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-09 Regularization" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (mlpLoss (head' (wNets world0)) (dsX (wTestD world0)) (dsY (wTestD world0)))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }
  where head' = headOr (copyMLP (bNet (wBest world0)))

world0 :: World
world0 = restartNets (newData w)
  where e = DataSet V.empty U.empty
        net0 = MLP 2 1 (U.replicate 5 0) 0 Nothing
        w = World 3 2 2 e e e [] [] 0 (Best posInf 0 net0) True False [Blank, Blank, Blank] [False, False, False] (rngOf 1) []

-- 2 つの三日月（[−1, 1]² の中）。flip の割合でラベルを入れかえる
moons :: Int -> Double -> R DataSet
moons n flipR = do
  ps <- mapM one [0 .. n - 1]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))
  where
    one i = do
      let c = i `mod` 2
      u <- rnd
      let t = pi * u
          x0 = if c == 1 then 1 - cos t else cos t
          y0 = if c == 1 then 0.5 - sin t else sin t
      gx <- randn
      let x = (x0 - 0.5) * 0.65 + 0.08 * gx
      gy <- randn
      let y = (y0 - 0.25) * 0.75 + 0.08 * gy
      f <- rnd
      pure (U.fromList [x, y], if f < flipR then 1 - c else c)

newData :: World -> World
newData w = let (tr, va, te) = withSeed (90 + wSeed w) ((,,) <$> moons nTr flipP <*> moons nVal flipP <*> moons nTe flipP)
            in w { wTrain = tr, wValid = va, wTestD = te }

-- 3 つとも同じ初期値から
restartNets :: World -> World
restartNets w =
  let nets = replicate 3 (withSeed 1 (makeMLP 2 hidden 9 1))
  in w { wNets = nets, wHist = [[], [], []], wStep = 0, wBest = Best posInf 0 (copyMLP (nets !!! 1)), wRng = rngOf 1234 }

(!!!) :: [MLP] -> Int -> MLP
xs !!! i = at' xs i (MLP 2 1 (U.replicate 5 0) 0 Nothing)

trainChunk :: Int -> World -> World
trainChunk k w =
  let tr = wTrain w
      [n0, n1, n2] = take 3 (wNets w ++ repeat (wNets w !!! 0))
      ((a, b, c), g) = runR (do a' <- mlpTrain n0 (dsX tr) (dsY tr) k lrR (at' wdList (wWd w) 0) 0
                                b' <- mlpTrain n1 (dsX tr) (dsY tr) k lrR 0 0
                                c' <- mlpTrain n2 (dsX tr) (dsY tr) k lrR 0 (at' dropList (wDrop w) 0)
                                pure (a', b', c')) (wRng w)
      st = wStep w + k
      v = mlpLoss b (dsX (wValid w)) (dsY (wValid w))
      best = if v < bVal (wBest w) then Best v st (copyMLP b) else wBest w
      rec m = (st, mlpLoss m (dsX tr) (dsY tr), mlpLoss m (dsX (wTestD w)) (dsY (wTestD w)))
  in w { wNets = [a, b, c], wStep = st, wBest = best, wRng = g
       , wHist = zipWith (:) (map rec [a, b, c]) (wHist w ++ repeat []) }

accOf :: MLP -> DataSet -> Double
accOf m d = let n = V.length (dsX d)
                ok = length [ () | i <- [0 .. n - 1], (if mlpOut m (dsX d V.! i) > 0.5 then 1 else 0) == dsY d U.! i ]
            in if n == 0 then 0 else i2d ok / i2d n

weightNorm :: MLP -> Double
weightNorm m = let p = mlP m
                   nW1 = mlH m * mlIn m
                   oW2 = nW1 + mlH m
                   s1 = U.foldl' (\s v -> s + v * v) 0 (U.slice 0 nW1 p)
               in sqrt (U.foldl' (\s v -> s + v * v) s1 (U.slice oW2 (mlH m) p))

shownNets :: World -> [MLP]
shownNets w = [wNets w !!! 0, if wShowBest w then bNet (wBest w) else wNets w !!! 1, wNets w !!! 2]

-- 境界の絵（64 × 64）：クラス 1 の確率 → マゼンタ、クラス 0 → ライム
paintNet :: MLP -> Picture
paintNet m = paintImage 64 64 (\u v -> let p = mlpOut m (U.fromList [-1.2 + 2.4 * u, -1.2 + 2.4 * v])
                                           t = 2 * p - 1
                                       in if t >= 0 then mixCol panel mag (0.1 + 0.45 * t) else mixCol panel lime (0.1 - 0.45 * t))

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w1 { wBufs = bufs, wBufOk = replicate 3 True }
  where
    w1 = if not (wPaused w0) && wStep w0 < maxSteps then trainChunk stepsPerFrame w0 else w0
    bufs = [ if envFrame env `mod` 10 == k || not ok then paintNet m else b
           | (k, m, b, ok) <- zip4' [0 ..] (shownNets w1) (wBufs w1) (wBufOk w1) ]
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Regularization: " ++ show (wStep w) ++ " / " ++ show maxSteps ++ " steps (train " ++ show nTr ++ " pts, " ++ show (jsRound (flipP * 100)) ++ "% wrong labels, " ++ show hidden ++ " hidden)")
             ("Up/Down weight decay lambda = " ++ jsNum (at' wdList (wWd w) 0) ++ "   Left/Right dropout p = " ++ jsNum (at' dropList (wDrop w) 0)
              ++ "   E early-stop weights <-> last weights (" ++ (if wShowBest w then "early stop" else "last") ++ ")   N new data   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R restart")
  , Pictures (zipWith3 panelK [0 ..] (shownNets w) (wBufs w))
  , legend (envW env - 300) (110 + sz + 76) [(sky, "training loss"), (amber, "test loss")]
  , drawSelfTest env (wTest w) ]
  where
    pw = (envW env - 120) / 3
    sz = min pw (envH env * 0.45)
    titles = [ "(1) weight decay lambda = " ++ jsNum (at' wdList (wWd w) 0), "(2) early stopping (no reg.)", "(3) dropout p = " ++ jsNum (at' dropList (wDrop w) 0) ]
    panelK :: Int -> MLP -> Picture -> Picture
    panelK k m buf =
      let x0 = 40 + i2f k * (pw + 20)
          y0 = 100
          tr = wTrain w
          g = Graph (x0 + 30) (y0 + sz + 70) (sz - 40) (envH env - (y0 + sz + 70) - 70) 0 (i2d maxSteps) (-1.6) 0.7
          hs = reverse (at' (wHist w) k [])
          best = wBest w
      in Pictures $
           [ imageBox x0 y0 sz sz 64 64 buf
           , Pictures [ let x = dsX tr V.! i
                            px = x0 + f2 ((x U.! 0 + 1.2) / 2.4) * sz
                            py = y0 + f2 (1 - (x U.! 1 + 1.2) / 2.4) * sz
                        in Pictures [Color (grayC 10) (circleAt px py 13), Color (col (if dsY tr U.! i == 1 then mag else lime)) (circleAt px py 10)]
                      | i <- [0 .. V.length (dsX tr) - 1] ]
           , say (at' titles k "") x0 (y0 - 8) 15 (col ink)
           , say ("test acc " ++ fmtN (100 * accOf m (wTestD w)) 1 ++ "%  train " ++ fmtN (100 * accOf m tr) 0 ++ "%  |w| = " ++ fmtN (weightNorm m) 1) x0 (y0 + sz + 20) 14 (col sub)
           , if k == 1 then say (if wShowBest w then "shown: weights at step " ++ show (bStep best) ++ " (lowest valid. loss)" else "shown: current weights (never stopped)") x0 (y0 + sz + 40) 14 (col sub) else Blank ]
           ++ (if grH g < 60 then [] else
                [ plotAxes g "steps" "log10 loss" [0, i2d maxSteps] [-1, 0]
                , if length hs > 1 then Pictures [ plotXY g (map (\(s, _, _) -> i2d s) hs) (map (\(_, a, _) -> logBase 10 (max 1e-4 a)) hs) (colA 200 sky) 1.5
                                                 , plotXY g (map (\(s, _, _) -> i2d s) hs) (map (\(_, _, b) -> logBase 10 (max 1e-4 b)) hs) (col amber) 3 ] else Blank
                , if k == 1 && bStep best > 0 then Color (colA 150 ink) (Line [(gX g (i2d (bStep best)), grY g), (gX g (i2d (bStep best)), grY g + grH g)]) else Blank ])

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> restartNets w { wWd = min (length wdList - 1) (wWd w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restartNets w { wWd = max 0 (wWd w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> restartNets w { wDrop = min (length dropList - 1) (wDrop w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restartNets w { wDrop = max 0 (wDrop w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> w { wShowBest = not (wShowBest w), wBufOk = [True, False, True] }
    | c `elem` "nN" -> restartNets (newData w { wSeed = wSeed w + 1 })
    | c `elem` "rR" -> restartNets w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 最後まで学習させて、② で E を押す。止めなかったネットの境界はどんな形？
-- 2. λ を 0.001 → 0.03 と変える。境界と ‖w‖ はどう変わる？ 大きすぎると何が起きる？
-- 3. flipP を 0 にすると、正則化の効き目はどうなる？ nTr を 200 にしたら？
-- 4. ドロップアウトの p を大きくすると、訓練の損失（細い線）はどうなる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("重み減衰つきの勾配 ∇L + λw = 差分（相対誤差 " ++ fmtE worst ++ "）", worst < 1e-6)
  , ("ドロップアウト p → 0 の勾配は、ふつうの勾配と同じ", U.and (U.zipWith (\v d -> near v d 1e-15) g0 gd))
  , ("ドロップアウトのマスク（残すと 1/(1 − p) 倍）の平均 = 1（" ++ fmtN (s / cnt) 3 ++ "）", abs (s / cnt - 1) < 0.03)
  , ("正則化なし 3000 歩：訓練の損失 " ++ fmtN ltr 3 ++ " ≪ テストの損失 " ++ fmtN lte 2 ++ "（過学習）", ltr < 0.1 && lte > 3 * ltr)
  , ("重み減衰（λ = 0.03）で重みが小さく（‖w‖ " ++ fmtN (weightNorm b) 1 ++ " < " ++ fmtN (weightNorm a) 1 ++ "）、テストの損失も下がる"
    , weightNorm b < weightNorm a && mlpLoss b (dsX te) (dsY te) < lte) ]
  where
    (tr, te) = withSeed 91 ((,) <$> moons nTr flipP <*> moons nTe flipP)
    m = withSeed 1 (makeMLP 2 8 5 2.5)
    p = mlP m
    lam = 0.05
    gp = withSeed 1 (mlpGrad m (dsX tr) (dsY tr) 0)
    regLoss q = mlpLoss m { mlP = q } (dsX tr) (dsY tr) + (lam / 2) * dotV q q
    worst = foldl' (\wst i -> let a' = p U.// [(i, p U.! i + 1e-6)]
                                  b' = p U.// [(i, p U.! i - 1e-6)]
                              in max wst (relErr (gp U.! i + lam * p U.! i) ((regLoss a' - regLoss b') / 2e-6))) 0 [0, 3 .. U.length p - 1]
    (g0, gd) = withSeed 5 ((,) <$> mlpGrad m (dsX tr) (dsY tr) 0 <*> mlpGrad m (dsX tr) (dsY tr) 1e-300)
    cnt = 20000
    s = withSeed 6 (foldM (\acc _ -> do u <- rnd; pure (acc + (if u < 0.5 then 1 / 0.5 else 0))) 0 [1 .. 20000 :: Int])
    a = withSeed 1 (makeMLP 2 hidden 9 1 >>= \n -> mlpTrain n (dsX tr) (dsY tr) 3000 lrR 0 0)
    b = withSeed 1 (makeMLP 2 hidden 9 1 >>= \n -> mlpTrain n (dsX tr) (dsY tr) 3000 lrR 0.03 0)
    ltr = mlpLoss a (dsX tr) (dsY tr)
    lte = mlpLoss a (dsX te) (dsY te)

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

-- ---- 隠れ層 1 層のニューラルネット（隠れ層 tanh、出力はシグモイド = 分類、交差エントロピーの損失）
-- パラメータは 1 列：w1（j 行 k 列）、b1、w2、b2 の順。学習はフルバッチの Adam。1 フレームに少しずつ呼べば画面は止まらない
data MLP = MLP { mlIn :: !Int, mlH :: !Int, mlP :: !Vec, mlT :: !Int, mlAd :: !(Maybe (Vec, Vec)) }

-- 注意：ここで乱数の種を決め直す
makeMLP :: Int -> Int -> Int -> Double -> R MLP
makeMLP nIn nH seed scale = do
  seedRng seed
  let s1 = scale / sqrt (i2d nIn)
  rows <- mapM (const (do w1 <- mapM (const ((s1 *) <$> randn)) [1 .. nIn]
                          b1 <- (0.5 *) <$> randn
                          w2 <- (/ sqrt (i2d nH)) <$> randn
                          pure (w1, b1, w2))) [1 .. nH]
  pure (MLP nIn nH (U.fromList (concat [w | (w, _, _) <- rows] ++ [b | (_, b, _) <- rows] ++ [w | (_, _, w) <- rows] ++ [0])) 0 Nothing)

copyMLP :: MLP -> MLP
copyMLP m = m { mlT = 0, mlAd = Nothing }

mlpHidden :: MLP -> Vec -> Vec
mlpHidden m x = U.generate (mlH m) (\j -> tanh (goK j 0 (p U.! (nH * nIn + j))))
  where p = mlP m
        nIn = mlIn m
        nH = mlH m
        goK j !k !z = if k >= nIn then z else goK j (k + 1) (z + U.unsafeIndex p (j * nIn + k) * U.unsafeIndex x k)

mlpOut :: MLP -> Vec -> Double
mlpOut m x = sigm (U.ifoldl' (\o j hj -> o + (p U.! (o2 + j)) * hj) (p U.! (o2 + nH)) (mlpHidden m x))
  where p = mlP m
        nH = mlH m
        o2 = nH * mlIn m + nH

-- 交差エントロピーの平均
mlpLoss :: MLP -> V.Vector Vec -> U.Vector Int -> Double
mlpLoss m xs ys = if V.null xs then 0 else s / i2d (V.length xs)
  where s = V.ifoldl' (\acc i x -> let p = clampv (mlpOut m x) 1e-12 (1 - 1e-12)
                                    in acc - (if ys U.! i /= 0 then log p else log (1 - p))) 0 xs

-- 誤差逆伝播：平均の損失の勾配（パラメータと同じ並び）。pDrop > 0 なら隠れ層のドロップアウト（残した素子は 1/(1 − p) 倍）
mlpGrad :: MLP -> V.Vector Vec -> U.Vector Int -> Double -> R Vec
mlpGrad m xs ys pDrop = do
  let nH = mlH m
      nIn = mlIn m
      keep = 1 - pDrop
  masks <- if pDrop > 0 then do us <- rndVec (V.length xs * nH)
                                pure (U.map (\u -> if u < keep then 1 / keep else 0) us)
           else pure (U.replicate (V.length xs * nH) 1)
  pure (mlpGradMask m xs ys masks)

mlpGradMask :: MLP -> V.Vector Vec -> U.Vector Int -> U.Vector Double -> Vec
mlpGradMask m xs ys masks = runST $ do
  let nH = mlH m
      nIn = mlIn m
      p = mlP m
      n = if V.null xs then 1 else V.length xs
      oB1 = nH * nIn
      oW2 = oB1 + nH
      oB2 = oW2 + nH
  g <- MU.replicate (U.length p) 0
  forM_ [0 .. V.length xs - 1] $ \i -> do
    let x = xs V.! i
        h = mlpHidden m x
        mk j = masks U.! (i * nH + j)
        o = foldl' (\acc j -> acc + (p U.! (oW2 + j)) * (h U.! j) * mk j) (p U.! oB2) [0 .. nH - 1]
        d = (sigm o - i2d (ys U.! i)) / i2d n
    MU.modify g (+ d) oB2
    forM_ [0 .. nH - 1] $ \j -> do
      let hj = h U.! j
      MU.modify g (+ d * hj * mk j) (oW2 + j)
      let dz = d * (p U.! (oW2 + j)) * mk j * (1 - hj * hj)
      MU.modify g (+ dz) (oB1 + j)
      forM_ [0 .. nIn - 1] $ \k -> MU.modify g (+ dz * (x U.! k)) (j * nIn + k)
  U.freeze g

-- Adam（β1 = 0.9, β2 = 0.999）、wd = 重み減衰（損失に wd/2 × ‖重み‖² を足す）
mlpTrain :: MLP -> V.Vector Vec -> U.Vector Int -> Int -> Double -> Double -> Double -> R MLP
mlpTrain m0 xs ys iters lr wd pDrop = go 0 m0
  where
    z = U.replicate (U.length (mlP m0)) 0
    go it m
      | it >= iters = pure m
      | otherwise = do
          g <- mlpGrad m xs ys pDrop
          let p = mlP m
              t = mlT m + 1
              (am, av) = fromMaybe (z, z) (mlAd m)
              c1 = 1 - 0.9 ** i2d t
              c2 = 1 - 0.999 ** i2d t
              gq = U.zipWith (\gv pv -> gv + wd * pv) g p
              m' = U.zipWith (\mv gv -> 0.9 * mv + 0.1 * gv) am gq
              v' = U.zipWith (\vv gv -> 0.999 * vv + 0.001 * gv * gv) av gq
              p' = U.zipWith3 (\pv mv vv -> clampv (pv - lr * (mv / c1) / (sqrt (vv / c2) + 1e-8)) (-50) 50) p m' v'
          go (it + 1) m { mlP = p', mlT = t, mlAd = Just (m', v') }

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
