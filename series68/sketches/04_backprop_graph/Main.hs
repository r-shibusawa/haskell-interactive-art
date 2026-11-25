-- ============================================
-- 68-04 計算グラフと誤差逆伝播 ─ 小さなネット L = (σ(w1 x + b) w2 − y)² を、1 回の計算ずつの「ノード」に分けて描く
-- 前向き（左 → 右）：各ノードの値を計算。後ろ向き（右 → 左）：∂L/∂(ノード) を連鎖律でもどす
--   ∂L/∂(入力) = ∂L/∂(出力) × ∂(出力)/∂(入力)（その場所だけの微分）。同じ値を 2 か所で使ったら勾配は足し算
-- 右の表：逆伝播の勾配と、差分 (L(p + h) − L(p − h))/2h が一致するかの確認（勾配チェック）
-- 操作：スペース 1 ノードずつ進める / A 自動再生 / ← → 変える数をえらぶ / ↑↓ その数を ±0.1 / G 勾配降下を 1 回 / T 学習し続ける / R ランダムな値
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
lr = 0.3                   -- ★ 勾配降下の学習率

editNames :: [String]
editNames = ["x", "w1", "b", "w2", "y"]   -- 変えられる数（入力のノード）

data Op = OpIn | OpMul | OpAdd | OpSub | OpSig | OpSq deriving Eq

-- ノード：名前、計算、入力ノードの番号、画面の位置（0〜1）
data Node = Node { nId :: !String, nOp :: !Op, nArgs :: ![Int], nPos :: !(Double, Double) }

-- ★ 計算グラフ。並び順 = 計算の順番
graphNodes :: [Node]
graphNodes =
  [ Node "x" OpIn [] (0.05, 0.18), Node "w1" OpIn [] (0.05, 0.45)
  , Node "u" OpMul [1, 0] (0.21, 0.3), Node "b" OpIn [] (0.21, 0.62)
  , Node "z" OpAdd [2, 3] (0.37, 0.45), Node "h" OpSig [4] (0.52, 0.45)
  , Node "w2" OpIn [] (0.52, 0.75), Node "o" OpMul [5, 6] (0.66, 0.58)
  , Node "y" OpIn [] (0.66, 0.88), Node "e" OpSub [7, 8] (0.8, 0.72)
  , Node "L" OpSq [9] (0.94, 0.72) ]

-- 入力の値：名前 → 値
type Vals = [(String, Double)]

valOf :: Vals -> String -> Double
valOf v k = fromMaybe 0 (lookup k v)

setVal :: String -> Double -> Vals -> Vals
setVal k x v = [ (k', if k' == k then x else y) | (k', y) <- v ]

-- 前向き：全部のノードの値
forward :: [Node] -> Vals -> [Double]
forward nodes v = reverse (foldl' one [] nodes)
  where
    one acc n =
      let vals = reverse acc
          a = [ at' vals i 0 | i <- nArgs n ]
          x = case (nOp n, a) of
                (OpIn, _) -> valOf v (nId n)
                (OpMul, [p, q]) -> p * q
                (OpAdd, [p, q]) -> p + q
                (OpSub, [p, q]) -> p - q
                (OpSig, [p]) -> sigm p
                (OpSq, [p]) -> p * p
                _ -> 0
      in x : acc

-- ∂(n)/∂(入力 k) の値（その場所だけの微分）
localGrads :: [Double] -> Node -> Double -> [Double]
localGrads vals n val =
  let a = [ at' vals i 0 | i <- nArgs n ]
  in case (nOp n, a) of
       (OpMul, [p, q]) -> [q, p]
       (OpAdd, _) -> [1, 1]
       (OpSub, _) -> [1, -1]
       (OpSig, _) -> [val * (1 - val)]
       (OpSq, [p]) -> [2 * p]
       _ -> []

-- ★ 誤差逆伝播：最後のノードの勾配 = 1 から、逆の順番に「自分の勾配 × その場所の微分」を入力へ足していく
backward :: [Node] -> [Double] -> U.Vector Double
backward nodes vals = foldl' back g0 (reverse (zip3 [0 ..] nodes vals))
  where
    nn = length nodes
    g0 = U.generate nn (\k -> if k == nn - 1 then 1 else 0)
    back g (k, n, val) = let gk = g U.! k
                         in foldl' (\gg (i, lg) -> gg U.// [(i, gg U.! i + gk * lg)]) g (zip (nArgs n) (localGrads vals n val))

lossAt :: Vals -> Double
lossAt v = lastOr 0 (forward graphNodes v)

gradCheck :: Vals -> String -> Double -> Double
gradCheck v k hh = (lossAt (setVal k (valOf v k + hh) v) - lossAt (setVal k (valOf v k - hh) v)) / (2 * hh)

nodeIndex :: String -> Int
nodeIndex k = headOr 0 [ i | (i, n) <- zip [0 ..] graphNodes, nId n == k ]

-- 勾配降下を 1 回：w1, b, w2 を勾配の反対向きに
gdStep :: Vals -> (Vals, Double)
gdStep v =
  let g = backward graphNodes (forward graphNodes v)
      v' = foldl' (\acc k -> setVal k (clampv (valOf acc k - lr * g U.! nodeIndex k) (-20) 20) acc) v ["w1", "b", "w2"]
  in (v', lossAt v')

data World = World
  { wVals    :: !Vals
  , wEdit    :: !Int
  , wPhase   :: !Int
  , wAuto    :: !Bool
  , wAutoT   :: !Int
  , wTrain   :: !Bool
  , wLossH   :: ![Double]             -- 新しい順（300 個まで）
  , wSeedV   :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-04 Backprop graph" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

vals0 :: Vals
vals0 = [("x", 1.5), ("w1", 0.8), ("b", -0.4), ("w2", 1.2), ("y", 0.2)]

world0 :: World
world0 = World vals0 1 0 True 0 False [] 1 []

nN :: Int
nN = length graphNodes

applyGd :: World -> World
applyGd w = let (v, l) = gdStep (wVals w) in w { wVals = v, wLossH = take 300 (l : wLossH w), wPhase = 2 * nN }

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w2
  where
    w1 = if wTrain w0 && envFrame env `mod` 3 == 0 then applyGd w0 else w0
    w2 = if wAuto w1 && not (wTrain w1)
           then let t = wAutoT w1 + 1 in w1 { wAutoT = t, wPhase = if t `mod` 30 == 0 then (wPhase w1 + 1) `mod` (2 * nN + 6) else wPhase w1 }
           else w1

opLabel :: Node -> String
opLabel n = case (nOp n, [ nId (at' graphNodes i n) | i <- nArgs n ]) of
  (OpMul, [a, b]) -> a ++ "*" ++ b
  (OpAdd, [a, b]) -> a ++ " + " ++ b
  (OpSub, [a, b]) -> a ++ " - " ++ b
  (OpSig, [a]) -> "sig(" ++ a ++ ")"
  (OpSq, [a]) -> a ++ "^2"
  _ -> ""

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Computational graph and backprop: L = (sig(w1 x + b) w2 - y)^2 = " ++ fmtN l 5 ++ "   "
              ++ (if ph < nN then "forward " ++ show (ph + 1) ++ " / " ++ show nN else "backward " ++ show (min nN (ph - nN + 1)) ++ " / " ++ show nN))
             ("Space next node   A auto " ++ onOff (wAuto w) ++ "   Left/Right value to change (" ++ at' editNames (wEdit w) "" ++ ")   Up/Down +-0.1   G one gradient step   T keep training " ++ onOff (wTrain w) ++ "   R random")
  -- 矢印（後ろ向きで勾配が流れたら、マゼンタで局所的な微分を書く）
  , Pictures [ let (ax, ay) = pos (at' graphNodes i n)
                   (bx', by') = pos n
                   back = ph >= nN && nN - 1 - (ph - nN) <= k
                   c | back = col mag
                     | ph >= k = col (let (r, g, b) = wire in (r + 60, g + 60, b + 60))
                     | otherwise = col wire
               in Pictures [ Color c (lineW (if back then 3 else 2) (ax + bw, ay + bh / 2) (bx', by' + bh / 2))
                           , if back then sayC ("x" ++ fmtN lg 3) ((ax + bw + bx') / 2) ((ay + by') / 2 + bh / 2 - 6) 14 (col mag) else Blank ]
             | (k, n, val) <- zip3 [0 ..] graphNodes vals, (i, lg) <- zip (nArgs n) (localGrads vals n val) ]
  , Pictures [ let (x, y) = pos n
                   shownF = ph >= k
                   shownB = ph >= nN && nN - 1 - (ph - nN) <= k
                   isEd = nId n == at' editNames (wEdit w) ""
               in Pictures [ Color (col panel) (rectTL x y bw bh)
                           , Color (col (if isEd then amber else if shownB then mag else wire)) (frameTL (if isEd then 3 else 1.5) x y bw bh)
                           , say (nId n ++ (if nOp n == OpIn then "" else " = " ++ opLabel n)) (x + 8) (y + 20) 16 (col ink)
                           , say ("v " ++ (if shownF then fmtN val 4 else "...")) (x + 8) (y + 39) 14 (col (if shownF then lime else wire))
                           , say ("g " ++ (if shownB then fmtN (grads U.! k) 4 else "...")) (x + 8) (y + 58) 14 (col (if shownB then mag else wire)) ]
             | (k, n, val) <- zip3 [0 ..] graphNodes vals ]
  -- 今のステップの説明
  , say explain 30 ty 16 (col ink)
  , say "green v = forward value, magenta g = dL/d(node). 'x number' on an arrow = the local derivative" 30 (ty + 24) 14 (col sub)
  , say "(mul: the other value, sig: sig(1 - sig), square: 2e)" 30 (ty + 44) 14 (col sub)
  -- ---- 右：勾配チェックと損失
  , say "Gradient check (central difference, h = 1e-5)" rx 110 16 (col ink)
  , Pictures [ say t (rx + dx) 136 14 (col sub) | (t, dx) <- zip ["var", "backprop", "difference", "rel. error"] cols ]
  , Pictures [ let bp = grads U.! nodeIndex k
                   fd = gradCheck (wVals w) k 1e-5
                   re = relErr bp fd
                   c = col (if re < 1e-6 then lime else mag)
               in Pictures [ say t (rx + dx) (162 + 24 * i2f i) 14 c | (t, dx) <- zip [k, fmtN bp 6, fmtN fd 6, fmtE re] cols ]
             | (i, k) <- zip [0 :: Int ..] editNames ]
  , plotAxes g "G steps" "loss L (G / T to learn)" [0, grX1 g] [0, roundPrec 2 (grY1 g)]
  , plotXY g (map i2d [0 .. length lh - 1]) lh (col amber) 2
  , drawSelfTest env (wTest w) ]
  where
    vals = forward graphNodes (wVals w)
    grads = backward graphNodes vals
    l = lastOr 0 vals
    ph = wPhase w
    gw = envW env * 0.64
    gy = 100
    gh = envH env - 270
    bw = min 118 (gw * 0.13)
    bh = 66
    pos n = let (px, py) = nPos n in (30 + f2 px * (gw - bw), gy + f2 py * gh)
    ty = gy + gh + 86
    onOff b = if b then "ON" else "OFF"
    explain
      | ph < nN = let n = at' graphNodes ph (headOr (Node "" OpIn [] (0, 0)) graphNodes)
                  in "forward: " ++ nId n ++ (if nOp n == OpIn then " is an input = " else " = " ++ opLabel n ++ " = ") ++ fmtN (at' vals ph 0) 5
      | ph < 2 * nN = let k = nN - 1 - (ph - nN)
                          n = at' graphNodes k (Node "" OpIn [] (0, 0))
                          users = [ m | m <- graphNodes, k `elem` nArgs m ]
                      in if k == nN - 1 then "backward: start from dL/dL = 1"
                         else "backward: dL/d" ++ nId n ++ " = " ++ intercalate " + " [ "dL/d" ++ nId m ++ " x d" ++ nId m ++ "/d" ++ nId n | m <- users ] ++ " = " ++ fmtN (grads U.! k) 5
      | otherwise = "all gradients are ready. G moves w1, b, w2 against the gradient by " ++ jsNum lr ++ " x"
    rx = gw + 40
    cols = [0, 50, 150, 250]
    lh = reverse (wLossH w)
    g = Graph (rx + 10) 330 (envW env - rx - 50) (min 220 (envH env - 470)) 0 (max 10 (i2d (length lh - 1))) 0 (maximum (1e-3 : 0.01 : lh))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = False, wPhase = (wPhase w + 1) `mod` (2 * nN + 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wEdit = (wEdit w + 1) `mod` length editNames }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wEdit = (wEdit w + length editNames - 1) `mod` length editNames }
  EventKey (SpecialKey KeyUp) Down _ _ -> nudge 0.1
  EventKey (SpecialKey KeyDown) Down _ _ -> nudge (-0.1)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "gG" -> applyGd w
    | c `elem` "tT" -> w { wTrain = not (wTrain w) }
    | c `elem` "rR" -> let vs = withSeed (400 + wSeedV w) (mapM (\k -> do u <- rnd; pure (k, i2d (jsRound (20 * (u - 0.5))) / 10)) editNames)
                       in w { wVals = vs, wSeedV = wSeedV w + 1, wLossH = [], wPhase = 0 }
  _ -> w
  where
    k = at' editNames (wEdit w) "x"
    nudge d = w { wVals = setVal k (clampv (i2d (jsRound ((valOf (wVals w) k + d) * 10)) / 10) (-5) 5) (wVals w), wPhase = 0 }

-- ---------- やってみよう ----------
-- 1. スペースで 1 つずつ進め、∂L/∂w1 = 2e × w2 × σ(1 − σ) × x になっていることを紙で確かめる
-- 2. y を σ の範囲（0〜1）の外、例えば 1.5 にしたら学習はどこまで進む？
-- 3. graphNodes に新しいノード（例：L2 = L + 0.1 w2²）を足し、opLabel・forward・localGrads を書きかえる
-- 4. x を 0 にすると w1 の勾配はいくつ？ どうして？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("20 組のランダムな値：逆伝播の勾配 = 中心差分（相対誤差 " ++ fmtE worst ++ " < 1e−6）", worst < 1e-6)
  , ("∂L/∂w1 = 2e · w2 · σ(1 − σ) · x（手で微分した式と一致）", near (gv U.! nodeIndex "w1") closed 1e-14)
  , ("同じ値を 2 回使う f = a·a：勾配は足し算で 2a = 6", sqG U.! 0 == 6)
  , ("勾配降下 60 回で損失が下がる（" ++ fmtN l0 4 ++ " → " ++ fmtE l1 ++ "）", l1 < 0.01 * l0) ]
  where
    sets = withSeed 404 (mapM (const (do x <- randn; w1 <- randn; b <- randn; w2 <- randn; y <- rnd
                                         pure [("x", 2 * x), ("w1", w1), ("b", b), ("w2", 2 * w2), ("y", y)])) [1 .. 20 :: Int])
    worst = maximum (0 : [ abs (bp - fd) / max 1e-4 (abs bp + abs fd)
                         | v <- sets, let g = backward graphNodes (forward graphNodes v)
                         , k <- editNames, let bp = g U.! nodeIndex k, let fd = gradCheck v k 1e-5 ])
    gv = backward graphNodes (forward graphNodes vals0)
    s = sigm (0.8 * 1.5 - 0.4)
    e = s * 1.2 - 0.2
    closed = 2 * e * 1.2 * s * (1 - s) * 1.5
    sqNodes = [Node "a" OpIn [] (0, 0), Node "f" OpMul [0, 0] (0, 0)]
    sqG = backward sqNodes (forward sqNodes [("a", 3)])
    l0 = lossAt vals0
    l1 = lossAt (foldl' (\v _ -> fst (gdStep v)) vals0 [1 .. 60 :: Int])

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
