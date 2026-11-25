-- ============================================
-- 68-01 活性化関数と勾配消失 ─ ニューロンの出力 = f(重みつきの和 z)。f が「活性化関数」
-- ステップ（0 か 1）・シグモイド σ(z) = 1/(1 + e⁻ᶻ)・tanh・ReLU = max(0, z)・softplus = log(1 + eᶻ) のグラフと微分 f'(z)
-- 何層も重ねた鎖 h_l = f(w_l h_(l−1) + b_l) の勾配は ∂h_L/∂h_l = Π w f'(z)（連鎖律で掛け算）。
-- シグモイドの微分は最大でも 1/4 → 層を重ねるほど勾配が指数的に小さくなる（勾配消失）。ReLU の微分は 1 なので消えにくい
-- 操作：1〜5 関数をえらぶ / マウス 接線を見る z / ↑↓ 層の数 L / ← → 重みの大きさ σ_w / R 鎖を作り直す
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

niceNames :: [String]
niceNames = ["step", "sigmoid", "tanh", "ReLU", "softplus"]

nChains :: Int
nChains = 300              -- ★ 平均をとる鎖の数

-- 関数ごとの log₁₀|∂h_L/∂h_l| の平均（足りない層は NaN）と、勾配が 0 になった鎖の割合
data ChainLog = ChainLog { clLg :: ![Double], clDead :: !Double }

data World = World
  { wSel    :: !Int
  , wDepth  :: !Int
  , wSigW   :: !Double
  , wSeed   :: !Int
  , wLog    :: ![(Act, ChainLog)]
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-01 Activation functions" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = rebuild (World 1 20 1.0 1 [] [])

actCol :: Int -> RGB                          -- 関数ごとの色
actCol k = at' [sub, mag, lime, sky, amber] k ink

-- 1 本の鎖：重み ws とバイアス bs
data Chain = Chain { chW :: ![Double], chB :: ![Double] }

-- ★ 前向きに z と h を計算（h_0 から）
chainForward :: Act -> Chain -> Double -> ([Double], [Double])
chainForward a ch h0 = go [h0] [] (zip (chW ch) (chB ch))
  where go hs zs [] = (reverse hs, reverse zs)
        go hs zs ((w, b) : rest) = let z = w * headOr 0 hs + b in go (actF a z : hs) (z : zs) rest

-- ★ 後ろ向きに ∂h_L/∂h_l を掛け算でためる（l = L, L−1, …, 0）
chainBackward :: Act -> [Double] -> [Double] -> [Double]
chainBackward a ws zs = foldr (\(w, z) acc -> headOr 1 acc * actD a z * w : acc) [1] (zip ws zs)

randomChain :: Int -> Double -> R Chain
randomChain l sw = do
  ps <- mapM (const (do w <- randn; b <- randn; pure (sw * w, 0.3 * b))) [1 .. l]
  pure (Chain (map fst ps) (map snd ps))

-- 300 本の鎖で log₁₀|∂h_L/∂h_l| の平均（幾何平均）を、関数ごとに
rebuild :: World -> World
rebuild w = w { wLog = [ (a, one a) | a <- [Sigmoid, Tanh, Relu, Softplus] ] }
  where
    l = wDepth w
    chains = withSeed (100 + wSeed w) (mapM (const (randomChain l (wSigW w))) [1 .. nChains])
    one a =
      let gs = [ let (_, zs) = chainForward a ch 0.5 in chainBackward a (chW ch) zs | ch <- chains ]
          zeroCnt = length [ () | g <- gs, headOr 0 g == 0 ]
          stat k = foldl' (\(s, c) g -> let v = at' g k 0 in if v /= 0 then (s + logBase 10 (abs v), c + 1) else (s, c)) (0, 0 :: Int) gs
          lg = [ let (s, c) = stat k in if i2d c >= 0.2 * i2d nChains then s / i2d c else 0 / 0 | k <- [0 .. l] ]
      in ChainLog lg (i2d zeroCnt / i2d nChains)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Activation functions and vanishing gradients: " ++ at' niceNames (wSel w) "" ++ "   layers L = " ++ show (wDepth w) ++ "   sigma_w = " ++ fmtN (wSigW w) 2)
             "1-5 function (1 step 2 sigmoid 3 tanh 4 ReLU 5 softplus)   mouse: tangent   Up/Down layers   Left/Right weight size sigma_w   R new chains"
  -- ---- 左：f(z) と f'(z)
  , plotAxes g1 "z" "function f(z) (thin = the other functions)" [-4, -2, 0, 2, 4] [-1, 0, 1, 2, 3, 4]
  , Pictures [ plotXY g1 zs (map (actF (actAt k)) zs) (colA 90 (actCol k)) 1 | k <- [0 .. 4], k /= wSel w ]
  , plotXY g1 zs (map (actF a) zs) (col (actCol (wSel w))) 3
  , Color (colA 160 ink) (Line [(gX g1 (zm - 1.2), gY g1 (fm - 1.2 * dm)), (gX g1 (zm + 1.2), gY g1 (fm + 1.2 * dm))])
  , Color (col ink) (circleAt (gX g1 zm) (gY g1 fm) 9)
  , plotAxes g2 "z" ("derivative f'(z) = slope of the tangent (at the mouse z: " ++ fmtN dm 3 ++ ")") [-4, -2, 0, 2, 4] [0, 0.25, 0.5, 1]
  , Color (col wire) (Line [(grX g2, gY g2 0.25), (grX g2 + grW g2, gY g2 0.25)])
  , plotXY g2 zs (map (actD a) zs) (col (actCol (wSel w))) 3
  , Color (col ink) (circleAt (gX g2 zm) (gY g2 dm) 9)
  -- ---- 右：深い鎖の勾配
  , plotAxes g3 "layer l (left = near the input)" ("log10 |dh_L / dh_l| (mean of " ++ show nChains ++ " chains)") [0, i2d (jsRound (i2d (wDepth w) / 2)), i2d (wDepth w)] [grY0 g3, i2d (jsRound (grY0 g3 / 2)), 0]
  , Color (col wire) (Line [(grX g3, gY g3 0), (grX g3 + grW g3, gY g3 0)])
  , Pictures [ plotXY g3 ls (clLg cl) (col (actCol (k + 1))) (if nm == a then 4 else 1.5) | (k, (nm, cl)) <- zip [0 ..] (wLog w) ]
  , legend (grX g3 + 12) (grY g3 + 16) [(mag, "sigmoid"), (lime, "tanh"), (sky, "ReLU"), (amber, "softplus")]
  , say "max of the derivative: sigmoid 1/4, tanh 1, ReLU 1, softplus < 1" rx ty 15 (col ink)
  , say "step: slope 0 everywhere, so no gradient learning (see 00)" rx (ty + 20) 14 (col sub)
  , case lookup a (wLog w) of
      Just c -> let lg0 = headOr (0 / 0) (clLg c)
                in say (at' niceNames (wSel w) "" ++ ": gradient at the input ~ " ++ (if isFin lg0 then "10^" ++ fmtN lg0 1 else "0 in most chains")
                        ++ (if a == Relu then "   dead chains (gradient 0) " ++ fmtN (100 * clDead c) 0 ++ "%" else "")) rx (ty + 46) 15 (col ink)
      Nothing -> Blank
  , say "sigmoid: <= |w| x 1/4 per layer, so the log falls linearly" rx (ty + 74) 14 (col sub)
  , say "larger sigma_w: tanh / ReLU explode instead (good size: 07)" rx (ty + 94) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    a = actAt (wSel w)
    lw = envW env * 0.44
    hh = envH env - 250
    g1 = Graph 60 110 (lw - 60) (hh * 0.55) (-4) 4 (-1.2) 4.2
    zs = [ -4 + i2d i / 30 | i <- [0 .. 240 :: Int] ]
    (mx, _) = mouseP env
    zm = clampv (d2 ((mx - grX g1) / grW g1) * 8 - 4) (-4) 4
    fm = actF a zm
    dm = actD a zm
    g2 = Graph 60 (grY g1 + grH g1 + 60) (grW g1) (hh * 0.3) (-4) 4 0 1.1
    rx = lw + 70
    y0 = max (-60) (foldl' (\m v -> if isFin v then min m (4 * i2d (floor (v / 4) :: Int)) else m) (-12) (concatMap (clLg . snd) (wLog w)))
    g3 = Graph rx 110 (envW env - rx - 40) (hh * 0.62) 0 (i2d (wDepth w)) y0 2
    ls = map i2d [0 .. wDepth w]
    ty = grY g3 + grH g3 + 56

actAt :: Int -> Act
actAt k = at' actNames k Sigmoid

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> w { wSel = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> rebuild w { wSeed = wSeed w + 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wDepth = min 50 (wDepth w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wDepth = max 5 (wDepth w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wSigW = min 4 (i2d (jsRound ((wSigW w + 0.25) * 100)) / 100) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wSigW = max 0.25 (i2d (jsRound ((wSigW w - 0.25) * 100)) / 100) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. マウスを左右に動かして、接線の傾きと下の微分のグラフの点が同じか確かめる
-- 2. シグモイドで σ_w を 4 にすると勾配消失はましになる？ その代わり何が起きる？
-- 3. ReLU で、勾配が 0 になってしまう鎖の割合（死んだ ReLU）を L を変えて見る
-- 4. actF に自分の関数（例 leaky ReLU：z > 0 なら z、そうでなければ 0.1 z）を足して、actD も書いてみる

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("4 つの関数の微分 f'(z) = 中心差分（相対誤差 " ++ fmtE worst ++ "）", worst < 1e-6)
  , ("シグモイドの微分 σ'(0) = 1/4 が最大（σ' = σ(1 − σ)）", near (actD Sigmoid 0) 0.25 1e-15 && mx <= 0.25 + 1e-15)
  , ("tanh(z) = 2σ(2z) − 1（ずれ " ++ fmtE e2 ++ "）", e2 < 1e-12)
  , ("6 層の鎖：連鎖律の掛け算 ∂h_L/∂h_0 = 差分（相対誤差 " ++ fmtE worstC ++ "）", worstC < 1e-6)
  , ("シグモイドの鎖の勾配 ≤ Π|w| × (1/4)^12", abs (headOr 0 gS) <= prodW * 0.25 ** 12 * (1 + 1e-12)) ]
  where
    hh = 1e-5
    worst = maximum (0 : [ let z = -4 + i2d i * 0.1 + 0.013     -- 0 ちょうど（ReLU の角）はさける
                               fd = (actF a (z + hh) - actF a (z - hh)) / (2 * hh)
                           in abs (fd - actD a z) / max 1e-3 (abs fd)
                         | a <- [Sigmoid, Tanh, Relu, Softplus], i <- [0 .. 80 :: Int] ])
    mx = maximum [ actD Sigmoid (-10 + i2d i / 100) | i <- [0 .. 2000 :: Int] ]
    e2 = maximum (0 : [ abs (tanh z - (2 * sigm (2 * z) - 1)) | k <- [0 .. 40 :: Int], let z = -5 + 0.25 * i2d k ])
    (ch, sh) = withSeed 7 ((,) <$> randomChain 12 1.2 <*> randomChain 6 2)
    lastH a h0 = lastOr 0 (fst (chainForward a sh h0))
    worstC = maximum (0 : [ let (_, zs) = chainForward a sh 0.5
                                g = chainBackward a (chW sh) zs
                                eps = 1e-5
                                fd = (lastH a (0.5 + eps) - lastH a (0.5 - eps)) / (2 * eps)
                            in relErr fd (headOr 0 g) | a <- [Sigmoid, Tanh, Softplus] ])
    gS = let (_, zs) = chainForward Sigmoid ch 0.5 in chainBackward Sigmoid (chW ch) zs
    prodW = foldl' (\p v -> p * abs v) 1 (chW ch)

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
