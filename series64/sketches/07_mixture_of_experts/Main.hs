-- ============================================
-- 64-07 専門家の混合（Mixture of Experts）─ 場所によって規則が変わるデータ。左は上り坂、まん中は下り坂、右は 2 次曲線
-- 専門家 k = 直線 a_k + b_k·x。ゲート = ソフトマックス（c_k + d_k·x）→ 「この x はどの専門家に任せるか」の確率 g_k(x)
--   予測 = Σ g_k(x)·専門家 k(x)。学習は勾配降下法：損失 = −log Σ g_k·exp(−(y − 専門家 k)² / 2s²)
--   各点の「担当の度合い」h_k を計算すると、専門家は自分の担当の点だけを、ゲートは g_k を h_k に近づけるように学ぶ
--   背景の色 = ゲートの確率（専門家の色）。1 本の直線や 2 次式より、ずっと小さな誤差になる
-- 操作：← → 専門家の数 / スペース 一時停止 / R 初期値を変えてやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 150                 -- ★ 点の数

noiseS, s2, lrM :: Double
noiseS = 0.04              -- ★ ノイズ
s2 = 0.05                  -- ★ s²（担当の分け方のするどさ）
lrM = 1.0                  -- ★ 学習率

maxIt :: Int
maxIt = 4000               -- ★ 学習の回数

expCol :: Int -> RGB
expCol k = case k `mod` 5 of
  0 -> (240, 120, 100)
  1 -> (110, 165, 240)
  2 -> (150, 210, 95)
  3 -> (190, 135, 235)
  _ -> (236, 214, 160)

ftrue :: Double -> Double                     -- ★ 場所で変わる規則
ftrue x | x < 0.4 = 0.2 + 1.2 * x
        | x < 0.7 = 0.68 - 1.5 * (x - 0.4)
        | otherwise = 0.23 + 6 * ((x - 0.7) * (x - 0.7)) - 0.5 * (x - 0.7)

data Reg = Reg { rxs :: ![Double], rys :: ![Double] }

-- 専門家 k = a_k + b_k·(x − 0.5)、ゲートのロジット = c_k + d_k·(x − 0.5)
data Moe = Moe { mK :: !Int, mA :: !(U.Vector Double), mB :: !(U.Vector Double), mC :: !(U.Vector Double), mD :: !(U.Vector Double) }

data World = World
  { wNExp    :: !Int
  , wSeedI   :: !Int
  , wPaused  :: !Bool
  , wTrain   :: !Reg
  , wTestR   :: !Reg
  , wMoe     :: !Moe
  , wIts     :: !Int
  , wLossH   :: ![Double]              -- 100 回ごとの訓練の損失（古い順）
  , wS1      :: !Double                -- 1 本の直線のテストの誤差
  , wS2      :: !Double                -- 1 つの 2 次式のテストの誤差
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-07 Mixture of experts" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (wS2 world0)
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 3 1 False (Reg [] []) (Reg [] []) (Moe 0 U.empty U.empty U.empty U.empty) 0 [] 0 0 [])

makeReg :: Int -> Int -> Reg
makeReg n seed = withSeed seed $ do
  ps <- mapM (const (do x <- rnd; g <- rndGauss; pure (x, ftrue x + noiseS * g))) [1 .. n]
  pure (Reg (map fst ps) (map snd ps))

-- ★ はじめは「左から順に担当」をゆるく決め、少しだけ乱数でずらす
makeMoE :: Int -> Int -> Moe
makeMoE k seed =
  let abd = withSeed seed (mapM (\j -> do a <- rndGauss; b <- rndGauss; d <- rndGauss
                                          pure (0.5 + 0.2 * a, 0.3 * b, 3 * fromIntegral j + d)) [0 .. k - 1])
      ds = [ d | (_, _, d) <- abd ]
      cs = 0 : snd (foldl' (\(cc, acc) (j, d1, d0) -> let c' = cc - (d1 - d0) * (-0.5 + fromIntegral j / fromIntegral k) in (c', acc ++ [c']))
                            (0, []) (zip3 [1 :: Int ..] (drop 1 ds) ds))
  in Moe k (U.fromList [ a | (a, _, _) <- abd ]) (U.fromList [ b | (_, b, _) <- abd ]) (U.fromList cs) (U.fromList ds)

gateOf :: Moe -> Double -> [Double]
gateOf m x = let t = x - 0.5
                 z = zipWith (\c d -> c + d * t) (U.toList (mC m)) (U.toList (mD m))
                 mx = maxOr 0 z
                 e = map (\v -> exp (v - mx)) z
                 s = sumL e
             in map (/ s) e

expertOf :: Moe -> Int -> Double -> Double
expertOf m k x = (mA m U.! k) + (mB m U.! k) * (x - 0.5)

moePredict :: Moe -> Double -> Double
moePredict m x = foldl' (+) 0 (zipWith (\k g -> g * expertOf m k x) [0 ..] (gateOf m x))

moeLoss :: Moe -> Reg -> Double
moeLoss m d =
  let one l (x, y) = let s = foldl' (\acc (k, g) -> let e = y - expertOf m k x in acc + g * exp (-(e * e) / (2 * s2))) 0 (zip [0 ..] (gateOf m x))
                     in l - log (max 1e-300 s)
  in foldl' one 0 (zip (rxs d) (rys d)) / fromIntegral (length (rxs d))

-- ★ 勾配：h_k = 担当の度合い（事後確率）。専門家 ← h_k·(y − 予測)、ゲート ← (g_k − h_k)
moeGrad :: Moe -> Reg -> Moe
moeGrad m d = foldl' one (Moe k z z z z) (zip (rxs d) (rys d))
  where
    k = mK m
    z = U.replicate k 0
    n = fromIntegral (length (rxs d))
    one gr (x, y) =
      let t = x - 0.5
          g = gateOf m x
          l = [ log (max 1e-300 gk) - ((y - expertOf m j x) * (y - expertOf m j x)) / (2 * s2) | (j, gk) <- zip [0 ..] g ]
          mx = maxOr 0 l
          h = map (\v -> exp (v - mx)) l
          s = sumL h
          hv = U.fromList h
          hk j = (hv U.! j) / s
          r j = (y - expertOf m j x) / s2
          gv = U.fromList g
      in Moe k (U.imap (\j ga -> ga - (hk j * r j) / n) (mA gr))
               (U.imap (\j gb -> gb - (hk j * r j * t) / n) (mB gr))
               (U.imap (\j gc -> gc + (gv U.! j - hk j) / n) (mC gr))
               (U.imap (\j gd -> gd + ((gv U.! j - hk j) * t) / n) (mD gr))

-- パラメータごとに歩幅を変えた勾配降下法
moeTrain :: Moe -> Reg -> Int -> Moe
moeTrain m0 d iters = foldl' (\m _ -> let g = moeGrad m d
                                      in m { mA = U.zipWith (\v gv -> v - lrM * s2 * gv) (mA m) (mA g)
                                           , mB = U.zipWith (\v gv -> v - 4 * lrM * s2 * gv) (mB m) (mB g)
                                           , mC = U.zipWith (\v gv -> clampv (v - 4 * lrM * gv) (-200) 200) (mC m) (mC g)
                                           , mD = U.zipWith (\v gv -> clampv (v - 20 * lrM * gv) (-400) 400) (mD m) (mD g) }) m0 [1 .. iters]

mseOn :: (Double -> Double) -> Reg -> Double
mseOn fn d = meanOf [ (fn x - y) * (fn x - y) | (x, y) <- zip (rxs d) (rys d) ]

restart :: World -> World
restart w =
  let tr = makeReg nPts 3
      te = makeReg 400 77
      m = makeMoE (wNExp w) (wSeedI w)
  in w { wPaused = False, wTrain = tr, wTestR = te, wMoe = m, wIts = 0, wLossH = [moeLoss m tr]
       , wS1 = mseOn (polyEval (polyFit (rxs tr) (rys tr) 1 1e-9)) te, wS2 = mseOn (polyEval (polyFit (rxs tr) (rys tr) 2 1e-9)) te }

step :: Env -> Float -> World -> IO World
step _ _ w
  | not (wPaused w) && wIts w < maxIt =
      let m = moeTrain (wMoe w) (wTrain w) 20
          its = wIts w + 20
      in pure w { wMoe = m, wIts = its, wLossH = if its `mod` 100 == 0 then wLossH w ++ [moeLoss m (wTrain w)] else wLossH w }
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  -- 背景：ゲートの確率を縦の帯で
  [ Pictures [ Color (colOf (mixC (gateOf m x))) (rectTL (grX g + fromIntegral q / fromIntegral cols * grW g) (grY g) (grW g / fromIntegral cols + 1) (grH g))
             | q <- [0 .. cols - 1], let x = (fromIntegral q + 0.5) / fromIntegral cols ]
  , plotAxes g "x" "dots = training data, coloured lines = experts, gold = mixture (background = gate)" [0, 0.5, 1] [0, 0.5, 1]
  , Pictures [ Color (colA 90 (expCol k)) (polyline 1 [ gPt g x (expertOf m k x) | x <- xsG ]) | k <- [0 .. mK m - 1] ]
  , Pictures [ Color (colOf (expCol k)) (lineW 4 (gPt g x0 (expertOf m k x0)) (gPt g x1 (expertOf m k x1)))
             | k <- [0 .. mK m - 1], (x0, x1) <- zip xsG (drop 1 xsG), (gateOf m ((x0 + x1) / 2) !! k) > 0.5 ]
  , Color (rgbaC 220 210 195 170) (Pictures [ uncurry circleAt (gPt g x y) 5 | (x, y) <- zip (rxs (wTrain w)) (rys (wTrain w)) ])
  , Color (colOf gold) (polyline 2.5 [ gPt g x (moePredict m x) | x <- xsG ])
  -- ---- 下：ゲートの確率
  , plotAxes g2 "x" "gate probabilities g_k(x) (they always add up to 1)" [0, 0.5, 1] [0, 0.5, 1]
  , Pictures [ Color (colOf (expCol k)) (polyline 2.5 [ gPt g2 x (gateOf m x !! k) | x <- xsG ]) | k <- [0 .. mK m - 1] ]
  -- ---- 右：損失と誤差の表
  , plotAxes g3 "training steps" "training loss (negative log-likelihood)" [0, fromIntegral maxIt / 2, fromIntegral maxIt] [roundTo 1 (grY0 g3), roundTo 1 (grY1 g3)]
  , Color (colOf gold) (polyline 2 [ gPt g3 (fromIntegral q * 100) v | (q, v) <- zip [0 :: Int ..] (wLossH w) ])
  , say "test squared error (smaller is better)" rx ty 15 (colOf ink)
  , Pictures [ Pictures [ say nm rx yy 14 c, sayR (fmtNum 5 v) (rx + 340) yy 14 c ]
             | (q, (nm, v)) <- zip [0 :: Int ..] rows
             , let yy = ty + 28 + 24 * fromIntegral q
                   c = if q == 2 then colOf gold else rgbC 220 210 195 ]
  , say ("it cannot go below the noise variance " ++ fmtNum 5 (noiseS * noiseS)) rx (ty + 28 + 24 * 3) 14 (rgbC 170 155 145)
  -- ---- 文字
  , say ("Mixture of experts   " ++ show (mK m) ++ " experts, softmax gate   training " ++ show (wIts w) ++ " steps" ++ (if wPaused w then " (paused)" else "")) 40 40 20 (colOf ink)
  , say "<- -> number of experts   Space pause   R new initial values" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    m = wMoe w
    cols = 160 :: Int
    xsG = [ fromIntegral i / 100 | i <- [0 .. 100 :: Int] ]
    g = Graph 60 110 (envW env * 0.56) (envH env * 0.5) 0 1 (-0.1) 1
    g2 = Graph 60 (grY g + grH g + 50) (grW g) (max 60 (envH env - grY g - grH g - 120)) 0 1 0 1
    rx = grX g + grW g + 70
    lh = wLossH w
    g3 = Graph rx 110 (envW env - rx - 50) (envH env * 0.3) 0 (fromIntegral maxIt) (minOr 0 lh - 0.1) (maxOr 0 lh + 0.1)
    ty = grY g3 + grH g3 + 60
    rows = [("one line", wS1 w), ("one quadratic", wS2 w), ("mixture of experts (" ++ show (mK m) ++ ")", mseOn (moePredict m) (wTestR w))]
    mixC gt = let (br, bg, bb) = bgCol
                  (r, gg, b) = foldl' (\(a1, a2, a3) (k, p) -> let (c1, c2, c3) = expCol k in (a1 + p * c1, a2 + p * c2, a3 + p * c3)) (0, 0, 0) (zip [0 ..] gt)
              in (br + 0.25 * (r - br), bg + 0.25 * (gg - bg), bb + 0.25 * (b - bb))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wNExp = min 5 (wNExp w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wNExp = max 1 (wNExp w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> restart w { wSeedI = wSeedI w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("ゲートの確率はどの x でも 0〜1 で合計 1。ロジットが ±500 でも NaN にならない", okG && noNaN gb && near (headOr 0 gb) 1 1e-12)
  , ("勾配の式 = 数値微分（専門家とゲートの 12 個のパラメータ、相対誤差 " ++ toExp1 worst ++ "）", worst <= 1e-4)
  , ("2000 回の学習で損失が 1/4 未満に下がる（" ++ fmtNum 3 l0 ++ " → " ++ fmtNum 3 (moeLoss mm tr) ++ "）", moeLoss mm tr < l0 / 4)
  , ("テストの誤差：専門家の混合 " ++ fmtNum 4 e ++ " < 1 つの 2 次式 " ++ fmtNum 4 e2 ++ " < 1 本の直線 " ++ fmtNum 4 e1, e < 0.5 * e2 && e2 < e1)
  , ("3 つの区間（x = 0.15, 0.55, 0.9）をそれぞれ別の専門家が担当する（" ++ intercalate ", " (map show owner) ++ "）"
    , length owner == 3 && all (\(a, b) -> a /= b) [ (a, b) | (i, a) <- zip [0 :: Int ..] owner, (j, b) <- zip [0 ..] owner, i < j ]) ]
  where
    m = makeMoE 3 5
    okG = all (\q -> let gg = gateOf m (fromIntegral q / 100) in not (abs (sumL gg - 1) > 1e-12 || any (\v -> not (v >= 0 && v <= 1)) gg)) [0 .. 100 :: Int]
    gb = gateOf (Moe 2 (U.fromList [0, 0]) (U.fromList [0, 0]) (U.fromList [500, -500]) (U.fromList [0, 0])) 0.3
    d = makeReg 40 9
    gr = moeGrad m d
    getP pn mo = case pn of { 0 -> mA mo; 1 -> mB mo; 2 -> mC mo; _ -> mD mo }
    setP pn k v mo = let upd xs = xs U.// [(k, v)]
                     in case pn of { 0 -> mo { mA = upd (mA mo) }; 1 -> mo { mB = upd (mB mo) }; 2 -> mo { mC = upd (mC mo) }; _ -> mo { mD = upd (mD mo) } }
    errs = [ let v = getP pn m U.! k
                 h = 1e-5
                 lp = moeLoss (setP pn k (v + h) m) d
                 lm = moeLoss (setP pn k (v - h) m) d
                 num = (lp - lm) / (2 * h)
             in abs (num - getP pn gr U.! k) / max 1e-3 (abs num)
           | pn <- [0 .. 3 :: Int], k <- [0 .. 2] ]
    worst = foldl' max 0 errs
    tr = makeReg 150 3
    te = makeReg 400 77
    mm0 = makeMoE 3 1
    l0 = moeLoss mm0 tr
    mm = moeTrain mm0 tr 2000
    e = mseOn (moePredict mm) te
    e1 = mseOn (polyEval (polyFit (rxs tr) (rys tr) 1 1e-9)) te
    e2 = mseOn (polyEval (polyFit (rxs tr) (rys tr) 2 1e-9)) te
    owner = map (argMaxL . gateOf mm) [0.15, 0.55, 0.9]

-- ---------- やってみよう ----------
-- 1. ← で専門家を 1 人にすると、ただの直線。2 人、3 人、4 人で誤差はどう変わる？
-- 2. R で初期値を変えると、2 人の専門家が同じ区間を取り合い、1 人が「さぼる」ことがある（局所解）
-- 3. s2 を 0.5 にすると担当の分け方がぼやける。0.01 にするとするどくなる
-- 4. 大きな言語モデルにも「専門家の混合」を使うものがある。ゲートが単語ごとに専門家を選ぶ
-- 5. 08 からはブースティング：前のモデルのまちがいを、次のモデルが担当する

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

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

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

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

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 連立 1 次方程式 A w = b（部分ピボットつきのガウスの消去法）。ridge = 対角に足す小さな数（解けないのを防ぐ）
solveLin :: [[Double]] -> [Double] -> Double -> [Double]
solveLin a b ridge = [ if abs (r U.! i) < 1e-14 then 0 else (r U.! n) / (r U.! i) | (i, r) <- zip [0 ..] (V.toList final) ]
  where
    n = length b
    m0 = V.fromList [ U.fromList (zipWith (\k v -> if k == i then v + ridge else v) [0 ..] row ++ [bi]) | (i, row, bi) <- zip3 [0 :: Int ..] a b ]
    final = foldl' elim m0 [0 .. n - 1]
    elim m c =
      let p = foldl' (\pp r -> if abs ((m V.! r) U.! c) > abs ((m V.! pp) U.! c) then r else pp) c [c + 1 .. n - 1]
      in if abs ((m V.! p) U.! c) < 1e-14 then m else
         let ms = m V.// [(c, m V.! p), (p, m V.! c)]
             pr = ms V.! c
         in V.imap (\r row -> if r == c then row else
                      let f = (row U.! c) / (pr U.! c)
                      in if f /= 0 then U.imap (\k v -> if k >= c then v - f * (pr U.! k) else v) row else row) ms

-- ---- 多項式の最小 2 乗法。x ∈ [0, 1] を [−1, 1] に直し、チェビシェフ多項式 T0, T1, … を特徴量にする（次数が高くても計算が安定）
chebFeat :: Double -> Int -> [Double]
chebFeat x deg = take (deg + 1) (go 1 t)
  where t = 2 * x - 1
        go a b = a : go b (2 * t * b - a)

data Poly = Poly { pDeg :: !Int, pC :: ![Double] }

-- (ΦᵀΦ + λI) c = Φᵀy
polyFit :: [Double] -> [Double] -> Int -> Double -> Poly
polyFit xs ys deg ridge =
  let d = deg + 1
      fs = map (\x -> U.fromList (chebFeat x deg)) xs
      bv = [ foldl' (\s (f, y) -> s + (f U.! p) * y) 0 (zip fs ys) | p <- [0 .. d - 1] ]
      am = [ [ foldl' (\s f -> s + (f U.! p) * (f U.! q)) 0 fs | q <- [0 .. d - 1] ] | p <- [0 .. d - 1] ]
  in Poly deg (solveLin am bv ridge)

polyEval :: Poly -> Double -> Double
polyEval m x = foldl' (+) 0 (zipWith (*) (pC m) (chebFeat x (pDeg m)))

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 95 70 75) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (rgbC 175 160 150) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gY g v + 6) 14 (rgbC 175 160 150) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (rgbC 215 205 190), say yl (grX g) (grY g - 8) 14 (rgbC 215 205 190) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.2 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashed wd on off pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
              k = max 1 (ceiling (l / (on + off))) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
              fr = on / (on + off)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + fr) / fromIntegral k)) | i <- [0 .. k - 1] ]

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (colOf c) (rectTL x (yy - 5) 14 10), say s (x + 22) (yy + 5) 14 (rgbC 220 210 195)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

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
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)
