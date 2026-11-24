-- ============================================
-- 64-13 アンサンブルで「自信のなさ」を測る ─ 種（シード）だけがちがう小さなニューラルネットを 8 個、同じデータで学習
-- データのある所では 8 個ともほぼ同じ答え。データのない所（すき間・外側）では、てんでばらばら
--   → 8 個の予測の標準偏差 σ(x) を「自信のなさ」として使える。1 つのモデルだけでは、どこで自信がないか分からない
--   金 = 8 個の平均、帯 = 平均 ± 2σ、下のグラフ = σ(x)。クリックで点を足すと、そこの σ が小さくなる
--   アンビギュイティ分解（00 の多様性予測定理と同じ式）：平均の誤差 = 1 つずつの誤差の平均 − ばらつき（どの点でもぴったり）
-- 操作：クリック 点を足す / C 足した点を消す / B ブートストラップ（ネットごとにデータを引き直す）/ R 種を変えてやり直す
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

nNets, hid :: Int
nNets = 8                      -- ★ ネットの数
hid = 20                       -- ★ 隠れ層の大きさ

lr :: Double
lr = 0.01                      -- ★ 学習率

netCol :: Int -> RGB
netCol k = headOr ink (drop (k `mod` 8) [(240, 120, 100), (236, 214, 160), (110, 165, 240), (190, 135, 235), (150, 210, 95), (245, 175, 140), (64, 186, 176), (200, 160, 200)])

xsDraw :: [Double]             -- 描く x（データの外側 −0.2〜1.2 まで）
xsDraw = [ -0.2 + (1.4 * fromIntegral i) / 140 | i <- [0 .. 140 :: Int] ]

ftrue :: Double -> Double      -- ★ 本当の関数
ftrue x = sin (5 * x) * 0.6 + 0.3 * x

type P2 = (Double, Double)

data World = World
  { wSeedBase :: !Int
  , wBoot     :: !Bool
  , wBase     :: ![P2]
  , wAdded    :: ![P2]
  , wNets     :: ![MLP]
  , wSets     :: ![(V.Vector (U.Vector Double), U.Vector Double)]   -- ネットごとの学習データ
  , wIters    :: !Int
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-13 Ensemble uncertainty" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 1 False [] [] [] [] 0 [])

-- データは 2 つの区間だけ（まん中と外側にすき間）
makeGap :: Int -> [P2]
makeGap seed = withSeed seed (mapM one [0 .. 39 :: Int])
  where one i = do u <- rnd
                   let x = if i < 22 then 0.05 + 0.3 * u else 0.62 + 0.25 * u
                   z <- rndGauss
                   pure (x, ftrue x + 0.05 * z)

inp :: Double -> U.Vector Double  -- ネットへの入力は [−1, 1] に直す
inp x = U.singleton (2 * x - 1)

restart :: World -> World
restart w = resetNets w { wBase = makeGap 4, wAdded = [] }

-- ★ ちがうのは種だけ（B のときはデータもネットごとに引き直す）
resetNets :: World -> World
resetNets w =
  let pts = wBase w ++ wAdded w
      n = length pts
      one k = let net = makeMLP 1 hid False (wSeedBase w * 1000 + k)
                  use = if wBoot w
                          then withSeed (wSeedBase w * 77 + k) (mapM (const (do u <- rnd; pure (pts !! floor (u * fromIntegral n)))) pts)
                          else pts
              in (net, (V.fromList (map (inp . fst) use), U.fromList (map snd use)))
      ps = map one [0 .. nNets - 1]
  in w { wNets = map fst ps, wSets = map snd ps, wIters = 0 }

predsOf :: [MLP] -> Double -> [Double]
predsOf nets x = map (\n -> mlpOut n (inp x)) nets

sdOf :: [Double] -> Double
sdOf = sqrt . varOf

-- 1 フレームに 10 回ずつ学習
step :: Env -> Float -> World -> IO World
step _ _ w
  | wIters w >= 3000 = pure w
  | otherwise = do
      let nets = zipWith (\n (xs, ys) -> mlpTrain n xs ys 10 lr 0) (wNets w) (wSets w)
      _ <- evaluate (sum (map (U.sum . mlP) nets))
      pure w { wNets = nets, wIters = wIters w + 10 }

-- ★ アンビギュイティ分解：(平均 − y)² = 平均ₖ (fₖ − y)² − 平均ₖ (fₖ − 平均)²（点ごとに成り立つ → 全部の点で平均）
data Amb = Amb { aEns :: !Double, aAvg :: !Double, aAmb :: !Double }

ambiguity :: [MLP] -> [P2] -> Amb
ambiguity nets pts =
  let (e, a, b) = foldl' (\(!e0, !a0, !b0) (x, y) -> let f = predsOf nets x
                                                         m = meanOf f
                                                     in (e0 + (m - y) * (m - y), a0 + meanOf (map (\v -> (v - y) * (v - y)) f), b0 + varOf f)) (0, 0, 0) pts
      n = fromIntegral (if null pts then 1 else length pts)
  in Amb (e / n) (a / n) (b / n)

graphMain :: Env -> Graph
graphMain env = Graph 70 110 (envW env - 140) (envH env * 0.48) (-0.2) 1.2 (-1.5) 1.5

draw :: Env -> World -> Picture
draw env w = view env
  [ Color (colA 60 teal) (Pictures [ Polygon [(x0, u0), (x1, u1), (x1, l1), (x0, l0)]
                                    | ((x0, u0, l0), (x1, u1, l1)) <- zip band (drop 1 band) ])
  , plotAxes g "x" "thin = 8 nets, gold = mean, band = mean +- 2 sigma, dotted = true function, dots = data" [-0.2, 0, 0.5, 1, 1.2] [-1, 0, 1]
  , Pictures [ Color (colA 120 (netCol k)) (polyline 1.2 [ gPt g x (p !! k) | (x, p) <- zip xsDraw pp ]) | k <- [0 .. length (wNets w) - 1] ]
  , Color (colA 150 ink) (dashed 1 4 5 [ gPt g x (ftrue x) | x <- xsDraw ])
  , Color (colOf gold) (polyline 3 [ gPt g x m | (x, m) <- zip xsDraw mu ])
  , Color (colOf ink) (Pictures [ circleAt (gX g x) (gY g y) 7 | (x, y) <- wBase w ])
  , Color (colOf wine) (Pictures [ circleAt (gX g x) (gY g y) 10 | (x, y) <- wAdded w ])
  -- ---- 下：σ(x)
  , plotAxes g2 "x" "uncertainty sigma(x) = std of the 8 predictions (ticks at the bottom = data x)" [-0.2, 0, 0.5, 1, 1.2] [0, roundTo 2 (grY1 g2)]
  , Color (colA 160 wine) (Pictures [ Polygon [gPt g2 x0 0, gPt g2 x0 s0, gPt g2 x1 s1, gPt g2 x1 0]
                                    | ((x0, s0), (x1, s1)) <- zip (zip xsDraw sd) (drop 1 (zip xsDraw sd)) ])
  , Color (colOf ink) (Pictures [ lineW 1.5 (gX g2 x, grY g2 + grH g2 - 6) (gX g2 x, grY g2 + grH g2) | (x, _) <- pts ])
  -- ---- 数字
  , say ("iterations " ++ show (wIters w) ++ "   on training data: ensemble error " ++ fmtNum 5 (aEns amb) ++ " = avg single error " ++ fmtNum 5 (aAvg amb)
         ++ " - spread " ++ fmtNum 5 (aAmb amb)) (grX g) (envH env - 40) 14 (rgbC 220 210 195)
  , say ("Ensemble uncertainty   " ++ show nNets ++ " nets (only the seed differs)" ++ (if wBoot w then " + bootstrap" else "")) 40 40 20 (colOf ink)
  , say "Click add point   C clear added   B bootstrap   R new seeds" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    g = graphMain env
    pp = map (predsOf (wNets w)) xsDraw
    mu = map meanOf pp
    sd = map sdOf pp
    band = [ (gX g x, gY g (m + 2 * s), gY g (m - 2 * s)) | (x, m, s) <- zip3 xsDraw mu sd ]
    g2 = Graph (grX g) (grY g + grH g + 50) (grW g) (max 70 (envH env - grY g - grH g - 130)) (-0.2) 1.2 0 (maximum (0.2 : sd))
    pts = wBase w ++ wAdded w
    amb = ambiguity (wNets w) pts

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let g = graphMain env
        (mx, my) = toP5 env p
        x = realToFrac ((mx - grX g) / grW g) * 1.4 - 0.2
        y = 1.5 - realToFrac ((my - grY g) / grH g) * 3
    in if x > -0.2 && x < 1.2 && y > -1.5 && y < 1.5 && length (wAdded w) < 40 then resetNets w { wAdded = wAdded w ++ [(x, y)] } else w
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> resetNets w { wAdded = [] }
    | c == 'b' || c == 'B' -> resetNets w { wBoot = not (wBoot w) }
    | c == 'r' || c == 'R' -> resetNets w { wSeedBase = wSeedBase w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("誤差逆伝播の勾配 = 数値微分（回帰、全 " ++ show (U.length p) ++ " パラメータ、相対誤差 " ++ toExp1 worst ++ "）", worst < 1e-5)
  , ("5 個のネットとも訓練の 2 乗誤差 < 0.01（" ++ intercalate ", " (map (fmtNum 4) mses) ++ "）", all (< 0.01) mses)
  , ("ばらつき σ：データのない所（すき間・外側）" ++ fmtNum 3 far ++ " > データのある所 " ++ fmtNum 3 near0 ++ " の 5 倍", far > 5 * near0)
  , ("データのある所では 5 個ともほぼ同じ（σ < 0.05）", near0 < 0.05)
  , ("アンビギュイティ分解：平均の誤差 = 1 つずつの誤差の平均 − ばらつき（1e-12）、だから平均は「平均的な 1 つ」より悪くならない"
    , near (aEns a) (aAvg a - aAmb a) 1e-12 && aEns a <= aAvg a + 1e-15) ]
  where
    net = makeMLP 1 6 False 3
    tx = map U.singleton [-0.5, 0.1, 0.7]
    ty = [0.2, -0.3, 0.5]
    p = mlP net
    gr = mlpGradP net p (V.fromList tx) (U.fromList ty)
    lossAt q = mlpLoss net { mlP = q } tx ty
    worst = maximum (0 : [ abs ((la - lb) / (2 * h) - gq) / max 1e-4 (abs gq)
                         | q <- [0 .. U.length p - 1]
                         , let h = 1e-6
                               v = p U.! q
                               la = lossAt (p U.// [(q, v + h)])
                               lb = lossAt (p U.// [(q, v - h)])
                               gq = gr U.! q ])
    pts = makeGap 4
    xs = V.fromList (map (inp . fst) pts)
    ys = U.fromList (map snd pts)
    nets = [ mlpTrain (makeMLP 1 hid False (1000 + k)) xs ys 1500 lr 0 | k <- [0 .. 4] ]
    mses = [ mlpLoss n (V.toList xs) (U.toList ys) * 2 | n <- nets ]
    near0 = meanOf [ sdOf (predsOf nets x) | x <- [0.15, 0.25, 0.7, 0.8] ]
    far = meanOf [ sdOf (predsOf nets x) | x <- [0.48, -0.15, 1.15] ]
    a = ambiguity nets pts

-- ---------- やってみよう ----------
-- 1. すき間（x = 0.4〜0.6）に点を 1 つクリックしよう。σ の山はどう変わる？
-- 2. 外側（x < 0 や x > 1）では σ が大きいまま。「知らない所では自信がない」と言えるのがアンサンブルの強み
-- 3. B でブートストラップにすると、データのある所でも少しばらつく。どちらが「自信のなさ」として正直？
-- 4. nNets を 2 や 20 にすると、σ の形は？ 少なすぎると σ そのものがあてにならない
-- 5. 下の式は 00 の多様性予測定理と同じ形。ネットどうしがばらつくほど、平均は 1 つずつより良くなる

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

-- ---- 隠れ層 1 層のニューラルネット（tanh）。cls = True なら出力はシグモイド（交差エントロピー）、False なら直線（2 乗誤差 / 2）
-- パラメータは 1 列：W1（j 行 k 列）、b1、W2、b2 の順。学習はフルバッチの Adam。少しずつ呼べば画面は止まらない
data MLP = MLP { mlIn :: !Int, mlH :: !Int, mlCls :: !Bool, mlP :: !(U.Vector Double), mlT :: !Int
               , mlM :: !(U.Vector Double), mlV :: !(U.Vector Double) }

makeMLPR :: Int -> Int -> Bool -> Int -> R MLP
makeMLPR nIn nH cls seed = do
  seedRng seed
  let s1 = 1.5 / sqrt (fromIntegral nIn)
  rows <- mapM (const (do w1 <- mapM (const ((s1 *) <$> rndGauss)) [1 .. nIn]
                          b1 <- (0.5 *) <$> rndGauss
                          w2 <- (/ sqrt (fromIntegral nH)) <$> rndGauss
                          pure (w1, b1, w2))) [1 .. nH]
  let p = U.fromList (concat [w | (w, _, _) <- rows] ++ [b | (_, b, _) <- rows] ++ [w | (_, _, w) <- rows] ++ [0])
  pure (MLP nIn nH cls p 0 U.empty U.empty)

makeMLP :: Int -> Int -> Bool -> Int -> MLP
makeMLP nIn nH cls seed = withSeed 1 (makeMLPR nIn nH cls seed)

mlpOutP :: Int -> Int -> Bool -> U.Vector Double -> U.Vector Double -> Double
mlpOutP nIn nH cls p x =
  let o = foldl' (\acc j -> let z = foldl' (\zz k -> zz + (p U.! (j * nIn + k)) * (x U.! k)) (p U.! (nH * nIn + j)) [0 .. nIn - 1]
                            in acc + (p U.! (nH * nIn + nH + j)) * tanh z) (p U.! (nH * nIn + 2 * nH)) [0 .. nH - 1]
  in if cls then sigm o else o

mlpOut :: MLP -> U.Vector Double -> Double
mlpOut net = mlpOutP (mlIn net) (mlH net) (mlCls net) (mlP net)

mlpLoss :: MLP -> [U.Vector Double] -> [Double] -> Double
mlpLoss net xs ys =
  let l = foldl' (\s (x, y) -> let p = mlpOut net x
                               in s + (if mlCls net then -(if y /= 0 then log (max 1e-12 p) else log (max 1e-12 (1 - p))) else 0.5 * ((p - y) * (p - y)))) 0 (zip xs ys)
  in if null xs then 0 else l / fromIntegral (length xs)

-- 誤差逆伝播：平均の損失の勾配（パラメータと同じ並び）
mlpGradP :: MLP -> U.Vector Double -> V.Vector (U.Vector Double) -> U.Vector Double -> U.Vector Double
mlpGradP net p xs ys = runST $ do
  let nIn = mlIn net
      nH = mlH net
      nn = fromIntegral (max 1 (V.length xs)) :: Double
      oB1 = nH * nIn
      oW2 = oB1 + nH
      oB2 = oW2 + nH
  g <- MU.replicate (U.length p) 0
  h <- MU.replicate nH 0
  let loopI i | i >= V.length xs = pure ()
              | otherwise = do
                  let x = xs V.! i
                      fwd j !o | j >= nH = pure o
                               | otherwise = do
                                   let z = foldl' (\zz k -> zz + (p U.! (j * nIn + k)) * (x U.! k)) (p U.! (oB1 + j)) [0 .. nIn - 1]
                                       hj = tanh z
                                   MU.write h j hj
                                   fwd (j + 1) (o + (p U.! (oW2 + j)) * hj)
                  o <- fwd 0 (p U.! oB2)
                  let d = ((if mlCls net then sigm o else o) - ys U.! i) / nn
                  MU.modify g (+ d) oB2
                  let back j | j >= nH = pure ()
                             | otherwise = do
                                 hj <- MU.read h j
                                 MU.modify g (+ d * hj) (oW2 + j)
                                 let dz = d * (p U.! (oW2 + j)) * (1 - hj * hj)
                                 MU.modify g (+ dz) (oB1 + j)
                                 mapM_ (\k -> MU.modify g (+ dz * (x U.! k)) (j * nIn + k)) [0 .. nIn - 1]
                                 back (j + 1)
                  back 0
                  loopI (i + 1)
  loopI 0
  U.freeze g

-- Adam（β1 = 0.9, β2 = 0.999）、wd = 重みの減衰
mlpTrain :: MLP -> V.Vector (U.Vector Double) -> U.Vector Double -> Int -> Double -> Double -> MLP
mlpTrain net0 xs ys iters lr wd = go 0 (if U.null (mlM net0) then net0 { mlM = z, mlV = z } else net0)
  where
    z = U.replicate (U.length (mlP net0)) 0
    go it net
      | it >= iters = net
      | otherwise =
          let p = mlP net
              g = mlpGradP net p xs ys
              t = mlT net + 1
              b1t = 1 - 0.9 ** fromIntegral t
              b2t = 1 - 0.999 ** fromIntegral t
              gq = U.zipWith (\gv pv -> gv + wd * pv) g p
              m' = U.zipWith (\mv gv -> 0.9 * mv + 0.1 * gv) (mlM net) gq
              v' = U.zipWith (\vv gv -> 0.999 * vv + 0.001 * gv * gv) (mlV net) gq
              p' = U.zipWith3 (\pv mv vv -> clampv (pv - lr * (mv / b1t) / (sqrt (vv / b2t) + 1e-8)) (-50) 50) p m' v'
          in go (it + 1) net { mlP = p', mlT = t, mlM = m', mlV = v' }

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
