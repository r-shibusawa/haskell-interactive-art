-- ============================================
-- 64-11 多様性の測り方 ─ 2 つのモデルの「当たり/はずれ」を 2 × 2 の表にして、似ている度合いを数で測る
-- N11 = 両方当たり、N00 = 両方はずれ、N10・N01 = 片方だけ当たり。テストの点 N 個で数える
--   不一致率 = (N10 + N01)/N、Q 統計量 = (N11·N00 − N10·N01)/(N11·N00 + N10·N01)、相関 ρ = 当たり/はずれ（0/1）の相関係数
--   Q と ρ は −1〜1。1 に近い = 同じ所でまちがう（多数決しても伸びない）、0 = 独立、負 = 互いに補い合う
--   6・7 はロジスティック回帰（2）の「ほぼコピー」（ブートストラップで学習）。2・6・7 の多数決は 2 とほとんど同じ
--   右下 = 3 つ組 35 通り：横 = 平均の Q、縦 = 多数決で伸びた分（多数決の正解率 − 3 つの平均）
-- 操作：1〜7 モデルを入れる/外す / M 表の物差し / D いちばん伸びる 3 つ（コピーなし）/ C コピー 3 つ / R データを作り直す
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

nTrain, nTest :: Int
nTrain = 200
nTest = 1000

flipP :: Double
flipP = 0.1                 -- ★ ノイズ

names7 :: [String]
names7 = ["k-NN", "logistic", "naive Bayes", "net", "stump", "logit copy 1", "logit copy 2"]

col7 :: [RGB]
col7 = [(240, 120, 100), (236, 214, 160), (110, 165, 240), (190, 135, 235), (150, 210, 95), (215, 190, 130), (195, 170, 110)]

measNames :: [String]
measNames = ["disagreement", "Q statistic", "correlation rho"]

-- 2 × 2 の表と 3 つの物差し
data PairSt = PairSt { psN11 :: !Double, psN10 :: !Double, psN01 :: !Double, psN00 :: !Double, psDis :: !Double, psQ :: !Double, psRho :: !Double }

-- 3 つ組：番号・平均の Q・多数決で伸びた分
data Triple = Triple { trIds :: ![Int], trQ :: !Double, trGain :: !Double }

data World = World
  { wKind    :: !Int
  , wSeed    :: !Int
  , wMeasure :: !Int                 -- ★ 0 不一致率、1 Q、2 ρ
  , wUse     :: ![Bool]
  , wTrain   :: !DataSet
  , wTestD   :: !DataSet
  , wModels  :: ![Model]
  , wMlpLeft :: !Int
  , wProbs   :: ![U.Vector Double]   -- テストの点での確率（先に計算しておく）
  , wCorrect :: ![U.Vector Int]      -- 当たり 1 / はずれ 0
  , wTriples :: ![Triple]
  , wBest    :: ![Int]
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-11 Diversity measures" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (length (wTriples world0))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- p5.js 版では自己テストが学習ずみのネットで refresh するので、はじめに選ばれているのは「いちばん伸びる 3 つ」
world0 :: World
world0 = (restart (testWorld { wMlpLeft = 300 })) { wUse = wUse testWorld }

blankW :: World
blankW = World 4 6 1 [True, False, True, True, False, False, False] (DataSet V.empty U.empty) (DataSet V.empty U.empty) [] 0 [] [] [] [0, 2, 3] []

restart :: World -> World
restart w =
  let tr = makeData2D (wKind w) nTrain flipP (wSeed w)
      te = makeData2D (wKind w) nTest flipP (wSeed w + 400)
      xs = dsX tr
      ys = dsY tr
      ms = [fitBase 0 xs ys, fitBase 1 xs ys, fitBase 2 xs ys, MMlp (makeMLP 2 10 True 7), fitBase 4 xs ys]
      copies = withSeed (wSeed w * 5) (mapM (const (do b <- bootIdx nTrain
                                                       let bx = V.fromList [ xs V.! i | i <- U.toList b ]
                                                           by = U.map (ys U.!) b
                                                       pure (MLogit False (fitLogit bx by False 0.01)))) [1, 2 :: Int])   -- ほぼコピー
  in refresh w { wTrain = tr, wTestD = te, wModels = ms ++ copies, wMlpLeft = 300 }

refresh :: World -> World
refresh w =
  let te = wTestD w
      probs = [ U.fromList [ probOf m x | x <- V.toList (dsX te) ] | m <- wModels w ]
      correct = [ U.zipWith (\p y -> if (if p > 0.5 then 1 else 0) == y then 1 else 0) pk (dsY te) | pk <- probs ]
      w1 = w { wProbs = probs, wCorrect = correct }
      cor k = correct !! k
      tri = [ Triple [a, b, c] (meanOf [psQ (pairStats (cor a) (cor b)), psQ (pairStats (cor a) (cor c)), psQ (pairStats (cor b) (cor c))])
                     (majAcc w1 [a, b, c] - meanOf (map (meanI . cor) [a, b, c]))
            | a <- [0 .. 6], b <- [a + 1 .. 6], c <- [b + 1 .. 6] ]
      -- コピーを使わない 3 つ組で、いちばん伸びる組
      best = trIds (headOr (Triple [0, 2, 3] 0 0) (sortBy (\p q -> compare (trGain q) (trGain p)) (filter (all (< 5) . trIds) tri)))
  in w1 { wTriples = tri, wBest = best, wUse = if wMlpLeft w <= 0 then map (`elem` best) [0 .. 6] else wUse w }

meanI :: U.Vector Int -> Double
meanI v = meanOf (map fromIntegral (U.toList v))

-- ★ 2 × 2 の表と 3 つの物差し（分母が 0 のとき：まったく同じ当たり方なら 1、それ以外は 0）
pairStats :: U.Vector Int -> U.Vector Int -> PairSt
pairStats ca cb =
  let (n11, n10, n01, n00) = U.foldl' (\(!a, !b, !c, !d) (x, y) -> if x /= 0 && y /= 0 then (a + 1, b, c, d) else if x /= 0 then (a, b + 1, c, d)
                                                              else if y /= 0 then (a, b, c + 1, d) else (a, b, c, d + 1)) (0, 0, 0, 0) (U.zip ca cb)
      same = n10 + n01 == 0
      dq = n11 * n00 + n10 * n01
      dr = sqrt ((n11 + n10) * (n01 + n00) * (n11 + n01) * (n10 + n00))
      num = n11 * n00 - n10 * n01
  in PairSt n11 n10 n01 n00 ((n10 + n01) / fromIntegral (U.length ca))
            (if dq > 0 then num / dq else if same then 1 else 0) (if dr > 0 then num / dr else if same then 1 else 0)

-- 選んだモデルの多数決の正解率（同数ならソフト投票）
majAcc :: World -> [Int] -> Double
majAcc w ids =
  let ys = dsY (wTestD w)
      n = length ids
      ok i = let y = ys U.! i
                 s = sum [ if (wCorrect w !! k) U.! i /= 0 then y else 1 - y | k <- ids ]
                 ps = foldl' (\acc k -> acc + (wProbs w !! k) U.! i) 0 ids
                 v = if 2 * s > n then 1 else if 2 * s < n then 0 else if ps / fromIntegral n > 0.5 then 1 else 0
             in v == y
  in fromIntegral (length (filter ok [0 .. U.length ys - 1])) / fromIntegral nTest

measOf :: Int -> PairSt -> Double
measOf m st = case m of
  0 -> psDis st
  1 -> psQ st
  _ -> psRho st

-- ニューラルネットを 1 フレームに 15 回ずつ学習
trainNet :: World -> World
trainNet w = case drop 3 (wModels w) of
  (MMlp net : rest) ->
    let net' = mlpTrain net (V.map centerX (dsX (wTrain w))) (U.map fromIntegral (dsY (wTrain w))) 15 0.03 0
        w' = w { wModels = take 3 (wModels w) ++ MMlp net' : rest, wMlpLeft = wMlpLeft w - 15 }
    in if wMlpLeft w' <= 0 then refresh w' else w'
  _ -> w { wMlpLeft = 0 }

step :: Env -> Float -> World -> IO World
step _ _ w = pure (if wMlpLeft w > 0 then trainNet w else w)

draw :: Env -> World -> Picture
draw env w = view env
  [ say ("pairwise " ++ (measNames !! wMeasure w) ++ " (M to switch)") 40 (gy - 30) 15 (colOf ink)
  , Pictures [ Pictures ([ sayRM (show (a + 1) ++ " " ++ (names7 !! a) ++ " " ++ pctStr (meanI (cor a))) (gx - 8) (gy + fromIntegral a * cs + cs / 2) 14 (colOf (col7 !! a))
                         , sayC (show (a + 1)) (gx + fromIntegral a * cs + cs / 2) (gy - 6) 14 (colOf (col7 !! a)) ]
                         ++ [ cell a b | b <- [0 .. 6] ])
             | a <- [0 .. 6] ]
  , sayWrap "Red = alike (wrong on the same points), teal = different. White frame = chosen models" 40 (gy + 7 * cs + 8) (gx + 7 * cs - 40) 14 (rgbC 170 155 145)
  -- ---- 右上：選んだモデルの多数決
  , say ("chosen models: " ++ (if null ids then "none" else intercalate ", " (map (show . (+ 1)) ids))) rx ty 15 (colOf ink)
  , if null ids then Blank else
      let ind = meanOf (map (meanI . cor) ids)
          mj = majAcc w ids
          prs = [ pairStats (cor a) (cor b) | (i, a) <- zip [0 :: Int ..] ids, b <- drop (i + 1) ids ]
      in Pictures
           [ say ("mean of singles " ++ pctStr ind ++ " -> vote " ++ pctStr mj ++ " (" ++ (if mj >= ind then "+" else "") ++ pctStr (mj - ind) ++ ")") rx (ty + 28) 14 (rgbC 220 210 195)
           , if null prs then Blank else
               say ("pair mean: disagreement " ++ fmtNum 3 (meanOf (map psDis prs)) ++ "  Q " ++ fmtNum 3 (meanOf (map psQ prs)) ++ "  rho " ++ fmtNum 3 (meanOf (map psRho prs))) rx (ty + 52) 14 (rgbC 220 210 195)
           , if length ids == 3 then say ("if errors were independent: p^3 + 3p^2(1-p) = " ++ pctStr (ind ** 3 + 3 * ind ** 2 * (1 - ind))) rx (ty + 76) 14 (rgbC 220 210 195) else Blank ]
  -- ---- 右下：3 つ組 35 通り
  , plotAxes g "mean Q of the triple" "35 triples: gain of the majority vote (accuracy difference)" [-0.2, 0, 0.5, 1] [-0.05, 0, 0.05]
  , Color (colOf frameCol) (lineW 1 (grX g, gY g 0) (grX g + grW g, gY g 0))
  , Pictures [ Color (colA (if on then 255 else 170) c) (circleAt (gX g (trQ t)) (gY g (trGain t)) (if on then 16 else 8))
             | t <- wTriples w
             , let on = all (wUse w !!) (trIds t) && length ids == 3
                   c | on = gold
                     | length (filter (>= 5) (trIds t)) + (if 1 `elem` trIds t then 1 else 0) >= 2 = wine
                     | otherwise = teal ]
  , legend (grX g + grW g - 260) (grY g + 14) [(teal, "ordinary triple"), (wine, "logistic + copies >= 2"), (gold, "the 3 chosen now")]
  -- ---- 文字
  , say ("Diversity measures   data: " ++ shapeName (wKind w) ++ (if wMlpLeft w > 0 then "   (training the net...)" else "")) 40 40 20 (colOf ink)
  , say "1-7 add/remove   M measure   D best 3   C 3 copies   R new data" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    cs = min 60 ((envH env - 260) / 7)
    gx = 250
    gy = 130
    cor k = wCorrect w !! k
    ids = [ k | (k, u) <- zip [0 ..] (wUse w), u ]
    cell a b =
      let st = pairStats (cor a) (cor b)
          v = measOf (wMeasure w) st
          t = if wMeasure w == 0 then 1 - min 1 (v * 3) else clampv v 0 1
          mix (bg, wn, tl) = bg + t * (wn - bg) + (1 - t) * (tl - bg) * 0.6
          (b0, b1, b2) = bgCol
          (w0', w1', w2') = wine
          (t0, t1, t2) = teal
          sel = (wUse w !! a) && (wUse w !! b)
          x = gx + fromIntegral b * cs
          y = gy + fromIntegral a * cs
      in Pictures [ Color (colOf (mix (b0, w0', t0), mix (b1, w1', t1), mix (b2, w2', t2))) (rectTL x y cs cs)
                  , Color (grayC (if sel then 255 else 40)) (frameTL (if sel then 1.5 else 1) x y cs cs)
                  , sayM (fmtNum 2 v) (x + cs / 2) (y + cs / 2) 14 (grayC 240) ]
    rx = gx + 7 * cs + 70
    ty = 120
    g = Graph (rx + 30) (ty + 140) (envW env - rx - 90) (envH env - ty - 260) (-0.2) 1 (-0.06) 0.08

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '7' -> let k = fromEnum c - fromEnum '1' in w { wUse = [ if j == k then not u else u | (j, u) <- zip [0 ..] (wUse w) ] }
    | c == 'm' || c == 'M' -> w { wMeasure = (wMeasure w + 1) `mod` 3 }
    | c == 'd' || c == 'D' -> w { wUse = map (`elem` wBest w) [0 .. 6] }
    | c == 'c' || c == 'C' -> w { wUse = [False, True, False, False, False, True, True] }
    | c == 'r' || c == 'R' -> restart w { wSeed = wSeed w + 1 }
  _ -> w

-- 自己テストの状態：kind = 4、種 6 でネットを 300 回学習してから refresh
testWorld :: World
testWorld = trained { wMlpLeft = 0 }
  where w0 = restart blankW
        trained = let w1 = iterate trainNet w0 !! 20 in w1

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("49 組すべてで Q・ρ は −1〜1、不一致率は 0〜1", all okR prs)
  , ("ρ = 当たり/はずれ（0/1）のピアソンの相関係数（1e-9）、Q と ρ は同じ符号", all okP allAB && all (\st -> psQ st * psRho st >= -1e-12) prs)
  , ("同じモデルどうし：Q = 1、ρ = 1、不一致率 = 0（全部当たりの 0/0 の場合も 1）"
    , psQ self == 1 && psRho self == 1 && psDis self == 0 && psQ allOk == 1 && psRho allOk == 1)
  , ("コピー 3 つ：Q " ++ fmtNum 2 (qOf [1, 5, 6]) ++ "、多数決 " ++ pctStr cl ++ " ≈ ロジスティック 1 つ " ++ pctStr lo ++ "（1% 以内）"
    , abs (cl - lo) <= 0.01 && qOf [1, 5, 6] > 0.95)
  , ("いちばん伸びる 3 つ（" ++ intercalate "・" (map (show . (+ 1)) best) ++ "）：Q " ++ fmtNum 2 (qOf best) ++ " < コピー 3 つ、多数決で +" ++ pctStr (dv - dvi)
    , qOf best < qOf [1, 5, 6] && dv - dvi > 0.02 && dv - dvi > cl - lo)
  , ("独立にまちがう 3 つ（p = 0.7）：多数決 " ++ pctStr (fromIntegral mj / 20000) ++ " ≈ p³ + 3p²(1 − p) = 78.4%、Q ≈ 0（" ++ fmtNum 3 qi ++ "）"
    , abs (fromIntegral mj / 20000 - 0.784) < 0.01 && abs qi < 0.05) ]
  where
    w = testWorld
    cor k = wCorrect w !! k
    allAB = [ (a, b) | a <- [0 .. 6], b <- [0 .. 6 :: Int] ]
    prs = [ pairStats (cor a) (cor b) | (a, b) <- allAB ]
    okR st = psQ st >= -1 && psQ st <= 1 && psRho st >= -1 && psRho st <= 1 && psDis st >= 0 && psDis st <= 1
    -- ふつうのピアソンの相関係数
    okP (a, b) =
      let ca = map fromIntegral (U.toList (cor a)) :: [Double]
          cb = map fromIntegral (U.toList (cor b))
          ma = meanOf ca
          mb = meanOf cb
          (cov, va, vb) = foldl' (\(!c, !x, !y) (p, q) -> (c + (p - ma) * (q - mb), x + (p - ma) * (p - ma), y + (q - mb) * (q - mb))) (0, 0, 0) (zip ca cb)
      in not (va > 0 && vb > 0 && abs (cov / sqrt (va * vb) - psRho (pairStats (cor a) (cor b))) > 1e-9)
    self = pairStats (cor 2) (cor 2)
    allOk = pairStats (U.fromList [1, 1, 1]) (U.fromList [1, 1, 1])
    best = wBest w
    cl = majAcc w [1, 5, 6]
    lo = meanI (cor 1)
    dv = majAcc w best
    dvi = meanOf (map (meanI . cor) best)
    qOf ids = case ids of
      [a, b, c] -> meanOf [psQ (pairStats (cor a) (cor b)), psQ (pairStats (cor a) (cor c)), psQ (pairStats (cor b) (cor c))]
      _ -> 0
    (mj, c3) = withSeed 21 (do vs <- mapM (const (mapM (const (do u <- rnd; pure (if u < 0.7 then 1 else 0))) [1 .. 3 :: Int])) [1 .. 20000 :: Int]
                               pure (length (filter ((>= 2) . sum) vs), vs))
    qi = psQ (pairStats (U.fromList (map (headOr 0) c3)) (U.fromList (map (headOr 0 . drop 1) c3)))

-- ---------- やってみよう ----------
-- 1. C（コピー 3 つ）と D（いちばん伸びる 3 つ）を押しくらべよう。多数決で伸びる分は？ 表の Q は？
-- 2. M で 3 つの物差しを切りかえよう。順位（どの組がいちばん似ているか）は同じ？
-- 3. 右下の 35 点で、Q が小さい組ほど伸びが大きい？ 例外はどんな組？（弱いモデルが 2 つ入ると…）
-- 4. 5（切り株）は弱いが、ほかと「ちがう」。入れると多数決は上がる？ 下がる？
-- 5. 02〜03 の「平均でばらつきが減る」・01 の「独立なら多数決は強い」を、ここでは実際のモデルで測っている

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

-- ---- 合成データ（[0, 1]² の 2 次元、2 クラス、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 三日月 / 5 しま
shapeNames :: [String]
shapeNames = ["blobs", "diagonal", "circle", "XOR", "moons", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapeNames)

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq (u - 0.32) + sq (v - 0.36) < sq (u - 0.68) + sq (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq (u - 0.5) + sq (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if v > 0.5 + 0.28 * cos (pi * (1.6 * u - 0.3)) - 0.18 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0
  where sq a = a * a

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2DR :: Int -> Int -> Double -> Int -> R DataSet
makeData2DR kind n flipP seed = do
  seedRng seed
  ps <- mapM (const one) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))
  where one = do
          a <- rnd
          b <- rnd
          let u = 0.02 + 0.96 * a
              v = 0.02 + 0.96 * b
              c = trueLabel kind u v
          f <- rnd
          pure (pt2 u v, if f < flipP then 1 - c else c)

makeData2D :: Int -> Int -> Double -> Int -> DataSet
makeData2D kind n flipP seed = withSeed 1 (makeData2DR kind n flipP seed)

-- ---- k 近傍法：近い k 個のうちクラス 1 の割合 = 確率
data Knn = Knn { knX :: !(U.Vector Double), knY :: !(U.Vector Double), knC :: !(U.Vector Int), knK :: !Int }

fitKnn :: V.Vector Pt -> U.Vector Int -> Int -> Knn
fitKnn xs ys k = Knn (U.fromList [x U.! 0 | x <- V.toList xs]) (U.fromList [x U.! 1 | x <- V.toList xs]) ys k

-- 小さい順の列に挿入（並べ替えはしない。同じ距離なら先に入った点が前）
knnProb :: Knn -> Pt -> Double
knnProb m x = if k == 0 then 0.5 else fromIntegral cnt / fromIntegral k
  where
    n = U.length (knX m)
    k = min (knK m) n
    x0 = x U.! 0
    x1 = x U.! 1
    cnt = runST $ do
      bd <- MU.replicate (max 1 k) 0
      bc <- MU.replicate (max 1 k) (0 :: Int)
      let loop !i !len
            | i >= n = pure ()
            | otherwise = do
                let a = U.unsafeIndex (knX m) i - x0
                    b = U.unsafeIndex (knY m) i - x1
                    d = a * a + b * b
                lastD <- if len > 0 then MU.read bd (len - 1) else pure 0
                if len < k || d < lastD
                  then do
                    let shift q = if q > 0
                                    then do e <- MU.read bd (q - 1)
                                            if e > d then do MU.write bd q e
                                                             MU.read bc (q - 1) >>= MU.write bc q
                                                             shift (q - 1)
                                                     else pure q
                                    else pure q
                    q <- shift (if len < k then len else len - 1)
                    MU.write bd q d
                    MU.write bc q (U.unsafeIndex (knC m) i)
                    loop (i + 1) (min k (len + 1))
                  else loop (i + 1) len
      loop 0 0
      foldM (\s q -> (+ s) <$> MU.read bc q) 0 [0 .. k - 1]

-- ---- ロジスティック回帰（ニュートン法、L2 正則化 λ）。F = 特徴量ベクトルの列、戻り値 = 重み w
dotV :: U.Vector Double -> U.Vector Double -> Double
dotV a b = U.foldl' (+) 0 (U.zipWith (*) a b)

fitLogitVec :: [U.Vector Double] -> U.Vector Int -> Double -> Int -> U.Vector Double
fitLogitVec fs ys lam iters = go 0 (U.replicate d 0)
  where
    d = U.length (headOr U.empty fs)
    yl = U.toList ys
    go it w
      | it >= iters = w
      | otherwise =
          let ps = [ sigm (dotV w f) | f <- fs ]
              rs = zipWith (\p y -> p - fromIntegral y) ps yl
              ss = map (\p -> max 1e-6 (p * (1 - p))) ps
              g = [ foldl' (\acc (r, f) -> acc + r * (f U.! a)) 0 (zip rs fs) + lam * (w U.! a) | a <- [0 .. d - 1] ]
              h = [ [ foldl' (\acc (s, f) -> acc + s * (f U.! a) * (f U.! b)) 0 (zip ss fs) + (if a == b then lam else 0) | b <- [0 .. d - 1] ] | a <- [0 .. d - 1] ]
              dw = solveLin h g 0
              w' = U.fromList (zipWith (\wa da -> wa - clampv da (-20) 20) (U.toList w) dw)
              big = maxOr 0 (map abs dw)
          in if big < 1e-9 then w' else go (it + 1) w'

logitFeat :: Pt -> Bool -> U.Vector Double
logitFeat x quad = let u = x U.! 0 - 0.5
                       v = x U.! 1 - 0.5
                   in U.fromList (if quad then [1, u, v, u * u, v * v, u * v] else [1, u, v])

fitLogit :: V.Vector Pt -> U.Vector Int -> Bool -> Double -> U.Vector Double
fitLogit xs ys quad lam = fitLogitVec (map (`logitFeat` quad) (V.toList xs)) ys lam 25

-- ---- ガウスのナイーブベイズ：クラスごとに、特徴量ごとの平均と分散（特徴量どうしは独立と仮定）
data NB = NB { nbPrior :: ![Double], nbMu :: ![[Double]], nbVa :: ![[Double]] }

fitNB :: V.Vector Pt -> U.Vector Int -> NB
fitNB xs ys = NB [p | (p, _, _) <- per] [m | (_, m, _) <- per] [s | (_, _, s) <- per]
  where
    n = V.length xs
    per = [ let pts = [ x | (i, x) <- zip [0 ..] (V.toList xs), ys U.! i == c ]
                cols = [ map (U.! f) pts | f <- [0, 1] ]
            in ( fromIntegral (length pts + 1) / fromIntegral (n + 2)
               , [ if null col then 0.5 else meanOf col | col <- cols ]
               , [ max 1e-3 (varOf col) | col <- cols ] )
          | c <- [0, 1 :: Int] ]

nbProb :: NB -> Pt -> Double
nbProb m x = sigm (lg 1 - lg 0)
  where
    lg c = log (nbPrior m !! c) + foldl' (term c) 0 [0, 1]
    term c s f = let va = nbVa m !! c !! f
                     dd = x U.! f - nbMu m !! c !! f
                 in s - 0.5 * log (2 * pi * va) - dd * dd / (2 * va)

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

-- ---- 切り株（深さ 1 の木）：特徴量 1 つ × しきい値 1 つ。重み w つき。左右の葉 = クラス 1 の重みの割合（確率）
data Stump = Stump { stFeat :: !Int, stThr :: !Double, stPL :: !Double, stPR :: !Double, stErr :: !Double }

fitStump :: V.Vector Pt -> U.Vector Int -> Maybe (U.Vector Double) -> Stump
fitStump xs ys mw = case best of
    Nothing -> Stump 0 2 ((w1 + 1) / (wT + 2)) 0.5 (min w1 (wT - w1))
    Just (f, thr, lw, l1, err) -> Stump f thr ((l1 + 0.5) / (lw + 1)) ((w1 - l1 + 0.5) / (wT - lw + 1)) err
  where
    n = V.length xs
    wOf i = maybe 1 (U.! i) mw
    (wT, w1) = foldl' (\(!a, !b) i -> let wi = wOf i in (a + wi, if ys U.! i /= 0 then b + wi else b)) (0, 0) [0 .. n - 1]
    best = foldl' scan Nothing [0, 1]
    scan b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) [0 .. n - 1])
          step (!b, !lw, !l1) q =
            let i = ord U.! q
                wi = wOf i
                lw' = lw + wi
                l1' = if ys U.! i /= 0 then l1 + wi else l1
                v0 = xAt xs i f
                v1 = xAt xs (ord U.! (q + 1)) f
                err = min l1' (lw' - l1') + min (w1 - l1') (wT - lw' - (w1 - l1'))
                b' = if not (v1 > v0) then b else case b of
                       Just (_, _, _, _, e) | not (err < e - 1e-12) -> b
                       _ -> Just (f, (v0 + v1) / 2, lw', l1', err)
            in (b', lw', l1')
          (bb, _, _) = foldl' step (b0, 0, 0) [0 .. n - 2]
      in bb

stumpProb :: Stump -> Pt -> Double
stumpProb m x = if x U.! stFeat m <= stThr m then stPL m else stPR m

-- ---- 5 種類の「ちがうモデル」をまとめて扱う：fitBase 番号 X y → モデル、probOf モデル x → クラス 1 の確率
data Model = MKnn !Knn | MLogit !Bool !(U.Vector Double) | MNB !NB | MMlp !MLP | MStump !Stump

baseNames :: [String]
baseNames = ["k-NN", "logistic", "naive Bayes", "neural net", "stump"]

baseCol :: Int -> RGB                         -- ★ モデルの色
baseCol k = case k of
  0 -> (240, 120, 100)
  1 -> (236, 214, 160)
  2 -> (110, 165, 240)
  3 -> (190, 135, 235)
  _ -> (150, 210, 95)

centerX :: Pt -> U.Vector Double              -- ネットには [−1, 1] に直して渡す
centerX x = U.fromList [2 * x U.! 0 - 1, 2 * x U.! 1 - 1]

fitBase :: Int -> V.Vector Pt -> U.Vector Int -> Model
fitBase kind xs ys = case kind of
  0 -> MKnn (fitKnn xs ys 15)                 -- ★ k = 15
  1 -> MLogit False (fitLogit xs ys False 0.01)   -- 直線の境界
  2 -> MNB (fitNB xs ys)
  3 -> MMlp (mlpTrain (makeMLP 2 10 True 7) (V.map centerX xs) (U.map fromIntegral ys) 300 0.03 0)
  _ -> MStump (fitStump xs ys Nothing)

probOf :: Model -> Pt -> Double
probOf m x = case m of
  MKnn k -> knnProb k x
  MLogit q w -> sigm (dotV w (logitFeat x q))
  MNB nb -> nbProb nb x
  MMlp net -> mlpOut net (centerX x)
  MStump s -> stumpProb s x

predOf :: Model -> Pt -> Int
predOf m x = if probOf m x > 0.5 then 1 else 0

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double
accuracyOf fn xs ys = let n = V.length xs
                          ok = length [ () | i <- [0 .. n - 1], fn (xs V.! i) == ys U.! i ]
                      in if n == 0 then 0 else fromIntegral ok / fromIntegral n

-- 投票：hard = 多数決（同数ならソフトの確率で決める）、soft = 確率の平均
hardVote :: [Model] -> Pt -> Int
hardVote ms x = let v = sum (map (`predOf` x) ms)
                    n = length ms
                in if 2 * v > n then 1 else if 2 * v < n then 0 else if softVote ms x > 0.5 then 1 else 0

softVote :: [Model] -> Pt -> Double
softVote ms x = if null ms then 0.5 else foldl' (\s m -> s + probOf m x) 0 ms / fromIntegral (length ms)

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
