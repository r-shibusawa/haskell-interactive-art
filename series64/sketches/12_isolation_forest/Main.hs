-- ============================================
-- 64-12 Isolation Forest で異常検知 ─ ランダムに縦か横に切っていくと、ほかから離れた点ほど早く 1 つだけになる（孤立する）
-- 1 本の木：ランダムな特徴量を、[最小, 最大] の一様乱数の位置で切る、をくり返す（深さの上限 ⌈log₂ ψ⌉）
--   点の経路長 h = 根から葉までの切った回数（+ 葉に残った点の数の補正 c(size)）。100 本の木の平均 E[h]
--   異常スコア s = 2^(−E[h]/c(ψ))、c(n) = 2H(n − 1) − 2(n − 1)/n（H = 調和数）。s が 1 に近いほど異常、0.5 よりずっと小さければふつう
--   背景 = スコアの地図（明るい = 異常）。マウスの下の点を、← → で選んだ木が孤立させるまでの箱を描く
-- 操作：クリック 点を足す / C 足した点を消す / ← → 木の番号 / ↑↓ 木の本数 / R データを作り直す
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

psi :: Int
psi = 256                      -- ★ 1 本の木に使う点の数 ψ

cells :: Int
cells = 80                     -- スコアの地図のマスの数（80 × 80）

data World = World
  { wNTrees :: !Int            -- ★ 木の本数
  , wSeedD  :: !Int
  , wTreeNo :: !Int
  , wBase   :: ![Pt]           -- かたまり 2 つ + 外れた点
  , wAdded  :: ![Pt]           -- クリックで足した点
  , wForest :: !IForest
  , wGrid   :: !(U.Vector Double)   -- スコアの地図（rows 行まで計算ずみ）
  , wRows   :: !Int
  , wScores :: ![Double]       -- 全部の点のスコア（森を作り直したときだけ計算）
  , wOrder  :: ![Int]          -- スコアの大きい順の番号
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-12 Isolation Forest" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (wScores world0))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 100 2 0 [] [] (IForest V.empty 1 1) U.empty 0 [] [] [])

-- かたまり 2 つ + 外れた点を少し
makeBlobs :: Int -> [Pt]
makeBlobs seed = withSeed seed $ do
  a <- mapM (const (do x <- rndGauss; y <- rndGauss; pure (pt2 (clampv (0.32 + 0.07 * x) 0 1) (clampv (0.62 + 0.07 * y) 0 1)))) [1 .. 160 :: Int]
  b <- mapM (const (do x <- rndGauss; y <- rndGauss; pure (pt2 (clampv (0.68 + 0.05 * x) 0 1) (clampv (0.3 + 0.09 * y) 0 1)))) [1 .. 110 :: Int]
  c <- mapM (const (do x <- rnd; y <- rnd; pure (pt2 (0.05 + 0.9 * x) (0.05 + 0.9 * y)))) [1 .. 8 :: Int]
  pure (a ++ b ++ c)

allPts :: World -> [Pt]
allPts w = wBase w ++ wAdded w

restart :: World -> World
restart w = rebuild w { wBase = makeBlobs (wSeedD w), wAdded = [] }

-- ★ 森を作り直す（種は データの種 × 101 + 足した点の数）
rebuild :: World -> World
rebuild w =
  let pts = allPts w
      f = iForest (V.fromList pts) (wNTrees w) psi (wSeedD w * 101 + length (wAdded w))
      sc = map (anomalyScore f) pts
      order = map snd (sortBy (\(a, _) (b, _) -> compare b a) (zip sc [0 :: Int ..]))
  in w { wForest = f, wGrid = U.replicate (cells * cells) 0, wRows = 0, wScores = sc, wOrder = order }

-- スコアの地図を 1 行ずつ計算（1 フレーム 6 ミリ秒まで）
oneRow :: World -> World
oneRow w = let j = wRows w
               row = [ (i + cells * j, anomalyScore (wForest w) (cellPt cells (i + cells * j))) | i <- [0 .. cells - 1] ]
           in w { wGrid = wGrid w U.// row, wRows = j + 1 }

step :: Env -> Float -> World -> IO World
step _ _ = sliced 6 (\w -> wRows w < cells) oneRow

scoreCol :: Double -> RGB
scoreCol s = let t = clampv ((s - 0.35) / 0.4) 0 1
                 (br, bg, bb) = bgCol
                 (wr, _, _) = wine
                 (_, gg, _) = gold
             in (br + t * (wr + 40 - br), bg + t * (gg - 30 - bg), bb + t * (70 - bb))

draw :: Env -> World -> Picture
draw env w = view env $
  [ imageBox 40 100 side side cells cells (gridImage cells (\q -> if q `div` cells < wRows w then scoreCol (wGrid w U.! q) else bgCol))
  , frameBox pb frameCol
  , Pictures [ Color (colA 220 (if i >= nBase then teal else ink)) (circleAt px py (if i >= nBase then 9 else 5))
             | (i, x) <- zip [0 ..] pts, let (px, py) = toPx pb (x U.! 0) (x U.! 1) ]
  , Pictures [ Pictures [ Color (colOf gold) (ringAt px py 18 2), sayLM (show (r + 1)) (px + 11) (py - 9) 14 (colOf gold) ]
             | (r, i) <- zip [0 :: Int ..] top5, let x = pts !! i, let (px, py) = toPx pb (x U.! 0) (x U.! 1) ]
  , mousePart
  -- ---- 右：スコアのヒストグラムと上位 5 つ
  , plotAxes g "anomaly score s" "scores of all points (right end = anomalous)" [0.3, 0.4, 0.5, 0.6, 0.7, 0.8] [0, grY1 g]
  , Pictures [ Color (colOf (scoreCol (0.31 + 0.02 * fromIntegral k))) (rectTL (gX g (0.3 + 0.02 * fromIntegral k) + 1) (gY g (fromIntegral c)) (grW g / 25 - 2) (grY g + grH g - gY g (fromIntegral c)))
             | (k, c) <- zip [0 :: Int ..] bins, c > 0 ]
  , Color (colOf ink) (dashed 1 4 4 [(gX g 0.5, grY g), (gX g 0.5, grY g + grH g)])
  , say "Top 5 scores (gold rings)" rx ty 15 (colOf ink)
  , Pictures [ say (show (r + 1) ++ ". s = " ++ fmtNum 3 (sc !! i) ++ "  E[h] = " ++ fmtNum 2 (avgPath f (pts !! i))
                    ++ "  (" ++ fmtNum 2 ((pts !! i) U.! 0) ++ ", " ++ fmtNum 2 ((pts !! i) U.! 1) ++ ")" ++ (if i >= nBase then "  added" else ""))
                   rx (ty + 26 + 22 * fromIntegral r) 14 (rgbC 220 210 195)
             | (r, i) <- zip [0 :: Int ..] top5 ]
  -- ---- 文字
  , say ("Isolation Forest   " ++ show (V.length (ifTrees f)) ++ " trees, psi = " ++ show (ifPsi f) ++ ", depth limit " ++ show (ifLimit f)
         ++ (if wRows w < cells then "   (computing map...)" else "")) 40 40 20 (colOf ink)
  , say "Click add point   C clear added   <- -> tree no.   Up/Down number of trees   R new data" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    side = min (envH env - 170) (envW env * 0.5)
    pb = Box 40 100 side side
    rx = 40 + side + 60
    pts = allPts w
    nBase = length (wBase w)
    f = wForest w
    sc = wScores w
    top5 = take 5 (wOrder w)
    bins0 = U.toList (U.accum (+) (U.replicate 25 (0 :: Int)) [ (floorI (clampv (fromIntegral (floorI ((s - 0.3) / 0.02))) 0 24), 1) | s <- sc ])
    bins = bins0
    g = Graph (rx + 10) 210 (envW env - rx - 60) (envH env * 0.3) 0.3 0.8 0 (fromIntegral (maximum (5 : bins)))
    ty = grY g + grH g + 60
    (mu, mv) = fromPx pb (mouseP env)
    inside = mu >= 0 && mu <= 1 && mv >= 0 && mv <= 1
    nT = V.length (ifTrees f)
    mousePart
      | not inside || nT == 0 = say "Put the mouse on the map to watch a tree cut" rx 130 15 (rgbC 200 185 170)
      | otherwise =
          let m = pt2 mu mv
              tno = wTreeNo w `mod` nT
              walk t (x0, x1, y0, y1) d acc = case t of
                ILeaf s -> (reverse acc, (x0, x1, y0, y1), d, s)
                INode fe thr l r ->
                  let goL = m U.! fe < thr
                  in if fe == 0
                       then let xx = fst (toPx pb thr 0)
                                ln = lineW 1 (xx, snd (toPx pb 0 y1)) (xx, snd (toPx pb 0 y0))
                            in walk (if goL then l else r) (if goL then (x0, thr, y0, y1) else (thr, x1, y0, y1)) (d + 1) (ln : acc)
                       else let yy = snd (toPx pb 0 thr)
                                ln = lineW 1 (fst (toPx pb x0 0), yy) (fst (toPx pb x1 0), yy)
                            in walk (if goL then l else r) (if goL then (x0, x1, y0, thr) else (x0, x1, thr, y1)) (d + 1) (ln : acc)
              (lns, (bx0, bx1, by0, by1), d, size) = walk (ifTrees f V.! tno) (0, 1, 0, 1) (0 :: Int) []
              (ax, ay) = toPx pb bx0 by1
              (cx, cy) = toPx pb bx1 by0
              s = anomalyScore f m
              eh = avgPath f m
          in Pictures
               [ Color (rgbaC 255 255 255 200) (Pictures lns)
               , Color (colOf gold) (frameTL 2.5 ax ay (cx - ax) (cy - ay))
               , say ("Tree " ++ show (tno + 1) ++ ": leaf after " ++ show d ++ " cuts (" ++ show size ++ " pts left, +c = " ++ fmtNum 2 (cFactor size) ++ ")") rx 120 15 (colOf ink)
               , say ("E[h] = " ++ fmtNum 2 eh ++ ",  c(psi) = " ++ fmtNum 2 (cFactor (ifPsi f))) rx 144 15 (colOf ink)
               , say ("s = 2^(-" ++ fmtNum 3 (eh / cFactor (ifPsi f)) ++ ") = " ++ fmtNum 3 s) rx 168 15 (colOf ink) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (u, v) = fromPx (Box 40 100 side side) (toP5 env p)
        side = min (envH env - 170) (envW env * 0.5)
    in if u >= 0 && u <= 1 && v >= 0 && v <= 1 && length (wAdded w) < 200 then rebuild w { wAdded = wAdded w ++ [pt2 u v] } else w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTreeNo = (wTreeNo w + 1) `mod` wNTrees w }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTreeNo = (wTreeNo w + wNTrees w - 1) `mod` wNTrees w }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wNTrees = min 300 (wNTrees w + 20) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wNTrees = max 1 (wNTrees w - 20) }
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> rebuild w { wAdded = [] }
    | c == 'r' || c == 'R' -> restart w { wSeedD = wSeedD w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("c(n) = 2H(n − 1) − 2(n − 1)/n（H をそのまま足して比べる、n = 3〜600）、c(1) = 0、c(2) = 1", okC && cFactor 1 == 0 && cFactor 2 == 1)
  , ("c(256) = " ++ fmtNum 4 (cFactor 256) ++ "：近似 2(ln 255 + 0.5772) − 2·255/256 と 0.1% 以内。n = 3 では近似は 10% 以上ずれる"
    , abs (approx / cFactor 256 - 1) < 1e-3 && abs ((2 * (log 2 + 0.5772156649) - 4 / 3) / cFactor 3 - 1) > 0.1)
  , ("はっきりした外れ値 (0.97, 0.03) のスコア " ++ fmtNum 3 sOut ++ " > 0.6、かたまりの中心 (0.32, 0.62) は " ++ fmtNum 3 sIn ++ " < 0.5", sOut > 0.6 && sIn < 0.5)
  , ("外れ値の平均の経路長 " ++ fmtNum 2 (avgPath f pOut) ++ " < 中心 " ++ fmtNum 2 (avgPath f pIn) ++ "（早く孤立する）", avgPath f pOut < avgPath f pIn)
  , ("どの点もスコアは 0〜1、経路長 ≤ 深さの上限 + c(ψ)。E[h] = c(ψ) ならちょうど s = 0.5", okB && near (2 ** (-(cFactor 256) / cFactor 256)) 0.5 1e-15) ]
  where
    okC = and [ abs (cFactor n - (2 * h - (2 * fromIntegral (n - 1)) / fromIntegral n)) <= 1e-9
              | n <- [3 .. 600], let h = foldl' (\s i -> s + 1 / fromIntegral i) 0 [1 .. n - 1 :: Int] ]
    approx = 2 * (log 255 + 0.5772156649) - (2 * 255) / 256
    f = iForest (V.fromList (makeBlobs 2)) 100 256 7
    pOut = pt2 0.97 0.03
    pIn = pt2 0.32 0.62
    sOut = anomalyScore f pOut
    sIn = anomalyScore f pIn
    xsB = withSeed 3 (mapM (const (do a <- rnd; b <- rnd; pure (pt2 a b))) [1 .. 200 :: Int])
    okB = and [ s > 0 && s <= 1 && all (\t -> pathLen t x <= fromIntegral (ifLimit f) + cFactor (ifPsi f) + 1e-9) (take 10 (V.toList (ifTrees f)))
              | x <- xsB, let s = anomalyScore f x ]

-- ---------- やってみよう ----------
-- 1. かたまりから少し離れた所をクリックしよう。足した点の順位は？ 何個も同じ所に足すと（小さなかたまりになると）？
-- 2. マウスを外れ値とかたまりの中に置き、← → で木を変えよう。外れ値は何回の切り口で箱が 1 点だけになる？
-- 3. ↓ で木を 1 本にすると、地図がカクカクになる（1 本の木はでたらめ）。100 本の平均でなめらかに
-- 4. psi を 16 にすると？ Isolation Forest は小さな部分標本でも動くのが長所
-- 5. 13 では、アンサンブルの「ばらつき」を、異常ではなく「自信のなさ」に使う

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

-- ---- Isolation Forest：ランダムな特徴量を、[最小, 最大] の一様乱数のしきい値で切っていく木。早く孤立する点ほど「外れ値」
data ITree = ILeaf !Int | INode !Int !Double ITree ITree

harmTable :: U.Vector Double                 -- H(k) = 1 + 1/2 + … + 1/k（ぴったりの和）
harmTable = U.scanl' (\h k -> h + 1 / fromIntegral k) 0 (U.enumFromN (1 :: Int) 4096)

harmonic :: Int -> Double
harmonic k = if k < U.length harmTable then harmTable U.! k else foldl' (\h i -> h + 1 / fromIntegral i) 0 [1 .. k]

cFactor :: Int -> Double                      -- n 点の木の平均の経路長 c(n)
cFactor n | n <= 1 = 0
          | n == 2 = 1
          | otherwise = 2 * harmonic (n - 1) - (2 * fromIntegral (n - 1)) / fromIntegral n

iTree :: V.Vector Pt -> U.Vector Int -> Int -> Int -> R ITree
iTree xs idx depth limit
  | depth >= limit || U.length idx <= 1 = pure (ILeaf (U.length idx))
  | otherwise = do
      u <- rnd
      let dim = U.length (xs V.! 0)
          f = floor (u * fromIntegral dim)
          vals = U.map (\i -> xAt xs i f) idx
          lo = U.foldl' min posInf vals
          hi = U.foldl' max negInf vals
      if not (hi > lo) then pure (ILeaf (U.length idx)) else do
        u2 <- rnd
        let thr = lo + u2 * (hi - lo)
            (l, r) = U.partition (\i -> xAt xs i f < thr) idx
        lt <- iTree xs l (depth + 1) limit
        rt <- iTree xs r (depth + 1) limit
        pure (INode f thr lt rt)

-- 葉に残った点の数のぶん c(size) を足す
pathLen :: ITree -> Pt -> Double
pathLen = go 0
  where go !d t x = case t of
          ILeaf s -> fromIntegral d + cFactor s
          INode f thr l r -> go (d + 1 :: Int) (if x U.! f < thr then l else r) x

data IForest = IForest { ifTrees :: !(V.Vector ITree), ifPsi :: !Int, ifLimit :: !Int }

iForest :: V.Vector Pt -> Int -> Int -> Int -> IForest
iForest xs nTrees psi seed = withSeed seed $ do
  let n = V.length xs
      m = min psi n
      limit = headOr 1 [ l | l <- [0 ..], 2 ^ l >= max 2 m ]
  ts <- mapM (const (do idx <- shuffleU (U.enumFromN 0 n); iTree xs (U.take m idx) 0 limit)) [1 .. nTrees]
  pure (IForest (V.fromList ts) m limit)

avgPath :: IForest -> Pt -> Double
avgPath f x = V.foldl' (\s t -> s + pathLen t x) 0 (ifTrees f) / fromIntegral (V.length (ifTrees f))

anomalyScore :: IForest -> Pt -> Double      -- s = 2^(−E[h]/c(ψ))
anomalyScore f x = 2 ** (-(avgPath f x) / cFactor (ifPsi f))

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

-- ---- 1 フレームの計算を budget ミリ秒までにする（p5.js の while (performance.now() − t0 < budget && more)）
--      step は必ず 1 回は動く
sliced :: Double -> (w -> Bool) -> (w -> w) -> w -> IO w
sliced budget more step w0 = if not (more w0) then pure w0 else do
  t0 <- getMonotonicTime
  let go w = do
        w' <- evaluate (step w)
        t <- getMonotonicTime
        if (t - t0) * 1000 < budget && more w' then go w' else pure w'
  go w0
