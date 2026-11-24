-- ============================================
-- 64-08 マージンで見るブースティング ─ 切り株を 1 本ずつ足す AdaBoost を、「マージン」で観察する（series62/10 の続き）
-- 点 i のマージン = yᵢ·F(xᵢ) / Σα（y = ±1、F = Σ α_t·切り株 t の答え）。−1〜1 で、正なら正解、大きいほど「票差が大きい」
--   訓練誤差が 0 になったあとも学習を続けると、いちばん小さいマージンが広がり続ける → テスト誤差は下がるか、ほとんど上がらない
--   右上 = マージンの累積分布（そのマージン以下の点の割合）：5・20・100・500 ラウンドの線を残す。右へ動くほど「自信のある正解」
--   重み ∝ exp(−yᵢF(xᵢ))：マージンの小さい点ほど重くなり、次の切り株はその点を当てにいく
-- 操作：スペース 一時停止 / ← → データの形（ノイズなし）/ R データを作り直す
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

nTrain, nTest, maxR, cells :: Int
nTrain = 200               -- ★ 点の数
nTest = 2000
maxR = 500                 -- ★ ラウンドの上限
cells = 70

snapsAt :: [Int]
snapsAt = [5, 20, 100, 500]   -- ★ 残す分布のラウンド

-- 1 ラウンドの記録：訓練誤差・テスト誤差・最小マージン
data Hist = Hist { hTr :: !Double, hTe :: !Double, hMn :: !Double }

-- 1 ラウンドの中身（自己テスト用）：重み・ε・γ・α
data RoundInfo = RoundInfo { riW :: !(U.Vector Double), riEps :: !Double, riGam :: !Double, riAl :: !Double }

data World = World
  { wKind      :: !Int
  , wSeed      :: !Int
  , wPaused    :: !Bool
  , wTrain     :: !DataSet
  , wTestD     :: !DataSet
  , wYs        :: !(U.Vector Double)     -- 訓練の y（±1）
  , wYt        :: !(U.Vector Double)     -- テストの y（±1）
  , wF         :: !(U.Vector Double)     -- 訓練の点の F
  , wFt        :: !(U.Vector Double)     -- テストの点の F
  , wAlphaSum  :: !Double
  , wRounds    :: !Int
  , wHist      :: ![Hist]                -- 新しい順
  , wSnaps     :: ![(Int, U.Vector Double)]   -- (ラウンド, 並べたマージン)
  , wGrid      :: !(U.Vector Float)      -- マスごとの Σ α·(±1)（p5.js 版と同じ 32 ビット）
  , wFirstZero :: !Int
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-08 Margins of boosting" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart 2 1

restart :: Int -> Int -> World
restart kind seed =
  let tr = makeData2D kind nTrain 0 seed
      te = makeData2D kind nTest 0 (seed + 500)
      pm = U.map (\v -> fromIntegral (2 * v - 1))
  in World kind seed False tr te (pm (dsY tr)) (pm (dsY te)) (U.replicate nTrain 0) (U.replicate nTest 0) 0 0 [] [] (U.replicate (cells * cells) 0) (-1) []

-- 重み ∝ exp(−yF)（大きな数にならないよう最小を引く）
boostWeights :: World -> U.Vector Double
boostWeights w =
  let yf = U.zipWith (*) (wYs w) (wF w)
      mn = U.foldl' min posInf yf
      ws = U.map (\v -> exp (-(v - mn))) yf
      s = sumU ws
  in U.map (/ s) ws

hOf :: Stump -> Pt -> Double
hOf st x = if stumpProb st x > 0.5 then 1 else -1

-- ★ 1 ラウンド：重み → 重みつき誤りが最小の切り株 → γ = 1 − 2ε、α = ½ ln((1 + γ)/(1 − γ)) → F に足す
boostRound :: Bool -> World -> (World, RoundInfo)
boostRound fillGrid w =
  let ws = boostWeights w
      xs = dsX (wTrain w)
      st = fitStump xs (dsY (wTrain w)) (Just ws)
      h = U.fromList [ hOf st x | x <- V.toList xs ]
      eps = clampv (U.ifoldl' (\e i v -> if v /= wYs w U.! i then e + ws U.! i else e) 0 h) 1e-10 (0.5 - 1e-10)
      gam = 1 - 2 * eps
      al = 0.5 * log ((1 + gam) / (1 - gam))
      f' = U.zipWith (\f hv -> f + al * hv) (wF w) h
      ft' = U.imap (\i f -> f + al * hOf st (dsX (wTestD w) V.! i)) (wFt w)
      asum = wAlphaSum w + al
      grid' = if fillGrid then U.imap (\q g -> realToFrac (realToFrac g + al * hOf st (cellPt cells q) :: Double)) (wGrid w) else wGrid w
      mar = U.zipWith (\y f -> y * f / asum) (wYs w) f'
      cnt p v = fromIntegral (U.length (U.filter p v))
      hh = Hist (cnt (<= 0) mar / fromIntegral nTrain) (cnt (<= 0) (U.zipWith (*) (wYt w) ft') / fromIntegral nTest) (U.foldl' min posInf mar)
      r = wRounds w + 1
      w' = w { wF = f', wFt = ft', wAlphaSum = asum, wGrid = grid', wRounds = r, wHist = hh : wHist w
             , wFirstZero = if wFirstZero w < 0 && hTr hh == 0 then r else wFirstZero w
             , wSnaps = if r `elem` snapsAt then (r, U.fromList (sortOn id (U.toList mar))) : wSnaps w else wSnaps w }
  in (w', RoundInfo ws eps gam al)

margins :: World -> U.Vector Double
margins w = U.zipWith (\y f -> if wAlphaSum w > 0 then y * f / wAlphaSum w else 0) (wYs w) (wF w)

-- 1 フレームに 2 ラウンド
step :: Env -> Float -> World -> IO World
step _ _ w = pure (go (2 :: Int) w)
  where go 0 v = v
        go k v = if wPaused v || wRounds v >= maxR then v else go (k - 1) (fst (boostRound True v))

-- i 番目のラウンドの記録（1 から）
histAt :: World -> Int -> Hist
histAt w r = headOr (Hist 0 0 0) (drop (wRounds w - r) (wHist w))

draw :: Env -> World -> Picture
draw env w = view env
  [ imageBox 40 110 side side cells cells region
  , frameBox bx frameCol
  , Pictures [ Pictures [Color (grayC 10) (circleAt px py (d + 2)), Color (colOf (clsCol (dsY (wTrain w) U.! i))) (circleAt px py d)]
             | i <- [0 .. nTrain - 1]
             , let x = dsX (wTrain w) V.! i
                   (px, py) = toPx bx (x U.! 0) (x U.! 1)
                   d = if wRounds w > 0 then f2 (clampv (sqrt (ws U.! i * fromIntegral nTrain) * 7) 3 22) else 7 ]
  , if wRounds w > 0 then let x = dsX (wTrain w) V.! iMin
                              (px, py) = toPx bx (x U.! 0) (x U.! 1)
                          in Color (grayC 255) (ringAt px py 26 2) else Blank
  , sayWrap "Colour strength = |F| / sum alpha, dot size = current weight, white ring = point with the smallest margin" 40 (110 + side + 6) side 14 (rgbC 200 185 170)
  -- ---- 右上：マージンの累積分布
  , plotAxes g "margin y*F(x)/sum alpha" "margin CDF (share of training points <= value)" [-1, -0.5, 0, 0.5, 1] [0, 0.5, 1]
  , Color (colOf frameCol) (lineW 1 (gX g 0, grY g) (gX g 0, grY g + grH g))
  , Pictures [ Pictures [ Color (colOf c) (polyline 1.5 (cdf arr))
                        , say ("r=" ++ show r) (gX g (arr U.! (nTrain `div` 2)) + 6) (gY g 0.5 + 14 * fromIntegral q - 20 + 5) 14 (colOf c) ]
             | (q, r, c) <- zip3 [0 :: Int ..] snapsAt snapCols, Just arr <- [lookup r (wSnaps w)], U.length arr == nTrain ]
  , if wRounds w > 0 then Color (colOf gold) (polyline 3 (cdf (U.fromList (sortOn id (U.toList mar))))) else Blank
  -- ---- 右下：誤差と最小マージン（横軸は対数）
  , plotAxes g2 "round (log scale: 1, 10, 100, 500)" "red = train error, teal = test error, gold = min margin" [0, 1, 2, logBase 10 (fromIntegral maxR)] [0, 0.1, 0.2]
  , Color (colOf frameCol) (lineW 1 (grX g2, gY g2 0) (grX g2 + grW g2, gY g2 0))
  , Pictures [ Color (colOf c) (polyline 2.2 [ gPt g2 (logBase 10 (fromIntegral k)) (sel hh) | (k, hh) <- zip [1 :: Int ..] hs ])
             | (sel, c) <- [(hTr, wine), (hTe, teal), (hMn, gold)] ]
  , if wFirstZero w > 0 then let xx = gX g2 (logBase 10 (fromIntegral (wFirstZero w)))
                             in Color (rgbaC 255 255 255 120) (dashed 1 4 4 [(xx, grY g2), (xx, grY g2 + grH g2)]) else Blank
  -- ---- 文字
  , say ("Boosting seen through margins   data: " ++ shapeName (wKind w) ++ "   round " ++ show (wRounds w) ++ (if wPaused w then " (paused)" else "")) 40 40 20 (colOf ink)
  , say "Space pause   <- -> data shape   R new data" 40 66 15 (rgbC 170 155 145)
  , case wHist w of
      (l : _) -> say ("train error " ++ pctStr (hTr l) ++ "   test error " ++ pctStr (hTe l) ++ "   min margin " ++ fmtNum 3 (hMn l)
                      ++ (if wFirstZero w > 0 then "   (train error 0 at round " ++ show (wFirstZero w) ++ ": dashed)" else "")) 40 (envH env - 40) 14 (rgbC 220 210 195)
      [] -> Blank
  , drawSelfTest env (wTest w) ]
  where
    side = min (envH env - 190) (envW env * 0.4)
    bx = Box 40 110 side side
    asum = if wAlphaSum w == 0 then 1 else wAlphaSum w
    region = gridImage cells (\q -> probCol (0.5 + 0.5 * clampv (3 * (realToFrac (wGrid w U.! q) / asum)) (-1) 1) 0.8)
    mar = margins w
    ws = boostWeights w
    iMin = U.ifoldl' (\b i m -> if m < mar U.! b then i else b) 0 mar
    rx = 40 + side + 70
    g = Graph rx 110 (envW env - rx - 50) (envH env * 0.33) (-1) 1 0 1
    cdf arr = let n = fromIntegral (U.length arr) :: Double
              in gPt g (-1) 0 : concat [ [gPt g m (fromIntegral k / n), gPt g m (fromIntegral (k + 1) / n)] | (k, m) <- zip [0 :: Int ..] (U.toList arr) ] ++ [gPt g 1 1]
    snapCols = [(150, 130, 130), (196, 72, 96), (110, 165, 240), (64, 186, 176)]
    g2 = Graph rx (grY g + grH g + 70) (grW g) (envH env - grY g - grH g - 170) 0 (logBase 10 (fromIntegral maxR)) (-0.05) 0.25
    hs = reverse (wHist w)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> keep (restart ((wKind w + 1) `mod` 6) (wSeed w))
  EventKey (SpecialKey KeyLeft) Down _ _ -> keep (restart ((wKind w + 5) `mod` 6) (wSeed w))
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> keep (restart (wKind w) (wSeed w + 1))
  _ -> w
  where keep v = v { wTest = wTest w }

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("選んだ切り株の重みつき誤り ε = 総当たりの最小（最初の 20 ラウンド）、重みの合計はいつも 1", okBest && okW)
  , ("指数損失の恒等式：(1/n) Σ exp(−yᵢF(xᵢ)) = Π √(1 − γ_t²)（500 ラウンド、相対 1e-9）→ 訓練誤差の上限", okId && hTr (histAt wEnd maxR) <= prodEnd + 1e-12)
  , ("マージンはすべて −1〜1（Σ|α·h| = Σα で割っているから）", U.all (\v -> v >= -1 - 1e-12 && v <= 1 + 1e-12) (margins wEnd))
  , ("円：訓練誤差 0 は " ++ show z ++ " 回目。その後も最小マージンは増え続ける（" ++ intercalate " → " (map (fmtNum 3) cps) ++ "）"
    , z > 0 && z < 250 && and (zipWith (<) cps (drop 1 cps)))
  , ("訓練誤差 0 のあと 500 回まで続けても、テスト誤差はほとんど上がらない（" ++ pctStr (hTe (histAt wEnd z)) ++ " → " ++ pctStr (hTe (histAt wEnd maxR)) ++ "）"
    , hTe (histAt wEnd maxR) <= hTe (histAt wEnd z) + 0.02) ]
  where
    w0 = restart 2 1
    xs = dsX (wTrain w0)
    ys = dsY (wTrain w0)
    -- (ラウンド番号, ラウンドの中身, 足した後の World, ここまでの Π √(1 − γ²))
    runs = go 0 1 w0
    go t prod w | t >= maxR = []
                | otherwise = let (w', r) = boostRound False w
                                  prod' = prod * sqrt (1 - riGam r * riGam r)
                              in (t, r, w', prod') : go (t + 1) prod' w'
    (wEnd, prodEnd) = case reverse runs of
      ((_, _, w', p) : _) -> (w', p)
      [] -> (w0, 1)
    okW = all (\(_, r, _, _) -> abs (sumU (riW r) - 1) <= 1e-9) runs
    okId = all (\(_, _, w', p) -> let ex = meanOf (U.toList (U.zipWith (\y f -> exp (-y * f)) (wYs w') (wF w'))) in abs (ex / p - 1) <= 1e-9) runs
    okBest = all (\(_, r, _, _) -> abs (bruteBest (riW r) - riEps r) <= 1e-9) (take 20 runs)
    -- 総当たり：全部の特徴量 × 全部の中点のしきい値で、左右を多数決にしたときの重みつき誤り
    bruteBest wv = minOr posInf
      [ minimum2 wl + minimum2 wr
      | f <- [0, 1]
      , let vals = map head' (groupSorted (sortOn id [ x U.! f | x <- V.toList xs ]))
      , (a, b) <- zip vals (drop 1 vals)
      , let th = (a + b) / 2
            (wl, wr) = foldl' (sideAdd wv f th) ((0, 0), (0, 0)) [0 .. V.length xs - 1] ]
    sideAdd wv f th ((l0, l1), (r0, r1)) i =
      let x = xs V.! i
          wi = wv U.! i
          c = ys U.! i
      in if x U.! f <= th then (if c == 0 then (l0 + wi, l1) else (l0, l1 + wi), (r0, r1))
         else ((l0, l1), if c == 0 then (r0 + wi, r1) else (r0, r1 + wi))
    minimum2 (a, b) = min a b
    head' = headOr 0
    groupSorted [] = []
    groupSorted (v : vs) = let (same, rest) = span (== v) vs in (v : same) : groupSorted rest
    z = wFirstZero wEnd
    cps = map (hMn . histAt wEnd) [z, max (z + 1) 100, 250, 500]

-- ---------- やってみよう ----------
-- 1. 白い輪（マージン最小の点）はどこにある？ 境界のすぐそば？ その点の大きさ（重み）は？
-- 2. 累積分布の線が、ラウンドとともに右へ動くのを見よう。訓練誤差 0（線が 0 の左にない）のあとも動き続ける
-- 3. ← → で XOR やしまのデータにすると、訓練誤差 0 までに何ラウンドかかる？ マージンはどこまで広がる？
-- 4. series62/10 の AdaBoost は α = ln((1 − ε)/ε)。ここは ½ がつくが、マージンは Σα で割るので同じ形になる
-- 5. ノイズ（まちがったラベル）があると、その点のマージンを広げようとして重みが集中する。makeData2D の 0 を 0.05 に

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
