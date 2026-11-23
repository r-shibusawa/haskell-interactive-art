-- ============================================
-- 62-11 勾配ブースティング ─ 「いまの予測のまちがい（残差）」に小さな回帰木をあてはめて、少しずつ足していく
-- F₀ = 平均。くり返し：残差 r = y − F(x) → 深さの小さい回帰木 h を r にあてはめる → F ← F + ν·h（ν = 学習率、縮小）
--   2 乗誤差の「勾配」がちょうど残差なので、これは関数の世界での勾配降下法
--   ν が小さいとゆっくり近づき（木がたくさん要る）、大きいと速いがノイズまでなぞりやすい（テスト誤差が上がる）
-- 操作：スペース 一時停止 / ↑↓ 学習率 ν / ← → 木の深さ / R やり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, nub, sort, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nus :: [Double]
nus = [0.02, 0.05, 0.1, 0.2, 0.5, 1]

nTrain, mMax, curveN :: Int
nTrain = 50                -- ★ 訓練の点の数
mMax = 300                 -- ★ 木の本数の上限
curveN = 300               -- 黄の曲線の点の数 − 1

noiseS :: Double
noiseS = 0.25              -- ★ ノイズの大きさ

-- ★ 本当の関数
hidden :: Double -> Double
hidden x = sin (7 * x) + 0.5 * cos (19 * x) * x + (if x > 0.7 then 0.6 else 0)

data World = World
  { wNuIdx  :: !Int
  , wDepth  :: !Int
  , wSeed   :: !Int
  , wPaused :: !Bool
  , wXtr    :: !(V.Vector Pt)
  , wYtr    :: !(U.Vector Double)
  , wXte    :: !(V.Vector Pt)
  , wYte    :: !(U.Vector Double)
  , wGbm    :: !GBM
  , wHist   :: [(Double, Double)]     -- (訓練, テスト) の平均 2 乗誤差。新しい順
  , wTestF  :: !(U.Vector Double)     -- テストの点の予測
  , wCurve  :: !(U.Vector Double)     -- 0〜1 の 301 点での F_m(x)（1 本ずつ足していく）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-11 Gradient boosting" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPaused w then w else addN 2 w)   -- 1 フレームに 2 本
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newData (World 2 2 6 False V.empty U.empty V.empty U.empty (makeGBM V.empty U.empty 0.1 2) [] U.empty U.empty [])

-- 訓練 n 個とテスト 300 個（同じ乱数の列から続けて引く）
makeReg :: Int -> Int -> Double -> R ((V.Vector Pt, U.Vector Double), (V.Vector Pt, U.Vector Double))
makeReg seed n s = do
  seedRng seed
  let one = do x <- rnd; g <- rndGauss; pure (U.singleton x, hidden x + s * g)
  a <- mapM (const one) [1 .. n]
  b <- mapM (const one) [1 .. 300 :: Int]
  let pack ps = (V.fromList (map fst ps), U.fromList (map snd ps))
  pure (pack a, pack b)

newData :: World -> World
newData w = let (((xa, ya), (xb, yb)), _) = evalR (rngOf 1) (makeReg (wSeed w) nTrain noiseS)
            in restart w { wXtr = xa, wYtr = ya, wXte = xb, wYte = yb }

restart :: World -> World
restart w = let g = makeGBM (wXtr w) (wYtr w) (headOr 0.1 (drop (wNuIdx w) nus)) (wDepth w)
            in w { wGbm = g, wHist = [], wTestF = U.map (const (gF0 g)) (wYte w), wCurve = U.replicate (curveN + 1) (gF0 g), wPaused = False }

-- ★ 1 本足す：残差に回帰木 → 予測を更新（テストの予測も同じ木で更新）
addOne :: World -> World
addOne w =
  let (g, t) = gbmAdd (wGbm w)
      nu = gNu g
      tf = U.imap (\i v -> v + nu * regPredict t (wXte w V.! i)) (wTestF w)
      cv = U.imap (\q v -> v + nu * regPredict t (U.singleton (fromIntegral q / fromIntegral curveN))) (wCurve w)
      h = (mseOf (U.toList (gFit g)) (U.toList (wYtr w)), mseOf (U.toList tf) (U.toList (wYte w)))
  in w { wGbm = g, wTestF = tf, wCurve = cv, wHist = h : wHist w }

addN :: Int -> World -> World
addN k w = if k <= 0 || length (gTrees (wGbm w)) >= mMax then w else addN (k - 1) (addOne w)

draw :: Env -> World -> Picture
draw env w = view env
  -- ---- データと予測
  [ plotAxes g "x" "y (white = true function, yellow = F_m(x), vertical lines = residuals)" [0, 0.5, 1] [-1, 0, 1, 2]
  , Color (rgbaC 255 255 255 80) (polyline 1.5 [gPt g (q / 200) (hidden (q / 200)) | q <- map fromIntegral [0 .. 200 :: Int]])
  , Pictures [ Color (rgbaC 245 140 75 150) (lineW 1 (gPt g x yv) (gPt g x fv))
             | i <- [0 .. V.length (wXtr w) - 1], let x = wXtr w V.! i U.! 0, let yv = wYtr w U.! i, let fv = gFit gb U.! i ]
  , Pictures [ Color (clsC 1) (uncurry circleAt (gPt g (wXtr w V.! i U.! 0) (wYtr w U.! i)) 7) | i <- [0 .. V.length (wXtr w) - 1] ]
  , Color (rgbC 255 225 110) (polyline 2.5 [gPt g (fromIntegral q / fromIntegral curveN) v | (q, v) <- zip [0 :: Int ..] (U.toList (wCurve w))])
  -- ---- 最後に足した木 ν·h(x)
  , plotAxes g2 "x" "last tree added  nu * h_m(x)  (small regression tree fitted to the residuals)" [0, 0.5, 1] [-0.5, 0, 0.5]
  , case reverse (gTrees gb) of
      (t : _) -> Color (clsC 2) (polyline 2.5 [gPt g2 x (gNu gb * regPredict t (U.singleton x)) | q <- [0 .. 300 :: Int], let x = fromIntegral q / 300])
      [] -> Blank
  -- ---- 誤差のグラフ
  , plotAxes g3 "trees m" "mean squared error (orange = train, green = test)" [0, 100, 200, 300] [0, 0.2, 0.4, 0.6]
  , Color (rgbC 245 160 80) (polyline 2.5 [gPt g3 (fromIntegral k) tr | (k, (tr, _)) <- hs])
  , Color (rgbC 175 225 95) (polyline 2.5 [gPt g3 (fromIntegral k) te | (k, (_, te)) <- hs])
  , Color (rgbaC 255 255 255 60) (lineW 1 (grX g3, gyOf g3 (noiseS * noiseS)) (grX g3 + grW g3, gyOf g3 (noiseS * noiseS)))
  , case best of
      Just (k, te) -> Color (rgbC 255 240 150) (uncurry circleAt (gPt g3 (fromIntegral k) te) 10)
      Nothing -> Blank
  -- ---- 文字
  , say ("Gradient boosting   learning rate nu = " ++ fmtNum 3 (gNu gb) ++ "   tree depth " ++ show (wDepth w) ++ "   trees " ++ show m ++ " / " ++ show mMax
         ++ (if wPaused w then "  (paused)" else "")) 40 40 20 (grayC 240)
  , say "Space pause   Up/Down learning rate nu   <- -> tree depth   R new data" 40 66 15 (grayC 170)
  , case (wHist w, best) of
      ((tr, te) : _, Just (k, bte)) -> Pictures
        [ say ("train " ++ fmtNum 4 tr ++ "   test " ++ fmtNum 4 te) gx ty 16 (grayC 225)
        , say ("lowest test error at tree " ++ show k ++ " (" ++ fmtNum 4 bte ++ ")") gx (ty + 26) 16 (rgbC 255 240 150) ]
      _ -> Blank
  , sayWrap "White line = noise variance sigma^2 (test error below it is normally impossible). Smaller nu with more trees: slower, but it tends to reach a lower test error (shrinkage)."
      gx (ty + 50) (grW g3) 14 (grayC 160)
  , drawSelfTest env (wTest w) ]
  where
    gb = wGbm w
    m = length (gTrees gb)
    g = Graph 70 110 (envW env * 0.52) (envH env * 0.5) 0 1 (-1.8) 2
    g2 = Graph 70 (grY g + grH g + 70) (grW g) (envH env - grY g - grH g - 150) 0 1 (-0.6) 0.6
    gx = grX g + grW g + 70
    g3 = Graph gx 130 (envW env - gx - 50) (envH env * 0.42) 0 (fromIntegral mMax) 0 0.6
    hs = zip [1 :: Int ..] (reverse (wHist w))
    -- テストがいちばん小さい本数（同じなら先のほう）
    best = foldl' (\b (k, (_, te)) -> case b of
                                        Just (_, bt) | not (te < bt) -> b
                                        _ -> Just (k, te)) Nothing hs
    ty = grY g3 + grH g3 + 60

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wNuIdx = min (length nus - 1) (wNuIdx w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wNuIdx = max 0 (wNuIdx w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wDepth = min 4 (wDepth w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wDepth = max 1 (wDepth w - 1) }
  EventKey (Char c) Down _ _ | c == 'r' || c == 'R' -> newData w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("F₀ = y の平均", near (gF0 g0) (sumU y / fromIntegral (U.length y)) 1e-9 && U.all (== gF0 g0) (gFit g0))
  , ("1 本目の木の葉の値 = その葉に入った点の残差 y − F₀ の平均", okLeaf)
  , ("ν = 0.1：訓練の 2 乗誤差は 1 本ごとに減るか同じ（150 本）", and (zipWith (\a b -> b <= a + 1e-12) errs (drop 1 errs)))
  , ("残差の合計はいつも 0（葉の値が平均だから）", all (\gm -> abs (sumU (U.zipWith (-) y (gFit gm))) <= 1e-8) (take 150 (drop 2 gs)))
  , ("まとめて計算した予測 F(x) = 1 本ずつ足してきた予測（全部の点）", and [near (gbmPredict gEnd (length (gTrees gEnd)) (x V.! i)) (gFit gEnd U.! i) 1e-9 | i <- [0 .. V.length x - 1]])
  , ("ν = 1・深さ 1 の 1 本目 = y にそのまま回帰の切り株をあてはめた予測", and [near (gFit s1 U.! i) (regPredict st (x V.! i)) 1e-9 | i <- [0 .. V.length x - 1]])
  , ("ν = 1・深さ 3 では過学習：60 本目のテスト誤差 " ++ fmtNum 3 (lastOr 0 tes) ++ " > 途中の最小 " ++ fmtNum 3 (minOr 0 tes), lastOr 0 tes > minOr 0 tes + 1e-6) ]
  where
    (((x, y), (xt, yt)), _) = evalR (rngOf 1) (makeReg 23 50 0.25)
    g0 = makeGBM x y 0.1 2
    gs = iterate (fst . gbmAdd) g0            -- 0 本、1 本、2 本、…
    (g1, t1) = gbmAdd g0
    r0 = U.map (subtract (gF0 g0)) y
    okLeaf = and [ abs (nValue q - foldl' (+) 0 (map (r0 U.!) ids) / fromIntegral (length ids)) <= 1e-12
                 | q <- treeNodes t1, nLeaf q
                 , let ids = [i | i <- [0 .. V.length x - 1], nId (leafOf t1 (x V.! i)) == nId q] ]
    errs = [mseOf (U.toList (gFit gm)) (U.toList y) | gm <- take 1 gs ++ take 150 (drop 2 gs)]   -- p5.js と同じ：F₀ の次は 2 本目と比べる
    gEnd = headOr g1 (drop 151 gs)
    s1 = fst (gbmAdd (makeGBM x y 1 1))
    st = growReg 1 1 Nothing x y
    hs3 = take 60 (drop 1 (iterate (fst . gbmAdd) (makeGBM x y 1 3)))
    tes = [mseOf [gbmPredict h (length (gTrees h)) p | p <- V.toList xt] (U.toList yt) | h <- hs3]

-- ---------- やってみよう ----------
-- 1. ↑↓ で ν を 0.02 と 1 にして、黄の曲線の近づき方とテスト誤差を比べよう
-- 2. ← → で木の深さを 1（切り株）と 4 に。深さ 1 でも、たくさん足すと曲線を表せる？
-- 3. 縦の線（残差）がだんだん短くなる。最後に足した木（下のグラフ）はどこを直している？
-- 4. テスト誤差がいちばん小さい本数で止めるのが「早期打ち切り」。ν を小さくすると何本目になる？
-- 5. 表形式のデータの勝負では、この方法（XGBoost・LightGBM など）がとても強い

-- ==================================================
-- ここから下は「決定木の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・いちばん大きい番号
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1, 2）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

-- いちばん大きい番号（同点なら小さい番号）
argMaxU :: U.Vector Double -> Int
argMaxU a = if U.null a then 0 else U.ifoldl' (\b i v -> if v > a U.! b then i else b) 0 a

maxU :: U.Vector Double -> Double
maxU a = if U.null a then 0 else U.maximum a

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- リストの最初（なければ d）
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

-- ---- 不純度：counts = 各クラスの個数（重みつきでもよい）
data Crit = Gini | Entropy | MisErr deriving (Eq, Show)

log2 :: Double -> Double
log2 q = log q / log 2

entropyP :: Double -> Double               -- 2 クラスのエントロピー（ビット）
entropyP p = if p <= 0 || p >= 1 then 0 else -p * log2 p - (1 - p) * log2 (1 - p)

entropyOf :: U.Vector Double -> Double
entropyOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\h c -> if c > 0 then let q = c / n in h - q * log2 q else h) 0 counts)

giniOf :: U.Vector Double -> Double
giniOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\s c -> s - (c / n) * (c / n)) 1 counts)

errOf :: U.Vector Double -> Double           -- 誤分類率
errOf counts = let n = sumU counts in if n <= 0 then 0 else max 0 (1 - maxU counts / n)

impurityOf :: Crit -> U.Vector Double -> Double
impurityOf crit = case crit of
  Entropy -> entropyOf
  MisErr -> errOf
  Gini -> giniOf

-- 分けたときの「不純度の減り」= 親 − (左の割合·左 + 右の割合·右)。Entropy なら情報利得
gainOf :: Crit -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Double
gainOf crit parent left right =
  let n = sumU parent
      nl = sumU left
      nr = sumU right
  in if n <= 0 then 0 else impurityOf crit parent - (nl / n) * impurityOf crit left - (nr / n) * impurityOf crit right

cnt :: [Double] -> U.Vector Double
cnt = U.fromList

-- ---- CART（分類）：全部の特徴量 × 全部のしきい値（並べた値の中点）を調べて、不純度がいちばん減る分割を選ぶ
-- ノード：分類の木も回帰の木もこの形。葉の nLeft, nRight は使わない
data Node = Node
  { nLeaf   :: !Bool
  , nFeat   :: !Int                 -- 何番目の特徴量で切るか（0 = x1）
  , nThr    :: !Double              -- しきい値（x ≤ thr なら左）
  , nLeft   :: Node
  , nRight  :: Node
  , nCounts :: !(U.Vector Double)   -- 各クラスの個数（重みつき）
  , nN      :: !Int                 -- 点の数
  , nImp    :: !Double              -- 不純度
  , nPred   :: !Int                 -- 多数決のクラス
  , nDec    :: !Double              -- 分けたときの重みつきの不純度の減り（回帰木では SSE の減り）
  , nDepth  :: !Int
  , nId     :: !Int                 -- 行きがけ順の番号（0 = 根）
  , nCut    :: !Double              -- 剪定：この α 以上で葉として扱う（刈られないなら +∞）
  , nValue  :: !Double              -- 回帰木：葉の値（平均）
  , nSse    :: !Double              -- 回帰木：2 乗誤差
  }

nilNode :: Node
nilNode = Node True 0 0 nilNode nilNode U.empty 0 0 0 0 0 0 posInf 0 0

-- o = { maxDepth, minLeaf, crit, mtry（分割ごとに見る特徴量の数、0 = 全部）, w（点の重み）, idx（使う点の番号）, nClass（0 = 自動）}
data TreeOpt = TreeOpt
  { oMaxDepth :: !Int, oMinLeaf :: !Int, oCrit :: !Crit, oMtry :: !Int
  , oW :: !(Maybe (U.Vector Double)), oIdx :: !(Maybe (U.Vector Int)), oNClass :: !Int }

defOpt :: TreeOpt
defOpt = TreeOpt 40 1 Gini 0 Nothing Nothing 0

-- 育てるときの道具箱：cCols = 特徴量ごとの列、cW = 点の重み
data Ctx = Ctx { cCols :: !(V.Vector (U.Vector Double)), cY :: !(U.Vector Int), cW :: !(U.Vector Double)
               , cK :: !Int, cD :: !Int, cO :: !TreeOpt }

-- 木を育てる（mtry を使うときだけ乱数を引く）
-- 速くするため、根で特徴量ごとに点を 1 回だけ並べ、子にはその並びを分けて渡す（並べ直したのと同じ順になる）
growTree :: TreeOpt -> V.Vector Pt -> U.Vector Int -> R Node
growTree o xs ys = numberTree <$> growNode ctx idx0 ords0 0
  where
    k = if oNClass o > 0 then oNClass o else max 1 (if U.null ys then 1 else U.maximum ys + 1)
    d = if V.null xs then 0 else U.length (V.head xs)
    n = V.length xs
    idx0 = fromMaybe (U.enumFromN 0 n) (oIdx o)
    cols = V.generate d (\f -> U.generate n (\i -> xAt xs i f))
    ords0 = V.map (\col -> U.fromList (sortOn (col U.!) (U.toList idx0))) cols
    ctx = Ctx cols ys (fromMaybe (U.replicate n 1) (oW o)) k d o { oMinLeaf = max 1 (oMinLeaf o) }

-- 乱数を使わない木（mtry = 0）
growTreeP :: TreeOpt -> V.Vector Pt -> U.Vector Int -> Node
growTreeP o xs ys = fst (runR (growTree o { oMtry = 0 } xs ys) (Rng 1))

-- idx = このノードの点（重複あり）、ords = 特徴量ごとに値の順に並べた idx
growNode :: Ctx -> U.Vector Int -> V.Vector (U.Vector Int) -> Int -> R Node
growNode ctx idx ords depth = do
  let o = cO ctx
      counts = U.accumulate (+) (U.replicate (cK ctx) 0) (U.map (\i -> (cY ctx U.! i, cW ctx U.! i)) idx)
      node = nilNode { nLeaf = True, nCounts = counts, nN = U.length idx, nImp = impurityOf (oCrit o) counts
                     , nPred = argMaxU counts, nDepth = depth, nDec = 0 }
      kinds = U.length (U.filter (> 0) counts)
  if depth >= oMaxDepth o || U.length idx < 2 * oMinLeaf o || kinds <= 1 then pure node else do
    mb <- bestSplit ctx ords counts
    case mb of
      Nothing -> pure node
      Just (f, thr, dec) -> do
        let col = cCols ctx V.! f
            goL i = col U.! i <= thr
            (l, r) = U.partition goL idx
        lt <- growNode ctx l (V.map (U.filter goL) ords) (depth + 1)
        rt <- growNode ctx r (V.map (U.filter (not . goL)) ords) (depth + 1)
        pure node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }

-- ★ いちばんよい分割：特徴量ごとに点を並べ、左から 1 つずつ動かしながら左右の個数を更新する
bestSplit :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> R (Maybe (Int, Double, Double))
bestSplit ctx ords counts = do
  let d = cD ctx
      mtry = oMtry (cO ctx)
  feats <- if mtry > 0 && mtry < d then U.toList <$> shuffleU (U.enumFromN 0 d) else pure [0 .. d - 1]   -- ランダムフォレスト：前の mtry 個だけを見る
  let first = if mtry > 0 then min mtry d else d
      best = splitOver ctx ords counts (take first feats)
  pure (if isNothing best && first < d then splitOver ctx ords counts (drop first feats) else best)   -- 切れる特徴量が 1 つもないときだけ残りを見る

splitOver :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> [Int] -> Maybe (Int, Double, Double)
splitOver ctx ords counts = foldl' feat Nothing
  where
    o = cO ctx
    crit = oCrit o
    minLeaf = oMinLeaf o
    w = sumU counts
    pImp = impurityOf crit counts
    feat best f =
      let ord = ords V.! f
          col = cCols ctx V.! f
          len = U.length ord
          go !k !lc !lw !b
            | k >= len - 1 = b
            | otherwise =
                let i = U.unsafeIndex ord k
                    wi = U.unsafeIndex (cW ctx) i
                    yi = U.unsafeIndex (cY ctx) i
                    lc' = U.unsafeUpd lc [(yi, U.unsafeIndex lc yi + wi)]
                    lw' = lw + wi
                    v0 = U.unsafeIndex col i
                    v1 = U.unsafeIndex col (U.unsafeIndex ord (k + 1))
                in if not (v1 > v0) || k + 1 < minLeaf || len - k - 1 < minLeaf then go (k + 1) lc' lw' b else
                   let rc = U.zipWith (-) counts lc'
                       dec = w * pImp - lw' * impurityOf crit lc' - (w - lw') * impurityOf crit rc
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) lc' lw' b'
      in go 0 (U.replicate (cK ctx) 0) 0 best

-- 行きがけ順に番号をつけ直す（p5.js 版の node.id と同じ順）
numberTree :: Node -> Node
numberTree t0 = fst (go 0 t0)
  where
    go !n t | nLeaf t = (t { nId = n }, n + 1)
            | otherwise = let (l, n1) = go (n + 1) (nLeft t)
                              (r, n2) = go n1 (nRight t)
                          in l `seq` r `seq` (t { nId = n, nLeft = l, nRight = r }, n2)

-- 見せる深さ dLim、剪定の α = aLim で「葉として扱う」か
isCut :: Int -> Double -> Node -> Bool
isCut dLim aLim t = nLeaf t || nDepth t >= dLim || aLim >= nCut t

noD :: Int                                    -- 深さの制限なし
noD = maxBound

-- 根から葉までたどる。dLim = 見せる深さ、aLim = 剪定の α（制限なし = noD, negInf）
leafOfL :: Int -> Double -> Node -> Pt -> Node
leafOfL dLim aLim node x
  | isCut dLim aLim node = node
  | otherwise = leafOfL dLim aLim (if x U.! nFeat node <= nThr node then nLeft node else nRight node) x

leafOf :: Node -> Pt -> Node
leafOf = leafOfL noD negInf

treePredict :: Node -> Pt -> Int
treePredict t x = nPred (leafOf t x)

treePredictL :: Int -> Double -> Node -> Pt -> Int
treePredictL dl al t x = nPred (leafOfL dl al t x)

-- 見えているノードを全部（行きがけ順）
treeNodesL :: Int -> Double -> Node -> [Node]
treeNodesL dl al t = t : if isCut dl al t then [] else treeNodesL dl al (nLeft t) ++ treeNodesL dl al (nRight t)

treeNodes :: Node -> [Node]
treeNodes = treeNodesL noD negInf

countLeavesL :: Int -> Double -> Node -> Int
countLeavesL dl al t = length (filter (isCut dl al) (treeNodesL dl al t))

countLeaves :: Node -> Int
countLeaves = countLeavesL noD negInf

treeDepthL :: Int -> Double -> Node -> Int
treeDepthL dl al t = maxOr 0 (map nDepth (treeNodesL dl al t))

treeDepth :: Node -> Int
treeDepth = treeDepthL noD negInf

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double   -- fn(x) → クラス
accuracyOf fn xs ys = let n = V.length xs
                          ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) [0 .. n - 1])
                      in if n > 0 then fromIntegral ok / fromIntegral n else 0

accuracyIdx :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> [Int] -> Double   -- idx の点だけ
accuracyIdx fn xs ys ids = let ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) ids)
                           in if null ids then 0 else fromIntegral ok / fromIntegral (length ids)

-- ---- 回帰木：葉の値 = 平均、分割は 2 乗誤差（SSE）がいちばん減るところ。SSE = Σy² − (Σy)²/n
sseOf :: Double -> Double -> Int -> Double
sseOf s s2 n = if n > 0 then max 0 (s2 - (s * s) / fromIntegral n) else 0

growReg :: Int -> Int -> Maybe (U.Vector Int) -> V.Vector Pt -> U.Vector Double -> Node   -- maxDepth, minLeaf, idx
growReg maxDepth minLeaf0 mIdx xs ys = numberTree (growRegNode xs ys maxDepth (max 1 minLeaf0) idx0 0)
  where idx0 = fromMaybe (U.enumFromN 0 (V.length xs)) mIdx

growRegNode :: V.Vector Pt -> U.Vector Double -> Int -> Int -> U.Vector Int -> Int -> Node
growRegNode xs ys maxDepth minLeaf idx depth
  | depth >= maxDepth || n < 2 * minLeaf || sse0 < 1e-12 = node
  | otherwise = case best of
      Nothing -> node
      Just (f, thr, dec) ->
        let (l, r) = U.partition (\i -> xAt xs i f <= thr) idx
            lt = growRegNode xs ys maxDepth minLeaf l (depth + 1)
            rt = growRegNode xs ys maxDepth minLeaf r (depth + 1)
        in lt `seq` rt `seq` node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }
  where
    n = U.length idx
    (s, s2) = U.foldl' (\(!a, !b) i -> let v = ys U.! i in (a + v, b + v * v)) (0, 0) idx
    sse0 = sseOf s s2 n
    node = nilNode { nLeaf = True, nValue = if n > 0 then s / fromIntegral n else 0, nN = n, nSse = sse0, nDepth = depth, nDec = 0 }
    d = if V.null xs then 0 else U.length (V.head xs)
    best = foldl' feat Nothing [0 .. d - 1]
    feat b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) (U.toList idx))
          go !k !ls !ls2 !b
            | k >= n - 1 = b
            | otherwise =
                let v = ys U.! (ord U.! k)
                    ls' = ls + v
                    ls2' = ls2 + v * v
                    v0 = xAt xs (ord U.! k) f
                    v1 = xAt xs (ord U.! (k + 1)) f
                in if not (v1 > v0) || k + 1 < minLeaf || n - k - 1 < minLeaf then go (k + 1) ls' ls2' b else
                   let dec = sse0 - sseOf ls' ls2' (k + 1) - sseOf (s - ls') (s2 - ls2') (n - k - 1)
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) ls' ls2' b'
      in go 0 0 0 b0

regPredict :: Node -> Pt -> Double
regPredict t x = nValue (leafOf t x)

regPredictL :: Int -> Node -> Pt -> Double
regPredictL dl t x = nValue (leafOfL dl negInf t x)

mseOf :: [Double] -> [Double] -> Double
mseOf pr ys = if null ys then 0 else foldl' (+) 0 (zipWith (\p y -> (p - y) * (p - y)) pr ys) / fromIntegral (length ys)

-- ---- 勾配ブースティング（2 乗誤差）：残差 r = y − F(x) に小さな回帰木をあてはめ、F ← F + ν·木（ν = 学習率）
data GBM = GBM { gX :: !(V.Vector Pt), gY :: !(U.Vector Double), gNu :: !Double, gDepth :: !Int
               , gF0 :: !Double, gTrees :: [Node], gFit :: !(U.Vector Double) }

makeGBM :: V.Vector Pt -> U.Vector Double -> Double -> Int -> GBM
makeGBM xs ys nu md = let f0 = if U.null ys then 0 else sumU ys / fromIntegral (U.length ys)
                      in GBM xs ys nu md f0 [] (U.map (const f0) ys)

gbmAdd :: GBM -> (GBM, Node)
gbmAdd g =
  let r = U.zipWith (-) (gY g) (gFit g)
      t = growReg (gDepth g) 1 Nothing (gX g) r
      fit = U.imap (\i v -> v + gNu g * regPredict t (gX g V.! i)) (gFit g)
  in fit `seq` (g { gFit = fit, gTrees = gTrees g ++ [t] }, t)

gbmPredict :: GBM -> Int -> Pt -> Double     -- 前の m 本
gbmPredict g m x = foldl' (\s t -> s + gNu g * regPredict t x) (gF0 g) (take m (gTrees g))

-- ---- 描く道具：クラスの色、領域の絵、点、数の書き方
type RGB = (Double, Double, Double)

clsCol :: Int -> RGB                          -- ★ クラス A・B・C の色
clsCol c = case c of
  0 -> (245, 140, 75)
  1 -> (85, 175, 240)
  2 -> (175, 225, 95)
  _ -> (200, 200, 200)

bgCol :: RGB                                  -- 背景（森の色）
bgCol = (20, 30, 26)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

clsC :: Int -> Color
clsC = colOf . clsCol

-- クラスの割合 → 背景と混ぜた色（k = 色の濃さ）
mixCol :: U.Vector Double -> Double -> RGB
mixCol probs k =
  let s0 = sumU probs
      s = if s0 == 0 then 1 else s0
      add (r, g, b) (j, p) = let (cr, cg, cb) = clsCol j
                                 (br, bg, bb) = bgCol
                             in (r + (p / s) * (cr - br) * k, g + (p / s) * (cg - bg) * k, b + (p / s) * (cb - bb) * k)
  in foldl' add bgCol (zip [0 ..] (U.toList probs))

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ws = 点ごとの大きさの倍率）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Maybe (U.Vector Double) -> Picture
drawPoints bx xs ys r ws = Pictures
  [ Pictures [Color (grayC 15) (circleAt px py (d + 2)), Color (clsC (ys U.! i)) (circleAt px py d)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1)
        d = r * maybe 1 (\w -> f2 (w U.! i)) ws ]

-- 数を短く書く（toFixed(d) のあと余分な 0 を消す。p5.js 版の fmtNum）
-- toFixed と同じく、double のちょうどの値で丸め、ちょうど半分なら絶対値の大きいほうへ
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = stripZeros (if d > 0 then ip ++ "." ++ fp else ip)
                in if n == 0 || v > 0 then body else '-' : body
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if lastOr ' ' t == '.' then init t else t
                     | otherwise = s

-- 件数 [5|7] の形
countsStr :: U.Vector Double -> String
countsStr c = "[" ++ intercalate "|" (map (fmtNum 2) (U.toList c)) ++ "]"

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gxOf :: Graph -> Double -> Float
gxOf g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gyOf :: Graph -> Double -> Float
gyOf g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gxOf g a, gyOf g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 80 100 90) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gxOf g v) (grY g + grH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gyOf g v + 6) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (grayC 205), say yl (grX g) (grY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = if length pts < 2 then Blank else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

-- 段落をいくつか上から順に（左上 = (x, y)、段落の間は gap）
sayParas :: [String] -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
sayParas ps x y maxW size gap c = Pictures (go y ps)
  where go _ [] = []
        go yy (p : rest) = let n = length (wrapText maxW size p)
                           in sayWrap p x yy maxW size c : go (yy + fromIntegral n * size * 1.3 + gap) rest

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
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- ---- 数の道具
-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
