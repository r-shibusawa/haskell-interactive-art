-- ============================================
-- 62-12 特徴量の重要度 ─ フォレストは「どの特徴量で判断した」？ 2 つの測り方を比べる
-- 不純度の減少（MDI）：その特徴量で分けたノードのジニの減りを全部たす（学習のついでに計算できる）
-- 並べ替え（パーミュテーション）：テストの点で 1 つの列だけをシャッフル → 正解率がどれだけ下がるか（下がらない = 使っていない）
--   答えのルール：y = [x1 + 0.6·x2 + ノイズ > 0.8]。x1 がいちばん大事、x2 が次。「乱数」の列は関係なし
--   C で x1 のコピー（x1 + 少しのずれ）を足すと、x1 の重要度が 2 つに分かれてしまう（相関のある特徴量の落とし穴）
-- 操作：C x1 のコピーを足す / N 乱数の列の数 / R やり直す
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

nTrain, nTest, nTrees, reps :: Int
nTrain = 300
nTest = 400
nTrees = 60                -- ★ 木の本数
reps = 3                   -- ★ シャッフルのくり返し回数

data World = World
  { wCopy   :: !Bool        -- x1 のコピーがあるか
  , wNoise  :: !Int         -- ★ 関係のない乱数の列の数
  , wSeed   :: !Int
  , wNames  :: [String]
  , wTrain  :: !DataSet
  , wTestD  :: !DataSet
  , wForest :: !Forest
  , wVotes  :: !(U.Vector Int)   -- テストの点ごとの「クラス 1」の票（木を足すたびに足す）
  , wAcc    :: !Double
  , wMdi    :: [Double]
  , wPerm   :: [Maybe Double]
  , wPermF  :: !Int
  , wPhase  :: !Int         -- 0 木を育てる / 1 並べ替え / 2 おわり
  , wRep    :: !Int         -- 並べ替え：いまの列の何回目か（1 フレームに 1 回ずつ）
  , wBase   :: !Double      -- 並べ替え：シャッフル前の正解率
  , wDrop   :: !Double      -- 並べ替え：下がった正解率の合計
  , wRng    :: !Rng         -- 乱数の列（p5.js 版のグローバルな rngState）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-12 Feature importance" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (work w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World False 2 12 [] emptyD emptyD (makeForest V.empty U.empty defOpt) U.empty 0 [] [] 0 0 0 0 0 (rngOf 1) [])

emptyD :: DataSet
emptyD = DataSet V.empty U.empty

makeImp :: Int -> Bool -> Int -> Int -> R DataSet
makeImp n copy noise seed = do
  seedRng seed
  ps <- mapM (\_ -> do
          a <- rnd
          b <- rnd
          c <- if copy then (\g -> [clampv (a + 0.05 * g) 0 1]) <$> rndGauss else pure []
          zs <- mapM (const rnd) [1 .. noise]
          g <- rndGauss
          pure (U.fromList ([a, b] ++ c ++ zs), if a + 0.6 * b + 0.12 * g > 0.8 then 1 else 0)) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))

featNames :: Bool -> Int -> [String]
featNames copy noise = ["x1", "x2"] ++ ["x1 copy" | copy] ++ ["noise " ++ show k | k <- [1 .. noise]]

restart :: World -> World
restart w =
  let names = featNames (wCopy w) (wNoise w)
      ((tr, te), g) = evalR (wRng w) $ do
        a <- makeImp nTrain (wCopy w) (wNoise w) (wSeed w)
        b <- makeImp nTest (wCopy w) (wNoise w) (wSeed w + 99)
        seedRng (wSeed w * 5)
        pure (a, b)
      mtry = max 1 (floor (sqrt (fromIntegral (length names) :: Double) + 0.5))
  in w { wNames = names, wTrain = tr, wTestD = te, wRng = g
       , wForest = makeForest (dsX tr) (dsY tr) defOpt { oMtry = mtry, oNClass = 2 }
       , wVotes = U.replicate nTest 0, wAcc = 0, wMdi = map (const 0) names, wPerm = map (const Nothing) names, wPermF = 0, wPhase = 0, wRep = 0, wBase = 0, wDrop = 0 }

-- ★ 少しずつ計算：まず木を育てる（1 フレームに 2 本）→ 1 フレームに 1 回ずつシャッフルして測る（1 列 = 3 フレーム）
work :: World -> World
work w = case wPhase w of
  0 -> let grow (fo, vs, g) _ | length (fTrees fo) >= nTrees = (fo, vs, g)
                              | otherwise = let ((fo', t), g') = evalR g (forestAdd fo)
                                                vs' = U.imap (\i v -> v + treePredict t (dsX (wTestD w) V.! i)) vs
                                            in (fo', vs', g')
           (fo, vs, g) = foldl' grow (wForest w, wVotes w, wRng w) [1, 2 :: Int]
           m = length (fTrees fo)
           -- 多数決（同数ならクラス 0。forestPredict と同じ）
           ok = U.length (U.filter id (U.zipWith (\v y -> (if v > m - v then 1 else 0) == y) vs (dsY (wTestD w))))
       in w { wForest = fo, wVotes = vs, wRng = g, wMdi = impurityImportance (fTrees fo) (length (wNames w))
            , wAcc = fromIntegral ok / fromIntegral (max 1 nTest), wPhase = if m >= nTrees then 1 else 0 }
  1 -> let f = wPermF w
           te = wTestD w
           fn = forestPredict (wForest w)
           -- 列の 1 回目：種を決めて、シャッフル前の正解率を測る
           (base, g0) = if wRep w == 0 then (accuracyOf fn (dsX te) (dsY te), snd (evalR (wRng w) (seedRng (wSeed w * 7 + f)))) else (wBase w, wRng w)
           (col, g1) = evalR g0 (shuffleU (U.fromList (map (U.! f) (V.toList (dsX te)))))
           xp = V.imap (\i x -> x U.// [(f, col U.! i)]) (dsX te)
           drop' = wDrop w + (base - accuracyOf fn xp (dsY te))
           rep = wRep w + 1
       in if rep < reps then w { wRng = g1, wRep = rep, wBase = base, wDrop = drop' }
          else w { wPerm = [if k == f then Just (drop' / fromIntegral reps) else q | (k, q) <- zip [0 ..] (wPerm w)]
                 , wRng = g1, wRep = 0, wDrop = 0, wPermF = f + 1, wPhase = if f + 1 >= length (wNames w) then 2 else 1 }
  _ -> w

draw :: Env -> World -> Picture
draw env w = view env
  [ say "blue = impurity decrease (MDI, sums to 1)" bx (y0 - 34) 14 (rgbC 120 190 240)
  , say ("green = accuracy drop when shuffled (test points, mean of " ++ show reps ++ ")") bx (y0 - 14) 14 (rgbC 175 225 95)
  , Pictures (zipWith3 row [0 ..] (wNames w) (zip (wMdi w) (wPerm w)))
  -- ---- 文字
  , say ("Feature importance   trees " ++ show (length (fTrees (wForest w))) ++ " / " ++ show nTrees ++ "   test accuracy " ++ fmtNum 1 (wAcc w * 100) ++ "%"
         ++ (if wPhase w < 2 then "   (computing...)" else "")) 40 40 20 (grayC 240)
  , say ("C copy of x1 (now " ++ (if wCopy w then "on" else "off") ++ ")   N number of noise columns (" ++ show (wNoise w) ++ ")   R restart") 40 66 15 (grayC 170)
  , sayWrap "Rule: y = 1 <=> x1 + 0.6 x2 + noise > 0.8" tx 112 tw 15 (grayC 220)
  , sayParas [ "MDI = how often and how much a column was used while learning. Deep trees also cut on noise columns to fit the noise, so useless columns get a few points too."
             , "Permutation = does it really matter on the test set? Shuffling a noise column does not change the answers -> almost 0."
             , "With a copy of x1, trees can cut on either x1 or the copy -> the importance is split in two. Permutation also shows both small, because breaking one still leaves the other."
             , "Importance tells what the model used, not what causes y." ]
      tx 162 tw 14 18 (grayC 190)
  , drawSelfTest env (wTest w) ]
  where
    bx = 220
    bw = envW env * 0.42
    nf = length (wNames w)
    rowH = min 56 ((envH env - 300) / fromIntegral (max 1 nf))
    y0 = 140
    maxP = maxOr 0.05 (0.05 : [v | Just v <- wPerm w])
    tx = bx + bw + 60
    tw = envW env - tx - 40
    row :: Int -> String -> (Double, Maybe Double) -> Picture
    row f nm (md, p) =
      let yy = y0 + fromIntegral f * rowH
          wM = bw * f2 md
      in Pictures $
           [ sayR nm (bx - 14) (yy + rowH * 0.4 + 6) 15 (if take 5 nm == "noise" then grayC 150 else grayC 235)
           , Color (clsC 1) (rectTL bx yy (max 1 wM) (rowH * 0.35))
           , say (fmtNum 3 md) (bx + wM + 6) (yy + rowH * 0.17 + 6) 14 (grayC 200) ]
           ++ case p of
                Just pv -> let wP = bw * 0.5 * f2 (clampv (pv / maxP) (-0.1) 1)
                           in [ Color (clsC 2) (rectTL bx (yy + rowH * 0.42) (max 1 wP) (rowH * 0.35))
                              , say (fmtNum 3 pv) (bx + max 1 wP + 6) (yy + rowH * 0.6 + 6) 14 (grayC 200) ]
                Nothing -> []

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> restart w { wCopy = not (wCopy w) }
    | c == 'n' || c == 'N' -> restart w { wNoise = if wNoise w >= 4 then 0 else wNoise w + 1 }
    | c == 'r' || c == 'R' -> restart w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("MDI は全部 0 以上で合計 1", all (>= 0) m && near (foldl' (+) 0 m) 1 1e-9)
  , ("乱数の列の並べ替え重要度 ≈ 0（" ++ fmtNum 3 (p 2) ++ "、" ++ fmtNum 3 (p 3) ++ "、|・| < 0.02）", abs (p 2) < 0.02 && abs (p 3) < 0.02)
  , ("並べ替えでいちばん大事なのは x1、次が x2", p 0 > p 1 && p 1 > max (p 2) (p 3))
  , ("深い木の MDI は乱数の列にも 0 でない点をつける（" ++ fmtNum 3 (mAt m 2) ++ " > 0.02）← MDI のくせ", mAt m 2 > 0.02)
  , ("使っていない列（切り株が x" ++ show (nFeat st + 1) ++ " で切るとき x2）をシャッフルしても、正解率はちょうど同じ", nFeat st == 0 && p2 == 0)
  , ("x1 のコピーを足すと、x1 の MDI が減る（" ++ fmtNum 3 (mAt m 0) ++ " → " ++ fmtNum 3 (mAt mc 0) ++ "）、コピーにも分かれる", mAt mc 0 < mAt m 0 && mAt mc 2 > 0.1) ]
  where
    mAt xs k = headOr 0 (drop k xs)
    p k = headOr 0 (drop k ps)
    ((m, ps, st, p2, mc), _) = evalR (rngOf 1) $ do
      tr <- makeImp 250 False 2 41
      te <- makeImp 300 False 2 42
      seedRng 3
      fo <- grow 30 (makeForest (dsX tr) (dsY tr) defOpt { oMtry = 2, oNClass = 2 })
      let m' = impurityImportance (fTrees fo) 4
      seedRng 4
      ps' <- mapM (\f -> permutationImportance (forestPredict fo) (dsX te) (dsY te) f 3) [0 .. 3]
      let st' = growTreeP defOpt { oMaxDepth = 1 } (dsX tr) (dsY tr)
      seedRng 5
      p2' <- permutationImportance (treePredict st') (dsX te) (dsY te) 1 3
      trc <- makeImp 250 True 2 41
      seedRng 3
      fc <- grow 30 (makeForest (dsX trc) (dsY trc) defOpt { oMtry = 2, oNClass = 2 })
      pure (m', ps', st', p2', impurityImportance (fTrees fc) 5)
    grow :: Int -> Forest -> R Forest
    grow k fo = if k <= 0 then pure fo else forestAdd fo >>= \(fo', _) -> grow (k - 1) fo'

-- ---------- やってみよう ----------
-- 1. 青（MDI）と緑（並べ替え）で、乱数の列の長さを比べよう。どちらを信じる？
-- 2. C で x1 のコピーを足すと、x1 の棒はどうなる？ 「x1 は大事じゃない」と読みまちがえないように
-- 3. N で乱数の列を 4 本に。テストの正解率は下がる？ MDI の乱数の列の合計は？
-- 4. 答えのルールの 0.6 を 0 にすると、x2 の重要度はどうなる？（makeImp の中）
-- 5. 1 本の深い木（mtry なし、1 本）だと重要度はどう変わる？ nTrees を 1 にしてみよう

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

-- ---- バギング / ランダムフォレスト：ブートストラップ（n 個を重複ありで引く）で木を 1 本ずつ育てる
-- 袋の外（OOB）の点 = その木の学習に使われなかった点 → その木の票を「テストの票」として集める
bootstrapIdx :: Int -> R (U.Vector Int, U.Vector Bool)
bootstrapIdx n = do
  js <- mapM (\_ -> do u <- rnd; pure (floor (u * fromIntegral n))) [1 .. n]
  let idx = U.fromList js
  pure (idx, U.accumulate (\_ b -> b) (U.replicate n False) (U.map (\j -> (j, True)) idx))

data Forest = Forest
  { fX :: !(V.Vector Pt), fY :: !(U.Vector Int), fOpt :: !TreeOpt, fK :: !Int
  , fTrees :: [Node]                -- 育てた順
  , fInb   :: [U.Vector Bool]       -- 木ごとの「袋の中」の印
  , fOob   :: !(U.Vector Int) }     -- 点 i のクラス c の OOB の票 = fOob ! (i·K + c)

makeForest :: V.Vector Pt -> U.Vector Int -> TreeOpt -> Forest
makeForest xs ys o =
  let k = if oNClass o > 0 then oNClass o else (if U.null ys then 0 else U.maximum ys) + 1
  in Forest xs ys o { oNClass = k } k [] [] (U.replicate (V.length xs * k) 0)

forestAdd :: Forest -> R (Forest, Node)
forestAdd fo = do
  (idx, inb) <- bootstrapIdx (V.length (fX fo))
  t <- growTree (fOpt fo) { oIdx = Just idx } (fX fo) (fY fo)
  let k = fK fo
      votes = [(i * k + treePredict t (fX fo V.! i), 1) | i <- [0 .. V.length (fX fo) - 1], not (inb U.! i)]
      oob = U.accum (+) (fOob fo) votes
  t `seq` oob `seq` pure (fo { fTrees = fTrees fo ++ [t], fInb = fInb fo ++ [inb], fOob = oob }, t)

forestVote :: Forest -> Pt -> Int -> U.Vector Double   -- 前の m 本の票
forestVote fo x m = U.accum (+) (U.replicate (fK fo) 0) [(treePredict t x, 1) | t <- take m (fTrees fo)]

forestPredict :: Forest -> Pt -> Int
forestPredict fo x = argMaxU (forestVote fo x (length (fTrees fo)))

forestPredictM :: Forest -> Int -> Pt -> Int
forestPredictM fo m x = argMaxU (forestVote fo x m)

oobVotes :: Forest -> Int -> U.Vector Double
oobVotes fo i = U.map fromIntegral (U.slice (i * fK fo) (fK fo) (fOob fo))

oobError :: Forest -> Double
oobError fo =
  let rs = [(argMaxU v /= fY fo U.! i) | i <- [0 .. V.length (fX fo) - 1], let v = oobVotes fo i, sumU v > 0]
  in if null rs then 0 else fromIntegral (length (filter id rs)) / fromIntegral (length rs)

-- ---- 特徴量の重要度
-- 不純度の減少（MDI）：その特徴量で分けたノードの「重みつきの不純度の減り」を全部たして、合計 1 にする
impurityImportance :: [Node] -> Int -> [Double]
impurityImportance trees d =
  let imp = foldl' (\acc t -> let w = let s = sumU (nCounts t) in if s == 0 then 1 else s
                              in foldl' (\a q -> if nLeaf q then a else U.accum (+) a [(nFeat q, nDec q / w)]) acc (treeNodes t))
                   (U.replicate d 0) trees
      s = sumU imp
  in map (\v -> if s > 0 then v / s else 0) (U.toList imp)

-- 並べ替え（パーミュテーション）：テストの点で 1 つの特徴量の列だけをシャッフル → 正解率がどれだけ下がるか（reps 回の平均）
permutationImportance :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Int -> Int -> R Double
permutationImportance fn xs ys f reps = do
  let base = accuracyOf fn xs ys
  drops <- mapM (\_ -> do
             col <- shuffleU (U.fromList (map (U.! f) (V.toList xs)))
             let xp = V.imap (\i x -> x U.// [(f, col U.! i)]) xs
             pure (base - accuracyOf fn xp ys)) [1 .. reps]
  pure (foldl' (+) 0 drops / fromIntegral reps)

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
