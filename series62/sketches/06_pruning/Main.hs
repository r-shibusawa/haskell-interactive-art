-- ============================================
-- 62-06 剪定と交差検証 ─ いったん最後まで育てた木の「弱い枝」から順に刈って、ちょうどよい大きさを選ぶ
-- コスト複雑度：R_α(T) = 訓練の誤り R(T) + α·(葉の数)。α を 0 から増やすと、g(t) = (R(t) − R(T_t)) / (葉の数 − 1) が
--   いちばん小さい枝（= 葉を 1 枚へらしても誤りがほとんど増えない枝）から刈られ、α の列と木の列ができる
--   どの α がよいかは「5 分割交差検証」：データを 5 つに分け、4 つで木を育てて剪定し、残り 1 つで誤りを数える × 5 回
-- 操作：← → 剪定の列を動かす / B 交差検証でいちばんよい α へ / 1〜6 データの形 / ↑↓ ノイズ / R 新しいデータ
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

nTrain, nTest, kFold :: Int
nTrain = 240               -- ★ 訓練の点の数
nTest = 600
kFold = 5                  -- ★ 何分割の交差検証か

imgN :: Int
imgN = 140

data World = World
  { wKind  :: !Int
  , wFlip  :: !Double       -- ★ ノイズの割合
  , wSeed  :: !Int
  , wTrain :: !DataSet
  , wTree  :: !Node         -- 剪定の印（nCut）がついた木
  , wSeq   :: [PStep]       -- α の列
  , wCv    :: [Double]      -- 交差検証の誤り（α ごと）
  , wTe    :: [Double]      -- テストの誤り（α ごと）
  , wSel   :: !Int
  , wBest  :: !Int
  , wImg   :: !Picture
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-06 Pruning and cross-validation" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      sum (wCv w) `seq` sum (wTe w) `seq` wImg w `seq` pure w
  , skDraw  = draw
  , skEvent = event
  }

emptyD :: DataSet
emptyD = DataSet V.empty U.empty

world0 :: World
world0 = newData (World 2 0.15 5 emptyD nilNode [] [] [] 0 0 Blank [])

gen :: Int -> Int -> Double -> Int -> DataSet
gen kind n flp seed = fst (evalR (rngOf 1) (makeData2D kind n flp seed))

-- 交差検証の誤りがいちばん小さい番号（同点なら後ろ = 小さい木）
bestIdx :: [Double] -> Int
bestIdx cv = let m = minOr 0 cv in foldl' (\b (k, e) -> if e <= m + 1e-12 then k else b) 0 (zip [0 ..] cv)

newData :: World -> World
newData w =
  let tr = gen (wKind w) nTrain (wFlip w) (wSeed w)
      te = gen (wKind w) nTest (wFlip w) (wSeed w + 777)
      (t, sq) = pruneSequence (growTreeP defOpt (dsX tr) (dsY tr))   -- ★ α の列（弱い枝から刈る）
      cv = fst (evalR (rngOf 1) (cvPrune (dsX tr) (dsY tr) defOpt sq kFold (wSeed w)))   -- ★ 交差検証の誤り（α ごと）
      teE = [1 - accuracyOf (treePredictL noD (psAlpha s) t) (dsX te) (dsY te) | s <- sq]
  in repaint w { wTrain = tr, wTree = t, wSeq = sq, wCv = cv, wTe = teE, wBest = bestIdx cv, wSel = 0 }

alphaOf :: World -> Double
alphaOf w = psAlpha (stepAt w (wSel w))

stepAt :: World -> Int -> PStep
stepAt w k = headOr (PStep 0 1 0) (drop k (wSeq w))

at' :: [Double] -> Int -> Double
at' xs k = headOr 0 (drop k xs)

repaint :: World -> World
repaint w = w { wImg = paintImage imgN imgN (\u v -> let t = leafOfL noD (alphaOf w) (wTree w) (pt2 u v)
                                                    in mixCol (nCounts t) (0.3 + 0.35 * (1 - 2 * nImp t))) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ imageBox 40 100 side side imgN imgN (wImg w)
  , drawPoints pb (dsX (wTrain w)) (dsY (wTrain w)) 7 Nothing
  , Color (rgbC 90 120 100) (frameTL 1 40 100 side side)
  -- ---- 木の図（刈った後）
  , fst (drawTree (wTree w) tb treeDraw { tdALim = a, tdText = nodeText })
  -- ---- 誤りのグラフ（横 = 葉の数）
  , plotAxes g "leaves" ("error (orange = train, green = " ++ show kFold ++ "-fold CV, dotted = test)")
      [1, fromIntegral (jsRoundI (fromIntegral l0 / 2)), fromIntegral l0] [0, 0.1, 0.2, 0.3, 0.4, 0.5]
  , Color (rgbC 245 160 80) (polyline 2.5 [gPt g (lv s) (psErr s) | s <- sq])
  , Color (rgbC 175 225 95) (polyline 2.5 [gPt g (lv s) e | (s, e) <- zip sq (wCv w)])
  , Color (rgbaC 255 255 255 150) (dashed [gPt g (lv s) e | (s, e) <- zip sq (wTe w)])
  , Color (rgbC 175 225 95) (Pictures [uncurry circleAt (gPt g (lv s) e) 5 | (s, e) <- zip sq (wCv w)])
  , Color (rgbC 255 240 150) (uncurry circleAt (gPt g (lv sel) (at' (wCv w) (wSel w))) 13)
  , Color (rgbaC 175 225 95 160) (lineW 1 (gxOf g (lv best), grY g) (gxOf g (lv best), grY g + grH g))
  -- ---- 文字
  , say ("Pruning and CV   step " ++ show (wSel w + 1) ++ " / " ++ show (length sq) ++ ": alpha = " ++ fmtNum 4 a ++ "   "
         ++ show (psLeaves sel) ++ " leaves" ++ (if wSel w == wBest w then "   <- best by CV" else "")) 40 40 20 (grayC 240)
  , say ("<- -> pruning sequence   B best alpha   1-6 data shape (" ++ shapeName (wKind w) ++ ")   Up/Down noise "
         ++ show (jsRoundI (wFlip w * 100)) ++ "%   R new data") 40 66 15 (grayC 170)
  , say ("train error " ++ fmtNum 1 (psErr sel * 100) ++ "%   CV " ++ fmtNum 1 (at' (wCv w) (wSel w) * 100) ++ "%   test "
         ++ fmtNum 1 (at' (wTe w) (wSel w) * 100) ++ "%") 40 ty 15 (grayC 225)
  , sayParas [ "best (CV): " ++ show (psLeaves best) ++ " leaves, test " ++ fmtNum 1 (at' (wTe w) (wBest w) * 100) ++ "%  /  unpruned ("
               ++ show l0 ++ " leaves): test " ++ fmtNum 1 (at' (wTe w) 0 * 100) ++ "%" ] 40 (ty + 8) (side + 60) 15 0 (grayC 225)
  , sayParas [ "alpha sequence (alpha -> leaves): " ++ unwords [fmtNum 3 (psAlpha s) ++ "->" ++ show (psLeaves s) | s <- take 12 sq] ++ (if length sq > 12 then " ..." else "")
             , "g in the tree = the alpha at which that branch is cut. Cross-validation chooses without looking at the test set." ]
             40 (ty + 60) (side + 60) 14 10 (grayC 160)
  , drawSelfTest env (wTest w) ]
  where
    side = min (envH env - 320) (envW env * 0.36)
    pb = Box 40 100 side side
    a = alphaOf w
    sq = wSeq w
    sel = stepAt w (wSel w)
    best = stepAt w (wBest w)
    tb = Box (40 + side + 40) 100 (envW env - side - 120) (envH env * 0.36)
    l0 = psLeaves (stepAt w 0)
    g = Graph (bxX tb + 20) (bxY tb + bxH tb + 60) (bxW tb - 40) (envH env - bxY tb - bxH tb - 200) 1 (fromIntegral (max 2 l0)) 0 0.5
    lv s = fromIntegral (psLeaves s)
    ty = 100 + side + 34

nodeText :: Node -> Bool -> [String]
nodeText t leaf
  | leaf = [countsStr (nCounts t), ""]
  | otherwise = ["x" ++ show (nFeat t + 1) ++ " <= " ++ fmtNum 2 (nThr t), "g = " ++ fmtNum 4 (nCut t)]

-- 点線の折れ線（4 ピクセルおき）
dashed :: [(Float, Float)] -> Picture
dashed ps = Pictures (concat (zipWith seg ps (drop 1 ps)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
              n = max 1 (floor (l / 8)) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
          in [lineW 1.5 (p (fromIntegral k / fromIntegral n)) (p (min 1 ((fromIntegral k + 0.5) / fromIntegral n))) | k <- [0 .. n - 1]]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> repaint w { wSel = min (length (wSeq w) - 1) (wSel w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> repaint w { wSel = max 0 (wSel w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> newData w { wFlip = min 0.35 (round2 (wFlip w + 0.05)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> newData w { wFlip = max 0 (round2 (wFlip w - 0.05)) }
  EventKey (Char c) Down _ _
    | c == 'b' || c == 'B' -> repaint w { wSel = wBest w }
    | c >= '1' && c <= '6' -> newData w { wKind = fromEnum c - fromEnum '1' }
    | c == 'r' || c == 'R' -> newData w { wSeed = wSeed w + 1 }
  _ -> w
  where round2 v = fromIntegral (round (v * 100) :: Int) / 100   -- +(v).toFixed(2)

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("α の列は増えるか同じ、葉の数は減るか同じ、最後は葉 1 枚（根だけ）", all okA runs)
  , ("列のどの木でも、訓練の誤り = その α で刈った木で 1 点ずつ数えた誤り", all okE runs)
  , ("α_k と α_(k+1) の間の α では、T_k が R(T) + α·葉の数 を最小にする（列の中で）", all okOpt runs)
  , ("子の枝は親より先に（小さい α で）刈られるか、同時", all okD runs)
  , ("5 分割の誤り率は 0〜1 で NaN なし（全部の点がちょうど 1 回ずつテストに使われる）", all (\e -> e >= 0 && e <= 1) cv && noNaN cv)
  , ("交差検証で選んだ木（葉 " ++ show (psLeaves (headOr (PStep 0 1 0) (drop kb sS))) ++ "）のテスト正解率 " ++ fmtNum 3 teB
     ++ " ≥ 刈る前（葉 " ++ show (psLeaves (headOr (PStep 0 1 0) sS)) ++ "）の " ++ fmtNum 3 te0, teB >= te0) ]
  where
    runs = [ (tr, t, sq) | s <- [0 .. 3], let tr = gen (s `mod` 6) 200 0.15 (60 + s), let (t, sq) = pruneSequence (growTreeP defOpt (dsX tr) (dsY tr)) ]
    okA (_, _, sq) = and (zipWith (\p q -> not (psAlpha q < psAlpha p || psLeaves q > psLeaves p || psErr q < psErr p - 1e-12)) sq (drop 1 sq))
                     && psLeaves (lastOr (PStep 0 0 0) sq) == 1
    okE (tr, t, sq) = all (\q -> abs (psErr q - (1 - accuracyOf (treePredictL noD (psAlpha q) t) (dsX tr) (dsY tr))) <= 1e-9) sq
    okOpt (_, _, sq) = and [ not (any (\q -> psErr q + al * fromIntegral (psLeaves q) < ck - 1e-12) sq)
                           | (p, q) <- zip sq (drop 1 sq), let al = (psAlpha p + psAlpha q) / 2, let ck = psErr p + al * fromIntegral (psLeaves p) ]
    okD (_, t, _) = and [ not (nCut (nLeft q) /= posInf && nCut (nLeft q) > nCut q + 1e-12) | q <- treeNodes t, not (nLeaf q) ]
    (trL, _, sqL) = lastOr (emptyD, nilNode, []) runs
    cv = fst (evalR (rngOf 1) (cvPrune (dsX trL) (dsY trL) defOpt sqL 5 3))
    dD = gen 2 240 0.15 5
    dE = gen 2 600 0.15 782
    (tT, sS) = pruneSequence (growTreeP defOpt (dsX dD) (dsY dD))
    cC = fst (evalR (rngOf 1) (cvPrune (dsX dD) (dsY dD) defOpt sS 5 5))
    kb = bestIdx cC
    te0 = accuracyOf (treePredict tT) (dsX dE) (dsY dE)
    teB = accuracyOf (treePredictL noD (psAlpha (headOr (PStep 0 1 0) (drop kb sS))) tT) (dsX dE) (dsY dE)

-- ---------- やってみよう ----------
-- 1. → を押していくと、どの枝から刈られる？ 木の図の g の小さい順になっている？
-- 2. 緑（交差検証）とテスト（点線）の曲線は似ている？ B で選んだ木の葉は何枚？
-- 3. ↑ でノイズを増やすと、最良の葉の数は増える？ 減る？
-- 4. kFold を 2 や 10 に変えると、緑の曲線はどう変わる？
-- 5. 05 の「深さで止める」と、ここの「育ててから刈る」はどちらがよい？ 同じ葉の数で比べよう

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

-- ---- コスト複雑度剪定：R_α(T) = R(T) + α·(葉の数)。R = 訓練の誤分類の割合
-- いちばん弱い枝 g(t) = (R(t) − R(T_t)) / (葉の数(T_t) − 1) が小さいノードから順に「葉にする」。nCut = 刈られる α
data PStep = PStep { psAlpha :: !Double, psLeaves :: !Int, psErr :: !Double }

pruneSequence :: Node -> (Node, [PStep])
pruneSequence root0 = go root1 0 [PStep 0 (countLeaves root1) (fst (subtreeStat rOf root1))]
  where
    nAll = let s = sumU (nCounts root0) in if s == 0 then 1 else s
    rOf t = (sumU (nCounts t) - maxU (nCounts t)) / nAll
    root1 = setInf root0
    setInf t = if nLeaf t then t else t { nCut = posInf, nLeft = setInf (nLeft t), nRight = setInf (nRight t) }
    go root alpha acc
      | nLeaf root || nCut root /= posInf = (root, reverse acc)
      | otherwise =
          let cand = subtreeCands rOf root
              gmin = minOr 0 (map snd cand)
              alpha' = max alpha gmin
              ids = S.fromList [nId t | (t, g) <- cand, g <= gmin + 1e-12]   -- 同点はまとめて刈る
              root' = mark ids alpha' root
              st = PStep alpha' (countLeavesL noD alpha' root') (fst (subtreeStat rOf root'))
          in go root' alpha' (st : acc)
    mark ids a t
      | nLeaf t = t
      | otherwise = let l = mark ids a (nLeft t)
                        r = mark ids a (nRight t)
                    in t { nCut = if S.member (nId t) ids then a else nCut t, nLeft = l, nRight = r }

-- いまの（刈った後の）部分木の R と葉の数
subtreeStat :: (Node -> Double) -> Node -> (Double, Int)
subtreeStat rOf t
  | nLeaf t || nCut t /= posInf = (rOf t, 1)
  | otherwise = let (ra, la) = subtreeStat rOf (nLeft t)
                    (rb, lb) = subtreeStat rOf (nRight t)
                in (ra + rb, la + lb)

-- 刈られていない内側のノードと g(t)
subtreeCands :: (Node -> Double) -> Node -> [(Node, Double)]
subtreeCands rOf t
  | nLeaf t || nCut t /= posInf = []
  | otherwise = let (r, l) = subtreeStat rOf t
                in subtreeCands rOf (nLeft t) ++ subtreeCands rOf (nRight t) ++ [(t, max 0 ((rOf t - r) / fromIntegral (l - 1)))]

-- k 分割交差検証：各 fold で木を育てて剪定の列を作り、本体の列の α_m と α_(m+1) の幾何平均で刈って、残りの点で誤差を数える
cvPrune :: V.Vector Pt -> U.Vector Int -> TreeOpt -> [PStep] -> Int -> Int -> R [Double]
cvPrune xs ys o sq kf seed = do
  seedRng (if seed == 0 then 1 else seed)
  let n = V.length xs
  order <- U.toList <$> shuffleU (U.enumFromN 0 n)
  let alphas = map psAlpha sq
      betas = zipWith (\m a -> if m + 1 < length alphas then sqrt (a * (alphas !! (m + 1))) else a) [0 :: Int ..] alphas
  errs <- mapM (\f -> do
            let test = [i | (q, i) <- zip [0 :: Int ..] order, q `mod` kf == f]
                train = U.fromList [i | (q, i) <- zip [0 :: Int ..] order, q `mod` kf /= f]
            tr0 <- growTree o { oIdx = Just train } xs ys
            let tr = fst (pruneSequence tr0)
            pure [fromIntegral (length [() | i <- test, treePredictL noD beta tr (xs V.! i) /= ys U.! i]) | beta <- betas]) [0 .. kf - 1]
  pure (map (/ fromIntegral n) (foldl' (zipWith (+)) (map (const 0) sq) errs))

-- ---- 合成データ（[0, 1]² の 2 次元、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 箱 / 5 しま
shapes :: [String]
shapes = ["blobs", "diagonal", "circle", "XOR", "box", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapes)

sq2 :: Double -> Double
sq2 a = a * a

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq2 (u - 0.32) + sq2 (v - 0.36) < sq2 (u - 0.68) + sq2 (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq2 (u - 0.5) + sq2 (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if u > 0.25 && u < 0.7 && v > 0.3 && v < 0.8 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2D :: Int -> Int -> Double -> Int -> R DataSet
makeData2D kind n flp seed = do
  seedRng seed
  ps <- mapM (\_ -> do
          u0 <- rnd
          v0 <- rnd
          let u = 0.02 + 0.96 * u0
              v = 0.02 + 0.96 * v0
              c = trueLabel kind u v
          r <- rnd
          pure (U.fromList [u, v], if r < flp then 1 - c else c)) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))

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

-- ---- 木の図：葉を左から順に並べ、親は子の真ん中。tdText t leaf → 箱の 2 行、tdCol → 色、tdHl = 光らせる id
data TreeDraw = TreeDraw { tdDLim :: !Int, tdALim :: !Double, tdText :: Node -> Bool -> [String]
                         , tdCol :: Node -> RGB, tdHl :: S.Set Int }

treeDraw :: TreeDraw
treeDraw = TreeDraw noD negInf (\_ _ -> []) (clsCol . nPred) S.empty

data TreeInfo = TreeInfo { tiLeaves :: !Int, tiMaxD :: !Int, tiPos :: IM.IntMap (Float, Float), tiBig :: !Bool }

drawTree :: Node -> Box -> TreeDraw -> (Picture, TreeInfo)
drawTree root bx o = (Pictures (edges ++ boxes), TreeInfo leafN maxD posMap big)
  where
    cut = isCut (tdDLim o) (tdALim o)
    vis = treeNodesL (tdDLim o) (tdALim o) root
    maxD = maxOr 0 (map nDepth vis)
    place (!k, m) t
      | cut t = ((k + 1, IM.insert (nId t) (fromIntegral k) m), fromIntegral k)
      | otherwise = let (s1, a) = place (k, m) (nLeft t)
                        ((k2, m2), b) = place s1 (nRight t)
                        p = (a + b) / 2
                    in ((k2, IM.insert (nId t) p m2), p)
    ((leafN, slot), _) = place (0 :: Int, IM.empty) root
    cw = bxW bx / fromIntegral (max 1 leafN)
    rh = min 92 (bxH bx / fromIntegral (maxD - nDepth root + 1))
    bw = min 150 (cw - 4)
    big = bw >= 96 && rh >= 50
    pOf t = (bxX bx + (IM.findWithDefault 0 (nId t) slot + 0.5) * cw, bxY bx + fromIntegral (nDepth t - nDepth root) * rh + 18)
    posMap = IM.fromList [(nId t, pOf t) | t <- vis]
    off = if big then 18 else 4
    edges = [ Color (rgbC 120 140 125) (lineW 1.5 (ax, ay + off) (bx', by - off))
            | t <- vis, not (cut t), c <- [nLeft t, nRight t], let (ax, ay) = pOf t, let (bx', by) = pOf c ]
    boxes = map one vis
    one t =
      let (x, yy) = pOf t
          leaf = cut t
          col@(cr, cg, cb) = tdCol o t
          hl = S.member (nId t) (tdHl o)
      in if big
           then Pictures
                  [ Color (if leaf then colOf (cr * 0.35 + 14, cg * 0.35 + 20, cb * 0.35 + 17) else rgbC 30 44 38) (rectTL (x - bw / 2) (yy - 18) bw 36)
                  , Color (if hl then rgbC 255 240 150 else if leaf then colOf col else rgbC 110 130 115) (frameTL (if hl then 3 else 1.5) (x - bw / 2) (yy - 18) bw 36)
                  , Pictures [sayM s x (yy - 8 + fromIntegral q * 16) 14 (grayC 235) | (q, s) <- zip [0 :: Int ..] (take 2 (tdText o t leaf))] ]
           else let d = max 5 (min 14 (cw * 0.6))
                in Pictures [ Color (if hl then rgbC 255 240 150 else grayC 15) (circleAt x yy (d + if hl then 6 else 2))
                            , Color (colOf col) (circleAt x yy d) ]

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
