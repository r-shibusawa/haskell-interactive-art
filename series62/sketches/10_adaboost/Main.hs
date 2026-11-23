-- ============================================
-- 62-10 AdaBoost ─ 弱い「切り株」（深さ 1 の木 = 線 1 本）を 1 本ずつ足し、まちがえた点をどんどん重くする
-- 1 ラウンド：重みつきの誤り率 ε がいちばん小さい切り株を選ぶ → 木の重み α = ln((1 − ε)/ε) → まちがえた点の重みを e^α 倍して合計 1 に
--   全体の答え = Σ α_t·(切り株 t の答え ±1) の符号。点の大きさ = いまの重み、黄の線 = いま選んだ切り株
--   訓練誤差は Π 2√(ε_t(1 − ε_t)) 以下（下のグラフの点線）。ε < 0.5 のかぎり、この上限は毎回小さくなる
-- 操作：スペース 1 本足す / A 自動 / 1〜6 データの形 / R 最初から
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
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

nTrain, nTest, maxRounds, cells :: Int
nTrain = 150               -- ★ 点の数
nTest = 500
maxRounds = 150
cells = 80                 -- 領域の絵のマスの数（80 × 80）

flipP :: Double
flipP = 0.03               -- ★ ラベルのノイズ

-- 1 ラウンドの記録
data Hist = Hist { hTr :: !Double, hTe :: !Double, hBound :: !Double, hEps :: !Double, hAlpha :: !Double }

data World = World
  { wKind  :: !Int                  -- ★ データの形（0〜5）
  , wSeed  :: !Int
  , wTrain :: !DataSet
  , wTestD :: !DataSet
  , wBoost :: !Boost
  , wAuto  :: !Bool
  , wHist  :: [Hist]                -- 新しい順
  , wGrid  :: !(U.Vector Double)    -- マスごとの Σ α·(±1)
  , wSTr   :: !(U.Vector Double)    -- 訓練の点の Σ α·(±1)（足していくだけ。まとめて計算したのと同じ値）
  , wSTe   :: !(U.Vector Double)    -- テストの点の Σ α·(±1)
  , wImg   :: !Picture
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-10 AdaBoost" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (wImg world0)              -- 最初のフレームが重くならないように先に計算
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

emptyD :: DataSet
emptyD = DataSet V.empty U.empty

world0 :: World
world0 = restart (World 2 8 emptyD emptyD (makeBoost V.empty U.empty) True [] U.empty U.empty U.empty Blank [])

restart :: World -> World
restart w =
  let tr = fst (evalR (rngOf 1) (makeData2D (wKind w) nTrain flipP (wSeed w)))
      te = fst (evalR (rngOf 1) (makeData2D (wKind w) nTest flipP (wSeed w + 300)))
  in repaint w { wTrain = tr, wTestD = te, wBoost = makeBoost (dsX tr) (dsY tr), wHist = []
               , wGrid = U.replicate (cells * cells) 0, wSTr = U.replicate nTrain 0, wSTe = U.replicate nTest 0 }

-- ★ 1 ラウンド進める（切り株を 1 本足す）
oneRound :: World -> World
oneRound w
  | bDone (wBoost w) || length (bRounds (wBoost w)) >= maxRounds = w
  | otherwise = case boostAdd (wBoost w) of
      (b, Nothing) -> w { wBoost = b }
      (b, Just r) ->
        let vote x = rAlpha r * (if treePredict (rStump r) x == 1 then 1 else -1)
            grid = U.imap (\k s -> let (j, i) = k `quotRem` cells
                                   in s + vote (pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells))) (wGrid w)
            sTr = U.imap (\i s -> s + vote (dsX (wTrain w) V.! i)) (wSTr w)
            sTe = U.imap (\i s -> s + vote (dsX (wTestD w) V.! i)) (wSTe w)
            err sc ys = let n = U.length ys
                            ok = U.length (U.filter id (U.zipWith (\s y -> (if s > 0 then 1 else 0) == y) sc ys))
                        in if n > 0 then 1 - fromIntegral ok / fromIntegral n else 1
            h = Hist (err sTr (dsY (wTrain w))) (err sTe (dsY (wTestD w))) (rBound r) (rEps r) (rAlpha r)
        in repaint w { wBoost = b, wGrid = grid, wSTr = sTr, wSTe = sTe, wHist = h : wHist w }

repaint :: World -> World
repaint w =
  let rs = bRounds (wBoost w)
      tot0 = foldl' (+) 0 (map rAlpha rs)
      tot = if tot0 == 0 then 1 else tot0
      cellOf u v = wGrid w U.! (min (cells - 1) (floor (u * fromIntegral cells)) + cells * min (cells - 1) (floor ((1 - v) * fromIntegral cells)))
  in w { wImg = paintImage cells cells (\u v -> if null rs then bgCol else
                  let s = cellOf u v / tot
                      b = if s > 0 then 1 else 0
                  in mixCol (cnt [1 - b, b]) (0.15 + 0.6 * min 1 (abs s * 2))) }

step :: Env -> Float -> World -> IO World
step env _ w = pure (if wAuto w && envFrame env `mod` 10 == 0 then oneRound w else w)

draw :: Env -> World -> Picture
draw env w = view env
  [ imageBox 40 100 side side cells cells (wImg w)
  , drawPoints pb (dsX tr) (dsY tr) 9 (Just ws)
  , stumpLine
  , Color (rgbC 90 120 100) (frameTL 1 40 100 side side)
  -- ---- 誤差のグラフ
  , plotAxes g "round" "error (orange = train, green = test, white dashed = bound prod 2 sqrt(e(1-e)))" [0, fromIntegral (jsRoundI (fromIntegral rr / 2)), fromIntegral rr] [0, 0.2, 0.4, 0.6]
  , Color (rgbC 245 160 80) (polyline 2.5 [gPt g (fromIntegral k) (hTr h) | (k, h) <- hs])
  , Color (rgbC 175 225 95) (polyline 2 [gPt g (fromIntegral k) (hTe h) | (k, h) <- hs])
  , Color (rgbaC 255 255 255 170) (dashed 1.5 [gPt g (fromIntegral k) (hBound h) | (k, h) <- hs])
  -- ---- 最近のラウンドの表
  , Pictures [ say s (gx + cx) ty 14 (grayC 220) | (s, cx) <- zip ["round", "eps (w. err)", "alpha", "train err", "bound"] cols ]
  , Pictures [ Pictures [ say s (gx + cx) yy 14 c | (s, cx) <- zip [show k, fmtNum 4 (hEps h), fmtNum 3 (hAlpha h), fmtNum 3 (hTr h), fmtNum 3 (hBound h)] cols ]
             | (q, (k, h)) <- zip [0 :: Int ..] recent
             , let yy = ty + 22 + fromIntegral q * 20
                   c = if q == length recent - 1 then rgbC 255 240 150 else grayC 200 ]
  -- ---- 文字
  , say ("AdaBoost   data: " ++ shapeName (wKind w) ++ "   round " ++ show (length (bRounds b))
         ++ (if bDone b then "  (stopped: eps = 0 or >= 0.5)" else "")) 40 40 20 (grayC 240)
  , say ("Space add one   A auto" ++ (if wAuto w then " (ON)" else " (OFF)") ++ "   1-6 data shape   R restart") 40 66 15 (grayC 170)
  , say "alpha = ln((1-e)/e)" (gx + 220) (ty + 22 + 8 * 20 + 6) 14 (grayC 150)
  , sayWrap "Dot size = weight. Points that keep being wrong grow, and the next stump goes after them. Colour strength = size of sum alpha * answer (confidence)."
      40 (100 + side + 12) side 14 (grayC 160)
  , drawSelfTest env (wTest w) ]
  where
    side = min (envH env - 200) (envW env * 0.45)
    pb = Box 40 100 side side
    tr = wTrain w
    b = wBoost w
    n = fromIntegral (V.length (dsX tr)) :: Double
    ws = U.map (\v -> clampv (sqrt (v * n)) 0.35 4) (bW b)
    stumpLine = case reverse (bRounds b) of
      (r : _) | not (nLeaf (rStump r)) ->
        let s = rStump r
            col = rgbC 255 240 150
        in if nFeat s == 0 then let x = fst (toPx pb (nThr s) 0) in Color col (lineW 3 (x, 100) (x, 100 + side))
           else let yy = snd (toPx pb 0 (nThr s)) in Color col (lineW 3 (40, yy) (40 + side, yy))
      _ -> Blank
    gx = 40 + side + 70
    nh = length (wHist w)
    rr = max 20 nh
    g = Graph gx 130 (envW env - gx - 50) (envH env * 0.36) 0 (fromIntegral rr) 0 0.6
    hs = zip [1 :: Int ..] (reverse (wHist w))
    recent = drop (nh - min 8 nh) hs
    ty = grY g + grH g + 60
    cols = [0, 80, 220, 320, 430]

-- 点線（4 ピクセル描いて 4 ピクセルあける）
dashed :: Float -> [(Float, Float)] -> Picture
dashed wd pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
              k = max 1 (ceiling (l / 8)) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + 0.5) / fromIntegral k)) | i <- [0 .. k - 1] ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> oneRound w { wAuto = False }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c >= '1' && c <= '6' -> restart w { wKind = fromEnum c - fromEnum '1' }
    | c == 'r' || c == 'R' -> restart w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("毎ラウンド：訓練誤差 ≤ Π 2√(ε(1 − ε))（40 ラウンド）", all okBound rounds)
  , ("選んだ切り株の ε = 全部のしきい値を総当たりした重みつき誤りの最小", all okBest rounds)
  , ("どのラウンドでも 0 < ε < 0.5、α = ln((1 − ε)/ε) > 0", all okEps rounds)
  , ("重みの合計はいつも 1", all (\(_, b, _) -> abs (sumU (bW b) - 1) <= 1e-9) rounds)
  , ("重みを更新すると、いま選んだ切り株の重みつき誤りはちょうど 0.5 になる", all okHalf rounds)
  , ("上限は " ++ show (length (bRounds bEnd)) ++ " ラウンドで " ++ fmtNum 3 (bBound bEnd) ++ " まで下がった（< 1）", bBound bEnd < 1 && noNaN (U.toList (bW bEnd))) ]
  where
    dd = fst (evalR (rngOf 1) (makeData2D 2 120 0.03 31))
    xs = V.toList (dsX dd)
    -- (前の重み, 足した後, そのラウンド)。止まったらそこまで
    go :: Int -> Boost -> [(U.Vector Double, Boost, Round)]
    go k b | k >= 40 = []
           | otherwise = case boostAdd b of
               (b', Just r) -> (bW b, b', r) : go (k + 1) b'
               _ -> []
    rounds = go 0 (makeBoost (dsX dd) (dsY dd))
    bEnd = case reverse rounds of
      ((_, b, _) : _) -> b
      [] -> makeBoost (dsX dd) (dsY dd)
    okBound (_, b, _) = 1 - accuracyOf (boostPredict b (length (bRounds b))) (dsX dd) (dsY dd) <= bBound b + 1e-12
    okBest (wOld, _, r) =                           -- 総当たりの切り株：左右を多数決にしたときの重みつき誤りの最小
      let best = minOr posInf [ minimum' wl + minimum' wr
                              | f <- [0, 1]
                              , let vals = sort (nub (map (U.! f) xs))
                              , (a, c) <- zip vals (drop 1 vals)
                              , let th = (a + c) / 2
                                    side p = U.toList (U.accumulate (+) (U.replicate 2 0) (U.fromList [(dsY dd U.! i, wOld U.! i) | (i, x) <- zip [0 ..] xs, p (x U.! f <= th)]))
                                    wl = side id
                                    wr = side not ]
          minimum' = minOr 0
      in abs (rEps r - best) <= 1e-9
    okEps (_, _, r) = rEps r > 0 && rEps r < 0.5 && abs (rAlpha r - log ((1 - rEps r) / rEps r)) <= 1e-12
    okHalf (_, b, r) = let e2 = foldl' (+) 0 [bW b U.! i | (i, x) <- zip [0 ..] xs, treePredict (rStump r) x /= dsY dd U.! i]
                       in abs (e2 - 0.5) <= 1e-9

-- ---------- やってみよう ----------
-- 1. スペースで 1 本ずつ足し、点の大きさの変化を見よう。大きくなるのはどんな点？
-- 2. 円（3）のデータを、線 1 本ずつの切り株で囲むには何ラウンドかかる？
-- 3. 訓練誤差（オレンジ）が 0 になったあとも、テスト誤差（緑）は下がり続けることがある。確かめよう
-- 4. flipP を 0.15 にすると、ノイズの点の重みはどうなる？ AdaBoost はノイズに弱い？
-- 5. 11 の勾配ブースティングは「重み」の代わりに「残差」に次の木をあてはめる

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

-- ---- AdaBoost（2 クラスの SAMME）：切り株（深さ 1 の木）を、まちがえた点ほど重くして 1 本ずつ足す
-- ε = 重みつきの誤り率、α = ln((1 − ε)/ε)、まちがえた点の重み × e^α → 合計 1 に。訓練誤差 ≤ Π 2√(ε(1 − ε))
data Round = Round { rStump :: Node, rEps :: !Double, rAlpha :: !Double, rBound :: !Double }

data Boost = Boost { bX :: !(V.Vector Pt), bY :: !(U.Vector Int), bW :: !(U.Vector Double)
                   , bRounds :: [Round], bBound :: !Double, bDone :: !Bool }

makeBoost :: V.Vector Pt -> U.Vector Int -> Boost
makeBoost xs ys = let n = V.length xs in Boost xs ys (U.replicate n (1 / fromIntegral n)) [] 1 False

boostAdd :: Boost -> (Boost, Maybe Round)
boostAdd b
  | bDone b = (b, Nothing)
  | eps <= 1e-10 || eps >= 0.5 - 1e-10 = (b { bDone = True }, Nothing)   -- 完ぺき、または当てずっぽう以下なら止める
  | otherwise = (b { bW = w2, bBound = bound, bRounds = bRounds b ++ [r] }, Just r)
  where
    st = growTreeP defOpt { oMaxDepth = 1, oW = Just (bW b), oCrit = MisErr, oNClass = 2 } (bX b) (bY b)
    n = V.length (bX b)
    wrong = U.generate n (\i -> treePredict st (bX b V.! i) /= bY b U.! i)
    eps = U.foldl' (+) 0 (U.zipWith (\bad v -> if bad then v else 0) wrong (bW b))
    alpha = log ((1 - eps) / eps)
    w1 = U.zipWith (\bad v -> if bad then v * exp alpha else v) wrong (bW b)
    s = sumU w1
    w2 = U.map (/ s) w1
    bound = bBound b * (2 * sqrt (eps * (1 - eps)))
    r = Round st eps alpha bound

boostScore :: Boost -> Int -> Pt -> Double     -- 前の m ラウンドの重みつきの票
boostScore b m x = foldl' (\s r -> s + rAlpha r * (if treePredict (rStump r) x == 1 then 1 else -1)) 0 (take m (bRounds b))

boostPredict :: Boost -> Int -> Pt -> Int
boostPredict b m x = if boostScore b m x > 0 then 1 else 0

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
