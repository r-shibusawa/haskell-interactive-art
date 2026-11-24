-- ============================================
-- 64-10 ランダム化と多様性 ─ 木の作り方にどれだけ「でたらめ」を入れるかで、木どうしの似かたと強さが変わる
-- バギング（点をブートストラップ）、ランダム部分空間（木ごとに特徴量を 2 つだけ）、Extra Trees（しきい値まで乱数で決める）
--   左 = κ-誤差図：木の組 1 つが点 1 つ。横 = κ（偶然を差し引いた一致度。1 = 同じ答え、0 = 偶然と同じくらい）、縦 = 2 本の平均の誤差
--   左下ほど「ちがうのに、それぞれ正確」。右 = Extra Trees の試す候補の数 K を減らす（でたらめを強くする）と…
--   1 本ずつは弱くなる（赤）のに、30 本の多数決（金）はほとんど落ちない／むしろ上がる範囲がある。多様性（青緑）が上がるから
--   特徴量 = 2 つの役に立つもの（三日月）+ 関係ない乱数。ランダムフォレスト（分割ごとに特徴量を選ぶ）は series62/09
-- 操作：スペース 一時停止 / ← → 関係ない特徴量の数 / R やり直す
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

nTrain, nTest, nTrees :: Int
nTrain = 300
nTest = 1000
nTrees = 30                   -- ★ 木の本数

sweepK :: [Int]
sweepK = [64, 16, 4, 2, 1]    -- ★ Extra Trees の候補の数 K

-- 方法の名前と色
methods :: [(String, RGB)]
methods = [ ("bagging", (236, 214, 160)), ("random subspace (2)", (110, 165, 240))
          , ("Extra Trees (K = 2)", (64, 186, 176)), ("fully random (K = 1)", (196, 72, 96)) ]

-- 木の組の記録：κ・2 本の平均の誤差・不一致率
data Pair = Pair { pK :: !Double, pE :: !Double, pDis :: !Double }

-- 方法ごとの木の集まり（gPreds, gPairs は新しい順）
data Group = Group { gPreds :: ![(U.Vector Int, Double)], gPairs :: ![Pair], gVotes :: !(U.Vector Int) }

data World = World
  { wNoise  :: !Int                 -- ★ 関係ない特徴量の数
  , wSeed   :: !Int
  , wPaused :: !Bool
  , wTrain  :: !DataSet
  , wTestD  :: !DataSet
  , wGroups :: ![Group]
  , wSweep  :: ![Group]
  , wRng    :: !Rng                 -- 木を育てる乱数の列（restart で種をまき、あとは続けて引く）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-10 Randomization and diversity" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart 2 3

newGroup :: Group
newGroup = Group [] [] (U.replicate nTest 0)

-- 三日月のデータ + 関係ない乱数の特徴量
makeHD :: Int -> Int -> Int -> DataSet
makeHD nNoise n seed =
  let d = makeData2D 4 n 0.1 seed
      extra = withSeed (seed + 7) (mapM (const (mapM (const rnd) [1 .. nNoise])) [1 .. n])
  in d { dsX = V.fromList (zipWith (\x e -> U.fromList (U.toList x ++ e)) (V.toList (dsX d)) extra) }

restart :: Int -> Int -> World
restart nNoise seed =
  World nNoise seed False (makeHD nNoise nTrain seed) (makeHD nNoise nTest (seed + 1))
        (map (const newGroup) methods) (map (const newGroup) sweepK) (rngOf (seed * 31)) []

-- ★ 方法ごとの木の作り方（でたらめの入れ方）
makeTree :: DataSet -> Int -> Int -> R RTree
makeTree tr m k
  | m == 0 = do b <- bootIdx nTrain; growRT xs ys b 0 rtOpt                       -- バギング
  | m == 1 = do fs <- shuffleU (U.enumFromN 0 d)                                   -- ランダム部分空間
                growRT xs ys allIdx 0 rtOpt { roFeats = Just (take (min 2 d) (U.toList fs)) }
  | otherwise = growRT xs ys allIdx 0 rtOpt { roRandK = if k > 0 then k else if m == 2 then 2 else 1 }   -- Extra Trees
  where xs = dsX tr
        ys = dsY tr
        d = U.length (xs V.! 0)
        allIdx = U.enumFromN 0 (V.length xs)

-- ★ κ（カッパ）：θ1 = 一致した割合、θ2 = 偶然に一致する割合 = Σ_c p_a(c)·p_b(c)。κ = (θ1 − θ2)/(1 − θ2)
kappaOf :: U.Vector Int -> U.Vector Int -> Double
kappaOf a b =
  let n = fromIntegral (U.length a)
      agree = fromIntegral (U.length (U.filter id (U.zipWith (==) a b)))
      a1 = fromIntegral (U.sum a)
      b1 = fromIntegral (U.sum b)
      t1 = agree / n
      t2 = (a1 / n) * (b1 / n) + (1 - a1 / n) * (1 - b1 / n)
  in if t2 >= 1 - 1e-12 then 1 else (t1 - t2) / (1 - t2)

-- 木を 1 本足し、前の木との組の κ と平均の誤差を記録
addTree :: DataSet -> DataSet -> Int -> Int -> Group -> R Group
addTree tr te m k gr = do
  t <- makeTree tr m k
  let p = U.fromList [ if rtProb t x > 0.5 then 1 else 0 | x <- V.toList (dsX te) ]
      e = fromIntegral (U.length (U.filter id (U.zipWith (/=) p (dsY te)))) / fromIntegral (U.length p)
      new = [ Pair (kappaOf q p) ((qe + e) / 2) (fromIntegral (U.length (U.filter id (U.zipWith (/=) q p))) / fromIntegral nTest)
            | (q, qe) <- reverse (gPreds gr) ]
  pure gr { gPreds = (p, e) : gPreds gr, gPairs = reverse new ++ gPairs gr, gVotes = U.zipWith (+) (gVotes gr) p }

data GStats = GStats { gsInd :: !Double, gsEns :: !Double, gsDis :: !Double }

groupStats :: DataSet -> Group -> Maybe GStats
groupStats te gr
  | null (gPreds gr) = Nothing
  | otherwise =
      let t = length (gPreds gr)
          ens = U.length (U.filter id (U.zipWith (\v y -> (if 2 * v > t then 1 else 0) == y) (gVotes gr) (dsY te)))
      in Just (GStats (1 - meanOf (map snd (reverse (gPreds gr)))) (fromIntegral ens / fromIntegral nTest)
                      (if null (gPairs gr) then 0 else meanOf (map pDis (reverse (gPairs gr)))))

-- 次に育てる木（方法の 4 つを先に、次に K を変える 5 つ）
more :: World -> Bool
more w = not (wPaused w) && any (\g -> length (gPreds g) < nTrees) (wGroups w ++ wSweep w)

growOne :: World -> World
growOne w =
  let full g = length (gPreds g) >= nTrees
      gi = headOr (-1) [ i | (i, g) <- zip [0 ..] (wGroups w), not (full g) ]
      si = headOr (-1) [ i | (i, g) <- zip [0 ..] (wSweep w), not (full g) ]
      upd i f gs = [ if j == i then f g else g | (j, g) <- zip [0 ..] gs ]
      tr = wTrain w
      te = wTestD w
  in if gi >= 0
       then let (g', r) = evalR (wRng w) (addTree tr te gi 0 (wGroups w !! gi)) in w { wGroups = upd gi (const g') (wGroups w), wRng = r }
       else if si >= 0
         then let (g', r) = evalR (wRng w) (addTree tr te 2 (sweepK !! si) (wSweep w !! si)) in w { wSweep = upd si (const g') (wSweep w), wRng = r }
         else w

-- 1 フレームに 7 ミリ秒ぶん木を育てる
step :: Env -> Float -> World -> IO World
step _ _ w = sliced 7 more growOne w

draw :: Env -> World -> Picture
draw env w = view env
  [ plotAxes g "kappa (right = more alike)" ("kappa-error diagram: dot = pair of trees (" ++ show nTrees ++ " trees -> " ++ show (nTrees * (nTrees - 1) `div` 2) ++ " pairs)") [-0.2, 0, 0.5, 1] [0.1, 0.2, 0.3, 0.4]
  , Pictures [ Color (colA 110 c) (Pictures [ circleAt (gX g (pK q)) (gY g (pE q)) 4 | q <- gPairs gr ]) | (gr, (_, c)) <- zip (wGroups w) methods ]
  , say "vertical = mean error of the 2 trees" (grX g + 6) (grY g + grH g - 8) 14 (rgbC 220 210 195)
  , legend (grX g + grW g - 240) (grY g + 16) [ (c, nm) | (nm, c) <- methods ]
  -- ---- 右上：方法ごとの 1 本の正解率と多数決の正解率
  , say ("Test accuracy: light = 1 tree, dark = vote of " ++ show nTrees) rx ty 15 (colOf ink)
  , Pictures [ Pictures [ Color (colA 110 c) (rectTL (rx + 170) yy (max 1 (f2 (gsInd s - 0.5) * 2 * bw)) 16)
                        , Color (colOf c) (rectTL (rx + 170) (yy + 18) (max 1 (f2 (gsEns s - 0.5) * 2 * bw)) 16)
                        , sayRM nm (rx + 160) (yy + 17) 14 (rgbC 210 195 180)
                        , sayLM (pctStr (gsInd s) ++ " -> " ++ pctStr (gsEns s) ++ " (" ++ fmtNum 3 (gsDis s) ++ ")") (rx + 176 + f2 (gsEns s - 0.5) * 2 * bw) (yy + 26) 14 (rgbC 210 195 180) ]
             | (m, gr, (nm, c)) <- zip3 [0 :: Int ..] (wGroups w) methods, Just s <- [groupStats (wTestD w) gr]
             , let yy = ty + 20 + 44 * fromIntegral m ]
  -- ---- 右下：Extra Trees の K を変える
  , plotAxes g2 "number of candidates K (right = more random)" "Extra Trees: red = 1 tree, gold = vote, teal = disagreement" [] [0, 0.5, 1]
  , Pictures [ sayC ("K=" ++ show k) (gX g2 (fromIntegral i)) (grY g2 + grH g2 + 18) 14 (rgbC 175 160 150) | (i, k) <- zip [0 :: Int ..] sweepK ]
  , Pictures [ Color (colOf c) (Pictures (polyline 2.5 pts : [ circleAt px py 7 | (px, py) <- pts ]))
             | (sel, c) <- [(gsInd, wine), (gsEns, gold), (gsDis, teal)]
             , let pts = [ gPt g2 (fromIntegral k) (sel s) | (k, gr) <- zip [0 :: Int ..] (wSweep w), Just s <- [groupStats (wTestD w) gr] ] ]
  -- ---- 文字
  , say ("Randomization and diversity   features: " ++ show (2 + wNoise w) ++ " (2 useful)" ++ (if wPaused w then "   (paused)" else "")) 40 40 20 (colOf ink)
  , say "Space pause   <- -> irrelevant features   R restart" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    side = min (envH env - 190) (envW env * 0.45)
    g = Graph 70 110 side side (-0.2) 1 0.1 0.45
    rx = grX g + grW g + 80
    rw = envW env - rx - 50
    bw = rw - 320
    ty = 120
    g2 = Graph rx (ty + 44 * 4 + 70) (rw - 30) (max 80 (envH env - (ty + 44 * 4 + 70) - 90)) 0 (fromIntegral (length sweepK - 1)) 0 1

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> keep (restart (min 8 (wNoise w + 1)) (wSeed w))
  EventKey (SpecialKey KeyLeft) Down _ _ -> keep (restart (max 0 (wNoise w - 1)) (wSeed w))
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> keep (restart (wNoise w) (wSeed w + 1))
  _ -> w
  where keep v = v { wTest = wTest w }

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("κ の式：手計算の例で 0.25、同じ答えどうしなら 1、半々の答えの正反対なら −1"
    , near (kappaOf a b) 0.25 1e-12 && near (kappaOf a a) 1 1e-12 && near (kappaOf c (U.map (1 -) c)) (-1) 1e-12)
  , ("どの組の κ も −1〜1、不一致率は 0〜1（Extra Trees 2 通り × 105 組）"
    , all (\q -> pK q >= -1 - 1e-12 && pK q <= 1 + 1e-12 && pDis q >= 0 && pDis q <= 1) (gPairs g64 ++ gPairs g1))
  , ("でたらめを強くする（K = 64 → 1）と 1 本の正解率は下がる（" ++ pctStr (gsInd s64) ++ " → " ++ pctStr (gsInd s1) ++ "）", gsInd s1 < gsInd s64)
  , ("でたらめを強くすると不一致率（多様性）は上がる（" ++ fmtNum 3 (gsDis s64) ++ " → " ++ fmtNum 3 (gsDis s1) ++ "）", gsDis s1 > gsDis s64)
  , ("多数決で伸びる分も大きくなる（K = 64：+" ++ pctStr (gsEns s64 - gsInd s64) ++ "、K = 1：+" ++ pctStr (gsEns s1 - gsInd s1) ++ "）"
    , gsEns s1 - gsInd s1 > gsEns s64 - gsInd s64 && gsEns s1 > gsInd s1)
  , ("バギングの 15 本の多数決 " ++ pctStr (gsEns sb) ++ " > 1 本の平均 " ++ pctStr (gsInd sb), gsEns sb > gsInd sb) ]
  where
    -- 手計算：一致 5/8、θ2 = (5/8)(4/8) + (3/8)(4/8) = 1/2 → κ = 0.25
    a = U.fromList [1, 1, 0, 0, 1, 0, 1, 1]
    b = U.fromList [1, 0, 0, 0, 1, 1, 1, 0]
    c = U.fromList [1, 0, 1, 0, 0, 1]
    w0 = restart 2 3
    tr = wTrain w0
    te = wTestD w0
    loop :: Int -> (Group, Group, Group) -> R (Group, Group, Group)
    loop 0 gs = pure gs
    loop k (x, y, z) = do
      x' <- addTree tr te 2 64 x
      y' <- addTree tr te 2 1 y
      z' <- addTree tr te 0 0 z
      loop (k - 1) (x', y', z')
    (g64, g1, gb) = fst (evalR (wRng w0) (loop 15 (newGroup, newGroup, newGroup)))
    st gr = fromMaybe (GStats 0 0 0) (groupStats te gr)
    s64 = st g64
    s1 = st g1
    sb = st gb

-- ---------- やってみよう ----------
-- 1. κ-誤差図で、赤（完全にランダム）の雲はどこにある？ バギング（象牙色）と比べて右左・上下は？
-- 2. 右下のグラフで、多数決（金）がいちばん高い K はいくつ？ 1 本の正解率（赤）がいちばん高い K と同じ？
-- 3. ← → で関係ない特徴量を増やすと、ランダム部分空間（青）はどうなる？ 役に立つ特徴量を引かない木が増える
-- 4. 不一致率が 0 の組は κ = 1。どんな作り方だとそうなる？（でたらめを全部やめると、同じ木しかできない）
-- 5. 11 では、木以外のモデルの組で「多様性」をいろいろな物差しで測る

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

-- ---- ランダム化した分類木（2 クラス、ジニ不純度）。maxDepth、feats（使える特徴量の番号）、mtry（分割ごとに見る数、0 = 全部）、randK（> 0 なら Extra Trees）
data RTree = RLeaf !Double | RNode !Int !Double RTree RTree

data RTOpt = RTOpt { roMaxDepth :: !Int, roFeats :: !(Maybe [Int]), roMtry :: !Int, roRandK :: !Int }

rtOpt :: RTOpt
rtOpt = RTOpt 30 Nothing 0 0

giniCount :: Double -> Double -> Double      -- ジニ不純度 × 点の数
giniCount n1 n = if n <= 0 then 0 else let p = n1 / n in 2 * p * (1 - p) * n

growRT :: V.Vector Pt -> U.Vector Int -> U.Vector Int -> Int -> RTOpt -> R RTree
growRT xs ys idx depth o
  | n1 == 0 || n1 == n || n < 2 || depth >= roMaxDepth o = pure leaf
  | otherwise = do
      best <- if roRandK o > 0
        then let tryOne b _ = do
                   u <- rnd
                   let f = headOr 0 (drop (floor (u * fromIntegral (length feats))) feats)
                       vals = U.map (\i -> xAt xs i f) idx
                       lo = U.foldl' min posInf vals
                       hi = U.foldl' max negInf vals
                   if hi > lo then do u2 <- rnd; pure (tryCut b f (lo + u2 * (hi - lo))) else pure b
             in foldM tryOne Nothing [1 .. roRandK o]
        else do
          fs <- if roMtry o > 0 && roMtry o < length feats then (take (roMtry o) . U.toList) <$> shuffleU (U.fromList feats) else pure feats
          pure (foldl' scanF Nothing fs)
      case best of
        Nothing -> pure leaf
        Just (f, thr, _) -> do
          let (l, r) = U.partition (\i -> xAt xs i f <= thr) idx
          lt <- growRT xs ys l (depth + 1) o
          rt <- growRT xs ys r (depth + 1) o
          pure (RNode f thr lt rt)
  where
    n = U.length idx
    n1 = U.foldl' (\s i -> s + ys U.! i) 0 idx
    nD = fromIntegral n :: Double
    n1D = fromIntegral n1 :: Double
    leaf = RLeaf (if n > 0 then n1D / nD else 0.5)
    feats = fromMaybe [0 .. U.length (xs V.! 0) - 1] (roFeats o)
    base = giniCount n1D nD
    better b f thr dec = case b of
      Just (_, _, d0) | not (dec > d0) -> b
      _ -> Just (f, thr, dec)
    tryCut b f thr =
      let (l, l1) = U.foldl' (\(!a, !c) i -> if xAt xs i f <= thr then (a + 1, c + ys U.! i) else (a, c)) (0 :: Int, 0 :: Int) idx
      in if l == 0 || l == n then b
         else better b f thr (base - giniCount (fromIntegral l1) (fromIntegral l) - giniCount (n1D - fromIntegral l1) (nD - fromIntegral l))
    scanF b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) (U.toList idx))
          step (!b, !l1) q =
            let l1' = l1 + fromIntegral (ys U.! (ord U.! q))
                v0 = xAt xs (ord U.! q) f
                v1 = xAt xs (ord U.! (q + 1)) f
                b' = if not (v1 > v0) then b
                     else better b f ((v0 + v1) / 2) (base - giniCount l1' (fromIntegral (q + 1)) - giniCount (n1D - l1') (fromIntegral (n - q - 1)))
            in (b', l1')
      in fst (foldl' step (b0, 0) [0 .. n - 2])

rtProb :: RTree -> Pt -> Double
rtProb t x = case t of
  RLeaf p -> p
  RNode f thr l r -> rtProb (if x U.! f <= thr then l else r) x

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
