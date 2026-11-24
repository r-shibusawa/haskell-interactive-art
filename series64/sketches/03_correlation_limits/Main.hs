-- ============================================
-- 64-03 相関があると平均は効きにくい ─ n 個の予測を平均すると、ばらつき（分散）はどこまで下がる？
-- 1 つの予測の誤差 = σ·(√ρ·z₀ + √(1 − ρ)·zᵢ)：z₀ はみんな共通の誤差、zᵢ はそれぞれの誤差 → どの 2 つも相関 ρ
--   平均の分散 = ρσ² + (1 − ρ)σ²/n。n をいくら増やしても ρσ²（点線）より下がらない
--   左の的：中心が正解、小さな点が n 個の予測、金の星が平均。ρ が大きいと点がそろって同じ方向にずれ、平均もずれる
--   右：線 = 式、白い点 = シミュレーション。だから「ちがう（相関の小さい）」モデルを集めることが大事
-- 操作：スライダー（ドラッグ）相関 ρ・誤差の大きさ σ・的の上の予測の数 n / スペース シミュレーションをやり直す
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

ns :: [Int]
ns = [1, 2, 3, 5, 8, 12, 20, 30, 50]   -- ★ シミュレーションする n

simBudget :: Double
simBudget = 5                  -- ★ 1 フレームにシミュレーションするミリ秒

data World = World
  { wSl    :: ![Slider]              -- ρ・σ・n
  , wDrag  :: !(Maybe Int)
  , wSimSq :: !(U.Vector Double)     -- 平均² の合計（正解は 0 なので、平均² の平均 = 分散）
  , wCnt   :: !(U.Vector Int)
  , wDart  :: !(Maybe ([Double], [Double]))   -- 的の上の n 個の予測（x, y）
  , wRng   :: !Rng                   -- 乱数の列（自己テストの続きから）
  , wTest  :: [(String, Bool)] }

sliders0 :: [Slider]
sliders0 =
  [ Slider 60 0 260 0 1 0.3 "correlation rho" 2 False             -- ★
  , Slider 60 0 260 0.2 2 1 "error of one prediction sigma" 2 False
  , Slider 60 0 260 1 50 10 "predictions on the target n" 0 True ]

main :: IO ()
main = runSketch (sketch "64-03 Correlation limits averaging" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World sliders0 Nothing (U.replicate (length ns) 0) (U.replicate (length ns) 0) Nothing testRng []

slv :: World -> Int -> Double
slv w k = slV (headOr (Slider 0 0 0 0 1 1 "" 0 False) (drop k (wSl w)))

-- ★ 相関 ρ の n 個の誤差（平均 0、分散 σ²）
corrErrors :: Int -> Double -> Double -> R [Double]
corrErrors n rho sg = do
  z0 <- rndGauss
  mapM (const (do z <- rndGauss; pure (sg * (sqrt rho * z0 + sqrt (1 - rho) * z)))) [1 .. n]

avgVarFormula :: Double -> Double -> Double -> Double   -- ★ ρσ² + (1 − ρ)σ²/n
avgVarFormula n rho sg = rho * sg * sg + ((1 - rho) * sg * sg) / n

resetSim :: World -> World
resetSim w = w { wSimSq = U.map (const 0) (wSimSq w), wCnt = U.map (const 0) (wCnt w), wDart = Nothing }

-- 全部の n で 1 回ずつ平均を作る
simOnce :: Double -> Double -> World -> World
simOnce rho sg w =
  let (ms, g) = evalR (wRng w) (mapM (\k -> meanOf <$> corrErrors k rho sg) ns)
  in w { wRng = g, wSimSq = U.zipWith (\s m -> s + m * m) (wSimSq w) (U.fromList ms), wCnt = U.map (+ 1) (wCnt w) }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w <- sliced simBudget (const True) (simOnce (slv w0 0) (slv w0 1)) w0
  pure $ case wDart w of
    Just _ | envFrame env `mod` 40 /= 0 -> w
    _ -> let n = round (slv w 2)
             (d, g) = evalR (wRng w) (do x <- corrErrors n (slv w 0) (slv w 1); y <- corrErrors n (slv w 0) (slv w 1); pure (x, y))
         in w { wDart = Just d, wRng = g }

-- 星（中心からの三角形 10 枚。へこんだ多角形も正しく塗れる）
star :: Float -> Float -> Float -> Picture
star x y r = Pictures [ Polygon [(x, y), v k, v (k + 1)] | k <- [0 .. 9 :: Int] ]
  where v k = let a = -pi / 2 + fromIntegral k * pi / 5
                  rr = if odd k then r * 0.45 else r
              in (x + rr * cos a, y + rr * sin a)

draw :: Env -> World -> Picture
draw env w = view env
  [ -- ---- 左：的
    Color (rgbC 110 80 85) (Pictures [ ringAt cx cy (2 * fromIntegral k * sc) 1 | k <- [3, 2, 1 :: Int] ])
  , Color (colOf ink) (Pictures [lineW 1 (cx - 8, cy) (cx + 8, cy), lineW 1 (cx, cy - 8) (cx, cy + 8)])
  , case wDart w of
      Just (xs, ys) | length xs == n ->
        Pictures [ Color (colA 200 teal) (Pictures [ circleAt (cx + f2 (clampv x (-3.3) 3.3) * sc) (cy - f2 (clampv y (-3.3) 3.3) * sc) 7 | (x, y) <- zip xs ys ])
                 , Color (colOf gold) (star (cx + f2 (clampv (meanOf xs) (-3.3) 3.3) * sc) (cy - f2 (clampv (meanOf ys) (-3.3) 3.3) * sc) 12) ]
      _ -> Blank
  , Pictures [ drawSlider (wDrag w == Just k) s | (k, s) <- zip [0 ..] (placed env w) ]
  -- ---- 右：平均の分散と n
  , plotAxes g "predictions averaged n" "Var(mean): line = formula, dots = simulation, dashed = floor rho sigma^2" [0, 10, 20, 30, 40, 50] [0, sg * sg / 2, sg * sg]
  , Pictures [ Pictures [ Color (rgbC 120 95 100) (polyline 1 [ gPt g (fromIntegral k) (avgVarFormula (fromIntegral k) rr sg) | k <- [1 .. 50 :: Int] ])
                        , sayLM ("rho=" ++ fmtNum 2 rr) (grX g + grW g + 4) (gY g (avgVarFormula 50 rr sg)) 14 (rgbC 150 130 130) ]
             | rr <- [0, 0.1, 0.3, 0.6, 1] ]
  , Color (colOf gold) (polyline 3 [ gPt g (fromIntegral k) (avgVarFormula (fromIntegral k) rho sg) | k <- [1 .. 50 :: Int] ])
  , Color (colOf wine) (dashed 1.5 6 5 [(grX g, gY g (rho * sg * sg)), (grX g + grW g, gY g (rho * sg * sg))])
  , Color (colOf ink) (Pictures [ circleAt (gX g (fromIntegral k)) (gY g (s / fromIntegral c)) 10
                                | (k, s, c) <- zip3 ns (U.toList (wSimSq w)) (U.toList (wCnt w)), c > 0 ])
  -- ---- 表
  , say ("n = " ++ show n ++ ": formula " ++ fmtNum 4 (avgVarFormula (fromIntegral n) rho sg) ++ "   simulation (n = 50: " ++ show c50 ++ " runs) "
         ++ fmtNum 4 (if c50 > 0 then s50 / fromIntegral c50 else 0 / 0) ++ " (formula " ++ fmtNum 4 (avgVarFormula 50 rho sg) ++ ")") (grX g) ty 14 (rgbC 220 210 195)
  , say ("one prediction: sigma^2 = " ++ fmtNum 3 (sg * sg) ++ "  ->  even with infinitely many: rho sigma^2 = " ++ fmtNum 3 (rho * sg * sg)
         ++ " (at most " ++ pctStr (1 - rho) ++ " less)") (grX g) (ty + 24) 14 (rgbC 220 210 195)
  -- ---- 文字
  , say "Correlation limits averaging   Var(mean) = rho sigma^2 + (1 - rho) sigma^2 / n" 40 40 20 (colOf ink)
  , say "Drag sliders rho, sigma, n   Space restart the simulation" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    rho = slv w 0
    sg = slv w 1
    n = round (slv w 2) :: Int
    r = min 170 (envH env * 0.24)        -- 半径 r = 誤差 3
    cx = 60 + 170
    cy = 130 + r
    sc = r / 3
    gx = 480
    g = Graph gx 120 (envW env - gx - 100) (envH env - 280) 0 50 0 (sg * sg * 1.05)
    ty = grY g + grH g + 68
    c50 = lastOr 0 (U.toList (wCnt w))
    s50 = lastOr 0 (U.toList (wSimSq w))

-- スライダーの位置（的の下から 56 ずつ）
placed :: Env -> World -> [Slider]
placed env w = let r = min 170 (envH env * 0.24)
               in [ s { slY = 130 + r + r + 60 + 56 * fromIntegral k } | (k, s) <- zip [0 :: Int ..] (wSl w) ]

setSl :: Env -> Int -> Float -> World -> World
setSl env k mx w = resetSim w { wSl = [ if j == k then sliderSet s mx else s | (j, s) <- zip [0 ..] (placed env w) ] }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pt ->
    let (mx, my) = toP5 env pt
    in case sliderHit (placed env w) (mx, my) of
         Just k -> setSl env k mx w { wDrag = Just k }
         Nothing -> w
  EventMotion pt | Just k <- wDrag w -> setSl env k (fst (toP5 env pt)) w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> resetSim w
  _ -> w

-- ---- 自己テスト
-- (5 通りの (ρ, n) の最大の相対のずれ, 相関の推定, その後の乱数の列)
simTests :: (Double, Double, Rng)
simTests =
  let tt = 20000 :: Int
      rel (rr, k) = do
        s <- foldM (\acc _ -> do m <- meanOf <$> corrErrors k rr 1.5; pure (acc + m * m)) 0 [1 .. tt]
        pure (abs (s / fromIntegral tt / avgVarFormula (fromIntegral k) rr 1.5 - 1))
      (rels, _) = evalR (rngOf 3) (mapM rel [(0, 1), (0, 10), (0.3, 10), (0.6, 50), (0.9, 5)])
      ((sxy, sxx), g) = evalR (rngOf 8) (foldM (\(!a, !b) _ -> do
                                                  e <- corrErrors 2 0.5 1
                                                  let e0 = headOr 0 e
                                                      e1 = headOr 0 (drop 1 e)
                                                  pure (a + e0 * e1, b + e0 * e0)) (0, 0) [1 .. tt])
  in (foldl' max 0 rels, sxy / sxx, g)

testRng :: Rng
testRng = let (_, _, g) = simTests in g

selfTest :: [(String, Bool)]
selfTest =
  [ ("シミュレーション（2 万回）= 式 ρσ² + (1 − ρ)σ²/n、5 通りの (ρ, n) で 5% 以内（最大のずれ " ++ pctStr worst ++ "）", worst <= 0.05)
  , ("式 = 共分散行列の和 ÷ n²（n = 7, ρ = 0.35, σ = 1.5、1e-12）", near (tot / 49) (avgVarFormula 7 0.35 1.5) 1e-12)
  , ("n = 1 ならどの ρ でも σ²、ρ = 1 ならどの n でも σ²（平均しても変わらない）", near (avgVarFormula 1 0.7 2) 4 1e-12 && near (avgVarFormula 40 1 2) 4 1e-12)
  , ("n → ∞ で ρσ² に近づく（n = 10⁶、ρ = 0.3）", near (avgVarFormula 1e6 0.3 1) 0.3 1e-6)
  , ("作った誤差どうしの相関 ≈ ρ = 0.5（" ++ fmtNum 3 corr ++ "）", abs (corr - 0.5) < 0.02) ]
  where
    (worst, corr, _) = simTests
    -- 共分散行列から直接：Var(平均) = (1/n²) Σᵢ Σⱼ Cov(i, j)
    tot = foldl' (+) 0 [ (if i == j then 1 else 0.35) * 2.25 | i <- [0 .. 6 :: Int], j <- [0 .. 6 :: Int] ]

-- ---------- やってみよう ----------
-- 1. ρ = 0 にして n を増やそう。平均の分散は σ²/n でどこまでも下がる（00 の集合知の、いちばん良い場合）
-- 2. ρ = 0.3 だと n = 50 でも ρσ² の少し上で止まる。「たくさん集める」より「ρ を下げる」方が効く
-- 3. 的の上で ρ = 0.9 にすると、点がみんな同じ方向にずれる。これが「同じまちがいをするモデル」
-- 4. ρ = 0.5 のとき、n = 1 → 2 → 10 で分散はそれぞれ何 % 減る？ 最初の数個がいちばん効く
-- 5. 04 からは、本当に「ちがう種類のモデル」を集めて組み合わせる。10・11 で相関（多様性）を測る

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

-- ---- 画面の中のスライダー：Slider x y w lo hi v name d（表示の桁）int（整数か）。マウスでドラッグ
data Slider = Slider { slX :: !Float, slY :: !Float, slW :: !Float, slLo :: !Double, slHi :: !Double, slV :: !Double
                     , slName :: !String, slD :: !Int, slInt :: !Bool }

drawSlider :: Bool -> Slider -> Picture
drawSlider active s =
  let t = f2 ((slV s - slLo s) / (slHi s - slLo s))
  in Pictures [ Color (rgbC 110 80 85) (lineW 4 (slX s, slY s) (slX s + slW s, slY s))
              , Color (colOf gold) (lineW 4 (slX s, slY s) (slX s + t * slW s, slY s))
              , Color (if active then rgbC 255 233 214 else colOf ink) (circleAt (slX s + t * slW s) (slY s) 16)
              , say (slName s ++ " = " ++ fmtNum (slD s) (slV s)) (slX s) (slY s - 12) 14 (rgbC 215 205 190) ]

sliderHit :: [Slider] -> (Float, Float) -> Maybe Int
sliderHit sl (mx, my) = headOr Nothing [ Just k | (k, s) <- zip [0 ..] sl, mx > slX s - 10, mx < slX s + slW s + 10, abs (my - slY s) < 14 ]

sliderSet :: Slider -> Float -> Slider
sliderSet s mx = let v = slLo s + clampv (realToFrac ((mx - slX s) / slW s)) 0 1 * (slHi s - slLo s)
                 in s { slV = if slInt s then fromIntegral (jsRoundI v) else v }

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
