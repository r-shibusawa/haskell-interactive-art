-- ============================================
-- 64-05 重み付き平均 ─ 3 つのモデルの予測を w₁·A + w₂·B + w₃·C で混ぜる。重みは「和 = 1、どれも ≥ 0」
-- 重みを決めるのは「検証データ」（学習に使っていない点）。検証の 2 乗誤差がいちばん小さい重みを、射影勾配法で探す
--   射影勾配法：勾配の向きに 1 歩 → 「和 = 1、≥ 0」の三角形（単体）の上のいちばん近い点に戻す、をくり返す
--   右の三角形 = 重みの全部の組合せ。色 = 検証の誤差（明るいほど小さい）、白い線 = 射影勾配法の道、金の点 = 最適な重み
--   D でモデル B のコピーを入れる（4 つ）→ 2 つのコピーの重みの「和」が、コピーなしのときの B の重みと同じになる
-- 操作：スペース もう一度探す / 三角形をクリック そこから探す / D コピーを入れる / R データを作り直す
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

nTrain, nVal :: Int
nTrain = 40                -- ★ 点の数
nVal = 40

noiseS :: Double
noiseS = 0.25              -- ★ ノイズ

names3 :: [String]
names3 = ["A: line (poly deg 1)", "B: k-NN (k = 2)", "C: polynomial deg 8"]

col3 :: Int -> RGB                            -- ★ A・B・C の色
col3 k = case k of
  0 -> (236, 214, 160)
  1 -> (240, 120, 100)
  _ -> (190, 135, 235)

ftrue :: Double -> Double                     -- ★ 本当の関数
ftrue x = 0.6 * sin (2 * pi * x) * x + 0.8 * x - 0.3

-- 回帰のデータ：x と y の列
data Reg = Reg { rxs :: ![Double], rys :: ![Double] }

-- 検証の 2 乗誤差 L(w) = wᵀAw − 2bᵀw + c の A, b, c
data Quad = Quad { qA :: ![[Double]], qB :: ![Double], qC :: !Double, qK :: !Int }

data World = World
  { wSeed   :: !Int
  , wDup    :: !Bool
  , wTrain  :: !Reg
  , wVal    :: !Reg
  , wTestR  :: !Reg
  , wPredV  :: ![[Double]]            -- P[i][k] = モデル k の点 i での予測（検証）
  , wPredT  :: ![[Double]]            -- 同じくテスト
  , wCurves :: ![U.Vector Double]     -- 3 つのモデルの線（描く x の上の値）
  , wLoss   :: !Quad
  , wOptW   :: ![Double]
  , wW      :: ![Double]              -- いまの重み
  , wPath   :: ![[Double]]            -- 射影勾配法の道（新しい順）
  , wIters  :: !Int
  , wTri    :: !Picture
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-05 Weighted average" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (wOptW world0))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 4 False e e e [] [] [] (Quad [] [] 0 0) [] [] [] 0 Blank [])
  where e = Reg [] []

makeReg :: Int -> Int -> Reg
makeReg n seed = withSeed seed $ do
  ps <- mapM (const (do x <- rnd; g <- rndGauss; pure (x, ftrue x + noiseS * g))) [1 .. n]
  pure (Reg (map fst ps) (map snd ps))

-- k 近傍法の回帰：近い k 点の y の平均
knnReg :: [Double] -> [Double] -> Double -> Int -> Double
knnReg xs ys x k = meanOf (map snd (take k (sortOn fst [ (abs (v - x), y) | (v, y) <- zip xs ys ])))

fitThree :: Reg -> [Double -> Double]
fitThree d = let a = polyFit (rxs d) (rys d) 1 1e-9
                 c = polyFit (rxs d) (rys d) 8 1e-6
             in [polyEval a, \x -> knnReg (rxs d) (rys d) x 2, polyEval c]

preds :: [Double -> Double] -> Reg -> [[Double]]
preds fs d = [ map ($ x) fs | x <- rxs d ]

xsG :: [Double]
xsG = [ fromIntegral i / 120 | i <- [0 .. 120 :: Int] ]

restart :: World -> World
restart w =
  let tr = makeReg nTrain (wSeed w)
      va = makeReg nVal (wSeed w + 50)
      te = makeReg 400 (wSeed w + 99)
      fs = fitThree tr
      pv = preds fs va
  in startPG Nothing (setupLoss w { wTrain = tr, wVal = va, wTestR = te, wPredV = pv, wPredT = preds fs te
                                  , wCurves = [ U.fromList (map f xsG) | f <- fs ] })

-- 何番のモデルを使うか（コピーありなら B が 2 つ）
memberIdx :: World -> [Int]
memberIdx w = if wDup w then [0, 1, 1, 2] else [0, 1, 2]

-- ★ 検証の 2 乗誤差を、先に A・b・c にまとめておく
lossOf :: [[Double]] -> [Double] -> Quad
lossOf ps ys =
  let n = fromIntegral (length ys)
      k = length (headOr [] ps)
      a = [ [ foldl' (\s p -> s + ((p !! i) * (p !! j)) / n) 0 ps | j <- [0 .. k - 1] ] | i <- [0 .. k - 1] ]
      b = [ foldl' (\s (p, y) -> s + ((p !! i) * y) / n) 0 (zip ps ys) | i <- [0 .. k - 1] ]
      c = foldl' (\s y -> s + (y * y) / n) 0 ys
  in Quad a b c k

lossF :: Quad -> [Double] -> Double
lossF q w = max 0 (foldl' rowA (qC q) (zip3 (qA q) (qB q) w))
  where rowA s (arow, ba, wa) = foldl' (\t (aq, wq) -> t + wa * aq * wq) (s - 2 * ba * wa) (zip arow w)

setupLoss :: World -> World
setupLoss w =
  let q = lossOf [ map (p !!) (memberIdx w) | p <- wPredV w ] (rys (wVal w))
      opt = runPG q (replicate (qK q) (1 / fromIntegral (qK q))) 3000
  in w { wLoss = q, wOptW = opt, wTri = if wDup w then wTri w else paintTriangle q }

-- ★ 単体への射影：和 = 1・≥ 0 の中でいちばん近い点（並べ替えて、引く量 θ を決める）
projSimplex :: [Double] -> [Double]
projSimplex v =
  let u = sortBy (flip compare) v
      (_, theta) = foldl' (\(css, th) (j, uj) -> let c = css + uj
                                                     t = (c - 1) / fromIntegral (j + 1 :: Int)
                                                 in (c, if uj - t > 0 then t else th)) (0, 0) (zip [0 ..] u)
  in map (\x -> max 0 (x - theta)) v

-- 1 歩：w ← 射影(w − η∇L)、η = 1 / (2·A の対角の和)（大きすぎない歩幅）
pgStep :: Quad -> [Double] -> [Double]
pgStep q w =
  let tr = foldl' (+) 0 [ row !! a | (a, row) <- zip [0 ..] (qA q) ]
      eta = 1 / (2 * tr + 1e-12)
      g = [ foldl' (\s (aq, wq) -> s + 2 * aq * wq) (-2 * ba) (zip arow w) | (arow, ba) <- zip (qA q) (qB q) ]
  in projSimplex (zipWith (\x ga -> x - eta * ga) w g)

runPG :: Quad -> [Double] -> Int -> [Double]
runPG q w0 iters = foldl' (\w _ -> pgStep q w) w0 [1 .. iters]

startPG :: Maybe [Double] -> World -> World
startPG mw w = let k = qK (wLoss w)
                   w0 = fromMaybe (replicate k (1 / fromIntegral k)) mw
               in w { wW = w0, wPath = [w0], wIters = 0 }

-- 三角形：上 = A、左下 = B、右下 = C。(u, v) ∈ [0,1]² の位置 → 重み（三角形の外なら Nothing）
triP, triQ, triR :: (Double, Double)
triP = (0.5, 0)
triQ = (0, 1)
triR = (1, 1)

baryAt :: Double -> Double -> Maybe [Double]
baryAt u v =
  let (p0, p1) = triP
      (q0, q1) = triQ
      (r0, r1) = triR
      d = (q1 - r1) * (p0 - r0) + (r0 - q0) * (p1 - r1)
      a = ((q1 - r1) * (u - r0) + (r0 - q0) * (v - r1)) / d
      b = ((r1 - p1) * (u - r0) + (p0 - r0) * (v - r1)) / d
      c = 1 - a - b
  in if a < -1e-9 || b < -1e-9 || c < -1e-9 then Nothing else Just [a, b, c]

triW, triH :: Int
triW = 180
triH = 156

-- 三角形の各画素 → 重み（重心座標）→ 検証の誤差の色
paintTriangle :: Quad -> Picture
paintTriangle q =
  let vals = U.generate (triW * triH) (\k -> let (py, px) = k `quotRem` triW
                                             in maybe (-1) (lossF q) (baryAt (fromIntegral px / fromIntegral triW) (fromIntegral py / fromIntegral triH)))
      ok = U.filter (>= 0) vals
      lo = if U.null ok then 0 else U.minimum ok
      hi = if U.null ok then 0 else U.maximum ok
      (br, bg, bb) = bgCol
  in pixelImage triW triH 1 (\px py ->
       let l = vals U.! (px + triW * py)
           t = if l < 0 then 0 else 1 - ((l - lo) / (hi - lo + 1e-12)) ** 0.35
           (r, g, b) = if l < 0 then bgCol else (br + t * 200, bg + t * 160, bb + t * 70)
           w8 x = fromIntegral (rnd8 x) :: Word8
       in (w8 r, w8 g, w8 b))

triBoxOf :: Env -> Box
triBoxOf env = let gw = min 560 (envW env * 0.45)
                   rx = 60 + gw + 80
                   tw = min 360 (envW env - rx - 330)
               in Box rx 120 tw (tw * 0.866)

triPos :: Box -> [Double] -> (Float, Float)
triPos bx w = let ws = take 3 (w ++ repeat 0)
                  ux = sum (zipWith (*) ws [fst triP, fst triQ, fst triR])
                  uy = sum (zipWith (*) ws [snd triP, snd triQ, snd triR])
              in (bxX bx + f2 ux * bxW bx, bxY bx + f2 uy * bxH bx)

-- 混ぜた予測の 2 乗誤差の平均（予測は restart で計算ずみ）
mixErr :: [Int] -> [[Double]] -> Reg -> [Double] -> Double
mixErr idx ps d w = meanOf [ let e = foldl' (\s (k, wa) -> s + wa * (p !! k)) 0 (zip idx w) - y in e * e | (p, y) <- zip ps (rys d) ]

step :: Env -> Float -> World -> IO World
step env _ w
  | wIters w < 400 && envFrame env `mod` 2 == 0 =
      let w' = foldl' (\x _ -> pgStep (wLoss w) x) (wW w) [1 .. 4 :: Int]
      in pure w { wW = w', wIters = wIters w + 4, wPath = w' : wPath w }
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ plotAxes g "x" "o train, filled = validation. lines: 3 models, gold = weighted average" [0, 0.5, 1] [-1, 0, 1]
  , Pictures [ Color (colA 170 (col3 k)) (polyline 1.5 [ gPt g x (c U.! i) | (i, x) <- zip [0 ..] xsG ]) | (k, c) <- zip [0 ..] (wCurves w) ]
  , Color (colOf gold) (polyline 3.5 [ gPt g x (sum0 [ wa * (wCurves w !! k) U.! i | (k, wa) <- zip idx (wW w) ]) | (i, x) <- zip [0 ..] xsG ])
  , Color (rgbC 200 190 175) (Pictures [ let (px, py) = gPt g x y in ringAt px py 7 1 | (x, y) <- zip (rxs (wTrain w)) (rys (wTrain w)) ])
  , Color (colOf wine) (Pictures [ let (px, py) = gPt g x y in circleAt px py 7 | (x, y) <- zip (rxs (wVal w)) (rys (wVal w)) ])
  -- ---- 右上：三角形
  , if not (wDup w)
      then Pictures [ imageBox (bxX tb) (bxY tb) (bxW tb) (bxH tb) triW triH (wTri w)
                    , Color white (polyline 1.5 (map (triPos tb) (reverse (wPath w))))
                    , Color (colOf gold) (uncurry circleAt (triPos tb (wOptW w)) 14)
                    , Color white (uncurry circleAt (triPos tb (wW w)) 7)
                    , sayC "A" (bxX tb + bxW tb / 2) (bxY tb - 6) 14 (colOf (col3 0))
                    , sayR "B" (bxX tb - 4) (bxY tb + bxH tb + 14) 14 (colOf (col3 1))
                    , say "C" (bxX tb + bxW tb + 4) (bxY tb + bxH tb + 14) 14 (colOf (col3 2)) ]
      else sayWrap "Now 4 members (B twice), so there is no triangle. See the weight bars." (bxX tb) (bxY tb) (bxW tb + 100) 15 (rgbC 200 185 170)
  -- ---- 右：重みの棒と誤差の表
  , say ("weights (sum = " ++ fmtNum 6 (sumL (wW w)) ++ ")") bx 130 15 (colOf ink)
  , Pictures [ Pictures [ Color (colOf (col3 c)) (rectTL (bx + 90) yy (max 1 (f2 wa * 160)) 20)
                        , sayRM nm (bx + 82) (yy + 10) 14 (rgbC 220 210 195)
                        , sayLM (fmtNum 3 wa) (min (envW env - 50) (bx + 96 + f2 wa * 160)) (yy + 10) 14 (rgbC 220 210 195) ]
             | (k, (wa, nm, c)) <- zip [0 :: Int ..] (zip3 (wW w) names idx), let yy = 150 + 30 * fromIntegral k ]
  , Pictures [ Pictures [ say a rx yy 14 col, sayR b (rx + 330) yy 14 col, sayR c (rx + 420) yy 14 col ]
             | (q, (a, b, c)) <- zip [0 :: Int ..] rows
             , let yy = ty + 24 * fromIntegral q
                   col = if q == length rows - 1 then colOf gold else rgbC 220 210 195 ]
  , say ("mean squared error (smaller is better). proj. gradient: " ++ show (wIters w) ++ " steps") rx (ty + 24 * fromIntegral (length rows) + 6) 14 (rgbC 170 155 145)
  -- ---- 文字
  , say ("Weighted average: weights chosen on validation data (sum 1, >= 0)" ++ (if wDup w then "  + copy of B" else "")) 40 40 20 (colOf ink)
  , say "Space search again   click triangle: start there   D add a copy of B   R new data" 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    g = Graph 60 110 (min 560 (envW env * 0.45)) (envH env - 330) 0 1 (-1) 1.4
    sum0 = foldl' (+) 0
    idx = memberIdx w
    tb = triBoxOf env
    rx = bxX tb
    bx = bxX tb + bxW tb + 50
    names = if wDup w then ["A", "B", "B copy", "C"] else ["A", "B", "C"]
    ty = max (bxY tb + bxH tb + 50) 330
    n = length idx
    eq = replicate n (1 / fromIntegral n)
    one k = [ if q == k then 1 / fromIntegral (length (filter (== k) idx)) else 0 | q <- idx ]
    errs ws = (fmtNum 4 (mixErr idx (wPredV w) (wVal w) ws), fmtNum 4 (mixErr idx (wPredT w) (wTestR w) ws))
    row nm ws = let (a, b) = errs ws in (nm, a, b)
    rows = [("", "valid.", "test")] ++ [ row nm (one k) | (k, nm) <- zip [0 ..] names3 ]
           ++ [row ("equal weights 1/" ++ show n) eq, row "proj. gradient weights" (wW w)]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p | not (wDup w) ->
    let (mx, my) = toP5 env p
        tb = triBoxOf env
    in case baryAt (realToFrac ((mx - bxX tb) / bxW tb)) (realToFrac ((my - bxY tb) / bxH tb)) of
         Just ws -> startPG (Just ws) w
         Nothing -> w
  EventKey (SpecialKey KeySpace) Down _ _ -> startPG Nothing w
  EventKey (Char c) Down _ _
    | c == 'd' || c == 'D' -> startPG Nothing (setupLoss w { wDup = not (wDup w) })
    | c == 'r' || c == 'R' -> restart w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("単体への射影：和 = 1・≥ 0、2 回やっても同じ、単体のどの点よりも近い（100 個のベクトル）", okP)
  , ("射影勾配法の重み（" ++ intercalate ", " (map (fmtNum 3) w) ++ "）は単体の上の点、検証の誤差 ≤ 0.01 刻みの総当たりの最小"
    , near (sumL w) 1 1e-12 && all (>= 0) w && lossF q w <= best + 1e-12 && best - lossF q w < 1e-3)
  , ("最適な重みの検証の誤差 " ++ fmtNum 4 (lossF q w) ++ " < 等しい重み " ++ fmtNum 4 (lossF q eq3) ++ "、≤ どの 1 つのモデル"
    , lossF q w < lossF q eq3 && all (\k -> lossF q w <= lossF q [ if a == k then 1 else 0 | a <- [0, 1, 2 :: Int] ] + 1e-12) [0, 1, 2])
  , ("B のコピーを入れると、コピー 2 つの重みの和 " ++ fmtNum 3 (wd !! 1 + wd !! 2) ++ " = コピーなしの B の重み " ++ fmtNum 3 (w !! 1) ++ "、誤差も同じ"
    , abs (wd !! 1 + wd !! 2 - w !! 1) < 2e-3 && abs (lossF qd wd - lossF q w) < 1e-6)
  , ("先に計算した式 wᵀAw − 2bᵀw + c = 直接計算した検証の誤差（1e-12）", near l direct 1e-12) ]
  where
    okP = withSeed 12 (and <$> mapM (const oneP) [1 .. 100 :: Int])
    oneP = do
      v <- mapM (const ((3 *) <$> rndGauss)) [1 .. 4 :: Int]
      let p = projSimplex v
          pp = projSimplex p
          dp = sumL (zipWith (\x a -> (x - a) * (x - a)) p v)
          ok1 = not (abs (sumL p - 1) > 1e-12 || any (< 0) p || or (zipWith (\x y -> abs (x - y) > 1e-12) pp p))
      oks <- mapM (const (do e <- mapM (const ((\x -> -(log (x + 1e-12))) <$> rnd)) [1 .. 4 :: Int]
                             let s = sumL e
                                 r = map (/ s) e
                             pure (not (sumL (zipWith (\x a -> (x - a) * (x - a)) r v) < dp - 1e-12)))) [1 .. 200 :: Int]
      pure (ok1 && and oks)
    tr = makeReg 40 4
    va = makeReg 40 54
    fs = fitThree tr
    q = lossOf (preds fs va) (rys va)
    eq3 = [1 / 3, 1 / 3, 1 / 3]
    w = runPG q eq3 3000
    best = minimum [ lossF q [fromIntegral i / 100, fromIntegral j / 100, fromIntegral (100 - i - j) / 100] | i <- [0 .. 100 :: Int], j <- [0 .. 100 - i] ]
    fd = [fs !! 0, fs !! 1, fs !! 1, fs !! 2]
    qd = lossOf (preds fd va) (rys va)
    wd = runPG qd [0.25, 0.25, 0.25, 0.25] 3000
    l = lossF (lossOf (preds fs va) (rys va)) [0.2, 0.5, 0.3]
    direct = meanOf [ let e = foldl' (\s (f, a) -> s + a * f x) 0 (zip fs [0.2, 0.5, 0.3]) - y in e * e | (x, y) <- zip (rxs va) (rys va) ]

-- ---------- やってみよう ----------
-- 1. 三角形のいろいろな所をクリックして、射影勾配法がいつも同じ金の点に着くことを確かめよう（誤差の地形は 1 つの谷）
-- 2. 最適な重みで、検証では良くなった。テストでも良くなっている？ 検証データが少ないと（nVal = 10）？
-- 3. D でコピーを入れると、2 つのコピーで重みを分け合う。分け方は決まらないが、和は決まる
-- 4. 「重み ≥ 0」をやめたらどうなる？ projSimplex を「和 = 1 だけ」にしてみよう（重みが負やとても大きくなることも）
-- 5. 06 のスタッキングでは、重みの代わりに「メタ学習器」というモデルを学習する

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

-- ---- 多項式の最小 2 乗法。x ∈ [0, 1] を [−1, 1] に直し、チェビシェフ多項式 T0, T1, … を特徴量にする（次数が高くても計算が安定）
chebFeat :: Double -> Int -> [Double]
chebFeat x deg = take (deg + 1) (go 1 t)
  where t = 2 * x - 1
        go a b = a : go b (2 * t * b - a)

data Poly = Poly { pDeg :: !Int, pC :: ![Double] }

-- (ΦᵀΦ + λI) c = Φᵀy
polyFit :: [Double] -> [Double] -> Int -> Double -> Poly
polyFit xs ys deg ridge =
  let d = deg + 1
      fs = map (\x -> U.fromList (chebFeat x deg)) xs
      bv = [ foldl' (\s (f, y) -> s + (f U.! p) * y) 0 (zip fs ys) | p <- [0 .. d - 1] ]
      am = [ [ foldl' (\s f -> s + (f U.! p) * (f U.! q)) 0 fs | q <- [0 .. d - 1] ] | p <- [0 .. d - 1] ]
  in Poly deg (solveLin am bv ridge)

polyEval :: Poly -> Double -> Double
polyEval m x = foldl' (+) 0 (zipWith (*) (pC m) (chebFeat x (pDeg m)))

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
