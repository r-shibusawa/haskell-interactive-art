-- ============================================
-- 70-07 状態はいくつ？ ─ カフェの時間帯（朝・昼・夜の 3 つ、見えない）が注文（コーヒー・紅茶・ジュース・ケーキ・サンド）を決めている。
-- 注文の列だけを見て、状態の数 N = 1〜6 の HMM をバウム-ウェルチで当てはめる。
--   訓練データの対数尤度：N を増やすほど上がり続ける（大きいモデルは小さいモデルを中にふくむ）→ これだけでは N を選べない
--   検証データ（学習に使っていない列）の対数尤度：本当の数をこえると上がらなくなり、下がることも（過学習）
--   BIC = −2 log L + k log T、AIC = −2 log L + 2k（k = パラメータの数）：小さいほど良い。増やしすぎに罰金
-- パラメータの数 k = N(N − 1)［A の各行］+ N(M − 1)［B の各行］+ (N − 1)［π］（行の合計が 1 なので 1 つずつ少ない）
-- 操作：スペース 学習をやり直す / ↑↓ データの長さ T / ← → 見るモデルの N / R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (Path, rose)
import Data.Array.Unboxed (UArray, accumArray, listArray, (!))
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import GHC.Clock (getMonotonicTime)
cafeObs :: [String]
cafeObs = ["coffee", "tea", "juice", "cake", "sandw"]   -- コーヒー・紅茶・ジュース・ケーキ・サンド

trueCafe :: Hmm                               -- ★ 本当のモデル（3 状態）
trueCafe = Hmm [1 / 3, 1 / 3, 1 / 3]
               [[0.9, 0.07, 0.03], [0.04, 0.9, 0.06], [0.06, 0.04, 0.9]]
               [[0.7, 0.15, 0.05, 0.05, 0.05], [0.05, 0.05, 0.1, 0.1, 0.7], [0.05, 0.15, 0.5, 0.25, 0.05]]

tChoices :: [Int]
tChoices = [300, 600, 1200, 2400]             -- ★ データの長さの候補

maxStates, restarts, shortIters, emIters :: Int
maxStates = 6                                      -- ★ ためす状態数の上限
restarts = 6                                  -- ★ 出発点の数
shortIters = 12                               -- ★ 短くためす回数
emIters = 50                                  -- ★ 仕上げの EM の回数

-- N ごとの結果
data FitRes = FitRes { frN :: !Int, frM :: Hmm, frK :: !Int, frTrainLL, frValLL, frBic, frAic :: !Double }

-- 学習の途中の状態（1 フレームに少しずつ進める）
data Fit = Fit
  { fTrain :: [Int], fVal :: [Int], fMaxN, fRestarts, fIters :: !Int
  , fN, fR, fIt :: !Int
  , fCur :: Maybe Hmm, fFallback :: Maybe Hmm, fBest :: Maybe (Hmm, Double)
  , fResults :: [FitRes], fDone :: !Bool }

data World = World
  { wTSel  :: !Int
  , wSeed  :: !Int
  , wFit   :: Fit
  , wViewN :: !Int
  , wRng   :: !Rng
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "70-07 How many states?" world0)
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> do
      t0 <- getMonotonicTime
      let loop !k wa = if fDone (wFit wa) || k >= 400 then pure wa else do
            let (f', g) = runWith (wRng wa) (fitStep (wFit wa))
                wb = wa { wFit = f', wRng = g }
            t <- forceFit f' `seq` getMonotonicTime
            if (t - t0) * 1000 < 6 then loop (k + 1 :: Int) wb else pure wb   -- 1 フレームに 6 ミリ秒だけ学習
      loop 0 w
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = startFit (World 1 77 (newFit [] [] 0 0 0) 3 (rngOf 1) [])

startFit :: World -> World
startFit w =
  let len = at' tChoices (wTSel w) 600
      ((tr, va), g) = runWith (rngOf (wSeed w)) $ do
        (_, a) <- hmmSample trueCafe len
        (_, b) <- hmmSample trueCafe len
        pure (a, b)
  in w { wFit = newFit tr va maxStates restarts emIters, wRng = g }

paramCount :: Int -> Int -> Int               -- ★ 自由に決められる数字の数
paramCount n k = n * (n - 1) + n * (k - 1) + (n - 1)

newFit :: [Int] -> [Int] -> Int -> Int -> Int -> Fit
newFit tr va mn rs its = Fit tr va mn rs its 1 0 0 Nothing Nothing Nothing [] False

-- ★ 状態を 1 つ 2 つに分ける：N 状態のモデルとまったく同じ確率を出す N + 1 状態のモデルを作る（ゆらす = 少しだけずらして EM が動けるように）
splitState :: Hmm -> Bool -> R Hmm
splitState m shake = do
  let n = nStates m
      s = argMax (stationaryOf (hTr m))
      p0 = modifyAt s (/ 2) (hP0 m ++ [at' (hP0 m) s 0 / 2])
      rows = [ modifyAt s (/ 2) (r ++ [at' r s 0 / 2]) | r <- hTr m ]
      tr = rows ++ [at' rows s []]
      em = hEm m ++ [at' (hEm m) s []]
  em' <- if not shake then pure em else
           foldlR (\e i -> do
                     us <- replicateR (length (at' e i [])) rnd
                     pure (setAt i (normRow (zipWith (\v u -> v * (0.6 + 0.8 * u)) (at' e i []) us)) e)) em [s, n]
  pure (Hmm p0 tr em')

-- でたらめな出発点（ただし「同じ状態にとどまりやすい」ところから始める）
stickyStart :: Int -> Int -> R Hmm
stickyStart n k = do
  m <- randomHmm n k
  pure m { hTr = [ normRow [ v * 0.3 + (if i == j then 2 else 0) | (j, v) <- zip [0 :: Int ..] r ] | (i, r) <- zip [0 ..] (hTr m) ] }

-- ★ 学習を 1 回分だけ進める（EM 1 回）。出発点ごとに短く（shortIters 回）ためして、いちばん良かったものだけを長く（fIters 回）みがく
fitStep :: Fit -> R Fit
fitStep f0
  | fDone f0 = pure f0
  | otherwise = do
      let k = length (headOr [] (hEm trueCafe))
      f <- case fCur f0 of
        Just _ -> pure f0
        Nothing
          | fR f0 == fRestarts f0 -> pure f0 { fCur = fmap fst (fBest f0), fIt = 0 }
          | fR f0 == 0 && fN f0 > 1 -> do
              let prev = frM (at' (fResults f0) (fN f0 - 2) (FitRes 0 trueCafe 0 0 0 0 0))
              fb <- splitState prev False
              cu <- splitState prev True
              pure f0 { fFallback = Just fb, fCur = Just cu, fIt = 0 }
          | otherwise -> do
              cu <- stickyStart (fN f0) k
              pure f0 { fCur = Just cu, fIt = 0 }
      let cur = maybe trueCafe id (fCur f)
          cur' = fst (baumWelchStep cur [fTrain f] 0)
          it = fIt f + 1
          f1 = f { fCur = Just cur', fIt = it }
      if it < (if fR f == fRestarts f then fIters f else shortIters) then pure f1 else do
        let ll = hmmLogLik cur' (fTrain f)
            best1 = case fBest f of
              Just (_, bl) | not (ll > bl) -> fBest f
              _ -> Just (cur', ll)
            r' = fR f + 1
            f2 = f1 { fCur = Nothing, fR = r', fBest = best1 }
        if r' <= fRestarts f then pure f2 else do
          let (bm0, bl0) = maybe (cur', ll) id best1
              (bm, bl) = case fFallback f of
                Just fbm -> let fl = hmmLogLik fbm (fTrain f) in if fl > bl0 then (fbm, fl) else (bm0, bl0)   -- 小さいモデルより悪くはならない
                Nothing -> (bm0, bl0)
              len = length (fTrain f)
              kk = paramCount (fN f) k
              res = FitRes (fN f) bm kk bl (hmmLogLik bm (fVal f)) (-2 * bl + i2d kk * log (i2d len)) (-2 * bl + 2 * i2d kk)
              n' = fN f + 1
          pure f2 { fResults = fResults f ++ [res], fBest = Nothing, fFallback = Nothing, fR = 0, fN = n', fDone = n' > fMaxN f }

-- 計算をその場で済ませる（あとで描くときにまとめて計算されて止まらないように）
forceFit :: Fit -> Double
forceFit f = sum (map hmmSum (maybe [] pure (fCur f) ++ maybe [] pure (fFallback f) ++ maybe [] (pure . fst) (fBest f)))
             + sum [ hmmSum (frM r) + frTrainLL r + frValLL r + frBic r + frAic r | r <- fResults f ]
  where hmmSum m = sumOf (hP0 m) + sum (map sumOf (hTr m)) + sum (map sumOf (hEm m))

bestBy :: [FitRes] -> (FitRes -> Double) -> Double -> Int
bestBy rs key sign = case rs of
  [] -> 0
  (r0 : rest) -> frN (foldl' (\b r -> if sign * key r > sign * key b then r else b) r0 rest)

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle env ("How many states? Fitting N = 1.." ++ show maxStates ++ " state HMMs to data from 3 states")
      ("Space refit   Up/Down data length T = " ++ show len ++ "   Left/Right view model N = " ++ show (wViewN w) ++ "   R new data   "
       ++ (if fDone f then "done" else "learning... N = " ++ show (fN f) ++ (if fR f < restarts then " (start " ++ show (fR f + 1) ++ " / " ++ show restarts ++ ")" else " (polishing)")))
  , plotAxes g1 "number of states N" "log-lik per observation (higher is better)" [1 .. 6] [to3 (gy0 g1), to3 (gy1 g1)]
  , plotAxes g2 "number of states N" "BIC, AIC (lower is better)" [1 .. 6] [i2d (jsRound (gy0 g2)), i2d (jsRound (gy1 g2))]
  , Pictures [ Pictures [ Color (colA 60 ink) (lineW 1 (gX g 3, gy g) (gX g 3, gy g + gh g)), Color (colA 20 ink) (rectTL (gX g (i2d (wViewN w)) - 14) (gy g) 28 (gh g)) ] | g <- [g1, g2] ]
  , dots g1 (\r -> frTrainLL r / i2d len) lan, dots g1 (\r -> frValLL r / i2d len) aqua
  , dots g2 frBic rose, dots g2 frAic lilac
  , legend (gx g1 + 14) (gy g1 + gh g1 - 46) [(lan, "training data (keeps rising)"), (aqua, "validation data")]
  , legend (gx g2 + 14) (gy g2 + 18) [(rose, "BIC"), (lilac, "AIC")]
  , note "vertical line = the true number (3)" (gx g1) (gy g1 + gh g1 + 56) dimc 14
  -- ---- 表
  , note "N" 90 ty dimc 14, note "params k" 130 ty dimc 14, note "train log L" 290 ty lan 14, note "valid log L" 410 ty aqua 14
  , note "BIC" 530 ty rose 14, note "AIC" 630 ty lilac 14
  , Pictures [ let y = ty + 24 * i2f (frN r); c = if frN r == wViewN w then ink else dimc
               in Pictures [ note (show (frN r)) 90 y c 14
                           , note (show (frK r) ++ " = " ++ show (frN r * (frN r - 1)) ++ " + " ++ show (frN r * 4) ++ " + " ++ show (frN r - 1)) 130 y c 14
                           , note (fmtN (frTrainLL r) 1) 290 y c 14, note (fmtN (frValLL r) 1) 410 y c 14
                           , note (fmtN (frBic r) 1) 530 y c 14, note (fmtN (frAic r) 1) 630 y c 14 ]
             | r <- rs ]
  , if fDone f then note ("Best N: validation -> " ++ show (bestBy rs frValLL 1) ++ "   BIC -> " ++ show (bestBy rs frBic (-1)) ++ "   AIC -> " ++ show (bestBy rs frAic (-1))
                         ++ "   (training data alone would pick " ++ show (bestBy rs frTrainLL 1) ++ ")") 90 (ty + 24 * i2f maxStates + 34) leaf 14
    else Blank
  -- ---- 選んだ N のモデル
  , note ("Emission matrix B learned with N = " ++ show (wViewN w)) mx 122 ink 14
  , note "(rows = states, columns = orders)" mx 142 dimc 14
  , case [ r | r <- rs, frN r == wViewN w ] of
      (sel : _) ->
        let cw = min 60 ((envW env - mx - 30) / 5); vn = i2f (wViewN w)
        in Pictures [ drawMatrix (hEm (frM sel)) mx 176 cw 30 [] cafeObs matOpt { moCol = lan }
                    , note "Transition matrix A" mx (176 + vn * 30 + 34) ink 14
                    , drawMatrix (hTr (frM sel)) mx (176 + vn * 30 + 46) 44 26 [] [] matOpt { moCol = rose }
                    , note (if wViewN w > 3 then "Similar rows appear (one time of day split in two)"
                            else if wViewN w < 3 then "Different times of day get merged"
                            else "Same shape as the true model (order may differ)") mx (176 + vn * 56 + 76) dimc 14 ]
      [] -> note "learning..." mx 180 dimc 14
  , note "True B: morning = coffee, noon = sandw," mx (envH env - 78) dimc 14
  , note "evening = juice, cake, tea" mx (envH env - 60) dimc 14
  , drawSelfTest env (wTest w) ]
  where
    f = wFit w
    rs = fResults f
    len = length (fTrain f)
    gwd = min 400 ((envW env - 600) / 2)
    v1 = filter isFin (concatMap (\r -> [frTrainLL r / i2d len, frValLL r / i2d len]) rs)
    v2 = concatMap (\r -> [frBic r, frAic r]) rs
    g1 = if null rs then G 90 150 gwd 260 0.5 (i2d maxStates + 0.5) (-1.62) (-1.3)
         else G 90 150 gwd 260 0.5 (i2d maxStates + 0.5) (minOr 0 v1 - 0.01) (maxOr 0 v1 + 0.01)
    g2 = if null rs then G (90 + gwd + 90) 150 gwd 260 0.5 (i2d maxStates + 0.5) 0 1
         else G (90 + gwd + 90) 150 gwd 260 0.5 (i2d maxStates + 0.5) (minOr 0 v2 - 10) (maxOr 0 v2 + 10)
    ty = gy g1 + gh g1 + 90
    mx = gx g2 + gw g2 + 50
    dots g fn c = Color (col c) (Pictures (polyline 2.5 [ (gX g (i2d (frN r)), gY g (fn r)) | r <- rs ] : [ circleAt (gX g (i2d (frN r))) (gY g (fn r)) 9 | r <- rs ]))

-- +v.toFixed(3)
to3 :: Double -> Double
to3 v = if isFin v then read (toFixed v 3) else 0

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let Rng s = wRng w in (startFit w) { wRng = rngOf (fromIntegral (s + 12345)) }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> startFit w { wSeed = wSeed w + 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> startFit w { wTSel = min (length tChoices - 1) (wTSel w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> startFit w { wTSel = max 0 (wTSel w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wViewN = min maxStates (wViewN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wViewN = max 1 (wViewN w - 1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 黄色（訓練）は右へ行くほど上がるのに、水色（検証）は N = 3 のあとどうなる？
-- 2. ↓ でデータを 300 個に減らすと、BIC と AIC が選ぶ N は変わる？（データが少ないと、複雑なモデルは選ばれにくい）
-- 3. → で N = 5 のモデルを見よう。B のどの行とどの行が似ている？
-- 4. trueCafe の em の 3 行をもっと似せると（見分けにくくすると）、3 状態だと当てられる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest = withSeed 770 $ do
  (_, tr) <- hmmSample trueCafe 600
  (_, va) <- hmmSample trueCafe 600
  let run !k f = if fDone f || k >= (5000 :: Int) then pure f else fitStep f >>= run (k + 1)
  f <- run 0 (newFit tr va 5 5 40)
  let rs = fResults f
      r k = at' rs k (FitRes 0 trueCafe 0 0 0 0 0)
      okUp = length rs == 5 && and [ not (frTrainLL b < frTrainLL a - 1e-9) | (a, b) <- zip rs (drop 1 rs) ]
      tl = hmmLogLik trueCafe tr
      p = bestPerm 3 (\i j -> rowDist (at' (hEm trueCafe) i []) (at' (hEm (frM (r 2))) j []))
      al = permuteHmm (frM (r 2)) p
  sp <- splitState (frM (r 2)) False
  pure
    [ ("パラメータの数：N = 3, M = 5 で 6 + 12 + 2 = 20、N = 1 で 4、N = 6 で 59", paramCount 3 5 == 20 && paramCount 1 5 == 4 && paramCount 6 5 == 59)
    , ("訓練データの対数尤度は N とともに上がる（下がらない）：" ++ intercalate' " → " [ fmtN (frTrainLL x) 0 | x <- rs ], okUp)
    , ("BIC がいちばん小さいのは N = 3（本当の数）：" ++ intercalate' ", " [ fmtN (frBic x) 0 | x <- rs ], bestBy rs frBic (-1) == 3)
    , ("検証データの対数尤度がいちばん大きいのも N = 3、N = 5 は N = 3 より悪い（過学習）", bestBy rs frValLL 1 == 3 && frValLL (r 4) < frValLL (r 2) - 1)
    , ("AIC は BIC より罰金が軽い：N ≥ 2 で AIC < BIC（log 600 > 2）", all (\x -> frAic x < frBic x) (drop 1 rs) && near (frBic (r 2) - frAic (r 2)) (20 * (log 600 - 2)) 1e-9)
    , ("状態を 2 つに分けたモデル（4 状態）は、もとの 3 状態のモデルとまったく同じ尤度", near (hmmLogLik sp tr) (frTrainLL (r 2)) 1e-8 && nStates sp == 4 && rowSumErr (hTr sp) < 1e-12 && near (sumOf (hP0 sp)) 1 1e-12)
    , ("N = 3 の訓練の対数尤度 " ++ fmtN (frTrainLL (r 2)) 1 ++ " ≥ 本当のモデル " ++ fmtN tl 1, frTrainLL (r 2) >= tl - 1e-6)
    , ("N = 3：番号を合わせると B は本物に近い（ずれ < 0.12）", maxAbsDiff (hEm al) (hEm trueCafe) < 0.12)
    , ("N = 1 のモデル：B = 注文の割合そのもの", maxAbsDiffV (at' (hEm (frM (r 0))) 0 []) (normRow [ i2d (length (filter (== k) tr)) | k <- [0 .. 4] ]) < 1e-9) ]

intercalate' :: String -> [String] -> String
intercalate' sep xs = concat (zipWith (++) ("" : repeat sep) xs)

-- ==================================================
-- ここから下は「HMM の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 計算は Double（倍精度）、描くときだけ Float にする。分布 = リスト、行列 = 行のリスト
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（霧の夜と灯り：黄 = 見える観測、ばら色 = 隠れた状態）
type RGB = (Float, Float, Float)
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（行のリスト）

bgc, ink, lan, rose, aqua, lilac, leaf, tang, dimc, panelc, linec :: RGB
bgc = (27, 33, 44)                            -- 背景（霧の夜の青ねずみ色）
ink = (230, 234, 240)                         -- 文字（やわらかい白）
lan = (250, 208, 100)
rose = (236, 124, 150)
aqua = (110, 200, 210)
lilac = (176, 156, 236)
leaf = (150, 210, 130)
tang = (240, 160, 96)
dimc = (142, 154, 172)
panelc = (38, 46, 60)
linec = (76, 88, 108)

stCols :: [RGB]                               -- 状態の色
stCols = [rose, aqua, lan, lilac, leaf, tang, (200, 200, 210), (130, 150, 240)]

stCol :: Int -> RGB
stCol i = at' stCols (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampF' :: Float -> Float -> Float -> Float
clampF' v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isFin v then realToFrac v else 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

-- 行列の (i, j)（なければ 0）
at2 :: [[Double]] -> Int -> Int -> Double
at2 a i j = at' (at' a i []) j 0

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

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

forceM :: [[Double]] -> [[Double]]
forceM xs = foldl' (\u r -> forceL r `seq` u) () xs `seq` xs

-- 最初にいちばん大きい値の番号（同点なら小さい番号）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に。∞ は画面では inf）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "∞" else "−∞"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ fixedDigits n d
  where n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer

fixedDigits :: Integer -> Int -> String
fixedDigits n d =
  let ds = show n
      padded = replicate (d + 1 - length ds) '0' ++ ds
      (ip, fp) = splitAt (length padded - d) padded
  in if d > 0 then ip ++ "." ++ fp else ip

-- v.toFixed(d) そのまま（−0.0000 も残す）
toFixed :: Double -> Int -> String
toFixed v d
  | not (isFin v) = jsNum v
  | otherwise = (if v < 0 then "-" else "") ++ fixedDigits (floor (toRational (abs v) * 10 ^ d + 1 / 2)) d

-- 確率の表示：小さい数は 1.2e-5 の形で
fmtP :: Double -> String
fmtP v
  | isNaN v = "?"
  | v == 0 = "0"
  | abs v >= 0.001 = toFixed v (if v >= 0.1 then 3 else 4)
  | otherwise = toExp1 v

-- v.toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | not (isFin v) = jsNum v
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

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
seedRng s = R (\_ -> ((), rngOf s))

rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

-- 乱数の列 g で計算して、(結果, 進んだ列)
runWith :: Rng -> R a -> (a, Rng)
runWith g m = runR m g

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + 0x6d2b79f5 in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- n 回くり返して結果を順に集める（1 回ずつ状態を進める）
replicateR :: Int -> R a -> R [a]
replicateR n m = go n []
  where go k acc = if k <= 0 then pure (reverse acc) else do { !x <- m; go (k - 1) (x : acc) }

-- 乱数を使う左たたみ込み（JavaScript の for で状態を書きかえていく計算）
foldlR :: (b -> a -> R b) -> b -> [a] -> R b
foldlR f = go
  where go !acc [] = pure acc
        go !acc (x : xs) = f acc x >>= \a -> go a xs

-- ---- 隠れマルコフモデルの道具
-- モデル = Hmm { hP0 = はじめの分布 π, hTr = 遷移行列 A（hTr !! i !! j = 状態 i の次に j）, hEm = 出力行列 B（hEm !! j !! k = 状態 j が記号 k を出す） }
-- どの計算も「出力の表」eb !! t !! j = b_j(x_t)（時刻 t の観測を状態 j が出す確率）を受け取る → 記号でも数（正規分布）でも同じ道具が使える
data Hmm = Hmm { hP0 :: Vec, hTr :: Mat, hEm :: Mat }

nStates :: Hmm -> Int
nStates = length . hP0

lg :: Double -> Double                        -- log 0 は −∞（NaN にしない）
lg p = if p > 0 then log p else negInf

-- log(Σ e^a_i)：いちばん大きい値を先に引く。全部 −∞ なら −∞
logSumExp :: [Double] -> Double
logSumExp a =
  let m = foldl' (\b v -> if v > b then v else b) negInf a
  in if m == negInf then negInf else m + log (sumOf [ exp (v - m) | v <- a ])

-- 合計 1 に（全部 0 なら一様に）
normRow :: Vec -> Vec
normRow r = let s = sumOf r in if s > 1e-300 then map (/ s) r else map (const (1 / i2d (length r))) r

rowSumErr :: Mat -> Double
rowSumErr a = foldl' (\d r -> maxN d (abs (sumOf r - 1))) 0 a

-- 2 つの行列（またはベクトル）のいちばん大きなずれ（NaN が混じれば NaN）
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- 横ベクトル × 行列：1 歩先の分布
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (length (headOr [] a)) 0) (zip v a)

emTable :: Hmm -> [Int] -> Mat                -- 記号の列 → 出力の表
emTable m xs = [ [ at' r x 0 | r <- hEm m ] | x <- xs ]

-- π, A, B からサイコロで列を作る：(隠れた状態, 観測)
hmmSample :: Hmm -> Int -> R ([Int], [Int])
hmmSample m n = do
  z0 <- sampleFrom (hP0 m)
  let go t z zs xs
        | t >= n = pure (reverse zs, reverse xs)
        | otherwise = do
            z' <- if t > 0 then sampleFrom (at' (hTr m) z []) else pure z
            x <- sampleFrom (at' (hEm m) z' [])
            go (t + 1) z' (z' : zs) (x : xs)
  go (0 :: Int) z0 [] []

-- ★ 前向き：α_t(j) = P(x_1..x_t, z_t = j) = b_j(x_t) Σ_i α_{t−1}(i) a_ij。尤度 P(x) = Σ_j α_T(j)
fwdPlain :: Hmm -> Mat -> (Mat, Double)
fwdPlain m eb = (al, if null al then 1 else sumOf (last al))
  where
    n = nStates m
    al = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s * at' e j 0 | j <- [0 .. n - 1]
                       , let s = if first then at' (hP0 m) j 0 else sumOf [ at' prev i 0 * at2 (hTr m) i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 後ろ向き：β_t(i) = P(x_{t+1}..x_T | z_t = i) = Σ_j a_ij b_j(x_{t+1}) β_{t+1}(j)。尤度 = Σ_i π_i b_i(x_1) β_1(i)
bwdPlain :: Hmm -> Mat -> (Mat, Double)
bwdPlain m eb = (be, lik)
  where
    n = nStates m
    be = if null eb then [] else foldr step [replicate n 1] (drop 1 eb)
    step e acc = let nb = headOr [] acc
                 in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] : acc
    lik = if null eb then 1 else sumOf [ at' (hP0 m) i 0 * at' (headOr [] eb) i 0 * at' (headOr [] be) i 0 | i <- [0 .. n - 1] ]

-- ★ スケーリングつきの前向き：毎時刻 合計 c_t で割る。fsAh !! t = P(z_t | x_1..x_t)（= フィルタ）、log P(x) = Σ log c_t
--   c_t = 0（ありえない観測）なら logLik = −∞ にして、その時刻は一様分布で先へ進む（NaN を作らない）
data FS = FS { fsAh :: [Vec], fsCs :: [Double], fsLogLik :: !Double }

fwdScaled :: Hmm -> Mat -> FS
fwdScaled m eb = let (ahR, csR, ll) = foldl' step ([], [], 0) eb in FS (reverse ahR) (reverse csR) ll
  where
    n = nStates m
    step (ah, cs, !ll) e =
      let pred' = if null ah then hP0 m else vecMat (headOr [] ah) (hTr m)
          row = zipWith (*) pred' e
          c = sumOf row
      in if not (c > 0) then (replicate n (1 / i2d n) : ah, 0 : cs, negInf)
         else (forceL (map (/ c) row) : ah, c : cs, if ll /= negInf then ll + log c else ll)

-- 同じ c_t で割った後ろ向き。γ_t(i) = ah !! t !! i × bh !! t !! i
bwdScaled :: Hmm -> Mat -> [Double] -> [Vec]
bwdScaled m eb cs = if null eb then [] else foldr step [replicate n 1] (zip (drop 1 eb) (drop 1 cs))
  where
    n = nStates m
    step (e, c0) acc =
      let nb = headOr [] acc
          c = if c0 > 0 then c0 else 1
      in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] / c | i <- [0 .. n - 1] ] : acc

-- 対数のまま：log α_t(j) = log b_j(x_t) + logSumExp_i(log α_{t−1}(i) + log a_ij)
fwdLog :: Hmm -> Mat -> (Mat, Double)
fwdLog m eb = (la, if null la then 0 else logSumExp (last la))
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    la = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s + lg (at' e j 0) | j <- [0 .. n - 1]
                       , let s = if first then lg (at' (hP0 m) j 0) else logSumExp [ at' prev i 0 + at2 lt i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 平滑化：γ_t(i) = P(z_t = i | 全部の観測)。smFilt = フィルタ P(z_t | x_1..x_t)
data SM = SM { smFilt :: [Vec], smGam :: [Vec], smBh :: [Vec], smCs :: [Double], smLogLik :: !Double }

smoothAll :: Hmm -> Mat -> SM
smoothAll m eb =
  let f = fwdScaled m eb
      bh = bwdScaled m eb (fsCs f)
      gam = zipWith (\a b -> forceL (normRow (zipWith (*) a b))) (fsAh f) bh
  in SM (fsAh f) gam bh (fsCs f) (fsLogLik f)

-- ξ_t(i, j) = P(z_t = i, z_{t+1} = j | 全部の観測)
xiAt :: Hmm -> Mat -> SM -> Int -> Mat
xiAt m eb sm t =
  let n = nStates m
      c0 = at' (smCs sm) (t + 1) 0
      c = if c0 > 0 then c0 else 1
      ft = at' (smFilt sm) t []
      e1 = at' eb (t + 1) []
      b1 = at' (smBh sm) (t + 1) []
  in [ [ at' ft i 0 * at2 (hTr m) i j * at' e1 j 0 * at' b1 j 0 / c | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

-- ★ ビタビ（対数）：vtDl !! t !! j = 時刻 t に状態 j で終わるいちばん良い道の log 確率、vtBp !! t !! j = そのとき 1 つ前にいた状態（戻り矢印、なければ −1）
data VT = VT { vtPath :: [Int], vtLogP :: !Double, vtDl :: [Vec], vtBp :: [[Int]] }

viterbiLog :: Hmm -> Mat -> VT
viterbiLog m eb = VT path logP dl bp
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    (dlR, bpR) = foldl' step ([], []) eb
    dl = reverse dlR
    bp = reverse bpR
    step (ds, bs) e =
      let prev = headOr [] ds
          cells = [ if null ds then (lg (at' (hP0 m) j 0), -1)
                    else foldl' (\(b, a) i -> let v = at' prev i 0 + at2 lt i j in if v > b then (v, i) else (b, a)) (negInf, -1) [0 .. n - 1]
                  | j <- [0 .. n - 1] ]
          row = forceL [ b + lg (at' e j 0) | (j, (b, _)) <- zip [0 ..] cells ]
      in (row : ds, map snd cells : bs)
    lastRow = lastOr [] dl
    zEnd = argMax lastRow
    logP = if null eb then 0 else at' lastRow zEnd negInf
    path = if null eb then [] else reverse (goBack zEnd bpR)
    goBack _ [] = []
    goBack z (b : rest) = let p = at' b z (-1) in z : goBack (if p >= 0 then p else 0) rest

-- 1 本の道 zs と観測がいっしょに起きる log 確率
pathLogProb :: Hmm -> Mat -> [Int] -> Double
pathLogProb m eb zs = foldl' (\s (t, z, zp, e) -> s + ((if t == 0 then lg (at' (hP0 m) z 0) else lg (at2 (hTr m) zp z)) + lg (at' e z 0))) 0
                        (zip4' [0 :: Int ..] zs (0 : zs) eb)

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

postDecode :: [Vec] -> [Int]                  -- 事後復号：時刻ごとにいちばんありそうな状態
postDecode = map argMax

-- ★ 総当たり：N^T 本の道を全部ためす（短い列だけ）。尤度、γ、ξ、いちばん良い道
data BF = BF { bfLik :: !Double, bfGam :: [Vec], bfXi :: [Mat], bfBest :: [Int], bfBestP :: !Double, bfPaths :: !Int }

bruteAll :: Hmm -> Mat -> BF
bruteAll m eb = BF lik gam xi best bestP total
  where
    n = nStates m
    len = length eb
    total = n ^ len
    ebA = listArray ((0, 0), (max 0 (len - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) eb ++ repeat 0) :: UArray (Int, Int) Double
    p0A = listArray (0, max 0 (n - 1)) (hP0 m ++ repeat 0) :: UArray Int Double
    trA = listArray ((0, 0), (max 0 (n - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) (hTr m) ++ repeat 0) :: UArray (Int, Int) Double
    pathOf k = U.generate len (\t -> (k `div` (n ^ (len - 1 - t))) `mod` n)
    probOf zs = let go t p = if t >= len || not (p > 0) then p
                             else let z = zs U.! t in go (t + 1) (p * ((if t == 0 then p0A ! z else trA ! (zs U.! (t - 1), z)) * ebA ! (t, z)))
                in go 0 1
    paths = [ (zs, probOf zs) | k <- [0 .. total - 1], let zs = pathOf k ]
    (lik0, bestP, bestV) = foldl' (\(l, bp, bz) (zs, p) -> let (bp', bz') = if p > bp then (p, zs) else (bp, bz) in (if p == 0 then l else l + p, bp', bz')) (0, -1, U.empty) paths
    best = U.toList bestV
    gamA = accumArray (+) 0 ((0, 0), (max 0 (len - 1), max 0 (n - 1))) [ ((t, zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [0 .. len - 1] ] :: UArray (Int, Int) Double
    xiA = accumArray (+) 0 ((0, 0, 0), (max 0 (len - 2), max 0 (n - 1), max 0 (n - 1))) [ ((t - 1, zs U.! (t - 1), zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [1 .. len - 1] ] :: UArray (Int, Int, Int) Double
    lik = lik0
    dv v = if lik > 0 then v / lik else v
    gam = [ [ dv (gamA ! (t, j)) | j <- [0 .. n - 1] ] | t <- [0 .. len - 1] ]
    xi = [ [ [ dv (xiA ! (t, i, j)) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] | t <- [0 .. len - 2] ]

-- 定常分布 π A = π（なまけた連鎖 (π + πA)/2 をくり返す。周期があっても収束する）
stationaryOf :: Mat -> Vec
stationaryOf tr = normRow (go (0 :: Int) (replicate n (1 / i2d n)))
  where
    n = length tr
    go k v
      | k >= 4000 = v
      | otherwise =
          let w = vecMat v tr
              u = forceL (zipWith (\a b -> (a + b) / 2) v w)
              d = foldl' maxN 0 (zipWith (\a b -> abs (a - b)) u v)
          in if d < 1e-15 then u else go (k + 1) u

hmmLogLik :: Hmm -> [Int] -> Double           -- 記号の列の対数尤度 log P(x)
hmmLogLik m xs = fsLogLik (fwdScaled m (emTable m xs))

predictK :: Vec -> Mat -> Int -> Vec          -- k 歩先の予測：A を k 回掛ける
predictK belief tr k = iterate (`vecMat` tr) belief !! max 0 k

-- ---- バウム-ウェルチ（EM）の道具
-- でたらめな行（(0.05 + rnd())^sharp を合計 1 に。sharp = 0 は 1 と同じ）
randomRow :: Int -> Double -> R Vec
randomRow k sharp = do
  r <- replicateR k (fmap (\u -> (0.05 + u) ** (if sharp == 0 then 1 else sharp)) rnd)
  pure (normRow r)

-- でたらめなモデル：π のあと、状態ごとに A の行、B の行の順に引く
randomHmm :: Int -> Int -> R Hmm
randomHmm n k = do
  p0 <- randomRow n 1
  rows <- replicateR n (do a <- randomRow n 1; b <- randomRow k 1; pure (a, b))
  pure (Hmm p0 (map fst rows) (map snd rows))

-- ★ 1 回の EM：E ステップで γ（状態 i にいた期待回数）と ξ（i → j と動いた期待回数）を足し合わせ、M ステップで割り算する
--   seqs = 観測列のリスト。eps = 出力の数に足す小さな数（0 でもよい）。戻り値の対数尤度は「更新する前」のモデルのもの
baumWelchStep :: Hmm -> [[Int]] -> Double -> (Hmm, Double)
baumWelchStep m seqs eps = (Hmm p0' tr' em', logLik)
  where
    n = nStates m
    k = length (headOr [] (hEm m))
    start = (replicate n 0, replicate n (replicate n 0), replicate n (replicate k eps), 0)
    (g0, trNum, emNum, logLik) = foldl' oneSeq start seqs
    oneSeq acc@(g, tn, en, !ll) xs
      | null xs = acc
      | otherwise =
          let eb = emTable m xs
              sm = smoothAll m eb
              ll' = ll + smLogLik sm
          in if smLogLik sm == negInf then (g, tn, en, ll') else
               let g' = forceL (zipWith (+) g (headOr [] (smGam sm)))
                   en' = foldl' (\e (x, gm) -> forceM [ [ if s == x then v + at' gm i 0 else v | (s, v) <- zip [0 ..] row ] | (i, row) <- zip [0 ..] e ]) en (zip xs (smGam sm))
                   tn' = foldl' (\tacc (ft, e1, b1, c) ->
                                   forceM [ let a = at' ft i 0 in if a == 0 then row else
                                              [ v + (a * at2 (hTr m) i j * at' e1 j 0 * at' b1 j 0) / c | (j, v) <- zip [0 ..] row ]
                                          | (i, row) <- zip [0 ..] tacc ])
                                tn (zip4' (smFilt sm) (drop 1 eb) (drop 1 (smBh sm)) (drop 1 (smCs sm)))
               in (g', tn', en', ll')
    p0' = if sumOf g0 > 0 then normRow g0 else hP0 m
    tr' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip trNum (hTr m) ]
    em' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip emNum (hEm m) ]

permsOf :: Int -> [[Int]]                     -- 0..n−1 の並べかえを全部（辞書順）
permsOf n = go [0 .. n - 1]
  where go [] = [[]]
        go xs = [ x : rest | x <- xs, rest <- go (filter (/= x) xs) ]

-- ★ ラベルの入れかえ：cost(本物 i, 学習 p !! i) の合計が最小になる並べかえ p
bestPerm :: Int -> (Int -> Int -> Double) -> [Int]
bestPerm n cost = snd (foldl' (\(bc, bp) p -> let c = sumOf [ cost i (at' p i 0) | i <- [0 .. n - 1] ] in if c < bc then (c, p) else (bc, bp)) (posInf, [0 .. n - 1]) (permsOf n))

permuteHmm :: Hmm -> [Int] -> Hmm
permuteHmm m p = Hmm [ at' (hP0 m) i 0 | i <- p ] [ [ at2 (hTr m) i j | j <- p ] | i <- p ] [ at' (hEm m) i [] | i <- p ]

rowDist :: Vec -> Vec -> Double
rowDist a b = sumOf (zipWith (\x y -> abs (x - y)) a b)

-- ---- グラフの道具：G { 左上 x y, 幅 w, 高さ h, x の範囲 x0〜x1, y の範囲 y0〜y1 }
data G = G { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double }

gX :: G -> Double -> Float
gX g v = gx g + f2 (clampv ((v - gx0 g) / (gx1 g - gx0 g)) (-0.02) 1.02) * gw g

gY :: G -> Double -> Float
gY g v = gy g + gh g - f2 (clampv ((v - gy0 g) / (gy1 g - gy0 g)) (-0.02) 1.02) * gh g

-- 枠と目盛り（目盛りの数は有効数字 4 桁）、x 軸と y 軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures
  [ Color (col linec) (frameTL 1 (gx g) (gy g) (gw g) (gh g))
  , Pictures [ sayT (precN 4 v) (gX g v) (gy g + gh g + 4) 14 (col dimc) True | v <- xt ]
  , Pictures [ sayRM (precN 4 v) (gx g - 5) (gY g v) 14 (col dimc) | v <- yt ]
  , sayR xl (gx g + gw g) (gy g + gh g + 34) 14 (col ink)
  , say yl (gx g) (gy g - 8) 14 (col ink) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで（from〜to−1 だけ）
drawCurve :: G -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff = drawCurveR g arr c sw xOff 0 (length arr)

drawCurveR :: G -> [Double] -> RGB -> Float -> Double -> Int -> Int -> Picture
drawCurveR g arr c sw xOff a b =
  let n = b - a
      stride = max 1 ((n + 599) `div` 600)
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [a ..] (take n (drop a arr)), (i - a) `mod` stride == 0, isFin v ]
  in Color (col c) (polyline sw pts)

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [ Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink) ] | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 行列を色つきの表で（明るい = 大きい）
data MatOpt = MatOpt { moDigits :: Int, moCol :: RGB, moTs :: Float, moHi :: Double, moNumbers :: Bool }

matOpt :: MatOpt
matOpt = MatOpt 2 aqua 14 1 True

drawMatrix :: Mat -> Float -> Float -> Float -> Float -> [String] -> [String] -> MatOpt -> Picture
drawMatrix a x y cw ch rowNames colNames o = Pictures
  [ Pictures [ Pictures [ Color (col (mixC t)) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
                        , if moNumbers o then sayM (fmtN v (moDigits o)) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col ink) else Blank ]
             | (i, row) <- zip [0 :: Int ..] a, (j, v) <- zip [0 :: Int ..] row, let t = f2 (clampv (abs v / moHi o) 0 1) ]
  , Pictures [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col dimc) | (i, nm) <- zip [0 :: Int ..] (take (length a) rowNames) ]
  , Pictures [ sayC nm (x + i2f j * cw + cw / 2) (y - 4) ts (col dimc) | (j, nm) <- zip [0 :: Int ..] (take (length (headOr [] a)) colNames) ] ]
  where
    ts = max 14 (moTs o)
    (pr, pg, pb) = panelc
    (cr, cg, cb) = moCol o
    mixC t = (pr + t * (cr * 0.75 - pr), pg + t * (cg * 0.75 - pg), pb + t * (cb * 0.75 - pb))

-- 状態の列を色の帯で（同じ色が続くところは 1 つの四角にまとめる）。色が Nothing なら描かない
drawBand :: [Int] -> Float -> Float -> Float -> Float -> (Int -> Maybe RGB) -> Picture
drawBand arr x y w h colOf = Pictures [ case colOf v of
                                          Just c -> Color (col c) (rectTL (x + i2f s / n * w) y (i2f (e - s) / n * w + 0.6) h)
                                          Nothing -> Blank
                                      | (s, e, v) <- runsOf arr ]
  where n = i2f (max 1 (length arr))

-- 同じ値が続く区間 (はじめ, 終わり, 値)
runsOf :: [Int] -> [(Int, Int, Int)]
runsOf = go 0
  where go _ [] = []
        go s (v : vs) = let k = length (takeWhile (== v) vs) in (s, s + k + 1, v) : go (s + k + 1) (drop k vs)

-- 矢印（太さ sw、頭の大きさ hs）
arrowLine :: Float -> Color -> (Float, Float) -> (Float, Float) -> Float -> Picture
arrowLine sw c (x1, y1) (x2, y2) hs =
  let l0 = hyp (x2 - x1) (y2 - y1)
      l = if l0 == 0 then 1 else l0
      ux = (x2 - x1) / l
      uy = (y2 - y1) / l
  in Color c (Pictures [ lineW sw (x1, y1) (x2 - ux * hs * 0.6, y2 - uy * hs * 0.6)
                       , Polygon [(x2, y2), (x2 - ux * hs - uy * hs * 0.5, y2 - uy * hs + ux * hs * 0.5), (x2 - ux * hs + uy * hs * 0.5, y2 - uy * hs - ux * hs * 0.5)] ])

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 画面の文字は ASCII だけ（∞ や − などは近い ASCII に直す）
asciiText :: String -> String
asciiText = concatMap f
  where f c = case c of
          '∞' -> "inf"; '−' -> "-"; '×' -> "x"; '≈' -> "~"; '≤' -> "<="; '≥' -> ">="; '→' -> "->"; '←' -> "<-"
          '·' -> "."; '…' -> "..."; '✓' -> "OK"; '✗' -> "x"; 'π' -> "pi"; 'α' -> "a"; 'β' -> "b"; 'γ' -> "g"
          _ -> [c]

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c (asciiText s)))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

-- p5.js 版の note(s, x, y, 色, 大きさ)：左揃え・BASELINE（大きさは 14 より小さくしない）
note :: String -> Float -> Float -> RGB -> Float -> Picture
note s x y c sz = say s x y (max 14 sz) (col c)

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
textW s size = fromIntegral (length (asciiText s)) * size * 0.72

-- 幅 w に収まるように単語の切れ目で折り返す
wrapText :: Float -> Float -> String -> [String]
wrapText w size s = concatMap wrapLine (lines s)
  where
    wrapLine ln = go [] (words ln)
    go cur [] = [unwords (reverse cur) | not (null cur)]
    go cur (x : xs) = let cand = unwords (reverse (x : cur))
                      in if null cur || textW cand size <= w then go (x : cur) xs else unwords (reverse cur) : go [x] xs

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か（左上を含み、右下を含まない）
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx >= bx && mx < bx + bw && my >= by && my < by + bh

-- パネル（p5.js 版の角の丸い長方形）
panelBox :: Float -> Float -> Float -> Float -> Picture
panelBox x y w h = Color (col panelc) (rectTL x y w h)

-- 左上の見出しと操作の説明（説明は幅に合わせて折り返す）
hudTitle :: Env -> String -> String -> Picture
hudTitle env t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (wrapText (envW env - 60) 14 s) ])

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

noNaN :: [Double] -> Bool                     -- NaN がない（∞ はよい）
noNaN = not . any isNaN

noNaNM :: [[Double]] -> Bool
noNaNM = all noNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  hSetEncoding stdout utf8
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下に小さく（p5.js 版の drawSelfTest）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 210) (envH env - 14) 14 (if all snd rs then col leaf else col (255, 90, 90))
