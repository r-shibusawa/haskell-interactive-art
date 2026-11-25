-- ============================================
-- 68-07 深さと初期化 ─ 幅 N のネットを 20 層重ね、ランダムな重み（分散 σ_w²/N）で入力を流す
-- 層ごとの信号の大きさ q_l = (z の 2 乗の平均) は、N が大きいと漸化式 q_l = σ_w² E[f(√q_(l−1) ξ)²]（ξ は標準正規分布）に従う
-- 勾配の大きさ G は後ろから G_(l−1) = σ_w² E[f'(z_l)²] G_l で伝わる。1 層あたりの倍率が 1 より小さいと消え、大きいと爆発
--   ReLU：E[f²] = q/2、E[f'²] = 1/2 → σ_w² = 2（ヒー/He の初期化）でちょうど 1 倍 → 何層でも保たれる
--   tanh：σ_w² = 1（ザビエル/Xavier）で小さな信号の倍率がちょうど 1 → 勾配がほぼ保たれる（信号はゆっくり小さくなる）
-- 太い線 = 理論の漸化式、点 = 本物のランダムなネット（毎フレーム 1 個作って平均）、細い線 = いちばん新しい 1 個
-- 操作：1 tanh / 2 ReLU / ← → σ_w² / X ザビエル（σ_w² = 1）/ H ヒー（σ_w² = 2）/ スペース 一時停止 / R 平均をやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

widthN, depth, batchB :: Int
widthN = 96                -- ★ 幅
depth = 20                 -- ★ 層の数
batchB = 4                 -- ★ 1 回に流す入力の数

unitsPerFrame :: Int
unitsPerFrame = 21         -- ★ 1 フレームに進める仕事の数（1 個のネット = 41 個：前向き 20 層、d、後ろ向き 20 層）

sw2List :: [Double]
sw2List = [0.5, 1, 1.5, 2, 3, 4]

data World = World
  { wAct    :: !Act
  , wSw2    :: !Int
  , wSimQ   :: ![Double]          -- 本物のネットの平均（l = 0〜20）
  , wSimG   :: ![Double]
  , wLastQ  :: !(Maybe [Double])  -- いちばん新しい 1 個
  , wLastG  :: !(Maybe [Double])
  , wNets   :: !Int
  , wPaused :: !Bool
  , wJob    :: !(Maybe Job)       -- 計算中のネット
  , wThQ    :: ![Double]          -- 理論の漸化式
  , wThG    :: ![Double]
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-07 Depth and initialization" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (wThQ world0) + sum (wThG world0))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = resetSim (World Relu 3 [] [] Nothing Nothing 0 False Nothing [] [] (rngOf 77) [])

sw2Of :: World -> Double
sw2Of w = at' sw2List (wSw2 w) 2

-- ガウス分布の期待値 E[g(√q ξ)]（中点則、ξ ∈ [−8, 8] を 800 等分。ξ = 0 ちょうどは使わないので ReLU の角でも正確）
gaussE :: (Double -> Double) -> Double -> Double
gaussE g q = (go 0 0 * (16 / i2d n)) / sqrt (2 * pi)
  where
    n = 800 :: Int
    sq = sqrt (max 0 q)
    go !i !s | i >= n = s
             | otherwise = let x = -8 + (16 * (i2d i + 0.5)) / i2d n in go (i + 1) (s + exp (-x * x / 2) * g (sq * x))

-- ★ 理論の漸化式：q[l]（l = 1…L の z の分散、q[0] = 0 は使わない）、grad[l]（l = 0…L、出力側を 1 とした勾配の 2 乗の平均）
theoryCurves :: Act -> Double -> Int -> ([Double], [Double])
theoryCurves act sw2 bigL = (qs, gs)
  where
    qs = 0 : take bigL (iterate (\qp -> sw2 * gaussE (\z -> let f = actF act z in f * f) qp) (sw2 * 1))
    gs = reverse (scanl (\gl l -> gl * sw2 * gaussE (\z -> let d = actD act z in d * d) (at' qs l 0)) 1 [bigL, bigL - 1 .. 1])

-- ★ 本物のネットを 1 個：前向きに q_l、後ろ向きに勾配（出力側にランダムな勾配ベクトルを置いて W の転置で戻す）
-- 乱数の順：入力 x（b × n）→ 層ごとの重み W（n × n）→ 出力側の勾配 d（b × n）
-- 1 個を何フレームかに分けて計算できるように、「仕事」を 1 層ずつ進める（Job）
data Job = Job { jStep :: !Int                 -- 0〜L−1：前向きの層、L：d を作る、L+1〜2L：後ろ向きの層
               , jH :: !Vec                    -- いまの層の出力 h（b × n）
               , jLayers :: ![(Vec, Vec)]      -- (W, z) 新しい順
               , jQ :: ![Double]               -- q_l 新しい順
               , jD :: !Vec                    -- 後ろ向きの d
               , jG :: ![Double] }             -- 勾配（後ろから。最後に 0〜L の順になる）

data SimP = SimP { spAct :: !Act, spSw2 :: !Double, spN :: !Int, spL :: !Int, spB :: !Int }

jobStart :: SimP -> R Job
jobStart sp = do
  h0 <- randnVec (spB sp * spN sp)
  pure (Job 0 h0 [] [0] U.empty [])

jobDone :: SimP -> Job -> Bool
jobDone sp j = jStep j > 2 * spL sp

-- 仕事を 1 つ進める
jobAdvance :: SimP -> Job -> R Job
jobAdvance sp j
  | k < bigL = do                               -- 前向き：W を引いて z = W h、q = z² の平均
      r <- randnVec (n * n)
      let w = U.map (sd *) r
          z = matRows w (jH j)
          s = U.foldl' (\acc a -> acc + a * a) 0 z
      pure j { jStep = k + 1, jH = U.map (actF act) z, jLayers = (w, z) : jLayers j, jQ = s / nb : jQ j }
  | k == bigL = do                              -- 出力側のランダムな勾配
      d0 <- randnVec (b * n)
      pure j { jStep = k + 1, jD = d0, jG = [U.foldl' (\acc v -> acc + v * v) 0 d0 / nb] }
  | otherwise =                                 -- 後ろ向き：u = d f'(z)、新しい d = Wᵀ u
      let l = 2 * bigL + 1 - k                  -- 層 l（L, L−1, …, 1）
          (w, z) = at' (reverse (jLayers j)) (l - 1) (U.empty, U.empty)
          u = U.imap (\ki dv -> dv * actD act (U.unsafeIndex z ki)) (jD j)
          out = matColsT w u
          s = U.foldl' (\acc v -> acc + v * v) 0 out
      in pure j { jStep = k + 1, jD = out, jG = s / nb : jG j }
  where
    k = jStep j
    act = spAct sp
    n = spN sp
    b = spB sp
    bigL = spL sp
    sd = sqrt (spSw2 sp / i2d n)
    nb = i2d (n * b)
    -- z[k][i] = Σ_j w[i][j] h[k][j]
    matRows w h = runST $ do
      zm <- MU.new (b * n)
      let loopK kk = when (kk < b) $ do
            let loopI i = when (i < n) $ do
                  let o = i * n
                      hk = kk * n
                      go !jj !a = if jj >= n then a else go (jj + 1) (a + U.unsafeIndex w (o + jj) * U.unsafeIndex h (hk + jj))
                  MU.unsafeWrite zm (hk + i) (go 0 0)
                  loopI (i + 1)
            loopI 0
            loopK (kk + 1)
      loopK 0
      U.unsafeFreeze zm
    -- out[k][j] = Σ_i w[i][j] u[k][i]（u = 0 の所はとばす）
    matColsT w u = runST $ do
      om <- MU.replicate (b * n) 0
      let loopK kk = when (kk < b) $ do
            let hk = kk * n
                loopI i = when (i < n) $ do
                  let ui = U.unsafeIndex u (hk + i)
                      o = i * n
                      loopJ jj = when (jj < n) $ do
                        MU.unsafeModify om (+ U.unsafeIndex w (o + jj) * ui) (hk + jj)
                        loopJ (jj + 1)
                  when (ui /= 0) (loopJ 0)
                  loopI (i + 1)
            loopI 0
            loopK (kk + 1)
      loopK 0
      U.unsafeFreeze om

-- 仕事が終わったら (q, G)（G は出力側 = 1 にそろえる）
jobResult :: Job -> ([Double], [Double])
jobResult j = let gL = lastOr 1 (jG j) in (reverse (jQ j), map (\v -> v / max 1e-300 gL) (jG j))

-- 1 個を最後まで（自己テスト用）
simulateNet :: Act -> Double -> Int -> Int -> Int -> R ([Double], [Double])
simulateNet act sw2 n bigL b = do
  let sp = SimP act sw2 n bigL b
      go j = if jobDone sp j then pure (jobResult j) else jobAdvance sp j >>= go
  jobStart sp >>= go

resetSim :: World -> World
resetSim w = let (tq, tg) = theoryCurves (wAct w) (sw2Of w) depth
             in w { wSimQ = replicate (depth + 1) 0, wSimG = replicate (depth + 1) 0, wNets = 0, wJob = Nothing, wLastQ = Nothing, wLastG = Nothing, wThQ = tq, wThG = tg }

-- ネットを少しずつ計算し、できたら平均に入れる（p5.js 版は 1 フレームに 1 個。ここでは unitsPerFrame ずつ）
step :: Env -> Float -> World -> IO World
step _ _ w0 = pure (go unitsPerFrame w0)
  where
    sp w = SimP (wAct w) (sw2Of w) widthN depth batchB
    go budget w
      | wPaused w || wNets w >= 2000 || budget <= 0 = w
      | otherwise =
          let (job, g1) = case wJob w of
                Just j -> (j, wRng w)
                Nothing -> runR (jobStart (sp w)) (wRng w)
              (job', g2) = runR (jobAdvance (sp w) job) g1
          in if jobDone (sp w) job'
               then let (q, g) = jobResult job'
                        k = i2d (wNets w + 1)
                        upd sim r = zipWith (\s v -> s + (v - s) / k) sim r
                    in go (budget - 1) w { wSimQ = upd (wSimQ w) q, wSimG = upd (wSimG w) g, wNets = wNets w + 1
                                         , wLastQ = Just q, wLastG = Just g, wJob = Nothing, wRng = g2 }
               else go (budget - 1) w { wJob = Just job', wRng = g2 }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle (actName ++ "  sigma_w^2 = " ++ jsNum sw2 ++ (if sw2 == crit then (if isRelu then " (He init)" else " (Xavier init)") else "")
              ++ "   width " ++ show widthN ++ ", " ++ show depth ++ " layers   mean of " ++ show (wNets w) ++ " nets")
             ("1 tanh   2 ReLU   Left/Right sigma_w^2   X Xavier (1)   H He (2)   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R restart the average")
  , plotAxes g1 "layer l (deeper to the right)" "forward: signal size log10 q_l" [1, 5, 10, 15, 20] [i2d (ceiling (fst rq) :: Int), 0, i2d (floor (snd rq) :: Int)]
  , plotAxes g2 "layer l (output at right)" "backward: gradient size log10 G_l (output = 1)" [0, 5, 10, 15, 20] [i2d (ceiling (fst rg) :: Int), 0, i2d (floor (snd rg) :: Int)]
  , panelP g1 (lgq (wThQ w)) (lgq (wSimQ w)) (fmap lgq (wLastQ w)) mag
  , panelP g2 (lgg (wThG w)) (lgg (wSimG w)) (fmap lgg (wLastG w)) lime
  , legend (grX g1 + 12) (grY g1 + 16) [(mag, "theory (recursion)"), (ink, "real nets (dot = mean, thin = one net)")]
  , say ("gain per layer (small signal): " ++ (if isRelu then "sigma_w^2 / 2 = " else "sigma_w^2 x tanh'(0)^2 = ") ++ fmtN per 2
         ++ (if abs (per - 1) < 1e-9 then "  -> exactly 1 (kept)" else if per < 1 then "  -> below 1 (vanishes)" else "  -> above 1 (explodes / tanh saturates)")) 70 ty 15 (col ink)
  , say ("q at layer 20: theory " ++ fmtE (lastQ (wThQ w)) ++ "  real " ++ fmtE (lastQ (wSimQ w)) ++ "  ratio " ++ fmtN ratio 3
         ++ "   input-side gradient G_0: theory " ++ fmtE (headOr 0 (wThG w)) ++ "  real " ++ fmtE (headOr 0 (wSimG w))) 70 (ty + 24) 15 (col ink)
  , say "The thin line (one net) is jagged: finite width shifts every layer a little. Averaging many nets approaches the theory." 70 (ty + 50) 14 (col sub)
  , say "tanh with sigma_w^2 = 2 etc.: q settles at a fixed point q*. Below 1 it goes to 0. Sigmoid gradient vanishing: see 01." 70 (ty + 70) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    isRelu = wAct w == Relu
    actName = if isRelu then "Depth and initialization: ReLU" else "Depth and initialization: tanh"
    sw2 = sw2Of w
    crit = if isRelu then 2 else 1
    per = if isRelu then sw2 / 2 else sw2
    ls = map i2d [0 .. depth]
    pw = (envW env - 140) / 2
    ph = envH env - 300
    lgq arr = [ if l == 0 then 0 / 0 else logBase 10 (max 1e-30 v) | (l, v) <- zip [0 :: Int ..] arr ]
    lgg = map (\v -> logBase 10 (max 1e-30 v))
    rangeOf arrs = let vs = [ v | a <- arrs, v <- a, isFin v ]
                       lo = foldl' min 0 vs
                       hi = foldl' max 0 vs
                   in (i2d (floor lo :: Int) - 0.5, i2d (ceiling hi :: Int) + 0.5)
    rq = rangeOf (if wNets w > 0 then [lgq (wThQ w), lgq (wSimQ w)] else [lgq (wThQ w)])
    rg = rangeOf (if wNets w > 0 then [lgg (wThG w), lgg (wSimG w)] else [lgg (wThG w)])
    g1 = Graph 70 110 pw ph 1 (i2d depth) (fst rq) (snd rq)
    g2 = Graph (70 + pw + 70) 110 (pw - 30) ph 0 (i2d depth) (fst rg) (snd rg)
    panelP g th sim lst c = Pictures
      [ Color (col wire) (Line [(grX g, gY g 0), (grX g + grW g, gY g 0)])
      , plotXY g ls th (col c) 4
      , maybe Blank (\l -> plotXY g ls l (colA 110 ink) 1) lst
      , if wNets w > 0 then Color (col ink) (Pictures [ circleAt (gX g l) (gY g v) 7 | (l, v) <- zip ls sim, isFin v ]) else Blank ]
    ty = 110 + ph + 60
    lastQ = lastOr 0
    ratio = if wNets w > 0 then lastQ (wSimQ w) / lastQ (wThQ w) else 0 / 0

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char '1') Down _ _ -> resetSim w { wAct = Tanh }
  EventKey (Char '2') Down _ _ -> resetSim w { wAct = Relu }
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim w { wSw2 = min (length sw2List - 1) (wSw2 w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim w { wSw2 = max 0 (wSw2 w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> resetSim w { wSw2 = 1 }
    | c `elem` "hH" -> resetSim w { wSw2 = 3 }
    | c `elem` "rR" -> resetSim w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ReLU で σ_w² = 1、2、3 をくらべる。20 層で信号は何倍になる？（理論：(σ_w²/2)^19 × σ_w²）
-- 2. tanh で σ_w² = 0.5、1、2、4。信号が 0 に行くか、ある値に落ち着くか
-- 3. widthN を 16 にすると、細い線（1 個のネット）のガタガタはどうなる？ 理論との平均のずれは？
-- 4. depth を 50 にして、ヒーの初期化でも勾配が保たれるか見る

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("数値積分：E[ξ²] = 1、E[ReLU(ξ)²] = 1/2、E[ReLU'(ξ)²] = 1/2"
    , near (gaussE (\z -> z * z) 1) 1 1e-9 && near (gaussE (\z -> let f = actF Relu z in f * f) 1) 0.5 1e-9 && near (gaussE (\z -> let d = actD Relu z in d * d) 1) 0.5 1e-9)
  , ("ReLU の漸化式 = 式 q_l = σ_w² (σ_w²/2)^(l−1)（σ_w² = 3）、ヒーなら理論の勾配はどの層も 1", closed && all (\v -> near v 1 1e-9) (snd thHe))
  , ("tanh（σ_w² = 2）：本物のネット 25 個の平均 = 漸化式（全部の層でずれ " ++ fmtN (100 * worst) 1 ++ "% < 10%）", worst < 0.1)
  , ("ヒーの初期化（ReLU、σ_w² = 2）：20 層まで信号が 1/2〜2 倍に保たれ、勾配も " ++ fmtN (headOr 0 heG) 2 ++ " 倍", okHe && headOr 0 heG > 0.5 && headOr 0 heG < 2)
  , ("ReLU σ_w² = 0.5 で消える（q_20/q_1 = " ++ fmtE (rat loQ) ++ "）、4 で爆発（" ++ fmtE (rat hiQ) ++ "）", rat loQ < 1e-8 && rat hiQ > 1e4) ]
  where
    (trq, _) = theoryCurves Relu 3 20
    closed = and [ near (at' trq l 0 / (3 * 1.5 ** i2d (l - 1))) 1 1e-5 | l <- [1 .. 20] ]
    thHe = theoryCurves Relu 2 20
    avg act sw2 k n = do
      rs <- mapM (const (simulateNet act sw2 n 20 4)) [1 .. k :: Int]
      let add sel = foldl' (\acc r -> zipWith (\a v -> a + v / i2d k) acc (sel r)) (replicate 21 0) rs
      pure (add fst, add snd)
    th2 = fst (theoryCurves Tanh 2 20)
    (s2q, _) = withSeed 701 (avg Tanh 2 25 64)
    worst = foldl' max 0 [ abs (at' s2q l 0 / at' th2 l 1 - 1) | l <- [1 .. 20] ]
    (heQ, heG) = withSeed 702 (avg Relu 2 20 64)
    okHe = and [ let r = at' heQ l 0 / at' heQ 1 1 in r > 0.5 && r < 2 | l <- [1 .. 20] ]
    ((loQ, _), (hiQ, _)) = withSeed 703 ((,) <$> avg Relu 0.5 3 48 <*> avg Relu 4 3 48)
    rat q = at' q 20 0 / at' q 1 1

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

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

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
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

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

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

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 活性化関数とその微分
data Act = Step | Sigmoid | Tanh | Relu | Softplus deriving (Eq, Show)

actNames :: [Act]
actNames = [Step, Sigmoid, Tanh, Relu, Softplus]

actF :: Act -> Double -> Double
actF a z = case a of
  Step -> if z >= 0 then 1 else 0
  Sigmoid -> sigm z
  Tanh -> tanh z
  Relu -> if z > 0 then z else 0
  Softplus -> if z > 30 then z else log (1 + exp z)   -- softplus = log(1 + eᶻ)

-- 微分 f'(z)（ステップは 0、ReLU の 0 での微分は 0 とする）
actD :: Act -> Double -> Double
actD a z = case a of
  Step -> 0
  Sigmoid -> let s = sigm z in s * (1 - s)
  Tanh -> let t = tanh z in 1 - t * t
  Relu -> if z > 0 then 1 else 0
  Softplus -> sigm z                          -- softplus の微分 = シグモイド

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

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
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
