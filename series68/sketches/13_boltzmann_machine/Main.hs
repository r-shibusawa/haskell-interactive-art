-- ============================================
-- 68-13 ボルツマンマシン ─ 見える 9 個（3 × 3 の画素）と隠れ 3 個の、0/1 のニューロン。同じ層どうしはつながない（制限ボルツマンマシン）
-- エネルギー E(v, h) = −a·v − b·h − vᵀ W h、確率 p(v, h) = e^(−E) / Z。Z = 全部の状態の e^(−E) の合計（分配関数）
-- 状態は 2^12 = 4096 通りしかないので、Z も確率も対数尤度もその勾配も、全部数えて「正確に」計算できる
--   正確な勾配 = ⟨v h⟩_データ − ⟨v h⟩_モデル。ふつうは後ろの項が計算できないので、
--   コントラスティブ・ダイバージェンス（CD-1）：データから 1 回だけギブスサンプリングした v′ で代わりにする
-- 下の「夢」：モデルから v → h → v → … とギブスサンプリング（1 変数ずつの条件付き分布は series65/12、0/1 の相互作用はイジング模型 series55/11）
-- 操作：スペース 一時停止 / ↑↓ 学習率 / R 初期値を変えてやり直す / F 夢の速さ
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

nv, nhid :: Int
nv = 9                     -- 見える・隠れの数（★ nv + nhid ≤ 12 なら全部数えられる）
nhid = 3

dataPats :: [String]
dataPats = ["111000000", "000111000", "000000111", "100100100", "010010010", "001001001"]   -- ★ しま模様（横 3 本・縦 3 本）

lrs :: [Double]
lrs = [0.02, 0.05, 0.1, 0.2]

-- 制限ボルツマンマシン：w（i 行 j 列 = 見える i と隠れ j）、a（見える層のバイアス）、b（隠れ層のバイアス）
data RBM = RBM { rbW :: !Vec, rbA :: !Vec, rbB :: !Vec }

-- 勾配（RBM と同じ形）
type Grad = RBM

data World = World
  { wLr      :: !Int
  , wCD      :: !RBM              -- CD-1 で学ぶ
  , wEx      :: !RBM              -- 正確な勾配で学ぶ
  , wLLCD    :: ![Double]         -- 対数尤度の記録（新しい順）
  , wLLEx    :: ![Double]
  , wEpochs  :: !Int
  , wSeed    :: !Int
  , wPaused  :: !Bool
  , wDreamV  :: !Vec
  , wDreams  :: ![Vec]            -- 夢（新しい順、40 個まで）
  , wDreamSpd :: !Int
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-13 Boltzmann machine" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restartRBM (World 3 e e [] [] 0 1 False U.empty [] 1 (rngOf 1) [])
  where e = RBM U.empty U.empty U.empty

dataV :: [Vec]
dataV = [ U.fromList [ if c == '1' then 1 else 0 | c <- s ] | s <- dataPats ]

bitsOf :: Int -> Int -> Vec
bitsOf k n = U.generate n (\i -> if testBit k i then 1 else 0)

-- 見える層の 512 通り（先に作っておく）
allV :: V.Vector Vec
allV = V.generate (2 ^ nv) (`bitsOf` nv)

makeRBM :: Int -> R RBM
makeRBM seed = do
  seedRng seed
  w <- U.map (0.1 *) <$> randnVec (nv * nhid)
  pure (RBM w (U.replicate nv 0) (U.replicate nhid 0))

wAt :: RBM -> Int -> Int -> Double
wAt m i j = U.unsafeIndex (rbW m) (i * nhid + j)

energyVH :: RBM -> Vec -> Vec -> Double
energyVH m v h = e2
  where e1 = foldl' (\e i -> e - (rbA m U.! i) * (v U.! i)) 0 [0 .. nv - 1]
        e2 = foldl' (\e j -> foldl' (\e' i -> e' - (v U.! i) * wAt m i j * (h U.! j)) (e - (rbB m U.! j) * (h U.! j)) [0 .. nv - 1]) e1 [0 .. nhid - 1]

hidInput :: RBM -> Vec -> Int -> Double
hidInput m v j = go 0 (U.unsafeIndex (rbB m) j)
  where go !i !s = if i >= nv then s else go (i + 1) (s + U.unsafeIndex v i * wAt m i j)

visInput :: RBM -> Vec -> Int -> Double
visInput m h i = go 0 (U.unsafeIndex (rbA m) i)
  where go !j !s = if j >= nhid then s else go (j + 1) (s + wAt m i j * U.unsafeIndex h j)

-- ★ 自由エネルギー F(v) = −a·v − Σ_j log(1 + e^(b_j + v·W_j))（h を全部足し合わせた結果。p(v) = e^(−F(v)) / Z）
freeEnergy :: RBM -> Vec -> Double
freeEnergy m v = go 0 f0
  where f0 = goA 0 0
        goA !i !f = if i >= nv then f else goA (i + 1) (f - U.unsafeIndex (rbA m) i * U.unsafeIndex v i)
        go !j !f = if j >= nhid then f else let x = hidInput m v j in go (j + 1) (f - (if x > 30 then x else log (1 + exp x)))

-- 見える層の 512 通り全部の確率（正確）と log Z
exactPv :: RBM -> (Vec, Double)
exactPv m = (U.map (\l -> exp (l - mx) / z) lp, mx + log z)
  where lp = U.generate (V.length allV) (\k -> negate (freeEnergy m (allV V.! k)))
        mx = U.foldl' max negInf lp
        z = U.foldl' (\s l -> s + exp (l - mx)) 0 lp

exactLL :: RBM -> [Vec] -> Double
exactLL m dat = let lz = snd (exactPv m) in foldl' (\s v -> s + (negate (freeEnergy m v) - lz)) 0 dat / i2d (length dat)

-- 勾配をためる道具：（v, 重み）の列から ⟨v_i p(h_j|v)⟩ を
accumGrad :: RBM -> [(Vec, Double)] -> Grad
accumGrad m items = runST $ do
  gw <- MU.replicate (nv * nhid) 0
  ga <- MU.replicate nv 0
  gb <- MU.replicate nhid 0
  forM_ items $ \(v, wt) -> do
    let ph = U.generate nhid (\j -> sigm (hidInput m v j))
    forM_ [0 .. nv - 1] $ \i -> do
      let vi = v U.! i
      MU.unsafeModify ga (+ wt * vi) i
      forM_ [0 .. nhid - 1] $ \j -> MU.unsafeModify gw (+ wt * vi * (ph U.! j)) (i * nhid + j)
    forM_ [0 .. nhid - 1] $ \j -> MU.unsafeModify gb (+ wt * (ph U.! j)) j
  RBM <$> U.freeze gw <*> U.freeze ga <*> U.freeze gb

-- ★ 正確な勾配：⟨v_i p(h_j|v)⟩_データ − Σ_v p(v) v_i p(h_j|v)
exactGrad :: RBM -> [Vec] -> Grad
exactGrad m dat = accumGrad m ([ (v, 1 / i2d (length dat)) | v <- dat ] ++ [ (allV V.! k, negate (pv U.! k)) | k <- [0 .. V.length allV - 1] ])
  where pv = fst (exactPv m)

-- ★ CD-1：v0 → h0（サンプル）→ v1（サンプル）→ p(h|v1)。勾配の代わりに v0 p(h|v0) − v1 p(h|v1)
cdGrad :: RBM -> [Vec] -> R Grad
cdGrad m dat = do
  samples <- mapM (\v0 -> do
      us <- rndVec (nhid + nv)
      let ph0 = U.generate nhid (\j -> sigm (hidInput m v0 j))
          h0 = U.generate nhid (\j -> if us U.! j < ph0 U.! j then 1 else 0)
          v1 = U.generate nv (\i -> if us U.! (nhid + i) < sigm (visInput m h0 i) then 1 else 0)
          ph1 = U.generate nhid (\j -> sigm (hidInput m v1 j))
      pure (v0, ph0, v1, ph1)) dat
  let n = i2d (length dat)
  pure $ runST $ do
    gw <- MU.replicate (nv * nhid) 0
    ga <- MU.replicate nv 0
    gb <- MU.replicate nhid 0
    forM_ samples $ \(v0, ph0, v1, ph1) -> do
      forM_ [0 .. nv - 1] $ \i -> do
        MU.unsafeModify ga (+ (v0 U.! i - v1 U.! i) / n) i
        forM_ [0 .. nhid - 1] $ \j -> MU.unsafeModify gw (+ ((v0 U.! i) * (ph0 U.! j) - (v1 U.! i) * (ph1 U.! j)) / n) (i * nhid + j)
      forM_ [0 .. nhid - 1] $ \j -> MU.unsafeModify gb (+ (ph0 U.! j - ph1 U.! j) / n) j
    RBM <$> U.freeze gw <*> U.freeze ga <*> U.freeze gb

ascend :: Double -> RBM -> Grad -> RBM
ascend lr m g = RBM (up (rbW m) (rbW g)) (up (rbA m) (rbA g)) (up (rbB m) (rbB g))
  where up = U.zipWith (\p q -> clampv (p + lr * q) (-20) 20)

restartRBM :: World -> World
restartRBM w =
  let cd = withSeed 1 (makeRBM (130 + wSeed w))
      ll = exactLL cd dataV
  in w { wCD = cd, wEx = cd, wLLCD = [ll], wLLEx = [ll], wEpochs = 0, wRng = rngOf (131 + wSeed w)
       , wDreamV = headOr U.empty dataV, wDreams = [] }

gibbsDream :: RBM -> Vec -> R Vec
gibbsDream m v = do
  us <- rndVec nhid
  let h = U.generate nhid (\j -> if us U.! j < sigm (hidInput m v j) then 1 else 0)
  ws <- rndVec nv
  pure (U.generate nv (\i -> if ws U.! i < sigm (visInput m h i) then 1 else 0))

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w2
  where
    lr = at' lrs (wLr w0) 0.1
    w1 | not (wPaused w0) && wEpochs w0 < 8000 =
           let one (c, e) _ = do
                 gr <- cdGrad c dataV
                 pure (ascend lr c gr, ascend lr e (exactGrad e dataV))
               ((cd, ex), g) = runR (foldM one (wCD w0, wEx w0) [1 .. 8 :: Int]) (wRng w0)
           in w0 { wCD = cd, wEx = ex, wRng = g, wEpochs = wEpochs w0 + 8
                 , wLLCD = exactLL cd dataV : wLLCD w0, wLLEx = exactLL ex dataV : wLLEx w0 }
       | otherwise = w0
    w2 = if envFrame env `mod` at' [12, 4, 1] (wDreamSpd w1) 1 == 0
           then let (v, g) = runR (gibbsDream (wCD w1) (wDreamV w1)) (wRng w1)
                in w1 { wDreamV = v, wDreams = take 40 (v : wDreams w1), wRng = g }
           else w1

-- 3 × 3 の小さな絵（div = 重みを −1〜1 の色で）
drawPix :: Vec -> Float -> Float -> Float -> Bool -> Picture
drawPix v x y cs dv = Pictures
  [ Color (col (if dv then divCol (v U.! i) else if v U.! i > 0.5 then ink else (26, 48, 50))) (rectTL (x + i2f (i `mod` 3) * cs) (y + i2f (i `div` 3) * cs) (cs - 1) (cs - 1))
  | i <- [0 .. min 8 (U.length v - 1)] ]

idxOf :: Vec -> Int                           -- 0/1 の列 → 番号（ビット i = v_i）
idxOf v = U.ifoldl' (\s i b -> if b > 0.5 then s + 2 ^ i else s) 0 v

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("RBM (9 visible, 3 hidden): " ++ show (wEpochs w) ++ " updates   log-likelihood  CD-1 " ++ fmtN (headOr 0 (wLLCD w)) 3 ++ " / exact gradient " ++ fmtN (headOr 0 (wLLEx w)) 3)
             ("Space " ++ (if wPaused w then "resume" else "pause") ++ "   Up/Down learning rate " ++ jsNum (at' lrs (wLr w) 0) ++ "   R new initial weights   F dream speed   |   Z = sum e^(-E) over all 2^12 = 4096 states")
  -- ---- 左：データとフィルタ
  , say "data to learn (6 stripes)" x0 (y0 - 4) 14 (col ink)
  , Pictures [ drawPix v (x0 + i2f k * (3 * 16 + 12)) (y0 + 6) 16 False | (k, v) <- zip [0 :: Int ..] dataV ]
  , say "weights W of the 3 hidden neurons (magenta = +, lime = -)" x0 (y0 + 96) 14 (col ink)
  , Pictures [ let c = U.generate nv (\i -> wAt (wCD w) i j)
                   mx = U.foldl' (\a v -> max a (abs v)) 0.5 c
               in drawPix (U.map (/ mx) c) (x0 + i2f j * (3 * 22 + 16)) (y0 + 106) 22 True | j <- [0 .. nhid - 1] ]
  -- ---- 中：確率の高い見える状態
  , say ("most probable visible states p(v) (exact)   the 6 data patterns together = " ++ fmtN (100 * onData) 1 ++ "%") x0 (myY - 6) 14 (col ink)
  , Pictures [ let v = allV V.! k
                   isData = U.toList v `elem` map U.toList dataV
                   x = x0 + i2f q * 58
                   bh = 80 * f2 (p / mx2)
               in Pictures [ drawPix v x (myY + 4) 14 False
                           , Color (col (if isData then lime else mag)) (rectTL x (myY + 150 - bh) 42 bh)
                           , say (fmtN (100 * p) 1 ++ "%") x (myY + 168) 14 (col sub) ]
             | (q, (p, k)) <- zip [0 :: Int ..] top ]
  , say ("lime = pattern in the data, magenta = not in the data (all probabilities sum to " ++ fmtN (U.sum pEx) 6 ++ ")") x0 (myY + 190) 14 (col sub)
  -- ---- 下：夢（ギブスサンプリング）
  , say "dreams: Gibbs sampling from the model v -> h -> v -> ... (last 40, oldest first)" x0 dy 14 (col ink)
  , Pictures [ drawPix v (x0 + i2f (q `mod` 20) * 38) (dy + 10 + i2f (q `div` 20) * 40) 10 False | (q, v) <- zip [0 :: Int ..] (reverse (wDreams w)) ]
  -- ---- 右：対数尤度
  , plotAxes g "updates (x8)" "exact log-likelihood (log p(v) per data pattern)" [0, grX1 g] [-7, -5, -3, -1]
  , Color (colA 160 amber) (Line [(grX g, gY g (log (1 / 6))), (grX g + grW g, gY g (log (1 / 6)))])
  , plotXY g (map i2d [0 .. length llEx - 1]) llEx (col lime) 2.5
  , plotXY g (map i2d [0 .. length llCD - 1]) llCD (col mag) 2
  , legend (grX g + 12) (grY g + grH g - 46) [(lime, "exact gradient (count all)"), (mag, "CD-1 (one Gibbs step)"), (amber, "ideal log(1/6)")]
  , Pictures [ say s (grX g) (ty + 20 * i2f q) 14 (col sub)
             | (q, s) <- zip [0 :: Int ..] [ "CD-1 is not the exact gradient (a sample: jagged,", "not always up). Run long and it may even go down."
                                         , "With many states Z cannot be counted -> people use CD.", "ideal = 1/6 for each of the 6 -> log(1/6) = " ++ fmtN (log (1 / 6)) 3
                                         , "3 hidden -> h has 8 states, enough to tell the 6 apart." ] ]
  , drawSelfTest env (wTest w) ]
  where
    x0 = 40
    y0 = 104
    myY = y0 + 220
    dy = myY + 224
    (pEx, _) = exactPv (wCD w)
    onData = sumOf [ pEx U.! idxOf v | v <- dataV ]
    top = take 12 (sortBy (\(a, _) (b, _) -> compare b a) (zip (U.toList pEx) [0 :: Int ..]))
    mx2 = fst (headOr (1, 0) top)
    llCD = reverse (wLLCD w)
    llEx = reverse (wLLEx w)
    rx = min (envW env * 0.62) 760
    g = Graph (rx + 30) 110 (envW env - rx - 80) ((envH env - 250) * 0.6) 0 (max 10 (i2d (length llCD - 1))) (-7) (-1)
    ty = grY g + grH g + 50

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> restartRBM w { wLr = min (length lrs - 1) (wLr w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restartRBM w { wLr = max 0 (wLr w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> restartRBM w { wSeed = wSeed w + 1 }
    | c `elem` "fF" -> w { wDreamSpd = (wDreamSpd w + 1) `mod` 3 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 学習が進むと、夢の模様はしま模様ばかりになる？ データにない模様はどれくらい出る？
-- 2. dataPats を自分の模様（例：対角線 "100010001"）に変えて学ばせる
-- 3. nhid を 1 や 2 にすると、データの 6 枚の合計の確率はどこまで上がる？（nv + nhid ≤ 12 で）
-- 4. 学習率を ↑↓ で変えて、CD-1 と正確な勾配の差を見る

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("Z を 4096 通り全部で数えた値 = 自由エネルギーで h を消した値（log Z = " ++ fmtN logZ 4 ++ "）", near (log zFull) logZ 1e-10)
  , ("見える層の 512 通りの確率の合計 = 1", near (U.foldl' (+) 0 pv) 1 1e-12)
  , ("p(h_j = 1 | v) = σ(b_j + Σ v_i W_ij)（全部数えた条件付き確率と一致）", cond)
  , ("正確な勾配 ⟨vh⟩_データ − ⟨vh⟩_モデル = 対数尤度の差分（相対誤差 " ++ fmtE worst ++ "）", worst < 1e-5)
  , ("正確な勾配で小さな学習率なら、対数尤度は毎回上がる（" ++ fmtN l0 3 ++ " → " ++ fmtN lEnd 3 ++ "）", mono && lEnd > l0)
  , ("CD-1 で 600 回：学習の終わりの正確な対数尤度が最初より高い（" ++ fmtN c0 3 ++ " → " ++ fmtN c1 3 ++ "）", c1 > c0 + 1) ]
  where
    m = withSeed 1 $ do
      m0 <- makeRBM 77
      seedRng 78
      a <- mapM (const ((0.3 *) <$> randn)) [1 .. nv]
      b <- mapM (const ((0.3 *) <$> randn)) [1 .. nhid]
      w <- randnVec (nv * nhid)
      pure m0 { rbA = U.fromList a, rbB = U.fromList b, rbW = w }
    zFull = foldl' (\s k -> foldl' (\s' q -> s' + exp (negate (energyVH m (bitsOf k nv) (bitsOf q nhid)))) s [0 .. 2 ^ nhid - 1]) 0 [0 .. 2 ^ nv - 1 :: Int]
    (pv, logZ) = exactPv m
    v3 = at' dataV 3 U.empty
    cond = and [ let (num, den) = foldl' (\(nu, de) q -> let h = bitsOf q nhid
                                                             e = exp (negate (energyVH m v3 h))
                                                         in (if h U.! j > 0 then nu + e else nu, de + e)) (0, 0) [0 .. 2 ^ nhid - 1 :: Int]
                 in near (num / den) (sigm (hidInput m v3 j)) 1e-12 | j <- [0 .. nhid - 1] ]
    g = exactGrad m dataV
    hh = 1e-6
    bumpW i j d r = r { rbW = rbW r U.// [(i * nhid + j, rbW r U.! (i * nhid + j) + d)] }
    fdW i j = (exactLL (bumpW i j hh m) dataV - exactLL (bumpW i j (-hh) m) dataV) / (2 * hh)
    bumpA i d r = r { rbA = rbA r U.// [(i, rbA r U.! i + d)] }
    fdA i = (exactLL (bumpA i hh m) dataV - exactLL (bumpA i (-hh) m) dataV) / (2 * hh)
    worst = foldl' max 0 ([ relErr (wAt g i j) (fdW i j) | (i, j) <- [(0, 0), (4, 1), (8, 2)] ] ++ [relErr (rbA g U.! 2) (fdA 2)])
    me = withSeed 1 (makeRBM 140)
    l0 = exactLL me dataV
    (mono, lEnd) = let go t r prev ok = if t >= (40 :: Int) then (ok, prev) else
                         let r' = ascend 0.05 r (exactGrad r dataV)
                             l = exactLL r' dataV
                         in go (t + 1) r' l (ok && not (l < prev - 1e-12))
                   in go 0 me l0 True
    mc = withSeed 1 (makeRBM 141)
    c0 = exactLL mc dataV
    c1 = exactLL (withSeed 142 (foldM (\r _ -> ascend 0.1 r <$> cdGrad r dataV) mc [1 .. 600 :: Int])) dataV

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

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

-- ---- 画像の道具：幅 iw × 高さ ih の画像の全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((i2d px + 0.5) / i2d iw) (1 - (i2d py + 0.5) / i2d ih)
  in (w8 r, w8 g, w8 b))

-- マス目の画像：fn i j（i = 左から、j = 上から）→ (r, g, b)
gridImage :: Int -> Int -> (Int -> Int -> RGB) -> Picture
gridImage iw ih fn = pixelImage iw ih 1 (\i j -> let (r, g, b) = fn i j in (w8 r, w8 g, w8 b))

w8 :: Double -> Word8                         -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）
w8 v = fromIntegral (c8 v)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / i2f iw) (negate h / i2f ih) pic)

mixCol :: RGB -> RGB -> Double -> RGB
mixCol (a0, a1, a2) (b0, b1, b2) t0 = let t = clampv t0 0 1 in (a0 + (b0 - a0) * t, a1 + (b1 - a1) * t, a2 + (b2 - a2) * t)

divCol :: Double -> RGB                       -- −1〜1 → ライム・暗い・マゼンタ
divCol v = if v >= 0 then mixCol panel mag v else mixCol panel lime (negate v)

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
