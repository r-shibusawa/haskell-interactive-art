-- ============================================
-- 61-08 混合ガウス分布と EM ─ データを「k 個の正規分布（楕円の山）を重みつきで足したもの」とみなしてあてはめる
--   E ステップ：各点がどの山から来たかの確率（所属確率 r_ik、合計 1）を計算 → 点の色の混ざりぐあい
--   M ステップ：所属確率を重みにして、各山の重み・平均・共分散（楕円の形と向き）を計算しなおす
--   対数尤度 log L は E → M のたびに必ず増えるか同じ（EM アルゴリズムの性質、右上のグラフ）。k は BIC = −2 log L + (母数の数)·log n が小さいものを選ぶ
--   k-means は「丸くて同じ大きさ・どれか 1 つに所属」の特別な場合。所属度で分けるファジィ c-平均法はシリーズ 50 の 11
-- 操作：スペース EM を 1 回 / A 自動 / ↑↓ k / I 初期値を変える / D データ / R 新しいデータ / クリック 点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 300                                  -- ★ 点の数

dKinds :: [String]
dKinds = ["ellipses", "aniso", "sizes", "moons"]

-- BIC を少しずつ計算する仕事：(k, 何回目のやり直しか, 乱数の状態, いままででいちばんよいあてはめ, 途中の EM)
data BicJob = BicJob !Int !Int !Rng !(Maybe GFit) !(Maybe FitRun)

-- 途中の EM（gmmFit を何フレームかに分けたもの）：回数 t、山、所属確率、log L、log L の記録（新しい順）
data FitRun = FitRun !Int !GMM !(V.Vector (U.Vector Double)) !Double ![Double]

emPerFrame :: Int
emPerFrame = 4                             -- 1 フレームに進める EM の回数（重くならないように）

-- gmmFit と同じ計算を n 回ぶんだけ進める（終わったら Left 結果）
advanceFit :: Pts -> Int -> Int -> FitRun -> Either GFit FitRun
advanceFit pts iters n run@(FitRun t gm resp ll hist)
  | t >= iters = Left (GFit gm ll resp (reverse (ll : hist)))
  | n <= 0 = Right run
  | otherwise =
      let gm' = gmmM pts resp (Just gm)
          (resp', ll') = gmmE pts gm'
      in if t > 3 && abs (ll' - ll) < 1e-9 * abs ll'
           then Left (GFit gm' ll' resp' (reverse (ll' : ll : hist)))
           else advanceFit pts iters (n - 1) (FitRun (t + 1) gm' resp' ll' (ll : hist))

data World = World
  { wData   :: !DS
  , wK      :: !Int                         -- ★ 山の数 k
  , wGm     :: !GMM
  , wResp   :: !(V.Vector (U.Vector Double))   -- 所属確率
  , wLL     :: !Double                      -- 対数尤度
  , wHist   :: ![Double]                    -- 対数尤度の記録
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wDataNo :: !Int
  , wSeedD  :: !Int
  , wSeedI  :: !Int
  , wBic    :: ![Double]                    -- k = 1, 2, … の BIC
  , wJob    :: !(Maybe BicJob)
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-08 Gaussian mixture and EM" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      wLL w `seq` pure w
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = newData (World (DS V.empty U.empty 0) 3 (GMM [] [] []) V.empty 0 [] True 0 0 81 1 [] Nothing [])

ellipses3 :: Int -> Word32 -> R DS          -- 共分散をきめた 3 つの楕円のかたまり
ellipses3 n seed = do
  seedR seed
  let a = floor (fromIntegral n * 0.4 :: Double)
      b = floor (fromIntegral n * 0.35 :: Double)
  makeBlobs [Blob a (-0.9, 0.5) (0.6, 0.15) 0.6, Blob b (1.0, 0.45) (0.45, 0.22) (-0.7), Blob (n - a - b) (0.05, -0.85) (0.22, 0.22) 0]

newData :: World -> World
newData w = reinit w { wData = d, wBic = [], wJob = Nothing }
  where
    kd = headOr "ellipses" (drop (wDataNo w) dKinds)
    sd = fromIntegral (wSeedD w)
    d = withSeed 1 (if kd == "ellipses" then ellipses3 nPts sd else makeDataset kd nPts sd)

reinit :: World -> World
reinit w = let gm = withSeed (fromIntegral (900 + wSeedI w)) (gmmInit (dsPts (wData w)) (wK w))
               (resp, ll) = gmmE (dsPts (wData w)) gm
           in w { wGm = gm, wResp = resp, wLL = ll, wHist = [ll] }

-- ★ EM を 1 回：M（今の所属確率で山を計算しなおす）→ E（新しい山で所属確率と log L）
emStep :: World -> World
emStep w = w { wGm = gm, wResp = resp, wLL = ll, wHist = trim (wHist w ++ [ll]) }
  where
    pts = dsPts (wData w)
    gm = gmmM pts (wResp w) (Just (wGm w))
    (resp, ll) = gmmE pts gm
    trim hs = if length hs > 300 then take 1 hs ++ drop 2 hs else hs

-- tries 回やり直して log L がいちばん大きいあてはめ（種 seed から）
bestFit :: Pts -> Int -> Int -> Word32 -> GFit
bestFit pts k tries seed = withSeed seed $ do
  fs <- mapM (const (gmmFit pts k 150)) [1 .. max 1 tries]
  pure (case fs of
          (f0 : rest) -> foldl' (\b f -> if gfLL f > gfLL b then f else b) f0 rest
          [] -> GFit (GMM [] [] []) 0 V.empty [])

-- BIC は重いので少しずつ（p5.js 版は 3 フレームに k を 1 つ。ここでは 1 フレームに EM を emPerFrame 回まで）
bicTick :: World -> World
bicTick w
  | length (wBic w) >= 6 = w
  | otherwise =
      let k = length (wBic w) + 1
          pts = dsPts (wData w)
          BicJob _ t rng best run = case wJob w of
            Just j@(BicJob jk _ _ _ _) | jk == k -> j
            _ -> BicJob k 0 (seedRng (fromIntegral (50 + k))) Nothing Nothing
      in case run of
           Nothing ->                         -- 新しいあてはめを始める（gmmFit の最初と同じ）
             let (gm0, rng') = runR (gmmInit pts k) rng
                 (r0, l0) = gmmE pts gm0
             in l0 `seq` w { wJob = Just (BicJob k t rng' best (Just (FitRun 0 gm0 r0 l0 []))) }
           Just fr -> case advanceFit pts 150 emPerFrame fr of
             Right fr' -> w { wJob = Just (BicJob k t rng best (Just fr')) }
             Left f ->
               let best' = case best of
                     Just b | gfLL f <= gfLL b -> b
                     _ -> f
               in if t + 1 >= 3
                    then w { wBic = wBic w ++ [gmmBIC (gfLL best') k (V.length pts)], wJob = Nothing }
                    else w { wJob = Just (BicJob k (t + 1) rng (Just best') Nothing) }

step :: Env -> World -> World
step _ w0 = bicTick w1
  where
    t = wTimer w0 + (if wAuto w0 then 1 else 0)
    w1 = if wAuto w0 && t `mod` 8 == 0 then emStep w0 { wTimer = t } else w0 { wTimer = t }

argmax :: U.Vector Double -> Int              -- いちばん大きい値の番号（同じなら前のほう）
argmax r = fst (U.ifoldl' (\(bi, bv) i x -> if x > bv then (i, x) else (bi, bv)) (0, -1 / 0) r)

mainView :: Env -> G
mainView env = fitView 40 100 (envW env * 0.58) (envH env - 190) 0 0 4.6

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) ]
  -- ---- 点：所属確率で色を混ぜる
  ++ [ Color (mix r) (circleAt (gX v (ptX p)) (gY v (ptY p)) 7) | (p, r) <- zip (V.toList pts) (V.toList (wResp w)) ]
  -- ---- 山（楕円 = 1σ と 2σ）
  ++ concat [ [ Color (palCA k (if s == 1 then 230 else 120)) (polyline (if s == 1 then 2.5 else 1.5) (ellipse m (covAxes cv) s)) | s <- [1, 2] ]
              ++ [ sayC ("w = " ++ fmt 2 wk) (gX v (fst m)) (gY v (snd m) - 6) 14 (palC k) ]
            | (k, wk, m, cv) <- zip4' [0 ..] (gW gm) (gMu gm) (gCv gm) ]
  -- ---- 対数尤度
  ++ [ plotAxes g1 "EM steps" "log-likelihood log L (never decreases)" [0, x1 g1] [fromIntegral (jsRoundI (lo - 1)), fromIntegral (jsRoundI (hi + 1))]
     , Color (rgbC 110 230 190) (polyline 2 [ (gX g1 (fromIntegral i), gY g1 l) | (i, l) <- zip [0 :: Int ..] (wHist w) ]) ]
  -- ---- BIC
  ++ [ say "BIC (smaller is better; best of 3 tries per k)" gx (400 - 10) 14 (grayC 205) ]
  ++ (if null (wBic w) then [] else
        concat [ [ Color (if i + 1 == best then rgbC 255 190 80 else if i + 1 == wK w then rgbC 176 140 255 else rgbC 90 95 120) (rectTL (gx + fromIntegral i * bw + 4) (400 + 160 - hgt) (bw - 8) hgt)
                 , sayC ("k=" ++ show (i + 1)) (gx + fromIntegral i * bw + bw / 2) (400 + 160 + 20) 14 (grayC 190) ]
               | (i, b) <- zip [0 :: Int ..] (wBic w), let hgt = 20 + f2 ((b - bl) / max 1e-9 (bh - bl)) * (160 - 30) ]
        ++ [ say (if length (wBic w) < 6 then "computing..." else "smallest BIC -> k = " ++ show best) gx (400 + 160 + 44) 14 (rgbC 255 200 120) ])
  -- ---- 文字
  ++ [ say ("Gaussian mixture + EM  k = " ++ show (wK w) ++ "  log L = " ++ fmt 2 (wLL w) ++ "  EM " ++ show (length (wHist w) - 1) ++ " steps  ARI (most likely) " ++ fmt 3 (ariScore lab (dsTruth (wData w)))) 40 40 18 (grayC 240)
     , say ("Space: 1 EM step   A auto" ++ (if wAuto w then " (on)" else "") ++ "   Up Down k   I new init   D data (" ++ dname ++ ")   R new data   click: add point") 40 66 15 (grayC 170)
     , sayWrap "Point colour = mix by membership probability (mixed = 'could be either'). Ellipses = 1 and 2 sigma. Elongated / different-size data that k-means (03) fails on are separated." 40 (envH env - 64) (pW v) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    v = mainView env
    pts = dsPts (wData w)
    gm = wGm w
    mix r = let (a, b, c) = U.ifoldl' (\(x, y, z) k p -> let (pr, pg, pb) = palRGB k in (x + p * fromIntegral pr, y + p * fromIntegral pg, z + p * fromIntegral pb)) (0, 0, 0) r
                q t = f2 (clampv 0 255 t) / 255
            in makeColor (q a) (q b) (q c) 1
    ellipse (mx, my) (s1, s2, ang) s = [ (gX v (mx + u * cos ang - vv * sin ang), gY v (my + u * sin ang + vv * cos ang))
                                       | t <- [0 .. 60 :: Int], let a = tau' * fromIntegral t / 60, let u = s * s1 * cos a, let vv = s * s2 * sin a ]
    tau' = 2 * pi :: Double
    gx = pX v + pW v + 60
    gw = envW env - gx - 40
    lo = foldl' min (1 / 0) (wHist w)
    hi = foldl' max (-1 / 0) (wHist w)
    g1 = G gx 130 gw 180 0 (fromIntegral (max 10 (length (wHist w) - 1))) (lo - 1) (hi + 1)
    bl = foldl' min (1 / 0) (wBic w)
    bh = foldl' max (-1 / 0) (wBic w)
    best = 1 + length (takeWhile (/= bl) (wBic w))
    bw = gw / 6
    lab = U.fromList (map argmax (V.toList (wResp w)))
    dname = let kd = headOr "" (drop (wDataNo w) dKinds) in if kd == "ellipses" then "3 ellipses" else dataName kd
    zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
    zip4' _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p0 ->
    let (mx, my) = toP5 env p0
        v = mainView env
    in if inBox v mx my
         then let d = appendDS (wData w) [(pt2 (invX v mx) (invY v my), 0)]
                  (resp, ll) = gmmE (dsPts d) (wGm w)
              in w { wData = d, wResp = resp, wLL = ll, wHist = [ll], wBic = [], wJob = Nothing }
         else w
  EventKey (SpecialKey KeySpace) Down _ _ -> emStep w { wAuto = False }
  EventKey (SpecialKey KeyUp) Down _ _ -> reinit w { wK = min 6 (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> reinit w { wK = max 1 (wK w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "iI" -> reinit w { wSeedI = wSeedI w + 1 }
    | c `elem` "dD" -> newData w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("log-likelihood never decreases in EM (5 initial values x 40 steps)", and (map fst runs))
  , ("memberships r_ik are in [0, 1] and sum to exactly 1 for every point", and (map snd runs))
  , ("log-sum-exp: log(e^-1000 + e^-1001) = -1000 + log(1 + e^-1) without NaN, and equals direct computation for ordinary values"
    , near (logSumExp [-1000, -1001]) (-1000 + log (1 + exp (-1))) 1e-12 && near (logSumExp [0.5, -0.3, 1.2]) (log (exp 0.5 + exp (-0.3) + exp 1.2)) 1e-12)
  , ("with one component the M step gives exactly the data mean and covariance (divided by n)"
    , near (fst mu1) mx 1e-12 && near (snd mu1) my 1e-12 && near ca sxx 1e-12 && near cb sxy 1e-12 && gW one == [1])
  , ("3-ellipse data: BIC is smallest at k = 3 (k = 1..5)", length (takeWhile (/= minimum bics) bics) == 2)
  , ("points on a line (collapsing covariance): covariance floor keeps log L finite, no NaN"
    , not (isInfinite (gfLL fl) || isNaN (gfLL fl)) && all (\(a, b, c) -> a * c - b * b > 0) (gCv (gfGm fl)) && noNaN (gfHist fl))
  , ("elongated blobs: Gaussian mixture ARI " ++ fmt 3 aG ++ " >= 0.9, k-means " ++ fmt 3 aK, aG >= 0.9 && aK < aG) ]
  where
    d = withSeed 1 (ellipses3 150 8)
    pts = dsPts d
    n = V.length pts
    runs = [ run s | s <- [0 .. 4] ]
    run s = let g0 = withSeed (30 + s) (gmmInit pts 3)
                (r0, l0) = gmmE pts g0
                go :: Int -> GMM -> V.Vector (U.Vector Double) -> Double -> (Bool, Bool)
                go t g r prev
                  | t >= 40 = (True, True)
                  | otherwise =
                      let g' = gmmM pts r (Just g)
                          (r', l') = gmmE pts g'
                          okM = not (l' < prev - 1e-9 * abs prev)
                          okS = V.all (\rr -> abs (U.foldl' (+) 0 rr - 1) < 1e-12 && U.all (\x -> x >= 0 && x <= 1) rr) r'
                          (a, b) = go (t + 1) g' r' l'
                      in (okM && a, okS && b)
            in go 0 g0 r0 l0
    one = gmmM pts (V.replicate n (U.singleton 1)) Nothing
    mu1 = headOr (0, 0) (gMu one)
    (ca, cb, _) = headOr (0, 0, 0) (gCv one)
    mx = meanOf (map ptX (V.toList pts))
    my = meanOf (map ptY (V.toList pts))
    sxx = meanOf [ (ptX p - mx) * (ptX p - mx) | p <- V.toList pts ]
    sxy = meanOf [ (ptX p - mx) * (ptY p - my) | p <- V.toList pts ]
    bics = [ gmmBIC (gfLL (bestFit pts k 1 (fromIntegral (70 + k)))) k n | k <- [1 .. 5] ]
    flat = V.fromList [ pt2 (fromIntegral i * 0.05) 0 | i <- [0 .. 39 :: Int] ]
    fl = withSeed 5 (gmmFit flat 2 50)
    an = withSeed 1 (makeDataset "aniso" 150 9)
    fa = bestFit (dsPts an) 3 2 4
    la = U.fromList (map argmax (V.toList (gfResp fa)))
    ka = withSeed 4 (kmeansBest (dsPts an) 3 10 True 100)
    aG = ariScore la (dsTruth an)
    aK = ariScore (kmLabels ka) (dsTruth an)

-- ---------- やってみよう ----------
-- 1. I で初期値を変えてスペースで 1 回ずつ。楕円がだんだんデータに合っていく様子と、log L のグラフを見よう
-- 2. ↑↓ で k を変えて BIC の棒を見る。log L は k を増やすほど大きくなるのに、BIC はなぜ k = 3 で最小？
-- 3. D で「細長い」「大きさがちがう」に。03 で k-means が失敗したデータを、楕円ならどう分ける？
-- 4. 三日月（D）では？ 正規分布の山は丸い（楕円）ので、三日月 1 つに山がいくつも必要になる
-- 5. 所属確率で色が混ざる点を探そう。シリーズ 50 の 11（ファジィ c-平均法）の所属度とくらべてみよう

-- ==================================================
-- ここから下は「クラスタリングの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点は U.Vector Double（[x, y] や [r, g, b]）、
-- 点の集まりは V.Vector（p5.js 版の配列の配列）、ラベルは U.Vector Int（−1 = ノイズ）
-- ==================================================
-- ---- 共通：クラスタの色・距離（点は [x, y] や [r, g, b] のような U.Vector Double）
type P = U.Vector Double
type Pts = V.Vector P

pt2 :: Double -> Double -> P                -- 2 次元の点 [x, y]
pt2 x y = U.fromList [x, y]

ptX, ptY :: P -> Double                     -- x 座標・y 座標（なければ 0）
ptX p = if U.length p > 0 then U.unsafeIndex p 0 else 0
ptY p = if U.length p > 1 then U.unsafeIndex p 1 else 0

palette :: [(Int, Int, Int)]
palette = [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130), (200, 240, 110), (255, 150, 220), (90, 210, 220), (255, 150, 70), (190, 190, 255)]

palRGB :: Int -> (Int, Int, Int)            -- クラスタ k の色（k < 0 = ノイズ = 灰色）
palRGB k | k < 0 = (120, 120, 135)
         | otherwise = headOr (0, 0, 0) (drop (k `mod` length palette) palette)

palC :: Int -> Color
palC k = palCA k 255

palCA :: Int -> Int -> Color                -- 不透明度 a（0〜255）つき
palCA k a = let (r, g, b) = palRGB k in rgbaC r g b a

sqd :: P -> P -> Double                     -- 距離の 2 乗
sqd a b = go 0 0
  where
    n = min (U.length a) (U.length b)
    go !d !s | d >= n = s
             | otherwise = let e = U.unsafeIndex a d - U.unsafeIndex b d in go (d + 1) (s + e * e)
{-# INLINE sqd #-}

edist :: P -> P -> Double                   -- ユークリッド距離
edist a b = sqrt (sqd a b)

meanOf :: [Double] -> Double
meanOf [] = 0
meanOf xs = foldl' (+) 0 xs / fromIntegral (length xs)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ。gauss の 2 つ目の値もとっておく）
data Rng = Rng !Word32 !(Maybe Double)

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s) Nothing

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s sp) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, Rng a sp)   -- Int を通すと速い

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

seedR :: Word32 -> R ()                     -- 計算の途中で seedRng(s)
seedR s = R (\_ -> ((), seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int               -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

gauss :: R Double                           -- 標準正規分布（ボックス-ミュラー法）
gauss = R $ \(Rng s sp) -> case sp of
  Just g -> (g, Rng s Nothing)
  Nothing ->
    let (u0, r1) = rndStep (Rng s Nothing)
        (v, Rng s2 _) = rndStep r1
        u = 1 - u0
        r = sqrt (-2 * log u)
    in (r * cos (2 * pi * v), Rng s2 (Just (r * sin (2 * pi * v))))

-- ---- 合成データ（種つき）。どれも DS { 点, 正解のグループ番号, 正解のグループ数 } を返す
-- blob = 点の数、中心 (x, y)、(長い向きの標準偏差, 短い向き)、傾き（ラジアン）
data Blob = Blob { bN :: !Int, bC :: !(Double, Double), bSd :: !(Double, Double), bRot :: !Double }
data DS = DS { dsPts :: !Pts, dsTruth :: !(U.Vector Int), dsK :: !Int }

makeBlobs :: [Blob] -> R DS
makeBlobs specs = do
  groups <- forM (zip [0 ..] specs) $ \(k, s) -> do
    let cr = cos (bRot s)
        sr = sin (bRot s)
        (cx, cy) = bC s
        (s0, s1) = bSd s
    replicateM (bN s) $ do
      a <- (* s0) <$> gauss
      b <- (* s1) <$> gauss
      pure (pt2 (cx + cr * a - sr * b) (cy + sr * a + cr * b), k)
  let ps = concat groups
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) (length specs))

makeMoons :: Int -> Double -> R DS          -- 2 つの三日月（かみ合った半円）
makeMoons n noise = do
  ps <- forM [0 .. n - 1] $ \i -> do
    t <- (* pi) <$> rnd
    let k = i `mod` 2
        x = if k == 1 then 1 - cos t else cos t
        y = if k == 1 then 0.5 - sin t else sin t
    gx <- gauss
    gy <- gauss
    pure (pt2 ((x - 0.5) * 1.6 + gx * noise) ((y - 0.25) * 1.6 + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

makeCircles :: Int -> Double -> Double -> R DS   -- 同心円（外の輪と内の輪。点の数は輪の長さに比例）
makeCircles n noise inner = do
  let nOut = jsRoundI (fromIntegral n * 1.3 / (1.3 + inner))
  ps <- forM [0 .. n - 1] $ \i -> do
    let k = if i < nOut then 0 else 1
        r = if k == 1 then inner else 1.3
    t <- (* (2 * pi)) <$> rnd
    gx <- gauss
    gy <- gauss
    pure (pt2 (r * cos t + gx * noise) (r * sin t + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

dataName :: String -> String                -- 画面に出すデータの名前
dataName kind = case kind of
  "blobs" -> "blobs"
  "moons" -> "moons"
  "circles" -> "circles"
  "aniso" -> "elongated"
  "density" -> "different densities"
  "sizes" -> "different sizes"
  "noise" -> "blobs + outliers"
  "chain" -> "chained"
  _ -> kind

appendDS :: DS -> [(P, Int)] -> DS          -- 点を後ろに足す
appendDS d extra = d { dsPts = dsPts d V.++ V.fromList (map fst extra), dsTruth = dsTruth d U.++ U.fromList (map snd extra) }

makeDataset :: String -> Int -> Word32 -> R DS
makeDataset kind n seed = do
  seedR seed
  case kind of
    "moons" -> makeMoons n 0.07
    "circles" -> makeCircles n 0.05 0.55
    "aniso" -> let m = n `div` 3 in makeBlobs [Blob m (-0.6, 0.9) (0.9, 0.1) 0.5, Blob m (0, 0) (0.9, 0.1) 0.5, Blob (n - 2 * m) (0.6, -0.9) (0.9, 0.1) 0.5]
    "density" -> let m = floor (fromIntegral n * 0.45 :: Double) in makeBlobs [Blob m (-0.9, 0.3) (0.12, 0.12) 0, Blob m (-0.3, 0.3) (0.12, 0.12) 0, Blob (n - 2 * m) (0.9, -0.2) (0.65, 0.65) 0]
    "sizes" -> let m = floor (fromIntegral n * 0.12 :: Double) in makeBlobs [Blob (n - 2 * m) (-0.5, 0) (0.6, 0.6) 0, Blob m (0.85, 0.75) (0.1, 0.1) 0, Blob m (0.85, -0.75) (0.1, 0.1) 0]
    "chain" -> do
      let m = floor (fromIntegral n * 0.4 :: Double)
          rest = n - 2 * m
      d <- makeBlobs [Blob m (-1.2, 0) (0.25, 0.25) 0, Blob m (1.2, 0) (0.25, 0.25) 0]
      extra <- forM [0 .. rest - 1] $ \i -> do
        g <- gauss
        pure (pt2 (-0.8 + 1.6 * (fromIntegral i + 0.5) / fromIntegral rest) (0.04 * g), if fromIntegral i < (fromIntegral rest / 2 :: Double) then 0 else 1)
      pure (appendDS d extra)
    _ -> do
      let m = n `div` 3
      d <- makeBlobs [Blob m (-1, 0.7) (0.25, 0.25) 0, Blob m (1, 0.6) (0.3, 0.22) 0.4, Blob (n - 2 * m) (0, -0.9) (0.28, 0.28) 0]
      if kind == "noise"
        then do
          let q = floor (fromIntegral n * 0.08 :: Double)
          extra <- replicateM q $ do
            x <- rnd
            y <- rnd
            pure (pt2 (-2 + 4 * x) (-1.8 + 3.6 * y), -1)
          pure (appendDS d extra)
        else pure d

-- ---- k-means（ロイドのアルゴリズム）。目的関数 = 慣性 = Σ |x_i − 中心|²
nearestCenter :: P -> Pts -> Int
nearestCenter p cs = go 0 0 (1 / 0)
  where
    k = V.length cs
    go !j !b !bd
      | j >= k = b
      | otherwise = let d = sqd p (V.unsafeIndex cs j) in if d < bd then go (j + 1) j d else go (j + 1) b bd

assignLabels :: Pts -> Pts -> U.Vector Int  -- ① 割り当て
assignLabels pts cs = U.generate (V.length pts) (\i -> nearestCenter (V.unsafeIndex pts i) cs)

inertiaOf :: Pts -> Pts -> U.Vector Int -> Double
inertiaOf pts cs labels = U.ifoldl' (\s i l -> s + sqd (pts V.! i) (cs V.! l)) 0 (U.take (V.length pts) labels)

-- ② 更新（仲間の平均。仲間 0 なら動かさない）
updateCenters :: Pts -> U.Vector Int -> Pts -> Pts
updateCenters pts labels cs
  | V.null cs = cs
  | otherwise = V.generate k (\j -> let c = cnt U.! j in if c > 0 then U.generate dim (\d -> sums U.! (j * dim + d) / fromIntegral c) else cs V.! j)
  where
    k = V.length cs
    dim = U.length (V.head cs)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) labels)
    sums = runST $ do
      s <- UM.replicate (k * dim) 0
      forM_ [0 .. V.length pts - 1] $ \i -> do
        let l = labels U.! i
            p = pts V.! i
        forM_ [0 .. min dim (U.length p) - 1] $ \d -> UM.unsafeModify s (+ U.unsafeIndex p d) (l * dim + d)
      U.freeze s

initRandomCenters :: Pts -> Int -> R Pts    -- ランダムに k 点えらぶ（重ならないように）
initRandomCenters pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = go 0 (U.enumFromN 0 n) []
  where
    n = V.length pts
    go j idx out
      | j >= min k n = pure (V.fromList (reverse out ++ replicate (k - length out) (V.head pts)))
      | otherwise = do
          u <- rnd
          let r = j + floor (u * fromIntegral (n - j))
              idx' = idx U.// [(j, idx U.! r), (r, idx U.! j)]
          go (j + 1) idx' (pts V.! (idx' U.! j) : out)

-- k-means++：今ある中心から遠い点ほど（距離² に比例して）えらばれやすい
initPlusPlus :: Pts -> Int -> R Pts
initPlusPlus pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = do
      u <- rnd
      let c0 = pts V.! min (n - 1) (floor (u * fromIntegral n))
      loop [c0] (U.generate n (\i -> sqd (pts V.! i) c0))
  where
    n = V.length pts
    loop out d2
      | length out >= k = pure (V.fromList (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pts V.! pick
          loop (c : out) (U.imap (\i v -> min v (sqd (pts V.! i) c)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

-- k-means の結果。hist = 半ステップ（割り当て・更新）ごとの慣性、all = やり直しごとの慣性
data KM = KM { kmCenters :: !Pts, kmLabels :: !(U.Vector Int), kmInertia :: !Double, kmHist :: ![Double], kmIters :: !Int, kmAll :: ![Double] }

-- 最後まで回す（maxIter 回まで。0 なら 100 回）
lloyd :: Pts -> Pts -> Int -> KM
lloyd pts c0 maxIter = go 0 c0 l0 [inertiaOf pts c0 l0]
  where
    mi = if maxIter <= 0 then 100 else maxIter
    l0 = assignLabels pts c0
    go it cs ls hist
      | it >= mi = finish it cs ls hist
      | otherwise =
          let nc = updateCenters pts ls cs
              h1 = inertiaOf pts nc ls
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              hist' = h2 : h1 : hist
          in if nl == ls then finish (it + 1) nc nl hist' else go (it + 1) nc nl hist'
    finish it cs ls hist = KM cs ls (headOr 0 hist) (reverse hist) it []

-- restarts 回やり直して慣性がいちばん小さいものを選ぶ（usePP = k-means++ で始める）
kmeansBest :: Pts -> Int -> Int -> Bool -> Int -> R KM
kmeansBest pts k restarts usePP maxIter = do
  rs <- forM [1 .. max 1 restarts] $ \_ -> do
    c0 <- if usePP then initPlusPlus pts k else initRandomCenters pts k
    pure (lloyd pts c0 maxIter)
  let best = foldl' (\b r -> if kmInertia r < kmInertia b then r else b) (headOr (KM V.empty U.empty 0 [] 0 []) rs) (drop 1 rs)
  pure best { kmAll = map kmInertia rs }

-- ---- 正解との一致：分割表、純度、調整ランド指数（ARI）、正規化相互情報量（NMI）。ラベル −1（ノイズ）も 1 つのグループとして数える
-- 分割表 M（行 = a のグループ、列 = b のグループ。どちらも小さい番号順）
contingency :: U.Vector Int -> U.Vector Int -> ([[Int]], [Int], [Int])
contingency a b = ([ [ M.findWithDefault 0 (x, y) cnt | y <- cb ] | x <- ra ], ra, cb)
  where
    ra = M.keys (M.fromList [ (x, ()) | x <- U.toList a ])
    cb = M.keys (M.fromList [ (y, ()) | y <- U.toList b ])
    cnt = M.fromListWith (+) [ ((x, y), 1 :: Int) | (x, y) <- zip (U.toList a) (U.toList b) ]

comb2 :: Double -> Double
comb2 x = x * (x - 1) / 2

ariScore :: U.Vector Int -> U.Vector Int -> Double
ariScore a b
  | U.null a = 1
  | otherwise = if maxv - expv == 0 then 1 else (sij - expv) / (maxv - expv)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a)
    sij = foldl' (+) 0 [ comb2 (fromIntegral v) | r <- mt, v <- r ]
    sa = foldl' (+) 0 [ comb2 (fromIntegral (sum r)) | r <- mt ]
    sb = foldl' (+) 0 [ comb2 (fromIntegral v) | v <- foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt ]
    expv = sa * sb / max 1 (comb2 n)
    maxv = (sa + sb) / 2

nmiScore :: U.Vector Int -> U.Vector Int -> Double   -- NMI = 2·I(A; B) / (H(A) + H(B))
nmiScore a b
  | U.null a = 1
  | otherwise = let hs = h rs + h cs in if hs == 0 then 1 else clampv 0 1 (2 * mi / hs)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a) :: Double
    rs = [ fromIntegral (sum r) | r <- mt ] :: [Double]
    cs = map fromIntegral (foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt) :: [Double]
    h arr = negate (foldl' (\s v -> s + (if v > 0 then (v / n) * log (v / n) else 0)) 0 arr)
    mi = foldl' (+) 0 [ (v / n) * log (n * v / (ri * cj)) | (r, ri) <- zip mt rs, (vi, cj) <- zip r cs, let v = fromIntegral vi, v > 0 ]

purityScore :: U.Vector Int -> U.Vector Int -> Double
purityScore pred' truth
  | U.null pred' = 0
  | otherwise = let (mt, _, _) = contingency pred' truth in fromIntegral (sum (map (foldl' max 0) mt)) / fromIntegral (U.length pred')

-- ---- 混合ガウス分布（2 次元、共分散は全部）。共分散 = (s_xx, s_xy, s_yy)。GMM { 重み, 平均, 共分散 }
type Cov = (Double, Double, Double)
data GMM = GMM { gW :: ![Double], gMu :: ![(Double, Double)], gCv :: ![Cov] }

covFloor :: Double
covFloor = 1e-4                             -- 共分散の固有値の下限（1 点に縮んで尤度が ∞ になるのを防ぐ）

logGauss2 :: P -> (Double, Double) -> Cov -> Double
logGauss2 p (mx, my) (a, b, c) = -log (2 * pi) - 0.5 * log det - 0.5 * q
  where
    det = a * c - b * b
    dx = ptX p - mx
    dy = ptY p - my
    q = (c * dx * dx - 2 * b * dx * dy + a * dy * dy) / det

logSumExp :: [Double] -> Double
logSumExp arr = let m = foldl' max (-1 / 0) arr
                in if m == -1 / 0 then -1 / 0 else m + log (foldl' (\s v -> s + exp (v - m)) 0 arr)

-- E ステップ：所属確率 r_ik（各点で合計 1）と対数尤度
gmmE :: Pts -> GMM -> (V.Vector (U.Vector Double), Double)
gmmE pts gm = (V.map fst rl, V.foldl' (\s (_, l) -> s + l) 0 rl)
  where
    rl = V.map one pts
    one p = let lg = zipWith3 (\w mu cv -> log (max w 1e-300) + logGauss2 p mu cv) (gW gm) (gMu gm) (gCv gm)
                lse = logSumExp lg
            in (U.fromList (map (\v -> exp (v - lse)) lg), lse)

floorCov :: Cov -> Cov                      -- 固有値が covFloor より小さければ持ち上げる（ふだんは何もしない）
floorCov cv@(a, b, c)
  | l2 >= covFloor && not (isInfinite l1 || isNaN l1) = cv
  | otherwise = (m1 * co * co + m2 * si * si, (m1 - m2) * co * si, m1 * si * si + m2 * co * co)
  where
    tr = (a + c) / 2
    df = sqrt (((a - c) / 2) * ((a - c) / 2) + b * b)
    l1 = tr + df
    l2 = tr - df
    th = 0.5 * atan2 (2 * b) (a - c)
    co = cos th
    si = sin th
    finite v = not (isInfinite v || isNaN v)
    m1 = max (if finite l1 then l1 else 1) covFloor
    m2 = max (if finite l2 then l2 else covFloor) covFloor

-- M ステップ：重み・平均・共分散を所属確率の重みつきで計算しなおす
gmmM :: Pts -> V.Vector (U.Vector Double) -> Maybe GMM -> GMM
gmmM pts resp old
  | V.null resp = maybe (GMM [] [] []) id old
  | otherwise = let sw = foldl' (+) 0 (map fst3 comps) in GMM (map (\(w, _, _) -> w / sw) comps) (map snd3 comps) (map thd3 comps)
  where
    k = U.length (V.head resp)
    n = V.length pts
    comps = map comp [0 .. k - 1]
    comp j =
      let (nk, sx, sy) = foldl' (\(a0, a1, a2) i -> let r = resp V.! i U.! j; p = pts V.! i in (a0 + r, a1 + r * ptX p, a2 + r * ptY p)) (0, 0, 0) [0 .. n - 1]
          mx = sx / nk
          my = sy / nk
          (a, b, c) = foldl' (\(a0, a1, a2) i -> let r = resp V.! i U.! j; p = pts V.! i; dx = ptX p - mx; dy = ptY p - my in (a0 + r * dx * dx, a1 + r * dx * dy, a2 + r * dy * dy)) (0, 0, 0) [0 .. n - 1]
      in if nk < 1e-8
           then (1e-8, maybe (let p = pts V.! (j `mod` n) in (ptX p, ptY p)) (\o -> headOr (0, 0) (drop j (gMu o))) old, maybe (1, 0, 1) (\o -> headOr (1, 0, 1) (drop j (gCv o))) old)
           else (nk / fromIntegral n, (mx, my), floorCov (a / nk, b / nk, c / nk))
    fst3 (x, _, _) = x
    snd3 (_, x, _) = x
    thd3 (_, _, x) = x

gmmInit :: Pts -> Int -> R GMM              -- k-means++ の中心から始めて、近い点を仲間にして M ステップ 1 回
gmmInit pts k = do
  cs <- initPlusPlus pts k
  let lab = assignLabels pts cs
      resp = V.generate (V.length pts) (\i -> U.generate k (\j -> if j == lab U.! i then 1 else 0))
  pure (gmmM pts resp (Just (GMM [] [ (ptX c, ptY c) | c <- V.toList cs ] (replicate k (0.3, 0, 0.3)))))

-- EM の結果。hist = 対数尤度の記録（必ず増えるか同じ）
data GFit = GFit { gfGm :: !GMM, gfLL :: !Double, gfResp :: !(V.Vector (U.Vector Double)), gfHist :: ![Double] }

gmmFit :: Pts -> Int -> Int -> R GFit       -- EM をくり返す
gmmFit pts k iters = do
  gm0 <- gmmInit pts k
  let go t gm (resp, ll) hist
        | t >= iters = GFit gm ll resp (reverse (ll : hist))
        | otherwise =
            let gm' = gmmM pts resp (Just gm)
                e'@(_, ll') = gmmE pts gm'
            in if t > 3 && abs (ll' - ll) < 1e-9 * abs ll'
                 then GFit gm' ll' (fst e') (reverse (ll' : ll : hist))
                 else go (t + 1) gm' e' (ll : hist)
  pure (go 0 gm0 (gmmE pts gm0) [])

gmmBIC :: Double -> Int -> Int -> Double    -- 母数の数 = 平均 2k + 共分散 3k + 重み (k − 1)
gmmBIC ll k n = -2 * ll + fromIntegral (6 * k - 1) * log (fromIntegral n)

covAxes :: Cov -> (Double, Double, Double)  -- 共分散の楕円：(長い軸の標準偏差, 短い軸, 傾き)
covAxes (a, b, c) = (sqrt (max 0 (tr + df)), sqrt (max 0 (tr - df)), 0.5 * atan2 (2 * b) (a - c))
  where
    tr = (a + c) / 2
    df = sqrt (((a - c) / 2) * ((a - c) / 2) + b * b)

-- ---- グラフと図の道具：G = 画面の四角（左上 x, y、幅、高さ）とデータの範囲（p5.js と同じ、左上が原点）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

invX :: G -> Float -> Double                -- 画面の x → データの x
invX g sx = x0 g + realToFrac ((sx - pX g) / pW g) * (x1 g - x0 g)

invY :: G -> Float -> Double
invY g sy = y0 g + realToFrac ((pY g + pH g - sy) / pH g) * (y1 g - y0 g)

inBox :: G -> Float -> Float -> Bool
inBox g sx sy = sx >= pX g && sx <= pX g + pW g && sy >= pY g && sy <= pY g + pH g

-- 縦横の縮尺をそろえる（span = 短い辺に入るデータの幅、(cx, cy) = まん中）
fitView :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> G
fitView x y w h cx cy sp = G x y w h (cx - hw) (cx + hw) (cy - hh) (cy + hh)
  where
    s = realToFrac (min w h) / sp
    hw = realToFrac w / 2 / s
    hh = realToFrac h / 2 / s

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- クラスタの色で点を描く（ラベルがなければ白）。r = 直径、al = 不透明度（0〜255）
drawPts :: G -> Pts -> Maybe (U.Vector Int) -> Float -> Int -> Picture
drawPts g pts labels r al = Pictures
  [ Color (maybe (rgbaC 230 230 240 al) (\ls -> palCA (if i < U.length ls then ls U.! i else 0) al) labels) (circleAt (gX g (ptX p)) (gY g (ptY p)) r)
  | (i, p) <- zip [0 ..] (V.toList pts) ]

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
clampv :: Double -> Double -> Double -> Double   -- clampv 下限 上限 値
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = let s = showFFloat (Just n) (fin 0 v) "" in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

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
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

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

-- 右下に小さく（view の中で使う）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
