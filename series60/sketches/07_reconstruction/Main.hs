-- ============================================
-- 60-07 復元と誤差 ─ k 個の主成分だけで画像を作り直す：x_hat = μ + Σ_(j≤k) ((x − μ)·v_j) v_j
-- データ = 8×8 マスの小さな模様（しま・ぼんやりした丸・明るさ）400 枚。d = 64 次元をヤコビ法で固有値分解
--   復元誤差の約束：E(k) = Σ_i ‖x_i − x_hat_i‖² / (n − 1)（共分散と同じく n − 1 で割る）→ E(k) = 捨てた固有値の和 λ_(k+1) + … + λ_64
--   同じ k 本なら、主成分の向きがいちばん誤差が小さい（ランダムな向きと比べる）
-- 操作：← → 使う成分の数 k / クリック 模様を選ぶ / R 新しいデータ / N ノイズ ON/OFF
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8, Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

pn, dimP, nPts :: Int
pn = 8                            -- ★ 模様の大きさ（8×8 = 64 次元）
dimP = pn * pn
nPts = 400                        -- ★ 模様の枚数

-- データから計算したもの（R・N のたびに作り直す。重いので裏で計算する）
data Built = Built
  { bX      :: !Mat               -- 模様 400 枚（1 行 = 64 個の明るさ）
  , bMu     :: !Vec               -- 平均の模様
  , bEig    :: !Eig               -- 共分散行列（64×64）の固有値分解
  , bErr    :: !Vec }             -- 実際に作り直した誤差 E(0)〜E(64)

data World = World
  { wK       :: !Int              -- ★ 使う成分の数 k
  , wSeed    :: !Int
  , wNoise   :: !Bool
  , wSel     :: !Int              -- 選んだ模様
  , wB       :: Built
  , wPending :: Maybe (Later Built)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-07 Reconstruction and error" world0)
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      printSelfTest selfTest
      let b = build 2 True
      _ <- evaluate (forceBuilt b)
      pure world0 { wB = b, wTest = selfTest }
  , skStep  = \_ _ w -> case wPending w of
      Just job -> do
        r <- pollLater job
        pure (maybe w (\b -> w { wB = b, wPending = Nothing }) r)
      Nothing -> pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 4 2 True 0 (build 2 True) Nothing []

-- ★ 模様の作り方：しま（向き・細かさはランダム）＋ ぼんやりした丸 ＋ 明るさ ＋ 少しのノイズ
makePatch :: Double -> R Vec
makePatch noise = do
  th <- (* pi) <$> rnd
  f <- rndRange 0.4 1.4
  ph <- (* (2 * pi)) <$> rnd
  a <- rndRange 0 0.5
  b <- rndRange (-0.6) 0.6
  cx <- rndRange 1 6
  cy <- rndRange 1 6
  c0 <- rndRange (-0.3) 0.3
  U.generateM dimP $ \i -> do
    let x = fromIntegral (i `mod` pn)
        y = fromIntegral (i `div` pn)
    g <- gaussRnd                 -- ノイズ 0 でも乱数は引く（p5.js 版と同じ）
    pure (c0 + a * cos (f * (x * cos th + y * sin th) + ph) + b * exp (-((x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int)) / 6) + noise * g)

build :: Int -> Bool -> Built
build seed noiseOn = Built x mu eg err
  where
    x = withSeed seed (V.replicateM nPts (makePatch (if noiseOn then 0.05 else 0)))
    mu = colMeans x
    eg = jacobiEigen (covMatrix x)
    err = reconErrors x mu (eVecs eg)

forceBuilt :: Built -> Double
forceBuilt b = sumMat (bX b) + sumMat (eVecs (bEig b)) + U.sum (bErr b)

-- k = 0〜d のすべてで復元誤差 E(k) を実際に作り直して計算
--   x_hat は μ から始めて成分を 1 本ずつ足していくので、k − 1 本の復元に k 本目を足せば k 本の復元になる
reconErrors :: Mat -> Vec -> Mat -> Vec
reconErrors x mu vecs = U.map (/ fromIntegral (nRows x - 1)) (V.foldl' addV (U.replicate (kmax + 1) 0) perRow)
  where
    kmax = nRows vecs
    d = U.length mu
    perRow = V.map (\r -> let xc = subV r mu
                              s = U.generate kmax (\k -> loopDot xc (mrow vecs k))    -- 得点 (x − μ)·v_k
                              xhs = scanl (\xh k -> let sk = vix s k in U.zipWith (\a b -> a + sk * b) xh (mrow vecs k)) mu [0 .. kmax - 1]
                          in U.fromList [ sqErr r h' | h' <- xhs ]) x
    sqErr r h = go 0 0 where go !j !acc = if j >= d then acc else let e = U.unsafeIndex r j - U.unsafeIndex h j in go (j + 1) (acc + e * e)

-- 内積を左から順に（(x − μ) を先に引いてから掛けても、p5.js 版の (r_j − μ_j)·v_j と同じ値）
loopDot :: Vec -> Vec -> Double
loopDot a b = go 0 0 where
  n = min (U.length a) (U.length b)
  go !j !acc = if j >= n then acc else go (j + 1) (acc + U.unsafeIndex a j * U.unsafeIndex b j)

-- ---- 模様の絵（1 マス = 1 画素の絵を拡大。いくつかの模様を 1 枚の絵に並べる）
patchGray :: Double -> (Word8, Word8, Word8)
patchGray v = let g = byte (clampv (128 + 160 * v) 0 255) in (g, g, g)

eigColor :: Double -> (Word8, Word8, Word8)     -- オレンジ = 正、青 = 負
eigColor t = if t > 0 then (byte (128 + 127 * t), byte (128 + 60 * t), byte (128 - 80 * t))
                      else (byte (128 + 80 * t), byte (128 - 20 * t), byte (128 - 127 * t))

-- 模様を cols 列に、1 マスのすき間をあけて並べた絵（左上 (0, 0) に置く大きさ k の絵を返す）
tiles :: Int -> Float -> [Vec -> Int -> (Word8, Word8, Word8)] -> [Vec] -> Picture
tiles cols k cols' vs = imageTL 0 0 w h k px
  where
    tv = V.fromList (zip vs cols')
    nt = V.length tv
    rows = (nt + cols - 1) `div` cols
    w = max 1 (cols * (pn + 1) - 1)
    h = max 1 (rows * (pn + 1) - 1)
    px x y = let (c, cx) = x `divMod` (pn + 1)
                 (r, cy) = y `divMod` (pn + 1)
                 t = r * cols + c
             in if cx >= pn || cy >= pn || t >= nt then (24, 30, 38)
                else let (v, f) = tv V.! t in f v (cx + pn * cy)

patchRow :: [Vec] -> Picture
patchRow vs = tiles 8 9 (map (const (\v i -> patchGray (vix v i))) vs) vs

eigTiles :: [Vec] -> Picture
eigTiles vs = tiles 8 6 (map (\v -> let m = U.foldl' (\a x -> max a (abs x)) 0 v
                                        m' = if m == 0 then 1 else m
                                    in \u i -> eigColor (vix u i / m')) vs) vs

bigPatch :: Vec -> Picture
bigPatch v = imageTL 0 0 pn pn 18 (\x y -> patchGray (vix v (x + pn * y)))

step9, step6 :: Float
step9 = 9 * fromIntegral (pn + 1)  -- 上の模様の間隔
step6 = 6 * fromIntegral (pn + 1)  -- 固有画像の間隔

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 上：もとの模様と復元（8 枚）
  [ say "Originals (click to select)" 40 112 15 (rgbC 205 212 222)
  , say ("Rebuilt with k = " ++ show k) 40 220 15 (rgbC 205 212 222)
  , Translate 40 122 (patchRow (map (mrow (bX b)) [0 .. 7]))
  , Translate 40 228 (patchRow [ rebuild (mrow (bX b) j) | j <- [0 .. 7] ])
  , Color (rgbC 255 225 140) (frameTL 2 (40 + fromIntegral (wSel w) * step9 - 3) 119 (fromIntegral pn * 9 + 6) (fromIntegral pn * 9 + 6)) ]
  -- ---- 選んだ模様を大きく：もと・復元・差
  ++ concat [ [ say t (40 + fromIntegral i * (fromIntegral pn * 18 + 24)) (by - 8) 14 (rgbC 205 212 222)
              , Translate (40 + fromIntegral i * (fromIntegral pn * 18 + 24)) by (bigPatch v) ]
            | (i, (t, v)) <- zip [0 :: Int ..] [("original", xs), ("rebuilt x_hat", xh), ("difference x - x_hat (x3)", U.map (* 3) (subV xs xh))] ]
  ++ [ say ("Error of this pattern |x - x_hat|^2 = " ++ fmtN (U.sum (U.map (^ (2 :: Int)) (subV xs xh))) 4) 40 (by + 144 + 28) 15 (grayC 220)
  -- ---- 固有画像（主成分 1〜16）
     , say "Components v1..v16 (orange +, blue -), the k in use framed yellow" 40 (by + 144 + 64) 14 (rgbC 205 212 222)
     , Translate 40 ey (eigTiles (V.toList (V.take 16 vecs))) ]
  ++ [ Color (rgbC 255 225 140) (frameTL 1.5 (40 + fromIntegral (i `mod` 8) * step6 - 2) (ey + fromIntegral (i `div` 8) * step6 - 2) (fromIntegral pn * 6 + 4) (fromIntegral pn * 6 + 4))
     | i <- [0 .. min 16 k - 1] ]
  -- ---- 誤差のグラフ
  ++ [ plotAxes g "number of components k" "E(k) (dots) and dropped eigenvalues (line)" [0, 16, 32, 48, 64] [0, tot / 2, tot]
     , Color (rgbC 90 165 240) (polyline 2.5 [ (gX g (fromIntegral i), gY g v) | (i, v) <- zip [0 :: Int ..] dropped ]) ]
  ++ [ Color (rgbC 255 225 140) (circleAt (gX g (fromIntegral i)) (gY g v) 5) | (i, v) <- zip [0 :: Int ..] (U.toList err), even i || i < 12 ]
  ++ [ Color (rgbC 255 125 105) (lineW 1 (gX g (fromIntegral k), pY g) (gX g (fromIntegral k), pY g + pH g))
  -- ---- 文字
     , say "Reconstruction and error" 40 44 22 (grayC 240)
     , say ("<- -> number of components k (" ++ show k ++ ")   click select a pattern   R new data   N noise (" ++ (if wNoise w then "ON" else "OFF") ++ ")") 40 72 15 (rgbC 165 175 190)
     , say ("Error actually rebuilt E(" ++ show k ++ ") = " ++ fmtN (vix err k) 6) tx ty 17 (rgbC 255 225 140)
     , say ("Dropped eigenvalues L" ++ show (k + 1) ++ " + ... + L64 = " ++ fmtN (nth 0 dropped k) 6) tx (ty + 26) 17 (rgbC 90 165 240)
     , say ("Kept (cumulative ratio) = " ++ fmtN (100 * (1 - vix err k / tot)) 2 ++ "%") tx (ty + 52) 17 (grayC 220)
     , say ("64 numbers described by " ++ show k ++ " scores") tx (ty + 76) 17 (grayC 220)
     , sayWrap "E(k) = sum |x - x_hat|^2 / (n - 1). k = 0 describes everything by the mean mu (= total variance = trace). k = 64 gives error 0."
               tx (ty + 92) (pW g) 14 (rgbC 165 175 190) ]
  ++ [ say "computing..." 420 44 18 (rgbC 255 225 140) | Just _ <- [wPending w] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    b = wB w
    k = wK w
    vecs = eVecs (bEig b)
    rebuild v = reconstructRow (mrow (projectData (V.singleton v) (bMu b) vecs k) 0) (bMu b) vecs   -- k 本で復元
    xs = mrow (bX b) (wSel w)
    xh = rebuild xs
    by = 350
    ey = by + 144 + 76
    err = bErr b
    tot = vix err 0
    vals = U.toList (eVals (bEig b))
    dropped = [ foldl' (+) 0 (drop i vals) | i <- [0 .. dimP] ]
    g = G (envW env * 0.6) 130 (envW env * 0.36) (envH env * 0.42) 0 (fromIntegral dimP) 0 (tot * 1.05)
    tx = pX g
    ty = pY g + pH g + 66

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hit = [ j | j <- [0 .. 7], let x = 40 + fromIntegral j * step9, mx > x && mx < x + fromIntegral pn * 9 && my > 122 && my < 122 + fromIntegral pn * 9 ]
    in pure (case hit of { (j : _) -> w { wSel = j }; [] -> w })
  EventKey (SpecialKey k) Down _ _
    | k == KeyLeft || k == KeyRight -> pure w { wK = max 0 (min dimP (wK w + (if k == KeyRight then 1 else -1))) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> restart w { wSeed = wSeed w + 1 }
    | c `elem` "nN" -> restart w { wNoise = not (wNoise w) }
  _ -> pure w

-- 新しいデータの計算を裏で始める（前の計算が残っていたら止める）
restart :: World -> IO World
restart w = do
  mapM_ cancelLater (wPending w)
  job <- startLater forceBuilt (build (wSeed w) (wNoise w))
  pure w { wPending = Just job }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Jacobi (64x64): C V = V L (<= 1e-9 x trace), orthonormal", eigResidual c e < 1e-9 * tr && orthoError (eVecs e) < 1e-9)
  , ("for every k = 0..64, E(k) = sum of dropped eigenvalues (relative 1e-9)", and [ near (vix ee k) (foldl' (+) 0 (drop k vals)) (1e-9 * tr) | k <- [0 .. dimP] ])
  , ("E(0) = trace (total variance), E(64) ~ 0", near (vix ee 0) tr (1e-9 * tr) && vix ee dimP < 1e-12 * tr)
  , ("E(k) decreases with k", and [ vix ee k <= vix ee (k - 1) + 1e-12 * tr | k <- [1 .. dimP] ])
  , ("k random orthonormal directions (20 sets) always give error >= principal components", okOpt)
  , ("all 64 components rebuild the original (1e-12); 0 components give the mean", okAll) ]
  where
    (x, bs) = withSeed 107 $ do
      xx <- V.replicateM 150 (makePatch 0.05)
      bb <- forM [0 .. 19 :: Int] $ \t -> do
        let k = 1 + t `mod` 8
        q <- orthonormalize =<< replicateM k (U.replicateM dimP gaussRnd)
        pure (k, V.fromList q)
      pure (xx, bb)
    mu = colMeans x
    c = covMatrix x
    e = jacobiEigen c
    vals = U.toList (eVals e)
    ee = reconErrors x mu (eVecs e)
    tr = traceOf c
    okOpt = and [ not (vix (reconErrors x mu bq) k < vix ee k - 1e-9) | (k, bq) <- bs ]
    x0r = mrow x 0
    r = reconstructRow (mrow (projectData (V.singleton x0r) mu (eVecs e) 64) 0) mu (eVecs e)
    okAll = and (U.toList (U.zipWith (\a b -> near a b 1e-12) r x0r)) && reconstructRow U.empty mu (eVecs e) == mu

-- ---------- やってみよう ----------
-- 1. k を 0 → 1 → 2 → … と増やすと、復元はどう変わる？ 何本で「ほぼ同じ」に見える？
-- 2. N でノイズを消すと、誤差のグラフはどこで 0 になる？（模様を作る要素の数を考えよう）
-- 3. 固有画像 v1, v2, … はどんな模様？ しまの向き？ 明るさ？
-- 4. 同じ誤差なら、平均だけ（k = 0）の何 % の誤差まで下げたら十分？ 06 の寄与率と比べよう
-- 5. 誤差 = 捨てた固有値の和 なのは「主成分どうしが直交していて、ピタゴラスの定理が使える」から

-- ==================================================
-- ここから下は「主成分分析の道具」（中身は読まなくても OK。消さないでね）
-- 約束：データ X は「n 行 × d 列」（1 行 = 1 個のデータ）。共分散は n − 1 で割る（C = Xcᵀ Xc / (n − 1)）
-- ベクトル Vec = Double の並び（unboxed）、行列 Mat = Vec（行）の並び。計算は Double、描くときだけ Float にする
-- ==================================================
-- ---- 数の表示と安全のための道具
clampv :: Double -> Double -> Double -> Double      -- clampv v a b：v を a〜b に（p5.js 版と同じ順）
clampv v a b = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double                   -- NaN・∞ を fb に
fin v fb = if isNaN v || isInfinite v then fb else v

f2 :: Double -> Float                               -- 描くときの Float（NaN は 0）
f2 v = realToFrac (fin v 0)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 数を小数 d 桁で（NaN は「?」、"-0.00" は "0.00"）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v || isInfinite v = "?"
  | otherwise = let s = showFFloat (Just d) v ""
                in if take 1 s == "-" && all (`elem` "0.") (drop 1 s) then drop 1 s else s

fmtVec :: Vec -> Int -> String
fmtVec v d = "(" ++ joinWith ", " (map (`fmtN` d) (U.toList v)) ++ ")"

-- 指数の形で小数 d 桁（toExponential）
fmtE :: Double -> Int -> String
fmtE v d = if isNaN v || isInfinite v then "?" else showEFloat (Just d) v ""

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin v0 0

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Integer)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- リストの k 番目（なければ d）
nth :: a -> [a] -> Int -> a
nth d xs k = if k < 0 then d else case drop k xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Int -> Rng                               -- (s >>> 0) || 1
seedRng s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- seedRng(s) してから計算し、結果だけ取り出す
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double                                     -- 0〜1 の一様乱数
rnd = R rndStep

rndRange :: Double -> Double -> R Double            -- a〜b の一様乱数
rndRange a b = do u <- rnd; pure (a + (b - a) * u)

gaussRnd :: R Double                                -- 標準正規分布（ボックス-ミュラー法。u, v の順に 2 つ引く）
gaussRnd = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- 行列の道具。データ X は「n 行 × d 列」（1 行 = 1 個のデータ）
type Vec = U.Vector Double
type Mat = V.Vector Vec

vec :: [Double] -> Vec
vec = U.fromList

mat :: [[Double]] -> Mat
mat = V.fromList . map U.fromList

vix :: Vec -> Int -> Double                         -- 成分（範囲の外は 0）
vix v i = maybe 0 id (v U.!? i)

mrow :: Mat -> Int -> Vec                           -- 行（範囲の外は空）
mrow m i = maybe U.empty id (m V.!? i)

mix :: Mat -> Int -> Int -> Double                  -- (i, j) 成分
mix m i j = vix (mrow m i) j

nRows, nCols :: Mat -> Int
nRows = V.length
nCols m = U.length (mrow m 0)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = V.generate n (\i -> U.generate n (\j -> if i == j then 1 else 0))

matT :: Mat -> Mat
matT a = V.generate (nCols a) (\j -> U.generate (nRows a) (\i -> mix a i j))

matMul :: Mat -> Mat -> Mat
matMul a b = V.map row a
  where m = nCols b
        row r = U.ifoldl' (\acc k x -> if x == 0 then acc else U.zipWith (\c bk -> c + x * bk) acc (mrow b k)) (U.replicate m 0) r

matVec :: Mat -> Vec -> Vec
matVec a x = V.convert (V.map (`dotv` x) a)

dotv :: Vec -> Vec -> Double                       -- 内積（左から順に足す）
dotv a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

vlen :: Vec -> Double
vlen a = sqrt (dotv a a)

unitv :: Vec -> Vec                                 -- 長さ 1 に（0 ベクトルは (1, 0, …)）
unitv a = let l = vlen a in if l > 1e-300 then U.map (/ l) a else U.imap (\i _ -> if i == 0 then 1 else 0) a

scaleV :: Double -> Vec -> Vec
scaleV k = U.map (k *)

addV, subV :: Vec -> Vec -> Vec
addV = U.zipWith (+)
subV = U.zipWith (-)

maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = V.foldl' max 0 (V.zipWith (\r s -> U.foldl' max 0 (U.zipWith (\x y -> abs (x - y)) r s)) a b)

colMeans :: Mat -> Vec                              -- 平均ベクトル（列ごとの平均）
colMeans x = U.map (/ fromIntegral (max 1 (nRows x))) (V.foldl' addV (U.replicate (nCols x) 0) x)

centerCols :: Mat -> Vec -> Mat                     -- 平均を引く（中心化）
centerCols x mu = V.map (`subV` mu) x

-- ★ 共分散行列 C = Xcᵀ Xc / (n − 1)（n − 1 で割る「不偏」の約束。点が 1 個以下ならゼロ行列）
covMatrix :: Mat -> Mat
covMatrix x
  | n < 2 = zerosMat d d
  | otherwise = toMat d $ runST $ do
      c <- UM.replicate (d * d) 0
      V.forM_ x $ \r -> forM_ [0 .. d - 1] $ \i -> do
        let a = vix r i - vix mu i
        forM_ [i .. d - 1] $ \j -> UM.modify c (+ a * (vix r j - vix mu j)) (i * d + j)
      forM_ [0 .. d - 1] $ \i -> forM_ [i .. d - 1] $ \j -> do
        v <- UM.read c (i * d + j)
        let v' = v / fromIntegral (n - 1)
        UM.write c (i * d + j) v'
        UM.write c (j * d + i) v'
      U.freeze c
  where n = nRows x
        d = nCols x
        mu = colMeans x

-- 長さ d·d の並びを d×d の行列に
toMat :: Int -> Vec -> Mat
toMat d flat = V.generate (if d > 0 then U.length flat `div` d else 0) (\i -> U.slice (i * d) d flat)

-- 固有値分解の結果：vals = 固有値（大きい順）、vecs の k 行目 = k 番目の固有ベクトル
data Eig = Eig { eVals :: !Vec, eVecs :: !Mat }

-- ★ ヤコビ法（対称行列の固有値分解）：非対角を 1 つずつ回転で 0 にしていく。d ≤ 64 くらいまで
-- 固有ベクトルは長さ 1、いちばん大きい成分を正に
jacobiEigen :: Mat -> Eig
jacobiEigen = jacobiEigenN 80

jacobiEigenN :: Int -> Mat -> Eig
jacobiEigenN maxSweeps c = Eig (U.fromList (map (\k -> vix diag k) idx)) (V.fromList (map vecOf idx))
  where
    d = nRows c
    (af, vf) = runST $ do
      a <- U.thaw (U.concat (V.toList (V.map (\r -> U.generate d (vix r)) c)))
      v <- U.thaw (U.generate (d * d) (\k -> if k `div` d == k `mod` d then 1 else 0))
      let rd i j = UM.unsafeRead a (i * d + j)
          wr i j x = UM.unsafeWrite a (i * d + j) x
          sweep sw = when (sw < maxSweeps) $ do
            (off, tot) <- foldM (\(!o, !t) k -> do
                                   x <- UM.unsafeRead a k
                                   let s = x * x
                                   pure (if k `div` d /= k `mod` d then o + s else o, t + s)) (0, 0) [0 .. d * d - 1]
            when (off > 1e-30 * tot) $ do
              forM_ [0 .. d - 2] $ \p -> forM_ [p + 1 .. d - 1] $ \q -> do
                apq <- rd p q
                when (abs apq >= 1e-300) $ do
                  app <- rd p p
                  aqq <- rd q q
                  let th = (aqq - app) / (2 * apq)
                      t = if th == 0 then 1 else signum th / (abs th + sqrt (th * th + 1))
                      cs = 1 / sqrt (t * t + 1)
                      sn = t * cs
                  forM_ [0 .. d - 1] $ \k -> do
                    akp <- rd k p
                    akq <- rd k q
                    wr k p (cs * akp - sn * akq)
                    wr k q (sn * akp + cs * akq)
                  forM_ [0 .. d - 1] $ \k -> do
                    apk <- rd p k
                    aqk <- rd q k
                    wr p k (cs * apk - sn * aqk)
                    wr q k (sn * apk + cs * aqk)
                  forM_ [0 .. d - 1] $ \k -> do
                    vkp <- UM.unsafeRead v (k * d + p)
                    vkq <- UM.unsafeRead v (k * d + q)
                    UM.unsafeWrite v (k * d + p) (cs * vkp - sn * vkq)
                    UM.unsafeWrite v (k * d + q) (sn * vkp + cs * vkq)
              sweep (sw + 1)
      sweep 0
      af' <- U.freeze a
      vf' <- U.freeze v
      pure (af', vf')
    diag = U.generate d (\i -> vix af (i * d + i))
    idx = sortBy (\x y -> compare (vix diag y) (vix diag x)) [0 .. d - 1]
    vecOf k = fixSign (U.generate d (\i -> vix vf (i * d + k)))

-- いちばん大きい成分（同じくらいなら前のほう）を正にそろえる
fixSign :: Vec -> Vec
fixSign v = let big = U.ifoldl' (\b i x -> if abs x > abs (vix v b) + 1e-12 then i else b) 0 v
            in if vix v big < 0 then U.map negate v else v

traceOf :: Mat -> Double
traceOf c = sum [ mix c i i | i <- [0 .. nRows c - 1] ]

-- C V = V Λ の残り（いちばん大きいずれ）
eigResidual :: Mat -> Eig -> Double
eigResidual c eg = foldl' max 0 [ abs (vix cv i - vix (eVals eg) k * vix v i)
                                | (k, v) <- zip [0 ..] (V.toList (eVecs eg)), let cv = matVec c v, i <- [0 .. U.length v - 1] ]

-- 固有ベクトルどうしの直交のずれ
orthoError :: Mat -> Double
orthoError vs = foldl' max 0 [ abs (dotv a b - (if i == j then 1 else 0)) | (i, a) <- zip [0 :: Int ..] l, (j, b) <- zip [0 ..] l ]
  where l = V.toList vs

-- 射影（主成分得点）：score_k = (x − μ)·v_k（k 個まで）
projectData :: Mat -> Vec -> Mat -> Int -> Mat
projectData x mu vecs k = V.map (\r -> V.convert (V.map (\v -> U.sum (U.zipWith3 (\a m b -> (a - m) * b) r mu v)) (V.take k vecs))) x

-- 復元：x_hat = μ + Σ score_k v_k
reconstructRow :: Vec -> Vec -> Mat -> Vec
reconstructRow score mu vecs = U.ifoldl' (\x k s -> U.zipWith (\a b -> a + s * b) x (mrow vecs k)) mu score

-- 向きのちがい（符号は無視、0〜90°）。atan2(‖v − (v·u)u‖, |v·u|)
angleBetween :: Vec -> Vec -> Double
angleBetween u v = let a = unitv u
                       b = unitv v
                       c = dotv a b
                       r = U.sum (U.zipWith (\ai bi -> (bi - c * ai) ^ (2 :: Int)) a b)
                   in atan2 (sqrt r) (abs c) * 180 / pi

-- 相関係数
corrOf :: [Double] -> [Double] -> Double
corrOf a b = if saa > 0 && sbb > 0 then sab / sqrt (saa * sbb) else 0
  where n = fromIntegral (length a)
        ma = foldl' (+) 0 a / n
        mb = foldl' (+) 0 b / n
        (sab, saa, sbb) = foldl' (\(!p, !q, !r) (x, y) -> (p + (x - ma) * (y - mb), q + (x - ma) ^ (2 :: Int), r + (y - mb) ^ (2 :: Int))) (0, 0, 0) (zip a b)

-- 計算を全部すませる（起動や作り直しのあと、最初のコマが重くならないように）
sumMat :: Mat -> Double
sumMat = V.foldl' (\s r -> s + U.sum r) 0
-- ---- 部分空間反復法とグラム行列（大きいほうから k 個の固有ベクトルだけ）
-- グラム-シュミット（修正版）。長さがほぼ 0 になったら乱数で作り直す
orthonormalize :: [Vec] -> R [Vec]
orthonormalize = go []
  where
    go done [] = pure (reverse done)
    go done (q : qs) = do
      let q' = foldl' (\acc b -> let p = dotv acc b in U.zipWith (\x y -> x - p * y) acc b) q (reverse done)
          l = vlen q'
      if l < 1e-12
        then do fresh <- U.replicateM (U.length q) gaussRnd
                go done (fresh : qs)
        else go (U.map (/ l) q' : done) qs

-- ★ 部分空間反復法：べき乗法を k 本まとめて回し、毎回直交化。applyM v = M v（M を作らなくてよい）
-- 最後に k×k の小さい行列をヤコビ法で（レイリー-リッツ）
subspaceEigen :: (Vec -> Vec) -> Int -> Int -> Int -> Double -> R Eig
subspaceEigen applyM dim k iters tol = do
  q0 <- orthonormalize =<< replicateM k (U.replicateM dim gaussRnd)
  q <- loop 0 q0
  let mq = map applyM q
      t0 = [ [ dotv qa mb | mb <- mq ] | qa <- q ]
      t = [ [ if b < a || b > a then (nth 0 (nth [] t0 a) b + nth 0 (nth [] t0 b) a) / 2 else nth 0 (nth [] t0 a) b
            | b <- [0 .. k - 1] ] | a <- [0 .. k - 1] ]
      eg = jacobiEigen (mat t)
      comb w = fixSign (U.ifoldl' (\v a c -> U.zipWith (\x y -> x + c * y) v (nth U.empty q a)) (U.replicate dim 0) w)
  pure (Eig (eVals eg) (V.map comb (eVecs eg)))
  where
    loop it q
      | it >= iters = pure q
      | otherwise = do
          z <- orthonormalize (map applyM q)
          let diff = foldl' max 0 (zipWith (\za qa -> 1 - abs (dotv za qa)) z q)
          if tol > 0 && diff < tol && it > 5 then pure z else loop (it + 1) z

-- PCA の結果（グラム行列のやり方）：vals = 固有値、vecs = 主成分（d 次元、長さ 1）、gramTrace = 全分散
data PcaG = PcaG { pgVals :: !Vec, pgVecs :: !Mat, pgTrace :: !Double }

-- ★ 次元 d が点の数 n より大きいとき：グラム行列 G = Xc Xcᵀ / (n − 1)（n×n）の固有ベクトル u から
-- 主成分 v = Xcᵀ u / ‖Xcᵀ u‖ を作る（G と C は 0 でない固有値が同じ）。u は部分空間反復法で上から k 個
pcaGram :: Mat -> Int -> Int -> R PcaG
pcaGram xc k iters = do
  eg <- subspaceEigen (matVec g) n (min (k + 2) n) iters 1e-13      -- 2 本多めに回すと k 本目までの収束が速い
  let vecs = V.map (\u -> unitv (U.ifoldl' (\v i ui -> U.zipWith (\a x -> a + ui * x) v (mrow xc i)) (U.replicate d 0) u)) (V.take k (eVecs eg))
  pure (PcaG (U.map (max 0) (U.take k (eVals eg))) vecs (traceOf g))
  where
    n = nRows xc
    d = nCols xc
    half = V.generate n (\i -> U.generate (n - i) (\t -> dotv (mrow xc i) (mrow xc (i + t))))   -- 上半分だけ計算（対称）
    g = V.generate n (\i -> U.generate n (\j -> (if j >= i then vix (mrow half i) (j - i) else vix (mrow half j) (i - j)) / fromIntegral (max 1 (n - 1))))
-- ---- 2×2 対称行列 [[a, b], [b, c]] の固有値・固有ベクトル（解の公式）
data E2 = E2 { e2L1, e2L2 :: !Double, e2V1, e2V2 :: !Vec, e2Angle :: !Double }

eig2sym :: Double -> Double -> Double -> E2
eig2sym a b c = E2 l1 l2 v1 (vec [-(vix v1 1), vix v1 0]) (atan2 (vix v1 1) (vix v1 0) * 180 / pi)
  where
    tr = a + c
    h = sqrt (((a - c) / 2) ^ (2 :: Int) + b * b)
    l1 = tr / 2 + h
    l2 = tr / 2 - h
    v0 = if abs b > 1e-14 * (abs a + abs c + 1e-300) then unitv (vec [l1 - c, b]) else if a >= c then vec [1, 0] else vec [0, 1]
    v1 = if vix v0 0 < 0 || (vix v0 0 == 0 && vix v0 1 < 0) then U.map negate v0 else v0

-- 2×2 の行列 C の固有分解
eig2of :: Mat -> E2
eig2of c = eig2sym (mix c 0 0) (mix c 0 1) (mix c 1 1)
-- ---- グラフと図の道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 80 92 108) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 160 170 185) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 160 170 185) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 212 222), say yl (pX g) (pY g - 8) 14 (rgbC 205 212 222) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 共分散 C の「k σ の楕円」の点（データの単位）。中心 μ
covEllipsePts :: Vec -> Mat -> Double -> Int -> [(Double, Double)]
covEllipsePts mu c k segs =
  [ (vix mu 0 + u * vix (e2V1 e) 0 + w * vix (e2V2 e) 0, vix mu 1 + u * vix (e2V1 e) 1 + w * vix (e2V2 e) 1)
  | i <- [0 .. segs], let t = 2 * pi * fromIntegral i / fromIntegral segs, let u = a * cos t, let w = b * sin t ]
  where e = eig2of c
        a = k * sqrt (max 0 (e2L1 e))
        b = k * sqrt (max 0 (e2L2 e))

-- 矢印（太さ wt、矢じりの大きさ hs）
arrow2 :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
arrow2 wt hs (xa, ya) (xb, yb) = Pictures [thickLine wt (xa, ya) (xb, yb), Polygon [(xb, yb), (xb - hs * cos (a - 0.4), yb - hs * sin (a - 0.4)), (xb - hs * cos (a + 0.4), yb - hs * sin (a + 0.4))]]
  where a = atan2 (yb - ya) (xb - xa)
-- ---- 重い計算を裏で（別のスレッドで計算し、できたら受け取る。そのあいだも画面は止まらない）
data Later a = Later ThreadId (MVar a)

-- force x で x を全部計算してから渡す（force は全部の数を足すなど、中身を最後まで使う関数）
startLater :: (a -> Double) -> a -> IO (Later a)
startLater force x = do
  mv <- newEmptyMVar
  tid <- forkIO (do r <- try (evaluate (force x)) :: IO (Either SomeException Double)
                    case r of
                      Right _ -> putMVar mv x
                      Left _ -> pure ())
  pure (Later tid mv)

pollLater :: Later a -> IO (Maybe a)          -- できていれば Just
pollLater (Later _ mv) = tryTakeMVar mv

cancelLater :: Later a -> IO ()               -- いらなくなった計算を止める
cancelLater (Later tid _) = killThread tid
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

-- 破線（線 dash、すき間 gap）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine wt dash gap (xa, ya) (xb, yb) = Pictures [ thickLine wt (at' t0) (at' (min l (t0 + dash))) | t0 <- takeWhile (< l) [0, dash + gap ..] ]
  where l = sqrt ((xb - xa) ^ (2 :: Int) + (yb - ya) ^ (2 :: Int))
        at' t = if l <= 0 then (xa, ya) else (xa + (xb - xa) * t / l, ya + (yb - ya) * t / l)

-- 幅 w × 高さ h の画素の絵を k 倍にして、左上が (x, y) になるように置く（f x y は左上が (0, 0)）
imageTL :: Float -> Float -> Int -> Int -> Float -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
imageTL x y w h k f = Translate (x + fromIntegral w * k / 2) (y + fromIntegral h * k / 2) (Scale 1 (-1) (pixelImage w h k f))

-- 0〜255 に丸めた画素の値
byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round (fin v 0) :: Int)))
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any (\v -> isNaN v || isInfinite v)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs && not (null rs) then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

selfTestOk :: [(String, Bool)] -> Bool
selfTestOk rs = not (null rs) && all snd rs

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if selfTestOk rs then rgbC 120 210 170 else rgbC 255 90 90)
