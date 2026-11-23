-- ============================================
-- 60-08 固有画像（固有顔）─ 24×24 = 576 次元の「顔」200 枚を PCA して、顔の「変わり方」を取り出す
-- 顔はパラメータ（顔の幅・目の間隔・目の高さ・目の大きさ・口の曲がり・口の幅・明るさ・光の向き）を乱数で選んで描いた合成画像
--   576 次元の共分散行列（576×576）は大きすぎるので、グラム行列 G = Xc Xcᵀ/(n − 1)（200×200）の固有ベクトル u を
--   部分空間反復法（べき乗法を 12 本まとめて）で求め、主成分 v = Xcᵀ u / ‖Xcᵀ u‖ にする（G と C の 0 でない固有値は同じ）
--   新しい顔 = 平均顔 + Σ c_k √λ_k v_k。各主成分が「どのパラメータと一番相関するか」を数で表示
-- 操作：1〜5 主成分をえらぶ / ↑↓ その係数 c（σ 単位）/ ドラッグ スライダー / 0 係数を 0 に / R 新しい 200 人
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

nFace, kPc :: Int
nFace = 200                       -- ★ 顔の数
kPc = 12                          -- ★ 求める主成分の数

-- パラメータの名前と取り出し方（p5.js 版の PNAMES と同じ順）
pNames :: [(String, FaceP -> Double)]
pNames = [ ("face width", fpFaceW), ("eye distance", fpEyeDist), ("eye height", fpEyeY), ("eye size", fpEyeSize)
         , ("mouth curve", fpMouthCurve), ("mouth width", fpMouthW), ("brightness", fpBright), ("light direction", fpLight) ]

-- 顔とその PCA（R のたびに作り直す。重いので裏で計算する）
data Analysis = Analysis
  { aP    :: [FaceP]              -- 顔のパラメータ
  , aF    :: !Mat                 -- 顔 200 枚（1 行 = 576 個の明るさ）
  , aMu   :: !Vec                 -- 平均顔
  , aXc   :: !Mat                 -- 中心化した顔
  , aPc   :: !PcaG                -- 主成分（固有顔）
  , aMean :: [(String, Double)] } -- PC1〜PC6 といちばん相関の強いパラメータ（名前, 相関）

data World = World
  { wSeed    :: !Int
  , wA       :: Analysis
  , wCoef    :: [Double]          -- ★ 係数 c1〜c5（σ 単位、−3〜3）
  , wSelK    :: !Int
  , wDragK   :: !Int
  , wPending :: Maybe (Later Analysis)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "60-08 Eigenfaces" world0)
  { skBack  = rgbC 24 30 38
  , skStart = \_ -> do
      _ <- evaluate (forceA analysis8)
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> case wPending w of
      Just job -> do
        r <- pollLater job
        pure (maybe w (\a -> w { wA = a, wPending = Nothing }) r)
      Nothing -> pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 8 analysis8 [0, 0, 0, 0, 0] 0 (-1) Nothing []

-- 種 8 の顔（起動のときと自己テストで同じものを使う）
analysis8 :: Analysis
analysis8 = analyse 8

-- ★ 顔を作って PCA（グラム行列のやり方）し、各主成分の意味（いちばん相関の強いパラメータ）を調べる
--   乱数は「顔のパラメータ 200 人分 → 部分空間反復法の始まりのベクトル」の順
analyse :: Int -> Analysis
analyse seed = Analysis ps f mu xc r meaning
  where
    (ps, r) = withSeed seed $ do
      ps' <- replicateM nFace randomFaceParams
      let f' = V.fromList (map makeFace ps')
      r' <- pcaGram (centerCols f' (colMeans f')) kPc 150
      pure (ps', r')
    f = V.fromList (map makeFace ps)
    mu = colMeans f
    xc = centerCols f mu
    sc = projectData f mu (pgVecs r) 6
    meaning = [ foldl' (\(bn, bc) (nm, cc) -> if abs cc > abs bc then (nm, cc) else (bn, bc)) (nth ("", 0) cs 0) (drop 1 cs)
              | k <- [0 .. 5], let col = [ vix s k | s <- V.toList sc ]
              , let cs = [ (nm, corrOf col (map get ps)) | (nm, get) <- pNames ] ]

forceA :: Analysis -> Double
forceA a = sumMat (aF a) + sumMat (pgVecs (aPc a)) + sum (map snd (aMean a))

-- 合成した顔 = 平均 + Σ c_k √λ_k v_k
synth :: Analysis -> [Double] -> Vec
synth a cs = foldl' (\x (k, c) -> let s = c * sqrt (vix (pgVals (aPc a)) k)
                                  in U.zipWith (\xi vi -> xi + s * vi) x (mrow (pgVecs (aPc a)) k)) (aMu a) (zip [0 ..] cs)

-- ---- 画像（明るさ 0〜1、または符号つき：オレンジ = 正、青 = 負）
grayPx :: Double -> (Word8, Word8, Word8)
grayPx v = let g = byte (clampv (255 * v) 0 255) in (g, g, g)

signedPx :: Double -> (Word8, Word8, Word8)
signedPx t = if t > 0 then (byte (128 + 127 * t), byte (128 + 70 * t), byte (128 - 60 * t))
                      else (byte (128 + 60 * t), byte (128 - 10 * t), byte (128 - 127 * t))

-- 顔を cols 列に並べた 1 枚の絵（横のすき間 gx マス、縦のすき間 gy マス、1 マス = k 画素）
faceTiles :: Int -> Int -> Int -> Float -> Bool -> [Vec] -> Picture
faceTiles cols gx gy k signed vs = imageTL 0 0 w h k px
  where
    tv = V.fromList [ (v, if signed then let m = U.foldl' (\a x -> max a (abs x)) 0 v in if m == 0 then 1 else m else 1) | v <- vs ]
    nt = V.length tv
    rows = (nt + cols - 1) `div` cols
    w = max 1 (cols * (faceN + gx) - gx)
    h = max 1 (rows * (faceN + gy) - gy)
    px x y = let (c, cx) = x `divMod` (faceN + gx)
                 (r, cy) = y `divMod` (faceN + gy)
                 t = r * cols + c
             in if cx >= faceN || cy >= faceN || t >= nt then (24, 30, 38)
                else let (v, m) = tv V.! t
                         val = vix v (cx + faceN * cy)
                     in if signed then signedPx (val / m) else grayPx val

-- スライダーの位置
sliderGeo :: Env -> Int -> (Float, Float, Float)
sliderGeo env k = (380, 150 + 52 * fromIntegral k, min 330 (envW env * 0.26))

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 合成した顔（大）
  [ say "Face = mean + sum c_k sqrt(L_k) v_k" 40 120 15 (rgbC 205 212 222)
  , Translate 40 130 (faceTiles 1 0 0 12.5 False [synth a (wCoef w)])
  -- ---- 平均顔と固有顔
  , say "Mean face" 40 (ey - 8) 14 (rgbC 205 212 222)
  , Translate 40 ey (faceTiles 1 0 0 es False [aMu a])
  , say "Eigenfaces v1..v10 (orange +, blue -) and ratio" (40 + stepE) (ey - 8) 14 (rgbC 205 212 222)
  , Translate (40 + stepE) ey (faceTiles 5 4 8 es True (V.toList (V.take 10 (pgVecs pc)))) ]
  ++ [ say ("v" ++ show (k + 1) ++ " " ++ fmtN (100 * vix (pgVals pc) k / pgTrace pc) 1 ++ "%")
           (40 + stepE * fromIntegral (1 + k `mod` 5)) (ey + fromIntegral (k `div` 5) * (fromIntegral (faceN + 8) * es) + fromIntegral faceN * es + 15) 14 (grayC 170)
     | k <- [0 .. 9] ]
  -- ---- もとの顔
  ++ [ say "Original faces (drawn from parameters)" gx 120 14 (rgbC 205 212 222)
     , Translate gx 130 (faceTiles 5 3 3 2.6 False (V.toList (V.take 20 (aF a)))) ]
  -- ---- スライダー
  ++ concat [ [ Color (if k == wSelK w then rgbC 255 225 140 else rgbC 80 92 108) (lineW 3 (sx, sy) (sx + sw, sy))
              , Color (if k == wSelK w then rgbC 255 225 140 else grayC 220) (circleAt (sx + realToFrac ((c + 3) / 6) * sw) sy 16)
              , say ("PC" ++ show (k + 1) ++ "  c = " ++ fmtN c 1 ++ "  " ++ maybe "" (\(nm, cc) -> "~ " ++ nm ++ " (corr " ++ fmtN cc 2 ++ ")") (lookupMean k))
                    sx (sy - 12) 14 (grayC 220) ]
            | (k, c) <- zip [0 .. 4] (wCoef w), let (sx, sy, sw) = sliderGeo env k ]
  -- ---- 右：数字
  ++ [ say ("576 dims, " ++ show nFace ++ " faces, total variance = " ++ fmtN (pgTrace pc) 3) gx ty 15 (grayC 220)
     , say ("L1..L5 = " ++ joinWith ", " [ fmtN (vix (pgVals pc) k) 3 | k <- [0 .. 4] ]) gx (ty + 24) 15 (grayC 220)
     , say ("5 components: cumulative " ++ fmtN (100 * cum) 1 ++ "% (576 numbers -> 5)") gx (ty + 48) 15 (grayC 220)
     , sayWrap "The faces were made from 8 parameters, so the top components roughly match them (not always one to one: brightness and face width both change the 'bright area', so they mix)."
               gx (ty + 64) (envW env - gx - 30) 14 (rgbC 165 175 190)
     , say "Eigenfaces" 40 44 22 (grayC 240)
     , say "1-5 choose a component   Up/Down coefficient   drag sliders   0 all to 0   R new 200 people" 40 72 15 (rgbC 165 175 190) ]
  ++ [ say "computing..." 420 44 18 (rgbC 255 225 140) | Just _ <- [wPending w] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    a = wA w
    pc = aPc a
    ey = 460
    es = 3.6
    stepE = (fromIntegral faceN + 4) * es
    gx = envW env * 0.66
    ty = 130 + 4 * (fromIntegral faceN + 3) * 2.6 + 30
    cum = U.sum (U.take 5 (pgVals pc)) / pgTrace pc
    lookupMean k = if k < length (aMean a) then Just (nth ("", 0) (aMean a) k) else Nothing

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        hits = [ k | k <- [0 .. 4], let (sx, sy, sw) = sliderGeo env k, mx > sx - 10 && mx < sx + sw + 10 && abs (my - sy) < 14 ]
    in pure (case reverse hits of
               (k : _) -> dragTo (mx, my) w { wDragK = k, wSelK = k }
               [] -> w { wDragK = -1 })
  EventMotion p | wDragK w >= 0 -> pure (dragTo (toP5 env p) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragK = -1 }
  EventKey (SpecialKey k) Down _ _
    | k == KeyUp || k == KeyDown -> pure w { wCoef = setAt (wSelK w) (\c -> clampv (c + (if k == KeyUp then 0.5 else -0.5)) (-3) 3) (wCoef w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> pure w { wSelK = fromEnum c - fromEnum '1' }
    | c == '0' -> pure w { wCoef = [0, 0, 0, 0, 0] }
    | c `elem` "rR" -> do
        mapM_ cancelLater (wPending w)
        let s = wSeed w + 1
        job <- startLater forceA (analyse s)
        pure w { wSeed = s, wCoef = [0, 0, 0, 0, 0], wPending = Just job }
  _ -> pure w
  where
    dragTo (mx, _) w' = let (sx, _, sw) = sliderGeo env (wDragK w')
                        in w' { wCoef = setAt (wDragK w') (const (clampv (realToFrac ((mx - sx) / sw) * 6 - 3) (-3) 3)) (wCoef w') }

setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("trace of the Gram matrix = trace of the covariance matrix (total variance)", near tr trC (1e-12 * trC))
  , ("the 12 components are orthonormal (1e-9)", orthoError (pgVecs pc) < 1e-9)
  , ("C v = L v (checked as Xc^T (Xc v) / (n - 1) without making C; error <= 1e-8 x trace)",
      and [ U.and (U.zipWith (\x vj -> abs (x - l * vj) < 1e-8 * tr) (cv v) v) | (k, v) <- zip [0 ..] (V.toList (pgVecs pc)), let l = vix (pgVals pc) k ])
  , ("direct power iteration in 576 dimensions (300 steps) gives the same L1 (1e-8) and direction", near (dotv vp (cv vp)) (vix (pgVals pc) 0) (1e-8 * tr) && angleBetween vp (mrow (pgVecs pc) 0) < 1e-3)
  , ("error of rebuilding with 12 components sum |x - x_hat|^2 / (n - 1) = trace - (L1 + ... + L12)", near e12 (tr - U.sum (pgVals pc)) (1e-9 * tr))
  , ("each of the top 6 components has |corr| >= 0.6 with some parameter", all (\(_, c) -> abs c >= 0.6) (aMean a))
  , ("one of the top 6 components correlates strongly (>= 0.7) with 'mouth curve'", any (\(nm, c) -> nm == "mouth curve" && abs c >= 0.7) (aMean a)) ]
  where
    a = analysis8
    pc = aPc a
    n = fromIntegral nFace
    -- C v を C を作らずに：t_i = Xc_i·v、out_j = Σ_i t_i Xc_ij / (n − 1)
    cv v = V.foldl' (\out r -> let t = dotv r v in U.zipWith (\o rj -> o + (t * rj) / (n - 1)) out r) (U.replicate (U.length v) 0) (aXc a)
    tr = pgTrace pc
    trC = V.foldl' (\s r -> s + dotv r r) 0 (aXc a) / (n - 1)
    vp = withSeed 3 $ do
      v0 <- unitv <$> U.replicateM (nCols (aF a)) gaussRnd
      pure (foldl' (\v _ -> unitv (cv v)) v0 [1 .. 300 :: Int])
    e12 = V.foldl' (\s r -> let xh = reconstructRow (mrow (projectData (V.singleton r) (aMu a) (pgVecs pc) kPc) 0) (aMu a) (pgVecs pc)
                             in s + U.sum (U.zipWith (\x h -> (x - h) ^ (2 :: Int)) r xh)) 0 (aF a) / (n - 1)

-- ---------- やってみよう ----------
-- 1. PC1〜PC5 の係数を 1 つずつ ±3 まで動かし、顔の何が変わるか言葉にしよう（表示の「~」と合っている？）
-- 2. 全部の係数を ±3 にすると、ありえない顔になる？ それはなぜ？
-- 3. randomFaceParams の範囲を変えて（例：口の曲がりを 0 に固定）R を押すと、固有顔はどう変わる？
-- 4. 寄与率を見て、何本あれば「だいたいの顔」が作れるか考えよう（07 の復元）
-- 5. 本物の顔写真の固有顔は、位置合わせや明るさの影響を強く受ける。この合成データで「光の向き」の主成分を探そう

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
-- ---- パラメータで描く 24×24 の「顔」（0〜1 の明るさ、ぼかした縁）。実在のデータではなく合成
faceN :: Int
faceN = 24

data FaceP = FaceP { fpFaceW, fpEyeDist, fpEyeY, fpEyeSize, fpMouthCurve, fpMouthW, fpBright, fpLight :: !Double }

soft :: Double -> Double -> Double                  -- 縁をなめらかに（dd < 0 が内側）
soft dd w = clampv (0.5 - dd / w) 0 1

makeFace :: FaceP -> Vec
makeFace p = U.generate (faceN * faceN) px
  where
    c = fromIntegral (faceN - 1) / 2
    px i =
      let x = i `mod` faceN
          y = i `div` faceN
          u = (fromIntegral x - c) / c
          v = (fromIntegral y - c) / c
          fr = hypot (u / fpFaceW p) (v / 0.9) - 1
          inFace = soft (fr * 6) 1
          shade = fpBright p * (1 + 0.35 * fpLight p * u)          -- 光の向き（左右の明るさの差）
          val0 = 0.12 + inFace * (shade - 0.12)
          eye s = let ex = u - s * fpEyeDist p
                      ey = v - fpEyeY p
                      er = hypot (ex / 1.3) ey - fpEyeSize p
                  in inFace * soft (er * 14) 1 * shade * 0.85
          val1 = val0 - eye (-1) - eye 1                       -- 目
          mx = u / fpMouthW p
          my = v - (0.45 + fpMouthCurve p * (0.2 - 0.2 * mx * mx))  -- 口：放物線（fpMouthCurve > 0 で笑顔）
          val2 = if abs mx < 1 then val1 - inFace * soft ((abs my - 0.045) * 18) 1 * shade * 0.7 else val1
      in clampv val2 0 1

randomFaceParams :: R FaceP                          -- 乱数はこの順に引く
randomFaceParams = FaceP <$> rndRange 0.62 0.86 <*> rndRange 0.25 0.42 <*> rndRange (-0.32) (-0.12) <*> rndRange 0.08 0.15
                         <*> rndRange (-1) 1 <*> rndRange 0.25 0.45 <*> rndRange 0.55 0.95 <*> rndRange (-1) 1
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
