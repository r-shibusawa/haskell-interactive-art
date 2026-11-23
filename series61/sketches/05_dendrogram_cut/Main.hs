-- ============================================
-- 61-05 デンドログラムと切る高さ ─ 04 の合体の記録を「木」にする。縦 = 合体した高さ（グループ間の距離）
--   横の線（切る高さ h）で木を切ると、その下にある枝の数 = クラスタの数。長い縦の枝 = はっきり離れたグループ
--   コーフェン距離 c_ij = i と j が初めて同じグループになった高さ。もとの距離 d_ij との相関（コーフェン相関係数）が
--   1 に近いほど、木がもとの距離をよく保っている。単連結の木はシリーズ 9 の 13（H0 のパーシステンス）と同じもの
-- 操作：マウスを木の上で上下 / ↑↓ 切る高さ / 1〜4 連結法 / D データ / R 新しいデータ / クリック（左の図）点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 60                                   -- ★ 点の数

dKinds :: [String]
dKinds = ["blobs", "noise", "chain", "moons"]

data World = World
  { wData   :: !DS
  , wMethod :: !Int                         -- 0 単連結 / 1 完全連結 / 2 群平均 / 3 ウォード法
  , wMerges :: ![Merge]
  , wOrder  :: ![Int]                       -- 葉の並び
  , wCoph   :: ![Double]                    -- 4 つの連結法のコーフェン相関係数
  , wCutH   :: !Double                      -- ★ 切る高さ
  , wDataNo :: !Int
  , wSeedD  :: !Int
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-05 Dendrogram cut" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      sum (wCoph w) `seq` pure w
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = newData (World (DS V.empty U.empty 0) 2 [] [] [] 0.8 0 51 [])

newData :: World -> World
newData w = rebuild w { wData = withSeed 1 (makeDataset (headOr "blobs" (drop (wDataNo w) dKinds)) nPts (fromIntegral (wSeedD w))) }

rebuild :: World -> World
rebuild w = w { wMerges = mg, wOrder = leafOrder mg n
              , wCoph = [ copheneticCorr pts (agglomerate pts m) | m <- [0 .. 3] ]
              , wCutH = clampv 0 (topH mg * 1.02) (wCutH w) }
  where
    pts = dsPts (wData w)
    n = V.length pts
    mg = agglomerate pts (wMethod w)

topH :: [Merge] -> Double                   -- いちばん上の合体の高さ
topH mg = if null mg then 1 else mH (last mg)

-- ★ 木の形：葉の横位置 = 並び順、つなぎ目 = 子の真ん中、高さ = 合体の高さ
layout :: World -> Int -> (IM.IntMap Double, IM.IntMap Double)
layout w n = foldl' add (IM.fromList (zip (wOrder w) [0 ..]), IM.fromList [ (i, 0) | i <- [0 .. n - 1] ]) (zip [0 ..] (wMerges w))
  where add (xs, hs) (t, e) = (IM.insert (n + t) ((IM.findWithDefault 0 (mA e) xs + IM.findWithDefault 0 (mB e) xs) / 2) xs, IM.insert (n + t) (mH e) hs)

-- 左の図と木の四角
leftView :: Env -> G
leftView env = fitView 40 100 (envW env * 0.36) (envH env * 0.62) 0 0 5.0

treeBox :: Env -> World -> G
treeBox env w = G tx 110 (envW env - tx - 40) (envH env - 260) (-0.5) (fromIntegral n - 0.5) 0 (topH (wMerges w) * 1.08)
  where
    v = leftView env
    tx = pX v + pW v + 80
    n = V.length (dsPts (wData w))

-- 高さ h で切った結果：ラベル、クラスタの数、シルエット係数
cutInfo :: World -> (U.Vector Int, Int, Double)
cutInfo w = (lab, k, sil)
  where
    pts = dsPts (wData w)
    n = V.length pts
    lab = cutTreeH (wMerges w) n (wCutH w)
    k = IS.size (IS.fromList (U.toList lab))
    sil = if k >= 2 && k < n then meanOf (U.toList (silhouetteVals pts lab)) else 0

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v))
  , drawPts v pts (Just lab) 9 255 ]
  -- ---- デンドログラム
  ++ concat [ [ Color (branchCol c) (lineW 2 (gX tr (xOf c), gY tr (hOf c)) (gX tr (xOf c), yp)) | c <- [mA e, mB e] ]
              ++ [ Color (nodeCol p) (lineW 2 (gX tr (xOf (mA e)), yp) (gX tr (xOf (mB e)), yp)) ]
            | (t, e) <- zip [0 ..] (wMerges w), let p = n + t, let yp = gY tr (mH e)
            , let branchCol c = if colOf c >= 0 && colOf p < 0 then palC (colOf c) else nodeCol p ]
  ++ [ Color (rgbC 255 100 120) (lineW 2 (x, yc) (min (x + 8) (pX tr + pW tr), yc)) | x <- [pX tr - 10, pX tr + 4 .. pX tr + pW tr] ]
  ++ [ sayR ("h = " ++ fmt 2 (wCutH w)) (pX tr - 14) (yc + 5) 14 (rgbC 255 130 150) ]
  ++ [ sayR (fmt 2 val) (pX tr - 14) (gY tr val + (if val == 0 then 5 else 19)) 14 (grayC 160) | val <- [0, y1 tr / 2, y1 tr] ]
  -- ---- コーフェン相関の表
  ++ [ say "Cophenetic correlation (how well the tree keeps the distances)" 40 ty 15 (grayC 215) ]
  ++ concat [ [ say (show (m + 1) ++ " " ++ linkName m) 40 y 15 c, say (fmt 3 cc) 170 y 15 c
              , Color (if m == wMethod w then rgbC 176 140 255 else rgbC 90 90 110) (rectTL 230 (y - 12) (f2 (max 0 cc) * 160) 12) ]
            | (m, cc) <- zip [0 ..] (wCoph w), let y = ty + 26 + fromIntegral m * 24
            , let c = if m == wMethod w then rgbC 200 180 255 else grayC 180 ]
  -- ---- 文字
  ++ [ say ("Dendrogram cut   " ++ linkName (wMethod w) ++ "   h = " ++ fmt 3 (wCutH w) ++ " -> " ++ show k ++ " clusters   silhouette " ++ fmt 3 sil ++ "   ARI (vs truth) " ++ fmt 3 (ariScore lab (dsTruth (wData w)))) 40 40 18 (grayC 240)
     , say ("Move mouse on the tree / Up Down: cut height   1-4 linkage   D data (" ++ dataName (headOr "" (drop (wDataNo w) dKinds)) ++ ")   R new data   click left: add point") 40 66 15 (grayC 170)
     , sayWrap "Branches joined below the red dashed line = one cluster (same colour). Grey = merges above the line. Cut where the vertical branches are long: moving h a little does not change the answer." (pX tr) (pY tr + pH tr + 26) (pW tr) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    pts = dsPts (wData w)
    n = V.length pts
    v = leftView env
    tr = treeBox env w
    (lab, k, sil) = cutInfo w
    (lxs, lhs) = layout w n
    xOf c = IM.findWithDefault 0 c lxs
    hOf c = IM.findWithDefault 0 c lhs
    -- 色：葉はクラスタの色、破線より下で同じ色どうしが合体したらその色、それ以外は −1（灰色）
    cols = foldl' (\m (t, e) -> let ca = IM.findWithDefault (-1) (mA e) m; cb = IM.findWithDefault (-1) (mB e) m
                                in IM.insert (n + t) (if mH e <= wCutH w && ca == cb then ca else -1) m)
                  (IM.fromList (zip [0 ..] (U.toList lab))) (zip [0 ..] (wMerges w))
    colOf c = IM.findWithDefault (-1) c cols
    nodeCol p = if colOf p >= 0 then palC (colOf p) else rgbC 170 170 185
    yc = gY tr (wCutH w)
    ty = pY v + pH v + 36

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventMotion p -> let (mx, my) = toP5 env p; tr = treeBox env w
                   in if inBox tr mx my then w { wCutH = clampv 0 (y1 tr) (invY tr my) } else w
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p; v = leftView env; d = wData w
    in if inBox v mx my then rebuild w { wData = appendDS d [(pt2 (invX v mx) (invY v my), 0)] } else w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wCutH = min (top * 1.05) (wCutH w + top / 60) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wCutH = max 0 (wCutH w - top / 60) }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> rebuild w { wMethod = fromEnum c - fromEnum '1' }
    | c `elem` "dD" -> newData w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  _ -> w
  where top = topH (wMerges w)

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 55・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("cophenetic correlations of the 4 linkages are finite and in [-1, 1]: " ++ unwords (map (fmt 3) cc), all (\x -> not (isNaN x || isInfinite x) && x >= -1 && x <= 1) cc)
  , ("cophenetic distance is an ultrametric: c_ij <= max(c_ik, c_kj) (all triples)", ultra)
  , ("cut at height h: number of clusters = n - (merges with height <= h), same as cutting at k", kOk)
  , ("single linkage cut at h = connected components of the graph 'distance <= h' (brute force)", ccOk)
  , ("leaf order is a permutation and every k gives contiguous clusters (no crossing branches)", ordOk)
  , ("single-linkage cophenetic distance = minimax path distance (brute force)", minimaxOk) ]
  where
    (pts, hs1, hs2) = withSeed 55 $ do
      ps <- mapM (const (do a <- rnd; b <- rnd; pure (pt2 (a * 3) (b * 2)))) [1 .. 28 :: Int]
      let mg0 = agglomerate (V.fromList ps) 2
          sl0 = agglomerate (V.fromList ps) 0
      a <- mapM (const ((\u -> u * mH (last mg0) * 1.1) <$> rnd)) [1 .. 40 :: Int]
      b <- mapM (const ((* mH (last sl0)) <$> rnd)) [1 .. 15 :: Int]
      pure (V.fromList ps, a, b)
    n = V.length pts
    cc = [ copheneticCorr pts (agglomerate pts m) | m <- [0 .. 3] ]
    ultra = and [ c U.! (i * n + j) <= max (c U.! (i * n + k)) (c U.! (k * n + j)) + 1e-12
                | m <- [0 .. 3], let c = copheneticMatrix (agglomerate pts m) n, i <- [0 .. n - 1], j <- [0 .. n - 1], k <- [0 .. n - 1] ]
    mg = agglomerate pts 2
    kOk = and [ IS.size (IS.fromList (U.toList lab)) == n - cnt && ariScore lab (cutTreeK mg n (n - cnt)) == 1
              | h <- hs1, let lab = cutTreeH mg n h, let cnt = length (filter (\e -> mH e <= h) mg) ]
    sl = agglomerate pts 0
    ccOk = and [ ariScore (cutTreeH sl n h) (components h) == 1 | h <- hs2 ]
    components h = U.fromList [ IS.findMin (reach h (IS.singleton i) [i]) | i <- [0 .. n - 1] ]   -- 連結成分（いちばん小さい番号を代表に）
    reach _ seen [] = seen
    reach h seen (x : st) = let new = [ y | y <- [0 .. n - 1], not (IS.member y seen), edist (pts V.! x) (pts V.! y) <= h ]
                            in reach h (foldr IS.insert seen new) (new ++ st)
    ordOk = and [ IS.size (IS.fromList od) == n && and [ contiguous [ lab U.! i | i <- od ] | k <- [1 .. n], let lab = cutTreeK g n k ]
                | m <- [0 .. 3], let g = agglomerate pts m, let od = leafOrder g n ]
    contiguous sq = go IS.empty (-1) sq
      where go _ _ [] = True
            go seen prev (x : xs) = not (x /= prev && IS.member x seen) && go (IS.insert x seen) x xs
    d0 = U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in edist (pts V.! i) (pts V.! j))
    dmm = foldl' (\d k -> U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in min (d U.! ij) (max (d U.! (i * n + k)) (d U.! (k * n + j))))) d0 [0 .. n - 1]
    cs = copheneticMatrix sl n
    minimaxOk = and [ i == j || abs (dmm U.! (i * n + j) - cs U.! (i * n + j)) < 1e-12 | i <- [0 .. n - 1], j <- [0 .. n - 1] ]

-- ---------- やってみよう ----------
-- 1. 木の上でマウスをゆっくり上下させ、クラスタの数がどこで変わるか見よう。長い縦の枝はどこ？
-- 2. 1〜4 で連結法を変え、コーフェン相関係数がいちばん大きいのはどれか確かめよう（群平均が大きくなりやすい）
-- 3. D で「かたまり + 外れ値」に。単連結では外れ値が最後まで 1 人ぼっちの枝になる
-- 4. シルエット（上の数字）がいちばん大きくなる高さを探そう。02 の k の選び方とくらべて
-- 5. シリーズ 9 の 13 クラスタの木は、単連結の木を「パーシステンス」として見たもの

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

-- ---- シルエット係数 s_i = (b − a) / max(a, b)（ちょうど O(n²)）
--   a = 同じクラスタの点までの平均距離、b = いちばん近い別クラスタの点までの平均距離。1 点だけのクラスタは s = 0
silhouetteVals :: Pts -> U.Vector Int -> U.Vector Double
silhouetteVals pts labels = U.generate n sil
  where
    n = V.length pts
    k = U.foldl' (\m l -> max m (l + 1)) 0 labels
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) (U.filter (>= 0) labels))
    sil i
      | own < 0 || cnt U.! own <= 1 = 0                -- ノイズ（−1）と 1 点だけのクラスタは 0
      | null bs = 0
      | otherwise = let m = max a b in if m > 0 then (b - a) / m else 0
      where
        own = labels U.! i
        p = pts V.! i
        sums = U.accumulate (+) (U.replicate k 0) (U.generate n (\j -> let l = labels U.! j in if j /= i && l >= 0 then (l, edist p (pts V.! j)) else (0, 0)))
        a = sums U.! own / fromIntegral (cnt U.! own - 1)
        bs = [ sums U.! c / fromIntegral (cnt U.! c) | c <- [0 .. k - 1], c /= own, cnt U.! c > 0 ]
        b = minimum' bs
    minimum' = foldl' min (1 / 0)

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

-- ---- 凝集型クラスタリング（ランス-ウィリアムズの更新式）。Merge { 合体した 2 つの番号, 高さ, 大きさ }
--   葉 = 0〜n−1、t 番目の合体でできたクラスタ = n + t。ウォード法は距離² で更新し、高さ = √（更新した値）
data Merge = Merge { mA :: !Int, mB :: !Int, mH :: !Double, mSize :: !Int }

linkName :: Int -> String
linkName m = headOr "" (drop m ["single", "complete", "average", "Ward"])

agglomerate :: Pts -> Int -> [Merge]        -- method: 0 単連結 / 1 完全連結 / 2 群平均 / 3 ウォード法
agglomerate pts method
  | n < 2 = []
  | otherwise = runST $ do
      dm <- U.thaw (U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in if method == 3 then sqd (pts V.! i) (pts V.! j) else edist (pts V.! i) (pts V.! j)))
      size <- UM.replicate n (1 :: Int)
      ids <- U.thaw (U.enumFromN 0 n)
      act <- UM.replicate n True
      let -- いちばん近い 2 つ（i < j）を探す
          findBest !i !bi !bj !bd
            | i >= n = pure (bi, bj, bd)
            | otherwise = do
                ai <- UM.unsafeRead act i
                if ai then inner i (i + 1) bi bj bd else findBest (i + 1) bi bj bd
          inner !i !j !bi !bj !bd
            | j >= n = findBest (i + 1) bi bj bd
            | otherwise = do
                aj <- UM.unsafeRead act j
                if not aj then inner i (j + 1) bi bj bd else do
                  d <- UM.unsafeRead dm (i * n + j)
                  if d < bd then inner i (j + 1) i j d else inner i (j + 1) bi bj bd
      forM [0 .. n - 2] $ \t -> do
        (bi0, bj0, bd) <- findBest 0 (-1) (-1) (1 / 0)
        (bi, bj) <- if bi0 >= 0 then pure (bi0, bj0) else do      -- 距離が全部 ∞ のとき（ふつうは起きない）
          alive <- filterM' (UM.unsafeRead act) [0 .. n - 1]
          pure (case alive of { (x : y : _) -> (x, y); _ -> (0, 1) })
        ni <- UM.unsafeRead size bi
        nj <- UM.unsafeRead size bj
        ia <- UM.unsafeRead ids bi
        ib <- UM.unsafeRead ids bj
        forM_ [0 .. n - 1] $ \k -> do
          ak <- UM.unsafeRead act k
          when (ak && k /= bi && k /= bj) $ do
            dik <- UM.unsafeRead dm (bi * n + k)
            djk <- UM.unsafeRead dm (bj * n + k)
            nk <- UM.unsafeRead size k
            let fi = fromIntegral ni; fj = fromIntegral nj; fk = fromIntegral nk
                v = case method of
                  0 -> min dik djk
                  1 -> max dik djk
                  2 -> (fi * dik + fj * djk) / (fi + fj)
                  _ -> ((fi + fk) * dik + (fj + fk) * djk - fk * bd) / (fi + fj + fk)
            UM.unsafeWrite dm (bi * n + k) v
            UM.unsafeWrite dm (k * n + bi) v
        UM.unsafeWrite size bi (ni + nj)
        UM.unsafeWrite act bj False
        UM.unsafeWrite ids bi (n + t)
        pure (Merge ia ib (if method == 3 then sqrt (max 0 bd) else bd) (ni + nj))
  where
    n = V.length pts
    filterM' f = foldr (\x acc -> do { ok <- f x; xs <- acc; pure (if ok then x : xs else xs) }) (pure [])

membersOf :: [Merge] -> Int -> V.Vector [Int]   -- 番号 → 中の点のリスト
membersOf merges n = mem
  where mem = V.fromList (map (: []) [0 .. n - 1] ++ [ (mem V.! mA m) ++ (mem V.! mB m) | m <- merges ])

-- はじめの count 回だけ合体したときのラベル（0, 1, 2… 小さい点の番号順）
labelsAfter :: [Merge] -> Int -> Int -> U.Vector Int
labelsAfter merges n count = U.replicate n 0 U.// [ (i, k) | (k, g) <- zip [0 ..] groups, i <- g ]
  where
    mem = membersOf merges n
    alive = foldl' (\s (t, m) -> IS.insert (n + t) (IS.delete (mB m) (IS.delete (mA m) s))) (IS.fromList [0 .. n - 1]) (zip [0 ..] (take count merges))
    groups = map snd (sortBy (\x y -> compare (fst x) (fst y)) [ (foldl' min maxBound g, g) | c <- IS.toList alive, let g = mem V.! c ])

cutTreeK :: [Merge] -> Int -> Int -> U.Vector Int   -- k 個のクラスタになるように切る
cutTreeK merges n k = labelsAfter merges n (max 0 (min (n - 1) (n - k)))

cutTreeH :: [Merge] -> Int -> Double -> U.Vector Int   -- 高さ h で切る
cutTreeH merges n h = labelsAfter merges n (length (takeWhile (\m -> mH m <= h) merges))

leafOrder :: [Merge] -> Int -> [Int]        -- デンドログラムの葉の並び（枝が交差しない順）
leafOrder merges n
  | n <= 1 = [0 .. n - 1]
  | otherwise = go [2 * n - 2]
  where
    mv = V.fromList merges
    go [] = []
    go (v : st)
      | v < n = v : go st
      | v - n < V.length mv = let m = mv V.! (v - n) in go (mA m : mB m : st)
      | otherwise = go st

copheneticMatrix :: [Merge] -> Int -> U.Vector Double   -- c_ij = i と j が初めて同じクラスタになった高さ（n × n）
copheneticMatrix merges n = runST $ do
  c <- UM.replicate (n * n) 0
  forM_ merges $ \m -> forM_ (mem V.! mA m) $ \x -> forM_ (mem V.! mB m) $ \y -> do
    UM.write c (x * n + y) (mH m)
    UM.write c (y * n + x) (mH m)
  U.freeze c
  where mem = membersOf merges n

pearson :: U.Vector Double -> U.Vector Double -> Double
pearson xs ys = if sxx > 0 && syy > 0 then clampv (-1) 1 (sxy / sqrt (sxx * syy)) else 0
  where
    n = U.length xs
    mx = if n == 0 then 0 else U.foldl' (+) 0 xs / fromIntegral n
    my = if n == 0 then 0 else U.foldl' (+) 0 ys / fromIntegral n
    (sxy, sxx, syy) = U.foldl' (\(a1, a2, a3) (x, y) -> let a = x - mx; b = y - my in (a1 + a * b, a2 + a * a, a3 + b * b)) (0, 0, 0) (U.zip xs ys)

copheneticCorr :: Pts -> [Merge] -> Double  -- もとの距離とコーフェン距離の相関
copheneticCorr pts merges = pearson (U.fromList (map fst prs)) (U.fromList (map snd prs))
  where
    n = V.length pts
    c = copheneticMatrix merges n
    prs = [ (edist (pts V.! i) (pts V.! j), c U.! (i * n + j)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]

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
