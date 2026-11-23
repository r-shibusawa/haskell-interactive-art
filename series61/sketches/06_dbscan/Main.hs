-- ============================================
-- 61-06 DBSCAN ─ 「点が混んでいるところ」をつないでクラスタにする（密度にもとづくクラスタリング）
--   半径 ε の円の中に minPts 個以上（自分も数える）点がある = コア点（大きい丸）
--   コア点の円の中にあるけれど自分はコアでない = 境界点（輪）、どのコア点の円にも入らない = ノイズ（灰色の ×）
--   コア点から ε 以内のコア点へ、次々に広がって 1 つのクラスタになる（アニメーション）。k を決めなくてよい・形は自由・外れ値はノイズに
--   近くの点は「ε の大きさのマス目」に分けて、周りの 3 × 3 マスだけ探す（全部の組を調べなくてよい）
--   画面では ε を eps と書く
-- 操作：スライダ / ← → ε / ↑↓ minPts / D データ / スペース 広がりを再生 / R 新しいデータ / クリック 点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import qualified Data.IntSet as IS
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
import qualified Data.IntMap.Strict as IM
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 300                                  -- ★ 点の数

dKinds :: [String]
dKinds = ["moons", "noise", "density", "circles"]

data World = World
  { wData   :: !DS
  , wDataNo :: !Int
  , wSeedD  :: !Int
  , wRes    :: !DB                          -- DBSCAN の結果
  , wKm     :: !KM                          -- くらべる k-means
  , wReveal :: !Int                         -- 広がりのアニメーション（何点まで見せたか）
  , wEps    :: !Slider
  , wMin    :: !Slider
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-06 DBSCAN" (world0 1280))
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      let w = (world0 (envW env)) { wTest = selfTest }
      kmInertia (wKm w) `seq` dbN (wRes w) `seq` pure w
  , skStep  = \_ _ w -> pure w { wReveal = min (length (dbOrder (wRes w))) (wReveal w + 3) }
  , skDraw  = draw
  , skEvent = event }

world0 :: Float -> World
world0 sw = newData (World (DS V.empty U.empty 0) 0 61 (dbscan V.empty 1 1) (KM V.empty U.empty 0 [] 0 []) 0
                       (makeSlider (sw - 360) 150 300 0.05 0.7 0.34 "eps (radius)" 2) (makeSlider (sw - 360) 215 300 1 20 6 "minPts" 0) [])

moonsNoise :: Int -> Word32 -> R DS          -- 三日月 + まばらな外れ値（正解 −1）
moonsNoise n seed = do
  d <- makeDataset "moons" n seed
  extra <- mapM (const (do x <- rnd; y <- rnd; pure (pt2 (-2.4 + 4.8 * x) (-1.6 + 3.2 * y), -1))) [1 .. floor (fromIntegral n * 0.06 :: Double) :: Int]
  pure (appendDS d extra)

newData :: World -> World
newData w = rerun w { wData = d, wKm = km }
  where
    kd = headOr "moons" (drop (wDataNo w) dKinds)
    sd = fromIntegral (wSeedD w)
    d = withSeed 1 (if kd == "moons" then moonsNoise nPts sd else makeDataset kd nPts sd)
    kTrue = IS.size (IS.fromList (filter (>= 0) (U.toList (dsTruth d))))
    km = withSeed 3 (kmeansBest (dsPts d) (max 1 kTrue) 10 True 100)

minPts :: World -> Int
minPts w = jsRoundI (slVal (wMin w))

rerun :: World -> World
rerun w = w { wRes = dbscan (dsPts (wData w)) (slVal (wEps w)) (minPts w), wReveal = 0 }

mainView :: Env -> G
mainView env = fitView 40 100 (envW env - 480) (envH env - 190) 0 0 4.4

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) ]
  -- いま広がっている点の ε の円
  ++ [ Color (rgbaC 255 240 150 160) (ringAt (gX v (ptX p)) (gY v (ptY p)) (2 * rpx) 1.5)
     | wReveal w < length ord, wReveal w > 0, let p = pts V.! headOr 0 (drop (wReveal w - 1) ord) ]
  -- マウスの位置の ε の円（コアかどうか確かめる）
  ++ [ Color (rgbaC 255 255 255 70) (ringAt mx my (2 * rpx) 1) | inBox v mx my ]
  ++ map pointPic [0 .. n - 1]
  -- ---- 右のパネル
  ++ [ drawSlider (wEps w), drawSlider (wMin w)
     , say ("clusters " ++ show (dbN res)) bx 270 15 (grayC 220)
     , say ("core " ++ show nCore ++ "   border " ++ show (n - nCore - nNoise) ++ "   noise " ++ show nNoise) bx 290 15 (grayC 220)
     , say ("DBSCAN ARI (vs truth) = " ++ fmt 3 (ariScore (dbLabels res) (dsTruth d))) bx 330 15 (rgbC 110 230 190)
     , say ("k-means ARI = " ++ fmt 3 (ariScore (kmLabels (wKm w)) (dsTruth d))) bx 354 15 (rgbC 255 160 150)
     , Color (grayC 70) (frameTL 1 (pX v2) (pY v2) (pW v2) (pH v2))
     , drawPts v2 pts (Just (kmLabels (wKm w))) 4 255
     , sayWrap "Compare: k-means (k = true number, noise must join some cluster)" (pX v2) (pY v2 + pH v2 + 4) 300 14 (grayC 170)
  -- ---- 文字
     , say ("DBSCAN   eps = " ++ fmt 2 (slVal (wEps w)) ++ "   minPts = " ++ show (minPts w) ++ "   -> " ++ show (dbN res) ++ " clusters, " ++ show nNoise ++ " noise points") 40 40 20 (grayC 240)
     , say ("Slider or <- -> eps   Up Down minPts   D data (" ++ dataName (headOr "" (drop (wDataNo w) dKinds)) ++ ")   Space replay   R new data   click: add point") 40 66 15 (grayC 170)
     , sayWrap "Filled = core   ring = border   x = noise.  White circle = eps around the mouse. Too small eps: all noise; too large: everything joins into one." 40 (envH env - 64) (pW v) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    d = wData w
    pts = dsPts d
    n = V.length pts
    res = wRes w
    ord = dbOrder res
    v = mainView env
    v2 = fitView bx 390 300 220 0 0 4.6
    bx = envW env - 360
    (mx, my) = mouseP env
    rpx = (gX v 1 - gX v 0) * f2 (slVal (wEps w))
    shown = IS.fromList (take (wReveal w) ord)
    nCore = U.length (U.filter id (dbCore res))
    nNoise = U.length (U.filter (< 0) (dbLabels res))
    pointPic i
      | l < 0 = Color (rgbC 130 130 145) (Pictures [lineW 1.5 (x - 4, y - 4) (x + 4, y + 4), lineW 1.5 (x - 4, y + 4) (x + 4, y - 4)])
      | not (IS.member i shown) = Color (rgbC 90 90 105) (circleAt x y 6)
      | dbCore res U.! i = Color (palC l) (circleAt x y 10)
      | otherwise = Color (palC l) (ringAt x y 9 2)
      where
        p = pts V.! i
        x = gX v (ptX p)
        y = gY v (ptY p)
        l = if i < U.length (dbLabels res) then dbLabels res U.! i else -1

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p0 ->
    let (mx, my) = toP5 env p0
        v = mainView env
    in case (sliderPress mx my (wEps w), sliderPress mx my (wMin w)) of
         (Just s, _) -> rerun w { wEps = s }
         (_, Just s) -> rerun w { wMin = s }
         _ | inBox v mx my ->
               let p = pt2 (invX v mx) (invY v my)
                   km = wKm w
                   w' = rerun w { wData = appendDS (wData w) [(p, -1)], wKm = km { kmLabels = U.snoc (kmLabels km) (nearestCenter p (kmCenters km)) } }
               in w' { wReveal = length (dbOrder (wRes w')) }
           | otherwise -> w
  EventMotion p0 | slDrag (wEps w) || slDrag (wMin w) ->
    let (mx, _) = toP5 env p0
        w' = rerun w { wEps = sliderDrag mx (wEps w), wMin = sliderDrag mx (wMin w) }
    in w' { wReveal = length (dbOrder (wRes w')) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wEps = sliderRelease (wEps w), wMin = sliderRelease (wMin w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> nudgeEps 0.01
  EventKey (SpecialKey KeyLeft) Down _ _ -> nudgeEps (-0.01)
  EventKey (SpecialKey KeyUp) Down _ _ -> nudgeMin 1
  EventKey (SpecialKey KeyDown) Down _ _ -> nudgeMin (-1)
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wReveal = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> newData w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  _ -> w
  where
    nudgeEps dv = let s = wEps w in rerun w { wEps = s { slVal = clampv (slLo s) (slHi s) (slVal s + dv) } }
    nudgeMin dv = let s = wMin w in rerun w { wMin = s { slVal = clampv (slLo s) (slHi s) (slVal s + dv) } }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
-- 答え合わせ用：全部の組の距離（O(n²)）→ コア点 → コア点どうしのつながり（小さい番号から）→ 境界点はいちばん小さい番号のクラスタへ
dbscanBrute :: Pts -> Double -> Int -> (U.Vector Int, U.Vector Bool)
dbscanBrute pts eps mp = (U.generate n final, core)
  where
    n = V.length pts
    nb = V.generate n (\i -> [ j | j <- [0 .. n - 1], edist (pts V.! i) (pts V.! j) <= eps ])
    core = U.generate n (\i -> length (nb V.! i) >= mp)
    lab0 = fst (foldl' start (U.replicate n (-1), 0 :: Int) [0 .. n - 1])
    start (lab, c) i
      | not (core U.! i) || lab U.! i >= 0 = (lab, c)
      | otherwise = (dfs [i] (lab U.// [(i, c)]), c + 1)
      where dfs [] l = l
            dfs (x : st) l = let new = [ j | j <- nb V.! x, core U.! j, l U.! j < 0 ]
                             in dfs (reverse new ++ st) (l U.// [ (j, c) | j <- new ])
    final i | core U.! i = lab0 U.! i
            | otherwise = foldl' (\b j -> if core U.! j && (b < 0 || lab0 U.! j < b) then lab0 U.! j else b) (-1) (nb V.! i)

selfTest :: [(String, Bool)]
selfTest =
  [ ("grid DBSCAN = all-pairs DBSCAN (8 data sets and eps/minPts, labels identical)", and [ a | (a, _, _, _) <- trials ])
  , ("core point <=> at least minPts points within eps (brute-force count)", and [ b | (_, b, _, _) <- trials ])
  , ("no core point near a noise point; every cluster point is within eps of a core point of its cluster", and [ c | (_, _, c, _) <- trials ])
  , ("two core points within eps are always in the same cluster", and [ e | (_, _, _, e) <- trials ])
  , ("moons: DBSCAN (eps = 0.34, minPts = 6) ARI " ++ fmt 3 aD ++ " >= 0.95, k-means " ++ fmt 3 aK ++ " < 0.6", aD >= 0.95 && aK < 0.6)
  , ("huge eps gives 1 cluster; minPts = 1 gives no noise (all core)", dbN one == 1 && U.all (>= 0) (dbLabels allc) && U.all id (dbCore allc)) ]
  where
    trials = map trial [0 .. 7 :: Int]
    trial t =
      let sd = fromIntegral (600 + t)
          d = withSeed 1 (if odd t then moonsNoise 160 sd else makeDataset (headOr "" (drop (t `mod` 4) dKinds)) 160 sd)
          pts = dsPts d
          n = V.length pts
          eps = 0.1 + 0.05 * fromIntegral (t `mod` 5)
          mp = 3 + t `mod` 6
          r = dbscan pts eps mp
          (bl, _) = dbscanBrute pts eps mp
          labs = dbLabels r
          cores = dbCore r
          near' i j = edist (pts V.! i) (pts V.! j) <= eps
          coreNb i = [ j | j <- [0 .. n - 1], cores U.! j, near' i j ]
      in ( labs == bl
         , and [ cores U.! i == (length (filter (near' i) [0 .. n - 1]) >= mp) | i <- [0 .. n - 1] ]
         , and [ if labs U.! i < 0 then null (coreNb i) else any (\j -> labs U.! j == labs U.! i) (coreNb i) | i <- [0 .. n - 1] ]
         , and [ labs U.! j == labs U.! i | i <- [0 .. n - 1], cores U.! i, j <- coreNb i ] )
    mo = withSeed 1 (makeDataset "moons" nPts 61)
    r5 = dbscan (dsPts mo) 0.34 6
    k2 = withSeed 2 (kmeansBest (dsPts mo) 2 10 True 100)
    aD = ariScore (dbLabels r5) (dsTruth mo)
    aK = ariScore (kmLabels k2) (dsTruth mo)
    one = dbscan (dsPts mo) 100 5
    allc = dbscan (dsPts mo) 0.1 1

-- ---------- やってみよう ----------
-- 1. ε を少しずつ大きくして、クラスタの数が 多い → 2 → 1 と変わるところを探そう
-- 2. minPts を大きくすると、境界点とノイズはどうなる？
-- 3. D で「密度がちがう」にすると、1 つの ε では両方のかたまりをうまく取れない。これが DBSCAN の弱点（→ OPTICS・HDBSCAN）
-- 4. 同心円（D）は ε と minPts をどうすれば分けられる？
-- 5. 何もないところをクリックして点を足すと、ノイズ（×）になる。たくさん足してコア点にしてみよう

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

-- ---- DBSCAN：半径 eps の中に minPts 個以上（自分も数える）ある点 = コア点。コア点どうしが eps でつながって 1 つのクラスタ
--   ラベル −1 = ノイズ。近くの点は「eps の大きさのマス目」で探す（周り 3 × 3 マスだけ見ればよい）
-- 結果：ラベル、コア点か、広がっていく順（アニメーション用）、クラスタの数、各点の近くの点
data DB = DB { dbLabels :: !(U.Vector Int), dbCore :: !(U.Vector Bool), dbOrder :: ![Int], dbN :: !Int, dbNb :: !(V.Vector [Int]) }

gridKey :: Int -> Int -> Int
gridKey cx cy = cx * 100003 + cy

cellOf :: Double -> Double -> Int
cellOf v eps = floorI (v / eps)

buildGrid :: Pts -> Double -> IM.IntMap [Int]
buildGrid pts eps = IM.map reverse (IM.fromListWith (++) [ (gridKey (cellOf (ptX p) eps) (cellOf (ptY p) eps), [i]) | (i, p) <- zip [0 ..] (V.toList pts) ])

neighborsGrid :: IM.IntMap [Int] -> Pts -> Int -> Double -> [Int]
neighborsGrid g pts i eps = sort [ j | dx <- [-1, 0, 1], dy <- [-1, 0, 1], j <- IM.findWithDefault [] (gridKey (cx + dx) (cy + dy)) g, sqd p (pts V.! j) <= eps * eps ]
  where
    p = pts V.! i
    cx = cellOf (ptX p) eps
    cy = cellOf (ptY p) eps

dbscan :: Pts -> Double -> Int -> DB
dbscan pts eps minPts = runST $ do
  lab <- UM.replicate n (-2 :: Int)
  (ordR, c) <- foldM (\(ord, c) i -> do
      li <- UM.read lab i
      if li /= -2 || not (core U.! i) then pure (ord, c) else do
        UM.write lab i c
        let bfs [] o = pure o
            bfs (q : qs) o = do
              (o', new) <- foldM (\(oa, na) j -> do
                  lj <- UM.read lab j
                  if lj >= 0 then pure (oa, na) else do
                    UM.write lab j c
                    pure (j : oa, if core U.! j then j : na else na)) (o, []) (nb V.! q)
              bfs (qs ++ reverse new) o'
        o2 <- bfs [i] (i : ord)
        pure (o2, c + 1)) ([], 0) [0 .. n - 1]
  labs <- U.freeze lab
  pure (DB (U.map (\l -> if l == -2 then -1 else l) labs) core (reverse ordR) c nb)
  where
    n = V.length pts
    e = max 1e-6 eps
    g = buildGrid pts e
    nb = V.generate n (\i -> neighborsGrid g pts i e)
    core = U.generate n (\i -> length (nb V.! i) >= minPts)

-- ---- かんたんなスライダ（ドラッグで値を変える）。座標は p5.js と同じ（左上が原点）
data Slider = Slider { slX, slY, slW :: !Float, slLo, slHi, slVal :: !Double, slName :: !String, slDigits :: !Int, slDrag :: !Bool }

makeSlider :: Float -> Float -> Float -> Double -> Double -> Double -> String -> Int -> Slider
makeSlider x y w lo hi val name digits = Slider x y w lo hi val name digits False

drawSlider :: Slider -> Picture
drawSlider s = Pictures
  [ Color (grayC 90) (lineW 4 (slX s, slY s) (slX s + slW s, slY s))
  , Color (rgbC 176 140 255) (lineW 4 (slX s, slY s) (slX s + u * slW s, slY s))
  , Color (grayC (if slDrag s then 255 else 220)) (circleAt (slX s + u * slW s) (slY s) 16)
  , say (slName s ++ " = " ++ fmt (slDigits s) (slVal s)) (slX s) (slY s - 12) 14 (grayC 200) ]
  where u = f2 ((slVal s - slLo s) / (slHi s - slLo s))

-- 押した場所がスライダの上なら、つかんで値を変える（Nothing = スライダの外）
sliderPress :: Float -> Float -> Slider -> Maybe Slider
sliderPress mx my s
  | abs (my - slY s) < 14 && mx > slX s - 10 && mx < slX s + slW s + 10 = Just (sliderDrag mx s { slDrag = True })
  | otherwise = Nothing

sliderDrag :: Float -> Slider -> Slider
sliderDrag mx s
  | not (slDrag s) = s
  | otherwise = let v = slLo s + clampv 0 1 (realToFrac ((mx - slX s) / slW s)) * (slHi s - slLo s)
                in s { slVal = if slDigits s == 0 then jsRound v else v }

sliderRelease :: Slider -> Slider
sliderRelease s = s { slDrag = False }

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
