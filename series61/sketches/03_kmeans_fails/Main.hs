-- ============================================
-- 61-03 k-means が苦手な形 ─ k-means は「中心からの距離」だけで分ける → 仲間の形はいつも凸（ボロノイの領域）で、丸くて同じ大きさが前提
--   三日月・同心円・細長い・密度がちがう・大きさがちがう、の 5 つで k-means（k = 本当の数、k-means++ で 10 回の最良）を試す
--   左 = 正解、右 = k-means。調整ランド指数 ARI（1 = 完全に一致、0 = でたらめと同じ）で比べる（測り方は 10）
--   おもしろいところ：k-means の J は、正解の分け方の J より小さい！ k-means は「自分の目的」では正しく、目的がちがうだけ
-- 操作：1〜6 データ / スペース やり直し / クリック 点を足す / R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kinds :: [String]
kinds = ["moons", "circles", "aniso", "density", "sizes", "blobs"]

-- なぜ失敗するか（画面に出す説明）
why :: String -> String
why k = case k of
  "moons" -> "Moons are not convex. Cutting by distance to a centre gives the tip of one moon to the other -> connect by density (06 DBSCAN) or think in a graph (09)"
  "circles" -> "The inner and the outer ring have the same centre, so distance to a centre cannot split them -> use links between near points (04 single linkage, 06, 09)"
  "aniso" -> "The ends of a long blob are closer to the centre of the neighbouring blob -> a Gaussian mixture with covariances (ellipses) (08)"
  "density" -> "With different densities, points of the big sparse blob are easily taken by the dense ones -> the Gaussian mixture of 08, or density-based 06 / 07"
  "sizes" -> "With different sizes (spreads) the border is drawn in the middle and the big blob gets cut -> the Gaussian mixture of 08"
  _ -> "Round blobs of about the same size are easy. This is exactly what k-means assumes"

nPts :: Int
nPts = 300                 -- ★ 点の数

data World = World
  { wKind  :: !Int
  , wData  :: !DS
  , wKm    :: !KM
  , wAri   :: !Double
  , wTruthJ :: !Double
  , wSeedD :: !Int
  , wSeedK :: !Int
  , wCells :: !Picture                 -- 右の背景（ボロノイ図）
  , wDirty :: !Bool
  , wJob   :: !(Maybe Job)            -- 計算中の k-means（終わったら wData・wKm を入れかえる）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-03 Shapes k-means gets wrong" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      let w = makeCells env world0 { wTest = selfTest }
      w `seq` pure w
  , skStep  = \env _ w -> pure (let w' = step w in if wDirty w' then makeCells env w' else w')
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = finish $ newData (World 0 (DS V.empty U.empty 1) (KM V.empty U.empty 0 [] 0 []) 0 0 31 1 Blank True Nothing [])

kindOf :: World -> String
kindOf w = headOr "blobs" (drop (wKind w) kinds)

newData :: World -> World
newData w = runKm w (withSeed 1 (makeDataset (kindOf w) nPts (fromIntegral (wSeedD w))))

-- k-means++ で 10 回やり直して最良（kmeansBest と同じ計算）。重いので 1 フレームに 2 回ずつ進める
data Job = Job { jData :: !DS, jRng :: !Rng, jLeft :: !Int, jBest :: !(Maybe KM), jAll :: ![Double] }

runKm :: World -> DS -> World
runKm w d = w { wJob = Just (Job d (seedRng (fromIntegral (wSeedK w))) 10 Nothing []) }

jobStep :: Job -> Job
jobStep j = j { jRng = rng', jLeft = jLeft j - 1, jBest = Just best, jAll = jAll j ++ [kmInertia r] }
  where
    pts = dsPts (jData j)
    (c0, rng') = runR (initPlusPlus pts (dsK (jData j))) (jRng j)
    r = lloyd pts c0 100
    best = case jBest j of
      Just b | not (kmInertia r < kmInertia b) -> b
      _ -> r

step :: World -> World
step w = case wJob w of
  Nothing -> w
  Just j0 ->
    let j = if jLeft j0 > 0 then jobStep j0 else j0
        j2 = if jLeft j > 0 then jobStep j else j
    in if jLeft j2 > 0 then w { wJob = Just j2 } else
         let d = jData j2
             km = (maybe (KM V.empty U.empty 0 [] 0 []) id (jBest j2)) { kmAll = jAll j2 }
         in w { wJob = Nothing, wData = d, wKm = km, wAri = ariScore (kmLabels km) (dsTruth d), wTruthJ = truthJ d, wDirty = True }

finish :: World -> World
finish w = maybe w (const (finish (step w))) (wJob w)

-- 正解の分け方で、その中心（平均）からの J
truthJ :: DS -> Double
truthJ d = inertiaOf (dsPts d) (updateCenters (dsPts d) (dsTruth d) (V.replicate (dsK d) (pt2 0 0))) (dsTruth d)

views :: Env -> (G, G)
views env = let w = (envW env - 120) / 2
                h = envH env - 260
            in (fitView 40 100 w h 0 0 5.0, fitView (80 + w) 100 w h 0 0 5.0)

makeCells :: Env -> World -> World
makeCells env w = img `seq` w { wCells = img, wDirty = False }
  where
    v = snd (views env)
    cw = ceiling (pW v / 4) :: Int
    ch = ceiling (pH v / 4) :: Int
    cxs = U.fromList (map ptX (V.toList (kmCenters (wKm w))))
    cys = U.fromList (map ptY (V.toList (kmCenters (wKm w))))
    -- いちばん近い中心の番号（nearestCenter と同じ計算を、速いように x, y を別々の配列で）
    nearest :: Double -> Double -> Int
    nearest px py = go 0 0 (1 / 0)
      where go !j !b !bd | j >= U.length cxs = b
                         | otherwise = let dx = px - U.unsafeIndex cxs j
                                           dy = py - U.unsafeIndex cys j
                                           d = dx * dx + dy * dy
                                       in if d < bd then go (j + 1) j d else go (j + 1) b bd
    img = imageTL (pX v) (pY v) (pW v) (pH v) cw ch $ \x y ->
      let px = x0 v + (fromIntegral x + 0.5) / fromIntegral cw * (x1 v - x0 v)
          py = y1 v - (fromIntegral y + 0.5) / fromIntegral ch * (y1 v - y0 v)
          (r, g, b) = palRGB (nearest px py)
      in (q (22 + fromIntegral r * 0.15), q (24 + fromIntegral g * 0.15), q (34 + fromIntegral b * 0.15))
    q :: Double -> Word8
    q t = fromIntegral (max 0 (min 255 (round t :: Int)))

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCells w ]
  ++ [ Color (grayC 80) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) | g <- [vl, vr] ]
  ++ [ drawPts vl (dsPts d) (Just (dsTruth d)) 7 255
     , drawPts vr (dsPts d) (Just (kmLabels km)) 7 255 ]
  ++ concat [ [Color (grayC 20) (rectTL (gX vr (ptX c) - 9) (gY vr (ptY c) - 9) 18 18), Color (palC j) (rectTL (gX vr (ptX c) - 7) (gY vr (ptY c) - 7) 14 14)]
            | (j, c) <- zip [0 ..] (V.toList (kmCenters km)) ]
  ++ [ say ("truth (" ++ dataName kind ++ ", " ++ show (dsK d) ++ " groups)   J = " ++ fmt 2 jt) (pX vl) (pY vl - 10) 17 (grayC 230)
     , say ("k-means (k = " ++ show (dsK d) ++ ")   J = " ++ fmt 2 (kmInertia km) ++ "   ARI = " ++ fmt 3 ari) (pX vr) (pY vr - 10) 17 (grayC 230)
     -- ---- 文字
     , say ("Shapes k-means gets wrong   " ++ show (wKind w + 1) ++ ". " ++ dataName kind ++ "   " ++ (if ari > 0.95 then "OK: split well" else "NG: differs from the truth")) 40 40 20 (grayC 240)
     , say "1 moons  2 circles  3 elongated  4 densities  5 sizes  6 blobs (easy)   Space retry   click add   R new data" 40 66 15 (grayC 170)
     , sayWrap ("Why? " ++ why kind) 40 (y0' - 16) (envW env - 80) 16 (rgbC 255 210 130)
     , sayWrap ((if kmInertia km < jt - 1e-9
                   then "k-means J (" ++ fmt 2 (kmInertia km) ++ ") < truth J (" ++ fmt 2 jt ++ ") -> k-means wins at its own goal 'make J small'. The goal just does not fit this data"
                   else "Here the k-means J and the truth J are about the same -> the goal fits the shape of the data")
                ++ ". Right background = Voronoi regions (borders are always straight).") 40 (y0' + 38) (envW env - 80) 14 (grayC 180)
     , drawSelfTest env (wTest w) ]
  ++ [ say ("k-means restart " ++ show (11 - jLeft j) ++ " / 10 ...") (pX vr) (pY vr + 22) 16 (rgbC 255 220 120) | Just j <- [wJob w] ]
  where
    (vl, vr) = views env
    d = wData w
    km = wKm w
    kind = kindOf w
    ari = wAri w
    jt = wTruthJ w
    y0' = pY vl + pH vl + 34

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        (vl, vr) = views env
        add v w' = if inBox v mx my
          then let d = maybe (wData w') jData (wJob w')
                   q = pt2 (invX v mx) (invY v my)
                   t = if V.null (dsPts d) then 0 else dsTruth d U.! nearestCenter q (dsPts d)
               in runKm w' d { dsPts = V.snoc (dsPts d) q, dsTruth = U.snoc (dsTruth d) t }
          else w'
    in add vr (add vl w)
  EventKey (Char c) Down _ _
    | c `elem` "123456" -> newData w { wKind = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> let w' = w { wSeedK = wSeedK w + 1 } in runKm w' (maybe (wData w) jData (wJob w))
  _ -> w

-- 左上 (x, y)、幅 w・高さ h の四角に、pw × ph のピクセル画像を引きのばして置く（view の中で使う）
imageTL :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
imageTL x y w h pw ph f = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral pw) (negate h / fromIntegral ph) (pixelImage pw ph 1 f))

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("blobs (round, same size): ARI >= 0.95: " ++ fmt 3 (ariOf "blobs"), ariOf "blobs" >= 0.95)
  , ("moons ARI < 0.6 (" ++ fmt 3 (ariOf "moons") ++ "), circles ARI < 0.2 (" ++ fmt 3 (ariOf "circles") ++ ")", ariOf "moons" < 0.6 && ariOf "circles" < 0.2)
  , ("elongated and different sizes also fail (ARI < 0.8): " ++ fmt 3 (ariOf "aniso") ++ ", " ++ fmt 3 (ariOf "sizes"), ariOf "aniso" < 0.8 && ariOf "sizes" < 0.8)
  , ("for moons, circles, elongated: k-means J < J of the true split (k-means wins at its own goal)", all (\kd -> let (d, r, _) = resOf kd in kmInertia r < truthJ d) ["moons", "circles", "aniso"])
  , ("every two k-means groups are split by the perpendicular bisector (a straight line) of their centres", sep)
  , ("every ARI is in -1..1 with no NaN", all (\kd -> let a = ariOf kd in a >= -1 && a <= 1 && noNaN [a]) kinds) ]
  where
    res = [ (kd, (d, r, ariScore (kmLabels r) (dsTruth d)))
          | kd <- kinds
          , let d = withSeed 1 (makeDataset kd nPts 90)
                r = withSeed 4 (kmeansBest (dsPts d) (dsK d) 10 True 100) ]
    resOf kd = maybe (DS V.empty U.empty 1, KM V.empty U.empty 0 [] 0 [], 0) id (lookup kd res)
    ariOf kd = let (_, _, a) = resOf kd in a
    sep = and [ not ((l == a && s > 1e-12) || (l == b && s < -1e-12))
              | kd <- ["moons", "aniso"]
              , let (d, r, _) = resOf kd
                    cs = kmCenters r
              , a <- [0 .. dsK d - 1], b <- [a + 1 .. dsK d - 1]
              , let ca = cs V.! a
                    cb = cs V.! b
                    nx = ptX cb - ptX ca
                    ny = ptY cb - ptY ca
                    mx = (ptX ca + ptX cb) / 2
                    my = (ptY ca + ptY cb) / 2
              , (i, p) <- zip [0 ..] (V.toList (dsPts d))
              , let s = (ptX p - mx) * nx + (ptY p - my) * ny
                    l = kmLabels r U.! i ]

-- ---------- やってみよう ----------
-- 1. 1〜5 を順に見て、k-means の右の図のどこがまちがっているか、なぜかを説明しよう
-- 2. スペースで何回やり直しても、三日月は分かれない。初期化の問題ではなく「目的」の問題
-- 3. 3（細長い）で、データの y を 5 倍に引きのばすと？（newData の後で V.map で y を 5 倍）。距離の測り方しだいで答えが変わる
-- 4. 正解の J と k-means の J を比べよう。k-means の J のほうが小さいのはどのデータ？
-- 5. この 5 つを、あとで DBSCAN（06）・混合ガウス分布（08）・スペクトラル（09）で解いてみよう

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
