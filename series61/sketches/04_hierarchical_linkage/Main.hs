-- ============================================
-- 61-04 凝集型クラスタリング ─ はじめは 1 点ずつ別のグループ。いちばん近い 2 つのグループを合体、をくり返す（n − 1 回で 1 つに）
--   「グループどうしの距離」の決め方（連結法）で結果が変わる
--   単連結 = いちばん近い点の組 / 完全連結 = いちばん遠い点の組 / 群平均 = 全部の組の平均 / ウォード法 = 合体で増える J（慣性）
--   合体のたびに全部を計算しなおさず、ランス-ウィリアムズの更新式 d(i∪j, k) = α_i d(i,k) + α_j d(j,k) + β d(i,j) + γ |d(i,k) − d(j,k)| で
--   単連結は「鎖」のように 1 点ずつつながって伸びやすい（鎖効果）。右の 4 つの小さな図 = 同じデータを 4 つの連結法で 3 つに分けた結果
-- 操作：1〜4 連結法 / スペース 再生・停止 / ← → 1 回ずつ / D データ / R 新しいデータ / クリック 点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IM
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
import qualified Data.IntSet as IS
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 46                  -- ★ 点の数（多いと合体が速すぎて見えない）

dKinds :: [String]
dKinds = ["chain", "blobs", "moons"]

data World = World
  { wDataNo  :: !Int
  , wData    :: !DS
  , wMethod  :: !Int
  , wAll4    :: !(V.Vector [Merge])     -- 4 つの連結法の合体の記録
  , wSegs    :: !(V.Vector (P, P))      -- 合体を絵にする線分
  , wCut3    :: ![(U.Vector Int, Double)]   -- 4 つの連結法で 3 つに切ったラベルと ARI
  , wTStep   :: !Int
  , wPlaying :: !Bool
  , wTimer   :: !Int
  , wSeedD   :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-04 Agglomerative clustering" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      w `seq` pure w
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newData (World 0 (DS V.empty U.empty 1) 0 V.empty V.empty [] 0 True 0 41 [])

newData :: World -> World
newData w = rebuild w { wData = withSeed 1 (makeDataset (headOr "blobs" (drop (wDataNo w) dKinds)) nPts (fromIntegral (wSeedD w))) }

rebuild :: World -> World
rebuild w = let pts = dsPts (wData w)
                a4 = V.fromList [ agglomerate pts m | m <- [0 .. 3] ]
                n = V.length pts
                cuts = [ (lb, ariScore lb (dsTruth (wData w))) | m <- [0 .. 3], let lb = cutTreeK (a4 V.! m) n 3 ]
            in setMethod (wMethod w) w { wAll4 = a4, wCut3 = cuts, wTStep = 0 }

setMethod :: Int -> World -> World
setMethod m w = w { wMethod = m, wSegs = V.fromList (mergeSegments (dsPts (wData w)) (merges w { wMethod = m }) m) }

merges :: World -> [Merge]
merges w = if V.null (wAll4 w) then [] else wAll4 w V.! wMethod w

-- ★ 合体を絵にするための線分：単連結 = いちばん近い組、完全連結 = いちばん遠い組、群平均・ウォード = 重心どうし
mergeSegments :: Pts -> [Merge] -> Int -> [(P, P)]
mergeSegments pts mg m = map seg mg
  where
    mem = membersOf mg (V.length pts)
    seg e =
      let as = mem V.! mA e
          bs = mem V.! mB e
      in if m >= 2 then (centroid pts as, centroid pts bs)
         else snd (foldl (\(bd, best) (i, j) -> let d = edist (pts V.! i) (pts V.! j)
                                                in if (m == 0 && d < bd) || (m == 1 && d > bd) then (d, (pts V.! i, pts V.! j)) else (bd, best))
                         (if m == 0 then 1 / 0 else -1, (pt2 0 0, pt2 0 0)) [ (i, j) | i <- as, j <- bs ])

centroid :: Pts -> [Int] -> P
centroid pts ids = let l = fromIntegral (length ids)
                       (cx, cy) = foldl (\(sx, sy) i -> (sx + ptX (pts V.! i) / l, sy + ptY (pts V.! i) / l)) (0, 0) ids
                   in pt2 cx cy

step :: World -> World
step w
  | wPlaying w = let t = wTimer w + 1
                     n = V.length (dsPts (wData w))
                 in w { wTimer = t, wTStep = if t `mod` 12 == 0 then (if wTStep w >= n - 1 then 0 else wTStep w + 1) else wTStep w }
  | otherwise = w

explain :: Int -> String
explain m = headOr "" (drop m
  [ "Yellow line = the closest pair of points just merged. All lines together are the minimum spanning tree (like the cluster tree of series 9). It easily grows long chains"
  , "Yellow line = the farthest pair of points of the two merged groups. Gives compact round groups with a small diameter"
  , "Yellow line = between centroids. Decided by the average distance over all pairs of points. Between single and complete linkage"
  , "Yellow line = between centroids. Picks the pair whose merge increases J (the k-means objective) least -> round groups like k-means" ])

linkFull :: Int -> String
linkFull m = linkName m ++ (if m == 3 then "'s method" else " linkage")

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) ]
  ++ [ if lastOne then Color (rgbC 255 240 150) (lineW 3 (sp a) (sp b)) else Color (rgbaC 255 255 255 70) (lineW 1.2 (sp a) (sp b))
     | t <- [0 .. min tStep (V.length (wSegs w)) - 1], let (a, b) = wSegs w V.! t, let lastOne = t == tStep - 1 ]
  ++ [ drawPts v pts (Just lab) 10 255 ]
  -- ---- 合体の高さのグラフ
  ++ [ plotAxes g1 "number of merges" "merge height (distance between groups)" [0, fromIntegral (n - 1)] [0, roundSig 2 hmax]
     , Color (rgbC 255 190 80) (polyline 2 [ (gX g1 (fromIntegral t + 1), gY g1 (mH e)) | (t, e) <- zip [0 :: Int ..] (take (max 1 tStep) mg) ])
     , Color (rgbaC 255 255 255 60) (Line [ (gX g1 (fromIntegral t + 1), gY g1 (mH e)) | (t, e) <- zip [0 :: Int ..] mg ]) ]
  -- ---- 4 つの連結法で 3 つに分けた結果
  ++ concat [ [ Color (if m == wMethod w then rgbC 176 140 255 else grayC 70) (frameTL (if m == wMethod w then 2 else 1) (pX sv) (pY sv) (pW sv) (pH sv))
              , drawPts sv pts (Just lb) 6 255
              , say (show (m + 1) ++ " " ++ linkName m ++ " (k = 3)   ARI " ++ fmt 2 ari) (pX sv) (pY sv - 6) 14 (if m == wMethod w then rgbC 200 180 255 else grayC 190) ]
            | (m, (lb, ari)) <- zip [0 ..] (wCut3 w)
            , let sv = fitView (gx + fromIntegral (m `mod` 2) * (tw + 30)) (340 + fromIntegral (m `div` 2) * (th + 40)) tw th 0 0 (if wDataNo w == 0 then 3.6 else 5.0) ]
  -- ---- 文字
  ++ [ say ("Agglomerative clustering   " ++ linkFull (wMethod w) ++ "   merge " ++ show tStep ++ " / " ++ show (n - 1) ++ "   groups " ++ show (n - tStep)
            ++ (if tStep > 0 && tStep <= length mg then "   height now " ++ fmt 3 (mH (mg !! (tStep - 1))) else "")) 40 40 20 (grayC 240)
     , say ("1 single  2 complete  3 average  4 Ward   Space play" ++ (if wPlaying w then "ing" else "/stop") ++ "   <- -> one merge   D data (" ++ dataName (headOr "" (drop (wDataNo w) dKinds)) ++ ")   R new   click add") 40 66 15 (grayC 170)
     , sayWrap (explain (wMethod w)) 40 (pY v + pH v + 16) (pW v) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    pts = dsPts (wData w)
    n = V.length pts
    mg = merges w
    tStep = wTStep w
    v = fitView 40 100 (envW env * 0.5) (envH env - 260) 0 0 (if wDataNo w == 0 then 3.4 else 4.8)
    sp p = (gX v (ptX p), gY v (ptY p))
    lab = labelsAfter mg n tStep
    gx = pX v + pW v + 60
    gw = envW env - gx - 40
    hmax = (if null mg then 1 else mH (last mg)) * 1.05
    g1 = G gx 120 gw 150 0 (fromIntegral (max 1 (n - 1))) 0 hmax
    tw = (gw - 30) / 2
    th = (envH env - 420) / 2 - 20

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        v = fitView 40 100 (envW env * 0.5) (envH env - 260) 0 0 (if wDataNo w == 0 then 3.4 else 4.8)
        d = wData w
    in if inBox v mx my then rebuild w { wData = d { dsPts = V.snoc (dsPts d) (pt2 (invX v mx) (invY v my)), dsTruth = U.snoc (dsTruth d) 0 } } else w
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> setMethod (fromEnum c - fromEnum '1') w
    | c `elem` "dD" -> newData w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPlaying = False, wTStep = min (n - 1) (wTStep w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPlaying = False, wTStep = max 0 (wTStep w - 1) }
  _ -> w
  where n = V.length (dsPts (wData w))

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
-- グループ A, B の距離をそのまま計算（総当たり）。ウォード = √(2|A||B|/(|A|+|B|))·|重心の差|
bruteLink :: Pts -> [Int] -> [Int] -> Int -> Double
bruteLink pts as bs m
  | m == 3 = sqrt (2 * na * nb / (na + nb)) * edist (centroid pts as) (centroid pts bs)
  | otherwise = foldl (\acc (i, j) -> let d = edist (pts V.! i) (pts V.! j)
                                      in if m == 0 then min acc d else if m == 1 then max acc d else acc + d / (na * nb))
                      (if m == 0 then 1 / 0 else if m == 1 then -1 / 0 else 0) [ (i, j) | i <- as, j <- bs ]
  where na = fromIntegral (length as)
        nb = fromIntegral (length bs)

selfTest :: [(String, Bool)]
selfTest =
  [ ("Lance-Williams update = group distance recomputed by brute force (4 linkages, 21 merges each)", all (\(a, _, _) -> a) per)
  , ("every merge joins the closest pair among the living groups (checked by brute force)", all (\(_, b, _) -> b) per)
  , ("merge heights only increase (monotone) -> the dendrogram can be drawn correctly", all (\(_, _, c) -> c) per)
  , ("single-linkage heights = edge lengths of the minimum spanning tree (Kruskal), in order", length sl == length mst && and (zipWith (\e d -> abs (mH e - d) < 1e-12) sl mst))
  , ("chaining: cutting two bridged blobs into 2, single ARI " ++ fmt 2 aS ++ " < Ward ARI " ++ fmt 2 aW, aS < 0.3 && aW > 0.6)
  , ("after n - 1 merges everything is one group (size of the last group = n)", all (\m -> let mg = agglomerate pts m in length mg == n - 1 && mSize (last mg) == n) [0 .. 3]) ]
  where
    pts = V.fromList (withSeed 44 (mapM (const (do { a <- rnd; b <- rnd; pure (pt2 (a * 3) (b * 2)) })) [1 .. 22 :: Int]))
    n = V.length pts
    per = [ check m | m <- [0 .. 3] ]
    check m =
      let mg = agglomerate pts m
          mem = membersOf mg n
          go _ _ [] = (True, True)
          go t alive (e : es) =
            let lw = abs (bruteLink pts (mem V.! mA e) (mem V.! mB e) m - mH e) <= 1e-9
                mn = and [ not (bruteLink pts (mem V.! a) (mem V.! b) m < mH e - 1e-9) | a <- alive, b <- alive, a < b ]
                (l2, m2) = go (t + 1) (filter (\c -> c /= mA e && c /= mB e) alive ++ [n + t]) es
            in (lw && l2, mn && m2)
          (lwOk, minOk) = go 0 [0 .. n - 1] mg
          mono = and (zipWith (\a b -> not (mH b < mH a - 1e-12)) mg (drop 1 mg))
      in (lwOk, minOk, mono)
    edges = sortBy (\a b -> compare (fst' a) (fst' b)) [ (edist (pts V.! i) (pts V.! j), i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
    fst' (d, _, _) = d
    root par x = let p = IM.findWithDefault x x par in if p == x then x else root par p
    mst = reverse (snd (foldl (\(par, acc) (d, i, j) -> let a = root par i; b = root par j in if a /= b then (IM.insert a b par, d : acc) else (par, acc)) (IM.empty, []) edges))
    sl = agglomerate pts 0
    ch = withSeed 1 (makeDataset "chain" nPts 7)
    aS = ariScore (cutTreeK (agglomerate (dsPts ch) 0) nPts 2) (dsTruth ch)
    aW = ariScore (cutTreeK (agglomerate (dsPts ch) 3) nPts 2) (dsTruth ch)

-- ---------- やってみよう ----------
-- 1. D で「鎖でつながる」にして 1（単連結）と 4（ウォード法）を比べよう。右の小さい図の ARI は？
-- 2. ← → で 1 回ずつ進め、黄色の線（いま合体した組）が長くなっていく様子を見よう。右上のグラフは下がらない
-- 3. 2（完全連結）で、離れた点を 1 つクリックで足すと？ 外れ値は最後まで 1 人ぼっち
-- 4. 三日月（D）では、どの連結法なら 2 つに分けられる？（03 で k-means は失敗した）
-- 5. 05 では合体の記録を「木（デンドログラム）」にして、切る高さを選ぶ

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
