-- ============================================
-- 61-02 k の選び方 ─ k-means は k を自分で決められない。J（慣性）は k を増やすほど小さくなる一方（k = 点の数で 0）
--   エルボー法：J の折れ線が「ひじ」のように曲がる k（ここでは、両端を結ぶ直線からいちばん下に離れた k）
--   シルエット係数 s_i = (b − a) / max(a, b)：a = 仲間までの平均距離、b = いちばん近い別グループまでの平均距離。−1〜1、大きいほどよい
--   全部の点の平均がいちばん大きい k を選ぶ。右の棒 = 各点の s_i（負 = 別のグループのほうが近い「迷子」）
-- 操作：← → 本当のグループ数 / ↑↓ 表示する k / D かたまりの広がり / R 新しいデータ / クリック 点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts, kMax :: Int
nPts = 240                 -- ★ 点の数
kMax = 10                  -- ★ 調べる k の上限

-- k ごとの結果
data KRes = KRes { rK :: !Int, rJ :: !Double, rLabels :: !(U.Vector Int), rCenters :: !Pts, rSil :: !(U.Vector Double), rSilMean :: !Double }

data World = World
  { wPts    :: !Pts
  , wTrueG  :: !Int
  , wSpread :: !Int                    -- 0 小 / 1 中 / 2 大
  , wShowK  :: !Int
  , wSeedD  :: !Int
  , wRes    :: ![KRes]
  , wPend   :: !(Maybe Pend)          -- 計算中（終わったら wPts と wRes を入れかえる）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-02 Choosing k" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = world0 { wTest = selfTest }
      sum (map rSilMean (wRes w)) `seq` pure w
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = let w = newData (World V.empty 4 1 4 21 [] Nothing []) in finish w

-- g 個のかたまりを輪の上に（sp = 広がり 0, 1, 2）
makeGroups :: Int -> Int -> Int -> Int -> R DS
makeGroups g sp seed n = do
  seedR (fromIntegral seed)
  let sd = headOr 0.2 (drop sp [0.12, 0.2, 0.32])
  makeBlobs [ Blob (n `div` g) (r * cos a, r * sin a) (sd, sd) 0
            | j <- [0 .. g - 1]
            , let a = 2 * pi * fromIntegral j / fromIntegral g + 0.3
                  r = if g == 1 then 0 else 1.3 ]

-- ★ k = 1〜kMax で J とシルエットを計算。k + 1 は「k の中心 + いちばん遠い点」からも始める → J は必ず減る
--   重いので、画面では 1 フレームに k を 1 つずつ計算する（Pend = 途中の状態。乱数の状態も持ち越す）
data Pend = Pend { pPts :: !Pts, pK :: !Int, pPrev :: !(Maybe KM), pRng :: !Rng, pAcc :: ![KRes] }

startAnalyse :: Pts -> Int -> Pend
startAnalyse pts seed = Pend pts 1 Nothing (seedRng (fromIntegral seed)) []

analyseStep :: Int -> Pend -> Pend
analyseStep restarts pd = pd { pK = k + 1, pPrev = Just best, pRng = rng', pAcc = e `seq` pAcc pd ++ [e] }
  where
    pts = pPts pd
    k = pK pd
    n = V.length pts
    (b0, rng') = runR (kmeansBest pts k restarts True 100) (pRng pd)
    best = case pPrev pd of
      Nothing -> b0
      Just pv ->
        let dOf i = sqd (pts V.! i) (kmCenters pv V.! (kmLabels pv U.! i))
            far = foldl (\f i -> if dOf i > dOf f then i else f) 0 [0 .. n - 1]
            warm = lloyd pts (V.snoc (kmCenters pv) (pts V.! far)) 100
        in if n > 0 && kmInertia warm < kmInertia b0 then warm else b0
    sil = if k >= 2 then silhouetteVals pts (kmLabels best) else U.replicate n 0
    e = KRes k (kmInertia best) (kmLabels best) (kmCenters best) sil (meanOf (U.toList sil))

analyse :: Pts -> Int -> Int -> [KRes]          -- 全部まとめて（自己テスト用）
analyse pts seed restarts = pAcc (until (\pd -> pK pd > kMax) (analyseStep restarts) (startAnalyse pts seed))

-- 両端を結ぶ直線から、いちばん下に離れた k（J と k を 0〜1 にそろえてから）
elbowK :: [KRes] -> Int
elbowK r = fst (foldl pick (1, -1) (zip [0 :: Int ..] r))
  where
    j0 = rJ (headOr dummy r)
    j1 = rJ (if null r then dummy else last r)
    pick (b, bd) (i, e) = let x = fromIntegral i / fromIntegral (length r - 1)
                              y = (rJ e - j1) / max 1e-12 (j0 - j1)
                              d = 1 - x - y
                          in if d > bd then (rK e, d) else (b, bd)

silK :: [KRes] -> Int
silK r = rK (foldl (\b e -> if rK e >= 2 && rSilMean e > rSilMean b then e else b) (headOr dummy (drop 1 r)) r)

dummy :: KRes
dummy = KRes 1 0 U.empty V.empty U.empty 0

newData :: World -> World
newData w = let d = withSeed 1 (makeGroups (wTrueG w) (wSpread w) (wSeedD w) nPts)
            in w { wPend = Just (startAnalyse (dsPts d) 3) }

-- 1 フレームに k を 1 つ
step :: World -> World
step w = case wPend w of
  Just pd | pK pd > kMax -> w { wPts = pPts pd, wRes = pAcc pd, wPend = Nothing }
          | otherwise -> w { wPend = Just (analyseStep 5 pd) }
  Nothing -> w

finish :: World -> World                        -- 最後まで一気に（はじめの 1 回）
finish w = case wPend w of
  Nothing -> w
  _ -> finish (step w)

viewOf :: Env -> G
viewOf env = fitView 40 100 (envW env * 0.36) (envH env * 0.56) 0 0 3.6

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v))
  , drawPts v (wPts w) (Just (rLabels e)) 7 255 ]
  ++ concat [ [Color (grayC 20) (rectTL (gX v (ptX c) - 8) (gY v (ptY c) - 8) 16 16), Color (palC j) (rectTL (gX v (ptX c) - 6) (gY v (ptY c) - 6) 12 12)]
            | (j, c) <- zip [0 ..] (V.toList (rCenters e)) ]
  -- ---- エルボー（J）とシルエットの平均
  ++ [ plotAxes g1 "k" "J (inertia) -> the elbow" [1, 2, 4, 6, 8, 10] [0, roundSig 2 (y1 g1)]
     , Color (rgbC 255 190 80) (polyline 2 [ (gX g1 (fromIntegral (rK r)), gY g1 (rJ r)) | r <- res ])
     , Color (rgbaC 255 255 255 60) (Line [(gX g1 1, gY g1 (rJ r0)), (gX g1 (fromIntegral kMax), gY g1 (rJ rL))]) ]
  ++ [ Color (if rK r == eK then rgbC 255 190 80 else if rK r == showK then white else grayC 120) (circleAt (gX g1 (fromIntegral (rK r))) (gY g1 (rJ r)) (if rK r == eK then 14 else 7)) | r <- res ]
  ++ [ plotAxes g2 "k" "mean silhouette -> the largest k" [1, 2, 4, 6, 8, 10] [0, 0.5, 1]
     , Color (rgbC 110 230 190) (polyline 2 [ (gX g2 (fromIntegral (rK r)), gY g2 (rSilMean r)) | r <- drop 1 res ]) ]
  ++ [ Color (if rK r == sK then rgbC 110 230 190 else if rK r == showK then white else grayC 120) (circleAt (gX g2 (fromIntegral (rK r))) (gY g2 (rSilMean r)) (if rK r == sK then 14 else 7)) | r <- drop 1 res ]
  -- ---- 各点のシルエット（クラスタごとに大きい順）
  ++ [ Color (grayC 120) (Line [(bx0, by), (bx0, by + bh)]) ]
  ++ [ Color (palC (rLabels e U.! i)) (rectTL (min bx0 (bx0 + wv)) y (abs wv) (max 1 (bh / fromIntegral nO)))
     | (r, i) <- zip [0 :: Int ..] order
     , let y = by + fromIntegral r / fromIntegral nO * bh
           wv = f2 (max (rSil e U.! i) (-0.44)) * bw * 0.68 ]
  ++ [ Color (rgbaC 255 255 255 120) (Line [(xm, by), (xm, by + bh)])
     , say ("s_i of each point (k = " ++ show showK ++ ")") bx (by - 10) 14 (grayC 205)
     , sayC "-0.3" (bx0 - 0.3 * bw * 0.68) (by + bh + 18) 14 (grayC 165)
     , sayC "0" bx0 (by + bh + 18) 14 (grayC 165)
     , sayC "1" (bx0 + bw * 0.68) (by + bh + 18) 14 (grayC 165) ]
  -- ---- 文字
  ++ [ say ("Choosing k   true groups = " ++ show (wTrueG w) ++ "   elbow -> k = " ++ show eK ++ okNg eK ++ "   silhouette -> k = " ++ show sK ++ okNg sK) 40 40 20 (grayC 240)
     , say ("<- -> true groups   Up/Down shown k   D spread (" ++ headOr "" (drop (wSpread w) ["small", "medium", "large"]) ++ ")   R new data   click add") 40 66 15 (grayC 170)
     , say ("k = " ++ show showK ++ ": J = " ++ fmt 2 (rJ e) ++ "   mean silhouette = " ++ fmt 3 (rSilMean e) ++ "   negative points " ++ show neg) 40 (pY v + pH v + 34) 15 (grayC 215)
     , sayWrap "J always decreases with k (k + 1 starts from the k answer plus one centre), so 'the k with the smallest J' cannot be chosen. With spread 'large' both methods start to get lost." 40 (pY v + pH v + 46) (pW v + 40) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  ++ [ say ("computing k = " ++ show (pK pd) ++ " / " ++ show kMax ++ " ...") (pX v) (pY v + 22) 16 (rgbC 255 220 120) | Just pd <- [wPend w] ]
  where
    v = viewOf env
    res = wRes w
    showK = wShowK w
    e = headOr dummy (drop (showK - 1) res)
    r0 = headOr dummy res
    rL = if null res then dummy else last res
    eK = elbowK res
    sK = silK res
    okNg k = if k == wTrueG w then " OK" else " NG"
    gx = pX v + pW v + 70
    gw = envW env * 0.27
    g1 = G gx 120 gw 200 1 (fromIntegral kMax) 0 (rJ r0 * 1.05)
    g2 = G gx 400 gw 200 1 (fromIntegral kMax) 0 1
    bx = gx + gw + 60
    bw = envW env - bx - 40
    by = 120
    bh = envH env - 230
    bx0 = bx + bw * 0.3
    nO = max 1 (V.length (wPts w))
    order = sortBy (\a b -> compare (rLabels e U.! a) (rLabels e U.! b) <> compare (rSil e U.! b) (rSil e U.! a)) [0 .. U.length (rLabels e) - 1]
    xm = bx0 + f2 (rSilMean e) * bw * 0.68
    neg = U.length (U.filter (< 0) (rSil e))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        v = viewOf env
    in if inBox v mx my
         then let pts = V.snoc (maybe (wPts w) pPts (wPend w)) (pt2 (invX v mx) (invY v my)) in w { wPend = Just (startAnalyse pts 3) }
         else w
  EventKey (SpecialKey KeyRight) Down _ _ -> newData w { wTrueG = min 8 (wTrueG w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> newData w { wTrueG = max 1 (wTrueG w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wShowK = min kMax (wShowK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wShowK = max 1 (wShowK w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> newData w { wSpread = (wSpread w + 1) `mod` 3 }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("hand-computed example: 0, 1 | 10, 11 gives s(0) = 1 - 1/10.5 = " ++ fmt 4 (1 - 1 / 10.5), near (s4 U.! 0) (1 - 1 / 10.5) 1e-12 && near (s4 U.! 1) (1 - 1 / 9.5) 1e-12)
  , ("s_i is in -1..1 for any labelling (10 random labellings)", inR)
  , ("well separated blobs (g = 2..6): the mean silhouette is largest at the true g: " ++ commaList (map fst gs), okG)
  , ("J decreases with k (k = 1..10)", and (zipWith (\a b -> rJ b <= rJ a + 1e-9) r4 (drop 1 r4)))
  , ("the elbow method is right for at least 3 of g = 2..6 (" ++ show okE ++ " / 5)", okE >= 3)
  , ("a one-point cluster has s = 0, and one single cluster gives all 0", one U.! 2 == 0 && U.all (== 0) (silhouetteVals rp (U.replicate (V.length rp) 0))) ]
  where
    s4 = silhouetteVals (V.fromList [pt2 0 0, pt2 1 0, pt2 10 0, pt2 11 0]) (U.fromList [0, 0, 1, 1])
    (rp, inR) = withSeed 5 $ do
      ps <- mapM (const (do { a <- rnd; b <- rnd; pure (pt2 a b) })) [1 .. 60 :: Int]
      let pv = V.fromList ps
      oks <- mapM (const (do
               lab <- U.fromList <$> mapM (const (rndInt 0 3)) ps
               pure (U.all (\x -> x >= -1 && x <= 1) (silhouetteVals pv lab)))) [1 .. 10 :: Int]
      pure (pv, and oks)
    gs = [ (silK r, elbowK r == g) | g <- [2 .. 6], let d = withSeed 1 (makeGroups g 0 (40 + g) 120), let r = analyse (dsPts d) 9 2 ]
    okG = and (zipWith (\(s, _) g -> s == g) gs [2 ..])
    okE = length (filter snd gs)
    r4 = analyse (dsPts (withSeed 1 (makeGroups 4 1 33 120))) 9 2
    one = silhouetteVals (V.fromList [pt2 0 0, pt2 1 1, pt2 5 5]) (U.fromList [0, 0, 1])
    commaList xs = case map show xs of
      [] -> ""
      (y : ys) -> y ++ concatMap (',' :) ys

-- ---------- やってみよう ----------
-- 1. ← → で本当のグループ数を 2〜8 に変えて、エルボー法とシルエットのどちらがよく当たるか数えよう
-- 2. D で広がりを「大」にすると、かたまりが重なる。どちらの方法が先に外れる？
-- 3. ↑↓ で k を本当の数より大きくすると、右の棒（各点の s_i）はどうなる？ 負の点はどこにある？
-- 4. 本当のグループ数を 1 にすると？ シルエットは k ≥ 2 でしか計算できない（だから 1 は選べない）
-- 5. nPts を 600 にすると計算が重くなる。シルエットは全部の点の組の距離（n² 個）を使うから

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
