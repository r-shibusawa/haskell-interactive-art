-- ============================================
-- 61-01 k-means は「最小化」─ 目的関数 = 慣性（各点と自分の中心の距離² の合計）J = Σ |x_i − c(i)|²
--   ① 割り当て（中心を固定して、各点をいちばん近い中心へ）② 更新（仲間を固定して、中心を平均へ）。どちらも J を減らすか同じにする
--   → J は必ず止まる。ただし「いちばん小さい J」とは限らない（初めの中心しだい）→ k-means++ と「10 回やり直して最良」
--   背景の色 = いちばん近い中心の領域 = ボロノイ図（シリーズ 22）。基本の k-means はシリーズ 17 の 10。ここはその発展
-- 操作：スペース 半ステップ / A 自動 / I ランダム初期化 / P k-means++ 初期化 / B 10 回ずつやり直して比べる / ↑↓ k / クリック 点を足す / R 新しいデータ
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
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 300                 -- ★ 点の数

data World = World
  { wPts      :: !Pts
  , wCenters  :: !Pts
  , wLabels   :: !(U.Vector Int)
  , wHist     :: ![Double]              -- 半ステップごとの J
  , wNextAsg  :: !Bool                  -- 次は ① 割り当て？
  , wK        :: !Int
  , wAuto     :: !Bool
  , wSeedD    :: !Int
  , wSeedI    :: !Int
  , wRestarts :: !(Maybe ([Double], [Double]))   -- B：ランダム初期化 10 回と k-means++ 10 回の最後の J
  , wInitName :: !String
  , wTimer    :: !Int
  , wCells    :: !Picture               -- ボロノイ図の背景（中心が動いたときだけ作りなおす）
  , wDirty    :: !Bool
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-01 k-means is minimisation" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      let w = makeCells env (world0 { wTest = selfTest })
      w `seq` pure w
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newData (World V.empty V.empty U.empty [] False 6 False 11 1 Nothing "" 0 Blank True [])

-- 6 つのかたまり（2 つずつ近い）
blobs6 :: Int -> R DS
blobs6 seed = do
  seedR (fromIntegral seed)
  makeBlobs [ Blob (nPts `div` 6) (r * cos a + s * sin a, r * sin a - s * cos a) (0.12, 0.12) 0
            | j <- [0 .. 5 :: Int]
            , let a = 2 * pi * fromIntegral (j `div` 2) / 3 + 0.5
                  r = 1.25
                  s = if odd j then 0.28 else -0.28 ]

newData :: World -> World
newData w = initCenters True w { wPts = dsPts (withSeed 1 (blobs6 (wSeedD w))), wRestarts = Nothing }

-- ★ 初期化：usePP = True なら k-means++、False ならランダム
initCenters :: Bool -> World -> World
initCenters usePP w =
  let cs = withSeed (fromIntegral (1000 + wSeedI w)) ((if usePP then initPlusPlus else initRandomCenters) (wPts w) (wK w))
      ls = assignLabels (wPts w) cs
  in w { wCenters = cs, wLabels = ls, wHist = [inertiaOf (wPts w) cs ls], wNextAsg = False, wSeedI = wSeedI w + 1
       , wInitName = if usePP then "k-means++" else "random", wDirty = True }

-- ★ 半ステップ：更新 → 割り当て → 更新 … J を記録
halfStep :: World -> World
halfStep w =
  let (cs, ls) = if wNextAsg w then (wCenters w, assignLabels (wPts w) (wCenters w)) else (updateCenters (wPts w) (wLabels w) (wCenters w), wLabels w)
      h = wHist w ++ [inertiaOf (wPts w) cs ls]
  in w { wCenters = cs, wLabels = ls, wNextAsg = not (wNextAsg w), wDirty = True
       , wHist = if length h > 200 then take 1 h ++ drop 2 h else h }

-- ランダム初期化と k-means++ をそれぞれ 10 回
runRestarts :: World -> World
runRestarts w =
  let (rr, pp) = withSeed (fromIntegral (500 + wSeedD w)) $ do
        a <- mapM (const (do c <- initRandomCenters (wPts w) (wK w); pure (lloyd (wPts w) c 100))) [1 .. 10 :: Int]
        b <- mapM (const (do c <- initPlusPlus (wPts w) (wK w); pure (lloyd (wPts w) c 100))) [1 .. 10 :: Int]
        pure (a, b)
      best = foldl1 (\a b -> if kmInertia b < kmInertia a then b else a) (pp ++ rr)
  in w { wRestarts = Just (map kmInertia rr, map kmInertia pp), wCenters = kmCenters best, wLabels = kmLabels best
       , wHist = [kmInertia best], wNextAsg = False, wInitName = "best of 20", wDirty = True }

step :: Env -> World -> World
step env w0 =
  let w1 = w0 { wTimer = if wAuto w0 then wTimer w0 + 1 else wTimer w0 }
      w2 = if wAuto w0 && wTimer w1 `mod` 20 == 0 then halfStep w1 else w1
  in if wDirty w2 then makeCells env w2 else w2

viewOf :: Env -> G
viewOf env = fitView 40 100 (envW env * 0.58) (envH env - 170) 0 0 3.6

-- ボロノイ図（4 ピクセルに 1 回だけ計算）
makeCells :: Env -> World -> World
makeCells env w = img `seq` w { wCells = img, wDirty = False }
  where
    v = viewOf env
    cw = ceiling (pW v / 4) :: Int
    ch = ceiling (pH v / 4) :: Int
    cxs = U.fromList (map ptX (V.toList (wCenters w)))
    cys = U.fromList (map ptY (V.toList (wCenters w)))
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
      in (q (22 + fromIntegral r * 0.16), q (24 + fromIntegral g * 0.16), q (34 + fromIntegral b * 0.16))
    q :: Double -> Word8
    q t = fromIntegral (max 0 (min 255 (round t :: Int)))

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCells w
  , Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) ]
  -- 点 → 中心の線、点、中心
  ++ [ Color (rgbaC 255 255 255 30) (Line [(gX v (ptX p), gY v (ptY p)), (gX v (ptX c), gY v (ptY c))])
     | (i, p) <- zip [0 ..] (V.toList pts), let c = cs V.! (lab U.! i) ]
  ++ [ drawPts v pts (Just lab) 7 255 ]
  ++ concat [ [Color (grayC 20) (rectTL (gX v (ptX c) - 9) (gY v (ptY c) - 9) 18 18), Color (palC j) (rectTL (gX v (ptX c) - 7) (gY v (ptY c) - 7) 14 14)]
            | (j, c) <- zip [0 ..] (V.toList cs) ]
  -- ---- 右：J のグラフ
  ++ [ plotAxes g1 "half-steps" "inertia J (objective)" [0, x1 g1] [0, roundSig 2 (y1 g1)]
     , Color (rgbC 255 220 120) (polyline 2 [ (gX g1 (fromIntegral i), gY g1 h) | (i, h) <- zip [0 :: Int ..] hist ]) ]
  ++ [ Color (if odd i then rgbC 255 170 70 else rgbC 110 180 255) (circleAt (gX g1 (fromIntegral i)) (gY g1 h) 6) | (i, h) <- zip [0 :: Int ..] hist, i > 0 ]
  -- ---- 右下：やり直しの比較
  ++ restartsPic
  -- ---- 文字
  ++ [ say ("k-means is minimisation   k = " ++ show (wK w) ++ "   J = " ++ fmt 3 jNow ++ "   init: " ++ wInitName w) 40 40 20 (grayC 240)
     , say ("next: " ++ (if wNextAsg w then "(1) assign (blue)" else "(2) update (orange)")) (gx0 + 0) 96 15 (if wNextAsg w then rgbC 110 180 255 else rgbC 255 170 70)
     , say ("Space half-step   A auto" ++ (if wAuto w then " (playing)" else "") ++ "   I random   P k-means++   B compare restarts   Up/Down k   click add   R new data") 40 66 15 (grayC 170)
     , sayWrap "Background = region of the nearest centre (Voronoi diagram). Borders are perpendicular bisectors of two centres, so k-means groups are always convex." 40 (envH env - 62) (pW v) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    v = viewOf env
    pts = wPts w
    cs = wCenters w
    lab = wLabels w
    hist = wHist w
    jNow = last' hist
    last' xs = if null xs then 0 else last xs
    gx0 = pX v + pW v + 70
    gw = envW env - gx0 - 40
    g1 = G gx0 130 gw 210 0 (max 10 (fromIntegral (length hist - 1))) 0 (foldl max 0 hist * 1.05)
    g2x = gx0
    g2y = 420
    g2h = 170
    restartsPic = case wRestarts w of
      Nothing -> [ sayWrap "Press B to run random init and k-means++ 10 times each to the end and compare." g2x (g2y - 14) gw 14 (grayC 150) ]
      Just (rr, pp) ->
        let allJ = rr ++ pp
            lo = foldl min (1 / 0) allJ
            hi = foldl max (-1 / 0) allJ
            bw = gw / 22
        in [ say "B: final J of 10 restarts each (lower is better)" g2x (g2y - 8) 14 (grayC 205) ]
           ++ [ Color (if i < 10 then rgbC 255 120 130 else rgbC 110 230 190) (rectTL x (g2y + g2h - h) (bw - 3) h)
              | (i, j) <- zip [0 :: Int ..] allJ
              , let x = g2x + fromIntegral (if i < 10 then i else i + 2) * bw
                    h = 20 + f2 ((j - lo) / max 1e-9 (hi - lo)) * (g2h - 30) ]
           ++ [ say ("random mean " ++ fmt 2 (meanOf rr)) g2x (g2y + g2h + 20) 14 (rgbC 255 140 150)
              , say ("k-means++ mean " ++ fmt 2 (meanOf pp)) (g2x + gw / 2) (g2y + g2h + 20) 14 (rgbC 130 235 200)
              , say ("smallest J = " ++ fmt 3 lo ++ " (how many times did it appear?)") g2x (g2y + g2h + 42) 14 (grayC 200) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        v = viewOf env
    in if inBox v mx my
         then let pts = V.snoc (wPts w) (pt2 (invX v mx) (invY v my))
                  ls = assignLabels pts (wCenters w)
              in w { wPts = pts, wLabels = ls, wHist = wHist w ++ [inertiaOf pts (wCenters w) ls], wNextAsg = False, wDirty = True }
         else w
  EventKey (SpecialKey KeySpace) Down _ _ -> halfStep w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "iI" -> initCenters False w
    | c `elem` "pP" -> initCenters True w
    | c `elem` "bB" -> runRestarts w
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> initCenters True w { wK = min 10 (wK w + 1), wRestarts = Nothing }
  EventKey (SpecialKey KeyDown) Down _ _ -> initCenters True w { wK = max 2 (wK w - 1), wRestarts = Nothing }
  _ -> w

-- 左上 (x, y)、幅 w・高さ h の四角に、pw × ph のピクセル画像を引きのばして置く（view の中で使う）
imageTL :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
imageTL x y w h pw ph f = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral pw) (negate h / fromIntegral ph) (pixelImage pw ph 1 f))

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Lloyd's J never increases at any half-step (30 runs, k = 3..8)", all (\(m, _, _) -> m) runs)
  , ("after assignment every point belongs to its nearest centre (0 points closer to another centre)", all (\(_, a, _) -> a) runs)
  , ("the mean is the best centre: J(mean + d) = J(mean) + n|d|^2 (moving it always increases J)", idOk)
  , ("k-means++ mean J " ++ fmt 2 (meanOf jp) ++ " <= random init mean " ++ fmt 2 (meanOf jr) ++ " (30 runs each, k = 6)", meanOf jp <= meanOf jr)
  , ("Lloyd stops after finitely many steps (labels stop changing, all 30 within 100)", all (\(_, _, s) -> s) runs)
  , ("Voronoi regions are convex: the midpoint of two points in one region is in it too (300 pairs)", convex) ]
  where
    d = dsPts (withSeed 1 (blobs6 77))
    (runs, idOk, jr, jp, convex) = withSeed 7 $ do
      rs <- mapM (\t -> do
              let k = 3 + t `mod` 6
              c <- initRandomCenters d k
              let res = lloyd d c 100
                  h = kmHist res
                  mono = and (zipWith (\a b -> b <= a + 1e-9) h (drop 1 h))
                  cs = kmCenters res
                  asg = and [ not (V.any (\c' -> sqd p c' < sqd p (cs V.! (kmLabels res U.! i)) - 1e-12) cs) | (i, p) <- zip [0 ..] (V.toList d) ]
              pure (mono, asg, kmIters res < 100)) [0 .. 29 :: Int]
      -- 平均 + δ
      let p50 = V.toList (V.take 50 d)
          cen = foldl (\(sx, sy) p -> (sx + ptX p / 50, sy + ptY p / 50)) (0, 0) p50
          sse (cx, cy) = foldl (\s p -> s + sqd p (pt2 cx cy)) 0 p50
      ids <- mapM (const (do
               a <- (* 0.3) <$> gauss
               b <- (* 0.3) <$> gauss
               pure (near (sse (fst cen + a, snd cen + b)) (sse cen + 50 * (a * a + b * b)) 1e-9))) [1 .. 20 :: Int]
      seedR 8
      js <- mapM (const (do
              c1 <- initRandomCenters d 6
              let a = kmInertia (lloyd d c1 100)
              c2 <- initPlusPlus d 6
              let b = kmInertia (lloyd d c2 100)
              pure (a, b))) [1 .. 30 :: Int]
      cs <- initPlusPlus d 5
      cv <- mapM (const (do
              ax <- rnd; ay <- rnd; bx <- rnd; by <- rnd
              let a = pt2 (-2 + 4 * ax) (-2 + 4 * ay)
                  b = pt2 (-2 + 4 * bx) (-2 + 4 * by)
                  m = pt2 ((ptX a + ptX b) / 2) ((ptY a + ptY b) / 2)
              pure (not (nearestCenter a cs == nearestCenter b cs && nearestCenter m cs /= nearestCenter a cs)))) [1 .. 300 :: Int]
      pure (rs, and ids, map fst js, map snd js, and cv)

-- ---------- やってみよう ----------
-- 1. I（ランダム）で始めてスペースを何回も押し、J のグラフが階段のように下がって止まるのを見よう
-- 2. I を何回か押してから A で自動再生。2 つのかたまりを 1 つの中心が取ってしまう「悪い止まり方」を探そう
-- 3. B で 10 回ずつ比べる。k-means++ のほうが平均の J が小さいのはなぜ？（遠い点から順に中心を選ぶ）
-- 4. ↑↓ で k を変えると J はどうなる？ k を増やすと J は小さくなる一方 → k は J だけでは選べない（02）
-- 5. 背景のボロノイ図の境界は、2 つの中心の垂直二等分線。シリーズ 22 のボロノイ図と同じもの

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
