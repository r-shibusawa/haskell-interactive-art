-- ============================================
-- 61-12 自己組織化マップ（SOM）─ 10 × 10 の格子にならんだニューロンが、データに合わせて広がって「地図」になる
--   ① データを 1 つ選ぶ ② いちばん似ているニューロン（BMU）を探す ③ BMU と、格子の上でその近くのニューロンを、データのほうへ少し動かす
--   動かす量 = 学習率 α(t) × 近傍 h = exp(−(格子の距離)² / 2σ(t)²)。α と σ はだんだん小さくする（はじめは大きく全体を、あとで細かく）
--   格子でとなりのニューロンが似たものを表す → 色なら「色の地図」、2 次元の点なら網がデータの形に広がる
--   k-means の中心どうしには「となり」がない。SOM は中心に格子のつながりをつけたもの
-- 操作：1 色の地図 / 2 点の網 / D データの形（2）/ スライダ 学習率 α0・近傍半径 σ0 / スペース 一時停止 / ↑↓ 速さ / R 最初から
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl', sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

gw, gh, tMax :: Int
gw = 10                      -- ★ 格子の大きさ
gh = 10
tMax = 4000                  -- ★ 学習の回数

shapeNames :: [String]
shapeNames = ["square", "ring", "3 blobs"]

data World = World
  { wMode    :: !Int                -- 1 色の地図 / 2 点の網
  , wShape   :: !Int
  , wData    :: !Pts
  , wNeurons :: !Pts                -- ニューロンの重み（格子の左上から横に順に）
  , wT       :: !Int
  , wPaused  :: !Bool
  , wSpeed   :: !Int
  , wQe      :: ![(Int, Double)]    -- (学習の回数, 量子化誤差)。新しいものが先頭
  , wSeed    :: !Int
  , wAlpha   :: !Slider
  , wSigma   :: !Slider
  , wRng     :: !Rng
  , wTest    :: ![(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-12 Self-organizing map" (initial 1280))
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      let w = initial (envW env)
      length (wQe w) `seq` pure w { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

initial :: Float -> World
initial width = reset (World 1 0 V.empty V.empty 0 False 10 [] 121
                        (makeSlider (width - 360) 150 300 0.05 1 0.5 "learning rate alpha0" 2)
                        (makeSlider (width - 360) 215 300 0.5 6 4 "neighbour radius sigma0 (cells)" 1) (seedRng 1) [])

-- データ：1 = ランダムな色 (r, g, b)、2 = 2 次元の点（正方形・輪・3 つのかたまり）
makeData :: Int -> Int -> Int -> Int -> Pts
makeData mode shape n seed = withSeed (fromIntegral seed) $ fmap V.fromList $ case () of
  _ | mode == 1 -> mapM (\_ -> do { r <- rnd; g <- rnd; b <- rnd; pure (U.fromList [r, g, b]) }) [1 .. n]
    | shape == 1 -> mapM (\_ -> do { u <- rnd; v <- rnd; let { a = u * 2 * pi; r = 0.7 + 0.25 * v }; pure (pt2 (r * cos a) (r * sin a)) }) [1 .. n]
    | shape == 2 -> mapM (\i -> do { gx <- gauss; gy <- gauss; let { a = 2 * pi * fromIntegral (i `mod` 3) / 3 }; pure (pt2 (0.6 * cos a + 0.12 * gx) (0.6 * sin a + 0.12 * gy)) }) [0 .. n - 1]
    | otherwise -> mapM (\_ -> do { x <- rnd; y <- rnd; pure (pt2 (-1 + 2 * x) (-1 + 2 * y)) }) [1 .. n]

initNeurons :: Int -> Int -> Pts
initNeurons dim seed = withSeed (fromIntegral seed) $ V.fromList <$> mapM (\_ -> U.fromList <$> mapM (\_ -> (\u -> if dim == 3 then 0.4 + 0.2 * u else -0.1 + 0.2 * u) <$> rnd) [1 .. dim]) [1 .. gw * gh]

reset :: World -> World
reset w = w { wData = d, wNeurons = ns, wT = 0, wQe = [(0, quantErr ns d)], wRng = seedRng (fromIntegral (wSeed w + 2)) }
  where
    d = makeData (wMode w) (wShape w) 400 (wSeed w)
    ns = initNeurons (U.length (V.head d)) (wSeed w + 1)

-- ★ 学習率と近傍の半径のスケジュール（t = 0 → tMax で小さくなる）
alphaAt :: Int -> Double -> Double
alphaAt t a0 = a0 * 0.02 ** (fromIntegral t / fromIntegral tMax)

sigmaAt :: Int -> Double -> Double
sigmaAt t s0 = s0 * (0.4 / s0) ** (fromIntegral t / fromIntegral tMax)

bmuOf :: Pts -> P -> Int                    -- いちばん近いニューロン（BMU）
bmuOf ws x = fst (V.ifoldl' (\(b, bd) i wv -> let d = sqd wv x in if d < bd then (i, d) else (b, bd)) (0, 1 / 0) ws)

neigh :: Int -> Int -> Double -> Double     -- 格子の上の近さ h
neigh i j sg = exp (-fromIntegral (dx * dx + dy * dy) / (2 * sg * sg))
  where
    dx = i `mod` gw - j `mod` gw
    dy = i `div` gw - j `div` gw

-- ★ 1 回の学習：BMU と、格子でその近くのニューロンを x のほうへ動かす
trainStep :: Pts -> P -> Int -> Double -> Double -> Pts
trainStep ws x t a0 s0 = V.imap move ws
  where
    b = bmuOf ws x
    al = alphaAt t a0
    sg = sigmaAt t s0
    move i wv = let f = al * neigh i b sg
                in if f < 1e-6 then wv else U.zipWith (\wd xd -> wd + f * (xd - wd)) wv x

quantErr :: Pts -> Pts -> Double            -- 量子化誤差 = データと BMU の平均距離
quantErr ws pts = meanOf [ edist x (ws V.! bmuOf ws x) | x <- V.toList pts ]

topoErr :: Pts -> Pts -> Double             -- 位相誤差 = 1 番目と 2 番目に近いニューロンが格子でとなりでない割合
topoErr ws pts = meanOf (map one (V.toList pts))
  where
    one x = case map snd (sortBy (\p q -> compare (fst p) (fst q)) [ (sqd wv x, i) | (i, wv) <- zip [0 ..] (V.toList ws) ]) of
      (a : b : _) -> if abs (a `mod` gw - b `mod` gw) + abs (a `div` gw - b `div` gw) == 1 then 0 else 1
      _ -> 1

step :: World -> World
step w
  | wPaused w = w
  | otherwise = go (wSpeed w) w
  where
    go 0 v = v
    go s v
      | wT v >= tMax = v
      | otherwise =
          let (u, r') = rndStep (wRng v)
              pts = wData v
              x = pts V.! min (V.length pts - 1) (floor (u * fromIntegral (V.length pts)))
              ns = trainStep (wNeurons v) x (wT v) (slVal (wAlpha v)) (slVal (wSigma v))
              t' = wT v + 1
              qe = if t' `mod` 100 == 0 then (t', quantErr ns pts) : wQe v else wQe v
          in go (s - 1 :: Int) v { wNeurons = ns, wT = t', wQe = qe, wRng = r' }

draw :: Env -> World -> Picture
draw env w = view env $
  mapPic
  ++ [ drawSlider (wAlpha w), drawSlider (wSigma w)
     , say ("steps " ++ show (wT w) ++ " / " ++ show tMax) bx 270 15 (grayC 220)
     , say ("now alpha = " ++ fmt 3 al ++ "   sigma = " ++ fmt 2 sg ++ " cells") bx 292 15 (grayC 220)
     , plotAxes g "steps" "quantization error (mean data-BMU distance)" [0, fromIntegral tMax] [0, roundSig 2 (y1 g)]
     , Color (rgbC 110 230 190) (polyline 2 [ (gX g (fromIntegral t), gY g q) | (t, q) <- reverse (wQe w) ])
     , say "neighbourhood h (grid distance 0-6 cells)" bx (height - 200) 14 (grayC 205) ]
  ++ concat [ [ Color (rgbC 176 140 255) (rectTL (bx + fromIntegral dd * 42) (height - 100 - 70 * hv) 32 (70 * hv))
              , sayC (show dd) (bx + fromIntegral dd * 42 + 16) (height - 80) 14 (grayC 170) ]
            | dd <- [0 .. 6 :: Int], let hv = f2 (exp (-fromIntegral (dd * dd) / (2 * sg * sg))) ]
  ++ [ say ("Self-organizing map   " ++ (if wMode w == 1 then "1 colour map (400 random colours)" else "2 point net (" ++ headOr "" (drop (wShape w) shapeNames) ++ ")")
            ++ "   QE " ++ fmt 4 (snd (headOr (0, 0) (wQe w))) ++ (if wT w >= tMax then "   done" else "")) 40 40 20 (grayC 240)
     , say ("1 colour map  2 net  D shape  sliders a0, s0 (applied on release / R)  Space pause" ++ (if wPaused w then "d" else "") ++ "  Up/Down speed " ++ show (wSpeed w) ++ "  R restart") 40 66 14 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    width = envW env
    height = envH env
    side = min (width - 480) (height - 190)
    ox = 40
    oy = 100
    ns = wNeurons w
    bx = width - 360
    al = alphaAt (wT w) (slVal (wAlpha w))
    sg = sigmaAt (wT w) (slVal (wSigma w))
    g = G bx 340 300 (max 80 (height - 600)) 0 (fromIntegral tMax) 0 (foldl' max 0 (map snd (wQe w)) * 1.05)
    c01 v = round (255 * clampv 0 1 v) :: Int
    mapPic
      | wMode w == 1 =                 -- ---- 色の地図
          let c = side / fromIntegral gw
          in [ Color (rgbC (c01 (wv U.! 0)) (c01 (wv U.! 1)) (c01 (wv U.! 2))) (rectTL (ox + fromIntegral (i `mod` gw) * c) (oy + fromIntegral (i `div` gw) * c) (c - 2) (c - 2))
             | (i, wv) <- zip [0 ..] (V.toList ns), U.length wv >= 3 ]
      | otherwise =                    -- ---- 点の網
          let v = fitView ox oy side side 0 0 2.4
              sp a = (gX v (ptX a), gY v (ptY a))
          in [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v)) ]
             ++ [ Color (rgbC 150 150 170) (circleAt (gX v (ptX p)) (gY v (ptY p)) 4) | p <- V.toList (wData w) ]
             ++ [ Color (rgbC 176 140 255) (lineW 1.5 (sp a) (sp b))
                | (i, a) <- zip [0 ..] (V.toList ns)
                , j <- [i + 1 | i `mod` gw < gw - 1] ++ [i + gw | i + gw < V.length ns]
                , let b = ns V.! j ]
             ++ [ Color (rgbC 255 220 120) (circleAt (gX v (ptX a)) (gY v (ptY a)) 6) | a <- V.toList ns ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
    in case sliderPress mx my (wAlpha w) of
         Just s -> w { wAlpha = s }
         Nothing -> maybe w (\s -> w { wSigma = s }) (sliderPress mx my (wSigma w))
  EventMotion p | envDown env ->
    let (mx, _) = toP5 env p in w { wAlpha = sliderDrag mx (wAlpha w), wSigma = sliderDrag mx (wSigma w) }
  EventKey (MouseButton LeftButton) Up _ _
    | slDrag (wAlpha w) || slDrag (wSigma w) -> reset w { wAlpha = sliderRelease (wAlpha w), wSigma = sliderRelease (wSigma w) }
  EventKey (Char c) Down _ _
    | c `elem` "12" -> reset w { wMode = fromEnum c - fromEnum '0' }
    | c `elem` "dD" -> reset w { wShape = (wShape w + 1) `mod` length shapeNames, wMode = 2 }
    | c `elem` "rR" -> reset w { wSeed = wSeed w + 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 100 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 1 (wSpeed w `div` 2) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("BMU = the nearest neuron (compared with all, 100 times)", bmuOk)
  , ("neighbourhood h: 1 at the BMU, symmetric, smaller for farther grid cells", hOk)
  , ("quantization error decreases: " ++ fmt 3 qe0 ++ " -> " ++ fmt 3 mid ++ " -> " ++ fmt 3 qe1, qe1 < mid && mid < qe0 && qe1 < 0.5 * qe0)
  , ("weights never leave the data square [-1, 1]^2 (alpha*h <= 1, always an interior point)", inside)
  , ("trained on the square the net does not twist: topographic error " ++ fmt 3 te ++ " < 0.1", te < 0.1)
  , ("colour map: neighbour colour difference " ++ fmt 3 (meanOf nb) ++ " < half of random pairs " ++ fmt 3 (meanOf rp), meanOf nb < 0.5 * meanOf rp) ]
  where
    pts = makeData 2 0 300 5
    ws0 = initNeurons 2 6
    (bmuOk, r1) = runR (do
        oks <- mapM (\_ -> do
          a <- rnd
          b <- rnd
          let x = pt2 (-1 + 2 * a) (-1 + 2 * b)
              bi = bmuOf ws0 x
          pure (not (V.any (\wv -> sqd wv x < sqd (ws0 V.! bi) x) ws0))) [1 .. 100 :: Int]
        pure (and oks)) (seedRng 7)
    hOk = neigh 34 34 2 == 1 && near (neigh 34 35 2) (neigh 35 34 2) 1e-15 && neigh 34 35 2 > neigh 34 36 2 && neigh 34 36 2 > neigh 34 56 2
    qe0 = quantErr ws0 pts
    (wsF, mid, inside, _) = foldl' train (ws0, 0, True, r1) [0 .. tMax - 1]
    train (!ws, !md, !ins, !r) t =
      let (u, r') = rndStep r
          ws' = trainStep ws (pts V.! floor (u * fromIntegral (V.length pts))) t 0.5 4
          md' = if t == tMax `div` 2 then quantErr ws' pts else md
          ins' = ins && not (V.any (\wv -> abs (ptX wv) > 1 || abs (ptY wv) > 1) ws')
      in (ws', md', ins', r')
    qe1 = quantErr wsF pts
    te = topoErr wsF pts
    cs = makeData 1 0 300 8
    wc0 = initNeurons 3 9
    (wc, _) = foldl' (\(ws, r) t -> let (u, r') = rndStep r in (trainStep ws (cs V.! floor (u * fromIntegral (V.length cs))) t 0.5 4, r')) (wc0, seedRng 10) [0, 2 .. tMax - 1]
    nb = [ edist (wc V.! i) (wc V.! (i + 1)) | i <- [0 .. V.length wc - 1], i `mod` gw < gw - 1 ]
    rp = [ edist (wc V.! i) (wc V.! ((i * 37 + 11) `mod` V.length wc)) | i <- [0 .. V.length wc - 1] ]

-- ---------- やってみよう ----------
-- 1. 1（色の地図）で学習を見よう。はじめはばらばらの色が、なめらかなグラデーションの地図になる
-- 2. σ0 を 0.5 にすると（スライダをはなすか R で反映）？ となりを動かさないので地図がまだらになる。これはほぼ k-means
-- 3. 2 → D で「輪」に。10 × 10 の網は輪の形にどう広がる？ 穴のところのニューロンはどうなる？
-- 4. α0 を 1 にすると最初の動きが大きすぎる？ 0.05 だと学習が終わっても広がりきらない？
-- 5. 13 のベクトル量子化（色を減らす）と比べよう。SOM の色の地図はパレットを「並べて」くれる
--    gw, gh を 20 にすると地図はこまかくなるけれど、1 回の学習は重くなる（BMU 探しが gw × gh に比例）

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
