-- ============================================
-- 61-07 平均値シフト ─ 各点を「近くの点の重みつき平均」の位置へ動かす、をくり返すと、密度の山のてっぺん（モード）に集まる
--   重み = ガウス核 exp(−|x − x_i|² / 2h²)。h = カーネル幅（どれくらい遠くまで見るか）
--   動く量 m(x) − x は、密度 f（カーネル密度推定）の坂の向き ∇f に比例する → 山登り。同じ山に登った点 = 1 つのクラスタ
--   k を決めなくてよい。h が小さいと山がたくさん、大きいと山がなだらかに合わさって数が減る。背景 = 密度 f の等高線
-- 操作：スライダ / ← → カーネル幅 h / スペース やり直す / D データ / R 新しいデータ / クリック 点を足す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 240                                  -- ★ 点の数

dKinds :: [String]
dKinds = ["blobs", "density", "sizes", "moons"]

data World = World
  { wData    :: !DS
  , wCloud   :: !Cloud                      -- データの点（x の列, y の列）
  , wWalkers :: !Pts                        -- 動いている点
  , wIter    :: !Int
  , wDone    :: !Bool
  , wModes   :: ![P]                        -- 山のてっぺん（モード）
  , wLabels  :: !(U.Vector Int)
  , wDataNo  :: !Int
  , wSeedD   :: !Int
  , wBw      :: !Slider                     -- カーネル幅 h
  , wHeat    :: !(Maybe (Int, Int, Picture))   -- 密度の画像（幅, 高さ, 画像）
  , wHeatDirty :: !Bool
  , wHeatRows :: !(Maybe [U.Vector Double])   -- 作りかけの密度（できた行）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-07 Mean shift" (world0 1280))
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 (envW env)) { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: Float -> World
world0 sw = newData (World (DS V.empty U.empty 0) (U.empty, U.empty) V.empty 0 False [] U.empty 0 71 (makeSlider (sw - 340) 150 280 0.08 1.2 0.3 "kernel width h" 2) Nothing True Nothing [])

newData :: World -> World
newData w = restart w { wData = withSeed 1 (makeDataset (headOr "blobs" (drop (wDataNo w) dKinds)) nPts (fromIntegral (wSeedD w))) }

restart :: World -> World
restart w = w { wCloud = cloudOf (dsPts (wData w)), wWalkers = dsPts (wData w), wIter = 0, wDone = False, wModes = [], wLabels = U.replicate (V.length (dsPts (wData w))) (-1), wHeatDirty = True }

-- 点の集まりを x の列と y の列にしておく（速く計算するため）
type Cloud = (U.Vector Double, U.Vector Double)

cloudOf :: Pts -> Cloud
cloudOf pts = (U.generate (V.length pts) (\i -> ptX (pts V.! i)), U.generate (V.length pts) (\i -> ptY (pts V.! i)))

-- ★ カーネル密度推定 f(x) = 1/(n·2πh²) Σ exp(−|x − x_i|² / 2h²)
kde :: P -> Cloud -> Double -> Double
kde x (cx, cy) h = go 0 0 / (fromIntegral n * 2 * pi * h * h)
  where
    n = U.length cx
    (a, b) = (ptX x, ptY x)
    go !i !s | i >= n = s
             | otherwise = let dx = a - U.unsafeIndex cx i; dy = b - U.unsafeIndex cy i
                           in go (i + 1) (s + exp (-(0 + dx * dx + dy * dy) / (2 * h * h)))

-- ★ 平均値シフトの 1 歩：重みつき平均へ
msStep :: P -> Cloud -> Double -> P
msStep x (cx, cy) h = go 0 0 0 0
  where
    n = U.length cx
    (a, b) = (ptX x, ptY x)
    go !i !w !sx !sy
      | i >= n = if w > 1e-300 then pt2 (sx / w) (sy / w) else x
      | otherwise = let px = U.unsafeIndex cx i; py = U.unsafeIndex cy i
                        dx = a - px; dy = b - py
                        k = exp (-(0 + dx * dx + dy * dy) / (2 * h * h))
                    in go (i + 1) (w + k) (sx + k * px) (sy + k * py)

-- 全部の点を 1 歩ずつ動かす（2 つ目 = いちばん大きく動いた量）
moveAll :: Pts -> Cloud -> Double -> (Pts, Double)
moveAll ws c h = let ys = V.map (\x -> msStep x c h) ws in (ys, V.foldl' max 0 (V.zipWith edist ws ys))

runToEnd :: Pts -> Double -> Int -> Pts       -- 全部の点を止まるまで動かす
runToEnd pts h maxIt = go 0 pts
  where
    c = cloudOf pts
    mi = if maxIt <= 0 then 500 else maxIt
    go t ws | t >= mi = ws
            | otherwise = let (ys, mv) = moveAll ws c h in if mv < 1e-5 * h then ys else go (t + 1) ys

groupModes :: Pts -> Double -> ([P], U.Vector Int)   -- h/2 より近くに止まった点は同じ山
groupModes ws h = let (ms, labs) = V.foldl' add ([], []) ws in (ms, U.fromList (reverse labs))
  where add (ms, labs) x = case [ j | (j, m) <- zip [0 ..] ms, edist x m < h / 2 ] of
          (j : _) -> (ms, j : labs)
          [] -> (ms ++ [x], length ms : labs)

mainView :: Env -> G
mainView env = fitView 40 100 (envW env - 440) (envH env - 190) 0 0 4.4

-- 1 フレームに 1 歩。密度の画像は必要なときだけ作りなおす
step :: Env -> World -> World
step env w0 = walk (heatStep env w0)
  where
    walk w
      | wDone w = w
      | otherwise =
          let h = slVal (wBw w)
              (ys, mv) = moveAll (wWalkers w) (wCloud w) h
              w' = w { wWalkers = ys, wIter = wIter w + 1 }
          in if mv < 1e-4 * h || wIter w' > 400
               then let (ms, labs) = groupModes ys h in w' { wDone = True, wModes = ms, wLabels = labs }
               else w'

-- 密度 f の等高線（4 ピクセルに 1 回だけ計算）。exp(−(dx² + dy²)/2h²) = exp(−dx²/2h²)·exp(−dy²/2h²) と分けて速く計算する
-- 密度の画像の大きさ（4 ピクセルに 1 回だけ計算）
heatSize :: Env -> (Int, Int)
heatSize env = let v = mainView env in (ceiling (pW v / 4), ceiling (pH v / 4))

heatRowsPerFrame :: Int
heatRowsPerFrame = 16                       -- 1 フレームに計算する行の数（重くならないように数フレームに分ける）

-- 密度の画像を少しずつ作る。全部の行ができたら等高線つきの画像にする
heatStep :: Env -> World -> World
heatStep env w
  | wHeatDirty w && not (slDrag (wBw w)) = heatStep env w { wHeatDirty = False, wHeatRows = Just [] }
  | otherwise = case wHeatRows w of
      Nothing -> w
      Just done ->
        let r0 = sum (map (\c -> U.length c `div` gw) done)
            r1 = min gh (r0 + heatRowsPerFrame)
            chunk = heatVals (mainView env) gw gh r0 r1 (slVal (wBw w)) (dsPts (wData w))
            done' = done ++ [chunk]
        in if r1 >= gh then w { wHeatRows = Nothing, wHeat = Just (gw, gh, makeHeat gw gh (U.concat done')) }
           else chunk `seq` w { wHeatRows = Just done' }
  where (gw, gh) = heatSize env

-- 密度 f の等高線（明るさ = f、10 段ごとの境目を明るく）
makeHeat :: Int -> Int -> U.Vector Double -> Picture
makeHeat gw gh vals = pixelImage gw gh 1 pix
  where
    mx = U.foldl' max 1e-12 vals
    lv = U.map (\a -> floorI (a / mx * 10)) vals
    pix x y =
      let q = x + gw * y
          u = U.unsafeIndex vals q / mx
          l = U.unsafeIndex lv q
          edge = (x + 1 < gw && U.unsafeIndex lv (q + 1) /= l) || (y + 1 < gh && U.unsafeIndex lv (q + gw) /= l)
          e a = if edge then a else 0
          b c = fromIntegral (max 0 (min 255 (floorI c)))
      in (b (22 + 60 * u + e 50), b (24 + 40 * u + e 40), b (34 + 110 * u + e 50))

-- 格子の行 ya〜yb−1 の密度 f（f(x) は kde と同じ）。exp(−(dx² + dy²)/2h²) = exp(−dx²/2h²)·exp(−dy²/2h²) と分けて速く計算する
heatVals :: G -> Int -> Int -> Int -> Int -> Double -> Pts -> U.Vector Double
heatVals v gw gh ya yb h pts = U.generate (gw * (yb - ya)) cell
  where
    n = V.length pts
    pxs = U.generate n (\i -> ptX (pts V.! i))
    pys = U.generate n (\i -> ptY (pts V.! i))
    c = 1 / (2 * h * h)
    ex = U.generate (gw * n) (\q -> let (x, i) = q `divMod` n; d = x0 v + (fromIntegral x + 0.5) / fromIntegral gw * (x1 v - x0 v) - U.unsafeIndex pxs i in exp (-d * d * c))
    ey = U.generate ((yb - ya) * n) (\q -> let (y, i) = q `divMod` n; d = y1 v - (fromIntegral (y + ya) + 0.5) / fromIntegral gh * (y1 v - y0 v) - U.unsafeIndex pys i in exp (-d * d * c))
    norm = fromIntegral (max 1 n) * 2 * pi * h * h
    cell :: Int -> Double
    cell q = let (y, x) = q `divMod` gw
                 !bx = x * n
                 !by = y * n
                 go :: Int -> Double -> Double
                 go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex ex (bx + i) * U.unsafeIndex ey (by + i))
             in go 0 0 / norm

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (pX v + pW v / 2) (pY v + pH v / 2) (Scale (pW v / fromIntegral hw) (negate (pH v / fromIntegral hh)) pic) | Just (hw, hh, pic) <- [wHeat w] ]
  ++ [ Color (grayC 80) (frameTL 1 (pX v) (pY v) (pW v) (pH v))
     , Color (rgbaC 255 255 255 40) (Pictures [ Line [(gX v (ptX p), gY v (ptY p)), (gX v (ptX q), gY v (ptY q))] | (p, q) <- zip (V.toList pts) (V.toList (wWalkers w)) ])
     , drawPts v pts (if wDone w then Just (wLabels w) else Nothing) 6 (if wDone w then 255 else 150)
     , Color (rgbC 255 240 150) (Pictures [ circleAt (gX v (ptX q)) (gY v (ptY q)) 5 | q <- V.toList (wWalkers w) ]) ]
  ++ concat [ let (cx, cy) = (gX v (ptX m), gY v (ptY m)); tri = [(cx, cy - 11), (cx - 9, cy + 6), (cx + 9, cy + 6)]
              in [Color (palC j) (Polygon tri), Color (grayC 20) (thickPath 2 (tri ++ take 1 tri))]
            | (j, m) <- zip [0 ..] (wModes w) ]
  -- カーネル幅の円（マウスの位置）
  ++ [ Color (rgbaC 255 255 255 80) (Pictures [ringAt mx my (2 * hp) 1, ringAt mx my (4 * hp) 1]) | inBox v mx my ]
  -- ---- 右
  ++ [ drawSlider (wBw w) ]
  ++ [ say s bx (210 + 20 * k) 15 (grayC 220) | (k, s) <- zip [0 ..] status ]
  ++ [ sayWrap "Yellow = moving points, lines = path from the start, triangles = density peaks (modes). Background brightness = density f, stripes = contours. White circles = h and 2h around the mouse." bx 300 300 14 (grayC 150)
     , sayWrap "Nearest peak = one cluster. m(x) - x = h^2 grad f(x) / f(x), so points climb straight up the density slope (checked in the self-test)." bx 470 300 14 (grayC 150)
  -- ---- 文字
     , say ("Mean shift   h = " ++ fmt 2 h ++ "   -> " ++ (if wDone w then show (length (wModes w)) else "?") ++ " peaks   " ++ dataName (headOr "" (drop (wDataNo w) dKinds))) 40 40 20 (grayC 240)
     , say "Slider or <- -> kernel width h   Space restart   D data   R new data   click: add point" 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    v = mainView env
    pts = dsPts (wData w)
    h = slVal (wBw w)
    hp = f2 h * (gX v 1 - gX v 0)
    (mx, my) = mouseP env
    bx = envW env - 340
    status = if wDone w
               then ["stopped: " ++ show (wIter w) ++ " steps", "number of peaks (modes) = " ++ show (length (wModes w)), "ARI (vs truth) = " ++ fmt 3 (ariScore (wLabels w) (dsTruth (wData w)))]
               else ["climbing... " ++ show (wIter w) ++ " steps"]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p0 ->
    let (mx, my) = toP5 env p0
        v = mainView env
    in case sliderPress mx my (wBw w) of
         Just s -> w { wBw = s }
         Nothing | inBox v mx my -> restart w { wData = appendDS (wData w) [(pt2 (invX v mx) (invY v my), 0)] }
                 | otherwise -> w
  EventMotion p0 | slDrag (wBw w) -> w { wBw = sliderDrag (fst (toP5 env p0)) (wBw w) }
  EventKey (MouseButton LeftButton) Up _ _ | slDrag (wBw w) -> restart w { wBw = sliderRelease (wBw w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> nudge 0.02
  EventKey (SpecialKey KeyLeft) Down _ _ -> nudge (-0.02)
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> newData w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "rR" -> newData w { wSeedD = wSeedD w + 1 }
  _ -> w
  where nudge dv = let s = wBw w in restart w { wBw = s { slVal = clampv (slLo s) (slHi s) (slVal s + dv) } }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("one mean-shift step never lowers the density f (40 points x 25 steps)", up)
  , ("shift m(x) - x = h^2 grad f(x) / f(x) (vs numerical derivative, 20 points)", gradOk)
  , ("at the stopping points (modes) m(x) = x (slope = 0)", still)
  , ("a mode is a density peak: f is larger than in 8 directions around it", peak)
  , ("3 separated blobs (h = 0.3): exactly 3 peaks, ARI = " ++ fmt 3 ari ++ " >= 0.95", length ms == 3 && ari >= 0.95)
  , ("larger h gives fewer peaks: " ++ concatMap (\(i, c) -> (if i > 0 then " -> " else "") ++ show c) (zip [0 :: Int ..] cnt), and (zipWith (<=) (drop 1 cnt) cnt) && last cnt == 1) ]
  where
    d = withSeed 1 (makeDataset "blobs" 90 77)
    pts = dsPts d
    c = cloudOf pts
    h = 0.3
    rp = do a <- rnd; b <- rnd; pure (pt2 (-2 + 4 * a) (-2 + 4 * b))
    (starts, gpts) = withSeed 3 (do a <- mapM (const rp) [1 .. 40 :: Int]; b <- mapM (const rp) [1 .. 20 :: Int]; pure (a, b))
    up = and [ kde y c h >= kde x c h * (1 - 1e-12) | x0' <- starts, (x, y) <- take 25 (let xs = iterate (\x -> msStep x c h) x0' in zip xs (drop 1 xs)) ]
    e = 1e-5
    gradOk = and [ not (f > 1e-6) || (abs (ptX m - ptX x - h * h * gx / f) <= 1e-5 && abs (ptY m - ptY x - h * h * gy / f) <= 1e-5)
                 | x <- gpts
                 , let f = kde x c h
                       gx = (kde (pt2 (ptX x + e) (ptY x)) c h - kde (pt2 (ptX x - e) (ptY x)) c h) / (2 * e)
                       gy = (kde (pt2 (ptX x) (ptY x + e)) c h - kde (pt2 (ptX x) (ptY x - e)) c h) / (2 * e)
                       m = msStep x c h ]
    (ms, labs) = groupModes (runToEnd pts h 0) h
    still = and [ edist (msStep m c h) m <= 1e-4 * h | m <- ms ]
    peak = and [ kde (pt2 (ptX m + 0.05 * h * cos a) (ptY m + 0.05 * h * sin a)) c h <= kde m c h * (1 + 1e-9) | m <- ms, a <- [0 .. 7] ]
    ari = ariScore labs (dsTruth d)
    cnt = [ length (fst (groupModes (runToEnd pts hh 150) hh)) | hh <- [0.15, 0.3, 0.6, 1.2, 2.4] ]

-- ---------- やってみよう ----------
-- 1. スライダで h をゆっくり大きくして、山の数が 多い → 3 → 1 と変わるところを探そう（手を離すと計算しなおす）
-- 2. 「密度がちがう」（D）では、h をどうすれば 3 つに分かれる？ 1 つの h では難しいことも
-- 3. 三日月（D）では、山は三日月の上に何個できる？ 平均値シフトも「丸い山」を探している
-- 4. 何もないところをクリックして点を 1 つ足すと、その点だけの小さな山ができる？（h しだい）
-- 5. 05 の木の高さと同じように、h は「どれくらいの細かさで見るか」の目盛り

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
