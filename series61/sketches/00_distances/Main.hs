-- ============================================
-- 61-00 距離のいろいろ ─ クラスタリングの「似ている」は「近い」。でも「近さ」の測り方は 1 つではない
--   ユークリッド √(Δx² + Δy²) = 円 / マンハッタン |Δx| + |Δy| = ひし形 / チェビシェフ max(|Δx|, |Δy|) = 正方形
--   ミンコフスキー (|Δx|^p + |Δy|^p)^(1/p)：p = 1 でマンハッタン、2 でユークリッド、∞ でチェビシェフ
--   コサイン距離 1 − cos θ：原点から見た「向き」だけを比べる（長さは無視）。三角不等式が成り立たないので、本当の距離（計量）ではない
--   基準の点（★）から「同じ距離」の線と、いちばん近い点を表示。距離を変えると、いちばん近い点が入れかわる（ボロノイ図にするとシリーズ 22 の 02）
-- 操作：1〜5 距離を切りかえ / クリック 基準の点 / ↑↓ 輪の大きさ / ← → p（5 のとき）/ R 点をまき直す / スペース 4 つの距離の近い点を全部表示
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

metricName :: Int -> String
metricName m = headOr "" (drop m ["Euclidean", "Manhattan", "Chebyshev", "Cosine distance", "Minkowski"])

nPts :: Int
nPts = 40                  -- ★ 点の数

data World = World
  { wPts     :: !Pts                  -- 点（データ）
  , wQuery   :: !P                    -- 基準の点 ★
  , wMetric  :: !Int                  -- 0〜4
  , wRingR   :: !Double               -- 輪の大きさ
  , wPExp    :: !Double               -- ミンコフスキーの p
  , wShowAll :: !Bool
  , wSeedP   :: !Word32
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-00 Distances" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (scatter 3) (pt2 0.6 0.35) 0 0.35 3 True 3 []

scatter :: Word32 -> Pts
scatter s = withSeed s (V.fromList <$> replicateM nPts (do x <- rnd; y <- rnd; pure (pt2 (-2.2 + 4.4 * x) (-1.6 + 3.2 * y))))

-- ★ 距離の式（m = 0〜4、p = ミンコフスキーの p）
metricD :: Int -> Double -> P -> P -> Double
metricD m p a b
  | m == 0 = sqrt (dx * dx + dy * dy)
  | m == 1 = dx + dy
  | m == 2 = max dx dy
  | m == 3 = let la = sqrt (ptX a * ptX a + ptY a * ptY a)     -- コサイン距離（原点から見た向きの差）。長さ 0 のベクトルは 1 とする
                 lb = sqrt (ptX b * ptX b + ptY b * ptY b)
             in if la < 1e-12 || lb < 1e-12 then 1 else clampv 0 2 (1 - (ptX a * ptX b + ptY a * ptY b) / (la * lb))
  | otherwise = (dx ** p + dy ** p) ** (1 / p)
  where
    dx = abs (ptX a - ptX b)
    dy = abs (ptY a - ptY b)

nearestIdx :: Int -> Double -> Pts -> P -> Int
nearestIdx m p pts q = fst (V.ifoldl' (\(b, bd) i x -> let d = metricD m p q x in if d < bd then (i, d) else (b, bd)) (-1, 1 / 0) pts)

-- ★ 「同じ距離」の線：中心 c から距離 r の点を角度ごとに求める（円・ひし形・正方形・その間）
ballPoint :: Int -> Double -> P -> Double -> Double -> (Double, Double)
ballPoint m p c r t = (ptX c + r * ux / unit, ptY c + r * uy / unit)
  where
    ux = cos t
    uy = sin t
    unit = metricD m p (pt2 0 0) (pt2 ux uy)

metricCol :: Int -> (Int, Int, Int)
metricCol m = headOr (255, 255, 255) (drop m [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130)])

colOf :: Int -> Int -> Color
colOf m a = let (r, g, b) = metricCol m in rgbaC r g b a

viewOf :: Env -> G
viewOf env = fitView 40 100 (envW env * 0.62) (envH env - 190) 0 0 4.2

draw :: Env -> World -> Picture
draw env w = view env $
  -- 原点の十字（コサイン距離の基準）
  [ Color (rgbC 60 64 84) (Pictures [lineW 1 (pX g, gY g 0) (pX g + pW g, gY g 0), lineW 1 (gX g 0, pY g) (gX g 0, pY g + pH g)])
  , Color (grayC 80) (frameTL 1 (pX g) (pY g) (pW g) (pH g))
  -- ---- 同じ距離の線
  , Color (colOf mNo 255) rings ]
  -- ---- 点といちばん近い点
  ++ [ Color (colOf m 150) (lineW (if m == mNo then 3 else 1.5) (sp q) (sp (pts V.! i))) | wShowAll w, (m, i) <- zip [0 ..] nn, i >= 0 ]
  ++ [ Color (colOf mNo 255) (lineW 3 (sp q) (sp (pts V.! cur))) | cur >= 0 ]
  ++ concat [ [ Color (if i == cur then colOf mNo 255 else rgbC 220 222 235) (circleAt sx sy (if i == cur then 14 else 8))
              , say (show i) (sx + 7) (sy - 2) 14 (grayC 140) ]
            | (i, p) <- zip [0 ..] (V.toList pts), let (sx, sy) = sp p ]
  ++ [ Color (rgbC 255 240 120) (uncurry starAt (sp q) 12) ]
  -- ---- 右の表：近い順に 6 点
  ++ [ say ("Nearest by " ++ metricName mNo) px0 py0 16 (grayC 230)
     , Pictures [ say h (px0 + cx) (py0 + 26) 14 (grayC 170) | (h, cx) <- zip ["No.", "L2", "L1", "Linf", "cos"] colX ] ]
  ++ [ Pictures (say (show i) (px0 + 4) yy 14 c : [ say (fmt 3 (metricD m pE q (pts V.! i))) (px0 + cx) yy 14 c | (m, cx) <- zip [0 .. 3] (drop 1 colX) ])
     | (r, i) <- zip [0 :: Int ..] order, let yy = py0 + 50 + fromIntegral r * 22, let c = if r == 0 then colOf mNo 255 else grayC 215 ]
  ++ [ say "Nearest point (thin line colour)" px0 (py0 + 196) 14 (grayC 200) ]
  ++ [ say (metricName m ++ " -> " ++ show i) px0 (py0 + 218 + fromIntegral m * 20) 14 (colOf m 255) | (m, i) <- zip [0 ..] nn ]
  ++ [ sayWrap (if diff > 1 then "Depending on the distance, the nearest point splits into " ++ show diff ++ " different points!" else "Right now every distance picks the same nearest point. Move the star!")
               px0 (py0 + 300) (envW env - px0 - 30) 15 (if diff > 1 then rgbC 255 200 120 else rgbC 150 230 160) ]
  -- 単位球の小さな見本
  ++ [ say "Unit balls (distance 1)" px0 (sy0 - 14) 14 (grayC 170) ]
  ++ concat [ [ Color (colOf m 255) (polyline (if m == mNo then 3 else 1.5) [ let (bx, by) = ballPoint m pE (pt2 0 0) 1 (2 * pi * fromIntegral i / 80) in (cx + f2 bx * 32, cy - f2 by * 32) | i <- [0 .. 80 :: Int] ])
              , sayC (if m == 4 then "p = " ++ show (round pE :: Int) else headOr "" (drop m ["L2", "L1", "Linf"])) cx (cy + 56) 14 (grayC 180) ]
            | (j, m) <- zip [0 :: Int ..] [1, 0, 4, 2], let cx = px0 + 45 + fromIntegral j * 82, let cy = sy0 + 50 ]
  -- ---- 文字
  ++ [ say ("Distances   now: " ++ metricName mNo ++ (if mNo == 4 then " (p = " ++ show (round pE :: Int) ++ ")" else "") ++ (if mNo == 3 then "   <- compares direction only (origin = cross)" else "")) 40 40 20 (grayC 240)
     , say "1-5 distance   click: place the star   Up/Down ring size   <- -> p (for 5)   R new points   Space thin lines on/off" 40 66 15 (grayC 170)
     , say ("Rings = distance " ++ fmt 2 rr ++ ", " ++ fmt 2 (2 * rr) ++ ", " ++ fmt 2 (3 * rr) ++ " from the star (cosine: half of that). k-means (01) uses squared Euclidean.") 40 (envH env - 50) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    g = viewOf env
    pts = wPts w
    q = wQuery w
    mNo = wMetric w
    pE = wPExp w
    rr = wRingR w
    sp p = (gX g (ptX p), gY g (ptY p))
    nn = [ nearestIdx m pE pts q | m <- [0 .. 3] ]
    cur = nearestIdx mNo pE pts q
    diff = length (L.nub nn)
    px0 = pX g + pW g + 40
    py0 = 120
    colX = [0, 44, 120, 196, 272]
    sy0 = envH env - 210
    order = take 6 (L.sortBy (\a b -> compare (metricD mNo pE q (pts V.! a)) (metricD mNo pE q (pts V.! b))) [0 .. V.length pts - 1])
    rings
      | mNo == 3 =                 -- コサイン：原点からのくさび形（向きの差が同じ線は、原点からの半直線）
          let th = atan2 (ptY q) (ptX q)
          in Pictures [ lineW 2 (gX g 0, gY g 0) (gX g (6 * cos (th + s * a)), gY g (6 * sin (th + s * a)))
                      | k <- [1 .. 3], let d = min 2 (rr * k * 0.5), let a = acos (clampv (-1) 1 (1 - d)), s <- [-1, 1] ]
      | otherwise = Pictures [ polyline 2 [ let (bx, by) = ballPoint mNo pE q (rr * k) (2 * pi * fromIntegral i / 160) in (gX g bx, gY g by) | i <- [0 .. 160 :: Int] ] | k <- [1, 2, 3] ]

-- 星の形（★）：中心 (x, y)、外側の半径 r
starAt :: Float -> Float -> Float -> Picture
starAt x y r = Translate x y $ Pictures $
  Polygon inner : [ Polygon [outer k, inner' k, inner' (k + 4)] | k <- [0 .. 4] ]
  where
    ang k = -pi / 2 + 2 * pi * fromIntegral k / 5
    outer k = (r * cos (ang k), r * sin (ang k))
    inner' k = let a = ang k + pi / 5 in (0.42 * r * cos a, 0.42 * r * sin a)
    inner = [ inner' k | k <- [0 .. 4 :: Int] ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> place p
  EventMotion p | envDown env -> place p
  EventKey (Char c) Down _ _
    | c `elem` "12345" -> w { wMetric = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> let s = wSeedP w + 1 in w { wSeedP = s, wPts = scatter s }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRingR = min 1.2 (wRingR w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRingR = max 0.1 (wRingR w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPExp = min 12 (wPExp w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPExp = max 1 (wPExp w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShowAll = not (wShowAll w) }
  _ -> w
  where
    g = viewOf env
    place p = let (mx, my) = toP5 env p in if inBox g mx my then w { wQuery = pt2 (invX g mx) (invY g my) } else w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 100・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest = withSeed 100 $ do
  let rp = do { x <- rnd; y <- rnd; pure (pt2 (-3 + 6 * x) (-3 + 6 * y)) }
  trip <- replicateM 400 ((,,) <$> rp <*> rp <*> rp)
  let axiom m (a, b, c) = let ab = metricD m 3 a b; ba = metricD m 3 b a; ac = metricD m 3 a c; bc = metricD m 3 b c
                          in ab >= 0 && metricD m 3 a a == 0 && abs (ab - ba) < 1e-12 && ac <= ab + bc + 1e-12
      axioms = and [ axiom m t | m <- [0, 1, 2, 4], t <- trip ]
      a = pt2 1 0
      b = pt2 1 1
      c = pt2 0 1
  cosPairs <- replicateM 200 ((,) <$> rp <*> rp)
  let cosOk = and [ d >= 0 && d <= 2 && abs (d - metricD 3 3 v u) < 1e-12 | (u, v) <- cosPairs, let d = metricD 3 3 u v ]
  ordPairs <- replicateM 300 ((,) <$> rp <*> rp)
  let ordOk = and [ li <= l2 + 1e-12 && l2 <= l1 + 1e-12 && l1 <= sqrt 2 * l2 + 1e-12 && l1 <= 2 * li + 1e-12
                  | (u, v) <- ordPairs, let l2 = metricD 0 3 u v, let l1 = metricD 1 3 u v, let li = metricD 2 3 u v ]
      u0 = pt2 0.3 (-1.2)
      v0 = pt2 (-0.7) 0.4
      q0 = pt2 0 0
      pa = pt2 0.9 0.9
      pb = pt2 1.2 0
  lens <- replicateM 100 (do { wv <- rp; s <- rnd; pure (wv, 0.1 + 5 * s) })
  let lenOk = and [ abs (metricD 3 3 wv (U.map (* s) wv)) <= 1e-12 | (wv, s) <- lens ]
  pure
    [ ("Euclidean, Manhattan, Chebyshev, Minkowski (p = 3) satisfy the metric axioms (>= 0, d(x, x) = 0, symmetric, triangle inequality; 400 triples)", axioms)
    , ("cosine distance is symmetric and in 0..2, but (1,0)->(1,1)->(0,1) breaks the triangle inequality: " ++ fmt 3 (metricD 3 3 a c) ++ " > " ++ fmt 3 (metricD 3 3 a b + metricD 3 3 b c)
      , cosOk && metricD 3 3 a c > metricD 3 3 a b + metricD 3 3 b c)
    , ("order: Linf <= L2 <= L1 <= sqrt2 L2, L1 <= 2 Linf (2D)", ordOk)
    , ("Minkowski: p = 1 is Manhattan, p = 2 is Euclidean, p = 64 is within 2% of Chebyshev"
      , near (metricD 4 1 u0 v0) (metricD 1 3 u0 v0) 1e-12 && near (metricD 4 2 u0 v0) (metricD 0 3 u0 v0) 1e-12 && abs (metricD 4 64 u0 v0 / metricD 2 3 u0 v0 - 1) < 0.02)
    , ("nearest point swaps: star = origin, (0.9, 0.9) vs (1.2, 0). L1 picks the latter, Linf the former"
      , metricD 1 3 q0 pb < metricD 1 3 q0 pa && metricD 2 3 q0 pa < metricD 2 3 q0 pb)
    , ("cosine distance ignores length: d(x, s x) = 0 (s > 0)", lenOk) ]

-- ---------- やってみよう ----------
-- 1. 1 と 3 を切りかえながら ★ を動かし、いちばん近い点が入れかわる場所を探そう
-- 2. 5（ミンコフスキー）で ← → を押して p を 1 → 12 に。単位球はひし形 → 円 → 正方形に変わる
-- 3. 4（コサイン）では、原点から同じ向きにある点は、遠くても「同じ」。文書の単語の数を比べるときに使う
-- 4. 右の表で、L∞ ≤ L2 ≤ L1 がいつも成り立っているか確かめよう
-- 5. 01 の k-means は「ユークリッド距離の 2 乗」で近さを測る。metricD を変えてマンハッタン距離にすると、平均ではなく中央値（k-medians）になる

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
