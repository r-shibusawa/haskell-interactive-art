-- ============================================
-- 67-00 確率の三角形 ─ 3 つの結果（グー・チョキ・パー）の確率 (p1, p2, p3) は、合計が 1 なので正三角形の中の「1 つの点」
-- 頂点 = 必ずその結果、まん中 = 3 つが同じ確率。背景の等高線はエントロピー H(p) = −Σ p log p（偏りの少なさ）
-- 「分布は点、分布の集まりは図形」。その図形の形や距離を調べるのが情報幾何学（コイン 1 枚のエントロピーはシリーズ 6 の 01）
-- 操作：ドラッグ 点を動かす / スペース サンプルを止める・動かす / 1 2 3 頂点へ / C まん中へ / ↑↓ サンプルの速さ / B ビット ⇔ ナット
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

names :: [String]
names = ["Rock", "Scissors", "Paper"]          -- ★ 3 つの結果の名前（グー・チョキ・パー）

sampleN :: Int
sampleN = 60                                  -- ★ 右下に並べるサンプルの数

data World = World
  { wP        :: [Double]                     -- いまの分布 p
  , wField    :: Maybe Field                  -- エントロピーの等高線（画面の大きさが変わったら作り直す）
  , wSize     :: !(Float, Float)
  , wDragging :: !Bool
  , wSamples  :: [Int]                        -- 新しい順
  , wCounts   :: [Double]                     -- 出た回数（多くなったら半分にする）
  , wTotal    :: !Int
  , wRunning  :: !Bool
  , wSpeed    :: !Int
  , wBits     :: !Bool
  , wRng      :: !Rng
  , wTest     :: [Check] }

main :: IO ()
main = runSketch (sketch "67-00 Probability simplex" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (layout env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあとの乱数の列をそのまま使う（種 5 で 10 万回引いたあと）
world0 :: World
world0 = World [0.5, 0.3, 0.2] Nothing (0, 0) False [] [0, 0, 0] 0 True 4 True rng0 []
  where rng0 = snd (runR (loopR 100000 (\_ -> rnd) 0) (rngOf 5))

frameOf :: Env -> SFrame
frameOf env = let w = d2 (envW env)
                  h = d2 (envH env)
                  side = min (w * 0.46) ((h - 170) * 1.1)
              in simplexFrame (w * 0.3) (h * 0.56) side

-- ★ エントロピーの等高線：最大 log 3 ≈ 1.10 ナット。間隔 0.1
layout :: Env -> World -> World
layout env w = w { wField = Just (simplexField (frameOf env) entropyH fieldOpt { foLevel = 0.1, foVmax = log 3, foC0 = (30, 45, 75), foC1 = (210, 120, 60) })
                 , wSize = (envW env, envH env) }

step :: Env -> World -> World
step env w0 =
  let w = if wSize w0 /= (envW env, envH env) then layout env w0 else w0
  in if wRunning w then fst (runR (loopR (wSpeed w) addSample w) (wRng w)) else w

-- サンプルを 1 つ引いて、列と回数に足す
addSample :: World -> R World
addSample w = R $ \g ->
  let (i, g') = runR (sampleIndex (wP w)) g
      cs = modifyAt i (+ 1) (wCounts w)
      tot = wTotal w + 1
      (cs', tot') = if tot > 20000 then let h = map (/ 2) cs in (h, jsRound (sumOf h)) else (cs, tot)
  in (w { wSamples = take sampleN (i : wSamples w), wCounts = forceL cs', wTotal = tot', wRng = g' }, g')

setP :: [Double] -> World -> World
setP p w = w { wP = clampSimplex p 0, wCounts = [0, 0, 0], wTotal = 0 }

empirical :: World -> [Double]
empirical w = if wTotal w > 0 then map (/ i2d (wTotal w)) (wCounts w) else [1 / 3, 1 / 3, 1 / 3]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawField (wField w)
  , hudTitle "Probability simplex: a distribution is one point"
             ("Drag move the point   1 2 3 to a vertex   C center   Space samples " ++ (if wRunning w then "running" else "paused")
              ++ "   Up/Down speed " ++ show (wSpeed w) ++ "   B units (" ++ (if wBits w then "bits" else "nats") ++ ")")
  , drawSimplexFrame fr names ]
  -- 点から 3 つの辺への垂線：長さが確率に比例する（高さ × p_i）
  ++ [ Color (colA 200 (outC i)) (dashLine 2 5 5 (f2 px, f2 py) (f2 (ax + t * (bx - ax)), f2 (ay + t * (by - ay))))
     | i <- [0 .. 2], let (ax, ay) = vtx fr ((i + 1) `mod` 3), let (bx, by) = vtx fr ((i + 2) `mod` 3)
     , let t = ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int)) ]
  ++ [ simplexDot fr emp violet 10 "observed"
     , simplexDot fr (wP w) orange 18 "p"
     -- ---- 右側：棒グラフ・数値・サンプル列
     , drawBars (wP w) (Box rx 110 rw 150) barOpt { boNames = names, boGhost = emp }
     , say ("p = " ++ fmtP 3 (wP w) ++ "  (sum " ++ fmtN 3 (sumOf (wP w)) ++ ")") (f2 rx) 310 16 (col ink)
     , say ("Entropy H(p) = " ++ fmtN 3 (h * cv) ++ unit) (f2 rx) 336 16 (col ink)
     , say ("(max " ++ fmtN 3 (log 3 * cv) ++ unit ++ " at the center)") (f2 rx) 358 14 (col dimc)
     , say ("White frames = observed shares (" ++ show (wTotal w) ++ " draws)") (f2 rx) 380 14 (col dimc)
     , say ("Gap to p (total variation) = " ++ fmtN 4 (tvDist emp (wP w))) (f2 rx) 400 14 (col dimc)
     , say "  shrinks like 1/sqrt(draws); more draws -> closer to p" (f2 rx) 418 14 (col dimc) ]
  -- サンプルの列（新しいほど左）
  ++ [ Pictures [ Color (colA (230 - 2 * i2f k) (outC s)) (rectTL x y (cw - 3) (cw - 3))
                , sayM (take 1 (at' names s "")) (x + (cw - 3) / 2) (y + (cw - 3) / 2) 14 (col bgC) ]
     | (k, s) <- zip [0 :: Int ..] (wSamples w), let x = f2 rx + i2f (k `mod` 15) * cw, let y = 436 + i2f (k `div` 15) * (cw + 4) ]
  ++ [ say "Every point is a lottery box: the sequences differ," (f2 rx) (envH env - 78) 14 (col dimc)
     , say "but the shares gather at the same place" (f2 rx) (envH env - 60) 14 (col dimc)
     , drawSelfTest env (wTest w) ]
  where
    fr = frameOf env
    emp = empirical w
    (px, py) = baryToXY fr (wP w)
    rx = d2 (envW env) * 0.6
    rw = d2 (envW env) * 0.34
    h = entropyH (wP w)
    unit = if wBits w then " bits" else " nats"
    cv = if wBits w then 1 / log 2 else 1
    cw = min 26 (f2 rw / 15)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (x, y) = baryToXY fr (wP w)
        b = xyToBary fr mx my
        dr = sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int)) < 40 || minimum b >= 0
    in (if dr then setP b else id) w { wDragging = dr }
  EventMotion _ | wDragging w -> setP (xyToBary fr mx my) w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 40 (wSpeed w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 1 (wSpeed w - 1) }
  EventKey (Char c) Down _ _
    | c == '1' -> setP [1, 0, 0] w
    | c == '2' -> setP [0, 1, 0] w
    | c == '3' -> setP [0, 0, 1] w
    | c == 'c' || c == 'C' -> setP [1 / 3, 1 / 3, 1 / 3] w
    | c == 'b' || c == 'B' -> w { wBits = not (wBits w) }
  _ -> w
  where fr = frameOf env
        (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. 点を辺の上に置くと、どの結果が一度も出なくなる？ 頂点の近くでは列はどう見える？
-- 2. エントロピーが同じ値の点を 3 つ探そう（同じ等高線の上）。棒グラフはどれくらいちがう？
-- 3. sampleN を 300 にして、まん中と頂点の近くで「出た割合」の点の揺れ方を比べよう
-- 4. 4 つの結果なら三角形ではなく正四面体（三角すい）の中の点になる。何次元の図形になる？
-- 5. 次の 01 では、2 つの点の「近さ」を KL ダイバージェンスで測る

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("三角形の点 ⇔ (p1, p2, p3) の行き来が 300 個の分布で一致（誤差 1e-12）、合計はいつも 1", okRound && okSum)
  , ("頂点 = 必ず 1 つの結果、まん中 = (1/3, 1/3, 1/3)"
    , and [ abs (at' (xyToBary f vx vy) i 0 - 1) < 1e-12 | i <- [0 .. 2], let (vx, vy) = vtx f i ] && abs (at' (xyToBary f (sfCx f) (sfCy f)) 0 0 - 1 / 3) < 1e-12)
  , ("エントロピー：頂点で 0、まん中で log 3 = " ++ fmtN 4 (log 3) ++ "、300 個のでたらめな分布はどれも log 3 以下"
    , entropyH [1, 0, 0] == 0 && abs (entropyH [1 / 3, 1 / 3, 1 / 3] - log 3) < 1e-12 && okMax)
  , ("コイン（p, 1−p, 0）のエントロピーは p = 0.5 で 1 ビット（シリーズ 6 の 01 と同じ）", abs (bitsOf (entropyH [0.5, 0.5, 0]) - 1) < 1e-12)
  , ("10 万回サンプルすると出た割合は p から 0.01 以内（" ++ fmtP 3 shares ++ "）", and (zipWith (\v q -> abs (v - q) < 0.01) shares p5))
  , ("辺から辺へ動くとエントロピーは途中で最大になり、端の値（0）より大きい", g > 0.9 && entropyH [0, 0, 1] == 0) ]
  where
    f = simplexFrame 400 400 300
    ps = withSeed 11 (mapM (\_ -> rndDist 3 0) [1 .. 300 :: Int])
    rt p = let (x, y) = baryToXY f p in xyToBary f x y
    okRound = and [ maximum (zipWith (\a b -> abs (a - b)) (rt p) p) <= 1e-12 | p <- ps ]
    okSum = and [ abs (sumOf (rt p) - 1) <= 1e-12 | p <- ps ]
    okMax = and [ entropyH p <= log 3 + 1e-12 | p <- ps ]
    p5 = [0.5, 0.3, 0.2]
    n = 100000 :: Int
    cnt = withSeed 5 (loopR n (\c -> do i <- sampleIndex p5
                                        pure (U.accum (+) c [(i, 1 :: Int)])) (U.replicate 3 0))
    shares = [ i2d (cnt U.! i) / i2d n | i <- [0 .. 2] ]
    g = foldl' max 0 [ entropyH [t * 0.6, t * 0.4, 1 - t] | k <- [0 .. 1000 :: Int], let t = i2d k / 1000 ]

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 確率の三角形（3 つの結果）：頂点 1 = 左下、2 = 右下、3 = 上。点 = 確率で重みをつけた頂点の平均（重心座標）
data SFrame = SFrame { sfCx :: !Double, sfCy :: !Double, sfSide :: !Double, sfV :: [(Double, Double)] }

simplexFrame :: Double -> Double -> Double -> SFrame
simplexFrame cx cy side =
  let h = side * sqrt 3 / 2
  in SFrame cx cy side [(cx - side / 2, cy + h / 3), (cx + side / 2, cy + h / 3), (cx, cy - 2 * h / 3)]

vtx :: SFrame -> Int -> (Double, Double)      -- i 番目の頂点
vtx fr i = at' (sfV fr) i (0, 0)

baryToXY :: SFrame -> [Double] -> (Double, Double)
baryToXY fr p = foldl' (\(x, y) (w, (vx, vy)) -> (x + w * vx, y + w * vy)) (0, 0) (zip p (sfV fr))

-- 画面の点 → (p1, p2, p3)（三角形の外なら負の値も出る）
xyToBary :: SFrame -> Double -> Double -> [Double]
xyToBary fr x y =
  let (a0, a1) = vtx fr 0
      (b0, b1) = vtx fr 1
      (c0, c1) = vtx fr 2
      det = (b1 - c1) * (a0 - c0) + (c0 - b0) * (a1 - c1)
      l1 = ((b1 - c1) * (x - c0) + (c0 - b0) * (y - c1)) / det
      l2 = ((c1 - a1) * (x - c0) + (a0 - c0) * (y - c1)) / det
  in [l1, l2, 1 - l1 - l2]

-- 三角形の中へ寄せる（どの確率も f 以上。p5.js 版の既定は 0.002）
clampSimplex :: [Double] -> Double -> [Double]
clampSimplex p f = mixFloor (normP p) f

sxy :: SFrame -> [Double] -> (Float, Float)   -- 描くときの Float の点
sxy fr p = let (x, y) = baryToXY fr p in (f2 x, f2 y)

vxy :: SFrame -> Int -> (Float, Float)
vxy fr i = let (x, y) = vtx fr i in (f2 x, f2 y)

-- 三角形のふちと頂点の名前（names が空なら「outcome 1」…）
drawSimplexFrame :: SFrame -> [String] -> Picture
drawSimplexFrame fr names = Pictures $
  Color (colA 170 ink) (thickPath 1.5 [vxy fr 0, vxy fr 1, vxy fr 2, vxy fr 0])
  : [ sayM (if null names then "outcome " ++ show (i + 1) else at' names i "") (x + ox) (y + oy) 14 (col (outC i))
    | (i, (ox, oy)) <- zip [0 ..] [(-16, 14), (16, 14), (0, -16)], let (x, y) = vxy fr i ]

-- 分布の列を三角形の上の折れ線に（2 点未満なら何もしない）
simplexPath :: SFrame -> [[Double]] -> Color -> Float -> Picture
simplexPath fr ps c w = Color c (polyline w (map (sxy fr) ps))

-- 点（直径 r）と名前
simplexDot :: SFrame -> [Double] -> RGB -> Float -> String -> Picture
simplexDot fr p c r name =
  let (x, y) = sxy fr p
  in Pictures [ Color (col bgC) (circleAt x y (r + 4)), Color (col c) (circleAt x y r)
              , if null name then Blank else sayLM name (x + 10) (y - 10) 15 (col c) ]

-- ★ 等高線の地図：三角形の中の各点で fn(p) を計算し、level の間隔で色の帯と線を描いた画像を作る（半分の解像度）
simplexField :: SFrame -> ([Double] -> Double) -> FieldOpt -> Field
simplexField fr fn opt =
  let pad = 4
      (v0x, _) = vtx fr 0
      (v2x, v2y) = vtx fr 2
      (_, v0y) = vtx fr 0
      x0 = min v0x v2x - pad
      y0 = v2y - pad
      w = sfSide fr + 2 * pad
      h = v0y - v2y + 2 * pad
      res = if foRes opt > 0 then foRes opt else 2
      gw = ceiling (w / i2d res) :: Int
      gh = ceiling (h / i2d res) :: Int
      lv = foLevel opt
      vmax = foVmax opt
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            b = xyToBary fr (x0 + (i2d i + 0.5) * i2d res) (y0 + (i2d j + 0.5) * i2d res)
        in if any (< 0) b then 0 / 0 else fn (map (max 1e-12) b)
      bandOf v = floor (min v (vmax * 4) / lv) :: Int
      pic = bandImage gw gh vals bandOf (/ vmax) opt 245 200
  in vals `seq` Field pic (f2 x0) (f2 y0) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 棒グラフ（ドラッグで高さを変えられる）
data BarOpt = BarOpt { boCols :: Int -> RGB, boNames :: [String], boVmax :: !Double, boGhost :: [Double] }

barOpt :: BarOpt                              -- 既定：結果の色、名前は 1 2 3 …、上限 1、重ねる分布なし
barOpt = BarOpt outC [] 1 []

drawBars :: [Double] -> Box -> BarOpt -> Picture
drawBars p r opt = Pictures $
  Color (col wire) (lineW 1 (f2 (bxX r), f2 (bxY r + bxH r)) (f2 (bxX r + bxW r), f2 (bxY r + bxH r)))
  : concat [ [ Color (colA 210 (boCols opt i)) (rectTL (x + i2f i * bw + 3) (y + h - hh) (bw - 6) hh)
             , if null (boGhost opt) then Blank
               else let gh = f2 (clampv (at' (boGhost opt) i 0) 0 vmax / vmax) * h
                    in Color (col ink) (frameTL 1.5 (x + i2f i * bw + 3) (y + h - gh) (bw - 6) gh)
             , if bw >= 34 then sayC (fmtN 2 v) (x + (i2f i + 0.5) * bw) (y + h - hh - 4) 14 (col ink) else Blank   -- 細い棒では数字を省く
             , sayC (if null (boNames opt) then show (i + 1) else at' (boNames opt) i "") (x + (i2f i + 0.5) * bw) (y + h + 17) 14 (col dimc) ]
           | (i, v) <- zip [0 ..] p, let hh = f2 (clampv v 0 vmax / vmax) * h ]
  where k = length p
        x = f2 (bxX r)
        y = f2 (bxY r)
        h = f2 (bxH r)
        bw = f2 (bxW r) / i2f (max 1 k)
        vmax = boVmax opt

-- 棒をドラッグ：i 番目の高さを変えて、残りを比例で縮める（合計 1 のまま）
barsDrag :: [Double] -> Box -> Double -> Double -> Double -> [Double]
barsDrag p r mx my fl0 =
  let k = length p
      fl = if fl0 == 0 then 0.001 else fl0
      i = floor ((mx - bxX r) / (bxW r / i2d k)) :: Int
      v = clampv ((bxY r + bxH r - my) / bxH r) fl (1 - fl * i2d (k - 1))
      rest = 1 - at' p i 0
      q = [ if j == i then v else if rest > 1e-9 then (pj / rest) * (1 - v) else (1 - v) / i2d (k - 1) | (j, pj) <- zip [0 ..] p ]
  in if i < 0 || i >= k || my < bxY r - 30 || my > bxY r + bxH r + 20 then p else mixFloor q fl

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
