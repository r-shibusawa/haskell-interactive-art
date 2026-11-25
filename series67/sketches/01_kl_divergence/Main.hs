-- ============================================
-- 67-01 KL ダイバージェンス ─ 2 つの分布の「近さ」 D(p‖q) = Σ p_i log(p_i / q_i)
-- 本当は p なのに q だと思って符号（暗号表）を作ると、1 回あたり平均で D(p‖q) ビットだけ長くなる（交差エントロピー = H(p) + D）
-- D(p‖q) ≥ 0 で、0 になるのは p = q のときだけ。でも D(p‖q) ≠ D(q‖p)（向きがある）、三角不等式も成り立たない → 距離ではない
-- 操作：左の三角形 p（オレンジ）と q（紫）をドラッグ / 棒グラフもドラッグできる / 右：p を固定したときの D(p‖·) と D(·‖p) の等高線
--       T 三角不等式の反例（中点 m を経由）/ S p と q を入れかえ / R でたらめ
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

level :: Double
level = 0.15                                  -- ★ 等高線の間隔（ナット）

data Grab = GNone | GP | GQ | GBarP | GBarQ deriving Eq

data World = World
  { wP       :: [Double]
  , wQ       :: [Double]
  , wFieldL  :: Maybe Field                   -- D(p‖x) の等高線
  , wFieldR  :: Maybe Field                   -- D(x‖p) の等高線
  , wKey     :: String                        -- 等高線を作ったときの p（小数 3 桁）と画面の幅
  , wGrab    :: !Grab
  , wShowTri :: !Bool
  , wRng     :: !Rng                          -- R キーの乱数（p5.js 版と同じく自己テストのあとの列の続き）
  , wTest    :: [Check] }

main :: IO ()
main = runSketch (sketch "67-01 KL divergence" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (rebuild env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World [0.7, 0.2, 0.1] [0.2, 0.3, 0.5] Nothing Nothing "" GNone True rngAfterTest []

-- ---- 配置（p5.js 版の layout）
frMain, frL, frR :: Env -> SFrame
frMain env = let side = min (d2 (envW env) * 0.32) ((d2 (envH env) - 460) / 0.91) in simplexFrame (d2 (envW env) * 0.22) (130 + side * 0.62) side
frL env = let s2 = small env in simplexFrame (d2 (envW env) * 0.55) (130 + s2 * 0.6) s2
frR env = let s2 = small env in simplexFrame (d2 (envW env) * 0.82) (130 + s2 * 0.6) s2

small :: Env -> Double
small env = min (d2 (envW env) * 0.2) (d2 (envH env) * 0.3)

barW, barY :: Env -> Double
barW env = min 260 (d2 (envW env) * 0.2)
barY env = d2 (envH env) - 250

boxP, boxQ :: Env -> Box
boxP env = Box 40 (barY env) (barW env) 120
boxQ env = Box (80 + barW env) (barY env) (barW env) 120

-- p が変わったときだけ等高線を作り直す（重いので）
rebuild :: Env -> World -> World
rebuild env w =
  let fk = intercalate "," (map (fmtN 3) (wP w)) ++ show (envW env)
      p = wP w
      opt c = fieldOpt { foLevel = level, foVmax = 2, foCap = 1.5, foC0 = (30, 45, 75), foC1 = c, foRes = 3 }
  in if fk == wKey w then w
     else w { wKey = fk
            , wFieldL = Just (simplexField (frL env) (\x -> min (divKL p x) 3) (opt orange))
            , wFieldR = Just (simplexField (frR env) (\x -> min (divKL x p) 3) (opt violet)) }

draw :: Env -> World -> Picture
draw env w = view env
  [ drawField (wFieldL w), drawField (wFieldR w)
  , hudTitle "KL divergence: a measure of closeness, but not a distance"
             ("Drag p (orange) / q (violet) or the bars   S swap   R random   T triangle counterexample " ++ (if wShowTri w then "ON" else "OFF"))
  , drawSimplexFrame fr [], simplexDot fr p orange 18 "p", simplexDot fr q violet 18 "q"
  , if wShowTri w then Pictures [simplexPath fr [p, mid, q] (colA 150 teal) 1.5, simplexDot fr mid teal 11 "m"] else Blank
  -- ---- 右上：等高線（形がちがう）
  , drawSimplexFrame fl ["1", "2", "3"], simplexDot fl p orange 12 "", simplexDot fl q violet 9 ""
  , drawSimplexFrame fr2 ["1", "2", "3"], simplexDot fr2 p orange 12 "", simplexDot fr2 q violet 9 ""
  , sayC "D(p||x): move x (p fixed)" (f2 (sfCx fl)) (f2 (snd (vtx fl 0)) + 38) 15 (col ink)
  , sayC "D(x||p): move x (p fixed)" (f2 (sfCx fr2)) (f2 (snd (vtx fr2 0)) + 38) 15 (col ink)
  , sayC "near an edge it jumps to inf" (f2 (sfCx fl)) (f2 (snd (vtx fl 0)) + 58) 14 (col dimc)
  , sayC "(q_i -> 0 inside the log)" (f2 (sfCx fl)) (f2 (snd (vtx fl 0)) + 76) 14 (col dimc)
  , sayC "finite even on the edge" (f2 (sfCx fr2)) (f2 (snd (vtx fr2 0)) + 58) 14 (col dimc)
  , sayC "(x_i = 0: 0 log 0 = 0)" (f2 (sfCx fr2)) (f2 (snd (vtx fr2 0)) + 76) 14 (col dimc)
  -- ---- 下：棒グラフと数値
  , drawBars p (boxP env) barOpt, drawBars q (boxQ env) barOpt
  , say "p" 40 (f2 by - 22) 15 (col orange), say "q" (f2 (80 + bw)) (f2 by - 22) 15 (col violet)
  , say ("D(p||q) = " ++ fmtN 4 dpq ++ " nats = " ++ fmtN 4 (bitsOf dpq) ++ " bits") tx (f2 by) 16 (col ink)
  , say ("D(q||p) = " ++ fmtN 4 dqp ++ " nats = " ++ fmtN 4 (bitsOf dqp) ++ " bits" ++ (if abs (dpq - dqp) > 1e-3 then "  <- not equal" else "")) tx (f2 by + 28) 16 (col ink)
  , say ("Code length: a code made for p uses H(p) = " ++ fmtN 3 (bitsOf (entropyH p)) ++ " bits") tx (f2 by + 58) 14 (col dimc)
  , say ("A code made for q, sending p: H(p, q) = " ++ fmtN 3 (bitsOf (crossH p q)) ++ " bits") tx (f2 by + 78) 14 (col dimc)
  , say ("-> extra bits = H(p, q) - H(p) = " ++ fmtN 4 (bitsOf (crossH p q - entropyH p)) ++ " = D(p||q)") tx (f2 by + 98) 14 (col dimc)
  , if wShowTri w
      then let bad = dpq > dpm + dmq + 1e-12
               c = col (if bad then rose else teal)
           in Pictures [ say ("Triangle inequality?  D(p||q) = " ++ fmtN 3 dpq) tx (f2 by + 132) 15 c
                       , say ((if bad then "  >  " else "  <=  ") ++ "D(p||m) + D(m||q) = " ++ fmtN 3 dpm ++ " + " ++ fmtN 3 dmq) tx (f2 by + 152) 15 c
                       , say (if bad then "-> via the midpoint m is \"closer\": it fails!" else "-> holds for this pair. Move p and q apart") tx (f2 by + 174) 15 c ]
      else Blank
  , drawSelfTest env (wTest w) ]
  where
    fr = frMain env
    fl = frL env
    fr2 = frR env
    p = wP w
    q = wQ w
    mid = mixM p q 0.5
    dpq = divKL p q
    dqp = divKL q p
    dpm = divKL p mid
    dmq = divKL mid q
    by = barY env
    bw = barW env
    tx = f2 (140 + 2 * bw)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (px, py) = baryToXY fr (wP w)
        (qx, qy) = baryToXY fr (wQ w)
        dd (x, y) = sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int))
        g | dd (px, py) < 22 = GP
          | dd (qx, qy) < 22 = GQ
          | inRect mx my (Box 40 (barY env - 30) (barW env) 170) = GBarP
          | inRect mx my (Box (80 + barW env) (barY env - 30) (barW env) 170) = GBarQ
          | otherwise = GNone
    in drag w { wGrab = g }
  EventMotion _ -> drag w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = GNone }
  EventKey (Char c) Down _ _
    | c == 's' || c == 'S' -> w { wP = wQ w, wQ = wP w }
    | c == 'r' || c == 'R' -> let ((a, b), g) = runR (do a' <- rndDist 3 0.01
                                                         b' <- rndDist 3 0.01
                                                         pure (a', b')) (wRng w)
                              in w { wP = a, wQ = b, wRng = g }
    | c == 't' || c == 'T' -> w { wShowTri = not (wShowTri w) }
  _ -> w
  where
    fr = frMain env
    (mx, my) = mouseD env
    drag w' = case wGrab w' of
      GP -> w' { wP = clampSimplex (xyToBary fr mx my) 0.002 }
      GQ -> w' { wQ = clampSimplex (xyToBary fr mx my) 0.002 }
      GBarP -> w' { wP = barsDrag (wP w') (boxP env) mx my 0.002 }
      GBarQ -> w' { wQ = barsDrag (wQ w') (boxQ env) mx my 0.002 }
      GNone -> w'

-- ---------- やってみよう ----------
-- 1. q を辺に近づけると D(p‖q) はどうなる？ D(q‖p) は？ 右の 2 つの等高線の形と比べよう
-- 2. D(p‖q) = D(q‖p) になる p と q を探そう（たとえば左右対称な位置）
-- 3. T の三角不等式：成り立たない組と成り立つ組の両方を探そう。どんな位置関係のとき破れる？
-- 4. 天気予報の確率 q と本当の確率 p。外れの予報は何ビット損をする？ 棒グラフで試そう
-- 5. 近い 2 点では D(p‖q) ≈ D(q‖p)。小さな差の 2 次の形がフィッシャー情報（02）

-- ---- 自己テスト
-- 500 組のでたらめな分布（種 3）。最後の乱数の状態は R キーで使う（p5.js 版は種を入れ直さない）
testPairs :: ([([Double], [Double])], Rng)
testPairs = runR (mapM (\k -> do p <- rndDist (3 + k `mod` 4) 0
                                 q <- rndDist (length p) 0
                                 pure (p, q)) [0 .. 499 :: Int]) (rngOf 3)

rngAfterTest :: Rng
rngAfterTest = snd testPairs

selfTest :: [Check]
selfTest =
  [ ("500 組のでたらめな分布で D(p‖q) ≥ 0、D(p‖p) = 0", okPos && okSelf)
  , ("交差エントロピー H(p, q) = H(p) + D(p‖q)（符号の余分な長さ、誤差 1e-12）", okGibbs)
  , ("向きがある：p = (0.9, 0.1)、q = (0.5, 0.5) で D(p‖q) = " ++ fmtN 4 (divKL a b) ++ " ≠ D(q‖p) = " ++ fmtN 4 (divKL b a)
    , abs (divKL a b - (0.9 * log 1.8 + 0.1 * log 0.2)) < 1e-12 && abs (divKL a b - divKL b a) > 0.05)
  , ("三角不等式の反例：D((0.9,0.1)‖(0.1,0.9)) = " ++ fmtN 3 (divKL p2 q2) ++ " ＞ 中点経由 " ++ fmtN 3 (divKL p2 m2 + divKL m2 q2)
    , divKL p2 q2 > divKL p2 m2 + divKL m2 q2 + 0.5)
  , ("q_i = 0 で p_i > 0 なら D(p‖q) = ∞、p_i = 0 の項は 0（0·log 0 = 0）", isInfinite (divKL [0.5, 0.5] [1, 0]) && abs (divKL [1, 0] [0.5, 0.5] - log 2) < 1e-15)
  , ("近くでは D(p‖p+dp) ≈ ½ Σ dp²/p（フィッシャー情報の 2 次の形、02 へ）相対誤差 " ++ fmtN 5 relErr, relErr < 1e-3) ]
  where
    pairs = fst testPairs
    okPos = and [ divKL p q >= 0 | (p, q) <- pairs ]
    okSelf = and [ divKL p p == 0 | (p, _) <- pairs ]
    okGibbs = and [ abs (crossH p q - entropyH p - divKL p q) <= 1e-12 | (p, q) <- pairs ]
    a = [0.9, 0.1]
    b = [0.5, 0.5]
    p2 = [0.9, 0.1]
    q2 = [0.1, 0.9]
    m2 = [0.5, 0.5]
    p3 = [0.5, 0.3, 0.2]
    e = 1e-4
    dv = [1, -2, 1]
    q3 = zipWith (\v d -> v + e * d) p3 dv
    fq = foldl' (\s (d, v) -> s + d * d / v) 0 (zip dv p3)
    relErr = abs (divKL p3 q3 / (0.5 * e * e * fq) - 1)

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
