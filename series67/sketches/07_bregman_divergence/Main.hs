-- ============================================
-- 67-07 ブレグマン・ダイバージェンス ─ 下に凸な関数 f のグラフと、点 y での接線のすき間
-- D_f(x, y) = f(x) − f(y) − f′(y)(x − y)。f = x² なら (x − y)²（2 乗距離）、f = x log x − x なら x log(x/y) − x + y（KL の 1 成分）
-- 点の集まり x_1 … x_n の「中心」：2 番目の引数 c について Σ D_f(x_i, c) を最小にすると、どの f でもいつも算術平均！
-- 1 番目の引数で最小にすると f によってちがう（x log x なら幾何平均）。k-means（クラスタリング、シリーズ 61）が KL でも使える理由
-- 操作：1 x²  2 x log x − x  3 −log x  4 eˣ / 上のグラフで x・y をドラッグ / 下をクリック 点を足す / R 新しい点 / Backspace 点を減らす
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

-- 凸関数 f と、その微分 f′、f′ の逆関数、「1 番目の引数の中心」の名前
data Fn = Fn { fnName :: String, fnF :: Double -> Double, fnD :: Double -> Double, fnInv :: Double -> Double, fnMean1 :: String }

fs :: [Fn]
fs =
  [ Fn "x^2 (squared distance)" (\x -> x * x) (\x -> 2 * x) (\u -> u / 2) "arithmetic mean"
  , Fn "x log x - x (KL)" (\x -> x * log x - x) log exp "geometric mean"
  , Fn "-log x (Itakura-Saito)" (\x -> -log x) (\x -> -1 / x) (\u -> -1 / u) "harmonic mean"
  , Fn "e^x" exp exp log "log(mean of e^x)" ]

fnOf :: Int -> Fn
fnOf k = at' fs k (headOr (Fn "" id id id "") fs)

xr0, xr1 :: Double
xr0 = 0.1                                     -- ★ x の範囲（x log x と −log x は x > 0 が必要）
xr1 = 4

data World = World
  { wK     :: !Int                            -- どの f か（0〜3）
  , wX     :: !Double
  , wY     :: !Double
  , wPts   :: [Double]                        -- 点の集まり（足した順）
  , wGrab  :: !Int                            -- 0 なし、1 x、2 y
  , wRng   :: !Rng
  , wTest  :: [Check] }

main :: IO ()
main = runSketch (sketch "67-07 Bregman divergence" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあとで種 17 から点を作る
world0 :: World
world0 = let (ps, g) = runR newPoints (rngOf 17) in World 1 3.2 1.0 ps 0 g []

topR, botR :: Env -> Box
topR env = Box 70 100 (d2 (envW env) * 0.5) (d2 (envH env) * 0.4)
botR env = Box 70 (d2 (envH env) * 0.6) (d2 (envW env) * 0.5) (d2 (envH env) * 0.3)

newPoints :: R [Double]
newPoints = mapM (\_ -> fmap (\u -> 0.3 + 3.4 * u ** 1.5) rnd) [1 .. 6 :: Int]

-- ★ ブレグマン・ダイバージェンス
breg :: Fn -> Double -> Double -> Double
breg f x y = max 0 (fnF f x - fnF f y - fnD f y * (x - y))

sum2, sum1 :: [Double] -> Fn -> Double -> Double
sum2 ps f c = foldl' (\s x -> s + breg f x c) 0 ps   -- 2 番目の引数を動かす
sum1 ps f c = foldl' (\s x -> s + breg f c x) 0 ps   -- 1 番目の引数を動かす

center2 :: [Double] -> Double
center2 ps = sumOf ps / i2d (length ps)

center1 :: [Double] -> Fn -> Double             -- ∇f の平均を ∇f で戻す
center1 ps f = fnInv f (foldl' (\s x -> s + fnD f x) 0 ps / i2d (length ps))

-- 黄金分割法で [a, b] の中の最小を探す
goldenMin :: (Double -> Double) -> Double -> Double -> Double
goldenMin fn a0 b0 = go (0 :: Int) a0 b0
  where g = (sqrt 5 - 1) / 2
        go it a b
          | it >= 200 = (a + b) / 2
          | otherwise = let c = b - g * (b - a)
                            d = a + g * (b - a)
                        in if fn c < fn d then go (it + 1) a d else go (it + 1) c b

xsOf :: [Double]
xsOf = [ xr0 + (xr1 - xr0) * i2d k / 200 | k <- [0 .. 200 :: Int] ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Bregman divergence: f = " ++ fnName f)
             "1 x^2  2 x log x - x  3 -log x  4 e^x   Top: drag x (orange) / y (violet)   Bottom: click to add a point   R new points   Backspace remove"
  -- ---- 上：f のグラフ、y での接線、x でのすき間
  , drawAxes b "x" "f(x) and the tangent at y"
  , plotLine b (zip xsOf ys) (col teal) 3
  , plotLine b [(xr0, tangent xr0), (xr1, tangent xr1)] (col violet) 1.8
  , Color (col orange) (lineW 5 (f2 gx, f2 g1) (f2 gx, f2 (clampv g2 (pbY b) (pbY b + pbH b)))) ]
  ++ [ Pictures [Color (col c) (circleAt (f2 sx) (f2 sy) 14), sayT nm (f2 sx) (f2 (pbY b + pbH b) + 3) 15 (col c) True]
     | (x, c, nm) <- [(wX w, orange, "x"), (wY w, violet, "y")], let (sx, sy) = toScr b x (fnF f x) ]
  ++ [ say ("D_f(x, y) = orange gap = " ++ fmtN 4 (breg f (wX w) (wY w))) rx 140 17 (col orange)
     , say ("D_f(y, x) = " ++ fmtN 4 (breg f (wY w) (wX w)) ++ " (usually differs)") rx 168 17 (col violet)
     , say ("x^2: (x - y)^2 = " ++ fmtN 4 ((wX w - wY w) ^ (2 :: Int))) rx 200 14 (col dimc)
     , say ("x log x - x: x log(x/y) - x + y = " ++ fmtN 4 (wX w * log (wX w / wY w) - wX w + wY w)) rx 222 14 (col dimc)
     , say ("-log x: x/y - log(x/y) - 1 = " ++ fmtN 4 (wX w / wY w - log (wX w / wY w) - 1)) rx 244 14 (col dimc)
     , say "f convex -> tangent below the graph -> D_f >= 0" rx 274 14 (col dimc)
     -- ---- 下：点の集まりと 2 つの「中心」
     , drawAxes b2 "c" "sum D_f(x_i, c) (orange) and sum D_f(c, x_i) (violet)"
     , plotLine b2 (zip xsOf (map (min top2) s2)) (col orange) 2.5
     , plotLine b2 (zip xsOf (map (min top2) s1)) (col violet) 2.5 ]
  ++ [ Color (col ink) (circleAt (f2 (fst (toScr b2 x 0))) (f2 (pbY b2 + pbH b2)) 10) | x <- wPts w ]
  ++ [ Color (col c) (dashLine 1.5 5 4 (sx, f2 (pbY b2)) (sx, f2 (pbY b2 + pbH b2))) | (cc, c) <- [(c2, orange), (c1, violet)], let sx = f2 (fst (toScr b2 cc 0)) ]
  ++ [ say ("Min over the 2nd arg: c = " ++ fmtN 4 c2) rx ry 16 (col orange)
     , say ("  = arithmetic mean " ++ fmtN 4 (center2 (wPts w))) rx (ry + 20) 16 (col orange)
     , say ("Min over the 1st arg: c = " ++ fmtN 4 c1) rx (ry + 46) 16 (col violet)
     , say ("  = " ++ fnMean1 f) rx (ry + 66) 16 (col violet)
     , say "Numerical minimum (golden section):" rx (ry + 94) 14 (col dimc)
     , say ("  " ++ fmtN 4 (goldenMin (sum2 ps f) xr0 xr1) ++ " and " ++ fmtN 4 (goldenMin (sum1 ps f) xr0 xr1)) rx (ry + 112) 14 (col dimc)
     , say "The mean is always the center ->" rx (ry + 140) 14 (col dimc)
     , say "k-means' \"recompute the mean\" step is right" rx (ry + 158) 14 (col dimc)
     , say "for every Bregman divergence" rx (ry + 176) 14 (col dimc)
     , drawSelfTest env (wTest w) ]
  where
    f = fnOf (wK w)
    ps = wPts w
    tr = topR env
    br = botR env
    ys = map (fnF f) xsOf
    lo = minOr 0 ys
    hi = maxOr 1 ys
    b = plotBox (bxX tr) (bxY tr) (bxW tr) (bxH tr) xr0 xr1 (lo - 0.1 * (hi - lo)) (hi + 0.05 * (hi - lo))
    tangent x = fnF f (wY w) + fnD f (wY w) * (x - wY w)
    (gx, g1) = toScr b (wX w) (fnF f (wX w))
    (_, g2) = toScr b (wX w) (tangent (wX w))
    rx = f2 (bxX tr + bxW tr) + 50
    c2 = center2 ps
    c1 = center1 ps f
    s2 = map (sum2 ps f) xsOf
    s1 = map (sum1 ps f) xsOf
    top2 = minimum [maxOr 0 s2, maxOr 0 s1, 4 * max (sum2 ps f c2) (sum1 ps f c1) + 1]
    b2 = plotBox (bxX br) (bxY br) (bxW br) (bxH br) xr0 xr1 0 top2
    ry = f2 (bxY br) + 10

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | inRect mx my (Box (bxX tr) (bxY tr) (bxW tr) (bxH tr + 30)) ->
        let ux = xr0 + (mx - bxX tr) / bxW tr * (xr1 - xr0)
        in dragged w { wGrab = if abs (ux - wX w) < abs (ux - wY w) then 1 else 2 }
    | inRect mx my br && length (wPts w) < 20 ->
        w { wPts = wPts w ++ [clampv (xr0 + (mx - bxX br) / bxW br * (xr1 - xr0)) (xr0 + 0.05) xr1] }
  EventMotion _ -> dragged w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = 0 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> w { wK = fromEnum c - fromEnum '1' }
    | c == 'r' || c == 'R' -> let (ps, g) = runR newPoints (wRng w) in w { wPts = ps, wRng = g }
    | c == '\b' || c == '\DEL' -> backspace
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace   -- Mac の delete キー
  _ -> w
  where
    tr = topR env
    br = botR env
    (mx, my) = mouseD env
    backspace = if length (wPts w) > 2 then w { wPts = take (length (wPts w) - 1) (wPts w) } else w
    dragged v = if wGrab v == 0 then v else
      let ux = clampv (xr0 + (mx - bxX tr) / bxW tr * (xr1 - xr0)) (xr0 + 0.02) xr1
      in if wGrab v == 1 then v { wX = ux } else v { wY = ux }

-- ---------- やってみよう ----------
-- 1. f = x² で x と y を入れかえても値は同じ？ ほかの f では？
-- 2. 3（−log x）で点を 1 つだけ 0.1 の近くに置くと、紫の中心はどう動く？ オレンジは？
-- 3. 自分で f を足そう：f(x) = x⁴、f(x) = x log x + (1−x) log(1−x)（範囲に注意）。中心はやっぱり平均？
-- 4. 下に凸でない f（たとえば sin x）にすると、D_f が負になる所がある。どこ？
-- 5. KL は f = Σ p log p のブレグマン・ダイバージェンス。ψ のブレグマン・ダイバージェンスも KL になる（04 の双対）

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("4 つの f で D_f(x, y) ≥ 0、D_f(x, x) = 0（300 組）", okPos)
  , ("f = x² なら D_f(x, y) = (x − y)²、f = x log x − x なら x log(x/y) − x + y（誤差 1e-12）", okSq && okKL)
  , ("確率分布なら各成分の和 Σ D_f(p_i, q_i) = KL ダイバージェンス D(p‖q)", abs (vk - divKL p q) < 1e-12)
  , ("どの f でも Σ D_f(x_i, c) の最小は算術平均（4 つの f × 500 個のでたらめな c に勝つ）", okMean)
  , ("1 番目の引数の最小 = ∇f⁻¹(∇f の平均)：黄金分割法の答えと一致（誤差 1e-6）", okGold)
  , ("x log x − x なら 1 番目の最小は幾何平均 " ++ fmtN 3 g ++ "（算術平均 " ++ fmtN 3 (center2 pts) ++ " とはちがう）"
    , abs (center1 pts (fnOf 1) - g) < 1e-12 && abs (g - center2 pts) > 0.01) ]
  where
    (xys, p, q) = withSeed 70 $ do
      a <- mapM (\_ -> do x <- rnd
                          y <- rnd
                          pure (0.1 + 3.9 * x, 0.1 + 3.9 * y)) [1 .. 300 :: Int]
      p' <- rndDist 5 0
      q' <- rndDist 5 0
      pure (a, p', q')
    okPos = and [ breg f x y >= 0 && not (breg f x x > 1e-12) | (x, y) <- xys, f <- fs ]
    okSq = and [ abs (breg (fnOf 0) x y - (x - y) ^ (2 :: Int)) <= 1e-12 | (x, y) <- xys ]
    okKL = and [ abs (breg (fnOf 1) x y - (x * log (x / y) - x + y)) <= 1e-12 | (x, y) <- xys ]
    vk = foldl' (+) 0 (zipWith (breg (fnOf 1)) p q)
    (pts, cs) = withSeed 71 $ do
      ps <- newPoints
      c <- mapM (\_ -> mapM (\_ -> fmap (\u -> 0.1 + 3.9 * u) rnd) [1 .. 500 :: Int]) fs
      pure (ps, c)
    okMean = and [ not (sum2 pts f c < sum2 pts f (center2 pts) - 1e-12) | (f, cl) <- zip fs cs, c <- cl ]
    okGold = and [ abs (goldenMin (sum1 pts f) xr0 xr1 - center1 pts f) <= 1e-6 | f <- fs ]
    g = exp (foldl' (\s x -> s + log x) 0 pts / i2d (length pts))

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

-- ---- 2 次元の図の枠（データ座標 ↔ 画面）
data PBox = PBox { pbX :: !Double, pbY :: !Double, pbW :: !Double, pbH :: !Double, pbX0 :: !Double, pbX1 :: !Double, pbY0 :: !Double, pbY1 :: !Double }

plotBox :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PBox
plotBox = PBox

pbBox :: PBox -> Box                          -- 枠の長方形（inRect 用）
pbBox b = Box (pbX b) (pbY b) (pbW b) (pbH b)

toScr :: PBox -> Double -> Double -> (Double, Double)
toScr b u v = (pbX b + (u - pbX0 b) / (pbX1 b - pbX0 b) * pbW b, pbY b + pbH b - (v - pbY0 b) / (pbY1 b - pbY0 b) * pbH b)

toScrF :: PBox -> Double -> Double -> (Float, Float)
toScrF b u v = let (x, y) = toScr b u v in (f2 x, f2 y)

fromScr :: PBox -> Double -> Double -> (Double, Double)
fromScr b sx sy = (pbX0 b + (sx - pbX b) / pbW b * (pbX1 b - pbX0 b), pbY0 b + (pbY b + pbH b - sy) / pbH b * (pbY1 b - pbY0 b))

drawAxes :: PBox -> String -> String -> Picture
drawAxes b xl yl = Pictures
  [ Color (col wire) (frameTL 1 x y w h)
  , sayT xl (x + w / 2) (y + h + 16) 14 (col dimc) True
  , sayT (fmtN 1 (pbX0 b)) x (y + h + 2) 14 (col dimc) False
  , sayR (fmtN 1 (pbX1 b)) (x + w) (y + h + 14) 14 (col dimc)
  , sayRM (fmtN 1 (pbY0 b)) (x - 4) (y + h) 14 (col dimc)
  , sayRM (fmtN 1 (pbY1 b)) (x - 4) y 14 (col dimc)
  , say yl x (y - 5) 14 (col dimc) ]
  where x = f2 (pbX b)
        y = f2 (pbY b)
        w = f2 (pbW b)
        h = f2 (pbH b)

-- データ座標の点列を折れ線に（枠の外の部分は枠のふちで切る）
plotLine :: PBox -> [(Double, Double)] -> Color -> Float -> Picture
plotLine b pts c w = Color c (Pictures (map (polyline w . map (\(x, y) -> (f2 x, f2 y))) (clipRuns b pts)))

-- 枠の中に入る部分の折れ線の列（リャン-バースキー法で線分を切る）
clipRuns :: PBox -> [(Double, Double)] -> [[(Double, Double)]]
clipRuns b pts = finish (foldl' stepP ([], [], Nothing) pts)
  where
    x0 = pbX b - 1
    x1 = pbX b + pbW b + 1
    y0 = pbY b - 1
    y1 = pbY b + pbH b + 1
    flush run done = if length run >= 2 then reverse run : done else done
    finish (run, done, _) = reverse (flush run done)
    clip (ax, ay) (cx, cy) =
      let dx = cx - ax
          dy = cy - ay
          go (t0, t1) (p, q)
            | p == 0 = if q < 0 then Nothing else Just (t0, t1)
            | otherwise = let r = q / p in Just (if p < 0 then (max t0 r, t1) else (t0, min t1 r))
          res = foldl' (\acc pq -> acc >>= \tt -> go tt pq) (Just (0, 1)) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      in case res of
           Just (t0, t1) | t0 <= t1 -> Just (t0, t1)
           _ -> Nothing
    stepP (run, done, prev) (u, v) =
      let s@(sx, sy) = toScr b u v
          good = not (isNaN sx || isNaN sy || isInfinite sx || isInfinite sy)
      in if not good then ([], flush run done, Nothing) else
         case prev of
           Nothing -> (run, done, Just s)
           Just pv@(px, py) -> case clip pv s of
             Nothing -> ([], flush run done, Just s)
             Just (t0, t1) ->
               let pa = (px + t0 * (sx - px), py + t0 * (sy - py))
                   pb = (px + t1 * (sx - px), py + t1 * (sy - py))
                   (run1, done1) = if t0 > 0 then ([pa], flush run done) else if null run then ([pa], done) else (run, done)
                   run2 = pb : run1
               in if t1 < 1 then ([], flush run2 done1, Just s) else (run2, done1, Just s)

-- 長方形の上の等高線の地図（fn(u, v)）
gridField :: PBox -> (Double -> Double -> Double) -> FieldOpt -> Field
gridField b fn opt =
  let res = if foRes opt > 0 then foRes opt else 3
      gw = ceiling (pbW b / i2d res) :: Int
      gh = ceiling (pbH b / i2d res) :: Int
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            (u, v) = fromScr b (pbX b + (i2d i + 0.5) * i2d res) (pbY b + (i2d j + 0.5) * i2d res)
        in fn u v
      lv = foLevel opt
      vmin = foVmin opt
      vmax = foVmax opt
      bandOf v = floor (clampv ((v - vmin) / lv) (-1e6) 1e6) :: Int
      pic = bandImage gw gh (U.map (\v -> if isNaN v then -1e300 else v) vals) bandOf (\v -> (v - vmin) / (vmax - vmin)) opt 235 170
  in vals `seq` Field pic (f2 (pbX b)) (f2 (pbY b)) (i2f (gw * res)) (i2f (gh * res)) gw gh

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
