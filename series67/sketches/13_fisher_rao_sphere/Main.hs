-- ============================================
-- 67-13 球に乗せた分布 ─ p → 2√p（各成分の平方根を 2 倍）で、確率の三角形が半径 2 の球の 1/8 に乗る
-- この球の上の距離（大円の長さ）がフィッシャー-ラオ距離 d = 2·arccos(Σ√(p_i q_i))。フィッシャー計量で測った本当の最短距離
-- 三角形の中のまっすぐ（m-測地線、白い点線）は球の上では遠回り。KL・ヘリンジャー・全変動と比べる
-- （「土を運ぶ量」で測る輸送距離（ワッサースタイン距離）は、また別の幾何）
-- 操作：ドラッグ 視点を回す / 左下の三角形の中をドラッグ p（オレンジ）・q（紫）/ 1 2 3 お手本 / R でたらめ / スペース 自動回転
-- 3D は自前の回転と透視投影で描き、奥から順に重ねる（画家のアルゴリズム、depthSort）
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

sc :: Double
sc = 140                                      -- ★ 1 の長さ（ピクセル）

meshN :: Int
meshN = 24                                    -- ★ 球の 1/8 を何分割で描くか

data World = World
  { wP      :: [Double]
  , wQ      :: [Double]
  , wStats  :: (Double, Double, Double)       -- (フィッシャー-ラオ d, 大円の長さ, まっすぐの像の長さ)。p, q が変わったときだけ計算
  , wSpin   :: !Bool
  , wSpinT  :: !Double
  , wRotY   :: !Double
  , wYaw    :: !Float                         -- ドラッグで回した視点（p5.js の orbitControl の代わり）
  , wPitch  :: !Float
  , wLast   :: !(Maybe (Float, Float))        -- 視点を回しているときの前のマウスの位置
  , wGrab   :: !Int                           -- 0 なし、1 p、2 q
  , wRng    :: !Rng
  , wTest   :: [Check] }

main :: IO ()
main = runSketch (sketch "67-13 Distributions on a sphere" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wSpin w then let t = wSpinT w + 0.008 in w { wSpinT = t, wRotY = -pi / 4 + 0.6 * sin t } else w)
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあとの乱数の列をそのまま使う（R キー）
world0 :: World
world0 = setPQ [0.7, 0.2, 0.1] [0.1, 0.3, 0.6] (World [] [] (0, 0, 0) True 0 (-pi / 4) 0 0 Nothing 0 (snd selfTestR) [])

setPQ :: [Double] -> [Double] -> World -> World
setPQ p q w = w { wP = p, wQ = q, wStats = (fisherRao p q, polyLen (geoPath p q 4000), polyLen (mPathOnSphere p q 4000)) }

-- 左下の枠と三角形
insetOf :: Env -> Box
insetOf env = Box 20 (d2 (envH env) - 290) 300 270

frIOf :: Env -> SFrame
frIOf env = let b = insetOf env in simplexFrame (bxX b + 150) (bxY b + 160) 230

-- ★ p → 2√p（長さはいつも 2）
toSphere :: [Double] -> [Double]
toSphere = map (\v -> 2 * sqrt (max 0 v))

fromSphere :: [Double] -> [Double]
fromSphere = map (\c -> c * c / 4)

-- 大円に沿って a から b へ（球の上のまっすぐ）
slerp :: [Double] -> [Double] -> Double -> [Double]
slerp a b t =
  let c = clampv (sumOf (zipWith (*) a b) / 4) (-1) 1
      om = acos c
  in if om < 1e-12 then a else zipWith (\v u -> (sin ((1 - t) * om) * v + sin (t * om) * u) / sin om) a b

polyLen :: [[Double]] -> Double
polyLen pts = sumOf [ sqrt (sumOf (zipWith (\x y -> (x - y) * (x - y)) b a)) | (a, b) <- zip pts (drop 1 pts) ]

geoPath :: [Double] -> [Double] -> Int -> [[Double]]
geoPath p q n = let a = toSphere p
                    b = toSphere q
                in [ slerp a b (i2d k / i2d n) | k <- [0 .. n] ]

mPathOnSphere :: [Double] -> [Double] -> Int -> [[Double]]
mPathOnSphere p q n = [ toSphere (mixM p q (i2d k / i2d n)) | k <- [0 .. n] ]

-- (x, y, z) の画面の向き：3 番目の結果を上に（p5.js の WEBGL の座標：y は下向き、z は手前）
v3 :: [Double] -> V3
v3 v = V3 (f2 (at' v 0 0 * sc)) (f2 (-(at' v 2 0) * sc)) (f2 (at' v 1 0 * sc))

-- モデルの点 → 視点の座標（translate → rotateY → rotateX(−0.55) → ドラッグの回転）
toView :: World -> V3 -> V3
toView w m =
  let s = f2 sc
      a = add3 m (V3 (-0.9 * s) (0.9 * s) (-0.9 * s))
  in rotX (wPitch w) (rotY (wYaw w) (rotX (-0.55) (rotY (f2 (wRotY w)) a)))

-- 画面の点（p5.js と同じく画面の中央が原点の透視投影。z = 0 の面では 1 ピクセル = 1）
scr :: Env -> V3 -> (Float, Float)
scr env v = let camD = envH env / 2 / tan (pi / 6)
                (x, y) = project camD v
            in (envW env / 2 + x, envH env / 2 + y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

draw :: Env -> World -> Picture
draw env w = view env [ Pictures (map snd (depthSort fst items)), hud env w ]
  where
    tv = toView w
    pt = scr env
    -- ---- 球の 1/8（色 = エントロピー：明るいほど大きい）
    surfP i j = [i2d i / i2d meshN, i2d j / i2d meshN, 1 - i2d (i + j) / i2d meshN]
    surfC p = let h = realToFrac (entropyH p / log 3) :: Float in (40 + 110 * h, 62 + 70 * h, 105 + 50 * h)
    tri ijs = let ps = [ surfP i j | (i, j) <- ijs ]
                  vs = map (tv . v3 . toSphere) ps
                  cs = map surfC ps
                  (r, g, b) = foldl' (\(a1, a2, a3) (c1, c2, c3) -> (a1 + c1 / 3, a2 + c2 / 3, a3 + c3 / 3)) (0, 0, 0) cs
              in (sum (map zOf vs) / 3, Color (col (r, g, b)) (Polygon (map pt vs)))
    surface = concat [ tri [(i, j), (i + 1, j), (i, j + 1)] : [ tri [(i + 1, j), (i + 1, j + 1), (i, j + 1)] | i + j + 2 <= meshN ]
                     | i <- [0 .. meshN - 1], j <- [0 .. meshN - 1 - i] ]
    -- 線分（少し手前に寄せて、面より先に描かれないように）
    seg c wt a b = let va = tv a
                       vb = tv b
                   in ((zOf va + zOf vb) / 2 + 3, Color c (lineW wt (pt va) (pt vb)))
    segs c wt vs = [ seg c wt a b | (a, b) <- zip vs (drop 1 vs) ]
    -- ---- 格子（p_i = 一定 の線）と軸
    gridLines = concat [ segs (colA 150 (outC c)) 1 [ v3 (map (* 1.003) (toSphere (place c a s))) | j <- [0 .. 24 :: Int], let s = (1 - a) * i2d j / 24 ]
                       | c <- [0 .. 2], k <- [1 .. 9 :: Int], let a = i2d k / 10 ]
    place c a s = [ if i == c then a else if i == (c + 1) `mod` 3 then s else 1 - a - s | i <- [0 .. 2] ]
    axes = concat [ segs (col (outC c)) 2 [ v3 [ if i == c then 2.4 * i2d m / 10 else 0 | i <- [0 .. 2] ] | m <- [0 .. 10 :: Int] ] | c <- [0 .. 2] ]
    -- ---- 大円（測地線）と、三角形のまっすぐの像（点線）
    geo = segs (col lime) 4 [ v3 (map (* 1.004) pp) | pp <- geoPath (wP w) (wQ w) 60 ]
    mp = [ v3 (map (* 1.004) pp) | pp <- mPathOnSphere (wP w) (wQ w) 60 ]
    mDash = [ seg (col ink) 2 a b | (k, a, b) <- zip3 [0 :: Int ..] mp (drop 1 mp), even k ]
    dots = [ let v = tv (v3 (map (* 1.01) (toSphere pp)))
                 (x, y) = pt v
                 k = (envH env / 2 / tan (pi / 6)) / max 1 (envH env / 2 / tan (pi / 6) - zOf v)
             in (zOf v + 10, Color (col c) (circleAt x y (14 * k)))
           | (pp, c) <- [(wP w, orange), (wQ w, violet)] ]
    items = surface ++ gridLines ++ axes ++ geo ++ mDash ++ dots

hud :: Env -> World -> Picture
hud env w = Pictures
  [ hudTitle "Distributions on a sphere: p -> 2 sqrt(p) and the Fisher-Rao distance"
             ("Drag view   Drag in the lower-left triangle p, q   1 2 3 examples   R random   Space auto-rotate " ++ (if wSpin w then "ON" else "OFF"))
  , say ("p = " ++ fmtP 3 p) x 110 15 (col orange)
  , say ("q = " ++ fmtP 3 q) x 132 15 (col violet)
  , say "Fisher-Rao d = 2 arccos(sum sqrt(p q))" x 162 15 (col lime)
  , say ("  = " ++ fmtN 5 frd) x 182 15 (col lime)
  , say ("Arc length (4000 pieces) = " ++ fmtN 5 arc) x 204 15 (col lime)
  , say ("Straight-line image = " ++ fmtN 5 ml ++ " (detour)") x 230 15 (col ink)
  , say ("Chord 2|sqrt p - sqrt q| = " ++ fmtN 4 (2 * sqrt (hellingerSq p q))) x 258 14 (col dimc)
  , say "  = 4 sin(d/4)" x 276 14 (col dimc)
  , say ("Hellinger sum(sqrt p - sqrt q)^2 = " ++ fmtN 4 (hellingerSq p q)) x 298 14 (col dimc)
  , say ("KL(p||q) = " ++ fmtN 4 (divKL p q) ++ ", KL(q||p) = " ++ fmtN 4 (divKL q p)) x 320 14 (col dimc)
  , say ("Total variation sum|p - q|/2 = " ++ fmtN 4 (tvDist p q)) x 342 14 (col dimc)
  , say "Nearby: KL ~ d^2/2 ~ (1/2) sum dp^2/p" x 370 14 (col dimc)
  , say "  (the same metric as 01)" x 388 14 (col dimc)
  , say "Max d is pi (two vertices = 1/4 of" x 410 14 (col dimc)
  , say "  a great circle)" x 428 14 (col dimc)
  -- 左下：三角形（p, q を動かす）と、球の大円を三角形にもどした曲線
  , Color (col panel) (rectTL (f2 (bxX b)) (f2 (bxY b)) (f2 (bxW b)) (f2 (bxH b)))
  , Color (colA 160 ink) (thickPath 1.2 [vxy fr 0, vxy fr 1, vxy fr 2, vxy fr 0])
  , Color (col lime) (polyline 2.5 [ sxy fr (fromSphere pp) | pp <- geoPath p q 60 ])
  , Color (col ink) (dashLine 1 4 4 (sxy fr p) (sxy fr q))
  , let (x1, y1) = sxy fr p in Color (col orange) (circleAt x1 y1 13)
  , let (x1, y1) = sxy fr q in Color (col violet) (circleAt x1 y1 13)
  , say "Green = great circle" (f2 (bxX b) + 10) (f2 (bxY b) + 20) 14 (col dimc)
  , say "(mapped back to the triangle)" (f2 (bxX b) + 10) (f2 (bxY b) + 258) 14 (col dimc)
  , drawSelfTest env (wTest w) ]
  where
    p = wP w
    q = wQ w
    (frd, arc, ml) = wStats w
    x = envW env - 430
    b = insetOf env
    fr = frIOf env

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (px, py) = baryToXY fri (wP w)
        (qx, qy) = baryToXY fri (wQ w)
        dp = sqrt ((mx - px) ^ (2 :: Int) + (my - py) ^ (2 :: Int))
        dq = sqrt ((mx - qx) ^ (2 :: Int) + (my - qy) ^ (2 :: Int))
    in if inRect mx my (insetOf env) then dragTo w { wGrab = if dp < dq then 1 else 2, wLast = Nothing }
       else w { wGrab = 0, wLast = Just (mouseP env) }
  EventMotion _
    | wGrab w /= 0 -> dragTo w
    | envDown env && not (inRect mx my (insetOf env)) ->
        let (x, y) = mouseP env
        in case wLast w of
             Just (lx, ly) -> w { wYaw = wYaw w + (x - lx) * 0.01, wPitch = max (-1.5) (min 1.5 (wPitch w - (y - ly) * 0.01)), wLast = Just (x, y) }
             Nothing -> w { wLast = Just (x, y) }
    | otherwise -> w { wLast = Nothing }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = 0, wLast = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpin = not (wSpin w) }
  EventKey (Char c) Down _ _
    | c == '1' -> setPQ [0.98, 0.01, 0.01] [0.01, 0.98, 0.01] w
    | c == '2' -> setPQ [0.34, 0.33, 0.33] [0.4, 0.3, 0.3] w
    | c == '3' -> setPQ [0.5, 0.49, 0.01] [0.01, 0.49, 0.5] w
    | c == 'r' || c == 'R' -> let ((p, q), g) = runR ((,) <$> rndDist 3 0.01 <*> rndDist 3 0.01) (wRng w)
                              in (setPQ p q w) { wRng = g }
  _ -> w
  where fri = frIOf env
        (mx, my) = mouseD env
        dragTo w' = let pp = clampSimplex (xyToBary fri mx my) 0.001
                    in if wGrab w' == 1 then setPQ pp (wQ w') w' else setPQ (wP w') pp w'

-- ---------- やってみよう ----------
-- 1. 1 のお手本（頂点どうし）で d = π に近いことを確かめよう。KL はどうなる？
-- 2. 2 のお手本（まん中の近く）で、大円とまっすぐはほとんど同じ。三角形の辺の近くでは？
-- 3. なぜ 2√p？ √p にすると半径 1 の球になる。距離の公式はどう変わる？
-- 4. 4 つの結果なら 4 次元の球の 1/16。どう描けばいい？
-- 5. 球の上の「等距離の円」（p から距離 0.5 の分布）を三角形に描くと、どんな形？

-- ---- 自己テスト
selfTest :: [Check]
selfTest = fst selfTestR

-- 自己テストと、そのあとの乱数の列（R キーはこの続きを使う）
selfTestR :: ([Check], Rng)
selfTestR = (checks, g2)
  where
    (pairs, g1) = runR (mapM (\_ -> (,) <$> rndDist 3 1e-3 <*> rndDist 3 1e-3) [1 .. 50 :: Int]) (rngOf 1313)
    (triples, g2) = runR (mapM (\_ -> (,,) <$> rndDist 3 0 <*> rndDist 3 0 <*> rndDist 3 0) [1 .. 1000 :: Int]) g1
    res = [ ( abs (polyLen g - d) <= 1e-6
            , all (\pp -> abs (sqrt (sumOf (map (^ (2 :: Int)) pp)) - 2) <= 1e-12) g
            , all (\pp -> abs (sumOf (fromSphere pp) - 1) <= 1e-12) g
            , abs (sqrt (sumOf (zipWith (\x y -> (x - y) ^ (2 :: Int)) a b)) - 4 * sin (d / 4)) <= 1e-12
            , polyLen (mPathOnSphere p q 3000) >= d - 1e-9 )
          | (p, q) <- pairs, let d = fisherRao p q, let g = geoPath p q 3000, let a = toSphere p, let b = toSphere q ]
    okArc = and [ x | (x, _, _, _, _) <- res ]
    okOn = and [ x | (_, x, _, _, _) <- res ]
    okBack = and [ x | (_, _, x, _, _) <- res ]
    okChord = and [ x | (_, _, _, x, _) <- res ]
    okShort = and [ x | (_, _, _, _, x) <- res ]
    okTri = and [ not (fisherRao a c > fisherRao a b + fisherRao b c + 1e-12) | (a, b, c) <- triples ]
    klBad = length [ () | (a, b, c) <- triples, divKL a c > divKL a b + divKL b c + 1e-12 ]
    pA = [0.7, 0.2, 0.1]
    qA = [0.1, 0.3, 0.6]
    p0 = [0.5, 0.3, 0.2]
    e = 1e-4
    dp = [e, -2 * e, e]
    q0 = zipWith (+) p0 dp
    dd = fisherRao p0 q0
    fi = sumOf (zipWith (\v pv -> v * v / pv) dp p0)
    checks =
      [ ("d = 2 arccos(Σ√pq) = 球の大円の長さ（3000 分割、50 組、誤差 1e-6）", okArc)
      , ("大円の上の点はどれも半径 2 の球の上、もどすと合計 1 の分布", okOn && okBack)
      , ("弦の長さ 2|√p − √q| = 4 sin(d/4)（誤差 1e-12）", okChord)
      , ("三角形のまっすぐ（m-測地線）の像は大円より長い（50 組）", okShort)
      , ("1000 組で三角不等式：フィッシャー-ラオはいつも成立、KL は " ++ show klBad ++ " 組で破れる", okTri && klBad > 0)
      , ("頂点どうしの距離 = π（最大）、同じ点は 0、向きによらない"
        , abs (fisherRao [1, 0, 0] [0, 1, 0] - pi) < 1e-12 && fisherRao pA pA < 1e-6 && abs (fisherRao pA qA - fisherRao qA pA) < 1e-15)
      , ("近くでは d² ≈ Σ dp²/p（フィッシャー計量、相対誤差 1e-3）", abs (dd * dd / fi - 1) < 1e-3) ]

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
