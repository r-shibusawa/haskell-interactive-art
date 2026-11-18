-- ============================================
-- 52-08 色の弁別 ─ 色の「違いがわかる範囲」は楕円になる（マカダムの考え方）
-- 円の左半分が基準の色、右半分が比べる色。CIELAB の a*b* 平面で、8 つの方向に基準からずらしてある
-- ←→（細かく）↑↓（大きく）で差を変え、「ちょうど違いがわからなくなった」所でスペース（調整法）
-- 8 方向の点に楕円 A x² + B xy + C y² = 1 を最小二乗で当てはめ、長い軸・短い軸・向きを表示する。差の大きさは ΔE（Lab の距離）
-- 操作：←→ ↑↓ = 差 / スペース = 決定 / C = 基準の色を変える / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 基準の色（名前と CIELAB の L*, a*, b*）
refs :: [(String, Lab)]
refs =                               -- ★ 基準の色
  [ ("gray", (60, 0, 0)), ("reddish skin", (65, 18, 22)), ("green", (60, -30, 20))
  , ("blue", (50, 5, -35)), ("yellow", (80, 0, 50)) ]

nDir :: Int
nDir = 8                             -- ★ 方向の数

-- ★ シミュレーション観察者の本当の楕円（半径 ΔE と向き [度]）、答えのゆらぎ
simA, simB, simAng, simNoise :: Double
simA = 3
simB = 1.2
simAng = 40
simNoise = 0.08

-- 楕円 A x² + B xy + C y² = 1。軸 = Just (長い軸, 短い軸, 向き)（楕円にならなければ Nothing）
data Ell = Ell { eA, eB, eC :: !Double, eAxes :: Maybe (Double, Double, Double) }

data World = World
  { wRi      :: !Int
  , wDirs    :: [Double]
  , wDi      :: !Int
  , wR       :: !Double          -- いまの差 ΔE
  , wPts     :: [(Double, Double)]
  , wFit     :: Maybe Ell
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-08 Color discrimination" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      r <- freshSeed
      pure (resetData False r world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetData False (seedRng 1) (World 0 [] 0 10 [] Nothing False 1 (seedRng 1) [])

refLab :: World -> Lab
refLab w = snd (getAt ("", (60, 0, 0)) refs (wRi w))

resetData :: Bool -> Rng -> World -> World
resetData sim r0 w = w { wSim = sim, wPts = [], wFit = Nothing, wDirs = ds, wDi = 0, wR = rr, wRng = r' }
  where
    ((ds, rr), r') = flip runRand r0 $ do
      d <- shuffleArr [ fromIntegral k * 2 * pi / fromIntegral nDir | k <- [0 .. nDir - 1] ]
      u <- rnd
      pure (d, 10 + 6 * u)

compLab :: World -> Double -> Double -> Lab
compLab w th rr = let (l, a, b) = refLab w in (l, a + rr * cos th, b + rr * sin th)

-- 次の方向は大きめの差から始める
accept :: World -> World
accept w
  | wDi w >= length (wDirs w) = w
  | otherwise = w { wPts = pts, wDi = wDi w + 1, wR = 10 + 6 * u, wRng = r', wFit = fitEllipse pts }
  where
    th = getAt 0 (wDirs w) (wDi w)
    pts = wPts w ++ [(wR w * cos th, wR w * sin th)]
    (u, r') = runRand rnd (wRng w)

-- 楕円 A x² + B xy + C y² = 1：未知数 A, B, C について 1 次式なので、ふつうの最小二乗（3 × 3 の連立方程式）
fitEllipse :: [(Double, Double)] -> Maybe Ell
fitEllipse p
  | length p < 3 = Nothing
  | abs d < 1e-12 = Nothing
  | not (l1 > 0 && l2 > 0) = Just (Ell a b c Nothing)
  | otherwise = Just (Ell a b c (Just (1 / sqrt l1, 1 / sqrt l2, 0.5 * atan2 b (a - c) + pi / 2)))   -- 長い軸 = 小さい固有値の向き
  where
    rows = [ [x * x, x * y, y * y] | (x, y) <- p ]
    m = [ [ sum [ (q !! i) * (q !! j) | q <- rows ] | j <- [0 .. 2] ] | i <- [0 .. 2 :: Int] ]
    v = [ sum [ q !! i | q <- rows ] | i <- [0 .. 2 :: Int] ]
    d = det3 m
    sol k = det3 [ [ if j == k then vi else x | (j, x) <- zip [0 :: Int ..] row ] | (row, vi) <- zip m v ] / d
    a = sol 0
    b = sol 1
    c = sol 2
    tr = a + c
    dd = sqrt (((a - c) / 2) ^ (2 :: Int) + (b / 2) ^ (2 :: Int))
    l1 = tr / 2 - dd                 -- 2 × 2 行列の固有値
    l2 = tr / 2 + dd

-- 向き（0〜180 度）
angDeg :: Double -> Double
angDeg a = jsMod (jsMod (a * 180 / pi) 180 + 180) 180
  where jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)

det3 :: [[Double]] -> Double
det3 m = g 0 0 * (g 1 1 * g 2 2 - g 1 2 * g 2 1) - g 0 1 * (g 1 0 * g 2 2 - g 1 2 * g 2 0) + g 0 2 * (g 1 0 * g 2 1 - g 1 1 * g 2 0)
  where g i j = getAt 0 (getAt [] m i) j

-- 方向 θ の半径
ellipseR :: Ell -> Double -> Double
ellipseR e th = let c = cos th; s = sin th in 1 / sqrt (eA e * c * c + eB e * c * s + eC e * s * s)

simEllipse :: Ell
simEllipse = Ell (c * c * ia + s * s * ib) (2 * c * s * (ia - ib)) (s * s * ia + c * c * ib) Nothing
  where t = simAng * pi / 180
        c = cos t
        s = sin t
        ia = 1 / simA ^ (2 :: Int)
        ib = 1 / simB ^ (2 :: Int)

-- シミュレーション観察者：本当の楕円の半径 ×（1 + ゆらぎ）で答える（n 方向）
simPoints :: Int -> Rand [(Double, Double)]
simPoints n = mapM one [0 .. n - 1]
  where one k = do
          e <- rndNormal
          let th = fromIntegral k * 2 * pi / fromIntegral n
              rr = ellipseR simEllipse th * (1 + simNoise * e)
          pure (rr * cos th, rr * sin th)

runSim :: Word32 -> Rng -> World -> World
runSim seed fresh w0 = w { wPts = pts, wDi = length (wDirs w), wFit = fitEllipse pts, wRng = r' }
  where
    w = resetData True fresh w0
    (pts, r') = runRand (simPoints 16) (seedRng seed)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w

rgbCol :: (Double, Double, Double) -> Color
rgbCol (r, g, b) = makeColor (f2 (clampv 0 1 (r / 255))) (f2 (clampv 0 1 (g / 255))) (f2 (clampv 0 1 (b / 255))) 1

-- 半円（a0 から a1 まで、p5.js の角度：y は下向き）
halfDisc :: Float -> Float -> Float -> Float -> Float -> Picture
halfDisc cx cy r a0 a1 = Polygon ((cx, cy) : [ (cx + r * cos a, cy + r * sin a) | i <- [0 .. 48 :: Int], let a = a0 + (a1 - a0) * fromIntegral i / 48 ])

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Left/Right fine   Up/Down coarse   Space: decide where the difference just disappears") 20 30 18 (gray255 235)
  , say ("C reference colour (" ++ refName ++ ")   O simulated observer   R reset") 20 54 16 (gray255 235)
  , say "Make the two halves look the same, at the very edge. You may go back if you went too far." 20 76 14 (gray255 170)
    -- ---- 左右に分かれた円（基準 | 比較）
  , Color (gray255 (f2 (linToSrgb 0.18))) (rectTL (cx - rr - 30) (cy - rr - 30) (2 * rr + 60) (2 * rr + 60)) ]
  ++ (if wDi w < length (wDirs w)
        then let (crgb, inG) = labToRgb (compLab w (getAt 0 (wDirs w) (wDi w)) (wR w))
             in [ Color (rgbCol refRgb) (halfDisc cx cy rr (pi / 2) (3 * pi / 2))
                , Color (rgbCol crgb) (halfDisc cx cy rr (-pi / 2) (pi / 2))
                , say ("Direction " ++ show (wDi w + 1) ++ " / " ++ show (length (wDirs w)) ++ "   difference now dE = " ++ fmt 2 (wR w)) tx (cy + rr + 60) 15 (gray255 220) ]
                ++ [ say "(outside the screen's colours: clipped)" tx (cy + rr + 82) 15 (gray255 220) | not inG ]
        else [ Color (rgbCol refRgb) (circleD cx cy (2 * rr))
             , say (if wSim w then "Simulated observer (16 directions)" else "Done! C = another colour, R = again") tx (cy + rr + 60) 15 (gray255 220) ])
  ++ [ say ((if wSim w then "Simulated observer" else "You are the observer") ++ "   reference L*a*b* = (" ++ fmt 0 l0 ++ ", " ++ fmt 0 a0 ++ ", " ++ fmt 0 b0 ++ ")") tx (cy + rr + 106) 14 (gray255 200)
    -- ---- a*b* 平面の楕円
     , Color (gray255 95) (frameTL 1 gxx gyy gs gs)
     , Color (gray255 70) (Line [(gxx, oy), (gxx + gs, oy)]), Color (gray255 70) (Line [(ox, gyy), (ox, gyy + gs)])
     , say "+a* (red)" (gxx + gs - 90) (oy - 8) 14 (gray255 170)
     , say "+b* (yellow)" (ox + 6) (gyy + 18) 14 (gray255 170)
     , say ("+-" ++ fmt 0 lim ++ " dE") (gxx + 6) (gyy + gs - 8) 14 (gray255 170) ]
  ++ [ drawE simEllipse (rgb255 120 200 140) 1.5 | wSim w ]
  ++ [ drawE e (rgb255 255 170 90) 3 | Just e <- [wFit w], Just _ <- [eAxes e] ]
  ++ [ Color (rgb255 120 190 255) (circleD (ox + k * f2 (clampv (-lim) lim x)) (oy - k * f2 (clampv (-lim) lim y)) 9) | (x, y) <- wPts w ]
  ++ [ Color (rgba255 255 255 255 120) (Line [(ox, oy), (ox + k * f2 (min (wR w) lim * cos th), oy - k * f2 (min (wR w) lim * sin th))])
     | wDi w < length (wDirs w), let th = getAt 0 (wDirs w) (wDi w) ]
  ++ (case wFit w of
        Just (Ell _ _ _ (Just (ma, mi, an))) ->
          [ say ("Ellipse: major axis dE " ++ fmt 2 ma ++ ", minor dE " ++ fmt 2 mi) gxx ry 16 (gray255 235)
          , say ("angle " ++ fmt 0 (angDeg an) ++ " deg") gxx (ry + 22) 16 (gray255 235) ]
        Just _ -> [ say "Not an ellipse (add points or measure again)" gxx ry 16 (gray255 235) ]
        Nothing -> [ say "Fits an ellipse with 3 or more directions" gxx ry 16 (gray255 235) ])
  ++ [ say "dE ~ 1-2 is the usual 'just noticeable' guide." gxx (ry + 46) 14 (gray255 170)
     , say "Size and angle of the ellipse depend on the reference colour." gxx (ry + 64) 14 (gray255 170) ]
  ++ [ say ("True ellipse: major " ++ prec3 simA ++ ", minor " ++ prec3 simB ++ ", angle " ++ prec3 simAng ++ " deg") gxx (ry + 88) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    (refName, (l0, a0, b0)) = getAt ("", (60, 0, 0)) refs (wRi w)
    refRgb = fst (labToRgb (refLab w))
    rr = min (envW env * 0.16) (envH env * 0.25)
    cx = envW env * 0.25
    cy = 120 + rr
    tx = cx - rr - 30
    gxx = envW env * 0.56
    gyy = 110
    gs = min (envW env * 0.38) (envH env * 0.55)
    lim = 8 :: Double
    ox = gxx + gs / 2
    oy = gyy + gs / 2
    k = gs / (2 * f2 lim)
    ry = gyy + gs + 40
    drawE e c wt = Color c (polyline wt [ (ox + k * f2 (r' * cos th), oy - k * f2 (r' * sin th))
                                        | i <- [0 .. 120 :: Int], let th = fromIntegral i / 120 * 2 * pi, let r' = min (ellipseR e th) (lim * 1.4) ])

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wR = clampv 0.2 40 (wR w / 1.06) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wR = clampv 0.2 40 (wR w * 1.06) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wR = clampv 0.2 40 (wR w / 1.3) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wR = clampv 0.2 40 (wR w * 1.3) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (accept w)
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> do r <- freshSeed
                          pure (resetData False r w { wRi = (wRi w + 1) `mod` length refs })
    | c `elem` "oO" -> do r <- freshSeed
                          let s = wSimSeed w + 1
                          pure (runSim s r w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("sRGB white -> Lab = (100, 0, 0)", let (l, a, b) = rgbToLab (255, 255, 255) in near l 100 1e-6 && near a 0 1e-6 && near b 0 1e-6)
  , ("dE between the same colour = 0, black -> L* = 0", near (deltaE (rgbToLab (200, 50, 30)) (rgbToLab (200, 50, 30))) 0 1e-9 && near (fst3 (rgbToLab (0, 0, 0))) 0 1e-9)
  , ("sRGB -> Lab -> sRGB comes back (200, 50, 30)", let ((r, g, b), _) = labToRgb (rgbToLab (200, 50, 30)) in near r 200 1e-6 && near g 50 1e-6 && near b 30 1e-6)
  , ("a* = 120 is outside the screen gamut", not (snd (labToRgb (50, 120, 0))) && snd (labToRgb (60, 0, 0)))
  , ("8 noise-free points give back the ellipse (3, 1.2, 40 deg)"
    , case fitEllipse p8 of
        Just (Ell _ _ _ (Just (ma, mi, an))) -> near ma 3 1e-6 && near mi 1.2 1e-6 && near (angDeg an) 40 1e-4
        _ -> False)
  , ("points on a circle (radius 2): major = minor = 2"
    , case fitEllipse [(2, 0), (0, 2), (-2, 0), (0, -2), (sqrt 2, sqrt 2)] of
        Just (Ell _ _ _ (Just (ma, mi, _))) -> near ma 2 1e-9 && near mi 2 1e-9
        _ -> False)
  , ("simulated observer, 64 directions: axis lengths within 15%"
    , case fitEllipse (fst (runRand (simPoints 64) (seedRng 52))) of
        Just (Ell _ _ _ (Just (ma, mi, _))) -> near ma 3 0.45 && near mi 1.2 0.18
        _ -> False) ]
  where
    fst3 (a, _, _) = a
    p8 = [ (rr * cos th, rr * sin th) | i <- [0 .. 7 :: Int], let th = fromIntegral i * pi / 4, let rr = ellipseR simEllipse th ]

-- ---------- やってみよう ----------
-- 1. 灰色で 8 方向を測ろう。楕円は円に近い？ どの方向に細長い？
-- 2. C で緑や青に変えて測り直そう。楕円の大きさや向きはどう変わる？（マカダムの楕円も場所で形がちがう）
-- 3. 自分の ΔE は 1〜2 くらい？ 画面の明るさや部屋の明かりで変わる？
-- 4. 文字の色と背景の色の ΔE がいくつなら読みやすい？ 色覚の多様性も考えよう
-- 5. nDir を 16 にすると楕円は安定する？ O でシミュレーション観察者と比べよう

-- ==================================================
-- ここから下は「心理物理の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列）。p5.js 版の rnd()（mulberry32）と同じ数が出る
newtype Rng = Rng Word32

-- 種 0 は 1 にする（p5.js 版の (s >>> 0) || 1 と同じ）
seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 乱数を「順番に」使う小さなしくみ：Rand a は「乱数の状態を受け取って、値と次の状態を返す」計算
newtype Rand a = Rand { runRand :: Rng -> (a, Rng) }

instance Functor Rand where
  fmap f (Rand g) = Rand (\r -> let (a, r') = g r in (f a, r'))
instance Applicative Rand where
  pure a = Rand (\r -> (a, r))
  Rand f <*> Rand g = Rand (\r -> let (h, r1) = f r; (a, r2) = g r1 in (h a, r2))
instance Monad Rand where
  Rand g >>= k = Rand (\r -> let (a, r1) = g r in runRand (k a) r1)

-- 0〜1 の一様乱数
rnd :: Rand Double
rnd = Rand $ \(Rng s0) ->
  let a = s0 + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法。u を先、v を後に引く）
rndNormal :: Rand Double
rndNormal = do
  u <- fmap (1 -) rnd
  v <- rnd
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー＝イェーツのシャッフル（後ろから順に入れかえる）
shuffleArr :: [a] -> Rand [a]
shuffleArr xs = go (V.fromList xs) (length xs - 1)
  where
    go v i | i <= 0 = pure (V.toList v)
           | otherwise = do
               u <- rnd
               let j = floor (u * fromIntegral (i + 1))
               go (v V.// [(i, v V.! j), (j, v V.! i)]) (i - 1)

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Rng
freshSeed = seedRng <$> randomRIO (1, maxBound)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の Math.round（.5 は大きい方へ）
roundJ :: Double -> Double
roundJ x = fromIntegral (floor (x + 0.5) :: Int)

-- ---- 正規分布：Φ（累積分布）と z（その逆関数。二分法で数値的に解く）
-- 誤差関数（Abramowitz & Stegun 7.1.26、誤差 1.5e-7 以下）
erfA :: Double -> Double
erfA x0 = s * y
  where
    s = if x0 < 0 then -1 else 1
    x = abs x0
    t = 1 / (1 + 0.3275911 * x)
    y = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-x * x)

phi :: Double -> Double
phi x = 0.5 * (1 + erfA (x / sqrt 2))

-- Φ(z) = p となる z
zOf :: Double -> Double
zOf p0 = go (-9) 9 (70 :: Int)
  where
    p = clampv 1e-9 (1 - 1e-9) p0
    go lo hi 0 = (lo + hi) / 2
    go lo hi n = let m = (lo + hi) / 2 in if phi m < p then go m hi (n - 1) else go lo m (n - 1)

-- 割合を 1/(2N) で補正（0 や 1 だと z が無限大になるので）
rateFix :: Double -> Double -> Double
rateFix k n = if n > 0 then clampv (1 / (2 * n)) (1 - 1 / (2 * n)) (k / n) else 0.5

-- ---- 色の道具：sRGB の値（0〜255）⇔ 光の量（0〜1）
srgbToLin :: Double -> Double
srgbToLin c0 = let c = c0 / 255 in if c <= 0.04045 then c / 12.92 else ((c + 0.055) / 1.055) ** 2.4

linToSrgb :: Double -> Double
linToSrgb v0 = let v = clampv 0 1 v0 in 255 * (if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055)

-- 光の量 ⇔ XYZ ⇔ CIELAB（白 = RGB(255,255,255) = D65）
mXYZ :: [[Double]]
mXYZ = [[0.4124, 0.3576, 0.1805], [0.2126, 0.7152, 0.0722], [0.0193, 0.1192, 0.9505]]

-- RGB = (1,1,1) の XYZ を白とする
whiteXYZ :: [Double]
whiteXYZ = map sum mXYZ

labF, labFi :: Double -> Double
labF t = if t > 216 / 24389 then t ** (1 / 3) else (24389 / 27 * t + 16) / 116
labFi f = if f * f * f > 216 / 24389 then f * f * f else (116 * f - 16) / (24389 / 27)

type Lab = (Double, Double, Double)

rgbToLab :: (Double, Double, Double) -> Lab
rgbToLab (r, g, b) = (116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz))
  where
    l = map srgbToLin [r, g, b]
    xyz = [ sum (zipWith (*) row l) | row <- mXYZ ]
    [fx, fy, fz] = zipWith (\v wv -> labF (v / wv)) xyz whiteXYZ

-- 戻り値 (RGB, 画面で出せる色か)。色域の外なら切りつめる
labToRgb :: Lab -> ((Double, Double, Double), Bool)
labToRgb (lL, la, lb) = ((linToSrgb r, linToSrgb g, linToSrgb b), all (\v -> v >= -1e-4 && v <= 1 + 1e-4) lin)
  where
    fy = (lL + 16) / 116
    fx = fy + la / 500
    fz = fy - lb / 200
    xyz = zipWith (*) [labFi fx, labFi fy, labFi fz] whiteXYZ
    m i j = getAt 0 (getAt [] mXYZ i) j
    d = m 0 0 * (m 1 1 * m 2 2 - m 1 2 * m 2 1) - m 0 1 * (m 1 0 * m 2 2 - m 1 2 * m 2 0) + m 0 2 * (m 1 0 * m 2 1 - m 1 1 * m 2 0)
    inv = [ [(m 1 1 * m 2 2 - m 1 2 * m 2 1) / d, (m 0 2 * m 2 1 - m 0 1 * m 2 2) / d, (m 0 1 * m 1 2 - m 0 2 * m 1 1) / d]
          , [(m 1 2 * m 2 0 - m 1 0 * m 2 2) / d, (m 0 0 * m 2 2 - m 0 2 * m 2 0) / d, (m 0 2 * m 1 0 - m 0 0 * m 1 2) / d]
          , [(m 1 0 * m 2 1 - m 1 1 * m 2 0) / d, (m 0 1 * m 2 0 - m 0 0 * m 2 1) / d, (m 0 0 * m 1 1 - m 0 1 * m 1 0) / d] ]
    lin = [ sum (zipWith (*) row xyz) | row <- inv ]
    (r, g, b) = (getAt 0 lin 0, getAt 0 lin 1, getAt 0 lin 2)

-- ΔE（CIE76）= Lab 空間の距離
deltaE :: Lab -> Lab -> Double
deltaE (a1, b1, c1) (a2, b2, c2) = sqrt ((a1 - a2) ^ (2 :: Int) + (b1 - b2) ^ (2 :: Int) + (c1 - c2) ^ (2 :: Int))

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 t) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 t) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (gray255 95) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 20) 14 (gray255 165) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 6) (gY g v + 6) 14 (gray255 165) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 44) 14 (gray255 205)
     , say yl (gx g) (gy g - 10) 14 (gray255 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

gray255 :: Float -> Color
gray255 v = gray (v / 255)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- Maybe の値（null なら不合格）
nearM :: Maybe Double -> Double -> Double -> Bool
nearM m b tol = maybe False (\a -> near a b tol) m

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 120 210 150 else rgb255 255 90 90)
