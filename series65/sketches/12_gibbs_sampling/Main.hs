-- ============================================
-- 65-12 ギブスサンプリング ─ 変数を 1 つずつ、「ほかを固定したときの分布（条件付き分布）」から引き直す
-- 2 変数の正規分布（相関 ρ）なら、x | y は 平均 ρy・分散 1 − ρ² の正規分布。x を引き、次に y を引く…をくり返す。
-- 動きは必ず軸に平行（階段）。相関が強いと細長い谷を小さな階段でしか進めず、ゆっくりになる（x の 1 つずれの自己相関 = ρ²）
-- M で 2 つ目の例：ノイズの入った白黒の画像。「となりどうしは同じ色になりやすい」（イジング事前分布）と
-- 「観測した色に近い」を合わせた条件付き確率で、画素を 1 つずつ引き直すとノイズが消えていく
-- 操作：M 例を切りかえ / ↑↓ 相関 ρ（例 1）・← → となりの結びつき J（例 2）/ N ノイズを作り直す / F 速さ / スペース 一時停止 / R やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), complement, shiftL, testBit)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

flipP :: Double
flipP = 0.15                                  -- ★ ノイズ：画素が反転している割合

imgW, imgH :: Int
imgW = 64                                     -- 画像の大きさ
imgH = 40

speedNames :: [String]
speedNames = ["slow", "1 step/frame", "full"]

fullSteps, fullSweeps :: Int
fullSteps = 3000                              -- ★ 「全力」のときの 1 フレームの回数（例 1）
fullSweeps = 20                               -- ★ 「全力」のときの 1 フレームのスイープ数（例 2）

dotK :: Int
dotK = 3                                      -- 点をためる絵は 1/3 の大きさ（3 倍にのばして描く）

data World = World
  { wRho     :: !Double                       -- ★ 相関（−0.99〜0.99）
  , wJ       :: !Double                       -- ★ となりの画素の結びつき J（0 = 事前分布なし）
  , wDemo    :: !Int
  , wSpeed   :: !Int
  , wPaused  :: !Bool
  -- 例 1：正規分布
  , wGp      :: !(Double, Double)
  , wStairs  :: [(Double, Double)]            -- 階段（新しい順、80 点まで）
  , wGxs     :: U.Vector Double               -- x の記録（20000 個まで）
  , wGn      :: !Int
  , wSxy     :: [Double]                      -- Σx, Σy, Σx², Σy², Σxy
  , wDotN    :: !Int                          -- 点の絵の 1 辺
  , wDots    :: U.Vector Double               -- 点の濃さ（0〜1）
  , wR1      :: Maybe Double                  -- 1 つずれの自己相関（20 フレームごと）
  -- 例 2：画像
  , wClean   :: U.Vector Int                  -- +1 = 白、−1 = 黒
  , wNoisy   :: U.Vector Int
  , wSpins   :: U.Vector Int
  , wPost    :: U.Vector Double
  , wSweeps  :: !Int
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-12 Gibbs sampling" world0)
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      let (_, _, side) = plotSq env
          n = max 1 (round side `div` dotK)
      pure (resetNormal world0 { wTest = selfTest, wDotN = n })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = makeImages (resetNormal (World 0.8 0.8 0 1 False (0, 0) [] U.empty 0 [] 1 U.empty Nothing U.empty U.empty U.empty U.empty 0 (rngOf 12) []))

-- ---- 例 1：2 変数の正規分布
resetNormal :: World -> World
resetNormal w = w { wGp = (3, -3), wStairs = [(3, -3)], wGxs = U.empty, wGn = 0, wSxy = [0, 0, 0, 0, 0], wDots = U.replicate (wDotN w * wDotN w) 0 }

-- ★ ギブスの 1 回 = x | y を引いて、y | x を引く
gibbsNormal :: (Double, Double) -> Double -> R (Double, Double)
gibbsNormal (_, py) r = do
  let sd = sqrt (1 - r * r)
  a <- randn
  let x = r * py + sd * a
  b <- randn
  let y = r * x + sd * b
  pure (x, y)

-- k 回すすめる（env は点の絵の場所に使う）
normalSteps :: Env -> Int -> World -> World
normalSteps env k w
  | k <= 0 = w
  | otherwise =
      let go 0 _ acc = pure (reverse acc)
          go j p acc = do q <- gibbsNormal p (wRho w); go (j - 1 :: Int) q (q : acc)
          (new, g) = runR (go k (wGp w) []) (wRng w)
          stairs = foldl' (\st (prev, q) -> q : (fst q, snd prev) : st) (wStairs w) (zip (wGp w : new) new)
          gx0 = wGxs w U.++ U.fromList (map fst new)
          sxy = foldl' (\s (x, y) -> case s of
                                       [a, b, c, d, e] -> forceL [a + x, b + y, c + x * x, d + y * y, e + x * y]
                                       _ -> s) (wSxy w) new
          (sx0, sy0, side) = plotSq env
          n = wDotN w
          cell = side / i2f n
          pix = [ iy * n + ix | q <- new, let (px, py) = toSq env q
                , let ix = floor ((px - sx0) / cell), let iy = floor ((py - sy0) / cell)
                , ix >= 0, iy >= 0, ix < n, iy < n ]
          dots = U.modify (\mv -> mapM_ (\i -> MU.modify mv (\a -> a + (1 - a) * dotAlpha) i) pix) (wDots w)
      in w { wGp = lastOr (wGp w) new, wStairs = take 80 stairs, wGn = wGn w + k
           , wGxs = U.drop (U.length gx0 - 20000) gx0, wSxy = sxy, wDots = dots, wRng = g }

-- 点 1 個の濃さ（透明度 70 の直径 3 の円を 3 × 3 の 1 マスにならしたもの）
dotAlpha :: Double
dotAlpha = 70 / 255 * (pi * 1.5 * 1.5 / 9)

sampleCorr :: [Double] -> Int -> Double      -- 相関係数（合計から）
sampleCorr s nI
  | nI < 2 = 0
  | otherwise = let n = i2d nI
                    [a, b, c, d, e] = take 5 (s ++ repeat 0)
                    mx = a / n
                    my = b / n
                    vx = c / n - mx * mx
                    vy = d / n - my * my
                in if vx > 0 && vy > 0 then (e / n - mx * my) / sqrt (vx * vy) else 0

lagCorr :: U.Vector Double -> Int -> Double  -- ずれ k の自己相関
lagCorr xs k =
  let n = U.length xs
      m = U.foldl' (+) 0 xs / i2d n
      v = U.foldl' (\s x -> s + (x - m) * (x - m)) 0 xs
      c = U.foldl' (+) 0 (U.zipWith (\a b -> (a - m) * (b - m)) xs (U.drop k xs))
  in if n <= k + 2 then 0 else if v > 0 then c / v else 0

plotSq :: Env -> (Float, Float, Float)       -- 正方形の左上と 1 辺
plotSq env = (40, 96, min (envH env - 140) (envW env * 0.5))

toSq :: Env -> (Double, Double) -> (Float, Float)
toSq env (x, y) = let (sx, sy, s) = plotSq env in (sx + f2 ((x + 3.5) / 7) * s, sy + f2 ((3.5 - y) / 7) * s)

-- ---- 例 2：ノイズ除去（値は +1 = 白、−1 = 黒）
cleanPixel :: Int -> Int -> Int              -- ★ 元の絵（自作の図形：丸、四角、斜めの帯）
cleanPixel i j
  | (x - 0.25) ** 2 * 2.5 + (y - 0.5) ** 2 < 0.07 = 1
  | x > 0.5 && x < 0.9 && y > 0.18 && y < 0.42 = 1
  | x > 0.5 && abs (y - 0.75 - (x - 0.7) * 0.6) < 0.08 = 1
  | otherwise = -1
  where x = i2d i / i2d imgW
        y = i2d j / i2d imgH

makeImages :: World -> World
makeImages w =
  let clean = U.generate (imgW * imgH) (\k -> let (j, i) = k `divMod` imgW in cleanPixel i j)
      (noisy, g) = runR (mapM (\c -> do u <- rnd; pure (if u < flipP then negate c else c)) (U.toList clean)) (wRng w)
  in resetSpins w { wClean = clean, wNoisy = U.fromList noisy, wRng = g }

resetSpins :: World -> World
resetSpins w = w { wSpins = wNoisy w, wPost = U.replicate (imgW * imgH) 0, wSweeps = 0 }

-- ★ 画素 k の条件付き確率 P(白 | となり, 観測) = 1 / (1 + exp(−2 (J Σ となり + h y_k)))、h = ½ ln((1 − ノイズ)/ノイズ)
gibbsSweep :: U.Vector Int -> U.Vector Int -> Double -> Int -> Int -> Rng -> (U.Vector Int, Rng)
gibbsSweep sp obs cj w h g0 = runST $ do
  mv <- U.thaw sp
  let hb = 0.5 * log ((1 - flipP) / flipP)
  g1 <- foldM (\g k -> do
                 let (j, i) = k `divMod` w
                 a <- if i > 0 then MU.read mv (k - 1) else pure 0
                 b <- if i < w - 1 then MU.read mv (k + 1) else pure 0
                 c <- if j > 0 then MU.read mv (k - w) else pure 0
                 d <- if j < h - 1 then MU.read mv (k + w) else pure 0
                 let nb = i2d (a + b + c + d)
                     (u, g') = rndStep g
                 MU.write mv k (if u < 1 / (1 + exp (-2 * (cj * nb + hb * i2d (obs U.! k)))) then 1 else -1)
                 pure g') g0 [0 .. w * h - 1]
  v <- U.freeze mv
  pure (v, g1)

imageSweep :: World -> World
imageSweep w =
  let (sp, g) = gibbsSweep (wSpins w) (wNoisy w) (wJ w) imgW imgH (wRng w)
      n = wSweeps w + 1
  in w { wSpins = sp, wSweeps = n, wRng = g, wPost = if n > 5 then U.zipWith (\p s -> p + i2d s) (wPost w) sp else wPost w }

errRate :: U.Vector Int -> U.Vector Int -> Double
errRate clean a = i2d (U.length (U.filter id (U.zipWith (/=) a clean))) / i2d (U.length clean)

postMeanImage :: World -> U.Vector Int
postMeanImage w = let n = i2d (max 1 (wSweeps w - 5)) in U.map (\v -> if wSweeps w > 5 then (if v / n >= 0 then 1 else -1) else 0) (wPost w)

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w2
  where
    fc = envFrame env
    reps = case wSpeed w0 of
      0 -> if fc `mod` 6 == 0 then 1 else 0
      1 -> 1
      _ -> if wDemo w0 == 0 then fullSteps else fullSweeps
    w1 | wPaused w0 = w0
       | wDemo w0 == 0 = normalSteps env reps w0
       | otherwise = iterate imageSweep w0 !! reps
    w2 = if wDemo w1 == 0 && (fc `mod` 20 == 0 || wR1 w1 == Nothing)
           then let gx = wGxs w1 in w1 { wR1 = Just (lagCorr (U.drop (U.length gx - 5000) gx) 1) }
           else w1

draw :: Env -> World -> Picture
draw env w = view env ((if wDemo w == 0 then drawNormal env w else drawImages env w) ++ [drawSelfTest env (wTest w)])

drawNormal :: Env -> World -> [Picture]
drawNormal env w =
  [ hudTitle ("Gibbs sampling (example 1: 2D normal): " ++ show (wGn w) ++ " steps   correlation rho = " ++ fmtN 2 rho)
             ("M switch example   Up/Down rho   F speed (" ++ at' speedNames (wSpeed w) "" ++ ")   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R reset")
  , imageBox sx sy side side n n (pixelImage n n 1 (\x y -> let a = wDots w U.! (y * n + x) in (mixW 42 dr a, mixW 38 dg a, mixW 35 db a)))
  , Color (col (70, 64, 58)) (Pictures [lineW 1 (sx, oy) (sx + side, oy), lineW 1 (ox, sy) (ox, sy + side)])
  -- 等高線：1σ・2σ の楕円（主軸は 45°）
  , Color (colA 160 cream) (Pictures [ polyline 1.5 [ toSq env ((a + b) / sqrt 2, (a - b) / sqrt 2)
                                                   | k <- [0 .. 80 :: Int], let t = i2d k / 80 * 2 * pi
                                                   , let a = rr * sqrt (1 + rho) * cos t, let b = rr * sqrt (1 - rho) * sin t ]
                                     | rr <- [1, 2] ])
  , Color (col mustard) (polyline 2 (map (toSq env) (reverse (wStairs w))))
  , let (cx, cy) = toSq env (wGp w) in Color (col teal) (circleAt cx cy 12)
  , plotAxes g1 "last 300 steps" "x trace (large rho: slow waves)" [0, 300] [-2, 0, 2]
  , drawCurve g1 (U.toList tl) teal 1.5 (300 - i2d (U.length tl))
  , say ("sample corr = " ++ fmtN 3 (sampleCorr (wSxy w) (wGn w)) ++ "   (target rho = " ++ fmtN 3 rho ++ ")") rx ty 15 (col cream)
  , say ("mean (x, y) = (" ++ fmtN 2 (s 0 / nn) ++ ", " ++ fmtN 2 (s 1 / nn) ++ ")   var x = " ++ fmtN 2 (s 2 / nn - (s 0 / nn) ** 2)) rx (ty + 26) 15 (col cream)
  , say ("lag-1 autocorr x = " ++ fmtN 3 (maybe 0 id (wR1 w)) ++ "   (rho^2 = " ++ fmtN 3 (rho * rho) ++ ")") rx (ty + 58) 15 (col mustard)
  , say ("ESS share ~ (1 - rho^2)/(1 + rho^2) = " ++ fmtN 3 ((1 - rho * rho) / (1 + rho * rho))) rx (ty + 84) 15 (col mustard)
  , Pictures [ say l rx (ty + 120 + 20 * i2f i) 14 (col sub) | (i, l) <- zip [0 :: Int ..]
      [ "conditionals: x | y ~ Normal(rho y, 1 - rho^2)"
      , "              y | x ~ Normal(rho x, 1 - rho^2)"
      , "one stair step moves only about sqrt(1 - rho^2),"
      , "the short width of the ellipse"
      , "-> rho = 0.99 crawls along the narrow valley;"
      , "   rho = 0 gives an independent sample each time" ] ] ]
  where
    rho = wRho w
    (sx, sy, side) = plotSq env
    n = wDotN w
    (dr, dg, db) = dice
    mixW base c a = fromIntegral (max 0 (min 255 (round (base * (1 - a) + realToFrac c * a) :: Int))) :: Word8
    (ox, oy) = toSq env (0, 0)
    rx = sx + side + 60
    rw = envW env - rx - 40
    g1 = Graph rx 110 rw 130 0 300 (-3.5) 3.5
    gl = U.length (wGxs w)
    tl = U.drop (gl - 300) (wGxs w)
    s i = at' (wSxy w) i 0
    nn = i2d (max 1 (wGn w))
    ty = 310

drawImages :: Env -> World -> [Picture]
drawImages env w =
  [ hudTitle ("Gibbs sampling (example 2: image denoising): " ++ show (wSweeps w) ++ " sweeps   J = " ++ fmtN 2 (wJ w) ++ "   noise " ++ show (jsRoundI (flipP * 100)) ++ "%")
             ("M switch example   <- -> J   N new noise   F speed (" ++ at' speedNames (wSpeed w) "" ++ ")   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R reset")
  , Pictures [ Pictures [ imageBox x y pw ph imgW imgH (spinPicture arr)
                        , say (name ++ (if k > 0 && (k < 3 || wSweeps w > 5) then "   errors " ++ fmtN 1 (100 * errRate (wClean w) arr) ++ "%" else "")) x (y - 8) 15 (col cream) ]
             | (k, (name, arr)) <- zip [0 :: Int ..] panels
             , let x = 40 + i2f (k `mod` 2) * (pw + 40)
             , let y = 110 + i2f (k `div` 2) * (ph + 50) ]
  , say "White = +1, black = -1.  P(white | neighbours, obs) = 1 / (1 + exp(-2 (J * sum of neighbours + h * obs)))" 40 (envH env - 48) 14 (col sub)
  , say "J = 0 just copies the observation; too large a J wipes out details" 40 (envH env - 28) 14 (col sub) ]
  where
    panels = [("Original (answer)", wClean w), ("Observed (noisy)", wNoisy w), ("Current sample", wSpins w), ("Posterior mean (vote)", postMeanImage w)]
    pw = min ((envW env - 120) / 2) ((envH env - 230) / 2 * (i2f imgW / i2f imgH))
    ph = pw * i2f imgH / i2f imgW
    q v = fromIntegral (max 0 (min 255 (round v :: Int))) :: Word8

-- +1 = 白、−1 = 黒、0 = 灰色の絵
spinPicture :: U.Vector Int -> Picture
spinPicture arr = pixelImage imgW imgH 1 (\i j ->
  let v = arr U.! (j * imgW + i)
      g = if v > 0 then 240 else if v < 0 then 36 else 110 :: Double
      q x = fromIntegral (max 0 (min 255 (round x :: Int))) :: Word8
  in (q g, q (g * 0.95), q (g * 0.85)))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wDemo w == 0 ->
    let r = min 0.99 (round2 (wRho w + 0.1)) in resetNormal w { wRho = if r > 0.9 then 0.99 else r }
  EventKey (SpecialKey KeyDown) Down _ _ | wDemo w == 0 ->
    resetNormal w { wRho = if wRho w >= 0.99 then 0.9 else max (-0.9) (round2 (wRho w - 0.1)) }
  EventKey (SpecialKey KeyRight) Down _ _ | wDemo w == 1 -> resetSpins w { wJ = min 2 (round1 (wJ w + 0.1)) }
  EventKey (SpecialKey KeyLeft) Down _ _ | wDemo w == 1 -> resetSpins w { wJ = max 0 (round1 (wJ w - 0.1)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'm' || c == 'M' -> w { wDemo = 1 - wDemo w }
    | c == 'n' || c == 'N' -> makeImages w
    | c == 'f' || c == 'F' -> w { wSpeed = (wSpeed w + 1) `mod` length speedNames }
    | c == 'r' || c == 'R' -> resetSpins (resetNormal w)
  _ -> w

-- ---------- やってみよう ----------
-- ・ρ = 0、0.8、0.99 で、階段の形とトレースの波打ち方を比べる。自己相関はいつも ρ² になっている？
-- ・ρ = −0.8 にすると楕円の向きは？ 自己相関 ρ² は正のまま？
-- ・例 2 で J = 0、0.8、2 を比べる。まちがいがいちばん少ない J は？
-- ・flipP を 0.3 に上げる。cleanPixel を書きかえて自分の絵（文字や模様）でも試す

-- ---- 自己テスト
normPdf2 :: Double -> Double -> Double -> Double
normPdf2 x y r = exp (negate (x * x - 2 * r * x * y + y * y) / (2 * (1 - r * r))) / (2 * pi * sqrt (1 - r * r))

normPdf1 :: Double -> Double -> Double -> Double
normPdf1 x m v = exp (negate ((x - m) ** 2) / (2 * v)) / sqrt (2 * pi * v)

selfTest :: [(String, Bool)]
selfTest =
  [ ("条件付き分布：p(x, y)/p(y) = 正規(ρy, 1 − ρ²)（ずれ " ++ fmtE worst ++ "）", worst < 1e-12)
  , ("ρ = 0.8：サンプルの相関 " ++ fmtN 4 rc ++ " ≈ 0.8", abs (rc - 0.8) < max 0.02 ((4 * (1 - 0.64)) / sqrt ess))
  , ("平均 ≈ 0、分散 ≈ 1（許容 4 SE）", abs (at' sx 0 0 / i2d nT) < 4 / sqrt ess && abs (at' sx 2 0 / i2d nT - 1) < 0.06)
  , ("x の 1 つずれの自己相関 " ++ fmtN 3 r1 ++ " ≈ ρ² = 0.64", abs (r1 - 0.64) < 0.03)
  , ("ρ = 0.99 は遅い：自己相関 " ++ fmtN 3 r99 ++ " ≈ 0.98", r99 > 0.95)
  , ("4 画素のギブス：行の合計 = 1、π P = π（π = ボルツマン分布）", rowSumErr km 1 < 1e-12 && maxAbsDiffV (vecMat piB km) piB < 1e-14 && maxAbsDiffV (maybe [] id (stationaryExact km)) piB < 1e-12)
  , ("ノイズ除去：まちがい " ++ fmtN 1 (100 * eNoisy) ++ "% → " ++ fmtN 1 (100 * eMean) ++ "%（半分未満）", eMean < eNoisy / 2) ]
  where
    worst = foldl' maxN 0 [ abs (normPdf2 x y 0.8 / normPdf1 y 0 1 - normPdf1 x (0.8 * y) (1 - 0.64)) | (x, y) <- [(0.3, -1.2), (2, 1.5), (-0.7, 0.4)] ]
    nT = 40000 :: Int
    chain :: Int -> Double -> R ([Double], [Double])
    chain cnt r = go cnt (0, 0) [] [0, 0, 0, 0, 0]
      where go 0 _ xs s = pure (reverse xs, s)
            go k p xs s = do
              q@(x, y) <- gibbsNormal p r
              go (k - 1) q (x : xs) (case s of
                                       [a, b, c, d, e] -> forceL [a + x, b + y, c + x * x, d + y * y, e + x * y]
                                       _ -> s)
    (xs08, sx) = withSeed 1201 (chain nT 0.8)
    ess = (i2d nT * (1 - 0.64)) / (1 + 0.64)
    rc = sampleCorr sx nT
    r1 = lagCorr (U.fromList xs08) 1
    r99 = lagCorr (U.fromList (fst (withSeed 1202 (chain 20000 0.99)))) 1
    -- 2 × 2 の画像（4 画素）：ランダムに 1 画素えらぶギブスの遷移行列 16 × 16 を正確に作る
    obs = [1, -1, 1, 1] :: [Double]
    cj = 0.7
    hb = 0.4
    edges = [(0, 1), (2, 3), (0, 2), (1, 3)]
    nbrs = [[1, 2], [0, 3], [0, 3], [1, 2]]
    spinOf st i = if testBit st i then 1 else -1 :: Double
    boltz = [ exp (foldl' (\e i -> e + hb * at' obs i 0 * spinOf st i) (foldl' (\e (a, b) -> e + cj * spinOf st a * spinOf st b) 0 edges) [0 .. 3]) | st <- [0 .. 15 :: Int] ]
    zb = sumOf boltz
    piB = map (/ zb) boltz
    km = foldl' (\m (st, i) ->
                   let nb = foldl' (\acc j -> acc + spinOf st j) 0 (at' nbrs i [])
                       pUp = 1 / (1 + exp (-2 * (cj * nb + hb * at' obs i 0)))
                   in modifyMat st (st .&. complement (shiftL 1 i)) (+ 0.25 * (1 - pUp)) (modifyMat st (st .|. shiftL 1 i) (+ 0.25 * pUp) m))
                (zerosMat 16) [ (st, i) | st <- [0 .. 15], i <- [0 .. 3] ]
    -- ノイズ除去：事後平均の多数決は、観測よりずっとまちがいが少ない
    wImg = makeImages (World 0.8 0.8 1 1 False (0, 0) [] U.empty 0 [] 1 U.empty Nothing U.empty U.empty U.empty U.empty 0 (rngOf 1203) [])
    eNoisy = errRate (wClean wImg) (wNoisy wImg)
    eMean = errRate (wClean wImg) (postMeanImage (iterate imageSweep wImg !! 40))

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

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

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
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

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

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

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

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

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
