-- ============================================
-- 68-02 カバーの定理と線形分離の容量 ─ N 個の点の色分けは 2^N 通り。そのうち 1 本の直線（超平面）で分けられるのは何通り？
-- カバー（1965）：点が「一般の位置」（平面なら 3 点が一直線に並ばない）なら、ちょうど C(N, d) = 2 Σ_{k=0}^{d−1} (N−1 k) 通り
--   d = 重みの数（バイアスを含む）。平面の直線 w1 x + w2 y + w3 なら d = 3。N = 4 なら 16 通り中 14 通り（XOR の 2 通りがだめ）
-- 割合 C(N, d)/2^N は、N ≤ d なら 1、N = 2d でちょうど 1/2、それより多いと急に 0 へ → 「容量」は重み 1 個あたり 2 点
-- 下の小さな絵が 2^N 通りの色分け（明るいふち = 直線で分けられる）。総当たりで数えて、式と比べる
-- 操作：クリック 点を置く / ドラッグ 動かす / ↑↓ 点の数 N（並べ直し）/ R ランダムに置き直す / L 一直線に並べる / マウスを小さな絵に 分け方を見る
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)
import qualified Data.Set as Set
dimD :: Int
dimD = 3                   -- 平面の直線の重みの数（x, y, バイアス）

type P2 = (Double, Double)

-- 直線で分けられるとき：法線 (nx, ny) と しきい値
data Sep = Sep { spOk :: !Bool, spNx :: !Double, spNy :: !Double, spThr :: !Double }

data World = World
  { wPts    :: ![P2]
  , wSep    :: !(V.Vector Sep)        -- マスクごとの結果
  , wNSep   :: !Int
  , wColli  :: !Bool
  , wDrag   :: !Int
  , wSeed   :: !Int
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-02 Cover's theorem" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = randomPoints 5 (World [] V.empty 0 False (-1) 1 [])

-- ★ カバーの式（整数で）：C(N, d) = 2 Σ_{k=0}^{d−1} binom(N−1, k)
binomInt :: Int -> Int -> Integer
binomInt n k = if k < 0 || k > n then 0 else product [ toInteger (n - k + i) | i <- [1 .. k] ] `div` product [ toInteger i | i <- [1 .. k] ]

coverCount :: Int -> Int -> Integer
coverCount n d = if n <= 0 then 1 else 2 * sum [ binomInt (n - 1) k | k <- [0 .. d - 1] ]

-- 割合 C(N, d)/2^N = P(2 項分布 Bin(N−1, 1/2) ≤ d − 1)（大きな N でもあふれないよう対数で）
coverFrac :: Int -> Int -> Double
coverFrac n d
  | n <= d = 1
  | otherwise = min 1 (go 0 ((i2d n - 1) * log 0.5) 0)
  where go k lp s = if k >= d then s else go (k + 1) (lp + log ((i2d n - 1 - i2d k) / (i2d k + 1))) (s + exp lp)

bit :: Int -> Int -> Bool
bit m k = testBit m k

-- ★ 総当たり：マスク m（ビット i が 1 = 点 i はマゼンタ）が直線で分けられるか。
--   分ける向きの範囲のはしは「2 点を通る直線」の向きなので、その向きを ±ε 回した法線を全部ためす（一直線の配置でも正しい）
separable :: [P2] -> Int -> Sep
separable ps m
  | m == 0 || m == full = Sep True 0 1 (-10)
  | otherwise = headOr (Sep False 0 0 0) tries
  where
    n = length ps
    full = 2 ^ n - 1
    pv = V.fromList ps
    tries = [ Sep True nx ny ((hi0 + lo1) / 2)
            | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]
            , let (xi, yi) = pv V.! i
                  (xj, yj) = pv V.! j
                  a0 = atan2 (xj - xi) (-(yj - yi))
            , da <- [1e-7, -1e-7, pi + 1e-7, pi - 1e-7]
            , let nx = cos (a0 + da)
                  ny = sin (a0 + da)
                  (lo1, hi0) = foldl' (\(lo, hi) (k, (x, y)) -> let s = nx * x + ny * y in if bit m k then (min lo s, hi) else (lo, max hi s)) (posInf, negInf) (zip [0 ..] ps)
            , hi0 < lo1 - 1e-12 ]

crossOf :: P2 -> P2 -> P2 -> Double
crossOf (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

-- もう 1 つの数え方：2 点 i, j を通る直線で残りを分け、i, j の色は 4 通りを全部（一般の位置で使える）
countByPairs :: [P2] -> Int
countByPairs ps = Set.size (Set.fromList ([0, 2 ^ n - 1 :: Int] ++ concat
  [ [ side .|. ((a .&. 1) `shiftL` i) .|. ((a `shiftR` 1) `shiftL` j) | side <- [base, rest - base], a <- [0 .. 3] ]   -- 左右どちらをマゼンタにしても
  | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]
  , let others = [ k | k <- [0 .. n - 1], k /= i, k /= j ]
        rest = foldl' (\r k -> r .|. (1 `shiftL` k)) 0 others
        base = foldl' (\r k -> if crossOf (pv V.! i) (pv V.! j) (pv V.! k) > 0 then r .|. (1 `shiftL` k) else r) 0 others ]))
  where n = length ps
        pv = V.fromList ps

hasCollinear :: [P2] -> Bool
hasCollinear ps = or [ abs (crossOf a b c) < 2e-3 | (i, a) <- ips, (j, b) <- ips, j > i, (k, c) <- ips, k > j ]
  where ips = zip [0 :: Int ..] ps

countSeparable :: [P2] -> Int
countSeparable ps = length [ () | m <- [0 .. 2 ^ length ps - 1], spOk (separable ps m) ]

analyze :: World -> World
analyze w = let ps = wPts w
                sv = V.generate (2 ^ length ps) (separable ps)
            in w { wSep = sv, wNSep = V.length (V.filter spOk sv), wColli = hasCollinear ps }

randomPoints :: Int -> World -> World
randomPoints n w = analyze w { wPts = withSeed (300 + wSeed w) (mapM (const (do a <- rnd; b <- rnd; pure (0.1 + 0.8 * a, 0.1 + 0.8 * b))) [1 .. n]) }

sideOf :: Env -> Float
sideOf env = min 320 (envH env * 0.42)

-- 下の小さな絵の並べ方：1 行の数、大きさ、上の y
thumbLayout :: Env -> Int -> (Int, Float, Float)
thumbLayout env n = (perRow, tw, 90 + sideOf env + 70)
  where perRow = if n <= 4 then 2 ^ n else if n <= 6 then 16 else 32
        tw = min 64 ((envW env - 60) / i2f perRow - 4)

-- マウスがのっている小さな絵の番号（なければ −1）
hoverMask :: Env -> World -> Int
hoverMask env w =
  let n = length (wPts w)
      (perRow, tw, ty) = thumbLayout env n
      rows = (2 ^ n + perRow - 1) `div` perRow
      bh = max 10 (i2f (jsRound (d2 (i2f rows * (tw + 4) + 8))))
      (mx, my) = mouseP env
      c = floor ((mx - 30) / (tw + 4)) :: Int
      r = floor ((my - ty - 4) / (tw + 4)) :: Int
      m = r * perRow + c
  in if my > ty && my < ty + bh && c >= 0 && c < perRow && m >= 0 && m < 2 ^ n then m else -1

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Cover's theorem: N = " ++ show n ++ ", split by a line (d = 3): " ++ show (wNSep w) ++ " / " ++ show (2 ^ n :: Int)
              ++ "   C(" ++ show n ++ ", 3) = " ++ show cf ++ (if toInteger (wNSep w) == cf then "  match" else "  (fewer: not general position)"))
             "click add (max 9)   drag move   Up/Down N   R re-place   L on a line   hover a small picture: its coloring and line"
  -- ---- 左上：点
  , Color (col panel) (rectTL 30 90 side side)
  , if hm >= 0 && hm /= 0 && hm /= 2 ^ n - 1 && spOk sp
      then let c = spThr sp
               nx = spNx sp
               ny = spNy sp
               end t = (30 + f2 (c * nx - t * ny) * side, 90 + (1 - f2 (c * ny + t * nx)) * side)
           in maybe Blank (\(p, q) -> Color (col amber) (lineW 2 p q)) (clipSeg 30 90 side side (end (-2)) (end 2))
      else Blank
  , Pictures [ Pictures [ Color (grayC 10) (circleAt px py 18)
                        , Color (col (if hm >= 0 then (if bit hm i then mag else lime) else ink)) (circleAt px py 16) ]
             | (i, (x, y)) <- zip [0 ..] (wPts w), let px = 30 + f2 x * side, let py = 90 + (1 - f2 y) * side ]
  , if wColli w then say "! 3 points almost on one line -> not in general position, fewer than the formula" 30 (90 + side + 22) 14 (col amber)
    else say "general position (no 3 points on one line)" 30 (90 + side + 22) 14 (col sub)
  , if hm >= 0 then say ("coloring #" ++ show hm ++ ": " ++ (if spOk sp then "split by a line (orange)" else "cannot be split by a line")) 30 (90 + side + 44) 14 (col (if spOk sp then lime else mag)) else Blank
  -- ---- 右上：カバーの曲線
  , plotAxes g "N / d (points per weight)" "fraction that can be split  C(N, d) / 2^N" [0, 1, 2, 3, 4] [0, 0.5, 1]
  , Color (col wire) (Pictures [Line [(gX g 2, grY g), (gX g 2, grY g + grH g)], Line [(grX g, gY g 0.5), (grX g + grW g, gY g 0.5)]])
  , Pictures [ let nns = [0, max 1 (jsRound (i2d d / 50)) .. 4 * d]
               in plotXY g [ i2d nn / i2d d | nn <- nns ] [ coverFrac nn d | nn <- nns ] (col c) (if d == dimD then 3 else 1.5)
             | (d, c) <- zip ds cols ]
  , legend (grX g + grW g - 130) (grY g + 20) [ (c, "d = " ++ show d ++ (if d == 3 then " (plane)" else "")) | (d, c) <- zip ds cols ]
  , Color (col ink) (circleAt cpx cpy 12)
  , say "current points (exhaustive)" (cpx + 10) (cpy - 8) 14 (col ink)
  , say "the larger d, the sharper the drop at N = 2d -> capacity = 2 points per weight" (grX g) (grY g + grH g + 56) 14 (col sub)
  -- ---- 下：2^N 通りの小さな絵
  , Pictures [ thumb m | m <- [0 .. 2 ^ n - 1], let y = ty + 4 + i2f (m `div` perRow) * (tw + 4), y < envH env ]
  , drawSelfTest env (wTest w) ]
  where
    n = length (wPts w)
    cf = coverCount n dimD
    side = sideOf env
    hm = hoverMask env w
    sp = if hm >= 0 && hm < V.length (wSep w) then wSep w V.! hm else Sep False 0 0 0
    gx = 30 + side + 70
    g = Graph gx 110 (envW env - gx - 40) (side - 30) 0 4 0 1.02
    ds = [1, 3, 10, 40, 200]
    cols = [sub, mag, sky, lime, amber]
    cpx = gX g (i2d n / i2d dimD)
    cpy = gY g (if n > 0 then i2d (wNSep w) / 2 ^^ n else 1)
    (perRow, tw, ty) = thumbLayout env n
    thumb m =
      let x = 30 + i2f (m `mod` perRow) * (tw + 4)
          y = ty + 4 + i2f (m `div` perRow) * (tw + 4)
          ok = m < V.length (wSep w) && spOk (wSep w V.! m)
      in Pictures $
           [ Color (if ok then col panel else makeColorI 12 26 28 255) (rectTL x y tw tw) ]
           ++ [ Color (colA 170 lime) (frameTL 1 x y tw tw) | ok ]
           ++ [ Color (colA (if ok then 255 else 80) (if bit m i then mag else lime)) (circleAt (x + 3 + f2 px * (tw - 6)) (y + 3 + (1 - f2 py) * (tw - 6)) (max 3 (tw / 9)))
              | (i, (px, py)) <- zip [0 ..] (wPts w) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        side = sideOf env
        u = d2 ((mx - 30) / side)
        v = 1 - d2 ((my - 90) / side)
        hit = foldl' (\d (i, (px, py)) -> if sqrt ((px - u) * (px - u) + (py - v) * (py - v)) < 0.04 then i else d) (-1) (zip [0 ..] (wPts w))
    in if u < 0 || u > 1 || v < 0 || v > 1 then w
       else if hit < 0 && length (wPts w) < 9 then analyze w { wPts = wPts w ++ [(u, v)], wDrag = -1 }
       else w { wDrag = hit }
  EventMotion pos | envDown env && wDrag w >= 0 ->
    let (mx, my) = toP5 env pos
        side = sideOf env
        p = (clampv (d2 ((mx - 30) / side)) 0 1, clampv (1 - d2 ((my - 90) / side)) 0 1)
    in analyze w { wPts = [ if i == wDrag w then p else q | (i, q) <- zip [0 ..] (wPts w) ] }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> randomPoints (min 9 (length (wPts w) + 1)) w
  EventKey (SpecialKey KeyDown) Down _ _ -> randomPoints (max 1 (length (wPts w) - 1)) w
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> let w' = w { wSeed = wSeed w + 1 } in randomPoints (max 1 (length (wPts w))) w'
    | c `elem` "lL" -> let n = length (wPts w)
                           m = i2d (max 1 (n - 1))
                       in analyze w { wPts = [ (0.1 + (0.8 * i2d i) / m, 0.3 + (0.4 * i2d i) / m) | i <- [0 .. n - 1] ] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. N = 4 で、分けられない 2 通りはどんな色分け？（XOR の形になっている？）
-- 2. N = 6（N/d = 2）で、ちょうど半分 32 / 64 になることを確かめる
-- 3. L で一直線に並べると何通り？ それは 1 次元の式 C(N, 2) = 2N と同じ？
-- 4. 点をドラッグして 3 点をそろえていくと、どこで数が減る？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("N = 4、d = 3：C(4, 3) = 14（16 通りのうち）", coverCount 4 3 == 14)
  , ("一般の位置のランダムな点 N = 1〜7：総当たりの数 = C(N, 3)（N = 7 で " ++ msg ++ "）", okAll)
  , ("2 つの数え方（向きの総当たり ／ 2 点を通る直線）が一致", okPair)
  , ("一直線上の点（1 次元、d = 2）：分けられる数 = C(N, 2) = 2N", line1)
  , ("C(2d, d) = 2^(2d−1)（ちょうど半分）d = 1〜20", half)
  , ("漸化式 C(N+1, d) = C(N, d) + C(N, d−1)、N ≤ d なら 2^N", rec) ]
  where
    sets = [ (n, ps) | s <- [1 .. 4], n <- [1 .. 7 :: Int]
                     , let ps = withSeed (900 + 10 * s + n) (mapM (const (do a <- rnd; b <- rnd; pure (a, b))) [1 .. n])
                     , not (hasCollinear ps) ]
    results = [ (n, countSeparable ps, countByPairs ps) | (n, ps) <- sets ]
    okAll = and [ toInteger c == coverCount n 3 | (n, c, _) <- results ]
    okPair = and [ p == c | (_, c, p) <- results ]
    msg = lastOr "" [ show c ++ " = " ++ show (coverCount 7 3) | (n, c, _) <- results, n == 7 ]
    line1 = and [ toInteger (countSeparable [ (0.1 + i2d i * 0.1, 0.5) | i <- [0 .. n - 1] ]) == coverCount n 2 | n <- [1 .. 8] ]
    half = and [ coverCount (2 * d) d == 2 ^ (2 * d - 1) && near (coverFrac (2 * d) d) 0.5 1e-12 | d <- [1 .. 20] ]
    rec = and [ coverCount (n + 1) d == coverCount n d + coverCount n (d - 1) && (n > d || coverCount n d == 2 ^ n) | n <- [1 .. 25], d <- [1 .. 25] ]

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

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

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
