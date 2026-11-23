-- ============================================
-- 58-03 複素数が出てくる ─ G(2) の擬スカラー I = e1e2 は I² = −1
-- 「スカラー ＋ I の何倍」という偶数次の元 z = x + yI は、複素数 x + yi とまったく同じ掛け算をする
-- ベクトル v に右から I を掛けると 90° 回る（vI）。左から掛けると逆向き（Iv = −vI）
-- 操作：左 v の先をドラッグ / I v に I を掛ける（4 回でもとにもどる）/ 右 z1・z2 の点をドラッグ / R 元にもどす
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

g2 :: Alg
g2 = makeAlgebra [1, 1]

ps :: MV
ps = blade g2 3 1                 -- 擬スカラー e12（名前 I はさけて ps と書く）

v0, z10, z20 :: P2
v0 = (2, 1)                       -- ★ 左のベクトル
z10 = (1.2, 0.6)                  -- ★ 右の 2 つの「複素数」（スカラー部, I の係数）
z20 = (0.5, 1.1)

unitPx :: Double
unitPx = 64

data World = World
  { wV, wZ1, wZ2 :: !P2
  , wDrag        :: !Int          -- 0 = v、1 = z1、2 = z2、−1 なし
  , wTurns       :: !Int
  , wTest        :: [(String, Bool)] }

vio, cya, sil, gold, pink :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)
pink = (255, 150, 200)

main :: IO ()
main = runSketch (sketch "58-03 Complex numbers appear" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World v0 z10 z20 (-1) 0 []

panel :: Env -> Int -> P2
panel env k = (realToFrac (envW env) * (if k == 0 then 0.26 else 0.72), realToFrac (envH env) * 0.63)

toPx :: Env -> Int -> P2 -> P2
toPx env k (x, y) = let (cx, cy) = panel env k in (cx + x * unitPx, cy - y * unitPx)

axesAt :: Env -> Int -> String -> String -> [Picture]
axesAt env k xl yl =
  [ Color (grayC 40) (circleS 1 (cx, cy) unitPx)
  , Color (grayC 70) (Pictures [lineS 1 (cx - r, cy) (cx + r, cy), lineS 1 (cx, cy - r) (cx, cy + r)])
  , say xl (cx + r - 30) (cy + 18) 14 (grayC 140), say yl (cx + 6) (cy - r + 12) 14 (grayC 140) ]
  where (cx, cy) = panel env k
        r = unitPx * 3

draw :: Env -> World -> Picture
draw env w = view env $
  -- 左：ベクトル × I = 90° 回転
  axesAt env 0 "e1" "e2"
  ++ concat [ let s = toPx env 0 (t2 (vecOf g2 cur)); al = if k == wTurns w `mod` 4 then 255 else 90; c = if k == 0 then vio else cya
              in [ arrowS (colA c al) (if k == wTurns w `mod` 4 then 4 else 2) o s
                 , say (getAt "" ["v", "vI", "vI^2", "vI^3"] k) (fst s + 6) (snd s - 6) 15 (rgbaC 220 220 220 al) ]
            | (k, cur) <- zip [0 .. 3] (iterate (\m -> gp g2 m ps) v) ]
  ++ [ Color (col3 pink) (dashed 1.5 5 5 o si), say "Iv = -vI" (fst si + 6) (snd si + 16) 15 (col3 pink) ]
  -- 右：偶数次の元 z = x + y I の掛け算 = 複素数の掛け算
  ++ axesAt env 1 "scalar" "I (e12)"
  ++ [ Color (col3 vio) (lineS 2 q0 q1), Color (col3 cya) (lineS 2 q0 q2), Color (col3 gold) (lineS 2 q0 qp)
     , Color (col3 vio) (discS q1 7), Color (col3 cya) (discS q2 7), Color (col3 gold) (discS qp 8)
     , say "z1" (fst q1 + 9) (snd q1 - 8) 15 (grayC 240), say "z2" (fst q2 + 9) (snd q2 - 8) 15 (grayC 240), say "z1 z2" (fst qp + 10) (snd qp - 8) 15 (grayC 240)
     -- 角の和を弧で
     , Color (colA gold 150) (polyline 1.5 False [ (fst q0 + 40 * cos t, snd q0 - 40 * sin t) | i <- [0 .. 40 :: Int], let t = ((a1 + a2) * fromIntegral i) / 40 ]) ]
  -- 文字
  ++ [ say ("left: drag the tip of v, I multiplies v by I (now vI^" ++ show (wTurns w `mod` 4) ++ ")   right: drag z1, z2   R: reset") 20 34 18 (col3 sil) ]
  ++ [ say s 20 (66 + 25 * fromIntegral k) 16 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ say "bridge between vectors and complex numbers: e1 v = v1 + v2 I (how e1 is 'turned' into v)" 20 (h - 62) 14 (grayC 150)
     , say "|z1 z2| = |z1||z2|, the angles add (yellow arc). z1 z2 = z2 z1" 20 (h - 40) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    h = realToFrac (envH env) :: Double
    t2 xs = (getAt 0 xs 0, getAt 0 xs 1)
    v = vec g2 [fst (wV w), snd (wV w)]
    o = toPx env 0 (0, 0)
    si = toPx env 0 (t2 (vecOf g2 (gp g2 ps v)))
    (z1x, z1y) = wZ1 w
    (z2x, z2y) = wZ2 w
    za = addM (scalar g2 z1x) (smul ps z1y)
    zb = addM (scalar g2 z2x) (smul ps z2y)
    zp = gp g2 za zb
    cx = (z1x * z2x - z1y * z2y, z1x * z2y + z1y * z2x)      -- 複素数の式
    q0 = toPx env 1 (0, 0)
    q1 = toPx env 1 (wZ1 w)
    q2 = toPx env 1 (wZ2 w)
    qp = toPx env 1 (zp U.! 0, zp U.! 3)
    a1 = atan2 z1y z1x
    a2 = atan2 z2y z2x
    rows =
      [ ("I = e1e2, I^2 = e1e2e1e2 = -e1e1e2e2 = " ++ str g2 (gp g2 ps ps) 2 ++ "  <- a 'number' with square -1, built from vectors", gold)
      , ("v = " ++ str g2 v 2 ++ "   vI = " ++ str g2 (gp g2 v ps) 2 ++ " (90 deg left)   Iv = " ++ str g2 (gp g2 ps v) 2 ++ " (90 deg right)", cya)
      , ("z1 = " ++ str g2 za 2 ++ "   z2 = " ++ str g2 zb 2 ++ "   geometric product z1 z2 = " ++ str g2 zp 3, gold)
      , ("complex formula (a + bi)(c + di) = (ac - bd) + (ad + bc)i = " ++ fmt 3 (fst cx) ++ (if snd cx < 0 then " - " else " + ") ++ fmt 3 (abs (snd cx)) ++ "i  -> the same!", sil) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        cands = [(0, 0, wV w), (1, 1, wZ1 w), (2, 1, wZ2 w)]
        pick (best, k) (i, pn, p) = let (sx, sy) = toPx env pn p; d = hypot (mx - sx) (my - sy) in if d < best then (d, i) else (best, k)
    in w { wDrag = snd (foldl' pick (28, -1) cands) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        (cx, cy) = panel env (if wDrag w == 0 then 0 else 1)
        p = (clampv (-3.4) 3.4 (jsRound (((mx - cx) / unitPx) * 20) / 20), clampv (-3.4) 3.4 (jsRound (((cy - my) / unitPx) * 20) / 20))
    in case wDrag w of
         0 -> w { wV = p }
         1 -> w { wZ1 = p }
         _ -> w { wZ2 = p }
  EventKey (Char c) Down _ _
    | c `elem` "iI" -> w { wTurns = wTurns w + 1 }
    | c `elem` "rR" -> world0 { wTest = wTest w }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 3 $ do
  rs <- mapM (\_ -> do
          a <- (\r -> r * 4 - 2) <$> nextR; b <- (\r -> r * 4 - 2) <$> nextR
          c <- (\r -> r * 4 - 2) <$> nextR; d <- (\r -> r * 4 - 2) <$> nextR
          let p = gp g2 (addM (scalar g2 a) (smul ps b)) (addM (scalar g2 c) (smul ps d))
              okM = near (p U.! 0) (a * c - b * d) 1e-12 && near (p U.! 3) (a * d + b * c) 1e-12 && near (p U.! 1) 0 1e-9 && near (p U.! 2) 0 1e-9
          v <- randVec g2
          let vi = gp g2 v ps
              (v1, v2) = (v U.! 1, v U.! 2)
              (w1, w2) = (vi U.! 1, vi U.! 2)
              okR = near (v1 * w1 + v2 * w2) 0 1e-12 && near (hypot w1 w2) (hypot v1 v2) 1e-12 && near (v1 * w2 - v2 * w1) (v1 * v1 + v2 * v2) 1e-12
              okA = nearM (gp g2 ps v) (smul vi (-1)) 1e-9
              okC = nearM (gp g2 (gp g2 vi ps) (gp g2 ps ps)) v 1e-9
          pure [okM, okR, okA, okC]) [1 .. 40 :: Int]
  let col k = all (\r -> getAt False r k) rs
      t = 0.83
      e = expB g2 (smul ps t) False
  pure
    [ ("I^2 = -1 (I = e1e2)", nearM (gp g2 ps ps) (scalar g2 (-1)) 1e-9)
    , ("product of even elements = complex product (ac - bd, ad + bc)", col 0)
    , ("vI is perpendicular to v, the same length, turned 90 deg left", col 1)
    , ("Iv = -vI (I anticommutes with vectors)", col 2)
    , ("vI^4 = v (one full turn in 4 steps)", col 3)
    , ("Euler's formula e^(I theta) = cos(theta) + I sin(theta) (from the exponential)", near (e U.! 0) (cos t) 1e-9 && near (e U.! 3) (sin t) 1e-9 && nearM (expB g2 (smul ps t) True) e 1e-12)
    , ("e1 v = v1 + v2 I (bridge between vectors and complex numbers)", nearM (gp g2 (blade g2 1 1) (vec g2 [0.3, -0.7])) (addM (scalar g2 0.3) (smul ps (-0.7))) 1e-9) ]

-- ---------- やってみよう ----------
-- 1. I を 4 回押して、v → vI → vI² → vI³ → v と 1 周することを確かめよう。vI² は何？
-- 2. 右で z1 を 1（スカラーだけ）にすると、z1 z2 はどうなる？ z1 = I なら？
-- 3. z1・z2 を単位円の上に置くと、積も単位円の上。角度はどうなっている？
-- 4. 複素数の i は「謎の数」ではなく、平面の向き付き面積 e1e2 だった。3 次元では I² は？（e123 で確かめよう）
-- 5. 3 次元の e12, e23, e13 も 2 乗すると −1。3 つの「虚数単位」は何になる？（07 と シリーズ 57 へ）

-- ==================================================
-- ここから下は「幾何代数の道具」（中身は読まなくても OK。消さないでね）
--   マルチベクトル = 2ⁿ 個の Double の配列（unboxed vector）。基底ブレードは「ビットマスク」で番号をつける
--   例（n = 3）：0 = 1、1 = e1、2 = e2、3 = e12、4 = e3、5 = e13、6 = e23、7 = e123（e31 = −e13）
--   幾何積：ブレード a と b を並べかえる回数（入れかえ 1 回で −1）× 同じ e_i が 2 つ並んだら e_i² を掛ける
--   計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
type MV = U.Vector Double

-- 代数：次元 n、基底の数 2ⁿ、各 e_i の 2 乗、次数、掛け算表（符号）、基底の名前
data Alg = Alg
  { aN      :: !Int
  , aNb     :: !Int
  , aMetric :: [Double]
  , aGrade  :: !(U.Vector Int)
  , aTbl    :: !(U.Vector Double)
  , aNames  :: [String] }

-- 基底 a のうしろに基底 b を並べ、標準の順（番号の小さい順）にするまでの入れかえ回数 → 奇数なら −1
reorderSign :: Int -> Int -> Double
reorderSign a0 b = go (a0 `shiftR` 1) 0
  where go 0 s = if odd s then -1 else 1
        go a s = go (a `shiftR` 1) (s + popCount (a .&. b))

-- 代数をつくる：metric = 各 e_i の 2 乗（+1 か −1）。[1,1,1] = G(3)、[-1,-1] = Cl(0,2)、[1,-1,-1,-1] = 時空の代数
makeAlgebra :: [Double] -> Alg
makeAlgebra metric = Alg n nb metric grade tbl names
  where
    n = length metric
    nb = 1 `shiftL` n
    grade = U.generate nb popCount
    tbl = U.generate (nb * nb) $ \k ->
      let (a, b) = k `divMod` nb
          c = a .&. b
      in foldl' (\s (i, m) -> if testBit c i then s * m else s) (reorderSign a b) (zip [0 ..] metric)   -- 同じ e_i どうし → e_i² = ±1
    names = [ if a == 0 then "1" else 'e' : concat [ show (i + 1) | i <- [0 .. n - 1], testBit a i ] | a <- [0 .. nb - 1] ]

-- 基底の名前を取りかえる
withNames :: [String] -> Alg -> Alg
withNames ns al = al { aNames = ns }

nameOf :: Alg -> Int -> String
nameOf al i = getAt "?" (aNames al) i

zeroM :: Alg -> MV
zeroM al = U.replicate (aNb al) 0

scalar :: Alg -> Double -> MV
scalar al s = zeroM al U.// [(0, s)]

-- 基底ブレード（係数 c）
blade :: Alg -> Int -> Double -> MV
blade al mask c = zeroM al U.// [(mask, c)]

-- ベクトル（足りない成分は 0）
vec :: Alg -> [Double] -> MV
vec al vs = zeroM al U.// [ (1 `shiftL` i, v) | (i, v) <- zip [0 .. aN al - 1] vs ]

vecOf :: Alg -> MV -> [Double]
vecOf al m = [ m U.! (1 `shiftL` i) | i <- [0 .. aN al - 1] ]

addM, subM :: MV -> MV -> MV
addM = U.zipWith (+)
subM = U.zipWith (-)

-- いくつも足す
addMs :: Alg -> [MV] -> MV
addMs al = foldl' addM (zeroM al)

smul :: MV -> Double -> MV
smul a s = U.map (* s) a

-- 積の共通部分：kind 0 = 幾何積、1 = 外積 ∧（共通の e_i がないときだけ）、2 = 左縮約 ⌋（a のブレードが b に含まれるときだけ）
prodK :: Alg -> Int -> MV -> MV -> MV
prodK al kind a b = U.accum (+) (zeroM al)
  [ (i `xor` j, U.unsafeIndex tbl (i * nb + j) * x * y)
  | i <- [0 .. nb - 1], let x = U.unsafeIndex a i, x /= 0
  , j <- [0 .. nb - 1], let y = U.unsafeIndex b j, y /= 0
  , kind /= 1 || i .&. j == 0
  , kind /= 2 || i .&. j == i ]
  where nb = aNb al
        tbl = aTbl al

gp, wedge, lc :: Alg -> MV -> MV -> MV
gp al = prodK al 0
wedge al = prodK al 1
lc al = prodK al 2

-- 3 つ以上の積（左から順に）
gps, wedges :: Alg -> [MV] -> MV
gps al ms = case ms of
  (m : rest) -> foldl' (gp al) m rest
  [] -> scalar al 1
wedges al ms = case ms of
  (m : rest) -> foldl' (wedge al) m rest
  [] -> scalar al 1

-- ベクトルどうしなら a·b
inner :: Alg -> MV -> MV -> MV
inner al a b = gradePart al (gp al a b) 0

-- スカラー部分
sp :: Alg -> MV -> MV -> Double
sp al a b = gp al a b U.! 0

gradePart :: Alg -> MV -> Int -> MV
gradePart al a k = U.imap (\i x -> if aGrade al U.! i == k then x else 0) a

gradeSize :: Alg -> MV -> Int -> Double
gradeSize al a k = sqrt (U.ifoldl' (\s i x -> if aGrade al U.! i == k then s + x * x else s) 0 a)

-- 逆順 ã（掛ける順を逆に）
rev :: Alg -> MV -> MV
rev al = U.imap (\i x -> let g = aGrade al U.! i in if odd ((g * (g - 1)) `div` 2) then -x else x)

-- 次数の反転（奇数次に −）
ginv :: Alg -> MV -> MV
ginv al = U.imap (\i x -> if odd (aGrade al U.! i) then -x else x)

normSq, norm :: Alg -> MV -> Double
normSq al a = gp al a (rev al a) U.! 0
norm al a = sqrt (abs (normSq al a))

unit :: Alg -> MV -> MV
unit al a = let n = norm al a in smul a (1 / (if n == 0 || isNaN n then 1 else n))

-- 逆元（ベクトル・ブレード・ベルソル用：a ã がスカラー）
inv :: Alg -> MV -> MV
inv al a = let r = rev al a in smul r (1 / (gp al a r U.! 0))

-- 擬スカラー（一番上のブレード）
pss :: Alg -> MV
pss al = blade al (aNb al - 1) 1

-- 双対 a* = a I⁻¹
dualOf, undual :: Alg -> MV -> MV
dualOf al a = gp al a (inv al (pss al))
undual al a = gp al a (pss al)

-- 交わり（向きと大きさは決めかた次第）
meet :: Alg -> MV -> MV -> MV
meet al a b = undual al (wedge al (dualOf al a) (dualOf al b))

-- R x R̃（回転）
sandwich :: Alg -> MV -> MV -> MV
sandwich al r x = gps al [r, x, rev al r]

-- −n x n⁻¹（法線 n の鏡で反射）
reflect :: Alg -> MV -> MV -> MV
reflect al nv x = smul (gps al [nv, x, inv al nv]) (-1)

isOdd :: Alg -> MV -> Bool
isOdd al v = o > e
  where (o, e) = U.ifoldl' (\(so, se) i x -> if odd (aGrade al U.! i) then (so + x * x, se) else (so, se + x * x)) (0, 0) v

-- ±V x V⁻¹（鏡 k 枚の積なら符号 (−1)^k）
applyVersor :: Alg -> MV -> MV -> MV
applyVersor al v x = smul (gps al [v, x, inv al v]) (if isOdd al v then -1 else 1)

-- 二重ベクトル B の指数関数 e^B：B² がスカラーなら閉じた式、そうでなければ級数（割って 2 乗をくり返す）
expB :: Alg -> MV -> Bool -> MV
expB al b forceSeries
  | not forceSeries && rest < 1e-12 =
      let s = U.head b2
          (c, k) | s < -1e-15 = let t = sqrt (-s) in (cos t, sin t / t)
                 | s > 1e-15 = let t = sqrt s in (cosh t, sinh t / t)
                 | otherwise = (1, 1)
          r = smul b k
      in r U.// [(0, U.head r + c)]
  | otherwise =
      let sz0 = U.foldl' (\m x -> max m (abs x)) 0 b
          halve (sz, q) = if sz > 0.25 && q < 40 then halve (sz / 2, q + 1) else q
          sq = halve (sz0, 0 :: Int)
          x = smul b (1 / (2 ^^ sq))
          series = snd (foldl' (\(term, sm) k -> let t' = smul (gp al term x) (1 / fromIntegral k) in (t', addM sm t')) (scalar al 1, scalar al 1) [1 .. 29 :: Int])
      in iterate (\m -> gp al m m) series !! sq
  where
    b2 = gp al b b
    rest = U.sum (U.map abs (U.drop 1 b2))

near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 2 つのマルチベクトルが同じか（tol の中）
nearM :: MV -> MV -> Double -> Bool
nearM a b tol = U.length a == U.length b && U.and (U.zipWith (\x y -> abs (x - y) <= tol) a b)

isZeroM :: MV -> Double -> Bool
isZeroM a tol = U.all (\x -> abs x <= tol) a

-- ランダムなマルチベクトル・ベクトル（成分は −1〜1）
randM, randVec :: Alg -> R MV
randM al = U.fromList <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aNb al]
randVec al = vec al <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aN al]

-- 文字にする：「1.20 + 0.50 e12」（記号は ASCII）
str :: Alg -> MV -> Int -> String
str al a d = if null s then "0" else s
  where
    s = foldl' piece "" (zip [0 ..] (U.toList a))
    piece acc (i, x)
      | abs x < 10 ^^ negate d / 2 = acc
      | otherwise =
          let v = fmt d (abs x)
              sg = if null acc then (if x < 0 then "-" else "") else (if x < 0 then " - " else " + ")
              body = if i > 0 then (if v == fmt d 1 then "" else v ++ " ") ++ nameOf al i else v
          in acc ++ sg ++ body

-- ---- 比べるための「ふつうの」計算（行列・外積・行列式）。ベクトルは [x, y, z] のリスト
dotv :: [Double] -> [Double] -> Double
dotv a b = foldl' (+) 0 (zipWith (*) a b)

crossv :: [Double] -> [Double] -> [Double]
crossv a b = [a1 * b2 - a2 * b1, a2 * b0 - a0 * b2, a0 * b1 - a1 * b0]
  where [a0, a1, a2] = take 3 (a ++ repeat 0)
        [b0, b1, b2] = take 3 (b ++ repeat 0)

lenv :: [Double] -> Double
lenv a = sqrt (dotv a a)

det3 :: [Double] -> [Double] -> [Double] -> Double
det3 a b c = dotv a (crossv b c)

matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = map (\r -> foldl' (+) 0 (zipWith (*) r v)) m

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul p q = [ [ foldl' (+) 0 (zipWith (*) r col) | col <- transpose q ] | r <- p ]

rotMat2 :: Double -> [[Double]]
rotMat2 t = [[cos t, -(sin t)], [sin t, cos t]]

-- 軸 ax（単位ベクトル）まわりに角 t 回す行列
rodrigues :: [Double] -> Double -> [[Double]]
rodrigues ax t =
  [ [c + x * x * k, x * y * k - z * s, x * z * k + y * s]
  , [y * x * k + z * s, c + y * y * k, y * z * k - x * s]
  , [z * x * k - y * s, z * y * k + x * s, c + z * z * k] ]
  where [x, y, z] = take 3 (ax ++ repeat 0)
        c = cos t
        s = sin t
        k = 1 - c

nearArr :: [Double] -> [Double] -> Double -> Bool
nearArr a b tol = length a == length b && and (zipWith (\x y -> abs (x - y) <= tol) a b)

-- ---- 種つきの乱数（自己テストで毎回同じ数を使う。p5.js 版の rnd と同じ mulberry32）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

nextRng :: Rng -> (Double, Rng)
nextRng (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に使う計算（状態を手で受けわたさなくてよいように）
newtype R a = R (Rng -> (a, Rng))

instance Functor R where fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where R g >>= k = R (\s -> let (a, s1) = g s; R h = k a in h s1)

nextR :: R Double
nextR = R nextRng

runRng :: R a -> Rng -> (a, Rng)
runRng (R g) = g

evalRng :: Word32 -> R a -> a
evalRng sd m = fst (runRng m (seedRng sd))

-- ---- 数の道具
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % と同じ（符号は割られる数と同じ）
jsMod :: Double -> Double -> Double
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Integer)

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Double
jsRound v = if isNaN v || isInfinite v then 0 else fromIntegral (floor (v + 0.5) :: Integer)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . clampv (-1e6) 1e6 . fin 0

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 1.2e-5 のような書き方（toExponential(1)）
fmtE :: Double -> String
fmtE v = showEFloat (Just 1) (fin 0 v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
mouseP :: Env -> (Double, Double)
mouseP env = let (mx, my) = envMouse env in (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

-- gloss のイベントの位置 → p5.js の座標
evP :: Env -> (Float, Float) -> (Double, Double)
evP env (mx, my) = (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- [r, g, b] の色（透明度 a）
colA :: (Int, Int, Int) -> Int -> Color
colA (r, g, b) a = rgbaC r g b a

col3 :: (Int, Int, Int) -> Color
col3 (r, g, b) = rgbC r g b

-- 左下が (x, y) の文字
say :: String -> Double -> Double -> Float -> Color -> Picture
say s x y size c = Translate (f2 x) (f2 y) (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Double -> Double -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Double
textW s size = fromIntegral (length s) * realToFrac size * 0.72

pt2 :: (Double, Double) -> (Float, Float)
pt2 (x, y) = (f2 x, f2 y)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Double -> Double -> Double -> Double -> Picture
rectTL x y w h = Polygon [pt2 (x, y), pt2 (x + w, y), pt2 (x + w, y + h), pt2 (x, y + h)]

-- 線（太さ wt）
lineS :: Float -> (Double, Double) -> (Double, Double) -> Picture
lineS wt a b = if wt <= 1.2 then Line [pt2 a, pt2 b] else thickLine wt (pt2 a) (pt2 b)

-- 点線（dash：線の長さ、gap：すき間）
dashed :: Float -> Double -> Double -> (Double, Double) -> (Double, Double) -> Picture
dashed wt dash gap (x1, y1) (x2, y2) = Pictures
  [ lineS wt (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + dash), y1 + uy * min l (s + dash)) | s <- takeWhile (< l) [0, dash + gap ..] ]
  where l = min 5000 (fin 0 (hypot (x2 - x1) (y2 - y1)))
        ux = if l > 0 then (x2 - x1) / l else 0
        uy = if l > 0 then (y2 - y1) / l else 0

-- 点が 2 つ以上あるときだけ折れ線を描く（closed なら閉じる）
polyline :: Float -> Bool -> [(Double, Double)] -> Picture
polyline wt closed pts = case pts of
  (p0 : _ : _) -> let ps = map pt2 (if closed then pts ++ [p0] else pts) in if wt <= 1.2 then Line ps else thickPath wt ps
  _ -> Blank

-- 閉じた形を塗る（へこんだ形でも大丈夫なように、重心からの三角形で塗る）
fillPoly :: [(Double, Double)] -> Picture
fillPoly pts = case pts of
  (_ : _ : _ : _) -> Pictures [ Polygon [pt2 c, pt2 a, pt2 b] | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]
  _ -> Blank
  where n = fromIntegral (length pts)
        c = (sum (map fst pts) / n, sum (map snd pts) / n)

-- 最初の点から扇のように塗る（beginShape の扇形・星形の形）
fanFill :: [(Double, Double)] -> Picture
fanFill pts = case pts of
  (p0 : rest@(_ : _ : _)) -> Pictures [ Polygon [pt2 p0, pt2 a, pt2 b] | (a, b) <- zip rest (drop 1 rest) ]
  _ -> Blank

-- 円（中心・半径）の枠と塗り
circleS :: Float -> (Double, Double) -> Double -> Picture
circleS wt (x, y) r = Translate (f2 x) (f2 y) (if wt <= 1.2 then Circle (f2 r) else ThickCircle (f2 r) wt)

discS :: (Double, Double) -> Double -> Picture
discS (x, y) r = Translate (f2 x) (f2 y) (circleSolid (f2 r))

-- 矢印（画面の座標）：線の太さ sw
arrowS :: Color -> Float -> (Double, Double) -> (Double, Double) -> Picture
arrowS c sw (x1, y1) (x2, y2) = Color c $ Pictures $
  lineS sw (x1, y1) (x2, y2) :
  [ Polygon [pt2 (x2, y2), pt2 (x2 - h * cos (a - 0.4), y2 - h * sin (a - 0.4)), pt2 (x2 - h * cos (a + 0.4), y2 - h * sin (a + 0.4))]
  | hypot (x2 - x1) (y2 - y1) >= 2 ]
  where a = atan2 (y2 - y1) (x2 - x1)
        h = 8 + realToFrac sw * 2

-- ---- 自己テストの道具
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
drawSelfTest env rs = say (selfTestLine rs) (realToFrac (envW env) - 200) (realToFrac (envH env) - 12) 14 (if all snd rs then rgbC 150 220 200 else rgbC 255 100 110)
