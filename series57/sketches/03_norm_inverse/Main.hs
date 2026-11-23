-- ============================================
-- 57-03 ノルムと逆元・単位四元数 ─ |pq| = |p||q|、q⁻¹ = q*/|q|²
-- 長さ（ノルム）は掛け算と両立する。だから 0 でない四元数には必ず逆元がある（割り算ができる）
-- |q| = 1 の四元数（単位四元数）は 4 次元の球面 S³ をつくる。(x, y, z) を 3D に、4 つ目の w を色にして描く
-- 操作：R 新しい p, q / ↑↓ w の切り口 / M 全部の点に u を掛け続ける / E オイラーの 4 平方恒等式の数を取りかえ / スペース 回す / ← → 向き
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortOn)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 1600                            -- ★ S³ の上の点の数

uStep :: Q
uStep = qAxisAngle (v3Unit (Vec 1 1 0)) 0.01   -- ★ M で掛け続ける単位四元数（少しだけ回す）

data World = World
  { wPts      :: ![Q]
  , wP, wQ    :: !Q
  , wIntA, wIntB :: ![Int]             -- オイラーの恒等式の整数
  , wCut      :: !Double
  , wMulOn    :: !Bool
  , wViewAng  :: !Double
  , wSpinning :: !Bool
  , wTest     :: [(String, Bool)] }

world0 :: World
world0 = World pts0 (Q 1 0 0 0) (Q 1 0 0 0) [1, 1, 0, 0] [1, 1, 1, 0] 0.5 False 0 True []
  where pts0 = evalGen 31 (replicateM nPts qRandom)

main :: IO ()
main = runSketch (sketch "57-03 Norm, inverse and unit quaternions" world0)
  { skBack  = rgbC 15 18 34
  , skStart = \_ -> do
      printSelfTest selfTest
      w1 <- newPQ world0
      w2 <- newInts w1
      pure w2 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

newPQ :: World -> IO World
newPQ w = do
  let one = Q <$> r <*> r <*> r <*> r
      r = (\v -> jsRound ((v * 6 - 3) * 10) / 10) <$> randomRIO (0, 1 :: Double)
  p <- one
  q <- one
  pure w { wP = p, wQ = q }

newInts :: World -> IO World
newInts w = do
  let one = replicateM 4 ((\v -> floor (v * 19) - 9) <$> randomRIO (0, 1 :: Double))
  a <- one
  b <- one
  pure w { wIntA = a, wIntB = b }

-- w = −1（青）→ 0（白）→ +1（金）
wColor :: Double -> Double -> Color
wColor w a = if w < 0 then rgbD (255 + w * 175) (255 + w * 105) (255 + w * 10) a
             else rgbD 255 (255 - w * 55) (255 - w * 175) a

step :: World -> World
step w0' =
  let w1 = if wSpinning w0' then w0' { wViewAng = wViewAng w0' + 0.004 } else w0'
  in if wMulOn w1 then w1 { wPts = forceList (map (qMul uStep) (wPts w1)) } else w1   -- 単位四元数を掛けても S³ の上にのこる

forceList :: [Q] -> [Q]
forceList xs = foldl' (\a x -> x `seq` a) () xs `seq` xs

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- S³ を 3D（x, y, z）と色（w）で
  [ let (cx, cy) = projXY (Vec 0 0 0) vw in Color (rgbaC 255 200 80 90) (circleLine 1 cx cy (f2 (rr * vs))) ]
  ++ [ let (sx, sy) = projXY (qRot vq (Vec x y z)) vw
           on = abs (x0 - wCut w) < 0.04
       in Color (wColor x0 (if on then 255 else 150)) (at (sx, sy) (disc (if on then 3.5 else 2)))
     | Q x0 x y z <- wPts w ]
  -- ---- 左：p, q のノルムと逆元
  ++ [ say ("p = " ++ qStr 2 p) tx ty 16 (grayC 240)
     , say ("q = " ++ qStr 2 q) tx (ty + 24) 16 (grayC 240)
     , say ("|p| = " ++ fmt 5 (qNorm p) ++ "   |q| = " ++ fmt 5 (qNorm q)) tx (ty + 56) 15 (grayC 210)
     , say ("|pq| = " ++ fmt 5 (qNorm prod)) tx (ty + 80) 15 (rgbC 130 230 150)
     , say ("|p||q| = " ++ fmt 5 (qNorm p * qNorm q)) tx (ty + 102) 15 (rgbC 130 230 150)
     , say ("q* = " ++ qStr 2 (qConj q)) tx (ty + 140) 15 (grayC 210)
     , say "q^-1 = q*/|q|^2" tx (ty + 164) 15 (grayC 210)
     , say ("     = " ++ qStr 4 qi) tx (ty + 186) 15 (grayC 210)
     , say ("q q^-1 = " ++ qStr 4 (qMul q qi)) tx (ty + 210) 15 (grayC 210)
     , say ("q^-1 q = " ++ qStr 4 (qMul qi q)) tx (ty + 234) 15 (grayC 210)
     , sayBox "q q* = q0^2 + q1^2 + q2^2 + q3^2 = |q|^2 (a real number), so dividing by |q|^2 gives the inverse"
              tx (ty + 250) 300 14 (grayC 170) ]
  -- ---- 右：オイラーの 4 平方恒等式（整数の四元数の積から）
  ++ [ say "Euler's four-square identity (1748)" rx ry 16 (rgbC 255 210 110)
     , say ("(" ++ intercalate " + " (map sqStr ia) ++ ")") rx (ry + 30) 15 (grayC 220)
     , say ("x (" ++ intercalate " + " (map sqStr ib) ++ ")") rx (ry + 54) 15 (grayC 220)
     , say ("= " ++ show sa ++ " x " ++ show sb ++ " = " ++ show (sa * sb)) rx (ry + 78) 15 (grayC 220)
     , say ("= " ++ intercalate " + " (map sqStr ic)) rx (ry + 110) 15 (rgbC 130 230 150)
     , say ("= " ++ show scc ++ (if scc == sa * sb then "   OK, equal" else "   NG")) rx (ry + 134) 15 (rgbC 130 230 150)
     , sayBox "The 4 numbers on the right are the 4 components of the integer quaternion a x b: |ab|^2 = |a|^2 |b|^2 in integers. A product of two sums of 4 squares is again a sum of 4 squares."
              rx (ry + 150) (ww - rx - 20) 14 (grayC 170)
     , say (show nPts ++ " points on S3: max ||x| - 1| = " ++ expo 1 maxN) rx (ry + 290) 15 (grayC 200)
     , sayBox ("Gold = the slice w ~ " ++ fmt 2 (wCut w) ++ " (a sphere of radius sqrt(1 - w^2) = " ++ fmt 3 rr ++ ")")
              rx (ry + 300) (ww - rx - 20) 15 (grayC 200)
     , say "Norm, inverse, unit quaternions   |pq| = |p||q|,  q^-1 = q*/|q|^2,  all |q| = 1 form the 3-sphere S3" 20 36 20 (grayC 240)
     , say "R new p, q   Up/Down: w slice   M: keep multiplying all points by a unit quaternion   E new integers   Space: spin on/off" 20 62 15 (grayC 175)
     , say "Point position = (x, y, z), colour = w (blue -1 -> white 0 -> gold +1). Fixing w, the rest (x, y, z) is a sphere" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    vs = realToFrac (min ww hh) * 0.27
    vw = View2 (realToFrac ww * 0.42) (realToFrac hh * 0.56) vs 6
    vq = qMul (qAxisAngle (Vec 1 0 0) 0.3) (qAxisAngle (Vec 0 1 0) (wViewAng w))
    rr = sqrt (max 0 (1 - wCut w * wCut w))
    maxN = maximum (0 : [ abs (qNorm x - 1) | x <- wPts w ])
    p = wP w; q = wQ w
    tx = 24; ty = 130
    prod = qMul p q
    qi = qInv q
    ia = wIntA w; ib = wIntB w
    toQ xs = let g = fromIntegral . getAt 0 xs in Q (g 0) (g 1) (g 2) (g 3)
    ic = [ round v :: Int | v <- qList (qMul (toQ ia) (toQ ib)) ]
    sa = sum (map (^ (2 :: Int)) ia); sb = sum (map (^ (2 :: Int)) ib); scc = sum (map (^ (2 :: Int)) ic)
    rx = ww * 0.72; ry = 130

sqStr :: Int -> String
sqStr v = if v < 0 then "(-" ++ show (negate v) ++ ")^2" else show v ++ "^2"

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> newPQ w
    | c `elem` "eE" -> newInts w
    | c `elem` "mM" -> pure w { wMulOn = not (wMulOn w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpinning = not (wSpinning w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wCut = min 0.98 (wCut w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wCut = max (-0.98) (wCut w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wViewAng = wViewAng w + 0.2 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wViewAng = wViewAng w - 0.2 }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("|pq| = |p||q| (200 pairs, relative 1e-9)", e1 < 1e-9)
  , ("q q^-1 = q^-1 q = 1", e2 < 1e-12)
  , ("(pq)^-1 = q^-1 p^-1", e3 < 1e-12)
  , ("q q* = |q|^2 (a real number)", e4 < 1e-12)
  , ("a product of unit quaternions is a unit quaternion (S3 is closed under multiplication)", e5 < 1e-12)
  , ("Euler's four-square identity: (sum a_i^2)(sum b_i^2) = sum c_i^2 exactly for 500 integer pairs", ok4) ]
  where
    one = Q 1 0 0 0
    (trials, ints) = evalGen 30 $ do
      ts <- replicateM 200 $ do
        p <- qScl <$> qRandom <*> ((\r -> 0.1 + 3 * r) <$> rndG)
        q <- qScl <$> qRandom <*> ((\r -> 0.1 + 3 * r) <$> rndG)
        u <- qRandom
        w <- qRandom
        pure (p, q, u, w)
      is <- replicateM 500 $ do
        let int = (\r -> fromIntegral (floor (r * 41) :: Int) - 20) <$> rndG
        a <- Q <$> int <*> int <*> int <*> int
        b <- Q <$> int <*> int <*> int <*> int
        pure (a, b)
      pure (ts, is)
    errs = [ ( abs (qNorm (qMul p q) / (qNorm p * qNorm q) - 1)
             , max (qDist (qMul q (qInv q)) one) (qDist (qMul (qInv q) q) one)
             , qDist (qInv (qMul p q)) (qMul (qInv q) (qInv p)) / qNorm (qInv (qMul p q))
             , qDist (qMul q (qConj q)) (Q (qDot q q) 0 0 0) / qDot q q
             , abs (qNorm (qMul u w) - 1) )
           | (p, q, u, w) <- trials ]
    e1 = maximum (0 : [ x | (x, _, _, _, _) <- errs ])
    e2 = maximum (0 : [ x | (_, x, _, _, _) <- errs ])
    e3 = maximum (0 : [ x | (_, _, x, _, _) <- errs ])
    e4 = maximum (0 : [ x | (_, _, _, x, _) <- errs ])
    e5 = maximum (0 : [ x | (_, _, _, _, x) <- errs ])
    sq q = sum (map (\v -> v * v) (qList q))
    ok4 = and [ sq a * sq b == sq c && all (\v -> v == fromIntegral (round v :: Integer)) (qList c)
              | (a, b) <- ints, let c = qMul a b ]

-- ---------- やってみよう ----------
-- 1. R を何回か押して、|pq| と |p||q| が毎回一致することを確かめよう
-- 2. q = (0, 0, 0, 0) なら逆元は？ 0 以外ならいつでも逆元がある → 四元数は「割り算のできる数」（斜体）
-- 3. ↑↓ で w の切り口を動かそう。w = ±1 に近づくと金色の球はどうなる？（S³ は球面を w 方向に積み重ねたもの）
-- 4. M で全部の点に u を掛け続けると、点は S³ の上をすべる。色（w）も変わる？ |x| は？
-- 5. E で整数を変えよう。2 = 1² + 1² + 0² + 0²、3 = 1² + 1² + 1² + 0² から、6 を 4 つの平方数の和で表そう

-- ==================================================
-- ここから下は「四元数の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）。自己テストで使う
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を引きながら計算する小さなモナド（JavaScript と同じく左から順に引く）
newtype Gen a = Gen { runGen :: Rng -> (a, Rng) }

instance Functor Gen where
  fmap f (Gen g) = Gen (\r -> case g r of (a, r1) -> (f a, r1))

instance Applicative Gen where
  pure a = Gen (\r -> (a, r))
  Gen f <*> Gen g = Gen (\r -> case f r of
                                 (h, !r1) -> case g r1 of (a, !r2) -> (h a, r2))

instance Monad Gen where
  Gen g >>= k = Gen (\r -> case g r of (a, !r1) -> runGen (k a) r1)

rndG, rndNormalG :: Gen Double
rndG = Gen rnd
rndNormalG = Gen rndNormal

-- 種 s から始めて m を計算する
evalGen :: Word32 -> Gen a -> a
evalGen s m = fst (runGen m (seedRng s))

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標に
mouseP5 :: Env -> (Double, Double)
mouseP5 env = let (x, y) = envMouse env in (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 0〜255 の小数で色を作る（明るさを掛けた色など）
rgbD :: Double -> Double -> Double -> Double -> Color
rgbD r g b a = makeColor (c r) (c g) (c b) (c a)
  where c v = f2 (clampv (v / 255) 0 1)

-- 左下が (x, y) の文字（y は文字の下の線 = p5.js の BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央（textAlign(CENTER, CENTER)）
sayCC :: String -> Float -> Float -> Float -> Color -> Picture
sayCC s x y size c = sayC s x (y + size * 0.36) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 w の箱に折り返して書く（p5.js の text(s, x, y, w, h)。(x, y) は箱の左上）
sayBox :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayBox s x y w size c = Pictures [ say ln x (y + size + fromIntegral k * size * 1.25) size c | (k, ln) <- zip [0 :: Int ..] (wrapWords (words s)) ]
  where
    wrapWords [] = []
    wrapWords (h : t) = go h t
    go cur [] = [cur]
    go cur (x1 : xs)
      | textW (cur ++ " " ++ x1) size <= w = go (cur ++ " " ++ x1) xs
      | otherwise = cur : go x1 xs

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 中心 (x, y)、半径 r の円の線（太さ wt）
circleLine :: Float -> Float -> Float -> Float -> Picture
circleLine wt x y r = Translate x y (ThickCircle r wt)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 数の道具
-- Math.hypot（JavaScript と同じ計算：最大値で割ってカハンの足し算）
hypotL :: [Double] -> Double
hypotL xs =
  let as = map abs xs
      m = maximum (0 : as)
      go (!s, !c) v = let n = v / m
                          summand = n * n - c
                          pre = s + summand
                      in (pre, (pre - s) - summand)
  in if any isInfinite as then 1 / 0 else if any isNaN as then 0 / 0 else if m == 0 then 0
     else sqrt (fst (foldl' go (0, 0) as)) * m

hypot3 :: Double -> Double -> Double -> Double
hypot3 a b c = hypotL [a, b, c]

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- JavaScript の % （0 の方へ切り捨てた余り）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (fin 0 v + 0.5) :: Integer)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v0 = let v = fin 0 v0
               s = showFFloat (Just n) v ""
           in if all (`elem` "-0.") s then dropWhile (== '-') s else s

-- 指数で書く（toExponential(n)、例 1.2e-16）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = fmt n 0 ++ "e+0"
  | otherwise = let s = showEFloat (Just n) v ""
                    (m, e) = break (== 'e') s
                    e' = drop 1 e
                in m ++ "e" ++ (if take 1 e' == "-" then e' else '+' : e')
  where v = fin 0 v0
-- ---- 四元数 Q w x y z = w + xi + yj + zk
data Q = Q !Double !Double !Double !Double deriving (Eq, Show)

qMul :: Q -> Q -> Q                    -- ハミルトン積（i² = j² = k² = ijk = −1）
qMul (Q a0 a1 a2 a3) (Q b0 b1 b2 b3) =
  Q (a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3)
    (a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2)
    (a0 * b2 - a1 * b3 + a2 * b0 + a3 * b1)
    (a0 * b3 + a1 * b2 - a2 * b1 + a3 * b0)

qConj :: Q -> Q                        -- 共役 q*
qConj (Q w x y z) = Q w (-x) (-y) (-z)

qAdd, qSub :: Q -> Q -> Q
qAdd (Q a b c d) (Q e f g h) = Q (a + e) (b + f) (c + g) (d + h)
qSub (Q a b c d) (Q e f g h) = Q (a - e) (b - f) (c - g) (d - h)

qScl :: Q -> Double -> Q
qScl (Q a b c d) s = Q (a * s) (b * s) (c * s) (d * s)

qDot :: Q -> Q -> Double
qDot (Q a b c d) (Q e f g h) = a * e + b * f + c * g + d * h

qNorm :: Q -> Double                   -- |q|
qNorm a = sqrt (qDot a a)

qUnit :: Q -> Q
qUnit a = let n = qNorm a in if n > 1e-300 then qScl a (1 / n) else Q 1 0 0 0

qInv :: Q -> Q                         -- q⁻¹ = q*/|q|²
qInv a = let n2 = qDot a a in if n2 > 1e-300 then qScl (qConj a) (1 / n2) else Q 0 0 0 0

qDist :: Q -> Q -> Double
qDist a b = qNorm (qSub a b)

qList :: Q -> [Double]
qList (Q w x y z) = [w, x, y, z]

qAt :: Q -> Int -> Double              -- 成分 0〜3
qAt q i = getAt 0 (qList q) i

qAxisAngle :: Vec -> Double -> Q       -- 軸 n まわりに角度 th の回転：cos(th/2) + sin(th/2) n
qAxisAngle n th = let Vec ux uy uz = v3Unit n
                      s = sin (th / 2)
                  in Q (cos (th / 2)) (ux * s) (uy * s) (uz * s)

qRot :: Q -> Vec -> Vec                -- v を q v q* で回す（v は 3 次元ベクトル → 純四元数）
qRot q (Vec x y z) = let Q _ a b c = qMul (qMul q (Q 0 x y z)) (qConj q) in Vec a b c

qToMat :: Q -> Mat                     -- 同じ q から作った 3×3 回転行列（単位四元数のとき）
qToMat (Q w x y z) =
  Mat (Vec (1 - 2 * (y * y + z * z)) (2 * (x * y - w * z)) (2 * (x * z + w * y)))
      (Vec (2 * (x * y + w * z)) (1 - 2 * (x * x + z * z)) (2 * (y * z - w * x)))
      (Vec (2 * (x * z - w * y)) (2 * (y * z + w * x)) (1 - 2 * (x * x + y * y)))

qExp :: Q -> Q                         -- exp(w + v) = e^w (cos|v| + sin|v| v/|v|)
qExp (Q a0 a1 a2 a3) =
  let vn = hypot3 a1 a2 a3
      ew = exp a0
      s = if vn > 1e-12 then sin vn / vn else 1 - (vn * vn) / 6
  in Q (ew * cos vn) (ew * s * a1) (ew * s * a2) (ew * s * a3)

qLog :: Q -> Q                         -- log q = ln|q| + acos(w/|q|) v/|v|
qLog a@(Q a0 a1 a2 a3) =
  let n = qNorm a
      vn = hypot3 a1 a2 a3
      th = atan2 vn a0
      s = if vn > 1e-12 then th / vn else 1 / max n 1e-300
  in Q (log (max n 1e-300)) (s * a1) (s * a2) (s * a3)

qSlerp :: Q -> Q -> Double -> Bool -> Q   -- 球面線形補間。shortest なら −b と近い方の道
qSlerp a b0 t shortest =
  let d0 = qDot a b0
      (b, d1) = if shortest && d0 < 0 then (qScl b0 (-1), -d0) else (b0, d0)
      d = clampv d1 (-1) 1
      om = acos d
      so = sin om
  in if so < 1e-9 then qUnit (qAdd (qScl a (1 - t)) (qScl b t))       -- ほぼ同じ向き → lerp で十分
     else qAdd (qScl a (sin ((1 - t) * om) / so)) (qScl b (sin (t * om) / so))

qNlerp :: Q -> Q -> Double -> Bool -> Q   -- 正規化した線形補間
qNlerp a b0 t shortest =
  let b = if shortest && qDot a b0 < 0 then qScl b0 (-1) else b0
  in qUnit (qAdd (qScl a (1 - t)) (qScl b t))

qAngle :: Q -> Q -> Double             -- 2 つの姿勢の間の回転角（0〜π）
qAngle a b = let Q d0 d1 d2 d3 = qMul (qConj (qUnit a)) (qUnit b)
             in 2 * atan2 (hypot3 d1 d2 d3) (abs d0)

qRandom :: Gen Q                       -- 一様にランダムな単位四元数（S³ の上の点）
qRandom = do
  a <- rndNormalG; b <- rndNormalG; c <- rndNormalG; d <- rndNormalG
  pure (qUnit (Q a b c d))

qStr :: Int -> Q -> String             -- (w, x, y, z) を小数点以下 dg 桁で
qStr dg q = "(" ++ intercalate ", " [ fmt dg (if abs v < 0.5 * 10 ** negate (fromIntegral dg) then 0 else v) | v <- qList q ] ++ ")"

-- ---- 3 次元ベクトルと 3×3 行列（行列は 3 つの行）
data Vec = Vec !Double !Double !Double deriving (Eq, Show)
data Mat = Mat !Vec !Vec !Vec deriving (Eq, Show)

v3Dot :: Vec -> Vec -> Double
v3Dot (Vec a b c) (Vec d e f) = a * d + b * e + c * f

v3Cross :: Vec -> Vec -> Vec
v3Cross (Vec a0 a1 a2) (Vec b0 b1 b2) = Vec (a1 * b2 - a2 * b1) (a2 * b0 - a0 * b2) (a0 * b1 - a1 * b0)

v3Len :: Vec -> Double
v3Len (Vec a b c) = hypot3 a b c

v3Unit :: Vec -> Vec
v3Unit a@(Vec x y z) = let n = v3Len a in if n > 1e-300 then Vec (x / n) (y / n) (z / n) else Vec 0 0 1

v3Add, v3Sub :: Vec -> Vec -> Vec
v3Add (Vec a b c) (Vec d e f) = Vec (a + d) (b + e) (c + f)
v3Sub (Vec a b c) (Vec d e f) = Vec (a - d) (b - e) (c - f)

v3Scl :: Vec -> Double -> Vec
v3Scl (Vec a b c) s = Vec (a * s) (b * s) (c * s)

v3Dist :: Vec -> Vec -> Double
v3Dist (Vec a b c) (Vec d e f) = hypot3 (a - d) (b - e) (c - f)

v3List :: Vec -> [Double]
v3List (Vec a b c) = [a, b, c]

matVec :: Mat -> Vec -> Vec
matVec (Mat r0 r1 r2) v = Vec (v3Dot r0 v) (v3Dot r1 v) (v3Dot r2 v)

matT :: Mat -> Mat
matT (Mat (Vec a b c) (Vec d e f) (Vec g h i)) = Mat (Vec a d g) (Vec b e h) (Vec c f i)

matMul :: Mat -> Mat -> Mat
matMul (Mat r0 r1 r2) b = let Mat c0 c1 c2 = matT b
                              row r = Vec (v3Dot r c0) (v3Dot r c1) (v3Dot r c2)
                          in Mat (row r0) (row r1) (row r2)

matDet :: Mat -> Double
matDet (Mat r0 r1 r2) = v3Dot r0 (v3Cross r1 r2)

matList :: Mat -> [[Double]]
matList (Mat a b c) = map v3List [a, b, c]

matDiff :: Mat -> Mat -> Double        -- 成分の差の最大
matDiff a b = maximum (0 : zipWith (\x y -> abs (x - y)) (concat (matList a)) (concat (matList b)))

matOrthoErr :: Mat -> Double           -- |MᵀM − I| の最大成分（回転行列なら 0）
matOrthoErr a = matDiff (matMul (matT a) a) matI

matI :: Mat
matI = Mat (Vec 1 0 0) (Vec 0 1 0) (Vec 0 0 1)
-- ---- 3D を 2D の画面に描く道具（透視投影）。View2 中心 x y・拡大 s・目までの距離 d
--   数学の向き：x 右・y 上・z 手前。回転は全部 q v q* で計算する
data View2 = View2 !Double !Double !Double !Double

-- 画面の点 (x, y)（p5.js の座標）と奥行き z
projP :: Vec -> View2 -> (Float, Float, Double)
projP (Vec x y z) (View2 vx vy vs vd) =
  let f = vd / max 0.2 (vd - z)
  in (f2 (vx + x * vs * f), f2 (vy - y * vs * f), z)

projXY :: Vec -> View2 -> (Float, Float)
projXY v vw = let (x, y, _) = projP v vw in (x, y)

-- 箱を組み合わせた物体を、回転の四元数 q で回して描く（奥の面から順にぬる＝画家のアルゴリズム）
--   p5.js 版と同じく箱は 1 本の枠線つき。alpha は 0〜255（255 = 不透明）
drawParts2D :: [Part] -> Q -> View2 -> Double -> Picture
drawParts2D parts q vw@(View2 _ _ _ vd) alpha = Pictures (concatMap face (sortOn (\(cz, _, _, _) -> cz) faces))
  where
    faces = [ (cz, fp, 0.42 + 0.58 * max 0 (v3Dot nw light2), col)
            | Part (Vec cx cy cz0) (Vec hx hy hz) col <- parts
            , let pts = [ qRot q (Vec (cx + x * hx) (cy + y * hy) (cz0 + z * hz)) | Vec x y z <- boxCorners ]
            , (n, idx) <- boxFaces
            , let nw = qRot q n
                  fp = map (getAt (Vec 0 0 0) pts) idx
                  cz = sum [ z | Vec _ _ z <- fp ] / 4
                  ctr = foldl' (\acc p -> v3Add acc (v3Scl p 0.25)) (Vec 0 0 0) fp
            , v3Dot nw (v3Sub (Vec 0 0 vd) ctr) > 0 ]            -- 裏を向いた面は描かない
    face (_, fp, b, (r, g, bl)) =
      let ps = map (`projXY` vw) fp
      in [ Color (rgbD (r * b) (g * b) (bl * b) alpha) (Polygon ps)
         , Color (rgbD 20 24 40 (if alpha >= 255 then 120 else alpha * 0.5)) (poly ps) ]

boxFaces :: [(Vec, [Int])]
boxFaces = [ (Vec 1 0 0, [1, 2, 6, 5]), (Vec (-1) 0 0, [0, 4, 7, 3]), (Vec 0 1 0, [2, 3, 7, 6])
           , (Vec 0 (-1) 0, [0, 1, 5, 4]), (Vec 0 0 1, [4, 5, 6, 7]), (Vec 0 0 (-1), [0, 3, 2, 1]) ]

boxCorners :: [Vec]
boxCorners = [ Vec (-1) (-1) (-1), Vec 1 (-1) (-1), Vec 1 1 (-1), Vec (-1) 1 (-1)
             , Vec (-1) (-1) 1, Vec 1 (-1) 1, Vec 1 1 1, Vec (-1) 1 1 ]

light2 :: Vec
light2 = v3Unit (Vec 0.4 0.7 0.8)

-- 3D の線分（太さ w）
line3 :: Color -> Float -> Vec -> Vec -> View2 -> Picture
line3 col w a b vw = Color col (thickLine w (projXY a vw) (projXY b vw))

-- 回した座標軸（x 赤、y 緑、z 青）
drawAxes2D :: Q -> View2 -> Double -> Picture
drawAxes2D q vw l = Pictures
  [ line3 c 2.5 (Vec 0 0 0) (qRot q e) vw
  | (c, e) <- [(rgbC 240 90 90, Vec l 0 0), (rgbC 110 220 120, Vec 0 l 0), (rgbC 100 160 255, Vec 0 0 l)] ]

-- 箱 Part 中心 半分の大きさ 色(r, g, b)
data Part = Part !Vec !Vec !(Double, Double, Double)

-- 飛行機のような物体（向きが分かりやすい）。機首 = +z、上 = +y、右の翼 = +x
planeParts :: [Part]
planeParts =
  [ Part (Vec 0 0 0) (Vec 0.18 0.18 0.9) (235, 235, 240)
  , Part (Vec 0 0 0.05) (Vec 1.0 0.04 0.26) (90, 200, 235)
  , Part (Vec 0 0.28 (-0.72)) (Vec 0.04 0.26 0.16) (245, 200, 80)
  , Part (Vec 0 0 (-0.75)) (Vec 0.4 0.03 0.12) (90, 200, 235)
  , Part (Vec 0 0.02 0.95) (Vec 0.1 0.1 0.06) (240, 110, 90) ]
