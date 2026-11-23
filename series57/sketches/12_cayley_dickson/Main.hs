-- ============================================
-- 57-12 ケーリー-ディクソン構成 ─ 「2 つ組」で数を倍に広げる
-- 実数 → 複素数 → 四元数 → 八元数 → 十六元数。どれも (a, b)(c, d) = (ac − d*b, da + bc*) の 1 つの式で作れる
-- 段を上がるたびに性質を 1 つずつ失う：大小の順序 → 交換法則 → 結合法則 → 割り算（十六元数には零因子 ab = 0 がある）
-- 操作：R ランダムな数を取りかえ / Z 十六元数の零因子さがしをやり直す / ↑↓ 例を見る段 / スペース さがす速さ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

-- 段：次元と名前
levels :: [(Int, String)]
levels = [(1, "real R"), (2, "complex C"), (4, "quaternion H"), (8, "octonion O"), (16, "sedenion S")]

nTrials :: Int
nTrials = 30                           -- ★ 性質を調べるランダムな組の数

-- 1 つの段で測った「ずれ」の最大（交換・結合・交代・ノルム）
data Stat = Stat { mComm, mAssoc, mAlt, mNrm :: !Double }

-- 零因子さがしの途中の状態
data Search = Search
  { sA, sB    :: !Int
  , sFound    :: [(Int, Int, Int, Int, Double)]   -- 見つけた順
  , sTried    :: !Int
  , sDone     :: !Bool }

data World = World
  { wStats    :: [Stat]
  , wShow     :: !Int                  -- 例を見る段
  , wSearch   :: !Search
  , wFast     :: !Bool
  , wRng      :: !Rng
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "57-12 Cayley-Dickson construction" (start []))
  { skBack  = rgbC 18 18 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (start selfTest)
  , skStep  = \_ _ w -> pure w { wSearch = searchSome (if wFast w then 6 else 1) (wSearch w) }
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- 自己テストのあと seedRng(12) で測る
start :: [(String, Bool)] -> World
start t = let (st, r) = runGen measure (seedRng 12)
          in World st 2 startSearch False r t

-- 各段で、ランダムな a, b, c を使って「ずれ」の最大を測る
measure :: Gen [Stat]
measure = mapM one levels
  where
    one (n, _) = do
      ts <- replicateM nTrials ((,,) <$> cdRandom n <*> cdRandom n <*> cdRandom n)
      let f (Stat c1 a1 l1 n1) (a, b, c) =
            Stat (max c1 (cdMaxAbs (cdSub (cdMul a b) (cdMul b a))))
                 (max a1 (cdMaxAbs (cdAssoc a b c)))
                 (max l1 (cdMaxAbs (cdAssoc a a b)))
                 (max n1 (abs (cdNorm (cdMul a b) - cdNorm a * cdNorm b)))
      pure (foldl' f (Stat 0 0 0 0) ts)

startSearch :: Search
startSearch = Search 1 2 [] 0 False

-- (e_a + e_b)(e_c ± e_d) = 0 となる組を少しずつさがす（1 回で b を 1 つ進める）
searchSome :: Int -> Search -> Search
searchSome k s
  | k <= 0 || sDone s = s
  | otherwise =
      let n = 16
          a = sA s; b = sB s
          x = cdAdd (cdBasis n a 1) (cdBasis n b 1)
          hits = [ (a, b, c, d, sg) | c <- [1 .. n - 1], d <- [c + 1 .. n - 1], sg <- [1, -1]
                                    , cdMaxAbs (cdMul x (cdAdd (cdBasis n c 1) (cdBasis n d sg))) == 0 ]
          tried = sTried s + (n - 1) * (n - 2)          -- c < d の組 × 符号 2 つ
          b' = b + 1
          (a2, b2) = if b' >= n then (a + 1, a + 2) else (a, b')
          s' = s { sA = a2, sB = b2, sFound = sFound s ++ hits, sTried = tried, sDone = a2 >= n - 1 }
      in searchSome (k - 1) s'

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 表
  [ say h x y0 15 (rgbC 255 210 110) | (h, x) <- zip heads (scanl (+) x0 cw) ]
  ++ concat [ row r lv m | (r, (lv, m)) <- zip [0 :: Int ..] (zip levels (wStats w)) ]
  -- ---- 2 つ組の例
  ++ [ say "Pair formula: (a, b)(c, d) = (ac - d* b,  d a + b c*),  (a, b)* = (a*, -b)" tx ty 17 (rgbC 255 210 110) ]
  ++ (if n > 1
        then let u = cdBasis n 1 1; v = cdBasis n h 1
             in [ say (lvName (wShow w) ++ " = pairs of " ++ lvName (wShow w - 1) ++ ".  e.g. e1 = (e1, 0), e" ++ show h ++ " = (0, 1)") tx (ty + 30) 15 (grayC 210)
                , say ("e1 * e" ++ show h ++ " = " ++ cdName (cdMul u v) ++ "     e" ++ show h ++ " * e1 = " ++ cdName (cdMul v u)) tx (ty + 56) 15 (grayC 210)
                , say ("e" ++ show h ++ " * e" ++ show h ++ " = " ++ cdName (cdMul v v) ++ "  (a new imaginary unit)") tx (ty + 82) 15 (grayC 210) ]
        else [ say "A real number is one number. Conjugate a* = a" tx (ty + 30) 15 (grayC 210) ])
  -- ---- 零因子
  ++ [ say "Sedenion zero divisor search" zx ty 17 (rgbC 255 210 110)
     , say "(ea + eb)(ec +- ed) = 0" zx (ty + 24) 17 (rgbC 255 210 110)
     , say ("tried " ++ show (sTried sr) ++ "   found " ++ show (length (sFound sr)) ++ (if sDone sr then "  (all checked)" else "")) zx (ty + 54) 15 (grayC 210) ]
  ++ (case sFound sr of
        ((a, b, c, d, s) : _) ->
          let pa = cdAdd (cdBasis 16 a 1) (cdBasis 16 b 1)
              pb = cdAdd (cdBasis 16 c 1) (cdBasis 16 d s)
          in [ say ("e.g. (e" ++ show a ++ " + e" ++ show b ++ ")(e" ++ show c ++ (if s > 0 then " + " else " - ") ++ "e" ++ show d ++ ") = " ++ cdName (cdMul pa pb)) zx (ty + 82) 15 (rgbC 255 150 150)
             , sayBox "|a| = |b| = sqrt 2, yet the product is 0 -> we cannot divide by b" zx (ty + 92) (envW env - zx - 20) 15 (grayC 200) ]
        [] -> [])
  ++ [ say "Cayley-Dickson construction: real -> complex -> quaternion -> octonion -> sedenion" 20 36 20 (grayC 240)
     , say "R: new random numbers, measure again   Up/Down level for the example   Z: restart the zero divisor search   Space: search speed" 20 62 15 (grayC 175)
     , say ("Table = max error over " ++ show nTrials ++ " random triples (near 0 -> OK). Only the reals are ordered (i > 0 and i < 0 both fail)") 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    sr = wSearch w
    x0 = 30; y0 = 140
    cw = [190, 120, 150, 150, 150, 170, 180] :: [Float]
    heads = ["", "ordered", "ab = ba", "(ab)c = a(bc)", "(aa)b = a(ab)", "|ab| = |a||b|", "division"]
    row r (n', name) m =
      let y = y0 + 40 + fromIntegral r * 54
          cells = [ (r == 0, if r == 0 then "yes" else "no")
                  , (mComm m < 1e-12, expo 0 (mComm m)), (mAssoc m < 1e-12, expo 0 (mAssoc m))
                  , (mAlt m < 1e-12, expo 0 (mAlt m)), (mNrm m < 1e-9, expo 0 (mNrm m))
                  , (r < 4, if r < 4 then "possible" else if null (sFound sr) then "searching..." else "zero div. " ++ show (length (sFound sr))) ]
      in [ Color (if r == wShow w then rgbC 40 50 90 else rgbC 28 30 52) (rectTL (x0 - 10) (y - 26) (sum cw) 46)
         , say (name ++ " (" ++ show n' ++ ")") x0 y 17 (grayC 235) ]
         ++ concat [ [ say (if ok then "OK" else "NO") xx y 17 (if ok then rgbC 130 230 150 else rgbC 255 130 130)
                     , say v (xx + 34) y 14 (grayC 185) ]
                   | ((ok, v), xx) <- zip cells (drop 1 (scanl (+) x0 cw)) ]
    tx = 30; ty = y0 + 330
    n = fst (getAt (1, "") levels (wShow w))
    h = n `div` 2
    lvName i = snd (getAt (1, "") levels i)
    zx = envW env * 0.62

-- 0 でない成分だけを「−e5」「2e3 + e7」のように
cdName :: CD -> String
cdName v =
  case [ (x, a) | (a, x) <- zip [0 :: Int ..] v, abs x > 1e-12 ] of
    [] -> "0"
    nz -> unwords [ (if x < 0 then "-" else if k > 0 then "+" else "")
                    ++ (if abs x == 1 && a /= 0 then "" else jsNum (abs x))
                    ++ (if a /= 0 then "e" ++ show a else "")
                  | (k, (x, a)) <- zip [0 :: Int ..] nz ]
  where jsNum x = if x == fromIntegral (round x :: Integer) then show (round x :: Integer) else show x

event :: Event -> World -> World
event ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> let (st, r) = runGen measure (wRng w) in w { wStats = st, wRng = r }
    | c `elem` "zZ" -> w { wSearch = startSearch }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wShow = max 0 (wShow w - 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wShow = min 4 (wShow w + 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wFast = not (wFast w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("cdMul in 2 dimensions = complex product, in 4 dimensions = qMul (Hamilton product)", e1 < 1e-12 && e2 < 1e-12)
  , ("at every level x x* = |x|^2 (a real number)", conjE < 1e-10)
  , ("up to the octonions the norm respects products (relative 1e-9), the sedenions break it (" ++ fmt 2 sedN ++ ")", e3 < 1e-9 && sedN > 0.05)
  , ("sedenions are not even alternative: (aa)b /= a(ab)", sedAlt > 0.1)
  , ("sedenion zero divisor: (e1 + e10)(e4 - e15) = 0 (neither is 0)", cdMaxAbs (cdMul za zb) == 0 && cdNorm za > 0 && cdNorm zb > 0)
  , ("octonions: no zero divisor among all pairs of the same form", octZero == 0) ]
  where
    trials = evalGen 120 $ replicateM 100 $ do
      a <- cdRandom 4; b <- cdRandom 4; z <- cdRandom 2; w <- cdRandom 2
      o1 <- cdRandom 8; o2 <- cdRandom 8
      s1 <- cdRandom 16; s2 <- cdRandom 16
      xs <- mapM cdRandom [2, 4, 8, 16]
      pure (a, b, z, w, o1, o2, s1, s2, xs)
    toQ v = case v of
      [p, q, r, s] -> Q p q r s
      _ -> Q 0 0 0 0
    mx = maximum . (0 :)
    e1 = mx [ cdMaxAbs (cdSub (cdMul a b) (qList (qMul (toQ a) (toQ b)))) | (a, b, _, _, _, _, _, _, _) <- trials ]
    e2 = mx [ max (abs (getAt 0 zw 0 - (z0 * w0 - z1 * w1))) (abs (getAt 0 zw 1 - (z0 * w1 + z1 * w0)))
            | (_, _, z@[z0, z1], w@[w0, w1], _, _, _, _, _) <- trials, let zw = cdMul z w ]
    e3 = mx [ abs (cdNorm (cdMul o1 o2) / (cdNorm o1 * cdNorm o2) - 1) | (_, _, _, _, o1, o2, _, _, _) <- trials ]
    sedN = mx [ abs (cdNorm (cdMul s1 s2) / (cdNorm s1 * cdNorm s2) - 1) | (_, _, _, _, _, _, s1, s2, _) <- trials ]
    sedAlt = mx [ cdMaxAbs (cdAssoc s1 s1 s2) | (_, _, _, _, _, _, s1, s2, _) <- trials ]
    conjE = mx [ max (abs (getAt 0 xx 0 - cdNorm x * cdNorm x)) (cdMaxAbs (drop 1 xx))
               | (_, _, _, _, _, _, _, _, xs) <- trials, x <- xs, let xx = cdMul x (cdConj x) ]
    za = cdAdd (cdBasis 16 1 1) (cdBasis 16 10 1)
    zb = cdAdd (cdBasis 16 4 1) (cdBasis 16 15 (-1))
    octZero = length [ () | a <- [1 .. 7], b <- [a + 1 .. 7], c <- [1 .. 7], d <- [c + 1 .. 7], s <- [1, -1]
                          , cdMaxAbs (cdMul (cdAdd (cdBasis 8 a 1) (cdBasis 8 b 1)) (cdAdd (cdBasis 8 c 1) (cdBasis 8 d s))) == 0 ]

-- ---------- やってみよう ----------
-- 1. R を何回か押そう。OK と NO の並びは変わる？ 数字（ずれの大きさ）は？
-- 2. ↓ で段を下りながら、e1 · e_h と e_h · e1 を比べよう。どの段から符号が変わる？
-- 3. 見つかった零因子の 1 つを、自分で cdMul に入れて確かめよう（ghci で cdMul a b）
-- 4. 32 次元（cdRandom 32）でも測ってみよう。ノルムは？ 零因子は？
-- 5. 「1, 2, 4, 8 次元だけが、ノルムが積と両立する」（フルヴィッツの定理）。表のノルムの列で確かめよう

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
-- ---- ケーリー-ディクソン構成：長さ 2ⁿ のリスト = 「数の 2 つ組」(a, b)
--   積の約束 (a, b)(c, d) = (ac − d* b, da + b c*)、共役 (a, b)* = (a*, −b)
--   長さ 1 = 実数、2 = 複素数、4 = 四元数（qMul と一致）、8 = 八元数、16 = 十六元数
type CD = [Double]

cdConj :: CD -> CD
cdConj x = case x of
  [v] -> [v]
  _ -> let h = length x `div` 2
           (a, b) = splitAt h x
       in cdConj a ++ map negate b

cdMul :: CD -> CD -> CD
cdMul x y = case (x, y) of
  ([u], v : _) -> [u * v]
  _ -> let h = length x `div` 2
           (a, b) = splitAt h x
           (c, d) = splitAt h y
           ac = cdMul a c
           db = cdMul (cdConj d) b
           da = cdMul d a
           bc = cdMul b (cdConj c)
       in zipWith (-) ac db ++ zipWith (+) da bc

cdAdd, cdSub :: CD -> CD -> CD
cdAdd = zipWith (+)
cdSub = zipWith (-)

cdNorm :: CD -> Double
cdNorm x = sqrt (foldl' (\s v -> s + v * v) 0 x)

cdBasis :: Int -> Int -> Double -> CD  -- s · e_a（長さ n）
cdBasis n a s = [ if t == a then s else 0 | t <- [0 .. n - 1] ]

cdRandom :: Int -> Gen CD
cdRandom n = replicateM n rndNormalG

cdMaxAbs :: CD -> Double
cdMaxAbs = foldl' (\m v -> max m (abs v)) 0

cdAssoc :: CD -> CD -> CD -> CD        -- 結合子 (xy)z − x(yz)
cdAssoc x y z = cdSub (cdMul (cdMul x y) z) (cdMul x (cdMul y z))
