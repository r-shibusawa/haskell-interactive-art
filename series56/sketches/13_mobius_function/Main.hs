-- ============================================
-- 56-13 メビウス関数と反転公式 ─ 区間 [x, y] ごとの数 μ(x, y) を再帰で計算する
-- μ(x, x) = 1、x < y なら μ(x, y) = −Σ_{x ≤ z < y} μ(x, z)。約数の束では μ(1, n) が数論のメビウス関数、部分集合の束では (−1)^|Y − X|（包除原理）
-- 反転公式：f(y) = Σ_{x ≤ y} g(x) なら g(y) = Σ_{x ≤ y} μ(x, y) f(x)。g を f から復元してみる
-- 操作：← → 半順序を変える / ↑↓ 約数の束の n / 要素をクリック → その x から見た μ(x, y) / スペース g を新しく / F 約数の束で g = オイラーの φ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (complement, shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl')
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

divNs :: [Int]
divNs = [12, 30, 36, 60, 72, 210, 360]   -- ★ 約数の束で選べる n

kinds :: [String]
kinds = ["divisor lattice", "subsets of {a,b,c,d}", "N5", "M3", "random poset"]

-- μ の表：mu ! (x, y)
type Mu = UArray (Int, Int) Int

data World = World
  { wKi, wDi :: !Int
  , wLat     :: Poset
  , wMu      :: !Mu
  , wX0      :: !Int
  , wG       :: [Int]
  , wPhi     :: !Bool
  , wRng     :: !Rng                 -- p5.js 版と同じ：乱数の状態はずっと続く
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-13 Moebius function and inversion" (rebuild env1 w0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env w0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty
        w0 = World 0 3 (divisorPoset 12) emptyT 0 [] True rngAfterTest []

-- k 番目の半順序（乱数を使うのは 4 だけ。種 13 から引く）
posetOf :: Int -> Int -> Rng -> (Poset, Rng)
posetOf k di r = case k of
  0 -> (divisorPoset (getAt 60 divNs di), r)
  1 -> (powersetPoset 4, r)
  2 -> (fromCovers ["0", "a", "b", "c", "1"] [(0, 1), (1, 2), (2, 4), (0, 3), (3, 4)], r)
  3 -> (fromCovers ["0", "x", "y", "z", "1"] [(0, 1), (0, 2), (0, 3), (1, 4), (2, 4), (3, 4)], r)
  _ -> let (q, r') = randomPoset 12 0.25 (seedRng 13)
       in (q { pLab = A.listArray (0, pN q - 1) [ [c] | c <- take (pN q) "abcdefghijkl" ] }, r')

-- μ(x, ·) を x ごとに、y をランクの低い順に計算
mobius :: Poset -> Mu
mobius lat = runSTUArray $ do
  let n = pN lat
      ord = sortBy (comparing (rankOf lat)) (allOf lat)
  m <- newArray ((0, 0), (n - 1, n - 1)) 0
  forM_ [0 .. n - 1] $ \x -> do
    writeArray m (x, x) 1
    forM_ ord $ \y -> when (y /= x && leq lat x y) $ do
      s <- foldM (\acc z -> if z /= y && leq lat x z && leq lat z y then (acc +) <$> readArray m (x, z) else pure acc) 0 [0 .. n - 1]
      writeArray m (x, y) (negate s)
  pure m

muAt :: Mu -> Int -> Int -> Int
muAt = tableAt

-- 数論のメビウス関数とオイラーの φ
muNT :: Int -> Int
muNT n = let f = primeFactors n in if any ((> 1) . snd) f then 0 else if odd (length f) then -1 else 1

phi :: Int -> Int
phi n = foldl' (\r (p, e) -> r * (p - 1) * p ^ (e - 1)) 1 (primeFactors n)

-- f(y) = Σ_{x ≤ y} g(x)、g(y) = Σ μ(x, y) f(x)
sumBelow :: Poset -> [Int] -> [Int]
sumBelow lat g = [ sum [ gx | (x, gx) <- zip [0 ..] g, leq lat x y ] | y <- allOf lat ]

invert :: Poset -> Mu -> [Int] -> [Int]
invert lat mu f = [ sum [ muAt mu x y * fx | (x, fx) <- zip [0 ..] f, leq lat x y ] | y <- allOf lat ]

-- 乱数で g を n 個（−2〜6）
randomG :: Int -> Rng -> ([Int], Rng)
randomG n r0 = let (acc, r) = foldl' (\(a, rr) _ -> let (u, r') = rnd rr in (floor (u * 9) - 2 : a, r')) ([], r0) [1 .. n] in (reverse acc, r)

newG :: World -> World
newG w
  | wKi w == 0 && wPhi w = w { wG = map phi (pVal (wLat w)) }
  | otherwise = let (g, r) = randomG (pN (wLat w)) (wRng w) in w { wG = g, wRng = r }

rebuild :: Env -> World -> World
rebuild env w = newG w { wLat = lat, wMu = mobius lat, wX0 = max 0 (botOf lat), wRng = r }
  where (q, r) = posetOf (wKi w) (wDi w) (wRng w)
        lat = layoutHasse q 90 180 (envW env * 0.55 - 90) (envH env - 300) (wKi w <= 1)

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = 20, oFs = 14, oCol = \i -> Just (colOf i) } ]
  ++ [ let (x, y) = posOf lat i in sayC ("mu=" ++ show (m i)) x (y - 25) 15 (txtCol i) | i <- allOf lat, leq lat x0 i ]
  -- ---- 右：数論・包除・反転公式
  ++ [ say (getAt "" kinds ki ++ (if ki == 0 then " n = " ++ show (getAt 0 divNs (wDi w)) else "") ++ "   mu(x, y) from x = " ++ labOf lat x0) px yy 16 (grayC 230) ]
  ++ (case ki of
        0 -> [ say ("mu(1, d) = number-theory mu(d): " ++ (if okNT then "OK for every divisor" else "NO")) px (yy + 30) 15 (grayC 200)
             , say "mu(d) = 0 (square factor), (-1)^k (k distinct primes)" px (yy + 54) 15 (grayC 200) ]
        1 -> [ say ("mu(X, Y) = (-1)^|Y - X|: " ++ (if okPS then "OK" else "NO") ++ "  -> inclusion-exclusion") px (yy + 30) 15 (grayC 200)
             , say "|AuBuC| = |A|+|B|+|C| - |AnB| - ... + |AnBnC|" px (yy + 54) 15 (grayC 200) ]
        _ -> [ say ("mu(bottom, top) = " ++ (if botOf lat >= 0 && topOf lat >= 0 then show (muAt mu (botOf lat) (topOf lat)) else "- (no bottom or top)")
                    ++ (if ki == 3 then "   (in M3: 3 - 1 = 2)" else "")) px (yy + 30) 15 (grayC 200) ])
  ++ [ say "Inversion: recover g from f(y) = sum_{x<=y} g(x)" px y1 15 (grayC 230) ]
  ++ [ say "(g = phi, so f(n) = n)" px (y1 + 22) 15 (grayC 230) | phiOn ]
  ++ [ say "y" px (y2 + 28) 14 (grayC 170), say "g(y)" (px + 90) (y2 + 28) 14 (grayC 170)
     , say "f(y)" (px + 170) (y2 + 28) 14 (grayC 170), say "sum mu(x,y) f(x)" (px + 250) (y2 + 28) 14 (grayC 170) ]
  ++ concat [ let ok = a == b
                  yk = y2 + 50 + 20 * fromIntegral k
              in [ say (labOf lat k) px yk 14 (grayC 220), say (show b) (px + 90) yk 14 (grayC 220), say (show c) (px + 170) yk 14 (grayC 220)
                 , say (show a ++ (if ok then " OK" else " NO")) (px + 250) yk 14 (if ok then rgbC 140 225 180 else rgbC 255 110 110) ]
            | (k, a, b, c) <- take rows (zip4 [0 :: Int ..] g2 gv f) ]
  ++ [ say ("... " ++ show (pN lat - rows) ++ " more rows (all " ++ (if g2 == gv then "match" else "do NOT match") ++ ")") px (y2 + 50 + 20 * fromIntegral rows) 14 (grayC 150) | rows < pN lat ]
  ++ [ say ("Moebius function and inversion   " ++ getAt "" kinds ki ++ "   (cyan +, red -, gray 0, dark = not x <= y)") 20 36 18 (grayC 240)
     , say ("Left/Right poset   Up/Down n   Click choose x   Space new random g   F g = phi on divisors " ++ (if wPhi w then "ON" else "OFF")) 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    mu = wMu w
    ki = wKi w
    x0 = wX0 w
    m i = muAt mu x0 i
    colOf i | i == x0 = rgbC 255 255 255
            | not (leq lat x0 i) = rgbC 70 76 110
            | m i > 0 = rgbC 90 225 215
            | m i < 0 = rgbC 255 130 150
            | otherwise = rgbC 150 160 205
    txtCol i | m i > 0 = rgbC 90 225 215
             | m i < 0 = rgbC 255 150 160
             | otherwise = grayC 190
    px = envW env * 0.58
    yy = 160
    y1 = yy + 90
    phiOn = ki == 0 && wPhi w
    y2 = if phiOn then y1 + 22 else y1
    gv = wG w
    f = sumBelow lat gv
    g2 = invert lat mu f
    rows = min (pN lat) (floor ((envH env - y2 - 90) / 20))
    okNT = and [ muAt mu 0 i == muNT d | (i, d) <- zip [0 ..] (pVal lat) ]
    okPS = and [ not (leq lat a b) || muAt mu a b == (if odd (popc (b .&. complement a)) then -1 else 1) | a <- allOf lat, b <- allOf lat ]
    zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let i = nodeAt (wLat w) (toP5 env pos) 24 in if i >= 0 then w { wX0 = i } else w
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wKi = (wKi w + 1) `mod` nk }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wKi = (wKi w + nk - 1) `mod` nk }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild env w { wDi = min (length divNs - 1) (wDi w + 1), wKi = 0 }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild env w { wDi = max 0 (wDi w - 1), wKi = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> newG w { wPhi = False }
  EventKey (Char c) Down _ _ | c `elem` "fF" -> newG w { wPhi = not (wPhi w) }
  _ -> w
  where nk = length kinds

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest = fst selfTestR

rngAfterTest :: Rng
rngAfterTest = snd selfTestR

selfTestR :: ([(String, Bool)], Rng)
selfTestR =
  ( [ ("n = 1..100: mu(1, n) on the divisor lattice = number-theory mu(n)", ok1)
    , ("and mu(d, n) = mu(n/d) (the interval [d, n] looks like the divisors of n/d)", ok2)
    , ("subset lattice: mu(X, Y) = (-1)^|Y - X| for every X <= Y"
      , and [ not (leq b4 a b) || muAt mb a b == (if odd (popc (b .&. complement a)) then -1 else 1) | a <- allOf b4, b <- allOf b4 ])
    , ("20 random posets: the inversion formula recovers g from f", inv)
    , ("sum_{d|n} phi(d) = n, inverting gives phi back (all divisors of 60)"
      , fn == pVal d60 && invert d60 (mobius d60) (pVal d60) == map phi (pVal d60))
    , ("chain: mu(i, i+1) = -1, 0 further apart. M3: mu(0, 1) = 2"
      , [ muAt mc 0 j | j <- [0 .. 3] ] == [1, -1, 0, 0] && [ muAt mc 1 j | j <- [0 .. 3] ] == [0, 1, -1, 0] && muAt m3 0 4 == 2) ]
  , rEnd )
  where
    dps = [ (n, divisorPoset n) | n <- [1 .. 100] ]
    ok1 = and [ muAt (mobius d) 0 (pN d - 1) == muNT n | (n, d) <- dps ]
    ok2 = and [ muAt md i (pN d - 1) == muNT (n `div` v) | (n, d) <- dps, let md = mobius d, (i, v) <- zip [0 ..] (pVal d) ]
    b4 = powersetPoset 4
    mb = mobius b4
    (inv, rEnd) = foldl' one (True, seedRng 1313) [0 .. 19 :: Int]
    one (ok, r0) t = let (q, r1) = randomPoset (5 + t `mod` 10) 0.3 r0
                         (gr, r2) = foldl' (\(a, rr) _ -> let (u, r') = rnd rr in (floor (u * 21) - 10 : a, r')) ([], r1) [1 .. pN q]
                         g = reverse gr
                     in (ok && invert q (mobius q) (sumBelow q g) == g, r2)
    d60 = divisorPoset 60
    fn = sumBelow d60 (map phi (pVal d60))
    mc = mobius (fromCovers ["0", "1", "2", "3"] [(0, 1), (1, 2), (2, 3)])
    m3 = mobius (fst (posetOf 3 0 (seedRng 1)))

-- ---------- やってみよう ----------
-- 1. 60 の約数で、μ(1, d) が 0 になるのはどの d？（4 = 2² で割り切れるもの）
-- 2. 部分集合の束で x = ∅（{}）を選ぶ。μ の符号は要素の数の偶奇で決まる → 包除原理の + − + −
-- 3. F で g = φ（n 以下で n と互いに素な数の個数）。f(n) = Σ φ(d) = n になるのはなぜ？
-- 4. スペースで g をランダムに。反転公式でいつも復元できることを確かめよう
-- 5. M3 の μ(0, 1) = 2、N5 では？ 計算してから画面で確かめよう
-- 6. mobius は runSTUArray の中で表を書きかえて作っている。リストだけで書くとどうなる？


-- ==================================================
-- ここから下は「束論の道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 順序の表：R ! (i, j) = True は「i ≤ j」（n × n の表）
type Rel = UArray (Int, Int) Bool

-- 順序集合 P。pJ・pM は上限・下限の表（tables で作る）、pPos はハッセ図の位置
data Poset = Poset
  { pN    :: !Int
  , pR    :: !Rel
  , pCov  :: [(Int, Int)]
  , pRk   :: !(UArray Int Int)
  , pLab  :: A.Array Int String
  , pPos  :: A.Array Int (Float, Float)
  , pVal  :: [Int]
  , pHint :: Maybe (Int -> Double)
  , pJ, pM :: !(UArray (Int, Int) Int) }

relSize :: Rel -> Int
relSize r = let ((_, _), (n1, _)) = bounds r in n1 + 1

elemsU :: Rel -> [Bool]
elemsU r = [ r ! (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

assocsU :: Rel -> [((Int, Int), Bool)]
assocsU r = [ ((i, j), r ! (i, j)) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

leq :: Poset -> Int -> Int -> Bool
leq p i j = pR p ! (i, j)

rankOf :: Poset -> Int -> Int
rankOf p i = if i >= 0 && i < pN p then pRk p ! i else 0

labOf :: Poset -> Int -> String
labOf p i = if i >= 0 && i < pN p then pLab p A.! i else "-"

posOf :: Poset -> Int -> (Float, Float)
posOf p i = let (lo, hi) = A.bounds (pPos p) in if i >= lo && i <= hi then pPos p A.! i else (0, 0)

makePoset :: Int -> (Int -> Int -> Bool) -> [String] -> Poset
makePoset n leqFn labs = posetFromRel (listArray ((0, 0), (n - 1, n - 1)) [ i == j || leqFn i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]) labs

-- labs が空なら 0, 1, 2, ... を名前にする
posetFromRel :: Rel -> [String] -> Poset
posetFromRel r labs = Poset n r cov (ranksOf n cov) (A.listArray (0, n - 1) (take n (labs ++ map show [length labs ..])))
                            (A.listArray (0, -1) []) [] Nothing emptyT emptyT
  where n = relSize r
        cov = coversOf r

emptyT :: UArray (Int, Int) Int
emptyT = listArray ((0, 0), (-1, -1)) []

-- 被覆の組から
fromCovers :: [String] -> [(Int, Int)] -> Poset
fromCovers labs cv = posetFromRel (closeRel (runSTUArray (do
  r <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ cv $ \(a, b) -> writeArray r (a, b) True
  pure r))) labs
  where n = length labs

-- 反射的・推移的閉包（ワーシャル法：k を経由して行けるなら i ≤ j）
closeRel :: Rel -> Rel
closeRel r0 = runSTUArray $ do
  let n = relSize r0
  c <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ [0 .. n - 1] $ \i -> forM_ [0 .. n - 1] $ \j -> writeArray c (i, j) (r0 ! (i, j) || i == j)
  forM_ [0 .. n - 1] $ \k -> forM_ [0 .. n - 1] $ \i -> do
    ik <- readArray c (i, k)
    when ik $ forM_ [0 .. n - 1] $ \j -> do
      kj <- readArray c (k, j)
      when kj (writeArray c (i, j) True)
  pure c

isReflexive, isAntisym, isTransitive, isPoset :: Rel -> Bool
isReflexive r = and [ r ! (i, i) | i <- [0 .. relSize r - 1] ]
isAntisym r = and [ not (r ! (i, j) && r ! (j, i)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ] where n = relSize r
isTransitive r = and [ r ! (i, k) | i <- [0 .. n - 1], j <- [0 .. n - 1], r ! (i, j), k <- [0 .. n - 1], r ! (j, k) ] where n = relSize r
isPoset r = isReflexive r && isAntisym r && isTransitive r

-- 被覆 a ⋖ b：a < b で、間にはさまる c がない（推移的簡約 = ハッセ図の辺）
coversOf :: Rel -> [(Int, Int)]
coversOf r = [ (a, b) | a <- [0 .. n - 1], b <- [0 .. n - 1], lt a b, not (any (\c -> lt a c && lt c b) [0 .. n - 1]) ]
  where n = relSize r
        lt a b = a /= b && r ! (a, b) && not (r ! (b, a))

-- ランク = 下から c までの最長の鎖の長さ（最大 n 回くり返せば止まる）
ranksOf :: Int -> [(Int, Int)] -> UArray Int Int
ranksOf n cov = runSTUArray $ do
  rk <- newArray (0, n - 1) 0
  let pass t = when (t <= n) $ do
        ch <- foldM (\c (a, b) -> do
                       ra <- readArray rk a
                       rb <- readArray rk b
                       if rb < ra + 1 && ra < n then writeArray rk b (ra + 1) >> pure True else pure c) False cov
        when ch (pass (t + 1))
  pass 0
  pure rk

allOf :: Poset -> [Int]
allOf p = [0 .. pN p - 1]

upperOf, lowerOf :: Poset -> Int -> Int -> [Int]
upperOf p a b = [ c | c <- allOf p, leq p a c, leq p b c ]     -- 上界：a と b の両方より上
lowerOf p a b = [ c | c <- allOf p, leq p c a, leq p c b ]     -- 下界

-- 最小元・最大元（なければ −1）
leastOf, greatestOf :: Poset -> [Int] -> Int
leastOf p sx = case [ x | x <- sx, all (leq p x) sx ] of { (x : _) -> x; [] -> -1 }
greatestOf p sx = case [ x | x <- sx, all (\y -> leq p y x) sx ] of { (x : _) -> x; [] -> -1 }

-- 上限 a ∨ b = 最小上界、下限 a ∧ b = 最大下界（なければ −1）
joinOf, meetOf :: Poset -> Int -> Int -> Int
joinOf p a b = leastOf p (upperOf p a b)
meetOf p a b = greatestOf p (lowerOf p a b)

botOf, topOf :: Poset -> Int
botOf p = leastOf p (allOf p)
topOf p = greatestOf p (allOf p)

isLattice :: Poset -> Bool
isLattice p = pN p > 0 && and [ joinOf p a b >= 0 && meetOf p a b >= 0 | a <- allOf p, b <- [a + 1 .. pN p - 1] ]

-- 上限・下限の表（何度も使うので先に計算）
tables :: Poset -> Poset
tables p = p { pJ = mk joinOf, pM = mk meetOf }
  where n = pN p
        mk f = listArray ((0, 0), (n - 1, n - 1)) [ f p a b | a <- [0 .. n - 1], b <- [0 .. n - 1] ]

-- 表を引く（tables の前や範囲の外なら −1）
tJ, tM :: Poset -> Int -> Int -> Int
tJ p a b = tableAt (pJ p) a b
tM p a b = tableAt (pM p) a b

tableAt :: UArray (Int, Int) Int -> Int -> Int -> Int
tableAt t a b = let ((_, _), (n1, _)) = bounds t in if a >= 0 && b >= 0 && a <= n1 && b <= n1 then t ! (a, b) else -1

-- ---- よく使う束：約数の束（割り切る）と部分集合の束（含まれる）
lcmOf :: Int -> Int -> Int
lcmOf a b = (a `div` gcd a b) * b

divisorsOf :: Int -> [Int]
divisorsOf n = [ k | k <- [1 .. n], n `mod` k == 0 ]

primeFactors :: Int -> [(Int, Int)]
primeFactors = go 2
  where
    go p n | p * p > n = if n > 1 then [(n, 1)] else []
           | n `mod` p == 0 = let (e, m) = strip p n 0 in (p, e) : go (p + 1) m
           | otherwise = go (p + 1) n
    strip p n e = if n `mod` p == 0 then strip p (n `div` p) (e + 1) else (e, n)

expOf :: Int -> Int -> Int
expOf p v = if v > 0 && v `mod` p == 0 then 1 + expOf p (v `div` p) else 0

divisorPoset :: Int -> Poset
divisorPoset n = (makePoset (length ds) (\i j -> dv j `mod` dv i == 0) (map show ds)) { pVal = ds, pHint = Just hint }
  where
    ds = divisorsOf n
    da = listArray (0, length ds - 1) ds :: UArray Int Int
    dv i = da ! i
    ps = map fst (primeFactors n)
    dir = [-1, 1, -0.3, 0.55, -0.62] :: [Double]
    hint i = sum [ fromIntegral (expOf p (dv i)) * getAt 0 dir (t `mod` 5) | (t, p) <- zip [0 ..] ps ]

popc :: Int -> Int
popc m = if m <= 0 then 0 else (m .&. 1) + popc (m `shiftR` 1)

-- 部分集合の名前（空集合は {}）
setName :: Int -> Int -> String
setName m k = case [ c | (t, c) <- zip [0 .. k - 1] "abcdefg", (m `shiftR` t) .&. 1 == 1 ] of
  "" -> "{}"
  s -> s

powersetPoset :: Int -> Poset
powersetPoset k = (makePoset (2 ^ k) (\i j -> i .&. j == i) [ setName m k | m <- [0 .. 2 ^ k - 1] ])
                    { pVal = [0 .. 2 ^ k - 1], pHint = if k <= 4 then Just hint else Nothing }
  where dir = [-1, 1, -0.3, 0.55] :: [Double]
        hint i = sum [ fromIntegral ((i `shiftR` t) .&. 1) * d | (t, d) <- zip [0 ..] dir ]

-- ---- ハッセ図を描く：高さ = ランク、横 = hint（なければ下の被覆の平均の順）
layoutHasse :: Poset -> Float -> Float -> Float -> Float -> Bool -> Poset
layoutHasse p x y w h exact = p { pPos = A.listArray (0, n - 1) [ (x + w * f2 (pxF i), y + h - (if mr > 0 then fromIntegral (rankOf p i) / fromIntegral mr else 0.5) * h) | i <- [0 .. n - 1] ] }
  where
    n = pN p
    mr = maximum (0 : [ rankOf p i | i <- [0 .. n - 1] ])
    useHint = exact && isJust (pHint p)
    lvl r = [ i | i <- [0 .. n - 1], rankOf p i == r ]
    step :: A.Array Int Double -> Int -> A.Array Int Double
    step px r =
      let ids = lvl r
          v i = case pHint p of
            Just f -> f i
            Nothing -> let ss = [ px A.! a | (a, b) <- pCov p, b == i ]
                       in (if null ss then 0.5 else sum ss / fromIntegral (length ss)) + fromIntegral i * 1e-6
          px1 = px A.// [ (i, v i) | i <- ids ]
          sorted = sortBy (comparing (px1 A.!)) ids
          m = length sorted
      in if useHint then px1 else px1 A.// [ (i, (fromIntegral k + 0.5) / fromIntegral m) | (k, i) <- zip [0 :: Int ..] sorted ]
    pxRaw = foldl' step (A.listArray (0, n - 1) (replicate n 0.5)) [0 .. mr]
    vs = A.elems pxRaw
    lo = if null vs then 0 else minimum vs
    hi = if null vs then 0 else maximum vs
    pxF i | useHint = if hi > lo then 0.04 + 0.92 * (pxRaw A.! i - lo) / (hi - lo) else 0.5
          | otherwise = pxRaw A.! i

-- 描き方の指定：要素の色・辺の色（Nothing なら標準の色）、丸の半径、文字の大きさ
data HOpt = HOpt { oCol :: Int -> Maybe Color, oEcol :: Int -> Int -> Maybe Color, oRad :: Float, oFs :: Float }

hopt :: HOpt
hopt = HOpt (const Nothing) (\_ _ -> Nothing) 16 14

drawHasse :: Poset -> HOpt -> Picture
drawHasse p o = Pictures $
  [ Color (maybe (rgbC 96 108 168) id (oEcol o a b)) (thickLine 2 (posOf p a) (posOf p b)) | (a, b) <- pCov p ]
  ++ concat [ [ Color bgCol (at (posOf p i) (disc (oRad o + 1)))
              , Color (maybe (rgbC 226 232 248) id (oCol o i)) (at (posOf p i) (disc (oRad o - 1)))
              , let (px, py) = posOf p i in sayM (labOf p i) px (py + 1) (max 14 (oFs o)) bgCol ]
            | i <- allOf p ]

-- (mx, my) にいちばん近い要素（r より遠ければ −1）
nodeAt :: Poset -> (Float, Float) -> Float -> Int
nodeAt p (mx, my) r = fst (foldl' f (-1, r) (allOf p))
  where f (best, bd) i = let (x, y) = posOf p i
                             d = sqrt ((x - mx) * (x - mx) + (y - my) * (y - my))
                         in if d < bd then (i, d) else (best, bd)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
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

-- ランダムな順番 o で、前 → 後ろに確率 p で辺 → 閉包
randomPoset :: Int -> Double -> Rng -> (Poset, Rng)
randomPoset n p r0 = (posetFromRel (closeRel rel) [], r2)
  where
    shuffle (o, r) i = let (u, r') = rnd r
                           j = floor (u * fromIntegral (i + 1))
                           oi = o A.! i
                           oj = o A.! j
                       in (o A.// [(i, oj), (j, oi)], r')
    (ord, r1) = foldl' shuffle (A.listArray (0, n - 1) [0 .. n - 1] :: A.Array Int Int, r0) [n - 1, n - 2 .. 1]
    pick (acc, r) (a, b) = let (u, r') = rnd r in (if u < p then (ord A.! a, ord A.! b) : acc else acc, r')
    (edges, r2) = foldl' pick ([], r1) [ (a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1] ]
    rel = runSTUArray $ do
      m <- newArray ((0, 0), (n - 1, n - 1)) False
      forM_ edges $ \e -> writeArray m e True
      pure m

-- p5.js 版と同じ順番：clampv v a b（v を a〜b に収める）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

f2 :: Double -> Float
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (if isNaN v || isInfinite v then 0 else v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss のマウス座標 → 左上が原点の座標
toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

bgCol :: Color
bgCol = rgbC 18 20 44

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃え（y は文字の下の線）、sayM は (x, y) が文字の真ん中
sayC, sayR, sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c
sayM s x y size c = say s (x - textW s size / 2) (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 220 170 else rgbC 255 90 90)
