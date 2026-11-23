-- ============================================
-- 56-05 補元とブール代数 ─ a ∧ b = ⊥ かつ a ∨ b = ⊤ となる b が「a の補元」
-- 補元は ない／1 つ／いくつも ある。分配束では補元は多くても 1 つ。どの元にも補元がある分配束 = ブール代数
-- ブール代数は 原子（⊥ のすぐ上）の集合の部分集合の束と同じ形 → 要素の数はいつも 2^k。約数の束は n に平方因子がないときブール代数
-- 操作：← → 束を変える / ↑↓ 約数の束の n / 要素をクリック → その補元 / スペース 選択を消す
--       （画面では ∧ を ^、∨ を v と書く）
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, shiftL, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, nub)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

divNs :: [Int]
divNs = [6, 12, 30, 36, 60, 105, 210]   -- ★ 約数の束で選べる n

kinds :: [String]
kinds = ["Divisor lattice", "Subsets of {a,b,c,d}", "N5", "M3", "Chain (4)", "2 x 3 grid"]

-- 補元の一覧・分配か・原子・原子の集合への写像
data Info = Info
  { iComp :: [[Int]], iDist, iCompl, iUnique, iBool :: !Bool
  , iAtoms :: [Int], iCode :: [Int], iIso :: !Bool }

data World = World
  { wGi   :: !Int
  , wDi   :: !Int
  , wLat  :: Poset
  , wInfo :: Info
  , wSel  :: !Int
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-05 Complements and Boolean algebras" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 2 lat0 (analyze lat0) (-1) []
  where lat0 = tables (latticeOf 0 2)

latticeOf :: Int -> Int -> Poset
latticeOf k di = case k of
  0 -> divisorPoset (getAt 30 divNs di)
  1 -> powersetPoset 4
  2 -> fromCovers ["0", "a", "b", "c", "1"] [(0, 1), (1, 2), (2, 4), (0, 3), (3, 4)]
  3 -> fromCovers ["0", "x", "y", "z", "1"] [(0, 1), (0, 2), (0, 3), (1, 4), (2, 4), (3, 4)]
  4 -> fromCovers ["0", "1", "2", "3"] [(0, 1), (1, 2), (2, 3)]
  _ -> fromCovers ["00", "10", "01", "11", "02", "12"] [(0, 1), (0, 2), (1, 3), (2, 3), (2, 4), (3, 5), (4, 5)]

-- 補元・分配・ブール代数かどうか（lat は tables 済み）
analyze :: Poset -> Info
analyze lat = Info comp dist compl uniq (compl && dist) atoms code iso
  where
    n = pN lat
    bt = botOf lat
    tp = topOf lat
    j = tJ lat
    m = tM lat
    comp = [ [ b | b <- [0 .. n - 1], m a b == bt, j a b == tp ] | a <- [0 .. n - 1] ]
    dist = and [ m x (j y z) == j (m x y) (m x z) | x <- [0 .. n - 1], y <- [0 .. n - 1], z <- [0 .. n - 1] ]
    compl = all (not . null) comp
    uniq = all (\c -> length c <= 1) comp
    atoms = [ b | (a, b) <- pCov lat, a == bt ]
    -- 原子の集合への写像 x → {x より下の原子}。ブール代数ならこれが部分集合の束への同型
    code = [ foldl' (\acc (t, at') -> acc .|. (if leq lat at' x then 1 `shiftL` t else 0)) 0 (zip [0 ..] atoms) | x <- [0 .. n - 1] ]
    ca = listArray (0, n - 1) code :: UArray Int Int
    iso = length (nub code) == n && n == 1 `shiftL` length atoms
          && and [ leq lat x y == ((ca ! x .&. ca ! y) == ca ! x) | x <- [0 .. n - 1], y <- [0 .. n - 1] ]

squarefree :: Int -> Bool
squarefree n = all ((== 1) . snd) (primeFactors n)

rebuild :: Env -> World -> World
rebuild env w = w { wLat = layoutHasse lat 110 170 (envW env * 0.52 - 110) (envH env - 290) (wGi w <= 1), wInfo = analyze lat, wSel = -1 }
  where lat = tables (latticeOf (wGi w) (wDi w))

factorStr :: Int -> String
factorStr n = intercalate "*" [ show p ++ (if e > 1 then "^" ++ show e else "") | (p, e) <- primeFactors n ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = 21, oFs = 14, oCol = col } ]
  -- ---- 右のパネル
  ++ [ say (getAt "" kinds gi ++ (if gi == 0 then " n = " ++ show dn ++ " = " ++ factorStr dn else "") ++ "   elements " ++ show (pN lat)) px y 18 (grayC 230)
     , Color (rgbC 110 116 150) (at (px + 6, y + 31) (disc 6)), say ("no complement " ++ show nz) (px + 16) (y + 36) 15 (rgbC 110 116 150)
     , Color (rgbC 90 225 215) (at (px + 196, y + 31) (disc 6)), say ("one " ++ show n1) (px + 206) (y + 36) 15 (rgbC 90 225 215)
     , Color (rgbC 255 190 90) (at (px + 316, y + 31) (disc 6)), say ("two or more " ++ show nm) (px + 326) (y + 36) 15 (rgbC 255 190 90)
     , say ("Complemented (every element has one)? " ++ yes (iCompl inf)) px (y + 74) 16 (grayC 220)
     , say ("Distributive? " ++ yes (iDist inf) ++ "   Complements unique? " ++ yes (iUnique inf)) px (y + 100) 16 (grayC 220)
     , say ("Boolean algebra (compl. + distrib.)? " ++ yes (iBool inf)) px (y + 136) 18 (if iBool inf then rgbC 140 225 180 else rgbC 255 140 150) ]
  ++ (if iBool inf
        then [ say (show (length (iAtoms inf)) ++ " atoms: " ++ intercalate ", " (map (labOf lat) (iAtoms inf)) ++ "  -> 2^" ++ show (length (iAtoms inf)) ++ " = " ++ show (pN lat) ++ " elements") px (y + 166) 15 (grayC 200)
             , say ("x -> {atoms below x} is an isomorphism to subsets: " ++ (if iIso inf then "OK" else "NO")) px (y + 190) 15 (grayC 200) ]
        else [])
  ++ [ say ("n = " ++ show dn ++ (if squarefree dn then " is squarefree -> Boolean" else " has a square factor p^2 -> not Boolean")) px (y + 220) 15 (grayC 200) | gi == 0 ]
  ++ (if sel >= 0
        then say ("Complements of " ++ labOf lat sel ++ ": " ++ (if null cs then "none" else intercalate ", " (map (labOf lat) cs))) px (y + 262) 16 (rgbC 255 140 180)
             : [ say ("  " ++ labOf lat sel ++ " ^ " ++ labOf lat c ++ " = " ++ labOf lat (tM lat sel c) ++ " (bottom),  " ++ labOf lat sel ++ " v " ++ labOf lat c ++ " = " ++ labOf lat (tJ lat sel c) ++ " (top)")
                   px (y + 290 + 24 * fromIntegral k) 15 (rgbC 255 140 180) | (k, c) <- zip [0 :: Int ..] (take 3 cs) ]
        else [ say "Click an element -> its complements (pink)" px (y + 262) 15 (grayC 175) ])
  ++ [ say ("Complements and Boolean algebras   " ++ show (gi + 1) ++ " / " ++ show (length kinds) ++ ": " ++ getAt "" kinds gi) 20 36 18 (grayC 240)
     , say ("Left/Right lattice   Up/Down n of the divisor lattice (" ++ intercalate ", " (map show divNs) ++ ")   Click complements   Space clear") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    inf = wInfo w
    gi = wGi w
    dn = getAt 30 divNs (wDi w)
    sel = wSel w
    cs = if sel >= 0 then getAt [] (iComp inf) sel else []
    cnum i = length (getAt [] (iComp inf) i)
    col i = Just $ if i == sel then rgbC 255 255 255 else if i `elem` cs then rgbC 255 120 170
                   else case cnum i of { 0 -> rgbC 110 116 150; 1 -> rgbC 90 225 215; _ -> rgbC 255 190 90 }
    px = envW env * 0.56
    y = 160
    yes v = if v then "yes" else "no"
    nz = length (filter null (iComp inf))
    n1 = length (filter ((== 1) . length) (iComp inf))
    nm = pN lat - nz - n1

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wGi = (wGi w + 1) `mod` nk }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wGi = (wGi w + nk - 1) `mod` nk }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild env w { wDi = min (length divNs - 1) (wDi w + 1), wGi = 0 }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild env w { wDi = max 0 (wDi w - 1), wGi = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSel = -1 }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wSel = nodeAt (wLat w) (toP5 env pos) 24 }
  _ -> w
  where nk = length kinds

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("n = 2..100: divisor lattice is Boolean <=> n is squarefree (" ++ show cnt ++ " of them)", okSq && cnt == 60)
  , ("every Boolean algebra found has 2^k elements and is isomorphic to the subsets of its atoms", pow2)
  , ("N5: complemented, but c has two complements a and b (not distributive)", iCompl n5 && getAt [] (iComp n5) 3 == [1, 2] && not (iDist n5))
  , ("M3: x has two complements y and z", getAt [] (iComp m3) 1 == [2, 3] && not (iDist m3))
  , ("chain 0 < 1 < 2 < 3: the middle 1, 2 have no complement", map length (iComp (an (latticeOf 4 0))) == [1, 0, 0, 1])
  , ("distributive lattices (divisors of 12, 36, 60, 72, 360) have at most one complement"
    , and [ iDist r && iUnique r | k <- [12, 36, 60, 72, 360], let r = an (divisorPoset k) ])
  , ("subsets: the complement of X is U - X (bit flip)"
    , and [ c == [15 `xor` x] | (x, c) <- zip [0 ..] (iComp (an (powersetPoset 4))) ]) ]
  where
    an = analyze . tables
    rs = [ (k, an (divisorPoset k)) | k <- [2 .. 100] ]
    okSq = and [ iBool r == squarefree k | (k, r) <- rs ]
    cnt = length [ () | (_, r) <- rs, iBool r ]
    pow2 = and [ iIso r && (len .&. (len - 1)) == 0 | (_, r) <- rs, iBool r, let len = length (iCode r) ]
    n5 = an (latticeOf 2 0)
    m3 = an (latticeOf 3 0)

-- ---------- やってみよう ----------
-- 1. ↑↓ で n = 30 と 60 をくらべよう。60 = 2²·3·5 では、どの約数の補元がない？ なぜ？
-- 2. N5 の c をクリック。補元が 2 つあるのは分配律が破れているから（04 を見よう）
-- 3. 約数の束で d の補元は n/d？ 平方因子がない n で確かめよう
-- 4. 部分集合の束の補元は補集合。ド・モルガンの法則 (X ∪ Y)ᶜ = Xᶜ ∩ Yᶜ も束の言葉で書ける
-- 5. ブール代数の要素の数はいつも 2 のべき。要素 6 個のブール代数がないのはなぜ？

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
