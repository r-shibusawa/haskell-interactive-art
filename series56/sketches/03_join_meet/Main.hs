-- ============================================
-- 56-03 上限と下限の計算 ─ 「最小上界」がいつもあるとは限らない
-- 2 点 a, b の上界 = 両方より上の元。その中に最小のものがあれば上限 a ∨ b（なければ上限なし）。下限 a ∧ b も同じ
-- どの 2 点にも上限と下限があるとき「束」。束は代数としても見られる：交換・結合・吸収・冪等の 4 つの法則
-- 操作：← → 図を変える（束と束でないもの 6 種類）/ 要素を 2 つクリック / スペース 選択を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ [名前, 要素の名前, 被覆 (下, 上) の組]
gallery :: [(String, [String], [(Int, Int)])]
gallery =
  [ ("divisors of 12", ["1", "2", "3", "4", "6", "12"], [(0, 1), (0, 2), (1, 3), (1, 4), (2, 4), (3, 5), (4, 5)])
  , ("N5 (pentagon)", ["0", "a", "b", "c", "1"], [(0, 1), (1, 2), (2, 4), (0, 3), (3, 4)])
  , ("M3 (diamond)", ["0", "a", "b", "c", "1"], [(0, 1), (0, 2), (0, 3), (1, 4), (2, 4), (3, 4)])
  , ("bow tie (not a lattice)", ["0", "a", "b", "c", "d", "1"], [(0, 1), (0, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 5), (4, 5)])
  , ("V with no top (not a lattice)", ["0", "a", "b", "c"], [(0, 1), (0, 2), (1, 3)])
  , ("2 x 3 grid", ["00", "10", "01", "11", "02", "12"], [(0, 1), (0, 2), (1, 3), (2, 3), (2, 4), (3, 5), (4, 5)]) ]

-- 4 つの法則と「順序にもどる」の結果
data Laws = Laws { lComm, lAssoc, lAbsorb, lIdem, lOrder :: !Bool }

data World = World
  { wGi   :: !Int
  , wLat  :: Poset
  , wSel  :: [Int]
  , wLaws :: Maybe Laws
  , wIsL  :: !Bool
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-03 Join and meet" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 (galleryPoset 0) [] Nothing False []

-- 被覆の組から順序の表（閉包）を作る
galleryPoset :: Int -> Poset
galleryPoset k = let (_, labs, cv) = getAt ("", [], []) gallery k in fromCovers labs cv

gName :: Int -> String
gName k = let (s, _, _) = getAt ("", [], []) gallery k in s

-- 極小元（下にほかがない）・極大元
minimalOf, maximalOf :: Poset -> [Int] -> [Int]
minimalOf p sx = [ x | x <- sx, not (any (\y -> y /= x && leq p y x) sx) ]
maximalOf p sx = [ x | x <- sx, not (any (\y -> y /= x && leq p x y) sx) ]

-- 束の 4 つの法則を全部の組（3 つ組）で確かめる
lawCheck :: Poset -> Laws
lawCheck p0 = Laws comm assoc absorb idem order
  where
    p = tables p0
    n = pN p
    j = tJ p
    m = tM p
    xs = [0 .. n - 1]
    idem = and [ j a a == a && m a a == a | a <- xs ]
    comm = and [ j a b == j b a && m a b == m b a | a <- xs, b <- xs ]
    absorb = and [ j a (m a b) == a && m a (j a b) == a | a <- xs, b <- xs ]
    order = and [ leq p a b == (m a b == a) && leq p a b == (j a b == b) | a <- xs, b <- xs ]
    assoc = and [ j (j a b) d == j a (j b d) && m (m a b) d == m a (m b d) | a <- xs, b <- xs, d <- xs ]

lawsOk :: Laws -> Bool
lawsOk l = and [lComm l, lAssoc l, lAbsorb l, lIdem l, lOrder l]

rebuild :: Env -> World -> World
rebuild env w = w { wLat = lat, wSel = [], wIsL = isL, wLaws = if isL then Just (lawCheck lat) else Nothing }
  where lat = layoutHasse (galleryPoset (wGi w)) 160 180 (envW env * 0.5 - 160) (envH env - 320) False
        isL = isLattice lat

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = 24, oFs = 16, oCol = col }
  -- ---- 右：上界・下界の計算
  , say (gName (wGi w) ++ "  ->  " ++ (if wIsL w then "lattice" else "not a lattice")) px y0 18 (grayC 230)
  , say (if wIsL w then "(every pair has a join and a meet)" else "(some pair has no join or no meet)") px (y0 + 22) 14 (grayC 175) ]
  ++ (if two
        then [ say ("a = " ++ labOf lat s0 ++ ", b = " ++ labOf lat s1) px y1 16 white
             , say ("upper bounds (>= a and b): " ++ nm up) px (y1 + 32) 16 (rgbC 150 205 210)
             , say ("their minimal elements: " ++ nm mu) px (y1 + 58) 16 (rgbC 255 130 150)
             , say (if jn >= 0 then "join a v b = " ++ labOf lat jn ++ " (one minimal = least)"
                    else if length mu > 1 then "no join: " ++ show (length mu) ++ " minimal elements, no least"
                    else "no join: there is no upper bound") px (y1 + 84) 16 (rgbC 90 225 215)
             , say ("lower bounds (<= a and b): " ++ nm lo) px (y1 + 124) 16 (rgbC 225 195 150)
             , say ("their maximal elements: " ++ nm ml) px (y1 + 150) 16 (rgbC 255 130 150)
             , say (if mt >= 0 then "meet a ^ b = " ++ labOf lat mt
                    else if length ml > 1 then "no meet: " ++ show (length ml) ++ " maximal elements"
                    else "no meet: there is no lower bound") px (y1 + 176) 16 (rgbC 255 190 90) ]
        else [ say "Click two elements" px y1 16 (grayC 175) ])
  ++ [ say "Two views: order (<=) and algebra (v, ^)" px y2 17 (grayC 230) ]
  ++ (case wLaws w of
        Just l -> [ say ((if ok then "OK " else "NO ") ++ s) px (y2 + 30 + 26 * k) 15 (if ok then rgbC 140 225 180 else rgbC 255 110 110)
                  | (k, (s, ok)) <- zip [0 ..]
                      [ ("commutative  a v b = b v a, a ^ b = b ^ a", lComm l)
                      , ("associative  (a v b) v c = a v (b v c)", lAssoc l)
                      , ("absorption  a v (a ^ b) = a, a ^ (a v b) = a", lAbsorb l)
                      , ("idempotent  a v a = a, a ^ a = a", lIdem l)
                      , ("back to order  a <= b iff a ^ b = a iff a v b = b", lOrder l) ] ]
                  ++ [ say ("checked on all a, b, c (" ++ show n ++ "^3 = " ++ show (n * n * n) ++ " triples)") px (y2 + 170) 14 (rgbC 150 156 190) ]
        Nothing -> [ say "Some pair has no join -> v is undefined," px (y2 + 30) 15 (rgbC 255 150 150)
                   , say "so the algebraic laws make no sense" px (y2 + 54) 15 (rgbC 255 150 150) ])
  ++ [ say ("Join and meet   " ++ show (wGi w + 1) ++ " / " ++ show (length gallery) ++ ": " ++ gName (wGi w)) 20 36 18 (grayC 240)
     , say "Left/Right picture   Click pick 2   Space clear" 20 62 15 (grayC 175)
     , say "white = picked   cyan = join   orange = meet   red = minimal / maximal candidates" 20 84 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    n = pN lat
    sel = wSel w
    two = length sel == 2
    s0 = getAt 0 sel 0
    s1 = getAt 0 sel 1
    up = if two then upperOf lat s0 s1 else []
    lo = if two then lowerOf lat s0 s1 else []
    mu = minimalOf lat up
    ml = maximalOf lat lo
    jn = if two then joinOf lat s0 s1 else -1
    mt = if two then meetOf lat s0 s1 else -1
    nm sx = if null sx then "{} (none)" else "{" ++ intercalate ", " (map (labOf lat) sx) ++ "}"
    col i | i `elem` sel = Just white
          | i == jn = Just (rgbC 90 225 215)
          | i == mt = Just (rgbC 255 190 90)
          | i `elem` mu || i `elem` ml = Just (rgbC 255 110 130)
          | i `elem` up = Just (rgbC 150 205 210)
          | i `elem` lo = Just (rgbC 225 195 150)
          | otherwise = Just (rgbC 150 160 205)
    px = envW env * 0.56
    y0 = 170
    y1 = y0 + 56
    y2 = y1 + 218

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let i = nodeAt (wLat w) (toP5 env pos) 28
        s = if length (wSel w) >= 2 then [] else wSel w
    in if i < 0 then w else w { wSel = if i `elem` s then s else s ++ [i] }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wGi = (wGi w + 1) `mod` length gallery }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wGi = (wGi w + length gallery - 1) `mod` length gallery }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSel = [] }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("bow tie: upper bounds of a, b are {c, d, 1}, two minimal (c, d) -> no join, not a lattice"
    , upperOf bow 1 2 == [3, 4, 5] && length (minimalOf bow (upperOf bow 1 2)) == 2 && joinOf bow 1 2 == -1 && not (isLattice bow))
  , ("V: b and c have no upper bound at all -> not a lattice", null (upperOf vv 2 3) && not (isLattice vv))
  , ("divisors of 12, N5, M3 and the 2 x 3 grid are lattices", all (isLattice . galleryPoset) lats)
  , ("in a lattice every pair has exactly one least upper bound (join is unique)", uniq)
  , ("4 lattices, all triples: commutative, associative, absorption, idempotent, a <= b iff a ^ b = a"
    , all (lawsOk . lawCheck . galleryPoset) lats)
  , ("60 random posets: 'lattice' = every pair has one minimal upper and one maximal lower bound (" ++ show nl ++ " are lattices)"
    , cnt == 60 && nl > 0 && nl < 60) ]
  where
    bow = galleryPoset 3
    vv = galleryPoset 4
    lats = [0, 1, 2, 5]
    uniq = and [ length [ x | x <- u, all (leq q x) u ] == 1
               | k <- lats, let q = galleryPoset k, a <- allOf q, b <- allOf q, let u = upperOf q a b ]
    (cnt, nl, _) = foldl' one (0 :: Int, 0 :: Int, seedRng 3) [0 .. 59 :: Int]
    one (c, l, r) t =
      let (q, r') = randomPoset (3 + t `mod` 6) 0.5 r
          byMin = and [ length (minimalOf q (upperOf q a b)) == 1 && length (maximalOf q (lowerOf q a b)) == 1 | a <- allOf q, b <- allOf q ]
      in (if isLattice q == byMin then c + 1 else c, if byMin then l + 1 else l, r')

-- ---------- やってみよう ----------
-- 1. ちょうネクタイで a と b を選ぶ。上界は 3 つあるのに上限がないのはなぜ？
-- 2. V 字に 1 つ要素を足して束にするには、どこに足せばいい？（gallery に書き足してみよう）
-- 3. N5 で a と c を選ぶ。上限・下限はどこ？ a と b では？
-- 4. 束なら 4 つの法則が全部 OK。どれか 1 つが NO になる図は作れる？
-- 5. 2 × 3 の格子の上限は「成分ごとの max」。01 の約数の束と同じしくみ

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
