-- ============================================
-- 56-02 部分集合の束 ─ {a, b, c, d} の部分集合 16 個を「含まれる ⊆」で並べる（4 次元の立方体）
-- 上限 = 和集合 X ∪ Y、下限 = 共通部分 X ∩ Y。ビット列で書くと ∪ は OR、∩ は AND（1 ビットずつ）
-- 各段の個数は二項係数 C(k, r)、ハッセ図の辺は k·2^(k−1) 本（1 要素ずつ足す）
-- 操作：↑↓ 要素の数 k（2〜5）/ 部分集合を 2 つクリック → 和集合・共通部分 / B 集合 ⇔ ビット列 / スペース 選択を消す
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

kStart :: Int
kStart = 4                     -- ★ はじめの要素の数

data World = World
  { wK      :: !Int
  , wLat    :: Poset
  , wSel    :: [Int]
  , wBits   :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-02 Subset lattice" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World kStart (powersetPoset kStart) [] False []

-- ビット列（d c b a の順）
bitsOf :: Int -> Int -> String
bitsOf m k = [ if (m `shiftR` t) .&. 1 == 1 then '1' else '0' | t <- [k - 1, k - 2 .. 0] ]

rebuild :: Env -> World -> World
rebuild env w = w { wLat = layoutHasse lat1 100 170 (envW env - 500) (envH env - 290) (k <= 4), wSel = [] }
  where k = wK w
        lat0 = powersetPoset k
        lat1 = if wBits w then lat0 { pLab = A.listArray (0, pN lat0 - 1) [ bitsOf m k | m <- pVal lat0 ] } else lat0

binom :: Int -> Int -> Int
binom n r = foldl' (\c i -> c * (n - r + i) `div` i) 1 [1 .. r]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = if k == 5 then 15 else 19, oFs = 14, oCol = col, oEcol = ecol }
  -- ---- 右のパネル
  , say ("Set {" ++ intercalate ", " [ [c] | c <- take k "abcde" ] ++ "}   2^" ++ show k ++ " = " ++ show (pN lat) ++ " subsets") px y0 16 (grayC 230)
  , say ("Hasse edges " ++ show (length (pCov lat)) ++ " (k*2^(k-1) = " ++ show (k * 2 ^ (k - 1)) ++ ")") px (y0 + 28) 14 (grayC 175)
  , say ("levels " ++ intercalate ", " (map show lv) ++ " = C(" ++ show k ++ ", r)") px (y0 + 52) 14 (grayC 175)
  , say "bits: right to left a, b, c, ... (1 = in)" px (y0 + 76) 14 (grayC 175) ]
  ++ (if two
        then [ say ("X = " ++ setName x k ++ " (" ++ bitsOf x k ++ ")") px y1 16 white
             , say ("Y = " ++ setName y k ++ " (" ++ bitsOf y k ++ ")") px (y1 + 26) 16 white
             , say ("join X v Y = " ++ labOf lat jn ++ "   X u Y = " ++ setName (x .|. y) k ++ okS (vl jn == x .|. y)) px (y1 + 60) 16 cyan
             , say ("  bit OR: " ++ bitsOf x k ++ " | " ++ bitsOf y k ++ " = " ++ bitsOf (x .|. y) k) px (y1 + 84) 16 cyan
             , say ("meet X ^ Y = " ++ labOf lat mt ++ "   X n Y = " ++ setName (x .&. y) k ++ okS (vl mt == x .&. y)) px (y1 + 116) 16 orange
             , say ("  bit AND: " ++ bitsOf x k ++ " & " ++ bitsOf y k ++ " = " ++ bitsOf (x .&. y) k) px (y1 + 140) 16 orange
             , say ("|X u Y| + |X n Y| = |X| + |Y|: " ++ show (popc (x .|. y) + popc (x .&. y)) ++ " = " ++ show (popc x + popc y)) px (y1 + 174) 14 (grayC 175) ]
        else [ say "Click two: union (cyan)" px y1 15 (grayC 175)
             , say "and intersection (orange)" px (y1 + 24) 15 (grayC 175)
             , say "Hover: all its subsets (purple)" px (y1 + 48) 15 (grayC 175) ])
  ++ [ say ("Subset lattice   k = " ++ show k ++ "   X <= Y iff X is a subset of Y   " ++ show k ++ "-dimensional cube") 20 36 18 (grayC 240)
     , say ("Up/Down k (2-5)   Click pick 2   B labels: " ++ (if wBits w then "bit strings" else "sets") ++ "   Space clear") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    k = wK w
    sel = wSel w
    two = length sel == 2
    s0 = getAt 0 sel 0
    s1 = getAt 0 sel 1
    jn = if two then joinOf lat s0 s1 else -1
    mt = if two then meetOf lat s0 s1 else -1
    hov = nodeAt lat (toP5 env (envMouse env)) 18
    vl i = getAt (-1) (pVal lat) i
    x = vl s0
    y = vl s1
    cyan = rgbC 90 225 215
    orange = rgbC 255 190 90
    okS b = if b then " OK" else " NO"
    col i | i == jn = Just cyan
          | i == mt = Just orange
          | i `elem` sel = Just white
          | hov >= 0 && i .&. vl hov == i = Just (rgbC 190 175 240)
          | otherwise = Just (rgbC 150 160 205)
    ecol _ b = if hov >= 0 && b .&. vl hov == b then Just (rgbC 170 150 240) else Nothing
    px = envW env - 370
    y0 = 160
    y1 = y0 + 130
    lv = [ length [ i | i <- allOf lat, rankOf lat i == r ] | r <- [0 .. k] ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let i = nodeAt (wLat w) (toP5 env pos) 22
        s = if length (wSel w) >= 2 then [] else wSel w
    in if i < 0 then w else w { wSel = if i `elem` s then s else s ++ [i] }
  EventKey (SpecialKey KeyUp) Down _ _ | wK w < 5 -> rebuild env w { wK = wK w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wK w > 2 -> rebuild env w { wK = wK w - 1 }
  EventKey (Char c) Down _ _ | c `elem` "bB" -> rebuild env w { wBits = not (wBits w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSel = [] }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("subsets of 4 elements: 16 subsets, 32 cover edges", pN q4 == 16 && length (pCov q4) == 32)
  , ("k = 1..5: edges = k*2^(k-1), level r has C(k, r) elements", ok2)
  , ("k = 3, 4, 5, all pairs: join from the order = OR (union), meet = AND (intersection)", ok3)
  , ("cover X < Y iff Y adds exactly one element to X", and [ popc b == popc a + 1 && a .&. b == a | (a, b) <- pCov q4 ])
  , ("bottom = {}, top = whole set, it is a lattice", botOf q4 == 0 && topOf q4 == 15 && isLattice q4) ]
  where
    q4 = powersetPoset 4
    ok2 = and [ length (pCov q) == k * 2 ^ (k - 1) && and [ length [ i | i <- allOf q, rankOf q i == r ] == binom k r | r <- [0 .. k] ]
              | k <- [1 .. 5], let q = powersetPoset k ]
    ok3 = and [ joinOf q a b == a .|. b && meetOf q a b == a .&. b | k <- [3, 4, 5], let q = powersetPoset k, a <- allOf q, b <- allOf q ]

-- ---------- やってみよう ----------
-- 1. k = 2 は正方形、k = 3 は立方体。k = 4 の図のどこに立方体が 2 つ見える？（d が入っている・いない）
-- 2. 2 つ選んで、ビット列の OR と AND が和集合と共通部分になることを確かめよう
-- 3. マウスをのせると、その部分集合の部分集合（下にある全部）が紫になる。いくつある？（2^|X|）
-- 4. 01 の n = 210 と見比べよう。どの素数がどの要素に対応する？
-- 5. 真ん中の段がいちばん多いのはなぜ？（12 のスペルナーの定理につながる）

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
