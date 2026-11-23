-- ============================================
-- 56-01 約数の束 ─ n の約数を「割り切る」で並べる
-- d | e（d が e を割り切る）は半順序。2 つの約数の上限（最小上界）は最小公倍数 lcm、下限（最大下界）は最大公約数 gcd
-- 上限・下限は順序の表から計算して、lcm・gcd と一致するかを確かめる。素因数が k 種類で平方因子がなければ k 次元の立方体
-- 操作：← → n を変える / 要素を 2 つクリック → 上限と下限 / スペース 選択を消す / L 数のかわりに素因数で表示
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

ns :: [Int]
ns = [12, 30, 36, 60, 72, 210, 360, 420]   -- ★ 選べる n

data World = World
  { wNi     :: !Int
  , wLat    :: Poset
  , wSel    :: [Int]
  , wFactor :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-01 Divisor lattice" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 3 (divisorPoset 60) [] False []

nOf :: World -> Int
nOf w = getAt 60 ns (wNi w)

rebuild :: Env -> World -> World
rebuild env w = w { wLat = layoutHasse lat1 120 170 (envW env - 520) (envH env - 290) True, wSel = [] }
  where lat0 = divisorPoset (nOf w)
        lat1 = if wFactor w then lat0 { pLab = A.listArray (0, pN lat0 - 1) (map factorName (pVal lat0)) } else lat0

-- 素因数で書いた名前（2^2*3 など）
factorName :: Int -> String
factorName d = case primeFactors d of
  [] -> "1"
  f -> intercalate "*" [ show p ++ (if e > 1 then "^" ++ show e else "") | (p, e) <- f ]

-- 約数の束 = 鎖の直積（指数 + 1 個ずつの鎖）
shapeText :: Int -> [String]
shapeText n = (intercalate " x " [ show (e + 1) | (_, e) <- f ] ++ " grid (product of chains)")
              : [ "= " ++ show (length f) ++ "-dimensional cube" | all ((== 1) . snd) f ]
  where f = primeFactors n

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = if n > 200 then 17 else 20, oFs = 14, oCol = col, oEcol = ecol }
  -- ---- 右のパネル
  , say ("n = " ++ show n ++ " = " ++ factorName n) px y0 16 (grayC 230) ]
  ++ [ say s px (y0 + 30 + 24 * k) 14 (grayC 175) | (k, s) <- zip [0 ..] panel ]
  ++ (if two
        then let a = vl s0; b = vl s1
                 l = lcmOf a b; g = gcd a b
             in [ say ("a = " ++ show a ++ ", b = " ++ show b) px y1 16 white
                , say ("join a v b = " ++ show (vl jn) ++ "  lcm = " ++ show l ++ (if vl jn == l then "  OK" else "  NO")) px (y1 + 30) 16 (rgbC 90 225 215)
                , say ("meet a ^ b = " ++ show (vl mt) ++ "  gcd = " ++ show g ++ (if vl mt == g then "  OK" else "  NO")) px (y1 + 58) 16 (rgbC 255 190 90) ]
                ++ boxText px (y1 + 90) ("upper bounds (multiples) " ++ show (length up) ++ ": " ++ unwords (map (show . vl) up))
                ++ boxText px (y1 + 140) ("lower bounds (divisors) " ++ show (length lo) ++ ": " ++ unwords (map (show . vl) lo))
                ++ [ say ("a*b = " ++ show (a * b) ++ " = lcm x gcd = " ++ show (l * g)) px (y1 + 190) 14 (grayC 175) ]
        else [ say "Click two divisors: their bounds," px y1 15 (grayC 175)
             , say "join (cyan), meet (orange)" px (y1 + 24) 15 (grayC 175) ])
  ++ [ say ("Divisor lattice   n = " ++ show n ++ "   read 'd divides e' as d <= e") 20 36 18 (grayC 240)
     , say ("Left/Right n (" ++ intercalate ", " (map show ns) ++ ")   Click pick 2   Space clear   L prime factors") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    n = nOf w
    sel = wSel w
    two = length sel == 2
    s0 = getAt 0 sel 0
    s1 = getAt 0 sel 1
    jn = if two then joinOf lat s0 s1 else -1
    mt = if two then meetOf lat s0 s1 else -1
    up = if two then upperOf lat s0 s1 else []
    lo = if two then lowerOf lat s0 s1 else []
    vl i = getAt 0 (pVal lat) i
    col i | i == jn = Just (rgbC 90 225 215)
          | i == mt = Just (rgbC 255 190 90)
          | i `elem` sel = Just (rgbC 255 255 255)
          | i `elem` up = Just (rgbC 160 215 220)
          | i `elem` lo = Just (rgbC 235 205 160)
          | otherwise = Just (rgbC 150 160 205)
    ecol a b | two && a `elem` up && b `elem` up = Just (rgbC 90 200 200)
             | two && a `elem` lo && b `elem` lo = Just (rgbC 220 160 80)
             | otherwise = Nothing
    px = envW env - 370
    y0 = 160
    y1 = y0 + 160
    panel = [ "divisors tau(n) = " ++ show (pN lat) ++ " = " ++ concat [ "(" ++ show e ++ "+1)" | (_, e) <- primeFactors n ]
            , "Hasse edges (covers) = " ++ show (length (pCov lat))
            , "height (rank) = number of prime factors"
            ] ++ shapeText n

-- 幅 350 px に折り返して書く（3 行まで）
boxText :: Float -> Float -> String -> [Picture]
boxText x y s = [ say ln x (y + 17 * k) 14 (grayC 175) | (k, ln) <- zip [0 ..] (take 3 (wrapWords 38 s)) ]

wrapWords :: Int -> String -> [String]
wrapWords m = go [] . words
  where go acc [] = [unwords (reverse acc) | not (null acc)]
        go acc (x : xs)
          | null acc = go [x] xs
          | length (unwords (reverse (x : acc))) > m = unwords (reverse acc) : go [x] xs
          | otherwise = go (x : acc) xs

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let i = nodeAt (wLat w) (toP5 env pos) 24
        s = if length (wSel w) >= 2 then [] else wSel w
    in if i < 0 then w else w { wSel = if i `elem` s then s else s ++ [i] }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wNi = (wNi w + 1) `mod` length ns }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wNi = (wNi w + length ns - 1) `mod` length ns }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSel = [] }
  EventKey (Char c) Down _ _ | c `elem` "lL" -> rebuild env w { wFactor = not (wFactor w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("divisors of 60 form a lattice of 12 elements", pN (divisorPoset 60) == 12)
  , ("n = 12, 60, 72, 210, 360, all pairs: join from the order = lcm, meet = gcd", ok2)
  , ("n = 2..120: elements = prod (e+1), covers = sum_p tau(n/p) (edges d -> d*p)", ok3)
  , ("rank (longest chain from below) = number of prime factors (with multiplicity)", ok4)
  , ("divisors of 210 = 2*3*5*7 and subsets of {a,b,c,d}: 16 elements, 32 edges, levels 1,4,6,4,1 (same shape)"
    , pN d210 == 16 && length (pCov d210) == 32 && length (pCov b4) == 32 && lv d210 == [1, 4, 6, 4, 1] && lv b4 == lv d210) ]
  where
    ok2 = and [ vl q (joinOf q a b) == lcmOf (vl q a) (vl q b) && vl q (meetOf q a b) == gcd (vl q a) (vl q b)
              | n <- [12, 60, 72, 210, 360], let q = divisorPoset n, a <- allOf q, b <- allOf q ]
    ok3 = and [ pN q == product [ e + 1 | (_, e) <- f ] && length (pCov q) == sum [ length (divisorsOf (n `div` p)) | (p, _) <- f ]
              | n <- [2 .. 120], let q = divisorPoset n, let f = primeFactors n ]
    ok4 = and [ rankOf q i == sum (map snd (primeFactors (vl q i))) | n <- [60, 360, 420], let q = divisorPoset n, i <- allOf q ]
    vl q i = getAt (-1) (pVal q) i
    d210 = divisorPoset 210
    b4 = powersetPoset 4
    lv q = [ length [ i | i <- allOf q, rankOf q i == r ] | r <- [0 .. 4] ]

-- ---------- やってみよう ----------
-- 1. n = 30 と 210 はどんな形？ 素因数が 3 個なら立方体、4 個なら 4 次元の立方体
-- 2. n = 36 = 2²·3² は 3 × 3 のます目。n = 72 は？ 形と素因数の指数の関係は？
-- 3. 2 つ選んで、上界（両方の倍数）の中のいちばん下 = lcm、下界の中のいちばん上 = gcd を確かめよう
-- 4. a・b = lcm × gcd はいつでも成り立つ？ いくつかの組で確かめよう
-- 5. ns に 64 や 105 を足してみよう。64 の約数の束はどんな形？

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
