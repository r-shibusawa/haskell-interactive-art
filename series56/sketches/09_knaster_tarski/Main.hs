-- ============================================
-- 56-09 束の不動点定理（クナスター-タルスキ）─ 単調な写像には必ず不動点があり、不動点全体も束になる
-- 有限の束 L の単調写像 f（x ≤ y なら f(x) ≤ f(y)）は、⊥ から f をくり返すと最小不動点、⊤ からくり返すと最大不動点に着く（クリーネの反復）
-- 不動点 f(x) = x を全部さがし、それだけで作った順序（右の小さな図）も束になることを確かめる。単調でないと不動点がないことも
-- 操作：← → 束を変える（部分集合・4 × 4 の格子・60 の約数）/ G ランダムな単調写像 / 1 到達集合の例（部分集合のみ）/ N 単調でない写像
--       スペース 反復を 1 歩 / A 自動で進める / R 反復をやり直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, elemIndex)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kinds :: [String]
kinds = ["subsets {a,b,c,d}", "4 x 4 grid", "divisors of 60"]

autoEvery :: Int
autoEvery = 40                -- ★ 自動のとき何フレームごとに 1 歩進めるか

data World = World
  { wKi    :: !Int
  , wLat   :: Poset            -- 束 L（上限・下限の表つき）
  , wF     :: [Int]            -- 写像 f（f !! x）
  , wMono  :: !Bool
  , wFps   :: [Int]            -- 不動点
  , wFpL   :: Maybe Poset      -- 不動点だけの順序（pVal = もとの番号）
  , wUp    :: [Int]            -- ⊥ からの反復
  , wDn    :: [Int]            -- ⊤ からの反復
  , wAuto  :: !Bool
  , wSd    :: !Word32          -- 次に使う乱数の種
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-09 Knaster-Tarski fixed points" (start0 1280 800))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (start0 (envW env) (envH env)) { wTest = selfTest }
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` autoEvery == 0 then stepIt w else w)
  , skDraw  = draw
  , skEvent = event
  }

start0 :: Float -> Float -> World
start0 wd h = randomMap wd h (build wd h (World 0 (latticeOf 0) [] True [] Nothing [] [] True 9 []))

latticeOf :: Int -> Poset
latticeOf k
  | k == 0 = tables (powersetPoset 4)
  | k == 1 = tables ((makePoset 16 (\i j -> (i .&. 3) <= (j .&. 3) && i `shiftR` 2 <= j `shiftR` 2) [ show (i .&. 3) ++ show (i `shiftR` 2) | i <- [0 .. 15 :: Int] ])
                       { pHint = Just (\i -> fromIntegral ((i .&. 3) - (i `shiftR` 2))) })
  | otherwise = tables (divisorPoset 60)

build :: Float -> Float -> World -> World
build wd h w = w { wLat = layoutHasse (latticeOf (wKi w)) 90 170 (wd * 0.52 - 90) (h - 300) True }

-- ランダムな単調写像：ランクの低い順に、f(x) ≥ （x のすぐ下の元の像の上限）となる元をランダムに選ぶ
randomMonotone :: Poset -> Rng -> ([Int], Rng)
randomMonotone lat r0 = (map (\x -> IM.findWithDefault 0 x fm) (allOf lat), r1)
  where
    ord = sortBy (comparing (rankOf lat)) (allOf lat)
    (fm, r1) = foldl' one (IM.empty, r0) ord
    one (f, r) x =
      let lb = foldl' (\l (a, b) -> if b == x then tJ lat l (IM.findWithDefault 0 a f) else l) (botOf lat) (pCov lat)
          up = [ y | y <- allOf lat, leq lat lb y ]
          (u, r') = rnd r
          k = floor (u * fromIntegral (length up) * 0.7)
      in (IM.insert x (getAt lb up k) f, r')

-- 順序を逆にする写像（単調でない）：補集合、(3 − x, 3 − y)、d → 60/d
antiMap :: Poset -> Int -> [Int]
antiMap lat k = [ if k == 0 then 15 `xor` i else if k == 1 then 15 - i else maybe (-1) id (elemIndex (60 `div` getAt 1 (pVal lat) i) (pVal lat)) | i <- allOf lat ]

-- 到達集合：f(X) = {a} ∪ （X から 1 歩で行ける所）。a→b, b→a, c→d, d→c
reachMap :: [Int]
reachMap = [ foldl' (\m (t, nx) -> if (x `shiftR` t) .&. 1 == 1 then m .|. nx else m) 1 (zip [0 ..] [2, 1, 8, 4]) | x <- [0 .. 15 :: Int] ]

fAt :: [Int] -> Int -> Int
fAt f x = getAt x f x

-- 被覆で確かめれば十分
isMonotone :: Poset -> [Int] -> Bool
isMonotone lat f = and [ okI (fAt f a) && okI (fAt f b) && leq lat (fAt f a) (fAt f b) | (a, b) <- pCov lat ]
  where okI i = i >= 0 && i < pN lat

kleene :: Poset -> [Int] -> Int -> [Int]
kleene lat f s = go 0 [s] s
  where go k acc x | k > pN lat = reverse acc
                   | otherwise = let y = fAt f x in if y == x then reverse acc else go (k + 1) (y : acc) y

lastOr :: Int -> [Int] -> Int
lastOr d xs = if null xs then d else last xs

fixedPoints :: Poset -> [Int] -> [Int]
fixedPoints lat f = [ x | x <- allOf lat, fAt f x == x ]

subPoset :: Poset -> [Int] -> Poset
subPoset lat s = (makePoset (length s) (\i j -> leq lat (sa ! i) (sa ! j)) [ labOf lat i | i <- s ]) { pVal = s }
  where sa = listArray (0, length s - 1) s :: UArray Int Int

setMap :: Float -> Float -> [Int] -> World -> World
setMap wd h f w = w { wF = f, wMono = isMonotone lat f, wFps = fps, wUp = [botOf lat], wDn = [topOf lat]
                    , wFpL = if null fps then Nothing else Just (layoutHasse (subPoset lat fps) (wd * 0.6) (h - 250) (wd * 0.32) 160 False) }
  where lat = wLat w
        fps = fixedPoints lat f

randomMap :: Float -> Float -> World -> World
randomMap wd h w = setMap wd h f w { wSd = wSd w + 1 }
  where (f, _) = randomMonotone (wLat w) (seedRng (wSd w))

stepIt :: World -> World
stepIt w = w { wUp = ext (wUp w), wDn = ext (wDn w) }
  where n = pN (wLat w)
        ext xs = let u = lastOr 0 xs
                     v = fAt (wF w) u
                 in if v /= u && length xs <= n then xs ++ [v] else xs

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 写像の矢印 x → f(x)（うすく）
  [ Color (rgbaC 255 255 255 40) (Line [p, (qx + (fst p - qx) * 0.12, qy + (snd p - qy) * 0.12)])
  | x <- allOf lat, let y = fAt (wF w) x, x /= y, let p = posOf lat x, let (qx, qy) = posOf lat y ]
  -- ---- 反復の道すじ（⊥ から = 水色、⊤ から = 橙）
  ++ [ Color (rgbaC 90 225 215 200) (thickPath 5 (map (posOf lat) up)) | length up > 1 ]
  ++ [ Color (rgbaC 255 190 90 200) (thickPath 5 (map (posOf lat) dn)) | length dn > 1 ]
  ++ [ drawHasse lat hopt { oRad = 20, oFs = 14, oCol = \i -> Just (if i == lastOr (-1) up then rgbC 90 225 215 else if i == lastOr (-1) dn then rgbC 255 190 90
                                                                    else if i `elem` fps then rgbC 255 120 170 else rgbC 150 160 205) } ]
  ++ [ Color (rgbC 255 120 170) (at (posOf lat x) (ring 26 2)) | x <- fps ]
  -- ---- 右のパネル
  ++ [ say ("f monotone? " ++ (if wMono w then "OK yes" else "NO (some x <= y has f(x) not <= f(y))")) px y0 17 (if wMono w then rgbC 140 225 180 else rgbC 255 130 140) ]
  ++ wrapAt px (y0 + 36) 16 20 (rgbC 90 225 215) boxW ("from bottom: " ++ chain up ++ (if fAt (wF w) (lastOr 0 up) == lastOr 0 up then "  = least fixed point" else " ..."))
  ++ wrapAt px (y0 + 90) 16 20 (rgbC 255 190 90) boxW ("from top: " ++ chain dn ++ (if fAt (wF w) (lastOr 0 dn) == lastOr 0 dn then "  = greatest fixed point" else " ..."))
  ++ wrapAt px (y0 + 146) 16 20 (rgbC 255 120 170) boxW ("fixed points f(x) = x: " ++ (if null fps then "none" else show (length fps) ++ ":  " ++ intercalate ", " (map nm fps)))
  ++ (case wFpL w of
        Just fl ->
          [ say ("Fixed points alone form a lattice? " ++ (if isLattice fl then "OK yes (Knaster-Tarski)" else "NO")) px (y0 + 196) 15 (grayC 200) ]
          ++ (case [ (i, j, a, b) | i <- allOf fl, j <- allOf fl, let a = getAt (-1) (pVal fl) (joinOf fl i j), let b = tJ lat (src fl i) (src fl j), a /= b ] of
                ((i, j, a, b) : _) -> wrapAt px (y0 + 220) 15 18 (grayC 200) boxW ("Note: join in the fixed points " ++ labOf fl i ++ " v " ++ labOf fl j ++ " = " ++ nm a
                                                                                  ++ " (the join " ++ nm b ++ " in L is not a fixed point)")
                [] -> [])
          ++ [ drawHasse fl hopt { oRad = 20, oFs = 14, oCol = const (Just (rgbC 255 120 170)) } ]
        Nothing -> [ say "A non-monotone map may have no fixed point (order-reversing)" px (y0 + 196) 15 (grayC 200) | not (wMono w) ])
  ++ [ say ("Fixed point theorem   " ++ getAt "" kinds (wKi w) ++ "   faint line = x -> f(x), pink ring = fixed point") 20 36 18 (grayC 240)
     , say ("Left/Right lattice  G random monotone map  1 reachability  N non-monotone  Space step  A auto " ++ (if wAuto w then "ON" else "OFF") ++ "  R restart") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    up = wUp w
    dn = wDn w
    fps = wFps w
    px = envW env * 0.57
    y0 = 160
    boxW = envW env - px - 20
    nm i = labOf lat i
    chain xs = intercalate " -> " (map nm xs)
    src fl i = getAt (-1) (pVal fl) i

-- 幅 boxW に収まるように単語で折り返して書く（行の間は lh）
wrapAt :: Float -> Float -> Float -> Float -> Color -> Float -> String -> [Picture]
wrapAt x y size lh c boxW s = [ say l x (y + lh * fromIntegral k) size c | (k, l) <- zip [0 :: Int ..] (wrapText (floor (boxW / (size * 0.65))) s) ]

wrapText :: Int -> String -> [String]
wrapText mx s = reverse (go (splitSp s) [] [])
  where
    go [] cur acc = if null cur then acc else cur : acc
    go (t : ts) cur acc
      | null cur = go ts t acc
      | length cur + 1 + length t <= mx = go ts (cur ++ " " ++ t) acc
      | otherwise = go ts t (cur : acc)
    splitSp str = case break (== ' ') str of
      (a, []) -> [a | not (null a)]
      (a, _ : rest) -> (if null a then id else (a :)) (splitSp rest)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> randomMap wd h (build wd h w { wKi = (wKi w + 1) `mod` 3 })
  EventKey (SpecialKey KeyLeft) Down _ _ -> randomMap wd h (build wd h w { wKi = (wKi w + 2) `mod` 3 })
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> randomMap wd h w
    | c == '1' -> setMap wd h reachMap (build wd h w { wKi = 0 })
    | c `elem` "nN" -> setMap wd h (antiMap (wLat w) (wKi w)) w
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> w { wUp = [botOf (wLat w)], wDn = [topOf (wLat w)] }
  EventKey (SpecialKey KeySpace) Down _ _ -> stepIt w
  _ -> w
  where wd = envW env
        h = envH env

-- ---- 自己テスト（p5.js 版と同じ 7 項目、同じ種 90）
selfTest :: [(String, Bool)]
selfTest =
  [ ("60 random monotone maps (3 kinds of lattice): monotone by construction", all (\(m, _, _, _, _) -> m) rs)
  , ("iterating f from bottom reaches a fixed point below every fixed point (least fixed point)", all (\(_, a, _, _, _) -> a) rs)
  , ("iterating from top reaches the greatest fixed point", all (\(_, _, b, _, _) -> b) rs)
  , ("the fixed points are non-empty and form a lattice by themselves (Knaster-Tarski)", all (\(_, _, _, c, _) -> c) rs)
  , ("least fixed point = meet{x | f(x) <= x} (Tarski's formula)", all (\(_, _, _, _, d) -> d) rs)
  , ("order-reversing maps (complement, (3-x, 3-y), d -> 60/d) are not monotone and have no fixed point", ns)
  , ("reachability: least fixed point = {a, b} (reachable from a), fixed points are {a,b} and {a,b,c,d}"
    , lastOr (-1) (kleene p0 reachMap 0) == 3 && fixedPoints p0 reachMap == [3, 15]) ]
  where
    lats = map latticeOf [0, 1, 2]
    rs = reverse (fst (foldl' one ([], seedRng 90) [0 .. 59 :: Int]))
    one (acc, r) t =
      let lat = getAt (latticeOf 0) lats (t `mod` 3)
          (f, r') = randomMonotone lat r
          fs = fixedPoints lat f
          lo = lastOr (-1) (kleene lat f (botOf lat))
          hi = lastOr (-1) (kleene lat f (topOf lat))
          okLo = fAt f lo == lo && all (leq lat lo) fs
          okHi = fAt f hi == hi && all (\x -> leq lat x hi) fs
          okLat = not (null fs) && isLattice (subPoset lat fs)
          pre = [ x | x <- allOf lat, leq lat (fAt f x) x ]                 -- タルスキの式：最小不動点 = ∧{x | f(x) ≤ x}
          m = foldl' (tM lat) (topOf lat) pre
      in ((isMonotone lat f, okLo, okHi, okLat, m == lo) : acc, r')
    ns = and [ let lat = getAt (latticeOf 0) lats k
                   f = antiMap lat k
               in not (isMonotone lat f) && null (fixedPoints lat f) | k <- [0, 1, 2] ]
    p0 = getAt (latticeOf 0) lats 0

-- ---------- やってみよう ----------
-- 1. G を何回か押す。水色（⊥ から）と橙（⊤ から）の道はいつも不動点で止まる？ 何歩かかる？
-- 2. 不動点が 1 つだけの写像、たくさんある写像を見つけよう。右の小さな図はいつも束？
-- 3. N で順序を逆にする写像。反復はどうなる？（ぐるぐる回って止まらない）
-- 4. 1 の到達集合：プログラムの「どこまで行けるか」も最小不動点（10 のデータフロー解析）
-- 5. 「注意」が出たら、不動点の束の上限と、もとの束の上限がちがう例。なぜ？


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
