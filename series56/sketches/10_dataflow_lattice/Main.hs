-- ============================================
-- 56-10 プログラム解析の束 ─ 「到達する定義」を束の上の不動点反復で求める
-- 各ノードの値は 定義 {d1, …, d6} の部分集合（2^6 = 64 個の束、⊥ = ∅）。OUT[n] = gen[n] ∪ (IN[n] − kill[n])、IN[n] = ∪ OUT[前のノード]
-- ⊥ から全ノードを順にくり返し更新すると、値は束を上へ上へと上がり、変わらなくなった所が最小不動点（09 のクリーネの反復）
-- 操作：スペース 1 周 / A 自動 / R ⊥ にもどす / ノードをクリック → そのノードの OUT が束を上っていく道を右に表示
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!), (//), elems)
import Data.Bits (complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ [コード, 定義の番号（なければ −1）, 変数, x, y]
nodesT :: [(String, Int, String, Float, Float)]
nodesT =
  [ ("START", -1, "", 0.5, 0), ("d1: i = 0", 0, "i", 0.5, 1), ("d2: s = 0", 1, "s", 0.5, 2), ("while (i < 10)", -1, "", 0.5, 3)
  , ("if (i % 3 == 0)", -1, "", 0.3, 4), ("d3: s = s + i", 2, "s", 0.1, 5), ("d4: s = s - 1", 3, "s", 0.5, 5), ("d5: i = i + 1", 4, "i", 0.3, 6)
  , ("d6: t = s * 2", 5, "t", 0.85, 4), ("print(t)", -1, "", 0.85, 5) ]

edgesT :: [(Int, Int)]
edgesT = [(0, 1), (1, 2), (2, 3), (3, 4), (3, 8), (4, 5), (4, 6), (5, 7), (6, 7), (7, 3), (8, 9)]

nNodes, nDefs :: Int
nNodes = length nodesT
nDefs = 6

type Vals = UArray Int Int     -- ノードごとの定義の集合（ビット）

data World = World
  { wIn, wOut :: !Vals
  , wHist     :: [Vals]        -- 1 周ごとの OUT（古い順）
  , wRounds   :: !Int
  , wDone     :: !Bool
  , wAuto     :: !Bool
  , wSel      :: !Int
  , wPS       :: Poset
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-10 Dataflow lattice" (world0 env1))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 env) { wTest = selfTest }
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 50 == 0 then stepIt w else w)
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: Env -> World
world0 env = resetIt (World zeros zeros [] 0 False True 6 ps [])
  where ps0 = powersetPoset nDefs
        ps1 = ps0 { pLab = fmap (const "") (pLab ps0) }
        ps = layoutHasse ps1 (envW env * 0.6) 200 (envW env * 0.37) (envH env - 330) False

zeros :: Vals
zeros = listArray (0, nNodes - 1) (replicate nNodes 0)

nodeAtT :: Int -> (String, Int, String, Float, Float)
nodeAtT n = getAt ("", -1, "", 0, 0) nodesT n

codeOf :: Int -> String
codeOf n = let (c, _, _, _, _) = nodeAtT n in c

varOf :: Int -> String
varOf n = let (_, _, v, _, _) = nodeAtT n in v

-- gen = そのノードの定義、kill = 同じ変数のほかの定義
genK :: [(Int, Int)]
genK = [ (if d >= 0 then shiftL 1 d else 0, foldl' (.|.) 0 [ shiftL 1 d2 | (_, d2, v2, _, _) <- nodesT, d >= 0, d2 >= 0, v2 == v, d2 /= d ])
       | (_, d, v, _, _) <- nodesT ]

genOf, killOf :: Int -> Int
genOf n = fst (getAt (0, 0) genK n)
killOf n = snd (getAt (0, 0) genK n)

preds :: Int -> [Int]
preds n = [ a | (a, b) <- edgesT, b == n ]

succs :: Int -> [Int]
succs n = [ b | (a, b) <- edgesT, a == n ]

-- 1 周：全ノードを順に更新（前のノードの新しい値をすぐ使う）。変わったら True
roundRobin :: Vals -> Vals -> (Bool, Vals, Vals)
roundRobin i0 o0 = foldl' upd (False, i0, o0) [0 .. nNodes - 1]
  where upd (ch, ii, oo) n =
          let i = foldl' (.|.) 0 [ oo ! p | p <- preds n ]
              o = genOf n .|. (i .&. complement (killOf n))
          in (ch || i /= ii ! n || o /= oo ! n, ii // [(n, i)], oo // [(n, o)])

-- start から変わらなくなるまでくり返す（周の数 k も返す）
solve :: Int -> (Vals, Vals, Int)
solve start = go (listArray (0, nNodes - 1) (replicate nNodes start)) (listArray (0, nNodes - 1) (replicate nNodes start)) 0
  where go i o k = let (ch, i', o') = roundRobin i o
                   in if ch && k < 100 then go i' o' (k + 1) else (i', o', k)

resetIt :: World -> World
resetIt w = w { wIn = zeros, wOut = zeros, wHist = [zeros], wRounds = 0, wDone = False }

stepIt :: World -> World
stepIt w
  | wDone w = w
  | otherwise = let (ch, i, o) = roundRobin (wIn w) (wOut w)
                in if ch then w { wIn = i, wOut = o, wRounds = wRounds w + 1, wHist = wHist w ++ [o] }
                         else w { wIn = i, wOut = o, wDone = True }

defName :: Int -> String
defName m = case [ 'd' : show (d + 1) | d <- [0 .. nDefs - 1], testBit m d ] of
  [] -> "{}"
  s -> "{" ++ intercalate "," s ++ "}"

defVar :: Int -> String
defVar d = case [ v | (_, d2, v, _, _) <- nodesT, d2 == d ] of { (v : _) -> v; [] -> "" }

varCol :: String -> Color
varCol v = case v of
  "i" -> rgbC 90 225 215
  "s" -> rgbC 255 190 90
  _ -> rgbC 255 120 170

-- フローグラフの箱の位置（左上）
boxW, boxH :: Float
boxW = 170
boxH = 34

boxAt :: Env -> Int -> (Float, Float)
boxAt env n = let (_, _, _, x, y) = nodeAtT n
                  gw = envW env * 0.52
                  gh = (envH env - 200) / 7
              in (30 + x * (gw - boxW), 120 + y * gh)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- フローグラフ
  [ Color (rgbC 110 120 180) $
      if a == 7 && b == 3
        then let (x1, y1) = boxAt env a
                 (x2, y2) = boxAt env b
             in thickPath 2 [(x1, y1 + boxH / 2), (x1 - 50, y1 + boxH / 2), (x1 - 50, y2 + boxH / 2), (x2, y2 + boxH / 2)]   -- ループの戻り
        else let (x1, y1) = boxAt env a
                 (x2, y2) = boxAt env b
             in thickLine 2 (x1 + boxW / 2, y1 + boxH) (x2 + boxW / 2, y2)
  | (a, b) <- edgesT ]
  ++ concat [ let (x, y) = boxAt env n
                  selN = n == wSel w
              in [ Color (rgbC 34 38 80) (rectTL x y boxW boxH)
                 , Color (if selN then white else rgbC 90 100 160) (frameTL (if selN then 3 else 1) x y boxW boxH)
                 , say (codeOf n) (x + 8) (y + 22) 14 (grayC 235) ]
                 ++ concat [ [ Color (varCol (defVar d)) (rectTL (x + 4 + fromIntegral d * 27) (y + boxH + 3) 25 18)
                             , say ('d' : show (d + 1)) (x + 7 + fromIntegral d * 27) (y + boxH + 18) 14 bgCol ]
                           | d <- [0 .. nDefs - 1], testBit (wOut w ! n) d ]
            | n <- [0 .. nNodes - 1] ]
  -- ---- 右：2^6 の束と、選んだノードの OUT の道
  ++ [ Color (rgbaC 255 255 255 60) (Line [posOf ps a, posOf ps b]) | (a, b) <- pCov ps ]
  ++ [ Color (if i `elem` path then rgbC 255 220 120 else rgbC 110 120 170) (at (posOf ps i) (disc (if Just i == lastP then 8 else 4))) | i <- allOf ps ]
  ++ [ Color (rgbC 255 220 120) (thickLine 3 (posOf ps p) (posOf ps q)) | (p, q) <- zip path (drop 1 path), p /= q ]
  ++ [ say ("Lattice 2^6: OUT of '" ++ codeOf (wSel w) ++ "' going up") (envW env * 0.6) 150 15 (grayC 220)
     , sayWrap (intercalate " -> " steps) (envW env * 0.6) 174 (envW env * 0.38) 15 (rgbC 255 220 120)
  -- ---- 文字
     , say ("Dataflow lattice: reaching definitions   round " ++ show (wRounds w)
            ++ (if wDone w then "  -> stable = least fixed point (" ++ show (wRounds w) ++ " rounds)" else "")) 20 36 18 (grayC 240)
     , say ("Space 1 round   A auto " ++ (if wAuto w then "ON" else "OFF") ++ "   R reset to bottom   Click a node -> its path   (chips = OUT: i cyan, s orange, t pink)") 20 62 15 (grayC 175)
     , say ("Which defs of s reach print(t)? Look at IN of d6: " ++ defName (wIn w ! 8) ++ "   (d2, d3, d4 can all reach)") 20 88 14 (grayC 200)
     , drawSelfTest env (wTest w) ]
  where
    ps = wPS w
    path = [ h ! wSel w | h <- wHist w ]
    lastP = case reverse path of { (p : _) -> Just p; [] -> Nothing }
    names = map defName path
    steps = [ s | (k, s) <- zip [0 :: Int ..] names, k == 0 || Just s /= lookup (k - 1) (zip [0 ..] names) ]

-- p5.js の text(s, x, y, w)：幅 w で折り返す（y は 1 行目の上）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y w size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.25) size c | (k, l) <- zip [0 :: Int ..] (wrapWords (max 8 (floor (w / (size * 0.65)))) s) ]

wrapWords :: Int -> String -> [String]
wrapWords m s = reverse (map unwords' (go (words s) [] []))
  where
    go [] cur acc = if null cur then acc else reverse cur : acc
    go (x : xs) cur acc
      | null cur = go xs [x] acc
      | length (unwords (reverse (x : cur))) <= m = go xs (x : cur) acc
      | otherwise = go xs [x] (reverse cur : acc)
    unwords' = unwords

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        hit = [ n | n <- [0 .. nNodes - 1], let (x, y) = boxAt env n, mx > x, mx < x + boxW, my > y, my < y + boxH ]
    in case reverse hit of { (n : _) -> w { wSel = n }; [] -> w }
  EventKey (SpecialKey KeySpace) Down _ _ -> stepIt w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> resetIt w
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("from bottom the iteration stops after " ++ show k ++ " rounds and OUT = gen U (IN - kill) holds everywhere (fixed point)", fix && k < 10)
  , ("iteration IN = answer of the path search (all nodes)", all (\n -> sI ! n == refOf n) [0 .. nNodes - 1])
  , ("worklist method (different update order) gives the same answer", elems i2 == elems sI && elems o2 == elems sO)
  , ("each round every node's value goes up the lattice (monotone)", incr && wDone wEnd)
  , ("starting from top (all defs) also reaches a fixed point, above the least one"
    , and [ (sO ! n .&. tO ! n) == sO ! n | n <- [0 .. nNodes - 1] ])
  , ("example: defs of s reaching d6 (t = s * 2) are d2, d3, d4 (and d1, d5 of i)", sI ! 8 == 31) ]
  where
    (sI, sO, k) = solve 0
    fix = and [ let i = foldl' (.|.) 0 [ sO ! p | p <- preds n ] in sI ! n == i && sO ! n == (genOf n .|. (i .&. complement (killOf n))) | n <- [0 .. nNodes - 1] ]
    -- 確かめ用：定義のノードから、同じ変数を定義し直すノードを通らずに行けるか（道の探索）
    refList = [ (m, shiftL 1 d) | (dn, (_, d, v, _, _)) <- zip [0 ..] nodesT, d >= 0, m <- bfs dn v ]
    refOf n = foldl' (.|.) 0 [ b | (m, b) <- refList, m == n ]
    bfs dn v = go (succs dn) []
      where go [] seen = seen
            go (m : q) seen
              | m `elem` seen = go q seen
              | m /= dn && varOf m == v = go q (seen ++ [m])
              | otherwise = go (q ++ succs m) (seen ++ [m])
    -- ワークリスト法（逆順から。先頭がスタックの上）
    (i2, o2) = wl [0 .. nNodes - 1] zeros zeros (0 :: Int)
    wl [] i o _ = (i, o)
    wl (n : rest) i o cnt
      | cnt >= 1000 = (i, o)
      | otherwise =
          let iv = foldl' (.|.) 0 [ o ! p | p <- preds n ]
              ov = genOf n .|. (iv .&. complement (killOf n))
              i' = i // [(n, iv)]
          in if ov /= o ! n
               then wl (foldl' (\st e -> if e `elem` st then st else e : st) rest (succs n)) i' (o // [(n, ov)]) (cnt + 1)
               else wl rest i' o (cnt + 1)
    -- 単調性
    (incr, wEnd) = foldl' (\(ok, w) _ -> let w' = stepIt w
                                             good = and [ (wOut w ! n .&. wOut w' ! n) == wOut w ! n | n <- [0 .. nNodes - 1] ]
                                         in (ok && good, w')) (True, resetIt (World zeros zeros [] 0 False True 6 (powersetPoset 1) [])) [1 .. 20 :: Int]
    (_, tO, _) = solve (shiftL 1 nDefs - 1)

-- ---------- やってみよう ----------
-- 1. スペースで 1 周ずつ進めよう。ループ（while）のせいで、2 周目に d5 が while に届く
-- 2. 「d4: s = s - 1」をクリック。その OUT は束の上でどう上っていく？
-- 3. edgesT から (7, 3) を消す（ループをなくす）と、何周で止まる？
-- 4. 定数伝播：値の束を「⊥ < 0, 1, 2, … < ⊤（わからない）」にすると、同じ反復で定数が見つかる
-- 5. 型推論や最適化も、束と不動点で考えられる


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
