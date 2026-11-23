-- ============================================
-- 56-11 鎖と反鎖 ─ 最長の鎖と最大の反鎖を計算して色づけ
-- 鎖 = どの 2 つも比べられる部分（一列に並ぶ）、反鎖 = どの 2 つも比べられない部分（横に並ぶ）
-- ミルスキーの定理：最長の鎖の長さ h = 反鎖に分けるときの最小の個数（ランクの段ごとに分ければちょうど h 個）
-- 操作：← → 半順序を変える / G 新しいランダムな半順序 / C 最長の鎖 / A 最大の反鎖 / L 段（反鎖への分割）の色 / 要素にマウス → 比べられる元
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (complement, countTrailingZeros, popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, elemIndex)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kinds :: [String]
kinds = ["random poset (G)", "subsets of {a,b,c,d}", "divisors of 60", "divisors of 72", "divisors of 120", "divisors of 180"]

data World = World
  { wKi    :: !Int
  , wSeed  :: !Int               -- ★ ランダムな半順序の種（G で 1 ずつ増える）
  , wLat   :: Poset
  , wChain :: [Int]              -- 最長の鎖（下から）
  , wAnti  :: [Int]              -- 最大の反鎖
  , wShowC, wShowA, wShowL :: !Bool
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-11 Chains and antichains" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 11 (powersetPoset 1) [] [] True True True []

posetOf :: Int -> Int -> Poset
posetOf seedN k
  | k == 0 = let (u, r1) = rnd (seedRng (fromIntegral seedN))
                 (q, _) = randomPoset (10 + floor (u * 7)) 0.22 r1
             in q { pLab = A.listArray (A.bounds (pLab q)) [ [c] | c <- take (pN q) "abcdefghijklmnopq" ] }
  | k == 1 = powersetPoset 4
  | otherwise = divisorPoset (getAt 60 [0, 0, 60, 72, 120, 180] k)

-- ランク（下からの最長の鎖）が最大の元から、ランクが 1 ずつ下がる被覆をたどる
longestChain :: Poset -> [Int]
longestChain lat = reverse (go x0)
  where
    h = maximum (0 : [ rankOf lat i | i <- allOf lat ])
    x0 = case [ i | i <- allOf lat, rankOf lat i == h ] of { (i : _) -> i; [] -> 0 }
    go x | rankOf lat x <= 0 = [x]
         | otherwise = case [ a | (a, b) <- pCov lat, b == x, rankOf lat a == rankOf lat x - 1 ] of
             (a : _) -> x : go a
             [] -> [x]

-- 比べられる元のビット（自分も入る）
compMasks :: Poset -> UArray Int Int
compMasks lat = listArray (0, pN lat - 1) [ foldl' (\m j -> if leq lat i j || leq lat j i then m .|. shiftL 1 j else m) 0 (allOf lat) | i <- allOf lat ]

-- 比べられない元の最大の集まり（分枝限定法：v を入れる／入れない）
maxAntichain :: Poset -> [Int]
maxAntichain lat = [ i | i <- allOf lat, testBit bestSet i ]
  where
    comp = compMasks lat
    (_, bestSet) = go (shiftL 1 (pN lat) - 1) 0 0 (0, 0)
    go :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
    go cand cur k st@(best, _)
      | k + popCount cand <= best = st
      | cand == 0 = (k, cur)
      | otherwise =
          let v = countTrailingZeros cand
              st1 = go (cand .&. complement (comp ! v)) (cur .|. shiftL 1 v) (k + 1) st   -- v を入れる → v と比べられる元は使えない
          in go (cand .&. complement (shiftL 1 v)) cur k st1                              -- v を入れない

isAntichain :: Poset -> [Int] -> Bool
isAntichain lat sx = and [ a == b || (not (leq lat a b) && not (leq lat b a)) | a <- sx, b <- sx ]

-- 部分集合（ビット）が反鎖か
isAntiMask :: UArray Int Int -> Int -> Int -> Bool
isAntiMask comp n s = and [ comp ! i .&. s == shiftL 1 i | i <- [0 .. n - 1], testBit s i ]

rebuild :: Env -> World -> World
rebuild env w = w { wLat = lat, wChain = longestChain lat, wAnti = maxAntichain lat }
  where lat = layoutHasse (posetOf (wSeed w) (wKi w)) 100 170 (envW env * 0.55 - 100) (envH env - 300) (wKi w >= 1)

lcol :: Int -> Color
lcol r = let (a, b, c) = getAt (70, 60, 120) [(70, 60, 120), (50, 80, 120), (60, 100, 90), (100, 80, 60), (100, 60, 90), (60, 70, 130), (90, 100, 60), (70, 90, 120)] (r `mod` 8)
         in rgbaC a b c 120

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 段（ランク）＝ 反鎖への分割（ミルスキー）
  (if wShowL w
     then concat [ case [ snd (posOf lat i) | i <- allOf lat, rankOf lat i == r ] of
                     (y : _) -> [ Color (lcol r) (roundTL 70 (y - 24) (envW env * 0.55 - 40) 48 24)
                                , say ("lv " ++ show (r + 1)) 14 (y + 5) 14 (grayC 200) ]
                     [] -> []
                 | r <- [0 .. h - 1] ]
     else [])
  ++ [ drawHasse lat hopt { oRad = 18, oFs = 14, oCol = col, oEcol = ecol }
  -- ---- 右のパネル
     , say (getAt "" kinds (wKi w) ++ "   elements " ++ show (pN lat)) px y0 18 (grayC 230)
     , sayWrap ("Longest chain (cyan): length " ++ show (length ch) ++ "   " ++ intercalate " < " (map (labOf lat) ch)) px (y0 + 40) (envW env - px - 20) 16 (rgbC 90 225 215)
     , sayWrap ("Max antichain (orange): size " ++ show (length an) ++ "   {" ++ intercalate ", " (map (labOf lat) an) ++ "}") px (y0 + 96) (envW env - px - 20) 16 (rgbC 255 190 90)
     , say ("Per level: " ++ intercalate ", " (map show levels) ++ " (" ++ show h ++ " levels, all antichains " ++ ok levOk ++ ")") px (y0 + 152) 16 (grayC 220) ]
  ++ [ say s px (y0 + 196 + 24 * fromIntegral k) 15 (grayC 200)
     | (k, s) <- zip [0 :: Int ..]
         [ "Mirsky's theorem:"
         , "  longest chain " ++ show (length ch) ++ " = min number of antichains " ++ show h ++ "  " ++ ok (length ch == h)
         , "  (a chain meets an antichain at most once)"
         , "Converse (Dilworth's theorem, sketch 12):"
         , "  max antichain " ++ show (length an) ++ " = min number of chains"
         , "In both chain and antichain (white): " ++ show (length (filter (`elem` an) ch)) ] ]
  ++ [ say ("Chains and antichains   " ++ show (wKi w + 1) ++ " / " ++ show (length kinds) ++ ": " ++ getAt "" kinds (wKi w)) 20 36 18 (grayC 240)
     , say ("Left/Right poset   G new random   C chain " ++ onOff (wShowC w) ++ "   A antichain " ++ onOff (wShowA w) ++ "   L levels " ++ onOff (wShowL w)
            ++ "   Hover -> comparable (lilac)") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    ch = wChain w
    an = wAnti w
    h = maximum (0 : [ rankOf lat i | i <- allOf lat ]) + 1
    hov = nodeAt lat (toP5 env (envMouse env)) 20
    inC i = wShowC w && i `elem` ch
    inA i = wShowA w && i `elem` an
    col i | hov >= 0 && i /= hov && (leq lat i hov || leq lat hov i) = Just (rgbC 200 200 255)
          | inC i && inA i = Just (rgbC 255 255 255)
          | inC i = Just (rgbC 90 225 215)
          | inA i = Just (rgbC 255 190 90)
          | otherwise = Just (rgbC 130 140 190)
    ecol a b = case (elemIndex a ch, elemIndex b ch) of
      (Just ia, Just ib) | inC a && inC b && abs (ia - ib) == 1 -> Just (rgbC 90 225 215)
      _ -> Nothing
    px = envW env * 0.6
    y0 = 160
    levels = [ length [ i | i <- allOf lat, rankOf lat i == r ] | r <- [0 .. h - 1] ]
    levOk = and [ isAntichain lat [ i | i <- allOf lat, rankOf lat i == r ] | r <- [0 .. h - 1] ]
    ok b = if b then "OK" else "NO"
    onOff b = if b then "ON" else "OFF"

-- 角の丸い長方形（左上 (x, y)、角の半径 r）
roundTL :: Float -> Float -> Float -> Float -> Float -> Picture
roundTL x y w h r = Pictures [ rectTL (x + r) y (w - 2 * r) h, rectTL x (y + r) r (h - 2 * r), rectTL (x + w - r) (y + r) r (h - 2 * r)
                             , at (x + r, y + r) (arcs 0), at (x + w - r, y + r) (arcs 1), at (x + r, y + h - r) (arcs 2), at (x + w - r, y + h - r) (arcs 3) ]
  where arcs :: Int -> Picture
        arcs q = let (a0, a1) = case q of { 0 -> (180, 270); 1 -> (270, 360); 2 -> (90, 180); _ -> (0, 90) }
                     pts = [ (r * cos (t * pi / 180), r * sin (t * pi / 180)) | k <- [0 .. 6 :: Int], let t = a0 + (a1 - a0) * fromIntegral k / 6 ]
                 in Polygon ((0, 0) : pts)

-- p5.js の text(s, x, y, w)：幅 w で折り返す（y は 1 行目の上）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y w size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.25) size c | (k, l) <- zip [0 :: Int ..] (wrapWords (max 8 (floor (w / (size * 0.65)))) s) ]

wrapWords :: Int -> String -> [String]
wrapWords m s = reverse (go (words s) [] [])
  where
    go [] cur acc = if null cur then acc else unwords (reverse cur) : acc
    go (x : xs) cur acc
      | null cur = go xs [x] acc
      | length (unwords (reverse (x : cur))) <= m = go xs (x : cur) acc
      | otherwise = go xs [x] (unwords (reverse cur) : acc)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wKi = (wKi w + 1) `mod` nk }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wKi = (wKi w + nk - 1) `mod` nk }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> rebuild env w { wKi = 0, wSeed = wSeed w + 1 }
    | c `elem` "cC" -> w { wShowC = not (wShowC w) }
    | c `elem` "aA" -> w { wShowA = not (wShowA w) }
    | c `elem` "lL" -> w { wShowL = not (wShowL w) }
  _ -> w
  where nk = length kinds

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("40 random posets: longest chain found = longest over all chains (DFS)", and (map (\(c, _, _) -> c) rs))
  , ("Mirsky: every rank level is an antichain, number of levels = longest chain", and (map (\(_, m, _) -> m) rs))
  , ("branch and bound max antichain = max over all subsets (up to 2^12)", and (map (\(_, _, a) -> a) rs))
  , ("subsets of {a,b,c,d}: longest chain 5, max antichain 6 = C(4, 2)", length (longestChain b) == 5 && length (maxAntichain b) == 6)
  , ("divisors of 60: longest chain 5 (1 < 2 < 4 < 12 < 60 ...), max antichain 4 (levels 1,3,4,3,1)", length (longestChain d) == 5 && length (maxAntichain d) == 4) ]
  where
    b = powersetPoset 4
    d = divisorPoset 60
    rs = reverse (fst (foldl' one ([], seedRng 111) [1 .. 40 :: Int]))
    one (acc, r0) _ =
      let (u, r1) = rnd r0
          (q, r2) = randomPoset (5 + floor (u * 8)) 0.3 r1
          n = pN q
          ch = longestChain q
          h = maximum (0 : [ rankOf q i | i <- allOf q ]) + 1
          -- 確かめ用：全部の鎖を深さ優先でたどる
          dfs x len = maximum (len : [ dfs y (len + 1) | y <- allOf q, y /= x, leq q x y ])
          bc = maximum (0 : [ dfs s 1 | s <- allOf q ])
          okC = length ch == bc && h == bc && and (zipWith (leq q) ch (drop 1 ch))
          okM = and [ isAntichain q [ i | i <- allOf q, rankOf q i == r ] | r <- [0 .. h - 1] ]
          a = maxAntichain q
          comp = compMasks q
          bw = maximum (0 : [ popCount s | s <- [0 .. shiftL 1 n - 1], isAntiMask comp n s ])   -- 全部の部分集合を調べる
          okW = isAntichain q a && length a == bw
      in ((okC, okM, okW) : acc, r2)

-- ---------- やってみよう ----------
-- 1. G を何回か押す。最長の鎖と最大の反鎖が共有する元（白）はいつも 1 個以下？ なぜ？
-- 2. 段（ランク）ごとに分けると反鎖になる。いちばん少ない段の数は最長の鎖の長さ（ミルスキー）
-- 3. 120 の約数で、最大の反鎖はどの段？ 真ん中の段がいちばん大きい？
-- 4. 要素にマウスをのせると、比べられる元がうす紫に。比べられない元だけを集めると反鎖
-- 5. 1 日の予定（00）で、同時にできる作業の最大数 = 最大の反鎖


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
