-- ============================================
-- 56-12 ディルワースの定理 ─ 最大の反鎖の大きさ = 鎖で全部をおおうときの最小の本数
-- 二部グラフ（左に各元のコピー、右に各元のコピー、x < y なら 左x — 右y）の最大マッチング M を増加道で求める
-- マッチした辺 x — y を「x の次は y」とつなぐと鎖に分かれ、本数 = n − |M|。ケーニヒの定理で同じ大きさの反鎖も見つかる
-- 操作：← → 半順序を変える / ↑↓ 部分集合の束の k（スペルナーの定理）/ G 新しいランダム / スペース 1 歩 / A 自動 / R やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!), (//), elems)
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (sortBy, sort, foldl', intercalate)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kinds :: [String]
kinds = ["random poset (G)", "subset lattice", "divisors of 60", "divisors of 360"]

-- マッチング：mL ! i = 左 i の相手（右）、mR ! j = 右 j の相手（左）。−1 はまだ
type Match = (UArray Int Int, UArray Int Int)

data World = World
  { wKi    :: !Int
  , wKSet  :: !Int               -- ★ 部分集合の束の k（2〜5）
  , wSeed  :: !Int
  , wLat   :: Poset
  , wM     :: Match
  , wNext  :: !Int               -- 次に増加道をさがす左の元
  , wAuto  :: !Bool
  , wAnti  :: [Int]              -- ケーニヒの反鎖（全部処理したら）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-12 Dilworth's theorem" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 25 == 0 then stepIt w else w)
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 4 12 (powersetPoset 1) (emptyMatch 0) 0 True [] []

posetOf :: World -> Poset
posetOf w
  | wKi w == 0 = let (u, r1) = rnd (seedRng (fromIntegral (wSeed w)))
                     (q, _) = randomPoset (9 + floor (u * 6)) 0.2 r1
                 in q { pLab = A.listArray (A.bounds (pLab q)) [ [c] | c <- take (pN q) "abcdefghijklmnop" ] }
  | wKi w == 1 = powersetPoset (wKSet w)
  | otherwise = divisorPoset (if wKi w == 2 then 60 else 360)

emptyMatch :: Int -> Match
emptyMatch n = (listArray (0, n - 1) (replicate n (-1)), listArray (0, n - 1) (replicate n (-1)))

-- 増加道：左 i から、まだマッチしていない右か、マッチ相手が別の道を見つけられる右へ（クーンの方法）
-- seen（もう調べた右）は p5.js 版と同じく、1 回の探索の中で共有する
augment :: Poset -> Int -> (Match, UArray Int Bool) -> (Bool, (Match, UArray Int Bool))
augment lat i st0 = go 0 st0
  where
    n = pN lat
    go j st@((mL, mR), seen)
      | j >= n = (False, st)
      | j /= i && leq lat i j && not (seen ! j) =      -- 推移的閉包の辺 i < j を使う（被覆だけでは足りない）
          let seen' = seen // [(j, True)]
              k = mR ! j
              (ok, ((mL2, mR2), seen2)) = if k < 0 then (True, ((mL, mR), seen')) else augment lat k ((mL, mR), seen')
          in if ok then (True, ((mL2 // [(i, j)], mR2 // [(j, i)]), seen2)) else go (j + 1) ((mL2, mR2), seen2)
      | otherwise = go (j + 1) st

augment1 :: Poset -> Int -> Match -> Match
augment1 lat i m = fst (snd (augment lat i (m, listArray (0, pN lat - 1) (replicate (pN lat) False))))

fullMatching :: Poset -> Match
fullMatching lat = foldl' (flip (augment1 lat)) (emptyMatch (pN lat)) (allOf lat)

-- マッチした辺 x → y をつないで鎖にする（右でマッチしていない元から始める）
chainsOf :: Poset -> Match -> [[Int]]
chainsOf lat (mL, mR) = [ follow [i] | i <- allOf lat, mR ! i < 0 ]
  where follow c@(x : _) = if mL ! x >= 0 && length c <= pN lat then follow (mL ! x : c) else reverse c
        follow [] = []

-- マッチしていない左から交互道でたどれる頂点 Z → 反鎖 = {x | 左x ∈ Z かつ 右x ∉ Z}
konig :: Poset -> Match -> [Int]
konig lat (mL, mR) = [ x | x <- allOf lat, zL ! x, not (zR ! x) ]
  where
    n = pN lat
    falses = listArray (0, n - 1) (replicate n False) :: UArray Int Bool
    starts = [ i | i <- allOf lat, mL ! i < 0 ]
    (zL, zR) = bfs starts (falses // [ (i, True) | i <- starts ]) falses
    bfs [] a b = (a, b)
    bfs (i : q) a b =
      let (q', a', b') = foldl' visit (q, a, b) [ j | j <- allOf lat, j /= i, leq lat i j, mL ! i /= j ]
          visit (qq, aa, bb) j
            | bb ! j = (qq, aa, bb)
            | otherwise = let k = mR ! j
                              bb' = bb // [(j, True)]
                          in if k >= 0 && not (aa ! k) then (qq ++ [k], aa // [(k, True)], bb') else (qq, aa, bb')
      in bfs q' a' b'

isAntichain :: Poset -> [Int] -> Bool
isAntichain lat sx = and [ a == b || (not (leq lat a b) && not (leq lat b a)) | a <- sx, b <- sx ]

binom :: Int -> Int -> Int
binom n r = product [n - r + 1 .. n] `div` product [1 .. r]

rebuild :: Env -> World -> World
rebuild env w = restart w { wLat = layoutHasse (posetOf w) 80 170 (envW env * 0.48 - 80) (envH env - 300) (wKi w >= 1) }

restart :: World -> World
restart w = w { wM = emptyMatch (pN (wLat w)), wNext = 0, wAnti = [] }

stepIt :: World -> World
stepIt w
  | wNext w < n = let m = augment1 lat (wNext w) (wM w)
                      nx = wNext w + 1
                  in w { wM = m, wNext = nx, wAnti = if nx == n then konig lat m else wAnti w }
  | otherwise = w
  where lat = wLat w
        n = pN lat

ccol :: Int -> Color
ccol k = let (a, b, c) = getAt (90, 225, 215) [(90, 225, 215), (255, 190, 90), (255, 120, 170), (170, 150, 255), (140, 225, 120), (255, 240, 120), (120, 170, 255), (255, 150, 100), (200, 120, 255), (120, 255, 200)] (k `mod` 10)
         in rgbC a b c

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：ハッセ図に鎖の色（マッチした辺 x → y を太く）
  [ Color (rgbaC 255 255 255 25) (Line [posOf lat a, posOf lat b]) | (a, b) <- pCov lat ]
  ++ [ Color (ccol (cid i)) (thickLine 5 (posOf lat i) (posOf lat (mL ! i))) | i <- allOf lat, mL ! i >= 0 ]
  ++ concat [ [ Color (if inAnti then white else bgCol) (at (posOf lat i) (disc (rad + (if inAnti then 2 else 1))))
              , Color (ccol (cid i)) (at (posOf lat i) (disc (rad - (if inAnti then 2 else 1))))
              , let (x, y) = posOf lat i in sayM (labOf lat i) x y 14 bgCol ]
            | i <- allOf lat, let inAnti = i `elem` wAnti w ]
  -- ---- 中：二部グラフ（左のコピー → 右のコピー）
  ++ [ if mt then Color (ccol (cid i)) (thickLine 3 (bx, by + fi i * bh) (bx + bw, by + fi j * bh))
             else Color (rgbaC 255 255 255 30) (Line [(bx, by + fi i * bh), (bx + bw, by + fi j * bh)])
     | i <- allOf lat, j <- allOf lat, i /= j, leq lat i j, let mt = mL ! i == j ]
  ++ concat [ let c = if i == wNext w then white else rgbC 200 206 230
              in [ sayM (labOf lat i) (bx - 18) (by + fi i * bh) 14 c, sayM (labOf lat i) (bx + bw + 18) (by + fi i * bh) 14 c ]
            | i <- allOf lat ]
  ++ [ say "left x - right y (x < y)" (bx - 10) (by - 18) 14 (grayC 200)
  -- ---- 右のパネル
     , say (getAt "" kinds (wKi w) ++ (if wKi w == 1 then " k = " ++ show (wKSet w) else "") ++ "   n = " ++ show n) px y0 17 (grayC 230)
     , say ("Matching |M| = " ++ show nm ++ " (left " ++ show (wNext w) ++ " / " ++ show n ++ " done)") px (y0 + 34) 15 (grayC 220)
     , say ("Chains = n - |M| = " ++ show (n - nm)) px (y0 + 60) 15 (grayC 220) ]
  ++ [ say (intercalate " < " (map (labOf lat) c)) px (y0 + 92 + 20 * fi k) 15 (ccol k) | (k, c) <- zip [0 ..] (take 10 chs) ]
  ++ (if wNext w == n
        then [ say ("Konig antichain (white ring): size " ++ show (length (wAnti w)) ++ (if isAntichain lat (wAnti w) then " OK antichain" else " NO")) px (y1 + 10) 15 white
             , say ("Max antichain " ++ show (length (wAnti w)) ++ " = min chains " ++ show (n - nm) ++ (if length (wAnti w) == n - nm then "  OK Dilworth" else "  NO")) px (y1 + 36) 15 (rgbC 140 225 180) ]
        else [ say "Space: next augmenting path" px (y1 + 10) 15 (grayC 175) ])
  ++ [ say ("Sperner: max antichain = middle level C(" ++ show (wKSet w) ++ ", " ++ show (wKSet w `div` 2) ++ ") = " ++ show (binom (wKSet w) (wKSet w `div` 2))) px (y1 + 70) 15 (rgbC 255 220 120) | wKi w == 1 ]
  ++ [ say ("Dilworth's theorem   " ++ getAt "" kinds (wKi w) ++ ": max matching -> min chain cover and max antichain") 20 36 18 (grayC 240)
     , say ("Left/Right poset   Up/Down subset k (2-5)   G new random   Space step   A auto " ++ (if wAuto w then "ON" else "OFF") ++ "   R restart") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    n = pN lat
    (mL, _) = wM w
    chs = chainsOf lat (wM w)
    cidA = A.accumArray (\_ v -> v) 0 (0, max 0 (n - 1)) [ (x, k) | (k, c) <- zip [0 ..] chs, x <- c ] :: A.Array Int Int
    cid i = if i >= 0 && i < n then cidA A.! i else 0
    nm = length (filter (>= 0) (elems mL))
    rad = if n > 20 then 13 else 16
    fi = fromIntegral :: Int -> Float
    bx = envW env * 0.52
    bw = 150
    by = 150
    bh = min 22 ((envH env - 220) / fromIntegral (max 1 n))
    px = bx + bw + 50
    y0 = 160
    y1 = y0 + 100 + 20 * fromIntegral (min 10 (length chs))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wKi = (wKi w + 1) `mod` nk }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wKi = (wKi w + nk - 1) `mod` nk }
  EventKey (SpecialKey KeyUp) Down _ _ | wKSet w < 5 -> rebuild env w { wKSet = wKSet w + 1, wKi = 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wKSet w > 2 -> rebuild env w { wKSet = wKSet w - 1, wKi = 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> stepIt w
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> rebuild env w { wKi = 0, wSeed = wSeed w + 1 }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> restart w
  _ -> w
  where nk = length kinds

-- ---- 自己テスト（p5.js 版と同じ 4 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("40 random posets: chains n - |M| = max antichain over all subsets = size of Konig's antichain", and (map fst rs))
  , ("the chains built from the matching are chains and cover every element exactly once", and (map snd rs))
  , ("Sperner: max antichain for k = 2, 3, 4, 5 is " ++ intercalate ", " (map show sp) ++ " = C(k, k/2) (6 for k = 4)", sp == [2, 3, 6, 10])
  , ("divisors of 360 = 2^3*3^2*5: max antichain = 6 in the middle level = 6 chains"
    , length (konig d m360) == 6 && pN d - length (filter (>= 0) (elems (fst m360))) == 6) ]
  where
    rs = reverse (fst (foldl' one ([], seedRng 1212) [1 .. 40 :: Int]))
    one (acc, r0) _ =
      let (u, r1) = rnd r0
          (q, r2) = randomPoset (4 + floor (u * 9)) 0.25 r1
          n = pN q
          m@(a, _) = fullMatching q
          nm = length (filter (>= 0) (elems a))
          an = konig q m
          ch = chainsOf q m
          comp = listArray (0, n - 1) [ foldl' (\s j -> if leq q i j || leq q j i then s .|. shiftL 1 j else s) 0 (allOf q) | i <- allOf q ] :: UArray Int Int
          bw = maximum (0 : [ popCount s | s <- [0 .. shiftL 1 n - 1], and [ comp ! i .&. s == shiftL 1 i | i <- allOf q, testBit s i ] ])   -- 確かめ用：全部の部分集合
          okD = n - nm == bw && length an == bw && isAntichain q an
          okC = sort (concat ch) == allOf q && length ch == n - nm && and [ and (zipWith (leq q) c (drop 1 c)) | c <- ch ]
      in ((okD, okC) : acc, r2)
    sp = [ let q = powersetPoset k in length (konig q (fullMatching q)) | k <- [2, 3, 4, 5] ]
    d = divisorPoset 360
    m360 = fullMatching d

-- ---------- やってみよう ----------
-- 1. スペースで 1 歩ずつ。マッチングが 1 つ増えるたびに鎖が 1 本減る
-- 2. 最後に白わくの反鎖が出る。それぞれの鎖から 1 個ずつ選ばれている？
-- 3. ↑↓ で k を変えて、部分集合の束の最大反鎖が真ん中の段 C(k, k/2) になることを確かめよう
-- 4. augment の中の leq lat i j を「被覆の辺だけ」にすると、鎖の本数が増えることがある。なぜ？
-- 5. 仕事の順番（半順序）を、何人で並行してやれば最短で終わる？（鎖 = 1 人の担当）


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
