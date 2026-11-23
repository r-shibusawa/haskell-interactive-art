-- ============================================
-- 56-04 分配束とモジュラー束 ─ すべての 3 つ組で法則を調べ、N5 と M3 をさがす
-- 分配律：x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)。モジュラー律：x ≤ z なら x ∨ (y ∧ z) = (x ∨ y) ∧ z
-- デデキントの定理「N5 を部分束に含まない ⇔ モジュラー」、バーコフの定理「N5 も M3 も含まない ⇔ 分配」を確かめる
-- 操作：← → 束を変える / G ランダムな束（部分集合の族を ∩ で閉じたもの）/ 最後の「マウスで作る束」は下の部分集合をクリックで出し入れ
--       スペース 破れる 3 つ組を順に表示 / N N5 を表示 / M M3 を表示
--       （画面では ∧ を ^、∨ を v と書く）
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, elemIndex)
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

latNames :: [String]
latNames = ["Divisors of 60", "Subsets of {a,b,c}", "Subspaces of F2^3", "N5", "M3", "Random lattice (G)", "Lattice built by mouse"]

myGens0 :: S.Set Int
myGens0 = S.fromList [1, 3, 4]   -- ★ マウスで作る束のはじめの集合 {a}, {a,b}, {c}（∩ で閉じると N5 の形）

-- 調べた結果：分配律・モジュラー律が破れる 3 つ組、見つかった N5・M3（0, a, b, c, 1 の順）
data Res = Res { rDist, rMod :: [(Int, Int, Int)], rN5, rM3 :: Maybe [Int] }

data Show' = ShTriple | ShN5 | ShM3 deriving Eq

data World = World
  { wLi    :: !Int
  , wLat   :: Poset
  , wRes   :: Res
  , wShow  :: !Show'
  , wTi    :: !Int
  , wSeed  :: !Int
  , wGens  :: S.Set Int
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-04 Distributive and modular lattices" (rebuild env1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 (makePoset 0 (\_ _ -> False) []) (Res [] [] Nothing Nothing) ShTriple 0 7 myGens0 []

-- N5 と M3（被覆から）
smallLattice :: String -> Poset
smallLattice kind
  | kind == "N5" = fromCovers ["0", "a", "b", "c", "1"] [(0, 1), (1, 2), (2, 4), (0, 3), (3, 4)]
  | otherwise = fromCovers ["0", "x", "y", "z", "1"] [(0, 1), (0, 2), (0, 3), (1, 4), (2, 4), (3, 4)]

-- 𝔽₂³ の 8 つのベクトルの集合（ビットマスク）のうち、0 を含み XOR で閉じたもの = 部分空間
subspaceLattice :: Poset
subspaceLattice = makePoset (length sp) (\i j -> (spv i .&. spv j) == spv i) (map lab sp)
  where
    has m u = (m `shiftR` u) .&. 1 == 1
    sp = sortBy (comparing popc) [ m | m <- [1, 3 .. 255], and [ has m (u `xor` v) | u <- [0 .. 7], v <- [0 .. 7], has m u, has m v ] ]
    spa = listArray (0, length sp - 1) sp :: UArray Int Int
    spv i = spa ! i
    bin3 u = [ if (u `shiftR` b) .&. 1 == 1 then '1' else '0' | b <- [2, 1, 0] ]
    lab m = let k = popc m
                vs = [ u | u <- [1 .. 7], has m u ]
            in if k == 1 then "0" else if k == 8 then "F^3" else if k == 2 then bin3 (getAt 0 vs 0) else "<" ++ concatMap show vs ++ ">"

-- {a,b,c,d} の部分集合をいくつか選び（gens、なければ種 sd でランダム）、共通部分 ∩ で閉じる（全体も入れる）→ いつも束
closureLattice :: Int -> Maybe (S.Set Int) -> Poset
closureLattice sd gens = makePoset (length fs) (\i j -> (fv i .&. fv j) == fv i) [ setName m 4 | m <- fs ]
  where
    start = case gens of
      Just g -> S.insert 15 g
      Nothing -> let (u, r1) = rnd (seedRng (fromIntegral sd))
                     k = 3 + floor (u * 5) :: Int
                     pick (acc, r) _ = let (v, r') = rnd r in (S.insert (floor (v * 15)) acc, r')
                 in fst (foldl' pick (S.singleton 15, r1) [1 .. k])
    grow fam = let fam' = S.union fam (S.fromList [ a .&. b | a <- S.toList fam, b <- S.toList fam ])
               in if S.size fam' == S.size fam then fam else grow fam'
    fs = sortBy (comparing (\m -> (popc m, m))) (S.toList (grow start))
    fa = listArray (0, length fs - 1) fs :: UArray Int Int
    fv i = fa ! i

latticeOf :: Int -> Int -> S.Set Int -> Poset
latticeOf k sd gens = case k of
  0 -> divisorPoset 60
  1 -> powersetPoset 3
  2 -> subspaceLattice
  3 -> smallLattice "N5"
  4 -> smallLattice "M3"
  5 -> closureLattice sd Nothing
  _ -> closureLattice 0 (Just gens)

-- すべての 3 つ組で調べる ＋ N5・M3 の部分束をさがす（lat は tables 済み）
analyze :: Poset -> Res
analyze lat = Res dist md n5 m3
  where
    n = pN lat
    j = tJ lat
    m = tM lat
    dist = [ (x, y, z) | x <- [0 .. n - 1], y <- [0 .. n - 1], z <- [0 .. n - 1], m x (j y z) /= j (m x y) (m x z) ]
    md = [ (x, y, z) | x <- [0 .. n - 1], y <- [0 .. n - 1], z <- [0 .. n - 1], leq lat x z, j x (m y z) /= m (j x y) z ]
    -- a < b と c で a∨c = b∨c、a∧c = b∧c
    n5 = case [ [m a c, a, b, c, j a c] | a <- [0 .. n - 1], b <- [0 .. n - 1], a /= b, leq lat a b, c <- [0 .. n - 1], j a c == j b c, m a c == m b c ] of
      (r : _) -> Just r
      [] -> Nothing
    -- 3 つの上限も下限も全部同じ
    m3 = case [ [m x y, x, y, z, j x y] | x <- [0 .. n - 1], y <- [x + 1 .. n - 1], z <- [y + 1 .. n - 1]
              , j x y == j y z, j x y == j x z, m x y == m y z, m x y == m x z ] of
      (r : _) -> Just r
      [] -> Nothing

rebuild :: Env -> World -> World
rebuild env w = w { wLat = layoutHasse lat 110 170 (envW env * 0.52 - 110) (envH env - 290) (wLi w <= 1), wRes = analyze lat, wTi = 0 }
  where lat = tables (latticeOf (wLi w) (wSeed w) (wGens w))

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawHasse lat hopt { oRad = 20, oFs = 14, oCol = col, oEcol = ecol } ]
  -- ---- 右のパネル
  ++ [ say (getAt "" latNames (wLi w) ++ " (" ++ show n ++ " elements, all " ++ show (n * n * n) ++ " triples)") px y 17 (grayC 230)
     , say ("Distributive? " ++ yes dist ++ "   failing triples " ++ show (length (rDist res))) px (y + 36) 16 (okC dist)
     , say ("Modular? " ++ yes md ++ "   failing triples " ++ show (length (rMod res))) px (y + 64) 16 (okC md)
     , say ("N5 sublattice: " ++ maybe "none" labs (rN5 res)) px (y + 100) 16 (rgbC 255 190 90)
     , say ("M3 sublattice: " ++ maybe "none" labs (rM3 res)) px (y + 126) 16 (rgbC 90 225 215)
     , say ((if md == isNothing (rN5 res) then "OK" else "NO") ++ "  Dedekind: modular <=> no N5") px (y + 164) 15 (grayC 200)
     , say ((if dist == (isNothing (rN5 res) && isNothing (rM3 res)) then "OK" else "NO") ++ "  Birkhoff: distributive <=> no N5, no M3") px (y + 188) 15 (grayC 200) ]
  ++ tripleText
  ++ wrapSay 14 (envW env - px - 20) px (y + 330) 18 (rgbC 150 156 190)
       "N5 = 0 < a < b < 1 and 0 < c < 1. M3 = 0 < x, y, z < 1. A sublattice is closed under v and ^ (not just any subposet)."
  ++ (if wLi w == 6 then mouseButtons else [])
  ++ [ say ("Distributive and modular lattices   " ++ show (wLi w + 1) ++ " / " ++ show (length latNames) ++ ": " ++ getAt "" latNames (wLi w)) 20 36 18 (grayC 240)
     , say "Left/Right lattice   G random   Space next failing triple   N show N5   M show M3   (7th: build by mouse)" 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    res = wRes w
    n = pN lat
    fails = rDist res
    nf = length fails
    trip = if nf > 0 then Just (getAt (0, 0, 0) fails (wTi w `mod` nf)) else Nothing
    hl = case wShow w of
      ShTriple -> maybe [] (\(a, b, c) -> [a, b, c]) trip
      ShN5 -> maybe [] id (rN5 res)
      ShM3 -> maybe [] id (rM3 res)
    hc = if wShow w == ShM3 then rgbC 90 225 215 else if wShow w == ShN5 then rgbC 255 190 90 else rgbC 255 110 130
    inHl a = a `elem` hl && wShow w /= ShTriple
    col i = Just $ case elemIndex i hl of
      Just k | wShow w == ShTriple -> getAt white [rgbC 255 255 255, rgbC 255 150 160, rgbC 255 200 120] k
             | otherwise -> hc
      Nothing -> rgbC 150 160 205
    ecol a b = if inHl a && inHl b then Just hc else Nothing
    px = envW env * 0.56
    y = 160
    yes v = if v then "yes" else "no"
    okC v = if v then rgbC 140 225 180 else rgbC 255 130 140
    dist = null (rDist res)
    md = null (rMod res)
    labs = intercalate ", " . map (labOf lat)
    l = labOf lat
    j = tJ lat
    m = tM lat
    tripleText = case (wShow w, trip) of
      (ShTriple, Just (x, yy, z)) ->
        [ say ("Triple " ++ show (wTi w `mod` nf + 1) ++ " / " ++ show nf ++ ": x = " ++ l x ++ " (white), y = " ++ l yy ++ " (red), z = " ++ l z ++ " (yellow)") px (y + 230) 14 white
        , say ("x ^ (y v z) = " ++ l x ++ " ^ " ++ l (j yy z) ++ " = " ++ l (m x (j yy z))) px (y + 258) 15 (rgbC 255 160 170)
        , say ("(x ^ y) v (x ^ z) = " ++ l (m x yy) ++ " v " ++ l (m x z) ++ " = " ++ l (j (m x yy) (m x z)) ++ "  <- different") px (y + 282) 15 (rgbC 255 160 170) ]
      (ShTriple, Nothing) -> [ say "The distributive law holds for every triple" px (y + 230) 15 (rgbC 140 225 180) ]
      _ -> []
    h = envH env
    -- マウスで作る束：下の 14 個の部分集合をクリックで出し入れ（全体と ∩ は自動で足す）
    mouseButtons =
      concat [ let x = 20 + fromIntegral (mm - 1) * 62
                   on = S.member mm (wGens w)
               in [ Color (if on then rgbC 40 90 100 else rgbC 30 34 70) (rectTL x (h - 70) 56 30)
                  , Color (if on then rgbC 90 225 215 else rgbC 80 88 140) (frameTL 1.5 x (h - 70) 56 30)
                  , sayM (setName mm 4) (x + 28) (h - 55) 14 (if on then white else grayC 170) ]
             | mm <- [1 .. 14 :: Int] ]
      ++ [ say "Click a set to toggle -> close under intersection, add abcd: always a lattice" 20 (h - 80) 14 (grayC 175) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wLi = (wLi w + 1) `mod` nk }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wLi = (wLi w + nk - 1) `mod` nk }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShow = ShTriple, wTi = wTi w + 1 }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> rebuild env w { wLi = 5, wSeed = wSeed w + 1 }
    | c `elem` "nN" -> w { wShow = ShN5 }
    | c `elem` "mM" -> w { wShow = ShM3 }
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        h = envH env
        mm = floor ((mx - 20) / 62) + 1 :: Int
    in if wLi w /= 6 || my < h - 70 || my > h - 40 || mm < 1 || mm > 14 || mx - 20 - fromIntegral (mm - 1) * 62 > 56 then w
       else rebuild env w { wGens = if S.member mm (wGens w) then S.delete mm (wGens w) else S.insert mm (wGens w) }
  _ -> w
  where nk = length latNames

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("N5: modular? no, distributive? no", not (null (rMod n5)) && not (null (rDist n5)))
  , ("M3: distributive? no, but modular? yes", not (null (rDist m3)) && null (rMod m3))
  , ("divisors of 60 and subsets are distributive (no N5, no M3)"
    , null (rDist d60) && null (rDist b3) && isNothing (rN5 d60) && isNothing (rM3 d60) && isNothing (rM3 b3))
  , ("subspaces of F2^3: 1 + 7 + 7 + 1 = 16, modular but not distributive (contains M3)"
    , pN vs == 16 && length [ () | i <- [0 .. pN vs - 1], rankOf vs i == 1 ] == 7 && null (rMod vr) && not (null (rDist vr)) && isJust (rM3 vr))
  , ("mouse lattice start {a}, {a,b}, {c} closed under intersection = N5 with 5 elements (not modular)"
    , pN my == 5 && isLattice my && not (null (rMod myr)) && isJust (rN5 myr))
  , ("60 random lattices: modular <=> no N5 (Dedekind)", okD && cn5 > 0)
  , ("60 random lattices: distributive <=> no N5, no M3 (Birkhoff), distributive => modular (distributive "
       ++ show cdist ++ ", with N5 " ++ show cn5 ++ ", with M3 " ++ show cm3 ++ ")", okB && cdist > 0) ]
  where
    an = analyze . tables
    n5 = an (smallLattice "N5")
    m3 = an (smallLattice "M3")
    d60 = an (divisorPoset 60)
    b3 = an (powersetPoset 3)
    vs = subspaceLattice
    vr = an vs
    my = closureLattice 0 (Just (S.fromList [1, 3, 4]))
    myr = an my
    rs = [ (lt, analyze lt) | sd <- [1 .. 60], let lt = tables (closureLattice (sd * 13) Nothing) ]
    okD = and [ isLattice lt && (null (rMod r) == isNothing (rN5 r)) | (lt, r) <- rs ]
    okB = and [ let dist = null (rDist r) in dist == (isNothing (rN5 r) && isNothing (rM3 r)) && not (dist && not (null (rMod r))) | (_, r) <- rs ]
    cn5 = length [ () | (_, r) <- rs, isJust (rN5 r) ]
    cm3 = length [ () | (_, r) <- rs, isJust (rM3 r) ]
    cdist = length [ () | (_, r) <- rs, null (rDist r) ]

-- 長い文を幅 wd に収まるように折り返して書く（行の間隔 lh）
wrapSay :: Float -> Float -> Float -> Float -> Float -> Color -> String -> [Picture]
wrapSay size wd x y lh c s = [ say ln x (y + fromIntegral k * lh) size c | (k, ln) <- zip [0 :: Int ..] (go (words s)) ]
  where
    maxC = max 10 (floor (wd / (size * 0.66)))
    go [] = []
    go (t : ts) = let (ln, rest) = fill t ts in ln : go rest
    fill acc (t : ts) | length acc + 1 + length t <= maxC = fill (acc ++ " " ++ t) ts
    fill acc ts = (acc, ts)

-- ---------- やってみよう ----------
-- 1. N5 でスペースを押して、分配律が破れる 3 つ組を手で計算しよう（x ∧ (y ∨ z) と (x ∧ y) ∨ (x ∧ z)）
-- 2. M3 はモジュラーなのに分配ではない。どの 3 つ組で破れる？
-- 3. 𝔽₂³ の部分空間（原点を通る直線と平面）の束で M を押す。どの 3 本の直線が M3 を作る？
-- 4. G を何回か押す。N5 を含む束と含まない束はどんな形？
-- 5. 約数の束はいつも分配束。なぜ？（gcd と lcm は「指数の min と max」）
-- 6. Haskell では analyze がリストの内包表記 1 つで全部の 3 つ組を調べている。読んでみよう

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
