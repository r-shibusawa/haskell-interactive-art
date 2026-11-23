-- ============================================
-- 56-08 形式概念分析 ─ 「対象 × 属性」の表から概念を全部見つけ、概念束を描く
-- 対象の集合 A に共通の属性 A′、属性の集合 B を全部持つ対象 B′。A′ = B かつ B′ = A となる組 (A, B) が「概念」（外延 A・内包 B）
-- 属性の部分集合 2^7 = 128 通りを全部調べて概念を集め、外延の ⊆ で並べる。属性名は上、対象名は下に書く（いちばん簡単なラベル）
-- 操作：表のマスをクリックで ✓ を切りかえ / 概念の丸をクリック → 外延・内包 / R もとの表 / C 表を空に / スペース 含意の表示を切りかえ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, sort, foldl', intercalate, findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

objs :: [String]
objs = ["Lion", "Eagle", "Penguin", "Dolphin", "Frog", "Snake", "Pigeon", "Bat"]      -- ★ 対象

attrs :: [String]
attrs = ["flies", "swims", "wings", "eggs", "mammal", "meat", "4 legs"]                -- ★ 属性

start :: [String]
start = ["0000111", "1011010", "0111010", "0100110", "0101011", "0001010", "1011000", "1000100"]   -- ★ 表（1 = ✓）

nA :: Int
nA = length attrs

-- 概念 = (外延 A, 内包 B)（どちらもビット）
data World = World
  { wCtx      :: [Int]            -- 表の行（対象ごとの属性のビット）
  , wConcepts :: [(Int, Int)]
  , wCL       :: Poset
  , wImp      :: [String]
  , wPick     :: !Int
  , wShowImp  :: !Bool
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-08 Formal concept analysis" (rebuild 1280 800 (World startCtx [] emptyP [] (-1) True [])))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild (envW env) (envH env) (World startCtx [] emptyP [] (-1) True selfTest))
  , skDraw  = draw
  , skEvent = event
  }
  where emptyP = posetFromRel (listArray ((0, 0), (-1, -1)) []) []

startCtx :: [Int]
startCtx = [ foldl' (.|.) 0 [ bit a | (a, c) <- zip [0 ..] r, c == '1' ] | r <- start ]

bit :: Int -> Int
bit k = 1 `shiftL` k

testB :: Int -> Int -> Bool
testB m k = (m `shiftR` k) .&. 1 == 1

-- 導出作用素：A′ = A の全員が持つ属性、B′ = B を全部持つ対象（ビットマスク）
objPrime :: [Int] -> Int -> Int -> Int
objPrime cx na a = foldl' (.&.) (bit na - 1) [ row | (g, row) <- zip [0 ..] cx, testB a g ]

attrPrime :: [Int] -> Int -> Int
attrPrime cx b = foldl' (.|.) 0 [ bit g | (g, row) <- zip [0 ..] cx, row .&. b == b ]

-- 属性の部分集合を全部調べる：B → B′ → B″ が内包
conceptsOf :: [Int] -> Int -> [(Int, Int)]
conceptsOf cx na = sortBy (comparing (\(a, _) -> (popc a, a))) (Map.toList seen)
  where seen = foldl' (\m b -> let a = attrPrime cx b in if Map.member a m then m else Map.insert a (objPrime cx na a) m) Map.empty [0 .. bit na - 1]

-- 確かめ用：対象の部分集合から（A → A′ → A″）
conceptsByObjects :: [Int] -> Int -> [Int]
conceptsByObjects cx na = Map.keys (Map.fromList [ (attrPrime cx (objPrime cx na a), ()) | a <- [0 .. bit (length cx) - 1] ])

conceptLattice :: [(Int, Int)] -> Poset
conceptLattice cs = makePoset (length cs) (\i j -> let ai = ca ! i in ai .&. (ca ! j) == ai) (map (const "") cs)
  where ca = listArray (0, length cs - 1) (map fst cs) :: UArray Int Int

-- 1 つの属性どうしの含意 a → b（a を持つ対象はみな b も持つ）
implications :: [Int] -> [String]
implications cx = [ getAt "" attrs a ++ " -> " ++ getAt "" attrs b
                  | a <- [0 .. nA - 1], b <- [0 .. nA - 1], a /= b
                  , let aa = attrPrime cx (bit a), let ab = attrPrime cx (bit b), aa /= 0, aa .&. ab == aa ]

rebuild :: Float -> Float -> World -> World
rebuild wd h w = w { wConcepts = cs, wCL = layoutHasse (conceptLattice cs) (wd * 0.47) 170 (wd * 0.5) (h - 290) False, wImp = implications (wCtx w), wPick = -1 }
  where cs = conceptsOf (wCtx w) nA

cw, chh, tx, ty :: Float
cw = 52
chh = 30
tx = 110
ty = 140

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 表（属性名は 2 段にずらして書く）
  [ sayC a (tx + fromIntegral k * cw + cw / 2) (ty - 8 - (if odd k then 18 else 0)) 14 (rgbC 90 225 215) | (k, a) <- zip [0 :: Int ..] attrs ]
  ++ concat [ let inA = pk >= 0 && testB cA g
              in sayR o (tx - 8) (ty + fromIntegral g * chh + chh / 2 + 5) 14 (if inA then rgbC 255 200 110 else grayC 210)
                 : concat [ let on = testB row a
                                inB = pk >= 0 && testB cB a
                                x = tx + fromIntegral a * cw
                                y = ty + fromIntegral g * chh
                            in [ Color (if inA && inB then rgbC 70 110 120 else rgbC 30 34 70) (rectTL x y cw chh)
                               , Color (rgbC 60 68 120) (frameTL 1 x y cw chh) ]
                               ++ (if on then [ Color white (thickPath 2.5 [(x + cw / 2 - 7, y + chh / 2), (x + cw / 2 - 2, y + chh / 2 + 6), (x + cw / 2 + 8, y + chh / 2 - 7)]) ] else [])
                          | a <- [0 .. nA - 1] ]
            | (g, o, row) <- zip3 [0 ..] objs (wCtx w) ]
  -- ---- 概念束（属性名は μ(m) = (m′, m″) の上に、対象名は γ(g) = (g″, g′) の下に）
  ++ [ drawHasse cl hopt { oRad = 9, oCol = \i -> Just (if i == pk then white else if pk >= 0 && leq cl i pk then rgbC 255 200 110
                                                        else if pk >= 0 && leq cl pk i then rgbC 90 225 215 else rgbC 150 160 205) } ]
  ++ [ let (px, py) = posOf cl i in sayC a px (py - 13 - 15 * fromIntegral (length [ () | j <- [0 .. k - 1], attrPrime cx (bit j) == aa ])) 14 (rgbC 90 225 215)
     | (k, a) <- zip [0 ..] attrs, let aa = attrPrime cx (bit k), Just i <- [findIndex ((== aa) . fst) cs] ]
  ++ [ let (px, py) = posOf cl i in sayC o px (py + 24 + 15 * fromIntegral (length [ () | h <- take g cx, h == b ])) 14 (rgbC 255 200 110)
     | (g, o, b) <- zip3 [0 ..] objs cx, Just i <- [findIndex ((== b) . snd) cs] ]
  -- ---- 文字
  ++ (if pk >= 0
        then wrapAt 20 y0 15 18 (rgbC 255 200 110) boxW ("Extent A = {" ++ intercalate ", " [ o | (g, o) <- zip [0 ..] objs, testB cA g ] ++ "}")
             ++ wrapAt 20 (y0 + 48) 15 18 (rgbC 90 225 215) boxW ("Intent B = {" ++ intercalate ", " [ a | (k, a) <- zip [0 ..] attrs, testB cB k ] ++ "}")
             ++ wrapAt 20 (y0 + 96) 15 18 (grayC 175) boxW ("A' = B and B' = A " ++ (if objPrime cx nA cA == cB && attrPrime cx cB == cA then "OK" else "NO")
                                                           ++ "  (orange = subconcepts below, cyan = superconcepts above)")
        else [ say "Click a concept circle -> extent (objects) and intent (attributes)" 20 y0 15 (grayC 175) ])
  ++ (if wShowImp w
        then say ("Implications (every object with a has b): " ++ show (length im)) 20 (y0 + 140) 15 (grayC 200)
             : wrapAt 20 (y0 + 150 + 15) 15 20 (rgbC 160 200 255) boxW (intercalate "   " (take 14 im))
        else [])
  ++ [ say ("Formal concept analysis  " ++ show (length objs) ++ " objects x " ++ show nA ++ " attributes -> " ++ show (length cs)
            ++ " concepts (from all 2^" ++ show nA ++ " = " ++ show (bit nA) ++ " attribute sets)") 20 36 18 (grayC 240)
     , say ("Click a cell: toggle   click a circle: concept   R original   C empty   Space implications " ++ (if wShowImp w then "on" else "off")) 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    cx = wCtx w
    cs = wConcepts w
    cl = wCL w
    pk = wPick w
    (cA, cB) = getAt (0, 0) cs pk
    im = wImp w
    y0 = ty + fromIntegral (length objs) * chh + 34
    boxW = envW env * 0.44

-- 幅 boxW に収まるように単語で折り返して書く（行の間は lh）
wrapAt :: Float -> Float -> Float -> Float -> Color -> Float -> String -> [Picture]
wrapAt x y size lh c boxW s = [ say l x (y + lh * fromIntegral k) size c | (k, l) <- zip [0 :: Int ..] (wrapText (floor (boxW / (size * 0.65))) s) ]

wrapText :: Int -> String -> [String]
wrapText mx s = reverse (map unwords' (go (words' s) [] []))
  where
    words' = splitSp
    unwords' = id
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
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        a = floor ((mx - tx) / cw) :: Int
        g = floor ((my - ty) / chh) :: Int
        i = nodeAt (wCL w) (mx, my) 14
    in if a >= 0 && a < nA && g >= 0 && g < length objs
         then rb w { wCtx = [ if h == g then r `xor` bit a else r | (h, r) <- zip [0 ..] (wCtx w) ] }
         else if i >= 0 then w { wPick = if i == wPick w then -1 else i } else w
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> rb w { wCtx = startCtx }
    | c `elem` "cC" -> rb w { wCtx = map (const 0) (wCtx w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShowImp = not (wShowImp w) }
  _ -> w
  where rb = rebuild (envW env) (envH env)

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("4 x 4 table with a check where i /= j: 2^4 = 16 concepts (the subset lattice)", length (conceptsOf ne 4) == 16)
  , ("4 x 4 table with only the diagonal: 4 + 2 = 6 concepts (shape M4)", length (conceptsOf ident 4) == 6)
  , ("animal table: the " ++ show (length cs) ++ " concepts from 128 attribute sets = concepts from 256 object sets", sort (map fst cs) == conceptsByObjects cx nA)
  , ("every concept has A' = B and B' = A", all (\(a, b) -> objPrime cx nA a == b && attrPrime cx b == a) cs)
  , ("the concept lattice is a lattice, extent of a meet = intersection of extents", ok)
  , ("implications a -> b = the ones checked row by row (" ++ show (length im) ++ ", e.g. wings -> eggs)", im == dir && "wings -> eggs" `elem` im) ]
  where
    ne = [ 15 .&. complementB (bit g) | g <- [0 .. 3] ]
    complementB m = (-1) `xor` m
    ident = [ bit g | g <- [0 .. 3] ]
    cx = startCtx
    cs = conceptsOf cx nA
    cl = conceptLattice cs
    ext i = fst (getAt (0, 0) cs i)
    ok = isLattice cl && and [ ext (meetOf cl i j) == ext i .&. ext j | i <- allOf cl, j <- allOf cl ]
    im = implications cx
    dir = [ getAt "" attrs a ++ " -> " ++ getAt "" attrs b | a <- [0 .. nA - 1], b <- [0 .. nA - 1], a /= b
          , any (`testB` a) cx, all (\r -> not (testB r a) || testB r b) cx ]

-- ---------- やってみよう ----------
-- 1. 「wings」のラベルがある丸をクリック。外延は？ 内包に「eggs」が入っているのはなぜ？
-- 2. コウモリ（Bat）の「flies」の ✓ を消すと、概念はいくつ減る？ 束の形はどう変わる？
-- 3. 含意「wings -> eggs」がある。ペンギンに ✓ を足し引きして、この含意をこわしてみよう
-- 4. 自分で対象（好きな食べ物など）と属性を書きかえて、概念束を作ろう（objs, attrs, start）
-- 5. 07 のガロア接続と同じ f, g。概念 = 閉じた集合の組


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
