-- ============================================
-- 56-07 ガロア接続と閉包 ─ 2 つの束の間を行き来する f と g
-- 1：数の集合 X → 「X の全員が持つ性質」f(X)、性質の集合 Y → 「Y を全部持つ数」g(Y)。Y ⊆ f(X) ⇔ X ⊆ g(Y)（向きが逆のガロア接続）
-- 2：関数 h(x) = x mod 4 の像 f(X) = h(X) と逆像 g(Y) = h⁻¹(Y)。f(X) ⊆ Y ⇔ X ⊆ g(Y)（向きが同じガロア接続）
-- どちらも c = g∘f は閉包作用素（拡大・単調・冪等）で、閉じた集合（c(X) = X）は束になる
-- 操作：1 2 例を切りかえ / 上の数をクリックで X に入れる・出す / 下の性質をクリックで Y / スペース X を c(X) にする / R 空にする
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, nub, elemIndex)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

numN :: Int
numN = 10                     -- ★ 数 1〜numN（10 まで）

-- ★ 性質：名前・ハッセ図のラベルの 1 文字・判定
props :: [(String, Char, Int -> Bool)]
props = [ ("even", 'e', even), ("odd", 'o', odd), ("prime", 'p', (`elem` [2, 3, 5, 7])), ("3k", '3', \k -> k `mod` 3 == 0)
        , ("<= 5", '5', (<= 5)), ("square", 's', (`elem` [1, 4, 9])) ]

nP :: Int
nP = length props

allO :: Int
allO = bit numN - 1

-- 数 o（0 始まり）が持つ性質のビット
objA :: UArray Int Int
objA = listArray (0, numN - 1) [ foldl' (.|.) 0 [ bit a | (a, (_, _, fn)) <- zip [0 ..] props, fn (o + 1) ] | o <- [0 .. numN - 1] ]

data World = World
  { wEx   :: !Int        -- 例 1 か 2
  , wX    :: !Int        -- 数の集合 X（ビット）
  , wY    :: !Int        -- 性質の集合 Y（ビット）
  , wCL   :: Poset       -- 閉じた集合の束（pVal = 閉じた集合）
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-07 Galois connections and closure" (world0 1280 800))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 (envW env) (envH env)) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> Float -> World
world0 wd h = World 1 20 0 (layoutCL wd h (closedLattice 1)) []

bit :: Int -> Int
bit k = 1 `shiftL` k

nB :: Int -> Int
nB ex = if ex == 1 then nP else 4

-- 1：共通の性質　2：像 h(X)
fOf :: Int -> Int -> Int
fOf ex x
  | ex == 1 = foldl' (.&.) (bit nP - 1) [ objA ! o | o <- [0 .. numN - 1], testB x o ]
  | otherwise = foldl' (.|.) 0 [ bit ((o + 1) `mod` 4) | o <- [0 .. numN - 1], testB x o ]

-- 1：Y を全部持つ数　2：逆像 h⁻¹(Y)
gOf :: Int -> Int -> Int
gOf ex y = foldl' (.|.) 0 [ bit o | o <- [0 .. numN - 1], if ex == 1 then (objA ! o) .&. y == y else testB y ((o + 1) `mod` 4) ]

cOf :: Int -> Int -> Int
cOf ex = gOf ex . fOf ex

testB :: Int -> Int -> Bool
testB m k = (m `shiftR` k) .&. 1 == 1

-- a ⊆ b
sub :: Int -> Int -> Bool
sub a b = a .&. b == a

-- 閉じた集合を全部集めて、⊆ の束を作る
closedLattice :: Int -> Poset
closedLattice ex = (makePoset (length ls) (\i j -> sub (la ! i) (la ! j)) [ bitNames (fOf ex m) (labs ex) | m <- ls ]) { pVal = ls }
  where
    ls = sortBy (comparing (\m -> (popc m, m))) (nub [ cOf ex x | x <- [0 .. allO] ])
    la = listArray (0, length ls - 1) ls :: UArray Int Int

layoutCL :: Float -> Float -> Poset -> Poset
layoutCL wd h p = layoutHasse p (wd * 0.6) 190 (wd * 0.37) (h - 300) False

labs :: Int -> String
labs ex = if ex == 1 then [ c | (_, c, _) <- props ] else "0123"

bitNames :: Int -> String -> String
bitNames m nm = case [ c | (a, c) <- zip [0 ..] nm, testB m a ] of
  "" -> "-"
  s -> s

setText :: Int -> String
setText x = "{" ++ intercalate ", " [ show (o + 1) | o <- [0 .. numN - 1], testB x o ] ++ "}"

propNames :: Int -> [String]
propNames ex = if ex == 1 then [ n | (n, _, _) <- props ] else ["mod 4 = 0", "rem 1", "rem 2", "rem 3"]

tileW :: Env -> Float
tileW env = min 64 ((envW env * 0.55 - 40) / fromIntegral numN)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 数のタイル（白わく = X、水色 = 閉包で増えた分）
  concat [ let x = 20 + fromIntegral o * bw
               inX = testB xm o
               inC = testB cx o
               fillC | inC = if inX then rgbC 70 120 150 else rgbC 60 200 190
                     | otherwise = rgbC 34 38 76
           in [ Color fillC (rectTL x 120 (bw - 8) (bw - 8))
              , Color (if inX then white else grayC 70) (frameTL (if inX then 3 else 1) x 120 (bw - 8) (bw - 8))
              , sayM (show (o + 1)) (x + bw / 2 - 4) (120 + bw / 2 - 4) 20 white ]
              ++ [ Color (rgbC 255 190 90) (at (x + bw / 2 - 4, 120 + bw - 2) (disc 4)) | testB gy o ]
         | o <- [0 .. numN - 1] ]
  -- ---- 性質のチップ（橙わく = Y、緑 = f(X)）
  ++ concat [ let x = 20 + fromIntegral a * 110
                  inF = testB fx a
                  inY = testB (wY w) a
              in [ Color (if inF then rgbC 60 150 100 else rgbC 34 38 76) (rectTL x 230 100 40)
                 , Color (if inY then rgbC 255 190 90 else rgbC 70 76 120) (frameTL (if inY then 3 else 1) x 230 100 40)
                 , sayM nm (x + 50) 250 14 (grayC 240) ]
            | (a, nm) <- zip [0 .. nB ex - 1] names ]
  -- ---- 説明
  ++ [ say ("X = " ++ setText xm) 20 y 16 (grayC 230)
     , say ((if ex == 1 then "f(X) = properties shared by all of X = " else "f(X) = image h(X) = ") ++ nameSet fx) 20 (y + 28) 16 (rgbC 140 225 160)
     , say ("c(X) = g(f(X)) = " ++ setText cx ++ (if cx == xm then "  <- X is closed" else "  (cyan = added)")) 20 (y + 56) 16 (rgbC 90 225 215)
     , say ("Y = " ++ nameSet (wY w) ++ "   g(Y) = " ++ setText gy ++ " (orange dots)") 20 (y + 84) 16 (rgbC 255 190 90)
     , say ((if ex == 1 then "Y <= f(X)? " else "f(X) <= Y? ") ++ yn lhs ++ "  <=>  X <= g(Y)? " ++ yn rhs ++ (if lhs == rhs then "   OK always the same answer" else "   NO")) 20 (y + 124) 17 white ]
  ++ [ say s 20 (y + 160 + 22 * k) 14 (grayC 175)
     | (k, s) <- zip [0 ..]
         [ "Closure operator c = g.f (checked for every X):"
         , "  extensive X <= c(X)   monotone X <= X' => c(X) <= c(X')"
         , "  idempotent c(c(X)) = c(X)        (<= means subset)"
         , if ex == 1 then "Order-reversing: bigger X -> smaller f(X)" else "Image/preimage: h^-1(h(X)) = numbers with the same remainders"
         , show (pN cl) ++ " closed sets, closed under intersection:"
         , "  join = c(X u X'), meet = X n X'  (lattice on the right)" ] ]
  -- ---- 右：閉じた集合の束
  ++ [ drawHasse cl hopt { oRad = if ex == 1 then 28 else 22, oFs = 14, oCol = \i -> Just (if i == cur then rgbC 90 225 215 else if sub xm (getAt 0 (pVal cl) i) then rgbC 150 190 205 else rgbC 130 140 190) }
     , say "Lattice of closed sets (label = f of the set)" (wd * 0.6) 140 14 (grayC 200)
     , say "cyan = c(X), light = closed sets containing X" (wd * 0.6) 160 14 (grayC 200) ]
  ++ [ say (setText (getAt 0 (pVal cl) hv)) (mx + 14) (my - 10) 15 white | hv >= 0 ]
  ++ [ say ("Galois connection and closure   example " ++ show ex ++ ": " ++ (if ex == 1 then "numbers <-> properties" else "image and preimage of h(x) = x mod 4")) 20 36 18 (grayC 240)
     , say "1 2 examples   click a number: X   click a property: Y   Space X := c(X)   R clear" 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    wd = envW env
    ex = wEx w
    xm = wX w
    cl = wCL w
    fx = fOf ex xm
    cx = cOf ex xm
    gy = gOf ex (wY w)
    bw = tileW env
    y = 310
    names = propNames ex
    nameSet m = "{" ++ intercalate ", " [ nm | (a, nm) <- zip [0 .. nB ex - 1] names, testB m a ] ++ "}"
    lhs = if ex == 1 then sub (wY w) fx else sub fx (wY w)
    rhs = sub xm gy
    yn b = if b then "yes" else "no"
    cur = maybe (-1) id (elemIndex cx (pVal cl))
    (mx, my) = toP5 env (envMouse env)
    hv = nodeAt cl (mx, my) (if ex == 1 then 28 else 22)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        bw = tileW env
        o = floor ((mx - 20) / bw) :: Int
        a = floor ((mx - 20) / 110) :: Int
        w1 = if my > 120 && my < 120 + bw && o >= 0 && o < numN then w { wX = wX w `xor` bit o } else w
    in if my > 230 && my < 270 && a >= 0 && a < nB (wEx w) then w1 { wY = wY w1 `xor` bit a } else w1
  EventKey (Char c) Down _ _
    | c `elem` "12" -> let e = fromEnum c - fromEnum '0' in w { wEx = e, wY = 0, wCL = layoutCL (envW env) (envH env) (closedLattice e) }
    | c `elem` "rR" -> w { wX = 0, wY = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wX = cOf (wEx w) (wX w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest = concatMap one [1, 2] ++ [("f.g.f = f (on the property side, f after g is closed too)", fgf)]
  where
    one e =
      let nb = bit (nB e)
          gal = and [ (if e == 1 then sub y fx else sub fx y) == sub x (gOf e y) | x <- [0 .. allO], let fx = fOf e x, y <- [0 .. nb - 1] ]
          ext = and [ sub x (cOf e x) | x <- [0 .. allO] ]
          idem = and [ cOf e c == c | x <- [0 .. allO], let c = cOf e x ]
          mono = and [ sub c (cOf e (x .|. bit o)) | x <- [0 .. allO], let c = cOf e x, o <- [0 .. numN - 1] ]   -- 1 つ足しても縮まない → 単調
          l = closedLattice e
          v i = getAt 0 (pVal l) i
          ok = isLattice l && and [ v (meetOf l i j) == v i .&. v j && v (joinOf l i j) == cOf e (v i .|. v j) | i <- allOf l, j <- allOf l ]
      in [ ("example " ++ show e ++ ": " ++ (if e == 1 then "Y <= f(X) <=> X <= g(Y)" else "f(X) <= Y <=> X <= g(Y)") ++ " (1024 X x " ++ show nb ++ " Y)", gal)
         , ("example " ++ show e ++ ": c = g.f is extensive, monotone, idempotent (closure operator)", ext && mono && idem)
         , ("example " ++ show e ++ ": the " ++ show (pN l) ++ " closed sets form a lattice, meet = intersection, join = c(union)" ++ (if e == 2 then ", 2^4 = 16 of them" else ""), ok && (e == 1 || pN l == 16)) ]
    fgf = and [ fOf 1 (gOf 1 (fOf 1 x)) == fOf 1 x | x <- [0 .. allO] ]

-- ---------- やってみよう ----------
-- 1. X = {3, 5} にすると f(X) と c(X) は？ 3 と 5 の両方が持つ性質を全部持つ数は？
-- 2. 性質を 1 つずつ Y に入れて、「Y ⊆ f(X)」と「X ⊆ g(Y)」の答えがいつも同じことを確かめよう
-- 3. スペースで X を c(X) にする。もう一度押すと変わる？（冪等）
-- 4. props に「6 以上」（("6+", '6', (>= 6))）を足すと閉じた集合の束はどう変わる？（08 の形式概念分析と同じしくみ）
-- 5. 例 2 の h を x mod 3 に変えると、閉じた集合はいくつ？


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
