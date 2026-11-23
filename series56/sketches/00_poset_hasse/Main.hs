-- ============================================
-- 56-00 半順序とハッセ図 ─ 「a ≤ b」の矢印から、順序の表とハッセ図を計算する
-- 半順序 = 反射律（a ≤ a）・反対称律（a ≤ b かつ b ≤ a なら a = b）・推移律（a ≤ b ≤ c なら a ≤ c）
-- 矢印を足りない分だけ足す（推移的閉包）→ 間に何もはさまらない矢印だけ残す（被覆 = ハッセ図の辺）→ ランクで高さをそろえる
-- 操作：左の何もない所をクリックで要素を追加 / 要素から要素へドラッグで「a ≤ b」の矢印 / D 最後の矢印を消す / C 全部消す
--       1 2 3 例（推移律が足りない・約数・輪になる）/ スペース 閉包で足した矢印を表示 / R 例 1 にもどす
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.List (sortBy, foldl')
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

maxNodes :: Int
maxNodes = 16                 -- ★ 要素の数の上限

nodeR :: Float
nodeR = 18                    -- ★ 要素の丸の半径

-- 自分で置いた要素（画面の位置と名前）
data Node = Node { ndX, ndY :: !Float, ndName :: String }

-- 矢印を調べた結果
data Info = Info
  { iR0    :: !Rel                -- 矢印（と a ≤ a）の表
  , iC     :: !Rel                -- 閉包
  , iMiss  :: [(Int, Int)]        -- 閉包で足した組
  , iCyc   :: Maybe (Int, Int)    -- 反対称律が破れる組
  , iTrans :: !Bool
  , iLat   :: Maybe Poset }       -- 半順序ならハッセ図

data World = World
  { wNodes    :: [Node]
  , wArrows   :: [(Int, Int)]
  , wDragFrom :: !Int
  , wShowCl   :: !Bool
  , wInfo     :: Info
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-00 Posets and Hasse diagrams" (preset env0w 1 world0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (preset env 1 world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }
  where env0w = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World [] [] (-1) True (analyze1 0 0 [] []) []

names :: String
names = "abcdefghijklmnop"

-- 例を読み込む（位置は 0〜1 で、画面の左半分に広げる）
preset :: Env -> Int -> World -> World
preset env k w = analyze env (w { wNodes = nds, wArrows = ar })
  where
    wd = envW env * 0.5
    h = envH env
    place pts = [ Node (40 + x * (wd - 80)) (140 + y * (h - 260)) [c] | ((x, y), c) <- zip pts names ]
    divs = [1, 2, 3, 4, 6, 12] :: [Int]
    (nds, ar) = case k of
      2 -> ( [ nd { ndName = show d } | (nd, d) <- zip (place [(0.5, 1), (0.15, 0.55), (0.85, 0.7), (0.3, 0.15), (0.7, 0.3), (0.5, 0)]) divs ]
           , [ (i, j) | i <- [0 .. 5], j <- [0 .. 5], i /= j, getAt 1 divs j `mod` getAt 1 divs i == 0 ] )
      3 -> (place [(0.2, 0.8), (0.8, 0.8), (0.5, 0.2), (0.5, 0.55)], [(0, 1), (1, 2), (2, 0), (0, 3)])
      _ -> (place [(0.2, 0.9), (0.1, 0.5), (0.5, 0.2), (0.6, 0.6), (0.8, 0.0), (0.15, 0.1)], [(0, 1), (1, 2), (0, 3), (3, 2), (2, 4), (1, 5)])

-- 矢印（と a ≤ a）の表
relFrom :: Int -> [(Int, Int)] -> Rel
relFrom n ar = runSTUArray $ do
  r <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ [0 .. n - 1] $ \i -> writeArray r (i, i) True
  forM_ ar $ \(a, b) -> when (a < n && b < n) (writeArray r (a, b) True)
  pure r

-- 矢印の表 → 閉包 → 半順序か調べる → ハッセ図
analyze :: Env -> World -> World
analyze env w = w { wInfo = analyze1 (envW env) (envH env) (wNodes w) (wArrows w) }

analyze1 :: Float -> Float -> [Node] -> [(Int, Int)] -> Info
analyze1 wd h nds ar = Info r0 c miss cyc (isTransitive r0) lat
  where
    n = length nds
    r0 = relFrom n ar
    c = closeRel r0
    miss = [ (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], c ! (i, j), not (r0 ! (i, j)) ]
    cyc = case [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], c ! (i, j), c ! (j, i) ] of
      (p : _) -> Just p
      [] -> Nothing
    lat | cyc == Nothing && n > 0 = Just (layoutHasse (posetFromRel c (map ndName nds)) (wd * 0.56) 170 (wd * 0.4) (h - 300) False)
        | otherwise = Nothing

isCover :: Info -> Int -> Int -> Bool
isCover inf a b = maybe False (\p -> (a, b) `elem` pCov p) (iLat inf)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgbC 60 66 110) (Line [(wd * 0.52, 110), (wd * 0.52, h - 40)]) ]
  -- ---- 左：自分で描いた矢印（ハッセ図に残る辺 = 水色、推移律で言える辺 = 灰色、閉包で足した辺 = 点線の橙）
  ++ (if wShowCl w then [ arrowLine (nd a) (nd b) (rgbC 255 180 90) True | (a, b) <- iMiss inf ] else [])
  ++ [ arrowLine (nd a) (nd b) (if iCyc inf /= Nothing then rgbC 255 110 110 else if isCover inf a b then rgbC 100 220 220 else rgbC 130 136 170) False
     | (a, b) <- wArrows w ]
  ++ [ Color (rgbC 255 220 120) (thickLine 2 (nd (wDragFrom w)) mouse) | wDragFrom w >= 0 ]
  ++ concat [ [ Color bgCol (at (ndX x, ndY x) (disc (nodeR + 1))), Color (rgbC 226 232 248) (at (ndX x, ndY x) (disc (nodeR - 1)))
              , sayM (ndName x) (ndX x) (ndY x + 1) 15 bgCol ] | x <- wNodes w ]
  -- ---- 右：ハッセ図（半順序のときだけ）
  ++ (case (iLat inf, iCyc inf) of
        (Just p, _) -> [ drawHasse p hopt { oRad = nodeR, oFs = 15 }
                       , say ("Hasse diagram: " ++ show (length (pCov p)) ++ " covers, height = rank") (wd * 0.56) 140 15 (rgbC 200 206 230) ]
        (_, Just (a, b)) -> [ say ("Not antisymmetric: " ++ nm a ++ " <= " ++ nm b ++ " and " ++ nm b ++ " <= " ++ nm a) (wd * 0.56) 140 16 (rgbC 255 120 120)
                            , say "Arrows form a cycle -> no Hasse diagram (D)" (wd * 0.56) 166 15 (rgbC 255 120 120) ]
        _ -> [])
  -- ---- 文字
  ++ [ say ("Posets and Hasse diagrams   elements " ++ show n ++ "   arrows " ++ show (length (wArrows w)) ++ "   pairs a < b in the closure: " ++ show (np - n)) 20 36 18 (grayC 240)
     , say ("Click add   Drag arrow a <= b   D delete   C clear   1 2 3 examples   Space closure arrows " ++ (if wShowCl w then "on" else "off") ++ "   R reset") 20 62 15 (grayC 175)
     , say ("Reflexive " ++ st True ++ " (a <= a added)   Antisymmetric " ++ st (iCyc inf == Nothing) ++ "   Transitive " ++ st (iTrans inf)
            ++ (if iTrans inf then "" else " (closure adds " ++ show (length (iMiss inf)) ++ " = orange dashes)")
            ++ "  -> " ++ (if iCyc inf == Nothing then "partial order" else "not a partial order")) 20 88 15 (rgbC 255 210 130)
     , say "cyan = arrows kept in the Hasse diagram (covers)   gray = implied by transitivity, omitted" 20 (h - 16) 14 (rgbC 150 156 190)
     , drawSelfTest env (wTest w) ]
  where
    wd = envW env
    h = envH env
    inf = wInfo w
    n = length (wNodes w)
    nd i = case drop i (wNodes w) of
      (x : _) | i >= 0 -> (ndX x, ndY x)
      _ -> (0, 0)
    nm i = maybe "?" ndName (lookup i (zip [0 ..] (wNodes w)))
    mouse = toP5 env (envMouse env)
    st ok = if ok then "OK" else "NO"
    np = length [ () | ((_, _), True) <- assocsU (iC inf) ]

-- 矢印（dashed = 点線）
arrowLine :: (Float, Float) -> (Float, Float) -> Color -> Bool -> Picture
arrowLine (px, py) (qx, qy) c dashed
  | l < 1 = Blank
  | otherwise = Color c $ Pictures $
      (if dashed
         then [ thickLine 2 (pt t) (pt (t + 0.05)) | t <- takeWhile (< 1) (iterate (+ 0.1) (0 :: Double)) ]
         else [thickLine 2 (x1, y1) (x2, y2)])
      ++ [Polygon [(x2, y2), (x2 - ux * 12 - uy * 6, y2 - uy * 12 + ux * 6), (x2 - ux * 12 + uy * 6, y2 - uy * 12 - ux * 6)]]
  where
    dx = qx - px
    dy = qy - py
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    x1 = px + ux * nodeR
    y1 = py + uy * nodeR
    x2 = qx - ux * (nodeR + 2)
    y2 = qy - uy * (nodeR + 2)
    pt t = let tf = realToFrac t in (x1 + (x2 - x1) * tf, y1 + (y2 - y1) * tf)

nodeNear :: [Node] -> (Float, Float) -> Int
nodeNear nds (x, y) = case [ i | (i, nd) <- zip [0 ..] nds, sqrt ((ndX nd - x) ^ (2 :: Int) + (ndY nd - y) ^ (2 :: Int)) < nodeR + 4 ] of
  (i : _) -> i
  [] -> -1

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        i = nodeNear (wNodes w) (mx, my)
    in if mx > envW env * 0.52 || my < 110 then w
       else if i >= 0 then w { wDragFrom = i }
       else if length (wNodes w) < maxNodes
         then analyze env w { wNodes = wNodes w ++ [Node mx my [getAt '?' names (length (wNodes w))]] }
         else w
  EventKey (MouseButton LeftButton) Up _ pos
    | wDragFrom w < 0 -> w
    | otherwise ->
        let j = nodeNear (wNodes w) (toP5 env pos)
            a = wDragFrom w
            w' = w { wDragFrom = -1 }
        in if j >= 0 && j /= a && (a, j) `notElem` wArrows w then analyze env w' { wArrows = wArrows w ++ [(a, j)] } else w'
  EventKey (Char c) Down _ _
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
    | c `elem` "rR" -> preset env 1 w
    | c `elem` "cC" -> analyze env w { wNodes = [], wArrows = [], wDragFrom = -1 }
    | c `elem` "dD\b\DEL" -> delLast
  EventKey (SpecialKey KeyBackspace) Down _ _ -> delLast
  EventKey (SpecialKey KeyDelete) Down _ _ -> delLast
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShowCl = not (wShowCl w) }
  _ -> w
  where delLast = if null (wArrows w) then w else analyze env w { wArrows = init (wArrows w) }

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("chain 0 <= 1 <= 2 <= 3: arrows alone not transitive, closure is a poset with 6 pairs '<', 3 covers"
    , not (isTransitive chain) && isPoset cc && npc - 4 == 6 && length (coversOf cc) == 3)
  , ("cyclic relation a <= b <= c <= a: after closure antisymmetry fails (not a poset)"
    , not (isAntisym (closeRel (relFrom 3 [(0, 1), (1, 2), (2, 0)]))))
  , ("a made-up relation (reflexive, antisymmetric, not transitive) is recognised"
    , isReflexive bad && isAntisym bad && not (isTransitive bad))
  , ("divisors of 12: 6 elements, 7 covers, rank of 12 = 3 (number of prime factors)"
    , pN d12 == 6 && length (pCov d12) == 7 && rankOf d12 5 == 3)
  , ("30 random posets: closure of covers = order, no cover can be dropped (minimal), rank grows along edges", okAll) ]
  where
    chain = relFrom 4 [(0, 1), (1, 2), (2, 3)]
    cc = closeRel chain
    npc = length (filter id (elemsU cc))
    bad = relFrom 3 [(0, 1), (1, 2)]
    d12 = divisorPoset 12
    okAll = and (fst (foldl' one ([], seedRng 56) [1 .. 30 :: Int]))
    one (acc, r0) _ =
      let (u, r1) = rnd r0
          (lat, r2) = randomPoset (4 + floor (u * 10)) 0.3 r1
          n = pN lat
          c2 = closeRel (relFrom n (pCov lat))
          ok1 = elemsU (pR lat) == elemsU c2                                   -- 被覆の閉包 = もとの順序
          ok2 = and [ not (closeRel (relFrom n [ e | (m, e) <- zip [0 :: Int ..] (pCov lat), m /= k ]) ! (a, b))
                    | (k, (a, b)) <- zip [0 ..] (pCov lat) ]                     -- どの辺も省けない
          ok3 = and [ rankOf lat b >= rankOf lat a + 1 | (a, b) <- pCov lat ]
      in ((ok1 && ok2 && ok3) : acc, r2)

-- ---------- やってみよう ----------
-- 1. 例 1：a ≤ b ≤ c なのに a ≤ c の矢印がない。橙の点線が閉包で足した矢印。ハッセ図ではどれが残る？
-- 2. 例 2：12 の約数の「割り切る」矢印を全部描いた。ハッセ図では何本に減る？
-- 3. 例 3 は輪になっている。どの矢印を消すと半順序になる？（D で最後の矢印を消す）
-- 4. C で消して、自分の 1 日の予定（「起きる ≤ 朝ごはん」など）を半順序にしてみよう
-- 5. 同じハッセ図になる矢印の描き方は何通りある？
-- 6. Haskell では順序の表は UArray (Int, Int) Bool。closeRel（ワーシャル法）を読んでみよう


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
