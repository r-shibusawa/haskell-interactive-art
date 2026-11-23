-- ============================================
-- 56-06 ブール代数と論理回路 ─ 論理式 → 真理値表・カルノー図 → いちばん短い式 → ゲートの数
-- 論理式は 4 変数のブール代数の元。クワイン-マクラスキー法で主項（これ以上 文字を消せない積）をすべて求め、
-- 主項の組み合わせを総当たりして、文字の数が最小の積和形（最簡積和形）を選ぶ。回路のゲートの数をくらべる
-- 操作：ボタンかキーで式を組む（A B C D、! = ¬、& = ∧、| = ∨、( )）/ Backspace 1 つ消す / 0 全部消す / 1〜5 例
--       カルノー図をクリックで 0 ⇔ 1
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (sortBy, foldl', intercalate, sort, nub)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

vars :: String
vars = "ABCD"

-- ★ 例（! = ¬、& = ∧、| = ∨）
presets :: [String]
presets = ["(A&B)|(A&!B)|(!A&B&C)", "(A&B)|(A&C)|(B&C)|(A&B&C)", "(!A&!B)|(!A&B&D)|(A&B&D)|(A&!B&!D)", "!(A&B)&(C|D)|(A&B&C&D)", "(A&!B)|(!A&B)"]

-- ボタン（最後の 2 つは「1 つ消す」と「全部消す」）
buttons :: [String]
buttons = ["A", "B", "C", "D", "!", "&", "|", "(", ")", "<-", "Clr"]

-- 論理式
data Expr = Var !Int | Not Expr | And Expr Expr | Or Expr Expr

-- ゲートの数
data Gates = Gates { gAnd, gOr, gNot :: !Int }

-- 簡単にした結果：主項、選んだ項、文字の数、最小の組み合わせの数、もとの式のゲート
type Term = (Int, Int)                  -- (値 v, 消した文字 d)。d のビットは「どちらでもよい」
data Sol = Sol { sPrimes, sTerms :: [Term], sLit, sNbest :: !Int, sOrig :: Gates }

data World = World
  { wToks    :: String
  , wTT      :: !Int
  , wErr     :: String
  , wFromMap :: !Bool
  , wSol     :: Sol
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-06 Boolean algebra and logic circuits" (loadExpr (getAt "" presets 0) world0))
  { skBack  = bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadExpr (getAt "" presets 0) world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] 0 "" False ((minimize 0) { sOrig = Gates 0 0 0 }) []

-- ---- 式の読み取り（! が最も強く、次に &、最後に |）。読めないときは理由を返す
parse :: String -> Either String Expr
parse tk = do
  (e, rest) <- orE tk
  case rest of
    [] -> Right e
    (t : _) -> Left ("'" ++ [t] ++ "' is extra")
  where
    atom ts = case ts of
      ('!' : r) -> do { (a, r') <- atom r; Right (Not a, r') }
      ('(' : r) -> do
        (e, r') <- orE r
        case r' of
          (')' : r'') -> Right (e, r'')
          _ -> Left "expected )"
      (t : r) | t `elem` vars -> Right (Var (length (takeWhile (/= t) vars)), r)
      (t : _) -> Left ("'" ++ [t] ++ "' cannot go here")
      [] -> Left "the expression ends too early"
    andE ts = atom ts >>= loopA
    loopA (e, '&' : r) = do { (b, r') <- atom r; loopA (And e b, r') }
    loopA x = Right x
    orE ts = andE ts >>= loopO
    loopO (e, '|' : r) = do { (b, r') <- andE r; loopO (Or e b, r') }
    loopO x = Right x

evalE :: Expr -> Int -> Int
evalE e m = case e of
  Var v -> (m `shiftR` (3 - v)) .&. 1
  Not a -> 1 - evalE a m
  And a b -> evalE a m .&. evalE b m
  Or a b -> evalE a m .|. evalE b m

-- 16 ビットで真理値表（m = 8A + 4B + 2C + D）
truthOf :: Expr -> Int
truthOf e = foldl' (\t m -> if evalE e m == 1 then t .|. (1 `shiftL` m) else t) 0 [0 .. 15]

gatesOf :: Expr -> Gates
gatesOf e = case e of
  Var _ -> Gates 0 0 0
  Not a -> let g = gatesOf a in g { gNot = gNot g + 1 }
  And a b -> let g = plus (gatesOf a) (gatesOf b) in g { gAnd = gAnd g + 1 }
  Or a b -> let g = plus (gatesOf a) (gatesOf b) in g { gOr = gOr g + 1 }
  where plus (Gates a1 o1 n1) (Gates a2 o2 n2) = Gates (a1 + a2) (o1 + o2) (n1 + n2)

truthStr :: String -> Maybe Int
truthStr s = either (const Nothing) (Just . truthOf) (parse s)

loadExpr :: String -> World -> World
loadExpr s w = reparse w { wToks = s, wFromMap = False }

reparse :: World -> World
reparse w = case parse (wToks w) of
  Right e -> let t = truthOf e in w { wTT = t, wErr = "", wSol = (minimize t) { sOrig = gatesOf e } }
  Left msg -> w { wErr = msg }

-- ---- クワイン-マクラスキー法：1 文字だけちがう 2 項をまとめていく。まとめられなかった項が主項
primesQM :: Int -> [Term]
primesQM t = go [ (m, 0) | m <- [0 .. 15], testBit t m ] []
  where
    go [] acc = acc
    go cur acc =
      let ca = A.listArray (0, length cur - 1) cur
          nc = length cur
          pairs = [ (i, j, (v1 .&. complement x, d1 .|. x))
                  | i <- [0 .. nc - 1], j <- [i + 1 .. nc - 1]
                  , let (v1, d1) = ca A.! i, let (v2, d2) = ca A.! j, let x = v1 `xor` v2
                  , d1 == d2, popc x == 1 ]
          used = S.fromList (concat [ [i, j] | (i, j, _) <- pairs ])
          acc' = acc ++ [ c | (i, c) <- zip [0 ..] cur, not (S.member i used) ]
          next = dedup S.empty [ p | (_, _, p) <- pairs ]     -- 同じ項は最初の 1 つだけ（順番はそのまま）
      in go next acc'
    dedup _ [] = []
    dedup s (p : ps) = if S.member p s then dedup s ps else p : dedup (S.insert p s) ps

cellsOf :: Term -> Int
cellsOf (v, d) = foldl' (\s m -> if (m .&. complement d) == v then s .|. (1 `shiftL` m) else s) 0 [0 .. 15]

litsOf :: Term -> Int
litsOf (_, d) = 4 - popc d

-- 主項の部分集合を総当たり：全部の 1 をおおう中で文字数（同じなら項の数）が最小
minimize :: Int -> Sol
minimize t = Sol pr [ p | (i, p) <- zip [0 ..] pr, testBit bestS i ] bestL nb (Gates 0 0 0)
  where
    pr = primesQM t
    np = length pr
    cellA = listArray (0, max 0 (np - 1)) (map cellsOf pr ++ [0 | np == 0]) :: UArray Int Int
    litA = listArray (0, max 0 (np - 1)) (map litsOf pr ++ [0 | np == 0]) :: UArray Int Int
    sub !s = let walk !i !cov !lit !k
                   | i >= np = (cov, lit, k)
                   | testBit s i = walk (i + 1) (cov .|. cellA ! i) (lit + litA ! i) (k + 1)
                   | otherwise = walk (i + 1) cov lit k
             in walk 0 0 0 (0 :: Int)
    -- (見つかったか, 文字数, 項の数, 部分集合, 同点の数)
    step (!found, !bl, !bk, !bs, !n) s =
      let (cov, lit, k) = sub s
      in if cov /= t then (found, bl, bk, bs, n)
         else if not found || lit < bl || (lit == bl && k < bk) then (True, lit, k, s, 1 :: Int)
         else if lit == bl && k == bk then (found, bl, bk, bs, n + 1)
         else (found, bl, bk, bs, n)
    (_, bestL, _, bestS, nb) = foldl' step (False, 0, 0, 0 :: Int, 0) [0 .. (1 `shiftL` np) - 1]

termStr :: Term -> String
termStr (v, d)
  | d == 15 = "1"
  | otherwise = concat [ (if v .&. b /= 0 then "" else "!") ++ [getAt '?' vars q] | q <- [0 .. 3], let b = 1 `shiftL` (3 - q), d .&. b == 0 ]

sopGates :: [Term] -> Gates
sopGates terms = Gates (sum [ max 0 (litsOf p - 1) | p <- terms ]) (max 0 (length terms - 1)) (length (nub nots))
  where nots = [ q | (v, d) <- terms, q <- [0 .. 3 :: Int], let b = 1 `shiftL` (3 - q), d .&. b == 0, v .&. b == 0 ]

termCols :: [Color]
termCols = [rgbC 90 225 215, rgbC 255 190 90, rgbC 255 120 170, rgbC 170 150 255, rgbC 140 225 120, rgbC 255 240 120, rgbC 120 170 255, rgbC 255 150 100]

tc :: Int -> Color
tc k = getAt white termCols (k `mod` 8)

grayOrd :: [Int]
grayOrd = [0, 1, 3, 2]

kx, ky :: Float
kx = 170
ky = 190

cellSize :: Env -> Float
cellSize env = min 70 ((envH env - 330) / 4)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Boolean algebra and logic circuits   expression -> truth table -> K-map -> minimal SOP -> circuit" 20 36 18 (grayC 240)
  , say "Buttons / keys (A B C D ! & | ( )) build it   Backspace delete   0 clear   1-5 examples   Click K-map 0 <-> 1" 20 62 15 (grayC 175) ]
  ++ concat [ [ Color (rgbC 40 46 90) (rectTL (20 + fromIntegral k * 50) 80 44 34), Color (rgbC 90 100 160) (frameTL 1 (20 + fromIntegral k * 50) 80 44 34)
              , sayM b (42 + fromIntegral k * 50) 97 (if k > 9 then 14 else 17) (grayC 230) ]
            | (k, b) <- zip [0 :: Int ..] buttons ]
  ++ [ say fStr 590 106 fSize white ]
  ++ [ say ("Not complete yet: " ++ wErr w ++ " (showing the last valid one)") 590 132 15 (rgbC 255 120 120) | not (null (wErr w)) ]
  -- ---- 真理値表
  ++ [ say "A B C D | f" 20 ty 14 (grayC 170) ]
  ++ [ say (unwords [ show ((m `shiftR` q) .&. 1) | q <- [3, 2, 1, 0] ] ++ " | " ++ show v) 24 (ty + 22 + fromIntegral m * 21) 14 (if v == 1 then rgbC 255 220 130 else rgbC 140 146 180)
     | m <- [0 .. 15 :: Int], let v = (tt `shiftR` m) .&. 1 ]
  -- ---- カルノー図（行 CD、列 AB をグレイ符号 00 01 11 10 の順に）
  ++ [ say "AB ->" kx (ky - 30) 14 (grayC 170), say "CD" (kx - 40) (ky - 8) 14 (grayC 170) ]
  ++ [ sayC lb (kx + fromIntegral c * cs + cs / 2) (ky - 8) 14 (grayC 170) | (c, lb) <- zip [0 :: Int ..] codes ]
  ++ [ say lb (kx - 26) (ky + fromIntegral r * cs + cs / 2 + 5) 14 (grayC 170) | (r, lb) <- zip [0 :: Int ..] codes ]
  ++ concat [ let m = getAt 0 grayOrd c * 4 + getAt 0 grayOrd r
                  v = (tt `shiftR` m) .&. 1
                  x0 = kx + fromIntegral c * cs
                  y0 = ky + fromIntegral r * cs
              in [ Color (if v == 1 then rgbC 70 76 130 else rgbC 28 32 64) (rectTL x0 y0 cs cs), Color (rgbC 70 80 130) (frameTL 1 x0 y0 cs cs)
                 , sayM (show v) (x0 + cs / 2) (y0 + cs / 2) 18 (if v == 1 then white else grayC 110) ]
                 ++ [ Color (tc k) (at (x0 + 9 + 11 * fromIntegral (k `mod` 5), y0 + cs - 9) (disc 4)) | (k, p) <- zip [0 ..] (sTerms sol), testBit (cellsOf p) m ]
            | r <- [0 .. 3 :: Int], c <- [0 .. 3 :: Int] ]
  -- ---- 結果と回路
  ++ wrapSay 16 (envW env - px - 20) px y 22 (grayC 230)
       ("Prime implicants (Quine-McCluskey) " ++ show (length (sPrimes sol)) ++ ": " ++ intercalate ", " (map termStr (sPrimes sol)))
  ++ [ say "Minimal SOP: f = " px (y + 70) 20 white ]
  ++ sopPics
  ++ [ say (show (sLit sol) ++ " literals, " ++ show (length (sTerms sol)) ++ " terms (" ++ show (sNbest sol) ++ " minimal choices; tried all 2^"
            ++ show (length (sPrimes sol)) ++ " subsets of primes)") px (y + 98) 15 (grayC 200) ]
  ++ [ say ("Original gates: AND " ++ show (gAnd g0) ++ ", OR " ++ show (gOr g0) ++ ", NOT " ++ show (gNot g0) ++ " (total " ++ show (gAnd g0 + gOr g0 + gNot g0) ++ ")") px (y + 128) 15 (grayC 200) | not (wFromMap w) ]
  ++ [ say ("Simplified: AND " ++ show (gAnd g1) ++ ", OR " ++ show (gOr g1) ++ ", NOT " ++ show (gNot g1) ++ " (total " ++ show (gAnd g1 + gOr g1 + gNot g1) ++ ", 2-input gates)") px (y + 152) 15 (rgbC 140 225 180)
     , drawCircuit sol px (y + 190) (min (envW env - px - 30) 520) (envH env - y - 240)
     , drawSelfTest env (wTest w) ]
  where
    tt = wTT w
    sol = wSol w
    cs = cellSize env
    ty = 170
    codes = ["00", "01", "11", "10"]
    px = kx + 4 * cs + 50
    y = 180
    g0 = sOrig sol
    g1 = sopGates (sTerms sol)
    fStr = "f = " ++ (if wFromMap w then "Sum m(" ++ intercalate "," [ show m | m <- [0 .. 15 :: Int], testBit tt m ] ++ ") (from the K-map)"
                      else wToks w ++ (if envFrame env `mod` 60 < 30 then "_" else ""))
    fSize = max 14 (min 20 ((envW env - 600) / (0.66 * fromIntegral (max 1 (length fStr)))))
    sopX = px + adv "Minimal SOP: f = "
    adv str = fromIntegral (length str) * 20 * 0.62     -- 文字の幅の目安（gloss の文字は少し広い）
    sopPics = if null (sTerms sol) then [ say "0" sopX (y + 70) 20 white ]
              else snd (foldl' (\(x, acc) (k, p) -> let s = termStr p ++ (if k < length (sTerms sol) - 1 then " | " else "")
                                                    in (x + adv s, acc ++ [say s x (y + 70) 20 (tc k)])) (sopX, []) (zip [0 ..] (sTerms sol)))

-- 入力の線 → AND（&）→ OR（>=1）
drawCircuit :: Sol -> Float -> Float -> Float -> Float -> Picture
drawCircuit sol x y w h = Pictures $
  [ Color (rgbC 90 100 160) (frameTL 1 (x - 10) (y - 10) (w + 20) (h + 10)) ]
  ++ concat [ [ Color (grayC 150) (Line [(x + 20 + fromIntegral q * 28, y), (x + 20 + fromIntegral q * 28, y + h - 10)])
              , say [getAt '?' vars q] (x + 15 + fromIntegral q * 28) (y + 4) 14 (grayC 220) ] | q <- [0 .. 3] ]
  ++ concat (zipWith term [0 ..] ts)
  ++ [ Color (rgbC 30 34 70) (rectTL ox (oy - 22) 44 44), Color (grayC 230) (frameTL 1.5 ox (oy - 22) 44 44)
     , Color (grayC 230) (thickLine 1.5 (ox + 44, oy) (ox + 64, oy))
     , say ">=1" (ox + 6) (oy + 5) 14 (grayC 230), say "f" (ox + 58) (oy - 6) 14 (grayC 230)
     , Color (rgbC 255 120 120) (at (x + 4, y + h - 19) (disc 4)), say "= NOT" (x + 12) (y + h - 14) 14 (rgbC 255 120 120) ]
  where
    ts = sTerms sol
    n = max 1 (length ts)
    gh = min 44 (h / fromIntegral n - 8)
    ox = x + w - 70
    oy = y + h / 2
    term k p@(v, d) =
      let gy = y + 10 + fromIntegral k * (h - 20) / fromIntegral n
          ax = x + 170
          lits = [ q | q <- [0 .. 3], (d .&. (1 `shiftL` (3 - q))) == 0 ]
          nl = length lits
          c = tc k
      in concat [ let yy = gy + 8 + fromIntegral li * 9
                  in Color c (Line [(x + 20 + fromIntegral q * 28, yy), (ax, yy)])
                     : [ Color (rgbC 255 120 120) (at (ax - 14, yy) (disc 3)) | v .&. (1 `shiftL` (3 - q)) == 0 ]
                | (li, q) <- zip [0 :: Int ..] lits ]
         ++ (if litsOf p > 1
               then [ Color (rgbC 30 34 70) (rectTL ax gy 40 (max gh (fromIntegral nl * 9 + 8))), Color c (frameTL 1 ax gy 40 (max gh (fromIntegral nl * 9 + 8)))
                    , say "&" (ax + 15) (gy + gh / 2 + 5) 14 (grayC 230), Color c (Line [(ax + 40, gy + gh / 2), (ox, oy)]) ]
               else [ Color c (Line [(ax, gy + 8), (ox, oy)]) ])   -- 1 文字の項は AND がいらない（線をそのまま OR へ）

addTok :: String -> World -> World
addTok t w = reparse w { wToks = toks', wFromMap = False }
  where toks' | t == "<-" = if null (wToks w) then [] else init (wToks w)
              | t == "Clr" = []
              | otherwise = wToks w ++ t

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        cs = cellSize env
        c = floor ((mx - kx) / cs) :: Int
        r = floor ((my - ky) / cs) :: Int
    in case [ b | (k, b) <- zip [0 :: Int ..] buttons, mx > 20 + fromIntegral k * 50, mx < 64 + fromIntegral k * 50, my > 80, my < 114 ] of
         (b : _) -> addTok b w
         [] | c >= 0 && c < 4 && r >= 0 && r < 4 ->
                let t = wTT w `xor` (1 `shiftL` (getAt 0 grayOrd c * 4 + getAt 0 grayOrd r))
                in w { wTT = t, wFromMap = True, wErr = "", wSol = (minimize t) { sOrig = Gates 0 0 0 } }
            | otherwise -> w
  EventKey (SpecialKey KeyBackspace) Down _ _ -> addTok "<-" w
  EventKey (SpecialKey KeyDelete) Down _ _ -> addTok "<-" w
  EventKey (Char ch) Down _ _
    | ch == '\b' || ch == '\DEL' -> addTok "<-" w
    | ch == '0' -> addTok "Clr" w
    | ch `elem` "12345" -> loadExpr (getAt "" presets (fromEnum ch - fromEnum '1')) w
    | otherwise -> case lookup ch keyMap of
        Just t -> addTok [t] w
        Nothing -> w
  _ -> w
  where keyMap = zip "abcdABCD!~&*|+()" "ABCDABCD!!&&||()"

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("De Morgan: !(A&B) and !A|!B agree on all 16 rows", isJust dm1 && dm1 == truthStr "!A|!B")
  , ("5 examples + 40 random functions: the simplified formula equals the function on all 16 rows", eqOk)
  , ("Quine-McCluskey primes = primes found by checking all 81 products (same set)", prOk)
  , ("example 1 is A | BC (3 literals), majority is AB | AC | BC (6 literals)"
    , sort (map termStr (sTerms s1)) == ["A", "BC"] && sLit maj == 6 && length (sTerms maj) == 3)
  , ("4-variable XOR: 8 terms, 32 literals (nothing merges)", sLit xr == 32 && length (sTerms xr) == 8 && length (sPrimes xr) == 8)
  , ("cyclic function Sum m(0,1,2,5,6,7) (3 variables): minimal form 3 terms, 6 literals, 2 ways"
    , sLit cyc == 6 && length (sTerms cyc) == 3 && sNbest cyc == 2) ]
  where
    dm1 = truthStr "!(A&B)"
    pres = [ maybe 0 id (truthStr s) | s <- presets ]
    rands = fst (foldl' (\(acc, r) _ -> let (u, r') = rnd r in (acc ++ [floor (u * 65536)], r')) ([], seedRng 6) [1 .. 40 :: Int])
    fs = pres ++ rands
    sols = [ (t, minimize t) | t <- fs ]
    eqOk = and [ foldl' (.|.) 0 (map cellsOf (sTerms s)) == t | (t, s) <- sols ]
    -- 81 個の積を全部調べた主項
    bruteP t = let imp = [ (v, d) | v <- [0 .. 15], d <- [0 .. 15], v .&. d == 0, cellsOf (v, d) .&. complement t == 0 ]
                   isImp = S.fromList imp
               in [ (v, d) | (v, d) <- imp, and [ not (S.member (v .&. complement b, d .|. b) isImp) | q <- [0 .. 3], let b = 1 `shiftL` q, d .&. b == 0 ] ]
    prOk = and [ sort (bruteP t) == sort (sPrimes s) | (t, s) <- sols ]
    s1 = minimize (getAt 0 fs 0)
    maj = minimize (getAt 0 fs 1)
    xr = minimize 0x6996
    cyc = minimize (foldl' (\t m -> t .|. (3 `shiftL` (2 * m))) 0 [0, 1, 2, 5, 6, 7])   -- D は関係なし

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
-- 1. 例 1 の式は長いのに、最簡形は A | BC。カルノー図で 1 のかたまりを見つけよう
-- 2. カルノー図のマスをクリックして、項が 1 つになる形、2 つになる形を作ろう（大きな四角 = 短い項）
-- 3. 例 5（XOR）は簡単にならない。カルノー図が市松模様だから
-- 4. !(A&B) と !A|!B を入力して、真理値表が同じになることを確かめよう（ド・モルガン）
-- 5. 4 変数の論理関数は全部で 2^16 = 65536 個。これは 16 個の原子をもつブール代数（05）

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
