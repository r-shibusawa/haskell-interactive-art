-- ============================================
-- 69-13 正直者とうそつき ─ ある島の住人は、いつも本当のことを言う正直者か、いつもうそを言ううそつき。
-- 「A が言う S」は「A が正直者 ⇔ S が正しい」という論理式になる（a = A が正直者）。全員の式を & でつなぎ、
-- 割り当て（だれが正直者か）を全部ためすと、答えが 1 つ・いくつも・1 つもない、のどれかがわかる。
-- 「私はうそつき」は a <-> ~a で、どの割り当てでも偽：うそつきのパラドックス
-- 操作：1〜6 問題 / 人のカードの言葉をクリック = 次の言葉（Shift + クリック = 前）/ N 人を足す（4 人まで）/ X 人を減らす
--       R でたらめな問題
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (findIndex, foldl', intercalate, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

names :: [String]
names = ["A", "B", "C", "D"]

-- 言葉の種類（数は人の番号 0〜3）
data Stmt = SNone | SKnight Int | SLiar Int | SSame Int Int | SDiff Int Int | SIfThen Int Int
          | SBothLiars Int Int | SExactly Int | SAtLeastOneLiar | SAllLiars
  deriving (Eq, Show)

puzzles :: [(String, [Stmt])]                 -- ★ 問題：人ごとの言葉
puzzles =
  [ ("classic", [SLiar 1, SSame 0 1])
  , ("both liars?", [SAllLiars, SNone])
  , ("chain of liars", [SLiar 1, SLiar 2, SBothLiars 0 1])
  , ("\"I am a knight\"", [SKnight 0, SNone])
  , ("the liar paradox", [SLiar 0, SNone])
  , ("counting testimony", [SExactly 1, SExactly 2, SAtLeastOneLiar]) ]

data World = World
  { wPeople :: [Stmt]
  , wP      :: !Int                           -- 問題の番号（-1 = 自分で作った問題）
  , wRng    :: !Rng                           -- R の乱数（自己テストのあとの続き）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-13 Knights and Knaves" (loadPuzzle 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPuzzle 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

-- 自己テストが種 13 の乱数を使ったあとの続きから R の問題を作る（p5.js 版と同じ列）
world0 :: World
world0 = World [] 0 rngAfterTest []

loadPuzzle :: Int -> World -> World
loadPuzzle k w = w { wP = k, wPeople = snd (at' puzzles k ("", [])) }

-- ---- 言葉 → 英語と論理式（変数 a b c d = その人が正直者）
vname :: Int -> String
vname i = [at' "abcd" i 'a']

stmtText :: Stmt -> Int -> String
stmtText s me = case s of
  SNone -> "(says nothing)"
  SKnight i -> if i == me then "I am a knight" else nm i ++ " is a knight"
  SLiar i -> if i == me then "I am a liar" else nm i ++ " is a liar"
  SSame i j -> nm i ++ " and " ++ nm j ++ " are the same kind"
  SDiff i j -> nm i ++ " and " ++ nm j ++ " are different kinds"
  SIfThen i j -> "if " ++ (if i == me then "I am" else nm i ++ " is") ++ " a knight, so " ++ (if j == me then "am I" else "is " ++ nm j)
  SBothLiars i j -> nm i ++ " and " ++ nm j ++ " are both liars"
  SExactly k -> "exactly " ++ show k ++ " of us " ++ (if k == 1 then "is a knight" else "are knights")
  SAtLeastOneLiar -> "at least one of us is a liar"
  SAllLiars -> "all of us are liars"
  where nm i = if i == me then "I" else at' names i "?"

-- 言葉の論理式（n = 人数）
stmtFml :: Stmt -> Int -> F
stmtFml s n = case s of
  SNone -> FTop
  SKnight i -> v i
  SLiar i -> FNot (v i)
  SSame i j -> FBin OIff (v i) (v j)
  SDiff i j -> FNot (FBin OIff (v i) (v j))
  SIfThen i j -> FBin OImp (v i) (v j)
  SBothLiars i j -> FBin OAnd (FNot (v i)) (FNot (v j))
  SAtLeastOneLiar -> ors [ FNot (v i) | i <- [0 .. n - 1] ]
  SAllLiars -> ands [ FNot (v i) | i <- [0 .. n - 1] ]
  SExactly k ->                               -- ちょうど k 人：正直者の組み合わせを全部 | でつなぐ
    let terms = [ ands [ if testBit m i then v i else FNot (v i) | i <- [0 .. n - 1] ] | m <- [0 .. 2 ^ n - 1 :: Int], popCount' m == k ]
    in if null terms then FBot else ors terms
  where v = FVar . vname
        ands = foldl1' (FBin OAnd)
        ors = foldl1' (FBin OOr)
        foldl1' g xs = case xs of
          (x : r) -> foldl' g x r
          [] -> FTop

popCount' :: Int -> Int
popCount' m = if m <= 0 then 0 else (m `mod` 2) + popCount' (m `div` 2)

-- 同じ意味を、式を使わずに直接計算（自己テストでくらべる）
stmtDirect :: Stmt -> [Bool] -> Int -> Bool
stmtDirect s env n = case s of
  SNone -> True
  SKnight i -> e i
  SLiar i -> not (e i)
  SSame i j -> e i == e j
  SDiff i j -> e i /= e j
  SIfThen i j -> not (e i) || e j
  SBothLiars i j -> not (e i) && not (e j)
  SExactly c -> k == c
  SAtLeastOneLiar -> k < n
  SAllLiars -> k == 0
  where k = length (filter id env)
        e i = at' env i False

personFml :: [Stmt] -> Int -> F                -- 「A の式」= a <-> A の言葉
personFml ps i = FBin OIff (FVar (vname i)) (stmtFml (at' ps i SNone) (length ps))

puzzleFml :: [Stmt] -> F
puzzleFml ps = case [ personFml ps i | i <- [0 .. length ps - 1] ] of
  (x : r) -> foldl' (FBin OAnd) x r
  [] -> FTop

-- k 番目の割り当て（上の桁が A）
asgOf :: Int -> Int -> Asg
asgOf n m = M.fromList [ (vname i, testBit m (n - 1 - i)) | i <- [0 .. n - 1] ]

-- ★ 解く：全部の割り当て（2ⁿ 通り）で、全員の「正直者 ⇔ 言葉が正しい」が成り立つものを集める
solve :: [Stmt] -> (F, [[Bool]])
solve ps =
  let n = length ps
      f = puzzleFml ps
  in (f, [ [ testBit m (n - 1 - i) | i <- [0 .. n - 1] ] | m <- [0 .. 2 ^ n - 1 :: Int], evalFml f (asgOf n m) ])

solveDirect :: [Stmt] -> [[Bool]]
solveDirect ps =
  let n = length ps
  in [ env | m <- [0 .. 2 ^ n - 1 :: Int], let env = [ testBit m (n - 1 - i) | i <- [0 .. n - 1] ]
           , and [ at' env i False == stmtDirect s env n | (i, s) <- zip [0 ..] ps ] ]

-- その人が言える言葉の一覧
choices :: Int -> Int -> [Stmt]
choices _ n =
  [SNone] ++ concat [ [SKnight i, SLiar i] | i <- [0 .. n - 1] ]
  ++ concat [ [SSame i j, SDiff i j, SBothLiars i j] | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
  ++ [ SIfThen i j | i <- [0 .. n - 1], j <- [0 .. n - 1], i /= j ]
  ++ [ SExactly k | k <- [0 .. n] ]
  ++ [SAtLeastOneLiar, SAllLiars]

-- 言葉を次（dir = 1）か前（dir = -1）に
cycleStmt :: Int -> Int -> World -> World
cycleStmt i dir w =
  let ps = wPeople w
      cs = choices i (length ps)
      k = maybe (-1) id (findIndex (== at' ps i SNone) cs)
      s = at' cs (wrapI (k + dir) (length cs)) SNone
  in w { wPeople = setAt i s ps, wP = -1 }

randomPuzzle :: Int -> R [Stmt]
randomPuzzle n = mapM (\i -> rndPick (drop 1 (choices i n))) [0 .. n - 1]

-- 人を減らしたあと、いない人を指す言葉を直す
fixRefs :: [Stmt] -> [Stmt]
fixRefs ps = map fix ps
  where
    n = length ps
    fix s = case s of
      SExactly k -> if k > n then SExactly n else s
      _ -> if any (>= n) (refs s) then SNone else s
    refs s = case s of
      SKnight i -> [i]
      SLiar i -> [i]
      SSame i j -> [i, j]
      SDiff i j -> [i, j]
      SIfThen i j -> [i, j]
      SBothLiars i j -> [i, j]
      _ -> []

-- ---- カードの位置 (番号, x, y, 幅, 高さ)
cardBoxes :: Env -> World -> [(Int, Float, Float, Float, Float)]
cardBoxes env w =
  let n = length (wPeople w)
      cw = min 280 ((envW env - 60) / i2f n - 16)
  in [ (i, 30 + i2f i * (cw + 16), 96, cw, 150) | i <- [0 .. n - 1] ]

draw :: Env -> World -> Picture
draw env w =
  let ps = wPeople w
      n = length ps
      (sf, models) = solve ps
      unique = case models of
        [m] -> Just m
        _ -> Nothing
      title = "Knights and knaves: " ++ (if wP w >= 0 then "puzzle " ++ show (wP w + 1) ++ " " ++ fst (at' puzzles (wP w) ("", [])) else "your own puzzle")
      card (i, x, y, cw, ch) =
        let s = at' ps i SNone
            role = fmap (\m -> at' m i False) unique
            rc = case role of
              Just True -> grn
              Just False -> pink
              Nothing -> linec
        in Pictures $
             [ chalkPanel x y cw ch, Color (col rc) (frameTL 2 x y cw ch)
             , say (at' names i "?") (x + 16) (y + 38) 28 (col ink) ]
             ++ [ say (if r then "knight" else "liar") (x + 56) (y + 36) 16 (col (if r then grn else pink)) | Just r <- [role] ]
             ++ [ say ln (x + 16) (y + 78 + 22 * i2f k) 17 (col yel) | (k, ln) <- zip [0 :: Int ..] (wrapText 17 (cw - 24) 2 ("\"" ++ stmtText s i ++ "\"")) ]
             ++ [ say ln (x + 16) (y + 128 + 18 * i2f k) 14 (col dimc) | (k, ln) <- zip [0 :: Int ..] (wrapText 14 (cw - 24) 1 (fmlStr (personFml ps i))) ]
      -- ---- 真理値表
      y0 = 290
      rows = 2 ^ n :: Int
      rh = min 26 ((envH env - y0 - 140) / i2f rows)
      colA' = 50 + i2f n * 44 + 40
      allX = colA' + i2f n * 80
      header = [ sayM (at' names i "?") (50 + i2f i * 44) y0 14 (col sky) | i <- [0 .. n - 1] ]
               ++ [ sayM (at' names i "?" ++ "'s eq") (colA' + i2f i * 80) y0 14 (col dimc) | i <- [0 .. n - 1] ]
               ++ [ sayM "all" allX y0 14 (col yel) ]
      row m =
        let e = asgOf n m
            y = y0 + 22 + i2f m * rh
            ok = evalFml sf e
        in [ Color (col linec) (rectTL 30 (y - rh / 2) (allX + 20) (rh - 2)) | ok ]
           ++ [ sayM (if b then "K" else "L") (50 + i2f i * 44) y 14 (col (if b then grn else pink)) | i <- [0 .. n - 1], let b = M.findWithDefault False (vname i) e ]
           ++ [ sayM (if v then "OK" else "x") (colA' + i2f i * 80) y 14 (col (if v then grn else dimc)) | i <- [0 .. n - 1], let v = evalFml (personFml ps i) e ]
           ++ [ sayM "answer" allX y 14 (col yel) | ok ]
      lx = allX + 80
      legend = [ "How to read:", "K = knight, L = liar (a = A is a knight)", "\"A's eq\" = a <-> (A's statement is true)"
               , "rows where every eq is OK are answers", ""
               , "Knights always tell the truth,", "liars always lie.", "So \"A says S\" becomes a <-> S."
               , "There are only 2^(people) assignments,", "so trying them all always solves it." ]
      legendPic = [ say ln lx (y0 + 4 + 22 * i2f k) 14 (col dimc) | (k, ln) <- zip [0 :: Int ..] legend ]
      -- ---- 結論
      by = envH env - 104
      verdict
        | Just m <- unique = say ("Exactly one answer: " ++ intercalate ", " [ at' names i "?" ++ " is a " ++ (if b then "knight" else "liar") | (i, b) <- zip [0 ..] m ]) 30 by 20 (col grn)
        | not (null models) = say (show (length models) ++ " answers: these statements do not decide it") 30 by 20 (col yel)
        | otherwise = say ("No answer: every assignment contradicts"
                           ++ (if or [ s == SLiar i | (i, s) <- zip [0 ..] ps ] then " (\"I am a liar\" = a <-> ~a: the liar paradox)" else "")) 30 by 20 (col pink)
      whole = wrapText 14 (envW env - 60) 2 ("Whole formula: " ++ clip 150 (fmlStr sf))
  in view env $
       [ hudTitle title "1-6 puzzles   click a statement = next (Shift+click = previous)   N add a person (max 4)   X remove   R random puzzle" ]
       ++ map card (cardBoxes env w)
       ++ header ++ concatMap row [0 .. rows - 1] ++ legendPic
       ++ [verdict]
       ++ [ say ln 30 (by + 30 + 18 * i2f k) 14 (col dimc) | (k, ln) <- zip [0 :: Int ..] whole ]
       ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

-- 単語の区切りで折り返す（size の文字、幅 wd、最大 maxL 行。はみ出す分は ... で切る）
wrapText :: Float -> Float -> Int -> String -> [String]
wrapText size wd maxL s =
  let perLine = max 4 (floor (wd / (size * 0.72)))
      go [] cur = [cur | not (null cur)]
      go (x : xs) cur
        | null cur = go xs x
        | length cur + 1 + length x <= perLine = go xs (cur ++ " " ++ x)
        | otherwise = cur : go xs x
      ls = concatMap (chop perLine) (go (words s) "")
      chop k t = if length t <= k then [t] else take k t : chop k (drop k t)
  in if length ls > maxL then take (maxL - 1) ls ++ [clip perLine (unwords (drop (maxL - 1) ls))] else ls

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down mods pos ->
    let m = toP5 env pos
        dir = if shift mods == Down then -1 else 1
    in case [ i | (i, x, y, cw, ch) <- cardBoxes env w, inBox m (x, y, cw, ch) ] of
         (i : _) -> cycleStmt i dir w
         [] -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '6' -> loadPuzzle (fromEnum c - fromEnum '1') w
    | isKey 'n' c -> if length (wPeople w) < 4 then w { wPeople = wPeople w ++ [SNone], wP = -1 } else w
    | isKey 'x' c -> if length (wPeople w) > 1 then w { wPeople = fixRefs (dropLast (wPeople w)), wP = -1 } else w
    | isKey 'r' c -> let (ps, g) = runR (do n <- rndInt 2 4; randomPuzzle n) (wRng w)
                     in w { wPeople = ps, wP = -1, wRng = g }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 問題 1 を自分で解いてから、右の表で確かめよう。「B が言う A と B は同じ種類」は、実は「A は正直者」と同じ意味（なぜ？）
-- 2. 問題 4「私は正直者」はなぜ答えが決まらない？ 問題 5「私はうそつき」はなぜ答えがない？
-- 3. 言葉をクリックで変えて、答えがただ 1 つになる 3 人の問題を作ろう。友だちに出題してみよう
-- 4. 「正直者はちょうど k 人」は & と | で書くと長い式になる。全体の式の長さを見よう
-- 5. 11 の「この文は証明できない」は、うそつきの文の「うそ」を「証明できない」にかえたもの。こちらは矛盾しない

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
-- でたらめな問題 300 個（種 13）。返り値 (問題の列, そのあとの乱数)
testPuzzles :: ([[Stmt]], Rng)
testPuzzles = runR (mapM (\_ -> do n <- rndInt 2 4; randomPuzzle n) [1 .. 300 :: Int]) (rngOf 13)

rngAfterTest :: Rng
rngAfterTest = snd testPuzzles

selfTest :: [(String, Bool)]
selfTest =
  [ ("問題 1：A は正直者、B はうそつき（ただ 1 つ）", ans 0 == ["正う"])
  , ("問題 2：「2 人ともうそつき」と言う A はうそつき、B は正直者", ans 1 == ["う正"])
  , ("問題 3：うそつきの連鎖は B だけが正直者", ans 2 == ["う正う"])
  , ("問題 4「私は正直者」は答えが 2 通り、問題 5「私はうそつき」は答えなし", length (ans 3) == 2 && null (ans 4))
  , ("うそつきの文 L ↔ ¬L は矛盾（真理値表で 2 行とも偽）", kindOf (FBin OIff (FVar "l") (FNot (FVar "l"))) == Contra)
  , ("でたらめな問題 " ++ show nT ++ " 個：論理式の真理値表で解いた答え = 言葉を直接計算した総当たりの答え", agree == nT)
  , ("答えなし " ++ show k0 ++ "・ただ 1 つ " ++ show k1 ++ "・複数 " ++ show k2 ++ " の全部が出て、答えはどれも全員の言葉と合う"
    , k0 > 0 && k1 > 0 && k2 > 0 && sat) ]
  where
    ans k = [ map (\v -> if v then '正' else 'う') m | m <- snd (solve (snd (at' puzzles k ("", [])))) ]
    tps = fst testPuzzles
    nT = length tps
    agree = length [ () | ps <- tps, snd (solve ps) == solveDirect ps ]
    cnt c = length [ () | ps <- tps, min 2 (length (snd (solve ps))) == c ]
    k0 = cnt 0
    k1 = cnt 1
    k2 = cnt 2
    sat = and [ and [ at' m i False == stmtDirect s m (length ps) | (i, s) <- zip [0 ..] ps ] | ps <- tps, m <- snd (solve ps) ]

-- ==================================================
-- ここから下は「論理の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 画面の記号は ASCII：~ = ¬、& = ∧、| = ∨、-> = →、<-> = ↔、T = ⊤、_|_ = ⊥、[] = □、<> = ◇、\ = λ
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（黒板とチョーク）
type RGB = (Float, Float, Float)

bgc, ink, yel, sky, pink, grn, lilac, dimc, panelc, linec :: RGB
bgc = (40, 54, 47)                            -- 背景（黒板の灰緑）
ink = (238, 238, 228)                         -- 文字（白いチョーク）
yel = (246, 218, 120)
sky = (128, 186, 222)
pink = (238, 146, 150)
grn = (164, 214, 146)
lilac = (196, 170, 230)
dimc = (150, 166, 156)
panelc = (50, 66, 58)
linec = (92, 112, 102)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- i 番目を取りのぞく（splice(i, 1)）
removeAt :: Int -> [a] -> [a]
removeAt i xs = [x | (k, x) <- zip [0 ..] xs, k /= i]

-- (k + n) % n（負の数でも 0〜n−1）
wrapI :: Int -> Int -> Int
wrapI k n = if n <= 0 then 0 else k `mod` n

hyp :: Float -> Float -> Float                -- 長さ √(a² + b²)（Math.hypot）
hyp a b = sqrt (a * a + b * b)

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ。[...new Set(xs)]）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- 長い文字列を n 文字で切る（p5.js 版の s.slice(0, n - 1) + '…'）
clip :: Int -> String -> String
clip n s = if length s > n then take (n - 1) s ++ "..." else s

-- 画面に出せない文字（ASCII 以外）を ? に
asciiOnly :: String -> String
asciiOnly = map (\c -> if c >= ' ' && c <= '~' then c else '?')

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab・Esc が SpecialKey で来る）
-- TClear = 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じるので Shift + Backspace でも）
data TypedKey = TChar Char | TBack | TClear | TEnter | TTab | TUp | TDown | TLeft | TRight | TOther
  deriving (Eq, Show)

typedKey :: Key -> Modifiers -> TypedKey
typedKey k m = case k of
  Char '\b' -> back
  Char '\DEL' -> back
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char '\ESC' -> TClear
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> back
  SpecialKey KeyDelete -> back                -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  SpecialKey KeyEsc -> TClear
  SpecialKey KeyUp -> TUp
  SpecialKey KeyDown -> TDown
  SpecialKey KeyLeft -> TLeft
  SpecialKey KeyRight -> TRight
  _ -> TOther
  where back = if shift m == Down then TClear else TBack

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

type PErr = (String, Int)                     -- (まちがいの説明, 何文字目)

-- まちがいの説明を 1 行に（p5.js 版の「（n 文字目あたり）」）
errLine :: PErr -> String
errLine (m, a) = m ++ " (near character " ++ show (a + 1) ++ ")"

-- 数の書き方：小数点以下 d 桁（toFixed と同じく、半分なら大きいほうへ）
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndPick :: [a] -> R a                         -- リストから 1 つ（空のリストには使わない）
rndPick xs = do
  u <- rnd
  let i = floor (u * fromIntegral (length xs))
  pure (case drop i xs of
          (x : _) -> x
          [] -> case xs of
            (x : _) -> x
            [] -> errorWithoutStackTrace "rndPick: empty list")

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 論理式の道具：字句 → 構文木（再帰下降）→ 文字列・真理値
-- 構文木 = FVar 名前 / FTop ⊤ / FBot ⊥ / FNot a / FBin 演算子 a b（様相論理だけ FBox a □ と FDia a ◇）
-- 強さの順：~ [] <> ＞ & ＞ | ＞ ->（右から結合：p->q->r = p->(q->r)）＞ <->
-- ASCII で書く：~ ! = ¬、& ^ = ∧、| = ∨、-> > = →、<-> = = ↔、1 T = ⊤、0 _|_ = ⊥、[] = □、<> = ◇（記号のままでも読める）
data Op = OAnd | OOr | OImp | OIff deriving (Eq, Ord, Show)

data F = FVar String | FTop | FBot | FNot F | FBin Op F F | FBox F | FDia F
  deriving (Eq, Ord, Show)

fmlKeys :: String                             -- 入力できる文字
fmlKeys = ['a' .. 'z'] ++ ['0' .. '9'] ++ "~!&|^-<>=()[] T_"

fmlMaxVars :: Int
fmlMaxVars = 6                                -- ★ 変数の数の上限（真理値表は 2⁶ = 64 行まで）

opSym :: Op -> String                         -- 画面の記号（ASCII）
opSym o = case o of
  OAnd -> "&"
  OOr -> "|"
  OImp -> "->"
  OIff -> "<->"

fVar :: String -> F
fVar = FVar

fNot :: F -> F
fNot = FNot

fBin :: Op -> F -> F -> F
fBin = FBin

fKids :: F -> [F]                             -- 子（構文木の下の段）
fKids f = case f of
  FNot a -> [a]
  FBox a -> [a]
  FDia a -> [a]
  FBin _ a b -> [a, b]
  _ -> []

data Tk = TkVar String | TkNot | TkAnd | TkOr | TkImp | TkIff | TkL | TkR | TkTop | TkBot | TkBox | TkDia
  deriving (Eq, Show)

tkSym :: Tk -> String
tkSym t = case t of
  TkNot -> "~"
  TkAnd -> "&"
  TkOr -> "|"
  TkImp -> "->"
  TkIff -> "<->"
  TkL -> "("
  TkR -> ")"
  TkTop -> "T"
  TkBot -> "_|_"
  TkBox -> "[]"
  TkDia -> "<>"
  TkVar n -> n

lexFormula :: Bool -> String -> Either PErr [(Tk, Int)]
lexFormula modal src = go 0 src
  where
    go _ [] = Right []
    go i s@(c : rest)
      | c == ' ' = go (i + 1) rest
      | pre "<->" = tok TkIff 3
      | pre "->" = tok TkImp 2
      | pre "_|_" = tok TkBot 3
      | pre "[]" || pre "<>" = if modal then tok (if c == '[' then TkBox else TkDia) 2
                               else Left ("[] <> (box, diamond) only in modal logic (12)", i)
      | c >= 'a' && c <= 'z' =
          let tl = takeWhile (\d -> (d >= '0' && d <= '9') || d == '\'') rest
              k = 1 + length tl
          in ((TkVar (c : tl), i) :) <$> go (i + k) (drop (k - 1) rest)
      | Just t <- lookup c one =
          if (t == TkBox || t == TkDia) && not modal then Left ("box and diamond only in modal logic (12)", i)
          else tok t 1
      | c `elem` "-<[]_" = Left ("'" ++ [c] ++ "' alone is not a symbol (write -> <-> [] <>)", i)
      | otherwise = Left ("character '" ++ asciiOnly [c] ++ "' not allowed", i)
      where pre p = take (length p) s == p
            tok t k = ((t, i) :) <$> go (i + k) (drop k s)
    one = [ ('¬', TkNot), ('~', TkNot), ('!', TkNot), ('∧', TkAnd), ('&', TkAnd), ('^', TkAnd), ('∨', TkOr), ('|', TkOr)
          , ('→', TkImp), ('>', TkImp), ('↔', TkIff), ('=', TkIff), ('(', TkL), (')', TkR)
          , ('⊤', TkTop), ('1', TkTop), ('T', TkTop), ('⊥', TkBot), ('0', TkBot), ('□', TkBox), ('◇', TkDia) ]

-- 返り値 Right 式 / Left (説明, 何文字目)（例外は出さない）。modal = 様相論理の □ ◇ を使えるか
parseFormulaM :: Bool -> String -> Either PErr F
parseFormulaM modal src
  | length src > 160 = Left ("too long (160 characters max)", 160)
  | otherwise = do
      toks <- lexFormula modal src
      if null toks then Left ("empty formula", 0) else do
        (f, rest) <- pIff 0 toks
        case rest of
          ((t, a) : _) -> Left (leftover t, a)
          [] -> let nv = length (fmlVars f)
                in if nv > fmlMaxVars then Left (show nv ++ " variables (" ++ show fmlMaxVars ++ " max)", 0) else Right f
  where
    len = length src
    atOf ts = case ts of
      ((_, a) : _) -> a
      [] -> len
    failAt ts m = Left (m, atOf ts)
    leftover t = case t of
      TkR -> "extra ')'"
      TkVar _ -> needOp
      TkL -> needOp
      TkTop -> needOp
      TkBot -> needOp
      TkNot -> needOp
      _ -> "'" ++ tkSym t ++ "' in a strange place"
    needOp = "an operator (& | -> ...) is needed here"
    -- 2 項演算の 1 段：左結合（right なら右結合）
    binL sub op d ts = do
      (a, r) <- sub d ts
      loop a r
      where loop a r = case r of
              ((t, _) : r') | t == op -> do (b, r2) <- sub d r'; loop (FBin (tkOp op) a b) r2
              _ -> Right (a, r)
    binR sub op d ts = do
      (a, r) <- sub d ts
      case r of
        ((t, _) : r') | t == op -> do (b, r2) <- binR sub op d r'; Right (FBin (tkOp op) a b, r2)
        _ -> Right (a, r)
    tkOp t = case t of
      TkAnd -> OAnd
      TkOr -> OOr
      TkImp -> OImp
      _ -> OIff
    pIff = binL pImp TkIff
    pImp = binR pOr TkImp
    pOr = binL pAnd TkOr
    pAnd = binL pUn TkAnd
    pUn d ts
      | d + 1 > 80 = failAt ts "nesting too deep"
      | otherwise = case ts of
          [] -> failAt ts "the formula ends too early"
          ((t, _) : r) -> case t of
            TkNot -> do (a, r2) <- pUn (d + 1) r; Right (FNot a, r2)
            TkBox -> do (a, r2) <- pUn (d + 1) r; Right (FBox a, r2)
            TkDia -> do (a, r2) <- pUn (d + 1) r; Right (FDia a, r2)
            TkVar nm -> Right (FVar nm, r)
            TkTop -> Right (FTop, r)
            TkBot -> Right (FBot, r)
            TkL -> do
              (a, r2) <- pIff (d + 1) r
              case r2 of
                ((TkR, _) : r3) -> Right (a, r3)
                _ -> failAt r2 "')' missing"
            TkR -> failAt ts "nothing before ')'"
            _ -> failAt ts ("nothing before '" ++ tkSym t ++ "'")

parseFormula :: String -> Either PErr F
parseFormula = parseFormulaM False

fmlPrec :: F -> Int                           -- 2 項演算の強さ（2 項演算でなければ 0）
fmlPrec f = case f of
  FBin OIff _ _ -> 1
  FBin OImp _ _ -> 2
  FBin OOr _ _ -> 3
  FBin OAnd _ _ -> 4
  _ -> 0

-- 構文木 → 文字列（かっこは必要な所と、ちがう演算子が混ざる所に）
fmlStr :: F -> String
fmlStr f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot a -> "~" ++ un a
  FBox a -> "[]" ++ un a
  FDia a -> "<>" ++ un a
  FBin op a b ->
    let side c isLeft = case c of
          FBin op2 _ _ -> let assocOk = op2 == op && (if op == OImp then not isLeft else isLeft)
                          in if assocOk then fmlStr c else "(" ++ fmlStr c ++ ")"
          _ -> fmlStr c
    in side a True ++ " " ++ opSym op ++ " " ++ side b False
  where un a = if fmlPrec a > 0 then "(" ++ fmlStr a ++ ")" else fmlStr a

-- 出てくる変数（名前の順）
fmlVars :: F -> [String]
fmlVars f = S.toList (go f S.empty)
  where go g s = case g of
          FVar n -> S.insert n s
          _ -> foldr go s (fKids g)

type Asg = M.Map String Bool                  -- 割り当て（変数 → 真偽）

-- 真理値（□ ◇ は真理値表では評価しない → 偽。12 を見よ）
evalFml :: F -> Asg -> Bool
evalFml f e = case f of
  FVar n -> M.findWithDefault False n e
  FTop -> True
  FBot -> False
  FNot a -> not (evalFml a e)
  FBin OAnd a b -> evalFml a e && evalFml b e
  FBin OOr a b -> evalFml a e || evalFml b e
  FBin OImp a b -> not (evalFml a e) || evalFml b e
  FBin OIff a b -> evalFml a e == evalFml b e
  _ -> False

-- k 番目の割り当て（上の桁が最初の変数）
envOf :: [String] -> Int -> Asg
envOf vars k = M.fromList [ (v, testBit k (length vars - 1 - i)) | (i, v) <- zip [0 ..] vars ]

-- 真理値表の行 (割り当て, 値)
truthRowsV :: [String] -> F -> [(Asg, Bool)]
truthRowsV vars f = [ (e, evalFml f e) | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

truthRows :: F -> [(Asg, Bool)]
truthRows f = truthRowsV (fmlVars f) f

data Kind = Taut | Sat | Contra deriving (Eq, Show)

-- 恒真 / 充足可能（恒真ではない）/ 矛盾、真の行の数、行の数
classifyFml :: F -> (Kind, Int, Int)
classifyFml f =
  let rows = truthRows f
      n = length (filter snd rows)
      m = length rows
  in (if n == m then Taut else if n == 0 then Contra else Sat, n, m)

kindOf :: F -> Kind
kindOf f = let (k, _, _) = classifyFml f in k

kindName :: Kind -> String
kindName k = case k of
  Taut -> "TAUTOLOGY (valid)"
  Sat -> "SATISFIABLE (not valid)"
  Contra -> "CONTRADICTION (unsatisfiable)"

fmlEq :: F -> F -> Bool
fmlEq = (==)

fmlSize :: F -> Int
fmlSize f = 1 + sum (map fmlSize (fKids f))

-- 全部の割り当てで同じ値か（同値）
sameTable :: F -> F -> Bool
sameTable f g =
  let vars = S.toList (S.fromList (fmlVars f ++ fmlVars g))
  in and [ evalFml f e == evalFml g e | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

-- でたらめな式（自己テスト用、種つき乱数。p5.js 版と同じ順に引く）
randomFml :: Int -> [String] -> Bool -> R F
randomFml d vars consts = do
  r <- rnd
  if d <= 0 || r < 0.22 then do
    c <- if consts then (< 0.12) <$> rnd else pure False
    if c then do u <- rnd; pure (if u < 0.5 then FTop else FBot)
    else FVar <$> rndPick vars
  else if r < 0.38 then FNot <$> randomFml (d - 1) vars consts
  else do
    op <- rndPick [OAnd, OOr, OImp, OIff, OAnd, OOr]
    a <- randomFml (d - 1) vars consts
    b <- randomFml (d - 1) vars consts
    pure (FBin op a b)

fmlTreeLabel :: F -> String                   -- 木の丸の中の文字
fmlTreeLabel f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot _ -> "~"
  FBox _ -> "[]"
  FDia _ -> "<>"
  FBin op _ _ -> opSym op

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx > bx && mx < bx + bw && my > by && my < by + bh

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 黒板のパネル（p5.js 版の角の丸い長方形）
chalkPanel :: Float -> Float -> Float -> Float -> Picture
chalkPanel x y w h = Color (col panelc) (rectTL x y w h)

-- 入力欄：ラベル、文字列（点滅するカーソル）、まちがいの説明。active = False なら灰色でカーソルなし
drawInputLine :: Int -> Float -> Float -> String -> String -> String -> Bool -> Picture
drawInputLine frame x y lab s err active =
  let lx = x + textW lab 15 + 8
      blink = active && frame `mod` 60 < 30
  in Pictures
       [ say lab x y 15 (col (if active then yel else dimc))
       , say (asciiOnly s ++ (if blink then "_" else "")) lx (y + 1) 20 (col ink)
       , if null err then Blank else say ("! " ++ asciiOnly err) lx (y + 22) 14 (col pink) ]

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  hSetEncoding stdout utf8
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 小さな表示（x, y は文字の左下。右端からはみ出さないように左へ寄せる）
drawSelfTest :: Env -> Float -> Float -> [(String, Bool)] -> Picture
drawSelfTest env x y rs = say (selfTestLine rs) (min x (envW env - 240)) y 14 (if all snd rs then col grn else col pink)
