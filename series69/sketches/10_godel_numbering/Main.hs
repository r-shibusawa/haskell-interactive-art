-- ============================================
-- 69-10 ゲーデル数 ─ 記号に番号をつけ、式（記号の並び）を素数のべき 2^a × 3^b × 5^c × … の 1 つの数にする
-- 素因数分解はただ 1 通りなので、数から式に必ず戻せる。証明（式の並び）も 2^g1 × 3^g2 × … で 1 つの数になる。
-- すると「式 g の最初の記号は ∀（画面では A）」のような「式についての話」が「g は 2^10 で割り切れて 2^11 では割り切れない」という
-- 「数についての話」になる。これが不完全性定理（11）の出発点。数は Haskell の Integer（桁数の制限なし）
-- 操作：文字キー = 入力（0 s = + * ( ) ~ -> と A = ∀、E = ∃、x y z。> だけでも →）/ Tab 式 ⇔ 数（数字を入れて式に戻す）
--       ↑↓ お手本 / Backspace 1 字消す / Shift + Backspace全部消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (elemIndex, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

symbols :: [String]                           -- ★ 記号の番号は 1, 2, 3, …（この並びの順。p5.js 版と同じ記号）
symbols = ["0", "s", "=", "+", "×", "(", ")", "¬", "→", "∀", "∃", "x", "y", "z"]

symAscii :: [String]                          -- 画面に出す形（symbols と同じ順）
symAscii = ["0", "s", "=", "+", "*", "(", ")", "~", "->", "A", "E", "x", "y", "z"]

presets :: [String]                           -- ★ お手本の式
presets = ["0 = 0", "s0 + s0 = ss0", "Ax (x + 0 = x)", "Ax ~(sx = 0)", "Ey (y = ss0)", "Ax Ay (x + sy = s(x + y))"]

maxDigits, maxLen :: Int
maxDigits = 300                               -- ★ 数の桁数の上限
maxLen = 24                                   -- ★ 式の長さ（記号の数）の上限

primes :: [Integer]                           -- はじめの 40 個の素数
primes = take 40 (go [] 2)
  where go ps k = if all (\p -> k `mod` p /= 0) ps then k : go (ps ++ [k]) (k + 1) else go ps (k + 1)

primeAt :: Int -> Integer
primeAt i = at' primes i 1

-- 符号化の結果：記号の番号（1〜14）の並びと、ゲーデル数
data Enc = Enc { eCodes :: [Int], eG :: Integer }

data Mode = MFml | MNum deriving (Eq)

data World = World
  { wMode   :: !Mode                          -- いま入力している欄（式 / 数）
  , wFsrc   :: String
  , wNsrc   :: String
  , wP      :: !Int
  , wEnc    :: Maybe Enc
  , wEncErr :: String                         -- 式のまちがい（前の式はそのまま残る）
  , wDecErr :: String                         -- 数のまちがい
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-10 Godel Numbering" (loadPreset 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPreset 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World MFml "" "" 0 Nothing "" "" []

-- 文字列 → 記号の番号の並び（空白は飛ばす。ASCII の別名も読む）
toSyms :: String -> Either String [Int]
toSyms s0 = go s0 []
  where
    go s acc = case s of
      [] -> if null acc then Left "empty formula"
            else if length acc > maxLen then Left ("too long (" ++ show maxLen ++ " symbols max)")
            else Right (reverse acc)
      (' ' : r) -> go r acc
      ('-' : '>' : r) -> go r (9 : acc)
      (c : r) -> let ch = case c of
                       '*' -> "×"
                       '~' -> "¬"
                       '>' -> "→"
                       'A' -> "∀"
                       'E' -> "∃"
                       _ -> [c]
                 in case elemIndex ch symbols of
                      Just i -> go r (i + 1 : acc)
                      Nothing -> Left ("symbol '" ++ asciiOnly [c] ++ "' not allowed")

-- ★ 符号化：i 番目の記号の番号 c_i を、i 番目の素数の指数にする
godel :: [Int] -> Integer
godel codes = product [ primeAt i ^ c | (i, c) <- zip [0 ..] codes ]

encodeSyms :: [Int] -> Enc
encodeSyms codes = Enc codes (godel codes)

-- ★ 復号：2, 3, 5, … で割れるだけ割って指数を数える。素数が飛んだり、残りがあったら「ゲーデル数ではない」
exponents :: Integer -> Int -> Either String [Int]
exponents g0 maxExp
  | g0 < 2 = Left "enter a number of 2 or more"
  | otherwise = go g0 primes []
  where
    go g ps acc
      | g <= 1 = Right (reverse acc)
      | otherwise = case ps of
          [] -> Left "other prime factors remain -> not a Godel number"
          (p : rest) -> case divCount g p 0 of
            Left () -> Left ("the exponent of " ++ show p ++ " is over " ++ show maxExp)
            Right (0, _) -> Left ("not divisible by " ++ show p ++ " (a prime is skipped) -> not a Godel number")
            Right (e, g') -> go g' rest (e : acc)
    divCount g p !e
      | g `mod` p == 0 = if e + 1 > maxExp then Left () else divCount (g `div` p) p (e + 1)
      | otherwise = Right (e, g)

decodeNum :: Integer -> Either String [Int]
decodeNum g = do
  es <- exponents g (length symbols)
  case filter (> length symbols) es of
    (e : _) -> Left ("there is no symbol number " ++ show e)
    [] -> Right es

symShow :: Int -> String                      -- 番号 → 画面の記号
symShow c = at' symAscii (c - 1) "?"

-- BigInt の常用対数（だいたい）
log10Big :: Integer -> Double
log10Big g =
  let t = show g
      l = length t
  in i2d (l - 1) + logBase 10 (read (take 15 t) / 10 ^^ (min 15 l - 1))

bigShort :: Integer -> String
bigShort g = let s = show g
             in if length s > 90 then take 40 s ++ " ... " ++ drop (length s - 40) s ++ " (" ++ show (length s) ++ " digits)"
                else s ++ " (" ++ show (length s) ++ " digits)"

loadPreset :: Int -> World -> World
loadPreset i w = let k = wrapI i (length presets) in rebuild w { wP = k, wFsrc = at' presets k "", wMode = MFml }

rebuild :: World -> World
rebuild w = case wMode w of
  MFml -> case toSyms (wFsrc w) of
    Left e -> w { wEncErr = e, wDecErr = "" }
    Right cs -> let e = encodeSyms cs in w { wEnc = Just e, wEncErr = "", wNsrc = show (eG e), wDecErr = "" }
  MNum
    | null (wNsrc w) -> w { wDecErr = "enter a number" }
    | length (wNsrc w) > maxDigits -> w { wDecErr = show maxDigits ++ " digits max" }
    | otherwise -> case decodeNum (read (wNsrc w)) of
        Left e -> w { wDecErr = e }
        Right cs -> w { wDecErr = "", wEnc = Just (encodeSyms cs), wEncErr = "", wFsrc = unwords (map symShow cs) }

-- ★ 「式についての性質」を「数についての性質」として計算する（文字列は見ない）
divides :: Integer -> Integer -> Int -> Bool
divides g p e = g `mod` (p ^ e) == 0

arithFirstIsForall :: Integer -> Bool
arithFirstIsForall g = let c = 10 in divides g 2 c && not (divides g 2 (c + 1))   -- ∀ は 10 番

arithLength :: Integer -> Int
arithLength g = length (takeWhile (\p -> divides g p 1) primes)

arithHasNot :: Integer -> Bool
arithHasNot g = let c = 8 in or [ divides g p c && not (divides g p (c + 1)) | p <- take (arithLength g) primes ]   -- ¬ は 8 番

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Godel numbers: formula -> product of prime powers -> one number"
      "Type (* = times, ~ = not, -> or > = implies, A = for all, E = exists)   Tab formula <-> number\nUp/Down examples   Backspace delete   Shift+Backspace clear"
  , drawInputLine (envFrame env) 30 120 "Formula:" fShown (if wMode w == MFml then wEncErr w else "") (wMode w == MFml)
  , drawInputLine (envFrame env) 30 166 "Number: " (if length (wNsrc w) > 68 then take 66 (wNsrc w) ++ "..." else wNsrc w)
      (if wMode w == MNum then wDecErr w else "") (wMode w == MNum) ]
  ++ maybe [] (drawEnc env) (wEnc w)
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]
  where
    fShown = if wMode w == MFml then wFsrc w
             else case wEnc w of
               Just e | null (wEncErr w) -> unwords (map symShow (eCodes e))
               _ -> wFsrc w

drawEnc :: Env -> Enc -> [Picture]
drawEnc env e =
  let wd = envW env
      cs = eCodes e
      n = length cs
      cw = min 58 ((wd - 60) / i2f (max 1 n))
      y0 = 216
      boxes = concat
        [ [ chalkPanel x y0 (cw - 4) 92
          , sayM (symShow c) (x + cw / 2 - 2) (y0 + 18) 20 (col yel)
          , sayM ('#' : show c) (x + cw / 2 - 2) (y0 + 44) 14 (col ink)
          , sayM (show (primeAt i) ++ "^" ++ show c) (x + cw / 2 - 2) (y0 + 70) 14 (col sky) ]
        | (i, c) <- zip [0 ..] cs, let x = 30 + i2f i * cw ]
      prodLines = wrapW (wd - 60) 16 ("Godel number = " ++ joinWith " * " [ show (primeAt i) ++ "^" ++ show c | (i, c) <- zip [0 ..] cs ])
      yb = y0 + 148 + i2f (length prodLines - 1) * 20 + 32
      g = eG e
      y1 = yb + 46
      len = arithLength g
      row k q v w2 =
        [ say q 40 (y1 + 28 + i2f k * 24) 14 (col ink)
        , say ((if v then "yes" else "no") ++ " (checking the string: " ++ (if w2 then "yes" else "no") ++ ")") (wd * 0.62) (y1 + 28 + i2f k * 24) 14 (col (if v == w2 then grn else pink)) ]
      -- ---- 証明も 1 つの数
      y2 = y1 + 120
      lg = [ log10Big gi + logBase 10 (logBase 10 (fromIntegral (primeAt i))) | (i, gi) <- zip [0 ..] [g, 270] ]
      top = maxOr 0 lg
      lgDigits = top + logBase 10 (sum [ 10 ** (v - top) | v <- lg ])
      gTxt = if length (show g) > 12 then "(" ++ show (length (show g)) ++ "-digit number)" else show g
      digTxt = if lgDigits < 6 then "about " ++ show (floor (10 ** lgDigits + 0.5) :: Integer) else "about 10^" ++ fmtN 1 lgDigits
      exLines = wrapW (wd - 60) 14 ("Example: [this formula, 0 = 0] -> 2^" ++ gTxt ++ " * 3^270 = a number with " ++ digTxt
                                    ++ " digits (too big to write down, but a definite number)")
      yN = y2 + 26 + i2f (length exLines) * 20 + 14
  in boxes
     ++ [ say ("Symbol numbers: " ++ unwords [ s ++ "=" ++ show i | (i, s) <- zip [1 :: Int ..] symAscii ]) 30 (y0 + 116) 14 (col dimc) ]
     ++ [ say ln 30 (y0 + 148 + i2f k * 20) 16 (col ink) | (k, ln) <- zip [0 :: Int ..] prodLines ]
     ++ [ say ("= " ++ bigShort g) 30 yb 16 (col grn)
        -- ---- 数の性質 = 式の性質
        , say "Questions about the formula, answered only by dividing the number (no string is read)" 30 y1 15 (col yel) ]
     ++ row 0 "First symbol is A?  <=>  2^10 divides g, 2^11 does not" (arithFirstIsForall g) (take 1 cs == [10])
     ++ row 1 ("Length is " ++ show len ++ "?  <=>  " ++ show (primeAt (max 0 (len - 1))) ++ " divides g, " ++ show (primeAt len) ++ " does not") (len == n) True
     ++ row 2 "Contains ~?  <=>  for some i-th prime p: p^8 divides g, p^9 does not" (arithHasNot g) (8 `elem` cs)
     ++ [ say "A proof (a list of formulas) is one number too: 2^(1st formula) * 3^(2nd formula) * ..." 30 y2 15 (col yel) ]
     ++ [ say ln 30 (y2 + 26 + i2f k * 20) 14 (col ink) | (k, ln) <- zip [0 :: Int ..] exLines ]
     ++ [ say ln 30 (yN + i2f k * 19) 14 (col dimc)
        | (k, ln) <- zip [0 :: Int ..] (wrapW (wd - 60) 14 "'p is the number of a proof of g' is also written with * and division -> arithmetic can talk about its own proofs (on to the diagonal argument in 11)") ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TTab -> rebuild w { wMode = if wMode w == MFml then MNum else MFml }
    TUp -> loadPreset (wP w - 1) w
    TDown -> loadPreset (wP w + 1) w
    t -> rebuild (if wMode w == MFml then w { wFsrc = edit t (wFsrc w) "0s=+*()~->AExyz " 60 }
                  else w { wNsrc = edit t (wNsrc w) "0123456789" (maxDigits + 1) })
  _ -> w
  where
    edit t s keys mx = case t of
      TBack -> dropLast s
      TClear -> ""
      TChar c -> typeInto s c keys mx
      _ -> s

-- ---------- やってみよう ----------
-- 1. 「0 = 0」の 270 を Tab で数の欄に入れて、式に戻そう。271 や 540 を入れると？
-- 2. 記号が 1 つ増えるごとに数は何倍くらいになる？（次の素数の、記号の番号乗）
-- 3. 「長さ」や「~ をふくむ」のように、割り算だけで判定できる式の性質をもう 1 つ考えよう（例：最後の記号が ) ）
-- 4. symbols の順番を変えると数はどう変わる？ それでも戻せるのはなぜ？
-- 5. 11 の「この文は証明できない」は、自分のゲーデル数についての式。数が自分のことを話せるしくみがここにある

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("「0 = 0」のゲーデル数は 2^1 × 3^3 × 5^1 = 270", either (const False) (\cs -> eG (encodeSyms cs) == 270) (toSyms "0 = 0"))
  , ("はじめの 20 個の素数が正しい（2, 3, 5, …, 71）", take 20 primes == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71])
  , ("でたらめな記号の列 " ++ show nR ++ " 個：数にして戻すと同じ列", rt == nR)
  , ("ちがう列は必ずちがう数（素因数分解はただ 1 通り）", clash == 0)
  , ("割り算だけで判定した「最初は ∀」「長さ」「¬ をふくむ」が、文字列で見た答えと全部一致", props == nR)
  , ("ゲーデル数でない数は説明つきで戻せない：10（3 が飛ぶ）、1、2^20（番号 20 はない）、2·3·7、270·11"
    , all (either (const True) (const False) . decodeNum) [10, 1, 2 ^ (20 :: Int), 2 * 3 * 7, 270 * 11])
  , ("並び [270, 3, 5] を 2^270 × 3^3 × 5^5 にして戻すと [270, 3, 5]（証明の番号のしくみ）"
    , exponents (2 ^ (270 :: Int) * 3 ^ (3 :: Int) * 5 ^ (5 :: Int)) 1000 == Right [270, 3, 5])
  , ("入力の上限：" ++ show maxLen ++ " 記号をこえる式、使えない記号はまちがいとして説明"
    , isLeft (toSyms (replicate (maxLen + 1) '0')) && isLeft (toSyms "0 = q") && isLeft (toSyms "")) ]
  where
    nR = 300
    seqs = withSeed 10 (mapM (const (do j <- rndInt 1 14; mapM (const pickSym) [1 .. j])) [1 .. nR])
    pickSym = (\s -> maybe 0 (+ 1) (elemIndex s symbols)) <$> rndPick symbols
    encs = [ (cs, encodeSyms cs) | cs <- seqs ]
    rt = length [ () | (cs, e) <- encs, decodeNum (eG e) == Right cs ]
    clash = snd (foldl' (\(m, k) (cs, e) -> case M.lookup (eG e) m of
                                              Just cs2 | cs2 /= cs -> (M.insert (eG e) cs m, k + 1)
                                              _ -> (M.insert (eG e) cs m, k)) (M.empty, 0 :: Int) encs)
    props = length [ () | (cs, e) <- encs, let g = eG e
                        , arithFirstIsForall g == (take 1 cs == [10]) && arithLength g == length cs && arithHasNot g == (8 `elem` cs) ]
    isLeft = either (const True) (const False)

-- ---- このスケッチの道具
joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : r) -> x ++ concatMap (sep ++) r

-- 幅 w に入るように単語の区切りで折り返す（文字の幅はだいたい 0.62 × 大きさ）
wrapW :: Float -> Float -> String -> [String]
wrapW w size s = go (words s) ""
  where
    maxC = max 10 (floor (w / (size * 0.62))) :: Int
    go [] cur = [cur | not (null cur)]
    go (x : r) cur
      | null cur = go r x
      | length cur + 1 + length x <= maxC = go r (cur ++ " " ++ x)
      | otherwise = cur : go (x : r) ""

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
