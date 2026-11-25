-- ============================================
-- 69-11 対角線論法と不完全性 ─ 「表に全部並べた」と言われたら、対角線を反転した行を作る。その行は表のどの行ともちがう
-- (1) カントール：0 と 1 の無限の列は数えきれない　(2) 停止問題：止まるかを判定するプログラムは作れない
-- (3) ゲーデル：「この文は証明できない」という文が作れる（10 のゲーデル数で、式が自分の番号について話せる）
-- 操作：1 2 3 図を選ぶ / スペース 対角線を 1 マスずつ / ↑↓ 表の大きさ / R 新しい表
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.&.), (.|.), shiftL)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

size0 :: Int
size0 = 10                                    -- ★ 表の大きさ（4〜16）

stepFrames :: Int
stepFrames = 30                               -- ★ 対角線を 1 マス進める間のフレーム数

data World = World
  { wPanel  :: !Int                           -- 0 カントール / 1 停止問題 / 2 ゲーデル
  , wSize   :: !Int
  , wTable  :: [[Int]]                        -- 0 と 1 の表
  , wSweep  :: !Int                           -- 対角線をどこまで見せたか（−1 = 全部）
  , wSweepT :: !Int
  , wSeed   :: !Int                           -- 次の表の乱数の種
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-11 Diagonal and Incompleteness" (newTable world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (newTable world0) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (advance w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World 0 size0 [] (-1) 0 1 []

makeTable :: Int -> R [[Int]]
makeTable n = mapM (const (mapM (const (fmap (\u -> if u < 0.5 then 1 else 0) rnd)) [1 .. n])) [1 .. n]

-- 新しい表（種を 1 つずつ変える）
newTable :: World -> World
newTable w = w { wTable = withSeed (wSeed w) (makeTable (wSize w)), wSeed = wSeed w + 1, wSweep = -1 }

-- ★ 対角線の反転：k 番目の行の k 番目を反対にした列 → k 番目の行とは k 番目でちがう
diagonalFlip :: [[Int]] -> [Int]
diagonalFlip t = [ 1 - at' r k 0 | (k, r) <- zip [0 ..] t ]

firstDiff :: [Int] -> [Int] -> Int
firstDiff a b = headOr (-1) [ i | (i, x, y) <- zip3 [0 ..] a b, x /= y ]

-- 3 つの図：題名、行の名前、列の名前、マスの文字、新しい行の名前、説明
data Panel = Panel { pTitle :: String, pRow :: Int -> String, pCol :: Int -> String, pCell :: Int -> String, pNew :: String, pNote :: [String] }

panels :: [Panel]
panels =
  [ Panel "(1) Cantor's diagonal argument" (\i -> "f(" ++ show i ++ ")") show show "d"
      [ "Suppose all infinite 0/1 sequences are listed as f(0), f(1), f(2), ..."
      , "Flip the whole diagonal (digit k of f(k)) to get d: d differs from f(k) at digit k."
      , "So d is nowhere in the table -> no list can hold them all (there are more reals than naturals)." ]
  , Panel "(2) The halting problem (Turing 1936)" (\i -> 'P' : show i) (\j -> 'P' : show j) (\v -> if v == 1 then "H" else "oo") "D"
      [ "Suppose a decider H answers 'does program P halt on input Q?' (the cells: H = halts, oo = runs forever)."
      , "Build D(x): loop forever if H says x(x) halts, halt if H says it does not. D's row is the flipped diagonal."
      , "D must be some row too, yet D(D) is a contradiction -> H cannot be built (busy beavers: series 66 / 11)." ]
  , Panel "(3) Godel's incompleteness theorem (1931)" (\i -> "phi" ++ show i ++ "(x)") show (\v -> if v == 1 then "|-" else ".") "psi"
      [ "Rows = formulas phi0, phi1, ... with one variable x (Godel-number order); cell = 'is phi_k(n) provable (|-)?'"
      , "psi(x) = 'phi_x(x) is not provable' is an arithmetic formula via Godel numbers (10). Say psi = phi_g. Then"
      , "G = psi(g) = 'phi_g(g) is not provable' = 'this sentence G is not provable'. Proving G gives a contradiction -> G is unprovable (and true)." ] ]

advance :: World -> World
advance w
  | wSweep w >= 0 && wSweep w < length (wTable w) =
      if wSweepT w + 1 > stepFrames then w { wSweepT = 0, wSweep = wSweep w + 1 } else w { wSweepT = wSweepT w + 1 }
  | otherwise = w

draw :: Env -> World -> Picture
draw env w =
  let wd = envW env
      ht = envH env
      pn = at' panels (wPanel w) (headOr emptyPanel panels)
      t = wTable w
      n = length t
      sweep = wSweep w
      d = diagonalFlip t
      x0 = 110
      y0 = 136
      cs = minimum [36, (ht - 346) / i2f (n + 2), (wd * 0.55 - x0) / i2f (max 1 n)]
      ts = max 14 (cs * 0.45)
      cellV i j = at' (at' t i []) j 0
      colLabels = [ sayM (pCol pn j) (x0 + i2f j * cs + cs / 2) (y0 - cs * 0.5) ts (col dimc) | j <- [0 .. n - 1] ]
      rows = concat
        [ sayRM (pRow pn i) (x0 - 10) (y0 + i2f i * cs + cs / 2) (min 15 ts) (col (if i == sweep then yel else dimc))
          : concat [ [ Color (col (if lit then (110, 96, 50) else panelc)) (rectTL (x0 + i2f j * cs + 1) (y0 + i2f i * cs + 1) (cs - 2) (cs - 2))
                     , sayM (pCell pn (cellV i j)) (x0 + i2f j * cs + cs / 2) (y0 + i2f i * cs + cs / 2) ts (col (if lit then yel else ink)) ]
                   | j <- [0 .. n - 1], let lit = i == j && (sweep < 0 || i <= sweep) ]
        | i <- [0 .. n - 1] ]
      -- ---- 反転した行
      yd = y0 + (i2f n + 0.6) * cs
      newRow = sayRM (pNew pn) (x0 - 10) (yd + cs / 2) ts (col pink)
        : concat [ [ Color (col bgc) (rectTL (x0 + i2f j * cs + 1) (yd + 1) (cs - 2) (cs - 2))
                   , Color (col pink) (frameTL 1.5 (x0 + i2f j * cs + 1) (yd + 1) (cs - 2) (cs - 2)) ]
                   ++ [ sayM (pCell pn (at' d j 0)) (x0 + i2f j * cs + cs / 2) (yd + cs / 2) ts (col pink) | sweep < 0 || j <= sweep ]
                 | j <- [0 .. n - 1] ]
      -- ---- それぞれの行とのちがい
      rx = wd * 0.6
      diffs = say (pNew pn ++ " differs from every row of the table:") rx 136 15 (col yel)
        : [ say (if shown then pNew pn ++ " != " ++ pRow pn i ++ "  (differs at " ++ show i ++ ": " ++ pCell pn (at' d i 0) ++ " vs " ++ pCell pn (cellV i i) ++ ")" else "...")
                rx (162 + i2f i * 19) 14 (col (if shown then (if at' d i 0 /= cellV i i then grn else pink) else dimc))
          | i <- [0 .. n - 1], let shown = sweep < 0 || i <= sweep ]
      -- ---- 説明と、定理の正確な主張
      noteLines = concatMap (wrapW (wd - 60) 14) (pNote pn)
      notes = [ say ln 30 (ht - 176 + i2f k * 21) 14 (col ink) | (k, ln) <- zip [0 :: Int ..] (take 4 noteLines) ]
      thmLines = concatMap (wrapW (wd - 60) 14) theorem
      thm = [ say ln 30 (ht - 92 + i2f k * 19) 14 (col sky) | (k, ln) <- zip [0 :: Int ..] (take 4 thmLines) ]
  in view env $
       [ hudTitle ("Diagonal argument and incompleteness: " ++ pTitle pn)
           ("1 Cantor   2 halting problem   3 Godel   Space step along the diagonal   Up/Down table size " ++ show n ++ "   R new table") ]
       ++ colLabels ++ rows ++ newRow ++ diffs ++ notes ++ thm
       ++ [drawSelfTest env (wd - 180) (ht - 14) (wTest w)]
  where emptyPanel = Panel "" show show show "" []

theorem :: [String]
theorem =
  [ "First incompleteness theorem (Godel 1931, in the stronger form by Rosser 1936): a consistent theory whose axioms can be listed by a program and that can handle addition and multiplication of natural numbers has a sentence it can neither prove nor refute."
  , "(Godel's original proof used the stronger assumption of omega-consistency for the 'cannot refute' half; Rosser weakened it to plain consistency. Second theorem: such a theory cannot prove its own consistency.)" ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TChar c
      | c >= '1' && c <= '3' -> w { wPanel = fromEnum c - fromEnum '1', wSweep = -1 }
      | c == ' ' -> w { wSweep = if wSweep w < 0 || wSweep w >= length (wTable w) then 0 else wSweep w, wSweepT = 0 }
      | isKey 'r' c -> newTable w
    TUp -> newTable w { wSize = min 16 (wSize w + 1) }
    TDown -> newTable w { wSize = max 4 (wSize w - 1) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. スペースで対角線を 1 マスずつ。d は f(0) と 0 番目、f(1) と 1 番目、… でちがうことを確かめよう
-- 2. 表を 16 にしても、何回 R を押しても、d は必ず表の外。表を作る人が「d も入れたよ」と言ったら？（その新しい表でまた作る）
-- 3. 停止問題の D と、ゲーデルの psi は「対角線を反転した行」の同じ形。どこが対応しているか表にしてみよう
-- 4. 不完全性定理の 3 つの条件（十分強い・無矛盾・公理が計算で列挙できる）の 1 つでもなくすと、どうなる？
--    （例：正しい算術の文を全部公理にすると完全になるが、公理を列挙するプログラムが作れない）
-- 5. 「この文はうそ」（13 のうそつきのパラドックス）と「この文は証明できない」のちがいは？

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな表 200 個（1〜16 行）：対角線を反転した列は、k 行目と k 番目で必ずちがう", ok == 200)
  , ("要素 1〜3 個の集合 S から部分集合への関数 " ++ show count ++ " 個を全部ためす：どれも { x : x ∉ f(x) } を取りこぼす（カントールの定理）"
    , allMiss && count == 2 + 16 + 512)
  , ("くらべ：自然数の組 (a, b) は数えられる（カントールの対関数が 0〜4999 で行って戻れる）", pairOk)
  , ("くらべ：有限の 0/1 の列は数えられる（n を 2 進数にして先頭の 1 を消すと、長さ 10 までの 2047 個がちょうど 1 回ずつ）", S.size seen == 2047)
  , ("「G ⇔ G は証明できない」と「証明できるものは正しい」を両方みたす真偽は 1 通り：G は正しく、証明できない", models == [(1, 0)]) ]
  where
    tables = withSeed 77 (mapM (const (rndInt 1 16 >>= makeTable)) [1 .. 200 :: Int])
    ok = length [ () | t <- tables, let d = diagonalFlip t, and [ firstDiff d r >= 0 && at' d i 0 /= at' r i 0 | (i, r) <- zip [0 ..] t ] ]
    -- 集合 S の要素 → S の部分集合（ビットの集まり）への関数を全部
    funcs = [ (s, [ (code `div` (nSub ^ x)) `mod` nSub | x <- [0 .. s - 1] ]) | s <- [1 .. 3 :: Int], let nSub = 2 ^ s :: Int, code <- [0 .. nSub ^ s - 1] ]
    count = length funcs
    allMiss = and [ diag `notElem` f | (s, f) <- funcs
                  , let diag = foldl' (.|.) 0 [ 1 `shiftL` x | x <- [0 .. s - 1], not (testBit (at' f x 0) x) ] ]   -- 対角線の集合 { x : x ∉ f(x) }
    pair a b = ((a + b) * (a + b + 1)) `div` 2 + b
    unpair z = let wv = floor ((sqrt (8 * fromIntegral z + 1 :: Double) - 1) / 2) :: Int
                   tv = (wv * (wv + 1)) `div` 2
                   b = z - tv
               in (wv - b, b)
    pairOk = and [ pair a b == z && a >= 0 && b >= 0 | z <- [0 .. 4999 :: Int], let (a, b) = unpair z ]
    seen = S.fromList [ drop 1 (toBin n) | n <- [1 .. 2047 :: Int] ]
    toBin n = if n <= 0 then "" else toBin (n `div` 2) ++ show (n `mod` 2)
    models = [ (g, pr) | g <- [0, 1 :: Int], pr <- [0, 1 :: Int], (g == 1) == (pr == 0), pr == 0 || g == 1 ]

-- ---- このスケッチの道具
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
