-- ============================================
-- 66-06 ポンピング補題ゲーム ─ 「この言語は正規言語（DFA で判定できる）ではない」をゲームで確かめる
-- p 状態の DFA が長さ p 以上の文字列を読むと、どこかで同じ状態を 2 回通る → その間の部分 y は何回くり返しても（0 回でも）受理のまま。
-- ゲーム：① 相手（コンピュータ）が p を決める ② あなたが言語の中の長さ p 以上の文字列を選ぶ ③ 相手が x y z に分ける（|xy| ≤ p, |y| ≥ 1）
-- ④ あなたが i を選び、x y^i z が言語の外に出たらあなたの勝ち。正規言語なら相手はいつでも勝てる分け方を見つける
-- 操作：1〜5 言語を選ぶ / ↑↓ 文字列の長さ（②）・i（④）を選ぶ / スペース 決定 / N 新しいゲーム
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl', isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

iMax :: Int
iMax = 5                                      -- ★ i は 0〜iMax から選ぶ（相手はこの範囲で全部受理になる分け方をさがす）

-- ★ 言語：名前・言語に入るか・あなたが選べる文字列 (k) → …・正規言語か
data Lang = Lang { lName :: String, lIn :: String -> Bool, lFam :: Int -> String, lRegular :: Bool }

langs :: [Lang]
langs =
  [ Lang "a^n b^n (same number of a and b, a first)" (\s -> isAsBs s && count 'a' s == count 'b' s) (\k -> rep k "a" ++ rep k "b") False
  , Lang "palindromes (same read backwards)" (\s -> s == reverse s) (\k -> rep k "a" ++ "b" ++ rep k "a") False
  , Lang "number of a's is a square (a, aaaa, a^9 ...)" (\s -> all (== 'a') s && isSquare (length s)) (\k -> rep (k * k) "a") False
  , Lang "a*b* (a's, then b's)" isAsBs (\k -> rep k "a" ++ rep k "b") True
  , Lang "(ab)* (ab repeated)" isAbStar (\k -> rep k "ab") True ]

-- 言語の判定（正規表現を使わずに手で書く）
isAsBs :: String -> Bool                      -- /^a*b*$/
isAsBs s = all (== 'b') (dropWhile (== 'a') s)

isAbStar :: String -> Bool                    -- /^(ab)*$/
isAbStar s = case s of
  [] -> True
  ('a' : 'b' : rest) -> isAbStar rest
  _ -> False

isSquare :: Int -> Bool
isSquare n = let r = floor (sqrt (fromIntegral n :: Double)) :: Int in any (\k -> k * k == n) [r - 1, r, r + 1]

count :: Char -> String -> Int
count c = length . filter (== c)

rep :: Int -> String -> String
rep k s = concat (replicate k s)

lang :: Int -> Lang
lang k = at' langs k (headOr (Lang "" (const False) (const "") False) langs)

-- x y^i z（sp = (|x|, |xy|)）
pump :: String -> (Int, Int) -> Int -> String
pump s (a, b) i = take a s ++ rep i (take (b - a) (drop a s)) ++ drop b s

-- ★ 相手の作戦：全部の分け方 (x, y) を調べ、i = 0〜iMax の全部で言語に入るものがあればそれを選ぶ
allSplits :: String -> Int -> [(Int, Int)]
allSplits s p = [ (a, b) | let m = min p (length s), a <- [0 .. m - 1], b <- [a + 1 .. m] ]

pumpable :: (String -> Bool) -> String -> (Int, Int) -> Bool
pumpable l s sp = all (\i -> l (pump s sp i)) [0 .. iMax]

cpuSplit :: (String -> Bool) -> String -> Int -> ((Int, Int), Bool)
cpuSplit l s p = let al = allSplits s p in case find (pumpable l s) al of
  Just g -> (g, True)
  Nothing -> (headOr (0, 1) al, False)

data World = World
  { wLang   :: !Int
  , wP      :: !Int                           -- 相手の p
  , wK      :: !Int                           -- あなたが選ぶ文字列の番号 k
  , wPhase  :: !Int                           -- 0〜3（② → ③ → ④ → 結果）
  , wXyz    :: Maybe ((Int, Int), Bool)       -- 相手の分け方と「安全」か
  , wI      :: !Int
  , wYou    :: !Int
  , wCpu    :: !Int
  , wSeed   :: !Int                           -- 次のゲームの種
  , wResult :: String
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-06 Pumping lemma game" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = newGame (World 0 3 3 0 Nothing 2 0 0 1 "" [])

-- 新しいゲーム：種 gameSeed で相手が p = 2〜5 を決める
newGame :: World -> World
newGame w = w { wP = withSeed (wSeed w) (rndInt 2 5), wSeed = wSeed w + 1, wK = 1, wPhase = 0, wXyz = Nothing, wI = 2, wResult = "" }

wordOf :: World -> String
wordOf w = lFam (lang (wLang w)) (wK w)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Pumping lemma game: " ++ lName lg ++ (if lRegular lg then "   <- regular" else ""))
             ("1-5 language   Up/Down choose   Space confirm   N new game      you " ++ show (wYou w) ++ " wins / computer " ++ show (wCpu w) ++ " wins")
  , say ("(1) Computer: p = " ++ show p ++ "   (\"my DFA has " ++ show p ++ " states\")") 30 y0 20 (col cyan)
  , say ("(2) You: a word in L with length >= " ++ show p ++ " (Up/Down)") 30 (y0 + 36) 18 (col (if wPhase w == 0 then amber else ink))
  , say ("length " ++ show (length wd) ++ (if length wd < p then "   x too short" else if lIn lg wd then "   ok" else "   x not in the language")) 30 (y0 + 114) 15 (col (if wPhase w == 0 then amber else dimc))
  , drawWord env wd (if wPhase w >= 1 then fmap fst (wXyz w) else Nothing) 30 (y0 + 56) Nothing ]
  ++ phase1 ++ phase2 ++ phase3 ++ right ++
  [ say "To show a language is not regular: for every p you can pick a word so that every split loses for some i" 30 (envH env - 60) 14 (col dimc)
  , say "a^n b^n tip: pick a^p b^p. Since |xy| <= p, y is only a's -> i = 0 removes a's and the counts differ" 30 (envH env - 40) 14 (col dimc)
  , drawSelfTest env (wTest w) ]
  where
    lg = lang (wLang w)
    p = wP w
    wd = wordOf w
    y0 = 110
    cut t = if length t > 16 then take 14 t ++ "..." else t
    phase1 = case wXyz w of
      Just ((a, b), safe) | wPhase w >= 1 ->
        [ say ("(3) Computer: x = \"" ++ cut (take a wd) ++ "\"  y = \"" ++ cut (take (b - a) (drop a wd)) ++ "\"  z = \"" ++ cut (drop b wd) ++ "\"")
              30 (y0 + 140) 18 (col (if wPhase w == 1 then amber else ink))
        , say ("|xy| = " ++ show b ++ " <= " ++ show p ++ ", |y| = " ++ show (b - a)) 30 (y0 + 162) 15 (col (if wPhase w == 1 then amber else ink))
        , say (if safe then "(the computer found a split that stays in the language for every i = 0.." ++ show iMax ++ ")"
                       else "(the computer cannot escape whatever it does... it just takes the first split)") 30 (y0 + 182) 14 (col dimc) ]
      _ -> []
    phase2 = case wXyz w of
      Just (sp, _) | wPhase w >= 2 ->
        let pw = pump wd sp (wI w)
        in [ say ("(4) You: i = " ++ show (wI w) ++ " (Up/Down) -> x y^" ++ show (wI w) ++ " z has length " ++ show (length pw) ++ (if lIn lg pw then "   in the language" else "   OUT of the language!"))
                 30 (y0 + 200) 18 (col (if wPhase w == 2 then amber else ink))
           , drawWord env pw Nothing 30 (y0 + 214) (Just (sp, wI w)) ]
      _ -> []
    phase3 = [ say (if wResult w == "you" then "You win! Repeating y left the language" else "The computer wins. Every repetition stays in the language")
                   30 (y0 + 300) 26 (col (if wResult w == "you" then lime else rose)) | wPhase w == 3 ]
    rx = envW env * 0.62
    right = if wPhase w >= 1 then [drawSplitTable (lIn lg) wd p (wXyz w) rx 110] else
      [ say "Words in this language (shortest first)" rx 110 15 (col ink)
      , Pictures [ say (if s == "" then "e (empty)" else s) (rx + i2f (i `mod` 2) * 170) (140 + i2f (i `div` 2) * 24) 14 (col dimc)
                 | (i, s) <- zip [0 :: Int ..] (take 16 (filter (lIn lg) (allStrings "ab" 9))) ]
      , say ("Up/Down to a word of length >= " ++ show p ++ ", then Space") 30 240 14 (col amber) ]

-- 文字を箱で。x = 灰、y = 金、z = 水色
drawWord :: Env -> String -> Maybe (Int, Int) -> Float -> Float -> Maybe ((Int, Int), Int) -> Picture
drawWord env s sp x y pumped = Pictures
  [ Pictures [ Color (col c) (rectTL (x + i2f k * cw) y (cw - 2) 36)
             , sayM [ch] (x + i2f k * cw + cw / 2 - 1) (y + 18) (max 14 (min 18 (cw * 0.7))) (col (if isPanel then ink else (10, 20, 15))) ]
  | (k, ch) <- zip [0 :: Int ..] s
  , let (c, isPanel) = colorOf k ]
  where
    cw = min 28 (envW env * 0.58 / i2f (max 1 (length s)))
    colorOf k = case pumped of
      Just ((a, b), i) -> let ylen = (b - a) * i in (if k < a then (70, 90, 80) else if k < a + ylen then amber else cyan, False)
      Nothing -> case sp of
        Just (a, b) -> (if k < a then (70, 90, 80) else if k < b then amber else cyan, False)
        Nothing -> (panel, True)

-- 相手の頭の中：全部の分け方 × i の表
drawSplitTable :: (String -> Bool) -> String -> Int -> Maybe ((Int, Int), Bool) -> Float -> Float -> Picture
drawSplitTable l s p xyz x y = Pictures $
  [ say "Computer's view: split x i (o = in the language)" x y 15 (col ink) ]
  ++ [ sayC ("i=" ++ show i) (x + 150 + i2f i * 36) (y + 24) 14 (col dimc) | i <- [0 .. iMax] ]
  ++ concat [ say ("|x|=" ++ show a ++ " y=" ++ take (b - a) (drop a s) ++ (if ok then " safe" else "")) x yy 14 (col (if chosen then amber else if ok then lime else dimc))
              : [ let v = l (pump s sp i) in sayC (if v then "o" else "x") (x + 150 + i2f i * 36) yy 14 (col (if v then lime else rose)) | i <- [0 .. iMax] ]
            | (r, sp@(a, b)) <- zip [0 :: Int ..] (take 14 al)
            , let yy = y + 48 + i2f r * 24
            , let ok = pumpable l s sp
            , let chosen = fmap fst xyz == Just sp ]
  ++ [ say ("... and " ++ show (length al - 14) ++ " more") x (y + 48 + 14 * 24) 14 (col dimc) | length al > 14 ]
  where al = allSplits s p

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> newGame w { wLang = fromEnum c - fromEnum '1' }
    | isKey 'n' c -> newGame w
  EventKey (SpecialKey KeyUp) Down _ _ -> updown 1
  EventKey (SpecialKey KeyDown) Down _ _ -> updown (-1)
  EventKey (SpecialKey KeySpace) Down _ _
    | wPhase w == 0 && length wd >= wP w && l wd -> w { wXyz = Just (cpuSplit l wd (wP w)), wPhase = 1 }
    | wPhase w == 1 -> w { wPhase = 2 }
    | wPhase w == 2, Just (sp, _) <- wXyz w ->
        if l (pump wd sp (wI w)) then w { wResult = "cpu", wCpu = wCpu w + 1, wPhase = 3 } else w { wResult = "you", wYou = wYou w + 1, wPhase = 3 }
    | wPhase w == 3 -> newGame w
  _ -> w
  where
    l = lIn (lang (wLang w))
    wd = wordOf w
    updown d
      | wPhase w == 0 = w { wK = clampI (wK w + d) 0 12 }
      | wPhase w == 2 = w { wI = clampI (wI w + d) 0 iMax }
      | otherwise = w

-- ---------- やってみよう ----------
-- 1. aⁿbⁿ で、わざと短い文字列（k を小さく）や a^1 b^(p) のような選び方をすると、相手が逃げられることがある。どの選び方が強い？
-- 2. 回文で a^p b a^p の代わりに (ab)^k (ba)^k を選ぶと？ 相手の表で「safe」が出るか見よう
-- 3. a*b* はどう選んでも勝てない。表の「safe」な分け方が必ずあるのはなぜ？（y = 最初の 1 文字）
-- 4. langs に「a の数 > b の数」や「a の数が 2 のべき」を足して、勝てる作戦を考えよう
-- 5. 08 のプッシュダウンオートマトン（スタックつき）なら aⁿbⁿ も回文も判定できる。機械を強くすると何が変わる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("a*b*（正規）：p = 2〜6、長さ p〜p+3 の全部の文字列で、相手は i = 0〜5 の全部で言語に入る分け方を見つける", cpuAlways (lIn (lang 3)))
  , ("(ab)*（正規）：同じく相手はいつも勝てる", cpuAlways (lIn (lang 4)))
  , ("aⁿbⁿ：a^p b^p を選べば、p = 1〜8 のどの分け方にも i = 0 か 2 で勝てる", youWin (lang 0) id [0, 2])
  , ("回文：a^p b a^p を選べば、どの分け方にも i = 0 で勝てる", youWin (lang 1) id [0])
  , ("a の数が平方数：a^(p²) を選べば i = 2 で長さが p² と (p+1)² の間 → 勝てる", youWin (lang 2) id [2])
  , ("相手の分け方はいつもルールどおり（|xy| ≤ p、|y| ≥ 1）", okSplit) ]
  where
    -- 正規言語：長さ p〜p+3 の言語の中の文字列なら全部、相手は安全な分け方を見つける
    cpuAlways l = and [ snd (cpuSplit l s p) | p <- [2 .. 6], s <- allStrings "ab" (p + 3), length s >= p, l s ]
    -- 正規でない言語：あなたの作戦（よい文字列 + よい i）でどの分け方にも勝てる
    youWin lg kOf is = and [ lIn lg s && length s >= p && all (\sp -> any (\i -> not (lIn lg (pump s sp i))) is) (allSplits s p)
                           | p <- [1 .. 8], let s = lFam lg (kOf p) ]
    okSplit = and [ b <= p && b - a >= 1 | p <- [1 .. 6], s <- ["ab", "aabb", "abab", "aaaa"], let ((a, b), _) = cpuSplit (const True) s p ]

-- ==================================================
-- ここから下は「オートマトンの道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type RGB = (Float, Float, Float)

bgCol, ink, amber, cyan, lime, rose, violet, dimc, panel, wire :: RGB
bgCol = (9, 20, 15)                           -- 背景（回路基板の黒緑）
ink = (226, 240, 228)                         -- 文字（蛍光の白）
amber = (255, 170, 50)
cyan = (70, 215, 230)
lime = (140, 230, 120)
rose = (255, 100, 120)
violet = (185, 150, 255)
dimc = (125, 155, 138)
panel = (18, 36, 28)
wire = (44, 74, 58)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

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

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab が SpecialKey で来る）
data TypedKey = TChar Char | TBack | TEnter | TTab | TOther

typedKey :: Key -> TypedKey
typedKey k = case k of
  Char '\b' -> TBack
  Char '\DEL' -> TBack
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> TBack
  SpecialKey KeyDelete -> TBack               -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  _ -> TOther

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）
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

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.12、2）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- n を 2 進数の文字列に（n.toString(2)）
toBin :: Int -> String
toBin n = if n <= 0 then "0" else reverse (go n)
  where go 0 = ""
        go k = (if odd k then '1' else '0') : go (k `div` 2)

-- 2 進数の文字列を数に（parseInt(s, 2)。0 と 1 でない文字で止まる）
fromBin :: String -> Int
fromBin = foldl' (\a c -> 2 * a + (if c == '1' then 1 else 0)) 0 . takeWhile (`elem` "01")

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

rndPick :: String -> R Char                   -- 文字の中から 1 つ
rndPick cs = do u <- rnd; pure (at' cs (floor (u * fromIntegral (length cs))) ' ')

rndString :: String -> Int -> R String        -- alpha の文字で長さ len の文字列
rndString alpha len = mapM (const (rndPick alpha)) [1 .. len]

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 有限オートマトンの道具
-- DFA = 状態の数・記号・遷移（dTr !! q で、記号 → 次の状態）・開始・受理
--   遷移がない記号 = 「行き止まり」（そこで不受理が決まる）
-- NFA = 遷移が「次の状態のリスト」。記号 epsSym が ε 遷移
type Sym = Char

data DFA = DFA { dN :: !Int, dAlpha :: [Sym], dTr :: [M.Map Sym Int], dStart :: !Int, dAcc :: [Bool] }
data NFA = NFA { nN :: !Int, nAlpha :: [Sym], nTr :: [M.Map Sym [Int]], nStart :: !Int, nAcc :: [Bool] }

epsSym :: Sym                                 -- ε（文字を読まずに進む）
epsSym = '\0'

mkDfa :: [Sym] -> [[(Sym, Int)]] -> Int -> [Bool] -> DFA
mkDfa alpha rows st acc = DFA (length rows) alpha (map M.fromList rows) st acc

mkNfa :: [Sym] -> [[(Sym, [Int])]] -> Int -> [Bool] -> NFA
mkNfa alpha rows st acc = NFA (length rows) alpha (map M.fromList rows) st acc

dfaNext :: DFA -> Int -> Sym -> Int
dfaNext d q c
  | q < 0 = -1
  | otherwise = case M.lookup c (at' (dTr d) q M.empty) of
      Just t | t >= 0 && t < dN d -> t
      _ -> -1

-- たどった状態の列（-1 = 行き止まり）
dfaTrace :: DFA -> String -> [Int]
dfaTrace d = scanl (dfaNext d) (dStart d)

dfaAccepts :: DFA -> String -> Bool
dfaAccepts d s = go (dStart d) s
  where go q [] = q >= 0 && at' (dAcc d) q False
        go q (c : cs) = let t = dfaNext d q c in t >= 0 && go t cs

-- 行き止まりを「ごみ箱」状態にまとめて、全部の遷移をそろえる（返り値の 2 つ目 = ごみ箱の番号、なければ −1）
completeDfaS :: DFA -> (DFA, Int)
completeDfaS d =
  let missing = or [ dfaNext d q c < 0 | q <- [0 .. dN d - 1], c <- dAlpha d ]
      sink = dN d
      fillRow q = foldl' (\m c -> if dfaNext d q c < 0 then M.insert c sink m else m) (at' (dTr d) q M.empty) (dAlpha d)
  in if not missing then (d, -1) else
       ( d { dN = dN d + 1
           , dTr = [ fillRow q | q <- [0 .. dN d - 1] ] ++ [M.fromList [ (c, sink) | c <- dAlpha d ]]
           , dAcc = take (dN d) (dAcc d ++ repeat False) ++ [False] }
       , sink )

completeDfa :: DFA -> DFA
completeDfa = fst . completeDfaS

complementDfa :: DFA -> DFA                   -- 受理と不受理を入れかえる
complementDfa d = let e = completeDfa d in e { dAcc = map not (dAcc e) }

data Op = OpAnd | OpOr | OpDiff | OpXor deriving (Eq, Show)

opFun :: Op -> Bool -> Bool -> Bool
opFun o x y = case o of
  OpAnd -> x && y
  OpOr -> x || y
  OpDiff -> x && not y
  OpXor -> x /= y

data Product = Product { prD :: DFA, prPairs :: [(Int, Int)], prLeft :: DFA, prRight :: DFA }

-- 積の構成：状態 = (A の状態, B の状態) の組。たどれる組だけ、見つけた順に番号をつける
productDfa :: DFA -> DFA -> Op -> Product
productDfa a0 b0 o =
  let a = completeDfa a0
      b = completeDfa b0
      alpha = filter (`elem` dAlpha b) (dAlpha a)
      start = (dStart a, dStart b)
      loop h ids prs n rows
        | h >= n = (prs, reverse rows)
        | otherwise =
            let (p, q) = IM.findWithDefault (0, 0) h prs
                addSym (ids1, prs1, n1, row) c =
                  let t = (dfaNext a p c, dfaNext b q c)
                  in case M.lookup t ids1 of
                       Just k -> (ids1, prs1, n1, M.insert c k row)
                       Nothing -> (M.insert t n1 ids1, IM.insert n1 t prs1, n1 + 1, M.insert c n1 row)
                (ids', prs', n', row') = foldl' addSym (ids, prs, n, M.empty) alpha
            in loop (h + 1) ids' prs' n' (row' : rows)
      (pairsM, trs) = loop 0 (M.singleton start 0) (IM.singleton 0 start) 1 []
      pairs = IM.elems pairsM
      accOf (p, q) = opFun o (at' (dAcc a) p False) (at' (dAcc b) q False)
  in Product (DFA (length pairs) alpha trs 0 (map accOf pairs)) pairs a b

-- 幅優先探索：受理状態までの一番短い文字列（Nothing = 空言語）
dfaShortestAccepted :: DFA -> Maybe String
dfaShortestAccepted d
  | dStart d < 0 || dStart d >= dN d = Nothing
  | otherwise = go [dStart d] [] (IM.singleton (dStart d) "")
  where
    go [] [] _ = Nothing
    go [] next seen = go (reverse next) [] seen
    go (q : qs) next seen
      | at' (dAcc d) q False = Just (reverse (IM.findWithDefault "" q seen))
      | otherwise =
          let path = IM.findWithDefault "" q seen
              (next', seen') = foldl' (\(nx, sn) c -> let t = dfaNext d q c in
                                          if t >= 0 && not (IM.member t sn) then (t : nx, IM.insert t (c : path) sn) else (nx, sn))
                                      (next, seen) (dAlpha d)
          in go qs next' seen'

-- Nothing なら同じ言語、そうでなければ反例
dfaEquivalent :: DFA -> DFA -> Maybe String
dfaEquivalent a b = dfaShortestAccepted (prD (productDfa a b OpXor))

-- スタートからたどれない状態を消す（2 つ目 = 新しい番号 → 元の番号）
reachableDfa :: DFA -> (DFA, [Int])
reachableDfa d =
  let go h order idx
        | h >= length order = (order, idx)
        | otherwise =
            let q = at' order h 0
                (order', idx') = foldl' (\(o, ix) c -> let t = dfaNext d q c in
                                            if t >= 0 && not (IM.member t ix) then (o ++ [t], IM.insert t (length o) ix) else (o, ix))
                                        (order, idx) (dAlpha d)
            in go (h + 1) order' idx'
      (ord', idxM) = go 0 [dStart d] (IM.singleton (dStart d) 0)
      row q = M.fromList [ (c, IM.findWithDefault 0 t idxM) | c <- dAlpha d, let t = dfaNext d q c, t >= 0 ]
  in (DFA (length ord') (dAlpha d) (map row ord') 0 (map (\q -> at' (dAcc d) q False) ord'), ord')

data Mini = Mini { mWork :: DFA, mOld :: [Int], mRounds :: [[Int]], mBlockOf :: [Int], mMin :: DFA }

-- 見つけた順に 0, 1, 2, … と番号をつけなおす
renum :: Ord k => [k] -> [Int]
renum keys = reverse (snd (foldl' (\(m, out) k -> case M.lookup k m of
                                     Just v -> (m, v : out)
                                     Nothing -> let v = M.size m in (M.insert k v m, v : out)) (M.empty, []) keys))

-- ムーアの分割の精密化：「区別できない状態」を同じブロックに
minimizeMoore :: DFA -> Mini
minimizeMoore d =
  let (e, old) = reachableDfa (completeDfa d)
      blk0 = renum (map (\v -> if v then 1 else 0 :: Int) (dAcc e))
      nDistinct = S.size . S.fromList
      refine blk rounds =
        let keys = [ (b, [ at' blk (dfaNext e q c) 0 | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
            nb = renum keys
        in if nDistinct nb == nDistinct blk then (blk, reverse rounds) else refine nb (nb : rounds)
      (blk, rounds) = refine blk0 [blk0]
      k = nDistinct blk
      accs = IM.fromList [ (b, at' (dAcc e) q False) | (q, b) <- zip [0 ..] blk ]
      trs = IM.fromListWith M.union [ (b, M.fromList [ (c, at' blk (dfaNext e q c) 0) | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
      mn = DFA k (dAlpha e) [ IM.findWithDefault M.empty b trs | b <- [0 .. k - 1] ] (at' blk (dStart e) 0) [ IM.findWithDefault False b accs | b <- [0 .. k - 1] ]
  in Mini e old rounds blk mn

-- 長さ 0〜maxLen の文字列を全部（短い順）
allStrings :: [Sym] -> Int -> [String]
allStrings alpha maxLen = concat (take (maxLen + 1) (iterate (\layer -> [ s ++ [c] | s <- layer, c <- alpha ]) [""]))

nfaRow :: NFA -> Int -> Sym -> [Int]
nfaRow nfa q c = M.findWithDefault [] c (at' (nTr nfa) q M.empty)

-- ε 遷移だけでたどれる状態を全部足す（小さい順）
epsClosure :: NFA -> [Int] -> [Int]
epsClosure nfa states = S.toAscList (go (S.fromList states) states)
  where go seen [] = seen
        go seen (q : todo) = let new = [ t | t <- nfaRow nfa q epsSym, not (S.member t seen) ]
                             in go (foldl' (flip S.insert) seen new) (new ++ todo)

nfaMove :: NFA -> [Int] -> Sym -> [Int]
nfaMove nfa set c = epsClosure nfa (S.toList (S.fromList (concatMap (\q -> nfaRow nfa q c) set)))

nfaTrace :: NFA -> String -> [[Int]]
nfaTrace nfa = scanl (nfaMove nfa) (epsClosure nfa [nStart nfa])

nfaAccepts :: NFA -> String -> Bool
nfaAccepts nfa s = go (epsClosure nfa [nStart nfa]) s
  where go cur [] = any (\q -> at' (nAcc nfa) q False) cur
        go cur (c : cs) = let nx = nfaMove nfa cur c in not (null nx) && go nx cs

data Subset = Subset { sSets :: [[Int]], sRows :: [(Int, M.Map Sym Int)], sDfa :: DFA }

-- 部分集合構成：DFA の 1 状態 = NFA の状態の集合（たどれる集合だけ、見つけた順に番号）
subsetConstruct :: NFA -> Int -> Subset
subsetConstruct nfa maxStates =
  let first = epsClosure nfa [nStart nfa]
      loop h sets ids n rows
        | h >= n || n > maxStates = (sets, reverse rows)
        | otherwise =
            let cur = IM.findWithDefault [] h sets
                addSym (sets1, ids1, n1, row) c =
                  let t = nfaMove nfa cur c
                  in case M.lookup t ids1 of
                       Just k -> (sets1, ids1, n1, M.insert c k row)
                       Nothing -> (IM.insert n1 t sets1, M.insert t n1 ids1, n1 + 1, M.insert c n1 row)
                (sets', ids', n', row') = foldl' addSym (sets, ids, n, M.empty) (nAlpha nfa)
            in loop (h + 1) sets' ids' n' ((h, row') : rows)
      (setsM, rows) = loop 0 (IM.singleton 0 first) (M.singleton first 0) 1 []
      sets = IM.elems setsM
      dfa = DFA (length sets) (nAlpha nfa) (map snd rows) 0 [ any (\q -> at' (nAcc nfa) q False) st | st <- sets ]
  in Subset sets rows dfa

setName :: [Int] -> String
setName st = "{" ++ joinWith "," (map show st) ++ "}"

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

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

textW :: String -> Float -> Float
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

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

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

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
