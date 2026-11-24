-- ============================================
-- 66-08 プッシュダウンオートマトン ─ 有限オートマトンに「スタック（積み重ねの記憶）」を 1 本つけた機械
-- かっこ：開きかっこは積む、閉じかっこは一番上と組なら取る。最後にスタックが空なら受理。深さに上限がないのが強み。
-- 有限の機械は「深さ D まで」しか覚えられない（状態の数 = 積み方の数）。D + 1 段で必ずまちがえる。
-- 回文 w wᴿ：どこが真ん中かわからないので、毎回「ここが真ん中」と推測する枝を全部ためす（非決定性 → 枝の探索）
-- 操作：Tab かっこ ⇔ 回文 / 文字キー 入力（かっこ：( ) [ ] { }、回文：a b）/ Backspace 消す / ← → 1 文字 / スペース 自動再生 /
--       ↑↓ お手本 / D 有限の機械の深さ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

partner :: Char -> Char                       -- 閉じかっこの相手
partner c = case c of { ')' -> '('; ']' -> '['; '}' -> '{'; _ -> ' ' }

brPresets, palPresets :: [String]
brPresets = ["({[]})[]", "(([]){})", "([)]", "((((()))))", "(()", "{[()()]}{}"]
palPresets = ["abba", "abaaba", "abab", "aabbaa", "baab", "aaaa"]

-- ★ かっこの PDA（決定的）：1 文字ごとのスタックの様子（下から順の文字列）を返す
data BrRun = BrRun { bOk :: !Bool, bSnaps :: [String], bFail :: !Int }

bracketPda :: String -> BrRun
bracketPda s = go 0 "" [""] s
  where
    go _ stk snaps [] = BrRun (null stk) (reverse snaps) (-1)
    go i stk snaps (c : cs)
      | c `elem` "([{" = let st = stk ++ [c] in go (i + 1) st (st : snaps) cs          -- 開き → 積む
      | not (null stk) && last stk == partner c = let st = init stk in go (i + 1) st (st : snaps) cs   -- 閉じ ＋ 一番上が組 → 取る
      | otherwise = BrRun False (reverse (stk : snaps)) i                               -- 組にならない → 不受理

-- ★ 有限の機械：積み方を深さ D までだけ覚える DFA（状態の数 = 1 + 3 + 9 + … + 3^D）。D を超えたら「ごみ箱」へ
boundedDfa :: String -> Int -> Bool
boundedDfa s d = go "" s
  where
    go stk [] = null stk
    go stk (c : cs)
      | c `elem` "([{" = let st = stk ++ [c] in length st <= d && go st cs
      | not (null stk) && last stk == partner c = go (init stk) cs
      | otherwise = False

boundedStates :: Int -> Int
boundedStates d = sum [ 3 ^ k | k <- [0 .. d] ] + 1

-- ★ 回文 w wᴿ の PDA（非決定的）：枝 = 「真ん中は k 文字目のあと」の推測。全部の枝を同時に動かす
data Branch = Branch { brK :: !Int, brSnaps :: [String], brDead :: !Int, brOk :: !Bool }

palindromePda :: String -> ([Branch], Bool)
palindromePda s = let bs = map branch [0 .. length s] in (bs, any brOk bs)
  where
    branch k = go 0 "" [] s
      where
        go _ stk snaps [] = Branch k (reverse snaps) (-1) (null stk)
        go i stk snaps (c : cs)
          | i < k = let st = stk ++ [c] in go (i + 1) st (st : snaps) cs
          | not (null stk) && last stk == c = let st = init stk in go (i + 1) st (st : snaps) cs
          | otherwise = Branch k (reverse snaps) i False

data World = World
  { wPal   :: !Bool                           -- 回文モードか
  , wWord  :: String
  , wPre   :: !Int
  , wT     :: !Int
  , wPlay  :: !Bool
  , wPlayT :: !Int
  , wDepth :: !Int                            -- 有限の機械の深さ D
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-08 Pushdown automaton" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World False (at' brPresets 0 "") 0 0 True 0 3 []

rerun :: World -> World
rerun w = w { wT = 0, wPlay = True, wPlayT = 0 }

-- 再生：30 フレームごとに 1 文字
step :: World -> World
step w0 =
  let w = if wPlay w0 then w0 { wPlayT = wPlayT w0 + 1 } else w0
      w1 = if wPlay w && wPlayT w `mod` 30 == 0 then (if wT w < length (wWord w) then w { wT = wT w + 1 } else w { wPlay = False }) else w
  in w1 { wT = clampI (wT w1) 0 (length (wWord w1)) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle (if wPal w then "Pushdown automaton: palindromes w w^R, nondeterministically" else "Pushdown automaton: matching brackets")
             ("Tab brackets <-> palindromes   " ++ (if wPal w then "a b" else "( ) [ ] { }") ++ " type   Backspace   Left/Right 1 char   Space play   Up/Down example"
              ++ (if wPal w then "" else "   D depth of the finite machine (" ++ show (wDepth w) ++ ")"))
  , Pictures [ Pictures [ Color (col (if i < wT w then (40, 70, 55) else panel)) (rectTL (30 + i2f i * 40) 96 36 40)
                        , Color (col (if i == wT w - 1 then amber else wire)) (frameTL (if i == wT w - 1 then 3 else 1) (30 + i2f i * 40) 96 36 40)
                        , sayM [c] (48 + i2f i * 40) 116 22 (col ink) ]
             | (i, c) <- zip [0 :: Int ..] (wWord w) ]
  , if wPal w then drawPalindrome env w else drawBrackets env w
  , drawSelfTest env (wTest w) ]

-- 下から積む
drawStack :: String -> Float -> Float -> Float -> RGB -> String -> Picture
drawStack stk x yb w c lab = Pictures $
  [ sayC lab (x + w / 2) (yb + 22) 14 (col dimc)
  , Color (col wire) (Pictures [lineW 2 (x - 4, yb) (x + w + 4, yb), lineW 2 (x - 4, yb) (x - 4, yb - 300), lineW 2 (x + w + 4, yb) (x + w + 4, yb - 300)]) ]
  ++ [ Pictures [ Color (colA (if i == n - 1 then 255 else 150) c) (rectTL x (yb - i2f (i + 1) * h) w (h - 3))
                , sayM [ch] (x + w / 2) (yb - (i2f i + 0.5) * h - 1) (max 14 (min 20 (h * 0.7))) (col (10, 20, 15)) ]
     | (i, ch) <- zip [0 ..] stk ]
  where n = length stk
        h = min 34 (290 / i2f (max 1 n))

drawBrackets :: Env -> World -> Picture
drawBrackets env w = Pictures $
  [ drawStack stk 60 (envH env - 120) 70 amber ("stack (height " ++ show (length stk) ++ ")")
  , say "Rules (one state; the only memory is the stack)" x 180 16 (col ink)
  , Pictures [ say ("- " ++ r) x (210 + i2f i * 26) 15 (col dimc)
             | (i, r) <- zip [0 :: Int ..] [ "read ( [ { -> push it", "read ) ] } and the top is its partner -> pop"
                                         , "anything else (empty, wrong partner) -> reject now", "end of input with an empty stack -> accept" ] ]
  , if wT w >= length wd || failed
      then say (if bOk tr then "Accept (the stack is empty)"
                else if failed then "Reject: char " ++ show (bFail tr + 1) ++ " '" ++ [at' wd (bFail tr) ' '] ++ "' has no partner"
                else "Reject: " ++ show (length stk) ++ " left on the stack") x 340 22 (col (if bOk tr then lime else rose))
      else Blank
  -- 深さのグラフ
  , say "Depth (stack height) over time" gx gy 15 (col ink)
  , Color (col amber) (polyline 2.5 [ (gx + i2f i / i2f (max 1 (length wd)) * gw, gy + 20 + gh - i2f (length (at' (bSnaps tr) i "")) / i2f maxD * gh) | i <- [0 .. k] ])
  , if wDepth w <= maxD then let yD = gy + 20 + gh - i2f (wDepth w) / i2f maxD * gh in Color (col rose) (dashLine 1 5 5 (gx, yD) (gx + gw, yD)) else Blank
  -- 有限の機械とくらべる
  , say "Compare with a finite automaton" px 180 16 (col ink)
  , say ("DFA = every stacking up to depth " ++ show (wDepth w)) px 210 14 (col dimc)
  , say ("states = 1 + 3 + ... + 3^" ++ show (wDepth w) ++ " + trash = " ++ show (boundedStates (wDepth w))) px 234 14 (col dimc)
  , say ("DFA: " ++ yn fin ++ "  PDA: " ++ yn (bOk tr) ++ (if fin == bOk tr then "  same" else "  DIFFERENT!")) px 272 18 (col (if fin == bOk tr then lime else rose))
  , say ("Depth " ++ show (wDepth w + 1) ++ " or more fools the DFA.") px 304 14 (col dimc)
  , say "More states never help: deeper brackets exist" px 326 14 (col dimc)
  , say "(same reason as (^n )^n in the pumping game 06)" px 348 14 (col dimc) ]
  where
    wd = wWord w
    tr = bracketPda wd
    k = min (wT w) (length (bSnaps tr) - 1)
    stk = at' (bSnaps tr) k ""
    failed = bFail tr >= 0 && wT w > bFail tr
    x = 230
    gx = x
    gy = 380
    gw = envW env * 0.5
    gh = min 220 (envH env - 520)
    maxD = max 1 (maxOr 0 (map length (bSnaps tr)))
    px = envW env * 0.68
    fin = boundedDfa wd (wDepth w)
    yn b = if b then "accept" else "reject"

drawPalindrome :: Env -> World -> Picture
drawPalindrome env w = Pictures $
  [ say "Branch = guess \"the middle is after k chars\": push the first half, pop matching chars after" 30 (y0 - 12) 16 (col ink) ]
  ++ [ Pictures $
         [ say ("k = " ++ show (brK b) ++ (if done then (if brOk b then "  accept!" else if brDead b >= 0 then "  stuck at char " ++ show (brDead b + 1) else "  stack left over")
                                            else if alive then "" else "  stuck"))
               30 (y + rh * 0.6) 14 (col (if done then (if brOk b then lime else rose) else if alive then ink else (90, 100, 95)))
         , Color (col wire) (lineW 1 (290, y + rh) (290 + 24 * 12, y + rh)) ]
         ++ [ Pictures [ Color (col (if alive then (if i < brK b then amber else cyan) else (70, 80, 75))) (rectTL (290 + i2f i * 24) (y + 4) 21 (rh - 8))
                       , sayM [c] (300 + i2f i * 24) (y + rh / 2) 14 (col (10, 20, 15)) ]
            | (i, c) <- zip [0 :: Int ..] stk ]
     | (r, b) <- zip [0 :: Int ..] bs
     , let y = y0 + i2f r * rh
     , let alive = brDead b < 0 || wT w <= brDead b
     , let stk = if wT w == 0 then "" else at' (brSnaps b) (min (wT w) (length (brSnaps b)) - 1) "" ]
  ++ [ say "Nondeterminism = try every branch" px (y0 + 20) 16 (col ink) ]
  ++ [ say t px (y0 + 52 + i2f i * 24) 14 (col dimc)
     | (i, t) <- zip [0 :: Int ..] [ "- branches = length + 1 = " ++ show (length bs) ++ " (where to turn)", "- accept if any branch accepts", "  (no need to know which guess is right)"
                                   , "- odd palindromes (aba) are not w w^R,", "  so they are not accepted"
                                   , "- a deterministic PDA cannot do this", "  (it never knows where the middle is)" ] ]
  ++ [ say (if ok then "Accept: of the form w w^R" else "Reject: no branch accepts") px (y0 + 240) 22 (col (if ok then lime else rose)) | wT w >= length wd ]
  where
    wd = wWord w
    (bs, ok) = palindromePda wd
    y0 = 170
    rh = min 40 ((envH env - 280) / i2f (max 1 (length bs)))
    done = wT w >= length wd
    px = envW env * 0.62

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down _ _ -> case typedKey k of
    TTab -> rerun w { wPal = not (wPal w), wPre = 0, wWord = at' (if wPal w then brPresets else palPresets) 0 "" }
    TChar c | c `elem` allowed -> rerun w { wWord = typeInto (wWord w) c allowed (if wPal w then 12 else 24) }
    TBack -> rerun w { wWord = dropLast (wWord w) }
    _ -> case k of
      SpecialKey KeyRight -> w { wPlay = False, wT = min (length (wWord w)) (wT w + 1) }
      SpecialKey KeyLeft -> w { wPlay = False, wT = max 0 (wT w - 1) }
      SpecialKey KeySpace -> w { wT = if wT w >= length (wWord w) then 0 else wT w, wPlay = not (wPlay w), wPlayT = 0 }
      SpecialKey KeyUp -> preset (length ps - 1)
      SpecialKey KeyDown -> preset 1
      Char c | isKey 'd' c -> w { wDepth = wDepth w `mod` 6 + 1 }
      _ -> w
  _ -> w
  where
    allowed = if wPal w then "ab" else "()[]{}"
    ps = if wPal w then palPresets else brPresets
    preset d = let p = (wPre w + d) `mod` length ps in rerun w { wPre = p, wWord = at' ps p "" }

-- ---------- やってみよう ----------
-- 1. ((((((((((  ))))))))))  のように深くすると、D の DFA はどこでまちがえる？ D を増やすと状態は 3 倍ずつ増える
-- 2. 「([)]」はなぜ不受理？ スタックの一番上を見よう。数えるだけ（カウンタ）では見分けられない
-- 3. 回文で、長さ 12 の文字列だと枝は 13 本。受理する枝はいくつある？（aaaa はいくつ？）
-- 4. 奇数の長さの回文（aba）も受理するには、「真ん中の 1 文字を読みとばす」枝を足せばよい。palindromePda を変えてみよう
-- 5. スタックを 2 本にすると？ → チューリング機械と同じ強さになる（10）

-- ---- 自己テスト
-- くらべる相手：組 () [] {} を消し続けて、空になるか（スタックを使わない素朴な方法）
removePairs :: String -> String
removePairs s = let r = go s in if r == s then s else removePairs r
  where go t = case t of
          (a : b : rest) | [a, b] `elem` ["()", "[]", "{}"] -> go rest
          (a : rest) -> a : go rest
          [] -> []

selfTest :: [(String, Bool)]
selfTest =
  [ ("( ) だけの 600 個：PDA = カウンタの判定（途中で負にならず最後が 0）", all (\(s, _) -> bOk (bracketPda s) == counter s) pairsR)
  , ("3 種類のかっこ 600 個：PDA = 「() [] {} を消し続けて空になる」", all (\(_, m) -> bOk (bracketPda m) == null (removePairs m)) pairsR)
  , ("深さ 3 までの DFA（" ++ show (boundedStates 3) ++ " 状態）は深さ 3 以内なら PDA と同じ、深さ 4 の (((()))) をまちがえる"
    , okB && bOk (bracketPda "(((())))") && not (boundedDfa "(((())))" 3))
  , ("回文 PDA（枝の探索）= 「長さが偶数で逆から読んでも同じ」、長さ 10 までの全部 2047 個", all (\s -> snd (palindromePda s) == (even (length s) && s == reverse s)) (allStrings "ab" 10))
  , ("abba は k = 2 の枝だけが受理、abab はどの枝も受理しない", map brK (filter brOk (fst (palindromePda "abba"))) == [2] && not (snd (palindromePda "abab"))) ]
  where
    pairsR = withSeed 8 (mapM (\_ -> do
               n1 <- rndInt 0 14
               s <- rndString "()" n1
               n2 <- rndInt 0 12
               m <- rndString "()[]{}" n2
               pure (s, m)) [1 .. 600 :: Int])
    -- 数えるだけの判定（1 種類なら十分）
    counter s = let cs = scanl (\c ch -> c + (if ch == '(' then 1 else -1)) (0 :: Int) s in all (>= 0) cs && lastOr 0 cs == 0
    okB = and [ boundedDfa s 3 == bOk p | s <- allStrings "()[" 8, let p = bracketPda s, maxOr 0 (map length (bSnaps p)) <= 3 ]

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
