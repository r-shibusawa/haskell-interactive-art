-- ============================================
-- 69-08 ラムダ計算 ─ 「関数を作る \x.M」と「関数に渡す M N」だけの小さな言語。計算は β 簡約：(\x.M) N → M の x を N に置きかえる
-- 数（チャーチ数 2 = \f.\x.f (f x)）も真偽（TRUE = \t.\f.t）も足し算も、全部 λ 式で書ける。黄色の部分が次に簡約する所。
-- 正規順序（外側・左から）なら、止まる方法があれば必ず止まる。内側から（作用的順序）だと止まらないことがある
-- 操作：文字キー = 入力（\ が λ、. ( ) 空白、小文字 = 変数、数字 = チャーチ数、大文字 = 名前つきの式 PLUS TIMES TRUE …）
--       → 1 歩 / ← もどる / Enter 自動で最後まで / Tab 正規順序 ⇔ 作用的順序 / ↑↓ お手本 / Backspace
--       Shift + Backspace 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じる）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

presets :: [(String, String)]                 -- ★ お手本 (式, 説明)
presets =
  [ ("PLUS 2 3", "addition: 2 + 3")
  , ("TIMES 2 3", "multiplication: 2 x 3")
  , ("SUCC (SUCC 0)", "the next number, twice")
  , ("IF TRUE a b", "if true then a, else b")
  , ("AND TRUE FALSE", "true and false")
  , ("PRED 3", "the number before (Kleene's trick)")
  , ("POW 2 3", "2 to the power 3")
  , ("(\\x.x x) (\\x.x x)", "Omega: never stops")
  , ("K a OMEGA", "stops in normal order, not in applicative order")
  , ("(\\x.\\y.x) y", "variable capture: y must be renamed") ]

maxSteps :: Int
maxSteps = 300                                -- ★ 簡約の回数の上限

-- 歩みの 1 つ：式と、次に簡約する場所（なければ正規形）。文字列の色分けは初めて描くときに 1 回だけ作られる
data H = H { hM :: Lam, hRedex :: Maybe [Int], hSize :: !Int, hSegs :: [(String, Bool)], hStr :: String }

mkH :: Order -> Lam -> H
mkH o m = let r = lamRedex m o in H m r (lamSize m) (lamStrHi m r) (lamStr m)

data End = Going | EndNormal | EndSteps | EndSize deriving (Eq)

data World = World
  { wSrc   :: String
  , wP     :: !Int
  , wErr   :: String
  , wHist  :: Q.Seq H                         -- これまでの式（0 歩目から）
  , wAt    :: !Int                            -- いま見ている歩
  , wOrder :: Order
  , wAuto  :: !Bool
  , wEnd   :: End
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-08 Lambda Calculus" (loadPreset 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPreset 0 world0) { wTest = selfTest }
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` 4 == 0 then stepForward w else w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World "" 0 "" Q.empty 0 Normal False Going []

loadPreset :: Int -> World -> World
loadPreset i w = let k = wrapI i (length presets) in rebuild w { wP = k, wSrc = fst (at' presets k ("", "")) }

-- 式を読み直す：まちがいなら説明を出して、前の正しい式のまま
rebuild :: World -> World
rebuild w = case lamParse (wSrc w) of
  Left e -> w { wErr = errLine e }
  Right m -> advanceInfo w { wErr = "", wHist = Q.singleton (mkH (wOrder w) m), wAt = 0, wAuto = False, wEnd = Going }

curH :: World -> Maybe H
curH w = Q.lookup (wAt w) (wHist w)

-- いまの式の状態（正規形・上限・大きすぎ・まだ）
advanceInfo :: World -> World
advanceInfo w = case curH w of
  Nothing -> w
  Just h -> w { wEnd = if hRedex h == Nothing then EndNormal else if wAt w >= maxSteps then EndSteps
                       else if hSize h > lamMaxSize then EndSize else Going }

stepForward :: World -> World
stepForward w
  | wAt w < Q.length (wHist w) - 1 = advanceInfo w { wAt = wAt w + 1 }
  | otherwise =
      let w1 = advanceInfo w
      in if wEnd w1 /= Going then w1 { wAuto = False } else case curH w1 >>= \h -> lamStep (hM h) (wOrder w1) of
           Nothing -> w1 { wAuto = False }
           Just (m', _) -> advanceInfo w1 { wHist = wHist w1 Q.|> mkH (wOrder w1) m', wAt = wAt w1 + 1 }

-- 色つきの部分を幅で折り返す（1 行に入る文字の数は同じとみなす）→ 行 [(y, 文字列, 黄色の範囲 [a, b))]、省略したか、最後の y
-- 黄色の部分は「行の頭から b 文字目までを黄色、a 文字目までを白で上書き」で描くので、文字の幅がちがっても位置がずれない
layoutSegs :: [(String, Bool)] -> Float -> Float -> Float -> Int -> ([(Float, String, Maybe (Int, Int))], Bool, Float)
layoutSegs segs y w size maxLines =
  let cs = [ (c, hi) | (s, hi) <- segs, c <- s ]
      perLine = max 1 (floor (w / (size * 0.72))) :: Int
      lh = size * 1.35
      chunks = chunk perLine cs
      shown = take maxLines chunks
      cut = length chunks > maxLines
      line i ch = let idx = [ k | (k, (_, h)) <- zip [0 ..] ch, h ]
                  in (y + i2f i * lh, map fst ch, if null idx then Nothing else Just (headOr 0 idx, lastOr 0 idx + 1))
      n = length shown
  in (zipWith line [0 ..] shown, cut, y + i2f (max 1 n + (if cut then 1 else 0)) * lh)
  where chunk k xs = if null xs then [] else take k xs : chunk k (drop k xs)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Lambda calculus: (\\x.M) N -> M[x := N]   order: " ++ (if wOrder w == Normal then "normal (outer, left)" else "applicative (inner, left)"))
      "Type a term (\\ = lambda, digits = Church numerals, CAPS = named terms)   Right step   Left back\nEnter auto   Tab order   Up/Down examples   Backspace delete   Shift+Backspace clear"
  , drawInputLine (envFrame env) 30 108 "Term:" (wSrc w) (wErr w) True ]
  ++ maybe [] (drawCur env w) (curH w)
  ++ drawDefs env
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

drawCur :: Env -> World -> H -> [Picture]
drawCur env w h =
  let (rows, cut, yEnd) = layoutSegs (hSegs h) 170 (envW env * 0.6) 19 9
      y = max (yEnd + 10) 330
      first = max 0 (wAt w - 11)
      nChars = max 10 (floor ((envW env * 0.66 - 90) / (14 * 0.72)))
      (msg, mc) = case wEnd w of
        EndNormal -> ("Normal form (no more reductions)" ++ maybe "" ("  ->  " ++) (lamMeaning (hM h)), grn)
        EndSteps -> ("Hit the limit of " ++ show maxSteps ++ " steps: it may never stop", yel)
        EndSize -> ("The term grew too big (" ++ show lamMaxSize ++ " symbols)", yel)
        Going -> (if wAuto w then "reducing..." else "Reduce the yellow (\\x.M) N  (Right arrow)", ink)
  in [ say ("Example " ++ show (wP w + 1) ++ ": " ++ snd (at' presets (wP w) ("", "")) ++ "    step " ++ show (wAt w) ++ "    size " ++ show (hSize h) ++ " symbols") 30 152 14 (col dimc) ]
     ++ concat [ [say str 30 (ry + 15) 19 (col ink)] ++ maybe [] (\(a, b) -> [say (take b str) 30 (ry + 15) 19 (col yel), say (take a str) 30 (ry + 15) 19 (col ink)]) hi
               | (ry, str, hi) <- rows ]
     ++ [ say "... (too long, cut)" 30 (yEnd - 19 * 1.35 + 15) 19 (col dimc) | cut ]
     ++ [ say msg 30 y 18 (col mc)
        , say "History (newest at the bottom)" 30 (y + 34) 14 (col sky) ]
     ++ [ say (show i ++ "  " ++ clip nChars (hStr hi)) 30 (y + 58 + i2f (i - first) * 19) 14 (col (if i == wAt w then ink else dimc))
        | i <- [first .. wAt w], Just hi <- [Q.lookup i (wHist w)] ]

-- 右：名前つきの式
drawDefs :: Env -> [Picture]
drawDefs env =
  let rx = envW env * 0.66
      names = ["0", "1", "2", "SUCC", "PLUS", "TIMES", "TRUE", "FALSE", "IF", "NOT", "AND", "K", "S", "OMEGA", "Y"]
      defOf nm = if all (`elem` ['0' .. '9']) nm then lamStr (churchNum (read nm)) else maybe "" id (lookup nm lamDefs)
      note = [ "n = 'use f n times'. PLUS m n = n times, then"
             , "m times. TRUE/FALSE pick 1 of 2 things."
             , "Church numerals up to " ++ show lamMaxNum ++ "."
             , "Y calls itself (recursion): as powerful as"
             , "a Turing machine (series 66 / 10)." ]
  in [ say "Named terms (write them in CAPS)" rx 150 15 (col yel) ]
     ++ concat [ [ say nm rx (178 + i2f i * 21) 14 (col sky), say (defOf nm) (rx + 78) (178 + i2f i * 21) 14 (col ink) ] | (i, nm) <- zip [0 :: Int ..] names ]
     ++ [ say ln rx (178 + 15 * 21 + 20 + i2f i * 19) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] note ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TUp -> loadPreset (wP w - 1) w
    TDown -> loadPreset (wP w + 1) w
    TRight | hasH -> stepForward w
    TLeft | hasH -> advanceInfo w { wAt = max 0 (wAt w - 1) }
    TEnter | hasH -> w { wAuto = not (wAuto w) }
    TTab -> rebuild w { wOrder = if wOrder w == Normal then Applicative else Normal }
    TBack -> rebuild w { wSrc = dropLast (wSrc w) }
    TClear -> rebuild w { wSrc = "" }
    TChar ch -> rebuild w { wSrc = typeInto (wSrc w) ch lamKeys 120 }
    _ -> rebuild w
  _ -> w
  where hasH = not (Q.null (wHist w))

-- ---------- やってみよう ----------
-- 1. PLUS 2 3 を → で 1 歩ずつ。PLUS の \m.\n.\f.\x に 2 と 3 が入っていく様子を追おう
-- 2. TIMES 3 3 は何歩？ POW 2 3 は？ 数が大きくなると歩数はどう増える？
-- 3. K a OMEGA で Tab（作用的順序）にすると止まらない。「使わない引数を先に計算してしまう」から
-- 4. (\x.\y.x) y で、もし名前を変えずに置きかえると \y.y（恒等関数）になってしまう。意味がどう変わる？
-- 5. lamDefs に足そう：OR、XOR、ISZERO を使った「0 なら 1、そうでなければ 2」など

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("PLUS 2 3 は 5 に、TIMES 2 3 は 6 に正規化される", isNum "PLUS 2 3" 5 && isNum "TIMES 2 3" 6)
  , ("PRED 3 = 2、POW 2 3 = 8", isNum "PRED 3" 2 && isNum "POW 2 3" 8)
  , ("SUCC n = n + 1（n = 0〜10）、PLUS と TIMES は a, b = 0〜4 の全部 25 組で正しい", okS && okP)
  , ("IF TRUE a b = a、NOT FALSE = TRUE、AND TRUE FALSE = FALSE、ISZERO 0 = TRUE"
    , same "IF TRUE a b" "a" && same "NOT FALSE" "TRUE" && same "AND TRUE FALSE" "FALSE" && same "ISZERO 0" "TRUE")
  , ("Ω = (λx.x x)(λx.x x) は 100 歩の上限に当たる（1 歩で自分に戻る）"
    , let (m, _, st) = run "OMEGA" Normal 100 in st == LSteps && lamAlphaEq m (parsed "OMEGA"))
  , ("K a Ω：正規順序は 2 歩で a、作用的順序は上限に当たる"
    , let (ma, na, sa) = run "K a OMEGA" Normal maxSteps
          (_, _, sb) = run "K a OMEGA" Applicative 100
      in sa == LNormal && na == 2 && ma == LV "a" && sb == LSteps)
  , ("変数の捕獲をさける：(λx.λy.x) y = λz.y（λy.y ではない）"
    , let (m, _, _) = run "(\\x.\\y.x) y" Normal maxSteps in lamAlphaEq m (parsed "\\z.y") && not (lamAlphaEq m (parsed "\\y.y")))
  , ("でたらめな λ 式 200 個：文字列にして読み直すと同じ式", rt == 200)
  , ("おかしな入力 " ++ show (length bad) ++ " 個（知らない名前、大きすぎる数を含む）は例外を出さずに説明を返す"
    , all (\b -> either (const True) (const False) (lamParse b)) bad) ]
  where
    parsed s = either (const (LV "?")) id (lamParse s)
    run s o n = lamNormalize (parsed s) n o
    isNum s v = let (m, _, st) = run s Normal maxSteps in st == LNormal && churchToInt m == Just v
    okS = and [ isNum ("SUCC " ++ show n) (n + 1) | n <- [0 .. 10] ]
    okP = and [ isNum ("PLUS " ++ show a ++ " " ++ show b) (a + b) && isNum ("TIMES " ++ show a ++ " " ++ show b) (a * b) | a <- [0 .. 4], b <- [0 .. 4] ]
    same s t = let (m, _, _) = run s Normal maxSteps in lamAlphaEq m (parsed t)
    -- でたらめな λ 式（乱数は p5.js 版と同じ順に引く）
    rl :: Int -> R Lam
    rl d = do
      r <- rnd
      if d <= 0 || r < 0.3 then LV <$> rndPick ["x", "y", "z"]
      else if r < 0.6 then do v <- rndPick ["x", "y"]; LL v <$> rl (d - 1)
      else do f <- rl (d - 1); x <- rl (d - 1); pure (LA f x)
    ms = withSeed 12 (mapM (const (rl 5)) [1 .. 200 :: Int])
    rt = length [ () | m <- ms, Right m' <- [lamParse (lamStr m)], lamDB m' == lamDB m ]
    bad = ["", "\\", "\\x", "(x", "x)", ".x", "FOO", "99", "123456ztr casnfg", "λ.x", "x ..y"]

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

-- ---- ラムダ計算の道具：λ 式 = LV 変数 / LL v b（\v.b）/ LA f x（適用 f x）
-- 書き方：\x.x（\x y.M = \x.\y.M）、適用は並べるだけ（左から：f x y = (f x) y）、
-- 数字 n = チャーチ数 \f.\x.f(...(f x))、大文字の名前 = 下の lamDefs
data Lam = LV String | LL String Lam | LA Lam Lam deriving (Eq, Show)

lamKeys :: String                             -- 入力できる文字
lamKeys = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "\\.() '"

lamMaxNum, lamMaxSize :: Int
lamMaxNum = 12                                -- ★ チャーチ数の上限
lamMaxSize = 2500                             -- ★ 式の大きさ（記号の数）の上限

lamDefs :: [(String, String)]                 -- ★ 名前つきの式（大文字）。自分で足してもよい
lamDefs =
  [ ("I", "\\x.x"), ("K", "\\x y.x"), ("S", "\\x y z.x z (y z)"), ("B", "\\f g x.f (g x)"), ("C", "\\f x y.f y x")
  , ("TRUE", "\\t f.t"), ("FALSE", "\\t f.f"), ("IF", "\\b t e.b t e"), ("NOT", "\\b.b FALSE TRUE"), ("AND", "\\a b.a b FALSE"), ("OR", "\\a b.a TRUE b")
  , ("SUCC", "\\n f x.f (n f x)"), ("PLUS", "\\m n f x.m f (n f x)"), ("TIMES", "\\m n f.m (n f)"), ("POW", "\\b e.e b")
  , ("PRED", "\\n f x.n (\\g h.h (g f)) (\\u.x) (\\u.u)"), ("ISZERO", "\\n.n (\\x.FALSE) TRUE")
  , ("OMEGA", "(\\x.x x) (\\x.x x)"), ("Y", "\\f.(\\x.f (x x)) (\\x.f (x x))") ]

churchNum :: Int -> Lam
churchNum n = LL "f" (LL "x" (iterate (LA (LV "f")) (LV "x") !! max 0 n))

lamSize :: Lam -> Int
lamSize m = case m of
  LV _ -> 1
  LL _ b -> 1 + lamSize b
  LA f x -> 1 + lamSize f + lamSize x

data LTk = LTLam | LTDot | LTL | LTR | LTVar String | LTDef String | LTNum String deriving (Eq, Show)

ltkName :: LTk -> String
ltkName t = case t of
  LTLam -> "lam"
  LTDot -> "."
  LTL -> "("
  LTR -> ")"
  LTVar _ -> "var"
  LTDef _ -> "def"
  LTNum _ -> "num"

-- 返り値 Right 式 / Left (説明, 何文字目)（例外は出さない）
lamParse :: String -> Either PErr Lam
lamParse = lamParseG 0

lamParseG :: Int -> String -> Either PErr Lam
lamParseG guard src
  | length src > 200 = Left ("too long (200 characters max)", 200)
  | otherwise = do
      toks <- lexL 0 src
      if null toks then Left ("empty term", 0) else do
        (m, rest) <- pTerm 0 toks
        case rest of
          ((t, a) : _) -> Left (if t == LTR then "extra ')'" else "something here cannot be read", a)
          [] -> if lamSize m > lamMaxSize then Left ("too big (" ++ show lamMaxSize ++ " symbols max)", 0) else Right m
  where
    len = length src
    lexL _ [] = Right []
    lexL i s@(c : rest)
      | c == ' ' = lexL (i + 1) rest
      | c == '\\' || c == 'λ' = ((LTLam, i) :) <$> lexL (i + 1) rest
      | c == '.' = ((LTDot, i) :) <$> lexL (i + 1) rest
      | c == '(' = ((LTL, i) :) <$> lexL (i + 1) rest
      | c == ')' = ((LTR, i) :) <$> lexL (i + 1) rest
      | c >= 'a' && c <= 'z' = run (\d -> (d >= '0' && d <= '9') || d == '\'') LTVar
      | c >= 'A' && c <= 'Z' = run (\d -> d >= 'A' && d <= 'Z') LTDef
      | c >= '0' && c <= '9' = run (\d -> d >= '0' && d <= '9') LTNum
      | otherwise = Left ("character '" ++ asciiOnly [c] ++ "' not allowed", i)
      where run p k = let tl = takeWhile p rest
                          n = 1 + length tl
                      in ((k (c : tl), i) :) <$> lexL (i + n) (drop n s)
    atOf ts = case ts of
      ((_, a) : _) -> a
      [] -> len
    failAt ts m = Left (m, atOf ts)
    stopper t = t == LTR || t == LTDot
    pTerm d ts
      | d + 1 > 80 = failAt ts "nesting too deep"
      | otherwise = case ts of
          ((LTLam, _) : r) ->
            let vs = [ v | (LTVar v, _) <- takeWhile (isVarT . fst) r ]
                r1 = dropWhile (isVarT . fst) r
            in if null vs then failAt r1 "a variable name (lower case) is needed after \\"
               else case r1 of
                 ((LTDot, _) : r2) -> do
                   (b, r3) <- pTerm (d + 1) r2
                   Right (foldr LL b vs, r3)
                 _ -> failAt r1 "'.' is needed after \\x"
          _ -> appLoop d Nothing ts
    isVarT t = case t of LTVar _ -> True; _ -> False
    appLoop d acc ts = case ts of
      ((t, _) : _) | not (stopper t) -> do
        (a, r) <- if t == LTLam then pTerm (d + 1) ts else pAtom (d + 1) ts
        appLoop d (Just (maybe a (\f -> LA f a) acc)) r
      _ -> case acc of
        Nothing -> failAt ts (case ts of
                                ((t, _) : _) -> "nothing before '" ++ ltkName t ++ "'"
                                [] -> "the term ends too early")
        Just m -> case ts of
          ((LTDot, _) : _) -> failAt ts "'.' only after \\x"
          _ -> Right (m, ts)
    pAtom d ts = case ts of
      ((LTVar v, _) : r) -> Right (LV v, r)
      ((LTNum s, _) : r) ->
        let v = read (take 6 s) :: Integer
            big = length s > 6 || v > fromIntegral lamMaxNum
        in if big then failAt ts ("number too big (" ++ show lamMaxNum ++ " max)") else Right (churchNum (fromIntegral v), r)
      ((LTDef nm, _) : r) -> case lookup nm lamDefs of
        Nothing -> failAt ts ("no term named '" ++ nm ++ "'")
        Just def
          | guard > 6 -> failAt ts "names inside names are too deep"
          | otherwise -> case lamParseG (guard + 1) def of
              Left (e, _) -> failAt ts ("definition of " ++ nm ++ " is wrong: " ++ e)
              Right m -> Right (m, r)
      ((LTL, _) : r) -> do
        (m, r2) <- pTerm d r
        case r2 of
          ((LTR, _) : r3) -> Right (m, r3)
          _ -> failAt r2 "')' missing"
      _ -> failAt ts "a term is needed here"

-- 文字列に。hi（簡約する場所：根からの道）の部分を (文字列, True) で返す。同じ色の部分はつなげる
lamStrHi :: Lam -> Maybe [Int] -> [(String, Bool)]
lamStrHi m0 hi = merge (go m0 [] False)
  where
    go m path inHi =
      let h = inHi || Just (reverse path) == hi
          mark s = (s, h)
      in case m of
        LV n -> [mark n]
        LL v b -> mark ("\\" ++ v ++ ".") : go b (0 : path) h
        LA f x ->
          let pf = case f of LL _ _ -> True; _ -> False
              px = case x of LV _ -> False; _ -> True
          in [mark "(" | pf] ++ go f (0 : path) h ++ [mark ")" | pf] ++ [mark " "]
             ++ [mark "(" | px] ++ go x (1 : path) h ++ [mark ")" | px]
    merge xs = case xs of
      ((s1, h1) : (s2, h2) : r) | h1 == h2 -> merge ((s1 ++ s2, h1) : r)
      (p : r) -> p : merge r
      [] -> []

lamStr :: Lam -> String
lamStr m = concatMap fst (lamStrHi m Nothing)

lamFree :: Lam -> S.Set String                -- 自由変数
lamFree m = case m of
  LV n -> S.singleton n
  LL v b -> S.delete v (lamFree b)
  LA f x -> S.union (lamFree f) (lamFree x)

lamAllNames :: Lam -> S.Set String
lamAllNames m = case m of
  LV n -> S.singleton n
  LL v b -> S.insert v (lamAllNames b)
  LA f x -> S.union (lamAllNames f) (lamAllNames x)

freshName :: String -> S.Set String -> String
freshName v avoid = if S.member v avoid then freshName (v ++ "'") avoid else v

-- m[x := s]（変数の捕獲をさける：ぶつかる束縛変数は名前を変える）
lamSubst :: Lam -> String -> Lam -> Lam
lamSubst m0 x s = go m0
  where
    fvS = lamFree s
    go m = case m of
      LV n -> if n == x then s else m
      LA f a -> LA (go f) (go a)
      LL v b
        | v == x -> m
        | not (S.member v fvS) -> LL v (go b)
        | otherwise ->
            let avoid = S.insert x (S.union fvS (lamAllNames b))
                w = freshName v avoid
            in LL w (go (lamSubst b v (LV w)))

data Order = Normal | Applicative deriving (Eq, Show)

-- 次に簡約する (\x.M) N の場所（根からの道）。Normal = 外側・左から、Applicative = 内側・左から
lamRedex :: Lam -> Order -> Maybe [Int]
lamRedex m order = case m of
  LV _ -> Nothing
  LL _ b -> (0 :) <$> lamRedex b order
  LA f x
    | order == Normal && isL f -> Just []
    | otherwise -> case (0 :) <$> lamRedex f order of
        Just p -> Just p
        Nothing -> case (1 :) <$> lamRedex x order of
          Just p -> Just p
          Nothing -> if isL f then Just [] else Nothing
  where isL t = case t of LL _ _ -> True; _ -> False

lamAt :: Lam -> [Int] -> Maybe Lam            -- 道の先の部分式
lamAt m p = case (p, m) of
  ([], _) -> Just m
  (0 : r, LL _ b) -> lamAt b r
  (0 : r, LA f _) -> lamAt f r
  (1 : r, LA _ x) -> lamAt x r
  _ -> Nothing

lamReplaceAt :: Lam -> [Int] -> Lam -> Lam    -- 道の先を rep に置きかえた新しい木
lamReplaceAt m p rep = case (p, m) of
  ([], _) -> rep
  (0 : r, LL v b) -> LL v (lamReplaceAt b r rep)
  (0 : r, LA f x) -> LA (lamReplaceAt f r rep) x
  (1 : r, LA f x) -> LA f (lamReplaceAt x r rep)
  _ -> m

-- β 簡約 1 歩：Just (新しい式, 簡約した場所) / Nothing（正規形）
lamStep :: Lam -> Order -> Maybe (Lam, [Int])
lamStep m order = do
  p <- lamRedex m order
  r <- lamAt m p
  case r of
    LA (LL v b) x -> Just (lamReplaceAt m p (lamSubst b v x), p)
    _ -> Nothing

data LStatus = LNormal | LSteps | LSize deriving (Eq, Show)

-- 返り値 (式, 手数, 状態：正規形 / 手数の上限 / 大きさの上限)
lamNormalize :: Lam -> Int -> Order -> (Lam, Int, LStatus)
lamNormalize m0 maxSteps order = go m0 0
  where
    go m !n
      | n >= maxSteps = (m, n, if lamRedex m order /= Nothing then LSteps else LNormal)
      | otherwise = case lamStep m order of
          Nothing -> (m, n, LNormal)
          Just (m', _) -> if lamSize m' > lamMaxSize then (m', n + 1, LSize) else go m' (n + 1)

-- ド・ブラウン記法の文字列（名前によらない形 → α 同値の判定に）
lamDB :: Lam -> String
lamDB m0 = go m0 []
  where
    go m ctx = case m of
      LV n -> case lookupIdx n ctx of
        Nothing -> n
        Just i -> '#' : show i
      LL v b -> "\\" ++ go b (v : ctx)
      LA f x -> "(" ++ go f ctx ++ " " ++ go x ctx ++ ")"
    lookupIdx n ctx = lookup n (zip ctx [0 :: Int ..])

lamAlphaEq :: Lam -> Lam -> Bool
lamAlphaEq a b = lamDB a == lamDB b

-- \f.\x.f (f ... x) なら数、ちがえば Nothing
churchToInt :: Lam -> Maybe Int
churchToInt m = case m of
  LL f (LL x b) | f /= x -> count f x b 0
  _ -> Nothing
  where count f x b !n = case b of
          LA (LV g) r | g == f -> count f x r (n + 1)
          LV y | y == x -> Just n
          _ -> Nothing

lamDef :: String -> Lam                       -- 名前つきの式（読めなければ \x.x）
lamDef nm = either (const (LL "x" (LV "x"))) id (lamParse (maybe "" id (lookup nm lamDefs)))

-- 結果の読み方：数・真偽
lamMeaning :: Lam -> Maybe String
lamMeaning m
  | lamAlphaEq m (lamDef "TRUE") = Just "TRUE"
  | lamAlphaEq m (lamDef "FALSE") = Just "FALSE (same shape as 0)"
  | otherwise = ("Church numeral " ++) . show <$> churchToInt m

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
