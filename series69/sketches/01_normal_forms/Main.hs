-- ============================================
-- 69-01 標準形 ─ どんな式も「節の &」（CNF 和積標準形）か「項の |」（DNF 積和標準形）に書きかえられる
-- (1) -> と <-> を ~ & | だけにする　(2) 否定を内側へ押しこむ（ド・モルガン、~~A = A）　(3) 分配法則で & と | を入れかえる
-- 書きかえは 1 回に 1 か所（外側・左から）。どの段階でも真理値表は変わらない（同値）。でも分配で式は爆発的に長くなることがある
-- 操作：文字キー = 入力（00 と同じ。~ & | -> <-> 1 0 T _|_）/ Backspace / Shift + Backspace全部消す / ↑↓ お手本
--       → 1 歩進む / ← 1 歩もどる / Enter 最後まで / Tab CNF ⇔ DNF
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Array (Array, listArray, bounds, (!))
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

presets :: [String]                           -- ★ お手本の式
presets =
  [ "(p -> q) & (q -> p)"
  , "p <-> q"
  , "~(p & (q | ~r))"
  , "(p & q) | (r & s)"
  , "(a & b) | (c & d) | (e & f)"
  , "~((p -> q) -> p) | _|_"
  , "p <-> (q <-> r)" ]

maxSteps, maxSize :: Int
maxSteps = 600                                -- ★ 書きかえの回数の上限
maxSize = 3000                                -- ★ 式の大きさの上限

phaseName :: Int -> String
phaseName ph = case ph of
  0 -> "(1) remove -> <->"
  1 -> "(2) push ~ inward"
  _ -> "(3) distribute"

-- 最後まで書きかえた記録（真理値表の確認と文字列は、見るときに 1 回だけ計算する）
data Run = Run
  { rF0     :: F
  , rSteps  :: Array Int NfStep
  , rOk     :: Array Int Bool                 -- その歩の式がはじめの式と同値か
  , rStr    :: Array Int String               -- その歩の式の文字列
  , rResult :: F
  , rBound  :: !Bool }

-- 爆発の表の 1 行
data Blow = Blow { bN :: !Int, bSize :: !Int, bClauses :: !Int, bCSize :: !Int, bSteps :: !Int }

data World = World
  { wSrc    :: String
  , wP      :: !Int
  , wTarget :: !NfTarget
  , wRun    :: Maybe Run
  , wAt     :: !Int                           -- いま見ている歩
  , wErr    :: String
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-01 Normal Forms" (loadPreset 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPreset 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World "" 0 CNF Nothing 0 "" []

nSteps :: Run -> Int                          -- 歩の数（はじめの式を除く）
nSteps r = snd (bounds (rSteps r))

loadPreset :: Int -> World -> World
loadPreset i w = let k = wrapI i (length presets) in (rebuild w { wP = k, wSrc = at' presets k "" }) { wAt = 0 }

rebuild :: World -> World
rebuild w = case parseFormula (wSrc w) of
  Left e -> w { wErr = errLine e }
  Right f ->
    let r = makeRun f (wTarget w)
    in w { wErr = "", wRun = Just r, wAt = clampI (wAt w) 0 (nSteps r) }

makeRun :: F -> NfTarget -> Run
makeRun f t =
  let (st, res, hit) = normalSteps f t maxSteps maxSize
      n = length st
      arr = listArray (0, n - 1) st
  in Run f arr (listArray (0, n - 1) [ sameTable f (nsF s) | s <- st ]) (listArray (0, n - 1) [ fmlStr (nsF s) | s <- st ]) res hit

-- ★ 爆発の例：(x1 & y1) | (x2 & y2) | ... | (xn & yn) の CNF は 2ⁿ 個の節
blowupFml :: Int -> F
blowupFml n = foldl1' [ FBin OAnd (FVar ("x" ++ show i)) (FVar ("y" ++ show i)) | i <- [1 .. max 1 n] ]
  where foldl1' (t : ts) = foldl' (FBin OOr) t ts
        foldl1' [] = FTop

blowup :: [Blow]
blowup = [ let (st, res, _) = normalSteps (blowupFml n) CNF 5000 20000
           in Blow n (fmlSize (blowupFml n)) (length (cnfClauses res)) (fmlSize res) (length st - 1)
         | n <- [1 .. 5] ]

dnfTerms :: F -> [[String]]
dnfTerms f
  | f == FBot = []
  | f == FTop = [[]]
  | otherwise = [ litSort (map litStr (flatten c OAnd)) | c <- flatten f OOr ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Normal forms: rewrite step by step into " ++ (if wTarget w == CNF then "CNF (clauses joined by &)" else "DNF (terms joined by |)"))
      "Type a formula   Backspace delete   Shift+Backspace clear   Up/Down examples\nRight 1 step   Left back   Enter to the end   Tab CNF <-> DNF"
  , drawInputLine (envFrame env) 30 114 "Formula:" (wSrc w) (wErr w) True ]
  ++ maybe [] (drawRun env w) (wRun w)
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

drawRun :: Env -> World -> Run -> [Picture]
drawRun env w r =
  let wd = envW env
      ht = envH env
      n = nSteps r
      a = clampI (wAt w) 0 n
      st = rSteps r ! a
      sz = fmlSize (nsF st)
      info = (if a == 0 then "start" else phaseName (nsPhase st) ++ "   step " ++ show a ++ " / " ++ show n ++ ": " ++ nsName st)
             ++ (if rBound r && a == n then "   (stopped at " ++ show maxSteps ++ " steps / size " ++ show maxSize ++ ")" else "")
      tsz = if sz > 60 then 16 else 22
      bigLines = wrapWords (floor (wd * 0.56 / (tsz * 0.72))) 5 (rStr r ! a)
      ok = rOk r ! a
      nv = length (fmlVars (rF0 r))
      -- ---- 手順の一覧
      y0 = 362
      lh = 21
      rows = max 1 (floor ((ht - y0 - 120) / lh))
      first = clampI (a - rows `div` 2) 0 (max 0 (n + 1 - rows))
      lineChars = floor ((wd * 0.6 - 76) / (14 * 0.72 * 1.2))
      stepRow i =
        let s = rSteps r ! i
            y = y0 + i2f (i - first) * lh
            c = if i == 0 then dimc else at' [sky, yel, grn] (nsPhase s) dimc
            line1 = (if i == 0 then "" else nsName s ++ "  :  ") ++ take (lineChars + 5) (rStr r ! i)
        in [ Color (col linec) (rectTL 26 (y - 2) (wd * 0.56) lh) | i == a ]
           ++ [ say (if i == 0 then "0" else show i ++ " " ++ show (nsPhase s + 1)) 32 (y + 14) 14 (col c)
              , say (clip lineChars line1) 76 (y + 14) 14 (col (if i <= a then ink else dimc)) ]
      -- ---- 右：節（または項）の一覧
      rx = wd * 0.62
      isC = wTarget w == CNF
      parts = if isC then cnfClauses (rResult r) else dnfTerms (rResult r)
      np = length parts
      partStr c = if null c then (if isC then "empty clause [] (false)" else "empty term (true)")
                  else "(" ++ joinWith (if isC then " | " else " & ") c ++ ")"
      -- ---- 右下：爆発の表
      by = ht - 250
      cxs = [0, 40, 140, 220, 320] :: [Float]
      rAt j = rx + at' cxs j 0 + (if j > 0 then 70 else 12)
  in [ say info 30 160 14 (col dimc) ]
     ++ [ say ln 30 (190 + i2f i * 28) tsz (col ink) | (i, ln) <- zip [0 :: Int ..] bigLines ]
     ++ [ say ((if ok then "OK equivalent to the start (" else "NOT equivalent (") ++ show (2 ^ nv :: Int) ++ "-row truth table)   size "
               ++ show (fmlSize (rF0 r)) ++ " -> " ++ show sz ++ " symbols") 30 344 14 (col (if ok then grn else pink)) ]
     ++ concatMap stepRow [first .. min n (first + rows - 1)]
     ++ [ say (if isC then "CNF clauses (fully rewritten)" else "DNF terms (fully rewritten)") rx 160 16 (col yel) ]
     ++ [ say (if isC then "no clauses = T (always true)" else "no terms = _|_ (always false)") rx 190 15 (col ink) | np == 0 ]
     ++ [ say (clip 36 (show (i + 1) ++ ". " ++ partStr c)) rx (190 + i2f i * 22) 15 (col ink) | (i, c) <- zip [0 :: Int ..] (take 14 parts) ]
     ++ [ say ("... " ++ show (np - 14) ++ " more (" ++ show np ++ " in all)") rx (190 + 14 * 22) 15 (col dimc) | np > 14 ]
     ++ [ say "Blow-up: (x1&y1) | ... | (xn&yn) to CNF" rx by 15 (col yel) ]
     ++ [ sayR h (rAt j) (by + 24) 14 (col dimc) | (j, h) <- zip [0 ..] ["n", "size", "clauses", "CNF size", "steps"] ]
     ++ [ sayR (show v) (rAt j) (by + 46 + i2f i * 20) 14 (col (if j == 2 then yel else ink))
        | (i, b) <- zip [0 :: Int ..] blowup, (j, v) <- zip [0 ..] [bN b, bSize b, bClauses b, bCSize b, bSteps b] ]
     ++ [ say ln rx (by + 160 + i2f i * 17) 14 (col dimc)
        | (i, ln) <- zip [0 :: Int ..] (wrapWords (floor ((wd - rx - 20) / (14 * 0.72 * 1.1))) 3
            "2^n clauses. Tseitin (series 18 / 04) adds new variables: a CNF of linear size, equisatisfiable (not equivalent)") ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case (typedKey k mods, wRun w) of
    (TUp, _) -> loadPreset (wP w - 1) w
    (TDown, _) -> loadPreset (wP w + 1) w
    (TRight, Just r) -> w { wAt = min (wAt w + 1) (nSteps r) }
    (TLeft, Just _) -> w { wAt = max (wAt w - 1) 0 }
    (TEnter, Just r) -> w { wAt = nSteps r }
    (TTab, _) -> rebuild w { wTarget = if wTarget w == CNF then DNF else CNF, wAt = 0 }
    (TBack, _) -> rebuild w { wSrc = dropLast (wSrc w), wAt = 0 }
    (TClear, _) -> rebuild w { wSrc = "", wAt = 0 }
    (TChar ch, _) -> rebuild w { wSrc = typeInto (wSrc w) ch fmlKeys 80, wAt = 0 }
    _ -> rebuild w { wAt = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. p <-> q を CNF と DNF の両方にしよう（Tab）。どちらが短い？ 真理値表の「1 の行」「0 の行」との関係は？
-- 2. (a & b) | (c & d) | (e & f) を CNF に。節は何個？ 右下の表と同じ 2³ = 8 個になる理由を考えよう
-- 3. (3) の分配を先にやると（(2) をとばすと）どうなる？ nfRules の順番を入れかえて試そう
-- 4. CNF は SAT ソルバ（シリーズ 18 の 03）や 03 の導出原理の入力の形。DNF なら充足可能かは一目でわかる。なぜ？
-- 5. p <-> (q <-> r) を CNF に。変数を増やした p <-> (q <-> (r <-> s)) では節がいくつ？（上限に当たるかも）

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな式 " ++ show nT ++ " 個（変数 4 つまで、⊤ ⊥ ↔ 入り）：CNF は CNF の形で、もとの式と同値（上限に当たった " ++ show bound ++ " 個を除く）"
    , okC == nT - bound && bound < 5)
  , ("同じく DNF も DNF の形で同値", okD == nT - bound)
  , ("② のあとは否定が変数の直前だけ（否定標準形 NNF）", okN == nT - bound)
  , ("40 個の全部の書きかえの 1 歩 1 歩で真理値表が変わらない", stepsOk)
  , ("爆発：(x1∧y1) ∨ … ∨ (xn∧yn) の CNF の節は 2ⁿ 個（n = 1〜5）", all (\b -> bClauses b == 2 ^ bN b) blowup)
  , ("⊤ ∧ p → p、¬(p ∨ ⊥) → ¬p、p ∨ ⊤ → ⊤（節なし）、p ∧ ⊥ → ⊥（空節 1 つ）"
    , fmlStr (cnfOf "1 & p") == "p" && fmlStr (cnfOf "~(p | 0)") == "~p" && null (cnfClauses (cnfOf "p | 1"))
      && cnfClauses (cnfOf "p & 0") == [[]])
  , ("(p ∨ q) ∧ (r ∨ s) の DNF は項が 4 つ（p∧r, p∧s, q∧r, q∧s）", length (dnfTerms (resOf "(p | q) & (r | s)" DNF)) == 4)
  , ("p ↔ q の CNF は (¬p ∨ q) ∧ (¬q ∨ p)、最初の 1 歩は ↔ の除去"
    , fmlStr iffRes == "(~p | q) & (~q | p)" && take 7 (nsName (at' iffSteps 1 (NfStep FTop "" 0 Nothing Nothing))) == "A <-> B") ]
  where
    nT = 150
    fs = withSeed 5 (mapM (const (randomFml 3 ["p", "q", "r", "s"] True)) [1 .. nT])
    runs = [ (k, f, normalSteps f CNF maxSteps maxSize, normalSteps f DNF maxSteps maxSize) | (k, f) <- zip [0 :: Int ..] fs ]
    good = [ x | x@(_, _, (_, _, hc), (_, _, hd)) <- runs, not (hc || hd) ]
    bound = nT - length good
    okC = length [ () | (_, f, (_, c, _), _) <- good, sameTable f c && isCNF c ]
    okD = length [ () | (_, f, _, (_, d, _)) <- good, sameTable f d && isDNF d ]
    okN = length [ () | (_, f, (st, _, _), _) <- good
                      , let g = maybe f nsF (lastMaybe [ s | s <- st, nsPhase s <= 1 ])
                      , isNNF g || g == FTop || g == FBot ]
    stepsOk = and [ sameTable f (nsF s) | (k, f, (st, _, _), _) <- good, k < 40, s <- st ]
    resOf s t = either (const FTop) (\f -> let (_, r, _) = normalSteps f t maxSteps maxSize in r) (parseFormula s)
    cnfOf s = resOf s CNF
    (iffSteps, iffRes) = either (const ([], FTop)) (\f -> let (st, r, _) = normalSteps f CNF maxSteps maxSize in (st, r)) (parseFormula "p <-> q")

lastMaybe :: [a] -> Maybe a
lastMaybe xs = if null xs then Nothing else Just (last xs)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- 単語の区切りで折り返す（1 行 n 文字まで、m 行まで。入りきらなければ最後に ...）
wrapWords :: Int -> Int -> String -> [String]
wrapWords n m s = go m (words (take (n * m + n) s))
  where
    go k ws
      | null ws = []
      | k <= 0 = []
      | otherwise =
          let (ln, rest) = fill [] 0 ws
          in if k == 1 && not (null rest) then [clip n (ln ++ " ...")] else ln : go (k - 1) rest
    fill acc len ws = case ws of
      (x : xs) | null acc || len + 1 + length x <= n -> fill (acc ++ [x]) (if null acc then length x else len + 1 + length x) xs
      _ -> (unwords acc, ws)

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

-- ---- 標準形の道具：-> <-> の除去 → 否定を内側へ（NNF）→ 分配（CNF / DNF）を 1 歩ずつ
data NfTarget = CNF | DNF deriving (Eq, Show)

type Rule = F -> Maybe (F, String)            -- 構文木 1 か所 → (書きかえた式, 規則の名前)

-- 段階ごとの書きかえ規則（0 = -> <-> の除去、1 = 否定を内側へ、2 = 分配）
nfRules :: Int -> NfTarget -> [Rule]
nfRules phase target
  | phase == 0 =
      [ \f -> case f of FBin OIff a b -> Just (FBin OAnd (FBin OImp a b) (FBin OImp b a), "A <-> B => (A -> B) & (B -> A)"); _ -> Nothing
      , \f -> case f of FBin OImp a b -> Just (FBin OOr (FNot a) b, "A -> B => ~A | B"); _ -> Nothing ]
  | phase == 1 =
      [ \f -> case f of FNot (FNot a) -> Just (a, "~~A => A"); _ -> Nothing
      , \f -> case f of FNot (FBin OAnd a b) -> Just (FBin OOr (FNot a) (FNot b), "~(A & B) => ~A | ~B (De Morgan)"); _ -> Nothing
      , \f -> case f of FNot (FBin OOr a b) -> Just (FBin OAnd (FNot a) (FNot b), "~(A | B) => ~A & ~B (De Morgan)"); _ -> Nothing
      , \f -> case f of FNot FTop -> Just (FBot, "~T => _|_"); _ -> Nothing
      , \f -> case f of FNot FBot -> Just (FTop, "~_|_ => T"); _ -> Nothing
      , \f -> case f of
          FBin op a b | (op == OAnd || op == OOr) && (isConst a || isConst b) -> Just (constRule f)
          _ -> Nothing ]
  | otherwise =
      let (o, i) = if target == CNF then (OOr, OAnd) else (OAnd, OOr)
          so = opSym o
          si = opSym i
      in [ \f -> case f of
             FBin o' a (FBin i' b c) | o' == o && i' == i ->
               Just (FBin i (FBin o a b) (FBin o a c), "A " ++ so ++ " (B " ++ si ++ " C) => (A " ++ so ++ " B) " ++ si ++ " (A " ++ so ++ " C) (distribute)")
             _ -> Nothing
         , \f -> case f of
             FBin o' (FBin i' a b) c | o' == o && i' == i ->
               Just (FBin i (FBin o a c) (FBin o b c), "(A " ++ si ++ " B) " ++ so ++ " C => (A " ++ so ++ " C) " ++ si ++ " (B " ++ so ++ " C) (distribute)")
             _ -> Nothing ]
  where isConst g = g == FTop || g == FBot

-- T と _|_ の整理：A & T => A、A | T => T など
constRule :: F -> (F, String)
constRule f = case f of
  FBin op a b ->
    let (c, other) = if a == FTop || a == FBot then (a, b) else (b, a)
    in if op == OAnd then (if c == FTop then (other, "A & T => A") else (FBot, "A & _|_ => _|_"))
       else (if c == FBot then (other, "A | _|_ => A") else (FTop, "A | T => T"))
  _ -> (f, "")

-- 書きかえ 1 回の結果：新しい式、規則の名前、書きかえた部分式、その場所（根からの道：0 = 左の子、1 = 右の子）
data RW = RW { rwF :: F, rwName :: String, rwAt :: F, rwPath :: [Int] }

-- 外側・左から順に、最初に当てはまる 1 か所だけ書きかえる
rewriteOnce :: F -> [Rule] -> Maybe RW
rewriteOnce f rules = case [ o | r <- rules, Just o <- [r f] ] of
  ((g, nm) : _) -> Just (RW g nm f [])
  [] -> case f of
    FBin op a b -> case rewriteOnce a rules of
      Just o -> Just o { rwF = FBin op (rwF o) b, rwPath = 0 : rwPath o }
      Nothing -> case rewriteOnce b rules of
        Just o -> Just o { rwF = FBin op a (rwF o), rwPath = 1 : rwPath o }
        Nothing -> Nothing
    FNot a -> sub1 FNot a
    FBox a -> sub1 FBox a
    FDia a -> sub1 FDia a
    _ -> Nothing
  where sub1 k a = case rewriteOnce a rules of
          Just o -> Just o { rwF = k (rwF o), rwPath = 0 : rwPath o }
          Nothing -> Nothing

-- 標準形にする 1 歩
data NfStep = NfStep { nsF :: F, nsName :: String, nsPhase :: !Int, nsAt :: Maybe F, nsPath :: Maybe [Int] }

-- 全部の手順。返り値 (手順, 結果, 上限に当たったか)
normalSteps :: F -> NfTarget -> Int -> Int -> ([NfStep], F, Bool)
normalSteps f target maxSteps maxSize = goPh 0 [NfStep f "start" (-1) Nothing Nothing] 1 f
  where
    goPh ph acc n cur
      | ph >= 3 = (reverse acc, cur, False)
      | otherwise = go ph (nfRules ph target) acc n cur
    go ph rules acc !n cur = case rewriteOnce cur rules of
      Nothing -> goPh (ph + 1) acc n cur
      Just o ->
        let cur' = rwF o
            acc' = NfStep cur' (rwName o) ph (Just (rwAt o)) (Just (rwPath o)) : acc
            n' = n + 1
        in if n' > maxSteps || fmlSize cur' > maxSize then (reverse acc', cur', True) else go ph rules acc' n' cur'

isLit :: F -> Bool
isLit f = case f of
  FVar _ -> True
  FNot (FVar _) -> True
  _ -> False

isNNF :: F -> Bool
isNNF f = case f of
  FNot a -> case a of FVar _ -> True; _ -> False
  FBin OImp _ _ -> False
  FBin OIff _ _ -> False
  _ -> all isNNF (fKids f)

isFlatOf :: F -> Op -> (F -> Bool) -> Bool
isFlatOf f op inner = case f of
  FBin o a b | o == op -> isFlatOf a op inner && isFlatOf b op inner
  _ -> inner f

isCNF, isDNF :: F -> Bool
isCNF f = f == FTop || f == FBot || isFlatOf f OAnd (\c -> isFlatOf c OOr isLit)
isDNF f = f == FTop || f == FBot || isFlatOf f OOr (\c -> isFlatOf c OAnd isLit)

flatten :: F -> Op -> [F]
flatten f op = case f of
  FBin o a b | o == op -> flatten a op ++ flatten b op
  _ -> [f]

litStr :: F -> String                         -- リテラルの文字列 'p' / '~p'
litStr f = case f of
  FVar n -> n
  FNot (FVar n) -> '~' : n
  _ -> fmlStr f

-- 重なりを除いて、変数の名前の順に（p の次に ~p）
litSort :: [String] -> [String]
litSort c = sortOn key (nubOrd c)
  where key l = case l of
          ('~' : r) -> r ++ "~"
          _ -> l

-- CNF の木 → 節のリスト（節 = リテラルの文字列の並び）。T = 節なし、_|_ = 空節 1 つ
cnfClauses :: F -> [[String]]
cnfClauses f
  | f == FTop = []
  | f == FBot = [[]]
  | otherwise = [ litSort (map litStr (flatten c OOr)) | c <- flatten f OAnd ]

-- 速い直接の CNF 変換（03 の節の集合づくり用）。節の重複・恒真の節は除く
toClauses :: F -> [[String]]
toClauses f =
  let (_, n, _) = normalSteps f CNF maxBound maxBound
      taut c = any (\l -> take 1 l /= "~" && ('~' : l) `elem` c) c
  in nubOrd (filter (not . taut) (cnfClauses n))

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
