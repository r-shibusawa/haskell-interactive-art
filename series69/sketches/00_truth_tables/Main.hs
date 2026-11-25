-- ============================================
-- 69-00 真理値表 ─ 論理式を打つと、構文木と真理値表を作って「恒真・充足可能・矛盾」を判定する
-- 式はまず木に読む（~ が一番強く、& ＞ | ＞ -> ＞ <-> の順。-> は右から：p->q->r = p->(q->r)）。
-- 変数への真偽の割り当てを全部（2ⁿ 通り）ためすのが真理値表。どの行でも真なら恒真、どの行でも偽なら矛盾。
-- 操作：文字キー = 入力（~ ¬、& ∧、| ∨、-> →、<-> ↔、1 か T = ⊤、0 か _|_ = ⊥）/ Backspace 1 字消す
--       Shift + Backspace全部消す / ↑↓ お手本の式 / ← → 表の行を選ぶ（行のクリックでも）→ 木の色がその行の真偽になる
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

presets :: [(String, String)]                 -- ★ お手本の式（自分の式を足してもよい）
presets =
  [ ("De Morgan's law", "~(p & q) <-> (~p | ~q)")
  , ("excluded middle", "p | ~p")
  , ("Peirce's law", "((p -> q) -> p) -> p")
  , ("contradiction", "p & ~p")
  , ("syllogism (transitivity)", "(p -> q) & (q -> r) -> (p -> r)")
  , ("contraposition", "(p -> q) <-> (~q -> ~p)")
  , ("is the converse always true?", "(p -> q) -> (q -> p)")
  , ("distributive law", "p & (q | r) <-> (p & q) | (p & r)")
  , ("majority (2 of 3 or more)", "(p & q) | (q & r) | (p & r)") ]

maxRowsShown :: Int
maxRowsShown = 32                             -- ★ 一度に見せる表の行の数

-- いま読めている式とその表
data Cur = Cur
  { cF    :: F
  , cVars :: [String]
  , cRows :: [(Asg, Bool)]
  , cKind :: (Kind, Int, Int)
  , cCols :: [F] }                            -- 表の列にする部分式

data World = World
  { wSrc  :: String                           -- 打った文字列
  , wP    :: !Int                             -- お手本の番号
  , wCur  :: Maybe Cur
  , wErr  :: String
  , wSel  :: !Int                             -- 選んでいる行
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-00 Truth Tables" (loadPreset 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPreset 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World "" 0 Nothing "" 0 []

loadPreset :: Int -> World -> World
loadPreset i w =
  let k = wrapI i (length presets)
  in rebuild w { wP = k, wSrc = snd (at' presets k ("", "")), wSel = 0 }

-- ★ 式を読み直す：まちがいなら説明を出して、前の正しい式のまま
rebuild :: World -> World
rebuild w = case parseFormula (wSrc w) of
  Left e -> w { wErr = errLine e }
  Right f ->
    let vars = fmlVars f
        rows = truthRowsV vars f
    in w { wErr = "", wCur = Just (Cur f vars rows (classifyFml f) (subColumns f)), wSel = clampI (wSel w) 0 (length rows - 1) }

-- 表の列にする部分式（内側から、同じものは 1 回）
subColumns :: F -> [F]
subColumns f = reverse (snd (walk f (S.empty, [])))
  where
    walk g acc =
      let acc' = foldl' (flip walk) acc (fKids g)
      in case g of
           FVar _ -> acc'
           _ -> let s = fmlStr g
                    (seen, out) = acc'
                in if S.member s seen then acc' else (S.insert s seen, g : out)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Truth tables: read a formula into a tree, try every assignment"
      "Type a formula (~ & | -> <-> T _|_, or 1 0)   Backspace delete   Shift+Backspace clear\nUp/Down examples   Left/Right choose a row (or click a row)"
  , drawInputLine (envFrame env) 30 120 "Formula:" (wSrc w) (wErr w) True ]
  ++ maybe [] (drawCur env w) (wCur w)
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

drawCur :: Env -> World -> Cur -> [Picture]
drawCur env w c =
  let wd = envW env
      ht = envH env
      asg = fst (at' (cRows c) (wSel w) (M.empty, False))
      lay = layoutTree (cF c) fKids 400
      tw = min (wd * 0.42) (60 + i2f (tlLeaves lay) * 52)
      tx = 30 + (wd * 0.42 - tw) / 2
      (kind, nTrue, nRows) = cKind c
      kc = case kind of
        Taut -> grn
        Contra -> pink
        Sat -> yel
      why = case kind of
        Taut -> "true in every row"
        Contra -> "false in every row -> its negation is a tautology"
        Sat -> "some rows true, some false"
      asgStr = unwords [ v ++ "=" ++ (if M.findWithDefault False v asg then "1" else "0") | v <- cVars c ]
  in [ say (clip 100 ("Example " ++ show (wP w + 1) ++ " / " ++ show (length presets) ++ ": " ++ fst (at' presets (wP w) ("", ""))
                      ++ "    read as: " ++ fmlStr (cF c))) 30 166 14 (col dimc)
     -- ---- 構文木（選んだ行の真偽で色分け：緑 = 真、桃 = 偽）
     , say ("Syntax tree (this row: " ++ asgStr ++ ")") 30 196 15 (col ink)
     , drawTree lay (tx + 20) 236 (tw - 40) (ht * 0.5 - 16) fmlTreeLabel (\g -> if evalFml g asg then grn else pink) 16
     -- ---- 判定
     , say (kindName kind) 30 (ht - 96) 26 (col kc)
     , say ("true rows " ++ show nTrue ++ " / " ++ show nRows ++ "  (" ++ why ++ ")") 30 (ht - 68) 15 (col ink)
     , say "The same table as an AND/OR/NOT circuit: series 56 / 06. Reading the tree = parsing" 30 (ht - 40) 14 (col dimc)
     , drawTable env w c ]

-- 表の形：列 (名前, 式, 変数か, 幅)、最初の行、見せる行の数、行の高さ
tableCols :: Float -> Cur -> [(String, F, Bool, Float)]
tableCols w c =
  let cols0 = [ (v, FVar v, True) | v <- cVars c ] ++ [ (fmlStr g, g, False) | g <- cCols c ]
      withW = [ (nm, g, iv, max 30 (textW (clip 26 nm) 14 + 14)) | (nm, g, iv) <- cols0 ]
      nv = length (cVars c)
      total = sum . map (\(_, _, _, cw) -> cw)
      shrink cs = if length cs > nv + 1 && total cs > w then shrink (removeAt nv cs) else cs   -- 入りきらない部分式の列は省く
  in shrink withW

tableGeom :: Env -> World -> Cur -> (Float, Float, Float, Float, Int, Int, Float)
tableGeom env w c =
  let x = envW env * 0.47
      y = 186
      tw = envW env * 0.51
      h = envH env - 246
      n = length (cRows c)
      first = (wSel w `div` maxRowsShown) * maxRowsShown
      shown = min maxRowsShown (n - first)
      rh = min 24 ((h - 30) / i2f (min n maxRowsShown))
  in (x, y, tw, h, first, shown, rh)

-- 行の当たり判定の箱 (行の番号, x, y, 幅, 高さ)
rowBoxes :: Env -> World -> Cur -> [(Int, Float, Float, Float, Float)]
rowBoxes env w c =
  let (x, y, tw, _, first, shown, rh) = tableGeom env w c
  in [ (first + i, x - 4, y + 24 + i2f i * rh, tw, rh) | i <- [0 .. shown - 1] ]

drawTable :: Env -> World -> Cur -> Picture
drawTable env w c =
  let (x, y, _, _, first, shown, rh) = tableGeom env w c
      cols = tableCols (envW env * 0.51) c
      n = length (cRows c)
      nv = length (cVars c)
      ncol = length cols
      totalW = sum [ cw | (_, _, _, cw) <- cols ]
      xs = scanl (+) x [ cw | (_, _, _, cw) <- cols ]
      sel = wSel w
      hi = if sel >= first && sel < first + shown
             then [Color (col linec) (rectTL (x - 4) (y + 24 + i2f (sel - first) * rh) (totalW + 8) (rh - 2))] else []
      ts = max 14 (min 15 (rh * 0.75))
      colPic j (nm, g, isVar, cw) cx =
        [ sayC (clip 26 nm) (cx + cw / 2) (y + 14) 14 (col (if isVar then sky else if j == ncol - 1 then yel else dimc)) ]
        ++ [ sayM (if v then "1" else "0") (cx + cw / 2) (ry + rh / 2 - 1) ts (col (if isVar then ink else if v then grn else pink))
           | i <- [0 .. shown - 1], let k = first + i, let ry = y + 24 + i2f i * rh
           , let v = evalFml g (fst (at' (cRows c) k (M.empty, False))) ]
        ++ [ Color (col linec) (lineW 1 (cx + cw, y) (cx + cw, y + 24 + i2f shown * rh)) | j == nv - 1 || j == ncol - 2 ]
      foot = if n > maxRowsShown
               then [say ("rows " ++ show (first + 1) ++ "-" ++ show (first + shown) ++ " / " ++ show n ++ " (Left/Right for more)") x (y + 42 + i2f shown * rh) 14 (col dimc)]
               else []
  in Pictures (hi ++ concat [ colPic j cl cx | (j, cl, cx) <- zip3 [0 :: Int ..] cols xs ] ++ foot)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> case wCur w of
    Just c -> let m = toP5 env pos
              in foldl' (\w1 (k, bx, by, bw, bh) -> if inBox m (bx, by, bw, bh) then w1 { wSel = k } else w1) w (rowBoxes env w c)
    Nothing -> w
  EventKey k Down mods _ -> case typedKey k mods of
    TUp -> loadPreset (wP w - 1) w
    TDown -> loadPreset (wP w + 1) w
    TLeft | Just c <- wCur w -> w { wSel = wrapI (wSel w - 1) (length (cRows c)) }
    TRight | Just c <- wCur w -> w { wSel = wrapI (wSel w + 1) (length (cRows c)) }
    TBack -> rebuild w { wSrc = dropLast (wSrc w) }
    TClear -> rebuild w { wSrc = "" }
    TChar ch -> rebuild w { wSrc = typeInto (wSrc w) ch fmlKeys 80 }
    _ -> rebuild w
  _ -> w

-- ---------- やってみよう ----------
-- 1. p -> q と ~p | q の真理値表をくらべよう。「同じ表」を <-> でつなぐと恒真になる
-- 2. 逆 (q -> p)・裏 (~p -> ~q)・対偶 (~q -> ~p) のうち、もとの p -> q と同じ表になるのはどれ？
-- 3. パースの法則 ((p->q)->p)->p は恒真。でも 09（カリー-ハワード）では「証明のプログラム」が作れない。どうして？
-- 4. 変数 3 つで、真になる行がちょうど 1 行の式を作ろう（ヒント：p & q & r）。ちょうど 4 行なら？
-- 5. 恒真の式は何個ある？ 変数 2 つの真理値表の「最後の列」は 2⁴ = 16 種類。そのうち恒真は 1 種類だけ

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな式 300 個：文字列にして読み直すと同じ木（⊤ ⊥ も含む）", rt == 300)
  , ("300 個：φ が恒真 ⇔ ¬φ が矛盾、φ が矛盾 ⇔ ¬φ が恒真", dual == 300)
  , ("ド・モルガンの法則 2 つは恒真", kind "¬(p ∧ q) ↔ (¬p ∨ ¬q)" == Just Taut && kind "¬(p ∨ q) ↔ (¬p ∧ ¬q)" == Just Taut)
  , ("排中律 p ∨ ¬p とパースの法則 ((p→q)→p)→p は恒真", kind "p ∨ ¬p" == Just Taut && kind "((p → q) → p) → p" == Just Taut)
  , ("p ∧ ¬p は矛盾、逆 (p→q)→(q→p) は恒真ではない", kind "p ∧ ¬p" == Just Contra && kind "(p → q) → (q → p)" == Just Sat)
  , ("p → q が真になるのは 4 行中 3 行（p = 1, q = 0 だけ偽）", case parseFormula "p -> q" of
        Right f -> let (_, n, _) = classifyFml f in n == 3 && not (evalFml f (M.fromList [("p", True), ("q", False)]))
        Left _ -> False)
  , ("ASCII の書き方（~ & | <->）と記号の書き方は同じ木", same "~(p&q)<->(~p|~q)" "¬(p∧q)↔(¬p∨¬q)")
  , ("強さの順：p|q&r = p∨(q∧r)、p->q->r = p→(q→r)、~p&q = (¬p)∧q、p&q->r = (p∧q)→r"
    , same "p|q&r" "p|(q&r)" && same "p->q->r" "p->(q->r)" && same "~p&q" "(~p)&q" && same "p&q->r" "(p&q)->r")
  , ("おかしな入力 " ++ show (length garbage) ++ " 個は例外を出さずに「まちがい」の説明を返す", all isErr garbage)
  , ("変数は " ++ show fmlMaxVars ++ " 個まで（7 個はまちがい、6 個は 64 行）"
    , isErr "a&b&c&d&e&f&g" && either (const False) (\f -> length (truthRows f) == 64) (parseFormula "a|b|c|d|e|f")) ]
  where
    fs = withSeed 11 (mapM (const (randomFml 4 ["p", "q", "r", "s"] True)) [1 .. 300 :: Int])
    rt = length [ () | f <- fs, parseFormula (fmlStr f) == Right f ]
    dual = length [ () | f <- fs, let a = kindOf f, let b = kindOf (FNot f), (a == Taut) == (b == Contra) && (a == Contra) == (b == Taut) ]
    kind s = either (const Nothing) (Just . kindOf) (parseFormula s)
    same s t = case (parseFormula s, parseFormula t) of
      (Right a, Right b) -> fmlEq a b
      _ -> False
    garbage = ["", "(", ")", "p q", "&p", "p->", "((p)", "-", "p<", "P", "123456ztr casnfg ", "p&&q", "[]p", "é", "p)("]
    isErr s = either (const True) (const False) (parseFormula s)

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

-- ---- 木の描き方：葉を左から順に並べ、親は子の真ん中
-- 返り値 TreeLay：ノード（前から順。x は 0〜1、深さ、親の番号）、深さの最大、葉の数
data TNode a = TNode { tnNode :: a, tnX :: !Float, tnDepth :: !Int, tnParent :: !Int }
data TreeLay a = TreeLay { tlNodes :: [TNode a], tlDepth :: !Int, tlLeaves :: !Int }

layoutTree :: a -> (a -> [a]) -> Int -> TreeLay a
layoutTree root kids maxNodes =
  let (nodes, _, leaves, _) = walk root 0 (-1) 0 0
      norm o = o { tnX = if leaves > 1 then tnX o / i2f (leaves - 1) else 0.5 }
      ns = map norm nodes
  in TreeLay ns (maxOr 0 (map tnDepth ns)) leaves
  where
    -- walk ノード 深さ 親 いままでの数 いままでの葉 → (このノード以下のリスト, 数, 葉, x)
    walk n d parent count lv
      | count >= maxNodes = ([], count, lv, Nothing)
      | otherwise =
          let me = count
              (subs, count', lv', xs) = foldl' step ([], count + 1, lv, []) (kids n)
              step (acc, c, l, xa) k = let (ns, c2, l2, mx) = walk k (d + 1) me c l
                                       in (acc ++ [ns], c2, l2, maybe xa (\x -> xa ++ [x]) mx)
              (x, lv2) = case xs of
                [] -> (i2f lv', lv' + 1)
                _ -> ((headOr 0 xs + lastOr 0 xs) / 2, lv')
          in (TNode n x d parent : concat subs, count', lv2, Just x)

-- 線と丸と文字。colOf ノード で色、r = 丸の半径（p5.js と同じ座標で描く）
drawTree :: TreeLay a -> Float -> Float -> Float -> Float -> (a -> String) -> (a -> RGB) -> Float -> Picture
drawTree lay x y w h lab colOf r =
  let dy = if tlDepth lay > 0 then min 60 (h / i2f (tlDepth lay)) else 0
      ns = tlNodes lay
      arr = M.fromList (zip [0 :: Int ..] ns)
      px o = x + tnX o * w
      py o = y + i2f (tnDepth o) * dy
      edges = [ lineW 1.5 (px p, py p) (px o, py o) | o <- ns, Just p <- [M.lookup (tnParent o) arr] ]
      ball o = let c = colOf (tnNode o)
               in Pictures [ Color (col bgc) (circleAt (px o) (py o) (r * 2)), Color (col c) (ringAt (px o) (py o) (r * 2) 1.5)
                           , sayM (lab (tnNode o)) (px o) (py o + 1) (max 14 (r * 0.95)) (col c) ]
  in Pictures (Color (col linec) (Pictures edges) : map ball ns)

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
