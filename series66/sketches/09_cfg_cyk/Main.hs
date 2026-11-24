-- ============================================
-- 66-09 文脈自由文法と CYK 構文解析 ─ 「式 → 式 + 項」のような書きかえ規則（文法）で、かっこの入った式の形を決める
-- チョムスキー標準形（右辺が「記号 2 つ」か「文字 1 つ」）に直した文法なら、短い部分から長い部分へ表を埋めていける：
-- 表のマス (i, 長さ) = 「i 文字目からその長さの部分を作れる記号」。一番上のマスに E があれば受理（CYK 法）。
-- あいまいな文法では、同じ式に構文木が 2 つ以上できる（n+n*n は「(n+n)*n」と「n+(n*n)」）
-- 操作：n + * ( ) 入力（数字も n）/ Backspace 消す / スペース 自動で埋める / → 1 マス / A 全部 / G 文法を変える / T 次の構文木 / ↑↓ お手本
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 文法：名前・元の規則・標準形の規則（A → B C）と（A → 文字）
data Grammar = Grammar { gName :: String, gOrig :: [String], gBin :: [(String, String, String)], gTerm :: [(String, Char)] }

grammars :: [Grammar]
grammars =
  [ Grammar "ordinary expression grammar (unambiguous)" ["E -> E + T | T", "T -> T * F | F", "F -> ( E ) | n"]
      [("E", "E", "X1"), ("E", "T", "X2"), ("E", "L", "X3"), ("X1", "P", "T"), ("T", "T", "X2"), ("T", "L", "X3"), ("X2", "M", "F"), ("F", "L", "X3"), ("X3", "E", "R")]
      [("E", 'n'), ("T", 'n'), ("F", 'n'), ("P", '+'), ("M", '*'), ("L", '('), ("R", ')')]
  , Grammar "ambiguous grammar" ["E -> E + E | E * E | ( E ) | n"]
      [("E", "E", "Y1"), ("E", "E", "Y2"), ("E", "L", "X3"), ("Y1", "P", "E"), ("Y2", "M", "E"), ("X3", "E", "R")]
      [("E", 'n'), ("P", '+'), ("M", '*'), ("L", '('), ("R", ')')] ]

grammar :: Int -> Grammar
grammar k = at' grammars k (Grammar "" [] [] [])

presets :: [String]
presets = ["n+n*n", "(n+n)*n", "n*(n+n*n)", "n+n+n+n", "n+*n", "((n))"]

-- 1 つのマス：記号 → 構文木の数（入った順を保つ）
type Cell = [(String, Integer)]

addCount :: String -> Integer -> Cell -> Cell
addCount a v cell = if any ((== a) . fst) cell then [ (k, if k == a then c + v else c) | (k, c) <- cell ] else cell ++ [(a, v)]

setCount :: String -> Integer -> Cell -> Cell
setCount a v cell = if any ((== a) . fst) cell then [ (k, if k == a then v else c) | (k, c) <- cell ] else cell ++ [(a, v)]

look :: M.Map (Int, Int) Cell -> Int -> Int -> String -> Integer
look t i l a = maybe 0 id (lookup a (M.findWithDefault [] (i, l) t))

-- ★ CYK：表の (i, l) = 「i 文字目から長さ l の部分」を各記号から作る構文木の数
cyk :: Grammar -> String -> M.Map (Int, Int) Cell
cyk g s = foldl' fill base [ (l, i, k) | l <- [2 .. n], i <- [0 .. n - l], k <- [1 .. l - 1] ]
  where
    n = length s
    base = M.fromList [ ((i, 1), foldl' (\cell (a, c) -> if c == ch then setCount a 1 cell else cell) [] (gTerm g)) | (i, ch) <- zip [0 ..] s ]
    -- 長さ l を 左 k + 右 l−k に分ける
    fill t (l, i, k) = foldl' (\t1 (a, b, c) -> let x = look t1 i k b
                                                    y = look t1 (i + k) (l - k) c
                                                in if x > 0 && y > 0 then M.insert (i, l) (addCount a (x * y) (M.findWithDefault [] (i, l) t1)) t1 else t1) t (gBin g)

data Tree = Tree String [Tree]

-- t 番目の構文木を取り出す
treeOf :: Grammar -> M.Map (Int, Int) Cell -> String -> String -> Int -> Int -> Integer -> Tree
treeOf g t s a i l n
  | l == 1 = Tree a [Tree [at' s i '?'] []]
  | otherwise = go [ (k, b, c) | k <- [1 .. l - 1], (a2, b, c) <- gBin g, a2 == a ] n
  where
    go [] _ = Tree a []
    go ((k, b, c) : rest) tt =
      let x = look t i k b
          y = look t (i + k) (l - k) c
      in if tt < x * y then Tree a [treeOf g t s b i k (tt `div` y), treeOf g t s c (i + k) (l - k) (tt `mod` y)] else go rest (tt - x * y)

data World = World
  { wG      :: !Int
  , wWord   :: String
  , wPre    :: !Int
  , wTable  :: M.Map (Int, Int) Cell
  , wFilled :: !Int                           -- 埋めたマスの数
  , wAuto   :: !Bool
  , wTree   :: !Int
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-09 CFG and CYK parsing" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = rerun (World 0 (at' presets 0 "") 0 M.empty 0 True 0 [])

cells :: String -> [(Int, Int)]                -- 埋める順（短い順、左から）
cells s = [ (i, l) | l <- [1 .. length s], i <- [0 .. length s - l] ]

rerun :: World -> World
rerun w = w { wTable = cyk (grammar (wG w)) (wWord w), wFilled = if wAuto w then 0 else length (cells (wWord w)), wTree = 0 }

step :: Env -> World -> World
step env w
  | wAuto w && envFrame env `mod` 10 == 0 && wFilled w < length (cells (wWord w)) = w { wFilled = wFilled w + 1 }
  | otherwise = w

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("CFG and CYK parsing: " ++ gName g) "n + * ( ) type   Backspace   Space auto fill   Right 1 cell   A all   G grammar   T next tree   Up/Down example"
  -- ---- 文法
  , say "Grammar" 30 100 16 (col ink)
  , Pictures [ say r 30 (126 + i2f i * 24) 16 (col amber) | (i, r) <- zip [0 :: Int ..] (gOrig g) ]
  , say "In Chomsky normal form:" 30 210 14 (col dimc)
  , Pictures [ say r (30 + (if i >= 9 then 120 else 0)) (234 + i2f (i `mod` 9) * 20) 14 (col dimc)
             | (i, r) <- zip [0 :: Int ..] ([ a ++ " -> " ++ b ++ " " ++ c | (a, b, c) <- gBin g ] ++ [ a ++ " -> " ++ [c] | (a, c) <- gTerm g ]) ]
  -- ---- 入力
  , Pictures [ Pictures [ Color (col panel) (rectTL (300 + i2f i * 54) 92 50 36), Color (col wire) (frameTL 1 (300 + i2f i * 54) 92 50 36)
                        , sayM [c] (325 + i2f i * 54) 110 20 (col ink) ] | (i, c) <- zip [0 :: Int ..] wd ] ]
  ++ (if n == 0 then [] else table ++ result)
  ++ [ drawSelfTest env (wTest w) ]
  where
    g = grammar (wG w)
    wd = wWord w
    n = length wd
    cs = cells wd
    cw = 54
    ch = min 52 ((envH env - 260) / i2f (max 1 n))
    bx = 300
    by = 150 + i2f n * ch
    -- ---- CYK の表（下の段 = 長さ 1、上へ行くほど長い）
    table =
      [ Pictures $ [ Color (col (if null m then panel else (28, 60, 45))) (rectTL x y (cw - 4) (ch - 4))
                   , Color (col (if lastC then amber else wire)) (frameTL (if lastC then 3 else 1) x y (cw - 4) (ch - 4)) ]
                   ++ [ sayM ln (x + cw / 2 - 2) (y + ch / 2 - 2 + (i2f j - (i2f (length lns) - 1) / 2) * 14) 14 (col (if lookup "E" m /= Nothing && l == n then lime else ink))
                      | (j, ln) <- zip [0 :: Int ..] lns ]
      | (c, (i, l)) <- zip [0 :: Int ..] (take (wFilled w) cs)
      , let x = bx + i2f i * cw + i2f (l - 1) * cw / 2
      , let y = by - i2f l * ch
      , let m = M.findWithDefault [] (i, l) (wTable w)
      , let lastC = c == wFilled w - 1 && wFilled w < length cs
      , let labs = [ k ++ (if v > 1 then "x" ++ show v else "") | (k, v) <- m ]
      , let lns = map unwords (chunks 2 labs) ]
      ++ [ say t bx (by + 20 + i2f i * 18) 14 (col dimc)
         | (i, t) <- zip [0 :: Int ..] ["cell (i, length) = symbols that", "can make it (xN = number of trees)", "length l: try every split k + (l-k)"] ]
    -- ---- 結果と構文木
    done = wFilled w >= length cs
    top = look (wTable w) 0 n "E"
    px = max (bx + i2f n * cw + 30) (envW env * 0.55)
    result =
      [ say (if done then (if top > 0 then "accept: " ++ show top ++ " parse tree" ++ (if top > 1 then "s (ambiguous! T next)" else "") else "reject: no E in the top cell") else "filling the table...")
            px 110 20 (col (if done then (if top > 0 then lime else rose) else dimc)) ]
      ++ (if done && top > 0
            then let tr = treeOf g (wTable w) wd "E" 0 n (fromIntegral (wTree w) `mod` top)
                 in [ say ("tree " ++ show (fromIntegral (wTree w) `mod` top + 1) ++ " / " ++ show top ++ ": " ++ bracketed tr False) px 140 14 (col dimc)
                    , drawTreeAt tr px 170 (envW env - px - 30) (envH env - 240) ]
            else [])

chunks :: Int -> [a] -> [[a]]
chunks k xs = if null xs then [] else take k xs : chunks k (drop k xs)

-- 構文木 → 計算のまとまりを [ ] で示した式（一番外は省く）
bracketed :: Tree -> Bool -> String
bracketed (Tree sym kids) inner
  | null kids = sym
  | otherwise = let s = concatMap (\k -> bracketed k True) kids
                in if inner && (sym == "E" || sym == "T") && length kids == 2 && take 1 s /= "(" then "[" ++ s ++ "]" else s

drawTreeAt :: Tree -> Float -> Float -> Float -> Float -> Picture
drawTreeAt t x y w h = fst (rec t x 0)
  where
    leaves (Tree _ ks) = if null ks then 1 else sum (map leaves ks)
    depth (Tree _ ks) = if null ks then 1 else 1 + maximum (map depth ks)
    lw = w / i2f (leaves t)
    dy = min 50 (h / i2f (depth t))
    rec u@(Tree sym ks) x0 lvl =
      let cx = x0 + i2f (leaves u) * lw / 2
          cy = y + i2f lvl * dy + 14
          offs = scanl (+) x0 [ i2f (leaves k) * lw | k <- ks ]
          kids = [ Pictures [ Color (col wire) (lineW 1.5 (cx, cy) (off + i2f (leaves k) * lw / 2, y + i2f (lvl + 1) * dy + 14)), fst (rec k off (lvl + 1)) ]   -- 線を先に、丸をあとに
                 | (k, off) <- zip ks offs ]
          node = Pictures [ Color (col (if null ks then ink else if sym == "E" then amber else cyan)) (circleAt cx cy 26), sayM sym cx (cy + 1) 14 (col (10, 20, 15)) ]
      in (Pictures (kids ++ [node]), ())

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down _ _ -> case typedKey k of
    TChar c0 | let c = if c0 >= '0' && c0 <= '9' then 'n' else c0, c `elem` "n+*()" -> rerun w { wWord = typeInto (wWord w) (if c0 >= '0' && c0 <= '9' then 'n' else c0) "n+*()" 12 }
    TBack -> rerun w { wWord = dropLast (wWord w) }
    _ -> case k of
      SpecialKey KeySpace -> let a = not (wAuto w) in w { wAuto = a, wFilled = if a && wFilled w >= length (cells (wWord w)) then 0 else wFilled w }
      SpecialKey KeyRight -> w { wAuto = False, wFilled = min (length (cells (wWord w))) (wFilled w + 1) }
      SpecialKey KeyUp -> preset (length presets - 1)
      SpecialKey KeyDown -> preset 1
      Char c | isKey 'a' c -> w { wFilled = length (cells (wWord w)) }
             | isKey 'g' c -> rerun w { wG = (wG w + 1) `mod` length grammars }
             | isKey 't' c -> w { wTree = wTree w + 1 }
      _ -> w
  _ -> w
  where preset d = let p = (wPre w + d) `mod` length presets in rerun w { wPre = p, wWord = at' presets p "" }

-- ---------- やってみよう ----------
-- 1. G であいまいな文法にして n+n*n。2 つの木の「どちらが先に計算されるか」をくらべよう。ふつうの文法ではなぜ 1 つ？
-- 2. n+n+n+n+n の木はいくつ？ 項を増やすと 1, 2, 5, 14, 42 …（カタラン数）
-- 3. 引き算 - を足すには？ 元の文法に E -> E - T を足し、標準形の規則を 2 つ足そう（P2 -> - と X4 -> P2 T）
-- 4. 文法の書きかえで図形を作るのが L システム（シリーズ 2 の 03）。こちらは「書きかえで文字列の形を判定する」側
-- 5. CYK の手間は 文字数³ × 規則の数。長い式で表を埋める時間がどう増えるか、数えてみよう

-- ---- 自己テスト
-- 総当たり：一番左の記号から書きかえ続けて、長さ maxLen までの文字列を全部作る
derivedStrings :: Grammar -> Int -> S.Set String
derivedStrings g maxLen = go [["E"]] (S.singleton ["E"]) S.empty
  where
    nts = S.fromList (map (\(a, _, _) -> a) (gBin g) ++ map fst (gTerm g))
    go [] _ out = out
    go (f : todo) seen out = case break (`S.member` nts) f of
      (_, []) -> go todo seen (S.insert (concat f) out)
      (pre, x : post) ->
        let nfs = [ pre ++ [b, c] ++ post | (a, b, c) <- gBin g, a == x, length f + 1 <= maxLen ]
                  ++ [ pre ++ [[c]] ++ post | (a, c) <- gTerm g, a == x ]
            new = filter (\nf -> not (S.member nf seen)) (nubOrd nfs)
        in go (new ++ todo) (foldl' (flip S.insert) seen new) out

-- 長さ maxLen までの全部の文字列（5 文字の組み合わせ）を CYK で。1 文字足すごとに表の右の列だけ計算（記号の集合 = ビット）
cykAcceptsAll :: Grammar -> Int -> [String]
cykAcceptsAll g maxLen = dfs [] [] 0
  where
    nts = nubOrd (map (\(a, _, _) -> a) (gBin g) ++ map fst (gTerm g))
    bit x = maybe 0 (\i -> 1 `shiftL` i) (lookup x (zip nts [0 :: Int ..])) :: Int
    eBit = bit "E"
    rules = [ (bit a, bit b, bit c) | (a, b, c) <- gBin g ]
    al = "n+*()"
    tm = [ foldl' (.|.) 0 [ bit a | (a, c') <- gTerm g, c' == c ] | c <- al ]
    -- cols !! k = 列 (j − 1 − k)…（新しい列が先頭）。列 j の i 番目 = T[i][j]
    dfs cols sRev j = concat
      [ let col0 = newCol cols j t
            cols' = col0 : cols
            s' = c : sRev
            here = [ reverse s' | (col0 U.! 0) .&. eBit /= 0 ]
        in here ++ (if j + 1 < maxLen then dfs cols' s' (j + 1) else [])
      | (c, t) <- zip al tm ]
    -- 列 j：T[j][j] = 文字の記号、T[i][j] = 左 T[i][k] と右 T[k+1][j] の組み合わせ
    newCol cols j t =
      let colAt k i = let cv = at' cols (j - 1 - k) U.empty in if i < U.length cv then cv U.! i else 0
          build i right   -- right = T[i+1..j][j]（下から順）
            | i < 0 = right
            | otherwise =
                let m = foldl' (.|.) 0 [ a | k <- [i .. j - 1], let l = colAt k i, let r = at' right (k - i) 0, l /= 0, r /= 0
                                           , (a, b, c) <- rules, l .&. b /= 0, r .&. c /= 0 ]
                in build (i - 1) (m : right)
      in U.fromList (build (j - 1) [t])

selfTest :: [(String, Bool)]
selfTest =
  [ ("ふつうの文法：長さ 7 までの全部の文字列（97655 個）で、CYK が受理 = 書きかえの総当たりで作れる（" ++ show (S.size d1) ++ " 個）", length a1 == S.size d1 && all (`S.member` d1) a1)
  , ("あいまいな文法でも同じ（" ++ show (S.size d2) ++ " 個）、しかも 2 つの文法は同じ言語", length a2 == S.size d2 && all (`S.member` d2) a2 && d1 == d2)
  , ("ふつうの文法では受理される式の構文木はいつも 1 個", all (\s -> look (cyk g0 s) 0 (length s) "E" == 1) (S.toList d1))
  , ("あいまいな文法の構文木の数：n+n*n → " ++ show c1 ++ "、n+n+n+n → " ++ show c2 ++ "、5 項 → " ++ show c3 ++ "（カタラン数 2, 5, 14）", c1 == 2 && c2 == 5 && c3 == 14)
  , ("n+*n、(n、n) は不受理", all (\s -> look (cyk g0 s) 0 (length s) "E" == 0) ["n+*n", "(n", "n)"]) ]
  where
    g0 = grammar 0
    g1 = grammar 1
    d1 = derivedStrings g0 7
    a1 = cykAcceptsAll g0 7
    d2 = derivedStrings g1 7
    a2 = cykAcceptsAll g1 7
    cnt s = look (cyk g1 s) 0 (length s) "E"
    c1 = cnt "n+n*n"
    c2 = cnt "n+n+n+n"
    c3 = cnt "n+n+n+n+n"

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
