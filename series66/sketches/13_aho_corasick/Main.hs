-- ============================================
-- 66-13 エイホ-コラシック法 ─ たくさんの単語を、テキストを 1 回読むだけで全部さがすオートマトン
-- 単語をトライ（先頭が同じなら枝を共有する木）にして、各ノードに「失敗リンク」（いまの文字列の、一番長い“後ろの一部”のノード）をつける。
-- 次の文字の枝がなければ失敗リンクをたどって戻り、そこから続ける。ノードにつく出力（その位置で終わる単語）を全部報告。
-- 1 つの単語の照合（シリーズ 17 の 02）を、たくさんの単語に広げたもの
-- 操作：Tab = 見る → 単語の編集 → テキストの編集 → 見る（編集中は英小文字と空白が入力、Backspace 消す、Enter 見るモードへ）/
--       見るモード：← → 1 文字 / スペース 自動再生 / ↑↓ お手本
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', isPrefixOf, sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

presets :: [([String], String)]               -- ★ お手本（単語, テキスト）
presets =
  [ (["he", "she", "his", "hers"], "ushers say his shell is here")
  , (["a", "ab", "bab", "bc", "bca", "c", "caa"], "abccab bcaab")
  , (["cat", "cattle", "at", "tt", "le"], "the cattle eat at the table") ]

lowerAndSpace :: String
lowerAndSpace = "abcdefghijklmnopqrstuvwxyz "

-- トライのノード：子（入れた順）・失敗リンク・出力・自分で終わる単語・深さ・親・文字
data Node = Node { nNext :: [(Char, Int)], nFail :: !Int, nOut :: [Int], nOwn :: [Int], nDepth :: !Int, nParent :: !Int, nCh :: Char }

data AC = AC { acNodes :: IM.IntMap Node, acOrder :: [Int] }

nodeOf :: AC -> Int -> Node
nodeOf m v = IM.findWithDefault (Node [] 0 [] [] 0 (-1) ' ') v (acNodes m)

child :: AC -> Int -> Char -> Maybe Int
child m v c = lookup c (nNext (nodeOf m v))

-- ★ トライを作り、幅優先で失敗リンクと出力をつける
buildAC :: [String] -> AC
buildAC ws =
  let root = IM.singleton 0 (Node [] 0 [] [] 0 (-1) ' ')
      addWord nodes (wi, w) =
        let go ns v [] = (ns, v)
            go ns v (c : cs) =
              let nv = IM.findWithDefault (Node [] 0 [] [] 0 (-1) ' ') v ns
              in case lookup c (nNext nv) of
                   Just u -> go ns u cs
                   Nothing -> let u = IM.size ns
                                  ns' = IM.insert u (Node [] 0 [] [] (nDepth nv + 1) v c) (IM.insert v nv { nNext = nNext nv ++ [(c, u)] } ns)
                              in go ns' u cs
            (ns1, v1) = go nodes 0 w
        in if null w then ns1 else IM.adjust (\n -> n { nOwn = nOwn n ++ [wi] }) v1 ns1
      trie = foldl' addWord root (zip [0 ..] ws)
      bfs h queue m
        | h >= length queue = AC m queue
        | otherwise =
            let v = at' queue h 0
                nv = IM.findWithDefault (Node [] 0 [] [] 0 (-1) ' ') v m
                failOut = if v /= 0 then nOut (IM.findWithDefault nv (nFail nv) m) else []
                m1 = IM.insert v nv { nOut = nOwn nv ++ failOut } m       -- 失敗リンク先の出力も引きつぐ
                setFail mm (c, u) =
                  let f0 = nFail nv
                      lk f = lookup c (nNext (IM.findWithDefault nv f mm))
                      climb f = if f /= 0 && lk f == Nothing then climb (nFail (IM.findWithDefault nv f mm)) else f
                      fu = if v == 0 then 0 else maybe 0 id (lk (climb f0))
                  in IM.adjust (\n -> n { nFail = fu }) u mm
                m2 = foldl' setFail m1 (nNext nv)
            in bfs (h + 1) (queue ++ map snd (nNext nv)) m2
  in bfs 0 [0] trie

data Step = Step { sV :: !Int, sHops :: [Int], sFound :: [Int] }
data Run = Run { rHits :: [(Int, Int)], rTrace :: [Step], rSteps :: !Int }

-- ★ テキストを 1 文字ずつ読む。steps = たどった枝 + 失敗リンクの数
runAC :: AC -> [String] -> String -> Run
runAC m ws s =
  let go (!v, hits, trace, !steps) (i, c) =
        let climb u hs st = if u /= 0 && child m u c == Nothing then climb (nFail (nodeOf m u)) (hs ++ [u]) (st + 1) else (u, hs, st)
            (u1, hops, st1) = climb v [] steps
            v2 = maybe 0 id (child m u1 c)
            out = nOut (nodeOf m v2)
            newHits = [ (i - length (at' ws wi "") + 1, wi) | wi <- out ]
        in (v2, hits ++ newHits, Step v2 hops out : trace, st1 + 1)
      (_, hs, tr, st) = foldl' go (0, [], [], 0) (zip [0 ..] s)
  in Run hs (reverse tr) st

wordOf :: AC -> Int -> String
wordOf m v = if v > 0 then wordOf m (nParent n) ++ [nCh n] else ""
  where n = nodeOf m v

data Mode = See | EditPat | EditTxt deriving (Eq)

data World = World
  { wPats    :: [String]
  , wTxt     :: String
  , wPatBuf  :: String
  , wAC      :: AC
  , wRun     :: Run
  , wPos     :: [Pos]                         -- トライの図の位置
  , wTpos    :: !Int
  , wPlaying :: !Bool
  , wMode    :: !Mode
  , wPIdx    :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-13 Aho-Corasick" world0)
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (loadPreset env 0 world0) { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World [] "" "" (buildAC []) (Run [] [] 0) [] 0 True See 0 []

loadPreset :: Env -> Int -> World -> World
loadPreset env k w =
  let i = k `mod` length presets
      (ps, t) = at' presets i ([], "")
  in rebuild env w { wPIdx = i, wPats = ps, wTxt = t, wPatBuf = unwords ps }

rebuild :: Env -> World -> World
rebuild env w =
  let pats = take 10 (words (wPatBuf w))
      ac = buildAC pats
      run = runAC ac pats (wTxt w)
      -- トライの図：葉の数で横に、深さで縦に並べる
      leaves v = let ks = map snd (nNext (nodeOf ac v)) in if null ks then 1 else sum (map leaves ks) :: Int
      maxD = max 1 (maxOr 0 (map nDepth (IM.elems (acNodes ac))))
      gx = 40
      gw = envW env * 0.62
      gy = 250
      gh = envH env - 420
      total = leaves 0
      place v x0 = let l = leaves v
                       me = (v, Pos (gx + (i2f x0 + i2f l / 2) / i2f total * gw) (gy + i2f (nDepth (nodeOf ac v)) / i2f maxD * gh) (Just (pi / 2)))
                       kids = sortOn fst (nNext (nodeOf ac v))
                       offs = scanl (+) x0 (map (leaves . snd) kids)
                   in me : concat (zipWith (\(_, u) o -> place u o) kids offs)
      posM = IM.fromList (place 0 0)
  in w { wPats = pats, wAC = ac, wRun = run, wTpos = min (wTpos w) (length (wTxt w))
       , wPos = [ IM.findWithDefault (Pos 0 0 Nothing) v posM | v <- [0 .. IM.size (acNodes ac) - 1] ] }

step :: Env -> World -> World
step env w
  | wPlaying w && wMode w == See && envFrame env `mod` 24 == 0 =
      if wTpos w < length (wTxt w) then w { wTpos = wTpos w + 1 } else w { wPlaying = False }
  | otherwise = w

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle "Aho-Corasick: find many words at once"
             (case wMode w of
                See -> "View mode: Left/Right 1 char   Space auto play   Up/Down example (" ++ show (wPIdx w + 1) ++ ")   Tab edit the words"
                m -> "Editing the " ++ (if m == EditPat then "words" else "text") ++ ": type a-z and space   Backspace delete   Tab next   Enter view mode")
  -- ---- 単語とテキスト
  , Pictures [ Pictures [ Color (col panel) (rectTL 110 y (envW env - 150) 30)
                        , Color (col (if on then amber else wire)) (frameTL (if on then 2 else 1) 110 y (envW env - 150) 30)
                        , sayLM lab 30 (y + 15) 15 (col (if on then amber else dimc)) ]
             | (m, lab, y) <- [(EditPat, "Words", 84), (EditTxt, "Text", 122)], let on = wMode w == m ]
  , sayLM (wPatBuf w ++ (if wMode w == EditPat && envFrame env `mod` 60 < 30 then "|" else "")) 122 100 17 (col ink)
  -- テキスト（読んだところまで・一致を色で）
  , Pictures [ Pictures [ if inHit i then Color (colA 90 lime) (rectTL x 124 cw 26) else Blank
                        , if i == tpos - 1 then Color (colA 150 amber) (rectTL x 124 cw 26) else Blank
                        , sayM [c] (x + cw / 2) 137 (max 14 (min 17 (cw * 0.9))) (col (if i < tpos then ink else dimc)) ]
             | (i, c) <- zip [0 ..] txt, let x = 122 + i2f i * cw ]
  , if wMode w == EditTxt && envFrame env `mod` 60 < 30 then Color (col amber) (rectTL (122 + i2f (length txt) * cw) 126 2 22) else Blank
  -- ---- トライと失敗リンク
  , say ("Trie: " ++ show nNodes ++ " nodes (double circle = a word ends there). Purple dashed = failure link") 40 212 14 (col dimc)
  , say "(links back to the root are not drawn). Red = nodes just left through a failure link." 40 232 14 (col dimc)
  , Pictures [ arrowLine (bezPts (pX a) (pY a - 16) mx my (pX b + 10) (pY b + 14) 20) (if v `elem` sHops cur then rose else (150, 110, 190)) 1.5 True
             | v <- [1 .. nNodes - 1], let f = nFail (nodeOf ac v), f /= 0
             , let a = at' (wPos w) v (Pos 0 0 Nothing), let b = at' (wPos w) f (Pos 0 0 Nothing)
             , let mx = (pX a + pX b) / 2 + 20, let my = (pY a + pY b) / 2 - 40 ]
  , drawMachine defOpt { oR = 16, oHi = hi, oNames = Just [ if v == 0 then "^" else [nCh (nodeOf ac v)] | v <- [0 .. nNodes - 1] ]
                       , oNameSize = Just 14, oLabels = False, oStart = False, oEdgeCol = (90, 130, 110) }
                nNodes 0 [ not (null (nOut (nodeOf ac v))) | v <- [0 .. nNodes - 1] ]
                (edgesFrom [ [ ([c], [u]) | (c, u) <- nNext (nodeOf ac v) ] | v <- [0 .. nNodes - 1] ]) (wPos w)
  -- ---- 右：いまの様子と見つかった単語
  , say ("Current node: \"" ++ wordOf ac (sV cur) ++ "\"") px py 16 (col ink)
  , if null (sHops cur) then Blank else say ("Failure links: " ++ joinWith " -> " [ "\"" ++ wordOf ac h ++ "\"" | h <- sHops cur ]) px (py + 26) 14 (col dimc)
  , say ("Found here: " ++ (if null (sFound cur) then "none" else joinWith ", " [ at' pats wi "" | wi <- sFound cur ])) px (py + 52) 14 (col lime)
  , say ("Words found " ++ show (length shownHits) ++ " / " ++ show (length (rHits run))) px (py + 92) 15 (col ink)
  , Pictures [ say (at' pats wi "" ++ " (from char " ++ show (i + 1) ++ ")") px (py + 118 + i2f k * 22) 14 (col dimc)
             | (k, (i, wi)) <- zip [0 :: Int ..] (drop (length shownHits - 14) shownHits) ]
  , say ("Edges + failure links followed " ++ show (rSteps run) ++ " <= 2 x length " ++ show (2 * length txt)) px (envH env - 60) 14 (col dimc)
  , drawSelfTest env (wTest w) ]
  where
    ac = wAC w
    run = wRun w
    pats = wPats w
    txt = wTxt w
    tpos = wTpos w
    nNodes = IM.size (acNodes ac)
    cw = min 22 ((envW env - 170) / i2f (max 1 (length txt)))
    lenOf wi = length (at' pats wi "")
    shownHits = [ h | h@(i, wi) <- rHits run, i + lenOf wi - 1 < tpos ]
    inHit i = any (\(s, wi) -> i >= s && i < s + lenOf wi) shownHits
    cur = if tpos > 0 then at' (rTrace run) (tpos - 1) (Step 0 [] []) else Step 0 [] []
    hi = IM.insert (sV cur) amber (IM.fromList [ (h, rose) | h <- sHops cur ])
    px = envW env * 0.68
    py = 250

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey k Down _ _ -> case (wMode w, typedKey k) of
    (m, TTab) -> w { wMode = case m of { See -> EditPat; EditPat -> EditTxt; EditTxt -> See } }
    (See, _) -> case k of
      SpecialKey KeyRight -> w { wPlaying = False, wTpos = min (length (wTxt w)) (wTpos w + 1) }
      SpecialKey KeyLeft -> w { wPlaying = False, wTpos = max 0 (wTpos w - 1) }
      SpecialKey KeySpace -> (if wTpos w >= length (wTxt w) then w { wTpos = 0 } else w) { wPlaying = not (wPlaying w) }
      SpecialKey KeyUp -> (loadPreset env (wPIdx w - 1 + length presets) w) { wTpos = 0 }
      SpecialKey KeyDown -> (loadPreset env (wPIdx w + 1) w) { wTpos = 0 }
      _ -> w
    (m, tk) ->
      let w1 = case tk of
            TBack -> if m == EditPat then w { wPatBuf = dropLast (wPatBuf w) } else w { wTxt = dropLast (wTxt w) }
            TEnter -> w { wMode = See }
            TChar c | m == EditPat -> w { wPatBuf = typeInto (wPatBuf w) c lowerAndSpace 60 }
                    | otherwise -> w { wTxt = typeInto (wTxt w) c lowerAndSpace 50 }
            _ -> w
      in rebuild env w1 { wTpos = min (wTpos w1) (length (wTxt w1)) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ushers を 1 文字ずつ進めよう。「she」まで読んだとき、出力に he も入っているのはなぜ？（失敗リンク先の出力を引きつぐ）
-- 2. 2 番目のお手本で、失敗リンクを何回たどる文字があるか見つけよう
-- 3. 単語を 1 つだけにすると、シリーズ 17 の 02 の「文字列照合のオートマトン」と同じ動きになる
-- 4. 単語の数を 10 個に増やしても、テキストを読む手間は増えない（増えるのはトライを作る手間と出力の数）
-- 5. 禁止語のフィルター、ウイルスの検出、DNA のパターンさがしでも同じしくみが使われる

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな単語の組 300 回：見つかった (位置, 単語) = 全部の位置で全部の単語を比べる素朴な方法", all (\(a, _, _) -> a) results)
  , ("失敗リンク = その文字列の一番長い真の後ろの一部（トライにあるもの）を総当たりで確かめた", all (\(_, b, _) -> b) results)
  , ("たどった回数（枝 + 失敗リンク）はいつも 2 × テキストの長さ以下", all (\(_, _, c) -> c) results)
  , ("有名な例：he, she, his, hers を ushers でさがすと she, he, hers の 3 個", sort [ at' ws wi "" | (_, wi) <- rHits r ] == ["he", "hers", "she"])
  , ("トライのノード数：he, she, his, hers → 10（根を含む）", IM.size (acNodes (buildAC ws)) == 10) ]
  where
    ws = ["he", "she", "his", "hers"]
    r = runAC (buildAC ws) ws "ushers"
    cases = withSeed 13 (mapM (const oneCase) [1 .. 300 :: Int])
    oneCase = do
      j <- rndInt 1 6
      wsR <- mapM (const (rndInt 1 4 >>= rndString "abc")) [1 .. j]
      s <- rndString "abc" 40
      pure (wsR, s)
    results = map check1 cases
    check1 (wsR, s) =
      let m = buildAC wsR
          rr = runAC m wsR s
          naive = [ (i, wi) | i <- [0 .. length s - 1], (wi, w) <- zip [0 ..] wsR, w `isPrefixOf` drop i s ]
          -- 失敗リンク = 一番長い「真の後ろの一部」で、トライにあるもの
          want v = let w = wordOf m v
                       walk suf = foldl' (\u c -> if u >= 0 then maybe (-1) id (child m u c) else -1) 0 suf
                   in headOr 0 [ u | l <- [length w - 1, length w - 2 .. 1], let u = walk (drop (length w - l) w), u >= 0 ]
          okF = and [ nFail (nodeOf m v) == want v | v <- [1 .. IM.size (acNodes m) - 1] ]
      in (sort (rHits rr) == sort naive, okF, rSteps rr <= 2 * length s)

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

-- ---- 状態遷移図を描く道具（円 = 状態、二重丸 = 受理、矢印 = 遷移。曲線は折れ線で描く）
data Pos = Pos { pX :: !Float, pY :: !Float, pLoop :: Maybe Float }   -- 位置と自己ループの向き

-- 辺 = 出る状態・入る状態・ラベル・記号のリスト
data Edge = Edge { eFrom :: !Int, eTo :: !Int, eLabel :: String, eSyms :: [String] }

epsLabel :: String                            -- ε の矢印のラベル（画面は ASCII だけなので "eps"）
epsLabel = "eps"

-- 2 次ベジェ曲線 → 折れ線の点
bezPts :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [(Float, Float)]
bezPts x0 y0 cx cy x1 y1 n =
  [ (u * u * x0 + 2 * u * t * cx + t * t * x1, u * u * y0 + 2 * u * t * cy + t * t * y1) | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t ]

-- 3 次ベジェ曲線 → 折れ線の点（自己ループ用）
cubicPts :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Int -> [(Float, Float)]
cubicPts (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) n =
  [ (a * p0x + b * p1x + c * p2x + d * p3x, a * p0y + b * p1y + c * p2y + d * p3y)
  | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t, let a = u * u * u, let b = 3 * u * u * t, let c = 3 * u * t * t, let d = t * t * t ]

-- 折れ線 + 先の三角（dashed = 点線：1 つおきの区間だけ描く）
arrowLine :: [(Float, Float)] -> RGB -> Float -> Bool -> Picture
arrowLine pts c w dashed = case reverse pts of
  ((x1, y1) : (x0, y0) : _) ->
    let l0 = hyp (x1 - x0) (y1 - y0)
        l = if l0 == 0 then 1 else l0
        ux = (x1 - x0) / l
        uy = (y1 - y0) / l
        s = 7 + w * 1.5
        body = if dashed then Pictures [ lineW w p q | (k, p, q) <- zip3 [0 :: Int ..] pts (drop 1 pts), even k ] else polyline w pts
        tip = Polygon [(x1, y1), (x1 - ux * s - uy * s * 0.5, y1 - uy * s + ux * s * 0.5), (x1 - ux * s + uy * s * 0.5, y1 - uy * s - ux * s * 0.5)]
    in Color (col c) (Pictures [body, tip])
  _ -> Blank

-- 状態ごとの「ラベル → 行き先のリスト」から辺のリスト（同じ向きの辺はラベルをまとめる。p5.js 版の edgeList）
edgesFrom :: [[(String, [Int])]] -> [Edge]
edgesFrom rows =
  let n = length rows
      items = [ ((q, t), lab) | (q, row) <- zip [0 ..] rows, (lab, ts) <- row, t <- ts, t >= 0, t < n ]
      keys = nubOrd (map fst items)
      symsOf k = [ lab | (k', lab) <- items, k' == k ]
      sortSyms = sortOn (\s -> (s == epsLabel, s))
  in [ Edge q t (joinWith "," (sortSyms (symsOf (q, t)))) (symsOf (q, t)) | (q, t) <- keys ]

symLabel :: Sym -> String
symLabel c = if c == epsSym then epsLabel else [c]

dfaEdges :: DFA -> [Edge]
dfaEdges d = edgesFrom [ [ (symLabel c, [t]) | (c, t) <- M.toList row ] | row <- take (dN d) (dTr d ++ repeat M.empty) ]

nfaEdges :: NFA -> [Edge]
nfaEdges m = edgesFrom [ [ (symLabel c, ts) | (c, ts) <- M.toList row ] | row <- take (nN m) (nTr m ++ repeat M.empty) ]

-- 1 本の辺の折れ線の点とラベルの位置
edgeGeometry :: Edge -> Pos -> Pos -> Float -> Bool -> ([(Float, Float)], (Float, Float))
edgeGeometry e a b r both
  | eFrom e == eTo e =                         -- 自己ループ：中心から外向き（または上）に輪
      let ang = maybe (-pi / 2) id (pLoop a)
          ux = cos ang
          uy = sin ang
          px = -uy
          py = ux
          s0 = (pX a + (ux * 0.6 + px * 0.8) * r, pY a + (uy * 0.6 + py * 0.8) * r)
          s1 = (pX a + (ux * 0.6 - px * 0.8) * r, pY a + (uy * 0.6 - py * 0.8) * r)
          pts = cubicPts s0 (fst s0 + (ux * 2.2 + px * 0.9) * r, snd s0 + (uy * 2.2 + py * 0.9) * r)
                            (fst s1 + (ux * 2.2 - px * 0.9) * r, snd s1 + (uy * 2.2 - py * 0.9) * r) s1 18
      in (pts, (pX a + ux * 2.35 * r, pY a + uy * 2.35 * r))
  | otherwise =
      let dx = pX b - pX a
          dy = pY b - pY a
          l0 = hyp dx dy
          l = if l0 == 0 then 1 else l0
          ux = dx / l
          uy = dy / l
          bend = if both then 0.22 * l else 0
          mx = (pX a + pX b) / 2 - uy * bend
          my = (pY a + pY b) / 2 + ux * bend
          d0x = mx - pX a
          d0y = my - pY a
          l0' = nz (hyp d0x d0y)
          d1x = mx - pX b
          d1y = my - pY b
          l1 = nz (hyp d1x d1y)
          pts = bezPts (pX a + (d0x / l0') * r) (pY a + (d0y / l0') * r) mx my (pX b + (d1x / l1) * (r + 2)) (pY b + (d1y / l1) * (r + 2)) 16
          (midX, midY) = at' pts 8 (mx, my)
      in (pts, (midX - uy * 13, midY + ux * 13))
  where nz v = if v == 0 then 1 else v

-- 図の描き方の選択（p5.js 版の opt）
data MOpt = MOpt
  { oR :: Float                               -- 円の半径
  , oNames :: Maybe [String]                  -- 状態の名前（Nothing なら q0, q1, …）
  , oHi :: IM.IntMap RGB                      -- 色をつける状態
  , oEdgeHi :: Edge -> Maybe RGB              -- 色をつける辺
  , oStart :: Bool                            -- 開始の矢印を描くか
  , oLabels :: Bool                           -- 辺のラベルを描くか
  , oLabelSize :: Float
  , oNameSize :: Maybe Float
  , oEdgeCol :: RGB
  , oLabelCol :: RGB
  , oStartCol :: RGB
  , oStartAng :: Maybe Float }

defOpt :: MOpt
defOpt = MOpt 22 Nothing IM.empty (const Nothing) True True 14 Nothing (120, 160, 140) (200, 225, 210) ink Nothing

-- 状態の数 n・開始・受理・辺・位置から図を描く
drawMachine :: MOpt -> Int -> Int -> [Bool] -> [Edge] -> [Pos] -> Picture
drawMachine opt n start acc edges pos = Pictures (edgePics ++ [startPic] ++ statePics)
  where
    r = oR opt
    has = S.fromList [ (eFrom e, eTo e) | e <- edges ]
    posOf q = if q >= 0 then at' (map Just pos) q Nothing else Nothing
    edgePics =
      [ Pictures [ arrowLine pts (maybe (oEdgeCol opt) id hc) (if hc == Nothing then 1.8 else 3.5) (eLabel e == epsLabel && hc == Nothing)
                 , if oLabels opt || hc /= Nothing then sayM (eLabel e) lx ly (max 14 (oLabelSize opt)) (col (maybe (oLabelCol opt) id hc)) else Blank ]
      | e <- edges, Just a <- [posOf (eFrom e)], Just b <- [posOf (eTo e)]
      , let (pts, (lx, ly)) = edgeGeometry e a b r (eFrom e /= eTo e && S.member (eTo e, eFrom e) has)
      , let hc = oEdgeHi opt e ]
    startPic = case posOf start of
      Just p | oStart opt && start < n ->
        let sa = case oStartAng opt of
              Just v -> v
              Nothing -> case pLoop p of
                Just lp | cos lp < -0.5 -> -2.2
                _ -> pi
            ux = cos sa
            uy = sin sa
        in arrowLine [(pX p + ux * (r + 34), pY p + uy * (r + 34)), (pX p + ux * (r + 2), pY p + uy * (r + 2))] (oStartCol opt) 2 False
      _ -> Blank
    statePics =
      [ Pictures [ Color (col (maybe panel (scaleRGB 0.35) hc)) (circleAt (pX p) (pY p) (2 * r))
                 , Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r) sw)
                 , if at' acc q False then Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r - 9) sw) else Blank
                 , sayM nm (pX p) (pY p + 1) (max 14 (maybe (min 16 (r * 0.75)) id (oNameSize opt))) (col (maybe ink (const (255, 255, 255)) hc)) ]
      | q <- [0 .. n - 1], Just p <- [posOf q]
      , let hc = IM.lookup q (oHi opt)
      , let sw = if hc == Nothing then 2 else 3.5
      , let nm = maybe ("q" ++ show q) (\ns -> at' ns q "") (oNames opt) ]

drawDfa :: MOpt -> DFA -> [Pos] -> Picture
drawDfa opt d = drawMachine opt (dN d) (dStart d) (dAcc d) (dfaEdges d)

drawNfa :: MOpt -> NFA -> [Pos] -> Picture
drawNfa opt m = drawMachine opt (nN m) (nStart m) (nAcc m) (nfaEdges m)

-- 円の上に並べる（自己ループは外向き）
circleLayout :: Int -> Float -> Float -> Float -> Float -> [Pos]
circleLayout n cx cy rad a0 =
  [ Pos (cx + rad * cos a) (cy + rad * sin a) (Just a) | i <- [0 .. n - 1], let a = a0 + 2 * pi * i2f i / i2f (max 1 n) ]

-- スタートからの距離（幅優先）で左 → 右に並べる
layerLayout :: Int -> Int -> [Edge] -> Float -> Float -> Float -> Float -> [Pos]
layerLayout n start edges x y w h
  | n <= 0 = []
  | otherwise =
      let st = clampI start 0 (n - 1)
          bfs i order depth
            | i >= length order = (order, depth)
            | otherwise =
                let q = at' order i 0
                    dq = IM.findWithDefault 0 q depth
                    (order', depth') = foldl' (\(o, dp) e -> if eFrom e == q && not (IM.member (eTo e) dp) && eTo e >= 0 && eTo e < n
                                                               then (o ++ [eTo e], IM.insert (eTo e) (dq + 1) dp) else (o, dp)) (order, depth) edges
                in bfs (i + 1) order' depth'
          (order0, depth0) = bfs 0 [st] (IM.singleton st 0)
          maxD0 = maxOr 0 (IM.elems depth0)
          rest = [ q | q <- [0 .. n - 1], not (IM.member q depth0) ]
          depth = foldl' (\dp q -> IM.insert q (maxD0 + 1) dp) depth0 rest
          order = order0 ++ rest
          maxD = maxOr 0 (IM.elems depth)
          cols = IM.fromListWith (flip (++)) [ (IM.findWithDefault 0 q depth, [q]) | q <- order ]
          posM = IM.fromList [ (q, Pos (x + (if maxD > 0 then i2f d / i2f maxD * w else w / 2)) (y + i2f (k + 1) / i2f (length cs + 1) * h) (Just (-pi / 2)))
                             | (d, cs) <- IM.toList cols, (k, q) <- zip [0 :: Int ..] cs ]
      in [ IM.findWithDefault (Pos x y Nothing) q posM | q <- [0 .. n - 1] ]

-- (x, y) の上にある状態（後ろから調べる。なければ −1）
hitState :: [Pos] -> Float -> Float -> Float -> Int
hitState pos x y r = headOr (-1) [ q | (q, p) <- reverse (zip [0 ..] pos), hyp (pX p - x) (pY p - y) < r ]

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
