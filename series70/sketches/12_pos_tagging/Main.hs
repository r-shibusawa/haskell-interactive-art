-- ============================================
-- 70-12 品詞タグ付け ─ 状態 = 品詞（名詞・動詞・…）、観測 = 単語。下の小さな英語コーパス（この教材のために書いた 56 文、品詞つき）を数えて HMM を作る：
--   遷移 A = 「限定詞の次は名詞が多い」などの品詞のつながり（数 +1 して割る）、出力 B = 「名詞のとき dog が出る」などの単語の出やすさ（数 +0.1 して割る）：加算スムージング
-- 文を打つと、ビタビでいちばんありそうな品詞の列を付ける。flies（ハエ / 飛ぶ）や like（好む / 〜のように）のように 2 つの品詞がありうる単語は、前後の単語で決まる。
-- 知らない単語：コーパスに 1 回しか出ない単語を「知らない単語の見本」と考え、その品詞の割合を使う（名詞・動詞になりやすい）
-- 操作：文字キー・スペース 文を打つ（全部入力になる）/ Backspace 1 文字消す / Shift + Backspace 全部消す（p5.js 版の Esc）/ ↑↓ 例文 / Tab 右の表を切りかえ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (Path, rose)
import Data.Array.Unboxed (UArray, accumArray, listArray, (!))
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (isAsciiLower, toLower)
import Data.List (elemIndex, intercalate)
tags, tagNames :: [String]
tags = ["D", "N", "V", "A", "P", "R", "B"]
tagNames = ["det", "noun", "verb", "adj", "prep", "pron", "adv"]   -- 限定詞・名詞・動詞・形容詞・前置詞・代名詞・副詞

corpus :: [String]                            -- ★ 学習用の文（単語/品詞）。自分の文を足してみよう
corpus =
  [ "the/D old/A dog/N sleeps/V", "a/D small/A bird/N flies/V over/P the/D lake/N", "the/D flies/N like/V the/D sweet/A jam/N", "she/R runs/V like/P the/D wind/N"
  , "they/R watch/V the/D old/A clock/N", "my/D watch/N is/V new/A", "the/D light/N is/V bright/A", "he/R carries/V a/D light/A bag/N"
  , "we/R fish/V in/P the/D river/N", "the/D fish/N swim/V quickly/B", "she/R walks/V to/P the/D station/N", "the/D long/A walks/N are/V good/A"
  , "the/D plants/N need/V water/N", "he/R plants/V a/D tree/N in/P the/D garden/N", "i/R like/V green/A tea/N", "the/D cat/N sleeps/V on/P the/D warm/A chair/N"
  , "a/D kite/N flies/V high/B", "the/D children/N play/V in/P the/D park/N", "birds/N sing/V in/P the/D morning/N", "he/R sings/V like/P a/D bird/N"
  , "the/D small/A flies/N sit/V on/P the/D fruit/N", "time/N moves/V slowly/B", "we/R like/V the/D new/A teacher/N", "the/D teacher/N reads/V a/D long/A book/N"
  , "she/R reads/V quickly/B", "they/R walk/V slowly/B in/P the/D rain/N", "the/D rain/N falls/V on/P the/D roof/N", "a/D bright/A star/N shines/V over/P the/D hill/N"
  , "you/R watch/V the/D birds/N", "the/D dog/N runs/V after/P the/D cat/N", "it/R looks/V like/P rain/N", "the/D red/A kite/N is/V in/P the/D tree/N"
  , "he/R is/V happy/A", "the/D river/N is/V deep/A", "i/R see/V a/D plane/N", "the/D plane/N flies/V over/P the/D city/N"
  , "she/R likes/V the/D quiet/A garden/N", "we/R eat/V fresh/A fish/N", "the/D cook/N makes/V a/D good/A soup/N", "they/R cook/V with/P fresh/A plants/N"
  , "the/D boy/N has/V a/D new/A watch/N", "a/D girl/N walks/V with/P her/D dog/N", "old/A friends/N talk/V often/B", "the/D students/N like/V music/N"
  , "he/R often/B reads/V in/P the/D park/N", "the/D wind/N is/V strong/A", "she/R sees/V the/D light/N of/P the/D moon/N", "flies/N are/V small/A"
  , "the/D bee/N flies/V to/P the/D flower/N", "it/R runs/V quickly/B", "we/R sit/V under/P the/D big/A tree/N", "the/D moon/N is/V like/P a/D lamp/N"
  , "dogs/N like/V long/A walks/N", "the/D baby/N sleeps/V quietly/B", "my/D friend/N plays/V the/D piano/N", "a/D cat/N sits/V near/P the/D door/N" ]

examples :: [String]                          -- ★ 例文（↑↓）
examples = ["the bird flies like the wind", "the flies like the jam", "she walks in the park", "the walks are long", "they watch the fish", "my watch is old", "he plants a blorf near the zorp"]

emitAdd :: Double
emitAdd = 0.1                                 -- ★ 単語の数に足す数。遷移は +1 だが、単語は種類が多い（120 種類）ので +1 だと強すぎる → +0.1

maxWords :: Int
maxWords = 10

typeKeys :: String
typeKeys = "abcdefghijklmnopqrstuvwxyz' "

-- 数えて作ったタグ付けのモデル
data Tagger = Tagger
  { tgHmm   :: Hmm                            -- p0 と tr（tr の行 + pEnd で合計 1）
  , tgPEnd  :: [Double]                       -- その品詞で文が終わる確率
  , tgEmitC :: [M.Map String Int]             -- 品詞ごとの単語の数
  , tgTagTotal :: [Int]
  , tgUnk   :: [Int]                          -- 1 回しか出ない単語の数（品詞ごと）
  , tgVocab :: [String]                       -- 出てきた順
  , tgWordTotal :: M.Map String Int
  , tgSentences :: Int }

-- タグ付けの結果
data Result = Result { rsWords :: [String], rsTags :: [Int], rsGam :: [Vec], rsLogP :: !Double, rsKnown :: [Bool], rsEb :: Mat }

data World = World
  { wTyped :: String
  , wExNo  :: !Int                            -- 例文の番号（−1 = 自分で打った文）
  , wRes   :: Result
  , wTable :: !Int                            -- 右の表：0 = 遷移行列、1 = 単語の出やすさ
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "70-12 Part-of-speech tagging" world0)
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

tagger :: Tagger
tagger = trainTagger corpus

world0 :: World
world0 = retag (World (headOr "" examples) 0 (Result [] [] [] 0 [] []) 0 [])

retag :: World -> World
retag w = w { wRes = tagSentence tagger (tokenize (wTyped w)) }

-- "dog/N" → ("dog", 1)。品詞がわからないものは捨てる
parseLine :: String -> [(String, Int)]
parseLine s = [ (wd, t) | tok <- splitOn ' ' s, let k = lastSlash tok, k >= 0, let wd = take k tok, let t = maybe (-1) id (elemIndex (drop (k + 1) tok) tags), not (null wd), t >= 0 ]
  where lastSlash tok = maybe (-1) id (lastIdx '/' tok)
        lastIdx c xs = case [ i | (i, x) <- zip [0 ..] xs, x == c ] of
          [] -> Nothing
          is -> Just (last is)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- ★ 数えて作る：文のはじめの品詞、品詞 → 品詞（「文の終わり」も行き先の 1 つ）、品詞 → 単語
trainTagger :: [String] -> Tagger
trainTagger lns = Tagger (Hmm p0 tr []) pEnd emitC tagTotal unk vocab wordTotal (length lns)
  where
    n = length tags
    sents = map parseLine lns
    start = [ length [ () | (_, t) : _ <- sents, t == i ] | i <- [0 .. n - 1] ]
    pairs = concat [ zip (map snd ws) (map snd (drop 1 ws)) | ws <- sents ]
    trans = [ [ length [ () | (a, b) <- pairs, a == i, b == j ] | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
    endC = [ length [ () | ws <- sents, not (null ws), snd (last ws) == i ] | i <- [0 .. n - 1] ]
    allW = concat sents
    emitC = [ M.fromListWith (+) [ (wd, 1) | (wd, t) <- allW, t == i ] | i <- [0 .. n - 1] ]
    tagTotal = [ length [ () | (_, t) <- allW, t == i ] | i <- [0 .. n - 1] ]
    wordTotal = M.fromListWith (+) [ (wd, 1) | (wd, _) <- allW ]
    vocab = nubOrd (map fst allW)
    unk = [ length [ () | wd <- M.keys (at' emitC t M.empty), M.lookup wd wordTotal == Just 1 ] | t <- [0 .. n - 1] ]
    p0 = [ i2d (c + 1) / i2d (length lns + n) | c <- start ]
    tr = [ let s = sum r + e in [ i2d (c + 1) / i2d (s + n + 1) | c <- r ] | (r, e) <- zip trans endC ]
    pEnd = [ i2d (c + 1) / i2d (sum r + c + n + 1) | (r, c) <- zip trans endC ]

nubOrd :: [String] -> [String]
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- ★ 品詞 t が単語 w を出す確率（加算スムージング）。知らない単語は unk !! t を数として使う。単語全部 + 「知らない単語」で合計 1
emitProb :: Tagger -> Int -> String -> Double
emitProb tg t w =
  let denom = i2d (at' (tgTagTotal tg) t 0 + at' (tgUnk tg) t 0) + emitAdd * i2d (length (tgVocab tg) + 1)
      cnt = if M.member w (tgWordTotal tg) then M.findWithDefault 0 w (at' (tgEmitC tg) t M.empty) else at' (tgUnk tg) t 0
  in (i2d cnt + emitAdd) / denom

-- 小文字にして a〜z と ' 以外で区切る（10 語まで）
tokenize :: String -> [String]
tokenize s = take maxWords (filter (not . null) (splitBy (map toLower s)))
  where ok c = isAsciiLower c || c == '\''
        splitBy str = case break (not . ok) str of
          (a, []) -> [a]
          (a, _ : rest) -> a : splitBy rest

-- ★ タグ付け：出力の表を作ってビタビ（最後の単語には「そこで文が終わる確率」も掛ける）。γ（単語ごとの品詞の確率）も計算する
tagSentence :: Tagger -> [String] -> Result
tagSentence tg ws
  | null ws = Result ws [] [] 0 [] []
  | otherwise =
      let nw = length ws
          eb = [ [ emitProb tg t wd * (if k == nw - 1 then at' (tgPEnd tg) t 0 else 1) | t <- [0 .. length tags - 1] ] | (k, wd) <- zip [0 ..] ws ]
          v = viterbiLog (tgHmm tg) eb
          sm = smoothAll (tgHmm tg) eb
      in Result ws (vtPath v) (smGam sm) (vtLogP v) [ M.member wd (tgWordTotal tg) | wd <- ws ] eb

tagsSeenFor :: Tagger -> String -> [Int]
tagsSeenFor tg wd = [ t | t <- [0 .. length tags - 1], M.findWithDefault 0 wd (at' (tgEmitC tg) t M.empty) > 0 ]

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle env ("POS tagging: HMM counted from a small English corpus (" ++ show (tgSentences tagger) ++ " sentences, " ++ show (length (tgVocab tagger)) ++ " words)")
      "Letter keys, Space: type a sentence   Backspace delete   Shift+Backspace clear all   Up/Down examples   Tab switch the right table   (all letter keys are input)"
  , say "Text:" 40 122 15 (col lan)
  , say (wTyped w ++ (if envFrame env `mod` 60 < 30 then "_" else "")) 100 124 24 (col ink)
  , if n == 0 then note "Type an English sentence (e.g. the bird flies like the wind)" x0 (y0 + 30) dimc 15 else Blank
  , Pictures [ wordCol k | k <- [0 .. n - 1] ]
  , Pictures [ note nm 6 (y0 + 88 + i2f j * 20) (stCol j) 14 | (j, nm) <- zip [0 :: Int ..] tagNames ]
  , note "Box = Viterbi tag   bars = gamma (prob. of each tag, uses words before and after)" x0 ty dimc 14
  , if n > 0 then note ("log P of the best tag sequence = " ++ fmtN (rsLogP res) 2 ++ "   yellow text = word seen with 2+ tags") x0 (ty + 22) dimc 14 else Blank
  , note ("Examples (Up/Down): " ++ (if wExNo w >= 0 then show (wExNo w + 1) ++ " / " ++ show (length examples) else "your own sentence")) x0 (ty + 50) ink 14
  , Pictures [ note ((if k == wExNo w then "> " else "  ") ++ ex) x0 (ty + 74 + i2f k * 20) (if k == wExNo w then lan else dimc) 14 | (k, ex) <- zip [0 :: Int ..] examples ]
  -- ---- 右：遷移行列 か 単語の出やすさ
  , if wTable w == 0 then Pictures
      [ note "Transition A (row = previous tag, col = next)" (rx - 40) 150 ink 14
      , drawMatrix (hTr (tgHmm tagger)) (rx + 60) 184 cs 30 tagNames tags matOpt { moCol = rose, moHi = 0.8 }
      , note "First tag of a sentence" rx (184 + 7 * 30 + 30) ink 14
      , drawMatrix [hP0 (tgHmm tagger)] (rx + 60) (184 + 7 * 30 + 56) cs 28 [] tags matOpt { moCol = aqua, moHi = 0.8 }
      , note "End of sentence" rx (184 + 7 * 30 + 116) ink 14
      , drawMatrix [tgPEnd tagger] (rx + 60) (184 + 7 * 30 + 142) cs 28 [] tags matOpt { moCol = lan, moHi = 0.8 }
      , note "After D (det) comes N or A; after R (pron) comes V" rx (184 + 7 * 30 + 196) dimc 14 ]
    else Pictures
      [ note "Emission b_tag(word) (row = tag, col = word)" (rx - 40) 150 ink 14
      , if n > 0 then drawMatrix [ [ emitProb tagger t wd | wd <- ws ] | t <- [0 .. length tags - 1] ] (rx + 20) 184 ecw 30 tagNames (map (take 4) ws)
                        matOpt { moCol = lan, moHi = 0.08, moDigits = 3, moNumbers = ecw >= 50 } else Blank
      , if n > 0 && ecw < 50 then note "(numbers hidden: too many words to fit)" (rx - 40) (184 + 7 * 30 + 54) dimc 14 else Blank
      , note "Unknown words are small for every tag (a bit larger for N, V, A)" (rx - 40) (184 + 7 * 30 + 30) dimc 14 ]
  , drawSelfTest env (wTest w) ]
  where
    res = wRes w
    ws = rsWords res
    n = length ws
    x0 = 50
    cw = min 118 ((min (envW env - 420) 1180 - x0) / i2f (max 1 n))
    y0 = 180
    ty = y0 + 76 + 7 * 20 + 30
    rx = max (envW env - 390) 700
    cs = 40
    ecw = min cs (330 / i2f (max 1 n))
    wordCol k =
      let x = x0 + i2f k * cw
          t = at' (rsTags res) k 0
          wd = at' ws k ""
          seen = tagsSeenFor tagger wd
          known = at' (rsKnown res) k False
          gm = at' (rsGam res) k []
      in Pictures
           [ sayC wd (x + cw / 2) y0 (max 14 (min 20 (cw / 5.2))) (col ink)
           , Color (col (stCol t)) (rectTL (x + 6) (y0 + 12) (cw - 12) 30)
           , sayM (at' tagNames t "") (x + cw / 2) (y0 + 27) 15 (col bgc)
           , note (if not known then "? unknown" else if length seen > 1 then intercalate "/" [ at' tags s "" | s <- seen ] else "") (x + 6) (y0 + 62) (if not known then rose else lan) 14
           , Pictures [ Pictures [ Color (col linec) (rectTL (x + 6) yy (cw - 12) 14)
                                 , Color (colA (if j == t then 255 else 150) (stCol j)) (rectTL (x + 6) yy ((cw - 12) * f2 p) 14)
                                 , if p > 0.005 then sayRM (fmtN p 2) (x + cw - 9) (yy + 7) 14 (col ink) else Blank ]
                      | j <- [0 .. length tags - 1], let yy = y0 + 76 + i2f j * 20, let p = at' gm j 0 ]
           , if k > 0 then arrowLine 1.5 (col dimc) (x - 4, y0 + 27) (x + 5, y0 + 27) 6 else Blank ]

-- キーの名前をそろえる（gloss では スペース・Backspace・Tab・Esc が SpecialKey で来る）
-- TClear = 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じるので Shift + Backspace）
data TypedKey = TChar Char | TBack | TClear | TTab | TUp | TDown | TOther

typedKey :: Key -> Modifiers -> TypedKey
typedKey k m = case k of
  Char '\b' -> back
  Char '\DEL' -> back
  Char '\t' -> TTab
  Char '\ESC' -> TClear
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> back
  SpecialKey KeyDelete -> back                -- Mac の delete キー
  SpecialKey KeyTab -> TTab
  SpecialKey KeyEsc -> TClear
  SpecialKey KeyUp -> TUp
  SpecialKey KeyDown -> TDown
  _ -> TOther
  where back = if shift m == Down then TClear else TBack

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TChar c | toLower c `elem` typeKeys -> retag w { wTyped = if length (wTyped w) < 60 then wTyped w ++ [toLower c] else wTyped w, wExNo = -1 }
    TBack -> retag w { wTyped = if null (wTyped w) then "" else init (wTyped w), wExNo = -1 }
    TClear -> retag w { wTyped = "", wExNo = -1 }
    TDown -> let e = (wExNo w + 1) `mod` ne in retag w { wExNo = e, wTyped = at' examples e "" }
    TUp -> let e = (wExNo w + ne - 1) `mod` ne in retag w { wExNo = e, wTyped = at' examples e "" }
    TTab -> w { wTable = 1 - wTable w }
    _ -> w
  _ -> w
  where ne = length examples

-- ---------- やってみよう ----------
-- 1. 「the bird flies like the wind」と「the flies like the jam」で、flies と like の品詞はどう変わる？ γ の棒はどのくらい迷っている？
-- 2. コーパスにない単語（blorf、zorp など）をまぜた文を打とう。どの品詞になる？ 前に the があるときと he があるときでちがう？
-- 3. 「fish fish fish」と打つとどうなる？ なぜそう読んだ？
-- 4. corpus に自分の文を 5 つ足して（単語/品詞 の形で）、タグ付けが変わるか試そう。「-ly で終わる知らない単語は副詞」というルールを足すには？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("コーパス " ++ show (tgSentences tg) ++ " 文：どの単語にも品詞があり、数えた単語の合計 = " ++ show (sum (tgTagTotal tg)),
      all (\ln -> length (parseLine ln) == length (splitOn ' ' ln)) corpus && sum (tgTagTotal tg) == length (splitOn ' ' (unwords corpus)))
  , ("文のはじめの確率の合計 = 1、遷移行列の行 + 「文の終わり」の合計 = 1（+1 スムージングで 0 はない）",
      near (sumOf (hP0 hm)) 1 1e-12 && and [ near (sumOf r + e) 1 1e-12 && all (> 0) r | (r, e) <- zip (hTr hm) (tgPEnd tg) ])
  , ("単語の出やすさ：全部の単語 + 「知らない単語」で合計 = 1（7 つの品詞とも）",
      and [ near (foldl' (\s wd -> s + emitProb tg t wd) (emitProb tg t "zzz-unknown") (tgVocab tg)) 1 1e-12 | t <- [0 .. nT - 1] ])
  , ("flies・like・watch・walks・plants・fish・light は 2 つ以上の品詞で出てくる", all (\wd -> length (tagsSeenFor tg wd) >= 2) ["flies", "like", "watch", "walks", "plants", "fish", "light"])
  , ("2 つの品詞がありうる単語をふくむ 8 文：期待どおりの品詞（flies は 名詞 / 動詞、like は 動詞 / 前置詞 に分かれる）" ++ (case bad of
        [] -> ""
        (s, _) : _ -> " ✗ " ++ s ++ " → " ++ tagStr s), null bad)
  , ("知らない単語：the zorp sleeps → D N V、he plants a blorf near the zorp → R V D N P D N", tagStr "the zorp sleeps" == "D N V" && tagStr "he plants a blorf near the zorp" == "R V D N P D N")
  , ("学習に使った文をタグ付けし直すと " ++ show right ++ " / " ++ show total ++ " 語が正解（95% 以上）", i2d right / i2d total >= 0.95)
  , ("ビタビ = 総当たり（7⁴ = 2401 通りの品詞の列）、γ = 総当たり", near (exp (rsLogP r4)) (bfBestP bf) (1e-12 * bfBestP bf) && rsTags r4 == bfBest bf && maxAbsDiff (rsGam r4) (bfGam bf) < 1e-12)
  , ("空の文・記号だけ・知らない単語だけの長い文でも止まらない（単語は " ++ show maxWords ++ " 個まで）、NaN なし",
      null (rsTags j0) && length (rsTags j1) <= 1 && length (rsTags j2) == maxWords && noNaNM (rsGam j2) && isFin (rsLogP j2))
  , ("知らない単語になりやすい品詞は名詞（1 回だけ出た単語がいちばん多い）、限定詞・前置詞は少ない", argMax (map i2d u) == 1 && at' u 0 0 < at' u 1 0 && at' u 4 0 < at' u 1 0) ]
  where
    tg = trainTagger corpus
    hm = tgHmm tg
    nT = length tags
    u = tgUnk tg
    tagStr s = unwords [ at' tags t "" | t <- rsTags (tagSentence tg (tokenize s)) ]
    want = [("the bird flies like the wind", "D N V P D N"), ("the flies like the jam", "D N V D N"), ("she walks in the park", "R V P D N"), ("the walks are long", "D N V A"), ("they watch the fish", "R V D N"), ("my watch is old", "D N V A"), ("we fish in the lake", "R V P D N"), ("he plants a tree", "R V D N")]
    bad = [ p | p@(s, t) <- want, tagStr s /= t ]
    scored = [ (length [ () | ((_, tt), rt) <- zip ws (rsTags r), rt == tt ], length ws) | ln <- corpus, let ws = parseLine ln, let r = tagSentence tg (map fst ws) ]
    right = sum (map fst scored)
    total = sum (map snd scored)
    r4 = tagSentence tg ["the", "flies", "like", "jam"]
    bf = bruteAll hm (rsEb r4)
    j0 = tagSentence tg (tokenize "")
    j1 = tagSentence tg (tokenize "'' 123 !!")
    j2 = tagSentence tg (tokenize "qq zz xx yy ww vv uu tt ss rr pp oo nn")

-- ==================================================
-- ここから下は「HMM の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 計算は Double（倍精度）、描くときだけ Float にする。分布 = リスト、行列 = 行のリスト
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（霧の夜と灯り：黄 = 見える観測、ばら色 = 隠れた状態）
type RGB = (Float, Float, Float)
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（行のリスト）

bgc, ink, lan, rose, aqua, lilac, leaf, tang, dimc, panelc, linec :: RGB
bgc = (27, 33, 44)                            -- 背景（霧の夜の青ねずみ色）
ink = (230, 234, 240)                         -- 文字（やわらかい白）
lan = (250, 208, 100)
rose = (236, 124, 150)
aqua = (110, 200, 210)
lilac = (176, 156, 236)
leaf = (150, 210, 130)
tang = (240, 160, 96)
dimc = (142, 154, 172)
panelc = (38, 46, 60)
linec = (76, 88, 108)

stCols :: [RGB]                               -- 状態の色
stCols = [rose, aqua, lan, lilac, leaf, tang, (200, 200, 210), (130, 150, 240)]

stCol :: Int -> RGB
stCol i = at' stCols (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampF' :: Float -> Float -> Float -> Float
clampF' v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isFin v then realToFrac v else 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

-- 行列の (i, j)（なければ 0）
at2 :: [[Double]] -> Int -> Int -> Double
at2 a i j = at' (at' a i []) j 0

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

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

forceM :: [[Double]] -> [[Double]]
forceM xs = foldl' (\u r -> forceL r `seq` u) () xs `seq` xs

-- 最初にいちばん大きい値の番号（同点なら小さい番号）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に。∞ は画面では inf）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "∞" else "−∞"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ fixedDigits n d
  where n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer

fixedDigits :: Integer -> Int -> String
fixedDigits n d =
  let ds = show n
      padded = replicate (d + 1 - length ds) '0' ++ ds
      (ip, fp) = splitAt (length padded - d) padded
  in if d > 0 then ip ++ "." ++ fp else ip

-- v.toFixed(d) そのまま（−0.0000 も残す）
toFixed :: Double -> Int -> String
toFixed v d
  | not (isFin v) = jsNum v
  | otherwise = (if v < 0 then "-" else "") ++ fixedDigits (floor (toRational (abs v) * 10 ^ d + 1 / 2)) d

-- 確率の表示：小さい数は 1.2e-5 の形で
fmtP :: Double -> String
fmtP v
  | isNaN v = "?"
  | v == 0 = "0"
  | abs v >= 0.001 = toFixed v (if v >= 0.1 then 3 else 4)
  | otherwise = toExp1 v

-- v.toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | not (isFin v) = jsNum v
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> ((), rngOf s))

rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

-- 乱数の列 g で計算して、(結果, 進んだ列)
runWith :: Rng -> R a -> (a, Rng)
runWith g m = runR m g

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + 0x6d2b79f5 in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- n 回くり返して結果を順に集める（1 回ずつ状態を進める）
replicateR :: Int -> R a -> R [a]
replicateR n m = go n []
  where go k acc = if k <= 0 then pure (reverse acc) else do { !x <- m; go (k - 1) (x : acc) }

-- 乱数を使う左たたみ込み（JavaScript の for で状態を書きかえていく計算）
foldlR :: (b -> a -> R b) -> b -> [a] -> R b
foldlR f = go
  where go !acc [] = pure acc
        go !acc (x : xs) = f acc x >>= \a -> go a xs

-- ---- 隠れマルコフモデルの道具
-- モデル = Hmm { hP0 = はじめの分布 π, hTr = 遷移行列 A（hTr !! i !! j = 状態 i の次に j）, hEm = 出力行列 B（hEm !! j !! k = 状態 j が記号 k を出す） }
-- どの計算も「出力の表」eb !! t !! j = b_j(x_t)（時刻 t の観測を状態 j が出す確率）を受け取る → 記号でも数（正規分布）でも同じ道具が使える
data Hmm = Hmm { hP0 :: Vec, hTr :: Mat, hEm :: Mat }

nStates :: Hmm -> Int
nStates = length . hP0

lg :: Double -> Double                        -- log 0 は −∞（NaN にしない）
lg p = if p > 0 then log p else negInf

-- log(Σ e^a_i)：いちばん大きい値を先に引く。全部 −∞ なら −∞
logSumExp :: [Double] -> Double
logSumExp a =
  let m = foldl' (\b v -> if v > b then v else b) negInf a
  in if m == negInf then negInf else m + log (sumOf [ exp (v - m) | v <- a ])

-- 合計 1 に（全部 0 なら一様に）
normRow :: Vec -> Vec
normRow r = let s = sumOf r in if s > 1e-300 then map (/ s) r else map (const (1 / i2d (length r))) r

rowSumErr :: Mat -> Double
rowSumErr a = foldl' (\d r -> maxN d (abs (sumOf r - 1))) 0 a

-- 2 つの行列（またはベクトル）のいちばん大きなずれ（NaN が混じれば NaN）
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- 横ベクトル × 行列：1 歩先の分布
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (length (headOr [] a)) 0) (zip v a)

emTable :: Hmm -> [Int] -> Mat                -- 記号の列 → 出力の表
emTable m xs = [ [ at' r x 0 | r <- hEm m ] | x <- xs ]

-- π, A, B からサイコロで列を作る：(隠れた状態, 観測)
hmmSample :: Hmm -> Int -> R ([Int], [Int])
hmmSample m n = do
  z0 <- sampleFrom (hP0 m)
  let go t z zs xs
        | t >= n = pure (reverse zs, reverse xs)
        | otherwise = do
            z' <- if t > 0 then sampleFrom (at' (hTr m) z []) else pure z
            x <- sampleFrom (at' (hEm m) z' [])
            go (t + 1) z' (z' : zs) (x : xs)
  go (0 :: Int) z0 [] []

-- ★ 前向き：α_t(j) = P(x_1..x_t, z_t = j) = b_j(x_t) Σ_i α_{t−1}(i) a_ij。尤度 P(x) = Σ_j α_T(j)
fwdPlain :: Hmm -> Mat -> (Mat, Double)
fwdPlain m eb = (al, if null al then 1 else sumOf (last al))
  where
    n = nStates m
    al = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s * at' e j 0 | j <- [0 .. n - 1]
                       , let s = if first then at' (hP0 m) j 0 else sumOf [ at' prev i 0 * at2 (hTr m) i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 後ろ向き：β_t(i) = P(x_{t+1}..x_T | z_t = i) = Σ_j a_ij b_j(x_{t+1}) β_{t+1}(j)。尤度 = Σ_i π_i b_i(x_1) β_1(i)
bwdPlain :: Hmm -> Mat -> (Mat, Double)
bwdPlain m eb = (be, lik)
  where
    n = nStates m
    be = if null eb then [] else foldr step [replicate n 1] (drop 1 eb)
    step e acc = let nb = headOr [] acc
                 in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] : acc
    lik = if null eb then 1 else sumOf [ at' (hP0 m) i 0 * at' (headOr [] eb) i 0 * at' (headOr [] be) i 0 | i <- [0 .. n - 1] ]

-- ★ スケーリングつきの前向き：毎時刻 合計 c_t で割る。fsAh !! t = P(z_t | x_1..x_t)（= フィルタ）、log P(x) = Σ log c_t
--   c_t = 0（ありえない観測）なら logLik = −∞ にして、その時刻は一様分布で先へ進む（NaN を作らない）
data FS = FS { fsAh :: [Vec], fsCs :: [Double], fsLogLik :: !Double }

fwdScaled :: Hmm -> Mat -> FS
fwdScaled m eb = let (ahR, csR, ll) = foldl' step ([], [], 0) eb in FS (reverse ahR) (reverse csR) ll
  where
    n = nStates m
    step (ah, cs, !ll) e =
      let pred' = if null ah then hP0 m else vecMat (headOr [] ah) (hTr m)
          row = zipWith (*) pred' e
          c = sumOf row
      in if not (c > 0) then (replicate n (1 / i2d n) : ah, 0 : cs, negInf)
         else (forceL (map (/ c) row) : ah, c : cs, if ll /= negInf then ll + log c else ll)

-- 同じ c_t で割った後ろ向き。γ_t(i) = ah !! t !! i × bh !! t !! i
bwdScaled :: Hmm -> Mat -> [Double] -> [Vec]
bwdScaled m eb cs = if null eb then [] else foldr step [replicate n 1] (zip (drop 1 eb) (drop 1 cs))
  where
    n = nStates m
    step (e, c0) acc =
      let nb = headOr [] acc
          c = if c0 > 0 then c0 else 1
      in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] / c | i <- [0 .. n - 1] ] : acc

-- 対数のまま：log α_t(j) = log b_j(x_t) + logSumExp_i(log α_{t−1}(i) + log a_ij)
fwdLog :: Hmm -> Mat -> (Mat, Double)
fwdLog m eb = (la, if null la then 0 else logSumExp (last la))
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    la = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s + lg (at' e j 0) | j <- [0 .. n - 1]
                       , let s = if first then lg (at' (hP0 m) j 0) else logSumExp [ at' prev i 0 + at2 lt i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 平滑化：γ_t(i) = P(z_t = i | 全部の観測)。smFilt = フィルタ P(z_t | x_1..x_t)
data SM = SM { smFilt :: [Vec], smGam :: [Vec], smBh :: [Vec], smCs :: [Double], smLogLik :: !Double }

smoothAll :: Hmm -> Mat -> SM
smoothAll m eb =
  let f = fwdScaled m eb
      bh = bwdScaled m eb (fsCs f)
      gam = zipWith (\a b -> forceL (normRow (zipWith (*) a b))) (fsAh f) bh
  in SM (fsAh f) gam bh (fsCs f) (fsLogLik f)

-- ξ_t(i, j) = P(z_t = i, z_{t+1} = j | 全部の観測)
xiAt :: Hmm -> Mat -> SM -> Int -> Mat
xiAt m eb sm t =
  let n = nStates m
      c0 = at' (smCs sm) (t + 1) 0
      c = if c0 > 0 then c0 else 1
      ft = at' (smFilt sm) t []
      e1 = at' eb (t + 1) []
      b1 = at' (smBh sm) (t + 1) []
  in [ [ at' ft i 0 * at2 (hTr m) i j * at' e1 j 0 * at' b1 j 0 / c | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

-- ★ ビタビ（対数）：vtDl !! t !! j = 時刻 t に状態 j で終わるいちばん良い道の log 確率、vtBp !! t !! j = そのとき 1 つ前にいた状態（戻り矢印、なければ −1）
data VT = VT { vtPath :: [Int], vtLogP :: !Double, vtDl :: [Vec], vtBp :: [[Int]] }

viterbiLog :: Hmm -> Mat -> VT
viterbiLog m eb = VT path logP dl bp
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    (dlR, bpR) = foldl' step ([], []) eb
    dl = reverse dlR
    bp = reverse bpR
    step (ds, bs) e =
      let prev = headOr [] ds
          cells = [ if null ds then (lg (at' (hP0 m) j 0), -1)
                    else foldl' (\(b, a) i -> let v = at' prev i 0 + at2 lt i j in if v > b then (v, i) else (b, a)) (negInf, -1) [0 .. n - 1]
                  | j <- [0 .. n - 1] ]
          row = forceL [ b + lg (at' e j 0) | (j, (b, _)) <- zip [0 ..] cells ]
      in (row : ds, map snd cells : bs)
    lastRow = lastOr [] dl
    zEnd = argMax lastRow
    logP = if null eb then 0 else at' lastRow zEnd negInf
    path = if null eb then [] else reverse (goBack zEnd bpR)
    goBack _ [] = []
    goBack z (b : rest) = let p = at' b z (-1) in z : goBack (if p >= 0 then p else 0) rest

-- 1 本の道 zs と観測がいっしょに起きる log 確率
pathLogProb :: Hmm -> Mat -> [Int] -> Double
pathLogProb m eb zs = foldl' (\s (t, z, zp, e) -> s + ((if t == 0 then lg (at' (hP0 m) z 0) else lg (at2 (hTr m) zp z)) + lg (at' e z 0))) 0
                        (zip4' [0 :: Int ..] zs (0 : zs) eb)

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

postDecode :: [Vec] -> [Int]                  -- 事後復号：時刻ごとにいちばんありそうな状態
postDecode = map argMax

-- ★ 総当たり：N^T 本の道を全部ためす（短い列だけ）。尤度、γ、ξ、いちばん良い道
data BF = BF { bfLik :: !Double, bfGam :: [Vec], bfXi :: [Mat], bfBest :: [Int], bfBestP :: !Double, bfPaths :: !Int }

bruteAll :: Hmm -> Mat -> BF
bruteAll m eb = BF lik gam xi best bestP total
  where
    n = nStates m
    len = length eb
    total = n ^ len
    ebA = listArray ((0, 0), (max 0 (len - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) eb ++ repeat 0) :: UArray (Int, Int) Double
    p0A = listArray (0, max 0 (n - 1)) (hP0 m ++ repeat 0) :: UArray Int Double
    trA = listArray ((0, 0), (max 0 (n - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) (hTr m) ++ repeat 0) :: UArray (Int, Int) Double
    pathOf k = U.generate len (\t -> (k `div` (n ^ (len - 1 - t))) `mod` n)
    probOf zs = let go t p = if t >= len || not (p > 0) then p
                             else let z = zs U.! t in go (t + 1) (p * ((if t == 0 then p0A ! z else trA ! (zs U.! (t - 1), z)) * ebA ! (t, z)))
                in go 0 1
    paths = [ (zs, probOf zs) | k <- [0 .. total - 1], let zs = pathOf k ]
    (lik0, bestP, bestV) = foldl' (\(l, bp, bz) (zs, p) -> let (bp', bz') = if p > bp then (p, zs) else (bp, bz) in (if p == 0 then l else l + p, bp', bz')) (0, -1, U.empty) paths
    best = U.toList bestV
    gamA = accumArray (+) 0 ((0, 0), (max 0 (len - 1), max 0 (n - 1))) [ ((t, zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [0 .. len - 1] ] :: UArray (Int, Int) Double
    xiA = accumArray (+) 0 ((0, 0, 0), (max 0 (len - 2), max 0 (n - 1), max 0 (n - 1))) [ ((t - 1, zs U.! (t - 1), zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [1 .. len - 1] ] :: UArray (Int, Int, Int) Double
    lik = lik0
    dv v = if lik > 0 then v / lik else v
    gam = [ [ dv (gamA ! (t, j)) | j <- [0 .. n - 1] ] | t <- [0 .. len - 1] ]
    xi = [ [ [ dv (xiA ! (t, i, j)) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] | t <- [0 .. len - 2] ]

-- 定常分布 π A = π（なまけた連鎖 (π + πA)/2 をくり返す。周期があっても収束する）
stationaryOf :: Mat -> Vec
stationaryOf tr = normRow (go (0 :: Int) (replicate n (1 / i2d n)))
  where
    n = length tr
    go k v
      | k >= 4000 = v
      | otherwise =
          let w = vecMat v tr
              u = forceL (zipWith (\a b -> (a + b) / 2) v w)
              d = foldl' maxN 0 (zipWith (\a b -> abs (a - b)) u v)
          in if d < 1e-15 then u else go (k + 1) u

hmmLogLik :: Hmm -> [Int] -> Double           -- 記号の列の対数尤度 log P(x)
hmmLogLik m xs = fsLogLik (fwdScaled m (emTable m xs))

predictK :: Vec -> Mat -> Int -> Vec          -- k 歩先の予測：A を k 回掛ける
predictK belief tr k = iterate (`vecMat` tr) belief !! max 0 k

-- ---- グラフの道具：G { 左上 x y, 幅 w, 高さ h, x の範囲 x0〜x1, y の範囲 y0〜y1 }
data G = G { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double }

gX :: G -> Double -> Float
gX g v = gx g + f2 (clampv ((v - gx0 g) / (gx1 g - gx0 g)) (-0.02) 1.02) * gw g

gY :: G -> Double -> Float
gY g v = gy g + gh g - f2 (clampv ((v - gy0 g) / (gy1 g - gy0 g)) (-0.02) 1.02) * gh g

-- 枠と目盛り（目盛りの数は有効数字 4 桁）、x 軸と y 軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures
  [ Color (col linec) (frameTL 1 (gx g) (gy g) (gw g) (gh g))
  , Pictures [ sayT (precN 4 v) (gX g v) (gy g + gh g + 4) 14 (col dimc) True | v <- xt ]
  , Pictures [ sayRM (precN 4 v) (gx g - 5) (gY g v) 14 (col dimc) | v <- yt ]
  , sayR xl (gx g + gw g) (gy g + gh g + 34) 14 (col ink)
  , say yl (gx g) (gy g - 8) 14 (col ink) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで（from〜to−1 だけ）
drawCurve :: G -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff = drawCurveR g arr c sw xOff 0 (length arr)

drawCurveR :: G -> [Double] -> RGB -> Float -> Double -> Int -> Int -> Picture
drawCurveR g arr c sw xOff a b =
  let n = b - a
      stride = max 1 ((n + 599) `div` 600)
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [a ..] (take n (drop a arr)), (i - a) `mod` stride == 0, isFin v ]
  in Color (col c) (polyline sw pts)

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [ Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink) ] | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 行列を色つきの表で（明るい = 大きい）
data MatOpt = MatOpt { moDigits :: Int, moCol :: RGB, moTs :: Float, moHi :: Double, moNumbers :: Bool }

matOpt :: MatOpt
matOpt = MatOpt 2 aqua 14 1 True

drawMatrix :: Mat -> Float -> Float -> Float -> Float -> [String] -> [String] -> MatOpt -> Picture
drawMatrix a x y cw ch rowNames colNames o = Pictures
  [ Pictures [ Pictures [ Color (col (mixC t)) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
                        , if moNumbers o then sayM (fmtN v (moDigits o)) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col ink) else Blank ]
             | (i, row) <- zip [0 :: Int ..] a, (j, v) <- zip [0 :: Int ..] row, let t = f2 (clampv (abs v / moHi o) 0 1) ]
  , Pictures [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col dimc) | (i, nm) <- zip [0 :: Int ..] (take (length a) rowNames) ]
  , Pictures [ sayC nm (x + i2f j * cw + cw / 2) (y - 4) ts (col dimc) | (j, nm) <- zip [0 :: Int ..] (take (length (headOr [] a)) colNames) ] ]
  where
    ts = max 14 (moTs o)
    (pr, pg, pb) = panelc
    (cr, cg, cb) = moCol o
    mixC t = (pr + t * (cr * 0.75 - pr), pg + t * (cg * 0.75 - pg), pb + t * (cb * 0.75 - pb))

-- 状態の列を色の帯で（同じ色が続くところは 1 つの四角にまとめる）。色が Nothing なら描かない
drawBand :: [Int] -> Float -> Float -> Float -> Float -> (Int -> Maybe RGB) -> Picture
drawBand arr x y w h colOf = Pictures [ case colOf v of
                                          Just c -> Color (col c) (rectTL (x + i2f s / n * w) y (i2f (e - s) / n * w + 0.6) h)
                                          Nothing -> Blank
                                      | (s, e, v) <- runsOf arr ]
  where n = i2f (max 1 (length arr))

-- 同じ値が続く区間 (はじめ, 終わり, 値)
runsOf :: [Int] -> [(Int, Int, Int)]
runsOf = go 0
  where go _ [] = []
        go s (v : vs) = let k = length (takeWhile (== v) vs) in (s, s + k + 1, v) : go (s + k + 1) (drop k vs)

-- 矢印（太さ sw、頭の大きさ hs）
arrowLine :: Float -> Color -> (Float, Float) -> (Float, Float) -> Float -> Picture
arrowLine sw c (x1, y1) (x2, y2) hs =
  let l0 = hyp (x2 - x1) (y2 - y1)
      l = if l0 == 0 then 1 else l0
      ux = (x2 - x1) / l
      uy = (y2 - y1) / l
  in Color c (Pictures [ lineW sw (x1, y1) (x2 - ux * hs * 0.6, y2 - uy * hs * 0.6)
                       , Polygon [(x2, y2), (x2 - ux * hs - uy * hs * 0.5, y2 - uy * hs + ux * hs * 0.5), (x2 - ux * hs + uy * hs * 0.5, y2 - uy * hs - ux * hs * 0.5)] ])

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 画面の文字は ASCII だけ（∞ や − などは近い ASCII に直す）
asciiText :: String -> String
asciiText = concatMap f
  where f c = case c of
          '∞' -> "inf"; '−' -> "-"; '×' -> "x"; '≈' -> "~"; '≤' -> "<="; '≥' -> ">="; '→' -> "->"; '←' -> "<-"
          '·' -> "."; '…' -> "..."; '✓' -> "OK"; '✗' -> "x"; 'π' -> "pi"; 'α' -> "a"; 'β' -> "b"; 'γ' -> "g"
          _ -> [c]

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c (asciiText s)))

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

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

-- p5.js 版の note(s, x, y, 色, 大きさ)：左揃え・BASELINE（大きさは 14 より小さくしない）
note :: String -> Float -> Float -> RGB -> Float -> Picture
note s x y c sz = say s x y (max 14 sz) (col c)

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
textW s size = fromIntegral (length (asciiText s)) * size * 0.72

-- 幅 w に収まるように単語の切れ目で折り返す
wrapText :: Float -> Float -> String -> [String]
wrapText w size s = concatMap wrapLine (lines s)
  where
    wrapLine ln = go [] (words ln)
    go cur [] = [unwords (reverse cur) | not (null cur)]
    go cur (x : xs) = let cand = unwords (reverse (x : cur))
                      in if null cur || textW cand size <= w then go (x : cur) xs else unwords (reverse cur) : go [x] xs

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

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か（左上を含み、右下を含まない）
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx >= bx && mx < bx + bw && my >= by && my < by + bh

-- パネル（p5.js 版の角の丸い長方形）
panelBox :: Float -> Float -> Float -> Float -> Picture
panelBox x y w h = Color (col panelc) (rectTL x y w h)

-- 左上の見出しと操作の説明（説明は幅に合わせて折り返す）
hudTitle :: Env -> String -> String -> Picture
hudTitle env t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (wrapText (envW env - 60) 14 s) ])

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

noNaN :: [Double] -> Bool                     -- NaN がない（∞ はよい）
noNaN = not . any isNaN

noNaNM :: [[Double]] -> Bool
noNaNM = all noNaN

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

-- 右下に小さく（p5.js 版の drawSelfTest）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 210) (envH env - 14) 14 (if all snd rs then col leaf else col (255, 90, 90))
