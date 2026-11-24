-- ============================================
-- 65-08 文字の n-gram で文章生成 ─ 「直前の k 文字」を状態にしたマルコフ連鎖
-- お手本の文章（このスケッチのために書いたオリジナルの短い文）で、k 文字の並びのあとに来た文字を数える
--   → P(次の文字 | 直前 k 文字) = 数 / 合計。サイコロを振って 1 文字ずつ足していくと、新しい文章ができる。
-- k を上げると本物らしくなるが、お手本の文のコピー（水色）が増える。エントロピー率 = 1 文字あたりの「迷い」（ビット）
-- 操作：スペース 新しく生成 / ↑↓ 次数 k（0〜4）/ T 英語 ⇔ 日本語
-- （gloss の文字は日本語を書けないので、日本語の文章は文字コード U+xxxx の 4 けたで表示する。計算は同じ）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat, showHex)
import System.Environment (lookupEnv)

-- ★ お手本の文章（自由に書きかえてよい。長いほど、高い次数でも新しい文が出やすい）
jaText, enText :: String
jaText = "朝の駅前には小さなパン屋があって、店の前を通るたびに焼きたての匂いがする。雨の日は傘の色がならび、晴れの日は自転車が川ぞいの道を走っていく。わたしは毎朝その道を歩いて、橋の上で少しだけ立ち止まる。川の水は昨日と同じように見えるけれど、本当は昨日とは別の水だ。空の雲も、道の影も、いつも少しずつちがう。だから同じ道を歩いても、同じ朝は二度とこない。パン屋の主人は、今日の空を見てから焼くパンを決めるそうだ。くもりの日には丸いパン、雨の日には甘いパン、晴れの日には長いパン。次に何を焼くかは、今日の空だけで決まる。昨日の空は、もう関係ない。夕方になると、川の上の空は少しずつ赤くなり、橋の上の人の影が長くのびる。わたしは帰り道にパン屋に寄って、残っているパンを一つ買う。店の主人は、明日の空はどうだろうねと言いながら、窓の外の雲を見ている。明日の朝も、わたしは同じ道を歩くだろう。けれど、明日の道は今日の道とは少しちがう。雨の日のあとには晴れの日が来ることが多く、晴れの日のあとにはまた晴れの日が来ることも多い。くもりの日は、雨にも晴れにもなる。どの日も、次の日のことは今日の空だけが知っている。"
enText = "the river town wakes up slowly. a bus turns at the corner, a dog waits by the bakery door, and the baker looks at the sky before choosing the bread. on cloudy days she bakes round loaves, on rainy days sweet buns, and on sunny days long thin sticks that crack when you break them. nobody remembers what the sky looked like last week, and it does not matter. tomorrow depends only on today. children walk to school along the river, counting the bridges, and every morning the water looks the same, though it is never the same water. the clouds drift, the shadows move, and the town keeps changing one small piece at a time. "

genLen, copyLen :: Int
genLen = 300                                  -- ★ 生成する文字数
copyLen = 8                                   -- ★ この長さの並びがお手本にそのまま出てきたら「コピー」とみなす

-- n-gram のモデル：文脈（直前 k 文字）→ 次の文字の確率（大きい順、同じなら先に出た順）
-- ctxOrder は文脈が初めて出た順（JavaScript の Map と同じ順番）
data Model = Model { mK :: !Int, mN :: !Int, mRows :: M.Map String [(Char, Int)], mOrder :: [String], mProbs :: M.Map String [(Char, Double)] }

data World = World
  { wLang    :: !Int                          -- 0 = 日本語、1 = 英語
  , wK       :: !Int                          -- 次数 k
  , wChars   :: String                        -- お手本の文字
  , wModels  :: [Model]                       -- k = 0〜4
  , wEnt     :: [Double]                      -- エントロピー率
  , wCopySet :: S.Set String
  , wCopyByK :: [Double]
  , wGen     :: String                        -- 生成した文
  , wShown   :: !Int                          -- 表示した文字数
  , wMask    :: [Bool]                        -- コピーの文字
  , wCopy    :: !Double
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-08 Character n-gram text" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure w { wShown = min (length (wGen w)) (wShown w + 3) }
  , skDraw  = draw
  , skEvent = event }

-- ★ はじめは英語（gloss で文字がそのまま読めるので）。T で日本語に
world0 :: World
world0 = loadCorpus (World 1 2 [] [] [] S.empty [] [] 0 [] 0 (rngOf 8) [])

corpusText :: Int -> String
corpusText lang = if lang == 0 then jaText else enText

loadCorpus :: World -> World
loadCorpus w =
  let ch = corpusText (wLang w)
      ms = [ buildNgram ch k | k <- [0 .. 4] ]
      cs = gramSet ch copyLen
      -- 次数ごとに 1000 文字ずつ試す
      (byK, g) = runR (mapM (\m -> do t <- generateText m ch 1000; pure (fst (copyCoverage t cs copyLen))) ms) (wRng w)
  in makeText w { wChars = ch, wModels = ms, wEnt = map ngramEntropy ms, wCopySet = cs, wCopyByK = byK, wRng = g }

makeText :: World -> World
makeText w =
  let (t, g) = runR (generateText (at' (wModels w) (wK w) (buildNgram (wChars w) 0)) (wChars w) genLen) (wRng w)
      (rate, mask) = copyCoverage t (wCopySet w) copyLen
  in w { wGen = t, wShown = 0, wMask = mask, wCopy = rate, wRng = g }

-- ★ 学習：文章を輪っか（最後の次は最初）として、直前 k 文字 → 次の文字 を数える。どの文脈にも必ず「次」がある
buildNgram :: String -> Int -> Model
buildNgram chars k =
  let n = length chars
      cyc = cycle chars
      grams = [ (take k (drop i cyc), at' (drop (i + k) cyc) 0 ' ') | i <- [0 .. n - 1] ]
      -- 数える（文脈と次の文字は、初めて出た順を覚えておく）
      (rows, order) = foldl' add (M.empty, []) grams
      add (m, ord0) (ctx, nx) = case M.lookup ctx m of
        Nothing -> (M.insert ctx [(nx, 1)] m, ctx : ord0)
        Just row -> (M.insert ctx (bump row) m, ord0)
        where bump r = if any ((== nx) . fst) r then [ (c, if c == nx then v + 1 else v) | (c, v) <- r ] else r ++ [(nx, 1)]
      probs = M.map (\row -> let tot = sum (map snd row)
                             in sortBy (comparing (negate . snd)) [ (c, i2d v / i2d tot) | (c, v) <- row ]) rows
  in Model k n rows (reverse order) probs

condProbs :: Model -> String -> [(Char, Double)]   -- P(次 | 文脈) を大きい順に
condProbs m ctx = M.findWithDefault [] ctx (mProbs m)

-- エントロピー率 H = −Σ_文脈 p(文脈) Σ_次 p(次|文脈) log₂ p(次|文脈)（輪っかなので p(文脈) は定常分布そのもの）
ngramEntropy :: Model -> Double
ngramEntropy m = foldl' rowH 0 [ M.findWithDefault [] c (mRows m) | c <- mOrder m ]
  where rowH h row = let tot = i2d (sum (map snd row))
                     in foldl' (\h2 (_, v) -> let p = i2d v / tot in h2 - (tot / i2d (mN m)) * p * logBase 2 p) h row

-- お手本のランダムな場所の k 文字から始めて、1 文字ずつサイコロ
generateText :: Model -> String -> Int -> R String
generateText m chars len = do
  let n = length chars
      k = mK m
  st <- rndInt 0 (n - 1)
  let start = take k (drop st (cycle chars))
      go !cnt rev
        | cnt >= len = pure (reverse rev)
        | otherwise = do
            let ctx = reverse (take k rev)
                pr = condProbs m ctx
            c <- if null pr then pure (headOr ' ' chars) else do i <- sampleFrom (map snd pr); pure (fst (at' pr i (' ', 0)))
            go (cnt + 1) (c : rev)
  out <- go 0 (reverse start)
  pure (drop k out)

gramSet :: String -> Int -> S.Set String
gramSet chars len = let n = length chars; cyc = cycle chars in S.fromList [ take len (drop i cyc) | i <- [0 .. n - 1] ]

-- お手本にある len 文字の並びに含まれる文字に印をつける。戻り値 (割合, 印)
copyCoverage :: String -> S.Set String -> Int -> (Double, [Bool])
copyCoverage gen set len =
  let n = length gen
      starts = [ i | (i, t) <- zip [0 .. n - len] (iterate (drop 1) gen), S.member (take len t) set ]
      marked = IM.fromList [ (i + j, ()) | i <- starts, j <- [0 .. len - 1] ]
      mask = [ IM.member i marked | i <- [0 .. n - 1] ]
  in (if n > 0 then i2d (IM.size marked) / i2d n else 0, mask)

-- 日本語は gloss で書けないので、文字コード（16 進 4 けた）で表す
codeOf :: Char -> String
codeOf c = let h = showHex (ord c) "" in replicate (4 - length h) '0' ++ h

-- 画面に出すときの 1 文字（英語のスペースは _）
showCh :: Int -> Char -> String
showCh lang c = if lang == 0 then codeOf c else if c == ' ' then "_" else [c]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Character n-gram: order k = " ++ show k ++ " (next char from the last " ++ show k ++ ")   " ++ (if lang == 0 then "Japanese" else "English"))
             ("Space new text   Up/Down order k   T English <-> Japanese   teal = copied run of " ++ show copyLen ++ "+ chars from the source")
  -- ---- 生成した文章（1 文字ずつ折り返す）
  , Color (col (46, 41, 38)) (rectTL (bx - 12) (by - 12) (bw + 24) (min (envH env - by - 100) (11 * lh + 24)))
  , Pictures [ say (if lang == 1 && c == ' ' then "" else showCh lang c) x y ts (col (if m then teal else cream))
             | (i, c, m) <- zip3 [0 :: Int ..] (take (wShown w) (wGen w)) (wMask w ++ repeat False)
             , let (r, cI) = i `divMod` perLine
             , r <= 10
             , let x = bx + i2f cI * adv
             , let y = by + i2f r * lh + ts ]
  , if lang == 0 then say "(gloss cannot draw Japanese: each character is shown as its Unicode code, e.g. 306e = no)" bx (by + 11 * lh + 8) 14 (col sub) else Blank
  , say ("Copy share (chars inside an " ++ show copyLen ++ "+ char run found in the source) = " ++ show (jsRoundI (wCopy w * 100)) ++ "%") bx ty 15 (col cream)
  , say "Copy share by order (1000 chars each)" bx (ty + 26) 14 (col sub)
  , Pictures [ Pictures [ Color (col (if kk == k then dice else (110, 100, 90))) (rectTL x2 (ty + 36) ((bw / 5 - 16) * f2 v) 14)
                        , say ("k = " ++ show kk ++ ": " ++ show (jsRoundI (v * 100)) ++ "%") x2 (ty + 68) 14 (col cream) ]
             | (kk, v) <- zip [0 :: Int ..] (wCopyByK w), let x2 = bx + i2f kk * (bw / 5) ]
  -- ---- 次数ごとのエントロピー率
  , say "Entropy rate H (bits / char)" px py 15 (col cream)
  , Pictures [ Pictures [ Color (col (if kk == k then dice else (110, 100, 90))) (rectTL (px + 50) yy (f2 (e / hmax) * barW) 20)
                        , sayLM ("k = " ++ show kk) px (yy + 10) 14 (col cream)
                        , sayLM (fmtN 2 e ++ "  ctx " ++ show (M.size (mRows m))) (px + 58 + f2 (e / hmax) * barW) (yy + 10) 14 (col cream) ]
             | (kk, e, m) <- zip3 [0 :: Int ..] (wEnt w) (wModels w), let yy = py + 14 + i2f kk * 30 ]
  , say (show nSym ++ " symbols -> uniform would be log2 " ++ show nSym ++ " = " ++ fmtN 2 (logBase 2 (i2d nSym)) ++ " bits") px (py + 175) 14 (col sub)
  , say "Higher k: less doubt (smaller H) = follows the source" px (py + 195) 14 (col sub)
  -- ---- 遷移表の一部：いまの文章の最後の k 文字のあとに来る文字
  , say ("Table row: after " ++ ctxStr ++ " comes?") px qy 15 (col cream)
  , Pictures [ Pictures [ Color (col teal) (rectTL (px + 56) yy (f2 p * (pw - 136)) 18)
                        , sayM (showCh lang c) (px + 22) (yy + 9) (if lang == 0 then 14 else 15) (col cream)
                        , sayLM (fmtN 2 p) (px + 64 + f2 p * (pw - 136)) (yy + 9) 14 (col cream) ]
             | (i, (c, p)) <- zip [0 :: Int ..] (take 8 pr), let yy = qy + 14 + i2f i * 26 ]
  , drawSelfTest env (wTest w) ]
  where
    lang = wLang w
    k = wK w
    bx = 40
    by = 100
    bw = envW env * 0.56
    ts = if lang == 0 then 15 else 19
    lh = 31
    adv = if lang == 0 then 52 else ts * 0.7
    perLine = max 1 (floor (bw / adv)) :: Int
    ty = min (envH env - 70) (by + 11 * lh + 44)
    px = envW env * 0.64
    py = 110
    pw = envW env - px - 40
    barW = pw - 190
    hmax = max 1 (at' (wEnt w) 0 1)
    nSym = M.size (mRows (at' (wModels w) 1 (buildNgram "" 1)))
    shownS = take (wShown w) (wGen w)
    ctx = if wShown w < k then "" else drop (length shownS - k) shownS
    pr = condProbs (at' (wModels w) k (buildNgram "" 0)) (if k == 0 then "" else ctx)
    ctxStr = if k == 0 then "(no context)" else if lang == 0 then "[" ++ unwords (map codeOf ctx) ++ "]" else "\"" ++ map (\c -> if c == ' ' then '_' else c) ctx ++ "\""
    qy = py + 235

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> makeText w
  EventKey (SpecialKey KeyUp) Down _ _ -> makeText w { wK = min 4 (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> makeText w { wK = max 0 (wK w - 1) }
  EventKey (Char c) Down _ _
    | c == 't' || c == 'T' -> loadCorpus w { wLang = 1 - wLang w }
  _ -> w

-- ---------- やってみよう ----------
-- 1. k = 0（文字の出る割合だけ）と k = 4 を比べよう。どこから「文章らしく」なる？
-- 2. enText を自分の書いた英語の文章に書きかえて、自分の文体のまねをさせよう
-- 3. お手本を 2 倍の長さにすると、同じ k でもコピーの割合は下がる？
-- 4. 文字ではなく「単語」を状態にすると？（words で単語の列にできる）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest = concatMap langChecks [("日本語", jaText), ("英語", enText)] ++
  [ ("k = 2 で生成した文の 3 文字の並びは、どれもお手本にある", allSeen)
  , ("k を上げるとコピーが増える（k = 1：" ++ show (jsRoundI (r1 * 100)) ++ "% → k = 4：" ++ show (jsRoundI (r4 * 100)) ++ "%）", r4 > r1 + 0.3)
  , ("「の」の次を 2 万回引いた割合 ≈ 遷移確率（ずれ < 0.015）", and [ abs (i2d (IM.findWithDefault 0 i cnt) / i2d nS - p) < 0.015 | (i, (_, p)) <- zip [0 ..] prNo ]) ]
  where
    langChecks (nm, tx) =
      let ms = [ buildNgram tx k | k <- [0 .. 4] ]
          okSum = and [ near (sumOf (map snd (condProbs m c))) 1 1e-12 | m <- ms, c <- mOrder m ]
          okNext = and [ M.member (take (mK m) g) (mRows m) | m <- ms, g <- S.toList (gramSet tx (mK m + 1)) ]
          hs = map ngramEntropy ms
          m1 = at' ms 1 (buildNgram tx 1)
          al = mOrder m1
          ix = M.fromList (zip al [0 ..])
          nA = length al
          tp = [ [ maybe 0 id (lookup d [ (dd, p) | (dd, p) <- condProbs m1 c ]) | d <- map head al ] | c <- al ]
          inv = 1 / i2d (length tx)
          freq = foldl' (\fr c -> modifyAt (M.findWithDefault 0 [c] ix) (+ inv) fr) (replicate nA 0) tx
      in [ (nm ++ "：どの文脈でも次の文字の確率の合計 = 1（k = 0〜4）", okSum)
         , (nm ++ "：どの文脈にも次の文字がある（輪っか）", okNext)
         , (nm ++ "：エントロピー率は k とともに減る（" ++ joinWith " ≥ " (map (fmtN 2) hs) ++ "）", and (zipWith (\a b -> b <= a + 1e-12) hs (drop 1 hs)))
         , (nm ++ "：k = 1 の文字の割合 π は π P = π を満たす", maxAbsDiffV (vecMat freq tp) freq < 1e-12)
         , (nm ++ "：k = 1 の H = 定常分布での −Σ π P log₂ P", near (entropyRate tp freq) (at' hs 1 0) 1e-10) ]
    ch = jaText
    s3 = gramSet ch 3
    cs8 = gramSet ch copyLen
    m1j = buildNgram ch 1
    prNo = condProbs m1j "の"
    nS = 20000 :: Int
    (allSeen, r1, r4, cnt) = withSeed 81 $ do
      g <- generateText (buildNgram ch 2) ch 2000
      let ok = and [ S.member (take 3 t) s3 | t <- take (length g - 2) (iterate (drop 1) g) ]
      t1 <- generateText m1j ch 3000
      t4 <- generateText (buildNgram ch 4) ch 3000
      c <- loopR nS (\m -> do i <- sampleFrom (map snd prNo); pure (IM.insertWith (+) i (1 :: Int) m)) IM.empty
      pure (ok, fst (copyCoverage t1 cs8 copyLen), fst (copyCoverage t4 cs8 copyLen), c)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

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

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
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

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

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

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

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

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

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

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
