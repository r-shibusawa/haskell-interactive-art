-- ============================================
-- 66-04 正規表現から NFA へ ─ 正規表現を構文木にして、トンプソン構成で NFA を組み立てる
-- 部品（1 文字・つなげる・または・くり返し）ごとに「入口 1 つ・出口 1 つ」の小さな NFA を作り、ε の矢印でつなぐ。
-- テキストの一致は NFA の「玉の集合」で 1 文字ずつ調べる（一番左から、一番長い一致）。後もどり（バックトラック）しないので
-- 意地悪な式でも時間は「テキストの長さ × 状態の数」まで。後もどりする方法とくらべる
-- 操作：Tab = 正規表現 ⇔ テキストの切りかえ / 文字キー = いま選んでいる欄に入力（正規表現は 英数字 | * + ? ( )、
--       テキストは英数字と空白）/ Backspace 1 文字消す / ↑↓ お手本を変える（5 番目は意地悪な式）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (find, findIndex, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

presets :: [(String, String)]                 -- ★ お手本 (正規表現, テキスト)
presets =
  [ ("(a|b)*abb", "babb ab aabb abab abba bbabb")
  , ("colou?r", "color colour colouur colr")
  , ("go+gle", "ggle gogle google goooogle")
  , ("(ab|c)+d?", "abd cab ababcd xd ccc")
  , ("a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa") ]

btLimit :: Int
btLimit = 3000000                             -- ★ 後もどりする方法の手数の上限（これを超えたら打ち切り）

wordChars :: String
wordChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

data BT = BT { btOk :: Maybe Bool, btSteps :: !Int, btCut :: !Bool }   -- 後もどりの結果（Nothing = 打ち切り）

data World = World
  { wP       :: !Int                          -- お手本の番号
  , wPat     :: String                        -- 正規表現
  , wHay     :: String                        -- テキスト
  , wEditPat :: !Bool                         -- 正規表現の欄を編集中か
  , wAst     :: Maybe Rx
  , wNfa     :: NFA
  , wNfaPos  :: [Pos]
  , wMatches :: [(Int, Int)]                  -- 一致 [始め, 終わり)
  , wSteps   :: !Int                          -- NFA の手数
  , wBt      :: Maybe BT
  , wErr     :: String
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-04 Regex to NFA" (loadPreset envT 0 world0))
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (loadPreset env 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

envT :: Env                                   -- 1280 × 800 の画面（はじめ用）
envT = Env 1280 800 (0, 0) False 0 0 S.empty

world0 :: World
world0 = World 0 "" "" True Nothing (thompson REps) [] [] 0 Nothing "" []

loadPreset :: Env -> Int -> World -> World
loadPreset env k w =
  let n = length presets
      i = (k + n) `mod` n
      (p, h) = at' presets i ("", "")
  in rebuild env w { wP = i, wPat = p, wHay = h }

-- まちがいがあれば前の機械のまま
rebuild :: Env -> World -> World
rebuild env w = case parseRegex (wPat w) of
  Left (msg, at) -> w { wErr = msg ++ " (at character " ++ show (at + 1) ++ ")" }
  Right ast ->
    let nfa = thompson ast
        (ms, steps) = findAll nfa (wHay w)
        bt = if ' ' `notElem` wHay w && length (wHay w) <= 40 then Just (btFullMatch ast (wHay w)) else Nothing
        -- 後もどりの計算はこのイベントの中で終わらせる（描くときに持ちこさない）
        !_ = maybe () (\b -> btSteps b `seq` ()) bt
    in w { wErr = "", wAst = Just ast, wNfa = nfa
         , wNfaPos = layerLayout (nN nfa) (nStart nfa) (nfaEdges nfa) (envW env * 0.4 + 30) 170 (envW env * 0.6 - 90) (envH env * 0.42)
         , wMatches = forceL ms, wSteps = steps, wBt = bt }

-- ★ NFA で一番左・一番長い一致を全部さがす（玉の集合を 1 文字ずつ動かす）。steps = 動かした玉の数 + 1 の合計
findAll :: NFA -> String -> ([(Int, Int)], Int)
findAll m s = go 0 [] 0
  where
    n = length s
    accAny = any (\q -> at' (nAcc m) q False)
    go !i acc !steps
      | i > n = (reverse acc, steps)
      | otherwise =
          let cur0 = epsClosure m [nStart m]
              best0 = if accAny cur0 then i else -1
              scan _ [] best st _ = (best, st)
              scan j (c : cs) best st cur
                | null cur = (best, st)
                | otherwise = let cur' = nfaMove m cur c
                                  st' = st + length cur' + 1
                                  best' = if accAny cur' then j + 1 else best
                              in scan (j + 1) cs best' st' cur'
              (best, steps') = scan i (drop i s) best0 steps cur0
          in if best > i then go best ((i, best) : acc) steps' else go (i + 1) acc steps'

-- ★ くらべる相手：後もどりする方法（構文木を上から順に試し、だめなら 1 つ前の分かれ道へ戻る）。テキスト全体が一致するか
data Res = Res !Bool !Int | Cut !Int

btFullMatch :: Rx -> String -> BT
btFullMatch a s = case m a 0 (\j st -> Res (j == n) st) 0 of
    Res ok st -> BT (Just ok) st False
    Cut st -> BT Nothing st True
  where
    n = length s
    arr = listArray (0, max 0 (n - 1)) (if null s then " " else s) :: UArray Int Char
    charAt i = if i >= 0 && i < n then Just (arr ! i) else Nothing
    orElse r k2 = case r of
      Res False st -> k2 st
      _ -> r
    -- m 部品 位置 続き 手数
    m :: Rx -> Int -> (Int -> Int -> Res) -> Int -> Res
    m node i k st0 =
      let st = st0 + 1 in
      if st > btLimit then Cut st else
      case node of
        RChr c -> if charAt i == Just c then k (i + 1) st else Res False st
        REps -> k i st
        RCat x y -> m x i (\j st2 -> m y j k st2) st
        RAlt x y -> m x i k st `orElse` m y i k
        ROpt x -> m x i k st `orElse` k i
        RStar x -> loop x k i st
        RPlus x -> m x i (\j st2 -> loop x k j st2) st
    loop x k j0 st = m x j0 (\j st2 -> if j > j0 then loop x k j st2 else Res False st2) st `orElse` k j0   -- * と +（空回りはしない）

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Regex to NFA (Thompson construction)"
             ("Tab regex <-> text   type to edit   Backspace   Up/Down examples (" ++ show (wP w + 1) ++ " / " ++ show (length presets) ++ ")   operators | * + ? ( )")
  , Pictures [ Pictures [ Color (col panel) (rectTL 120 y (envW env - 160) 32)
                        , Color (col (if on then amber else wire)) (frameTL (if on then 2 else 1) 120 y (envW env - 160) 32)
                        , sayLM (lab ++ (if on then " >" else "")) 30 (y + 16) 15 (col (if on then amber else dimc))
                        , sayLM (s ++ (if on && envFrame env `mod` 60 < 30 then "|" else "")) 132 (y + 17) 18 (col ink) ]
             | (on, lab, s, y) <- [(wEditPat w, "regex", wPat w, 84), (not (wEditPat w), "text", wHay w, 124)] ]
  , if null (wErr w) then Blank else say ("error: " ++ wErr w ++ " (keeping the previous machine)") 132 178 14 (col rose) ]
  ++ case wAst w of
       Nothing -> []
       Just ast ->
         [ say (clip (floor ((envW env * 0.4 - 50) / (15 * 0.72 * 1.2))) ("Syntax tree: " ++ astString ast)) 30 200 15 (col ink)
         , drawTree ast 30 220 (envW env * 0.38 - 30) (envH env * 0.62 - 220)
         , say ("Thompson NFA: " ++ show (nN nfa) ++ " states (dashed = eps)") (envW env * 0.4) 200 15 (col ink)
         , drawNfa defOpt { oR = if nN nfa > 30 then 9 else if nN nfa > 16 then 12 else 16
                          , oNames = if nN nfa > 30 then Just (replicate (nN nfa) "") else if nN nfa > 16 then Just (map show [0 .. nN nfa - 1]) else Nothing
                          , oLabels = nN nfa <= 30                  -- 状態が多すぎるときは矢印のラベルを省く
                          , oLabelSize = 14, oNameSize = Just 14, oEdgeCol = (70, 110, 90) } nfa (wNfaPos w)
         -- ---- テキストの一致
         , say ("Matches (leftmost, longest): " ++ show (length ms) ++ "   " ++ fitList 70 [ "\"" ++ take (b - a) (drop a hay) ++ "\"" | (a, b) <- ms ]) 30 (ty - 14) 15 (col ink)
         , Pictures [ Pictures [ Color (col (maybe panel (\k -> if odd k then cyan else amber) inM)) (rectTL x ty (cw - 2) 34)
                               , sayM [c] (x + cw / 2 - 1) (ty + 17) (max 14 (min 18 (cw * 0.8))) (col (if inM == Nothing then ink else (10, 20, 15))) ]
                    | (i, c) <- zip [0 ..] hay, let x = 30 + i2f i * cw
                    , let inM = findIndex (\(a, b) -> i >= a && i < b) ms ]
         -- ---- 手数のくらべっこ
         , say ("NFA (set of tokens): " ++ show (wSteps w) ++ " steps for the whole search  <= length x number of states") 30 (ty + 66) 14 (col lime)
         , case wBt w of
             Just b -> say ("Backtracking (does the whole text match?): "
                            ++ (if btCut b then "stopped at " ++ show btLimit ++ " steps (it would take forever!)"
                                else show (btSteps b) ++ " steps, answer: " ++ (if btOk b == Just True then "match" else "no match")))
                           30 (ty + 90) 14 (col (if btCut b then rose else if btSteps b > 50 * max 1 (wSteps w) then amber else dimc))
             Nothing -> say "Compare with backtracking: use a text without spaces (example 5)" 30 (ty + 90) 14 (col dimc)
         , drawSelfTest env (wTest w) ]
  where
    nfa = wNfa w
    ms = wMatches w
    hay = wHay w
    ty = envH env * 0.7
    cw = min 26 ((envW env - 60) / i2f (max 1 (length hay)))

-- 長すぎる文字は n 文字で切る
clip :: Int -> String -> String
clip n t = if length t > n then take (n - 3) t ++ "..." else t

-- 並べた文字が長すぎるときは「…」で切る
fitList :: Int -> [String] -> String
fitList maxLen xs = go 0 xs
  where go _ [] = ""
        go l (y : ys) = if l + length y + 1 > maxLen then "..." else y ++ " " ++ go (l + length y + 1) ys

-- 構文木：葉の数で横幅を分ける（線を先に、丸をあとに）
drawTree :: Rx -> Float -> Float -> Float -> Float -> Picture
drawTree a x y w h = Pictures (rec a x 0)
  where
    kids n = case n of
      RCat p q -> [p, q]
      RAlt p q -> [p, q]
      RStar p -> [p]
      RPlus p -> [p]
      ROpt p -> [p]
      _ -> []
    leaves n = if null (kids n) then 1 else sum (map leaves (kids n)) :: Int
    depthOf n = 1 + maxOr 0 (map depthOf (kids n)) :: Int
    lw = w / i2f (leaves a)
    dy = min 46 (h / i2f (depthOf a))
    rec n x0 lvl =
      let cx = x0 + i2f (leaves n) * lw / 2
          cy = y + i2f lvl * dy + 14
          offs = scanl (\o k -> o + i2f (leaves k) * lw) x0 (kids n)
          sub = concat [ [ Color (col wire) (lineW 1.5 (cx, cy) (off + i2f (leaves k) * lw / 2, y + i2f (lvl + 1) * dy + 14)) ] ++ rec k off (lvl + 1)
                       | (k, off) <- zip (kids n) offs ]
          (lab, c) = case n of
            RChr ch -> ([ch], amber)
            REps -> ("e", dimc)
            RCat _ _ -> (".", cyan)
            RAlt _ _ -> ("|", cyan)
            RStar _ -> ("*", cyan)
            RPlus _ -> ("+", cyan)
            ROpt _ -> ("?", cyan)
          d = min 26 (lw * 0.9)
      in sub ++ [ Color (col c) (circleAt cx cy d)
                , if lw >= 18 then sayM lab cx (cy + 1) 14 (col (10, 20, 15)) else Blank ]   -- 細かすぎるときは記号を省く

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> loadPreset env (wP w - 1) w
  EventKey (SpecialKey KeyDown) Down _ _ -> loadPreset env (wP w + 1) w
  EventKey k Down _ _ -> case typedKey k of
    TTab -> w { wEditPat = not (wEditPat w) }
    TBack -> if wEditPat w then rebuild env w { wPat = dropLast (wPat w) } else rebuild env w { wHay = dropLast (wHay w) }
    TChar c
      | wEditPat w -> changed w { wPat = typeInto (wPat w) c (wordChars ++ "|*+?()") 50 }
      | otherwise -> changed w { wHay = typeInto (wHay w) c (wordChars ++ " ") 60 }
    _ -> w
  _ -> w
  where
    -- 文字が変わったときだけ作り直す（変わらなければ答えも同じ）
    changed w1 = if wPat w1 == wPat w && wHay w1 == wHay w then w else rebuild env w1

-- ---------- やってみよう ----------
-- 1. (a|b)*abb の NFA の状態の数を数えよう。1 文字につき 2 状態、| や * でさらに 2 状態ずつ増える
-- 2. 5 番目のお手本で、テキストの a を 1 つずつ増やすと、後もどりの手数は約 2 倍ずつ増える。NFA の手数は？
-- 3. 正規表現を 02 の部分集合構成にかけると DFA になる。(a|b)*abb の DFA は何状態？（答え：最小 4 状態）
-- 4. メールアドレスや郵便番号のような形を、使える記号だけで書いてみよう（[0-9] がないので (0|1|2|3|4|5|6|7|8|9) と書く）
-- 5. 「一番左・一番長い」の代わりに「一番左・一番短い」一致にするには findAll のどこを変える？

-- ---- 自己テスト
-- でたらめな正規表現：文字列と、同じ意味の構文木を一緒に作る（乱数は p5.js 版と同じ順に引く）
randomRegex :: Int -> R (String, Rx)
randomRegex d = do
  r <- rnd
  if d <= 0 || r < 0.3 then do c <- rndPick "abc"; pure ([c], RChr c)
  else if r < 0.5 then do (sa, a) <- randomRegex (d - 1); (sb, b) <- randomRegex (d - 1); pure (sa ++ sb, RCat a b)
  else if r < 0.65 then do (sa, a) <- randomRegex (d - 1); (sb, b) <- randomRegex (d - 1); pure ("(" ++ sa ++ "|" ++ sb ++ ")", RAlt a b)
  else do
    (sa, a) <- randomRegex (d - 1)
    op <- rndPick "*+?"
    pure ("(" ++ sa ++ ")" ++ [op], case op of { '*' -> RStar a; '+' -> RPlus a; _ -> ROpt a })

-- くらべる相手（参照の照合器）：構文木の部品ごとに「i 文字目から始めて、どこで終われるか」の位置の集合を計算する
refEnds :: String -> Rx -> Int -> S.Set Int
refEnds s = ends
  where
    n = length s
    ends node i = case node of
      RChr c -> if i < n && at' s i ' ' == c then S.singleton (i + 1) else S.empty
      REps -> S.singleton i
      RCat a b -> S.unions [ ends b j | j <- S.toList (ends a i) ]
      RAlt a b -> S.union (ends a i) (ends b i)
      ROpt a -> S.insert i (ends a i)
      RStar a -> closure a (S.singleton i) [i]
      RPlus a -> let firsts = ends a i in closure a firsts (S.toList firsts)
    closure a seen [] = seen
    closure a seen (j : todo) = let new = S.toList (ends a j `S.difference` seen) in closure a (foldl' (flip S.insert) seen new) (new ++ todo)

refMatch :: Rx -> String -> Bool                -- 全体が一致するか
refMatch a s = S.member (length s) (refEnds s a 0)

selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな正規表現 250 個 × 24 文字列：トンプソンの NFA の答え = 参照の照合器（位置の集合で調べる、全体一致）、" ++ show tot ++ " 回中ちがい " ++ show bad, bad == 0)
  , ("後もどりする方法の答えも参照の照合器と同じ（1500 回）", btBad == 0)
  , ("一番左・一番長い一致のさがし方 = 部分文字列を全部参照の照合器で調べた答え（40 回）", okFind)
  , ("まちがった式 (ab a) *a a** a|( ? は例外ではなく「まちがい」として返る", all (\s -> either (const True) (const False) (parseRegex s)) ["(ab", "a)", "*a", "a**", "a|(", "?"])
  , ("意地悪な式 (a?)^14 a^14：NFA は " ++ show fSteps ++ " 手、後もどりは " ++ show (btSteps b14) ++ " 手（100 倍以上）、答えは同じ"
    , btOk b14 == Just True && nfaAccepts m14 s14 && btSteps b14 > 100 * fSteps) ]
  where
    (results, finds) = withSeed 4 $ do
      rs <- mapM (\_ -> do
              (re, g) <- randomRegex 4
              ss <- mapM (\_ -> do l <- rndInt 0 7; rndString "abc" l) [1 .. 24 :: Int]
              pure (re, g, ss)) [1 .. 250 :: Int]
      fs <- mapM (\_ -> do
              (re, g) <- randomRegex 3
              s <- rndString "abc" 12
              pure (re, g, s)) [1 .. 40 :: Int]
      pure (rs, fs)
    checks = [ (nfaAccepts m s /= ref, j < 6 && btOk (btFullMatch p s) /= Just ref)
             | (re, g, ss) <- results, Right p <- [parseRegex re], let m = thompson p
             , (j, s) <- zip [0 :: Int ..] ss, let ref = refMatch g s ]
    tot = length checks
    bad = length (filter fst checks)
    btBad = length (filter snd checks)
    okFind = and [ fst (findAll (thompson p) s) == want g s | (re, g, s) <- finds, Right p <- [parseRegex re] ]
                 && length [ () | (re, _, _) <- finds, Right _ <- [parseRegex re] ] == 40
    want g s = go 0
      where n = length s
            go i | i > n = []
                 | otherwise = case find (\j -> refMatch g (take (j - i) (drop i s))) [n, n - 1 .. i + 1] of
                     Just b -> (i, b) : go b
                     Nothing -> go (i + 1)
    n14 = 14
    re14 = concat (replicate n14 "a?") ++ replicate n14 'a'
    ast14 = either (const REps) id (parseRegex re14)
    m14 = thompson ast14
    s14 = replicate n14 'a'
    fSteps = snd (findAll m14 s14)
    b14 = btFullMatch ast14 s14

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

-- ---- 正規表現の道具：再帰下降の構文解析 → 構文木 → トンプソン構成の NFA
-- 使える記号：文字（英字・数字）、| （または）、つなげる、* （0 回以上）、+ （1 回以上）、? （0 か 1 回）、( )
data Rx = RChr Char | REps | RCat Rx Rx | RAlt Rx Rx | RStar Rx | RPlus Rx | ROpt Rx deriving (Eq, Show)

-- 返り値 Right 構文木 または Left (まちがいの説明, 位置)（例外は投げない）
parseRegex :: String -> Either (String, Int) Rx
parseRegex src = case alt 0 of
    Left e -> Left e
    Right (a, i) -> if i < n then Left ("missing (", i) else Right a
  where
    n = length src
    peek i = if i < n then Just (at' src i ' ') else Nothing
    isChr c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    isOp c = c == '*' || c == '+' || c == '?'
    alt i = do
      (a, j) <- cat i
      altMore a j
    altMore a j = if peek j == Just '|'
      then do (b, k) <- cat (j + 1); altMore (RAlt a b) k
      else Right (a, j)
    cat i = go Nothing i
      where go acc j = case peek j of
              Just c | c /= '|' && c /= ')' -> do
                (r, k) <- rep j
                go (Just (maybe r (\a -> RCat a r) acc)) k
              _ -> Right (maybe REps id acc, j)
    rep i = do
      (a, j) <- atom i
      let (a', k) = case peek j of
            Just '*' -> (RStar a, j + 1)
            Just '+' -> (RPlus a, j + 1)
            Just '?' -> (ROpt a, j + 1)
            _ -> (a, j)
      case peek k of
        Just c | isOp c -> Left ("two operators in a row (use parentheses)", k)
        _ -> Right (a', k)
    atom i = case peek i of
      Just '(' -> do
        (a, j) <- alt (i + 1)
        if peek j /= Just ')' then Left ("missing )", j) else Right (a, j + 1)
      Just c | isChr c -> Right (RChr c, i + 1)
      Nothing -> Left ("the expression ends too early", i)
      Just c -> Left ("'" ++ [c] ++ "' cannot be here", i)

-- 構文木 → 文字列（かっこを補って）。ε は "e" と書く
astString :: Rx -> String
astString a = case a of
  RChr c -> [c]
  REps -> "e"
  RCat x y -> astString x ++ astString y
  RAlt x y -> "(" ++ astString x ++ "|" ++ astString y ++ ")"
  RStar x -> "(" ++ astString x ++ ")*"
  RPlus x -> "(" ++ astString x ++ ")+"
  ROpt x -> "(" ++ astString x ++ ")?"

-- トンプソン構成：部品ごとに「入口 1 つ・出口 1 つ」の NFA を作ってつなぐ（状態の番号は p5.js 版と同じ順）
data TB = TB { tbN :: !Int, tbTr :: IM.IntMap (M.Map Sym [Int]), tbAlpha :: S.Set Sym }

thompson :: Rx -> NFA
thompson ast =
  let ns tb = (tbN tb, tb { tbN = tbN tb + 1 })
      add p c q tb = tb { tbTr = IM.insertWith (\_ old -> M.insertWith (\_ o -> o ++ [q]) c [q] old) p (M.singleton c [q]) (tbTr tb) }
      build a tb0 =
        let (s, tb1) = ns tb0 in
        case a of
          RChr c -> let (f, tb2) = ns tb1 in ((s, f), add s c f tb2 { tbAlpha = S.insert c (tbAlpha tb2) })
          REps -> let (f, tb2) = ns tb1 in ((s, f), add s epsSym f tb2)
          RCat x y -> let ((x0, x1), tb2) = build x tb1
                          ((y0, y1), tb3) = build y tb2
                          tb4 = add x1 epsSym y0 (add s epsSym x0 tb3)
                          (f, tb5) = ns tb4
                      in ((s, f), add y1 epsSym f tb5)
          RAlt x y -> let ((x0, x1), tb2) = build x tb1
                          ((y0, y1), tb3) = build y tb2
                          (f, tb4) = ns tb3
                      in ((s, f), add y1 epsSym f (add x1 epsSym f (add s epsSym y0 (add s epsSym x0 tb4))))
          _ -> let x = case a of { RStar u -> u; RPlus u -> u; ROpt u -> u; _ -> a }
                   ((x0, x1), tb2) = build x tb1
                   (f, tb3) = ns tb2
                   tb4 = add x1 epsSym f (add s epsSym x0 tb3)
                   tb5 = case a of { RPlus _ -> tb4; _ -> add s epsSym f tb4 }       -- * と ? は 0 回でも OK
                   tb6 = case a of { ROpt _ -> tb5; _ -> add x1 epsSym x0 tb5 }      -- * と + はもう一度
               in ((s, f), tb6)
      ((s0, f0), tbE) = build ast (TB 0 IM.empty S.empty)
      n = tbN tbE
  in NFA n (S.toAscList (tbAlpha tbE)) [ IM.findWithDefault M.empty q (tbTr tbE) | q <- [0 .. n - 1] ] s0 [ q == f0 | q <- [0 .. n - 1] ]

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
