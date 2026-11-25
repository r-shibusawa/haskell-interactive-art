-- ============================================
-- 69-06 意味のタブロー ─ 「φ は恒真？」を、~φ（φ が偽になる）から出発して確かめる木
-- & のような式は 1 本の枝に 2 つ並べ（α 規則）、| のような式は枝を 2 つに分ける（β 規則）。枝の上に p と ~p が両方あれば
-- その枝は閉じる x（ありえない）。全部の枝が閉じたら ~φ はありえない ＝ φ は恒真。開いたまま終わった枝 o からは反例が読める
-- 操作：文字キー = 入力（00 と同じ）/ Backspace 1 字消す / Shift + Backspace全部消す / ↑↓ お手本
--       → 1 歩広げる / ← もどる / Enter 最後まで
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

presets :: [String]                           -- ★ お手本
presets =
  [ "((p -> q) -> p) -> p"
  , "(p -> q) -> (~q -> ~p)"
  , "(p | q) & ~p -> q"
  , "(p -> q) & (q -> r) -> (p -> r)"
  , "p -> q"
  , "p | q -> p"
  , "~(p & q) <-> ~p | ~q" ]

maxNodes :: Int
maxNodes = 300                                -- ★ 木の大きさの上限

-- ---- タブローのノード（番号 = 作った順）
data St = SOpen | SClosed | SDone | SBound | SInner deriving (Eq, Show)

data Node = Node
  { nF      :: F
  , nParent :: !Int                           -- 親の番号（根は -1）
  , nKids   :: [Int]
  , nUsed   :: IS.IntSet                      -- この枝でもう使った式（ノードの番号）
  , nStatus :: !St }

data Tab = Tab
  { tNodes   :: IM.IntMap Node
  , tSteps   :: !Int
  , tHit     :: !Bool                         -- 上限に当たった
  , tClosed  :: !Bool                         -- 全部の枝が閉じた
  , tOpen    :: Maybe Int }                   -- 開いた枝（もう使える式がない葉）

data World = World
  { wSrc   :: String
  , wP     :: !Int
  , wF0    :: Maybe F
  , wErr   :: String
  , wTab   :: Maybe Tab
  , wN     :: !Int                            -- いま何歩まで広げたか
  , wTotal :: !Int                            -- 最後までの歩数
  , wTaut  :: !Bool                           -- 真理値表の答え
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-06 Semantic Tableaux" (loadPreset 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadPreset 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World "" 0 Nothing "" Nothing 0 0 False []

loadPreset :: Int -> World -> World
loadPreset i w = let k = wrapI i (length presets) in rebuild w { wP = k, wSrc = at' presets k "" }

rebuild :: World -> World
rebuild w = case parseFormula (wSrc w) of
  Left e -> w { wErr = errLine e }
  Right f ->
    let full = buildTableau f maxBound maxNodes
    in w { wErr = "", wF0 = Just f, wTotal = tSteps full, wN = 0, wTab = Just (buildTableau f 0 maxNodes), wTaut = kindOf f == Taut }

-- ★ 規則：返り値 (α なら True（1 本に並べる）/ β なら False（2 つに分ける）, 式の組, 名前) / Nothing（文字か定数）
tabRule :: F -> Maybe (Bool, [[F]], String)
tabRule f = case f of
  FNot (FNot a) -> Just (True, [[a]], "~~")
  FBin OAnd a b -> Just (True, [[a, b]], "&")
  FNot (FBin OOr a b) -> Just (True, [[FNot a, FNot b]], "~|")
  FNot (FBin OImp a b) -> Just (True, [[a, FNot b]], "~->")
  FBin OOr a b -> Just (False, [[a], [b]], "|")
  FNot (FBin OAnd a b) -> Just (False, [[FNot a], [FNot b]], "~&")
  FBin OImp a b -> Just (False, [[FNot a], [b]], "->")
  FBin OIff a b -> Just (False, [[a, b], [FNot a, FNot b]], "<->")
  FNot (FBin OIff a b) -> Just (False, [[a, FNot b], [FNot a, b]], "~<->")
  _ -> Nothing

-- 根からその葉までのノードの番号
pathOf :: IM.IntMap Node -> Int -> [Int]
pathOf ns i0 = go i0 []
  where go i acc = case IM.lookup i ns of
          Just n -> go (nParent n) (i : acc)
          Nothing -> acc

-- 枝が閉じるか：_|_、~T、または A と ~A。返り値はぶつかったノード
closesWith :: IM.IntMap Node -> [Int] -> Maybe [Int]
closesWith ns path =
  let fs = [ (i, nF n) | i <- path, Just n <- [IM.lookup i ns] ]
      seen = S.fromList (map snd fs)
      hit (i, g) = case g of
        FBot -> Just [i]
        FNot FTop -> Just [i]
        FNot a | S.member a seen -> Just (i : [ j | Just (j, _) <- [find ((== a) . snd) fs] ])
        _ -> Nothing
  in case [ c | x <- fs, Just c <- [hit x] ] of
       (c : _) -> Just c
       [] -> Nothing

-- ★ タブローを maxSteps 回だけ広げる。各枝で「まだ使っていない式」を α 優先で 1 つ選んで規則を使う
buildTableau :: F -> Int -> Int -> Tab
buildTableau f maxSteps maxN =
  let root0 = Node (FNot f) (-1) [] IS.empty SOpen
      ns0 = IM.singleton 0 root0
      root = root0 { nStatus = if closesWith ns0 [0] /= Nothing then SClosed else SOpen }
      (ns, steps, hit) = loop (IM.singleton 0 root) 0
      leaves = [ (i, n) | (i, n) <- IM.toList ns, null (nKids n) ]
      closed = all ((== SClosed) . nStatus . snd) leaves
      openLeaf = fst <$> find ((== SDone) . nStatus . snd) leaves
  in Tab ns steps hit closed openLeaf
  where
    loop ns !steps
      | steps >= maxSteps = (ns, steps, False)
      | otherwise = case find (\(_, n) -> null (nKids n) && nStatus n == SOpen) (IM.toList ns) of
          Nothing -> (ns, steps, False)
          Just (li, leaf) ->
            let path = pathOf ns li
                cand = [ (i, r) | i <- path, not (IS.member i (nUsed leaf)), Just n <- [IM.lookup i ns], Just r <- [tabRule (nF n)] ]
                pick = case filter (\(_, (a, _, _)) -> a) cand of
                  (c : _) -> Just c
                  [] -> case cand of
                    (c : _) -> Just c
                    [] -> Nothing
            in case pick of
                 Nothing -> loop (IM.insert li leaf { nStatus = SDone } ns) steps   -- もう使える式がない ＝ 開いた枝（反例）
                 Just (pk, (_, parts, _))
                   | IM.size ns >= maxN -> (IM.insert li leaf { nStatus = SBound } ns, steps, True)
                   | otherwise ->
                       let used = IS.insert pk (nUsed leaf)
                           ns' = foldl' (addPart li used) ns parts
                           ns'' = IM.map (\n -> if null (nKids n) then n else n { nStatus = SInner }) ns'
                       in loop ns'' (steps + 1)
    -- 1 つの組を葉の下に縦に並べ、最後のノードで閉じるか調べる
    addPart li used ns part =
      let (ns1, lastI) = foldl' (\(m, par) g ->
                                   let i = IM.size m
                                       nd = Node g par [] used SOpen
                                       m' = IM.adjust (\p -> p { nKids = nKids p ++ [i] }) par (IM.insert i nd m)
                                   in (m', i)) (ns, li) part
      in case closesWith ns1 (pathOf ns1 lastI) of
           Just _ -> IM.adjust (\n -> n { nStatus = SClosed }) lastI ns1
           Nothing -> ns1

-- 開いた枝の上の文字から割り当てを作る（出てこない変数は 0）
counterModel :: IM.IntMap Node -> Int -> F -> Asg
counterModel ns leaf f =
  let base = M.fromList [ (v, False) | v <- fmlVars f ]
  in foldl' (\e i -> case nF <$> IM.lookup i ns of
                       Just (FVar v) -> M.insert v True e
                       _ -> e) base (pathOf ns leaf)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Semantic tableaux: start from ~phi; if every branch closes, phi is valid"
      "Type a formula   Backspace delete   Shift+Backspace clear   Up/Down examples\nRight 1 step   Left back   Enter to the end"
  , drawInputLine (envFrame env) 30 120 "Formula phi:" (wSrc w) (wErr w) True ]
  ++ (case (wTab w, wF0 w) of
        (Just t, Just f0) -> drawTab env w t f0
        _ -> [])
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

drawTab :: Env -> World -> Tab -> F -> [Picture]
drawTab env w t f0 =
  let ns = tNodes t
      lay = layoutTree 0 (\i -> maybe [] nKids (IM.lookup i ns)) maxNodes
      x0 = 50
      wd = envW env - 100
      y0 = 172
      h = envH env - 300
      dy = if tlDepth lay > 0 then min 38 (h / i2f (tlDepth lay)) else 0
      ts = max 14 (min 16 (min (dy * 0.5) ((wd / i2f (max 1 (tlLeaves lay))) / 7)))
      px o = x0 + tnX o * wd
      py o = y0 + i2f (tnDepth o) * dy
      edges = [ lineW 1.2 (px p, py p + ts * 0.6) (px o, py o - ts * 0.6) | o <- tlNodes lay, Just p <- [IM.lookup (tnParent o) byIdx] ]
      byIdx = IM.fromList (zip [0 ..] (tlNodes lay))
      nodePic o = case IM.lookup (tnNode o) ns of
        Nothing -> []
        Just n ->
          let s = fmlStr (nF n)
              c = if tnNode o == 0 then yel else if isLitT (nF n) then sky else ink
              tw = textW s ts + 8
              mark = case nStatus n of
                SClosed -> "x"
                SDone -> "o"
                SBound -> "..."
                _ -> ""
              mc = case nStatus n of
                SClosed -> pink
                SDone -> grn
                _ -> yel
          in [ Color (col bgc) (rectTL (px o - tw / 2) (py o - ts * 0.7) tw (ts * 1.4))
             , sayM s (px o) (py o) ts (col c) ]
             ++ [ sayM mark (px o) (py o + ts * 1.3) (ts + 2) (col mc) | null (nKids n), not (null mark) ]
      by = envH env - 110
      tt = wTaut w
      verdict
        | tClosed t = say "All branches closed (x) -> ~phi is impossible -> phi is VALID" 30 by 20 (col grn)
        | Just l <- tOpen t =
            let e = counterModel ns l f0
            in say (clip 80 ("Open branch (o) left -> counterexample: " ++ commaSep [ v ++ " = " ++ (if b then "1" else "0") | (v, b) <- M.toList e ]
                     ++ " (then phi is " ++ (if evalFml f0 e then "true!?" else "false") ++ ")")) 30 by 20 (col pink)
        | tHit t = say ("Hit the limit of " ++ show maxNodes ++ " nodes") 30 by 20 (col yel)
        | otherwise = say ("Expanding: " ++ show (wN w) ++ " / " ++ show (wTotal w) ++ " steps (Right to continue)") 30 by 20 (col ink)
      agreeS = if tClosed t || tOpen t /= Nothing
                 then (if tClosed t == tt then "agrees OK (sound: closes => valid, complete: valid => closes)" else "DISAGREES")
                 else "tableau not finished"
  in [ Color (col linec) (Pictures edges) ] ++ concatMap nodePic (tlNodes lay)
     ++ [ verdict
        , say ("Truth table: " ++ (if tt then "valid" else "not valid") ++ "  ->  " ++ agreeS) 30 (by + 28) 14 (col dimc)
        , say ("alpha (same branch): & ~| ~-> ~~   beta (split): | ~& -> <-> ~<->   alpha first = smaller tree.  nodes "
               ++ show (IM.size ns)) 30 (by + 52) 14 (col dimc) ]
  where commaSep = foldr1' (\a b -> a ++ ", " ++ b)
        foldr1' g xs = if null xs then "" else foldr1 g xs

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TUp -> loadPreset (wP w - 1) w
    TDown -> loadPreset (wP w + 1) w
    TRight | Just f <- wF0 w -> stepTo f (clampI (wN w + 1) 0 (wTotal w))
    TLeft | Just f <- wF0 w -> stepTo f (clampI (wN w - 1) 0 (wTotal w))
    TEnter | Just f <- wF0 w -> stepTo f (wTotal w)
    TBack -> rebuild w { wSrc = dropLast (wSrc w) }
    TClear -> rebuild w { wSrc = "" }
    TChar ch -> rebuild w { wSrc = typeInto (wSrc w) ch fmlKeys 80 }
    _ -> rebuild w
  _ -> w
  where stepTo f n = w { wN = n, wTab = Just (buildTableau f n maxNodes) }

-- ---------- やってみよう ----------
-- 1. パースの法則を → で 1 歩ずつ。~(A -> B) は A と ~B を並べる（α）。最後に p と ~p がそろって閉じる
-- 2. p | q -> p は閉じない。開いた枝 o を根までたどって、反例の割り当てを読もう
-- 3. ~(p & q) <-> ~p | ~q は <-> で枝が 2 つに分かれる。真理値表（00）と同じ答えになるのを確かめよう
-- 4. buildTableau の中で β 規則を先に使うようにすると、木の大きさはどう変わる？（ノードの数をくらべる）
-- 5. 一階述語論理のタブローでは Ax の式に何度でも新しい値を入れられるので、木が止まらないことがある（上限が要る理由）

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("でたらめな式 " ++ show nT ++ " 個（恒真 " ++ show nValid ++ " 個）：タブローが閉じる ⇔ 真理値表で恒真（上限に当たった " ++ show bound ++ " 個を除く）"
    , agree == nT - bound && bound == 0 && nValid > 10)
  , ("恒真でない式では、開いた枝から読んだ割り当てで φ が本当に偽になる（反例）", cmOk)
  , ("閉じた枝には必ず A と ¬A（または ⊥）が両方ある", clashOk)
  , ("パースの法則は閉じる、p → q は開いて反例 p = 1, q = 0"
    , tClosed pe && not (tClosed pq) && case tOpen pq of
        Just l -> counterModel (tNodes pq) l (fm "p -> q") == M.fromList [("p", True), ("q", False)]
        Nothing -> False)
  , ("大きな式で上限（40 ノード）にすると「上限に当たった」と報告する", tHit big && IM.size (tNodes big) <= 42)
  , ("(p ∧ q) → p は α 規則だけで閉じる（枝分かれなし：¬φ、p ∧ q、¬p、p、q の 5 個）"
    , tClosed st && IM.size (tNodes st) == 5 && all ((<= 1) . length . nKids) (IM.elems (tNodes st))) ]
  where
    nT = 300 :: Int
    fs = withSeed 17 (mapM (\_ -> do d <- rndInt 2 4; randomFml d ["p", "q", "r", "s"] True) [1 .. nT])
    res = [ (f, buildTableau f maxBound 2000) | f <- fs ]
    bound = length [ () | (_, t) <- res, tHit t ]
    ok = [ (f, t) | (f, t) <- res, not (tHit t) ]
    nValid = length [ () | (f, _) <- ok, kindOf f == Taut ]
    agree = length [ () | (f, t) <- ok, tClosed t == (kindOf f == Taut) ]
    cmOk = and [ case tOpen t of
                   Just l -> not (evalFml f (counterModel (tNodes t) l f))
                   Nothing -> False
               | (f, t) <- ok, not (tClosed t) ]
    clashOk = and [ closesWith (tNodes t) (pathOf (tNodes t) i) /= Nothing | (_, t) <- ok, (i, n) <- IM.toList (tNodes t), nStatus n == SClosed ]
    fm s = either (const FTop) id (parseFormula s)
    pe = buildTableau (fm "((p -> q) -> p) -> p") maxBound maxNodes
    pq = buildTableau (fm "p -> q") maxBound maxNodes
    big = buildTableau (fm "(a<->b)<->((c<->d)<->(e<->f))") maxBound 40
    st = buildTableau (fm "(p & q) -> p") maxBound maxNodes

isLitT :: F -> Bool                           -- 文字か、その否定（空色で描く）
isLitT f = case f of
  FVar _ -> True
  FNot (FVar _) -> True
  _ -> False

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
