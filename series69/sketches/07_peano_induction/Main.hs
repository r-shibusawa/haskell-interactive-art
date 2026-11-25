-- ============================================
-- 69-07 ペアノ算術と数学的帰納法 ─ 数は 0 と「次の数」s だけで作る：1 = s(0)、2 = s(s(0))、…
-- 足し算と掛け算は 2 番目の数についての再帰で定義する（A1〜M2）。2 + 3 は書きかえ規則を 1 回ずつ使って s⁵(0) になる。
-- 「0 + n = n」は定義からすぐには出ない（定義は n + 0 = n）→ 数学的帰納法：n = 0 で成り立ち、n = k で成り立てば k + 1 でも成り立つ
-- 操作：1 足し算 2 掛け算 / ↑↓ 左の数 a / ← → 右の数 b / スペース 1 歩 / A 自動 / T 定理を選ぶ / D ドミノを倒す
-- （画面では × を * と書く）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (find, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

rules :: [(String, String, String)]           -- ★ 定義（X, Y は何の項でもよい）
rules =
  [ ("A1", "X + 0", "X"), ("A2", "X + s(Y)", "s(X + Y)")
  , ("M1", "X * 0", "0"), ("M2", "X * s(Y)", "X * Y + X") ]

-- ★ 帰納法の証明：(名前, 主張, 基底（n = 0）の等式の列, 帰納の段（n = k → s(k)）の等式の列, 帰納法の仮定)
--   理由 "IH" = 帰納法の仮定、"Thm 1" "Thm 2" = 前の定理
type Chain = [(String, String)]

theorems :: [(String, String, Chain, Chain, (String, String))]
theorems =
  [ ("Theorem 1", "0 + n = n", [("0 + 0", ""), ("0", "A1")], [("0 + s(k)", ""), ("s(0 + k)", "A2"), ("s(k)", "IH")], ("0 + k", "k"))
  , ("Theorem 2", "s(m) + n = s(m + n)", [("s(m) + 0", ""), ("s(m)", "A1"), ("s(m + 0)", "A1")]
    , [("s(m) + s(k)", ""), ("s(s(m) + k)", "A2"), ("s(s(m + k))", "IH"), ("s(m + s(k))", "A2")], ("s(m) + k", "s(m + k)"))
  , ("Theorem 3 (commutativity)", "m + n = n + m", [("m + 0", ""), ("m", "A1"), ("0 + m", "Thm 1")]
    , [("m + s(k)", ""), ("s(m + k)", "A2"), ("s(k + m)", "IH"), ("s(k) + m", "Thm 2")], ("m + k", "k + m")) ]

lemmaEq :: [(String, (String, String))]
lemmaEq = [("Thm 1", ("0 + X", "X")), ("Thm 2", ("s(X) + Y", "s(X + Y)"))]

maxRewrites :: Int
maxRewrites = 400                             -- ★ 書きかえの回数の上限

-- ---- 項：0 / s(a) / a + b / a * b / 変数（m, k など。大文字 X, Y は規則のパターン）
data T = Z | S T | Plus T T | Times T T | V Char deriving (Eq, Show)

data Domino = Domino { dN :: !Int, dOk :: !Bool, dAng :: !Float }

data World = World
  { wOp     :: !Char                          -- '+' か '*'
  , wA      :: !Int
  , wB      :: !Int
  , wTrace  :: [(T, String)]                  -- 書きかえの列 (項, 使った規則)
  , wShown  :: !Int                           -- 見せている行の数
  , wTh     :: !Int                           -- 定理の番号
  , wBad    :: (Int, Int)                     -- 定理の検査：まちがいの行（-1 = 全部正しい）
  , wDom    :: [Domino]
  , wPush   :: !Int                           -- ドミノを倒しはじめてからのフレーム（-1 = まだ）
  , wAuto   :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-07 Peano Arithmetic and Induction" (start []))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (start selfTest)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

start :: [(String, Bool)] -> World
start t = setTheorem 0 (rebuildTrace (World '+' 2 3 [] 1 0 (-1, -1) resetDominoes (-1) False t))

num :: Int -> T
num n = iterate S Z !! max 0 n

-- 'X * s(Y) + X' のような文字列 → 項（+ は * より弱い、左から）
pTerm :: String -> T
pTerm src = let (t, _) = sumP src in t
  where
    ws = dropWhile (== ' ')
    sumP s = let (a, r) = prodP s in loopS a (ws r)
    loopS a r = case r of
      ('+' : r') -> let (b, r2) = prodP r' in loopS (Plus a b) (ws r2)
      _ -> (a, r)
    prodP s = let (a, r) = atom s in loopP a (ws r)
    loopP a r = case r of
      ('*' : r') -> let (b, r2) = atom r' in loopP (Times a b) (ws r2)
      _ -> (a, r)
    atom s0 = case ws s0 of
      ('0' : r) -> (Z, r)
      ('s' : '(' : r) -> let (a, r2) = sumP r in (S a, drop 1 (ws r2))
      ('(' : r) -> let (a, r2) = sumP r in (a, drop 1 (ws r2))
      (c : r) -> (V c, r)
      [] -> (V '?', [])

tStr :: T -> String
tStr t = case t of
  Z -> "0"
  V c -> [c]
  S a -> "s(" ++ tStr a ++ ")"
  Plus a b -> bin "+" a b False
  Times a b -> bin "*" a b True
  where
    bin op a b isTimes =
      let sa = case a of Plus _ _ | isTimes -> "(" ++ tStr a ++ ")"; _ -> tStr a
          sb = case b of
            Plus _ _ -> "(" ++ tStr b ++ ")"
            Times _ _ | isTimes -> "(" ++ tStr b ++ ")"
            _ -> tStr b
      in sa ++ " " ++ op ++ " " ++ sb

tEq :: T -> T -> Bool
tEq a b = tStr a == tStr b

sCount :: T -> Int                            -- s...s(0) なら数、ちがえば -1
sCount t = go t 0
  where go u !n = case u of
          S a -> go a (n + 1)
          Z -> n
          _ -> -1

short :: T -> String                          -- s^5(0) のように短く
short t = let n = sCount t in if n > 3 then "s^" ++ show n ++ "(0)" else tStr t

-- ---- パターンの一致と書きかえ
type Bind = M.Map Char T

isPat :: Char -> Bool
isPat c = c >= 'A' && c <= 'Z'

-- pat の X, Y に何を入れれば t になるか
pmatch :: T -> T -> Bind -> Maybe Bind
pmatch pat t b = case (pat, t) of
  (V c, _) | isPat c -> case M.lookup c b of
    Just u -> if tEq u t then Just b else Nothing
    Nothing -> Just (M.insert c t b)
  (Z, Z) -> Just b
  (V c, V d) -> if c == d then Just b else Nothing
  (S p, S u) -> pmatch p u b
  (Plus p q, Plus u v) -> pmatch p u b >>= pmatch q v
  (Times p q, Times u v) -> pmatch p u b >>= pmatch q v
  _ -> Nothing

inst :: T -> Bind -> T
inst pat b = case pat of
  V c -> maybe pat id (M.lookup c b)
  Z -> Z
  S a -> S (inst a b)
  Plus x y -> Plus (inst x b) (inst y b)
  Times x y -> Times (inst x b) (inst y b)

ruleT :: [(String, T, T)]
ruleT = [ (nm, pTerm l, pTerm r) | (nm, l, r) <- rules ]

byRule :: T -> Maybe (T, String)
byRule t = headOr Nothing [ Just (inst r b, nm) | (nm, l, r) <- ruleT, Just b <- [pmatch l t M.empty] ]

-- 子を書きかえる（a が先、次に b）
inKids :: (T -> Maybe (T, String)) -> T -> Maybe (T, String)
inKids f t = case t of
  S a -> fmap (\(u, r) -> (S u, r)) (f a)
  Plus a b -> two Plus a b
  Times a b -> two Times a b
  _ -> Nothing
  where two k a b = case f a of
          Just (u, r) -> Just (k u b, r)
          Nothing -> fmap (\(u, r) -> (k a u, r)) (f b)

-- 内側・左から 1 か所だけ書きかえる
rewrite1 :: T -> Maybe (T, String)
rewrite1 t = case inKids rewrite1 t of
  Just o -> Just o
  Nothing -> byRule t

-- 外側・左から（自己テスト：順番を変えても答えは同じ）
rewriteOuter :: T -> Maybe (T, String)
rewriteOuter t = case byRule t of
  Just o -> Just o
  Nothing -> inKids rewriteOuter t

normTrace :: T -> Bool -> [(T, String)]
normTrace t0 outer = reverse (go [(t0, "")] 1 t0)
  where
    go acc !n t
      | n > maxRewrites = acc
      | otherwise = case (if outer then rewriteOuter else rewrite1) t of
          Nothing -> acc
          Just (u, r) -> go ((u, r) : acc) (n + 1) u

lastT :: [(T, String)] -> T
lastT tr = fst (lastOr (Z, "") tr)

mkOp :: Char -> T -> T -> T
mkOp op = if op == '+' then Plus else Times

rebuildTrace :: World -> World
rebuildTrace w = w { wTrace = normTrace (mkOp (wOp w) (num (wA w)) (num (wB w))) False, wShown = 1 }

-- ---- 証明の 1 歩の検査：右の項が、左の項のどこか 1 か所に規則（どちら向きでも）を使ったものか
oneStepSet :: T -> [(T, T)] -> [T]
oneStepSet t eqs =
  [ inst q b | (l, r) <- eqs, (p, q) <- [(l, r), (r, l)], Just b <- [pmatch p t M.empty] ]
  ++ case t of
       S a -> map S (oneStepSet a eqs)
       Plus a b -> map (`Plus` b) (oneStepSet a eqs) ++ map (Plus a) (oneStepSet b eqs)
       Times a b -> map (`Times` b) (oneStepSet a eqs) ++ map (Times a) (oneStepSet b eqs)
       _ -> []

-- 返り値：まちがいの行の番号（-1 = 全部正しい）
checkChain :: Chain -> (String, String) -> Int
checkChain chain (ihL, ihR) = headOr (-1) [ i | (i, prev, (s, why)) <- zip3 [1 ..] (map fst chain) (drop 1 chain), bad prev s why ]
  where
    bad prev s why =
      let p = pTerm prev
          c = pTerm s
          eqs | why == "IH" = Just [(pTerm ihL, pTerm ihR)]
              | Just (l, r) <- lookup why lemmaEq = Just [(pTerm l, pTerm r)]
              | otherwise = fmap (\(_, l, r) -> [(l, r)]) (find (\(nm, _, _) -> nm == why) ruleT)
      in case eqs of
           Nothing -> True
           Just e -> not (any (tEq c) (oneStepSet p e))

setTheorem :: Int -> World -> World
setTheorem k w =
  let (_, _, base, stp, ih) = at' theorems k ("", "", [], [], ("", ""))
  in w { wTh = k, wBad = (checkChain base ih, checkChain stp ih) }

resetDominoes :: [Domino]
resetDominoes = [ Domino n (sCount (lastT (normTrace (Plus Z (num n)) False)) == n) 0 | n <- [0 .. 13] ]

-- ---- 時間：自動の 1 歩と、ドミノの角度
step :: Env -> Float -> World -> IO World
step env _ w = pure (dominoStep (autoStep w))
  where
    autoStep w1 =
      if wAuto w1 && envFrame env `mod` 12 == 0
        then let s = min (length (wTrace w1)) (wShown w1 + 1) in w1 { wShown = s, wAuto = s /= length (wTrace w1) }
        else w1
    dominoStep w1 =
      let p = if wPush w1 >= 0 then wPush w1 + 1 else wPush w1
          upd i d = let target = if p >= 0 && p > i * 10 then 1 else 0
                    in d { dAng = dAng d + (target * 1.15 - dAng d) * 0.25 }
      in w1 { wPush = p, wDom = zipWith upd [0 ..] (wDom w1) }

draw :: Env -> World -> Picture
draw env w = view env (left ++ right ++ dominoes ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)])
  where
    wd = envW env
    ht = envH env
    opS = [wOp w]
    tr = wTrace w
    nT = length tr
    left =
      [ hudTitle ("Peano arithmetic and induction: " ++ show (wA w) ++ " " ++ opS ++ " " ++ show (wB w) ++ " by rewriting")
          "1 addition   2 multiplication   Up/Down a   Left/Right b   Space one step\nA auto   T next theorem   D push the dominoes"
      , say "Definitions (recursion on the 2nd number)" 30 112 15 (col yel) ]
      ++ [ say (nm ++ ": " ++ map nm' l ++ " = " ++ map nm' r) (30 + i2f (i `mod` 2) * 250) (138 + i2f (i `div` 2) * 22) 15 (col ink)
         | (i, (nm, l, r)) <- zip [0 :: Int ..] rules ]
      ++ [ say "Axioms: 0 is a number; if n is, so is s(n); s(n) != 0;" 30 186 14 (col dimc)
         , say "s(n) = s(m) -> n = m; and induction" 30 204 14 (col dimc)
         , say (show (wA w) ++ " " ++ opS ++ " " ++ show (wB w) ++ " = " ++ short (num (wA w)) ++ " " ++ opS ++ " " ++ short (num (wB w))
                ++ "  (" ++ show (nT - 1) ++ " rewrites)") 30 y0 15 (col yel) ]
      ++ concat [ [ say (if i == 0 then "start" else r) 30 y 14 (col dimc)
                  , say (clip 62 (tStr t)) 80 y (if length (tStr t) > 70 then 14 else 15) (col (if i == nT - 1 then grn else ink)) ]
                | i <- [first .. wShown w - 1], let (t, r) = at' tr i (Z, ""), let y = y0 + 26 + i2f (i - first) * lh ]
      ++ [ say ("= " ++ short (lastT tr) ++ " = " ++ show (sCount (lastT tr)) ++ "  (no rule applies = normal form)") 80
               (y0 + 26 + i2f (wShown w - first) * lh + 6) 16 (col grn) | wShown w == nT ]
    nm' c = case c of
      'X' -> 'n'
      'Y' -> 'm'
      _ -> c
    y0 = 232
    lh = min 24 ((ht - y0 - 60) / i2f (max 12 (min nT 22)))
    first = max 0 (wShown w - 20)
    -- ---- 右：帰納法の証明
    rx = wd * 0.58
    (thName, thClaim, base, stp, (ihL, ihR)) = at' theorems (wTh w) ("", "", [], [], ("", ""))
    (bad1, bad2) = wBad w
    chainPic ch y bad title =
      ( say title rx y 14 (col sky)
        : concat [ [ say ((if i > 0 then "= " else "  ") ++ s) (rx + 14) (y + 22 + i2f i * 21) 15 (col (if i == bad then pink else ink))
                   , if null why then Blank else say ("(" ++ why ++ ")") (rx + 260) (y + 22 + i2f i * 21) 14 (col dimc) ]
                 | (i, (s, why)) <- zip [0 :: Int ..] ch ]
      , y + 22 + i2f (length ch) * 21 )
    (pb, yb) = chainPic base 140 bad1 "Base n = 0"
    (ps, ye) = chainPic stp (yb + 40) bad2 "Step n = s(k)"
    okAll = bad1 < 0 && bad2 < 0
    right =
      [ say (thName ++ ": " ++ thClaim) rx 112 16 (col yel) ]
      ++ pb
      ++ [ say ("Induction hypothesis: " ++ ihL ++ " = " ++ ihR ++ " (assume for n = k)") rx (yb + 14) 14 (col dimc) ]
      ++ ps
      ++ (if okAll
            then [ say "OK: each line uses one rule / IH / theorem once," rx (ye + 14) 14 (col grn)
                 , say "machine-checked -> by induction true for every n" rx (ye + 33) 14 (col grn) ]
            else [ say "x some line does not pass the check" rx (ye + 14) 14 (col pink) ])
    -- ---- ドミノ
    dy = ht - 150
    dw = min 40 ((wd - rx - 40) / i2f (max 1 (length (wDom w))))
    dominoes =
      [ say "Dominoes: #n = \"0 + n = n\", each checked by rewriting." rx (dy - 56) 14 (col dimc)
      , say "D: the 1st falls = base, each knocks the next = step" rx (dy - 38) 14 (col dimc) ]
      ++ concat [ [ Translate (rx + 10 + i2f i * dw) (dy + 40) (Rotate (negate (dAng d) * 180 / pi)
                      (Pictures [ Color (col panelc) (rectTL 0 (-50) (dw * 0.35) 50)
                                , Color (col (if dOk d then grn else pink)) (frameTL 1.5 0 (-50) (dw * 0.35) 50) ]))
                  , sayC (show (dN d)) (rx + 10 + i2f i * dw + 4) (dy + 60) 14 (col ink) ]
                | (i, d) <- zip [0 :: Int ..] (wDom w) ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TChar '1' -> rebuildTrace w { wOp = '+' }
    TChar '2' -> rebuildTrace w { wOp = '*', wA = min (wA w) 4, wB = min (wB w) 4 }
    TUp -> rebuildTrace w { wA = min lim (wA w + 1) }
    TDown -> rebuildTrace w { wA = max 0 (wA w - 1) }
    TRight -> rebuildTrace w { wB = min lim (wB w + 1) }
    TLeft -> rebuildTrace w { wB = max 0 (wB w - 1) }
    TChar ' ' -> w { wShown = if wShown w >= length (wTrace w) then 1 else wShown w + 1 }
    TChar c
      | isKey 'a' c -> w { wShown = if wShown w >= length (wTrace w) then 1 else wShown w, wAuto = not (wAuto w) }
      | isKey 't' c -> setTheorem ((wTh w + 1) `mod` length theorems) w
      | isKey 'd' c -> w { wDom = map (\d -> d { dAng = 0 }) (wDom w), wPush = 0 }
    _ -> w
  _ -> w
  where lim = if wOp w == '+' then 8 else 4

-- ---------- やってみよう ----------
-- 1. 2 * 3 をスペースで 1 歩ずつ。M2 で「掛け算 → 1 つ少ない掛け算 + 足し算」になるのを追おう
-- 2. 定理 1「0 + n = n」の帰納の段で、仮定（0 + k = k）をどこで使っている？ 使わずに書けるだろうか
-- 3. 定理 3（交換法則）の証明は定理 1 と 2 を使っている。証明の中の "Thm 2" を "A2" に変えると、検査はどの行で x になる？
-- 4. rules に累乗を足そう：("P1", "X ^ 0", "s(0)"), ("P2", "X ^ s(Y)", "X ^ Y * X")（T と pTerm と tStr にも ^ を足す）
-- 5. ドミノが全部倒れるのは「1 枚目が倒れる」と「どの 1 枚が倒れても次が倒れる」の 2 つがそろうから。片方だけだと？

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("2 + 3 は書きかえ 4 回（A2 を 3 回、A1 を 1 回）で s⁵(0)"
    , sCount (lastT p) == 5 && length p - 1 == 4 && map snd (drop 1 p) == ["A2", "A2", "A2", "A1"])
  , ("2 × 3 は s⁶(0) になる", sCount (lastT m) == 6)
  , ("a, b = 0〜6 の全部の組（49 組）：a + b と a × b の正規形 = ふつうの計算", and [ okPair a b | a <- [0 .. 6], b <- [0 .. 6] ])
  , ("外側から書きかえても内側からでも同じ答え（a, b ≤ 4）"
    , and [ tEq (lastT (normTrace (Times (num a) (num b)) True)) (lastT (normTrace (Times (num a) (num b)) False)) | a <- [0 .. 4], b <- [0 .. 4] ])
  , ("0 + n = n を n = 0〜13 の全部で書きかえて確かめた（ドミノ。ただしこれは証明ではない）", all dOk resetDominoes)
  , ("3 つの帰納法の証明の全部の行が、規則・仮定・前の定理の 1 回の使用として検査に通る"
    , and [ checkChain b ih < 0 && checkChain s ih < 0 | (_, _, b, s, ih) <- theorems ])
  , ("まちがった行ははねる：0 + s(k) = s(k) を A2 だけで、s(0 + k) = s(k) を A1 で"
    , checkChain [("0 + s(k)", ""), ("s(k)", "A2")] ("0 + k", "k") == 1 && checkChain [("s(0 + k)", ""), ("s(k)", "A1")] ("0 + k", "k") == 1)
  , ("a + b の書きかえの回数はいつも b + 1（再帰は 2 番目の数について）"
    , all (\b -> length (normTrace (Plus (num 3) (num b)) False) - 1 == b + 1) [0, 1, 2, 5, 8]) ]
  where
    p = normTrace (Plus (num 2) (num 3)) False
    m = normTrace (Times (num 2) (num 3)) False
    okPair a b =
      let s = normTrace (Plus (num a) (num b)) False
          t = normTrace (Times (num a) (num b)) False
      in sCount (lastT s) == a + b && sCount (lastT t) == a * b && length t <= maxRewrites

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
