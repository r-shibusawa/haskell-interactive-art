-- ============================================
-- 69-09 カリー-ハワード対応 ─ 「型 = 命題」「プログラム（λ 式）= 証明」。λ 式の型を推論すると、その導出の木は自然演繹の証明と同じ形
-- \x.M（関数を作る）= ->導入（仮定 x を外す）、M N（関数を使う）= ->除去、変数 = 仮定。\x.\y.x : A -> B -> A は p -> (q -> p) の証明。
-- ただし対応するのは直観主義論理：パースの法則 ((A->B)->A)->A や ~~A -> A は古典論理では恒真なのに、その型を持つ λ 式はない
-- （型 → 式の探索が有限の場合を全部調べて「ない」と確かめる。古典論理には継続（call/cc）のような道具が要る）。~A は A -> _|_ と書く
-- 操作：Tab 式 → 型 ⇔ 型 → 式 / ↑↓ お手本 / 式 → 型 では文字キーで λ 式を入力（08 と同じ：\ = λ、大文字 = I K S B C など）
--       Backspace 1 字消す / Shift + Backspace 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じる）
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

termPresets :: [String]                       -- ★ 式 → 型 のお手本
termPresets = ["K", "I", "S", "B", "C", "\\x.\\f.f x", "2", "\\f.\\g.\\x.g (f x)", "\\x.x x", "Y"]

typePresets :: [String]                       -- ★ 型 → 式 のお手本（大文字 1 字 = 型の変数、_|_ = 偽）
typePresets =
  [ "A -> B -> A", "(A -> B -> C) -> (A -> B) -> A -> C", "(A -> B) -> (B -> C) -> A -> C", "A -> (A -> _|_) -> _|_", "(A -> B) -> (B -> _|_) -> A -> _|_"
  , "((A -> B) -> A) -> A", "((A -> _|_) -> _|_) -> A", "A -> B" ]

searchMax :: Int
searchMax = 20000                             -- ★ 探索の上限（呼び出しの回数）

-- ---- 型：TV 型の変数 / TFn a b（a -> b）/ TBot（_|_）
data Ty = TV String | TFn Ty Ty | TBot deriving (Eq, Ord, Show)

type Subst = M.Map String Ty                  -- 型の変数 → 型（単一化で解いた答え）

tyStr :: Ty -> String
tyStr ty = case ty of
  TV n -> n
  TBot -> "_|_"
  TFn a b -> (case a of TFn _ _ -> "(" ++ tyStr a ++ ")"; _ -> tyStr a) ++ " -> " ++ tyStr b

-- 'A -> (B -> C)' → 型（-> は右から。→ ⊥ の記号でも読める）
tyParse :: String -> Ty
tyParse str = fst (arrow (toks str))
  where
    toks s = case s of
      [] -> []
      ('-' : '>' : r) -> "->" : toks r
      ('→' : r) -> "->" : toks r
      ('_' : '|' : '_' : r) -> "_|_" : toks r
      ('⊥' : r) -> "_|_" : toks r
      (c : r) | c `elem` "()" || (c >= 'A' && c <= 'Z') -> [c] : toks r
              | otherwise -> toks r
    atom ts = case ts of
      ("(" : r) -> let (t, r2) = arrow r in (t, drop 1 r2)
      ("_|_" : r) -> (TBot, r)
      (t : r) -> (TV t, r)
      [] -> (TV "?", [])
    arrow ts = let (a, r) = atom ts in case r of
      ("->" : r2) -> let (b, r3) = arrow r2 in (TFn a b, r3)
      _ -> (a, r)

tyWalk :: Ty -> Subst -> Ty
tyWalk ty s = case ty of
  TV n | Just t <- M.lookup n s -> tyWalk t s
  _ -> ty

tyOccurs :: String -> Ty -> Subst -> Bool
tyOccurs n ty s = case tyWalk ty s of
  TV m -> m == n
  TFn a b -> tyOccurs n a s || tyOccurs n b s
  TBot -> False

-- 型の単一化（出現検査つき）。失敗なら Nothing
tyUnify :: Ty -> Ty -> Subst -> Maybe Subst
tyUnify a0 b0 s =
  let a = tyWalk a0 s
      b = tyWalk b0 s
  in case (a, b) of
       (TV x, TV y) | x == y -> Just s
       (TV x, _) -> if tyOccurs x b s then Nothing else Just (M.insert x b s)
       (_, TV _) -> tyUnify b a s
       (TBot, TBot) -> Just s
       (TFn a1 a2, TFn b1 b2) -> tyUnify a1 b1 s >>= tyUnify a2 b2
       _ -> Nothing

tyResolve :: Ty -> Subst -> Ty
tyResolve ty s = case tyWalk ty s of
  TFn a b -> TFn (tyResolve a s) (tyResolve b s)
  t -> t

-- 型の変数を出てくる順に A, B, C, … に（10 個をこえたら Z11 など）
tyRename :: Ty -> Ty
tyRename ty = fst (go ty M.empty)
  where
    go u m = case u of
      TV n -> case M.lookup n m of
        Just v -> (TV v, m)
        Nothing -> let k = M.size m
                       v = if k < 10 then [at' "ABCDEFGHIJ" k 'Z'] else 'Z' : show (k + 1)
                   in (TV v, M.insert n v m)
      TFn a b -> let (a', m1) = go a m
                     (b', m2) = go b m1
                 in (TFn a' b', m2)
      TBot -> (TBot, m)

-- 導出の木の 1 段：規則（変数 = 仮定、λ = ->I、適用 = ->E）、式、型、上の段
data DRule = RVar | RAbs | RApp deriving (Eq)
data Deriv = Deriv { dRule :: DRule, dM :: Lam, dTy :: Ty, dKids :: [Deriv], dV :: String }

-- ★ 型の推論（ヒンドリー-ミルナーの let なし版）：導出の木を作りながら、型の方程式を単一化で解く
-- 返り値 Right (木, 名前をつけ直した型, 解) / Left 型がつかない理由
inferType :: Lam -> Either String (Deriv, Ty, Subst)
inferType m0 = do
  (d, (s, _)) <- go m0 [] (M.empty, 0 :: Int)
  pure (d, tyRename (tyResolve (dTy d) s), s)
  where
    go m ctx st@(s, fresh) = case m of
      LV n -> case lookup n ctx of
        Nothing -> Left ("free variable " ++ n ++ " has no type (no hypothesis)")
        Just t -> Right (Deriv RVar m t [] "", st)
      LL v b -> do
        let a = TV ('t' : show fresh)
        (body, st') <- go b ((v, a) : ctx) (s, fresh + 1)
        pure (Deriv RAbs m (TFn a (dTy body)) [body] v, st')
      LA f x -> do
        (df, st1) <- go f ctx st
        (dx, (s2, fr2)) <- go x ctx st1
        let r = TV ('t' : show fr2)
        case tyUnify (dTy df) (TFn (dTy dx) r) s2 of
          Nothing -> Left ("types do not fit: cannot apply " ++ clip 30 (lamStr f) ++ " to " ++ clip 30 (lamStr x) ++ " (infinite type or wrong shape)")
          Just s3 -> pure (Deriv RApp m r [df, dx] "", (s3, fr2 + 1))

-- ★ 型 → 式：直観主義の証明探索（目標が A -> B なら仮定 A を足して B を、A がただの変数なら仮定から A を作る）
-- 返り値 (見つけた式, 呼び出しの回数, 上限に当たったか)
data SS = SS { ssCalls :: !Int, ssNames :: !Int, ssHit :: !Bool }

searchTerm :: Ty -> (Maybe Lam, Int, Bool)
searchTerm goal = let (m, st) = go [] goal S.empty (SS 0 0 False) in (m, ssCalls st, ssHit st)
  where
    target ty = case ty of
      TFn a b -> let (as, h) = target b in (a : as, h)
      _ -> ([], ty)
    -- ctx は新しい仮定が先頭（p5.js 版の ctx を後ろから見るのと同じ順）
    go ctx g path st0 =
      let st = st0 { ssCalls = ssCalls st0 + 1 }
      in if ssCalls st > searchMax then (Nothing, st { ssHit = True }) else case g of
        TFn a b ->
          let k = ssNames st
              x = at' "xyzuvwpqrs" (k `mod` 10) 'x' : (if k >= 10 then show (k `div` 10) else "")
              (r, st') = go ((x, a) : ctx) b path st { ssNames = k + 1 }
          in (LL x <$> r, st')
        _ ->
          let key = (S.fromList (map snd ctx), g)
          in if S.member key path then (Nothing, st)   -- 同じ問いに戻ってきた（ループ）
             else tryHyps ctx g (S.insert key path) ctx st
    tryHyps ctx g path hs st = case hs of
      [] -> (Nothing, st)
      ((nm, t) : rest) ->
        let (args, hd) = target t
        in if hd /= g then tryHyps ctx g path rest st
           else case args1 ctx path args (LV nm) st of
             (Just term, st') -> (Just term, st')
             (Nothing, st') -> tryHyps ctx g path rest st'
    args1 ctx path as term st = case as of
      [] -> (Just term, st)
      (a : rest) -> case go ctx a path st of
        (Nothing, st') -> (Nothing, st')
        (Just sub, st') -> args1 ctx path rest (LA term sub) st'

tyToFml :: Ty -> F
tyToFml ty = case ty of
  TV n -> FVar (map toLowerC n)
  TBot -> FBot
  TFn a b -> FBin OImp (tyToFml a) (tyToFml b)
  where toLowerC c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- general の型の変数に何かを入れると goal になるか
tyMatches :: Ty -> Ty -> Bool
tyMatches general goal = maybe False (const True) (go general goal M.empty)
  where
    go g t b = case g of
      TV n -> case M.lookup n b of
        Just u -> if u == t then Just b else Nothing
        Nothing -> Just (M.insert n t b)
      TBot -> if t == TBot then Just b else Nothing
      TFn ga gb -> case t of
        TFn ta tb -> go ga ta b >>= go gb tb
        _ -> Nothing

-- 型 → 式 の結果（お手本ごと）
data Found = Found { fGoal :: Ty, fM :: Maybe Lam, fCalls :: !Int, fHit :: !Bool, fTaut :: !Bool, fItTy :: Maybe Ty }

findFor :: String -> Found
findFor s =
  let g = tyParse s
      (m, calls, hit) = searchTerm g
      it = m >>= \t -> either (const Nothing) (\(_, ty, _) -> Just ty) (inferType t)
  in Found g m calls hit (kindOf (tyToFml g) == Taut) it

-- 式 → 型 の結果：木の各段のラベル（木の型の変数は根から順に A, B, …）
data Cur = Cur { cM :: Lam, cRes :: Either String (Deriv, Ty, Subst) }

data World = World
  { wTermMode :: !Bool                        -- True = 式 → 型、False = 型 → 式
  , wSrc      :: String
  , wT        :: !Int                         -- 式のお手本の番号
  , wY        :: !Int                         -- 型のお手本の番号
  , wErr      :: String
  , wCur      :: Maybe Cur
  , wFound    :: Found
  , wAll      :: [Found]                      -- 型のお手本の全部（はじめに 1 回だけ探す）
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-09 Curry-Howard" (start world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (start world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World True "" 0 0 "" Nothing (findFor (headOr "" typePresets)) [] []

start :: World -> World
start w = (loadType 0 (loadTerm 0 w)) { wAll = map findFor typePresets }

loadTerm :: Int -> World -> World
loadTerm i w = let k = wrapI i (length termPresets) in rebuild w { wT = k, wSrc = at' termPresets k "" }

loadType :: Int -> World -> World
loadType i w = let k = wrapI i (length typePresets) in w { wY = k, wFound = findFor (at' typePresets k "") }

rebuild :: World -> World
rebuild w = case lamParse (wSrc w) of
  Left e -> w { wErr = errLine e }
  Right m -> w { wErr = "", wCur = Just (Cur m (inferType m)) }

-- ---- 証明の木（下が結論、上が前提）を描く
treeGap :: Float
treeGap = 64                                  -- 並んだ前提のあいだ（規則の名前が入る）

-- 木の各段の幅（ラベルの幅と、上の段の幅の合計の大きいほう）
data WTree = WTree { wtD :: Deriv, wtLab :: String, wtW :: !Float, wtKids :: [WTree] }

treeWidth :: (Deriv -> String) -> Deriv -> WTree
treeWidth lab d =
  let ks = map (treeWidth lab) (dKids d)
      s = lab d
      kw = sum (map wtW ks) + treeGap * i2f (length ks - 1)
  in WTree d s (max (textW s 14 + 20) kw) ks

-- x = 左はし、y = 結論の線の下（p5.js と同じ座標）。logic = 自然演繹の側か
drawProofTree :: Bool -> Bool -> WTree -> Float -> Float -> [Picture]
drawProofTree logic isRoot t x y =
  let d = wtD t
      cs = wtLab t
      ks = wtKids t
      kw = if null ks then 0 else sum (map wtW ks) + treeGap * i2f (length ks - 1)
      cx = x + wtW t / 2
      conc = sayC cs cx (y + 17) 14 (col (if isRoot then yel else ink))
      firstW = maybe 0 wtW (headMay ks)
      lastW = maybe 0 wtW (if null ks then Nothing else Just (lastOr t ks))
      lw = max (textW cs 14) (kw - (if null ks then 0 else firstW / 2 + lastW / 2) + 40) + 10
      ruleName = case dRule d of
        RAbs -> if logic then "->I " ++ dV d else "\\"
        _ -> if logic then "->E" else "app"
      kxs = scanl (\a k -> a + wtW k + treeGap) (x + (wtW t - max 0 kw) / 2) ks
  in if dRule d == RVar
       then [conc, sayC (if logic then "hyp " ++ (case dM d of LV n -> n; _ -> "") else "var") cx (y - 2) 14 (col dimc)]
       else [ conc, Color (col sky) (lineW 1.3 (cx - lw / 2, y) (cx + lw / 2, y))
            , sayLM ruleName (cx + lw / 2 + 4) y 14 (col sky) ]
            ++ concat [ drawProofTree logic False k kx (y - 40) | (k, kx) <- zip ks kxs ]
  where headMay xs = case xs of (a : _) -> Just a; [] -> Nothing

treeDepth :: Deriv -> Int
treeDepth d = 1 + maxOr 0 (map treeDepth (dKids d))

-- 木の型の変数の名前：根の型から、木を上から順に（A〜J、そのあとは Z）
renMap :: Deriv -> Subst -> M.Map String String
renMap d s = foldl' add M.empty (concatMap tvs (map (\t -> tyResolve t s) (dTy d : preorder d)))
  where
    preorder e = concatMap (\k -> dTy k : preorder k) (dKids e)
    tvs t = case t of
      TV n -> [n]
      TFn a b -> tvs a ++ tvs b
      TBot -> []
    add m n = if M.member n m then m else M.insert n [at' "ABCDEFGHIJ" (M.size m) 'Z'] m

renTy :: M.Map String String -> Subst -> Ty -> Ty
renTy nm s ty = go (tyResolve ty s)
  where go t = case t of
          TV n -> TV (M.findWithDefault n n nm)
          TFn a b -> TFn (go a) (go b)
          TBot -> TBot

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Curry-Howard: " ++ (if wTermMode w then "infer the type (proposition) of a term (program)" else "search a term (proof) for a type (proposition)"))
      "Tab term->type / type->term   Up/Down examples   (term->type) type a lambda term (\\ = lambda, I K S B C ...)\nBackspace delete   Shift+Backspace clear" ]
  ++ (if wTermMode w then drawTerm env w else drawType env w)
  ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)]

drawTerm :: Env -> World -> [Picture]
drawTerm env w =
  drawInputLine (envFrame env) 30 108 "Term:" (wSrc w) (wErr w) True : case wCur w of
    Nothing -> []
    Just c -> case cRes c of
      Left e ->
        [ say ("No type: " ++ e) 30 160 18 (col pink)
        , say "An untypable term is not a proof. \\x.x x needs x : A and x : A -> B, so A = A -> B (an infinite type)" 30 188 14 (col dimc) ]
      Right (tree, ty, s) ->
        let nm = renMap tree s
            ren = renTy nm s
            fm = tyToFml ty
            dep = treeDepth tree
            baseY = min (envH env - 120) (230 + i2f dep * 40)
            labT d = clip 26 (lamStr (dM d)) ++ " : " ++ tyStr (ren (dTy d))
            labL d = tyStr (ren (dTy d))
            wT = treeWidth labT tree
            wL = treeWidth labL tree
        in [ say (lamStr (cM c) ++ "  :  " ++ tyStr ty) 30 160 18 (col yel)
           , say ("As a formula: " ++ fmlStr fm ++ "  (" ++ kindName (kindOf fm) ++ ")") 30 186 14 (col dimc) ]
           ++ drawProofTree False True wT (30 + max 0 ((envW env * 0.5 - 40 - wtW wT) / 2)) baseY
           ++ drawProofTree True True wL (envW env * 0.55 + max 0 ((envW env * 0.43 - wtW wL) / 2)) baseY
           ++ [ say "Type derivation (program)" 30 214 15 (col sky), say "Natural deduction proof (proposition)" (envW env * 0.55) 214 15 (col sky)
              , say "Same shape: \\ = ->I (drop a hypothesis), application = ->E, variable = hypothesis" 30 (envH env - 60) 14 (col dimc) ]

drawType :: Env -> World -> [Picture]
drawType env w =
  let f = wFound w
      g = fGoal f
      res = case fM f of
        Just m ->
          [ say ("Found a term (proof): " ++ lamStr m) 30 190 20 (col grn)
          , say ("Its inferred type is " ++ maybe "?" tyStr (fItTy f) ++ "  (the goal is a special case of it)") 30 220 15 (col dimc) ]
        Nothing
          | fHit f -> [ say ("Hit the search limit (" ++ show searchMax ++ " calls): unknown") 30 190 20 (col yel) ]
          | otherwise ->
              [ say ("No term has this type (searched every case in " ++ show (fCalls f) ++ " calls)") 30 190 20 (col pink)
              , say (if fTaut f then "Classically valid, but not provable intuitionistically (typed lambda). Classical logic needs call/cc."
                     else "Not even a classical tautology, so of course there is no proof") 30 220 15 (col dimc) ]
      expl = [ "Search: for a goal A -> B, assume x : A and look for B (\\x. ...). For a type-variable goal, pick a hypothesis"
             , "'... -> goal' and search its arguments. Stop if the same goal comes back with the same hypotheses (a loop)."
             , "Hypotheses are parts of the goal, so there are finitely many questions and the search always ends." ]
  in [ say ("Type (proposition): " ++ tyStr g ++ "  (" ++ show (wY w + 1) ++ " / " ++ show (length typePresets) ++ ")") 30 110 20 (col yel)
     , say ("As a formula: " ++ fmlStr (tyToFml g) ++ "    classically (truth table): " ++ (if fTaut f then "tautology" else "not a tautology")) 30 140 15 (col dimc) ]
     ++ res
     ++ [ say ln 30 (274 + i2f i * 19) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] expl ]
     ++ concat [ [ say (at' typePresets k "") 60 (374 + i2f k * 26) 14 (col (if k == wY w then yel else ink))
                 , say (maybe "no term" (\m -> "term: " ++ lamStr m) (fM o)) 420 (374 + i2f k * 26) 14 (col (if fM o /= Nothing then grn else pink))
                 , say (if fTaut o then "classical: tautology" else "classical: not a tautology") 800 (374 + i2f k * 26) 14 (col dimc) ]
               | (k, o) <- zip [0 ..] (wAll w) ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TTab -> w { wTermMode = not (wTermMode w) }
    TUp -> if wTermMode w then loadTerm (wT w - 1) w else loadType (wY w - 1) w
    TDown -> if wTermMode w then loadTerm (wT w + 1) w else loadType (wY w + 1) w
    _ | not (wTermMode w) -> w
    TBack -> rebuild w { wSrc = dropLast (wSrc w) }
    TClear -> rebuild w { wSrc = "" }
    TChar ch -> rebuild w { wSrc = typeInto (wSrc w) ch lamKeys 120 }
    _ -> rebuild w
  _ -> w

-- ---------- やってみよう ----------
-- 1. K = \x.\y.x の 2 つの木を見くらべよう。「y を使わない」= 仮定 B を使わずに外す（->I）
-- 2. \f.\g.\x.g (f x) の型は？ 論理式として読むと「三段論法」(p->q)->(q->r)->(p->r)
-- 3. 型 → 式（Tab）で A -> B を選ぶと「ない」。古典論理でも恒真ではないので当然。パースの法則とのちがいは？
-- 4. ~~A -> A は証明できないが、A -> ~~A はできる（\x.\f.f x）。02 の自然演繹の課題 2 で背理法が要った理由
-- 5. typePresets に足そう：(A -> A -> B) -> A -> B、((A -> B) -> C) -> (A -> C) など。λ 式は見つかる？

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("I : A → A、K : A → B → A", ty "I" == Just (tyParse "A → A") && ty "K" == Just (tyParse "A → B → A"))
  , ("S : (A → B → C) → (A → B) → A → C", ty "S" == Just (tyParse "(A → B → C) → (A → B) → A → C"))
  , ("合成 B : (A → B) → (C → A) → C → B、C : (A → B → C) → B → A → C"
    , ty "B" == Just (tyParse "(A → B) → (C → A) → C → B") && ty "C" == Just (tyParse "(A → B → C) → B → A → C"))
  , ("チャーチ数 2 : (A → A) → A → A", ty "2" == Just (tyParse "(A → A) → A → A"))
  , ("λx.x x、Y、Ω には型がつかない（出現検査で失敗）", ty "\\x.x x" == Nothing && ty "Y" == Nothing && ty "OMEGA" == Nothing)
  , ("導出の木の全部の段が規則（λ = →I、適用 = →E）の形になっている（8 つの式）", okTree)
  , ("K・S の型、三段論法、A → ¬¬A には λ 式が見つかり、その式の型は目標に合う", okFind)
  , ("パースの法則と ¬¬A → A：全部調べても λ 式はない（上限には当たらない）が、真理値表では恒真"
    , let (pm, _, ph) = searchTerm (tyParse "((A → B) → A) → A")
          (dm, _, dh) = searchTerm (tyParse "((A → ⊥) → ⊥) → A")
      in pm == Nothing && not ph && dm == Nothing && not dh
         && kindOf (tyToFml (tyParse "((A → B) → A) → A")) == Taut && kindOf (tyToFml (tyParse "((A → ⊥) → ⊥) → A")) == Taut)
  , ("でたらめな型 150 個：見つけた λ 式（" ++ show nFound ++ " 個）は型が合い、その命題は古典論理でも恒真（健全）", sound && nFound > 10) ]
  where
    parsed s = either (const (LV "?")) id (lamParse s)
    ty s = either (const Nothing) (\(_, t, _) -> Just t) (inferType (parsed s))
    okTree = and [ either (const False) (\(d, _, s) -> chk s d) (inferType (parsed src)) | src <- ["I", "K", "S", "B", "C", "\\x.\\f.f x", "2", "SUCC"] ]
    chk s d =
      let t = tyResolve (dTy d) s
          here = case dRule d of
            RAbs -> case (t, dKids d) of
              (TFn _ b, [k]) -> tyResolve (dTy k) s == b
              _ -> False
            RApp -> case dKids d of
              [kf, kx] -> case tyResolve (dTy kf) s of
                TFn a b -> a == tyResolve (dTy kx) s && b == t
                _ -> False
              _ -> False
            RVar -> True
      in here && all (chk s) (dKids d)
    okFind = all (\s -> let g = tyParse s
                            (m, _, _) = searchTerm g
                        in case m of
                             Nothing -> False
                             Just t -> either (const False) (\(_, it, _) -> tyMatches it g) (inferType t))
                 ["A → B → A", "(A → B → C) → (A → B) → A → C", "(A → B) → (B → C) → A → C", "A → (A → ⊥) → ⊥"]
    -- でたらめな型（乱数は p5.js 版と同じ順に引く）
    rt :: Int -> R Ty
    rt d = do
      leaf <- if d <= 0 then pure True else (< 0.3) <$> rnd
      if leaf then do
        u <- rnd
        if u < 0.15 then pure TBot else TV <$> rndPick ["A", "B", "C"]
      else do a <- rt (d - 1); b <- rt (d - 1); pure (TFn a b)
    gs = withSeed 14 (mapM (const (rt 3)) [1 .. 150 :: Int])
    results = [ (g, searchTerm g) | g <- gs ]
    nFound = length [ () | (_, (Just _, _, False)) <- results ]
    sound = and [ ok | (g, (m, _, hit)) <- results
                , let ok = not hit && case m of
                        Nothing -> True
                        Just t -> either (const False) (\(_, it, _) -> tyMatches it g) (inferType t) && kindOf (tyToFml g) == Taut ]

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
