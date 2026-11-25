-- ============================================
-- 69-05 単一化と論理プログラミング ─ 家系図の小さな Prolog。質問の目標を、規則の頭と「単一化」（同じ形にする代入を探す）して
-- 体の目標に置きかえていく（SLD 導出）。目標がなくなれば []（空節）＝ 答え。どの規則を使うかで枝分かれする木を、深さ優先で探す。
-- 単一化はロビンソンの方法：同じ記号なら中身どうしを、変数なら代入。X = f(X) は「出現検査」で失敗（無限の項になってしまう）
-- 操作：↑↓ 質問を選ぶ / スペース 探索の一時停止・再開 / ← → 木のノードを選ぶ / U 単一化のお手本を次へ / A 一度に全部
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

program :: [String]                           -- ★ プログラム（事実と規則）。大文字で始まるのが変数
program =
  [ "parent(tom, bob)", "parent(tom, liz)", "parent(bob, ann)", "parent(bob, pat)", "parent(pat, jim)"
  , "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)"
  , "ancestor(X, Y) :- parent(X, Y)"
  , "ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)"
  , "anc_left(X, Y) :- anc_left(X, Z), parent(Z, Y)"      -- 左再帰：深さ優先では止まらない
  , "anc_left(X, Y) :- parent(X, Y)" ]

queries :: [String]
queries = ["parent(tom, X)", "grandparent(tom, W)", "ancestor(tom, D)", "ancestor(A, jim)", "grandparent(X, Y)", "ancestor(jim, X)", "anc_left(tom, D)"]

unifyEx :: [(String, String)]
unifyEx = [("f(X, g(Y))", "f(a, g(X))"), ("g(X, f(Y))", "g(f(Z), X)"), ("h(X, Y, Z)", "h(Y, Z, a)"), ("p(X, X)", "p(a, b)"), ("X", "f(X)"), ("f(X, a)", "g(X, a)")]

maxDepth, maxNodes, perFrame :: Int
maxDepth = 10                                 -- ★ 探索の深さの上限
maxNodes = 200                                -- ★ ノードの数の上限
perFrame = 2                                  -- ★ 1 フレームに広げるノードの数

-- ---- 項：TV "X" 変数 / TF "parent" [引数]（引数が空なら定数）
data Term = TV String | TF String [Term] deriving (Eq, Show)

data Clause = Clause { clHead :: Term, clBody :: [Term], clSrc :: String }

type Sub = M.Map String Term                  -- 代入（変数 → 項）

data Status = Open | Inner | Success | Fail | Bound deriving (Eq, Show)

-- SLD の木のノード（番号 = 作った順）
data SNode = SNode
  { nGoals  :: [Term]
  , nSub    :: Sub
  , nDepth  :: !Int
  , nKids   :: [Int]
  , nStatus :: !Status
  , nVia    :: !Int                           -- 使った規則の番号（根は −1）
  , nParent :: !Int }

data SLD = SLD
  { sNodes  :: IM.IntMap SNode
  , sStack  :: [Int]                          -- 先頭がスタックの上
  , sRename :: !Int                           -- 名前を新しくした回数（X → X_7）
  , sQVars  :: [String] }                     -- 質問の変数

data World = World
  { wQ      :: !Int                           -- 質問の番号
  , wSld    :: SLD
  , wPaused :: !Bool
  , wSel    :: !Int                           -- 選んだノード
  , wU      :: !Int                           -- 単一化のお手本の番号
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-05 Unification and Prolog" (startQuery 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (startQuery 0 world0) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPaused w then w else w { wSld = iterN perFrame (wSld w) })
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World 0 (newSld 0) False 0 0 []

clauses :: [Clause]
clauses = map parseClause program

-- ---- 項の読み書き
parseTerm :: String -> Maybe Term
parseTerm s0 = case one s0 of
  Just (t, r) | null (ws r) -> Just t
  _ -> Nothing
  where
    ws = dropWhile (== ' ')
    nameCh c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'
    one s = let s1 = ws s
                name = takeWhile nameCh s1
                r = ws (drop (length name) s1)
            in case name of
                 [] -> Nothing
                 (c : _) | (c >= 'A' && c <= 'Z') || c == '_' -> Just (TV name, drop (length name) s1)
                 _ -> case r of
                   ('(' : r1) -> args name [] r1
                   _ -> Just (TF name [], r)
    args name acc s = do
      (a, r) <- one s
      case ws r of
        (',' : r2) -> args name (acc ++ [a]) r2
        (')' : r2) -> Just (TF name (acc ++ [a]), r2)
        _ -> Nothing

-- 「頭 :- 体」を読む（体はかっこの外のカンマで分ける）
parseClause :: String -> Clause
parseClause s =
  let (h, b) = breakOn ":-" s
      hd = maybe (TF "?" []) id (parseTerm (trim h))
      body = if null b then [] else [ maybe (TF "?" []) id (parseTerm (trim x)) | x <- splitTop (drop 2 b) ]
  in Clause hd body s
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    breakOn pat str = case str of
      [] -> ([], [])
      _ | take (length pat) str == pat -> ([], str)
      (c : r) -> let (a, z) = breakOn pat r in (c : a, z)
    splitTop str = go (0 :: Int) "" str
      where go _ cur [] = [reverse cur]
            go d cur (c : r)
              | c == ',' && d == 0 = reverse cur : go d "" r
              | c == '(' = go (d + 1) (c : cur) r
              | c == ')' = go (d - 1) (c : cur) r
              | otherwise = go d (c : cur) r

termStr :: Term -> String
termStr t = case t of
  TV v -> v
  TF f [] -> f
  TF f as -> f ++ "(" ++ joinWith ", " (map termStr as) ++ ")"

walk :: Term -> Sub -> Term
walk t s = case t of
  TV v | Just u <- M.lookup v s -> walk u s
  _ -> t

resolve :: Term -> Sub -> Term
resolve t0 s = go t0 0
  where go t d = case walk t s of
          TV v -> TV v
          TF f as -> if d > (50 :: Int) then TF "..." [] else TF f [ go a (d + 1) | a <- as ]

occurs :: String -> Term -> Sub -> Bool
occurs v t s = case walk t s of
  TV u -> u == v
  TF _ as -> any (\a -> occurs v a s) as

-- ★ ロビンソンの単一化：等式のリストを 1 つずつ片づける。返り値 (代入（失敗なら Nothing）, 説明の行)
unify :: Term -> Term -> Sub -> Bool -> (Maybe Sub, [String])
unify a b s0 occursCheck = go [(a, b)] s0 []
  where
    go [] s steps = (Just s, reverse steps)
    go ((x0, y0) : todo) s steps =
      let x1 = walk x0 s
          y1 = walk y0 s
          (x, y) = case (x1, y1) of
            (TF _ _, TV _) -> (y1, x1)
            _ -> (x1, y1)
      in case (x, y) of
           (TV u, TV v) | u == v -> go todo s steps
           (TV u, _)
             | occursCheck && occurs u y s ->
                 (Nothing, reverse (("occurs check: " ++ u ++ " is inside " ++ termStr (resolve y s) ++ " -> fail") : steps))
             | otherwise -> let s' = M.insert u y s
                            in go todo s' (("bind " ++ u ++ " := " ++ termStr (resolve y s')) : steps)
           (TF f xs, TF g ys)
             | f /= g || length xs /= length ys ->
                 (Nothing, reverse (("symbols differ: " ++ f ++ "/" ++ show (length xs) ++ " and " ++ g ++ "/" ++ show (length ys) ++ " -> fail") : steps))
             | otherwise ->
                 let st = if null xs then steps else ("decompose: " ++ termStr (resolve x s) ++ " = " ++ termStr (resolve y s) ++ " -> argument pairs") : steps
                 in go (todo ++ zip xs ys) s st
           _ -> (Nothing, reverse steps)

-- 規則を使うたびに変数の名前を新しくする（X → X_7）
renameClause :: Int -> Clause -> (Term, [Term])
renameClause k c = (rn (clHead c), map rn (clBody c))
  where rn t = case t of
          TV v -> TV (v ++ "_" ++ show k)
          TF f as -> TF f (map rn as)

termVars :: Term -> [String]                  -- 出てくる変数（出てきた順）
termVars t = nubOrd (go t)
  where go u = case u of
          TV v -> [v]
          TF _ as -> concatMap go as

-- ---- SLD 導出の木（深さ優先、上限つき）
newSld :: Int -> SLD
newSld k =
  let q = maybe (TF "?" []) id (parseTerm (at' queries k ""))
      root = SNode [q] M.empty 0 [] Open (-1) (-1)
  in SLD (IM.singleton 0 root) [0] 0 (termVars q)

startQuery :: Int -> World -> World
startQuery k w = let i = wrapI k (length queries) in w { wQ = i, wSld = newSld i, wSel = 0, wPaused = False }

setStatus :: Status -> Int -> IM.IntMap SNode -> IM.IntMap SNode
setStatus st i = IM.adjust (\n -> n { nStatus = st }) i

-- スタックの上のノードを 1 つ広げる（何もなければ Nothing）
expandOne :: SLD -> Maybe SLD
expandOne sld = case sStack sld of
  [] -> Nothing
  (i : rest) -> case IM.lookup i (sNodes sld) of
    Nothing -> Just sld { sStack = rest }
    Just n
      | null (nGoals n) -> Just sld { sStack = rest, sNodes = setStatus Success i (sNodes sld) }
      | nDepth n >= maxDepth -> Just sld { sStack = rest, sNodes = setStatus Bound i (sNodes sld) }
      | IM.size (sNodes sld) >= maxNodes ->
          Just sld { sStack = [], sNodes = foldl' (flip (setStatus Bound)) (sNodes sld) (i : rest) }
      | otherwise ->
          let g = headOr (TF "?" []) (nGoals n)
              gf = case walk g (nSub n) of
                TF f _ -> Just f
                _ -> Nothing
              step (kids, rc) (ci, c)
                | Just (fName (clHead c)) /= gf = (kids, rc)
                | otherwise =
                    let rc' = rc + 1
                        (hd, body) = renameClause rc' c
                    in case fst (unify g hd (nSub n) True) of
                         Just s -> (kids ++ [SNode (body ++ drop 1 (nGoals n)) s (nDepth n + 1) [] Open ci i], rc')
                         Nothing -> (kids, rc')
              (kids, rc2) = foldl' step ([], sRename sld) (zip [0 ..] clauses)
              base = IM.size (sNodes sld)
              ids = [base .. base + length kids - 1]
              nodes1 = IM.insert i n { nKids = ids, nStatus = if null kids then Fail else Inner } (sNodes sld)
              nodes2 = foldl' (\m (k, kn) -> IM.insert k kn m) nodes1 (zip ids kids)
          in Just sld { sNodes = nodes2, sStack = ids ++ rest, sRename = rc2 }
  where fName t = case t of
          TF f _ -> f
          TV v -> v

iterN :: Int -> SLD -> SLD
iterN k s = if k <= 0 then s else maybe s (iterN (k - 1)) (expandOne s)

runAllS :: SLD -> SLD
runAllS s = maybe s runAllS (expandOne s)

runAll :: Int -> SLD
runAll k = runAllS (newSld k)

-- 成功した葉の、質問の変数への答え（木を上から・左から）
answers :: SLD -> [String]
answers sld = go 0
  where
    go i = case IM.lookup i (sNodes sld) of
      Nothing -> []
      Just n -> [ ansStr n | nStatus n == Success ] ++ concatMap go (nKids n)
    ansStr n = let s = joinWith ", " [ v ++ " = " ++ termStr (resolve (TV v) (nSub n)) | v <- sQVars sld ]
               in if null s then "yes" else s

hasBound :: SLD -> Bool
hasBound sld = any ((== Bound) . nStatus) (IM.elems (sNodes sld))

-- ---- 描く
draw :: Env -> World -> Picture
draw env w =
  let wd = envW env
      ht = envH env
      sld = wSld w
      nodes = sNodes sld
      nCount = IM.size nodes
      sel = clampI (wSel w) 0 (nCount - 1)
      selN = IM.lookup sel nodes
      -- ---- プログラム
      progPics = [ say (show (i + 1) ++ ". " ++ clSrc c ++ ".") 30 (104 + i2f i * 21) 14
                     (col (if fmap nVia selN == Just i then yel else if i >= 8 then dimc else ink))
                 | (i, c) <- zip [0 :: Int ..] clauses ]
      -- ---- 単一化のお手本
      (ua, ub) = at' unifyEx (wU w) ("X", "X")
      ta = maybe (TF "?" []) id (parseTerm ua)
      tb = maybe (TF "?" []) id (parseTerm ub)
      (uSub, uSteps) = unify ta tb M.empty True
      y0 = 104 + i2f (length clauses) * 21 + 26
      ly = y0 + 26 + i2f (length uSteps) * 19
      uniPics = [ say ("Unification (U for next): " ++ ua ++ " = " ++ ub) 30 y0 15 (col sky) ]
        ++ [ say (show (k + 1) ++ ". " ++ s) 40 (y0 + 22 + i2f k * 19) 14 (col (if isFail s then pink else ink)) | (k, s) <- zip [0 :: Int ..] uSteps ]
        ++ case uSub of
             Just s -> [ say ("most general unifier: { " ++ joinWith ", " [ v ++ " -> " ++ termStr (resolve (TV v) s) | v <- nubOrd (termVars ta ++ termVars tb) ] ++ " }") 40 ly 14 (col grn)
                       , say ("apply it to both sides: " ++ termStr (resolve ta s) ++ " (the same)") 40 (ly + 20) 14 (col dimc) ]
             Nothing -> if ua == "X"
                          then [ say "Without the occurs check X := f(X) 'succeeds'" 40 ly 14 (col dimc)
                               , say "-> X = f(f(f(...))), an infinite term" 40 (ly + 19) 14 (col dimc) ]
                          else []
      -- ---- 右：SLD の木
      tx = wd * 0.42
      tw = wd - tx - 30
      ty = 140
      th = ht - 372
      lay = layoutTree 0 (\i -> maybe [] nKids (IM.lookup i nodes)) maxNodes
      dy = if tlDepth lay > 0 then min 46 (th / i2f (tlDepth lay)) else 0
      lnodes = tlNodes lay
      byIdx = IM.fromList [ (tnNode o, o) | o <- lnodes ]
      px o = tx + tnX o * tw
      py o = ty + i2f (tnDepth o) * dy
      parentOf o = IM.lookup (maybe (-1) nParent (IM.lookup (tnNode o) nodes)) byIdx
      stCol st = case st of
        Success -> grn
        Fail -> pink
        Bound -> yel
        Open -> dimc
        Inner -> sky
      edges = Color (col linec) (Pictures [ lineW 1 (px p, py p) (px o, py o) | o <- lnodes, tnParent o >= 0, Just p <- [parentOf o] ])
      nodePic o = case IM.lookup (tnNode o) nodes of
        Nothing -> Blank
        Just n ->
          let c = col (stCol (nStatus n))
              mark = case nStatus n of
                Success -> "[]"
                Fail -> "x"
                Bound -> "..."
                _ -> ""
          in Pictures $
               [ Color c (circleAt (px o) (py o) (if tnNode o == sel then 16 else 10)) ]
               ++ [ sayC mark (px o) (py o + 19) 14 c | not (null mark) ]
               ++ [ sayC (show (nVia n + 1)) ((px o + px p) / 2 - 8) ((py o + py p) / 2) 14 (col dimc)
                  | nVia n >= 0, length lnodes < 40, Just p <- [parentOf o] ]
      treeHead = [ say ("SLD tree: " ++ show nCount ++ " nodes " ++ (if null (sStack sld) then "(finished)" else "(searching" ++ (if wPaused w then ", paused)" else ")"))) tx 92 14 (col dimc)
                 , say ("blue = inner, green [] = success, pink x = fail, yellow ... = bound (depth " ++ show maxDepth ++ ", nodes " ++ show maxNodes ++ ")") tx 112 14 (col dimc) ]
      -- ---- 選んだノードと答え
      by = ht - 196
      selPics = case selN of
        Nothing -> []
        Just n ->
          [ say ("Selected node (depth " ++ show (nDepth n) ++ (if nVia n >= 0 then ", used rule " ++ show (nVia n + 1) else "") ++ ")") tx by 14 (col ink)
          , say (clip 90 ("Goals: " ++ (if null (nGoals n) then "[] (all solved = success)" else joinWith ", " [ termStr (resolve g (nSub n)) | g <- nGoals n ]))) tx (by + 22) 14 (col sky)
          , say (clip 90 ("Query variables: " ++ (let s = joinWith ", " [ v ++ " = " ++ termStr (resolve (TV v) (nSub n)) | v <- sQVars sld ] in if null s then "none" else s))) tx (by + 44) 14 (col dimc) ]
      ans = answers sld
      ansText = "Answers (" ++ show (length ans) ++ "): " ++ (if not (null ans) then joinWith "  |  " ans else if null (sStack sld) then "none (no)" else "...")
      ansPics = [ say ln tx (by + 76 + i2f k * 20) 15 (col grn) | (k, ln) <- zip [0 :: Int ..] (take 3 (wrapW tw 15 ansText)) ]
      boundPics = [ say "A branch hit the bound: an answer may lie beyond it (left recursion never ends depth-first)" tx (by + 140) 14 (col yel) | hasBound sld ]
  in view env $
       [ hudTitle ("Unification and logic programming:  ?- " ++ at' queries (wQ w) "" ++ ".")
           "Up/Down query   Space pause/resume   Left/Right select a node   U next unification example   A expand all" ]
       ++ progPics ++ uniPics ++ treeHead ++ [edges] ++ map nodePic lnodes ++ selPics ++ ansPics ++ boundPics
       ++ [drawSelfTest env (wd - 180) (ht - 14) (wTest w)]
  where isFail s = endsWith "fail" s

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down mods _ -> case typedKey k mods of
    TUp -> startQuery (wQ w - 1) w
    TDown -> startQuery (wQ w + 1) w
    TLeft -> w { wSel = max 0 (clampSel (wSel w) - 1) }
    TRight -> w { wSel = min (IM.size (sNodes (wSld w)) - 1) (clampSel (wSel w) + 1) }
    TChar ' ' -> w { wPaused = not (wPaused w) }
    TChar c
      | isKey 'u' c -> w { wU = wrapI (wU w + 1) (length unifyEx) }
      | isKey 'a' c -> w { wSld = runAllS (wSld w) }
    _ -> w
  _ -> w
  where clampSel s = clampI s 0 (IM.size (sNodes (wSld w)) - 1)

-- ---------- やってみよう ----------
-- 1. ?- ancestor(tom, D). の木で、規則 7 と 8 のどちらを使うと何が起こる？ ← → でノードをたどろう
-- 2. 規則 8 の体の順番を「ancestor(Z, Y), parent(X, Z)」に入れかえると？（anc_left と同じ左再帰になる）
-- 3. program に家族を足そう：parent(liz, sue) など。答えはどう増える？
-- 4. 単一化のお手本 g(X, f(Y)) = g(f(Z), X)：X := f(Z) のあと f(Y) = f(Z) が残る。順番を追ってみよう
-- 5. sibling(X, Y) :- parent(P, X), parent(P, Y). を足すと、自分自身も兄弟になってしまう。なぜ？（X と Y が同じでも単一化できる）

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("f(X, g(Y)) = f(a, g(X)) の単一化子は { X ↦ a, Y ↦ a } で、代入すると両辺が同じ"
    , case unify a1 b1 M.empty True of
        (Just s, _) -> termStr (resolve (TV "X") s) == "a" && termStr (resolve (TV "Y") s) == "a" && termStr (resolve a1 s) == termStr (resolve b1 s)
        _ -> False)
  , ("X = f(X) は出現検査で失敗、出現検査なしだと「成功」してしまう"
    , fst (unify (pt "X") (pt "f(X)") M.empty True) == Nothing && fst (unify (pt "X") (pt "f(X)") M.empty False) /= Nothing)
  , ("p(X, X) = p(a, b) と f(X, a) = g(X, a) は失敗"
    , fst (unify (pt "p(X, X)") (pt "p(a, b)") M.empty True) == Nothing && fst (unify (pt "f(X, a)") (pt "g(X, a)") M.empty True) == Nothing)
  , ("でたらめな項の組 300 個：単一化できたもの（" ++ show nOk ++ " 組）は代入すると必ず両辺が同じ", okR == 300 && nOk > 30)
  , ("?- ancestor(tom, D). の答え = { bob, liz, ann, pat, jim }", eqSet (set 2) ["D = bob", "D = liz", "D = ann", "D = pat", "D = jim"])
  , ("?- grandparent(tom, W). の答え = { ann, pat }、?- ancestor(A, jim). = { pat, bob, tom }"
    , eqSet (set 1) ["W = ann", "W = pat"] && eqSet (set 3) ["A = pat", "A = bob", "A = tom"])
  , ("?- grandparent(X, Y).（質問と規則の変数の名前が同じ）の答え = 事実から直接作った祖父母の組 " ++ show (length gp) ++ " 個", eqSet (set 4) gp)
  , ("?- ancestor(jim, X). は答えなしで、木は上限に当たらずに終わる", S.null (set 5) && not (hasBound (runAll 5)))
  , ("?- anc_left(tom, D).（左再帰）は深さ " ++ show maxDepth ++ " の上限に当たり、ノードは " ++ show maxNodes ++ " 個以下"
    , let r = runAll 6 in hasBound r && IM.size (sNodes r) <= maxNodes + 10) ]
  where
    pt s = maybe (TF "?" []) id (parseTerm s)
    a1 = pt "f(X, g(Y))"
    b1 = pt "f(a, g(X))"
    -- でたらめな項（乱数は p5.js 版と同じ順に引く：引数を先に作ってから関数の名前）
    rt :: Int -> R Term
    rt d = do
      r <- rnd
      if d <= 0 || r < 0.35 then do
        u <- rnd
        if u < 0.5 then TV <$> rndPick ["X", "Y", "Z"] else (\c -> TF c []) <$> rndPick ["a", "b"]
      else do
        k <- rndInt 1 2
        as <- mapM (const (rt (d - 1))) [1 .. k]
        f <- rndPick ["f", "g"]
        pure (TF f as)
    pairs = withSeed 9 (mapM (const ((,) <$> rt 3 <*> rt 3)) [1 .. 300 :: Int])
    results = [ fmap (\s -> termStr (resolve x s) == termStr (resolve y s)) (fst (unify x y M.empty True)) | (x, y) <- pairs ]
    nOk = length [ () | Just _ <- results ]
    okR = length [ () | r <- results, r /= Just False ]
    set k = S.fromList (answers (runAll k))
    eqSet s xs = S.size s == length xs && all (`S.member` s) xs
    facts = [ map termStr as | c <- clauses, TF "parent" as <- [clHead c] ]
    gp = [ "X = " ++ p ++ ", Y = " ++ s | [p, q] <- facts, [r, s] <- facts, q == r ]

-- ---- このスケッチの道具
joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : r) -> x ++ concatMap (sep ++) r

endsWith :: String -> String -> Bool
endsWith suf s = reverse suf == take (length suf) (reverse s)

-- 幅 w に入るように単語の区切りで折り返す（文字の幅はだいたい 0.62 × 大きさ）
wrapW :: Float -> Float -> String -> [String]
wrapW w size s = go (words s) ""
  where
    maxC = max 10 (floor (w / (size * 0.62))) :: Int
    go [] cur = [cur | not (null cur)]
    go (x : r) cur
      | null cur = go r x
      | length cur + 1 + length x <= maxC = go r (cur ++ " " ++ x)
      | otherwise = cur : go (x : r) ""

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
