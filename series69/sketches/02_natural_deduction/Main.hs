-- ============================================
-- 69-02 自然演繹 ─ 前提から結論までを、規則を 1 回ずつ使って 1 行ずつ導く（フィッチ式：仮定を置くと箱が開く）
-- 証明は「意味」を見ずに「形」だけで検査できる。->I・~I・背理法（RAA）は箱を閉じて仮定を外す（箱の中の行は外から使えない）
-- 操作：左の行をクリックで選ぶ（順番に 1・2・3）→ 右の規則ボタン。仮定・|I・_|_E の式は下の候補から選ぶ
--       1〜6 課題 / U（か Backspace）1 行もどす / X 箱を閉じる（結論なし：|E の 2 つ目の箱の前に）/ H お手本を見る / C 最初から
-- 画面の記号：~ = ¬、& = ∧、| = ∨、-> = →、_|_ = ⊥、|- = ⊢
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (find, foldl', sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

-- ★ 課題：(名前, 前提, 結論, お手本 [(深さ, 規則, 参照する行の番号, 式)])
exercises :: [(String, [String], String, [(Int, String, [Int], String)])]
exercises =
  [ ("syllogism", ["p -> q", "q -> r"], "p -> r", [(1, "assume", [], "p"), (1, "impE", [1, 3], "q"), (1, "impE", [2, 4], "r"), (0, "impI", [3], "p -> r")])
  , ("double negation", ["~~p"], "p", [(1, "assume", [], "~p"), (1, "notE", [2, 1], "_|_"), (0, "raa", [2], "p")])
  , ("swap &", ["p & q"], "q & p", [(0, "andE2", [1], "q"), (0, "andE1", [1], "p"), (0, "andI", [2, 3], "q & p")])
  , ("swap |", ["p | q"], "q | p", [(1, "assume", [], "p"), (1, "orI2", [2], "q | p"), (1, "assume", [], "q"), (1, "orI1", [4], "q | p"), (0, "orE", [1, 2, 4], "q | p")])
  , ("contraposition", ["p -> q"], "~q -> ~p", [(1, "assume", [], "~q"), (2, "assume", [], "p"), (2, "impE", [1, 3], "q"), (2, "notE", [4, 2], "_|_"), (1, "notI", [3], "~p"), (0, "impI", [2], "~q -> ~p")])
  , ("challenge: excluded middle", [], "p | ~p", [(1, "assume", [], "~(p | ~p)"), (2, "assume", [], "p"), (2, "orI1", [2], "p | ~p"), (2, "notE", [3, 1], "_|_"), (1, "notI", [2], "~p"), (1, "orI2", [5], "p | ~p"), (1, "notE", [6, 1], "_|_"), (0, "raa", [1], "p | ~p")]) ]

-- 規則の名前と、参照するもの（L = 行、B = 閉じた箱）
ruleInfo :: String -> Maybe (String, String)
ruleInfo r = lookup r
  [ ("prem", ("Prem", "")), ("assume", ("Assume", "")), ("reit", ("R", "L")), ("andI", ("&I", "LL")), ("andE1", ("&E", "L")), ("andE2", ("&E", "L"))
  , ("orI1", ("|I", "L")), ("orI2", ("|I", "L")), ("orE", ("|E", "LBB")), ("impI", ("->I", "B")), ("impE", ("->E", "LL"))
  , ("notI", ("~I", "B")), ("notE", ("~E", "LL")), ("botE", ("_|_E", "L")), ("dnE", ("~~E", "L")), ("raa", ("RAA", "B")) ]

ruleName :: String -> String
ruleName r = maybe r fst (ruleInfo r)

buttons :: [(String, String)]                 -- 右のボタン (規則, 表示)
buttons =
  [ ("assume", "Assume (candidate)"), ("reit", "R reiterate"), ("andI", "&I (A, B -> A&B)"), ("andE1", "&E left (A&B -> A)"), ("andE2", "&E right (A&B -> B)")
  , ("orI1", "|I (A -> A|cand)"), ("orI2", "|I (A -> cand|A)"), ("orE", "|E (A|B, 2 boxes)"), ("impI", "->I (close box)"), ("impE", "->E (A->B and A)")
  , ("notI", "~I (box ends _|_)"), ("notE", "~E (A, ~A -> _|_)"), ("botE", "_|_E (_|_ -> cand)"), ("dnE", "~~E (~~A -> A)"), ("raa", "RAA (box ~A.._|_)")
  , ("close", "X close box"), ("undo", "U undo") ]

-- 証明の 1 行と、証明（行・次の行の深さ・結論）
data PLine = PLine { lF :: F, lRule :: String, lRefs :: [Int], lDepth :: !Int }
data Proof = Proof { pLines :: [PLine], pDepth :: !Int, pGoal :: F }

data MsgKind = MInfo | MOk | MErr deriving Eq

data World = World
  { wEx    :: !Int                            -- 課題の番号
  , wPf    :: Proof
  , wHist  :: [Proof]                         -- もどすための履歴
  , wSel   :: [Int]                           -- 選んだ行
  , wCand  :: Maybe F                         -- 選んだ候補の式
  , wCands :: [F]
  , wMsg   :: (MsgKind, String)
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-02 Natural Deduction" (loadExercise 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadExercise 0 world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World 0 (freshProof 0) [] [] Nothing [] (MInfo, "") []

fm :: String -> F                             -- 文字列 → 式
fm s = either (const FBot) id (parseFormula s)

exAt :: Int -> (String, [String], String, [(Int, String, [Int], String)])
exAt k = at' exercises k ("", [], "p", [])

freshProof :: Int -> Proof
freshProof k = let (_, ps, g, _) = exAt k in Proof [ PLine (fm s) "prem" [] 0 | s <- ps ] 0 (fm g)

-- 候補 = 前提と結論の部分式と、その否定、_|_（同じ式は 1 回、22 個まで）
candsFor :: Int -> [F]
candsFor k =
  let (_, ps, g, _) = exAt k
      walk f = f : concatMap walk (fKids f)
      base = nubBy' (concatMap (walk . fm) (ps ++ [g]))
  in take 22 (nubBy' (base ++ map FNot base ++ [FBot]))
  where nubBy' = map snd . nubOn . map (\f -> (fmlStr f, f))
        nubOn xs = go S.empty xs
        go _ [] = []
        go seen ((s, f) : r) = if S.member s seen then go seen r else (s, f) : go (S.insert s seen) r

loadExercise :: Int -> World -> World
loadExercise k w =
  let e = clampI k 0 (length exercises - 1)
  in w { wEx = e, wPf = freshProof e, wHist = [], wSel = [], wCand = Nothing, wCands = candsFor e
       , wMsg = (MInfo, "Click lines on the left to select them, then press a rule") }

solutionLines :: Int -> [PLine]
solutionLines k = let (_, _, _, sol) = exAt k in [ PLine (fm s) r (map (subtract 1) refs) d | (d, r, refs, s) <- sol ]

-- ---- 証明のチェッカー：各行の「箱（スコープ）」を求めて、規則の形を調べる。返り値 (説明（"" なら正しい）, 箱)
isPrefix :: [Int] -> [Int] -> Bool
isPrefix a b = length a <= length b && and (zipWith (==) a b)

ndCheck :: [PLine] -> ([String], [[Int]])
ndCheck lines = go 0 lines [] False [] M.empty
  where
    go !k ls stack seenOther msgs scopes = case ls of
      [] -> (reverse msgs, M.elems scopes)
      (l : rest) ->
        let m0 = if lRule l == "prem" then (if seenOther || lDepth l /= 0 then "premises only at the start" else "") else ""
            seen' = seenOther || lRule l /= "prem"
            d = lDepth l
            (m1, stack') =
              if lRule l == "assume"
                then (if null m0 && (d < 1 || d > length stack + 1) then "wrong box depth" else m0, take (max 0 (d - 1)) stack ++ [k])
                else (if null m0 && (d > length stack || d < 0) then "cannot enter a box without an assumption" else m0, take (max 0 d) stack)
            scopes' = M.insert k stack' scopes
            m = if null m1 then ruleCheck lines scopes' k else m1
        in go (k + 1) rest stack' seen' (m : msgs) scopes'

data Arg = AL F | AB F F                      -- 行の式 / 箱（仮定, 最後の行）

ruleCheck :: [PLine] -> M.Map Int [Int] -> Int -> String
ruleCheck lines scopes k = case ruleInfo rule of
  Nothing -> "unknown rule"
  Just (nm, kinds)
    | length refs /= length kinds -> nm ++ " takes " ++ show (length kinds) ++ " reference(s)"
    | otherwise -> case sequence (zipWith arg refs kinds) of
        Left e -> e
        Right as -> shape as
  where
    l = at' lines k (PLine FBot "?" [] 0)
    rule = lRule l
    refs = lRefs l
    sc i = M.findWithDefault [] i scopes
    fOf i = lF (at' lines i l)
    arg r kd
      | not (r >= 0 && r < k) = Left "only earlier lines can be used"
      | kd == 'L' = if isPrefix (sc r) (sc k) then Right (AL (fOf r)) else Left ("line " ++ show (r + 1) ++ " is inside a closed box, not usable here")
      | lRule (at' lines r l) /= "assume" = Left ("line " ++ show (r + 1) ++ " does not start a box (assumption)")
      | r `elem` sc k = Left ("the box from line " ++ show (r + 1) ++ " is not closed yet")
      | not (isPrefix (dropLast (sc r)) (sc k)) = Left ("the box at line " ++ show (r + 1) ++ " is not visible from here")
      | otherwise =
          let e = foldl' (\acc i -> if r `elem` sc i then i else acc) r [r + 1 .. k - 1]
          in if lastOr (-1) (sc e) /= r then Left ("the last line of the box at line " ++ show (r + 1) ++ " is inside a deeper box")
             else Right (AB (fOf r) (fOf e))
    f = lF l
    ok c msg = if c then "" else msg
    shape as = case (rule, as) of
      ("prem", _) -> ""
      ("assume", _) -> ""
      ("reit", [AL a]) -> ok (f == a) "R: not the same formula"
      ("andI", [AL a, AL b]) -> ok (f == FBin OAnd a b) "&I: the result is 'first & second'"
      ("andE1", [AL a]) -> ok (case a of FBin OAnd x _ -> x == f; _ -> False) "&E: from A & B get A"
      ("andE2", [AL a]) -> ok (case a of FBin OAnd _ y -> y == f; _ -> False) "&E: from A & B get B"
      ("orI1", [AL a]) -> ok (case f of FBin OOr x _ -> x == a; _ -> False) "|I: from A get A | B"
      ("orI2", [AL a]) -> ok (case f of FBin OOr _ y -> y == a; _ -> False) "|I: from A get B | A"
      ("orE", [AL a, AB a1 e1, AB a2 e2]) -> ok (case a of FBin OOr x y -> a1 == x && a2 == y && e1 == f && e2 == f; _ -> False)
                                               "|E: A | B, box A..C, box B..C => C"
      ("impI", [AB a e]) -> ok (f == FBin OImp a e) "->I: box A..B => A -> B"
      ("impE", [AL a, AL b]) -> ok (case a of FBin OImp x y -> x == b && y == f; _ -> False) "->E: from A -> B and A get B"
      ("notI", [AB a e]) -> ok (e == FBot && f == FNot a) "~I: box A.._|_ => ~A"
      ("notE", [AL a, AL b]) -> ok (f == FBot && b == FNot a) "~E: from A and ~A get _|_"
      ("botE", [AL a]) -> ok (a == FBot) "_|_E: from _|_ anything"
      ("dnE", [AL a]) -> ok (a == FNot (FNot f)) "~~E: from ~~A get A"
      ("raa", [AB a e]) -> ok (e == FBot && a == FNot f) "RAA: box ~A.._|_ => A"
      _ -> "unknown rule"

ndOk :: [PLine] -> Bool
ndOk ls = all null (fst (ndCheck ls))

-- ---- ボタンから新しい行を作る（作った行は必ずチェッカーで検査してから足す）
openBoxes :: Proof -> [Int]
openBoxes st = take (pDepth st) (lastOr [] (snd (ndCheck (pLines st))))

-- 箱 b の最後の行（b を含む箱の中で一番うしろの行）
boxEnd :: [[Int]] -> Int -> Int -> Int
boxEnd sc n b = foldl' (\acc i -> if b `elem` at' sc i [] then i else acc) b [b + 1 .. n - 1]

ndBuild :: Proof -> String -> [Int] -> Maybe F -> Either String PLine
ndBuild st rule s cf
  | rule == "assume" = maybe (Left "pick the formula to assume from the candidates") (\c -> Right (PLine c rule [] (d + 1))) cf
  | kinds == "B" =
      let found = find (\i -> rl i == "assume" && i `notElem` ob) s
      in case (found, ob) of
           (Nothing, []) -> Left "no box to close (an assumption opens a box)"
           _ -> let closing = found == Nothing
                    b = maybe (lastOr 0 ob) id found
                    e = boxEnd sc n b
                    fa = fo b
                    fe = fo e
                    mf = case rule of
                      "impI" -> Just (FBin OImp fa fe)
                      "notI" -> Just (FNot fa)
                      _ -> case fa of FNot x -> Just x; _ -> Nothing
                in case mf of
                     Nothing -> Left "RAA: the assumption must be ~A"
                     Just f -> Right (PLine f rule [b] (if closing then lDepth (lineOf b) - 1 else d))
  | rule == "orE" =
      let inner = if null ob then Nothing else Just (lastOr 0 ob)
          boxes = filter (\i -> rl i == "assume" && (i `notElem` ob || Just i == inner)) s
          rest = filter (`notElem` boxes) s
      in case (rest, sort boxes) of
           ([r0], [b0, b1]) ->
             let e = boxEnd sc n b0
                 dep = case inner of
                   Just i | i `elem` boxes -> lDepth (lineOf i) - 1
                   _ -> d
             in Right (PLine (fo e) rule [r0, b0, b1] dep)
           _ -> Left "|E: pick the A | B line and the 2 box assumptions"
  | length s /= length kinds = Left (ruleName rule ++ ": pick " ++ show (length kinds) ++ " line(s) (now " ++ show (length s) ++ ")")
  | otherwise =
      let s0 = headOr 0 s
          s1 = at' s 1 0
          a0 = fo s0
          b0 = fo s1
          res :: Either String (Maybe F, [Int])
          res = case rule of
            "reit" -> Right (Just a0, s)
            "andI" -> Right (Just (FBin OAnd a0 b0), s)
            "andE1" -> Right (case a0 of FBin OAnd x _ -> Just x; _ -> Nothing, s)
            "andE2" -> Right (case a0 of FBin OAnd _ y -> Just y; _ -> Nothing, s)
            "orI1" -> maybe (Left "|I: pick the formula to add from the candidates") (\c -> Right (Just (FBin OOr a0 c), s)) cf
            "orI2" -> maybe (Left "|I: pick the formula to add from the candidates") (\c -> Right (Just (FBin OOr c a0), s)) cf
            "impE" ->
              let swap = isImp b0 && not (isImp a0)
                  (a, refs) = if swap then (b0, [s1, s0]) else (a0, s)
              in Right (case a of FBin OImp _ y -> Just y; _ -> Nothing, refs)
            "notE" ->
              let swap = isNot a0 && not (b0 == FNot a0)
              in Right (Just FBot, if swap then [s1, s0] else s)
            "botE" -> maybe (Left "_|_E: pick the conclusion from the candidates") (\c -> Right (Just c, s)) cf
            "dnE" -> Right (case a0 of FNot (FNot x) -> Just x; _ -> Nothing, s)
            _ -> Right (Nothing, s)
      in case res of
           Left e -> Left e
           Right (Nothing, _) -> Left (ruleName rule ++ ": the chosen lines have the wrong shape")
           Right (Just f, refs) -> Right (PLine f rule refs d)
  where
    ls = pLines st
    n = length ls
    d = pDepth st
    kinds = maybe "" snd (ruleInfo rule)
    ob = openBoxes st
    sc = snd (ndCheck ls)
    lineOf i = at' ls i (PLine FBot "?" [] 0)
    fo = lF . lineOf
    rl = lRule . lineOf
    isImp g = case g of FBin OImp _ _ -> True; _ -> False
    isNot g = case g of FNot _ -> True; _ -> False

-- 返り値 ("" = 足せた / エラーの説明, 新しい証明)
tryRule :: Proof -> String -> [Int] -> Maybe F -> (String, Proof)
tryRule st rule s cf
  | rule == "close" = if pDepth st == 0 then ("no open box", st) else ("", st { pDepth = pDepth st - 1 })
  | otherwise = case ndBuild st rule s cf of
      Left e -> (e, st)
      Right l ->
        let m = lastOr "" (fst (ndCheck (pLines st ++ [l])))
        in if null m then ("", st { pLines = pLines st ++ [l], pDepth = lDepth l }) else (m, st)

press :: String -> World -> World
press rule w
  | rule == "undo" = case wHist w of
      (h : rest) -> w { wPf = h, wHist = rest, wMsg = (MInfo, "undid one step"), wSel = [] }
      [] -> w { wSel = [] }
  | otherwise =
      let (m, pf') = tryRule (wPf w) rule (wSel w) (wCand w)
      in if not (null m) then w { wMsg = (MErr, "x " ++ m) }
         else w { wPf = pf', wHist = wPf w : wHist w, wSel = []
                , wMsg = if rule == "close" then (MInfo, "closed the box")
                         else (MOk, "OK " ++ ruleName rule ++ " gives " ++ fmlStr (lF (lastOr (PLine FBot "" [] 0) (pLines pf')))) }

-- 箱の外に結論が出たら完成
solved :: Proof -> Bool
solved st = let (ms, sc) = ndCheck (pLines st)
            in all null ms && or [ null s && lF l == pGoal st | (l, s) <- zip (pLines st) sc ]

justify :: PLine -> Int -> [[Int]] -> String
justify l k sc
  | lRule l == "prem" || lRule l == "assume" = ruleName (lRule l)
  | otherwise =
      let kinds = maybe "" snd (ruleInfo (lRule l))
          one (r, kd) = if kd == 'L' then show (r + 1) else show (r + 1) ++ "-" ++ show (boxEnd sc k r + 1)
      in ruleName (lRule l) ++ " " ++ joinWith ", " (map one (zip (lRefs l) (kinds ++ repeat 'L')))

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : r) -> x ++ concatMap (sep ++) r

-- ---- 画面の配置（描くのとクリックの判定で同じものを使う）
lineGeom :: Env -> World -> (Float, Float, Float)   -- (x0, y0, 行の高さ)
lineGeom env w = (40, 116, min 32 ((envH env - 260) / i2f (max 10 (length (pLines (wPf w)) + 1))))

buttonBoxes :: Env -> [(String, String, (Float, Float, Float, Float))]
buttonBoxes env =
  let rx = envW env * 0.62
      bw = (envW env - rx - 30) / 2
  in [ (r, lab, (rx + i2f (i `mod` 2) * (bw + 8), 96 + i2f (i `div` 2) * 34, bw, 28)) | (i, (r, lab)) <- zip [0 :: Int ..] buttons ]

candTop :: Float
candTop = 96 + i2f ((length buttons + 1) `div` 2) * 34 + 20

candBoxes :: Env -> World -> [(F, (Float, Float, Float, Float))]
candBoxes env w = go (wCands w) rx (candTop + 10)
  where
    rx = envW env * 0.62
    go fs cx cy = case fs of
      [] -> []
      (f : rest) ->
        let bw = textW (fmlStr f) 14 + 16
            (cx', cy') = if cx + bw > envW env - 20 then (rx, cy + 30) else (cx, cy)
        in (f, (cx', cy', bw, 24)) : go rest (cx' + bw + 6) cy'

-- 上揃えの文字（textAlign(LEFT, TOP)）
sayT, sayTR :: String -> Float -> Float -> Float -> Color -> Picture
sayT s x y size c = say s x (y + size * 0.82) size c
sayTR s x y size c = sayR s x (y + size * 0.82) size c

draw :: Env -> World -> Picture
draw env w =
  let (nm, ps, g, _) = exAt (wEx w)
      pf = wPf w
      ls = pLines pf
      (msgs, sc) = ndCheck ls
      (x0, y0, lh) = lineGeom env w
      wd = envW env
      ht = envH env
      linePic k l =
        let y = y0 + i2f k * lh
            s = at' sc k []
            ix = x0 + 40
            si = lookup k (zip (wSel w) [0 :: Int ..])
            tx = ix + i2f (length s) * 22 + 8
            lastPrem = lRule l == "prem" && (k + 1 == length ls || lRule (at' ls (k + 1) l) /= "prem")
            c = if not (null (at' msgs k "")) then pink else if lF l == pGoal pf && null s then yel else ink
        in Pictures $
             [ Color (col linec) (rectTL (x0 - 6) (y - 3) (wd * 0.56) (lh - 2)) | si /= Nothing ]
             ++ [ Color (col sky) (lineW 2 (bx, y - 3) (bx, y + lh - 5)) | lv <- [0 .. length s - 1], let bx = ix + i2f lv * 22 ]
             ++ [ Color (col sky) (lineW 2 (tx - 8, y + lh - 5) (tx + 120, y + lh - 5)) | lRule l == "assume" ]
             ++ [ Color (col ink) (lineW 2 (ix, y + lh - 4) (ix + 160, y + lh - 4)) | lastPrem ]
             ++ [ sayTR (show (k + 1)) (x0 + 22) (y + 2) 15 (col dimc)
                , sayT (fmlStr (lF l)) tx y 18 (col c)
                , sayT (justify l k sc) (x0 + wd * 0.4) (y + 3) 14 (col dimc) ]
             ++ [ sayT ("sel " ++ show (i + 1)) (x0 + wd * 0.5) (y + 3) 14 (col yel) | Just i <- [si] ]
      yb = y0 + i2f (length ls) * lh + 6
      (mk, msg) = wMsg w
      btn (r, lab, (bx, by, bw, bh)) =
        Pictures [ Color (col panelc) (rectTL bx by bw bh), Color (col (if r == "close" || r == "undo" then lilac else sky)) (frameTL 1.2 bx by bw bh)
                 , sayM lab (bx + bw / 2) (by + 14) 14 (col ink) ]
      cand (f, (cx, cy, bw, bh)) =
        Pictures [ Color (col panelc) (rectTL cx cy bw bh), Color (col (if wCand w == Just f then yel else linec)) (frameTL 1.2 cx cy bw bh)
                 , sayM (fmlStr f) (cx + bw / 2) (cy + 12) 14 (col ink) ]
      goalStr = joinWith ", " ps ++ " |- " ++ g
  in view env $
       [ hudTitle ("Natural deduction (Fitch)   Exercise " ++ show (wEx w + 1) ++ ": " ++ nm ++ "    " ++ goalStr)
           "Click lines to select them, then a rule button. Click a candidate formula too\n1-6 exercise   U undo   X close box   H model proof   C restart" ]
       ++ zipWith linePic [0 ..] ls
       ++ [ sayT ("Next line depth: " ++ show (pDepth pf) ++ (if pDepth pf > 0 then " (inside a box: ->I, ~I, RAA or X closes it)" else " (outermost)")) x0 yb 14 (col dimc)
          , sayT msg x0 (ht - 92) 16 (col (case mk of MErr -> pink; MOk -> grn; MInfo -> ink)) ]
       ++ [ sayT ("Proved!  " ++ goalStr ++ "  (the checker verified every line)") x0 (ht - 62) 24 (col yel) | solved pf ]
       -- ---- 右：規則のボタンと候補の式
       ++ map btn (buttonBoxes env)
       ++ [ say ("Candidate (for Assume, |I, _|_E): " ++ maybe "none" fmlStr (wCand w)) (wd * 0.62) candTop 14 (col yel) ]
       ++ map cand (candBoxes env w)
       ++ [ drawSelfTest env (wd - 180) (ht - 14) (wTest w) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = toP5 env pos
        (x0, y0, lh) = lineGeom env w
        lineHits = [ (k, (x0 - 6, y0 + i2f k * lh - 3, envW env * 0.56, lh)) | k <- [0 .. length (pLines (wPf w)) - 1] ]
    in case find (inBox m . snd) lineHits of
         Just (k, _) -> if k `elem` wSel w then w { wSel = filter (/= k) (wSel w) }
                        else if length (wSel w) < 3 then w { wSel = wSel w ++ [k] } else w
         Nothing -> case find (\(_, _, b) -> inBox m b) (buttonBoxes env) of
           Just (r, _, _) -> press r w
           Nothing -> case find (inBox m . snd) (candBoxes env w) of
             Just (f, _) -> w { wCand = if wCand w == Just f then Nothing else Just f }
             Nothing -> w
  EventKey k Down mods _ -> case typedKey k mods of
    TChar c
      | c >= '1' && c <= '6' -> loadExercise (fromEnum c - fromEnum '1') w
      | isKey 'u' c -> press "undo" w
      | isKey 'x' c -> press "close" w
      | isKey 'c' c -> loadExercise (wEx w) w
      | isKey 'h' c -> w { wPf = (freshProof (wEx w)) { pLines = pLines (freshProof (wEx w)) ++ solutionLines (wEx w), pDepth = 0 }
                         , wHist = [], wSel = [], wMsg = (MInfo, "Model proof (checked by the checker). C to start over yourself") }
    TBack -> press "undo" w
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 課題 1：p を仮定（候補の p → Assume）、1 と 3 を選んで ->E、2 と 4 を選んで ->E、->I で箱を閉じる
-- 2. 課題 4 は |E：p を仮定して q | p、X で箱を閉じ、q を仮定して q | p。最後に 1・2・4 行目を選んで |E（2 つ目の箱はここで閉じる）
-- 3. 課題 2 を ~~E を使わずに。背理法（RAA）なしでは ~~p |- p は証明できない（直観主義論理。09 のカリー-ハワードを見よう）
-- 4. わざとまちがえよう：閉じた箱の中の行を選んで使うと、チェッカーは何と言う？
-- 5. exercises に課題を足そう：例 ("distribute", ["p & (q | r)"], "(p & q) | (p & r)", [])（お手本は空でもよいが自己テストは FAIL になる）

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
-- 意味の検査：前提 ＋ その行で開いている仮定 |= その行（真理値表）
entailed :: Proof -> [[Int]] -> Int -> Bool
entailed st sc k =
  let ls = pLines st
      hyps = [ lF l | l <- ls, lRule l == "prem" ] ++ [ lF (at' ls i (PLine FTop "" [] 0)) | i <- at' sc k [] ]
      allH = foldl' (FBin OAnd) FTop hyps
  in kindOf (FBin OImp allH (lF (at' ls k (PLine FTop "" [] 0)))) == Taut

selfTest :: [(String, Bool)]
selfTest =
  [ ("お手本 " ++ show (length exercises) ++ " 問：チェッカーが全部の行を認めて、結論に着く", okAll)
  , ("お手本の全部の行で「前提 ＋ 開いている仮定 ⊨ その行」（真理値表で意味を確認）", sound)
  , ("でたらめにボタンを押して作った " ++ show made ++ " 行も、チェッカーが通した行は全部「意味として正しい」（健全性）", badSem == 0 && made > 80)
  , ("はねる例 1：閉じた箱の中の行（q）を外で使う", rej 0 (solutionLines 0 ++ [ln 0 "reit" [4] "q"]))
  , ("はねる例 2：→E の前件がちがう（q → r と p から r）", rej 0 [ln 1 "assume" [] "p", ln 1 "impE" [2, 3] "r"])
  , ("はねる例 3：∧E で p ∧ q から q を「左」として取り出す", rej 2 [ln 0 "andE1" [1] "q"])
  , ("はねる例 4：⊥ で終わらない箱で背理法", rej 1 [ln 1 "assume" [] "~p", ln 1 "reit" [2] "~p", ln 0 "raa" [2] "p"])
  , ("はねる例 5：箱の最後の行が内側の箱の中（p を仮定 / q を仮定 / q → 外の箱で p → q）"
    , rej 5 [ln 1 "assume" [] "p", ln 2 "assume" [] "q", ln 2 "reit" [2] "q", ln 0 "impI" [1] "p -> q"])
  , ("はねる例 6：まだ閉じていない箱を →I で参照する、仮定なしで深い行"
    , rej 5 [ln 1 "assume" [] "p", ln 1 "impI" [1] "p -> p"] && rej 0 [ln 1 "reit" [1] "p -> q"]) ]
  where
    withSol k = let st = freshProof k in st { pLines = pLines st ++ solutionLines k }
    sols = map withSol [0 .. length exercises - 1]
    okAll = and [ ndOk (pLines st) && solved st | st <- sols ]
    sound = and [ entailed st (snd (ndCheck (pLines st))) i | st <- sols, i <- [0 .. length (pLines st) - 1] ]
    rules = [ r | (r, _) <- buttons, r /= "undo" ]
    -- でたらめにボタンを押す（乱数は p5.js 版と同じ順：選ぶ行の数、行、規則、候補を使うか、候補）
    (made, badSem) = withSeed 21 (go 0 0 [ (k, t) | k <- [0 .. length exercises - 1], t <- [0 .. 3 :: Int] ])
    go !md !bad ts = case ts of
      [] -> pure (md, bad)
      ((k, _) : rest) -> do
        (st, md') <- presses (candsFor k) (freshProof k) md 0
        let (ms, sc) = ndCheck (pLines st)
            bad' = bad + (if all null ms then 0 else 1) + length [ () | i <- [0 .. length (pLines st) - 1], not (entailed st sc i) ]
        go md' bad' rest
    presses ck st !md !a
      | a >= (60 :: Int) || length (pLines st) >= 24 = pure (st, md)
      | otherwise = do
          let n = length (pLines st)
          j <- rndInt 0 3
          s <- if n > 0 then replicateM j (rndInt 0 (n - 1)) else pure []
          r <- rndPick rules
          u <- rnd
          cf <- if u < 0.8 then Just <$> rndPick ck else pure Nothing
          let (m, st') = tryRule st r s cf
          presses ck st' (if null m && length (pLines st') > n then md + 1 else md) (a + 1)
    rej k extra = let st = freshProof k in not (ndOk (pLines st ++ extra))
    ln d r refs s = PLine (fm s) r (map (subtract 1) refs) d

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
