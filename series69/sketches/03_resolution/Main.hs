-- ============================================
-- 69-03 導出原理 ─ 節（リテラルの |）を 2 つ選び、p と ~p のように打ち消し合う文字を消して 1 つにまとめる（導出）
-- (A | p) と (B | ~p) から (A | B)。どんどん作って空節 []（何もない節 = 偽）が出たら、節の集合は矛盾（充足不能）。
-- 「前提 |- 結論」を示すには、前提と「結論の否定」の節から [] を出す（背理法）。[] が出ないまま新しい節が尽きたら充足可能
-- 操作：1〜5 問題 / R でたらめな節の集合 / スペース 自動で探す（幅優先・上限つき）/ N 1 段だけ / C 作った節を消す
--       節を 2 つクリック = 手で導出（打ち消し合う文字がないと作れない）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

problems :: [(String, [String])]              -- ★ (名前, 論理式のリスト（それぞれ CNF の節にする）)
problems =
  [ ("Case split: p or q, either way r", ["p | q", "p -> r", "q -> r", "~r"])
  , ("Syllogism p->q, q->r |- p->r (add the negated conclusion)", ["p -> q", "q -> r", "~(p -> r)"])
  , ("A satisfiable set ([] never appears)", ["p | q", "~p | q", "q -> r"])
  , ("Pigeonhole: 3 birds, 2 nests (no nest holds 2)", ["a1 | a2", "b1 | b2", "c1 | c2", "~a1 | ~b1", "~a1 | ~c1", "~b1 | ~c1", "~a2 | ~b2", "~a2 | ~c2", "~b2 | ~c2"])
  , ("Contraposition p->q |- ~q->~p", ["p -> q", "~(~q -> ~p)"]) ]

maxClauses :: Int
maxClauses = 800                              -- ★ 作る節の上限（変数 6 個なら節は多くても 3⁶ = 729 種類なので、ここまでに必ず終わる）

pairsPerFrame :: Int
pairsPerFrame = 600                           -- ★ 1 フレームに調べる組の数（多いほど速い）

type Clause = [String]                        -- リテラル 'p' / '~p' の並び（名前の順）

-- 節の集合の 1 つ：節、作った親 (j, i)（最初の節は Nothing）、消した文字
data Cl = Cl { clC :: Clause, clFrom :: Maybe (Int, Int), clPivot :: String }

-- 節の集合（並び、もうある節、空節があるか）
data CSet = CSet { csList :: Q.Seq Cl, csKeys :: S.Set Clause, csEmpty :: !Bool }

data Status = Ready | Running | Unsat | SatS | Bound deriving (Eq, Show)

data World = World
  { wProb    :: !Int                          -- 問題の番号（-1 = でたらめ）
  , wTitle   :: String
  , wCls     :: CSet
  , wGiven   :: !Int
  , wPartner :: !Int
  , wStatus  :: !Status
  , wAuto    :: !Bool
  , wPick    :: [Int]                         -- クリックで選んだ節
  , wNote    :: String
  , wBrute   :: !Bool                         -- 最初の節の総当たりの答え（充足可能か）
  , wRng     :: Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-03 Resolution" (loadProblem 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadProblem 0 world0) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (stepAuto w)
  , skDraw  = draw
  , skEvent = event }

-- 乱数の列は自己テストのあとの続き（p5.js 版と同じ）
world0 :: World
world0 = World 0 "" (startSet []) 1 0 Ready False [] "" True rngAfterTest []

clauseKey :: Clause -> String
clauseKey c = joinWith "," c

clauseStr :: Clause -> String
clauseStr c = if null c then "[]" else joinWith " | " c

negLit :: String -> String
negLit l = case l of
  ('~' : r) -> r
  _ -> '~' : l

isTautCl :: Clause -> Bool                    -- p | ~p を含む
isTautCl c = any (\x -> negLit x `elem` c) c

-- a と b の導出：打ち消し合う文字 1 組ごとに 1 つ。恒真（p | ~p を含む）はすてる。返り値 [(節, 消した文字)]
resolvents :: Clause -> Clause -> [(Clause, String)]
resolvents a b =
  [ (c, l) | l <- a, negLit l `elem` b
  , let c = litSort (filter (/= l) a ++ filter (/= negLit l) b)
  , not (isTautCl c) ]

startSet :: [Clause] -> CSet
startSet list = foldl' (\s c -> fst (addClause s c Nothing "")) (CSet Q.empty S.empty False) list

-- 新しい節なら足す。返り値 (集合, 足したか)
addClause :: CSet -> Clause -> Maybe (Int, Int) -> String -> (CSet, Bool)
addClause s c from pv
  | S.member c (csKeys s) = (s, False)
  | otherwise = (CSet (csList s Q.|> Cl c from pv) (S.insert c (csKeys s)) (csEmpty s || null c), True)

clAt :: CSet -> Int -> Cl
clAt s i = maybe (Cl [] Nothing "") id (Q.lookup i (csList s))

csLen :: CSet -> Int
csLen = Q.length . csList

loadProblem :: Int -> World -> World
loadProblem k w =
  let (t, fs) = at' problems k ("", [])
      cls = concat [ either (const []) toClauses (parseFormula s) | s <- fs ]
  in resetWith (startSet cls) w { wProb = k, wTitle = t }

resetWith :: CSet -> World -> World
resetWith s w = w { wCls = s, wGiven = 1, wPartner = 0, wStatus = if csEmpty s then Unsat else Ready, wAuto = False, wPick = [], wNote = ""
                  , wBrute = bruteSat (initial s) }

initial :: CSet -> [Clause]                   -- 最初の節（作った節でないもの）
initial s = [ clC x | x <- foldr (:) [] (csList s), clFrom x == Nothing ]

-- ★ 幅優先の飽和：「given 番目の節」と「それより前の全部の節」の組を順に調べる（組は 1 回ずつ）
-- 返り値 (Unsat / SatS（もう新しい節がない）/ Bound / Running, 集合, (given, partner))
saturate :: CSet -> (Int, Int) -> Int -> (Status, CSet, (Int, Int))
saturate set0 st0 budget0 = go set0 st0 budget0
  where
    go set st@(g, p) budget
      | budget <= 0 = (Running, set, st)
      | csEmpty set = (Unsat, set, st)
      | g >= csLen set = (SatS, set, st)
      | p >= g = go set (g + 1, 0) (budget - 1)
      | otherwise =
          let st' = (g, p + 1)
              rs = resolvents (clC (clAt set g)) (clC (clAt set p))
              addAll s [] = Left s
              addAll s ((c, pv) : more) =
                let (s', added) = addClause s c (Just (p, g)) pv
                in if added && null c then Right (Unsat, s')
                   else if csLen s' >= maxClauses then Right (Bound, s')
                   else addAll s' more
          in case addAll set rs of
               Right (r, s') -> (r, s', st')
               Left s' -> go s' st' (budget - 1)

-- 総当たり：全部の割り当てで節が全部真になるものがあるか
bruteSat :: [Clause] -> Bool
bruteSat list =
  let vars = S.toList (S.fromList [ filter (/= '~') l | c <- list, l <- c ])
      litVal e l = case l of
        ('~' : r) -> not (M.findWithDefault False r e)
        _ -> M.findWithDefault False l e
  in or [ all (any (litVal e)) list | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

-- でたらめな節の集合（乱数は p5.js 版と同じ順に引く）
randomSet :: Int -> Int -> R [Clause]
randomSet nv nc = do
  let vs = take nv "pqrs"
  cs <- mapM (const (do
          j <- rndInt 1 3
          ls <- mapM (const (do
                  v <- rndPick vs
                  u <- rnd
                  pure (if u < 0.5 then [v] else ['~', v]))) [1 .. j]
          pure (litSort ls))) [1 .. nc]
  pure (filter (not . isTautCl) cs)

-- スペースで自動のとき、1 フレームに pairsPerFrame 組だけ進める
stepAuto :: World -> World
stepAuto w
  | wAuto w && wStatus w `notElem` [Unsat, SatS, Bound] =
      let (s, set, (g, p)) = saturate (wCls w) (wGiven w, wPartner w) pairsPerFrame
      in w { wCls = set, wGiven = g, wPartner = p, wStatus = s, wAuto = s == Running }
  | otherwise = w

-- 見せる行（最初の節（半分まで）と、新しい節）：(節の番号, 上の y)
shownRows :: Env -> World -> [(Int, Float)]
shownRows env w =
  let set = wCls w
      n = csLen set
      lh = 22
      rows = max 2 (floor ((envH env - 214) / lh))
      nInit = maybe n id (Q.findIndexL (\x -> clFrom x /= Nothing) (csList set))
      first = [0 .. min nInit (rows `div` 2) - 1]
      restStart = max (length first) (n - (rows - length first))
      idx = first ++ [restStart .. n - 1]
  in [ (i, 104 + i2f r * lh) | (r, i) <- zip [0 :: Int ..] idx ]

draw :: Env -> World -> Picture
draw env w =
  let wd = envW env
      ht = envH env
      set = wCls w
      rows = shownRows env w
      clauseChars = floor ((wd * 0.2 - 8) / (15 * 0.72 * 1.15))
      row k (i, y) =
        let x = clAt set i
            gap = case at' rows (k - 1) (-1, 0) of
              (pi', _) | k > 0 && i /= pi' + 1 ->
                [ say ("... (" ++ show (i - pi' - 1) ++ " not shown)") 150 (y + 2) 14 (col dimc), Color (col linec) (lineW 1 (32, y - 2) (140, y - 2)) ]
              _ -> []
        in gap
           ++ [ Color (col linec) (rectTL 26 (y - 2) (wd * 0.5) 20) | i `elem` wPick w ]
           ++ [ say ("C" ++ show (i + 1)) 32 (y + 14) 15 (col dimc)
              , say (clip clauseChars (clauseStr (clC x))) 80 (y + 14) 15 (col (if null (clC x) then pink else if clFrom x /= Nothing then yel else ink)) ]
           ++ case clFrom x of
                Just (a, b) -> [ say ("<- C" ++ show (a + 1) ++ " & C" ++ show (b + 1) ++ " on " ++ filter (/= '~') (clPivot x)) (80 + wd * 0.2) (y + 14) 14 (col dimc) ]
                Nothing -> []
      msg = case wStatus w of
        Ready -> "Space = automatic search, or click two clauses"
        Running -> "searching... " ++ show (csLen set) ++ " clauses (checking clause " ++ show (wGiven w + 1) ++ ")"
        Unsat -> "[] derived! This clause set is unsatisfiable (contradiction)"
        SatS -> "No new clauses (saturated). No [] -> satisfiable"
        Bound -> "Hit the limit of " ++ show maxClauses ++ " clauses: undecided"
      mc = case wStatus w of
        Unsat -> pink
        SatS -> grn
        Bound -> yel
        _ -> ink
      ei = Q.findIndexL (null . clC) (csList set)
  in view env $
       [ hudTitle ("Resolution: " ++ (if wProb w >= 0 then "problem " ++ show (wProb w + 1) ++ "  " ++ wTitle w else "(random clauses)"))
           "1-5 problems   R random   Space automatic search (breadth-first)   N one round   C clear derived clauses\nClick two clauses = resolve by hand" ]
       ++ concat (zipWith row [0 ..] rows)
       ++ [ say msg 30 (ht - 76) 20 (col mc)
          , say ((if null (wNote w) then "" else wNote w ++ "   ") ++ "Brute force says: " ++ (if wBrute w then "satisfiable" else "unsatisfiable")) 30 (ht - 48) 14 (col dimc)
          , say "DPLL (series 18 / 03) looks for an assignment; resolution looks for a contradiction" 30 (ht - 26) 14 (col dimc) ]
       ++ (case ei of
             Just e -> [drawRefutation set e (wd * 0.53 + 30) 130 (wd * 0.45 - 60) (ht - 240)]   -- 端の節が画面からはみ出さないように少し内側に
             Nothing -> [ say "When [] appears, its derivation tree is drawn here" (wd * 0.55) 130 14 (col dimc)
                        , say "([] at the bottom, the starting clauses on top)" (wd * 0.55) 150 14 (col dimc) ])
       ++ [drawSelfTest env (wd - 180) (ht - 6) (wTest w)]

-- [] から親をたどった木（根 [] を下に）。混みあうときは節の文字の代わりに番号 C1 … を書く
drawRefutation :: CSet -> Int -> Float -> Float -> Float -> Float -> Picture
drawRefutation set ei x y w h =
  let kids i = case clFrom (clAt set i) of
        Just (a, b) -> [a, b]
        Nothing -> []
      lay = layoutTree ei kids 80
      ns = tlNodes lay
      dy = if tlDepth lay > 0 then min 54 (h / i2f (tlDepth lay)) else 0
      px o = x + tnX o * w
      py o = y + h - i2f (tnDepth o) * dy
      arr = M.fromList (zip [0 :: Int ..] ns)
      spacing = if tlLeaves lay > 1 then w / i2f (tlLeaves lay - 1) else w
      full = maxOr 0 [ textW (clauseStr (clC (clAt set (tnNode o)))) 14 + 10 | o <- ns ] <= spacing
      lab i = if full then clauseStr (clC (clAt set i)) else if null (clC (clAt set i)) then "[]" else "C" ++ show (i + 1)
      box o =
        let i = tnNode o
            s = lab i
            tw = textW s 14 + 10
            c = clAt set i
            sc = if null (clC c) then pink else if clFrom c /= Nothing then yel else ink
        in Pictures [ Color (col bgc) (rectTL (px o - tw / 2) (py o - 11) tw 22), Color (col sc) (frameTL 1.5 (px o - tw / 2) (py o - 11) tw 22)
                    , sayM s (px o) (py o) 14 (col ink) ]
  in Pictures ( [ Color (col linec) (lineW 1.5 (px p, py p) (px o, py o)) | o <- ns, Just p <- [M.lookup (tnParent o) arr] ]
             ++ map box ns
             ++ [ say ("Refutation tree of [] (clauses used: " ++ show (length ns) ++ (if length ns >= 80 then ", cut at 80" else "") ++ ")"
                       ++ (if full then "" else ", labels = clause numbers")) (x - 30) (y - 14) 14 (col dimc) ] )

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = toP5 env pos
        hit = [ i | (i, y) <- shownRows env w, inBox m (26, y - 2, envW env * 0.5, 22) ]
    in case hit of
         (i : _) -> clickClause i w
         [] -> w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> loadProblem (fromEnum c - fromEnum '1') w
    | isKey 'r' c ->
        let (set, g) = runR (do n <- rndInt 4 7; randomSet 3 n) (wRng w)
        in (resetWith (startSet set) w { wProb = -1 }) { wRng = g }
    | isKey 'n' c -> oneRound w
    | isKey 'c' c -> resetWith (startSet (initial (wCls w))) w
  _ -> w

-- N：given 番目の節を、それより前の全部の節と 1 回ずつ組み合わせる
oneRound :: World -> World
oneRound w = go (wCls w) (wGiven w, 0)
  where
    g0 = wGiven w
    go set st@(g, _)
      | g /= g0 = w { wCls = set, wGiven = g, wPartner = 0 }
      | otherwise = case saturate set st 1 of
          (Running, set', st') -> go set' st'
          (s, set', (g', _)) -> w { wCls = set', wGiven = g', wPartner = 0, wStatus = s }

-- 節のクリック：2 つ選んだら手で導出する
clickClause :: Int -> World -> World
clickClause i w
  | i `elem` wPick w = w { wPick = filter (/= i) (wPick w) }
  | length pick < 2 = w { wPick = pick }
  | otherwise =
      let set = wCls w
          (p0, p1) = (at' pick 0 0, at' pick 1 0)
          rs = resolvents (clC (clAt set p0)) (clC (clAt set p1))
          a = min p0 p1
          b = max p0 p1
          (set', added) = foldl' (\(s, acc) (c, pv) -> let (s2, ok) = addClause s c (Just (a, b)) pv in (s2, if ok then acc ++ [c] else acc)) (set, []) rs
          note
            | null rs = "C" ++ show (a + 1) ++ " & C" ++ show (b + 1) ++ ": no clashing literals (or only tautologies)"
            | null added = "already there"
            | otherwise = "by hand: " ++ joinWith ", " (map clauseStr added)
      in w { wCls = set', wNote = note, wPick = [], wStatus = if csEmpty set' then Unsat else wStatus w }
  where pick = wPick w ++ [i]

-- ---------- やってみよう ----------
-- 1. 問題 1 を手で：C1 (p | q) と C2 (~p | r) → (q | r)。続けて [] まで何回で行ける？ 自動（スペース）とくらべよう
-- 2. 問題 3 はなぜ [] が出ない？ 全部の節を真にする割り当てを 1 つ見つけよう（それが「充足可能」の証拠）
-- 3. 問題 4 の鳩の巣は、鳩を 4 羽・巣を 3 つにすると節が一気に増える。problems に足して、作る節の数をくらべよう
-- 4. 「前提 |- 結論」を調べるには「結論の否定」を足す。自分の三段論法を problems に書いてみよう
-- 5. Prolog（05）は、この導出を「ホーン節」（正のリテラルが 1 つまでの節）に限って速くしたもの

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
-- 乱数を引く部分：でたらめな節の集合 150 個（種 3）。あとの乱数の列（R キー）もここから続く
randomSets :: R [[Clause]]
randomSets = mapM (const (do n <- rndInt 3 8; randomSet 3 n)) [1 .. 150 :: Int]

testSets :: [[Clause]]
rngAfterTest :: Rng
(testSets, rngAfterTest) = runR randomSets (rngOf 3)

selfTest :: [(String, Bool)]
selfTest =
  [ ("(p ∨ q) と (¬p ∨ r) の導出は (q ∨ r)、(p ∨ q) と (¬p ∨ ¬q) は恒真しか出ないので何も作らない"
    , map (clauseKey . fst) r1 == ["q,r"] && null (resolvents ["p", "q"] ["~p", "~q"]))
  , ("(p) と (¬p) の導出は空節 □", map fst (resolvents ["p"] ["~p"]) == [[]])
  , ("でたらめな節の集合 " ++ show nT ++ " 個（変数 3 つ、うち充足不能 " ++ show unsatN ++ " 個）：□ が出る ⇔ 総当たりで充足不能"
    , agree == nT && unsatN > 10 && unsatN < nT)
  , ("作った節は全部、記録した 2 つの親の本当の導出になっている", treeOk)
  , ("問題 1・2・4（鳩の巣）・5 は □ が出る、問題 3 は飽和して □ が出ない", res == [Unsat, Unsat, SatS, Unsat, Unsat])
  , ("問題の答えは総当たりとも一致", and [ bruteSat (map clC (foldr (:) [] (csList (wCls (loadProblem k world0))))) == (r == SatS) | (k, r) <- zip [0 ..] res ])
  , ("p → q を節にすると (¬p ∨ q)、¬(p → r) は (p) と (¬r) の 2 つ"
    , map clauseKey (take 1 (cl "p -> q")) == ["~p,q"] && joinWith "|" (map clauseKey (cl "~(p -> r)")) == "p|~r") ]
  where
    r1 = resolvents ["p", "q"] ["~p", "r"]
    nT = length testSets
    runs = [ (r, set) | list <- testSets, let s0 = startSet list, let (r, set, _) = saturate s0 (1, 0) 1000000 ]
    agree = length [ () | (r, set) <- runs, (r == Unsat) == not (bruteSat (map clC (foldr (:) [] (csList set)))), r /= Bound ]
    unsatN = length [ () | (r, _) <- runs, r == Unsat ]
    treeOk = and [ any (\(y, _) -> y == clC x) (resolvents (clC (clAt set a)) (clC (clAt set b)))
                 | (_, set) <- runs, x <- foldr (:) [] (csList set), Just (a, b) <- [clFrom x] ]
    res = [ let (r, _, _) = saturate (wCls (loadProblem k world0)) (1, 0) 1000000 in r | k <- [0 .. length problems - 1] ]
    cl s = either (const []) toClauses (parseFormula s)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

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

-- ---- 標準形の道具：-> <-> の除去 → 否定を内側へ（NNF）→ 分配（CNF / DNF）を 1 歩ずつ
data NfTarget = CNF | DNF deriving (Eq, Show)

type Rule = F -> Maybe (F, String)            -- 構文木 1 か所 → (書きかえた式, 規則の名前)

-- 段階ごとの書きかえ規則（0 = -> <-> の除去、1 = 否定を内側へ、2 = 分配）
nfRules :: Int -> NfTarget -> [Rule]
nfRules phase target
  | phase == 0 =
      [ \f -> case f of FBin OIff a b -> Just (FBin OAnd (FBin OImp a b) (FBin OImp b a), "A <-> B => (A -> B) & (B -> A)"); _ -> Nothing
      , \f -> case f of FBin OImp a b -> Just (FBin OOr (FNot a) b, "A -> B => ~A | B"); _ -> Nothing ]
  | phase == 1 =
      [ \f -> case f of FNot (FNot a) -> Just (a, "~~A => A"); _ -> Nothing
      , \f -> case f of FNot (FBin OAnd a b) -> Just (FBin OOr (FNot a) (FNot b), "~(A & B) => ~A | ~B (De Morgan)"); _ -> Nothing
      , \f -> case f of FNot (FBin OOr a b) -> Just (FBin OAnd (FNot a) (FNot b), "~(A | B) => ~A & ~B (De Morgan)"); _ -> Nothing
      , \f -> case f of FNot FTop -> Just (FBot, "~T => _|_"); _ -> Nothing
      , \f -> case f of FNot FBot -> Just (FTop, "~_|_ => T"); _ -> Nothing
      , \f -> case f of
          FBin op a b | (op == OAnd || op == OOr) && (isConst a || isConst b) -> Just (constRule f)
          _ -> Nothing ]
  | otherwise =
      let (o, i) = if target == CNF then (OOr, OAnd) else (OAnd, OOr)
          so = opSym o
          si = opSym i
      in [ \f -> case f of
             FBin o' a (FBin i' b c) | o' == o && i' == i ->
               Just (FBin i (FBin o a b) (FBin o a c), "A " ++ so ++ " (B " ++ si ++ " C) => (A " ++ so ++ " B) " ++ si ++ " (A " ++ so ++ " C) (distribute)")
             _ -> Nothing
         , \f -> case f of
             FBin o' (FBin i' a b) c | o' == o && i' == i ->
               Just (FBin i (FBin o a c) (FBin o b c), "(A " ++ si ++ " B) " ++ so ++ " C => (A " ++ so ++ " C) " ++ si ++ " (B " ++ so ++ " C) (distribute)")
             _ -> Nothing ]
  where isConst g = g == FTop || g == FBot

-- T と _|_ の整理：A & T => A、A | T => T など
constRule :: F -> (F, String)
constRule f = case f of
  FBin op a b ->
    let (c, other) = if a == FTop || a == FBot then (a, b) else (b, a)
    in if op == OAnd then (if c == FTop then (other, "A & T => A") else (FBot, "A & _|_ => _|_"))
       else (if c == FBot then (other, "A | _|_ => A") else (FTop, "A | T => T"))
  _ -> (f, "")

-- 書きかえ 1 回の結果：新しい式、規則の名前、書きかえた部分式、その場所（根からの道：0 = 左の子、1 = 右の子）
data RW = RW { rwF :: F, rwName :: String, rwAt :: F, rwPath :: [Int] }

-- 外側・左から順に、最初に当てはまる 1 か所だけ書きかえる
rewriteOnce :: F -> [Rule] -> Maybe RW
rewriteOnce f rules = case [ o | r <- rules, Just o <- [r f] ] of
  ((g, nm) : _) -> Just (RW g nm f [])
  [] -> case f of
    FBin op a b -> case rewriteOnce a rules of
      Just o -> Just o { rwF = FBin op (rwF o) b, rwPath = 0 : rwPath o }
      Nothing -> case rewriteOnce b rules of
        Just o -> Just o { rwF = FBin op a (rwF o), rwPath = 1 : rwPath o }
        Nothing -> Nothing
    FNot a -> sub1 FNot a
    FBox a -> sub1 FBox a
    FDia a -> sub1 FDia a
    _ -> Nothing
  where sub1 k a = case rewriteOnce a rules of
          Just o -> Just o { rwF = k (rwF o), rwPath = 0 : rwPath o }
          Nothing -> Nothing

-- 標準形にする 1 歩
data NfStep = NfStep { nsF :: F, nsName :: String, nsPhase :: !Int, nsAt :: Maybe F, nsPath :: Maybe [Int] }

-- 全部の手順。返り値 (手順, 結果, 上限に当たったか)
normalSteps :: F -> NfTarget -> Int -> Int -> ([NfStep], F, Bool)
normalSteps f target maxSteps maxSize = goPh 0 [NfStep f "start" (-1) Nothing Nothing] 1 f
  where
    goPh ph acc n cur
      | ph >= 3 = (reverse acc, cur, False)
      | otherwise = go ph (nfRules ph target) acc n cur
    go ph rules acc !n cur = case rewriteOnce cur rules of
      Nothing -> goPh (ph + 1) acc n cur
      Just o ->
        let cur' = rwF o
            acc' = NfStep cur' (rwName o) ph (Just (rwAt o)) (Just (rwPath o)) : acc
            n' = n + 1
        in if n' > maxSteps || fmlSize cur' > maxSize then (reverse acc', cur', True) else go ph rules acc' n' cur'

isLit :: F -> Bool
isLit f = case f of
  FVar _ -> True
  FNot (FVar _) -> True
  _ -> False

isNNF :: F -> Bool
isNNF f = case f of
  FNot a -> case a of FVar _ -> True; _ -> False
  FBin OImp _ _ -> False
  FBin OIff _ _ -> False
  _ -> all isNNF (fKids f)

isFlatOf :: F -> Op -> (F -> Bool) -> Bool
isFlatOf f op inner = case f of
  FBin o a b | o == op -> isFlatOf a op inner && isFlatOf b op inner
  _ -> inner f

isCNF, isDNF :: F -> Bool
isCNF f = f == FTop || f == FBot || isFlatOf f OAnd (\c -> isFlatOf c OOr isLit)
isDNF f = f == FTop || f == FBot || isFlatOf f OOr (\c -> isFlatOf c OAnd isLit)

flatten :: F -> Op -> [F]
flatten f op = case f of
  FBin o a b | o == op -> flatten a op ++ flatten b op
  _ -> [f]

litStr :: F -> String                         -- リテラルの文字列 'p' / '~p'
litStr f = case f of
  FVar n -> n
  FNot (FVar n) -> '~' : n
  _ -> fmlStr f

-- 重なりを除いて、変数の名前の順に（p の次に ~p）
litSort :: [String] -> [String]
litSort c = sortOn key (nubOrd c)
  where key l = case l of
          ('~' : r) -> r ++ "~"
          _ -> l

-- CNF の木 → 節のリスト（節 = リテラルの文字列の並び）。T = 節なし、_|_ = 空節 1 つ
cnfClauses :: F -> [[String]]
cnfClauses f
  | f == FTop = []
  | f == FBot = [[]]
  | otherwise = [ litSort (map litStr (flatten c OOr)) | c <- flatten f OAnd ]

-- 速い直接の CNF 変換（03 の節の集合づくり用）。節の重複・恒真の節は除く
toClauses :: F -> [[String]]
toClauses f =
  let (_, n, _) = normalSteps f CNF maxBound maxBound
      taut c = any (\l -> take 1 l /= "~" && ('~' : l) `elem` c) c
  in nubOrd (filter (not . taut) (cnfClauses n))

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
