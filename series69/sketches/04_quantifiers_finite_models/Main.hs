-- ============================================
-- 69-04 量化子と有限モデル ─ 点（もの）と矢印（関係 R）の小さな世界で、∀（すべての）∃（ある）を含む式の真偽を調べる
-- Ax Ey R(x,y)「どの点からも矢印が出ている」と Ey Ax R(x,y)「全員から矢印が来る 1 点がある」は順番だけちがうが意味がちがう。
-- 世界が有限なら、∀ は「全部の点で調べる」、∃ は「1 つ見つかればよい」で計算できる。光る矢印が証拠（∃ の見つけた点）
-- 操作：空いた所をクリック = 点を足す（最初の 1 回はキーを届かせるだけ）/ 点から点へドラッグ = 矢印 R(x,y) の切りかえ（同じ点へ戻ると自分への矢印）
--       点をクリック = 性質 P の切りかえ（黄色い点）/ 点を空いた所へドラッグ = 動かす / Backspace 最後の点を消す
--       ↑↓ 式を選ぶ / ← → x の値を選ぶ / 1 輪（a→b→c→a）2 星 3 空っぽ / R でたらめな世界
-- 画面の記号：A = ∀、E = ∃、~ = ¬、& = ∧、| = ∨、-> = →
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

formulas :: [(String, String)]                -- ★ 調べる式（∀ ∃ ¬ ∧ ∨ →、R(x,y)、P(x)、x = y。A E ~ & | -> でも書ける）と説明
formulas =
  [ ("∀x ∃y R(x,y)", "every point has an outgoing arrow")
  , ("∃y ∀x R(x,y)", "some point gets arrows from everyone (itself too)")
  , ("∃x ∀y R(x,y)", "some point sends arrows to everyone")
  , ("∀x ∃y R(y,x)", "every point has an incoming arrow")
  , ("∀x R(x,x)", "reflexive")
  , ("∀x ∀y (R(x,y) → R(y,x))", "symmetric")
  , ("∀x ∀y ∀z (R(x,y) ∧ R(y,z) → R(x,z))", "transitive")
  , ("∀x (P(x) → ∃y (R(x,y) ∧ ¬P(y)))", "every yellow point sends an arrow to a non-yellow one")
  , ("∃y ∀x R(x,y) → ∀x ∃y R(x,y)", "EA implies AE (always true)")
  , ("∀x ∃y R(x,y) → ∃y ∀x R(x,y)", "AE implies EA? (false in some worlds)") ]

maxPts :: Int
maxPts = 7                                    -- ★ 点の数の上限

pr :: Float
pr = 22                                       -- ★ 点の半径

names :: String
names = "abcdefg"

-- 一階の式
data FO = FR Char Char | FP Char | FEq Char Char | FONot FO | FOAnd FO FO | FOOr FO FO | FOImp FO FO
        | FAll Char FO | FEx Char FO
  deriving (Eq, Show)

data Pt = Pt { ptX :: !Float, ptY :: !Float, ptP :: !Bool }

data World = World
  { wPts      :: [Pt]
  , wRel      :: S.Set (Int, Int)             -- 矢印 R(i,j)
  , wF        :: !Int                         -- 選んでいる式
  , wXSel     :: !Int                         -- x の値
  , wFocus    :: !Bool                        -- 最初のクリックをすませたか
  , wDragFrom :: !Int
  , wMoved    :: !Bool
  , wRng      :: Rng                          -- R でたらめな世界の乱数（自己テストの続き）
  , wTest     :: [(String, Bool)] }

parsedF :: [Maybe FO]
parsedF = map (foParse . fst) formulas

main :: IO ()
main = runSketch (sketch "69-04 Quantifiers and Finite Models" w0)
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure w0
  , skDraw  = draw
  , skEvent = event }
  where w0 = exampleWorld 1280 800 1 (World [] S.empty 0 0 False (-1) False rngAfterTest selfTest)

-- 例の世界：1 輪、2 星、3 空っぽ、4 でたらめ
exampleWorld :: Float -> Float -> Int -> World -> World
exampleWorld wd ht k w =
  let cx = wd * 0.3
      cy = ht * 0.52
      rr = min wd ht * 0.2
      ring n = [ Pt (cx + rr * cos a) (cy + rr * sin a) False | i <- [0 .. n - 1], let a = -pi / 2 + i2f i * 2 * pi / i2f n ]
      w1 = case k of
        1 -> w { wPts = setP0 (ring 3), wRel = S.fromList [(0, 1), (1, 2), (2, 0)] }
        2 -> w { wPts = ring 4, wRel = S.fromList [ (i, 0) | i <- [0 .. 3] ] }
        3 -> w { wPts = ring 3, wRel = S.empty }
        4 -> let ((ps, rel), g) = runR (randomWorld ring) (wRng w) in w { wPts = ps, wRel = rel, wRng = g }
        _ -> w { wRel = S.empty }
  in w1 { wXSel = 0 }
  where setP0 ps = case ps of
          (p : r) -> p { ptP = True } : r
          [] -> []

randomWorld :: (Int -> [Pt]) -> R ([Pt], S.Set (Int, Int))
randomWorld ring = do
  n <- rndInt 3 5
  let ps0 = ring n
  rows <- mapM (\_ -> do
                  p <- (< 0.4) <$> rnd
                  js <- mapM (\_ -> (< 0.3) <$> rnd) [0 .. n - 1]
                  pure (p, js)) [0 .. n - 1]
  let ps = [ pt { ptP = p } | (pt, (p, _)) <- zip ps0 rows ]
      rel = S.fromList [ (i, j) | (i, (_, js)) <- zip [0 ..] rows, (j, b) <- zip [0 ..] js, b ]
  pure (ps, rel)

-- ---- 一階の式：小さな構文解析（∀x φ・∃x φ・R(x,y)・P(x)・x = y と ¬ ∧ ∨ →。A E ~ & | -> でも可）。読めなければ Nothing
foParse :: String -> Maybe FO
foParse s0 = case imp s0 of
  Just (f, rest) | all (== ' ') rest -> Just f
  _ -> Nothing
  where
    ws = dropWhile (== ' ')
    eat c s = let t = ws s in if take (length c) t == c then Just (drop (length c) t) else Nothing
    eatAny cs s = case [ r | c <- cs, Just r <- [eat c s] ] of
      (r : _) -> Just r
      [] -> Nothing
    vname s = case ws s of
      (c : r) | c >= 'a' && c <= 'z' -> Just (c, r)
      _ -> Nothing
    imp s = do
      (a, r) <- orr s
      case eatAny ["→", "->"] r of
        Just r2 -> do (b, r3) <- imp r2; Just (FOImp a b, r3)
        Nothing -> Just (a, r)
    orr s = andd s >>= uncurry (loop FOOr ["∨", "|"] andd)
    andd s = un s >>= uncurry (loop FOAnd ["∧", "&"] un)
    loop k ops sub a r = case eatAny ops r of
      Just r2 -> do (b, r3) <- sub r2; loop k ops sub (k a b) r3
      Nothing -> Just (a, r)
    un s
      | Just r <- eatAny ["¬", "~"] s = do (a, r2) <- un r; Just (FONot a, r2)
      | Just r <- eatAny ["∀", "A"] s = do (v, r2) <- vname r; (a, r3) <- un r2; Just (FAll v a, r3)
      | Just r <- eatAny ["∃", "E"] s = do (v, r2) <- vname r; (a, r3) <- un r2; Just (FEx v a, r3)
      | Just r <- eat "(" s = do (a, r2) <- imp r; r3 <- eat ")" r2; Just (a, r3)
      | Just r <- eat "R(" s = do
          (a, r2) <- vname r
          let r3 = maybe r2 id (eat "," r2)
          (b, r4) <- vname r3
          r5 <- eat ")" r4
          Just (FR a b, r5)
      | Just r <- eat "P(" s = do (a, r2) <- vname r; r3 <- eat ")" r2; Just (FP a, r3)
      | otherwise = do (a, r) <- vname s; r2 <- eat "=" r; (b, r3) <- vname r2; Just (FEq a b, r3)

-- 世界：点 0..n-1、関係 rel i j、性質 pv i。env = 変数 → 点の番号
foEval :: FO -> M.Map Char Int -> Int -> (Int -> Int -> Bool) -> (Int -> Bool) -> Bool
foEval f env n rel pv = case f of
  FR x y -> rel (v x) (v y)
  FP x -> pv (v x)
  FEq x y -> v x == v y
  FONot a -> not (ev a)
  FOAnd a b -> ev a && ev b
  FOOr a b -> ev a || ev b
  FOImp a b -> not (ev a) || ev b
  FAll x a -> all (\d -> foEval a (M.insert x d env) n rel pv) [0 .. n - 1]
  FEx x a -> any (\d -> foEval a (M.insert x d env) n rel pv) [0 .. n - 1]
  where v x = M.findWithDefault (-1) x env
        ev a = foEval a env n rel pv

evalHere :: World -> FO -> M.Map Char Int -> Bool
evalHere w f env = foEval f env (length (wPts w)) (\i j -> S.member (i, j) (wRel w)) (\i -> maybe False ptP (ptAt w i))

ptAt :: World -> Int -> Maybe Pt
ptAt w i = if i < 0 then Nothing else case drop i (wPts w) of
  (p : _) -> Just p
  [] -> Nothing

-- ∃ なら最初の証拠の点、∀ なら最初の反例の点（なければ -1）
witness :: World -> FO -> M.Map Char Int -> Int
witness w f env = case f of
  FEx x a -> first x a id
  FAll x a -> first x a not
  _ -> -1
  where first x a want = headOr (-1) [ d | d <- [0 .. length (wPts w) - 1], want (evalHere w a (M.insert x d env)) ]

-- 画面用の ASCII（∀ → A、∃ → E、¬ → ~、∧ → &、∨ → |、→ → ->）
foAscii :: String -> String
foAscii = concatMap (\c -> case c of
  '∀' -> "A"
  '∃' -> "E"
  '¬' -> "~"
  '∧' -> "&"
  '∨' -> "|"
  '→' -> "->"
  _ -> [c])

-- 外側の量化子の変数の値ごとの説明と、光らせる矢印・印
data Explain = Explain { exLines :: [(String, Bool, Int)], exEdges :: [(Int, Int)], exX :: !Int, exY :: !Int }

explain :: World -> Explain
explain w = case (headOr Nothing (drop (wF w) parsedF), length (wPts w)) of
  (Just f, n) | n > 0, Just (v, inner) <- quant f ->
    let xs = clampI (wXSel w) 0 (n - 1)
        one d =
          let env = M.singleton v d
              val = evalHere w inner env
              base = [v] ++ " = " ++ [nm d] ++ ": " ++ (if val then "true" else "false")
          in case quant inner of
               Just (iv, ia) ->
                 let wt = witness w inner env
                     isEx = case inner of FEx _ _ -> True; _ -> False
                     s = base ++ (if wt >= 0 then " (" ++ [iv] ++ " = " ++ [nm wt] ++ (if isEx then " is a witness)" else " is a counterexample)")
                                  else if isEx then " (no " ++ [iv] ++ " found)" else " (holds for every " ++ [iv] ++ ")")
                     (e, y) = if d == xs && wt >= 0
                                then (case ia of FR a _ -> [if a == v then (d, wt) else (wt, d)]; _ -> [], wt)
                                else ([], -1)
                 in (s, val, d, e, y)
               Nothing ->
                 let e = case inner of FR a b | d == xs && a == v && b == v -> [(d, d)]; _ -> []
                 in (base, val, d, e, -1)
        rs = map one [0 .. n - 1]
    in Explain [ (s, val, d) | (s, val, d, _, _) <- rs ] (concat [ e | (_, _, _, e, _) <- rs ]) xs (maxOr (-1) [ y | (_, _, _, _, y) <- rs ])
  _ -> Explain [] [] (-1) (-1)
  where
    quant g = case g of
      FAll v a -> Just (v, a)
      FEx v a -> Just (v, a)
      _ -> Nothing
    nm d = at' names d '?'

draw :: Env -> World -> Picture
draw env w =
  let wd = envW env
      ht = envH env
      n = length (wPts w)
      ex = explain w
      hi (i, j) = (i, j) `elem` exEdges ex
      arrows = [ drawArrow w i j (if hi (i, j) then yel else linec) | i <- [0 .. n - 1], j <- [0 .. n - 1], S.member (i, j) (wRel w) ]
      dragLine = case ptAt w (wDragFrom w) of
        Just p | wMoved w -> [Color (col sky) (lineW 2 (ptX p, ptY p) (mouseP env))]
        _ -> []
      ptPic i p =
        let isX = i == exX ex
            isY = i == exY ex
            sc = if isX then sky else if isY then yel else ink
        in Pictures [ Color (col (if ptP p then (150, 128, 60) else panelc)) (circleAt (ptX p) (ptY p) (pr * 2))
                    , Color (col sc) (ringAt (ptX p) (ptY p) (pr * 2) (if isX || isY then 4 else 2))
                    , sayM [at' names i '?'] (ptX p) (ptY p) 18 (col ink) ]
      rx = wd * 0.58
      fRow k (s, _) =
        let v = case headOr Nothing (drop k parsedF) of
              Just g | n > 0 -> Just (evalHere w g M.empty)
              _ -> Nothing
            y = 108 + i2f k * 27
        in Pictures $ [ Color (col linec) (rectTL (rx - 8) (y - 18) (wd - rx - 12) 25) | k == wF w ]
             ++ [ say (maybe "?" (\b -> if b then "T" else "F") v) rx y 15 (col (maybe dimc (\b -> if b then grn else pink) v))
                , say (foAscii s) (rx + 30) y 15 (col ink) ]
      y0 = 108 + i2f (length formulas) * 27 + 16
      xs = clampI (wXSel w) 0 (n - 1)
  in view env $
       [ hudTitle "Quantifiers and finite models: the order of A and E changes the meaning"
           "Click: add a point / toggle P   Drag: arrow R / move a point   Up/Down formula   Left/Right choose x\n1 ring  2 star  3 empty  R random   Backspace delete the last point" ]
       ++ arrows ++ dragLine ++ zipWith ptPic [0 ..] (wPts w)
       ++ zipWith fRow [0 ..] formulas
       ++ [ say ("\"" ++ snd (at' formulas (wF w) ("", "")) ++ "\"") rx y0 15 (col yel) ]
       ++ [ say ((if d == xs then "> " else "  ") ++ s) rx (y0 + 26 + i2f k * 21) 14 (col (if d == xs then ink else dimc)) | (k, (s, _, d)) <- zip [0 :: Int ..] (exLines ex) ]
       ++ [ say "Blue ring = current x, yellow = witness / counterexample y" rx (ht - 86) 14 (col dimc)
          , say "The yellow arrow is that relation" rx (ht - 64) 14 (col dimc)
          , say (show n ++ " points, " ++ show (S.size (wRel w)) ++ " arrows (relations on n points: 2^(n*n))") rx (ht - 42) 14 (col dimc)
          , drawSelfTest env (wd - 180) (ht - 14) (wTest w) ]

drawArrow :: World -> Int -> Int -> RGB -> Picture
drawArrow w i j c = case (ptAt w i, ptAt w j) of
  (Just a, Just b)
    | i == j -> Color (col c) (ringAt (ptX a) (ptY a - pr - 12) 26 sw)
    | otherwise ->
        let dx = ptX b - ptX a
            dy = ptY b - ptY a
            l0 = hyp dx dy
            l = if l0 == 0 then 1 else l0
            ux = dx / l
            uy = dy / l
            off = if S.member (j, i) (wRel w) then 6 else 0
            x0 = ptX a + ux * pr - uy * off
            y0 = ptY a + uy * pr + ux * off
            x1 = ptX b - ux * (pr + 3) - uy * off
            y1 = ptY b - uy * (pr + 3) + ux * off
        in Color (col c) (Pictures [ lineW sw (x0, y0) (x1, y1)
                                   , Polygon [(x1, y1), (x1 - ux * 11 - uy * 5, y1 - uy * 11 + ux * 5), (x1 - ux * 11 + uy * 5, y1 - uy * 11 - ux * 5)] ])
  _ -> Blank
  where sw = if c == yel then 3 else 2

hitPt :: World -> (Float, Float) -> Int
hitPt w (x, y) = headOr (-1) [ i | (i, p) <- reverse (zip [0 ..] (wPts w)), hyp (ptX p - x) (ptY p - y) < pr ]

event :: Env -> Event -> World -> IO World
event env ev w = pure (fixX (handle ev))
  where
    m = mouseP env
    handle e = case e of
      EventKey (MouseButton LeftButton) Down _ pos
        | not (wFocus w) -> w { wFocus = True }      -- 最初の 1 回はキーを届かせるだけ
        | fst (toP5 env pos) > envW env * 0.56 -> w
        | otherwise ->
            let p = toP5 env pos
                h = hitPt w p
            in if h < 0 && length (wPts w) < maxPts && snd p > 80
                 then w { wDragFrom = h, wMoved = False, wPts = wPts w ++ [Pt (fst p) (snd p) False] }
                 else w { wDragFrom = h, wMoved = False }
      EventMotion _ | envDown env, Just p <- ptAt w (wDragFrom w) ->
        if hyp (fst m - ptX p) (snd m - ptY p) > pr then w { wMoved = True } else w
      EventKey (MouseButton LeftButton) Up _ pos | wDragFrom w >= 0 ->
        let p = toP5 env pos
            to = hitPt w p
            from = wDragFrom w
            w1 | not (wMoved w) && to == from = w { wPts = modifyAt from (\q -> q { ptP = not (ptP q) }) (wPts w) }
               | to >= 0 = w { wRel = if S.member (from, to) (wRel w) then S.delete (from, to) (wRel w) else S.insert (from, to) (wRel w) }
               | wMoved w = w { wPts = modifyAt from (\q -> q { ptX = clampF 30 (envW env * 0.55) (fst p), ptY = clampF 90 (envH env - 30) (snd p) }) (wPts w) }
               | otherwise = w
        in w1 { wDragFrom = -1 }
      EventKey k Down mods _ -> case typedKey k mods of
        TUp -> w { wF = wrapI (wF w - 1) (length formulas) }
        TDown -> w { wF = wrapI (wF w + 1) (length formulas) }
        TLeft -> w { wXSel = max 0 (wXSel w - 1) }
        TRight -> w { wXSel = min (length (wPts w) - 1) (wXSel w + 1) }
        TChar c
          | c >= '1' && c <= '3' -> exampleWorld (envW env) (envH env) (fromEnum c - fromEnum '0') w
          | isKey 'r' c -> exampleWorld (envW env) (envH env) 4 w
        TBack -> delLast
        TClear -> delLast
        _ -> w
      _ -> w
    delLast = let k = length (wPts w) - 1
              in if k < 0 then w else w { wPts = take k (wPts w), wRel = S.filter (\(a, b) -> a /= k && b /= k) (wRel w) }
    -- p5.js 版は draw の中で x の値をはさむ（外側が量化子の式で点があるとき）
    fixX w1 = let n = length (wPts w1)
                  q = case headOr Nothing (drop (wF w1) parsedF) of
                        Just (FAll _ _) -> True
                        Just (FEx _ _) -> True
                        _ -> False
              in if q && n > 0 then w1 { wXSel = clampI (wXSel w1) 0 (n - 1) } else w1

-- ---------- やってみよう ----------
-- 1. 輪（1）で、どの式が真でどれが偽？ 矢印を 1 本足して Ey Ax R(x,y) を真にするには？
-- 2. ← → で x を動かして、Ax Ey の「y の証拠」が x ごとに変わる様子を見よう。Ey Ax では y は 1 つに決まっている
-- 3. 反射的・対称的・推移的を全部みたす世界（同値関係）を作ろう。点はいくつかのグループに分かれる
-- 4. 最後の 2 つの式：EA -> AE はどんな世界でも真（妥当）。逆が偽になる一番小さい世界は？
-- 5. formulas に式を足そう：「矢印が来ない点がある」Ex Ay ~R(y,x)、「x = y でなければ矢印がある」など

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
-- でたらめな世界 200 個（乱数は p5.js 版と同じ順：点の数、関係の行列、性質）
randomWorlds :: R [(Int, [Bool], [Bool])]
randomWorlds = mapM (\_ -> do
  n <- rndInt 1 5
  mm <- mapM (const ((< 0.35) <$> rnd)) [1 .. n * n]
  pa <- mapM (const ((< 0.5) <$> rnd)) [1 .. n]
  pure (n, mm, pa)) [1 .. 200 :: Int]

testWorlds :: ([(Int, [Bool], [Bool])], Rng)
testWorlds = runR randomWorlds (rngOf 8)

rngAfterTest :: Rng                           -- 自己テストのあとの乱数（R の世界はこの続きから）
rngAfterTest = snd testWorlds

selfTest :: [(String, Bool)]
selfTest =
  [ ("式 " ++ show (length formulas) ++ " 個が全部読める", all (/= Nothing) parsedF)
  , ("輪 a→b→c→a：∀x∃y R(x,y) は真、∃y∀x R(x,y) は偽（順番で意味が変わる）", ev 0 3 cyc noP && not (ev 1 3 cyc noP))
  , ("でたらめな世界 " ++ show nW ++ " 個（点 1〜5 個）：式の計算 = 二重・三重ループで直接調べた答え（8 つの式）", agree == nW)
  , ("∃y∀x R(x,y) → ∀x∃y R(x,y) は全部の世界で真、逆は偽になる世界がある（" ++ show convFail ++ " 個）", valid && convFail > 0)
  , ("点 2 個の世界の関係 16 通り：∀x∃y R は (2²−1)² = 9 通り、∃y∀x R は 16 − 3² = 7 通りで真"
    , length [ () | m <- [0 .. 15 :: Int], ev 0 2 (bitsR m) noP ] == 9 && length [ () | m <- [0 .. 15 :: Int], ev 1 2 (bitsR m) noP ] == 7)
  , ("輪の世界で ∀x∃y R(x,y) の証拠：a → b、b → c、c → a（次の点）"
    , case headOr Nothing parsedF of
        Just (FAll _ inner) -> all (\d -> witness ringW inner (M.singleton 'x' d) == (d + 1) `mod` 3) [0, 1, 2]
        _ -> False)
  , ("おかしな式は読めない（null を返し、例外を出さない）", foParse "∀ R(x)" == Nothing && foParse "R(x,y" == Nothing && foParse "" == Nothing) ]
  where
    ev k n r pv = case headOr Nothing (drop k parsedF) of
      Just f -> foEval f M.empty n r pv
      Nothing -> False
    noP _ = False
    cyc i j = (i + 1) `mod` 3 == j
    bitsR m i j = testBit m (i * 2 + j)
    worlds = fst testWorlds
    nW = length worlds
    allN n p = all p [0 .. n - 1]
    someN n p = any p [0 .. n - 1]
    results = [ (direct == map (\k -> ev k n r pv) [0 .. 7], ev 8 n r noP, ev 9 n r noP)
              | (n, mm, pa) <- worlds
              , let r i j = at' mm (i * n + j) False
              , let pv i = at' pa i False
              , let direct = [ allN n (\x -> someN n (\y -> r x y)), someN n (\y -> allN n (\x -> r x y)), someN n (\x -> allN n (\y -> r x y))
                             , allN n (\x -> someN n (\y -> r y x)), allN n (\x -> r x x), allN n (\x -> allN n (\y -> not (r x y) || r y x))
                             , allN n (\x -> allN n (\y -> allN n (\z -> not (r x y && r y z) || r x z)))
                             , allN n (\x -> not (pv x) || someN n (\y -> r x y && not (pv y))) ] ]
    agree = length [ () | (a, _, _) <- results, a ]
    valid = and [ v | (_, v, _) <- results ]
    convFail = length [ () | (_, _, c) <- results, not c ]
    ringW = World [Pt 0 0 False, Pt 0 0 False, Pt 0 0 False] (S.fromList [(0, 1), (1, 2), (2, 0)]) 0 0 False (-1) False (rngOf 1) []

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
