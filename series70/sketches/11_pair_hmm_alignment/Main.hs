-- ============================================
-- 70-11 ペア HMM と配列アラインメント ─ 2 本の文字列（DNA 風の A C G T）を、1 つの HMM が「同時に」出していくと考える。状態は 3 つ：
--   M 一致：両方に 1 文字ずつ出す（同じ文字が出やすい）　X：上の文字列だけに 1 文字（下はすき間 −）　Y：下の文字列だけに 1 文字
-- 状態の道 1 本 = 並べ方（アラインメント）1 つ。
--   ビタビ = いちばん確率の高い並べ方。対数にすると「一致の点数・すき間の開始の罰・のばす罰」を足す動的計画法の表（Needleman–Wunsch、アフィンギャップ）と同じ
--   前向き = 全部の並べ方の確率の合計（2 本が「関係ある」確率）。前向き × 後ろ向きで、マスごとの「x_i と y_j が対応している確率」が出る
-- （文字を 1 つずつ直す回数を数える編集距離はシリーズ 12 の 04）
-- 操作：A C G T 文字を足す / Backspace 消す / Tab かクリックで上下を切りかえ / ↑↓ すき間の始まりやすさ δ / ← → すき間ののびやすさ ε / R 新しい 2 本
-- ※ A・C・G・T のキーは文字の入力に使う。操作は矢印・Tab・Backspace・R
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
import qualified Data.Array as A
import Data.Char (toUpper)
dna :: String
dna = "ACGT"

-- ★ δ = M → X（または Y）、ε = X → X、τ = 終わる、same = M が同じ文字を出す確率の合計
data PP = PP { pDelta, pEps, pTau, pSame :: !Double }

pp0 :: PP
pp0 = PP 0.1 0.3 0.05 0.85

maxLen :: Int
maxLen = 14

qBase :: Double
qBase = 0.25                                  -- X・Y が 1 文字を出す確率（4 文字とも同じ）

-- 並べ方の 1 列：状態（'M' 'X' 'Y'）、そこまでに使った x の文字数 i、y の文字数 j
data Col = Col { cS :: !Char, cI :: !Int, cJ :: !Int }

data Res = Res { rVit :: ([Col], Double), rFwd :: !Double, rPost :: [[Double]], rNw :: !Double, rBase :: !Double }

data World = World
  { wStrs   :: (String, String)
  , wActive :: !Int
  , wPP     :: PP
  , wRes    :: Res
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "70-11 Pair HMM and sequence alignment" world0)
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = recompute (World ("GATTACA", "GACTAA") 0 pp0 (Res ([], 0) 0 [] 0 0) (rngOf 81) [])

pMatch :: PP -> Char -> Char -> Double        -- M が (a, b) を出す確率（16 通りの合計 = 1）
pMatch p a b = if a == b then pSame p / 4 else (1 - pSame p) / 12

type Grid = A.Array (Int, Int)

mkGrid :: Int -> Int -> ((Int, Int) -> a) -> Grid a
mkGrid n m f = A.listArray ((0, 0), (n, m)) [ f (i, j) | i <- [0 .. n], j <- [0 .. m] ]

-- 最初にいちばん大きいもの
pick :: [(Double, Char)] -> (Double, Char)
pick cs = foldl' (\b c -> if fst c > fst b then c else b) (headOr (negInf, ' ') cs) cs

-- ★ ビタビ（対数）：vM ! (i, j) = x の i 文字目と y の j 文字目までを出して、状態 M で終わるいちばん良い並べ方の log 確率（と戻る先の状態）
pairViterbi :: String -> String -> PP -> ([Col], Double)
pairViterbi x y p = (cols, fst end + lg (pTau p))
  where
    n = length x
    m = length y
    xa = A.listArray (1, max 1 n) x
    ya = A.listArray (1, max 1 m) y
    lmm = lg (1 - 2 * pDelta p - pTau p)
    lgm = lg (1 - pEps p - pTau p)
    ld = lg (pDelta p)
    le = lg (pEps p)
    lq = lg qBase
    none = (negInf, ' ')
    vM = mkGrid n m $ \(i, j) -> if i > 0 && j > 0
           then let (b, s) = pick [(fst (vM A.! (i - 1, j - 1)) + lmm, 'M'), (fst (vX A.! (i - 1, j - 1)) + lgm, 'X'), (fst (vY A.! (i - 1, j - 1)) + lgm, 'Y')]
                in (b + lg (pMatch p (xa A.! i) (ya A.! j)), s)
           else if i == 0 && j == 0 then (0, ' ') else none
    vX = mkGrid n m $ \(i, j) -> if i > 0 then let (b, s) = pick [(fst (vM A.! (i - 1, j)) + ld, 'M'), (fst (vX A.! (i - 1, j)) + le, 'X')] in (b + lq, s) else none
    vY = mkGrid n m $ \(i, j) -> if j > 0 then let (b, s) = pick [(fst (vM A.! (i, j - 1)) + ld, 'M'), (fst (vY A.! (i, j - 1)) + le, 'Y')] in (b + lq, s) else none
    end = pick [(fst (vM A.! (n, m)), 'M'), (fst (vX A.! (n, m)), 'X'), (fst (vY A.! (n, m)), 'Y')]
    grid s = case s of { 'M' -> vM; 'X' -> vX; _ -> vY }
    cols = reverse (back n m (snd end))
    back i j s
      | not ((i > 0 || j > 0) && fst end > negInf) = []
      | otherwise = let pr = snd (grid s A.! (i, j))
                        (i', j') = case s of { 'M' -> (i - 1, j - 1); 'X' -> (i - 1, j); _ -> (i, j - 1) }
                    in Col s i j : back i' j' pr

-- ★ 前向き：max を「合計」に変えるだけ。全部の並べ方の確率の合計。(M, X, Y の表, 合計)
pairForward :: String -> String -> PP -> ((Grid Double, Grid Double, Grid Double), Double)
pairForward x y p = ((fM, fX, fY), pTau p * (fM A.! (n, m) + fX A.! (n, m) + fY A.! (n, m)))
  where
    n = length x
    m = length y
    xa = A.listArray (1, max 1 n) x
    ya = A.listArray (1, max 1 m) y
    mm = 1 - 2 * pDelta p - pTau p
    gm = 1 - pEps p - pTau p
    fM = mkGrid n m $ \(i, j) -> if i > 0 && j > 0 then pMatch p (xa A.! i) (ya A.! j) * (mm * fM A.! (i - 1, j - 1) + gm * (fX A.! (i - 1, j - 1) + fY A.! (i - 1, j - 1)))
                                 else if i == 0 && j == 0 then 1 else 0
    fX = mkGrid n m $ \(i, j) -> if i > 0 then qBase * (pDelta p * fM A.! (i - 1, j) + pEps p * fX A.! (i - 1, j)) else 0
    fY = mkGrid n m $ \(i, j) -> if j > 0 then qBase * (pDelta p * fM A.! (i, j - 1) + pEps p * fY A.! (i, j - 1)) else 0

-- 後ろ向き：bM ! (i, j) = (i, j) まで出して M にいるとき、残りを全部出して終わる確率。(M, X, Y の表)
pairBackward :: String -> String -> PP -> (Grid Double, Grid Double, Grid Double)
pairBackward x y p = (fst3 <$> cell, snd3 <$> cell, thd3 <$> cell)
  where
    n = length x
    m = length y
    xa = A.listArray (0, max 0 (n - 1)) x
    ya = A.listArray (0, max 0 (m - 1)) y
    mm = 1 - 2 * pDelta p - pTau p
    gm = 1 - pEps p - pTau p
    cell = mkGrid n m $ \(i, j) ->
      if i == n && j == m then (pTau p, pTau p, pTau p) else
        let em = if i < n && j < m then pMatch p (xa A.! i) (ya A.! j) * fst3 (cell A.! (i + 1, j + 1)) else 0
            ex = if i < n then qBase * snd3 (cell A.! (i + 1, j)) else 0
            ey = if j < m then qBase * thd3 (cell A.! (i, j + 1)) else 0
        in (mm * em + pDelta p * (ex + ey), gm * em + pEps p * ex, gm * em + pEps p * ey)
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

-- ★ マス (i, j) ごとの P(x_i と y_j が対応 | 2 本の文字列)
matchPosterior :: String -> String -> PP -> [[Double]]
matchPosterior x y p =
  let ((fM, _, _), prob) = pairForward x y p
      (bM, _, _) = pairBackward x y p
  in [ [ if prob > 0 then (fM A.! (i, j) * bM A.! (i, j)) / prob else 0 | j <- [1 .. length y] ] | i <- [1 .. length x] ]

-- ★ ペア HMM の確率から、動的計画法の点数を作る：s(a, b) = log( p_ab / (q_a q_b) ) + log(1 − 2δ − τ)、すき間の開始 d、のばす e、最後の補正 c
data Sc = Sc { scSub :: Char -> Char -> Double, scD, scE, scC :: !Double }

scoresFromHmm :: PP -> Sc
scoresFromHmm p =
  let mm = 1 - 2 * pDelta p - pTau p
      gm = 1 - pEps p - pTau p
  in Sc (\a b -> log (pMatch p a b / (qBase * qBase)) + log mm) (negate (log ((pDelta p * gm) / mm))) (negate (log (pEps p))) (log mm - log gm)

-- ★ Needleman–Wunsch（アフィンギャップ、3 枚の表）：点数を足して最大をとるだけ。確率は出てこない
nwAffine :: String -> String -> Sc -> Double
nwAffine x y sc = maximum [tM A.! (n, m), tX A.! (n, m) + scC sc, tY A.! (n, m) + scC sc]
  where
    n = length x
    m = length y
    xa = A.listArray (1, max 1 n) x
    ya = A.listArray (1, max 1 m) y
    tM = mkGrid n m $ \(i, j) -> if i > 0 && j > 0 then scSub sc (xa A.! i) (ya A.! j) + maximum [tM A.! (i - 1, j - 1), tX A.! (i - 1, j - 1), tY A.! (i - 1, j - 1)]
                                 else if i == 0 && j == 0 then 0 else negInf
    tX = mkGrid n m $ \(i, j) -> if i > 0 then max (tM A.! (i - 1, j) - scD sc) (tX A.! (i - 1, j) - scE sc) else negInf
    tY = mkGrid n m $ \(i, j) -> if j > 0 then max (tM A.! (i, j - 1) - scD sc) (tY A.! (i, j - 1) - scE sc) else negInf

recompute :: World -> World
recompute w =
  let (x, y) = wStrs w
      p = wPP w
      sc = scoresFromHmm p
  in w { wRes = Res (pairViterbi x y p) (snd (pairForward x y p)) (matchPosterior x y p) (nwAffine x y sc) (i2d (length x + length y) * log qBase + log (pTau p)) }

-- 1 本を作り、もう 1 本はそれを少し変えたもの（置きかえ・ぬけ・追加）
-- ※ p5.js 版と同じく、x の長さの rndInt(6, 10) は for の条件の中にあるので、1 文字ごとに引き直す
randomPair :: R (String, String)
randomPair = do
  let genX k acc = do
        lim <- rndInt 6 10
        if k < lim then do { c <- rndInt 0 3; genX (k + 1) (acc ++ [at' dna c 'A']) } else pure acc
  x <- genX (0 :: Int) ""
  let genY acc [] = pure acc
      genY acc (ch : rest) = do
        u <- rnd
        if u < 0.12 then genY acc rest else do
          c1 <- if u < 0.27 then fmap (\k -> at' dna k 'A') (rndInt 0 3) else pure ch
          v <- rnd
          extra <- if v < 0.1 then fmap (\k -> [at' dna k 'A']) (rndInt 0 3) else pure ""
          genY (acc ++ [c1] ++ extra) rest
  y <- genY "" x
  let y' = take maxLen y
  pure (x, if null y' then "A" else y')

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle env "Pair HMM and alignment: match M, gap X, gap Y emit two strings together"
      ("A C G T add a letter   Backspace delete   Tab / click switch top/bottom   Up/Down delta = " ++ fmtN (pDelta p) 2 ++ "   Left/Right eps = " ++ fmtN (pEps p) 2 ++ "   R new pair")
  -- ---- 入力
  , Pictures [ Pictures [ note (if k == 1 then "y (bot)" else "x (top)") 30 yy (if k == wActive w then lan else dimc) 14
                        , say (s ++ (if k == wActive w && envFrame env `mod` 60 < 30 then "_" else "")) 100 (yy + 2) 22 (col ink) ]
             | (k, s) <- [(0 :: Int, x), (1, y)], let yy = 112 + i2f k * 34 ]
  -- ---- 表：色 = 対応している確率、線 = ビタビの並べ方
  , note "Brightness = P(x_i and y_j aligned)" 40 (gy - 40) dimc 14
  , note "Yellow line = Viterbi path (diagonal M, down X, right Y)" 40 (gy - 22) dimc 14
  , Pictures [ sayM [ch] (gx - cs * 0.1) (gy + (i2f i + 0.5) * cs) 16 (col ink) | (i, ch) <- zip [1 :: Int ..] x ]
  , Pictures [ sayM [ch] (gx + (i2f j + 0.5) * cs) (gy + cs * 0.5) 16 (col ink) | (j, ch) <- zip [1 :: Int ..] y ]
  , Pictures [ Pictures [ Color (col (mixP (f2 pr))) (rectTL (cx + 1) (cy + 1) (cs - 2) (cs - 2))
                        , if pr > 0.3 && cs > 40 then sayM (fmtN pr 2) (cx + cs / 2) (cy + cs / 2) 14 (col ink) else Blank ]
             | (i, row) <- zip [1 :: Int ..] (rPost r), (j, pr) <- zip [1 :: Int ..] row, let cx = gx + i2f j * cs, let cy = gy + i2f i * cs ]
  , Color (col lan) (polyline 3 ((gx + cs, gy + cs) : [ (gx + i2f (cJ c + 1) * cs, gy + i2f (cI c + 1) * cs) | c <- cols ]))
  -- ---- 右：並べ方と数字
  , note "Viterbi alignment (| same letter, . different, - gap)" rx 130 lan 14
  , mono top rx 164 ink, mono mid rx 188 dimc, mono bot rx 212 ink
  , note ("Viterbi: log P(best alignment) = " ++ fmtN lv 3) rx 256 lan 14
  , note ("DP score " ++ fmtN (rNw r) 3 ++ " + constant " ++ fmtN (rBase r) 3 ++ " = " ++ fmtN (rNw r + rBase r) 3 ++ (if abs (rNw r + rBase r - lv) < 1e-9 then "  OK same" else "  x")) rx 280 ink 14
  , note ("Scores: same " ++ fmtN (scSub sc 'A' 'A') 2 ++ "  diff " ++ fmtN (scSub sc 'A' 'C') 2 ++ "  gap open -" ++ fmtN (scD sc) 2 ++ "  extend -" ++ fmtN (scE sc) 2) rx 304 dimc 14
  , note "(constant = log P(all letters at random) + log tau;" rx 324 dimc 14
  , note " scores = log-odds against random letters)" rx 342 dimc 14
  , note ("Forward: log P(sum of all alignments) = " ++ fmtN lf 3) rx 366 rose 14
  , Color (col linec) (rectTL rx 380 300 14)
  , Color (col lan) (rectTL rx 380 (300 * f2 (clampv share 0 1)) 14)
  , note ("best = " ++ fmtN (share * 100) 1 ++ "% of all") (rx + 310) 392 ink 14
  , note "Many near-ties: trust the cell probabilities, not one path" rx 420 dimc 14
  , note "Larger delta: gaps start more easily. Larger eps: longer gaps." rx 444 dimc 14
  , drawSelfTest env (wTest w) ]
  where
    (x, y) = wStrs w
    p = wPP w
    r = wRes w
    n = length x
    m = length y
    gx = 80
    gy = 210
    cs = minimum [46, (envH env - gy - 60) / i2f (n + 1), (envW env * 0.5 - gx) / i2f (m + 1)]
    (pr0, pg0, pb0) = panelc
    (rr, rg, rb) = rose
    mixP t = (pr0 + t * (rr - pr0), pg0 + t * (rg - pg0), pb0 + t * (rb - pb0))
    cols = fst (rVit r)
    rx = max (gx + i2f (m + 1) * cs + 50) (envW env * 0.52)
    lineOf c = let a = if cS c == 'Y' then '-' else at' x (cI c - 1) '?'
                   b = if cS c == 'X' then '-' else at' y (cJ c - 1) '?'
               in (a, if cS c == 'M' then (if a == b then '|' else '.') else ' ', b)
    trip = map lineOf cols
    top = [ a | (a, _, _) <- trip ]
    mid = [ b | (_, b, _) <- trip ]
    bot = [ c | (_, _, c) <- trip ]
    lv = snd (rVit r)
    lf = lg (rFwd r)
    sc = scoresFromHmm p
    share = if rFwd r > 0 then exp (lv - lf) else 0

-- 等幅で 1 文字ずつ置く（gloss の文字は等幅ではないので）
mono :: String -> Float -> Float -> RGB -> Picture
mono s x y c = Pictures [ sayC [ch] (x + 9 + i2f k * 18) y 22 (col c) | (k, ch) <- zip [0 :: Int ..] s ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        hits = [ k | k <- [0, 1], let yy = 112 + i2f k * 34, inBox (mx, my) (30, yy - 24, 420, 32) ]
    in if null hits then w else w { wActive = last hits }
  EventKey (Char c) Down _ _
    | toUpper c `elem` dna -> let s = activeStr w in if length s < maxLen then recompute (setActive (s ++ [toUpper c]) w) else w
    | c == '\b' || c == '\DEL' -> backspace
    | c == '\t' -> w { wActive = 1 - wActive w }
    | c == 'r' || c == 'R' -> let (pr, g) = runWith (wRng w) randomPair in recompute w { wStrs = pr, wRng = g }
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace           -- Mac の delete キー
  EventKey (SpecialKey KeyTab) Down _ _ -> w { wActive = 1 - wActive w }
  EventKey (SpecialKey KeyUp) Down _ _ -> setPP (wPP w) { pDelta = min 0.4 (round2 (pDelta (wPP w) + 0.02)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> setPP (wPP w) { pDelta = max 0.02 (round2 (pDelta (wPP w) - 0.02)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> setPP (wPP w) { pEps = min 0.8 (round2 (pEps (wPP w) + 0.05)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> setPP (wPP w) { pEps = max 0.05 (round2 (pEps (wPP w) - 0.05)) }
  _ -> w
  where
    setPP p = recompute w { wPP = p }
    backspace = recompute (setActive (let s = activeStr w in take (length s - 1) s) w)
    round2 v = i2d (jsRound (v * 100)) / 100

activeStr :: World -> String
activeStr w = if wActive w == 0 then fst (wStrs w) else snd (wStrs w)

setActive :: String -> World -> World
setActive s w = w { wStrs = if wActive w == 0 then (s, snd (wStrs w)) else (fst (wStrs w), s) }

-- ---------- やってみよう ----------
-- 1. 上を GATTACA、下を GATTACA にすると、表はどうなる？ 下の 1 文字を消すと、すき間はどこに入る？
-- 2. 上を AAAA、下を AAA にすると、いちばん良い並べ方は全体の何 %？ なぜ小さい？（すき間の場所が 4 通りある）
-- 3. ↑ で δ を大きくしていくと、「ちがう文字」として並べていた所が、すき間 2 つに変わることがある。どこで変わる？
-- 4. ← で ε を小さくすると、長いすき間 1 つと短いすき間 2 つのどちらが選ばれやすい？

-- ---- 自己テスト
-- 総当たり：M・X・Y の道を全部たどる（X のすぐあとに Y、Y のすぐあとに X は来ない）。(合計, いちばん良い, 道の数, マスごとの合計)
pairBrute :: String -> String -> PP -> (Double, Double, Int, [[Double]])
pairBrute x y p = (total, best, length ends, mass)
  where
    n = length x
    m = length y
    ends = rec 0 0 'M' 1 []
    rec i j prev pr matched
      | i == n && j == m = [(pr * pTau p, matched)]
      | otherwise =
          let toM = if prev == 'M' then 1 - 2 * pDelta p - pTau p else 1 - pEps p - pTau p
              gapP = if prev == 'M' then pDelta p else pEps p
          in (if i < n && j < m then rec (i + 1) (j + 1) 'M' (pr * toM * pMatch p (x !! i) (y !! j)) (matched ++ [(i, j)]) else [])
             ++ (if i < n && prev /= 'Y' then rec (i + 1) j 'X' (pr * gapP * qBase) matched else [])
             ++ (if j < m && prev /= 'X' then rec i (j + 1) 'Y' (pr * gapP * qBase) matched else [])
    total = foldl' (\s (q, _) -> s + q) 0 ends
    best = foldl' (\b (q, _) -> if q > b then q else b) 0 ends
    massA = A.accumArray (+) 0 ((0, 0), (max 0 (n - 1), max 0 (m - 1))) [ (ij, q) | (q, ms) <- ends, ij <- ms ] :: A.Array (Int, Int) Double
    mass = [ [ massA A.! (i, j) | j <- [0 .. m - 1] ] | i <- [0 .. n - 1] ]

selfTest :: [(String, Bool)]
selfTest = withSeed 810 $ do
  let p0 = pp0
      tot = foldl' (+) 0 [ pMatch p0 a b | a <- dna, b <- dna ]
      rs len = fmap (map (\k -> at' dna k 'A')) (replicateR len (rndInt 0 3))
      randP = do
        d <- rnd
        e <- rnd
        t <- rnd
        s <- rnd
        pure (PP (0.03 + d * 0.3) (0.1 + e * 0.6) (0.02 + t * 0.1) (0.4 + s * 0.55))
  small <- mapM (\k -> do
              p <- if k < 5 then pure p0 else randP
              x <- rndInt 0 4 >>= rs
              y <- rndInt 0 4 >>= rs
              let (bsum, bbest, cnt, mass) = pairBrute x y p
                  (_, vlog) = pairViterbi x y p
                  fprob = snd (pairForward x y p)
                  post = matchPosterior x y p
                  wp = foldl' maxN 0 [ abs (at2 post i j - at2 mass i j / bsum) | i <- [0 .. length x - 1], j <- [0 .. length y - 1] ]
              pure (abs (exp vlog - bbest) / bbest, abs (fprob - bsum) / bsum, wp, cnt)) [0 .. 24 :: Int]
  let worstB = foldl' maxN 0 [ a | (a, _, _, _) <- small ]
      worstS = foldl' maxN 0 [ b | (_, b, _, _) <- small ]
      worstP = foldl' maxN 0 [ c | (_, _, c, _) <- small ]
      counts = sum [ d | (_, _, _, d) <- small ]
  big <- mapM (\k -> do
              p <- if k < 10 then pure p0 else randP
              x <- rndInt 0 12 >>= rs
              y <- rndInt 0 12 >>= rs
              let (cols, vlog) = pairViterbi x y p
                  fprob = snd (pairForward x y p)
                  (bM, _, _) = pairBackward x y p
                  nw = abs (nwAffine x y (scoresFromHmm p) + i2d (length x + length y) * log qBase + log (pTau p) - vlog)
                  ci = length [ () | c <- cols, cS c /= 'Y' ]
                  cj = length [ () | c <- cols, cS c /= 'X' ]
                  ge = not (fprob < exp vlog * (1 - 1e-12)) && ci == length x && cj == length y
              pure (nw, ge, abs (bM A.! (0, 0) - fprob) / fprob)) [0 .. 39 :: Int]
  let worstN = foldl' maxN 0 [ a | (a, _, _) <- big ]
      okGe = and [ b | (_, b, _) <- big ]
      worstBk = foldl' maxN 0 [ c | (_, _, c) <- big ]
      (sameC, _) = pairViterbi "GATTACA" "GATTACA" p0
      (gapC, _) = pairViterbi "GATTACA" "GATACA" p0
      (_, e0) = pairViterbi "" "" p0
      e1 = snd (pairForward "" "AC" p0)
      sh = exp (snd (pairViterbi "AAAA" "AAA" p0)) / snd (pairForward "AAAA" "AAA" p0)
  pure
    [ ("確率の合計：M が出す 16 通り = 1、M から出る矢印 (1 − 2δ − τ) + 2δ + τ = 1", near tot 1 1e-12 && near (1 - 2 * pDelta p0 - pTau p0 + 2 * pDelta p0 + pTau p0) 1 1e-12)
    , ("短い文字列 25 組（合わせて " ++ show counts ++ " 通りの並べ方を総当たり）：ビタビ = いちばん良い並べ方の確率", worstB < 1e-10)
    , ("同じ 25 組：前向き = 全部の並べ方の確率の合計", worstS < 1e-10)
    , ("同じ 25 組：マスごとの「対応している確率」= 総当たり", worstP < 1e-10)
    , ("でたらめな 40 組（長さ 12 まで、パラメータもいろいろ）：ビタビの log 確率 = Needleman–Wunsch の点数 + 定数（差 < 1e-9）", worstN < 1e-9)
    , ("前向き ≥ ビタビ、後ろ向きの合計 = 前向きの合計、並べ方は両方の文字を全部使う", okGe && worstBk < 1e-10)
    , ("同じ文字列どうしは全部「一致」。1 文字ぬけた文字列は、すき間 1 つと一致 6 つ", all ((== 'M') . cS) sameC && length (filter ((== 'X') . cS) gapC) == 1 && length (filter ((== 'M') . cS) gapC) == 6)
    , ("空の文字列でも動く：P(空, 空) = τ、P(空, AC) = δ ε τ / 16", near (exp e0) (pTau p0) 1e-12 && near e1 ((pDelta p0 * pEps p0 * pTau p0) / 16) 1e-15 && all noNaN (matchPosterior "" "AC" p0))
    , ("AAAA と AAA：いちばん良い並べ方は全体の " ++ fmtN (sh * 100) 1 ++ "%（すき間の場所が 4 通りあるので 30% 未満）", sh < 0.3 && sh > 0.15) ]

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
