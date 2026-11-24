-- ============================================
-- 65-05 すごろくの平均手数 ─ オリジナルの 30 マスのすごろく（はしごで上へ、へびで下へ）
-- サイコロの目で進むだけなので、次のマスは「今のマス」だけで決まる → マルコフ連鎖。ゴール 30 は吸収状態。
-- まだゴールしていないマス 0〜29 の間の遷移 Q から、基本行列 N = (I − Q)⁻¹ を作ると
--   N[0][j] = スタートから、マス j に止まる平均の回数　　Σ_j N[0][j] = ゴールまでの平均の手数
-- ぴったり 30 に止まらないとゴールできない（出た目が大きすぎたら、その場で休み）
-- 操作：マスをクリック → 別のマスをクリック はしご・へびを作る（同じマスを 2 回 = 消す）/ スペース 1 手 / A 自動
--       R はしご・へびを元に / F シミュレーションの速さ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

board :: Int
board = 30                                    -- ★ ゴールのマス

startJumps :: IM.IntMap Int                   -- ★ はしご（上へ）とへび（下へ）
startJumps = IM.fromList [(3, 12), (8, 16), (13, 22), (19, 27), (11, 2), (17, 5), (24, 15), (28, 9)]

stepsSlow, stepsFast :: Int
stepsSlow = 3000                              -- ★ 1 フレームのシミュレーションの歩数（ふつう）
stepsFast = 12000                             -- ★ 速いとき

-- 基本行列の答え：Q、N = (I − Q)⁻¹、各マスからの平均の手数
data Sol = Sol { soQ :: Mat, soN :: Mat, soMoves :: [Double] }

data World = World
  { wJumps    :: IM.IntMap Int
  , wSol      :: Maybe Sol                    -- ゴールに行けない盤では Nothing
  , wViMoves  :: !Double                      -- 価値反復の答え
  , wViIters  :: !Int
  , wSel      :: !Int                         -- 選んでいるマス（なければ −1）
  , wToken    :: !Int
  , wTokMoves :: !Int
  , wLastRoll :: !Int
  , wAuto     :: !Bool
  , wGames    :: !Int
  , wSum      :: !Int
  , wHist     :: IM.IntMap Int                -- 手数（60 以上はまとめて）の回数
  , wFast     :: !Bool
  , wRng      :: !Rng
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-05 Snakes and ladders" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = rebuild (World startJumps Nothing 0 0 (-1) 0 0 0 True 0 0 IM.empty False (rngOf 5) [])

-- ★ 1 手のルール：pos から roll だけ進む。行きすぎならその場、着いたマスにはしご・へびがあれば飛ぶ
landing :: IM.IntMap Int -> Int -> Int -> Int
landing jm pos roll = let np0 = pos + roll
                          np = if np0 > board then pos else np0
                      in IM.findWithDefault np np jm

buildQ :: IM.IntMap Int -> Mat                -- マス 0〜29 の間の遷移 Q（ゴールへ行く分は Q に入らない）
buildQ jm = [ foldl' (\row r -> let t = landing jm s r in if t < board then modifyAt t (+ 1 / 6) row else row) (replicate board 0) [1 .. 6]
            | s <- [0 .. board - 1] ]

-- ★ 基本行列 N = (I − Q)⁻¹。ゴールに行けない盤では逆行列がない → Nothing
solveBoard :: IM.IntMap Int -> Maybe Sol
solveBoard jm = do
  let q = buildQ jm
      iq = zipWith (zipWith (-)) (eyeMat board) q
  fm <- matInv iq
  if not (noNaN (concat fm)) || any (\v -> v < -1e-9 || v > 1e8) (concat fm) then Nothing
    else Just (Sol q fm (map sumOf fm))

-- 価値反復：E[s] = 1 + (1/6) Σ_r E[着いたマス]、E[ゴール] = 0（新しい値をすぐ使う）。戻り値 (E, 回数, 収束したか)
valueIter :: IM.IntMap Int -> Double -> Int -> (U.Vector Double, Int, Bool)
valueIter jm tol maxIt = runST $ do
  e <- MU.replicate (board + 1) 0
  let land = U.fromList [ landing jm s r | s <- [0 .. board - 1], r <- [1 .. 6] ]
      sweep !s !d
        | s >= board = pure d
        | otherwise = do
            v <- foldlM' (\acc r -> do x <- MU.read e (land U.! (s * 6 + r - 1)); pure (acc + x / 6)) 1 [1 .. 6]
            old <- MU.read e s
            MU.write e s v
            sweep (s + 1) (max d (abs (v - old)))
      loop !it !d
        | d > tol && it < maxIt = sweep 0 0 >>= loop (it + 1)
        | otherwise = pure (it, d)
  (it, d) <- loop 0 posInf
  ev <- U.freeze e
  pure (ev, it, d <= tol)
  where foldlM' f z xs = go z xs where go !a [] = pure a
                                       go !a (y : ys) = f a y >>= \a' -> go a' ys

-- 1 回ゴールまで遊ぶ。visit = 止まったマス（ゴール以外）ごとに呼ぶ。戻り値 (手数, 数えたもの)
playOnce :: IM.IntMap Int -> (a -> Int -> a) -> a -> R (Int, a)
playOnce jm visit = go 0 0
  where go !s !n !acc
          | s == board || n >= 100000 = pure (n, acc)
          | otherwise = do
              r <- rndInt 1 6
              let s' = landing jm s r
              go s' (n + 1) (if s' < board then visit acc s' else acc)

rebuild :: World -> World
rebuild w =
  let (e, it, ok) = valueIter (wJumps w) 1e-10 20000
  in w { wSol = solveBoard (wJumps w), wViMoves = if ok then e U.! 0 else posInf, wViIters = it
       , wGames = 0, wSum = 0, wHist = IM.empty, wToken = 0, wTokMoves = 0 }

-- シミュレーション（1 フレームに決まった歩数ぶん、ゲームの途中では切らない）と、自動のコマ
step :: Env -> World -> World
step env w0 =
  let w1 = case wSol w0 of
             Nothing -> w0
             Just _ -> sims w0 0
  in if wAuto w1 && envFrame env `mod` 30 == 0 then moveToken w1 else w1
  where
    budget = if wFast w0 then stepsFast else stepsSlow
    sims w used
      | used >= budget = w
      | otherwise = let ((n, _), g) = runR (playOnce (wJumps w) (\a _ -> a) ()) (wRng w)
                    in sims w { wGames = wGames w + 1, wSum = wSum w + n, wHist = IM.insertWith (+) (min 60 n) 1 (wHist w), wRng = g } (used + max 1 n)

moveToken :: World -> World
moveToken w0 =
  let w = if wToken w0 == board then w0 { wToken = 0, wTokMoves = 0 } else w0
      (r, g) = runR (rndInt 1 6) (wRng w)
  in w { wLastRoll = r, wToken = landing (wJumps w) (wToken w) r, wTokMoves = wTokMoves w + 1, wRng = g }

-- マス k の左上（下の段から、左右に折り返して並べる。0 はスタートの外）
cellPos :: Int -> Float -> Float -> Float -> (Float, Float)
cellPos k bx by cs = let r = (k - 1) `div` 6
                         c0 = (k - 1) `mod` 6
                         c = if odd r then 5 - c0 else c0
                     in (bx + i2f c * cs, by + i2f (4 - r) * cs)

cellSize :: Env -> Float
cellSize env = minimum [84, (envH env - 170) / 5, envW env * 0.48 / 6]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Snakes and ladders: " ++ maybe "inf" (fmtN 3 . headOr 0 . soMoves) (wSol w) ++ " moves to goal on average (fundamental matrix)")
             ("Click square -> square: ladder/snake   Space 1 move   A auto (" ++ onOff (wAuto w) ++ ")   R original board   F speed" ++ (if wFast w then " (fast)" else ""))
  -- ---- 盤（色 = スタートからそのマスに止まる平均回数 N[0][j]）
  , Pictures [ Pictures [ Color (if k == board then col mustard else col (52 + v * 180, 46 + v * 60, 42 + v * 10)) (rectTL (x + 2) (y + 2) (cs - 4) (cs - 4))
                        , if k == wSel w then Color (col mustard) (frameTL 3 (x + 2) (y + 2) (cs - 4) (cs - 4)) else Blank
                        , sayT (if k == board then "GOAL" else show k) (x + 7) (y + 6) 14 (if k == board then grayC 30 else grayC 235) False
                        , case wSol w of
                            Just so | k < board -> sayR (fmtN 2 (n0 so k)) (x + cs - 6) (y + cs - 7) 14 (col (200, 190, 175))
                            _ -> Blank ]
             | k <- [1 .. board], let (x, y) = cellPos k bx by cs
             , let v = case wSol w of
                         Just so | k < board -> f2 (n0 so k / hi)
                         _ -> 0 ]
  -- はしご = 水色の 2 本線、へび = 赤い波線
  , Pictures [ jumpPic a b | (a, b) <- IM.toList (wJumps w) ]
  , let (tx, ty) = if wToken w == 0 then (bx - cs * 0.6, by + 4 * cs) else cellPos (wToken w) bx by cs
    in Pictures [ Color (grayC 20) (circleAt (tx + cs / 2) (ty + cs / 2) (cs * 0.36 + 4)), Color (col cream) (circleAt (tx + cs / 2) (ty + cs / 2) (cs * 0.36)) ]
  , sayT ("Token: move " ++ show (wTokMoves w) ++ "   roll " ++ (if wLastRoll w > 0 then show (wLastRoll w) else "-")) bx (by + 5 * cs + 8) 14 (col sub) False
  , sayT "(small number = average visits to that square)" bx (by + 5 * cs + 28) 14 (col sub) False
  -- ---- 3 つの方法
  , say "Average number of moves to the goal" rx 120 16 (col cream)
  , Pictures [ Pictures [ say a rx (150 + 26 * i2f k) 15 (col c), say b (rx + 330) (150 + 26 * i2f k) 15 (col cream) ]
             | (k, (a, b, c)) <- zip [0 :: Int ..]
                 [ ("1 fundamental matrix sum N[0][j]", maybe "inf (goal unreachable)" (fmtN 4 . headOr 0 . soMoves) (wSol w), mustard)
                 , ("2 value iteration (" ++ show (wViIters w) ++ " sweeps)", fmtN 4 (wViMoves w), teal)
                 , ("3 simulation (" ++ commas (toInteger (wGames w)) ++ " games)", if wGames w > 0 then fmtN 2 (i2d (wSum w) / i2d (wGames w)) else "...", dice) ] ]
  -- 手数のヒストグラム
  , plotAxes g "moves (60+ grouped)" "share of games by number of moves (simulation)" [0, 20, 40, 60] [0]
  , Color (col dice) (Pictures [ rectTL (gX g (i2d n) - 2) (gY g 0 - hh) 4 hh | n <- [0 .. 60], let hh = gY g 0 - gY g (frac n), hh > 0 ])
  , case wSol w of
      Just so -> let m = headOr 0 (soMoves so) in Color (col mustard) (lineW 2 (gX g m, grY g) (gX g m, grY g + grH g))
      Nothing -> Blank
  , sayT "yellow line = mean (fundamental matrix). Long right tail:" rx (grY g + grH g + 44) 14 (col sub) False
  , sayT "many games take longer than the mean." rx (grY g + grH g + 64) 14 (col sub) False
  , if wSel w > 0 then sayT ("square " ++ show (wSel w) ++ " selected -> click the target square") rx (grY g + grH g + 88) 14 (col mustard) False else Blank
  , drawSelfTest env (wTest w) ]
  where
    cs = cellSize env
    bx = 40
    by = 100
    n0 so k = at' (headOr [] (soN so)) k 0
    hi = case wSol w of
           Just so -> let m = maxOr 0 (drop 1 (headOr [] (soN so))) in if m > 0 then m else 1
           Nothing -> 1
    rx = bx + 6 * cs + 60
    games = max 1 (wGames w)
    frac n = i2d (IM.findWithDefault 0 n (wHist w)) / i2d games
    g = Graph rx 270 (min 440 (envW env - rx - 50)) (envH env * 0.3) 0 60 0 (max 0.01 (maxOr 0 (map frac [0 .. 60])) * 1.1)
    jumpPic a b =
      let (x1, y1) = (\(x, y) -> (x + cs / 2, y + cs / 2)) (cellPos a bx by cs)
          (x2, y2) = (\(x, y) -> (x + cs / 2, y + cs / 2)) (cellPos b bx by cs)
          l0 = hyp (x2 - x1) (y2 - y1)
          l = if l0 == 0 then 1 else l0
          nx = -(y2 - y1) / l
          ny = (x2 - x1) / l
      in if b > a
           then Color (col teal) (Pictures [ lineW 4 (x1 + nx * 7, y1 + ny * 7) (x2 + nx * 7, y2 + ny * 7), lineW 4 (x1 - nx * 7, y1 - ny * 7) (x2 - nx * 7, y2 - ny * 7) ])
           else Color (col dice) (Pictures [ polyline 5 [ (x1 + (x2 - x1) * t + nx * wv, y1 + (y2 - y1) * t + ny * wv)
                                                         | k <- [0 .. 30 :: Int], let t = i2f k / 30, let wv = sin (t * pi * 4) * 9 * (1 - t * 0.5) ]
                                           , circleAt x1 y1 14 ])

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        cs = cellSize env
        hits = [ k | k <- [1 .. board], let (x, y) = cellPos k 40 100 cs, mx > x, mx < x + cs, my > y, my < y + cs ]
        hit = lastOr (-1) hits
    in if hit < 0 then w { wSel = -1 }
       else if wSel w < 0 then (if hit < board then w { wSel = hit } else w)
       else let jm = if hit == wSel w then IM.delete hit (wJumps w)
                     else IM.filterWithKey (\a b -> a /= b) (IM.insert (wSel w) hit (IM.delete hit (wJumps w)))
            in rebuild w { wJumps = jm, wSel = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> moveToken w
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'r' || c == 'R' -> rebuild w { wJumps = startJumps }
    | c == 'f' || c == 'F' -> w { wFast = not (wFast w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. へび 28 → 9 を消すと平均の手数は何手へる？（マス 28 をクリックして、もう一度 28）
-- 2. 色の濃いマス（よく止まるマス）にはしごを置くと、効き目が大きい？
-- 3. landing を「行きすぎたら、はね返る（30 − あまり）」に変えると平均はどう変わる？
-- 4. へびだらけにして、ゴールに行けない盤を作ると N はどうなる？（逆行列がなくなる → 画面は inf）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("各マスの行：Q の合計 + ゴールへ行く確率 = 1", rowsOk)
  , ("(I − Q) N = I（ずれ < 10⁻¹⁰）", maxAbsDiff (matMul iq fm) (eyeMat board) < 1e-10)
  , ("平均の手数：基本行列 " ++ fmtN 4 m0 ++ " = 価値反復（全マス、ずれ < 10⁻⁹）", and [ abs (m - ve U.! s) < 1e-9 | (s, m) <- zip [0 ..] moves ])
  , ("N = I + Q + Q² + …（止まる回数の期待値の和、ずれ < 10⁻⁹）", maxAbsDiff ser fm < 1e-9)
  , ("シミュレーション 2 万回の平均 " ++ fmtN 2 (i2d simSum / i2d games) ++ " ≈ " ++ fmtN 2 m0 ++ "（3% 以内）", abs (i2d simSum / i2d games / m0 - 1) < 0.03)
  , ("マスごとの止まる回数もシミュレーション ≈ N[0][j]（ずれ < 0.06）", and [ j == 0 || abs (i2d (IM.findWithDefault 0 j vis) / i2d games - at' (headOr [] fm) j 0) < 0.06 | j <- [0 .. board - 1] ])
  , ("ゴールに行けない盤では N がない（null）→ 画面は ∞", isNothingSol (solveBoard trap)) ]
  where
    jm = startJumps
    so = solveBoard jm
    q = buildQ jm
    fm = maybe [] soN so
    moves = maybe [] soMoves so
    m0 = headOr 0 moves
    rowsOk = and [ abs (sumOf (q !! s) + toGoal - 1) <= 1e-12
                 | s <- [0 .. board - 1], let toGoal = foldl' (\t r -> if landing jm s r == board then t + 1 / 6 else t) 0 [1 .. 6 :: Int] ]
    iq = zipWith (zipWith (-)) (eyeMat board) q
    (ve, _, _) = valueIter jm 1e-12 200000
    ser = serGo (zerosMat board) (eyeMat board) (0 :: Int)
    serGo s qk k
      | k >= 3000 = s
      | otherwise = let s' = zipWith (zipWith (+)) s qk
                        qk' = matMul qk q
                    in if maxAbsDiff qk' (zerosMat board) < 1e-15 then s' else serGo s' qk' (k + 1)
    games = 20000 :: Int
    (simSum, vis) = withSeed 55 (simGo games 0 IM.empty)
    simGo :: Int -> Int -> IM.IntMap Int -> R (Int, IM.IntMap Int)
    simGo 0 !s v = pure (s, v)
    simGo k !s v = do
      (n, v') <- playOnce jm (\a t -> IM.insertWith (+) t 1 a) v
      simGo (k - 1) (s + n) v'
    trap = IM.fromList [(25, 20), (26, 20), (27, 20), (28, 20), (29, 20), (24, 20), (30, 20)]   -- ゴールの手前が全部へび → ゴールできない
    isNothingSol x = case x of
      Nothing -> True
      Just _ -> False

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

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

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
