-- ============================================
-- 65-06 グラフ上のランダムウォーク ─ 頂点から出る辺を 1 本、同じ確率で選んで歩く（P_ij = 1 / 次数(i)）
-- 長く歩くと、各頂点にいる時間の割合は「次数（つながっている辺の数）に比例」：π_i = 次数(i) / (2 × 辺の数)
-- 理由は詳細つり合い π_i P_ij = π_j P_ji：どの辺でも、行きと帰りの流れが同じ（1 / (2 × 辺の数)）
-- カバー時間：全部の頂点を 1 回以上訪れるまでの歩数（何回もやって平均）
-- （向きのあるリンクとランダムなジャンプにすると、検索のページランク：シリーズ 8 の 08、シリーズ 31 の 11）
-- 操作：空き地をクリック 頂点を追加 / 頂点から頂点へドラッグ 辺をつける・消す / 頂点を空き地へドラッグ 移動
--       R 数え直し / 1 2 3 例のグラフ / ↑↓ 速さ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (findIndex, foldl')
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

maxV :: Int
maxV = 12                                     -- ★ 頂点の数の上限

nodeR :: Float
nodeR = 20

-- いまのカバー（全部の頂点を訪れるまで）の記録
data Cover = Cover { cvSeen :: U.Vector Bool, cvNSeen :: !Int, cvT :: !Int, cvDone :: [Int], cvStart :: !Int }

data World = World
  { wPos     :: [(Float, Float)]
  , wAdj     :: Mat                           -- 隣接行列（1 = 辺あり）
  , wP       :: Mat                           -- ランダムウォークの遷移行列
  , wCum     :: V.Vector (Double, U.Vector Double)   -- 行ごとの（合計、累積和）：サイコロを速く振るため
  , wPiDeg   :: Vec                           -- 次数 / (2 × 辺の数)
  , wWalker  :: !Int
  , wVisits  :: U.Vector Int
  , wTWalk   :: !Int
  , wSpeed   :: !Int
  , wCover   :: Cover
  , wDrag    :: !Int                          -- ドラッグを始めた頂点（なければ −1）
  , wPress   :: !(Float, Float)
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-06 Random walk on a graph" (blankWorld 1280 800))
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (blankWorld (envW env) (envH env)) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (walkBatch w)
  , skDraw  = draw
  , skEvent = event }

blankWorld :: Float -> Float -> World
blankWorld ww hh = example ww hh 1 (World [] [] [] V.empty [] 0 U.empty 0 200 (Cover U.empty 0 0 [] 0) (-1) (0, 0) (rngOf 6) [])

-- ★ 例のグラフ（頂点の数と辺のリスト）
example :: Float -> Float -> Int -> World -> World
example ww hh k w =
  let cx = ww * 0.3
      cy = hh * 0.5
      r = min (ww * 0.22) (hh * 0.32)
      (n, list) = case k of
        2 -> (8, [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)])                                       -- 1 本の道
        3 -> (9, [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (1, 2), (5, 6)])               -- 星（中心の次数が大きい）
        _ -> (8, [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (3, 4), (4, 5), (5, 6), (6, 7), (7, 4), (4, 6), (5, 7), (1, 3)])   -- 2 つのかたまり
      m = if k == 3 then n - 1 else n
      pos = [ if k == 3 && i == 0 then (cx, cy) else (cx + r * cos t, cy + r * sin t) | i <- [0 .. n - 1], let t = i2f i * 2 * pi / i2f m ]
  in rebuild w { wPos = pos, wAdj = graphOf n list }

graphOf :: Int -> [(Int, Int)] -> Mat
graphOf n list = foldl' (\a (i, j) -> modifyMat i j (const 1) (modifyMat j i (const 1) a)) (zerosMat n) list

-- ★ ランダムウォークの遷移：P_ij = 辺があれば 1 / 次数(i)。辺のない頂点はその場にとどまる
walkP :: Mat -> Mat
walkP = fixRows

degreesOf :: Mat -> [Double]
degreesOf = map sumOf

rebuild :: World -> World
rebuild w =
  let p = walkP (wAdj w)
      deg = degreesOf (wAdj w)
      tot = sumOf deg
      walker = max 0 (maybe (-1) id (findIndex (> 0) deg))
      n = length (wPos w)
  in w { wP = p, wCum = V.fromList [ (sumOf r, U.fromList (scanl1 (+) r)) | r <- p ]
       , wPiDeg = [ if tot > 0 then d / tot else 0 | d <- deg ]
       , wWalker = walker, wVisits = U.replicate n 0, wTWalk = 0
       , wCover = Cover (U.generate n (== walker)) 1 0 [] walker }

reachableCount :: Mat -> Int -> Int
reachableCount a s = go (S.singleton s) [s]
  where go seen [] = S.size seen
        go seen (u : q) = let new = [ j | (j, v) <- zip [0 ..] (at' a u []), v /= 0, not (S.member j seen) ]
                          in go (foldl' (flip S.insert) seen new) (q ++ new)

-- 2 つの頂点の間の「平均の到達時間」h_i = 1 + Σ_j P_ij h_j（h_target = 0）を連立方程式で
hittingTimes :: Mat -> Int -> Maybe Vec
hittingTimes a target =
  let n = length a
      m = [ if i == target then [ if j == i then 1 else 0 | j <- [0 .. n - 1] ] else [ (if i == j then 1 else 0) - at' row j 0 | j <- [0 .. n - 1] ]
          | (i, row) <- zip [0 ..] a ]
      b = [ if i == target then 0 else 1 | i <- [0 .. n - 1] ]
  in solveLinear m b

-- 1 フレームぶん（speed 歩）歩く。カバーも数える
walkBatch :: World -> World
walkBatch w
  | n == 0 = w
  | otherwise = runST $ do
      vis <- U.thaw (wVisits w)
      seen <- U.thaw (cvSeen cv0)
      let go !k !walker !g !nSeen !ct !done !start
            | k >= wSpeed w = pure (walker, g, nSeen, ct, done, start)
            | otherwise = do
                let (u0, g') = rndStep g
                    (tot, cum) = wCum w V.! walker
                    u = u0 * tot
                    nx = maybe (U.length cum - 1) id (U.findIndex (u <) cum)
                MU.modify vis (+ 1) nx
                s <- MU.read seen nx
                ns <- if s then pure nSeen else MU.write seen nx True >> pure (nSeen + 1)
                if ns >= comp
                  then do
                    MU.set seen False
                    MU.write seen nx True
                    go (k + 1) nx g' 1 0 (take 2000 ((ct + 1) : done)) nx
                  else go (k + 1) nx g' ns (ct + 1) done start
      (walker, g, nSeen, ct, done, start) <- go 0 (wWalker w) (wRng w) (cvNSeen cv0) (cvT cv0) (cvDone cv0) (cvStart cv0)
      vis' <- U.freeze vis
      seen' <- U.freeze seen
      pure w { wWalker = walker, wRng = g, wVisits = vis', wTWalk = wTWalk w + wSpeed w
             , wCover = Cover seen' nSeen ct done start }
  where
    n = length (wPos w)
    cv0 = wCover w
    comp = reachableCount (wAdj w) (wWalker w)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Random walk on a graph: " ++ show n ++ " vertices, " ++ showD edgesN ++ " edges, " ++ commas (toInteger (wTWalk w)) ++ " steps")
             ("Click empty: vertex   drag vertex->vertex: edge   drag vertex->empty: move   R recount   1 2 3 examples   Up/Down speed " ++ show (wSpeed w))
  -- ---- グラフ（頂点の大きさ = 滞在割合、白い輪 = 次数から計算した π）
  , Color (col (110, 100, 90)) (Pictures [ lineW 2 (at' (wPos w) i (0, 0)) (at' (wPos w) j (0, 0)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], at' (at' (wAdj w) i []) j 0 /= 0 ])
  , Pictures [ Pictures [ Color (col (if seenAt i then teal else (90, 82, 76))) (circleAt x y (2 * nodeR + 120 * sqrt (f2 (frAt i))))
                        , Color (col cream) (ringAt x y (2 * nodeR + 120 * sqrt (f2 (at' (wPiDeg w) i 0))) 1.5)
                        , sayM (show i) x y 14 (col cream) ]
             | (i, (x, y)) <- zip [0 ..] (wPos w) ]
  , if n > 0 then let (x, y) = at' (wPos w) (wWalker w) (0, 0) in Color (col dice) (circleAt x (y - nodeR - 8) 12) else Blank
  , if wDrag w >= 0 then Color (col mustard) (lineW 2 (at' (wPos w) (wDrag w) (0, 0)) (mouseP env)) else Blank
  , sayT "Disc size = time share, white ring = degree / (2 x edges). Teal = visited in this cover, red dot = walker" 40 (envH env - 50) 14 (col sub) False
  -- ---- 滞在割合 と π（次数）
  , say "Per vertex: bar = time share, line = degree / (2 x edges)" x0 120 15 (col cream)
  , drawBars x0 135 gw bh fr (const teal) top (Just (map show [0 .. n - 1]))
  , Color (col cream) (Pictures [ lineW 2 (bx - gw / i2f n / 2.5, y) (bx + gw / i2f n / 2.5, y)
                                | i <- [0 .. n - 1], let bx = x0 + (i2f i + 0.5) * (gw / i2f n), let y = 135 + bh - f2 (at' (wPiDeg w) i 0 / top) * bh ])
  -- 詳細つり合い
  , say ("balance: max |pi_i P_ij - pi_j P_ji| = " ++ fmtE db) x0 ty 15 (col cream)
  , say ("pi P vs pi: max diff = " ++ (if n > 0 then fmtE (maxAbsDiffV (vecMat (wPiDeg w) (wP w)) (wPiDeg w)) else "-") ++ " (pi is stationary)") x0 (ty + 24) 15 (col cream)
  , say (if comp < n then "Some vertices are not connected -> only the walker's part counts" else "The graph is connected") x0 (ty + 48) 14 (col (if comp < n then dice else sub))
  -- カバー時間
  , say "Cover time (until every vertex is visited):" x0 cy2 16 (col mustard)
  , say ("mean " ++ (if null done then "..." else fmtN 1 (meanOf (map i2d done))) ++ " steps (" ++ show (length done) ++ " runs)") x0 (cy2 + 22) 16 (col mustard)
  , say ("This run: " ++ show (cvNSeen cv) ++ " / " ++ show comp ++ " vertices, step " ++ show (cvT cv)) x0 (cy2 + 44) 14 (col sub)
  , if n > 1 && comp == n
      then case hittingTimes (wP w) (n - 1) of
             Just h -> say ("Hitting time 0 -> " ++ show (n - 1) ++ " (linear system) = " ++ fmtN 1 (headOr 0 h) ++ " steps") x0 (cy2 + 66) 14 (col sub)
             Nothing -> Blank
      else Blank
  , drawSelfTest env (wTest w) ]
  where
    n = length (wPos w)
    cv = wCover w
    done = cvDone cv
    seenAt i = i < U.length (cvSeen cv) && cvSeen cv U.! i
    frAt i = if i < U.length (wVisits w) then i2d (wVisits w U.! i) / i2d (max 1 (wTWalk w)) else 0
    fr = map frAt [0 .. n - 1]
    edgesN = sumOf (degreesOf (wAdj w)) / 2
    comp = if n > 0 then reachableCount (wAdj w) (wWalker w) else 0
    x0 = envW env * 0.6
    gw = min 460 (envW env - x0 - 40)
    top = maximum (0.05 : wPiDeg w ++ fr) * 1.15
    bh = envH env * 0.24
    pd i = at' (wPiDeg w) i 0
    pij i j = at' (at' (wP w) i []) j 0
    db = foldl' maxN 0 [ abs (pd i * pij i j - pd j * pij j i) | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
    ty = 135 + bh + 50
    cy2 = ty + 90

nodeAt :: World -> (Float, Float) -> Int
nodeAt w (x, y) = headOr (-1) [ i | (i, (px, py)) <- zip [0 ..] (wPos w), hyp (x - px) (y - py) < nodeR + 6 ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        w1 = w { wPress = (mx, my) }
        i = nodeAt w (mx, my)
        n = length (wPos w)
    in if mx > envW env * 0.57 || my < 90 then w1
       else if i >= 0 then w1 { wDrag = i }
       else if n < maxV then rebuild w1 { wPos = wPos w ++ [(mx, my)], wAdj = map (++ [0]) (wAdj w) ++ [replicate (n + 1) 0] }
       else w1
  EventKey (MouseButton LeftButton) Up _ pos | wDrag w >= 0 ->
    let (mx, my) = toP5 env pos
        d = wDrag w
        j = nodeAt w (mx, my)
        (px, py) = wPress w
        v = if at' (at' (wAdj w) d []) j 0 /= 0 then 0 else 1
    in if j >= 0 && j /= d then rebuild w { wAdj = modifyMat d j (const v) (modifyMat j d (const v) (wAdj w)), wDrag = -1 }
       else if j < 0 && hyp (mx - px) (my - py) > 8
         then w { wPos = setAt d (realToFrac (clampv (realToFrac mx) 40 (realToFrac (envW env * 0.57))), realToFrac (clampv (realToFrac my) 100 (realToFrac (envH env - 70)))) (wPos w), wDrag = -1 }
         else w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 20000 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 1 (wSpeed w `div` 2) }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> rebuild w
    | c >= '1' && c <= '3' -> example (envW env) (envH env) (fromEnum c - fromEnum '0') w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 例 3（星）：中心の割合は？ 次数 8 の頂点は、次数 1 の頂点の 8 倍の時間いる
-- 2. 例 2（1 本の道）はカバー時間が長い。両はしの間の到達時間は（辺の数）² になる
-- 3. 2 つのかたまりを 1 本の辺だけでつなぐと、カバー時間はどう変わる？（ボトルネック）
-- 4. 全部の頂点どうしをつないだ「完全グラフ」のカバー時間は、クーポン集めの問題と同じ

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("遷移行列の行の合計 = 1", rowSumErr pA 1 < 1e-12)
  , ("連立方程式の定常分布 = 次数 / (2 × 辺の数)", maybe False (\pe -> maxAbsDiffV pe piA < 1e-12) (stationaryExact pA))
  , ("詳細つり合い π_i P_ij = π_j P_ji（ずれ " ++ fmtE db ++ "）", db < 1e-15)
  , ("偶数の輪（周期 2）でも π = 一様（1/6）", maybe False (\pe -> maxAbsDiffV pe (replicate 6 (1 / 6)) < 1e-12) (stationaryExact (walkP cyc)))
  , ("20 万歩の滞在割合 ≈ 次数に比例（ずれ < 0.01）", maxAbsDiffV (map (\c -> i2d c / i2d nS) cnt) piA < 0.01)
  , ("1 本の道（辺 5 本）：はし → はしの平均到達時間 = 5² = 25", maybe False (\h -> near (headOr 0 h) 25 1e-9) (hittingTimes (walkP path) 5))
  , ("完全グラフ K₆ のカバー時間（6000 回）" ++ fmtN 2 (i2d tot / i2d runs) ++ " ≈ クーポン集め " ++ fmtN 2 expect ++ "（4% 以内）", abs (i2d tot / i2d runs / expect - 1) < 0.04) ]
  where
    gA = graphOf 6 [(0, 1), (1, 2), (2, 0), (2, 3), (3, 4), (4, 5), (5, 3), (1, 4)]
    pA = walkP gA
    dA = degreesOf gA
    piA = map (/ sumOf dA) dA
    db = foldl' maxN 0 [ abs (piA !! i * pA !! i !! j - piA !! j * pA !! j !! i) | i <- [0 .. 5], j <- [0 .. 5] ]
    cyc = graphOf 6 [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 0)]
    path = graphOf 6 [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5)]
    nS = 200000 :: Int
    pK = walkP (graphOf 6 [ (i, j) | i <- [0 .. 5], j <- [i + 1 .. 5] ])
    runs = 6000 :: Int
    expect = 5 * (1 + 1 / 2 + 1 / 3 + 1 / 4 + 1 / 5)
    (cnt, tot) = withSeed 66 $ do
      c <- walkGo nS 0 (IM.fromList [(k, 0) | k <- [0 .. 5]])
      t <- coverGo runs 0
      pure (IM.elems c, t)
    walkGo :: Int -> Int -> IM.IntMap Int -> R (IM.IntMap Int)
    walkGo 0 _ m = pure m
    walkGo k s m = do
      s' <- sampleFrom (pA !! s)
      walkGo (k - 1) s' $! IM.insertWith (+) s' 1 m
    coverGo :: Int -> Int -> R Int
    coverGo 0 !acc = pure acc
    coverGo r !acc = do
      t <- oneCover 0 (S.singleton 0) 0
      coverGo (r - 1) (acc + t)
    oneCover :: Int -> S.Set Int -> Int -> R Int
    oneCover s seen !t
      | S.size seen >= 6 = pure t
      | otherwise = do
          s' <- sampleFrom (pK !! s)
          oneCover s' (S.insert s' seen) (t + 1)

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
