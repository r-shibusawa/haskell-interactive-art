-- ============================================
-- 59-03 行列の積と最長経路 ─ (A ⊗ B)_ij = max_k (a_ik + b_kj)。A^{⊗k} の (i, j) = 「ちょうど k 本の辺で i から j へ行く道の最大重み」
-- ここでは a_ij = 「i → j の辺の重み」（辺がなければ ε）。右の A^{⊗k} の数を、全部の道を数えた答え（総当たり）と並べて確かめる
--   （シリーズ 38 の 09 は min で最短経路。ここは max で「いちばん時間のかかる道」= 待ち合わせの連鎖）
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps、A^{⊗k} を A^k と書く
-- 操作：ノードをドラッグ / ノード → ノードの順にクリック = 辺を足す・消す / 辺の数字をクリックして ↑↓ 重み / ← → k
--       マスにマウス = その道を表示 / R ランダム
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nNodes :: Int
nNodes = 5                       -- ★ ノードの数

-- 総当たりの答え：(最大の重み, その道, 道の数)
data Best = Best { bW :: !Double, bPath :: Maybe [Int], bCount :: !Int }

data World = World
  { wWts   :: Mat                 -- ★ 重み a_ij（i → j）
  , wK     :: !Int                -- ★ k
  , wPos   :: [(Float, Float)]    -- ノードの位置（p5 の座標）
  , wDragN, wFirstN :: !Int
  , wSelArc :: Maybe (Int, Int)
  , wMoved :: !Bool
  , wPowK  :: Mat                 -- A^{⊗k}（キーやクリックのたびに計算し直す）
  , wPowK1 :: Mat                 -- A^{⊗(k−1)}
  , wBrute :: [[Best]]            -- 総当たり
  , wTest  :: [(String, Bool)] }

wts0 :: Mat
wts0 = [ [e, 4, e, 2, e], [e, e, 3, e, 5], [1, e, e, 6, e], [e, e, 2, e, 3], [2, e, e, e, 1] ]
  where e = negInf

main :: IO ()
main = runSketch (sketch "59-03 Matrix product and longest paths" (recompute (world0 (1280, 800))))
  { skBack  = rgbC 22 22 28
  , skStart = \env -> do
      printSelfTest selfTest
      pure (recompute (world0 (envW env, envH env))) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: (Float, Float) -> World
world0 (w, h) = World wts0 3 pos0 (-1) (-1) Nothing False [] [] [] []
  where pos0 = [ (w * 0.22 + 190 * cos (tau * fromIntegral i / fromIntegral nNodes - pi / 2), h * 0.5 + 190 * sin (tau * fromIntegral i / fromIntegral nNodes - pi / 2)) | i <- [0 .. nNodes - 1] ]

-- ★ 総当たり：ちょうど k 本の辺の道（同じノードを何度通ってもよい）を全部たどり、(i, j) ごとに最大の重みと道を記録
bruteWalks :: Mat -> Int -> [[Best]]
bruteWalks a k = [ [ foldl' upd (Best negInf Nothing 0) [ (wt, p) | (v, wt, p) <- walks, v == j ] | j <- [0 .. n - 1] ]
                 | s <- [0 .. n - 1], let walks = rec s 0 0 [s] ]
  where
    n = length a
    -- (終点, 重み, 道) をたどった順に
    rec v d w path
      | d == k = [(v, w, reverse path)]
      | otherwise = concat [ rec u (d + 1) (w + avu) (u : path) | u <- [0 .. n - 1], let avu = ix a v u, not (isEps avu) ]
    upd (Best bw bp bc) (wt, p) = if wt > bw then Best wt (Just p) (bc + 1) else Best bw bp (bc + 1)

recompute :: World -> World
recompute w = w { wPowK = mpPow (wWts w) (wK w), wPowK1 = mpPow (wWts w) (wK w - 1), wBrute = bruteWalks (wWts w) (wK w) }

bestAt :: [[Best]] -> Int -> Int -> Best
bestAt b i j = headOr (Best negInf Nothing 0) (drop j (headOr [] (drop i b)))

drawMatrix :: Mat -> Float -> Float -> Float -> String -> Maybe (Int, Int) -> Picture
drawMatrix m x0' y0' cs title hl = Pictures $
  [ say title x0' (y0' - 12) 16 (grayC 220) ]
  ++ concat [ [ Color (grayC (if isEps v then 28 else 40)) (rectTL (x0' + fromIntegral j * cs) (y0' + fromIntegral i * cs) cs cs)
              , Color (if on then rgbC 255 220 120 else grayC 70) (frameTL (if on then 3 else 1) (x0' + fromIntegral j * cs) (y0' + fromIntegral i * cs) cs cs)
              , sayM (mpFmt 0 v) (x0' + (fromIntegral j + 0.5) * cs) (y0' + (fromIntegral i + 0.5) * cs) 15 (grayC (if isEps v then 110 else 240)) ]
            | (i, row) <- zip [0 :: Int ..] m, (j, v) <- zip [0 :: Int ..] row, let on = hl == Just (i, j) ]
  ++ concat [ [ sayM (show i) (x0' - 12) (y0' + (fromIntegral i + 0.5) * cs) 14 (grayC 150)
              , sayM (show i) (x0' + (fromIntegral i + 0.5) * cs) (y0' + fromIntegral (length m) * cs + 12) 14 (grayC 150) ]
            | i <- [0 .. length m - 1] ]

-- 道の上の辺か
onPath :: Maybe [Int] -> Int -> Int -> Bool
onPath mp i j = case mp of
  Nothing -> False
  Just p -> (i, j) `elem` zip p (drop 1 p)

posOf :: World -> Int -> (Float, Float)
posOf w i = headOr (0, 0) (drop i (wPos w))

-- 辺 i → j の数字の位置（自己ループは上）
labelPos :: World -> Int -> Int -> (Float, Float)
labelPos w i j
  | i == j = let (x, y) = posOf w i in (x, y - 58)
  | otherwise = snd (arrowTo 2 (posOf w i) (posOf w j) 24 (bendOf w i j))

bendOf :: World -> Int -> Int -> Float
bendOf w i j = if isEps (ix (wWts w) j i) then 0 else 10

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- グラフ
  concat [ let hot = onPath path i j
               sel = wSelArc w == Just (i, j)
               col = if hot then rgbC 255 200 80 else rgbC 150 170 200
               (x, y) = posOf w i
               arcPic = if i == j then ringAt x (y - 34) 32 (if hot then 4 else 2)
                        else fst (arrowTo (if hot then 4 else 2) (posOf w i) (posOf w j) 24 (bendOf w i j))
               (lx, ly) = labelPos w i j
           in [ Color col arcPic
              , Color (if sel then rgbC 255 220 120 else grayC 30) (circleAt lx ly 26)
              , sayM (mpFmt 0 v) lx ly 14 (grayC (if sel then 20 else 240)) ]
         | (i, row) <- zip [0 ..] (wWts w), (j, v) <- zip [0 ..] row, not (isEps v) ]
  ++ concat [ [ Color (if isEnd then rgbC 255 170 70 else rgbC 60 90 140) (circleAt x y 44)
              , Color (if i == wFirstN w then rgbC 255 220 120 else grayC 200) (ringAt x y 44 (if i == wFirstN w then 4 else 2))
              , sayM (show i) x y 18 (grayC 255) ]
            | i <- [0 .. nNodes - 1], let (x, y) = posOf w i
            , let isEnd = case path of { Just p@(p0 : _) -> p0 == i || last p == i; _ -> False } ]
  ++ [ drawMatrix (wWts w) mx1 my cs "Matrix A (a_ij = i -> j)" Nothing
     , drawMatrix (wPowK w) mx2 my cs ("A^" ++ show k) hover ]
  -- ---- 説明と答え合わせ
  ++ [ say ("Matrix product and longest paths   k = " ++ show k ++ " (exactly " ++ show k ++ " arcs)") 40 40 20 (grayC 240)
     , say "Drag nodes   click node -> node: arc   click a number, Up/Down weight   <- -> k   hover the right table   R random" 40 66 15 (grayC 170)
     , say (if allSame then "OK: all 25 cells of A^" ++ show k ++ " = brute-force max over all walks" else "NO: some cells differ")
           mx1 ty 16 (if allSame then rgbC 120 210 150 else rgbC 255 100 90) ]
  ++ (case hover of
        Just (i, j) ->
          let b = bestAt (wBrute w) i j
              terms = [ mpFmt 0 p ++ "(x)" ++ mpFmt 0 amj | m <- [0 .. nNodes - 1], let p = if k > 0 then ix (wPowK1 w) i m else negInf
                      , let amj = ix (wWts w) m j, not (isEps p), not (isEps amj) ]
          in [ sayWrap ("(" ++ show i ++ ", " ++ show j ++ "): " ++ show (bCount b) ++ " walks with " ++ show k ++ " arcs. Heaviest: "
                        ++ maybe "none -> eps" (\p -> joinWith " -> " (map show p) ++ "   weight " ++ mpFmt 0 (bW b)) (bPath b))
                       mx1 (ty + 14) (envW env - mx1 - 30) 16 (grayC 220)
             , sayWrap ("= (+)_m (A^" ++ show (k - 1) ++ ")_" ++ show i ++ "m (x) a_m" ++ show j ++ " = " ++ (if null terms then "eps" else joinWith " (+) " terms))
                       mx1 (ty + 44) (envW env - mx1 - 30) 16 (grayC 220) ]
        Nothing -> [ say "Hover a cell of the right table to see its walk on the graph" mx1 (ty + 30) 16 (grayC 220) ])
  ++ [ sayWrap "Just replace 'sum and times' of the ordinary matrix product by 'max and plus'. As k grows, walks that go round heavy loops win."
               mx1 (ty + 90) (envW env - mx1 - 30) 14 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    k = wK w
    cs = 50
    mx1 = envW env * 0.47
    mx2 = mx1 + fromIntegral nNodes * cs + 80
    my = 150
    (mxp, myp) = mouseP env
    ci = floor ((myp - my) / cs) :: Int
    cj = floor ((mxp - mx2) / cs) :: Int
    hover = if ci >= 0 && ci < nNodes && cj >= 0 && cj < nNodes then Just (ci, cj) else Nothing
    path = maybe Nothing (\(i, j) -> bPath (bestAt (wBrute w) i j)) hover
    ty = my + fromIntegral nNodes * cs + 60
    allSame = and [ ix (wPowK w) i j == bW (bestAt (wBrute w) i j) | i <- [0 .. nNodes - 1], j <- [0 .. nNodes - 1] ]

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

nodeAt :: World -> (Float, Float) -> Int
nodeAt w (x, y) = headOr (-1) [ i | i <- [0 .. nNodes - 1], let (px, py) = posOf w i, sqrt ((x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)) < 24 ]

labelAt :: World -> (Float, Float) -> Maybe (Int, Int)
labelAt w (x, y) = headOr Nothing
  [ Just (i, j) | (i, row) <- zip [0 ..] (wWts w), (j, v) <- zip [0 ..] row, not (isEps v)
  , let (px, py) = labelAtPos i j, sqrt ((x - px) ^ (2 :: Int) + (y - py) ^ (2 :: Int)) < 14 ]
  where
    -- p5 の labelAt と同じ位置（中点から 1.6 倍ずらす）
    labelAtPos i j
      | i == j = let (px, py) = posOf w i in (px, py - 58)
      | otherwise = let (xi, yi) = posOf w i
                        (xj, yj) = posOf w j
                        dx = xj - xi
                        dy = yj - yi
                        l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
                        b = bendOf w i j
                    in ((xi + xj) / 2 - dy / l * b * 1.6, (yi + yj) / 2 + dx / l * b * 1.6)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let q = toP5 env p
        dn = nodeAt w q
        w1 = w { wDragN = dn, wMoved = False }
    in pure (case labelAt w q of
               Just l | dn < 0 -> w1 { wSelArc = Just l }
               _ -> w1)
  EventMotion p | wDragN w >= 0 ->
    pure w { wPos = [ if i == wDragN w then toP5 env p else q | (i, q) <- zip [0 ..] (wPos w) ], wMoved = True }
  EventKey (MouseButton LeftButton) Up _ _
    | wDragN w >= 0 && not (wMoved w) ->
        if wFirstN w < 0 then pure w { wFirstN = wDragN w, wDragN = -1 }
        else let i = wFirstN w
                 j = wDragN w
                 nv = if isEps (ix (wWts w) i j) then 3 else negInf
             in pure (recompute w { wWts = setM i j nv (wWts w), wSelArc = if isEps nv then Nothing else Just (i, j), wFirstN = -1, wDragN = -1 })
    | otherwise -> pure w { wDragN = -1 }
  EventKey k Down _ _ | not (isMouse k) -> recompute <$> key k
  _ -> pure w
  where
    isMouse k = case k of { MouseButton _ -> True; _ -> False }
    key k = case k of
      SpecialKey KeyRight -> pure w { wK = min 6 (wK w + 1) }
      SpecialKey KeyLeft -> pure w { wK = max 1 (wK w - 1) }
      SpecialKey KeyUp -> pure (bump 1)
      SpecialKey KeyDown -> pure (bump (-1))
      Char c | c `elem` "rR" -> do
        seed <- randomRIO (0, 999999 :: Int)
        let m = withSeed (fromIntegral seed) (randMat nNodes 0.3 1 9)
        pure w { wWts = m, wSelArc = Nothing }
      _ -> pure w
    bump d = case wSelArc w of
      Just (i, j) -> w { wWts = setM i j (clampv (-9) 9 (ix (wWts w) i j + d)) (wWts w) }
      Nothing -> w

-- n × n のランダムな行列：マスごとに確率 p で lo〜hi の重み、ほかは ε（乱数は行ごとに左から）
randMat :: Int -> Double -> Int -> Int -> R Mat
randMat n p lo hi = replicateM n (replicateM n cell)
  where cell = do u <- rnd
                  if u < p then fromIntegral <$> rndInt lo hi else pure negInf

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種 3・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("(A^k)_ij = brute-force max weight (30 random graphs, k = 1..4, eps when no walk)", all (\(p, _, _, _) -> p) rs)
  , ("associativity of the matrix product (A(x)B)(x)C = A(x)(B(x)C)", all (\(_, a, _, _) -> a) rs)
  , ("identity matrix E (diagonal 0, others eps): E (x) A = A (x) E = A", all (\(_, _, e, _) -> e) rs)
  , ("power law A^5 = A^2 (x) A^3, no NaN", all (\(_, _, _, s) -> s) rs)
  , ("initial graph: A^3 = brute force", mpVecEq 1e-9 (concat (mpPow wts0 3)) (map bW (concat (bruteWalks wts0 3)))) ]
  where
    rs = withSeed 3 (mapM one [0 .. 29 :: Int])
    one t = do
      let n = 4 + t `mod` 2
      a <- randMat n 0.45 (-5) 9
      let okP = and [ ix p i j == bW (bestAt b i j) | k <- [1 .. 4], let p = mpPow a k, let b = bruteWalks a k, i <- [0 .. n - 1], j <- [0 .. n - 1] ]
          bm = map (map (\v -> if isEps v then v else v - 2)) a
          c = mpPow a 2
          okA = mpVecEq 1e-9 (concat (mpMatMul (mpMatMul a bm) c)) (concat (mpMatMul a (mpMatMul bm c)))
          okE = mpVecEq 1e-9 (concat (mpMatMul (mpEye n) a)) (concat a) && mpVecEq 1e-9 (concat (mpMatMul a (mpEye n))) (concat a)
          okS = mpVecEq 1e-9 (concat (mpPow a 5)) (concat (mpMatMul (mpPow a 2) (mpPow a 3))) && noNaN (concat (mpPow a 5))
      pure (okP, okA, okE, okS)

-- ---------- やってみよう ----------
-- 1. k を 1, 2, 3 と増やして、ε のマスがどう埋まっていくか見よう。ずっと ε のマスはなぜ？
-- 2. マスにマウスをのせ、表示された道の重みを自分で足して確かめよう
-- 3. 自己ループ（同じノードを 2 回クリック）に大きい重みをつけると、k を増やしたとき何が起きる？
-- 4. 辺の重みを全部負にすると、長い道ほど軽くなる。A^{⊗k} はどう変わる？
-- 5. 04 では E ⊕ A ⊕ A² ⊕ … をまとめて「全部の長さの道」の最大を一度に求める

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- マックスプラス行列（リストのリスト）。(A ⊗ B)_ij = max_k (a_ik + b_kj)
type Mat = [[Double]]
type Vec = [Double]

-- (i, j) の成分（範囲の外は ε）
ix :: Mat -> Int -> Int -> Double
ix a i j = vix (rowOf a i) j

rowOf :: Mat -> Int -> Vec
rowOf a i = case drop i a of
  (r : _) | i >= 0 -> r
  _ -> []

vix :: Vec -> Int -> Double
vix v j = case drop j v of
  (x : _) | j >= 0 -> x
  _ -> negInf

setM :: Int -> Int -> Double -> Mat -> Mat
setM i j x = zipWith (\r row -> if r == i then zipWith (\c v -> if c == j then x else v) [0 ..] row else row) [0 ..]

mpMat :: Int -> Int -> Double -> Mat
mpMat n m v = replicate n (replicate m v)

mpEye :: Int -> Mat                           -- 単位行列 E（対角 e = 0、他は ε）
mpEye n = [ [ if i == j then 0 else negInf | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

transposeM :: Mat -> Mat
transposeM a = case a of
  [] -> []
  (r : _) -> [ map (`vix` j) a | j <- [0 .. length r - 1] ]

mpMatMul :: Mat -> Mat -> Mat
mpMatMul a b = let bt = transposeM b
               in [ [ foldl' mpAdd negInf (zipWith mpMul r c) | c <- bt ] | r <- a ]

mpMatAdd :: Mat -> Mat -> Mat
mpMatAdd = zipWith (zipWith mpAdd)

mpMatVec :: Mat -> Vec -> Vec
mpMatVec a x = [ foldl' mpAdd negInf (zipWith mpMul r x) | r <- a ]

mpPow :: Mat -> Int -> Mat
mpPow a k = foldl' (\p _ -> mpMatMul p a) (mpEye (length a)) [1 .. k]

-- A* = E ⊕ A ⊕ A² ⊕ … ⊕ A^(n−1)。正の回路があると A* は発散する → 2 つ目が True（tol は計算誤差のゆとり）
mpStar :: Double -> Mat -> (Mat, Bool)
mpStar tol a = (st, positive)
  where
    n = length a
    pows = take (max 1 (n - 1)) (iterate (`mpMatMul` a) a)      -- A, A², …, A^(n−1)（n = 1 なら A だけ）
    st = foldl' mpMatAdd (mpEye n) pows
    plus = foldl' mpMatAdd a (take (max 0 (n - 1)) (drop 1 (iterate (`mpMatMul` a) a)))   -- A ⊕ … ⊕ A^n
    positive = or [ ix plus i i > tol | i <- [0 .. n - 1] ]

-- 残余（いちばん大きい劣解）：A ⊗ x ≤ b を満たす最大の x。x_j = min_i (b_i − a_ij)（a_ij = ε の行は制約なし）
mpResid :: Mat -> Vec -> Vec
mpResid a b = [ foldl' min posInf [ if isEps bi then negInf else bi - aij
                                  | (r, bi) <- zip a b, let aij = vix r j, not (isEps aij) ]
              | j <- [0 .. m - 1] ]
  where m = case a of { (r : _) -> length r; [] -> 0 }

mpVecLeq :: Double -> Vec -> Vec -> Bool
mpVecLeq tol u v = and (zipWith (\a b -> isEps a || a <= b + tol) u v) && length u <= length v

mpVecEq :: Double -> Vec -> Vec -> Bool
mpVecEq tol u v = length u <= length v && and (zipWith (\a b -> (isEps a && isEps b) || (not (isEps a) && not (isEps b) && abs (a - b) <= tol)) u v)
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- 矢印（ノードの半径 r の手前で止める）。bend で少し曲げる（往復の辺を分ける）。返り値の 2 つ目 = 数字を書く位置
arrowTo :: Float -> Point -> Point -> Float -> Float -> (Picture, Point)
arrowTo wt (xa, ya) (xb, yb) r bend = (Pictures [thickLine wt (ax, ay) (bx, by), Polygon [(bx, by), (hx - uy * 6, hy + ux * 6), (hx + uy * 6, hy - ux * 6)]], lp)
  where
    dx = xb - xa
    dy = yb - ya
    l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
    ux = dx / l
    uy = dy / l
    ox = -uy * bend
    oy = ux * bend
    ax = xa + ux * r + ox
    ay = ya + uy * r + oy
    bx = xb - ux * r + ox
    by = yb - uy * r + oy
    hx = bx - ux * 12
    hy = by - uy * 12
    lp = ((ax + bx) / 2 + ox * 0.6, (ay + by) / 2 + oy * 0.6)
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine
-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
