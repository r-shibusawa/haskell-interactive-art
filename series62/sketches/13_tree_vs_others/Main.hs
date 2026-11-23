-- ============================================
-- 62-13 木の得意と苦手 ─ 同じデータで、決定木・k 近傍法・ロジスティック回帰の境界を並べて比べる
-- 決定木は「x1 ≤ ○」「x2 ≤ ○」としか聞けない → 境界はいつも軸に平行な階段。斜めの境界は苦手、XOR や箱は得意
-- k 近傍法 = いちばん近い k 個の訓練の点の多数決。ロジスティック回帰 = 直線 1 本（勾配法で自作）、XOR や円は表せない
--   ← → でデータを回転させると、ロジスティック回帰と k 近傍法は同じ形のまま回るが、木は形が変わる（回転に弱い）
--   XOR は「最初の 1 回」ではどこで切っても情報利得がほぼ 0 → 欲ばりな CART は最初の切り方をまちがえる（画面に実際の利得を表示）。H で手作りの深さ 2 の木と比べる
-- 操作：1 斜め / 2 円 / 3 XOR / 4 箱 / ← → 回転 / ↑↓ 木の深さ / K 近傍の数 k / H 手作りの XOR の木（深さ 2）/ R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, nub, sort, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kinds :: [Int]
kinds = [1, 2, 3, 4]       -- shapes の番号：斜め・円・XOR・箱

nTrain, nTest, cells :: Int
nTrain = 200
nTest = 400
cells = 60                 -- 境界の絵のマスの数（60 × 60）

data World = World
  { wKindI  :: !Int
  , wRot    :: !Int         -- 回転（度）
  , wDepth  :: !Int         -- ★ 木の深さ
  , wK      :: !Int         -- 近傍の数 k
  , wSeed   :: !Int
  , wHand   :: !Bool        -- 手作りの XOR の木を使うか
  , wTrain  :: !DataSet
  , wTestD  :: !DataSet
  , wTree   :: !Node
  , wLogi   :: !Logi
  , wEdge   :: [(Double, Double)]               -- 本当の境界の点
  , wGains  :: ((Double, Double), (Double, Double), Double)   -- 根の利得 (x1 の最良, しきい値), (x2 …), x1 ≤ 0.5 の利得
  , wImgs   :: [Picture]
  , wAccs   :: [Double]
  , wTrAcc  :: [Double]
  , wKnnLab :: !(U.Vector Int)   -- k 近傍法の答え（マスごと）
  , wJob    :: !Int              -- k 近傍法の計算の進み具合：−1 = 待つ、0〜5 = 10 行ずつ塗る、6 = 正解率、7 = おわり
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-13 Trees vs others" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (length (show (wAccs world0, wTrAcc world0, wEdge world0)))   -- 最初のフレームが重くならないように先に計算
      mapM_ evaluate (wImgs world0)
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (knnJob w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = finishKnn $ newData (World 0 0 3 5 13 False (DataSet V.empty U.empty) (DataSet V.empty U.empty) nilNode (Logi 0 0 0 []) [] ((0, 0.5), (0, 0.5), 0) [] [0, 0, 0] [0, 0, 0] U.empty 0 [])

kindOf :: World -> Int
kindOf w = headOr 1 (drop (wKindI w) kinds)

-- ★ 回転したデータ：点を −θ 回してから、もとの形のルールでラベルをつける
rotLabel :: Int -> Double -> Double -> Int -> Int
rotLabel kind u v deg =
  let a = (-(fromIntegral deg) * pi) / 180
      du = u - 0.5
      dv = v - 0.5
  in trueLabel kind (0.5 + du * cos a - dv * sin a) (0.5 + du * sin a + dv * cos a)

makeRot :: Int -> Int -> Int -> Int -> DataSet
makeRot kind n deg seed = fst $ evalR (rngOf 1) $ do
  seedRng seed
  ps <- mapM (\_ -> do
          u0 <- rnd
          v0 <- rnd
          let u = 0.02 + 0.96 * u0
              v = 0.02 + 0.96 * v0
          pure (pt2 u v, rotLabel kind u v deg)) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))

newData :: World -> World
newData w =
  let kd = kindOf w
      tr = makeRot kd nTrain (wRot w) (wSeed w)
      te = makeRot kd nTest (wRot w) (wSeed w + 1)
      edge = [ (u, v) | j <- [0 .. 89 :: Int], i <- [0 .. 89 :: Int]       -- 本当の境界：細かいマスで、となりとラベルがちがう所
                      , let u = (fromIntegral i + 0.5) / 90
                            v = (fromIntegral j + 0.5) / 90
                            c = rotLabel kd u v (wRot w)
                      , (i + j) `mod` 2 == 0 && (rotLabel kd (u + 1 / 90) v (wRot w) /= c || rotLabel kd u (v + 1 / 90) (wRot w) /= c) ]
      gains = (bestGainFeat (dsX tr) (dsY tr) 0, bestGainFeat (dsX tr) (dsY tr) 1, gainAtCut (dsX tr) (dsY tr) 0 0.5)
  in retrainTree $ restartKnn w { wTrain = tr, wTestD = te, wLogi = logisticTrain (dsX tr) (dsY tr) 400 8, wEdge = edge, wGains = gains }

retrainTree :: World -> World
retrainTree w =
  let tr = wTrain w
      te = wTestD w
      t = growTreeP defOpt { oMaxDepth = wDepth w } (dsX tr) (dsY tr)
      fns = models w { wTree = t }
      acc fn = (accuracyOf fn (dsX te) (dsY te), accuracyOf fn (dsX tr) (dsY tr))
      (aT, rT) = acc (headOr (const 0) fns)
      (aL, rL) = acc (lastOr (const 0) fns)
  in w { wTree = t, wAccs = [aT, headOr 0 (drop 1 (wAccs w)), aL], wTrAcc = [rT, headOr 0 (drop 1 (wTrAcc w)), rL]
       , wImgs = [labelImg (\u v -> headOr (const 0) fns (pt2 u v)), headOr Blank (drop 1 (wImgs w)), labelImg (\u v -> lastOr (const 0) fns (pt2 u v))] }

-- k 近傍法を最初から計算し直す（データか k が変わったとき）。重いので step で少しずつ進める
restartKnn :: World -> World
restartKnn w = w { wJob = -1, wKnnLab = U.replicate (cells * cells) 0 }

-- 全部を一度に（起動のとき）
finishKnn :: World -> World
finishKnn w = if wJob w >= 7 then w else finishKnn (knnJob w)

knnJob :: World -> World
knnJob w
  | j < 0 = w { wJob = 0 }                  -- データを作ったフレームでは休む
  | j <= 5 =
      let fn = knnFast (wTrain w) (wK w)
          rows = [j * 10 .. j * 10 + 9]
          new = [ (i + cells * r, fn (pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral r + 0.5) / fromIntegral cells))) | r <- rows, i <- [0 .. cells - 1] ]
          lab = wKnnLab w U.// new
      in w { wKnnLab = lab, wJob = j + 1, wImgs = setAt 1 (labelImg (\u v -> lab U.! (min (cells - 1) (floor (u * fromIntegral cells)) + cells * min (cells - 1) (floor ((1 - v) * fromIntegral cells))))) (wImgs w) }
  | j == 6 =
      let fn = knnFast (wTrain w) (wK w)
      in w { wJob = 7, wAccs = setAt 1 (accuracyOf fn (dsX (wTestD w)) (dsY (wTestD w))) (wAccs w), wTrAcc = setAt 1 (accuracyOf fn (dsX (wTrain w)) (dsY (wTrain w))) (wTrAcc w) }
  | otherwise = w
  where j = wJob w

setAt :: Int -> a -> [a] -> [a]
setAt k v xs = [if i == k then v else x | (i, x) <- zip [0 ..] xs]

-- 答え 0 / 1 → 境界の絵
labelImg :: (Double -> Double -> Int) -> Picture
labelImg fn = paintImage cells cells (\u v -> mixCol (if fn u v == 1 then cnt [0, 1] else cnt [1, 0]) 0.45)

isXor :: World -> Bool
isXor w = kindOf w == 3

models :: World -> [Pt -> Int]
models w = [ if wHand w && isXor w then treePredict xorHandTree else treePredict (wTree w)
           , knnFast (wTrain w) (wK w)
           , \x -> if logisticProb (wLogi w) x > 0.5 then 1 else 0 ]

-- ★ 特徴量 f だけで切るときの、いちばん大きい情報利得（ビット）とそのしきい値（全部の中点）
bestGainFeat :: V.Vector Pt -> U.Vector Int -> Int -> (Double, Double)
bestGainFeat xs ys f = go 0 0 0 (0, 0.5)
  where
    o = U.fromList (sortOn (\i -> xAt xs i f) [0 .. V.length xs - 1])
    p0 = fromIntegral (U.length (U.filter (== 0) ys))
    p1 = fromIntegral (U.length (U.filter (== 1) ys))
    pc = cnt [p0, p1]
    go !k !l0 !l1 best
      | k + 1 >= U.length o = best
      | otherwise =
          let i = o U.! k
              (l0', l1') = if ys U.! i == 0 then (l0 + 1, l1) else (l0, l1 + 1)
              a = xAt xs i f
              b = xAt xs (o U.! (k + 1)) f
              g = gainOf Entropy pc (cnt [l0', l1']) (cnt [p0 - l0', p1 - l1'])
          in go (k + 1) l0' l1' (if b > a && g > fst best then (g, (a + b) / 2) else best)

gainAtCut :: V.Vector Pt -> U.Vector Int -> Int -> Double -> Double
gainAtCut xs ys f t =
  let side p = U.accum (+) (U.replicate 2 0) [(ys U.! i, 1) | i <- [0 .. V.length xs - 1], p (xAt xs i f <= t)]
      l = side id
      r = side not
  in gainOf Entropy (U.zipWith (+) l r) l r

-- 手作りの XOR の木（深さ 2）：x1 ≤ 0.5？ → x2 ≤ 0.5？
xorHandTree :: Node
xorHandTree = numberTree (inner 0 0 (inner 1 1 (leaf 0) (leaf 1)) (inner 1 1 (leaf 1) (leaf 0)))
  where
    leaf c = nilNode { nLeaf = True, nPred = c, nDepth = 2, nCounts = cnt (if c == 0 then [1, 0] else [0, 1]) }
    inner f d l r = nilNode { nLeaf = False, nFeat = f, nThr = 0.5, nDepth = d, nLeft = l, nRight = r }

-- k 近傍法（kit の knnPredict と同じ答え：近い順に挿入、同じ距離なら先の点が前。速くするため列を分けて持つ）
knnFast :: DataSet -> Int -> Pt -> Int
knnFast ds k = \x -> runST $ do
  let !x0 = x U.! 0
      !x1 = x U.! 1
  bd <- MU.replicate (max 1 k) 0 :: ST s (MU.MVector s Double)   -- 近い順の距離²
  bc <- MU.replicate (max 1 k) 0 :: ST s (MU.MVector s Int)      -- そのクラス
  let go !i !len
        | i >= n = pure len
        | otherwise = do
            let !a = U.unsafeIndex c0 i - x0
                !b = U.unsafeIndex c1 i - x1
                !d = a * a + b * b
            lastD <- if len > 0 then MU.unsafeRead bd (len - 1) else pure 0
            if len < k || d < lastD
              then do
                let shiftDown !q = if q > 0
                                     then do
                                       e <- MU.unsafeRead bd (q - 1)
                                       if e > d
                                         then do
                                           MU.unsafeWrite bd q e
                                           MU.unsafeRead bc (q - 1) >>= MU.unsafeWrite bc q
                                           shiftDown (q - 1)
                                         else pure q
                                     else pure q
                q <- shiftDown (if len < k then len else len - 1)
                MU.unsafeWrite bd q d
                MU.unsafeWrite bc q (U.unsafeIndex ys i)
                go (i + 1) (min k (len + 1))
              else go (i + 1) len
  len <- go 0 0
  v1 <- foldM (\acc q -> (\c -> if c == 1 then acc + 1 else acc) <$> MU.unsafeRead bc q) (0 :: Int) [0 .. len - 1]
  pure (if v1 > len - v1 then 1 else 0)
  where
    n = V.length (dsX ds)
    ys = dsY ds
    c0 = U.generate n (\i -> xAt (dsX ds) i 0)
    c1 = U.generate n (\i -> xAt (dsX ds) i 1)

draw :: Env -> World -> Picture
draw env w = view env $
  concat [ panel k bx t img a ta | (k, t, img, a, ta) <- zip5 [0 :: Int ..] titles (wImgs w) (wAccs w) (wTrAcc w), let bx = Box (40 + fromIntegral k * (side + 40)) 130 side side ]
  ++
  -- ---- 文字
  [ say ("Trees vs others   data: " ++ shapeName (kindOf w) ++ "   rotation " ++ show (wRot w) ++ " deg") 40 40 20 (grayC 240)
  , say "1 diagonal  2 circle  3 XOR  4 box   <- -> rotate (15 deg)   Up/Down tree depth   K neighbours k   H hand-made XOR tree   R new data" 40 66 15 (grayC 170)
  , if isXor w
      then let ((g0, t0), (g1, t1), gc) = wGains w
           in sayParas
                [ "Why does the tree struggle on XOR? CART only looks for the cut that gains most right now (greedy). On this data the best root information gain is  x1: "
                  ++ fmtNum 4 g0 ++ " bits (x1 <= " ++ fmtNum 3 t0 ++ "),  x2: " ++ fmtNum 4 g1 ++ " bits (x2 <= " ++ fmtNum 3 t1 ++ ")."
                , "The right cut x1 <= 0.5 also gains only " ++ fmtNum 4 gc ++ " bits (both sides stay half A, half B). But a second cut x2 <= 0.5 makes everything pure. Choosing by the first gain alone picks a useless line near the edge, and even depth 4 may stay around 60%. Press H to compare with the hand-made depth-2 tree ("
                  ++ (if wHand w then "shown now" else "close to 100%") ++ ")." ]
                40 ty (envW env - 80) 14 8 (rgbC 255 225 110)
      else Pictures
        [ sayParas [ "Decision tree: only axis-parallel lines. A diagonal is approximated by a staircase -> needs depth. But 'case splits' like XOR or a box need few questions."
                   , "k nearest neighbours: any shape, but must remember every training point. Logistic regression: one straight line, so no circle or XOR. White dots = true boundary."
                   , "Fixes for trees: make features (add x1 + x2 ...), mix many trees (08-11). Rotation changes the tree's score because the feature axes carry meaning." ]
            40 ty (envW env - 80) 14 8 (grayC 190) ]
  , drawSelfTest env (wTest w) ]
  where
    side = min ((envW env - 160) / 3) (envH env - 260)
    ty = 130 + side + 50
    titles = [ if wHand w && isXor w then "hand-made tree (depth 2)" else "decision tree (CART, depth " ++ show (wDepth w) ++ ")"
             , "k nearest neighbours (k = " ++ show (wK w) ++ ")", "logistic regression (line)" ]
    tr = wTrain w
    panel k bx t img a ta =
      [ imageBox (bxX bx) (bxY bx) side side cells cells img
      , Pictures [ Pictures [Color (grayC 15) (circleAt px py 7.6), Color (clsC (dsY tr U.! i)) (circleAt px py 6)]
                 | i <- [0 .. V.length (dsX tr) - 1], let (px, py) = toPx bx (xAt (dsX tr) i 0) (xAt (dsX tr) i 1) ]
      , Color (rgbaC 255 255 255 150) (Pictures [ rectTL px py 1.8 1.8 | (u, v) <- wEdge w, let (px, py) = toPx bx u v ])   -- 本当の境界（白い点線）
      , Color (rgbC 90 120 100) (frameTL 1 (bxX bx) (bxY bx) side side)
      , say t (bxX bx) (bxY bx - 12) 16 (grayC 240)
      , say (if k == 1 && wJob w < 7 then "computing..." else "test " ++ fmtNum 1 (a * 100) ++ "%   train " ++ fmtNum 1 (ta * 100) ++ "%") (bxX bx) (bxY bx + side + 28) 16 (rgbC 255 240 150) ]

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 (a : as) (b : bs) (c : cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> newData w { wKindI = fromEnum c - fromEnum '1' }
    | c == 'h' || c == 'H' -> retrainTree w { wHand = not (wHand w) }
    | c == 'k' || c == 'K' -> retrainTree $ restartKnn w { wK = if wK w == 1 then 5 else if wK w == 5 then 15 else 1 }
    | c == 'r' || c == 'R' -> newData w { wSeed = wSeed w + 1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> newData w { wRot = (wRot w + 15) `mod` 360 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> newData w { wRot = (wRot w + 345) `mod` 360 }
  EventKey (SpecialKey KeyUp) Down _ _ -> retrainTree w { wDepth = min 10 (wDepth w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> retrainTree w { wDepth = max 1 (wDepth w - 1) }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("斜めの境界：深さ 3 の木は訓練でも 100% にならない（" ++ fmtNum 1 (aT * 100) ++ "%）、ロジスティック回帰は ≥ 98%（" ++ fmtNum 1 (aL * 100) ++ "%）", aT < 1 && aL >= 0.98)
  , ("ロジスティック回帰の損失（交差エントロピー）は勾配法の 1 歩ごとに減る", and (zipWith (\a b -> b <= a + 1e-12) ls (drop 1 ls)) && noNaN ls)
  , ("k 近傍法で k = 1 なら訓練の点は 100% 当たる", accuracyOf (\x -> knnPredict (dsX d) (dsY d) x 1 2) (dsX d) (dsY d) == 1)
  , ("箱（軸に平行、辺が 4 本）：深さ 4 の木のテスト " ++ fmtNum 1 (a0 * 100) ++ "% ≥ 95%、45° 回すと " ++ fmtNum 1 (a45 * 100) ++ "% に下がる", a0 >= 0.95 && a45 < a0 - 0.03)
  , ("XOR：深さ 4 の木 " ++ fmtNum 1 (aXT * 100) ++ "% ≥ 90%、ロジスティック回帰 " ++ fmtNum 1 (aXL * 100) ++ "% ≤ 65%（直線では無理）", aXT >= 0.9 && aXL <= 0.65)
  , ("深さ 3 の木の葉は 8 枚以下、境界は軸に平行（どの分割も x1 か x2 のしきい値）", countLeaves t3 <= 8 && all (\q -> nLeaf q || nFeat q == 0 || nFeat q == 1) (treeNodes t3))
  , ("既定の XOR：根の最良の情報利得 x1 " ++ fmtNum 4 g0 ++ "・x2 " ++ fmtNum 4 g1 ++ " < 0.05 ビット、x1 ≤ 0.5 で切ったあとの 2 回目は " ++ fmtNum 3 g2 ++ " > 0.9 ビット", g0 < 0.05 && g1 < 0.05 && g2 > 0.9)
  , ("手作りの深さ 2 の木は既定の XOR の訓練・テストとも 100%（欲ばりな CART の深さ 4 より上）",
      accuracyOf (treePredict xorHandTree) (dsX xd) (dsY xd) == 1 && accuracyOf (treePredict xorHandTree) (dsX xt) (dsY xt) == 1
      && accuracyOf (treePredict (tree4 xd)) (dsX xt) (dsY xt) < 1) ]
  where
    d = makeRot 1 200 0 31
    t3 = growTreeP defOpt { oMaxDepth = 3 } (dsX d) (dsY d)
    lg = logisticTrain (dsX d) (dsY d) 400 8
    ls = lLosses lg
    lpred m x = if logisticProb m x > 0.5 then 1 else 0
    aT = accuracyOf (treePredict t3) (dsX d) (dsY d)
    aL = accuracyOf (lpred lg) (dsX d) (dsY d)
    tree4 s = growTreeP defOpt { oMaxDepth = 4 } (dsX s) (dsY s)
    b0 = makeRot 4 200 0 32
    b0t = makeRot 4 400 0 33
    b45 = makeRot 4 200 45 32
    b45t = makeRot 4 400 45 33
    a0 = accuracyOf (treePredict (tree4 b0)) (dsX b0t) (dsY b0t)
    a45 = accuracyOf (treePredict (tree4 b45)) (dsX b45t) (dsY b45t)
    x0 = makeRot 3 200 0 34
    x0t = makeRot 3 400 0 35
    lx = logisticTrain (dsX x0) (dsY x0) 400 8
    aXT = accuracyOf (treePredict (tree4 x0)) (dsX x0t) (dsY x0t)
    aXL = accuracyOf (lpred lx) (dsX x0t) (dsY x0t)
    xd = makeRot 3 200 0 13
    g0 = fst (bestGainFeat (dsX xd) (dsY xd) 0)
    g1 = fst (bestGainFeat (dsX xd) (dsY xd) 1)
    li = [i | i <- [0 .. V.length (dsX xd) - 1], xAt (dsX xd) i 0 <= 0.5]
    g2 = fst (bestGainFeat (V.fromList [dsX xd V.! i | i <- li]) (U.fromList [dsY xd U.! i | i <- li]) 1)
    xt = makeRot 3 400 0 14

-- ---------- やってみよう ----------
-- 1. 1（斜め）で ↑ を押して木を深くしていくと、階段はどうなる？ テストの成績は？
-- 2. 4（箱）で → を 3 回（45°）押すと、木の成績はどれくらい下がる？ ほかの 2 つは？
-- 3. 3（XOR）でロジスティック回帰はなぜ 50% くらい？ 木は深さ 2 で表せるのに、深さ 3 では足りないことがある。なぜ？（最初の分割ではジニがほとんど減らないので、欲ばりな CART は迷う）
-- 4. K で k を 1・5・15 に。k = 1 の境界はなぜギザギザ？
-- 5. 木に x1 + x2 という特徴量を足せば、斜めも 1 回で切れる。retrainTree の中で点に列を足すにはどうする？
-- 6. 深さ 2 で XOR を表せる木を、02 で手で作れる？（02 では 4 キーで XOR）。なぜ欲ばりな CART はそれを見つけられない？ H で手作りの木と比べよう

-- ==================================================
-- ここから下は「決定木の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・いちばん大きい番号
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1, 2）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

-- いちばん大きい番号（同点なら小さい番号）
argMaxU :: U.Vector Double -> Int
argMaxU a = if U.null a then 0 else U.ifoldl' (\b i v -> if v > a U.! b then i else b) 0 a

maxU :: U.Vector Double -> Double
maxU a = if U.null a then 0 else U.maximum a

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- リストの最初（なければ d）
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

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
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

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 不純度：counts = 各クラスの個数（重みつきでもよい）
data Crit = Gini | Entropy | MisErr deriving (Eq, Show)

log2 :: Double -> Double
log2 q = log q / log 2

entropyP :: Double -> Double               -- 2 クラスのエントロピー（ビット）
entropyP p = if p <= 0 || p >= 1 then 0 else -p * log2 p - (1 - p) * log2 (1 - p)

entropyOf :: U.Vector Double -> Double
entropyOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\h c -> if c > 0 then let q = c / n in h - q * log2 q else h) 0 counts)

giniOf :: U.Vector Double -> Double
giniOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\s c -> s - (c / n) * (c / n)) 1 counts)

errOf :: U.Vector Double -> Double           -- 誤分類率
errOf counts = let n = sumU counts in if n <= 0 then 0 else max 0 (1 - maxU counts / n)

impurityOf :: Crit -> U.Vector Double -> Double
impurityOf crit = case crit of
  Entropy -> entropyOf
  MisErr -> errOf
  Gini -> giniOf

-- 分けたときの「不純度の減り」= 親 − (左の割合·左 + 右の割合·右)。Entropy なら情報利得
gainOf :: Crit -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Double
gainOf crit parent left right =
  let n = sumU parent
      nl = sumU left
      nr = sumU right
  in if n <= 0 then 0 else impurityOf crit parent - (nl / n) * impurityOf crit left - (nr / n) * impurityOf crit right

cnt :: [Double] -> U.Vector Double
cnt = U.fromList

-- ---- CART（分類）：全部の特徴量 × 全部のしきい値（並べた値の中点）を調べて、不純度がいちばん減る分割を選ぶ
-- ノード：分類の木も回帰の木もこの形。葉の nLeft, nRight は使わない
data Node = Node
  { nLeaf   :: !Bool
  , nFeat   :: !Int                 -- 何番目の特徴量で切るか（0 = x1）
  , nThr    :: !Double              -- しきい値（x ≤ thr なら左）
  , nLeft   :: Node
  , nRight  :: Node
  , nCounts :: !(U.Vector Double)   -- 各クラスの個数（重みつき）
  , nN      :: !Int                 -- 点の数
  , nImp    :: !Double              -- 不純度
  , nPred   :: !Int                 -- 多数決のクラス
  , nDec    :: !Double              -- 分けたときの重みつきの不純度の減り（回帰木では SSE の減り）
  , nDepth  :: !Int
  , nId     :: !Int                 -- 行きがけ順の番号（0 = 根）
  , nCut    :: !Double              -- 剪定：この α 以上で葉として扱う（刈られないなら +∞）
  , nValue  :: !Double              -- 回帰木：葉の値（平均）
  , nSse    :: !Double              -- 回帰木：2 乗誤差
  }

nilNode :: Node
nilNode = Node True 0 0 nilNode nilNode U.empty 0 0 0 0 0 0 posInf 0 0

-- o = { maxDepth, minLeaf, crit, mtry（分割ごとに見る特徴量の数、0 = 全部）, w（点の重み）, idx（使う点の番号）, nClass（0 = 自動）}
data TreeOpt = TreeOpt
  { oMaxDepth :: !Int, oMinLeaf :: !Int, oCrit :: !Crit, oMtry :: !Int
  , oW :: !(Maybe (U.Vector Double)), oIdx :: !(Maybe (U.Vector Int)), oNClass :: !Int }

defOpt :: TreeOpt
defOpt = TreeOpt 40 1 Gini 0 Nothing Nothing 0

-- 育てるときの道具箱：cCols = 特徴量ごとの列、cW = 点の重み
data Ctx = Ctx { cCols :: !(V.Vector (U.Vector Double)), cY :: !(U.Vector Int), cW :: !(U.Vector Double)
               , cK :: !Int, cD :: !Int, cO :: !TreeOpt }

-- 木を育てる（mtry を使うときだけ乱数を引く）
-- 速くするため、根で特徴量ごとに点を 1 回だけ並べ、子にはその並びを分けて渡す（並べ直したのと同じ順になる）
growTree :: TreeOpt -> V.Vector Pt -> U.Vector Int -> R Node
growTree o xs ys = numberTree <$> growNode ctx idx0 ords0 0
  where
    k = if oNClass o > 0 then oNClass o else max 1 (if U.null ys then 1 else U.maximum ys + 1)
    d = if V.null xs then 0 else U.length (V.head xs)
    n = V.length xs
    idx0 = fromMaybe (U.enumFromN 0 n) (oIdx o)
    cols = V.generate d (\f -> U.generate n (\i -> xAt xs i f))
    ords0 = V.map (\col -> U.fromList (sortOn (col U.!) (U.toList idx0))) cols
    ctx = Ctx cols ys (fromMaybe (U.replicate n 1) (oW o)) k d o { oMinLeaf = max 1 (oMinLeaf o) }

-- 乱数を使わない木（mtry = 0）
growTreeP :: TreeOpt -> V.Vector Pt -> U.Vector Int -> Node
growTreeP o xs ys = fst (runR (growTree o { oMtry = 0 } xs ys) (Rng 1))

-- idx = このノードの点（重複あり）、ords = 特徴量ごとに値の順に並べた idx
growNode :: Ctx -> U.Vector Int -> V.Vector (U.Vector Int) -> Int -> R Node
growNode ctx idx ords depth = do
  let o = cO ctx
      counts = U.accumulate (+) (U.replicate (cK ctx) 0) (U.map (\i -> (cY ctx U.! i, cW ctx U.! i)) idx)
      node = nilNode { nLeaf = True, nCounts = counts, nN = U.length idx, nImp = impurityOf (oCrit o) counts
                     , nPred = argMaxU counts, nDepth = depth, nDec = 0 }
      kinds = U.length (U.filter (> 0) counts)
  if depth >= oMaxDepth o || U.length idx < 2 * oMinLeaf o || kinds <= 1 then pure node else do
    mb <- bestSplit ctx ords counts
    case mb of
      Nothing -> pure node
      Just (f, thr, dec) -> do
        let col = cCols ctx V.! f
            goL i = col U.! i <= thr
            (l, r) = U.partition goL idx
        lt <- growNode ctx l (V.map (U.filter goL) ords) (depth + 1)
        rt <- growNode ctx r (V.map (U.filter (not . goL)) ords) (depth + 1)
        pure node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }

-- ★ いちばんよい分割：特徴量ごとに点を並べ、左から 1 つずつ動かしながら左右の個数を更新する
bestSplit :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> R (Maybe (Int, Double, Double))
bestSplit ctx ords counts = do
  let d = cD ctx
      mtry = oMtry (cO ctx)
  feats <- if mtry > 0 && mtry < d then U.toList <$> shuffleU (U.enumFromN 0 d) else pure [0 .. d - 1]   -- ランダムフォレスト：前の mtry 個だけを見る
  let first = if mtry > 0 then min mtry d else d
      best = splitOver ctx ords counts (take first feats)
  pure (if isNothing best && first < d then splitOver ctx ords counts (drop first feats) else best)   -- 切れる特徴量が 1 つもないときだけ残りを見る

splitOver :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> [Int] -> Maybe (Int, Double, Double)
splitOver ctx ords counts = foldl' feat Nothing
  where
    o = cO ctx
    crit = oCrit o
    minLeaf = oMinLeaf o
    w = sumU counts
    pImp = impurityOf crit counts
    feat best f =
      let ord = ords V.! f
          col = cCols ctx V.! f
          len = U.length ord
          go !k !lc !lw !b
            | k >= len - 1 = b
            | otherwise =
                let i = U.unsafeIndex ord k
                    wi = U.unsafeIndex (cW ctx) i
                    yi = U.unsafeIndex (cY ctx) i
                    lc' = U.unsafeUpd lc [(yi, U.unsafeIndex lc yi + wi)]
                    lw' = lw + wi
                    v0 = U.unsafeIndex col i
                    v1 = U.unsafeIndex col (U.unsafeIndex ord (k + 1))
                in if not (v1 > v0) || k + 1 < minLeaf || len - k - 1 < minLeaf then go (k + 1) lc' lw' b else
                   let rc = U.zipWith (-) counts lc'
                       dec = w * pImp - lw' * impurityOf crit lc' - (w - lw') * impurityOf crit rc
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) lc' lw' b'
      in go 0 (U.replicate (cK ctx) 0) 0 best

-- 行きがけ順に番号をつけ直す（p5.js 版の node.id と同じ順）
numberTree :: Node -> Node
numberTree t0 = fst (go 0 t0)
  where
    go !n t | nLeaf t = (t { nId = n }, n + 1)
            | otherwise = let (l, n1) = go (n + 1) (nLeft t)
                              (r, n2) = go n1 (nRight t)
                          in l `seq` r `seq` (t { nId = n, nLeft = l, nRight = r }, n2)

-- 見せる深さ dLim、剪定の α = aLim で「葉として扱う」か
isCut :: Int -> Double -> Node -> Bool
isCut dLim aLim t = nLeaf t || nDepth t >= dLim || aLim >= nCut t

noD :: Int                                    -- 深さの制限なし
noD = maxBound

-- 根から葉までたどる。dLim = 見せる深さ、aLim = 剪定の α（制限なし = noD, negInf）
leafOfL :: Int -> Double -> Node -> Pt -> Node
leafOfL dLim aLim node x
  | isCut dLim aLim node = node
  | otherwise = leafOfL dLim aLim (if x U.! nFeat node <= nThr node then nLeft node else nRight node) x

leafOf :: Node -> Pt -> Node
leafOf = leafOfL noD negInf

treePredict :: Node -> Pt -> Int
treePredict t x = nPred (leafOf t x)

treePredictL :: Int -> Double -> Node -> Pt -> Int
treePredictL dl al t x = nPred (leafOfL dl al t x)

-- 見えているノードを全部（行きがけ順）
treeNodesL :: Int -> Double -> Node -> [Node]
treeNodesL dl al t = t : if isCut dl al t then [] else treeNodesL dl al (nLeft t) ++ treeNodesL dl al (nRight t)

treeNodes :: Node -> [Node]
treeNodes = treeNodesL noD negInf

countLeavesL :: Int -> Double -> Node -> Int
countLeavesL dl al t = length (filter (isCut dl al) (treeNodesL dl al t))

countLeaves :: Node -> Int
countLeaves = countLeavesL noD negInf

treeDepthL :: Int -> Double -> Node -> Int
treeDepthL dl al t = maxOr 0 (map nDepth (treeNodesL dl al t))

treeDepth :: Node -> Int
treeDepth = treeDepthL noD negInf

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double   -- fn(x) → クラス
accuracyOf fn xs ys = let n = V.length xs
                          ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) [0 .. n - 1])
                      in if n > 0 then fromIntegral ok / fromIntegral n else 0

accuracyIdx :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> [Int] -> Double   -- idx の点だけ
accuracyIdx fn xs ys ids = let ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) ids)
                           in if null ids then 0 else fromIntegral ok / fromIntegral (length ids)

-- ---- 合成データ（[0, 1]² の 2 次元、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 箱 / 5 しま
shapes :: [String]
shapes = ["blobs", "diagonal", "circle", "XOR", "box", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapes)

sq2 :: Double -> Double
sq2 a = a * a

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq2 (u - 0.32) + sq2 (v - 0.36) < sq2 (u - 0.68) + sq2 (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq2 (u - 0.5) + sq2 (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if u > 0.25 && u < 0.7 && v > 0.3 && v < 0.8 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2D :: Int -> Int -> Double -> Int -> R DataSet
makeData2D kind n flp seed = do
  seedRng seed
  ps <- mapM (\_ -> do
          u0 <- rnd
          v0 <- rnd
          let u = 0.02 + 0.96 * u0
              v = 0.02 + 0.96 * v0
              c = trueLabel kind u v
          r <- rnd
          pure (U.fromList [u, v], if r < flp then 1 - c else c)) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))

-- ---- 比べる相手：k 近傍法と、ロジスティック回帰（勾配法で自作）
knnPredict :: V.Vector Pt -> U.Vector Int -> Pt -> Int -> Int -> Int   -- いちばん近い k 個を、並べかえずに挿入で集める
knnPredict xs ys x k nk =
  let ins (bd, bc) i =
        let d = sq2 (xAt xs i 0 - x U.! 0) + sq2 (xAt xs i 1 - x U.! 1)
            full = length bd >= k
        in if not full || d < lastOr 0 bd
             then let keep = if full then init (zip bd bc) else zip bd bc
                      (a, b) = span (\(e, _) -> e <= d) keep      -- 同じ距離なら先に入った点が前
                      l = a ++ [(d, ys U.! i)] ++ b
                  in (map fst l, map snd l)
             else (bd, bc)
      (_, cs) = foldl' ins ([], []) [0 .. V.length xs - 1]
  in argMaxU (U.accum (+) (U.replicate (max 2 nk) 0) [(c, 1) | c <- cs])

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

data Logi = Logi { lW0 :: !Double, lW1 :: !Double, lB :: !Double, lLosses :: [Double] }

logisticTrain :: V.Vector Pt -> U.Vector Int -> Int -> Double -> Logi   -- w·(x − 0.5) + b。交差エントロピーの勾配を全部の点で平均して下る
logisticTrain xs ys epochs lr = go 0 (Logi 0 0 0 [])
  where
    n = V.length xs
    nn = fromIntegral (if n == 0 then 1 else n)
    go e m | e >= epochs = m { lLosses = reverse (lLosses m) }
           | otherwise =
               let (g0, g1, gb, l) = foldl' (\(!a, !b, !c, !d) i ->
                       let u = xAt xs i 0 - 0.5
                           v = xAt xs i 1 - 0.5
                           p = sigm (lW0 m * u + lW1 m * v + lB m)
                           yi = ys U.! i
                           r = p - fromIntegral yi
                       in (a + r * u, b + r * v, c + r, d - (if yi /= 0 then log (max 1e-12 p) else log (max 1e-12 (1 - p))))) (0, 0, 0, 0) [0 .. n - 1]
               in go (e + 1) (Logi (lW0 m - (lr * g0) / nn) (lW1 m - (lr * g1) / nn) (lB m - (lr * gb) / nn) (l / nn : lLosses m))

logisticProb :: Logi -> Pt -> Double
logisticProb m x = sigm (lW0 m * (x U.! 0 - 0.5) + lW1 m * (x U.! 1 - 0.5) + lB m)

-- ---- 描く道具：クラスの色、領域の絵、点、数の書き方
type RGB = (Double, Double, Double)

clsCol :: Int -> RGB                          -- ★ クラス A・B・C の色
clsCol c = case c of
  0 -> (245, 140, 75)
  1 -> (85, 175, 240)
  2 -> (175, 225, 95)
  _ -> (200, 200, 200)

bgCol :: RGB                                  -- 背景（森の色）
bgCol = (20, 30, 26)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

clsC :: Int -> Color
clsC = colOf . clsCol

-- クラスの割合 → 背景と混ぜた色（k = 色の濃さ）
mixCol :: U.Vector Double -> Double -> RGB
mixCol probs k =
  let s0 = sumU probs
      s = if s0 == 0 then 1 else s0
      add (r, g, b) (j, p) = let (cr, cg, cb) = clsCol j
                                 (br, bg, bb) = bgCol
                             in (r + (p / s) * (cr - br) * k, g + (p / s) * (cg - bg) * k, b + (p / s) * (cb - bb) * k)
  in foldl' add bgCol (zip [0 ..] (U.toList probs))

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ws = 点ごとの大きさの倍率）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Maybe (U.Vector Double) -> Picture
drawPoints bx xs ys r ws = Pictures
  [ Pictures [Color (grayC 15) (circleAt px py (d + 2)), Color (clsC (ys U.! i)) (circleAt px py d)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1)
        d = r * maybe 1 (\w -> f2 (w U.! i)) ws ]

-- 数を短く書く（toFixed(d) のあと余分な 0 を消す。p5.js 版の fmtNum）
-- toFixed と同じく、double のちょうどの値で丸め、ちょうど半分なら絶対値の大きいほうへ
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = stripZeros (if d > 0 then ip ++ "." ++ fp else ip)
                in if n == 0 || v > 0 then body else '-' : body
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if lastOr ' ' t == '.' then init t else t
                     | otherwise = s

-- 件数 [5|7] の形
countsStr :: U.Vector Double -> String
countsStr c = "[" ++ intercalate "|" (map (fmtNum 2) (U.toList c)) ++ "]"

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

-- 段落をいくつか上から順に（左上 = (x, y)、段落の間は gap）
sayParas :: [String] -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
sayParas ps x y maxW size gap c = Pictures (go y ps)
  where go _ [] = []
        go yy (p : rest) = let n = length (wrapText maxW size p)
                           in sayWrap p x yy maxW size c : go (yy + fromIntegral n * size * 1.3 + gap) rest

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
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

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
