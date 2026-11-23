-- ============================================
-- 59-12 min-plus と最短経路 ─ ⊕ を max から min に替えると (min, +) 半環。ゼロは +∞、いちは 0。今度は「いちばん近い道」
-- 迷路のマスがノード。となりのマスへ入るコスト：道 1、ぬかるみ 4、壁は入れない（+∞）
--   ベルマン-フォード法 = min-plus の 1 次方程式 x = A ⊗' x ⊕' b をくり返し x ← A ⊗' x ⊕' b で解く（b = 出発点 0、他は +∞）
--   1 回くり返すごとに「1 歩ぶん」波が広がる。止まった答えを、ダイクストラ法（優先度つきキュー）の答えと比べる
--   画面では ⊕' を (+)'、⊗' を (x)' と書く
-- 操作：ドラッグで塗る（1 壁 / 2 ぬかるみ / 3 道 / 4 出発点）/ スペース 一時停止 / ← → 1 コマのくり返し回数 / R ランダムな迷路 / C 全部消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cols, rows :: Int
cols = 40                  -- ★ 迷路の大きさ
rows = 24

costRoad, costMud :: Double
costRoad = 1               -- ★ マスに入るコスト
costMud = 4

inf :: Double
inf = posInf               -- min-plus のゼロ（行けない）

data World = World
  { wCell    :: !(U.Vector Int)      -- 0 道、1 壁、2 ぬかるみ
  , wSrc     :: !Int                 -- 出発点
  , wBrush   :: !Int
  , wXs      :: !(U.Vector Double)   -- ベルマン-フォードのいまの値
  , wIter    :: !Int
  , wDone    :: !Bool
  , wPaused  :: !Bool
  , wPerFrame :: !Int
  , wDj      :: !(U.Vector Double)   -- ダイクストラ法の答え
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-12 Min-plus shortest paths" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World cell (idx 1 1) 1 U.empty 0 False False 1 U.empty r1 [])
  where
    (t0, r1) = runR (randomMaze cols rows) (seedRng 12)
    cell = t0 U.// [ (idx c r, 0) | c <- [0 .. 2], r <- [0 .. 2] ]

idx :: Int -> Int -> Int
idx c r = c + cols * r

mnAdd :: Double -> Double -> Double          -- a ⊕' b = min(a, b)
mnAdd a b = if a <= b then a else b

mnMul :: Double -> Double -> Double          -- a ⊗' b = a + b（+∞ があれば +∞）
mnMul a b = if a == inf || b == inf then inf else a + b

costOf :: Int -> Double                       -- 0 道、1 壁、2 ぬかるみ
costOf t = if t == 1 then inf else if t == 2 then costMud else costRoad

neighbors :: Int -> Int -> Int -> [Int]       -- 左・右・上・下（p5.js と同じ順）
neighbors i nc nr = let (r, c) = i `divMod` nc
                    in [i - 1 | c > 0] ++ [i + 1 | c < nc - 1] ++ [i - nc | r > 0] ++ [i + nc | r < nr - 1]

-- ★ ベルマン-フォードの 1 回：x_i ← min( b_i, min_j (x_j ⊗' a_ij) )、a_ij = となりの j から i に入るコスト
bfStep :: U.Vector Double -> U.Vector Int -> Int -> Int -> Int -> (U.Vector Double, Bool)
bfStep x t s nc nr = (y, y /= x)
  where
    y = U.generate (U.length x) $ \i ->
          let v0 = if i == s then 0 else inf
              ci = costOf (t U.! i)
          in if ci == inf then v0 else foldl' (\v j -> mnAdd v (mnMul (x U.! j) ci)) v0 (neighbors i nc nr)

-- ★ ダイクストラ法（優先度つきキューの代わりに Data.Set を使う）
dijkstra :: U.Vector Int -> Int -> Int -> Int -> U.Vector Double
dijkstra t s nc nr = go (S.singleton (0, s)) (U.replicate (U.length t) inf U.// [(s, 0)])
  where
    go q d = case S.minView q of
      Nothing -> d
      Just ((dv, v), q')
        | dv > d U.! v -> go q' d
        | otherwise ->
            let ups = [ (u, dv + w) | u <- neighbors v nc nr, let w = costOf (t U.! u), w /= inf, dv + w < d U.! u ]
                -- 同じ u が 2 回出ることはない（となりは別々のマス）
            in go (foldl' (\qq (u, du) -> S.insert (du, u) qq) q' ups) (d U.// ups)

randomMaze :: Int -> Int -> R (U.Vector Int)
randomMaze nc nr = U.fromList <$> mapM (const cellR) [1 .. nc * nr]
  where cellR = do u <- rnd
                   pure (if u < 0.22 then 1 else if u < 0.36 then 2 else 0)

restart :: World -> World
restart w = w { wXs = U.generate (U.length (wCell w)) (\i -> if i == wSrc w then 0 else inf), wIter = 0, wDone = False
              , wDj = dijkstra (wCell w) (wSrc w) cols rows }

step :: World -> World
step w | wPaused w || wDone w = w
       | otherwise = go (wPerFrame w) w
  where
    go 0 v = v
    go k v | wDone v = v
           | otherwise = let (y, ch) = bfStep (wXs v) (wCell v) (wSrc v) cols rows
                         in go (k - 1 :: Int) (if ch then v { wXs = y, wIter = wIter v + 1 } else v { wDone = True })

cellSize :: Env -> Float
cellSize env = fromIntegral (floor (min ((envW env - 80) / fromIntegral cols) ((envH env - 190) / fromIntegral rows)) :: Int)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color col (rectTL (ox + fromIntegral c * cs) (oy + fromIntegral r * cs) (cs - 1) (cs - 1))
  | i <- [0 .. U.length cell - 1]
  , let (r, c) = i `divMod` cols
        t = cell U.! i
        v = xs U.! i
        u = v / dmax
        col | t == 1 = rgbC 90 95 105
            | v == inf = if t == 2 then rgbC 60 45 30 else grayC 34
            | otherwise = makeColor (clamp01 ((255 - 150 * f2 u) / 255)) (clamp01 ((190 - 80 * f2 u + (if t == 2 then -60 else 0)) / 255)) (clamp01 ((90 + 140 * f2 u) / 255)) 1 ]
  -- ---- マウスのマスから出発点までの最短の道（距離が 1 つずつ減るとなりをたどる）
  ++ [ Color white (thickPath 3 [ (ox + (fromIntegral (p `mod` cols) + 0.5) * cs, oy + (fromIntegral (p `div` cols) + 0.5) * cs) | p <- pathFrom i ])
     | Just i <- [hover], xs U.! i < inf ]
  ++ [ Color white (circleAt (ox + (fromIntegral (src `mod` cols) + 0.5) * cs) (oy + (fromIntegral (src `div` cols) + 0.5) * cs) (cs * 0.8))
     , say ("min-plus and shortest paths   iteration " ++ show (wIter w) ++ (if wDone w then " (stopped)" else "") ++ info) 40 40 20 (grayC 240)
     , say ("Drag to paint (now " ++ brushName ++ ")   Space pause   <- -> " ++ show (wPerFrame w) ++ " steps per frame   R random   C clear") 40 66 15 (grayC 170)
     , sayWrap (if wDone w then (if same then "OK: Bellman-Ford (x <- A (x)' x (+)' b) = Dijkstra on all " ++ show (cols * rows) ++ " cells (unreachable cells are +inf)" else "Not equal")
                else "The wave spreads... value after k steps = shortest distance using paths of at most k steps (walking more can still shorten it)")
               40 (oy + fromIntegral rows * cs + 14) (envW env - 80) 16 (if wDone w && same then rgbC 120 210 150 else rgbC 255 220 120)
     , drawSelfTest env (wTest w) ]
  where
    cell = wCell w
    xs = wXs w
    src = wSrc w
    cs = cellSize env
    ox = 40
    oy = 100
    dmax = U.foldl' max 1 (U.filter (< inf) (wDj w))
    (mx, my) = mouseP env
    mc = floor ((mx - ox) / cs) :: Int
    mr = floor ((my - oy) / cs) :: Int
    hover = if mc >= 0 && mc < cols && mr >= 0 && mr < rows then Just (idx mc mr) else Nothing
    info = case hover of
      Just i -> "   mouse cell: Bellman-Ford " ++ mpFmt 0 (xs U.! i) ++ ", Dijkstra " ++ mpFmt 0 (wDj w U.! i)
      Nothing -> ""
    same = xs == wDj w
    brushName = headOr "" (drop (wBrush w) ["", "1 wall", "2 mud", "3 road", "4 start"])
    pathFrom i0 = go (0 :: Int) i0
      where go g i | g >= cols * rows || i == src = [src]
                   | otherwise = let ci = costOf (cell U.! i)
                                 in case [ j | j <- neighbors i cols rows, mnMul (xs U.! j) ci == xs U.! i ] of
                                      (j : _) -> i : go (g + 1) j
                                      [] -> [i, src]
    clamp01 = max 0 . min 1

paint :: Env -> (Float, Float) -> World -> World
paint env p w =
  let cs = cellSize env
      (mx, my) = toP5 env p
      c = floor ((mx - 40) / cs) :: Int
      r = floor ((my - 100) / cs) :: Int
      i = idx c r
  in if c < 0 || c >= cols || r < 0 || r >= rows then w
     else restart (if wBrush w == 4 then w { wSrc = i, wCell = wCell w U.// [(i, 0)] }
                   else if i /= wSrc w then w { wCell = wCell w U.// [(i, case wBrush w of { 1 -> 1; 2 -> 2; _ -> 0 })] }
                   else w)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> paint env p w
  EventMotion p | envDown env -> paint env p w
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> w { wBrush = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> let (t, r') = runR (randomMaze cols rows) (wRng w)
                           t' = t U.// [ (j, 0) | j <- wSrc w : neighbors (wSrc w) cols rows ]
                       in restart w { wCell = t', wRng = r' }
    | c `elem` "cC" -> restart w { wCell = U.replicate (cols * rows) 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPerFrame = min 64 (wPerFrame w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPerFrame = max 1 (wPerFrame w `div` 2) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 120・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Bellman-Ford (min-plus iteration) = Dijkstra (20 random mazes, +inf agrees too)", all (\(a, _, _) -> a) mazes)
  , ("the iteration stops within n - 1 rounds", all (\(_, b, _) -> b) mazes)
  , ("the answer satisfies the triangle inequality x_i <= x_j + (cost to enter i)", all (\(_, _, c) -> c) mazes)
  , ("min-plus matrix product (A (x)' A)_ij = shortest 2-step path (brute force)", all fst mats)
  , ("min-plus = max-plus with signs flipped: A (x)' B = -((-A) (x) (-B))", all snd mats)
  , ("+inf (x)' a = +inf, +inf (+)' a = a (no NaN)", mnMul inf 5 == inf && mnMul inf inf == inf && mnAdd inf 5 == 5) ]
  where
    (mazes, mats) = withSeed 120 $ do
      a <- mapM mazeTest [0 .. 19]
      b <- mapM (const matTest) [1 .. 30 :: Int]
      pure (a, b)
    mazeTest t = do
      let nc = 12 + (t `mod` 5); nr = 9
      t0 <- randomMaze nc nr
      s <- rndInt 0 (nc * nr - 1)
      let tt = t0 U.// [(s, 0)]
          x0 = U.generate (nc * nr) (\i -> if i == s then 0 else inf)
          run x k = let (y, ch) = bfStep x tt s nc nr in if ch && k < nc * nr then run y (k + 1) else (x, k)
          (x, k) = run x0 (0 :: Int)
          d = dijkstra tt s nc nr
          tri = and [ x U.! i <= mnMul (x U.! j) (costOf (tt U.! i)) | i <- [0 .. nc * nr - 1], costOf (tt U.! i) /= inf, j <- neighbors i nc nr ]
      pure (x == d && not (U.any isNaN x), k <= nc * nr - 1, tri)
    matTest = do
      let n = 4
      a <- mapM (const (mapM (const (do u <- rnd; if u < 0.3 then pure inf else fromIntegral <$> rndInt 1 9)) [1 .. n])) [1 .. n :: Int]
      let p = [ [ foldl' mnAdd inf [ mnMul (ix a i k) (ix a k j) | k <- [0 .. n - 1] ] | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
          b = [ [ foldl' min inf [ ix a i k + ix a k j | k <- [0 .. n - 1], ix a i k < inf, ix a k j < inf ] | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
          neg = map (map (\v -> if v == inf then negInf else if isEps v then inf else -v))
          q = neg (mpMatMul (neg a) (neg a))
      pure (p == b, q == p)

-- ---------- やってみよう ----------
-- 1. スペースで止めて ← → で 1 回ずつ進め、波（色のついたマス）が 1 歩ずつ広がるのを見よう
-- 2. 2 でぬかるみを塗って、最短の道がぬかるみをよけるか確かめよう。costMud を 2 にすると？
-- 3. 壁で出発点を囲むと、外のマスはずっと +∞（暗いまま）。ε = −∞ の min 版
-- 4. くり返しの回数とダイクストラ法の手間を比べよう。ダイクストラが速いのはなぜ？（負のコストがないとき）
-- 5. 03 の A^{⊗k}（max）とシリーズ 38 の 09（min）は、符号を反対にすれば同じ計算

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
