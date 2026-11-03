-- ============================================
-- 12 選択問題：最小・最大と中央値
-- 「i 番目に小さい数」を、全部並べずに見つける
-- 1：最小と最大を同時に。2 個ずつ組にすると 2n 回でなく約 1.5n 回の比較で済む
-- 2：RANDOMIZED-SELECT。クイックソートの分割を片側だけくり返す。平均 Θ(n)
-- 3：SELECT（中央値の中央値）。5 個ずつの組の中央値の、さらに中央値をピボットにして、最悪でも Θ(n)
-- スペース：再生／停止　→：1 手　1〜3：方法　←：i を小さく　↑↓ 速さ　R：新しい数列
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

n :: Int
n = 45              -- ★ 要素数

data S = S
  { sA      :: !(U.Vector Int)
  , sGen    :: !StdGen
  , sLo     :: !Int
  , sHi     :: !Int
  , sP      :: !Int
  , sJ      :: !Int
  , sGroups :: ![(Int, Int)]    -- 5 個ずつの組
  , sMeds   :: ![Int]           -- 組の中央値の場所
  , sAnswer :: !Int
  , sMn     :: !Int
  , sMx     :: !Int
  , sComps  :: !Int
  , sDone   :: !Bool
  }

data World = World
  { wS       :: !S
  , wRest    :: [S]
  , wAlgo    :: !Int
  , wTarget  :: !Int             -- 何番目を探すか（0 始まり）
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-12 Selection" initialWorld)
  { skBack  = hsb 300 25 9
  , skStart = \_ -> reset initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: StdGen -> U.Vector Int -> S
initialS g a = S a g (-1) (-1) (-1) (-1) [] [] (-1) (-1) (-1) 0 False

initialWorld :: World
initialWorld = World (initialS (mkStdGen 1) U.empty) [] 2 (n `div` 2) True 6 0

reset :: World -> IO World
reset w = do
  xs <- mapM (const (randomRIO (1, 99))) [1 .. n]
  restart w { wS = (wS w) { sA = U.fromList xs } }

-- 同じ数列のまま、はじめから（分割で数列の順番は変わったまま）
restart :: World -> IO World
restart w = do
  g <- newStdGen
  let a = sA (wS w)
      top = U.length a - 1
      target = clampI 0 (max 0 top) (wTarget w)
      s0 = initialS g a
      program = case wAlgo w of
        1 -> minMax
        2 -> randSelect 0 top target
        _ -> momSelect 0 top target
  pure w { wTarget = target, wS = s0, wRest = steps program s0 }

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

rd :: Int -> Gen S Int
rd i = gets (\s -> sA s U.! i)

-- x < y を調べて比較回数を数える
less :: Int -> Int -> Gen S Bool
less x y = do
  modify (\s -> s { sComps = sComps s + 1 })
  pure (x < y)

-- 最小と最大を同時に
minMax :: Gen S ()
minMax = do
  len <- gets (U.length . sA)
  (mn0, mx0, start) <-
    if even len                    -- 奇数個なら最初の 1 個を最小・最大にする
      then do
        v1 <- rd 1
        v0 <- rd 0
        lt <- less v1 v0
        pure (if lt then (1, 0, 2) else (0, 1, 2))
      else pure (0, 0, 1)
  modify (\s -> s { sMn = mn0, sMx = mx0 })
  yield
  let pair (mn, mx) i = do
        a0 <- rd i
        a1 <- rd (i + 1)
        lt <- less a1 a0                 -- まず 2 個を比べて
        let (small, big) = if lt then (i + 1, i) else (i, i + 1)
        vs <- rd small
        vb <- rd big
        vmn <- rd mn
        vmx <- rd mx
        ltMin <- less vs vmn              -- 小さいほうは最小と
        ltMax <- less vmx vb              -- 大きいほうは最大と
        let mn' = if ltMin then small else mn
            mx' = if ltMax then big else mx
        modify (\s -> s { sMn = mn', sMx = mx', sJ = i })
        yield
        pure (mn', mx')
  _ <- foldM pair (mn0, mx0) [start, start + 2 .. len - 2]
  modify (\s -> s { sJ = -1, sDone = True })
  yield

swapAt :: Int -> Int -> Gen S ()
swapAt i j = modify (\s ->
  let a = sA s
  in s { sA = a U.// [(i, a U.! j), (j, a U.! i)] })

partitionAround :: Int -> Int -> Int -> Gen S Int
partitionAround lo hi pIdx = do
  swapAt pIdx hi
  x <- rd hi
  let go i j
        | j >= hi = pure i
        | otherwise = do
            v <- rd j
            modify (\s -> s { sComps = sComps s + 1 })
            i' <- if v <= x then swapAt (i + 1) j >> pure (i + 1) else pure i
            modify (\s -> s { sJ = j, sP = hi })
            when ((j - lo) `mod` 3 == 0) yield
            go i' (j + 1)
  i <- go (lo - 1) lo
  swapAt (i + 1) hi
  modify (\s -> s { sP = i + 1, sJ = -1 })
  yield
  pure (i + 1)

setRange :: Int -> Int -> Gen S ()
setRange lo hi = modify (\s -> s { sLo = lo, sHi = hi })

-- 答えが見つかった（最後の 1 手を流して終わる）
finish :: Int -> Int -> Int -> Gen S ()
finish ans lo hi = do
  modify (\s -> s { sAnswer = ans, sLo = lo, sHi = hi, sDone = True })
  yield

-- 乱択選択（i は 0 始まり）
randSelect :: Int -> Int -> Int -> Gen S ()
randSelect lo hi i = do
  setRange lo hi
  if lo == hi
    then finish lo lo hi
    else do
      g <- gets sGen
      let (r, g') = randomR (lo, hi) g
      modify (\s -> s { sGen = g' })
      q <- partitionAround lo hi r
      narrow randSelect lo hi i q

-- 答えがある側だけ残す
narrow :: (Int -> Int -> Int -> Gen S ()) -> Int -> Int -> Int -> Int -> Gen S ()
narrow again lo hi i q
  | q == i = finish q q q
  | i < q = again lo (q - 1) i
  | otherwise = again (q + 1) hi i

-- 並べて、比較回数を返す（挿入ソート）
sortCounting :: [Int] -> ([Int], Int)
sortCounting = foldl' ins ([], 0)
  where
    ins (sorted, c) x =
      let (small, big) = span (<= x) sorted
          used = length small + (if null big then 0 else 1)
      in (small ++ x : big, c + used)

medianOf :: [Int] -> Int
medianOf grp = case drop ((length grp - 1) `div` 2) grp of
  (v : _) -> v
  [] -> 0

chunks5 :: [a] -> [[a]]
chunks5 [] = []
chunks5 xs = let (a, b) = splitAt 5 xs in a : chunks5 b

-- 中央値の中央値を（アニメなしで）求める：添字を返す
momIndex :: Int -> Int -> Gen S Int
momIndex lo hi = do
  a <- gets sA
  let slice = [a U.! k | k <- [lo .. hi]]
      medians xs =
        let sorted = map sortCounting (chunks5 xs)
        in (map (medianOf . fst) sorted, sum (map snd sorted))
      -- 再帰の代わりに、中央値の列にもう一度同じことをする
      reduce (vals, c)
        | length vals > 1 = let (m, c') = medians vals in reduce (m, c + c')
        | otherwise = (vals, c)
      (best, count) = reduce (medians slice)
      answer = case best of
        (v : _) -> case [k | k <- [lo .. hi], a U.! k == v] of
          (k : _) -> k
          [] -> lo
        [] -> lo
  modify (\s -> s { sComps = sComps s + count })
  pure answer

-- 中央値の中央値による選択
momSelect :: Int -> Int -> Int -> Gen S ()
momSelect lo hi i = do
  setRange lo hi
  if lo == hi
    then finish lo lo hi
    else do
      -- 5 個ずつの組を作って、組ごとに並べる
      modify (\s -> s { sGroups = [] })
      forM_ [lo, lo + 5 .. hi] $ \g -> do
        let e = min (g + 4) hi
        a <- gets sA
        let (grp, c) = sortCounting [a U.! k | k <- [g .. e]]
        modify (\s -> s
          { sA = sA s U.// zip [g ..] grp
          , sComps = sComps s + c
          , sGroups = sGroups s ++ [(g, e)] })
        yield
      modify (\s -> s { sMeds = [g + (e - g) `div` 2 | (g, e) <- sGroups s] })
      yield
      pIdx <- momIndex lo hi
      modify (\s -> s { sP = pIdx })
      yield
      modify (\s -> s { sGroups = [], sMeds = [] })
      q <- partitionAround lo hi pIdx
      narrow momSelect lo hi i q

advance :: World -> World
advance w = case wRest w of
  [] -> w
  (s : r) -> w { wS = s, wRest = r }

stepW :: Env -> Float -> World -> IO World
stepW _ dt w
  | wPlaying w && not (sDone (wS w)) = pure (drain w { wAcc = wAcc w + wSpeed w * dt })
  | otherwise = pure w

drain :: World -> World
drain w
  | wAcc w >= 1 = drain (advance w) { wAcc = wAcc w - 1 }
  | otherwise = w

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      a = sA s
      algo = wAlgo w
      len = U.length a
      l = 60
      wd = envW env - 120
      baseY = envH env - 130
      hH = envH env - 320
      bw = wd / fromIntegral (max 1 len)
      target = wTarget w
      group' (g, e) =
        box env (hsb (fromIntegral ((g * 13) `mod` 360)) 40 30) (l + fromIntegral g * bw) (baseY + 6)
          (fromIntegral (e - g + 1) * bw) 10
      bar k =
        let h = fromIntegral (a U.! k) * hH / 100
            inR = algo == 1 || (k >= sLo s && k <= sHi s)
            c | k == sAnswer s = hsb 140 80 100
              | k == sMx s = hsb 10 80 100
              | k == sMn s = hsb 200 80 100
              | k == sP s && algo /= 1 = hsb 330 80 100
              | k == sJ s = hsb 45 90 100
              | k `elem` sMeds s = hsb 190 70 95
              | inR = hsb 270 30 70
              | otherwise = hsb 0 0 22
        in box env c (l + fromIntegral k * bw + 1) (baseY - h) (bw - 2) h
      targetMark
        | algo /= 1 = txtC env (l + fromIntegral target * bw + bw / 2) (baseY + 30) 14 (hsb 140 80 100)
                        ("^ #" ++ show (target + 1))
        | otherwise = Blank
      name = case algo of
        1 -> "Min and max together"
        2 -> "RANDOMIZED-SELECT"
        _ -> "SELECT (median of medians)"
      res | sDone s && algo == 1 && len > 0 =
              "  min " ++ show (a U.! max 0 (sMn s)) ++ "  max " ++ show (a U.! max 0 (sMx s))
          | sDone s && algo /= 1 && sAnswer s >= 0 =
              "  #" ++ show (target + 1) ++ " = " ++ show (a U.! sAnswer s)
          | otherwise = ""
      help = hsb 0 0 65
      noteLine
        | algo == 1 = "Compare in pairs: about 3n/2 = " ++ show (3 * n `div` 2)
                        ++ " times (searching separately takes 2n - 2 = " ++ show (2 * n - 2) ++ ")"
        | otherwise = "Gray = no longer needed. Pink = pivot. Keep only the side with the answer"
                        ++ (if algo == 3 then ". Cyan = medians" else "")
  in Pictures
       [ Pictures (map group' (sGroups s))
       , Pictures (map bar [0 .. len - 1])
       , targetMark
       , txt env 20 40 22 (hsb 0 0 100) (name ++ "  compares " ++ show (sComps s) ++ res)
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   1-3 method   Left: smaller i   R: new numbers   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help noteLine
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    let m = max 1 (U.length (sA (wS w)))
    in restart w { wTarget = (wTarget w - 1 + m) `mod` m }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 240 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> restart w { wAlgo = digitToInt c }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n を 2 倍にして、2 と 3 の比較回数がほぼ 2 倍（= Θ(n)）になるか確かめる
-- 2. 3 の組の大きさを 5 から 3 にすると、最悪 Θ(n) が保証されなくなる。なぜか考えよう
-- 3. 中央値は「平均値」とどうちがう？ 大きな外れ値を 1 つ入れて比べよう

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)
txt, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 速さの表示（4 や 1.5）
num :: Float -> String
num v
  | v == fromIntegral (round v :: Int) = show (round v :: Int)
  | otherwise = show v
