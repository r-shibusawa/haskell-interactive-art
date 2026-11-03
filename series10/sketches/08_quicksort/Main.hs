-- ============================================
-- 08 クイックソート
-- 基準（ピボット）を 1 つ選び、それより小さいものを左、大きいものを右に分ける（PARTITION）
-- 左右をそれぞれ同じように並べれば完成。平均 Θ(n log n)、しかも実際にとても速い
-- でも、整列済みの入力で毎回端をピボットにすると Θ(n^2)。ランダムに選べば防げる
-- スペース：再生／停止　→：1 手　1：ロムート分割　2：ホーア分割　3：ランダム化
-- 4：ランダム入力　5：整列済み入力　6：同じ値だらけ　R：やり直し　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import qualified Data.IntSet as IS
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

n :: Int
n = 40              -- ★ 要素数

data S = S
  { sA     :: !(U.Vector Int)
  , sGen   :: !StdGen            -- ランダム化のピボット選びに使う乱数
  , sLo    :: !Int
  , sHi    :: !Int
  , sP     :: !Int               -- ピボット
  , sI     :: !Int
  , sJ     :: !Int
  , sPv    :: !Int               -- ホーア分割のピボットの値（-1 = なし）
  , sFixed :: !IS.IntSet         -- 位置が決まった場所
  , sStack :: ![(Int, Int)]      -- 再帰の途中の区間（深いものが先頭）
  , sComps :: !Int
  , sSwaps :: !Int
  , sDepth :: !Int
  , sDone  :: !Bool
  }

data World = World
  { wS       :: !S
  , wRest    :: [S]
  , wMethod  :: !Int
  , wInput   :: !Int
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-08 Quicksort" initialWorld)
  { skBack  = hsb 340 30 10
  , skStart = \_ -> reset initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: StdGen -> [Int] -> S
initialS g xs = S (U.fromList xs) g (-1) (-1) (-1) (-1) (-1) (-1) IS.empty [] 0 0 0 False

initialWorld :: World
initialWorld = World (initialS (mkStdGen 1) []) [] 1 4 True 8 0

reset :: World -> IO World
reset w = do
  g <- newStdGen
  xs0 <- mapM (const (if wInput w == 6
                        then (50 +) <$> randomRIO (0, 2)
                        else randomRIO (5, 99))) [1 .. n]
  let xs = if wInput w == 5 then sort xs0 else xs0
      s0 = initialS g xs
      program = do
        quick (wMethod w) 0 (U.length (sA s0) - 1) 0
        modify (\s -> s { sDone = True, sLo = -1, sHi = -1, sP = -1, sI = -1, sJ = -1 })
        yield
  pure w { wS = s0, wRest = steps program s0 }

swapAt :: Int -> Int -> Gen S ()
swapAt i j
  | i == j = pure ()
  | otherwise = modify (\s ->
      let a = sA s
      in s { sA = a U.// [(i, a U.! j), (j, a U.! i)], sSwaps = sSwaps s + 1 })

rd :: Int -> Gen S Int
rd i = gets (\s -> sA s U.! i)

-- lo 以上 hi 以下の乱数
randomIn :: Int -> Int -> Gen S Int
randomIn lo hi = do
  g <- gets sGen
  let (v, g') = randomR (lo, hi) g
  modify (\s -> s { sGen = g' })
  pure v

-- ロムートの分割：最後の要素をピボットに、小さいものを左へ集める
lomuto :: Int -> Int -> Int -> Gen S Int
lomuto method lo hi = do
  if method == 3
    then randomIn lo hi >>= \r -> swapAt r hi    -- ランダムに選んだピボットを末尾へ
    else pure ()
  x <- rd hi
  let go i j
        | j >= hi = pure i
        | otherwise = do
            modify (\s -> s { sComps = sComps s + 1, sLo = lo, sHi = hi, sP = hi, sI = i, sJ = j, sPv = -1 })
            yield
            v <- rd j
            if v <= x
              then swapAt (i + 1) j >> go (i + 1) (j + 1)
              else go i (j + 1)
  i <- go (lo - 1) lo
  swapAt (i + 1) hi
  modify (\s -> s { sLo = lo, sHi = hi, sP = i + 1, sI = -1, sJ = -1, sPv = -1 })
  yield
  pure (i + 1)

-- ホーアの分割：両端から近づいて、まちがった側にあるもの同士を交換
hoare :: Int -> Int -> Gen S Int
hoare lo hi = do
  x <- rd lo
  let look i j = do
        modify (\s -> s { sComps = sComps s + 1, sLo = lo, sHi = hi, sP = -1, sI = i, sJ = j, sPv = x })
        yield
      downJ i j = do
        look i (j - 1)
        v <- rd (j - 1)
        if v > x then downJ i (j - 1) else pure (j - 1)
      upI i j = do
        look (i + 1) j
        v <- rd (i + 1)
        if v < x then upI (i + 1) j else pure (i + 1)
      loop i j = do
        j' <- downJ i j
        i' <- upI i j'
        if i' < j'
          then swapAt i' j' >> loop i' j'
          else pure j'
  loop (lo - 1) (hi + 1)

quick :: Int -> Int -> Int -> Int -> Gen S ()
quick method lo hi depth = do
  modify (\s -> s { sDepth = max (sDepth s) depth })
  case compare lo hi of
    GT -> pure ()
    EQ -> modify (\s -> s { sFixed = IS.insert lo (sFixed s) })
    LT -> do
      modify (\s -> s { sStack = (lo, hi) : sStack s })
      if method == 2
        then do
          q <- hoare lo hi
          quick method lo q (depth + 1)
          quick method (q + 1) hi (depth + 1)
        else do
          q <- lomuto method lo hi
          modify (\s -> s { sFixed = IS.insert q (sFixed s) })   -- ピボットはもう最終位置
          quick method lo (q - 1) (depth + 1)
          quick method (q + 1) hi (depth + 1)
      modify (\s -> s { sStack = drop 1 (sStack s) })

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
      len = U.length a
      method = wMethod w
      done = sDone s
      bw = (envW env - 120) / fromIntegral (max 1 len)
      baseY = envH env - 150
      hH = envH env - 330
      xOf k = 60 + fromIntegral k * bw
      inRange k = k >= sLo s && k <= sHi s
      rangeBack
        | sLo s >= 0 = box env (hsba 0 0 100 0.06) (xOf (sLo s)) (baseY - hH - 10)
                         (fromIntegral (sHi s - sLo s + 1) * bw) (hH + 10)
        | otherwise = Blank
      bar k =
        let h = fromIntegral (a U.! k) * hH / 100
            c | not done && k == sP s = hsb 330 80 100
              | not done && method == 2 && k == sI s = hsb 45 90 100
              | not done && k == sJ s = hsb 45 90 100
              | done || IS.member k (sFixed s) = hsb 150 60 80
              | inRange k =
                  if method /= 2 && sI s >= 0 && k <= sI s then hsb 200 60 85        -- ピボット以下
                  else if method /= 2 && sJ s >= 0 && k > sI s && k < sJ s then hsb 20 60 85  -- ピボットより大
                  else hsb 0 0 60
              | otherwise = hsb 0 0 30
        in box env c (xOf k + 1) (baseY - h) (bw - 2) h
      pivotLine
        | method == 2 && sPv s >= 0 && not done && sLo s >= 0 =
            let y = baseY - fromIntegral (sPv s) * hH / 100
            in seg env (hsb 330 80 100) 1.5 (xOf (sLo s), y) (xOf (sHi s + 1), y)
        | otherwise = Blank
      -- 再帰の深さ（下にはみ出さないよう、深いときは間隔をつめる）
      stack = reverse (sStack s)
      dy = min 5 (105 / fromIntegral (max 1 (length stack)))
      stackBar (d, (lo, hi)) =
        box env (hsb (200 + fromIntegral d * 8) 50 80) (xOf lo) (baseY + 38 + fromIntegral (d :: Int) * dy)
          (fromIntegral (hi - lo + 1) * bw) (max 2 (dy - 1))
      log2n = logBase 2 (fromIntegral (max 2 n)) :: Float
      name = case method of
        2 -> "Hoare (first as pivot)"
        3 -> "Random pivot"
        _ -> "Lomuto (last as pivot)"
      input = case wInput w of
        5 -> "sorted"
        6 -> "many equal"
        _ -> "random"
      help = hsb 0 0 65
  in Pictures
       [ rangeBack
       , Pictures (map bar [0 .. len - 1])
       , pivotLine
       , txt env 60 (baseY + 30) 14 (hsb 0 0 70)
           ("Recursion depth (max " ++ show (sDepth s) ++ ", log2 n = " ++ show (round log2n :: Int) ++ ")")
       , Pictures (map stackBar (zip [0 ..] stack))
       , txt env 20 40 22 (hsb 0 0 100)
           ("Quicksort  " ++ name ++ "  input: " ++ input ++ "  cmp " ++ show (sComps s)
             ++ "  swap " ++ show (sSwaps s) ++ (if done then "  Done!" else ""))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   1-3 partition   4-6 input   R: redo   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help "Pink = pivot   Blue = at most pivot   Orange = above pivot   Yellow = looking   Green = placed"
       , txt env 20 116 15 help
           ("Compare: n log2 n = " ++ show (round (fromIntegral n * log2n) :: Int) ++ "   n^2/2 = " ++ show (n * n `div` 2))
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 480 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> reset w { wMethod = digitToInt c }
    | c `elem` "456" -> reset w { wInput = digitToInt c }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 整列済み（5）で、1（ロムート）と 3（ランダム化）の比較回数・再帰の深さを比べる
-- 2. 同じ値だらけ（6）だとロムートは遅くなる。ホーア（2）はどう？
-- 3. ピボットを「3 つの中央値」（最初・真ん中・最後）にしてみよう

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
