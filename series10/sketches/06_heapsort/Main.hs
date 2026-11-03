-- ============================================
-- 06 ヒープとヒープソート
-- 配列を「上のほうが大きい」木（最大ヒープ）として見る。i の子は 2i+1 と 2i+2（0 始まり）
-- ① BUILD-MAX-HEAP：下の親から順に MAX-HEAPIFY（大きい子と入れかえて沈める）→ Θ(n)
-- ② 根（最大）を末尾と交換して、ヒープを 1 つ小さくして MAX-HEAPIFY、をくり返す → Θ(n log n)
-- スペース：再生／停止　→：1 手　R：新しい数列　1：ランダム　2：整列済み　3：逆順　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import qualified Data.Vector.Unboxed as U

n :: Int
n = 15              -- ★ 要素数（15 = 4 段のきれいな木）

data S = S
  { sA     :: !(U.Vector Int)
  , sSize  :: !Int       -- heapSize
  , sPhase :: !String
  , sHl    :: ![Int]     -- 比べている親と子
  , sSwap  :: ![Int]     -- 入れかえた 2 つ
  , sComps :: !Int
  , sSwaps :: !Int
  , sDone  :: !Bool
  }

data World = World
  { wS       :: !S
  , wRest    :: [S]
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  , wMode    :: !Int
  }

main :: IO ()
main = runSketch (sketch "10-06 Heapsort" initialWorld)
  { skBack  = hsb 20 30 10
  , skStart = \_ -> reset 1 initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: [Int] -> S
initialS xs = S (U.fromList xs) (length xs) "" [] [] 0 0 False

initialWorld :: World
initialWorld = World (initialS []) [] True 3 0 1     -- ★ 3 = 1 秒あたりの手数

reset :: Int -> World -> IO World
reset m w = do
  xs0 <- mapM (const (randomRIO (1, 99))) [1 .. n]
  let xs = case m of
        2 -> sort xs0
        3 -> sortBy (flip compare) xs0
        _ -> xs0
      s0 = initialS xs
  pure w { wMode = m, wS = s0, wRest = steps heapSort s0 }

maxHeapify :: Int -> Gen S ()
maxHeapify i = do
  s <- gets id
  let size = sSize s
      a = sA s
      l = 2 * i + 1
      r = 2 * i + 2
      v k = a U.! k
      afterL = if l < size && v l > v i then l else i
      largest = if r < size && v r > v afterL then r else afterL
      comps = length (filter (< size) [l, r])
  modify (\st -> st { sHl = filter (< size) [i, l, r], sComps = sComps st + comps })
  yield
  if largest == i
    then pure ()
    else do
      swapAt i largest
      modify (\st -> st { sSwap = [i, largest] })
      yield
      modify (\st -> st { sSwap = [] })
      maxHeapify largest

swapAt :: Int -> Int -> Gen S ()
swapAt i j = modify (\s ->
  let a = sA s
  in s { sA = a U.// [(i, a U.! j), (j, a U.! i)], sSwaps = sSwaps s + 1 })

heapSort :: Gen S ()
heapSort = do
  len <- gets (U.length . sA)
  modify (\s -> s { sPhase = "(1) BUILD-MAX-HEAP" })
  forM_ [len `div` 2 - 1, len `div` 2 - 2 .. 0] maxHeapify
  modify (\s -> s { sPhase = "(2) take the root out" })
  forM_ [len - 1, len - 2 .. 1] $ \end -> do
    swapAt 0 end
    modify (\s -> s { sSwap = [0, end], sHl = [] })
    yield
    modify (\s -> s { sSwap = [], sSize = sSize s - 1 })
    maxHeapify 0
  modify (\s -> s { sSize = 0, sHl = [], sPhase = "Done!", sDone = True })
  yield

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

-- i 番目のノードは木の何段目か（1, 2, 4, 8 … 個ずつ）
depthOf :: Int -> Int
depthOf i = length (takeWhile (<= i + 1) (iterate (* 2) 2))

nodePos :: Env -> Int -> (Float, Float)
nodePos env i =
  let depth = depthOf i
      cnt = 2 ^ depth :: Int
      idx = i - (cnt - 1)
      wd = envW env - 120
  in ( 60 + wd * (fromIntegral idx + 0.5) / fromIntegral cnt
     , 150 + fromIntegral depth * min 90 ((envH env * 0.55 - 150) / 4) )

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      a = sA s
      len = U.length a
      idxs = [0 .. len - 1]
      inHeap i = i < sSize s
      -- 木
      edge i =
        seg env (hsb 0 0 (if inHeap i then 45 else 18)) 2 (nodePos env ((i - 1) `div` 2)) (nodePos env i)
      r = min 44 ((envW env - 120) / 9)
      node i =
        let p = nodePos env i
            v = fromIntegral (a U.! i)
            c | i `elem` sSwap s = hsb 330 80 100
              | i `elem` sHl s = hsb 45 90 100
              | inHeap i = hsb (20 + v * 0.4) 70 (40 + v * 0.6)
              | otherwise = hsb 150 50 30
            tc = hsb 0 0 (if inHeap i then 10 else 70)
        in Pictures
             [ Color c (at (pt env p) (disc (r / 2)))
             , txtC env (fst p) (snd p) (max 14 (r * 0.38)) tc (show (a U.! i)) ]
      -- 配列
      bw = (envW env - 120) / fromIntegral (max 1 len)
      by = envH env - 110
      cellOf i =
        let c | i `elem` sSwap s = hsb 330 80 100
              | i `elem` sHl s = hsb 45 90 100
              | inHeap i = hsb 20 50 45
              | otherwise = hsb 150 60 70
            xc = 60 + fromIntegral i * bw + bw / 2
        in Pictures
             [ box env c (60 + fromIntegral i * bw + 2) by (bw - 4) 46
             , txtC env xc (by + 23) 16 (hsb 0 0 100) (show (a U.! i))
             , txtC env xc (by + 60) 14 (hsb 0 0 50) (show i) ]
      help = hsb 0 0 65
  in Pictures
       [ Pictures (map edge [1 .. len - 1])
       , Pictures (map node idxs)
       , Pictures (map cellOf idxs)
       , txt env 60 (by - 10) 14 (hsb 0 0 70) "Array (the left heapSize items are the heap, green on the right is finished)"
       , txt env 20 40 22 (hsb 0 0 100)
           ("Heapsort   " ++ sPhase s ++ "   compares " ++ show (sComps s) ++ "   swaps " ++ show (sSwaps s))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   R: redo   1 random   2 sorted   3 reversed   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help "Yellow = comparing parent and children   Pink = swap. Only fix where parent >= child is broken"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 120 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> reset (wMode w) w
    | c `elem` "123" -> reset (digitToInt c) w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ① の BUILD-MAX-HEAP の交換回数は n に比例する（n log n ではない）。n = 15, 31 で確かめよう
-- 2. 不等号を逆にすると「最小ヒープ」になり、降順に並ぶ
-- 3. 整列済みの入力でもヒープソートは速くならない。挿入ソートとの違いは？

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
