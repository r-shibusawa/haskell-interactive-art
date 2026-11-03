-- ============================================
-- 13 ソートを聞く（自由制作のベース）
-- 比べた・動かした数を「音の高さ」にすると、アルゴリズムごとにちがう音楽になる
-- 挿入ソートはなめらかに上がり、マージソートは小さな山がつながり、ヒープソートは上下に跳ねる
-- 6 番の mySort に自分のソートを書いて、見た目と音を作ろう
-- クリック：再生　1〜6：ソートを選ぶ　スペース：再生／停止　R：混ぜる
-- C：棒 ↔ 円の表示　S：音階（なめらか ↔ ペンタトニック）　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (foldM, forM_, unless, when)
import Data.Bits (shiftL, testBit)
import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as U

n :: Int
n = 64              -- ★ 要素数

startSpeed :: Int
startSpeed = 12     -- ★ 1 フレームあたりの手数（速いと音が重なる）

noteWave :: Wave
noteWave = Triangle -- ★ 音色（Square や Sawtooth に）

-- アルゴリズムの状態：配列と、いま見ている・動かした添字
data S = S { sA :: !(U.Vector Int), sMark :: ![Int] }

data World = World
  { wS       :: !S
  , wRest    :: [S]
  , wAlgo    :: !Int
  , wPlaying :: !Bool
  , wDone    :: !Bool
  , wCircle  :: !Bool
  , wPenta   :: !Bool
  , wSpeed   :: !Int
  , wCounter :: !Int
  }

main :: IO ()
main = runSketch (sketch "10-13 Listening to Sorts" initialWorld)
  { skBack  = hsb 250 30 8
  , skStart = \_ -> shuffleAll initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World (S U.empty []) [] 1 False False False True startSpeed 0

shuffleAll :: World -> IO World
shuffleAll w = do
  xs <- shuffle [1 .. n]
  pure (restart w { wS = S (U.fromList xs) [] })

restart :: World -> World
restart w =
  let s0 = (wS w) { sMark = [] }
      a = U.length (sA s0)
      program = case wAlgo w of
        2 -> mergeSort 0 (a - 1)
        3 -> heapSort
        4 -> quick 0 (a - 1)
        5 -> radix
        6 -> mySort
        _ -> insertion
  in w { wS = s0, wRest = steps program s0, wDone = False, wCounter = 0 }

-- どのソートも、見る・動かすたびに emit [添字…] で知らせる → 音と光になる
emit :: [Int] -> Gen S ()
emit m = modify (\s -> s { sMark = m }) >> yield

rd :: Int -> Gen S Int
rd i = gets (\s -> sA s U.! i)

wr :: Int -> Int -> Gen S ()
wr i v = modify (\s -> s { sA = sA s U.// [(i, v)] })

swapAt :: Int -> Int -> Gen S ()
swapAt i j = modify (\s ->
  let a = sA s
  in s { sA = a U.// [(i, a U.! j), (j, a U.! i)] })

lenA :: Gen S Int
lenA = gets (U.length . sA)

insertion :: Gen S ()
insertion = do
  len <- lenA
  let sink i = when (i > 0) $ do
        x <- rd (i - 1)
        y <- rd i
        when (x > y) $ do
          swapAt (i - 1) i
          emit [i - 1]
          sink (i - 1)
  forM_ [1 .. len - 1] sink

mergeSort :: Int -> Int -> Gen S ()
mergeSort lo hi = unless (lo >= hi) $ do
  let mid = (lo + hi) `div` 2
  mergeSort lo mid
  mergeSort (mid + 1) hi
  a <- gets sA
  let go k ls rs = case (ls, rs) of
        (x : xs, y : ys)
          | x <= y -> wr k x >> emit [k] >> go (k + 1) xs rs
          | otherwise -> wr k y >> emit [k] >> go (k + 1) ls ys
        (x : xs, []) -> wr k x >> emit [k] >> go (k + 1) xs []
        ([], y : ys) -> wr k y >> emit [k] >> go (k + 1) [] ys
        ([], []) -> pure ()
  go lo [a U.! i | i <- [lo .. mid]] [a U.! i | i <- [mid + 1 .. hi]]

heapSort :: Gen S ()
heapSort = do
  len <- lenA
  let sift size i = do
        a <- gets sA
        let l = 2 * i + 1
            r = l + 1
            m1 = if l < size && a U.! l > a U.! i then l else i
            m = if r < size && a U.! r > a U.! m1 then r else m1
        unless (m == i) $ do
          swapAt i m
          emit [m]
          sift size m
  forM_ [len `div` 2 - 1, len `div` 2 - 2 .. 0] (sift len)
  forM_ [len - 1, len - 2 .. 1] $ \size -> do
    swapAt 0 size
    emit [size]
    sift size 0

quick :: Int -> Int -> Gen S ()
quick lo hi = unless (lo >= hi) $ do
  x <- rd hi
  let step i j = do
        v <- rd j
        if v <= x
          then do
            swapAt (i + 1) j
            emit [i + 1, j]
            pure (i + 1)
          else pure i
  i <- foldM step (lo - 1) [lo .. hi - 1]
  swapAt (i + 1) hi
  emit [i + 1]
  quick lo i
  quick (i + 2) hi

radix :: Gen S ()
radix = forM_ (takeWhile (\b -> shiftL 1 b <= n) [0 ..]) $ \bit -> do
  xs <- gets (U.toList . sA)
  let zeros = filter (\v -> not (testBit v bit)) xs
      ones = filter (`testBit` bit) xs
  forM_ (zip [0 ..] (zeros ++ ones)) $ \(k, v) -> do
    wr k v
    emit [k]

-- ★ ここに自分のソートを書いてみよう（いまは「バブルソート」：となり同士を比べて入れかえる）
mySort :: Gen S ()
mySort = do
  len <- lenA
  forM_ [len - 1, len - 2 .. 1] $ \end ->
    forM_ [0 .. end - 1] $ \i -> do
      x <- rd i
      y <- rd (i + 1)
      when (x > y) $ do
        swapAt i (i + 1)
        emit [i + 1]

-- 数を音の高さに
freqOf :: Bool -> Int -> Float
freqOf penta v
  | not penta = 180 + fromIntegral v / fromIntegral n * 900
  | otherwise =
      let step = floor (fromIntegral v / fromIntegral n * 20 :: Float) :: Int
          oct = step `div` 5
          semis = pentatonic (step `mod` 5)
      in 220 * 2 ** (fromIntegral (oct * 12 + semis) / 12)

pentatonic :: Int -> Int
pentatonic k = case k of
  0 -> 0
  1 -> 2
  2 -> 4
  3 -> 7
  _ -> 9

-- 1 フレーム分（speed 手）進める。最初に音を出せる手の数を note で返す
runFrame :: Int -> World -> (World, Maybe Int, Bool)
runFrame 0 w = (w, Nothing, False)
runFrame k w = case wRest w of
  [] -> (w { wDone = True, wPlaying = False }, Nothing, True)
  (s : r) ->
    let w1 = w { wS = s, wRest = r, wCounter = wCounter w + 1 }
        here = case sMark s of
          (i : _) -> Just (sA s U.! i)
          [] -> Nothing
        (w2, later, fin) = runFrame (k - 1) w1
        note = case here of
          Nothing -> later
          _ -> here
    in (w2, note, fin)

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wPlaying w && not (wDone w) = do
      let (w', note, finished) = runFrame (wSpeed w) w
      forM_ note (\v -> playTone (freqOf (wPenta w) v) 0.12 0.09 noteWave)
      when finished (finale w')
      pure w'
  | otherwise = pure w

-- 完成したら下から上へ駆け上がる（同時に鳴らせる音に限りがあるので、4 個おきに少しゆっくり）
finale :: World -> IO ()
finale w = do
  let vs = U.toList (sA (wS w))
      picked = [v | (k, v) <- zip [0 :: Int ..] vs, k `mod` 4 == 0]
  _ <- forkIO (forM_ picked (\v -> do
         playTone (freqOf (wPenta w) v) 0.08 0.12 Sine
         threadDelay 40000))
  pure ()

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      a = sA s
      len = U.length a
      marked k = k `elem` sMark s
      frac k = fromIntegral (a U.! k) / fromIntegral n :: Float
      circleBars =
        let r0 = min (envW env) (envH env) * 0.36
            cx = envW env / 2
            cy = envH env / 2 + 40
            spoke k =
              let ang = tau * fromIntegral k / fromIntegral len - pi / 2
                  r = r0 * (0.25 + 0.75 * frac k)
              in seg env (hsb (frac k * 330) 70 (if marked k then 100 else 85)) (if marked k then 6 else 3)
                   (cx, cy) (cx + cos ang * r, cy + sin ang * r)
        in map spoke [0 .. len - 1]
      bars =
        let bw = (envW env - 80) / fromIntegral (max 1 len)
            hH = envH env - 200
            bar k =
              let h = frac k * hH
              in box env (hsb (frac k * 330) (if marked k then 20 else 70) (if marked k then 100 else 90))
                   (40 + fromIntegral k * bw) (envH env - 40 - h) (bw - 1) h
        in map bar [0 .. len - 1]
      name = case wAlgo w of
        2 -> "Merge sort"
        3 -> "Heapsort"
        4 -> "Quicksort"
        5 -> "Radix sort (binary)"
        6 -> "mySort (write your own)"
        _ -> "Insertion sort"
  in Pictures
       [ Pictures (if wCircle w then circleBars else bars)
       , txt env 20 40 22 (hsb 0 0 100)
           ("Listening to sorts   " ++ name ++ "   " ++ show (wCounter w) ++ " steps"
             ++ (if wDone w then "   Done!" else ""))
       , txt env 20 68 15 (hsb 0 0 65)
           ("Click: play   1-6 sort   Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   R: shuffle   C: view (" ++ (if wCircle w then "circle" else "bars")
             ++ ")   S: scale (" ++ (if wPenta w then "penta" else "smooth")
             ++ ")   Up/Down speed " ++ show (wSpeed w))
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | not (wPlaying w) && not (wDone w) -> pure w { wPlaying = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    w' <- if wDone w then shuffleAll w else pure w
    pure w' { wPlaying = not (wPlaying w') }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 200 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w `div` 2) }
  EventKey (Char c) Down _ _
    | c `elem` "123456" -> do
        w' <- shuffleAll w { wAlgo = digitToInt c }
        pure w' { wPlaying = True }
    | c `elem` "rR" -> shuffleAll w
    | c `elem` "cC" -> pure w { wCircle = not (wCircle w) }
    | c `elem` "sS" -> pure w { wPenta = not (wPenta w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. mySort を「選択ソート」（残りの最小を探して先頭と交換）に書きかえる
-- 2. 音色 noteWave を Square や Sawtooth に。比べたときにも音を出すには？
-- 3. 円の表示で、色が虹のように並んだら完成。自分だけの「完成の演出」を作ろう

-- ---------- 道具：リストを混ぜる ----------
shuffle :: [Int] -> IO [Int]
shuffle xs = do
  let v0 = U.fromList xs
  v <- foldM (\v i -> do
                j <- randomRIO (0, i)
                pure (v U.// [(i, v U.! j), (j, v U.! i)]))
             v0 [U.length v0 - 1, U.length v0 - 2 .. 1]
  pure (U.toList v)

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
