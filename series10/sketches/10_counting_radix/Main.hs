-- ============================================
-- 10 計数ソートと基数ソート
-- 比べずに並べる！ 値が 0〜k の整数なら「各値が何個あるか」を数えれば、置き場所が決まる（計数ソート）
-- Θ(n + k) なので、09 の下界 n log n より速い（比較をしないから下界が当てはまらない）
-- 基数ソート：3 けたの数を「1 の位 → 10 の位 → 100 の位」の順に、安定な計数ソートで並べる
-- スペース：再生／停止　→：1 手　1：計数ソート　2：基数ソート　R：新しい数列　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U

n, k :: Int
n = 16              -- ★ 計数ソートの個数
k = 9               -- ★ 計数ソートの最大値

data S = S
  { sA      :: !(U.Vector Int)
  , sC      :: !(U.Vector Int)        -- 数えた表（はじめは空）
  , sOut    :: !(U.Vector Int)        -- 出力（-1 = まだ空）
  , sBucket :: !(IM.IntMap [Int])     -- 基数ソートのバケツ（中身は入れた順）
  , sHlA    :: !Int
  , sHlC    :: !Int
  , sHlB    :: !Int
  , sDigit  :: !Int
  , sPhase  :: !String
  , sOps    :: !Int
  , sDone   :: !Bool
  }

data World = World
  { wS       :: !S
  , wRest    :: [S]
  , wAlgo    :: !Int
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-10 Counting and Radix Sort" initialWorld)
  { skBack  = hsb 120 25 9
  , skStart = \_ -> reset initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: [Int] -> S
initialS xs = S (U.fromList xs) U.empty (U.replicate n (-1)) IM.empty (-1) (-1) (-1) 0 "" 0 False

initialWorld :: World
initialWorld = World (initialS []) [] 1 True 4 0

reset :: World -> IO World
reset w = do
  xs <- mapM (const (if wAlgo w == 1 then randomRIO (0, k) else randomRIO (100, 999))) [1 .. n]
  let s0 = initialS xs
      program = if wAlgo w == 1 then countingSort else radixSort
  pure w { wS = s0, wRest = steps program s0 }

hl :: Int -> Int -> Int -> Gen S ()
hl a c b = modify (\s -> s { sHlA = a, sHlC = c, sHlB = b, sOps = sOps s + 1 })

-- 計数ソート：数える → 累積する → 後ろから置く
countingSort :: Gen S ()
countingSort = do
  xs <- gets sA
  let len = U.length xs
  modify (\s -> s { sC = U.replicate (k + 1) 0, sPhase = "(1) Count: C[A[j]] += 1" })
  forM_ [0 .. len - 1] $ \j -> do
    let v = xs U.! j
    addC v 1
    hl j v (-1)
    yield
  modify (\s -> s { sPhase = "(2) Sum up: C[i] = how many are <= i" })
  forM_ [1 .. k] $ \i -> do
    prev <- gets (\s -> sC s U.! (i - 1))
    addC i prev
    hl (-1) i (-1)
    yield
  modify (\s -> s { sPhase = "(3) From the right, place at C[A[j]] (stable)" })
  forM_ [len - 1, len - 2 .. 0] $ \j -> do
    let v = xs U.! j
    addC v (-1)
    pos <- gets (\s -> sC s U.! v)
    modify (\s -> s { sOut = sOut s U.// [(pos, v)] })
    hl j v pos
    yield
  modify (\s -> s { sPhase = "Done!", sDone = True, sHlA = -1, sHlC = -1, sHlB = -1 })
  yield

addC :: Int -> Int -> Gen S ()
addC i d = modify (\s -> let c = sC s in s { sC = c U.// [(i, c U.! i + d)] })

-- 基数ソート（各けたは 10 個のバケツで安定に分ける）
radixSort :: Gen S ()
radixSort = do
  forM_ [0, 1, 2] $ \digit -> do
    let pw = 10 ^ digit :: Int
        place = show pw
        emptyBuckets = IM.fromList [(d, []) | d <- [0 .. 9]]
    modify (\s -> s { sDigit = digit, sPhase = "Split by the " ++ place ++ "s digit", sBucket = emptyBuckets })
    xs <- gets sA
    forM_ [0 .. U.length xs - 1] $ \j -> do
      let v = xs U.! j
          d = (v `div` pw) `mod` 10
      modify (\s -> s { sBucket = IM.adjust (++ [v]) d (sBucket s) })
      hl j d (-1)
      yield
    modify (\s -> s { sPhase = "Collect by the " ++ place ++ "s digit (ties keep their order)" })
    modify (\s ->
      let out = concat (IM.elems (sBucket s))
      in s { sA = U.fromList out, sOps = sOps s + length out, sHlA = -1, sHlC = -1, sHlB = -1 })
    yield
    yield
    modify (\s -> s { sBucket = IM.empty })
  modify (\s -> s { sDigit = 3, sPhase = "Done!", sDone = True })
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

-- 1 マス（下に小さな見出しつき）
cell :: Env -> Float -> Float -> Float -> Float -> String -> Color -> String -> Picture
cell env x y w h v c lbl = Pictures
  [ box env c (x + 2) y (w - 4) h
  , txtC env (x + w / 2) (y + h / 2) (max 14 (min 18 (w * 0.4))) (hsb 0 0 100) v
  , if null lbl then Blank else txtC env (x + w / 2) (y + h + 11) 14 (hsb 0 0 50) lbl ]

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      l = 60
      wd = envW env - 120
      algo = wAlgo w
      a = sA s
      bw = wd / fromIntegral (max 1 (U.length a))
      head1 y str = txt env l y 15 (hsb 0 0 80) str
      counting = Pictures
        [ head1 150 "A (input)"
        , Pictures
            [ cell env (l + fromIntegral j * bw) 160 bw 44 (show v)
                (if j == sHlA s then hsb 45 90 90 else hsb (fromIntegral v * 36) 50 55) (show j)
            | (j, v) <- zip [0 ..] (U.toList a) ]
        , head1 270 "C (count table)"
        , let cw = wd / fromIntegral (k + 1)
          in Pictures
               [ cell env (l + fromIntegral i * cw) 280 cw 44
                   (if U.null (sC s) then "" else show (sC s U.! i))
                   (if i == sHlC s then hsb 330 70 90 else hsb (fromIntegral i * 36) 40 35)
                   ("val " ++ show i)
               | i <- [0 .. k] ]
        , head1 390 "B (output)"
        , Pictures
            [ cell env (l + fromIntegral j * bw) 400 bw 44 (if v < 0 then "" else show v)
                (if j == sHlB s then hsb 140 80 85
                 else if v < 0 then hsb 0 0 18
                 else hsb (fromIntegral v * 36) 50 55)
                (show j)
            | (j, v) <- zip [0 ..] (U.toList (sOut s)) ]
        , if sHlA s >= 0 && sHlB s >= 0
            then seg env (hsb 140 80 100) 2
                   (l + fromIntegral (sHlA s) * bw + bw / 2, 204)
                   (l + fromIntegral (sHlB s) * bw + bw / 2, 400)
            else Blank
        ]
      radix = Pictures
        [ head1 150 "A (current order)"
        , Pictures (map number (zip [0 ..] (U.toList a)))
        , Pictures (map bucket [0 .. 9])
        ]
      number (j, v) =
        let x = l + fromIntegral (j :: Int) * bw
            digits = show (v :: Int)
            dig (q, ch) = txtC env (x + bw / 2 + (fromIntegral q - 1) * 14) 182 16
              (if 2 - q == sDigit s && not (sDone s) then hsb 45 90 100 else hsb 0 0 95) [ch]
        in Pictures
             ( box env (if j == sHlA s then hsb 45 90 90 else hsb 200 30 35) (x + 2) 160 (bw - 4) 44
             : map dig (zip [0 :: Int ..] digits) )
      bucketW = wd / 10
      bucket d =
        let x = l + fromIntegral d * bucketW
            items = IM.findWithDefault [] d (sBucket s)
            item (i, v) = txtC env (x + bucketW / 2) (292 + fromIntegral (i :: Int) * 22) 15 (hsb 0 0 95) (show v)
        in Pictures
             ( box env (if d == sHlC s then hsb 330 60 50 else hsb 200 20 20) (x + 4) 250 (bucketW - 8) (envH env - 400)
             : txtC env (x + bucketW / 2) 264 16 (hsb 0 0 80) (show d)
             : map item (zip [0 ..] items) )
      help = hsb 0 0 65
  in Pictures
       [ if algo == 1 then counting else radix
       , txt env 20 40 20 (hsb 0 0 100)
           ((if algo == 1 then "Counting sort Theta(n+k)" else "Radix sort Theta(d(n+10))")
             ++ "  " ++ sPhase s ++ "  ops " ++ show (sOps s))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   1 counting   2 radix   R: new numbers   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help
           (if algo == 1
              then "Equal values keep their original order (stable), so it can be a building block of radix sort"
              else "Start from the lowest digit. Starting from the highest would break the earlier result")
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 120 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "12" -> reset w { wAlgo = digitToInt c }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. k を 99 にすると表 C が大きくなる。k が n よりずっと大きいと計数ソートは得か？
-- 2. ③ を「左から」にすると安定でなくなる。同じ値に色をつけて確かめよう
-- 3. 基数ソートを上の位（100 の位）から始めると、どうなる？

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
