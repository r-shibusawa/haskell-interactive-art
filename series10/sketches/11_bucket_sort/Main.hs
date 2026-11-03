-- ============================================
-- 11 バケットソート
-- 0 以上 1 未満の数を、n 個のバケツ [0, 0.05), [0.05, 0.1), … に振り分け、
-- バケツの中だけ挿入ソートして、左から順につなげる
-- 数がばらけて（一様に）いれば、各バケツは平均 1 個ちょっと → 全体で平均 Θ(n)
-- 偏っていると 1 つのバケツに集中して Θ(n^2) に。入力の分布で速さが変わる
-- スペース：再生／停止　→：1 手　1：一様な入力　2：偏った入力（正規分布）　3：最悪（全部同じバケツ）
-- R：やり直し　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, unless)
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

n :: Int
n = 20              -- ★ 個数（= バケツの数）

data S = S
  { sB     :: !(IM.IntMap (U.Vector Float))   -- バケツ
  , sOut   :: ![Float]                        -- 出力（新しいものが先頭）
  , sHlA   :: !Int
  , sHlB   :: !Int
  , sHlI   :: !Int
  , sPhase :: !String
  , sOps   :: !Int
  , sDone  :: !Bool
  }

data World = World
  { wA       :: !(U.Vector Float)
  , wS       :: !S
  , wRest    :: [S]
  , wInput   :: !Int
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-11 Bucket Sort" initialWorld)
  { skBack  = hsb 40 25 9
  , skStart = \_ -> reset initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: S
initialS = S (IM.fromList [(b, U.empty) | b <- [0 .. n - 1]]) [] (-1) (-1) (-1) "" 0 False

initialWorld :: World
initialWorld = World U.empty initialS [] 1 True 6 0

-- p5.js の randomGaussian（ボックス＝ミュラー法）
gaussian :: Float -> Float -> IO Float
gaussian mean sd = do
  u1 <- randomRIO (1e-6, 1)
  u2 <- randomRIO (0, 1)
  pure (mean + sd * sqrt (-2 * log u1) * cos (tau * u2))

reset :: World -> IO World
reset w = do
  xs <- mapM (const one) [1 .. n]
  let a = U.fromList xs
  pure w { wA = a, wS = initialS, wRest = steps (bucketSort a) initialS }
  where
    one = do
      v <- case wInput w of
        2 -> clampF 0 0.999 <$> gaussian 0.5 0.08
        3 -> randomRIO (0.3, 0.349)
        _ -> randomRIO (0, 0.9999)
      pure (fromIntegral (floor (v * 1000) :: Int) / 1000)

setHl :: Int -> Int -> Int -> Gen S ()
setHl a b i = modify (\s -> s { sHlA = a, sHlB = b, sHlI = i })

addOp :: Gen S ()
addOp = modify (\s -> s { sOps = sOps s + 1 })

-- バケットソート：振り分け → バケツごとに挿入ソート → つなげる
bucketSort :: U.Vector Float -> Gen S ()
bucketSort a = do
  modify (\s -> s { sPhase = "(1) Split: A[i] into bucket floor(n A[i])" })
  forM_ [0 .. U.length a - 1] $ \i -> do
    let v = a U.! i
        b = min (n - 1) (floor (fromIntegral n * v))
    modify (\s -> s { sB = IM.adjust (`U.snoc` v) b (sB s) })
    addOp
    setHl i b (-1)
    yield
  modify (\s -> s { sPhase = "(2) Insertion sort inside each bucket" })
  forM_ [0 .. n - 1] $ \b -> do
    len <- gets (U.length . bucket b)
    forM_ [1 .. len - 1] $ \j -> do
      key <- gets (\s -> bucket b s U.! j)
      i <- slide b key (j - 1)
      wrB b (i + 1) key
    unless (len == 0) (setHl (-1) b (-1) >> yield)
  modify (\s -> s { sPhase = "(3) Join the buckets from the left" })
  forM_ [0 .. n - 1] $ \b -> do
    vs <- gets (U.toList . bucket b)
    forM_ vs $ \v -> do
      modify (\s -> s { sOut = v : sOut s })
      addOp
      setHl (-1) b (-1)
      yield
  modify (\s -> s { sPhase = "Done!", sDone = True, sHlA = -1, sHlB = -1, sHlI = -1 })
  yield

-- バケツ b の中で key より大きいものを右へずらす
slide :: Int -> Float -> Int -> Gen S Int
slide b key i
  | i < 0 = pure i
  | otherwise = do
      addOp
      setHl (-1) b i
      yield
      v <- gets (\s -> bucket b s U.! i)
      if v <= key
        then pure i
        else wrB b (i + 1) v >> slide b key (i - 1)

bucket :: Int -> S -> U.Vector Float
bucket b s = IM.findWithDefault U.empty b (sB s)

wrB :: Int -> Int -> Float -> Gen S ()
wrB b i v = modify (\s -> s { sB = IM.adjust (U.// [(i, v)]) b (sB s) })

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

-- 0.123 を ".123" と書く
short :: Float -> String
short v = drop 1 (showFFloat (Just 3) v "")

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      a = wA w
      l = 60
      wd = envW env - 120
      hg = envH env
      -- 入力を数直線の上に
      dot' (i, v) =
        let c = if i == sHlA s then hsb 45 90 100 else hsb (v * 300) 60 90
        in Color c (at (pt env (l + v * wd, 160)) (disc (if i == sHlA s then 8 else 5)))
      tick q = txtC env (l + fromIntegral (q :: Int) / 10 * wd) 182 14 (hsb 0 0 60)
                 (showFFloat (Just 1) (fromIntegral q / 10 :: Float) "")
      -- バケツ
      bw = wd / fromIntegral n
      top = 220
      bh = hg - top - 170
      maxLen = foldr (max . U.length) 4 (IM.elems (sB s))
      ch = min 28 ((bh - 10) / fromIntegral maxLen)
      drawBucket b =
        let x = l + fromIntegral b * bw
            vs = U.toList (bucket b s)
            hot = b == sHlB s
            item (i, v) =
              let y = top + bh - fromIntegral (i + 1) * ch
                  c = if hot && (i == sHlI s || i == sHlI s + 1) then hsb 45 90 100 else hsb (v * 300) 55 80
              in Pictures
                   [ box env c (x + 5) y (bw - 10) (ch - 2)
                   , if ch > 15 && bw > 36
                       then txtC env (x + bw / 2) (y + ch / 2) 14 (hsb 0 0 10) (short v)
                       else Blank ]
        in Pictures
             ( box env (if hot then hsb 330 50 40 else hsb 40 15 18) (x + 2) top (bw - 4) bh
             : txtC env (x + bw / 2) (top + bh + 12) 14 (hsb 0 0 50) (show b)
             : map item (zip [0 :: Int ..] vs) )
      -- 出力
      outCell (i, v) =
        let x = l + fromIntegral (i :: Int) * bw
        in Pictures
             [ box env (hsb (v * 300) 55 80) (x + 2) (hg - 120) (bw - 4) 32
             , if bw > 34 then txtC env (x + bw / 2) (hg - 104) 14 (hsb 0 0 10) (short v) else Blank ]
      input = case wInput w of
        2 -> "skewed"
        3 -> "worst"
        _ -> "uniform"
      help = hsb 0 0 65
  in Pictures
       [ seg env (hsb 0 0 40) 1 (l, 160) (l + wd, 160)
       , Pictures (map dot' (zip [0 ..] (U.toList a)))
       , Pictures (map tick [0 .. 10])
       , Pictures (map drawBucket [0 .. n - 1])
       , Pictures (map outCell (zip [0 ..] (reverse (sOut s))))
       , txt env l (hg - 128) 14 (hsb 0 0 60) "Output"
       , txt env 20 40 20 (hsb 0 0 100)
           ("Bucket sort  input: " ++ input ++ "  " ++ sPhase s ++ "  ops " ++ show (sOps s))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   1 uniform   2 skewed   3 worst   R: redo   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help
           ("Compare the op counts for 1, 2, 3. With n = " ++ show n ++ ", n is " ++ show n ++ " and n^2 is " ++ show (n * n))
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 240 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> reset w { wInput = digitToInt c }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 偏った入力（2）で、バケツの幅を「真ん中ほど細く」すると速くなる。どう変える？
-- 2. バケツの数を n / 4 にすると、操作回数はどう変わる？
-- 3. 点数（0〜100 点）のテスト結果を並べるなら、どのソートを使う？

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
