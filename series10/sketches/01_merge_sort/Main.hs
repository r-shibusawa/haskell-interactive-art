-- ============================================
-- 01 マージソート
-- 半分に分けて、それぞれ並べて、2 つの並んだ山を「小さいほうから取る」で合わせる（分割統治）
-- 下の段ほど細かく分かれる。段の数は log2 n、各段の仕事は n なので全体は n log n
-- スペース：再生／一時停止　→：1 手　R：新しい数列　1：ランダム　2：逆順　3：ほぼ整列　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import qualified Data.Vector.Unboxed as U

n :: Int
n = 32            -- ★ 並べる数（2 のべき乗だと木がきれい）

-- 処理中の区間（再帰の深さつき）
data Range = Range { rLo :: !Int, rHi :: !Int, rDepth :: !Int, rMerged :: !Bool }

data S = S
  { sA      :: !(U.Vector Int)
  , sLo     :: !Int
  , sHi     :: !Int
  , sI      :: !Int
  , sJ      :: !Int
  , sK      :: !Int
  , sRanges :: ![Range]
  , sComps  :: !Int
  , sWrites :: !Int
  , sDone   :: !Bool
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
main = runSketch (sketch "10-01 Merge Sort" initialWorld)
  { skBack  = hsb 250 30 10
  , skStart = \_ -> reset 1 initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: [Int] -> S
initialS xs = S (U.fromList xs) (-1) (-1) (-1) (-1) (-1) [] 0 0 False

initialWorld :: World
initialWorld = World (initialS []) [] True 6 0 1     -- ★ 6 = 1 秒あたりの手数

reset :: Int -> World -> IO World
reset m w = do
  xs0 <- mapM (const (randomRIO (5, 99))) [1 .. n]
  let xs = case m of
        2 -> sortBy (flip compare) xs0
        3 -> let v = U.fromList (sort xs0)
             in U.toList (v U.// [(3, v U.! (n - 4)), (n - 4, v U.! 3)])
        _ -> xs0
      s0 = initialS xs
      program = do
        mergeSort 0 (n - 1) 0
        modify (\s -> s { sDone = True })
        yield
  pure w { wMode = m, wS = s0, wRest = steps program s0 }

mergeSort :: Int -> Int -> Int -> Gen S ()
mergeSort lo hi depth
  | lo >= hi = pure ()
  | otherwise = do
      let mid = (lo + hi) `div` 2
      modify (\s -> s { sRanges = Range lo hi depth False : sRanges s
                      , sLo = lo, sHi = hi, sI = -1, sJ = -1, sK = -1 })
      yield
      mergeSort lo mid (depth + 1)
      mergeSort (mid + 1) hi (depth + 1)
      merge lo mid hi
      modify (\s -> s { sRanges = map (markMerged lo hi) (sRanges s) })

markMerged :: Int -> Int -> Range -> Range
markMerged lo hi r
  | rLo r == lo && rHi r == hi = r { rMerged = True }
  | otherwise = r

-- 合わせる処理。左の山 L と右の山 R の先頭を比べて小さいほうを戻す
merge :: Int -> Int -> Int -> Gen S ()
merge lo mid hi = do
  a <- gets sA
  let slice from to = [a U.! k | k <- [from .. to]]
      go i j k ls rs = case (ls, rs) of
        (l : ls', r : rs') -> do
          modify (\s -> s { sComps = sComps s + 1, sLo = lo, sHi = hi
                          , sI = lo + i, sJ = mid + 1 + j, sK = k })
          yield
          if l <= r
            then wr k l >> go (i + 1) j (k + 1) ls' rs
            else wr k r >> go i (j + 1) (k + 1) ls rs'
        (l : ls', []) -> wr k l >> setK (k + 1) >> yield >> go (i + 1) j (k + 1) ls' []
        ([], r : rs') -> wr k r >> setK (k + 1) >> yield >> go i (j + 1) (k + 1) [] rs'
        ([], []) -> pure ()
  go (0 :: Int) (0 :: Int) lo (slice lo mid) (slice (mid + 1) hi)

setK :: Int -> Gen S ()
setK k = modify (\s -> s { sK = k })

wr :: Int -> Int -> Gen S ()
wr i v = modify (\s -> s { sA = sA s U.// [(i, v)], sWrites = sWrites s + 1 })

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
      bw = (envW env - 120) / fromIntegral n
      top = 130
      barH = envH env * 0.38
      baseY = top + barH
      xOf k = 60 + fromIntegral k * bw
      bar k =
        let h = fromIntegral (a U.! k) * barH / 100
            inRange = k >= sLo s && k <= sHi s
            c | sDone s = hsb 150 60 85
              | k == sK s - 1 = hsb 330 70 100
              | k == sI s || k == sJ s = hsb 45 90 100
              | inRange = hsb 200 50 80
              | otherwise = hsb 220 15 45
        in box env c (xOf k + 1) (baseY - h) (bw - 2) h
      -- 分割の木（区間を深さごとに帯で描く）
      levels = ceiling (logBase 2 (fromIntegral n) :: Float) + 1 :: Int
      ty = baseY + 40
      lh = min 36 ((envH env - ty - 30) / fromIntegral levels)
      band r =
        let x1 = xOf (rLo r)
            x2 = xOf (rHi r + 1)
            y = ty + fromIntegral (rDepth r) * lh
        in box env (if rMerged r then hsb 150 60 70 else hsb 200 40 40) (x1 + 2) y (x2 - x1 - 4) (lh - 6)
      leaf k = box env (hsb 150 30 40) (xOf k + 2) (ty + fromIntegral (levels - 1) * lh) (bw - 4) (lh - 6)
      depthLabel d =
        txt env 16 (ty + fromIntegral d * lh + 20) 14 (hsb 0 0 70) ("d" ++ show d)
      help = hsb 0 0 65
      nLog = round (fromIntegral n * logBase 2 (fromIntegral n) :: Float) :: Int
  in Pictures
       [ Pictures (map bar [0 .. len - 1])
       , Pictures (map band (sRanges s))
       , Pictures (map leaf [0 .. len - 1])
       , Pictures (map depthLabel [0 .. levels - 1])
       , txt env 20 40 22 (hsb 0 0 100)
           ("Merge sort   compares " ++ show (sComps s) ++ "   writes " ++ show (sWrites s)
             ++ (if sDone s then "   Done!" else ""))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "pause" else "play")
             ++ "   Right: 1 step   R: redo   1 random   2 reversed   3 nearly sorted   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help
           ("n log2 n = " ++ show nLog ++ "   n^2/2 = " ++ show (n * n `div` 2)
             ++ "   (yellow = the two compared, pink = just written)")
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 240 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> reset (wMode w) w
    | c `elem` "123" -> reset (digitToInt c) w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 逆順（2）でも比較回数があまり変わらないことを確かめる（挿入ソートとの違い）
-- 2. n を 64、128 にして、比較回数と n log2 n を比べる
-- 3. 区間が小さい（例：8 個以下）ときは挿入ソートに切りかえると速くなる。やってみよう

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
