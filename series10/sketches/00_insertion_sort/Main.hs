-- ============================================
-- 00 挿入ソート
-- トランプの手札を並べるときと同じ。左側の「並んだ山」に、次の 1 枚を正しい場所へ差し込む
-- 緑の部分はいつも並んでいる（ループ不変条件）。これが正しさの証明になる
-- スペース：再生／一時停止　→：1 手だけ進める　R：新しい数列　1：ランダム　2：ほぼ整列　3：逆順
-- 上下キー：速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.List (sort, sortBy)
import qualified Data.Vector.Unboxed as U

n :: Int
n = 24            -- ★ 並べる数の個数（多すぎると見づらい）

startSpeed :: Float
startSpeed = 4    -- ★ 1 秒あたりの手数のめやす（上下キーで変わる）

-- アルゴリズムの状態（1 手ごとにこれが 1 つ流れてくる）
data S = S
  { sA      :: !(U.Vector Int)
  , sJ      :: !Int            -- 0..j-1 は並んでいる
  , sHole   :: !Int            -- 札を抜いた穴（-1 = なし）
  , sCmp    :: !Int            -- 比べている場所（-1 = なし）
  , sPlaced :: !Int            -- 差し込んだ場所（-1 = なし）
  , sKey    :: !(Maybe Int)    -- 手に持っている札
  , sComps  :: !Int
  , sMoves  :: !Int
  , sDone   :: !Bool
  }

data World = World
  { wS       :: !S
  , wRest    :: [S]            -- これから流れてくる状態
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  , wMode    :: !Int
  }

main :: IO ()
main = runSketch (sketch "10-00 Insertion Sort" initialWorld)
  { skBack  = hsb 230 30 10
  , skStart = \_ -> reset 1 initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: [Int] -> S
initialS xs = S (U.fromList xs) 1 (-1) (-1) (-1) Nothing 0 0 False

initialWorld :: World
initialWorld = World (initialS []) [] True startSpeed 0 1

-- 数列を作り直す（1：ランダム　2：ほぼ整列　3：逆順）
reset :: Int -> World -> IO World
reset m w = do
  xs0 <- mapM (const (randomRIO (5, 99))) [1 .. n]
  xs <- case m of
    2 -> swapRandomly (3 :: Int) (sort xs0)
    3 -> pure (sortBy (flip compare) xs0)
    _ -> pure xs0
  let s0 = initialS xs
  pure w { wMode = m, wS = s0, wRest = steps insertionSort s0 }

-- 3 組だけ入れかえて「ほぼ整列」にする
swapRandomly :: Int -> [Int] -> IO [Int]
swapRandomly 0 xs = pure xs
swapRandomly k xs = do
  i <- randomRIO (0, n - 1)
  j <- randomRIO (0, n - 1)
  let v = U.fromList xs
  swapRandomly (k - 1) (U.toList (v U.// [(i, v U.! j), (j, v U.! i)]))

-- 挿入ソートを、1 手ずつ止まれるように Gen で書いたもの
insertionSort :: Gen S ()
insertionSort = do
  len <- gets (U.length . sA)
  forM_ [1 .. len - 1] $ \j -> do
    key <- rd j
    modify (\s -> s { sJ = j, sKey = Just key, sHole = j, sCmp = -1, sPlaced = -1 })
    yield
    i <- slide key (j - 1)
    wr (i + 1) key                  -- 空いた場所に差し込む
    modify (\s -> s { sMoves = sMoves s + 1, sJ = j + 1, sKey = Nothing, sHole = -1, sCmp = -1, sPlaced = i + 1 })
    yield
  modify (\s -> s { sDone = True, sJ = len, sKey = Nothing, sHole = -1, sCmp = -1, sPlaced = -1 })
  yield

-- key より大きい数を右へずらしていき、最後の i を返す
slide :: Int -> Int -> Gen S Int
slide key i
  | i < 0 = pure i
  | otherwise = do
      modify (\s -> s { sComps = sComps s + 1, sHole = i + 1, sCmp = i })
      yield
      v <- rd i
      if v <= key                   -- ★ <= を < にすると？（やってみよう 2）
        then pure i
        else do
          wr (i + 1) v              -- 大きい数を 1 つ右へずらす
          modify (\s -> s { sMoves = sMoves s + 1, sHole = i, sCmp = -1 })
          yield
          slide key (i - 1)

rd :: Int -> Gen S Int
rd i = gets (\s -> sA s U.! i)

wr :: Int -> Int -> Gen S ()
wr i v = modify (\s -> s { sA = sA s U.// [(i, v)] })

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
      wd = envW env - 120
      bw = wd / fromIntegral (max 1 len)
      baseY = envH env - 120
      unit = (envH env - 320) / 100
      sortedEnd = sJ s
      xOf k = 60 + fromIntegral k * bw
      white' = hsb 0 0 100
      bar k =
        let v = a U.! k
            h = fromIntegral v * unit
            c | k == sPlaced s = hsb 140 90 100
              | k == sCmp s = hsb 45 90 100
              | sDone s || k < sortedEnd = hsb 140 60 85
              | otherwise = hsb 220 20 55
        in if k == sHole s && sKey s /= Nothing
             then Blank                     -- 抜き出した札の穴
             else Pictures
               [ box env c (xOf k + 2) (baseY - h) (bw - 4) h
               , txtC env (xOf k + bw / 2) (baseY + 16) 14 (hsb 0 0 80) (show v) ]
      -- 手に持っている札（key）を上に浮かせて描く
      held = case sKey s of
        Just key | sHole s >= 0 ->
          let h = fromIntegral key * unit
              x = xOf (sHole s)
              pink = hsb 330 80 100
          in Pictures
               [ box env pink (x + 2) (baseY - h - 60) (bw - 4) h
               , txtC env (x + bw / 2) (baseY - h - 78) 14 pink ("key=" ++ show key) ]
        _ -> Blank
      -- 不変条件の帯
      invariant
        | not (sDone s) && sortedEnd > 0 =
            let g = hsb 140 60 85
            in Pictures
                 [ seg env g 3 (60, baseY + 34) (60 + fromIntegral sortedEnd * bw, baseY + 34)
                 , txt env 60 (baseY + 58) 14 g
                     ("A[0.." ++ show (sortedEnd - 1) ++ "] is sorted (loop invariant)") ]
        | otherwise = Blank
      help = hsb 0 0 65
  in Pictures
       [ Pictures (map bar [0 .. len - 1])
       , held
       , invariant
       , txt env 20 40 22 white'
           ("Insertion sort   compares " ++ show (sComps s) ++ "   moves " ++ show (sMoves s)
             ++ (if sDone s then "   Done!" else ""))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "pause" else "play")
             ++ "   Right: 1 step   R: redo   1 random   2 nearly sorted   3 reversed   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help
           ("Worst case (reversed) is about n^2/2 = " ++ show (n * n `div` 2)
             ++ " compares, nearly sorted about n. Speed depends on the input")
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
-- 1. 3（逆順）と 2（ほぼ整列）で比較回数を比べる。n を 2 倍にすると何倍になる？
-- 2. slide の v <= key を v < key にしても並ぶ？ 同じ数の順番（安定性）はどうなる？
-- 3. 降順に並べるには、どこを 1 文字変えればいい？

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
