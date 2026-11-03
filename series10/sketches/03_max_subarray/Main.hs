-- ============================================
-- 03 最大部分配列（分割統治）
-- 株価の上がり下がり（毎日の変化）から「いつ買って、いつ売れば一番もうかるか」を探す
-- 総当たりは n^2 通り。分割統治は「左だけ」「右だけ」「真ん中をまたぐ」の 3 つに分けて n log n
-- さらに 1 回なめるだけ（n）のカダンの方法もある
-- スペース：再生／停止　→：1 手　1：分割統治　2：カダン　3：総当たり　R：新しいデータ　↑↓ 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as U

n :: Int
n = 24              -- ★ 日数

-- 部分配列 [lo..hi] とその和
data Sub = Sub { bLo :: !Int, bHi :: !Int, bSum :: !Int }

-- 「まだ見つかっていない」を表す和（JavaScript の -Infinity の代わり）
minusInf :: Int
minusInf = -1000000

noBest :: Sub
noBest = Sub 0 (-1) minusInf

data S = S
  { sLo   :: !Int     -- 調べている範囲
  , sHi   :: !Int
  , sMid  :: !Int
  , sP    :: !Int     -- いま足している部分の両端
  , sQ    :: !Int
  , sBest :: !Sub
  , sOps  :: !Int
  , sDone :: !Bool
  }

data World = World
  { wD       :: !(U.Vector Int)
  , wS       :: !S
  , wRest    :: [S]
  , wMethod  :: !Int
  , wPlaying :: !Bool
  , wSpeed   :: !Float
  , wAcc     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-03 Maximum Subarray" initialWorld)
  { skBack  = hsb 200 35 9
  , skStart = \_ -> newData initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialS :: S
initialS = S (-1) (-1) (-1) (-1) (-1) noBest 0 False

initialWorld :: World
initialWorld = World U.empty initialS [] 1 True 6 0

newData :: World -> IO World
newData w = do
  xs <- mapM (const (randomRIO (-12, 12))) [1 .. n]
  pure (restart w { wD = U.fromList xs })

restart :: World -> World
restart w =
  let d = wD w
      program = case wMethod w of
        2 -> kadane d
        3 -> brute d
        _ -> do
          r <- findMax d 0 (U.length d - 1)
          modify (\s -> s { sBest = r })
      finish = do
        program
        modify (\s -> s { sDone = True, sLo = -1, sHi = -1, sMid = -1, sP = -1, sQ = -1 })
        yield
  in w { wS = initialS, wRest = steps finish initialS }

addOp :: Gen S ()
addOp = modify (\s -> s { sOps = sOps s + 1 })

-- 真ん中をまたぐ部分の最大
findCrossing :: U.Vector Int -> Int -> Int -> Int -> Gen S Sub
findCrossing d lo mid hi = do
  let leftStep (acc, bestS, bestI) i = do
        let acc' = acc + d U.! i
            (bestS', bestI') = if acc' > bestS then (acc', i) else (bestS, bestI)
        addOp
        modify (\s -> s { sLo = lo, sHi = hi, sMid = mid, sP = i, sQ = mid })
        yield
        pure (acc', bestS', bestI')
  (_, leftSum, maxL) <- foldM leftStep (0, minusInf, mid) [mid, mid - 1 .. lo]
  let rightStep (acc, bestS, bestJ) j = do
        let acc' = acc + d U.! j
            (bestS', bestJ') = if acc' > bestS then (acc', j) else (bestS, bestJ)
        addOp
        modify (\s -> s { sLo = lo, sHi = hi, sMid = mid, sP = maxL, sQ = j })
        yield
        pure (acc', bestS', bestJ')
  (_, rightSum, maxR) <- foldM rightStep (0, minusInf, mid + 1) [mid + 1 .. hi]
  pure (Sub maxL maxR (leftSum + rightSum))

-- 分割統治：左半分・右半分・真ん中をまたぐ、の 3 つのうち一番大きいもの
findMax :: U.Vector Int -> Int -> Int -> Gen S Sub
findMax d lo hi
  | lo == hi = addOp >> pure (Sub lo hi (d U.! lo))
  | otherwise = do
      let mid = (lo + hi) `div` 2
      modify (\s -> s { sLo = lo, sHi = hi, sMid = mid, sP = -1, sQ = -1 })
      yield
      l <- findMax d lo mid
      r <- findMax d (mid + 1) hi
      c <- findCrossing d lo mid hi
      let res | bSum l >= bSum r && bSum l >= bSum c = l
              | bSum r >= bSum l && bSum r >= bSum c = r
              | otherwise = c
      modify (\s -> if bSum res > bSum (sBest s) then s { sBest = res } else s)
      pure res

kadane :: U.Vector Int -> Gen S ()
kadane d = do
  let go (cur, start) i = do
        addOp
        -- マイナスを引きずるなら捨てて今日から
        let (cur', start') = if cur <= 0 then (d U.! i, i) else (cur + d U.! i, start)
        modify (\s -> if cur' > bSum (sBest s) then s { sBest = Sub start' i cur' } else s)
        modify (\s -> s { sLo = -1, sHi = -1, sMid = -1, sP = start', sQ = i })
        yield
        pure (cur', start')
  _ <- foldM go (0 :: Int, 0 :: Int) [0 .. U.length d - 1]
  pure ()

brute :: U.Vector Int -> Gen S ()
brute d =
  forM_ [0 .. U.length d - 1] $ \i -> do
    let go acc j = do
          let acc' = acc + d U.! j
          addOp
          modify (\s -> if acc' > bSum (sBest s) then s { sBest = Sub i j acc' } else s)
          modify (\s -> s { sLo = -1, sHi = -1, sMid = -1, sP = i, sQ = j })
          yield
          pure acc'
    _ <- foldM go (0 :: Int) [i .. U.length d - 1]
    pure ()

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
      d = wD w
      len = U.length d
      l = 70
      r = envW env - 70
      bw = (r - l) / fromIntegral (max 1 len)
      xOf k = l + fromIntegral k * bw
      hgt = envH env
      midY = hgt * 0.42
      sc = (hgt * 0.22) / 12
      best = sBest s
      hasBest = bHi best >= bLo best && bHi best >= 0
      -- 範囲
      rangeBand
        | sLo s >= 0 = box env (hsba 200 40 30 0.6) (xOf (sLo s)) (midY - 13 * sc)
                         (fromIntegral (sHi s - sLo s + 1) * bw) (26 * sc)
        | otherwise = Blank
      midLine
        | sMid s >= 0 = seg env (hsb 45 80 100) 2 (xOf (sMid s + 1), midY - 14 * sc) (xOf (sMid s + 1), midY + 14 * sc)
        | otherwise = Blank
      bestBand
        | hasBest = box env (hsba 140 70 60 0.5) (xOf (bLo best)) (midY + 14 * sc)
                      (fromIntegral (bHi best - bLo best + 1) * bw) 10
        | otherwise = Blank
      curBand
        | sP s >= 0 = box env (hsba 45 80 100 0.3) (xOf (min (sP s) (sQ s))) (midY - 13 * sc)
                        (fromIntegral (abs (sQ s - sP s) + 1) * bw) (26 * sc)
        | otherwise = Blank
      bar i =
        let v = d U.! i
            h = fromIntegral v * sc
            inBest = i >= bLo best && i <= bHi best
            hue = if v >= 0 then 140 else 0
            c = hsb hue (if inBest then 80 else 45) (if inBest then 95 else 70)
        in Pictures
             [ box env c (xOf i + 2) (midY - max h 0) (bw - 4) (abs h)
             , txtC env (xOf i + bw / 2) (midY + (if v >= 0 then 12 else -12)) 14 (hsb 0 0 80) (show v) ]
      -- 株価（累積和）
      pts = U.toList (U.scanl (+) 0 d)
      py0 = hgt - 80
      psc = (hgt * 0.22) / fromIntegral (max 1 (foldr (max . abs) 0 pts))
      priceY p = py0 - hgt * 0.12 - fromIntegral p * psc * 0.5
      priceAt i = U.scanl (+) 0 d U.! i
      priceLine = Color (hsb 190 70 100)
        (thickPath 2.5 [pt env (xOf i, priceY p) | (i, p) <- zip [0 :: Int ..] pts])
      dots
        | hasBest = Pictures
            [ Color (hsb 140 80 100) (at (pt env (xOf (bLo best), priceY (priceAt (bLo best)))) (disc 6))
            , Color (hsb 330 80 100) (at (pt env (xOf (bHi best + 1), priceY (priceAt (bHi best + 1)))) (disc 6)) ]
        | otherwise = Blank
      name = case wMethod w of
        2 -> "Kadane Theta(n)"
        3 -> "Brute force Theta(n^2)"
        _ -> "Divide & conquer Theta(n log n)"
      help = hsb 0 0 65
  in Pictures
       [ rangeBand, midLine, bestBand, curBand
       , Pictures (map bar [0 .. len - 1])
       , seg env (hsb 0 0 50) 1 (l, midY) (r, midY)
       , priceLine, dots
       , txt env l (hgt - 30) 14 (hsb 0 0 60) "Below: stock price (running sum of daily changes). Buy at the green dot, sell at the pink dot"
       , txt env 20 40 22 (hsb 0 0 100)
           ("Max subarray   " ++ name ++ "   ops " ++ show (sOps s) ++ "   best "
             ++ (if bSum best == minusInf then "-" else show (bSum best))
             ++ (if sDone s then "   Done!" else ""))
       , txt env 20 68 15 help
           ("Space: " ++ (if wPlaying w then "stop" else "play")
             ++ "   Right: 1 step   1 divide & conquer   2 Kadane   3 brute force   R: new data   Up/Down speed " ++ num (wSpeed w))
       , txt env 20 92 15 help "Yellow line = middle (divide & conquer compares the best of: left only, right only, crossing the middle)"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w { wPlaying = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 240 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w / 2) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> newData w
    | c `elem` "123" -> pure (restart w { wMethod = digitToInt c })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 つの方法で計算回数を比べる。n を 2 倍にすると、それぞれ何倍になる？
-- 2. 全部マイナスのデータ（randomRIO (-12, -1)）だと答えはどうなる？
-- 3. カダンの方法がなぜ正しいのか、「マイナスの和を引きずらない」で説明してみよう

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
