-- ============================================
-- 06 ハンガリー法（割り当て問題）
-- n 人に n 個の仕事を 1 つずつ。表の数字 = その人がその仕事をするコスト。合計を最小にしたい
-- 総当たりは n! 通り（10 人で 362 万通り）。ハンガリー法は O(n³)
-- 各行・各列に「ポテンシャル」u, v を持ち、c − u − v ≥ 0 を保ちながら、0 になったマスだけでマッチングを広げる
-- スペース = 1 手 / A = 自動 / マスをクリック = コスト +1（Shift で −1）/ 上下 = 人数 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.List (delete, foldl')
import qualified Data.Vector.Unboxed as U

-- アルゴリズムの状態（1 手ごとにこれが 1 つ流れてくる）
data S = S
  { sU :: !(U.Vector Int)
  , sV :: !(U.Vector Int)
  , sP :: !(U.Vector Int)    -- sP ! j = 仕事 j の担当の人（0 = なし）
  , sWay :: !(U.Vector Int)
  , sRow :: !Int
  , sUsed :: [Bool]          -- 調べ中の列（[] = なし）
  , sFin :: !Bool
  , sMsg :: String
  }

data World = World
  { wN :: !Int
  , wCost :: [[Int]]
  , wS :: !S
  , wRest :: [S]             -- これから流れてくる状態
  , wAuto :: !Bool
  , wTimer :: !Int
  , wBest :: Maybe Int       -- 総当たりの答え（n ≤ 8 のときだけ）
  }

infI :: Int
infI = 1000000000

main :: IO ()
main = runSketch (sketch "15-06 Hungarian Method" (restart (world0 5 [])))
  { skBack  = hsb 170 30 9
  , skStart = \_ -> randomCost (world0 5 [])    -- ★ 最初の人数
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initS :: Int -> S
initS n =
  let z = U.replicate (n + 1) 0
  in S z z z z 0 [] False "Space: start with row 1 (person 1)"

world0 :: Int -> [[Int]] -> World
world0 n c = World n c (initS n) [] False 0 Nothing

randomCost :: World -> IO World
randomCost w = do
  let n = wN w
  c <- mapM (const (mapM (const (randomRIO (1, 19))) [1 .. n])) [1 .. n]
  pure (restart w { wCost = c })

restart :: World -> World
restart w =
  let n = wN w
      c = wCost w
  in case (if length c == n then steps (hungarian c) (initS n) else []) of
       (s : r) -> w { wS = s, wRest = r, wAuto = False, wBest = if n <= 8 then Just (bruteForce c) else Nothing }
       [] -> w { wS = initS n, wRest = [], wAuto = False, wBest = Nothing }

-- ハンガリー法（ポテンシャル版）。1 行ずつ加え、最短の「交互道」を見つけてマッチングを 1 つ増やす
hungarian :: [[Int]] -> Gen S ()
hungarian cost = do
  yield
  forM_ [1 .. n] $ \i -> do
    modify (\s -> s { sP = sP s U.// [(0, i)] })
    j0 <- search i 0 (U.replicate (n + 1) infI) (U.replicate (n + 1) False)
    flipPath j0
    modify (\s -> s { sRow = i, sUsed = [], sMsg = "Re-pair along the alternating path -> " ++ show i ++ " assigned" })
    yield
  modify (\s -> s { sRow = n + 1, sUsed = [], sFin = True
                  , sMsg = "Done! Minimum cost = " ++ show (negate (sV s U.! 0)) ++ " (matches -v0)" })
  yield
  where
    n = length cost
    a i j = cost !! (i - 1) !! (j - 1)

    -- 空いている仕事に着くまで、ポテンシャルをずらしながら広げる
    search :: Int -> Int -> U.Vector Int -> U.Vector Bool -> Gen S Int
    search i j0 minv used = do
      s <- gets id
      let used1 = used U.// [(j0, True)]
          i0 = sP s U.! j0
          scan (mv, wy, d, jb) j
            | used1 U.! j = (mv, wy, d, jb)
            | otherwise =
                let cur = a i0 j - sU s U.! i0 - sV s U.! j
                    (mv', wy') = if cur < mv U.! j
                                   then (mv U.// [(j, cur)], wy U.// [(j, j0)])
                                   else (mv, wy)
                in if mv' U.! j < d then (mv', wy', mv' U.! j, j) else (mv', wy', d, jb)
          (minv1, way1, delta, j1) = foldl' scan (minv, sWay s, infI, 0) [1 .. n]
          usedJs = [j | j <- [0 .. n], used1 U.! j]
          u1 = U.accum (+) (sU s) [(sP s U.! j, delta) | j <- usedJs]
          v1 = U.accum (-) (sV s) [(j, delta) | j <- usedJs]
          minv2 = U.imap (\j m -> if used1 U.! j then m else m - delta) minv1
          owner = sP s U.! j1
          tailMsg | owner /= 0 = " (person " ++ show owner ++ " is there: scan that row too)"
                  | otherwise = " (free!)"
      modify (\st -> st { sU = u1, sV = v1, sWay = way1, sRow = i, sUsed = U.toList used1
                        , sMsg = "Person " ++ show i ++ ": shift potentials by " ++ show delta
                                 ++ ", job " ++ show j1 ++ " cell becomes 0" ++ tailMsg })
      yield
      if owner /= 0 then search i j1 minv2 used1 else pure j1

    -- 交互道にそって担当を付けかえる
    flipPath :: Int -> Gen S ()
    flipPath j0 = do
      s <- gets id
      let j1 = sWay s U.! j0
      modify (\st -> st { sP = sP st U.// [(j0, sP st U.! j1)] })
      if j1 /= 0 then flipPath j1 else pure ()

-- 総当たり（枝刈りつき）
bruteForce :: [[Int]] -> Int
bruteForce cost = go cost [0 .. length cost - 1] 0 maxBound
  where
    go :: [[Int]] -> [Int] -> Int -> Int -> Int
    go _ _ s best | s >= best = best
    go [] _ s _ = s
    go (r : rs) free s best = foldl' (\b j -> go rs (delete j free) (s + r !! j) b) best free

advance :: World -> World
advance w = case wRest w of
  (s : r) -> w { wS = s, wRest = r, wAuto = wAuto w && not (sFin s) }
  [] -> w { wAuto = False }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 30 == 0 then advance w' else w')
  | otherwise = pure w

cellSize :: Env -> World -> Float
cellSize env w =
  let k = fromIntegral (wN w + 1)
  in minimum [70, (envH env - 260) / k, (envW env * 0.55) / k]

ox, oy :: Env -> Float
ox env = envW env * 0.08 + 60
oy _ = 140

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      s = cellSize env w
      x0 = ox env
      y0 = oy env
      st = wS w
      row = sRow st
      used = sUsed st
      usedAt j = case drop j used of
        (b : _) -> b
        [] -> False
      fi' :: Int -> Float
      fi' = fromIntegral
      header j = Pictures
        [ txtC env (x0 + (fi' j - 0.5) * s) (y0 - 34) 15 (if usedAt j then hsb 45 70 90 else hsb 170 20 70) ("Job" ++ show j)
        , txtC env (x0 + (fi' j - 0.5) * s) (y0 - 12) 14 (hsb 195 70 100) ("v=" ++ show (sV st U.! j)) ]
      assignedAt i j = sP st U.! j == i
      counted i = sFin st || i < row || (i == row && null used)
      total = sum [ wCost w !! (i - 1) !! (j - 1) | i <- [1 .. n], j <- [1 .. n], assignedAt i j, counted i ]
      rowPic i = Pictures
        ( txtC env (x0 - 76) (y0 + (fi' i - 0.5) * s) 15 (if i <= row then lum 95 else lum 50) ("P" ++ show i)
        : txtC env (x0 - 30) (y0 + (fi' i - 0.5) * s) 14 (hsb 330 60 100) ("u=" ++ show (sU st U.! i))
        : map (cellPic i) [1 .. n] )
      cellPic i j =
        let c = wCost w !! (i - 1) !! (j - 1)
            red = c - sU st U.! i - sV st U.! j
            assigned = assignedAt i j
            fc | assigned = hsb 330 70 85
               | i <= row && red == 0 = hsb 45 80 75
               | otherwise = hsb 170 25 (remap (fi' c) 1 20 35 15)
            bx = x0 + (fi' j - 1) * s
            by = y0 + (fi' i - 1) * s
        in Pictures
             [ box env fc bx by s s
             , wire env (hsb 170 20 25) bx by s s
             , txtC env (x0 + (fi' j - 0.5) * s) (y0 + (fi' i - 0.68) * s) (max 14 (s * 0.32)) (lum 100) (show c)
             , if i <= row
                 then txtC env (x0 + (fi' j - 0.5) * s) (y0 + (fi' i - 0.22) * s) 14 (lum (if assigned then 100 else 70)) ("r=" ++ show red)
                 else Blank ]
      rx = x0 + fi' n * s + 50
      notes = [ "Yellow = cell with r = c - u - v = 0"
              , "Pink = assigned (always r = 0)"
              , "r never goes negative"
              , "-> any assignment >= sum u + sum v"
              , "-> all assigned on r = 0 means minimum" ]
  in Pictures
       [ Pictures (map header [1 .. n])
       , Pictures (map rowPic [1 .. n])
       , txt env rx (y0 + 10) 22 (lum 100) ("Assigned cost " ++ show total)
       , case wBest w of
           Just b -> txt env rx (y0 + 44) 17 (lum 75) ("Brute force (" ++ show (factorial n) ++ " ways) min = " ++ show b)
           Nothing -> Blank
       , Pictures [txt env rx (y0 + 90 + fromIntegral k * 26) 15 (lum 70) l | (k, l) <- zip [0 :: Int ..] notes]
       , txt env 20 36 18 (lum 100) ("Space: 1 step  A: auto  Click cell: cost +1 (Shift -1)  Up/Down: people " ++ show n ++ "  R: random")
       , txt env 20 62 15 (lum 65) "u = person potential, v = job potential. The method also solves the dual of the assignment LP"
       , txt env 20 (envH env - 26) 18 (hsb 45 80 100) (sMsg st)
       ]

factorial :: Int -> Int
factorial k = product [1 .. k]

-- マスのクリック：コストを変えて最初から
clickCell :: Env -> Bool -> World -> World
clickCell env sh w =
  let (mx, my) = mouseP env
      s = cellSize env w
      j = floor ((mx - ox env) / s) :: Int
      i = floor ((my - oy env) / s) :: Int
      n = wN w
      bump c = max 0 (min 99 (c + (if sh then -1 else 1)))
  in if i >= 0 && i < n && j >= 0 && j < n
       then restart w { wCost = setAt i (setAt j (bump (wCost w !! i !! j)) (wCost w !! i)) (wCost w) }
       else w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down m _ -> pure (clickCell env (shift m == Down) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> randomCost w { wN = min 9 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomCost w { wN = max 2 (wN w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> randomCost w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 最後のコストが総当たりの最小と必ず一致することを確かめる
-- 2. ある人のコストを全部 +5 すると？（u が 5 増えるだけで、割り当ては変わらない）
-- 3. 「最大」にしたいとき（利益の割り当て）は、コストを（大きい数 − 利益）にすればよい

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

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 左上 (x, y)、幅 w、高さ h の枠線
wire :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
wire env c x y w h = Color c (rectLine (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- リストの i 番目だけ置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]
