-- ============================================
-- 03 置換と巡回表記
-- n 個のボールの並べかえ（置換）を「巡回」に分けて書く：(1 3 5)(2 4) は 1→3→5→1 と 2→4→2
--   偶奇 = 入れかえ（互換）の回数の偶奇。位数 = 巡回の長さの最小公倍数
-- ボールを 2 つクリック = 入れかえ / スペース = 同じ並べかえをもう 1 回（σ², σ³, …）/ X = ランダム
-- ↑↓ = ボールの数 / 0 = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (findIndex, intercalate)
import qualified Data.Set as Set

data World = World
  { wN     :: !Int      -- ★ ボールの数
  , wArr   :: ![Int]    -- arr[位置] = ボールの番号（0 始まり）
  , wSigma :: ![Int]    -- 「並べかえ」= 1 回目にできた並び（もう 1 回やるときに使う）
  , wSwaps :: !Int      -- 入れかえの回数
  , wPick  :: !Int      -- えらんだ位置（なければ -1）
  , wAnim  :: !(Maybe ([Int], Float))   -- 各ボールの前の位置と進み具合
  , wPower :: !Int      -- 何回目か（σ^k）
  }

fresh :: Int -> World
fresh n = World n [0 .. n - 1] [0 .. n - 1] 0 (-1) Nothing 1

main :: IO ()
main = runSketch (sketch "23-03 Permutations and Cycles" (fresh 6))
  { skBack  = rgb255 250 246 238
  , skStep  = \_ _ w -> pure (stepAnim w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

stepAnim :: World -> World
stepAnim w = case wAnim w of
  Just (f, t) | t + 0.035 >= 1 -> w { wAnim = Nothing }
              | otherwise -> w { wAnim = Just (f, t + 0.035) }
  Nothing -> w

slotX :: Int -> Int -> Float
slotX n i = width * 0.5 + (fromIntegral i - (fromIntegral n - 1) / 2) * min 110 ((width - 120) / fromIntegral n)

slotY :: Float
slotY = height * 0.36

ballD :: Int -> Float
ballD n = min 80 ((width - 120) / fromIntegral n * 0.7)

drawWorld :: World -> Picture
drawWorld w = Pictures (arcs ++ balls ++ hud)
  where
    n = wN w
    r = ballD n
    arr = wArr w
    cyc = cycles arr
    -- 巡回の矢印（ボールがもとの位置から今の位置へ）
    arcs = concat
      [ cycleArc (hsba (fromIntegral ((ci * 67) `mod` 360)) 60 80 0.8) a (arr !! a)
      | (ci, c) <- zip [0 :: Int ..] cyc, length c >= 2, a <- c ]
    cycleArc col a b =       -- 位置 a にあるのはボール b（もとは位置 b）→ 位置 b から a へ弧
      let x1 = slotX n b
          x2 = slotX n a
          mid = (x1 + x2) / 2
          h = abs (x2 - x1) * 0.18 + 16
          ay = slotY + r * 0.8
          halfW = abs (x2 - x1) / 2
          ps = [ (mid + halfW * cos t, ay + h * sin t) | i <- [0 .. 24 :: Int], let t = pi * fromIntegral i / 24 ]
      in [ Color col (thickPath 3 (map pt ps))
         , Color col (Polygon (map pt [(x2, ay), (x2 - 7, ay + 14), (x2 + 7, ay + 14)])) ]
    -- ボール
    balls = concat
      [ [ discAt x y (r / 2 + 2) (gray 0.12) | p == wPick w ]
        ++ [ discAt x y (r / 2) (hsb (fromIntegral b * 360 / fromIntegral n) 60 92)
           , txtC x y (r * 0.45) white (show (b + 1))
           , txtC (slotX n p) (slotY - r * 0.5 - 30) 14 (gray 0.55) ("pos " ++ show (p + 1)) ]
      | (p, b) <- zip [0 ..] arr
      , let (x, y) = ballPos p ]
    ballPos p = case wAnim w of
      Just (from, t) ->      -- 前の位置から弧を描いて移動
        let f = from !! p
            e = t * t * (3 - 2 * t)
        in ( lerpF (slotX n f) (slotX n p) e
           , slotY - sin (e * pi) * (40 + fromIntegral (abs (f - p)) * 18) )
      Nothing -> (slotX n p, slotY)
    -- 文字
    y = height * 0.66
    px = width * 0.55
    sgn = (n - length cyc) `mod` 2
    lens = map length cyc
    ord = foldl lcm 1 lens
    hud =
      [ txt 20 36 18 (gray 0.12) ("Click two balls: swap   Space: same rearrangement again   X: random   Up/Down: n = " ++ show n ++ "   0: reset")
      , txt 20 62 15 (gray 0.43) "Any rearrangement is a product of swaps. Even or odd does not depend on how you write it"
      , txt 40 y 18 (gray 0.12) ("One-line form: balls at positions 1-" ++ show n)
      , txt 40 (y + 40) 30 (gray 0.12) ("( " ++ intercalate "  " (map (show . (+ 1)) arr) ++ " )")
      , txt 40 (y + 90) 18 (gray 0.12) "Cycle notation"
      , txt 40 (y + 132) 34 (rgb255 160 60 200) (cycleStr cyc)
      , txt px y 18 (gray 0.12) ("Parity: " ++ (if sgn /= 0 then "odd permutation (sign -1)" else "even permutation (sign +1)"))
      , txt px (y + 26) 15 (gray 0.43) ("n - cycles = " ++ show n ++ " - " ++ show (length cyc) ++ " = " ++ show (n - length cyc)
          ++ " (fewest swaps). Clicked swaps: " ++ show (wSwaps w))
      , txt px (y + 70) 18 (gray 0.12) ("Order: lcm(" ++ intercalate ", " (map show lens) ++ ") = " ++ show ord)
      , txt px (y + 96) 15 (gray 0.43) ("Press Space " ++ show ord ++ " times to return to the start")
      , txt px (y + 140) 18 (gray 0.12) ("Now = " ++ (if wPower w == 0 then "identity (back to the start)" else "sigma" ++ (if wPower w > 1 then "^" ++ show (wPower w) else "")) ++ "  (sigma = what Space repeats)")
      ]

-- 置換を巡回に分ける（位置 i → arr[i] をたどる）
cycles :: [Int] -> [[Int]]
cycles p = go Set.empty 0
  where
    n = length p
    go seen i
      | i >= n = []
      | Set.member i seen = go seen (i + 1)
      | otherwise =
          let c = walk i True
          in c : go (foldr Set.insert seen c) (i + 1)
      where walk j first | not first && j == i = []
                         | otherwise = j : walk (p !! j) False

cycleStr :: [[Int]] -> String
cycleStr cyc =
  let s = concat [ "(" ++ unwords (map (show . (+ 1)) c) ++ ")" | c <- cyc, length c > 1 ]
  in if null s then "e (nothing moved)" else s

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click (toP5 (envMouse env)) w)
  EventKey (SpecialKey KeySpace) Down _ _ | Nothing <- wAnim w ->
    -- 同じ並べかえをもう 1 回：新しい並び[p] = 今の並び[σ(p)]（位置 σ(p) のボールが位置 p へ）
    let arr' = [wArr w !! s | s <- wSigma w]
        pw = wPower w + 1
    in pure w { wArr = arr', wAnim = Just (wSigma w, 0)
              , wPower = if arr' == [0 .. wN w - 1] then 0 else pw }
  EventKey (Char c) Down _ _ | c == 'x' || c == 'X' -> do
    a <- shuffle [0 .. wN w - 1]
    pure w { wArr = a, wSigma = a, wPower = 1, wSwaps = 0, wAnim = Nothing }
  EventKey (Char '0') Down _ _ -> pure (fresh (wN w))
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (fresh (min 10 (wN w + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (fresh (max 3 (wN w - 1)))
  _ -> pure w

click :: Point -> World -> World
click mouse w =
  case findIndex (\p -> dist2 mouse (slotX n p, slotY) < ballD n / 2) [0 .. n - 1] of
    Nothing -> w
    Just p
      | wPick w < 0 -> w { wPick = p }
      | wPick w == p -> w { wPick = -1 }
      | otherwise ->
          let q = wPick w
              sw i | i == p = q
                   | i == q = p
                   | otherwise = i
              arr' = [wArr w !! sw i | i <- [0 .. n - 1]]
          in w { wArr = arr', wAnim = Just (map sw [0 .. n - 1], 0), wSwaps = wSwaps w + 1
               , wPick = -1, wSigma = arr', wPower = 1 }
  where n = wN w

-- フィッシャー・イェーツのシャッフル
shuffle :: [Int] -> IO [Int]
shuffle xs = go (length xs - 1) xs
  where
    go i a
      | i <= 0 = pure a
      | otherwise = do
          j <- randomRIO (0, i)
          go (i - 1) (swapAt i j a)
    swapAt i j a = [ if k == i then a !! j else if k == j then a !! i else v | (k, v) <- zip [0 ..] a ]

-- ---------- やってみよう ----------
-- 1. 3 回入れかえて巡回表記を見る。偶奇は「入れかえ回数の偶奇」と一致する？
-- 2. (1 2 3)(4 5) の位数は 6。スペースを押して確かめよう。σ³ と σ² はどんな形？
-- 3. X を何回か押して、位数がいちばん大きいものを探そう（n = 10 なら 30 = lcm(2, 3, 5)）
-- 4. 長さ 3 の巡回は偶置換（入れかえ 2 回）。だから 15 パズル（04）で困ったことが起きる

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))
