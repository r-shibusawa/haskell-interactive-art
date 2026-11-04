-- ============================================
-- 41-00 順列をすべて並べる
-- n 個の色玉の並べ方（順列）は n! = n × (n−1) × … × 1 通り
-- Heap のアルゴリズム（隣り合わない 2 つを入れ替えていく）で、1 つずつ全部つくる
-- 操作：↑↓ = n（3〜6）/ スペース = 一時停止 / L = 辞書順生成と Heap を切り替え / R = 最初から / マウス左右 = 速さ
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U

startN :: Int
startN = 4               -- ★ 玉の数（3〜6）

data World = World
  { wN      :: !Int              -- 玉の数
  , wPerm   :: !(U.Vector Int)   -- いまの順列（0..n-1 の並び）
  , wTiles  :: ![Picture]        -- できた順列の一覧（新しい順。1 度つくった絵を使い回す）
  , wCount  :: !Int              -- できた順列の数
  , wPaused :: !Bool
  , wLex    :: !Bool             -- True なら辞書順（next permutation）
  , wDone   :: !Bool
  , wSwap   :: !(Int, Int)       -- 直前に入れ替えた 2 か所（光らせる）
  , wTick   :: !Float
  , wHeapC  :: !(U.Vector Int)   -- Heap のアルゴリズム用の状態
  , wHeapI  :: !Int
  }

main :: IO ()
main = runSketch (sketch "41-00 All permutations" world0)
  { skBack  = hsb 230 30 10
  , skStart = \env -> pure (restart env world0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World startN U.empty [] 0 False False False (-1, -1) 0 U.empty 1

restart :: Env -> World -> World
restart env w =
  let n = wN w
  in record env w
       { wPerm = U.enumFromN 0 n, wTiles = [], wCount = 0
       , wHeapC = U.replicate n 0, wHeapI = 1, wDone = False, wSwap = (-1, -1) }

-- ★ Heap のアルゴリズム：1 回呼ぶと次の順列に進む（入れ替えは 1 回だけ）
heapNext :: World -> Maybe World
heapNext w = go (wHeapI w) (wHeapC w)
  where
    n = wN w
    go i c
      | i >= n = Nothing                 -- 全部できた
      | c U.! i < i =
          let j = if even i then 0 else c U.! i
          in Just w { wPerm = swapAt j i (wPerm w), wSwap = (j, i)
                    , wHeapC = c U.// [(i, c U.! i + 1)], wHeapI = 1 }
      | otherwise = go (i + 1) (c U.// [(i, 0)])

-- ★ 辞書順：後ろから「上がる所」を探して入れ替え、残りを反転
lexNext :: World -> Maybe World
lexNext w =
  let p = wPerm w
      n = wN w
  in case [i | i <- [n - 2, n - 3 .. 0], p U.! i < p U.! (i + 1)] of
       [] -> Nothing
       (i : _) -> case [j | j <- [n - 1, n - 2 .. 0], p U.! j > p U.! i] of
         [] -> Nothing
         (j : _) ->
           let q = swapAt i j p
           in Just w { wPerm = U.take (i + 1) q U.++ U.reverse (U.drop (i + 1) q), wSwap = (i, j) }

-- 一覧に 1 つぶん描き足す
record :: Env -> World -> World
record env w =
  let (s, tileW, tileH, x, y, perRow) = layout env (wN w)
      idx = wCount w
      x0 = x + fromIntegral (idx `mod` perRow) * tileW
      y0 = y + fromIntegral (idx `div` perRow) * tileH
      tile = Pictures
        [ box env (hsb (ballHue (wN w) v) 70 95) (x0 + fromIntegral i * s) y0 (s - 1) (s - 1)
        | (i, v) <- zip [0 :: Int ..] (U.toList (wPerm w)) ]
  in w { wTiles = tile : wTiles w, wCount = idx + 1 }

-- 一覧の並べ方（n によって大きさを変える）
layout :: Env -> Int -> (Float, Float, Float, Float, Float, Int)
layout env n =
  let s | n <= 4 = 14
        | n == 5 = 10
        | otherwise = 7
      tileW = fromIntegral n * s + 8
      tileH = s + 4
      perRow = max 1 (floor ((envW env - 60) / tileW))
  in (s, tileW, tileH, 30, 250, perRow)

ballHue :: Int -> Int -> Float
ballHue n v = fromIntegral v * 360 / fromIntegral n + 10

speedOf :: Env -> Float
speedOf env = clampF 2 30 (remap (fst (mouseP env)) 0 (envW env) 2 30)   -- ★ 1 秒あたりの順列の数

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w || wDone w = pure w
  | otherwise = pure (advance env w { wTick = wTick w + speedOf env / 60 })

advance :: Env -> World -> World
advance env w
  | wTick w < 1 || wDone w = w
  | otherwise =
      let w1 = w { wTick = wTick w - 1 }
          next = if wLex w1 then lexNext w1 else heapNext w1
      in case next of
           Just w2 -> advance env (record env w2)
           Nothing -> w1 { wDone = True, wSwap = (-1, -1) }

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      r = 56
      gap = 16
      fn = fromIntegral n :: Float
      x0 = envW env / 2 - (fn * (r + gap) - gap) / 2 + r / 2
      y0 = 160
      ball (i, v) =
        let x = x0 + fromIntegral i * (r + gap)
            (a, b) = wSwap w
            glow = i == a || i == b
        in Pictures
             [ if glow then at (toG env x y0) (Color (hsba 50 80 100 0.4) (disc ((r + 22) / 2))) else Blank
             , at (toG env x y0) (Color (hsb (ballHue n v) 70 95) (disc (r / 2)))
             , txtM env x y0 22 (hsb 0 0 15) (show (v + 1)) ]
      formula = intercalate " x " (map show [n, n - 1 .. 1])
      total = product [1 .. n]
  in Pictures
       [ Pictures (reverse (wTiles w))
       , Pictures (map ball (zip [0 :: Int ..] (U.toList (wPerm w))))
       , txt env 20 34 18 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   " ++ show n ++ "! = " ++ formula ++ " = " ++ show total
            ++ "   made: " ++ show (wCount w) ++ " / " ++ show total)
       , txt env 20 60 15 (hsb 50 60 100)
           ("method: " ++ (if wLex w then "lexicographic" else "Heap") ++ " (L)"
            ++ (if wPaused w then "   PAUSED" else "") ++ (if wDone w then "   DONE! R: start again" else ""))
       , txt env 20 84 14 (hsb 0 0 60)
           (if wLex w then "Lexicographic: find the last rise from the right, swap there, reverse the rest."
                      else "Heap: swaps only 2 balls each time and never repeats. Mouse X: speed, Space: pause.")
       , txt env 30 240 14 (hsb 0 0 55) "All permutations made so far (one tile = one permutation)"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 6 -> restart env w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 3 -> restart env w { wN = wN w - 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "lL" -> restart env w { wLex = not (wLex w) }
    | c `elem` "rR" -> restart env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. n を 3 → 4 → 5 → 6 と増やすと、できる数は 6 → 24 → 120 → 720。一覧の長さの増え方を見よう
-- 2. 辞書順（L）では一覧がきれいに並ぶ。Heap ではどう見える？ どちらも全部の順列を 1 回ずつ通る
-- 3. 「1 番の玉が先頭にある順列」だけ数えると (n−1)! になるはず。一覧から数えてみよう
-- 4. ballHue を変えて、2 色（同じ色の玉が 2 個）にしたら「見かけ上ちがう並び」はいくつ？（n!/2!）

-- ---------- 道具 ----------
swapAt :: Int -> Int -> U.Vector Int -> U.Vector Int
swapAt i j v = v U.// [(i, v U.! j), (j, v U.! i)]

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 中心が (x, y) の文字
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s
