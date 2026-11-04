-- ============================================
-- 41-11 ラテン方格と魔方陣
-- ラテン方格：n×n のマスに n 種類の記号を、どの行・どの列にも 1 回ずつ置く（数独のルール）
-- 2 つのラテン方格を重ねてすべてのペアが 1 回ずつ現れるとき「直交」（グレコ・ラテン方格）
-- 直交する 2 つから魔方陣（行・列・対角線の和が全部同じ）がつくれる
-- 操作：↑↓ = n（3〜7）/ 1 = ラテン方格 / 2 = グレコ・ラテン方格 / 3 = 魔方陣 / スペース = 行をシャッフル / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', nub)
import qualified Data.Vector.Unboxed as U

data World = World
  { wN        :: !Int               -- ★ 大きさ
  , wView     :: !Int               -- 1: ラテン方格, 2: 直交ペア, 3: 魔方陣
  , wA, wB    :: !(U.Vector Int)    -- 2 つのラテン方格（0〜n−1、n×n を 1 列に並べた）
  , wRows     :: !(U.Vector Int)    -- 行の並べ替え（シャッフル用）
  , wHasPair  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "41-11 Latin and magic squares" (build (World 5 2 U.empty U.empty U.empty True)))
  { skBack  = hsb 222 28 11
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 直交するラテン方格のペアをつくる
build :: World -> World
build w =
  let n = wN w
      grid f = U.fromList [f i j | i <- [0 .. n - 1], j <- [0 .. n - 1]]
      base = w { wRows = U.enumFromN 0 n, wHasPair = True }
  in if odd n
       then
         -- 奇数 n：A = (−i − j + cA) mod n, B = (i − j + cB) mod n → 直交し、対角線の和もそろう
         let cA = ((n - 3) `div` 2 + n) `mod` n
             cB = (n - 1) `div` 2
         in base { wA = grid (\i j -> (-i - j + cA) `mod` n), wB = grid (\i j -> (i - j + cB) `mod` n) }
       else if n == 4
         then base { wA = U.fromList [0,1,2,3, 2,3,0,1, 3,2,1,0, 1,0,3,2]
                   , wB = U.fromList [0,1,2,3, 3,2,1,0, 1,0,3,2, 2,3,0,1] }
         -- n = 6：直交するペアは存在しない（オイラーの 36 人の士官問題、タリー 1901 年）
         else base { wHasPair = False
                   , wA = grid (\i j -> (i + j) `mod` n), wB = grid (\i j -> (i + 2 * j) `mod` n) }

cellRect :: Env -> Int -> Int -> Int -> (Float, Float, Float)
cellRect env n i j =
  let fn = fromIntegral n
      s = min 78 ((envH env - 220) / fn)
      x0 = envW env * 0.36 - fn * s / 2
      y0 = 130 + ((envH env - 220) - fn * s) / 2
  in (x0 + fromIntegral j * s, y0 + fromIntegral i * s, s)

a', b', magic :: World -> Int -> Int -> Int
a' w i j = wA w U.! (wRows w U.! i * wN w + j)
b' w i j = wB w U.! (wRows w U.! i * wN w + j)
magic w i j = wN w * a' w i j + b' w i j + 1

-- 直交しているか：ペア (A,B) がすべて異なるか
orthogonal :: World -> Bool
orthogonal w =
  let n = wN w
      ps = [a' w i j * n + b' w i j | i <- [0 .. n - 1], j <- [0 .. n - 1]]
  in length (nub ps) == length ps

latinCount :: Int -> String
latinCount m = case m of
  3 -> "12"
  4 -> "576"
  5 -> "161280"
  6 -> "812851200"
  7 -> "61479419904000"
  _ -> "?"

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      view = wView w
      target = n * (n * n + 1) `div` 2
      rng = [0 .. n - 1]
      okColor s = hsb (if s == target then 130 else 0) 80 100
      cell (i, j) =
        let (x, y, s) = cellRect env n i j
            hueA = fromIntegral (a' w i j) * 360 / fromIntegral n
            cx = x + s / 2
            cy = y + s / 2
        in Pictures
             [ box env (if view == 3 then hsb 0 0 22 else hsb hueA 60 85) (x + 1) (y + 1) (s - 2) (s - 2)
             , case view of
                 1 -> txtM env cx cy (s * 0.4) (hsb 0 0 10) (show (a' w i j + 1))
                 2 -> at (toG env cx cy) (Color (hsb 0 0 100) (symbol (b' w i j) (s * 0.21)))
                 _ -> txtM env cx cy (s * 0.36) (hsb hueA 50 100) (show (magic w i j)) ]
      -- 魔方陣：和を表示
      sums
        | view /= 3 = []
        | otherwise =
            let rowSum i = sum [magic w i j | j <- rng]
                colSum j = sum [magic w i j | i <- rng]
                d1 = sum [magic w i i | i <- rng]
                d2 = sum [magic w i (n - 1 - i) | i <- rng]
                (r0x, r0y, _) = cellRect env n 0 0
                (r1x, r1y, r1s) = cellRect env n 0 (n - 1)
            in [ let (x, y, s) = cellRect env n i (n - 1)
                 in txtM env (x + s + 28) (y + s / 2) 14 (okColor (rowSum i)) (show (rowSum i)) | i <- rng ]
               ++ [ let (x, y, s) = cellRect env n (n - 1) j
                    in txtM env (x + s / 2) (y + s + 20) 14 (okColor (colSum j)) (show (colSum j)) | j <- rng ]
               ++ [ txtM env (r0x - 30) (r0y - 16) 14 (okColor d1) ("\\ " ++ show d1)
                  , txtM env (r1x + r1s + 30) (r1y - 16) 14 (okColor d2) (show d2 ++ " /") ]
      px = envW env * 0.66
      py = 130
      ortho = orthogonal w
      plain dy s = txt env px (py + dy) 15 (hsb 0 0 70) s
      panelText = case view of
        1 -> [ txt env px py 20 (hsb 0 0 100) "Latin square (color = A)"
             , plain 34 ("Each row and column has 1 to " ++ show n ++ " once.")
             , plain 56 "Sudoku = Latin + 3x3 block rule."
             , plain 92 ("Latin squares for n = " ++ show n ++ ":")
             , plain 114 (latinCount n)
             , plain 138 "(1, 2, 12, 576, 161280, 812851200, ...)"
             , plain 174 "Space: swap rows, still a Latin square" ]
        2 -> [ txt env px py 20 (hsb 0 0 100) "Graeco-Latin (color A + mark B)"
             , plain 34 "Stack a color square and a mark square."
             , plain 56 ("All " ++ show (n * n) ++ " (color, mark) pairs appear once")
             , txt env px (py + 92) 17 (hsb (if ortho then 130 else 20) 80 100)
                 (if ortho then "Orthogonal: OK" else "NOT orthogonal (a pair repeats)")
             ]
             ++ (if not (wHasPair w)
                   then [ plain 128 "n = 6 has no orthogonal pair!"
                        , plain 150 "(Euler's 36 officers, Tarry 1901)"
                        , plain 172 "All n except 2, 6 have one (solved 1959)" ]
                   else [ plain 128 "Odd n: A = (-i-j), B = (i-j) mod n."
                        , plain 150 "n = 4: hand-made pair (field GF(4))" ])
             ++ [ plain 208 "Experiment design: row = day, col = field,"
                , plain 230 "color = fertilizer, mark = crop: every"
                , plain 252 "combination is tested without bias." ]
        _ -> [ txt env px py 20 (hsb 0 0 100) "Magic square: n*A + B + 1"
             , plain 34 ("Rows and columns hold A 0 to " ++ show (n - 1) ++ " and B once")
             , plain 56 "each -> every sum is the same"
             , plain 92 ("Magic sum = n(n^2+1)/2 = " ++ show target)
             , plain 128 "Diagonals match only if A, B diagonals"
             , plain 150 "are Latin too (green = match, red = no)" ]
             ++ (if not (wHasPair w) then [plain 186 "n = 6 has no pair: this method fails"] else [])
             ++ [ plain 220 "Space: swapped rows break the diagonals" ]
  in Pictures
       [ Pictures (map cell [(i, j) | i <- rng, j <- rng])
       , Pictures sums
       , box env (hsba 0 0 100 0.08) (px - 16) (py - 30) (envW env - px + 6) (envH env - py - 30)
       , Pictures panelText
       , txt env 20 40 16 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   1: Latin   2: Graeco-Latin   3: Magic   Space: shuffle rows   R: reset")
       , txt env 20 68 14 (hsb 0 0 60)
           "Latin square = Sudoku rule. Orthogonal pairs are used in experiments, cryptography and error correction."
       ]

-- 7 種類の記号（文字の代わりに図形で描く）
symbol :: Int -> Float -> Picture
symbol k r = case k of
  0 -> disc r
  1 -> Polygon [(0, r), (r, -r * 0.8), (-r, -r * 0.8)]
  2 -> rect 0 0 (r * 1.7) (r * 1.7)
  3 -> Polygon [(0, r * 1.1), (r * 1.1, 0), (0, -r * 1.1), (-r * 1.1, 0)]
  4 -> starShape r
  5 -> Pictures [rect 0 0 (r * 2) (r * 0.6), rect 0 0 (r * 0.6) (r * 2)]
  _ -> Pictures
         [ Translate (-r * 0.48) (r * 0.35) (disc (r * 0.55))
         , Translate (r * 0.48) (r * 0.35) (disc (r * 0.55))
         , Polygon [(-r * 1.0, r * 0.2), (r * 1.0, r * 0.2), (0, -r * 1.05)] ]

-- 星はへこんだ形なので、中心から 10 個の三角形に分けて塗る
starShape :: Float -> Picture
starShape r =
  let p i = let a = pi / 2 + fromIntegral (i :: Int) * pi / 5
                rr = if even i then r * 1.15 else r * 0.5
            in (rr * cos a, rr * sin a)
  in Pictures [Polygon [(0, 0), p i, p (i + 1)] | i <- [0 .. 9]]

-- Fisher–Yates で行を並べかえる
shuffleRows :: World -> IO World
shuffleRows w = do
  v <- foldl' (\acc i -> do
                  cur <- acc
                  j <- randomRIO (0, i)
                  pure (cur U.// [(i, cur U.! j), (j, cur U.! i)]))
              (pure (wRows w))
              [wN w - 1, wN w - 2 .. 1]
  pure w { wRows = v }

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 7 -> pure (build w { wN = wN w + 1 })
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 3 -> pure (build w { wN = wN w - 1 })
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> pure (build w)
  EventKey (SpecialKey KeySpace) Down _ _ -> shuffleRows w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 3 のラテン方格は 12 個。行の入れかえ（スペース）だけで何種類できる？（3! = 6）
-- 2. 魔方陣モードで行をシャッフルすると、行・列の和はそのまま、対角線だけくずれる。なぜ？
-- 3. n = 6 で 2 を押す：このペアの B は同じ行に同じ記号が 2 回出て、ラテン方格ではない。どの行か探そう
-- 4. 偶数 n = 4 のペア A, B をクリックで編集できるようにして、直交チェックが OK のまま保てるか試そう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 中心が (x, y) の文字（大きさは 14 より小さくしない）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y size c s = let (gx, gy) = toG env x y in labelC gx gy (max 14 size) c s
