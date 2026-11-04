-- ============================================
-- 41-01 組合せと二項係数
-- n 個から k 個を「選ぶ」（順番は関係なし）やり方は C(n,k) = n! / (k!(n−k)!) 通り
-- 全部のパターンを並べて数え、C(n,k) = C(n−1,k−1) + C(n−1,k) を色分けで確かめる
-- 操作：↑↓ = n（1〜8）/ ←→ = k / H = 漸化式の色分け / C = 補集合（選ばなかった方）を見る
-- ============================================
module Main (main) where

import Art

startN, startK :: Int
startN = 5             -- ★ 最初の n
startK = 2             -- ★ 最初の k

data World = World
  { wN, wK    :: !Int
  , wSubsets  :: ![[Int]]   -- 選び方の一覧（0/1 のリスト）
  , wRecur    :: !Bool      -- 漸化式の色分け
  , wComp     :: !Bool      -- 補集合表示
  }

main :: IO ()
main = runSketch (sketch "41-01 Combinations" (rebuild (World startN startK [] True False)))
  { skBack  = hsb 225 25 11
  , skDraw  = draw
  , skEvent = event
  }

-- ★ k 個選ぶパターンを辞書順に全部つくる（再帰）
rebuild :: World -> World
rebuild w = w { wSubsets = map toRow (rec 0 (wK w)) }
  where
    n = wN w
    rec start left
      | left == 0 = [[]]
      | otherwise = [i : rest | i <- [start .. n - left], rest <- rec (i + 1) (left - 1)]
    toRow cur = [if i `elem` cur then 1 else 0 | i <- [0 .. n - 1]]

choose :: Int -> Int -> Int
choose a b
  | b < 0 || b > a = 0
  | otherwise = product [a - b + 1 .. a] `div` product [1 .. b]

hasLast :: [Int] -> Bool
hasLast s = foldl (\_ x -> x) 0 s == 1

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      k = wK w
      total = length (wSubsets w)
      -- 漸化式：最後の要素（n 番目）を含むか含まないかで 2 グループ
      withLast = filter hasLast (wSubsets w)
      without = filter (not . hasLast) (wSubsets w)
      list = if wRecur w then withLast ++ without else wSubsets w
      -- レイアウト
      cell = if n <= 6 then 16 else 13 :: Float
      rowH = cell + 6
      tileW = fromIntegral n * cell + 30
      x0 = 30
      y0 = 200 :: Float
      perCol = max 1 (floor ((envH env - y0 - 30) / rowH)) :: Int
      px = envW env - 440     -- 右パネルの左端（英語の幅に合わせて広めにしている）
      py = 200
      tile (idx, s) =
        let col = idx `div` perCol
            row = idx `mod` perCol
            x = x0 + fromIntegral col * tileW
            y = y0 + fromIntegral row * rowH
            ch = if wRecur w then (if hasLast s then 35 else 185) else 45
            one (i, v) =
              let chosen = if wComp w then 1 - v else v
                  c = if chosen == 1 then hsb ch 75 100 else hsb 0 0 30
              in box env c (x + fromIntegral (i :: Int) * cell) y (cell - 2) (cell - 2)
        in if x > px - 20 then Blank else Pictures (map one (zip [0 ..] s))
      rowVals = [choose n j | j <- [0 .. n]]
      widths = [textW 14 (show v) + 14 | v <- rowVals]
      starts = scanl (\a b -> a + b + 6) px widths
      pascal (j, v, cx, wd)
        | j == k = Pictures
            [ box env (hsb 45 80 100) (cx - 4) (py + 226) wd 24
            , txt env (cx + 3) (py + 244) 14 (hsb 0 0 10) (show v) ]
        | otherwise = txt env (cx + 3) (py + 244) 14 (hsb 0 0 80) (show (v :: Int))
  in Pictures
       [ Pictures (map tile (zip [0 :: Int ..] list))
       -- 右パネル：数と式
       , box env (hsba 0 0 100 0.08) (px - 20) (py - 40) 430 340
       , txt env px (py + 10) 34 (hsb 0 0 100) ("C(" ++ show n ++ "," ++ show k ++ ") = " ++ show total)
       , txt env px (py + 44) 14 (hsb 0 0 70)
           (show n ++ "! / (" ++ show k ++ "! x " ++ show (n - k) ++ "!) = " ++ show (fact n)
            ++ " / (" ++ show (fact k) ++ " x " ++ show (fact (n - k)) ++ ")")
       , if wRecur w then Pictures
           [ txt env px (py + 86) 15 (hsb 35 75 100)
               ("# With " ++ show n ++ ": C(" ++ show (n - 1) ++ "," ++ show (k - 1) ++ ") = " ++ show (length withLast))
           , txt env px (py + 110) 15 (hsb 185 75 100)
               ("# Without " ++ show n ++ ": C(" ++ show (n - 1) ++ "," ++ show k ++ ") = " ++ show (length without))
           , txt env px (py + 134) 15 (hsb 0 0 100)
               ("Sum: " ++ show (length withLast) ++ " + " ++ show (length without) ++ " = " ++ show total)
           ] else Blank
       , txt env px (py + 176) 15 (hsb 0 0 70)
           ("Symmetry: C(" ++ show n ++ "," ++ show (n - k) ++ ") = " ++ show (choose n (n - k))
            ++ (if wComp w then " (complement)" else ""))
       , txt env px (py + 214) 15 (hsb 0 0 100) ("Pascal's triangle, row " ++ show n ++ ":")
       , Pictures (map pascal (zip4' [0 ..] rowVals starts widths))
       , txt env px (py + 280) 15 (hsb 0 0 70)
           ("Total " ++ show ((2 :: Int) ^ n) ++ " = 2^" ++ show n ++ " (all selections)")
       -- 説明
       , txt env 20 40 18 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   k = " ++ show k ++ " (Left/Right)   H: recurrence colors "
            ++ onOff (wRecur w) ++ "   C: complement " ++ onOff (wComp w))
       , txt env 20 68 14 (hsb 0 0 60)
           "One row = one selection (bright = chosen). With or without the last ball: C(n-1,k-1) + C(n-1,k)"
       , txt env 20 92 14 (hsb 0 0 60)
           "Complement (C): look at the balls NOT chosen to see why C(n,k) = C(n,n-k)"
       ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 8 -> rebuild w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 1 ->
    rebuild w { wN = wN w - 1, wK = min (wK w) (wN w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ | wK w < wN w -> rebuild w { wK = wK w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wK w > 0 -> rebuild w { wK = wK w - 1 }
  EventKey (Char c) Down _ _
    | c `elem` "hH" -> w { wRecur = not (wRecur w) }
    | c `elem` "cC" -> w { wComp = not (wComp w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 6 で k を 0 → 6 と動かす。数が 1, 6, 15, 20, 15, 6, 1 と左右対称になる理由を C で確かめよう
-- 2. k = 2 の C(n,2) = n(n−1)/2。「握手の数」「対角線＋辺の数」と同じ。n を変えて確かめよう
-- 3. rebuild を書きかえて、「隣どうしを選ばない」組合せだけ数えると？（フィボナッチ数が出る）
-- 4. 選んだ k 個をさらに並べる（順列）と n!/(n−k)! 通り。00 とくらべて k! 倍になる理由は？

-- ---------- 道具 ----------
fact :: Int -> Int
fact m = product [1 .. m]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

-- 文字の幅のおおよその見積もり
textW :: Float -> String -> Float
textW size s = fromIntegral (length s) * size * 0.85

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
