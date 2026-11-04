-- ============================================
-- 41-09 整数分割とヤング図形
-- n を正の整数の和に分ける方法（順番はちがっても同じ）の数 p(n)：4 = 4 = 3+1 = 2+2 = 2+1+1 = 1+1+1+1 → p(4) = 5
-- 各分割はヤング図形（箱を左づめに積む）で描ける。転置すると「共役分割」になる
-- 操作：↑↓ = n（1〜14）/ ←→ or クリック = 分割を選ぶ / T = 転置アニメ / スペース = 自動で次へ
-- ============================================
module Main (main) where

import Art
import Data.List (group, intercalate)

data World = World
  { wN      :: !Int         -- ★ 分割する数
  , wParts  :: ![[Int]]     -- 分割の一覧（各要素は降順のリスト）
  , wSel    :: !Int
  , wAuto   :: !Bool
  , wTick   :: !Float
  , wFlip   :: !Float       -- 0 = もとの図形, 1 = 転置（アニメで 0↔1）
  , wTarget :: !Float
  }

main :: IO ()
main = runSketch (sketch "41-09 Integer partitions" (rebuild (World 6 [] 0 True 0 0 0)))
  { skBack  = hsb 224 28 11
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 分割を全部つくる：大きい数から順に（降順）
rebuild :: World -> World
rebuild w = w { wParts = rec (wN w) (wN w), wSel = 0, wFlip = 0, wTarget = 0 }
  where
    rec :: Int -> Int -> [[Int]]
    rec left maxPart
      | left == 0 = [[]]
      | otherwise = [p : rest | p <- [min left maxPart, min left maxPart - 1 .. 1], rest <- rec (left - p) p]

-- 共役分割：列の長さを読む
conjugate :: [Int] -> [Int]
conjugate lam = case lam of
  [] -> []
  (l0 : _) -> [length (filter (> j) lam) | j <- [0 .. l0 - 1]]

-- ★ 五角数定理による p(n)：p(n) = Σ (−1)^{k+1} [ p(n − k(3k−1)/2) + p(n − k(3k+1)/2) ]
partitionNumbers :: [Int]
partitionNumbers = map p [0 ..]
  where
    p :: Int -> Int
    p 0 = 1
    p m = sum (takeWhile' m [1 ..])
    takeWhile' m (k : ks)
      | g1 > m = []
      | otherwise =
          sign * val (m - g1) : (if g2 <= m then [sign * val (m - g2)] else []) ++ takeWhile' m ks
      where
        g1 = k * (3 * k - 1) `div` 2
        g2 = k * (3 * k + 1) `div` 2
        sign = if odd k then 1 else -1
    takeWhile' _ [] = []
    val i = case drop i partitionNumbers of
      (x : _) -> x
      [] -> 0

-- 一覧の 1 行の高さ
rowHeight :: World -> Float
rowHeight w
  | length (wParts w) > 60 = 16
  | length (wParts w) > 30 = 20
  | otherwise = 26

colWidth :: Float
colWidth = 170

-- 列の幅に入らない分割は 2^3（2 が 3 個）の形にちぢめて書く
partText :: [Int] -> String
partText lam =
  let full = intercalate " + " (map show lam)
      short = intercalate "+" (map squash (group lam))
      squash g = case g of
        (v : _) | length g > 1 -> show v ++ "^" ++ show (length g)
                | otherwise -> show v
        [] -> ""
  in if fromIntegral (length full) * 14 * 0.85 > colWidth - 12 then short else full

step :: Env -> Float -> World -> IO World
step _ dt w =
  let w1 = w { wFlip = wFlip w + (wTarget w - wFlip w) * 0.1 }
  in pure $
       if not (wAuto w1) then w1
       else if wTick w1 + dt > 1 then w1 { wTick = 0, wSel = (wSel w1 + 1) `mod` max 1 (length (wParts w1)) }
       else w1 { wTick = wTick w1 + dt }

draw :: Env -> World -> Picture
draw env w =
  let parts = wParts w
      lam = case drop (wSel w) parts of
        (x : _) -> x
        [] -> []
      l0 = case lam of
        (x : _) -> x
        [] -> 1
      conj = conjugate lam
      -- 一覧（左）
      lx = 30
      ly = 110
      rowH = rowHeight w
      perCol = max 1 (floor ((envH env - ly - 30) / rowH)) :: Int
      entry (i, p) =
        let c = i `div` perCol
            r = i `mod` perCol
            x = lx + fromIntegral c * colWidth
            y = ly + fromIntegral r * rowH
            isSelf = p == conjugate p
        in if x > envW env * 0.45 then Blank else Pictures
             [ if i == wSel w then box env (hsba 50 80 100 0.3) (x - 6) (y - rowH + 4) (colWidth - 10) rowH else Blank
             , txt env x y 14 (if isSelf then hsb 330 60 100 else hsb 0 0 85) (partText p) ]
      -- ヤング図形（中央）：転置アニメ（箱 (i,j) → (j,i) へ移動）
      big = fromIntegral (max (length lam) l0) :: Float
      cell = min 40 (360 / big)
      gx = envW env * 0.52
      gy = 160
      f = wFlip w
      square i j =
        let fi = fromIntegral (i :: Int)
            fj = fromIntegral (j :: Int)
            x = gx + (fj * (1 - f) + fi * f) * cell
            y = gy + (fi * (1 - f) + fj * f) * cell
        in box env (hsb (wrapF 360 (fi * 40 + 180)) 55 95) x y (cell - 2) (cell - 2)
      below = gy + big * cell + 30
      pn = partitionNumbers
      tableLine a b = intercalate "  " [show i ++ ":" ++ show v | (i, v) <- zip [a .. b :: Int] (drop a pn)]
      ty = envH env - 150
  in Pictures
       [ Pictures (map entry (zip [0 :: Int ..] parts))
       , Pictures [square i j | (i, v) <- zip [0 ..] lam, j <- [0 .. v - 1]]
       , txt env gx (gy - 20) 16 (hsb 0 0 100)
           ("lambda = (" ++ commas lam ++ ")  " ++ show (wN w) ++ " boxes, " ++ show (length lam) ++ (if length lam == 1 then " row" else " rows"))
       , txt env gx below 14 (hsb 0 0 70)
           ("Conjugate = (" ++ commas conj ++ ")" ++ (if lam == conj then "  self-conjugate!" else ""))
       , txt env gx (below + 22) 14 (hsb 0 0 70) "T: swap rows and columns. k rows <-> largest part k"
       -- p(n) の表（右下）
       , txt env gx ty 14 (hsb 0 0 85) ("p(n)  " ++ tableLine 0 6)
       , txt env gx (ty + 20) 14 (hsb 0 0 85) (tableLine 7 13)
       , txt env gx (ty + 40) 14 (hsb 0 0 85) (tableLine 14 20)
       , txt env gx (ty + 66) 14 (hsb 0 0 60) "Pentagonal number theorem: p(n) = p(n-1) + p(n-2)"
       , txt env gx (ty + 86) 14 (hsb 0 0 60) "  - p(n-5) - p(n-7) + p(n-12) + p(n-15) - ..."
       , txt env gx (ty + 106) 14 (hsb 0 0 60) "1,2,5,7,12,15 = k(3k-+1)/2. Signs flip every 2."
       , txt env 20 40 16 (hsb 0 0 100)
           ("n = " ++ show (wN w) ++ " (Up/Down)  p(" ++ show (wN w) ++ ") = " ++ show (length parts)
            ++ "  [" ++ show (wSel w + 1) ++ " / " ++ show (length parts) ++ "] L/R or click  T: transpose  Space: auto "
            ++ (if wAuto w then "ON" else "off"))
       , txt env 20 68 14 (hsb 0 0 60)
           "Pink = self-conjugate. 2^3 means 2+2+2. p(n) also comes from the product of 1/(1-x^k) (sketch 04)."
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 14 -> rebuild w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 1 -> rebuild w { wN = wN w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` len, wAuto = False }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w - 1 + len) `mod` len, wAuto = False }
  EventKey (Char c) Down _ _ | c `elem` "tT" -> w { wTarget = 1 - wTarget w }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        ly = 110
        rowH = rowHeight w
        perCol = max 1 (floor ((envH env - ly - 30) / rowH)) :: Int
        c = floor ((mx - 24) / colWidth) :: Int
        r = floor ((my - ly + rowH - 6) / rowH) :: Int
        i = c * perCol + r
    in if c >= 0 && r >= 0 && r < perCol && i < length (wParts w) && mx < envW env * 0.45
         then w { wSel = i, wAuto = False } else w
  _ -> w
  where len = max 1 (length (wParts w))

-- ---------- やってみよう ----------
-- 1. n = 5 の 7 通りを順に転置し、どの分割がどれと対になるか表にしよう。自己共役はいくつ？
-- 2. 「相異なる数だけの分割」と「奇数だけの分割」の数は同じ（オイラー）。n = 7 で両方数えてみよう
-- 3. 行数が 2 本以下の分割の数は floor(n/2)+1。一覧の中から数えて確かめよう
-- 4. rebuild の rec を書きかえて「各数は k 以下」という条件をつけると、転置で「行数 k 以下」と同じ数になる

-- ---------- 道具 ----------
commas :: [Int] -> String
commas = intercalate ", " . map show

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
