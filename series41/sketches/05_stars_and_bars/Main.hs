-- ============================================
-- 41-05 重複組合せ（星と棒）
-- 同じ n 個の星を k 個の箱に分ける方法（空の箱もOK）は C(n+k−1, k−1) 通り
-- 「**|*|***」のように、星 n 個と棒 k−1 本の並べ方として数える
-- 操作：↑↓ = 星の数 n（1〜8）/ ←→ = 箱の数 k（1〜5）/ スペース = 一覧を順に見る / クリック = 一覧から選ぶ
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')

data World = World
  { wN, wK      :: !Int        -- ★ 星と箱の数
  , wSolutions  :: ![[Int]]    -- 各要素は [箱1の個数, 箱2, …]
  , wSel        :: !Int        -- いま注目している並び
  , wAuto       :: !Bool
  , wTick       :: !Float
  , wListPic    :: Picture     -- 一覧の絵（作り直すまで使い回す）
  }

main :: IO ()
main = runSketch (sketch "41-05 Stars and bars" (World 5 3 [] 0 True 0 Blank))
  { skBack  = hsb 228 28 11
  , skStart = \env -> pure (rebuild env (World 5 3 [] 0 True 0 Blank))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ★ n 個を k 個の箱に分ける全パターン（再帰）
partitionsInto :: Int -> Int -> [[Int]]
partitionsInto left boxes
  | boxes == 1 = [[left]]
  | otherwise = [i : rest | i <- [0 .. left], rest <- partitionsInto (left - i) (boxes - 1)]

rebuild :: Env -> World -> World
rebuild env w =
  let w1 = w { wSolutions = partitionsInto (wN w) (wK w), wSel = 0 }
  in w1 { wListPic = drawList env w1 }

choose :: Int -> Int -> Int
choose a b = product [a - b + 1 .. a] `div` product [1 .. b]

-- 一覧の配置：(マスの大きさ, タイルの幅, タイルの高さ, 1 行の数)
listX, listY :: Float
listX = 30
listY = 330

-- 画面の高さに収まる、いちばん大きなマスを選ぶ
tileLayout :: Env -> World -> (Float, Float, Float, Int)
tileLayout env w =
  let count = length (wSolutions w)
      fit cell =
        let tileW = fromIntegral (wN w + wK w - 1) * cell + 14
            tileH = cell + 5
            perRow = max 1 (floor ((envW env * 0.62 - 40) / tileW)) :: Int
            rows = (count + perRow - 1) `div` perRow
        in (cell, tileW, tileH, perRow, listY + fromIntegral rows * tileH <= envH env - 16)
      start | count > 200 = [6, 5, 4, 3]
            | count > 60 = [9, 8, 7, 6, 5, 4, 3]
            | otherwise = [13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3]
      pick xs = case xs of
        [] -> (3, 3 * fromIntegral (wN w + wK w - 1) + 14, 8, 1)
        [c] -> let (a, b, d, e, _) = fit c in (a, b, d, e)
        (c : r) -> let (a, b, d, e, ok) = fit c in if ok then (a, b, d, e) else pick r
  in pick start

tilePos :: Env -> World -> Int -> Point
tilePos env w idx =
  let (_, tileW, tileH, perRow) = tileLayout env w
  in (listX + fromIntegral (idx `mod` perRow) * tileW, listY + fromIntegral (idx `div` perRow) * tileH)

drawList :: Env -> World -> Picture
drawList env w =
  let (cell, _, _, _) = tileLayout env w
      k = wK w
      tile (idx, s) =
        let (px, py) = tilePos env w idx
            -- 左から順に星と棒を置いていく
            go (x, acc) (b, cnt) =
              let stars = [box env (hsb 45 80 100) (x + fromIntegral j * cell) py (cell - 1) (cell - 1) | j <- [0 .. cnt - 1]]
                  x1 = x + fromIntegral cnt * cell
              in if b < k - 1
                   then (x1 + cell, box env (hsb 200 60 90) (x1 + cell * 0.3) py (cell * 0.35) (cell - 1) : stars ++ acc)
                   else (x1, stars ++ acc)
        in Pictures (snd (foldl' go (px, []) (zip [0 :: Int ..] s)))
  in Pictures (map tile (zip [0 :: Int ..] (wSolutions w)))

step :: Env -> Float -> World -> IO World
step _ dt w
  | not (wAuto w) = pure w
  | wTick w + dt > 0.5 = pure w { wTick = 0, wSel = (wSel w + 1) `mod` max 1 (length (wSolutions w)) }
  | otherwise = pure w { wTick = wTick w + dt }

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      k = wK w
      wd = envW env
      hg = envH env
      sols = wSolutions w
      s = case drop (wSel w) sols of
        (x : _) -> x
        [] -> []
      (_, tileW, tileH, _) = tileLayout env w
      (tx, ty) = tilePos env w (wSel w)
      -- 上：選んだ分け方を箱で描く
      bx = 40
      by = 120
      fk = fromIntegral k :: Float
      bw = min 150 ((wd * 0.6 - 16 * (fk - 1)) / fk)
      bh = 150
      boxPic (b, cnt) =
        let x = bx + fromIntegral (b :: Int) * (bw + 16)
        in Pictures
             [ Color (hsb 0 0 60) (frameBox env 2 x by bw bh)
             , Color (hsb 45 80 100) (Pictures
                 [ star env (x + 22 + fromIntegral (j `mod` 4) * 30) (by + bh - 24 - fromIntegral (j `div` 4) * 30) 11
                 | j <- [0 .. cnt - 1 :: Int] ])
             , txt env (x + 8) (by + 22) 16 (hsb 0 0 100) ("Box " ++ show (b + 1) ++ ": " ++ show cnt) ]
      -- 星と棒の文字列
      str = concat [replicate c '*' ++ (if b < k - 1 then "|" else "") | (b, c) <- zip [0 ..] s]
      -- 右：ヒストグラム（箱 1 に i 個入る分け方の数）
      hx = wd * 0.66
      hy = 330
      hw = wd - hx - 40
      hh = hg - hy - 60
      counts = [length [() | (c : _) <- sols, c == i] | i <- [0 .. n]]
      maxC = foldl' max 1 counts
      bwid = hw / fromIntegral (n + 1)
      first = case s of
        (c : _) -> c
        [] -> -1
      hist (i, c) =
        let hgt = fromIntegral c / fromIntegral maxC * (hh - 40) :: Float
            x = hx + fromIntegral i * bwid
        in Pictures
             [ box env (hsb (if i == first then 330 else 200) 70 95) (x + 4) (hy + hh - 20 - hgt) (bwid - 8) hgt
             , txtC env (x + bwid / 2) (hy + hh - 24 - hgt) 14 (hsb 0 0 80) (show c)
             , txtC env (x + bwid / 2) (hy + hh) 14 (hsb 0 0 80) (show i) ]
      px = wd * 0.66
      py = 110
      total = length sols
      a = n + k - 1
  in Pictures
       [ wListPic w
       , Color (hsb 330 80 100) (frameBox env 2 (tx - 3) (ty - 3) (tileW - 8) (tileH + 1))
       , Pictures (map boxPic (zip [0 ..] s))
       , txt env bx (by + bh + 36) 24 (hsb 0 0 100) str
       , Pictures (map hist (zip [0 :: Int ..] counts))
       , txt env hx (hy - 10) 14 (hsb 0 0 70)
           ("Ways with i in box 1 (" ++ show n ++ "-i left for " ++ show (k - 1) ++ " boxes)")
       , txt env px (py + 10) 28 (hsb 0 0 100) ("C(" ++ show a ++ "," ++ show (k - 1) ++ ") = " ++ show total)
       , txt env px (py + 44) 15 (hsb 0 0 70) (show n ++ " stars + " ++ show (k - 1) ++ " bars = " ++ show a ++ " symbols")
       , txt env px (py + 66) 15 (hsb 0 0 70) ("Place bars: choose " ++ show (k - 1) ++ " of " ++ show a ++ " spots")
       , txt env px (py + 88) 15 (hsb 0 0 70) ("Or place stars: C(" ++ show a ++ "," ++ show n ++ ") = " ++ show (choose a n))
       , txt env px (py + 120) 15 (hsb 0 0 70) ("The list has " ++ show total ++ " items: it matches")
       , txt env 20 40 16 (hsb 0 0 100)
           ("n = " ++ show n ++ " stars (Up/Dn)  k = " ++ show k ++ " boxes (L/R)  Space: auto "
            ++ (if wAuto w then "ON" else "off") ++ "  Click: pick  [" ++ show (wSel w + 1) ++ " / " ++ show total ++ "]")
       , txt env 20 68 14 (hsb 0 0 60)
           "Same stars into boxes = choosing n from k kinds with repeats = orderings of stars and bars"
       ]

-- 5 本のとげの星（へこんだ形なので、中心から 10 個の三角形に分けて塗る）
star :: Env -> Float -> Float -> Float -> Picture
star env x y r =
  let p i = let a = -pi / 2 + fromIntegral (i :: Int) * pi / 5
                rr = if even i then r else r * 0.45
            in toG env (x + rr * cos a) (y + rr * sin a)
      c = toG env x y
  in Pictures [Polygon [c, p i, p (i + 1)] | i <- [0 .. 9]]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 8 -> rebuild env w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 1 -> rebuild env w { wN = wN w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ | wK w < 5 -> rebuild env w { wK = wK w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wK w > 1 -> rebuild env w { wK = wK w - 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        (_, tileW, tileH, _) = tileLayout env w
        hit idx = let (x, y) = tilePos env w idx
                  in mx >= x - 3 && mx < x + tileW - 6 && my >= y - 3 && my < y + tileH
    in case filter hit [0 .. length (wSolutions w) - 1] of
         (i : _) -> w { wSel = i, wAuto = False }
         [] -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. k = 2 のとき分け方は n + 1 通り。k = 3 では (n+1)(n+2)/2 通り（三角数）。確かめよう
-- 2. 「どの箱も 1 個以上」にすると C(n−1, k−1) 通り。partitionsInto の i を 1 から始めて数えてみよう
-- 3. ヒストグラムの形は k によって変わる。k = 2 なら平ら、k が大きいと右下がり。なぜ？
-- 4. 箱を「x + y + z = n の非負整数解」と読みかえると、方程式の解の数を数えていることになる

-- ---------- 道具 ----------
-- 文字の幅のおおよその見積もり
textW :: Float -> String -> Float
textW size s = fromIntegral (length s) * size * 0.85

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 太さ t の枠
frameBox :: Env -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env t x y w h =
  let a = toG env x y
      b = toG env (x + w) y
      c = toG env (x + w) (y + h)
      d = toG env x (y + h)
  in thickPath t [a, b, c, d, a]

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 横は中央ぞろえ、y は文字の下
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - textW size s / 2) y size c s
