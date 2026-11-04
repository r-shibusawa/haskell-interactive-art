-- ============================================
-- 41-08 カタラン数
-- Dyck 経路（対角線を越えない格子路）・正しい括弧列・二分木・多角形の三角形分割は
-- どれも同じ数 C_n = 1, 1, 2, 5, 14, 42, … で数えられる（全部が 1 対 1 に対応する）
-- 操作：↑↓ = n（1〜5）/ ←→ or クリック = どれを見るか / スペース = 自動で次へ
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate)

data Tree = Leaf | Node !Tree !Tree !Int    -- 左、右、大きさ（内部節点の数）

data World = World
  { wN     :: !Int        -- ★ 対の数（括弧 n 組、辺 2n の経路、内部節点 n 個の木）
  , wItems :: ![String]   -- 括弧列の一覧（辞書順）
  , wSel   :: !Int
  , wAuto  :: !Bool
  , wTick  :: !Float
  }

main :: IO ()
main = runSketch (sketch "41-08 Catalan numbers" (rebuild (World 3 [] 0 True 0)))
  { skBack  = hsb 226 28 11
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 正しい括弧列を全部つくる（開き > 閉じ を守りながら再帰）
rebuild :: World -> World
rebuild w = w { wItems = rec "" 0 0, wSel = 0 }
  where
    n = wN w
    rec :: String -> Int -> Int -> [String]
    rec s open close
      | length s == 2 * n = [reverse s]
      | otherwise =
          (if open < n then rec ('(' : s) (open + 1) close else [])
          ++ (if close < open then rec (')' : s) open (close + 1) else [])

catalan :: [Int]
catalan = map c [0 ..]
  where c m = if m == 0 then 1 else sum [atI catalan j * atI catalan (m - 1 - j) | j <- [0 .. m - 1]]

-- 括弧列 → 二分木：'(' 左部分木 ')' 右部分木
parseTree :: String -> (Tree, String)
parseTree ('(' : s) =
  let (l, r1) = parseTree s
      (r, r2) = parseTree (drop 1 r1)    -- ')' を読みとばす
  in (Node l r (1 + size l + size r), r2)
parseTree s = (Leaf, s)

size :: Tree -> Int
size Leaf = 0
size (Node _ _ k) = k

step :: Env -> Float -> World -> IO World
step _ dt w
  | not (wAuto w) = pure w
  | wTick w + dt > 1.2 = pure w { wTick = 0, wSel = (wSel w + 1) `mod` max 1 (length (wItems w)) }
  | otherwise = pure w { wTick = wTick w + dt }

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      fn = fromIntegral n :: Float
      items = wItems w
      s = case drop (wSel w) items of
        (x : _) -> x
        [] -> ""
      wd = envW env
      -- 一覧（上の帯）：小さな Dyck 経路
      tileW = tileWidth env w
      ty = 128
      tile (i, it) =
        let x = 30 + fromIntegral i * tileW
            sel = i == wSel w
        in Pictures
             [ if sel then box env (hsba 50 80 100 0.25) (x - 4) (ty - 44) (tileW - 2) 60 else Blank
             , drawDyck env n it x (ty + 8) (min (min (tileW - 10) 80 / (2 * fn)) (44 / fn)) (if sel then 2 else 1) ]
      -- 4 つの表現
      colW = (wd - 80) / 4
      top = 220
      boxH = envH env - top - 60
      titles = ["Dyck path", "Parentheses", "Binary tree: ( L ) R", "Triangulation (" ++ show (n + 2) ++ "-gon)"]
      panel (c, title) =
        let x = 40 + fromIntegral (c :: Int) * colW
        in Pictures
             [ box env (hsba 0 0 100 0.06) x top (colW - 16) boxH
             , txt env (x + 12) (top + 24) 14 (hsb 0 0 85) title ]
      -- 1: Dyck
      u = min ((colW - 60) / (2 * fn)) 36
      -- 2: 括弧列（深さを表示）
      bx = 40 + colW + 20
      by = top + 80
      cw = (colW - 50) / (2 * fn)
      fontS = min 40 (cw * 1.6)
      parens = goParen 0 0 s
      goParen :: Int -> Int -> String -> [Picture]
      goParen _ _ [] = []
      goParen i depth (ch : rest) =
        let depth1 = if ch == '(' then depth + 1 else depth
            pic = txt env (bx + fromIntegral i * cw) (by + fromIntegral depth1 * 22) fontS
                    (hsb (if ch == '(' then 140 else 20) 70 100) [ch]
            depth2 = if ch == ')' then depth1 - 1 else depth1
        in pic : goParen (i + 1) depth2 rest
      -- 3: 二分木、4: 三角形分割
      (tree, _) = parseTree s
      cx = 40 + 3 * colW + (colW - 16) / 2
      cy = top + boxH / 2 + 10
      bigR = min colW boxH * 0.36
      c7 = take 7 catalan
      recNames = [ "C_" ++ show j ++ "C_" ++ show (n - 1 - j) | j <- [0 .. n - 1] ]
      recVals = [ atI catalan j * atI catalan (n - 1 - j) | j <- [0 .. n - 1] ]
  in Pictures
       [ Pictures (map tile (zip [0 :: Int ..] items))
       , Pictures (map panel (zip [0 ..] titles))
       , drawDyck env n s (40 + 20) (top + boxH - 60) u 5
       , Pictures parens
       , txt env bx (top + boxH - 30) 14 (hsb 0 0 60) "Any prefix: #( >= #)"
       , drawTree env tree (40 + 2 * colW + (colW - 16) / 2) (top + 70) ((colW - 40) / 4) 60
       , drawTriangulation env n tree cx cy bigR
       , txt env 20 28 18 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   C_" ++ show n ++ " = " ++ show (length items)
            ++ "   [" ++ show (wSel w + 1) ++ " / " ++ show (length items) ++ "] L/R or click   Space: auto "
            ++ (if wAuto w then "ON" else "off"))
       , txt env 20 52 14 (hsb 0 0 60)
           ("C_" ++ show n ++ " = " ++ intercalate " + " recNames ++ " = " ++ intercalate " + " (map show recVals)
            ++ " = " ++ show (atI catalan n))
       , txt env 20 74 14 (hsb 0 0 60)
           ("C_n = sum C_j C_(n-1-j) = C(2n,n)/(n+1): " ++ intercalate ", " (map show c7) ++ " ...")
       , txt env 20 170 14 (hsb 0 0 50)
           "The first ( and its matching ) split the rest into inside (j pairs) and after (n-1-j pairs)."
       ]

-- 一覧のタイルの幅
tileWidth :: Env -> World -> Float
tileWidth env w = min 90 ((envW env - 60) / fromIntegral (max 1 (length (wItems w))))

drawDyck :: Env -> Int -> String -> Float -> Float -> Float -> Float -> Picture
drawDyck env n s x0 y0 u sw =
  let pts = scanl (\(x, y) ch -> (x + u, if ch == '(' then y - u else y + u)) (x0, y0) s
      gpts = map (uncurry (toG env)) pts
      base = Color (hsb 0 0 35) (Line [toG env x0 y0, toG env (x0 + 2 * fromIntegral n * u) y0])
      path = Color (hsb 50 80 100) (if sw > 1 then thickPath sw gpts else Line gpts)
      dots = if sw > 2 then Color (hsb 0 0 100) (Pictures [at p (disc 4) | p <- gpts]) else Blank
  in Pictures [base, path, dots]

drawTree :: Env -> Tree -> Float -> Float -> Float -> Float -> Picture
drawTree _ Leaf _ _ _ _ = Blank
drawTree env (Node l r _) x y dx dy =
  let edge tx ty = Color (hsb 0 0 60) (thickLine 2 (toG env x y) (toG env tx ty))
      leaf (Leaf) sx = box env (hsb 0 0 50) (x + sx * dx * 0.5 - 5) (y + dy * 0.6 - 5) 10 10
      leaf _ _ = Blank
      child Leaf _ = Blank
      child t sx = Pictures
        [ edge (x + sx * dx) (y + dy)
        , drawTree env t (x + sx * dx) (y + dy) (dx * 0.55) dy ]
  in Pictures
       [ child l (-1), child r 1
       , at (toG env x y) (Color (hsb 140 70 100) (disc 9))
       , leaf l (-1), leaf r 1 ]

-- 二分木 → 三角形分割：辺 (a,b) を底辺にもつ三角形の頂点 = a + 左部分木の大きさ + 1
drawTriangulation :: Env -> Int -> Tree -> Float -> Float -> Float -> Picture
drawTriangulation env n tree cx cy bigR =
  let m = n + 2
      vert i =
        let a = -pi / 2 + fromIntegral i * tau / fromIntegral m
        in toG env (cx + cos a * bigR) (cy + sin a * bigR)
      rec :: Tree -> Int -> Int -> [Picture]
      rec Leaf _ _ = []
      rec (Node l r _) a b =
        let apex = a + size l + 1
        in Color (hsba (fromIntegral ((apex * 60) `mod` 360)) 50 70 0.5) (Polygon [vert a, vert apex, vert b])
           : rec l a apex ++ rec r apex b
      ring' = map vert [0 .. m - 1]
  in Pictures
       [ Pictures (rec tree 0 (m - 1))
       , Color (hsb 0 0 80) (thickPath 2 (ring' ++ take 1 ring'))
       , Color (hsb 50 80 100) (thickLine 3 (vert 0) (vert (m - 1)))     -- 根に対応する辺
       , Color (hsb 0 0 100) (Pictures [at p (disc 4) | p <- ring'])
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 5 -> rebuild w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 1 -> rebuild w { wN = wN w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` len, wAuto = False }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w - 1 + len) `mod` len, wAuto = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        i = floor ((mx - 30) / tileWidth env w) :: Int
    in if my > 50 && my < 170 && i >= 0 && i < len then w { wSel = i, wAuto = False } else w
  _ -> w
  where len = max 1 (length (wItems w))

-- ---------- やってみよう ----------
-- 1. n = 3 の 5 通りを順に見て、経路の「山」と木の「枝」と三角形の「扇」がどう対応するか言葉にしよう
-- 2. C_n = C(2n,n) − C(2n,n+1)：対角線を越える経路を「反射」で数えると引き算で出る（02 とつなぐ）
-- 3. 括弧列の生成で close < open の条件を外すと、ただの 2^(2n) 通りの文字列になる。試して C_n と比べよう
-- 4. 「n 段の階段を 1 段か 2 段で登る」はフィボナッチ、「山脈の形」はカタラン。似た問題でも数列がちがう

-- ---------- 道具 ----------
-- 範囲外なら 0 を返す取り出し
atI :: [Int] -> Int -> Int
atI xs i = case drop i xs of
  (x : _) -> x
  [] -> 0

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
txt env x y size' c s = let (gx, gy) = toG env x y in label gx gy size' c s
