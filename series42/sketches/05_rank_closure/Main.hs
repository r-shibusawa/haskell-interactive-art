-- ============================================
-- 42-05 階数関数と閉包
-- 平面に点を置く（格子にスナップ）。点の集合 A の「階数」r(A) は、A が張る広さ：1 点 = 1、同じ直線上 = 2、広がっている = 3
--   「閉包」cl(A) = A を足しても階数が変わらない点の全部（同じ直線上の点は A に入っていなくても閉包に入る）
--   2 つの集合 A・B で劣モジュラ性 r(A∪B) + r(A∩B) ≤ r(A) + r(B) を数字で確かめる
-- 操作：クリック = A に入れる・外す / T = 編集する集合を A ⇄ B / 点をドラッグ = 動かす / ↑↓ = 点の数 / R = ランダム / C = 選択を消す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, maximumBy)

nPts0 :: Int
nPts0 = 7                -- ★ 点の数（4〜9）

cell :: Float
cell = 70                -- ★ 格子の間隔（px）

data Pt = Pt { gx, gy :: !Int, inA, inB :: !Bool } deriving (Eq)

data World = World
  { wN      :: !Int
  , wPts    :: ![Pt]
  , wTarget :: !Char     -- 今クリックで編集している集合（'A' か 'B'）
  , wDrag   :: !Int
  , wMoved  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "42-05 Rank and closure" world0)
  { skBack  = hsb 170 25 9
  , skStart = \env -> randomPoints env world0
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World nPts0 [] 'A' (-1) False

cols, rowsN :: Env -> Int
cols env = floor ((envW env - 480) / cell)
rowsN env = floor ((envH env - 140) / cell)

toXY :: Pt -> Point
toXY p = (40 + fromIntegral (gx p) * cell, 110 + fromIntegral (gy p) * cell)

randomPoints :: Env -> World -> IO World
randomPoints env w = do
  let go :: Int -> [Pt] -> IO [Pt]
      go tries ps
        | length ps >= wN w || tries <= 0 = pure ps
        | otherwise = do
            x <- randomRIO (0, cols env - 1)
            y <- randomRIO (0, rowsN env - 1)
            go (tries - 1) (if any (\p -> gx p == x && gy p == y) ps then ps else ps ++ [Pt x y False False])
  ps <- go 500 []
  -- 最初の 3 点を同じ直線に乗せておく
  --（右端からはみ出さないよう、1 点目を必要なら左へずらす）
  let line3 (a0 : b : c : r) =
        let a = a0 { gx = min (gx a0) (cols env - 4) }
        in a : b { gx = gx a + 1, gy = gy a } : c { gx = gx a + 3, gy = gy a } : r
      line3 r = r
  pure w { wPts = line3 ps }

-- ★ 階数：点 (gx, gy) を (gx, gy, 1) というベクトルとみなして、その階数
rankOf :: [Pt] -> Int
rankOf = rankReal . map (\p -> [fromIntegral (gx p), fromIntegral (gy p), 1])

-- ★ 閉包：足しても階数が増えない点
closureOf :: [Pt] -> [Pt] -> [Pt]
closureOf allPts list = let r = rankOf list in filter (\p -> rankOf (p : list) == r) allPts

drawSpan :: Env -> [Pt] -> Float -> Picture
drawSpan env list hue = case (rankOf list, list) of
  (2, p0 : _) ->
    let others = filter (\p -> gx p /= gx p0 || gy p /= gy p0) list
        a@(ax, ay) = toXY p0
        (bx, by) = toXY (pick p0 others 0)
        dx = bx - ax
        dy = by - ay
        l = max 1 (sqrt (dx * dx + dy * dy))
    in seg env 3 (hsba hue 70 90 0.6) (ax - dx / l * 3000, ay - dy / l * 3000) (ax + dx / l * 3000, ay + dy / l * 3000)
  (3, _) -> box env (hsba hue 60 60 0.12) 20 90 (envW env - 480) (envH env - 110)
  _ -> Blank

draw :: Env -> World -> Picture
draw env w =
  let c = cols env
      r = rowsN env
      xAt i = 40 + fromIntegral i * cell
      yAt j = 110 + fromIntegral j * cell
      -- 格子
      grid = [ seg env 1 (gray 0.2) (xAt i, 110) (xAt i, yAt (r - 1)) | i <- [0 .. c - 1] ]
          ++ [ seg env 1 (gray 0.2) (40, yAt j) (xAt (c - 1), yAt j) | j <- [0 .. r - 1] ]
      pts = wPts w
      setA = filter inA pts
      setB = filter inB pts
      setU = filter (\p -> inA p || inB p) pts
      setI = filter (\p -> inA p && inB p) pts
      rA = rankOf setA
      rB = rankOf setB
      rU = rankOf setU
      rI = rankOf setI
      clA = if null setA then [] else closureOf pts setA
      -- 点
      ptPic i p =
        let q@(x, y) = toXY p
            inCl = p `elem` clA && not (inA p)
        in Pictures $
             [ dashCircle env 17 2 (hsb 130 70 90) q | inCl ] ++
             [ Color (hsb 290 70 100) (at (sp env q) (ring 15 4)) | inB p ] ++
             [ dot env 10 (if inA p then hsb 130 75 90 else gray 0.6) q
             , txt env (x + 14) (y - 12) 14 white ("p" ++ show (i + 1 :: Int)) ]
      idxOf p = pick 0 [ k | (k, q) <- zip [1 :: Int ..] pts, q == p ] 0
      px = envW env - 460
      py = 120
      lhs = rU + rI
      rhs = rA + rB
      nA = length setA
      tgt = [wTarget w]
  in Pictures $
       grid ++
       [ drawSpan env setA 130 | not (null setA) ] ++
       [ drawSpan env setB 290 | not (null setB) ] ++
       zipWith ptPic [0 ..] pts ++
       [ txt env 20 40 16 white ("Click: toggle in " ++ tgt ++ "  T: edit A/B  Drag: move  Up/Down: points " ++ show (wN w) ++ "  R: random  C: clear")
       , txt env 20 66 14 (gray 0.6) "Green dot = A. Purple ring = B. Green dashed ring = in closure of A. Green line/area = span of A."
       , box env (withA 0.08 white) (px - 20) (py - 40) 470 (envH env - py + 20)
       , stack env px py
           [ (22, hsb 130 70 100, "r(A) = " ++ show rA ++ "   |A| = " ++ show nA)
           , (14, gray 0.7, case rA of { 0 -> "Empty set"; 1 -> "One point"; 2 -> "On one line"; _ -> "Fills the plane" })
           , (14, hsb 130 50 90, "cl(A) = {" ++ intercalate "," (map (\p -> "p" ++ show (idxOf p)) clA) ++ "}")
           , (14, gray 0.7, if nA == 0 then "Choose some points for A"
                            else if length clA == nA then "A is closed (a flat)"
                            else "Closure is bigger: some points add no rank")
           , (8, white, "")
           , (22, hsb 290 60 100, "r(B) = " ++ show rB ++ "   |B| = " ++ show (length setB))
           , (18, white, "r(AuB) = " ++ show rU ++ "   r(AnB) = " ++ show rI)
           , (17, hsb (if lhs <= rhs then 130 else 5) 80 100, "r(AuB) + r(AnB) = " ++ show lhs)
           , (17, hsb (if lhs <= rhs then 130 else 5) 80 100, "<= " ++ show rhs ++ " = r(A) + r(B)" ++ (if lhs <= rhs then "  [ok]" else "  [x]"))
           , (14, gray 0.7, "Submodular: the union spans no more")
           , (14, gray 0.7, "than the two spans added together.")
           , (14, gray 0.7, if lhs < rhs then "Now <. Sharing a line brings it closer to =."
                                         else "Equality holds now (modular).")
           , (8, white, "")
           , (16, white, "Rank axioms")
           , (14, gray 0.7, "(R1) 0 <= r(A) <= |A|")
           , (14, gray 0.7, "(R2) A in B implies r(A) <= r(B)")
           , (14, gray 0.7, "(R3) submodular")
           , (14, gray 0.7, "Independent <=> r(A) = |A|."
                            ++ (if nA == 0 then "" else if rA == nA then " A: indep." else " A: dep."))
           ]
       ]

nearest :: Env -> World -> Int
nearest env w = nearestIn 18 [ dist2 (mouseP env) (toXY p) | p <- wPts w ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wDrag = nearest env w, wMoved = False }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        x = clampI 0 (cols env - 1) (floor ((mx - 40) / cell + 0.5))
        y = clampI 0 (rowsN env - 1) (floor ((my - 110) / cell + 0.5))
        p = pick (Pt 0 0 False False) (wPts w) (wDrag w)
        occupied = or [ gx q == x && gy q == y | (k, q) <- zip [0 ..] (wPts w), k /= wDrag w ]
    in pure (if (x /= gx p || y /= gy p) && not occupied
               then w { wPts = setAt (wDrag w) (\q -> q { gx = x, gy = y }) (wPts w), wMoved = True }
               else w)
  EventKey (MouseButton LeftButton) Up _ _
    | wDrag w >= 0 && not (wMoved w) ->
        let toggle p = if wTarget w == 'A' then p { inA = not (inA p) } else p { inB = not (inB p) }
        in pure w { wPts = setAt (wDrag w) toggle (wPts w), wDrag = -1 }
    | otherwise -> pure w { wDrag = -1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> randomPoints env w { wN = min 9 (wN w + 1), wDrag = -1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomPoints env w { wN = max 4 (wN w - 1), wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> pure w { wTarget = if wTarget w == 'A' then 'B' else 'A' }
    | c `elem` "rR" -> randomPoints env w { wDrag = -1 }
    | c `elem` "cC" -> pure w { wPts = map (\p -> p { inA = False, inB = False }) (wPts w) }
  _ -> pure w

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- ---------- やってみよう ----------
-- 1. 直線上の 2 点を A にして、3 点目をドラッグでその直線に乗せる。閉包に入る瞬間（破線の輪）を見る
-- 2. A = 直線上の 3 点、B = 別の直線上の 3 点で交点を共有させると r(A∪B) + r(A∩B) はいくつ？
-- 3. 「閉集合（フラット）」を全部挙げる：点、直線（2 点以上が乗っているもの）、平面全体、空集合
-- 4. 階数を「3 次元」に拡張すると、4 点が同じ平面に乗るかどうかが問題になる（00 の空間と同じ）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 破線の円（半径 r、太さ w）
dashCircle :: Env -> Float -> Float -> Color -> Point -> Picture
dashCircle env r w c (x, y) =
  let n = max 6 (floor (tau * r / 16)) :: Int
      pt a = (x + r * cos a, y + r * sin a)
      da = tau / fromIntegral n
  in Pictures [ seg env w c (pt a) (pt (a + da / 2)) | k <- [0 .. n - 1], let a = fromIntegral k * da ]

-- 塗りつぶしの丸（半径 r）
dot :: Env -> Float -> Color -> Point -> Picture
dot env r c p = Color c (at (sp env p) (disc r))

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- リストの i 番目だけ f で変える
setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- limit より近いものの中でいちばん近い番号（なければ -1）
nearestIn :: Float -> [Float] -> Int
nearestIn limit ds = fst (foldl' pickNear (-1, limit) (zip [0 ..] ds))
  where pickNear (bi, bd) (i, d) = if d < bd then (i, d) else (bi, bd)

-- 実数のベクトルの階数（ガウスの消去法。小さい値は 0 とみなす）
rankReal :: [[Double]] -> Int
rankReal rows = go 0 rows
  where
    n = case rows of { (r : _) -> length r; [] -> 0 }
    val c r = case drop c r of { (v : _) -> v; [] -> 0 }
    go c rs
      | c >= n || null rs = 0
      | abs (val c p) < 1e-6 = go (c + 1) rs
      | otherwise = 1 + go (c + 1) [ zipWith (\x y -> x - f r * y) r p | r <- rest ]
      where
        ip = fst (maximumBy (\a b -> compare (abs (val c (snd a))) (abs (val c (snd b)))) (zip [0 :: Int ..] rs))
        p = case drop ip rs of { (r : _) -> r; [] -> [] }
        rest = [ r | (i, r) <- zip [0 ..] rs, i /= ip ]
        f r = val c r / val c p
