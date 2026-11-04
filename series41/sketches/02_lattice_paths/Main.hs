-- ============================================
-- 41-02 格子路の数え上げ
-- 左下 (0,0) から右上 (m,n) まで、右か上にしか進めない道は C(m+n, n) 通り
-- 各交差点に「ここまで来る道の数」を書くと、パスカルの三角形が現れる
-- 操作：←→ = m（1〜8）/ ↑↓ = n（1〜8）/ クリック = その点を通る道の数 / スペース = ランダムな道を次々に / R = 道を消す
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (foldl')

startM, startN :: Int
startM = 6             -- ★ 格子の横の大きさ
startN = 4             -- ★ 格子の縦の大きさ

drawSpeed :: Float
drawSpeed = 0.12       -- ★ 1 フレームに描き進める歩数

data World = World
  { wM, wN      :: !Int
  , wWays       :: !(Array (Int, Int) Int)   -- (i,j) までの道の数
  , wVisits     :: !(Array (Int, Int) Int)   -- 各点を通った回数（ランダム道の統計）
  , wSteps      :: !String                   -- いま描いている道（'R' / 'U' の列）
  , wProgress   :: !Float                    -- 道をどこまで描いたか
  , wAuto       :: !Bool
  , wPicked     :: !(Maybe (Int, Int))       -- クリックした点
  , wPathCount  :: !Int
  }

main :: IO ()
main = runSketch (sketch "41-02 Lattice paths" world0)
  { skBack  = hsb 220 30 10
  , skStart = \_ -> rebuild world0
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World startM startN (waysTable startM startN) (zeros startM startN) "" 0 True Nothing 0

zeros :: Int -> Int -> Array (Int, Int) Int
zeros m n = listArray ((0, 0), (m, n)) (repeat 0)

-- ★ 動的計画法：ways[i][j] = ways[i-1][j] + ways[i][j-1]
waysTable :: Int -> Int -> Array (Int, Int) Int
waysTable m n = t
  where
    t = array ((0, 0), (m, n))
          [ ((i, j), if i == 0 || j == 0 then 1 else t ! (i - 1, j) + t ! (i, j - 1))
          | i <- [0 .. m], j <- [0 .. n] ]

rebuild :: World -> IO World
rebuild w = newPath w
  { wWays = waysTable (wM w) (wN w), wVisits = zeros (wM w) (wN w), wPicked = Nothing, wPathCount = 0 }

-- ランダムな道：残りの R と U から 1 つ選ぶ（全部の道が同じ確率になるよう、残り数で重みづけ）
newPath :: World -> IO World
newPath w = do
  steps <- go 0 0
  pure w { wSteps = steps, wProgress = 0 }
  where
    m = wM w
    n = wN w
    go i j
      | i >= m && j >= n = pure ""
      | otherwise = do
          let right = if i < m then waysFrom w (i + 1) j else 0
              up = if j < n then waysFrom w i (j + 1) else 0
          r <- randomRIO (0, 1 :: Float)
          if r * fromIntegral (right + up) < fromIntegral right
            then ('R' :) <$> go (i + 1) j
            else ('U' :) <$> go i (j + 1)

-- (i,j) から (m,n) までの道の数 = C((m-i)+(n-j), n-j)
waysFrom :: World -> Int -> Int -> Int
waysFrom w i j
  | i > wM w || j > wN w = 0
  | otherwise = wWays w ! (wM w - i, wN w - j)

gridPos :: Env -> World -> Int -> Int -> Point
gridPos env w i j =
  let cell = min 90 (min ((envW env - 420) / fromIntegral (wM w)) ((envH env - 220) / fromIntegral (wN w)))
  in (60 + fromIntegral i * cell, envH env - 80 - fromIntegral j * cell)

-- 道の通る点の列（(0,0) から）
walk :: String -> [(Int, Int)]
walk = scanl (\(i, j) s -> if s == 'R' then (i + 1, j) else (i, j + 1)) (0, 0)

step :: Env -> Float -> World -> IO World
step _ _ w
  | not (wAuto w) = pure w
  | wProgress w + drawSpeed >= fromIntegral (length (wSteps w)) + 6 =
      -- 道を記録して次へ
      newPath w
        { wVisits = foldl' (\v p -> v // [(p, v ! p + 1)]) (wVisits w) (walk (wSteps w))
        , wPathCount = wPathCount w + 1 }
  | otherwise = pure w { wProgress = wProgress w + drawSpeed }

draw :: Env -> World -> Picture
draw env w =
  let m = wM w
      n = wN w
      g i j = let (x, y) = gridPos env w i j in toG env x y
      shown = min (floor (wProgress w)) (length (wSteps w))
      current = take shown (wSteps w)
      pathPts = map (uncurry g) (walk current)
      maxVisit = foldl' max 1 (elems (wVisits w))
      point (a, b) =
        let heat = fromIntegral (wVisits w ! (a, b)) / fromIntegral maxVisit :: Float
            (qx, qy) = gridPos env w a b
        in Pictures
             [ at (toG env qx qy) (Color (hsb (200 - heat * 160) (60 + heat * 30) (50 + heat * 50)) (disc ((14 + heat * 10) / 2)))
             , txtC env qx (qy - 14) 14 (hsb 0 0 100) (show (wWays w ! (a, b))) ]
      px = envW env - 400
      py = 120
      pickedPic = case wPicked w of
        Nothing -> Blank
        Just (i, j) ->
          let (qx, qy) = gridPos env w i j
              a = wWays w ! (i, j)
              b = waysFrom w i j
              msg = "(" ++ show i ++ "," ++ show j ++ ") paths through: " ++ show a ++ " x " ++ show b ++ " = " ++ show (a * b)
              -- 右パネルにかかるときは点の左側に書く
              lx = if qx + 24 + textW 16 msg > px - 20 then qx - 24 - textW 16 msg else qx + 24
          in Pictures
               [ at (toG env qx qy) (Color (hsb 330 80 100) (ring 17 3))
               , txt env lx (qy + 6) 16 (hsb 330 80 100) msg ]
  in Pictures
       [ -- 格子線
         Color (hsb 0 0 35) (Pictures
           ( [Line [g i 0, g i n] | i <- [0 .. m]]
          ++ [Line [g 0 j, g m j] | j <- [0 .. n]] ))
         -- 道（進んだところまで）
       , Color (hsb 45 90 100) (Pictures (thickPath 6 pathPts : [at p (disc 3) | p <- pathPts]))
         -- 交差点：道の数、通った回数
       , Pictures (map point [(a, b) | a <- [0 .. m], b <- [0 .. n]])
       , pickedPic
         -- 右パネル
       , box env (hsba 0 0 100 0.08) (px - 16) (py - 30) 400 250
       , txt env px (py + 12) 26 (hsb 0 0 100)
           ("C(" ++ show (m + n) ++ "," ++ show n ++ ") = " ++ show (wWays w ! (m, n)))
       , txt env px (py + 46) 15 (hsb 0 0 70)
           ("R x " ++ show m ++ " and U x " ++ show n ++ ": " ++ show (m + n) ++ " steps in all.")
       , txt env px (py + 68) 15 (hsb 0 0 70) ("Which " ++ show n ++ " steps are U? A combination")
       , txt env px (py + 90) 15 (hsb 0 0 70) "Each number = left + below (Pascal)"
       , txt env px (py + 126) 15 (hsb 45 90 100)
           ("Path: " ++ current ++ (if shown < length (wSteps w) then "..." else ""))
       , txt env px (py + 150) 15 (hsb 0 0 70)
           ("Random paths: " ++ show (wPathCount w) ++ " (color = visits)")
       , txt env px (py + 172) 15 (hsb 0 0 70) "Middle points get more visits"
       , txt env 20 40 16 (hsb 0 0 100)
           ("m = " ++ show m ++ " (L/R)  n = " ++ show n ++ " (Up/Down)  Click: paths through a point  Space: auto "
            ++ (if wAuto w then "ON" else "off") ++ "  R: clear")
       , txt env 20 68 14 (hsb 0 0 60)
           "Bottom-left to top-right, moving only right or up. Number of paths = orderings of R and U = C(m+n, n)"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ | wM w < 8 -> rebuild w { wM = wM w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wM w > 1 -> rebuild w { wM = wM w - 1 }
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 8 -> rebuild w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 1 -> rebuild w { wN = wN w - 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> rebuild w
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click env w)
  _ -> pure w

-- いちばん近い交差点を選ぶ（もう一度押すと解除）
click :: Env -> World -> World
click env w =
  let (mx, my) = mouseP env
      d2 (a, b) = let (qx, qy) = gridPos env w a b in (qx - mx) * (qx - mx) + (qy - my) * (qy - my)
      pick (bd, best) p = let d = d2 p in if d < bd then (d, Just p) else (bd, best)
      (bestD, bestP) = foldl' pick (1e9, Nothing) [(a, b) | a <- [0 .. wM w], b <- [0 .. wN w]]
  in if bestD < 40 * 40
       then w { wPicked = if wPicked w == bestP then Nothing else bestP }
       else w

-- ---------- やってみよう ----------
-- 1. m = n = 4 で、対角線上の点 (2,2) を通る道の数 = C(4,2) × C(4,2) = 36。クリックして確かめよう
-- 2. 道をたくさん流すと、どの点がよく通られる？ 端の点と真ん中の点をくらべよう（二項分布）
-- 3. 「対角線より上に出ない道」だけ数えるとカタラン数（08）になる。newPath に条件を足してみよう
-- 4. 右上だけでなく「斜め右上」にも進めるようにすると数はどうなる？（デラノイ数）

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

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 横は中央ぞろえ、y は文字の下
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - textW size s / 2) y size c s
