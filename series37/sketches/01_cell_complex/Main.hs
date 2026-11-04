-- ============================================
-- 01 胞体複体を作る ─ 頂点・辺・面を置いてオイラー標数を数える
-- 離散モース理論の舞台は「胞体複体」：点（0 次元）、線（1 次元）、面（2 次元）を貼り合わせたもの
--   V − E + F = オイラー標数 χ = b0 − b1。b0 = 塊の数、b1 = 穴の数（穴 = 辺で囲まれているのに面で埋まっていない所）
-- クリック = 頂点を置く / 頂点からドラッグ = 辺を引く / F = マウスのある三角形・四角形を面にする / X = 近くの頂点を消す / R = ランダムな複体 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Art
import Control.Monad (foldM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showFFloat)

data World = World
  { wVerts :: !(U.Vector Point)
  , wEdges :: ![(Int, Int)]     -- 頂点番号（a < b）
  , wFaces :: ![[Int]]          -- 頂点番号の列（3 つか 4 つ、まわる順）
  , wDrag  :: !Int              -- ドラッグを始めた頂点（-1 = なし）
  , wMsg   :: !String
  }

snapR :: Float
snapR = 18                      -- ★ 頂点をつかむ距離

empty :: World
empty = World U.empty [] [] (-1) ""

main :: IO ()
main = runSketch (sketch "37-01 Cell Complex" empty)
  { skBack  = hsb 225 30 8
  , skStart = \_ -> randomComplex
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

nearestVertex :: World -> Point -> Int
nearestVertex w p = fst (U.ifoldl' pick (-1, snapR) (wVerts w))
  where pick (b, bd) i q = let d = dist2 p q in if d < bd then (i, d) else (b, bd)

hasEdge :: World -> Int -> Int -> Bool
hasEdge w a b = (min a b, max a b) `elem` wEdges w

addEdge :: Int -> Int -> World -> World
addEdge a b w
  | a /= b && not (hasEdge w a b) = w { wEdges = wEdges w ++ [(min a b, max a b)] }
  | otherwise = w

neighbors :: World -> Int -> [Int]
neighbors w a = concat [[y | x == a] ++ [x | y == a] | (x, y) <- wEdges w]

-- 多角形の中にマウスがあるか
inside :: World -> [Int] -> Point -> Bool
inside w ps (x, y) = odd (length (filter crosses (zip vs (drop 1 vs ++ take 1 vs))))
  where
    vs = map (wVerts w U.!) ps
    crosses ((ax, ay), (bx, by)) = (ay > y) /= (by > y) && x < (bx - ax) * (y - ay) / (by - ay) + ax

polyArea :: World -> [Int] -> Float
polyArea w ps = abs (sum (zipWith cr vs (drop 1 vs ++ take 1 vs))) / 2
  where vs = map (wVerts w U.!) ps
        cr (ax, ay) (bx, by) = ax * by - bx * ay

sameFace :: [Int] -> [Int] -> Bool
sameFace p q = length p == length q && all (`elem` q) p

-- ★ F：マウスを囲む、辺でできた三角形・四角形のうち一番小さいものを面にする
addFaceAtMouse :: Point -> World -> World
addFaceAtMouse m w = case fst (foldl' consider ([], 1e18) cands) of
  [] -> w { wMsg = "No triangle or quad under the mouse" }
  best -> w { wFaces = wFaces w ++ [best], wMsg = "Added a face: " ++ show (length best) ++ "-gon" }
  where
    n = U.length (wVerts w)
    consider (b, ba) p
      | inside w p m && not (any (sameFace p) (wFaces w)) && polyArea w p < ba = (p, polyArea w p)
      | otherwise = (b, ba)
    cands = concat
      [ [[a, b, c] | c > a, hasEdge w a c]
        ++ [ [a, b, c, d] | d <- neighbors w c, d /= b, d > a, hasEdge w a d, not (hasEdge w a c), not (hasEdge w b d) ]
      | a <- [0 .. n - 1], b <- neighbors w a, b > a, c <- neighbors w b, c /= a ]

-- オイラー標数とベッチ数（b0 は Union-Find、b1 = b0 − χ：平面なので b2 = 0）
betti :: World -> (Int, Int, Int)
betti w = (b0, b0 - x, x)
  where
    n = U.length (wVerts w)
    root = components n (wEdges w)
    b0 = length [i | i <- [0 .. n - 1], root U.! i == i]
    x = n - length (wEdges w) + length (wFaces w)

-- 辺 ab と pq が交わるか（端点を共有するものは除く）
crossing :: U.Vector Point -> Int -> Int -> (Int, Int) -> Bool
crossing vs a b (p, q)
  | a == p || a == q || b == p || b == q = False
  | otherwise = cr pa pb pp * cr pa pb pq' < 0 && cr pp pq' pa * cr pp pq' pb < 0
  where
    pa = vs U.! a
    pb = vs U.! b
    pp = vs U.! p
    pq' = vs U.! q
    cr (ux, uy) (vx, vy) (wx, wy) = (vx - ux) * (wy - uy) - (vy - uy) * (wx - ux)

randomComplex :: IO World
randomComplex = do
  let n = 14 :: Int
  ps <- mapM (const ((,) <$> randomRIO (scrW * 0.15, scrW * 0.85) <*> randomRIO (scrH * 0.2, scrH * 0.85))) [1 .. n]
  let vs = U.fromList ps
      w0 = empty { wVerts = vs, wMsg = "Random complex" }
      tryEdge w (a, b)
        | dist2 (vs U.! a) (vs U.! b) >= 170 = pure w
        | otherwise = do
            r <- randomRIO (0, 1 :: Float)
            pure (if r < 0.8 && not (any (crossing vs a b) (wEdges w)) then addEdge a b w else w)
  w1 <- foldM tryEdge w0 [(a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1]]
  let tryFace w f = do
        r <- randomRIO (0, 1 :: Float)
        pure (if r < 0.5 then w { wFaces = wFaces w ++ [f] } else w)
  foldM tryFace w1 [[a, b, c] | a <- [0 .. n - 1], b <- neighbors w1 a, a < b, c <- neighbors w1 b, b < c, hasEdge w1 a c]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  map (faceFill . map (wVerts w U.!)) (wFaces w)
  ++ [seg 3 (hsb 85 60 90) (vs a) (vs b) | (a, b) <- wEdges w]
  ++ [seg 3 (hsb 48 80 100) (vs (wDrag w)) m | wDrag w >= 0]
  ++ map (\p -> dotC (hsb 272 60 100) p 14) (U.toList (wVerts w))
  ++ [ringC white (vs nv) 26 2 | nv >= 0]
  ++ [ txt 30 40 20 white ("V = " ++ show (U.length (wVerts w)) ++ "  E = " ++ show (length (wEdges w)) ++ "  F = " ++ show (length (wFaces w))
         ++ "   chi = V - E + F = " ++ show x ++ "   b0 = " ++ show b0 ++ "  b1 = " ++ show b1 ++ "   chi = b0 - b1")
     , txt 30 66 15 (gray 0.65) "Click: vertex  Drag: edge  F: fill triangle/quad at mouse  X: delete vertex  R: random  C: clear"
     , txt 30 90 15 (hsb 48 60 100) (wMsg w)
     , txt 30 (scrH - 30) 14 (gray 0.55) "A hole is a place ringed by edges but not filled by a face. Filling it lowers b1 by 1 and raises chi by 1."
     ]
  where
    vs = (wVerts w U.!)
    m = mouseP env
    nv = nearestVertex w m
    (b0, b1, x) = betti w

-- 面を塗る。四角形はへこんでいても塗れるように、内側を通る対角線で三角形 2 つに分ける
faceFill :: [Point] -> Picture
faceFill ps = Color (hsba 12 70 90 0.45) $ Pictures $ map (Polygon . map fl) $ case ps of
  [a, b, c, d] | opposite a c b d -> [[a, b, c], [a, c, d]]
               | otherwise -> [[b, c, d], [b, d, a]]
  _ -> [ps]
  where
    side (ux, uy) (vx, vy) (wx, wy) = (vx - ux) * (wy - uy) - (vy - uy) * (wx - ux)
    opposite u v p q = side u v p * side u v q < 0

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < 80 -> pure w
    | v >= 0 -> pure w { wDrag = v }
    | otherwise -> pure w { wVerts = U.snoc (wVerts w) m, wDrag = -1, wMsg = "Added a vertex" }
  EventKey (MouseButton LeftButton) Up _ _ -> pure (release w)
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> pure (addFaceAtMouse m w)
    | c `elem` "rR" -> randomComplex
    | c `elem` "cC" -> pure empty
    | c `elem` "xX" -> pure (if v < 0 then w else removeVertex v w)
  _ -> pure w
  where
    m@(_, my) = mouseP env
    v = nearestVertex w m
    release w0
      | wDrag w0 < 0 = w0
      | v >= 0 && v /= d = (addEdge d v w0) { wDrag = -1, wMsg = "Added an edge" }
      | v < 0 && dist2 m (wVerts w0 U.! d) > snapR =
          let w1 = w0 { wVerts = U.snoc (wVerts w0) m }
          in (addEdge d (U.length (wVerts w1) - 1) w1) { wDrag = -1, wMsg = "Added a vertex and an edge" }
      | otherwise = w0 { wDrag = -1 }
      where d = wDrag w0

removeVertex :: Int -> World -> World
removeVertex v w = w
  { wVerts = U.take v (wVerts w) U.++ U.drop (v + 1) (wVerts w)
  , wEdges = [(sh a, sh b) | (a, b) <- wEdges w, a /= v, b /= v]
  , wFaces = [map sh f | f <- wFaces w, v `notElem` f]
  , wDrag = -1
  , wMsg = "Deleted a vertex" }
  where sh k = if k > v then k - 1 else k

-- ---------- やってみよう ----------
-- 1. 三角形を 1 つ作る（V=3, E=3, F=0）→ χ = 0、b1 = 1。F で埋めると χ = 1。表で確かめよう
-- 2. 「8 の字」（穴 2 つ）を作って b1 = 2 にしよう。最低何個の頂点がいる？
-- 3. 離れた塊を 2 つ作ると b0 = 2。辺 1 本でつなぐと χ はどう変わる？
-- 4. 四角形の面を 2 つ並べて共通の辺を X で消す（頂点ごと）。面も消えるのはなぜ？

-- ---------- 離散モース理論の道具 ----------

-- p5.js と同じ座標（左上が原点、y は下向き）で描くための小道具。screen で全体を画面に合わせる
scrW, scrH :: Float
scrW = 1280
scrH = 800

screen :: Env -> Picture -> Picture
screen env = Translate (-envW env / 2) (envH env / 2)

toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = toP5 env (envMouse env)

fl :: Point -> Point
fl (x, y) = (x, -y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (-y)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (thickLine w (fl a) (fl b))

-- 太さ w の折れ線
polyL :: Float -> Color -> [Point] -> Picture
polyL w c ps = Color c (thickPath w (map fl ps))

-- 左上 (x, y)、幅 w、高さ h の長方形
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (-(y + h / 2)) w h)

-- 直径 d の円（p5.js の circle）
dotC :: Color -> Point -> Float -> Picture
dotC c p d = Color c (at (fl p) (disc (d / 2)))

-- 直径 d、線の太さ w の輪
ringC :: Color -> Point -> Float -> Float -> Picture
ringC c p d w = Color c (at (fl p) (ring (d / 2) w))

fmt :: Int -> Float -> String
fmt n x = showFFloat (Just n) x ""

fi :: Int -> Float
fi = fromIntegral

-- Union-Find：n 個の要素を組 (a, b) で結び、各要素の根を返す
components :: Int -> [(Int, Int)] -> U.Vector Int
components n links = U.create $ do
  p <- U.thaw (U.enumFromN 0 n)
  let find a = do
        q <- MU.read p a
        if q == a then pure a else do
          r <- MU.read p q
          MU.write p a r
          find r
      unite (a, b) = do
        x <- find a
        y <- find b
        if x /= y then MU.write p x y else pure ()
  mapM_ unite links
  mapM_ (\i -> find i >>= MU.write p i) [0 .. n - 1]
  pure p
