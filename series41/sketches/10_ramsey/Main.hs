-- ============================================
-- 41-10 ラムゼー理論：R(3,3) = 6
-- 6 人のパーティでは「3 人が互いに知り合い」か「3 人が互いに他人」が必ずいる
-- 完全グラフ K_6 の 15 本の辺をどう 2 色に塗っても、単色の三角形が必ずできる。K_5 なら避けられる
-- 操作：5 / 6 = 頂点の数 / クリック = 辺の色を切り替え / スペース = ランダムに塗る / E = K_5 の回避例 / A = 自動で塗り直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.Array.Unboxed
import Data.List (foldl')

data World = World
  { wV     :: !Int                     -- ★ 頂点の数（5 か 6）
  , wCol   :: !(UArray (Int, Int) Int) -- 0（赤）or 1（青）
  , wAuto  :: !Bool
  , wTick  :: !Float
  , wFound :: ![(Int, Int, Int, Int)]  -- 単色三角形の一覧 (i, j, k, 色)
  , wFlash :: !Float
  }

world0 :: World
world0 = World 6 (listArray ((0, 0), (5, 5)) (repeat 0)) False 0 [] 0

main :: IO ()
main = runSketch (sketch "41-10 Ramsey R(3,3) = 6" world0)
  { skBack  = hsb 228 28 11
  , skStart = \_ -> randomColor world0
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

vpos :: Env -> Int -> Int -> Point
vpos env v i =
  let cx = envW env * 0.4
      cy = envH env * 0.55
      big = min (envW env) (envH env) * 0.33
      a = -pi / 2 + fromIntegral i * tau / fromIntegral v
  in (cx + cos a * big, cy + sin a * big)

-- 上三角の色のリストから、対称な色の表をつくる
fromEdges :: Int -> [((Int, Int), Int)] -> UArray (Int, Int) Int
fromEdges v es =
  listArray ((0, 0), (v - 1, v - 1)) (repeat 0) // concat [[((i, j), c), ((j, i), c)] | ((i, j), c) <- es]

edges :: Int -> [(Int, Int)]
edges v = [(i, j) | i <- [0 .. v - 1], j <- [i + 1 .. v - 1]]

randomColor :: World -> IO World
randomColor w = do
  es <- forM (edges (wV w)) $ \e -> do
    c <- randomRIO (0, 1 :: Int)
    pure (e, c)
  pure (findTriangles w { wCol = fromEdges (wV w) es })

-- K_5 の回避例：隣（距離 1）は赤、2 つ先（距離 2）は青 → 五角形と五芒星
avoidExample :: World -> World
avoidExample w =
  let v = 5
      col (i, j) = if min (j - i) (v - (j - i)) == 1 then 0 else 1
  in findTriangles w { wV = v, wCol = fromEdges v [(e, col e) | e <- edges v] }

-- ★ 単色三角形を全部探す（3 頂点の組合せ C(V,3) 通りを調べるだけ）
findTriangles :: World -> World
findTriangles w =
  let v = wV w
      c = wCol w
  in w { wFound = [ (i, j, k, c ! (i, j))
                  | i <- [0 .. v - 1], j <- [i + 1 .. v - 1], k <- [j + 1 .. v - 1]
                  , c ! (i, j) == c ! (j, k), c ! (j, k) == c ! (i, k) ]
       , wFlash = 1 }

step :: Env -> Float -> World -> IO World
step _ dt w0 = do
  let w = w0 { wFlash = wFlash w0 * 0.96 }
  if wAuto w
    then if wTick w + dt > 0.8 then randomColor w { wTick = 0 } else pure w { wTick = wTick w + dt }
    else pure w

draw :: Env -> World -> Picture
draw env w =
  let v = wV w
      g i = let (x, y) = vpos env v i in toG env x y
      found = wFound w
      triFill (a, b, c, col) =
        Color (hsba (if col == 0 then 0 else 215) 80 100 ((10 + wFlash w * 20) / 100)) (Polygon [g a, g b, g c])
      inTri i j = any (\(a, b, c, _) -> i `elem` [a, b, c] && j `elem` [a, b, c]) found
      edgePic (i, j) =
        let t = inTri i j
        in Color (hsb (if wCol w ! (i, j) == 0 then 0 else 215) 80 (if t then 100 else 70))
                 (thickLine (if t then 7 else 3) (g i) (g j))
      vertex i =
        let (x, y) = vpos env v i
        in Pictures
             [ at (g i) (Color (hsb 0 0 100) (disc 17))
             , txtM env x y 16 (hsb 0 0 10) (show (i + 1)) ]
      px = envW env * 0.64
      py = 130
      reds = length [() | (_, _, _, 0) <- found]
      blues = length found - reds
      line i s = txt env px (py + i) 14 (hsb 0 0 70) s
      explanation
        | v == 6 =
            [ line 130 "Proof: 3 of vertex 1's 5 edges share a"
            , line 150 "color (pigeonhole). Call their ends a, b, c."
            , line 170 "Two of them joined in that color: triangle"
            , line 190 "with 1. Otherwise a b c is a triangle."
            , line 214 "-> true for all 2^15 = 32768 colorings" ]
        | otherwise =
            [ line 130 "K_5 can avoid it: pentagon red, star blue (E)"
            , line 150 "-> R(3,3) = 6 (5 is not enough, 6 always is)"
            , line 176 "How often does a random coloring avoid it?" ]
  in Pictures
       [ Pictures (map triFill found)
       , Pictures (map edgePic (edges v))
       , Pictures (map vertex [0 .. v - 1])
       , box env (hsba 0 0 100 0.08) (px - 16) (py - 30) (envW env - px + 6) 330
       , txt env px py 20 (hsb 0 0 100)
           ("K_" ++ show v ++ ": " ++ show (v * (v - 1) `div` 2) ++ " edges, " ++ show (v * (v - 1) * (v - 2) `div` 6) ++ " triangles")
       , txt env px (py + 36) 16 (hsb 0 80 100) ("Red triangles: " ++ show reds)
       , txt env px (py + 60) 16 (hsb 215 80 100) ("Blue triangles: " ++ show blues)
       , txt env px (py + 96) 18 (hsb (if null found then 130 else 50) 80 100)
           (if null found then "No one-color triangle!" else show (length found) ++ " one-color triangles")
       , Pictures explanation
       , line 246 "R(4,4) = 18; R(5,5) is in 43-46 (open!)"
       , line 268 "\"If aliens demanded R(6,6)...\" (Erdos)"
       , txt env 20 40 16 (hsb 0 0 100)
           ("Vertices " ++ show v ++ " (5 / 6)   Click: flip an edge   Space: random   E: K_5 example   A: auto "
            ++ (if wAuto w then "ON" else "off"))
       , txt env 20 68 14 (hsb 0 0 60)
           (if v == 6 then "Click edges to remove all one-color triangles. You can't! (red = friends, blue = strangers)"
                      else "K_5 has a coloring with no one-color triangle. Find one (E shows the answer).")
       ]

-- 点と線分の距離
segDist :: Float -> Float -> Point -> Point -> Float
segDist px py (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l2 = dx * dx + dy * dy
      t = clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / l2)
  in dist2 (px, py) (ax + t * dx, ay + t * dy)

-- クリックした所の辺の色を変える
click :: Env -> World -> World
click env w =
  let (mx, my) = mouseP env
      v = wV w
      try' (bd, best) (i, j) =
        let a@(ax, ay) = vpos env v i
            b@(bx, by) = vpos env v j
            d = segDist mx my a b
            mid = dist2 (mx, my) ((ax + bx) / 2, (ay + by) / 2)   -- 頂点のそばで複数の辺が重なるので、中点に近い方を優先
            closer = case best of
              Just (_, _, m0) -> d < 12 && mid < m0
              Nothing -> False
        in if d < bd || closer then (d, Just (i, j, mid)) else (bd, best)
  in case snd (foldl' try' (12, Nothing) (edges v)) of
       Just (i, j, _) ->
         let c = 1 - wCol w ! (i, j)
         in findTriangles w { wCol = wCol w // [((i, j), c), ((j, i), c)], wAuto = False }
       Nothing -> w

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c == '5' && wV w /= 5 -> randomColor w { wV = 5 }
    | c == '6' && wV w /= 6 -> randomColor w { wV = 6 }
    | c `elem` "eE" -> pure (avoidExample w)
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> randomColor w
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click env w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. K_6 で辺を 1 本ずつクリックし、単色三角形を減らしてみる。最小はいくつ？（2 個）
-- 2. K_5 でランダムに塗って、単色三角形なしが出るまで何回かかる？（A で自動）
-- 3. 色を 3 色にすると、避けられなくなるのは K_17（R(3,3,3) = 17）。色の値を 0〜2 にして K_7 くらいで試そう
-- 4. 「3 人が互いに知り合い」を「4 人」に変えると R(4,4) = 18。V を 18 にしたら三角形の代わりに K_4 を探す

-- ---------- 道具 ----------
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
