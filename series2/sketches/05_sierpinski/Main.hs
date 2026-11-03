-- ============================================
-- 05 シェルピンスキーの三角形とカーペット
-- 「真ん中を抜く」を再帰でくり返す
-- マウス左右 = 深さ / クリックで 三角形 ⇄ カーペット
-- ============================================
module Main (main) where

import Art

data Mode = Tri | Car deriving Eq

newtype World = World { wMode :: Mode }

background :: Color
background = hsb 260 30 10

main :: IO ()
main = runSketch (sketch "02-05 Sierpinski" (World Tri))
  { skBack  = background
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [shape, info]
  where
    ww = envW env
    wh = envH env
    triangleMode = wMode w == Tri
    -- 図形が多すぎないよう、深さの上限は 三角形 8・カーペット 5（p5.js 版は右端だけ 9・6）
    top = if triangleMode then 9 else 6 :: Float
    limit = if triangleMode then 8 else 5 :: Int
    maxDepth = min limit (floor (clampF 0 top (remap (fst (envMouse env) + ww / 2) 0 ww 0 top)))
    s = min ww wh * 0.8
    col = hsb (fromIntegral maxDepth * 30 + fromIntegral (envFrame env) * 0.3) 60 100
    (shape, count)
      | triangleMode =
          -- 大きな三角形の 3 つの頂点
          let y0 = s * 0.43
              y1 = y0 - s * 0.866
              tris = sierpinski maxDepth (0, y0) (-s / 2, y1) (s / 2, y1) 0
          in (Color col (Pictures tris), length tris)
      | otherwise =
          ( Pictures [Color col (rect 0 0 s s), Color background (Pictures (carpetHoles maxDepth 0 0 s 0))]
          , 8 ^ maxDepth :: Int )
    info = label (-ww / 2 + 20) (wh / 2 - 30) 18 white
      ( "Depth " ++ show maxDepth ++ "   Shapes " ++ show count ++ "   Click to switch ("
        ++ (if triangleMode then "triangle" else "carpet") ++ ")" )

-- 三角形：深さが残っていれば、3 つの小さい三角形に分ける（真ん中は描かない）
sierpinski :: Int -> Point -> Point -> Point -> Int -> [Picture]
sierpinski maxDepth p1 p2 p3 depth
  | depth >= maxDepth = [Polygon [p1, p2, p3]]
  | otherwise =
      -- 各辺の真ん中の点
      let m12 = middle p1 p2
          m23 = middle p2 p3
          m31 = middle p3 p1
      in sierpinski maxDepth p1 m12 m31 (depth + 1)     -- 上
           ++ sierpinski maxDepth m12 p2 m23 (depth + 1)  -- 左下
           ++ sierpinski maxDepth m31 m23 p3 (depth + 1)  -- 右下

-- カーペット：9 分割して、真ん中に穴をあけ、残りの 8 個にも同じことをする
-- （p5.js 版は 8 個の四角を塗るが、深さ 5 では 32768 個になるので、
--   全体を塗ってから穴を背景色で描く。穴なら 4681 個ですみ、見た目は同じ）
carpetHoles :: Int -> Float -> Float -> Float -> Int -> [Picture]
carpetHoles maxDepth cx cy s depth
  | depth >= maxDepth = []
  | otherwise =
      rect cx cy t t
        : concat
            [ carpetHoles maxDepth (cx + i * t) (cy + j * t) t (depth + 1)
            | i <- [-1, 0, 1], j <- [-1, 0, 1]
            , not (i == 0 && j == 0) ]  -- 真ん中はスキップ
  where t = s / 3

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wMode = if wMode w == Tri then Car else Tri }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 三角形で、真ん中を「描いて」まわりを描かないようにすると？
-- 2. カーペットで、スキップするマスを変えてみよう（i == -1 && j == -1 など）
-- 3. 深さが 1 増えると、図形の数は何倍になる？（三角形 3 倍、カーペット 8 倍）

-- ---------- 道具 ----------

middle :: Point -> Point -> Point
middle (a, b) (c, d) = ((a + c) / 2, (b + d) / 2)
