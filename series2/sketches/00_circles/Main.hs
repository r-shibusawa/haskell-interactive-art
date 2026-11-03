-- ============================================
-- 00 円の中に円（はじめての再帰）
-- 「自分自身を呼び出す」関数で、同じ形がどこまでも続く
-- マウスを左右に動かすと、くり返しの深さが変わる
-- ============================================
module Main (main) where

import Art

main :: IO ()
main = runSketch (sketch "02-00 Circles in Circles" ())
  { skBack = hsb 230 30 10
  , skDraw = \env _ -> drawWorld env
  }

drawWorld :: Env -> Picture
drawWorld env = Pictures (circles ++ [info])
  where
    w = envW env
    h = envH env
    -- マウスの x 位置 → 深さ（1〜9。いちばん右の端だけ 9）
    maxDepth = floor (clampF 1 9 (remap (mouseX env) 0 w 1 9)) :: Int
    -- 最初の大きな円から始める
    circles = drawCircle maxDepth 0 0 (min w h * 0.42) 0
    info = label (-w / 2 + 20) (h / 2 - 30) 18 white
      ("Depth " ++ show maxDepth ++ "   Circles " ++ show (length circles) ++ "   (move the mouse left/right)")

-- 円を描いて、その中にもっと小さい円を描く関数
-- depth = 今なんばん目の深さか
drawCircle :: Int -> Float -> Float -> Float -> Int -> [Picture]
drawCircle maxDepth x y r depth =
  at (x, y) (Color (hsb (fromIntegral (depth * 40 `mod` 360)) 70 100) (ring r 2))
    : children
  where
    -- まだ深さが残っていたら、半分の大きさの円を左右に描く
    -- ★ ここで「自分自身」を呼んでいる = 再帰
    children
      | depth < maxDepth =
          drawCircle maxDepth (x - r / 2) y (r / 2) (depth + 1)
            ++ drawCircle maxDepth (x + r / 2) y (r / 2) (depth + 1)
      | otherwise = []

-- ---------- やってみよう ----------
-- 1. 左右だけでなく、上下にも円を描いてみよう（円が増えすぎるので深さは 5 くらいまでに）
--    drawCircle maxDepth x (y - r / 2) (r / 2) (depth + 1)
--    drawCircle maxDepth x (y + r / 2) (r / 2) (depth + 1)
-- 2. r / 2 を r / 3 にすると？
-- 3. 円の数は深さが 1 増えるごとに何倍になる？

-- ---------- 道具 ----------

-- 左端を 0 としたマウスの x（p5.js の mouseX）
mouseX :: Env -> Float
mouseX env = fst (envMouse env) + envW env / 2
