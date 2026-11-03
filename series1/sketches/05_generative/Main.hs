-- ============================================
-- 05 ジェネラティブアート
-- コンピュータがランダムに作品をつくる
-- クリックするたびに新しい作品が生まれる / S で保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)

-- 状態は「いま表示している作品の絵」だけ
newtype World = World Picture

cellStep :: Float
cellStep = 50              -- ★ マス目の大きさ

background :: Color
background = hsb 40 10 97

main :: IO ()
main = runSketch (sketch "01-05 Generative Art" (World Blank))
  { skBack  = background
  , skStart = \env -> World <$> generate env      -- 最初の作品をつくる
  , skDraw  = \_ (World pic) -> pic
  , skEvent = onEvent
  }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w@(World pic) = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> World <$> generate env   -- もう 1 作品
  EventKey (Char c) Down _ _
    | c == 's' || c == 'S' -> do
        saveSvg "generative" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), pic])
        pure w
  _ -> pure w

-- 作品を 1 つつくる（乱数を使うので IO）
generate :: Env -> IO Picture
generate env = do
  baseHue <- randF 0 360                 -- 今回のテーマカラーをランダムに決める
  -- リスト内包表記で、マス目を全部まわる
  let along size = takeWhile (< size) (iterate (+ cellStep) (cellStep / 2))
      centers = [ (x, y) | x <- along (envW env), y <- along (envH env) ]
  Pictures <$> forM centers (\(x, y) -> cell baseHue (toGloss env (x, y)))

-- マス 1 つぶんの図形
cell :: Float -> Point -> IO Picture
cell baseHue c = do
  -- テーマカラーの近くの色を選ぶ
  hueShift <- randF (-40) 40             -- ★ randF (-180) 180 にすると色はどうなる？
  sat <- randF 50 90
  bri <- randF 70 100
  size <- randF 10 (cellStep * 1.2)
  r <- randF 0 1                         -- 0〜1 のランダムな数
  let h = wrapF 360 (baseHue + hueShift)
      fillCol = hsba h sat bri 0.85
  -- r の値で、描く図形を変える
  pure $ if r < 0.3 then Color fillCol (at c (disc (size / 2)))
    else if r < 0.6 then Color fillCol (at c (roundSquare size 6))
    else if r < 0.85 then Color fillCol (at c (triangleOf size))
    else Color (hsb h 80 60) (at c (ring (size * 0.8 / 2) 3))     -- 線だけの円

-- ---------- やってみよう ----------
-- 1. cellStep を 20 や 100 にすると？
-- 2. randF (-40) 40 を randF (-180) 180 にすると色はどうなる？
-- 3. 好きな図形を追加して、自分だけのルールをつくろう

-- ---------- 道具 ----------

-- 左上が原点の座標 → gloss の座標
toGloss :: Env -> Point -> Point
toGloss env (x, y) = (x - envW env / 2, envH env / 2 - y)

randF :: Float -> Float -> IO Float
randF a b = randomRIO (a, b)

-- 上向きの三角形（幅・高さ s）
triangleOf :: Float -> Picture
triangleOf s = Polygon [(0, s / 2), (-s / 2, -s / 2), (s / 2, -s / 2)]

-- 角の丸い正方形（中心が原点、一辺 s、角の丸み r）
roundSquare :: Float -> Float -> Picture
roundSquare s r0 = Polygon (concatMap corner [(1, 1, 0), (-1, 1, 90), (-1, -1, 180), (1, -1, 270)])
  where
    r = min r0 (s / 2)
    q = s / 2 - r
    corner (sx, sy, a0) =
      [ (sx * q + r * cos a, sy * q + r * sin a)
      | i <- [0 .. 4 :: Int], let a = (a0 + fromIntegral i * 90 / 4) * pi / 180 ]
