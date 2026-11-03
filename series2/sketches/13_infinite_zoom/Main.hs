-- ============================================
-- 13 無限ズーム
-- シェルピンスキーのカーペットに、どこまでも吸い込まれていく
-- 「拡大しても同じ形」= 自己相似 だから、ずっとズームし続けられる
-- マウス左右 = 速さ / クリックで 一時停止
-- ============================================
module Main (main) where

import Art

data World = World
  { wT      :: !Float    -- ズームの進み具合
  , wPaused :: !Bool
  }

minHole :: Float
minHole = 3         -- ★ これより小さい穴は描かない（画面ではほとんど見えない）

background :: Color
background = hsb 0 0 4

main :: IO ()
main = runSketch (sketch "02-13 Infinite Zoom" (World 0 False))
  { skBack  = background
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w = w { wT = if t >= 1 then t - 1 else t }  -- 1 周したら最初に戻す（9 倍ズームで元の形と重なる）
  where
    ww = envW env
    speed = clampF 0.001 0.01 (remap (fst (envMouse env) + ww / 2) 0 ww 0.001 0.01)
    t = if wPaused w then wT w else wT w + speed

-- 見える範囲とズーム
data View = View { vW, vH, vS, vZoom :: !Float, vLeaf :: !Int }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color col (square (-s / 2) (-s / 2) s)
  , Color background (Pictures (holes view (-s / 2) (-s / 2) s 0))
  , Color (withA 0.7 black) (rect 0 (envH env / 2 - 22) (envW env) 44)   -- 文字を読みやすくする帯
  , label (-envW env / 2 + 20) (envH env / 2 - 30) 16 white
      ("Mouse left/right: speed   Click: pause   Squares " ++ show (countLeaves view (-s / 2) (-s / 2) s 0))
  ]
  where
    -- 9^t 倍に拡大（左上→右下 の順に 2 回たどる ＝ 9 倍 で同じ形に戻る）
    zoom = 9 ** wT w
    s = min (envW env) (envH env) * 0.9
    view = View (envW env) (envH env) s zoom (leafDepth (s * zoom) 0)
    col = hsb (wrapF 360 (fromIntegral (vLeaf view) * 25 + fromIntegral (envFrame env) * 0.2)) 60 100
    -- 四角 (x, y, 大きさ)。x, y は p5.js と同じ「左上」の座標（y は下向き）
    square x y size =
      let z = vZoom view
          sz = size * z
          left = (x - s / 4) * z
          top = (y - s / 4) * z
      in rect (left + sz / 2) (-(top + sz / 2)) sz sz

-- 同じ深さの四角は同じ大きさなので、塗りつぶす深さは 1 つに決まる
leafDepth :: Float -> Int -> Int
leafDepth screenSize depth
  | screenSize < 8 || depth > 12 = depth
  | otherwise = leafDepth (screenSize / 3) (depth + 1)

-- この四角は画面の外か（これで無限に深くても速い）
outside :: View -> Float -> Float -> Float -> Bool
outside v x y size = left > vW v / 2 || top > vH v / 2 || left + sz < -vW v / 2 || top + sz < -vH v / 2
  where
    z = vZoom v
    sz = size * z
    left = (x - vS v / 4) * z
    top = (y - vS v / 4) * z

-- カーペット：真ん中に穴をあけて、残りの 8 個にも同じことをする
-- （p5.js 版は 8 個の四角を塗るが、最大で 3 万個を超えるので、
--   全体を 1 色で塗ってから穴を背景色で描く。見た目はほぼ同じで、数は 7 分の 1 になる）
holes :: View -> Float -> Float -> Float -> Int -> [Picture]
holes v x y size depth
  | outside v x y size || depth >= vLeaf v || u * z < minHole = []
  | otherwise = [hole | not (outside v (x + u) (y + u) u)] ++ concat
      [ holes v (x + i * u) (y + j * u) u (depth + 1)
      | i <- [0, 1, 2], j <- [0, 1, 2]
      , not (i == 1 && j == 1) ]         -- 真ん中は穴
  where
    u = size / 3
    z = vZoom v
    sz = u * z
    left = (x + u - vS v / 4) * z
    top = (y + u - vS v / 4) * z
    hole = rect (left + sz / 2) (-(top + sz / 2)) sz sz

-- 画面に映っている「塗る四角」の数
countLeaves :: View -> Float -> Float -> Float -> Int -> Int
countLeaves v x y size depth
  | outside v x y size = 0
  | depth >= vLeaf v = 1
  | otherwise = sum
      [ countLeaves v (x + i * u) (y + j * u) u (depth + 1)
      | i <- [0, 1, 2], j <- [0, 1, 2]
      , not (i == 1 && j == 1) ]
  where u = size / 3

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPaused = not (wPaused w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 穴を「真ん中」から「左上」に変えると、ズームの中心もずれる（どこに変えればいい？）
-- 2. 色の式を変えて、深さごとに虹色にしよう
-- 3. カーペットではなく、05 の三角形で無限ズームをつくってみよう
