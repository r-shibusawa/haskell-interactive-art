-- ============================================
-- 06 コッホ雪片
-- 線を 4 つに折る、を全部の線にくり返す
-- クリックで世代を進める（0〜6）/ マウス上下 = 折る角度
-- ============================================
module Main (main) where

import Art

newtype World = World { wLevel :: Int }

main :: IO ()
main = runSketch (sketch "02-06 Koch Snowflake" (World 0))
  { skBack  = hsb 200 50 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [Color (hsb 190 50 100) outline, info]
  where
    ww = envW env
    wh = envH env
    level = wLevel w
    -- 折る角度：ふつうのコッホ曲線は 60°。マウスで変えると別の形になる
    bend = clampF 20 85 (remap (wh / 2 - snd (envMouse env)) 0 wh 85 20)
    -- 最初は正三角形（3 本の線）
    s = min ww wh * 0.6
    cy = -s * 0.12
    start = [(0, cy + s * 0.577), (s / 2, cy - s * 0.289), (-s / 2, cy - s * 0.289)]
    -- level 回、全部の線を折る
    pts = iterate (fold (bend * pi / 180)) start !! level
    closed = case pts of
      (p : _) -> pts ++ [p]
      [] -> []
    weight = remap (fromIntegral level) 0 6 3 1
    outline
      -- 世代 6 は太さ 1 なので細い線 1 本で描く（線が 12288 本あるので、そのほうが軽い）
      | level >= 6 = poly pts
      | level <= 3 = Pictures (thickPath weight closed : [at p (disc (weight / 2)) | p <- pts])
      | otherwise = thickPath weight closed
    info = label (-ww / 2 + 20) (wh / 2 - 30) 18 white
      ( "Level " ++ show level ++ "   Lines " ++ show (length pts) ++ "   Angle "
        ++ show (round bend :: Int) ++ " deg   Click: next level" )

-- 点の列を受け取り、となり合う 2 点の間を「4 本の線」に折って返す
fold :: Float -> [Point] -> [Point]
fold angle pts = concat (zipWith piece pts (drop 1 pts ++ take 1 pts))  -- 最後は最初につなぐ
  where
    piece a@(ax, ay) (bx, by) =
      let dx = (bx - ax) / 3                -- 線の 1/3
          dy = (by - ay) / 3
          p1 = (ax + dx, ay + dy)           -- 1/3 の点
          p3 = (ax + 2 * dx, ay + 2 * dy)   -- 2/3 の点
          -- とがった先の点：p1 から angle だけ回した方向へ（二等辺三角形になる長さ）
          k = 0.5 / cos angle
          rx = (dx * cos angle - dy * sin angle) * k
          ry = (dx * sin angle + dy * cos angle) * k
          p2 = (fst p1 + rx, snd p1 + ry)
      in [a, p1, p2, p3]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wLevel = (wLevel w + 1) `mod` 7 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 世代 0 → 6 と進めて、線の数が 4 倍ずつ増えるのを確かめよう
-- 2. 角度を変えて、雪の結晶・ギザギザの島・星をつくろう
-- 3. 最初の形を三角形ではなく四角形（4 点）にすると？
