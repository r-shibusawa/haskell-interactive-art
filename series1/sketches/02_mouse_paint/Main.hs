-- ============================================
-- 02 お絵かきツール
-- ドラッグで線を描く / C キーで消す / S キーで保存
-- ============================================
-- gloss は毎フレーム画面を描きなおすので、p5.js のように「描いたものが残る」
-- ことはない。そこで、描いた線をリストに貯めて、毎回ぜんぶ描く。
module Main (main) where

import Art

data World = World
  { wStrokes :: ![Picture]   -- これまでに描いた線（新しいものが先頭）
  , wPrev    :: !Point       -- 1 フレーム前のマウス位置（p5.js の pmouseX, pmouseY）
  , wWasDown :: !Bool        -- 1 フレーム前にマウスを押していたか
  }

maxStrokes :: Int
maxStrokes = 7500            -- ★ 覚えておく線の数（超えると古い線から消える）

background :: Color
background = gray (20 / 255)

main :: IO ()
main = runSketch (sketch "01-02 Mouse Paint" (World [] (0, 0) False))
  { skBack  = background
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawStrokes w
  , skEvent = onEvent
  }

-- 押している間、前のフレームの位置から今の位置まで線を足す
step :: Env -> World -> World
step env w = w { wStrokes = strokes, wPrev = m, wWasDown = envDown env }
  where
    m = envMouse env
    -- 止まっている間は同じ点を増やさない（押した瞬間の 1 点だけは描く）
    moved = m /= wPrev w || not (wWasDown w)
    strokes
      | envDown env && moved = take maxStrokes (newStroke (wPrev w) m : wStrokes w)
      | otherwise = wStrokes w

newStroke :: Point -> Point -> Picture
newStroke a b = Color white            -- ★ 線の色
                      (capsule 8 a b)  -- ★ 線の太さ

drawStrokes :: World -> Picture
drawStrokes w = Pictures (reverse (wStrokes w))   -- 古い線から順に描く

-- キーが押されたときに 1 回だけ呼ばれる
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wStrokes = [] }     -- 全部消す
    | c == 's' || c == 'S' -> do                           -- 画像として保存
        saveSvg "my-drawing" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), drawStrokes w])
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. newStroke の white を rgb255 200 100 255 にすると？
--    （毎回ちがう色にしたいときは、step を IO にして randomRIO を使う）
-- 2. 太さ 8 を (snd b + 400) / 20 にすると、高さで太さが変わる
-- 3. capsule の代わりに at b (disc 10) にすると？

-- ---------- 道具 ----------

-- 両はしが丸い太い線を、1 つの多角形で描く（太さ w）
capsule :: Float -> Point -> Point -> Picture
capsule w (x1, y1) (x2, y2) = Polygon (cap x2 y2 (ang - pi / 2) ++ cap x1 y1 (ang + pi / 2))
  where
    ang = if x1 == x2 && y1 == y2 then 0 else atan2 (y2 - y1) (x2 - x1)
    cap cx cy a0 =
      [ (cx + w / 2 * cos a, cy + w / 2 * sin a)
      | i <- [0 .. 4 :: Int], let a = a0 + pi * fromIntegral i / 4 ]
