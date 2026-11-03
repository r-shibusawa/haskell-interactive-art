-- ============================================
-- 03 レインボーブラシ
-- 色がどんどん変わる＆速く動かすと太くなる筆
-- C キーで消す / S キーで保存
-- ============================================
module Main (main) where

import Art

data World = World
  { wStrokes :: ![Picture]   -- 描いた線（新しいものが先頭）
  , wHue     :: !Float       -- 色相（色の種類）を覚えておく変数
  , wPrev    :: !Point       -- 1 フレーム前のマウス位置
  , wWasDown :: !Bool        -- 1 フレーム前にマウスを押していたか
  }

maxStrokes :: Int
maxStrokes = 7500            -- ★ 覚えておく線の数（超えると古い線から消える）

background :: Color
background = hsb 0 0 10

main :: IO ()
main = runSketch (sketch "01-03 Rainbow Brush" (World [] 0 (0, 0) False))
  { skBack  = background
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawStrokes w
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w
  | envDown env = w
      { wStrokes = if moved then take maxStrokes (stroke : wStrokes w) else wStrokes w
      , wHue = nextHue
      , wPrev = m
      , wWasDown = True
      }
  | otherwise = w { wPrev = m, wWasDown = False }
  where
    m = envMouse env
    moved = m /= wPrev w || not (wWasDown w)
    -- マウスの速さ = 前のフレームからの移動距離
    speed = dist2 (wPrev w) m
    -- 速さ 0〜50 を 太さ 4〜40 に変換（範囲外は切り捨て）
    weight = clampF 4 40 (remap speed 0 50 4 40)       -- ★ remap speed 0 50 40 4 にすると？
    stroke = Color (hsba (wHue w) 80 100 0.8)          -- ★ 彩度 80 を 30 にするとパステルカラー
                   (capsule weight (wPrev w) m)
    -- 少しずつ色を変える。一周したら最初に戻る
    hue1 = wHue w + 2                                  -- ★ 色の変わる速さ
    nextHue = if hue1 > 360 then 0 else hue1

drawStrokes :: World -> Picture
drawStrokes w = Pictures (reverse (wStrokes w))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wStrokes = [] }
    | c == 's' || c == 'S' -> do
        saveSvg "rainbow" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), drawStrokes w])
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. wHue w + 2 の 2 を 10 にすると？
-- 2. 太さの関係を逆にしてみよう（速いと細い）→ remap speed 0 50 40 4
-- 3. 彩度 80 を 30 にすると、パステルカラーになる

-- ---------- 道具 ----------

-- 両はしが丸い太い線を、1 つの多角形で描く（太さ w）
capsule :: Float -> Point -> Point -> Picture
capsule w (x1, y1) (x2, y2) = Polygon (cap x2 y2 (ang - pi / 2) ++ cap x1 y1 (ang + pi / 2))
  where
    ang = if x1 == x2 && y1 == y2 then 0 else atan2 (y2 - y1) (x2 - x1)
    cap cx cy a0 =
      [ (cx + w / 2 * cos a, cy + w / 2 * sin a)
      | i <- [0 .. 4 :: Int], let a = a0 + pi * fromIntegral i / 4 ]
