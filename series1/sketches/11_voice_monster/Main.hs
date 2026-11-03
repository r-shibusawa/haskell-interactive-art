-- ============================================
-- 11 声で動くモンスター
-- 声を出すと口が開いて、大声だと真っ赤になって震える！
-- 目はマウスを追いかける / クリックでマイク ON（使えないときはデモの信号）
-- ============================================
module Main (main) where

import Art

data World = World
  { wMic   :: !(Maybe Mic)
  , wLive  :: !Bool
  , wVol   :: !Float     -- なめらかにした音量
  , wShake :: !Float     -- 震えの大きさ
  }

loudest :: Float
loudest = 0.25           -- ★ どのくらいの声で最大になるか（感度）

-- 音量 → 口の開き具合（0〜1）
openOf :: World -> Float
openOf w = clampF 0 1 (wVol w / loudest)

main :: IO ()
main = runSketch (sketch "01-11 Voice Monster" (World Nothing False 0 0))
  { skBack  = rgb255 255 245 230
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = do
  level <- maybe (pure 0) micLevel (wMic w)
  live <- maybe (pure False) micLive (wMic w)
  -- 音量を取得して、なめらかにする
  let w' = w { wVol = lerpF (wVol w) level 0.3, wLive = live }
  -- 大声のときは震える
  shake <- if openOf w' > 0.7 then randomRIO (-8, 8) else pure 0
  pure w' { wShake = shake }

outline :: Color
outline = gray (50 / 255)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures [at center monster, message]
  where
    open = openOf w
    center = (wShake w, -wShake w)        -- p5.js の +shake は右下へ動く
    monster = Pictures
      [ body open
      , horn 1, horn (-1)
      , eye env center open (-65)
      , eye env center open 65
      , mouth open
      ]
    message = labelC 0 (envH env / 2 - 50) 22 (gray (60 / 255)) $ case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _ | open > 0.7 -> "ROAR!!!"
             | wLive w -> "Make some noise!"
             | otherwise -> "No mic found: demo signal"

-- ---- 体（緑 → 赤に変わる）----
body :: Float -> Picture
body open = outlined 6 (mixRGB calm angry open) (0, -20) 320 340
  where
    calm = (110, 200, 120)     -- ★ 落ち着いているときの色
    angry = (240, 60, 60)      -- ★ 怒っているときの色

-- ---- ツノ（side は左右の向き）----
horn :: Float -> Picture
horn side = Pictures
  [ Color (rgb255 255 230 120) (Polygon pts)
  , Color outline (thickPath 6 (pts ++ take 1 pts))
  , Color outline (Pictures (map (\q -> at q (disc 3)) pts))
  ]
  where pts = [(side * 110, 110), (side * 70, 200), (side * 40, 130)]

-- ---- 目（マウスを見る）----
eye :: Env -> Point -> Float -> Float -> Picture
eye env (ox, oy) open x = Pictures
  [ outlined 5 (Color255 255 255 255) (x, y) size size
  , Color (gray (30 / 255)) (at (px, py) (disc 17))
  , Color white (at (px + 6, py + 6) (disc 5))      -- 目のハイライト
  ]
  where
    y = 40
    size = 80 + open * 20                   -- 大声で目も大きくなる
    (mx, my) = envMouse env
    -- 黒目をマウスの方向へ動かす（震えているぶんも考える）
    angle = atan2 (my - (oy + y)) (mx - (ox + x))
    px = x + cos angle * 18
    py = y + sin angle * 18

-- ---- 口 ----
mouth :: Float -> Picture
mouth open = Pictures (outlined 6 (Color255 80 20 40) (0, -80) 150 mouthH : teeth)
  where
    mouthH = 10 + open * 140
    top = -80 + mouthH / 2 - 4
    tooth s = Polygon [(s * 40, top), (s * 24, top), (s * 32, top - 20)]
    -- 歯
    teeth | mouthH > 30 = [Color white (tooth (-1)), Color white (tooth 1)]
          | otherwise = []

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 体の色 calm / angry を好きな色に変えよう
-- 2. loudest 0.25（どのくらいの声で最大になるか）を変えて、感度を調整しよう
-- 3. 大声のときに、06 の花火を口から出してみよう

-- ---------- 道具 ----------

-- 色（赤, 緑, 青）。0〜255
data Color255 = Color255 !Float !Float !Float

-- 2 つの色の間をとる（p5.js の lerpColor）
mixRGB :: (Float, Float, Float) -> (Float, Float, Float) -> Float -> Color255
mixRGB (r1, g1, b1) (r2, g2, b2) t = Color255 (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t)

-- ふちのあるだ円。中心 c、幅 w、高さ h、ふちの太さ t
outlined :: Float -> Color255 -> Point -> Float -> Float -> Picture
outlined t (Color255 r g b) c w h = at c (Pictures
  [ Color outline (Scale ((w + t) / 2) ((h + t) / 2) (disc 1))
  , Color (makeColor (r / 255) (g / 255) (b / 255) 1) (Scale (max 0 (w - t) / 2) (max 0 (h - t) / 2) (disc 1))
  ])
