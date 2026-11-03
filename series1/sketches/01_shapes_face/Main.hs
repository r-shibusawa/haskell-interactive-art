-- ============================================
-- 01 図形と色でキャラクターをつくる
-- 目がマウスを追いかけるロボット。マウスを押すと口が開く
-- ============================================
-- 座標： gloss は画面の中央が (0, 0)、y は上向きが正。
-- ここでは p5.js と同じ「600 × 400、左上が (0, 0)、y は下向き」で考えて、
-- 関数 p で gloss の座標に直してから、全体を画面いっぱいに拡大する。
module Main (main) where

import Art

main :: IO ()
main = runSketch (sketch "01-01 Robot Face" ())
  { skBack = rgb255 250 240 220           -- クリーム色の背景
  , skDraw = \env _ -> drawScene env
  }

-- 600 × 400 の絵を何倍にすれば画面に収まるか
zoomOf :: Env -> Float
zoomOf env = min (envW env / 600) (envH env / 400)

drawScene :: Env -> Picture
drawScene env = Pictures [ground, Scale k k robot]
  where
    k = zoomOf env
    w = envW env
    h = envH env
    -- ---- 地面（y = 330 より下。横は画面いっぱい）----
    groundTop = (200 - 330) * k
    groundBottom = -h / 2
    ground = Color (rgb255 120 200 140)
      (rect 0 ((groundTop + groundBottom) / 2) w (groundTop - groundBottom))
    -- マウスの位置を p5.js と同じ「左上が原点」に直す
    (mx, my) = envMouse env
    mouseX = mx + w / 2
    mouseY = h / 2 - my
    -- ---- 黒目（マウスのほうを向く）----
    -- remap (値) (元の最小) (元の最大) (新しい最小) (新しい最大)
    dx = remap mouseX 0 w (-15) 15        -- ★ 黒目の動く範囲
    dy = remap mouseY 0 h (-15) 15
    robot = Pictures
      [ antenna
      , face
      , whiteEye 250, whiteEye 350
      , pupil (250 + dx) (180 + dy), pupil (350 + dx) (180 + dy)
      , mouth (envDown env)
      , cheek 215 240, cheek 385 240
      ]

outline :: Color
outline = gray (60 / 255)

-- ---- アンテナ ----
antenna :: Picture
antenna = Pictures
  [ Color outline (thickLine 6 (p 300 100) (p 300 50))
  , Color (rgb255 255 80 80) (at (p 300 45) (disc 12))
  ]

-- ---- 顔（ふちの分だけ大きい四角を下に敷く）----
face :: Picture
face = Pictures
  [ Color outline (roundRect 187 97 226 206 43)
  , Color (rgb255 255 210 90) (roundRect 193 103 214 194 37)   -- ★ 顔の色
  ]

-- ---- 白目 ----
whiteEye :: Float -> Picture
whiteEye x = Pictures
  [ Color outline (at (p x 180) (disc 38))
  , Color white (at (p x 180) (disc 32))
  ]

-- ---- 黒目 ----
pupil :: Float -> Float -> Picture
pupil x y = Color (gray (40 / 255)) (at (p x y) (disc 15))

-- ---- 口（押すと開く）----
mouth :: Bool -> Picture
mouth True = Pictures
  [ Color outline (ellipseAt 300 255 86 56)
  , Color (rgb255 200 60 80) (ellipseAt 300 255 74 44)
  ]
mouth False = Color outline (Pictures
  [ thickPath 6 smile
  , at (p 260 240) (disc 3)
  , at (p 340 240) (disc 3)
  ])
  where
    -- 半円のにっこり口（y は下向きなので、0〜π は下側の半円）
    smile = [ p (300 + 40 * cos t) (240 + 20 * sin t)
            | i <- [0 .. 24 :: Int], let t = pi * fromIntegral i / 24 ]

-- ---- ほっぺ ----
cheek :: Float -> Float -> Picture
cheek x y = Color (rgba255 255 150 150 150) (at (p x y) (disc 15))   -- 4 つ目の数字は透明度

-- ---------- やってみよう ----------
-- 1. 色を変えて、自分だけのロボットにしよう
-- 2. 耳や体を roundRect / disc / Polygon で付け足そう
--    例） Polygon [p 100 100, p 150 100, p 125 50]
-- 3. 黒目の動く範囲（-15, 15）を大きくすると？

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
p :: Float -> Float -> Point
p x y = (x - 300, 200 - y)

-- 中心 (x, y)、幅 w、高さ h のだ円
ellipseAt :: Float -> Float -> Float -> Float -> Picture
ellipseAt x y w h = at (p x y) (Scale (w / 2) (h / 2) (disc 1))

-- 角の丸い四角。(x, y) は左上の角、r は角の丸み
roundRect :: Float -> Float -> Float -> Float -> Float -> Picture
roundRect x y w h r = Polygon (concat
  [ corner (x + w - r) (y + r) (-90)
  , corner (x + w - r) (y + h - r) 0
  , corner (x + r) (y + h - r) 90
  , corner (x + r) (y + r) 180
  ])
  where
    corner cx cy a0 =
      [ p (cx + r * cos a) (cy + r * sin a)
      | i <- [0 .. 8 :: Int], let a = (a0 + fromIntegral i * 90 / 8) * pi / 180 ]
