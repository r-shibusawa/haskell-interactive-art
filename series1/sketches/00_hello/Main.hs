-- ============================================
-- 00 はじめてのスケッチ
-- マウスを動かすと、円がついてくる
-- ============================================
module Main (main) where

import Art

-- main … p5.js の setup / draw をレコードにまとめて渡す
-- このスケッチは何も覚えておかなくてよいので、状態は () （空っぽ）
main :: IO ()
main = runSketch (sketch "01-00 Hello" ())
  { skBack = gray (30 / 255)              -- ★ 背景の色（0=黒 〜 1=白）
  , skDraw = \env _ -> drawCircle env     -- 毎秒約 60 回よばれて、絵を返す
  }

drawCircle :: Env -> Picture
drawCircle env =
  at (envMouse env)                       -- マウスの位置に動かして
     (Color (rgb255 255 100 150)          -- ★ 塗りの色（赤, 緑, 青）それぞれ 0〜255
            (disc 25))                    -- ★ 半径 25 の円（p5.js の直径 50）

-- ---------- やってみよう ----------
-- 1. rgb255 の 3 つの数字を変えて、好きな色にしよう
-- 2. disc 25 を disc 50 にすると？
-- 3. envMouse の座標は画面の中央が (0, 0)、y は上向きが正。
--    at (0, 0) にすると円はどこに出る？
