-- ============================================
-- 02 サンプリング（連続な波を、点にする）
-- コンピュータは波を「1 秒間に何回か」測った点でしか持てない
-- 点が少なすぎると、元の波とはちがう波に見えてしまう（エイリアシング）
-- マウス左右 = サンプリング周波数 / 上下 = 波の周波数
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

main :: IO ()
main = runSketch (sketch "05-02 Sampling" ())
  { skBack = hsb 220 40 10
  , skDraw = \env _ -> drawWorld env
  }

drawWorld :: Env -> Picture
drawWorld env = Pictures (original : stems ++ dots ++ [rebuilt, verdict, info1, info2])
  where
    ww = envW env
    h = envH env
    (mx, my) = mouseP env
    sampleRate = floor (mapC mx 0 ww 3 60) :: Int   -- ★ 画面内に何個の点を取るか
    signalFreq = mapC my 0 h 20 1                   -- ★ 波の周波数（画面内に何周期あるか）
    left = 60
    right = ww - 60
    mid = h * 0.5
    amp = h * 0.3
    -- ---- 元の波（連続）----
    original = Color (hsba 170 60 100 0.5) $
      thickPath 2 [ pt env x (mid - sin (tau * signalFreq * (x - left) / (right - left)) * amp)
                  | x <- [left, left + 2 .. right] ]
    -- ---- サンプル点 ----
    samples =
      [ (remap t 0 1 left right, sin (tau * signalFreq * t))   -- ★ この瞬間の値だけを測る
      | i <- [0 .. sampleRate - 1]
      , let t = fromIntegral i / fromIntegral sampleRate ]
    stems = [ Color (hsb 40 90 100) (Line [pt env x mid, pt env x (mid - y * amp)]) | (x, y) <- samples ]
    dots = [ Color (hsb 40 90 100) (at (pt env x (mid - y * amp)) (disc 5)) | (x, y) <- samples ]
    -- ---- 点だけから波を「復元」する（点を線でつなぐ）----
    rebuilt = Color (hsb 0 90 100) (thickPath 3 [ pt env x (mid - y * amp) | (x, y) <- samples ])
    -- 判定：ナイキストの条件 サンプリング周波数 > 2 × 波の周波数
    ok = fromIntegral sampleRate > 2 * signalFreq
    verdict = text' env 60 (h - 60) 26 (hsb (if ok then 120 else 0) 80 100)
      (if ok then "OK: can rebuild (samples > 2 x wave frequency)" else "NG: looks like another wave! (aliasing)")
    info1 = text' env 20 40 18 (hsb 0 0 100)
      ("Samples " ++ show sampleRate ++ " (left/right)   Wave " ++ printf "%.1f" signalFreq ++ " cycles (up/down)")
    info2 = text' env 20 68 16 (hsb 0 0 100) "Cyan = the true wave   Red = rebuilt from the dots only"

-- ---------- やってみよう ----------
-- 1. サンプルを減らしていき、復元した波がおかしくなる境目を探そう。波の周波数の 2 倍？
-- 2. サンプル数 = 波の周期数 ぴったりにすると、まっすぐな線になる（波が消える！）
-- 3. CD は 44100 Hz。人の耳の上限 20000 Hz の 2 倍より少し多い理由がこれ

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標（中央が原点、y は上向き）に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の map(v, a, b, c, d, true)：範囲からはみ出さない
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str
