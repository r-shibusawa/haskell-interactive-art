-- ============================================
-- 07 蝶（butterfly）
-- V(x) = x⁶ + t·x⁴ + a·x³ + b·x² + c·x。コントロールが 4 つ。t と a を固定して (b, c) の平面で切る
--   t < 0 にすると、カスプの中に小さな「ポケット」が現れる：そこでは谷が 3 つ（左・右・真ん中）
--   ジーマンはこれを「妥協のポケット」と呼んだ：賛成・反対の 2 つに割れた意見の間に、第 3 の中間の立場が生まれる
-- マウス = (b, c) の点 / ↑↓ = t（蝶の因子） / ←→ = a（かたより） / G = 色 / R = 戻す
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import Numeric (showFFloat)

bMin, bMax, cRange :: Float
bMin = -2                  -- ★ 表示範囲
bMax = 3
cRange = 2.5

fieldW, fieldH :: Int
fieldW = 180               -- ★ 色分けの細かさ
fieldH = 135

data World = World
  { wT     :: !Float       -- ★ 固定するコントロール
  , wA     :: !Float
  , wB     :: !Float       -- マウスの点
  , wC     :: !Float
  , wColor :: !Bool
  , wField :: !Picture
  }

world0 :: World
world0 = World (-1.5) 0 0.8 0 True (buildField (-1.5) 0)

main :: IO ()
main = runSketch (sketch "35-07 Butterfly" world0)
  { skBack  = hsb 300 25 10
  , skDraw  = drawAll
  , skEvent = onEvent
  }

dV :: Float -> Float -> Float -> Float -> Float -> Float
dV t a b c x = (((6 * x * x + 4 * t) * x + 3 * a) * x + 2 * b) * x + c

vOf :: World -> Float -> Float
vOf w x = x ^ (6 :: Int) + wT w * x ^ (4 :: Int) + wA w * x * x * x + wB w * x * x + wC w * x

-- 谷（V' が − から + に変わる所）の数
countMinima :: Float -> Float -> Float -> Float -> Int
countMinima t a b c = go 1 (dV t a b c (-2.2)) 0
  where
    go :: Int -> Float -> Int -> Int
    go !i !prev !n
      | i > 140 = n
      | otherwise =
          let cur = dV t a b c (-2.2 + 4.4 * fromIntegral i / 140)
          in go (i + 1) cur (if prev < 0 && cur >= 0 then n + 1 else n)

-- ★ 谷 1・2・3 個の色
palette :: Int -> (Word8, Word8, Word8)
palette 2 = (40, 90, 140)
palette 3 = (200, 90, 150)
palette _ = (24, 30, 48)

buildField :: Float -> Float -> Picture
buildField t a = pixelImage fieldW fieldH 1 $ \i j ->
  palette (countMinima t a
    (bMin + (bMax - bMin) * (fromIntegral i + 0.5) / fromIntegral fieldW)
    (cRange - 2 * cRange * (fromIntegral j + 0.5) / fromIntegral fieldH))

layout :: Env -> (Float, Float, Float, Float)
layout env =
  let py = 110
      ph = envH env - py - 40
      pw = min (envW env * 0.58) (ph * 4 / 3)
  in (30, py, pw, ph)

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      t = wT w
      a = wA w
      (px, py, pw, ph) = layout env
      toB v = px + (v - bMin) / (bMax - bMin) * pw
      toC v = py + (cRange - v) / (2 * cRange) * ph
      panel = hsb 300 20 16
      -- ★ 分岐集合（V' = V'' = 0）：b = −15x⁴ − 6t·x² − 3a·x,  c = 24x⁵ + 8t·x³ + 3a·x²
      setPts = [ if b > bMin - 0.2 && b < bMax + 0.2 && abs c < cRange + 0.2 then Just (toB b, toC c) else Nothing
               | i <- [0 .. 800 :: Int]
               , let x = -1.6 + 0.004 * fromIntegral i
                     x2 = x * x
                     b = -15 * x2 * x2 - 6 * t * x2 - 3 * a * x
                     c = 24 * x2 * x2 * x + 8 * t * x2 * x + 3 * a * x2 ]
      setLines = concat (zipWith link setPts (drop 1 setPts))
      link (Just p) (Just q) = [thickLine 2.5 p q]
      link _ _ = []
      -- 右：V(x)
      rx = px + pw + 30
      rw = wd - rx - 30
      toX x = rx + (x + 1.8) / 3.6 * rw
      xs200 = [ -1.8 + 3.6 * fromIntegral i / 200 | i <- [0 .. 200 :: Int] ] :: [Float]
      vmin = foldr (min . vOf w) 1e9 xs200
      toY v = py + ph - 30 - (v - vmin) * ph * 0.12
      clampY = clampF py (py + ph)
      curve = [ (toX x, clampY (toY (vOf w x))) | x <- xs200 ]
      xsG = [ -1.8 + 3.6 * fromIntegral i / 400 | i <- [0 .. 400 :: Int] ] :: [Float]
      dvs = [ (x, dV t a (wB w) (wC w) x) | x <- xsG ]
      eqs = [ (x, p0 < 0) | ((_, p0), (x, cur)) <- zip dvs (drop 1 dvs), p0 * cur < 0 ]
      mins = length (filter snd eqs)
      eqDots = [ Color (if st then hsb 160 70 100 else hsb 0 70 100) (at (toX x, clampY (toY (vOf w x))) (disc 7)) | (x, st) <- eqs ]
  in Pictures
    [ if wColor w
        then Translate (px + pw / 2 - wd / 2) (h / 2 - py - ph / 2)
               (Scale (pw / fromIntegral fieldW) (ph / fromIntegral fieldH) (wField w))
        else onP5 env (Color panel (box px py pw ph))
    , onP5 env (Pictures
        [ Color (hsbA 0 0 100 20) (Pictures [Line [(toB 0, py), (toB 0, py + ph)], Line [(px, toC 0), (px + pw, toC 0)]])
        , Color (hsb 50 90 100) (Pictures setLines)
        , Color white (at (toB (wB w), toC (wC w)) (disc 7))
        , Color panel (box rx py rw ph)
        , Color (hsb 160 60 90) (thickPath 3 curve)
        , Pictures eqDots
        ])
    , txt env (px + 12) (py + 22) 14 (gray 0.9) "b across, c up. Valleys: dark 1, blue 2, pink 3 (pocket)"
    , txt env (rx + 12) (py + 22) 14 (gray 0.9) ("V(x)  valleys (green) = " ++ show mins)
    , if mins == 3 then txt env (rx + 12) (py + 44) 14 (gray 0.9) "Middle valley = the compromise" else Blank
    , txt env 20 36 18 white ("t = " ++ nf 2 t ++ " (Up/Dn)   a = " ++ nf 2 a ++ " (<- ->)   G: color   R: reset   (b, c) = ("
                              ++ nf 2 (wB w) ++ ", " ++ nf 2 (wC w) ++ ")")
    , txt env 20 62 14 (gray 0.6) "t > 0: a plain cusp (at most 2 valleys). t < 0: the cusp tip splits and a pocket opens."
    , txt env 20 84 14 (gray 0.6) "Moving a away from 0 tilts the pocket to one side."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _
    | mx > px && mx < px + pw && my > py && my < py + ph ->
        w { wB = bMin + (mx - px) / pw * (bMax - bMin), wC = cRange - (my - py) / ph * 2 * cRange }
  EventKey (SpecialKey KeyUp) Down _ _    -> redo w { wT = min 1.5 (wT w + 0.25) }
  EventKey (SpecialKey KeyDown) Down _ _  -> redo w { wT = max (-3) (wT w - 0.25) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> redo w { wA = max (-1.5) (wA w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> redo w { wA = min 1.5 (wA w + 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> w { wColor = not (wColor w) }
    | c `elem` "rR" -> redo w { wT = -1.5, wA = 0 }
  _ -> w
  where
    (mx, my) = mouseP5 env
    (px, py, pw, ph) = layout env
    redo v = v { wField = buildField (wT v) (wA v) }

-- ---------- やってみよう ----------
-- 1. t = 1 → 0 → −1 → −2 と下げて、ポケットが生まれる瞬間を見よう
-- 2. ポケットの中に点を置き、右のグラフで谷が 3 つあることを確かめよう。真ん中の谷はいちばん浅い？
-- 3. a を ±0.5 にすると、ポケットはどちらへ傾く？ 意見の「かたより」と考えるとどういう意味？
-- 4. 03 のように状態 x が坂を下る動き（x − 0.002·V'(x)）を skStep に入れて、マウスで b, c を動かしたときのとび方を見よう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
