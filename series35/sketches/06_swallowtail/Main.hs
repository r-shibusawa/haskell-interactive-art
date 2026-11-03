-- ============================================
-- 06 つばめの尾（swallowtail）
-- V(x) = x⁵ + a·x³ + b·x² + c·x。コントロールが 3 つ (a, b, c) あるので、分岐集合は 3 次元の曲面
--   a を固定して (b, c) の平面で切ると、a < 0 のときツバメの尾のような形（2 つのカスプと交差）が現れる
--   色 = 平衡点（V' = 0）の数：0・2・4。尾の三角の内側だけ平衡点が 4 つ（谷が 2 つ）
-- マウス = (b, c) の点（右に V(x) を表示） / ←→ = a / A = a を自動で動かす / G = 色の表示
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import Numeric (showFFloat)

bRange, cRange :: Float
bRange = 2.5               -- ★ 表示する範囲
cRange = 2.5

fieldW, fieldH :: Int
fieldW = 160               -- ★ 色分けの細かさ
fieldH = 120

data World = World
  { wA     :: !Float       -- ★ 切る位置
  , wAuto  :: !Bool
  , wColor :: !Bool
  , wB     :: !Float       -- マウスの点
  , wC     :: !Float
  , wField :: !Picture     -- 平衡点の数の絵
  , wLastA :: !Float
  , wT     :: !Float
  }

world0 :: World
world0 = World (-2) False True 0 0.4 (buildField (-2)) (-2) 0

main :: IO ()
main = runSketch (sketch "35-06 Swallowtail" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

dV :: Float -> Float -> Float -> Float -> Float
dV x a b c = ((5 * x * x + 3 * a) * x + 2 * b) * x + c

vOf :: World -> Float -> Float
vOf w x = x * x * x * x * x + wA w * x * x * x + wB w * x * x + wC w * x

-- V' = 0 の解の数（x を細かく調べて符号が変わる回数）
countRoots :: Float -> Float -> Float -> Int
countRoots a b c = go 1 (dV (-2.5) a b c) 0
  where
    go :: Int -> Float -> Int -> Int
    go !i !prev !n
      | i > 90 = n
      | otherwise =
          let cur = dV (-2.5 + 5 * fromIntegral i / 90) a b c
          in go (i + 1) cur (if prev * cur < 0 then n + 1 else n)

-- ★ 平衡点の数ごとの色（RGB）：0 = 暗い、2 = 青、4 = 明るい黄
palette :: Int -> (Word8, Word8, Word8)
palette 1 = (40, 40, 60)
palette 2 = (40, 70, 130)
palette 3 = (60, 60, 90)
palette 4 = (150, 120, 40)
palette _ = (22, 22, 38)

buildField :: Float -> Picture
buildField a = pixelImage fieldW fieldH 1 $ \i j ->
  palette (countRoots a
    (negate bRange + 2 * bRange * (fromIntegral i + 0.5) / fromIntegral fieldW)
    (cRange - 2 * cRange * (fromIntegral j + 0.5) / fromIntegral fieldH))

stepWorld :: Env -> World -> World
stepWorld env w =
  let t = if wAuto w then wT w + 0.01 else wT w
      a = if wAuto w then -1 + 1.8 * sin t else wA w
      redo = a /= wLastA w && (envFrame env `mod` 5 == 0 || not (wAuto w))
  in if redo then w { wT = t, wA = a, wField = buildField a, wLastA = a } else w { wT = t, wA = a }

-- 左の図の大きさ
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
      a = wA w
      (px, py, pw, ph) = layout env
      toB v = px + (v + bRange) / (2 * bRange) * pw
      toC v = py + (cRange - v) / (2 * cRange) * ph
      panel = hsb 230 25 16
      -- ★ 分岐集合：V' = 0 と V'' = 0 を同時にみたす (b, c)。x をパラメータにすると
      --   b = −10x³ − 3a·x,  c = 15x⁴ + 3a·x²
      setPts = [ if abs b < bRange * 1.05 && abs c < cRange * 1.05 then Just (toB b, toC c) else Nothing
               | i <- [0 .. 600 :: Int]
               , let x = -1.5 + 0.005 * fromIntegral i
                     b = -10 * x * x * x - 3 * a * x
                     c = 15 * x * x * x * x + 3 * a * x * x ]
      setLines = concat (zipWith link setPts (drop 1 setPts))
      link (Just p) (Just q) = [thickLine 3 p q]
      link _ _ = []
      -- 右：V(x) のグラフ
      rx = px + pw + 30
      rw = wd - rx - 30
      toX x = rx + (x + 1.8) / 3.6 * rw
      toY v = py + ph / 2 - v * ph * 0.06
      clampY = clampF py (py + ph)
      curve = [ (toX x, clampY (toY (vOf w x))) | i <- [0 .. 200 :: Int], let x = -1.8 + 3.6 * fromIntegral i / 200 ]
      xsG = [ -1.8 + 3.6 * fromIntegral i / 400 | i <- [0 .. 400 :: Int] ] :: [Float]
      dvs = [ (x, dV x a (wB w) (wC w)) | x <- xsG ]
      eqs = [ (x, p0 < 0) | ((_, p0), (x, cur)) <- zip dvs (drop 1 dvs), p0 * cur < 0 ]
      n = length eqs
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
        , Color (hsbA 0 0 100 15) (Line [(rx, toY 0), (rx + rw, toY 0)])
        , Color (hsb 160 60 90) (thickPath 3 curve)
        , Pictures eqDots
        ])
    , txt env (px + 12) (py + 22) 14 (gray 0.9) "b across, c up. Equilibria: dark 0, blue 2, yellow 4"
    , txt env (rx + 12) (py + 22) 14 (gray 0.85) ("V(x)  " ++ show n ++ " equilibria (green " ++ show mins ++ ", red " ++ show (n - mins) ++ ")")
    , txt env (rx + 12) (py + 44) 14 (gray 0.85) "x^5: the left end always falls away,"
    , txt env (rx + 12) (py + 66) 14 (gray 0.85) "so a valley is only stable for now."
    , txt env 20 36 18 white ("a = " ++ nf 2 a ++ " (<- ->)   A: auto " ++ onOff (wAuto w) ++ "   G: color " ++ onOff (wColor w)
                              ++ "   mouse = (b, c) = (" ++ nf 2 (wB w) ++ ", " ++ nf 2 (wC w) ++ ")")
    , txt env 20 62 14 (gray 0.6) "a > 0: no tail, just a fold line. a < 0: the line crosses itself, making a tail and 2 cusps."
    , txt env 20 84 14 (gray 0.6) "Each time you cross the line, 2 equilibria are born or vanish."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _
    | mx > px && mx < px + pw && my > py && my < py + ph ->
        w { wB = (mx - px) / pw * 2 * bRange - bRange, wC = cRange - (my - py) / ph * 2 * cRange }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wA = max (-3) (wA w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = min 1.5 (wA w + 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "gG" -> w { wColor = not (wColor w) }
  _ -> w
  where
    (mx, my) = mouseP5 env
    (px, py, pw, ph) = layout env

-- ---------- やってみよう ----------
-- 1. a = −2 で、尾の三角の内側・尾の下・外側にマウスを置き、右の V(x) の谷の数を数えよう
-- 2. 2 つのカスプ（とがった点）の位置を式で求めてみよう（ヒント：V''' = 0 も足す → 60x² + 6a = 0）
-- 3. a を 0 にすると尾が 1 点につぶれる。この点が「つばめの尾の点」
-- 4. 03 のように (a, b, c) の 3D で分岐集合の曲面を描いてみよう（V3 と project を使う）


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
