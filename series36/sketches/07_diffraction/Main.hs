-- ============================================
-- 07 単スリット・多スリットの回折（フラウンホーファー回折）
-- 幅 a のスリットを N 本、間隔 d で並べたときの遠くのスクリーンの明るさ：
--   I(θ) = sinc²(β) × [ sin(Nα) / (N sin α) ]²     β = π a sinθ / λ,  α = π d sinθ / λ
--   sinc² = 1 本のスリットの回折（包絡線）、後ろの項 = N 本の干渉（鋭い山）
-- ←→ = スリット幅 a / ↑↓ = 本数 N / Z X = 間隔 d / Q A = 波長 λ / マウス左右 = 値を読む
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data World = World
  { wA      :: !Float      -- ★ スリット幅（μm）
  , wD      :: !Float      -- ★ スリット間隔（μm）
  , wN      :: !Int        -- ★ スリットの本数
  , wLambda :: !Float      -- ★ 波長（nm）
  }

main :: IO ()
main = runSketch (sketch "36-07 Single and Multiple Slit Diffraction" (World 20 60 2 550))
  { skBack  = rgb255 10 10 16
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 強度（0〜1）と包絡線。s = sinθ
--    （山の頂上では sin(Nα) と N sin α がどちらも小さくなるので、ここだけ精度の高い Double で計算する）
intensity :: World -> Float -> (Float, Float)
intensity w s0 =
  let s = realToFrac s0 :: Double
      lamUm = realToFrac (wLambda w) / 1000 :: Double
      beta = pi * realToFrac (wA w) * s / lamUm
      alpha = pi * realToFrac (wD w) * s / lamUm
      nn = fromIntegral (wN w) :: Double
      env = if beta == 0 then 1 else sin beta / beta
      sa = sin alpha
      inter = if abs sa < 1e-9 then 1 else sin (nn * alpha) / (nn * sa)
  in (realToFrac (env * env * inter * inter), realToFrac (env * env))

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      a = wA w
      d = wD w
      n = wN w
      lamUm = wLambda w / 1000
      smax = min 0.95 (3.2 * lamUm / a)        -- 表示する sinθ の範囲（1 本の回折の 3 番目の暗線まで）
      x0 = 80
      x1 = wd - 80
      bigW = x1 - x0
      toS x = remap x x0 x1 (negate smax) smax
      toX s = remap s (negate smax) smax x0 x1
      (cr, cg, cb) = wavelengthToRGB (wLambda w)
      c = col255 (cr, cg, cb)
      -- スクリーンの像（上の帯）
      bandY = 110
      bandH = 70
      nCols = floor bigW :: Int
      band = [ Color (makeColor (cr / 255 * g) (cg / 255 * g) (cb / 255 * g) 1) (box x bandY 1.5 bandH)
             | i <- [0 .. nCols]
             , let x = x0 + fromIntegral i
                   g = sqrt (fst (intensity w (toS x))) ]    -- 目の感じ方に合わせて明るさを圧縮
      -- グラフ
      gy0 = 230
      gh = h - 350
      gy1 = gy0 + gh
      envCurve = [ (x, gy1 - snd (intensity w (toS x)) * gh)
                 | i <- [0, 2 .. nCols], let x = x0 + fromIntegral i ]
      iCurve = [ (x, gy1 - fst (intensity w (toS x)) * gh)
               | i <- [0 .. nCols], let x = x0 + fromIntegral i ]
      -- 単スリットの暗線 sinθ = mλ/a、多スリットの明線 sinθ = mλ/d
      darkTicks = [ Line [(xx, gy1), (xx, gy1 - 18)]
                  | m <- [-8 .. 8 :: Int], m /= 0
                  , let s = fromIntegral m * lamUm / a, abs s < smax, let xx = toX s ]
      brightTicks = if n <= 1 then [] else
                  [ Line [(xx, gy0), (xx, gy0 + 14)]
                  | m <- [-40 .. 40 :: Int]
                  , let s = fromIntegral m * lamUm / d, abs s < smax, let xx = toX s ]
      -- スリットの絵
      sy = 60
      totalW = fromIntegral n * d
      scaleS = min 1.5 (300 / max totalW 1)
      left = wd - 100 - totalW * scaleS
      wall = thickLine 6 (left - 40, sy) (left, sy)
           : [ thickLine 6 (xs + a * scaleS, sy) (xs + d * scaleS, sy)
             | i <- [0 .. n - 1], let xs = left + fromIntegral i * d * scaleS ]
      (mx, _) = mouseP5 env
      readout
        | mx > x0 && mx < x1 =
            let s = toS mx
                (i0, _) = intensity w s
            in Pictures
              [ onP5 env (Color (rgba255 255 255 255 60) (Line [(mx, gy0), (mx, gy1)]))
              , txt env (min (mx + 10) (wd - 470)) (gy0 + 40) 14 white
                  ("sin = " ++ nf 4 s ++ "  theta = " ++ nf 2 (asin s * 180 / pi) ++ " deg  I = " ++ nf 3 i0) ]
        | otherwise = Blank
  in Pictures
    [ onP5 env (Pictures
        ( band ++
        [ Color (gray (80 / 255)) (rectLine (x0 + bigW / 2) (bandY + bandH / 2) bigW bandH)
        , Color (gray (50 / 255)) (Line [(x0, gy1), (x1, gy1)])
        , Color (gray (50 / 255)) (Line [((x0 + x1) / 2, gy0), ((x0 + x1) / 2, gy1)])
        , Color (rgba255 255 255 255 70) (thickPath 1.5 envCurve)
        , Color c (thickPath 2.5 iCurve)
        , Color (rgba255 255 80 80 120) (Pictures darkTicks)
        , Color (rgba255 120 200 255 100) (Pictures brightTicks)
        , Color (gray (200 / 255)) (Pictures wall)
        ]))
    , readout
    -- 軸ラベル
    , txt env x0 (gy1 + 20) 14 (gray 0.63) ("sin = -" ++ nf 3 smax)
    , txt env ((x0 + x1) / 2 - 3) (gy1 + 20) 14 (gray 0.63) "0"
    , txt env (x1 - 60) (gy1 + 20) 14 (gray 0.63) ("+" ++ nf 3 smax)
    , txt env x0 (gy1 + 42) 14 (gray 0.63) "Red ticks: dark lines of one slit, sin = m lam/a.   Blue ticks: bright lines, sin = m lam/d."
    , txt env (left - 40) (sy + 24) 14 (gray 0.63) "Slits (top view)"
    -- HUD
    , txt env 20 40 18 white
        ("a = " ++ nf 0 a ++ " um (L/R)  N = " ++ show n ++ " (Up/Down)  d = " ++ nf 0 d
         ++ " um (Z X)  lam = " ++ nf 0 (wLambda w) ++ " nm (Q A)")
    , if n == 1
        then txt env 20 68 15 (gray 0.78) "Single slit: center band width ~ lam/a. Narrower slit, wider spread."
        else Pictures
          [ txt env 20 66 15 (gray 0.78) "N slits: sharp peaks at positions set by d, heights set by the"
          , txt env 20 86 15 (gray 0.78) "single-slit envelope (white). More slits, sharper peaks." ]
    , txt env 20 (h - 24) 14 (gray 0.78)
        ("Ratio to the center brightness. "
         ++ (if n > 1 then "d/a = " ++ nf 1 (d / a) ++ ": interference peak " ++ show (round (d / a) :: Int)
                           ++ " falls on a dark line and vanishes (missing order)"
                      else ""))
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure (fixD (case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = min 200 (wA w + 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wA = max 2 (wA w - 2) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wN = min 12 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wN = max 1 (wN w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> w { wD = min 400 (wD w + 5) }
    | c `elem` "zZ" -> w { wD = max (wA w + 2) (wD w - 5) }
    | c `elem` "qQ" -> w { wLambda = min 700 (wLambda w + 10) }
    | c `elem` "aA" -> w { wLambda = max 400 (wLambda w - 10) }
  _ -> w))
  where fixD v = if wD v < wA v + 2 then v { wD = wA v + 2 } else v

-- ---------- やってみよう ----------
-- 1. N = 1 で a を小さくしていく。中央の帯が広がり、a < λ でほぼ全方向に広がる
-- 2. N = 2 → 06 の縞と同じ。N = 10 にすると山が針のように細くなる（回折格子・CD の虹）
-- 3. λ を 400 → 700 に変えると山の位置が動く。格子で色が分かれる理由
-- 4. d = 3a にすると 3 番目の干渉の山が消える（欠落次数）。d/a を整数にして確かめよう


-- ==================================================
-- ここから下は「道具」（座標の変換・数字）
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

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- ==================================================
-- ここから下は「色の道具」（波長 nm → RGB の近似、値は 0〜255）
-- ==================================================
wavelengthToRGB :: Float -> (Float, Float, Float)
wavelengthToRGB nm0 =
  let nm = max 380 (min 780 nm0)
      (r, g, b)
        | nm < 440 = (negate (nm - 440) / 60, 0, 1)
        | nm < 490 = (0, (nm - 440) / 50, 1)
        | nm < 510 = (0, 1, negate (nm - 510) / 20)
        | nm < 580 = ((nm - 510) / 70, 1, 0)
        | nm < 645 = (1, negate (nm - 645) / 65, 0)
        | otherwise = (1, 0, 0)
      f | nm < 420 = 0.3 + 0.7 * (nm - 380) / 40
        | nm > 700 = 0.3 + 0.7 * (780 - nm) / 80
        | otherwise = 1
      q v = 255 * (v * f) ** 0.8
  in (q r, q g, q b)

-- 0〜255 の値から色を作る
col255 :: (Float, Float, Float) -> Color
col255 (r, g, b) = makeColor (r / 255) (g / 255) (b / 255) 1
