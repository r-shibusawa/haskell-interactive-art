-- ============================================
-- 45-01 スライダクランク機構（エンジンのピストン）
-- クランクの回転 ⇄ ピストンの往復。位置 x(θ) = r cos θ + √(L² − r² sin² θ)
-- 連接棒比 λ = r / L が大きいほど、動きは単振動からずれる（上死点側で加速度が大きい）
-- 操作：←→ = 連接棒の長さ L / ↑↓ = 回転数 / クランクピンをドラッグ = 手で回す / A = 近似式を重ねる / スペース = 止める
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

crankR :: Float
crankR = 60               -- ★ クランク半径 r（mm として扱う。ストローク = 2r）

data World = World
  { wRodL     :: !Float   -- ★ 連接棒の長さ L（mm）
  , wRpm      :: !Float   -- ★ 回転数（1 分あたりの回転）
  , wTh       :: !Float   -- クランク角 θ（ラジアン）
  , wPlaying  :: !Bool
  , wApprox   :: !Bool
  , wGrabbing :: !Bool
  }

main :: IO ()
main = runSketch (sketch "45-01 Slider-crank" (World 200 1200 0.6 True False False))
  { skBack  = hsb 212 45 13
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

-- クランク軸の位置（p5.js の座標）
center :: Env -> Point
center env = (envW env * 0.2, envH env * 0.3)

-- ★ ピストンピンの位置（クランク軸からの距離）
--   微分を数値で取るので、中の計算は桁の多い Double で行う
pistonPos :: Float -> Float -> Float
pistonPos rodL t = crankR * cos t + sqrt (rodL * rodL - crankR * crankR * sin t * sin t)

-- dx/dθ（解析的）
pistonD1 :: Float -> Float -> Float
pistonD1 rodL t = realToFrac (d1 (realToFrac rodL) (realToFrac t))

d1 :: Double -> Double -> Double
d1 rodL t =
  let r = realToFrac crankR
      q = sqrt (rodL * rodL - r * r * sin t * sin t)
  in -r * sin t - (r * r * sin t * cos t) / q

-- d²x/dθ²（数値微分）
pistonD2 :: Float -> Float -> Float
pistonD2 rodL t =
  let l = realToFrac rodL
      t' = realToFrac t
  in realToFrac ((d1 l (t' + 1e-4) - d1 l (t' - 1e-4)) / 2e-4)

step :: World -> World
step w = w { wTh = wrapF tau th }
  where th = if wPlaying w && not (wGrabbing w) then wTh w + 0.015 + wRpm w / 60000 else wTh w   -- 画面上はスローモーション

draw :: Env -> World -> Picture
draw env w = view env $
  -- シリンダー
  [ Color (hsb 0 0 30) (Pictures [ rectTL (cx + rodL - crankR - 30) (cy - 48) (headX - (cx + rodL - crankR - 30)) 8
                                 , rectTL (cx + rodL - crankR - 30) (cy + 40) (headX - (cx + rodL - crankR - 30)) 8
                                 , rectTL headX (cy - 48) 8 96 ]) ]
  -- 上死点・下死点の線
  ++ [ seg 1 (hsbA 0 0 50 60) (x, cy - 70) (x, cy + 70) | x <- [cx + rodL + crankR, cx + rodL - crankR] ]
  ++ [ txt (cx + rodL + crankR - 18) (cy - 78) 14 (hsb 0 0 60) "TDC", txt (cx + rodL - crankR - 18) (cy - 78) 14 (hsb 0 0 60) "BDC"
     -- クランクの円
     , Color (hsb 0 0 40) (Translate cx cy (Circle crankR))
     -- クランク、連接棒、ピストン
     , seg 12 (hsb 8 75 100) (cx, cy) (pinX, pinY)
     , seg 9 (hsb 200 60 95) (pinX, pinY) (cx + px, cy)
     , Color (hsb 40 20 85) (rectTL (cx + px - 18) (cy - 40) 62 80)
     , dot (hsb 0 0 100) cx cy 18, dot (hsb 0 0 100) pinX pinY 16, dot (hsb 0 0 100) (cx + px) cy 14
     , txt (cx - 20) (cy + crankR + 26) 14 (hsb 0 0 100) ("r = " ++ fmt 0 crankR)
     , txt ((pinX + cx + px) / 2 - 30) (cy + 70) 14 (hsb 0 0 100) ("L = " ++ fmt 0 rodL ++ " (Left/Right)")
     -- 値
     , txt 20 40 18 (hsb 0 0 100) ("Left/Right: rod L   Up/Down: " ++ fmt 0 (wRpm w) ++ " rpm   Drag pin: turn by hand   A: approx "
                                   ++ onOff (wApprox w) ++ "   Space: " ++ (if wPlaying w then "stop" else "run"))
     , txt 20 68 15 (hsb 0 0 60) ("Rod ratio lambda = r / L = " ++ fmt 3 lam ++ ". Larger lambda (shorter rod): motion near TDC and BDC becomes asymmetric")
     , Color (hsbA 0 0 100 8) (rectTL (ix - 16) (iy - 30) 390 150)
     , txt ix iy 16 (hsb 0 0 100) "x = r cos t + sqrt(L^2 - r^2 sin^2 t)"
     , txt ix (iy + 30) 16 (hsb 35 90 100) ("t = " ++ fmt 0 (th * 180 / pi) ++ " deg   x = " ++ fmt 1 px ++ " mm")
     , txt ix (iy + 56) 16 (hsb 190 70 100) ("velocity v = " ++ fmt 2 v ++ " m/s")
     , txt ix (iy + 82) 16 (hsb 320 60 100) ("accel a = " ++ fmt 0 acc ++ " m/s^2 (" ++ fmt 0 (acc / 9.8) ++ " G)")
     , txt ix (iy + 106) 14 (hsb 0 0 60) "approx: a = -r w^2 (cos t + lambda cos 2t)" ]
  -- グラフ 3 段（位置・速度・加速度）
  ++ concat (zipWith graphRow [0 ..] rows)
  ++ [ seg 1 (hsbA 0 0 100 40) (gx + th / tau * gw, gy) (gx + th / tau * gw, gy + gh * 3) ]
  ++ [ txt (gx + fromIntegral d / 360 * gw - 8) (gy + gh * 3 + 18) 14 (hsb 0 0 60) (show d ++ " deg") | d <- [0, 90 .. 360 :: Int] ]
  where
    rodL = wRodL w
    th = wTh w
    (cx, cy) = center env
    omega = wRpm w * tau / 60                      -- 角速度（rad/s）
    px = pistonPos rodL th
    pinX = cx + crankR * cos th
    pinY = cy - crankR * sin th
    lam = crankR / rodL
    headX = cx + rodL + crankR + 70
    v = pistonD1 rodL th * omega / 1000             -- m/s
    acc = pistonD2 rodL th * omega * omega / 1000   -- m/s²
    ix = envW env - 400
    iy = 120
    gx = 70
    gw = envW env - 140
    gy = envH env * 0.5
    gh = (envH env - gy - 40) / 3
    rows = [ ("x", 35, \t -> pistonPos rodL t - rodL, crankR * 1.05, Nothing)
           , ("v", 190, pistonD1 rodL, crankR * (1 + lam) * 1.05, Nothing)
           , ("a", 320, pistonD2 rodL, crankR * (1 + lam) * 1.1, Just (\t -> -crankR * (cos t + lam * cos (2 * t)))) ]
    graphRow :: Int -> (String, Float, Float -> Float, Float, Maybe (Float -> Float)) -> [Picture]
    graphRow i (name, hue, f, peak, approx) =
      let y0 = gy + gh * fromIntegral i + gh / 2
          amp = gh * 0.42
          curve g = [ (gx + fromIntegral k / 180 * gw, y0 - g (fromIntegral k / 180 * tau) / peak * amp) | k <- [0 .. 180 :: Int] ]
      in [ seg 1 (hsb 0 0 30) (gx, y0) (gx + gw, y0)
         , path 2.5 (hsb hue 70 100) (curve f) ]
         ++ [ path 1.5 (hsbA 0 0 100 70) (curve g) | Just g <- [approx], wApprox w ]
         ++ [ dot (hsb hue 70 100) (gx + th / tau * gw) (y0 - f th / peak * amp) 12
            , txt (gx - 56) (y0 + 5) 14 (hsb hue 50 100) name ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (cx, cy) = center env
    in w { wGrabbing = dist2 (mouseP env) (cx + crankR * cos (wTh w), cy - crankR * sin (wTh w)) < 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrabbing = False }
  EventMotion _ | wGrabbing w ->
    let (cx, cy) = center env
        (mx, my) = mouseP env
    in w { wTh = wrapF tau (atan2 (cy - my) (mx - cx)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRodL = clampF (crankR + 20) 400 (wRodL w - 10) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRodL = clampF (crankR + 20) 400 (wRodL w + 10) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRpm = clampF 300 9000 (wRpm w + 300) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRpm = clampF 300 9000 (wRpm w - 300) }
  EventKey (Char k) Down _ _ | k `elem` "aA" -> w { wApprox = not (wApprox w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. L を 80 まで短くすると、加速度のグラフの下側（下死点）にくぼみが 2 つできる。λ がいくつを超えると現れる？
-- 2. A で近似式 a ≈ −rω²(cos θ + λ cos 2θ) を重ねよう。cos 2θ の項が「2 次の慣性力」＝エンジンの振動のもと
-- 3. 速度が最大になるクランク角は 90° より手前。連接棒とクランクが直角になるあたり、を図で確かめよう
-- 4. 9000 rpm での加速度は何 G？ ピストンの質量を 0.3 kg として、連接棒にかかる力（F = ma）を計算しよう
-- 5. crankR を 2 倍にすると（ストローク 2 倍）、同じ回転数で加速度は何倍？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
