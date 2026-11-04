-- ============================================
-- 15 ラグランジュ形式と最小作用
-- 時刻 0 に高さ y₀、時刻 T に高さ y₁ ── 両端を決めたとき、本物の運動はどの道を通る？
--   作用 S = ∫ (T − V) dt（運動エネルギー − 位置エネルギー）を計算すると、本物の道で S が「停留」する（ずらしても 1 次では変わらない）
-- ドラッグ = 道の制御点を動かす / 1 = 重力（投げ上げ）/ 2 = ばね / ←→ = 時間 T / R = 本物の道に戻す / スペース = 玉を動かす
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Text.Printf (printf)

-- 作用の小さな差（dS/dε、d²S/dε²）を調べるので、この計算だけは倍精度 Double を使う
type R = Double

gravity, mass, w0 :: R
gravity = 9.8   -- ★ 重力加速度
mass = 1        -- ★ 質量
w0 = 2.0        -- ★ ばねの角振動数 ω [rad/s]

ns :: Int
ns = 400        -- ★ 作用の積分の分割数

data World = World
  { wMode     :: !Int        -- 1: 重力 V = mgy、2: ばね V = ½ky²
  , wT        :: !R          -- ★ かかる時間 [s]
  , wY0, wY1  :: !R          -- 両端の高さ
  , wCt, wCdy :: !R          -- 制御点：時刻 t で本物の道から dy ずらす
  , wDragging :: !Bool
  , wAnim     :: !Float
  }

main :: IO ()
main = runSketch (sketch "24-15 Least Action" (setMode 1 (World 1 2 0 0 0.8 1 False 0)))
  { skBack  = hsb 280 25 10
  , skStep  = \_ _ w -> pure w { wAnim = wrapF 1 (wAnim w + 1 / 60 / f (wT w) * 0.5) }
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

setMode :: Int -> World -> World
setMode n w =
  let (a, b, t) = if n == 1 then (0, 0, 2.0) else (1.0, 0.5, 1.2)
  in w { wMode = n, wY0 = a, wY1 = b, wT = t, wCt = t * 0.4, wCdy = 1.0 }

-- ★ 本物の道（オイラー＝ラグランジュ方程式 m y'' = −dV/dy の解で、両端を通るもの）
truePath :: World -> R -> R
truePath w t
  | wMode w == 1 = wY0 w + (wY1 w - wY0 w) * t / bigT + 0.5 * gravity * t * (bigT - t)
  | abs s < 1e-6 = wY0 w * cos (w0 * t)             -- 半周期ちょうどは解が決まらない
  | otherwise = wY0 w * cos (w0 * t) + (wY1 w - wY0 w * cos (w0 * bigT)) / s * sin (w0 * t)
  where
    bigT = wT w
    s = sin (w0 * bigT)

-- ずらし方の形：制御点で 1、両端で 0、なめらか
bump :: World -> R -> R
bump w t =
  let tc = wCt w
      u | t <= tc = sin (pi / 2 * t / tc)
        | otherwise = sin (pi / 2 * (wT w - t) / (wT w - tc))
  in u * u

pathY :: World -> R -> R -> R
pathY w t eps = truePath w t + eps * wCdy w * bump w t

potential :: World -> R -> R
potential w y
  | wMode w == 1 = mass * gravity * y
  | otherwise = 0.5 * mass * w0 * w0 * y * y

-- ★ 作用 S = Σ (½ m v² − V(y)) Δt
action :: World -> R -> R
action w eps = go 0 (pathY w 0 eps) 0
  where
    h = wT w / fromIntegral ns
    go :: Int -> R -> R -> R
    go !i !ya !s
      | i >= ns = s
      | otherwise =
          let yb = pathY w (fromIntegral (i + 1) * h) eps
              v = (yb - ya) / h
          in go (i + 1) yb (s + (0.5 * mass * v * v - potential w ((ya + yb) / 2)) * h)

drawAll :: World -> Picture
drawAll w = Pictures
  [ -- 左：時刻 t と高さ y のグラフ上の道
    view ( Color (hsb 0 0 13) (boxAt px py pw ph)
         : Color (hsb 0 0 30) (Line [(px, yS 0), (px + pw, yS 0)])
           -- いろいろなずらし方（薄い線）
         : [ Color (hsba 0 0 50 0.3) (Line [(xS t, yS (pathY w t e)) | i <- [0 .. 80 :: Int], let t = bigT * fromIntegral i / 80])
           | k <- [-4 .. 4 :: Int], let e = fromIntegral k * 0.25 ]
        ++ [ -- 本物の道（黄色）とずらした道（ピンク）
             Color (hsb 55 90 100) (thickPath 3 [(xS t, yS (truePath w t)) | t <- ts120])
           , Color (hsb 330 70 100) (thickPath 3 [(xS t, yS (pathY w t 1)) | t <- ts120])
           , Color (gray 1) (at (xS 0, yS (wY0 w)) (disc 6))
           , Color (gray 1) (at (xS bigT, yS (wY1 w)) (disc 6))
           , Color (hsb 330 70 100) (at (xS (wCt w), yS (pathY w (wCt w) 1)) (disc 9))
             -- 玉の動き（2 つの道を同時に動かす）
           , Color (hsb 55 90 100) (at (xS ta, yS (truePath w ta)) (disc 5))
           , Color (hsb 330 70 100) (at (xS ta, yS (pathY w ta 1)) (disc 5)) ] )
  , txt (px + 8) (py + 20) 14 (hsb 0 0 70) ("Across: time t (0-" ++ nf 2 (f bigT) ++ " s). Up: height y.")
  , txt (px + 8) (py + 38) 14 (hsb 0 0 70) "White dots: fixed ends. Drag the pink dot."
    -- 右：S(ε) のグラフ（ずらす量を連続に変えたときの作用）
  , view [ Color (hsb 0 0 13) (boxAt gx gy gw gh)
         , Color (hsb 0 0 35) (Line [(sX 0, gy + 26), (sX 0, gy + gh - 26)])
         , Color (hsb 190 70 100) (thickPath 2.5 [(sX e, sY s) | (e, s) <- ss])
         , Color (hsb 55 90 100) (at (sX 0, sY s0) (disc 7))
         , Color (hsb 330 70 100) (at (sX 1, sY s1) (disc 7)) ]
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) "Action S(e): pink shift times e (e=0 true, e=1 pink)"
  , txt (gx + gw / 2 - 5) (gy + gh - 8) 14 (hsb 0 0 70) "e"
    -- 数字
  , txt gx ty 16 (hsb 0 0 90) ("True path    S = " ++ nf 4 (f s0))
  , txt gx (ty + 28) 16 (hsb 330 70 100) ("Pink path    S = " ++ nf 4 (f s1) ++ "  (diff " ++ nf 4 (f (s1 - s0)) ++ ")")
  , txt gx (ty + 60) 15 (hsb 0 0 75) ("Slope at e = 0: dS/de = " ++ nf 5 (f slope) ++ " ~ 0 (stationary)")
  , txt gx (ty + 86) 15 (hsb 0 0 75) ("Curvature d2S/de2 = " ++ nf 3 (f curv) ++ (if abs curv < 1e-6 then " = 0: not shifted (pink = true path)" else if curv > 0 then " > 0: minimum" else " < 0: maximum!"))
  , if curv > -1e-6 then Blank else txt gx (ty + 108) 14 (hsb 0 0 75) "So nature picks 'stationary', not 'minimum'."
  , if wMode w == 2
      then Pictures
        [ txt gx (ty + 130) 14 (hsb 0 0 75) ("Spring: T = " ++ nf 2 (f bigT) ++ " s, half period pi/w = " ++ nf 2 (pi / f w0) ++ " s")
        , if bigT > pi / w0 then txt gx (ty + 150) 14 (hsb 0 0 75) "Longer than half a period: may stop being a minimum" else Blank ]
      else Blank
  , txt 20 36 18 (gray 1) ((if wMode w == 1 then "1: gravity (throw up) V = mgy" else "2: spring V = m w^2 y^2 / 2") ++ "   T = " ++ nf 2 (f bigT) ++ " s (Left/Right)   Drag: shift   R: back to true path")
  , txt 20 66 14 (hsb 0 0 65) "L = T - V (Lagrangian). The condition that S is stationary is m y'' = -dV/dy, Newton's law = Euler-Lagrange equation."
  ]
  where
    bigT = wT w
    px = width * 0.05
    py = height * 0.14
    pw = width * 0.44
    ph = height * 0.62
    (ylo, yhi) = yRange w
    xS t = remap (f t) 0 (f bigT) (px + 30) (px + pw - 30)
    yS y = remap (clampF (ylo - 0.5) (yhi + 0.5) (f y)) ylo yhi (py + ph - 20) (py + 20)
    ts120 = [bigT * fromIntegral i / 120 | i <- [0 .. 120 :: Int]]
    ta = realToFrac (wAnim w) * bigT
    gx = width * 0.53
    gy = height * 0.14
    gw = width * 0.43
    gh = height * 0.5
    ss = [(e, action w e) | i <- [0 .. 60 :: Int], let e = -1.5 + 3 * fromIntegral i / 60]
    smin = foldl' min 1e9 (map snd ss)
    smax = foldl' max (-1e9) (map snd ss)
    pad = (smax - smin) * 0.15 + 1e-3
    sX :: R -> Float
    sX e = remap (f e) (-1.5) 1.5 (gx + 30) (gx + gw - 20)
    sY :: R -> Float
    sY s = gy + gh - 30 - f ((s - (smin - pad)) / (2 * pad + smax - smin)) * (gh - 60)
    s0 = action w 0
    s1 = action w 1
    slope = (action w 0.001 - action w (-0.001)) / 0.002
    curv = (action w 0.01 - 2 * s0 + action w (-0.01)) / 0.0001
    ty = gy + gh + 30

yRange :: World -> (Float, Float)
yRange w = if wMode w == 1 then (-3, 8) else (-2, 2.5)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | mx > px && mx < px + pw && my > height * 0.14 && my < height * 0.76 -> w { wDragging = True }
    where (mx, my) = toP5 pos
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = False }
  EventMotion pos | wDragging w ->
    let (mx, my) = toP5 pos
        py = height * 0.14
        ph = height * 0.62
        (ylo, yhi) = yRange w
        bigT = wT w
        t = max (bigT * 0.05) (min (bigT * 0.95) (d (remap mx (px + 30) (px + pw - 30) 0 (f bigT))))
        yy = d (remap my (py + ph - 20) (py + 20) ylo yhi)
    in w { wCt = t, wCdy = max (-6) (min 6 (yy - truePath w t)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> changeT (\t -> min (t + 0.2) 4) w
  EventKey (SpecialKey KeyLeft) Down _ _ -> changeT (\t -> max (t - 0.2) 0.4) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAnim = 0 }
  EventKey (Char '1') Down _ _ -> setMode 1 w
  EventKey (Char '2') Down _ _ -> setMode 2 w
  EventKey (Char c) Down _ _ | toLower c == 'r' -> w { wCdy = 0 }
  _ -> w
  where
    px = width * 0.05
    pw = width * 0.44

changeT :: (R -> R) -> World -> World
changeT g w =
  let r = wCt w / wT w
      t = g (wT w)
  in w { wT = t, wCt = r * t }

-- ---------- やってみよう ----------
-- 1. ピンクの点をどこへ動かしても、S は本物（黄色）より大きくなる？ 青い曲線の底が ε = 0
-- 2. 2（ばね）で T を半周期 π/ω（約 1.57 s）より長くする。S(ε) の曲線がひっくり返る → 自然は「最小」ではなく「停留」を選ぶ
-- 3. V の符号を変えて T + V にすると？ 停留する道は本物ではなくなる（L = T − V が正しい理由）
-- 4. 光の屈折（フェルマーの原理：時間が停留する道）も同じ考え方。最短時間の道を描いてみよう


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- 倍精度と単精度の変換
f :: R -> Float
f = realToFrac

d :: Float -> R
d = realToFrac

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf k v = printf "%.*f" k v
