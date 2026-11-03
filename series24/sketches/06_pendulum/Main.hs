-- ============================================
-- 06 単振り子と非線形性
-- 本物の振り子 θ'' = −(g/L) sin θ と、小さい角度の近似 θ'' = −(g/L) θ を並べて比べる
--   振れ幅が大きいと周期が長くなる。右上は位相図（θ, ω）、太い線がセパラトリクス（揺れると回るの境目）
-- ↑↓ = 振れ幅 / 位相図をクリック = その状態からスタート / D = 空気抵抗 / スペース = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

gravity, lenM, gammaD :: Float
gravity = 9.8   -- ★ 重力加速度 [m/s²]
lenM = 1.0      -- ★ ひもの長さ [m]
gammaD = 0.15   -- ★ 空気抵抗の強さ

data World = World
  { wAmp     :: !Float               -- ★ 最初の振れ幅 [rad]
  , wDamping :: !Bool
  , wTh, wOm, wThL, wOmL :: !Float   -- 本物（非線形）と近似（線形）
  , wTrail   :: !(Seq.Seq Point)
  , wTSim    :: !Float
  , wLastCross, wMeasured :: !(Maybe Float)
  , wPrevOm  :: !Float
  }

main :: IO ()
main = runSketch (sketch "24-06 Pendulum" (reset 1.2 0 world0))
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 1.2 False 0 0 0 0 Seq.empty 0 Nothing Nothing 0

reset :: Float -> Float -> World -> World
reset a v w = w { wTh = a, wOm = v, wThL = a, wOmL = v, wTrail = Seq.empty, wTSim = 0
                , wLastCross = Nothing, wMeasured = Nothing, wPrevOm = v }

-- ★ 運動方程式（非線形と線形）
accelOf :: Bool -> Bool -> Float -> Float -> Float
accelOf damping linear t v =
  let a = if linear then -(gravity / lenM) * t else -(gravity / lenM) * sin t
  in if damping then a - gammaD * v else a

rk4 :: Bool -> Bool -> Float -> Float -> Float -> (Float, Float)
rk4 damping linear t v h =
  let f = accelOf damping linear
      k1t = v
      k1w = f t v
      k2t = v + k1w * h / 2
      k2w = f (t + k1t * h / 2) (v + k1w * h / 2)
      k3t = v + k2w * h / 2
      k3w = f (t + k2t * h / 2) (v + k2w * h / 2)
      k4t = v + k3w * h
      k4w = f (t + k3t * h) (v + k3w * h)
  in (t + h / 6 * (k1t + 2 * k2t + 2 * k3t + k4t), v + h / 6 * (k1w + 2 * k2w + 2 * k3w + k4w))

-- 周期の理論値：T = T₀ / AGM(1, cos(θ₀/2))（算術幾何平均を使うと楕円積分が速く計算できる）
periodRatio :: Float -> Float
periodRatio a
  | y0 <= 1e-6 = 99
  | otherwise = go (20 :: Int) 1 y0
  where
    y0 = cos (a / 2)
    go 0 x _ = 1 / x
    go n x y = go (n - 1) ((x + y) / 2) (sqrt (x * y))

stepWorld :: World -> World
stepWorld w0' =
  let sub = 10 :: Int
      h = 1 / 60 / fromIntegral sub
      d = wDamping w0'
      loop :: Int -> World -> World
      loop 0 w = w
      loop k w =
        let (th, om) = rk4 d False (wTh w) (wOm w) h
            (thL, omL) = rk4 d True (wThL w) (wOmL w) h
            t = wTSim w + h
            -- 周期を測る：ω が + から − に変わった瞬間（右端）の間隔
            crossed = wPrevOm w > 0 && om <= 0
            measured = case (crossed, wLastCross w) of
              (True, Just c) -> Just (t - c)
              _ -> wMeasured w
        in loop (k - 1) w { wTh = th, wOm = om, wThL = thL, wOmL = omL, wTSim = t, wPrevOm = om
                          , wMeasured = measured
                          , wLastCross = if crossed then Just t else wLastCross w }
      w1 = loop sub w0'
      om1 = clampF (-50) 50 (wOm w1)
      trail = wTrail w1 Seq.|> (wTh w1, om1)
  in w1 { wOm = om1, wOmL = clampF (-50) 50 (wOmL w1)
        , wTrail = if Seq.length trail > 400 then Seq.drop 1 trail else trail }

-- ---- 位相図の枠
phX0, phY0, phW, phH, tmax, wmax :: Float
phX0 = width * 0.52
phY0 = height * 0.12
phW = width * 0.44
phH = height * 0.42
tmax = pi * 1.25
wmax = 9

phX, phY :: Float -> Float
phX t = remap t (-tmax) tmax phX0 (phX0 + phW)
phY v = remap v (-wmax) wmax (phY0 + phH) phY0

-- 位相図の背景：エネルギー E = ω²/2 − (g/L) cos θ の等高線を色の帯で（一度だけ作る）
phaseBG :: Picture
phaseBG =
  Translate (phX0 + phW / 2 - width / 2) (height / 2 - phY0 - phH / 2)
    (Scale (phW / 220) (phH / 110) (pixelImageF 220 110 1 pix))
  where
    es = 2 * gravity / lenM
    pix i j =
      let t = remap (fromIntegral i) 0 219 (-tmax) tmax
          v = remap (fromIntegral j) 0 109 wmax (-wmax)
          e = 0.5 * v * v + (gravity / lenM) * (1 - cos t)
          band = (floor (e / 3) :: Int) `mod` 2 == 1
      in rgbOf (hsb (if e < es then 220 else 280) 40 (if band then 22 else 17))
    rgbOf c = let (r, g, b, _) = rgbaOfColor c in (r, g, b)

-- セパラトリクス ω = ±2√(g/L) cos(θ/2)
separatrix :: Picture
separatrix = Color (hsb 55 90 100) (Pictures
  [ thickPath 2.5 [ (phX t, phY (sgn * 2 * sqrt (gravity / lenM) * abs (cos (t / 2))))
                  | t <- [-tmax, -tmax + 0.05 .. tmax] ]
  | sgn <- [1, -1] ])

-- 振れ幅と周期のグラフの枠
pgx, pgy, pgw, pgh :: Float
pgx = phX0
pgy = height * 0.6
pgw = phW
pgh = height * 0.3

pgX, pgY :: Float -> Float
pgX a = remap a 0 pi (pgx + 20) (pgx + pgw - 20)
pgY r = remap r 0.8 4 (pgy + pgh - 20) (pgy + 30)

periodCurve :: Picture
periodCurve = Color (hsb 30 80 100) (thickPath 2 [(pgX a, pgY (min (periodRatio a) 4)) | a <- [0.01, 0.03 .. 3.1]])

-- −π〜π に折りたたむ
wrapTh :: Float -> Float
wrapTh t = wrapF tau (t + pi) - pi

drawAll :: World -> Picture
drawAll w = Pictures
  [ -- 振り子 2 つ
    view [ Color (hsb 0 0 60) (at (pvx, pvy) (disc 5))
         , bob (wThL w) 200 0.55
         , bob (wTh w) 30 1 ]
  , txt (width * 0.04) (height * 0.82) 14 (hsb 30 70 100) "* Real: sin(theta)"
  , txt (width * 0.04) (height * 0.85) 14 (hsb 200 70 100) "* Approx: theta (sin(theta) ~ theta at small angles)"
    -- 位相図
  , phaseBG
  , view ( separatrix
         : Color (hsb 30 80 100) (Pictures (map (thickPath 2) trailSegs))
         : [Color (hsb 30 80 100) (at (phX (wrapTh (wTh w)), phY (clampF (-wmax) wmax (wOm w))) (disc 6))] )
  , txt (phX0 + 8) (phY0 + 20) 14 (hsb 0 0 80) "Phase (theta, omega)  Yellow: separatrix  Click: start"
    -- 振れ幅と周期のグラフ
  , view [ Color (hsb 0 0 14) (boxAt pgx pgy pgw pgh)
         , periodCurve
         , Color (hsb 200 70 100) (thickLine 2 (pgx + 20, pgY 1) (pgx + pgw - 20, pgY 1))
         , Color (hsb 55 90 100) (at (pgX a0, pgY (min (periodRatio a0) 4)) (disc 6)) ]
  , txt (pgx + 8) (pgy + 20) 14 (hsb 0 0 80) ("T/T0 vs amplitude (0-180 deg)   T0 = 2 pi sqrt(L/g) = " ++ nf 3 t0 ++ " s")
  , txt (pgx + 8) (pgy + 38) 14 (hsb 0 0 80) "T grows without limit near 180 deg"
  , txt 20 36 18 (gray 1) ("Amplitude = " ++ nf 0 (wAmp w * 180 / pi) ++ " deg (Up/Down)   Drag " ++ (if wDamping w then "ON" else "OFF") ++ " (D)   Space: reset")
  , txt 20 66 14 (hsb 0 0 65) ("Period: measured " ++ maybe "-" (nf 3) (wMeasured w) ++ " s   theory " ++ nf 3 (t0 * periodRatio a0)
                               ++ " s   approx " ++ nf 3 t0 ++ " s (independent of amplitude: isochronism, Galileo)")
  ]
  where
    pvx = width * 0.25
    pvy = height * 0.38
    lp = min (height * 0.36) (width * 0.18)
    a0 = abs (wAmp w)
    t0 = tau * sqrt (lenM / gravity)
    bob a hue al =
      let p = (pvx + lp * sin a, pvy + lp * cos a)
      in Pictures [ Color (hsba hue 40 90 al) (thickLine 2 (pvx, pvy) p)
                  , Color (hsba hue 70 100 al) (at p (disc 17)) ]
    trailSegs = cutJumps [ (wrapTh t, clampF (-wmax) wmax v) | (t, v) <- toList (wTrail w) ]

-- θ が −π と π の間で飛ぶところで線を切る
cutJumps :: [Point] -> [[Point]]
cutJumps = map (map (\(t, v) -> (phX t, phY v))) . go []
  where
    go cur [] = [reverse cur]
    go [] (p : ps) = go [p] ps
    go cur@(q : _) (p : ps)
      | abs (fst p - fst q) > pi = reverse cur : go [p] ps
      | otherwise = go (p : cur) ps

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | mx > phX0 && mx < phX0 + phW && my > phY0 && my < phY0 + phH ->
        let t = remap mx phX0 (phX0 + phW) (-tmax) tmax
            v = remap my (phY0 + phH) phY0 (-wmax) wmax
        in reset t v w { wAmp = clampF (-pi) pi t }
    where (mx, my) = toP5 pos
  EventKey (SpecialKey KeyUp) Down _ _ ->
    let a = min (wAmp w + rad 10) (rad 179) in reset a 0 w { wAmp = a }
  EventKey (SpecialKey KeyDown) Down _ _ ->
    let a = max (wAmp w - rad 10) (rad 5) in reset a 0 w { wAmp = a }
  EventKey (SpecialKey KeySpace) Down _ _ -> reset (wAmp w) 0 w
  EventKey (Char c) Down _ _ | toLower c == 'd' -> w { wDamping = not (wDamping w) }
  _ -> w
  where rad d = d * pi / 180

-- ---------- やってみよう ----------
-- 1. 振れ幅 10° では 2 つの振り子はほぼ同じ。90° では？ 170° では？ どのくらいでずれ始める？
-- 2. 位相図でセパラトリクスの外側をクリック → 振り子はぐるぐる回り続ける
-- 3. セパラトリクスのすぐ内側をクリック → 真上でしばらく止まりそうになる（周期がとても長い）
-- 4. D で空気抵抗を入れると、位相図の点は渦を巻いて (0, 0) に吸い込まれる


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

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
nf d v = printf "%.*f" d v
