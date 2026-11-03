-- ============================================
-- 02 放物運動と空気抵抗
-- 空気抵抗がなければ 45° がいちばん遠くへ飛ぶ（R = v² sin2θ / g）。空気抵抗があると最適な角度は小さくなる
--   発射台からマウスをドラッグして離すと発射（向き = 角度、長さ = 速さ）
-- ドラッグ = 発射 / ↑↓ = 空気抵抗 k / A = 15°〜75° を一斉発射 / C = 軌道を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Text.Printf (printf)

gravity, vMax, worldM :: Float
gravity = 9.8   -- ★ 重力加速度 [m/s²]
vMax = 40       -- ★ 最大の初速 [m/s]
worldM = 170    -- ★ 画面に入れる距離 [m]

-- 発射した軌道
data Shot = Shot
  { shPts  :: ![Point]    -- 空気あり
  , shVac  :: ![Point]    -- 真空
  , shN    :: !Int        -- shPts の点の数
  , shTh   :: !Float
  , shBorn :: !Int        -- 発射したフレーム
  }

data World = World
  { wK      :: !Float     -- ★ 空気抵抗の係数（a = −k|v|v、0 で真空）
  , wShots  :: ![Shot]
  , wCurve  :: ![(Float, Float, Float)]   -- 角度ごとの飛距離 (θ, 真空, 空気あり)
  , wAiming :: !Bool
  , wAimV   :: !Float
  , wAimTh  :: !Float
  }

main :: IO ()
main = runSketch (sketch "24-02 Projectile and Drag" world0)
  { skBack  = hsb 210 35 11
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = makeCurve (World 0.01 [] [] False 25 (pi / 4))

ox, oy, sc :: Float
ox = width * 0.04
oy = height * 0.82
sc = width * 0.44 / worldM

data Q = Q !Float !Float !Float !Float

-- ★ 空気抵抗ありの軌道を RK4 で計算する（地面に落ちるまで）
flight :: Float -> Float -> Float -> Float -> [Point]
flight v0 th kk h = (0, 0) : go (0 :: Int) (Q 0 0 (v0 * cos th) (v0 * sin th))
  where
    nSteps = floor (20 / h) :: Int
    f (Q _ _ vx vy) =
      let sp = sqrt (vx * vx + vy * vy)
      in Q vx vy (-kk * sp * vx) (-gravity - kk * sp * vy)
    add (Q a b c d) (Q e g i j) t = Q (a + e * t) (b + g * t) (c + i * t) (d + j * t)
    go !i s@(Q x y _ _)
      | i >= nSteps = []
      | otherwise =
          let k1 = f s
              k2 = f (add s k1 (h / 2))
              k3 = f (add s k2 (h / 2))
              k4 = f (add s k3 h)
              ns@(Q nx ny _ _) = add (add (add (add s k1 (h / 6)) k2 (h / 3)) k3 (h / 3)) k4 (h / 6)
          in if ny < 0
               then let r = y / (y - ny) in [(x + (nx - x) * r, 0)]   -- 地面との交点
               else if i `mod` 3 == 0 then (nx, ny) : go (i + 1) ns else go (i + 1) ns

rangeOf :: [Point] -> Float
rangeOf = foldl' (\_ p -> fst p) 0

-- 角度 1°〜89° の飛距離を全部計算してグラフにする
makeCurve :: World -> World
makeCurve w =
  let v = wAimV w
      row d = let th = d * pi / 180
                  !r = rangeOf (flight v th (wK w) 0.03)
              in (d, v * v * sin (2 * th) / gravity, r)
  in w { wCurve = map row [1, 3 .. 89] }

launch :: Int -> Float -> Float -> World -> World
launch frame v0 th w =
  let pts = flight v0 th (wK w) 0.01
      s = Shot pts (flight v0 th 0 0.01) (length pts) th frame
      all' = wShots w ++ [s]
  in w { wShots = if length all' > 10 then drop 1 all' else all' }

stepWorld :: Env -> World -> World
stepWorld env w
  | not (wAiming w) = w
  | otherwise =
      let (mx, my) = toP5 (envMouse env)
          dx = mx - ox
          dy = oy - my
          w1 = w { wAimTh = clampF 0.02 (pi / 2 - 0.02) (atan2 dy dx)
                 , wAimV = clampF 3 vMax (sqrt (dx * dx + dy * dy) / (width * 0.3) * vMax) }
      in if envFrame env `mod` 6 == 0 then makeCurve w1 else w1

deg :: Float -> Float
deg r = r * 180 / pi

drawAll :: Env -> World -> Picture
drawAll env w = Pictures
  [ -- 地面と目盛り
    view ( Color (hsb 0 0 35) (thickLine 2 (ox, oy) (ox + worldM * sc, oy))
         : [Color (hsb 0 0 55) (boxAt (ox + m * sc) oy 1 8) | m <- ticks] )
  , Pictures [txtC (ox + m * sc) (oy + 24) 14 (hsb 0 0 55) (nf 0 m ++ " m") | m <- ticks]
    -- これまでの軌道（点線 = 真空、実線 = 空気あり）
  , Pictures (zipWith shotPic [0 :: Int ..] (wShots w))
    -- 狙い中の矢印
  , view [ Color (hsb 55 90 100) (thickLine 3 (ox, oy) (tipX, tipY))
         , Color (hsb 0 0 80) (at (ox, oy) (disc 9)) ]
  , txt (tipX + 8) tipY 14 (hsb 55 90 100) (nf 0 (deg (wAimTh w)) ++ " deg  " ++ nf 1 (wAimV w) ++ " m/s")
  , drawRangePlot w
  , txt 20 36 18 (gray 1) ("Drag from the launcher and release   Drag k = " ++ nf 3 (wK w) ++ " (Up/Down)   A: 15-75 deg   C: clear")
  , txt 20 66 14 (hsb 0 0 65) "Dashed = no air (parabola). Solid = with air (a = -g - k|v|v). With air the ball comes down more steeply."
  ]
  where
    ticks = [0, 20 .. 160]
    len = wAimV w / vMax * width * 0.3
    tipX = ox + len * cos (wAimTh w)
    tipY = oy - len * sin (wAimTh w)
    toS (x, y) = (ox + x * sc, oy - y * sc)
    lastShot = length (wShots w) - 1
    shotPic i s =
      let hue = remap (deg (shTh s)) 0 90 180 360
          n = min (shN s) ((envFrame env - shBorn s) * 2 + 2)   -- 少しずつ伸ばす
          shown = take n (shPts s)
          endP = foldl' (\_ p -> p) (0, 0) shown
          c = hsb hue 70 100
          (ex, ey) = toS endP
          isLast = i == lastShot
      in Pictures
           ( view [ Color (hsba hue 50 80 0.6) (dashPath 1.5 4 6 (map toS (every2 (shVac s))))
                  , Color c (thickPath 2.5 (map toS (every2 shown)))
                  , Color c (at (ex, ey) (disc 5)) ]
           : [txt (ex - 16) (oy - 10) 14 c (nf 1 (rangeOf (shPts s)) ++ " m") | n == shN s && isLast] )

-- 右：角度と飛距離のグラフ（いまの初速で）
drawRangePlot :: World -> Picture
drawRangePlot w = Pictures
  [ view ( Color (hsb 0 0 14) (boxAt gx gy gw gh)
         : [Color (hsb 0 0 30) (Line [(px d, gy + 30), (px d, gy + gh - 30)]) | d <- angles]
        ++ [ Color (hsb 0 0 60) (dashPath 2 4 6 [(px d, py r) | (d, r, _) <- wCurve w])
           , Color (hsb 30 80 100) (thickPath 2 [(px d, py r) | (d, _, r) <- wCurve w])
           , Color (hsb 0 0 80) (at (px bvD, py bvR) (disc 5))
           , Color (hsb 30 80 100) (at (px baD, py baR) (disc 6))
           , Color (hsb 55 90 100) (thickLine 2 (px (deg (wAimTh w)), gy + 30) (px (deg (wAimTh w)), gy + gh - 30)) ] )
  , Pictures [txtC (px d) (gy + gh - 8) 14 (hsb 0 0 60) (nf 0 d) | d <- angles]
  , txt (gx + 50) (gy + 50) 14 (hsb 0 0 80) ("No air: best " ++ nf 0 bvD ++ " deg, " ++ nf 1 bvR ++ " m (v^2/g = " ++ nf 1 (wAimV w * wAimV w / gravity) ++ ")")
  , txt (gx + 50) (gy + 72) 14 (hsb 30 80 100) ("With air: best " ++ nf 0 baD ++ " deg, " ++ nf 1 baR ++ " m")
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) ("Angle vs range (start speed " ++ nf 1 (wAimV w) ++ " m/s)")
  ]
  where
    gx = width * 0.52
    gy = height * 0.16
    gw = width * 0.44
    gh = height * 0.6
    angles = [0, 15 .. 90]
    rMax = max 10 (wAimV w * wAimV w / gravity * 1.1)
    px d = remap d 0 90 (gx + 40) (gx + gw - 16)
    py r = remap r 0 rMax (gy + gh - 30) (gy + 30)
    (baD, baR) = foldl' (\b@(_, br) (d, _, r) -> if r > br then (d, r) else b) (0, 0) (wCurve w)
    (bvD, bvR) = foldl' (\b@(_, br) (d, r, _) -> if r > br then (d, r) else b) (0, 0) (wCurve w)

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | fst (toP5 pos) < width * 0.5 && snd (toP5 pos) > 90 -> w { wAiming = True }
  EventKey (MouseButton LeftButton) Up _ _
    | wAiming w -> let w1 = makeCurve w { wAiming = False }
                   in launch (envFrame env) (wAimV w1) (wAimTh w1) w1
  EventKey (SpecialKey KeyUp) Down _ _ -> makeCurve w { wK = min (wK w + 0.005) 0.1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> makeCurve w { wK = max (wK w - 0.005) 0 }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> foldl' (\q d -> launch (envFrame env) (wAimV q) (d * pi / 180) q) w [15, 30, 45, 60, 75]
    'c' -> w { wShots = [] }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. k = 0 にして 30° と 60° で撃つ。飛距離が同じになる（sin2θ が同じ）
-- 2. k を大きくすると最適な角度は何度になる？ 野球のホームランは 25°〜35° くらい
-- 3. 初速を 2 倍にすると、真空では飛距離 4 倍。空気ありでは？
-- 4. 月面（gravity = 1.62、k = 0）にしてみる。同じ初速でどれだけ飛ぶ？


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

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- 1 つおきに間引く（最後の点は残す）
every2 :: [a] -> [a]
every2 (a : _ : c : rest) = a : every2 (c : rest)
every2 xs = xs

-- 点線：線の長さ on、すきま off
dashPath :: Float -> Float -> Float -> [Point] -> Picture
dashPath w on off = Pictures . go True on
  where
    go draw left (a : b : rest)
      | l <= left = [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
      | otherwise =
          let u = left / l
              m = (lerpF (fst a) (fst b) u, lerpF (snd a) (snd b) u)
          in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
      where l = dist2 a b
    go _ _ _ = []
