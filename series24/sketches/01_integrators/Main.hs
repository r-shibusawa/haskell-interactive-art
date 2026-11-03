-- ============================================
-- 01 ニュートンの運動方程式と数値積分
-- F = ma → a がわかれば、少しずつ v と x を更新して未来を計算できる（数値積分）
--   オイラー法・半陰的オイラー法・ルンゲ＝クッタ法（RK4）を、正解（式）と比べる
-- ↑↓ = 時間の刻み dt / M = 放物運動 ↔ ばね / D = 空気抵抗 ON/OFF / スペース = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

gravity, kDrag, w0 :: Float
gravity = 9.8   -- ★ 重力加速度 [m/s²]
kDrag = 0.03    -- ★ 空気抵抗の係数（a = −k|v|v）
w0 = pi         -- ★ ばねの角振動数 [rad/s]（周期 2 秒）

histLen :: Int
histLen = 600

-- 状態
data S = S { sx, sy, svx, svy :: !Float }

-- 1 つの積分法
data Method = Method
  { mName  :: String
  , mHue   :: !Float
  , mStep  :: Int                 -- 0: オイラー, 1: 半陰的, 2: RK4
  , mS     :: !S
  , mTrail :: !(Seq.Seq Point)
  , mPh    :: !(Seq.Seq Point)    -- 位相図の記録（ばねのとき）
  }

data World = World
  { wDt      :: !Float            -- ★ 時間の刻み [s]（大きいほど誤差が増える）
  , wScene   :: !Int              -- 0: 放物運動, 1: ばね
  , wDrag    :: !Bool
  , wSimT    :: !Float
  , wAcc     :: !Float
  , wMethods :: ![Method]
  , wHist    :: ![Seq.Seq Float]  -- E / E0 の記録（方法ごと + 正解）
  }

main :: IO ()
main = runSketch (sketch "24-01 Integrators" world0)
  { skBack  = hsb 220 30 11
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = restart World
  { wDt = 0.1, wScene = 0, wDrag = False, wSimT = 0, wAcc = 0
  , wMethods = [ Method "Euler" 0 0 s0 Seq.empty Seq.empty
               , Method "Semi-implicit" 130 1 s0 Seq.empty Seq.empty
               , Method "RK4" 200 2 s0 Seq.empty Seq.empty ]
  , wHist = replicate 4 Seq.empty }
  where s0 = S 0 0 0 0

restart :: World -> World
restart w =
  let s0 = if wScene w == 0 then S 0 0 13 18 else S 1 0 0 0
  in w { wSimT = 0, wAcc = 0
       , wMethods = [m { mS = s0, mTrail = Seq.singleton (sx s0, sy s0) } | m <- wMethods w] }

-- ★ 運動方程式：状態 → 加速度
accel :: World -> S -> (Float, Float)
accel w s
  | wScene w == 1 = (-w0 * w0 * sx s, 0)
  | wDrag w = let sp = sqrt (svx s * svx s + svy s * svy s)
              in (-kDrag * sp * svx s, -gravity - kDrag * sp * svy s)
  | otherwise = (0, -gravity)

-- ---- 3 つの積分法（1 ステップ h 進める）
eulerStep, semiImplicitStep, rk4Step :: World -> S -> Float -> S
eulerStep w s h =
  let (ax, ay) = accel w s
  in S (sx s + svx s * h) (sy s + svy s * h) (svx s + ax * h) (svy s + ay * h)   -- 古い v で x を進める
semiImplicitStep w s h =
  let (ax, ay) = accel w s
      vx = svx s + ax * h
      vy = svy s + ay * h
  in S (sx s + vx * h) (sy s + vy * h) vx vy                                     -- 新しい v で x を進める
rk4Step w s h =
  let f q = let (ax, ay) = accel w q in S (svx q) (svy q) ax ay
      add q d c = S (sx q + sx d * c) (sy q + sy d * c) (svx q + svx d * c) (svy q + svy d * c)
      k1 = f s
      k2 = f (add s k1 (h / 2))
      k3 = f (add s k2 (h / 2))
      k4 = f (add s k3 h)
      mix g = g s + h / 6 * (g k1 + 2 * g k2 + 2 * g k3 + g k4)
  in S (mix sx) (mix sy) (mix svx) (mix svy)

-- 正解（式）：空気抵抗なしの放物線 / ばねの cos
exact :: World -> Float -> S
exact w tt
  | wScene w == 1 = S (cos (w0 * tt)) 0 (-w0 * sin (w0 * tt)) 0
  | otherwise = S (13 * tt) (18 * tt - 0.5 * gravity * tt * tt) 13 (18 - gravity * tt)

energy :: World -> S -> Float
energy w s
  | wScene w == 1 = 0.5 * svx s * svx s + 0.5 * w0 * w0 * sx s * sx s
  | otherwise = 0.5 * (svx s * svx s + svy s * svy s) + gravity * sy s

stepOnce :: World -> World
stepOnce w =
  let h = wDt w
      t = wSimT w + h
      lim = clampF (-1e4) 1e4
      one m =
        let st = case mStep m of
              0 -> eulerStep
              1 -> semiImplicitStep
              _ -> rk4Step
            q = st w (mS m) h
            s' = S (lim (sx q)) (lim (sy q)) (lim (svx q)) (lim (svy q))
        in m { mS = s', mTrail = bounded 400 (mTrail m Seq.|> (sx s', sy s')) }
  in w { wAcc = wAcc w - h, wSimT = t, wMethods = map one (wMethods w) }

stepWorld :: World -> World
stepWorld w0' =
  let -- ★ 実時間で 1/60 秒たつごとに、dt ずつ計算を進める
      loop :: Int -> World -> World
      loop !g w | wAcc w >= wDt w && g < 200 = loop (g + 1) (stepOnce w)
                | otherwise = w
      w1 = loop 0 w0' { wAcc = wAcc w0' + 1 / 60 }
      e0 = energy w1 (exact w1 0)
      vals = [energy w1 (mS m) / e0 | m <- wMethods w1]
             ++ [if wDrag w1 && wScene w1 == 0 then 0 / 0 else 1]
      w2 = w1 { wHist = zipWith pushHist (wHist w1) vals }
      w3 = if wScene w2 == 1
             then w2 { wMethods = [m { mPh = bounded 300 (mPh m Seq.|> (sx (mS m), svx (mS m) / w0)) } | m <- wMethods w2] }
             else w2
  in if wScene w3 == 0 && (all (\m -> sy (mS m) < -2) (wMethods w3) || wSimT w3 > 8)
       then restart w3 else w3

drawAll :: World -> Picture
drawAll w = Pictures
  [ if wScene w == 0 then drawProjectile w else drawSpring w
  , drawPlot gx (height * 0.58) gw (height * 0.32) series 0 2 "E / E0"
  , txt 20 36 18 (gray 1) ((if wScene w == 0 then "Projectile" else "Spring (harmonic)") ++ " (M)   dt = " ++ nf 3 (wDt w)
                           ++ " s (Up/Down)   Drag " ++ (if wDrag w then "ON" else "OFF") ++ " (D)   t = " ++ nf 2 (wSimT w) ++ " s")
  , txt 20 58 14 (hsb 0 0 65) "Euler: x += v dt, v += a dt.  Semi-implicit: v += a dt, then x += v dt.  RK4: average of 4 slopes."
  , txt 20 78 14 (hsb 0 0 65) "Gray dashes = exact answer from the formula. Space: restart"
  ]
  where
    gx = width * 0.5
    gw = width * 0.46
    series = case reverse (wHist w) of
      (hExact : _) -> Series hExact (hsb 0 0 60) (hsb 0 0 70) "Exact 1.0"
                      : [Series d (hsb (mHue m) 70 100) (hsb (mHue m) 70 100) (mName m) | (m, d) <- zip (wMethods w) (wHist w)]
      [] -> []

drawProjectile :: World -> Picture
drawProjectile w = Pictures
  [ view ( Color (hsb 0 0 35) (thickLine 2 (ox, oy) (ox + 50 * sc, oy))
         : [ Color (hsb 0 0 70) (dashPath 1.5 5 6 [toS (exact w tt) | tt <- [0, 0.05 .. 3.8]]) | not (wDrag w) ]
        ++ map trailPic (wMethods w) )
    -- 右上：各方法の誤差
  , view [Color (hsb 0 0 14) (boxAt gx gy (width * 0.46) (height * 0.38))]
  , txt (gx + 16) (gy + 30) 16 (hsb 0 0 90) "Difference from the exact answer"
  , Pictures (zipWith errLine [0 ..] (wMethods w))
  , txt (gx + 16) (gy + 190) 14 (hsb 0 0 60) "With gravity only, RK4 matches exactly (a is constant)."
  , txt (gx + 16) (gy + 214) 14 (hsb 0 0 60) "D adds drag and the methods differ."
  , txt (gx + 16) (gy + 238) 14 (hsb 0 0 60) "Graph below: leaving 1 means error."
  ]
  where
    ox = width * 0.05
    oy = height * 0.8
    sc = min (width * 0.42 / 50) (height * 0.6 / 22)
    toS p = (ox + sx p * sc, oy - sy p * sc)
    toP (x, y) = (ox + x * sc, oy - y * sc)
    gx = width * 0.5
    gy = height * 0.14
    ex = exact w (wSimT w)
    trailPic m =
      let c = hsb (mHue m) 70 100
          ps = map toP (toList (mTrail m))
      in Pictures [ Color (withA 0.8 c) (thickPath 2 ps)
                  , Color c (Pictures [at p (disc 2.5) | p <- ps])      -- 点 = 計算した時刻
                  , Color c (at (toS (mS m)) (disc 7)) ]
    errLine :: Float -> Method -> Picture
    errLine i m =
      let err = dist2 (sx (mS m), sy (mS m)) (sx ex, sy ex)
          msg | wDrag w = mName m ++ ": no formula with drag, compare with RK4"
              | otherwise = mName m ++ ": position error " ++ nf 3 err ++ " m"
      in txt (gx + 16) (gy + 64 + i * 30) 15 (hsb (mHue m) 70 100) msg

drawSpring :: World -> Picture
drawSpring w = Pictures
  ( zipWith cart [0 ..] (wMethods w)
 ++ [ view [Color (hsb 0 0 60) (boxAt (mxE - 20) (yE - 20) 40 40)]
    , txt (width * 0.03 + 8) (yE - 30) 14 (hsb 0 0 80) "Exact x = cos wt"
      -- 右上：位相図（x, v）
    , view ( Color (hsb 0 0 14) (boxAt gx gy gw gh)
           : Color (hsb 0 0 50) (dashPath 1 5 6 [(pcx + ps * cos a, pcy + ps * sin a) | a <- [0, 0.1 .. tau + 0.1]])
           : map phaseTrail (wMethods w)
          ++ [Color (hsb (mHue m) 70 100) (at (ph (sx (mS m), svx (mS m) / w0)) (disc 5)) | m <- wMethods w] )
    , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) "Phase plot (x, v/w). Dashed circle = exact answer."
    , txt (gx + 8) (gy + 38) 14 (hsb 0 0 70) "Spiral outward = the energy grows."
    ] )
  where
    cx = width * 0.24
    sc = width * 0.16
    wall = width * 0.03
    ex = exact w (wSimT w)
    yE = height * 0.76
    mxE = cx + sx ex * sc
    gx = width * 0.5
    gy = height * 0.14
    gw = width * 0.46
    gh = height * 0.38
    pcx = gx + gw / 2
    pcy = gy + gh / 2
    ps = gh * 0.2
    ph (a, b) = (pcx + clampF (-2.4) 2.4 a * ps, pcy - clampF (-2.4) 2.4 b * ps)
    phaseTrail m = Color (hsba (mHue m) 70 100 0.6) (thickPath 1.5 (map ph (toList (mPh m))))
    cart :: Float -> Method -> Picture
    cart i m =
      let y = height * (0.25 + i * 0.17)
          mx = cx + clampF (-2.5) 2.5 (sx (mS m)) * sc
          zig j = let u = fromIntegral j / 16 :: Float
                      off | j == 0 || j == (16 :: Int) = 0
                          | odd j = -10
                          | otherwise = 10
                  in (lerpF wall (mx - 20) u, y + off)
      in Pictures
           [ view [ Color (hsb 0 0 40) (thickLine 4 (wall, y - 30) (wall, y + 30))
                  , Color (hsb (mHue m) 60 100) (thickPath 2 (map zig [0 .. 16]))
                  , Color (hsb (mHue m) 70 100) (boxAt (mx - 20) (y - 20) 40 40) ]
           , txt (wall + 8) (y - 30) 14 (hsb 0 0 80) (mName m) ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wDt = min (wDt w * 1.5) 0.4 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wDt = max (wDt w / 1.5) 0.002 }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart (clearPhase w)
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> restart (clearPhase w) { wScene = 1 - wScene w, wHist = replicate 4 Seq.empty }
    'd' -> restart w { wDrag = not (wDrag w) }
    _ -> w
  _ -> w
  where clearPhase q = q { wMethods = [m { mPh = Seq.empty } | m <- wMethods q] }

-- ---------- やってみよう ----------
-- 1. ばね（M）で dt を大きくする。オイラー法は外へ渦巻き（エネルギーが増え続け）、半陰的オイラーは円のまわりにとどまる
-- 2. dt を半分にすると、オイラー法の誤差は約 1/2、RK4 は約 1/16 になる（1 次と 4 次の精度）
-- 3. 空気抵抗（D）を入れて、kDrag を 0.1 にしてみる。飛ぶ距離はどれだけ短くなる？
-- 4. ゲームの物理エンジンは半陰的オイラー法が多い。速くて、エネルギーが暴走しにくいから


-- ==================================================
-- 道具（グラフを描く）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標（左上が原点、y は下向き）で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- 長さを n 以下に保つ
bounded :: Int -> Seq.Seq a -> Seq.Seq a
bounded n s = if Seq.length s > n then Seq.drop (Seq.length s - n) s else s

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v = bounded histLen (s Seq.|> v)

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

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sLine, sText :: !Color
  , sName :: String
  }

-- 時間プロット（縦の範囲 lo〜hi）。y = 1 に基準線。NaN のところは描かない
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view ( Color (hsb 0 0 14) (boxAt x0 y0 w h)
         : Color (hsb 0 0 30) (Line [(x0, y1), (x0 + w, y1)])
         : map curve series )
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    y1 = remap 1 lo hi (y0 + h) y0
    curve s =
      let d = toList (sData s)
          off = histLen - length d
          pts = [ if isNaN v then Nothing
                  else Just ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
                            , remap (clampF lo hi v) lo hi (y0 + h) y0 )
                | (i, v) <- zip [0 :: Int ..] d, even i ]
      in Color (sLine s) (Pictures (map (thickPath 2) (segments pts)))
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (sText s) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest

segments :: [Maybe Point] -> [[Point]]
segments ps = case dropWhile isNone ps of
  [] -> []
  rest -> let (a, b) = span (not . isNone) rest
          in [p | Just p <- a] : segments b
  where isNone m = case m of { Nothing -> True; _ -> False }
