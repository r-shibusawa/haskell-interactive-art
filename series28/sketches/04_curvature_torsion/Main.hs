-- ============================================
-- 04 曲線の曲率と捩率
-- 曲率 κ = どれだけ曲がっているか = いちばんよく接する円（接触円）の半径の逆数 1/ρ
--   3D の曲線では、進む向き T・曲がる向き N・その直角 B の「フレネ枠」がねじれる速さ = 捩率 τ
-- 1 = 平面の曲線（←→ で形）/ 2 = つる巻き線（↑↓ でピッチ）/ 3 = 三葉結び目 / スペース = 止める / O = 接触円 / ドラッグ = 視点（3D）
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Numeric (showFFloat)

-- 3 階微分まで差分でとるので、計算は Double で行う
type D = Double
data D3 = D3 !D !D !D

speed :: D
speed = 0.006              -- ★ 点の進む速さ

data World = World
  { wMode, wShape :: !Int
  , wT :: !D
  , wPlaying, wCircle :: !Bool
  , wPitch :: !D           -- ★ つる巻き線のピッチ b（半径 a = 1）
  , wYaw, wTilt :: !Float  -- 3D の見る向き
  , wLast :: !Point
  }

main :: IO ()
main = runSketch (sketch "28-04 Curvature and Torsion" (World 1 0 0 True True 0.35 0.6 0.5 (0, 0)))
  { skBack  = rgb255 18 18 28
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wT = modTau (wT w + speed) } else w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }
  where modTau x = if x >= 2 * pi then x - 2 * pi else x

-- ★ 平面の曲線 (x(t), y(t))、t は 0〜2π
curve2 :: Int -> D -> D3
curve2 0 t = D3 (1.4 * cos t) (0.8 * sin t) 0
curve2 1 t = D3 ((0.6 + cos t) * cos t) ((0.6 + cos t) * sin t) 0
curve2 2 t = D3 ((t - pi) / 2.2) (0.7 * sin (2 * t)) 0
curve2 _ t = D3 (1.3 * sin (2 * t)) (0.9 * sin (3 * t)) 0

curveName :: Int -> String
curveName 0 = "ellipse"
curveName 1 = "limacon"
curveName 2 = "sine wave"
curveName _ = "Lissajous"

curve3 :: World -> D -> D3
curve3 w s
  | wMode w == 2 = D3 (cos (2 * s)) (sin (2 * s)) (wPitch w * (2 * s - 2 * pi))   -- つる巻き線（2 周、半径 a = 1）
  | otherwise = D3 (0.45 * (sin s + 2 * sin (2 * s))) (0.45 * (cos s - 2 * cos (2 * s))) (-0.45 * sin (3 * s))   -- 三葉結び目

-- ★ 数値微分：r'、r''、r'''
data Derivs = Derivs { dP, d1, d2, d3 :: !D3 }

derivs :: (D -> D3) -> D -> Derivs
derivs f s =
  let h = 0.002
      a = f (s - 2 * h)
      b = f (s - h)
      c = f s
      d = f (s + h)
      e = f (s + 2 * h)
  in Derivs c (vmul (vsub d b) (1 / (2 * h)))
              (vmul (vadd (vsub d (vmul c 2)) b) (1 / (h * h)))
              (vmul (vsub (vadd (vsub e (vmul d 2)) (vmul b 2)) a) (1 / (2 * h * h * h)))

unitLen :: Env -> Float
unitLen env = min (envW env) (envH env) * 0.25

draw :: Env -> World -> Picture
draw env w =
  let hudTop = [ txt env 20 34 16 white ("1: plane curve (L/R: " ++ curveName (wShape w) ++ ")   2: helix (U/D: pitch " ++ nf 2 (wPitch w) ++ ")   3: trefoil knot   Space: stop   O: circle")
               , txt env 20 60 14 (gray 0.67)
                   (if wMode w == 1 then "Yellow = osculating circle. Smaller radius rho = sharper curve: k = 1/rho. Sign: left turn +, right turn -"
                    else "T (red) = direction, N (green) = turning, B (blue) = T x N. How fast B turns = torsion tau") ]
  in Pictures ((if wMode w == 1 then draw2d env w else draw3d env w) : hudTop)

draw2d :: Env -> World -> Picture
draw2d env w =
  let f = curve2 (wShape w)
      cx = envW env / 2
      cy = envH env * 0.45
      u = unitLen env
      sc (D3 x y _) = px env (cx + f2 x * u) (cy - f2 y * u)
      fi = fromIntegral :: Int -> D
      kappa (Derivs _ (D3 ax ay _) (D3 bx by _) _) = (ax * by - ay * bx) / (ax * ax + ay * ay) ** 1.5
      samples = [ (s, derivs f s) | i <- [0 .. 300 :: Int], let s = fi i / 300 * 2 * pi ]
      ks = map (kappa . snd) samples
      -- 曲率で色をつけた曲線
      segs = [ Color (makeColor (if k >= 0 then 1 else 120 / 255) ((160 - min (abs (f2 k)) 4 * 20) / 255) (if k >= 0 then 120 / 255 else 1) 1)
                 (thickLine 3 (sc (f (s - 2 * pi / 300))) (sc (dP dd)))
             | ((s, dd), k) <- drop 1 (zip samples ks) ]
      dd = derivs f (wT w)
      D3 tx ty _ = d1 dd
      sp = sqrt (tx * tx + ty * ty)
      k0 = kappa dd
      (t1, t2) = (tx / sp, ty / sp)
      (n1, n2) = (-t2, t1)          -- N は左向き（符号つき曲率と合わせる）
      D3 p1 p2 _ = dP dd
      (pxx, pyy) = (cx + f2 p1 * u, cy - f2 p2 * u)
      pS = px env pxx pyy
      gold = rgb255 255 230 90
      circleP
        | wCircle w && abs k0 > 0.02 =
            let rho = 1 / k0
                c = D3 (p1 + n1 * rho) (p2 + n2 * rho) 0
                cS = sc c
                rr = abs (f2 rho) * u
                (ccx, ccy) = cS
                pts = [ (ccx + rr * cos a, ccy + rr * sin a) | i <- [0 .. 120 :: Int], let a = fromIntegral i / 120 * tau ]
            in [ Color (withA (200 / 255) gold) (thickPath 2 pts)
               , Color (withA (120 / 255) gold) (Line [pS, cS])
               , at cS (Color gold (disc 3)) ]
        | otherwise = []
      fr1 = px env (pxx + f2 t1 * 70) (pyy - f2 t2 * 70)
      fr2 = px env (pxx + f2 n1 * 50) (pyy - f2 n2 * 50)
      rhoTxt = if abs k0 > 0.01 then nf 3 (1 / abs k0) else "inf (inflection)"
  in Pictures $
     segs ++ circleP ++
     [ Color (rgb255 255 90 90) (thickLine 3 pS fr1)
     , Color (rgb255 90 230 120) (thickLine 3 pS fr2)
     , at pS (Color white (disc 6))
     , txt env 20 (envH env - 175) 16 gold ("k = (x'y'' - y'x'') / (x'^2 + y'^2)^(3/2) = " ++ nf 3 k0 ++ "    circle radius rho = 1/|k| = " ++ rhoTxt)
     , plot env ks [] (wT w) "k(t) (red = left turn, blue = right turn)"
     ]

-- 3D → 画面（正射影）
proj :: Env -> World -> D3 -> Point
proj env w (D3 x y z) =
  let c = cos (wYaw w)
      s = sin (wYaw w)
      x1 = c * f2 x + s * f2 y
      y1 = -s * f2 x + c * f2 y
      up = y1 * sin (wTilt w) + f2 z * cos (wTilt w)
      k = if wMode w == 2 then unitLen env * 0.55 else unitLen env   -- つる巻き線は縦に長いので小さめに
  in px env (envW env / 2 + x1 * k) (envH env * 0.42 - up * k)

draw3d :: Env -> World -> Picture
draw3d env w =
  let f = curve3 w
      fi = fromIntegral :: Int -> D
      samples = [ derivs f (fi i / 400 * 2 * pi) | i <- [0 .. 400 :: Int] ]
      kOf dd = let cr = vcross (d1 dd) (d2 dd) in vlen cr / vlen (d1 dd) ^ (3 :: Int)
      tOf dd = let cr = vcross (d1 dd) (d2 dd) in vdot cr (d3 dd) / max (vdot cr cr) 1e-9
      ks = map kOf samples
      ts = map tOf samples
      curveLine = Color (rgba255 160 180 255 160) (thickPath 2 (map (proj env w . dP) samples))
      dd = derivs f (wT w)
      cr = vcross (d1 dd) (d2 dd)
      t' = vunit (d1 dd)
      b' = vunit cr
      n' = vcross b' t'
      k0 = kOf dd
      tau0 = tOf dd
      p = proj env w (dP dd)
      gold = rgb255 255 230 90
      -- 接触円（T と N が張る平面の中）
      osc | wCircle w && k0 > 0.02 =
              let rho = 1 / k0
                  c = vadd (dP dd) (vmul n' rho)
                  pts = [ proj env w (vadd c (vadd (vmul n' (-rho * cos a)) (vmul t' (rho * sin a))))
                        | i <- [0 .. 60 :: Int], let a = fi i / 60 * 2 * pi ]
              in [Color (withA (180 / 255) gold) (thickPath 1.5 pts)]
          | otherwise = []
      l = 0.35
      frameLines = [ Color c (thickLine 4 p (proj env w (vadd (dP dd) (vmul v l))))
                   | (v, c) <- [(t', rgb255 255 90 90), (n', rgb255 90 230 120), (b', rgb255 100 150 255)] ]
      pitch = wPitch w
      theory = if wMode w == 2
        then [ txt env 20 (envH env - 155) 15 gold ("Theory: k = a/(a^2+b^2) = " ++ nf 3 (1 / (1 + pitch * pitch)) ++ ",  tau = b/(a^2+b^2) = " ++ nf 3 (pitch / (1 + pitch * pitch))) ]
        else []
  in Pictures $
     [curveLine] ++ osc ++ frameLines ++
     [ at p (Color white (disc 6))
     , txt env 20 (envH env - 178) 16 gold ("k = |r' x r''| / |r'|^3 = " ++ nf 3 k0 ++ "    tau = (r' x r'').r''' / |r' x r''|^2 = " ++ nf 3 tau0)
     ] ++ theory ++
     [ plot env ks ts (wT w) "k (yellow) and tau (light blue)" ]

-- 下のグラフ
plot :: Env -> [D] -> [D] -> D -> String -> Picture
plot env a b tNow title =
  let x0 = 40
      x1 = envW env - 40
      y0 = envH env - 140
      h = 110
      big = foldl' max 0 (map abs (a ++ b))
      m = max 1 (min 8 (big * 1.1))
      one c arr =
        let n = length arr
            fi = fromIntegral :: Int -> Float
            pts = [ px env (x0 + (x1 - x0) * fi i / fi (max 1 (n - 1))) (y0 + h / 2 - f2 (max (-m) (min m v) / m) * (h / 2 - 4))
                  | (i, v) <- zip [0 ..] arr ]
        in Color c (thickPath 2 pts)
      xn = x0 + (x1 - x0) * f2 tNow / tau
  in Pictures
     [ box env x0 y0 (x1 - x0) h (withA (12 / 255) white)
     , Color (withA (60 / 255) white) (Line [px env x0 (y0 + h / 2), px env x1 (y0 + h / 2)])
     , one (rgb255 255 230 90) a
     , if null b then Blank else one (rgb255 120 220 255) b
     , Color white (Line [px env xn y0, px env xn (y0 + h)])
     , txt env (x0 + 8) (y0 + 18) 14 (gray 0.78) (title ++ "   axis +-" ++ nf 1 m)
     ]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> w { wLast = p }
  EventMotion p@(x, y) ->
    let (lx, ly) = wLast w
        w1 = w { wLast = p }
    in if envDown env && wMode w > 1
         then w1 { wYaw = wYaw w - (x - lx) * 0.01, wTilt = clampF (-1.5) 1.5 (wTilt w - (y - ly) * 0.01) }
         else w1
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> w { wMode = fromEnum c - fromEnum '0' }
    | toLower c == 'o' -> w { wCircle = not (wCircle w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wShape = (wShape w + 3) `mod` 4 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wShape = (wShape w + 1) `mod` 4 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wPitch = min 1.5 (wPitch w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wPitch = max 0 (wPitch w - 0.05) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 楕円の κ がいちばん大きいのはどこ？ 接触円の半径と楕円の長さ 1.4・0.8 の関係（ρ = b²/a）を確かめよう
-- 2. 正弦波で κ = 0 になる点（変曲点）では接触円が直線（半径 ∞）になる
-- 3. つる巻き線のピッチを 0 にすると τ = 0（平面の円）。ピッチを大きくすると κ と τ はどう変わる？
-- 4. curve3 に自分の 3D 曲線（例：(cos t, sin t, sin 3t)）を入れて、κ と τ のグラフを見よう

---------------------------------------------------------------- 道具
-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
px :: Env -> Float -> Float -> Point
px env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（p5 の text と同じく左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (a, b) = px env x y in label a b s c str

-- 左上が (x, y) の塗りつぶし長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (a, b) = px env (x + w / 2) (y + h / 2) in Color c (rect a b w h)

-- p5 の nf：小数点以下 d 桁の文字列
nf :: RealFloat a => Int -> a -> String
nf d x | isNaN x = "--"
       | isInfinite x = if x > 0 then "inf" else "-inf"
       | otherwise = showFFloat (Just d) x ""

degF :: Float -> Float
degF x = x * 180 / pi

-- JavaScript の % と同じ（符号は x に合わせる）
jsMod :: Float -> Float -> Float
jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)

f2 :: D -> Float
f2 = realToFrac

vadd, vsub, vcross :: D3 -> D3 -> D3
vadd (D3 a b c) (D3 d e f) = D3 (a + d) (b + e) (c + f)
vsub (D3 a b c) (D3 d e f) = D3 (a - d) (b - e) (c - f)
vcross (D3 a b c) (D3 d e f) = D3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

vmul :: D3 -> D -> D3
vmul (D3 a b c) s = D3 (a * s) (b * s) (c * s)

vdot :: D3 -> D3 -> D
vdot (D3 a b c) (D3 d e f) = a * d + b * e + c * f

vlen :: D3 -> D
vlen a = sqrt (vdot a a)

vunit :: D3 -> D3
vunit a = let l = vlen a in if l == 0 then a else vmul a (1 / l)
