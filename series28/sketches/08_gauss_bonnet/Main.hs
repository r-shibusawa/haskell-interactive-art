{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 08 ガウス＝ボネの定理
-- 球面の三角形（辺は大円 = 測地線）の内角の和は 180° より大きい。はみ出した分 = 面積 ÷ R²
--   面全体では「曲率の合計 ∬K dA = 2π × オイラー標数 χ」。形をどう変えても球面は 4π、トーラスは 0
-- 頂点をドラッグ = 三角形を変える / 球の他の所をドラッグ = 回す / 1〜3 = 三角形の例 / ↑↓ = 球の半径 R / M = 面積を点で数える
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

ns :: Int
ns = 30000                 -- ★ 面積を数えるための点の数

data World = World
  { wA, wB, wC :: !V3      -- 三角形の頂点（単位ベクトル）
  , wR :: !Float           -- ★ 球の半径（単位は自由。面積 = 超過 × R²）
  , wYaw, wTilt :: !Float
  , wDrag :: !Int          -- つまんだ頂点（-1 = なし）
  , wStart :: !(Maybe (Float, Float, Float, Float))
  , wMC :: !Bool
  , wSamples :: !(U.Vector (Float, Float, Float))   -- 球面上に一様にばらまいた点
  , wTotals :: [(String, D, Int)]                   -- 右の表：閉じた面の ∬K dA
  }

main :: IO ()
main = runSketch (sketch "28-08 Gauss-Bonnet" (preset 2 (World v0 v0 v0 1 2.36 0.5 (-1) Nothing True U.empty [])))
  { skBack  = rgb255 16 18 28
  , skStart = \_ -> start
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }
  where v0 = V3 0 0 1

start :: IO World
start = do
  -- 球面上に一様にばらまいた点（面積を数えるため）
  pts <- replicateM ns $ do
    z <- randomRIO (-1, 1)
    a <- randomRIO (0, tau)
    let r = sqrt (max 0 (1 - z * z))
    pure (r * cos a, r * sin a, z)
  let totals = [ (nm, integrateK f rng, chi) | (nm, f, rng, chi) <- closedSurfaces ]
      w = preset 2 (World (V3 0 0 1) (V3 0 0 1) (V3 0 0 1) 1 2.36 0.5 (-1) Nothing True (U.fromList pts) totals)
  -- 重い計算は最初に済ませておく
  sum [v | (_, v, _) <- totals] `seq` pure w

-- ★ いろいろな閉じた面（名前、式、v の範囲、χ）
closedSurfaces :: [(String, Surf, (D, D), Int)]
closedSurfaces =
  [ ("Sphere", sphere1, (-pi / 2, pi / 2), 2)
  , ("Ellipsoid", \u v -> D3 (1.2 * cos v * cos u) (0.8 * cos v * sin u) (0.5 * sin v), (-pi / 2, pi / 2), 2)
  , ("Lumpy sphere", \u v -> vmul (sphere1 u v) (1 + 0.25 * sin (3 * u) * cos v ^ (2 :: Int) + 0.15 * cos (2 * v)), (-pi / 2, pi / 2), 2)
  , ("Torus", \u v -> D3 ((1 + 0.4 * cos v) * cos u) ((1 + 0.4 * cos v) * sin u) (0.4 * sin v), (-pi, pi), 0)
  , ("Bent torus", \u v -> D3 ((1 + 0.3 * cos v + 0.15 * cos (3 * u)) * cos u) ((1 + 0.3 * cos v) * sin u) (0.3 * sin v + 0.2 * sin (2 * u)), (-pi, pi), 0)
  ]
  where sphere1 u v = D3 (cos v * cos u) (cos v * sin u) (sin v)

-- ∬K dA を (u, v) の格子で足し合わせる（K√(EG − F²) du dv）
integrateK :: Surf -> (D, D) -> D
integrateK f (v0, v1) =
  let nu = 120 :: Int
      nv = 80 :: Int
      du = 2 * pi / fromIntegral nu
      dv = (v1 - v0) / fromIntegral nv
      h = 0.002
      term acc (i, j) =
        let u = (fromIntegral i + 0.5) * du
            v = v0 + (fromIntegral j + 0.5) * dv
            p = f u v
            ru = vmul (vsub (f (u + h) v) (f (u - h) v)) (1 / (2 * h))
            rv = vmul (vsub (f u (v + h)) (f u (v - h))) (1 / (2 * h))
            ruu = vmul (vadd (vsub (f (u + h) v) (vmul p 2)) (f (u - h) v)) (1 / (h * h))
            rvv = vmul (vadd (vsub (f u (v + h)) (vmul p 2)) (f u (v - h))) (1 / (h * h))
            ruv = vmul (vsub (vsub (f (u + h) (v + h)) (f (u + h) (v - h))) (vsub (f (u - h) (v + h)) (f (u - h) (v - h)))) (1 / (4 * h * h))
            cr = vcross ru rv
            wd = vlen cr
        in if wd < 1e-9 then acc else
           let n = vmul cr (1 / wd)
               l = vdot ruu n
               m = vdot ruv n
               nn = vdot rvv n
           in acc + (l * nn - m * m) / (wd * wd) * wd * du * dv     -- K dA
  in foldl' term 0 [(i, j) | i <- [0 .. nu - 1], j <- [0 .. nv - 1]]

sLat :: Float -> Float -> V3
sLat la lo = V3 (cos la * cos lo) (cos la * sin lo) (sin la)

preset :: Int -> World -> World
preset 1 w = w { wA = sLat 0.5 0.4, wB = sLat 0.62 0.55, wC = sLat 0.45 0.6 }      -- 小さい三角形：ほぼ平ら
preset 2 w = w { wA = sLat (pi / 2 - 0.001) 0, wB = sLat 0 0, wC = sLat 0 (pi / 2) } -- 北極と赤道の 2 点：3 つとも 90°
preset 3 w = w { wA = sLat 0.9 (-0.3), wB = sLat (-0.4) (-0.6), wC = sLat (-0.2) 1.6 } -- 大きい三角形
preset _ w = w

-- 3D → 画面（正射影）。戻り値は gloss の座標と奥行き
data Vw = Vw !Float !Float !Float

radiusOf :: Env -> Float
radiusOf env = min (envW env * 0.28) (envH env * 0.36)

centerOf :: Env -> Point
centerOf env = (envW env * 0.33 - envW env / 2, envH env / 2 - envH env * 0.52)

view :: Env -> World -> V3 -> Vw
view env w (V3 x y z) =
  let c = cos (wYaw w)
      s = sin (wYaw w)
      t = wTilt w
      x1 = c * x + s * y
      y1 = -s * x + c * y
      up = y1 * sin t + z * cos t
      depth = -y1 * cos t + z * sin t
      r = radiusOf env
      (cx, cy) = centerOf env
  in Vw (cx + x1 * r) (cy + up * r) depth

-- 画面 → 球面の手前の点
unview :: Env -> World -> Point -> Maybe V3
unview env w (mx, my) =
  let r = radiusOf env
      (cx, cy) = centerOf env
      x' = (mx - cx) / r
      u' = (my - cy) / r
      dd = 1 - x' * x' - u' * u'
  in if dd < 0 then Nothing else
     let d = sqrt dd
         t = wTilt w
         y1 = u' * sin t - d * cos t
         z1 = u' * cos t + d * sin t
         c = cos (wYaw w)
         s = sin (wYaw w)
     in Just (V3 (c * x' - s * y1) (s * x' + c * y1) z1)

slerp :: V3 -> V3 -> Float -> V3
slerp a b t =
  let w = acos (clampF (-1) 1 (dot3 a b))
  in if w < 1e-6 then a
     else add3 (scale3 (sin ((1 - t) * w) / sin w) a) (scale3 (sin (t * w) / sin w) b)

-- ★ 頂点 A での内角：A から B へ向かう大円の向きと、A から C へ向かう向きのなす角
angleAt :: V3 -> V3 -> V3 -> Float
angleAt a b c =
  let tb = unit3 (sub3 b (scale3 (dot3 a b) a))
      tc = unit3 (sub3 c (scale3 (dot3 a c) a))
  in acos (clampF (-1) 1 (dot3 tb tc))

sphPt :: Float -> Float -> V3
sphPt lo la = V3 (cos la * cos lo) (cos la * sin lo) (sin la)

-- 面積を数える：3 つの行列式 det(a, b, p) の符号がそろう点が三角形の中
-- 戻り値は中の点の数と、描く点（3 個に 1 個、手前だけ）
countInside :: Env -> World -> (Int, [Point])
countInside env w =
  let V3 cx cy cz = wC w
      -- a × b などを先に計算しておく（点ごとの内積だけで判定できる）
      V3 abx aby abz = cross3 (wA w) (wB w)
      V3 bcx bcy bcz = cross3 (wB w) (wC w)
      V3 cax cay caz = cross3 (wC w) (wA w)
      s0 = signum (abx * cx + aby * cy + abz * cz)
      keep = wMC w
      c = cos (wYaw w)
      s = sin (wYaw w)
      t = wTilt w
      st = sin t
      ct = cos t
      r = radiusOf env
      (ox, oy) = centerOf env
      go (!n, acc) i (x, y, z)
        | signum (abx * x + aby * y + abz * z) == s0
          && signum (bcx * x + bcy * y + bcz * z) == s0
          && signum (cax * x + cay * y + caz * z) == s0 =
            let x1 = c * x + s * y
                y1 = -s * x + c * y
                depth = -y1 * ct + z * st
                up = y1 * st + z * ct
                acc' = if keep && i `rem` 3 == 0 && depth > 0 then (ox + x1 * r, oy + up * r) : acc else acc
            in (n + 1, acc')
        | otherwise = (n, acc)
  in U.ifoldl' go (0, []) (wSamples w)

draw :: Env -> World -> Picture
draw env w =
  let r = radiusOf env
      center = centerOf env
      gridCol = rgba255 170 190 255 60
      front a b = let Vw _ _ da = a; Vw _ _ db = b in da > 0 && db > 0
      ptOf (Vw x y _) = (x, y)
      curve f = [view env w (f (fromIntegral i / 80)) | i <- [0 .. 80 :: Int]]
      -- 経線・緯線（手前だけ）
      frontLine pts = [ Line [ptOf a, ptOf b] | (a, b) <- zip pts (drop 1 pts), front a b ]
      grid = concat [ frontLine (curve (\t -> sphPt (t * tau) (la * pi / 180))) | la <- [-60, -30 .. 60] ]
          ++ concat [ frontLine (curve (\t -> sphPt (lo * pi / 180) (t * tau))) | lo <- [0, 30 .. 150] ]
      (inside, dots) = countInside env w
      dotPics = [ Polygon [(x, y), (x + 1.6, y), (x + 1.6, y + 1.6), (x, y + 1.6)] | (x, y) <- dots ]
      -- 三角形の辺（大円の弧）。裏側は薄く
      edge a b = let pts = curve (slerp a b)
                 in [ Color (if front p q then rgb255 255 120 120 else rgba255 255 120 120 60) (thickLine 3 (ptOf p) (ptOf q))
                    | (p, q) <- zip pts (drop 1 pts) ]
      va = wA w
      vb = wB w
      vc = wC w
      angA = angleAt va vb vc
      angB = angleAt vb vc va
      angC = angleAt vc va vb
      vertex i nm v ang =
        let Vw x y d = view env w v
            fr = d > 0
        in Pictures
           [ at (x, y) (Color (if fr then white else gray (120 / 255)) (disc (if wDrag w == i then 10 else 7.5)))
           , label (x + 12) (y + 10) 17 (withA (if fr then 1 else 120 / 255) white) (nm ++ " " ++ nf 1 (degF ang) ++ " deg") ]
      total = angA + angB + angC
      excess = total - pi
      mcArea = 4 * pi * fromIntegral inside / fromIntegral ns :: Float
      x = 20
      y = envH env - 150
      onOff b = if b then "ON" else "off"
  in Pictures $
     [ at center (Color (rgb255 32 44 72) (disc r))
     , Color gridCol (Pictures grid)
     , Color (rgba255 255 210 90 160) (Pictures dotPics) ]
     ++ edge va vb ++ edge vb vc ++ edge vc va ++
     [ vertex 0 "A" va angA, vertex 1 "B" vb angB, vertex 2 "C" vc angC
     , txt env 20 34 16 white ("Drag vertex: triangle   Drag elsewhere: turn   1-3: examples   U/D: radius R = " ++ nf 1 (wR w) ++ "   M: count dots " ++ onOff (wMC w))
     , txt env 20 60 14 (gray 0.67) "Sides are great circles (geodesics). Bigger triangles exceed 180 deg more; tiny ones are nearly 180 (locally flat)."
     , box env (x - 10) (y - 30) 600 150 (withA (160 / 255) black)
     , txt env x y 16 white ("Angle sum = " ++ nf 2 (degF total) ++ " deg -> excess = " ++ nf 2 (degF excess) ++ " deg = " ++ nf 4 excess ++ " rad")
     , txt env x (y + 32) 16 (rgb255 255 210 90) ("Area (yellow dots) = 4 pi R^2 x " ++ show inside ++ " / " ++ show ns ++ " = " ++ nf 4 (mcArea * wR w * wR w))
     , txt env x (y + 62) 16 (rgb255 120 220 255) ("Area / R^2 = " ++ nf 4 mcArea ++ "  ~  excess " ++ nf 4 excess ++ "  (Gauss-Bonnet)")
     , txt env x (y + 90) 14 (gray 0.67) "K = 1/R^2, so int K dA = area/R^2. For a triangle: angle sum - pi = int K dA"
     , drawTable env w
     ]

-- 右の表：閉じた面の曲率の合計
drawTable :: Env -> World -> Picture
drawTable env w =
  let x = envW env * 0.66
      y = 140
      n = fromIntegral (length (wTotals w)) :: Float
      row (i, (nm, v, chi)) =
        let yy = y + 30 + fromIntegral (i :: Int) * 34
        in Pictures
           [ txt env x yy 14 (gray 0.86) nm
           , txt env (x + 108) yy 14 (rgb255 255 210 90) ("K dA = " ++ nf 3 v)
           , txt env (x + 228) yy 14 (rgb255 120 220 255) ("/2pi = " ++ nf 2 (v / (2 * pi)) ++ " (chi " ++ show chi ++ ")") ]
      yy2 = y + 40 + n * 34
  in Pictures $
     [ box env (x - 16) (y - 34) (envW env * 0.34 - 10) (60 + n * 34 + 70) (withA (160 / 255) black)
     , txt env x (y - 6) 16 white "Whole closed surface: int K dA (numeric)"
     ] ++ map row (zip [0 ..] (wTotals w)) ++
     [ txt env x yy2 14 (gray 0.67) "Dent or stretch it: the total stays 2 pi chi."
     , txt env x (yy2 + 22) 14 (gray 0.67) "chi = 2 - 2g (g = number of holes)."
     , txt env x (yy2 + 44) 14 (gray 0.67) "The amount of curvature depends on topology." ]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p@(x, y) ->
    let near i v = let Vw a b d = view env w v in [i | d > 0 && dist2 p (a, b) < 18]
        hits = near 0 (wA w) ++ near 1 (wB w) ++ near 2 (wC w)
    in w { wDrag = foldl' (\_ h -> h) (-1) hits, wStart = Just (x, y, wYaw w, wTilt w) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion p@(x, y) | envDown env -> case wDrag w of
    0 -> setV (\v -> w { wA = v })
    1 -> setV (\v -> w { wB = v })
    2 -> setV (\v -> w { wC = v })
    _ -> case wStart w of
      Just (sx, sy, yaw, tilt) | x + envW env / 2 < envW env * 0.64 ->
        w { wYaw = yaw - (x - sx) * 0.01, wTilt = clampF (-1.4) 1.4 (tilt - (y - sy) * 0.01) }
      _ -> w
    where setV f = maybe w f (unview env w p)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> preset (fromEnum c - fromEnum '0') w
    | toLower c == 'm' -> w { wMC = not (wMC w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wR = min 10 (wR w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wR = max 0.5 (wR w - 0.5) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 例 2（北極と赤道の 2 点）：3 つの角がすべて 90°。和 270°、超過 90° = π/2 = 球面の 1/8 の面積
-- 2. 頂点を寄せて小さい三角形に。超過は面積に比例して 0 に近づく（平面の 180° にもどる）
-- 3. R を 2 倍にすると、同じ形の三角形の面積は 4 倍。でも角度は変わらない → 「面積 ÷ R²」が角度で決まる
-- 4. closedSurfaces に自分の面（例：(1 + 0.3 sin 5u) をかけた球）を追加。∬K dA は 4π のまま？

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

type D = Double
data D3 = D3 !D !D !D
type Surf = D -> D -> D3

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
