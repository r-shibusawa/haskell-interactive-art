-- ============================================
-- 01 スネルの法則と全反射
-- 上半分が屈折率 n1、下半分が n2 の 2 つの物質。境界で光は折れ曲がる
--   スネルの法則： n1 sin θ1 = n2 sin θ2    （θ は法線＝境界に垂直な線からはかる）
--   密→疎（n1 > n2）で θ1 が臨界角 sin θc = n2/n1 をこえると屈折光が消える（全反射）
-- マウス = 光源の位置（上か下か、入射角） / ←→ = n1 / ↑↓ = n2 / W = 波面 / F = 反射率（フレネル） / 1〜3 = 組み合わせ / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

lambda0 :: Float
lambda0 = 26               -- ★ 真空中の波長（px）

data World = World
  { wN1      :: !Float     -- ★ 屈折率（空気 1.0、水 1.33、ガラス 1.5、ダイヤ 2.42）
  , wN2      :: !Float
  , wWave    :: !Bool      -- 波面（光の進む速さが変わるのを見る）
  , wFresnel :: !Bool      -- 反射光の強さをフレネルの式で計算する
  , wT       :: !Float
  }

main :: IO ()
main = runSketch (sketch "36-01 Snell's Law and Total Reflection" (World 1 1.5 True True 0))
  { skBack  = hsb 210 20 25
  , skStep  = \_ _ w -> pure w { wT = wT w + 0.6 }
  , skDraw  = drawAll
  , skEvent = onEvent
  }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      n1 = wN1 w
      n2 = wN2 w
      t = wT w
      iy = h * 0.55
      ox = wd * 0.5
      o = (ox, iy)
      (mx, my) = mouseP5 env
      -- 光源 → 境界の点 O へ向かう光
      fromTop = my < iy
      na = if fromTop then n1 else n2     -- na = 入射側
      nb = if fromTop then n2 else n1     -- nb = 透過側
      sx = mx
      sy = if fromTop then min my (iy - 20) else max my (iy + 20)
      len = max 1e-3 (norm2 (ox - sx, iy - sy))
      dx = (ox - sx) / len
      dy = (iy - sy) / len
      th1 = atan2 (abs dx) (abs dy)        -- 入射角（法線から）
      sinT2 = na / nb * sin th1
      tir = sinT2 > 1
      th2 = if tir then 0 else asin sinT2
      sgnX = if dx >= 0 then 1 else -1
      sgnY = if fromTop then 1 else -1
      rdx = sgnX * sin th2                  -- 屈折光の向き
      rdy = sgnY * cos th2
      fdx = dx                              -- 反射光の向き
      fdy = negate dy
      -- フレネルの反射率（s 偏光と p 偏光の平均）
      c1 = cos th1
      c2 = cos th2
      rs = (na * c1 - nb * c2) / (na * c1 + nb * c2)
      rp = (na * c2 - nb * c1) / (na * c2 + nb * c1)
      rF | tir = 1
         | otherwise = (rs * rs + rp * rp) / 2
      r | wFresnel w = rF
        | tir = 1
        | otherwise = 0.25
      nAng = if fromTop then negate (pi / 2) else pi / 2   -- 入射側の法線の向き
      crit = if na > nb then Just (asin (nb / na)) else Nothing
      px = wd - 600
      py = h - 220
      deg v = nf 1 (v * 180 / pi)
      critText = case crit of
        Just c -> "Critical angle = " ++ deg c ++ " deg"
        Nothing -> "(low n to high n: no total reflection)"
  in Pictures
    [ onP5 env (Pictures
        -- 物質を塗る（屈折率が大きいほど青く濃く）
        ([ Color (hsb 210 (20 + (n1 - 1) * 30) 25) (box 0 0 wd iy)
         , Color (hsb 210 (20 + (n2 - 1) * 30) 25) (box 0 iy wd (h - iy))
         , Color (hsbA 0 0 100 60) (Line [(0, iy), (wd, iy)])
         , Color (hsbA 0 0 100 30) (dashed (ox, iy - 300) (ox, iy + 300))
         -- 光線
         , Color (hsb 45 70 100) (thickLine 3 (sx, sy) o)
         , Color (hsbA 45 70 100 (15 + 85 * r)) (thickLine 3 o (ox + fdx * 2000, iy + fdy * 2000))
         , if tir then Blank else Color (hsbA 170 70 100 (15 + 85 * (1 - r))) (thickLine 3 o (ox + rdx * 2000, iy + rdy * 2000))
         ]
         ++ (if wWave w then
               [ wavefronts sx sy dx dy len (lambda0 / na) t 45
               , wavefronts ox iy fdx fdy 500 (lambda0 / na) (t + len) 45
               , if tir then Blank else wavefronts ox iy rdx rdy 500 (lambda0 / nb) ((t + len) * na / nb) 170 ]
             else [])
         ++
         -- 角度の弧
         [ Color (hsb 45 90 100) (arcBetween o 60 nAng (atan2 (negate dy) (negate dx)))
         , Color (hsbA 45 90 100 60) (arcBetween o 75 nAng (atan2 fdy fdx))
         , if tir then Blank else Color (hsb 170 90 100) (arcBetween o 60 (negate nAng) (atan2 rdy rdx))
         , Color (hsb 45 20 100) (at (sx, sy) (disc 7))
         -- 臨界角
         , case crit of
             Just c -> Color (hsbA 0 80 100 60)
               (dashed o (ox - sgnX * sin c * 400, iy - sgnY * cos c * 400))
             Nothing -> Blank
         -- 説明パネルの下地
         , Color (hsbA 0 0 100 8) (box (px - 16) (py - 30) 600 210)
         ]))
    , txt env 20 40 18 white
        ("n1 = " ++ nf 2 n1 ++ " (L/R)  n2 = " ++ nf 2 n2 ++ " (Up/Down)  W: wave " ++ onOff (wWave w)
         ++ "  F: Fresnel " ++ onOff (wFresnel w))
    , txt env 20 68 15 (hsb 0 0 70) "1: air-water  2: air-glass  3: glass-air  R: reset   Mouse moves the source (above or below)."
    , txt env 20 92 15 (hsb 0 0 70) "Yellow = incident and reflected, cyan = refracted. Red dashes = critical angle."
    , txt env px py 16 white ("Incident n = " ++ nf 2 na ++ "  th1 = " ++ deg th1 ++ " deg")
    , if tir
        then txt env px (py + 28) 16 (hsb 0 80 100)
               ("Total reflection!  n sin th1 = " ++ nf 3 (na * sin th1) ++ " > " ++ nf 2 nb)
        else Pictures
          [ txt env px (py + 28) 16 white ("Refracted n = " ++ nf 2 nb ++ "  th2 = " ++ deg th2 ++ " deg")
          , txt env px (py + 60) 16 (hsb 45 90 100)
              ("n1 sin th1 = " ++ nf 2 na ++ " x " ++ nf 3 (sin th1) ++ " = " ++ nf 3 (na * sin th1))
          , txt env px (py + 84) 16 (hsb 45 90 100)
              ("n2 sin th2 = " ++ nf 2 nb ++ " x " ++ nf 3 (sin th2) ++ " = " ++ nf 3 (nb * sin th2))
          ]
    , txt env px (py + 112) 14 (hsb 0 0 70)
        ("Reflectance R = " ++ nf 1 (r * 100) ++ "%  Transmittance " ++ nf 1 ((1 - r) * 100) ++ "%")
    , txt env px (py + 134) 14 (hsb 0 0 70) critText
    , txt env px (py + 156) 14 (hsb 0 0 70)
        ("Speed: c/" ++ nf 2 na ++ " then c/" ++ nf 2 nb ++ ", wavelength also 1/n (wavefronts)")
    ]

-- 光線に直角な短い線（波面）を波長の間隔で描く
wavefronts :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Picture
wavefronts x y dx dy len lam phase hue =
  let off = wrapF lam phase
      count = max 0 (ceiling ((len - off) / lam)) :: Int
      one i =
        let s = off + fromIntegral i * lam
            px = x + dx * s
            py = y + dy * s
        in thickLine 1.5 (px - dy * 10, py + dx * 10) (px + dy * 10, py - dx * 10)
  in Color (hsbA hue 40 100 50) (Pictures (map one [0 .. count - 1]))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wN1 = max 1 (wN1 w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN1 = min 2.5 (wN1 w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wN2 = max 1 (wN2 w - 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wN2 = min 2.5 (wN2 w + 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "wW" -> w { wWave = not (wWave w) }
    | c `elem` "fF" -> w { wFresnel = not (wFresnel w) }
    | c == '1' -> w { wN1 = 1, wN2 = 1.33 }
    | c == '2' -> w { wN1 = 1, wN2 = 1.5 }
    | c == '3' -> w { wN1 = 1.5, wN2 = 1 }
    | c `elem` "rR" -> w { wN1 = 1, wN2 = 1.5 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 光源を下（ガラス側）に置いて、横へ動かすと屈折光が消える瞬間がある。そのときの θ1 と臨界角を比べよう
-- 2. n1 = n2 にすると何が起きる？（境界が「ない」のと同じ）
-- 3. 波面（W）を見る：密な物質の中では間隔が狭い = 進むのが遅い。屈折はこの速さの違いから生まれる
-- 4. F を押して反射率を見る。θ1 = 90° に近づくと鏡のように反射する（夕方の水面がまぶしい理由）


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字・破線）
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

-- 2 つの角度の間の弧
arcBetween :: Point -> Float -> Float -> Float -> Picture
arcBetween (x, y) r a1 a2 =
  let d = atan2 (sin (a2 - a1)) (cos (a2 - a1))
  in thickPath 2 [ (x + r * cos a, y + r * sin a) | k <- [0 .. 16 :: Int], let a = a1 + d * fromIntegral k / 16 ]

-- 点線
dashed :: Point -> Point -> Picture
dashed (x1, y1) (x2, y2) =
  let n = max 1 (floor (dist2 (x1, y1) (x2, y2) / 12)) :: Int
      f i = fromIntegral i / fromIntegral n
      pt t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Pictures [ Line [pt (f i), pt (f (i + 1))] | i <- [0, 2 .. n - 1] ]
