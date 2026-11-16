-- ============================================
-- 45-08 モールの応力円
-- 同じ応力状態でも、見る向き（切り口の角度 θ）で 垂直応力 σ と せん断応力 τ の値は変わる
-- 要素を θ 回すと、モールの円の上では 2θ 進む。円の左右の端が主応力、上下の端が最大せん断応力
-- 操作：円の上の X・Y をドラッグ = σx, σy, τxy を変える / 左の要素をドラッグ = 回す / ←→ = θ
--       P = 主応力の向き / M = 最大せん断の向き / 1〜4 = 見本 / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

pxPerMPa :: Float
pxPerMPa = 1.9             -- ★ 円の図の縮尺

data World = World
  { wSx, wSy, wTxy :: !Float   -- ★ 応力（MPa）：σx, σy, τxy
  , wTheta :: !Float           -- 要素を回す角度（ラジアン、反時計回りが正）
  , wGrab  :: !Char            -- つかんでいるもの（'X' / 'Y' / 'E' / ' ' = なし）
  }

world0 :: World
world0 = World 80 (-20) 40 0.35 ' '

main :: IO ()
main = runSketch (sketch "45-08 Mohr's circle" world0)
  { skBack  = hsb 212 45 13
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 要素の中心とモール円の原点（p5.js の座標）
elemC, mohrC :: Env -> Point
elemC env = (envW env * 0.24, envH env * 0.56)
mohrC env = (envW env * 0.66, envH env * 0.56)

-- ★ 応力の座標変換（θ だけ回した面での σx′, σy′, τx′y′）
rotated :: World -> Float -> (Float, Float, Float)
rotated w t =
  let c = (wSx w + wSy w) / 2
      d = (wSx w - wSy w) / 2
  in ( c + d * cos (2 * t) + wTxy w * sin (2 * t)
     , c - d * cos (2 * t) - wTxy w * sin (2 * t)
     , -d * sin (2 * t) + wTxy w * cos (2 * t) )

-- ←→ を押している間、要素を回す
step :: Env -> World -> World
step env w = w { wTheta = atan2 (sin t) (cos t) }   -- −180°〜180° に収める
  where
    t = wTheta w + (if isDown env (SpecialKey KeyLeft) then 0.01 else 0)
                 - (if isDown env (SpecialKey KeyRight) then 0.01 else 0)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：回した要素と、その面にかかる応力の矢印
  [ seg 1 (hsb 0 0 40) (ecx - 150, ecy) (ecx + 150, ecy), seg 1 (hsb 0 0 40) (ecx, ecy - 150) (ecx, ecy + 150)
  , Color (hsbA 200 40 55 70) (Polygon sq), path 2 (hsb 200 50 100) (sq ++ take 1 sq) ]
  ++ normalArrows
  ++ [ arrowL 3 (hsb 320 60 100) (a + 14, h) (a + 14, -h), arrowL 3 (hsb 320 60 100) (-a - 14, -h) (-a - 14, h)
     , arrowL 3 (hsb 320 60 100) (-h, -a - 14) (h, -a - 14), arrowL 3 (hsb 320 60 100) (h, a + 14) (-h, a + 14)
     , seg 1.5 (hsbA 0 0 100 70) (tr (0, 0)) (tr (a, 0)) ]   -- x′ 軸
  ++ [ path 1.5 (hsbA 0 0 100 60) (arcPts ecx ecy 40 (min (-th) 0) (max (-th) 0)) | abs th > 0.01 ]
  ++ [ txt (ecx + 48) (ecy - 10) 14 (hsb 0 0 100) ("t = " ++ fmt 1 (th * 180 / pi) ++ " deg") ]
  -- ---- 右：モールの応力円（横軸 σ、縦軸 τ は下向きが正）
  ++ [ seg 1 (hsb 0 0 45) (mcx - 290, mcy) (mcx + 290, mcy), seg 1 (hsb 0 0 45) (mcx, mcy - 240) (mcx, mcy + 240)
     , txt (mcx + 150) (mcy - 26) 14 (hsb 0 0 60) "sigma (+ tension)", txt (mcx + 8) (mcy + 236) 14 (hsb 0 0 60) "tau (down = +)" ]
  ++ [ txt (xS v - 12) (mcy + 18) 14 (hsb 0 0 60) (show (round v :: Int)) | v <- [-120, -80, -40, 40, 80, 120 :: Float] ]
  ++ [ circ 2 (hsb 0 0 100) (xS c) mcy (2 * bigR * pxPerMPa)
     -- もとの面 X, Y（ドラッグできる）と、回した面 X′, Y′
     , seg 1.5 (hsbA 0 0 100 35) (xS sx, yT txy) (xS sy, yT (-txy))
     , seg 2.5 (hsb 50 90 100) (xS rsx, yT rtxy) (xS rsy, yT (-rtxy)) ]
  -- 2θ の弧
  ++ [ path 1.5 (hsbA 50 90 100 70) (arcPts (xS c) mcy 30 (min a0 (a0 - 2 * th)) (max a0 (a0 - 2 * th))) | bigR > 3 && abs th > 0.01 ]
  ++ [ dot (hsb 0 0 70) (xS sx) (yT txy) 16, dot (hsb 0 0 70) (xS sy) (yT (-txy)) 16
     , dot (hsb 35 90 100) (xS rsx) (yT rtxy) 13, dot (hsb 150 70 100) (xS rsy) (yT (-rtxy)) 13
     , dot (hsb 0 80 100) (xS s1) mcy 10, dot (hsb 0 80 100) (xS s2) mcy 10
     , dot (hsb 320 60 100) (xS c) (yT bigR) 10, dot (hsb 320 60 100) (xS c) (yT (-bigR)) 10
     , txt (xS sx + 12) (yT txy + 18) 14 (hsb 0 0 100) "X (sx, txy)", txt (xS sy - 110) (yT (-txy) - 10) 14 (hsb 0 0 100) "Y (sy, -txy)"
     , txt (xS s1 + 8) (mcy - 8) 14 (hsb 0 0 100) "s1", txt (xS s2 - 26) (mcy - 8) 14 (hsb 0 0 100) "s2"
     , txt (xS c + 8) (yT bigR + 18) 14 (hsb 0 0 100) "tau max" ]
  -- ---- 数値
  ++ [ txt 20 40 17 (hsb 0 0 100) "Drag X/Y: stress   Drag element or Left/Right: rotate   P: principal   M: max shear   1-4: presets"
     , txt 20 68 15 (hsb 0 0 60) "Turn the element by t and its points (orange, green) move 2t around the circle. Zero shear = principal direction."
     , Color (hsbA 0 0 100 8) (rect (ix - 12 + bw / 2) (iy - 26 + 74) bw 148)
     , txt ix iy 14 (hsb 0 0 100) ("sx = " ++ fmt 0 sx ++ "  sy = " ++ fmt 0 sy ++ "  txy = " ++ fmt 0 txy ++ " MPa")
     , txt ix (iy + 24) 14 (hsb 0 0 100) ("center (sx+sy)/2 = " ++ fmt 0 c ++ "   radius R = " ++ fmt 1 bigR)
     , txt ix (iy + 48) 14 (hsb 35 90 100) ("rotated: sx' = " ++ fmt 1 rsx ++ "  sy' = " ++ fmt 1 rsy ++ "  txy' = " ++ fmt 1 rtxy)
     , txt ix (iy + 72) 14 (hsb 0 80 100) ("principal s1 = " ++ fmt 1 s1 ++ "  s2 = " ++ fmt 1 s2 ++ "  tp = " ++ fmt 1 (thP * 180 / pi) ++ " deg")
     , txt ix (iy + 96) 14 (hsb 320 60 100) ("max shear tau = R = " ++ fmt 1 bigR ++ " MPa (45 deg from principal)") ]
  where
    (ecx, ecy) = elemC env
    (mcx, mcy) = mohrC env
    sx = wSx w; sy = wSy w; txy = wTxy w; th = wTheta w
    (rsx, rsy, rtxy) = rotated w th
    c = (sx + sy) / 2
    d = (sx - sy) / 2
    bigR = sqrt (d * d + txy * txy)
    s1 = c + bigR
    s2 = c - bigR
    thP = 0.5 * atan2 txy d
    a0 = atan2 txy d
    xS s = mcx + s * pxPerMPa
    yT t = mcy + t * pxPerMPa
    -- 要素の座標 → 画面（p5.js の rotate(−θ)：画面では反時計回り）
    tr (x, y) = let ph = negate th in (ecx + x * cos ph - y * sin ph, ecy + x * sin ph + y * cos ph)
    a = 62
    k = 0.7
    sq = map tr [(-a, -a), (a, -a), (a, a), (-a, a)]
    nx = abs rsx * k
    ny = abs rsy * k
    h = rtxy * k / 2
    arrowL wd col p q = arrowSeg wd col (tr p) (tr q)
    -- 垂直応力（引張 = 外向き、圧縮 = 内向き）
    normalArrows =
      (if rsx >= 0
         then [arrowL 3 (hsb 35 90 100) (a + 6, 0) (a + 6 + nx, 0), arrowL 3 (hsb 35 90 100) (-a - 6, 0) (-a - 6 - nx, 0)]
         else [arrowL 3 (hsb 35 90 100) (a + 6 + nx, 0) (a + 6, 0), arrowL 3 (hsb 35 90 100) (-a - 6 - nx, 0) (-a - 6, 0)])
      ++ (if rsy >= 0
            then [arrowL 3 (hsb 150 70 100) (0, -a - 6) (0, -a - 6 - ny), arrowL 3 (hsb 150 70 100) (0, a + 6) (0, a + 6 + ny)]
            else [arrowL 3 (hsb 150 70 100) (0, -a - 6 - ny) (0, -a - 6), arrowL 3 (hsb 150 70 100) (0, a + 6 + ny) (0, a + 6)])
    ix = 30
    iy = envH env - 140
    bw = envW env * 0.44

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wGrab = pick }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = ' ' }
  EventMotion _ -> case wGrab w of
    'X' -> w { wSx = clampF (-120) 120 ((mx - mcx) / pxPerMPa), wTxy = clampF (-100) 100 ((my - mcy) / pxPerMPa) }
    'Y' -> w { wSy = clampF (-120) 120 ((mx - mcx) / pxPerMPa), wTxy = clampF (-100) 100 (-(my - mcy) / pxPerMPa) }
    'E' -> w { wTheta = atan2 (ecy - my) (mx - ecx) }
    _ -> w
  EventKey (Char ch) Down _ _
    | Just (a, b, c) <- lookup ch presets -> w { wSx = a, wSy = b, wTxy = c }
    | ch `elem` "pP" -> w { wTheta = thP }
    | ch `elem` "mM" -> w { wTheta = thP + pi / 4 }
    | ch `elem` "rR" -> world0
  _ -> w
  where
    (mx, my) = mouseP env
    (ecx, ecy) = elemC env
    (mcx, mcy) = mohrC env
    presets = [('1', (100, 0, 0)), ('2', (0, 0, 60)), ('3', (80, 80, 0)), ('4', (80, -20, 40))]
    thP = 0.5 * atan2 (wTxy w) ((wSx w - wSy w) / 2)
    xS s = mcx + s * pxPerMPa
    yT t = mcy + t * pxPerMPa
    pick | dist2 (mx, my) (xS (wSx w), yT (wTxy w)) < 20 = 'X'
         | dist2 (mx, my) (xS (wSy w), yT (-(wTxy w))) < 20 = 'Y'
         | dist2 (mx, my) (ecx, ecy) < 190 && my > 90 = 'E'
         | otherwise = ' '

-- ---------- やってみよう ----------
-- 1. 1（単軸引張）で要素を 45° 回そう。せん断応力が最大（σ/2）になる。延性材料が斜め 45° にすべる理由
-- 2. 2（純せん断）で 45° 回すと、引張と圧縮だけになる。チョークをねじると斜め 45° に割れる理由
-- 3. 3（等二軸）では円が点になる。どの向きでも同じ応力（せん断なし）。風船の表面がこれ
-- 4. P を押して主応力の向きにし、要素の矢印からせん断（ピンク）が消えるのを確かめよう
-- 5. 降伏条件（トレスカ）：τmax が降伏せん断応力に達したら降伏。τmax = 60 MPa を超えない σx, σy の組を探そう

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

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 線で描く矢印（p5.js の arrow と同じ形。短すぎるときは描かない）
arrowSeg :: Float -> Color -> Point -> Point -> Picture
arrowSeg w c p@(x1, y1) q@(x2, y2)
  | dist2 p q < 4 = Blank
  | otherwise = Pictures [ seg w c p q
                         , seg w c q (x2 - 10 * cos (an - 0.45), y2 - 10 * sin (an - 0.45))
                         , seg w c q (x2 - 10 * cos (an + 0.45), y2 - 10 * sin (an + 0.45)) ]
  where an = atan2 (y2 - y1) (x2 - x1)

-- 円弧の点（p5.js の arc と同じ：角度は画面の向き、時計回り）
arcPts :: Float -> Float -> Float -> Float -> Float -> [Point]
arcPts x y r a0 a1 = [ (x + r * cos t, y + r * sin t) | i <- [0 .. 24 :: Int], let t = a0 + (a1 - a0) * fromIntegral i / 24 ]

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
