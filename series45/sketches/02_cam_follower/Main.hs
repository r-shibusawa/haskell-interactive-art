-- ============================================
-- 45-02 カムと従動節
-- 「どんなふうに持ち上げたいか」（変位曲線 s(θ)）を先に決め、そこからカムの輪郭を作る
-- 等速は角で加速度が無限大（衝撃）、単振動は加速度が飛ぶ、サイクロイドはなめらか
-- 回転が速すぎると、ばねが従動節を押さえきれずカムから離れる（跳躍・ジャンプ）
-- 操作：1〜3 = 変位曲線（等速・単振動・サイクロイド）/ ←→ = 基礎円の半径 / ↑↓ = 回転数 / スペース = 止める
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

liftH, rollerR, springAcc :: Double
liftH = 25                 -- ★ リフト量 h（mm）
rollerR = 8                -- ★ ローラーの半径（mm）
springAcc = 300000         -- ★ ばねが従動節を押し戻す加速度（mm/s²）

riseEnd, dwellEnd, fallEnd :: Double
riseEnd = 120              -- ★ 上昇・休止・下降の区切り（度）
dwellEnd = 180
fallEnd = 300

pxmm :: Float
pxmm = 2                   -- 1 mm を何 px で描くか

-- 運動の計算は桁の多い Double で行う（数値微分の誤差を小さくするため）
data World = World
  { wLaw     :: !Int       -- 1 等速 / 2 単振動 / 3 サイクロイド
  , wBaseR   :: !Double    -- ★ 基礎円の半径（mm）
  , wRpm     :: !Double    -- ★ カムの回転数
  , wTh      :: !Double    -- カム角
  , wPlaying :: !Bool
  , wFollY   :: !Double    -- 従動節の持ち上がり（mm）
  , wFollV   :: !Double    -- その速度（mm/s）
  , wGapMax  :: !Double    -- この 1 回転で離れた最大のすき間
  , wJump    :: !Int       -- 「跳躍あり」を出し続けるフレーム数
  }

main :: IO ()
main = runSketch (sketch "45-02 Cam and follower" (World 1 60 300 0 True 0 0 0 0))
  { skBack  = hsb 212 45 13
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 変位曲線（u = 0〜1 → 0〜1）
law :: Int -> Double -> Double
law 1 u = u                                       -- 等速
law 2 u = (1 - cos (pi * u)) / 2                  -- 単振動
law _ u = u - sin (2 * pi * u) / (2 * pi)         -- サイクロイド

-- カム角 θ（ラジアン）→ リフト s（mm）
camLift :: Int -> Double -> Double
camLift lw t
  | d < riseEnd = liftH * law lw (d / riseEnd)
  | d < dwellEnd = liftH
  | d < fallEnd = liftH * (1 - law lw ((d - dwellEnd) / (fallEnd - dwellEnd)))
  | otherwise = 0
  where d = wrapD 360 (t * 180 / pi)

camD1, camD2 :: Int -> Double -> Double
camD1 lw t = (camLift lw (t + 1e-3) - camLift lw (t - 1e-3)) / 2e-3                          -- ds/dθ
camD2 lw t = (camLift lw (t + 2e-3) - 2 * camLift lw t + camLift lw (t - 2e-3)) / 4e-6        -- d²s/dθ²

-- ---- 従動節の運動（スローモーションで積分）：カムに触れていなければ、ばねの加速度で落ちる
step :: World -> World
step w0 = w2 { wJump = max 0 (wJump w2 - 1) }
  where
    lw = wLaw w0
    omega = wRpm w0 * 2 * pi / 60
    n = 20 :: Int
    dth = 0.03 / fromIntegral n
    dt = dth / omega
    sub w _ =
      let th = wTh w + dth
          v1 = wFollV w - springAcc * dt
          y1 = wFollY w + v1 * dt
          s = camLift lw th
          (y2, v2) = if y1 <= s then (s, camD1 lw th * omega) else (y1, v1)
      in w { wTh = th, wFollY = y2, wFollV = v2, wGapMax = max (wGapMax w) (y2 - s) }
    w1 = if wPlaying w0 then foldl' sub w0 [1 .. n] else w0
    w2 | wPlaying w0 && wTh w1 > 2 * pi =
           w1 { wTh = wTh w1 - 2 * pi, wJump = if wGapMax w1 > 0.05 then 90 else wJump w1, wGapMax = 0 }
       | otherwise = w1

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate ccx ccy (Pictures camPic) ]
  ++ concat (zipWith graphRow [0 ..] rows)
  ++ [ txt (gx + gw * 0.36) (gy + gh * 2 + 14) 14 (hsb 320 60 100) "^ constant velocity: infinite accel = shock" | lw == 1 ]
  ++ [ txt (gx + fromIntegral d / 360 * gw - 8) (gy + gh * 3 + 18) 14 (hsb 0 0 60) (show d) | d <- [0, 60 .. 360 :: Int] ]
  -- ---- 文字
  ++ [ txt 20 40 18 (hsb 0 0 100) ("1-3: motion law (" ++ lawName ++ ")   Left/Right: base circle " ++ fmt 0 (wBaseR w)
                                   ++ " mm   Up/Down: " ++ fmt 0 (wRpm w) ++ " rpm   Space: " ++ (if wPlaying w then "stop" else "run"))
     , txt 20 68 15 (hsb 0 0 60) ("Choose s(t), then radius = base circle + s(t) gives the cam. Slow motion (really "
                                  ++ fmt 1 (wRpm w / 60) ++ " turns per second)")
     , Color (hsbA 0 0 100 8) (rectTL (ix - 12) (iy - 26) 520 118)
     , txt ix iy 16 (hsb 0 0 100) "Pressure angle: tan a = (ds/dt) / (R0 + s)"
     , txt ix (iy + 28) 16 (hsb (if aDeg > 30 then 0 else 140) 70 100)
         ("now a = " ++ fmt 1 (alphaNow * 180 / pi) ++ " deg   max " ++ fmt 1 aDeg ++ " deg"
          ++ (if aDeg > 30 then " (over 30: binds)" else " (30 or less: good)"))
     , txt ix (iy + 56) 16 (hsb (if jumping then 0 else 140) 70 100)
         (if jumping then "JUMP! follower left the cam (gap " ++ fmt 2 (max gap 0) ++ " mm)" else "No jump (spring holds the follower)")
     , txt ix (iy + 80) 14 (hsb 0 0 60) "Jump when |d2s/dt2| w^2 needed by the cam > spring acceleration" ]
  where
    lw = wLaw w
    th = wTh w
    baseR = wBaseR w
    lawName = case lw of { 1 -> "constant velocity"; 2 -> "simple harmonic"; _ -> "cycloidal" }
    gap = wFollY w - camLift lw th
    jumping = wJump w > 0 || gap > 0.05
    ccx = envW env * 0.25
    ccy = envH env * 0.7
    -- ---- カムの輪郭：ピッチ曲線（ローラー中心の通り道）を内側へローラー半径ぶんオフセット
    pitch = [ (r * cos a, r * sin a)
            | i <- [0 .. 239 :: Int]
            , let p = fromIntegral i / 240 * 2 * pi
                  r = realToFrac (baseR + rollerR + camLift lw p) * pxmm
                  a = realToFrac (-pi / 2 + p - th) :: Float ]   -- カム角 p の点は、いま画面のこの向きにある
    prev = drop 239 pitch ++ take 239 pitch
    next = drop 1 pitch ++ take 1 pitch
    rr = realToFrac rollerR * pxmm
    prof = zipWith3 (\(ax, ay) (x, y) (bx, by) ->
                       let tx = bx - ax
                           ty = by - ay
                           tl = let l = sqrt (tx * tx + ty * ty) in if l == 0 then 1 else l
                       in (x - ty / tl * rr, y + tx / tl * rr)) prev pitch next
    thF = realToFrac th :: Float
    ry = -(realToFrac (baseR + rollerR + wFollY w)) * pxmm
    topY = -(realToFrac (baseR + rollerR + liftH)) * pxmm - 120
    camPic =
      [ Color (hsbA 0 0 45 60) (Circle (realToFrac baseR * pxmm))           -- 基礎円
      , Color (hsbA 35 60 80 50) (closedLine pitch)                        -- ピッチ曲線
      , Color (hsb 200 45 60) (convexFan (0, 0) prof)                             -- カム
      , Color (hsb 200 50 95) (thickPath 2 (prof ++ take 1 prof))
      , seg 1 (hsbA 0 0 100 60) (0, 0) (40 * cos (-pi / 2 - thF), 40 * sin (-pi / 2 - thF))   -- 回転の目印
      , dot (hsb 0 0 100) 0 0 14
      -- 従動節（ローラー + 棒 + ばね）
      , seg 8 (hsb 0 0 85) (0, ry) (0, ry - 120)
      , dot (hsb (if gap > 0.05 then 0 else 35) 85 100) 0 ry (rr * 2)
      , Color (hsb 0 0 35) (Pictures [rectTL (-50) (topY - 10) 100 10, rectTL (-34) (ry - 50) 68 6])   -- ばね受けとつば
      , path 2 (hsb 140 50 90) [ (if i == 0 || i == 12 then 0 else if odd i then 20 else -20, lerpF topY (ry - 50) (fromIntegral i / 12))
                               | i <- [0 .. 12 :: Int] ] ]
    -- ---- 圧力角 α：tan α = (ds/dθ) / (基礎円 + ローラー + s)
    alphaOf t = atan (camD1 lw t / (baseR + rollerR + camLift lw t))
    alphaNow = alphaOf th
    alphaMax = maximum (0 : [abs (alphaOf (fromIntegral i / 360 * 2 * pi)) | i <- [0 .. 359 :: Int]])
    aDeg = alphaMax * 180 / pi
    -- ---- グラフ（変位・速度・加速度）
    gx = envW env * 0.54
    gw = envW env - gx - 50
    gy = 240
    gh = (envH env - gy - 50) / 3
    riseRad = riseEnd * pi / 180
    peakV = liftH / riseRad * 2.2
    peakA = liftH / (riseRad * riseRad) * 7
    rows = [ ("lift s", 35, camLift lw, liftH * 1.1, 0.9)
           , ("velocity ds/dt", 190, camD1 lw, peakV, 0.5)
           , ("accel d2s/dt2", 320, camD2 lw, peakA, 0.5) ]
    thNow = wrapD (2 * pi) th
    ix = envW env * 0.54 + 12
    iy = 120
    graphRow :: Int -> (String, Float, Double -> Double, Double, Float) -> [Picture]
    graphRow i (name, hue, f, peak, base) =
      let y0 = gy + gh * fromIntegral i + gh * base
          amp = gh * (if base > 0.6 then 0.8 else 0.45)
          yOf t = y0 - clampF (-1.05) 1.05 (realToFrac (f t / peak)) * amp
      in [ seg 1 (hsb 0 0 30) (gx, y0) (gx + gw, y0)
         , path 2.5 (hsb hue 70 100) [ (gx + fromIntegral k / 360 * gw, yOf (fromIntegral k / 360 * 2 * pi)) | k <- [0 .. 360 :: Int] ]
         , dot (hsb hue 70 100) (gx + realToFrac (thNow / (2 * pi)) * gw) (yOf thNow) 11
         , txt gx (gy + gh * fromIntegral i + 14) 14 (hsb hue 50 100) name ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _ | k `elem` "123" -> w { wLaw = fromEnum k - fromEnum '0' }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wBaseR = clampD 20 80 (wBaseR w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wBaseR = clampD 20 80 (wBaseR w + 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRpm = clampD 100 3000 (wRpm w + 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRpm = clampD 100 3000 (wRpm w - 100) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 → 2 → 3 と切り替えて加速度のグラフを比べよう。とげ（∞）→ 段差 → なめらか
-- 2. ↑ で回転数を上げていく。単振動（2）とサイクロイド（3）で、跳躍が始まる回転数はそれぞれいくつ？
-- 3. ← で基礎円を小さくすると圧力角が大きくなる。30° を超えない最小の基礎円を探そう（小さいカム ⇄ 小さい圧力角）
-- 4. riseEnd を 60 にすると（急に持ち上げる）、圧力角と跳躍はどうなる？
-- 5. law に「5 次多項式 10u³ − 15u⁴ + 6u⁵」を 4 番目として足し（event の "123" も "1234" に）、加速度の最大値をサイクロイドと比べよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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

-- 閉じた細い折れ線
closedLine :: [Point] -> Picture
closedLine ps = Line (ps ++ take 1 ps)

-- 中心 o から見て一周する輪郭を塗る。gloss の Polygon は凸の形しか正しく塗れないので、
-- 中心と輪郭の点をつないだ扇を、凸のままでいられるところまでまとめて 1 枚ずつ塗る
convexFan :: Point -> [Point] -> Picture
convexFan _ [] = Blank
convexFan o (p0 : rest) = Pictures (go [p0] (rest ++ [p0]))
  where
    go grp [] = [Polygon (o : reverse grp)]
    go grp@(lastP : _) (q : qs)
      | convex (o : reverse (q : grp)) = go (q : grp) qs
      | otherwise = Polygon (o : reverse grp) : go [q, lastP] qs
    go [] _ = []
    convex ps =
      let n = length ps
          trip = zip3 ps (drop 1 ps ++ take 1 ps) (drop 2 ps ++ take 2 ps)
          cr = [ (bx - ax) * (cy - by) - (by - ay) * (cx - bx) | ((ax, ay), (bx, by), (cx, cy)) <- trip ]
      in n < 4 || all (>= -1e-3) cr || all (<= 1e-3) cr

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

wrapD :: Double -> Double -> Double
wrapD m x = x - m * fromIntegral (floor (x / m) :: Int)

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""
