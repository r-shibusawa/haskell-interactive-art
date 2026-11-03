-- ============================================
-- 01 地図帳（チャートとアトラス）
-- 球面全体を 1 枚の平らな地図で表すことはできない。でも 2 枚あれば足りる（地図帳 = アトラス）
--   北極から投影した地図（北極が無限遠に飛ぶ）と、南極から投影した地図。重なる所は w → w / |w|² で行き来できる
-- 地図や球をクリック = 点 P を動かす / ←→ ↑↓ = P を動かす / ドラッグ（球）= 回す / M = 右の地図をメルカトルに / A = 自動 / C = 小さな円
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

circR, chartScale :: Float
circR = 0.18               -- ★ P のまわりの小さな円（球の上の角度）
chartScale = 0.22          -- ★ 地図の拡大率（パネルの幅に対する 1 の長さ）

data World = World
  { wLat, wLon :: !Float       -- 点 P（緯度・経度、ラジアン）
  , wYaw, wTilt :: !Float      -- 球の見る向き
  , wMercator, wAuto, wCircle :: !Bool
  , wStart :: !(Maybe (Float, Float, Float, Float))   -- 押した位置（p5 座標）と、そのときの向き
  }

main :: IO ()
main = runSketch (sketch "28-01 Charts and Atlas" (World 0.5 0.6 0.4 0.45 False False True Nothing))
  { skBack  = rgb255 16 20 30
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- 緯度経度 → 球面の点（z が北）
sph :: Float -> Float -> V3
sph la lo = V3 (cos la * cos lo) (cos la * sin lo) (sin la)

-- ★ 2 枚のチャート（立体射影）
chartN, chartS, merc :: V3 -> Point
chartN (V3 x y z) = (x / (1 - z), y / (1 - z))   -- 北極から：北極は ∞ へ
chartS (V3 x y z) = (x / (1 + z), y / (1 + z))   -- 南極から：南極は ∞ へ
merc (V3 x y z) =
  let la = asin (clampF (-1) 1 z)
  in (atan2 y x, log (tan (pi / 4 + clampF (-1.4) 1.4 la / 2)))

invN, invS :: Point -> V3
invN (a, b) = let d = 1 + a * a + b * b in V3 (2 * a / d) (2 * b / d) ((d - 2) / d)
invS (a, b) = let d = 1 + a * a + b * b in V3 (2 * a / d) (2 * b / d) ((2 - d) / d)

step :: Env -> World -> World
step env w0 =
  let w1 = if wAuto w0
             then w0 { wLat = 1.45 * sin (fromIntegral (envFrame env) * 0.008), wLon = wLon w0 + 0.004 }
             else w0
      lim = pi / 2 - 0.001
      hold k f w = if isDown env (SpecialKey k) then f w else w
  in hold KeyLeft (\w -> w { wLon = wLon w - 0.02 })
   $ hold KeyRight (\w -> w { wLon = wLon w + 0.02 })
   $ hold KeyUp (\w -> w { wLat = min (wLat w + 0.02) lim })
   $ hold KeyDown (\w -> w { wLat = max (wLat w - 0.02) (-lim) }) w1

-- パネルの配置
data Panel = Panel { pnX, pnW, pnCX, pnCY, pnK :: !Float, pnKind :: !Char }

panelTop :: Float
panelTop = 120

panelH :: Env -> Float
panelH env = envH env - panelTop - 150

panels :: Env -> World -> [Panel]
panels env w =
  let pw = envW env / 3
      cy = panelTop + panelH env / 2
      mk i kind = Panel (i * pw) pw (i * pw + pw / 2) cy (if kind == 'M' then pw * 0.15 else pw * chartScale) kind
  in [mk 0 'G', mk 1 'N', mk 2 (if wMercator w then 'M' else 'S')]

chartOf :: Char -> V3 -> Point
chartOf 'N' = chartN
chartOf 'S' = chartS
chartOf _ = merc

fmt :: Float -> String
fmt v | abs v > 999 = if v > 0 then "inf" else "-inf"
      | otherwise = nf 2 v

draw :: Env -> World -> Picture
draw env w =
  let p = sph (wLat w) (wLon w)
      pns = panels env w
      top = panelTop
      ph = panelH env
      frameOf pn = let (a, b) = px env (pnX pn + pnW pn / 2) (top - 10 + (ph + 20) / 2)
                   in Color (withA 0.16 white) (rectLine a b (pnW pn - 20) (ph + 20))
      one pn = if pnKind pn == 'G' then drawGlobe env w pn p else drawChart env w pn p top ph
      pw = envW env / 3
      (wNx, wNy) = chartN p
      (wSx, wSy) = chartS p
      r2 = wNx * wNx + wNy * wNy
      by = envH env - 110
      onOff b = if b then "ON" else "off"
      gold = rgb255 255 210 90
      lonDeg = wrapF 360 (jsMod (degF (wLon w)) 360 + 540) - 180
  in Pictures $
     map frameOf pns ++ map one pns ++
     [ txt env 20 34 16 white ("Click: place P   Arrows: move P   Drag globe: turn   M: Mercator " ++ onOff (wMercator w) ++ "   A: auto " ++ onOff (wAuto w) ++ "   C: circle")
     , txt env 20 60 14 (gray 0.67) "One chart always misses one point (its projection center). Two charts cover the whole sphere: an atlas."
     , txt env 24 (top + 16) 15 gold "Sphere"
     , txt env (pw + 24) (top + 16) 15 (rgb255 120 200 255) "Chart phiN (from north pole)"
     , txt env (2 * pw + 24) (top + 16) 15 (rgb255 255 140 200) (if wMercator w then "Mercator (lat up to +-80 deg)" else "Chart phiS (from south pole)")
     , txt env 30 by 15 white ("P = lat " ++ nf 1 (degF (wLat w)) ++ " deg   lon " ++ nf 1 lonDeg ++ " deg")
     , txt env 30 (by + 26) 15 white ("phiN(P) = (" ++ fmt wNx ++ ", " ++ fmt wNy ++ ")     phiS(P) = (" ++ fmt wSx ++ ", " ++ fmt wSy ++ ")")
     , txt env 30 (by + 52) 15 gold ("Transition phiS o phiN^-1 (w) = w/|w|^2 = (" ++ fmt (wNx / r2) ++ ", " ++ fmt (wNy / r2) ++ ")  <- same as phiS(P)!")
     , txt env 30 (by + 78) 14 (gray 0.67) "Near the north pole phiN goes to infinity (off the map). Then use phiS. Both work except at the poles."
     ]

-- 球（正射影で描く）
data Vw = Vw !Float !Float !Bool !Float   -- 画面の x, y（p5 座標）、手前か、半径

view :: Env -> World -> Panel -> V3 -> Vw
view env w pn (V3 x y z) =
  let c = cos (wYaw w)
      s = sin (wYaw w)
      x1 = c * x + s * y
      y1 = -s * x + c * y
      t = wTilt w
      up = y1 * sin t + z * cos t
      depth = -y1 * cos t + z * sin t
      r = min (pnW pn * 0.4) (envH env * 0.28)
  in Vw (pnCX pn + x1 * r) (pnCY pn - up * r) (depth > 0) r

drawGlobe :: Env -> World -> Panel -> V3 -> Picture
drawGlobe env w pn p =
  let Vw _ _ _ r = view env w pn p
      (cx, cy) = px env (pnCX pn) (pnCY pn)
      gridCol a = withA (a / 255) (rgb255 200 220 255)
      lats = [ Color (gridCol (if la == 0 then 160 else 70)) (curveOnGlobe env w pn (\t -> sph (la * pi / 180) (t * tau)))
             | la <- [-75, -60 .. 75] ]
      lons = [ Color (gridCol 70) (curveOnGlobe env w pn (\t -> sph ((t - 0.5) * pi) (lo * pi / 180)))
             | lo <- [0, 30 .. 330] ]
      pole q c = let Vw x y f _ = view env w pn q
                 in if f then at (px env x y) (Color c (disc 6)) else Blank
      small = if wCircle w
                then Color (rgb255 120 255 160) (thickGlobe env w pn (smallCircle p))
                else Blank
      Vw qx qy qf _ = view env w pn p
  in Pictures $
     [ at (cx, cy) (Color (rgb255 30 50 80) (disc r)) ] ++ lats ++ lons ++
     [ pole (V3 0 0 1) (rgb255 120 200 255), pole (V3 0 0 (-1)) (rgb255 255 140 200), small
     , at (px env qx qy) (Color (withA (if qf then 1 else 80 / 255) (rgb255 255 210 90)) (disc 8)) ]

-- 手前にある部分だけをつないだ折れ線
frontPieces :: Env -> World -> Panel -> (Float -> V3) -> [[Point]]
frontPieces env w pn f = go [] [view env w pn (f (fromIntegral i / 96)) | i <- [0 .. 96 :: Int]]
  where
    go cur [] = [cur | length cur >= 2]
    go cur (Vw x y fr _ : rest)
      | fr = go (px env x y : cur) rest
      | otherwise = [cur | length cur >= 2] ++ go [] rest

curveOnGlobe :: Env -> World -> Panel -> (Float -> V3) -> Picture
curveOnGlobe env w pn f = Pictures (map Line (frontPieces env w pn f))

thickGlobe :: Env -> World -> Panel -> (Float -> V3) -> Picture
thickGlobe env w pn f = Pictures (map (thickPath 2) (frontPieces env w pn f))

-- P を中心とする角度 circR の小さな円の点（t = 0〜1）
smallCircle :: V3 -> Float -> V3
smallCircle p t =
  let cr = cross3 (V3 0 0 1) p
      e1 = if norm3 cr < 1e-6 then V3 1 0 0 else unit3 cr
      e2 = cross3 p e1
      a = t * tau
  in add3 (scale3 (cos circR) p) (scale3 (sin circR) (add3 (scale3 (cos a) e1) (scale3 (sin a) e2)))

-- チャート（平面の地図）。枠の外は切り取る
drawChart :: Env -> World -> Panel -> V3 -> Float -> Float -> Picture
drawChart env w pn p top ph =
  let clipR = ( pnX pn + 10, top - 10, pnX pn + pnW pn - 10, top + ph + 10 )
      (cx, cy) = px env (pnCX pn) (pnCY pn)
      gridCol a = withA (a / 255) (rgb255 200 220 255)
      lats = [ Color (gridCol (if la == 0 then 170 else 70))
                 (Pictures (map Line (chartPieces env pn clipR (\t -> sph (la * pi / 180) (t * tau)))))
             | la <- [-75, -60 .. 75] ]
      lons = [ Color (gridCol 70)
                 (Pictures (map Line (chartPieces env pn clipR (\t -> sph ((t - 0.5) * (pi - 0.02)) (lo * pi / 180)))))
             | lo <- [0, 30 .. 330] ]
      small = if wCircle w
                then Color (rgb255 120 255 160) (Pictures (map (thickPath 2) (chartPieces env pn clipR (smallCircle p))))
                else Blank
      (wx, wy) = chartOf (pnKind pn) p
      qx = pnCX pn + wx * pnK pn
      qy = pnCY pn - wy * pnK pn
      (x0, y0, x1, y1) = clipR
      dot' = if qx >= x0 && qx <= x1 && qy >= y0 && qy <= y1 && not (isNaN qx || isNaN qy)
               then at (px env qx qy) (Color (rgb255 255 210 90) (disc 7)) else Blank
      unit = if pnKind pn /= 'M' then at (cx, cy) (Color (rgb255 30 50 80) (disc (pnK pn))) else Blank   -- 単位円 = 赤道
  in Pictures ([unit] ++ lats ++ lons ++ [small, dot'])

chartPieces :: Env -> Panel -> (Float, Float, Float, Float) -> (Float -> V3) -> [[Point]]
chartPieces env pn r f =
  let g = chartOf (pnKind pn)
      n = 120 :: Int
      pts = [ if ok then Just (pnCX pn + a * pnK pn, pnCY pn - b * pnK pn) else Nothing
            | i <- [0 .. n]
            , let (a, b) = g (f (fromIntegral i / fromIntegral n))
                  ok = abs a < 50 && abs b < 50 ]
      -- ひと続きの部分に分ける（横に大きく飛ぶ所も切る）
      split cur [] = [reverse cur | length cur >= 2]
      split cur (Nothing : rest) = [reverse cur | length cur >= 2] ++ split [] rest
      split [] (Just q : rest) = split [q] rest
      split cur@((lx, _) : _) (Just q@(x, _) : rest)
        | abs (x - lx) < pnW pn * 0.5 = split (q : cur) rest
        | otherwise = [reverse cur | length cur >= 2] ++ split [q] rest
  in map (map (uncurry (px env))) (concatMap (clipPath r) (split [] pts))

-- 折れ線を長方形で切る
clipPath :: (Float, Float, Float, Float) -> [Point] -> [[Point]]
clipPath r pts = go [] (zip pts (drop 1 pts))
  where
    flush cur = [reverse cur | length cur >= 2]
    go cur [] = flush cur
    go cur ((a, b) : rest) = case clipSeg r a b of
      Nothing -> flush cur ++ go [] rest
      Just (a', b')
        | null cur || a' /= a -> flush cur ++ (if b' == b then go [b', a'] rest else [[a', b']] ++ go [] rest)
        | b' == b -> go (b : cur) rest
        | otherwise -> flush (b' : cur) ++ go [] rest

-- 線分を長方形で切る（リャン＝バースキー）
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (x0, y0, x1, y1) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      edge (t0, t1) (p, q)
        | p == 0 = if q < 0 then (2, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (t0', t1') = foldl edge (0, 1) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      pt t = if t == 0 then (ax, ay) else if t == 1 then (bx, by) else (ax + dx * t, ay + dy * t)
  in if t0' > t1' then Nothing else Just (pt t0', pt t1')

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (x, y) ->
    let (mx, my) = toP5 env x y in w { wStart = Just (mx, my, wYaw w, wTilt w) }
  EventMotion (x, y) -> case wStart w of
    Just (sx, sy, yaw, tilt) | envDown env && sx < envW env / 3 ->
      let (mx, my) = toP5 env x y
      in w { wYaw = yaw - (mx - sx) * 0.01, wTilt = clampF (-1.4) 1.4 (tilt + (my - sy) * 0.01) }
    _ -> w
  EventKey (MouseButton LeftButton) Up _ (x, y) ->
    let (mx, my) = toP5 env x y
    in case wStart w of
         Just (sx, sy, _, _) | dist2 (mx, my) (sx, sy) <= 4 -> (place env mx my w) { wStart = Nothing }
         _ -> w { wStart = Nothing }
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> w { wMercator = not (wMercator w) }
    'a' -> w { wAuto = not (wAuto w) }
    'c' -> w { wCircle = not (wCircle w) }
    _ -> w
  _ -> w

toP5 :: Env -> Float -> Float -> Point
toP5 env x y = (x + envW env / 2, envH env / 2 - y)

-- クリックした所に P を置く（メルカトルの地図の上では置けない）
place :: Env -> Float -> Float -> World -> World
place env mx my w =
  let setP (V3 x y z) = w { wLat = clampF (-(pi / 2 - 0.001)) (pi / 2 - 0.001) (asin (clampF (-1) 1 z))
                          , wLon = atan2 y x, wAuto = False }
      hit pn = mx >= pnX pn && mx <= pnX pn + pnW pn && pnKind pn /= 'M'
  in case filter hit (panels env w) of
       [] -> w
       (pn : _) -> case pnKind pn of
         'G' ->
           let Vw _ _ _ r = view env w pn (V3 1 0 0)
               x' = (mx - pnCX pn) / r
               u' = -(my - pnCY pn) / r
               d2' = 1 - x' * x' - u' * u'
           in if d2' < 0 then w else
              let d = sqrt d2'            -- 見ている向きの成分 → 元の座標へもどす
                  t = wTilt w
                  y1 = u' * sin t - d * cos t
                  z1 = u' * cos t + d * sin t
                  c = cos (wYaw w)
                  s = sin (wYaw w)
              in setP (V3 (c * x' - s * y1) (s * x' + c * y1) z1)
         kind ->
           let q = ((mx - pnCX pn) / pnK pn, -(my - pnCY pn) / pnK pn)
           in setP (if kind == 'N' then invN q else invS q)

-- ---------- やってみよう ----------
-- 1. P を北極に近づける（↑ を押し続ける）。φN の黄色い点はどこへ行く？ φS ではどこ？
-- 2. 緑の小さな円は、どの地図でも「円」のまま（立体射影とメルカトルは角度を保つ = 等角）。でも大きさは変わる
-- 3. 球面を 1 枚のチャートで覆えないのはなぜ？（球はコンパクト、平面はちがう）。トーラスなら何枚いる？
-- 4. chartN の式を (x, −y) / (1 − z) にすると、座標変換はどうなる？（複素数の 1/w になる）

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
