-- ============================================
-- 10 球面幾何と地図投影
-- 球面（K > 0）を平面（K = 0）に、ゆがみなしで写すことはできない（ガウスの驚異の定理）。どこかを犠牲にする
--   ティソーの指示楕円：地球上の同じ大きさの小さな円が、地図の上でどう変形するかを見る
-- 1 = 地球儀 2 = 正距円筒 3 = メルカトル 4 = 平射（立体射影）5 = ランベルト正積 6 = モルワイデ / T = ティソーの円 / G = 経緯線 / ドラッグ = 回す（地球儀）/ S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Numeric (showFFloat)

tissotR :: Float
tissotR = 6                -- ★ ティソーの円の半径（度）

data World = World
  { wMode :: !Int
  , wTissot, wGrid :: !Bool
  , wYaw, wTilt :: !Float  -- 地球儀の向き
  , wLast :: !Point
  , wMap :: Picture        -- 地図の絵（変わったときだけ描き直す）
  }

main :: IO ()
main = runSketch (sketch "28-10 Map Projections" (World 3 True True (-2.4) 0.5 (0, 0) Blank))
  { skBack  = rgb255 12 22 40
  , skStart = \env -> sum (map (length . lTris) land) `seq` pure (refresh env (World 3 True True (-2.4) 0.5 (0, 0) Blank))
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 大陸のおおまかな輪郭（経度, 緯度）。本物の地図データは使わず、手で打った多角形
continents :: [[(Float, Float)]]
continents =
  [ [(-168, 66), (-162, 70), (-140, 70), (-120, 72), (-95, 72), (-80, 73), (-62, 66), (-55, 52), (-66, 45), (-75, 38), (-81, 31), (-80, 25), (-90, 29), (-97, 26), (-97, 20), (-87, 21), (-83, 10), (-78, 8), (-92, 15), (-105, 20), (-112, 30), (-117, 33), (-124, 40), (-125, 48), (-133, 56), (-150, 60), (-165, 60)]
  , [(-50, 60), (-42, 60), (-20, 70), (-18, 80), (-35, 83), (-60, 82), (-70, 78), (-55, 70)]
  , [(-78, 8), (-60, 10), (-50, 0), (-35, -5), (-39, -15), (-48, -25), (-58, -35), (-65, -42), (-68, -55), (-74, -50), (-72, -35), (-71, -18), (-80, -5), (-77, 3)]
  , [(-17, 15), (-17, 21), (-10, 30), (-5, 36), (10, 37), (20, 32), (32, 31), (35, 28), (43, 12), (51, 12), (40, -2), (40, -15), (35, -25), (20, -35), (17, -29), (12, -17), (13, -5), (9, 4), (-8, 4), (-14, 10)]
  , [(-10, 36), (-9, 43), (-2, 44), (-5, 48), (5, 53), (10, 58), (5, 62), (15, 69), (28, 71), (40, 67), (60, 69), (70, 73), (90, 76), (110, 77), (140, 72), (160, 70), (180, 68), (180, 65), (170, 60), (160, 55), (142, 54), (140, 46), (130, 42), (127, 35), (122, 40), (120, 30), (110, 20), (106, 10), (100, 13), (98, 8), (103, 1), (95, 15), (90, 22), (80, 15), (77, 8), (72, 20), (67, 25), (57, 25), (52, 18), (44, 13), (35, 28), (35, 36), (27, 37), (28, 41), (20, 39), (13, 45), (16, 38), (9, 44), (3, 43), (-1, 37)]
  , [(114, -22), (122, -18), (131, -12), (137, -12), (142, -11), (146, -19), (153, -25), (151, -34), (146, -39), (138, -35), (131, -31), (118, -35), (115, -34)]
  , [(130, 31), (135, 34), (140, 35), (141, 38), (142, 40), (141, 45), (145, 44), (140, 41), (139, 38), (136, 37), (132, 35)]
  , [(-5, 50), (1, 51), (-2, 56), (-5, 58), (-6, 55)]
  , [(44, -25), (47, -25), (50, -15), (49, -12), (44, -17)]
  ]

rad :: Float -> Float
rad d = d * pi / 180

-- 陸：輪郭（ラジアン）と、塗るための三角形
data Land = Land { lOutline :: [Point], lTris :: [(Point, Point, Point)] }

land :: [Land]
land = map mk (map subdivide continents ++ [antarctica])
  where mk o = Land o (triangulate o)

-- 輪郭の辺を 2° 以下に細かく分ける（曲がった地図でも辺が曲がるように）
subdivide :: [(Float, Float)] -> [Point]
subdivide poly = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    edge (ax, ay) (bx, by) =
      let n = max 1 (ceiling (max (abs (bx - ax)) (abs (by - ay)) / 2)) :: Int
      in [ (rad (ax + (bx - ax) * fromIntegral k / fromIntegral n), rad (ay + (by - ay) * fromIntegral k / fromIntegral n)) | k <- [0 .. n - 1] ]

-- 南極大陸（帯状）
antarctica :: [Point]
antarctica =
  [ (rad lo, rad (-70 + 4 * sin (rad (lo * 3)))) | lo <- [-180, -177 .. 180] ]
  ++ [(pi, -pi / 2 + 0.01), (-pi, -pi / 2 + 0.01)]

-- へこんだ多角形を三角形に分ける（耳切り法）。gloss の Polygon はへこみを塗れないため
triangulate :: [Point] -> [(Point, Point, Point)]
triangulate pts0 = go (orient pts0) []
  where
    area ps = sum (zipWith (\(a, b) (c, d) -> a * d - c * b) ps (drop 1 ps ++ take 1 ps))
    orient ps = if area ps < 0 then reverse ps else ps
    cross' (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    inside a b c p = p /= a && p /= b && p /= c && cross' a b p >= 0 && cross' b c p >= 0 && cross' c a p >= 0
    rotate1 ps = drop 1 ps ++ take 1 ps
    -- 耳を探して切り取る。見つからないときは（輪郭が自分と交わる所など）そのまま 1 つ切って先へ進む
    go ps acc
      | length ps < 3 = acc
      | otherwise =
          let n = length ps
              search k qs
                | k >= n = Nothing
                | isEar' qs = Just qs
                | otherwise = search (k + 1) (rotate1 qs)
              -- 先頭の 3 点が「耳」か
              isEar' qs = case qs of
                (a : b : c : rest) -> cross' a b c > 1e-7 && not (any (inside a b c) rest)
                _ -> False
          in case search 0 ps of
               Just (a : b : c : rest) -> go (rotate1 (a : c : rest)) ((a, b, c) : acc)
               _ -> case ps of
                 (a : b : c : rest) -> go (a : c : rest) (if abs (cross' a b c) > 1e-7 then (a, b, c) : acc else acc)
                 _ -> acc

-- ★ 投影の式：(経度 λ, 緯度 φ)（ラジアン）→ 平面 (x, y)
projName, projNote :: Int -> String
projName 2 = "Equirectangular"
projName 3 = "Mercator"
projName 4 = "Stereographic"
projName 5 = "Lambert cylindrical equal-area"
projName _ = "Mollweide"
projNote 2 = "Lat/lon form an even grid. Stretched sideways at high latitudes. Neither conformal nor equal-area"
projNote 3 = "Conformal (keeps angles and small shapes): good for navigation. Area x4 at lat 60, the pole is at infinity"
projNote 4 = "The chart of 01. Conformal. Circles stay circles (bigger farther from the center)"
projNote 5 = "Equal-area (keeps area ratios), but high latitudes are squashed vertically"
projNote _ = "An equal-area ellipse map. Shapes distort near the edges"

projF :: Int -> Float -> Float -> Point
projF 2 l p = (l, p)
projF 3 l p = (l, log (tan (pi / 4 + clampF (-1.4) 1.4 p / 2)))
projF 4 l p = let r = 2 * tan ((pi / 2 - p) / 2) in (r * sin l, -r * cos l)
projF 5 l p = (l, sin p)
projF _ l p = let t = mollTheta p in (2 * sqrt 2 / pi * l * cos t, sqrt 2 * sin t)

-- モルワイデの補助角 θ：2θ + sin 2θ = π sin φ をニュートン法で解く
mollTheta :: Float -> Float
mollTheta p0 = realToFrac (go (12 :: Int) p)
  where
    p = realToFrac p0 :: Double
    go 0 t = t
    go i t =
      let f = 2 * t + sin (2 * t) - pi * sin p
          d = 2 + 2 * cos (2 * t)
      in if abs d < 1e-9 then t else go (i - 1) (t - f / d)

extent :: Int -> Point
extent 2 = (pi, pi / 2)
extent 3 = (pi, 2.45)
extent 4 = (3.6, 3.6)
extent 5 = (pi, 1)
extent _ = (2.83, 1.42)

-- 画面への変換（gloss の座標と、手前かどうか）。地球儀は正射影（裏側は縁へ押しつける）
toScreen :: Env -> World -> Float -> Float -> (Point, Bool)
toScreen env w l p
  | wMode w == 1 =
      let x = cos p * cos l
          y = cos p * sin l
          z = sin p
          c = cos (wYaw w)
          s = sin (wYaw w)
          x1 = c * x + s * y
          y1 = -s * x + c * y
          t = wTilt w
          up = y1 * sin t + z * cos t
          depth = -y1 * cos t + z * sin t
          r = min (envW env) (envH env) * 0.36
          m0 = sqrt (x1 * x1 + up * up)
          m = if m0 == 0 then 1 else m0
          (x2, u2) = if depth < 0 then (x1 / m, up / m) else (x1, up)
      in (px env (envW env / 2 + x2 * r) (envH env * 0.54 - u2 * r), depth >= 0)
  | otherwise =
      let (qx, qy) = projF (wMode w) l p
          (ex, ey) = extent (wMode w)
          k = min (envW env * 0.46 / ex) (envH env * 0.36 / ey)
      in (px env (envW env / 2 + qx * k) (envH env * 0.54 - qy * k), True)

-- 地図の絵（文字以外）をつくる
mapPicture :: Env -> World -> Picture
mapPicture env w =
  let mode = wMode w
      wd = envW env
      globe = if mode == 1
        then at (px env (wd / 2) (envH env * 0.54)) (Color (rgb255 30 70 120) (disc (min wd (envH env) * 0.36)))
        else Blank
      grid = if wGrid w
        then Color (rgba255 180 200 255 70) (Pictures (map Line (concat
          ( [ polyline env w (\t -> ((t - 0.5) * tau, rad la)) | la <- [-75, -60 .. 75] ]
         ++ [ polyline env w (\t -> (rad lo, (t - 0.5) * (pi - 0.02))) | lo <- [-180, -150 .. 180] ] ))))
        else Blank
      -- 陸：三角形で塗り、輪郭を線で描く
      limP p | mode == 3 = clampF (-1.4) 1.4 p
             | mode == 4 = max (rad (-55)) p
             | otherwise = p
      skipLand o = mode == 4 && all (\(_, p) -> p < rad (-55)) o
      shown = filter (not . skipLand . lOutline) land
      vert (l, p) = toScreen env w l (limP p)
      tri (a, b, c) =
        let (pa, fa) = vert a
            (pb, fb) = vert b
            (pc, fc) = vert c
        in [Polygon [pa, pb, pc] | fa || fb || fc]    -- 全部裏側の三角形は描かない
      fills = Color (rgb255 110 170 100) (Pictures (concatMap (concatMap tri . lTris) shown))
      outline o =
        let vs = map vert o
            ok = if mode == 1 then frontRuns vs else [map fst vs ++ take 1 (map fst vs)]
        in map Line ok
      outlines = Color (rgb255 40 90 50) (Pictures (concatMap (outline . lOutline) shown))
      -- ティソーの指示楕円
      r = rad tissotR
      tissots = if wTissot w
        then [ Pictures [ Color (rgba255 255 120 90 90) (Polygon spts), Color (rgb255 255 120 90) (thickPath 1.5 (spts ++ take 1 spts)) ]
             | la <- [-60, -30 .. 60], lo <- [-150, -120 .. 180]
             , not (mode == 4 && la < -30)
             , let c = (rad lo, rad la)
                   pts = [ toScreen env w ql qp | i <- [0 .. 35 :: Int], let (ql, qp) = offset c r (fromIntegral i / 36 * tau) ]
                   spts = map fst pts
             , all snd pts
             , mode == 1 || farOk wd spts ]
        else []
  in Pictures ([globe, grid, fills, outlines] ++ tissots)

-- 反対側の点が画面の半分以上離れていたら（地図の端をまたいだら）描かない
farOk :: Float -> [Point] -> Bool
farOk wd ps = case (ps, drop 18 ps) of
  ((a, _) : _, (b, _) : _) -> abs (a - b) <= wd * 0.5
  _ -> True

frontRuns :: [(Point, Bool)] -> [[Point]]
frontRuns = go []
  where
    flush cur = [cur | length cur >= 2]
    go cur [] = flush cur
    go cur ((q, f) : rest)
      | f = go (q : cur) rest
      | otherwise = flush cur ++ go [] rest

polyline :: Env -> World -> (Float -> (Float, Float)) -> [[Point]]
polyline env w f = go [] [f (fromIntegral i / 120) | i <- [0 .. 120 :: Int]]
  where
    mode = wMode w
    flush cur = [cur | length cur >= 2]
    go cur [] = flush cur
    go cur ((l, p0) : rest)
      | mode == 4 && p0 < rad (-40) = flush cur ++ go [] rest
      | otherwise =
          let p = if mode == 3 then clampF (-1.4) 1.4 p0 else p0
              (s@(sx, _), fr) = toScreen env w l p
              jump = case cur of
                ((lx, _) : _) -> abs (sx - lx) > envW env * 0.4
                [] -> False
          in if not fr || jump then flush cur ++ go [] rest else go (s : cur) rest

-- 中心 c（経度, 緯度）から角度 r、方位 az の点（球面上）
offset :: (Float, Float) -> Float -> Float -> (Float, Float)
offset (l, p) r az =
  let p2 = asin (clampF (-1) 1 (sin p * cos r + cos p * sin r * cos az))
      l2 = l + atan2 (sin az * sin r * cos p) (cos r - sin p * sin p2)
  in (l2, p2)

refresh :: Env -> World -> World
refresh env w = w { wMap = mapPicture env w }

draw :: Env -> World -> Picture
draw env w =
  let mode = wMode w
      hg = envH env
  in Pictures
     [ wMap w
     , txt env 20 28 16 white "1: globe  2: equirect  3: Mercator  4: stereographic  5: Lambert  6: Mollweide"
     , txt env 20 50 16 white ("T: Tissot circles   G: grid   S: save" ++ (if mode == 1 then "   Drag: turn" else ""))
     , txt env 20 74 14 (gray 0.67) ("The red circles are all the same size on Earth (radius " ++ nf 0 tissotR ++ " deg). Change of size or shape on the map = distortion")
     , txt env 20 (hg - 56) 19 (rgb255 255 220 120) (if mode == 1 then "Globe (no distortion, but not flat)" else projName mode)
     , txt env 20 (hg - 28) 15 (gray 0.86)
         (if mode == 1 then "Sphere K = 1/R^2, paper K = 0. Different curvature: no map keeps all of length, angle and area (05, 08)"
          else projNote mode)
     ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wLast = p }
  EventMotion p@(x, y) ->
    let (lx, ly) = wLast w
        w1 = w { wLast = p }
    in pure $ if envDown env && wMode w == 1
         then refresh env w1 { wYaw = wYaw w - (x - lx) * 0.01, wTilt = clampF (-1.4) 1.4 (wTilt w - (y - ly) * 0.01) }
         else w1
  EventResize _ -> pure (refresh env w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '6' -> pure (refresh env w { wMode = fromEnum c - fromEnum '0' })
    | toLower c == 't' -> pure (refresh env w { wTissot = not (wTissot w) })
    | toLower c == 'g' -> pure (refresh env w { wGrid = not (wGrid w) })
    | toLower c == 's' -> saveSvg "map_projection" env (draw env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. メルカトルとランベルトでグリーンランドとアフリカの大きさを比べよう（本当はアフリカが約 14 倍）
-- 2. 3 と 4 は等角：赤い円が円のまま。5 と 6 は正積：円がつぶれるが面積は同じ。両方を満たす地図はない
-- 3. projF に自分の投影を追加しよう。例：サンソン図法 (λ cos φ, φ)（正積）
-- 4. continents の日本の輪郭を細かくしてみよう。高緯度の北海道はメルカトルでどれくらい大きくなる？

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
