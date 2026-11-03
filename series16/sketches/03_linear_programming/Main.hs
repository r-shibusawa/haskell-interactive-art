-- ============================================
-- 03 線形計画法：制約の中でいちばん得をする点
-- 「aᵢ₁x₁ + aᵢ₂x₂ ≤ bᵢ」という直線の不等式をいくつか満たす点（実行可能領域・凸多角形）の中で、
-- 目的関数 c₁x₁ + c₂x₂ を最大にする点を探す。答えは必ず多角形の「角」にある
-- ドラッグ：直線の白い丸（制約を動かす）・矢印の先（目的の向き） / F：マウスに近い制約の向きを反転
-- N：制約を 1 本追加 / D：マウスに近い制約を消す / 1：お手本 2：パン屋 3：非有界 4：実行不能
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, toLower)
import Numeric (showFFloat)

range :: Double
range = 10        -- ★ 画面に表示する範囲（0〜range）

sweepSpeed :: Float
sweepSpeed = 0.01 -- ★ 等高線が動く速さ

type V2 = (Double, Double)

-- 制約 a1 x1 + a2 x2 <= b
data Con = Con { a1, a2, cb :: !Double }

data Status = Optimal | Unbounded | Infeasible deriving Eq

data Drag
  = DragObj
  | DragCon !Int !V2 !Double !V2 -- 制約の番号、動かさない丸、法線の長さ、法線の向き

data World = World
  { wCons   :: ![Con]
  , wObj    :: !V2
  , wDrag   :: !(Maybe Drag)
  , wSweep  :: !Float
  , wPoly   :: ![V2]
  , wBest   :: !(Maybe (Double, Double, Double))
  , wStatus :: !Status
  }

main :: IO ()
main = runSketch (sketch "16-03 Linear Programming" (preset 1 (World [] (1, 1) Nothing 0 [] Nothing Infeasible)))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure w { wSweep = if wStatus w /= Infeasible then wrapF 1.2 (wSweep w + sweepSpeed) else wSweep w }
  , skDraw  = drawW
  , skEvent = onEvent
  }

preset :: Int -> World -> World
preset k w = case k of
  1 -> set [Con 3 (-1) 9, Con 1 1 7, Con (-1) 2 8] (2, 1)   -- 最適は角 (4, 3)、z = 11
  2 -> set [Con 2 1 10, Con 1 2 8, Con 0 1 3.5] (3, 2)
  3 -> set [Con (-1) 1 2, Con 1 (-2) 2] (1, 1)
  _ -> set [Con 1 1 2, Con (-1) (-1) (-5)] (1, 1)
  where set cs o = solve w { wCons = cs, wObj = o }

-- 大きな箱を半平面で切り取って実行可能領域を作り、角を全部調べる
solve :: World -> World
solve w =
  let big = 60
      poly0 = [(0, 0), (big, 0), (big, big), (0, big)]
      polyN = foldl (\p c -> clipHalf p (a1 c) (a2 c) (cb c)) poly0 (wCons w)
      (ox, oy) = wObj w
      pick b (x, y) =
        let z = ox * x + oy * y
        in case b of
             Just (_, _, bz) | z <= bz + 1e-9 -> b
             _ -> Just (x, y, z)
      best = foldl pick Nothing polyN
  in if length polyN < 3
       then w { wPoly = polyN, wBest = Nothing, wStatus = Infeasible }
       else w { wPoly = polyN, wBest = best
              , wStatus = case best of
                  Just (x, y, _) | x > big - 1e-6 || y > big - 1e-6 -> Unbounded
                  _ -> Optimal }

clipHalf :: [V2] -> Double -> Double -> Double -> [V2]
clipHalf ps u v b = concat (zipWith edge ps (drop 1 ps ++ take 1 ps))
  where
    edge p@(px, py) (qx, qy) =
      let fp = u * px + v * py - b
          fq = u * qx + v * qy - b
          keep = [p | fp <= 1e-9]
          cross
            | (fp < -1e-9 && fq > 1e-9) || (fp > 1e-9 && fq < -1e-9) =
                let t = fp / (fp - fq) in [(px + t * (qx - px), py + t * (qy - py))]
            | otherwise = []
      in keep ++ cross

-- 図の位置と大きさ
regionS :: Env -> Float
regionS env = min (envW env * 0.55) (envH env - 140)

rx0, ry0 :: Float
rx0 = 40
ry0 = 110

toS :: Env -> V2 -> (Float, Float)
toS env (x, y) =
  let s = regionS env
  in (rx0 + realToFrac (x / range) * s, ry0 + s - realToFrac (y / range) * s)

toW :: Env -> (Float, Float) -> V2
toW env (x, y) =
  let s = regionS env
  in (realToFrac ((x - rx0) / s) * range, realToFrac ((ry0 + s - y) / s) * range)

-- 制約の直線が画面に入る部分の 1/4 と 3/4 の点（ドラッグ用の丸）
handles :: Con -> Maybe (V2, V2)
handles c =
  let borders = [(0, 0, 1, 0), (0, range, 1, 0), (0, 0, 0, 1), (range, 0, 0, 1)]
      hit (px, py, dx, dy) =
        let den = a1 c * dx + a2 c * dy
            t = (cb c - a1 c * px - a2 c * py) / den
            qx = px + dx * t
            qy = py + dy * t
        in [ (qx, qy) | abs den >= 1e-9, qx >= -1e-6, qx <= range + 1e-6, qy >= -1e-6, qy <= range + 1e-6 ]
      pts = concatMap hit borders
  in case pts of
       (p : _ : _) ->
         let q = last pts
         in Just (lerpP p q 0.25, lerpP p q 0.75)
       _ -> Nothing

lerpP :: V2 -> V2 -> Double -> V2
lerpP (a, b) (c, d) t = (a + (c - a) * t, b + (d - b) * t)

levelLine :: Env -> World -> Double -> Color -> Float -> Picture
levelLine env w z col wgt =
  let (a, b) = wObj w
      (p, q)
        | abs b > abs a = ((-range, (z + a * range) / b), (2 * range, (z - 2 * a * range) / b))
        | otherwise = (((z + b * range) / a, -range), ((z - 2 * b * range) / a, 2 * range))
  in seg env col wgt (toS env p) (toS env q)

drawW :: Env -> World -> Picture
drawW env w =
  let s = regionS env
      cons = wCons w
      status = wStatus w
      (ox, oy) = wObj w
      gridC = rgb255 45 38 62
      grid = [ let (x, _) = toS env (k, 0)
                   (_, y) = toS env (0, k)
               in Pictures [seg env gridC 1 (x, ry0) (x, ry0 + s), seg env gridC 1 (rx0, y) (rx0 + s, y)]
             | k <- [0 .. range] ]
      polyPic
        | length (wPoly w) >= 3 =
            let q = map (pt env . toS env) (wPoly w)
            in Pictures [ Color (rgba255 78 225 190 60) (Polygon q)
                        , Color mint (thickPath 2 (q ++ take 1 q)) ]
        | otherwise = Blank
      -- 等高線 c·x = z が広がっていく
      zmax = case (status, wBest w) of
        (Optimal, Just (_, _, z)) -> z
        _ -> (ox + oy) * range * 1.5
      levels
        | status == Infeasible = []
        | otherwise =
            levelLine env w (realToFrac (min (wSweep w) 1) * zmax) (rgba255 255 200 90 200) 3
              : [levelLine env w (zmax * k / 4) (rgba255 255 200 90 40) 1 | k <- [1 .. 4]]
      conPic (i, c) = case handles c of
        Nothing -> Blank
        Just (h0, h1) ->
          let s1 = toS env (lerpP h0 h1 (-2))
              s2 = toS env (lerpP h0 h1 3)
              nl = sqrt (a1 c * a1 c + a2 c * a2 c)
              nx = realToFrac (if nl > 0 then a1 c / nl else 0) :: Float
              ny = realToFrac (if nl > 0 then negate (a2 c) / nl else 0) :: Float
              -- 「ダメな側」に短い斜線
              hatch = [ let (mx, my) = toS env (lerpP h0 h1 (t * 3 - 1))
                        in seg env (rgba255 255 95 109 120) 2 (mx, my) (mx + nx * 10, my + ny * 10)
                      | k <- [0 .. 12 :: Int], let t = fromIntegral k * 0.08 ]
              (m1x, m1y) = toS env h0
              (m2x, m2y) = toS env h1
          in Pictures
               [ seg env coral 2.5 s1 s2
               , Pictures hatch
               , dot env white m1x m1y 14
               , dot env white m2x m2y 14
               , txt env (m2x + 8) (m2y - 6) 14 (rgb255 255 150 160) ("(" ++ show (i :: Int) ++ ")")
               ]
      bestPic = case (status, wBest w) of
        (Optimal, Just (x, y, _)) ->
          let (sxx, syy) = toS env (x, y)
          in Pictures [dot env amber sxx syy 22, dot env bgC sxx syy 8]
        _ -> Blank
      -- 枠の外を背景色で隠す（p5.js の clip の代わり）
      mask = maskOut env bgC rx0 ry0 s s
      (ox0, oy0) = toS env (0, 0)
      tipX = ox0 + realToFrac ox * 30
      tipY = oy0 - realToFrac oy * 30
      px = rx0 + s + 30
      pw = envW env - px - 30
      f = fmtC
      conLine (i, c) = "(" ++ show (i :: Int) ++ ")  " ++ f (a1 c) ++ "x1 " ++ sgn (a2 c) ++ " " ++ f (abs (a2 c)) ++ "x2 <= " ++ f (cb c)
      slackLine (i, c) = "s" ++ show (i :: Int) ++ " = " ++ f (cb c) ++ " " ++ sgn (negate (a1 c)) ++ " " ++ f (abs (a1 c)) ++ "x1 "
                           ++ sgn (negate (a2 c)) ++ " " ++ f (abs (a2 c)) ++ "x2"
      numbered = zip [1 ..] cons
      n = fromIntegral (length cons) :: Float
      yStd = 172 + n * 24
      ySl = yStd + 40
      yRes = ySl + 30 + n * 24 + 24
      result = case (status, wBest w) of
        (Optimal, Just (x, y, z)) -> para env px yRes 18 mint pw ("Optimal x = (" ++ f x ++ ", " ++ f y ++ ")\nMaximum z = " ++ f z)
        (Unbounded, _) -> para env px yRes 18 amber pw "Unbounded: you can go on forever along the arrow, so there is no maximum"
        (Infeasible, _) -> para env px yRes 18 (rgb255 255 100 100) pw "Infeasible: no point satisfies all the constraints"
        _ -> Blank
  in Pictures
       [ box env plotC rx0 ry0 s s
       , Pictures grid
       , polyPic
       , Pictures levels
       , Pictures (map conPic numbered)
       , bestPic
       , mask
       , seg env amber 4 (ox0, oy0) (tipX, tipY)
       , dot env amber tipX tipY 16
       , txt env 30 50 26 (g8 235) "Linear programming: the best point inside the constraints"
       , txt env 30 80 15 (g8 150) "Dots: move line  Arrow: goal  F: flip  N: add  D: del  1: sample  2: bakery  3: unbounded  4: infeasible"
       , txtT env px 115 18 (g8 235) "Standard form"
       , txtT env px 145 16 amber ("Maximize  " ++ f ox ++ "x1 " ++ sgn oy ++ " " ++ f (abs oy) ++ "x2")
       , Pictures [ txtT env (px + 20) (172 + fromIntegral i * 24) 16 (g8 235) (conLine (i + 1, c)) | (i, c) <- zip [0 :: Int ..] cons ]
       , txtT env (px + 20) yStd 16 (g8 235) "x1, x2 >= 0"
       , txtT env px ySl 18 (g8 235) "Slack form (add s_i >= 0)"
       , Pictures [ txtT env (px + 20) (ySl + 30 + fromIntegral i * 24) 16 (g8 200) (slackLine (i + 1, c)) | (i, c) <- zip [0 :: Int ..] cons ]
       , result
       , footer env px pw 14 (g8 150)
           "Slide the yellow level line c.x = z along the arrow: the last point touched is the answer, so it is a corner. Simplex (04) walks corner to corner. Shortest paths and max flow can be LPs too."
       ]

nearestCons :: Env -> World -> Int
nearestCons env w =
  let (x, y) = toW env (mouseP env)
      dOf c = abs (a1 c * x + a2 c * y - cb c) / sqrt (a1 c * a1 c + a2 c * a2 c)
      pick (bi, bd) (i, c) = let d = dOf c in if d < bd then (i, d) else (bi, bd)
  in fst (foldl pick (0, 1e9) (zip [0 ..] (wCons w)))

r10 :: Double -> Double
r10 v = fromIntegral (floor (v * 10 + 0.5) :: Int) / 10

r2 :: Float -> Double
r2 v = fromIntegral (floor (v * 2 + 0.5) :: Int) / 2

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        (ox0, oy0) = toS env (0, 0)
        (cx, cy) = wObj w
        onTip = dist2 m (ox0 + realToFrac cx * 30, oy0 - realToFrac cy * 30) < 14
        hitCon (i, c) = case handles c of
          Nothing -> []
          Just (h0, h1) ->
            [ DragCon i other (sqrt (a1 c * a1 c + a2 c * a2 c)) (a1 c, a2 c)
            | (h, other) <- [(h0, h1), (h1, h0)], dist2 m (toS env h) < 12 ]
    in pure $
         if onTip then w { wDrag = Just DragObj }
         else case concatMap hitCon (zip [0 ..] (wCons w)) of
           (d : _) -> w { wDrag = Just d }
           [] -> w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> pure $ case wDrag w of
    Nothing -> w
    Just DragObj ->
      let (ox0, oy0) = toS env (0, 0)
          (mx, my) = mouseP env
          o = (r2 ((mx - ox0) / 30), r2 ((oy0 - my) / 30))
      in solve w { wObj = if o == (0, 0) then (0.5, 0) else o }
    Just (DragCon i q@(qx, qy) len (n0x, n0y)) ->
      let (px, py) = toW env (mouseP env)
          dx = qx - px
          dy = qy - py
          ml = sqrt (dx * dx + dy * dy)
      in if ml < 0.3 then w else
         let (nx, ny) = if negate dy * n0x + dx * n0y < 0 then (dy, negate dx) else (negate dy, dx)
             s = len / sqrt (nx * nx + ny * ny)
             na = r10 (nx * s)
             nb = r10 (ny * s)
             con = Con na nb (r10 (na * qx + nb * qy))
             cons' = [if j == i then con else c | (j, c) <- zip [0 ..] (wCons w)]
         in solve w { wCons = cons', wDrag = Just (DragCon i q len (na, nb)) }
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    c | c `elem` "1234" -> pure (preset (digitToInt c) w { wDrag = Nothing })
    'f' | not (null (wCons w)) ->
      let i = nearestCons env w
      in pure (solve w { wDrag = Nothing, wCons = [ if j == i then Con (negate (a1 k)) (negate (a2 k)) (negate (cb k)) else k | (j, k) <- zip [0 ..] (wCons w)] })
    'n' | length (wCons w) < 7 -> do
      ang <- randomRIO (0, 2 * pi) :: IO Double
      let u = fromIntegral (floor (cos ang * 30 + 0.5) :: Int) / 10
          v = fromIntegral (floor (sin ang * 30 + 0.5) :: Int) / 10
      pure (solve w { wCons = wCons w ++ [Con u v (r10 (u * 4 + v * 4 + 6))] })
    'd' | not (null (wCons w)) ->
      let i = nearestCons env w
      in pure (solve w { wDrag = Nothing, wCons = [k | (j, k) <- zip [0 ..] (wCons w), j /= i] })
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 矢印をゆっくり回すと、答えの角がパッと隣の角へ飛ぶ。辺と矢印が直角のときは辺全体が答え
-- 2. 2 のパン屋で「バターをもう 1 増やす」（(2) の b を増やす）と利益はいくら増える？（05 の双対へ）
-- 3. 4（実行不能）で F を押して制約の向きを反転すると、領域が現れる

-- ---------- 道具 ----------
fmtC :: Double -> String
fmtC v
  | abs (v - fromIntegral r) < 1e-9 = show r
  | otherwise = showFFloat (Just 1) v ""
  where r = floor (v + 0.5) :: Int

sgn :: Double -> String
sgn v = if v < 0 then "-" else "+"

-- (x, y, w, h) の外側を塗りつぶす（描いたものを枠で切り取る）
maskOut :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
maskOut env c x y w h =
  let far = 3000
  in Pictures
       [ box env c (x - far) (y - far) far (h + 2 * far)
       , box env c (x + w) (y - far) far (h + 2 * far)
       , box env c x (y - far) w far
       , box env c x (y + h) w far ]

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v
