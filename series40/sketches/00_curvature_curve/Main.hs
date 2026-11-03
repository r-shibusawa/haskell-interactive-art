-- ============================================
-- 40-00 平面曲線の曲率と接触円
-- 曲線の各点での「曲がり具合」が曲率 κ。κ = 1/r（r は曲線にいちばんぴったり合う円 = 接触円の半径）
--   x(t), y(t) の 1 階・2 階微分から κ = (x'y'' − y'x'') / (x'² + y'²)^{3/2}（差分で計算）
--   進む向きに対して左に曲がると +、右に曲がると −。まっすぐなら 0（接触円は無限に大きい）
-- 操作：ドラッグ = 自分で曲線を描く / P = プリセット / スペース = 一時停止 / ←→ = 速さ
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

gridH :: Float
gridH = 6                  -- ★ 曲線を等間隔に打ち直すときの点の間隔（px）

speed0 :: Float
speed0 = 1.2               -- ★ 曲線の上を走る速さ（1 フレームに進む点の数）

maxR :: Float
maxR = 1500                -- ★ これより大きい接触円は「ほぼ直線」とみなして描かない

names :: [String]
names = ["Ellipse", "Limacon (inner loop)", "Lemniscate", "Cardioid", "Sine wave (open)"]

data World = World
  { wPts     :: !(U.Vector Point)   -- 等間隔の点列（p5.js の座標）
  , wKappa   :: !(U.Vector Float)   -- 各点の曲率
  , wClosed  :: !Bool
  , wRaw     :: ![Point]            -- 描いている途中の線（新しい順）
  , wDrawing :: !Bool
  , wHead    :: !Float              -- 走る点の位置
  , wPaused  :: !Bool
  , wPreset  :: !Int
  , wSpeed   :: !Float
  }

main :: IO ()
main = runSketch (sketch "40-00 Curvature and osculating circle" world0)
  { skBack  = hsb 150 25 10
  , skStart = \env -> pure (setPreset env 0 world0)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World U.empty U.empty True [] False 0 False 0 speed0

-- ★ プリセットの曲線（媒介変数 t で定義）
presetCurve :: Env -> Int -> Float -> Point
presetCurve env no t =
  let cx = envW env * 0.5
      cy = envH env * 0.42
      s = min (envW env) (envH env) * 0.19
  in case no of
       0 -> (cx + 1.7 * s * cos t, cy + 0.9 * s * sin t)
       1 -> let r = 0.55 * s * (1 + 2 * cos t) in (cx - 0.5 * s + r * cos t, cy + r * sin t)
       2 -> let d = 1 + sin t ^ (2 :: Int) in (cx + 1.8 * s * cos t / d, cy + 1.8 * s * sin t * cos t / d)
       3 -> let r = 0.9 * s * (1 - cos t) in (cx + 0.6 * s + r * cos t, cy + r * sin t)
       _ -> (cx + (t / tau - 0.5) * envW env * 0.8, cy + 0.8 * s * sin (2 * t))

setPreset :: Env -> Int -> World -> World
setPreset env no w =
  let closed = no /= 4
      src = [presetCurve env no (fromIntegral i / 400 * tau) | i <- [0 .. 400 :: Int]]
      pts = U.fromList (resample gridH closed src)
  in w { wPreset = no, wClosed = closed, wPts = pts, wKappa = curvatureOf gridH closed pts, wHead = 0 }

step :: Env -> World -> World
step env w =
  let n = max (U.length (wPts w)) 1
      advanced = if wPaused w || wDrawing w then wHead w else wrapF (fromIntegral n) (wHead w + wSpeed w)
      s1 = if isDown env (SpecialKey KeyLeft) then max 0.2 (wSpeed w - 0.03) else wSpeed w
      s2 = if isDown env (SpecialKey KeyRight) then min 6 (s1 + 0.03) else s1
  in w { wHead = advanced, wSpeed = s2 }

draw :: Env -> World -> Picture
draw env w =
  let pts = wPts w
      kap = wKappa w
      n = U.length pts
      kmax = U.foldl' (\m k -> max m (abs k)) 1e-6 kap
      -- 曲線（曲率で色をつける：左曲がり = 赤、右曲がり = 青、まっすぐ = 白）
      curve = [ seg2 env (hsb (if k > 0 then 5 else 215) (min 100 (abs k * 110)) 100) 3 (pts U.! i) (pts U.! ((i + 1) `mod` n))
              | i <- [0 .. n - (if wClosed w then 1 else 2)], let k = kap U.! i / kmax ]
      rawLine = [path2 env (hsba 0 0 100 0.6) 2 (wRaw w) | wDrawing w]
      cur = if n == 0 then 0 else (floor (wHead w) :: Int) `mod` n
  in Pictures $
       curve ++ rawLine ++ [runner env w cur | n > 3 && not (wDrawing w)] ++ [graph env w kmax cur] ++
       [ txt env 20 40 18 white ("P: preset (" ++ nameOf (wPreset w) ++ ")   drag: draw your own")
       , txt env 20 64 16 white ("Space: " ++ (if wPaused w then "resume" else "pause") ++ "   <- ->: speed " ++ nf 1 (wSpeed w))
       , txt env 20 88 14 grayText "k = (x'y'' - y'x'') / (x'^2 + y'^2)^(3/2)"
       , txt env 20 108 14 grayText "red = turns left (k > 0)   blue = turns right (k < 0)   yellow = osculating circle (radius 1/|k|)"
       ]

grayText :: Color
grayText = hsb 0 0 65

nameOf :: Int -> String
nameOf i = case drop i names of
  (s : _) -> s
  [] -> ""

-- 走る点と接触円
runner :: Env -> World -> Int -> Picture
runner env w i =
  let pts = wPts w
      n = U.length pts
      p@(px, py) = pts U.! i
      k = wKappa w U.! i
      (nx, ny) = pts U.! ((i + 1) `mod` n)
      (vx, vy) = pts U.! ((i - 1 + n) `mod` n)
      tl0 = norm2 (nx - vx, ny - vy)
      tl = if tl0 == 0 then 1 else tl0
      tx = (nx - vx) / tl
      ty = (ny - vy) / tl
      (lx, ly) = (ty, negate tx)              -- 進む向きの左側の法線（p5.js の画面は y が下向き）
      r = 1 / max (abs k) 1e-9
      sgn = if k > 0 then 1 else -1
      c = (px + lx * r * sgn, py + ly * r * sgn)
      circlePart
        | r < maxR = [ ringAt env (hsba 45 90 100 0.7) r 2 c
                     , seg2 env (hsba 45 90 100 0.5) 1 p c
                     , dotAt env (hsb 45 90 100) 3.5 c ]
        | otherwise = [seg2 env (hsba 0 0 100 0.6) 1 (px - tx * 200, py - ty * 200) (px + tx * 200, py + ty * 200)]
  in Pictures $ circlePart ++
       [ seg2 env (hsba 0 0 100 0.8) 3 p (px + tx * 50, py + ty * 50)     -- 接線ベクトル
       , dotAt env white 6 p
       , txt env 20 (envH env - 185) 18 white
           ("k = " ++ nf 4 k ++ " /px   radius r = 1/|k| = " ++ (if r < maxR then nf 1 r ++ " px" else "inf (nearly straight)"))
       ]

-- 下のグラフ：横 = 曲線の上の位置、縦 = κ
graph :: Env -> World -> Float -> Int -> Picture
graph env w kmax cur =
  let gx = 20
      gy = envH env - 160
      gw = envW env - 40
      gh = 130
      kap = wKappa w
      m = U.length kap
      fm = fromIntegral (max m 1) :: Float
      ptAt i = (gx + fromIntegral i / fm * gw, gy + gh / 2 - kap U.! i / kmax * gh * 0.45)
      x = gx + fromIntegral cur / fm * gw
  in Pictures $
       [ box env gx gy gw gh (hsba 0 0 100 0.06)
       , seg2 env (hsba 0 0 100 0.25) 1 (gx, gy + gh / 2) (gx + gw, gy + gh / 2) ] ++
       [ seg2 env (hsb (if kap U.! i > 0 then 5 else 215) 70 100) 2 (ptAt i) (ptAt (i + 1)) | i <- [0 .. m - 2] ] ++
       [ seg2 env (hsb 45 90 100) 2 (x, gy) (x, gy + gh)
       , txt env (gx + 8) (gy + 18) 14 (hsb 0 0 60) ("graph of k (max |k| = " ++ nf 4 kmax ++ ")")
       , txt env (gx + gw - 20) (gy + 18) 14 (hsb 0 0 60) "+"
       , txt env (gx + gw - 20) (gy + gh - 8) 14 (hsb 0 0 60) "-"
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | snd (fromG env pos) < envH env - 170 -> w { wDrawing = True, wRaw = [fromG env pos] }
  EventKey (MouseButton LeftButton) Up _ _ | wDrawing w -> finishDrawing w
  EventMotion pos | wDrawing w -> case wRaw w of
    (l : _) | dist2 (fromG env pos) l > 3 -> w { wRaw = take 4000 (fromG env pos : wRaw w) }
    _ -> w
  EventKey (Char c) Down _ _ | c == 'p' || c == 'P' -> setPreset env ((wPreset w + 1) `mod` length names) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- 描き終わったら：なめらかにして、打ち直して、曲率を計算する
finishDrawing :: World -> World
finishDrawing w0 =
  let w = w0 { wDrawing = False }
      raw = reverse (wRaw w)
  in case (raw, wRaw w) of
       (a : _, l : _) | length raw >= 12 ->
         let closed = dist2 a l < 60
             pts = U.fromList (smoothPoly 2 closed (resample gridH closed (smoothPoly 3 closed raw)))
         in w { wClosed = closed, wPts = pts, wKappa = curvatureOf gridH closed pts, wHead = 0 }
       _ -> w

-- ---------- やってみよう ----------
-- 1. 楕円の κ が最大になるのはどこ？（長軸の端）。接触円の大きさが場所で変わるのを見よう
-- 2. リマソンの内側の輪と外側で κ の符号は同じ？ 走る点が「左に曲がる」か「右に曲がる」かで考える
-- 3. 自分でまっすぐな線を描くと κ ≈ 0、接触円はどうなる？ ギザギザに描くとグラフはどうなる？
-- 4. gridH（点の間隔）を 3 や 12 にすると κ のグラフはどう変わる？（差分の精度と、ノイズの関係）

-- ==================================================
-- ここから下は「曲線の道具」です（等間隔に打ち直す・なめらかにする・曲率を差分で求める）
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | 折れ線を間隔 h の点列に打ち直す
resample :: Float -> Bool -> [Point] -> [Point]
resample _ _ [] = []
resample h isClosed src@(p0 : _) = trimEnd (p0 : walk 0 (zip src (drop 1 src ++ [p0 | isClosed])))
  where
    walk _ [] = []
    walk acc (((ax, ay), (bx, by)) : rest)
      | d < 1e-9 = walk acc rest
      | otherwise = inner acc 0
      where
        d = sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
        inner a t
          | a + (d - t) >= h =
              let t' = t + (h - a)
              in (ax + (bx - ax) * (t' / d), ay + (by - ay) * (t' / d)) : inner 0 t'
          | otherwise = walk (a + d - t) rest
    -- 閉じた曲線で、最後の点が最初の点に近すぎたら捨てる
    trimEnd out = case reverse out of
      (l : more@(_ : _)) | isClosed && dist2 p0 l < h * 0.5 -> reverse more
      _ -> out

-- | 3 点平均を n 回かけてなめらかにする（開いた曲線は端を固定）
smoothPoly :: Int -> Bool -> [Point] -> [Point]
smoothPoly n isClosed p0 = U.toList (iterate once (U.fromList p0) !! max 0 n)
  where
    once q =
      let m = U.length q
      in U.generate m $ \i ->
           if not isClosed && (i == 0 || i == m - 1) then q U.! i else
           let (ax, ay) = q U.! ((i - 1 + m) `mod` m)
               (bx, by) = q U.! ((i + 1) `mod` m)
               (x, y) = q U.! i
           in ((ax + 2 * x + bx) / 4, (ay + 2 * y + by) / 4)

-- | 等間隔の点列の曲率（中心差分）。p5.js の座標は y が下向きなので符号を反転して「左曲がり = +」にする
curvatureOf :: Float -> Bool -> U.Vector Point -> U.Vector Float
curvatureOf h isClosed p = U.generate n at'
  where
    n = U.length p
    at' i =
      let im = if isClosed then (i - 1 + n) `mod` n else max (i - 1) 0
          ip = if isClosed then (i + 1) `mod` n else min (i + 1) (n - 1)
          (xm, ym) = p U.! im
          (xp, yp) = p U.! ip
          (x, y) = p U.! i
          x1 = (xp - xm) / (2 * h)
          y1 = (yp - ym) / (2 * h)
          x2 = (xp - 2 * x + xm) / (h * h)
          y2 = (yp - 2 * y + ym) / (h * h)
          s = x1 * x1 + y1 * y1
          den = s * sqrt s
      in if den > 1e-9 then negate (x1 * y2 - y1 * x2) / den else 0

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
