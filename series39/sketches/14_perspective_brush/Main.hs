-- ============================================
-- 39-14 透視図法ブラシ
-- 画面に描いた線を「地面の上の線」として受け取り（画面 → 地面の逆射影）、壁の高さぶん立ち上げて透視図にする
--   描いた線は世界座標（地面の X, Z）で覚えておくので、カメラを回すと 1 点透視 ↔ 2 点透視に同じ絵が変わる
--   地面のどの平行線も消失点に向かう。G で消失点へ向かう補助線（線路）を表示
-- 地平線より下をドラッグ = 描く / ↑↓ = 壁の高さ / ←→ = カメラを回す / 1 = 1 点透視 / 2 = 2 点透視 / G = 補助線 / C = 消す / S = 保存（SVG） / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

wallH0, eyeH, focal :: Float
wallH0 = 1.2   -- ★ 壁の高さ（m）
eyeH = 1.6     -- ★ 目の高さ（m）
focal = 650    -- ★ 焦点距離（px）

maxInk :: Int
maxInk = 1800  -- ★ 覚えておく点の数。超えると古い線から消える

-- 描いた線：地面の点（古い順）、壁の高さ、色相
data Ink = Ink { iPts :: ![Point], iH :: !Float, iHue :: !Float }

data World = World
  { wInk   :: ![Ink]        -- 描いた線（新しい順）
  , wCur   :: !(Maybe Ink)  -- 描いている最中の線（点は新しい順）
  , wWallH :: !Float
  , wYaw   :: !Float        -- カメラの回転（0 = 1 点透視）
  , wGuide :: !Bool
  , wHueI  :: !Int
  , wHold  :: !Int          -- ←→ を押し続けているフレーム数
  }

main :: IO ()
main = runSketch (sketch "39-14 Perspective brush" (World [] Nothing wallH0 0 True 0 0))
  { skBack  = hsb 30 35 11
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

cxOf, cyOf :: Env -> Float
cxOf env = envW env / 2
cyOf env = envH env * 0.45   -- 地平線の高さ

-- ★ 世界の点（地面の X, Z と高さ hgt）→ 画面。カメラは原点、目の高さ eyeH、yaw だけ回っている
toScreen :: Env -> World -> Float -> Float -> Float -> Maybe Point
toScreen env w x z hgt =
  let c = cos (wYaw w)
      s = sin (wYaw w)
      xc = c * x - s * z
      zc = s * x + c * z
      yc = eyeH - hgt
  in if zc < 0.15 then Nothing else Just (cxOf env + focal * xc / zc, cyOf env + focal * yc / zc)

-- ★ 画面の点 → 地面の点（逆射影）。地平線より上は地面に当たらない
unproject :: Env -> World -> Point -> Maybe Point
unproject env w (sx, sy) =
  let dx = (sx - cxOf env) / focal
      dy = (sy - cyOf env) / focal
      t = eyeH / dy
      xc = dx * t
      zc = t
      c = cos (wYaw w)
      s = sin (wYaw w)
  in if dy < 0.01 then Nothing else Just (c * xc + s * zc, negate s * xc + c * zc)

step :: Env -> World -> World
step env w =
  let m@(_, my) = mouseP5 env
      w1 = case (wCur w, envDown env && my > 90, unproject env w m) of
        (Just cur, True, Just g) | keep cur g -> w { wCur = Just cur { iPts = g : iPts cur } }
        _ -> w
      keep cur g = length (iPts cur) < maxInk && case iPts cur of
        (l : _) -> dist2 g l > 0.03
        [] -> True
      -- ←→ は押し続けると回り続ける
      l' = isDown env (SpecialKey KeyLeft)
      r' = isDown env (SpecialKey KeyRight)
      hold = if l' || r' then wHold w1 + 1 else 0
      turn = if hold >= 20 && hold `mod` 3 == 0 then (if l' then -0.05 else 0.05) else 0
  in w1 { wHold = hold, wYaw = wYaw w1 + turn }

draw :: Env -> World -> Picture
draw env w = view env $
  [ fillRect (hsb 220 40 20) 0 0 (envW env) cy
  , fillRect (hsb 35 40 13) 0 cy (envW env) (envH env - cy) ]
  -- 地面の格子（薄く）と消失点
  ++ concat [ [ seg3 env w 1 gridC (k, 0.5, 0) (k, 200, 0), seg3 env w 1 gridC (-200, max 0.5 k, 0) (200, max 0.5 k, 0) ] | k <- [-20, -18 .. 20] ]
  ++ [ seg 1 (hsba 45 60 100 0.5) (0, cy) (envW env, cy) ]
  ++ [ dotAt (hsb 45 90 100) v 10
     | Just v <- [toScreen env w 0 1e6 0, toScreen env w 1e6 1 0, toScreen env w (-1e6) 1 0]
     , fst v > -50, fst v < envW env + 50 ]
  -- 線：地面の線、壁（半透明）、上の線、柱
  ++ concatMap (drawInk env w) (reverse (wInk w) ++ maybe [] (\c -> [c { iPts = reverse (iPts c) }]) (wCur w))
  ++ [ txt 20 28 18 white ("Drag below the horizon: draw   Wall " ++ nf 1 (wWallH w) ++ " m (Up/Down)   Camera " ++ nf 0 (wYaw w * 180 / pi) ++ " deg (Left/Right)")
     , txt 20 50 14 (gray 0.8) ("1/2: 1-point, 2-point   G: guides " ++ (if wGuide w then "ON" else "off") ++ "   C: clear   S: save   R: reset")
     , txt 20 70 14 (gray 0.65) "Your line lies on the ground. Turn the camera: the same line is redrawn toward other VPs (yellow = axis VPs)." ]
  where
    cy = cyOf env
    gridC = hsba 45 30 70 0.14

drawInk :: Env -> World -> Ink -> [Picture]
drawInk env w (Ink ps h hue)
  | length ps < 2 = []
  | otherwise =
      [ fillQuad (hsba hue 50 80 0.35) [a, b, c, d]
      | ((x1, z1), (x2, z2)) <- pairs
      , Just a <- [toScreen env w x1 z1 0], Just b <- [toScreen env w x2 z2 0]
      , Just c <- [toScreen env w x2 z2 h], Just d <- [toScreen env w x1 z1 h] ]
      ++ concat [ [ seg3 env w 2.5 lineC (x1, z1, 0) (x2, z2, 0), seg3 env w 2.5 lineC (x1, z1, h) (x2, z2, h) ] | ((x1, z1), (x2, z2)) <- pairs ]
      ++ [ seg3 env w 1 (hsba hue 40 100 0.7) (x, z, 0) (x, z, h) | (x, z) <- every 4 ps ]
      ++ (if wGuide w then [ seg3 env w 1 (hsba 45 80 100 0.25) (x, z, 0) (x, z + 400, 0) | (x, z) <- every 6 ps ] else [])
  where
    pairs = zip ps (drop 1 ps)
    lineC = hsb hue 70 100

every :: Int -> [a] -> [a]
every _ [] = []
every n (x : xs) = x : every n (drop (n - 1) xs)

-- 世界の線分を画面に描く（途中で手前に来るものは細かく刻む）
seg3 :: Env -> World -> Float -> Color -> (Float, Float, Float) -> (Float, Float, Float) -> Picture
seg3 env w sw c (x1, z1, h1) (x2, z2, h2) =
  case (toScreen env w x1 z1 h1, toScreen env w x2 z2 h2) of
    (Just a, Just b) -> seg sw c a b
    _ ->
      let ps = [ toScreen env w (x1 + (x2 - x1) * t) (z1 + (z2 - z1) * t) (h1 + (h2 - h1) * t)
               | i <- [0 .. 20 :: Int], let t = fromIntegral i / 20 ]
      in Pictures [seg sw c a b | (Just a, Just b) <- zip ps (drop 1 ps)]

-- 点の数が maxInk を超えないように、古い線から消す
limitInk :: [Ink] -> [Ink]
limitInk = go 0
  where
    go _ [] = []
    go n (i : is) =
      let n' = n + length (iPts i)
      in if n' > maxInk then [] else i : go n' is

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m@(_, my) = mouseP5 env
    in pure $ case unproject env w m of
         Just _ | my > 90 ->
           let hues = [15, 45, 200, 140, 300] :: [Float]
           in w { wCur = Just (Ink [] (wWallH w) (hues !! (wHueI w `mod` 5))), wHueI = wHueI w + 1 }
         _ -> w
  EventKey (MouseButton LeftButton) Up _ _ -> pure $ case wCur w of
    Just c | length (iPts c) > 1 -> w { wInk = limitInk (c { iPts = reverse (iPts c) } : wInk w), wCur = Nothing }
    _ -> w { wCur = Nothing }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wWallH = min 4 (wWallH w + 0.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wWallH = max 0 (wWallH w - 0.2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wYaw = wYaw w - 0.05 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wYaw = wYaw w + 0.05 }
  EventKey (Char '1') Down _ _ -> pure w { wYaw = 0 }
  EventKey (Char '2') Down _ _ -> pure w { wYaw = pi / 6 }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> pure w { wGuide = not (wGuide w) }
    | c `elem` "cC" -> pure w { wInk = [] }
    | c `elem` "sS" -> saveSvg "perspective_brush" env (draw env w) >> pure w
    | c `elem` "rR" -> pure w { wInk = [], wYaw = 0, wWallH = wallH0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 画面の下のほうに横一直線を描いてから、← を押し続けて回す。地面の直線は回しても直線のまま？
-- 2. 壁の高さを 0 にして、地面にだけ絵を描く。「上から見た地図」を描くつもりで描くと透視図はどう見える？
-- 3. 四角い部屋を描いて（4 本の線）2 点透視にすると、建築のスケッチになる。eyeH を 0.3 にすると「床すれすれ」
-- 4. 線に沿って柱を立てるかわりに、線を「屋根」にする（上の線をずらして斜めに）。ジオメトリは自由

-- ==================================================
-- ここから下は「道具」：p5.js と同じ座標（左上が原点、y は下向き）で描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- 図全体を「左上が原点、y は下向き」の座標に置く
view :: Env -> [Picture] -> Picture
view env ps = Translate (negate (envW env) / 2) (envH env / 2) (Scale 1 (-1) (Pictures ps))

-- gloss の座標 → p5.js の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = toP5 env (envMouse env)

-- 文字（(x, y) が左下。y が下向きなので文字だけ裏返して戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 塗りつぶした丸（d は直径。p5.js の circle と同じ）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 輪（d は直径、w は線の太さ）
ringAt :: Color -> Point -> Float -> Float -> Picture
ringAt c p d w
  | w <= 1 = Color c (at p (Circle (d / 2)))
  | otherwise = Color c (at p (ring (d / 2) w))

-- 線分（太さ 1 以下は細い線）
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b
  | w <= 0 = Blank
  | w <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine w a b)

-- 閉じた折れ線
closedPath :: Float -> Color -> [Point] -> Picture
closedPath _ _ [] = Blank
closedPath w c ps@(p : rest) = Pictures (zipWith (seg w c) ps (rest ++ [p]))

-- 左上 (x, y)、幅 w、高さ h の長方形
fillRect :: Color -> Float -> Float -> Float -> Float -> Picture
fillRect c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

strokeRect :: Color -> Float -> Float -> Float -> Float -> Picture
strokeRect c x y w h = closedPath 1 c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 四角形を塗る。gloss の Polygon はへこんだ形を正しく塗れないので、
-- へこみ・ねじれ（8 の字）のときは三角形に分ける
fillQuad :: Color -> [Point] -> Picture
fillQuad col [a, b, c, d]
  | sameSign [o a b c, o b c d, o c d a, o d a b] = Color col (Polygon [a, b, c, d])
  | o a b c * o a c d > 0 = tris [[a, b, c], [a, c, d]]
  | o b c d * o b d a > 0 = tris [[b, c, d], [b, d, a]]
  | otherwise = case (crossAt a b c d, crossAt b c d a) of
      (Just x, _) -> tris [[x, b, c], [x, d, a]]
      (_, Just x) -> tris [[a, b, x], [c, d, x]]
      _ -> Blank
  where
    o (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    sameSign vs = all (> 0) vs || all (< 0) vs
    tris ts = Color col (Pictures (map Polygon ts))
    -- 線分 pq と線分 rs の交点
    crossAt (px, py) (qx, qy) (rx, ry) (sx, sy) =
      let dx1 = qx - px; dy1 = qy - py; dx2 = sx - rx; dy2 = sy - ry
          den = dx1 * dy2 - dy1 * dx2
          t = ((rx - px) * dy2 - (ry - py) * dx2) / den
          u = ((rx - px) * dy1 - (ry - py) * dx1) / den
      in if abs den < 1e-6 || t < 0 || t > 1 || u < 0 || u > 1 then Nothing
         else Just (px + t * dx1, py + t * dy1)
fillQuad _ _ = Blank

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 小数 d 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf d v
  | isNaN v || isInfinite v = "-"
  | d <= 0 = show (round v :: Int)
  | otherwise = showFFloat (Just d) v ""
