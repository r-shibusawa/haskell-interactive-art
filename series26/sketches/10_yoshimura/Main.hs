-- ============================================
-- 10 吉村パターンと円筒の座屈（3D）
-- 薄い円筒（空き缶）を上から押すと、ある力で突然ダイヤ模様につぶれる（座屈）。その模様が吉村パターン
--   吉村パターンは剛体のままでは折れない：縮めるには面がひずむ（色 = ひずみ）。K でねじれる「クレスリング」
-- ←→ = 押す（縮める）/ A = 自動 / K = 吉村 ⇄ クレスリング / ↑↓ = 周りの数 n / W = 線だけ / ドラッグ = 視点 / ホイール = 近づく
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

rings :: Int
rings = 8                -- ★ 段の数

r0, h0 :: Float
r0 = 120                 -- ★ 円筒の半径（px）
h0 = 46                  -- ★ 1 段の高さ（伸びきったとき）

data World = World
  { wN     :: !Int       -- ★ 1 周の三角形の数
  , wT     :: !Float     -- 縮み 0〜1
  , wAuto  :: !Bool
  , wDir   :: !Float
  , wKres  :: !Bool
  , wWire  :: !Bool
  , wYaw   :: !Float
  , wPitch :: !Float
  , wDist  :: !Float
  , wLast  :: !Point
  }

main :: IO ()
main = runSketch (sketch "26-10 Yoshimura Pattern" (World 10 0 True 1 False False 0 (atan2 260 640) (sqrt (260 * 260 + 640 * 640)) (0, 0)))
  { skBack  = rgb255 30 28 36
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 縮み t のときの形：段の高さ h、半径 R、ねじれ ψ
data Shape = Shape { sH, sR, sPsi :: !Float }

shapeOf :: World -> Float -> Shape
shapeOf w t =
  let n = fromIntegral (wN w)
      h = h0 * (1 - 0.8 * t)
      e = tau * r0 / n                      -- 横の辺の長さ（紙のときと同じに保つ）
      r = e / (2 * sin (pi / n))
      psi | wKres w =
              -- クレスリング：斜めの辺の 1 本の長さを保つように、ねじれ ψ を決める
              let c0 = 2 * r * sin ((tau / n - pi / n * 0.6) / 2)
                  l0sq = h0 * h0 + c0 * c0
                  c = sqrt (max 0 (l0sq - h * h)) / (2 * r)
                  d = 2 * asin (min 1 c)
              in tau / n - d
          | otherwise = 0
  in Shape h r psi

-- ★ 段 k・番号 i の頂点（y が上下、上向きが正）
vert :: World -> Shape -> (Int, Int) -> V3
vert w st (k, i) =
  let n = fromIntegral (wN w)
      phi = tau * fromIntegral i / n
          + (if wKres w then fromIntegral k * sPsi st else fromIntegral (k `mod` 2) * pi / n)
  in V3 (sR st * cos phi) (-(fromIntegral rings / 2 - fromIntegral k) * sH st) (sR st * sin phi)

-- この段・この番号の 2 枚の三角形
trisAt :: World -> Int -> Int -> [[(Int, Int)]]
trisAt w k i
  | wKres w || even k = [[(k, i), (k, i + 1), (k + 1, i)], [(k, i + 1), (k + 1, i + 1), (k + 1, i)]]
  | otherwise = [[(k, i), (k, i + 1), (k + 1, i + 1)], [(k, i), (k + 1, i + 1), (k + 1, i)]]

-- 3 辺のひずみのうち絶対値が最大のもの：今の長さ ÷ 紙のときの長さ − 1
strainOf :: [V3] -> [V3] -> Float
strainOf ps qs = foldl pick 0 (zipWith strain (edges ps) (edges qs))
  where
    edges xs = zip xs (drop 1 xs ++ take 1 xs)
    strain (a, b) (a0, b0) = norm3 (sub3 a b) / norm3 (sub3 a0 b0) - 1
    pick s v = if abs v > abs s then v else s

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let w1 = if isDown env (SpecialKey KeyRight) then w0 { wT = min 1 (wT w0 + 0.006), wAuto = False } else w0
      w2 = if isDown env (SpecialKey KeyLeft) then w1 { wT = max 0 (wT w1 - 0.006), wAuto = False } else w1
      w3 | wAuto w2 =
             let t = wT w2 + 0.004 * wDir w2
             in if t > 1 || t < 0 then w2 { wT = clampF 0 1 t, wDir = -(wDir w2) } else w2 { wT = t }
         | otherwise = w2
  in pure w3

toView :: World -> V3 -> V3
toView w = rotX (wPitch w) . rotY (wYaw w)

persp :: World -> V3 -> Point
persp w v = let (x, y) = project (wDist w) v
                k = 800 / wDist w
            in (x * k, y * k)

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let st = shapeOf w (wT w)
      st0 = shapeOf w 0
      tris =
        [ (d, s, pic)
        | k <- [0 .. rings - 1]
        , i <- [0 .. wN w - 1]
        , tri <- trisAt w k i
        , let ps = map (vert w st) tri
        , let qs = map (vert w st0) tri
        , let s = strainOf ps qs
        , let view = map (toView w) ps
        , let d = sum [z | V3 _ _ z <- view] / 3
        , let pts = map (persp w) view
        , let pic = triPic w k i s ps pts ]
      maxS = maximum (0 : [abs s | (_, s, _) <- tris])
      scene = [p | (_, _, p) <- depthSort (\(d, _, _) -> d) tris]
  in Pictures (scene ++ [drawHud env w st maxS])

triPic :: World -> Int -> Int -> Float -> [V3] -> [Point] -> Picture
triPic w k i s ps pts
  | wWire w = Color (gray 0.94) (poly pts)
  | otherwise =
      let m = clampF 0 1 (abs s * 4)
          (br, bg, bb) = if odd (k + i) then (246, 240, 226) else (235, 225, 205)
          (hr, hg, hb) = if s < 0 then (70, 120, 240) else (230, 70, 50)
          n = case ps of
            [a, b, c] -> unit3 (cross3 (sub3 b a) (sub3 c a))
            _ -> V3 0 0 1
          l1 = unit3 (V3 (-0.3) (-0.6) (-1))
          l2 = unit3 (V3 0.6 0.3 0.5)
          lit = min 1 (100 / 255 + 0.7 * abs (dot3 n l1) + 0.3 * abs (dot3 n l2))
          mix a b = lerpF a b m / 255 * lit
      in Pictures
           [ Color (makeColor (mix br hr) (mix bg hg) (mix bb hb) 1) (Polygon pts)
           , Color (rgba255 40 30 30 140) (poly pts) ]

-- 円筒を押したときの力（実験でよく見る形を簡単にしたもの）
forceShape :: Float -> Float
forceShape x
  | x < 0.08 = x / 0.08
  | otherwise = 0.3 + 0.08 * sin (x * 40) * (1 - x) + 0.2 * x * x

drawHud :: Env -> World -> Shape -> Float -> Picture
drawHud env w st maxS =
  let n = wN w
      -- 展開図（左下）
      ox = 30
      oy = envH env - 230
      cw = 260
      ch = 170
      rws = 4 :: Int
      (pcx, pcy) = sc env (ox + cw / 2, oy + ch / 2)
      dx = cw / fromIntegral n
      dy = ch / fromIntegral rws
      ln a b = thickLine 1.5 (sc env a) (sc env b)
      horiz = [ Color (rgb255 230 70 50) (ln (ox, oy + fromIntegral k * dy) (ox + cw, oy + fromIntegral k * dy)) | k <- [0 .. rws] ]
      diag = Color (rgb255 60 110 230) (Pictures (concat
        [ if wKres w
            then let sh = dx * 0.4
                 in [ p | i > 0, p <- [ln (x0, y0) (x0 - sh, y0 + dy), ln (x0 - dx, y0) (x0 - sh, y0 + dy)] ]
            else
              -- ジグザグ：上の点と下の点を交互に結ぶ
              let xt = ox + fromIntegral i * dx + (if odd k then dx / 2 else 0)
                  xb = ox + fromIntegral i * dx + (if odd k then 0 else dx / 2)
              in [ ln (xt, y0) (xb, y0 + dy) | i < n ]
                 ++ [ ln (xb, y0 + dy) (xt + dx, y0) | i < n, xt + dx <= ox + cw + 0.5 ]
        | k <- [0 .. rws - 1], i <- [0 .. n]
        , let x0 = ox + fromIntegral i * dx
        , let y0 = oy + fromIntegral k * dy ]))
      patt = [Color (rgb255 246 240 226) (rect pcx pcy cw ch)] ++ horiz ++ [diag]
             ++ [txt env ox (oy + ch + 22) 14 (gray 0.78) "Crease pattern (red = M, blue = V). Roll it up and join the ends to make a tube."]
      -- 押す力のイメージ
      gx = envW env - 330
      gy = envH env - 70
      gw = 280
      gh = 150
      (bx, by) = sc env (gx - 30 + (gw + 50) / 2, gy - gh - 40 + (gh + 80) / 2)
      graph =
        [ Color (rgba255 0 0 0 120) (rect bx by (gw + 50) (gh + 80))
        , Color (gray 0.47) (thickLine 1 (sc env (gx, gy)) (sc env (gx + gw, gy)))
        , Color (gray 0.47) (thickLine 1 (sc env (gx, gy)) (sc env (gx, gy - gh)))
        , Color (rgb255 255 190 40) (thickPath 2.5 [sc env (gx + gw * x, gy - gh * forceShape x) | k <- [0 .. 100 :: Int], let x = fromIntegral k / 100])
        , Color white (at (sc env (gx + gw * wT w, gy - gh * forceShape (wT w))) (disc 5))
        , txt env (gx - 20) (gy - gh - 16) 14 (gray 0.78) "Pressing force (idea)"
        , txt env (gx + gw - 50) (gy + 18) 14 (gray 0.78) "shrink ->"
        , txt env (gx + 60) (gy - gh * 0.95) 14 (gray 0.78) "Buckling: force drops"
        ]
  in Pictures $
       [ txt env 20 40 18 white ("Left/Right: press   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   K: " ++ (if wKres w then "Kresling (twist)" else "Yoshimura (diamond)") ++ "   Up/Down: n = " ++ show n ++ "   W: wire   Drag: view")
       , txt env 20 68 15 (gray 0.67) "Strong color = paper must stretch (red) or shrink (blue). Paper hardly stretches, so the real one snaps."
       , txt env 20 110 20 (rgb255 255 190 40) ("Height " ++ nf 0 (100 * sH st / h0) ++ "%   Max strain " ++ nf 1 (maxS * 100) ++ "%")
       ]
       ++ [ txt env 20 138 16 (gray 0.86) ("Twist per ring " ++ nf 1 (sPsi st * 180 / pi) ++ " deg") | wKres w ]
       ++ patt ++ graph

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> w { wLast = p }
  EventKey (MouseButton WheelUp) Down _ _ -> w { wDist = max 300 (wDist w * 0.93) }
  EventKey (MouseButton WheelDown) Down _ _ -> w { wDist = min 2500 (wDist w * 1.07) }
  EventMotion p@(x, y) | envDown env ->
    let (lx, ly) = wLast w
    in w { wYaw = wYaw w + (x - lx) * 0.01
         , wPitch = clampF (-1.55) 1.55 (wPitch w - (y - ly) * 0.01)
         , wLast = p }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wN = min 16 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wN = max 5 (wN w - 1) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> w { wAuto = not (wAuto w) }
    'k' -> w { wKres = not (wKres w) }
    'w' -> w { wWire = not (wWire w) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 吉村パターンとクレスリングで、同じだけ縮めたときのひずみの最大を比べよう。どちらが紙にやさしい？
-- 2. n を 6 と 16 にすると、ひずみはどう変わる？ 空き缶の座屈模様の数と見比べよう
-- 3. 紙で吉村パターンの筒を折って、上から押してみよう。「パチン」と縮む瞬間がある（双安定）
-- 4. クレスリングは押すとねじれる。ストローや折りたたみボトル、ロボットの関節に使われている

-- ==================================================
-- ここから下は「画面の道具」
-- ==================================================

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
