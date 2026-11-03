-- ============================================
-- 04 プリズムと分散
-- 白色光は 400〜700 nm の光の混ざりもの。ガラスの屈折率は波長で少し違う（短い波長ほど大きい）
--   コーシーの式： n(λ) = A + B/λ²   → 紫はよく曲がり、赤はあまり曲がらない → 虹色に分かれる（分散）
-- マウス上下 = 光が入る高さ / ←→ = 入射角 / ↑↓ = 頂角 / G = ガラスの種類（クラウン・フリント） / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

stepNm :: Int
stepNm = 10                -- ★ 波長の刻み（nm）小さいほど光線が多い

-- ★ B の単位 μm²
glassOf :: Int -> (String, Float, Float)
glassOf 0 = ("Crown glass", 1.5046, 0.00420)    -- ふつうのガラス
glassOf _ = ("Flint glass", 1.6000, 0.01200)    -- 分散が大きい

data World = World
  { wApex  :: !Float       -- ★ 頂角（度）
  , wInc   :: !Float       -- ★ 入射光の傾き（度）負 = 上向き
  , wGlass :: !Int         -- 0 クラウン、1 フリント
  }

main :: IO ()
main = runSketch (sketch "36-04 Prism and Dispersion" (World 60 (-20) 0))
  { skBack  = rgb255 12 12 20
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 屈折率 n(λ)、λ は nm
refIndex :: Int -> Float -> Float
refIndex g nm =
  let (_, a, b) = glassOf g
      um = nm / 1000
  in a + b / (um * um)

-- 光線と線分の交点
raySeg :: Point -> Point -> Point -> Point -> Maybe Float
raySeg (px, py) (dx, dy) (ax, ay) (bx, by)
  | abs den < 1e-9 = Nothing
  | t > 1e-3 && s >= 0 && s <= 1 = Just t
  | otherwise = Nothing
  where
    ex = bx - ax
    ey = by - ay
    den = dx * ey - dy * ex
    t = ((ax - px) * ey - (ay - py) * ex) / den
    s = ((ax - px) * dy - (ay - py) * dx) / den

-- スネルの法則で向きを変える（na → nb）。全反射なら Nothing
snellRefract :: Point -> Point -> Float -> Float -> Maybe Point
snellRefract (dx, dy) (nx0, ny0) na nb =
  let c0 = negate (dx * nx0 + dy * ny0)
      flipN = c0 < 0                      -- 法線を光の来た側へ
      nx = if flipN then negate nx0 else nx0
      ny = if flipN then negate ny0 else ny0
      c = abs c0
      r = na / nb
      k = 1 - r * r * (1 - c * c)
      f = r * c - sqrt k
  in if k < 0 then Nothing else Just (r * dx + f * nx, r * dy + f * ny)

type Edge = (Int, Point, Point)

-- いちばん近い辺（t, 番号, 法線）
nearest :: Point -> Point -> [Edge] -> Int -> Maybe (Float, Int, Point)
nearest p d edges skip = foldl' pick Nothing edges
  where
    pick best (i, a@(ax, ay), b@(bx, by))
      | i == skip = best
      | otherwise = case (raySeg p d a b, best) of
          (Nothing, _) -> best
          (Just t, Just (t0, _, _)) | t >= t0 -> best
          (Just t, _) ->
            let ex = bx - ax
                ey = by - ay
                len = max 1e-6 (norm2 (ex, ey))
            in Just (t, i, (negate ey / len, ex / len))

-- ★ 多角形のガラスを通り抜ける光線を追跡（点の列を返す）
tracePrism :: Point -> Point -> [Edge] -> Float -> [Point]
tracePrism p0@(px0, py0) d0@(dx0, dy0) edges n =
  case nearest p0 d0 edges (-1) of
    Nothing -> [p0, far p0 d0]
    Just (t, i, nrm) ->
      let p1 = (px0 + dx0 * t, py0 + dy0 * t)
      in case snellRefract d0 nrm 1 n of
           Nothing -> [p0, p1, far p1 d0]
           Just d1 -> p0 : p1 : inside (6 :: Int) p1 d1 i
  where
    far (x, y) (dx, dy) = (x + dx * 3000, y + dy * 3000)
    inside 0 p d _ = [far p d]                         -- 中で最大 6 回
    inside k p@(x, y) d@(dx, dy) lastE = case nearest p d edges lastE of
      Nothing -> [far p d]
      Just (t, i, nrm@(nx, ny)) ->
        let q = (x + dx * t, y + dy * t)
        in case snellRefract d nrm n 1 of
             Just out -> [q, far q out]
             Nothing ->                                 -- 全反射
               let dp = dx * nx + dy * ny
               in q : inside (k - 1) q (dx - 2 * dp * nx, dy - 2 * dp * ny) i

-- 列の最後の 2 点
lastTwo :: [Point] -> Maybe (Point, Point)
lastTwo (a : b : []) = Just (a, b)
lastTwo (_ : r) = lastTwo r
lastTwo [] = Nothing

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      (gName, gA, gB) = glassOf (wGlass w)
      cx = wd * 0.42
      cy = h * 0.5
      len = 340
      half = wApex w * pi / 180 / 2
      top = cy - len * cos half * 0.55
      bottom = top + len * cos half
      a = (cx, top)
      b = (cx + len * sin half, bottom)
      c = (cx - len * sin half, bottom)
      edges = [(0, a, b), (1, b, c), (2, c, a)]
      scrX = wd - 70
      -- 白色光（入射）：左端の高さ y0 から傾き incDeg で
      my = snd (mouseP5 env)
      y0 = clampF (top + 20) (max (top + 20) (bottom - 20)) my
      incR = wInc w * pi / 180
      dx = cos incR
      dy = sin incR
      sx = 40
      sy = y0 - (cx - 80 - sx) * tan incR
      nms = [400, 400 + stepNm .. 700]
      traces = [ (nm, wavelengthToRGB (fromIntegral nm), tracePrism (sx, sy) (dx, dy) edges (refIndex (wGlass w) (fromIntegral nm)))
               | nm <- nms ]
      -- 入射の線はどの波長も同じ場所を通るので、足すと白（p5.js の加算合成と同じ見え方）
      firstSeg = case traces of
        ((_, _, p : q : _) : _) -> Color (gray 0.9) (thickLine 2 p q)
        _ -> Blank
      -- 波長ごとに追跡して半透明で重ねる（p5.js の加算合成の代わり）
      rayPic (_, col, _ : q : r : rest) = Pictures
        [ fanPic col q r
        , Color (withA 0.55 (col255 col)) (thickPath 2 (r : rest)) ]
      rayPic (_, col, pts) = Color (withA 0.55 (col255 col)) (thickPath 2 (drop 1 pts))
      -- ガラスの中の光線は、入ったすぐ後はほとんど重なっている。重なって白く見える所は白に近い色で描く
      fanW = case (traces, reverse traces) of
        ((_, _, _ : _ : r0 : _) : _, (_, _, _ : _ : r1 : _) : _) -> dist2 r0 r1
        _ -> 0
      fanPic (cr, cg, cb) (x0, y0') (x1, y1) = Pictures
        [ Color (withA 0.55 (makeColor (mixW cr) (mixW cg) (mixW cb) 1))
            (thickLine 2 (x0 + (x1 - x0) * t0, y0' + (y1 - y0') * t0) (x0 + (x1 - x0) * t1, y0' + (y1 - y0') * t1))
        | k <- [0 .. 7 :: Int]
        , let t0 = fromIntegral k / 8
              t1 = fromIntegral (k + 1) / 8
              white' = 1 - min 1 (fanW * t1 / 60)
              mixW v = v / 255 + (1 - v / 255) * white' ]
      -- 偏角（入射方向と出射方向のなす角）
      deviation pts = case lastTwo pts of
        Just ((x1, y1), (x2, y2)) ->
          let odx = x2 - x1
              ody = y2 - y1
              oL = max 1e-6 (norm2 (odx, ody))
          in acos (clampF (-1) 1 ((odx * dx + ody * dy) / oL)) * 180 / pi
        Nothing -> 0
      devAt nmWanted = sum [ deviation pts | (nm, _, pts) <- traces, nm == nmWanted ]
      devViolet = devAt 400
      devRed = devAt 700
      -- スクリーンに映るスペクトル
      landed = [ (y1 + (y2 - y1) * ((scrX - x1) / (x2 - x1)), col)
               | (_, col, pts) <- traces
               , Just ((x1, y1), (x2, y2)) <- [lastTwo pts]
               , x2 - x1 > 0 ]
      spot (y, col)
        | y > 60 && y < h - 60 = Color (col255 col) (thickLine 5 (scrX - 3, y - 4) (scrX - 3, y + 4))
        | otherwise = Blank
      legend = [ Color (col255 (wavelengthToRGB nm)) (thickLine 3 (x, 100) (x, 116))
               | k <- [0, 2 .. 300 :: Int]
               , let nm = 400 + fromIntegral k
                     x = wd - 360 + fromIntegral k ]
      px = 20
      py = h - 100
  in Pictures
    [ onP5 env (Pictures
        ( [ Color (rgba255 120 160 200 40) (Polygon [a, b, c])
          , Color (rgba255 180 210 240 160) (thickPath 2 [a, b, c, a])
          , Color (gray (80 / 255)) (thickLine 6 (scrX, 60) (scrX, h - 60))
          , firstSeg ]
          ++ map rayPic traces
          ++ map spot landed
          ++ [ Color white (thickLine 3 (sx, sy) (sx + dx * 120, sy + dy * 120))
             , Color (rgba255 255 255 255 20) (box (px - 4) (py - 28) 700 90) ]
          ++ legend ))
    , txt env 20 40 18 white
        ("Apex " ++ nf 0 (wApex w) ++ " deg (Up/Down)  Incidence " ++ nf 0 (wInc w) ++ " deg (L/R)  " ++ gName
         ++ " (G)  Mouse: height  R: reset")
    , txt env 20 68 15 (gray 0.63)
        ("n(lam) = A + B/lam^2 = " ++ nf 4 gA ++ " + " ++ nf 4 gB ++ "/lam^2 (lam in um)   n(400) = "
         ++ nf 4 (refIndex (wGlass w) 400) ++ "  n(700) = " ++ nf 4 (refIndex (wGlass w) 700))
    , txt env (px + 10) py 16 white
        ("Deviation: red 700 nm " ++ nf 2 devRed ++ " deg, violet 400 nm " ++ nf 2 devViolet
         ++ " deg, gap " ++ nf 2 (devViolet - devRed) ++ " deg")
    , txt env (px + 10) (py + 28) 14 (gray 0.78) "A wider gap spreads the rainbow more. Flint glass disperses more (chandeliers)."
    , txt env (px + 10) (py + 50) 14 (gray 0.78) "Apex above 90 deg: total reflection inside, so some faces let no light out."
    , txt env (wd - 360) 136 14 (gray 0.63) "400 nm"
    , txt env (wd - 100) 136 14 (gray 0.63) "700 nm"
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wApex = min 120 (wApex w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wApex = max 10 (wApex w - 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wInc = max (-40) (wInc w - 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wInc = min 40 (wInc w + 2) }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> w { wGlass = 1 - wGlass w }
    | c `elem` "rR" -> World 60 (-20) 0
  _ -> w

-- ---------- やってみよう ----------
-- 1. 入射角（←→）を変えて、偏角の差（紫 − 赤）がいちばん小さい角を探そう（最小偏角）
-- 2. 頂角を小さく（10°）すると虹はほとんど広がらない。窓ガラスで虹が見えない理由
-- 3. B を 0 にすると全部同じに曲がる（分散なし）。B を 0.03 にすると？
-- 4. プリズムを 2 つ逆向きに並べると色がもとに戻る（ニュートンの実験）。edges をもう 1 組足してみよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
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

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- ==================================================
-- ここから下は「色の道具」（波長 nm → RGB の近似、値は 0〜255）
-- ==================================================
wavelengthToRGB :: Float -> (Float, Float, Float)
wavelengthToRGB nm0 =
  let nm = max 380 (min 780 nm0)
      (r, g, b)
        | nm < 440 = (negate (nm - 440) / 60, 0, 1)
        | nm < 490 = (0, (nm - 440) / 50, 1)
        | nm < 510 = (0, 1, negate (nm - 510) / 20)
        | nm < 580 = ((nm - 510) / 70, 1, 0)
        | nm < 645 = (1, negate (nm - 645) / 65, 0)
        | otherwise = (1, 0, 0)
      f | nm < 420 = 0.3 + 0.7 * (nm - 380) / 40
        | nm > 700 = 0.3 + 0.7 * (780 - nm) / 80
        | otherwise = 1
      q v = 255 * (v * f) ** 0.8
  in (q r, q g, q b)

-- 0〜255 の値から色を作る
col255 :: (Float, Float, Float) -> Color
col255 (r, g, b) = makeColor (r / 255) (g / 255) (b / 255) 1
