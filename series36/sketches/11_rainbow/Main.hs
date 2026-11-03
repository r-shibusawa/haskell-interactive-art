-- ============================================
-- 11 虹のしくみ
-- 太陽の光が水滴に入り（屈折）、裏で 1 回はね返り（反射）、出てくる（屈折）。この光が 42° の方向に集中する
--   水滴に入る高さ（衝突パラメータ b）を変えて光線を追跡し、戻ってくる角度をヒストグラムにする
--   角度が「いちばん溜まる」ところ（偏角が最小）が虹。波長で屈折率が違うので角度がずれて色に分かれる
-- マウス上下 = 注目する光線の高さ b / 1 = 主虹（反射 1 回） 2 = 副虹（反射 2 回） / ↑↓ = 描く光線の本数 / W = 全部の色の光線を描く
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

wavelengths :: [Float]
wavelengths = [400, 450, 500, 550, 600, 650, 700]

histN, bins :: Int
histN = 20000              -- ★ ヒストグラム用に追跡する光線の本数（色ごと）
bins = 96

data World = World
  { wBounces :: !Int       -- 1 = 主虹、2 = 副虹
  , wRays    :: !Int       -- ★ 描く光線の本数
  , wAll     :: !Bool
  , wHist    :: !Hist
  }

-- 色ごとの数え上げと、その最大値
data Hist = Hist ![U.Vector Int] !Int

main :: IO ()
main = runSketch (sketch "36-11 How Rainbows Form" (World 1 30 False (buildHist 1)))
  { skBack  = rgb255 12 16 28
  , skDraw  = drawAll
  , skEvent = onEvent
  }

loOf :: Int -> Float
loOf b = if b == 1 then 36 else 46

-- ★ 水の屈折率（コーシーの式、λ は nm）
nWater :: Float -> Float
nWater nm = let um = nm / 1000 in 1.3240 + 0.00318 / (um * um)

-- 光線と円の交点までの距離（inside = 中から外へ）
hitCircle :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool -> Maybe Float
hitCircle px py dx dy cx cy r inside =
  let ox = px - cx
      oy = py - cy
      b = 2 * (ox * dx + oy * dy)
      c = ox * ox + oy * oy - r * r
      d = b * b - 4 * c
      q = sqrt d
      t = if inside then (negate b + q) / 2 else (negate b - q) / 2
  in if d < 0 || t <= 1e-3 then Nothing else Just t

-- ★ 高さ b（0〜1）で入った光線を追跡：屈折して入る → 中で k 回反射 → 屈折して出る。道すじと「戻ってくる角度」（度）を返す
traceDrop :: Float -> Float -> Int -> Float -> Float -> Float -> ([Point], Float)
traceDrop b n k cx cy r =
  let px0 = cx - 420
      py0 = cy - b * r
  in case hitCircle px0 py0 1 0 cx cy r False of
       Nothing -> ([(px0, py0), (px0 + 900, py0)], 0)
       Just t ->
         let px1 = px0 + t
             py1 = py0
             -- 空気 → 水：法線（内向き）とスネルの法則
             nx = (cx - px1) / r
             ny = (cy - py1) / r
             cosI = nx
             cosT = sqrt (max 0 (1 - (1 - cosI * cosI) / (n * n)))
             f = cosI / n - cosT
             dx1 = 1 / n - f * nx
             dy1 = negate f * ny
             -- 中で k 回反射
             reflect :: Int -> (Float, Float, Float, Float, [Point]) -> (Float, Float, Float, Float, [Point])
             reflect 0 st = st
             reflect s st@(x, y, dx, dy, acc) = case hitCircle x y dx dy cx cy r True of
               Nothing -> st
               Just t2 ->
                 let x' = x + dx * t2
                     y' = y + dy * t2
                     mx = (cx - x') / r
                     my = (cy - y') / r
                     dp = dx * mx + dy * my
                 in reflect (s - 1) (x', y', dx - 2 * dp * mx, dy - 2 * dp * my, (x', y') : acc)
             (x2, y2, dx2, dy2, acc2) = reflect k (px1, py1, dx1, dy1, [(px1, py1), (px0, py0)])
             -- 水 → 空気
             (x3, y3, dx3, dy3, acc3) = case hitCircle x2 y2 dx2 dy2 cx cy r True of
               Nothing -> (x2, y2, dx2, dy2, acc2)
               Just t3 ->
                 let x' = x2 + dx2 * t3
                     y' = y2 + dy2 * t3
                     mx = (cx - x') / r
                     my = (cy - y') / r
                     c1 = negate (dx2 * mx + dy2 * my)
                     s2 = n * n * (1 - c1 * c1)
                     c2 = sqrt (max 0 (1 - s2))
                     g = n * c1 - c2
                     dp = dx2 * mx + dy2 * my
                 in if s2 < 1
                      then (x', y', n * dx2 + g * mx, n * dy2 + g * my, (x', y') : acc2)
                      else (x', y', dx2 - 2 * dp * mx, dy2 - 2 * dp * my, (x', y') : acc2)
             pts = reverse ((x3 + dx3 * 700, y3 + dy3 * 700) : acc3)
             -- 戻る角度：入ってきた向き (1, 0) の逆 (−1, 0) とのなす角
             back = acos (clampF (-1) 1 (negate dx3)) * 180 / pi
         in (pts, back)

-- 戻る角度（反太陽点から）を式で求める
backAngle :: Int -> Float -> Float -> Float
backAngle bounces n b =
  let i = asin b
      r = asin (sin i / n)
      dd = (2 * i - 2 * fromIntegral (bounces + 1) * r + fromIntegral bounces * pi) * 180 / pi   -- 偏角
  in abs (180 - dd)

-- ★ ヒストグラム：色ごとに b を細かく変えて戻る角度を集計（反射の回数が変わったときだけ計算）
buildHist :: Int -> Hist
buildHist bounces =
  let lo = loOf bounces
      hi = lo + 12
      one nm =
        let n = nWater nm
            place q =
              let b = (fromIntegral q + 0.5) / fromIntegral histN
                  bi = floor ((backAngle bounces n b - lo) / (hi - lo) * fromIntegral bins) :: Int
              in if bi >= 0 && bi < bins then Just bi else Nothing
            hits = U.mapMaybe place (U.enumFromN (0 :: Int) histN)
        in U.accumulate (+) (U.replicate bins 0) (U.map (\bi -> (bi, 1 :: Int)) hits)
      counts = map one wavelengths
  in Hist counts (foldl' max 1 (map U.maximum counts))

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      cx = wd * 0.32
      cy = h * 0.5
      r = 200
      bounces = wBounces w
      my = snd (mouseP5 env)
      -- 光線を描く（多いときは細い線にして軽くする）
      nRays = wRays w
      colsToDraw = if wAll w then wavelengths else [550]
      many = nRays * length colsToDraw > 300
      rayPics =
        [ Color (withA 0.35 (col255 (wavelengthToRGB nm))) (Pictures
            [ if many then Line pts else thickPath 1.5 pts
            | q <- [0 .. nRays - 1]
            , let b = (fromIntegral q + 0.5) / fromIntegral nRays
                  (pts, _) = traceDrop b (nWater nm) bounces cx cy r ])
        | nm <- colsToDraw ]
      -- 注目する 1 本（マウス）
      bSel = clampF 0.01 0.999 (remap my (cy - r) cy 1 0)
      (selPts, selBack) = traceDrop bSel (nWater 550) bounces cx cy r
      -- ヒストグラム
      hx = wd * 0.6
      hw = wd - hx - 40
      hy = h - 90
      hh = 300
      lo = loOf bounces
      hi = lo + 12
      Hist counts maxC = wHist w
      barW = hw / fromIntegral bins
      stack bi =
        let x = hx + fromIntegral bi / fromIntegral bins * hw
            go _ [] = []
            go y ((nm, c) : rest) =
              let hgt = fromIntegral (c U.! bi) / fromIntegral maxC * hh / 2.2
              in Color (withA (200 / 255) (col255 (wavelengthToRGB nm))) (box x (y - hgt) (barW + 0.5) hgt)
                 : go (y - hgt) rest
        in go hy (zip wavelengths counts)
      axisLabels = [ txt env (hx + (a - lo) / (hi - lo) * hw - 8) (hy + 20) 14 (gray 0.78) (nf 0 a)
                   | k <- [0 .. 6 :: Int], let a = lo + 2 * fromIntegral k ]
      -- 虹の角度（デカルト光線）：戻る角度が折り返す所（主虹は最大、副虹は最小）
      bestAngle nm =
        let n = nWater nm
            ang q = backAngle bounces n ((fromIntegral (q :: Int) + 0.5) / 2000)
            pick = if bounces == 1 then max else min
        in foldl' (\a q -> pick a (ang q)) (if bounces == 1 then 0 else 1e9) [0 .. 1999]
  in Pictures
    [ onP5 env (Pictures
        ( [ Color (rgba255 60 100 160 70) (at (cx, cy) (disc r))
          , Color (rgba255 150 200 255 120) (at (cx, cy) (ring r 2)) ]
          ++ rayPics ++
          [ Color (rgb255 255 255 200) (thickPath 3 selPts)
          , Color (rgba255 255 255 255 12) (box (hx - 10) (hy - hh - 30) (hw + 20) (hh + 60)) ]
          ++ concatMap stack [0 .. bins - 1]
          ++ [ if selBack > lo && selBack < hi
                 then let x = hx + (selBack - lo) / (hi - lo) * hw
                      in Color (rgb255 255 255 200) (thickLine 2 (x, hy) (x, hy - hh / 2))
                 else Blank ]))
    , Pictures axisLabels
    , txt env hx (hy - hh - 12) 14 (gray 0.78) "Rays per return angle (from antisolar point)"
    , txt env 20 40 18 white
        ((if bounces == 1 then "Primary bow (1 reflection)" else "Secondary bow (2 reflections)")
         ++ " (1/2)  Rays " ++ show nRays ++ " (Up/Down)  W: all colors " ++ onOff (wAll w))
    , txt env 20 68 15 (gray 0.78)
        ("Mouse up/down moves the white ray. b = " ++ nf 2 bSel ++ "  return angle " ++ nf 1 selBack ++ " deg")
    , txt env hx (hy + 42) 14 (gray 0.78)
        ("Light collects at: violet " ++ nf 2 (bestAngle 400) ++ " deg, red " ++ nf 2 (bestAngle 700) ++ " deg")
    , txt env hx (hy + 62) 14 (gray 0.78)
        (if bounces == 1 then "Primary: red outside, about 42 deg" else "Secondary: violet outside, about 51 deg, order reversed")
    , txt env 20 (h - 30) 14 (gray 0.78)
        ("n(400) = " ++ nf 4 (nWater 400) ++ "  n(700) = " ++ nf 4 (nWater 700))
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRays = min 200 (wRays w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRays = max 5 (wRays w - 10) }
  EventKey (Char c) Down _ _
    | c == '1' && wBounces w /= 1 -> w { wBounces = 1, wHist = buildHist 1 }
    | c == '2' && wBounces w /= 2 -> w { wBounces = 2, wHist = buildHist 2 }
    | c `elem` "wW" -> w { wAll = not (wAll w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. マウスで b を 0 → 1 に動かす。戻る角度は大きくなって、約 42° で折り返す。折り返す所に光が溜まる
-- 2. 副虹（2）は 51° 付近。主虹と副虹の間（42〜51°）は光が少なくて暗い（アレキサンダーの暗帯）
-- 3. nWater の B を 0 にすると色が分かれない（白い虹）。霧の細かい水滴で本当に見える（霧虹）
-- 4. 屈折率をガラス（1.5）にすると虹の角度はいくつ？ ガラス玉で虹を作れる？


-- ==================================================
-- ここから下は「道具」（座標の変換・数字）
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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

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
