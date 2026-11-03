-- ============================================
-- 11 ブラウワーの不動点定理（2 次元のイメージ）
-- 円板を自分自身の中へ連続に（ちぎらずに）写すと、必ずどこか 1 点は動かない（不動点）
--   色 = 各点が動く向き。すべての色が集まる点が不動点。矢印 = 点がどこへ行くか
-- ドラッグ = 縮める中心 q を動かす / ←→ = 回転 / ↑↓ = 縮める割合 k / S = うずまき ON/OFF / クリック（Shift） = その点から写像をくり返す / A = 矢印
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Data.Word (Word8)
import Text.Printf (printf)

imgN :: Int
imgN = 140           -- 色の画像の 1 辺

width, height, rd, cx, cy :: Float
width = 1280
height = 800
rd = min (width * 0.3) (height * 0.38)
cx = width * 0.08 + rd
cy = height * 0.55

-- 不動点の誤差 1e-7 まで見たいので、写像の計算は Double で行う
type D2 = (Double, Double)

data Params = Params
  { pTheta :: !Double   -- ★ 回転の角度
  , pK :: !Double       -- ★ 縮める割合（1 = 縮めない）
  , pSwirl :: !Double   -- ★ うずまきの強さ（中心ほど多く回る）
  , pQ :: !D2           -- 縮める中心
  }

data World = World
  { wP :: !Params
  , wArrows :: !Bool
  , wIter :: !(Maybe D2)
  , wDrag :: !Bool
  -- ↓ 写像を変えたときに作り直す（必要になるまで計算されない）
  , wImg :: Picture
  , wFixed :: (Double, Double, Double)
  }

main :: IO ()
main = runSketch (sketch "27-11 Brouwer Fixed Point Theorem (2D picture)" (refresh world0))
  { skBack  = bgCol
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

bgCol :: Color
bgCol = hsb 260 25 11

world0 :: World
world0 = World (Params 0.9 0.8 2.5 (0.3, -0.2)) True Nothing False Blank (0, 0, 0)

refresh :: World -> World
refresh w = w { wImg = makeColors (wP w), wFixed = findFixed (wP w) }

-- | ★ 写像 f：回転（中心ほど多く回るうずまき）してから、点 q へ向かって k 倍に縮める。円板の中に収まる
f :: Params -> Double -> Double -> D2
f (Params th k sw (qx, qy)) x y =
  let a = th + sw * (1 - (x * x + y * y))
      ca = cos a
      sa = sin a
      rx = x * ca - y * sa
      ry = x * sa + y * ca
  in (qx + k * (rx - qx), qy + k * (ry - qy))

-- | 不動点さがし：粗く |f(p) − p| が最小の所を探し、細かく詰める。戻り値は (x, y, 誤差)
findFixed :: Params -> (Double, Double, Double)
findFixed par =
  let gap x y = let (px, py) = f par x y in sqrt ((px - x) ^ (2 :: Int) + (py - y) ^ (2 :: Int))
      grid = [ (-1 + 2 * fromIntegral i / 60, -1 + 2 * fromIntegral j / 60) | i <- [0 .. 60 :: Int], j <- [0 .. 60 :: Int] ]
      coarse (bx, by, bd) (x, y)
        | x * x + y * y > 1 = (bx, by, bd)
        | otherwise = let d = gap x y in if d < bd then (x, y, d) else (bx, by, bd)
      start = foldl' coarse (0, 0, 1e9) grid
      refine :: Int -> Double -> (Double, Double, Double) -> (Double, Double, Double)
      refine it h b@(bx, by, bd)
        | it >= 200 || h <= 1e-7 = b
        | otherwise =
            let try1 acc@(_, _, ad) (dx, dy) =
                  let x = bxOf acc + dx
                      y = byOf acc + dy
                      d = gap x y
                  in if d < ad then (x, y, d) else acc
                b' = foldl' try1 b [(h, 0), (-h, 0), (0, h), (0, -h)]
                (_, _, bd') = b'
            in refine (it + 1) (if bd' < bd then h else h / 2) b'
      bxOf (x, _, _) = x
      byOf (_, y, _) = y
  in refine 0 0.03 start

-- | 色 = 点が動く向き。暗い = ほとんど動かない
makeColors :: Params -> Picture
makeColors par =
  let n = fromIntegral imgN :: Double
      (bgR, bgG, bgB, _) = rgbaOfColor bgCol
      w8 :: Float -> Word8
      w8 v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
      outside = (w8 bgR, w8 bgG, w8 bgB)
      px i j =
        let x = -1 + 2 * (fromIntegral i + 0.5) / n
            y = 1 - 2 * (fromIntegral j + 0.5) / n
        in if x * x + y * y > 1 then outside else
           let (qx, qy) = f par x y
               dx = qx - x
               dy = qy - y
               m = sqrt (dx * dx + dy * dy)
               deg = atan2 dy dx * 180 / pi + 360
               h = deg - 360 * fromIntegral (floor (deg / 360) :: Int)
           in hsb2rgb h 0.7 (0.3 + 0.6 * min 1 (m * 3))
  in pixelImage imgN imgN (rd * 2 / fromIntegral imgN) px

-- | 色相（0〜360）・彩度・明るさ（0〜1）→ RGB（0〜255）
hsb2rgb :: Double -> Double -> Double -> (Word8, Word8, Word8)
hsb2rgb h s v =
  let c = v * s
      hh = h / 60
      x = c * (1 - abs (hh - 2 * fromIntegral (floor (hh / 2) :: Int) - 1))
      m = v - c
      (r, g, b)
        | h < 60 = (c, x, 0)
        | h < 120 = (x, c, 0)
        | h < 180 = (0, c, x)
        | h < 240 = (0, x, c)
        | h < 300 = (x, 0, c)
        | otherwise = (c, 0, x)
      q t = fromIntegral (max 0 (min 255 (round ((t + m) * 255) :: Int)))
  in (q r, q g, q b)

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w =
  let par = wP w
      left = isDown env (SpecialKey KeyLeft)
      right = isDown env (SpecialKey KeyRight)
      up = isDown env (SpecialKey KeyUp)
      down = isDown env (SpecialKey KeyDown)
      th1 = pTheta par + (if left then -0.02 else 0) + (if right then 0.02 else 0)
      k1 = (if up then min 1 (pK par + 0.005) else pK par)
      k2 = if down then max 0.1 (k1 - 0.005) else k1
      (mx, my) = toP5 (envMouse env)
      x0 = realToFrac ((mx - cx) / rd) :: Double
      y0 = realToFrac (-(my - cy) / rd) :: Double
      m = sqrt (x0 * x0 + y0 * y0)
      q1 | wDrag w = if m > 0.95 then (x0 * 0.95 / m, y0 * 0.95 / m) else (x0, y0)
         | otherwise = pQ par
      dirty = left || right || up || down || wDrag w
  in if dirty then refresh w { wP = par { pTheta = th1, pK = k2, pQ = q1 } } else w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods pos ->
    let (mx, my) = toP5 pos
        x = (mx - cx) / rd
        y = -(my - cy) / rd
    in if x * x + y * y > 1 then w
       else if shift mods == Down then w { wIter = Just (realToFrac x, realToFrac y) }
       else w { wDrag = True }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> refresh w { wP = (wP w) { pSwirl = if pSwirl (wP w) /= 0 then 0 else 2.5 } }
    | c `elem` "aA" -> w { wArrows = not (wArrows w) }
  _ -> w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let par = wP w
      toScr (x, y) = (cx + realToFrac x * rd, cy - realToFrac y * rd) :: (Float, Float)
      c70 = hsba 0 0 100 0.7
      -- 矢印：p → f(p)
      arrowPic i j =
        let x = fromIntegral i / 7.5 :: Double
            y = fromIntegral j / 7.5 :: Double
            (ax, ay) = toScr (x, y)
            (bx, by) = toScr (f par x y)
            l0 = dist2 (ax, ay) (bx, by)
            s = min 1 (22 / (if l0 == 0 then 1 else l0))
            e = (ax + (bx - ax) * s, ay + (by - ay) * s)
        in if x * x + y * y > 0.98 then Blank
           else Color c70 (Pictures [thickLine 1.5 (pt (ax, ay)) (pt e), at (pt e) (disc 2)])
      arrows = if wArrows w then [arrowPic i j | i <- [-7 .. 7 :: Int], j <- [-7 .. 7 :: Int]] else []
      -- くり返しの軌跡
      iterPic = case wIter w of
        Nothing -> Blank
        Just p0 ->
          let ps = take 61 (iterate (\(x, y) -> f par x y) p0)
          in Color (hsb 50 90 100) (thickPath 2 (map (pt . toScr) ps))
      (qx, qy) = pQ par
      (fx, fy, err) = wFixed w
      fixedScr = toScr (fx, fy)
      x0 = cx + rd + 60
      y0 = height * 0.3
      lines' =
        [ "Color = direction moved. Dark = barely moves"
        , "All colors meet at the fixed point (star)"
        , "Yellow: f x 60 from the Shift+clicked point"
        , "Small k: sucked into the point (contraction)"
        , "k = 1: it may keep circling"
        , "Stir coffee gently, then stop:"
        , "one point ends where it started" ]
  in Pictures $
       [ at (pt (cx, cy)) (wImg w)
       , at (pt (cx, cy)) (Color (gray 1) (ring rd 2)) ]
       ++ arrows
       ++ [ iterPic
          , at (pt (toScr (qx, qy))) (Color (gray 1) (disc 6))
          , star fixedScr 9 20
          , txt x0 y0 18 (gray 1) (printf "Fixed point x* = (%.3f, %.3f)" fx fy)
          , txt x0 (y0 + 34) 16 (gray 1) (printf "|f(x*) - x*| = %.1e" err)
          , txt x0 (y0 + 68) 16 (gray 1)
              (printf "Rotation %.2f   k = %.2f   Swirl %s" (pTheta par) (pK par) (if pSwirl par /= 0 then "ON" else "off" :: String)) ]
       ++ [ txt x0 (y0 + 120 + fromIntegral i * 26) 14 (gray 0.65) s | (i, s) <- zip [0 :: Int ..] lines' ]
       ++ [ txt 20 40 16 (gray 1) "Drag: move q   <- ->: rotate   Up/Down: k   S: swirl   Shift+click: iterate   A: arrows"
          , txt 20 68 14 (gray 0.6) "A continuous map disk -> disk always has a fixed point (Brouwer). White dot = q. The star never disappears." ]

-- | 星：へこみがあるので、中心からの三角形を並べて塗る
star :: (Float, Float) -> Float -> Float -> Picture
star (x, y) r1 r2 =
  let ps = [ pt (x + r0 * cos a, y + r0 * sin a)
           | i <- [0 .. 9 :: Int]
           , let a = -pi / 2 + fromIntegral i * pi / 5
           , let r0 = if odd i then r1 else r2 ]
      closed = ps ++ take 1 ps
      c = pt (x, y)
  in Pictures
       [ Color (hsb 55 90 100) (Pictures (zipWith (\a b -> Polygon [c, a, b]) closed (drop 1 closed)))
       , Color black (thickPath 3 closed) ]

-- ---------- やってみよう ----------
-- 1. q をどこへ動かしても、回転をどう変えても星は消えない。円板の外へ出す写像（k > 1）にすると？
-- 2. 円板のかわりに「ドーナツ形（輪）」だと不動点のない写像がある。どんな写像？（ヒント：回転だけ）
-- 3. f を自分の式に変えてみよう。円板の中に収まっていれば、必ず色が全部集まる点がある
-- 4. 1 次元版：[0, 1] → [0, 1] の連続関数のグラフは必ず y = x と交わる。紙に描いて確かめよう

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
