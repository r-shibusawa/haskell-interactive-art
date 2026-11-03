-- ============================================
-- 04 黄金螺旋と白銀螺旋 ─ 正方形の渦と対数螺旋
-- 01 の分割は「1/φ 倍して 90° 回す」の繰り返し。だから角は 1 本の対数螺旋の上に並ぶ
--   z(t) = C + (z0 − C) · φ^(−t) · e^(i·πt/2)   1 周（t = 4）で φ⁴ 倍
-- ←→ = n / L = 対数螺旋 / A = 円弧の近似 / S = 正方形 / スペース = 無限ズーム / ↑↓ = ズームの速さ
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data World = World
  { wN         :: !Int     -- ★ 貴金属比の n
  , wLog       :: !Bool
  , wArc       :: !Bool
  , wSq        :: !Bool
  , wZooming   :: !Bool
  , wZoomT     :: !Float   -- 0〜1 をくり返す（1 段分のズーム）
  , wZoomSpeed :: !Float   -- ★
  }

initialWorld :: World
initialWorld = World 1 True True True False 0 0.006

main :: IO ()
main = runSketch (sketch "33-04 Metallic Spirals" initialWorld)
  { skBack  = hsb 235 30 9
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

stepWorld :: World -> World
stepWorld w
  | wZooming w = let t = wZoomT w + wZoomSpeed w
                 in w { wZoomT = if t >= 1 then t - 1 else t }
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (shapes ++ spiral ++ [center] ++ hud)
  where
    n = wN w
    fn = fromIntegral n :: Float
    phi = metallic fn
    width' = envW env
    height' = envH env
    -- いちばん外の長方形：横 L0、縦 L0/φ。画面の中央に置く
    l0 = min (width' * 0.8) ((height' - 160) * 0.85 * phi)
    o0 = (width' / 2 - l0 / 2, height' / 2 + 30 - l0 / phi / 2)
    u0 = (1, 0)
    -- 渦の中心 C = O0 + L0·u0 / (1 − i/φ)
    c = cadd o0 (cmul (cscale u0 l0) (cdiv (1, 0) (1, -1 / phi)))
    -- ズーム：中心 C のまわりに φ^t 倍・−90°·t 回すと、1 段内側が外側にぴったり重なる
    zoom p = cadd c (cscale (crot (csub p c) (-(pi / 2) * wZoomT w)) (phi ** wZoomT w))
    sc p = let (x, y) = zoom p in pt env x y
    ks = takeWhile (\k -> l0 * phi ** negate (fromIntegral k) >= 0.6) [-3 .. 25 :: Int]
    shapes = concatMap level ks
    level k =
      let fk = fromIntegral k :: Float
          lk = l0 * phi ** negate fk
          uk = crot u0 (fk * pi / 2)
          vk = crot uk (pi / 2)
          ok = cadd c (cmul (csub o0 c) (cpowPolar (1 / phi) (pi / 2) fk))
          s = lk / phi       -- 正方形の一辺
          squares =
            [ let p = cadd ok (cscale uk (fromIntegral j * s))
                  corners = [p, cadd p (cscale uk s), cadd p (cadd (cscale uk s) (cscale vk s)), cadd p (cscale vk s)]
                  hue = wrapF 360 (fromIntegral ((k + 30) * 37 + j * 14))
              in Pictures
                   [ Color (hsba hue 50 70 0.35) (Polygon (map sc corners))
                   , Color (hsba 0 0 100 0.5) (Line (map sc (corners ++ take 1 corners)))
                   ]
            | wSq w, j <- [0 .. n - 1] ]
          -- 円弧の近似：角 Q を中心とする 1/4 楕円（n=1 なら 1/4 円）
          q = cadd ok (cadd (cscale uk (fn * s)) (cscale vk s))
          arc = [ Color (hsb 190 80 100) (thickPath 3
                    [ sc (cadd q (cadd (cscale uk (-fn * s * cos th)) (cscale vk (-s * sin th))))
                    | i <- [0 .. 16 :: Int], let th = fromIntegral i * pi / 32 ])
                | wArc w ]
      in squares ++ arc
    -- 本物の対数螺旋
    p0 = cadd o0 (0, l0 / phi)    -- いちばん外の長方形の左下の角
    spiral =
      [ Color (hsb 50 90 100) (thickPath 2.5
          [ sc (cadd c (cmul (csub p0 c) (cpowPolar (1 / phi) (pi / 2) t)))
          | i <- [0 .. 579 :: Int], let t = -3 + fromIntegral i * 0.05 ])
      | wLog w ]
    center = Color white (at (let (x, y) = c in pt env x y) (disc 3))
    hud =
      [ Color (hsba 0 0 0 0.55) (box env 0 0 width' 84)
      , Color (hsba 0 0 0 0.55) (box env 0 (height' - 56) width' 56)
      , txt env 20 32 15 white ("n = " ++ show n ++ " (L/R) " ++ nameOf n ++ "   L log " ++ onOff (wLog w) ++ "   A arcs " ++ onOff (wArc w) ++ "   S squares " ++ onOff (wSq w) ++ "   Space zoom " ++ onOff (wZooming w))
      , txt env 20 62 14 (gray 0.75) "Yellow: true log spiral through all corners. Cyan: quarter circles (ellipses for n>=2)."
      , txt env 20 (height' - 34) 14 (gray 0.75) ("One turn x" ++ nf 2 (phi ** 4) ++ ", half turn x" ++ nf 3 (phi * phi) ++ ", quarter turn x" ++ nf 4 phi)
      , txt env 20 (height' - 12) 14 (gray 0.75) "A nautilus grows about 3x per turn: looser than the golden spiral (6.85x)."
      ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN = min (wN w + 1) 5 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wN = max (wN w - 1) 1 }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wZoomSpeed = min 0.03 (wZoomSpeed w * 1.4) }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wZoomSpeed = max 0.001 (wZoomSpeed w / 1.4) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wZooming = not (wZooming w) }
  EventKey (Char c) Down _ _
    | c == 'l' || c == 'L' -> w { wLog = not (wLog w) }
    | c == 'a' || c == 'A' -> w { wArc = not (wArc w) }
    | c == 's' || c == 'S' -> w { wSq = not (wSq w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 1 で 1/4 円と対数螺旋がほとんど重なることを確かめよう。n = 3 ではどれくらいずれる？
-- 2. 無限ズーム中、どこが「つなぎ目」か分かる？ 分からないのは形が自己相似だから
-- 3. cpowPolar の pi / 2 を pi / 3 に変えると？（もう長方形の渦ではなくなるが螺旋は描ける）
-- 4. 正方形の色を「k が偶数か奇数か」で 2 色に塗り分けて、渦の腕を見つけよう

-- ---------- 道具（複素数 = (x, y) として計算）----------
metallic :: Float -> Float
metallic k = (k + sqrt (k * k + 4)) / 2

nameOf :: Int -> String
nameOf 1 = "golden"
nameOf 2 = "silver"
nameOf 3 = "bronze"
nameOf k = "#" ++ show k

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

cadd, csub, cmul, cdiv :: Point -> Point -> Point
cadd (a, b) (c, d) = (a + c, b + d)
csub (a, b) (c, d) = (a - c, b - d)
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
cdiv (a, b) (c, d) = let m = c * c + d * d in ((a * c + b * d) / m, (b * c - a * d) / m)

cscale :: Point -> Float -> Point
cscale (a, b) s = (a * s, b * s)

crot :: Point -> Float -> Point
crot (a, b) th = (a * cos th - b * sin th, a * sin th + b * cos th)

-- (r·e^(iθ))^t
cpowPolar :: Float -> Float -> Float -> Point
cpowPolar r th t = let m = r ** t in (m * cos (th * t), m * sin (th * t))

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]
