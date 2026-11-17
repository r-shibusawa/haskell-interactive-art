-- ============================================
-- 48-12 主観的輪郭 ─ カニッツァの三角形とネオン色拡散
-- 欠けた円（パックマン）を 3 つ置くだけで、描いていない白い三角形のふちが見える。真ん中は背景より白く見える
-- 口の向きがそろっていると見え、少し回すと消える。ネオン色拡散：線の一部の色だけで、面に色がしみ出して見える
-- 操作：3・4 = 三角形 / 四角形 / ←→ = パックマンを回す / ↑↓ = 口の角度 / O = 線の角を足す / N = ネオン色拡散
--   T またはマウスを押している間 = 真実を見る（描いていない輪郭を点線で示し、中と外の画素を読む）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate)
import System.Environment (lookupEnv)

bgV, ink :: Int
bgV = 250                    -- 背景の明るさ
ink = 20                     -- パックマンの色

neonC :: RGB
neonC = (40, 110, 240)       -- ★ ネオンの色

pr :: Float
pr = 70                      -- ★ パックマンの半径 px

gap, rn :: Float
gap = 36                     -- ネオン：格子の間隔 px
rn = 150                     -- ネオン：青くする円の半径 px

data World = World
  { wN       :: !Int           -- ★ パックマンの数（3 = 三角形、4 = 四角形）
  , wTwist   :: !Int           -- パックマンを回す角度（度）。0 で口がそろう
  , wExtra   :: !Int           -- 口の角度の追加（度）。0 で多角形の角とぴったり
  , wOutline :: !Bool
  , wNeon    :: !Bool
  , wTruth   :: !Bool
  , wTest    :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-12 Kanizsa triangle" w0)
  { skBack  = g' bgV
  , skStart = \env -> do
      let rs = selfTest env w0
      printSelfTest rs
      pure w0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = World 3 0 0 False False False []

-- 図の中心と大きさ（計算は Double で正確に）
geom :: Env -> (Double, Double, Double)
geom env = (fromIntegral (jsRound (envW env * 0.45)), fromIntegral (jsRound (envH env * 0.55)), fromIntegral (jsRound (min (envW env) (envH env) * 0.27)))

-- 多角形の頂点（上向きの三角形・正方形）
verts :: Env -> World -> [(Double, Double)]
verts env w =
  [ (cx + r * cos a, cy + r * sin a)
  | i <- [0 .. n - 1], let a = -pi / 2 + fromIntegral i * 2 * pi / fromIntegral n + (if n == 4 then pi / 4 else 0) ]
  where (cx, cy, r) = geom env
        n = wN w

-- i 番目のパックマンの口：中心の向きと、口の半分の角度（ラジアン）
mouth :: Env -> World -> (Double, Double) -> (Double, Double)
mouth env w (vx, vy) =
  let (cx, cy, _) = geom env
      toC = atan2 (cy - vy) (cx - vx)
      interior = pi - 2 * pi / fromIntegral (wN w)          -- 正多角形の内角（三角形 60°、四角形 90°）
  in (toC + fromIntegral (wTwist w) * pi / 180, (interior + fromIntegral (wExtra w) * pi / 180) / 2)

scene :: Env -> World -> Bool -> [Shape]
scene env w truth
  | wNeon w = neonLines env ++ (if truth then dashedCircle (f cx, f cy) rn else [])
  | otherwise =
      (if wOutline w then corners else [])
      ++ [ Pie (g ink) (f vx) (f vy) pr (f (d + h)) (f (d - h + 2 * pi)) | v@(vx, vy) <- vs, let (d, h) = mouth env w v ]
      ++ (if truth then dashedPoly [ (f x, f y) | (x, y) <- vs ] else [])
  where
    (cx, cy, _) = geom env
    vs = verts env w
    n = wN w
    -- 線でできた逆向きの三角形（角だけ）
    ws = [ (cx + (2 * cx - x - cx) * 0.95, cy + (2 * cy - y - cy) * 0.95) | (x, y) <- vs ]
    corners = [ RSeg (g ink) 3 (f x, f y) (f (x + (x2 - x) * 0.22), f (y + (y2 - y) * 0.22))
              | (i, (x, y)) <- zip [0 ..] ws, j <- [1, n - 1], let (x2, y2) = getAt (0, 0) ws ((i + j) `mod` n) ]

-- ネオン色拡散：黒い格子の線のうち、円の中の部分だけ青
neonLines :: Env -> [Shape]
neonLines env =
  concat [ one True x | x <- [wrapF gap fcx, wrapF gap fcx + gap .. envW env - 0.01] ]
  ++ concat [ one False y | y <- [wrapF gap fcy, wrapF gap fcy + gap .. envH env - 0.01] ]
  where
    (cx, cy, _) = geom env
    fcx = f cx
    fcy = f cy
    one vert c =
      let off = if vert then c - fcx else c - fcy
          hh = sqrt (rn * rn - off * off)
          blackL = if vert then Seg (g ink) 3 (c, 0) (c, envH env) else Seg (g ink) 3 (0, c) (envW env, c)
          blueL = if vert then Seg neonC 3 (c, fcy - hh) (c, fcy + hh) else Seg neonC 3 (fcx - hh, c) (fcx + hh, c)
      in blackL : [ blueL | abs off < rn ]

-- ふちの点：1 番目と 2 番目の頂点の中点（ふちの上）と、外側・内側に 20 px
edgeProbe :: Env -> World -> ((Int, Int), (Int, Int), (Int, Int))
edgeProbe env w =
  let vs = verts env w
      (cx, cy, _) = geom env
      (ax, ay) = getAt (0, 0) vs 0
      (bx, by) = getAt (0, 0) vs (1 `mod` wN w)
      (mx, my) = ((ax + bx) / 2, (ay + by) / 2)
      dx = mx - cx
      dy = my - cy
      d = sqrt (dx * dx + dy * dy)
      rd v = floor (v + 0.5) :: Int
  in ((rd mx, rd my), (rd (mx + dx / d * 20), rd (my + dy / d * 20)), (rd (mx - dx / d * 20), rd (my - dy / d * 20)))

draw :: Env -> World -> Picture
draw env w = view env $
  [ render shs ]
  ++ (if truth && not (wNeon w) then
        let (on, out, _) = edgeProbe env w
            (cx, cy, _) = geom env
            rp (x, y) = showRGB (readPix (g bgV) shs x y)
        in [ txt 20 (envH env - 40) 15 (rgb255 230 40 40)
               ("inside " ++ rp (jsRound (f cx), jsRound (f cy)) ++ "   on the edge " ++ rp on ++ "   outside " ++ rp out) ]
      else [])
  -- ネオンの格子の上でも文字が読めるように、文字の下だけ背景色の帯をしく
  ++ (if wNeon w then [ Color (rgba255 bgV bgV bgV 225) (rect (envW env / 2) 46 (envW env) 92), Color (rgba255 bgV bgV bgV 225) (rect 110 (envH env - 21) 220 26) ] else [])
  ++ [ txt 20 30 18 (g' 20) ("3/4: " ++ (if wN w == 3 then "triangle" else "square") ++ "   Left/Right: twist " ++ show (wTwist w)
                             ++ " deg   Up/Down: mouth " ++ (if wExtra w >= 0 then "+" else "") ++ show (wExtra w) ++ " deg")
     , txt 20 54 18 (g' 20) ("O: corners " ++ onOff (wOutline w) ++ "   N: neon " ++ onOff (wNeon w) ++ "   T or hold mouse: truth")
     , txt 20 78 15 (g' 80) (if wNeon w then "Only the lines inside the circle are blue. The white gaps are " ++ show bgV ++ " like outside, yet look bluish."
                             else "No edge is drawn (the middle is " ++ show bgV ++ " like the background). Twist the mouths: does it vanish?")
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    shs = scene env w truth

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "34" -> w { wN = fromEnum c - fromEnum '0' }
    | c `elem` "oO" -> w { wOutline = not (wOutline w) }
    | c `elem` "nN" -> w { wNeon = not (wNeon w) }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTwist = min 90 (wTwist w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTwist = max (-90) (wTwist w - 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wExtra = min 60 (wExtra w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wExtra = max (-50) (wExtra w - 5) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ←→ で少しずつ回す。三角形が見えなくなるのは何度ぐらい？
-- 2. ↑↓ で口を広げる・せばめる。ぴったり（0°）のときが一番くっきり？
-- 3. pr（パックマンの大きさ）を小さくする。すき間が広いと輪郭は弱くなる？
-- 4. O で線の角を足すと、三角形が「上に重なっている」感じが強くなる
-- 5. ネオン（N）で、neonC の色を赤や緑に変えてみよう

f :: Double -> Float
f = realToFrac

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

showRGB :: RGB -> String
showRGB (r, gg, b) = intercalate "," (map show [r, gg, b])

getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 赤い点線（7 px 描いて 6 px あける）
dashed :: Point -> Point -> [Shape]
dashed (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      at' t = (x1 + (x2 - x1) * t / l, y1 + (y2 - y1) * t / l)
  in [ Seg (230, 40, 40) 1.5 (at' s) (at' (min l (s + 7))) | l > 0, s <- [0, 13 .. l] ]

dashedPoly :: [Point] -> [Shape]
dashedPoly ps = concat (zipWith dashed ps (drop 1 ps ++ take 1 ps))

dashedCircle :: Point -> Float -> [Shape]
dashedCircle (x, y) r = let n = 72 :: Int; p k = (x + r * cos (tau * fromIntegral k / fromIntegral n), y + r * sin (tau * fromIntegral k / fromIntegral n))
                        in [ Seg (230, 40, 40) 1.5 (p k) (p (k + 1)) | k <- [0, 2 .. n - 1] ]

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s
txtR x y size c s = txt (x - fromIntegral (length s) * size * 0.72) y size c s

-- 灰色（0〜255）
g :: Int -> RGB
g v = (v, v, v)

g' :: Int -> Color
g' v = rgb255 v v v

c' :: RGB -> Color
c' (r, gg, b) = rgb255 r gg b

-- JavaScript の Math.round と同じ（0.5 は上へ）
jsRound :: Float -> Int
jsRound x = floor (x + 0.5)

-- ==================================================
-- ここから下は「錯視の道具」（画素を読む・自己テスト。中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 図は「形のリスト」で作る。同じリストから、画面の絵（render）と「画素を読む」関数（readPix）の両方を作る
--   座標は p5.js と同じ（左上が原点、y は下向き）。色は 0〜255 の整数なので、ぴったり同じ値かどうかを比べられる
type RGB = (Int, Int, Int)

data Shape
  = Box !RGB !Float !Float !Float !Float          -- 左上 (x, y)・幅・高さ の長方形
  | Dot !RGB !Float !Float !Float                 -- 中心 (x, y)・半径 の円
  | Ell !RGB !Float !Float !Float !Float          -- 中心 (x, y)・横の半径・縦の半径 のだ円
  | Pie !RGB !Float !Float !Float !Float !Float   -- 中心 (x, y)・半径・角度 a0 から a1 まで の扇形（p5.js の arc PIE）
  | Seg !RGB !Float !Point !Point                 -- 太さ w の線分（はしは四角）
  | RSeg !RGB !Float !Point !Point                -- 太さ w の線分（はしは丸い）
  | Poly !RGB [Point]                             -- 凸多角形
  | Img !Float !Float !Float !Float !Int !Int (Int -> Int -> RGB) Picture
      -- 画像：左上 (x, y)・横と縦の倍率・幅・高さ（画素）・画素の色・作っておいた絵
  | Fade !Int Shape                               -- 透明度 0〜255 をつける

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map (recolour 255)
  where
    recolour a sh = case sh of
      Box c x y w h -> Color (col a c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col a c) (at (x, y) (disc r))
      Ell c x y rx ry -> Color (col a c) (Translate x y (Scale rx ry (circleSolid 1)))
      Pie c x y r a0 a1 -> Color (col a c) (Pictures [ Polygon ((x, y) : [ (x + r * cos t, y + r * sin t) | t <- arcPts b0 b1 ]) | (b0, b1) <- pieces a0 a1 ])
      Seg c w p q -> Color (col a c) (thickLine w p q)
      RSeg c w p@(px, py) q@(qx, qy) -> Color (col a c) (Pictures [thickLine w p q, at (px, py) (disc (w / 2)), at (qx, qy) (disc (w / 2))])
      Poly c ps -> Color (col a c) (Polygon ps)
      Img x y kx ky iw ih _ pic -> Translate (x + fromIntegral iw * kx / 2) (y + fromIntegral ih * ky / 2) (Scale kx (-ky) pic)
      Fade b s -> recolour (a * b `div` 255) s
    col a (r, gg, b) = rgba255 r gg b a
    -- 扇形は 90° 以下に分けると凸になる（gloss の Polygon は凸の形だけ）
    pieces a0 a1 = let n = max 1 (ceiling ((a1 - a0) / (pi / 2)) :: Int)
                       d = (a1 - a0) / fromIntegral n
                   in [ (a0 + fromIntegral i * d, a0 + fromIntegral (i + 1) * d) | i <- [0 .. n - 1] ]
    arcPts b0 b1 = [ b0 + (b1 - b0) * fromIntegral i / 12 | i <- [0 .. 12 :: Int] ]

-- 画素の絵を作る（左上が (0, 0)、1 画素 = 1）。Img に入れて使う
imgPicture :: Int -> Int -> (Int -> Int -> RGB) -> Picture
imgPicture iw ih f = pixelImage iw ih 1 (\x y -> let (r, gg, b) = f x y in (fromIntegral r, fromIntegral gg, fromIntegral b))

-- 形が点 p をおおっていれば (色, 透明度)
covers :: Shape -> Point -> Maybe (RGB, Int)
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  Ell c x y rx ry -> hit c (((px - x) / rx) ^ (2 :: Int) + ((py - y) / ry) ^ (2 :: Int) <= 1)
  Pie c x y r a0 a1 -> hit c (dist2 p (x, y) <= r && wrapF (2 * pi) (atan2 (py - y) (px - x) - a0) <= a1 - a0)
  Seg c w a b -> hit c (segDist True a b p <= w / 2)
  RSeg c w a b -> hit c (segDist False a b p <= w / 2)
  Poly c ps -> hit c (inside ps p)
  Img x y kx ky iw ih f _ ->
    let ix = floor ((px - x) / kx)
        iy = floor ((py - y) / ky)
    in if ix >= 0 && iy >= 0 && ix < iw && iy < ih then Just (f ix iy, 255) else Nothing
  Fade a s -> fmap (\(c, b) -> (c, a * b `div` 255)) (covers s p)
  where hit c ok = if ok then Just (c, 255) else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' over bg shs
  where
    over d sh = case covers sh p of
      Nothing -> d
      Just (c, 255) -> c
      Just (c, a) -> blend a c d
    blend a (r, gg, b) (r', g2, b') = (mix r r', mix gg g2, mix b b')
      where mix s t = (s * a + t * (255 - a) + 127) `div` 255

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- 点と線分の距離（butt = True なら はしの外は遠いとみなす）
segDist :: Bool -> Point -> Point -> Point -> Float
segDist butt (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax
      dy = by - ay
      l2 = dx * dx + dy * dy
      t0 = if l2 < 1e-9 then 0 else ((px - ax) * dx + (py - ay) * dy) / l2
      t = clampF 0 1 t0
  in if butt && (t0 < 0 || t0 > 1) then 1e9 else dist2 (px, py) (ax + t * dx, ay + t * dy)

-- 多角形の内側か（偶奇の規則）
inside :: [Point] -> Point -> Bool
inside ps (x, y) = odd (length [() | ((x1, y1), (x2, y2)) <- zip ps (drop 1 ps ++ take 1 ps)
                                   , (y1 > y) /= (y2 > y), x < x1 + (y - y1) * (x2 - x1) / (y2 - y1)])

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(s, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ s)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

testColour :: [(String, Bool)] -> Color
testColour rs = if not (null rs) && all snd rs then rgb255 60 170 90 else rgb255 220 60 60

-- ---- 自己テスト：輪郭も白い面も描いていないことを画素で、口の向きを幾何で確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w0 =
  [ ("middle of the triangle = background " ++ show bgV ++ " (no white face drawn)", same (rp fig (rd cx, rd cy)))
  , ("pixels on, inside and outside the edge are all background (no contour drawn)", all (same . rp fig) [on, inn, out])
  , ("both mouth edges point exactly at the neighbouring pacmen (edges line up)", aligned)
  , ("pacman body is black (0.5 r outward from the corner)", let (r, _, _) = rp fig body in r == ink)
  , ("neon: gaps inside the circle = gaps outside = background " ++ show bgV ++ " (no blue spreads)",
       same (rp neonS gapIn) && (fst gapOut >= round (envW env) || same (rp neonS gapOut)))
  , ("neon: lines inside the circle are blue, outside black",
       rp neonS (rd cx, rd (cy + gap / 2)) == neonC && rp neonS (rd cx, rd (cy + rn + gap / 2 + 6)) == g ink) ]
  where
    w = w0 { wNeon = False }
    (cxD, cyD, _) = geom env
    cx = f cxD
    cy = f cyD
    fig = scene env w False
    neonS = scene env w { wNeon = True } False
    rp shs (x, y) = readPix (g bgV) shs x y
    rd v = floor (v + 0.5) :: Int
    same c = c == g bgV
    (on, out, inn) = edgeProbe env w
    vs = verts env w
    n = wN w
    aligned = and [ all (\a -> min (angD a (d + h)) (angD a (d - h)) < 1e-9) nb
                  | (i, v@(vx, vy)) <- zip [0 ..] vs, let (d, h) = mouth env w v
                  , let nb = [ atan2 (y - vy) (x - vx) | (x, y) <- [getAt (0, 0) vs ((i + 1) `mod` n), getAt (0, 0) vs ((i + n - 1) `mod` n)] ] ]
    angD a b = abs (atan2 (sin (a - b)) (cos (a - b)))
    body = let (vx, vy) = getAt (0, 0) vs 0
               dd = sqrt ((vx - cxD) ^ (2 :: Int) + (vy - cyD) ^ (2 :: Int))
           in (rd (f (vx + (vx - cxD) / dd * realToFrac pr * 0.5)), rd (f (vy + (vy - cyD) / dd * realToFrac pr * 0.5)))
    gapIn = (rd (cx + gap / 2), rd (cy + gap / 2))
    gapOut = (rd (cx + gap * 5.5), rd (cy + gap / 2))
