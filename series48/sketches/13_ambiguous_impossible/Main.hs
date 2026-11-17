-- ============================================
-- 48-13 あいまい図形と不可能図形 ─ ネッカーの立方体とペンローズの三角形
-- 1 ネッカーの立方体：線だけの立方体は、奥行きの手がかりがないので 2 通りに見える（見ているうちに反転する）
-- 2 ペンローズの三角形：本当は「つながっていない」L 字に曲がった 3 本の棒。ある 1 方向から見たときだけ輪に見える
-- 操作：ドラッグ・←→↑↓ = 回す / M = 魔法の角度（ぴったりつながって見える向き）/ スペース = 自動で回す
--   F = ネッカーの見え方を色で固定（なし → 解釈 A → 解釈 B）/ T = 真実（本当の奥行き・すき間を表示）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import qualified Data.Set as S
import System.Environment (lookupEnv)
import Text.Printf (printf)

nc :: Int
nc = 5                       -- ★ ペンローズの 1 本の棒の立方体の数

-- (1, 1, 1) の方向から見る向き
magicYaw, magicPitch :: Double
magicYaw = pi / 4
magicPitch = asin (1 / sqrt 3)

data World = World
  { wWhich :: !Int
  , wYaw   :: !Double          -- 見る向き（ラジアン）
  , wPitch :: !Double
  , wAuto  :: !Bool
  , wFace  :: !Int             -- 0 = なし、1 = 解釈 A、2 = 解釈 B
  , wTruth :: !Bool
  , wDrag  :: !(Maybe Point)   -- ドラッグ中の前のマウス位置
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-13 Necker cube and Penrose triangle" w0)
  { skBack  = g' 238
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure w0 { wTest = rs }
  , skStep  = \_ _ w -> pure (if wAuto w then w { wYaw = wYaw w + 0.006 } else w)
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = World 2 magicYaw magicPitch False 0 False Nothing []

-- ---- 3D → 画面（正射影：遠くても小さくならない）。計算は Double で正確に
type D3 = (Double, Double, Double)
data Basis = Basis { bD, bR, bU :: !D3 }      -- 見る人の方向・右・上

basis :: Double -> Double -> Basis
basis yw pt = Basis (cos pt * cos yw, cos pt * sin yw, sin pt) (-sin yw, cos yw, 0) (-sin pt * cos yw, -sin pt * sin yw, cos pt)

dot3' :: D3 -> D3 -> Double
dot3' (a, b, c) (d, e, f') = a * d + b * e + c * f'

-- 画面の (x, y) と奥行き z（大きいほど手前）
projectD :: D3 -> Basis -> Double -> Double -> Double -> D3
projectD p b sc cx cy = (cx + sc * dot3' p (bR b), cy - sc * dot3' p (bU b), dot3' p (bD b))

comp :: Int -> D3 -> Double
comp a (x, y, z) = case a of { 0 -> x; 1 -> y; _ -> z }

-- ペンローズ：x 方向 → y 方向 → z 方向に nc 個ずつ並べた立方体（最初の立方体は (0,0,0)）
chainCubes :: [D3]
chainCubes = go (0, 0, 0) [ a | a <- [0, 1, 2 :: Int], _ <- [1 .. nc] ]
  where
    go _ [] = []
    go p@(x, y, z) (a : as) = p : go (case a of { 0 -> (x + 1, y, z); 1 -> (x, y + 1, z); _ -> (x, y, z + 1) }) as
    -- 最後の立方体の次の位置は (nc, nc, nc) = 最初の位置 + (nc, nc, nc)

cubeCorners :: D3 -> [D3]
cubeCorners (x, y, z) = [ (x + fromIntegral (i `mod` 2), y + fromIntegral ((i `div` 2) `mod` 2), z + fromIntegral ((i `div` 4) `mod` 2)) | i <- [0 .. 7 :: Int] ]

draw :: Env -> World -> Picture
draw env w = view env $
  (if wWhich w == 1 then necker env w b cx cy else penrose env w b cx cy)
  ++ [ txt 20 30 18 (g' 20) ("1/2: " ++ (if wWhich w == 1 then "Necker cube" else "Penrose triangle") ++ "   Drag/arrows: rotate   M: magic angle" ++ (if magic then " (now)" else ""))
     , txt 20 54 18 (g' 20) ("Space: auto " ++ (if wAuto w then "ON" else "off") ++ (if wWhich w == 1 then "   F: fix the view" else "") ++ "   T: truth")
     , txt 20 78 15 (g' 80) (if wWhich w == 1 then "Lines only: either square can be the front. F colours one face and it tends to come forward."
                             else "Rotate a little: it is not a closed ring (there is a gap). M brings it back.")
     , txt 20 100 15 (g' 80) (printf "view: yaw %.1f deg, pitch %.1f deg" (wYaw w * 180 / pi) (wPitch w * 180 / pi))
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    b = basis (wYaw w) (wPitch w)
    cx = realToFrac (envW env) * 0.45
    cy = realToFrac (envH env) * 0.55
    magic = abs (wYaw w - magicYaw) < 1e-3 && abs (wPitch w - magicPitch) < 1e-3

penrose :: Env -> World -> Basis -> Double -> Double -> [Picture]
penrose env w b cx cy =
  [ render (concatMap cubeFaces order) ]
  ++ (if wTruth w then truthPics else [])
  where
    sc = realToFrac (min (envW env) (envH env)) / (fromIntegral nc * 2.3)
    cubes = chainCubes
    nC = fromIntegral (length cubes)
    off = [ sum [ comp a c + 0.5 | c <- cubes ] / nC | a <- [0, 1, 2] ]      -- 立方体の重心を画面の中心に
    shift (x, y, z) = case off of { [ox, oy, oz] -> (x - ox, y - oy, z - oz); _ -> (x, y, z) }
    pr' p = projectD (shift p) b sc cx cy
    xy (x, y, _) = (f x, f y)
    -- 奥から手前の順に描く（画家のアルゴリズム）
    order = sortOn (\(x, y, z) -> dot3' (x + 0.5, y + 0.5, z + 0.5) (bD b)) cubes
    faces = [ ([1, 3, 7, 5], [0, 4, 6, 2], (240, 120, 60)), ([2, 6, 7, 3], [0, 1, 5, 4], (250, 200, 70)), ([4, 5, 7, 6], [0, 2, 3, 1], (255, 240, 200)) ]   -- +x, +y, +z の面
    cubeFaces c =
      let k = map (xy . pr') (cubeCorners c)
      in concat [ Poly col' pts : [ Seg (g 60) 1.2 p q | (p, q) <- zip pts (drop 1 pts ++ take 1 pts) ]
                | (a, (ip, ineg, col)) <- zip [0 ..] faces
                , let pos = comp a (bD b) > 0
                      col' = if pos then col else dim col
                      pts = [ getAt (0, 0) k j | j <- if pos then ip else ineg ] ]
    dim (r, gg, bb) = (jsRound (fromIntegral r * 0.7), jsRound (fromIntegral gg * 0.7), jsRound (fromIntegral bb * 0.7))
    a0 = pr' (0, 0, 0)
    e0 = pr' (fromIntegral nc, fromIntegral nc, fromIntegral nc)
    (ax, ay) = xy a0
    (ex, ey) = xy e0
    redC = rgb255 220 30 30
    bt = basis magicYaw 1.2
    x0 = realToFrac (envW env) - 160
    y0 = 190
    path = [ xy (projectD (shift c) bt 14 x0 y0) | c <- cubes ++ [(fromIntegral nc, fromIntegral nc, fromIntegral nc)] ]
    truthPics =
      [ Color redC (at (ax, ay) (ring 11 3)), Color redC (at (ex, ey) (ring 17 3)), Color redC (thickLine 3 (ax, ay) (ex, ey))
      , txt 20 (envH env - 44) 15 redC (printf "first and last corner: %.1f px apart on screen, %.2f apart in 3D (%d cubes)"
                                          (dist2 (ax, ay) (ex, ey)) (fromIntegral nc * sqrt 3 :: Double) (nc * 3))
      , Color (g' 255) (rect (f x0) (f y0) 240 220), Color (g' 180) (rectLine (f x0) (f y0) 240 220)
      , Color (rgb255 30 90 200) (thickPath 3 path)
      , txtC (f x0) (f y0 + 96) 14 (g' 30) "Other angle: not joined" ]

necker :: Env -> World -> Basis -> Double -> Double -> [Picture]
necker env w b cx cy =
  [ render (faceFill ++ edgeShapes) ]
  ++ (if wTruth w then [ txt 20 (envH env - 44) 15 (rgb255 220 30 30) "Truth: thicker and darker = nearer (real 3D depth). Flip the depth: the line drawing is identical." ] else [])
  where
    sc = realToFrac (min (envW env) (envH env)) * 0.2
    vs = [ projectD p b sc cx cy | p <- cubeCorners (-0.5, -0.5, -0.5) ]
    vAt = getAt (0, 0, 0) vs
    xy (x, y, _) = (f x, f y)
    edges = [ (i, i + bit) | i <- [0 .. 7 :: Int], bit <- [1, 2, 4], (i `div` bit) `mod` 2 == 0 ]
    -- F：2 つの解釈のどちらかの「手前の面」を色で示す（同じ線の絵のまま）
    faceFill
      | wFace w == 0 = []
      | otherwise =
          let ax = foldl' (\m a -> if abs (comp a (bD b)) > abs (comp m (bD b)) then a else m) 0 [0, 1, 2]
              bit = 2 ^ ax :: Int
              nearSide = comp ax (bD b) > 0
              side = if wFace w == 1 then nearSide else not nearSide
              fa = [ i | i <- [0 .. 7], ((i `div` bit) `mod` 2 == 1) == side ]
              fa' = [ getAt 0 fa 0, getAt 0 fa 1, getAt 0 fa 3, getAt 0 fa 2 ]
          in [ Fade 90 (Poly (80, 140, 240) [ xy (vAt j) | j <- fa' ]) ]
    edgeShapes =
      [ if wTruth w
          then let (_, _, z1) = vAt i
                   (_, _, z2) = vAt j
                   z = f ((z1 + z2) / 2)
                   gv = max 0 (min 255 (jsRound (remap z (-0.9) 0.9 190 20)))
               in RSeg (g gv) (max 0.5 (remap z (-0.9) 0.9 1 8)) (xy (vAt i)) (xy (vAt j))
          else RSeg (g 30) 3 (xy (vAt i)) (xy (vAt j))
      | (i, j) <- edges ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' -> w { wWhich = 1, wYaw = 0.45, wPitch = 0.35 }
    | c == '2' -> w { wWhich = 2, wYaw = magicYaw, wPitch = magicPitch }
    | c `elem` "mM" -> w { wYaw = magicYaw, wPitch = magicPitch, wAuto = False }
    | c `elem` "fF" -> w { wFace = (wFace w + 1) `mod` 3 }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wYaw = wYaw w - 0.087 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wYaw = wYaw w + 0.087 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wPitch = min 1.5 (wPitch w + 0.087) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wPitch = max (-1.5) (wPitch w - 0.087) }
  EventKey (MouseButton LeftButton) Down _ p -> w { wDrag = Just p }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventMotion p@(mx, my) -> case wDrag w of
    Just (px, py) | envDown env ->
      -- 画面の y は上向きなので、p5.js の (mouseY − pmouseY) は −(my − py)
      w { wYaw = wYaw w - realToFrac (mx - px) * 0.008, wPitch = max (-1.5) (min 1.5 (wPitch w - realToFrac (my - py) * 0.008)), wDrag = Just p }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. ネッカーの立方体をじっと見て、何秒ごとに反転するか数える
-- 2. ペンローズで ← を 1 回だけ押す。どこにすき間が現れる？
-- 3. nc を 3 や 8 にしても、魔法の角度ではつながって見える？（(nc, nc, nc) はいつも視線の方向）
-- 4. 魔法の角度で T：画面の上では 0 px なのに、3D では離れている
-- 5. エッシャーのような「上り続ける階段」も、同じ考え方で作れる。挑戦しよう

f :: Double -> Float
f = realToFrac

getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- ---- 自己テスト：射影の計算（画面に描くのと同じ関数）で「つながって見える」「2 通りに見える」理由を確かめる
selfTest :: [(String, Bool)]
selfTest =
  [ (printf "magic angle: first and last corner are the same screen point (%.1e px)" gapM, gapM < 1e-9)
  , (printf "but apart in 3D: distance %.2f (depth difference %.0f)" (fromIntegral nc * sqrt 3 :: Double) (ez - az), abs (ez - az) > 1)
  , (printf "rotate a little (5.7 deg): a gap of %.0f px appears" gapR, gapR > 20)
  , ("Penrose = 3 bars x " ++ show nc ++ " cubes, not a ring (no cubes overlap)", length cubes == 3 * nc && S.size (S.fromList cubes) == length cubes)
  , ("Necker: the depth-flipped cube gives exactly the same line drawing (orthographic)", all neckerSame (cubeCorners (-0.5, -0.5, -0.5)))
  , ("view vector has length 1 and is perpendicular to right and up",
       abs (dot3' (bD bb) (bD bb) - 1) < 1e-12 && abs (dot3' (bD bb) (bR bb)) < 1e-12 && abs (dot3' (bD bb) (bU bb)) < 1e-12 && abs (dot3' (bR bb) (bU bb)) < 1e-12) ]
  where
    ncD = fromIntegral nc
    bm = basis magicYaw magicPitch
    (ax, ay, az) = projectD (0, 0, 0) bm 100 0 0
    (ex, ey, ez) = projectD (ncD, ncD, ncD) bm 100 0 0
    gapM = sqrt ((ax - ex) ^ (2 :: Int) + (ay - ey) ^ (2 :: Int))
    bo = basis (magicYaw + 0.1) magicPitch
    (ax2, ay2, _) = projectD (0, 0, 0) bo 100 0 0
    (ex2, ey2, _) = projectD (ncD, ncD, ncD) bo 100 0 0
    gapR = sqrt ((ax2 - ex2) ^ (2 :: Int) + (ay2 - ey2) ^ (2 :: Int))
    cubes = chainCubes
    bb = basis 0.4 0.3
    mirror p@(x, y, z) = let k = 2 * dot3' p (bD bb); (dx, dy, dz) = bD bb in (x - k * dx, y - k * dy, z - k * dz)
    neckerSame p = let (qx, qy, qz) = projectD p bb 100 0 0
                       (mx, my, mz) = projectD (mirror p) bb 100 0 0
                   in abs (qx - mx) < 1e-9 && abs (qy - my) < 1e-9 && abs (qz + mz) < 1e-9
