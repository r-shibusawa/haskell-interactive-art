-- ============================================
-- 48-05 影の中のチェッカー（明るさの恒常性）
-- 床の白いマスに円柱の影がかかる。影の中の白いマス A と、日なたの黒いマス B は、画面の上ではまったく同じ灰色
-- 影の濃さを「黒マス ÷ 白マス」から計算しているので、A と B は 1 の位までぴったり同じ（Adelson の原理を使ったオリジナルの図）
-- 操作：←→ = 黒マスの明るさ（影の濃さも自動で変わる） / H = 影を消す / ↑↓ = 右の見本の灰色 / Enter = 記録 / R = 消す
--   T またはマウスを押している間 = 真実を見る（A と B を同じ灰色の帯でつなぐ）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import System.Environment (lookupEnv)
import Text.Printf (printf)

lightV :: Int
lightV = 200                  -- ★ 白いマスの明るさ

tile :: Double
tile = 0.8                   -- ★ マスの大きさ m

tA, tB :: (Int, Int)
tA = (1, 4)                  -- 影の中の白マス A（マスの番号）
tB = (-2, 4)                 -- 日なたの黒マス B

cyl, shEnd :: (Double, Double)
cyl = (3.6, 6.2)             -- 円柱の位置（マス単位）
shEnd = (0.6, 3.6)           -- 影の先（マス単位）

kImg :: Int
kImg = 4                     -- 床の画像の 1 画素 = 画面の 4 × 4 px

data Trial = Trial { tP :: !Int, tReal :: !Int }

data World = World
  { wDark   :: !Int            -- ★ 黒いマスの明るさ（←→）
  , wShadow :: !Bool           -- 影を描くか
  , wTruth  :: !Bool
  , wProbe  :: !Int            -- 見本の灰色
  , wFloor  :: Shape           -- 床の画像（パラメータを変えたときだけ計算し直す）
  , wSize   :: !(Float, Float)
  , wTrials :: [Trial]
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-05 Checker shadow" (world0 1280 800))
  { skBack  = g' 220
  , skStart = \env -> do
      let w = world0 (envW env) (envH env)
          rs = selfTest env w
      printSelfTest rs
      pure w { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> Float -> World
world0 sw sh = rebuild (World 100 True False 150 (Box (0, 0, 0) 0 0 0 0) (sw, sh) [] [])

-- 影の中の明るさの倍率 = 黒 ÷ 白（自動で計算）
shadowK :: World -> Double
shadowK w = fromIntegral (wDark w) / fromIntegral lightV

-- ---- カメラ：床の点 (X, 高さ Y, 距離 z) → 画面
data Cam = Cam { cCx, cVy, cF, cHc :: !Double }

cam :: (Float, Float) -> Cam
cam (sw, sh) = Cam (fromIntegral (jsRound (sw * 0.35))) (fromIntegral (jsRound (sh * 0.18))) (realToFrac sh * 0.9) 1.6

proj :: Cam -> Double -> Double -> Double -> (Double, Double)
proj c x y z = (cCx c + cF c * x / z, cVy c + cF c * (cHc c - y) / z)

-- マスの真ん中の画素（整数）
tileCenter :: World -> (Int, Int) -> (Int, Int)
tileCenter w (i, j) =
  let (x, y) = proj (cam (wSize w)) ((fromIntegral i + 0.5) * tile) 0 ((fromIntegral j + 0.5) * tile)
  in (jsRoundD x, jsRoundD y)

-- 影の強さ s（0 = 日なた、1 = 影の中心）：円柱から影の先までの線分からの距離で決める
shadowAt :: Double -> Double -> Double
shadowAt u v =
  let (cx, cy) = cyl
      vx = fst shEnd - cx
      vy = snd shEnd - cy
      l = vx * vx + vy * vy
      t = max 0 (min 1 (((u - cx) * vx + (v - cy) * vy) / l))
      d = sqrt ((u - cx - t * vx) ^ (2 :: Int) + (v - cy - t * vy) ^ (2 :: Int))
  in if d < 1.0 then 1 else if d < 1.4 then 1 - (d - 1.0) / 0.4 else 0   -- 本影（ぴったり 1）＋ ぼかし

-- 画面の画素 (x, y) の床の明るさ（整数で計算）
floorVal :: World -> Int -> Int -> Int
floorVal w x y
  | fromIntegral y <= cVy c = 215 + jsRoundD (20 * fromIntegral y / cVy c)          -- 奥の壁
  | otherwise =
      let z = cF c * cHc c / (fromIntegral y - cVy c)
          xx = (fromIntegral x - cCx c) * z / cF c
          u = xx / tile
          v = z / tile
          base = if odd (floor u + floor v :: Int) then lightV else wDark w
          s = if wShadow w then shadowAt u v else 0
          v8 = jsRoundD (fromIntegral base * (1 - (1 - shadowK w) * s))          -- 影 = 明るさに倍率をかける
      in if z > 14 then jsRoundD (fromIntegral v8 + fromIntegral (225 - v8) * min 1 ((z - 14) / 30)) else v8   -- 遠くはかすむ
  where c = cam (wSize w)

-- 床を 1 画素ずつ計算して画像にする（4 × 4 px ごとに、その真ん中の画素の値）
rebuild :: World -> World
rebuild w =
  let (sw, sh) = wSize w
      iw = ceiling (sw / fromIntegral kImg) :: Int
      ih = ceiling (sh / fromIntegral kImg) :: Int
      vals = U.generate (iw * ih) (\i -> let (j, k) = i `divMod` iw in fromIntegral (floorVal w (k * kImg + kImg `div` 2) (j * kImg + kImg `div` 2)) :: Word8)
      f i j = let v = fromIntegral (vals U.! (j * iw + i)) in (v, v, v)
      pic = pixelImage iw ih (fromIntegral kImg) (\i j -> let v = vals U.! (j * iw + i) in (v, v, v))
  in vals `seq` w { wFloor = Img 0 0 (fromIntegral kImg) iw ih f pic }

-- 床の画像の画素（p5.js の pixAt）
pixAt :: World -> (Int, Int) -> RGB
pixAt w (x, y) = readPix (0, 0, 0) [wFloor w] x y

scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  [wFloor w] ++ cylinder w
  ++ (if truth then [ Box (g (wDark w)) (ax - 8) ay 16 (yb - ay), Box (g (wDark w)) (bx - 8) by 16 (yb - by), Box (g (wDark w)) (bx - 8) (yb - 8) (ax - bx + 16) 16 ] else [])
  ++ [ Box (g 180) (px - 20) (py - 20) 160 160, Box (g (wProbe w)) (px + 10) (py + 10) 100 100 ]   -- 見本の灰色（調整法）
  where
    (ax, ay) = fl (tileCenter w tA)
    (bx, by) = fl (tileCenter w tB)
    yb = max ay by + 70
    px = envW env - 190
    py = fromIntegral (jsRound (envH env * 0.5))
    fl (x, y) = (fromIntegral x, fromIntegral y)

-- 円柱（影を落とす物）。光は右奥から
cylinder :: World -> [Shape]
cylinder w =
  [ Ell (60, 66, 90) bx by r ry ]                  -- 底（手前半分が見える）
  ++ [ Box (jsRound (gg * 0.75), jsRound (gg * 0.8), jsRound gg) (bx + k) ty 2.5 (by - ty)
     | k <- takeWhile (< r) [-r, -r + 2 ..], let gg = 70 + 110 * max 0 ((k + r * 0.3) / (r * 1.3)) ]
  ++ [ Ell (190, 200, 230) tx ty r ry ]
  where
    c = cam (wSize w)
    z = snd cyl * tile
    xx = fst cyl * tile
    (bx, by) = toF (proj c xx 0 z)
    (tx, ty) = toF (proj c xx 1.5 z)
    r = realToFrac (0.35 * cF c / z)
    ry = r * 0.28
    toF (a, b) = (realToFrac a, realToFrac b)

yellow' :: Color
yellow' = rgb255 255 220 40

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w truth) ]
  -- マスの左手前の隅に名前
  ++ [ txtC (realToFrac qx) (realToFrac qy + 11) 22 yellow' nm
     | ((i, j), nm) <- [(tA, "A"), (tB, "B")], let (qx, qy) = proj (cam (wSize w)) ((fromIntegral i + 0.18) * tile) 0 ((fromIntegral j + 0.2) * tile) ]
  ++ (if truth then [ txtC (fromIntegral ax) (yb + 32) 16 (rgb255 255 230 60) ("A pixel " ++ showRGB (pixAt w (ax, ay)))
                    , txtC (fromIntegral bx) (yb + 32) 16 (rgb255 255 230 60) ("B pixel " ++ showRGB (pixAt w (bx, by))) ]
               else [])
  ++ [ txtC (px + 60) (py + 160) 14 (g' 30) ("probe " ++ show (wProbe w))
     , txt 20 30 18 (g' 30) (printf "Left/Right: dark tile %d (shadow factor %.3f)   H: shadow %s" (wDark w) (shadowK w) (if wShadow w then "ON" else "OFF"))
     , txt 20 54 18 (g' 30) "Up/Down: probe grey   Enter: record   R: clear   T or hold mouse: truth"
     , txt 20 78 15 (g' 60) (printf "Shadowed white tile A = white %d x %.3f = %d, sunlit dark tile B = %d." lightV (shadowK w) (jsRoundD (fromIntegral lightV * shadowK w)) (wDark w))
     , txt 20 98 15 (g' 60) "Set the probe on the right to look as bright as A, then press Enter." ]
  ++ table w (envW env - 300) 130
  ++ [ Color (rgba255 255 255 255 220) (rect 85 (envH env - 22) 150 28)
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    (ax, ay) = tileCenter w tA
    (bx, by) = tileCenter w tB
    yb = fromIntegral (max ay by + 70) :: Float
    px = envW env - 190
    py = fromIntegral (jsRound (envH env * 0.5))

showRGB :: RGB -> String
showRGB (r, gg, b) = show r ++ "," ++ show gg ++ "," ++ show b

-- 記録の表
table :: World -> Float -> Float -> [Picture]
table w x y =
  [ txt x y 15 (g' 30) "Trial  Probe  A real  Diff" ]
  ++ [ txt x (y + 22 * fromIntegral i) 15 (g' 60) (printf "%3d    %3d    %3d    %s" k (tP t) (tReal t) ((if d > 0 then "+" else "") ++ show d))
     | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT, let d = tP t - tReal t ]
  ++ [ txt x (y + 22 * 9 + 4) 15 (g' 30) (printf "Mean diff %.1f" (mean [fromIntegral (tP t - tReal t) | t <- wTrials w])) | n > 0 ]
  where
    n = length (wTrials w)
    lastT = drop (n - 8) (wTrials w)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wDark = min (lightV - 10) (wDark w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wDark = max 20 (wDark w - 10) }
  EventKey (Char c) Down _ _
    | c `elem` "hH" -> rebuild w { wShadow = not (wShadow w) }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> w { wTrials = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wProbe = min 255 (wProbe w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wProbe = max 0 (wProbe w - 2) }
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    let (r, _, _) = pixAt w (tileCenter w tA)
    in w { wTrials = takeLast 500 (wTrials w ++ [Trial (wProbe w) r]), wProbe = 150 }
  _ -> w

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

jsRoundD :: Double -> Int
jsRoundD x = floor (x + 0.5)

-- ---------- やってみよう ----------
-- 1. 見本を A に合わせて何回か記録。A の本当の値（= B）より明るく選んでしまう？
-- 2. T で帯をつなぐと、A と B が同じ色に見える瞬間がある
-- 3. H で影を消すと、A は白 200 に戻る。影の中の 200 × 倍率 が、B と同じ値になっていた
-- 4. ←→ で黒マスを明るくすると影は薄くなる。いつでも A = B が保たれる（計算で決めているから）
-- 5. shadowAt の 1.4（ぼかしの幅）を 3 にすると、影のふちがぼやけて錯視は強くなる？

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

takeLast :: Int -> [a] -> [a]
takeLast k xs = drop (length xs - k) xs

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
  | Seg !RGB !Float !Point !Point                 -- 太さ w の線分（はしは四角）
  | RSeg !RGB !Float !Point !Point                -- 太さ w の線分（はしは丸い）
  | Poly !RGB [Point]                             -- 凸多角形
  | Ell !RGB !Float !Float !Float !Float          -- 中心 (x, y)・横の半径・縦の半径 のだ円
  | Img !Float !Float !Float !Int !Int (Int -> Int -> RGB) Picture  -- 左上 (x, y)・倍率 k・幅・高さ の画像（作った絵を覚えておく）
  | Fade !Int Shape                               -- 透明度 0〜255 をつける

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map one
  where
    one sh = case sh of
      Fade a s -> recolour a s
      _ -> recolour 255 sh
    recolour a sh = case sh of
      Box c x y w h -> Color (col a c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col a c) (at (x, y) (disc r))
      Seg c w p q -> Color (col a c) (thickLine w p q)
      RSeg c w p@(px, py) q@(qx, qy) -> Color (col a c) (Pictures [thickLine w p q, at (px, py) (disc (w / 2)), at (qx, qy) (disc (w / 2))])
      Poly c ps -> Color (col a c) (Polygon ps)
      Ell c x y rx ry -> Color (col a c) (Translate x y (Scale 1 (ry / max 1e-6 rx) (circleSolid rx)))
      Img x y k w h _ pic -> Translate (x + fromIntegral w * k / 2) (y + fromIntegral h * k / 2) (Scale 1 (-1) pic)
      Fade b s -> recolour (a * b `div` 255) s
    col a (r, gg, b) = rgba255 r gg b a

-- 形が点 p をおおっていれば (色, 透明度)
covers :: Shape -> Point -> Maybe (RGB, Int)
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  Seg c w a b -> hit c (segDist True a b p <= w / 2)
  RSeg c w a b -> hit c (segDist False a b p <= w / 2)
  Poly c ps -> hit c (inside ps p)
  Ell c x y rx ry -> hit c (((px - x) / rx) ^ (2 :: Int) + ((py - y) / ry) ^ (2 :: Int) <= 1)
  Img x y k w h f _ ->
    let i = floor ((px - x) / k)
        j = floor ((py - y) / k)
    in if i >= 0 && i < w && j >= 0 && j < h then Just (f i j, 255) else Nothing
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

-- 高さ y の行で、色が c に近い画素が続く範囲の長さ（x の範囲 xa〜xb。p5.js の inkRun）
inkRun :: RGB -> [Shape] -> Int -> Int -> Int -> RGB -> Int -> Int
inkRun bg shs y xa xb c tol =
  case [x | x <- [max 0 xa .. xb], near tol (readPix bg shs x y) c] of
    [] -> 0
    xs@(x0 : _) -> last xs - x0 + 1

near :: Int -> RGB -> RGB -> Bool
near tol (a, b, c) (d, e, f) = abs (a - d) <= tol && abs (b - e) <= tol && abs (c - f) <= tol

-- ---- 自己テスト：A と B の画素がぴったり同じか、影の倍率の計算、A が本影・B が日なたにあるか
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("screen pixels: A " ++ showRGB pa ++ " = B " ++ showRGB pb, pa == pb)
  , ("shadow factor = dark / white = " ++ show (wDark w) ++ " / " ++ show lightV ++ ", white x factor = dark (exactly)"
    , jsRoundD (fromIntegral lightV * shadowK w) == wDark w && pa == g (wDark w))
  , ("the 4 corners of A are all in full shadow (s = 1), the 4 corners of B all in the sun (s = 0)"
    , all (\(u, v) -> shadowAt u v == 1) (corners tA) && all (\(u, v) -> shadowAt u v == 0) (corners tB))
  , ("A is a white tile, B is a dark tile", odd (fst tA + snd tA) && even (fst tB + snd tB))
  , ("dark tile 40, 70, 130, 170 -> still A = B (read from the rebuilt floor image each time)"
    , and [ let w' = rebuild w { wDark = dv } in pixAt w' (tileCenter w' tA) == g dv && pixAt w' (tileCenter w' tB) == g dv | dv <- [40, 70, 130, 170] ])
  , ("tile A is uniform (centre and near a corner have the same value)", pixAt w q == pixAt w a) ]
  where
    sc = scene env w False
    a = tileCenter w tA
    b = tileCenter w tB
    pa = uncurry (readPix (0, 0, 0) sc) a
    pb = uncurry (readPix (0, 0, 0) sc) b
    corners (i, j) = [(fromIntegral i, fromIntegral j), (fromIntegral i + 1, fromIntegral j), (fromIntegral i, fromIntegral j + 1), (fromIntegral i + 1, fromIntegral j + 1)]
    q = let (x, y) = proj (cam (wSize w)) ((fromIntegral (fst tA) + 0.15) * tile) 0 ((fromIntegral (snd tA) + 0.15) * tile) in (jsRoundD x, jsRoundD y)

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
testColour rs = if not (null rs) && all snd rs then rgb255 40 140 70 else rgb255 200 40 40
