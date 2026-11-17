-- ============================================
-- 48-02 傾きの錯視 ─ ツェルナー・カフェウォール・ヘリング
-- 1 ツェルナー：平行な縦線に斜めの短い線を付けると、縦線が傾いて見える
-- 2 カフェウォール：白黒のタイルを半分ずらし、灰色の目地（モルタル）を入れると、水平な目地がくさび形に見える
-- 3 ヘリング：放射状の線の上の 2 本の水平線が、ふくらんで見える
-- 操作：1〜3 = 切替 / ←→ = 斜線の角度・ずらし・放射線の数 / ↑↓ = 補正（傾け・曲げて打ち消す）・目地の明るさ / Enter = 記録 / R = 消す
--   T またはマウスを押している間 = 真実を見る（赤い定規を重ねる）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Text.Printf (printf)

paper, ink :: Int
paper = 245                  -- 紙の明るさ
ink = 20                     -- 線の明るさ

hering :: RGB
hering = (190, 30, 40)       -- ヘリングの 2 本の線の色

-- 1 回の記録（ツェルナーは補正の角度、ヘリングは曲げた量）
data Trial = Trial { tW :: !Int, tCond :: String, tVal :: String, tNum :: !Float }

data World = World
  { wWhich  :: !Int
  , wHatch  :: !Int            -- ★ ツェルナーの斜線の角度（度）
  , wComp   :: !Float          -- ツェルナー：線を交互に傾けて打ち消す量（度）
  , wOffset :: !Int            -- ★ カフェウォールのずらし（タイル幅に対する割合を 100 倍した整数）
  , wMortar :: !Int            -- ★ 目地の明るさ（0 = 黒、255 = 白）
  , wRays   :: !Int            -- ★ ヘリングの放射線の数
  , wBend   :: !Int            -- ヘリング：線を逆に曲げて打ち消す量 px
  , wTruth  :: !Bool
  , wTrials :: [Trial]
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-02 Tilt illusions" world0)
  { skBack  = g' paper
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 30 0 30 128 36 0 False [] []

-- ---- 図をかく範囲と、図の部品の位置
data Area = Area { ax0, ay0, ax1, ay1 :: !Float }

area :: Env -> Area
area env = Area 40 90 (fromIntegral (jsRound (envW env * 0.72))) (envH env - 60)

-- ツェルナーの 7 本の縦線の x
zLines :: Env -> [Float]
zLines env = [ax0 a + gap * fromIntegral k | k <- [1 .. n]]
  where a = area env
        n = 7 :: Int
        gap = fromIntegral (jsRound ((ax1 a - ax0 a) / fromIntegral (n + 1)))

-- カフェウォールのタイルの高さと、各列の上の y
cafeTh :: Float
cafeTh = 40

cafeRows :: Env -> [Float]
cafeRows env = takeWhile (\y -> y + cafeTh < ay1 a) [ay0 a + 20, ay0 a + 20 + cafeTh + 4 ..]
  where a = area env

-- ヘリングの 2 本の線の y
heringYs :: Env -> [Float]
heringYs env = [cy - 70, cy + 70]
  where a = area env
        cy = fromIntegral (jsRound ((ay0 a + ay1 a) / 2))

scene :: Env -> World -> Bool -> [Shape]
scene env w truth = case wWhich w of
  1 -> concat [ zoellner k x | (k, x) <- zip [0 :: Int ..] (zLines env) ]
       ++ (if truth then [ Box red (x - 0.5) (ay0 a) 1 (ay1 a - ay0 a) | x <- zLines env ] else [])
  2 -> cafe ++ (if truth then [ Box red (ax0 a) (y - 2.5) (ax1 a - ax0 a) 1 | y <- drop 1 rows ] else [])
  _ -> [ Seg (g ink) 2 (cx, cy) (cx + 900 * cos t, cy + 900 * sin t) | k <- [0 .. wRays w - 1], let t = fromIntegral k / fromIntegral (wRays w) * tau ]
       ++ [ Box (g paper) 0 0 (ax0 a) h, Box (g paper) (ax1 a) 0 (envW env) h, Box (g paper) 0 0 (envW env) (ay0 a), Box (g paper) 0 (ay1 a) (envW env) h ]
       ++ concat [ path hering 6 [ (x, y + s * fromIntegral (wBend w) * (1 - u * u)) | x <- [ax0 a + 40, ax0 a + 50 .. ax1 a - 40], let u = (x - cx) / ((ax1 a - ax0 a) / 2 - 40) ]
                 | (i, y) <- zip [0 :: Int ..] (heringYs env), let s = if i == 0 then 1 else -1 ]   -- 上の線は下へ、下の線は上へ曲げる
       ++ (if truth then [ Box (40, 90, 230) (ax0 a) (y - 0.5) (ax1 a - ax0 a) 1 | y <- heringYs env ] else [])
  where
    a = area env
    h = envH env
    red = (230, 40, 40)
    cx = (ax0 a + ax1 a) / 2
    cy = (ay0 a + ay1 a) / 2
    rows = cafeRows env
    -- ツェルナー：k 番目の縦線（交互に ±comp 度傾ける）と、斜めの短い線
    zoellner k x =
      let s = if odd k then 1 else -1
          len = ay1 a - ay0 a - 40
          tilt = s * wComp w * pi / 180
          dx = tan tilt * len / 2
          ang = s * fromIntegral (wHatch w) * pi / 180
          hl = 22
      in sqSeg (g ink) 4 (x - dx, cy - len / 2) (x + dx, cy + len / 2)
         : [ sqSeg (g ink) 3 (px - hl * sin ang, py - hl * cos ang) (px + hl * sin ang, py + hl * cos ang)
           | t <- takeWhile (< len / 2) [-len / 2 + 15, -len / 2 + 45 ..], let px = x + dx * 2 * t / len, let py = cy + t ]
    -- カフェウォール：灰色の目地の上に、白黒のタイルを 1 列おきにずらして並べる
    cafe = case rows of
      [] -> []
      (r0 : _) ->
        Box (g (wMortar w)) (ax0 a) (r0 - 4) (ax1 a - ax0 a) (last rows + cafeTh + 4 - (r0 - 4))
        : concat [ [ Box (g (if odd k then 255 else 0)) xa y (xb - xa) cafeTh
                   | (k, x) <- zip [0 :: Int ..] (takeWhile (< ax1 a) [ax0 a - 2 * tw + sh, ax0 a - tw + sh ..])
                   , let xa = max (ax0 a) x, let xb = min (ax1 a) (x + tw), xb > xa ]
                 | (r, y) <- zip [0 :: Int ..] rows
                 , let sh = if odd r then fromIntegral (jsRound (fromIntegral (wOffset w) / 100 * tw)) else 0 ]
    tw = cafeTh

-- 折れ線（つなぎ目は丸い）
path :: RGB -> Float -> [Point] -> [Shape]
path c wd ps = zipWith (RSeg c wd) ps (drop 1 ps)

-- はしが四角い線分（p5.js の strokeCap(SQUARE)：太さの半分だけ両はしをのばす）
sqSeg :: RGB -> Float -> Point -> Point -> Shape
sqSeg c wd (x1, y1) (x2, y2) =
  let l = max 1e-6 (dist2 (x1, y1) (x2, y2))
      ex = (x2 - x1) / l * wd / 2
      ey = (y2 - y1) / l * wd / 2
  in Seg c wd (x1 - ex, y1 - ey) (x2 + ex, y2 + ey)

names :: Int -> String
names k = case k of { 1 -> "Zollner"; 2 -> "Cafe wall"; _ -> "Hering" }

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w (wTruth w || envDown env)) ]
  ++ [ txt 20 30 18 (g' 30) ("1-3: " ++ names (wWhich w) ++ "   " ++ ctl ++ "   Enter: record   T: ruler")
     , txt 20 56 15 (g' 90) (if wWhich w == 2 then "Try mortar 0 (black) or 255 (white). Which brightness tilts the most?"
                             else "Up/Down: tilt (bend) the other way until it looks straight/parallel, then Enter.")
     , txt 20 76 15 (g' 90) (if wWhich w == 2 then "" else "The correction you needed = the strength of the illusion.") ]
  ++ table w (fromIntegral (jsRound (envW env * 0.75))) 110
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    ctl = case wWhich w of
      1 -> printf "Left/Right: hatch angle %d deg   Up/Down: correction %.1f deg" (wHatch w) (wComp w)
      2 -> printf "Left/Right: shift %.2f   Up/Down: mortar %d" (fromIntegral (wOffset w) / 100 :: Float) (wMortar w)
      _ -> printf "Left/Right: %d rays   Up/Down: correction %d px" (wRays w) (wBend w)

-- 記録の表（最近の 12 回と平均の補正）
table :: World -> Float -> Float -> [Picture]
table w x y =
  cols (g' 30) ["Trial", "Fig", "Cond", "Corr"] y
  ++ concat [ cols (g' 70) [show k, if tW t == 1 then "Z" else "H", tCond t, tVal t] (y + 22 * fromIntegral i)
            | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT ]
  ++ [ txt x (y + 22 * 14 + (if k == 3 then 24 else 0)) 16 (g' 30) (names k ++ " mean: " ++ avg k) | k <- [1, 3] ]
  where
    n = length (wTrials w)
    lastT = drop (n - 12) (wTrials w)
    cw = [60, 50, 90, 80]
    cols c vals yy = [ txt xx yy 16 c v | (xx, v) <- zip (scanl (+) x cw) vals ]
    avg k = case [tNum t | t <- wTrials w, tW t == k] of
      [] -> "-"
      vs -> printf "%.1f" (mean vs) ++ (if k == 1 then " deg" else " px")

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wWhich = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> w { wTrials = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> adjust 1 0
  EventKey (SpecialKey KeyDown) Down _ _ -> adjust (-1) 0
  EventKey (SpecialKey KeyRight) Down _ _ -> adjust 0 1
  EventKey (SpecialKey KeyLeft) Down _ _ -> adjust 0 (-1)
  EventKey (SpecialKey KeyEnter) Down _ _
    | wWhich w == 1 -> record (Trial 1 (show (wHatch w) ++ " deg") (printf "%.1f deg" (wComp w)) (wComp w))
    | wWhich w == 3 -> record (Trial 3 (show (wRays w) ++ " rays") (show (wBend w) ++ " px") (fromIntegral (wBend w)))
  _ -> w
  where
    adjust up rt = case wWhich w of
      1 -> w { wComp = clampF (-10) 10 (wComp w + fromIntegral up * 0.5), wHatch = max 5 (min 85 (wHatch w + rt * 5)) }
      2 -> w { wMortar = max 0 (min 255 (wMortar w + up * 16)), wOffset = max 0 (min 100 (wOffset w + rt * 5)) }
      _ -> w { wBend = max (-30) (min 30 (wBend w + up)), wRays = max 4 (min 120 (wRays w + rt * 4)) }
    record t = w { wTrials = takeLast 500 (wTrials w ++ [t]), wComp = 0, wBend = 0 }

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- ---------- やってみよう ----------
-- 1. ツェルナー：斜線の角度を 10°・30°・60° にして、必要な補正を比べる
-- 2. カフェウォール：目地を 0（黒）・128（灰）・255（白）にする。くさびが消えるのはどれ？
-- 3. カフェウォール：ずらしを 0（そろえる）や 0.5（半分）にすると？
-- 4. ヘリング：放射線を 4 本まで減らすと曲がりは弱くなる？
-- 5. T の赤い定規を重ねたまま見ても、まだ傾いて見える？

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

-- ---- 自己テスト：3 つの図を描いて、線に沿って画素を読み、本当にまっすぐ・平行かを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("Zollner: the 7 vertical lines are black in the same column from top to bottom (no tilt, parallel)"
    , and [ near 30 (readPix bg zs (round x) y) (g ink) | x <- zLines env, y <- ys ])
  , ("Zollner: the gaps between the lines are all equal", and (zipWith (==) gaps (drop 1 gaps)))
  , ("Cafe wall: the " ++ show (length rows - 1) ++ " mortar lines are " ++ show (wMortar w) ++ " along the same row from left to right (horizontal)"
    , and [ readPix bg cs x (round y - 2) == g (wMortar w) | y <- drop 1 rows, x <- xs ])
  , ("Cafe wall: the tiles are only black 0 and white 255"
    , and [ let (v, _, _) = readPix bg cs x (round (y + cafeTh / 2)) in v == 0 || v == 255 | y <- rows, x <- xs ])
  , ("Hering: the two red lines are red along the same row everywhere (straight)"
    , and [ near 30 (readPix bg hs x (round y)) hering | y <- heringYs env, x <- hx ])
  , ("corrections start at 0 (nothing added to the figure)", wComp w == 0 && wBend w == 0) ]
  where
    a = area env
    bg = g paper
    zs = scene env w { wWhich = 1 } False
    cs = scene env w { wWhich = 2 } False
    hs = scene env w { wWhich = 3 } False
    rows = cafeRows env
    ys = [ jsRound (ay0 a + 40 + (ay1 a - ay0 a - 80) * fromIntegral k / 23) | k <- [0 .. 23 :: Int] ]
    xs = [ jsRound (ax0 a + 50 + (ax1 a - ax0 a - 100) * fromIntegral k / 29) | k <- [0 .. 29 :: Int] ]
    hx = [ x | x <- xs, fromIntegral x > ax0 a + 50, fromIntegral x < ax1 a - 50 ]
    gaps = zipWith (-) (drop 1 (zLines env)) (zLines env)

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
