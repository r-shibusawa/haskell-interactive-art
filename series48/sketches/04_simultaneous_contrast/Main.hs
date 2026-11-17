-- ============================================
-- 48-04 明るさの同時対比
-- 同じ灰色でも、暗い背景の上では明るく、明るい背景の上では暗く見える（となりとの「差」を見ている）
-- 1 = 左右の背景　2 = グラデーションの上の帯　3 = ホワイト錯視（黒白のしまに灰色を埋めこむ）
-- 操作：↑↓ = 右の灰色の明るさ（比較） / ←→ = 背景のコントラスト / Enter = 記録 / R = 消す
--   T またはマウスを押している間 = 真実を見る（背景を消す）　B = 2 つの灰色を同じ色の帯でつなぐ
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Text.Printf (printf)

paper :: Int
paper = 200                  -- 紙の明るさ

g0 :: Int
g0 = 128                     -- ★ 左の灰色（標準）。0〜255

sw :: Float
sw = 14                      -- ★ ホワイト錯視のしまの幅 px

-- 1 回の記録：図、コントラスト、差（右 − 左）
data Trial = Trial { tWhich :: !Int, tC :: !Int, tDiff :: !Int }

data World = World
  { wWhich    :: !Int
  , wCmp      :: !Int          -- 右の灰色（比較）。最初は g0 と同じ
  , wContrast :: !Int          -- ★ 背景のコントラスト（100 倍した整数。0 = 一様、100 = 黒と白）
  , wTruth    :: !Bool
  , wBridge   :: !Bool
  , wTrials   :: [Trial]
  , wTest     :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-04 Simultaneous contrast" world0)
  { skBack  = g' paper
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 g0 85 False False [] []

data Area = Area { ax0, ay0, ax1, ay1 :: !Float }

area :: Env -> Area
area env = Area 40 100 (fromIntegral (jsRound (envW env * 0.7))) (envH env - 70)

-- 背景の暗い側・明るい側の値
bgDark, bgLight :: World -> Int
bgDark w = jsRound (128 - 125 * fromIntegral (wContrast w) / 100)
bgLight w = jsRound (128 + 125 * fromIntegral (wContrast w) / 100)

-- 2 つの的（標準・比較）の中心と大きさ：((sx, sy), (cx, cy), r)
targets :: Env -> World -> ((Float, Float), (Float, Float), Float)
targets env w
  | wWhich w == 3 = ((ax0 a + rnd (wd * 0.25), cy), (ax0 a + rnd (wd * 0.75), cy), 0)
  | otherwise = ((ax0 a + rnd (wd * q), cy), (ax1 a - rnd (wd * q), cy), 60)
  where
    a = area env
    cy = rnd ((ay0 a + ay1 a) / 2)
    wd = ax1 a - ax0 a
    q = if wWhich w == 1 then 0.25 else 0.12
    rnd = fromIntegral . jsRound

scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  (case wWhich w of
     1 -> (if truth then [] else [Box (g (bgDark w)) (ax0 a) (ay0 a) (wd / 2) h, Box (g (bgLight w)) (ax0 a + wd / 2) (ay0 a) (wd - wd / 2) h])
          ++ [patch s r g0, patch c r (wCmp w)]
     2 -> (if truth then [] else
             [ Box (g (jsRound (fromIntegral (bgDark w) + fromIntegral (bgLight w - bgDark w) * (x - ax0 a) / wd))) x (ay0 a) 4 h
             | x <- takeWhile (< ax1 a) [ax0 a, ax0 a + 4 ..] ])
          ++ [ Box (g g0) (fst s - 40) (snd s - 30) (mid - (fst s - 40)) 60          -- 左半分：一様な灰色 g0 の帯
             , Box (g (wCmp w)) mid (snd c - 30) (fst c + 40 - mid) 60 ]           -- 右半分：比較（最初は g0）
     -- ホワイト錯視：黒白のしま。左は黒いしまの一部、右は白いしまの一部を灰色に置きかえる
     _ -> concat [ (if truth then [] else [Box (g (if dark then bgDark w else bgLight w)) x (ay0 a) sw h])
                   ++ [ Box (g g0) x yb0 sw (yb1 - yb0) | left, dark, abs (x + sw / 2 - fst s) < wd * 0.15 ]
                   ++ [ Box (g (wCmp w)) x yb0 sw (yb1 - yb0) | not left, not dark, abs (x + sw / 2 - fst c) < wd * 0.15 ]
                 | k <- [0 .. floor (wd / sw) - 1 :: Int]
                 , let x = ax0 a + fromIntegral k * sw, let dark = even k, let left = x + sw / 2 < ax0 a + wd / 2 ])
  ++ [ Box (g g0) (fst s) (snd s - 6) (fst c - fst s) 12 | wBridge w ]
  where
    a = area env
    wd = ax1 a - ax0 a
    h = ay1 a - ay0 a
    (s, c, r) = targets env w
    mid = ax0 a + fromIntegral (jsRound (wd / 2))
    yb0 = snd s - 70
    yb1 = snd s + 70
    patch (px, py) rr v = Box (g v) (px - rr) (py - rr) (2 * rr) (2 * rr)

figName :: Int -> String
figName k = case k of { 1 -> "Sides"; 2 -> "Gradient"; _ -> "White" }

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w (wTruth w || envDown env)) ]
  ++ [ txt 20 30 18 (g' 20) (printf "1-3: %s   Up/Down: right grey %d   Left/Right: background contrast %.2f" (figName (wWhich w)) (wCmp w) (fromIntegral (wContrast w) / 100 :: Float))
     , txt 20 54 18 (g' 20) ("Enter: record   R: clear   T or hold mouse: hide background   B: bridge " ++ (if wBridge w then "ON" else "off"))
     , txt 20 80 15 (g' 60) (printf "Left grey = %d (RGB %d, %d, %d). Enter when the right looks the same; the true difference is recorded." g0 g0 g0 g0) ]
  ++ table w (fromIntegral (jsRound (envW env * 0.73))) 120
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]

-- 記録の表（最近の 10 回と、図ごとの平均の差）
table :: World -> Float -> Float -> [Picture]
table w x y =
  cols (g' 20) ["Trial", "Figure", "Contr.", "Diff"] y
  ++ concat [ cols (g' 50) [show k, figName (tWhich t), printf "%.2f" (fromIntegral (tC t) / 100 :: Float), (if tDiff t > 0 then "+" else "") ++ show (tDiff t)] (y + 24 * fromIntegral i)
            | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT ]
  ++ [ txt x (y + 24 * 11 + fromIntegral k * 24) 15 (g' 20) (figName k ++ ": mean diff " ++ avg k) | k <- [1 .. 3] ]
  ++ [ txt x (y + 24 * 16) 14 (g' 60) "Diff = right - left: the brightness"
     , txt x (y + 24 * 16 + 20) 14 (g' 60) "needed to cancel the illusion (0-255)" ]
  where
    n = length (wTrials w)
    lastT = drop (n - 10) (wTrials w)
    cw = [56, 90, 74, 60]
    cols c vals yy = [ txt xx yy 16 c v | (xx, v) <- zip (scanl (+) x cw) vals ]
    avg k = case [fromIntegral (tDiff t) | t <- wTrials w, tWhich t == k] of
      [] -> "-"
      ds -> printf "%.1f (%d)" (mean ds) (length ds)

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wWhich = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> pure w { wTruth = not (wTruth w) }
    | c `elem` "bB" -> pure w { wBridge = not (wBridge w) }
    | c `elem` "rR" -> pure w { wTrials = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wCmp = min 255 (wCmp w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wCmp = max 0 (wCmp w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wContrast = min 100 (wContrast w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wContrast = max 0 (wContrast w - 5) }
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    r <- randomRIO (-0.5, 0.5 :: Float)
    let t = Trial (wWhich w) (wContrast w) (wCmp w - g0)
    -- 次の試行は少しずらしてから始める
    pure w { wTrials = takeLast 500 (wTrials w ++ [t]), wCmp = max 0 (min 255 (g0 + jsRound (r * 60))) }
  _ -> pure w

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- ---------- やってみよう ----------
-- 1. コントラストを 0 にすると、差は 0 に近づく？ 1 にすると？ グラフに描いてみよう
-- 2. B で帯をつなぐと、つないだ瞬間に同じ色に見える？
-- 3. ホワイト錯視（3）は、近くの背景（左は黒）ではなく「しまの仲間」と比べている。だから向きが逆になることも
-- 4. g0 を 60（暗い灰色）や 200（明るい灰色）にして、錯視の大きさは変わる？
-- 5. sw（しまの幅）を 4 や 60 にすると、ホワイト錯視はどうなる？

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

-- x に一番近い「灰色のしま」の中心（ホワイト錯視）
grayStripeX :: Env -> Float -> Int
grayStripeX env x =
  let a = area env
      k0 = floor ((x - ax0 a) / sw) :: Int
      left = x < ax0 a + (ax1 a - ax0 a) / 2
      k = if even k0 /= left then k0 + 1 else k0      -- 左は黒いしま（偶数）、右は白いしま（奇数）
  in round (ax0 a) + k * round sw + round sw `div` 2

-- ---- 自己テスト：3 つの図を描いて、左右の灰色の画素を読み、本当に同じ RGB かを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  ("start: comparison = standard = " ++ show g0 ++ " (one constant)", wCmp w == g0)
  : concatMap one [1 .. 3]
  where
    a = area env
    bg = g paper
    one k =
      let w' = w { wWhich = k }
          sc = scene env w' False
          ((sx, sy), (cx, cy), _) = targets env w'
          xs = if k == 3 then grayStripeX env sx else round sx
          xc = if k == 3 then grayStripeX env cx else round cx
          pa = readPix bg sc xs (round sy)
          pb = readPix bg sc xc (round cy)
          band = [ jsRound (sx + 2 + (cx - sx - 4) * fromIntegral i / 19) | i <- [0 .. 19 :: Int] ]
      in [ (figName k ++ ": the two grey pixels " ++ show pa ++ " and " ++ show pb ++ " are the same", pa == pb && pa == g g0) ]
         ++ [ ("Gradient: the band reads " ++ show g0 ++ " everywhere from left to right (though the background goes dark -> light)"
              , all (\x -> readPix bg sc x (round sy) == g g0) band) | k == 2 ]
         ++ [ ("background: left is dark " ++ show (bgDark w') ++ ", right is light " ++ show (bgLight w')
              , readPix bg sc (round (ax0 a) + 5) (round (ay0 a) + 5) == g (bgDark w') && readPix bg sc (round (ax1 a) - 5) (round (ay0 a) + 5) == g (bgLight w')) | k == 1 ]

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
