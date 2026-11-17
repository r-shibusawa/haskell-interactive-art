-- ============================================
-- 48-01 エビングハウス錯視とポンゾ錯視
-- 1：同じ大きさのオレンジの円でも、大きな円に囲まれると小さく、小さな円に囲まれると大きく見える
-- 2：線路のように奥へ向かう 2 本の線の間では、上（遠く）の棒のほうが長く見える
-- 操作：↑↓ = 右（下）のオレンジの大きさ / ←→ = まわりの大きさ・線の傾き / 1・2 = 錯視の切替 / Enter = 記録 / R = 消す
--   T またはマウスを押している間 = 真実を見る（まわりを消して、本当の大きさを px で表示）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Text.Printf (printf)

paper, ringV :: Int
paper = 240                  -- 紙の明るさ
ringV = 110                  -- まわりの円・線路の明るさ

orangeC :: RGB
orangeC = (235, 120, 30)      -- 比べる 2 つの色

r0, b0, smallRatio :: Float
r0 = 40                      -- ★ 左のオレンジの円の半径 px（標準）
b0 = 160                     -- ★ ポンゾの上の棒の長さ px（標準）
smallRatio = 0.35            -- ★ 小さい円の半径 ÷ r0

-- 1 回の記録
data Trial = Trial { tWhich :: !Int, tPar :: String, tDiff :: !Int, tPct :: !Float }

data World = World
  { wWhich  :: !Int            -- 1 = エビングハウス、2 = ポンゾ
  , wCmpR   :: !Int            -- 右の円の半径（比較）。最初は r0 と同じ
  , wBig    :: !Int            -- ★ 大きい円の半径 ÷ r0（10 倍した整数。17 = 1.7）
  , wCmpB   :: !Int            -- 下の棒（比較）
  , wConv   :: !Int            -- ★ ポンゾの線の傾き（100 倍した整数。0 = 平行）
  , wTruth  :: !Bool
  , wTrials :: [Trial]
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-01 Ebbinghaus and Ponzo" world0)
  { skBack  = g' paper
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 (round r0) 17 (round b0) 32 False [] []

bigRatio, conv :: World -> Float
bigRatio w = fromIntegral (wBig w) / 10
conv w = fromIntegral (wConv w) / 100

-- 位置（画面の大きさから決める）
data Layout = Layout { lY, lXL, lXR, lCx, lYTop, lYBot :: !Float }

layout :: Env -> Layout
layout env = Layout (r (envH env * 0.45)) (r (envW env * 0.22)) (r (envW env * 0.55)) (r (envW env * 0.38)) (r (envH env * 0.3)) (r (envH env * 0.66))
  where r = fromIntegral . jsRound

-- ---- 図（truth ならまわりを消す）
scene :: Env -> World -> Bool -> [Shape]
scene env w truth
  | wWhich w == 1 =
      (if truth then [] else ringOf (lXL l) (lY l) r0 6 (r0 * bigRatio w) 14 ++ ringOf (lXR l) (lY l) cmpR 8 (r0 * smallRatio) 8)
      ++ [Dot orangeC (lXL l) (lY l) r0, Dot orangeC (lXR l) (lY l) cmpR]
  | otherwise =
      (if truth then [] else
         [ RSeg (g ringV) 5 (lCx l - 30, vy) (lCx l - half h, h), RSeg (g ringV) 5 (lCx l + 30, vy) (lCx l + half h, h) ]
         ++ [ Fade 120 (RSeg (g ringV) 2 (lCx l - half y, y) (lCx l + half y, y))
            | k <- [1 .. 13 :: Int], let y = vy + (h - vy) * (fromIntegral k / 14) ** 1.6 ])
      ++ [ Box orangeC (lCx l - fromIntegral (round b0 `div` 2 :: Int)) (lYTop l - 6) b0 12
         , Box orangeC (lCx l - fromIntegral (wCmpB w `div` 2)) (lYBot l - 6) (fromIntegral (wCmpB w)) 12 ]
  where
    l = layout env
    h = envH env
    cmpR = fromIntegral (wCmpR w)
    -- 線路：消失点 (cx, vy) から広がる 2 本の線と枕木
    vy = fromIntegral (jsRound (h * 0.08))
    half y = (y - vy) * conv w + 30

-- 中心 (x, y)・半径 r の円のまわりに、半径 rr の円を n 個、すき間 gap で並べる
ringOf :: Float -> Float -> Float -> Int -> Float -> Float -> [Shape]
ringOf x y r n rr gap =
  [ Dot (g ringV) (x + d * cos a) (y + d * sin a) rr
  | k <- [0 .. n - 1], let a = fromIntegral k / fromIntegral n * tau + pi / fromIntegral n ]
  where d = r + gap + rr

-- 本当の大きさの物差し（赤）
measureBar :: Float -> Float -> Float -> String -> [Picture]
measureBar x0 x1 y s =
  [ Color redC (Pictures [thickLine 2 (x0, y) (x1, y), thickLine 2 (x0, y - 6) (x0, y + 6), thickLine 2 (x1, y - 6) (x1, y + 6)])
  , txtC ((x0 + x1) / 2) (y + 26) 16 redC s ]

redC :: Color
redC = rgb255 200 40 40

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w truth) ]
  ++ (if not truth then [] else
        if wWhich w == 1
          then measureBar (lXL l - r0) (lXL l + r0) (lY l + r0 + 24) ("diameter " ++ show (2 * round r0 :: Int) ++ " px")
               ++ measureBar (lXR l - cmpR) (lXR l + cmpR) (lY l + cmpR + 24) ("diameter " ++ show (2 * wCmpR w) ++ " px")
          else let xa = lCx l - fromIntegral (round b0 `div` 2 :: Int)
                   xb = lCx l - fromIntegral (wCmpB w `div` 2)
               in measureBar xa (xa + b0) (lYTop l + 26) (show (round b0 :: Int) ++ " px")
                  ++ measureBar xb (xb + fromIntegral (wCmpB w)) (lYBot l + 26) (show (wCmpB w) ++ " px"))
  ++ [ txt 20 30 18 (g' 30) ("1/2: " ++ (if wWhich w == 1 then "Ebbinghaus" else "Ponzo") ++ "   Up/Down: size of the "
                             ++ (if wWhich w == 1 then "right circle" else "lower bar") ++ "   Left/Right: " ++ par)
     , txt 20 54 18 (g' 30) "Enter: record   R: clear   T or hold mouse: truth"
     , txt 20 78 15 (g' 90) (if wWhich w == 1 then "Press Enter when the right orange circle looks as big as the left one."
                                              else "Press Enter when the lower bar looks as long as the upper one.") ]
  ++ table w (fromIntegral (jsRound (envW env * 0.7))) 110
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    l = layout env
    cmpR = fromIntegral (wCmpR w)
    par = if wWhich w == 1 then printf "big circles x %.1f" (bigRatio w) else printf "line slope %.2f" (conv w)

-- 記録の表（最近の 10 回と、錯視ごとの平均）
table :: World -> Float -> Float -> [Picture]
table w x y =
  cols (g' 30) ["Trial", "Condition", "Diff px", "Illusion"] y
  ++ concat [ cols (g' 70) [show k, (if tWhich t == 1 then "Ebb x" else "Ponzo ") ++ tPar t, (if tDiff t > 0 then "+" else "") ++ show (tDiff t), printf "%.1f %%" (tPct t)] (y + 24 * fromIntegral i)
            | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT ]
  ++ [ txt x (y + 24 * 11 + fromIntegral (k - 1) * 26) 16 (g' 30) (nm ++ " mean: " ++ avg k) | (k, nm) <- [(1, "Ebbinghaus"), (2, "Ponzo")] ]
  ++ [ txt x (y + 24 * 11 + 56) 14 (g' 90) "Illusion % = (matched - real)"
     , txt x (y + 24 * 11 + 76) 14 (g' 90) "                / real x 100" ]
  where
    n = length (wTrials w)
    lastT = drop (n - 10) (wTrials w)
    cw = [56, 130, 80, 80]
    cols c vals yy = [ txt xx yy 16 c v | (xx, v) <- zip (scanl (+) x cw) vals ]
    avg k = case [tPct t | t <- wTrials w, tWhich t == k] of
      [] -> "-"
      ps -> printf "%.1f %% (n=%d)" (mean ps) (length ps)

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12" -> pure w { wWhich = if c == '1' then 1 else 2, wTruth = False }
    | c `elem` "tT" -> pure w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> pure w { wTrials = [] }
  EventKey (SpecialKey KeyUp) Down _ _
    | ebb -> pure w { wCmpR = min 90 (wCmpR w + 1) }
    | otherwise -> pure w { wCmpB = min 400 (wCmpB w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _
    | ebb -> pure w { wCmpR = max 10 (wCmpR w - 1) }
    | otherwise -> pure w { wCmpB = max 40 (wCmpB w - 2) }
  EventKey (SpecialKey KeyRight) Down _ _
    | ebb -> pure w { wBig = min 26 (wBig w + 1) }
    | otherwise -> pure w { wConv = min 80 (wConv w + 4) }
  EventKey (SpecialKey KeyLeft) Down _ _
    | ebb -> pure w { wBig = max 4 (wBig w - 1) }
    | otherwise -> pure w { wConv = max 0 (wConv w - 4) }
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    r <- randomRIO (-0.5, 0.5 :: Float)
    let std = if ebb then 2 * round r0 else round b0 :: Int
        seen = if ebb then 2 * wCmpR w else wCmpB w
        t = Trial (wWhich w) (if ebb then printf "%.1f" (bigRatio w) else printf "%.2f" (conv w)) (seen - std)
                  (illusionPct (fromIntegral seen) (fromIntegral std))
        w' = w { wTrials = takeLast 500 (wTrials w ++ [t]) }
    -- 次の試行は少しずらしてから始める
    pure (if ebb then w' { wCmpR = round r0 + jsRound (r * 16) } else w' { wCmpB = round b0 + jsRound (r * 60) })
  _ -> pure w
  where ebb = wWhich w == 1

-- 錯視量 %：（同じに見えた大きさ − 本当）÷ 本当
illusionPct :: Float -> Float -> Float
illusionPct seen real = (seen - real) / real * 100

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- ---------- やってみよう ----------
-- 1. エビングハウス：←→ で大きい円を r0 と同じ大きさ（× 1.0）にすると錯視は弱くなる？
-- 2. ringOf の gap（すき間）を大きくすると？ まわりが遠いと効きにくい
-- 3. ポンゾ：線の傾きを 0（平行）にすると、2 本の棒は同じに見える？
-- 4. 枕木だけ・線だけにして（scene の中のリストを消して）、どちらが効いているか確かめよう
-- 5. 何回か測って、エビングハウスとポンゾの平均の錯視量を比べよう

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

-- ---- 自己テスト：両方の錯視を描いて画素を測り、比べる 2 つが本当に同じ大きさかを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("start: comparison = standard (radius " ++ show (round r0 :: Int) ++ ", bar " ++ show (round b0 :: Int) ++ " px, one constant)"
    , wCmpR w == round r0 && wCmpB w == round b0)
  , ("Ebbinghaus: measured diameters " ++ show a ++ " px and " ++ show b ++ " px (equal)", a == b && abs (a - 2 * round r0) <= 2)
  , ("Ebbinghaus: both circle centres have the same colour " ++ show pa, pa == pb && pa == orangeC)
  , ("Ponzo: measured bar lengths " ++ show c ++ " px and " ++ show d ++ " px (equal)", c == d && abs (c - round b0) <= 1)
  , ("illusion %: 80 px seen as 88 px is +10 %", abs (illusionPct 88 80 - 10) < 1e-4)
  , ("mean: [] = 0, [-4, 10] = 3", mean [] == 0 && mean [-4, 10] == 3) ]
  where
    l = layout env
    ebb = scene env w { wWhich = 1 } False
    ponzo = scene env w { wWhich = 2 } False
    bg = g paper
    iy = round (lY l)
    ixL = round (lXL l)
    ixR = round (lXR l)
    icx = round (lCx l)
    a = inkRun bg ebb iy (ixL - 100) (ixL + 100) orangeC 12
    b = inkRun bg ebb iy (ixR - 100) (ixR + 100) orangeC 12
    pa = readPix bg ebb ixL iy
    pb = readPix bg ebb ixR iy
    c = inkRun bg ponzo (round (lYTop l)) (icx - 300) (icx + 300) orangeC 12
    d = inkRun bg ponzo (round (lYBot l)) (icx - 300) (icx + 300) orangeC 12

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
