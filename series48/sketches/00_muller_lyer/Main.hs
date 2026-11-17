-- ============================================
-- 48-00 ミュラー＝リヤー錯視
-- 同じ長さの 2 本の線でも、両はしの矢羽が外向き（＞─＜）だと長く、内向き（＜─＞）だと短く見える
-- 下の線（比較）の長さを ↑↓ で変えて「上と同じに見える」所で Enter → 実際の差（px と %）を記録する（調整法）
-- 操作：↑↓ = 比較の長さ / ←→ = 矢羽の角度 / A・Z = 矢羽の長さ / F = 矢羽の向きを入れかえ / Enter = 記録 / R = 記録を消す
--   T またはマウスを押している間 = 真実を見る（矢羽を消して、本当の長さを px で表示）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Text.Printf (printf)

paper, ink :: Int
paper = 236                  -- 紙の明るさ
ink = 25                     -- 線の明るさ

sw :: Float
sw = 6                       -- ★ 線の太さ px

-- 1 回の記録
data Trial = Trial { tAng :: !Int, tFl :: !Int, tLen :: !Int, tDiff :: !Int, tPct :: !Float }

data World = World
  { wL0     :: !Int            -- ★ 上の線（標準刺激）の長さ px
  , wCmp    :: !Int            -- 下の線（比較刺激）の長さ。最初は L0 と同じ
  , wAng    :: !Int            -- ★ 矢羽の角度（度）
  , wFin    :: !Int            -- ★ 矢羽の長さ px
  , wSwap   :: !Bool           -- 矢羽の向きを入れかえる
  , wTruth  :: !Bool           -- T で真実を見る
  , wTrials :: [Trial]         -- 記録（新しいものが後ろ）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-00 Mueller-Lyer illusion" (world0 1280))
  { skBack  = g' paper
  , skStart = \env -> do
      let w = world0 (envW env)
          rs = selfTest env w
      printSelfTest rs
      pure w { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> World
world0 width = World l0 l0 35 60 False False [] []
  where l0 = jsRound (min 300 (width * 0.28))

-- 2 本の線の位置（画面の大きさから決める）
layout :: Env -> (Float, Float, Float)
layout env = (fromIntegral (jsRound (envW env * 0.36)), fromIntegral (jsRound (envH env * 0.3)), fromIntegral (jsRound (envH env * 0.55)))

-- ---- 図：上が標準（外向きの矢羽）、下が比較（内向きの矢羽）。truth なら矢羽を消して補助線
scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  muellerLyer w cx y1 (wL0 w) (not (wSwap w)) (not truth)
  ++ muellerLyer w cx y2 (wCmp w) (wSwap w) (not truth)
  ++ (if truth then concatMap guides [(y1, wL0 w), (y2, wCmp w)] else [])
  where
    (cx, y1, y2) = layout env
    red = (200, 40, 40)
    -- 両はしに縦の補助線（線のすぐ外側の 1 px）と、50 px ごとの目盛り
    guides (y, len) =
      let x0 = cx - fromIntegral (len `div` 2)
          x1 = x0 + fromIntegral len
      in [ Box red (x0 - 1) (y - 40) 1 80, Box red x1 (y - 40) 1 80 ]
         ++ [ Box red (x - 0.5) (y + 14) 1 8 | x <- [x0, x0 + 50 .. x1] ]

-- ミュラー＝リヤー図形：中心 cx、高さ y、長さ len。out = True なら外向き（＞─＜）
muellerLyer :: World -> Float -> Float -> Int -> Bool -> Bool -> [Shape]
muellerLyer w cx y len out fins =
  Box (g ink) x0 (y - sw / 2) (fromIntegral len) sw                 -- 線（軸）は長方形で描く → 長さが正確
  : if not fins then [] else
      concat [ [RSeg (g ink) sw (x, y) (px, py), RSeg (g ink) sw (x, y) (px, 2 * y - py)]
             | (x, dir) <- [(x0, -1), (x1, 1)], let (px, py) = finTip w x y dir out ]
  where
    x0 = cx - fromIntegral (len `div` 2)
    x1 = x0 + fromIntegral len

-- 矢羽の先の位置：dir = −1 が左はし、+1 が右はし
finTip :: World -> Float -> Float -> Float -> Bool -> Point
finTip w x y dir out =
  let a = fromIntegral (wAng w) * pi / 180
      s = if out then dir else -dir
      fl = fromIntegral (wFin w)
  in (x + s * fl * cos a, y - fl * sin a)

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w truth) ]
  ++ (if truth then [ txt (cx - fromIntegral (wL0 w `div` 2)) (y1 + 66) 18 redC ("top " ++ show (wL0 w) ++ " px")
                    , txt (cx - fromIntegral (wCmp w `div` 2)) (y2 + 66) 18 redC
                          ("bottom " ++ show (wCmp w) ++ " px (diff " ++ signed (wCmp w - wL0 w) ++ " px)") ]
               else [])
  ++ [ txt 20 30 18 (g' ink) ("Up/Down: bottom line length   Left/Right: fin angle " ++ show (wAng w) ++ " deg   A/Z: fin length " ++ show (wFin w) ++ " px")
     , txt 20 54 18 (g' ink) "F: flip fins   Enter: record   R: clear   T or hold mouse: truth"
     , txt 20 78 15 (g' 90) "Press Enter when the bottom line looks as long as the top one." ]
  ++ table env w 20 (envH env - 230)
  ++ graph w (fromIntegral (jsRound (envW env * 0.66))) 110 (fromIntegral (jsRound (envW env * 0.3))) (fromIntegral (jsRound (envH env * 0.42)))
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    (cx, y1, y2) = layout env
    redC = rgb255 200 40 40

-- 記録の表（最近の 6 回と平均）
table :: Env -> World -> Float -> Float -> [Picture]
table _ w x y =
  cols (g' ink) ["Trial", "Angle", "Fin", "Compare", "Diff px", "Illusion"] y
  ++ concat [ cols (g' 70) [show k, show (tAng t) ++ " deg", show (tFl t), show (tLen t), signed (tDiff t), printf "%.1f %%" (tPct t)] (y + 24 * fromIntegral i)
            | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT ]
  ++ [ txt x (y + 24 * 7 + 6) 18 (g' ink)
         (if n > 0 then printf "Mean illusion %.1f %% (%d trials)" (mean (map tPct (wTrials w))) n else "No trials yet") ]
  where
    n = length (wTrials w)
    lastT = drop (n - 6) (wTrials w)
    cw = [70, 80, 60, 100, 90, 90]
    cols c vals yy = [ txt xx yy 16 c v | (xx, v) <- zip (scanl (+) x cw) vals ]

-- 角度ごとの錯視量のグラフ（横：角度、縦：%）
graph :: World -> Float -> Float -> Float -> Float -> [Picture]
graph w gx gy gw gh =
  [ Color (g' 150) (rectLine (gx + gw / 2) (gy + gh / 2) gw gh)
  , Color (g' 190) (Line [(gx, yOf 0), (gx + gw, yOf 0)]) ]
  ++ [ txtR (gx - 6) (yOf p + 6) 14 (g' 90) (show (round p :: Int) ++ "%") | p <- [-10, 0, 20, 40] ]
  ++ [ txtC (xOf a) (gy + gh + 22) 14 (g' 90) (show (round a :: Int) ++ " deg") | a <- [0, 30, 60, 90] ]
  ++ [ txt gx (gy - 10) 14 (g' 90) "Fin angle vs illusion (dot = trial)" ]
  ++ [ Color (rgb255 200 40 40) (at (xOf (fromIntegral (tAng t)), clampF gy (gy + gh) (yOf (tPct t))) (disc 4)) | t <- wTrials w ]
  where
    yOf p = gy + gh - ((p + 10) / 50) * gh
    xOf a = gx + (a / 90) * gw

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wCmp = min (jsRound (envW env * 0.6)) (wCmp w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wCmp = max 40 (wCmp w - 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wAng = min 85 (wAng w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wAng = max 5 (wAng w - 5) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wFin = min 140 (wFin w + 10) }
    | c `elem` "zZ" -> pure w { wFin = max 0 (wFin w - 10) }
    | c `elem` "fF" -> pure w { wSwap = not (wSwap w) }
    | c `elem` "tT" -> pure w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> pure w { wTrials = [] }
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    r <- randomRIO (-0.5, 0.5 :: Float)
    let t = Trial (wAng w) (wFin w) (wCmp w) (wCmp w - wL0 w) (illusionPct (fromIntegral (wCmp w)) (fromIntegral (wL0 w)))
    -- 次の試行は少しずらしてから始める
    pure w { wTrials = takeLast 500 (wTrials w ++ [t]), wCmp = wL0 w + jsRound (r * 80) }
  _ -> pure w

-- 錯視量 %：（同じに見えた長さ − 本当の長さ）÷ 本当の長さ
illusionPct :: Float -> Float -> Float
illusionPct seen real = (seen - real) / real * 100

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

signed :: Int -> String
signed v = (if v > 0 then "+" else if v < 0 then "-" else "+-") ++ show (abs v)

-- ---------- やってみよう ----------
-- 1. 角度 35° で 3 回測って平均を出す。次に 70° で。どちらが錯視が強い？
-- 2. 矢羽の長さを 0 にすると錯視量は 0 % に近づく？（Z を押し続ける）
-- 3. F で向きを入れかえると、差の符号はどうなる？
-- 4. T で真実を見てから、もう一度測る。知っていても錯視は消えない？
-- 5. 友だちと測り比べよう。錯視の強さには個人差がある
-- 6. sw（線の太さ）や ink（線の色）を変えても、自己テストは通る？（2 本は同じ定数から作っている）

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

-- ---- 自己テスト：描く図（形のリスト）の画素を読んで、2 本が本当に同じ長さかを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("start: standard = comparison = " ++ show l0 ++ " px (one constant)", wCmp w == l0)
  , ("measured in pixels: top " ++ show m1 ++ " px, bottom " ++ show m2 ++ " px (equal, L0 +-1)", m1 == m2 && abs (m1 - l0) <= 1)
  , ("with fins the two lines have the same colour", c1 == c2 && c1 == g ink)
  , ("fin tips: outward and inward are mirror images", abs (fst tO - 100 + (fst tI - 100)) < 1e-4 && snd tO == snd tI)
  , ("illusion %: a 300 px line seen as 330 px is +10 %", abs (illusionPct 330 300 - 10) < 1e-4)
  , ("mean: no trials = 0, [2, 4, 9] = 5", mean [] == 0 && mean [2, 4, 9] == 5) ]
  where
    l0 = wL0 w
    (cx, y1, y2) = layout env
    ix = round cx :: Int
    truthPic = scene env w True                 -- 真実モード（矢羽なし）で画素を測る
    finPic = scene env w False                  -- 錯視の図（矢羽あり）
    m1 = inkRun (g paper) truthPic (round y1) (ix - l0) (ix + l0) (g ink) 12
    m2 = inkRun (g paper) truthPic (round y2) (ix - l0) (ix + l0) (g ink) 12
    c1 = readPix (g paper) finPic ix (round y1)
    c2 = readPix (g paper) finPic ix (round y2)
    tO = finTip w 100 50 1 True
    tI = finTip w 100 50 1 False

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
