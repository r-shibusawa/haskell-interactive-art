-- ============================================
-- 48-06 クレイク＝オブライエン＝コーンスウィート錯視
-- 左右の面はまったく同じ灰色。境界の「ふち」だけ、左は少し暗く・右は少し明るくしてある
-- それだけで、右の面全体が明るく見える。脳はふち（変化）から面の明るさを推測している
-- 操作：↑↓ = ふちの強さ / ←→ = ふちの幅 / A・Z = 下の「本物の段差」の大きさ / Enter = 記録 / R = 消す
--   T またはマウスを押している間 = 真実を見る（境界を棒でかくす → 左右が同じに見える）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Text.Printf (printf)

gG :: Int
gG = 128                     -- ★ 面の灰色

back :: Int
back = 200                   -- 背景の明るさ

-- 1 回の記録：ふちの強さ・幅・選んだ段差
data Trial = Trial { tAmp :: !Int, tW :: !Int, tD :: !Int }

data World = World
  { wAmp    :: !Int            -- ★ ふちの強さ（境界での明暗の差の半分）
  , wPx     :: !Int            -- ★ ふちの幅 px（指数関数のなだらかさ）
  , wStep   :: !Int            -- 本物の段差（左 G − stepD、右 G + stepD）
  , wTruth  :: !Bool
  , wTrials :: [Trial]
  , wProf   :: !(U.Vector Int) -- 上の図の明るさの断面（パラメータを変えたときだけ作り直す）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-06 Cornsweet illusion" (build env1 world0))
  { skBack  = g' back
  , skStart = \env -> do
      let w = build env world0
          rs = selfTest env w
      printSelfTest rs
      pure w { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 40 40 0 False [] U.empty []

-- 図の範囲
data Area = Area { ax0, ax1, ay0, ay1, ay2, ay3 :: !Float }

area :: Env -> Area
area env = Area 40 (r (envW env * 0.72)) 90 (r (envH env * 0.38)) (r (envH env * 0.42)) (r (envH env * 0.6))
  where r = fromIntegral . jsRound

edgeX :: Env -> Int
edgeX env = let a = area env in jsRound ((ax0 a + ax1 a) / 2)

-- コーンスウィートの明るさの断面：境界からの距離 d で指数関数的に G に戻る
cornsweet :: Env -> World -> Int -> Int
cornsweet env w x =
  let d = fromIntegral x + 0.5 - fromIntegral (edgeX env) :: Float
  in jsRound (fromIntegral gG + signum d * fromIntegral (wAmp w) * exp (negate (abs d) / fromIntegral (wPx w)))

-- 断面を計算して覚えておく（パラメータを変えたときだけ）
build :: Env -> World -> World
build env w = w { wProf = U.generate n (\i -> cornsweet env w (x0 + i)) }
  where
    a = area env
    x0 = round (ax0 a)
    n = max 0 (round (ax1 a) - x0)

-- ---- 図：上はふちだけの明暗（1 列ずつ）、下は本物の段差。truth なら境界を棒でかくす
scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  [ Cols (ax0 a) (ay0 a) (ay1 a - ay0 a) (wProf w)
  , Cols (ax0 a) (ay2 a) (ay3 a - ay2 a) (U.generate n (\i -> if round (ax0 a) + i < ex then gG - wStep w else gG + wStep w)) ]
  ++ [ Box (g 40) (fromIntegral ex - m) (ay0 a - 10) (2 * m) (ay1 a - ay0 a + 20) | truth ]
  where
    a = area env
    ex = edgeX env
    n = U.length (wProf w)
    m = max 12 (fromIntegral (wPx w) * 2.5)

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w truth)
  -- 断面のグラフ（上の図の 1 行の明るさ）
  , Color (g' 150) (rectLine ((ax0 a + ax1 a) / 2) ((gy0 + gy1) / 2) (ax1 a - ax0 a) (gy1 - gy0))
  , Color (g' 120) (Pictures [ Line [(x, yOf gG), (x + 4, yOf gG)] | x <- [ax0 a, ax0 a + 8 .. ax1 a - 4] ])
  , Color (rgb255 220 60 40) (thickPath 2.5 [ (ax0 a + fromIntegral i, yOf v) | (i, v) <- zip [0 :: Int ..] (U.toList (wProf w)) ])
  , Color (rgb255 40 90 220) (thickPath 1.5 [ (ax0 a, yOf (gG - s)), (ex, yOf (gG - s)), (ex, yOf (gG + s)), (ax1 a, yOf (gG + s)) ])
  , txt (ax0 a + 8) (gy0 + 20) 14 (g' 30) ("Red = brightness profile of the top picture (= its pixels, checked by self-test)   Blue = real step   Dots = " ++ show gG)
  , txtR (ax0 a - 4) (yOf gG + 5) 14 (g' 30) (show gG)
  , txt 20 24 18 (g' 20) ("Up/Down: edge strength " ++ show (wAmp w) ++ "   Left/Right: edge width " ++ show (wPx w) ++ " px   A/Z: real step +-" ++ show s)
  , txt 20 46 18 (g' 20) "Enter: record   R: clear   T or hold mouse: hide the border"
  , txt 20 66 15 (g' 60) ("Far from the edge both halves are " ++ show gG ++ ". Adjust the real step (A/Z) until it looks the same, then Enter.")
  , txt (ax0 a) (ay0 a - 6) 15 (g' 30) "Top: Cornsweet (edges only)"
  , txt (ax0 a) (ay2 a - 6) 15 (g' 30) "Bottom: real step"
  , txt tx ty 15 (g' 30) "Trial  Strength  Width  Step" ]
  ++ [ txt tx (ty + 22 * fromIntegral i) 15 (g' 60) (printf "%-6d %-9d %-6d +-%d" k (tAmp t) (tW t) (tD t))
     | (i, k, t) <- zip3 [1 :: Int ..] [nT - length lastT + 1 ..] lastT ]
  ++ [ txt tx (ty + 22 * 11 + 6) 15 (g' 20)
         (printf "Mean step +-%.1f (%d %% of edge)" (mean (map (fromIntegral . tD) (wTrials w)))
                 (truncate (100 * mean [ fromIntegral (tD t) / fromIntegral (tAmp t) | t <- wTrials w, tAmp t > 0 ]) :: Int))
     | nT > 0 ]
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    a = area env
    ex = fromIntegral (edgeX env)
    s = wStep w
    gy0 = fromIntegral (jsRound (envH env * 0.66))
    gy1 = envH env - 50
    yOf v = gy1 - (fromIntegral (v - (gG - 90)) / 180) * (gy1 - gy0)
    tx = fromIntegral (jsRound (envW env * 0.75))
    ty = 110
    nT = length (wTrials w)
    lastT = drop (nT - 10) (wTrials w)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> build env w { wAmp = min 100 (wAmp w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> build env w { wAmp = max 0 (wAmp w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> build env w { wPx = min 200 (wPx w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> build env w { wPx = max 5 (wPx w - 5) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wStep = min 100 (wStep w + 1) }
    | c `elem` "zZ" -> w { wStep = max 0 (wStep w - 1) }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> w { wTrials = [] }
  EventKey (SpecialKey KeyEnter) Down _ _ -> w { wTrials = takeLast 500 (wTrials w ++ [Trial (wAmp w) (wPx w) (wStep w)]), wStep = 0 }
  EventResize _ -> build env w
  _ -> w

mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- ---------- やってみよう ----------
-- 1. ふちの強さ 40 のとき、本物の段差はいくつに見えた？ 何回か測って平均を出す
-- 2. ふちの幅を 5 px（くっきり）と 150 px（なだらか）で比べる。どちらが「面全体」の差に見える？
-- 3. T で境界をかくすと、左右は同じに見える。指で画面の境界をかくしても OK
-- 4. 断面のグラフの赤い線：遠くでは左右とも点線（128）に重なっている
-- 5. cornsweet の exp をやめて、直線で G に戻すとどうなる？

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
  | Cols !Float !Float !Float !(U.Vector Int)     -- 左上 (x, y)・高さ：1 px 幅の灰色の縦の列を並べる
  | Fade !Int Shape                               -- 透明度 0〜255 をつける

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map (recolour 255)
  where
    recolour a sh = case sh of
      Box c x y w h -> Color (col a c) (rect (x + w / 2) (y + h / 2) w h)
      Cols x y h vs ->
        -- 同じ値が続く列はまとめて 1 つの長方形にする（画素の値はそのまま）
        Pictures [ Color (col a (g v)) (rect (x + fromIntegral i0 + fromIntegral n / 2) (y + h / 2) (fromIntegral n) h) | (i0, n, v) <- runs (U.toList vs) ]
      Fade b s -> recolour (a * b `div` 255) s
    col a (r, gg, b) = rgba255 r gg b a

-- 同じ値の続き：(始まりの番号, 長さ, 値)
runs :: [Int] -> [(Int, Int, Int)]
runs = go 0
  where
    go _ [] = []
    go i (v : vs) = let n = 1 + length (takeWhile (== v) vs) in (i, n, max 0 (min 255 v)) : go (i + n) (drop (n - 1) vs)

-- 形が点 p をおおっていれば (色, 透明度)
covers :: Shape -> Point -> Maybe (RGB, Int)
covers sh (px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Cols x y h vs ->
    let i = floor (px - x) :: Int
    in if i >= 0 && i < U.length vs && py >= y && py < y + h then Just (g (max 0 (min 255 (vs U.! i))), 255) else Nothing
  Fade a s -> fmap (\(c, b) -> (c, a * b `div` 255)) (covers s (px, py))
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

-- ---- 自己テスト：上の図の 1 行の画素を全部読み、左右の面が同じ灰色か、ふちの明暗が打ち消し合うかを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("left surface " ++ show l ++ " = right surface " ++ show r ++ " = " ++ show gG, l == r && l == g gG)
  , ("just left of the edge is darker " ++ show (px (ex - 1) y) ++ ", just right is lighter " ++ show (px ex y)
    , px (ex - 1) y < gG && px ex y > gG)
  , ("edge light and dark cancel: sum of (pixel - " ++ show gG ++ ") = " ++ show dev ++ " (about 0)", abs dev <= 3)
  , ("pixels on screen = computed profile (all " ++ show (length row) ++ " pixels)", row == U.toList (wProf w))
  , ("the real step starts at 0: bottom picture is " ++ show gG ++ " on both sides"
    , px (x0 + 10) (y3 - 10) == gG && px (x1 - 10) (y3 - 10) == gG) ]
  where
    a = area env
    ex = edgeX env
    sc = scene env w False
    x0 = round (ax0 a)
    x1 = round (ax1 a)
    y3 = round (ay3 a)
    y = jsRound ((ay0 a + ay1 a) / 2)
    pix x yy = readPix (g back) sc x yy
    px x yy = let (v, _, _) = pix x yy in v
    row = [ px x y | x <- [x0 .. x1 - 1] ]
    l = pix (x0 + 10) y
    r = pix (x1 - 10) y
    dev = sum [ v - gG | v <- row ]

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
testColour rs = if not (null rs) && all snd rs then rgb255 30 130 60 else rgb255 200 40 40
