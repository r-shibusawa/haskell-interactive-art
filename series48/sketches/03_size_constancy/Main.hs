-- ============================================
-- 48-03 大きさの恒常性 ─ 廊下の巨人
-- 遠近法の廊下に、画面の上でまったく同じ大きさ（同じ px）の人を 2 人置く。奥の人ほど大きく見える
-- 脳は「遠くにあるのにこの大きさ → 本当は大きい」と距離で補正している（大きさの恒常性）
-- 操作：ドラッグ・←→ = 奥の人を前後に動かす / ↑↓ = 奥の人の大きさ / Enter = 記録 / R = 消す
--   T = 真実を見る（廊下を消して、奥の人の「影」を手前の人のとなりに並べる）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Text.Printf (printf)

h0 :: Int
h0 = 170                     -- ★ 手前の人の画面の高さ px

camH, person :: Double
camH = 1.6                   -- ★ 目の高さ m
person = 1.7                 -- ★ 人の背の高さ m

personC :: RGB
personC = (40, 90, 200)         -- 2 人の色

-- 1 回の記録：距離、選んだ高さ、遠近法どおりの高さ、恒常性 %
data Trial = Trial { tZ :: !Double, tChosen :: !Int, tPersp :: !Double, tK :: !Double }

data World = World
  { wFarH   :: !Int            -- 奥の人の画面の高さ px（最初は h0 と同じ）
  , wZFar   :: !Double         -- 奥の人までの距離 m
  , wTruth  :: !Bool
  , wTrials :: [Trial]
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-03 Size constancy" world0)
  { skBack  = g' 214
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World h0 30 False [] []

-- ---- 遠近法：距離 z m の床は画面の y = vy + f·camH / z、高さ s m のものは f·s / z px に写る
--   （計算は Double。自己テストで 1e-9 の精度まで確かめる）
data Cam = Cam { cCx, cVy, cF :: !Double }

cam :: Env -> Cam
cam env = Cam (fromIntegral (jsRound (envW env * 0.4))) (fromIntegral (jsRound (envH env * 0.4))) (realToFrac (envH env) * 0.6)

floorY :: Env -> Double -> Int
floorY env z = let c = cam env in jsRoundD (cVy c + cF c * camH / z)

screenSize :: Env -> Double -> Double -> Double
screenSize env s z = cF (cam env) * s / z

-- 手前の人は本当の 1.7 m が h0 px に写る距離
zNear :: Env -> Double
zNear env = cF (cam env) * person / fromIntegral h0

screenX :: Env -> Double -> Double -> Int
screenX env x z = let c = cam env in jsRoundD (cCx c + cF c * x / z)

-- 2 人の足もとの位置
feet :: Env -> World -> ((Float, Float), (Float, Float))
feet env w =
  let zN = zNear env
      fi = fromIntegral
  in ((fi (screenX env (-1.0) zN), fi (floorY env zN)), (fi (screenX env 0.6 (wZFar w)), fi (floorY env (wZFar w))))

scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  Box (g (if truth then 235 else 214)) 0 0 (envW env) (envH env)
  : (if truth then [] else corridor env 70)
  ++ drawPerson xN yN (fromIntegral h0) personC
  ++ drawPerson xF yF (fromIntegral (wFarH w)) personC
  -- 影：奥の人をそのまま手前の人の左へ（同じ px）
  ++ (if truth then drawPerson (xN - 100) yN (fromIntegral (wFarH w)) (120, 140, 190) else [])
  where ((xN, yN), (xF, yF)) = feet env w

-- 廊下：床・壁・天井・突き当たりと、目地の線（遠いほどつまる）
corridor :: Env -> Double -> [Shape]
corridor env zBack =
  [ face [p (-wd) 0 z0, p wd 0 z0, p wd 0 zBack, p (-wd) 0 zBack] 186          -- 床
  , face [p (-wd) 0 z0, p (-wd) hc z0, p (-wd) hc zBack, p (-wd) 0 zBack] 204  -- 左の壁
  , face [p wd 0 z0, p wd hc z0, p wd hc zBack, p wd 0 zBack] 198              -- 右の壁
  , face [p (-wd) hc z0, p wd hc z0, p wd hc zBack, p (-wd) hc zBack] 224      -- 天井
  , face [p (-wd) 0 zBack, p wd 0 zBack, p wd hc zBack, p (-wd) hc zBack] 150  -- 突き当たり
  ]
  ++ concat [ [ln (p (-wd) 0 z) (p wd 0 z), ln (p (-wd) 0 z) (p (-wd) hc z), ln (p wd 0 z) (p wd hc z)]
            | z <- takeWhile (< zBack) (iterate (* 1.2) 2) ]
  ++ [ ln (p x 0 z0) (p x 0 zBack) | k <- [0 .. 8 :: Int], let x = -wd + fromIntegral k * wd / 4 ]
  where
    wd = 2.2                   -- 廊下の半分の幅 m
    hc = 3.0                   -- 天井の高さ m
    z0 = 0.8                   -- いちばん手前の距離
    c = cam env
    -- 3D (X, 高さ Y, 距離 z) → 画面
    p x y z = (realToFrac (cCx c + cF c * x / z), realToFrac (cVy c + cF c * (camH - y) / z)) :: Point
    face ps v = Poly (g v) ps
    ln = Seg (g 120) 1

-- 足もと (x, y)・高さ h px の人（頭・胴・脚）。頭のてっぺんが y − h
drawPerson :: Float -> Float -> Float -> RGB -> [Shape]
drawPerson x y h c =
  [ Dot c x (y - h + r) r
  , Box c (x - h * 0.14) (y - h * 0.79) (h * 0.28) (h * 0.38)
  , Box c (x - h * 0.12) (y - h * 0.43) (h * 0.1) (h * 0.43)
  , Box c (x + h * 0.02) (y - h * 0.43) (h * 0.1) (h * 0.43) ]
  where r = h * 0.09

redC :: Color
redC = rgb255 200 40 40

-- 高さの物差し
heightBar :: Float -> Float -> Int -> [Picture]
heightBar x y hi =
  let h = fromIntegral hi
  in [ Color redC (Pictures [thickLine 2 (x, y) (x, y - h), thickLine 2 (x - 6, y) (x + 6, y), thickLine 2 (x - 6, y - h) (x + 6, y - h)])
     , txtC x (y - h - 6) 15 redC (show hi ++ " px") ]

-- 点線の長方形（左上 x, y・幅・高さ）
dashedRect :: Float -> Float -> Float -> Float -> Picture
dashedRect x y w h = Color redC (Pictures (concatMap side [((x, y), (x + w, y)), ((x + w, y), (x + w, y + h)), ((x + w, y + h), (x, y + h)), ((x, y + h), (x, y))]))
  where
    side ((ax, ay), (bx, by)) =
      let l = dist2 (ax, ay) (bx, by)
          at' t = (ax + (bx - ax) * t / l, ay + (by - ay) * t / l)
      in [ thickLine 2 (at' t) (at' (min l (t + 6))) | t <- takeWhile (< l) [0, 11 ..] ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (scene env w (wTruth w)) ]
  ++ (if wTruth w
        then heightBar (xN - 170) yN h0 ++ heightBar (xF + 60) yF (wFarH w)
             ++ [ dashedRect (xF - hp * 0.18) (yF - hp) (hp * 0.36) hp
                , txt (xF + 20) (yF + 24) 15 redC ("dashed = a 1.7 m person there (" ++ show (round hp :: Int) ++ " px)") ]
        else [])
  ++ [ txt 20 30 18 (g' 25) (printf "Drag or Left/Right: far person's distance %.1f m   Up/Down: far person's size %d px" (wZFar w) (wFarH w))
     , txt 20 54 18 (g' 25) "Enter: record   R: clear   T: truth"
     , txt 20 78 15 (g' 70) (printf "Make the far person look as tall as the near one, then Enter. Now the far person would be %.2f m tall." realM) ]
  ++ table w (fromIntegral (jsRound (envW env * 0.7))) 120
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    ((xN, yN), (xF, yF)) = feet env w
    hp = fromIntegral (jsRoundD (screenSize env person (wZFar w))) :: Float
    realM = person * fromIntegral (wFarH w) * wZFar w / (fromIntegral h0 * zNear env)

-- 記録の表
table :: World -> Float -> Float -> [Picture]
table w x y =
  cols (g' 25) ["Trial", "Dist", "Chosen", "Persp", "Const"] y
  ++ concat [ cols (g' 60) [show k, printf "%.1f m" (tZ t), show (tChosen t) ++ " px", printf "%.0f px" (tPersp t), printf "%.0f %%" (tK t)] (y + 24 * fromIntegral i)
            | (i, k, t) <- zip3 [1 :: Int ..] [n - length lastT + 1 ..] lastT ]
  ++ [ txt x (y + 24 * 11 + 6) 17 (g' 25) (if n > 0 then printf "Mean constancy %.0f %% (%d trials)" (mean (map tK (wTrials w))) n else "No trials yet")
     , txt x (y + 24 * 12 + 6) 14 (g' 80) "0 % = judged by screen size"
     , txt x (y + 24 * 12 + 26) 14 (g' 80) "100 % = fully corrected for distance" ]
  where
    n = length (wTrials w)
    lastT = drop (n - 10) (wTrials w)
    cw = [54, 76, 76, 70, 70]
    cols c vals yy = [ txt xx yy 16 c v | (xx, v) <- zip (scanl (+) x cw) vals ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  -- ドラッグ：マウスの高さ → 床の距離
  EventMotion (_, my) | envDown env ->
    let c = cam env
        y = clampD (fromIntegral (floorY env 60) + 1) (fromIntegral (floorY env (zN * 1.05))) (realToFrac (envH env / 2 - my))
    in w { wZFar = clampD (zN * 1.05) 60 (cF c * camH / (y - cVy c)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wZFar = min 60 (wZFar w * 1.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wZFar = max (zN * 1.05) (wZFar w / 1.1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wFarH = min 400 (wFarH w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wFarH = max 10 (wFarH w - 2) }
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
    | c `elem` "rR" -> w { wTrials = [] }
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    let hp = screenSize env person (wZFar w)
        t = Trial (wZFar w) (wFarH w) hp (constancy (fromIntegral (wFarH w)) (fromIntegral h0) hp)
    in w { wTrials = takeLast 500 (wTrials w ++ [t]), wFarH = h0 }
  _ -> w
  where zN = zNear env

-- 恒常性 %：画面で同じ大きさを選んだら 0 %、遠近法どおり（本当の 1.7 m）を選んだら 100 %
constancy :: Double -> Double -> Double -> Double
constancy chosen same persp = (same - chosen) / (same - persp) * 100

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

jsRoundD :: Double -> Int
jsRoundD x = floor (x + 0.5)

-- ---------- やってみよう ----------
-- 1. 奥の人を一番奥まで動かす。画面では同じ 170 px なのに、何 m の巨人に見える？
-- 2. 恒常性を 3 つの距離で測る。遠いほど 100 % に近い？
-- 3. T で廊下を消すと、2 人は同じ大きさに見える？ 錯視の原因は「人」ではなく「廊下」
-- 4. corridor の中の目地の線（ln）を消すと、錯視はどうなる？
-- 5. camH（目の高さ）を 0.5 にすると、子どもの目線の廊下になる

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

-- 四角い範囲の中で、色 c の画素の上下のはし → 高さ px（p5.js の colorHeight）
colourHeight :: RGB -> [Shape] -> Int -> Int -> Int -> Int -> RGB -> Int -> Int
colourHeight bg shs xa xb ya yb c tol =
  case [y | x <- [xa, xa + 2 .. xb], y <- [max 0 ya .. yb], near tol (readPix bg shs x y) c] of
    [] -> 0
    ys -> maximum ys - minimum ys + 1

-- ---- 自己テスト：描いた図で 2 人の高さを画素で測る。遠近法の式も確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("start: far person = near person = " ++ show h0 ++ " px (one constant)", wFarH w == h0)
  , ("measured heights in pixels: near " ++ show hN ++ " px, far " ++ show hF ++ " px (equal)", hN == hF && abs (hN - h0) <= 2)
  , ("perspective: twice as far -> half as big", abs (screenSize env 1 10 - 2 * screenSize env 1 20) < 1e-9)
  , ("the near person's 1.7 m maps to exactly H0 px", abs (screenSize env person zN - fromIntegral h0) < 1e-9)
  , ("floor lines approach the vanishing point as they go farther", floorY env 5 > floorY env 10 && floorY env 10 > floorY env 40 && fromIntegral (floorY env 40) > cVy (cam env))
  , ("constancy: same as screen = 0 %, perspective = 100 %", constancy 170 170 50 == 0 && constancy 50 170 50 == 100) ]
  where
    zN = zNear env
    sc = scene env w False
    bg = g 214
    ((xN, yN), (xF, yF)) = feet env w
    ix = round :: Float -> Int
    hN = colourHeight bg sc (ix xN - 20) (ix xN + 20) (ix yN - h0 - 10) (ix yN + 5) personC 14
    hF = colourHeight bg sc (ix xF - 20) (ix xF + 20) (ix yF - wFarH w - 10) (ix yF + 5) personC 14

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
