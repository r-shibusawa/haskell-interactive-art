-- ============================================
-- 54-02 ガウスの法則 ─ 閉じた曲線を通りぬける「電束」は、中の電荷だけで決まる
-- 電荷は紙に垂直な長い線（線電荷 λ [nC/m]）なので、電場は 2 次元：E = λ / (2π ε0 r)
-- 曲線にそって E·n（外向きの成分）を足し合わせる（数値の線積分）と、Φ = (中の電荷の合計) / ε0 になる
-- 操作：何もない所をドラッグ = 閉じた曲線を描く / 電荷をドラッグ = 動かす / A・N = 追加 / F = 符号 / ↑↓ = 大きさ
--       1 2 = 円・星形 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

eps0 :: Double
eps0 = 8.854e-12                       -- 真空の誘電率 ε0 [F/m]

pxM :: Double
pxM = 0.001                            -- ★ 1 px = 1 mm

sub :: Double
sub = 2                                -- ★ 線積分の細かさ：曲線を SUB px ごとに区切る

-- 線電荷 1 つ：位置 [px]（p5.js と同じ、左上が原点）、λ [nC/m]
data Charge = Charge { cX, cY, cQ :: !Double }

-- 曲線の 1 辺：両はし、外向きの法線、その辺の E·n·Δl の合計
data SegInfo = SegInfo { sX1, sY1, sX2, sY2, sNx, sNy, sEn :: !Double }

type Pt = (Double, Double)

data World = World
  { wW, wH     :: !Double
  , wCs        :: [Charge]
  , wLoop      :: [Pt]                 -- 閉じた曲線
  , wDrawing   :: Maybe [Pt]           -- 描いている途中の曲線（新しい点が先頭）
  , wGrab      :: !Int
  , wDirty     :: !Bool
  , wFlux      :: !Double
  , wQIn       :: !Double
  , wSegs      :: [SegInfo]
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-02 Gauss's law" (world0 1280 800))
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 (realToFrac (envW env)) (realToFrac (envH env))) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wDirty w then recompute w else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Double -> Double -> World
world0 wd ht = resetAll (World wd ht [] [] Nothing (-1) True 0 0 [] [])

resetAll :: World -> World
resetAll w = w { wCs = [Charge (cx - 60) cy 2, Charge (cx + 70) (cy + 40) (-1), Charge (cx + 330) (cy - 120) 3]
               , wLoop = circlePts cx cy 190 160, wDirty = True }
  where cx = wW w * 0.42
        cy = wH w * 0.55

circlePts :: Double -> Double -> Double -> Int -> [Pt]
circlePts cx cy r n = [ (cx + r * cos a, cy + r * sin a) | i <- [0 .. n - 1], let a = (2 * pi * fromIntegral i) / fromIntegral n ]

starPts :: Double -> Double -> Double -> Int -> [Pt]
starPts cx cy r n = [ (cx + rr * cos a, cy + rr * sin a)
                    | i <- [0 .. n - 1]
                    , let a = (2 * pi * fromIntegral i) / fromIntegral n
                          rr = r * (1 + 0.35 * sin (5 * a) + 0.12 * cos (3 * a)) ]

-- 電束を数値積分し、中の電荷を巻き数で数える
recompute :: World -> World
recompute w = w { wDirty = False, wFlux = fl, wSegs = segs, wQIn = qin }
  where
    (fl, segs) = fluxThrough (wLoop w) (wCs w) sub
    o = let s = signum (signedArea (wLoop w)) in if s == 0 then 1 else s   -- 曲線の向き（逆回りなら巻き数は −1）
    qin = foldl' (\s c -> s + o * fromIntegral (windingNumber (wLoop w) (cX c) (cY c)) * cQ c) 0 (wCs w)

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- 背景：電場の矢印（向き、明るさ = 強さ）
  [ Color (rgba255 (round (90 + 120 * b)) (round (110 + 100 * b)) (round (150 + 80 * b)) (round (60 + 120 * b)))
          (arrowTo 1 4 (x - 7 * ux, y - 7 * uy) (x + 7 * ux, y + 7 * uy))
  | y <- [30, 66 .. envH env - 1], x <- [20, 56 .. envW env - 1]
  , let (ex, ey) = fieldAt (wCs w) (realToFrac x) (realToFrac y)
        m = hyp ex ey
  , m /= 0
  , let b = f2 (clampv 0 2 (logBase 10 m - 1) / 2)
        ux = f2 (ex / m)
        uy = f2 (ey / m) ]
  -- 曲線：外向きの電束 E·n > 0 は橙、< 0 は青
  ++ [ Color (if sEn s > 0 then rgb255 255 160 70 else rgb255 80 170 255) (thickLine 4 (f2 (sX1 s), f2 (sY1 s)) (f2 (sX2 s), f2 (sY2 s))) | s <- segs ]
  -- ところどころに外向きの法線 n
  ++ [ Color (rgba255 220 220 230 150) (arrowTo 1 5 (mx, my) (mx + 18 * f2 (sNx s), my + 18 * f2 (sNy s)))
     | (k, s) <- zip [0 :: Int ..] segs, k `mod` every == 0
     , let mx = f2 ((sX1 s + sX2 s) / 2); my = f2 ((sY1 s + sY2 s) / 2) ]
  ++ [ Color (rgb255 255 240 150) (polyline 2 [ (f2 x, f2 y) | (x, y) <- reverse d ]) | Just d <- [wDrawing w] ]
  ++ concat [ [ Color (if cQ c > 0 then rgb255 240 90 70 else rgb255 70 150 250) (circleD x y (16 + 6 * sq))
              , if windingNumber (wLoop w) (cX c) (cY c) /= 0 then Color (rgb255 255 240 150) (at (x, y) (ring ((30 + 6 * sq) / 2) 2)) else Blank
              , sayC (if cQ c > 0 then "+" else "-") x (y + 6) 15 white
              , say ((if cQ c > 0 then "+" else "") ++ fmt 1 (cQ c)) (x + 14) (y - 12) 14 (gray 0.86) ]
            | c <- wCs w, let x = f2 (cX c); y = f2 (cY c); sq = f2 (sqrt (abs (cQ c))) ]
  -- 文字
  ++ [ Color (rgba255 0 0 0 160) (rect (10 + bw / 2) (10 + 76) bw 152)
     , say "Drag empty space: draw a curve | drag a charge: move | A/N add  F sign  Up/Down q | 1 circle  2 star  R reset" 20 34 16 (gray 0.92)
     , say ("Flux Phi = closed integral of E.n dl = " ++ fmt 1 (wFlux w) ++ " V  ->  eps0 Phi = " ++ fmt 3 (wFlux w * eps0 * 1e9) ++ " nC/m") 20 66 20 (rgb255 255 230 120)
     , say ("Total charge inside the curve = " ++ fmt 3 (wQIn w) ++ " nC/m (ringed charges)  diff " ++ fmt 4 (abs (wFlux w * eps0 * 1e9 - wQIn w))) 20 96 20 (rgb255 150 230 170)
     , say ("Line charges lambda (nC/m, long lines through the screen), 1 px = 1 mm. Curve of " ++ show (length segs) ++ " edges, cut every " ++ showJ sub ++ " px, summing E.n dl.") 20 122 14 (gray 0.67)
     , say "Orange = flux going out, blue = coming in. Moving the charges outside does not change Phi." 20 144 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    segs = wSegs w
    every = max 1 (length segs `div` 28)
    bw = min (envW env - 20) 1120

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let g = pickMin (-1) [ (d, i) | (i, c) <- zip [0 ..] (wCs w), let d = hyp (cX c - mx) (cY c - my), d < 22 ]
    in pure w { wGrab = g, wDrawing = if g < 0 then Just [(mx, my)] else wDrawing w }
  EventMotion _ | envDown env ->
    if wGrab w >= 0
      then pure w { wCs = updateAt (wGrab w) (\c -> c { cX = clampv 5 (wW w - 5) mx, cY = clampv 5 (wH w - 5) my }) (wCs w), wDirty = True }
      else case wDrawing w of
        Just d@((lx, ly) : _) | hyp (mx - lx) (my - ly) > 5 && length d < 2000 -> pure w { wDrawing = Just ((mx, my) : d) }
        _ -> pure w
  EventKey (MouseButton LeftButton) Up _ _ ->
    let w1 = case wDrawing w of
               Just d | length d >= 3 && abs (signedArea d) > 400 -> w { wLoop = reverse d, wDirty = True }   -- 小さすぎる曲線（クリックだけ）は使わない
               _ -> w
    in pure w1 { wDrawing = Nothing, wGrab = -1 }
  EventKey k Down _ _ -> pure (key k w) { wDirty = True }
  _ -> pure w
  where
    (mx0, my0) = mouseP5 env
    mx = realToFrac mx0
    my = realToFrac my0
    key k w0 =
      let cx = wW w0 * 0.42
          cy = wH w0 * 0.55
          w1 = case k of
            Char '1' -> w0 { wLoop = circlePts cx cy 190 160 }
            Char '2' -> w0 { wLoop = starPts cx cy 180 300 }
            Char c | c `elem` "rR" -> resetAll w0
                   | c `elem` "aAnN" && length (wCs w0) < 8 ->
                       let x = clampv 20 (wW w0 - 20) mx
                           y = clampv 150 (wH w0 - 20) my
                       in if all (\o -> hyp (cX o - x) (cY o - y) > 20) (wCs w0)
                            then w0 { wCs = wCs w0 ++ [Charge x y (if c `elem` "aA" then 1 else -1)] } else w0
            _ -> w0
          i = pickMin (-1) [ (hyp (cX c - mx) (cY c - my), j) | (j, c) <- zip [0 ..] (wCs w1) ]
          upd f = w1 { wCs = updateAt i f (wCs w1) }
      in if i < 0 then w1 else case k of
           Char c | c `elem` "fF" -> upd (\o -> o { cQ = cQ o * (-1) })
           SpecialKey KeyUp -> upd (\o -> o { cQ = signum (cQ o) * min 5 (abs (cQ o) + 0.5) })
           SpecialKey KeyDown -> upd (\o -> o { cQ = signum (cQ o) * max 0.5 (abs (cQ o) - 0.5) })
           _ -> w1

-- いちばん小さい d の番号（同じなら先のもの）
pickMin :: Int -> [(Double, Int)] -> Int
pickMin d xs = snd (foldl' (\(bd, bi) (e, i) -> if e < bd then (e, i) else (bd, bi)) (1e9, d) xs)

-- ---- 自己テスト：電束 = 中の電荷 / ε0 を数値で確かめる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("2D Gauss: E x 2 pi r = lambda / eps0", nearRel (uncurry hyp (fieldAt q1 620 400) * 2 * pi * 0.12) bigQ1 1e-12)
  , ("+1 nC/m inside a circle -> Phi = q/eps0 (within 1%)", nearRel (flux circ q1) bigQ1 0.01)
  , ("charge outside -> Phi ~ 0 (within 1% of q/eps0)", abs (flux circ [Charge 800 420 1]) < 0.01 * bigQ1)
  , ("star-shaped curve: Phi eps0 = total charge inside 1.5 nC/m (within 1%)", nearRel (flux st cs * eps0 * 1e9) 1.5 0.01)
  , ("drawing the curve the other way gives the same Phi (outward n from the sign of the area)", nearRel (flux (reverse st) cs) (flux st cs) 1e-9)
  , ("moving a charge outside does not change Phi", nearRel (flux st cs2) (flux st cs) 0.01)
  , ("winding number: 1 inside a square, 0 outside (-1 the other way round)"
    , windingNumber sq 50 50 == 1 && windingNumber sq 150 50 == 0 && windingNumber (reverse sq) 50 50 == -1) ]
  where
    q1 = [Charge 500 400 1]
    circ = circlePts 500 400 150 720
    bigQ1 = 1e-9 / eps0
    flux p c = fst (fluxThrough p c sub)
    st = starPts 400 400 160 400
    cs = [Charge 380 410 2, Charge 450 360 (-0.5), Charge 700 120 4]
    cs2 = take 2 cs ++ [Charge 120 650 4]
    sq = [(0, 0), (100, 0), (100, 100), (0, 100)]

-- ---------- やってみよう ----------
-- 1. ＋の電荷のまわりに、大きな円・小さな円・ぐにゃぐにゃの曲線を描いて Φ を比べよう
-- 2. ＋2 と −2 を両方かこむと Φ は？ 曲線の上では E·n が 0 でないのに、なぜ合計が 0 になる？
-- 3. 外の電荷を曲線のすぐ近くまで動かすと、橙と青の部分はどう変わる？ Φ は？
-- 4. sub を 20 にすると、電荷が曲線に近いときに誤差が出る。なぜ？
-- 5. 8 の字（自分と交わる曲線）を描くと、中の電荷はどう数えられる？（巻き数）


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 線電荷の電場（2 次元）：E = λ / (2π ε0 r) r̂。電荷の位置は px、λ は nC/m、結果は N/C
fieldAt :: [Charge] -> Double -> Double -> (Double, Double)
fieldAt cs x y = foldl' add (0, 0) cs
  where
    add (ex, ey) c =
      let dx = (x - cX c) * pxM
          dy = (y - cY c) * pxM
          r2 = dx * dx + dy * dy
          f = (cQ c * 1e-9) / (2 * pi * eps0 * r2)
      in if r2 < 1e-12 then (ex, ey) else (ex + f * dx, ey + f * dy)

-- 曲線の辺：(点, 次の点)。最後の点から最初の点へもどる辺もふくむ
edges :: [Pt] -> [(Pt, Pt)]
edges p = zip p (drop 1 p ++ take 1 p)

-- ---- 多角形の符号つき面積（向きを決めるため）
signedArea :: [Pt] -> Double
signedArea p = foldl' (\a ((x1, y1), (x2, y2)) -> a + x1 * y2 - x2 * y1) 0 (edges p) / 2

-- ---- 電束 Φ = ∮ E·n dl（中点則）。n は外向き
fluxThrough :: [Pt] -> [Charge] -> Double -> (Double, [SegInfo])
fluxThrough p cs sb = (foldl' (\a sg -> a + sEn sg) 0 segs, segs)
  where
    s0 = signum (signedArea p)
    s = if s0 == 0 then 1 else s0
    segs = [ SegInfo ax ay bx by nx ny en
           | ((ax, ay), (bx, by)) <- edges p
           , let l = hyp (bx - ax) (by - ay)
           , l /= 0
           , let nx = (s * (by - ay)) / l
                 ny = (-s * (bx - ax)) / l
                 m = max 1 (ceiling (l / sb)) :: Int
                 en = foldl' (\acc k -> let t = (fromIntegral k + 0.5) / fromIntegral m
                                            (ex, ey) = fieldAt cs (ax + t * (bx - ax)) (ay + t * (by - ay))
                                        in acc + (ex * nx + ey * ny) * (l / fromIntegral m) * pxM) 0 [0 .. m - 1] ]

-- ---- 巻き数：点 (x, y) のまわりを曲線が何回まわるか（角度の変化の合計 ÷ 2π）
windingNumber :: [Pt] -> Double -> Double -> Int
windingNumber p x y = floor (tot / (2 * pi) + 0.5)
  where
    tot = foldl' (\t ((ax, ay), (bx, by)) -> t + wrapPi (atan2 (by - y) (bx - x) - atan2 (ay - y) (ax - x))) 0 (edges p)
    wrapPi d | d > pi = wrapPi (d - 2 * pi)
             | d < -pi = wrapPi (d + 2 * pi)
             | otherwise = d

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 線分（太さ w。1 以下なら細い線）
seg :: Float -> Point -> Point -> Picture
seg w a b = if w <= 1 then Line [a, b] else thickLine w a b

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- (x1, y1) → (x2, y2) の矢印。太さ w、hs = 矢じりの大きさ
arrowTo :: Float -> Float -> Point -> Point -> Picture
arrowTo w hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Pictures [ seg w (x1, y1) (x2, y2)
                         , seg w (x2, y2) (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux)
                         , seg w (x2, y2) (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)
