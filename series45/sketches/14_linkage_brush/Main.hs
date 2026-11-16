-- ============================================
-- 45-14 リンク機構ブラシ（お絵かきマシン）
-- 00 の 4 節リンクのカプラ点にペンを付け、紙を回転台でゆっくり回す → 少しずつずれた曲線が重なって模様になる
-- 2 はスピログラフ：内歯車の中を小さな歯車が転がり、歯車の穴に差したペンが花模様を描く
-- 操作：1 = リンク機構 / 2 = スピログラフ / ● をドラッグ = リンクの長さ・ペンの位置 / ←→ = 回転台の速さ・歯数
--       ↑↓ = 描く速さ / T = 回転台 / H = 色 / C = 消す / S = 保存（SVG） / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

maxInk :: Int
maxInk = 6000            -- ★ 覚えておく線分の数（超えると古いものから消える）

dth :: Float
dth = 0.02               -- 1 回に進めるクランク角

-- 紙に描いた線：色と、紙の上の座標の折れ線（新しいものが先頭）
data Stroke = Stroke { skHue :: !Float, skPts :: ![Point] }

data World = World
  { wMachine :: !Int                  -- 1 リンク機構 / 2 スピログラフ
  , wLa, wLb, wLc, wLd :: !Float      -- ★ 4 節リンクの長さ（クランク・カプラ・ロッカー・固定）
  , wCu, wCv   :: !Float              -- ★ ペンの位置（カプラ A→B 方向に cu×b、直角方向に cv）
  , wRatio     :: !Float              -- ★ クランク 1 回転あたりの回転台の回転（割り切れない数ほど細かい模様）
  , wRingZ, wWheelZ :: !Int           -- ★ スピログラフの歯数（内歯車・動く歯車）
  , wHole      :: !Float              -- ★ ペンの穴の位置（歯車の半径に対する比）
  , wSteps     :: !Int                -- 1 フレームに進める回数（↑↓）
  , wTh, wTable, wBranch, wDir :: !Float
  , wTableOn, wRainbow :: !Bool
  , wHue       :: !Float
  , wInk       :: ![Stroke]           -- 紙の上の線
  , wLast      :: !(Maybe Point)      -- 前のペンの位置（紙の座標）
  , wGrab      :: !String
  }

world0 :: World
world0 = World 1 70 250 170 200 1.9 0 0.031 96 52 0.7 8 0 0 (-1) 1 True True 190 [] Nothing ""

main :: IO ()
main = runSketch (sketch "45-14 Linkage brush" world0)
  { skBack  = hsb 212 45 13
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 紙の半径と中心、リンクの台の位置
data Layout = Layout { paperR, pcx, pcy, ox, oy :: !Float }

layout :: Env -> Layout
layout env = Layout r cx cy (cx - r * 0.45 - 356) (cy + 243)   -- リンクの台は紙の左下。ペンが紙の左半分に届く位置
  where
    r = min (envH env * 0.4) (envW env * 0.26)
    cx = envW env - r - 40
    cy = envH env * 0.52

data Sol = Sol { sA, sB, sP :: !Point, sUx, sUy :: !Float }

-- 00 と同じ：4 節リンクのループを解く（A = クランクの先、B = 2 つの円の交点、P = ペン）
solveBar :: Layout -> World -> Float -> Float -> Maybe Sol
solveBar l w theta br
  | e > lb + lc || e < abs (lb - lc) || e < 1e-6 = Nothing
  | otherwise = Just (Sol (ax, ay) (bx, by) (ax + wCu w * lb * ux - wCv w * uy, ay + wCu w * lb * uy + wCv w * ux) ux uy)
  where
    la = wLa w; lb = wLb w; lc = wLc w; ld = wLd w
    ax = ox l + la * cos theta
    ay = oy l - la * sin theta
    dx = ox l + ld - ax
    dy = oy l - ay
    e = sqrt (dx * dx + dy * dy)
    k = (lb * lb - lc * lc + e * e) / (2 * e)
    hh = sqrt (max (lb * lb - k * k) 0)
    bx = ax + (k * dx - br * hh * dy) / e
    by = ay + (k * dy + br * hh * dx) / e
    ux = (bx - ax) / lb
    uy = (by - ay) / lb

-- スピログラフ：角度 t のときの動く歯車の中心、半径、内歯車の半径、自転角、ペンの位置（ハイポトロコイド）
spiro :: Layout -> World -> Float -> (Point, Float, Float, Float, Point)
spiro l w t = ((wx, wy), rw, rr, roll, (wx + wHole w * rw * cos roll, wy + wHole w * rw * sin roll))
  where
    rr = paperR l * 0.92
    rw = rr * fromIntegral (wWheelZ w) / fromIntegral (wRingZ w)
    wx = pcx l + (rr - rw) * cos t
    wy = pcy l + (rr - rw) * sin t
    roll = -t * fromIntegral (wRingZ w - wWheelZ w) / fromIntegral (wWheelZ w)   -- 転がるので逆向きに自転する

-- ★ ペンの位置を「回っている紙の上の座標」に直して線を引く（紙の外ならペンを上げる）
drawOnPaper :: Layout -> Maybe Point -> World -> World
drawOnPaper _ Nothing w = w { wLast = Nothing }
drawOnPaper l (Just (px, py)) w
  | sqrt (dx * dx + dy * dy) > paperR l - 2 = w { wLast = Nothing }
  | otherwise = case wLast w of
      Just _ -> w { wInk = addPt (wInk w), wLast = Just q }
      Nothing -> w { wInk = Stroke (wHue w) [q] : wInk w, wLast = Just q }
  where
    dx = px - pcx l
    dy = py - pcy l
    c = cos (negate (wTable w))
    s = sin (negate (wTable w))
    q = (dx * c - dy * s, dx * s + dy * c)
    -- 色がほとんど同じあいだは同じ折れ線に足す
    addPt (Stroke h ps : rest) | abs (h - wHue w) < 0.5 && length ps < 64 = Stroke h (q : ps) : rest
    addPt st = case wLast w of
      Just p0 -> Stroke (wHue w) [q, p0] : st
      Nothing -> Stroke (wHue w) [q] : st

-- 線分の数が maxInk を超えたら古いものを捨てる
trimInk :: [Stroke] -> [Stroke]
trimInk = go 0
  where
    go _ [] = []
    go n (s : rest)
      | n >= maxInk = []
      | otherwise = s : go (n + length (skPts s) - 1) rest

step :: Env -> World -> World
step env w0
  | wGrab w0 /= "" = w0
  | otherwise = let w = iterate one w0 !! max 0 (wSteps w0) in w { wInk = trimInk (wInk w) }
  where
    l = layout env
    one w =
      let w1 | wMachine w == 1 =
                 let next = wTh w + wDir w * dth
                     wa = case solveBar l w next (wBranch w) of
                            Just _ -> w { wTh = next }
                            Nothing -> w { wDir = negate (wDir w), wBranch = negate (wBranch w) }
                 in drawOnPaper l (fmap sP (solveBar l wa (wTh wa) (wBranch wa))) wa
             | otherwise =
                 let wa = w { wTh = wTh w + dth }
                     (_, _, _, _, p) = spiro l wa (wTh wa)
                 in drawOnPaper l (Just p) wa
      in w1 { wTable = if wTableOn w1 then wTable w1 + dth * wRatio w1 else wTable w1
            , wHue = if wRainbow w1 then wrapF 360 (wHue w1 + 0.02) else wHue w1 }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 紙（回転台ごと回して表示）
  [ dot (hsb 220 30 8) (pcx l) (pcy l) (paperR l * 2 + 10) ]
  ++ [ Color (hsbA h 70 100 55) (Line (map toScreen ps)) | Stroke h ps <- wInk w ]
  ++ [ seg 1 (hsbA 0 0 100 25) (toScreen (paperR l - 14, 0)) (toScreen (paperR l, 0))
     , Color (hsbA 0 0 100 30) (Translate (pcx l) (pcy l) (Circle (paperR l))) ]
  -- ---- 機械
  ++ machine
  -- ---- 文字
  ++ [ txt 20 40 18 (hsb 0 0 100) ("1: linkage  2: spirograph   drag dots: shape   Left/Right: "
                                    ++ (if link then "table ratio " ++ fmt 3 (wRatio w) else "teeth " ++ show (wWheelZ w) ++ " / " ++ show (wRingZ w))
                                    ++ "   Up/Down: speed " ++ show (wSteps w) ++ "   T: table " ++ (if wTableOn w then "ON" else "off")
                                    ++ "   H: color  C: clear  S: save")
     , txt 20 68 15 (hsb 0 0 60)
         (if link then "The coupler curve (00) drawn on a turning sheet. Change one link length and the pattern changes completely."
          else "Petals = ring teeth / gcd. " ++ show rz ++ " and " ++ show wz ++ " -> " ++ show (rz `div` g) ++ " petals, closes after "
               ++ show (wz `div` g) ++ " laps") ]
  ++ (if link
        then [ Color (hsbA 0 0 100 8) (box 24 100 270 96)
             , txt 38 126 14 (hsb 0 0 100) ("a = " ++ fmt 0 (wLa w) ++ "  b = " ++ fmt 0 (wLb w) ++ "  c = " ++ fmt 0 (wLc w) ++ "  d = " ++ fmt 0 (wLd w))
             , txt 38 150 14 (hsb 0 0 100) ("pen: cu = " ++ fmt 2 (wCu w) ++ "  cv = " ++ fmt 0 (wCv w))
             , txt 38 176 14 (hsb 0 0 60) "Drag O4, A, B or the pen dot" ]
        else [])
  where
    l = layout env
    link = wMachine w == 1
    rz = wRingZ w
    wz = wWheelZ w
    g = max 1 (gcd rz wz)
    ca = cos (wTable w)
    sa = sin (wTable w)
    toScreen (x, y) = (pcx l + x * ca - y * sa, pcy l + x * sa + y * ca)
    machine
      | link =
          let o2 = (ox l, oy l)
              o4 = (ox l + wLd w, oy l)
          in [ Color (hsb 0 0 40) (Polygon [(x, y), (x - 14, y + 20), (x + 14, y + 20)]) | (x, y) <- [o2, o4] ]
             ++ case solveBar l w (wTh w) (wBranch w) of
                  Just so ->
                    [ seg 7 (hsbA 8 75 100 85) o2 (sA so), seg 7 (hsbA 200 60 95 85) (sA so) (sB so), seg 7 (hsbA 150 60 85 85) o4 (sB so)
                    , seg 2 (hsbA 200 60 95 55) (sA so) (sP so), seg 2 (hsbA 200 60 95 55) (sB so) (sP so) ]
                    ++ [ dot (hsb 0 0 100) x y 13 | (x, y) <- [o2, o4, sA so, sB so] ]
                    ++ [ dot (hsb (wHue w) 70 100) (fst (sP so)) (snd (sP so)) 14 ]
                  Nothing ->
                    [ txt (ox l - 20) (oy l - 200) 16 (hsb 0 80 100) "Cannot assemble. Move a dot or R"
                    , dot (hsb 0 0 100) (fst o2) (snd o2) 13, dot (hsb 0 0 100) (fst o4) (snd o4) 13
                    , dot (hsb 0 0 100) (ox l + wLa w * cos (wTh w)) (oy l - wLa w * sin (wTh w)) 13 ]
      | otherwise =
          let ((wx, wy), rw, rr, roll, (px, py)) = spiro l w (wTh w)
          in [ Color (hsbA 150 50 80 70) (Translate (pcx l) (pcy l) (ring rr 3))
             , dot (hsbA 200 50 80 12) wx wy (rw * 2)
             , Color (hsbA 200 55 95 80) (Translate wx wy (ring rw 2)) ]
             ++ [ seg 1 (hsbA 200 55 95 50) (wx, wy) (wx + rw * cos a, wy + rw * sin a) | k <- [0 .. 5 :: Int], let a = roll + fromIntegral k * pi / 3 ]
             ++ [ dot (hsb (wHue w) 70 100) px py 14 ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (press env w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wGrab = "" }
  EventMotion _ | wGrab w /= "" -> pure (dragTo env w { wLast = Nothing })
  EventKey (Char k) Down _ _
    | k `elem` "12" -> pure w { wMachine = fromEnum k - fromEnum '0', wLast = Nothing }
    | k `elem` "tT" -> pure w { wTableOn = not (wTableOn w) }
    | k `elem` "hH" -> pure w { wRainbow = not (wRainbow w), wHue = wrapF 360 (wHue w + 67) }
    | k `elem` "cC" -> pure w { wInk = [], wLast = Nothing }
    | k `elem` "sS" -> saveSvg "linkage-brush" env (draw env w) >> pure w
    | k `elem` "rR" -> pure w { wLa = 70, wLb = 250, wLc = 170, wLd = 200, wCu = 1.9, wCv = 0, wTh = 0, wBranch = -1, wDir = 1, wLast = Nothing }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (side (-1))
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (side 1)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSteps = max 2 (min 40 (wSteps w + 2)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSteps = max 2 (min 40 (wSteps w - 2)) }
  _ -> pure w
  where
    side d | wMachine w == 1 = w { wRatio = clampF 0.003 0.5 (wRatio w + 0.004 * d), wLast = Nothing }
           | otherwise = w { wWheelZ = max 20 (min 88 (wWheelZ w + round d)), wLast = Nothing }

press :: Env -> World -> World
press env w
  | wMachine w == 1 =
      let s = solveBar l w (wTh w) (wBranch w)
          cand = [("O4", (ox l + wLd w, oy l)), ("A", (ox l + wLa w * cos (wTh w), oy l - wLa w * sin (wTh w)))]
                 ++ maybe [] (\so -> [("B", sB so), ("P", sP so)]) s
      in w { wGrab = foldl (\g (i, p) -> if dist2 m p < 22 then i else g) "" cand }
  | otherwise =
      let (c, rw, _, _, _) = spiro l w (wTh w)
      in w { wGrab = if dist2 m c < rw + 10 then "hole" else "" }
  where
    l = layout env
    m = mouseP env

dragTo :: Env -> World -> World
dragTo env w0
  | wGrab w0 == "hole" = let (c, rw, _, _, _) = spiro l w0 (wTh w0) in w0 { wHole = clampF 0.05 1.3 (dist2 (mx, my) c / rw) }
  | otherwise = w3
  where
    l = layout env
    (mx, my) = mouseP env
    w1 | wGrab w0 == "O4" = w0 { wLd = clampF 40 420 (mx - ox l) }
       | wGrab w0 == "A" = w0 { wLa = clampF 20 220 (dist2 (mx, my) (ox l, oy l)), wTh = atan2 (oy l - my) (mx - ox l) }
       | otherwise = w0
    ax = ox l + wLa w1 * cos (wTh w1)
    ay = oy l - wLa w1 * sin (wTh w1)
    w2 | wGrab w1 == "B" =
           w1 { wLb = clampF 40 480 (dist2 (mx, my) (ax, ay))
              , wLc = clampF 40 480 (dist2 (mx, my) (ox l + wLd w1, oy l))
              , wBranch = if (ox l + wLd w1 - ax) * (my - ay) - (oy l - ay) * (mx - ax) >= 0 then 1 else -1 }
       | otherwise = w1
    w3 | wGrab w2 == "P" = case solveBar l w2 (wTh w2) (wBranch w2) of
           Just so ->
             let wx = mx - fst (sA so)
                 wy = my - snd (sA so)
             in w2 { wCu = clampF (-2) 3 ((wx * sUx so + wy * sUy so) / wLb w2)
                   , wCv = clampF (-500) 500 (-wx * sUy so + wy * sUx so) }
           Nothing -> w2
       | otherwise = w2

-- ---------- やってみよう ----------
-- 1. リンク機構（1）で C を押してから、ペンの ● を少し動かして描き直そう。同じ機械でも模様がまるで変わる
-- 2. world0 の回転台の比 0.031 を 0.25（ちょうど 1/4）にすると 4 回で同じ線に戻る。0.031 のような半端な数だと？
-- 3. スピログラフ（2）で歯数を 48 / 96 にすると直線（だ円）になる。なぜ？（カルダン歯車）
-- 4. T で回転台を止めて、スピログラフの花びらの数を数えよう。歯数の最大公約数から予想できる
-- 5. 作品づくり：C で消し、H で色を固定 → 形を変えて重ね描き → S で保存。背景色や maxInk も変えてみよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y)、幅 w、高さ h の塗りつぶした長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
