-- ============================================
-- 45-00 4 節リンク機構
-- 4 本の棒をピンでつないだ、いちばん基本の機構。クランク a を回すとロッカー c が揺れ、カプラ b 上の点が曲線を描く
-- グラスホフの条件：最短 s + 最長 l ≤ 残り p + q なら、どれかのリンクが 1 回転できる
-- 操作：● をドラッグ = リンクの長さを変える / 1〜4 = 見本 / B = 組み立て方を反転 / ↑↓ = 速さ / スペース = 止める / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import Numeric (showFFloat)

data World = World
  { wLa, wLb, wLc, wLd :: !Float   -- ★ クランク a、カプラ b、ロッカー c、固定リンク d（px）
  , wCu, wCv   :: !Float           -- ★ カプラ点 P の位置（A→B 方向に cu×b、直角方向に cv）
  , wTh        :: !Float           -- クランク角
  , wDir       :: !Float           -- 回る向き
  , wBranch    :: !Float           -- 組み立て方（+1 / −1）
  , wSpeed     :: !Float           -- ★ 回る速さ（ラジアン/フレーム）
  , wPlaying   :: !Bool
  , wGrab      :: !String          -- つかんでいる点（"" = なし）
  }

world0 :: World
world0 = World 90 260 180 240 0.5 90 0.5 1 1 0.03 True ""

main :: IO ()
main = runSketch (sketch "45-00 Four-bar linkage" world0)
  { skBack  = hsb 212 45 13
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 固定ピン O2 の位置（p5.js の座標）
origin :: Env -> Point
origin env = (envW env * 0.3, envH env * 0.66)

-- 解いた形：A はクランクの先、B はカプラとロッカーのピン、P はカプラ点、(ux, uy) は A→B の向き
data Sol = Sol { sA :: !Point, sB :: !Point, sP :: !Point, sUx :: !Float, sUy :: !Float }

-- ★ ループ閉じ込みの式を解く：B は「A 中心・半径 b の円」と「O4 中心・半径 c の円」の交点
solveBar :: Env -> World -> Float -> Float -> Maybe Sol
solveBar env w theta br
  | e > lb + lc || e < abs (lb - lc) || e < 1e-6 = Nothing   -- 円が交わらない = 組み立てられない
  | otherwise = Just (Sol (ax, ay) (bx, by) (pxx, pyy) ux uy)
  where
    (ox, oy) = origin env
    la = wLa w; lb = wLb w; lc = wLc w; ld = wLd w
    ax = ox + la * cos theta
    ay = oy - la * sin theta
    dx = ox + ld - ax
    dy = oy - ay
    e = sqrt (dx * dx + dy * dy)
    k = (lb * lb - lc * lc + e * e) / (2 * e)
    hh = sqrt (max (lb * lb - k * k) 0)
    bx = ax + (k * dx - br * hh * dy) / e
    by = ay + (k * dy + br * hh * dx) / e
    ux = (bx - ax) / lb
    uy = (by - ay) / lb
    pxx = ax + wCu w * lb * ux - wCv w * uy
    pyy = ay + wCu w * lb * uy + wCv w * ux

-- グラスホフの条件で機構の種類を判定する
classify :: World -> (Float, Float, String, Bool)
classify w = (sl, pq, kind, sl <= pq + 0.5)
  where
    v = sort [wLa w, wLb w, wLc w, wLd w]
    (v0, v1, v2, v3) = case v of
      [a, b, c, d] -> (a, b, c, d)
      _ -> (0, 0, 0, 0)
    sl = v0 + v3
    pq = v1 + v2
    kind | abs (sl - pq) < 0.5 = "Change point (s + l = p + q)"
         | sl > pq = "Double rocker (non-Grashof: no link turns fully)"
         | wLd w == v0 = "Double crank (ground link is shortest)"
         | wLb w == v0 = "Double rocker (coupler is shortest: it turns)"
         | otherwise = "Crank-rocker (crank is shortest)"

-- 回す：解けなくなったら死点。向きを反転し、組み立て方も入れ替わる
step :: Env -> World -> World
step env w
  | not (wPlaying w) || wGrab w /= "" = w
  | ok next = w { wTh = next }
  | ok (wTh w) = w { wDir = negate (wDir w), wBranch = negate (wBranch w) }
  | otherwise = w
  where
    next = wTh w + wDir w * wSpeed w
    ok t = isJust' (solveBar env w t (wBranch w))

isJust' :: Maybe a -> Bool
isJust' = maybe False (const True)

-- 組み立てられるクランク角を探す
findStart :: Env -> World -> World
findStart env w = case [t | i <- [0 .. 359 :: Int], let t = wTh w + fromIntegral i / 360 * tau, isJust' (solveBar env w t (wBranch w))] of
  (t : _) -> w { wTh = t }
  [] -> w

-- カプラ曲線：クランク角を 1 周ぶん試して P をつなぐ（解けない角度では線を切る）
couplerCurve :: Env -> World -> Float -> Float -> Color -> Picture
couplerCurve env w br wd c = Pictures (map (path wd c) (pieces pts))
  where
    pts = [fmap sP (solveBar env w (fromIntegral i / 360 * tau) br) | i <- [0 .. 360 :: Int]]
    pieces xs = case dropWhile (not . isJust' ) xs of
      [] -> []
      ys -> let (run, rest) = span isJust' ys in [p | Just p <- run] : pieces rest

draw :: Env -> World -> Picture
draw env w = view env $
  [ couplerCurve env w (negate (wBranch w)) 1.5 (hsbA 35 60 70 35)
  , couplerCurve env w (wBranch w) 3 (hsbA 35 90 100 90)
  -- 固定リンク（地面）
  , seg 3 (hsb 0 0 45) (ox - 40, oy + 22) (fst o4 + 40, oy + 22) ]
  ++ [ Color (hsb 0 0 40) (Polygon [(x, y), (x - 16, y + 22), (x + 16, y + 22)]) | (x, y) <- [(ox, oy), o4] ]
  ++ mech
  ++ [ txt 20 40 18 (hsb 0 0 100) ("Drag dots: link lengths   1-4: presets   B: flip assembly   Up/Down: speed   Space: "
                                    ++ (if wPlaying w then "stop" else "run") ++ "   R: reset")
     , txt 20 68 15 (hsb 0 0 60) "Turn the crank (red): coupler point P (yellow) traces a coupler curve - walking robots and wipers use it."
     , Color (hsbA 0 0 100 8) (rect (pnx - 16 + 210) (pny - 30 + 109) 420 218)
     , txt pnx pny 16 (hsb 0 0 100) ("a = " ++ fmt 0 (wLa w) ++ "  b = " ++ fmt 0 (wLb w) ++ "  c = " ++ fmt 0 (wLc w) ++ "  d = " ++ fmt 0 (wLd w))
     , txt pnx (pny + 30) 16 (hsb 0 0 100) "Grashof condition: s + l <= p + q"
     , txt pnx (pny + 56) 16 (hsb (if gOk then 140 else 0) 70 100)
         ("s + l = " ++ fmt 0 sl ++ (if gOk then " <= " else " > ") ++ "p + q = " ++ fmt 0 pq ++ (if gOk then "  -> holds" else "  -> fails"))
     , txt pnx (pny + 86) 15 (hsb 35 90 100) kind ]
  ++ transmission
  where
    (ox, oy) = origin env
    o4 = (ox + wLd w, oy)
    s = solveBar env w (wTh w) (wBranch w)
    (sl, pq, kind, gOk) = classify w
    pnx = envW env - 430
    pny = 120
    mech = case s of
      Just so ->
        let a = sA so; b = sB so; p = sP so in
        [ Color (hsbA 200 50 90 22) (Polygon [a, b, p])
        , seg 10 (hsb 8 75 100) (ox, oy) a          -- クランク a
        , seg 10 (hsb 200 60 95) a b                -- カプラ b
        , seg 10 (hsb 150 60 85) o4 b               -- ロッカー c
        , seg 2 (hsbA 200 60 95 60) a p, seg 2 (hsbA 200 60 95 60) b p ]
        ++ [ dot (hsb 0 0 100) x y 16 | (x, y) <- [(ox, oy), o4, a, b] ]
        ++ [ dot (hsb 35 90 100) (fst p) (snd p) 16
           , txt (ox - 34) (oy + 4) 14 (hsb 0 0 100) "O2", txt (fst o4 + 14) (oy + 4) 14 (hsb 0 0 100) "O4 (drag)"
           , txt (fst a + 12) (snd a - 10) 14 (hsb 0 0 100) "A", txt (fst b + 12) (snd b - 10) 14 (hsb 0 0 100) "B"
           , txt (fst p + 12) (snd p - 10) 14 (hsb 0 0 100) "P" ]
      Nothing ->
        [ txt (ox - 60) (oy - 200) 20 (hsb 0 90 100) "Cannot assemble with these lengths (b + c too short / too long). Move a dot or R"
        , dot (hsb 0 0 100) ox oy 16, dot (hsb 0 0 100) (fst o4) (snd o4) 16
        , dot (hsb 8 75 100) (ox + wLa w * cos (wTh w)) (oy - wLa w * sin (wTh w)) 16 ]
    -- 伝達角 μ：カプラとロッカーのなす角。90° に近いほど力がよく伝わる
    transmission = case s of
      Just so ->
        let (ax, ay) = sA so; (bx, by) = sB so
            mu = acos (clampF (-1) 1 (((ax - bx) * (fst o4 - bx) + (ay - by) * (snd o4 - by)) / (wLb w * wLc w)))
            muDeg = mu * 180 / pi
            bad = muDeg < 40 || muDeg > 140
        in [ txt pnx (pny + 116) 16 (hsb 0 (if bad then 80 else 0) 100)
               ("Transmission angle mu = " ++ fmt 0 muDeg ++ " deg" ++ (if bad then " (poor force transfer)" else ""))
           , txt pnx (pny + 142) 14 (hsb 0 0 60) ("Crank angle = " ++ fmt 0 (wrapF 360 (wTh w * 180 / pi)) ++ " deg   assembly "
                                                ++ (if wBranch w > 0 then "+" else "-"))
           , txt pnx (pny + 164) 14 (hsb 0 0 60) "B solves |B - A| = b and |B - O4| = c (2 ways)" ]
      Nothing -> []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = "" }
  EventMotion _ | wGrab w /= "" -> dragTo env w
  EventKey (Char k) Down _ _
    | Just [a, b, c, d] <- lookup k presets -> findStart env w { wLa = a, wLb = b, wLc = c, wLd = d }
    | k `elem` "bB" -> findStart env w { wBranch = negate (wBranch w) }
    | k `elem` "rR" -> world0 { wSpeed = wSpeed w, wPlaying = wPlaying w }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = clampF 0.005 0.12 (wSpeed w + 0.005) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = clampF 0.005 0.12 (wSpeed w - 0.005) }
  _ -> w
  where
    presets = [('1', [90, 260, 180, 240]), ('2', [200, 220, 210, 80]), ('3', [200, 80, 210, 240]), ('4', [150, 180, 160, 300])]

press :: Env -> World -> World
press env w = w { wGrab = pick }
  where
    (ox, oy) = origin env
    m = mouseP env
    s = solveBar env w (wTh w) (wBranch w)
    cand = [("O4", (ox + wLd w, oy)), ("A", (ox + wLa w * cos (wTh w), oy - wLa w * sin (wTh w)))]
           ++ maybe [] (\so -> [("B", sB so), ("P", sP so)]) s
    -- p5.js と同じく、条件に合う最後の候補を選ぶ
    pick = foldl (\g (i, p) -> if dist2 m p < 22 then i else g) "" cand

dragTo :: Env -> World -> World
dragTo env w0 = w3
  where
    (ox, oy) = origin env
    (mx, my) = mouseP env
    w1 | wGrab w0 == "O4" = w0 { wLd = clampF 40 520 (mx - ox) }
       | wGrab w0 == "A" = w0 { wLa = clampF 30 320 (dist2 (mx, my) (ox, oy)), wTh = atan2 (oy - my) (mx - ox) }
       | otherwise = w0
    ax = ox + wLa w1 * cos (wTh w1)
    ay = oy - wLa w1 * sin (wTh w1)
    w2 | wGrab w1 == "B" =
           let dx = ox + wLd w1 - ax
               dy = oy - ay
           in w1 { wLb = clampF 40 520 (dist2 (mx, my) (ax, ay))
                 , wLc = clampF 40 520 (dist2 (mx, my) (ox + wLd w1, oy))
                 , wBranch = if dx * (my - ay) - dy * (mx - ax) >= 0 then 1 else -1 }
       | otherwise = w1
    w3 | wGrab w2 == "P" = case solveBar env w2 (wTh w2) (wBranch w2) of
           Just so ->
             let wx = mx - fst (sA so)
                 wy = my - snd (sA so)
             in w2 { wCu = clampF (-1.5) 2.5 ((wx * sUx so + wy * sUy so) / wLb w2)
                   , wCv = clampF (-400) 400 (-wx * sUy so + wy * sUx so) }
           Nothing -> w2
       | otherwise = w2

-- ---------- やってみよう ----------
-- 1. 1〜4 を押して種類を見比べよう。2（両クランク）ではロッカーも 1 回転する。どのリンクが最短？
-- 2. 4（非グラスホフ）ではクランクが途中で止まって引き返す。止まる瞬間、A・B・O4 は一直線（死点）
-- 3. 点 P をドラッグして、ほぼ直線になる部分を持つカプラ曲線を探そう（ワットやチェビシェフの近似直線機構）
-- 4. 伝達角 μ が 40° を切らないように a〜d を決めるのが設計の基本。1 の見本で μ の最小値を読み取ろう
-- 5. world0 の cu = 0.5, cv = 0 にすると P は棒の上。曲線はどう変わる？

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

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
