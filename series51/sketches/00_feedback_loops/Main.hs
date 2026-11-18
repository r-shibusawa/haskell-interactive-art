-- ============================================
-- 51-00 負のフィードバックと正のフィードバック
-- 1：部屋とサーモスタット（負のフィードバック）。温度が下がるとヒーターが入り、上がると切れる → 温度が一定に保たれる
-- 2：マイクのハウリング（正のフィードバック）。音が増えるほどもっと増える → 暴走して、アンプの上限で飽和する
-- 操作：1 2 = 切りかえ / ↑↓ = 外気温（1）・ループゲイン G（2） / ←→ = 設定温度 / H = ヒステリシスの幅 / F = サーモスタットを外す
--       スペース・クリック = 窓を開ける（1）・音を入れる（2） / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

tauRoom, heatRate :: Double
tauRoom = 60         -- ★ 部屋の時定数 [秒]（大きいほど冷めにくい）
heatRate = 0.5       -- ★ ヒーターの温め速さ [C/秒]（ヒーターだけなら外気 + 30C まで上がる）

bands :: [Double]
bands = [0.25, 0.5, 1, 2]   -- ★ ヒステリシスの半分の幅 [C]（設定 ± この幅で ON / OFF）

tauHowl :: Double
tauHowl = 0.2        -- ★ 音が 1 周する時間の目安 [秒]

speed1, speed2 :: Double
speed1 = 4           -- 1 フレームで進む時間 = (1/60) × SPEED
speed2 = 0.5

-- 部屋：温度とヒーター（1 = ON、0 = OFF、0.5 = サーモスタットを外して半分の強さ）
data Room = Room { rT :: !Double, rOn :: !Double }

-- 部屋の設定
data RoomP = RoomP { pTau, pHeat, pSetT, pOutT :: !Double, pBandI :: !Int, pThermo, pWindow :: !Bool }

data World = World
  { wMode    :: !Int
  , wSetT    :: !Double         -- ★ 設定温度 [C]
  , wOutT    :: !Double         -- ★ 外気温 [C]
  , wBandI   :: !Int
  , wThermo  :: !Bool
  , wWindow  :: !Bool
  , wG       :: !Double         -- ★ ハウリングのループゲイン（マイク → アンプ → スピーカー → マイク の 1 周の倍率）
  , wRoom    :: !Room
  , wHowl    :: !Double
  , wHist    :: [(Double, Double)]   -- 履歴（新しい順、600 個まで）
  , wTSim    :: !Double
  , wSwitch  :: [Double]        -- ヒーターが入った時刻（新しい順、6 個まで）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-00 Feedback loops" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetAll (World 1 20 5 1 True False 1.5 (Room 17 0) 1e-3 [] 0 [] [])

resetAll :: World -> World
resetAll w = w { wRoom = Room 17 0, wHowl = 1e-3, wHist = [], wTSim = 0, wSwitch = [] }

-- ---- 1：部屋の温度 T' = (外気 − T) / τ + (ヒーター ON なら heatRate)
roomStep :: RoomP -> Double -> Room -> Room
roomStep p dt r = Room (clampv (-50) 80 t') on
  where
    band = getAt 0.5 bands (pBandI p)
    tau = if pWindow p then pTau p / 3 else pTau p
    on | not (pThermo p) = 0.5                      -- 外すと：ヒーターを半分の強さでつけっぱなし
       | rT r < pSetT p - band = 1                  -- サーモスタット：下の線より下で ON、上の線より上で OFF
       | rT r > pSetT p + band = 0
       | otherwise = rOn r
    t' = rT r + dt * ((pOutT p - rT r) / tau + pHeat p * on)

roomParams :: World -> RoomP
roomParams w = RoomP tauRoom heatRate (wSetT w) (wOutT w) (wBandI w) (wThermo w) (wWindow w)

-- ---- 2：ハウリング y' = (G · tanh(y) − y) / τ（tanh = アンプの上限で飽和）
howlDeriv :: [Double] -> (Double, Double) -> [Double]
howlDeriv s (g, tau) = [ (g * tanh y - y) / tau | y <- take 1 s ]

-- 毎フレーム：4 回に分けて進める
step :: World -> World
step w
  | wMode w == 1 =
      let go (!r, !t, sw) _ =
            let r' = roomStep p (speed1 / 240) r
                t' = t + speed1 / 240
            in (r', t', if rOn r' /= rOn r && rOn r' == 1 then t' : sw else sw)
          (room, ts, sws) = foldl' go (wRoom w, wTSim w, wSwitch w) [1 .. 4 :: Int]
      in w { wRoom = room, wTSim = ts, wSwitch = take 6 sws, wHist = take 600 ((rT room, rOn room) : wHist w) }
  | otherwise =
      let go (!y, !t) _ = (clampv (-5) 5 (getAt 0 (rk4 howlDeriv [y] (wG w, tauHowl) (speed2 / 240)) 0), t + speed2 / 240)
          (y', ts) = foldl' go (wHowl w, wTSim w) [1 .. 4 :: Int]
      in w { wHowl = y', wTSim = ts, wHist = take 600 ((y', 0) : wHist w) }
  where p = roomParams w

-- サーモスタットの周期（式）：lo→hi に温める時間 + hi→lo に冷める時間
thermoPeriod :: Double -> Double -> Double -> Double -> Double -> Maybe Double
thermoPeriod lo hi out ton tau
  | not (ton > hi && out < lo) = Nothing
  | otherwise = Just (tau * log ((ton - lo) / (ton - hi)) + tau * log ((hi - out) / (lo - out)))

-- y = G tanh(y) の正の解（2 分法）。G ≤ 1 なら 0
fixedHowl :: Double -> Double
fixedHowl g
  | g <= 1 = 0
  | otherwise = go (200 :: Int) 1e-9 (g + 1)
  where go 0 a b = (a + b) / 2
        go k a b = let m = (a + b) / 2 in if g * tanh m - m > 0 then go (k - 1) m b else go (k - 1) a m

draw :: Env -> World -> Picture
draw env w = view env $
  [ say (if mode1 then "1: Negative feedback - room and thermostat" else "2: Positive feedback - microphone howling") 20 36 20 cTxt ]
  ++ (if mode1
        then [ say ("Up/Down outside " ++ num (wOutT w) ++ " C   Left/Right set " ++ num (wSetT w) ++ " C   H band +-" ++ num band ++ " C") 20 60 15 cSub
             , say ("F thermostat " ++ (if wThermo w then "ON" else "removed") ++ "   Space window " ++ (if wWindow w then "open" else "closed") ++ "   R reset   2 howling") 20 82 15 cSub ]
        else [ say ("Up/Down loop gain G = " ++ fmt 2 (wG w) ++ "   Space/click add sound   R reset   1 thermostat") 20 60 15 cSub ])
  ++ diagram
  ++ numbers
  ++ graph
  ++ [ drawSelfTest env (wTest w) ]
  where
    mode1 = wMode w == 1
    band = getAt 0.5 bands (wBandI w)
    room = wRoom w
    ph = fract (fromIntegral (envFrame env) / 90)
    sw = envW env
    -- ---- ループの図
    bx = 30
    by = 106
    bw = 150
    bh = 56
    gap = 50
    xi i = bx + i * (bw + gap)
    diagram
      | mode1 =
          let e = wSetT w - rT room
              onC = if rOn room > 0 then cTube else cSub
          in [ blockBox (xi 0) by bw bh "Set point" (Just (num (wSetT w) ++ " C")) cBrass
             , blockBox (xi 1) by bw bh "Compare" (Just ("e = " ++ fmt 2 e)) cTube
             , blockBox (xi 2) by bw bh "Thermostat" (Just (if wThermo w then (if rOn room > 0 then "ON" else "OFF") else "removed")) onC
             , blockBox (xi 3) by bw bh "Heater" (Just (fmt 2 (heatRate * rOn room) ++ " C/s")) onC
             , blockBox (xi 3) (by + 130) bw bh "Room" (Just (fmt 2 (rT room) ++ " C")) cScope
             , blockBox (xi 1) (by + 130) bw bh "Thermometer" (Just (fmt 2 (rT room) ++ " C")) cScope ]
             ++ [ flowArrow (xi i + bw) (by + bh / 2) (xi (i + 1)) (by + bh / 2) cTube ph Nothing | i <- [0, 1, 2] ]
             ++ [ flowArrow (xi 3 + bw / 2) (by + bh) (xi 3 + bw / 2) (by + 130) cTube ph Nothing
                , flowArrow (xi 3) (by + 130 + bh / 2) (xi 1 + bw) (by + 130 + bh / 2) cScope ph (Just ("heat leaks to " ++ num (wOutT w) ++ " C"))
                , flowArrow (xi 1 + bw / 2) (by + 130) (xi 1 + bw / 2) (by + bh) cScope ph (Just "- (subtract)") ]
      | otherwise =
          [ blockBox (xi 0) by bw bh "Microphone" (Just (fmt 3 (wHowl w))) cScope
          , blockBox (xi 1) by bw bh "Amp x G" (Just ("G = " ++ fmt 2 (wG w))) cTube
          , blockBox (xi 2) by bw bh "Limit" (Just "tanh") cTube
          , blockBox (xi 3) by bw bh "Speaker" (Just (fmt 3 (wG w * tanh (wHowl w)))) cTube
          , blockBox (xi 1.5) (by + 130) bw bh "Air (room)" (Just ("tau = " ++ num tauHowl ++ " s")) cScope ]
          ++ [ flowArrow (xi i + bw) (by + bh / 2) (xi (i + 1)) (by + bh / 2) cTube ph Nothing | i <- [0, 1, 2] ]
          ++ [ flowArrow (xi 3 + bw / 2) (by + bh) (xi 1.5 + bw) (by + 130 + bh / 2) cTube ph Nothing
             , flowArrow (xi 1.5) (by + 130 + bh / 2) (bx + bw / 2) (by + bh) cScope ph (Just "+ (add)") ]
    -- ---- 計算した数
    mx = xi 4 + 10
    my = by + 14
    numbers
      | mode1 =
          let tau = if wWindow w then tauRoom / 3 else tauRoom
              ton = wOutT w + heatRate * tau
              per = thermoPeriod (wSetT w - band) (wSetT w + band) (wOutT w) ton tau
              sws = wSwitch w
              meas = case (sws, reverse sws) of
                       (lastT : _ : _, firstT : _) -> Just ((lastT - firstT) / fromIntegral (length sws - 1))
                       _ -> Nothing
          in [ say ("ON / OFF lines: " ++ num (wSetT w - band) ++ " / " ++ num (wSetT w + band) ++ " C") mx my 15 cTxt
             , say ("Heater only: up to " ++ fmt 1 ton ++ " C") mx (my + 26) 15 cTxt
             , say (maybe "Period (formula): cannot hold here" (\v -> "Period (formula) = " ++ fmt 2 v ++ " s") per) mx (my + 52) 15 cTube
             , say "  = tau ln(..) + tau ln(..)" mx (my + 74) 14 cSub
             , say ("Period (measured) = " ++ maybe "-" (\v -> fmt 2 v ++ " s") meas) mx (my + 100) 15 cScope
             , say ("Error e = " ++ fmt 2 (wSetT w - rT room) ++ " C") mx (my + 126) 15 cTxt
             , say (if wThermo w then "F removes it: then T drifts with outside" else "Removed: change outside with Up/Down") mx (my + 152) 14 cSub ]
      | otherwise =
          let g = wG w
              ys = fixedHowl g
              rate = (g - 1) / tauHowl
          in [ say "Small: y ~ e^((G - 1) t / tau)" mx my 15 cTxt
             , say ("Growth (G - 1)/tau = " ++ fmt 2 rate ++ " /s") mx (my + 26) 15 cTube
             , say (if g > 1 then "Doubling time = " ++ fmt 3 (log 2 / rate) ++ " s" else "G <= 1: the sound dies out") mx (my + 52) 15 cTube
             , say ("Settles at y* = G tanh(y*) -> " ++ fmt 4 ys) mx (my + 78) 15 cScope
             , say (if g > 1 then "Runaway -> stuck at the limit (howl)" else "A small sound becomes quiet again") mx (my + 104) 15 (if g > 1 then cAlarm else cScope) ]
    -- ---- 時間グラフ
    gx = 40
    gy = by + 230
    gwid = sw - 80
    ght = envH env - gy - 50
    hist = reverse (wHist w)
    graph
      | mode1 =
          let lo = min (wSetT w - 6) (wOutT w)
              hi = wSetT w + 6
          in [ plotFrame gx gy gwid ght ("Room T (" ++ num lo ++ " to " ++ num hi ++ " C), heater (bottom strip)   width = " ++ show (round (600 / 60 * speed1) :: Int) ++ " s")
             , hLine (wSetT w + band) gx gy gwid ght lo hi cTube
             , hLine (wSetT w - band) gx gy gwid ght lo hi cTube
             , hLine (wSetT w) gx gy gwid ght lo hi cBrass
             , plotLine (map fst hist) gx gy gwid ght lo hi cScope 600 2.5
             , plotLine (map snd hist) gx (gy + ght - 30) gwid 24 0 1 cTube 600 2 ]
      | otherwise =
          let hi = max 1.2 (fixedHowl (max (wG w) 1) * 1.2)
          in [ plotFrame gx gy gwid ght ("Microphone level y (0 to " ++ fmt 2 hi ++ ")   width = " ++ show (round (600 / 60 * speed2) :: Int) ++ " s")
             , hLine (fixedHowl (wG w)) gx gy gwid ght 0 hi cTube
             , plotLine (map (abs . fst) hist) gx gy gwid ght 0 hi cTube 600 2.5 ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '1' -> resetAll w { wMode = 1 }
    | c == '2' -> resetAll w { wMode = 2 }
    | c `elem` "hH" -> w { wBandI = (wBandI w + 1) `mod` length bands }
    | c `elem` "fF" -> w { wThermo = not (wThermo w) }
    | c `elem` "rR" -> resetAll w
  EventKey (SpecialKey KeyUp) Down _ _
    | wMode w == 1 -> w { wOutT = min 18 (wOutT w + 1) }
    | otherwise -> w { wG = min 3 (wG w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _
    | wMode w == 1 -> w { wOutT = max (-15) (wOutT w - 1) }
    | otherwise -> w { wG = max 0.2 (wG w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSetT = min 28 (wSetT w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSetT = max 12 (wSetT w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> kick w
  EventKey (MouseButton LeftButton) Down _ _ -> kick w
  _ -> w

kick :: World -> World
kick w
  | wMode w == 1 = w { wWindow = not (wWindow w) }
  | otherwise = w { wHowl = clampv (-5) 5 (wHowl w + 0.02) }

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("after settling, T stays in 19.5..20.5 C (overshoot <= 0.01 C)", lo > 19.5 - 0.01 && hi < 20.5 + 0.01)
  , ("ON -> ON period matches tau ln(15.5/14.5) x 2 = " ++ fmt 3 perF ++ " s within 1%", length ons > 5 && abs (per - perF) / perF < 0.01)
  , ("narrower band -> more switching (+-0.25 C " ++ show n1 ++ " > +-2 C " ++ show n3 ++ ")", n1 > 3 * n3 && n3 > 0)
  , ("outside -5 C: with feedback mean ~20 C (" ++ fmt 2 mean ++ "), without -> 10 C", abs (mean - 20) < 0.6 && near (rT ro) 10 0.01)
  , ("positive feedback G = 1.5: small sound x e^2.5 = 12.18 in 1 s (1%)", abs (getAt 0 s1 0 / 1e-4 / exp 2.5 - 1) < 0.01)
  , ("runaway stops at the limit: y -> y* = 1.5 tanh(y*) = " ++ fmt 4 (fixedHowl 1.5)
    , near (getAt 0 s2 0) (fixedHowl 1.5) 1e-6 && near (1.5 * tanh (fixedHowl 1.5)) (fixedHowl 1.5) 1e-9)
  , ("G = 0.8 < 1: the sound dies (0.5 -> " ++ showEFloat (Just 1) (getAt 0 q 0) "" ++ " in 20 s)", abs (getAt 0 q 0) < 1e-6) ]
  where
    p = RoomP 60 0.5 20 5 1 True False
    dt = 0.01
    -- 600 秒まわし、300 秒より後の最低・最高と ON になった時刻
    loop (!r, !t, !l, !h, os) | t >= 600 = (l, h, reverse os)
                              | otherwise =
      let r' = roomStep p dt r
          t' = t + dt
          late = t' > 300
      in loop (r', t', if late then min l (rT r') else l, if late then max h (rT r') else h
              , if late && rOn r == 0 && rOn r' /= 0 then t' : os else os)
    (lo, hi, ons) = loop (Room 12 0, 0, 1e9, -1e9, [])
    per = case (ons, reverse ons) of
            (a : _ : _, b : _) -> (b - a) / fromIntegral (length ons - 1)
            _ -> 0
    perF = maybe 1 id (thermoPeriod 19.5 20.5 5 35 60)
    n1 = countSwitches 0
    n3 = countSwitches 3
    pc = p { pOutT = -5 }
    po = pc { pThermo = False }
    (_, ro, sc, nn) = foldl' (\(!a, !b, !s, !n) k -> let a' = roomStep pc dt a
                                                         b' = roomStep po dt b
                                                     in if k > 30000 then (a', b', s + rT a', n + 1) else (a', b', s, n))
                             (Room 20 0, Room 20 0, 0, 0 :: Int) [0 .. 59999 :: Int]
    mean = sc / fromIntegral (max 1 nn)
    iter n f x = foldl' (\v _ -> f v) x [1 .. n :: Int]
    s1 = iter 100 (\s -> rk4 howlDeriv s (1.5, 0.2) 0.01) [1e-4]
    s2 = iter 2000 (\s -> rk4 howlDeriv s (1.5, 0.2) 0.01) s1
    q = iter 2000 (\s -> rk4 howlDeriv s (0.8, 0.2) 0.01) [0.5]

countSwitches :: Int -> Int
countSwitches bi = snd (foldl' go (Room 20 0, 0) [1 .. 40000 :: Int])
  where
    p = RoomP 60 0.5 20 5 bi True False
    go (!r, !n) _ = let r' = roomStep p 0.01 r in (r', if rOn r == 0 && rOn r' /= 0 then n + 1 else n)

-- ---------- やってみよう ----------
-- 1. H で幅を ±0.25C にすると温度の波はどうなる？ ヒーターの ON/OFF の回数は？（機械が早くこわれる理由）
-- 2. F でサーモスタットを外し、↑↓ で外気温を変える。温度はどこまでずれる？ もう一度 F で戻すと？
-- 3. スペースで窓を開けると、周期（式）はどう変わる？ 測った周期と比べよう
-- 4. 2 のハウリングで G を 1.00 ちょうどにすると？ 0.95 と 1.05 で何が起きる？
-- 5. tauHowl を変えると「2 倍になる時間」はどう変わる？ 本物のハウリングが一瞬で起きるのはなぜ？

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- グラフの中の水平線（目標値など、点線）
hLine :: Double -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Picture
hLine v x y w h lo hi c =
  let yy = y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h
  in Color (col c 70) (Pictures [ Line [(x + s, yy), (x + min w (s + 6), yy)] | s <- [0, 11 .. w] ])

-- ブロック図の箱（名前と今の値）
blockBox :: Float -> Float -> Float -> Float -> String -> Maybe String -> HSB -> Picture
blockBox x y w h name val c = Pictures $
  [ Color (hsb 25 40 16) (rectTL x y w h), Color (col c 100) (frameTL 2 x y w h)
  , sayC name (x + w / 2) (y + h / 2 - (if val == Nothing then 0 else 10)) 15 cTxt ]
  ++ [ sayC v (x + w / 2) (y + h / 2 + 12) 14 c | Just v <- [val] ]

-- 信号の矢印：点が流れて「ループを回る」ようすを見せる（phase = 0〜1）
flowArrow :: Float -> Float -> Float -> Float -> HSB -> Float -> Maybe String -> Picture
flowArrow x1 y1 x2 y2 c phase lab = Pictures $
  [ Color (col c 100) (thickLine 2 (x1, y1) (x2, y2))
  , Color (col c 100) (Polygon [(x2, y2), (x2 - 11 * cos (a - 0.4), y2 - 11 * sin (a - 0.4)), (x2 - 11 * cos (a + 0.4), y2 - 11 * sin (a + 0.4))]) ]
  ++ [ Color (col c 100) (at (x1 + (x2 - x1) * f, y1 + (y2 - y1) * f) (disc 3)) | k <- [0, 1, 2], let f = fract (phase + k / 3) ]
  ++ [ say s ((x1 + x2) / 2 + 6) ((y1 + y2) / 2 - 6) 14 cSub | Just s <- [lab] ]
  where a = atan2 (y2 - y1) (x2 - x1)

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 4 次のルンゲ＝クッタ法（f s p = ds/dt、s はリスト）
rk4 :: ([Double] -> p -> [Double]) -> [Double] -> p -> Double -> [Double]
rk4 f s p h =
  let k1 = f s p
      k2 = f (zipWith (\v k -> v + h / 2 * k) s k1) p
      k3 = f (zipWith (\v k -> v + h / 2 * k) s k2) p
      k4 = f (zipWith (\v k -> v + h * k) s k3) p
  in zipWith5 (\v a b c d -> v + h / 6 * (a + 2 * b + 2 * c + d)) s k1 k2 k3 k4
  where zipWith5 g (a : as) (b : bs) (c : cs) (d : ds) (e : es) = g a b c d e : zipWith5 g as bs cs ds es
        zipWith5 _ _ _ _ _ _ = []

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
