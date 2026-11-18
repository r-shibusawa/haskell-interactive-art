-- ============================================
-- 54-14 テルミン風 ─ 手とアンテナの「静電容量」で音の高さが変わる楽器
-- 手（導体）がアンテナに近づくと、そのあいだの静電容量 C手 がふえる → LC 発振器の周波数 f = 1/(2π√(L(C0 + C手))) が少し下がる
-- 本物のテルミンは 約 500 kHz の発振器 2 つの「差」（うなり）を聞こえる音にする：Δf = f固定 − f可変。ここでも同じ計算で音を鳴らす
-- 操作：マウス左右 = 手とアンテナの距離 / マウス上下 = 音量 / クリック = 音を出す / スペース = 音 ON・OFF / ↑↓ = 手の大きさ
-- 音量は小さめから（全体の音量 master は 0.2 以下）。続く音の代わりに、短い音を 0.15 秒ごとにくり返して鳴らす
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

eps0 :: Double
eps0 = 8.854e-12

lH :: Double
lH = 2.5e-3                            -- ★ コイル [H]（2.5 mH）

c0 :: Double
c0 = 40e-12                            -- ★ 発振器のコンデンサ [F]（40 pF）

bAnt :: Double
bAnt = 0.002                           -- アンテナの「大きさ」[m]（球とみなした実効的な半径）

noteEvery :: Float
noteEvery = 0.15                       -- ★ 短い音をくり返す間隔 [秒]

data World = World
  { wHandR   :: !Double                -- ★ 手の大きさ [m]（球とみなした半径）。はじめ 0.05
  , wSoundOn :: !Bool
  , wAudio   :: !Bool                  -- クリックかキーで音を出しはじめた
  , wNext    :: !Float                 -- 次の音を鳴らす時刻 [秒]
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-14 Theremin" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.05 True False 0 []

fRef :: Double
fRef = 1 / (2 * pi * sqrt (lH * c0))

fVar :: Double -> Double -> Double
fVar hr d = 1 / (2 * pi * sqrt (lH * (c0 + handCap hr d)))

beat :: Double -> Double -> Double
beat hr d = fRef - fVar hr d

-- マウス → 距離（2〜60 cm）と音量
antXOf :: Env -> Double
antXOf env = realToFrac (envW env) - 160

handDistOf :: Env -> Double
handDistOf env = clampv 0.02 0.6 (((antX - realToFrac mx) / (antX - 60)) * 0.6)
  where antX = antXOf env
        (mx, _) = mouseP5 env

volOf :: Env -> Double
volOf env = clampv 0 1 (1 - (realToFrac my - 120) / (realToFrac (envH env) - 200))
  where (_, my) = mouseP5 env

-- 音：0.15 秒ごとに Δf の短い音を鳴らす
step :: Env -> Float -> World -> IO World
step env _ w
  | wAudio w && envTime env >= wNext w = do
      let df = beat (wHandR w) (handDistOf env)
          v = if wSoundOn w then volOf env else 0
      setPitch df v
      pure w { wNext = envTime env + noteEvery }
  | otherwise = pure w

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- アンテナと手
  [ Color (gray 0.75) (thickLine 6 (ax, ht * 0.25) (ax, ht * 0.7))
  , Color (gray 0.75) (rectTL (ax - 30) (ht * 0.7) 60 30)
  , Color (rgb255 240 200 170) (at (hx, ht * 0.45) (Scale (70 * k) (90 * k) (disc 0.5)))
  , Color (rgba255 255 230 120 120) (dashed 4 6 [(hx + 30, ht * 0.45), (ax - 6, ht * 0.45)])
  , say ("distance d = " ++ fmt 1 (hd * 100) ++ " cm") ((hx + ax) / 2 - 50) (ht * 0.45 - 12) 15 (rgb255 255 230 120)
  , say ("C_hand = " ++ fmt 3 (ch * 1e12) ++ " pF") ((hx + ax) / 2 - 50) (ht * 0.45 + 24) 15 (rgb255 255 230 120)
  -- しくみの図（ブロック）
  , box bx by "Fixed oscillator" ("f1 = " ++ fmt 3 (fRef / 1000) ++ " kHz") (rgb255 150 200 255)
  , box bx (by + bh + gap) "Variable (C0 + C_hand)" ("f2 = " ++ fmt 3 (fVar hr hd / 1000) ++ " kHz") (rgb255 255 190 140)
  , box (bx + bw + 40) yMid "Mixer (multiply)" "f1-f2 and f1+f2" (rgb255 200 160 255)
  , box (bx + 2 * bw + 80) yMid "Low-pass filter" "removes high f1+f2" (rgb255 160 230 170)
  , box (bx + 3 * bw + 120) yMid "Speaker" ("df = " ++ fmt 1 df ++ " Hz") (rgb255 255 230 120)
  , Color (gray 0.63) $ Pictures
      [ arrowTo 2 7 (bx + bw, by + bh / 2) (bx + bw + 40, yMid + 18)
      , arrowTo 2 7 (bx + bw, by + bh + gap + bh / 2) (bx + bw + 40, yMid + 36)
      , arrowTo 2 7 (bx + 2 * bw + 40, yMid + bh / 2) (bx + 2 * bw + 80, yMid + bh / 2)
      , arrowTo 2 7 (bx + 3 * bw + 80, yMid + bh / 2) (bx + 3 * bw + 120, yMid + bh / 2) ]
  -- うなり：2 つの波を足すと、ふくらみ（包絡線）が Δf でくり返す
  , Color (rgb255 70 80 100) (frameTL 1 gx gy gw gh)
  , Color (rgba255 200 160 255 160) (polyline 1.5 envP)
  , Color (rgba255 200 160 255 160) (polyline 1.5 envN)
  , Color (rgb255 255 230 120) (polyline 2.5 outP)
  , say "Purple = envelope of f1 + f2 added (the 500 kHz wave inside is too fine to draw)," gx (gy - 26) 14 (gray 0.78)
  , say ("yellow = the sound df after the filter (width " ++ fmt 1 (tT * 1000) ++ " ms)") gx (gy - 8) 14 (gray 0.78)
  -- 数字
  , say ("Pitch df = " ++ fmt 1 df ++ " Hz (" ++ noteName df ++ ")   volume " ++ fmt 0 (vol * 100) ++ "%   "
         ++ (if wAudio w then (if wSoundOn w then "playing" else "sound OFF") else "click to start the sound")) 40 ty 16 (gray 0.92)
  , say "C_hand is below 1 pF (f2 changes only about 0.2%), but the difference is a few hundred Hz." 40 (ty + 22) 14 (gray 0.67)
  , say "The hand's capacitance uses the two-sphere formula C ~ 4 pi eps0 a b / d." 40 (ty + 40) 14 (gray 0.67)
  , say ("Mouse left/right = hand distance  up/down = volume  click = sound  Space on/off  Up/Down hand size (" ++ showJ (hr * 100) ++ " cm)") 20 34 17 (gray 0.92)
  , say "Start with a low volume. Be careful with headphones." 20 60 14 (rgb255 255 200 120)
  , drawSelfTest env (wTest w) ]
  where
    ht = envH env
    ax = f2 (antXOf env)
    hr = wHandR w
    hd = handDistOf env
    vol = volOf env
    df = beat hr hd
    ch = handCap hr hd
    hx = ax - f2 (hd / 0.6) * (ax - 60)
    k = f2 (hr / 0.05)
    bx = 40; by = 110; bw = 220; bh = 54; gap = 26
    yMid = by + (bh + gap) / 2
    box x y t1 t2 c = Pictures [ Color c (frameTL 2 x y bw bh), say t1 (x + 10) (y + 22) 14 c, say t2 (x + 10) (y + 42) 14 (gray 0.86) ]
    gx = 40; gy = ht * 0.62; gw = ax - 240; gh = ht * 0.18
    tT = 3 / max df 1
    ts = [ (fromIntegral i / 300, (fromIntegral i / 300) * tT) | i <- [0 .. 300 :: Int] ]
    envP = [ (gx + u * gw, gy + gh / 2 - (f2 e * gh) / 4.4) | (u, t) <- ts, let e = abs (2 * cos (pi * df * t)) ]
    envN = [ (gx + u * gw, gy + gh / 2 + (f2 e * gh) / 4.4) | (u, t) <- ts, let e = abs (2 * cos (pi * df * t)) ]
    outP = [ (gx + u * gw, gy + gh / 2 - f2 (cos (2 * pi * df * t)) * gh * 0.3) | (u, t) <- ts ]
    ty = ht * 0.88

-- ---- マウスとキー（最初のクリックかキーで音を出しはじめる）
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wAudio = True }
  EventKey k Down _ _ -> pure (key k w { wAudio = True })
  _ -> pure w
  where
    key k w0 = case k of
      SpecialKey KeySpace -> w0 { wSoundOn = not (wSoundOn w0) }
      SpecialKey KeyUp -> w0 { wHandR = min 0.1 (wHandR w0 + 0.01) }
      SpecialKey KeyDown -> w0 { wHandR = max 0.02 (wHandR w0 - 0.01) }
      _ -> w0

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("LC oscillator f = 1/(2 pi sqrt(LC)) = 503.3 kHz", nearRel fRef 503292 1e-4)
  , ("multiply + low-pass extracts the difference frequency 300 Hz (within 2%)", nearRel fb (f2' - f1') 0.02)
  , ("closer hand -> larger C_hand and higher pitch", handCap 0.05 0.05 > handCap 0.05 0.2 && beat 0.05 0.05 > beat 0.05 0.2 && beat 0.05 0.2 > 0)
  , ("small C_hand: df ~ f dC/(2 C0) (within 1%)", nearRel (beat 0.05 d) approx 0.01)
  , ("volume limit: master <= 0.2", master <= 0.2)
  , ("note names: 440 Hz = A4, 261.6 Hz = C4", noteName 440 == "A4" && noteName 261.63 == "C4") ]
  where
    -- うなりの検出：10 kHz と 10.3 kHz を数値で作り、かけ算 → 平均（ローパス）→ 0 をよぎる回数で周波数
    fs = 200000; f1' = 10000; f2' = 10300; n = 20000 :: Int; win = 20 :: Int
    (_, _, _, crossR) = foldl' one (Seq.empty, 0, 1, []) [0 .. n - 1]
    one (buf, s, prev, cross) i =
      let t = fromIntegral i / fs :: Double
          m = sin (2 * pi * f1' * t) * sin (2 * pi * f2' * t)
          buf1 = buf Seq.|> m
          s1 = s + m
          (buf2, s2) = if Seq.length buf1 > win
                         then case Seq.viewl buf1 of
                                x Seq.:< rest -> (rest, s1 - x)
                                Seq.EmptyL -> (buf1, s1)
                         else (buf1, s1)
          lp = s2 / fromIntegral (Seq.length buf2)
          -- ヒステリシス：−0.1 より下がってから +0.1 をこえたら 1 回
          (prev', cross')
            | i > win && prev < (0 :: Int) && lp > 0.1 = (1, t : cross)
            | lp < -0.1 = (-1, cross)
            | otherwise = (prev, cross)
      in s2 `seq` (buf2, s2, prev', cross')
    crosses = reverse crossR
    fb = case (crosses, crossR) of
      (c1 : _, cl : _) -> fromIntegral (length crosses - 1) / (cl - c1)
      _ -> 0
    d = 0.3
    approx = (fRef * handCap 0.05 d) / (2 * c0)

-- ---------- やってみよう ----------
-- 1. 手を近づけると音が高くなる。距離を半分にすると Δf は何倍？（C手 ∝ 1/d）
-- 2. ↑↓ で手の大きさを変えると、同じ距離でも音が変わる。なぜ？
-- 3. c0 を 2 倍にすると、f₁ と 音の高さ Δf はどう変わる？
-- 4. ドレミを弾くには、手をどの距離に置けばいい？ noteName で確かめよう
-- 5. 本物のテルミンは音量用のアンテナもある。マウスの上下の代わりに、もう 1 つの静電容量で音量を決めてみよう
-- 6. noteEvery を短くすると、音はつながって聞こえる？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 手（半径 hr の球）とアンテナ（半径 bAnt の球）の間の静電容量：遠いとき C ≈ 4πε0·a·b/d
handCap :: Double -> Double -> Double
handCap hr d = (4 * pi * eps0 * hr * bAnt) / max d 0.01

-- 周波数 → 音名（A4 = 440 Hz）
noteName :: Double -> String
noteName f
  | not (f > 0) || isInfinite f = "-"
  | otherwise = getAt "?" names (n `mod` 12) ++ show (n `div` 12)
  where
    n = floor (12 * logBase 2 (f / 440) + 0.5) + 57 :: Int
    names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- ---- 音の道具（Art.Sound の playTone で鳴らす）
master :: Double
master = 0.12                          -- ★ 全体の音量（0.2 以下にしておく）

-- 音の高さ f [Hz] と大きさ v（0〜1）で短い音を 1 つ鳴らす（少し長めにして次の音とつなぐ）
setPitch :: Double -> Double -> IO ()
setPitch f v
  | v <= 0 || isNaN v = pure ()
  | otherwise = playTone (realToFrac (clampv 30 4000 (if isNaN f || isInfinite f then 100 else f)))
                         (realToFrac (min 0.2 master * clampv 0 1 v * 0.8)) (noteEvery + 0.03) Sine

-- ---- 数の道具
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
        | abs (v - fromIntegral (round v :: Integer)) < 1e-9 && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

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

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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
