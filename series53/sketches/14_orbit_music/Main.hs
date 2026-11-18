-- ============================================
-- 53-14 惑星の音楽 ─ 公転の周期の比を、そのままリズムと音の高さにする
-- 天体が基準の線（観測者の方向）を通るたびに音を 1 つ鳴らす。内側ほど速く回るので、細かいリズムになる
-- 木星の衛星イオ・エウロパ・ガニメデは周期の比がほぼ 1 : 2 : 4（ラプラス共鳴）。n₁ − 3n₂ + 2n₃ = 0 で、
-- 角度 φ = λ₁ − 3λ₂ + 2λ₃ はいつも 180° のまま → 3 つは決して一列にそろわない。F = 周期の比をそのまま周波数の比にした和音
-- 操作：1 ガリレオ衛星 2 TRAPPIST-1 3 内惑星 / ↑↓ = テンポ / スペース = 再生/停止 / F = 和音 / 音量は小さめから
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 天体の集まり：名前、周期の単位、(天体の名前, 公転周期 [日])
data System = System { syName :: String, syUnit :: String, syBodies :: [(String, Double)] }

-- 公転周期 [日]（ガリレオ衛星・TRAPPIST-1 の惑星・水星〜火星）
systems :: [System]
systems =
  [ System "Galilean moons (Jupiter)" "d" [("Io", 1.769138), ("Europa", 3.551181), ("Ganymede", 7.154553), ("Callisto", 16.689017)]
  , System "TRAPPIST-1 planets" "d" [("b", 1.51087), ("c", 2.42182), ("d", 4.04961), ("e", 6.09965), ("f", 9.20669), ("g", 12.35294), ("h", 18.7672)]
  , System "Inner planets (Sun)" "d" [("Mercury", 87.969), ("Venus", 224.701), ("Earth", 365.256), ("Mars", 686.98)] ]

scaleSteps :: [Int]
scaleSteps = [0, 2, 4, 7, 9, 12, 14, 16]   -- ★ 音階（ペンタトニック、半音の数）。内側の天体ほど高い音

baseF :: Double
baseF = 220                            -- ★ いちばん外側の天体の音 [Hz]

tempos :: [[Double]]
tempos = [[0.5, 1, 2, 4, 8], [0.5, 1, 2, 4, 8], [20, 40, 80, 160, 320]]   -- 1 秒あたりに進む日数

data World = World
  { wSys     :: !Int
  , wTp      :: !Int
  , wDays    :: !Double
  , wPlaying :: !Bool
  , wPhase0  :: [Double]               -- はじめの経度 [度]
  , wFlash   :: [Double]               -- 音が鳴ったときに光る（だんだん消える）
  , wHits    :: [(Int, Float)]         -- 鳴った音 (天体, 時刻 [秒])。古い順
  , wQueue   :: [Note]                 -- 予約した音（和音）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-14 Orbit music" world0)
  { skBack  = rgb255 6 8 24
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = choose 0 (World 0 1 0 True [] [] [] [] [])

sysOf :: World -> System
sysOf w = getAt (System "" "d" []) systems (wSys w)

bodiesOf :: Int -> [(String, Double)]
bodiesOf k = syBodies (getAt (System "" "d" []) systems k)

-- はじめの経度：イオだけ 180° ずらす（ラプラスの角 φ = 180°）
phasesOf :: Int -> [Double]
phasesOf k = [ if k == 0 && i == 0 then 180 else 0 | (i, _) <- zip [0 :: Int ..] (bodiesOf k) ]

choose :: Int -> World -> World
choose k w = w { wSys = k, wDays = 0, wHits = [], wPhase0 = phasesOf k, wFlash = map (const 0) (bodiesOf k) }

-- 平均の経度 [度]（円軌道）
lonOf :: [(String, Double)] -> [Double] -> Int -> Double -> Double
lonOf bs ph i t = getAt 0 ph i + 360 * t / snd (getAt ("", 1) bs i)

-- ラプラスの角 φ = λイオ − 3λエウロパ + 2λガニメデ
laplace :: Double -> Double
laplace t = wrap360 (l 0 - 3 * l 1 + 2 * l 2)
  where l i = lonOf (bodiesOf 0) (phasesOf 0) i t

noteFreqOf :: Int -> Int -> Double
noteFreqOf i n = baseF * 2 ** (fromIntegral (getAt 0 scaleSteps (min (length scaleSteps - 1) (n - 1 - i))) / 12)

-- 周期の比をそのまま周波数の比に：f = 基準 × (いちばん外側の周期 / 自分の周期)。ガリレオ衛星なら 1 : 2 : 4 = オクターブ
chordFreqs :: [(String, Double)] -> [Double]
chordFreqs bs = filter (< 4000) [ 110 * (pn / p) | (_, p) <- bs ]
  where pn = snd (getAt ("", 1) bs (length bs - 1))

step :: Env -> Float -> World -> IO World
step env _ w = do
  let bs = syBodies (sysOf w)
      n = length bs
      dt = if wPlaying w then getAt 1 (getAt [] tempos (wSys w)) (wTp w) / 60 else 0
      t0 = wDays w
      t1 = t0 + dt
      lon = lonOf bs (wPhase0 w)
      -- 基準の線（経度 0°）を通った天体
      crossed = [ i | dt > 0, i <- [0 .. n - 1], floor (lon i t1 / 360) > (floor (lon i t0 / 360) :: Integer) ]
      now = envTime env
  mapM_ (\i -> tone (noteFreqOf i n) 0.25 0.5 Triangle) crossed
  q <- playDue now (wQueue w)
  pure w { wDays = t1
         , wFlash = [ if i `elem` crossed then 1 else f * 0.9 | (i, f) <- zip [0 ..] (wFlash w) ]
         , wHits = filter ((>= now - 6) . snd) (wHits w ++ [ (i, now) | i <- crossed ])
         , wQueue = q }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 軌道（半径 ∝ 周期^(2/3)：ケプラーの第 3 法則）
  [ Color (rgba255 255 230 160 90) (Line [(cx, cy), (cx + rm * 1.08, cy)])
  , Color (rgb255 255 210 120) (circleD cx cy 24) ]
  ++ concat [ [ Color (rgba255 120 140 200 80) (Line [ (cx + r * cos t, cy + r * sin t) | k <- [0 .. 72 :: Int], let t = tau * fromIntegral k / 72 ])
              , Color (bodyCol i) (circleD bx by (12 + 16 * f2 (getAt 0 (wFlash w) i)))
              , say nm (bx + 10) (by - 8) 14 (rgb255 220 225 240) ]
            | (i, (nm, p)) <- zip [0 ..] bs
            , let r = rm * f2 ((p / pmax) ** (2 / 3))
            , let a = f2 (lonOf bs (wPhase0 w) i (wDays w) * degR)
            , let bx = cx + r * cos a, let by = cy - r * sin a ]
  -- ---- 音の譜面（過去 6 秒）
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx0 gy0 gw0 gh0) ]
  ++ [ Color (bodyCol i) (rectTL (gx0 + gw0 - (envTime env - ms) / 6 * gw0) (gy0 + (fromIntegral i + 0.2) / fromIntegral n * gh0) 6 (0.6 * gh0 / fromIntegral n))
     | (i, ms) <- wHits w ]
  ++ [ say "Score: past 6 s (right = now)" gx0 (gy0 - 8) 14 (rgb255 200 205 225)
     , say "Top = inner body = higher note" gx0 (gy0 + gh0 + 20) 14 (rgb255 200 205 225) ]
  -- ---- 周期の表
  ++ [ say "Body" gx0 y0 14 c0, say "Period" (gx0 + 95) y0 14 c0, say "Ratio to prev." (gx0 + 225) y0 14 c0, say "Note" (gx0 + 360) y0 14 c0 ]
  ++ concat [ [ say nm gx0 yy 14 c1
              , say (showJ p ++ " " ++ syUnit (sysOf w)) (gx0 + 95) yy 14 c1
              , say (if i > 0 then fmt 3 (p / snd (getAt ("", 1) bs (i - 1))) else "-") (gx0 + 225) yy 14 c1
              , say (fmt 0 (noteFreqOf i n) ++ " Hz") (gx0 + 360) yy 14 (rgb255 250 225 140) ]
            | (i, (nm, p)) <- zip [0 :: Int ..] bs, let yy = y0 + 22 * fromIntegral (i + 1) ]
  ++ [ say "Orbit music | 1 Jupiter  2 TRAPPIST-1  3 Inner planets  Up/Down tempo  Space play  F chord" 20 34 17 (rgb255 240 236 215)
     , say (syName (sysOf w) ++ "   day " ++ fmt 1 (wDays w) ++ " (" ++ showJ (getAt 1 (getAt [] tempos (wSys w)) (wTp w)) ++ " days/s)") 20 60 16 (rgb255 250 225 140) ]
  ++ [ say ("Laplace angle phi = L_Io - 3 L_Europa + 2 L_Ganymede = " ++ fmt 2 (laplace (wDays w)) ++ " deg (always 180)") 20 84 15 (rgb255 250 225 140) | wSys w == 0 ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    bs = syBodies (sysOf w)
    n = length bs
    cx = envW env * 0.3
    cy = envH env * 0.52
    rm = min (envW env * 0.25) (envH env * 0.38)
    pmax = snd (getAt ("", 1) bs (n - 1))
    bodyCol i = hsbCol (fromIntegral i / fromIntegral (max 1 n) * 300) 0.6 1
    gx0 = envW env * 0.6
    gw0 = envW env - gx0 - 30
    gy0 = 110
    gh0 = envH env * 0.4
    y0 = gy0 + gh0 + 44
    c0 = rgb255 240 236 215
    c1 = rgb255 215 220 235

-- 色相 h（0〜360）、彩度 s、明るさ v → 色
hsbCol :: Double -> Double -> Double -> Color
hsbCol h s v = makeColor (f2 (f 5)) (f2 (f 3)) (f2 (f 1)) 1
  where f k = let q = jsRem (k + h / 60) 6 in v - v * s * max 0 (minimum3 q (4 - q) 1)
        minimum3 a b c = min a (min b c)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure (choose (fromEnum c - fromEnum '1') w)
    | c `elem` "fF" -> pure w { wQueue = wQueue w ++ [ Note (envTime env + 0.05 + fromIntegral i * 0.25) f (2 - fromIntegral i * 0.25) 0.35 Sine
                                                      | (i, f) <- zip [0 :: Int ..] (chordFreqs (syBodies (sysOf w))) ] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wTp = min 4 (wTp w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wTp = max 0 (wTp w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Laplace relation n_Io - 3 n_Europa + 2 n_Ganymede ~ 0 (within 0.01 deg/day)", abs (g 0 - 3 * g 1 + 2 * g 2) < 0.01)
  , ("period ratios Europa/Io ~ 2, Ganymede/Europa ~ 2 (within 1%)", near (p0 1 / p0 0) 2 0.02 && near (p0 2 / p0 1) 2 0.02)
  , ("Laplace angle phi stays 180 deg after 1000 days (within 1 deg)", dev < 1)
  , ("TRAPPIST-1: c/b ~ 8/5, d/c ~ 5/3, e/d ~ 3/2 (resonance chain within 1%)", near (p1 1 / p1 0) 1.6 0.016 && near (p1 2 / p1 1) (5 / 3) 0.017 && near (p1 3 / p1 2) 1.5 0.015)
  , ("Galilean moon frequencies ~ 4 : 2 : 1 (octaves, within 1.5%)", near (ch 0 / ch 2 / 4) 1 0.015 && near (ch 1 / ch 2 / 2) 1 0.015)
  , ("master volume <= 0.2", master <= 0.2) ]
  where
    p0 i = snd (getAt ("", 1) (bodiesOf 0) i)
    p1 i = snd (getAt ("", 1) (bodiesOf 1) i)
    g i = 360 / p0 i                    -- 平均運動 [度/日]
    ch i = 1 / p0 i
    dev = foldl' (\m t -> max m (abs (wrap180 (laplace t - 180)))) 0 (takeWhile (<= 1000) (iterate (+ 3.7) 0))

-- ---------- やってみよう ----------
-- 1. 1 のガリレオ衛星を聞こう。イオ 4 回、エウロパ 2 回、ガニメデ 1 回のリズム
-- 2. F で和音。1 : 2 : 4 はオクターブなので、きれいに重なって聞こえる
-- 3. 2 の TRAPPIST-1 は 7 つの惑星の共鳴の鎖。どんなリズムに聞こえる？
-- 4. 3 の内惑星は共鳴していない。リズムは規則正しい？
-- 5. scaleSteps や baseF を変えて、自分の「天体の曲」を作ろう

-- ==================================================
-- ここから下は「天文の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 度 → ラジアン
degR :: Double
degR = pi / 180

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 0〜360 にそろえる（JavaScript の ((a % 360) + 360) % 360 と同じ）
wrap360 :: Double -> Double
wrap360 a = jsRem (jsRem a 360 + 360) 360

wrap180 :: Double -> Double
wrap180 a = wrap360 (a + 180) - 180

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 (fin 0 t)) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 (fin 0 t)) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgb255 80 90 120) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (rgb255 160 170 195) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 5) (gY g v + 6) 14 (rgb255 160 170 195) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 38) 14 (rgb255 205 210 225)
     , say yl (gx g) (gy g - 8) 14 (rgb255 205 210 225) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

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

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = show v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- ---- 音の道具（Art.Sound の playTone で鳴らす）
-- 音量の上限：全体の音量 master は 0.2 以下（大きな音は出ない）。1 音の大きさ vol も 0〜1 に制限
master :: Double
master = 0.15                          -- ★ 全体の音量（0.2 以下にしておく）

-- 予約した 1 音：鳴らす時刻 [秒]、周波数、長さ、大きさ、波の形
data Note = Note { nAt :: !Float, nF, nDur, nVol :: !Double, nWave :: !Wave }

-- 1 音を鳴らす：周波数 f、長さ dur 秒、大きさ vol（0〜1）、波の形。全体の音量 master をかける
tone :: Double -> Double -> Double -> Wave -> IO ()
tone f dur vol wave
  | isNaN f || isInfinite f || f <= 0 || isNaN vol || isInfinite vol = pure ()
  | otherwise = playTone (realToFrac (clampv 20 12000 f)) (realToFrac (min 0.2 master * clampv 0 1 vol)) (realToFrac (clampv 0.03 2 dur)) wave

-- 時刻が来た音を鳴らし、残りを返す
playDue :: Float -> [Note] -> IO [Note]
playDue now ns = do
  mapM_ (\n -> tone (nF n) (nDur n) (nVol n) (nWave n)) (filter ((<= now) . nAt) ns)
  pure (filter ((> now) . nAt) ns)
