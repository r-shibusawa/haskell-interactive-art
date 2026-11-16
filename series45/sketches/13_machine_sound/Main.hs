-- ============================================
-- 45-13 機械の音 🔊
-- 機械の音の高さは「1 秒に何回ぶつかるか」で決まる
--   歯車：かみ合い周波数 = 歯数 × 回転数 / 60　　エンジン（4 サイクル）：爆発の周波数 = 気筒数 × 回転数 / 120
-- 操作：まずクリックで音を出す / 1 = 歯車 / 2 = エンジン / ↑↓（押し続ける）= 回転数 / ←→ = 歯数・気筒数
--       E = 偏心（歯車）・失火（エンジン）/ M = 消音
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)

cylList :: [Int]
cylList = [1, 2, 3, 4, 6, 8]   -- ★ 気筒数の候補

volume :: Float
volume = 0.12                  -- ★ 音の大きさ（0〜0.3）。解析の信号はこの大きさで作る

playGain :: Float
playGain = 2.5                 -- ★ 実際に鳴らすときの倍率（ブラウザの圧縮器の代わり）

slowMo :: Float
slowMo = 1 / 30                -- ★ 絵のスローモーション倍率

pa :: Float
pa = 20 * pi / 180             -- 圧力角 20°

rate :: Float
rate = 22050                   -- 音の標本化周波数（Art.Sound と同じ）

fftN :: Int
fftN = 4096                    -- 解析の窓（約 0.19 秒。ブラウザの 8192 点 / 44.1 kHz と同じ長さ）

hzPerBin :: Float
hzPerBin = rate / fromIntegral fftN

data World = World
  { wMachine :: !Int           -- 1 歯車 / 2 エンジン
  , wRpm     :: !Float         -- ★ 回転数（1 分あたり）
  , wZ       :: !Int           -- ★ 駆動歯車の歯数
  , wCylId   :: !Int
  , wFault   :: !Bool          -- 偏心・失火（異常）
  , wMuted   :: !Bool
  , wStarted :: !Bool          -- 音を出したか
  , wCrank   :: !Float         -- 回転角（絵のため）
  , wLastPlay :: !Float        -- 最後に音を鳴らした時刻
  , wOutA, wOutB :: ![Point]   -- 歯車の輪郭
  , wSpec    :: !(U.Vector Float)   -- なめらかにした振幅
  }

world0 :: World
world0 = rebuild (World 1 900 20 3 False False False 0 (-10) [] [] (U.replicate (fftN `div` 2) 0))

rebuild :: World -> World
rebuild w = w { wOutA = makeGearOutline (wZ w) 9 pa 1 1.25, wOutB = makeGearOutline 30 9 pa 1 1.25 }

main :: IO ()
main = runSketch (sketch "45-13 Machine sound" world0)
  { skBack  = hsb 212 45 13
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 音の高さを決める 2 つの周波数（Hz）：主の音と遅い周期
frequencies :: World -> (Float, Float, String, String)
frequencies w
  | wMachine w == 1 = (fromIntegral (wZ w) * shaft, shaft, "mesh frequency = teeth x rpm / 60", "shaft rotation")
  | otherwise = (fromIntegral (nCyl w) * wRpm w / 120, wRpm w / 120, "firing frequency = cylinders x rpm / 120", "one cycle (2 turns)")
  where shaft = wRpm w / 60

nCyl :: World -> Int
nCyl w = case drop (wCylId w) cylList of
  (n : _) -> n
  [] -> 4

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let r1 | isDown env (SpecialKey KeyUp) = wRpm w0 * 1.012
         | isDown env (SpecialKey KeyDown) = wRpm w0 / 1.012
         | otherwise = wRpm w0
      w1 = w0 { wRpm = clampF 200 7000 r1 }
      (fMain, fSlow, _, _) = frequencies w1
      crank = wCrank w1 + wRpm w1 / 60 * 2 * pi * (1 / 60) * slowMo
      now = envTime env
  -- ★ 連続音の代わりに 0.3 秒の音を 0.25 秒ごとに鳴らす：主の音（のこぎり波）＋ 2 倍音（矩形波）
  --   異常のときは音量のゆれ（AM 変調）を、同じ意味の側帯波 f ± 遅い周期 で鳴らす
  w2 <- if wStarted w1 && not (wMuted w1) && now - wLastPlay w1 >= 0.25
          then do
            let v = volume * playGain
                q f = fromIntegral (round f :: Int)
            playTone (q (clampF 5 8000 fMain)) v 0.3 Sawtooth
            playTone (q (clampF 5 12000 (fMain * 2))) (v * 0.25) 0.3 Square
            when (wFault w1) $ do
              playTone (q (max 5 (fMain - fSlow))) (v * 0.4) 0.3 Sawtooth
              playTone (q (fMain + fSlow)) (v * 0.4) 0.3 Sawtooth
            pure w1 { wLastPlay = now }
          else pure w1
  -- 鳴らしている音と同じ信号をプログラムの中で作って、スペクトルを求める
  let spec | wStarted w2 = U.zipWith (\o n -> 0.6 * o + 0.4 * n) (wSpec w2) (analyse w2 now)
           | otherwise = wSpec w2
  pure w2 { wCrank = crank, wSpec = spec }

-- ★ Web Audio の音と同じ信号：(のこぎり波 f + 0.25 × 矩形波 2f) → ローパス → 音量 → AM 変調
signal :: World -> Float -> U.Vector Float
signal w now = U.fromListN fftN (lowpass cutoff raw)
  where
    (fMain, fSlow, _, _) = frequencies w
    vol = if wMuted w then 0 else volume
    cutoff = if wMachine w == 1 then 5000 else min (fMain * 8) 3000
    depth = if wFault w then 0.8 else 0
    f1 = clampF 5 8000 fMain
    f2 = clampF 5 12000 (fMain * 2)
    t0 = now - fromIntegral fftN / rate
    raw = [ let t = t0 + fromIntegral i / rate
                saw = 2 * fract (f1 * t) - 1
                sq = if fract (f2 * t) < 0.5 then 1 else -1
            in (saw + 0.25 * sq, vol * (1 + depth * sin (tau * clampF 0.1 200 fSlow * t)))
          | i <- [0 .. fftN - 1] ]

-- 2 次のローパスフィルター（RBJ の式、Q = 1/√2）。音量と AM はフィルターの後でかける
lowpass :: Float -> [(Float, Float)] -> [Float]
lowpass fc = go 0 0 0 0
  where
    w0 = tau * clampF 100 (rate * 0.45) fc / rate
    alpha = sin w0 / (2 * 0.7071)
    a0 = 1 + alpha
    b0 = (1 - cos w0) / 2 / a0
    b1 = (1 - cos w0) / a0
    b2 = b0
    a1 = -2 * cos w0 / a0
    a2 = (1 - alpha) / a0
    go _ _ _ _ [] = []
    go x1 x2 y1 y2 ((x, g) : rest) =
      let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
      in y * g : go x x1 y y1 rest

-- 窓（Blackman）+ FFT で振幅を求める（ブラウザの AnalyserNode と同じ手順）
blackman :: U.Vector Float
blackman = U.generate fftN (\i -> let a = tau * fromIntegral i / fromIntegral fftN in 0.42 - 0.5 * cos a + 0.08 * cos (2 * a))

analyse :: World -> Float -> U.Vector Float
analyse w now =
  U.map (/ fromIntegral fftN) (fftMag (U.zipWith (*) (signal w now) blackman))

-- 振幅を 0〜255 に（−100 dB〜−30 dB を割り当てる）
toByte :: Float -> Float
toByte m = clampF 0 255 (255 / 70 * (20 * logBase 10 (max 1e-9 m) + 100))

draw :: Env -> World -> Picture
draw env w = view env $
  machine
  -- ---- スペクトル（どの高さの音が入っているか）
  ++ [ seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh) ]
  ++ (if wStarted w
        then [ Color (hsb 190 70 100) (box (gx + fromIntegral i * hzPerBin / fTop * gw) (gy + gh - v) (max (gw / fromIntegral nb * 0.8) 1) v)
             | i <- [1 .. nb - 1], let v = toByte (wSpec w U.! i) / 255 * gh, v > 2 ]
        else [])
  -- 理論の周波数に印
  ++ concat [ [ Color (hsb 50 90 100) (box (gx + f / fTop * gw - 1) gy 2 10) ]
              ++ [ txt (gx + f / fTop * gw + 4) (gy + 10) 14 (hsb 50 90 100) (if k == 1 then fmt 0 f ++ " Hz" else "x" ++ show k)
                 | k == 1 || fMain / fTop * gw > 60 ]
            | k <- takeWhile (\k -> fMain * fromIntegral k <= fTop) [1 .. 6 :: Int], let f = fMain * fromIntegral k ]
  ++ [ txt (gx + f / fTop * gw - 10) (gy + gh + 16) 14 (hsb 0 0 60) (fmt 0 f) | k <- [0 .. 8 :: Int], let f = fTop / 8 * fromIntegral k ]
  ++ [ txt (gx + gw - 110) (gy + gh + 34) 14 (hsb 0 0 60) "frequency (Hz)"
  -- ---- 文字
     , txt 20 40 18 (hsb 0 0 100) ("1: gears  2: engine   Up/Down: rpm " ++ fmt 0 (wRpm w) ++ "   Left/Right: " ++ (if gear then "teeth" else "cylinders")
                                    ++ "   E: " ++ (if gear then "eccentric" else "misfire") ++ " " ++ onOff (wFault w) ++ "   M: mute " ++ onOff (wMuted w))
     , txt 20 68 15 (hsb 0 0 60) (if wStarted w
                                    then "Picture is " ++ show (round (1 / slowMo) :: Int) ++ "x slow motion, sound is real speed. Spectrum peaks match the yellow marks."
                                    else "> Click the canvas first to start the sound")
     , Color (hsbA 0 0 100 8) (box (ix - 14) (iy - 26) (envW env - ix - 20) 190)
     , txt ix iy 15 (hsb 0 0 100) mainName
     , txt ix (iy + 34) 22 (hsb 50 90 100)
         (if gear then show (wZ w) ++ " x " ++ fmt 0 (wRpm w) ++ " / 60 = " ++ fmt 0 fMain ++ " Hz"
                  else show (nCyl w) ++ " x " ++ fmt 0 (wRpm w) ++ " / 120 = " ++ fmt 0 fMain ++ " Hz")
     , txt ix (iy + 64) 14 (hsb 0 0 70) (slowName ++ ": " ++ fmt 1 fSlow ++ " Hz")
     , txt ix (iy + 88) 14 (hsb 0 0 70) ("as a note: " ++ noteName fMain)
     , txt ix (iy + 116) 14 (if wFault w then hsb 0 70 100 else hsb 0 0 55)
         (if wFault w
            then (if gear then "Eccentric: wobble each turn, sidebands +-" ++ fmt 1 fSlow ++ " Hz"
                          else "Misfire: 1 cylinder skips, wobble per 2 turns")
            else "Normal. Press E for a fault, then diagnose it")
     , txt ix (iy + 140) 14 (hsb 0 0 55) "Diagnosis reads peak positions and sidebands" ]
  where
    gear = wMachine w == 1
    (fMain, fSlow, mainName, slowName) = frequencies w
    ccy = envH env * 0.36
    gx = 60
    gw = envW env - 120
    gh = envH env * 0.2
    gy = envH env - 60 - gh
    fTop = if gear then 4000 else 1000
    nb = min (fftN `div` 2) (floor (fTop / hzPerBin))
    ix = envW env * 0.62
    iy = 130
    crank = wCrank w
    onOff b = if b then "ON" else "off"
    machine
      | gear =
          -- ---- 歯車：歯が 1 枚かみ合うたびに 1 回「カチッ」
          let z = wZ w
              r1 = 9 * fromIntegral z / 2
              r2 = 9 * 30 / 2
              ccx = envW env * 0.34
              fit = min 1 (envH env * 0.42 / (2 * max r1 r2 + 30))
              wob = if wFault w then 5 else 0                -- 偏心：軸の中心がずれている
              rotB = pi + pi / 30 - crank * fromIntegral z / 30
              toothPhase = fract (crank * fromIntegral z / (2 * pi))
              place = Translate ccx ccy . Scale fit fit
          in [ place (Pictures
                 [ gearPic (hsb 200 55 75) z (wOutA w) (-r1 + wob * cos crank) (wob * sin crank) crank
                 , gearPic (hsb 28 60 80) 30 (wOutB w) r2 0 rotB
                 , dot (hsb 0 0 100) (-r1) 0 12, dot (hsb 0 0 100) r2 0 12
                 , dot (hsb 0 0 100) (-r1 + (r1 - 22) * cos crank) ((r1 - 22) * sin crank) 9
                 -- かみ合いの瞬間のフラッシュ
                 , dot (hsbA 50 90 100 ((1 - toothPhase) * 90)) 0 0 (30 * (1 - toothPhase) + 8) ])
             , txt (ccx - r1 * fit - 40) (ccy + max r1 r2 * fit + 34) 14 (hsb 0 0 100) ("teeth " ++ show z ++ " (Left/Right)") ]
      | otherwise =
          -- ---- エンジン：気筒が順番に爆発する（2 回転 = 720° で全気筒が 1 回ずつ）
          let n = nCyl w
              cw = min 90 (envW env * 0.6 / fromIntegral n)
              x0 = envW env * 0.34 - fromIntegral n * cw / 2
          in concat
               [ [ Color (if firing then hsb 20 90 100 else hsb 0 0 22) (box (x - cw * 0.36) (ccy - 110) (cw * 0.72) (py - (ccy - 110)))   -- 燃焼室
                 , Color (hsb 0 0 35) (Pictures [ box (x - cw * 0.36 - 6) (ccy - 116) 6 190, box (x + cw * 0.36) (ccy - 116) 6 190
                                                , box (x - cw * 0.36 - 6) (ccy - 116) (cw * 0.72 + 12) 6 ])
                 , Color (hsb 40 15 85) (box (x - cw * 0.36) py (cw * 0.72) 30)                                                         -- ピストン
                 , seg 5 (hsb 0 0 70) (x, py + 30) (kx, ky)
                 , dot (hsb 8 70 100) kx ky 10
                 , txt (x - 12) (ccy + 132) 14 (hsb 0 0 100) (if wFault w && i == 0 then "MISS" else "#" ++ show (i + 1)) ]
               | i <- [0 .. n - 1]
               , let ang = crank + fromIntegral i * 4 * pi / fromIntegral n    -- 気筒ごとに 720°/n ずつずらす
                     cyc = wrapF (4 * pi) ang
                     py = ccy - 30 - 46 * cos ang
                     x = x0 + fromIntegral i * cw + cw / 2
                     firing = cyc < 0.9 && not (wFault w && i == 0)
                     kx = x + 22 * sin ang
                     ky = ccy + 110 - 22 * cos ang - 24 ]
             ++ [ txt x0 (ccy + 158) 14 (hsb 0 0 100) ("cylinders " ++ show n ++ " (Left/Right)") ]

-- 周波数 → 音名（A = 440 Hz）
noteName :: Float -> String
noteName f =
  let names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
      midi = round (69 + 12 * logBase 2 (max f 1 / 440)) :: Int
  in concat (take 1 (drop (midi `mod` 12) names)) ++ show (midi `div` 12 - 1)

event :: Env -> Event -> World -> IO World
event _ ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w
  EventKey (Char k) Down _ _
    | k == '1' -> w { wMachine = 1 }
    | k == '2' -> w { wMachine = 2 }
    | k `elem` "eE" -> w { wFault = not (wFault w) }
    | k `elem` "mM" -> w { wMuted = not (wMuted w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild (side (-1))
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild (side 1)
  EventKey _ Down _ _ -> w
  _ -> w0
  where
    w = w0 { wStarted = True }    -- クリックかキーで音が始まる
    side d | wMachine w == 1 = w { wZ = max 8 (min 48 (wZ w + d)) }
           | otherwise = w { wCylId = max 0 (min (length cylList - 1) (wCylId w + d)) }

-- ---------- やってみよう ----------
-- 1. 歯数 20、900 rpm のかみ合い周波数は 300 Hz。回転数を 2 倍にすると 1 オクターブ上がる。耳と数字で確かめよう
-- 2. 歯数を 1 枚ずつ増やすと音はどう変わる？ 電車のモーター音（ギア音）が加速で高くなる理由
-- 3. エンジンを 4 気筒 3000 rpm と 8 気筒 1500 rpm にして聞き比べよう。爆発の周波数は同じ。何がちがう？
-- 4. E で偏心・失火を入れ、スペクトルの山の両側（側帯波）を探そう。うねりの周期は何 Hz？
-- 5. step の Sawtooth を Square や Triangle に（signal の saw も同じ形に）変えると音色（倍音の並び）が変わる。スペクトルで見比べよう

-- ---------- 歯車の道具（インボリュート歯形の輪郭を作る） ----------
-- 歯数 z、モジュール m、圧力角 pa（ラジアン）の歯車の輪郭を点のリストで返す
--   歯 1 枚の中心が角度 0（+x 方向）を向く。ha・hf は歯先・歯元の高さ（モジュールの何倍か）
--   インボリュート：基礎円 rb に巻いた糸をほどくときの糸の先。ほどいた角 t の点は
--   半径 rb√(1 + t²)、角度 inv = t − atan t
makeGearOutline :: Int -> Float -> Float -> Float -> Float -> [Point]
makeGearOutline z m pa' ha hf = concatMap tooth [0 .. z - 1]
  where
    zf = fromIntegral z
    r = m * zf / 2
    rb = r * cos pa'
    ra = r + ha * m
    rf = max (r - hf * m) m
    half = pi / (2 * zf) + tan pa' - pa'                -- 基礎円の上での歯の半角
    t0 = if rf > rb then sqrt ((rf / rb) * (rf / rb) - 1) else 0
    t1 = sqrt (max ((ra / rb) * (ra / rb) - 1) (t0 * t0))
    flank = [(rf, half) | rf < rb]                      -- 基礎円より内側はまっすぐ
            ++ [ (rb * sqrt (1 + t * t), max (half - (t - atan t)) 0)
               | i <- [0 .. 8 :: Int], let t = t0 + (t1 - t0) * fromIntegral i / 8 ]
    tooth k =
      let c = fromIntegral k * 2 * pi / zf
      in (rf * cos (c - pi / zf), rf * sin (c - pi / zf))                      -- 歯底の真ん中
         : [ (rr * cos (c - a), rr * sin (c - a)) | (rr, a) <- flank ]
         ++ [ (rr * cos (c + a), rr * sin (c + a)) | (rr, a) <- reverse flank ]

-- 輪郭を (x, y) に角度 rot で描く。gloss の Polygon は凹んだ形を塗れないので、
-- 歯底の円（disc）と、歯 1 枚ずつの凸な多角形（インボリュートの両側の歯面）に分けて塗り、輪郭線を重ねる
gearPic :: Color -> Int -> [Point] -> Float -> Float -> Float -> Picture
gearPic c z pts x y rot =
  let tr (px, py) = (x + px * cos rot - py * sin rot, y + px * sin rot + py * cos rot)
      ps = map tr pts
      closed = ps ++ take 1 ps
      per = length pts `div` max 1 z                     -- 歯 1 枚ぶんの点の数（最初の 1 点は歯底の真ん中）
      teeth = [ take (per - 1) (drop (k * per + 1) ps) | k <- [0 .. z - 1] ]
      rf = case pts of
        ((px, py) : _) -> sqrt (px * px + py * py)
        [] -> 0
  in Pictures [ Color c (Translate x y (disc rf))
              , Color c (Pictures (map Polygon teeth))
              , Color (hsbA 0 0 100 70) (Line closed) ]

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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

-- 実数の列の高速フーリエ変換（長さは 2 の累乗）。前半 n/2 個のビンの大きさ |X_k| を返す
fftMag :: U.Vector Float -> U.Vector Float
fftMag x = U.create run
  where
    n = U.length x
    bits = length (takeWhile (< n) (iterate (* 2) 1))
    rev i = foldl (\acc b -> (acc `shiftL` 1) .|. ((i `shiftR` b) .&. 1)) 0 [0 .. bits - 1]
    run :: ST s (M.MVector s Float)
    run = do
      re <- U.thaw (U.generate n (\i -> x U.! rev i))
      im <- M.replicate n 0
      let stage len = forM_ [0, len .. n - 1] $ \s -> forM_ [0 .. len `div` 2 - 1] $ \k -> do
            let ang = -2 * pi * fromIntegral k / fromIntegral len
                wr = cos ang
                wi = sin ang
                i = s + k
                j = i + len `div` 2
            ar <- M.read re i
            ai <- M.read im i
            br <- M.read re j
            bi <- M.read im j
            let tr = wr * br - wi * bi
                ti = wr * bi + wi * br
            M.write re i (ar + tr)
            M.write im i (ai + ti)
            M.write re j (ar - tr)
            M.write im j (ai - ti)
      forM_ (takeWhile (<= n) (iterate (* 2) 2)) stage
      out <- M.new (n `div` 2)
      forM_ [0 .. n `div` 2 - 1] $ \k -> do
        a <- M.read re k
        b <- M.read im k
        M.write out k (sqrt (a * a + b * b))
      pure out
