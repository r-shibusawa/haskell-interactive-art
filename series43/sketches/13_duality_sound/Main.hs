-- ============================================
-- 43-13 時間と周波数を聴く
-- 短い「カチッ」という音 ↔ 幅の広いスペクトル、長く伸びる純音 ↔ 鋭いピーク（11 の不確定性を耳で）
--   クリックで音を鳴らし、鳴らした音と同じ信号をプログラムの中で作って、窓 + DFT で波形とスペクトルを描く
--   音の長さはマウス左右（5 ms 〜 1 s）、高さはマウス上下
-- 操作：クリック = 鳴らす / 1〜3 = 波形（サイン・矩形・ノイズ） / A = 自動でくり返す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

holdDecay :: Float
holdDecay = 0.985         -- ★ ピークホールドの減り方（1 に近いほど長く残る）

rate :: Float
rate = 22050              -- 音の標本化周波数（Art.Sound と同じ）

fftN :: Int
fftN = 1024               -- ★ 解析する標本の数（約 46 ms 分）

binHz :: Float
binHz = rate / fromIntegral fftN

nb :: Int
nb = floor (4000 / binHz) -- 4 kHz までのビンの数

-- 鳴っている音：始めた時刻、長さ、周波数の組、1 音の音量、波形
data Tone = Tone { tStart :: !Float, tDur :: !Float, tFreqs :: ![Float], tVol :: !Float, tWave :: !Wave }

data World = World
  { wNo      :: !Int
  , wAuto    :: !Bool
  , wStarted :: !Bool              -- 1 度でも音を出したか
  , wLastDur :: !Float
  , wLastT   :: !Float
  , wTones   :: ![Tone]
  , wWave    :: !(U.Vector Float)  -- 最新の波形
  , wMag     :: !(U.Vector Float)  -- なめらかにした振幅
  , wSpec    :: !(U.Vector Float)  -- 0〜255 のスペクトル
  , wHold    :: !(U.Vector Float)  -- 短い音も見えるようにピークを残す
  , wHist    :: ![(Float, Float, Float)] -- 直近の（長さ、帯域幅、鳴らした時刻）
  }

world0 :: World
world0 = World 1 False False 0 (-10) []
  (U.replicate fftN 0) (U.replicate nb 0) (U.replicate nb 0) (U.replicate nb 0) []

main :: IO ()
main = runSketch (sketch "43-13 Hear time and frequency" world0)
  { skBack  = hsb 250 40 10
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

durFromMouse, freqFromMouse :: Env -> Float
durFromMouse env =
  let (mx, _) = mouseP env
      w = envW env
  in 10 ** remap (clampF 40 (w - 40) mx) 40 (w - 40) (logBase 10 0.005) 0
freqFromMouse env =
  let (_, my) = mouseP env
      h = envH env
  in remap (clampF 100 (h - 60) my) 100 (h - 60) 1200 200

-- ★ 長さ d 秒・周波数 f の音を 1 回鳴らす。ノイズは音をたくさん重ねて代わりにする
playNow :: Env -> World -> IO World
playNow env w = do
  let d0 = durFromMouse env
      -- Art.Sound は 10 ms 刻みの名前で音を保存するので、長さもそれに合わせる
      d = if d0 < 0.0075 then 0.005 else fromIntegral (round (d0 * 100) :: Int) / 100
      f = fromIntegral (round (freqFromMouse env) :: Int)
      now = envTime env
  tone <- case wNo w of
    1 -> pure (Tone now d [f] 0.5 Sine)
    2 -> pure (Tone now d [f] 0.5 Square)
    _ -> do
      ks <- replicateM 8 (randomRIO (0, 60 :: Int))
      pure (Tone now d [150 * 2 ** (fromIntegral k / 12) | k <- ks] 0.18 Sawtooth)
  playChord (tFreqs tone) (tVol tone) (tDur tone) (tWave tone)
  pure w { wStarted = True, wLastDur = d, wLastT = now, wTones = tone : wTones w }

-- Art.Sound の音と同じ式で、時刻 t（鳴り始めから）の値を作る
toneAt :: Tone -> Float -> Float
toneAt tn t
  | t < 0 || t >= tDur tn = 0
  | otherwise =
      let env0 = min 1 (t / 0.008) * max 0 (1 - t / tDur tn) ** 1.2
          osc f = let p = fract (f * t) in case tWave tn of
            Sine -> sin (tau * p)
            Square -> if p < 0.5 then 1 else -1
            Sawtooth -> 2 * p - 1
            Triangle -> 4 * abs (p - 0.5) - 1
      in tVol tn * 0.8 * env0 * foldl' (\s f -> s + osc f) 0 (tFreqs tn)

-- ★ 窓（Blackman）をかけてビンごとに DFT。ブラウザの AnalyserNode と同じ手順
blackman, cosT, sinT :: U.Vector Float
blackman = U.generate fftN (\i -> let a = tau * fromIntegral i / fromIntegral fftN in 0.42 - 0.5 * cos a + 0.08 * cos (2 * a))
cosT = U.generate fftN (\k -> cos (tau * fromIntegral k / fromIntegral fftN))
sinT = U.generate fftN (\k -> sin (tau * fromIntegral k / fromIntegral fftN))

spectrum :: U.Vector Float -> U.Vector Float
spectrum x =
  let wx = U.zipWith (*) x blackman
      bin k =
        let go !n !re !im
              | n >= fftN = sqrt (re * re + im * im) / fromIntegral fftN
              | otherwise =
                  let i = (k * n) `mod` fftN
                      v = U.unsafeIndex wx n
                  in go (n + 1) (re + v * U.unsafeIndex cosT i) (im - v * U.unsafeIndex sinT i)
        in go 0 0 0
  in U.generate nb bin

-- 振幅を 0〜255 に（−100 dB〜−30 dB を割り当てる）
toByte :: Float -> Float
toByte m =
  let db = 20 * logBase 10 (max 1e-9 m)
  in clampF 0 255 (255 / 70 * (db + 100))

-- ピークのビンと、ピークの半分より大きいビンの範囲
band :: U.Vector Float -> Maybe (Int, Int, Int)
band hold =
  let mi = U.maxIndex hold
      mx = hold U.! mi
      goLo i = if i > 0 && hold U.! (i - 1) > mx / 2 then goLo (i - 1) else i
      goHi i = if i < nb - 1 && hold U.! (i + 1) > mx / 2 then goHi (i + 1) else i
  in if mx > 40 then Just (mi, goLo mi, goHi mi) else Nothing

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let dur = durFromMouse env
      now = envTime env
      due = wAuto w0 && wStarted w0 && (now - wLastT w0) * 1000 > max 700 (dur * 1000 + 400)
  w <- if due then playNow env w0 else pure w0
  let tones = [t | t <- wTones w, now - tStart t < tDur t + 0.1]
      wave = if null tones
               then U.replicate fftN 0
               else U.generate fftN (\i ->
                      let back = fromIntegral (fftN - 1 - i) / rate
                      in foldl' (\s t -> s + toneAt t (now - tStart t - back)) 0 tones)
      mag1 = if null tones then U.replicate nb 0 else spectrum wave
      mag = U.zipWith (\o n -> 0.5 * o + 0.5 * n) (wMag w) mag1    -- なめらかにする
      spec = U.map toByte mag
      hold = U.zipWith (\h s -> max (h * holdDecay) s) (wHold w) spec   -- ★ ピークホールド
      -- 記録（長さ vs 帯域幅）
      since = (now - wLastT w) * 1000
      newRec = case band hold of
        Just (_, lo, hi) | since > 100 && since < 400 && not (any (\(_, _, t) -> t == wLastT w) (wHist w)) ->
          [(wLastDur w, fromIntegral (hi - lo + 1) * binHz, wLastT w)]
        _ -> []
  pure w { wTones = tones, wWave = wave, wMag = mag, wSpec = spec, wHold = hold
         , wHist = take 5 (newRec ++ wHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ box (hsb 0 0 35) lx ly lw lh, box (hsb 0 0 35) lx ry lw rh
  , txt (lx + 10) (ly + 22) 14 (hsb 0 0 70) "waveform (time)"
  , txt (lx + 10) (ry + 22) 14 (hsb 0 0 70) "spectrum (frequency, 0 to 4 kHz)   aqua = now   yellow line = peak hold" ]
  ++ (if wStarted w then analysis else [ txt (lx + lw / 2 - 80) (ly + lh / 2) 20 (hsb 0 0 60) "Click to play a sound" ])
  ++ [ txt (lx + 10) (ry + rh - 10) 14 (hsb 0 0 70)
         ("log (length -> width): " ++ intercalate "   " [fmt 0 (d * 1000) ++ "ms->" ++ fmt 0 b ++ "Hz" | (d, b, _) <- reverse (wHist w)])
     | not (null (wHist w)) ]
  ++ [ txt 20 32 18 (hsb 0 0 100) ("length " ++ fmt 0 (dur * 1000) ++ " ms (mouse X)   pitch " ++ fmt 0 fq ++ " Hz (Y)   wave: " ++ wname ++ " (1-3)   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   click: play")
     , txt 20 58 14 (hsb 0 0 60) "Far left (5 ms) is a click and the spectrum spreads wide. Far right (1 s) is a beep and the spectrum is a thin needle."
     , txt 20 78 14 (hsb 0 0 60) "Time width x frequency width is about constant (uncertainty)." ]
  where
    dur = durFromMouse env
    fq = freqFromMouse env
    wname = case wNo w of
      1 -> "sine"
      2 -> "square"
      _ -> "noise"
    lx = 40
    ly = 110
    lw = envW env - 80
    lh = (envH env - 200) * 0.35
    ry = ly + lh + 50
    rh = (envH env - 200) * 0.5
    fnb = fromIntegral nb :: Float
    analysis =
      -- 波形
      [ path 2 (hsb 10 80 100)
          [ (lx + fromIntegral i / fromIntegral fftN * lw, ly + lh / 2 - v * lh * 0.45)
          | (i, v) <- U.toList (U.indexed (wWave w)), even i ] ]
      -- スペクトル（4 kHz まで）
      ++ [ solid (hsbA 180 80 100 80) (lx + fromIntegral i / fnb * lw) (ry + rh - s / 255 * (rh - 30)) (lw / fnb + 0.5) (s / 255 * (rh - 30))
         | (i, s) <- U.toList (U.indexed (wSpec w)), s > 0 ]
      ++ [ path 1.5 (hsbA 55 70 100 70)
             [ (lx + (fromIntegral i + 0.5) / fnb * lw, ry + rh - h / 255 * (rh - 30))
             | (i, h) <- U.toList (U.indexed (wHold w)) ] ]
      -- ★ 帯域幅：ピークの半分より大きいビンの数 × 刻み
      ++ (case band (wHold w) of
            Just (mi, lo, hi) ->
              let x1 = lx + fromIntegral lo / fnb * lw
                  x2 = lx + fromIntegral (hi + 1) / fnb * lw
                  s = "width ~ " ++ fmt 0 (fromIntegral (hi - lo + 1) * binHz) ++ " Hz (peak " ++ fmt 0 (fromIntegral mi * binHz) ++ " Hz)"
              in [ seg 2 (hsb 55 90 100) (x1, ry + 40) (x2, ry + 40)
                 , txt (min (x2 + 8) (lx + lw - 10 * fromIntegral (length s))) (ry + 58) 14 (hsb 55 90 100) s ]
            Nothing -> [])
      ++ concat [ [ seg 1 (hsb 0 0 30) (sx, ry + rh - 6) (sx, ry + rh)
                  , txt (sx - 12) (ry + rh + 17) 14 (hsb 0 0 50) (show f) ]
                | f <- [0, 500 .. 4000 :: Int], let sx = lx + fromIntegral f / 4000 * lw ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> playNow env w
  EventKey (Char k) Down _ _
    | k `elem` "123" -> pure w { wNo = fromEnum k - fromEnum '0' }
    | k `elem` "aA" -> pure w { wAuto = not (wAuto w), wStarted = True }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ高さで長さだけ変えて、スペクトルの幅を記録しよう。長さ × 幅 はだいたい一定？
-- 2. 矩形波（2）は倍音がたくさん出る。長くしても「針が何本も」立つ。ノイズ（3）は？
-- 3. 長さを 20 ms にしてマウス上下で高さを変える。高さがわかりにくいのはなぜ？（幅が広いから）
-- 4. 打楽器は短い音、笛は長い音。その楽器の「音色」の違いをスペクトルの幅で説明してみよう

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

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上が (x, y) の塗りつぶした長方形
solid :: Color -> Float -> Float -> Float -> Float -> Picture
solid c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""
