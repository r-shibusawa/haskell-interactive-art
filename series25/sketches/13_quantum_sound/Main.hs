-- ============================================
-- 13 量子の音
-- 固有状態のエネルギー En を音の高さに、混ぜる割合 |cn|² を音量にする。重ね合わせ = 和音
--   |ψ|² が揺れる速さはエネルギーの差 Em − En。音では 2 つの音の差 = うなり。3（二重井戸）では本当に「同じ速さ」で揺れて聞こえる
-- クリックで音をスタート / 左の準位をクリック = 0 → 半分 → 全部 / 1 = 箱 / 2 = 調和振動子 / 3 = 二重井戸（トンネル） / ←→ = 3 の分裂 Δ（Hz） / ↑↓ = 基本の高さ / スペース = 消音
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

mM :: Int
mM = 300

slow :: Float
slow = 220           -- 箱・調和振動子の画面の時間は音の 1/220 の速さ（目で見える速さに）

loudness :: Float
loudness = 0.55      -- ★ 全部の音を合わせた音量

traceMax :: Int
traceMax = 400

type Field = U.Vector Float

data World = World
  { wMode  :: !Int
  , wC     :: !Field     -- 6 つの準位の係数（0, 0.5, 1）
  , wBase  :: !Float     -- ★ 箱の E1 の音の高さ（Hz）
  , wDelta :: !Float     -- ★ 二重井戸の分裂（Hz）= うなりの回数/秒
  , wMuted :: !Bool
  , wAudio :: !Bool      -- クリックしたら音をスタート
  , wNext  :: !Float     -- 次に音を鳴らす時刻
  , wClock :: !Double    -- 時刻（位相の桁が足りるよう Double）
  , wXs    :: !Field     -- x の格子
  , wWf    :: !Field     -- 固有関数：準位 n の点 i は n * mM + i
  , wTrace :: ![Float]   -- 左半分にいる確率（新しいものが先頭）
  , wRe    :: !Field
  , wIm    :: !Field
  }

main :: IO ()
main = runSketch (sketch "25-13 Quantum Sound" world0)
  { skBack  = hsb 290 30 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = makeWaves (World 1 chord0 55 2 False False 0 0 U.empty U.empty [] U.empty U.empty)

chord0 :: Field
chord0 = U.fromList [0, 1, 1, 0, 0, 0]

-- 各モードの「固有関数」と「音の高さ」
freq :: World -> Int -> Float
freq w n =
  let fn = fromIntegral n :: Float
  in case wMode w of
       1 -> wBase w * (fn + 1) * (fn + 1)              -- 箱：En ∝ n²
       2 -> wBase w * 2 * (2 * fn + 1)                 -- 調和：En = n + 1/2 → 奇数倍音
       _ | n == 0 -> 220 - wDelta w / 2                -- 二重井戸：近い 2 つの準位
         | n == 1 -> 220 + wDelta w / 2
         | otherwise -> 0

nLevels :: World -> Int
nLevels w = if wMode w == 3 then 2 else 6

-- エルミート関数（06 と同じ漸化式）を 1 点ぶん
hermite6 :: Float -> [Float]
hermite6 x =
  let p0 = pi ** (-0.25) * exp (-x * x / 2)
      p1 = sqrt 2 * x * p0
      go :: Int -> Float -> Float -> [Float]
      go n a b
        | n >= 5 = []
        | otherwise =
            let fn = fromIntegral n
                c = sqrt (2 / (fn + 1)) * x * b - sqrt (fn / (fn + 1)) * a
            in c : go (n + 1) b c
  in p0 : p1 : go 1 p0 p1

makeWaves :: World -> World
makeWaves w =
  let mode = wMode w
      m1 = fromIntegral (mM - 1) :: Float
      xs = U.generate mM (\i -> let f = fromIntegral i in if mode == 1 then f / m1 else -5 + 10 * f / m1)
      dxx = xs U.! 1 - xs U.! 0
      raw n x = case mode of
        1 -> sqrt 2 * sin (fromIntegral (n + 1) * pi * x)
        2 -> case drop n (hermite6 x) of
               (v : _) -> v
               [] -> 0
        _ -> let g1 = exp (-sq (x + 2) / 0.7)
                 g2 = exp (-sq (x - 2) / 0.7)
             in if n == 0 then g1 + g2 else if n == 1 then g1 - g2 else 0
      -- どのモードでも ∫|ψn|² dx = 1 にそろえる
      level n =
        let v = U.map (raw n) xs
            s = U.sum (U.map (\a -> a * a * dxx) v)
        in if s > 0 then U.map (/ sqrt s) v else v
      wf = U.concat (map level [0 .. 5 :: Int])
      c = if mode == 3 then U.fromList [1, 1, 0, 0, 0, 0] else wC w
  in w { wXs = xs, wWf = wf, wC = c, wTrace = [] }

normOf :: World -> Float
normOf w = sqrt (max 1e-9 (sum [ sq (wC w U.! n) | n <- [0 .. nLevels w - 1] ]))

stepW :: Env -> Float -> World -> IO World
stepW env dt w0 = do
  let clock = wClock w0 + realToFrac dt
      w = w0 { wClock = clock }
      nl = nLevels w
      norm = normOf w
      mode3 = wMode w == 3
      -- ★ 音：準位 n → 周波数 freq(n)、音量 ∝ |cn|²。連続音の代わりに短い音をくり返す
      (dur, every) = if mode3 then (1.25, 1.3) else (0.65, 0.7)     -- 3 はうなりが聞こえるよう長めに
      time = envTime env
      due = wAudio w && not (wMuted w) && time >= wNext w
  when due $ forM_ [0 .. nl - 1] $ \n -> do
    let v = loudness * sq (wC w U.! n / norm)
    when (v > 0) (playTone (clampF 20 5000 (freq w n)) v dur Sine)
  -- ψ(x,t) = Σ cn ψn(x) e^{−i ωn t}。二重井戸は本当の音と同じ速さ、ほかは 1/slow
  let active = [ (n * mM, a * cos th, a * sin th)
               | n <- [0 .. nl - 1]
               , wC w U.! n /= 0
               , let a = wC w U.! n / norm
               , let om = 2 * pi * realToFrac (freq w n) / (if mode3 then 1 else realToFrac slow) :: Double
               , let th = realToFrac (wrapD (negate om * clock)) :: Float ]
      sumAt f i = foldl' (\acc tr@(o, _, _) -> acc + f tr * wWf w `U.unsafeIndex` (o + i)) 0 active
      re = U.generate mM (sumAt (\(_, a, _) -> a))
      im = U.generate mM (sumAt (\(_, _, b) -> b))
      dxx = wXs w U.! 1 - wXs w U.! 0
      left = U.sum (U.take (mM `div` 2) (U.zipWith (\a b -> (a * a + b * b) * dxx) re im))
  pure w { wRe = re, wIm = im
         , wTrace = take traceMax (left : wTrace w)
         , wNext = if due then time + every else wNext w }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      mode = wMode w
      nl = nLevels w
      norm = normOf w
      -- 左：準位のリスト
      lx = 30
      ly = 130
      rh = min 70 ((height - 200) / 6)
      levelPic n =
        let y = ly + fromIntegral (nl - 1 - n) * rh
            cn = wC w U.! n
            a2 = sq (cn / norm)
            hue = fromIntegral n * 50 + 180
            name | mode == 2 = "n=" ++ show n
                 | mode == 3 = if n == 0 then "sym" else "anti"
                 | otherwise = "n=" ++ show (n + 1)
        in [ box env (hsb 0 0 (if cn /= 0 then 22 else 14)) lx y 300 (rh - 8)
           , box env (hsb hue 60 100) lx y (300 * a2) 6
           , curve env 2 (hsba hue 60 100 (if cn /= 0 then 1 else 0.35))
               [ (lx + 150 + fromIntegral i / fromIntegral mM * 140, y + rh / 2 - 4 - wWf w U.! (n * mM + i) * (if mode == 1 then 10 else 14))
               | i <- [0, 6 .. mM - 1] ]
           , txt env (lx + 10) (y + rh / 2 - 2) 14 white (name ++ " " ++ nf (if mode == 3 then 1 else 0) (freq w n) ++ " Hz")
           , txt env (lx + 10) (y + rh / 2 + 18) 14 (gray 0.6) ("|c|^2 = " ++ nf 2 a2) ]
      -- 中央：|ψ|²（色 = 位相）
      px = 380
      pw = width - px - 40
      pb = height * 0.6
      ph = height * 0.38
      probs = U.zipWith (\a b -> a * a + b * b) (wRe w) (wIm w)
      pmax = U.foldl' max 0 probs
      fM = fromIntegral mM :: Float
      bars = [ seg env (pw / fM + 0.8) (hsb ((atan2 (wIm w U.! i) (wRe w U.! i) + pi) / tau * 360) 70 100) (x, pb) (x, pb - p / max pmax 0.5 * ph)
             | i <- [0 .. U.length probs - 1]
             , let p = probs U.! i
             , let x = px + fromIntegral i / fM * pw ]
      -- 左半分にいる確率の時間変化
      gy = height - 40
      gh = height * 0.22
      traceAsc = reverse (wTrace w)
      soundText | not (wAudio w) = "Click to start sound"
                | wMuted w = "Muted"
                | otherwise = "Sound on"
      modeName = case mode of { 1 -> "box"; 2 -> "harmonic"; _ -> "double well" } :: String
  in Pictures $
       concatMap levelPic [0 .. nl - 1]
       ++ bars
       ++ [ seg env 1 (hsb 0 0 50) (px, pb) (px + pw, pb)
          , frameBox env (hsb 0 0 35) px (gy - gh) pw gh
          , curve env 2 (hsb 55 90 100) [ (px + fromIntegral i / fromIntegral traceMax * pw, gy - v * gh) | (i, v) <- zip [0 :: Int ..] traceAsc ]
          , txt env px (gy - gh - 8) 14 (gray 0.7) ("Chance of being in the left half (" ++ (if mode == 3 then "real time: same speed as the beat" else "screen time = sound / " ++ nf 0 slow) ++ ")")
          , txt env 20 26 16 white (soundText ++ "   1: box  2: harmonic  3: double well (" ++ modeName ++ ")   Click a level")
          , txt env 20 48 16 white ("Left/Right: delta = " ++ nf 1 (wDelta w) ++ " Hz   Up/Down: pitch " ++ nf 0 (wBase w) ++ " Hz   Space: mute")
          , txt env 20 70 14 (gray 0.6) (case mode of
              3 -> "Mixing the two levels makes it tunnel between wells. Trips per second = the beat of the two tones."
              2 -> "Evenly spaced levels give odd harmonics (like a clarinet). The sloshing is perfectly periodic."
              _ -> "Box: En ~ n^2 gives 1, 4, 9, 16 times the pitch. Dissonant, but the pattern returns every 1/E1.")
          ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> onClick env w0 { wAudio = True }
  EventKey (Char c) Down _ _ -> onChar c w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wDelta = min 8 (wDelta w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wDelta = max 0.5 (wDelta w - 0.5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wBase = min 110 (wBase w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wBase = max 30 (wBase w - 5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wMuted = not (wMuted w) }
  _ -> w0
  where w = w0 { wAudio = True }

onChar :: Char -> World -> World
onChar c w
  | c `elem` "123" =
      let mode = fromEnum c - fromEnum '0'
      in makeWaves w { wMode = mode, wC = if mode /= 3 then chord0 else wC w }
  | otherwise = w

onClick :: Env -> World -> World
onClick env w =
  let (mx, my) = mouseP env
      lx = 30
      ly = 130
      rh = min 70 ((envH env - 200) / 6)
      nl = nLevels w
      hit n = let y = ly + fromIntegral (nl - 1 - n) * rh
              in mx > lx && mx < lx + 300 && my > y && my < y + rh - 8
      next v | v == 0 = 0.5
             | v == 0.5 = 1
             | otherwise = 0
  in w { wC = U.imap (\n v -> if n < nl && hit n then next v else v) (wC w) }

-- ---------- やってみよう ----------
-- 1. 3（二重井戸）で Δ = 1 Hz にして、うなりの「うわん」と黄色いグラフの山が同じタイミングか聞き比べよう
-- 2. 対称だけ（反対称を 0）にすると、粒子は両方の井戸に半分ずつのまま動かない。音もうなりが消える
-- 3. 2 で n = 0 と 1 だけ・n = 0 と 2 だけ を比べると、揺れの速さは？（差が 1 と 2）
-- 4. freq を自分の音階（ドレミ）に変えて、「準位 = 鍵盤」の量子ピアノにしよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- 位相を −π〜π の小さな値に折り返す（Float に直しても桁が残るように）
wrapD :: Double -> Double
wrapD x = x - 2 * pi * fromIntegral (round (x / (2 * pi)) :: Int)

sq :: Float -> Float
sq x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)
