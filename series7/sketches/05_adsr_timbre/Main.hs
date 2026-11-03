-- ============================================
-- 05 音色をつくる（倍音とエンベロープ）
-- 同じ「ド」でもピアノとフルートが違って聞こえるのは、倍音の混ざり方（スペクトル）と
-- 音の立ち上がり・消え方（エンベロープ ADSR）が違うから
-- 上の棒をドラッグ = 倍音の大きさ / 下のグラフをドラッグ = A D S R / スペースで鳴らす / 1〜3 でお手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.List (sortOn)
import Text.Printf (printf)

data Adsr = Adsr { aA :: !Float, aD :: !Float, aS :: !Float, aR :: !Float }  -- 秒・秒・割合・秒

data World = World
  { wHarm  :: ![Float]   -- 8 個の倍音の大きさ
  , wAdsr  :: !Adsr
  , wPlay  :: !Float     -- 鳴らし始めた時刻（秒）
  , wGrain :: !Float     -- 次の短い音を鳴らす時刻（秒）
  }

baseHz, grainEvery, grainLen :: Float
baseHz = 220               -- ★ 基音の周波数
grainEvery = 0.12          -- ★ 短い音を出す間隔（秒）
grainLen = 0.16            -- 短い音 1 つの長さ（秒）

maxVoices :: Int
maxVoices = 4              -- 1 度に鳴らす倍音の数（Art.Sound は同時に 8 音まで）

main :: IO ()
main = runSketch (sketch "07-05 ADSR and Timbre" (World [1, 0.5, 0.33, 0.25, 0.2, 0.17, 0.14, 0.12] (Adsr 0.02 0.2 0.6 0.4) (-10) 0))
  { skBack  = hsb 260 40 10
  , skStep  = \env _ w -> stepVoice (envTime env) w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

totalTime :: Adsr -> Float
totalTime a = aA a + aD a + 1.0 + aR a     -- サスティンを 1 秒と仮定する

-- 時刻 t のエンベロープの高さ（0〜1）
envAt :: Adsr -> Float -> Float
envAt a t
  | t < aA a = t / aA a
  | t < aA a + aD a = 1 - (1 - aS a) * ((t - aA a) / aD a)
  | t < aA a + aD a + 1.0 = aS a
  | otherwise = max 0 (aS a * (1 - (t - aA a - aD a - 1.0) / aR a))

-- ★ 連続音は作れないので、短い音をくり返し、音量を ADSR にしたがって変える
stepVoice :: Float -> World -> IO World
stepVoice now w
  | since < totalTime a && wGrain w <= now = do
      let e = envAt a since
          strongest = take maxVoices (sortOn (negate . snd) (zip [1 ..] (wHarm w)))
      forM_ strongest $ \(i, h) ->
        if h < 0.01 then pure () else
          playTone (baseHz * i) (0.3 * e * h) grainLen Sine   -- ★ 倍音の足し算
      pure w { wGrain = now + grainEvery }
  | otherwise = pure w
  where
    a = wAdsr w
    since = now - wPlay w

-- 倍音の棒の位置（p5.js の座標）
hx, barW, gap :: Float
hx = 80
barW = 50
gap = 20

hy, maxH :: Env -> Float
hy env = envH env * 0.42
maxH env = envH env * 0.25

-- ADSR のグラフの位置
ex, ey, ew, eh :: Env -> Float
ex _ = 80
ey env = envH env * 0.88
ew env = envW env - 160
eh env = envH env * 0.25

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (bars ++ wave ++ envelope ++ hud)
  where
    harm = wHarm w
    a = wAdsr w
    top = hy env - maxH env
    -- ---- 倍音の棒 ----
    bars =
      txt env hx (top - 20) 16 white "Harmonics (drag the bars)"
      : concat
        [ [ Color (hsb 260 30 20) (box env x top barW (maxH env))
          , Color (hsb (remap fi 0 8 170 330) 70 100) (box env x (hy env - h * maxH env) barW (h * maxH env))
          , txtC env (x + barW / 2) (hy env + 20) 14 (hsb 0 0 70) (show (i + 1) ++ "x")
          ]
        | (i, h) <- zip [0 :: Int ..] harm
        , let fi = fromIntegral i
              x = hx + fi * (barW + gap) ]
    -- ---- 波形（1 周期）----
    wx = hx + 8 * (barW + gap) + 60
    ww = envW env - wx - 60
    wmid = hy env - maxH env / 2
    wave =
      [ Color (hsb 0 0 30) (Line [toG env (wx, wmid), toG env (wx + ww, wmid)])
      , Color (hsb 45 90 100) (thickPath 2.5
          [ toG env (wx + x, wmid - v * maxH env * 0.2)
          | x <- [0, 2 .. ww]
          , let t = x / ww * tau
                v = sum (zipWith (\i h -> h * sin (i * t)) [1 ..] harm) ])  -- ★ 倍音の足し算
      , txt env wx (top - 20) 14 (hsb 0 0 60) "Waveform (1 period)"
      ]
    -- ---- ADSR ----
    total = totalTime a
    xOf sec = ex env + sec / total * ew env
    y0 = ey env
    envelope =
      [ Color (hsb 0 0 30) (Line [toG env (ex env, y0), toG env (ex env + ew env, y0)])
      , Color (hsb 170 80 100) (thickPath 3 (map (toG env)
          [ (xOf 0, y0)
          , (xOf (aA a), y0 - eh env)                                 -- Attack：0 → 最大
          , (xOf (aA a + aD a), y0 - aS a * eh env)                   -- Decay：最大 → サスティンの高さ
          , (xOf (aA a + aD a + 1.0), y0 - aS a * eh env)             -- Sustain：押している間
          , (xOf total, y0)                                           -- Release：離してから 0 へ
          ]))
      , if since < total
          then Color (hsb 45 90 100) (at (toG env (xOf since, y0 - envAt a since * eh env)) (disc 8))
          else Blank
      -- 文字が重ならないように 2 段に分ける
      , txtC env (xOf (aA a / 2)) (y0 + 22) 14 (hsb 0 0 60) ("A attack " ++ f2 (aA a) ++ "s")
      , txtC env (xOf (aA a + aD a / 2)) (y0 + 44) 14 (hsb 0 0 60) ("D decay " ++ f2 (aD a) ++ "s")
      , txtC env (xOf (aA a + aD a + 0.5)) (y0 + 22) 14 (hsb 0 0 60) ("S sustain " ++ f2 (aS a))
      , txtC env (xOf (total - aR a / 2)) (y0 + 22) 14 (hsb 0 0 60) ("R release " ++ f2 (aR a) ++ "s")
      , txt env (ex env) (y0 - eh env - 16) 14 (hsb 0 0 60) "Envelope (loudness over time). Drag here to change A / D / S / R"
      ]
    since = envTime env - wPlay w
    hud =
      [ txt env 20 40 18 white "Space: play     1: piano-like     2: flute-like     3: organ-like"
      , txt env hx (top - 44) 15 (hsb 0 0 70) "1x = fundamental, 2x = one octave up, 3x = a fifth above that..."
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion pos | envDown env -> pure (dragTo env (fromG env pos) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlay = envTime env, wGrain = envTime env }
  EventKey (Char '1') Down _ _ -> pure w { wHarm = [1, 0.6, 0.3, 0.2, 0.1, 0.08, 0.05, 0.03], wAdsr = Adsr 0.005 0.6 0.15 0.5 }
  EventKey (Char '2') Down _ _ -> pure w { wHarm = [1, 0.1, 0.05, 0.02, 0, 0, 0, 0], wAdsr = Adsr 0.15 0.1 0.8 0.3 }
  EventKey (Char '3') Down _ _ -> pure w { wHarm = [1, 0.8, 0.6, 0.5, 0.4, 0.3, 0.3, 0.2], wAdsr = Adsr 0.01 0.01 1.0 0.05 }
  _ -> pure w

-- ドラッグで倍音と ADSR を変える
dragTo :: Env -> Point -> World -> World
dragTo env (mx, my) = adsrPart . harmPart
  where
    i = floor ((mx - hx) / (barW + gap)) :: Int
    harmPart w
      | i >= 0 && i < 8 && my > hy env - maxH env - 10 && my < hy env + 10 =
          let v = clampF 0 1 (remap my (hy env) (hy env - maxH env) 0 1)
          in w { wHarm = [if j == i then v else h | (j, h) <- zip [0 ..] (wHarm w)] }
      | otherwise = w
    -- ADSR：横の位置で 4 つの領域
    adsrPart w
      | my > ey env - eh env - 20 && my < ey env + 10 =
          let q = (mx - ex env) / ew env
              v = clampF 0 1 (remap my (ey env) (ey env - eh env) 0 1)
              a = wAdsr w
              a' | q < 0.25 = a { aA = 0.005 + v * 1.0 }
                 | q < 0.5 = a { aD = 0.01 + v * 1.0 }
                 | q < 0.75 = a { aS = v }
                 | otherwise = a { aR = 0.01 + v * 2.0 }
          in w { wAdsr = a' }
      | otherwise = w

-- ---------- やってみよう ----------
-- 1. ピアノ風（1）とオルガン風（3）：倍音は似ているのに ADSR で全然ちがう楽器に
-- 2. 偶数倍音（2, 4, 6, 8）を 0 にするとクラリネットっぽい音（筒が片側だけ閉じているため）
-- 3. A を 1 秒にすると「ふわっ」と立ち上がる。R を 2 秒にすると残響のように

-- ---------- 道具 ----------

f2 :: Float -> String
f2 = printf "%.2f"

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（中央揃え。y は文字の下端）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx (gy + s / 2) s c str
