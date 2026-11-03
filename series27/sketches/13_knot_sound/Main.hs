-- ============================================
-- 13 結び目の音
-- トーラス結び目 (p, q) は「まわりを p 回」と「穴を q 回」の 2 つの回転の組み合わせ。2 つの回転を 2 つの音にする
--   音の高さの比 p : q（2:3 = 完全 5 度、3:4 = 4 度、4:5 = 長 3 度）。結び目を横から見た影は、2 つの音のリサジュー図形とそっくり
-- クリックで音をスタート / ←→ = p / ↑↓ = q / M = 和音 ↔ メロディ / 1〜3 = 音色 / スペース = 止める / B = 基本の高さ
-- ============================================
module Main (main) where

import Art
import Data.List (sortOn)
import Text.Printf (printf)

speed :: Float
speed = 0.01         -- ★ なぞる速さ

scaleSteps :: [Int]
scaleSteps = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21]   -- ★ メロディの音階（ペンタトニック、半音の数）

noteGap :: Float
noteGap = 0.25       -- 音を鳴らす間隔（秒）

width, height :: Float
width = 1280
height = 800

data World = World
  { wP, wQ :: !Int          -- ★ 結び目の (p, q)
  , wT :: !Float
  , wPaused, wMelody :: !Bool
  , wBase :: !Float         -- ★ 基本の高さ（Hz）
  , wWave :: !Wave
  , wWaveName :: !String
  , wRot :: !Float
  , wAudio :: !Bool
  , wTimer :: !Float
  }

main :: IO ()
main = runSketch (sketch "27-13 Sound of Knots" (World 2 3 0 False False 110 Triangle "triangle" 0 False 0))
  { skBack  = hsb 320 25 10
  , skStep  = \_ dt w -> stepWorld dt w
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | トーラス結び目の点
knotPt :: Int -> Int -> Float -> (Float, Float, Float)
knotPt p q s =
  let fp = fromIntegral p
      fq = fromIntegral q
      r = 2 + cos (fq * s)
  in (r * cos (fp * s), r * sin (fp * s), sin (fq * s))

---------------------------------------------------------------- 動き

stepWorld :: Float -> World -> IO World
stepWorld dt w0 = do
  let w1 = if wPaused w0 then w0 else w0 { wT = wT w0 + speed, wRot = wRot w0 + 0.004 }
      timer = wTimer w1 - dt
  -- ★ 音：和音モードは p : q の 2 音、なぞる点の高さ z で 2 音目の大きさ
  if wAudio w1 && not (wPaused w1) && timer <= 0
    then do
      playVoices w1
      pure w1 { wTimer = noteGap }
    else pure w1 { wTimer = max 0 timer }

playVoices :: World -> IO ()
playVoices w = do
  let (_, _, z) = knotPt (wP w) (wQ w) (wT w)
      dur = noteGap * 1.3
      voice f v = playTone (clampF 20 4000 f) (clampF 0 0.3 v) dur (wWave w)
  if not (wMelody w)
    then do
      voice (wBase w * fromIntegral (wP w)) 0.16
      voice (wBase w * fromIntegral (wQ w)) (0.08 + 0.08 * (z + 1) / 2)
    else do
      let n = length scaleSteps
          k = max 0 (min (n - 1) (floor ((z + 1) / 2 * fromIntegral n)))
          st = case drop k scaleSteps of
            (s : _) -> s
            [] -> 0
      voice (wBase w * 2 * 2 ** (fromIntegral st / 12)) 0.18
      voice (wBase w) 0.06

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> on w
  EventKey (SpecialKey k) Down _ _ -> on (case k of
    KeyRight -> w { wP = min 9 (wP w + 1) }
    KeyLeft -> w { wP = max 1 (wP w - 1) }
    KeyUp -> w { wQ = min 9 (wQ w + 1) }
    KeyDown -> w { wQ = max 1 (wQ w - 1) }
    KeySpace -> w { wPaused = not (wPaused w) }
    _ -> w)
  EventKey (Char c) Down _ _ -> on (onChar c w)
  _ -> w
  where on x = x { wAudio = True }

onChar :: Char -> World -> World
onChar c w
  | c `elem` "mM" = w { wMelody = not (wMelody w) }
  | c `elem` "bB" = w { wBase = if wBase w == 110 then 165 else if wBase w == 165 then 82.5 else 110 }
  | c == '1' = w { wWave = Sine, wWaveName = "sine" }
  | c == '2' = w { wWave = Triangle, wWaveName = "triangle" }
  | c == '3' = w { wWave = Sawtooth, wWaveName = "sawtooth" }
  | otherwise = w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let p = wP w
      q = wQ w
      t = wT w
      rot = wRot w
      cur@(curX, _, curZ) = knotPt p q t
      -- 左：回る結び目
      cx = width * 0.3
      cy = height * 0.55
      sS = min (width * 0.09) (height * 0.13)
      proj (vx, vy, vz) =
        let x = vx * cos rot - vy * sin rot
            y = vx * sin rot + vy * cos rot
            yy = y * cos 1.0 - vz * sin 1.0
            zz = y * sin 1.0 + vz * cos 1.0
        in ((cx + x * sS, cy - zz * sS), yy)
      n = 600 :: Int
      fn = fromIntegral n :: Float
      pts = [proj (knotPt p q (fromIntegral i / fn * tau)) | i <- [0 .. n]]
      segs = zipWith3 (\i (a, da) (b, db) -> ((da + db) / 2, i, a, b)) [0 :: Int ..] pts (drop 1 pts)
      segPic (d, i, a, b) =
        Color (hsb (fromIntegral i / fn * 360) 60 (remap d (-3) 3 100 35))
          (thickLine (remap d (-3) 3 9 4) (pt a) (pt b))
      (curP, _) = proj cur
      -- 右上：横から見た影（x と z）、右下：2 つの音のリサジュー図形
      px = width * 0.68
      wW = min (width * 0.13) (height * 0.17)
      fp = fromIntegral p
      fq = fromIntegral q
      d = gcd p q
      tail'
        | d > 1 = "  (link of " ++ show d ++ " rings)"
        | p == 1 || q == 1 = "  (comes undone)"
        | otherwise = ""
      topLine = (if wAudio w then "Sound on" else "Click for sound")
                ++ "   <- ->: p   Up/Down: q   M: " ++ (if wMelody w then "melody" else "chord")
                ++ "   1-3: " ++ wWaveName w ++ "   B: " ++ showHz (wBase w) ++ " Hz   Space: stop"
      lower :: String
      lower = if wMelody w
                then "Melody: tracing height -> pitch. One lap repeats the phrase."
                else printf "Chord: %.0f Hz and %.0f Hz. Simple ratios blend best." (wBase w * fp) (wBase w * fq)
  in Pictures $
       map segPic (sortOn (\(dp, _, _, _) -> negate dp) segs)
       ++ [ at (pt curP) (Color (gray 1) (disc 11))
          , panel px (height * 0.32) wW "Knot's shadow from the side (x, z)"
              (\s -> let (x, _, z) = knotPt p q s in (x / 3, z)) (curX / 3, curZ)
          , panel px (height * 0.74) wW "Lissajous of the 2 notes (cos p t, sin q t)"
              (\s -> (cos (fp * s), sin (fq * s))) (cos (fp * t), sin (fq * t))
          , txt 20 (height - 60) 20 (gray 1)
              ("(p, q) = (" ++ show p ++ ", " ++ show q ++ ")   ratio " ++ show (p `div` d) ++ " : " ++ show (q `div` d) ++ tail')
          , txt 20 (height - 30) 14 (gray 0.7) lower
          , txt 20 40 16 (gray 1) topLine
          , txt 20 68 14 (gray 0.6) "The white dot traces the knot. Its 2 rotations (p and q times) become the frequency ratio of the 2 notes."
          ]

showHz :: Float -> String
showHz f = if f == fromIntegral (round f :: Int) then show (round f :: Int) else show f

panel :: Float -> Float -> Float -> String -> (Float -> (Float, Float)) -> (Float, Float) -> Picture
panel x y wW title fn (nx, ny) =
  let (gx, gy) = pt (x, y)
      curve = [ let (a, b) = fn (fromIntegral i / 400 * tau) in pt (x + a * wW, y - b * wW) | i <- [0 .. 400 :: Int] ]
  in Pictures
       [ Color (hsb 0 0 30) (rectLine gx gy (wW * 2.2) (wW * 2.2))
       , Color (hsb 180 60 100) (thickPath 2 curve)
       , at (pt (x + nx * wW, y - ny * wW)) (Color (gray 1) (disc 6))
       , txt (x - wW * 1.1) (y - wW * 1.1 - 10) 14 (gray 0.7) title ]

-- ---------- やってみよう ----------
-- 1. (2, 3) (3, 4) (4, 5) (5, 6) を順に鳴らそう。きれいに聞こえるのはどれ？ 交点の数との関係は？
-- 2. (2, 4) のように最大公約数が 2 以上だと絡み目になる。音の比は約分されて (1, 2) ＝ オクターブ
-- 3. メロディモードの音階（scaleSteps）を変えて、結び目ごとの「曲」をつくろう
-- 4. 3 つ目の音を足して、結び目の r（中心からの距離）を音の高さにしてみよう

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
