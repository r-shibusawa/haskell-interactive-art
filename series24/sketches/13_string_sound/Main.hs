-- ============================================
-- 13 振動を聞く
-- 弦を N 個のおもりとばねに分けて、波動方程式 y'' = c² y_xx を差分で解く。マウスではじくと揺れが左右に伝わる
--   音は「基準モード（07）」の足し算：はじいた形をモードに分けて、ピックアップ（マイク）の位置で聞く
-- ドラッグ = 弦をつまんで離す / ←→ = ピックアップの位置 / ↑↓ = 音の高さ / 1〜5 = モードだけ鳴らす / D = 減衰
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

nMass, modes :: Int
nMass = 80      -- ★ おもりの数（弦の分け方）
modes = 24      -- ★ 音にするモードの数

decay :: Float
decay = 1.5     -- ★ 音が消えるまでの時間の目安 [s]

data World = World
  { wY, wVy   :: !(U.Vector Float)    -- 長さ N + 2（両端は固定）
  , wF0       :: !Float               -- ★ 基本の音の高さ [Hz]（A2）
  , wPickup   :: !Float               -- ピックアップの位置（0〜1）
  , wGrabbing :: !Bool
  , wGrabX, wGrabY :: !Float
  , wDamp     :: !Bool
  }

main :: IO ()
main = runSketch (sketch "24-13 Listening to a String" world0)
  { skBack  = hsb 270 30 10
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World zeros zeros 110 0.2 False 0.5 0 True
  where zeros = U.replicate (nMass + 2) 0

sx :: Float -> Float
sx i = remap i 0 (fromIntegral nMass + 1) (width * 0.05) (width * 0.95)

cy, amp :: Float
cy = height * 0.36
amp = height * 0.18

-- はじく前の三角形の形
triShape :: Float -> Float -> U.Vector Float
triShape px py = U.generate (nMass + 2) $ \i ->
  if i == 0 || i == nMass + 1 then 0 else
  let u = fromIntegral i / fromIntegral (nMass + 1)
  in if u < px then py * u / px else py * (1 - u) / (1 - px)

-- いまの形を基準モード sin(nπx) に分ける（フーリエ級数の係数）
analyze :: U.Vector Float -> [Float]
analyze y =
  [ 2 * s / fromIntegral (nMass + 1)
  | n <- [1 .. modes]
  , let k = fromIntegral n * pi / fromIntegral (nMass + 1) :: Float
  , let s = U.ifoldl' (\acc i v -> if i >= 1 && i <= nMass then acc + v * sin (k * fromIntegral i) else acc) 0 y ]

stepWorld :: World -> World
stepWorld w
  | wGrabbing w =
      let y = triShape (clampF 0.02 0.98 (wGrabX w)) (wGrabY w)
      in w { wY = U.map (clampF (-2) 2) y, wVy = U.replicate (nMass + 2) 0 }
  | otherwise =
      -- ★ 波動方程式の差分：となりとの高さの差が力になる（画面はスローモーション。本物は 1 秒に f₀ 回揺れる）
      let dmp = if wDamp w then 0.0015 else 0
          one :: (U.Vector Float, U.Vector Float) -> (U.Vector Float, U.Vector Float)
          one (y, vy) =
            let vy' = U.imap (\i v -> if i == 0 || i == nMass + 1 then v else
                                  v + (U.unsafeIndex y (i - 1) - 2 * U.unsafeIndex y i + U.unsafeIndex y (i + 1)) * 0.5 - dmp * v) vy
                y' = U.imap (\i q -> if i == 0 || i == nMass + 1 then q else q + U.unsafeIndex vy' i * 0.5) y
            in (y', vy')
          (y4, v4) = one (one (one (one (wY w, wVy w))))
      in w { wY = U.map (clampF (-2) 2) y4, wVy = v4 }

drawAll :: World -> Picture
drawAll w = Pictures
  [ -- 弦
    view ( Color (hsb 0 0 30) (Line [(sx 0, cy), (sx nf1, cy)])
         : Color (hsb 45 80 100) (thickPath 3 [(sx (fromIntegral i), cy - v * amp) | (i, v) <- zip [0 :: Int ..] (U.toList y)])
         : [Color (hsb 45 60 90) (at (sx (fromIntegral i), cy - U.unsafeIndex y i * amp) (disc 2.5)) | i <- [1, 3 .. nMass]]
        ++ [ Color (hsb 0 0 60) (boxAt (sx 0 - 8) (cy - 40) 8 80)
           , Color (hsb 0 0 60) (boxAt (sx nf1) (cy - 40) 8 80)
             -- ピックアップ
           , Color (hsb 200 70 100) (boxAt (pxp - 14) (cy + amp + 20) 28 16)
           , Color (hsba 200 70 100 0.6) (Line [(pxp, cy - amp), (pxp, cy + amp + 20)]) ] )
  , txtC pxp (cy + amp + 54) 14 (hsb 200 70 100) "pickup"
    -- 下：モードの大きさ（ピックアップの重みつき = 聞こえる音のスペクトル）
  , view ( Color (hsb 0 0 14) (boxAt gx gy gw gh)
         : [ Color (hsb (remap (fromIntegral n) 1 (fromIntegral modes) 30 300) 60 95)
               (boxAt (gx + fromIntegral (n - 1) * bw + 3) (gy + gh - 20 - hh) (bw - 6) hh)
           | (n, a) <- zip [1 :: Int ..] amps
           , let aa = abs (a * sin (fromIntegral n * pi * wPickup w))
           , let hh = clampF 0 1 (aa / 0.6) * (gh - 40) ] )
  , Pictures [ txtC (gx + (fromIntegral n - 0.5) * bw) (gy + gh - 4) 14 (hsb 0 0 60) (nf 0 (fromIntegral n * wF0 w))
             | n <- [1 .. modes], n <= 12 || n `mod` 4 == 0 ]
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 75) "Audible harmonics = |mode size x sin(n pi p)|  (p = pickup; numbers = n x f0 in Hz; a discrete string's high modes run flat)"
  , txt 20 36 17 (gray 1) ("Drag: pinch and release   Pickup " ++ nf 2 (wPickup w) ++ " (Left/Right)   f0 = " ++ nf 0 (wF0 w) ++ " Hz (Up/Down)   1-5: mode   D: damping " ++ (if wDamp w then "ON" else "OFF"))
  , txt 20 66 14 (hsb 0 0 65) "Where you pluck changes the tone: plucking the middle removes even harmonics. The pickup position changes it too."
  ]
  where
    y = wY w
    nf1 = fromIntegral nMass + 1 :: Float
    pxp = sx (wPickup w * nf1)
    amps = analyze y
    gx = width * 0.05
    gy = height * 0.68
    gw = width * 0.9
    gh = height * 0.24
    bw = gw / fromIntegral modes

-- 弦のモード n の周波数（N 個のおもりの弦：ω_n ∝ sin(nπ / 2(N+1))）
modeFreq :: Float -> Int -> Float
modeFreq f0 n =
  let d = 2 * fromIntegral (nMass + 1) :: Float
  in f0 * sin (fromIntegral n * pi / d) / sin (pi / d)

-- ★ モードの足し算で音をつくる：Σ aₙ sin(nπp) sin(2π fₙ t) e^(−t/τₙ)（高いモードほど早く消える）
--   同時に鳴らせるのは 8 音までなので、大きいモードから 8 つを、それぞれサイン波で重ねる
pluckSound :: World -> IO ()
pluckSound w = do
  let weighted = [ (n, abs a * abs (sin (fromIntegral n * pi * wPickup w)))
                 | (n, a) <- zip [1 ..] (analyze (wY w)) ]
      top = take 8 (sortOn (negate . snd) [q | q@(_, a) <- weighted, a >= 1e-4])
      total = max 1e-6 (sum (map snd top))
  forM_ top $ \(n, a) -> do
    let tauN = decay / (1 + 0.15 * fromIntegral n)
    playTone (modeFreq (wF0 w) n) (0.8 * a / total) (min (decay * 2) (tauN * 3)) Sine

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | my > height * 0.1 && my < height * 0.64 -> pure (grabAt mx my w { wGrabbing = True })
    where (mx, my) = toP5 pos
  EventMotion pos | wGrabbing w -> let (mx, my) = toP5 pos in pure (grabAt mx my w)
  EventKey (MouseButton LeftButton) Up _ _ | wGrabbing w -> do
    pluckSound w
    pure w { wGrabbing = False }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wPickup = max 0.03 (wPickup w - 0.03) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wPickup = min 0.97 (wPickup w + 0.03) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wF0 = min (wF0 w * 2 ** (1 / 12)) 440 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wF0 = max (wF0 w / 2 ** (1 / 12)) 55 }
  EventKey (Char c) Down _ _
    | toLower c == 'd' -> pure w { wDamp = not (wDamp w) }
    | c >= '1' && c <= '5' -> do
        let n = fromEnum c - fromEnum '0'
            w1 = w { wY = U.generate (nMass + 2) (\i ->
                       if i == nMass + 1 then 0
                       else 0.6 * sin (fromIntegral n * pi * fromIntegral i / fromIntegral (nMass + 1)))
                   , wVy = U.replicate (nMass + 2) 0 }
        pluckSound w1
        pure w1
  _ -> pure w

grabAt :: Float -> Float -> World -> World
grabAt mx my w = w { wGrabX = (mx - sx 0) / (sx (fromIntegral nMass + 1) - sx 0)
                   , wGrabY = clampF (-1) 1 ((cy - my) / amp) }

-- ---------- やってみよう ----------
-- 1. 端の近くではじく（明るい音）と、まん中ではじく（丸い音）。スペクトルの棒を比べる
-- 2. ピックアップをまん中（0.5）に置くと、偶数番のモードが全部聞こえなくなる。なぜ？（そこが節）
-- 3. 1〜5 でモードだけ鳴らすと純音（サイン波）。2 は 1 オクターブ上、3 はその 5 度上…これが倍音
-- 4. nMass を 10 にすると、高いモードの周波数が整数倍からずれる（分散）。音はどう変わる？


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v
