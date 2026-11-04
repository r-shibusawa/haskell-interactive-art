-- ============================================
-- 10 ピンホールカメラとレンズカメラ
-- 小さな穴（ピンホール）を通った光は、奥の面にさかさまの像をつくる。穴が大きいとボケる、小さすぎても回折でボケる
--   像の大きさ = 物体の大きさ × v / z   （v = 穴からセンサーまで、z = 穴から物体まで）
--   ピンホールのボケ ≈ D (1 + v/z) + 2.44 λ v / D　　レンズのボケ（錯乱円） = D |v − v'| / v'   1/f = 1/z + 1/v'
-- 上の図で物体をドラッグ = 奥行きと位置 / ←→ = 穴の直径 D / ↑↓ = センサーまでの距離 v（レンズ：ピント距離） / L = ピンホール ⇄ レンズ
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import Numeric (showFFloat)

focalLen, lambdaMm :: Float
focalLen = 50              -- ★ レンズの焦点距離 mm
lambdaMm = 0.00055         -- 緑の光の波長 mm

-- 物体：横位置・高さ・奥行き（mm）、半径 mm、色
data Obj = Obj { oLat, oUp, oZ, oR, oHue :: !Float }

data World = World
  { wD      :: !Float      -- ★ 穴（絞り）の直径 mm
  , wV      :: !Float      -- ★ 穴からセンサーまでの距離 mm
  , wFocusZ :: !Float      -- レンズのピントを合わせる距離 mm
  , wLens   :: !Bool
  , wObjs   :: ![Obj]
  , wDrag   :: !Int        -- つかんでいる物体（−1 = なし）
  }

world0 :: World
world0 = World 0.6 50 800 False
  [ Obj (-150) 40 500 40 0, Obj 60 (-20) 800 60 120
  , Obj 200 80 1300 70 210, Obj (-60) (-60) 2000 90 45
  , Obj 120 120 2600 120 300 ] (-1)

main :: IO ()
main = runSketch (sketch "36-10 Pinhole and Lens Camera" world0)
  { skBack  = backC
  , skDraw  = drawAll
  , skEvent = onEvent
  }

backC :: Color
backC = hsb 225 35 10

-- 上の図（真上から見た図）の座標：穴が (hx, hy)、物体は右（z が大きいほど遠く）
topView :: Env -> (Float, Float, Float)
topView env = (140, envH env * 0.25, (envW env - 220) / 3000)

-- 像がセンサーからはみ出さないように、まず像を描き、そのまわりを背景色の長方形でおおう（切り抜きの代わり）
drawSensor :: World -> Float -> Float -> Float -> Float -> Float -> [Obj] -> Picture
drawSensor w sx sy sw sh pxPerMm sorted = Pictures
  ( Color (hsb 0 0 4) (box sx sy sw sh)
  : map one sorted ++
  [ Color backC (box (sx - 2000) (sy - 2000) 2000 (sh + 4000))          -- 左
  , Color backC (box (sx + sw) (sy - 2000) 2000 (sh + 4000))            -- 右
  , Color backC (box (sx - 2000) (sy - 2000) (sw + 4000) 2000)          -- 上
  , Color backC (box (sx - 2000) (sy + sh) (sw + 4000) 2000)            -- 下
  ])
  where
    v = wV w
    d = wD w
    one o =
      let mag = v / oZ o
          ix = sx + sw / 2 - oLat o * mag * pxPerMm     -- さかさま
          iy = sy + sh / 2 + oUp o * mag * pxPerMm
          sz = oR o * mag * 2 * pxPerMm
          vp = 1 / (1 / focalLen - 1 / oZ o)
          blurMm | not (wLens w) = d * (1 + v / oZ o) + 2.44 * lambdaMm * v / d    -- 幾何のボケ + 回折のボケ
                 | vp > 0 = d * abs (v - vp) / vp
                 | otherwise = d
          blur = min 300 (blurMm * pxPerMm)
          -- ボケを同心円で表す（ぼかしの幅だけ薄く広げる）
          steps = 7 :: Int
          maxR = sz / 2 + blur / 2
          a = 95 / fromIntegral steps * min 1 ((sz / 2) / maxR + 0.15)
      in Color (hsbA (oHue o) 70 100 a) (Pictures
           [ at (ix, iy) (disc (sz / 2 + (blur / 2) * (fromIntegral k / fromIntegral steps)))
           | k <- [steps, steps - 1 .. 1] ])

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      (hx, hy, s) = topView env
      d = wD w
      v = wV w
      lens = wLens w
      sorted = sortOn (negate . oZ) (wObjs w)     -- 遠い順
      -- ---- 下：センサーの像（48 × 36 mm）
      sw = 480
      sh = 360
      sx = wd * 0.5 - sw / 2
      sy = h - sh - 40
      pxPerMm = sw / 48
      -- ---- 上：真上から見た図
      camX = hx - v * s * 2 - 10
      gap = d * (if lens then 2 else 10)          -- 穴の見た目の半径（レンズは D が 25 まであるので縮めて描く）
      bladeH = 60 - gap
      topObj o =
        let x = hx + oZ o * s
            y = hy + oLat o * s * 0.9
        in Pictures
          [ Color (hsb (oHue o) 70 100) (at (x, y) (disc (oR o * s + 4)))
          , Color (hsbA (oHue o) 70 100 40) (Line [(x, y), (hx, hy)])
          , Color (hsbA (oHue o) 70 100 40)
              (Line [(hx, hy), (hx - v * s * 2, hy - oLat o * s * 0.9 * (v * 2 / oZ o))]) ]
      topLabels = [ txt env (hx + oZ o * s - 28) (hy + oLat o * s * 0.9 + oR o * s + 20) 14 white (nf 0 (oZ o) ++ " mm")
                  | o <- sorted ]
      fx = hx + wFocusZ w * s
      px = sx + sw + 24
      py = sy + 18
      best = sqrt (2.44 * lambdaMm * v)
      vNow = 1 / (1 / focalLen - 1 / wFocusZ w)
      panel
        | not lens =
            [ txt env px py 15 white "Pinhole"
            , txt env px (py + 26) 15 white ("D = " ++ nf 2 d ++ " mm  v = " ++ nf 0 v ++ " mm")
            , txt env px (py + 56) 15 white "Blur ~ D(1 + v/z) + 2.44 lam v/D"
            , txt env px (py + 82) 15 (hsb 45 90 100) "Sharpest D ~ sqrt(2.44 lam v)"
            , txt env px (py + 104) 15 (hsb 45 90 100) ("         = " ++ nf 2 best ++ " mm")
            , txt env px (py + 134) 14 (hsb 0 0 60) "Big hole: thick beam blurs."
            , txt env px (py + 156) 14 (hsb 0 0 60) "Tiny hole: diffraction blurs"
            , txt env px (py + 178) 14 (hsb 0 0 60) "(and the image gets dark)." ]
        | otherwise =
            [ txt env px py 15 white ("Lens  f = " ++ nf 0 focalLen ++ " mm  D = " ++ nf 1 d ++ " mm")
            , txt env px (py + 26) 15 white ("Focus " ++ nf 0 (wFocusZ w) ++ " mm -> v = " ++ nf 1 vNow ++ " mm")
            , txt env px (py + 56) 15 white "Blur circle = D |v - v'| / v'"
            , txt env px (py + 82) 15 (hsb 45 90 100) ("f-number = f/D = " ++ nf 1 (focalLen / d))
            , txt env px (py + 112) 14 (hsb 0 0 60) "Open up (big D): all but the"
            , txt env px (py + 134) 14 (hsb 0 0 60) "focus blurs (shallow depth)."
            , txt env px (py + 156) 14 (hsb 0 0 60) "Stop down (small D): all sharp,"
            , txt env px (py + 178) 14 (hsb 0 0 60) "like a pinhole." ]
  in Pictures
    [ onP5 env (Pictures
        [ drawSensor w sx sy sw sh pxPerMm sorted
        , Color (hsbA 0 0 100 50) (rectLine (sx + sw / 2) (sy + sh / 2) sw sh)
        , Color (hsbA 0 0 100 5) (box 20 100 (wd - 40) (h * 0.33))
        , Color (hsbA 0 0 100 30) (Line [(hx, hy), (wd - 40, hy)])
        -- カメラ（箱）
        , Color (hsb 0 0 30) (box camX (hy - 60) (v * s * 2 + 10) 120)
        , Color white (box camX (hy - 44) 4 88)        -- センサー
        , Color (hsb (if lens then 190 else 45) 60 100) (Pictures
            [ box (hx - 2) (hy - 60) 4 bladeH
            , box (hx - 2) (hy + gap) 4 bladeH ])
        , Pictures (map topObj sorted)
        , if lens then Color (hsbA 190 80 100 60) (thickLine 2 (fx, hy - 70) (fx, hy + 70)) else Blank
        ])
    , Pictures topLabels
    , if lens then txt env (fx - 40) (hy - 76) 14 (hsb 190 80 100) ("Focus " ++ nf 0 (wFocusZ w)) else Blank
    , txt env sx (sy - 8) 14 (hsb 0 0 60) "Image on the sensor (48 x 36 mm)"
    , Pictures panel
    , txt env 20 40 18 white
        ((if lens then "Lens camera" else "Pinhole camera") ++ " (L)  D = " ++ nf 2 d ++ " mm (L/R)  "
         ++ (if lens then "Focus " ++ nf 0 (wFocusZ w) ++ " mm" else "v = " ++ nf 0 v ++ " mm") ++ " (Up/Down)")
    , txt env 20 68 15 (hsb 0 0 60)
        "Drag a circle in the top view. The image is upside down; far things look small (size x v/z)."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = foldl' (\b (i, o) ->
          if dist2 (mx, my) (hx + oZ o * s, hy + oLat o * s * 0.9) < oR o * s + 10 then i else b) (wDrag w) (zip [0 ..] (wObjs w)) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 && envDown env ->
    w { wObjs = [ if i == wDrag w
                    then o { oZ = clampF 200 3000 ((mx - hx) / s), oLat = clampF (-300) 300 ((my - hy) / (s * 0.9)) }
                    else o
                | (i, o) <- zip [0 ..] (wObjs w) ] }
  EventKey (SpecialKey KeyRight) Down _ _ -> fixV w { wD = min (if lens then 25 else 3) (wD w + (if lens then 1 else 0.1)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> fixV w { wD = max (if lens then 1 else 0.05) (wD w - (if lens then 1 else 0.1)) }
  EventKey (SpecialKey KeyUp) Down _ _
    | lens -> fixV w { wFocusZ = min 3000 (wFocusZ w + 100) }
    | otherwise -> w { wV = min 150 (wV w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _
    | lens -> fixV w { wFocusZ = max 200 (wFocusZ w - 100) }
    | otherwise -> w { wV = max 15 (wV w - 5) }
  EventKey (Char c) Down _ _
    | c `elem` "lL" -> if lens then w { wLens = False, wD = 0.6, wV = 50 }
                               else fixV w { wLens = True, wD = 8 }
  _ -> w
  where
    (mx, my) = mouseP5 env
    (hx, hy, s) = topView env
    lens = wLens w
    fixV v | wLens v = v { wV = 1 / (1 / focalLen - 1 / wFocusZ v) }
           | otherwise = v

-- ---------- やってみよう ----------
-- 1. D を 0.05 から 3 まで変えて、像がいちばんくっきりする D を探そう。√(2.44λv) と合う？
-- 2. v を大きく（↑）すると像が大きくなる（望遠）。でも暗くなる（実際のカメラも同じ）
-- 3. L でレンズに。D = 25 にして手前にピントを合わせると、奥が大きくボケる（ポートレート写真）
-- 4. 物体をピント距離の前後に置いて、どちら側のボケが大きいか比べよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""
