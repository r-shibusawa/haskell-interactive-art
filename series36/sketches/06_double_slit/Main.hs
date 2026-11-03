-- ============================================
-- 06 2 つの波源の干渉（ヤングの実験）
-- 2 つのスリット（点波源）から出た波が重なり、山と山で強め合い、山と谷で打ち消し合う
--   画面の各点で 2 つの波を足す： cos(k r1 − ωt) + cos(k r2 − ωt)   （k = 2π/λ）
--   スクリーン上の明るい縞の間隔  Δy ≈ λ L / d   （L = スクリーンまでの距離、d = スリット間隔）
-- ↑↓ = 波長 λ / ←→ = スリット間隔 d / T = 瞬間の波 ⇄ 時間平均の明るさ / 1 = 波源 1 つだけ / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

cell :: Float
cell = 3.2                 -- ★ 計算の粗さ（px）小さいほどきれいだが重い

sx, screenX, canvasW, canvasH :: Float
sx = 90                    -- 波源の x
screenX = canvasW - 300    -- スクリーンの x
canvasW = 1280
canvasH = 800

gw, gh :: Int
gw = ceiling (screenX / cell)
gh = ceiling (canvasH / cell)

data World = World
  { wLambda :: !Float      -- ★ 波長（px）
  , wD      :: !Float      -- ★ スリット間隔（px）
  , wSingle :: !Bool
  , wPaused :: !Bool
  , wAvg    :: !Bool
  , wT      :: !Float
  , wR1, wR2 :: !(U.Vector Float)   -- 各マスから 2 つの波源までの距離
  }

world0 :: World
world0 = withDistances (World 18 70 False False False 0 U.empty U.empty)

main :: IO ()
main = runSketch (sketch "36-06 Two-Source Interference" world0)
  { skBack  = gray (10 / 255)
  , skStep  = \_ _ w -> pure (if wPaused w then w else w { wT = wrapF tau (wT w + 0.18) })
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 各マスから 2 つの波源までの距離（d が変わったときだけ計算）
withDistances :: World -> World
withDistances w =
  let cy = canvasH * 0.5
      d = wD w
      table y0 = U.generate (gw * gh) (\idx ->
        let (j, i) = idx `divMod` gw
            x = fromIntegral i * cell + cell / 2
            y = fromIntegral j * cell + cell / 2
        in norm2 (x - sx, y - y0))
  in w { wR1 = table (cy - d / 2), wR2 = table (cy + d / 2) }

-- 値 −1〜1 → 色の表（青 ← 黒 → 橙）
lut :: U.Vector Word8
lut = U.fromList (concatMap entry [0 .. 255 :: Int])
  where
    entry i =
      let v = (fromIntegral i - 128) / 128 :: Float
          q c = fromIntegral (max 0 (min 255 (round (c * abs v) :: Int)))
      in if v > 0 then [q 255, q 170, q 60] else [q 60, q 150, q 255]

drawAll :: Env -> World -> Picture
drawAll env w =
  let lam = wLambda w
      d = wD w
      single = wSingle w
      k = tau / lam
      t = wT w
      r1 = wR1 w
      r2 = wR2 w
      -- ★ 波の重ね合わせをピクセルごとに計算
      pixel x y =
        let idx = y * gw + x
            a = U.unsafeIndex r1 idx
            b = U.unsafeIndex r2 idx
            amp1 = 6 / sqrt (a + 20)
            amp2 = if single then 0 else 6 / sqrt (b + 20)
            v | wAvg w =
                  -- 時間平均の強さ I = A1² + A2² + 2 A1 A2 cos(k(r1 − r2))
                  min 1 ((amp1 * amp1 + amp2 * amp2 + 2 * amp1 * amp2 * cos (k * (a - b))) * 1.6)
              | otherwise = amp1 * cos (k * a - t) + amp2 * cos (k * b - t)
            ci = max 0 (min 255 (floor (128 + 127 * v))) :: Int
        in (U.unsafeIndex lut (ci * 3), U.unsafeIndex lut (ci * 3 + 1), U.unsafeIndex lut (ci * 3 + 2))
      imgW = fromIntegral gw * cell
      imgH = fromIntegral gh * cell
      cy = canvasH * 0.5
      l = screenX - sx
      -- スクリーン上の明るさ（時間平均）のグラフ
      curve = [ (screenX + 10 + i0 / (4 / l) * 220, y)
              | n <- [0 .. floor (canvasH / 2) - 1 :: Int]
              , let y = fromIntegral n * 2
                    a = norm2 (screenX - sx, y - (cy - d / 2))
                    b = norm2 (screenX - sx, y - (cy + d / 2))
                    a1 = 1 / sqrt a
                    a2 = if single then 0 else 1 / sqrt b
                    i0 = a1 * a1 + a2 * a2 + 2 * a1 * a2 * cos (k * (a - b)) ]
      -- 明線の位置（理論）
      dy = lam * l / d
      fringes = if single then [] else
        [ Line [(screenX, y), (screenX + 240, y)]
        | m <- [-12 .. 12 :: Int], let y = cy + fromIntegral m * dy, y > 0, y < canvasH ]
      tx = screenX + 10
  in Pictures
    [ Translate (imgW / 2 - canvasW / 2) (canvasH / 2 - imgH / 2) (pixelImage gw gh cell pixel)
    , Translate (negate canvasW / 2) (canvasH / 2) (Scale 1 (-1) (Pictures
        ( [ Color (gray (60 / 255)) (rect (sx - 5) (canvasH / 2) 10 canvasH)
          , Color white (at (sx, cy - d / 2) (disc 4))
          , if single then Blank else Color white (at (sx, cy + d / 2) (disc 4))
          , Color (gray (80 / 255)) (thickLine 4 (screenX, 0) (screenX, canvasH))
          , Color (rgb255 255 200 80) (thickPath 2 curve)
          , Color (rgba255 255 255 255 70) (Pictures fringes)
          -- 文字の下地
          , Color (withA 0.55 black) (rect 490 56 980 72) ])))
    , txt env 20 40 18 white
        ("Lambda = " ++ nf 0 lam ++ " px (Up/Down)  d = " ++ nf 0 d ++ " px (L/R)  T: "
         ++ (if wAvg w then "time average" else "instant wave"))
    , txt env 20 64 15 (gray 0.8)
        ("1: sources " ++ (if single then "1" else "2") ++ "  Space: " ++ (if wPaused w then "play" else "pause")
         ++ "  Orange = crest, blue = trough. Fringe gap = lam L/d = " ++ nf 1 dy ++ " px")
    , txt env 20 86 14 (gray 0.8) "Right: yellow = screen brightness (time average), white = predicted bright fringes."
    , txt env tx (envH env - 56) 14 (gray 0.8) "Wider d: finer fringes."
    , txt env tx (envH env - 38) 14 (gray 0.8) "Longer lambda: wider fringes."
    , txt env tx (envH env - 20) 14 (gray 0.8) "Fringes show light is a wave."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLambda = min 60 (wLambda w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLambda = max 6 (wLambda w - 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> withDistances w { wD = min 300 (wD w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> withDistances w { wD = max 10 (wD w - 10) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> w { wAvg = not (wAvg w) }
    | c == '1' -> w { wSingle = not (wSingle w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. T で時間平均を見る。縞が双曲線の形になる。縞の数は d/λ と関係がある
-- 2. 1 を押して波源を 1 つにすると縞が消える。干渉には 2 つの波が要る
-- 3. λ を 2 倍にすると縞の間隔は？ d を 2 倍にすると？ Δy = λL/d を確かめよう
-- 4. 波源を 3 つ・4 つに増やす（wR3 を足す）と縞が鋭くなる → 回折格子（07）


-- ==================================================
-- ここから下は「道具」（文字・数字）
-- ==================================================
-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""
