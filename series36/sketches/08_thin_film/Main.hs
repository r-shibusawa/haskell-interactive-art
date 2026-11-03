-- ============================================
-- 08 薄膜干渉（シャボン玉の色）
-- 薄い膜の表と裏で反射した 2 つの光が干渉する。膜の厚さ t で強め合う波長が変わる → 場所ごとに違う色
--   位相差 δ = 2π · 2 n t cosθ' / λ + π    （+π は「屈折率が大きい物質で反射するとき」の位相の反転）
--   反射の強さ I(λ) ∝ 1 + cos δ   → 波長ごとに計算して足し合わせると見える色になる
-- 1〜3 = 膜の形（くさび・シャボン膜・ニュートン環） / マウス = 厚さの勾配・中心 / ↑↓ = 膜の屈折率 n / P = 位相の反転 ON/off
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

cell, canvasW, canvasH, maxT :: Float
cell = 4                   -- ★ 計算の粗さ（px）
canvasW = 1280
canvasH = 800
maxT = 1200                -- 色の表の最大厚さ（nm）

gw, gh, lutN :: Int
gw = ceiling (canvasW / cell)
gh = ceiling (canvasH / cell)
lutN = floor (maxT / 2) + 1

data World = World
  { wN     :: !Float       -- ★ 膜の屈折率（水 1.33、油 1.45）
  , wInv   :: !Bool        -- 表面での位相の反転を考える
  , wShape :: !Int         -- 1 くさび、2 シャボン膜、3 ニュートン環
  , wTime  :: !Float
  , wLut   :: !(U.Vector Word8)   -- 厚さ 2 nm ごとの色（R, G, B の順）
  }

main :: IO ()
main = runSketch (sketch "36-08 Thin-Film Interference" (World 1.33 True 2 0 (buildLUT 1.33 True)))
  { skBack  = black
  , skStep  = \_ _ w -> pure w { wTime = wTime w + 0.016 }
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 場所ごとのゆらぎ（noise − 0.5）
noiseMap :: U.Vector Float
noiseMap = U.generate (gw * gh) (\idx ->
  let (j, i) = idx `divMod` gw
  in noise2 (fromIntegral i * 0.02) (fromIntegral j * 0.02) - 0.5)

-- ★ 厚さ t(nm) → 色：波長 400〜700 nm を 10 nm ごとに足し合わせる
buildLUT :: Float -> Bool -> U.Vector Word8
buildLUT nFilm inversion = U.fromList (concatMap entry [0 .. lutN - 1])
  where
    nms = [ 400 + 10 * fromIntegral i | i <- [0 .. 30 :: Int] ] :: [Float]
    cols = [ (nm, wavelengthToRGB nm) | nm <- nms ]
    add3f (a, b, c) (d, e, f) = (a + d, b + e, c + f)
    (sumR, sumG, sumB) = foldl' add3f (0, 0, 0) (map snd cols)
    entry k =
      let t = fromIntegral k * 2 :: Float
          term (nm, (r, g, b)) =
            let delta = tau * 2 * nFilm * t / nm + (if inversion then pi else 0)
                i0 = (1 + cos delta) / 2     -- 2 つの反射光の干渉（0〜1）：位相差 0 で強め合う
            in (i0 * r, i0 * g, i0 * b)
          (ar, ag, ab) = foldl' add3f (0, 0, 0) (map term cols)
          q v s = fromIntegral (min 255 (round (255 * (v / s) ** 0.7) :: Int))
      in [q ar sumR, q ag sumG, q ab sumB]

-- 膜の厚さ（nm）
thickness :: World -> Float -> Float -> Int -> Int -> Float
thickness w mx my i j =
  let fi = fromIntegral i
      fj = fromIntegral j
      u = fi / fromIntegral gw
      v = fj / fromIntegral gh
      big = clampF 150 2500 (remap mx 0 canvasW 150 2500)
      nz = U.unsafeIndex noiseMap (j * gw + i)
  in case wShape w of
       1 -> big * u + nz * 60                                      -- くさび（空気層）
       2 -> let vv = v + 0.05 * sin (wTime w * 0.7 + u * 6)        -- 下にたまる
            in big * vv * vv * 0.8 + nz * 120
       _ -> let dx = fi * cell - mx                                -- ニュートン環 t ∝ r²
                dy = fj * cell - my
            in 1800 * ((dx * dx + dy * dy) / (canvasW * canvasW)) * 4

shapeName :: Int -> String
shapeName 1 = "Wedge (air gap)"
shapeName 2 = "Soap film (drains down)"
shapeName _ = "Newton's rings"

drawAll :: Env -> World -> Picture
drawAll env w =
  let (mx, my) = mouseP5 env
      lut = wLut w
      pixel i j =
        let t = thickness w mx my i j
            idx = max 0 (min (lutN - 1) (floor (t / 2))) * 3
        in (U.unsafeIndex lut idx, U.unsafeIndex lut (idx + 1), U.unsafeIndex lut (idx + 2))
      wd = envW env
      h = envH env
      -- 厚さ → 色 の表（下の帯）
      bx = 40
      bw = wd - 80
      by = h - 70
      bar = [ Color (rgb255 (fromIntegral (lut U.! c)) (fromIntegral (lut U.! (c + 1))) (fromIntegral (lut U.! (c + 2))))
                (box (bx + fromIntegral x) by 1.5 26)
            | x <- [0 .. floor bw - 1 :: Int]
            , let c = 3 * floor (fromIntegral x / bw * fromIntegral (lutN - 1)) ]
  in Pictures
    [ pixelImage gw gh cell pixel
    , onP5 env (Pictures
        ( bar ++
        [ Color (withA 0.6 black) (box bx (by - 24) bw 22)
        , Color (withA 0.47 black) (box 10 14 1260 70) ]))
    , txt env bx (by - 7) 14 white "0 nm"
    , txt env (bx + bw - 66) (by - 7) 14 white (nf 0 maxT ++ " nm")
    , txt env (bx + 60) (by - 7) 14 white
        ("Thin: " ++ (if wInv w then "black (the two reflections cancel)" else "bright")
         ++ "   Thick: colors fade to white (many wavelengths mix)")
    , txt env 20 40 18 white
        (shapeName (wShape w) ++ " (1-3)  n = " ++ nf 2 (wN w) ++ " (Up/Down)  P: phase flip " ++ onOff (wInv w)
         ++ "  Mouse: " ++ (if wShape w == 3 then "center" else "max thickness"))
    , txt env 20 68 15 (gray 0.8)
        ("delta = 4 pi n t / lam" ++ (if wInv w then " + pi" else "")
         ++ "   Same thickness, same color. A soap bubble's top turns black just before it pops.")
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wN = min 2 (wN w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wN = max 1 (wN w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wShape = fromEnum c - 48 }
    | c `elem` "pP" -> rebuild w { wInv = not (wInv w) }
  _ -> w
  where rebuild v = v { wLut = buildLUT (wN v) (wInv v) }

-- ---------- やってみよう ----------
-- 1. くさび（1）でマウスを左右に動かす。縞の数が変わる。縞 1 本 = 厚さが λ/(2n) 増えた
-- 2. P を押して位相の反転をなくすと、いちばん薄い所が黒 → 明るいに変わる。本物のシャボン玉はどっち？
-- 3. n を大きくすると縞が詰まる（光路長 2nt が長くなる）。油膜（1.45）と水（1.33）で比べよう
-- 4. ニュートン環（3）：中心からの距離の 2 乗で厚さが増える → 外側ほど縞が細かい。レンズの検査に使われる


-- ==================================================
-- ここから下は「道具」（座標の変換・数字）
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

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- ==================================================
-- ここから下は「色の道具」（波長 nm → RGB の近似、値は 0〜255）
-- ==================================================
wavelengthToRGB :: Float -> (Float, Float, Float)
wavelengthToRGB nm0 =
  let nm = max 380 (min 780 nm0)
      (r, g, b)
        | nm < 440 = (negate (nm - 440) / 60, 0, 1)
        | nm < 490 = (0, (nm - 440) / 50, 1)
        | nm < 510 = (0, 1, negate (nm - 510) / 20)
        | nm < 580 = ((nm - 510) / 70, 1, 0)
        | nm < 645 = (1, negate (nm - 645) / 65, 0)
        | otherwise = (1, 0, 0)
      f | nm < 420 = 0.3 + 0.7 * (nm - 380) / 40
        | nm > 700 = 0.3 + 0.7 * (780 - nm) / 80
        | otherwise = 1
      q v = 255 * (v * f) ** 0.8
  in (q r, q g, q b)

-- 0〜255 の値から色を作る
col255 :: (Float, Float, Float) -> Color
col255 (r, g, b) = makeColor (r / 255) (g / 255) (b / 255) 1
