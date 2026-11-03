-- ============================================
-- 10 自然の中のボロノイ（ウォーリーノイズ）
-- キリンの模様、ひび割れた泥、細胞、トンボの羽 … 自然にはボロノイ図のような模様がたくさんある
--   ウォーリーノイズ：F1 = いちばん近い点までの距離、F2 = 2 番目に近い点までの距離。F2 − F1 が小さい所 = 境界
--   点はマス目に 1 個ずつ置くので、調べるのはまわり 3×3 マスの 9 個だけ → 速い
-- 1 = キリン / 2 = ひび割れた泥 / 3 = 細胞 / 4 = トンボの羽 / ↑↓ = 模様の大きさ / スペース = 一時停止 / P = 点を表示
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (xor, (.&.))
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

res :: Float
res = 4                   -- ★ 何画素ずつまとめて計算するか

gw, gh :: Int
gw = ceiling (width / res)
gh = ceiling (height / res)

-- マスごとの乱数（点の位置と色のゆらぎ）
data Seeds = Seeds { sdA, sdB, sdS, sdV :: !(U.Vector Float) }

data World = World
  { wStyle  :: !Int
  , wGs     :: !Float     -- ★ マス目の大きさ（画面の px。大きいほど模様が大きい）
  , wT      :: !Float
  , wPause  :: !Bool
  , wPts    :: !Bool
  , wSeeds  :: !Seeds
  }

main :: IO ()
main = runSketch (sketch "22-10 Nature Worley" (World 1 100 0 False False (Seeds U.empty U.empty U.empty U.empty)))
  { skStart = \_ -> do
      let list lo hi = U.fromList <$> replicateM 4096 (randomRIO (lo, hi))
      sd <- Seeds <$> list 0 tau <*> list 0 tau <*> list 0.5 1.5 <*> list 0 1
      pure (World 1 100 0 False False sd)
  , skStep  = \_ _ w -> pure (if wPause w then w else w { wT = wT w + 0.01 })
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

seedOf :: Int -> Int -> Int
seedOf i j = ((i * 73856093) `xor` (j * 19349663)) .&. 4095

-- マス (i, j) の点の位置（ゆっくり動く）
featurePoint :: World -> Int -> Int -> (Float, Float, Float)
featurePoint w i j =
  let sd = wSeeds w
      k = seedOf i j
      s = U.unsafeIndex (sdS sd) k
      t = wT w
  in ( fromIntegral i + 0.5 + 0.4 * sin (t * s + U.unsafeIndex (sdA sd) k)
     , fromIntegral j + 0.5 + 0.4 * sin (t * s * 0.8 + U.unsafeIndex (sdB sd) k)
     , U.unsafeIndex (sdV sd) k )

-- トンボの羽は横に長いセル
stretch :: Int -> (Float, Float)
stretch style = if style == 4 then (0.55, 1.6) else (1, 1)

gridSize :: World -> (Int, Int)
gridSize w =
  let (sx, sy) = stretch (wStyle w)
  in (ceiling (width * sx / wGs w) + 3, ceiling (height * sy / wGs w) + 3)

data F = F !Float !Float !Float

-- ★ 模様ごとの色の決め方（F1, F2 − F1 を使う）
{-# INLINE shade #-}
shade :: Int -> Float -> Float -> Float -> Float -> Float -> (Float, Float, Float)
shade style gs f1 f2 v x
  | style == 1 =                                      -- キリン：太いクリーム色のすき間と茶色のまだら
      if e < 0.16 then (238, 222, 180) else
      let k = 0.75 + 0.25 * v - 0.25 * f1 in (150 * k + 20, 85 * k, 35 * k)
  | style == 2 =                                      -- ひび割れた泥：細い黒いひびと、縁が反り返った明るい土
      if e < max 0.05 lw then (45, 30, 20) else
      let k = 0.7 + 0.35 * min 1 (e * 2) + 0.1 * v in (160 * k, 118 * k, 78 * k)
  | style == 3 =                                      -- 細胞：明るい膜、紫の核、ピンクの細胞質
      if e < max 0.06 lw then (250, 235, 245)
      else if f1 < 0.16 + 0.05 * v then (90, 40, 120)
      else let k = 1 - 0.5 * f1 in (230 * k, 120 * k + 40 * v, 170 * k)
  | e < max 0.035 lw = (30, 35, 40)                   -- トンボの羽：細い脈と、虹色に光る膜
  | otherwise = hsbToRgb (wrapF 360 (200 + 120 * f1 + 40 * sin (x * 0.05))) (25 + 20 * v) (92 - 25 * f1)
  where
    e = f2 - f1
    lw = res * (if style == 4 then 1.6 else 1) * 1.8 / gs   -- lw = 線が途切れない最低の太さ（画素 2〜3 個分）

paintBuffer :: World -> Picture
paintBuffer w = pixelImage gw gh res pixel
  where
    style = wStyle w
    gs = wGs w
    (sx, sy) = stretch style
    (cw, ch) = gridSize w
    -- このフレームの点の位置（マスごとに 1 回だけ計算しておく）
    pts = U.generate (cw * ch) (\k -> let (j, i) = k `divMod` cw in featurePoint w (i - 1) (j - 1))
    fx = U.map (\(a, _, _) -> a) pts
    fy = U.map (\(_, b, _) -> b) pts
    fv = U.map (\(_, _, c) -> c) pts
    -- ★ ウォーリーノイズ：F1, F2 と、いちばん近い点の乱数 v を返す
    worley !x !y =
      let ci = floor x :: Int
          cj = floor y :: Int
          loop !a !b !f1 !f2 !v
            | b > 1 = F (sqrt f1) (sqrt f2) v
            | a > 1 = loop (-1) (b + 1) f1 f2 v
            | otherwise =
                let k = (ci + a + 1) + (cj + b + 1) * cw
                    dx = U.unsafeIndex fx k - x
                    dy = U.unsafeIndex fy k - y
                    d = dx * dx + dy * dy      -- 比べるだけなら √ はいらない（最後に 2 回だけ計算する）
                in if d < f1 then loop (a + 1) b d f1 (U.unsafeIndex fv k)
                   else if d < f2 then loop (a + 1) b f1 d v
                   else loop (a + 1) b f1 f2 v
      in loop (-1 :: Int) (-1 :: Int) 81 81 0
    pixel gx gy =
      let x = (fromIntegral gx + 0.5) * res * sx / gs
          y = (fromIntegral gy + 0.5) * res * sy / gs
          F f1 f2 v = worley x y
          (r, g, b) = shade style gs f1 f2 v (fromIntegral gx)
      in (w8 r, w8 g, w8 b)

drawAll :: World -> Picture
drawAll w = Pictures
  [ paintBuffer w
  , view (points ++ [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)])
  , txt 20 30 16 (gray 1) ("1-4: " ++ styleName ++ "  Up/Down: size " ++ show (round (wGs w) :: Int) ++ "  Space: " ++ (if wPause w then "resume" else "pause") ++ "  P: points " ++ onOff (wPts w))
  , txt 20 52 14 (gray 0.8) "F1 = distance to the nearest point, F2 = to the second. F2 - F1 near 0 is a boundary (Voronoi edge)."
  , txt 20 74 14 (gray 0.8) "Only the threshold and the coloring change: giraffe, mud, cells, wings."
  ]
  where
    styleName = case wStyle w of
      1 -> "Giraffe"
      2 -> "Cracked mud"
      3 -> "Cells"
      _ -> "Dragonfly wing"
    (cw, ch) = gridSize w
    (sx, sy) = stretch (wStyle w)
    points
      | wPts w = [ dotAt (gray 1) (x * wGs w / sx, y * wGs w / sy) 6
                 | j <- [0 .. ch - 3], i <- [0 .. cw - 3], let (x, y, _) = featurePoint w i j ]
      | otherwise = []

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wGs = min 300 (wGs w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGs = max 30 (wGs w - 10) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPause = not (wPause w) }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> pure w { wStyle = 1 }
    '2' -> pure w { wStyle = 2 }
    '3' -> pure w { wStyle = 3 }
    '4' -> pure w { wStyle = 4 }
    'p' -> pure w { wPts = not (wPts w) }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. キリン（1）のしきい値 0.16 を 0.05 や 0.3 にしてみよう。どの動物に見える？（ヒョウ？ カメの甲羅？）
-- 2. F1 だけで明るさを決めると「泡」や「水面のゆらぎ（コースティクス）」のように見える。shade を書きかえよう
-- 3. 点を 2 段階（大きなマスと小さなマス）重ねて、大きなひびと小さなひびのある泥にしよう
-- 4. 0.4 * sin の 0.4 を 0 にすると、点がマスの中心にそろう。模様はどうなる？（正方形のタイル）

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 色の変換（HSB → RGB 0〜255）
hsbToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hsbToRgb h s0 b0 = (f 5, f 3, f 1)
  where
    s = s0 / 100
    b = b0 / 100
    f :: Float -> Float
    f n = let k = wrapF 6 (n + h / 60) in 255 * b * (1 - s * max 0 (min k (min (4 - k) 1)))

w8 :: Float -> Word8
w8 v = fromIntegral (max 0 (min 255 (truncate (v + 0.5) :: Int)))
