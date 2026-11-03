-- ============================================
-- 07 グレースケール・モルフォロジー
-- 0/1 のかわりに明るさ（0〜1）。膨張 = SE の中の最大値、収縮 = 最小値 ── それだけで同じ道具が全部使える
-- トップハット（元 − オープニング）= 小さく明るいもの、ブラックハット（クロージング − 元）= 小さく暗いもの
-- 1〜7 で操作 / 左右で SE の大きさ / K で SE の形 / N で別の画像 / マウス上下 = 断面の行
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 80
rows = 56

oy :: Float
oy = 130

type Gray = U.Vector Float

data World = World
  { wGray   :: !Gray
  , wResult :: !Gray
  , wOp     :: !Int   -- ★ 1 元 2 膨張 3 収縮 4 オープン 5 クローズ 6 トップハット 7 ブラックハット
  , wR      :: !Int   -- ★ SE の半径
  , wBox    :: !Bool  -- ★ True = 正方形、False = 円
  , wSeed   :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-07 Grayscale Morphology" (recompute (makeImage (World e e 2 2 False 1))))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }
  where e = U.replicate (cols * rows) 0

-- ★ 画像：ゆるい明るさの変化 + 明るい丸 + 暗い四角 + 細かい点
makeImage :: World -> World
makeImage w = w { wGray = U.generate (cols * rows) pixel }
  where
    sd = fi (wSeed w) * 17.3  -- 乱数の種ごとにノイズの場所を変える
    us = U.fromListN (cols * rows * 2) (randomRs (0, 1 :: Float) (mkStdGen (wSeed w)))
    pixel i =
      let x = i `rem` cols
          y = i `quot` cols
          base = 0.25 + 0.3 * fbm2 4 (fi x * 0.05 + sd) (fi y * 0.05) + 0.2 * (fi x / fi cols)
          a = if distI x y 20 20 < 9 then 0.4 else 0
          b = if distI x y 58 36 < 12 then 0.3 else 0
          d = if x > 40 && x < 52 && y > 8 && y < 24 then -0.3 else 0
          n | us U.! (2 * i) < 0.04 = if us U.! (2 * i + 1) < 0.5 then 0.5 else -0.5
            | otherwise = 0
      in clampF 0 1 (base + a + b + d + n)

recompute :: World -> World
recompute w = w { wResult = result }
  where
    se = makeSE (wBox w) (wR w)
    f = wGray w
    result = case wOp w of
      1 -> f
      2 -> grayDilate se f
      3 -> grayErode se f
      4 -> grayDilate se (grayErode se f)
      5 -> grayErode se (grayDilate se f)
      6 -> U.zipWith (-) f (grayDilate se (grayErode se f))
      _ -> U.zipWith (-) (grayErode se (grayDilate se f)) f

drawPanel :: Env -> Gray -> Float -> String -> String -> Picture
drawPanel env a px title sub = Pictures
  [ imagePic env (cellOf env) px oy (\x y -> let v = clampF 0 1 (a U.! idx x y) in (v, v, v))
  , txt env px (oy - 30) 17 (lum 100) title
  , txt env px (oy - 10) 14 (lum 60) sub
  ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ drawPanel env (wGray w) 20 "Original f" "Brightness 0 to 1"
  , drawPanel env result (20 + pw + gap) (opName op ++ ": " ++ formula op)
      ("SE = " ++ (if wBox w then "Square " else "Disk ") ++ show n ++ "x" ++ show n ++ " (Left/Right: size, K: shape)")
  -- 断面：マウスの行の明るさを折れ線に
  , Color (hsba 45 90 100 0.7) (Pictures
      [ Line [pt env 20 rowY, pt env (20 + pw) rowY]
      , Line [pt env (20 + pw + gap) rowY, pt env (20 + 2 * pw + gap) rowY] ])
  ]
  ++ (if gh > 30
        then [ box env (hsb 170 30 14) 20 gy gw gh
             , Color (lum 100) (thickPath 1.5 (profile (wGray w)))
             , Color (hsb 45 90 100) (thickPath 2.5 (profile result))
             , txt env 28 (gy + 18) 14 (lum 100) ("- Original f (row " ++ show my ++ ")")
             , txt env 230 (gy + 18) 14 (hsb 45 90 100) ("- " ++ opName op) ]
        else [])
  ++ [ txt env 20 40 16 (lum 100)
         "1-7: op (orig, dilate, erode, open, close, top-hat, black-hat)   Left/Right: SE   K: shape   N: new image   Mouse Y: row"
     , txt env 20 68 14 (lum 60) (hint op)
     ]
  where
    c = cellOf env
    op = wOp w
    result = wResult w
    n = 2 * wR w + 1
    pw = fi cols * c
    gap = 30
    my = max 0 (min (rows - 1) (floor ((snd (mouseXY env) - oy) / c)))
    rowY = oy + fi my * c + c / 2
    gy = oy + fi rows * c + 50
    gh = envH env - gy - 40
    gw = 2 * pw + gap
    profile a =
      [ pt env (20 + (fi x / fi (cols - 1)) * gw) (gy + gh - clampF 0 1 (a U.! idx x my) * (gh - 6) - 3)
      | x <- [0 .. cols - 1] ]

opName, formula, hint :: Int -> String
opName k = case k of
  1 -> "Original"
  2 -> "Dilation (max)"
  3 -> "Erosion (min)"
  4 -> "Opening"
  5 -> "Closing"
  6 -> "Top-hat"
  _ -> "Black-hat"
formula k = case k of
  1 -> "f"
  2 -> "f (+) B = max"
  3 -> "f (-) B = min"
  4 -> "(f (-) B) (+) B"
  5 -> "(f (+) B) (-) B"
  6 -> "f - (f o B)"
  _ -> "(f . B) - f"
hint k = case k of
  1 -> "Slow shading + bright disks + dark square + salt-and-pepper. Pick an operation: what survives?"
  2 -> "Max filter: bright areas spread. Dark dots vanish, bright dots grow."
  3 -> "Min filter: dark areas spread. Bright dots vanish, dark dots grow."
  4 -> "Small bright things (white dots) vanish; the large bright disks remain."
  5 -> "Small dark things (black dots) fill in; the large dark square remains."
  6 -> "Removes the slow background shading and keeps only small bright things (uneven lighting fix)."
  _ -> "Keeps only small dark things (finding text, cracks, blood vessels)."

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wR = max 1 (wR w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wR = min 6 (wR w + 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    'k' -> recompute w { wBox = not (wBox w) }
    'n' -> recompute (makeImage w { wSeed = wSeed w + 1 })
    d | d >= '1' && d <= '7' -> recompute w { wOp = fromEnum d - fromEnum '0' }
    _   -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. トップハット（6）で SE を大きくしていく。明るい丸が「背景」から「対象」に変わるのはいつ？
-- 2. 断面の折れ線を見ながら 2 と 3 を切り替える。山が太る・谷が太る、を確認
-- 3. makeImage に自分の模様を足す：細い暗い線を何本か引いて、7 で取り出す

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、グレースケールの膨張と収縮）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 70) / 2 / fi cols) ((envH env - 310) / fi rows)

idx :: Int -> Int -> Int
idx x y = x + y * cols

distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- 構造要素：True なら正方形、False なら円
makeSE :: Bool -> Int -> [(Int, Int)]
makeSE isBox r =
  [ (dx, dy) | dy <- [-r .. r], dx <- [-r .. r]
  , isBox || fi (dx * dx + dy * dy) <= fi (r * r) + fi r * 0.5 ]

-- グレースケールの膨張（max）／収縮（min）。画像の外側は端の値とみなす
grayDilate, grayErode :: [(Int, Int)] -> Gray -> Gray
grayDilate se a = U.generate (U.length a) (\i -> foldr (max . sampleAt a i) (-1) se)
grayErode se a = U.generate (U.length a) (\i -> foldr (min . sampleAt a i) 2 se)

sampleAt :: Gray -> Int -> (Int, Int) -> Float
sampleAt a i (dx, dy) =
  let nx = max 0 (min (cols - 1) (i `rem` cols + dx))
      ny = max 0 (min (rows - 1) (i `quot` cols + dy))
  in a U.! (nx + ny * cols)

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 塗りつぶした長方形（左上が (x, y)。p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

fi :: Int -> Float
fi = fromIntegral

-- マス目全体を 1 枚の画像にする。f x y がマスの色。1 マスずつ長方形を描くより速い
imagePic :: Env -> Float -> Float -> Float -> (Int -> Int -> (Float, Float, Float)) -> Picture
imagePic env c x0 y0 f =
  at (pt env (x0 + fi cols * c / 2) (y0 + fi rows * c / 2)) (pixelImageF cols rows c f)
