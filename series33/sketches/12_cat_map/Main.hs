-- ============================================
-- 12 力学系と貴金属比 ─ 猫写像とフィボナッチ行列
-- 画素 (x, y) を (n·x + y, x) mod P に動かす。行列 [[n, 1], [1, 0]] の固有値は φn と −1/φn
--   φn 方向に引きのばされ、−1/φn 方向に縮む。ぐちゃぐちゃになるのに、何回かで元の絵に戻る！
-- スペース = 1 回 / A = 自動 / ←→ = n / ↑↓ = 絵の大きさ P / R = 元の絵に戻す / D = 絵をかえる
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)

data World = World
  { wN      :: !Int     -- ★ 行列 [[n, 1], [1, 0]]
  , wSIdx   :: !Int     -- ★ P = sizes の wSIdx 番目
  , wP      :: !Int
  , wPic    :: !(U.Vector Word32)   -- 絵（P×P、0xRRGGBB）
  , wOrig   :: !(U.Vector Word32)   -- 元の絵
  , wImage  :: !Picture             -- 絵が変わったときだけ作り直す
  , wIter   :: !Int
  , wPeriod :: !Int
  , wAuto   :: !Bool
  , wDesign :: !Int
  , wTick   :: !Int
  , wHist   :: ![Float]             -- 元の絵との一致率（新しい順、400 個まで）
  }

sizes :: [Int]
sizes = [60, 89, 101, 128, 150, 199, 256]

initialWorld :: World
initialWorld = reset (World 1 3 128 U.empty U.empty Blank 0 0 False 0 0 [1])

main :: IO ()
main = runSketch (sketch "33-12 Cat Map" initialWorld)
  { skBack  = hsb 230 25 9
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 最初の絵に戻す
reset :: World -> World
reset w =
  let p = case drop (wSIdx w) sizes of
            (s : _) -> s
            [] -> 128
      pic = drawPicture (wDesign w) p
  in w { wP = p, wPic = pic, wOrig = pic, wImage = toImage p pic, wIter = 0
       , wHist = [1], wPeriod = matrixPeriod (wN w) p }

-- 元の絵：顔・しま・文字（1 画素ずつ色を決める）
drawPicture :: Int -> Int -> U.Vector Word32
drawPicture design p = U.generate (p * p) $ \i ->
  let (y, x) = i `divMod` p
  in pixel design (fromIntegral p) (fromIntegral x + 0.5) (fromIntegral y + 0.5)

pixel :: Int -> Float -> Float -> Float -> Word32
pixel design p x y = case design of
  0 | onSmile -> dark
    | inCircle (p * 0.36) (p * 0.42) (p * 0.06) || inCircle (p * 0.64) (p * 0.42) (p * 0.06) -> dark
    | inCircle (p * 0.5) (p * 0.5) (p * 0.4) -> pack (hsb 45 80 100)
    | otherwise -> back
  1 -> pack (hsb (fromIntegral (floor (x * 8 / p) :: Int) * 45) 70 95)
  _ | onPhi -> pack white
    | otherwise -> back
  where
    back = pack (hsb 220 30 15)
    dark = pack (hsb 0 0 10)
    inCircle cx cy r = (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
    -- 笑った口（楕円の下半分）
    onSmile =
      let a = p * 0.2
          b = p * 0.15
          u = (x - p * 0.5) / a
          v = (y - p * 0.55) / b
          th = atan2 v u
          ex = a * cos th - (x - p * 0.5)
          ey = b * sin th - (y - p * 0.55)
      in th >= 0.2 && th <= pi - 0.2 && ex * ex + ey * ey <= (p * 0.02) * (p * 0.02)
    -- 文字 φ（楕円の輪 + 縦棒）
    onPhi =
      let u = (x - p * 0.5) / (p * 0.16)
          v = (y - p * 0.5) / (p * 0.13)
          r = sqrt (u * u + v * v)
          bar = abs (x - p * 0.5) <= p * 0.022 && y >= p * 0.27 && y <= p * 0.73
      in bar || (r >= 0.75 && r <= 1.05)

pack :: Color -> Word32
pack c =
  let (r, g, b, _) = rgbaOfColor c
      q v = fromIntegral (round (v * 255) :: Int) :: Word32
  in (q r `shiftL` 16) .|. (q g `shiftL` 8) .|. q b

-- 画面に置く画像（倍率は描くときに決める）
toImage :: Int -> U.Vector Word32 -> Picture
toImage p v = pixelImage p p 1 $ \x y ->
  let c = v U.! (y * p + x)
  in (fromIntegral (c `shiftR` 16), fromIntegral ((c `shiftR` 8) .&. 255), fromIntegral (c .&. 255))

-- 1 回の写像：(x, y) → (n·x + y, x) mod P
stepMap :: World -> World
stepMap w =
  let p = wP w
      n = wN w
      src = wPic w
      -- 移動先 (nx, ny) の色 = 移動元 (ny, nx − n·ny) の色
      dst = U.generate (p * p) $ \j ->
        let (ny, nx) = j `divMod` p
            x = ny
            y = (nx - n * x) `mod` p
        in src U.! (y * p + x)
      -- 元の絵とどれくらい同じか（赤と緑を比べる）
      mask = 0xFFFF00 :: Word32
      same = U.length (U.filter id (U.zipWith (\a b -> a .&. mask == b .&. mask) dst (wOrig w)))
  in w { wPic = dst, wImage = toImage p dst, wIter = wIter w + 1
       , wHist = take 400 (fromIntegral same / fromIntegral (p * p) : wHist w) }

stepWorld :: World -> World
stepWorld w
  | wAuto w =
      let t = wTick w + 1
          w1 = w { wTick = t }
      in if t `mod` 6 == 0 then stepMap w1 else w1
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([image, frame'] ++ hud ++ [diagram] ++ notes ++ graph)
  where
    n = wN w
    fn = fromIntegral n :: Float
    p = wP w
    phi = metallic fn
    side = min (envH env - 170) (envW env * 0.48)
    x0 = 40
    y0 = 110
    image = at (pt env (x0 + side / 2) (y0 + side / 2))
              (Scale (side / fromIntegral p) (side / fromIntegral p) (wImage w))
    frame' = Color (withA 0.4 white) (Line (closed x0 y0 side side))
    closed x y a b = map (uncurry (pt env)) [(x, y), (x + a, y), (x + a, y + b), (x, y + b), (x, y)]
    hud =
      [ txt env 20 24 15 white ("Matrix [[" ++ show n ++ ", 1], [1, 0]] (L/R)   P = " ++ show p ++ " (Up/Dn)   step " ++ show (wIter w) ++ "   period " ++ show (wPeriod w))
      , txt env 20 46 14 (gray 0.75) ("Space one step   A auto " ++ onOff (wAuto w) ++ "   R reset   D change picture")
      , txt env 20 68 14 (gray 0.75) "Multiply and mod: looks random (chaos), yet finite pixels must return to the start."
      ]
    -- 右：単位正方形がどう引きのばされるか
    px = x0 + side + 60
    py = y0 + 20
    u = min 70 ((envW env - px - 40) / (fn + 3))
    -- 原点 (px + u, py + 2.2u)、y は上向きで描く
    o x y = pt env (px + u + x * u) (py + u * 2.2 - y * u)
    len = 2.5 / sqrt (phi * phi + 1)
    diagram = Pictures
      [ Color (withA 0.25 white) (Pictures [Line [o (-1) 0, o (fn + 2) 0], Line [o 0 (-1), o 0 2]])
      , Color (hsba 200 50 90 0.3) (Polygon [o 0 0, o 1 0, o 1 1, o 0 1])
      , Color (hsb 200 60 100) (Line [o 0 0, o 1 0, o 1 1, o 0 1, o 0 0])
      -- 移った平行四辺形：(1,0)→(n,1), (0,1)→(1,0)
      , Color (hsba 45 70 100 0.3) (Polygon [o 0 0, o fn 1, o (fn + 1) 1, o 1 0])
      , Color (hsb 45 80 100) (Line [o 0 0, o fn 1, o (fn + 1) 1, o 1 0, o 0 0])
      -- 固有ベクトル (φ, 1) 方向
      , Color (hsb 330 80 100) (thickLine 2 (o (-phi * len * 0.3) (-len * 0.3)) (o (phi * len) len))
      ]
    ty = py + u * 3.6
    notes =
      [ txt env px ty 14 (gray 0.8) "Blue: unit square. Yellow: its image (area 1)."
      , txt env px (ty + 22) 14 (hsb 330 70 100) ("Pink: eigenvector (phi, 1), grows x" ++ nf 4 phi)
      , txt env px (ty + 44) 14 (gray 0.8) ("Across it: shrinks x" ++ nf 4 (1 / phi) ++ " and flips")
      , txt env px (ty + 66) 14 (gray 0.8) "Its powers contain the numbers of sketch 03."
      , txt env px (ty + 88) 14 (gray 0.8) "n=1 squared, [[2,1],[1,1]], is Arnold's cat map."
      ]
    -- 一致率のグラフ
    gx = px
    gy = ty + 120
    gw = envW env - px - 40
    gh = min 150 (envH env - gy - 40)
    graph
      | gh <= 40 = []
      | otherwise =
          [ Color (withA 0.2 white) (Line (closed gx gy gw gh))
          , Color (hsb 120 70 100) (Line
              [ pt env (gx + fromIntegral i * gw / 400) (gy + gh - v * gh)
              | (i, v) <- zip [0 :: Int ..] (reverse (wHist w)) ])
          , txt env (gx + 6) (gy + 18) 14 (gray 0.7) "Pixels equal to the start (wait for 100%)"
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> stepMap w
  EventKey (SpecialKey KeyRight) Down _ _ -> reset w { wN = min (wN w + 1) 5 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> reset w { wN = max (wN w - 1) 1 }
  EventKey (SpecialKey KeyUp) Down _ _    -> reset w { wSIdx = min (wSIdx w + 1) (length sizes - 1) }
  EventKey (SpecialKey KeyDown) Down _ _  -> reset w { wSIdx = max (wSIdx w - 1) 0 }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'r' || c == 'R' -> reset w
    | c == 'd' || c == 'D' -> reset w { wDesign = (wDesign w + 1) `mod` 3 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. P = 101 と P = 128 で周期をくらべよう。P を少し変えるだけで周期は大きく変わる
-- 2. 周期のちょうど半分のとき、絵はどうなっている？（さかさまになることがある）
-- 3. 行列を [[2, 1], [1, 1]]（猫写像）に変えるには stepMap の式をどう書く？
-- 4. 自分の名前を pixel 関数に描いて、ぐちゃぐちゃ → 元どおりを見せよう

-- ---------- 道具 ----------
metallic :: Float -> Float
metallic k = (k + sqrt (k * k + 4)) / 2

-- 行列 [[n,1],[1,0]] を mod P で何回かけると単位行列にもどるか
matrixPeriod :: Int -> Int -> Int
matrixPeriod nn m = go 1 1 0 0 1
  where
    go :: Int -> Int -> Int -> Int -> Int -> Int
    go k a b c d
      | k > 6 * m = -1
      | na == 1 `mod` m && nb == 0 && nc == 0 && nd == 1 `mod` m = k
      | otherwise = go (k + 1) na nb nc nd
      where
        na = (nn * a + c) `mod` m
        nb = (nn * b + d) `mod` m
        nc = a `mod` m
        nd = b `mod` m

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str
