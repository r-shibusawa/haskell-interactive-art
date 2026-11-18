-- ============================================
-- 55-15 カメラ映像を熱源に 📷 ─ 明るい所が温め続け、熱伝導の方程式で広がる「サーモグラフィ風」の絵
-- 映像の明るさ B が しきい値より大きいマスは、温度 100·B に向かって温められる（熱源）
-- 熱はとなりのマスへ拡散（02 と同じ陽解法、r = 0.2 ≤ 1/4）し、まわりの空気へ少しずつ逃げる（ニュートンの冷却）
-- 操作：↑↓ 1 コマの計算回数 / ← → 熱源のしきい値 / 1 鉄の色　2 虹色 / C 熱を消す / V カメラ ⇔ テスト画像 / S 保存
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch :: Int
cw = 160                   -- カメラとマス目の大きさ
ch = 120

rDiff, heatRate, cool :: Double
rDiff = 0.2                -- ★ r = DΔt/Δx²（1/4 以下で安定）
heatRate = 0.08            -- ★ 熱源が温める速さ
cool = 0.002               -- ★ まわりへ逃げる速さ（ニュートンの冷却）

data World = World
  { wTemp   :: !(U.Vector Double)   -- 温度 0〜100
  , wBright :: !(U.Vector Double)   -- 映像の明るさ 0〜1
  , wIters  :: !Int
  , wThr    :: !Double              -- 熱源のしきい値
  , wCmap   :: !Int                 -- 1 鉄の色、2 虹色
  , wUseCam :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wFrame  :: !B.ByteString        -- カメラの映像（RGB）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-15 Camera image as a heat source" world0)
  { skBack  = rgbC 18 16 20
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (U.replicate (cw * ch) 0) (U.replicate (cw * ch) 0) 6 0.6 1 True Nothing False B.empty []

-- 熱の 1 ステップ：拡散（周りは断熱）＋ 熱源 ＋ 冷却
heatStep :: Int -> Int -> Double -> Maybe (U.Vector Double) -> Double -> Double -> Double -> U.Vector Double -> U.Vector Double
heatStep nx ny r src th rate cl t = U.generate (nx * ny) px
  where
    ix = U.unsafeIndex
    px i =
      let x = i `rem` nx
          y = i `quot` nx
          ti = ix t i
          d1 = if x > 0 then ix t (i - 1) - ti else 0
          d2 = if x < nx - 1 then d1 + (ix t (i + 1) - ti) else d1
          d3 = if y > 0 then d2 + (ix t (i - nx) - ti) else d2
          d4 = if y < ny - 1 then d3 + (ix t (i + nx) - ti) else d3
          v = ti + r * d4 - cl * ti
          v' = case src of
                 Just s | ix s i > th -> v + rate * (100 * ix s i - v)
                 _ -> v
      in clampv 0 100 v'

-- 温度 0〜100 → 色（1：鉄の色 黒→紫→赤→橙→黄→白、2：虹色 青→緑→赤）
heatColor :: Double -> Int -> (Double, Double, Double)
heatColor t m
  | m == 2 = (255 * clampv 0 1 (2 * u - 0.6), 255 * (1 - abs (2 * u - 1)), 255 * clampv 0 1 (1 - 2 * u + 0.2))
  | otherwise =
      let s = u * 5
          k = min 4 (floor s)
          f = s - fromIntegral k
          (a1, a2, a3) = getAt (0, 0, 0) stops k
          (b1, b2, b3) = getAt (0, 0, 0) stops (k + 1)
      in (a1 + (b1 - a1) * f, a2 + (b2 - a2) * f, a3 + (b3 - a3) * f)
  where u = clampv 0 1 (t / 100)
        stops = [(0, 0, 0), (70, 0, 110), (200, 30, 60), (255, 120, 0), (255, 220, 40), (255, 255, 255)]

-- テスト画像：暗い背景に、明るい円（ろうそく）と、明るさの横じま（灰色 0〜255）
testImage :: U.Vector Word8
testImage = U.generate (cw * ch) $ \i ->
  let (y, x) = i `divMod` cw
      fx = fromIntegral x :: Double
      fy = fromIntegral y
      v | y > 95 && y < 102 && x > 80 = 200
        | sqrt ((fx - 115) ^ (2 :: Int) + (fy - 40) ^ (2 :: Int)) < 7 = 230
        | sqrt ((fx - 45) ^ (2 :: Int) + (fy - 60) ^ (2 :: Int)) < 12 = 250
        | otherwise = 30 + 20 * sin (fx * 0.1)
  in toByte v

testBright :: U.Vector Double
testBright = U.map (\b -> let v = fromIntegral b in (0.299 * v + 0.587 * v + 0.114 * v) / 255) testImage

step :: Env -> Float -> World -> IO World
step _ _ w = do
  (live, fr) <- case wCam w of
    Just cam | wUseCam w -> do
      ok <- camLive cam
      if not ok then pure (False, B.empty) else do
        f <- cameraFrame cam
        pure (if B.length f >= cw * ch * 3 then (True, f) else (False, B.empty))
    _ -> pure (False, B.empty)
  let g k = fromIntegral (BU.unsafeIndex fr k) :: Double
      br | live = U.generate (cw * ch) (\i -> (0.299 * g (3 * i) + 0.587 * g (3 * i + 1) + 0.114 * g (3 * i + 2)) / 255)
         | otherwise = testBright
      t' = foldl' (\t _ -> heatStep cw ch rDiff (Just br) (wThr w) heatRate cool t) (wTemp w) [1 .. wIters w]
  pure w { wLive = live, wFrame = fr, wBright = br, wTemp = t' }

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (x0 + iw / 2, y0 + ih / 2) (Scale 1 (-1) (pixelImage cw ch sc (\x y -> let (r, g, b) = heatColor (t U.! (x + cw * y)) (wCmap w) in (toByte r, toByte g, toByte b))))
  -- 小さく元の映像
  , at (tx + tw / 2, y0 + th2 / 2) (Scale 1 (-1) srcImg)
  , say (if wLive w then "camera image" else "test image (V for camera)") tx (y0 + th2 + 18) 14 (grayC 170) ]
  -- 色の目盛り
  ++ [ Color (let (r, g, b) = heatColor (fromIntegral k) (wCmap w) in makeColor (q r) (q g) (q b) 1) (rectTL (tx + fromIntegral k / 100 * tw) (y0 + th2 + 40) (tw / 100 + 1) 16) | k <- [0 .. 99 :: Int] ]
  ++ [ say "0" tx (y0 + th2 + 74) 14 (grayC 200), say "100" (tx + tw - 30) (y0 + th2 + 74) 14 (grayC 200) ]
  ++ [ say s tx (y0 + th2 + 110 + 24 * fromIntegral k) 15 (grayC 235)
     | (k, s) <- zip [0 :: Int ..] [ "hottest " ++ fmt 1 mx, "mean " ++ fmt 2 mean, "source cells " ++ show ns ++ " (bright > " ++ fmt 2 (wThr w) ++ ")", show (wIters w) ++ " steps/frame" ] ]
  ++ [ say ("Camera image as a heat source: bright parts heat, the diffusion equation spreads it   r = " ++ show rDiff ++ " (<= 1/4)") 20 36 18 (grayC 240)
     , say "Up/Down steps   Left/Right threshold   1 iron colours   2 rainbow   C clear heat   V camera <-> test image   S save" 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    t = wTemp w
    sc = min (envW env * 0.7 / fromIntegral cw) ((envH env - 170) / fromIntegral ch)
    x0 = 20
    y0 = 100
    iw = fromIntegral cw * sc
    ih = fromIntegral ch * sc
    tx = x0 + iw + 30
    tw = envW env - tx - 30
    th2 = tw * fromIntegral ch / fromIntegral cw
    k2 = tw / fromIntegral cw
    srcImg = case wCam w of
      Just cam | wLive w -> camImage cam (wFrame w) k2
      _ -> pixelImage cw ch k2 (\x y -> let v = testImage U.! (x + cw * y) in (v, v, v))
    mx = U.foldl' max 0 t
    mean = U.foldl' (\s v -> s + v / fromIntegral (cw * ch)) 0 t
    ns = U.length (U.filter (> wThr w) (wBright w))
    q v = realToFrac (clampv 0 255 v / 255)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wIters = min 30 (wIters w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wIters = max 1 (wIters w - 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wThr = min 0.95 (wThr w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wThr = max 0.1 (wThr w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "12" -> pure w { wCmap = fromEnum c - fromEnum '0' }
    | c `elem` "cC" -> pure w { wTemp = U.replicate (cw * ch) 0 }
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "thermo_camera" env (draw env w) >> pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("stability: r = " ++ show rDiff ++ " <= 1/4", rDiff <= 0.25)
  , ("no source and no cooling: total heat constant (200 steps, relative 1e-12)", abs (U.foldl' (+) 0 a200 / h0 - 1) < 1e-12)
  , ("spread of one hot point: x variance = 2r x steps = " ++ prec3 (2 * rDiff * 50) ++ " (" ++ fmt 4 vx ++ ")", near vx (2 * rDiff * 50) 1e-6)
  , ("colours: 0 is black, 100 is white (iron colours)", c0 == (0, 0, 0) && c1 == (255, 255, 255))
  , ("one bright cell as a source: steady state hottest at the centre, within 0..100"
    , sMid == U.maximum s3000 && sMid <= 100 && s3000 U.! 0 > 0) ]
  where
    n = 40
    (rs, _) = rndList (n * n) (seedRng 15)
    a0 = U.fromList (map (50 *) rs)
    h0 = U.foldl' (+) 0 a0
    a200 = foldl' (\t _ -> heatStep n n rDiff Nothing 1 0 0 t) a0 [1 .. 200 :: Int]
    m = 81
    p0 = U.generate (m * m) (\i -> if i == 40 + m * 40 then 1 else 0)
    p50 = foldl' (\t _ -> heatStep m m rDiff Nothing 1 0 0 t) p0 [1 .. 50 :: Int]
    vx = foldl' (\s i -> s + p50 U.! i * fromIntegral ((i `mod` m) - 40) ^ (2 :: Int)) 0 [0 .. m * m - 1]
    c0 = heatColor 0 1
    c1 = heatColor 100 1
    src = U.generate 81 (\i -> if i == 40 then 1 else 0)
    s3000 = foldl' (\t _ -> heatStep 9 9 rDiff (Just src) 0.5 heatRate cool t) (U.replicate 81 0) [1 .. 3000 :: Int]
    sMid = s3000 U.! 40

-- ---------- やってみよう ----------
-- 1. 手やスマホの画面をカメラに向けよう。明るい所から熱が広がる
-- 2. ← → でしきい値を変えると、熱源になる所はどう変わる？
-- 3. 動くと、通ったあとに熱の「尾」が残る。cool を 0 にすると？ 0.02 にすると？
-- 4. rDiff を 0.3 にすると何が起きる？（1/4 をこえると不安定。02 と同じ）
-- 5. S で保存して、自分のサーモグラフィ風の写真を作ろう

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 0〜255 の小数を画素の値に（Uint8ClampedArray と同じ丸め）
toByte :: Double -> Word8
toByte v = fromIntegral (max 0 (min 255 (round (fin 0 v) :: Int)))

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を n 個まとめて
rndList :: Int -> Rng -> ([Double], Rng)
rndList n r0 = go n r0 []
  where go 0 r acc = (reverse acc, r)
        go k r acc = let (v, r') = rnd r in go (k - 1) r' (v : acc)


-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0'
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin 0 v0'

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
