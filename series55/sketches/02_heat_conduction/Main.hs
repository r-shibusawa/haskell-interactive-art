-- ============================================
-- 55-02 熱伝導 ─ 2 次元の板の温度を、拡散方程式 ∂T/∂t = ∇·(k∇T) で計算する
-- マス目ごとの温度を、となりとの温度差 × 熱伝導率 k だけ毎回やり取りする（陽解法の差分）
-- 熱の流れ q = −k·(温度の傾き)（フーリエの法則）。周りは断熱なので、熱の合計は変わらずに一様になっていく
-- 安定の条件：r = k·Δt/Δx² ≤ 1/4（2 次元）。ここでは r = 0.24 × k
-- 操作：ドラッグで塗る（1 加熱　2 冷却　3 銅　4 鉄　5 断熱材）/ F 両はしを 100 °C と 0 °C に固定 / ↑↓ 速さ / スペース 停止 / C 初期化
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nx, ny :: Int
nx = 160                   -- ★ マス目の数
ny = 100

rMax :: Double
rMax = 0.24                -- ★ r = k·Δt/Δx²（1/4 をこえると不安定！ 0.3 にしてみよう）

-- ★ 熱伝導率（銅 = 1 とした比。本物は銅 400、鉄 80 W/(m·K)）
matK :: Int -> Double
matK b = case b of
  3 -> 1      -- 銅
  4 -> 0.2    -- 鉄
  _ -> 0      -- 断熱材

brushNames :: [String]
brushNames = ["", "heat", "cool", "copper", "iron", "insulator"]

sx, sy :: Float
sx = 20                    -- 板の左上
sy = 100

data World = World
  { wT       :: !(U.Vector Double)   -- 温度 [°C]
  , wK       :: !(U.Vector Double)   -- 熱伝導率
  , wFaces   :: !Faces               -- 面ごとの熱伝導率（k が変わったら計算し直す）
  , wBrush   :: !Int
  , wIters   :: !Int                 -- 1 コマのステップ数
  , wPaused  :: !Bool
  , wFourier :: !Bool                -- 両はしを固定するか
  , wLog     :: [Double]             -- 平均の温度（新しい順、200 個まで）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-02 Heat conduction" world0)
  { skBack  = rgbC 20 20 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = initPlate (World U.empty U.empty (faces U.empty 0 0) 1 20 False False [] [])

-- 1 マスの大きさ [px]
cellOf :: Env -> Float
cellOf env = fromIntegral (max 2 (floor (min (envW env * 0.62 / fromIntegral nx) ((envH env - 170) / fromIntegral ny)) :: Int))

-- はじめの板：左が熱い・右が冷たい、銅の棒と、すき間のある断熱の壁
initPlate :: World -> World
initPlate w = w { wT = t, wK = k, wFaces = faces k nx ny, wLog = [] }
  where
    t = U.generate (nx * ny) (\i -> if i `mod` nx < nx `div` 2 then 80 else 20)
    k = U.generate (nx * ny) $ \i ->
      let x = i `mod` nx; y = i `div` nx
      in if x == 110 && (y < 45 || y > 75) then 0
         else if y > 20 && y < 30 && x > 20 && x < 140 then 1 else 0.2

-- 面ごとの熱伝導率は両側の調和平均（どちらかが 0 なら流れない）
faceK :: Double -> Double -> Double
faceK a b = if a + b > 0 then (2 * a * b) / (a + b) else 0

-- 各マスの 左・右・上・下 の面の熱伝導率
data Faces = Faces !(U.Vector Double) !(U.Vector Double) !(U.Vector Double) !(U.Vector Double)

faces :: U.Vector Double -> Int -> Int -> Faces
faces k w h = Faces (f (\x _ -> x > 0) (-1)) (f (\x _ -> x < w - 1) 1) (f (\_ y -> y > 0) (-w)) (f (\_ y -> y < h - 1) w)
  where f ok o = U.generate (w * h) (\i -> if ok (i `mod` w) (i `div` w) then faceK (k U.! i) (k U.! (i + o)) else 0)

-- 熱の 1 ステップ（fix が NaN でないマスはその温度に固定）
heatStepF :: Faces -> Int -> Int -> Double -> Maybe (U.Vector Double) -> U.Vector Double -> U.Vector Double
heatStepF (Faces kw ke kn ks) w h r fix t = U.generate (w * h) px
  where
    ix = U.unsafeIndex
    px i =
      let x = i `rem` w
          y = i `quot` w
          ti = ix t i
          d1 = if x > 0 then ix kw i * (ix t (i - 1) - ti) else 0
          d2 = if x < w - 1 then d1 + ix ke i * (ix t (i + 1) - ti) else d1
          d3 = if y > 0 then d2 + ix kn i * (ix t (i - w) - ti) else d2
          d4 = if y < h - 1 then d3 + ix ks i * (ix t (i + w) - ti) else d3
      in case fix of
           Just fv | not (isNaN (ix fv i)) -> ix fv i
           _ -> ti + r * d4

heatStep :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double -> Maybe (U.Vector Double) -> U.Vector Double
heatStep t k w h r fix = heatStepF (faces k w h) w h r fix t

totalHeat :: U.Vector Double -> Double
totalHeat = U.foldl' (+) 0

-- F：左のはし 100 °C、右のはし 0 °C
fixFourier :: U.Vector Double
fixFourier = U.generate (nx * ny) (\i -> let x = i `mod` nx in if x == 0 then 100 else if x == nx - 1 then 0 else 0 / 0)

-- マウスの下のマス
mouseCell :: Env -> (Int, Int)
mouseCell env = (floor ((mx - sx) / c), floor ((my - sy) / c))
  where (x, y) = envMouse env
        mx = x + envW env / 2
        my = envH env / 2 - y
        c = cellOf env

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w { wT = t', wLog = lg }
  where
    (mx, my) = mouseCell env
    inside = mx >= 0 && my >= 0 && mx < nx && my < ny
    w = if envDown env && inside then paint mx my w0 else w0
    fix = if wFourier w then Just fixFourier else Nothing
    t' | wPaused w = wT w
       | otherwise = foldl' (\t _ -> heatStepF (wFaces w) nx ny rMax fix t) (wT w) [1 .. wIters w]
    lg | envFrame env `mod` 4 == 0 = take 200 (totalHeat t' / fromIntegral (nx * ny) : wLog w)
       | otherwise = wLog w

-- 半径 3 マスの円を塗る（温度か材質）
paint :: Int -> Int -> World -> World
paint mx my w
  | b <= 2 = w { wT = wT w U.// [ (i, if b == 1 then 100 else 0) | i <- cells ] }
  | otherwise = let k = wK w U.// [ (i, matK b) | i <- cells ] in w { wK = k, wFaces = faces k nx ny }
  where
    b = wBrush w
    cells = [ x + nx * y | y <- [my - 3 .. my + 3], x <- [mx - 3 .. mx + 3]
            , x >= 0, y >= 0, x < nx, y < ny, (x - mx) ^ (2 :: Int) + (y - my) ^ (2 :: Int) <= 9 ]

-- 温度 → 色（0 °C 氷の青 → 50 °C 暗い赤 → 100 °C 白っぽい橙）
heatColor :: Double -> (Double, Double, Double)
heatColor t
  | u < 0.5 = let s = u / 0.5 in (40 + 120 * s, 120 - 90 * s, 230 - 190 * s)
  | otherwise = let s = (u - 0.5) / 0.5 in (160 + 95 * s, 30 + 200 * s, 40 + 150 * s)
  where u = clampv 0 1 (t / 100)

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (sx + pw / 2, sy + ph / 2) (Scale 1 (-1) (pixelImage nx ny cell px)) ]
  ++ [ Color white (Line [(sx, sy + (fromIntegral my + 0.5) * cell), (sx + pw, sy + (fromIntegral my + 0.5) * cell)]) | inside ]
  -- ---- グラフ：マウスの行の温度の分布、熱の合計
  ++ [ plotAxes g1 "position x (mouse row)" "temperature [C]" [0, 80, 159] [0, 50, 100]
     , Color (rgbC 255 170 90) (polyline 2 [ (gX g1 (fromIntegral x), gY g1 (t U.! (x + nx * row))) | x <- [0 .. nx - 1] ])
     , plotAxes g2 "time ->" "mean temperature (= total heat / cells)" [] [0, 50, 100]
     , Color (rgbC 130 200 255) (polyline 2 [ (gX g2 (fromIntegral k), gY g2 v) | (k, v) <- zip [0 :: Int ..] (reverse (wLog w)) ]) ]
  ++ [ say ("Heat conduction: dT/dt = div(k grad T)   r = k dt/dx^2 = " ++ show rMax ++ " (stable if <= 1/4)   " ++ show (wIters w) ++ " steps/frame") 20 36 18 (grayC 240)
     , say ("Drag paints: 1 heat  2 cool  3 copper k=1  4 iron k=0.2  5 insulator k=0   brush: " ++ getAt "" brushNames (wBrush w)) 20 62 15 (grayC 175)
     , say ("F fixed ends 100 C / 0 C: " ++ (if wFourier w then "ON" else "OFF") ++ "   Up/Down speed   Space pause   C reset") 20 86 15 (grayC 175)
     , say msg sx (sy + ph + 30) 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    t = wT w
    k = wK w
    cell = cellOf env
    pw = fromIntegral nx * cell
    ph = fromIntegral ny * cell
    (_, my) = mouseCell env
    (mxc, _) = mouseCell env
    inside = mxc >= 0 && my >= 0 && mxc < nx && my < ny
    row = max 0 (min (ny - 1) my)
    px x y = let i = x + nx * y
                 (r, g, b) = heatColor (t U.! i)
                 kk = k U.! i
             in if kk == 0 then (90, 90, 96)
                else if kk >= 1 then (toByte (min 255 (r + 25)), toByte (g + 10), toByte b)
                else (toByte r, toByte g, toByte b)
    gx = sx + pw + 70
    gw = envW env - gx - 30
    g1 = graph gx (sy + 10) gw (ph / 2 - 50) 0 (fromIntegral (nx - 1)) 0 100
    g2 = graph gx (pY g1 + pH g1 + 80) gw (pH g1) 0 200 0 100
    msg | wFourier w =
            let q = foldl' (\s y -> let i = nx `div` 2 + nx * y in s + (-(faceK (k U.! i) (k U.! (i - 1))) * (t U.! i - t U.! (i - 1)))) 0 [0 .. ny - 1]
            in "F: heat flow through the middle, sum of q = -k dT/dx = " ++ fmt 2 q ++ " (steady: same at every cross-section)"
        | otherwise = "Mean temperature " ++ fmt 3 (totalHeat t / fromIntegral (nx * ny)) ++ " C (insulated box: constant unless you paint)"

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12345" -> w { wBrush = fromEnum c - fromEnum '0' }
    | c `elem` "fF" -> w { wFourier = not (wFourier w) }
    | c `elem` "cC" -> initPlate w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wIters = min 80 (wIters w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wIters = max 1 (wIters w `div` 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("stability: r = " ++ show rMax ++ " <= 1/4 (2D explicit scheme)", rMax <= 0.25)
  , ("insulated edges: total heat unchanged (random materials, 300 steps, relative 1e-12)", abs (totalHeat t300 / h0 - 1) < 1e-12)
  , ("checkerboard: r = 0.24 decays (max " ++ fmt 3 ma ++ "), r = 0.3 explodes (" ++ fmt 0 mb ++ ")", ma < 1 && mb > 100)
  , ("bar with ends 100 C and 0 C: steady state is a straight line (Fourier's law, 1e-3)"
    , and [ near v (100 - 100 * fromIntegral x / fromIntegral (len - 1)) 1e-3 | (x, v) <- zip [0 :: Int ..] (U.toList bar) ])
  , ("copper + iron bar: same flow through every face, q = dT / sum(1/k) = " ++ fmt 3 (100 / rsum)
    , all (\q -> near q (100 / rsum) 1e-4) flux) ]
  where
    n = 30
    -- 温度と材質をばらばらに（rnd の順番も p5.js 版と同じ）
    gen (acc, r) _ = let (a, r1) = rnd r
                         (b, r2) = rnd r1
                     in if b < 0.2 then ((100 * a, 0) : acc, r2)
                        else let (c, r3) = rnd r2 in ((100 * a, 0.2 + 0.8 * c) : acc, r3)
    (tk, _) = foldl' gen ([], seedRng 2) [1 .. n * n]
    t0 = U.fromList (map fst (reverse tk))
    k0 = U.fromList (map snd (reverse tk))
    h0 = totalHeat t0
    fk = faces k0 n n
    t300 = foldl' (\t _ -> heatStepF fk n n rMax Nothing t) t0 [1 .. 300 :: Int]
    chk = U.generate (n * n) (\i -> if odd (i `mod` n + i `div` n) then 1 else -1)
    ones = faces (U.replicate (n * n) 1) n n
    a20 = foldl' (\t _ -> heatStepF ones n n 0.24 Nothing t) chk [1 .. 20 :: Int]
    b20 = foldl' (\t _ -> heatStepF ones n n 0.3 Nothing t) chk [1 .. 20 :: Int]
    ma = U.maximum a20
    mb = U.maximum b20
    len = 20
    fx = Just (U.generate len (\x -> if x == 0 then 100 else if x == len - 1 then 0 else 0 / 0))
    kb = faces (U.replicate len 1) len 1
    bar = foldl' (\t _ -> heatStepF kb len 1 0.24 fx t) (U.replicate len 0) [1 .. 6000 :: Int]
    ksv = U.generate len (\x -> if x < len `div` 2 then 1 else 0.25)
    fks = faces ksv len 1
    bs = foldl' (\t _ -> heatStepF fks len 1 0.24 fx t) (U.replicate len 0) [1 .. 20000 :: Int]
    kfs = [ faceK (ksv U.! x) (ksv U.! (x - 1)) | x <- [1 .. len - 1] ]
    flux = [ -kf * (bs U.! x - bs U.! (x - 1)) | (x, kf) <- zip [1 ..] kfs ]
    rsum = foldl' (\s kf -> s + 1 / kf) 0 kfs

-- ---------- やってみよう ----------
-- 1. 銅の棒と鉄の板で、熱が伝わる速さを比べよう。断熱の壁のすき間から熱がもれていく様子も
-- 2. 3（銅）で板の上に線を描き、1 で片方のはしを温める。熱の「道」ができる？
-- 3. F で両はしを固定すると、やがて温度は直線（一様な板）になる。q はどの断面でも同じ？
-- 4. rMax を 0.3 にしてみよう（1/4 をこえる）。何が起きる？ なぜ？
-- 5. matK の k を変えて、自分の材質（ガラス・木など）を作ろう

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
