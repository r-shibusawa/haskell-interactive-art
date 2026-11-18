-- ============================================
-- 53-13 宇宙の膨張 ─ どの銀河から見ても、遠い銀河ほど速く遠ざかる
-- ぶどうパン模型：パン（空間）がふくらむと、すべての間の距離が同じ割合 a(t) で伸びる → 速さ v = (ȧ/a) × 距離
-- ハッブルの法則 v = H₀ d。銀河の距離と後退速度のグラフに、原点を通る直線を最小二乗で当てはめる：H₀ = Σvd / Σd²
-- 宇宙の年齢の目安 = 1/H₀（ずっと同じ速さで広がったとしたら、何年前に全部が 1 点にいたか）
-- 操作：クリック = 自分の銀河を選ぶ / ↑↓ = 銀河の固有の動き（ばらつき）/ ←→ = 本当の H₀ / R = 新しい銀河 / スペース = 再生/停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cKms :: Double
cKms = 299792.458                      -- 光の速さ [km/s]

gyrPerInvH :: Double
gyrPerInvH = 977.79                    -- 1/H₀ [10 億年] = 977.79 ÷ H₀ [km/s/Mpc]

data World = World
  { wH0true  :: !Double                -- ★ 本当の H₀ [km/s/Mpc]
  , wSigmaV  :: !Double                -- ★ ばらつき（銀河の固有の動き）[km/s]
  , wLattice :: [(Double, Double)]     -- ぶどうパンの中の銀河（格子 + 少しのずれ）
  , wMe      :: !Int                   -- 自分の銀河
  , wT       :: !Double                -- ぶどうパンの時刻 0〜1
  , wPlaying :: !Bool
  , wSample  :: [(Double, Double)]     -- (距離 d [Mpc], 後退速度 v [km/s])
  , wFit     :: !Double
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-13 Expansion of the universe" world0)
  { skBack  = rgb255 6 8 24
  , skStart = \_ -> do
      printSelfTest selfTest
      s <- freshSeed
      pure (newSample s world0) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newSample 1 (World 70 300 lat (length lat `div` 2) 0 True [] 0 [])
  where
    lat = fst (runRand (sequence [ do a <- rnd
                                      b <- rnd
                                      pure (fromIntegral i - 6 + 0.35 * (a - 0.5), fromIntegral j - 4 + 0.35 * (b - 0.5))
                                 | j <- [0 .. 8 :: Int], i <- [0 .. 12 :: Int] ]) (seedRng 11))

-- ぶどうパンの大きさ（1 → 1.6 をくり返す）
scaleA :: Double -> Double
scaleA t = 1 + 0.6 * t

-- 25 個の銀河：距離 5〜400 Mpc、速さ = H₀ d + ばらつき
makeSample :: Double -> Double -> Rand [(Double, Double)]
makeSample h0 sig = mapM (const one) [1 .. 25 :: Int]
  where one = do u <- rnd
                 let d = 5 + 395 * u
                 e <- rndNormal
                 pure (d, h0 * d + sig * e)

newSample :: Word32 -> World -> World
newSample seed w = w { wSample = s, wFit = fitThroughOrigin s }
  where s = fst (runRand (makeSample (wH0true w) (wSigmaV w)) (seedRng seed))

-- 原点を通る直線 v = H d の最小二乗：H = Σvd / Σd²
fitThroughOrigin :: [(Double, Double)] -> Double
fitThroughOrigin pts = if sxx > 0 then sxy / sxx else 0
  where (sxy, sxx) = foldl' (\(a, b) (x, y) -> (a + x * y, b + x * x)) (0, 0) pts

step :: World -> World
step w | wPlaying w = let t = wT w + 1 / 240 in w { wT = if t > 1 then 0 else t }
       | otherwise = w

-- ぶどうパンの置き場所と大きさ
breadGeom :: Env -> (Float, Float, Float)
breadGeom env = (envW env * 0.27, envH env * 0.55, min (envW env * 0.022) (envH env * 0.035))

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgba255 70 50 30 120) (rectTL (cx - f2 ((ox + 6.7) * a) * sc) (cy - f2 ((oy + 4.7) * a) * sc) (f2 (13.4 * a) * sc) (f2 (9.4 * a) * sc)) ]   -- パン
  ++ concat [ [ Color (rgba255 140 190 255 160) (Line [(cx + dx, cy + dy), (cx + dx + vx, cy + dy + vy)]) | k /= wMe w ]
              ++ [ Color (if k == wMe w then rgb255 255 130 110 else rgb255 255 230 170) (circleD (cx + dx) (cy + dy) (if k == wMe w then 12 else 7)) ]
            | (k, (px, py)) <- zip [0 ..] (wLattice w)
            , let dx = f2 ((px - ox) * a) * sc, let dy = f2 ((py - oy) * a) * sc
            , let vx = f2 ((px - ox) * adot) * sc * 0.9, let vy = f2 ((py - oy) * adot) * sc * 0.9 ]
  ++ [ sayC ("Raisin bread model: size a = " ++ fmt 2 a ++ " (red = you, click to change)") cx (cy + 4.6 * 1.6 * sc + 40) 14 c1
     , sayC "Arrows = speed seen from you (longer if farther)." cx (cy + 4.6 * 1.6 * sc + 58) 14 c1
     , sayC "Pick any galaxy: the pattern looks the same." cx (cy + 4.6 * 1.6 * sc + 76) 14 c1 ]
  -- ---- ハッブル図
  ++ [ plotAxes g "Distance d [Mpc]" "Recession velocity v [km/s]" [0, 100, 200, 300, 400] [0, 10000, 20000, 30000]
     , Color (rgba255 140 230 180 120) (Line [(gX g 0, gY g 0), (gX g 400, gY g (wH0true w * 400))])
     , Color (rgb255 250 225 140) (thickLine 2 (gX g 0, gY g 0) (gX g 400, gY g (wFit w * 400))) ]
  ++ [ Color (rgb255 255 240 210) (circleD (gX g d) (gY g v) 8) | (d, v) <- wSample w ]
  ++ [ say ("Fit: H0 = Sum(vd) / Sum(d^2) = " ++ fmt 2 fit ++ " km/s/Mpc") (gx g) y0 16 (rgb255 250 225 140)
     , say ("(true " ++ showJ (wH0true w) ++ ", scatter " ++ showJ (wSigmaV w) ++ " km/s)") (gx g) (y0 + 22) 14 c1
     , say ("Age estimate 1/H0 = 977.8 / H0 = " ++ fmt 2 (gyrPerInvH / fit) ++ " billion years") (gx g) (y0 + 48) 15 c2
     , say ("Galaxy at 100 Mpc: v = " ++ fmt 0 (fit * 100) ++ " km/s, redshift z ~ v/c = " ++ fmt 4 (fit * 100 / cKms)) (gx g) (y0 + 72) 15 c2
     , say "Green line = true H0, yellow line = H0 fitted to the data" (gx g) (y0 + 98) 14 c3
     , say "Today's measurements: about 67-73 (Hubble tension)" (gx g) (y0 + 116) 14 c3
     , say "Expansion | click your galaxy  Up/Down scatter  Left/Right true H0  R new galaxies  Space play" 20 34 18 (rgb255 240 236 215)
     , drawSelfTest env (wTest w) ]
  where
    (cx, cy, sc) = breadGeom env
    a = scaleA (wT w)
    adot = 0.6
    (ox, oy) = getAt (0, 0) (wLattice w) (wMe w)
    g = Graph (envW env * 0.55) 110 (envW env * 0.41) (envH env * 0.55) 0 400 0 32000 False False
    y0 = gy g + gh g + 56
    fit = wFit w
    c1 = rgb255 200 205 225
    c2 = rgb255 220 225 240
    c3 = rgb255 170 175 200

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (pick w)
  EventKey (SpecialKey KeyUp) Down _ _ -> fresh w { wSigmaV = min 3000 (wSigmaV w + 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> fresh w { wSigmaV = max 0 (wSigmaV w - 100) }
  EventKey (SpecialKey KeyRight) Down _ _ -> fresh w { wH0true = min 150 (wH0true w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> fresh w { wH0true = max 20 (wH0true w - 5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> fresh w
  _ -> pure w
  where
    fresh w1 = do s <- freshSeed
                  pure (newSample s w1)
    -- いちばん近い銀河（20 ピクセル以内）を自分にする
    pick w0 =
      let (cx, cy, sc) = breadGeom env
          (mx, my) = mouseP5 env
          a = scaleA (wT w0)
          (ox, oy) = getAt (0, 0) (wLattice w0) (wMe w0)
          ds = [ (k, sqrt ((cx + f2 ((px - ox) * a) * sc - mx) ^ (2 :: Int) + (cy + f2 ((py - oy) * a) * sc - my) ^ (2 :: Int)))
               | (k, (px, py)) <- zip [0 ..] (wLattice w0) ]
          (best, _) = foldl' (\(bk, bd) (k, d) -> if d < bd then (k, d) else (bk, bd)) (-1, 20) ds
      in if best >= 0 then w0 { wMe = best } else w0

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("data without scatter give exactly H0 = 70", near (fitThroughOrigin [ (d, 70 * d) | d <- [10, 50, 120, 300] ]) 70 1e-9)
  , ("H0 = 70 km/s/Mpc -> 1/H0 = 13.97 billion years", near (gyrPerInvH / 70) 13.97 0.01)
  , ("unit of 1/H0: 1 Mpc / (1 km/s) = 977.8 billion years", near ((3.0857e19 / 1) / 3.15576e16) 977.8 0.1)
  , ("raisin bread: v / distance = a'/a from every galaxy (the same value)", bread)
  , ("scatter 300 km/s, 25 galaxies: H0 still within 5%", near (fitThroughOrigin (fst (runRand (makeSample 70 300) (seedRng 5))) / 70) 1 0.05)
  , ("v = 7000 km/s at 100 Mpc -> z ~ 0.0233", near (70 * 100 / cKms) 0.02335 0.0001) ]
  where
    l = [(0, 0), (3, 1), (-2, 4), (5, -3)] :: [(Double, Double)]
    t = 0.4
    dt = 1e-6
    bread = and [ near (v / (d0 * scaleA t)) (0.6 / scaleA t) 1e-6
                | ia <- [0, 1], (k, (kx, ky)) <- zip [0 :: Int ..] l, k /= ia
                , let (ax, ay) = getAt (0, 0) l ia
                , let d0 = sqrt ((kx - ax) ^ (2 :: Int) + (ky - ay) ^ (2 :: Int))
                , let v = d0 * (scaleA (t + dt) - scaleA t) / dt ]

-- ---------- やってみよう ----------
-- 1. ぶどうパンで、いろいろな銀河をクリックして自分にしよう。矢印の形は変わる？
-- 2. ↑↓ でばらつきを 2000 km/s に。R を何回か押すと、H₀ の推定はどれくらいぶれる？
-- 3. 近い銀河（10 Mpc 以内）だけで当てはめると？ 固有の動きのせいで誤差が大きい
-- 4. ハッブルが 1929 年に出した H₀ は約 500 だった。そのときの宇宙の年齢の目安は？
-- 5. 1/H₀ は「ずっと同じ速さ」の目安。本当の年齢（約 138 億年）とのちがいを調べよう

-- ==================================================
-- ここから下は「天文の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 度 → ラジアン
degR :: Double
degR = pi / 180

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 0〜360 にそろえる（JavaScript の ((a % 360) + 360) % 360 と同じ）
wrap360 :: Double -> Double
wrap360 a = jsRem (jsRem a 360 + 360) 360

wrap180 :: Double -> Double
wrap180 a = wrap360 (a + 180) - 180

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 (fin 0 t)) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 (fin 0 t)) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgb255 80 90 120) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (rgb255 160 170 195) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 5) (gY g v + 6) 14 (rgb255 160 170 195) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 38) 14 (rgb255 205 210 225)
     , say yl (gx g) (gy g - 8) 14 (rgb255 205 210 225) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = show v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- ---- 種つきの乱数（同じ種なら毎回同じ列）。p5.js 版の rnd()（mulberry32）と同じ数が出る
newtype Rng = Rng Word32

-- 種 0 は 1 にする（p5.js 版の (s >>> 0) || 1 と同じ）
seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 乱数を「順番に」使う小さなしくみ：Rand a は「乱数の状態を受け取って、値と次の状態を返す」計算
newtype Rand a = Rand { runRand :: Rng -> (a, Rng) }

instance Functor Rand where
  fmap f (Rand g) = Rand (\r -> let (a, r') = g r in (f a, r'))
instance Applicative Rand where
  pure a = Rand (\r -> (a, r))
  Rand f <*> Rand g = Rand (\r -> let (h, r1) = f r; (a, r2) = g r1 in (h a, r2))
instance Monad Rand where
  Rand g >>= k = Rand (\r -> let (a, r1) = g r in runRand (k a) r1)

-- 0〜1 の一様乱数
rnd :: Rand Double
rnd = Rand $ \(Rng s0) ->
  let a = s0 + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法。u を先、v を後に引く）
rndNormal :: Rand Double
rndNormal = do
  u <- fmap (1 -) rnd
  v <- rnd
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Word32
freshSeed = randomRIO (1, maxBound)
