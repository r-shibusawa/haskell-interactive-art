-- ============================================
-- 53-12 銀河の回転曲線 ─ 見えない質量（暗黒物質）があると考えるのはなぜ？
-- 半径 r の円軌道の速さ v = √(G M(<r) / r)（M(<r) = r より内側の質量。球対称の近似）
-- 見える物質（バルジ + 円盤）だけなら、外側ほど遅くなる（v ∝ 1/√r、ケプラー的な減少）
-- 実際の銀河の回転曲線は外側でも平ら → 見えない「ハロー」M_h(<r) = (v∞²/G)(r − rc·atan(r/rc)) を足すと説明できる
-- 操作：↑↓ = ハローの重さ（v∞）/ H = 星の動きにハローを入れる・入れない / スペース = 再生/停止 / 銀河の値は天の川銀河に似せた目安
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

gK :: Double
gK = 4.30091e-6                        -- 重力定数 [kpc (km/s)² / M☉]

mBulge0, aBulge, mDisk0, rDisk, rCore :: Double
mBulge0 = 1.5e10                       -- ★ バルジの質量 [M☉]
aBulge = 0.6                           -- ★ バルジの大きさ [kpc]
mDisk0 = 6e10                          -- ★ 円盤の質量 [M☉]
rDisk = 3                              -- ★ 円盤の広がり [kpc]
rCore = 5                              -- ★ ハローの芯の半径 [kpc]

kpcKm, myrS :: Double
kpcKm = 3.0857e16                      -- 1 kpc [km]
myrS = 3.15576e13                      -- 100 万年 [s]

data World = World
  { wVHalo   :: !Double                -- ★ ハローの v∞ [km/s]
  , wHaloOn  :: !Bool
  , wPlaying :: !Bool
  , wStars   :: !(U.Vector (Double, Double))   -- 円盤の星 (半径 r [kpc], 角度 θ)
  , wObs     :: [(Double, Double)]     -- 観測のような点 (r, v)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-12 Galaxy rotation curve" world0)
  { skBack  = rgb255 4 6 20
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 200 True True stars obs []
  where ((stars, obs), _) = runRand makeGalaxy (seedRng 3)

-- 円盤の星：指数関数の分布で半径を選ぶ（2 本の腕の模様をつける）。そのあと観測のような点（v∞ = 210 のモデル + ばらつき）
makeGalaxy :: Rand (U.Vector (Double, Double), [(Double, Double)])
makeGalaxy = do
  ss <- mapM (const star) [1 .. 1500 :: Int]
  os <- mapM (\r -> do e <- rndNormal
                       pure (r, vTotal r 210 + 6 * e)) [2, 4 .. 30]
  pure (U.fromList ss, os)
  where
    star = do
      u <- rnd
      let r = -rDisk * log (1 - u * 0.995) + 0.3
      ua <- rnd
      let arm = fromIntegral (floor (ua * 2) :: Int) * pi
      e <- rndNormal
      pure (min r 30, arm + log (r + 1) * 1.6 + 0.5 * e * 0.4)

-- ---- 内側の質量 [M☉] と速さ
mBulge, mDisk :: Double -> Double
mBulge r = mBulge0 * r * r / ((r + aBulge) * (r + aBulge))
mDisk r = let x = r / rDisk in mDisk0 * (1 - (1 + x) * exp (-x))

mHalo :: Double -> Double -> Double
mHalo r v = (v * v / gK) * (r - rCore * atan (r / rCore))

vOf :: Double -> Double -> Double
vOf m r = sqrt (gK * max 0 m / max r 1e-6)

vVisible :: Double -> Double
vVisible r = vOf (mBulge r + mDisk r) r

vTotal :: Double -> Double -> Double
vTotal r v = vOf (mBulge r + mDisk r + mHalo r v) r

-- 1 周の時間 [100 万年]
periodMyr :: Double -> Double -> Double
periodMyr r v = 2 * pi * r * kpcKm / max v 1e-6 / myrS

-- 1 フレーム = 50 万年
step :: World -> World
step w
  | wPlaying w = w { wStars = U.map move (wStars w) }
  | otherwise = w
  where
    move (r, th) = let v = if wHaloOn w then vTotal r (wVHalo w) else vVisible r
                   in (r, th + (v / (r * kpcKm)) * 0.5 * myrS)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 回る円盤
  [ Color (rgba255 255 230 180 30) (circleD cx cy (fromIntegral i * 14)) | i <- [6, 5 .. 1 :: Int] ]
  ++ [ Color (rgba255 255 240 210 170) (Pictures [ rect (cx + f2 r * sc * cos (f2 th)) (cy + f2 r * sc * sin (f2 th)) 2 2 | (r, th) <- U.toList (wStars w) ])
     , Color (rgba255 250 225 140 120) (Line [ (cx + 8 * sc * cos t, cy + 8 * sc * sin t) | k <- [0 .. 72 :: Int], let t = tau * fromIntegral k / 72 ])
     , say "Sun (8 kpc)" (cx + 8 * sc * 0.72) (cy - 8 * sc * 0.72) 14 (rgb255 250 225 140)
     , sayC (if wHaloOn w then "Stars move with the halo (same speed outside)" else "Stars move with visible matter only (slower outside)") cx (cy + 18 * sc + 30) 14 (rgb255 200 205 225) ]
  -- ---- 回転曲線
  ++ [ plotAxes g "Distance from the center r [kpc]" "Rotation speed v [km/s]" [0, 5, 10, 15, 20, 25, 30] [0, 100, 200, 300]
     , curve (\r -> vOf (mBulge r) r) (rgba255 255 170 120 150) 1
     , curve (\r -> vOf (mDisk r) r) (rgba255 140 190 255 150) 1
     , curve (\r -> vOf (mHalo r vh) r) (rgba255 200 140 255 180) 1
     , curve vVisible (rgb255 140 190 255) 2.5
     , curve (\r -> vTotal r vh) (rgb255 250 225 140) 3 ]
  ++ [ Color (rgba255 255 255 255 200) (circleD (gX g r) (gY g v) 7) | (r, v) <- wObs w ]
  ++ [ Color (rgb255 255 120 110) (circleD (gX g 8) (gY g 230) 11) ]
  -- 凡例（グラフの下）
  ++ concat [ [ Color c (rectTL lx (ly - 6) 12 4), say l (lx + 16) ly 14 (rgb255 210 215 230) ]
            | (i, (l, c)) <- zip [0 :: Int ..] legend
            , let lx = gx g + fromIntegral (i `mod` 4) * (gw g / 4)
            , let ly = gy g + gh g + 64 + fromIntegral (i `div` 4) * 20 ]
  -- ---- 数字
  ++ [ say ("Halo v_inf = " ++ showJ vh ++ " km/s (Up/Down)") x0 y0 15 (rgb255 250 225 140)
     , say ("At 8 kpc: visible only " ++ fmt 0 (vVisible 8) ++ ", with halo " ++ fmt 0 (vTotal 8 vh) ++ " km/s") x0 (y0 + 22) 15 (rgb255 250 225 140)
     , say ("Inside 30 kpc: visible " ++ fmt 1 (mv / 1e10) ++ ", halo " ++ fmt 1 (mh / 1e10) ++ " (x 1e10 Msun)") x0 (y0 + 44) 15 c2
     , say ("  -> halo / visible = " ++ fmt 1 (mh / mv) ++ " times") x0 (y0 + 66) 15 c2
     , say ("Sun's orbit 2 pi r / v = " ++ fmt 0 (periodMyr 8 (vTotal 8 vh)) ++ " Myr (about 200 million years)") x0 (y0 + 88) 15 c2
     , say ("Visible only: v(30) / v(15) = " ++ fmt 2 (vVisible 30 / vVisible 15) ++ " (1/sqrt2 = 0.71)") x0 (y0 + 110) 15 c2
     , say "Galaxy rotation curve | Up/Down halo mass  H halo for the stars  Space play/pause" 20 34 18 (rgb255 240 236 215)
     , drawSelfTest env (wTest w) ]
  where
    wd = envW env
    ht = envH env
    vh = wVHalo w
    cx = wd * 0.25
    cy = ht * 0.5
    sc = min (wd * 0.23) (ht * 0.4) / 18          -- 半径 18 kpc までを描く
    g = Graph (wd * 0.52) 110 (wd * 0.44) (ht * 0.5) 0 30 0 300 False False
    curve fn col wt = Color col (polyline wt [ (gX g r, gY g (fn r)) | i <- [1 .. 150 :: Int], let r = 30 * fromIntegral i / 150 ])
    legend = [ ("All (+ halo)", rgb255 250 225 140), ("Visible only", rgb255 140 190 255), ("Bulge", rgb255 255 170 120)
             , ("Disk", rgba255 140 190 255 150), ("Halo", rgb255 200 140 255), ("Mock data", white), ("Sun ~230 km/s", rgb255 255 120 110) ]
    x0 = gx g
    y0 = gy g + gh g + 116
    mv = mBulge 30 + mDisk 30
    mh = mHalo 30 vh
    c2 = rgb255 220 225 240

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wVHalo = min 300 (wVHalo w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wVHalo = max 0 (wVHalo w - 10) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (Char c) Down _ _ | c `elem` "hH" -> pure w { wHaloOn = not (wHaloOn w) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("visible matter only: v ~ 1/sqrt r outside, v(60)/v(30) ~ 0.71 (within 2%)", near (vVisible 60 / vVisible 30) (sqrt 0.5) 0.014)
  , ("with the halo (v_inf = 200) the curve is flat: v(50)/v(25) = 1 +- 0.1", near (vTotal 50 200 / vTotal 25 200) 1 0.1)
  , ("halo mass formula = numerical integral of rho0 / (1 + (r/rc)^2) (within 0.01%)", near (s / mHalo 20 200) 1 1e-4)
  , ("halo alone: speed approaches v_inf far away", near (vOf (mHalo 5000 200) 5000) 200 2)
  , ("default setting: speed at r = 8 kpc is 200-260 km/s (Sun ~230 km/s)", v8 > 200 && v8 < 260)
  , ("one orbit at 8 kpc and 230 km/s ~ 214 million years", near (periodMyr 8 230) 214 3)
  , ("the disk mass reaches M_DISK far away", near (mDisk 300) mDisk0 1) ]
  where
    n = 20000 :: Int
    r = 20
    s = foldl' (\acc i -> let x = (fromIntegral i + 0.5) / fromIntegral n * r
                          in acc + (4 * pi * x * x * (200 * 200)) / (4 * pi * gK * rCore * rCore * (1 + (x / rCore) ^ (2 :: Int))) * (r / fromIntegral n))
               0 [0 .. n - 1]
    v8 = vTotal 8 200

-- ---------- やってみよう ----------
-- 1. ↓ でハローを 0 に。回転曲線は外側で下がっていく。白い点（観測）と合う？
-- 2. ↑↓ で白い点にいちばん合う v∞ を探そう。そのときハローは見える物質の何倍？
-- 3. H で星の動きからハローを外すと、外側の星がおくれて腕の形が変わる
-- 4. mDisk0 を 2 倍にしても、外側を平らにできる？（内側が速くなりすぎないか見よう）
-- 5. 暗黒物質のほかに、重力の法則そのものを変える考え（MOND など）もある。調べてみよう

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
