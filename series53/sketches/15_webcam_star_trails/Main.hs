-- ============================================
-- 53-15 カメラで星の軌跡 ─ 映像の明るい点を「星」にして、日周運動の長時間露光を作る
-- カメラの画素のうち明るさがしきい値より上の点を星とみなし、北極星（天の北極）のまわりに少しずつ回しながら重ねる
-- 空は 1 恒星日（23 時間 56 分）で 360° 回る → 1 時間に 15.04°。回した角度から「何時間ぶんの露光か」を計算
-- 回転：x' = cx + (x − cx) cos θ − (y − cy) sin θ、y' = cy + (x − cx) sin θ + (y − cy) cos θ（中心からの距離は変わらない）
-- 操作：クリック = 天の北極の位置 / ↑↓ = 回す速さ / ←→ = しきい値 / C = 消す / V = カメラ ⇔ テスト画像 / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch :: Int
cw = 160                               -- カメラの大きさ（小さくして計算を軽く）
ch = 120

tw, th :: Int
tw = 320                               -- 軌跡の画像の大きさ（p5.js 版は 400 × 250。8 万画素以下にするため）
th = 200

siderealH :: Double
siderealH = 23.9345                    -- 1 恒星日 [時間]

data World = World
  { wDeg    :: !Double                 -- ★ 1 フレームで回す角度 [度]
  , wThresh :: !Double                 -- ★ 星とみなす明るさ（0〜1）
  , wUseCam :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wFrame  :: !B.ByteString           -- カメラの映像（右上に小さく出す）
  , wTrR, wTrG, wTrB :: !(U.Vector Float)   -- 軌跡の画像（0〜1）
  , wTheta  :: !Double                 -- 回した角度 [度]
  , wPole   :: !(Double, Double)       -- 天の北極の位置（画像の割合）
  , wN      :: !Int                    -- 星とみなした画素の数
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-15 Star trails from the camera" world0)
  { skBack  = black
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = \_ _ w -> step w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.25 0.6 True Nothing False B.empty z z z 0 (0.5, 0.18) 0 []
  where z = U.replicate (tw * th) 0

rotatePt :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
rotatePt x y cx cy t = (cx + (x - cx) * c - (y - cy) * s, cy + (x - cx) * s + (y - cy) * c)
  where c = cos t
        s = sin t

lum :: Double -> Double -> Double -> Double
lum r g b = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255

hoursOf :: Double -> Double
hoursOf d = d / 360 * siderealH

clearTrails :: World -> World
clearTrails w = w { wTrR = z, wTrG = z, wTrB = z, wTheta = 0 }
  where z = U.replicate (tw * th) 0

-- テスト画像：暗い空に、明るさと色のちがう星（種つきの乱数で毎回同じ）。RGB を 1 画素 3 バイトで
testImage :: U.Vector Word8
testImage = base U.// concatMap paint stars
  where
    base = U.generate (cw * ch * 3) (\i -> case i `mod` 3 of { 0 -> 5; 1 -> 8; _ -> 20 })
    cols = [(255, 200, 150), (255, 255, 240), (190, 210, 255), (255, 170, 120)] :: [(Double, Double, Double)]
    stars = fst (runRand (mapM (const star) [1 .. 120 :: Int]) (seedRng 2024))
    star = do
      x <- fmap (\u -> floor (u * fromIntegral cw)) rnd
      y <- fmap (\u -> floor (u * fromIntegral ch)) rnd
      b <- fmap (\u -> 0.5 + 0.5 * u) rnd
      c <- fmap (\u -> getAt (0, 0, 0) cols (floor (u * 4))) rnd
      big <- fmap (< 0.15) rnd
      pure (x, y, b, c, big)
    paint (x, y, b, (r, g, bl), big) =
      concat [ [(o, cl (r * b)), (o + 1, cl (g * b)), (o + 2, cl (bl * b))]
             | dy <- [0 .. (if big then 1 else 0)], dx <- [0 .. (if big then 1 else 0)]
             , let o = 3 * (min (ch - 1) (y + dy) * cw + min (cw - 1) (x + dx)) ]
    cl v = fromIntegral (max 0 (min 255 (round v :: Int)))   -- Uint8ClampedArray と同じ丸め

step :: World -> IO World
step w = do
  fr <- case wCam w of
    Just cam | wUseCam w -> do
      live <- camLive cam
      if not live then pure Nothing else do
        f <- cameraFrame cam
        pure (if B.length f >= cw * ch * 3 then Just f else Nothing)
    _ -> pure Nothing
  let px i = case fr of
        Just f -> fromIntegral (BU.unsafeIndex f i) :: Double
        Nothing -> fromIntegral (testImage U.! i)
      (polX, polY) = wPole w
      pcx = polX * fromIntegral tw
      pcy = polY * fromIntegral th
      t = wTheta w * degR
      thr = wThresh w
      -- 明るい画素を回して、軌跡の画像に「明るいほうを残す」で重ねる
      bright = [ (o, l) | y <- [0 .. ch - 1], x <- [0 .. cw - 1]
                        , let o = 3 * (y * cw + x), let l = lum (px o) (px (o + 1)) (px (o + 2)), l >= thr ]
      hits = [ (iy * tw + ix, (px o / 255 * wt, px (o + 1) / 255 * wt, px (o + 2) / 255 * wt))
             | (o, l) <- bright
             , let x = (o `div` 3) `mod` cw, let y = (o `div` 3) `div` cw
             , let (qx, qy) = rotatePt ((fromIntegral x + 0.5) / fromIntegral cw * fromIntegral tw) ((fromIntegral y + 0.5) / fromIntegral ch * fromIntegral th) pcx pcy t
             , let ix = floor qx, let iy = floor qy
             , ix >= 0, iy >= 0, ix < tw, iy < th
             , let wt = (l - thr) / max 1e-6 (1 - thr) * 0.7 + 0.3 ]
      upd v sel = U.accum max v [ (k, realToFrac (sel c)) | (k, c) <- hits ]
      (r', g', b') = (upd (wTrR w) (\(a, _, _) -> a), upd (wTrG w) (\(_, a, _) -> a), upd (wTrB w) (\(_, _, a) -> a))
  pure (r' `seq` g' `seq` b' `seq` w { wTrR = r', wTrG = g', wTrB = b', wTheta = wTheta w + wDeg w
                                     , wN = length bright, wLive = fr /= Nothing, wFrame = maybe B.empty id fr })

-- 画像の置き場所 (x0, y0, 倍率)
imgGeom :: Env -> (Float, Float, Float)
imgGeom env = ((envW env - iw) / 2, 74, sc)
  where sc = min (envW env / fromIntegral tw) ((envH env - 90) / fromIntegral th)
        iw = fromIntegral tw * sc

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (x0 + iw / 2, y0 + ih / 2) (Scale 1 (-1) (pixelImage tw th sc pix))
  , Color (rgba255 255 120 110 160) (at (ppx, ppy) (ThickCircle 7 1.5))
  , Color (rgba255 255 120 110 160) (Line [(ppx - 10, ppy), (ppx + 10, ppy)]) ]
  ++ [ at (x0 + iw - fromIntegral cw / 2 - 8, y0 + 8 + fromIntegral ch / 2)
          (Scale 1 (-1) (pixelImage cw ch 1 (\x y -> let i = 3 * (y * cw + x) in (B.index (wFrame w) i, B.index (wFrame w) (i + 1), B.index (wFrame w) (i + 2)))))
     | wLive w, B.length (wFrame w) >= cw * ch * 3 ]
  ++ [ say ((if wLive w then "Camera" else "Test image (V: camera)") ++ " | click: pole  Up/Down speed  Left/Right threshold  C clear  S save") 20 24 16 (rgb255 240 236 215)
     , say ("Rotated " ++ fmt 1 (wTheta w) ++ " deg = " ++ fmt 2 (hoursOf (wTheta w)) ++ " h of exposure (1 h = " ++ fmt 2 (360 / siderealH) ++ " deg)") 20 46 15 (rgb255 250 225 140)
     , say ("Per frame " ++ fmt 2 (wDeg w) ++ " deg   threshold " ++ fmt 2 (wThresh w) ++ "   pixels taken as stars " ++ show (wN w)) 20 66 15 (rgb255 250 225 140)
     , say (selfTestLine (wTest w)) 20 (envH env - 10) 14 (if all snd (wTest w) then rgb255 130 215 160 else rgb255 255 95 95) ]
  where
    (x0, y0, sc) = imgGeom env
    iw = fromIntegral tw * sc
    ih = fromIntegral th * sc
    (polX, polY) = wPole w
    ppx = x0 + f2 (polX * fromIntegral tw) * sc
    ppy = y0 + f2 (polY * fromIntegral th) * sc
    q v = fromIntegral (max 0 (min 255 (round (255 * v) :: Int))) :: Word8
    pix x y = let k = y * tw + x in (q (wTrR w U.! k), q (wTrG w U.! k), q (min 1 (wTrB w U.! k + 0.05)))

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx >= x0 && mx <= x0 + iw && my >= y0 && my <= y0 + ih ->
        pure (clearTrails w { wPole = (realToFrac ((mx - x0) / iw), realToFrac ((my - y0) / ih)) })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wDeg = min 2 (wDeg w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wDeg = max 0.05 (wDeg w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wThresh = min 0.95 (wThresh w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wThresh = max 0.1 (wThresh w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure (clearTrails w)
    | c `elem` "vV" -> pure (clearTrails w { wUseCam = not (wUseCam w) })
    | c `elem` "sS" -> do saveSvg "star_trails" env (draw env w)
                          pure w
  _ -> pure w
  where
    (x0, y0, sc) = imgGeom env
    iw = fromIntegral tw * sc
    ih = fromIntegral th * sc
    (mx, my) = mouseP5 env

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("rotating keeps the distance from the center (celestial pole)", near (sqrt ((px - 100) ^ (2 :: Int) + (py - 20) ^ (2 :: Int))) (sqrt (30 * 30 + 20 * 20)) 1e-9)
  , ("rotating by 360 deg brings the point back", near qx 130 1e-9 && near qy 40 1e-9)
  , ("speed of the sky = 360 deg / 23.934 h = 15.04 deg/h", near (360 / siderealH) 15.041 0.001)
  , ("4 hours of exposure -> arc of about 60.2 deg", near (4 / siderealH * 360) 60.16 0.01 && near (hoursOf 60.164) 4 0.001)
  , ("white has brightness 1, black 0 (stars are chosen by a threshold)", near (lum 255 255 255) 1 1e-12 && lum 0 0 0 == 0)
  , ("the test image has stars (20 or more pixels >= 0.6)", nStars >= 20) ]
  where
    (px, py) = rotatePt 130 40 100 20 1.234
    (qx, qy) = rotatePt 130 40 100 20 (2 * pi)
    tv i = fromIntegral (testImage U.! i)
    nStars = length [ () | i <- [0 .. cw * ch - 1], lum (tv (3 * i)) (tv (3 * i + 1)) (tv (3 * i + 2)) >= 0.6 ]

-- ---------- やってみよう ----------
-- 1. 部屋を暗くして、スマホのライトや豆電球をカメラに映そう。光が星の軌跡になる
-- 2. 天の北極をクリックで画面の真ん中にすると、軌跡は全部まん丸になる
-- 3. 回す速さを変えると点線のような軌跡になる。なめらかにするには？
-- 4. 1 時間 15° を使って、3 時間ぶん（45°）回したところで止まるようにしてみよう
-- 5. S で保存して、本物の星の軌跡の写真と比べよう

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
