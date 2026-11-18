-- ============================================
-- 51-15 ビデオフィードバック 📷
-- カメラで「カメラの映像を映している画面」を撮ると、映像が映像の中に入り続ける閉ループになる
-- ここでは 1 フレーム前の画面を少し拡大・回転して × g 倍し、カメラの映像を足す。g・回転・拡大で模様が自分で育つ
-- g > 1 なら暴走（正のフィードバック）するが、画素は 0〜255 で飽和するので止まる
-- 操作：↑↓ = ループのゲイン g / ←→ = 回転 / 1 2 = 拡大 −/+ / C = 色をずらす / V = カメラ ↔ テスト画像 / S = 保存 / R = 消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

fw, fh :: Int
fw = 200             -- ループの画像の大きさ（小さくして計算を軽く）
fh = 150

inW :: Double
inW = 0.35           -- ★ カメラの映像を足す強さ

-- 1 周の設定
data LoopP = LoopP { lpG, lpRot, lpZoom, lpInW :: !Double, lpShift :: !Bool }

data World = World
  { wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wGain   :: !Double         -- ★ ループのゲイン（1 周で何倍になるか）
  , wRotDeg :: !Double         -- ★ 1 周ごとの回転 [度]
  , wZoom   :: !Double         -- ★ 1 周ごとの拡大
  , wShift  :: !Bool
  , wUseCam :: !Bool
  , wFb     :: !(U.Vector Double)   -- いまの画面（RGB を 0〜255 の小数で）
  , wHist   :: [(Double, Double, Double)]   -- (平均の明るさ, エントロピー, 飽和の割合)（新しい順、400 個まで）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-15 Video feedback" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera fw fh
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World Nothing False 0.85 3 1.04 True True (U.replicate (fw * fh * 3) 0) [] []

-- 1 周：新しい画面(p) = g × 前の画面(拡大・回転をもどした位置) + inW × 入力(p)、0〜255 で飽和
loopStep :: Int -> Int -> LoopP -> U.Vector Double -> U.Vector Double -> U.Vector Double
loopStep w h p src a = U.generate (w * h * 3) px
  where
    cx = fromIntegral (w - 1) / 2
    cy = fromIntegral (h - 1) / 2
    c = cos (-(lpRot p)) / lpZoom p
    s = sin (-(lpRot p)) / lpZoom p
    px i =
      let (o, k) = i `divMod` 3
          (y, x) = o `divMod` w
          dx = fromIntegral x - cx
          dy = fromIntegral y - cy
          sx = cx + c * dx - s * dy
          sy = cy + s * dx + c * dy
          x0 = floor sx :: Int
          y0 = floor sy :: Int
          fx = sx - fromIntegral x0
          fy = sy - fromIntegral y0
          kk = if lpShift p then (k + 1) `mod` 3 else k          -- 色をずらす：前の G → 今の R …
          v | x0 >= 0 && y0 >= 0 && x0 < w && y0 < h =            -- 画面の外は黒
                let dx1 = if x0 + 1 < w then 3 else 0
                    dy1 = if y0 + 1 < h then 3 * w else 0
                    i00 = 3 * (y0 * w + x0) + kk
                    i10 = i00 + dx1
                    i01 = i00 + dy1
                    i11 = i01 + dx1
                    g j = U.unsafeIndex a j
                in (g i00 * (1 - fx) + g i10 * fx) * (1 - fy) + (g i01 * (1 - fx) + g i11 * fx) * fy   -- 双線形補間
            | otherwise = 0
          nv = lpG p * v + lpInW p * U.unsafeIndex src i
      in if nv > 255 then 255 else if nv < 0 then 0 else nv

-- 明るさのヒストグラム（32 区間）のエントロピー [ビット]：0 = 一色、5 = すべての明るさが同じだけ
lumEntropy :: U.Vector Double -> Double
lumEntropy arr = negate (sum [ (c / n) * logBase 2 (c / n) | c <- U.toList hgm, c > 0 ])
  where
    np = U.length arr `div` 3
    n = fromIntegral np
    bin i = min 31 (floor ((U.unsafeIndex arr (3 * i) + U.unsafeIndex arr (3 * i + 1) + U.unsafeIndex arr (3 * i + 2)) / 3 / 256 * 32))
    hgm = U.accum (+) (U.replicate 32 (0 :: Double)) [ (bin i, 1) | i <- [0 .. np - 1] ]

-- カメラがないときのテスト画像（動く丸と帯）
testFrame :: Double -> U.Vector Double
testFrame t = U.generate (fw * fh * 3) $ \i ->
  let (o, k) = i `divMod` 3
      (y, x) = o `divMod` fw
      d = sqrt ((fromIntegral x - fromIntegral fw / 2 - 40 * cos t) ^ (2 :: Int) + (fromIntegral y - fromIntegral fh / 2 - 30 * sin (1.3 * t)) ^ (2 :: Int))
      on = if d < 14 then 1 else 0
      band = if abs (fromIntegral x - fromIntegral fw / 2 :: Double) < 3 then 0.6 else 0
  in case k of
       0 -> 255 * on
       1 -> 160 * on + 200 * band
       _ -> 60 * on + 255 * band

step :: Env -> Float -> World -> IO World
step env _ w = do
  camSrc <- case wCam w of
    Just cam | wUseCam w -> do
      live <- camLive cam
      if not live then pure Nothing else do
        fr <- cameraFrame cam
        pure $ if B.length fr >= fw * fh * 3
                 then Just (U.generate (fw * fh * 3) (\i -> fromIntegral (BU.unsafeIndex fr i)))
                 else Nothing
    _ -> pure Nothing
  let src = maybe (testFrame (realToFrac (envTime env))) id camSrc
      fb = loopStep fw fh (LoopP (wGain w) (wRotDeg w * pi / 180) (wZoom w) inW (wShift w)) src (wFb w)
      tot = U.sum fb
      sat = U.length (U.filter id (U.generate (fw * fh) (\i -> fb U.! (3 * i) >= 254.5 || fb U.! (3 * i + 1) >= 254.5 || fb U.! (3 * i + 2) >= 254.5)))
      mean = tot / fromIntegral (fw * fh * 3)
      ent | envFrame env `mod` 6 == 0 = lumEntropy fb
          | otherwise = case wHist w of
              ((_, e, _) : _) -> e
              [] -> 0
  pure w { wFb = fb, wLive = camSrc /= Nothing
         , wHist = take 400 ((mean, ent, fromIntegral sat / fromIntegral (fw * fh)) : wHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (20 + iw / 2, 90 + ih / 2) (Scale 1 (-1) (pixelImage fw fh sc px))
  , say ((if wLive w then "Camera" else "Test image (V: camera)") ++ " - video feedback: film the filmed screen") 20 30 20 cTxt
  , say ("Up/Down g = " ++ fmt 2 (wGain w) ++ "   Left/Right rotate " ++ fmt 1 (wRotDeg w) ++ " deg   1 2 zoom " ++ fmt 3 (wZoom w)) 20 54 15 cSub
  , say ("C colour shift " ++ (if wShift w then "ON" else "OFF") ++ "   V switch   S save   R clear") 20 76 15 cSub
    -- ループの図と数
  , blockBox hx hy (bw * 0.45) 50 "Camera" (Just ("x " ++ num inW)) cScope
  , blockBox (hx + bw * 0.55) hy (bw * 0.45) 50 "+ -> screen" (Just "clip 0..255") cTube
  , blockBox (hx + bw * 0.55) (hy + 90) (bw * 0.45) 50 "Film screen" (Just "zoom rot x g") cBrass
  , flowArrow (hx + bw * 0.45) (hy + 25) (hx + bw * 0.55) (hy + 25) cScope ph Nothing
  , flowArrow (hx + bw * 0.78) (hy + 50) (hx + bw * 0.78) (hy + 90) cTube ph Nothing
  , flowArrow (hx + bw * 0.62) (hy + 90) (hx + bw * 0.62) (hy + 50) cBrass ph (Just "g")
  , say ("Mean brightness " ++ fmt 1 mean) hx my 15 cTxt
  , say ("Saturated pixels " ++ fmt 1 (100 * sat) ++ "%") hx (my + 24) 15 cTxt
  , say ("Brightness entropy " ++ fmt 3 ent ++ " bits (max 5)") hx (my + 48) 15 cTube ]
  ++ (if g < 1
        then [ say ("White input -> " ++ num inW ++ " x 255 / (1 - g)") hx (my + 72) 14 cScope
             , say ("  = " ++ (if fp > 255 then "255 (saturated)" else fmt 1 fp)) hx (my + 92) 14 cScope ]
        else [ say "g >= 1: bright parts stick at 255" hx (my + 72) 14 cAlarm ])
  ++ [ plotFrame hx gy bw gh "orange: brightness  green: entropy"
     , say "red: saturated share" (hx + 8) (gy + 38) 14 cSub
     , plotLine (map (\(a, _, _) -> a) hist) hx gy bw gh 0 255 cTube 400 1.8
     , plotLine (map (\(_, b, _) -> b) hist) hx gy bw gh 0 5 cScope 400 1.8
     , plotLine (map (\(_, _, c) -> c) hist) hx gy bw gh 0 1 cAlarm 400 1.2
     , drawSelfTest env (wTest w) ]
  where
    sc = min (envW env * 0.62 / fromIntegral fw) ((envH env - 130) / fromIntegral fh)
    iw = fromIntegral fw * sc
    ih = fromIntegral fh * sc
    fb = wFb w
    q v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px x y = let o = 3 * (y * fw + x) in (q (fb U.! o), q (fb U.! (o + 1)), q (fb U.! (o + 2)))
    hx = 40 + iw
    hy = 100
    bw = envW env - hx - 30
    ph = fract (fromIntegral (envFrame env) / 50)
    (mean, ent, sat) = case wHist w of
      (h0 : _) -> h0
      [] -> (0, 0, 0)
    g = wGain w
    fp = inW * 255 / (1 - g)
    my = hy + 180
    gy = my + 110
    gh = envH env - gy - 50
    hist = reverse (wHist w)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wGain = min 1.5 (roundTo 2 (wGain w + 0.05)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGain = max 0 (roundTo 2 (wGain w - 0.05)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wRotDeg = min 15 (wRotDeg w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wRotDeg = max (-15) (wRotDeg w - 0.5) }
  EventKey (Char c) Down _ _
    | c == '1' -> pure w { wZoom = max 0.9 (roundTo 3 (wZoom w - 0.01)) }
    | c == '2' -> pure w { wZoom = min 1.2 (roundTo 3 (wZoom w + 0.01)) }
    | c `elem` "cC" -> pure w { wShift = not (wShift w) }
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "video_feedback" env (draw env w) >> pure w
    | c `elem` "rR" -> pure w { wFb = U.replicate (fw * fh * 3) 0 }
  _ -> pure w

-- 小数点以下 n 桁に丸める（+(v).toFixed(n)）
roundTo :: Int -> Double -> Double
roundTo n v = fromIntegral (round (v * 10 ^ n) :: Int) / 10 ^ n

-- ---- 自己テスト（小さな 20 × 16 の画像で。p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("g = 0: no loop -> screen = 0.5 x camera (exact)"
    , U.and (U.zipWith (\v sv -> near v (0.5 * sv) 1e-4) t1 src))
  , ("g = 0.5, no input: fades in 30 rounds (max < 1, 0.5^30 x 255)", U.maximum t2 < 1)
  , ("g = 1.5 (runaway): pixels stay in 0..255 (saturation), no NaN", ok3)
  , ("no zoom/rotation, g = 0.6, input 100: settles at 50 / (1 - 0.6) = 125 (error < 0.01)", U.all (\v -> near v 125 0.01) t4)
  , ("entropy: one colour = 0 bits, one in each of 32 bins = 5 bits"
    , near (lumEntropy (U.replicate 30 77)) 0 1e-9 && near (lumEntropy uni) 5 1e-12) ]
  where
    w = 20
    h = 16
    n = w * h * 3
    src = U.fromList (map (\r -> fromIntegral (floor (r * 256) :: Int)) (take n (makeRng 3)))
    t1 = loopStep w h (LoopP 0 0.3 1.1 0.5 True) src (U.replicate n 200)
    zero = U.replicate n 0
    iter k p s a = foldr (\_ v -> loopStep w h p s v) a [1 .. k :: Int]
    t2 = iter 30 (LoopP 0.5 0.1 1.02 0.5 False) zero (U.replicate n 255)
    ok3 = fst (foldl (\(ok, a) _ -> let a' = loopStep w h (LoopP 1.5 0.2 1.05 0.5 True) src a
                                    in (ok && U.all (\v -> v >= 0 && v <= 255) a', a')) (True, zero) [1 .. 50 :: Int])
    t4 = iter 80 (LoopP 0.6 0 1 0.5 False) (U.replicate n 100) zero
    uni = U.fromList (concat [ replicate 3 (fromIntegral i * 8 + 4) | i <- [0 .. 31 :: Int] ])

-- ---------- やってみよう ----------
-- 1. ↑↓ で g を 0.5 → 0.95 → 1.05 と変えよう。模様の「寿命」はどう変わる？ 1 を超えると？
-- 2. ←→ で回転を変えると、らせんの腕の数や巻き方が変わる。1 2 で拡大と組み合わせよう
-- 3. C で色ずらしを切ると、色はどうなる？ ON だと 1 周ごとに R ← G ← B と色が回る
-- 4. 手をゆっくり動かすと、残像が「時間の地層」になる。S で保存しよう
-- 5. 本物のビデオカメラとモニターでもできる（1960 年代のビデオアートの技法）。ここで作った式との共通点は？

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- ブロック図の箱（名前と今の値）
blockBox :: Float -> Float -> Float -> Float -> String -> Maybe String -> HSB -> Picture
blockBox x y w h name val c = Pictures $
  [ Color (hsb 25 40 16) (rectTL x y w h), Color (col c 100) (frameTL 2 x y w h)
  , sayC name (x + w / 2) (y + h / 2 - (if val == Nothing then 0 else 10)) 15 cTxt ]
  ++ [ sayC v (x + w / 2) (y + h / 2 + 12) 14 c | Just v <- [val] ]

-- 信号の矢印：点が流れて「ループを回る」ようすを見せる（phase = 0〜1）
flowArrow :: Float -> Float -> Float -> Float -> HSB -> Float -> Maybe String -> Picture
flowArrow x1 y1 x2 y2 c phase lab = Pictures $
  [ Color (col c 100) (thickLine 2 (x1, y1) (x2, y2))
  , Color (col c 100) (Polygon [(x2, y2), (x2 - 11 * cos (a - 0.4), y2 - 11 * sin (a - 0.4)), (x2 - 11 * cos (a + 0.4), y2 - 11 * sin (a + 0.4))]) ]
  ++ [ Color (col c 100) (at (x1 + (x2 - x1) * f, y1 + (y2 - y1) * f) (disc 3)) | k <- [0, 1, 2], let f = fract (phase + k / 3) ]
  ++ [ say s ((x1 + x2) / 2 + 6) ((y1 + y2) / 2 - 6) 14 cSub | Just s <- [lab] ]
  where a = atan2 (y2 - y1) (x2 - x1)

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
makeRng :: Word32 -> [Double]
makeRng = go
  where
    go s0 =
      let s = s0 + 0x6d2b79f5
          t1 = (s `xor` (s `shiftR` 15)) * (1 .|. s)
          t2 = (t1 + ((t1 `xor` (t1 `shiftR` 7)) * (61 .|. t1))) `xor` t1
          r = t2 `xor` (t2 `shiftR` 14)
      in fromIntegral r / 4294967296 : go s

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
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
