-- ============================================
-- 59-15 カメラ映像とマックスプラス ─ 画像の「膨張」は max-plus のたたみこみ (f ⊕⊗ g)(x) = max_y ( f(y) + g(x − y) )
-- ふつうのたたみこみ（ぼかし）の「Σ と ×」を「max と +」に替えただけ。g = 放物線 −c·|z|²（やわらかい構造要素）
--   1 膨張（max-plus）：明るい所がふくらむ / 2 収縮（min-plus）：暗い所がふくらむ（= −膨張(−f)）
--   3 距離変換：しきい値より明るいマスまでの距離² = min_y ( 0 or +∞ + |x − y|² )（min-plus のたたみこみ）
--   放物線は |z|² = z₁² + z₂² なので、横 → 縦の 1 次元 2 回に分けられる（同じ答えで速い）。シリーズ 19 のモルフォロジーとつながる
-- 操作：1 膨張 / 2 収縮 / 3 距離変換 / ↑↓ 大きさ s / ← → しきい値（3）/ V カメラ ⇔ テスト画像 / S 保存（SVG）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
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

rMax :: Int
rMax = 14                  -- ★ 窓の半径（大きいほど遠くまで届くが重い）

data World = World
  { wMode   :: !Int        -- 1 膨張 / 2 収縮 / 3 距離変換
  , wSize   :: !Int        -- ★ 放物線の大きさ s
  , wThr    :: !Int        -- ★ 距離変換のしきい値
  , wUseCam :: !Bool
  , wLive   :: !Bool
  , wCam    :: Maybe Camera
  , wF      :: !(U.Vector Double)   -- もとの明るさ（0〜255）
  , wRes    :: !(U.Vector Double)   -- たたみこみの答え
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-15 Camera and max-plus" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch          -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 4 150 True False Nothing (U.replicate (cw * ch) 0) (U.replicate (cw * ch) 0) []

-- ★ 1 次元の max-plus（isMax）/ min-plus たたみこみ。w の d + R 番目 = 構造要素 g(d)（max）または罰金（min）
conv1d :: U.Vector Double -> Int -> Int -> Bool -> Int -> U.Vector Double -> Bool -> U.Vector Double
conv1d src wd ht horiz r w isMax = U.generate (wd * ht) px
  where
    px i =
      let (y, x) = i `quotRem` wd
          (pos, len, stride) = if horiz then (x, wd, 1) else (y, ht, wd)
          dlo = max (-r) (pos - (len - 1))     -- 画面の外（xx < 0 や xx ≥ W）は飛ばす
          dhi = min r pos
          -- d 番目：もとの画像のマス i − d·stride と g(d) = w の d + R 番目
          goMax !d !best
            | d > dhi = best
            | otherwise = let v = U.unsafeIndex src (i - d * stride)
                              t = v + U.unsafeIndex w (d + r)
                          in goMax (d + 1) (if v /= negInf && t > best then t else best)
          goMin !d !best
            | d > dhi = best
            | otherwise = let v = U.unsafeIndex src (i - d * stride)
                              t = v + U.unsafeIndex w (d + r)
                          in goMin (d + 1) (if v /= posInf && t < best then t else best)
      in if isMax then goMax dlo negInf else goMin dlo posInf

parab :: Int -> Double -> Double -> U.Vector Double
parab r c sgn = U.generate (2 * r + 1) (\i -> sgn * c * fromIntegral ((i - r) * (i - r)))

-- ★ 2 次元 = 横 → 縦（放物線は分けられる）
morph2d :: U.Vector Double -> Int -> Int -> Int -> Double -> Bool -> U.Vector Double
morph2d src wd ht r c isMax =
  let w = parab r c (if isMax then -1 else 1)
  in conv1d (conv1d src wd ht True r w isMax) wd ht False r w isMax

-- mask が True のマスまでの距離²（窓の外は +∞）
distanceSq :: U.Vector Bool -> Int -> Int -> Int -> U.Vector Double
distanceSq mask wd ht r = morph2d (U.map (\m -> if m then 0 else posInf) mask) wd ht r 1 False

-- カメラがないときのテスト画像（明るい円・線・点）
testPix :: U.Vector Double
testPix = U.generate (cw * ch) $ \i ->
  let (y, x) = i `quotRem` cw
      fx = fromIntegral x :: Double
      fy = fromIntegral y
      v0 = 30 + 0.2 * fx
      v1 = if (x - 45) ^ (2 :: Int) + (y - 60) ^ (2 :: Int) < 18 * 18 then 220 else v0
      v2 = if abs (fy - 0.5 * fx - 10) < 1.5 && x > 80 then 250 else v1
      v3 = if x `mod` 23 == 5 && y `mod` 19 == 7 then 255 else v2
      v4 = if x > 110 && x < 140 && y > 75 && y < 100 then 170 else v3
      q = fromIntegral (jsByte v4)            -- Uint8ClampedArray に入れると整数になる
  in 0.3 * q + 0.59 * q + 0.11 * q

-- Uint8ClampedArray と同じ丸め（0〜255、最も近い整数、ちょうど半分は偶数へ）
jsByte :: Double -> Int
jsByte v = max 0 (min 255 (round v))

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
      f | live = U.generate (cw * ch) (\i -> 0.3 * g (3 * i) + 0.59 * g (3 * i + 1) + 0.11 * g (3 * i + 2))
        | otherwise = testPix
      s = fromIntegral (wSize w) :: Double
      c = 30 / (s * s)
      r = min rMax (ceiling (s * 3))
      res = case wMode w of
        1 -> morph2d f cw ch r c True
        2 -> morph2d f cw ch r c False
        _ -> distanceSq (U.map (> fromIntegral (wThr w)) f) cw ch rMax
  pure w { wLive = live, wF = f, wRes = res }

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (40 + 160, 110 + 120) (Scale 1 (-1) (pixelImage cw ch 2 (\x y -> let q = toB (wF w U.! (x + cw * y)) in (q, q, q))))
  , at (400 + fromIntegral cw * big / 2, 110 + fromIntegral ch * big / 2) (Scale 1 (-1) (pixelImage cw ch big (\x y -> outColor (wRes w U.! (x + cw * y)))))
  -- ---- 構造要素 g の形
  , Color (grayC 80) (frameTL 1 gx gy gw gh)
  , Color (rgbC 255 170 70) (thickPath 2 [ (gx + fromIntegral (d + rMax) / fromIntegral (2 * rMax) * gw, if mode == 1 then gy + 10 + f2 u * (gh - 20) else gy + gh - 10 - f2 u * (gh - 20))
                                        | d <- [-rMax .. rMax]
                                        , let fd = fromIntegral d :: Double
                                              v | mode == 3 = -fd * fd
                                                | abs d <= r = -c * fd * fd
                                                | otherwise = -255
                                              u = clampv 0 1 (-v / (if mode == 3 then fromIntegral (rMax * rMax) else 255)) ])
  , say (case mode of
           3 -> "penalty |z|^2 (min-plus)"
           2 -> "penalty -g(z) = +" ++ fmt 2 c ++ " z^2 (min-plus, radius " ++ show r ++ ")"
           _ -> "g(z) = -" ++ fmt 2 c ++ " z^2 (window radius " ++ show r ++ ")") gx (gy - 8) 14 (grayC 200)
  , say ("Camera and max-plus   " ++ headOr "" (drop mode ["", "1 dilation (max-plus)", "2 erosion (min-plus)", "3 distance transform (min-plus)"])
         ++ (if wLive w then "" else "   (test image)")) 40 40 20 (grayC 240)
  , say ("1 dilation   2 erosion   3 distance   Up/Down size s = " ++ show (wSize w) ++ "   <- -> threshold " ++ show (wThr w) ++ "   V camera / test image   S save") 40 66 15 (grayC 170)
  ] ++ [ say l 40 (gy + gh + 30 + fromIntegral k * 18) 14 col | (k, (l, col)) <- zip [0 :: Int ..] notes ] ++
  [ drawSelfTest env (wTest w) ]
  where
    mode = wMode w
    notes = [ (l, grayC 200) | l <- wrapText 340 14 (case mode of
               1 -> "(f (+)(x) g)(x) = max_y ( f(y) + g(x - y) ): each bright point becomes a parabola 'hill' and spreads"
               2 -> "(f (-) g)(x) = min_y ( f(y) - g(x - y) ) = -((-f) (+)(x) g): dark parts spread"
               _ -> "dist^2 = min_y ( 0 (bright) or +inf ) + |x - y|^2: stripes = equal-distance lines every 3 px") ]
            ++ [ ("", grayC 150) ]
            ++ [ (l, grayC 150) | l <- wrapText 340 14 "Left = original brightness. Ordinary convolution (blur) with sum and times replaced by max and +" ]
    big = min ((envW env - 420) / fromIntegral cw) ((envH env - 170) / fromIntegral ch)
    gx = 40
    gy = 400
    gw = 320
    gh = 120
    s = fromIntegral (wSize w) :: Double
    c = 30 / (s * s)
    r = min rMax (ceiling (s * 3))
    toB v = fromIntegral (jsByte (clampv 0 255 v)) :: Word8
    outColor v
      | mode == 3 = if v == posInf then (25, 25, 35)
                    else let dd = sqrt v
                             band = fromIntegral ((floor (dd / 3) :: Int) `mod` 2)
                             u = dd / fromIntegral rMax
                         in (b8 (255 - 180 * u), b8 (170 - 60 * u + band * 30), b8 (70 + 160 * u))
      | otherwise = let q = clampv 0 255 v
                    in (b8 q, b8 (q * 0.85 + (if mode == 2 then 20 else 0)), b8 (q * (if mode == 1 then 0.6 else 1)))
    b8 x = fromIntegral (jsByte x) :: Word8

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char k) Down _ _
    | k `elem` "123" -> pure w { wMode = fromEnum k - fromEnum '0' }
    | k `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | k `elem` "sS" -> do saveSvg "maxplus_camera" env (draw env w); pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSize = min 8 (wSize w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSize = max 1 (wSize w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wThr = min 250 (wThr w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wThr = max 10 (wThr w - 10) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 15・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("horizontal then vertical = the 2-D max-plus convolution (all of the window checked)", okSep)
  , ("dilation >= original image >= erosion (because g(0) = 0)", U.and (U.zipWith (>=) out img) && U.and (U.zipWith (<=) er img))
  , ("duality: erosion(f) = -dilation(-f)", U.and (U.zipWith (\a b -> abs (a + b) < 1e-9) er dn))
  , ("brightness +7 -> dilation +7 (max-plus 'linear')", U.and (U.zipWith (\a b -> abs (a - b - 7) < 1e-9) ds out))
  , ("distance transform = squared distance to the nearest bright cell (brute force)", okDT)
  , ("no bright cell at all -> everything +inf (no NaN)", U.all (== posInf) de && not (U.any isNaN out)) ]
  where
    wd = 20
    ht = 16
    r = 4
    c = 2.5
    (img, m) = withSeed 15 $ do
      a <- U.fromList <$> mapM (const (fromIntegral <$> rndInt 0 255)) [1 .. wd * ht]
      b <- U.fromList <$> mapM (const ((< 0.05) <$> rnd)) [1 .. wd * ht]
      pure (a, b U.// [(37, True)])
    out = morph2d img wd ht r c True
    okSep = and [ abs (best - out U.! (x + wd * y)) <= 1e-9
                | y <- [0 .. ht - 1], x <- [0 .. wd - 1]
                , let best = foldl' max negInf [ img U.! (xx + wd * yy) - c * fromIntegral (dx * dx + dy * dy)
                                               | dy <- [-r .. r], dx <- [-r .. r], let xx = x - dx, let yy = y - dy
                                               , xx >= 0, yy >= 0, xx < wd, yy < ht ] ]
    er = morph2d img wd ht r c False
    dn = morph2d (U.map negate img) wd ht r c True
    ds = morph2d (U.map (+ 7) img) wd ht r c True
    dt = distanceSq m wd ht 25
    okDT = and [ b == dt U.! i
               | i <- [0 .. wd * ht - 1]
               , let b = foldl' min posInf [ fromIntegral ((i `mod` wd - j `mod` wd) ^ (2 :: Int) + (i `div` wd - j `div` wd) ^ (2 :: Int))
                                           | j <- [0 .. wd * ht - 1], m U.! j ] ]
    de = distanceSq (U.replicate (wd * ht) False) wd ht 5

-- ---------- やってみよう ----------
-- 1. 1（膨張）で ↑ を押して s を大きくすると、明るい点はどんな形に広がる？（放物線の山）
-- 2. 2（収縮）にすると、細い明るい線はどうなる？ 1 → 2 の順にかけると「オープニング」（シリーズ 19）
-- 3. 3（距離変換）で ← → のしきい値を変え、しま模様（等距離線）を見よう
-- 4. parab を放物線ではなく「円の中は 0、外は −∞」にすると、ふつうの（かたい）膨張になる
-- 5. S で保存して、膨張と収縮をくり返した絵を作ろう

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine
-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

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
