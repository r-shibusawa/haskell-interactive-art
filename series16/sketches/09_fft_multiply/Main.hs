-- ============================================
-- 09 FFT で巨大な数を掛け算する
-- 数の各けたを多項式の係数と見る：1234 = 4 + 3x + 2x² + 1x³（x = 10）
-- 係数 → FFT で点の値 → 点ごとに掛ける → 逆 FFT で係数 → くり上がり。全部で Θ(n log n)
-- スペース：次の段階 / A：自動 / R：新しい数 / 1：4 けた  2：8 けた  3：16 けた  4：32 けた
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftL, testBit, (.|.))
import Data.Char (digitToInt, intToDigit, toLower)
import Data.List (foldl')

speed :: Int
speed = 70 -- ★ 自動再生の間隔（フレーム）

type C = (Double, Double)

data World = World
  { wDigits :: !Int
  , wA      :: !String
  , wB      :: !String
  , wN      :: !Int
  , wFa     :: [C]
  , wFb     :: [C]
  , wFc     :: [C]
  , wConv   :: [Int]
  , wProd   :: [Int]     -- くり上がりのあとのけた（1 の位が先頭）
  , wOk     :: !Bool     -- Integer の計算と一致したか
  , wStage  :: !Int
  , wAuto   :: !Bool
  , wTimer  :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-09 FFT Multiply" (compute "12345678" "87654321" (World 8 "" "" 1 [] [] [] [] [] False 0 False 0)))
  { skBack  = bgC
  , skStart = \_ -> newNumbers (World 8 "" "" 1 [] [] [] [] [] False 0 False 0)
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

newNumbers :: World -> IO World
newNumbers w = do
  let rnd' = do
        h <- randomRIO (1, 9 :: Int)
        t <- replicateM (wDigits w - 1) (randomRIO (0, 9 :: Int))
        pure (map intToDigit (h : t))
  a <- rnd'
  b <- rnd'
  pure (compute a b w)

digitsOf :: String -> [Int]
digitsOf = reverse . map digitToInt

compute :: String -> String -> World -> World
compute a b w =
  let da = digitsOf a
      db = digitsOf b
      nn = head' (dropWhile (< length da + length db) (iterate (* 2) 1))
      pad xs = map (\v -> (fromIntegral v, 0)) (xs ++ replicate (nn - length xs) 0)
      fa = fft False (pad da)
      fb = fft False (pad db)
      fc = zipWith cmul fa fb
      conv = map (\(re, _) -> floor (re + 0.5)) (fft True fc) :: [Int]
      prod = trim (carry 0 conv)
      asText = map intToDigit (reverse prod)
      ok = asText == show ((read a :: Integer) * (read b :: Integer))
  in w { wA = a, wB = b, wN = nn, wFa = fa, wFb = fb, wFc = fc, wConv = conv, wProd = prod, wOk = ok, wStage = 0 }
  where
    head' (x : _) = x
    head' [] = 1
    carry c [] = if c > 0 then c `mod` 10 : carry (c `div` 10) [] else []
    carry c (v : vs) = let s = v + c in s `mod` 10 : carry (s `div` 10) vs
    trim = reverse . dropZeros . reverse
    dropZeros (0 : r@(_ : _)) = dropZeros r
    dropZeros r = r

cmul :: C -> C -> C
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

-- 反復版 FFT（inv = True なら逆変換）
fft :: Bool -> [C] -> [C]
fft inv x =
  let n = length x
      l = round (logBase 2 (fromIntegral n :: Double)) :: Int
      rev i = foldl' (\r b -> if testBit i b then r .|. shiftL 1 (l - 1 - b) else r) 0 [0 .. l - 1] :: Int
      start = [x !! rev i | i <- [0 .. n - 1]]
      sign = if inv then -1 else 1 :: Double
      level xs m =
        let ang = sign * 2 * pi / fromIntegral m
            tw j = (cos (ang * fromIntegral j), sin (ang * fromIntegral j))
            block ys =
              let (u, v) = splitAt (m `div` 2) ys
                  t = zipWith (\j y -> cmul (tw (j :: Int)) y) [0 ..] v
              in zipWith (\(a, b) (c, d) -> (a + c, b + d)) u t ++ zipWith (\(a, b) (c, d) -> (a - c, b - d)) u t
        in concatMap block (chunks m xs)
      out = foldl' level start (takeWhile (<= n) (iterate (* 2) 2))
  in if inv then map (\(a, b) -> (a / fromIntegral n, b / fromIntegral n)) out else out

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks m xs = let (a, b) = splitAt m xs in a : chunks m b

stepW :: World -> World
stepW w0 =
  let timer = if wAuto w0 then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer }
  in if wAuto w0 && timer `mod` speed == 0 then w1 { wStage = (wStage w1 + 1) `mod` 6 } else w1

magC :: C -> Double
magC (a, b) = sqrt (a * a + b * b)

drawW :: Env -> World -> Picture
drawW env w =
  let nn = wN w
      nf = fromIntegral nn :: Float
      left = 30
      cw = min 36 ((envW env * 0.68 - 120) / nf)
      long = wDigits w > 8
      yTitle = 120
      y0 = if long then yTitle + 64 else yTitle + 40
      rowLabel s yy = txtR env (left + 96) (yy + 6) 14 (g8 170) s
      -- けたのマス（マスに入らない数は色の濃さだけで見せる）
      cells arr yy col =
        let top = fromIntegral (foldl' max 1 arr) :: Float
            one i =
              let x = left + 110 + fromIntegral i * cw
              in case drop i arr of
                   (v : _) ->
                     let str = show v
                         fits = fromIntegral (length str) * 14 <= cw + 1
                         k = if fits then 1 else 0.35 + 0.65 * fromIntegral v / top
                     in Pictures
                          [ box env (withA k col) x (yy - 15) (cw - 3) 30
                          , if fits then txtC env (x + cw / 2 - 1) yy 14 (g8 240) str else Blank ]
                   [] -> box env panelC x (yy - 15) (cw - 3) 30
        in Pictures (map one [0 .. nn - 1])
      bars arr yy col h =
        let mx0 = foldl' max 0 (map magC arr)
            mx = if mx0 == 0 then 1 else mx0
        in Pictures [ let bh = realToFrac (magC v / mx) * h
                      in box env col (left + 110 + fromIntegral i * cw) (yy - bh) (cw - 3) bh
                    | (i, v) <- zip [0 :: Int ..] arr ]
      st = wStage w
      y1 = y0
      y2 = y1 + 40
      y3 = y2 + 34
      y4 = y3 + 34
      y5 = y4 + 170
      y6 = y5 + 90
      y7 = y6 + 40
      prodStr = map intToDigit (reverse (wProd w))
      -- 右：説明と計算量
      px = envW env * 0.7
      pw = envW env - px - 30
      msgs = [ "Digits as coefficients of A(x), B(x). Put x = 10 to get the numbers back"
             , "FFT: values at the N-th roots of unity (point values). Theta(N log N)"
             , "Multiply pointwise: C(w^k) = A(w^k) * B(w^k). Only N products"
             , "Inverse FFT: back to C's coefficients: the convolution (columns of long multiplication)"
             , "Carry: send anything over 10 to the next digit"
             , "Done!" ]
      msg = case drop st msgs of
        (m : _) -> m
        [] -> ""
      dg = wDigits w
      school = dg * dg
      fftc = floor (3 * nf * logBase 2 nf / 2 + nf + 0.5) :: Int
      yCount = 120 + 140 + 28
      fftLine = "FFT: about " ++ show fftc ++ " (3 FFTs + " ++ show nn ++ " products)"
      yGraph = yCount + 24 + paraH 15 pw fftLine + 10
      gX = px
      gY = yGraph + 150
      gW = pw - 10
      gH = 140
      curve col f = Color col (thickPath 2.5
        [ pt env (gX + i / 100 * gW, gY - min gH (f (i * 10) / 1e6 * gH)) | i <- [1 .. 100] ])
  in Pictures
       [ txt env 30 50 26 (g8 235) "Multiplying huge numbers with FFT"
       , txt env 30 80 15 (g8 150) ("Space: next stage  A: auto" ++ (if wAuto w then " (on)" else "") ++ "  R: new numbers  1: 4 digits  2: 8  3: 16  4: 32")
       , if long
           then Pictures [ txtT env left yTitle 18 (g8 235) (wA w), txtT env left (yTitle + 26) 18 (g8 235) ("x " ++ wB w) ]
           else txtT env left yTitle 20 (g8 235) (wA w ++ " x " ++ wB w)
       , rowLabel "A digits" (y1 + 15)
       , cells (digitsOf (wA w)) (y1 + 15) (rgb255 110 40 55)
       , rowLabel "B digits" (y2 + 15)
       , cells (digitsOf (wB w)) (y2 + 15) (rgb255 110 40 55)
       , txtT env (left + 110) y3 14 (g8 150) ("<- ones digit  (digits reversed, padded with 0 to length " ++ show nn ++ ")")
       , if st >= 1
           then Pictures [ rowLabel "FFT(A)" (y4 + 40), bars (wFa w) (y4 + 70) amber 60
                         , rowLabel "FFT(B)" (y4 + 120), bars (wFb w) (y4 + 150) amber 60 ]
           else Blank
       , if st >= 2 then Pictures [rowLabel "Pointwise" (y5 + 40), bars (wFc w) (y5 + 70) coral 60] else Blank
       , if st >= 3 then Pictures [rowLabel "Inv FFT" (y6 + 15), cells (wConv w) (y6 + 15) (rgb255 30 90 80)] else Blank
       , if st >= 4 then Pictures [rowLabel "Carry" (y7 + 15), cells (wProd w) (y7 + 15) mint] else Blank
       , if st >= 5
           then para env left (y7 + 50) 16 mint (envW env * 0.68 - 30)
                  ("= " ++ unwords (chunks 48 prodStr) ++ "\n" ++ (if wOk w then "OK: matches Integer arithmetic" else "NG: differs (rounding error)"))
           else Blank
       , box env panelC (px - 10) 110 (pw + 10) 120
       , txtT env px 118 16 (g8 235) ("Stage " ++ show st ++ " / 5")
       , para env px 142 15 (g8 235) (pw - 10) msg
       , txtT env px 260 16 (g8 200) "Number of multiplications"
       , txtT env px yCount 15 coral ("School: " ++ show dg ++ " x " ++ show dg ++ " = " ++ show school)
       , para env px (yCount + 24) 15 amber pw fftLine
       -- n² と n log n のグラフ
       , seg env (g8 70) 1 (gX, gY) (gX + gW, gY)
       , seg env (g8 70) 1 (gX, gY) (gX, gY - gH)
       , curve coral (\m -> m * m)
       , curve amber (\m -> 3 * m * logBase 2 (2 * m))
       , txt env (gX + gW - 140) (gY + 20) 14 (g8 150) "digits -> 1000"
       , txt env (gX + gW * 0.55) (gY - gH + 14) 14 (g8 150) "n^2"
       , txt env (gX + gW - 80) (gY - 26) 14 (g8 150) "n log n"
       , para env px (gY + 30) 14 (g8 150) (pw - 10)
           "A million digits: school 10^12, FFT about 10^8. Records for pi and huge crypto numbers use this. Watch for rounding when numbers get too big."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wStage = (wStage w + 1) `mod` 6 }
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> newNumbers w
    '1' -> newNumbers w { wDigits = 4 }
    '2' -> newNumbers w { wDigits = 8 }
    '3' -> newNumbers w { wDigits = 16 }
    '4' -> newNumbers w { wDigits = 32 }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 4 けたで、逆 FFT の行を手で確かめる：1 の位 = a0 b0、10 の位 = a0 b1 + a1 b0、…
-- 2. 99999999 × 99999999 のような数（compute を直接呼ぶ）でくり上がりがたくさん起きる
-- 3. 10 進数でなく 100 進数（2 けたずつ）にすると、配列が半分になって速くなる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v
