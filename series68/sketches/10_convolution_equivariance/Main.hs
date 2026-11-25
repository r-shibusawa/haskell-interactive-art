-- ============================================
-- 68-10 畳み込みと等変性 ─ 3 × 3 のフィルタ（重み 9 個）を画像の全部の場所で使い回す「畳み込み層」
-- 出力 y[i][j] = Σ k[a][b] × x[i + a][j + b]。どの場所でも同じ重みなので、入力をずらすと出力もそのままずれる（並進等変性）
--   conv(ずらす(x)) = ずらす(conv(x))。この画面では、はしは反対側につながる「輪っか」（循環境界）で計算するので、差はぴったり 0
--   （0 で埋める境界だと、はしの数マスだけ差が出る。内側の画素は同じ）
-- 全結合なら 32 × 24 の画像 → 同じ大きさの出力に 768 × 768 = 589,824 個の重み。畳み込みは 9 個
-- 層を重ねると、1 つの出力が見ている入力の範囲（受容野）が 3 × 3 → 5 × 5 → 7 × 7 … と広がる
-- 操作：ドラッグ 絵を描く（Shift で消す）/ 1〜4 フィルタ / ← → ↑↓ 入力をずらす / L 受容野の層の数 / C 消す / D お手本の絵 / N ランダムな絵
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

iw, ih :: Int
iw = 32                    -- ★ 画像の大きさ
ih = 24

-- 画像：幅・高さと、画素（行の順、左上から）
data Img = Img { imW :: !Int, imH :: !Int, imPx :: !Vec }

-- 3 × 3 のフィルタ：k[b][a]（b = 上下 −1〜1、a = 左右 −1〜1）を行の順に 9 個
type Kernel = [Double]

filters :: [(String, Kernel)]
filters =
  [ ("vertical edges (Sobel x)", [-1, 0, 1, -2, 0, 2, -1, 0, 1])
  , ("horizontal edges (Sobel y)", [-1, -2, -1, 0, 0, 0, 1, 2, 1])
  , ("blur", [1 / 16, 2 / 16, 1 / 16, 2 / 16, 4 / 16, 2 / 16, 1 / 16, 2 / 16, 1 / 16])
  , ("Laplacian", [0, 1, 0, 1, -4, 1, 0, 1, 0]) ]

data World = World
  { wImg    :: !Img
  , wF      :: !Int
  , wShX    :: !Int
  , wShY    :: !Int
  , wLayers :: !Int               -- 受容野の層の数
  , wSeed   :: !Int
  , wShift  :: !Bool              -- Shift を押しながらクリックした（消す）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-10 Convolution equivariance" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World defaultPicture 0 3 2 2 1 False []

pxAt :: Img -> Int -> Int -> Double         -- x[j][i]
pxAt a i j = imPx a U.! (j * imW a + i)

makeImg :: Int -> Int -> (Int -> Int -> Double) -> Img
makeImg w h f = Img w h (U.generate (w * h) (\q -> let (j, i) = q `quotRem` w in f i j))

blankImg :: Img
blankImg = makeImg iw ih (\_ _ -> 0)

-- ★ お手本：家と丸
defaultPicture :: Img
defaultPicture = makeImg iw ih pix
  where
    pix i j =
      let x = i2d i
          y = i2d j
          v1 = if i >= 4 && i <= 13 && j >= 11 && j <= 19 then 1 else 0
          v2 = if j >= 5 && j <= 10 && abs (x - 8.5) <= y - 4.5 then 1 else v1
          v3 = if i >= 7 && i <= 9 && j >= 15 && j <= 19 then 0 else v2
          v4 = if (x - 23) ^ (2 :: Int) + (y - 9) ^ (2 :: Int) <= 20 then 1 else v3
      in if j == 21 && i >= 18 && i <= 29 then 0.6 else v4

-- ランダムな丸 6 個（丸の中の画素ごとに濃さを引く。0 になったら 1）
randomPicture :: Int -> R Img
randomPicture seed = do
  seedRng seed
  let circ a _ = do
        cx <- (* i2d iw) <$> rnd
        cy <- (* i2d ih) <$> rnd
        rr <- (\u -> 2 + 4 * u) <$> rnd
        foldM (\b q -> let (j, i) = q `quotRem` iw
                       in if (i2d i - cx) ^ (2 :: Int) + (i2d j - cy) ^ (2 :: Int) < rr * rr
                            then do u <- rnd
                                    let v = i2d (jsRound (u * 4)) / 4
                                    pure (b U.// [(q, if v == 0 then 1 else v)])
                            else pure b) a [0 .. iw * ih - 1]
  px <- foldM circ (U.replicate (iw * ih) 0) [1 .. 6 :: Int]
  pure (Img iw ih px)

-- ★ 循環境界の畳み込み：はしを越えたら反対側から（(i + a) mod W）
convCirc :: Img -> Kernel -> Img
convCirc x k = makeImg w h (\i j -> foldl' (\s (kv, a, b) -> s + kv * pxAt x ((i + a + w) `mod` w) ((j + b + h) `mod` h)) 0 (kIdx k))
  where w = imW x
        h = imH x

kIdx :: Kernel -> [(Double, Int, Int)]
kIdx k = zip3 k [ a | _ <- [-1 .. 1 :: Int], a <- [-1 .. 1] ] [ b | b <- [-1 .. 1], _ <- [-1 .. 1 :: Int] ]

-- 0 で埋める境界（くらべる用）
convZero :: Img -> Kernel -> Img
convZero x k = makeImg w h (\i j -> foldl' (add i j) 0 (kIdx k))
  where add i j s (kv, a, b) = let ii = i + a
                                   jj = j + b
                               in if jj >= 0 && jj < h && ii >= 0 && ii < w then s + kv * pxAt x ii jj else s
        w = imW x
        h = imH x

shiftCirc :: Img -> Int -> Int -> Img
shiftCirc x dx dy = makeImg (imW x) (imH x) (\i j -> pxAt x ((i - dx) `mod` imW x) ((j - dy) `mod` imH x))

maxDiff :: Img -> Img -> Double
maxDiff a b = U.foldl' max 0 (U.zipWith (\p q -> abs (p - q)) (imPx a) (imPx b))

subsample2 :: Img -> Img
subsample2 x = makeImg ((imW x + 1) `div` 2) ((imH x + 1) `div` 2) (\i j -> pxAt x (2 * i) (2 * j))

-- 受容野：出力 (ci, cj) に効く入力の画素 = 衝撃（1 点だけ 1）を、全部 1 の 3 × 3 で layers 回広げた範囲
receptiveField :: Int -> Int -> Int -> Img
receptiveField layers ci cj = iterate (`convCirc` replicate 9 1) delta !! max 0 layers
  where delta = makeImg iw ih (\i j -> if i == ci && j == cj then 1 else 0)

-- パネルの大きさと位置
layout :: Env -> (Float, Float, [Float], [Float])
layout env = let pw = min ((envW env - 140) / 3) (((envH env - 260) / 2) * (i2f iw / i2f ih))
                 ph = pw * (i2f ih / i2f iw)
             in (pw, ph, [40, 40 + pw + 30, 40 + 2 * (pw + 30)], [110, 110 + ph + 60])

-- 画像を描く：kind 0 = 入力（0〜1 を黒〜白）、1 = −scl〜scl をライム〜マゼンタ、2 = 0〜scl を暗い〜オレンジ
showImg :: Int -> Img -> Float -> Float -> Float -> Float -> Double -> String -> Picture
showImg kind a x y w h scl title = Pictures
  [ imageBox x y w h iw ih (gridImage iw ih (\i j -> let v = pxAt a i j in case kind of
      0 -> (20 + 225 * v, 20 + 215 * v, 22 + 200 * v)
      1 -> divCol (v / max 1e-9 scl)
      _ -> mixCol panel amber (v / max 1e-9 scl)))
  , say title x (y - 8) 14 (col ink) ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Convolution: " ++ fst (at' filters (wF w) ("", [])) ++ "   shift (" ++ show (wShX w) ++ ", " ++ show (wShY w)
              ++ ")   |conv(shift x) - shift(conv x)| = " ++ (if dif == 0 then "0  OK" else fmtE dif))
             ("drag draw (Shift erases)   1-4 filter   arrows shift the input   L receptive-field layers " ++ show (wLayers w) ++ "   C clear   D example   N random   (circular border)")
  , showImg 0 img (gx 0) (gy 0) pw ph 1 "input x (draw here)"
  , showImg 1 out (gx 1) (gy 0) pw ph mx "output conv(x)"
  , showImg 0 xs (gx 0) (gy 1) pw ph 1 "shifted input shift(x)"
  , showImg 1 outS (gx 1) (gy 1) pw ph mx "conv(shift(x))"
  , showImg 1 sOut (gx 2) (gy 1) pw ph mx "shift(conv(x))  <- same as left"
  , showImg 2 rf (gx 2) (gy 0) pw ph rmx ("receptive field: " ++ show l ++ " layers of 3x3 -> " ++ show (2 * l + 1) ++ "x" ++ show (2 * l + 1) ++ " (" ++ show cnt ++ " px)")
  , Color (col amber) (frameTL 2 (gx 1 + i2f ci / i2f iw * pw) (gy 0 + i2f cj / i2f ih * ph) (pw / i2f iw) (ph / i2f ih))
  , say ("weights: fully connected " ++ show (iw * ih) ++ " x " ++ show (iw * ih) ++ " = " ++ commas (iw * ih * iw * ih) ++ "   |   convolution 3 x 3 = 9 (the same 9 everywhere)") 40 ty 15 (col ink)
  , say ("zero-padded border: max difference = " ++ fmtN zd 3 ++ " (edges only). Subsample by 2: 1-px shift breaks it, 2-px ok") 40 (ty + 22) 14 (col sub)
  , say "Hover the output panel to see its receptive field (top right)." 40 (ty + 42) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    (pw, ph, gxs, gys) = layout env
    gx k = at' gxs k 0
    gy k = at' gys k 0
    img = wImg w
    k = snd (at' filters (wF w) ("", []))
    out = convCirc img k
    xs = shiftCirc img (wShX w) (wShY w)
    outS = convCirc xs k
    sOut = shiftCirc out (wShX w) (wShY w)
    dif = maxDiff outS sOut
    zd = maxDiff (convZero xs k) (shiftCirc (convZero img k) (wShX w) (wShY w))
    mx = U.foldl' (\m v -> max m (abs v)) 0 (imPx out)
    (mxp, myp) = mouseP env
    onOut = mxp > gx 1 && mxp < gx 1 + pw && myp > gy 0 && myp < gy 0 + ph
    (ci, cj) = if onOut then (clampI (floor ((mxp - gx 1) / pw * i2f iw)) 0 (iw - 1), clampI (floor ((myp - gy 0) / ph * i2f ih)) 0 (ih - 1)) else (16, 12)
    l = wLayers w
    rf = receptiveField l ci cj
    cnt = U.length (U.filter (> 0) (imPx rf))
    rmx = U.foldl' max 0 (imPx rf)
    ty = gy 1 + ph + 40

commas :: Int -> String                        -- 589824 → "589,824"
commas n = let s = show n
               go t = if length t <= 3 then t else go (take (length t - 3) t) ++ "," ++ drop (length t - 3) t
           in go s

-- マウスの位置の 2 × 2 の画素を描く（Shift なら消す）
paintAt :: Env -> Bool -> (Float, Float) -> World -> World
paintAt env shiftDown (mx, my) w =
  let (pw, ph, _, _) = layout env
      i = floor ((mx - 40) / pw * i2f iw)
      j = floor ((my - 110) / ph * i2f ih)
      v = if shiftDown then 0 else 1
      img = wImg w
      ups = [ ((j + b) * iw + i + a, v) | b <- [0, 1], a <- [0, 1], j + b < ih, i + a < iw ]
  in if i < 0 || i >= iw || j < 0 || j >= ih then w else w { wImg = img { imPx = imPx img U.// ups } }

shiftHeld :: Env -> Bool
shiftHeld env = isDown env (SpecialKey KeyShiftL) || isDown env (SpecialKey KeyShiftR)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down mods pos ->
    let sh = shift mods == Down || shiftHeld env in paintAt env sh (toP5 env pos) w { wShift = sh }
  EventMotion pos | envDown env -> paintAt env (wShift w || shiftHeld env) (toP5 env pos) w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wShX = (wShX w + 1) `mod` iw }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wShX = (wShX w + iw - 1) `mod` iw }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wShY = (wShY w + 1) `mod` ih }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wShY = (wShY w + ih - 1) `mod` ih }
  EventKey (Char ch) Down _ _
    | ch >= '1' && ch <= '4' -> w { wF = fromEnum ch - fromEnum '1' }
    | ch `elem` "lL" -> w { wLayers = wLayers w `mod` 5 + 1 }
    | ch `elem` "cC" -> w { wImg = blankImg }
    | ch `elem` "dD" -> w { wImg = defaultPicture }
    | ch `elem` "nN" -> w { wImg = withSeed 1 (randomPicture (wSeed w + 1)), wSeed = wSeed w + 1 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ← → で入力をずらしても、下の 2 つのパネルがいつも同じになることを確かめる
-- 2. 自分のフィルタを filters に足す（例：斜めの縁 [0, 1, 2, -1, 0, 1, -2, -1, 0]）
-- 3. L で層を増やす。5 層だと受容野は何 × 何？ 真ん中ほど濃いのはなぜ？
-- 4. draw の convCirc を convZero に変えると、ずらしたときの差は 0 のまま？ どこにずれが出る？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("循環境界：conv(ずらす(x)) = ずらす(conv(x)) が差 0 でぴったり（ランダムな画像・フィルタ・ずらし 6 組）", okShift)
  , ("真ん中だけ 1 のフィルタ → 出力 = 入力", maxDiff (convCirc x delta) x == 0)
  , ("線形：conv(2x − 3y) = 2 conv(x) − 3 conv(y)（ずれ " ++ fmtE lin ++ "）", lin < 1e-12)
  , ("0 で埋める境界も、はしから 1 マス内側の画素は循環境界と同じ", inner == 0)
  , ("受容野：3 × 3 を L 層重ねると (2L + 1)² 画素（L = 1〜5）", rfOk)
  , ("2 マスおきに間引くと：1 マスずらしは等変でない、2 マスずらしは 1 マスずれるだけ"
    , maxDiff s1 (shiftCirc base 1 0) > 0 && maxDiff s1 base > 0 && maxDiff s2 (shiftCirc base 1 0) == 0) ]
  where
    okShift = withSeed 1010 (and <$> mapM (const one) [1 .. 6 :: Int])
    one = do
      px <- rndVec (iw * ih)
      kv <- mapM (const randn) [1 .. 9 :: Int]
      dx <- rndInt (-40) 40
      dy <- rndInt (-30) 30
      let xi = Img iw ih px
      pure (maxDiff (convCirc (shiftCirc xi dx dy) kv) (shiftCirc (convCirc xi kv) dx dy) == 0)
    x = withSeed 1 (randomPicture 5)
    y = withSeed 1 (randomPicture 6)
    delta = [0, 0, 0, 0, 1, 0, 0, 0, 0]
    k = snd (at' filters 0 ("", []))
    cy = convCirc y k
    lin = maxDiff (convCirc (Img iw ih (U.zipWith (\v u -> 2 * v - 3 * u) (imPx x) (imPx y))) k)
                  (Img iw ih (U.zipWith (\v u -> 2 * v - 3 * u) (imPx (convCirc x k)) (imPx cy)))
    lap = snd (at' filters 3 ("", []))
    cz = convZero x lap
    cc = convCirc x lap
    inner = maximum (0 : [ abs (pxAt cz i j - pxAt cc i j) | j <- [1 .. ih - 2], i <- [1 .. iw - 2] ])
    rfOk = and [ U.length (U.filter (> 0) (imPx (receptiveField l 15 11))) == (2 * l + 1) ^ (2 :: Int) | l <- [1 .. 5] ]
    base = subsample2 (convCirc x k)
    s1 = subsample2 (convCirc (shiftCirc x 1 0) k)
    s2 = subsample2 (convCirc (shiftCirc x 2 0) k)

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 画像の道具：幅 iw × 高さ ih の画像の全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((i2d px + 0.5) / i2d iw) (1 - (i2d py + 0.5) / i2d ih)
  in (w8 r, w8 g, w8 b))

-- マス目の画像：fn i j（i = 左から、j = 上から）→ (r, g, b)
gridImage :: Int -> Int -> (Int -> Int -> RGB) -> Picture
gridImage iw ih fn = pixelImage iw ih 1 (\i j -> let (r, g, b) = fn i j in (w8 r, w8 g, w8 b))

w8 :: Double -> Word8                         -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）
w8 v = fromIntegral (c8 v)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / i2f iw) (negate h / i2f ih) pic)

mixCol :: RGB -> RGB -> Double -> RGB
mixCol (a0, a1, a2) (b0, b1, b2) t0 = let t = clampv t0 0 1 in (a0 + (b0 - a0) * t, a1 + (b1 - a1) * t, a2 + (b2 - a2) * t)

divCol :: Double -> RGB                       -- −1〜1 → ライム・暗い・マゼンタ
divCol v = if v >= 0 then mixCol panel mag v else mixCol panel lime (negate v)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
