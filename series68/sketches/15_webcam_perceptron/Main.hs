-- ============================================
-- 68-15 カメラ映像を学ぶパーセプトロン 📷 ─ 映像を 16 × 12 = 192 画素の明るさに縮め、それを入力にする 1 個のニューロン
-- 1 キー = 「A（例：手をあげる）」、2 キー = 「B（手をおろす）」として、いまの映像を 10 枚ずつ教える
-- 学習は 00 と同じローゼンブラットの規則（まちがえたら w ← w + y x）を、ためた例で毎フレーム少しずつ（オンライン学習）
-- 重み w を画像として見ると、ネットが「どこを見ているか」が分かる（マゼンタ = 明るいと A、ライム = 明るいと B）
-- 入力は 1 枚ごとに平均 0・標準偏差 1 にそろえる（部屋の明るさが変わっても大丈夫なように）
-- 操作：1 2 例を教える / スペース 学習を止める / R 全部忘れる / C カメラ ⇔ テスト画像 / S 画面を保存（SVG）
-- カメラが使えないときは、テスト画像（マウスを上にすると手があがる人）で動く
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
import qualified Data.ByteString as B
gw16, gh12, nIn :: Int
gw16 = 16                  -- 縮めた画像の大きさ
gh12 = 12
nIn = gw16 * gh12

burst, maxEx, passesPerFrame :: Int
burst = 10                 -- ★ 1 回に教える枚数
maxEx = 300                -- ★ ためる例の最大
passesPerFrame = 40        -- ★ 1 フレームに調べる例の数

-- 教えた例：入力 x（193 個）、答え y（+1 = A、−1 = B）、映像の絵
data Example = Example { exX :: !Vec, exY :: !Int, exPic :: Picture }

data World = World
  { wCam      :: Maybe Camera
  , wUseCam   :: !Bool
  , wLive     :: !Bool              -- いまカメラの映像を使っているか
  , wW        :: !Vec               -- 重み（最後がバイアス）
  , wExs      :: !(V.Vector Example)
  , wBurst    :: !Int
  , wBurstCls :: !Int
  , wCursor   :: !Int
  , wMist     :: ![Int]             -- 最近 200 回の修正（1 = 修正した、新しい順）
  , wLearn    :: !Bool
  , wGray     :: !Vec               -- いまの映像（64 × 48 の明るさ）
  , wX        :: !Vec               -- いまの入力
  , wNoise    :: !Int
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-15 Webcam perceptron" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera 64 48               -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World Nothing True False (U.replicate (nIn + 1) 0) V.empty 0 1 0 [] True g (features g) 1 []
  where g = testScene 0.5 1

-- テスト画像（64 × 48 の明るさ）：人の形。handUp（0〜1）で右手が上がる。ノイズは種つき
testScene :: Double -> Int -> Vec
testScene handUp seed = U.imap pix (withSeed seed (rndVec (64 * 48)))
  where
    ts = takeWhile (<= 1) (iterate (+ 0.05) 0)   -- t = 0, 0.05, … を足し算で（p5.js 版と同じ値）
    hx = 44
    hy = 30 - 22 * handUp                         -- 右手の先
    pix i u =
      let (y, x) = i `quotRem` 64
          xd = i2d x
          yd = i2d y
          v0 = 60 + yd * 1.2 + 25 * u
          v1 = if (xd - 32) ** 2 + (yd - 16) ** 2 < 36 then 200 else v0          -- 頭
          v2 = if x > 24 && x < 40 && y > 22 && y < 46 then 170 else v1          -- 体
          v3 = foldl' (\v t -> let ax = 39 + (hx - 39) * t
                                   ay = 26 + (hy - 26) * t
                               in if (xd - ax) ** 2 + (yd - ay) ** 2 < 6 then 210 else v) v2 ts
      in min 255 v3

-- ★ 縮める：4 × 4 の平均で 16 × 12 に → 平均 0・標準偏差 1 にそろえて、最後にバイアスの 1
features :: Vec -> Vec
features gray = U.snoc (U.map (\v -> if sd > 1e-6 then (v - m) / sd else 0) f) 1
  where
    f = U.accum (+) (U.replicate nIn 0) [ ((y `quot` 4) * gw16 + x `quot` 4, (gray U.! (y * 64 + x)) / 16) | y <- [0 .. 47], x <- [0 .. 63] ]
    fl = U.toList f
    m = meanOf fl
    sd = sqrt (meanOf (map (\v -> (v - m) ** 2) fl))

-- カメラの RGB → 明るさ
camGray64 :: B.ByteString -> Vec
camGray64 bs = U.generate (64 * 48) (\i -> 0.3 * byte (3 * i) + 0.59 * byte (3 * i + 1) + 0.11 * byte (3 * i + 2))
  where byte k = fromIntegral (B.index bs k) :: Double

-- ★ パーセプトロンの 1 回（00 と同じ）：まちがえたら w ← w + y x
perceptronUpdate :: Vec -> Vec -> Int -> (Vec, Bool)
perceptronUpdate w x y = let yd = i2d y in
  if yd * dotV w x <= 0 then (U.zipWith (\wi xi -> wi + yd * xi) w x, True) else (w, False)

-- 止まるまで（または maxEpochs 周まで）：戻り値 = (重み, 修正の回数, 周の数, 止まったか)
trainPerceptron :: Vec -> [(Vec, Int)] -> Int -> (Vec, Int, Int, Bool)
trainPerceptron w0 exs maxEpochs = go 0 w0 0
  where
    go e w k
      | e >= maxEpochs = (w, k, maxEpochs, False)
      | otherwise =
          let (w', bad) = foldl' (\(ww, b) (x, y) -> let (w2, ch) = perceptronUpdate ww x y in (w2, if ch then b + 1 else b)) (w, 0 :: Int) exs
          in if bad == 0 then (w', k, e + 1, True) else go (e + 1) w' (k + bad)

grayPic :: Vec -> Picture
grayPic g = pixelImage 64 48 1 (\x y -> let v = g U.! (y * 64 + x) in (w8 v, w8 v, w8 (v * 0.95)))

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  camG <- case wCam w0 of
    Just cam | wUseCam w0 -> do
      ok <- camLive cam
      if ok then do f <- cameraFrame cam
                    pure (if B.length f >= 64 * 48 * 3 then Just (camGray64 f) else Nothing)
            else pure Nothing
    _ -> pure Nothing
  let (_, my) = mouseP env
      (gray, noise) = case camG of
        Just g -> (g, wNoise w0)
        Nothing -> (testScene (clampv (1 - d2 my / max 1 (d2 (envH env))) 0 1) (5000 + wNoise w0 `mod` 997), wNoise w0 + 1)
      x = features gray
      w1 = w0 { wGray = gray, wX = x, wNoise = noise, wLive = camG /= Nothing }
      w2 = if wBurst w1 > 0 && envFrame env `mod` 3 == 0
             then let exs = V.snoc (wExs w1) (Example x (wBurstCls w1) (grayPic gray))
                  in w1 { wExs = if V.length exs > maxEx then V.drop 1 exs else exs, wBurst = wBurst w1 - 1 }
             else w1
      w3 = if wLearn w2 && not (V.null (wExs w2)) then foldl' (\w _ -> learnOne w) w2 [1 .. passesPerFrame] else w2
  pure w3
  where
    learnOne w = let ex = wExs w V.! (wCursor w `mod` V.length (wExs w))
                     (wv, ch) = perceptronUpdate (wW w) (exX ex) (exY ex)
                 in w { wW = wv, wMist = take 200 ((if ch then 1 else 0) : wMist w), wCursor = wCursor w + 1 }

-- 16 × 12 のベクトルをマス目に（div = 重みの色、そうでなければ明るさ）
drawVec :: Vec -> Float -> Float -> Float -> Double -> Bool -> Picture
drawVec v x y cw scl dv = Pictures
  [ Color (col (if dv then divCol a else mixCol (10, 20, 22) ink ((a + 1) / 2))) (rectTL (x + i2f i * cw) (y + i2f j * cw) (cw - 1) (cw - 1))
  | j <- [0 .. gh12 - 1], i <- [0 .. gw16 - 1], let a = (v U.! (j * gw16 + i)) / scl ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Webcam perceptron: answer now " ++ (if score > 0 then "A (1)" else if score < 0 then "B (2)" else "?") ++ "   examples A " ++ show nA ++ ", B " ++ show (nEx - nA)
              ++ "   stored examples correct " ++ show okCnt ++ " / " ++ show nEx)
             ("1 teach A   2 teach B (10 frames each)   Space learning " ++ (if wLearn w then "ON" else "OFF") ++ "   R forget all   C "
              ++ (if wLive w then "camera" else "test image (mouse height = hand)") ++ "   S save")
  -- ---- 左：映像
  , imageBox x0 y0 vw vh 64 48 (grayPic (wGray w))
  , Color (col wire) (Pictures ([ Line [(x0 + i2f i * vw / i2f gw16, y0), (x0 + i2f i * vw / i2f gw16, y0 + vh)] | i <- [1 .. gw16 - 1] ]
                             ++ [ Line [(x0, y0 + i2f j * vh / i2f gh12), (x0 + vw, y0 + i2f j * vh / i2f gh12)] | j <- [1 .. gh12 - 1] ]))
  , if wBurst w > 0 then Color (col (if wBurstCls w > 0 then mag else lime)) (frameTL 6 x0 y0 vw vh) else Blank
  -- 答えのバー
  , Color (col panel) (rectTL x0 by vw 26)
  , Color (col (if sc > 0 then mag else lime)) (rectTL (min (x0 + vw / 2) (x0 + vw / 2 + f2 sc * vw / 2)) by (abs (f2 sc) * vw / 2) 26)
  , sayM ("w.x = " ++ fmtN score 1 ++ "   (right = A, left = B)") (x0 + vw / 2) (by + 13) 14 (col ink)
  -- ---- 中：入力と重み
  , say "input x (16 x 12, mean 0)" mx0 (y0 - 6) 14 (col ink)
  , drawVec (wX w) mx0 y0 cw 2.5 False
  , say "weights w (where it looks)" mx0 (y0 + i2f gh12 * cw + 40) 14 (col ink)
  , drawVec (wW w) mx0 (y0 + i2f gh12 * cw + 50) cw wm True
  , say "magenta = bright there -> A" mx0 (y0 + 2 * i2f gh12 * cw + 70) 14 (col sub)
  , say "lime = bright there -> B" mx0 (y0 + 2 * i2f gh12 * cw + 88) 14 (col sub)
  -- ---- 右：例と学習のようす
  , say "taught examples (latest A / B)" rx (y0 - 6) 14 (col ink)
  , Pictures [ thumb e (rx + i2f (q `mod` 3) * (tw + 8)) (y0 + i2f (q `div` 3) * (tw * 0.75 + 8)) mag | (q, e) <- zip [0 :: Int ..] showA ]
  , Pictures [ thumb e (rx + i2f (q `mod` 3) * (tw + 8)) (y0 + 2 * (tw * 0.75 + 8) + 20 + i2f (q `div` 3) * (tw * 0.75 + 8)) lime | (q, e) <- zip [0 :: Int ..] showB ]
  , say ("corrections in the last 200 tries " ++ fmtN (100 * mr) 1 ++ "%") rx ry 14 (col ink)
  , Pictures [ say s rx (ry + 24 + 18 * i2f q) 14 (col sub)
             | (q, s) <- zip [0 :: Int ..] [ "If A and B can be split by a hyperplane,", "corrections stop within (R/gamma)^2 (theorem of 00)"
                                         , "Similar frames taught as both A and B: never stops", "192 inputs + bias = 193 weights" ] ]
  , drawSelfTest env (wTest w) ]
  where
    score = dotV (wW w) (wX w)
    exs = V.toList (wExs w)
    nEx = length exs
    nA = length (filter ((> 0) . exY) exs)
    okCnt = length [ () | e <- exs, i2d (exY e) * dotV (wW w) (exX e) > 0 ]
    vw = min (envW env * 0.36) ((envH env - 200) * (4 / 3))
    vh = vw * 0.75
    x0 = 40
    y0 = 100
    by = y0 + vh + 30
    sc = clampv (score / (abs score + 50)) (-1) 1
    mx0 = x0 + vw + 40
    cw = min 16 ((envW env - mx0 - 300) / i2f gw16)
    wm = maxOr 1e-9 (1e-9 : map abs (U.toList (U.take nIn (wW w))))
    rx = mx0 + i2f gw16 * cw + 40
    lastN k xs = drop (length xs - k) xs
    showA = lastN 6 (filter ((> 0) . exY) exs)
    showB = lastN 6 (filter ((< 0) . exY) exs)
    tw = min 80 ((envW env - rx - 40) / 3 - 8)
    thumb e x y c = Pictures [imageBox x y tw (tw * 0.75) 64 48 (exPic e), Color (col c) (frameTL 2 x y tw (tw * 0.75))]
    ry = y0 + 4 * (tw * 0.75 + 8) + 50
    mr = if null (wMist w) then 0 else meanOf (map i2d (wMist w))

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure w { wBurst = burst, wBurstCls = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wBurst = burst, wBurstCls = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wLearn = not (wLearn w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> pure w { wW = U.replicate (nIn + 1) 0, wExs = V.empty, wMist = [], wCursor = 0 }
    | c `elem` "cC" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> do saveSvg "webcam_perceptron" env (draw env w); pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 手をあげて 1、おろして 2 を何回か。重みの画像のどこが濃くなる？
-- 2. 顔を左右に動かすなど、別の 2 つを教えてみる。何が見分けやすくて、何が難しい？
-- 3. A と B に同じような映像を教えると、修正の割合はどうなる？（分けられないと止まらない）
-- 4. gw16 × gh12 を 8 × 6 にすると、学習は速くなる？ 見分けられなくなる？（features の 4 も変える）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("真っ平らな画像：入力は全部 0（標準偏差 0 でも NaN にならない）、最後はバイアスの 1", U.all (== 0) (U.take nIn flat) && flat U.! nIn == 1)
  , ("入力は平均 0・標準偏差 1 にそろう（" ++ fmtE m ++ "、" ++ fmtN sd 6 ++ "）", abs m < 1e-12 && near sd 1 1e-9)
  , ("テスト画像 60 枚（手をあげる / おろす）：" ++ show epochs ++ " 周で止まり、全部正しい", conv && gam > 0)
  , ("修正の回数 " ++ show k ++ " ≤ (R/γ)² = " ++ fmtN ((r / gam) ** 2) 0 ++ "（γ は止まった重みで測る）", i2d k <= (r / gam) ** 2)
  , ("学習に使っていない 40 枚の正解率 " ++ fmtN (100 * acc) 0 ++ "% ≥ 95%", acc >= 0.95)
  , ("重みの画像：手が上がる場所（右上）は A（+）、手が下がる場所は B（−）", up > 0 && low < 0) ]
  where
    flat = features (U.replicate (64 * 48) 120)
    f = U.toList (U.take nIn (features (testScene 0.3 1)))
    m = meanOf f
    sd = sqrt (meanOf (map (\v -> (v - m) ** 2) f))
    tr = [ (features (testScene (if odd k then 0.55 + 0.45 * i2d ((k * 7) `mod` 10) / 10 else 0.45 * i2d ((k * 3) `mod` 10) / 10) (100 + k)), if odd k then 1 else -1) | k <- [0 .. 59 :: Int] ]
    te = [ (features (testScene (if odd k then 0.6 + 0.4 * i2d ((k * 3) `mod` 10) / 10 else 0.4 * i2d ((k * 7) `mod` 10) / 10) (900 + k)), if odd k then 1 else -1) | k <- [0 .. 39 :: Int] ]
    (wt, k, epochs, conv) = trainPerceptron (U.replicate (nIn + 1) 0) tr 200
    r = maximum (map (normV . fst) tr)
    nw = normV wt
    gam = minimum [ (i2d y * dotV wt x) / nw | (x, y) <- tr ]
    acc = i2d (length (filter (\(x, y) -> i2d y * dotV wt x > 0) te)) / i2d (length te)
    up = sumOf [ wt U.! (j * gw16 + i) | j <- [0 .. 3], i <- [10 .. 12] ]
    low = sumOf [ wt U.! (j * gw16 + i) | j <- [6 .. 8], i <- [10 .. 12] ]

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

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

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
