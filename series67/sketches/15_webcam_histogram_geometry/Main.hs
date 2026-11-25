-- ============================================
-- 67-15 カメラ映像の色の分布 📷 ─ 画面を 8×6 のブロックに分け、ブロックごとに「色の分布」（8 色のヒストグラム）を作る
-- R で記録した基準の分布とのフィッシャー-ラオ距離 d = 2 arccos(Σ√(p q))（13 の球の上の大円）をヒートマップに
-- 動いた所・照明が変わった所が「分布の距離」で光る。K で KL ダイバージェンスに切りかえ（数え始めに 0.5 を足して log 0 を防ぐ）
-- 操作：R 基準を記録 / K 距離 ⇔ KL / ↑↓ 色の濃さ / C カメラ ⇔ テスト画像 / S 画面を保存 / マウスを映像に乗せる = そのブロックの分布
-- カメラは Art.Device（64 × 48、左右反転）。カメラがないときはテスト画像。画素とヒストグラムは unboxed ベクトル
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import qualified Data.ByteString as B
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

camW', camH', nbx, nby :: Int
camW' = 64                                    -- ★ 映像の大きさとブロックの数
camH' = 48
nbx = 8
nby = 6

binNames :: [String]
binNames = ["blk", "gray", "wht", "red", "yel", "grn", "blue", "pur"]   -- 黒・灰・白・赤・黄・緑・青・紫

binCols :: [RGB]
binCols = [(70, 70, 80), (150, 150, 160), (235, 235, 235), (235, 80, 70), (235, 210, 70), (90, 200, 110), (80, 130, 235), (180, 100, 230)]

pseudo :: Double
pseudo = 0.5                                  -- ★ 各色に最初から足しておく数（0 の色をなくす）

type Pix = U.Vector Int                       -- 画素の列（R G B をくり返す、左上から）
type Hists = U.Vector Double                  -- 48 個のヒストグラム × 8 色（ブロック k の色 i は k * 8 + i）

data World = World
  { wCam      :: Maybe Camera
  , wUseCam   :: !Bool
  , wLive     :: !Bool
  , wLastLive :: !(Maybe Bool)
  , wPix      :: !Pix
  , wRef      :: !(Maybe Hists)
  , wCur      :: !(Maybe Hists)
  , wSeen     :: !Int
  , wKL       :: !Bool
  , wGain     :: !Double
  , wFlash    :: !Int
  , wTest     :: [Check] }

main :: IO ()
main = runSketch (sketch "67-15 Webcam color distributions" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera camW' camH'          -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World Nothing True False Nothing (testFrame 0) Nothing Nothing 0 False 1 0 []

-- ★ 1 画素の色 → 8 色のどれか（暗い = 黒、色がうすい = 灰か白、それ以外は色相で 5 色）
colorBin :: Int -> Int -> Int -> Int
colorBin r g b
  | mx < 50 = 0
  | mx - mn < 40 = if mx < 160 then 1 else 2
  | otherwise = let d = i2d (mx - mn)
                    h0 | mx == r = 60 * i2d (g - b) / d
                       | mx == g = 120 + 60 * i2d (b - r) / d
                       | otherwise = 240 + 60 * i2d (r - g) / d
                    h = jsMod (h0 + 360) 360
                in if h < 30 || h >= 330 then 3 else if h < 90 then 4 else if h < 180 then 5 else if h < 270 then 6 else 7
  where mx = max r (max g b)
        mn = min r (min g b)

jsMod :: Double -> Double -> Double           -- JavaScript の %（符号は左の数と同じ）
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Int)

-- 画素の列 → 48 個のヒストグラム（合計 1）
blockHists :: Pix -> Hists
blockHists px = U.concat [ normB (U.slice (k * 8) 8 counts) | k <- [0 .. nbx * nby - 1] ]
  where
    bw = camW' `div` nbx
    bh = camH' `div` nby
    idx = [ (y `div` bh * nbx + x `div` bw) * 8 + colorBin (px U.! o) (px U.! (o + 1)) (px U.! (o + 2))
          | y <- [0 .. camH' - 1], x <- [0 .. camW' - 1], let o = 3 * (y * camW' + x) ]
    counts = U.accum (+) (U.replicate (nbx * nby * 8) pseudo) [ (i, 1) | i <- idx ]
    normB v = let s = U.foldl' (+) 0 (U.map (max 0) v) in if s > 0 then U.map (\c -> max 0 c / s) v else U.replicate 8 (1 / 8)

blockOf :: Hists -> Int -> [Double]
blockOf h k = if U.length h >= (k + 1) * 8 && k >= 0 then U.toList (U.slice (k * 8) 8 h) else replicate 8 (1 / 8)

-- ---- テスト画像（カメラがないとき）：色の帯 ＋ 動く丸 ＋ ゆっくり変わる明るさ（値は 0〜255 に丸める）
testFrame :: Double -> Pix
testFrame t = U.generate (camW' * camH' * 3) px
  where
    cx = 32 + 22 * cos (t * 0.6)
    cy = 24 + 12 * sin (t * 0.9)
    lum = 0.75 + 0.25 * sin (t * 0.25)
    px i = let (j, ch) = i `divMod` 3
               (y, x) = j `divMod` camW'
               band = x `div` 16
               c0 = at' [(200, 60, 50), (60, 160, 80), (60, 90, 200), (220, 200, 70)] band (0, 0, 0)
               c1 = if y > 36 then (200, 200, 200) else c0
               c = if (i2d x - cx) ^ (2 :: Int) + (i2d y - cy) ^ (2 :: Int) < 49 then (170, 70, 200) else c1
               (r, g, b) = c :: (Double, Double, Double)
               v = case ch of
                     0 -> r
                     1 -> g
                     _ -> b
           in max 0 (min 255 (round (v * lum)))  -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）

camPix :: B.ByteString -> Pix
camPix bs = U.generate (camW' * camH' * 3) (\i -> fromIntegral (B.index bs i))

distOf :: Bool -> [Double] -> [Double] -> Double
distOf useKL p q = if useKL then divKL p q else fisherRao p q

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  live <- case wCam w0 of
    Just cam | wUseCam w0 -> camLive cam
    _ -> pure False
  -- カメラが動き出した・切りかえた → 基準を取り直す
  let w1 = if Just live /= wLastLive w0 then w0 { wLastLive = Just live, wSeen = 0, wCur = Nothing } else w0
  px <- case wCam w1 of
    Just cam | live -> do
      f <- cameraFrame cam
      pure (if B.length f >= camW' * camH' * 3 then camPix f else testFrame (d2 (envTime env)))
    _ -> pure (testFrame (d2 (envTime env)))
  let now = blockHists px
      cur = case wCur w1 of
        Just h -> U.zipWith (\v n -> 0.6 * v + 0.4 * n) h now   -- 少しなめらかに（ちらつき防止）
        Nothing -> now
      -- 最初の 0.5 秒は基準を更新（カメラの立ち上がりを待つ）
      (ref, seen) = case wRef w1 of
        Nothing -> (cur, wSeen w1)
        Just r -> (if wSeen w1 < 30 then cur else r, wSeen w1 + 1)
  pure w1 { wLive = live, wPix = px, wCur = Just cur, wRef = Just ref, wSeen = seen, wFlash = max 0 (wFlash w1 - 1) }

-- 映像の場所（左上 vx, vy と幅・高さ）
videoBox :: Env -> Box
videoBox env = let vw = min (d2 (envW env) * 0.55) ((d2 (envH env) - 170) * (4 / 3)) in Box 30 90 vw (vw * 0.75)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Color distribution of the video: " ++ (if wKL w then "KL(now||ref)" else "Fisher-Rao distance") ++ " (" ++ (if wLive w then "camera" else "test image") ++ ")")
             ("R record reference  K distance <-> KL  Up/Down color gain x" ++ fmtN 1 (wGain w) ++ "  C camera <-> test image  S save  Mouse on video = that block")
  -- ---- 左：映像とヒートマップ
  , imageBox vx vy vw vh camW' camH' (pixelImage camW' camH' 1 (\x y -> let o = 3 * (y * camW' + x) in (w8 (wPix w U.! o), w8 (wPix w U.! (o + 1)), w8 (wPix w U.! (o + 2)))))
  , Pictures [ let t = f2 (clampv (d * wGain w / dmax) 0 1)
                   x = vx + i2f (k `mod` nbx) * cw
                   y = vy + i2f (k `div` nbx) * chh
               in Pictures [ Color (colA (200 * t) (255, 150 * (1 - 0.4 * t), 75 * (1 - t))) (rectTL x y cw chh)
                           , Color (colA 40 ink) (frameTL 1 x y cw chh)
                           , sayT (fmtN 2 d) (x + 3) (y + 3) 14 (colA (120 + 135 * t) ink) False ]
             | (k, d) <- zip [0 ..] ds ]
  , if wFlash w > 0 then Color (colA (i2f (wFlash w) * 4) lime) (rectTL vx vy vw vh) else Blank
  -- ---- 右：ブロックの分布
  , Color (col lime) (frameTL 3 (vx + i2f (sel `mod` nbx) * cw) (vy + i2f (sel `div` nbx) * chh) cw chh)
  , drawBars p (Box (d2 rx) 130 (d2 rw) 150) barOpt { boNames = binNames, boGhost = q, boCols = \i -> at' binCols i ink }
  , say ("Block " ++ show sel ++ " colors (white frame = reference)") rx 112 15 (col ink)
  , say ("Fisher-Rao d = " ++ fmtN 4 (fisherRao p q)) rx 330 15 (col ink)
  , say ("KL(now||ref) = " ++ fmtN 4 (divKL p q)) rx 354 15 (col ink)
  , say ("KL(ref||now) = " ++ fmtN 4 (divKL q p)) rx 376 15 (col ink)
  , say ("Hellinger sum(sqrt p - sqrt q)^2 = " ++ fmtN 4 (hellingerSq p q)) rx 400 14 (col dimc)
  , say ("Total variation = " ++ fmtN 4 (tvDist p q)) rx 420 14 (col dimc)
  , say "d is a true distance in 0..pi (triangle" rx 444 14 (col dimc)
  , say "  inequality OK); KL has a direction" rx 462 14 (col dimc)
  , say ("Whole-frame distance d = " ++ fmtN 4 (fisherRao allC allR)) rx 488 14 (col dimc)
  , say "  (motions can cancel out)" rx 506 14 (col dimc)
  , say "Lighting change: every block glows a little;" rx 530 14 (col dimc)
  , say "  motion: one block glows strongly" rx 548 14 (col dimc)
  , drawSelfTest env (wTest w) ]
  where
    vb = videoBox env
    vx = f2 (bxX vb)
    vy = f2 (bxY vb)
    vw = f2 (bxW vb)
    vh = f2 (bxH vb)
    cw = vw / i2f nbx
    chh = vh / i2f nby
    w8 :: Int -> Word8
    w8 = fromIntegral . max 0 . min 255
    zeros = U.replicate (nbx * nby * 8) 0
    cur = maybe zeros id (wCur w)
    ref = maybe zeros id (wRef w)
    ds = [ distOf (wKL w) (blockOf cur k) (blockOf ref k) | k <- [0 .. nbx * nby - 1] ]
    dmax = if wKL w then 1.5 else 1.2
    (mx, my) = mouseD env
    sel = if inRect mx my vb
            then jsRound (clampv (i2d (floor ((my - bxY vb) / d2 chh) :: Int)) 0 (i2d nby - 1)) * nbx + jsRound (clampv (i2d (floor ((mx - bxX vb) / d2 cw) :: Int)) 0 (i2d nbx - 1))
            else let m = maxOr 0 ds in length (takeWhile (/= m) ds)
    rx = vx + vw + 40
    rw = envW env - rx - 30
    p = blockOf cur sel
    q = blockOf ref sel
    sumAll h = normP (foldl' (zipWith (+)) (replicate 8 0) [ blockOf h k | k <- [0 .. nbx * nby - 1] ])
    allC = sumAll cur
    allR = sumAll ref

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wGain = min 5 (wGain w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGain = max 0.2 (wGain w / 1.25) }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> pure w { wRef = wCur w, wFlash = 40 }
    | c == 'k' || c == 'K' -> pure w { wKL = not (wKL w) }
    | c == 'c' || c == 'C' -> pure w { wUseCam = not (wUseCam w), wCur = Nothing, wRef = Nothing }
    | c == 's' || c == 'S' -> do saveSvg "color_distribution" env (draw env w); pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 手をふって、どのブロックが光るか見よう。R で基準を取り直すと？
-- 2. 部屋の明かりを少し暗くすると、全部のブロックが同時に変わる？（テスト画像でも明るさがゆっくり変わる）
-- 3. K で KL に切りかえよう。基準に「なかった色」が現れると KL は大きくなりやすい。なぜ？
-- 4. pseudo を 0.01 にすると KL はどう変わる？ 0 にするとどうなる？（log 0 に注意）
-- 5. nbx, nby を 16, 12 にすると細かくなるが、1 ブロックの画素は 16 個だけ。分布はどれくらい信用できる？

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("48 個のヒストグラムはどれも合計 1、0 の色はない（数え始め 0.5）", all (\h -> abs (sumOf h - 1) < 1e-12 && all (> 0) h) blocksA)
  , ("色の判定：黒・灰・白・赤・黄・緑・青・紫"
    , and [ colorBin r g b == i | (i, (r, g, b)) <- zip [0 ..] [(10, 10, 10), (120, 120, 125), (240, 240, 235), (220, 40, 30), (220, 210, 40), (40, 200, 60), (40, 80, 220), (170, 60, 210)] ])
  , ("d は向きによらず 0〜π、8 色でも d = 2 × 単位球の大円の長さ（誤差 1e-5）", okSym && okArc)
  , ("同じ分布なら d = 0、KL = 0", all (\h -> fisherRao h h < 1e-6 && divKL h h == 0) blocksA)
  , ("テスト画像（明るさが同じ 2 つの時刻）：丸が動いたブロックの距離 " ++ fmtN 2 moved ++ " は、動かない下の帯 " ++ fmtN 3 still ++ " よりずっと大きい", moved > 0.5 && still < 0.05)
  , ("数え始め 0.5 のおかげで KL はどこでも有限", all (\(a, b) -> not (isInfinite (divKL a b) || isNaN (divKL a b))) (zip blocksA blocksB)) ]
  where
    ha = blockHists (testFrame 0)
    hb = blockHists (testFrame (8 * pi))
    blocksA = [ blockOf ha k | k <- [0 .. nbx * nby - 1] ]
    blocksB = [ blockOf hb k | k <- [0 .. nbx * nby - 1] ]
    pairs = withSeed 15 (mapM (\_ -> (,) <$> rndDist 8 0.01 <*> rndDist 8 0.01) [1 .. 100 :: Int])
    res = [ (abs (d - fisherRao q p) <= 1e-15 && d >= 0 && d <= pi, abs (2 * arcL - d) <= 1e-5)
          | (p, q) <- pairs
          , let d = fisherRao p q
          , let sp = map sqrt p
          , let sq = map sqrt q
          , let om = acos (clampv (bhatta p q) (-1) 1)
          , let pts = sp : [ zipWith (\v u -> (sin ((1 - t) * om) * v + sin (t * om) * u) / sin om) sp sq | j <- [1 .. 500 :: Int], let t = i2d j / 500 ]
          , let arcL = sumOf [ sqrt (sumOf (zipWith (\c pv -> (c - pv) ^ (2 :: Int)) b a)) | (a, b) <- zip pts (drop 1 pts) ] ]
    okSym = all fst res
    okArc = all snd res
    ds = zipWith fisherRao blocksA blocksB
    still = at' ds 44 0
    moved = maxOr 0 ds

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

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

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 棒グラフ（ドラッグで高さを変えられる）
data BarOpt = BarOpt { boCols :: Int -> RGB, boNames :: [String], boVmax :: !Double, boGhost :: [Double] }

barOpt :: BarOpt                              -- 既定：結果の色、名前は 1 2 3 …、上限 1、重ねる分布なし
barOpt = BarOpt outC [] 1 []

drawBars :: [Double] -> Box -> BarOpt -> Picture
drawBars p r opt = Pictures $
  Color (col wire) (lineW 1 (f2 (bxX r), f2 (bxY r + bxH r)) (f2 (bxX r + bxW r), f2 (bxY r + bxH r)))
  : concat [ [ Color (colA 210 (boCols opt i)) (rectTL (x + i2f i * bw + 3) (y + h - hh) (bw - 6) hh)
             , if null (boGhost opt) then Blank
               else let gh = f2 (clampv (at' (boGhost opt) i 0) 0 vmax / vmax) * h
                    in Color (col ink) (frameTL 1.5 (x + i2f i * bw + 3) (y + h - gh) (bw - 6) gh)
             , if bw >= 34 then sayC (fmtN 2 v) (x + (i2f i + 0.5) * bw) (y + h - hh - 4) 14 (col ink) else Blank   -- 細い棒では数字を省く
             , sayC (if null (boNames opt) then show (i + 1) else at' (boNames opt) i "") (x + (i2f i + 0.5) * bw) (y + h + 17) 14 (col dimc) ]
           | (i, v) <- zip [0 ..] p, let hh = f2 (clampv v 0 vmax / vmax) * h ]
  where k = length p
        x = f2 (bxX r)
        y = f2 (bxY r)
        h = f2 (bxH r)
        bw = f2 (bxW r) / i2f (max 1 k)
        vmax = boVmax opt

-- 棒をドラッグ：i 番目の高さを変えて、残りを比例で縮める（合計 1 のまま）
barsDrag :: [Double] -> Box -> Double -> Double -> Double -> [Double]
barsDrag p r mx my fl0 =
  let k = length p
      fl = if fl0 == 0 then 0.001 else fl0
      i = floor ((mx - bxX r) / (bxW r / i2d k)) :: Int
      v = clampv ((bxY r + bxH r - my) / bxH r) fl (1 - fl * i2d (k - 1))
      rest = 1 - at' p i 0
      q = [ if j == i then v else if rest > 1e-9 then (pj / rest) * (1 - v) else (1 - v) / i2d (k - 1) | (j, pj) <- zip [0 ..] p ]
  in if i < 0 || i >= k || my < bxY r - 30 || my > bxY r + bxH r + 20 then p else mixFloor q fl

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
