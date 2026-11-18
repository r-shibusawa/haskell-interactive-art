-- ============================================
-- 52-15 知覚の尺度でカメラ映像 📷 ─ 明るさを「物理的」「フェヒナー的」「スティーヴンス的」に段階化
-- 画素の光の量 Y（0〜1）を K 段に分ける（ポスタリゼーション）。分け方は 3 通り：
--   1 物理的に等間隔（Y で等分）　2 フェヒナー（log Y で等分）　3 スティーヴンス（Y^(1/3) で等分 ≒ L*）
-- どの分け方だと、となりの段の境目が「同じくらいの差」に見える？ 右の帯と映像で比べる
-- 4 = JND より小さい差を誇張（まわりとの差が JND（k·Y）より小さい所だけ G 倍に広げる）
-- 操作：1〜4 = 表示 / ↑↓ = 段の数 K / ←→ = 誇張の倍率 G / V = カメラ ⇔ テスト画像 / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, ch :: Int
cw = 160                             -- カメラの大きさ（小さくして計算を軽く）
ch = 120

yMin, jndK :: Double
yMin = 0.005                         -- ★ いちばん暗い光の量（log の下限）
jndK = 0.03                          -- ★ 明るさの JND の比（ウェーバー比）

names :: Int -> String
names 1 = "Physical, equal steps (Y)"
names 2 = "Fechner (log Y)"
names 3 = "Stevens (Y^1/3)"
names _ = "Exaggerate differences below the JND"

data World = World
  { wK      :: !Int              -- ★ 段の数
  , wG      :: !Int              -- ★ 誇張の倍率
  , wView   :: !Int              -- 1〜4
  , wUseCam :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wFrame  :: !B.ByteString     -- 元のカメラ映像（小さく表示する）
  , wOut    :: !(U.Vector Word8) -- 段階化した明るさ（0〜255）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-15 Perceptual scales on camera" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw ch
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 6 8 1 True Nothing False B.empty (U.replicate (cw * ch) 0) []

-- 尺度：光の量 Y → 0〜1 の「感じの目盛り」と、その逆
toScale :: Double -> Int -> Double
toScale y0 v
  | v == 2 = log (y / yMin) / log (1 / yMin)
  | v == 3 = (cbrt y - cbrt yMin) / (1 - cbrt yMin)
  | otherwise = (y - yMin) / (1 - yMin)
  where y = clampv yMin 1 y0

fromScale :: Double -> Int -> Double
fromScale s0 v
  | v == 2 = yMin * (1 / yMin) ** s
  | v == 3 = (cbrt yMin + s * (1 - cbrt yMin)) ** 3
  | otherwise = yMin + s * (1 - yMin)
  where s = clampv 0 1 s0

-- 0〜k−1 段目
levelOf :: Double -> Int -> Int -> Int
levelOf y v k = min (k - 1) (floor (toScale y v * fromIntegral k))

-- 段の代表の光の量（目盛りでまん中）
levelY :: Int -> Int -> Int -> Double
levelY i v k = fromScale ((fromIntegral i + 0.5) / fromIntegral k) v

boundaries :: Int -> Int -> [Double]
boundaries v k = [ fromScale (fromIntegral i / fromIntegral k) v | i <- [1 .. k - 1] ]

-- JND より小さい差を誇張：まわり 3 × 3 の平均 m との差 d が k·m より小さければ m + G·d
exaggerate :: Double -> Double -> Double -> Double -> Double
exaggerate y m k g = let d = y - m in if abs d < k * m then clampv 0 1 (m + g * d) else y

-- 0〜255 → 光の量（毎回計算しないための表）
lut :: U.Vector Double
lut = U.generate 256 (srgbToLin . fromIntegral)

-- 0〜255 の小数を画素の値に（Uint8ClampedArray と同じ：切りつめて、.5 は偶数へ丸める）
toByte :: Double -> Word8
toByte v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- テスト画像：左から右へなめらかな明るさ、下半分に 2% だけ明るい小さな四角（JND くらいの差）。灰色なので 1 画素 1 バイト
testImage :: U.Vector Word8
testImage = U.generate (cw * ch) $ \i ->
  let (y, x) = i `divMod` cw
      y1 = yMin + (1 - yMin) * (fromIntegral x / fromIntegral (cw - 1)) ** 2.2
      y2 = if fromIntegral y > (fromIntegral ch / 2 :: Double) && (x `div` 20 + y `div` 20) `mod` 2 == 0 then y1 * 1.02 else y1
      y3 = if sqrt ((fromIntegral x - fromIntegral cw * 0.3) ^ (2 :: Int) + (fromIntegral y - fromIntegral ch * 0.3) ^ (2 :: Int)) < (18 :: Double) then 0.03 else y2
  in toByte (linToSrgb y3)

-- テスト画像の光の量（p5.js 版と同じく Float32 で持つ）
testYs :: U.Vector Float
testYs = U.map (\b -> let l = lut U.! fromIntegral b in realToFrac (0.2126 * l + 0.7152 * l + 0.0722 * l)) testImage

-- 画素ごとの光の量 Y → 表示する明るさ
render :: Int -> Int -> Int -> U.Vector Float -> U.Vector Word8
render v k g ys = U.generate (cw * ch) px
  where
    yAt i = realToFrac (ys U.! i) :: Double
    px i
      | v == 4 =
          let (y, x) = i `divMod` cw
              nb = [ yAt (yy * cw + xx) | dy <- [-1, 0, 1], dx <- [-1, 0, 1], let xx = x + dx, let yy = y + dy, xx >= 0, yy >= 0, xx < cw, yy < ch ]
              m = foldl' (+) 0 nb / fromIntegral (length nb)
          in toByte (linToSrgb (exaggerate (yAt i) m jndK (fromIntegral g)))
      | otherwise = toByte (linToSrgb (levelY (levelOf (yAt i) v k) v k))

step :: Env -> Float -> World -> IO World
step _ _ w = do
  src <- case wCam w of
    Just cam | wUseCam w -> do
      live <- camLive cam
      if not live then pure Nothing else do
        fr <- cameraFrame cam
        pure (if B.length fr >= cw * ch * 3 then Just fr else Nothing)
    _ -> pure Nothing
  -- 画素ごとに光の量 Y（sRGB を線形にもどして重みづけ）
  let ys = case src of
        Just fr -> U.generate (cw * ch) (\i -> let c o = lut U.! fromIntegral (BU.unsafeIndex fr (3 * i + o))
                                               in realToFrac (0.2126 * c 0 + 0.7152 * c 1 + 0.0722 * c 2))
        Nothing -> testYs
      out = render (wView w) (wK w) (wG w) ys
  pure (out `seq` w { wOut = out, wLive = src /= Nothing, wFrame = maybe B.empty id src })

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (x0 + iw / 2, y0 + ih / 2) (Scale 1 (-1) (pixelImage cw ch sc (\x y -> let v = wOut w U.! (y * cw + x) in (v, v, v)))) ]
  -- 小さく元の映像
  ++ [ at (x0 + iw - fromIntegral cw / 2 - 8, y0 + 8 + fromIntegral ch / 2)
          (Scale 1 (-1) (pixelImage cw ch 1 (\x y -> let o = 3 * (y * cw + x) in (BU.unsafeIndex fr o, BU.unsafeIndex fr (o + 1), BU.unsafeIndex fr (o + 2)))))
     | wLive w, let fr = wFrame w, B.length fr >= cw * ch * 3 ]
  ++ [ say ((if wLive w then "Camera" else "Test image (V: camera)") ++ "   1-3 scale   4 exaggerate (G = " ++ show (wG w) ++ ")") 20 30 18 (gray255 235)
     , say "Up/Down number of steps   Left/Right gain G   V switch   S save" 20 54 16 (gray255 235)
     , say ("Now: " ++ names (wView w) ++ (if wView w == 4 then "   only where the difference to the surround is below "
                                                                  ++ fmt 0 (jndK * 100) ++ "%, x " ++ show (wG w)
                                                              else "   equal physical steps squash the dark parts into one step")) 20 78 14 (gray255 170) ]
    -- ---- 3 つの尺度の段の帯
  ++ [ say ("Steps K = " ++ show k) bx (by - 12) 15 (gray255 235) ]
  ++ concat [ let yy = by + fromIntegral (v - 1) * 86
              in say (show v ++ " " ++ names v) bx (yy + 16) 14 (if wView w == v then rgb255 255 210 120 else gray255 200)
                 : [ Color (gray255 (realToFrac (linToSrgb (levelY i v k)))) (rectTL (bx + fromIntegral i * bw / fromIntegral k) (yy + 24) (bw / fromIntegral k + 1) 40) | i <- [0 .. k - 1] ]
            | v <- [1 .. 3] ]
    -- 境目の位置（光の量 Y の軸）
  ++ [ Color (gray255 110) (Line [(bx, ay), (bx + bw, ay)])
     , say "0" bx (ay + 18) 14 (gray255 150), sayR "Y = 1" (bx + bw) (ay + 18) 14 (gray255 150) ]
  ++ concat [ [ Color c (rectTL (bx + f2 b * bw - 1) (ay - 8 - fromIntegral v * 10) 2 9) | b <- boundaries v k ]
            | (v, c) <- zip [1 .. 3] [rgb255 120 190 255, rgb255 220 130 255, rgb255 255 170 90] ]
  ++ [ say "Boundaries in light Y:" bx (ay + 40) 14 (gray255 170)
     , say "blue physical / purple Fechner / orange Stevens" bx (ay + 60) 14 (gray255 170)
     , say "Which bands look evenly spaced?" bx (ay + 90) 14 (gray255 170)
     , drawSelfTest env (wTest w) ]
  where
    k = wK w
    sc = min (envW env * 0.62 / fromIntegral cw) ((envH env - 260) / fromIntegral ch)
    iw = fromIntegral cw * sc
    ih = fromIntegral ch * sc
    x0 = 20
    y0 = 90
    bx = x0 + iw + 30
    bw = envW env - bx - 30
    by = 130
    ay = by + 280

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wK = min 16 (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wK = max 2 (wK w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wG = min 30 (wG w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wG = max 1 (wG w - 1) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "perceptual_camera" env (draw env w) >> pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("physical: boundaries equally spaced in Y", near (b1 1 - b1 0) (b1 2 - b1 1) 1e-12 && near (b1 0) (yMin + (1 - yMin) / 4) 1e-12)
  , ("Fechner: boundaries in equal ratios (equal steps in log)", near (b2 1 / b2 0) (b2 2 / b2 1) 1e-9 && near (b2 0) (yMin * (1 / yMin) ** 0.25) 1e-12)
  , ("Stevens: Y^(1/3) of the boundaries equally spaced", near (cbrt (b3 1) - cbrt (b3 0)) (cbrt (b3 2) - cbrt (b3 1)) 1e-12)
  , ("darkest -> step 0, Y = 1 -> step K - 1 (never outside)", all (\v -> levelOf 0 v 6 == 0 && levelOf 1 v 6 == 5) [1, 2, 3])
  , ("toScale and fromScale are inverses (all 3)", all (\v -> near (toScale (fromScale 0.37 v) v) 0.37 1e-12) [1, 2, 3])
  , ("sRGB 128 -> Y = 0.216 -> back to 128", near (lut U.! 128) 0.21586 1e-5 && near (linToSrgb (lut U.! 128)) 128 1e-9)
  , ("exaggerate: a difference below the JND (1%) x G, a large one (10%) unchanged", near (exaggerate 0.505 0.5 0.03 8) 0.54 1e-12 && near (exaggerate 0.55 0.5 0.03 8) 0.55 1e-12) ]
  where
    b1 = getAt 0 (boundaries 1 4)
    b2 = getAt 0 (boundaries 2 4)
    b3 = getAt 0 (boundaries 3 4)

-- ---------- やってみよう ----------
-- 1. 1〜3 を切りかえて、自分の顔の影の部分がいちばん自然に段になるのはどれか比べよう
-- 2. ↑↓ で K を 3 や 12 に。物理的に等分すると、暗い所は何段に分かれる？
-- 3. 4 の誇張で、テスト画像の下半分の「2% だけ明るい四角」は見えるようになった？
-- 4. jndK を 0.01 や 0.1 にすると、誇張される所はどう変わる？
-- 5. S で保存して、ポスター風の作品にしよう。段ごとに好きな色をつけてもよい

-- ---- 光の量の道具
-- 立方根（Math.cbrt）
cbrt :: Double -> Double
cbrt x = if x < 0 then -((-x) ** (1 / 3)) else x ** (1 / 3)

-- sRGB の値（0〜255）→ 光の量（0〜1）と、その逆
srgbToLin :: Double -> Double
srgbToLin c0 = let c = c0 / 255 in if c <= 0.04045 then c / 12.92 else ((c + 0.055) / 1.055) ** 2.4

linToSrgb :: Double -> Double
linToSrgb v0 = let v = clampv 0 1 v0 in 255 * (if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055)

-- ==================================================
-- ここから下は「心理物理の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
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

-- フィッシャー＝イェーツのシャッフル（後ろから順に入れかえる）
shuffleArr :: [a] -> Rand [a]
shuffleArr xs = go (V.fromList xs) (length xs - 1)
  where
    go v i | i <= 0 = pure (V.toList v)
           | otherwise = do
               u <- rnd
               let j = floor (u * fromIntegral (i + 1))
               go (v V.// [(i, v V.! j), (j, v V.! i)]) (i - 1)

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Rng
freshSeed = seedRng <$> randomRIO (1, maxBound)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の Math.round（.5 は大きい方へ）
roundJ :: Double -> Double
roundJ x = fromIntegral (floor (x + 0.5) :: Int)

-- ---- 正規分布：Φ（累積分布）と z（その逆関数。二分法で数値的に解く）
-- 誤差関数（Abramowitz & Stegun 7.1.26、誤差 1.5e-7 以下）
erfA :: Double -> Double
erfA x0 = s * y
  where
    s = if x0 < 0 then -1 else 1
    x = abs x0
    t = 1 / (1 + 0.3275911 * x)
    y = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-x * x)

phi :: Double -> Double
phi x = 0.5 * (1 + erfA (x / sqrt 2))

-- Φ(z) = p となる z
zOf :: Double -> Double
zOf p0 = go (-9) 9 (70 :: Int)
  where
    p = clampv 1e-9 (1 - 1e-9) p0
    go lo hi 0 = (lo + hi) / 2
    go lo hi n = let m = (lo + hi) / 2 in if phi m < p then go m hi (n - 1) else go lo m (n - 1)

-- 割合を 1/(2N) で補正（0 や 1 だと z が無限大になるので）
rateFix :: Double -> Double -> Double
rateFix k n = if n > 0 then clampv (1 / (2 * n)) (1 - 1 / (2 * n)) (k / n) else 0.5

-- ---- 上下法（階段法）：nDown 回続けて正答で 1 段下げ（難しく）、1 回まちがえたら 1 段上げ（やさしく）
-- 値は掛け算で動かす（factor 倍）。1-up-2-down は正答率 70.7%（p² = 0.5）、1-up-3-down は 79.4% に集まる
-- fast = True なら、最初にまちがえるまでは 1 回正解ごとに factor² ずつ下げて、すばやくしきい値の近くへ
data Stair = Stair
  { stV, stFactor :: !Double, stNDown :: !Int, stLo, stHi :: !Double, stFast :: !Bool
  , stRun, stDir :: !Int
  , stTrack :: [(Double, Bool)]     -- 試行の記録（値, 正答か）。新しい順
  , stRevs  :: [(Int, Double)] }    -- 反転点（試行番号, 値）。新しい順

makeStair :: Double -> Double -> Int -> Double -> Double -> Bool -> Stair
makeStair start f nd lo hi fast = Stair start f nd lo hi fast 0 0 [] []

-- これまでの試行数
stN :: Stair -> Int
stN = length . stTrack

stairUpdate :: Stair -> Bool -> Stair
stairUpdate st0 correct
  | stFast st = st { stDir = -1, stV = clampv (stLo st) (stHi st) (stV st / (stFactor st * stFactor st)) }
  | move == 0 = st { stRun = run' }
  | otherwise = st { stRun = run', stDir = move
                   , stRevs = if stDir st /= 0 && move /= stDir st then (stN st - 1, stV st) : stRevs st else stRevs st   -- 向きが変わった = 反転点
                   , stV = clampv (stLo st) (stHi st) (if move < 0 then stV st / stFactor st else stV st * stFactor st) }
  where
    st1 = st0 { stTrack = (stV st0, correct) : stTrack st0 }
    st = if stFast st1 && not correct then st1 { stFast = False } else st1
    (run', move) | correct = if stRun st + 1 >= stNDown st then (0, -1) else (stRun st + 1, 0 :: Int)
                 | otherwise = (0, 1)

-- しきい値 = 反転点の幾何平均（最初の skip 個は練習とみなして捨てる）。足りなければ最後の 6 試行の幾何平均
stairThreshold :: Stair -> Int -> Maybe Double
stairThreshold st skip
  | null r = Nothing
  | otherwise = Just (exp (foldl' (\s v -> s + log v) 0 r / fromIntegral (length r)))
  where
    r0 = map snd (drop skip (reverse (stRevs st)))
    r = if length r0 < 2 then map fst (reverse (take 6 (stTrack st))) else r0

-- ---- 直線 y = a + b x を最小二乗法で当てはめる（r² = 当てはまりのよさ）
data Lin = Lin { linA, linB, linR2 :: !Double }

linFit :: [Double] -> [Double] -> Maybe Lin
linFit xs ys
  | n < 2 || sxx < 1e-12 = Nothing
  | otherwise = Just (Lin (my - b * mx) b (if syy > 1e-12 then sxy * sxy / (sxx * syy) else 1))
  where
    n = length xs
    nf = fromIntegral n
    mx = sum xs / nf
    my = sum ys / nf
    sxx = sum [ (x - mx) ^ (2 :: Int) | x <- xs ]
    sxy = sum [ (x - mx) * (y - my) | (x, y) <- zip xs ys ]
    syy = sum [ (y - my) ^ (2 :: Int) | y <- ys ]
    b = sxy / sxx

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 t) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 t) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (gray255 95) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 20) 14 (gray255 165) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 6) (gY g v + 6) 14 (gray255 165) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 44) 14 (gray255 205)
     , say yl (gx g) (gy g - 10) 14 (gray255 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

gray255 :: Float -> Color
gray255 v = gray (v / 255)

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

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

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

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- Maybe の値（null なら不合格）
nearM :: Maybe Double -> Double -> Double -> Bool
nearM m b tol = maybe False (\a -> near a b tol) m

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
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 120 210 150 else rgb255 255 90 90)
