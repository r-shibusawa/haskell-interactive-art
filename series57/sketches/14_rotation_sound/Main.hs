-- ============================================
-- 57-14 回転の音 ─ 単位四元数の 4 成分を 4 つの声に
-- q = (w, x, y, z) の各成分 c を音の高さ f = 220 × 2^c Hz（110〜440 Hz）と音量 c² に。|q| = 1 なので音量の合計はいつも一定
-- 姿勢をキーからキーへ slerp でゆっくり回すと、和音がなめらかに変わる。q と −q は同じ姿勢なのに音がちがう（二重被覆が聞こえる）
-- 操作：クリック 音を出す / スペース 止める・動かす / N q → −q / ↑↓ 速さ / M 消音 / R 新しいキー姿勢
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_, replicateM, when)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortOn)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

master :: Double
master = 0.15                          -- ★ 全体の音量（0.2 以下）

baseF :: Double
baseF = 220                            -- ★ 成分 0 のときの高さ [Hz]

names :: [String]
names = ["w", "x", "y", "z"]

cols :: [(Int, Int, Int)]
cols = [(255, 210, 110), (240, 100, 100), (110, 210, 120), (100, 160, 255)]

data World = World
  { wKeys    :: [Q]                    -- キー姿勢（6 個）
  , wSeg     :: !Int
  , wU       :: !Double
  , wSpeed   :: !Double
  , wRunning :: !Bool
  , wFlip    :: !Double                -- +1 か −1
  , wMuted   :: !Bool
  , wAudio   :: !Bool                  -- 音を出し始めたか（最初のクリックかキー）
  , wTrail   :: [Q]                    -- q の道のり（新しい順、400 個まで）
  , wTest    :: [(String, Bool)] }

world0 :: World
world0 = World [Q 1 0 0 0] 0 0 0.004 True 1 False False [] []

main :: IO ()
main = runSketch (sketch "57-14 Sound of rotation" world0)
  { skBack  = rgbC 16 16 32
  , skStart = \_ -> do
      printSelfTest selfTest
      newKeys world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 新しいキー姿勢（種は Math.random の代わりに randomRIO で決める）
newKeys :: World -> IO World
newKeys w = do
  s <- randomRIO (0, 999999 :: Int)
  let ks = evalGen (fromIntegral s) (replicateM 5 qRandom)
  pure w { wKeys = Q 1 0 0 0 : ks, wSeg = 0, wU = 0, wTrail = [] }

freqOf :: Double -> Double
freqOf c = baseF * 2 ** c

nKeys :: World -> Int
nKeys w = max 1 (length (wKeys w))

keyAt :: World -> Int -> Q
keyAt w i = getAt (Q 1 0 0 0) (wKeys w) (i `mod` nKeys w)

currentQ :: World -> Q
currentQ w = qSlerp (keyAt w (wSeg w)) (keyAt w (wSeg w + 1)) (wU w) True

-- 着いたキーは近い方の符号にそろえる
currentQOf :: World -> Int -> Q
currentQOf w s = let a = keyAt w s; b = keyAt w (s + 1) in if qDot a b < 0 then qScl b (-1) else b

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [ if k == i then v else x | (k, x) <- zip [0 ..] xs ]

step :: Env -> Float -> World -> IO World
step env _ w0' = do
  let w1 | wRunning w0' =
             let u = wU w0' + wSpeed w0'
             in if u >= 1
                  then let s = wSeg w0'
                           nxt = (s + 1) `mod` nKeys w0'
                       in w0' { wU = 0, wKeys = setAt nxt (currentQOf w0' s) (wKeys w0'), wSeg = nxt }
                  else w0' { wU = u }
         | otherwise = w0'
      q = qScl (currentQ w1) (wFlip w1)
  -- 4 つの声：p5.js 版の続く音の代わりに、0.25 秒の音を 1 秒に 4 回くり返す
  when (wAudio w1 && not (wMuted w1) && envFrame env `mod` 15 == 0) $
    forM_ (zip [0 :: Int ..] (qList q)) $ \(n, c) ->
      playTone (realToFrac (clampv (freqOf c) 40 2000)) (realToFrac (min 0.2 master * clampv (c * c * 0.25) 0 1)) 0.25 (if n == 0 then Sine else Triangle)
  pure w1 { wTrail = take 400 (q : wTrail w1) }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 物体
  [ drawParts2D planeParts (qMul vq q) (View2 (realToFrac ww * 0.3) (realToFrac hh * 0.55) (realToFrac (min ww hh) * 0.17) 8) 255 ]
  -- ---- 4 つの声の棒（高さ = 音の高さ、太さ = 音量）
  ++ [ Color (grayC 70) (Line [(bx - 20, by - bh / 2), (bx + 380, by - bh / 2)]) ]
  ++ concat [ [ Color (rgbaC r g b (round (60 + 195 * vol))) (rectTL x (by - h) 50 h)
              , sayC (nm ++ " = " ++ fmt 3 c) (x + 25) (by + 24) 14 (grayC 230)
              , sayC (fmt 1 (freqOf c) ++ " Hz") (x + 25) (by + 44) 14 (grayC 180)
              , sayC ("vol " ++ fmt 2 vol) (x + 25) (by + 62) 14 (grayC 180) ]
            | (n, (nm, (r, g, b), c)) <- zip [0 :: Int ..] (zip3 names cols (qList q))
            , let x = bx + fromIntegral n * 95
                  h = f2 ((c + 1) / 2) * bh
                  vol = c * c ]
  ++ [ say "line = component 0 (220 Hz)" (bx - 20) (by - bh / 2 - 8) 14 (grayC 170) ]
  -- ---- S³ の上の道のり（x, y を平面に、w の符号を色に）
  ++ [ Color (grayC 90) (circleLine 1 px py pr) ]
  ++ [ Color (if bw > 0 then rgbaC 255 210 110 al else rgbaC 110 160 255 al)
             (thickLine 3 (px + f2 ax * pr, py - f2 ay * pr) (px + f2 bx' * pr, py - f2 by' * pr))
     | (n, (Q _ ax ay _, Q bw bx' by' _)) <- zip [1 :: Int ..] (zip tr (drop 1 tr))
     , let al = 60 + n `div` 3 ]
  ++ [ say "path of q: x across, y up" (px - pr) (py + pr + 22) 14 (grayC 170)
     , say "colour = sign of w" (px - pr) (py + pr + 40) 14 (grayC 170)
     , say ("q = " ++ qStr 3 q ++ (if wFlip w < 0 then "   (playing -q)" else "")) 20 120 15 (grayC 230)
     , say ("sum of volumes = w^2 + x^2 + y^2 + z^2 = " ++ fmt 6 (sum [ v * v | v <- qList q ])) 20 146 15 (grayC 230)
     , say ("key " ++ show (wSeg w + 1) ++ " -> " ++ show ((wSeg w + 1) `mod` nKeys w + 1) ++ "   t = " ++ fmt 2 (wU w)) 20 172 15 (grayC 230)
     , say ("Sound of rotation: 4 components (w, x, y, z) as 4 voices" ++ (if wAudio w then "" else "  (click for sound)")) 20 36 20 (grayC 240)
     , say "Click: sound on   Space: stop/go   N: q <-> -q (same pose, other sound)   Up/Down speed   M mute   R new keys" 20 62 15 (grayC 175)
     , say "Start with a low volume. The master volume is 0.2 or less" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    q = qScl (currentQ w) (wFlip w)
    vq = qAxisAngle (Vec 1 0 0) 0.35
    bx = ww * 0.5; by = hh * 0.78; bh = hh * 0.5
    px = ww * 0.86; py = hh * 0.3; pr = min ww hh * 0.13
    tr = reverse (wTrail w)                -- 古い順

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wAudio = True }
  EventKey k Down _ _ -> key k w { wAudio = True }
  _ -> pure w
  where
    key k w' = case k of
      SpecialKey KeySpace -> pure w' { wRunning = not (wRunning w') }
      SpecialKey KeyUp -> pure w' { wSpeed = min 0.03 (wSpeed w' * 1.5) }
      SpecialKey KeyDown -> pure w' { wSpeed = max 0.0005 (wSpeed w' / 1.5) }
      Char c
        | c `elem` "nN" -> pure w' { wFlip = negate (wFlip w') }
        | c `elem` "mM" -> pure w' { wMuted = not (wMuted w') }
        | c `elem` "rR" -> newKeys w'
      _ -> pure w'

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("sum of volumes sum c^2 x 0.25 = 0.25 (always the same because |q| = 1)", e1 < 1e-12)
  , ("q and -q give the same rotation matrix (the same pose)", e2 < 1e-15)
  , ("but q and -q sound at different pitches (largest difference " ++ fmt 0 diff ++ " Hz)", diff > 50)
  , ("pitches f(c) and f(-c) are symmetric around 220 Hz: f(c) f(-c) = 220^2", e5 < 1e-8)
  , ("pitch always within 110-440 Hz (components are in -1..1)", fr) ]
  where
    qs = evalGen 14 $ replicateM 100 $ do
      a <- qRandom
      b <- qRandom
      t <- rndG
      pure (qSlerp a b t True)
    mx = maximum . (0 :)
    e1 = mx [ abs (foldl' (\s v -> s + v * v * 0.25) 0 (qList q) - 0.25) | q <- qs ]
    e2 = mx [ matDiff (qToMat q) (qToMat (qScl q (-1))) | q <- qs ]
    diff = mx [ abs (freqOf c - freqOf (-c)) | q <- qs, c <- qList q ]
    fr = and [ freqOf c >= 110 - 1e-9 && freqOf c <= 440 + 1e-9 | q <- qs, c <- qList q ]
    e5 = mx [ abs (freqOf c * freqOf (-c) - baseF * baseF) | q <- qs, c <- qList q ]

-- ---------- やってみよう ----------
-- 1. 音を出して N を押そう。飛行機の向きは変わらないのに、音はどう変わる？
-- 2. 速さを ↑ で上げると、和音の変化はどう聞こえる？ キーのところで音の動きは急に変わる？
-- 3. freqOf を baseF × 2^(2c)（2 オクターブ）や、ドレミの音階に丸める式に変えよう
-- 4. 音量を c² でなく |c| にすると、合計は一定？（式で確かめよう）
-- 5. 自由制作：マウスの位置で回転軸を決めて、自分で「回転の楽器」を作ろう

-- ==================================================
-- ここから下は「四元数の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）。自己テストで使う
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を引きながら計算する小さなモナド（JavaScript と同じく左から順に引く）
newtype Gen a = Gen { runGen :: Rng -> (a, Rng) }

instance Functor Gen where
  fmap f (Gen g) = Gen (\r -> case g r of (a, r1) -> (f a, r1))

instance Applicative Gen where
  pure a = Gen (\r -> (a, r))
  Gen f <*> Gen g = Gen (\r -> case f r of
                                 (h, !r1) -> case g r1 of (a, !r2) -> (h a, r2))

instance Monad Gen where
  Gen g >>= k = Gen (\r -> case g r of (a, !r1) -> runGen (k a) r1)

rndG, rndNormalG :: Gen Double
rndG = Gen rnd
rndNormalG = Gen rndNormal

-- 種 s から始めて m を計算する
evalGen :: Word32 -> Gen a -> a
evalGen s m = fst (runGen m (seedRng s))

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標に
mouseP5 :: Env -> (Double, Double)
mouseP5 env = let (x, y) = envMouse env in (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 0〜255 の小数で色を作る（明るさを掛けた色など）
rgbD :: Double -> Double -> Double -> Double -> Color
rgbD r g b a = makeColor (c r) (c g) (c b) (c a)
  where c v = f2 (clampv (v / 255) 0 1)

-- 左下が (x, y) の文字（y は文字の下の線 = p5.js の BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央（textAlign(CENTER, CENTER)）
sayCC :: String -> Float -> Float -> Float -> Color -> Picture
sayCC s x y size c = sayC s x (y + size * 0.36) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 w の箱に折り返して書く（p5.js の text(s, x, y, w, h)。(x, y) は箱の左上）
sayBox :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayBox s x y w size c = Pictures [ say ln x (y + size + fromIntegral k * size * 1.25) size c | (k, ln) <- zip [0 :: Int ..] (wrapWords (words s)) ]
  where
    wrapWords [] = []
    wrapWords (h : t) = go h t
    go cur [] = [cur]
    go cur (x1 : xs)
      | textW (cur ++ " " ++ x1) size <= w = go (cur ++ " " ++ x1) xs
      | otherwise = cur : go x1 xs

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 中心 (x, y)、半径 r の円の線（太さ wt）
circleLine :: Float -> Float -> Float -> Float -> Picture
circleLine wt x y r = Translate x y (ThickCircle r wt)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 数の道具
-- Math.hypot（JavaScript と同じ計算：最大値で割ってカハンの足し算）
hypotL :: [Double] -> Double
hypotL xs =
  let as = map abs xs
      m = maximum (0 : as)
      go (!s, !c) v = let n = v / m
                          summand = n * n - c
                          pre = s + summand
                      in (pre, (pre - s) - summand)
  in if any isInfinite as then 1 / 0 else if any isNaN as then 0 / 0 else if m == 0 then 0
     else sqrt (fst (foldl' go (0, 0) as)) * m

hypot3 :: Double -> Double -> Double -> Double
hypot3 a b c = hypotL [a, b, c]

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- JavaScript の % （0 の方へ切り捨てた余り）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (fin 0 v + 0.5) :: Integer)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v0 = let v = fin 0 v0
               s = showFFloat (Just n) v ""
           in if all (`elem` "-0.") s then dropWhile (== '-') s else s

-- 指数で書く（toExponential(n)、例 1.2e-16）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = fmt n 0 ++ "e+0"
  | otherwise = let s = showEFloat (Just n) v ""
                    (m, e) = break (== 'e') s
                    e' = drop 1 e
                in m ++ "e" ++ (if take 1 e' == "-" then e' else '+' : e')
  where v = fin 0 v0
-- ---- 四元数 Q w x y z = w + xi + yj + zk
data Q = Q !Double !Double !Double !Double deriving (Eq, Show)

qMul :: Q -> Q -> Q                    -- ハミルトン積（i² = j² = k² = ijk = −1）
qMul (Q a0 a1 a2 a3) (Q b0 b1 b2 b3) =
  Q (a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3)
    (a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2)
    (a0 * b2 - a1 * b3 + a2 * b0 + a3 * b1)
    (a0 * b3 + a1 * b2 - a2 * b1 + a3 * b0)

qConj :: Q -> Q                        -- 共役 q*
qConj (Q w x y z) = Q w (-x) (-y) (-z)

qAdd, qSub :: Q -> Q -> Q
qAdd (Q a b c d) (Q e f g h) = Q (a + e) (b + f) (c + g) (d + h)
qSub (Q a b c d) (Q e f g h) = Q (a - e) (b - f) (c - g) (d - h)

qScl :: Q -> Double -> Q
qScl (Q a b c d) s = Q (a * s) (b * s) (c * s) (d * s)

qDot :: Q -> Q -> Double
qDot (Q a b c d) (Q e f g h) = a * e + b * f + c * g + d * h

qNorm :: Q -> Double                   -- |q|
qNorm a = sqrt (qDot a a)

qUnit :: Q -> Q
qUnit a = let n = qNorm a in if n > 1e-300 then qScl a (1 / n) else Q 1 0 0 0

qInv :: Q -> Q                         -- q⁻¹ = q*/|q|²
qInv a = let n2 = qDot a a in if n2 > 1e-300 then qScl (qConj a) (1 / n2) else Q 0 0 0 0

qDist :: Q -> Q -> Double
qDist a b = qNorm (qSub a b)

qList :: Q -> [Double]
qList (Q w x y z) = [w, x, y, z]

qAt :: Q -> Int -> Double              -- 成分 0〜3
qAt q i = getAt 0 (qList q) i

qAxisAngle :: Vec -> Double -> Q       -- 軸 n まわりに角度 th の回転：cos(th/2) + sin(th/2) n
qAxisAngle n th = let Vec ux uy uz = v3Unit n
                      s = sin (th / 2)
                  in Q (cos (th / 2)) (ux * s) (uy * s) (uz * s)

qRot :: Q -> Vec -> Vec                -- v を q v q* で回す（v は 3 次元ベクトル → 純四元数）
qRot q (Vec x y z) = let Q _ a b c = qMul (qMul q (Q 0 x y z)) (qConj q) in Vec a b c

qToMat :: Q -> Mat                     -- 同じ q から作った 3×3 回転行列（単位四元数のとき）
qToMat (Q w x y z) =
  Mat (Vec (1 - 2 * (y * y + z * z)) (2 * (x * y - w * z)) (2 * (x * z + w * y)))
      (Vec (2 * (x * y + w * z)) (1 - 2 * (x * x + z * z)) (2 * (y * z - w * x)))
      (Vec (2 * (x * z - w * y)) (2 * (y * z + w * x)) (1 - 2 * (x * x + y * y)))

qExp :: Q -> Q                         -- exp(w + v) = e^w (cos|v| + sin|v| v/|v|)
qExp (Q a0 a1 a2 a3) =
  let vn = hypot3 a1 a2 a3
      ew = exp a0
      s = if vn > 1e-12 then sin vn / vn else 1 - (vn * vn) / 6
  in Q (ew * cos vn) (ew * s * a1) (ew * s * a2) (ew * s * a3)

qLog :: Q -> Q                         -- log q = ln|q| + acos(w/|q|) v/|v|
qLog a@(Q a0 a1 a2 a3) =
  let n = qNorm a
      vn = hypot3 a1 a2 a3
      th = atan2 vn a0
      s = if vn > 1e-12 then th / vn else 1 / max n 1e-300
  in Q (log (max n 1e-300)) (s * a1) (s * a2) (s * a3)

qSlerp :: Q -> Q -> Double -> Bool -> Q   -- 球面線形補間。shortest なら −b と近い方の道
qSlerp a b0 t shortest =
  let d0 = qDot a b0
      (b, d1) = if shortest && d0 < 0 then (qScl b0 (-1), -d0) else (b0, d0)
      d = clampv d1 (-1) 1
      om = acos d
      so = sin om
  in if so < 1e-9 then qUnit (qAdd (qScl a (1 - t)) (qScl b t))       -- ほぼ同じ向き → lerp で十分
     else qAdd (qScl a (sin ((1 - t) * om) / so)) (qScl b (sin (t * om) / so))

qNlerp :: Q -> Q -> Double -> Bool -> Q   -- 正規化した線形補間
qNlerp a b0 t shortest =
  let b = if shortest && qDot a b0 < 0 then qScl b0 (-1) else b0
  in qUnit (qAdd (qScl a (1 - t)) (qScl b t))

qAngle :: Q -> Q -> Double             -- 2 つの姿勢の間の回転角（0〜π）
qAngle a b = let Q d0 d1 d2 d3 = qMul (qConj (qUnit a)) (qUnit b)
             in 2 * atan2 (hypot3 d1 d2 d3) (abs d0)

qRandom :: Gen Q                       -- 一様にランダムな単位四元数（S³ の上の点）
qRandom = do
  a <- rndNormalG; b <- rndNormalG; c <- rndNormalG; d <- rndNormalG
  pure (qUnit (Q a b c d))

qStr :: Int -> Q -> String             -- (w, x, y, z) を小数点以下 dg 桁で
qStr dg q = "(" ++ intercalate ", " [ fmt dg (if abs v < 0.5 * 10 ** negate (fromIntegral dg) then 0 else v) | v <- qList q ] ++ ")"

-- ---- 3 次元ベクトルと 3×3 行列（行列は 3 つの行）
data Vec = Vec !Double !Double !Double deriving (Eq, Show)
data Mat = Mat !Vec !Vec !Vec deriving (Eq, Show)

v3Dot :: Vec -> Vec -> Double
v3Dot (Vec a b c) (Vec d e f) = a * d + b * e + c * f

v3Cross :: Vec -> Vec -> Vec
v3Cross (Vec a0 a1 a2) (Vec b0 b1 b2) = Vec (a1 * b2 - a2 * b1) (a2 * b0 - a0 * b2) (a0 * b1 - a1 * b0)

v3Len :: Vec -> Double
v3Len (Vec a b c) = hypot3 a b c

v3Unit :: Vec -> Vec
v3Unit a@(Vec x y z) = let n = v3Len a in if n > 1e-300 then Vec (x / n) (y / n) (z / n) else Vec 0 0 1

v3Add, v3Sub :: Vec -> Vec -> Vec
v3Add (Vec a b c) (Vec d e f) = Vec (a + d) (b + e) (c + f)
v3Sub (Vec a b c) (Vec d e f) = Vec (a - d) (b - e) (c - f)

v3Scl :: Vec -> Double -> Vec
v3Scl (Vec a b c) s = Vec (a * s) (b * s) (c * s)

v3Dist :: Vec -> Vec -> Double
v3Dist (Vec a b c) (Vec d e f) = hypot3 (a - d) (b - e) (c - f)

v3List :: Vec -> [Double]
v3List (Vec a b c) = [a, b, c]

matVec :: Mat -> Vec -> Vec
matVec (Mat r0 r1 r2) v = Vec (v3Dot r0 v) (v3Dot r1 v) (v3Dot r2 v)

matT :: Mat -> Mat
matT (Mat (Vec a b c) (Vec d e f) (Vec g h i)) = Mat (Vec a d g) (Vec b e h) (Vec c f i)

matMul :: Mat -> Mat -> Mat
matMul (Mat r0 r1 r2) b = let Mat c0 c1 c2 = matT b
                              row r = Vec (v3Dot r c0) (v3Dot r c1) (v3Dot r c2)
                          in Mat (row r0) (row r1) (row r2)

matDet :: Mat -> Double
matDet (Mat r0 r1 r2) = v3Dot r0 (v3Cross r1 r2)

matList :: Mat -> [[Double]]
matList (Mat a b c) = map v3List [a, b, c]

matDiff :: Mat -> Mat -> Double        -- 成分の差の最大
matDiff a b = maximum (0 : zipWith (\x y -> abs (x - y)) (concat (matList a)) (concat (matList b)))

matOrthoErr :: Mat -> Double           -- |MᵀM − I| の最大成分（回転行列なら 0）
matOrthoErr a = matDiff (matMul (matT a) a) matI

matI :: Mat
matI = Mat (Vec 1 0 0) (Vec 0 1 0) (Vec 0 0 1)
-- ---- 3D を 2D の画面に描く道具（透視投影）。View2 中心 x y・拡大 s・目までの距離 d
--   数学の向き：x 右・y 上・z 手前。回転は全部 q v q* で計算する
data View2 = View2 !Double !Double !Double !Double

-- 画面の点 (x, y)（p5.js の座標）と奥行き z
projP :: Vec -> View2 -> (Float, Float, Double)
projP (Vec x y z) (View2 vx vy vs vd) =
  let f = vd / max 0.2 (vd - z)
  in (f2 (vx + x * vs * f), f2 (vy - y * vs * f), z)

projXY :: Vec -> View2 -> (Float, Float)
projXY v vw = let (x, y, _) = projP v vw in (x, y)

-- 箱を組み合わせた物体を、回転の四元数 q で回して描く（奥の面から順にぬる＝画家のアルゴリズム）
--   p5.js 版と同じく箱は 1 本の枠線つき。alpha は 0〜255（255 = 不透明）
drawParts2D :: [Part] -> Q -> View2 -> Double -> Picture
drawParts2D parts q vw@(View2 _ _ _ vd) alpha = Pictures (concatMap face (sortOn (\(cz, _, _, _) -> cz) faces))
  where
    faces = [ (cz, fp, 0.42 + 0.58 * max 0 (v3Dot nw light2), col)
            | Part (Vec cx cy cz0) (Vec hx hy hz) col <- parts
            , let pts = [ qRot q (Vec (cx + x * hx) (cy + y * hy) (cz0 + z * hz)) | Vec x y z <- boxCorners ]
            , (n, idx) <- boxFaces
            , let nw = qRot q n
                  fp = map (getAt (Vec 0 0 0) pts) idx
                  cz = sum [ z | Vec _ _ z <- fp ] / 4
                  ctr = foldl' (\acc p -> v3Add acc (v3Scl p 0.25)) (Vec 0 0 0) fp
            , v3Dot nw (v3Sub (Vec 0 0 vd) ctr) > 0 ]            -- 裏を向いた面は描かない
    face (_, fp, b, (r, g, bl)) =
      let ps = map (`projXY` vw) fp
      in [ Color (rgbD (r * b) (g * b) (bl * b) alpha) (Polygon ps)
         , Color (rgbD 20 24 40 (if alpha >= 255 then 120 else alpha * 0.5)) (poly ps) ]

boxFaces :: [(Vec, [Int])]
boxFaces = [ (Vec 1 0 0, [1, 2, 6, 5]), (Vec (-1) 0 0, [0, 4, 7, 3]), (Vec 0 1 0, [2, 3, 7, 6])
           , (Vec 0 (-1) 0, [0, 1, 5, 4]), (Vec 0 0 1, [4, 5, 6, 7]), (Vec 0 0 (-1), [0, 3, 2, 1]) ]

boxCorners :: [Vec]
boxCorners = [ Vec (-1) (-1) (-1), Vec 1 (-1) (-1), Vec 1 1 (-1), Vec (-1) 1 (-1)
             , Vec (-1) (-1) 1, Vec 1 (-1) 1, Vec 1 1 1, Vec (-1) 1 1 ]

light2 :: Vec
light2 = v3Unit (Vec 0.4 0.7 0.8)

-- 3D の線分（太さ w）
line3 :: Color -> Float -> Vec -> Vec -> View2 -> Picture
line3 col w a b vw = Color col (thickLine w (projXY a vw) (projXY b vw))

-- 回した座標軸（x 赤、y 緑、z 青）
drawAxes2D :: Q -> View2 -> Double -> Picture
drawAxes2D q vw l = Pictures
  [ line3 c 2.5 (Vec 0 0 0) (qRot q e) vw
  | (c, e) <- [(rgbC 240 90 90, Vec l 0 0), (rgbC 110 220 120, Vec 0 l 0), (rgbC 100 160 255, Vec 0 0 l)] ]

-- 箱 Part 中心 半分の大きさ 色(r, g, b)
data Part = Part !Vec !Vec !(Double, Double, Double)

-- 飛行機のような物体（向きが分かりやすい）。機首 = +z、上 = +y、右の翼 = +x
planeParts :: [Part]
planeParts =
  [ Part (Vec 0 0 0) (Vec 0.18 0.18 0.9) (235, 235, 240)
  , Part (Vec 0 0 0.05) (Vec 1.0 0.04 0.26) (90, 200, 235)
  , Part (Vec 0 0.28 (-0.72)) (Vec 0.04 0.26 0.16) (245, 200, 80)
  , Part (Vec 0 0 (-0.75)) (Vec 0.4 0.03 0.12) (90, 200, 235)
  , Part (Vec 0 0.02 0.95) (Vec 0.1 0.1 0.06) (240, 110, 90) ]
