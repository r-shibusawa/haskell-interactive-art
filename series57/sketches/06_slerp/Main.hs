-- ============================================
-- 57-06 球面線形補間（slerp）─ 2 つの姿勢の間をなめらかに
-- lerp：q₀ と q₁ をまっすぐ結んでから長さを 1 にもどす（nlerp）。slerp：S³ の大円の上を一定の速さで進む
--   slerp(q₀, q₁, t) = [sin((1−t)Ω) q₀ + sin(tΩ) q₁] / sin Ω = q₀ (q₀⁻¹q₁)^t
-- 操作：スペース 再生/停止 / ← → t を動かす / ↑↓ 2 つの姿勢の間の角 / N 近い方の道（q と −q）/ K 4 つのキー姿勢をつなぐ / R ランダム
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortOn)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

angle0 :: Double
angle0 = 150                           -- ★ 2 つの姿勢の間の回転角（度）

axis0 :: Vec
axis0 = v3Unit (Vec 0.3 1 0.5)         -- ★ 回転軸

ns :: Int
ns = 120                               -- ★ グラフの分割数

speed :: Double
speed = 0.004                          -- ★ 再生の速さ

-- 斜め横から見る
viewQ :: Q
viewQ = qMul (qAxisAngle (Vec 1 0 0) 0.5) (qAxisAngle (Vec 0 1 0) (-1.1))

data World = World
  { wAngle    :: !Double
  , wAxis     :: !Vec
  , wT        :: !Double
  , wPlaying  :: !Bool
  , wShortest :: !Bool
  , wKeyMode  :: !Bool
  , wKeys     :: ![Q]                  -- キー姿勢 4 つ
  , wRng      :: !Rng                  -- p5.js 版と同じ乱数の続き
  , wTest     :: [(String, Bool)] }

world0 :: World
world0 = newKeys (World angle0 axis0 0 True True False [] (seedRng 66) [])

main :: IO ()
main = runSketch (sketch "57-06 Spherical linear interpolation (slerp)" world0)
  { skBack  = rgbC 16 19 36
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wT = jsMod (wT w + speed) 1 } else w)
  , skDraw  = draw
  , skEvent = event
  }

-- キー姿勢を 4 つ作る（1 つ前の姿勢から、ランダムな軸で 1.2〜2.2 rad 回す）
newKeys :: World -> World
newKeys w = let (ks, r) = runGen gen (wRng w) in w { wKeys = ks, wRng = r }
  where
    gen = go 3 [Q 1 0 0 0]
    go :: Int -> [Q] -> Gen [Q]
    go 0 acc = pure (reverse acc)
    go k acc@(prev : _) = do
      a <- rndNormalG; b <- rndNormalG; c <- rndNormalG
      r <- rndG
      go (k - 1) (qUnit (qMul prev (qAxisAngle (Vec a b c) (1.2 + r))) : acc)
    go _ [] = pure []

poses :: World -> (Q, Q)
poses w = let q0 = qAxisAngle (Vec 1 0 0) (-0.3)
          in (q0, qMul (qAxisAngle (wAxis w) ((wAngle w * pi) / 180)) q0)

-- 角速度（1 あたりの回転角）を数値微分で
speeds :: (Double -> Q) -> [(Double, Double)]
speeds fn = [ ((fromIntegral s - 0.5) / n, qAngle (fn (fromIntegral (s - 1) / n)) (fn (fromIntegral s / n)) * n) | s <- [1 .. ns] ]
  where n = fromIntegral ns

-- K：キー姿勢 0→1→2→3→0 を slerp でつなぐ
keyPose :: [Q] -> Double -> Q
keyPose ks u = let fl = floor (u * 4) :: Int
                   n = fl `mod` 4
                   f = u * 4 - fromIntegral fl
                   k i = getAt (Q 1 0 0 0) ks i
               in qSlerp (k n) (k ((n + 1) `mod` 4)) f True

draw :: Env -> World -> Picture
draw env w = view env $
  concat [ drawModel m | m <- [0, 1 :: Int], not (wKeyMode w && m == 0) ]
  -- ---- 角速度のグラフ
  ++ [ plotAxes g "t" "angular speed d theta/dt [rad / unit time]" [0, 0.25, 0.5, 0.75, 1] [0, gy1 g / 2, gy1 g] ]
  ++ (if wKeyMode w then [ Color (col 1) (polyline 3 (pl kp)) ]
      else [ Color (col m) (polyline 3 (pl sp)) | (m, sp) <- [(0 :: Int, spN), (1, spS)] ])
  ++ [ Color (rgbaC 255 255 255 120) (Line [(gX g (wT w), gy g), (gX g (wT w), gy g + gh g)]) ]
  -- ---- 数値
  ++ (if not (wKeyMode w)
        then let vs = map snd spS; vn = map snd spN
             in [ sayBox ("Angle between the poses = " ++ show (round (wAngle w) :: Int) ++ " deg   q0.q1 = " ++ fmt 3 dotv
                          ++ (if dotv < 0 then (if wShortest w then "  (negative -> use -q1: the near path)" else "  (negative -> as is: the far path)") else ""))
                         tx (ty - 15) (ww - tx - 20) 15 (grayC 230)
                , say ("slerp: max " ++ fmt 4 (maximum (0 : vs)) ++ "  min " ++ fmt 4 (minimum (1e9 : vs)) ++ "  (constant = angle)") tx (ty + 50) 15 (col 1)
                , say ("lerp : max " ++ fmt 4 (maximum (0 : vn)) ++ "  min " ++ fmt 4 (minimum (1e9 : vn)) ++ "  (faster in the middle)") tx (ty + 74) 15 (col 0)
                , say ("t = " ++ fmt 3 (wT w) ++ "   Omega on S3 = " ++ fmt 1 ((om * 180) / pi) ++ " deg (half the angle)") tx (ty + 104) 15 (grayC 200) ]
        else [ sayBox "Constant speed between keys, but direction and speed jump at each key (for smoother motion use curves such as squad)"
                      tx (ty - 15) (ww - tx - 20) 15 (grayC 200) ])
  ++ [ say "Spherical linear interpolation: lerp joins straight, slerp runs along a great circle at constant speed" 20 36 20 (grayC 240)
     , say "Space play/stop   Left/Right t   Up/Down angle +-30 deg   N near path on/off (q and -q)   K four key poses   R random" 20 62 15 (grayC 175)
     , say "Dots = path of the nose tip (24 equal steps of t). Faint objects = start and end poses" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    (q0, q1) = poses w
    sl t = qSlerp q0 q1 t (wShortest w)
    nl t = qNlerp q0 q1 t (wShortest w)
    kpf = keyPose (wKeys w)
    col m = if m == 0 then rgbC 255 150 90 else rgbC 110 200 255
    cola m a = if m == 0 then rgbaC 255 150 90 a else rgbaC 110 200 255 a
    fnOf m = if wKeyMode w then kpf else if m == 0 then nl else sl
    toView q = qMul viewQ q
    drawModel m =
      let vs = realToFrac (min ww hh) * (if wKeyMode w then 0.2 else 0.14)
          vx = if wKeyMode w then realToFrac ww * 0.28 else realToFrac ww * (0.15 + 0.27 * fromIntegral m)
          vw = View2 vx (realToFrac hh * 0.5) vs 8
          fn = fnOf m
      in -- 機首の先が通る点（t を等間隔に 24 個）：間隔がそろっていれば速さが一定
         [ Color (cola m 170) (at (projXY (qRot (toView (fn (fromIntegral s / 24))) (Vec 0 0 1.05)) vw) (disc 3.5)) | s <- [0 .. 24 :: Int] ]
         ++ (if not (wKeyMode w) then [drawParts2D planeParts (toView q0) vw 50, drawParts2D planeParts (toView q1) vw 50] else [])
         ++ [ drawParts2D planeParts (toView (fn (wT w))) vw 255
            , sayC (if wKeyMode w then "Four key poses joined by slerp" else if m == 0 then "lerp (normalized)" else "slerp")
                   (f2 vx) (hh * 0.5 + f2 (vs * 2.4)) 18 (col m) ]
    spN = speeds nl
    spS = speeds sl
    kp = speeds kpf
    yMax = maximum (0 : map snd (if wKeyMode w then kp else spN ++ spS)) * 1.25 + 1e-6
    g = G (ww * 0.6) 150 (ww * 0.36) (hh * 0.36) 0 1 0 yMax False
    pl ps = [ (gX g a, gY g b) | (a, b) <- ps ]
    tx = gx g; ty = gy g + gh g + 70
    dotv = qDot q0 q1
    om = acos (clampv (abs dotv) 0 1)

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT = min 1 (wT w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wT = max 0 (wT w - 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAngle = min 330 (wAngle w + 30) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAngle = max 30 (wAngle w - 30) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> pure w { wShortest = not (wShortest w) }
    | c `elem` "kK" -> pure w { wKeyMode = not (wKeyMode w) }
    | c `elem` "rR" -> do
        a <- randomRIO (0, 1 :: Double)
        b <- randomRIO (0, 1)
        d <- randomRIO (0, 1)
        pure (newKeys w { wAxis = v3Unit (Vec (a - 0.5) (b - 0.5) (d - 0.5)) })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("slerp ends: q0 at t = 0, q1 (or -q1) at t = 1", e1 < 1e-12)
  , ("slerp has constant angular speed: angle(q0, slerp(t)) = t x angle(q0, q1)", e2 < 1e-7)
  , ("slerp always gives a unit quaternion", e3 < 1e-12)
  , ("slerp(q0, q1, t) = q0 exp(t log(q0^-1 q1))", e4 < 1e-12)
  , ("normalized lerp is not constant speed: at 150 deg max/min = " ++ fmt 2 ratio, ratio > 1.5)
  , ("near path: if q0.q1 < 0 use -q1, the turn is 0.3 rad (far path 2 pi - 0.3)"
    , near (qAngle qa (qSlerp qa qb 1 True)) 0.3 1e-12 && near (2 * acos (qDot qa (qSlerp qa qb 1 False))) (2 * pi - 0.3) 1e-9) ]
  where
    pairs = evalGen 6 (replicateM 100 ((,) <$> qRandom <*> qRandom))
    errs = [ ( max (qDist (qSlerp a b 0 True) a) (min (qDist (qSlerp a b 1 True) b) (qDist (qSlerp a b 1 True) (qScl b (-1))))
             , maximum (0 : [ abs (qAngle a m - u * om) | (u, m) <- ms ])
             , maximum (0 : [ abs (qNorm m - 1) | (_, m) <- ms ])
             , maximum (0 : [ qDist m (qMul a (qExp (qScl (qLog (qMul (qConj a) bb)) u))) | (u, m) <- ms ]) )
           | (a, b) <- pairs
           , let om = qAngle a b
           , let ms = [ (u, qSlerp a b u True) | s <- [0 .. 10 :: Int], let u = fromIntegral s / 10 ]
           , let bb = if qDot a b < 0 then qScl b (-1) else b ]
    e1 = maximum (0 : [ x | (x, _, _, _) <- errs ])
    e2 = maximum (0 : [ x | (_, x, _, _) <- errs ])
    e3 = maximum (0 : [ x | (_, _, x, _) <- errs ])
    e4 = maximum (0 : [ x | (_, _, _, x) <- errs ])
    q0 = Q 1 0 0 0
    q1 = qAxisAngle (Vec 0 1 0) ((150 * pi) / 180)
    vn = map snd (speeds (\u -> qNlerp q0 q1 u True))
    ratio = maximum (0 : vn) / minimum (1e300 : vn)
    qa = qAxisAngle (Vec 0 0 1) 0.2
    qb = qScl (qAxisAngle (Vec 0 0 1) 0.5) (-1)

-- ---------- やってみよう ----------
-- 1. ↑↓ で角を 30° にしよう。lerp と slerp のグラフの差は？ 150° にすると？（小さい角なら lerp でも十分）
-- 2. 角を 210° 以上にして N を押そう。遠い方の道では、機体はどちらに回る？
-- 3. K で 4 つのキー姿勢をつなごう。グラフのキーのところでどうなっている？
-- 4. 回転行列の成分をそのまま lerp するとどうなる？（行列が回転でなくなり、物体がつぶれる）
-- 5. ゲームやアニメのカメラ・キャラクターの向きの補間には slerp（とその仲間）が使われている

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
-- ---- グラフの道具（左上が原点、y は下向き）。G 左 上 幅 高さ x0 x1 y0 y1 片対数か
data G = G { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogy :: !Bool }

gX :: G -> Double -> Float
gX g v = gx g + f2 (clampv ((v - gx0 g) / (gx1 g - gx0 g)) (-0.02) 1.02) * gw g

gY :: G -> Double -> Float
gY g v = gy g + gh g - f2 (clampv t (-0.02) 1.02) * gh g
  where t | glogy g = (logBase 10 (max v 1e-300) - logBase 10 (gy0 g)) / (logBase 10 (gy1 g) - logBase 10 (gy0 g))
          | otherwise = (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (if glogy g then "1e" ++ show (round (logBase 10 v) :: Int) else prec3 v) (gx g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 40) 14 (grayC 205), say yl (gx g) (gy g - 8) 14 (grayC 205) ]

-- 有効数字 3 桁（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0'
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin 0 v0'
