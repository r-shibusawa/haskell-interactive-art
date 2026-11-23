-- ============================================
-- 57-07 二重被覆とベルトのトリック ─ 360° ではもどらず、720° でもどる
-- 壁と物体をベルトでつなぐ。ベルトの各点の向きを単位四元数 q(s)（s = 0 壁 → 1 物体）で表す
-- 物体を 360° 回すと物体の q は −1（向きは元どおり）。ベルトのねじれは、物体を動かしても（向きは変えずに）取れない
-- 720° 回すと q = +1 にもどり、ベルトを物体のまわりにくぐらせるとねじれが取れる。q と −q は同じ回転（S³ → SO(3) は 2 対 1）
-- 操作：1 360° / 2 720° / スペース 再生・停止 / ← → 少しずつ / R 最初から
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

speed :: Double
speed = 0.004                          -- ★ 再生の速さ

nSeg :: Int
nSeg = 64                              -- ★ ベルトの分割数

beltLen, beltW :: Double
beltLen = 5.2                          -- ★ ベルトの長さ
beltW = 0.32                           -- ★ ベルトの幅

viewQ :: Q
viewQ = qMul (qAxisAngle (Vec 1 0 0) 0.45) (qAxisAngle (Vec 0 1 0) (-0.35))

data World = World
  { wMode    :: !Int                   -- 1 = 360°、2 = 720°
  , wPhase   :: !Double                -- 0〜1 回す、1〜2 ベルトを動かす
  , wPlaying :: !Bool
  , wTest    :: [(String, Bool)] }

world0 :: World
world0 = World 2 0 True []

main :: IO ()
main = runSketch (sketch "57-07 Double cover and the belt trick" world0)
  { skBack  = rgbC 16 18 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wPhase = jsMod (wPhase w + speed) 2 } else w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

swing :: Double -> Vec                 -- b(u)：i → j → −i
swing u = Vec (cos (pi * u)) (sin (pi * u)) 0

expV :: Vec -> Double -> Q             -- exp(a·v)（v は単位の純四元数）
expV (Vec x y z) a = qExp (Q 0 (x * a) (y * a) (z * a))

-- ベルトの点 s の向き（md = 1：360°、2：720°）
beltQ :: Int -> Double -> Double -> Q
beltQ md s ph
  | ph <= 1 = expV (Vec 1 0 0) (fromIntegral md * pi * s * ph)          -- 物体を x 軸まわりに回す（ねじれが増える）
  | md == 1 = expV (swing u) (pi * s)                                   -- 端は exp(π b) = −1 のまま
  | otherwise = qMul (expV (Vec 1 0 0) (pi * s)) (expV (swing u) (pi * s))   -- 端は (−1)(−1) = +1、u = 1 で全部 1
  where u = ph - 1

-- ベルトの中心線 = 各点の接線 R(q)·x̂ をつないだもの（物体は動いてよい）。(中心, 向き) のリスト
beltShape :: Int -> Double -> [(Vec, Q)]
beltShape md ph = go 0 (Vec (-beltLen / 2) 0 0)
  where go n c | n > nSeg = []
               | otherwise = let q = beltQ md (fromIntegral n / fromIntegral nSeg) ph
                             in (c, q) : go (n + 1) (v3Add c (v3Scl (qRot q (Vec 1 0 0)) (beltLen / fromIntegral nSeg)))

draw :: Env -> World -> Picture
draw env w = view env $
  -- 壁（いちばん奥にあるので先に描く）
  [ Color (rgbC 90 96 120) (Polygon [ glP env sc (vF (Vec (-beltLen / 2 - 0.05) (a * 1.3) b)) | (a, b) <- [(-1, -1.3), (-1, 1.3), (1, 1.3), (1, -1.3)] ])
  , drawItems $
      -- ベルト：表（金）と裏（青）で色を変える
      [ Item (zc * sc) (Color (if front then rgbC 255 200 80 else rgbC 80 150 255) (Polygon (map (glP env sc) quad)))
      | ((ac, aq), (bc, bq)) <- zip belt (drop 1 belt)
      , let wa = v3Scl (qRot aq (Vec 0 0 1)) beltW
            wb = v3Scl (qRot bq (Vec 0 0 1)) beltW
            Vec _ _ nz = qRot viewQ (qRot aq (Vec 0 1 0))
            front = nz > 0
            quad = map vF [v3Add ac wa, v3Add bc wb, v3Sub bc wb, v3Sub ac wa]
            zc = sum [ z | Vec _ _ z <- quad ] / 4 ]
      -- 物体（立方体に印）
      ++ glParts env sc (\v -> vF (v3Add endC (qRot qEnd v))) (Vec 0 0 0) 255 cube
  , hud ]
  where
    sc = realToFrac (min (envW env) (envH env)) * 0.13
    vF = qRot viewQ
    md = wMode w
    phase = wPhase w
    belt = beltShape md phase
    (endC, qEnd) = getAt (Vec 0 0 0, Q 1 0 0 0) belt nSeg
    cube = [ Part (Vec 0.45 0 0) (Vec 0.45 0.45 0.45) (235, 235, 240), Part (Vec 0.45 0.5 0) (Vec 0.12 0.08 0.12) (240, 110, 90) ]
    deg = fromIntegral md * 360 * min 1 phase
    isI = matDiff (qToMat qEnd) matI < 1e-9
    x = 20; y = 140
    gx' = envW env - 330; gy' = 140; gw' = 300; gh' = 120
    hud = Pictures $
      [ say ("Double cover and the belt trick   turned by " ++ (if md == 1 then "360 deg" else "720 deg")) 20 34 20 (grayC 240)
      , say "1: 360 deg   2: 720 deg   Space: play / stop   Left/Right: step   R reset" 20 60 15 (grayC 175)
      , say "Gold = front of the belt, blue = back. Each belt point has an orientation = unit quaternion q(s). At the wall always q = 1" 20 84 15 (grayC 175)
      , say (if phase <= 1 then "(1) turn the object about the x axis: " ++ fmt 0 deg ++ " deg"
                           else "(2) keep the object's orientation, pass the belt around it") x y 17 (rgbC 255 210 110)
      , say ("object q = " ++ qStr 3 qEnd) x (y + 28) 15 (grayC 230)
      , say ("object rotation matrix = " ++ (if isI then "identity (original orientation)" else "turned")) x (y + 52) 15 (grayC 230) ]
      ++ [ say (if md == 1 then "q stays -1 and cannot move -> the twist cannot be removed"
                else if phase > 1.97 then "q = +1, the belt is straight again!" else "q stays +1 while the twist unwinds")
               x (y + 80) 15 (if md == 1 then rgbC 255 140 140 else rgbC 130 230 150) | phase > 1 ]
      ++ [ say s x (y + 120 + fromIntegral n * 22) 14 (grayC 200)
         | (n, s) <- zip [0 :: Int ..]
             [ "q and -q are the same rotation (same rotation matrix)."
             , "Turning 360 deg: q = 1 -> -1. Not a closed path on S^3."
             , "Turning 720 deg: q = 1 -> -1 -> 1: one closed loop on S^3."
             , "This loop can shrink inside S^3 -> the twist comes out."
             , "An electron spin (1/2) changes sign after 360 deg (series 25, Bloch sphere)" ] ]
      -- 物体の q の w 成分の道のり（S³ の上で 1 → −1 → 1）
      ++ [ Color (grayC 90) (frameTL 1 gx' gy' gw' gh'), Color (grayC 90) (Line [(gx', gy' + gh' / 2), (gx' + gw', gy' + gh' / 2)])
         , Color (rgbC 255 200 80) (polyline 2 [ (gx' + gw' * fromIntegral n / 100, gy' + gh' / 2 - f2 qw * gh' / 2.2)
                                              | n <- [0 .. 100 :: Int], let Q qw _ _ _ = beltQ md 1 (2 * fromIntegral n / 100) ])
         , Color white (Line [(gx' + gw' * f2 phase / 2, gy'), (gx' + gw' * f2 phase / 2, gy' + gh')])
         , say "w component of the object's q (+1 / -1)" gx' (gy' - 8) 14 (grayC 180)
         , say "(1) turn" (gx' + 4) (gy' + gh' + 20) 14 (grayC 180)
         , say "(2) pass around" (gx' + gw' / 2 + 4) (gy' + gh' + 20) 14 (grayC 180)
         , drawSelfTest env (wTest w) ]

event :: Event -> World -> World
event ev w = case ev of
  EventKey (Char '1') Down _ _ -> w { wMode = 1, wPhase = 0 }
  EventKey (Char '2') Down _ _ -> w { wMode = 2, wPhase = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPhase = min 2 (wPhase w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPhase = max 0 (wPhase w - 0.05) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> w { wPhase = 0, wPlaying = True }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("|q| = 1 at every belt point, the wall end is always q = 1", unit < 1e-14 && wall < 1e-15)
  , ("720 deg: the object's q = +1 right after turning and while passing around", end720 < 1e-14)
  , ("720 deg: at the end every belt point has q = 1 (no twist)", flat < 1e-14)
  , ("360 deg: the object's q stays -1 (but the rotation matrix is the identity)", end360 < 1e-14 && matDiff (qToMat (Q (-1) 0 0 0)) matI == 0)
  , ("the belt is continuous where (1) and (2) meet", joinE < 1e-9)
  , ("q and -q give the same rotation matrix (2 to 1)", e == 0) ]
  where
    pts = [ (md, a, b, ph, s, beltQ md s ph)
          | md <- [1, 2], a <- [0 .. 40 :: Int], b <- [0 .. 20 :: Int]
          , let ph = (2 * fromIntegral a) / 40, let s = fromIntegral b / 20 ]
    mx = maximum . (0 :)
    unit = mx [ abs (qNorm q - 1) | (_, _, _, _, _, q) <- pts ]
    wall = mx [ qDist q (Q 1 0 0 0) | (_, _, b, _, _, q) <- pts, b == 0 ]
    end720 = mx [ qDist q (Q 1 0 0 0) | (md, _, b, ph, _, q) <- pts, b == 20, ph >= 1, md == 2 ]
    end360 = mx [ qDist q (Q (-1) 0 0 0) | (md, _, b, ph, _, q) <- pts, b == 20, ph >= 1, md == 1 ]
    joinE = mx [ qDist (beltQ md s 1) (beltQ md s (1 + 1e-12)) | (md, _, _, _, s, _) <- pts ]
    flat = mx [ qDist (beltQ 2 (fromIntegral b / 20) 2) (Q 1 0 0 0) | b <- [0 .. 20 :: Int] ]
    qs = evalGen 7 (replicateM 100 qRandom)
    e = mx [ matDiff (qToMat q) (qToMat (qScl q (-1))) | q <- qs ]

-- ---------- やってみよう ----------
-- 1. 2（720°）で再生。② でベルトはどう動く？ 物体の向き（赤い印）は変わっている？
-- 2. 1（360°）で再生。② でベルトの形は変わるのに、ねじれ（金と青の入れかわり）は残る。何回入れかわる？
-- 3. 本物のベルトやリボンと本（または手のひらにのせたコップ）でやってみよう（「皿のトリック」）
-- 4. swing の i → j → −i を i → k → −i（Vec cos 0 sin）にすると、② の形はどう変わる？
-- 5. シリーズ 25 のブロッホ球：スピン 1/2 は 2π 回すと状態に −1 がつき、4π でもどる。同じ「二重被覆」

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
-- ---- 3D の道具（p5.js の WEBGL の代わり）：回転は全部 q v q* で計算し、頂点を自分で透視投影する
--   数学の向き（x 右・y 上・z 手前、右手系）。目は z = eyeZ（p5.js の WEBGL の標準のカメラと同じ距離）にある
--   面も線も「奥行き z と絵」の組（Item）にして、奥から順に描く（画家のアルゴリズム）
data Item = Item !Double Picture

eyeZ :: Env -> Double
eyeZ env = realToFrac (envH env) / 2 / tan (pi / 6)

-- 3D の点 v（拡大 s）を画面（p5.js の座標）へ
glP :: Env -> Double -> Vec -> (Float, Float)
glP env s (Vec x y z) =
  let d = eyeZ env
      f = d / max 1 (d - z * s)
  in (envW env / 2 + f2 (x * s * f), envH env / 2 - f2 (y * s * f))

-- 奥から順に並べて 1 枚の絵に
drawItems :: [Item] -> Picture
drawItems its = Pictures [ p | Item _ p <- sortOn (\(Item z _) -> z) its ]

lightDir :: Vec
lightDir = v3Unit (Vec 0.4 0.7 0.8)

-- 線分 a → b（長い線は奥行きを正しく並べるため短く分ける）
glLine :: Env -> Double -> Color -> Float -> Vec -> Vec -> [Item]
glLine env s col w a b =
  let n = max 1 (min 12 (ceiling (v3Dist a b / 0.3) :: Int))
      pt k = v3Add a (v3Scl (v3Sub b a) (fromIntegral k / fromIntegral n))
  in [ Item (0.5 * (zOf p + zOf q) * s + 0.5) (Color col (thickLine w (glP env s p) (glP env s q)))
     | k <- [0 .. n - 1], let p = pt k, let q = pt (k + 1) ]
  where zOf (Vec _ _ z) = z

-- 折れ線（beginShape / vertex / endShape）
glPath :: Env -> Double -> Color -> Float -> [Vec] -> [Item]
glPath env s col w ps = concat (zipWith (glLine env s col w) ps (drop 1 ps))

-- a → b の矢印（線 + 先の 4 本のひげ）
glArrow :: Env -> Double -> Color -> Float -> Vec -> Vec -> [Item]
glArrow env s col w a b =
  let d = v3Sub b a
      l = v3Len d
      u = v3Scl d (1 / l)
      Vec _ uy _ = u
      side = v3Unit (v3Cross u (if abs uy < 0.9 then Vec 0 1 0 else Vec 1 0 0))
      up = v3Cross u side
      h = min 0.25 (l * 0.3)
  in glLine env s col w a b
     ++ if l < 1e-9 then [] else
          concat [ glLine env s col w b (v3Add (v3Add b (v3Scl u (-h))) (v3Scl t (h * 0.4))) | t <- [side, up, v3Scl side (-1), v3Scl up (-1)] ]

-- 箱を組み合わせた物体。Part 中心 半分の大きさ 色(r, g, b)
data Part = Part !Vec !Vec !(Double, Double, Double)

cubeFaces :: [(Vec, [Int])]
cubeFaces = [ (Vec 1 0 0, [1, 2, 6, 5]), (Vec (-1) 0 0, [0, 4, 7, 3]), (Vec 0 1 0, [2, 3, 7, 6])
            , (Vec 0 (-1) 0, [0, 1, 5, 4]), (Vec 0 0 1, [4, 5, 6, 7]), (Vec 0 0 (-1), [0, 3, 2, 1]) ]

cubeCorners :: [Vec]
cubeCorners = [ Vec (-1) (-1) (-1), Vec 1 (-1) (-1), Vec 1 1 (-1), Vec (-1) 1 (-1)
              , Vec (-1) (-1) 1, Vec 1 (-1) 1, Vec 1 1 1, Vec (-1) 1 1 ]

partCorners :: (Vec -> Vec) -> Vec -> Part -> [Vec]
partCorners rotFn offset (Part (Vec cx cy cz) (Vec hx hy hz) _) =
  [ v3Add (rotFn (Vec (cx + x * hx) (cy + y * hy) (cz + z * hz))) offset | Vec x y z <- cubeCorners ]

-- 物体を回転 rotFn（v → 回した v）で描く。alpha は 0〜255。裏を向いた面は描かない
glParts :: Env -> Double -> (Vec -> Vec) -> Vec -> Double -> [Part] -> [Item]
glParts env s rotFn offset alpha parts =
  [ Item (cz * s) (Color (rgbD (r * b) (g * b) (bl * b) alpha) (Polygon (map (glP env s) fp)))
  | part@(Part _ _ (r, g, bl)) <- parts
  , let pts = partCorners rotFn offset part
  , (n, idx) <- cubeFaces
  , let fp = map (getAt (Vec 0 0 0) pts) idx
        ctr = v3Scl (foldl' v3Add (Vec 0 0 0) fp) 0.25
        Vec _ _ cz = ctr
        nw = v3Unit (rotFn n)
        b = 0.42 + 0.58 * max 0 (v3Dot nw lightDir)
  , v3Dot nw (v3Sub (Vec 0 0 (eyeZ env / s)) ctr) > 0 ]

-- 同じ物体を針金（辺だけ）で。面より少し手前に置いて、面に隠れないようにする
glWire :: Env -> Double -> (Vec -> Vec) -> Color -> Vec -> [Part] -> [Item]
glWire env s rotFn col offset parts =
  [ Item (z + 0.08 * s) p
  | part <- parts
  , let pts = partCorners rotFn offset part
  , (a, b) <- [(0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (3, 7)]
  , Item z p <- glLine env s col 1.5 (getAt (Vec 0 0 0) pts a) (getAt (Vec 0 0 0) pts b) ]

-- 飛行機のような物体（向きが分かりやすい）。機首 = +z、上 = +y、右の翼 = +x
planeParts :: [Part]
planeParts =
  [ Part (Vec 0 0 0) (Vec 0.18 0.18 0.9) (235, 235, 240)
  , Part (Vec 0 0 0.05) (Vec 1.0 0.04 0.26) (90, 200, 235)
  , Part (Vec 0 0.28 (-0.72)) (Vec 0.04 0.26 0.16) (245, 200, 80)
  , Part (Vec 0 0 (-0.75)) (Vec 0.4 0.03 0.12) (90, 200, 235)
  , Part (Vec 0 0.02 0.95) (Vec 0.1 0.1 0.06) (240, 110, 90) ]

-- ---- アークボール（仮想トラックボール）：画面の点 → 球面の点 → 2 点を結ぶ回転の四元数
arcballVec :: Env -> (Double, Double) -> Vec
arcballVec env (mx, my) =
  let w = realToFrac (envW env); h = realToFrac (envH env)
      rr = 0.42 * min w h
      x = (mx - w / 2) / rr
      y = negate (my - h / 2) / rr
      d = x * x + y * y
  in if d < 1 then Vec x y (sqrt (1 - d)) else v3Unit (Vec x y 0)

arcballQ :: Vec -> Vec -> Q            -- a を b に重ねる最小の回転（角度 = a と b の間）
arcballQ a b = let Vec cx cy cz = v3Cross a b in qUnit (Q (1 + v3Dot a b) cx cy cz)
