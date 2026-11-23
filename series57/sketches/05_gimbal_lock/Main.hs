-- ============================================
-- 57-05 オイラー角とジンバルロック ─ 3 つの角では自由度が 1 つ消える場所がある
-- 姿勢 R = Ry(ヨー ψ) · Rx(ピッチ θ) · Rz(ロール φ)。3 つのリング（ジンバル）がそれぞれの角を表す
-- ピッチ = ±90° でヨーの軸とロールの軸が重なり、角を変えても回れない向きができる（ヤコビアンの階数 3 → 2）
-- 操作：← → ヨー / ↑↓ ピッチ / A D ロール / 9 ピッチ 90° / Q 四元数モード（機体の軸で直接回す）/ R 最初から
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

yaw0, pitch0, roll0 :: Double
yaw0 = 0.5                             -- ★ 最初のオイラー角（ラジアン）
pitch0 = 0.4
roll0 = 0.3

stepAng :: Double
stepAng = pi / 36                      -- ★ キー 1 回で回す角（5°）

viewQ :: Q
viewQ = qMul (qAxisAngle (Vec 1 0 0) 0.42) (qAxisAngle (Vec 0 1 0) (-0.7))

-- ヨー（金）・ピッチ（緑）・ロール（赤）
ringCol :: Int -> Int -> Color
ringCol n a = case n of
  0 -> rgbaC 255 200 80 a
  1 -> rgbaC 110 210 120 a
  _ -> rgbaC 240 100 100 a

data World = World
  { wYaw, wPitch, wRoll :: !Double
  , wQuatMode :: !Bool
  , wBody     :: !Q
  , wTest     :: [(String, Bool)] }

world0 :: World
world0 = World yaw0 pitch0 roll0 False (eulerQ yaw0 pitch0 roll0) []

main :: IO ()
main = runSketch (sketch "57-05 Euler angles and gimbal lock" world0)
  { skBack  = rgbC 14 18 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

eulerQ :: Double -> Double -> Double -> Q
eulerQ ps th ph = qMul (qMul (qAxisAngle (Vec 0 1 0) ps) (qAxisAngle (Vec 1 0 0) th)) (qAxisAngle (Vec 0 0 1) ph)

-- 行列から角を読み取る（ピッチ ±90° では ψ と φ が決まらない）
eulerFromQ :: Q -> (Double, Double, Double)
eulerFromQ q =
  let Mat (Vec _ _ r02) (Vec r10 r11 r12) (Vec _ _ r22) = qToMat q
  in (atan2 r02 r22, asin (clampv (-r12) (-1) 1), atan2 r10 r11)

-- 角の速さ (ψ', θ', φ') → 角速度 ω の 3 本の列
jacobian :: Double -> Double -> [Vec]
jacobian ps th =
  let qy = qAxisAngle (Vec 0 1 0) ps
      qyx = qMul qy (qAxisAngle (Vec 1 0 0) th)
  in [Vec 0 1 0, qRot qy (Vec 1 0 0), qRot qyx (Vec 0 0 1)]

-- 列 cols を行列とみなす（matDet 用：行 = 列ベクトル、det は転置しても同じ）
colsMat :: [Vec] -> Mat
colsMat cs = case cs of
  [a, b, c] -> Mat a b c
  _ -> matI

-- 特異値 = √(JᵀJ の固有値)（ヤコビ法、大きい順）
singVals :: [Vec] -> [Double]
singVals cols =
  let a0 = [ [ v3Dot (getAt (Vec 0 0 0) cols r) (getAt (Vec 0 0 0) cols c) | c <- [0 .. 2] ] | r <- [0 .. 2 :: Int] ]
      at2 m r c = getAt 0 (getAt [] m r) c
      rot m (r, c)
        | abs (at2 m r c) < 1e-300 = m
        | otherwise =
            let ph = 0.5 * atan2 (2 * at2 m r c) (at2 m c c - at2 m r r)
                cs = cos ph; sn = sin ph
                -- 列 r と c を回す
                m1 = [ [ if k == r then cs * at2 m t r - sn * at2 m t c else if k == c then sn * at2 m t r + cs * at2 m t c else at2 m t k
                       | k <- [0 .. 2] ] | t <- [0 .. 2] ]
                -- 行 r と c を回す
                m2 = [ [ if t == r then cs * at2 m1 r k - sn * at2 m1 c k else if t == c then sn * at2 m1 r k + cs * at2 m1 c k else at2 m1 t k
                       | k <- [0 .. 2] ] | t <- [0 .. 2] ]
            in m2
      af = foldl' rot a0 (concat (replicate 30 [(0, 1), (0, 2), (1, 2)]))
  in sortBy (flip compare) [ sqrt (max 0 (at2 af k k)) | k <- [0 .. 2] ]

step :: World -> World
step w
  | wQuatMode w = let (y, p, r) = eulerFromQ (wBody w) in w { wYaw = y, wPitch = p, wRoll = r }
  | otherwise = w

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawItems $
      -- 3 つのリング：外（ヨー、xy 面）・中（ピッチ、xz 面）・内（ロール、yz 面）
      concat [ glPath env sc (ringCol n 255) 7
                 [ vF (qRot qr (v3Add (v3Scl u (r * cos a)) (v3Scl v (r * sin a)))) | s <- [0 .. 72 :: Int], let a = (fromIntegral s / 72) * 2 * pi ]
             | (n, (qr, r, u, v)) <- zip [0 ..] rings ]
      -- 回転軸（ヤコビアンの列）：この向きのまわりにしか回れない
      ++ concat [ glLine env sc (ringCol n 200) 2 (vF (v3Scl ax (-3))) (vF (v3Scl ax 3)) | (n, ax) <- zip [0 ..] jac ]
      ++ glParts env sc (vF . qRot qAll) (Vec 0 0 0) 255 planeParts
  , hud ]
  where
    sc = realToFrac (min (envW env) (envH env)) * 0.16
    vF = qRot viewQ
    yaw = wYaw w; pitch = wPitch w; roll = wRoll w
    qy = qAxisAngle (Vec 0 1 0) yaw
    qyx = qMul qy (qAxisAngle (Vec 1 0 0) pitch)
    qAll = if wQuatMode w then wBody w else eulerQ yaw pitch roll
    rings = [ (qy, 2.5, Vec 1 0 0, Vec 0 1 0), (qyx, 2.2, Vec 1 0 0, Vec 0 0 1)
            , (qMul qyx (qAxisAngle (Vec 0 0 1) roll), 1.9, Vec 0 1 0, Vec 0 0 1) ]
    jac = jacobian yaw pitch
    sv = singVals jac
    det = matDet (colsMat jac)
    rank = length (filter (> 1e-6) sv)
    lock = rank < 3
    d v = fmt 1 (v * 180 / pi) ++ " deg"
    x = 20; y = 140
    hud = Pictures $
      [ say ("Euler angles and gimbal lock   " ++ (if wQuatMode w then "quaternion mode (turn about the body axes)" else "Euler angle mode")) 20 34 20 (grayC 240)
      , say "Left/Right yaw   Up/Down pitch   A D roll   9: pitch 90 deg   Q: quaternion <-> Euler mode   R reset" 20 60 15 (grayC 175)
      , say "R = Ry(yaw) Rx(pitch) Rz(roll).  Gold = yaw ring, green = pitch, red = roll.  Thin lines = their rotation axes" 20 84 15 (grayC 175) ]
      ++ [ say (nm ++ " = " ++ d v) x (y + fromIntegral n * 26) 17 (ringCol n 255)
         | (n, (nm, v)) <- zip [0 ..] [("yaw psi", yaw), ("pitch theta", pitch), ("roll phi", roll)] ]
      ++ [ say "Jacobian J (columns = the 3 rotation axes)" x (y + 100) 15 (grayC 230) ]
      ++ [ say (intercalate "    " [ let c = getAt 0 (v3List col) r in (if c >= 0 then " " else "") ++ fmt 3 c | col <- jac ])
               (x + 10) (y + 124 + fromIntegral r * 20) 15 (grayC 230) | r <- [0 .. 2] ]
      ++ [ say ("det J = " ++ fmt 4 det ++ "   (= -cos theta = " ++ fmt 4 (negate (cos pitch)) ++ ")") x (y + 200) 15 (grayC 230)
         , say ("singular values = " ++ intercalate ", " (map (fmt 4) sv)) x (y + 224) 15 (grayC 230)
         , say ("rank J = " ++ show rank ++ (if lock then "   GIMBAL LOCK! one degree of freedom is gone" else "   (can turn in 3 directions)"))
               x (y + 256) 18 (if lock then rgbC 255 110 110 else rgbC 130 230 150) ]
      ++ [ say "Now adding the same amount to yaw and roll does not change the pose (only yaw - roll matters)" x (y + 282) 14 (grayC 200) | abs (cos pitch) < 0.02 ]
      ++ [ say "Even getting close is risky: as pitch -> 90 deg the smallest singular value -> 0 and the angle rates J^-1 w -> infinity (09)"
               x (y + 304) 14 (grayC 200) ]
      ++ [ say "Quaternion mode: q <- q * (small turn about a body axis). Turns in all 3 directions anywhere. Angles above are read back from q"
               x (y + 330) 14 (rgbC 255 210 110) | wQuatMode w ]
      ++ [ drawSelfTest env (wTest w) ]

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> turn (Vec 0 1 0) (-1)
  EventKey (SpecialKey KeyRight) Down _ _ -> turn (Vec 0 1 0) 1
  EventKey (SpecialKey KeyUp) Down _ _ -> turn (Vec 1 0 0) (-1)
  EventKey (SpecialKey KeyDown) Down _ _ -> turn (Vec 1 0 0) 1
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> turn (Vec 0 0 1) (-1)
    | c `elem` "dD" -> turn (Vec 0 0 1) 1
    | c == '9' -> w { wPitch = pi / 2, wBody = eulerQ (wYaw w) (pi / 2) (wRoll w) }
    | c `elem` "qQ" -> w { wQuatMode = not (wQuatMode w), wBody = eulerQ (wYaw w) (wPitch w) (wRoll w) }
    | c `elem` "rR" -> w { wYaw = yaw0, wPitch = pitch0, wRoll = roll0, wBody = eulerQ yaw0 pitch0 roll0 }
  _ -> w
  where
    turn ax@(Vec ax0 ax1 _) sg
      | wQuatMode w = w { wBody = qUnit (qMul (wBody w) (qAxisAngle ax (sg * stepAng))) }   -- 機体の軸で回す（いつでも 3 方向）
      | ax1 /= 0 = w { wYaw = wYaw w + sg * stepAng }
      | ax0 /= 0 = w { wPitch = clampv (wPitch w + sg * stepAng) (-pi / 2) (pi / 2) }
      | otherwise = w { wRoll = wRoll w + sg * stepAng }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Euler angles -> product of quaternions = product of matrices Ry Rx Rz (200 sets)", e1 < 1e-12)
  , ("det J = -cos theta (determinant of the Jacobian)", e2 < 1e-12)
  , ("if theta /= +-90 deg the angles can be read back from the matrix", e3 < 1e-9)
  , ("at theta = 90 deg rank J = 2 (smallest singular value " ++ expo 1 (getAt 1 sv 2) ++ "), at theta = 0.3 it is 3"
    , getAt 1 sv 2 < 1e-7 && getAt 0 sv 1 > 0.5 && getAt 0 sv0 2 > 0.5)
  , ("at theta = 90 deg adding the same to yaw and roll gives the same pose (two angles become one)", d1 < 1e-12 && d2 > 0.1)
  , ("10000 quaternion turns: |q| = 1 and the matrix stays orthogonal (3 directions everywhere)"
    , abs (qNorm qEnd - 1) < 1e-14 && matOrthoErr (qToMat qEnd) < 1e-14) ]
  where
    rm ax a = qToMat (qAxisAngle ax a)
    (trials, qEnd) = evalGen 5 $ do
      ts <- replicateM 200 $ do
        a <- rndG; b <- rndG; c <- rndG
        pure ((a * 2 - 1) * pi, (b * 2 - 1) * 1.5, (c * 2 - 1) * pi)
      let axes = [Vec 1 0 0, Vec 0 1 0, Vec 0 0 1]
          go !t !q | t >= (10000 :: Int) = pure q
                   | otherwise = do
                       r <- rndG
                       go (t + 1) (qUnit (qMul q (qAxisAngle (getAt (Vec 0 0 0) axes (t `mod` 3)) ((r - 0.5) * 0.2))))
      q <- go 0 (Q 1 0 0 0)
      pure (ts, q)
    errs = [ ( matDiff (qToMat (eulerQ ps th ph)) (matMul (rm (Vec 0 1 0) ps) (matMul (rm (Vec 1 0 0) th) (rm (Vec 0 0 1) ph)))
             , abs (matDet (colsMat (jacobian ps th)) + cos th)
             , let (b0, b1, b2) = eulerFromQ (eulerQ ps th ph) in maximum [abs (b0 - ps), abs (b1 - th), abs (b2 - ph)] )
           | (ps, th, ph) <- trials ]
    e1 = maximum (0 : [ v | (v, _, _) <- errs ])
    e2 = maximum (0 : [ v | (_, v, _) <- errs ])
    e3 = maximum (0 : [ v | (_, _, v) <- errs ])
    sv = singVals (jacobian 0.7 (pi / 2))
    sv0 = singVals (jacobian 0.7 0.3)
    base = qToMat (eulerQ 0.3 (pi / 2) 0.5)
    d1 = matDiff base (qToMat (eulerQ (0.3 + 0.4) (pi / 2) (0.5 + 0.4)))
    d2 = matDiff base (qToMat (eulerQ (0.3 + 0.4) (pi / 2) 0.5))

-- ---------- やってみよう ----------
-- 1. 9 でピッチを 90° にしてから、← → と A D を押そう。同じ動きになっていない？ 金と赤の細い線は？
-- 2. ピッチ 90° のまま、機首を「左右」（今のヨーの軸と直角）に振る方法はある？
-- 3. Q で四元数モードにして同じことをしよう。どの向きにも回れる？ 上の角の表示はどうなる？
-- 4. ピッチを 85°、89°、89.9° と近づけて、最小の特異値と det J を見よう（stepAng を小さくする）
-- 5. アポロ宇宙船の姿勢を測る装置もジンバル式で、中のジンバルの角が大きくなると警告が出た。ゲームのカメラではどんなときに困る？

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
