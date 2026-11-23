-- ============================================
-- 57-09 角速度の積分 ─ ジャイロの値から姿勢を求める
-- 機体の軸まわりの角速度 ω（ジャイロセンサーが測る量）から、姿勢 q を q̇ = ½ q ω で積分する（RK4 + 毎回 |q| = 1 に）
-- 同じ ω をオイラー角の微分方程式（角の速さ = J⁻¹ R ω、05 のヤコビアン）で積分したものと並べる。ω が一定なら答えは q₀ exp(½ωt)
-- 操作：1 2 3 ω の向き / ↑↓ 回る速さ / ← → 時間のきざみ Δt / スペース 一時停止 / R 最初から
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

-- ★ 機体の軸での角速度の向き（1：ピッチ 90° のすぐ近くを通る 2：73° まで 3：ヨー中心）
omegas :: [Vec]
omegas = [Vec 1 0.02 0.01, Vec 1 0.3 0, Vec 0.1 1 0]

-- 斜めから見る向き
viewQ :: Q
viewQ = qMul (qAxisAngle (Vec 1 0 0) 0.35) (qAxisAngle (Vec 0 1 0) (-0.6))

-- 記録 1 つ：時刻、四元数の誤差、オイラー角の誤差、正しい姿勢のピッチ
data Rec = Rec !Double !Double !Double !Double

data World = World
  { wPreset :: !Int
  , wSpeed  :: !Double                 -- ★ 回る速さ |ω| [rad/s]（最初 1.2）
  , wDt     :: !Double                 -- ★ 時間のきざみ Δt [s]（最初 0.02）
  , wPaused :: !Bool
  , wT      :: !Double
  , wQ      :: !Q                      -- 四元数で積分した姿勢
  , wAng    :: !Vec                    -- オイラー角（ヨー ψ, ピッチ θ, ロール φ）で積分した姿勢
  , wHist   :: [Rec]                   -- 新しい順（600 個まで）
  , wTest   :: [(String, Bool)] }

world0 :: World
world0 = World 0 1.2 0.02 False 0 (Q 1 0 0 0) (Vec 0 0 0) [] []

resetAll :: World -> World
resetAll w = w { wT = 0, wQ = Q 1 0 0 0, wAng = Vec 0 0 0, wHist = [] }

main :: IO ()
main = runSketch (sketch "57-09 Integrating angular velocity" world0)
  { skBack  = rgbC 14 17 32
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

omegaBody :: World -> Vec
omegaBody w = v3Scl (v3Unit (getAt (Vec 1 0 0) omegas (wPreset w))) (wSpeed w)

-- ヨー（y 軸）→ ピッチ（x 軸）→ ロール（z 軸）の順の回転
eulerQ :: Vec -> Q
eulerQ (Vec a0 a1 a2) = qMul (qMul (qAxisAngle (Vec 0 1 0) a0) (qAxisAngle (Vec 1 0 0) a1)) (qAxisAngle (Vec 0 0 1) a2)

-- 四元数：q̇ = ½ q ω（ω は機体の軸での純四元数）を RK4 で 1 きざみ
quatRK4 :: Q -> Vec -> Double -> Q
quatRK4 q (Vec wx wy wz) h =
  let f x = qScl (qMul x (Q 0 wx wy wz)) 0.5
      k1 = f q
      k2 = f (qAdd q (qScl k1 (h / 2)))
      k3 = f (qAdd q (qScl k2 (h / 2)))
      k4 = f (qAdd q (qScl k3 h))
  in qUnit (qAdd q (qScl (qAdd (qAdd k1 (qScl k2 2)) (qAdd (qScl k3 2) k4)) (h / 6)))

-- J の列 = ヨー・ピッチ・ロールの軸（05 と同じ）
eulerCols :: Vec -> (Vec, Vec, Vec)
eulerCols (Vec a0 a1 _) =
  let qy = qAxisAngle (Vec 0 1 0) a0
  in (Vec 0 1 0, qRot qy (Vec 1 0 0), qRot (qMul qy (qAxisAngle (Vec 1 0 0) a1)) (Vec 0 0 1))

-- オイラー角：角の速さ = J⁻¹ (R ω)。cos θ → 0 で割り算が爆発する
eulerRates :: Vec -> Vec -> Vec
eulerRates a w =
  let (c0, c1, c2) = eulerCols a
      wW = qRot (eulerQ a) w
      det0 = v3Dot c0 (v3Cross c1 c2)
      det = if abs det0 < 1e-12 then (if det0 < 0 then -1e-12 else 1e-12) else det0
      cl v = clampv v (-1e4) 1e4
  in Vec (cl (v3Dot wW (v3Cross c1 c2) / det)) (cl (v3Dot c0 (v3Cross wW c2) / det)) (cl (v3Dot c0 (v3Cross c1 wW) / det))   -- クラメルの公式

eulerRK4 :: Vec -> Vec -> Double -> Vec
eulerRK4 a w h =
  let k1 = eulerRates a w
      k2 = eulerRates (v3Add a (v3Scl k1 (h / 2))) w
      k3 = eulerRates (v3Add a (v3Scl k2 (h / 2))) w
      k4 = eulerRates (v3Add a (v3Scl k3 h)) w
      Vec x y z = v3Add a (v3Scl (v3Add (v3Add k1 (v3Scl k2 2)) (v3Add (v3Scl k3 2) k4)) (h / 6))
      cl v = clampv v (-1e6) 1e6
  in Vec (cl x) (cl y) (cl z)

exactQ :: Double -> Vec -> Q           -- ω が一定なら q(t) = exp(½ω t)
exactQ t (Vec x y z) = qExp (qScl (Q 0 x y z) (t / 2))

pitchOf :: Q -> Double                 -- 姿勢のピッチ = asin(−R[1][2])
pitchOf q = let Mat (Vec _ _ _) (Vec _ _ r12) _ = qToMat q in asin (clampv (-r12) (-1) 1)

step :: World -> World
step w
  | wPaused w = w
  | otherwise =
      let om = omegaBody w
          q = quatRK4 (wQ w) om (wDt w)
          a = eulerRK4 (wAng w) om (wDt w)
          t = wT w + wDt w
          qe = exactQ t om
          r = Rec t (max 1e-16 (qAngle q qe)) (max 1e-16 (qAngle (eulerQ a) qe)) (pitchOf qe)
      in w { wQ = q, wAng = a, wT = t, wHist = take 600 (r : wHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawItems $
      glParts env sc (viewF . qRot (wQ w)) offL 255 planeParts
      ++ glParts env sc (viewF . qRot (eulerQ (wAng w))) offR 255 planeParts
      ++ glWire env sc (viewF . qRot qe) white offL planeParts
      ++ glWire env sc (viewF . qRot qe) white offR planeParts
      -- ω の矢印（機体の軸 → いまの姿勢で回して表示）
      ++ glArrow env sc (rgbC 255 200 80) 3 offL (v3Add offL (viewF (qRot (wQ w) (v3Scl (v3Unit om) 1.6))))
  , hud ]
  where
    ww = envW env; hh = envH env
    sc = realToFrac (min ww hh) * 0.13
    viewF = qRot viewQ
    om = omegaBody w
    qe = exactQ (wT w) om
    d = realToFrac ww * 0.2 / sc
    offL = Vec (-d) (-1) 0
    offR = Vec d (-1) 0
    Rec _ errQ errE _ = case wHist w of
      (r : _) -> r
      [] -> Rec 0 1e-16 1e-16 0
    hist = reverse (wHist w)
    t0 = case hist of
      (Rec t _ _ _ : _) -> t
      [] -> 0
    gr = G (ww - 400) 125 360 140 t0 (max (wT w) 1e-3) 1e-16 10 True
    gp = G (ww - 400) 335 360 70 t0 (max (wT w) 1e-3) (-pi / 2) (pi / 2) False
    Vec a0 a1 a2 = wAng w
    degS v = fmt 0 (v * 180 / pi) ++ " deg"
    hud = Pictures $
      [ say "Integrating angular velocity:  dq/dt = q w / 2 (RK4)  vs  Euler angles" 20 34 20 (grayC 240)
      , say ("1 2 3: direction of w   Up/Down speed |w| = " ++ fmt 1 (wSpeed w) ++ " rad/s   Left/Right dt = " ++ fmt 3 (wDt w)
             ++ " s   Space: pause   R reset") 20 60 15 (grayC 175)
      , say "White wire = the exact answer q0 exp(w t / 2).  Gold arrow = angular velocity w (constant in the body axes)" 20 84 15 (grayC 175)
      , sayC "integrated with quaternions" (ww * 0.3) (hh - 64) 17 (rgbC 130 230 150)
      , sayC "integrated with Euler angles" (ww * 0.7) (hh - 64) 17 (rgbC 255 150 120)
      , sayC ("error " ++ expo 1 errQ ++ " rad") (ww * 0.3) (hh - 40) 15 (grayC 220)
      , sayC ("error " ++ expo 1 errE ++ " rad   angles = (" ++ intercalate ", " (map degS [a0, a1, a2]) ++ ")") (ww * 0.7) (hh - 40) 15 (grayC 220)
      , plotAxes gr "time t [s]" "attitude error [rad] (log)" [gx0 gr, gx1 gr] [1e-16, 1e-8, 1]
      , Color (rgbC 130 230 150) (polyline 2 [ (gX gr t, gY gr e) | Rec t e _ _ <- hist ])
      , Color (rgbC 255 150 120) (polyline 2 [ (gX gr t, gY gr e) | Rec t _ e _ <- hist ])
      -- ピッチの角（±90° の近くが危ない）
      , Color (grayC 95) (frameTL 1 (gx gp) (gy gp) (gw gp) (gh gp))
      , Color (rgbC 255 200 80) (polyline 2 [ (gX gp t, gY gp p) | Rec t _ _ p <- hist ])
      , say "pitch of the exact attitude (edges = +-90 deg)" (gx gp) (gy gp - 6) 14 (grayC 180) ]
      ++ [ say s 20 (120 + fromIntegral n * 22) 14 (grayC 200)
         | (n, s) <- zip [0 :: Int ..]
             [ "A gyro sensor (phone, drone, spacecraft) measures w about the body axes."
             , "Integrating dq/dt = q w / 2 gives the attitude (drift is corrected with"
             , "accelerometer and magnetometer). The Euler-angle equations divide by"
             , "cos(pitch), so near +-90 deg the error grows by many digits." ] ]
      ++ [ drawSelfTest env (wTest w) ]

event :: Event -> World -> World
event ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> resetAll w { wPreset = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> resetAll w
  EventKey (SpecialKey KeyUp) Down _ _ -> resetAll w { wSpeed = min 4 (wSpeed w + 0.4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetAll w { wSpeed = max 0.4 (wSpeed w - 0.4) }
  EventKey (SpecialKey KeyRight) Down _ _ -> resetAll w { wDt = min 0.1 (wDt w * 1.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetAll w { wDt = max 0.002 (wDt w / 1.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("quaternion RK4 (1000 steps): |q| = 1, difference from the exact exp(w t / 2) " ++ expo 1 eq ++ " rad"
    , abs (qNorm qN - 1) < 1e-14 && eq < 1e-9)
  , ("the same w with Euler angles: passing near pitch 90 deg gives error " ++ expo 1 ea ++ " rad (over 1000 times the quaternion)"
    , ea > 1000 * eq)
  , ("rate formula: J (psi', theta', phi') = R w (away from pitch +-90 deg)", e3 < 1e-10)
  , ("yaw only (pitch stays 0) is exact with Euler angles too: psi = 0.8 x 10 = 8 rad", near ya 8 1e-9 && near yb 0 1e-12)
  , ("exp(w t / 2) = quaternion turning |w| t about the axis w-hat", e5 < 1e-12) ]
  where
    w1 = v3Scl (v3Unit (getAt (Vec 1 0 0) omegas 0)) 1.2
    h = 0.01
    run (!q, !a, !mq, !ma) n =
      let q' = quatRK4 q w1 h
          a' = eulerRK4 a w1 h
          ex = exactQ (fromIntegral n * h) w1
      in (q', a', max mq (qAngle q' ex), max ma (qAngle (eulerQ a') ex))
    (qN, _, eq, ea) = foldl' run (Q 1 0 0 0, Vec 0 0 0, 0, 0) [1 .. 1000 :: Int]
    (e3, e5) = evalGen 9 $ do
      xs <- replicateM 100 $ do
        r0 <- rndG; r1 <- rndG; r2 <- rndG
        n0 <- rndNormalG; n1 <- rndNormalG; n2 <- rndNormalG
        let ang = Vec ((r0 - 0.5) * 6) ((r1 - 0.5) * 2.8) ((r2 - 0.5) * 6)
            ww = Vec n0 n1 n2
            Vec ra rb rc = eulerRates ang ww
            (c0, c1, c2) = eulerCols ang
        pure (v3Dist (v3Add (v3Add (v3Scl c0 ra) (v3Scl c1 rb)) (v3Scl c2 rc)) (qRot (eulerQ ang) ww))
      ys <- replicateM 50 $ do
        n0 <- rndNormalG; n1 <- rndNormalG; n2 <- rndNormalG
        tt0 <- rndG
        let ww = Vec n0 n1 n2
            tt = tt0 * 5
        pure (qDist (exactQ tt ww) (qAxisAngle ww (v3Len ww * tt)))
      pure (maximum (0 : xs), maximum (0 : ys))
    Vec ya yb _ = foldl' (\a _ -> eulerRK4 a (Vec 0 0.8 0) 0.01) (Vec 0 0 0) [1 .. 1000 :: Int]

-- ---------- やってみよう ----------
-- 1. 1 で再生。ピッチのグラフが ±90° に近づいたとき、オレンジの誤差はどうなる？ 右の飛行機は？
-- 2. 3（ヨー中心）にすると、オイラー角でも誤差は小さい。なぜ？
-- 3. ← → で Δt を変えよう。緑の誤差（RK4 の誤差）は Δt⁴ に比例する？
-- 4. quatRK4 の qUnit を外すと |q| はどうなる？（ゆっくりずれる）
-- 5. ω を時間で変えてみよう（omegaBody に sin (wT w) を入れる）。そのときは正しい答えの式がない → 実際のジャイロと同じ

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
