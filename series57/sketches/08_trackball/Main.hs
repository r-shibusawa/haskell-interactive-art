-- ============================================
-- 57-08 3D の物体操作（アークボール）─ 回転を四元数で積み重ねる
-- ドラッグの 2 点を球面に写し、a を b に重ねる回転 dq = (1 + a·b, a × b)/|…| を作って q ← dq q と積み重ねる
-- 同じ回転を「行列を掛け続ける」方法でも積み重ねて、誤差（直交からのずれ）を比べる
--   1：正確な小回転を float32 で掛ける（ずれは小さいが増えていく）　2：小さい角の近似 M ← M + [ω]×M Δt（ずれが目に見える）
-- 操作：ドラッグ 回す / スペース 自動で回し続ける（1 フレームに 200 回）/ 1 2 更新のしかた / N 四元数の正規化 ON/OFF / R 最初から
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortOn)
import Data.Word (Word32)
import GHC.Float (double2Float, float2Double)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

perFrame :: Int
perFrame = 200                         -- ★ 自動回転で 1 フレームに積み重ねる回数

omegaDt :: Double
omegaDt = 0.01                         -- ★ 1 回の小さな回転の角（ラジアン）

viewQ :: Q
viewQ = qAxisAngle (Vec 1 0 0) 0.25

data World = World
  { wQ        :: !Q                    -- 四元数で積み重ねた向き
  , wM        :: !Mat                  -- 行列で積み重ねた向き（成分は float32 に丸める）
  , wSteps    :: !Int
  , wMethod   :: !Int
  , wNormOn   :: !Bool
  , wAuto     :: !Bool
  , wDragFrom :: !(Maybe Vec)
  , wErrHist  :: [(Double, Double, Double)]   -- (回数, q の誤差, M の誤差)、新しい順に 400 個まで
  , wTest     :: [(String, Bool)] }

world0 :: World
world0 = World (Q 1 0 0 0) matI 0 1 True False Nothing [] []

resetAll :: World -> World
resetAll w = w { wQ = Q 1 0 0 0, wM = matI, wSteps = 0, wErrHist = [] }

main :: IO ()
main = runSketch (sketch "57-08 Arcball and accumulated error" world0)
  { skBack  = rgbC 15 18 32
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 行列の成分を float32 に丸める（GPU やゲームでよく使う精度。Math.fround と同じ）
f32m :: Mat -> Mat
f32m (Mat a b c) = Mat (r a) (r b) (r c)
  where r (Vec x y z) = Vec (f x) (f y) (f z)
        f = float2Double . double2Float

-- [ω]× M（ω × 各列）
skewMul :: Vec -> Mat -> Mat
skewMul w m = let Mat c0 c1 c2 = matT m in matT (Mat (v3Cross w c0) (v3Cross w c1) (v3Cross w c2))

matAdd :: Mat -> Mat -> Mat
matAdd (Mat a b c) (Mat d e f) = Mat (v3Add a d) (v3Add b e) (v3Add c f)

-- 角速度ベクトル w（= 軸 × 角）の小さな回転を 1 回積み重ねる
applyStep :: Vec -> World -> World
applyStep v@(Vec vx vy vz) w =
  let (q1, m1) | wMethod w == 1 = let dq = qAxisAngle v (v3Len v)
                                  in (qMul dq (wQ w), f32m (matMul (qToMat dq) (wM w)))
               | otherwise = ( qAdd (wQ w) (qScl (qMul (Q 0 vx vy vz) (wQ w)) 0.5)       -- q ← q + ½ ω q Δt（1 次の近似）
                             , f32m (matAdd (wM w) (skewMul v (wM w))) )
  in w { wQ = if wNormOn w then qUnit q1 else q1, wM = m1, wSteps = wSteps w + 1 }

tooFar :: World -> Bool                -- ずれすぎたか
tooFar w = matOrthoErr (wM w) > 3 || qNorm (wQ w) > 4

step :: Env -> World -> World
step env w0' =
  let w1 = if tooFar w0' then w0' { wAuto = False } else w0'      -- ずれすぎたら止める（R でやり直し）
      spin ww = let t = fromIntegral (wSteps ww) * 0.002
                in applyStep (v3Scl (v3Unit (Vec (sin t) (cos (1.3 * t)) 0.6)) omegaDt) ww
      w2 = if wAuto w1 then foldl' (\ww _ -> spin ww) w1 [1 .. perFrame] else w1
      w3 = case wDragFrom w2 of
        Just from | envDown env ->
          let to = arcballVec env (mouseP5 env)
              Q d0 d1 d2 d3 = arcballQ from to
              ang = 2 * acos (clampv d0 (-1) 1)
              w' = if ang > 1e-6 && matOrthoErr (wM w2) < 3 && qNorm (wQ w2) < 4
                     then applyStep (v3Scl (v3Unit (Vec d1 d2 d3)) ang) w2 else w2
          in w' { wDragFrom = Just to }
        _ -> w2
      eq = maximum [1e-17, matOrthoErr (qToMat (qUnit (wQ w3))), abs (qNorm (wQ w3) - 1)]
      em = max 1e-17 (matOrthoErr (wM w3))
  in if envFrame env `mod` 3 == 0
       then w3 { wErrHist = take 400 ((fromIntegral (wSteps w3), fin 10 eq, fin 10 em) : wErrHist w3) }
       else w3

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawItems $
      glParts env sc (vF . qRot (wQ w)) (Vec (-d) (-0.1) 0) 255 cube
      ++ glParts env sc (vF . matVec (wM w)) (Vec d (-0.1) 0) 255 cube
  , hud ]
  where
    sc = realToFrac (min (envW env) (envH env)) * 0.12
    vF = qRot viewQ
    d = realToFrac (envW env) * 0.22 / sc
    cube = [ Part (Vec 0 0 0) (Vec 0.9 0.9 0.9) (230, 230, 235), Part (Vec 0 0 0.95) (Vec 0.3 0.3 0.08) (240, 110, 90)
           , Part (Vec 0.95 0 0) (Vec 0.08 0.3 0.3) (90, 200, 235), Part (Vec 0 0.95 0) (Vec 0.3 0.08 0.3) (245, 200, 80) ]
    ww = envW env; hh = envH env
    qn = qNorm (wQ w)
    oe = matOrthoErr (wM w)
    hist = reverse (wErrHist w)
    hx0 = case hist of
      ((s, _, _) : _) -> s
      [] -> 0
    gr = G (ww - 380) 120 340 150 hx0 (maximum [1, fromIntegral (wSteps w), hx0 + 1]) 1e-17 10 True
    hud = Pictures $
      [ say ("Arcball and error   " ++ (if wMethod w == 1 then "1: exact small turns with a float32 matrix" else "2: small-angle approximate update")) 20 34 20 (grayC 240)
      , say ("Drag: turn   Space: keep turning (" ++ show perFrame ++ " per frame)   1 2: update method   N: normalize q "
             ++ (if wNormOn w then "ON" else "OFF") ++ "   R reset") 20 60 15 (grayC 175)
      , sayC (if wNormOn w then "accumulated in quaternion q (|q| = 1 restored each time)" else "accumulated in quaternion q (not restored)")
             (ww * 0.28) (hh - 170) 17 (rgbC 130 230 150)
      , sayC "keep multiplying rotation matrix M (not restored)" (ww * 0.72) (hh - 170) 17 (rgbC 255 170 120)
      , sayC ("|q| - 1 = " ++ expo 2 (qn - 1)) (ww * 0.28) (hh - 144) 15 (grayC 220)
      , sayC ("|M^T M - I| = " ++ expo 2 oe ++ "   det M = " ++ fmt 6 (matDet (wM w))) (ww * 0.72) (hh - 144) 15 (grayC 220)
      -- 誤差のグラフ（片対数）
      , plotAxes gr "steps" "error (log)" [gx0 gr, gx1 gr] [1e-16, 1e-8, 1]
      , Color (rgbC 130 230 150) (polyline 2 [ (gX gr s, gY gr e) | (s, e, _) <- hist ])
      , Color (rgbC 255 170 120) (polyline 2 [ (gX gr s, gY gr e) | (s, _, e) <- hist ])
      , say ("steps = " ++ show (wSteps w) ++ (if tooFar w then "   drifted too far, stopped -> R to restart" else "")) 20 100 14 (grayC 200)
      , say (if wMethod w == 1 then "Even with exact turns, float32 rounding piles up and M slowly stops being a rotation"
             else "The approximate update stretches by (1 + (w dt)^2) each time -> the box swells and warps") 20 124 14 (grayC 200)
      , say "A quaternion goes back to |q| = 1 with one division of 4 numbers (a matrix needs 9 numbers re-orthogonalized)" 20 148 14 (grayC 200)
      , drawSelfTest env (wTest w) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDragFrom = Just (arcballVec env (mouseP5 env)) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragFrom = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c == '1' -> resetAll w { wMethod = 1 }
    | c == '2' -> resetAll w { wMethod = 2 }
    | c `elem` "nN" -> w { wNormOn = not (wNormOn w) }
    | c `elem` "rR" -> resetAll w
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("the arcball dq moves point a onto point b", e1 < 1e-12)
  , ("matrix of the accumulated q = accumulated matrix (float64, 20000 steps, difference " ++ expo 1 (matDiff (qToMat q) m64) ++ ")"
    , matDiff (qToMat q) m64 < 1e-10)
  , ("exact small turns in float64: matrix drift is small (" ++ expo 1 (matOrthoErr m64) ++ "), in float32 large (" ++ expo 1 (matOrthoErr m32) ++ ")"
    , matOrthoErr m64 < 1e-11 && matOrthoErr m32 > 100 * matOrthoErr m64)
  , ("approximate update 10000 times: the matrix drifts a lot (" ++ expo 1 (matOrthoErr ma) ++ ") but the normalized q stays a rotation"
    , matOrthoErr ma > 0.1 && matOrthoErr (qToMat qa) < 1e-14 && abs (qNorm qa - 1) < 1e-15)
  , ("without normalizing |q| grows too: |q| = " ++ fmt 3 (qNorm qb) ++ " = (1 + (w dt/2)^2)^(n/2)"
    , near (qNorm qb) ((1 + (omegaDt * omegaDt) / 4) ** 5000) 1e-9) ]
  where
    unitN = do
      a <- rndNormalG; b <- rndNormalG; c <- rndNormalG
      pure (v3Unit (Vec a b c))
    (pairs, ws) = evalGen 8 $ do
      ps <- replicateM 100 ((,) <$> unitN <*> unitN)
      vs <- replicateM 20000 (fmap (`v3Scl` omegaDt) unitN)
      pure (ps, vs)
    e1 = maximum (0 : [ v3Dist (qRot (arcballQ a b) a) b | (a, b) <- pairs, v3Dot a b > -0.99 ])
    (q, m64, m32) = foldl' (\(!q0, !ma0, !mb0) v -> let dq = qAxisAngle v (v3Len v)
                                                   in (qUnit (qMul dq q0), matMul (qToMat dq) ma0, f32m (matMul (qToMat dq) mb0)))
                           (Q 1 0 0 0, matI, matI) ws
    pq0 (Vec x y z) = Q 0 x y z
    (qa, ma) = foldl' (\(!q0, !m0) v -> (qUnit (qAdd q0 (qScl (qMul (pq0 v) q0) 0.5)), matAdd m0 (skewMul v m0)))
                      (Q 1 0 0 0, matI) (take 10000 ws)
    qb = foldl' (\ !q0 v -> qAdd q0 (qScl (qMul (pq0 v) q0) 0.5)) (Q 1 0 0 0) (take 10000 ws)

-- ---------- やってみよう ----------
-- 1. 2 を押してからスペース。右の箱はどうなる？ 左の箱は？ グラフのオレンジ線は？
-- 2. N で四元数の正規化を OFF にすると、左の箱はどうなる？（|q| がのびると q v q* は |q|² 倍にのびる）
-- 3. 1 にもどしてスペース。float32 のずれはどのくらいの速さで増える？（回数を 10 倍にすると誤差は？）
-- 4. 行列を直交化する（グラム-シュミット）コードを書いて、右の箱にも使ってみよう
-- 5. arcballQ の (1 + a·b, a × b) は「a から b への回転の半分の角」の四元数。なぜ 1 + a·b なのか考えよう

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
