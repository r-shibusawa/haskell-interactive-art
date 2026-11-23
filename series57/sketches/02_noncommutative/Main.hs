-- ============================================
-- 57-02 掛け算が交換しない ─ pq ≠ qp
-- 四元数 = スカラー部 + ベクトル部。積は pq = (p₀q₀ − p⃗·q⃗,  p₀q⃗ + q₀p⃗ + p⃗×q⃗)：内積と外積が両方入っている
-- 外積だけが順番で符号を変えるので、pq − qp = 2(p⃗ × q⃗)。ベクトル部が平行なら交換できる
-- 操作：スライダをドラッグ p と q の 4 成分 / R ランダム / 0 q⃗ を p⃗ と平行に / スペース 3D の図を回す / ← → 図の向き
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

vals0 :: [Double]
vals0 = [0.6, 1.0, 0.4, -0.3, -0.4, 0.2, 1.1, 0.5]   -- ★ p = 0〜3 番、q = 4〜7 番

labels :: [String]
labels = ["p0", "p1 (i)", "p2 (j)", "p3 (k)", "q0", "q1 (i)", "q2 (j)", "q3 (k)"]

data World = World
  { wVals     :: ![Double]
  , wDragIdx  :: !Int                  -- つかんでいるスライダ（−1 = なし）
  , wViewAng  :: !Double
  , wSpinning :: !Bool
  , wTest     :: [(String, Bool)] }

world0 :: World
world0 = World vals0 (-1) 0.6 True []

main :: IO ()
main = runSketch (sketch "57-02 Multiplication does not commute" world0)
  { skBack  = rgbC 17 20 36
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 積をスカラー部とベクトル部の式で組み立てる：(スカラー部, ベクトル部, 内積, 外積)
parts :: Q -> Q -> (Double, Vec, Double, Vec)
parts (Q p0 p1 p2 p3) (Q q0 q1 q2 q3) =
  let pv = Vec p1 p2 p3
      qv = Vec q1 q2 q3
      cr = v3Cross pv qv
  in (p0 * q0 - v3Dot pv qv, v3Add (v3Add (v3Scl qv p0) (v3Scl pv q0)) cr, v3Dot pv qv, cr)

vecOf :: Q -> Vec                      -- ベクトル部
vecOf (Q _ x y z) = Vec x y z

pureQ :: Vec -> Q                      -- 純四元数 (0, v)
pureQ (Vec x y z) = Q 0 x y z

-- スライダ n の左はし (x, y) と幅
slider :: Int -> (Float, Float, Float)
slider n = (40, 140 + fromIntegral n * 52 + (if n >= 4 then 24 else 0), 300)

pqOf :: [Double] -> (Q, Q)
pqOf vs = let g = getAt 0 vs in (Q (g 0) (g 1) (g 2) (g 3), Q (g 4) (g 5) (g 6) (g 7))

step :: Env -> World -> World
step env w0' =
  let w1 = if wSpinning w0' then w0' { wViewAng = wViewAng w0' + 0.006 } else w0'
      i = wDragIdx w1
      (sx, _, sw) = slider i
      (mx, _) = mouseP5 env
      v = jsRound (clampv (((mx - realToFrac sx) / realToFrac sw) * 4 - 2) (-2) 2 * 20) / 20
  in if i >= 0 then w1 { wVals = [ if k == i then v else x | (k, x) <- zip [0 ..] (wVals w1) ] } else w1

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- スライダ
  concat [ [ Color (rgbC 70 80 120) (thickLine 4 (sx, sy) (sx + sw, sy))
           , Color (if n < 4 then rgbC 110 170 255 else rgbC 255 150 90) (thickLine 4 (sx + sw / 2, sy) (sx + t * sw, sy))
           , Color white (at (sx + t * sw, sy) (disc 9))
           , say (getAt "" labels n ++ " = " ++ fmt 2 v) sx (sy - 14) 14 (grayC 200) ]
         | (n, v) <- zip [0 :: Int ..] (wVals w), let (sx, sy, sw) = slider n, let t = f2 ((v + 2) / 4) ]
  -- ---- 3D の図：p⃗（青）、q⃗（橙）、p⃗×q⃗（緑）、q⃗×p⃗（赤）
  ++ [ line3 (rgbC 70 80 120) 1 (vv (v3Scl e (-1))) (vv e) vw | e <- [Vec 2.2 0 0, Vec 0 2.2 0, Vec 0 0 2.2] ]
  ++ [ arrow3 (vv (vecOf p)) (rgbC 110 170 255) "p"
     , arrow3 (vv (vecOf q)) (rgbC 255 150 90) "q"
     , arrow3 (vv cr) (rgbC 130 230 150) "p x q"
     , arrow3 (vv (v3Scl cr (-1))) (rgbC 255 110 110) "q x p"
     -- p⃗ と q⃗ の作る平行四辺形（面積 = |p⃗×q⃗|）
     , Color (rgbaC 255 255 255 30) (Polygon [ projXY (vv c) vw | c <- [Vec 0 0 0, vecOf p, v3Add (vecOf p) (vecOf q), vecOf q] ]) ]
  -- ---- 右の表
  ++ [ say ("p q = " ++ qStr 3 pq) tx ty 16 (grayC 240)
     , say ("q p = " ++ qStr 3 qp) tx (ty + 26) 16 (grayC 240)
     , say ("pq - qp = " ++ qStr 3 diff) tx (ty + 52) 16 (rgbC 255 150 150)
     , say ("2(p x q) = " ++ qStr 3 (pureQ (v3Scl cr 2))) tx (ty + 78) 16 (rgbC 130 230 150)
     , say "Split the product" tx (ty + 124) 15 (rgbC 255 210 110)
     , say ("scalar  p0q0 - p.q = " ++ fmt 3 (p0 * q0) ++ " - " ++ fmt 3 dt ++ " = " ++ fmt 3 s) tx (ty + 150) 14 (grayC 210)
     , say "vector  p0 q + q0 p + p x q" tx (ty + 174) 14 (grayC 210)
     , say ("  = " ++ vStr (v3Add (v3Scl (vecOf q) p0) (v3Scl (vecOf p) q0)) ++ " + " ++ vStr cr) tx (ty + 196) 14 (grayC 210)
     , say ("  = " ++ vStr v ++ "  <- same as qMul") tx (ty + 218) 14 (grayC 210)
     , say (if par then "p, q parallel -> cross product 0 -> pq = qp (they commute)"
                   else "|p x q| = " ++ fmt 3 (v3Len cr) ++ "  (area of the parallelogram)")
           tx (ty + 260) 15 (if par then rgbC 130 230 150 else grayC 200)
     , sayBox "Only i, j, k: ij = k (cross product x * y = z), i*i = -1 (minus the dot product 1). The quaternion product contains both the dot and the cross product from the start."
              tx (ty + 290) (ww - tx - 20) 14 (grayC 170)
     , say "Multiplication does not commute   pq - qp = 2(p x q)" 20 36 20 (grayC 240)
     , say "Drag the sliders: p (blue) and q (orange)   R random   0: make q parallel to p   Space: spin the 3D view on/off" 20 62 15 (grayC 175)
     , say "Green = p x q, red = q x p (opposite). The difference pq - qp is twice the cross product" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    (p, q) = pqOf (wVals w)
    Q p0 _ _ _ = p
    Q q0 _ _ _ = q
    pq = qMul p q
    qp = qMul q p
    diff = qSub pq qp
    (s, v, dt, cr) = parts p q
    par = v3Len cr < 1e-9
    vw = View2 (realToFrac ww * 0.44) (realToFrac hh * 0.55) (realToFrac (min ww hh) * 0.12) 9
    vq = qMul (qAxisAngle (Vec 1 0 0) 0.35) (qAxisAngle (Vec 0 1 0) (wViewAng w))
    vv = qRot vq
    tx = ww * 0.66; ty = 130
    vStr (Vec a b c) = "(" ++ intercalate ", " (map (fmt 3) [a, b, c]) ++ ")"
    arrow3 e col lab =
      let o@(ox, oy) = projXY (Vec 0 0 0) vw
          ep@(ex, ey) = projXY e vw
          a = atan2 (ey - oy) (ex - ox)
          hd d = (ex - 12 * cos (a + d), ey - 12 * sin (a + d))
      in Color col $ Pictures $ thickLine 3 o ep :
           (if sqrt ((ex - ox) ^ (2 :: Int) + (ey - oy) ^ (2 :: Int)) < 4 then []
            else [Polygon [ep, hd (-0.4), hd 0.4], say lab (ex + 8) (ey - 6) 16 col])

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        hits = [ n | n <- [0 .. 7], let (sx, sy, sw) = slider n
                   , abs (my - realToFrac sy) < 16 && mx > realToFrac sx - 12 && mx < realToFrac (sx + sw) + 12 ]
    in pure (case reverse hits of
               (n : _) -> w { wDragIdx = n }
               [] -> w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragIdx = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> do
        vs <- mapM (const (randomRIO (0, 1 :: Double))) (wVals w)
        pure w { wVals = [ jsRound ((r * 4 - 2) * 20) / 20 | r <- vs ] }
    | c == '0' -> pure w { wVals = [ if k >= 5 then jsRound (getAt 0 (wVals w) (k - 4) * 0.7 * 100) / 100 else x
                                   | (k, x) <- zip [0 :: Int ..] (wVals w) ] }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpinning = not (wSpinning w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wViewAng = wViewAng w + 0.2 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wViewAng = wViewAng w - 0.2 }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("pq = (p0q0 - p.q, p0 q + q0 p + p x q) (200 pairs)", e1 < 1e-12)
  , ("pq - qp = 2(0, p x q)", e2 < 1e-12)
  , ("pq and qp have the same scalar part", e3 < 1e-12)
  , ("if the vector parts are parallel, pq = qp", e4 < 1e-12)
  , ("(pq)* = q* p* (the order is swapped)", e5 < 1e-12)
  , ("pure quaternions: uw = -u.w + u x w, u^2 = -|u|^2"
    , near uw0 (-(v3Dot (vecOf u) (vecOf w'))) 1e-12 && v3Dist (vecOf uw) (v3Cross (vecOf u) (vecOf w')) < 1e-12 && qDist (qMul u u) (Q (-6) 0 0 0) < 1e-12) ]
  where
    trials = evalGen 3 $ replicateM 200 $ do
      p <- qScl <$> qRandom <*> ((2 *) <$> rndG)
      q <- qScl <$> qRandom <*> ((2 *) <$> rndG)
      k <- rndNormalG
      pure (p, q, k)
    errs = [ ( qDist pq (Q s vx vy vz)
             , qDist (qSub pq qp) (pureQ (v3Scl cr 2))
             , abs (qa pq - qa qp)
             , let Q q0 _ _ _ = q
                   Vec a b c = v3Scl (vecOf p) k
                   qpar = Q q0 a b c
               in qDist (qMul p qpar) (qMul qpar p)
             , qDist (qConj pq) (qMul (qConj q) (qConj p)) )
           | (p, q, k) <- trials
           , let (s, Vec vx vy vz, _, cr) = parts p q
           , let pq = qMul p q
           , let qp = qMul q p ]
    qa (Q a _ _ _) = a
    e1 = maximum (0 : [ x | (x, _, _, _, _) <- errs ])
    e2 = maximum (0 : [ x | (_, x, _, _, _) <- errs ])
    e3 = maximum (0 : [ x | (_, _, x, _, _) <- errs ])
    e4 = maximum (0 : [ x | (_, _, _, x, _) <- errs ])
    e5 = maximum (0 : [ x | (_, _, _, _, x) <- errs ])
    u = Q 0 1 2 (-1)
    w' = Q 0 (-3) 0.5 2
    uw@(Q uw0 _ _ _) = qMul u w'

-- ---------- やってみよう ----------
-- 1. 0 キーで q⃗ を p⃗ と平行にしよう。pq − qp はいくつ？ 複素数（ベクトル部が 1 方向だけ）が交換できる理由もこれ
-- 2. p₀ と q₀ だけ動かしても pq − qp は変わらない。なぜ？
-- 3. p = (0, 1, 0, 0)（= i）、q = (0, 0, 1, 0)（= j）にしよう。pq と qp は？ 01 の表と比べよう
-- 4. p⃗ と q⃗ を直角にすると |pq − qp| が一番大きくなる。平行四辺形の面積と比べよう
-- 5. ベクトル解析の内積・外積は、四元数の積を 2 つに分けたもの（ギブズとヘヴィサイドが 19 世紀の終わりに整理した）

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
