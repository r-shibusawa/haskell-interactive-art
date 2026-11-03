-- ============================================
-- 07 ボード線図（周波数応答）
-- 正弦波を入れると、同じ周波数の正弦波が「小さくなって・遅れて」出てくる
--   ゲイン |G(jω)|（何倍になるか、dB）と 位相 ∠G(jω)（どれだけ遅れるか、度）を ω ごとに描いたのがボード線図
-- マウス左右 = 周波数 ω（対数）/ 1〜3 でシステム（一次遅れ・二次系・PID 付きループ）
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.List (foldl', mapAccumL)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

tauSys, wn, zeta, kP, kI, kD :: Double
tauSys = 1.0   -- ★ 一次遅れの時定数
wn = 2.0       -- ★ 二次系の固有角周波数
zeta = 0.2     -- ★ 二次系の減衰比（小さいと共振で山ができる）
kP = 3         -- ★ PID 付きループのゲイン
kI = 2
kD = 0.5

data World = World
  { wSys    :: !Int     -- 1〜3：どのシステムか
  , wMouseX :: !Float   -- マウスの横位置（p5.js と同じく最初は 0）
  , wAnim   :: !Float   -- 波を流すための時間
  , wGains  :: !(U.Vector Float)   -- ゲイン [dB]（ω ごと）
  , wPhases :: !(U.Vector Float)   -- 位相 [deg]（ω ごと）
  }

-- 伝達関数の分子・分母（昇べきの係数：c0 + c1 s + c2 s² …）
makeSystem :: Int -> ([Double], [Double])
makeSystem 1 = ([1], [1, tauSys])
makeSystem 2 = ([wn * wn], [wn * wn, 2 * zeta * wn, 1])
makeSystem _ = ([kI, kP, kD], polyMul [0, 1] (polyMul [1, 1] [1, 0.2]))  -- C = (Kd s² + Kp s + Ki)/s

sysName :: Int -> String
sysName 1 = "1st order G = 1/(tau s + 1)"
sysName 2 = "2nd order G = wn^2/(s^2 + 2 zeta wn s + wn^2)"
sysName _ = "PID loop L = C(s)P(s), P = 1/((s+1)(0.2s+1))"

setSys :: Int -> World -> World
setSys s w =
  let (gs, ps) = bodeData s
  in w { wSys = s, wGains = gs, wPhases = ps }

-- ボード線図のデータ：ω を対数で 0.1〜100 まで動かす
bodeData :: Int -> (U.Vector Float, U.Vector Float)
bodeData s =
  let (num, den) = makeSystem s
      n = 240 :: Int
      ws = [ 10 ** (fromIntegral i / fromIntegral n * 3 - 1) | i <- [0 .. n] ] :: [Double]
      gs = map evalAt ws
      evalAt w = evalTF num den w
      gains = [ realToFrac (20 * logBase 10 (max (cabs g) 1e-9)) | g <- gs ]
      -- 位相がとびとびにならないようにつなぐ
      unwrap Nothing p = let q = if p > 0 && s /= 3 then p - 360 else p in (Just q, q)
      unwrap (Just prev) p = let q = wrapNear prev p in (Just q, q)
      (_, phases) = mapAccumL unwrap Nothing (map phaseDeg gs)
  in (U.fromList gains, U.fromList (map realToFrac phases))

wrapNear :: Double -> Double -> Double
wrapNear prev p
  | p - prev > 180 = wrapNear prev (p - 360)
  | p - prev < -180 = wrapNear prev (p + 360)
  | otherwise = p

phaseDeg :: Cx -> Double
phaseDeg (re, im) = atan2 im re * 180 / pi

world0 :: World
world0 = setSys 1 (World 1 0 0 U.empty U.empty)

main :: IO ()
main = runSketch (sketch "20-07 Bode Plot" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure w { wAnim = wAnim w + 0.04 }
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ -- 左：入力と出力の波（横軸は位相 ωt、2 周期分）
    box ax ay aw ah (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (ax, ay + ah / 2) (ax + aw, ay + ah / 2)
  , Color white (thickPath 2 (wave (\a -> sin a)))
  , Color (hsb 30 80 100) (thickPath 3 (wave (\a -> dispGain * sin (a + phaseR))))
    -- 右端の「いま」の値をおもりで表す（入力＝手、出力＝つながれた物体）
  , dot (ax + aw + 30) (ay + ah / 2 - vIn * amp) 18 white
  , dot (ax + aw + 60) (ay + ah / 2 - vOut * amp) 18 (hsb 30 80 100)
  , txt (ax + 8) (ay + 20) 14 (hsb 0 0 70) ("Input sin(wt) white, output orange  w = " ++ nf 2 w ++ " rad/s")
    -- 右：ボード線図（ゲインと位相）
  , drawBode bx by1 bw bh wGains (-60) 40 "Gain [dB]  20 log10 |G(jw)|" [0, -20, -40] 120
  , drawBode bx by2 bw bh wPhases (-270) 90 "Phase [deg]  arg G(jw)" [0, -90, -180] 200
    -- 選んだ ω の印
  , seg 2 yel (mx, by1) (mx, by1 + bh)
  , seg 2 yel (mx, by2) (mx, by2 + bh)
  , dot mx (remap (clampF (-60) 40 gainDb) (-60) 40 (by1 + bh) by1) 10 yel
  , dot mx (remap (clampF (-270) 90 phase) (-270) 90 (by2 + bh) by2) 10 yel
  , txt 20 40 18 white ("1-3: " ++ sysName wSys ++ "   w = " ++ nf 2 w ++ " rad/s (mouse L/R)")
  , txt 20 68 15 grayText ("Gain " ++ nf 3 gain ++ " x (" ++ nf 1 gainDb ++ " dB)   Phase " ++ nf 1 phase ++ " deg"
        ++ (if phase < 0 then " (lag)" else "") ++ "   Horizontal axis is log: 0.1 to 100 rad/s")
  , txt 20 (ht - 30) 15 grayText "Low w passes; high w gets smaller and later (low-pass). 1st order: -3 dB, -45 deg at w = 1/tau. 2nd: peak at wn."
  ]
  where
    wd = envW env
    ht = envH env
    w = 10 ** remap (clampF 0 wd wMouseX) 0 wd (-1) 2 :: Float
    (num, den) = makeSystem wSys
    g = evalTF num den (realToFrac w)
    gain = realToFrac (cabs g) :: Float
    gainDb = 20 * logBase 10 (max gain 1e-9)
    ph0 = realToFrac (phaseDeg g) :: Float
    phase = if ph0 > 0 && wSys /= 3 then ph0 - 360 else ph0
    phaseR = phase * pi / 180
    ax = wd * 0.05
    ay = ht * 0.2
    aw = wd * 0.4
    ah = ht * 0.4
    amp = ah * 0.2
    dispGain = min gain 2.2
    wave f = [ ( remap (fromIntegral i) 0 200 ax (ax + aw)
               , ay + ah / 2 - f (remap (fromIntegral i) 0 200 0 (4 * pi) - wAnim) * amp )
             | i <- [0 .. 200 :: Int] ]
    vIn = sin (4 * pi - wAnim)
    vOut = dispGain * sin (4 * pi - wAnim + phaseR)
    bx = wd * 0.52
    bw = wd * 0.44
    by1 = ht * 0.18
    bh = ht * 0.3
    by2 = ht * 0.55
    mx = remap (logBase 10 w) (-1) 2 bx (bx + bw)
    yel = hsb 45 90 100
    grayText = hsb 0 0 60

drawBode :: Float -> Float -> Float -> Float -> U.Vector Float -> Float -> Float -> String -> [Float] -> Float -> Picture
drawBode x y w h dat lo hi title gridVals hue = Pictures $
  [ box x y w h (hsb 0 0 14) ]
  ++ [ seg 1 (hsb 0 0 28) (xOfD d, y) (xOfD d, y + h) | d <- decades ]
  ++ [ seg 1 (hsb 0 0 28) (x, yOf gv) (x + w, yOf gv) | gv <- gridVals ]
  ++ [ txt (xOfD d + (if d >= 2 then -30 else 3)) (y + h - 4) 14 (hsb 0 0 60) s | (d, s) <- zip decades ["0.1", "1", "10", "100"] ]
  ++ [ txt (x + 3) (yOf gv - 3) 14 (hsb 0 0 60) (show (round gv :: Int)) | gv <- gridVals ]
  ++ [ Color (hsb hue 70 100) (thickPath 2.5 pts)
     , txt (x + 8) (y + 20) 14 (hsb 0 0 70) title ]
  where
    decades = [-1, 0, 1, 2] :: [Float]
    xOfD d = remap d (-1) 2 x (x + w)
    yOf v = remap v lo hi (y + h) y
    n = U.length dat
    pts = [ (remap (fromIntegral i) 0 (fromIntegral (max 1 (n - 1))) x (x + w), yOf (clampF lo hi v))
          | (i, v) <- zip [0 :: Int ..] (U.toList dat) ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventMotion p -> w { wMouseX = fst (toP5 env p) }
  EventKey (MouseButton LeftButton) Down _ p -> w { wMouseX = fst (toP5 env p) }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> setSys (fromEnum c - fromEnum '0') w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 一次遅れで ω = 1/τ（= 1）にマウスを置く。ゲイン −3 dB、位相 −45° を確かめる
-- 2. 二次系の ζ を 0.05 にすると山はどれだけ高くなる？ ζ = 0.7 では？
-- 3. PID 付きループ（3）で、位相が −180° になる ω でのゲインが 0 dB より下なら安定（→ 09 のゲイン余裕）
-- 4. 低い ω でゲインが上がっていく（積分 1/s）のが 3 の特徴。I があると「ゆっくりの誤差」に強い

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- ---------- 複素数と多項式 ----------
-- 複素数は (実部, 虚部)。根の計算は桁が必要なので Double を使う
type Cx = (Double, Double)

cmul, cdiv, csub :: Cx -> Cx -> Cx
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
cdiv (a, b) (c, d) = let e = c * c + d * d + 1e-30 in ((a * c + b * d) / e, (b * c - a * d) / e)
csub (a, b) (c, d) = (a - c, b - d)

cabs :: Cx -> Double
cabs (a, b) = sqrt (a * a + b * b)

-- 多項式は昇べきの係数のリスト：c0 + c1 s + c2 s² …
polyMul :: [Double] -> [Double] -> [Double]
polyMul a b = foldr (\ai acc -> zipLong (map (* ai) b) (0 : acc)) [] a

zipLong :: [Double] -> [Double] -> [Double]
zipLong (x : xs) (y : ys) = (x + y) : zipLong xs ys
zipLong xs [] = xs
zipLong [] ys = ys

polyAdd :: [Double] -> [Double] -> [Double]
polyAdd = zipLong

polyScale :: [Double] -> Double -> [Double]
polyScale a k = map (* k) a

-- 複素数 s で評価（ホーナー法）
polyEvalC :: [Double] -> Cx -> Cx
polyEvalC c s = foldr (\ci acc -> let (re, im) = cmul acc s in (re + ci, im)) (0, 0) c

-- 伝達関数 G(jω) = num(jω) / den(jω)
evalTF :: [Double] -> [Double] -> Double -> Cx
evalTF num den w = cdiv (polyEvalC num (0, w)) (polyEvalC den (0, w))
