-- ============================================
-- 55-12 気体と液体（ファン・デル・ワールス）─ 分子の大きさ b と引き合う力 a を入れた状態方程式
-- (P + a/V²)(V − b) = RT。臨界点で割った「換算単位」では P = 8T/(3V − 1) − 3/V²（どの気体でも同じ形）
-- 臨界温度より低いと等温線が波打つ → 本当は液体と気体が共存して圧力が一定（水平な線）
-- その高さは「上と下の面積が等しい」所（マクスウェルの等面積則）。二分法で数値的に求める
-- 操作：↑↓ 温度 / マウスを左右 体積を動かす / M 波打つ線 ⇔ 共存の線 / I 理想気体の線 / スペース 共存の範囲（ドーム）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.Fixed (mod')
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

tList :: [Double]
tList = [0.8, 0.85, 0.9, 0.95, 1, 1.05, 1.1, 1.2]   -- ★ うすく描く等温線

co2A, co2B, rGas :: Double
co2A = 0.364               -- ★ 二酸化炭素の a [Pa·m⁶/mol²]
co2B = 4.267e-5            -- ★ 二酸化炭素の b [m³/mol]
rGas = 8.314

data World = World
  { wT        :: !Double     -- ★ 温度 T/Tc
  , wShowLoop :: !Bool
  , wIdeal    :: !Bool
  , wDome     :: !Bool
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-12 Gas and liquid (van der Waals)" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.9 False True True []

pVdw, pInt, pIdeal, dP :: Double -> Double -> Double
pVdw v t = (8 * t) / (3 * v - 1) - 3 / (v * v)
pInt v t = ((8 * t) / 3) * log (3 * v - 1) + 3 / v      -- ∫P dV（式で）
pIdeal v t = (8 * t) / (3 * v)
dP v t = (-24 * t) / (3 * v - 1) ** 2 + 6 / v ** 3

-- 二分法：f の符号が変わる所を n 回で
bisect :: (Double -> Double) -> Double -> Double -> Int -> Double
bisect f lo0 hi0 n0 = go n0 lo0 hi0 (f lo0)
  where go 0 lo hi _ = (lo + hi) / 2
        go k lo hi flo = let m = (lo + hi) / 2
                             fm = f m
                         in if fm * flo > 0 then go (k - 1) m hi fm else go (k - 1) lo m flo

-- 波の山と谷（dP/dV = 0 の 2 点）
spinodal :: Double -> (Double, Double)
spinodal t = (bisect (`dP` t) 0.34 1 80, bisect (`dP` t) 1 30 80)

-- 圧力 p の水平線と等温線の交点（左・右）
roots :: Double -> Double -> (Double, Double) -> (Double, Double)
roots p t (s0, s1) = (bisect (\v -> pVdw v t - p) 0.3334 s0 100, bisect (\v -> pVdw v t - p) s1 5000 100)

-- 共存：圧力 p*、液体の V、気体の V
data Mx = Mx { mP, mVl, mVg :: !Double }

-- マクスウェルの等面積則：∫(P − p*)dV = 0 になる p* を二分法で
maxwell :: Double -> Maybe Mx
maxwell t
  | t >= 1 = Nothing
  | otherwise = let (vl, vg) = roots p t sp in Just (Mx p vl vg)
  where
    sp = spinodal t
    pLo = max 1e-6 (pVdw (fst sp) t)
    pHi = pVdw (snd sp) t
    area q = let (a, b) = roots q t sp in pInt b t - pInt a t - q * (b - a)
    p = bisect area (pLo + 1e-9) (pHi - 1e-9) 80

-- 共存の範囲（ドーム）：T = 0.8 から 0.004 ずつ
dome :: [Mx]
dome = go 0.8
  where go t | t >= 0.9999 = []
             | otherwise = maybe id (:) (maxwell t) (go (t + 0.004))

-- うすく描く等温線の共存（1 回だけ計算）
tListMx :: [(Double, Maybe Mx)]
tListMx = [ (t, maxwell t) | t <- tList ]

-- 状態方程式で計算した圧力（共存の所は水平な線）
pressure :: Maybe Mx -> Bool -> Double -> Double -> Double
pressure mm loop v t = case mm of
  Just m | not loop && v > mVl m && v < mVg m -> mP m
  _ -> pVdw v t

vs :: [Double]
vs = [ 0.4 * (5 / 0.4) ** (fromIntegral k / 299) | k <- [0 .. 299 :: Int] ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ plotAxes g "volume V/Vc (log)" "pressure P/Pc" [0.5, 1, 2, 5] [0, 0.5, 1, 1.5, 2] ]
  ++ [ Color (if t < 1 then rgbaC 120 170 255 70 else if t == 1 then rgbaC 255 255 255 120 else rgbaC 255 150 90 70)
             (Line [ (gX g v, gY g (clampv (-0.1) 2.1 (pressure m' (wShowLoop w) v t))) | v <- vs ])
     | (t, m') <- tListMx ]
  -- 共存のドーム（帯に分けて塗る）
  ++ (if wDome w && not (null dome)
        then Color (rgbaC 120 200 255 30) (Pictures (zipWith strip dome (drop 1 dome)))
             : [ Color (rgbaC 120 200 255 30) (Polygon [pt (mVl d) (mP d), pt 1 1, pt (mVg d) (mP d)]) | d <- take 1 (reverse dome) ]
        else [])
  -- 等面積：水平線と波の間の 2 つの面積
  ++ concat [ [ Color (rgbaC 255 120 80 90) (Pictures (lobe (mVl m) (mP m) [ (v, max (-0.1) (pVdw v tr)) | v <- inside m, pVdw v tr < mP m ]))
              , Color (rgbaC 120 220 140 90) (Pictures (lobe' (mP m) [ (v, pVdw v tr) | v <- inside m, pVdw v tr >= mP m ]))
              , Color (rgbaC 255 255 255 120) (Line [ (gX g v, gY g (clampv (-0.1) 2.1 (pVdw v tr))) | v <- inside m ]) ]
            | Just m <- [mx] ]
  ++ [ Color (rgbC 255 210 120) (polyline 3 [ (gX g v, gY g (clampv (-0.1) 2.1 (pressure mx (wShowLoop w) v tr))) | v <- vs ]) ]
  ++ [ Color (rgbaC 200 200 200 120) (polyline 1.5 [ (gX g v, gY g (min 2.1 (pIdeal v tr))) | v <- vs ]) | wIdeal w ]
  ++ [ Color (rgbC 255 90 90) (at (gX g 1, gY g 1) (disc 5))
     , Color white (at (gX g vm, gY g (min pm 2.05)) (disc 6)) ]
  -- 容器の絵（液体と気体の割合：てこの規則）
  ++ [ Color (grayC 200) (frameTL 2 cx cy cw ch)
     , Color (rgbaC 80 140 230 200) (rectTL (cx + 2) (cy + ch - (ch - 4) * f2 liq * 0.5) (cw - 4) ((ch - 4) * f2 liq * 0.5)) ]
  ++ [ Color (rgbaC 230 230 240 160) (at (cx + 10 + (fromIntegral k * 53) `mod'` (cw - 20), cy + 10 + (fromIntegral k * 37) `mod'` max 10 ((ch - 20) * (1 - f2 liq * 0.5))) (disc 2.5))
     | k <- [0 :: Int .. ceiling (30 * (1 - liq)) - 1] ]
  ++ [ say (if liq == 1 then "Liquid" else if liq == 0 then (if tr >= 1 then "Supercritical fluid (no difference)" else "Gas")
            else "Liquid " ++ fmt 0 (liq * 100) ++ "% + gas " ++ fmt 0 (xg * 100) ++ "%") cx (cy + ch + 26) 15 (grayC 235)
     , say ("V/Vc = " ++ fmt 3 vm ++ "   P/Pc = " ++ fmt 4 pm) cx (cy + ch + 50) 15 (grayC 235) ]
  -- CO₂ の例（本当の単位）
  ++ [ say s cx (cy + ch + 90 + 22 * fromIntegral k) 14 (grayC 170)
     | (k, s) <- zip [0 :: Int ..]
         [ "CO2: Tc = 8a/(27Rb) = " ++ fmt 1 tcK ++ " K", "Pc = a/(27b^2) = " ++ fmt 2 pcM ++ " MPa"
         , "Now = " ++ fmt 1 (tr * tcK) ++ " K (" ++ fmt 1 (tr * tcK - 273.15) ++ " C)"
         , maybe "Above Tc: never liquefies" (\m -> "Coexistence P = " ++ fmt 2 (mP m * pcM) ++ " MPa") mx ] ]
  -- ---- 文字
  ++ [ say ("Van der Waals gas   T/Tc = " ++ fmt 3 tr ++ maybe "   (above Tc: one smooth curve)"
              (\m -> "   coexistence P* = " ++ fmt 4 (mP m) ++ "   liquid V = " ++ fmt 3 (mVl m) ++ "   gas V = " ++ fmt 3 (mVg m)) mx) 20 36 18 (grayC 240)
     , say ("Up/Down temperature   mouse volume   M " ++ (if wShowLoop w then "wavy loop" else "flat coexistence line")
            ++ "   I ideal gas (gray)   Space dome   | red dot = critical point") 20 62 15 (grayC 175)
     , say (maybe "Above Tc, squeezing never separates liquid and gas"
              (\m -> "Red and green areas are equal (Maxwell's equal-area rule). Difference = "
                     ++ expo 1 (pInt (mVg m) tr - pInt (mVl m) tr - mP m * (mVg m - mVl m))) mx) 20 86 15 (grayC 175) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    tr = wT w
    mx = maxwell tr
    g = (graph 90 110 (envW env * 0.62) (envH env - 220) 0.4 5 0 2) { logX = True }
    pt v p = (gX g v, gY g p)
    strip a b = Polygon [pt (mVl a) (mP a), pt (mVg a) (mP a), pt (mVg b) (mP b), pt (mVl b) (mP b)]
    inside m = filter (\v -> v > mVl m && v < mVg m) vs
    -- 水平線 p と曲線の間を、細い帯に分けて塗る
    lobe vl p pts = lobe' p ((vl, p) : pts)
    lobe' p pts = zipWith (\(v1, c1) (v2, c2) -> Polygon [pt v1 p, pt v1 c1, pt v2 c2, pt v2 p]) pts (drop 1 pts)
    -- マウスの体積の状態
    (mxP, _) = mouseP env
    vm = clampv 0.4 5 (0.4 * (5 / 0.4) ** realToFrac ((mxP - pX g) / pW g))
    pm = pressure mx False vm tr
    inCo = maybe False (\m -> vm > mVl m && vm < mVg m) mx
    xg = case mx of
           Just m | inCo -> (vm - mVl m) / (mVg m - mVl m)
           _ -> if vm < maybe 1 mVl mx then 0 else 1
    liq | inCo = 1 - xg
        | vm < maybe 1 mVl mx && tr < 1 = 1
        | otherwise = 0
    cx = pX g + pW g + 70
    cy = 140
    cw = envW env - cx - 40
    ch = 260
    tcK = (8 * co2A) / (27 * rGas * co2B)
    pcM = co2A / (27 * co2B * co2B) / 1e6

mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wT = min 1.3 (jsRound ((wT w + 0.01) * 1000) / 1000) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wT = max 0.8 (jsRound ((wT w - 0.01) * 1000) / 1000) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> w { wShowLoop = not (wShowLoop w) }
    | c `elem` "iI" -> w { wIdeal = not (wIdeal w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wDome = not (wDome w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("critical point (V, T) = (1, 1): P = 1, dP/dV = 0, d2P/dV2 = 0", near (pVdw 1 1) 1 1e-12 && abs d1 < 1e-12 && abs d2 < 1e-3)
  , ("CO2: Tc = 8a/(27Rb) = " ++ fmt 1 tcK ++ " K ~ 304 K, Pc ~ 7.4 MPa", abs (tcK - 304) < 1.5 && abs (pc / 1e6 - 7.4) < 0.1)
  , ("T = 0.9 coexistence: P* = " ++ fmt 4 (mP m) ++ " ~ 0.647, V liquid = " ++ fmt 3 (mVl m) ++ " < 1 < V gas = " ++ fmt 3 (mVg m)
    , abs (mP m - 0.647) < 0.002 && mVl m < 1 && mVg m > 1)
  , ("equal areas: trapezoid integral of (P - P*)dV ~ 0 (below 1e-4 of one lobe)", abs aa < 1e-4 * lobe)
  , ("both ends on the same pressure: P(V liquid) = P(V gas) = P*", near (pVdw (mVl m) 0.9) (mP m) 1e-9 && near (pVdw (mVg m) 0.9) (mP m) 1e-9)
  , ("near Tc the coexistence narrows (T = 0.99); above Tc (T = 1.1) no wave", mVg m2 - mVl m2 < mVg m - mVl m && mono)
  , ("dilute gas (V = 200) ~ ideal gas 8T/3V (1%)", abs (pVdw 200 0.9 / pIdeal 200 0.9 - 1) < 0.01) ]
  where
    h = 1e-3
    d1 = dP 1 1
    d2 = (pVdw (1 + h) 1 - 2 * pVdw 1 1 + pVdw (1 - h) 1) / (h * h)
    tcK = (8 * co2A) / (27 * rGas * co2B)
    pc = co2A / (27 * co2B * co2B)
    m = maybe (Mx 0 0 0) id (maxwell 0.9)
    n = 20000 :: Int
    wd = mVg m - mVl m
    aa = foldl' (\s k -> let a = mVl m + (wd * fromIntegral k) / fromIntegral n
                             b = a + wd / fromIntegral n
                         in s + 0.5 * (pVdw a 0.9 + pVdw b 0.9 - 2 * mP m) * (b - a)) 0 [0 .. n - 1]
    lobe = foldl' (\s k -> let a = mVl m + (wd * fromIntegral k) / fromIntegral n
                           in s + abs (pVdw a 0.9 - mP m) * (wd / fromIntegral n) / 2) 0 [0 .. n - 1]
    m2 = maybe (Mx 0 0 0) id (maxwell 0.99)
    mono = let go v | v >= 10 = True
                    | dP v 1.1 >= 0 = False
                    | otherwise = go (v + 0.01)
           in go 0.4

-- ---------- やってみよう ----------
-- 1. ↑↓ で温度を変え、波がなくなる温度（臨界温度 T = 1）を確かめよう
-- 2. M で波打つ線を出し、赤と緑の面積を見比べよう。水平線を上げ下げすると面積はどう変わる？
-- 3. マウスで体積を小さくしていくと、どこから液体ができ始める？ 全部液体になるのは？
-- 4. I の理想気体の線と比べて、体積が小さいときに大きくずれるのはなぜ？（分子の大きさ b、引き合う力 a）
-- 5. co2A・co2B を水（a = 0.5536、b = 3.049e-5）に変えて、水の臨界温度を計算しよう（本当は 647 K）

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
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

-- 乱数を n 個まとめて
rndList :: Int -> Rng -> ([Double], Rng)
rndList n r0 = go n r0 []
  where go 0 r acc = (reverse acc, r)
        go k r acc = let (v, r') = rnd r in go (k - 1) r' (v : acc)

-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

-- 指数表記（toExponential）
expo :: Int -> Double -> String
expo n v = let s = showEFloat (Just n) (fin 0 v) ""
               (a, b) = break (== 'e') s
           in case b of
                ('e' : '-' : _) -> s
                ('e' : rest) -> a ++ "e+" ++ rest
                _ -> s

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

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
