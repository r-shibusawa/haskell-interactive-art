-- ============================================
-- 02 連分数 ─ φn = [n; n, n, n, …]
-- 数 x を「整数部分 + 1/(残り)」とくり返し分解する。貴金属比はずっと同じ数 n が並ぶ
-- 途中で打ち切った分数（近似分数 p/q）がどれだけ速く x に近づくか、誤差を対数グラフで比べる
-- ←→ = n / 1〜4 = 主役の数（1 φn・2 √2・3 π・4 e）/ スペース = 一時停止 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Numeric (showEFloat, showFFloat)

-- 比べる数
data Num' = Num' { nName :: String, nV :: !Double, nHue :: !Float }

data World = World
  { wN      :: !Int    -- ★ 貴金属比の n
  , wWho    :: !Int    -- 主役（1〜4）
  , wShownK :: !Int    -- アニメーションで見えている項の数
  , wPaused :: !Bool
  , wTimer  :: !Int
  }

terms :: Int
terms = 14             -- ★ 何項まで計算するか

initialWorld :: World
initialWorld = World 1 1 0 False 0

main :: IO ()
main = runSketch (sketch "33-02 Continued Fraction" initialWorld)
  { skBack  = hsb 220 30 10
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

stepWorld :: World -> World
stepWorld w
  | wPaused w = w
  | wTimer w + 1 > 45 = w { wTimer = 0, wShownK = if wShownK w >= terms then 1 else wShownK w + 1 }
  | otherwise = w { wTimer = wTimer w + 1 }

numbers :: Int -> [Num']
numbers n =
  [ Num' ("phi" ++ show n) (metallic (fromIntegral n)) 45
  , Num' "sqrt2" (sqrt 2) 200
  , Num' "pi" pi 330
  , Num' "e" (exp 1) 120
  ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ txt env 20 26 15 white ("Main: " ++ nName m ++ " = " ++ nfD 10 (nV m) ++ " (1-4)   n = " ++ show (wN w) ++ " (L/R)   Space pause   R restart")
  , txt env 20 52 14 (gray 0.75) "x = a0 + 1/(a1 + 1/(a2 + ...)). For phi_n every a is n. Pi has a big 292: a sudden jump closer."
  , txt env 20 102 24 (hsb (nHue m) 60 100) cfString
  , drawNested env cf 40 170 (nHue m) shownK
  , drawTable env m cv shownK
  , drawNumberLine env m cv shownK (envW env * 0.5) 130 (envW env * 0.46) 150
  , drawErrorPlot env nums (wWho w) shownK (envW env * 0.5) 330 (envW env * 0.46) (envH env - 400)
  ]
  where
    nums = numbers (wN w)
    m = case drop (wWho w - 1) nums of
          (x : _) -> x
          [] -> Num' "?" 1 0
    cf = contFrac (nV m) terms
    cv = convergents cf
    shownK = wShownK w
    cfString =
      let a0 = case cf of { (a : _) -> show a; [] -> "0" }
          rest = [ " " ++ show a ++ (if i < shownK - 1 then "," else "")
                 | (i, a) <- zip [1 ..] (drop 1 cf), i < shownK ]
      in "[" ++ a0 ++ ";" ++ concat rest ++ " ... ]"

-- 近似分数の表
drawTable :: Env -> Num' -> [(Integer, Integer)] -> Int -> Picture
drawTable env m cv shownK = Pictures (header : rows)
  where
    tx = 30
    ty = 340
    cols = [tx, tx + 40, tx + 250, tx + 410]
    header = Pictures (zipWith (\x s -> txt env x ty 14 white s) cols ["k", "p / q", "value", "error |x-p/q|"])
    rows =
      [ let y = ty + 24 + fromIntegral k * 22
            v = fromIntegral p / fromIntegral q :: Double
            err = abs (nV m - v)
            col = if k == shownK - 1 then hsb 55 80 100 else gray 0.75
        in Pictures
             [ txt env tx y 14 col (show k)
             , txt env (tx + 40) y 14 col (show p ++ " / " ++ show q)
             , txt env (tx + 250) y 14 col (nfD 8 v)
             , txt env (tx + 410) y 14 col (if err < 1e-15 then "< 1e-15" else expD 2 err)
             ]
      | (k, (p, q)) <- zip [0 :: Int ..] (take (min shownK 12) cv) ]

-- a0 + 1/(a1 + 1/(a2 + …)) を階段状に描く
drawNested :: Env -> [Integer] -> Float -> Float -> Float -> Int -> Picture
drawNested env cf x y hue shownK = Pictures (zipWith one [0 .. 3] cf)
  where
    one :: Int -> Integer -> Picture
    one i a =
      let on = i < shownK
          fi = fromIntegral i
          xx = x + fi * 70
          yy = y + fi * 34
          mainText = txt env xx yy 20 (if on then hsb hue 60 100 else hsb 0 0 40)
                       (show a ++ (if i < 3 then " +" else " + ..."))
          frac = [ Color (hsb 0 0 (if on then 80 else 30)) (Line [pt env (xx + 60) (yy + 8), pt env (xx + 260 - fi * 30) (yy + 8)])
                 , txt env (xx + 90) (yy - 2) 20 (hsb 0 0 (if on then 90 else 40)) "1" ]
      in Pictures (mainText : if i < 3 then frac else [])

-- 数直線のズーム：近似分数が左右交互に近づく
drawNumberLine :: Env -> Num' -> [(Integer, Integer)] -> Int -> Float -> Float -> Float -> Float -> Picture
drawNumberLine env m cv shownK x y w h = Pictures
  ( [ Color (withA 0.15 white) (boxLine env x y w h)
    , Color (withA 0.6 white) (Line [pt env (x + 10) (y + h / 2), pt env (x + w - 10) (y + h / 2)])
    , Color (hsb (nHue m) 70 100) (thickLine 3 (pt env (toX (nV m)) (y + 20)) (pt env (toX (nV m)) (y + h - 20)))
    ]
    ++ concat
      [ let px = toX (valueOf c)
            on = i == k
        in if px < x || px > x + w then [] else
             Color (if on then hsb 55 90 100 else hsba 0 0 70 0.6) (at (pt env px (y + h / 2)) (disc (if on then 7 else 4)))
             : [ txt env (min (px - 20) (x + w - 170)) (y + h / 2 - 16) 14 (hsb 55 90 100) (show p ++ "/" ++ show q) | on, let (p, q) = c ]
      | (i, c) <- zip [0 ..] (take (k + 1) cv) ]
    ++ [ txt env (x + 10) (y + h - 8) 14 (gray 0.7) ("Number line zoom (width +-" ++ expD 1 span' ++ ")") ])
  where
    k = max 0 (min shownK (length cv) - 1)
    latest = case drop k cv of
               (c : _) -> valueOf c
               [] -> nV m
    span' = max (abs (nV m - latest) * 4) 1e-12
    toX :: Double -> Float
    toX v = x + w / 2 + realToFrac ((v - nV m) / span') * (w * 0.45)

drawErrorPlot :: Env -> [Num'] -> Int -> Int -> Float -> Float -> Float -> Float -> Picture
drawErrorPlot env nums who shownK x y w h = Pictures
  ( [ Color (withA 0.15 white) (boxLine env x y w h) ]
    ++ concat [ [ Color (withA 0.1 white) (Line [pt env x (yOf (10 ** fromIntegral d)), pt env (x + w) (yOf (10 ** fromIntegral d))])
                , txt env (x + 4) (yOf (10 ** fromIntegral d) - 3) 14 (gray 0.6) ("1e" ++ show d) ]
              | d <- [0, -4, -8, -12, -16 :: Int] ]
    ++ zipWith plotOne [0 ..] nums
    ++ [ txt env (x + 50) (y + h - 4) 14 (gray 0.7) "Error, log scale (x = term k). phi1 falls slowest" ])
  where
    yOf :: Double -> Float
    yOf e = y + h - 20 - remap (realToFrac (clampD (-16) 0 (log10 (max e 1e-16)))) (-16) 0 0 (h - 40)
    plotOne :: Int -> Num' -> Picture
    plotOne j nm =
      let cv = convergents (contFrac (nV nm) terms)
          main' = j == who - 1
          pts = [ pt env (x + 50 + fromIntegral k * (w - 70) / fromIntegral terms) (yOf (abs (nV nm - valueOf c)))
                | (k, c) <- zip [0 :: Int ..] (take shownK cv) ]
      in Pictures
           [ Color (hsba (nHue nm) 70 100 (if main' then 1 else 0.45)) (thickPath (if main' then 3 else 1.5) pts)
           , txt env (x + w - 70) (y + 24 + fromIntegral j * 20) 14 (hsb (nHue nm) 70 100) (nName nm)
           ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> w { wWho = fromEnum c - fromEnum '0', wShownK = 0 }
    | c == 'r' || c == 'R' -> w { wShownK = 0, wTimer = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN = min (wN w + 1) 9, wShownK = 0 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wN = max (wN w - 1) 1, wShownK = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. π の近似分数 22/7 と 355/113 を表で見つけよう。355/113 のあとの項 292 が大きいのが理由
-- 2. n を大きくすると誤差の下がり方はどうなる？ 1 項ごとにおよそ 1/φn² 倍ずつ
-- 3. numbers に √3 や好きな数を足して連分数を見てみよう（√3 = [1; 1, 2, 1, 2, …]）
-- 4. terms を 30 にすると途中から数字が崩れる。コンピュータの小数（倍精度）の限界を体験しよう

-- ---------- 道具 ----------
-- 誤差 1e-15 まで見るので、このスケッチは倍精度の Double で計算する
metallic :: Double -> Double
metallic k = (k + sqrt (k * k + 4)) / 2

-- 連分数の係数 [a0, a1, ...]
contFrac :: Double -> Int -> [Integer]
contFrac _ 0 = []
contFrac x m =
  let f = floor x :: Integer
      rest = x - fromIntegral f
  in if rest < 1e-12 then [f] else f : contFrac (1 / rest) (m - 1)

-- 近似分数 p_k / q_k（漸化式 p_k = a_k p_{k-1} + p_{k-2}）
convergents :: [Integer] -> [(Integer, Integer)]
convergents as = go as 1 0 0 1
  where
    go [] _ _ _ _ = []
    go (a : r) p1 q1 p0 q0 =
      let p2 = a * p1 + p0
          q2 = a * q1 + q0
      in (p2, q2) : go r p2 q2 p1 q1

valueOf :: (Integer, Integer) -> Double
valueOf (p, q) = fromIntegral p / fromIntegral q

log10 :: Double -> Double
log10 v = log v / log 10

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

expD :: Int -> Double -> String
expD d v = showEFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

boxLine :: Env -> Float -> Float -> Float -> Float -> Picture
boxLine env x y w h =
  Line [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h), pt env x y]
