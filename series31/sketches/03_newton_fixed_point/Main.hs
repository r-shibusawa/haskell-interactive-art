-- ============================================
-- 03 ニュートン法は不動点反復
-- f(x) = 0 を解くニュートン法 x ← x − f(x)/f'(x) は、写像 g(x) = x − f(x)/f'(x) の不動点反復（01）そのもの
--   根 r で g'(r) = 0 → 01 の「傾き 0 は一番速い」。正しい桁数が毎回ほぼ 2 倍になる（2 次収束）
-- クリック = 出発点 x₀ / スペース = 1 回進める / A = 自動 / 1〜4 = 関数 / F = 複素平面のニュートン・フラクタル（z³ − 1）
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showEFloat, showFFloat)

type RGB = (Word8, Word8, Word8)

-- 桁数を見るので計算は Double で行う
data Fn = Fn
  { fnName :: String
  , fnF, fnD :: Double -> Double
  , fnRoots :: [Double]
  }

fnOf :: Int -> Fn
fnOf 0 = Fn "x^2 - 2" (\x -> x * x - 2) (\x -> 2 * x) [sqrt 2, negate (sqrt 2)]
fnOf 1 = Fn "x^3 - 2x + 2" (\x -> x * x * x - 2 * x + 2) (\x -> 3 * x * x - 2) [-1.7692923542386314]
fnOf 2 = Fn "cos x - x" (\x -> cos x - x) (\x -> negate (sin x) - 1) [0.7390851332151607]
fnOf _ = Fn "x^3 - x" (\x -> x * x * x - x) (\x -> 3 * x * x - 1) [-1, 0, 1]

data World = World
  { wFi      :: !Int
  , wX0      :: !Double
  , wXs      :: ![Double]
  , wShown   :: !Int
  , wAuto    :: !Bool
  , wBasins  :: !(U.Vector RGB)         -- 実数直線の「どの根へ行くか」の帯
  , wFractal :: !Bool
  , wFracRow :: !Int
  , wOrbit   :: ![(Double, Double)]
  }

lo, hi :: Double
lo = -3                    -- ★ 横軸の範囲
hi = 3

fw, fh :: Int
fw = 240                   -- ★ フラクタルの解像度（小さいほど速い）
fh = 150

main :: IO ()
main = runSketch (sketch "31-03 Newton as Fixed Point" (restart (World 0 2.6 [] 1 True U.empty False 0 [])))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ ニュートン写像
newton :: Fn -> Double -> Double
newton fn x = let d = fnD fn x in if abs d < 1e-12 then x else x - fnF fn x / d

restart :: World -> World
restart w =
  let fn = fnOf (wFi w)
      lim = max (-1e6) . min 1e6
  in w { wXs = take 31 (iterate (lim . newton fn) (wX0 w)), wShown = 1, wBasins = buildBasins fn }

nearestRoot :: Fn -> Double -> Int
nearestRoot fn x = fst (foldl' pick (-1, 1e-6) (zip [0 ..] (fnRoots fn)))
  where pick (b, bd) (i, r) = if abs (x - r) < bd then (i, abs (x - r)) else (b, bd)

rootHue :: Int -> Float
rootHue 0 = 190
rootHue 1 = 45
rootHue _ = 320

buildBasins :: Fn -> U.Vector RGB
buildBasins fn = U.generate 400 $ \i ->
  let start = lo + (hi - lo) * fromIntegral i / 399
      go :: Int -> Double -> (Int, Double)
      go it x
        | it >= 60 = (it, x)
        | otherwise =
            let x' = newton fn x
            in if nearestRoot fn x' >= 0 then (it, x') else go (it + 1) x'
      (its, xe) = go 0 start
      r = nearestRoot fn xe
  in if r < 0 then (0, 0, 0) else hsbToRgb (rootHue r) 70 (100 - fromIntegral its * 2.5)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wFractal w = pure w { wFracRow = min fh (wFracRow w + 12) }   -- 少しずつ計算する
  | wAuto w && envFrame env `mod` 40 == 0 && wShown w < 12 = pure w { wShown = wShown w + 1 }
  | otherwise = pure w

drawWorld :: Env -> World -> Picture
drawWorld env w
  | wFractal w = drawFractal env w
  | otherwise = screen env $
      [ box (hsb 0 0 35) ax ay pw ph ]
      ++ clipped ax (hsb 0 0 45) 1 [(ax, yF 0), (ax + pw, yF 0)]
      ++ clipped ax (hsb 190 70 100) 2.5 [ (xOf x ax, yF (fnF fn x)) | s <- [0 .. 300 :: Int], let x = lerpD lo hi (fromIntegral s / 300) ]
      ++ concat [ clipped ax (hsba 320 70 100 0.5) 1 [(xOf x ax, yF 0), (xOf x ax, yF (fnF fn x))]
                  ++ clipped ax (hsb 320 70 100) 1.5 [(xOf x ax, yF (fnF fn x)), (xOf x' ax, yF 0)]   -- 接線を x 軸まで
                | (x, x') <- steps ]
      ++ [ spot (hsb 320 70 100) (xOf cur ax, yF 0) 9 | inBox (xOf cur ax) (yF 0) ax ]
      ++ [ txt ax (ay - 10) 14 (hsb 0 0 70) ("y = " ++ fnName fn ++ "   The tangent hits the x axis at the next x") ]
      -- 右：g(x) = x − f/f' と y = x、クモの巣
      ++ [ box (hsb 0 0 35) bxp ay pw ph
         , ln 1.5 (hsba 0 0 70 0.6) (bxp, ay + ph) (bxp + pw, ay) ]
      ++ gCurve
      ++ concat [ clipped bxp (hsb 320 70 100) 1.5 [(xOf x bxp, yG x), (xOf x bxp, yG x')]
                  ++ clipped bxp (hsb 320 70 100) 1.5 [(xOf x bxp, yG x'), (xOf x' bxp, yG x')]
                | (x, x') <- steps ]
      ++ [ spot (hsb 45 90 100) (xOf r bxp, yG r) 12 | r <- fnRoots fn ]
      ++ [ txt bxp (ay - 10) 14 (hsb 0 0 70) "g(x) = x - f/f'. Crossing y = x: root = fixed point (g' = 0)" ]
      -- 帯：実数直線の各点がどの根へ行くか
      ++ [ Translate (ax + pw / 2) (ay + ph + 33) (Scale (pw / 400) 13
             (pixelImage 400 2 1 (\x _ -> wBasins w U.! x)))
         , ln 2 white (xOf (wX0 w) ax, ay + ph + 14) (xOf (wX0 w) ax, ay + ph + 52)
         , txt ax (ay + ph + 72) 14 (hsb 0 0 60) "Color = which root each start reaches (black = none)."
         , txt ax (ay + ph + 92) 14 (hsb 0 0 60) "Wide bands near a root; colors tangle near f' = 0."
         ]
      ++ table
      ++ [ txt 20 34 15 white (show (wFi w + 1) ++ ": f(x) = " ++ fnName fn ++ "   click: x0 = " ++ nf 3 (wX0 w) ++ "   Space: one step   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   1-4: function   F: fractal")
         , txt 20 58 14 (hsb 0 0 60) "Uses \"slope near 0 is fastest\" from sketch 01 to the limit. Quadratic: error e -> const * e^2 (digits double)."
         , txt 20 78 14 (hsb 0 0 60) "With function 2 and x0 = 0, the sequence gets stuck in the cycle 0 <-> 1."
         ]
  where
    fn = fnOf (wFi w)
    pw = (envW env - 120) / 2
    ph = envH env * 0.45
    ax = 40
    ay = 110
    bxp = 80 + pw
    xOf :: Double -> Float -> Float
    xOf x x0p = x0p + realToFrac ((x - lo) / (hi - lo)) * pw
    yF :: Double -> Float
    yF y = clampF (ay - 100) (ay + ph + 100) (ay + ph - realToFrac ((y + 4) / 8) * ph)
    yG :: Double -> Float
    yG y = clampF (ay - 100) (ay + ph + 100) (ay + ph - realToFrac ((y - lo) / (hi - lo)) * ph)
    inBox x y x0 = x >= x0 && x <= x0 + pw && y >= ay && y <= ay + ph
    -- 枠からはみ出さないように切ってから描く（p5 の clip の代わり）
    clipped x0 c t ps = [ Color c (thickLine t a' b') | (a, b) <- zip ps (drop 1 ps), Just (a', b') <- [clipSeg x0 ay (x0 + pw) (ay + ph) a b] ]
    shownXs = take (wShown w) (wXs w)
    steps = zip shownXs (drop 1 (take (wShown w) (wXs w)))
    cur = foldl (\_ x -> x) (wX0 w) shownXs
    gCurve =
      let pts = [ (xOf x bxp, yG y, abs y < 50) | s <- [0 .. 400 :: Int], let x = lerpD lo hi (fromIntegral s / 400), let y = newton fn x ]
      in concat [ clipped bxp (hsb 45 90 100) 2.5 [(x1, y1), (x2, y2)]
                | ((x1, y1, ok1), (x2, y2, ok2)) <- zip pts (drop 1 pts), ok1, ok2, abs (y2 - y1) < ph * 0.5 ]
    -- 表：桁数が倍々になる
    tx = bxp
    ty = ay + ph + 44
    table =
      [ txt tx ty 14 white "n", txt (tx + 34) ty 14 white "x_n", txt (tx + 205) ty 14 white "|x_n - r|", txt (tx + 330) ty 14 white "correct digits" ]
      ++ concat [ row i x | (i, x) <- zip [0 :: Int ..] (take (min (wShown w) 8) (wXs w)) ]
    row i x =
      let r = nearest x (fnRoots fn)
          e = abs (x - r)
          digits = if e > 0 then max 0 (negate (logBase 10 e)) else 16
          y = ty + 24 + fromIntegral i * 22
      in [ txt tx y 14 (hsb 0 0 85) (show i)
         , txt (tx + 34) y 14 (hsb 0 0 85) (nf 12 x)
         , txt (tx + 205) y 14 (hsb 0 0 85) (if e > 0 then showEFloat (Just 2) e "" else "0")
         , fillBox (hsb 140 70 100) (tx + 330) (y - 12) (realToFrac digits * 10) 12 ]

nearest :: Double -> [Double] -> Double
nearest _ [] = 0
nearest x (r : rs) = foldl' (\a b -> if abs (x - b) < abs (x - a) then b else a) r rs

-- 複素平面の z³ − 1：3 つの根のどれへ行くかで色分け（行ごとに必要になった時に計算される）
fracRows :: V.Vector (U.Vector RGB)
fracRows = V.generate fh $ \row ->
  U.generate fw $ \i ->
    let zr0 = -2.4 + 4.8 * fromIntegral i / fromIntegral fw
        zi0 = -1.5 + 3 * fromIntegral row / fromIntegral fh
        go :: Int -> Double -> Double -> (Int, Int)
        go it zr zi
          | it >= 40 = (it, -1)
          | otherwise =
              let (zr', zi') = newtonZ zr zi
                  r = whichRoot zr' zi'
              in if r >= 0 then (it, r) else go (it + 1) zr' zi'
        (its, root) = go 0 zr0 zi0
    in if root < 0 then (0, 0, 0) else hsbToRgb (rootHue root) 70 (100 - fromIntegral its * 4)

newtonZ :: Double -> Double -> (Double, Double)
newtonZ zr zi =
  -- z ← z − (z³ − 1)/(3z²)
  let z2r = zr * zr - zi * zi
      z2i = 2 * zr * zi
      z3r = z2r * zr - z2i * zi
      z3i = z2r * zi + z2i * zr
      nr = z3r - 1
      ni = z3i
      dr = 3 * z2r
      di = 3 * z2i
      den = dr * dr + di * di
  in if den < 1e-12 then (zr, zi) else (zr - (nr * dr + ni * di) / den, zi - (ni * dr - nr * di) / den)

whichRoot :: Double -> Double -> Int
whichRoot zr zi
  | near 1 0 = 0
  | near (-0.5) 0.8660254 = 1
  | near (-0.5) (-0.8660254) = 2
  | otherwise = -1
  where near a b = (zr - a) * (zr - a) + (zi - b) * (zi - b) < 1e-6

drawFractal :: Env -> World -> Picture
drawFractal env w = screen env $
  [ Translate (ww / 2) (hh / 2) (Scale (ww / fromIntegral fw) (negate (hh / fromIntegral fh))
      (pixelImage fw fh 1 (\x y -> if y < row then (fracRows V.! y) U.! x else (16, 18, 30))))
  ]
  ++ [ Color white (thickPath 2 (map toS (wOrbit w))) | length (wOrbit w) > 1 ]
  ++ [ fillBox (makeColor 0 0 0 0.6) 0 0 ww 84
     , txt 20 36 16 white "z^3 - 1 = 0 by Newton's method (complex)    click: orbit from that point    F: back"
     , txt 20 64 14 (hsb 0 0 75) "3 colors = basins of the 3 roots (1, -1/2 +- (sqrt 3 / 2) i). On the boundary all 3 colors tangle endlessly."
     ]
  where
    row = wFracRow w
    ww = envW env
    hh = envH env
    toS (a, b) = (remap (realToFrac a) (-2.4) 2.4 0 ww, remap (realToFrac b) (-1.5) 1.5 0 hh)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wFractal w ->
        let z = (realToFrac (remap mx 0 (envW env) (-2.4) 2.4), realToFrac (remap my 0 (envH env) (-1.5) 1.5))
        in pure w { wOrbit = take 31 (iterate (uncurry newtonZ) z) }
    | my < 100 -> pure w
    | otherwise ->
        let pw = (envW env - 120) / 2
            rx = if mx < 40 + pw then (mx - 40) / pw else (mx - 80 - pw) / pw
        in pure (restart w { wX0 = lerpD lo hi (realToFrac (clampF 0 1 rx)) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wShown = min (length (wXs w)) (wShown w + 1), wAuto = False }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (restart w { wFi = fromEnum c - fromEnum '1' })
    | toLower c == 'a' -> pure w { wAuto = not (wAuto w) }
    | toLower c == 'f' -> pure w { wFractal = not (wFractal w), wFracRow = 0, wOrbit = [] }
  _ -> pure w
  where (mx, my) = mouseP env

-- ---------- やってみよう ----------
-- 1. 関数 1 で表の「正しい桁」の棒が 1 → 2 → 4 → 8 → 16 と倍々になるのを確かめよう（01 の関数 3 は 1 桁ずつ）
-- 2. 関数 4（x³ − x）の帯で、x₀ = 0.45 付近の色の乱れを拡大してみよう（lo, hi を 0.4, 0.5 に）
-- 3. 関数 2 で x₀ = 0 から始めると 0 → 1 → 0 → … 。g の不動点でない「周期点」につかまる
-- 4. newtonZ を z³ − 1 から z⁴ − 1 に変えると色は 4 つ。whichRoot の根も書きかえよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

lerpD :: Double -> Double -> Double -> Double
lerpD a b t = a + (b - a) * t

-- 線分を長方形の中だけに切る（グラフの枠の外に線がはみ出さないようにする。リャン＝バースキー法）
clipSeg :: Float -> Float -> Float -> Float -> Point -> Point -> Maybe (Point, Point)
clipSeg x0 y0 x1 y1 (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) [(negate dx, ax - x0), (dx, x1 - ax), (negate dy, ay - y0), (dy, y1 - ay)]
  in if a > b then Nothing else Just ((ax + a * dx, ay + a * dy), (ax + b * dx, ay + b * dy))

-- HSB（0〜360, 0〜100, 0〜100）→ RGB。画素に直接色を書くときに使う
hsbToRgb :: Float -> Float -> Float -> RGB
hsbToRgb h s b =
  let (r, g, bl, _) = rgbaOfColor (hsb h s (max 0 b))
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
  in (q r, q g, q bl)
