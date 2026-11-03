-- ============================================
-- 01 反復とクモの巣図
-- x₁ = f(x₀), x₂ = f(x₁), … とくり返す。グラフ上では「縦に f へ → 横に対角線へ」をくり返す（クモの巣図）
--   不動点 x* で |f'(x*)| < 1 なら近くの点は吸い寄せられ、> 1 なら押し返される
-- クリック = 出発点 x₀ / 1〜4 = 関数 / ↑↓ = パラメータ / ←→ = 速さ / スペース = 止める・動かす / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

-- 関数の表（数値微分で 1e-5 を使うので、計算は Double で行う）
data Fn = Fn
  { fnName :: String
  , fnLo, fnHi, fnAMin, fnAMax :: !Double
  , fnF :: Double -> Double -> Double
  }

fnOf :: Int -> Fn
fnOf 0 = Fn "a*cos x" 0 1.6 0.2 1.6 (\x a -> a * cos x)
fnOf 1 = Fn "logistic r*x(1-x)" 0 1 0.5 4 (\x a -> a * x * (1 - x))
fnOf 2 = Fn "x - a(x^2 - 2) (finds sqrt 2)" 0 2.4 0.05 0.95 (\x a -> x - a * (x * x - 2))
fnOf _ = Fn "tanh(a*x)" (-2) 2 0.3 3 (\x a -> tanh (a * x))

data World = World
  { wFi      :: !Int                 -- 今の関数の番号
  , wAs      :: !(U.Vector Double)   -- それぞれの関数のパラメータ
  , wX0      :: !Double              -- 出発点
  , wPath    :: ![Double]            -- 反復の値 x0, x1, x2, …
  , wEvery   :: !Int                 -- ★ 何フレームごとに 1 回進めるか（小さいほど速い）
  , wRunning :: !Bool
  , wFixed   :: ![(Double, Double)]  -- 不動点と、そこでの傾き f'(x*)
  }

maxSteps :: Int
maxSteps = 120                       -- ★ 何回くり返すか

main :: IO ()
main = runSketch (sketch "31-01 Iteration and Cobweb" (restart world0))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World 1 (U.fromList [1, 2.8, 0.25, 1.6]) 0.12 [] 12 True []

fv :: World -> Double -> Double
fv w x = fnF (fnOf (wFi w)) x (wAs w U.! wFi w)

restart :: World -> World
restart w = w { wPath = [wX0 w], wFixed = findFixed w }

-- 不動点と、そこでの傾き f'(x*)
findFixed :: World -> [(Double, Double)]
findFixed w = go 1 (gf lo)
  where
    fn = fnOf (wFi w)
    lo = fnLo fn
    hi = fnHi fn
    n = 800 :: Int
    f = fv w
    gf x = f x - x
    go s pg
      | s > n = []
      | otherwise =
          let x = lo + (hi - lo) * fromIntegral s / fromIntegral n
              g = gf x
              rest = go (s + 1) g
          in if pg * g <= 0 && not (pg == 0 && s > 1)
               then let xs = bisect (50 :: Int) (x - (hi - lo) / fromIntegral n) x
                        h = 1e-5
                    in (xs, (f (xs + h) - f (xs - h)) / (2 * h)) : rest
               else rest
    bisect 0 a b = (a + b) / 2
    bisect k a b =
      let m = (a + b) / 2
      in if gf a * gf m <= 0 then bisect (k - 1) a m else bisect (k - 1) m b

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRunning w && envFrame env `mod` wEvery w == 0 && length (wPath w) < maxSteps =
      let nx = fv w (lastOr 0 (wPath w))
      in pure (if isNaN nx || isInfinite nx then w else w { wPath = wPath w ++ [max (-1e6) (min 1e6 nx)] })
  | otherwise = pure w

-- レイアウト
gx, gy :: Float
gx = 60
gy = 100

sizeOf :: Env -> Float
sizeOf env = min (envW env * 0.5) (envH env - 170)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ box (hsb 0 0 40) gx gy s s
  , ln 2 (hsba 0 0 70 0.7) (gx, gy + s) (gx + s, gy)
  , Color (hsb 190 70 100) (thickPath 3 [(tp x, tq (fv w x)) | i <- [0 .. 300 :: Int], let x = lo + (hi - lo) * fromIntegral i / 300])
  , Color (hsba 320 70 100 0.8) (thickPath 1.5 cobweb)
  , spot (hsb 320 70 100) (tp p0, tq lo) 10
  , spot white (tp lastX, tq lastX) 9
  ]
  ++ concatMap fixedMark (wFixed w)
  ++ drawSeries env w s lastX
  ++ [ txt 20 34 15 white (show (wFi w + 1) ++ ": " ++ fnName fn ++ "  param = " ++ nf 3 a ++ " (Up/Down)  click: x0  1-4: function  Left/Right: speed  Space: " ++ (if wRunning w then "pause" else "run") ++ "  R: restart")
     , txt 20 58 14 (hsb 0 0 60) "Vertical move to the curve = apply f once. Horizontal move to the diagonal = feed the value back in."
     , txt 20 78 14 (hsb 0 0 60) "Only fixed points with slope f' between -1 and 1 pull the sequence in."
     ]
  where
    fn = fnOf (wFi w)
    a = wAs w U.! wFi w
    lo = fnLo fn
    hi = fnHi fn
    s = sizeOf env
    tp x = clampF (gx - 60) (gx + s + 60) (gx + realToFrac ((x - lo) / (hi - lo)) * s)
    tq y = clampF (gy - 60) (gy + s + 60) (gy + s - realToFrac ((y - lo) / (hi - lo)) * s)
    path = wPath w
    p0 = case path of { (x : _) -> x; [] -> wX0 w }
    lastX = lastOr p0 path
    -- クモの巣：(x, x) → (x, f(x)) → (f(x), f(x)) → …
    cobweb = (tp p0, tq lo) : concat (zipWith (\u v -> [(tp u, tq v), (tp v, tq v)]) path (drop 1 path))
    fixedMark (x, d) =
      let attract = abs d < 1
          c = hsb (if attract then 140 else 0) 80 100
      in [ spot c (tp x, tq x) 14
         , txt (tp x + 12) (tq x + 22) 14 c ("x* = " ++ nf 3 x ++ "  |f'| = " ++ nf 2 (abs d) ++ (if attract then "  attracts" else "  repels"))
         ]

-- 右側：x_n を回数ごとに並べたグラフ
drawSeries :: Env -> World -> Float -> Double -> [Picture]
drawSeries env w s lastX
  | pw < 160 = []
  | otherwise =
      [ box (hsb 0 0 35) x0p y0p pw h ]
      ++ [ Color (hsba (if abs d < 1 then 140 else 0) 70 90 0.5) (Line [(x0p, yOf x), (x0p + pw, yOf x)]) | (x, d) <- wFixed w ]
      ++ [ Color (hsb 320 70 100) (thickPath 2 [ (x0p + fromIntegral i / 59 * pw, clampF (y0p - 10) (y0p + h + 10) (yOf v)) | (i, v) <- zip [0 :: Int ..] recent ])
         , txt x0p (y0p - 10) 14 (hsb 0 0 70) "x_n (horizontal = step n, last 60 steps)"
         , txt x0p ty 15 white ("n = " ++ show (len - 1) ++ "   x_n = " ++ nf 6 lastX)
         ]
      ++ [ txt x0p (ty + 30 + fromIntegral i * 24) 14 (hsb 0 0 60) l | (i, l) <- zip [0 :: Int ..] notes ]
  where
    fn = fnOf (wFi w)
    lo = fnLo fn
    hi = fnHi fn
    x0p = gx + s + 60
    pw = envW env - x0p - 40
    y0p = gy + 20
    h = s * 0.55
    ty = y0p + h + 34
    len = length (wPath w)
    recent = drop (max 0 (len - 60)) (wPath w)
    yOf v = remap (realToFrac v) (realToFrac lo) (realToFrac hi) (y0p + h) y0p
    notes =
      [ "- 0 < f' < 1: approaches from one side (stairs)"
      , "- -1 < f' < 0: zigzags inward (spiral)"
      , "- |f'| > 1: moves away; to another fixed point,"
      , "  a cycle, or chaos"
      , "- Logistic: what happens for r > 3? r = 3.5, 3.9?"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        s = sizeOf env
        fn = fnOf (wFi w)
        x0 = realToFrac (remap mx gx (gx + s) 0 1) * (fnHi fn - fnLo fn) + fnLo fn
    in pure (if mx > gx && mx < gx + s && my > gy - 20 && my < gy + s + 20
               then restart w { wX0 = x0 } else w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' ->
        let i = fromEnum c - fromEnum '1'
            fn = fnOf i
        in pure (restart w { wFi = i, wX0 = fnLo fn + (fnHi fn - fnLo fn) * 0.12 })
    | toLower c == 'r' -> pure (restart w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (restart (nudge 0.05))
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (restart (nudge (-0.05)))
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wEvery = min 60 (wEvery w + 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wEvery = max 1 (wEvery w - 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  _ -> pure w
  where
    nudge d =
      let fn = fnOf (wFi w)
          a = max (fnAMin fn) (min (fnAMax fn) (wAs w U.! wFi w + d))
      in w { wAs = wAs w U.// [(wFi w, a)] }

-- ---------- やってみよう ----------
-- 1. 関数 1（cos）で a = 1 のとき、どこから出発しても同じ値（0.739…）に着く。電卓で cos を押し続けても同じ
-- 2. 関数 3 で a を 0.35 にすると一番速く √2 に着く。そのとき f'(√2) はいくつ？（0 に近いほど速い → 03 ニュートン法）
-- 3. 関数 2 で r = 3.2 にすると 2 つの値を行き来する（周期 2）。不動点はあるのに着かないのはなぜ？
-- 4. fnOf に自分の関数（例 \x _ -> sin (3 * x)）を足して、不動点の数と傾きを調べよう

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

box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

lastOr :: a -> [a] -> a
lastOr d xs = foldl (\_ x -> x) d xs
