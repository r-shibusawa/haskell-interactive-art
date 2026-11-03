-- ============================================
-- 12 微分方程式と不動点（ピカールの逐次近似）
-- y' = f(t, y)、y(0) = y₀ の解は、「関数を関数に写す写像」Φ(y)(t) = y₀ + ∫₀ᵗ f(s, y(s)) ds の不動点
--   定数 y₀ から始めて Φ を何度もかけると、曲線が 1 本ずつ本当の解（白い点線）に近づく。これが解の存在と一意性の証明（ピカール＝リンデレーフ）
-- スペース = Φ をもう 1 回 / A = 自動 / 1〜4 = 方程式 / ↑↓ = 区間の長さ T / グラフをクリック = 初期値 y₀ / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

-- 誤差 1e-12 まで見るので計算は Double で行う
data Eqn = Eqn
  { eName :: String
  , eF :: Double -> Double -> Double
  , eY0, eT, eLo, eHi :: !Double
  , eNote :: [String]
  }

eqnOf :: Int -> Eqn
eqnOf 0 = Eqn "y' = y" (\_ y -> y) 1 2 (-0.5) 8
  ["y_k is the Taylor series of e^t:", "1 + t + t^2/2! + ... + t^k/k!"]
eqnOf 1 = Eqn "y' = t - y^2" (\t y -> t - y * y) 0 3 (-1) 3
  ["Even an equation with no formula for its", "solution is approached by repeated integrals"]
eqnOf 2 = Eqn "y' = y^2" (\_ y -> y * y) 1 0.9 (-0.5) 12
  ["True solution 1/(1 - t) blows up at t = 1.", "With T > 1 the approximation does not converge"]
eqnOf _ = Eqn "y' = -2t*y" (\t y -> -2 * t * y) 1 2.5 (-1.5) 1.5
  ["Solution e^(-t^2). With long T it takes many", "steps (big swings before it settles)"]

m :: Int
m = 400                    -- ★ 区間を何点で表すか

data World = World
  { wEi    :: !Int
  , wIters :: ![U.Vector Double]  -- y_0, y_1, …（それぞれ m+1 個の値、古い順）
  , wExact :: !(U.Vector Double)
  , wAuto  :: !Bool
  , wY0    :: !Double
  , wT     :: !Double
  }

main :: IO ()
main = runSketch (sketch "31-12 Picard Iteration" (pick 0 (World 0 [] U.empty True 1 2)))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

pick :: Int -> World -> World
pick i w = let e = eqnOf i in restart w { wEi = i, wY0 = eY0 e, wT = eT e }

limit :: Double -> Double
limit = max (-1e6) . min 1e6

restart :: World -> World
restart w =
  let f = eF (eqnOf (wEi w))
      y0 = wY0 w
      h = wT w / fromIntegral m / 10
      -- 本当の解（ルンゲ＝クッタ法で細かく）
      rkStep y s =
        let t = fromIntegral s * h
            k1 = f t y
            k2 = f (t + h / 2) (y + h / 2 * k1)
            k3 = f (t + h / 2) (y + h / 2 * k2)
            k4 = f (t + h) (y + h * k3)
        in limit (y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4))
      big i y = foldl rkStep y [i * 10 .. i * 10 + 9 :: Int]
      exact = U.scanl' (\y i -> big i y) y0 (U.enumFromN 0 m)
  in w { wIters = [U.replicate (m + 1) y0], wExact = exact }

-- ★ ピカール写像 Φ：y₀ + （f(s, y(s)) の積分を台形でたし合わせる）
picard :: World -> U.Vector Double -> U.Vector Double
picard w yk =
  let f = eF (eqnOf (wEi w))
      y0 = wY0 w
      h = wT w / fromIntegral m
      trap acc i =
        let t0 = fromIntegral (i - 1) * h
            t1 = fromIntegral i * h
        in acc + h * (f t0 (yk U.! (i - 1)) + f t1 (yk U.! i)) / 2
      accs = U.scanl' trap 0 (U.enumFromN 1 m)
  in U.map (\a -> limit (y0 + a)) accs

nextIter :: World -> World
nextIter w
  | length (wIters w) >= 30 = w
  | otherwise = case reverse (wIters w) of
      (l : _) -> w { wIters = wIters w ++ [picard w l] }
      [] -> w

supDiff :: U.Vector Double -> U.Vector Double -> Double
supDiff a b = U.foldl' max 0 (U.zipWith (\x y -> abs (x - y)) a b)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wAuto w && envFrame env `mod` 50 == 0 = pure (nextIter w)
  | otherwise = pure w

-- レイアウト
gx, gy :: Float
gx = 70
gy = 110

gw, gh :: Env -> Float
gw env = envW env * 0.52
gh env = envH env - 200

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ box (hsb 0 0 35) gx gy pw ph
  , Color (hsb 0 0 45) (Line [(gx, yOf 0), (gx + pw, yOf 0)])
  ]
  -- これまでの近似
  ++ [ if k == cnt - 1
         then Color (hsb hue 60 100) (thickPath 3 (curve it))
         else Color (hsba hue 60 100 0.45) (Line (curve it))
     | (k, it) <- zip [0 ..] (wIters w), let hue = kHue k ]
  -- 本当の解
  ++ [ Color white (dashed 5 7 2 (curve (wExact w)))
     , spot white (xOf 0, yOf (wY0 w)) 10
     , txt (gx - 10) (gy + ph + 20) 14 (hsb 0 0 60) "t = 0"
     , txt (gx + pw - 60) (gy + ph + 20) 14 (hsb 0 0 60) ("t = " ++ nf 2 (wT w))
     , txt (gx - 62) (gy + 6) 14 (hsb 0 0 60) ("y=" ++ showNum (eHi e))
     , txt (gx - 62) (gy + ph) 14 (hsb 0 0 60) ("y=" ++ showNum (eLo e))
     ]
  ++ [ Color (hsba 0 80 100 0.6) (Line [(xOf i1, gy), (xOf i1, gy + ph)]) | wEi w == 2, wT w > 1, let i1 = round (fromIntegral m / wT w) :: Int ]
  -- 右：差の大きさ（対数）
  ++ [ box (hsb 0 0 35) rx gy rw rh ]
  ++ concat [ let d = realToFrac (supDiff cur prev) :: Double
                  er = realToFrac (supDiff cur (wExact w)) :: Double
                  x = rx + fromIntegral (k - 1) * bw
              in [ fillBox (hsb (kHue k) 60 100) (x + 1) (ly d) (bw * 0.45) (gy + rh - ly d)
                 , fillBox (hsba 0 0 100 0.5) (x + bw * 0.5) (ly er) (bw * 0.4) (gy + rh - ly er) ]
            | (k, prev, cur) <- zip3 [1 :: Int ..] (wIters w) (drop 1 (wIters w)) ]
  ++ [ txt rx (gy - 28) 14 (hsb 0 0 70) "Color bars: neighbor gap |y_k - y_(k-1)|"
     , txt rx (gy - 8) 14 (hsb 0 0 70) "White bars: gap from true solution |y_k - y| (log)"
     ]
  ++ [ txt (rx - 46) (ly (10 ** v) + 5) 14 (hsb 0 0 70) ("1e" ++ show (round v :: Int)) | v <- [4, 0, -4, -8, -12 :: Double] ]
  ++ [ txt rx ty 15 white ("k = " ++ show (cnt - 1) ++ "  y_k(T) = " ++ nf 6 lastEnd ++ "  true " ++ nf 6 (U.last (wExact w))) ]
  ++ [ txt rx (ty + 32 + fromIntegral i * 24) 14 (hsb 0 0 80) l | (i, l) <- zip [0 :: Int ..] (eNote e ++ notes) ]
  ++ [ txt 20 34 15 white (show (wEi w + 1) ++ ": " ++ eName e ++ ", y(0) = " ++ nf 2 (wY0 w) ++ "   Space: apply Phi   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   1-4: equation   T = " ++ nf 2 (wT w) ++ " (Up/Down)   click: y0   R: restart")
     , txt 20 62 14 (hsb 0 0 60) "y_(k+1)(t) = y0 + integral from 0 to t of f(s, y_k(s)) ds. Each step, the \"correct part\" grows from left to right."
     ]
  where
    e = eqnOf (wEi w)
    pw = gw env
    ph = gh env
    cnt = length (wIters w)
    kHue :: Int -> Float
    kHue k = wrapF 360 (200 + fromIntegral k * 22)
    xOf :: Int -> Float
    xOf i = gx + fromIntegral i / fromIntegral m * pw
    yOf :: Double -> Float
    yOf y = clampF (gy - 5) (gy + ph + 5) (gy + ph - realToFrac ((y - eLo e) / (eHi e - eLo e)) * ph)
    curve v = [ (xOf i, yOf (v U.! i)) | i <- [0, 2 .. m] ]
    rx = gx + pw + 50
    rw = envW env - rx - 30
    rh = ph * 0.5
    bw = rw / 30
    ly :: Double -> Float
    ly v = remap (clampF (-12) 4 (realToFrac (logBase 10 (max v 1e-12)))) 4 (-12) gy (gy + rh)
    ty = gy + rh + 40
    lastEnd = foldl (\_ it -> U.last it) (wY0 w) (wIters w)
    showNum v = if v == fromIntegral (round v :: Int) then show (round v :: Int) else nf 1 v
    notes =
      [ ""
      , "Why does it converge? If f is Lipschitz in y"
      , "(|f(t,y) - f(t,z)| <= L|y - z|), then"
      , "  |Phi^k(y) - Phi^k(z)| <= (LT)^k / k! |y - z|"
      , "k! grows faster than any power"
      , "-> eventually a contraction (02): one solution"
      , ""
      , "If f grows fast, like y' = y^2, no global L"
      , "exists and the solution can blow up"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx > gx && mx < gx + pw && my > gy && my < gy + ph ->
        let e = eqnOf (wEi w)
            y = eLo e + realToFrac ((gy + ph - my) / ph) * (eHi e - eLo e)
            y' = if wEi w == 2 then max (-3) (min 3 y) else y
        in pure (restart w { wY0 = y' })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (nextIter w { wAuto = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (restart w { wT = min 6 (wT w + 0.1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (restart w { wT = max 0.2 (wT w - 0.1) })
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (pick (fromEnum c - fromEnum '1') w)
    | toLower c == 'a' -> pure w { wAuto = not (wAuto w) }
    | toLower c == 'r' -> pure (restart w)
  _ -> pure w
  where
    (mx, my) = mouseP env
    pw = gw env
    ph = gh env

-- ---------- やってみよう ----------
-- 1. 方程式 1 で y_3 を手で計算しよう：y_1 = 1 + t、y_2 = 1 + t + t²/2、y_3 = ？ グラフと比べる
-- 2. 方程式 3 で T を 0.9 → 1.3 にすると、白い点線（本当の解）はどうなる？ 近似の曲線は？
-- 3. 方程式 4 で T = 2.5 と T = 1 で、白い棒が 1e-6 を切るまでの回数を比べよう（(LT)ᵏ/k! の LT が効く）
-- 4. eqnOf に y' = sin(y) + t などを足してみよう。eLo, eHi も合わせて変える

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

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [Point] -> Picture
dashed on off w ps = Pictures (go True on ps)
  where
    go draw left (a : b : rest) =
      let l = dist2 a b
      in if l <= left
           then [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
           else let t = left / l
                    q = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [thickLine w a q | draw] ++ go (not draw) (if draw then off else on) (q : b : rest)
    go _ _ _ = []
