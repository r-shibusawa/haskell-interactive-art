-- ============================================
-- 02 縮小写像の原理（バナッハの不動点定理）
-- どの 2 点の距離も k 倍（k < 1）以下に縮める写像 T は、不動点をちょうど 1 つ持ち、どこから始めても T をくり返すとそこへ着く
--   誤差は毎回 k 倍 → 対数グラフでは直線。始める前に「何回で足りるか」まで分かる（事前評価）
-- ←→ = 縮小率 k / ↑↓ = 回転 θ / N = 曲がった写像（非線形）/ クリック = 出発点 / スペース = 止める・動かす / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

type P = (Double, Double)  -- 誤差 1e-12 まで見るので計算は Double で行う

data World = World
  { wK       :: !Double   -- ★ 縮小率（1 以上にすると？）
  , wTheta   :: !Double   -- ★ 1 回ごとに回す角度
  , wNon     :: !Bool     -- 曲がった写像か
  , wStart   :: !P
  , wSeq     :: ![P]      -- 出発点からの列 x0, x1, …
  , wShown   :: !Int      -- 何個目まで表示したか
  , wOthers  :: ![[P]]    -- ほかの出発点たち（新しい順）
  , wRunning :: !Bool
  , wFixed   :: !P
  }

bx, by, eps :: Double
bx = 0.55                  -- ★ 平行移動
by = 0.3
eps = 1e-6                 -- ★ 目標の精度

main :: IO ()
main = runSketch (sketch "31-02 Banach Contraction" (rebuild (World 0.8 0.6 False (-1.3, 1.1) [] 1 [] True (0, 0))))
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 写像 T：回して k 倍して平行移動。N のときは少し曲げる（曲げた分を割って縮小率は k 以下のまま）
mapT :: World -> P -> P
mapT w (px, py) =
  let c = 0.35
      wv = 1.6
      (x, y) | wNon w = ((px + c * sin (wv * py)) / (1 + c * wv), (py + c * sin (wv * px)) / (1 + c * wv))
             | otherwise = (px, py)
      cs = cos (wTheta w)
      sn = sin (wTheta w)
      k = wK w
      lim = max (-1e3) . min 1e3
  in (lim (k * (cs * x - sn * y) + bx), lim (k * (sn * x + cs * y) + by))

rebuild :: World -> World
rebuild w =
  let t = mapT w
      fixedP = iterate' (3000 :: Int) t (0, 0)   -- 何度もくり返して求める（k < 1 なら必ず着く）
      others = [ [(1.6 * cos a, 1.6 * sin a)] | i <- [0 .. 13 :: Int], let a = fromIntegral i / 14 * 2 * pi ]
  in w { wFixed = fixedP, wSeq = take 81 (iterate t (wStart w)), wShown = 1, wOthers = others }

iterate' :: Int -> (a -> a) -> a -> a
iterate' 0 _ !x = x
iterate' n f !x = iterate' (n - 1) f (f x)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRunning w && envFrame env `mod` 20 == 0 && wShown w < length (wSeq w) =
      let grow o = case o of
            (q : _) | length o < 40 -> mapT w q : o
            _ -> o
      in pure w { wShown = wShown w + 1, wOthers = map grow (wOthers w) }
  | otherwise = pure w

-- レイアウト
cx0, cy0, unitU :: Env -> Float
cx0 env = envW env * 0.3
cy0 env = envH env * 0.57
unitU env = min (envW env) (envH env) * 0.2

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  squares
  ++ concatMap drawOther (wOthers w)
  ++ [ Color pink (thickLine 2 a b) | (a, b) <- zip mainPts (drop 1 mainPts) ]
  ++ [ spot (hsba 320 70 100 (if i == (0 :: Int) then 1 else 0.8)) p (if i == 0 then 12 else 7) | (i, p) <- zip [0 ..] mainPts ]
  ++ [ spot (hsb 45 90 100) (toS (wFixed w)) 14 | wK w < 1 ]
  ++ drawErrorPlot env w
  ++ [ txt 20 34 15 white ("k = " ++ nf 2 (wK w) ++ " (Left/Right)  theta = " ++ nf 2 (wTheta w) ++ " (Up/Down)  N: " ++ (if wNon w then "curved map" else "straight (similarity)") ++ "  click: start  Space: " ++ (if wRunning w then "pause" else "run") ++ "  R: restart")
     , txt 20 60 14 (hsb 0 0 60)
         (if wK w < 1
            then "Repeating T on the square Q gives nested shapes that shrink to one point (yellow = fixed point). Every start (white) gets there."
            else "k >= 1: not a contraction. Even if a fixed point exists, the sequence does not reach it (it spreads or keeps circling).")
     ]
  where
    pink = hsb 320 70 100
    toS (x, y) = (cx0 env + realToFrac x * unitU env, cy0 env - realToFrac y * unitU env)
    mainPts = map toS (take (wShown w) (wSeq w))
    -- 正方形 Q, T(Q), T²(Q), … ：入れ子になって 1 点に縮む
    corners = [(-1.6, -1.6), (1.6, -1.6), (1.6, 1.6), (-1.6, 1.6)] :: [P]
    sq0 = concat (zipWith edge corners (drop 1 corners ++ take 1 corners))
    edge (ax, ay) (qx, qy) = [ (ax + (qx - ax) * t, ay + (qy - ay) * t) | j <- [0 .. 19 :: Int], let t = fromIntegral j / 20 ]
    polys = take 9 (iterate (map (mapT w)) sq0)
    squares = [ Color (hsba (200 + fromIntegral n * 14) 50 90 ((70 - fromIntegral n * 6) / 100))
                  (if n == (0 :: Int) then thickPath 2 (closed pts) else Line (closed pts))
              | (n, poly') <- zip [0 ..] polys, let pts = map toS poly' ]
    closed ps = ps ++ take 1 ps
    -- ほかの出発点の軌跡（どこから始めても同じ点へ）
    drawOther o = Color (hsba 0 0 70 0.4) (Line (map toS o))
                  : [ spot (hsba 0 0 80 0.7) (toS q) 5 | q <- take 1 o ]

-- 右側：誤差 |x_n − x*| の対数グラフと事前評価
drawErrorPlot :: Env -> World -> [Picture]
drawErrorPlot env w =
  [ box (hsb 0 0 35) x0p y0p pw h ]
  ++ concat [ [ Color (hsb 0 0 25) (Line [(x0p, gyv (10 ** e)), (x0p + pw, gyv (10 ** e))])
              , txt (x0p - 48) (gyv (10 ** e) + 5) 14 (hsb 0 0 55) ("1e" ++ show (round e :: Int)) ]
            | e <- [1, -2, -5, -8, -11 :: Double] ]
  ++ [ Color (hsba 45 90 100 0.6) (Line [(x0p, gyv eps), (x0p + pw, gyv eps)]) ]
  ++ [ Color (hsb 190 70 100) (dashed 6 6 1.5 [ (x0p + fromIntegral n / fromIntegral nMax * pw, gyv (k ^ n / (1 - k) * d01)) | n <- [0 .. nMax] ]) | k < 1 ]
  ++ [ spot (hsb 320 70 100) (x0p + fromIntegral n / fromIntegral nMax * pw, gyv e) 6 | (n, e) <- errs ]
  ++ [ txt x0p (y0p - 12) 14 (hsb 0 0 70) "Error |x_n - x*| (vertical: log, horizontal: step n)" ]
  ++ notes
  where
    k = wK w
    x0p = envW env * 0.6
    pw = envW env * 0.36
    y0p = 130
    h = envH env * 0.42
    nMax = 60 :: Int
    top = 1 :: Float
    bot = -12 :: Float
    gyv :: Double -> Float
    gyv v = remap (clampF bot top (realToFrac (logBase 10 (max v 1e-14)))) top bot y0p (y0p + h)
    (s0, s1) = case wSeq w of
      (a : b : _) -> (a, b)
      _ -> ((0, 0), (0, 0))
    dd (a, b) (c, d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))
    d01 = dd s0 s1
    errs = zip [0 :: Int ..] (map (`dd` wFixed w) (take (min (wShown w) (nMax + 1)) (wSeq w)))
    reached = [n | (n, e) <- errs, e < eps]
    epsS = "1e-6"
    ty = y0p + h + 30
    notes
      | k < 1 =
          let need = ceiling (log (eps * (1 - k) / max d01 1e-12) / log k) :: Int
          in [ txt x0p ty 14 white "A priori bound (dashed): |x_n-x*| <= k^n/(1-k) |x1-x0|"
             , txt x0p (ty + 26) 14 (hsb 190 70 100) ("-> error <= " ++ epsS ++ " needs n >= " ++ show (max 0 need) ++ " (known in advance)")
             , txt x0p (ty + 52) 14 (hsb 320 70 100) ("Actually went below " ++ epsS ++ " at n = " ++ (case reached of { (n : _) -> show n; [] -> "..." }))
             , txt x0p (ty + 80) 14 (hsb 0 0 60) ("Slope = log10 k = " ++ nf 3 (logBase 10 k) ++ " (" ++ nf 2 (negate (logBase 10 k)) ++ " digits per step)")
             , txt x0p (ty + 104) 14 (hsb 0 0 60) "A posteriori: |x_n-x*| <= k/(1-k) |x_n - x_(n-1)|"
             , txt x0p (ty + 124) 14 (hsb 0 0 60) "(used to decide when to stop)"
             ]
      | otherwise = [ txt x0p ty 15 (hsb 0 80 100) "k >= 1: the bounds do not apply" ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in pure (if mx < envW env * 0.56 && my > 90
               then rebuild w { wStart = (realToFrac ((mx - cx0 env) / unitU env), realToFrac ((cy0 env - my) / unitU env)) }
               else w)
  -- p5 版と同じく、どのキーでも作り直す
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (rebuild w { wK = max 0.1 (wK w - 0.05) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild w { wK = min 1.1 (wK w + 0.05) })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild w { wTheta = wTheta w + 0.1 })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild w { wTheta = wTheta w - 0.1 })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (rebuild w { wRunning = not (wRunning w) })
  EventKey (Char c) Down _ _
    | toLower c == 'n' -> pure (rebuild w { wNon = not (wNon w) })
    | toLower c == 'r' -> pure (rebuild w { wStart = (-1.3, 1.1) })
    | otherwise -> pure (rebuild w)
  EventKey (SpecialKey _) Down _ _ -> pure (rebuild w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. k = 0.5 と k = 0.9 で、誤差のグラフの傾きを比べよう。10 桁合わせるのに何回ずつ要る？
-- 2. k = 1、θ = 0.6 にすると？（回るだけで近づかない。不動点はあるのに着かない）
-- 3. N の曲がった写像でも直線のグラフになる。曲がっていても「縮小率 k 以下」なら定理が使える
-- 4. eps を 1e-10 にして、事前評価の回数と実際の回数を比べよう。事前評価はいつも「多め」に言う

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

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

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
                    m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
    go _ _ _ = []
