-- ============================================
-- 49-04 慣性項とネステロフの加速
-- 最急降下法（GD）に「勢い」をつける。重い球（ヘビーボール）：x ← x − α∇f(x) + β(x − 前の x)
--   ネステロフ：先に勢いで進んだ点 y で勾配を計算する。2 次関数では 1 回あたりの縮み方が 1 − 1/κ → 1 − 1/√κ に
--   3 = たくさんの変数の「鎖」の 2 次関数で、f − f* の上限 O(1/k)（GD）と O(1/k²)（ネステロフ）を比べる
-- 操作：1 = 2 次関数 / 2 = log-sum-exp / 3 = 鎖（n 次元）/ ↑↓ = κ / クリック = 初期点 / スペース = 自動 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kaps :: [Double]
kaps = [2, 5, 10, 30, 100, 300]   -- ★ 条件数 κ の候補

kMax :: Int
kMax = 600                        -- ★ 2 次元での反復回数

nCh, kCh :: Int
nCh = 150                         -- ★ 鎖の次元 n
kCh = 3000                        -- ★ 鎖の反復回数

-- 3 つの方法の名前と色
methods :: [(String, Color)]
methods = [("GD (t = 1/L)", cCy), ("Heavy ball", cPk), ("Nesterov", cOr)]

-- 1 つの方法の記録：点、f − f*、‖∇f‖、‖x − x*‖
data Run = Run { rP :: [V2], rFv, rGn, rEn :: [Double] }

-- 鎖の状態（GD とネステロフを同時に）。fg・fn は新しい順
data Chain = Chain
  { chXs :: !(U.Vector Double), chR2 :: !Double, chL :: !Double, chK :: !Int
  , chGd, chX, chXp :: !(U.Vector Double), chFg, chFn :: [Double] }

data World = World
  { wWhich :: !Int            -- 0 = 2 次関数、1 = log-sum-exp、2 = 鎖
  , wKi    :: !Int
  , wF     :: Fn
  , wV     :: !View
  , wCI    :: Picture
  , wX0    :: !V2
  , wRuns  :: [Run]
  , wShown :: !Int
  , wAuto  :: !Bool
  , wCh    :: Chain
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-04 Momentum and Nesterov acceleration" w00)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env True w00 { wTests = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where w00 = World 0 3 (quadFn 30 0.5) (makeView 16 74 614 676 (0, 0, 3.2)) Blank (0, 0) [] 0 True (chainStart nCh) []

rebuild :: Env -> Bool -> World -> World
rebuild env newFn w0
  | wWhich w0 == 2 = w0 { wCh = chainStart nCh }
  | otherwise = w { wRuns = race (wF w) (wX0 w) kMax, wShown = 0 }
  where
    w | newFn =
          let f = if wWhich w0 == 0 then quadFn (getAt 30 kaps (wKi w0)) 0.5 else lse
              v = makeView 16 74 (realToFrac (envW env) * 0.48) (realToFrac (envH env) - 124) (fnView f)
              th = fnTh f
              x0 = if wWhich w0 == 0 then (-2.6 * cos th - 0.5 * sin th, -2.6 * sin th + 0.5 * cos th) else fnStart f   -- 2 次関数：ゆるい谷の向きから
          in w0 { wF = f, wV = v, wCI = fst (contourImage v (fnF f) 18), wX0 = x0 }
      | otherwise = w0

-- μ と L：2 次関数はちょうど 1 と κ。log-sum-exp は L ≤ 10、μ は最小点でのヘッセ行列の小さい固有値（局所的な値）
data Params = Params { pMu, pL, pAlpha, pBeta, pBn :: !Double }

params :: Fn -> Params
params f = Params mu l (4 / (sqrt l + sqrt mu) ^ (2 :: Int)) (q * q) q
  where
    mu = if fnMu f /= 0 then fnMu f else eL1 (eig2 (uncurry (fnH f) (getAt (0, 0) (fnXs f) 0)))
    l = fnL f
    sk = sqrt (l / mu)
    q = (sk - 1) / (sk + 1)

-- 3 つの方法を同じ初期点から
race :: Fn -> V2 -> Int -> [Run]
race f p0 kk = [runM m | m <- [0 .. 2 :: Int]]
  where
    pp = params f
    xs = getAt (0, 0) (fnXs f) 0
    fv x = uncurry (fnF f) x - fnFs f
    runM m = go 0 p0 p0 [p0] [fv p0] [vlen (uncurry (fnG f) p0)] [errTo f p0]
      where
        go k x xp ps fs gs es
          | k >= kk || not (headD fs > 1e-300) = done ps fs gs es    -- 1e−300 まで（遅い成分の速さを長く測る）
          | bad fn = done ps (0 / 0 : fs) gs es
          | otherwise = go (k + 1) xn x (xn : ps) (fn : fs) (vlen (uncurry (fnG f) xn) : gs) (vlen (vsub xn xs) : es)
          where
            xn = case m of
              0 -> vaxpy (-1 / pL pp) (uncurry (fnG f) x) x
              1 -> vadd (vaxpy (-pAlpha pp) (uncurry (fnG f) x) x) (vscale (pBeta pp) (vsub x xp))
              _ -> let y = vaxpy (pBn pp) (vsub x xp) x in vaxpy (-1 / pL pp) (uncurry (fnG f) y) y
            fn = fv xn
        done ps fs gs es = Run (reverse ps) (reverse fs) (reverse gs) (reverse es)
    headD (v : _) = v
    headD [] = 0

-- 反復行列の固有値：x_{k+1} = a x_k − b x_{k−1}（固有値 λ の成分ごと）→ z² − a z + b = 0 の根の絶対値の大きいほう
rootR :: Double -> Double -> Double
rootR a b = let d = a * a - 4 * b in if d >= 0 then max (abs (a + sqrt d)) (abs (a - sqrt d)) / 2 else sqrt b

specRad :: Int -> Params -> [Double] -> Double
specRad m pp lams = foldl' max (-1 / 0) (map one lams)
  where
    one l = case m of
      0 -> abs (1 - l / pL pp)
      1 -> rootR (1 + pBeta pp - pAlpha pp * l) (pBeta pp)
      _ -> rootR ((1 - l / pL pp) * (1 + pBn pp)) ((1 - l / pL pp) * pBn pp)

closedForm :: Int -> Params -> (Double, String)
closedForm m pp = getAt (0, "") [(1 - 1 / k, "1 - 1/k"), ((s - 1) / (s + 1), "(sk-1)/(sk+1)"), (1 - 1 / s, "1 - 1/sk")] m
  where
    k = pL pp / pMu pp
    s = sqrt k

-- 最後の w 回で ‖x − x*‖ が 1 回あたり何倍になったか
lateRate :: [Double] -> Double
lateRate es = if w < 1 then 0 / 0 else (ev U.! n / ev U.! (n - w)) ** (1 / fromIntegral w)
  where
    ev = U.fromList es
    n = lastGood (U.length ev - 1)
    lastGood i = if i > 0 && not (ev U.! i > 1e-250) then lastGood (i - 1) else i
    w = min 50 (n `div` 2)

-- ---- 鎖の 2 次関数 f(x) = ½ xᵀTx − x₁（T = 三重対角 2, −1）。L = 4、x*ᵢ = 1 − i/(n+1)
chainGrad :: U.Vector Double -> U.Vector Double
chainGrad x = U.imap (\i v -> 2 * v - (if i > 0 then x U.! (i - 1) else 0) - (if i < n - 1 then x U.! (i + 1) else 0) - (if i == 0 then 1 else 0)) x
  where n = U.length x

chainGap :: U.Vector Double -> U.Vector Double -> Double    -- f − f* = ½ eᵀTe
chainGap x xs = U.sum (U.imap (\i ei -> ei * (2 * ei - (if i > 0 then e U.! (i - 1) else 0) - (if i < n - 1 then e U.! (i + 1) else 0))) e) / 2
  where
    e = U.zipWith (-) x xs
    n = U.length e

chainStart :: Int -> Chain
chainStart n = Chain xs (U.sum (U.map (\v -> v * v) xs)) 4 0 z z z [g0] [g0]
  where
    xs = U.generate n (\i -> 1 - fromIntegral (i + 1) / fromIntegral (n + 1))
    z = U.replicate n 0
    g0 = chainGap z xs

-- GD：x ← x − ∇f/L　ネステロフ：y = x_k + (k−1)/(k+2)(x_k − x_{k−1})、x_{k+1} = y − ∇f(y)/L
chainIter :: Chain -> Chain
chainIter c = c { chGd = gd', chX = x', chXp = chX c, chK = k + 1, chFg = chainGap gd' (chXs c) : chFg c, chFn = chainGap x' (chXs c) : chFn c }
  where
    gd' = U.zipWith (\v g -> v - g / chL c) (chGd c) (chainGrad (chGd c))
    k = chK c
    m = if k >= 1 then fromIntegral (k - 1) / fromIntegral (k + 2) else 0
    y = U.zipWith (\v vp -> v + m * (v - vp)) (chX c) (chXp c)
    x' = U.zipWith (\v g -> v - g / chL c) y (chainGrad y)

---------------------------------------------------------------- 時間

step :: Env -> Float -> World -> IO World
step _ _ w
  | not (wAuto w) = pure w
  | wWhich w == 2 = pure w { wCh = iterN (10 :: Int) (wCh w) }
  | wShown w < nMax w - 1 = pure w { wShown = wShown w + 1 }
  | otherwise = pure w
  where
    iterN 0 c = c
    iterN i c = if chK c < kCh then iterN (i - 1) (chainIter c) else c

nMax :: World -> Int
nMax w = maximum (1 : map (length . rP) (wRuns w))

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ hud [ (getAt "" [fnName (wF w), "log-sum-exp (convex)", "Chain quadratic (n = " ++ show nCh ++ ", L = 4, x0 = 0)"] (wWhich w) ++ " -- momentum and Nesterov", cInk)
        , ("1 quadratic  2 log-sum-exp  3 chain  Up/Down kappa  click start  Space auto " ++ (if wAuto w then "ON" else "off") ++ "  R restart", cDim) ] 16 14 16 1.45 ]
  ++ (if wWhich w == 2 then drawChain env (wCh w) else draw2 env w)
  ++ [ drawSelfTest env (wTests w) ]

draw2 :: Env -> World -> [Picture]
draw2 env w =
  [ wCI w ]
  ++ [ drawPathClip v (take (wShown w + 1) (rP r)) (snd m) 2 | (r, m) <- zip (wRuns w) methods ]
  ++ velo
  ++ [ hud [ ("mu = " ++ fmt 4 (pMu pp) ++ (if wWhich w == 1 then " (local)" else "") ++ "  L = " ++ fmt 3 (pL pp) ++ "  kappa = " ++ fmt 4 (pL pp / pMu pp) ++ "  Nesterov momentum " ++ fmt 3 (pBn pp), cDim)
           , ("heavy ball a = 4/(sqrt L + sqrt mu)^2 = " ++ fmt 3 (pAlpha pp) ++ "  b = " ++ fmt 3 (pBeta pp), cDim)
           , ("Factor per step of |x - x*|: formula / |eigenvalue| / measured (last 50)", cInk) ] x y 14 1.45 ]
  ++ [ txt (x + 150) (y + 84) 14 cDim "formula", txt (x + 390) (y + 84) 14 cDim "eigenvalue", txt (x + 500) (y + 84) 14 cDim "measured" ]
  ++ concat [ [ txt x yy 14 col nm
              , txt (x + 150) yy 14 cInk (s ++ " = " ++ fmt 4 c)
              , txt (x + 390) yy 14 cInk (if wWhich w == 0 then fmt 4 (specRad i pp lams) else "(not quad.)")
              , txt (x + 500) yy 14 cInk (fmt 4 (lateRate (rEn r))) ]
            | (i, r, (nm, col)) <- zip3 [0 ..] (wRuns w) methods, let (c, s) = closedForm i pp, let yy = y + 108 + fromIntegral i * 24 ]
  ++ [ hud (if wWhich w == 0
              then [("Assumes a quadratic f, alpha and beta optimal from mu and L. Double root:", cDim), ("error ~ k rho^k (so measured is a bit larger)", cDim)]
              else [("Not quadratic: the heavy-ball rate is not guaranteed", cDim), ("(near the minimum f is close to a quadratic)", cDim)]) x (y + 166) 14 1.3
     , logPlot (x + 44) top (wd - 50) ph (sl rFv) plotOpt { poLo = Just (-80), poHi = Just 4, poKmax = Just (n - 1), poTitle = "f(x_k) - f*" }
     , logPlot (x + 44) (top + ph + 54) (wd - 50) ph (sl rGn) plotOpt { poLo = Just (-40), poHi = Just 3, poKmax = Just (n - 1), poTitle = "|grad f(x_k)|" } ]
  where
    v = wV w
    pp = params (wF w)
    lams = [pMu pp, pL pp]
    x = envW env * 0.51
    y = 80
    wd = envW env - x - 24
    top = y + 232
    ph = (envH env - top - 110) / 2
    n = nMax w
    sl key = [ series (take (wShown w + 1) (key r)) col nm | (r, (nm, col)) <- zip (wRuns w) methods ]
    -- 重い球の「速度」 x_k − x_{k−1}（4 倍で表示）
    velo = case drop 1 (wRuns w) of
      (r : _) ->
        let ps = rP r
            k = min (wShown w) (length ps - 1)
            pk = getAt (0, 0) ps k
            pk1 = getAt (0, 0) ps (k - 1)
            a = toS v pk
            b = toS v (vaxpy 4 (vsub pk pk1) pk)
        in if k > 0 && insideS v a
             then [ maybe Blank (\(p, q) -> arrowP p q cYe 2) (clipSeg (rectOf v) a b), Color cPk (at a (disc 9)) ]
             else []
      [] -> []

drawChain :: Env -> Chain -> [Picture]
drawChain env c =
  [ Color (rgba255 8 26 50 230) (rectTL x0 y0 w h), Color cLine (rectLineTL x0 y0 w h) ]
  ++ concat [ [ Color (rgba255 255 255 255 40) (rectTL xx (y0 + h * (1 - f xsi)) bw (h * f xsi))
              , Color cCy (rectTL xx (y0 + h * (1 - f gi)) (bw * 0.45) (h * f gi))
              , Color cOr (rectTL (xx + bw * 0.5) (y0 + h * (1 - f (min 1.2 xi))) (bw * 0.45) (h * f (min 1.2 xi))) ]
            | i <- [0 .. chN - 1], let xx = x0 + fromIntegral i * bw
            , let xsi = chXs c U.! i, let gi = chGd c U.! i, let xi = chX c U.! i ]
  ++ [ hud [ ("x1 ... xn (white = answer x*, blue = GD, orange = Nesterov)  k = " ++ show (chK c), cInk)
           , ("The gradient only reaches the next neighbour: after k steps x_(k+1)...", cDim)
           , ("are still 0 (for any gradient method)", cDim) ] x0 (y0 + h + 10) 14 1.4
     , logPlot (x + 44) y (wd - 50) (envH env - y - 130)
         [ series (reverse (chFg c)) cCy "GD f - f*", series (reverse (chFn c)) cOr "Nesterov (k-1)/(k+2)"
         , (series (bnd (\k -> chL c * chR2 c / (2 * k))) cCy "bound L|x0-x*|^2/(2k)") { sDash = True }
         , (series (bnd (\k -> 2 * chL c * chR2 c / (k + 1) ^ (2 :: Int))) cOr "bound 2L|x0-x*|^2/(k+1)^2") { sDash = True } ]
         plotOpt { poLogx = True, poKmax = Just kCh, poLo = Just (-8), poHi = Just 3, poTitle = "log-log: f(x_k) - f* (convex, L-smooth: below the bound for every k)" }
     , hud [ ("Assumes f convex with L-Lipschitz gradient (no strong convexity).", cDim)
           , ("In the strongly convex 2D examples the bound is not visible.", cDim) ] x (envH env - 76) 14 1.35 ]
  where
    chN = U.length (chXs c)
    x0 = 16
    y0 = 120
    w = envW env * 0.46
    h = envH env - 240
    bw = w / fromIntegral chN
    f = d2f
    x = envW env * 0.51
    y = 120
    wd = envW env - x - 30
    bnd g = [ if k == 0 then 0 / 0 else g (fromIntegral k) | k <- [0 .. chK c] ]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | wWhich w < 2 && inside (wV w) (toP5 env p) -> rebuild env False w { wX0 = toW (wV w) (toP5 env p) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> rebuild env True w { wWhich = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> rebuild env False w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyUp) Down _ _ | wKi w < length kaps - 1 -> kap (wKi w + 1)
  EventKey (SpecialKey KeyDown) Down _ _ | wKi w > 0 -> kap (wKi w - 1)
  _ -> w
  where kap i = let w' = w { wKi = i } in if wWhich w == 0 then rebuild env True w' else w'

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ let (eg, eh) = derivErr f [(0.4, -0.3), (-1.1, 0.8), (1.5, 0.2)]
    in stOk (fnName f ++ ": gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-5) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
  | f <- [quadFn 10 0.5, lse] ]
  ++ concatMap kapTests [10, 100]
  ++ [ stOk "Chain: grad f = 0 at x*_i = 1 - i/(n+1)" (U.maximum (U.map abs (chainGrad (chXs c0))) < 1e-12) ""
     , stOk ("Chain (n = " ++ show nCh ++ "): GD f - f* <= L|x0-x*|^2/(2k) (k = 1..1000)") okg
         ("k=1000: " ++ fmt 3 (getAt 0 fg1000 0) ++ " <= " ++ fmt 3 (chL c0 * chR2 c0 / 2000))
     , stOk "Chain: Nesterov f - f* <= 2L|x0-x*|^2/(k+1)^2 (k = 1..1000)" okn
         ("k=1000: " ++ fmt 3 (getAt 0 fn1000 0) ++ " <= " ++ fmt 3 (2 * chL c0 * chR2 c0 / 1001 ^ (2 :: Int)))
     , stOk "Chain: after 20 steps x_21... are still 0 (gradient spreads one neighbour per step)"
         (U.all (== 0) (U.drop 20 (chGd c20)) && U.all (== 0) (U.drop 20 (chX c20))) ""
     , stOk "log-sum-exp: all three reach the minimum (-ln2/2, 0)" (all (\r -> lastD (rEn r) < 1e-8) (race lse (fnStart lse) 600)) "" ]
  where
    rel a b = abs (a - b) / b
    c0 = chainStart nCh
    c1000 = iterate chainIter c0 !! 1000
    fg1000 = chFg c1000            -- 新しい順：先頭が k = 1000
    fn1000 = chFn c1000
    ks = [1000, 999 .. 1] :: [Int]
    okg = and [ v <= chL c0 * chR2 c0 / (2 * fromIntegral k) | (k, v) <- zip ks fg1000 ]
    okn = and [ v <= 2 * chL c0 * chR2 c0 / fromIntegral (k + 1) ^ (2 :: Int) | (k, v) <- zip ks fn1000 ]
    c20 = iterate chainIter c0 !! 20
    lastD xs = if null xs then 0 / 0 else last xs
    kapTests kap =
      let q0 = quadFn kap 0.5
          pp = params q0
          rs = race q0 (-2.2, 1.7) 1500
          lams = [1, kap]
          rows = [ (c, sr, me, m) | (m, r) <- zip [0 .. 2 :: Int] rs, let c = fst (closedForm m pp), let sr = specRad m pp lams, let me = lateRate (rEn r) ]
          okc = and [ rel sr c < 1e-6 | (c, sr, _, _) <- rows ]
          okm = and [ rel me sr < (if m == 0 then 1e-9 else 0.01) | (_, sr, me, m) <- rows ]
          info = concat [ getAt "" ["GD", "heavy ball", "Nesterov"] m ++ " formula " ++ fmt 5 c ++ " eig " ++ fmt 5 sr ++ " measured " ++ fmt 5 me ++ "; " | (c, sr, me, m) <- rows ]
          q = 1 - 1 / sqrt kap
          fv2 = rFv (getAt (Run [] [] [] []) rs 2)
          cc0 = getAt 0 fv2 0 + 0.5 * hypot (-2.2) 1.7 ^ (2 :: Int)
          okb = and [ v <= q ^ k * cc0 * (1 + 1e-9) + 1e-300 | (k, v) <- zip [0 :: Int ..] fv2 ]
          ks' = "kappa=" ++ show (round kap :: Int)
      in [ stOk (ks' ++ ": formula = |eigenvalue| of the iteration matrix (1-1/k, (sk-1)/(sk+1), 1-1/sk)") okc ""
         , stOk (ks' ++ ": measured ratio of |x| = |eigenvalue| (GD 1e-9, the two double roots 1%)") okm info
         , stOk (ks' ++ ": Nesterov f - f* <= (1-1/sk)^k (f0 - f* + mu/2 |x0-x*|^2) for every k") okb "" ]

-- ---------- やってみよう ----------
-- 1. κ = 300 にすると、GD はほとんど進まないのに重い球とネステロフは速い。1−1/κ と 1−1/√κ の差
-- 2. 黄色の矢印は重い球の速度。谷底を行ったり来たり（振動）しながら進む
-- 3. 2（log-sum-exp）では μ は局所的な値なので、理論の保証はない。それでも速い？
-- 4. 3（鎖）は「どんな勾配法でもこれ以上速くできない」ネステロフの例。上限の点線と実測を比べよう
-- 5. params の pBeta を 0.99 にすると重い球はどうなる？（大きすぎる勢いは振動して遅くなる）

-- ---------- 道具 ----------
-- 表示の枠（左, 上, 右, 下）
rectOf :: View -> (Float, Float, Float, Float)
rectOf v = (d2f (vPx v), d2f (vPy v), d2f (vPx v + vPw v), d2f (vPy v + vPh v))

insideS :: View -> Point -> Bool
insideS v (x, y) = let (l, t, r, b) = rectOf v in x >= l && x <= r && y >= t && y <= b

-- 線分を長方形に切りつめる（リャン・バースキー）。全部外なら Nothing
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (l, t, r, b) (x1, y1) (x2, y2) = go [(-dx, x1 - l), (dx, r - x1), (-dy, y1 - t), (dy, b - y1)] 0 1
  where
    dx = x2 - x1
    dy = y2 - y1
    go [] t0 t1 = if t0 <= t1 then Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy)) else Nothing
    go ((p, q) : rest) t0 t1
      | p == 0 = if q < 0 then Nothing else go rest t0 t1
      | p < 0 = go rest (max t0 (q / p)) t1
      | otherwise = go rest t0 (min t1 (q / p))

-- 折れ線を長方形の中だけの折れ線たちにする
clipPoly :: (Float, Float, Float, Float) -> [Point] -> [[Point]]
clipPoly rc ps = filter ((>= 2) . length) (go (zip ps (drop 1 ps)) [])
  where
    go [] cur = [reverse cur]
    go ((a, b) : rest) cur = case clipSeg rc a b of
      Nothing -> reverse cur : go rest []
      Just (p, q) ->
        let cur' = if null cur then [q, p] else q : cur
        in if q /= b then reverse cur' : go rest [] else go rest cur'

-- 枠の外を描かない drawPath
drawPathClip :: View -> [V2] -> Color -> Float -> Picture
drawPathClip _ [] _ _ = Blank
drawPathClip v ps c w = Pictures (map (Color c . fatLine w) (clipPoly (rectOf v) ss) ++ ds ++ [dot (last ss) 5.5 c | insideS v (last ss)])
  where
    ss = map (toS v) ps
    stepN = max 1 (length ss `div` 60)
    ds = [Color c (at s (disc 2.5)) | (i, s) <- zip [0 :: Int ..] ss, i `mod` stepN == 0, insideS v s]

-- ==================================================
-- ここから下は「最適化の道具」（中身は読まなくても OK。消さないでね）
-- テスト関数（値・勾配・ヘッセ行列）、等高線、対数グラフ、2×2 行列、直線探索、自己テスト
-- 計算はすべて Double（倍精度）で行い、描くときだけ Float にする（1e-10 より小さい誤差まで測るため）
-- ==================================================

-- ---- 色（p5.js 版の PAL）
cBg, cInk, cDim, cLine, cOr, cCy, cGr, cYe, cPk, cVi, cRd :: Color
cBg = rgb255 12 36 69
cInk = rgb255 232 242 251
cDim = rgb255 142 178 207
cLine = rgb255 44 90 133
cOr = rgb255 255 122 47
cCy = rgb255 95 216 240
cGr = rgb255 141 224 122
cYe = rgb255 255 210 74
cPk = rgb255 255 111 168
cVi = rgb255 183 156 255
cRd = rgb255 255 90 90

-- ---- 2 次元のベクトルと 2×2 行列
type V2 = (Double, Double)
data M2 = M2 !Double !Double !Double !Double     -- [[a, b], [c, d]]

vadd, vsub :: V2 -> V2 -> V2
vadd (a, b) (c, d) = (a + c, b + d)
vsub (a, b) (c, d) = (a - c, b - d)

vscale :: Double -> V2 -> V2
vscale k (a, b) = (k * a, k * b)

vaxpy :: Double -> V2 -> V2 -> V2    -- a·x + y
vaxpy k (a, b) (c, d) = (c + k * a, d + k * b)

vdot :: V2 -> V2 -> Double
vdot (a, b) (c, d) = a * c + b * d

vlen :: V2 -> Double
vlen (a, b) = hypot a b

hypot :: Double -> Double -> Double
hypot a b = let m = max (abs a) (abs b) in if m == 0 || isInfinite m then m else m * sqrt ((a / m) ^ (2 :: Int) + (b / m) ^ (2 :: Int))

mvec :: M2 -> V2 -> V2
mvec (M2 a b c d) (x, y) = (a * x + b * y, c * x + d * y)

-- 描くために Float へ（NaN は 0、とても大きい値は ±1e5 に切りつめる）
d2f :: Double -> Float
d2f v = if isNaN v then 0 else realToFrac (max (-1e5) (min 1e5 v))

-- 発散したか（NaN・無限大・とても大きい）
bad :: Double -> Bool
bad v = isNaN v || isInfinite v || abs v > 1e12

finite :: Double -> Bool
finite v = not (isNaN v || isInfinite v)

-- 対称行列の固有値（小さい順 l1 ≤ l2）と固有ベクトル
data Eig = Eig { eL1, eL2 :: !Double, eV1, eV2 :: !V2 }

eig2 :: M2 -> Eig
eig2 (M2 a b _ d) = Eig l1 (t + r) v (negate (snd v), fst v)
  where
    t = (a + d) / 2
    r = hypot ((a - d) / 2) b
    l1 = t - r
    v0 | abs b > 1e-14 * (abs a + abs d + 1) = (l1 - d, b)
       | a <= d = (1, 0)
       | otherwise = (0, 1)
    v = vscale (1 / vlen v0) v0

solve2 :: M2 -> V2 -> V2
solve2 (M2 a b c d) (r0, r1) = let det = a * d - b * c in ((d * r0 - b * r1) / det, (a * r1 - c * r0) / det)

-- ---- テスト関数：値 f、勾配 g、ヘッセ行列 h、最小点 xs、最小値 fs、表示範囲 view（中心 x, 中心 y, 横の半幅）
data Fn = Fn
  { fnName  :: String
  , fnF     :: Double -> Double -> Double
  , fnG     :: Double -> Double -> V2
  , fnH     :: Double -> Double -> M2
  , fnXs    :: [V2]
  , fnFs    :: Double
  , fnView  :: (Double, Double, Double)
  , fnStart :: V2
  , fnMu    :: Double            -- 強凸性 μ（0 = わからない）
  , fnL     :: Double            -- 勾配のリプシッツ定数 L（0 = わからない）
  , fnConvex :: Bool
  , fnTh    :: Double            -- 2 次関数の回転角
  }

fn0 :: Fn
fn0 = Fn "" (\_ _ -> 0) (\_ _ -> (0, 0)) (\_ _ -> M2 0 0 0 0) [] 0 (0, 0, 1) (0, 0) 0 0 False 0

-- f = ½ xᵀAx、A の固有値は μ = 1 と L = κ（θ だけ回転）
quadFn :: Double -> Double -> Fn
quadFn kap th = fn0
  { fnName = "quadratic kappa = " ++ fmt 3 kap
  , fnF = \x y -> 0.5 * (a * x * x + 2 * b * x * y + d * y * y)
  , fnG = \x y -> (a * x + b * y, b * x + d * y)
  , fnH = \_ _ -> M2 a b b d
  , fnMu = 1, fnL = kap, fnXs = [(0, 0)], fnFs = 0, fnView = (0, 0, 3.2), fnStart = (-2.6, 1.6), fnConvex = True, fnTh = th }
  where
    c = cos th
    s = sin th
    a = c * c + kap * s * s
    b = (1 - kap) * c * s
    d = s * s + kap * c * c

rosen :: Fn
rosen = fn0
  { fnName = "Rosenbrock"
  , fnF = \x y -> (1 - x) ^ (2 :: Int) + 100 * (y - x * x) ^ (2 :: Int)
  , fnG = \x y -> (-2 * (1 - x) - 400 * x * (y - x * x), 200 * (y - x * x))
  , fnH = \x y -> M2 (2 - 400 * (y - x * x) + 800 * x * x) (-400 * x) (-400 * x) 200
  , fnXs = [(1, 1)], fnFs = 0, fnView = (-0.2, 0.9, 2.1), fnStart = (-1.2, 1) }

himmel :: Fn
himmel = h { fnXs = map (\p -> fromMaybe p (criticalPoint h p 60)) (fnXs h) }   -- 4 つの最小点を ∇f = 0 のニュートン法で精密に
  where
    h = fn0
      { fnName = "Himmelblau"
      , fnF = \x y -> (x * x + y - 11) ^ (2 :: Int) + (x + y * y - 7) ^ (2 :: Int)
      , fnG = \x y -> (4 * x * (x * x + y - 11) + 2 * (x + y * y - 7), 2 * (x * x + y - 11) + 4 * y * (x + y * y - 7))
      , fnH = \x y -> M2 (12 * x * x + 4 * y - 42) (4 * x + 4 * y) (4 * x + 4 * y) (4 * x + 12 * y * y - 26)
      , fnXs = [(3, 2), (-2.8051, 3.1313), (-3.7793, -3.2832), (3.5844, -1.8481)], fnFs = 0, fnView = (0, 0, 6.2), fnStart = (-0.3, -0.9) }

beale :: Fn
beale = fn0
  { fnName = "Beale"
  , fnF = \x y -> sum [r * r | (r, _, _, _, _) <- terms x y]
  , fnG = \x y -> foldl' (\(p, q) (r, rx, ry, _, _) -> (p + 2 * r * rx, q + 2 * r * ry)) (0, 0) (terms x y)
  , fnH = \x y -> let (a, b, d) = foldl' (\(a0, b0, d0) (r, rx, ry, rxy, ryy) -> (a0 + 2 * rx * rx, b0 + 2 * (rx * ry + r * rxy), d0 + 2 * (ry * ry + r * ryy))) (0, 0, 0) (terms x y)
                  in M2 a b b d
  , fnXs = [(3, 0.5)], fnFs = 0, fnView = (0.6, 0, 4.6), fnStart = (-1.5, 1.8) }
  where
    terms x y = [ (c - x + x * y ^ n, y ^ n - 1, fromIntegral n * x * y ^ (n - 1), fromIntegral n * y ^ (n - 1)
                  , if n > 1 then fromIntegral (n * (n - 1)) * x * y ^ (n - 2) else 0)
                | (c, n) <- zip [1.5, 2.25, 2.625] [1 :: Int ..] ]

-- log(e^(x+3y−0.1) + e^(x−3y−0.1) + e^(−x−0.1))：凸。最小点 (−ln2 / 2, 0)、最小値 (3/2)ln2 − 0.1
lse :: Fn
lse = fn0
  { fnName = "log-sum-exp"
  , fnF = \x y -> fst (pr x y)
  , fnG = \x y -> let (p0, p1, p2) = snd (pr x y) in (p0 + p1 - p2, 3 * p0 - 3 * p1)
  , fnH = \x y -> let (p0, p1, p2) = snd (pr x y)
                      g0 = p0 + p1 - p2
                      g1 = 3 * p0 - 3 * p1
                  in M2 (1 - g0 * g0) (g1 - g0 * g1) (g1 - g0 * g1) (9 * (p0 + p1) - g1 * g1)
  , fnXs = [(-log 2 / 2, 0)], fnFs = 1.5 * log 2 - 0.1, fnView = (-0.4, 0, 2.4), fnStart = (-2, 1.1), fnConvex = True, fnL = 10 }
  where
    pr x y =
      let z0 = x + 3 * y - 0.1
          z1 = x - 3 * y - 0.1
          z2 = -x - 0.1
          m = max z0 (max z1 z2)
          e0 = exp (z0 - m)
          e1 = exp (z1 - m)
          e2 = exp (z2 - m)
          s = e0 + e1 + e2
      in (m + log s, (e0 / s, e1 / s, e2 / s))

-- 最小点までの距離（最小点が複数なら近いほう）
errTo :: Fn -> V2 -> Double
errTo f p = foldl' min (1 / 0) [vlen (vsub p q) | q <- fnXs f]

-- ---- 微分のチェック（中心差分）。H は「解析的な勾配」の差分と比べる
fdGrad :: (Double -> Double -> Double) -> Double -> Double -> Double -> V2
fdGrad fn x y h = ((fn (x + h) y - fn (x - h) y) / (2 * h), (fn x (y + h) - fn x (y - h)) / (2 * h))

derivErr :: Fn -> [V2] -> (Double, Double)
derivErr f pts = foldl' one (0, 0) pts
  where
    h = 1e-6
    rel a b = abs (a - b) / max 1 (abs b)
    one (eg, eh) (x, y) =
      let (g0, g1) = fnG f x y
          (n0, n1) = fdGrad (fnF f) x y h
          M2 ha hb hc hd = fnH f x y
          (ax1, ay1) = fnG f (x + h) y
          (ax0, ay0) = fnG f (x - h) y
          (bx1, by1) = fnG f x (y + h)
          (bx0, by0) = fnG f x (y - h)
          nm = [(ax1 - ax0) / (2 * h), (bx1 - bx0) / (2 * h), (ay1 - ay0) / (2 * h), (by1 - by0) / (2 * h)]
      in (maximum [eg, rel g0 n0, rel g1 n1], maximum (eh : zipWith rel [ha, hb, hc, hd] nm))

-- ∇f = 0 をニュートン法で解く（停留点を探す）。見つからなければ Nothing
criticalPoint :: Fn -> V2 -> Int -> Maybe V2
criticalPoint f p0 it = go 0 p0
  where
    go k p
      | k >= it = done p
      | not (finite (fst d + snd d)) = Nothing
      | vlen d < 1e-14 = done p'
      | otherwise = go (k + 1) p'
      where
        d = solve2 (uncurry (fnH f) p) (uncurry (fnG f) p)
        p' = vsub p d
    done p = if vlen (uncurry (fnG f) p) < 1e-8 then Just p else Nothing

-- アルミホの条件 f(x + t d) ≤ f(x) + c₁ t ∇fᵀd になるまで t を β 倍（試した点も返す）
data Armijo = Armijo { arT :: !Double, arTries :: [(Double, Double)], arSlope :: !Double }

armijo :: Fn -> V2 -> V2 -> Double -> Double -> Double -> Armijo
armijo f p d c1 beta t0 = go (0 :: Int) t0 []
  where
    f0 = uncurry (fnF f) p
    sl = vdot (uncurry (fnG f) p) d
    go k t acc
      | k >= 60 = Armijo t (reverse acc) sl
      | fq <= f0 + c1 * t * sl = Armijo t (reverse acc') sl
      | otherwise = go (k + 1) (t * beta) acc'
      where
        fq = uncurry (fnF f) (vaxpy t d p)
        acc' = (t, fq) : acc

armijo0 :: Fn -> V2 -> V2 -> Armijo
armijo0 f p d = armijo f p d 1e-4 0.5 1

-- ---- 画面と世界の座標（縦横の縮尺は同じ）。画面は p5.js と同じ（左上が原点、y は下向き）
data View = View { vPx, vPy, vPw, vPh, vCx, vCy, vHw, vSc, vHh :: !Double }

makeView :: Double -> Double -> Double -> Double -> (Double, Double, Double) -> View
makeView px py pw ph (cx, cy, hw) = View px py pw' ph' cx cy hw sc (ph' / (2 * sc))
  where
    pw' = fromIntegral (floor (pw + 0.5) :: Int)
    ph' = fromIntegral (floor (ph + 0.5) :: Int)
    sc = pw' / (2 * hw)

toSD :: View -> V2 -> V2
toSD v (x, y) = (vPx v + vPw v / 2 + (x - vCx v) * vSc v, vPy v + vPh v / 2 - (y - vCy v) * vSc v)

toS :: View -> V2 -> Point
toS v p = let (a, b) = toSD v p in (d2f a, d2f b)

toWD :: View -> V2 -> V2
toWD v (sx, sy) = (vCx v + (sx - vPx v - vPw v / 2) / vSc v, vCy v - (sy - vPy v - vPh v / 2) / vSc v)

toW :: View -> Point -> V2
toW v (sx, sy) = toWD v (realToFrac sx, realToFrac sy)

inside :: View -> Point -> Bool
inside v (sx, sy) = let x = realToFrac sx; y = realToFrac sy in x >= vPx v && x <= vPx v + vPw v && y >= vPy v && y <= vPy v + vPh v

-- 等高線の画像：格子で値を計算 → 分位点で段を決め、色ぬり ＋ マーチングスクエアで線（関数を変えたときだけ呼ぶ）
--   返すのは（絵、段の値）。絵は view の中（p5.js の座標）でそのまま使う
contourImage :: View -> (Double -> Double -> Double) -> Int -> (Picture, [Double])
contourImage v fz nLev = contourImageT v fz nLev (\_ _ -> Nothing)

-- tint x y = Just (r, g, b, 透明度 0〜1) なら、その場所の色に重ねる
contourImageT :: View -> (Double -> Double -> Double) -> Int -> (Double -> Double -> Maybe (Double, Double, Double, Double)) -> (Picture, [Double])
contourImageT v fz nLev tint = (Pictures ([image, Color (rgba255 120 200 240 31) (Pictures gridLines)] ++ map contourOf levIx), levs)
  where
    cs = 5 :: Double
    nx = ceiling (vPw v / cs) + 1 :: Int
    ny = ceiling (vPh v / cs) + 1 :: Int
    zs = U.generate (nx * ny) $ \q ->
      let (j, i) = q `divMod` nx
          (x, y) = toWD v (vPx v + fromIntegral i * cs, vPy v + fromIntegral j * cs)
          z = fz x y
      in if finite z then z else 1e300
    zAt i j = zs U.! (j * nx + i)
    srt = U.fromList (sort (filter (< 1e300) (U.toList zs)))
    ns = U.length srt
    levs = if ns == 0 then [] else
      [ srt U.! min (ns - 1) (floor (fromIntegral ns * (fromIntegral k / fromIntegral (nLev + 1)) ** (1.7 :: Double))) | k <- [1 .. nLev] ]
    bands = U.map (\z -> if z >= 1e300 then 255 else length (takeWhile (< z) levs)) zs :: U.Vector Int
    -- 色ぬり（8 万画素以下になるように拡大率 k を選ぶ）
    k = getAt 5 [kk | kk <- [1, 2, 2.5, 4, 5], fromIntegral (iw kk * ih kk) <= (80000 :: Double)] 0
    iw kk = max 1 (floor (vPw v / kk)) :: Int
    ih kk = max 1 (floor (vPh v / kk)) :: Int
    w' = iw k
    h' = ih k
    pix x y =
      let sx = (fromIntegral x + 0.5) * k
          sy = (fromIntegral y + 0.5) * k
          i = min (nx - 1) (floor (sx / cs + 0.5))
          j = min (ny - 1) (floor (sy / cs + 0.5))
          b = bands U.! (j * nx + i)
          t = if b == 255 then 1.25 else fromIntegral b / fromIntegral nLev
          base = (14 + 34 * (1 - t), 40 + 62 * (1 - t), 72 + 70 * (1 - t))
          (cr, cg, cb) = case uncurry tint (toWD v (vPx v + sx, vPy v + sy)) of
            Nothing -> base
            Just (r, g, bb, a) -> let (r0, g0, b0) = base in (r0 * (1 - a) + r * a, g0 * (1 - a) + g * a, b0 * (1 - a) + bb * a)
          q c = fromIntegral (max 0 (min 255 (floor c :: Int)))
      in (q cr, q cg, q cb)
    image = Translate (realToFrac (vPx v + fromIntegral w' * k / 2)) (realToFrac (vPy v + fromIntegral h' * k / 2))
              (Scale 1 (-1) (pixelImage w' h' (realToFrac k) pix))
    -- 方眼（製図用紙）
    gs0 = 10 ** fromIntegral (floor (logBase 10 (vHw v / 2) + 1e-12) :: Int)
    gs1 = if vHw v / gs0 > 10 then gs0 * 2 else gs0
    gs = if vHw v / gs1 > 10 then gs1 * 2.5 else gs1
    gridLines =
      [ Line [(sx, realToFrac (vPy v)), (sx, realToFrac (vPy v + vPh v))]
      | x <- takeWhile (< vCx v + vHw v) [fromIntegral (ceiling ((vCx v - vHw v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sx = fst (toS v (x, 0)) ]
      ++
      [ Line [(realToFrac (vPx v), sy), (realToFrac (vPx v + vPw v), sy)]
      | y <- takeWhile (< vCy v + vHh v) [fromIntegral (ceiling ((vCy v - vHh v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sy = snd (toS v (0, y)) ]
    -- マーチングスクエア：ます目の 4 隅の最小〜最大にかかる段だけ調べ、線分を辺の番号でつないで長い折れ線にする
    hid i j = 2 * (j * nx + i)
    vid i j = 2 * (j * nx + i) + 1
    levIx = zip [0 :: Int ..] levs
    segMap = IM.fromListWith (++) (concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ])
    cellSegs i j =
      let a = zAt i j
          b = zAt (i + 1) j
          c = zAt (i + 1) (j + 1)
          d = zAt i (j + 1)
          lo = min (min a b) (min c d)
          hi = max (max a b) (max c d)
          fi = fromIntegral i :: Double
          fj = fromIntegral j :: Double
          one lv =
            let up z = z > lv
                ps = [ (hid i j, (fi + (lv - a) / (b - a), fj)) | up a /= up b ]
                  ++ [ (vid (i + 1) j, (fi + 1, fj + (lv - b) / (c - b))) | up b /= up c ]
                  ++ [ (hid i (j + 1), (fi + (lv - d) / (c - d), fj + 1)) | up d /= up c ]
                  ++ [ (vid i j, (fi, fj + (lv - a) / (d - a))) | up a /= up d ]
            in pairUp ps
      in [ (li, ss) | (li, lv) <- takeWhile ((< hi) . snd) (dropWhile ((< lo) . snd) levIx), let ss = one lv, not (null ss) ]
    pairUp (p : q : rest) = (p, q) : pairUp rest
    pairUp _ = []
    scr (gx, gy) = (d2f (vPx v + gx * cs), d2f (vPy v + gy * cs)) :: Point
    contourOf (li, _) = Color (rgba255 110 215 250 140) (Pictures [Line (map scr c) | c <- chains (IM.findWithDefault [] li segMap)])

-- 線分（端の番号つき）をつないで折れ線のリストにする
chains :: [((Int, V2), (Int, V2))] -> [[V2]]
chains ss = go starts IS.empty
  where
    adj = IM.fromListWith (++) (concat [[(ka, [kb]), (kb, [ka])] | ((ka, _), (kb, _)) <- ss])
    pos = IM.fromList (concat [[(ka, pa), (kb, pb)] | ((ka, pa), (kb, pb)) <- ss])
    starts = [k | (k, ns) <- IM.toList adj, length ns == 1] ++ IM.keys adj
    nbrs k = IM.findWithDefault [] k adj
    go [] _ = []
    go (s : rest) seen
      | IS.member s seen = go rest seen
      | otherwise = let (path, seen') = walk s s (IS.insert s seen) [s]
                    in map (\k -> IM.findWithDefault (0, 0) k pos) path : go rest seen'
    walk s cur seen acc = case [n | n <- nbrs cur, not (IS.member n seen)] of
      (n : _) -> walk s n (IS.insert n seen) (n : acc)
      [] -> (if length acc > 2 && s `elem` nbrs cur then s : acc else acc, seen)

-- ---- 対数グラフ：0 以下・NaN は描かない
data Series = Series { sV :: [Double], sC :: Color, sName :: String, sDash :: Bool, sWt :: Float }

series :: [Double] -> Color -> String -> Series
series vs c nm = Series vs c nm False 2

data PlotOpt = PlotOpt { poLo, poHi :: Maybe Double, poLogx :: Bool, poTitle :: String, poKmax :: Maybe Int, poXlabel :: String }

plotOpt :: PlotOpt
plotOpt = PlotOpt Nothing Nothing False "" Nothing "iteration k"

logPlot :: Float -> Float -> Float -> Float -> [Series] -> PlotOpt -> Picture
logPlot x y w h ss opt = Pictures $
  [ Color (rgba255 8 26 50 230) (rectTL x y w h), Color cLine (rectLineTL x y w h) ]
  ++ concat [ [ Color (rgb255 40 80 120) (Line [(x, yy), (x + w, yy)]), txtR (x - 4) (yy + 5) 14 cDim ("1e" ++ show e) ]
            | e <- takeWhile (<= hi) [lo, lo + de ..], let yy = yOf (10 ^^ e) ]
  ++ [ txtC (xOf k) (y + h + 12) 14 cDim (show k) | k <- kt ]
  ++ [ txtC (x + w / 2) (y + h + 30) 14 cDim (poXlabel opt) ]
  ++ map curve ss
  ++ concat [ [ txtR (x + w - 34) (yy + 12) 14 (sC s) (sName s)
              , Color (sC s) (if sDash s then dashLine 2 4 3 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)] else fatLine 2 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)]) ]
            | (i, s) <- zip [0 :: Int ..] (filter (not . null . sName) ss), let yy = y + 6 + 17 * fromIntegral i ]
  ++ [ txt x (y - 6) 14 cInk (poTitle opt) | not (null (poTitle opt)) ]
  where
    good v = v > 0 && finite v
    allV = [logBase 10 v | s <- ss, v <- sV s, good v]
    mn = foldl' min (1 / 0) allV
    mx = foldl' max (-1 / 0) allV
    kmax = case poKmax opt of
      Just km | km > 0 -> km
      _ -> maximum (1 : [length (sV s) - 1 | s <- ss])
    lo = case poLo opt of
      Just l -> floor l
      Nothing -> if null allV then -16 else max (-17) (floor mn) :: Int
    hi0 = case poHi opt of
      Just hv -> ceiling hv
      Nothing -> if null allV then 2 else ceiling mx :: Int
    hi = if hi0 <= lo then lo + 1 else hi0
    de = max 1 (ceiling (fromIntegral (hi - lo) / 8 :: Double)) :: Int
    xOf :: Int -> Float
    xOf k = x + w * (if poLogx opt then realToFrac (log (1 + fromIntegral k) / log (1 + fromIntegral kmax) :: Double) else fromIntegral k / fromIntegral kmax)
    yOf :: Double -> Float
    yOf v = y + h - h * d2f ((logBase 10 v - fromIntegral lo) / fromIntegral (hi - lo))
    kt = if poLogx opt then filter (<= kmax) [1, 10, 100, 1000, 10000] else [0, (kmax + 1) `div` 2, kmax]
    curve s = Color (sC s) (Pictures [ case run of
                                         [p] -> at p (disc (sWt s / 2 + 0.5))     -- 1 点だけなら点を打つ
                                         _ -> if sDash s then dashLine (sWt s) 6 5 run else fatLine (sWt s) run
                                     | run <- runs s ])
    runs s = filter (not . null) (splitRuns [ if good v then Just (xOf k, max y (min (y + h) (yOf v))) else Nothing | (k, v) <- zip [0 .. kmax] (sV s) ])
    splitRuns ps = case break (== Nothing) ps of
      (a, []) -> [[p | Just p <- a]]
      (a, _ : rest) -> [p | Just p <- a] : splitRuns rest

-- ---- 描く道具
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss の座標 → p5.js の座標（左上が原点、y は下向き）
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

p5Mouse :: Env -> Point
p5Mouse env = toP5 env (envMouse env)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c s

-- 右端が x、下が y の文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size c s = txt (x - fromIntegral (length s) * size * 0.72) y size c s

-- 左上が (x, y) の長方形（p5.js の rect）：塗り・枠
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = rectLine (x + w / 2) (y + h / 2) w h

-- 太さ w の折れ線（細い線を少しずつずらして重ねる。点が多くても描く要素が少なくてすむ）
fatLine :: Float -> [Point] -> Picture
fatLine w ps
  | w <= 1.5 = Line ps
  | otherwise = Pictures [Translate dx dy (Line ps) | (dx, dy) <- (0, 0) : concat [[(o, 0), (-o, 0), (0, o), (0, -o)] | o <- offs]]
  where offs = takeWhile (<= (w - 1) / 2 + 1e-3) [0.5, 1 ..]

-- 点線（on 描いて off あける）
dashLine :: Float -> Float -> Float -> [Point] -> Picture
dashLine w on off ps = Pictures (map (fatLine w) (go True on (zip ps (drop 1 ps)) []))
  where
    go _ _ [] cur = [reverse cur | length cur >= 2]
    go drawing left ((a, b) : rest) cur
      | l < 1e-6 = go drawing left rest cur
      | left >= l = go drawing (left - l) rest (if drawing then b : start a cur else [])
      | otherwise =
          let m = (fst a + (fst b - fst a) * left / l, snd a + (snd b - snd a) * left / l)
          in if drawing then reverse (m : start a cur) : go False off ((m, b) : rest) []
             else go True on ((m, b) : rest) [m]
      where l = dist2 a b
    start a cur = if null cur then [a] else cur

-- 矢印（色 c、太さ w）
arrowP :: Point -> Point -> Color -> Float -> Picture
arrowP (x1, y1) (x2, y2) c w = Color c (Pictures ([thickLine w (x1, y1) (x2, y2)] ++ [Polygon [(x2, y2), hd (-0.4), hd 0.4] | l > 2]))
  where
    a = atan2 (y2 - y1) (x2 - x1)
    l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    hl = min 12 (l * 0.4)
    hd d = (x2 - hl * cos (a + d), y2 - hl * sin (a + d))

-- 白いふちの丸（半径 r）
dot :: Point -> Float -> Color -> Picture
dot p r c = at p (Pictures [Color white (disc r), Color c (disc (r - 1.2))])

-- 反復点の列を折れ線と点で
drawPath :: View -> [V2] -> Color -> Float -> Bool -> Picture
drawPath _ [] _ _ _ = Blank
drawPath v ps c w dots = Pictures ([Color c (fatLine w ss) | length ss >= 2] ++ ds ++ [dot (last ss) 5.5 c])
  where
    ss = map (toS v) ps
    stepN = max 1 (length ss `div` 60)
    ds = if dots then [Color c (at s (disc 2.5)) | (i, s) <- zip [0 :: Int ..] ss, i `mod` stepN == 0] else []

-- 文字の列（左上が (x, y)、大きさ sz、行の間隔 lh）
hud :: [(String, Color)] -> Float -> Float -> Float -> Float -> Picture
hud ls x y sz lh = Pictures [txt x (y + fromIntegral i * sz * lh + sz) sz c s | (i, (s, c)) <- zip [0 :: Int ..] ls, not (null s)]

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 数の表示（発散・とても小さい数も）：有効数字 d 桁
fmt :: Int -> Double -> String
fmt d v
  | bad v = "diverged"
  | v == 0 = "0"
  | a < 1e-3 || a >= 1e5 = showFFloat (Just (d - 1)) (v / 10 ^^ e) "" ++ "e" ++ show e
  | otherwise =
      let r = fromIntegral (round (v / 10 ^^ (e - d + 1)) :: Integer) * 10 ^^ (e - d + 1) :: Double
      in if e - d + 1 >= 0 then show (round r :: Integer) else stripZ (showFFloat (Just (d - 1 - e)) r "")
  where
    a = abs v
    e = log10i a
    stripZ s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last t == '.' then init t else t else s

-- floor(log10 a)（10 のべきでも正確に）
log10i :: Double -> Int
log10i a = let e0 = floor (logBase 10 a) :: Int in if 10 ^^ (e0 + 1) <= a then e0 + 1 else if 10 ^^ e0 > a then e0 - 1 else e0

-- 決まった種から同じ乱数の列（p5.js 版と同じ mulberry32）
rngList :: Int -> [Double]
rngList seed = go (fromIntegral seed :: Word32)
  where
    go s0 =
      let s = s0 + 0x6d2b79f5
          t1 = (s `xor` (s `shiftR` 15)) * (1 .|. s)
          t2 = (t1 + ((t1 `xor` (t1 `shiftR` 7)) * (61 .|. t1))) `xor` t1
          r = t2 `xor` (t2 `shiftR` 14)
      in fromIntegral r / 4294967296 : go s

-- ---- 自己テスト（起動時に、知られている答えと照合する）：（項目, 合格か, 実測値など）
type Test = (String, Bool, String)

stOk :: String -> Bool -> String -> Test
stOk name ok info = (name, ok, info)

-- 結果を端末に出す（テストモードでは失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Test] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else [r | r@(_, ok, _) <- rs, not ok]
  mapM_ (\(nm, ok, info) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm ++ (if null info then "" else "  [" ++ info ++ "]"))) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [Test] -> String
selfTestLine rs = "Self-test " ++ (if allOk then "OK " else "NG ") ++ show (length [() | (_, True, _) <- rs]) ++ " / " ++ show (length rs)
  where allOk = and [ok | (_, ok, _) <- rs]

drawSelfTest :: Env -> [Test] -> Picture
drawSelfTest env rs = txt 16 (envH env - 12) 14 (if and [ok | (_, ok, _) <- rs] then cGr else cRd) (selfTestLine rs)
