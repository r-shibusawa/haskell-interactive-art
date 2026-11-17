-- ============================================
-- 49-12 劣勾配と近接勾配法（LASSO）
-- F(x) = ½‖Ax − b‖² + λ‖x‖₁。|x| は 0 でとがっていて微分できない → 近接勾配法（ISTA）：勾配で 1 歩、そのあと「ソフト閾値」
--   S(v, k) = sign(v)·max(|v| − k, 0) で小さい係数はちょうど 0 になる。FISTA は慣性つきで O(1/k²)、劣勾配法は O(1/√k)
-- 操作：←→ = λ / クリック（左の図）= ISTA の初期点 / 1〜3 = 収束グラフの表示（劣勾配・ISTA・FISTA）
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

mRows, nCols :: Int
mRows = 40                                         -- ★ データの数 m
nCols = 8                                          -- ★ 係数の数 n

truth :: [Double]
truth = [3, -2, 0, 0, 1.5, 0, 0, 0]                -- ★ 本当の係数（ほとんど 0 ＝ スパース）

noiseAmp, corr :: Double
noiseAmp = 0.3                                     -- ★ ノイズの大きさ
corr = 0.7                                         -- ★ 列どうしの相関（大きいほど AᵀA の条件数が大きく、遅くなる）

kIt, nLam :: Int
kIt = 300                                          -- ★ 反復の回数
nLam = 50                                          -- ★ λ の段数

-- 行列（行ごとに並べた数の列）とベクトル
data Mat = Mat { mR, mC :: !Int, mD :: !(U.Vector Double) }
type Vec = U.Vector Double

-- データ：A、b、L = ‖AᵀA‖、λ_max、λ の列、正則化パス
data Data = Data { dA :: Mat, dB :: Vec, dLip, dLamMax :: !Double, dLams :: [Double], dPath :: [Vec], dA2 :: Mat }

-- 収束の記録
data Run = Run { ruP :: [Vec], ruFv :: [Double], ruTs :: [Double], ruG :: !Double }

data World = World
  { wLi    :: !Int            -- λ の番号
  , wSt2   :: !V2             -- 左の図の ISTA の初期点
  , wVis   :: [Bool]
  , wXstar :: Vec
  , wFstar :: !Double
  , wRuns  :: [Run]
  , wLam2  :: !Double
  , wX2s   :: Vec
  , wLip2  :: !Double
  , wCI2   :: Picture
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-12 Subgradient and proximal gradient (LASSO)" (pickLam env1 world0))
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (pickLam env world0) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 18 (-1, 2.5) [True, True, True] U.empty 0 [] 0 U.empty 1 Blank []

---------------------------------------------------------------- 計算

-- 正規分布の乱数（ボックス・ミュラー）。乱数の列を前から 2 つ使う
gauss :: [Double] -> (Double, [Double])
gauss (r1 : r2 : rest) = (sqrt (-2 * log (1 - r1)) * cos (2 * pi * r2), rest)
gauss rs = (0, rs)

dat :: Data
dat = Data a b lip lamMax lams path a2
  where
    (rowsA, rs1) = foldl' mkRow ([], rngList 12) [1 .. mRows]
    mkRow (acc, rs) _ =
      let (h, rs') = gauss rs
          (row, rs'') = foldl' (\(r, q) _ -> let (g, q') = gauss q in (r ++ [(sqrt (1 - corr) * g + sqrt corr * h) / sqrt (fromIntegral mRows)], q')) ([], rs') [1 .. nCols]
      in (acc ++ [row], rs'')
    a = Mat mRows nCols (U.fromList (concat rowsA))
    tr = U.fromList truth
    (bl, _) = foldl' (\(acc, rs) row -> let (g, rs') = gauss rs in (acc ++ [vdotN (U.fromList row) tr + noiseAmp * g], rs')) ([], rs1) rowsA
    b = U.fromList bl
    lip = lipOf a
    lamMax = U.maximum (U.map abs (atv a b))
    lams = [lamMax * 10 ** (-3 * fromIntegral j / fromIntegral (nLam - 1)) | j <- [0 .. nLam - 1]]
    path = drop 1 (scanl (\x l -> cdSolve a b l x) (U.replicate nCols 0) lams)
    a2 = Mat mRows 2 (U.fromList (concat [take 2 r | r <- rowsA]))

mAt :: Mat -> Int -> Int -> Double
mAt m i j = mD m U.! (i * mC m + j)

vdotN :: Vec -> Vec -> Double
vdotN x y = U.sum (U.zipWith (*) x y)

vlenN :: Vec -> Double
vlenN x = sqrt (vdotN x x)

axv :: Mat -> Vec -> Vec
axv m x = U.generate (mR m) (\i -> foldl' (\s j -> s + mAt m i j * (x U.! j)) 0 [0 .. mC m - 1])

atv :: Mat -> Vec -> Vec
atv m r = U.generate (mC m) (\j -> foldl' (\s i -> s + mAt m i j * (r U.! i)) 0 [0 .. mR m - 1])

softT :: Double -> Double -> Double
softT v k = signum v * max (abs v - k) 0

smoothF :: Mat -> Vec -> Vec -> Double
smoothF m b x = let r = U.zipWith (-) (axv m x) b in 0.5 * vdotN r r

smoothG :: Mat -> Vec -> Vec -> Vec
smoothG m b x = atv m (U.zipWith (-) (axv m x) b)

fObj :: Mat -> Vec -> Double -> Vec -> Double
fObj m b lam x = smoothF m b x + lam * U.foldl' (\s v -> s + abs v) 0 x

-- ‖AᵀA‖ をべき乗法で（少しだけ大きめに）
lipOf :: Mat -> Double
lipOf m = go (0 :: Int) (U.replicate (mC m) 1) 0 * (1 + 1e-9)
  where
    go k v l
      | k >= 500 = l
      | otherwise = let w = atv m (axv m v)
                        nw = vlenN w
                    in go (k + 1) (U.map (/ nw) w) (nw / vlenN v)

-- 座標降下法：1 つずつ係数をソフト閾値で正確に最小化（λ を変えた解を高い精度で）
cdSolve :: Mat -> Vec -> Double -> Vec -> Vec
cdSolve m b lam x0 = sweep (0 :: Int) x0 (U.imap (\i bi -> bi - U.foldl' (+) 0 (U.imap (\j xj -> mAt m i j * xj) x0)) b)
  where
    n = mC m
    cn = U.generate n (\j -> foldl' (\s i -> s + mAt m i j * mAt m i j) 0 [0 .. mR m - 1])
    sweep sw x r
      | sw >= 20000 = x
      | otherwise =
          let (x', r', ch) = foldl' one (x, r, 0) [0 .. n - 1]
          in if ch < 1e-15 then x' else sweep (sw + 1) x' r'
    one (x, r, ch) j =
      let c = cn U.! j
          z = x U.! j + foldl' (\s i -> s + mAt m i j * (r U.! i)) 0 [0 .. mR m - 1] / c
          nx = softT z (lam / c)
          d = nx - x U.! j
      in if d /= 0 then (x U.// [(j, nx)], U.imap (\i ri -> ri - mAt m i j * d) r, max ch (abs d)) else (x, r, ch)

-- ISTA（mom = False）と FISTA（mom = True）：x_{k+1} = S(y − ∇f(y)/L, λ/L)
prox :: Mat -> Vec -> Double -> Double -> Vec -> Int -> Bool -> Run
prox m b lam l x0 kk mom = go (0 :: Int) x0 x0 1 [x0] [fObj m b lam x0]
  where
    go k x y t ps fv
      | k >= kk = Run (reverse ps) (reverse fv) [] 0
      | otherwise =
          let g = smoothG m b y
              xn = U.imap (\j v -> softT (v - g U.! j / l) (lam / l)) y
              tn = if mom then (1 + sqrt (1 + 4 * t * t)) / 2 else 1
              y' = if mom then U.imap (\j v -> v + ((t - 1) / tn) * (v - x U.! j)) xn else xn
          in go (k + 1) xn y' tn (xn : ps) (fObj m b lam xn : fv)

-- 劣勾配法：g ∈ ∂F（sign(0) = 0）、ステップ 1/(L√(k+1))。いちばん良い値 F_best を記録
subgrad :: Mat -> Vec -> Double -> Double -> Vec -> Int -> Run
subgrad m b lam l x0 kk = go (0 :: Int) x0 b0 [b0] [] 0
  where
    b0 = fObj m b lam x0
    go k x best fv ts gm
      | k >= kk = Run [] (reverse fv) (reverse ts) gm
      | otherwise =
          let g = U.imap (\j v -> v + lam * signum (x U.! j)) (smoothG m b x)
              t = 1 / (l * sqrt (fromIntegral k + 1))
              x' = U.imap (\j v -> v - t * g U.! j) x
              best' = min best (fObj m b lam x')
          in go (k + 1) x' best' (best' : fv) (t : ts) (max gm (vlenN g))

viewOf :: Env -> View
viewOf env = makeView 16 74 (realToFrac (envW env) * 0.4) (realToFrac (envH env) - 124) (1.2, -0.4, 3.6)

-- λ を選んだときの計算（3 つの方法の収束、左の図の 2 係数の問題と等高線）
pickLam :: Env -> World -> World
pickLam env w = w { wXstar = xs, wFstar = fs, wRuns = runs, wLam2 = lam2, wX2s = x2s, wLip2 = lip2
                  , wCI2 = fst (contourImage (viewOf env) (fObj2 lam2) 18) }
  where
    lam = getAt 0 (dLams dat) (wLi w)
    xs = getAt (U.replicate nCols 0) (dPath dat) (wLi w)
    x0 = U.replicate nCols 0
    a = dA dat
    b = dB dat
    runs = [subgrad a b lam (dLip dat) x0 kIt, prox a b lam (dLip dat) x0 kIt False, prox a b lam (dLip dat) x0 kIt True]
    fs = foldl' min (fObj a b lam xs) (concatMap ruFv (drop 1 runs))
    lm2 = U.maximum (U.map abs (atv (dA2 dat) b))
    lam2 = lam * (lm2 / dLamMax dat)
    x2s = cdSolve (dA2 dat) b lam2 (U.fromList [0, 0])
    lip2 = lip2C

-- 左の図の L（λ によらないので 1 回だけ計算）
lip2C :: Double
lip2C = lipOf (dA2 dat)

-- 2 係数の F(x, y)（等高線用に速く。fObj と同じ式）
fObj2 :: Double -> Double -> Double -> Double
fObj2 lam x y = 0.5 * foldl' (\s i -> let r = mAt a2 i 0 * x + mAt a2 i 1 * y - dB dat U.! i in s + r * r) 0 [0 .. mRows - 1] + lam * (abs x + abs y)
  where a2 = dA2 dat

---------------------------------------------------------------- 描く

palette :: [Color]
palette = [cOr, cCy, cGr, cYe, cPk, cVi, cRd, cInk]

draw :: Env -> World -> Picture
draw env w = view env $
  -- 左の図（2 係数の問題）
  [ wCI2 w
  , Color cDim (Line [(vx, oy), (vx + vw, oy)]), Color cDim (Line [(ox, vy), (ox, vy + vh)])
  , Color cVi (thickPath 2 (map (toS v) [(r1, 0), (0, r1), (-r1, 0), (0, -r1), (r1, 0)]))
  , drawPath v (map (\p -> (p U.! 0, p U.! 1)) (ruP (prox (dA2 dat) (dB dat) (wLam2 w) (wLip2 w) (U.fromList [fst (wSt2 w), snd (wSt2 w)]) 60 False))) cOr 2 True
  -- 図の外をかくす（p5.js の clip の代わり）
  , Color cBg (rectTL 0 0 (envW env) vy), Color cBg (rectTL 0 (vy + vh) (envW env) (envH env - vy - vh))
  , Color cBg (rectTL 0 0 vx (envH env)), Color cBg (rectTL (vx + vw) 0 (envW env - vx - vw) (envH env))
  , hud [ ("Subgradient and proximal gradient (LASSO) -- lambda = " ++ fmt 4 lam ++ "  (lambda_max = |A^T b|_inf = " ++ fmt 4 (dLamMax dat) ++ ")", cInk)
        , ("Left/Right: lambda   click (left): ISTA start   1-3: show / hide curves", cDim) ] 16 14 16 1.45
  , hud [ ("Two-coefficient problem: contours of F(x)", cInk), ("(kinked on the axes)", cInk)
        , ("Violet: diamond |x|_1 = |x*|_1", cDim)
        , ("ISTA (orange) ends at x* = (" ++ fmt 3 (x2 0) ++ ", " ++ fmt 3 (x2 1) ++ ")", cDim) ] (vx + 8) (vy + 6) 14 1.45
  -- 正則化パス
  , Color (rgba255 8 26 50 230) (rectTL px py pw ph), Color cLine (rectLineTL px py pw ph)
  , Color cLine (Line [(px, yP 0), (px + pw, yP 0)]) ]
  ++ [ Color c (fatLine 2 [(xP j, yP (p U.! ci)) | (j, p) <- zip [0 :: Int ..] (dPath dat)]) | (ci, c) <- zip [0 .. nCols - 1] palette ]
  ++ [ Color cInk (Line [(xP (wLi w), py), (xP (wLi w), py + ph)])
     , txt px (py - 6) 14 cInk "Regularization path x_j(lambda): left lambda_max -> right lambda_max/1000 (log lambda)" ]
  ++ [ txt (px + 8 + fromIntegral (ci `mod` 4) * (pw / 4)) (py + ph + 20 + fromIntegral (ci `div` 4) * 18) 14 c
           ("x" ++ show (ci + 1) ++ " = " ++ fmt 3 (wXstar w U.! ci) ++ " (true " ++ fmt 3 (getAt 0 truth ci) ++ ")")
     | (ci, c) <- zip [0 .. min nCols (U.length (wXstar w)) - 1] palette ]
  ++ [ txt (px + 8) (py + ph + 56) 14 cDim ("Coefficients exactly 0: " ++ show (U.length (U.filter (== 0) (wXstar w))) ++ " / " ++ show nCols)
  -- 収束のグラフ
     , logPlot (px + 34) gy (pw - 34) (envH env - gy - 46) shown
         plotOpt { poLo = Just (-13), poHi = Just 3, poKmax = Just kIt, poLogx = True
                 , poTitle = "F(x_k) - F*  (log-log, x0 = 0, R = |x0 - x*|, L = |A^T A| = " ++ fmt 4 (dLip dat) ++ ")" }
     , drawSelfTest env (wTests w) ]
  where
    v = viewOf env
    vx = d2f (vPx v)
    vy = d2f (vPy v)
    vw = d2f (vPw v)
    vh = d2f (vPh v)
    (ox, oy) = toS v (0, 0)
    x2 i = if U.length (wX2s w) > i then wX2s w U.! i else 0
    r1 = abs (x2 0) + abs (x2 1)
    lam = getAt 0 (dLams dat) (wLi w)
    px = envW env * 0.45
    pw = envW env - px - 30
    py = 92
    ph = envH env * 0.36
    lastP = getAt (U.replicate nCols 0) (dPath dat) (nLam - 1)
    cmax = U.maximum (U.map abs lastP) * 1.1
    xP j = px + pw * fromIntegral j / fromIntegral (nLam - 1)
    yP val = py + ph / 2 - d2f (val / cmax) * (ph / 2 - 6)
    gy = py + ph + 92
    rr = vlenN (wXstar w)
    runs = wRuns w
    fstar = wFstar w
    fvOf i = map (subtract fstar) (ruFv (getAt (Run [] [] [] 0) runs i))
    ks = [0 .. kIt]
    lip = dLip dat
    se = [ series (fvOf 0) cVi "subgradient (F_best)", series (fvOf 1) cOr "ISTA", series (fvOf 2) cCy "FISTA" ]
    bd = [ (series [if k > 0 then lip * rr * rr / (2 * fromIntegral k) else 0 / 0 | k <- ks] cOr "ISTA bound LR^2/(2k)") { sDash = True }
         , (series [if k > 0 then 2 * lip * rr * rr / fromIntegral ((k + 1) * (k + 1)) else 0 / 0 | k <- ks] cCy "FISTA bound 2LR^2/(k+1)^2") { sDash = True } ]
    vis = wVis w
    shown = [s | (s, on) <- zip se vis, on] ++ [s | (s, on) <- zip bd (drop 1 vis), on]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p | inside (viewOf env) (toP5 env p) -> w { wSt2 = toW (viewOf env) (toP5 env p) }
  EventKey (SpecialKey KeyRight) Down _ _ | wLi w < nLam - 1 -> pickLam env w { wLi = wLi w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wLi w > 0 -> pickLam env w { wLi = wLi w - 1 }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' ->
    let k = fromEnum c - fromEnum '1' in w { wVis = [if i == k then not o else o | (i, o) <- zip [0 ..] (wVis w)] }
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "Soft threshold S(3,1) = 2, S(-0.5,1) = 0, S(-3,1) = -2" (softT 3 1 == 2 && softT (-0.5) 1 == 0 && softT (-3) 1 == -2) ""
  , stOk "Smooth part 1/2|Ax-b|^2: gradient A^T(Ax-b) = finite differences" (eG < 1e-7) (fmt 3 eG)
  , stOk "If lambda >= |A^T b|_inf the solution is exactly x = 0"
      (U.all (== 0) z && U.all (== 0) z1 && U.all (== 0) (getAt (U.replicate 1 1) (dPath dat) 0)) ""
  , stOk "KKT: x_j = 0 -> |A_j^T(b-Ax)| <= lambda, x_j != 0 -> = lambda sign(x_j) (3 lambdas)" (kktMax < 1e-8 && nz > 0)
      ("residual " ++ fmt 3 kktMax ++ ", exact zeros " ++ show nz)
  , stOk "If A^T A = I the solution is S_lambda(A^T b) (coordinate descent and FISTA)"
      (U.maximum (U.map abs (U.zipWith (-) want got)) < 1e-12 && U.maximum (U.map abs (U.zipWith (-) want gf)) < 1e-9) ""
  , stOk "F* is below every ISTA / FISTA value (F* accurate enough)" (foldl' min (1 / 0) (ruFv ri ++ ruFv rf) >= fs - 1e-13) ("F* = " ++ showFFloat (Just 14) fs "")
  , stOk "ISTA: F(x_k) - F* <= L|x0-x*|^2/(2k) (every k)"
      (and [k == 0 || val - fs <= lip * r * r / (2 * fromIntegral k) + 1e-12 | (k, val) <- zip [0 :: Int ..] (ruFv ri)])
      ("k=100: " ++ fmt 3 (getAt 0 (ruFv ri) 100 - fs) ++ " <= " ++ fmt 3 (lip * r * r / 200))
  , stOk "FISTA: F(x_k) - F* <= 2L|x0-x*|^2/(k+1)^2 (every k)"
      (and [k == 0 || val - fs <= 2 * lip * r * r / fromIntegral ((k + 1) * (k + 1)) + 1e-12 | (k, val) <- zip [0 :: Int ..] (ruFv rf)])
      ("k=30: " ++ fmt 3 (getAt 0 (ruFv rf) 30 - fs) ++ " <= " ++ fmt 3 (2 * lip * r * r / 961))
  , stOk "Subgradient: F_best - F* <= (R^2 + G^2 sum t^2)/(2 sum t) (every k)" okS ("k=" ++ show kIt ++ ": " ++ fmt 3 (getAt 0 (ruFv rs) kIt - fs)) ]
  where
    a = dA dat
    b = dB dat
    lip = dLip dat
    x = U.fromList [0.3, -1, 2, 0.1, 0, 0.5, -0.2, 1]
    g = smoothG a b x
    h = 1e-6
    eG = foldl' max 0 [ abs ((smoothF a b (x U.// [(j, x U.! j + h)]) - smoothF a b (x U.// [(j, x U.! j - h)])) / (2 * h) - g U.! j) | j <- [0 .. nCols - 1] ]
    zeros = U.replicate nCols 0
    z = cdSolve a b (dLamMax dat * 1.0001) (U.fromList truth)
    z1 = getAt (U.replicate 1 1) (ruP (prox a b (dLamMax dat * 1.0001) lip zeros 5 False)) 5
    kk = [ (val, ck, lj) | j <- [10, 25, 40], let xs = getAt zeros (dPath dat) j, let lj = getAt 0 (dLams dat) j
                         , let c = atv a (U.zipWith (-) b (axv a xs)), (val, ck) <- zip (U.toList xs) (U.toList c) ]
    kktMax = foldl' max 0 [ if val == 0 then max 0 (abs ck - lj) else abs (ck - lj * signum val) | (val, ck, lj) <- kk ]
    nz = length [() | (val, _, _) <- kk, val == 0]
    -- 正規直交な A（グラム・シュミット）なら解 = S_λ(Aᵀb)
    colsA = [U.generate mRows (\i -> mAt a i j) | j <- [0 .. nCols - 1]]
    qCols = foldl' (\done cj -> let v' = foldl' (\vv qk -> let d = vdotN vv qk in U.zipWith (\p q -> p - d * q) vv qk) cj done
                                    nn = sqrt (vdotN v' v')
                                in done ++ [U.map (/ nn) v']) [] colsA
    qMat = Mat mRows nCols (U.generate (mRows * nCols) (\q -> let (i, j) = q `divMod` nCols in getAt U.empty qCols j U.! i))
    lq = 0.2
    want = U.map (`softT` lq) (atv qMat b)
    got = cdSolve qMat b lq zeros
    gf = getAt zeros (ruP (prox qMat b lq (lipOf qMat) zeros 300 True)) 300
    li = 18
    lam = getAt 0 (dLams dat) li
    xs0 = getAt zeros (dPath dat) li
    fs = fObj a b lam xs0
    r = vlenN xs0
    ri = prox a b lam lip zeros kIt False
    rf = prox a b lam lip zeros kIt True
    rs = subgrad a b lam lip zeros kIt
    okS = and (snd (foldl' (\(( st, st2), acc) (k, t) ->
                               let st' = st + t
                                   st2' = st2 + t * t
                               in ((st', st2'), (getAt 0 (ruFv rs) (k + 1) - fs <= (r * r + ruG rs * ruG rs * st2') / (2 * st') + 1e-12) : acc))
                            ((0, 0), []) (zip [0 :: Int ..] (ruTs rs))))

-- ---------- やってみよう ----------
-- 1. ←→ で λ を大きくすると、正則化パスの係数が 1 つずつちょうど 0 になる（λ_max より大きいと全部 0）
-- 2. 左の図：λ を大きくすると、ISTA の行き先が軸の上（片方がちょうど 0）に乗る。ひし形の角に当たるから
-- 3. FISTA のグラフは O(1/k²) の上限の下。ISTA は O(1/k)、劣勾配法はずっと遅い（O(1/√k)）
-- 4. truth を変えて、本当に 0 の係数がパスでも 0 になるか確かめよう。noiseAmp を大きくすると？
-- 5. 劣勾配法のステップ 1/(L√(k+1)) を一定にすると、F_best はある高さで止まる

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
