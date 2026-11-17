-- ============================================
-- 49-05 確率的勾配法（SGD）と適応的手法（RMSProp・Adam）
-- データ N 点の損失の平均 f(w, b) = (1/N) Σ fᵢ(w, b) を、全部の勾配の代わりに B 個だけ選んだ勾配（ミニバッチ）で下る
--   1 回は安いがゆらぐ。学習率 η が一定だと、f − f* はある高さ（雑音の床）で止まる。η を小さくしていけば床も下がる
-- 操作：1 = 線形回帰 / 2 = ロジスティック回帰 / ↑↓ = 学習率の倍率 / ←→ = バッチの大きさ B / N = 新しいデータ / スペース = 自動 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (log1p, showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 120                         -- ★ データの点の数

bss :: [Int]
bss = [1, 4, 16, 64, nPts]         -- ★ バッチの大きさの候補

kMax, perF :: Int
kMax = 4000                        -- ★ 反復の最大回数
perF = 6                           -- ★ 1 フレームに何回

rho, b1, b2, eps :: Double
rho = 0.9                          -- ★ RMSProp の ρ
b1 = 0.9                           -- ★ Adam の β₁
b2 = 0.999                         -- ★ Adam の β₂
eps = 1e-8

-- 5 つの方法の名前と色
methods :: [(String, Color)]
methods = [("GD (full gradient)", cCy), ("SGD eta const", cPk), ("SGD eta/(1+k/200)", cGr), ("RMSProp", cVi), ("Adam", cOr)]

-- データと損失：θ = (w, b)、aᵢ = (xᵢ, 1)、zᵢ = w xᵢ + b
data Model = Model
  { mMd :: !Int, mX, mY :: !(U.Vector Double), mFn :: Fn, mL :: !Double }

-- 1 つの方法の状態（列はすべて新しい順）
data RS = RS
  { rsTh, rsM, rsV :: !V2
  , rsP :: [V2], rsFv, rsGn, rsMa :: [Double], rsN :: !Int
  , rsSw, rsAcc :: !Double, rsNacc :: !Int, rsDead :: !Bool, rsHasMa :: !Bool }

data World = World
  { wWhich, wBi :: !Int
  , wLrm  :: !Double
  , wSeed :: !Int
  , wM    :: Model
  , wV    :: !View
  , wCI   :: Picture
  , wRuns :: [RS]
  , wK    :: !Int
  , wAuto :: !Bool
  , wRnd  :: [Double]          -- 乱数の列（ミニバッチを選ぶ）
  , wFP   :: !Double           -- 雑音の床の予測
  , wKey  :: !(Int, Int)      -- いまの等高線を作ったときの（モデル, 種）
  , wTests :: [Test]
  , wZRows :: [U.Vector Double]  -- 等高線の格子の値（計算できた行、新しい順）
  , wZPend :: !Bool             -- 等高線を計算している途中か
  }

main :: IO ()
main = runSketch (sketch "49-05 SGD, RMSProp and Adam" w00)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env w00 { wTests = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where w00 = World 0 0 1 7 (makeModel 0 7) (makeView 16 74 589 676 (0, 0, 3.2)) Blank [] 0 True [] 0 (-1, -1) [] [] False

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-z))

makeModel :: Int -> Int -> Model
makeModel md sd = Model md xv yv fn { fnXs = [th], fnFs = fnF fn (fst th) (snd th), fnView = (fst th, snd th, 3.2) } lip
  where
    (xl, yl) = unzip (gen nPts (rngList sd))
    gen 0 _ = []
    gen n rs = case (md, rs) of
      (0, r1 : r2 : r3 : rest) -> let x = 2 * r1 - 1
                                      g = sqrt (-2 * log (1 - r2)) * cos (2 * pi * r3)
                                  in (x, 1.5 * x - 0.5 + 0.5 * g) : gen (n - 1 :: Int) rest
      (_, r1 : r2 : rest) | md /= 0 -> let x = 4 * r1 - 2 in (x, if r2 < sigm (2 * x - 0.5) then 1 else 0) : gen (n - 1) rest
      _ -> []
    xv = U.fromList xl
    yv = U.fromList yl
    nn = fromIntegral nPts
    lossI z y = if md == 0 then 0.5 * (z - y) ^ (2 :: Int) else (if z > 0 then z + log1p (exp (-z)) else log1p (exp z)) - y * z
    resI z y = if md == 0 then z - y else sigm z - y
    curvI z = if md == 0 then 1 else sigm z * (1 - sigm z)
    fn = fn0
      { fnName = if md == 0 then "linear" else "logistic"
      , fnF = \w b -> U.sum (U.zipWith (\x y -> lossI (w * x + b) y) xv yv) / nn
      , fnG = \w b -> let (p, q) = U.foldl' (\(a, c) (x, y) -> let r = resI (w * x + b) y in (a + r * x, c + r)) (0, 0) (U.zip xv yv) in (p / nn, q / nn)
      , fnH = \w b -> let (a, c, d) = U.foldl' (\(a0, c0, d0) x -> let s = curvI (w * x + b) in (a0 + s * x * x, c0 + s * x, d0 + s)) (0, 0, 0) xv
                      in M2 (a / nn) (c / nn) (c / nn) (d / nn) }
    -- 最小点：線形は正規方程式 1 回（= ニュートン 1 回）、ロジスティックはニュートン法を ‖∇f‖ < 1e−14 まで
    th = newton (0 :: Int) (0, 0)
    newton it p
      | it >= 50 || vlen (uncurry (fnG fn) p) < 1e-14 = p
      | otherwise = newton (it + 1) (vsub p (solve2 (uncurry (fnH fn) p) (uncurry (fnG fn) p)))
    lip = eL2 (eig2 (if md == 0 then uncurry (fnH fn) th else fnH fn 0 0))   -- ロジスティック：σ' ≤ 1/4 で H ≤ H(0,0)

-- 等高線用の速い損失（値は fnF と同じ式。線形は和をまとめて 1 回で、ロジスティックは 1 本のループで）
fastLoss :: Model -> Double -> Double -> Double
fastLoss mo
  | mMd mo == 0 = \w b -> 0.5 * (w * w * sxx + 2 * w * b * sx + b * b - 2 * w * sxy - 2 * b * sy + syy)
  | otherwise = \w b -> U.foldl' (\acc (x, y) -> let z = w * x + b in acc + (if z > 0 then z + log1p (exp (-z)) else log1p (exp z)) - y * z) 0 xy / nn
  where
    xy = U.zip (mX mo) (mY mo)
    nn = fromIntegral (U.length (mX mo))
    mean f = U.sum (U.map f xy) / nn
    sxx = mean (\(x, _) -> x * x)
    sx = mean fst
    sxy = mean (\(x, y) -> x * y)
    sy = mean snd
    syy = mean (\(_, y) -> y * y)

-- 等高線の格子（等高線の道具と同じ 5 px おきの点）を 1 フレームに数行ずつ計算し、そろったら等高線の絵を作る
--   ロジスティック回帰は 1 点ごとに 120 個の exp・log を使うので、一度に計算すると 1 フレームに収まらない
contourStep :: World -> World
contourStep w
  | not (wZPend w) = w
  | length rows' >= ny = w { wZRows = rows', wZPend = False, wCI = fst (contourImage v look 18) }
  | otherwise = w { wZRows = rows' }
  where
    v = wV w
    mo = wM w
    cs = 5
    nx = ceiling (vPw v / cs) + 1 :: Int
    ny = ceiling (vPh v / cs) + 1 :: Int
    per = if mMd mo == 0 then ny else 16
    done = length (wZRows w)
    newRows = [ U.generate nx (\i -> uncurry (fastLoss mo) (toWD v (vPx v + fromIntegral i * cs, vPy v + fromIntegral j * cs)))
              | j <- [done .. min ny (done + per) - 1] ]
    rows' = foldl' (\acc r -> r `seq` U.sum r `seq` (r : acc)) (wZRows w) newRows
    grid = U.concat (reverse rows')
    look x y = let (sx, sy) = toSD v (x, y)
                   i = max 0 (min (nx - 1) (floor ((sx - vPx v) / cs + 0.5)))
                   j = max 0 (min (ny - 1) (floor ((sy - vPy v) / cs + 0.5)))
               in grid U.! (j * nx + i)

-- 1 つのデータの勾配
gi :: Model -> Int -> V2 -> V2
gi mo i (w, b) = let x = mX mo U.! i
                     y = mY mo U.! i
                     z = w * x + b
                     q = if mMd mo == 0 then z - y else sigm z - y
                 in (q * x, q)

lrs :: Model -> Double -> [Double]
lrs mo lrm = map (* lrm) [1 / mL mo, 0.5 / mL mo, 0.5 / mL mo, 0.01, 0.02]

xsOf :: Model -> V2
xsOf mo = getAt (0, 0) (fnXs (mFn mo)) 0

fOf :: Model -> V2 -> Double
fOf mo = uncurry (fnF (mFn mo))

gOf :: Model -> V2 -> V2
gOf mo = uncurry (fnG (mFn mo))

newRS :: Model -> V2 -> Bool -> RS
newRS mo t0 hasMa = RS t0 (0, 0) (0, 0) [t0] [f0] [vlen (gOf mo t0)] [f0 | hasMa] 1 f0 0 0 False hasMa
  where f0 = fOf mo t0 - fnFs (mFn mo)

rebuild :: Env -> World -> World
rebuild env w = w { wM = mo, wV = v, wCI = ci, wKey = key, wZRows = if same then wZRows w else [], wZPend = if same then wZPend w else True, wRnd = rngList (wSeed w + 99)
                  , wRuns = map (const (newRS mo t0 True)) methods, wK = 0, wFP = floorPred mo (getAt 0 (lrs mo (wLrm w)) 1) (getAt 1 bss (wBi w)) }
  where
    key = (wWhich w, wSeed w)
    mo = makeModel (wWhich w) (wSeed w)
    -- データが同じなら損失の等高線も同じなので作りなおさない（学習率・バッチを変えたとき）
    same = key == wKey w
    ci = if same then wCI w else Blank      -- 新しい等高線は step で数フレームに分けて計算する
    v = makeView 16 74 (realToFrac (envW env) * 0.46) (realToFrac (envH env) - 124) (fnView (mFn mo))
    (a, b) = xsOf mo
    t0 = (a - 2.4, b + 1.8)

-- 復元抽出で B 個選んだ勾配の平均（使った乱数の残りも返す）
batchGrad :: Model -> V2 -> Int -> [Double] -> (V2, [Double])
batchGrad mo th bb = go bb (0, 0)
  where
    go 0 p rs = (vscale (1 / fromIntegral bb) p, rs)
    go j p (r : rs) = go (j - 1) (vadd p (gi mo (floor (r * fromIntegral nPts)) th)) rs
    go _ p [] = (p, [])

-- 1 回すすめる（m = 方法の番号、k = 反復の番号）
iterRS :: Model -> Int -> Int -> Int -> Double -> [Double] -> RS -> (RS, [Double])
iterRS mo m k bb eta rs0 r
  | rsDead r = (r, rs0)
  | bad f || bad (fst th') || bad (snd th') = (r { rsDead = True, rsFv = 0 / 0 : rsFv r, rsN = rsN r + 1, rsM = m', rsV = v' }, rs1)
  | otherwise = (r2, rs1)
  where
    (g, rs1) = if m == 0 then (gOf mo (rsTh r), rs0) else batchGrad mo (rsTh r) bb rs0
    th = rsTh r
    sq (a, b) = (a * a, b * b)
    (th', m', v') = case m of
      2 -> let e = eta / (1 + fromIntegral k / 200) in (vaxpy (-e) g th, rsM r, rsV r)
      3 -> let v2 = vadd (vscale rho (rsV r)) (vscale (1 - rho) (sq g))
           in ((fst th - eta * fst g / (sqrt (fst v2) + eps), snd th - eta * snd g / (sqrt (snd v2) + eps)), rsM r, v2)
      4 -> let m2 = vadd (vscale b1 (rsM r)) (vscale (1 - b1) g)
               v2 = vadd (vscale b2 (rsV r)) (vscale (1 - b2) (sq g))
               c1 = 1 - b1 ^ (k + 1)
               c2 = 1 - b2 ^ (k + 1)
               upd t mj vj = t - (eta * mj / c1) / (sqrt (vj / c2) + eps)
           in ((upd (fst th) (fst m2) (fst v2), upd (snd th) (snd m2) (snd v2)), m2, v2)
      _ -> (vaxpy (-eta) g th, rsM r, rsV r)
    f = fOf mo th' - fnFs (mFn mo)
    n = rsN r + 1
    fvs = f : rsFv r
    sw = rsSw r + f - (if n > 50 then getAt 0 fvs 50 else 0)    -- 50 回の移動平均
    r1 = r { rsTh = th', rsM = m', rsV = v', rsP = th' : rsP r, rsFv = fvs, rsN = n, rsGn = vlen (gOf mo th') : rsGn r }
    r1' = if rsHasMa r then r1 { rsSw = sw, rsMa = sw / fromIntegral (min 50 n) : rsMa r } else r1
    r2 = if k >= kMax `div` 2 then r1' { rsAcc = rsAcc r + f, rsNacc = rsNacc r + 1 } else r1'

-- 雑音の床の予測：誤差 e の 2 次モーメント P が止まる式 AP + PA − η 𝓜(P) = η Σ_B を解き、E[f − f*] = ½ tr(AP)
--   線形回帰・復元抽出：𝓜(P) = (1/B) E[aaᵀPaaᵀ] + (1 − 1/B) APA、Σ_B = (1/B) E[r*² aaᵀ]（この 2 つで厳密）
--   ロジスティック：A = H(θ*)、𝓜(P) = APA、Σ_B = (1/B) E[gᵢgᵢᵀ]（雑音が θ によらないとした近似）
floorPred :: Model -> Double -> Int -> Double
floorPred mo eta bb = 0.5 * (a00 * p0 + 2 * a01 * p1 + a11 * p2)
  where
    aM@(M2 a00 a01 _ a11) = uncurry (fnH (mFn mo)) (xsOf mo)
    nn = fromIntegral nPts
    bf = fromIntegral bb
    mm (M2 a b c d) (M2 e f g h) = M2 (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)
    madd (M2 a b c d) (M2 e f g h) = M2 (a + e) (b + f) (c + g) (d + h)
    msc k (M2 a b c d) = M2 (k * a) (k * b) (k * c) (k * d)
    (s00, s01, s11) = foldl' (\(p, q, r) i -> let (u, w) = gi mo i (xsOf mo) in (p + u * u / nn / bf, q + u * w / nn / bf, r + w * w / nn / bf)) (0, 0, 0) [0 .. nPts - 1]
    mop pm
      | mMd mo == 0 =
          let s = foldl' madd (M2 0 0 0 0) [ msc (1 / nn) (mm (mm am pm) am) | i <- [0 .. nPts - 1], let x = mX mo U.! i, let am = M2 (x * x) x x 1 ]
          in madd (msc (1 / bf) s) (msc (1 - 1 / bf) (mm (mm aM pm) aM))
      | otherwise = mm (mm aM pm) aM
    op pm = let M2 x00 x01 _ x11 = mm aM pm
                M2 y00 y01 _ y11 = mm pm aM
                M2 c00 c01 _ c11 = mop pm
            in [x00 + y00 - eta * c00, x01 + y01 - eta * c01, x11 + y11 - eta * c11]
    cols = [op (M2 1 0 0 0), op (M2 0 1 1 0), op (M2 0 0 0 1)]
    rhs = [eta * s00, eta * s01, eta * s11]
    el c i j = getAt 0 (getAt [] c i) j
    det3 c = el c 0 0 * (el c 1 1 * el c 2 2 - el c 1 2 * el c 2 1) - el c 1 0 * (el c 0 1 * el c 2 2 - el c 0 2 * el c 2 1) + el c 2 0 * (el c 0 1 * el c 1 2 - el c 0 2 * el c 1 1)
    dd = det3 cols
    pj j = det3 [if i == j then rhs else c | (i, c) <- zip [0 :: Int ..] cols] / dd    -- クラメルの公式（cols は列）
    p0 = pj 0
    p1 = pj 1
    p2 = pj 2

---------------------------------------------------------------- 時間

step :: Env -> Float -> World -> IO World
step _ _ w0
  | wAuto w = pure (go perF w)
  | otherwise = pure w
  where
    w = contourStep w0
    go 0 w' = w'
    go s w'
      | wK w' >= kMax = w'
      | otherwise = go (s - 1 :: Int) (oneStep w')
    oneStep w' =
      let et = lrs (wM w') (wLrm w')
          bb = getAt 1 bss (wBi w')
          (rs', rnd') = foldl' (\(acc, rnd) (m, r) -> let (r', rnd2) = iterRS (wM w') m (wK w') bb (getAt 0 et m) rnd r in (acc ++ [r'], rnd2))
                               ([], wRnd w') (zip [0 ..] (wRuns w'))
      in w' { wRuns = rs', wRnd = rnd', wK = wK w' + 1 }

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w ]
  ++ [ drawPathClip v (reverse (take 300 (rsP r))) col 1.5 | (r, (_, col)) <- zip (wRuns w) methods ]
  ++ [ hud [ ((if wWhich w == 0 then "Linear regression y ~ w x + b (squared error)" else "Logistic regression P(y=1) = s(w x + b)") ++ "   loss f(w, b): w across, b up   k = " ++ show (wK w), cInk)
           , ("1 linear  2 logistic  Up/Down rate x" ++ fmt 3 (wLrm w) ++ "  Left/Right batch B = " ++ show bb ++ "  N new data  Space auto " ++ (if wAuto w then "ON" else "off") ++ "  R restart", cDim) ] 16 14 16 1.45
     -- データと、いまの (w, b) の直線・曲線
     , Color (rgba255 8 26 50 230) (rectTL x0 y0 wd h), Color cLine (rectLineTL x0 y0 wd h) ]
  ++ [ Color cInk (at (dx (mX mo U.! i), dy (max ylo (min yhi (mY mo U.! i)))) (disc 2)) | i <- [0 .. nPts - 1] ]
  ++ [ Color col (Line [ (dx x, dy (max ylo (min yhi yv))) | j <- [0 .. 40 :: Int], let x = -xr + 2 * xr * fromIntegral j / 40
                                                           , let z = fst (rsTh r) * x + snd (rsTh r), let yv = if wWhich w == 0 then z else sigm z ])
     | (r, (_, col)) <- zip (wRuns w) methods, not (rsDead r) ]
  ++ [ hud ([ ("SGD eta const = " ++ fmt 3 (getAt 0 et 1) ++ ", B = " ++ show bb ++ " noise floor: predicted E[f - f*] ~ " ++ fmt 3 (wFP w), cPk)
           , ("   measured (mean of 2nd half) " ++ (if rsNacc r1 > 0 then fmt 3 (rsAcc r1 / fromIntegral (rsNacc r1)) else "..."), cPk)
           ] ++ (if wWhich w == 0 then [("Assumes: linear regression, sampling with replacement,", cDim), ("stationary 2nd moment of the error (exact here)", cDim)]
                 else [("Assumes: quadratic model near the minimum,", cDim), ("constant noise (approximate)", cDim)])) x0 (y0 + h + 6) 14 1.3
     , logPlot (x0 + 44) top (wd - 50) ph
         ([ series (reverse (if m == 0 then rsFv r else rsMa r)) col (nm ++ (if rsDead r then " (diverged)" else "")) | (m, r, (nm, col)) <- zip3 [0 :: Int ..] (wRuns w) methods ]
          ++ [ (series (replicate (rsN r0) (wFP w)) cYe "SGD const floor (pred.)") { sDash = True, sWt = 3 } ])
         plotOpt { poKmax = Just kMax, poLo = Just (-12), poTitle = "f(theta_k) - f* (the 4 stochastic ones: moving average of 50)" }
     , logPlot (x0 + 44) (top + ph + 50) (wd - 50) (envH env - top - ph - 100) [ series (reverse (rsGn r)) col "" | (r, (_, col)) <- zip (wRuns w) methods ]
         plotOpt { poKmax = Just kMax, poLo = Just (-8), poTitle = "|grad f(theta_k)|" }
     , drawSelfTest env (wTests w) ]
  where
    v = wV w
    mo = wM w
    bb = getAt 1 bss (wBi w)
    et = lrs mo (wLrm w)
    r0 = getAt (newRS mo (0, 0) True) (wRuns w) 0
    r1 = getAt r0 (wRuns w) 1
    x0 = envW env * 0.5
    y0 = 80
    wd = envW env - x0 - 24
    h = envH env * 0.25
    xr = if wWhich w == 0 then 1.1 else 2.2
    ylo = if wWhich w == 0 then -3.5 else -0.2
    yhi = if wWhich w == 0 then 2.5 else 1.2
    dx x = x0 + d2f ((x + xr) / (2 * xr)) * wd
    dy y = y0 + h - d2f ((y - ylo) / (yhi - ylo)) * h
    top = y0 + h + 100
    ph = (envH env - top - 100) * 0.6

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12" -> rebuild env w { wWhich = fromEnum c - fromEnum '1' }
    | c `elem` "nN" -> rebuild env w { wSeed = wSeed w + 1 }
    | c `elem` "rR" -> rebuild env w
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild env w { wLrm = min 16 (wLrm w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild env w { wLrm = max (1 / 16) (wLrm w / 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env w { wBi = min (length bss - 1) (wBi w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env w { wBi = max 0 (wBi w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  concatMap modelTests [ml, mg]
  ++ [ stOk "logistic: the data cannot be split by one threshold (so f* is finite)" (not (mono o) && not (mono (reverse o))) ""
     , stOk "Adam first step: each component moves exactly eta |g|/(|g|+eps) ~ eta (bias correction)"
         (abs (abs (fst (rsTh ad) - 1) - 0.02) < 1e-6 && abs (abs (snd (rsTh ad) - 1) - 0.02) < 1e-6)
         ("moved " ++ fmt 6 (abs (fst (rsTh ad) - 1)) ++ ", " ++ fmt 6 (abs (snd (rsTh ad) - 1)) ++ " (|g| ~ " ++ fmt 3 (vlen (gOf ml (1, 1))) ++ ")") ]
  ++ [ let fp = floorPred ml eta bb
           (sm, n) = floorRun bb (rngList sd)
       in stOk ("linear, B=" ++ show bb ++ ", eta=0.5/L: noise floor measured (mean of 29000) ~ predicted (+-5%)") (abs (sm / fromIntegral n / fp - 1) < 0.05)
            ("predicted " ++ fmt 4 fp ++ " measured " ++ fmt 4 (sm / fromIntegral n))
     | (bb, sd) <- [(1, 11), (4, 12)] ]
  ++ [ stOk "SGD with decreasing eta gets closer to f* than constant eta (mean of last 1000)" (tl rb < tl ra) (fmt 3 (tl rb) ++ " < " ++ fmt 3 (tl ra))
     , stOk "Full-gradient GD (t = 1/L) does not fluctuate and converges to f*" (getAt 1 (rsFv gd) 0 < 1e-25) (fmt 3 (getAt 1 (rsFv gd) 0)) ]
  where
    ml = makeModel 0 7
    mg = makeModel 1 7
    eta = 0.5 / mL ml
    modelTests mo =
      let (eg, eh) = derivErr (mFn mo) [(0.3, -0.2), (2, 1), (-1, 0.5)]
          nm = if mMd mo == 1 then "logistic" else "linear"
          th = (0.7, -0.4)
          perm = shuffle (rngList 5) [0 .. nPts - 1]
          avg = foldl' vadd (0, 0) [ vscale (1 / (fromIntegral nPts / 10)) (foldl' vadd (0, 0) [vscale (1 / 10) (gi mo i th) | i <- take 10 (drop s perm)])
                                   | s <- [0, 10 .. nPts - 1] ]
          g = gOf mo th
          (sx, sy) = xsOf mo
      in [ stOk (nm ++ ": loss gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-5) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
         , stOk (nm ++ ": |grad f| < 1e-12 at the minimum theta*") (vlen (gOf mo (xsOf mo)) < 1e-12) ("theta* = (" ++ fmt 5 sx ++ ", " ++ fmt 5 sy ++ ")")
         , stOk (nm ++ ": mean of 12 disjoint minibatch gradients = full gradient") (vlen (vsub avg g) < 1e-12) "" ]
    o = map snd (sortOn fst (zip (U.toList (mX mg)) (U.toList (mY mg))))
    mono s = and (zipWith (<=) s (drop 1 s))
    blank th = newRS ml th False
    -- B = N でも復元抽出なので g は全体の勾配と少し違う → 大きさだけ確かめる
    ad = fst (iterRS ml 4 0 nPts 0.02 (rngList 1) (blank (1, 1)) { rsP = [], rsFv = [], rsGn = [], rsN = 0 })
    floorRun bb rs0 = go 0 (xsOf ml) rs0 0 (0 :: Int)
      where
        go kk th rs sm n
          | kk >= (30000 :: Int) = (sm, n)
          | otherwise =
              let (gb, rs') = batchGrad ml th bb rs
                  th' = vaxpy (-eta) gb th
              in if kk >= 1000 then go (kk + 1) th' rs' (sm + fOf ml th' - fnFs (mFn ml)) (n + 1) else go (kk + 1) th' rs' sm n
    runK m n rs r = fst (foldl' (\(rr, rr2) k -> iterRS ml m k 1 eta rr2 rr) (r, rs) [0 .. n - 1])
    ra = runK 1 kMax (rngList 3) (blank (0, 0))
    rb = runK 2 kMax (rngList 3) (blank (0, 0))
    tl r = sum (take 1000 (rsFv r)) / 1000
    gd = fst (foldl' (\(rr, rs) k -> iterRS ml 0 k 1 (1 / mL ml) rs rr) (blank (0, 0), []) [0 .. 299])

-- ---------- やってみよう ----------
-- 1. ←→ で B を 1 → 4 → 16 にすると、SGD η 一定の床が 1/4 ずつ下がる（予測の点線と比べよう）
-- 2. ↑↓ で学習率を 2 倍にすると床も約 2 倍。GD は 2/L を超えると発散する
-- 3. Adam と RMSProp は成分ごとに歩幅をそろえる。等高線の楕円がつぶれていても進み方があまり変わらない
-- 4. 2（ロジスティック）では床の予測は近似。データの点と、確率の曲線（σ）の当てはまりを見よう
-- 5. N で別のデータにしても、床の予測と実測は合う？

-- ---------- 道具 ----------
-- 乱数の列で並べかえる（フィッシャー・イェーツ）
shuffle :: [Double] -> [Int] -> [Int]
shuffle rs xs = U.toList (go (U.fromList xs) (U.length (U.fromList xs) - 1) rs)
  where
    go v i (r : rest) | i > 0 = let j = floor (r * fromIntegral (i + 1)) in go (v U.// [(i, v U.! j), (j, v U.! i)]) (i - 1) rest
    go v _ _ = v

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

-- 枠の外を描かない折れ線（点なし、最後の点に丸）
drawPathClip :: View -> [V2] -> Color -> Float -> Picture
drawPathClip _ [] _ _ = Blank
drawPathClip v ps c w = Pictures (map (Color c . fatLine w) (clipPoly (rectOf v) ss) ++ [dot (last ss) 5.5 c | insideS v (last ss)])
  where ss = map (toS v) ps

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
