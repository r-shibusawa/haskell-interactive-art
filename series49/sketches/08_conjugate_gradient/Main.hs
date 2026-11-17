-- ============================================
-- 49-08 共役勾配法（CG）
-- Ax = b（A は対称・正定値）を、f = ½xᵀAx − bᵀx の最小化として解く。前の方向と「A について直交」（dᵢᵀAdⱼ = 0）な方向へ進む
--   左：2 次元なら CG はちょうど 2 回で解に着く（最急降下法はジグザグ）
--   右：100×100 の格子のポアソン方程式（未知数 1 万個）。誤差 ‖e‖_A の理論の上限 2((√κ−1)/(√κ+1))^k と比べる
-- 操作：↑↓ = κ（左）/ クリック = 初期点（左）/ 1〜3 = 正解の絵 / ←→ = 1 フレームの反復回数 / スペース = 止める / R = 最初から
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

nG :: Int
nG = 100                     -- ★ 格子の大きさ（未知数は N × N 個）

kMax :: Int
kMax = 1500                  -- ★ 反復の最大回数

kaps :: [Double]
kaps = [2, 5, 10, 30, 100]   -- ★ 左の 2 次関数の κ

type Vec = U.Vector Double

-- CG・最急降下法の途中の状態（x、r = b − Ax、方向 d、rᵀr、回数）
data CGS = CGS { sX, sR, sD :: !Vec, sRR :: !Double, sK :: !Int }

-- 1 回ごとの記録（A ノルムの誤差の比など）
data Hist = Hist { hCg, hSd, hRes, hBcg, hBsd :: !Vec }

data World = World
  { wKi      :: !Int
  , wX0      :: !V2
  , wCgP, wGdP :: ([V2], [V2])     -- 左：（点の列、方向）
  , wView    :: !View
  , wCI      :: Picture
  , wPat     :: !Int
  , wRun     :: !Bool
  , wPerFrame :: !Int              -- ★ 1 フレームあたりの反復回数
  , wTrue, wB :: !Vec
  , wCG, wSD :: !CGS
  , wE0, wR0 :: !Double
  , wHist    :: !Hist
  , wImgTrue :: Picture
  , wTests   :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-08 Conjugate gradient" world0)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      let w = restart (rebuild2d env True world0) { wTests = selfTest }
      U.sum (sX (wCG w)) `seq` wE0 w `seq` pure w
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2 (-2.4, 1.2) ([], []) ([], []) (makeView 16 74 538 448 (0, 0, 1)) Blank 1 True 4
               U.empty U.empty st0 st0 1 1 (Hist one one one one one) Blank []
  where
    st0 = CGS U.empty U.empty U.empty 0 0
    one = U.singleton 1

kapP, qcg, qsd :: Double
kapP = sin (fromIntegral nG * pi / (2 * fromIntegral (nG + 1))) ^ (2 :: Int) / sin (pi / (2 * fromIntegral (nG + 1))) ^ (2 :: Int)
qcg = (sqrt kapP - 1) / (sqrt kapP + 1)
qsd = (kapP - 1) / (kapP + 1)

-- ---- 左：2 次元
quadOf :: Int -> Fn
quadOf ki = quadFn (getAt 10 kaps ki) 0.5

rebuild2d :: Env -> Bool -> World -> World
rebuild2d env newFn w0 = w { wCgP = solvePath av (wX0 w) True, wGdP = solvePath av (wX0 w) False }
  where
    f = quadOf (wKi w0)
    v = makeView 16 74 (realToFrac (envW env) * 0.42) (realToFrac (envH env) * 0.56) (fnView f)
    w = if newFn then w0 { wView = v, wCI = fst (contourImage v (fnF f) 18) } else w0
    av = mat2 (fnH f 0 0)

mat2 :: M2 -> Vec -> Vec
mat2 m x = let (a, b) = mvec m (x U.! 0, x U.! 1) in U.fromList [a, b]

-- b = 0（解は原点）。CG か 最急降下法（正確な直線探索）の点の列と方向
solvePath :: (Vec -> Vec) -> V2 -> Bool -> ([V2], [V2])
solvePath av (px, py) conj = go (0 :: Int) s0 [toV (sX s0)] []
  where
    s0 = cgInit av (U.fromList [0, 0]) (U.fromList [px, py])
    rr0 = sRR s0
    toV x = (x U.! 0, x U.! 1)
    go k s ps ds
      | k < 60 && sRR s > 1e-24 * rr0 =
          let s' = if conj then cgStep av s else sdStep av s
          in go (k + 1) s' (toV (sX s') : ps) (toV (if conj then sD s else sR s) : ds)
      | otherwise = (reverse ps, reverse ds)

-- ---- CG と最急降下法の 1 回分
cgInit :: (Vec -> Vec) -> Vec -> Vec -> CGS
cgInit av b x = let r = U.zipWith (-) b (av x) in CGS x r r (dotU r r) 0

cgStep :: (Vec -> Vec) -> CGS -> CGS
cgStep av (CGS x r d rr k) = CGS x' r' d' rr' (k + 1)
  where
    ad = av d
    a = rr / dotU d ad
    x' = U.zipWith (\xi di -> xi + a * di) x d
    r' = U.zipWith (\ri ai -> ri - a * ai) r ad
    rr' = dotU r' r'
    be = rr' / rr
    d' = U.zipWith (\ri di -> ri + be * di) r' d

sdStep :: (Vec -> Vec) -> CGS -> CGS
sdStep av (CGS x r d rr k) = CGS x' r' d (dotU r' r') (k + 1)
  where
    ar = av r
    a = rr / dotU r ar
    x' = U.zipWith (\xi ri -> xi + a * ri) x r
    r' = U.zipWith (\ri ai -> ri - a * ai) r ar

-- ---- 右：5 点差分のラプラシアン（4 − 上下左右）。行列は作らず、かけ算だけ
lapMul :: Int -> Vec -> Vec
lapMul n v = U.generate (n * n) $ \q ->
  let (j, i) = q `divMod` n
      s0 = 4 * v U.! q
      s1 = if i > 0 then s0 - v U.! (q - 1) else s0
      s2 = if i < n - 1 then s1 - v U.! (q + 1) else s1
      s3 = if j > 0 then s2 - v U.! (q - n) else s2
  in if j < n - 1 then s3 - v U.! (q + n) else s3

pattern :: Int -> Int -> Vec
pattern n which = U.generate (n * n) $ \q ->
  let (j, i) = q `divMod` n
      u = fromIntegral (i + 1) / fromIntegral (n + 1)
      v = fromIntegral (j + 1) / fromIntegral (n + 1)
      s = sin (pi * u) * sin (pi * v)
  in case which of
       1 -> s * exp (-((u - 0.4) ^ (2 :: Int) + (v - 0.55) ^ (2 :: Int)) * 6)
       2 -> s * sin (9 * u * pi) * sin (7 * v * pi)
       _ -> s * (if odd (floor (u * 6) + floor (v * 6) :: Int) then 1 else -0.5)

restart :: World -> World
restart w = w { wTrue = xt, wB = b, wCG = cg, wSD = cg, wE0 = sqrt (errA xt cg), wR0 = sqrt (sRR cg)
              , wHist = Hist one one one one one, wImgTrue = fieldImage xt }
  where
    xt = pattern nG (wPat w)
    b = lapMul nG xt
    cg = cgInit (lapMul nG) b (U.replicate (nG * nG) 0)
    one = U.singleton 1

-- ‖e‖_A² = eᵀAe = −eᵀr
errA :: Vec -> CGS -> Double
errA xt s = max 0 (negate (U.sum (U.zipWith3 (\x t r -> (x - t) * r) (sX s) xt (sR s))))

---------------------------------------------------------------- 動かす

step :: Env -> Float -> World -> IO World
step _ _ w
  | wRun w = pure (go (wPerFrame w) w)
  | otherwise = pure w
  where
    av = lapMul nG
    go t w1
      | t <= 0 || sK (wSD w1) >= kMax = w1
      | otherwise =
          let h = wHist w1
              cg = if at0 (hCg h) (sK (wCG w1)) > 1e-14 then cgStep av (wCG w1) else wCG w1
              sd = sdStep av (wSD w1)
              k = sK sd
              cgV = if at0 (hCg h) (k - 1) > 1e-14 then sqrt (errA (wTrue w1) cg) / wE0 w1 else 0
              resV = if cgV /= 0 then sqrt (sRR cg) / wR0 w1 else 0
              h' = Hist (U.snoc (hCg h) cgV) (U.snoc (hSd h) (sqrt (errA (wTrue w1) sd) / wE0 w1)) (U.snoc (hRes h) resV)
                        (U.snoc (hBcg h) (2 * qcg ^ k)) (U.snoc (hBsd h) (qsd ^ k))
          in go (t - 1) w1 { wCG = cg, wSD = sd, wHist = h' }
    at0 vv i = if i >= 0 && i < U.length vv then vv U.! i else 0

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w, drawPath v (fst (wGdP w)) cCy 2 True, drawPath v (fst (wCgP w)) cOr 3 True
  , hud [ ("Conjugate gradient -- left: " ++ fnName f ++ "   right: Poisson equation " ++ show nG ++ "x" ++ show nG ++ " (" ++ show (nG * nG) ++ " unknowns)", cInk)
        , ("Up/Down: kappa   click: start   1-3: true picture   Left/Right: " ++ show (wPerFrame w) ++ " steps/frame   space: " ++ (if wRun w then "stop" else "run") ++ "   R: restart", cDim) ] 16 14 16 1.4
  , hud [ ("CG (orange): reaches the solution in " ++ show (length (fst (wCgP w)) - 1) ++ " steps", cOr)
        , ("  (always 2 in 2D)", cOr)
        , ("Steepest descent (blue, exact line search):", cCy)
        , ("  " ++ show (length (fst (wGdP w)) - 1) ++ " steps to |r|/|r0| < 1e-12", cCy)
        , ("CG directions: d0^T A d1 / (|d0|_A |d1|_A) = " ++ fmt 3 cosA, cYe)
        , ("  (orthogonal with respect to A)", cYe)
        , ("plain d0.d1 / (|d0| |d1|) = " ++ fmt 3 cosP ++ " (not orthogonal)", cDim)
        , ("In n dimensions exact CG always ends in <= n steps", cInk) ] 16 (d2f (vPy v + vPh v) + 10) 14 1.4 ]
  ++ concat [ [ at (x + fromIntegral i * (cell + 10) + cell / 2, 92 + cell / 2) img, txt (x + fromIntegral i * (cell + 10)) 86 14 cInk t ]
            | (i, img, t) <- zip3 [0 :: Int ..] [sc (wImgTrue w), sc (fieldImage (sX (wCG w))), sc (fieldImage (sX (wSD w)))]
                                  ["true x*", "CG  k = " ++ show (sK (wCG w)), "Steepest  k = " ++ show (sK (wSD w))] ]
  ++ [ hud [ ("kappa = sin^2(N pi/(2N+2)) / sin^2(pi/(2N+2)) = " ++ fmt 5 kapP, cInk)
           , ("sqrt(kappa) = " ++ fmt 4 (sqrt kapP), cInk)
           , ("|e|_A/|e0|_A < 1e-8: CG measured " ++ (if kp < 0 then "..." else show kp ++ " steps") ++ " / bound " ++ show pred' ++ " steps", cYe)
           , ("Steepest descent bound ((k-1)/(k+1))^k: about " ++ fmt 3 (log 1e-8 / log qsd) ++ " steps", cDim) ] x (100 + cell) 14 1.4
     , logPlot (x + 34) top (pw - 40) (envH env - top - 46)
         [ series (U.toList (hCg h)) cOr "CG |e|_A/|e0|_A", (series (U.toList (hBcg h)) cOr "bound 2((sqrt k-1)/(sqrt k+1))^k") { sDash = True }
         , (series (U.toList (hRes h)) cYe "CG residual |r|/|r0| (not bounded)") { sWt = 1 }
         , series (U.toList (hSd h)) cCy "steepest |e|_A/|e0|_A", (series (U.toList (hBsd h)) cCy "bound ((k-1)/(k+1))^k") { sDash = True } ]
         plotOpt { poLo = Just (-14), poHi = Just 4, poKmax = Just (max 50 (sK (wSD w))), poTitle = "Error in the A-norm (log scale)" }
     , drawSelfTest env (wTests w) ]
  where
    f = quadOf (wKi w)
    v = wView w
    h = wHist w
    m = fnH f 0 0
    ds = snd (wCgP w)
    d0 = getAt (0, 0) ds 0
    d1 = getAt (0, 0) ds 1
    ad1 = mvec m d1
    cosA = vdot d0 ad1 / max 1e-300 (sqrt (vdot d0 (mvec m d0) * vdot d1 ad1))
    cosP = vdot d0 d1 / max 1e-300 (vlen d0 * vlen d1)
    x = envW env * 0.46
    pw = envW env - x - 20
    cell = min (pw / 3 - 10) (envH env * 0.26)
    top = 100 + cell + 100
    kp = case U.findIndex (< 1e-8) (hCg h) of { Just i -> i; Nothing -> -1 }
    pred' = ceiling (log (1e-8 / 2) / log qcg) :: Int
    sc = Scale (cell / fromIntegral nG) (cell / fromIntegral nG)

-- 格子の値を色の画像に（100×100、拡大は呼ぶ側で）。view の中で上下が逆にならないよう戻す
fieldImage :: Vec -> Picture
fieldImage xs
  | U.length xs /= nG * nG = Blank
  | otherwise = Scale 1 (-1) (pixelImage nG nG 1 pix)
  where
    pix i j =
      let t = max (-1) (min 1 (xs U.! ((nG - 1 - j) * nG + i) / 0.9))
          q c = fromIntegral (max 0 (min 255 (floor c :: Int)))
      in (q (if t > 0 then 20 + 235 * t else 20), q (40 + 90 * abs t), q (if t < 0 then 70 + 185 * (-t) else 70 + 30 * t))

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | inside (wView w) (toP5 env p) -> rebuild2d env False w { wX0 = toW (wView w) (toP5 env p) }
  EventKey (SpecialKey KeyUp) Down _ _ | wKi w < length kaps - 1 -> rebuild2d env True w { wKi = wKi w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wKi w > 0 -> rebuild2d env True w { wKi = wKi w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPerFrame = min 16 (wPerFrame w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPerFrame = max 1 (wPerFrame w `div` 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> restart w { wPat = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> restart w
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest = denseTests rs0 [2, 5, 10] ++ [t3, t2] ++ concatMap poisson [30, 100]
  where
    rs0 = rngList 11
    -- 乱数で直交行列 Q（グラム・シュミット）
    orth n rs = go n rs []
      where
        go 0 r qs = (reverse qs, r)
        go k r qs =
          let (vs, r') = splitAt n r
              v0 = [2 * x - 1 | x <- vs]
              v = foldl' (\vv q -> zipWith (\a b -> a - dotL vv q * b) vv q) v0 (reverse qs)
              l = sqrt (dotL v v)
          in go (k - 1) r' (map (/ l) v : qs)
    fromEig lams rs = let n = length lams
                          (q, r') = orth n rs
                      in ([[foldl' (\s (l, qm) -> s + (qm !! i) * l * (qm !! j)) 0 (zip lams q) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]], r')
    dense a = \v -> U.fromList [dotL row (U.toList v) | row <- a]
    solveCG a b =
      let av = dense a
          s0 = cgInit av (U.fromList b) (U.replicate (length b) 0)
          r0 = sqrt (sRR s0)
          go s ds | sqrt (sRR s) > 1e-10 * r0 && sK s < 3 * length b = go (cgStep av s) (sD s : ds)
                  | otherwise = (s, reverse ds)
      in (go s0 [], av)
    denseTests _ [] = []
    denseTests rs (n : ns) =
      let (lv, r1) = splitAt n rs
          (a, r2) = fromEig [1 + 9 * x | x <- lv] r1
          (bv, r3) = splitAt n r2
          ((s, ds), av) = solveCG a [x - 0.5 | x <- bv]
          dA x y = dotU x (av y)
          c = foldl' max 0 [abs (dA di dj) / sqrt (dA di di * dA dj dj) | (i, di) <- zip [0 :: Int ..] ds, (j, dj) <- zip [0 ..] ds, j < i]
      in stOk ("n = " ++ show n ++ ": CG solves in " ++ show (sK s) ++ " steps (<= n), directions A-orthogonal") (sK s <= n && c < 1e-8) ("max |di^T A dj| (normalized) " ++ fmt 3 c)
         : denseTests r3 ns
    restAfterDense = foldl' (\r n -> drop (n + n * n + n) r) rs0 [2, 5, 10]
    t3 = let (a3, r1) = fromEig [1, 1, 1, 1, 3, 3, 3, 7, 7, 7] restAfterDense
             ((s, _), _) = solveCG a3 (take 10 r1)
         in stOk "10x10 with only 3 distinct eigenvalues -> CG ends in 3 steps" (sK s == 3) (show (sK s) ++ " steps")
    t2 = let q2 = quadFn 30 0.5
             (ps, _) = solvePath (mat2 (fnH q2 0 0)) (-2, 1) True
             p0 = getAt (0, 0) ps 0
             p2 = getAt (1 / 0, 0) ps 2
             pl = getAt (0, 0) ps (length ps - 1)
         in stOk "2D (kappa = 30): CG reaches the origin in 2 steps (error < 1e-12)" (length ps == 3 && vlen p2 < 1e-12 * vlen p0) ("error " ++ fmt 3 (vlen pl))
    poisson n =
      let nf = fromIntegral n :: Double
          kp = sin (nf * pi / (2 * (nf + 1))) ^ (2 :: Int) / sin (pi / (2 * (nf + 1))) ^ (2 :: Int)
          q = (sqrt kp - 1) / (sqrt kp + 1)
          ev j = U.generate (n * n) (\i -> let (b, a) = i `divMod` n in sin (fromIntegral (j * (a + 1)) * pi / (nf + 1)) * sin (fromIntegral (j * (b + 1)) * pi / (nf + 1)))
          res j = let v = ev j
                      avv = lapMul n v
                      l = 8 * sin (fromIntegral j * pi / (2 * (nf + 1))) ^ (2 :: Int)
                  in (l, U.maximum (U.zipWith (\x y -> abs (x - l * y)) avv v))
          (l1, e1) = res 1
          (ln, en) = res n
          xt = U.generate (n * n) (\i -> sin (fromIntegral i * 0.37) + fromIntegral ((i * 7) `mod` 13) / 13)
          b = lapMul n xt
          av = lapMul n
          s0 = cgInit av b (U.replicate (n * n) 0)
          eA s = sqrt (errA xt s)
          e0 = eA s0
          loop s okb worst
            | sK s >= 2000 = (s, okb, worst)
            | otherwise =
                let s' = cgStep av s
                    r = eA s' / e0
                    bnd = 2 * q ^ sK s'
                in if r < 1e-12 then (s', okb, worst) else loop s' (okb && r <= bnd) (max worst (r / bnd))
          (sF, okB, worstB) = loop s0 True 0
      in [ stOk ("N = " ++ show n ++ ": eigenvectors sin.sin give Av = lambda v, lambda_max/lambda_min = kappa formula")
             (e1 < 1e-12 && en < 1e-12 && abs (ln / l1 / kp - 1) < 1e-12) ("kappa = " ++ fmt 6 kp)
         , stOk ("N = " ++ show n ++ ": CG |e_k|_A/|e0|_A <= 2((sqrt k-1)/(sqrt k+1))^k (every step)") okB
             (show (sK sF) ++ " steps to 1e-12, max measured / bound " ++ fmt 3 worstB) ]

-- ---------- やってみよう ----------
-- 1. ↑↓ で κ を変えても、CG（橙）はいつも 2 回。最急降下法（水色）の回数は κ とともに増える
-- 2. 正解の絵 2（細かいしま）と 1（なめらかな山）で、CG の誤差の減り方を比べよう。上限は同じ
-- 3. CG は ‖e‖_A を減らす方法。残差 ‖r‖（黄）はときどき増えることもある
-- 4. nG を 50 や 200 にすると κ ≈ (2(N+1)/π)² が変わり、必要な回数は約 N に比例する（√κ に比例）
-- 5. 最急降下法は 1500 回でもほとんど進まない。κ ≈ 4000 では (κ−1)/(κ+1) ≈ 0.9995

-- ---------- 道具 ----------
dotU :: Vec -> Vec -> Double
dotU a b = U.sum (U.zipWith (*) a b)

dotL :: [Double] -> [Double] -> Double
dotL xs ys = foldl' (+) 0 (zipWith (*) xs ys)

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
