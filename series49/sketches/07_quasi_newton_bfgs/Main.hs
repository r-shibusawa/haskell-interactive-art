-- ============================================
-- 49-07 準ニュートン法 BFGS
-- ヘッセ行列を計算しないで、勾配の変化 y = ∇f(x₊) − ∇f(x) と移動 s = x₊ − x から「近似のヘッセ行列」B を育てる
--   B₊ s = y（セカント条件）を満たすように毎回すこしだけ直す。右の楕円：B（橙）と本物の ∇²f（水色）
--   同じ関数で 最急降下法（アルミホ）・減衰ニュートン法・BFGS を競走させる
-- 操作：←→ = 関数 / クリック = 初期点 / スペース = 自動 / R = 最初から / E = 最後まで
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

kMax :: Int
kMax = 3000                  -- ★ 反復の最大回数

gTol :: Double
gTol = 1e-10                 -- ★ ‖∇f‖ がこれより小さくなったら止める

c1A, betaA :: Double
c1A = 1e-4                   -- ★ アルミホの定数（3 つの方法で共通）
betaA = 0.5

starts :: [V2]
starts = [(-1.2, 1), (2, 2), (-0.3, -0.9), (-2.6, 1.6)]   -- ★ 関数ごとの初期点

fns :: [Fn]
fns = [rosen, beale, himmel, quadFn 20 0.5]

-- 3 つの方法の名前と色
meths :: [(String, Color)]
meths = [("Steepest descent", cCy), ("Damped Newton", cGr), ("BFGS", cOr)]

-- 1 つの方法の結果：点の列、f − f*、‖∇f‖、逆ヘッセ近似 H_k、f・勾配・ヘッセ行列の計算回数
data Run = Run
  { rP  :: U.Vector (Double, Double)
  , rFv :: U.Vector Double
  , rGn :: U.Vector Double
  , rHs :: U.Vector (Double, Double, Double, Double)
  , rNf, rNg, rNh :: !Int
  }

data World = World
  { wFi    :: !Int
  , wX0    :: !V2
  , wRuns  :: [Run]
  , wShown :: !Int
  , wAuto  :: !Bool
  , wView  :: !View
  , wCI    :: Picture
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-07 Quasi-Newton BFGS" (rebuild env0' True world0))
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      let w = rebuild env True world0
      mapM_ (\r -> U.length (rP r) `seq` pure ()) (wRuns w)
      pure w { wTests = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where env0' = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 (-1.2, 1) [] 0 True (makeView 16 74 640 676 (0, 0, 1)) Blank []

rebuild :: Env -> Bool -> World -> World
rebuild env newFn w0 = w { wRuns = [runMethod f (wX0 w) m | m <- [0 .. 2]], wShown = 0 }
  where
    f = getAt rosen fns (wFi w0)
    v = makeView 16 74 (realToFrac (envW env) * 0.5) (realToFrac (envH env) - 124) (fnView f)
    w = if newFn then w0 { wView = v, wCI = fst (contourImage v (fnF f) 18), wX0 = getAt (0, 0) starts (wFi w0) } else w0

-- 逆行列の形の BFGS 更新：H₊ = (I − ρsyᵀ) H (I − ρysᵀ) + ρssᵀ、ρ = 1/(yᵀs)（n 次元）
bfgsInv :: [[Double]] -> [Double] -> [Double] -> [[Double]]
bfgsInv h s y = [ [ hij - r * (hyi * sj + si * hyj) + (r * r * yHy + r) * si * sj | (hij, sj, hyj) <- zip3 row s hy ] | (row, si, hyi) <- zip3 h s hy ]
  where
    r = 1 / dotL y s
    hy = map (`dotL` y) h
    yHy = dotL y hy

inv2 :: M2 -> M2
inv2 (M2 a b c d) = let dt = a * d - b * c in M2 (d / dt) (-b / dt) (-c / dt) (a / dt)

-- m = 0 最急降下、1 減衰ニュートン（∇²f が正定値でなければ対角をずらす）、2 BFGS。どれもアルミホで t を決める
runMethod :: Fn -> V2 -> Int -> Run
runMethod f p0 m = go 0 p0 (uncurry (fnG f) p0) (M2 1 0 0 1) [p0] [uncurry (fnF f) p0 - fnFs f] [vlen (uncurry (fnG f) p0)] [M2 1 0 0 1] 1 1 0
  where
    go :: Int -> V2 -> V2 -> M2 -> [V2] -> [Double] -> [Double] -> [M2] -> Int -> Int -> Int -> Run
    go k p g hi ps fv gn hs nf ng nh
      | k >= kMax || not (headD gn > gTol) = done ps fv gn hs nf ng nh
      | bad fq || bad (fst q) || bad (snd q) = done ps ((0 / 0) : fv) gn hs nf' ng' nh'
      | otherwise = go (k + 1) q g2 hi' (q : ps) (max fq 0 : fv) (vlen g2 : gn) (hi' : hs) nf' ng' nh'
      where
        (d, nh') = case m of
          0 -> (vscale (-1) g, nh)
          1 -> let hm@(M2 a b c dd) = uncurry (fnH f) p
                   e = eig2 hm
                   sh = if eL1 e > 1e-8 * max 1 (abs (eL2 e)) then 0 else -(eL1 e) + 1e-3 * (1 + abs (eL2 e))
               in (vscale (-1) (solve2 (M2 (a + sh) b c (dd + sh)) g), nh + 1)
          _ -> (vscale (-1) (mvec hi g), nh)
        ar = armijo f p d c1A betaA 1
        nf' = nf + length (arTries ar)
        q = vaxpy (arT ar) d p
        g2 = uncurry (fnG f) q
        ng' = ng + 1
        s = vsub q p
        yv = vsub g2 g
        hi' | m == 2 && vdot s yv > 1e-12 * vlen s * vlen yv = fromL (bfgsInv (toL hi) [fst s, snd s] [fst yv, snd yv])   -- sᵀy ≤ 0 なら更新しない
            | otherwise = hi
        fq = uncurry (fnF f) q - fnFs f
    done ps fv gn hs nf ng nh = Run (U.fromList (reverse ps)) (U.fromList (reverse fv)) (U.fromList (reverse gn))
                                    (U.fromList [(a, b, c, d) | M2 a b c d <- reverse hs]) nf ng nh
    headD xs = case xs of { (x : _) -> x; [] -> 0 }
    toL (M2 a b c d) = [[a, b], [c, d]]
    fromL [[a, b], [c, d]] = M2 a b c d
    fromL _ = M2 1 0 0 1

-- 楕円 { d : dᵀ M d = 1 } の点（M が正定値のときだけ）
ellipsePts :: M2 -> Maybe [V2]
ellipsePts mm
  | not (eL1 e > 0) = Nothing
  | otherwise = Just [ vadd (vscale c (eV1 e)) (vscale s (eV2 e))
                     | i <- [0 .. 64 :: Int], let a = fromIntegral i / 64 * 2 * pi, let c = cos a / sqrt (eL1 e), let s = sin a / sqrt (eL2 e) ]
  where e = eig2 mm

---------------------------------------------------------------- 動かす

step :: Env -> Float -> World -> IO World
step env _ w = pure $
  if wAuto w && envFrame env `mod` 3 == 0 && wShown w < lenMax w
    then w { wShown = wShown w + max 1 (floor (fromIntegral (wShown w) * 0.04 :: Double)) } else w

lenMax :: World -> Int
lenMax w = foldl' max 0 [U.length (rP r) | r <- wRuns w]

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w ]
  ++ [ drawPath v (U.toList (U.take (shown + 1) (rP r))) c 2 True | (r, (_, c)) <- zip (wRuns w) meths ]
  ++ [ Color cGr (at (toS v q) (ring 7 2)) | q <- fnXs f ]
  ++ [ hud [ ("Quasi-Newton BFGS -- " ++ fnName f ++ "  (f* = " ++ fmt 3 (fnFs f) ++ ")", cInk)
           , ("Left/Right: function   click: start   space: auto " ++ (if wAuto w then "ON" else "off") ++ "   R: restart   E: to the end", cDim)
           , ("All use Armijo c1 = " ++ fmt 3 c1A ++ ", beta = " ++ fmt 3 betaA ++ ", t0 = 1", cDim) ] 16 14 16 1.4
     , hud [ ("Steps until |grad f| < " ++ fmt 3 gTol, cInk), ("(f evals / gradients / Hessians)", cDim) ] x y 15 1.3 ]
  ++ [ hud [ (nm ++ ": " ++ cnt r ++ " (" ++ show (rNf r) ++ "/" ++ show (rNg r) ++ "/" ++ show (rNh r) ++ ") -> (" ++ fmt 4 (fst lst) ++ ", " ++ fmt 4 (snd lst) ++ ")", c) ] x (y + 44 + fromIntegral i * 22) 15 1.4
     | (i, r, (nm, c)) <- zip3 [0 :: Int ..] (wRuns w) meths, let lst = lastP r ]
  ++ ellipseBox
  ++ [ logPlot (x + 34) top (pw - 40) ph [series (sl rFv r) c nm | (r, (nm, c)) <- zip (wRuns w) meths]
         plotOpt { poLo = Just (-20), poKmax = Just (max 10 (lenMax w - 1)), poLogx = True, poTitle = "f(x_k) - f*  (both axes log)" }
     , logPlot (x + 34) (top + ph + 56) (pw - 40) ph [series (sl rGn r) c nm | (r, (nm, c)) <- zip (wRuns w) meths]
         plotOpt { poLo = Just (-12), poKmax = Just (max 10 (lenMax w - 1)), poLogx = True, poTitle = "|grad f(x_k)|" }
     , drawSelfTest env (wTests w) ]
  where
    f = getAt rosen fns (wFi w)
    v = wView w
    shown = wShown w
    x = envW env * 0.53
    y = 84
    pw = envW env - x - 24
    top = y + 312
    ph = (envH env - top - 40 - 56) / 2
    sl fld r = U.toList (U.take (shown + 1) (fld r))
    lastP r = if U.null (rP r) then (0, 0) else U.last (rP r)
    cnt r = let n = U.length (rP r) - 1
                ok = not (U.null (rGn r)) && U.last (rGn r) <= gTol
                fvLast = if U.null (rFv r) then 0 else U.last (rFv r)
            in if finite fvLast then (if ok then show n else "> " ++ show n) else "diverged"
    -- BFGS の近似ヘッセ行列 B = H⁻¹ と本物の ∇²f を楕円で
    ellipseBox = case drop 2 (wRuns w) of
      (b : _) | not (U.null (rP b)) ->
        let j = min shown (U.length (rP b) - 1)
            (ha, hb, hc, hd) = rHs b U.! j
            bm@(M2 b0 b1 b2 b3) = inv2 (M2 ha hb hc hd)
            pj = rP b U.! j
            tm@(M2 t0 t1 t2 t3) = uncurry (fnH f) pj
            eB = ellipsePts bm
            eT = ellipsePts tm
            cx = x + 90
            cy = y + 199
            r = foldl' max 0 [vlen p | Just ps <- [eB, eT], p <- ps]
            sc = if r > 0 && finite r then 80 / r else 1
            dif = hypot (hypot (b0 - t0) (b1 - t1)) (hypot (b2 - t2) (b3 - t3)) / max 1e-300 (hypot (hypot t0 t1) (hypot t2 t3))
            ratio = if j >= 2 && rFv b U.! (j - 1) > 0 then errTo f pj / errTo f (rP b U.! (j - 1)) else 0 / 0
            curve ps c = Color c (fatLine 2 [(cx + d2f (px * sc), cy - d2f (py * sc)) | (px, py) <- ps])
        in [ Color cLine (rectLineTL x (y + 119) 180 160) ]
           ++ [ curve ps cCy | Just ps <- [eT] ] ++ [ curve ps cOr | Just ps <- [eB] ]
           ++ [ hud [ ("BFGS iteration k = " ++ show j, cOr), ("Ellipses {d : d^T M d = 1}, same scale", cDim)
                    , ("orange = B_k (approx.)", cInk), ("blue = Hessian at x_k", cInk)
                    , (case eT of { Nothing -> "Hessian not pos. def.: no blue ellipse"; Just _ -> "" }, cPk)
                    , ("|B_k - Hess| / |Hess| = " ++ fmt 3 dif, cYe)
                    , ("error ratio e_k / e_(k-1) = " ++ (if isNaN ratio then "-" else fmt 3 ratio), cYe)
                    , ("(-> 0 if superlinear)", cYe)
                    , ("B_k need not approach the Hessian", cDim), ("(only along the steps s_k)", cDim) ] (x + 196) (y + 122) 14 1.25 ]
      _ -> []

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | inside (wView w) (toP5 env p) -> rebuild env False w { wX0 = toW (wView w) (toP5 env p) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild env True w { wFi = (wFi w + 1) `mod` length fns }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild env True w { wFi = (wFi w + length fns - 1) `mod` length fns }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> w { wShown = 0 }
    | c `elem` "eE" -> w { wShown = lenMax w }
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ let (eg, eh) = derivErr g [(-1.2, 1), (0.5, -0.3), (2, 1.5)]
    in stOk (fnName g ++ ": gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-6) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
  | g <- [rosen, beale, himmel] ]
  ++ [ stOk "Secant condition B+ s = y (exact)" (vlen secErr < 1e-12 * vlen (1.3, -0.5)) ("error " ++ fmt 3 (vlen secErr)) ]
  ++ quadTests (drop 0 (rngList 7)) [2, 5, 8]
  ++ [ stOk "Rosenbrock (-1.2, 1): BFGS reaches (1, 1) (error < 1e-8)" (errTo rosen lastB < 1e-8) (show nb ++ " steps, error " ++ fmt 3 (errTo rosen lastB))
     , stOk "Rosenbrock: damped Newton also reaches (1, 1); BFGS needs > 10x fewer steps than steepest descent"
         (errTo rosen (lastOf (rsAt 1)) < 1e-8 && 10 * nb < U.length (rP (rsAt 0)) - 1)
         ("GD > " ++ show (U.length (rP (rsAt 0)) - 1) ++ " / Newton " ++ show (U.length (rP (rsAt 1)) - 1) ++ " / BFGS " ++ show nb)
     , stOk "BFGS: in the last few steps e_k/e_(k-1) < 0.1 (sign of superlinear convergence)" (minJS rat < 0.1) ("ratios " ++ unwords (map (fmt 2) rat))
     , stOk "BFGS also reaches a minimum on Himmelblau and Beale"
         (and [ errTo g (lastOf (runMethod g (getAt (0, 0) starts i) 2)) < 1e-7 | (g, i) <- [(himmel, 2), (beale, 1)] ]) "" ]
  where
    h1 = bfgsInv [[2, 0.3], [0.3, 0.7]] [0.4, -1.1] [1.3, -0.5]
    b1 = case h1 of { [[a, b], [c, d]] -> inv2 (M2 a b c d); _ -> M2 0 0 0 0 }
    secErr = vsub (mvec b1 (0.4, -1.1)) (1.3, -0.5)
    rs = [runMethod rosen (-1.2, 1) i | i <- [0 .. 2]]
    rsAt i = getAt (runMethod rosen (-1.2, 1) i) rs i
    lastOf r = if U.null (rP r) then (0 / 0, 0 / 0) else U.last (rP r)
    rb = rsAt 2
    nb = U.length (rP rb) - 1
    lastB = lastOf rb
    rat = [ errTo rosen (rP rb U.! k) / errTo rosen (rP rb U.! (k - 1)) | k <- [nb - 4 .. nb], k >= 1 ]
    minJS xs = if any isNaN xs then 0 / 0 else foldl' min (1 / 0) xs
    -- n 次元 2 次関数 ½xᵀAx − bᵀx を正確な直線探索で：n 回以内に終わり、H_n = A⁻¹
    quadTests _ [] = []
    quadTests rs0 (n : ns) =
      let (mvals, rs1) = splitAt (n * n) rs0
          mm = chunks n [2 * r - 1 | r <- mvals]
          cols = [[row !! i | row <- mm] | i <- [0 .. n - 1]]
          a = [[dotL ci cj + (if i == j then 0.5 else 0) | (j, cj) <- zip [0 :: Int ..] cols] | (i, ci) <- zip [0 :: Int ..] cols]
          (bv, rs2) = splitAt n rs1
          ax vv = map (`dotL` vv) a
          g0v = zipWith (-) (ax (replicate n 0)) bv
          g0 = sqrt (dotL g0v g0v)
          ident = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1 :: Int]]
          loop xx g hh its
            | sqrt (dotL g g) > 1e-11 * g0 && its < n + 3 =
                let d = map (\row -> negate (dotL row g)) hh
                    t = negate (dotL g d) / dotL d (ax d)
                    sv = map (t *) d
                    xx' = zipWith (+) xx sv
                    g2 = zipWith (-) (ax xx') bv
                    yv = zipWith (-) g2 g
                in loop xx' g2 (bfgsInv hh sv yv) (its + 1)
            | otherwise = (hh, its)
          (hn, itsN) = loop (replicate n 0) g0v ident (0 :: Int)
          hcols = [[row !! j | row <- hn] | j <- [0 .. n - 1]]
          e = foldl' max 0 [abs (dotL r cj - (if i == j then 1 else 0)) | (i, r) <- zip [0 :: Int ..] a, (j, cj) <- zip [0 ..] hcols]
      in stOk ("n = " ++ show n ++ " quadratic, exact line search: " ++ show itsN ++ " steps (<= n), A H_n = I") (itsN <= n && e < 1e-7) ("|A H_n - I|max " ++ fmt 3 e)
         : quadTests rs2 ns
    chunks k xs = case splitAt k xs of
      (c, []) -> [c | not (null c)]
      (c, rest) -> c : chunks k rest

-- ---------- やってみよう ----------
-- 1. ローゼンブロックの谷の中をクリック。最急降下法は数千回、BFGS は数十回。グラフの横軸は対数
-- 2. E で最後まで進め、楕円が重なっていくか見よう。近づくのは「通った方向 s_k の上の曲がり方」だけ
-- 3. 2 次関数（κ = 20）では BFGS が数回で終わる。正確な直線探索なら n = 2 回で必ず終わる（自己テスト）
-- 4. ヒンメルブラウで初期点を変えると、3 つの方法が別々の最小点へ行くことがある
-- 5. bfgsInv の更新をやめる（runMethod の hi' をずっと単位行列）と、BFGS はただの最急降下法になる

-- ---------- 道具 ----------
-- リストのベクトルの内積（n 次元）
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
