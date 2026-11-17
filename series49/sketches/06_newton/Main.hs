-- ============================================
-- 49-06 ニュートン法
-- いまの点で f を 2 次式 m(d) = f + ∇fᵀd + ½ dᵀHd で近似し、その停留点 d = −H⁻¹∇f へ跳ぶ（H = ヘッセ行列）
--   最小点の近くでは誤差が e → C·e²（正しい桁数が毎回ほぼ倍）。遠くからだと発散したり、鞍点・最大点へ行ったりする
--   減衰ニュートン：H が正定値でなければ −∇f に切りかえ、アルミホの直線探索で歩幅を決める
-- 操作：1〜4 = 関数 / クリック = 初期点 / スペース = 1 回すすむ / A = 自動 / D = 減衰ニュートン ON/off / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kMax :: Int
kMax = 40                     -- ★ 反復の最大回数

floorE :: Double
floorE = 1e-13                -- ★ これより小さい誤差は丸め誤差に埋もれるので、収束の次数に使わない

-- f = √(1+x²) + √(1+y²)：凸だが遠くでは平ら（2 次近似が悪い）。最小点 (0, 0)、最小値 2
sqFn :: Fn
sqFn = fn0
  { fnName = "sqrt(1+x^2) + sqrt(1+y^2)"
  , fnF = \x y -> sqrt (1 + x * x) + sqrt (1 + y * y)
  , fnG = \x y -> (x / sqrt (1 + x * x), y / sqrt (1 + y * y))
  , fnH = \x y -> M2 ((1 + x * x) ** (-1.5)) 0 0 ((1 + y * y) ** (-1.5))
  , fnXs = [(0, 0)], fnFs = 2, fnView = (0, 0, 3.2), fnStart = (1.2, 0.6) }

fns :: [Fn]
fns = [rosen, himmel, sqFn, beale]

-- 1 回分のようす：H の固有値、勾配、ニュートン方向 ds、形（0 正定値 / 1 負定値 / 2 不定）、歩幅 t、−∇f に切りかえたか、跳び先
data Step = Step { sE :: Eig, sG :: !V2, sH :: !M2, sDs :: !V2, sKind :: !Int, sT :: !Double, sFb :: !Bool, sNext :: !V2 }

-- ニュートン法の記録：点、各回、発散したか、行き先の停留点、誤差 e_k、行き先の種類
data RunN = RunN { nP :: [V2], nI :: [Step], nDiv :: Bool, nTgt :: Maybe V2, nE :: U.Vector Double, nKind :: String, nFv, nGn :: [Double] }

data World = World
  { wFi    :: !Int
  , wDamped :: !Bool
  , wAuto  :: !Bool
  , wV     :: !View
  , wCI    :: Picture
  , wX0    :: !V2
  , wRun   :: RunN
  , wShown :: !Int
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-06 Newton's method" w00)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (pick env 0 w00 { wTests = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where w00 = World 0 False False (makeView 16 74 602 676 (0, 0, 1)) Blank (0, 0) (runNewton rosen (-1.2, 1) False kMax) 0 []

curF :: World -> Fn
curF w = getAt rosen fns (wFi w)

pick :: Env -> Int -> World -> World
pick env i w = rerun w { wFi = i, wV = v, wCI = fst (contourImage v (fnF f) 18), wX0 = fnStart f }
  where
    f = getAt rosen fns i
    v = makeView 16 74 (realToFrac (envW env) * 0.47) (realToFrac (envH env) - 124) (fnView f)

rerun :: World -> World
rerun w = w { wRun = runNewton (curF w) (wX0 w) (wDamped w) kMax, wShown = 0 }

-- 1 回分：H の固有値で 2 次近似の形を判定し、跳び先を決める
newtonStep :: Fn -> V2 -> Bool -> Step
newtonStep f p damped = Step e g h ds kind t fb (vaxpy t d p)
  where
    g = uncurry (fnG f) p
    h = uncurry (fnH f) p
    e = eig2 h
    ds = vscale (-1) (solve2 h g)
    kind | eL1 e > 0 = 0
         | eL2 e < 0 = 1
         | otherwise = 2
    fb = damped && (not (eL1 e > 0) || not (vdot ds g < 0))
    d = if fb then vscale (-1) g else ds
    t = if damped then arT (armijo f p d 1e-4 0.5 1) else 1

runNewton :: Fn -> V2 -> Bool -> Int -> RunN
runNewton f p0 damped kk = RunN ps is dv tgt es kind (map (\q -> uncurry (fnF f) q - fnFs f) ps) (map (vlen . uncurry (fnG f)) ps)
  where
    (ps, is, dv) = go 0 p0 [p0] []
    go k p accP accI
      | k >= kk || not (vlen (uncurry (fnG f) p) > 1e-15) = (reverse accP, reverse accI, False)
      | bad (fst nx) || bad (snd nx) || bad (uncurry (fnF f) nx) = (reverse accP, reverse (s : accI), True)
      | nx == p = (reverse accP, reverse accI, False)
      | otherwise = go (k + 1) nx (nx : accP) (s : accI)
      where
        s = newtonStep f p damped
        nx = sNext s
    pl = last ps
    tgt = if dv then Nothing else criticalPoint f pl 60
    es = U.fromList [ maybe (0 / 0) (\t -> vlen (vsub q t)) tgt | q <- ps ]
    kind = case tgt of
      Nothing -> ""
      Just t -> let e = eig2 (uncurry (fnH f) t) in if eL1 e > 0 then "minimum" else if eL2 e < 0 then "maximum" else "saddle point"

-- 収束の次数 q ≈ log(e_{k+1}/e_k) / log(e_k/e_{k−1})（3 つとも丸め誤差より上のときだけ）
orderAt :: U.Vector Double -> Int -> Double
orderAt e k
  | k >= 1 && k + 1 < U.length e && e U.! (k + 1) > floorE && e U.! k > floorE && e U.! (k - 1) > floorE && e U.! k < 0.5 =
      log (e U.! (k + 1) / e U.! k) / log (e U.! k / e U.! (k - 1))
  | otherwise = 0 / 0

-- 2 次近似 m の等高線：固有ベクトルの向きに λ₁u² + λ₂w² = 2c（楕円か双曲線）
modelCurves :: View -> V2 -> Step -> [[V2]]
modelCurves v p s
  | abs l1 < 1e-12 || abs l2 < 1e-12 || not (finite c0) = []
  | otherwise = concat [ one (c0 * sc * sg) | sc <- [0.15, 0.45, 1, 2], sg <- if l1 * l2 > 0 then [1] else [1, -1] ]
  where
    e = sE s
    l1 = eL1 e
    l2 = eL2 e
    v1 = eV1 e
    v2 = eV2 e
    ds = sDs s
    c0 = 0.5 * vdot ds (mvec (sH s) ds)
    r = 4 * vHw v
    pt u w = vadd (vadd p ds) (vadd (vscale u v1) (vscale w v2))
    one c
      | l1 * l2 > 0 =
          if c / l1 <= 0 then [] else
            let a = sqrt (2 * c / l1)
                b = sqrt (2 * c / l2)
            in if a > r then [] else [[ pt (a * cos th) (b * sin th) | i <- [0 .. 48 :: Int], let th = fromIntegral i * pi / 24 ]]
      | otherwise =
          let lp = max l1 l2
              ln = min l1 l2
              up = l1 > 0
              a = sqrt (abs (2 * c / (if c > 0 then lp else ln)))
              b = sqrt (abs (2 * c / (if c > 0 then ln else lp)))
              sS = acosh (max 1.01 (r / a))
          in [ [ if (c > 0) == up then pt aa bb else pt bb aa
               | i <- [0 .. 40 :: Int], let t = -sS + 2 * sS * fromIntegral i / 40, let aa = br * a * cosh t, let bb = b * sinh t ]
             | br <- [1, -1] ]

---------------------------------------------------------------- 時間

step :: Env -> Float -> World -> IO World
step env _ w
  | wAuto w && envFrame env `mod` 40 == 0 && wShown w < length (nP (wRun w)) - 1 = pure w { wShown = wShown w + 1 }
  | otherwise = pure w

---------------------------------------------------------------- 描く

kindName :: Int -> String
kindName k = getAt "" ["positive definite", "negative definite", "indefinite"] k

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w ]
  ++ model
  ++ [ drawPathClip v (take (shown + 1) (nP run)) pathCol 2 ]
  ++ [ Color cGr (at q (ring 8 2)) | m <- fnXs f, let q = toS v m, insideS v q ]   -- 最小点
  ++ [ hud [ ("Newton's method -- " ++ fnName f ++ "   " ++ (if wDamped w then "damped Newton (-grad f if H not PD, Armijo step)" else "pure Newton (always t = 1)"), cInk)
           , ("1-4 function  click start  Space one step  A auto " ++ onOff (wAuto w) ++ "  D damped " ++ onOff (wDamped w) ++ "  R restart", cDim) ] 16 14 16 1.45
     , hud info x y 14 1.4 ]
  ++ [ txt (x + wd * c) (ty + 14) 14 cDim h | (h, c) <- zip ["k", "f - f*", "|grad f|", "e_k", "e_k+1/e_k^2", "order q"] cw ]
  ++ concat [ [ txt (x + wd * c) yy 14 col s | (s, c) <- zip row cw ]
            | k <- [k0 .. min shown (n - 1)]
            , let ek i = if i >= 0 && i < U.length es then es U.! i else 0 / 0
            , let r = if k + 1 < n && ek (k + 1) > floorE && ek k > floorE && ek k < 0.5 then ek (k + 1) / (ek k * ek k) else 0 / 0
            , let q = orderAt es k
            , let yy = ty + 34 + fromIntegral (k - k0) * 18
            , let col = if k == shown then cYe else cInk
            , let row = [ show k, fmt 3 (getAt 0 (nFv run) k), fmt 3 (getAt 0 (nGn run) k)
                        , if nTgt run /= Nothing then fmt 3 (ek k) else "-"
                        , if finite r then fmt 3 r else "-", if finite q then showFFloat (Just 2) q "" else "-" ] ]
  ++ [ hud [ ("x_inf = the stationary point reached. Bounded e_k+1/e_k^2 means", cDim)
           , ("quadratic convergence (q ~ 2). Function 3 has e_k+1 = e_k^3 (q = 3)", cDim) ] x (ty + 20 + 12 * 18 + 8) 14 1.35
     , logPlot (x + 44) top (wd - 50) (envH env - top - 50)
         [ series (take (shown + 1) (nFv run)) pathCol "f - f*"
         , (series (take (shown + 1) (nGn run)) cCy "|grad f|") { sDash = True }
         , series (take (shown + 1) (U.toList es)) cYe "e_k" ]
         plotOpt { poLo = Just (-17), poKmax = Just (max 6 (n - 1)), poTitle = "log scale: with quadratic convergence the curve drops like a cliff" }
     , drawSelfTest env (wTests w) ]
  where
    f = curF w
    v = wV w
    run = wRun w
    es = nE run
    n = length (nP run)
    shown = wShown w
    p = getAt (0, 0) (nP run) shown
    ms = drop shown (nI run)
    pathCol = if wDamped w then cGr else cOr
    onOff b = if b then "ON" else "off"
    rc = rectOf v
    model = case ms of
      (s : _) ->
        let a = toS v p
            m = toS v (vadd p (sDs s))
            nx = toS v (sNext s)
        in [ Color cYe (Pictures [ Line c | curve <- modelCurves v p s, c <- clipPoly rc (map (toS v) curve) ]) ]
           ++ [ Color cYe (dashLine 1.3 5 4 [p1, p2]) | Just (p1, p2) <- [clipSeg rc a m] ]
           ++ [ Color cYe (at m (disc 4.5)) | insideS v m ]
           ++ [ arrowP p1 p2 (if wDamped w then cGr else cOr) 2.5 | not (nDiv run), Just (p1, p2) <- [clipSeg rc a nx] ]
      [] -> []
    x = envW env * 0.5
    y = 82
    wd = envW env - x - 24
    info = (case ms of
              (s : _) -> [ ("H eigenvalues " ++ fmt 3 (eL1 (sE s)) ++ ", " ++ fmt 3 (eL2 (sE s)) ++ " -> " ++ kindName (sKind s)
                            ++ (if sFb s then " -> switch to -grad f" else "") ++ (if wDamped w then "  t = " ++ fmt 3 (sT s) else ""), if sKind s == 0 then cYe else cPk)
                         , ("Model (yellow): " ++ (case sKind s of { 2 -> "hyperbolas, stationary point is a saddle"; 0 -> "ellipses, stationary point is a minimum"; _ -> "ellipses, stationary point is a maximum" }), if sKind s == 0 then cYe else cPk) ]
              [] -> [])
      ++ [ if nDiv run then ("Diverged: |x| keeps growing (for sqrt(1+x^2): x -> -x^3)", cRd)
           else case nTgt run of
             Just t -> ("Reached: " ++ nKind run ++ " (" ++ fmt 6 (fst t) ++ ", " ++ fmt 6 (snd t) ++ ")  " ++ show (length (nI run)) ++ " steps"
                         ++ (if nKind run == "minimum" then "" else "  <- grad f = 0 but not a minimum"), if nKind run == "minimum" then cGr else cPk)
             Nothing -> ("Not converged yet", cDim) ]
    ty = y + 66
    cw = [0, 0.07, 0.25, 0.43, 0.62, 0.84]
    k0 = max 0 (min shown (n - 1) - 11)
    top = ty + 20 + 12 * 18 + 76

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | inside (wV w) (toP5 env p) -> rerun w { wX0 = toW (wV w) (toP5 env p) }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pick env (fromEnum c - fromEnum '1') w
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "dD" -> rerun w { wDamped = not (wDamped w) }
    | c `elem` "rR" -> w { wShown = 0 }
  EventKey (SpecialKey k) Down _ _
    | k == KeySpace || k == KeyRight -> w { wShown = min (length (nP (wRun w)) - 1) (wShown w + 1) }
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ let (eg, eh) = derivErr g pts in stOk (fnName g ++ ": gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-5) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
  | g <- [rosen, himmel, beale, sqFn] ]
  ++ [ stOk "Quadratic (kappa = 30) is solved in one step (the model is exact)" (vlen p1 < 1e-14) ("error after 1 step " ++ fmt 3 (vlen p1))
     , stOk "Rosenbrock (-1.2, 1): damped Newton reaches (1, 1) within 1e-10" (errTo rosen (last (nP rd)) < 1e-10) (show (length (nI rd)) ++ " steps")
     , stOk "Rosenbrock: near (1, 1), e_k+1 <= (M / 2 lambda_min(H(x_k))) e_k^2 (quadratic convergence theorem)" (okq && cnt >= 2)
         (show cnt ++ " times, e_k+1/e_k^2 = " ++ intercalate ", " ratios ++ ", q = " ++ intercalate ", " (map (\q -> showFFloat (Just 2) q "") qs))
     , stOk "sqrt(1+x^2): one Newton step is x -> -x^3 ((0.7, -0.4) -> (-0.343, 0.064))" (abs (fst nx + 0.343) < 1e-14 && abs (snd nx - 0.064) < 1e-14) ""
     , stOk "sqrt(1+x^2): from |x| > 1 pure Newton diverges, damped Newton goes to (0, 0)" (nDiv rs && errTo sqFn (last (nP rsd)) < 1e-10)
         (show (length (nI rs)) ++ " steps to diverge, damped " ++ show (length (nI rsd)))
     , stOk "sqrt(1+x^2): from (0.6, 0) order q = 3 (e_k+1 = e_k^3, +-0.01)" (not (null q3) && all (\q -> abs (q - 3) < 0.01) q3) ("q = " ++ intercalate ", " (map (\q -> showFFloat (Just 3) q "") q3))
     , stOk "Himmelblau: from near each of the 4 minima, reaches that minimum within 1e-10" okh ""
     , stOk "Himmelblau (-0.3, -0.9): pure Newton goes to a maximum (H negative definite), damped Newton to a minimum"
         (nKind rh == "maximum" && nKind rhd == "minimum")
         (case nTgt rh of { Just t -> "maximum (" ++ fmt 6 (fst t) ++ ", " ++ fmt 6 (snd t) ++ "), f = " ++ fmt 6 (uncurry (fnF himmel) t); Nothing -> "-" }) ]
  where
    pts = [(-0.7, 0.4), (1.3, 1.1), (2.2, -1.5), (0.4, 0.9)]
    p1 = getAt (1 / 0, 0) (nP (runNewton (quadFn 30 0.5) (2.4, -1.3) False kMax)) 1
    rd = runNewton rosen (-1.2, 1) True kMax
    ordersOf r = filter finite [orderAt (nE r) k | k <- [0 .. U.length (nE r) - 1]]
    qs = ordersOf rd
    -- 定理：e_{k+1} ≤ ‖H(x_k)⁻¹‖·(M/2)·e_k²（M = H のリプシッツ定数）。(1, 1) の近く（|x| ≤ 1.02）では 3 階微分から M ≤ √((2400·1.02)² + 3·400²)
    mr = sqrt ((2400 * 1.02) ^ (2 :: Int) + 3 * 400 ^ (2 :: Int))
    ek = U.toList (nE rd)
    near = [ (k, a, b) | (k, a, b) <- zip3 [0 :: Int ..] ek (drop 1 ek), a < 0.01, b > 0 ]
    cnt = length near
    okq = and [ b <= mr / (2 * eL1 (eig2 (uncurry (fnH rosen) (getAt (0, 0) (nP rd) k)))) * a * a | (k, a, b) <- near ]
    ratios = [ fmt 3 (b / (a * a)) | (_, a, b) <- near ]
    nx = sNext (newtonStep sqFn (0.7, -0.4) False)
    rs = runNewton sqFn (1.2, 0.6) False kMax
    rsd = runNewton sqFn (1.2, 0.6) True kMax
    q3 = ordersOf (runNewton sqFn (0.6, 0) False kMax)
    okh = and [ nKind r == "minimum" && errTo himmel (last (nP r)) < 1e-10 | m <- fnXs himmel, let r = runNewton himmel (vadd m (0.3, -0.25)) False kMax ]
    rh = runNewton himmel (fnStart himmel) False kMax
    rhd = runNewton himmel (fnStart himmel) True kMax

-- ---------- やってみよう ----------
-- 1. スペースで 1 回ずつ進め、表の e_k の指数（1e-3 → 1e-6 → 1e-12）が倍々になるのを確かめよう
-- 2. 2（ヒンメルブラウ）の初期点は最大点のそば。D で減衰ニュートンにすると最小点へ行き先が変わる
-- 3. 3（√(1+x²)）で (0.9, 0.9) と (1.1, 1.1) をクリック。|x| = 1 が収束と発散の境目
-- 4. 黄色の 2 次近似の等高線が楕円か双曲線かで、跳び先が最小点・鞍点・最大点のどれになるかが決まる
-- 5. ニュートン法は 1 回に H の計算と連立方程式が必要。変数が 100 万個だと？（→ 07 準ニュートン法・08 共役勾配法）

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
