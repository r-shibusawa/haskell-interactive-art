-- ============================================
-- 49-11 ペナルティ法と内点法（対数障壁）
-- 制約つきの問題 min f(x) = ½(x−c)ᵀQ(x−c)  s.t. aᵢᵀx ≤ bᵢ（4 本の半平面）を、制約なしの問題の列で解く
--   ペナルティ法 f + (1/2μ)Σ max(0, gᵢ)² … 外側から近づく ／ 内点法 f − μΣ log(−gᵢ) … 内側から近づく（中心パス）
--   μ → 0 で x(μ) → x*。誤差は O(μ)（x* で効いている制約の乗数が正 ＝ 狭義相補性 のとき）。各 x(μ) はニュートン法で解く
-- 操作：↑↓ = μ / 1 = ペナルティの等高線 / 2 = 障壁の等高線 / 3 = もとの f / クリック = 中心 c を動かす / R = c を戻す
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

qm :: M2
qm = M2 2 0.5 0.5 1                                   -- ★ f の行列 Q（正定値）

c0 :: V2
c0 = (2.5, 2)                                         -- ★ f の中心 c（制約がなければここが最小）

abC :: [(Double, Double, Double)]
abC = [(1, 1, 2), (-1, 2, 2), (0, -1, 1), (-1, 0, 1)]  -- ★ 制約 a₁x + a₂y ≤ b を (a₁, a₂, b) で

mus :: [Double]
mus = [4 * 2 ^^ negate k | k <- [0 .. 24 :: Int]]   -- μ の列（半分ずつ小さく）

xIn :: V2
xIn = (0.3, 0)                                        -- 内点法のスタート（制約の内側）

data Kind = Pen | Bar | Plain deriving Eq

-- 正確な解
data Exact = Exact { exX :: !V2, exLam :: [Double], exV :: !Double }

-- μ の列で解いた結果
data Runs = Runs { rSol :: Exact, rXb, rXp :: [V2], rLamB :: [[Double]], rLogB :: [V2] }

data World = World
  { wCC    :: !V2          -- 中心 c
  , wMi    :: !Int         -- μ の番号
  , wKind  :: !Int         -- 等高線（1〜3）
  , wRuns  :: Runs
  , wCI    :: Picture
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-11 Penalty and interior-point methods" (world0 env1))
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 env) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: Env -> World
world0 env = redraw2 env (World c0 6 1 (solveAll c0) Blank [])

viewOf :: Env -> View
viewOf env = makeView 16 74 (realToFrac (envW env) * 0.52) (realToFrac (envH env) - 124) (0.9, 0.4, 2.8)

fq :: V2 -> V2 -> Double
fq cc x = let (d0, d1) = vsub x cc; M2 a b _ d = qm in 0.5 * (a * d0 * d0 + 2 * b * d0 * d1 + d * d1 * d1)

gcon :: Int -> V2 -> Double
gcon i (x, y) = let (a1, a2, b) = getAt (0, 0, 0) abC i in a1 * x + a2 * y - b

-- 障壁つき（Bar）・ペナルティつき（Pen）の関数：値、勾配、ヘッセ行列
merit :: V2 -> Kind -> Double -> V2 -> (Double, V2, M2)
merit cc kind mu x = go 0 (fq cc x) (mvec qm d) qm
  where
    d = vsub x cc
    go i v g@(g0, g1) h@(M2 h00 h01 h10 h11)
      | i >= length abC = (v, g, h)
      | otherwise =
          let (a0, a1, _) = getAt (0, 0, 0) abC i
              gi = gcon i x
              add w hh = go (i + 1) (v + 0) (g0 + w * a0, g1 + w * a1) (M2 (h00 + hh * a0 * a0) (h01 + hh * a0 * a1) (h10 + hh * a0 * a1) (h11 + hh * a1 * a1))
          in case kind of
               Bar | gi >= 0 -> (1 / 0, g, h)
                   | otherwise -> let (v', g', h') = add (mu / negate gi) (mu / (gi * gi)) in (v' - mu * log (negate gi), g', h')
               Pen | gi <= 0 -> go (i + 1) v g h
                   | otherwise -> let (v', g', h') = add (gi / mu) (1 / mu) in (v' + gi * gi / (2 * mu), g', h')
               Plain -> go (i + 1) v g h

-- 値だけ（v を足していく順番も p5.js と同じにするため、こちらで計算する）
meritV :: V2 -> Kind -> Double -> V2 -> Double
meritV cc kind mu x = go 0 (fq cc x)
  where
    go i v
      | i >= length abC = v
      | otherwise =
          let gi = gcon i x
          in case kind of
               Bar | gi >= 0 -> 1 / 0
                   | otherwise -> go (i + 1) (v - mu * log (negate gi))
               Pen | gi <= 0 -> go (i + 1) v
                   | otherwise -> go (i + 1) (v + gi * gi / (2 * mu))
               Plain -> go (i + 1) v

meritFull :: V2 -> Kind -> Double -> V2 -> (Double, V2, M2)
meritFull cc kind mu x = let (_, g, h) = merit cc kind mu x in (meritV cc kind mu x, g, h)

-- ダンプ付きニュートン法（障壁では、外に出る点や増える点は t を半分に）。通った点も返す
argmin :: V2 -> Kind -> Double -> V2 -> (V2, [V2])
argmin cc kind mu x0 = go (0 :: Int) x0 []
  where
    go it x acc
      | it >= 200 = (x, reverse acc)
      | not (dec > 1e-28) = (x, reverse acc)
      | otherwise = let x' = vaxpy (-t) d x in go (it + 1) x' (x' : acc)
      where
        (mv, g, h) = meritFull cc kind mu x
        d = solve2 h g
        dec = vdot g d
        t = shrink 1
        shrink t0
          | t0 > 1e-14 =
              let vy = meritV cc kind mu (vaxpy (-t0) d x)
              in if finite vy && (dec < 1e-10 || vy <= mv - 0.25 * t0 * dec) then t0 else shrink (t0 * 0.5)
          | otherwise = t0

-- 連立 1 次方程式（部分ピボットのガウス・ジョルダン）。解けなければ Nothing
gaussSolve :: [[Double]] -> [Double] -> Maybe [Double]
gaussSolve m r = go 0 (zipWith (\row ri -> row ++ [ri]) m r)
  where
    n = length r
    go c a
      | c >= n = Just [getAt 0 row n / getAt 1 row i | (i, row) <- zip [0 ..] a]
      | abs (getAt 0 (getAt [] a p) c) < 1e-12 = Nothing
      | otherwise =
          let sw = [ if i == c then getAt [] a p else if i == p then getAt [] a c else row | (i, row) <- zip [0 ..] a ]
              pr = getAt [] sw c
              pc = getAt 1 pr c
              el = [ if i == c then row else let f = getAt 0 row c / pc in [ if j >= c then x - f * getAt 0 pr j else x | (j, x) <- zip [0 ..] row ]
                   | (i, row) <- zip [0 ..] sw ]
          in go (c + 1) el
      where
        p = foldl' (\bp i -> if abs (getAt 0 (getAt [] a i) c) > abs (getAt 0 (getAt [] a bp) c) then i else bp) c [c + 1 .. n - 1]

-- 正確な解：有効な制約の組（0〜2 本）を全部ためし、KKT 条件をみたすものを選ぶ
exactSol :: V2 -> Maybe Exact
exactSol cc = foldl' pick Nothing (map try sets)
  where
    m = length abC
    sets = [] : concat [[i] : [[i, j] | j <- [i + 1 .. m - 1]] | i <- [0 .. m - 1]]
    M2 q00 q01 q10 q11 = qm
    a3 i = getAt (0, 0, 0) abC i
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c
    try s =
      let mat = ([q00, q01] ++ map (fst3 . a3) s)
              : ([q10, q11] ++ map (snd3 . a3) s)
              : [ [fst3 (a3 i), snd3 (a3 i)] ++ map (const 0) s | i <- s ]
          rhs = [q00 * fst cc + q01 * snd cc, q10 * fst cc + q11 * snd cc] ++ map (thd3 . a3) s
      in case gaussSolve mat rhs of
           Nothing -> Nothing
           Just sv ->
             let x = (getAt 0 sv 0, getAt 0 sv 1)
                 lam = [ maybe 0 (\k -> getAt 0 sv (2 + k)) (lookup i (zip s [0 ..])) | i <- [0 .. m - 1] ]
             in if any (< -1e-12) lam || any (\i -> gcon i x > 1e-12) [0 .. m - 1] then Nothing
                else Just (Exact x lam (fq cc x))
    pick best Nothing = best
    pick Nothing (Just e) = Just e
    pick (Just b) (Just e) = if exV e < exV b then Just e else Just b

-- 双対関数 q(λ) = min_x f(x) + Σλᵢgᵢ(x)：x̂ = c − Q⁻¹Aᵀλ を入れる
dualQ :: V2 -> [Double] -> Double
dualQ cc lam = fq cc x + sum [l * gcon i x | (i, l) <- zip [0 ..] lam]
  where
    s = foldl' (\(s0, s1) ((a0, a1, _), l) -> (s0 + l * a0, s1 + l * a1)) (0, 0) (zip abC lam)
    x = vsub cc (solve2 qm s)

solveAll :: V2 -> Runs
solveAll cc = Runs sol (reverse xb) (reverse xp) (reverse lb) (concat (reverse lg))
  where
    sol = fromMaybe (Exact cc (map (const 0) abC) 0) (exactSol cc)
    (_, _, xb, xp, lb, lg) = foldl' one (xIn, cc, [], [], [], []) mus
    one (b, p, axb, axp, alb, alg) mu =
      let (b', lgB) = argmin cc Bar mu b
          (p', _) = argmin cc Pen mu p
      in (b', p', b' : axb, p' : axp, [mu / negate (gcon i b') | i <- [0 .. length abC - 1]] : alb, lgB : alg)

kindOf :: Int -> Kind
kindOf k = case k of { 1 -> Pen; 2 -> Bar; _ -> Plain }

redraw2 :: Env -> World -> World
redraw2 env w = w { wCI = fst (contourImage (viewOf env) (\x y -> meritV (wCC w) (kindOf (wKind w)) (getAt 1 mus (wMi w)) (x, y)) 18) }

errOf :: Exact -> [V2] -> [Double]
errOf s = map (\x -> vlen (vsub x (exX s)))

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w ]
  ++ (if length fp >= 3 then [ Color (rgba255 255 255 255 18) (Polygon (map (toS v) fp)), Color cInk (thickPath 2 (map (toS v) (fp ++ take 1 fp))) ] else [])
  ++ [ drawPath v (take (mi + 1) (rXp r)) cOr 2 True, drawPath v (take (mi + 1) (rXb r)) cCy 2 True
     , Color cGr (at (toS v (exX sol)) (ring 9 2.5))
     , Color cYe (at (toS v (wCC w)) (disc 5)), txt (fst cs + 8) (snd cs + 6) 14 cYe "c"
     , hud [ ("Penalty and interior point -- contours: " ++ getAt "" kindNames (wKind w - 1), cInk)
           , ("Up/Down: mu   1: penalty   2: barrier   3: original f   click: move centre c   R: reset", cDim) ] 16 14 16 1.45
     , hud [ ("mu = " ++ fmt 4 mu ++ "  (Up/Down halves / doubles it)", cInk)
           , ("Exact (try every active set): x* = (" ++ lst (tup (exX sol)) ++ ")", cGr)
           , ("  lam* = (" ++ lst (exLam sol) ++ ")", cGr)
           , ("Penalty (from outside): x(mu) = (" ++ lst (tup (at' (rXp r))) ++ ")", cOr)
           , ("  |x - x*| = " ++ fmt 3 (getAt 0 ep mi) ++ ",  estimate max(0,g_i)/mu = (" ++ lst lp ++ ")", cDim)
           , ("Interior point (from inside): x(mu) = (" ++ lst (tup (at' (rXb r))) ++ ")", cCy)
           , ("  |x - x*| = " ++ fmt 3 (getAt 0 eb mi) ++ ",  lam_i = mu/(-g_i) = (" ++ lst lbNow ++ ")", cDim)
           , ("  duality gap f - q(lam) = " ++ fmt 6 gap ++ "  (theory m mu = " ++ fmt 6 (4 * mu) ++ ", m = 4)", cDim)
           , ("Halving mu multiplies the error by: penalty x" ++ ratio ep ++ "  barrier x" ++ ratio eb, cYe)
           , ("  (O(mu) -> 0.5)", cYe)
           , ("Assumes f strongly convex and lam* > 0 on the constraints", cDim)
           , ("active at x* (strict complementarity). Moving c to where", cDim)
           , ("lam* = 0 makes the approach slower.", cDim) ] x0 80 14 1.45
     , logPlot (x0 + 34) 380 (pw' - 40) (envH env - 460)
         [ series (take (mi + 1) ep) cOr "penalty", series (take (mi + 1) eb) cCy "interior point (barrier)"
         , (series ref cDim "proportional to mu (ref.)") { sDash = True } ]
         plotOpt { poLo = Just (-8), poHi = Just 1, poKmax = Just (length mus - 1)
                 , poXlabel = "k in mu = 4 * 2^-k (right = smaller mu)", poTitle = "|x(mu) - x*|  (log scale)" }
     , drawSelfTest env (wTests w) ]
  where
    v = viewOf env
    r = wRuns w
    sol = rSol r
    mi = wMi w
    mu = getAt 1 mus mi
    fp = feasPoly v
    cs = toS v (wCC w)
    kindNames = ["penalty f + (1/2mu) sum max(0,g_i)^2", "barrier f - mu sum log(-g_i) (inf outside)", "original f"]
    x0 = envW env * 0.55
    pw' = envW env - x0 - 24
    tup (a, b) = [a, b]
    lst = foldr1' . map (fmt 3)
    foldr1' xs = case xs of { [] -> ""; _ -> foldr1 (\a b -> a ++ ", " ++ b) xs }
    at' xs = getAt (0, 0) xs mi
    ep = errOf sol (rXp r)
    eb = errOf sol (rXb r)
    lbNow = getAt [] (rLamB r) mi
    gap = fq (wCC w) (at' (rXb r)) - dualQ (wCC w) lbNow
    lp = [max 0 (gcon i (at' (rXp r))) / mu | i <- [0 .. length abC - 1]]
    ratio e = if mi > 0 then fmt 4 (getAt 0 e mi / getAt 1 e (mi - 1)) else "-"
    nM = length mus
    lastE = getAt 0 ep (nM - 1)
    ref = [m * lastE / getAt 1 mus (nM - 1) | m <- mus]

-- 見えている長方形を半平面で切りとる（許される領域の多角形）
feasPoly :: View -> [V2]
feasPoly v = foldl' cut [(x0, y0), (x1, y0), (x1, y1), (x0, y1)] [0 .. length abC - 1]
  where
    (x0, y0) = toWD v (vPx v, vPy v + vPh v)
    (x1, y1) = toWD v (vPx v + vPw v, vPy v)
    cut [] _ = []
    cut ps i = concat [ [a | ga <= 0] ++ [vaxpy (ga / (ga - gb)) (vsub b a) a | (ga <= 0) /= (gb <= 0)]
                      | (a, b) <- zip ps (drop 1 ps ++ take 1 ps), let ga = gcon i a, let gb = gcon i b ]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | inside v (toP5 env p) -> let cc = toW v (toP5 env p) in redraw2 env w { wCC = cc, wRuns = solveAll cc }
  EventKey (SpecialKey KeyUp) Down _ _ | wMi w > 0 -> redraw2 env w { wMi = wMi w - 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wMi w < length mus - 1 -> redraw2 env w { wMi = wMi w + 1 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> redraw2 env w { wKind = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> redraw2 env w { wCC = c0, wRuns = solveAll c0 }
  _ -> w
  where v = viewOf env

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "Barrier and penalty functions: gradient and Hessian = finite differences"
      (egB < 1e-6 && ehB < 1e-5 && egP < 1e-6 && ehP < 1e-5) ("barrier " ++ fmt 3 egB ++ " / penalty " ++ fmt 3 egP)
  , stOk "Exact x* = (1.875, 0.125), lam* = (2.1875, 0, 0, 0), KKT residual ~ 0"
      (vlen (vsub (exX sol) (1.875, 0.125)) < 1e-12 && abs (getAt 0 (exLam sol) 0 - 2.1875) < 1e-12 && kkt < 1e-12 && vlen st < 1e-12)
      ("complementarity " ++ fmt 3 kkt)
  , stOk "Interior point: duality gap f(x(mu)) - q(lam(mu)) = m mu (for every mu)" gapOk
      ("mu=" ++ fmt 3 (getAt 0 mus 8) ++ ": " ++ fmt 8 (fq c0 (getAt (0, 0) xb 8) - dualQ c0 (getAt [] lamB 8)) ++ " / " ++ fmt 8 (4 * getAt 0 mus 8))
  , stOk "Interior point: every Newton point is strictly inside (g_i < 0)" (all (\x -> all (\i -> gcon i x < 0) [0 .. length abC - 1]) (rLogB rr))
      (show (length (rLogB rr)) ++ " points")
  , stOk "Penalty: multiplier estimate g_1/mu = lam* s/(s+mu) (s = a^T Q^-1 a) -> lam*"
      (okL && abs (gcon 0 (getAt (0, 0) xp n) / getAt 1 mus n - 2.1875) < 1e-5) ("at the smallest mu " ++ fmt 8 (gcon 0 (getAt (0, 0) xp n) / getAt 1 mus n))
  , stOk "Penalty: 2mu -> mu multiplies the error by (s+2mu)/(2s+2mu) -> 0.5"
      (okR && abs (getAt 0 ep n / getAt 1 ep (n - 1) - 0.5) < 1e-3) ("measured " ++ fmt 6 (getAt 0 ep n / getAt 1 ep (n - 1)))
  , stOk "Interior point: halving mu -> error x0.5 (O(mu))" (abs (getAt 0 eb n / getAt 1 eb (n - 1) - 0.5) < 0.01)
      ("measured " ++ fmt 6 (getAt 0 eb n / getAt 1 eb (n - 1)))
  , stOk "Interior point: lam_i = mu/(-g_i) -> lam* = (2.1875, 0, 0, 0)"
      (and [abs (l - s) < 1e-4 | (l, s) <- zip (getAt [] lamB n) (exLam sol)]) (fmt 8 (getAt 0 (getAt [] lamB n) 0)) ]
  where
    rr = solveAll c0
    sol = rSol rr
    xb = rXb rr
    xp = rXp rr
    lamB = rLamB rr
    mk kind = fn0 { fnF = \x y -> let (val, _, _) = meritFull c0 kind 0.1 (x, y) in val
                  , fnG = \x y -> let (_, g, _) = meritFull c0 kind 0.1 (x, y) in g
                  , fnH = \x y -> let (_, _, h) = meritFull c0 kind 0.1 (x, y) in h }
    (egB, ehB) = derivErr (mk Bar) [(0, 0), (0.5, 0.6), (-0.5, -0.4)]
    (egP, ehP) = derivErr (mk Pen) [(2.5, 1), (0, 0), (1.5, -1.6)]
    kkt = maximum [abs (l * gcon i (exX sol)) | (i, l) <- zip [0 ..] (exLam sol)]
    (_, g0, _) = meritFull c0 Plain 1 (exX sol)
    st = foldl' (\acc ((a0, a1, _), l) -> vadd acc (l * a0, l * a1)) g0 (zip abC (exLam sol))
    gapOk = and [ abs (fq c0 x - dualQ c0 lb - 4 * mu) < 1e-9 * 4 * mu | (mu, x, lb) <- zip3 mus xb lamB ]
    (a00, a01, _) = getAt (0, 0, 0) abC 0
    s = vdot (a00, a01) (solve2 qm (a00, a01))
    ep = errOf sol xp
    eb = errOf sol xb
    n = length mus - 1
    okL = and [ abs (gcon 0 x / mu - 2.1875 * s / (s + mu)) < 1e-9 | (mu, x) <- zip mus xp ]
    okR = and [ abs (e1 / e0 - (s + 2 * mu) / (2 * s + 2 * mu)) < 1e-6 | (mu, e0, e1) <- zip3 (drop 1 mus) ep (drop 1 ep) ]

-- ---------- やってみよう ----------
-- 1. ↓ で μ を小さくすると、オレンジ（ペナルティ）は外側から、水色（内点法）は内側から x* に近づく
-- 2. 2 の等高線：μ が小さいほど、障壁は制約のふちぎりぎりまで平らになる（壁がうすくなる）
-- 3. 双対ギャップ f − q(λ) がいつもちょうど mμ（m = 4）。「あと mμ 以内」と保証つきで止められる
-- 4. c を許される領域の中にクリックすると、制約は効かない（λ* = 0）。x(μ) はどう近づく？
-- 5. c をちょうど λ* = 0 の境目に置くと狭義相補性がくずれ、障壁の誤差は O(√μ) くらいに遅くなる

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
