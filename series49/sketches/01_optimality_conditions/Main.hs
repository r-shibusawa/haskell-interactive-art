-- ============================================
-- 49-01 最適性条件 ─ 勾配とヘッセ行列
-- 1 次の条件：最小点では ∇f = 0（停留点）。2 次の条件：ヘッセ行列 H の固有値が 両方 > 0 → 極小、両方 < 0 → 極大、正と負 → 鞍点
--   固有値に 0 があると 2 次の条件では決まらない。クリックした点の 2 次近似 m(d) = f + ∇fᵀd + ½dᵀHd を、本当の f と並べて描く
-- 操作：クリック = 点を選ぶ / N = ニュートン法で ∇f = 0 へ 1 歩 / ←→ = 関数 / A = 停留点を全部表示
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

zoom :: Double
zoom = 0.22                  -- ★ 右の 2 つの窓の広さ（全体の何倍か）

saddle, monkey, quart :: Fn
saddle = fn0 { fnName = "saddle x^2 - y^2", fnF = \x y -> x * x - y * y, fnG = \x y -> (2 * x, -2 * y)
             , fnH = \_ _ -> M2 2 0 0 (-2), fnView = (0, 0, 2) }
monkey = fn0 { fnName = "monkey saddle x^3 - 3xy^2", fnF = \x y -> x ^ (3 :: Int) - 3 * x * y * y
             , fnG = \x y -> (3 * x * x - 3 * y * y, -6 * x * y), fnH = \x y -> M2 (6 * x) (-6 * y) (-6 * y) (-6 * x), fnView = (0, 0, 1.4) }
quart = fn0 { fnName = "x^4 + y^2", fnF = \x y -> x ^ (4 :: Int) + y * y, fnG = \x y -> (4 * x ^ (3 :: Int), 2 * y)
            , fnH = \x _ -> M2 (12 * x * x) 0 0 2, fnView = (0, 0, 1.4) }

fns :: [Fn]
fns = [himmel, rosen, saddle, monkey, quart]

data World = World
  { wFi      :: !Int
  , wView    :: !View
  , wCI      :: Picture
  , wCrit    :: [V2]                 -- 停留点（全部）
  , wP       :: !V2                  -- 選んだ点
  , wPath    :: [V2]                 -- ニュートン法の道のり
  , wShowAll :: !Bool
  , wInset   :: Maybe (View, View, Picture, Picture)   -- 右の 2 つの窓（本当の f と 2 次近似）
  , wTests   :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-01 Optimality conditions" world0)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (pickFn env 0 world0) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 (makeView 16 74 666 676 (0, 0, 1)) Blank [] (0, 0) [] True Nothing []

curFn :: World -> Fn
curFn w = getAt himmel fns (wFi w)

-- 関数を選ぶ：等高線と停留点を計算しなおす
pickFn :: Env -> Int -> World -> World
pickFn env i w = setPoint env (cx + hw * 0.33, cy + hw * 0.21) w'
  where
    f = getAt himmel fns i
    (cx, cy, hw) = fnView f
    v = makeView 16 74 (realToFrac (envW env) * 0.52) (realToFrac (envH env) - 124) (fnView f)
    w' = w { wFi = i, wView = v, wCI = fst (contourImage v (fnF f) 18), wCrit = allCritical f }

-- 点を選ぶ：右の窓（点のまわりの本当の f と 2 次近似）を作りなおす
setPoint :: Env -> V2 -> World -> World
setPoint env p w = w { wP = p, wPath = [p], wInset = Just (w1, w2, fst (contourImage w1 (fnF f) 14), fst (contourImage w2 qm 14)) }
  where
    f = curFn w
    ew = realToFrac (envW env)
    s = min (ew * 0.2) (realToFrac (envH env) - 430)
    (_, _, hw) = fnView f
    vv = (fst p, snd p, hw * zoom)
    w1 = makeView (ew * 0.55) 350 s s vv
    w2 = makeView (ew * 0.55 + s + 24) 350 s s vv
    f0 = uncurry (fnF f) p
    g0 = uncurry (fnG f) p
    h = uncurry (fnH f) p
    qm x y = quadModel f0 g0 h (x - fst p, y - snd p)

quadModel :: Double -> V2 -> M2 -> V2 -> Double
quadModel f0 (g0, g1) (M2 a b _ d) (d0, d1) = f0 + g0 * d0 + g1 * d1 + 0.5 * (a * d0 * d0 + 2 * b * d0 * d1 + d * d1 * d1)

-- 固有値の符号で分類（0 に近い固有値があれば 2 次の条件では決まらない）：（名前, 色, 番号）
classify :: Fn -> V2 -> (String, Color, Int)
classify f p
  | abs l1 < tol || abs l2 < tol = ("undecided (eigenvalue 0)", cVi, 0)
  | l1 > 0 = ("minimum", cGr, 1)
  | l2 < 0 = ("maximum", cRd, 2)
  | otherwise = ("saddle", cYe, 3)
  where
    e = eig2 (uncurry (fnH f) p)
    l1 = eL1 e
    l2 = eL2 e
    tol = 1e-6 * (1 + abs l1 + abs l2)

clsOf :: Fn -> V2 -> Int
clsOf f p = let (_, _, k) = classify f p in k

-- 格子の点からニュートン法で ∇f = 0 を解き、同じ点をまとめる
allCritical :: Fn -> [V2]
allCritical f = foldl' add [] [ (cx + hw * fromIntegral i / 6.3, cy + hw * 0.6 * fromIntegral j / 4.2) | i <- [-6 .. 6 :: Int], j <- [-4 .. 4 :: Int] ]
  where
    (cx, cy, hw) = fnView f
    add out s = case criticalPoint f s 80 of
      Just p | abs (fst p - cx) < hw && not (any (\q -> vlen (vsub q p) < 1e-5) out) -> out ++ [p]
      _ -> out

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w ]
  ++ [ Color col (at (toS v c) (ring 8 2.5)) | wShowAll w, c <- wCrit w, let (_, col, _) = classify f c ]
  ++ [ drawPath v (wPath w) cOr 2 True ]
  ++ [ Color (if l > 0 then cGr else cPk) (thickLine 2 (sx - d2f (fst ev) * 46, sy + d2f (snd ev) * 46) (sx + d2f (fst ev) * 46, sy - d2f (snd ev) * 46))
     | (l, ev) <- [(eL1 e, eV1 e), (eL2 e, eV2 e)] ]
  ++ (if stat then [] else
       let r = 70 / gl in
       [ arrowP (sx, sy) (sx + d2f (g0 * r), sy - d2f (g1 * r)) cYe 2.5
       , arrowP (sx, sy) (sx - d2f (g0 * r), sy + d2f (g1 * r)) cCy 1.5 ])
  ++ [ hud [ ("Optimality conditions -- " ++ fnName f, cInk)
           , ("Click: point   N: Newton step to grad f = 0   Left/Right: function   A: critical points " ++ (if wShowAll w then "ON" else "off"), cDim) ] 16 10 15 1.35
     , hud panel x0 68 14 1.32 ]
  ++ insets
  ++ [ drawSelfTest env (wTests w) ]
  where
    f = curFn w
    v = wView w
    p = wP w
    (g0, g1) = uncurry (fnG f) p
    hm@(M2 ha hb hc hd) = uncurry (fnH f) p
    e = eig2 hm
    (sx, sy) = toS v p
    gl = vlen (g0, g1)
    (cls, ccol, ck) = classify f p
    stat = gl < 1e-8 * (1 + abs (uncurry (fnF f) p))
    x0 = envW env * 0.55
    cnt k = length (filter ((== k) . clsOf f) (wCrit w))
    panel =
      [ ("Point x = (" ++ fmt 5 (fst p) ++ ", " ++ fmt 5 (snd p) ++ ")   f(x) = " ++ fmt 5 (uncurry (fnF f) p), cInk)
      , ("grad f = (" ++ fmt 4 g0 ++ ", " ++ fmt 4 g1 ++ ")   |grad f| = " ++ fmt 3 gl, cYe)
      , ("Yellow = grad f (uphill), cyan = -grad f", cDim)
      , ("H = [[" ++ fmt 4 ha ++ ", " ++ fmt 4 hb ++ "], [" ++ fmt 4 hc ++ ", " ++ fmt 4 hd ++ "]]", cInk)
      , ("Eigenvalues l1 = " ++ fmt 4 (eL1 e) ++ ", l2 = " ++ fmt 4 (eL2 e), cInk)
      , ("Lines: eigenvectors (green up, pink down)", cDim)
      , (if stat then "grad f = 0: critical point" else "grad f != 0: not a critical point", if stat then ccol else cDim)
      , (if stat then "  2nd order test: " ++ cls
         else "  local shape: " ++ (case ck of { 1 -> "bowl (convex)"; 2 -> "hill (concave)"; 3 -> "saddle"; _ -> "flat" }), if stat then ccol else cDim)
      , (show (length (wCrit w)) ++ " critical points:", cInk)
      , ("  min " ++ show (cnt 1) ++ ", saddle " ++ show (cnt 3) ++ ", max " ++ show (cnt 2) ++ ", undecided " ++ show (cnt 0), cInk)
      , ("Circles: green min, yellow saddle,", cDim)
      , ("  red max, violet undecided", cDim)
      , ("Right window: m(d) = f + g'd + d'Hd/2", cDim) ]
    insets = case wInset w of
      Nothing -> []
      Just (w1, w2, i1, i2) ->
        concat [ [ img, Color cLine (rectLineTL (d2f (vPx wv)) (d2f (vPy wv)) (d2f (vPw wv)) (d2f (vPh wv)))
                 , Color cOr (at (toS wv p) (ring 4.5 2))
                 , txt (d2f (vPx wv)) (d2f (vPy wv) - 8) 14 cInk ttl ]
               | (wv, img, ttl) <- [(w1, i1, "True f (near the point)"), (w2, i2, "Quadratic model m(d)")] ]
        ++ let d = vHw w1 * 0.8
               err = abs (fnF f (fst p + d * 0.6) (snd p + d * 0.8) - quadModel (uncurry (fnF f) p) (g0, g1) hm (d * 0.6, d * 0.8))
           in [ hud [ ("At the window edge (distance " ++ fmt 3 d ++ "):", cDim), ("  error |f - m| = " ++ fmt 3 err, cDim)
                    , ("Halve the distance -> about 1/8 (cubic term)", cDim) ]
                    (d2f (vPx w1)) (d2f (vPy w1 + vPh w1) + 10) 14 1.4 ]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = toP5 env pos in if inside (wView w) m then setPoint env (toW (wView w) m) w else w
  EventKey (SpecialKey KeyRight) Down _ _ -> pickFn env ((wFi w + 1) `mod` n) w
  EventKey (SpecialKey KeyLeft) Down _ _ -> pickFn env ((wFi w + n - 1) `mod` n) w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wShowAll = not (wShowAll w) }
    | c `elem` "nN" ->      -- ニュートン法：H d = −∇f を解いて x ← x + d（極小とはかぎらず、近くの停留点へ）
        let f = curFn w
            d = solve2 (uncurry (fnH f) (wP w)) (uncurry (fnG f) (wP w))
            q = vsub (wP w) d
        in if finite (fst q + snd q) && abs (fst q) < 1e6 && abs (snd q) < 1e6
             then (setPoint env q w) { wPath = wPath w ++ [q] } else w
  _ -> w
  where n = length fns

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ let (eg, eh) = derivErr f [(0.37, -0.81), (1.3, 0.6), (-0.9, 1.1)]
    in stOk (fnName f ++ ": gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-6) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
  | f <- fns ]
  ++
  [ stOk "Himmelblau: 9 critical points = 4 min, 4 saddles, 1 max; min - saddle + max = 1"
      (length cH == 9 && nMin == 4 && nSad == 4 && nMax == 1 && nMin - nSad + nMax == 1) (show nMin ++ " / " ++ show nSad ++ " / " ++ show nMax)
  , stOk "Himmelblau: all minima have f = 0, max ~ (-0.270845, -0.923039), f ~ 181.617"
      (all (\c -> fnF himmel (fst c) (snd c) < 1e-20) (filter ((== 1) . clsOf himmel) cH)
       && vlen (vadd mx (0.270845, 0.923039)) < 1e-5 && abs (uncurry (fnF himmel) mx - 181.617) < 1e-3)
      ("max f = " ++ fmt 7 (uncurry (fnF himmel) mx))
  , stOk "Rosenbrock: only critical point is (1, 1), a minimum, H = [[802, -400], [-400, 200]]"
      (case rC of
         [r] -> vlen (vsub r (1, 1)) < 1e-9 && clsOf rosen r == 1 && hr0 == 802 && hr1 == -400 && hr3 == 200
         _ -> False) ""
  , stOk "x^2 - y^2: the origin is a saddle" (clsOf saddle (0, 0) == 3 && length (allCritical saddle) == 1) ""
  , stOk "Monkey saddle: only critical point is the origin, H = 0 undecided; f(-t, 0) < 0 < f(t, 0) so not a minimum"
      (case allCritical monkey of
         [m] -> clsOf monkey m == 0 && fnF monkey (-0.1) 0 < 0 && fnF monkey 0.1 0 > 0
         _ -> False) ""
  , stOk "x^4 + y^2: origin has H = diag(0, 2), undecided, but f >= 0 = f(0) so it is the minimum"
      (clsOf quart (0, 0) == 0 && all (\(x, y) -> fnF quart x y > 0) [(0.3, 0), (-0.2, 0.1), (0, -0.4)]) ""
  , stOk "Quadratic model error is cubic: halving the distance gives about 1/8" (abs (ratio - 0.125) < 0.01) ("ratio " ++ fmt 4 ratio ++ " (predicted 0.125)")
  , stOk "Newton step d = -H^-1 grad f is the critical point of the quadratic model (grad m(d) = 0)" (vlen gm < 1e-12 * (1 + vlen g)) "" ]
  where
    cH = allCritical himmel
    cnt k = length (filter ((== k) . clsOf himmel) cH)
    nMin = cnt 1
    nSad = cnt 3
    nMax = cnt 2
    mx = getAt (0, 0) (filter ((== 2) . clsOf himmel) cH) 0
    rC = allCritical rosen
    M2 hr0 hr1 _ hr3 = fnH rosen 1 1
    p = (1, 1)
    g = uncurry (fnG himmel) p
    h@(M2 a b c d) = uncurry (fnH himmel) p
    f0 = uncurry (fnF himmel) p
    er t = abs (fnF himmel (1 + 0.6 * t) (1 + 0.8 * t) - quadModel f0 g h (0.6 * t, 0.8 * t))
    ratio = er 0.025 / er 0.05
    q = solve2 h g
    gm = (fst g - (a * fst q + b * snd q), snd g - (c * fst q + d * snd q))

-- ---------- やってみよう ----------
-- 1. ヒンメルブラウで N を何回か押す。どの停留点に着くかは出発点しだい。鞍点や極大にも着く（ニュートン法は ∇f = 0 を解くだけ）
-- 2. 鞍点の近くをクリック：2 次近似の窓は双曲線（鞍）。極小の近くは楕円（お椀）
-- 3. モンキーサドルと x^4 + y^2 は、どちらも原点の H の固有値に 0 がある。片方は極小、片方はそうでない。2 次の条件だけでは決まらない
-- 4. 右の窓の誤差：zoom を半分にすると誤差は約 1/8 になる（テイラー展開の 3 次の項）
-- 5. ローゼンブロックの (1, 1) の固有値の比 λ₂/λ₁ は約 2500。谷がとても細長い（02 の条件数 κ）

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
