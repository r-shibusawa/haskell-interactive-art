-- ============================================
-- 49-03 直線探索 ── アルミホの条件とウルフの条件
-- 点 x から下り坂の向き d = −∇f(x) に進む。どこまで進むか（ステップ幅 t）を、1 次元の断面 φ(t) = f(x + t d) で決める
--   アルミホ：φ(t) ≤ φ(0) + c₁ t φ'(0)（十分に下がる）　ウルフ：φ'(t) ≥ c₂ φ'(0)（坂がゆるむまで進む）
-- 操作：1〜3 = 関数 / クリック = 点 x / ↑↓ = c₁ / ←→ = c₂ / B = β / スペース = その t で 1 歩すすむ / R = 最初から
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

c1s, c2s, betas :: [Double]
c1s = [1e-4, 0.1, 0.3, 0.5]            -- ★ c₁ の候補
c2s = [0.1, 0.5, 0.9]                  -- ★ c₂ の候補
betas = [0.5, 0.2, 0.8]                -- ★ β の候補

t0 :: Double
t0 = 1                                 -- ★ 最初に試すステップ幅

fns :: [Fn]
fns = [rosen, himmel, quadFn 10 0.5]

-- 断面 φ(t) と、その微分 φ'(t) = ∇f(x + t d)·d
data Ln = Ln { lPhi :: Double -> Double, lDphi :: Double -> Double }

lineOf :: Fn -> V2 -> V2 -> Ln
lineOf f p d = Ln (\t -> uncurry (fnF f) (vaxpy t d p)) (\t -> vdot (uncurry (fnG f) (vaxpy t d p)) d)

-- t を 0〜tm で N 等分して、条件を満たすかを調べた表
data Scan = Scan { sT :: [Double], sArm, sWeak, sStrong, sBoth :: [Bool] }

scanSets :: Ln -> Double -> Double -> Double -> Int -> Scan
scanSets ln tm c1 c2 n = Scan ts (map fst4 rows) (map snd4 rows) (map thd4 rows) (map fth4 rows)
  where
    p0 = lPhi ln 0
    s0 = lDphi ln 0
    ts = [tm * fromIntegral i / fromIntegral n | i <- [0 .. n]]
    rows = [ (a, wk, abs s <= c2 * abs s0, a && wk) | t <- ts, let p = lPhi ln t, let s = lDphi ln t, let a = p <= p0 + c1 * t * s0, let wk = s >= c2 * s0 ]
    fst4 (a, _, _, _) = a
    snd4 (_, b, _, _) = b
    thd4 (_, _, c, _) = c
    fth4 (_, _, _, d) = d

-- 条件を満たす t の区間のリスト
intervals :: [Double] -> [Bool] -> [(Double, Double)]
intervals ts bs = go Nothing (zip3 ts (0 / 0 : ts) bs)
  where
    go (Just a) [] = [(a, getAt 0 ts (length ts - 1))]
    go Nothing [] = []
    go st ((t, prev, v) : rest) = case st of
      Nothing | v -> go (Just t) rest
      Just a | not v -> (a, prev) : go Nothing rest
      _ -> go st rest

-- ウルフ探索（2 分法）：下がりすぎなら上限 hi、坂がまだ急なら下限 lo を更新
--   φ'(0) が負でない（勾配 0 など）ときは動かない（t を 2 倍し続けて ∞ になるのを防ぐ）
data Wolfe = Wolfe { woT :: !Double, woTries :: [Double], woOk :: !Bool }

wolfeSearch :: Ln -> Double -> Double -> Double -> Wolfe
wolfeSearch ln c1 c2 tStart
  | not (s0 < 0) || not (finite s0) = Wolfe 0 [] False
  | otherwise = go (0 :: Int) 0 (1 / 0) tStart []
  where
    p0 = lPhi ln 0
    s0 = lDphi ln 0
    go k lo hi t acc
      | k >= 80 = Wolfe t (reverse acc) False
      | not (lPhi ln t <= p0 + c1 * t * s0) = next lo t
      | lDphi ln t < c2 * s0 = next t hi
      | otherwise = Wolfe t (reverse acc') True
      where
        acc' = t : acc
        next lo' hi' = go (k + 1) lo' hi' (if isInfinite hi' then 2 * t else (lo' + hi') / 2) acc'

-- いまの点での直線探索の結果
data LS = LS { lsD :: V2, lsLn :: Ln, lsP0, lsS0, lsTm :: !Double, lsScan :: Scan, lsBt :: Armijo, lsWo :: Wolfe }

data World = World
  { wFi    :: !Int
  , wC1i, wC2i, wBi :: !Int
  , wView  :: !View
  , wCI    :: Picture
  , wX     :: !V2
  , wPath  :: [V2]
  , wLS    :: Maybe LS
  , wTests :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-03 Line search: Armijo and Wolfe" world0)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (pick env 0 world0) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 2 2 0 (makeView 16 74 576 676 (0, 0, 1)) Blank (0, 0) [] Nothing []

curFn :: World -> Fn
curFn w = getAt rosen fns (wFi w)

pick :: Env -> Int -> World -> World
pick env i w = analyse w { wFi = i, wView = v, wCI = fst (contourImage v (fnF f) 18), wX = fnStart f, wPath = [fnStart f] }
  where
    f = getAt rosen fns i
    v = makeView 16 74 (realToFrac (envW env) * 0.45) (realToFrac (envH env) - 124) (fnView f)

analyse :: World -> World
analyse w
  | not (s0 < -1e-24) = w { wLS = Nothing }
  | otherwise = w { wLS = Just (LS d ln p0 s0 tm (scanSets ln tm c1 c2 400) (armijo f x d c1 (getAt 0.5 betas (wBi w)) t0) (wolfeSearch ln c1 c2 t0)) }
  where
    f = curFn w
    x = wX w
    d = vscale (-1) (uncurry (fnG f) x)
    ln = lineOf f x d
    c1 = getAt 0.3 c1s (wC1i w)
    c2 = getAt 0.9 c2s (wC2i w)
    p0 = lPhi ln 0
    s0 = lDphi ln 0
    grow t = if t < 1e6 && lPhi ln t < p0 then grow (t * 1.5) else t   -- φ(t) が φ(0) より上に戻るところまで
    tm = grow 1e-9 * 1.3

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w, drawPath v (wPath w) cVi 2 True
  , hud [ ("Line search -- " ++ fnName f ++ "   x = (" ++ fmt 3 (fst x) ++ ", " ++ fmt 3 (snd x) ++ ")   f - f* = " ++ fmt 3 (uncurry (fnF f) x - fnFs f), cInk)
        , ("1-3: function   click: point   Up/Down: c1 = " ++ fmt 3 c1 ++ "   Left/Right: c2 = " ++ fmt 3 c2, if c1 >= c2 then cRd else cDim)
        , ("B: beta = " ++ fmt 3 (getAt 0.5 betas (wBi w)) ++ "   Space: 1 step   R: restart" ++ (if c1 >= c2 then "   (c1 < c2 is required)" else ""), if c1 >= c2 then cRd else cDim) ] 16 8 15 1.35 ]
  ++ (case wLS w of
        Nothing -> [hud [("grad f = 0: a critical point. No direction to go.", cGr)] x0 y0 18 1.45]
        Just ls -> lsPics ls)
  ++ [ drawSelfTest env (wTests w) ]
  where
    f = curFn w
    v = wView w
    x = wX w
    c1 = getAt 0.3 c1s (wC1i w)
    c2 = getAt 0.9 c2s (wC2i w)
    x0 = envW env * 0.5
    pw = envW env - x0 - 24
    y0 = 96
    h = envH env * 0.36
    lsPics ls =
      let ln = lsLn ls
          d = lsD ls
          p0 = lsP0 ls
          s0 = lsS0 ls
          tm = lsTm ls
          sc = lsScan ls
          bt = lsBt ls
          wo = lsWo ls
          ys = map (lPhi ln) (sT sc)
          lo = foldl' min (1 / 0) ys
          hi = p0 + 0.35 * (p0 - lo) + 1e-300
          tx t = x0 + pw * d2f (t / tm)
          ty z = y0 + h - h * d2f ((min hi (max lo z) - lo) / (hi - lo))
          ptAt t = toS v (vaxpy t d x)
          tries = arTries bt
          (lastT, lastF) = getAt (0, 0) tries (length tries - 1)
          bl = map (fmt 3 . fst) tries
          btStr = if length bl > 5 then concatArrow (take 2 bl) ++ " -> ... -> " ++ concat (drop (length bl - 1) bl) else concatArrow bl
          concatArrow = foldr1' (\a b -> a ++ " -> " ++ b)
          foldr1' g xs = case xs of { [] -> ""; _ -> foldr1 g xs }
          strongOk = abs (lDphi ln (woT wo)) <= c2 * abs s0
          yt = y0 + h + 116
          fv = [uncurry (fnF f) p - fnFs f | p <- wPath w]
          gn = [vlen (uncurry (fnG f) p) | p <- wPath w]
          bands = [(sArm sc, "Armijo", cOr), (sWeak sc, "weak Wolfe phi'(t) >= c2 phi'(0)", cYe), (sStrong sc, "strong Wolfe |phi'(t)| <= c2 |phi'(0)|", cGr), (sBoth sc, "Armijo + weak Wolfe", cVi)]
      in
      -- 等高線の上：進む直線と、試した点
      [ Color (rgba255 255 255 255 90) (thickLine 1.5 (ptAt 0) (ptAt tm)) ]
      ++ [ Color cPk (at (ptAt t) (disc 3)) | (t, _) <- tries, t <= tm ]
      ++ [ dot (ptAt (woT wo)) 5.5 cVi ]
      -- φ(t) のグラフ
      ++ [ Color (rgba255 8 26 50 230) (rectTL x0 y0 pw h), Color cLine (rectLineTL x0 y0 pw h)
         , Color cCy (fatLine 2.5 [(tx t, ty z) | (t, z) <- zip (sT sc) ys])
         , Color cDim (dashLine 1.5 5 5 [(tx t, ty (p0 + t * s0)) | i <- [0 .. 40 :: Int], let t = tm * fromIntegral i / 40])
         , Color cOr (fatLine 1.5 [(tx t, ty (p0 + c1 * t * s0)) | i <- [0 .. 40 :: Int], let t = tm * fromIntegral i / 40]) ]
      ++ [ Color cPk (at (tx t, ty fq) (disc (if i == length tries - 1 then 6.5 else 4))) | (i, (t, fq)) <- zip [0 :: Int ..] tries, t <= tm ]
      ++ [ Color cVi (at (tx t, y0 + h - 6) (disc 3)) | t <- woTries wo, t <= tm ]
      ++ [ dot (tx (woT wo), ty (lPhi ln (woT wo))) 6 cVi
         , hud [("phi(t) = f(x + t d)", cCy), ("phi(0) + c1 t phi'(0)  (Armijo line)", cOr), ("phi(0) + t phi'(0)  (tangent)", cDim)] (x0 + 8) (y0 + 6) 14 1.35
         , txtC x0 (y0 + h + 10) 14 cDim "0", txtR (x0 + pw) (y0 + h + 17) 14 cDim (fmt 3 tm), txtC (x0 + pw / 2) (y0 + h + 10) 14 cDim "t" ]
      -- 条件を満たす t の区間（帯）
      ++ concat [ [ Color (rgb255 30 60 95) (rectTL x0 yy pw 16) ]
                  ++ [ Color c (rectTL (tx a) yy (max 2 (tx b - tx a)) 16) | (a, b) <- intervals (sT sc) bs ]
                  ++ [ txt (x0 + 5) (yy + 14) 14 black nm, txt (x0 + 4) (yy + 13) 14 cInk nm ]
                | (r, (bs, nm, c)) <- zip [0 :: Int ..] bands, let yy = y0 + h + 22 + fromIntegral r * 22 ]
      ++ [ hud [ ("Backtracking (from t0 = " ++ fmt 3 t0 ++ ", times beta): " ++ show (length bl) ++ " tries", cPk)
               , ("   " ++ btStr, cPk)
               , ("   accepted t = " ++ fmt 4 lastT ++ ": phi = " ++ fmt 5 lastF ++ " <= " ++ fmt 5 (p0 + c1 * lastT * s0), cDim)
               , ("Wolfe search (bisection): " ++ show (length (woTries wo)) ++ " tries, t = " ++ fmt 4 (woT wo), cVi)
               , (if woOk wo then "   Armijo OK, weak Wolfe OK, strong Wolfe " ++ (if strongOk then "OK" else "NG") else "   not found", cVi) ]
               x0 yt 14 1.4
         , logPlot (x0 + 50) (yt + 124) (pw - 56) (envH env - yt - 174)
             [series fv cVi "f - f*", (series gn cCy "|grad f|") { sDash = True }]
             plotOpt { poKmax = Just (max 5 (length (wPath w) - 1)), poTitle = "f - f* and |grad f| of the points (steepest descent, Wolfe t)" } ]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = toP5 env pos in if inside (wView w) m then let p = toW (wView w) m in analyse w { wX = p, wPath = [p] } else w
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pick env (fromEnum c - fromEnum '1') w
  EventKey k Down _ _ -> analyse (key k)
  _ -> w
  where
    n1 = length c1s
    n2 = length c2s
    f = curFn w
    key k = case k of
      SpecialKey KeyUp -> w { wC1i = (wC1i w + 1) `mod` n1 }
      SpecialKey KeyDown -> w { wC1i = (wC1i w + n1 - 1) `mod` n1 }
      SpecialKey KeyRight -> w { wC2i = (wC2i w + 1) `mod` n2 }
      SpecialKey KeyLeft -> w { wC2i = (wC2i w + n2 - 1) `mod` n2 }
      Char c | c `elem` "bB" -> w { wBi = (wBi w + 1) `mod` length betas }
             | c `elem` "rR" -> w { wX = fnStart f, wPath = [fnStart f] }
      SpecialKey KeySpace -> case wLS w of
        Just ls | woOk (lsWo ls) ->
          let nx = vaxpy (woT (lsWo ls)) (lsD ls) (wX w)
          in if bad (uncurry (fnF f) nx) then w else w { wX = nx, wPath = wPath w ++ [nx] }
        _ -> w
      _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "phi'(t) = grad f(x + t d).d agrees with finite differences (3 functions x 4 points)" (maxe < 1e-5) ("relative error " ++ fmt 3 maxe)
  , stOk "Quadratic: Armijo interval = [0, 2(1-c1)t*] (compared with the scan, 12 cases)" ok1 ""
  , stOk "Quadratic: weak Wolfe = [(1-c2)t*, inf), strong Wolfe = [(1-c2)t*, (1+c2)t*]" ok2 ""
  , stOk "Quadratic: backtracking t in (beta 2(1-c1)t*, 2(1-c1)t*] (t0 = 10)" ok3 ""
  , stOk "Rosenbrock, Himmelblau (16 points): Wolfe search t satisfies Armijo and weak Wolfe" ok4 ""
  , stOk "Same 16 points: backtracking t satisfies Armijo" ok5 ""
  , stOk "Steepest descent with Wolfe t reaches (1, 1) even on Rosenbrock (slowly)" (errTo rosen pR < 1e-3) (show itR ++ " steps, error " ++ fmt 3 (errTo rosen pR)) ]
  where
    q = quadFn 10 0.5
    rs0 = rngList 3
    -- 1. φ' と差分（3 関数 × 4 点、乱数を 3 つずつ）
    (maxe, rs1) = foldl' derivOne (0, rs0) [f | f <- [rosen, himmel, q], _ <- [1 .. 4 :: Int]]
    derivOne (m, r1 : r2 : r3 : rs) f =
      let (cx, cy, hw) = fnView f
          p = (cx + (r1 - 0.5) * hw, cy + (r2 - 0.5) * hw)
          g = uncurry (fnG f) p
          ln = lineOf f p (vscale (-1) g)
          t = 0.3 * r3 / (1 + vlen g)
          h = 1e-6 / (1 + vlen g)
      in (max m (abs (lDphi ln t - (lPhi ln (t + h) - lPhi ln (t - h)) / (2 * h)) / max 1 (abs (lDphi ln t))), rs)
    derivOne acc _ = acc
    -- 2. 2 次関数で区間を確かめる（12 通り）
    (quadRes, rs2) = takeQ (0 :: Int) rs1
    takeQ 12 rs = ([], rs)
    takeQ n (r1 : r2 : rs) = let (xs, rs') = takeQ (n + 1) rs in (quadOne n (4 * r1 - 2, 4 * r2 - 2) : xs, rs')
    takeQ _ rs = ([], rs)
    quadOne n p =
      let g = uncurry (fnG q) p
          d = vscale (-1) g
          ln = lineOf q p d
          aa = vdot d (mvec (fnH q 0 0) d)
          ts = -lDphi ln 0 / aa
          c1 = getAt 0 [1e-4, 0.1, 0.3] (n `mod` 3)
          c2 = getAt 0 [0.5, 0.9, 0.7] (n `mod` 3)
          beta = getAt 0 [0.5, 0.2, 0.8] (n `mod` 3)
          tm = 2.5 * ts
          nn = 2000
          dt = 1.01 * tm / fromIntegral nn
          sc = scanSets ln tm c1 c2 nn
          ia = intervals (sT sc) (sArm sc)
          iw = intervals (sT sc) (sWeak sc)
          isw = intervals (sT sc) (sStrong sc)
          o1 = case ia of { [(a, b)] -> a == 0 && abs (b - 2 * (1 - c1) * ts) <= dt; _ -> False }
          o2 = case (iw, isw) of
            ([(a, b)], [(sa, sb)]) -> abs (a - (1 - c2) * ts) <= dt && b == tm && abs (sa - (1 - c2) * ts) <= dt && abs (sb - min tm ((1 + c2) * ts)) <= dt
            _ -> False
          tb = arT (armijo q p d c1 beta 10)
          ub = 2 * (1 - c1) * ts
          o3 = tb <= ub * (1 + 1e-12) && tb > beta * ub * (1 - 1e-12)
      in (o1, o2, o3)
    ok1 = and [a | (a, _, _) <- quadRes] && length quadRes == 12
    ok2 = and [b | (_, b, _) <- quadRes]
    ok3 = and [c | (_, _, c) <- quadRes]
    -- 3. ローゼンブロック・ヒンメルブラウの 16 点
    (ok4, ok5) = foldl' (\(a, b) (x, y) -> (a && x, b && y)) (True, True) (go16 [f | f <- [rosen, himmel], _ <- [1 .. 8 :: Int]] rs2)
    go16 (f : fs) (r1 : r2 : rs) =
      let (cx, cy, hw) = fnView f
          p = (cx + (r1 - 0.5) * hw, cy + (r2 - 0.5) * hw)
          g = uncurry (fnG f) p
          d = vscale (-1) g
          ln = lineOf f p d
          wo = wolfeSearch ln 1e-4 0.9 1
          t = woT wo
          o4 = woOk wo && lPhi ln t <= lPhi ln 0 + 1e-4 * t * lDphi ln 0 && lDphi ln t >= 0.9 * lDphi ln 0
          bt = arT (armijo f p d 1e-4 0.5 1)
          o5 = uncurry (fnF f) (vaxpy bt d p) <= lPhi ln 0 + 1e-4 * bt * lDphi ln 0
      in (if vlen g < 1e-8 then id else ((o4, o5) :)) (go16 fs rs)
    go16 _ _ = []
    -- 4. ウルフの t で最急降下（ローゼンブロック）
    (pR, itR) = descend (fnStart rosen) (0 :: Int)
    descend p it
      | it >= 20000 || not (vlen (uncurry (fnG rosen) p) > 1e-6) = (p, it)
      | otherwise = let d = vscale (-1) (uncurry (fnG rosen) p)
                        wo = wolfeSearch (lineOf rosen p d) 1e-4 0.9 1
                    in descend (vaxpy (woT wo) d p) (it + 1)

-- ---------- やってみよう ----------
-- 1. ローゼンブロックでは ∇f が大きいので t は 0.001 くらい。t₀ = 1 からのバックトラックは何回縮める？
-- 2. c₁ を 0.5 にすると、2 次関数ではちょうど t* までしか許されない（2(1−c₁)t* = t*）
-- 3. c₂ を 0.1 にすると強ウルフの区間がせまくなり、ほぼ「正確な直線探索」になる
-- 4. スペースを何回も押して、ローゼンブロックの谷をジグザグに進むのを見よう（06・07 の方法と比べる）
-- 5. c₁ ≥ c₂ にすると、両方の条件を満たす t がなくなることがある（帯の紫が消える）

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
