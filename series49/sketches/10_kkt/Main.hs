-- ============================================
-- 49-10 KKT 条件（不等式制約）
-- f = ½(x − c)ᵀQ(x − c) を、4 本の直線の制約 gᵢ(x) = aᵢᵀx − bᵢ ≤ 0 の中で最小にする
--   最小点では −∇f = Σ λᵢ∇gᵢ、λᵢ ≥ 0、λᵢgᵢ = 0（相補性：効いていない制約の λ は 0）
--   解き方：「どの制約が等号か（有効制約）」を全部ためして、KKT 条件を満たすものを選ぶ
-- 操作：ドラッグ = 目標 c（制約がなければの最小点）/ 1〜4 = 制約をえらぶ / ←→ = 回す / ↑↓ = 平行に動かす
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

qMat :: M2
qMat = M2 2 0.6 0.6 1                        -- ★ f の形（対称・正定値）

cons0 :: [(Double, Double)]
cons0 = [(20, 1.6), (110, 1.3), (200, 1.8), (290, 1.2)]   -- ★ 制約：法線の角度（度）と bᵢ

data World = World
  { wTgt    :: !V2                 -- 目標 c
  , wSel    :: !Int                -- えらんでいる制約
  , wCons   :: [(Double, Double)]
  , wSol    :: Maybe Sol           -- Nothing なら実行可能な点がない
  , wRegion :: [V2]                -- 実行可能領域の多角形
  , wTests  :: [Test]
  }

-- KKT 点：x、λ、gᵢ(x)、f(x)、有効制約の組 S
data Sol = Sol { soX :: !V2, soLam :: [Double], soG :: [Double], soF :: !Double, soS :: [Int] }

main :: IO ()
main = runSketch (sketch "49-10 KKT conditions" (update world0))
  { skBack  = cBg
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (update world0) { wTests = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (2.6, 1.8) 0 cons0 Nothing [] []

viewOf :: Env -> View
viewOf env = makeView 16 74 (realToFrac (envW env) * 0.56) (realToFrac (envH env) - 124) (0, 0, 4.2)

consAB :: [(Double, Double)] -> [(V2, Double)]
consAB = map (\(d, b) -> ((cos (d * pi / 180), sin (d * pi / 180)), b))

update :: World -> World
update w = let ab = consAB (wCons w) in w { wSol = solveKKT qMat ab (wTgt w), wRegion = polygonAB ab 12 }

inv2 :: M2 -> M2
inv2 (M2 a b c d) = let det = a * d - b * c in M2 (d / det) (-b / det) (-c / det) (a / det)

-- 有効制約の組 S（0〜2 本）を全部ためす：S の制約を等号にして解き、実行可能かつ λ ≥ 0 なら KKT 点
solveKKT :: M2 -> [(V2, Double)] -> V2 -> Maybe Sol
solveKKT q ab c = foldl' pick Nothing (concatMap try sets)
  where
    m = length ab
    sets = [] : concat [[i] : [[i, j] | j <- [i + 1 .. m - 1]] | i <- [0 .. m - 1]]
    qv = inv2 q
    abAt i = getAt ((0, 0), 0) ab i
    setL ps = [fromMaybe 0 (lookup k ps) | k <- [0 .. m - 1]]
    try s = case s of
      [] -> [check [] c (setL [])]
      [i] -> let (a, b) = abAt i
                 qa = mvec qv a
                 l = (vdot a c - b) / vdot a qa
             in [check s (vaxpy (-l) qa c) (setL [(i, l)])]
      [i, j] -> let (a1, b1) = abAt i
                    (a2, b2) = abAt j
                    q1 = mvec qv a1
                    q2 = mvec qv a2
                    mm@(M2 m00 m01 m10 m11) = M2 (vdot a1 q1) (vdot a1 q2) (vdot a2 q1) (vdot a2 q2)
                    (l1, l2) = solve2 mm (vdot a1 c - b1, vdot a2 c - b2)
                in if abs (m00 * m11 - m01 * m10) < 1e-12 then []          -- 平行な 2 本
                   else [check s (vaxpy (-l2) q2 (vaxpy (-l1) q1 c)) (setL [(i, l1), (j, l2)])]
      _ -> []
    check s x lam =
      let g = [vdot a x - b | (a, b) <- ab]
      in if foldl' max (-1 / 0) g > 1e-10 || foldl' min (1 / 0) lam < -1e-10 then Nothing
         else Just (Sol x lam g (fval q x c) s)
    pick best Nothing = best
    pick Nothing (Just s) = Just s
    pick (Just b) (Just s) = if soF s < soF b - 1e-14 then Just s else Just b

fval :: M2 -> V2 -> V2 -> Double
fval q x c = let d = vsub x c in 0.5 * vdot d (mvec q d)

-- 実行可能領域の多角形（大きな正方形を半平面で切っていく）
polygonAB :: [(V2, Double)] -> Double -> [V2]
polygonAB ab r = foldl' cut [(-r, -r), (r, -r), (r, r), (-r, r)] ab
  where
    cut [] _ = []
    cut ps (a, b) = concat [ [p | gp <= 0] ++ [vaxpy (gp / (gp - gq)) (vsub q p) p | gp * gq < 0]
                           | (p, q) <- zip ps (drop 1 ps ++ take 1 ps), let gp = vdot a p - b, let gq = vdot a q - b ]

-- 別の方法（自己テスト用）：c が外なら、多角形の辺の上で f を最小に（各辺は 1 次元の 2 次関数）
projectByEdges :: M2 -> [V2] -> V2 -> Maybe V2
projectByEdges q ps c
  | length ps < 3 = Nothing
  | isIn = Just c
  | otherwise = Just (foldl' better (getAt c cands 0) cands)
  where
    edges = zip ps (drop 1 ps ++ take 1 ps)
    isIn = and [ (qx - px) * (snd c - py) - (qy - py) * (fst c - px) >= 0 | ((px, py), (qx, qy)) <- edges ]
    cands = [ vaxpy s e p | (p, q') <- edges, let e = vsub q' p, let qe = mvec q e
                          , let s = max 0 (min 1 (negate (vdot (vsub p c) qe) / vdot e qe)) ]
    better b x = if fval q x c < fval q b c then x else b

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgb255 8 26 50) (rectTL px py pw ph) ]
  ++ [ Color (rgba255 120 200 240 30) (Line [(fst (toS v (k, 0)), py), (fst (toS v (k, 0)), py + ph)]) | k <- [-4 .. 4] ]
  ++ [ Color (rgba255 120 200 240 30) (Line [(px, snd (toS v (0, k))), (px + pw, snd (toS v (0, k)))]) | k <- [-4 .. 4] ]
  ++ [ Color (rgba255 110 215 250 120) (Line (ellipse (0.32 * fromIntegral lv))) | lv <- [1 .. 14 :: Int] ]
  ++ (if length (wRegion w) >= 3
        then [ Color (rgba255 255 210 74 55) (Polygon (map (toS v) (wRegion w))), Color cYe (poly (map (toS v) (wRegion w))) ]
        else [])
  ++ concat [ conLine i ab | (i, ab) <- zip [0 ..] abs' ]
  ++ [ Color cPk (at cs (disc 6)), txt (fst cs + 8) (snd cs - 8) 16 cPk "c" ]
  ++ solPics
  -- 見える範囲の外をかくす（p5.js の clip の代わり）
  ++ [ Color cBg (rectTL 0 0 (envW env) py), Color cBg (rectTL 0 (py + ph) (envW env) (envH env - py - ph))
     , Color cBg (rectTL 0 0 px (envH env)), Color cBg (rectTL (px + pw) 0 (envW env - px - pw) (envH env)) ]
  ++ [ hud [ ("KKT conditions -- f = 1/2 (x - c)^T Q (x - c), constraints g_i = a_i^T x - b_i <= 0 (yellow = feasible)", cInk)
           , ("drag: target c   1-4: pick constraint (now g" ++ show (wSel w + 1) ++ ")   Left/Right: rotate   Up/Down: shift", cDim) ] 16 14 16 1.45 ]
  ++ panel
  ++ [ drawSelfTest env (wTests w) ]
  where
    v = viewOf env
    px = d2f (vPx v)
    py = d2f (vPy v)
    pw = d2f (vPw v)
    ph = d2f (vPh v)
    tgt = wTgt w
    abs' = consAB (wCons w)
    e = eig2 qMat
    ellipse r = [ toS v (vaxpy vv (eV2 e) (vaxpy u (eV1 e) tgt))
                | i <- [0 .. 72 :: Int], let t = fromIntegral i / 72 * 2 * pi
                , let u = r * cos t / sqrt (eL1 e), let vv = r * sin t / sqrt (eL2 e) ]
    lamOf i = case wSol w of
      Just s -> getAt 0 (soLam s) i
      Nothing -> 0
    conLine i ((a0, a1), b) =
      let p0 = (a0 * b, a1 * b)
          t = (-a1, a0)
          s1 = toS v (vaxpy (-9) t p0)
          s2 = toS v (vaxpy 9 t p0)
          act = isJustSol && lamOf i > 1e-9
          col = if act then cOr else if i == wSel w then cInk else cDim
          (mx, my) = toS v (vaxpy 1.2 t p0)
      in [ Color col (thickLine (if i == wSel w then 3 else 1.5) s1 s2), txt (mx + 6) (my - 6) 16 col ("g" ++ show (i + 1)) ]
    isJustSol = case wSol w of { Just _ -> True; Nothing -> False }
    cs = toS v tgt
    kk = 50
    solPics = case wSol w of
      Nothing -> []
      Just s ->
        let xs@(sx, sy) = toS v (soX s)
            (gf0, gf1) = mvec qMat (vsub (soX s) tgt)
            arrows = go (sx, sy) [ (i, a, l) | i <- soS s, let (a, _) = getAt ((0, 0), 0) abs' i, let l = getAt 0 (soLam s) i, l > 1e-9 ]
            go _ [] = []
            go (qx, qy) ((_, (a0, a1), l) : rest) =
              let nq = (qx + d2f (a0 * l * kk), qy - d2f (a1 * l * kk))
              in arrowP (qx, qy) nq cOr 2 : go nq rest
        in [ arrowP xs (sx - d2f (gf0 * kk), sy + d2f (gf1 * kk)) cPk 7 ] ++ arrows ++ [ Color white (at xs (disc 6)) ]
    x0 = envW env * 0.6
    y0 = 90
    panel = case wSol w of
      Nothing -> [ hud [("No feasible point (the constraints contradict)", cRd)] x0 y0 16 1.45 ]
      Just s ->
        let lam = soLam s
            g = soG s
            row j = [ show (j + 1), fmt 4 (getAt 0 g j), fmt 4 (getAt 0 lam j), fmt 2 (getAt 0 lam j * getAt 0 g j)
                    , if getAt 0 lam j > 1e-9 then "active (lam > 0)" else if getAt 0 g j > -1e-9 then "tight, lam = 0" else "inactive" ]
            rows = ["i", "g_i(x*)", "lam_i", "lam_i g_i", ""] : map row [0 .. length abs' - 1]
            cols = [0, 30, 135, 235, 335]
            (st0, st1) = foldl' (\acc (((a0, a1), _), l) -> vadd acc (l * a0, l * a1)) (mvec qMat (vsub (soX s) tgt)) (zip abs' lam)
        in [ hud [ ("Minimum x* = (" ++ fmt 5 (fst (soX s)) ++ ", " ++ fmt 5 (snd (soX s)) ++ "),  f(x*) = " ++ fmt 5 (soF s), cInk)
                 , ("Pink -grad f(x*); orange lam_i grad g_i chained -> tips meet", cDim) ] x0 y0 15 1.45 ]
           ++ [ txt (x0 + c) (y0 + 60 + fromIntegral j * 24 + 15) 15 (if j == 0 then cDim else if getAt 0 lam (j - 1) > 1e-9 then cOr else cInk) str
              | (j, r) <- zip [0 :: Int ..] rows, (c, str) <- zip cols r ]
           ++ [ hud [ ("KKT residuals", cYe)
                    , ("(1) stationarity |grad f + sum lam_i grad g_i| = " ++ fmt 3 (vlen (st0, st1)), cInk)
                    , ("(2) primal feasibility  max g_i = " ++ fmt 3 (foldl' max (-1 / 0) g) ++ " <= 0", cInk)
                    , ("(3) dual feasibility  min lam_i = " ++ fmt 3 (foldl' min (1 / 0) lam) ++ " >= 0", cInk)
                    , ("(4) complementarity  max |lam_i g_i| = " ++ fmt 3 (foldl' max (-1 / 0) (zipWith (\l gi -> abs (l * gi)) lam g)), cInk)
                    , ("f convex, constraints linear: KKT <=> minimum", cGr)
                    , ("lam_i = -df*/db_i: how fast f* drops when", cDim)
                    , ("constraint i is relaxed (b_i increased)", cDim) ] x0 (y0 + 200) 15 1.45 ]

---------------------------------------------------------------- 操作

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> clickAt (toP5 env p)
  EventMotion p | envDown env -> clickAt (toP5 env p)
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> update w { wSel = fromEnum c - fromEnum '1' }
  EventKey (SpecialKey KeyLeft) Down _ _ -> update (modSel (\(d, b) -> (d + 5, b)))
  EventKey (SpecialKey KeyRight) Down _ _ -> update (modSel (\(d, b) -> (d - 5, b)))
  EventKey (SpecialKey KeyUp) Down _ _ -> update (modSel (\(d, b) -> (d, min 4 (b + 0.1))))
  EventKey (SpecialKey KeyDown) Down _ _ -> update (modSel (\(d, b) -> (d, max (-3) (b - 0.1))))
  EventKey (Char _) Down _ _ -> update w
  _ -> w
  where
    v = viewOf env
    clickAt m = if inside v m then update w { wTgt = toW v m } else w
    modSel f = w { wCons = [if i == wSel w then f c else c | (i, c) <- zip [0 ..] (wCons w)] }

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "Q = I, project (2,2) onto x + y <= 1: x* = (0.5, 0.5), lam = 1.5"
      (case s1 of
         Just s -> vlen (vsub (soX s) (0.5, 0.5)) < 1e-14 && abs (getAt 0 (soLam s) 0 - 1.5) < 1e-14
         Nothing -> False)
      ("lam = " ++ maybe "none" (\s -> fmt 6 (getAt 0 (soLam s) 0)) s1)
  , stOk "300 random c: residuals of the 4 KKT conditions ~ 0" (worst < 1e-12)
      ("max " ++ fmt 3 worst ++ ", active constraints 0/1/2 = " ++ show c0 ++ "/" ++ show c1 ++ "/" ++ show c2)
  , stOk "Agrees with another method (minimise on the polygon edges)" (wdiff < 1e-9) ("max diff " ++ fmt 3 wdiff)
  , stOk "Cases with 0, 1 and 2 active constraints all occur" (c0 > 0 && c1 > 0 && c2 > 0) ""
  , stOk "lam_i = -df*/db_i (change of the minimum when a constraint is relaxed, vs finite differences)" (ws < 1e-6) ("max diff " ++ fmt 3 ws)
  , stOk "Moving c changes the active set, but x* moves continuously (|dx*|_Q <= |dc|_Q)" okLip "" ]
  where
    s1 = solveKKT (M2 1 0 0 1) [((1, 1), 1)] (2, 2)
    ab = consAB cons0
    pg = polygonAB ab 12
    cs = take 300 (pairUp (rngList 5))
    pairUp (a : b : xs) = (8 * a - 4, 8 * b - 4) : pairUp xs
    pairUp _ = []
    results = [ (c, solveKKT qMat ab c) | c <- cs ]
    resid c ms = case ms of
      Nothing -> 1 / 0
      Just s -> let gf = foldl' (\acc ((a, _), l) -> vaxpy l a acc) (mvec qMat (vsub (soX s) c)) (zip ab (soLam s))
                in maximum ([vlen gf, maximum (0 : soG s), maximum (0 : map negate (soLam s))] ++ zipWith (\l g -> abs (l * g)) (soLam s) (soG s))
    worst = foldl' max 0 [resid c ms | (c, ms) <- results]
    wdiff = foldl' max 0 [ case (projectByEdges qMat pg c, ms) of
                             (Just y, Just s) -> vlen (vsub y (soX s))
                             _ -> 1 / 0
                         | (c, ms) <- results ]
    nAct ms = maybe 0 (length . filter (> 1e-9) . soLam) ms
    c0 = length [() | (_, ms) <- results, nAct ms == 0]
    c1 = length [() | (_, ms) <- results, nAct ms == 1]
    c2 = length [() | (_, ms) <- results, nAct ms == 2]
    circ = [ (c, solveKKT qMat ab c) | k <- [0 .. 200 :: Int], let t = fromIntegral k / 200 * 2 * pi, let c = (3.5 * cos t, 3.5 * sin t) ]
    okLip = and [ case (ma, mb) of
                    (Just a, Just b) -> let dx = vsub (soX b) (soX a)
                                            dc = vsub cb ca
                                        in vdot dx (mvec qMat dx) <= vdot dc (mvec qMat dc) * (1 + 1e-9)
                    _ -> False
                | ((ca, ma), (cb, mb)) <- zip circ (drop 1 circ) ]
    h = 1e-6
    fAt ab' c = maybe (0 / 0) soF (solveKKT qMat ab' c)
    shifted i dh = [ (a, if j == i then b + dh else b) | (j, (a, b)) <- zip [0 :: Int ..] ab ]
    ws = foldl' max 0 [ case solveKKT qMat ab c of
                          Nothing -> 1 / 0
                          Just s -> abs ((fAt (shifted i h) c - fAt (shifted i (-h)) c) / (2 * h) + getAt 0 (soLam s) i)
                      | c <- [(2.6, 1.8), (-3, 0.5), (0.5, -3.2), (3, -3)], i <- [0 .. length ab - 1] ]

-- ---------- やってみよう ----------
-- 1. c を領域の中にドラッグ：x* = c、λ は全部 0。外に出すと、どれかの制約が有効になる
-- 2. 角（2 本の直線の交点）に x* が来るとき、−∇f は 2 本の法線の間（錐）に入る。λ が 2 つとも正
-- 3. ↑↓ で有効な制約の bᵢ を 0.1 動かすと、f* は約 λᵢ × 0.1 だけ変わる（ラグランジュの λ と同じ意味）
-- 4. 制約を動かして領域を空にすると「実行可能な点がない」
-- 5. qMat を M2 1 0 0 1 にすると、x* は c から領域へのふつうの最短点（射影）になる

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
