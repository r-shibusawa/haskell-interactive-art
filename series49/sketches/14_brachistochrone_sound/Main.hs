-- ============================================
-- 49-14 変分法と最速降下線 🔊
-- A から B へ、玉が摩擦なしで転がり落ちる（速さ v = √(2gh)、h は A からの落差）。いちばん早く着く曲線は？
--   折れ線の時間は 1 本ずつ正確に t = 2L/(v₁ + v₂)（まっすぐな坂では一定の加速度）。答えはサイクロイド（変分法）
--   折れ線の高さ y₁…y_N を勾配法で最適化すると、サイクロイドに近づいていく。着いた順に音（速いほど高い）
-- 操作：ドラッグ = 曲線を描く / スペース = 競走 / O = 最適化 ON/OFF / R = 最適化をやり直す / ←→ = B の位置 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

grav :: Double
grav = 9.8                     -- ★ 重力加速度 g（m/s²）

bxs :: [Double]
bxs = [0.5, 1, 2, 3]           -- ★ B の横の位置（A から右へ何 m）。B は A の 1 m 下

byB :: Double
byB = 1

nSeg :: Int
nSeg = 40                      -- ★ 最適化する折れ線の区間の数

slow :: Double
slow = 3                       -- ★ スローモーション（何倍ゆっくり見せるか）

-- 座標は (x, y)：x は右、y は A からの落差（下向きが正）。単位は m
data Cyc = Cyc { cyTh, cyR, cyT, cyRes :: !Double, cyPts :: [V2] }

data World = World
  { wBi      :: !Int
  , wCyc     :: Cyc
  , wOpt     :: [V2]            -- 勾配法で最適化している折れ線
  , wOptOn   :: !Bool
  , wHist    :: [Double]        -- T − T* の履歴（新しい順）
  , wHistN   :: !Int
  , wTstep   :: !Double         -- 直線探索のステップ幅（毎回 2 倍から試す）
  , wUser    :: Maybe [V2]      -- 描いた曲線
  , wDrawing :: !Bool
  , wRace    :: !Double         -- 競走の時刻（−1 = 競走していない）
  , wArrived :: [Int]           -- 着いた曲線の番号
  , wTests   :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-14 Brachistochrone" (reset world0))
  { skBack  = cBg
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (reset world0) { wTests = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2 (cycloid 2 1) [] True [] 0 1e-3 Nothing False (-1) [] []

bxOf :: World -> Double
bxOf w = getAt 2 bxs (wBi w)

reset :: World -> World
reset w = w { wCyc = cycloid bx byB, wOpt = startLine bx, wHist = [], wHistN = 0, wTstep = 1e-3, wUser = Nothing, wRace = -1 }
  where bx = bxOf w

startLine :: Double -> [V2]
startLine bx = [(x, byB * x / bx) | x <- optGrid bx]

-- 折れ線の時間（正確）：区間ごとに 2L/(v₁+v₂)。A より上に出たら ∞（到達できない）
polyT :: [V2] -> Double
polyT ps = go 0 (zip ps (drop 1 ps))
  where
    go t [] = t
    go t (((x1, h1), (x2, h2)) : rest)
      | h1 < 0 || h2 < 0 = 1 / 0
      | l == 0 = go t rest
      | s == 0 = 1 / 0
      | otherwise = go (t + 2 * l / s) rest
      where
        l = hypot (x2 - x1) (h2 - h1)
        s = sqrt (2 * grav * h1) + sqrt (2 * grav * h2)

-- ∂T/∂yᵢ（両端は動かさない）：t = 2L/s の微分、∂L/∂y = ±Δy/L、∂v/∂h = g/v
gradT :: [V2] -> [Double]
gradT ps = zeroEnds (zipWith (+) (map fst segs ++ [0]) (0 : map snd segs))
  where
    -- 区間ごとに（左端への寄与、右端への寄与）
    segs = [ (-2 * dy / l / s - (2 * l / (s * s)) * (if v1 > 0 then grav / v1 else 0)
             , 2 * dy / l / s - (2 * l / (s * s)) * (if v2 > 0 then grav / v2 else 0))
           | ((x1, h1), (x2, h2)) <- zip ps (drop 1 ps)
           , let v1 = sqrt (2 * grav * h1), let v2 = sqrt (2 * grav * h2), let dy = h2 - h1
           , let l = hypot (x2 - x1) dy, let s = v1 + v2 ]
    zeroEnds g = let n = length g in [if i == 0 || i == n - 1 then 0 else v | (i, v) <- zip [0 :: Int ..] g]

-- 勾配法 1 回（アルミホ。ステップ幅は毎回 2 倍から試す。ただし 1 まで）
--   勾配 0（収束）や異常値なら動かない（ステップ幅が ∞ になって止まるのを防ぐ）
optStep :: ([V2], Double) -> ([V2], Double)
optStep (ps, tstep)
  | not (gg > 0) || not (finite gg) || not (finite t0) = (ps, tstep)
  | otherwise = go (min (tstep * 2) 1)
  where
    g = gradT ps
    gg = sum (map (\v -> v * v) g)
    t0 = polyT ps
    go t
      | not (t > 1e-16) = (ps, t)
      | polyT q <= t0 - 1e-4 * t * gg = (q, t)
      | otherwise = go (t * 0.5)
      where q = zipWith (\(x, y) gi -> (x, y - t * gi)) ps g

-- A の近く（急な所）を細かく
optGrid :: Double -> [Double]
optGrid bx = [bx * (fromIntegral i / fromIntegral nSeg) ^ (2 :: Int) | i <- [0 .. nSeg]]

-- サイクロイド x = R(θ − sinθ)、y = R(1 − cosθ)：(θ − sinθ)/(1 − cosθ) = BX/BY を二分法 → ニュートン法で解く
cycloid :: Double -> Double -> Cyc
cycloid bx by = Cyc th rr (th * sqrt (rr / grav)) (fF th) [(rr * (t - sin t), rr * (1 - cos t)) | k <- [0 .. 300 :: Int], let t = th * fromIntegral k / 300]
  where
    r = bx / by
    fF t = (t - sin t) / (1 - cos t) - r
    bis :: Int -> Double -> Double -> Double
    bis 60 lo hi = (lo + hi) / 2
    bis k lo hi = let m = (lo + hi) / 2 in if fF m > 0 then bis (k + 1) lo m else bis (k + 1) m hi
    newton :: Int -> Double -> Double
    newton 5 t = t
    newton k t = let h = 1e-7; d = (fF (t + h) - fF (t - h)) / (2 * h) in newton (k + 1) (t - fF t / d)
    th = newton 0 (bis 0 1e-9 (2 * pi - 1e-9))
    rr = by / (1 - cos th)

-- サイクロイドの x での深さ
cycY :: Cyc -> Double -> Double
cycY c x
  | x <= 0 = 0
  | otherwise = cyR c * (1 - cos (go (0 :: Int) (min (cyTh c) ((6 * x / cyR c) ** (1 / 3)))))
  where
    go 60 t = t
    go k t = let t' = t - (cyR c * (t - sin t) - x) / (cyR c * (1 - cos t))
             in go (k + 1) (max 1e-12 (min (cyTh c) t'))

-- 時刻 τ の玉の位置：区間の中は s = v₁t + ½at²
ballAt :: [V2] -> Double -> V2
ballAt ps tau0 = go tau0 (zip ps (drop 1 ps))
  where
    go _ [] = getAt (0, 0) ps (length ps - 1)
    go tau (((x1, y1), (x2, y2)) : rest)
      | l == 0 = go tau rest
      | tau <= dt = let a = (v2 * v2 - v1 * v1) / (2 * l)
                        s = min l (v1 * tau + 0.5 * a * tau * tau)
                    in (x1 + (x2 - x1) * s / l, y1 + (y2 - y1) * s / l)
      | otherwise = go (tau - dt) rest
      where
        v1 = sqrt (2 * grav * y1)
        v2 = sqrt (2 * grav * y2)
        l = hypot (x2 - x1) (y2 - y1)
        dt = 2 * l / (v1 + v2)

-- 画面の配置（縮尺 sc、A の位置 ox, oy）
layout :: Env -> World -> (Double, Double, Double)
layout env w = (ox, 110, sc)
  where
    ymax = max byB (2 * cyR (wCyc w))
    pw = realToFrac (envW env) * 0.6
    ph = realToFrac (envH env) - 170
    sc = min (pw / (bxOf w + 0.3)) (ph / (ymax + 0.25))
    ox = 40 + 0.1 * sc

toScr :: (Double, Double, Double) -> V2 -> Point
toScr (ox, oy, sc) (x, y) = (d2f (ox + x * sc), d2f (oy + y * sc))

toWorld :: (Double, Double, Double) -> Point -> V2
toWorld (ox, oy, sc) (sx, sy) = ((realToFrac sx - ox) / sc, (realToFrac sy - oy) / sc)

-- 競走する曲線：（点の列、名前、色、太さ）
curvesOf :: World -> [([V2], String, Color, Float)]
curvesOf w =
  [ ([(0, 0), (bxOf w, byB)], "Straight line", cDim, 3)
  , (cyPts (wCyc w), "Cycloid (theory T*)", cGr, 3)
  , (wOpt w, "Optimized polyline", cOr, 2) ]
  ++ [ (u, "Your curve", cCy, 3) | Just u <- [wUser w], length u > 1 ]

step :: Env -> Float -> World -> IO World
step _ dt w0 = do
  let w1 = if wOptOn w0 then foldl' (\w _ -> let (o, t) = optStep (wOpt w, wTstep w)
                                             in w { wOpt = o, wTstep = t, wHist = (polyT o - cyT (wCyc w)) : wHist w, wHistN = wHistN w + 1 }) w0 [1 .. 10 :: Int]
           else w0
      w2 = if wHistN w1 > 4000 then w1 { wOptOn = False } else w1
  if wRace w2 < 0 then pure w2 else do
    let race = wRace w2 + realToFrac dt / slow
        cs = curvesOf w2
        ts = [polyT p | (p, _, _, _) <- cs]
        tl = getAt 1 ts 0
        new = [i | (i, t) <- zip [0 ..] ts, finite t, race >= t, i `notElem` wArrived w2]
    forM_ new $ \i -> do
      let t = getAt 1 ts i
          f = max 110 (min 1500 (330 * 2 ** ((tl - t) / (tl - cyT (wCyc w2)))))
      when (finite f) (playTone (realToFrac f) 0.3 0.6 Triangle)
    pure w2 { wRace = race, wArrived = wArrived w2 ++ new }

draw :: Env -> World -> Picture
draw env w = view env $
  -- 方眼と A・B
  [ Color (rgb255 40 80 120) (Line [(sx, realToFrac oy - 20), (sx, envH env - 50)])
  | k <- [0 :: Int .. 40], let x = 0.25 * fromIntegral k, x <= bx + 1e-9, let sx = fst (toScr lay (x, 0)) ]
  ++ [ Color (rgb255 40 80 120) (Line [(d2f ox - 20, sy), (d2f (ox + (bx + 0.1) * sc), sy)])
     | k <- [0 :: Int .. 40], let y = 0.25 * fromIntegral k, y <= max byB (2 * cyR (wCyc w)) + 1e-9, let sy = snd (toScr lay (0, y)) ]
  ++ [ Color c (fatLine wt (map (toScr lay) p)) | (p, _, c, wt) <- cs ]
  ++ [ txt (fst a - 22) (snd a) 16 cInk "A", txt (fst b + 10) (snd b + 6) 16 cInk "B"
     , Color cInk (at a (disc 4)), Color cInk (at b (disc 4)) ]
  -- 競走
  ++ (if wRace w >= 0
        then [ dot (toScr lay (ballAt p (min (wRace w) t))) 8 c | ((p, _, c, _), t) <- zip cs ts, finite t ]
        else [])
  ++ [ hud [ ("Brachistochrone -- B is " ++ fmt 3 bx ++ " m right of A and " ++ fmt 3 byB ++ " m below (g = " ++ fmt 3 grav ++ ")", cInk)
           , ("drag: draw a curve   space: race (" ++ fmt 3 slow ++ "x slow)   O: optimize " ++ (if wOptOn w then "ON" else "OFF") ++ "   R: restart   Left/Right: B   C: clear", cDim) ] 16 14 16 1.45
     , hud panel x0 80 14 1.45
     , logPlot (x0 + 34) 290 (envW env - x0 - 70) (envH env - 380)
         [ series (reverse (wHist w)) cOr "T (optimizing) - T*"
         , (series (map (const (tcp - cyT (wCyc w))) (wHist w)) cDim "through cycloid pts - T*") { sDash = True } ]
         plotOpt { poLo = Just (-6), poHi = Just 0, poKmax = Just (max 100 (wHistN w - 1)), poTitle = "Gradient descent: T(y1..yN) - T* (s, log)" }
     , drawSelfTest env (wTests w) ]
  where
    lay@(ox, oy, sc) = layout env w
    bx = bxOf w
    cs = curvesOf w
    ts = [polyT p | (p, _, _, _) <- cs]
    a = toScr lay (0, 0)
    b = toScr lay (bx, byB)
    tcp = polyT [(x, cycY (wCyc w) x) | x <- optGrid bx]
    x0 = envW env * 0.64
    cy = wCyc w
    f5 v = showFFloat (Just 5) v ""
    panel =
      [ ("Time to reach B (polyline: exact per segment)", cInk) ]
      ++ [ (nm ++ ": " ++ (if finite t then f5 t ++ " s = " ++ showFFloat (Just 4) (t / cyT cy) "" ++ " T*" else "unreachable (goes above A)"), c)
         | ((_, nm, c, _), t) <- zip cs ts ]
      ++ [ ("Polyline through cycloid: " ++ f5 tcp ++ " s (" ++ show nSeg ++ " segs)", cDim)
         , ("th_B = " ++ fmt 6 (cyTh cy) ++ ", R = " ++ fmt 6 (cyR cy) ++ " m", cGr)
         , ("T* = th_B sqrt(R/g) = " ++ showFFloat (Just 6) (cyT cy) "" ++ " s", cGr)
         , ("Line formula sqrt(2L^2/(g dy)) = " ++ showFFloat (Just 6) (sqrt (2 * (bx * bx + byB * byB) / (grav * byB))) "" ++ " s", cDim) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m@(mx, my) = toP5 env p
    in if mx < envW env * 0.62 && my > 70 then w { wDrawing = True, wUser = Just [(0, 0), toWorld lay m] } else w
  EventMotion p | wDrawing w ->
    let q = toWorld lay (toP5 env p)
    in case wUser w of
         Just u | vlen (vsub q (getAt (0, 0) u (length u - 1))) * sc > 5 && length u < 400 -> w { wUser = Just (u ++ [q]) }
         _ -> w
  EventKey (MouseButton LeftButton) Up _ _ | wDrawing w ->
    w { wDrawing = False, wUser = fmap (++ [(bxOf w, byB)]) (wUser w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRace = 0, wArrived = [] }
  EventKey (SpecialKey KeyRight) Down _ _ | wBi w < length bxs - 1 -> reset w { wBi = wBi w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wBi w > 0 -> reset w { wBi = wBi w - 1 }
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> w { wOptOn = not (wOptOn w) }
    | c `elem` "rR" -> w { wOpt = startLine (bxOf w), wHist = [], wHistN = 0, wTstep = 1e-3, wOptOn = True }
    | c `elem` "cC" -> w { wUser = Nothing }
  _ -> w
  where lay@(_, _, sc) = layout env w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "line: sum of segment formula (37 segs) = closed form sqrt(2L^2/(g dy))"
      (abs (polyT sub - tl) < 1e-12 && abs (polyT [(0, 0), (bx, byB)] - tl) < 1e-12) (showFFloat (Just 9) tl "")
  , stOk "solved the cycloid equation (th - sin th)/(1 - cos th) = dx/dy (residual ~ 0)" (abs (cyRes cy) < 1e-12) ("residual " ++ fmt 3 (abs (cyRes cy)))
  , stOk "cycloid as a fine polyline (300 segs) ~ T* = th_B sqrt(R/g) (slightly slower)"
      (td >= cyT cy && td < cyT cy * (1 + 1e-4)) ("T* " ++ showFFloat (Just 6) (cyT cy) "" ++ " / polyline " ++ showFFloat (Just 6) td "")
  , stOk "dT/dy_i (formula) = finite differences" (e < 1e-6) (fmt 3 e)
  , stOk "1500 descent steps from the line: T* <= T(polyline), within 1% of T*"
      (cyT cy <= to && (to - cyT cy) / cyT cy < 0.01) ("gap " ++ fmt 3 ((to - cyT cy) / cyT cy * 100) ++ "%")
  , stOk "starting from the polyline through cycloid points gets shorter still (but never below T*)"
      (cyT cy <= tpc && tpc < tc) (showFFloat (Just 7) tc "" ++ " -> " ++ showFFloat (Just 7) tpc "" ++ " >= " ++ showFFloat (Just 7) (cyT cy) "")
  , stOk "a curve that goes above A is unreachable (infinite)"
      (isInfinite (polyT [(0, 0), (0.5, -0.1), (bx, byB)]) && isInfinite (polyT [(0, 0), (1, 0), (bx, byB)])) "" ]
  where
    bx = getAt 2 bxs 2
    cy = cycloid bx byB
    tl = sqrt (2 * (bx * bx + byB * byB) / (grav * byB))
    sub = [(bx * fromIntegral k / 37, byB * fromIntegral k / 37) | k <- [0 .. 37 :: Int]]
    td = polyT (cyPts cy)
    p0 = [(x, byB * x / bx + 0.2 * sin (pi * x / bx)) | x <- optGrid bx]
    g0 = gradT p0
    e = foldl' max 0 [ abs ((polyT (bump h) - polyT (bump (-h))) / (2 * h) - gi) / max 1 (abs gi)
                     | i <- [1 .. length p0 - 2], let h = 1e-7, let gi = getAt 0 g0 i
                     , let bump d = [if k == i then (x, y + d) else (x, y) | (k, (x, y)) <- zip [0 ..] p0] ]
    runN :: Int -> ([V2], Double) -> ([V2], Double)
    runN 0 s = s
    runN n s = runN (n - 1) (optStep s)
    to = polyT (fst (runN 1500 (startLine bx, 1e-3)))
    pc0 = [(x, cycY cy x) | x <- optGrid bx]
    tc = polyT pc0
    tpc = polyT (fst (runN 300 (pc0, 1e-3)))

-- ---------- やってみよう ----------
-- 1. 直線より速い曲線を描けるかな？ 最初に急に落ちて速さをかせぐのがコツ
-- 2. ←→ で B を遠くにすると、サイクロイドは B より下にもぐってから上がる。それでも直線より速い
-- 3. 最適化した折れ線（オレンジ）は、勾配法の回数とともにサイクロイド（緑）に重なっていく
-- 4. nSeg を 10 や 80 にすると、点線（区間 nSeg の折れ線で近づける限界の目安）はどう変わる？
-- 5. 音の高さ：直線を基準に、T* に近いほど 1 オクターブまで高くなる。自分の曲線で和音を作ろう

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
