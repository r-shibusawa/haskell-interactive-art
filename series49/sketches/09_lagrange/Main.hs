-- ============================================
-- 49-09 ラグランジュの未定乗数法
-- 曲線 g(x, y) = c の上で f を最小にする。最小点では ∇f と ∇g が平行：∇f = λ∇g（ラグランジュ関数 L = f − λ(g − c)）
--   点を曲線に沿ってドラッグすると、∇f の「曲線に沿った成分」（ピンク）が 0 になる所が最小点
--   λ の意味：制約の値 c を動かしたときの最小値の変化 df*/dc = λ（右のグラフの接線の傾き）
-- 操作：1〜4 = 制約の曲線 / F = 関数 f / ↑↓ = c / ドラッグ = 曲線上の点 / スペース = 曲線に沿って下る
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nS :: Int
nS = 600                     -- ★ 曲線を何点で調べるか

-- 関数 f（値・勾配・ヘッセ行列）
fs :: [Fn]
fs =
  [ fn0 { fnName = "f = (x-2)^2 + 2(y-1)^2 + 0.8xy"
        , fnF = \x y -> (x - 2) ^ (2 :: Int) + 2 * (y - 1) ^ (2 :: Int) + 0.8 * x * y
        , fnG = \x y -> (2 * (x - 2) + 0.8 * y, 4 * (y - 1) + 0.8 * x)
        , fnH = \_ _ -> M2 2 0.8 0.8 4 }
  , fn0 { fnName = "f = x + 2y", fnF = \x y -> x + 2 * y, fnG = \_ _ -> (1, 2), fnH = \_ _ -> M2 0 0 0 0 } ]

-- 制約の曲線 g = c（cPar t c は曲線のパラメータ表示、cTr は t の範囲）
data Con = Con { cG :: Fn, cPar :: Double -> Double -> V2, cTr :: (Double, Double), cC0, cCmin :: Double }

cons :: [Con]
cons =
  [ Con fn0 { fnName = "circle x^2 + y^2 = c", fnF = \x y -> x * x + y * y, fnG = \x y -> (2 * x, 2 * y), fnH = \_ _ -> M2 2 0 0 2 }
        (\t c -> (sqrt c * cos t, sqrt c * sin t)) (0, 2 * pi) 4 0.1
  , Con fn0 { fnName = "ellipse x^2/4 + y^2 = c", fnF = \x y -> x * x / 4 + y * y, fnG = \x y -> (x / 2, 2 * y), fnH = \_ _ -> M2 0.5 0 0 2 }
        (\t c -> (2 * sqrt c * cos t, sqrt c * sin t)) (0, 2 * pi) 1 0.05
  , Con fn0 { fnName = "line x + y = c", fnF = \x y -> x + y, fnG = \_ _ -> (1, 1), fnH = \_ _ -> M2 0 0 0 0 }
        (\t c -> (t, c - t)) (-6, 6) 1 (-4)
  , Con fn0 { fnName = "wave y - sin x = c", fnF = \x y -> y - sin x, fnG = \x _ -> (-cos x, 1), fnH = \x _ -> M2 (sin x) 0 0 0 }
        (\t c -> (t, sin t + c)) (-6, 6) 0.5 (-3) ]

-- 解：(x, y, λ)、f*、残差、接線方向の ∇²L
data Sol = Sol { sZ :: !(Double, Double, Double), sFs, sRes, sCurv :: !Double }

data World = World
  { wFi, wCi  :: !Int
  , wC        :: !Double
  , wView     :: !View
  , wCI       :: Picture
  , wT        :: !Double           -- 白い点の曲線上の位置 t
  , wSliding  :: !Bool
  , wSol      :: Maybe Sol
  , wCurve    :: [V2]
  , wFstar    :: [(Double, Double)]
  , wTests    :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-09 Lagrange multipliers" world0)
  { skBack  = cBg
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env True world0) { wTests = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 0 4 (makeView 16 74 717 676 (0, 0, 4.5)) Blank 0.5 False Nothing [] [] []

conAt :: Int -> Con
conAt i = getAt (Con fn0 (\_ _ -> (0, 0)) (0, 1) 1 0) cons i

fAt :: Int -> Fn
fAt i = getAt fn0 fs i

rebuild :: Env -> Bool -> World -> World
rebuild env newF w0 = w { wCurve = pts, wSol = sol, wFstar = graph }
  where
    v = makeView 16 74 (realToFrac (envW env) * 0.56) (realToFrac (envH env) - 124) (0, 0, 4.5)
    w = if newF then w0 { wView = v, wCI = fst (contourImage v (fnF (fAt (wFi w0))) 18) } else w0
    g = conAt (wCi w)
    (t0, t1) = cTr g
    pts = [cPar g (t0 + (t1 - t0) * fromIntegral i / fromIntegral nS) (wC w) | i <- [0 .. nS]]
    sol = solveLagrange (fAt (wFi w)) g (wC w) Nothing
    graph = [ (c, sFs s) | i <- [-20 .. 20 :: Int], let c = wC w + fromIntegral i * 0.05, c >= cCmin g
                         , Just s <- [solveLagrange (fAt (wFi w)) g c (sZ <$> sol)] ]

-- 3×3 の連立方程式（ガウスの消去法、部分ピボット）
solve3 :: [[Double]] -> [Double] -> [Double]
solve3 m r = back (elim 0 (zipWith (\row ri -> row ++ [ri]) m r))
  where
    elim k a
      | k >= 3 = a
      | otherwise =
          let p = foldl' (\pb i -> if abs (a !! i !! k) > abs (a !! pb !! k) then i else pb) k [k + 1 .. 2]
              a1 = swap k p a
              rk = a1 !! k
              a2 = [ if i > k then let mm = (row !! k) / (rk !! k) in [if j >= k then x - mm * (rk !! j) else x | (j, x) <- zip [0 :: Int ..] row] else row
                   | (i, row) <- zip [0 :: Int ..] a1 ]
          in elim (k + 1) a2
    swap i j xs = [ if n == i then xs !! j else if n == j then xs !! i else x | (n, x) <- zip [0 ..] xs ]
    back a = foldr (\i xs -> let row = a !! i
                                 s = row !! 3 - sum [row !! j * (xs !! (j - i - 1)) | j <- [i + 1 .. 2]]
                             in s / (row !! i) : xs) [] [0 .. 2]

-- ∇f − λ∇g = 0, g = c を (x, y, λ) のニュートン法で。初期値：曲線を細かく調べて f が最小の点（z0 があればそこから）
solveLagrange :: Fn -> Con -> Double -> Maybe (Double, Double, Double) -> Maybe Sol
solveLagrange fn g c z0 = do
  zi <- case z0 of
    Just z -> Just z
    Nothing ->
      let (t0, t1) = cTr g
          cands = [ (uncurry (fnF fn) p, p, i) | i <- [0 .. nS], let p = cPar g (t0 + (t1 - t0) * fromIntegral i / fromIntegral nS) c ]
          (_, bp, bi) = foldl' (\b@(bv, _, _) x@(xv, _, _) -> if xv < bv then x else b) (getAt (1 / 0, (0, 0), 0) cands 0) cands
          gf = uncurry (fnG fn) bp
          gg = uncurry (fnG (cG g)) bp
      in if t0 < 0 && (bi < 3 || bi > nS - 3) then Nothing      -- 端で最小 → 曲線の上で下限なし
         else Just (fst bp, snd bp, vdot gf gg / vdot gg gg)
  (x, y, l) <- newton (0 :: Int) zi
  let gg = fnG (cG g) x y
      gf = fnG fn x y
      M2 fa fb fc fd = fnH fn x y
      M2 ga gb gc gd = fnH (cG g) x y
      n = vlen gg
      (tx, ty) = (-(snd gg) / n, fst gg / n)
      (w0, w1, w2, w3) = (fa - l * ga, fb - l * gb, fc - l * gc, fd - l * gd)
      res = hypot (hypot (fst gf - l * fst gg) (snd gf - l * snd gg)) (fnF (cG g) x y - c)
  pure (Sol (x, y, l) (fnF fn x y) res (tx * (w0 * tx + w1 * ty) + ty * (w2 * tx + w3 * ty)))
  where
    newton k z@(x, y, l)
      | k >= 40 = Just z
      | not (all finite [x', y', l']) = Nothing
      | hypot (hypot d0 d1) d2 < 1e-15 * (1 + hypot (hypot x' y') l') = Just (x', y', l')
      | otherwise = newton (k + 1) (x', y', l')
      where
        gf = fnG fn x y
        gg = fnG (cG g) x y
        M2 fa fb fc fd = fnH fn x y
        M2 ga gb gc gd = fnH (cG g) x y
        r = [fst gf - l * fst gg, snd gf - l * snd gg, fnF (cG g) x y - c]
        j = [[fa - l * ga, fb - l * gb, -(fst gg)], [fc - l * gc, fd - l * gd, -(snd gg)], [fst gg, snd gg, 0]]
        (d0, d1, d2) = case solve3 j r of { [a, b, e] -> (a, b, e); _ -> (0 / 0, 0, 0) }
        (x', y', l') = (x - d0, y - d1, l - d2)

fstarAt :: Int -> Int -> Double -> Maybe (Double, Double, Double) -> Double
fstarAt fi ci c z0 = maybe (0 / 0) sFs (solveLagrange (fAt fi) (conAt ci) c z0)

---------------------------------------------------------------- 動かす

-- スペース：曲線に沿って下る（t ← t − 0.02 ∇f·(dp/dt) / max(1, |dp/dt|²)）
step :: Env -> Float -> World -> IO World
step _ _ w = pure w { wT = clampT g t' }
  where
    g = conAt (wCi w)
    fn = fAt (wFi w)
    t = wT w
    h = 1e-5
    a = cPar g (t - h) (wC w)
    b = cPar g (t + h) (wC w)
    dp = vscale (1 / (2 * h)) (vsub b a)
    gf = uncurry (fnG fn) (cPar g t (wC w))
    t' = if wSliding w then t - 0.02 * vdot gf dp / max 1 (vdot dp dp) else t

clampT :: Con -> Double -> Double
clampT g t = let (t0, t1) = cTr g in if isNaN t then t0 else max t0 (min t1 t)

---------------------------------------------------------------- 描く

draw :: Env -> World -> Picture
draw env w = view env $
  [ wCI w
  , Color cCy (fatLine 3 [ (max px0 (min px1 sx), max py0 (min py1 sy)) | p <- wCurve w, let (sx, sy) = toS v p ]) ]
  ++ [ arrowP s (fst s + d2f (fst a), snd s - d2f (snd a)) c 3 | (vv, c) <- [(gf, cOr), (gg, cCy), (tan', cPk)], let a = sc vv ]
  ++ [ Color white (at s (disc 6)) ]
  ++ [ Color cGr (at (toS v (x, y)) (ring 11 3)) | Just Sol { sZ = (x, y, _) } <- [wSol w] ]
  ++ [ hud [ ("Lagrange multipliers -- " ++ fnName fn ++ "   constraint " ++ fnName (cG g) ++ ", c = " ++ fmt 3 (wC w), cInk)
           , ("1-4: constraint   F: function   Up/Down: c   drag: point on the curve   space: slide down " ++ (if wSliding w then "ON" else "off"), cDim) ] 16 14 16 1.4
     , hud [ ("White point (" ++ fmt 3 (fst p) ++ ", " ++ fmt 3 (snd p) ++ "): f = " ++ fmt 4 (uncurry (fnF fn) p), cInk)
           , ("orange grad f, blue grad g (normal),", cDim)
           , ("pink = part of grad f along the curve", cDim)
           , ("along-curve part = " ++ fmt 3 (vlen tan') ++ " (candidates where 0)", cPk)
           , ("if grad f = lambda grad g: lambda = " ++ fmt 4 (vdot gf gg / vdot gg gg), cInk) ] x y 14 1.45 ]
  ++ solPart
  ++ [ drawSelfTest env (wTests w) ]
  where
    fn = fAt (wFi w)
    g = conAt (wCi w)
    v = wView w
    px0 = d2f (vPx v)
    px1 = d2f (vPx v + vPw v)
    py0 = d2f (vPy v)
    py1 = d2f (vPy v + vPh v)
    p = cPar g (wT w) (wC w)
    s = toS v p
    gf = uncurry (fnG fn) p
    gg = uncurry (fnG (cG g)) p
    nrm = vscale (1 / vlen gg) gg
    gn = vdot gf nrm
    tan' = vsub gf (vscale gn nrm)
    sc vv = let l = vlen vv in if not (l >= 1e-12) || isNaN l then (0, 0) else vscale (70 / l) vv
    x = envW env * 0.6
    y = 84
    solPart = case wSol w of
      Just sol ->
        let (zx, zy, zl) = sZ sol
            h = 1e-4
            fd = (fstarAt (wFi w) (wCi w) (wC w + h) (Just (sZ sol)) - fstarAt (wFi w) (wCi w) (wC w - h) (Just (sZ sol))) / (2 * h)
        in [ hud [ ("Minimum by Newton's method (green circle)", cGr)
                 , ("x* = (" ++ fmt 5 zx ++ ", " ++ fmt 5 zy ++ "), f* = " ++ fmt 6 (sFs sol), cInk)
                 , ("lambda = " ++ fmt 6 zl ++ " (L = f - lambda(g - c))", cYe)
                 , ("df*/dc (re-solved at c +- 0.0001) = " ++ fmt 6 fd, cYe)
                 , ("residual |grad f - lambda grad g| + |g - c|", cDim)
                 , ("  = " ++ fmt 3 (sRes sol), cDim)
                 , ("2nd order: tangential Hess L = " ++ fmt 4 (sCurv sol), if sCurv sol > 0 then cGr else cRd)
                 , (if sCurv sol > 0 then "  > 0 -> local minimum" else "  <= 0 -> not a local minimum", if sCurv sol > 0 then cGr else cRd) ] x (y + 112) 14 1.45
           , drawFstar sol (x + 30) (y + 330) (envW env - x - 60) (envH env - y - 400) ]
      Nothing -> [ hud [ ("On this curve f decreases forever", cRd), ("(no minimum)", cRd) ] x (y + 112) 16 1.45 ]
    -- f*(c) のグラフと、傾き λ の接線
    drawFstar sol gx gy gw gh
      | length (wFstar w) < 2 = Blank
      | otherwise =
          let css = map fst (wFstar w)
              fss = map snd (wFstar w)
              c0 = foldl' min (1 / 0) css
              c1 = foldl' max (-1 / 0) css
              f0 = foldl' min (1 / 0) fss
              f1 = foldl' max (-1 / 0) fss + 1e-9
              xS c = gx + d2f ((c - c0) / (if c1 - c0 == 0 then 1 else c1 - c0)) * gw
              yS f = gy + gh - d2f ((f - f0) / (f1 - f0)) * gh
              (_, _, l) = sZ sol
              d = (c1 - c0) * 0.3
              cv = wC w
          in Pictures $
               [ Color cLine (rectLineTL gx gy gw gh)
               , Color cOr (fatLine 2 [(xS c, yS f) | (c, f) <- wFstar w]) ]
               ++ [ Color cYe (thickLine 1.5 a b) | Just (a, b) <- [clipY gy (gy + gh) (xS (cv - d), yS (sFs sol - l * d)) (xS (cv + d), yS (sFs sol + l * d))] ]
               ++ [ Color cYe (at (xS cv, yS (sFs sol)) (disc 4.5))
                  , txt gx (gy - 6) 14 cInk "f*(c) (orange), tangent of slope lambda (yellow)"
                  , txtC (gx + gw / 2) (gy + gh + 12) 14 cDim "c" ]

---------------------------------------------------------------- 操作

nearestT :: World -> Point -> Double
nearestT w (mx, my) = snd (foldl' pick (1e18, wT w) [0 .. nS])
  where
    g = conAt (wCi w)
    (t0, t1) = cTr g
    pick (best, bt) i =
      let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral nS
          (sx, sy) = toSD (wView w) (cPar g t (wC w))
          d = (sx - realToFrac mx) ^ (2 :: Int) + (sy - realToFrac my) ^ (2 :: Int)
      in if d < best then (d, t) else (best, bt)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | inside (wView w) (toP5 env p) -> w { wT = nearestT w (toP5 env p), wSliding = False }
  EventMotion p | envDown env && inside (wView w) (toP5 env p) -> w { wT = nearestT w (toP5 env p) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild env False w { wC = wC w + 0.1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild env False w { wC = max (cCmin (conAt (wCi w))) (wC w - 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSliding = not (wSliding w) }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> let ci = fromEnum c - fromEnum '1'
                             g = conAt ci
                             (t0, t1) = cTr g
                         in rebuild env False w { wCi = ci, wC = cC0 g, wT = t0 + 0.3 * (t1 - t0) }
    | c `elem` "fF" -> rebuild env True w { wFi = 1 - wFi w }
  _ -> w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ let (eg, eh) = derivErr g [(0.7, -1.2), (-1.5, 0.4)]
    in stOk (fnName g ++ ": gradient, Hessian = finite differences") (eg < 1e-6 && eh < 1e-6) ("g " ++ fmt 3 eg ++ ", H " ++ fmt 3 eh)
  | g <- fs ++ map cG cons ]
  -- 閉じた形：x + 2y を x² + y² = r² の上で最小 → x* = −r(1, 2)/√5、λ = −√5/(2r)、f* = −√5 r
  ++ [ let lam = -sqrt 5 / (2 * r)
           ms = solveLagrange (fAt 1) (conAt 0) (r * r) Nothing
           ok = case ms of
             Just s -> let (x, y, l) = sZ s
                       in hypot (x + r / sqrt 5) (y + 2 * r / sqrt 5) < 1e-12 && abs (l - lam) < 1e-12 && abs (sFs s + sqrt 5 * r) < 1e-12
             Nothing -> False
       in stOk ("x + 2y on circle r = " ++ fmt 3 r ++ ": x* = -r(1,2)/sqrt5, lambda = -sqrt5/(2r), f* = -sqrt5 r") ok
            ("lambda predicted " ++ fmt 6 lam ++ " computed " ++ maybe "none" (\s -> let (_, _, l) = sZ s in fmt 6 l) ms)
     | r <- [1, 2, 3] ]
  -- どの制約でも df*/dc（解き直しの差分）= λ、2 次の条件 > 0、∇f ∥ ∇g
  ++ [ let g = conAt b
           c = cC0 g
           h = 1e-4
       in case solveLagrange (fAt a) g c Nothing of
            Nothing -> stOk (fnName (fAt a) ++ ", " ++ fnName (cG g) ++ ": df*/dc = lambda, grad f || grad g, tangential Hess L > 0") False "no solution"
            Just s ->
              let (x, y, l) = sZ s
                  fd = (fstarAt a b (c + h) (Just (sZ s)) - fstarAt a b (c - h) (Just (sZ s))) / (2 * h)
                  gf = fnG (fAt a) x y
                  gg = fnG (cG g) x y
                  cr = abs (fst gf * snd gg - snd gf * fst gg) / (vlen gf * vlen gg + 1e-300)
              in stOk (fnName (fAt a) ++ ", " ++ fnName (cG g) ++ ": df*/dc = lambda, grad f || grad g, tangential Hess L > 0")
                   (abs (fd - l) < 1e-6 * (1 + abs l) && sRes s < 1e-10 && cr < 1e-10 && sCurv s > 0)
                   ("lambda " ++ fmt 6 l ++ " difference " ++ fmt 6 fd)
     | (a, b) <- [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1)] ]
  ++ [ stOk "x + 2y on a line -> judged to have no minimum" (isNothing (solveLagrange (fAt 1) (conAt 2) 1 Nothing)) "" ]

-- ---------- やってみよう ----------
-- 1. 点をドラッグして、ピンクの矢印が消える所を探そう。そこで橙（∇f）と水色（∇g）が同じ直線の上に並ぶ
-- 2. ↑↓ で c を 0.1 ずつ動かし、f* の変化 ÷ 0.1 が λ にほぼ等しいことを確かめよう（経済学では「影の価格」）
-- 3. 円の上で f = x + 2y（F）：最大点でも ∇f ∥ ∇g。2 次の条件で極小と極大を見分ける
-- 4. 波の曲線（4）では ∇f ∥ ∇g になる点がいくつもある。スペースで下ると、どの点に止まるかは始めた場所しだい
-- 5. 制約を 2 つ（g₁ = c₁、g₂ = c₂）にすると ∇f = λ₁∇g₁ + λ₂∇g₂。3 次元で考えてみよう

-- ---------- 道具 ----------
-- 線分 p→q を y の範囲 [y0, y1] に切りつめる
clipY :: Float -> Float -> Point -> Point -> Maybe (Point, Point)
clipY y0 y1 (x1, ya) (x2, yb)
  | t0 > t1 = Nothing
  | otherwise = Just (pt t0, pt t1)
  where
    dy = yb - ya
    (t0, t1) | abs dy < 1e-9 = if ya >= y0 && ya <= y1 then (0, 1) else (1, 0)
             | otherwise = let ta = (y0 - ya) / dy
                               tb = (y1 - ya) / dy
                           in (max 0 (min ta tb), min 1 (max ta tb))
    pt t = (x1 + t * (x2 - x1), ya + t * dy)

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
