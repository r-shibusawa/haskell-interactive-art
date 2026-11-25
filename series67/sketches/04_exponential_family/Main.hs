-- ============================================
-- 67-04 指数型分布族 ─ p(x; θ) = exp(θ·x − ψ(θ))（× 土台の重み）。ψ(θ) は合計を 1 にする「対数分配関数」
-- 自然パラメータ θ と、期待値パラメータ η = E[x] = ∇ψ(θ) の 2 つの座標で同じ分布を表せる
-- ψ の傾きが η、ψ をルジャンドル変換した φ(η) = θ·η − ψ(θ) の傾きが θ。ψ(θ) + φ(η) − θ·η = 0 がいつも成り立つ
-- 操作：1 ベルヌーイ 2 ポアソン 3 3 値のカテゴリ 4 正規分布 / 左（θ）か右（η）をドラッグ：もう片方が動く / スペース 自動で動かす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat, log1p)
import System.Environment (lookupEnv)

-- 分布の族：名前、次元、θ と η の図の範囲、はじめの θ、ψ、η = ∇ψ、φ、θ = ∇φ
data Fam = Fam
  { fName  :: String
  , fDim   :: !Int
  , fTh    :: [Double]
  , fEt    :: [Double]
  , fStart :: [Double]
  , fPsi   :: [Double] -> Double
  , fEta   :: [Double] -> [Double]
  , fPhi   :: [Double] -> Double
  , fTheta :: [Double] -> [Double] }

-- ★ 4 つの族
fams :: [Fam]
fams =
  [ Fam "Bernoulli (coin)" 1 [-5, 5] [0, 1] [0.8]
        (\t -> let a = c0 t in if a > 0 then a + log1p (exp (-a)) else log1p (exp a))
        (\t -> [1 / (1 + exp (-(c0 t)))]) (\e -> xlogx (c0 e) + xlogx (1 - c0 e)) (\e -> [log (c0 e / (1 - c0 e))])
  , Fam "Poisson (counts)" 1 [-3, 2.5] [0, 12] [1]
        (\t -> exp (c0 t)) (\t -> [exp (c0 t)]) (\e -> xlogx (c0 e) - c0 e) (\e -> [log (c0 e)])
  , Fam "3 categories (indicators of x = 1, 2, 3)" 2 [-4, 4, -4, 4] [0, 1, 0, 1] [1, -0.5]
        (\t -> log (1 + exp (c0 t) + exp (c1 t)))
        (\t -> let z = 1 + exp (c0 t) + exp (c1 t) in [exp (c0 t) / z, exp (c1 t) / z])
        (\e -> xlogx (c0 e) + xlogx (c1 e) + xlogx (1 - c0 e - c1 e))
        (\e -> [log (c0 e / (1 - c0 e - c1 e)), log (c1 e / (1 - c0 e - c1 e))])
  , Fam "Normal (counts x and x^2)" 2 [-3, 3, -3, -0.1] [-3, 3, 0, 12] [1, -0.5]
        (\t -> -(c0 t) * c0 t / (4 * c1 t) - 0.5 * log (-2 * c1 t))
        (\t -> [-(c0 t) / (2 * c1 t), c0 t * c0 t / (4 * c1 t * c1 t) - 1 / (2 * c1 t)])
        (\e -> -0.5 - 0.5 * log (c1 e - c0 e * c0 e))
        (\e -> let v = c1 e - c0 e * c0 e in [c0 e / v, -1 / (2 * v)]) ]

c0, c1 :: [Double] -> Double                  -- 0 番目・1 番目の成分（なければ 0）
c0 v = at' v 0 0
c1 v = at' v 1 0

xlogx :: Double -> Double
xlogx v = if v > 0 then v * log v else 0

data Grab = GNone | GL | GR deriving Eq

data World = World
  { wFam   :: !Int
  , wTheta :: [Double]
  , wEta   :: [Double]
  , wGrab  :: !Grab
  , wAuto  :: !Bool
  , wTAuto :: !Double
  , wTest  :: [Check] }

main :: IO ()
main = runSketch (sketch "67-04 Exponential families" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = setTheta (fStart (famOf 0)) (World 0 [0.8] [0] GNone False 0 [])

famOf :: Int -> Fam
famOf k = at' fams k (headOr (Fam "" 1 [] [] [] (const 0) id (const 0) id) fams)

fam :: World -> Fam
fam = famOf . wFam

-- 図の範囲・定義域の中に収める（正規分布は θ2 < 0 が必要なので上限 −0.1）
clampTheta :: Fam -> [Double] -> [Double]
clampTheta f t = clampv (c0 t) (at' (fTh f) 0 0) (at' (fTh f) 1 0)
                 : [ clampv (c1 t) (at' (fTh f) 2 0) (at' (fTh f) 3 0) | fDim f == 2 ]

setTheta :: [Double] -> World -> World
setTheta t w = let f = fam w
                   th = clampTheta f t
               in w { wTheta = th, wEta = fEta f th }

setEta :: [Double] -> World -> World
setEta e0 w =
  let f = fam w
      e | fDim f == 1 = [clampv (c0 e0) (at' (fEt f) 0 0 + 1e-3) (at' (fEt f) 1 0 - 1e-3)]
        | wFam w == 2 = let a = clampv (c0 e0) 0.003 0.994 in [a, clampv (c1 e0) 0.003 (0.997 - a)]
        | otherwise = let m = clampv (c0 e0) (-3) 3 in [m, clampv (c1 e0) (m * m + 0.17) (min 12 (m * m + 5))]   -- σ² = η2 − η1² を θ の範囲に合わせる
  in setTheta (fTheta f e) w

step :: World -> World
step w
  | not (wAuto w) = w
  | otherwise =
      let f = fam w
          th = fTh f
          ta = wTAuto w + 0.01
          cx = 0.5 * (at' th 0 0 + at' th 1 0)
          cy = if fDim f == 2 then 0.5 * (at' th 2 0 + at' th 3 0) else 0
      in setTheta [ cx + 0.35 * (at' th 1 0 - at' th 0 0) * cos ta
                  , if fDim f == 2 then cy + 0.3 * (at' th 3 0 - at' th 2 0) * sin (1.3 * ta) else 0 ] w { wTAuto = ta }

boxL, boxR :: Env -> Box
boxL env = Box 70 110 (d2 (envW env) * 0.4) (d2 (envH env) * 0.5)
boxR env = Box (d2 (envW env) * 0.54) 110 (d2 (envW env) * 0.4) (d2 (envH env) * 0.5)

boxes :: Env -> Fam -> (PBox, PBox)
boxes env f
  | fDim f == 1 =
      let pl = fTh f
          el = fEt f
          ys = [ fPsi f [at' pl 0 0 + (at' pl 1 0 - at' pl 0 0) * i2d k / 100] | k <- [0 .. 100 :: Int] ]
          yt = [ fPhi f [at' el 0 0 + 1e-4 + (at' el 1 0 - at' el 0 0 - 2e-4) * i2d k / 100] | k <- [0 .. 100 :: Int] ]
      in (mk bl (at' pl 0 0) (at' pl 1 0) (minOr 0 ys - 0.5) (maxOr 0 ys + 0.5), mk br (at' el 0 0) (at' el 1 0) (minOr 0 yt - 0.5) (maxOr 0 yt + 0.5))
  | otherwise = (mk bl (at' (fTh f) 0 0) (at' (fTh f) 1 0) (at' (fTh f) 2 0) (at' (fTh f) 3 0), mk br (at' (fEt f) 0 0) (at' (fEt f) 1 0) (at' (fEt f) 2 0) (at' (fEt f) 3 0))
  where bl = boxL env
        br = boxR env
        mk b = plotBox (bxX b) (bxY b) (bxW b) (bxH b)

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Exponential family: theta and eta coordinates  -  " ++ fName f)
             ("1 Bernoulli   2 Poisson   3 categories   4 normal   drag left (theta) or right (eta)   Space auto " ++ (if wAuto w then "ON" else "OFF"))
  , if fDim f == 1 then draw1D f w bL bR else draw2D f w bL bR
  -- ---- 下：数値と分布
  , say ("theta = " ++ fmtP 3 th) 70 (f2 y0) 16 (col orange)
  , say ("eta = grad psi(theta) = " ++ fmtP 3 et ++ "  (finite diff. " ++ fmtP 3 fd ++ ")") 70 (f2 y0 + 26) 16 (col violet)
  , say ("psi(theta) = " ++ fmtN 4 ps ++ ", phi(eta) = " ++ fmtN 4 ph ++ ", theta.eta = " ++ fmtN 4 te) 70 (f2 y0 + 52) 16 (col ink)
  , say ("psi + phi - theta.eta = " ++ jsExp 2 (ps + ph - te) ++ "  (always 0: Legendre)") 70 (f2 y0 + 78) 16 (col lime)
  , drawDist f w (Box (ww * 0.6) (y0 - 6) (ww * 0.34) (d2 (envH env) - y0 - 44))
  , drawSelfTest env (wTest w) ]
  where
    f = fam w
    (bL, bR) = boxes env f
    th = wTheta w
    et = wEta w
    ww = d2 (envW env)
    ps = fPsi f th
    ph = fPhi f et
    te = sumOf (zipWith (*) th et)
    fd = [ (fPsi f (modifyAt i (+ h) th) - fPsi f (modifyAt i (subtract h) th)) / (2 * h) | i <- [0 .. length th - 1], let h = 1e-5 ]
    y0 = pbY bL + pbH bL + 60

-- ψ のグラフと接線（傾き η、切片 −φ）、φ のグラフと接線（傾き θ、切片 −ψ）
draw1D :: Fam -> World -> PBox -> PBox -> Picture
draw1D f w bL bR = Pictures
  [ drawAxes bL "theta (natural parameter)" "psi(theta): tangent slope = eta, intercept = -phi(eta)"
  , drawAxes bR "eta (expectation parameter)" "phi(eta): tangent slope = theta, intercept = -psi(theta)"
  , plotLine bL c1 (col orange) 3, plotLine bR c2 (col violet) 3
  , plotLine bL [(pbX0 bL, ps + e * (pbX0 bL - t)), (pbX1 bL, ps + e * (pbX1 bL - t))] (colA 200 violet) 1.5
  , plotLine bR [(pbX0 bR, ph + t * (pbX0 bR - e)), (pbX1 bR, ph + t * (pbX1 bR - e))] (colA 200 orange) 1.5
  , let (sx, sy) = toScrF bL t ps in Color (col orange) (circleAt sx sy 14)
  , let (sx, sy) = toScrF bR e ph in Color (col violet) (circleAt sx sy 14) ]
  where
    c1 = [ (x, fPsi f [x]) | k <- [0 .. 200 :: Int], let x = pbX0 bL + (pbX1 bL - pbX0 bL) * i2d k / 200 ]
    c2 = [ (x, fPhi f [x]) | k <- [0 .. 200 :: Int], let x = pbX0 bR + 1e-4 + (pbX1 bR - pbX0 bR - 2e-4) * i2d k / 200 ]
    t = c0 (wTheta w)
    e = c0 (wEta w)
    ps = fPsi f (wTheta w)
    ph = fPhi f (wEta w)

-- θ 平面の格子と、それを η へ写した曲がった格子
draw2D :: Fam -> World -> PBox -> PBox -> Picture
draw2D f w bL bR = Pictures $
  [ drawAxes bL "theta1" "theta plane (straight grid)"
  , drawAxes bR "eta1" "eta plane (the same grid mapped by eta = grad psi bends)" ]
  ++ concat [ [ plotLine bL l1 (colA 170 orange) 1.3, plotLine bL l2 (colA 170 violet) 1.3
              , plotLine bR (map (pr . fEta f) l1') (colA 170 orange) 1.3, plotLine bR (map (pr . fEta f) l2') (colA 170 violet) 1.3 ]
            | k <- [0 .. 8 :: Int]
            , let a = th 0 + (th 1 - th 0) * i2d k / 8
            , let b = th 2 + (th 3 - th 2) * i2d k / 8
            , let l1' = [ clampTheta f [a, th 2 + (th 3 - th 2) * i2d j / 120] | j <- [0 .. 120 :: Int] ]
            , let l2' = [ clampTheta f [th 0 + (th 1 - th 0) * i2d j / 120, b] | j <- [0 .. 120 :: Int] ]
            , let l1 = map pr l1'
            , let l2 = map pr l2' ]
  ++ [ let (x, y) = toScrF bL (c0 (wTheta w)) (c1 (wTheta w)) in Color (col orange) (circleAt x y 16)
     , let (x, y) = toScrF bR (c0 (wEta w)) (c1 (wEta w)) in Color (col violet) (circleAt x y 16) ]
  where th i = at' (fTh f) i 0
        pr v = (c0 v, c1 v)

-- いまの分布の形
drawDist :: Fam -> World -> Box -> Picture
drawDist _ w r = Pictures
  [ say "the distribution of this theta (= this eta)" (f2 (bxX r)) (f2 (bxY r)) 14 (col dimc)
  , case wFam w of
      0 -> drawBars [1 - c0 et, c0 et] rr barOpt { boNames = ["x = 0", "x = 1"] }
      1 -> let lam = c0 et
               ps = take 16 (iterate' lam)
           in drawBars ps rr barOpt { boNames = map show [0 .. 15 :: Int], boCols = const teal, boVmax = maxOr 0 ps * 1.15 }
      2 -> drawBars [c0 et, c1 et, 1 - c0 et - c1 et] rr barOpt
      _ -> let m = c0 et
               s = sqrt (max 1e-6 (c1 et - m * m))
               b = plotBox (bxX rr) (bxY rr) (bxW rr) (bxH rr) (-6) 6 0 1.4
               c = [ (x, min 1.4 (gaussPdf x m s)) | k <- [0 .. 200 :: Int], let x = -6 + 12 * i2d k / 200 ]
           in Pictures [drawAxes b ("x   (mu = " ++ fmtN 2 m ++ ", sigma = " ++ fmtN 2 s ++ ")") "", plotLine b c (col teal) 2.5] ]
  where
    et = wEta w
    rr = Box (bxX r) (bxY r + 14) (bxW r) (bxH r - 40)
    -- ポアソン分布 P(x) = e^−λ λ^x / x!（x = 0, 1, 2, … を順にかけて作る）
    iterate' lam = go 0 (exp (-lam))
      where go :: Int -> Double -> [Double]
            go x v = v : go (x + 1) (v * lam / i2d (x + 1))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let g | inRect mx my (boxL env) = GL
          | inRect mx my (boxR env) = GR
          | otherwise = GNone
    in drag w { wGrab = g, wAuto = if g /= GNone then False else wAuto w }
  EventMotion _ -> drag w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = GNone }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> let k = fromEnum c - fromEnum '1' in setTheta (fStart (famOf k)) w { wFam = k }
  _ -> w
  where
    (mx, my) = mouseD env
    drag w' =
      let f = fam w'
          (bL, bR) = boxes env f
      in case wGrab w' of
           GL -> let (u, v) = fromScr bL mx my in setTheta (if fDim f == 1 then [u] else [u, v]) w'
           GR -> let (u, v) = fromScr bR mx my in setEta (if fDim f == 1 then [u] else [u, v]) w'
           GNone -> w'

-- ---------- やってみよう ----------
-- 1. ベルヌーイで θ を大きくすると η はどこまで行く？ ψ のグラフの傾きと比べよう（シグモイド関数）
-- 2. 左の接線の切片が −φ(η) になっているのを確かめよう。右の接線の切片は？
-- 3. 正規分布で、θ 平面のまっすぐな格子が η 平面でどう曲がるか見よう。どの向きに混んでいる？
-- 4. ψ はどれも下に凸。もし凸でなかったら η = ∇ψ(θ) から θ が 1 つに決まらない
-- 5. 次の 05：θ でまっすぐ（e-測地線）と η でまっすぐ（m-測地線）。2 つの「まっすぐ」がある

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("4 つの族で η = ∇ψ(θ)（中心差分、相対誤差 1e-6）", all (\(a, _, _, _) -> a) res)
  , ("逆向きも θ = ∇φ(η)（双対：φ の傾きが θ）", all (\(_, b, _, _) -> b) res)
  , ("θ → η → θ がもとにもどる（誤差 1e-8）", all (\(_, _, c, _) -> c) res)
  , ("ψ(θ) + φ(η) − θ·η = 0（120 点、誤差 1e-10）", all (\(_, _, _, d) -> d) res)
  , ("ルジャンドル変換 φ(η) = max_θ (θη − ψ(θ)) を総当たりで（ベルヌーイ、誤差 1e-6）", abs (best - fPhi bern [e0]) < 1e-6)
  , ("ψ の 2 階微分 = x の分散 = フィッシャー情報（ベルヌーイ η(1−η)、相対誤差 1e-6）", abs (dd2 / (mu * (1 - mu)) - 1) < 1e-6)
  , ("正規分布 N(2, 1.5²)：θ = (μ/σ², −1/(2σ²)) → η = (μ, μ² + σ²) = (2, 6.25)", abs (c0 en - 2) < 1e-12 && abs (c1 en - 6.25) < 1e-12) ]
  where
    -- 種 4 で、族ごとに 30 点（p5.js 版と同じ順に乱数を引く）
    pts = withSeed 4 (mapM (\(fi, f) -> mapM (\_ -> do
                                                u <- rnd
                                                let t0 = at' (fTh f) 0 0 * 0.6 + u * (at' (fTh f) 1 0 - at' (fTh f) 0 0) * 0.6
                                                if fDim f == 2
                                                  then do v <- rnd
                                                          pure (f, [t0, if fi == (3 :: Int) then -0.2 - v * 2.7 else -3 + v * 6])
                                                  else pure (f, [t0])) [1 .. 30 :: Int]) (zip [0 ..] fams))
    res = map testOne (concat pts)
    testOne (f, t) =
      let e = fEta f t
          h = 1e-5
          grads = [ abs ((fPsi f (modifyAt i (+ h) t) - fPsi f (modifyAt i (subtract h) t)) / (2 * h) - at' e i 0) <= 1e-6 * (1 + abs (at' e i 0))
                  | i <- [0 .. fDim f - 1] ]
          duals = [ abs ((fPhi f (modifyAt i (+ hh) e) - fPhi f (modifyAt i (subtract hh) e)) / (2 * hh) - at' t i 0) <= 1e-4 * (1 + abs (at' t i 0))
                  | i <- [0 .. fDim f - 1], let hh = 1e-6 * (1 + abs (at' e i 0)) ]
          t2 = fTheta f e
          inv = and [ not (abs (v - at' t i 0) > 1e-8 * (1 + abs (at' t i 0))) | (i, v) <- zip [0 ..] t2 ]
          leg = not (abs (fPsi f t + fPhi f e - sumOf (zipWith (*) t e)) > 1e-10 * (1 + abs (fPsi f t)))
      in (and grads, and duals, inv, leg)
    bern = famOf 0
    e0 = 0.3
    best = foldl' max (-1 / 0) [ t * e0 - fPsi bern [t] | k <- [0 .. 20000 :: Int], let t = -6 + 12 * i2d k / 20000 ]
    t0' = 0.7
    h2 = 1e-4
    dd2 = (fPsi bern [t0' + h2] - 2 * fPsi bern [t0'] + fPsi bern [t0' - h2]) / (h2 * h2)
    mu = c0 (fEta bern [t0'])
    en = fEta (famOf 3) [2 / (1.5 * 1.5), -1 / (2 * (1.5 * 1.5))]

-- v.toExponential(d)（JavaScript と同じ形：1.11e-16、0.00e+0）
jsExp :: Int -> Double -> String
jsExp d v
  | isNaN v || isInfinite v = fmtN 2 v
  | otherwise = let s = showEFloat (Just d) v ""
                    (m, e) = break (== 'e') s
                in case e of
                     ('e' : '-' : ds) -> m ++ "e-" ++ ds
                     ('e' : ds) -> m ++ "e+" ++ ds
                     _ -> s

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 棒グラフ（ドラッグで高さを変えられる）
data BarOpt = BarOpt { boCols :: Int -> RGB, boNames :: [String], boVmax :: !Double, boGhost :: [Double] }

barOpt :: BarOpt                              -- 既定：結果の色、名前は 1 2 3 …、上限 1、重ねる分布なし
barOpt = BarOpt outC [] 1 []

drawBars :: [Double] -> Box -> BarOpt -> Picture
drawBars p r opt = Pictures $
  Color (col wire) (lineW 1 (f2 (bxX r), f2 (bxY r + bxH r)) (f2 (bxX r + bxW r), f2 (bxY r + bxH r)))
  : concat [ [ Color (colA 210 (boCols opt i)) (rectTL (x + i2f i * bw + 3) (y + h - hh) (bw - 6) hh)
             , if null (boGhost opt) then Blank
               else let gh = f2 (clampv (at' (boGhost opt) i 0) 0 vmax / vmax) * h
                    in Color (col ink) (frameTL 1.5 (x + i2f i * bw + 3) (y + h - gh) (bw - 6) gh)
             , if bw >= 34 then sayC (fmtN 2 v) (x + (i2f i + 0.5) * bw) (y + h - hh - 4) 14 (col ink) else Blank   -- 細い棒では数字を省く
             , sayC (if null (boNames opt) then show (i + 1) else at' (boNames opt) i "") (x + (i2f i + 0.5) * bw) (y + h + 17) 14 (col dimc) ]
           | (i, v) <- zip [0 ..] p, let hh = f2 (clampv v 0 vmax / vmax) * h ]
  where k = length p
        x = f2 (bxX r)
        y = f2 (bxY r)
        h = f2 (bxH r)
        bw = f2 (bxW r) / i2f (max 1 k)
        vmax = boVmax opt

-- 棒をドラッグ：i 番目の高さを変えて、残りを比例で縮める（合計 1 のまま）
barsDrag :: [Double] -> Box -> Double -> Double -> Double -> [Double]
barsDrag p r mx my fl0 =
  let k = length p
      fl = if fl0 == 0 then 0.001 else fl0
      i = floor ((mx - bxX r) / (bxW r / i2d k)) :: Int
      v = clampv ((bxY r + bxH r - my) / bxH r) fl (1 - fl * i2d (k - 1))
      rest = 1 - at' p i 0
      q = [ if j == i then v else if rest > 1e-9 then (pj / rest) * (1 - v) else (1 - v) / i2d (k - 1) | (j, pj) <- zip [0 ..] p ]
  in if i < 0 || i >= k || my < bxY r - 30 || my > bxY r + bxH r + 20 then p else mixFloor q fl

-- ---- 正規分布 N(μ, σ) の道具。フィッシャー計量 ds² = (dμ² + 2dσ²) / σ²
gaussPdf :: Double -> Double -> Double -> Double
gaussPdf x m s = exp (-((x - m) * (x - m)) / (2 * s * s)) / (s * sqrt (2 * pi))

-- フィッシャー-ラオ距離（閉じた式）
gaussFR :: Double -> Double -> Double -> Double -> Double
gaussFR m1 s1 m2 s2 =
  let a = (m1 - m2) * (m1 - m2) / 2 + (s1 - s2) * (s1 - s2)
  in sqrt 2 * acosh (1 + a / (2 * s1 * s2))

-- D(N1‖N2)
gaussKL :: Double -> Double -> Double -> Double -> Double
gaussKL m1 s1 m2 s2 = log (s2 / s1) + (s1 * s1 + (m1 - m2) * (m1 - m2)) / (2 * s2 * s2) - 0.5

-- ★ 測地線：u = μ/√2 と置くと (u, σ) では半円（中心は σ = 0 の上）→ (μ, σ) では横に √2 倍の半楕円
gaussGeodesic :: Double -> Double -> Double -> Double -> Int -> [(Double, Double)]
gaussGeodesic m1 s1 m2 s2 n
  | abs (u1 - u2) < 1e-9 * (1 + abs u1) = [ (m1, s1 * (s2 / s1) ** (i2d k / i2d n)) | k <- [0 .. n] ]
  | otherwise = [ ((c + rr * cos t) * sqrt 2, rr * sin t) | k <- [0 .. n], let t = t1 + (t2 - t1) * i2d k / i2d n ]
  where u1 = m1 / sqrt 2
        u2 = m2 / sqrt 2
        c = (u2 * u2 + s2 * s2 - u1 * u1 - s1 * s1) / (2 * (u2 - u1))
        rr = sqrt ((u1 - c) * (u1 - c) + s1 * s1)
        t1 = atan2 s1 (u1 - c)
        t2 = atan2 s2 (u2 - c)

-- 折れ線の長さをフィッシャー計量で（区間の真ん中の σ を使う）
metricLength :: [(Double, Double)] -> Double
metricLength path = sumOf [ sqrt (dm * dm + 2 * ds * ds) / sm
                          | ((a0, b0), (a1, b1)) <- zip path (drop 1 path), let dm = a1 - a0, let ds = b1 - b0, let sm = (b1 + b0) / 2 ]

-- 測地線の方程式（クリストッフェル記号から）：μ'' = 2μ'σ'/σ、σ'' = −μ'²/(2σ) + σ'²/σ を RK4 で解く
geodesicRK4 :: Double -> Double -> Double -> Double -> Double -> Int -> [(Double, Double)]
geodesicRK4 m s vm vs tT n = go n (m, s, vm, vs) [(m, s)]
  where
    h = tT / i2d n
    fd (_, b, c, d) = (c, d, 2 * c * d / b, -(c * c) / (2 * b) + d * d / b)
    ad (a, b, c, d) k (e, f, g, hh) = (a + k * e, b + k * f, c + k * g, d + k * hh)
    go 0 _ acc = reverse acc
    go j st acc =
      let k1 = fd st
          k2 = fd (ad st (h / 2) k1)
          k3 = fd (ad st (h / 2) k2)
          k4 = fd (ad st h k3)
          (a, b, c, d) = st
          (e1, f1, g1, h1) = k1
          (e2, f2', g2, h2) = k2
          (e3, f3, g3, h3) = k3
          (e4, f4, g4, h4) = k4
          a' = a + (h / 6) * (e1 + 2 * e2 + 2 * e3 + e4)
          b0' = b + (h / 6) * (f1 + 2 * f2' + 2 * f3 + f4)
          c' = c + (h / 6) * (g1 + 2 * g2 + 2 * g3 + g4)
          d' = d + (h / 6) * (h1 + 2 * h2 + 2 * h3 + h4)
          b' = if b0' > 1e-6 then b0' else 1e-6
      in a' `seq` b' `seq` c' `seq` d' `seq` go (j - 1) (a', b', c', d') ((a', b') : acc)

-- ---- 2 次元の図の枠（データ座標 ↔ 画面）
data PBox = PBox { pbX :: !Double, pbY :: !Double, pbW :: !Double, pbH :: !Double, pbX0 :: !Double, pbX1 :: !Double, pbY0 :: !Double, pbY1 :: !Double }

plotBox :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PBox
plotBox = PBox

pbBox :: PBox -> Box                          -- 枠の長方形（inRect 用）
pbBox b = Box (pbX b) (pbY b) (pbW b) (pbH b)

toScr :: PBox -> Double -> Double -> (Double, Double)
toScr b u v = (pbX b + (u - pbX0 b) / (pbX1 b - pbX0 b) * pbW b, pbY b + pbH b - (v - pbY0 b) / (pbY1 b - pbY0 b) * pbH b)

toScrF :: PBox -> Double -> Double -> (Float, Float)
toScrF b u v = let (x, y) = toScr b u v in (f2 x, f2 y)

fromScr :: PBox -> Double -> Double -> (Double, Double)
fromScr b sx sy = (pbX0 b + (sx - pbX b) / pbW b * (pbX1 b - pbX0 b), pbY0 b + (pbY b + pbH b - sy) / pbH b * (pbY1 b - pbY0 b))

drawAxes :: PBox -> String -> String -> Picture
drawAxes b xl yl = Pictures
  [ Color (col wire) (frameTL 1 x y w h)
  , sayT xl (x + w / 2) (y + h + 16) 14 (col dimc) True
  , sayT (fmtN 1 (pbX0 b)) x (y + h + 2) 14 (col dimc) False
  , sayR (fmtN 1 (pbX1 b)) (x + w) (y + h + 14) 14 (col dimc)
  , sayRM (fmtN 1 (pbY0 b)) (x - 4) (y + h) 14 (col dimc)
  , sayRM (fmtN 1 (pbY1 b)) (x - 4) y 14 (col dimc)
  , say yl x (y - 5) 14 (col dimc) ]
  where x = f2 (pbX b)
        y = f2 (pbY b)
        w = f2 (pbW b)
        h = f2 (pbH b)

-- データ座標の点列を折れ線に（枠の外の部分は枠のふちで切る）
plotLine :: PBox -> [(Double, Double)] -> Color -> Float -> Picture
plotLine b pts c w = Color c (Pictures (map (polyline w . map (\(x, y) -> (f2 x, f2 y))) (clipRuns b pts)))

-- 枠の中に入る部分の折れ線の列（リャン-バースキー法で線分を切る）
clipRuns :: PBox -> [(Double, Double)] -> [[(Double, Double)]]
clipRuns b pts = finish (foldl' stepP ([], [], Nothing) pts)
  where
    x0 = pbX b - 1
    x1 = pbX b + pbW b + 1
    y0 = pbY b - 1
    y1 = pbY b + pbH b + 1
    flush run done = if length run >= 2 then reverse run : done else done
    finish (run, done, _) = reverse (flush run done)
    clip (ax, ay) (cx, cy) =
      let dx = cx - ax
          dy = cy - ay
          go (t0, t1) (p, q)
            | p == 0 = if q < 0 then Nothing else Just (t0, t1)
            | otherwise = let r = q / p in Just (if p < 0 then (max t0 r, t1) else (t0, min t1 r))
          res = foldl' (\acc pq -> acc >>= \tt -> go tt pq) (Just (0, 1)) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      in case res of
           Just (t0, t1) | t0 <= t1 -> Just (t0, t1)
           _ -> Nothing
    stepP (run, done, prev) (u, v) =
      let s@(sx, sy) = toScr b u v
          good = not (isNaN sx || isNaN sy || isInfinite sx || isInfinite sy)
      in if not good then ([], flush run done, Nothing) else
         case prev of
           Nothing -> (run, done, Just s)
           Just pv@(px, py) -> case clip pv s of
             Nothing -> ([], flush run done, Just s)
             Just (t0, t1) ->
               let pa = (px + t0 * (sx - px), py + t0 * (sy - py))
                   pb = (px + t1 * (sx - px), py + t1 * (sy - py))
                   (run1, done1) = if t0 > 0 then ([pa], flush run done) else if null run then ([pa], done) else (run, done)
                   run2 = pb : run1
               in if t1 < 1 then ([], flush run2 done1, Just s) else (run2, done1, Just s)

-- 長方形の上の等高線の地図（fn(u, v)）
gridField :: PBox -> (Double -> Double -> Double) -> FieldOpt -> Field
gridField b fn opt =
  let res = if foRes opt > 0 then foRes opt else 3
      gw = ceiling (pbW b / i2d res) :: Int
      gh = ceiling (pbH b / i2d res) :: Int
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            (u, v) = fromScr b (pbX b + (i2d i + 0.5) * i2d res) (pbY b + (i2d j + 0.5) * i2d res)
        in fn u v
      lv = foLevel opt
      vmin = foVmin opt
      vmax = foVmax opt
      bandOf v = floor (clampv ((v - vmin) / lv) (-1e6) 1e6) :: Int
      pic = bandImage gw gh (U.map (\v -> if isNaN v then -1e300 else v) vals) bandOf (\v -> (v - vmin) / (vmax - vmin)) opt 235 170
  in vals `seq` Field pic (f2 (pbX b)) (f2 (pbY b)) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
