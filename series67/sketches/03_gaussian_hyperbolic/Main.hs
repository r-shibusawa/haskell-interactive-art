-- ============================================
-- 67-03 正規分布の族は双曲平面 ─ 正規分布 N(μ, σ) を (μ, σ) の上半平面の点として並べ、フィッシャー情報で長さを測る
-- フィッシャー計量 ds² = (dμ² + 2dσ²) / σ²：σ が小さい（とがった）分布ほど、同じ Δμ でも「遠い」（区別しやすい）
-- 最短路（測地線）は横に √2 倍した半円 = 半楕円。距離 d = √2·arccosh(1 + (Δμ²/2 + Δσ²)/(2σ1σ2))
-- 測地線の方程式を RK4 で解いた道（点線）と、半楕円の長さの数値積分が、閉じた式とぴったり合う（ポアンカレ円板はシリーズ 28 の 09）
-- 操作：ドラッグ A・B を動かす / E 等距離の円 / G 分布の形を並べる / 1 σ が小さい所で比べる 2 σ が大きい所で比べる / R もどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

muRange, sgRange :: (Double, Double)
muRange = (-4, 4)                             -- ★ 図の範囲（μ）
sgRange = (0, 3)                              -- ★ 図の範囲（σ）

type Pt = (Double, Double)

data Grab = GNone | GA | GB deriving Eq

-- A と B が決まると決まるもの（A・B が動いたときだけ計算し直す）
data Geo = Geo { gKey :: (Pt, Pt), gD :: !Double, gPath :: [Pt], gRK :: [Pt], gLgeo :: !Double, gLline :: !Double }

data World = World
  { wA       :: !Pt
  , wB       :: !Pt
  , wGrab    :: !Grab
  , wShowEq  :: !Bool
  , wGlyph   :: !Bool
  , wField   :: Maybe Field                   -- A からの距離の等高線
  , wFKey    :: (Pt, (Float, Float))
  , wGeo     :: Maybe Geo
  , wTest    :: [Check] }

main :: IO ()
main = runSketch (sketch "67-03 Gaussian family is hyperbolic" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (refresh env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (refresh env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World (-1, 0.6) (1.5, 1.2) GNone True True Nothing ((0, 0), (0, 0)) Nothing []

pbOf :: Env -> PBox
pbOf env = plotBox 70 100 (d2 (envW env) * 0.6) (d2 (envH env) - 190) (fst muRange) (snd muRange) (fst sgRange) (snd sgRange)

-- 等高線と測地線を、必要なときだけ作り直す
refresh :: Env -> World -> World
refresh env w =
  let fk = (wA w, (envW env, envH env))
      (am, as) = wA w
      w1 = if fk == wFKey w && hasField w then w
           else w { wFKey = fk
                  , wField = Just (gridField (pbOf env) (\m s -> if s > 0.005 then gaussFR am as m s else 9)
                                             fieldOpt { foLevel = 0.5, foVmax = 5, foCap = 4.6, foC0 = (60, 85, 125), foC1 = (28, 38, 60), foRes = 4 }) }
      gk = (wA w, wB w)
  in case wGeo w1 of
       Just g | gKey g == gk -> w1
       _ -> w1 { wGeo = Just (makeGeo (wA w) (wB w)) }
  where hasField w' = case wField w' of
          Just _ -> True
          Nothing -> False

makeGeo :: Pt -> Pt -> Geo
makeGeo a@(am, as) b@(bm, bs) =
  let d = gaussFR am as bm bs
      lline = metricLength [ (am + (bm - am) * i2d k / 4000, as + (bs - as) * i2d k / 4000) | k <- [0 .. 4000 :: Int] ]
  in Geo (a, b) d (gaussGeodesic am as bm bs 400) (shoot a b 400) (metricLength (gaussGeodesic am as bm bs 4000)) lline

-- ★ A から B への測地線の向き（半楕円の接線）を、計量で長さ 1 にした速度にする
initialVelocity :: Pt -> Pt -> (Double, Double)
initialVelocity (am, as) (bm, bs) =
  let u1 = am / sqrt 2
      u2 = bm / sqrt 2
      (vm, vs) = if abs (u1 - u2) < 1e-12 then (0, if bs > as then 1 else -1)
                 else let c = (u2 * u2 + bs * bs - u1 * u1 - as * as) / (2 * (u2 - u1))
                          t1 = atan2 as (u1 - c)
                          t2 = atan2 bs (u2 - c)
                          sg = if t2 > t1 then 1 else -1
                      in (-(sin t1) * sg * sqrt 2, cos t1 * sg)
      sp = sqrt (vm * vm + 2 * vs * vs) / as
  in (vm / sp, vs / sp)

shoot :: Pt -> Pt -> Int -> [Pt]
shoot a@(am, as) b@(bm, bs) n = let (vm, vs) = initialVelocity a b in geodesicRK4 am as vm vs (gaussFR am as bm bs) n

-- A から距離 d の点の集まり：(u, σ) では中心 (u0, σ0 cosh(d/√2))、半径 σ0 sinh(d/√2) の円
eqCircle :: Pt -> Double -> [Pt]
eqCircle (cm, cs) d =
  let r = d / sqrt 2
      cy = cs * cosh r
      rr = cs * sinh r
  in [ (cm + sqrt 2 * rr * cos t, cy + rr * sin t) | k <- [0 .. 120 :: Int], let t = 2 * pi * i2d k / 120 ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawField (wField w)
  , hudTitle "Normal family = hyperbolic plane: (mu, sigma) with the Fisher metric"
             ("Drag A / B   E equal-distance circles " ++ onOff (wShowEq w) ++ "   G distribution shapes " ++ onOff (wGlyph w)
              ++ "   1 small sigma   2 large sigma   R reset")
  , drawAxes pb "mu (mean)" "sigma (std. dev.)   background = Fisher-Rao distance from A (every 0.5)" ]
  -- その点の正規分布の形（高さをそろえた形、幅 = σ）
  ++ (if wGlyph w
        then [ Color (colA 120 sand) (polyline 1 [ (sx, f2 cy - 24 * f2 (exp (-((x - m) * (x - m)) / (2 * s * s))))
                                                  | k <- [0 .. 30 :: Int], let x = m - 0.65 + 1.3 * i2d k / 30, let sx = fst (toScrF pb x 0) ])
             | m <- [-3, -1.5, 0, 1.5, 3], s <- [0.35, 0.9, 1.8, 2.6], let (_, cy) = toScr pb m s ]
        else [])
  ++ (if wShowEq w then [ plotLine pb (eqCircle (wA w) d) (colA 170 teal) 1.2 | d <- [0.5, 1, 1.5, 2] ] else [])
  ++ [ plotLine pb [wA w, wB w] (colA 160 dimc) 1.5        -- ふつうの直線（最短ではない）
     , plotLine pb geoPath (col orange) 3.5
     , Color (col violet) (Pictures (map (dashedRun 2 6 6) (clipRuns pb rk))) ]
  ++ [ Pictures [ Color (col bgC) (circleAt x y 20), Color (col c) (circleAt x y 16), sayLM nm (x + 10) (y - 12) 16 (col c) ]
     | ((pm, ps), nm, c) <- [(wA w, "A", orange), (wB w, "B", violet)], let (x, y) = toScrF pb pm ps ]
  -- ---- 右：数値
  ++ [ say ("A = N(" ++ fmtN 2 am ++ ", " ++ fmtN 2 as ++ "^2)") rx 130 16 (col ink)
     , say ("B = N(" ++ fmtN 2 bm ++ ", " ++ fmtN 2 bs ++ "^2)") rx 154 16 (col ink)
     , say ("closed form d = " ++ fmtN 5 dAB) rx 190 16 (col orange)
     , say ("half-ellipse length = " ++ fmtN 5 lgeo) rx 214 16 (col orange)
     , say "  (numerical integral)" rx 232 14 (col orange)
     , say "end of the RK4 geodesic shot" rx 256 16 (col violet)
     , say ("  (" ++ fmtN 4 (fst end) ++ ", " ++ fmtN 4 (snd end) ++ ")") rx 278 16 (col violet)
     , say ("straight segment = " ++ fmtN 5 lline) rx 306 16 (col dimc)
     , say "  (longer)" rx 324 14 (col dimc)
     , say "green circles = distance 0.5, 1, 1.5, 2" rx 348 14 (col teal)
     , say "from A; bigger higher up" rx 366 14 (col teal)
     , say "vs KL: for close points D ~ 1/2 d^2" rx 390 14 (col ink)
     , say ("D(A||B) = " ++ fmtN 4 (gaussKL am as bm bs) ++ ", 1/2 d^2 = " ++ fmtN 4 (0.5 * dAB * dAB)) rx 410 14 (col dimc)
     -- 2 つの分布の形
     , drawAxes b2 "x" "the two normal distributions"
     , plotLine b2 ca (col orange) 2, plotLine b2 cb (col violet) 2
     , drawSelfTest env (wTest w) ]
  where
    pb = pbOf env
    onOff b = if b then "ON" else "OFF"
    (am, as) = wA w
    (bm, bs) = wB w
    geo = maybe (makeGeo (wA w) (wB w)) id (wGeo w)
    dAB = gD geo
    geoPath = gPath geo
    rk = gRK geo
    lgeo = gLgeo geo
    lline = gLline geo
    end = lastOr (0, 0) rk
    rxD = pbX pb + pbW pb + 40
    rx = f2 rxD
    b2 = plotBox rxD 440 (d2 (envW env) - rxD - 40) 150 (-5) 5 0 1.3
    ca = [ (x, min 1.3 (gaussPdf x am as)) | k <- [0 .. 200 :: Int], let x = -5 + i2d k / 20 ]
    cb = [ (x, min 1.3 (gaussPdf x bm bs)) | k <- [0 .. 200 :: Int], let x = -5 + i2d k / 20 ]

-- 点線の折れ線（画面の点の列。長さ on の線と off のすき間をくり返す。setLineDash の代わり）
dashedRun :: Float -> Float -> Float -> [(Double, Double)] -> Picture
dashedRun wt on off pts = Pictures (go 0 (map (\(x, y) -> (f2 x, f2 y)) pts))
  where
    per = on + off
    go :: Float -> [(Float, Float)] -> [Picture]
    go s ((x1, y1) : rest@((x2, y2) : _)) =
      let l = hyp (x2 - x1) (y2 - y1)
          seg a b = let t0 = a / max l 1e-9
                        t1 = b / max l 1e-9
                    in lineW wt (x1 + (x2 - x1) * t0, y1 + (y2 - y1) * t0) (x1 + (x2 - x1) * t1, y1 + (y2 - y1) * t1)
          -- この線分の中の「線」の部分（s = 線分の始まりの、模様の中の位置）
          pieces = [ seg (max 0 a) (min l b) | k <- [0 .. ceiling ((s + l) / per) :: Int]
                   , let a = i2f k * per - s, let b = a + on, b > 0, a < l ]
      in pieces ++ go (s + l - per * i2f (floor ((s + l) / per) :: Int)) rest
    go _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (ax, ay) = toScr pb (fst (wA w)) (snd (wA w))
        (bx, by) = toScr pb (fst (wB w)) (snd (wB w))
        dd x y = sqrt ((mx - x) ^ (2 :: Int) + (my - y) ^ (2 :: Int))
    in w { wGrab = if dd ax ay < 20 then GA else if dd bx by < 20 then GB else GNone }
  EventMotion _ | wGrab w /= GNone ->
    let (m, s) = fromScr pb mx my
        p = (clampv m (fst muRange) (snd muRange), clampv s 0.08 (snd sgRange))
    in if wGrab w == GA then w { wA = p } else w { wB = p }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = GNone }
  EventKey (Char c) Down _ _
    | c == 'e' || c == 'E' -> w { wShowEq = not (wShowEq w) }
    | c == 'g' || c == 'G' -> w { wGlyph = not (wGlyph w) }
    | c == '1' -> w { wA = (-1, 0.3), wB = (1, 0.3) }
    | c == '2' -> w { wA = (-1, 2), wB = (1, 2) }
    | c == 'r' || c == 'R' -> w { wA = (-1, 0.6), wB = (1.5, 1.2) }
  _ -> w
  where pb = pbOf env
        (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. 1 と 2 を押して、同じ Δμ = 2 の 2 点の距離を比べよう。なぜ σ が小さいと遠い？（分布の形を見る）
-- 2. A を下（σ → 0）へ近づけると、等距離の円はどうなる？ σ = 0 の線は「無限に遠い」
-- 3. 測地線はなぜ上に膨らむ？ 「一度ぼかしてから移動する」ほうが近道になる理由を考えよう
-- 4. 計量の 2 を 1 に変えると、ふつうの双曲平面（シリーズ 28 の 09）と同じになる。半楕円は半円になる
-- 5. 平均だけを動かす族（σ 固定）は直線、σ だけの族は縦の直線。どちらも測地線？

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("閉じた式 √2·arccosh(…) = 半楕円の長さの数値積分（12 組、最大誤差 " ++ jsExp1 worst ++ " ≤ 1e-4）", all (\(e, _, _) -> e <= 1e-4) stats)
  , ("測地線の方程式を RK4 で長さ d だけ進むと B に着く（12 組、誤差 1e-4）", all (\(_, s, _) -> s) stats)
  , ("まっすぐな線分は測地線より長い（12 組）", all (\(_, _, l) -> l) stats)
  , ("同じ Δμ = 2 でも σ = 0.3 では d = " ++ fmtN 3 (gaussFR (-1) 0.3 1 0.3) ++ "、σ = 2 では d = " ++ fmtN 3 (gaussFR (-1) 2 1 2)
    , gaussFR (-1) 0.3 1 0.3 > 3 * gaussFR (-1) 2 1 2)
  , ("等距離の円の上の 121 点はどれも距離 1.3（誤差 1e-9）", all (\(m, s) -> abs (gaussFR 0.5 0.8 m s - 1.3) < 1e-9) (eqCircle (0.5, 0.8) 1.3))
  , ("近い 2 点で KL ≈ ½ ds²（フィッシャー計量そのもの、相対誤差 1e-2）", abs (kl / (0.5 * ds2) - 1) < 1e-2)
  , ("σ だけちがう 2 点の距離 = √2 |log(σ2/σ1)|（縦の直線が測地線）", abs (gaussFR 0 0.5 0 2 - sqrt 2 * log 4) < 1e-12) ]
  where
    pairs = withSeed 8 (mapM (\_ -> do a1 <- rnd
                                       a2 <- rnd
                                       b1 <- rnd
                                       b2 <- rnd
                                       pure ((a1 * 6 - 3, 0.2 + a2 * 2), (b1 * 6 - 3, 0.2 + b2 * 2))) [1 .. 12 :: Int])
    stats = [ (abs (l - d), okS, ls >= d - 1e-6)
            | (a@(am, as), b@(bm, bs)) <- pairs
            , let d = gaussFR am as bm bs
            , let l = metricLength (gaussGeodesic am as bm bs 6000)
            , let (ex, ey) = lastOr (0 / 0, 0 / 0) (shoot a b 2000)
            , let okS = sqrt ((ex - bm) * (ex - bm) + (ey - bs) * (ey - bs)) <= 1e-4
            , let ls = metricLength [ (am + (bm - am) * i2d i / 3000, as + (bs - as) * i2d i / 3000) | i <- [0 .. 3000 :: Int] ] ]
    worst = foldl' max 0 [ e | (e, _, _) <- stats ]
    h = 1e-3
    kl = gaussKL 0.3 0.7 (0.3 + h) (0.7 + h)
    ds2 = (h * h + 2 * h * h) / (0.7 * 0.7)

-- v.toExponential(1)（JavaScript と同じ形：1.2e-5、1.2e+0）
jsExp1 :: Double -> String
jsExp1 v = let s = showEFloat (Just 1) v ""
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
