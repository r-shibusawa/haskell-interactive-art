-- ============================================
-- 55-04 PV 図と 4 つの過程 ─ 等温・断熱・定圧・定積
-- 理想気体 1 mol（PV = nRT、V はリットル、P は kPa → PV はジュール）。ピストンを動かすと PV 図に線が引かれる
-- 仕事 W = ∫P dV（線の下の面積）を細かく分けて数値で足し、式の値と比べる。熱 Q = ΔU + W（第一法則）
-- 操作：1 等温　2 断熱　3 定圧　4 定積 / ← → 体積（4 では温度）/ PV 図をドラッグ / ↑↓ 気体の種類（γ）/ R 最初から / C 線を消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nMol, rGas :: Double
nMol = 1                   -- ★ 物質量 [mol]
rGas = 8.314               -- 気体定数 [J/(mol·K)]

-- ★ 自由度 f → Cv = f/2·R
gases :: [(String, Double)]
gases = [("monatomic (He, Ar)", 3), ("diatomic (air)", 5), ("polyatomic", 7)]

sub :: Int
sub = 400                  -- ★ 1 回の動きを何個に分けて積分するか

kinds :: [String]
kinds = ["", "isothermal", "adiabatic", "isobaric", "isochoric"]

kindCol :: Int -> Color
kindCol k = case k of
  1 -> rgbC 255 170 80
  2 -> rgbC 120 200 255
  3 -> rgbC 150 230 140
  _ -> rgbC 240 120 200

-- 気体の状態（体積 [L]、温度 [K]）
data St = St { sV, sT :: !Double }

-- 1 つの過程の結果
data Leg = Leg
  { lS :: !St, lPts :: [(Double, Double)]          -- 終わりの状態、線の点 (V, P)
  , lW, lQ, lDU, lDS, lWc, lQc, lDSc :: !Double     -- 数値の W・Q・ΔS と、式の値
  , lK :: !Int, lFrom :: !(Double, Double) }

-- 合計 (W, Q, ΔU, ΔS)
data Sum4 = Sum4 { sumW, sumQ, sumDU, sumDS :: !Double }

data World = World
  { wGas  :: !Int
  , wKind :: !Int
  , wCur  :: !St
  , wLegs :: [Leg]           -- 新しい順
  , wSum  :: !Sum4
  , wDrag :: !Bool
  , wTest :: [(String, Bool)] }

st0 :: St
st0 = St 20 300

main :: IO ()
main = runSketch (sketch "55-04 PV diagram and four processes" world0)
  { skBack  = rgbC 24 22 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 1 st0 [] (Sum4 0 0 0 0) False []

cvOf, gamOf :: Int -> Double
cvOf gi = (snd (getAt ("", 5) gases gi) / 2) * rGas
gamOf gi = 1 + 2 / snd (getAt ("", 5) gases gi)

pOf :: St -> Double
pOf s = (nMol * rGas * sT s) / sV s          -- kPa（J ÷ L）

-- 1 つの過程を nsub 個に分けて進め、W・Q・ΔS を数値で足す（式の値 Wc・Qc も一緒に）
runLeg :: St -> Int -> Double -> Double -> Double -> Int -> Leg
runLeg s k target cv g nsub = Leg (St v2 t2) (reverse pts) w q du ds wc (du + wc) dsc k (v1, p1)
  where
    v1 = sV s; t1 = sT s
    p1 = (nMol * rGas * t1) / v1
    go !i a !w0 !q0 !s0 ps
      | i > nsub = (a, w0, q0, s0, ps)
      | otherwise =
          let b | k == 4 = St (sV s) (t1 + ((target - t1) * fromIntegral i) / fromIntegral nsub)   -- 定積：温度を変える
                | otherwise =
                    let v = v1 + ((target - v1) * fromIntegral i) / fromIntegral nsub
                    in if k == 1 then St v t1
                       else if k == 2 then St v (t1 * (v1 / v) ** (g - 1))                     -- TV^(γ−1) = 一定
                       else St v ((p1 * v) / (nMol * rGas))                                    -- P = 一定
              pa = (nMol * rGas * sT a) / sV a
              pb = (nMol * rGas * sT b) / sV b
              dW = 0.5 * (pa + pb) * (sV b - sV a)                                               -- 台形の面積
              dQ = nMol * cv * (sT b - sT a) + dW                                                -- dQ = dU + dW
              ps' = if i `mod` 10 == 0 || i == nsub then (sV b, pb) : ps else ps                 -- 絵には 10 個に 1 個だけ
          in go (i + 1) b (w0 + dW) (q0 + dQ) (s0 + dQ / (0.5 * (sT a + sT b))) ps'
    (St v2 t2, w, q, ds, pts) = go 1 s 0 0 0 []
    p2 = (nMol * rGas * t2) / v2
    du = nMol * cv * (t2 - t1)
    wc | k == 1 = nMol * rGas * t1 * log (v2 / v1)
       | k == 2 = (p1 * v1 - p2 * v2) / (g - 1)
       | k == 3 = p1 * (v2 - v1)
       | otherwise = 0
    dsc = nMol * cv * log (t2 / t1) + nMol * rGas * log (v2 / v1)

-- 体積（定積では温度）を target まで動かす
move :: Double -> World -> World
move target0 w
  | kind == 2 && let t2 = sT cur * (sV cur / target) ** (gamOf (wGas w) - 1) in t2 > 1200 || t2 < 80 = w
  | kind == 3 && let t2 = (sT cur * target) / sV cur in t2 > 1200 || t2 < 80 = w
  | otherwise =
      let l = runLeg cur kind target (cvOf (wGas w)) (gamOf (wGas w)) sub
          ok = not (null (lPts l)) && abs (lW l) + abs (lQ l) > 1e-9
          legs' = case wLegs w of
            (lst : rest) | lK lst == kind -> merge lst l : rest
            ls -> take 60 (l : ls)
          sm = wSum w
          sm' = Sum4 (sumW sm + lW l) (sumQ sm + lQ l) (sumDU sm + lDU l) (sumDS sm + lDS l)
      in if ok then w { wLegs = legs', wSum = sm', wCur = lS l } else w { wCur = lS l }
  where
    kind = wKind w
    cur = wCur w
    target = if kind == 4 then clampv 80 1200 target0 else clampv 5 60 target0
    merge a b = let ps = lPts a ++ lPts b
                    n = length ps
                    ps' = if n > 3000 then [ p | (j, p) <- zip [0 :: Int ..] ps, even j || j == n - 1 ] else ps
                in a { lPts = ps', lW = lW a + lW b, lQ = lQ a + lQ b, lDU = lDU a + lDU b, lDS = lDS a + lDS b
                     , lWc = lWc a + lWc b, lQc = lQc a + lQc b, lDSc = lDSc a + lDSc b, lS = lS b }

-- PV 図の枠
pvGraph :: Env -> G
pvGraph env = graph 380 110 (envW env - 380 - 360) (envH env - 230) 0 62 0 900

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

step :: Env -> Float -> World -> IO World
step env _ w
  | wDrag w && mx > pX g - 20 && mx < pX g + pW g + 20 =
      let tv = x0 g + realToFrac ((mx - pX g) / pW g) * (x1 g - x0 g)
      in if wKind w == 4
           then let p = y1 g * (1 - realToFrac ((my - pY g) / pH g))
                in pure (move ((clampv 5 900 p * sV (wCur w)) / (nMol * rGas)) w)
           else pure (if abs (tv - sV (wCur w)) > 0.05 then move tv w else w)
  | otherwise = pure w
  where g = pvGraph env
        (mx, my) = toP5 env (envMouse env)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- PV 図：うすく等温線（200〜1000 K）
  [ plotAxes g "volume V [L]" "pressure P [kPa]" [0, 10, 20, 30, 40, 50, 60] [0, 300, 600, 900] ]
  ++ [ Color (rgbaC 255 170 80 40) (Line [ (gX g v, gY g ((nMol * rGas * t) / v)) | i <- [0 .. 59 :: Int], let v = 3 + fromIntegral i ]) | t <- [200, 300 .. 1000] ]
  -- 最後の過程の下の面積 = 仕事（細い台形を並べる）
  ++ (case lastLeg of
        Just l | lK l /= 4 ->
          let ps = lFrom l : thin (lPts l)
          in [ Color (rgbaC 255 220 120 40) (Pictures [ Polygon [(gX g va, gY g 0), (gX g va, gY g pa), (gX g vb, gY g pb), (gX g vb, gY g 0)]
                                                      | ((va, pa), (vb, pb)) <- zip ps (drop 1 ps) ]) ]
        _ -> [])
  ++ [ Color (kindCol (lK l)) (polyline 3 [ (gX g v, gY g p) | (v, p) <- lFrom l : thin (lPts l) ]) | l <- reverse (wLegs w) ]
  ++ [ Color white (at (gX g (sV cur), gY g (pOf cur)) (disc 6))
     , say (getAt "" kinds (wKind w)) (gX g (sV cur) + 10) (gY g (pOf cur) - 10) 14 (kindCol (wKind w)) ]
  -- ---- シリンダーの絵（ピストンの位置 = 体積、気体の色 = 温度）
  ++ [ Color (grayC 200) (thickLine 3 (cx, cy - 20) (cx, cy + ch)), Color (grayC 200) (thickLine 3 (cx + cw, cy - 20) (cx + cw, cy + ch))
     , Color (grayC 200) (thickLine 3 (cx, cy + ch) (cx + cw, cy + ch))
     , Color (makeColor ((80 + 175 * u) / 255) ((120 - 40 * u) / 255) ((255 - 200 * u) / 255) (170 / 255)) (rectTL (cx + 3) px (cw - 6) (cy + ch - px))
     , Color (rgbC 180 185 200) (rectTL (cx + 3) (px - 16) (cw - 6) 16)
     , Color (rgbC 180 185 200) (rectTL (cx + cw / 2 - 8) (cy - 40) 16 (px - 16 - cy + 40)) ]
  ++ [ let q = lQ l
       in say (if q > 1 then "heat flows in  Q > 0" else if q < -1 then "heat flows out  Q < 0" else "no heat exchange  Q = 0") cx (cy + ch + 34) 16
              (if q > 1 then rgbC 255 120 50 else if q < -1 then rgbC 90 170 255 else grayC 120) | Just l <- [lastLeg] ]
  ++ [ say ("V = " ++ fmt 2 (sV cur) ++ " L") cx (cy + ch + 64) 16 (grayC 230)
     , say ("P = " ++ fmt 1 (pOf cur) ++ " kPa") cx (cy + ch + 88) 16 (grayC 230)
     , say ("T = " ++ fmt 1 (sT cur) ++ " K") cx (cy + ch + 112) 16 (grayC 230) ]
  -- ---- 表
  ++ [ say ("Last process (" ++ maybe "-" (getAt "" kinds . lK) lastLeg ++ ")") tx ty 16 (grayC 240) ]
  ++ concat [ [ say nm tx (ty + 28 + fromIntegral k * 22) 14 (grayC 175)
              , say (fmt (if k >= 4 then 4 else 2) v ++ (if k >= 4 then " J/K" else " J")) (tx + 190) (ty + 28 + fromIntegral k * 22) 14 (grayC 235) ]
            | (k, (nm, v)) <- zip [0 :: Int ..] rows ]
  ++ [ say "Total from the start" tx (ty + 180) 16 (grayC 240) ]
  ++ concat [ [ say nm tx (ty + 208 + fromIntegral k * 22) 14 (grayC 175)
              , say (fmt (if k == 4 then 4 else 2) v) (tx + 190) (ty + 208 + fromIntegral k * 22) 14 (grayC 235) ]
            | (k, (nm, v)) <- zip [0 :: Int ..] srows ]
  ++ [ say s tx (ty + 340 + 20 * fromIntegral k) 14 (grayC 170)
     | (k, s) <- zip [0 :: Int ..] [ "Isothermal W = nRT ln(V2/V1)", "Adiabatic W = (P1V1 - P2V2)/(g - 1)", "Isobaric W = P dV   isochoric W = 0"
                                  , "dS = nCv ln(T2/T1) + nR ln(V2/V1)", "Faint lines = isotherms 200-1000 K" ] ]
  ++ [ say ("PV diagram, four processes   now: " ++ getAt "" kinds (wKind w) ++ "   " ++ fst (getAt ("", 5) gases (wGas w))
            ++ "   gamma = " ++ fmt 3 (gamOf (wGas w)) ++ "   Cv = " ++ fmt 1 (cvOf (wGas w) / rGas) ++ "R") 20 36 18 (grayC 240)
     , say ("1 isothermal  2 adiabatic  3 isobaric  4 isochoric   Left/Right " ++ (if wKind w == 4 then "temperature" else "volume") ++ "   drag the PV diagram") 20 62 15 (grayC 175)
     , say "Up/Down gamma   R reset   C clear lines   | yellow area = work W of the last process (V grows -> W > 0)" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    g = pvGraph env
    cur = wCur w
    lastLeg = case wLegs w of
      (l : _) -> Just l
      [] -> Nothing
    -- 点が多いときは間引いて描く（計算はそのまま）
    thin ps = let n = length ps
                  k = max 1 (n `div` 500)
              in if k == 1 then ps else [ p | (j, p) <- zip [0 :: Int ..] ps, j `mod` k == 0 || j == n - 1 ]
    cx = 40; cy = 140; ch = 300; cw = 260
    px = cy + ch - f2 (sV cur / 62) * ch
    u = f2 (clampv 0 1 ((sT cur - 100) / 900))
    tx = envW env - 340
    ty = 130
    rows = case lastLeg of
      Just l -> [ ("W numeric  int P dV", lW l), ("W formula", lWc l), ("dU = nCv dT", lDU l), ("Q = dU + W", lQ l)
                , ("dS numeric  sum dQ/T", lDS l), ("dS formula", lDSc l) ]
      Nothing -> []
    sm = wSum w
    srows = [ ("W (work by gas)", sumW sm), ("Q (heat received)", sumQ sm), ("dU", sumDU sm), ("Q - W", sumQ sm - sumW sm), ("dS", sumDS sm) ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> w { wKind = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> w { wCur = st0, wLegs = [], wSum = Sum4 0 0 0 0 }
    | c `elem` "cC" -> w { wLegs = [] }
  EventKey (SpecialKey KeyRight) Down _ _ -> move (if wKind w == 4 then sT (wCur w) + 25 else sV (wCur w) + 1) w
  EventKey (SpecialKey KeyLeft) Down _ _ -> move (if wKind w == 4 then sT (wCur w) - 25 else sV (wCur w) - 1) w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wGas = min 2 (wGas w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wGas = max 0 (wGas w - 1) }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = True }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("isothermal 10 -> 30 L: numeric W = nRT ln 3 = " ++ fmt 2 (lWc a) ++ " J (relative 1e-4), Q = W"
    , abs (lW a / (rGas * 300 * log 3) - 1) < 1e-4 && abs (lQ a / lW a - 1) < 1e-4)
  , ("adiabatic: PV^gamma constant, numeric W = (P1V1 - P2V2)/(gamma - 1), Q ~ 0"
    , all (\x -> near (x / pv0) 1 1e-12) pv && abs (lW b / lWc b - 1) < 1e-4 && abs (lQ b) < 1e-3 * abs (lW b))
  , ("isobaric: W = P dV, Q = nCp dT (Cp = Cv + R)", near (lW c) (((rGas * 300) / 10) * 15) 1e-6 && near (lQ c) ((cv + rGas) * (sT (lS c) - 300)) 1e-6)
  , ("isochoric: W = 0, Q = nCv dT", lW d == 0 && near (lQ d) (cv * 200) 1e-9)
  , ("entropy is path independent: sum dQ/T (isothermal->isochoric) = (isochoric->isothermal) = formula (1e-4)"
    , near (lDS p1 + lDS p2) (lDS q1 + lDS q2) 1e-4 && near (lDS p1 + lDS p2) (lDSc p1 + lDSc p2) 1e-4)
  , ("rectangle cycle: dU = 0 after one loop -> Q = W = enclosed area " ++ fmt 1 area ++ " J"
    , near (sT (lS e4)) 300 1e-9 && near wcyc area 1e-6 && near qcyc wcyc 1e-6) ]
  where
    cv = 1.5 * rGas
    g = 5 / 3
    s = St 10 300
    a = runLeg s 1 30 cv g 2000
    b = runLeg s 2 30 cv g 2000
    pv = [ p * v ** g | (v, p) <- lPts b ]
    pv0 = case pv of
      (x : _) -> x
      [] -> 1
    c = runLeg s 3 25 cv g 400
    d = runLeg s 4 500 cv g 400
    p1 = runLeg s 1 30 cv g 2000
    p2 = runLeg (lS p1) 4 450 cv g 2000
    q1 = runLeg s 4 450 cv g 2000
    q2 = runLeg (lS q1) 1 30 cv g 2000
    e1 = runLeg s 3 30 cv g 400
    e2 = runLeg (lS e1) 4 150 cv g 400
    e3 = runLeg (lS e2) 3 10 cv g 400
    e4 = runLeg (lS e3) 4 300 cv g 400
    wcyc = lW e1 + lW e2 + lW e3 + lW e4
    qcyc = lQ e1 + lQ e2 + lQ e3 + lQ e4
    area = (((rGas * 300) / 10) - (rGas * 150) / 30) * 20

-- ---------- やってみよう ----------
-- 1. 1（等温）と 2（断熱）で、同じ 20 L → 40 L に広げよう。線の傾きはどちらが急？ W はどちらが大きい？
-- 2. 断熱で圧縮すると温度はどうなる？（ディーゼルエンジンの着火、自転車の空気入れ）
-- 3. ↑↓ で γ を変えて断熱線を比べよう。単原子の気体のほうが急なのはなぜ？
-- 4. 3 → 4 → 3 → 4 で 1 周して元にもどろう。合計の ΔU は？ Q − W は？
-- 5. 同じ 2 つの状態の間を、ちがう道すじで行こう。W と Q は変わるのに、ΔS は変わらない？

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0'
  | v == 0 = "0"
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (2 - e) :: Double
                    r = fromIntegral (round (v * k) :: Integer) / k
                in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""
  where v = fin 0 v0'

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
