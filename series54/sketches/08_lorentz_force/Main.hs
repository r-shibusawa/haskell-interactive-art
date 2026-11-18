-- ============================================
-- 54-08 ローレンツ力 ─ 磁場の中の荷電粒子は円をえがく
-- 力 F = q(E + v × B)。磁場だけなら力はいつも速さに垂直 → 速さは変わらず円運動（半径 r = mv/(qB)、周期 T = 2πm/(qB)）
-- 計算はボリス法：電場で半分加速 → 磁場で「回転」→ 電場で半分加速。磁場だけなら速さがきっちり保存される
-- 操作：1 円運動 / 2 らせん / 3 E×B ドリフト / 4 質量分析器 / クリック = 粒子を打ち出す / ↑↓ = B
--       ←→ = 速さ（3 は E、4 は電圧）/ F = 電荷の符号 / C = 消す / スペース = 止める / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

qe, mp, uKg :: Double
qe = 1.602e-19                         -- 電気素量 [C]
mp = 1.673e-27                         -- 陽子の質量 [kg]
uKg = 1.661e-27                        -- 原子質量単位 [kg]

pxm :: Double
pxm = 1000                             -- ★ 1 m = 1000 px（1 px = 1 mm）

timeK :: Double
timeK = 2e-6                           -- ★ 画面の 1 秒 = 2 μs（とても速いのでスローで見る）

stepsPerTurn :: Double
stepsPerTurn = 200                     -- ★ 1 周を何歩で計算するか

masses :: [Double]
masses = [12, 13, 14]                  -- ★ 質量分析器に入れるイオン（原子質量単位 u、電荷 +e）

type V3d = (Double, Double, Double)

-- 粒子：位置 [m]、速度 [m/s]、質量 [kg]、電荷 [C]、通った跡（画面の座標）
data P = P { pX, pY, pZ, pVx, pVy, pVz, pM, pQ :: !Double, pTrail :: !(Seq.Seq Point) }

data World = World
  { wMode    :: !Int
  , wBm, wSpeed, wE, wVacc, wBspec, wSign :: !Double
  , wPaused  :: !Bool
  , wParts   :: [P]
  , wHits    :: [Double]               -- 検出板に当たった位置（新しい順、400 個まで）
  , wEmitT   :: !Double
  , wDriftLog :: !(Seq.Seq (Double, Double))   -- E×B ドリフトの記録 (x, 時刻 [秒])
  , wDims    :: !(Double, Double)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-08 Lorentz force" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (startMode 1 world0 { wDims = (realToFrac (envW env), realToFrac (envH env)), wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 0.01 1e5 200 1000 0.1 1 False [] [] 0 Seq.empty (1280, 800) []

center :: World -> (Double, Double)
center w = let (wd, ht) = wDims w in (wd * 0.45, ht * 0.55)

startMode :: Int -> World -> World
startMode m w0 = case m of
  1 -> launch 0 ((mp * sp) / (qe * wBm w)) sp 0 0 Nothing Nothing w
  2 -> launch 0 ((mp * sp) / (qe * wBm w)) (sp * 0.95) 0 (sp * 0.3) Nothing Nothing w
  3 -> launch (-0.5) 0 0 0 0 Nothing Nothing w
  _ -> w
  where
    w = w0 { wMode = m, wParts = [], wHits = [] }
    sp = wSpeed w

-- 粒子を 1 つ加える（14 個まで。多いと古いものから消す）
launch :: Double -> Double -> Double -> Double -> Double -> Maybe Double -> Maybe Double -> World -> World
launch x y vx vy vz mm mq w =
  w { wParts = (if length (wParts w) >= 14 then drop 1 (wParts w) else wParts w)
               ++ [P x y (if wMode w == 2 then -0.2 else 0) vx vy vz (maybe mp id mm) (maybe (wSign w * qe) id mq) Seq.empty] }

-- 場所ごとの場：4 は y > 0 だけに磁場
fieldsAt :: World -> P -> (V3d, V3d)
fieldsAt w p = case wMode w of
  4 -> ((0, 0, 0), (0, 0, if pY p > 0 then wBspec w else 0))
  3 -> ((0, wE w, 0), (0, 0, wBm w))
  _ -> ((0, 0, 0), (0, 0, wBm w))

proj :: World -> P -> Point
proj w p
  | wMode w == 2 = (f2 (cx + pX p * pxm), f2 (cy + 160 + pY p * pxm * 0.35 - pZ p * pxm))
  | otherwise = (f2 (cx + pX p * pxm), f2 (cy - pY p * pxm))
  where (cx, cy) = center w

bNow :: World -> Double
bNow w = if wMode w == 4 then wBspec w else wBm w

-- ---- 1 フレーム進める
step :: Env -> Float -> World -> IO World
step env dtF w0
  | wPaused w0 = pure w0
  | otherwise = do
      let moved = [ let q = foldl' (\a _ -> let (e, b) = fieldsAt w0 a in boris e b (simT / fromIntegral n) a) p [1 .. n]
                    in q { pTrail = pushCap 1500 (proj w0 q) (pTrail q) }
                  | p <- wParts w0 ]
          w1 = w0 { wParts = moved }
      w2 <- if wMode w1 /= 4 then pure w1 else do
        let et = wEmitT w1 + ms
        w' <- if et > 250
                then do
                  k <- randomRIO (0, length masses - 1)
                  let m = getAt 12 masses k * uKg
                      v = sqrt ((2 * qe * wVacc w1) / m)
                  pure (launch (-0.25) (-0.005) 0 v 0 (Just m) (Just qe) w1 { wEmitT = 0 })
                else pure w1 { wEmitT = et }
        let (gone, stay) = foldr (\p (g, s) -> if pY p < -0.006 && pVy p < 0 then (p : g, s) else (g, p : s)) ([], []) (wParts w')
            hits = take 400 (reverse (map (\p -> pX p + 0.25) gone) ++ wHits w')
        pure w' { wParts = stay, wHits = hits }
      let w3 = if wMode w2 == 3 then w2 { wParts = filter (\p -> pX p < 0.9) (wParts w2) } else w2
      -- E×B ドリフト：最後の粒子の x と時刻を記録（平均の速さを出す）
      pure $ case lastM (wParts w3) of
        Just p | wMode w3 == 3 -> w3 { wDriftLog = pushCap 120 (pX p, realToFrac (envTime env)) (wDriftLog w3) }
        _ -> w3
  where
    dt = (2 * pi * mp) / (qe * bNow w0) / stepsPerTurn * (if wMode w0 == 4 then 12 else 1)
    simT = realToFrac (min dtF 0.05) * timeK
    n = min 2000 (ceiling (simT / dt)) :: Int
    ms = realToFrac (min dtF 0.05) * 1000

driftNow :: World -> Double
driftNow w = case (Seq.viewl (wDriftLog w), Seq.viewr (wDriftLog w)) of
  ((ax, at') Seq.:< _, _ Seq.:> (bx, bt)) | Seq.length (wDriftLog w) >= 2 -> (bx - ax) / ((bt - at') * timeK)
  _ -> 0

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ drawBackground env w ]
  ++ concat [ [ Color (if pQ p > 0 then rgb255 255 140 90 else rgb255 110 190 255) (polyline 1 (toList (pTrail p)))
              , Color white (circleD sx sy 8) ]
              ++ [ Color (rgb255 255 230 120) (arrowTo 2 7 (sx, sy) (sx + f2 (fx / fm) * 30, sy - f2 (fy / fm) * 30)) | wMode w /= 2, fm > 0 ]
              ++ [ Color (rgb255 150 255 170) (arrowTo 1.5 6 (sx, sy) (sx + f2 (pVx p / v) * 22, sy - f2 (pVy p / v) * 22)) | wMode w /= 2, v > 0 ]
            | p <- wParts w
            , let (sx, sy) = proj w p
                  v = hyp (pVx p) (pVy p)
                  ((ex, ey, _), (_, _, bz)) = fieldsAt w p
                  fx = pQ p * (ex + pVy p * bz)
                  fy = pQ p * (ey - pVx p * bz)
                  fm = hyp fx fy ]
  ++ [ say s (envW env - 350) (140 + 26 * fromIntegral k) 15 (gray 0.92) | (k, s) <- zip [0 :: Int ..] (infoLines w) ]
  ++ [ say "1 circle  2 helix  3 ExB drift  4 mass spectrometer | click: launch a particle" 20 34 17 (gray 0.92)
     , say ("Up/Down B  Left/Right " ++ getAt "" ["", "speed", "speed", "field E", "voltage"] (wMode w) ++ "  F charge sign  C clear  Space pause (" ++ (if wPaused w then "paused" else "running") ++ ")  R reset") 20 56 15 (gray 0.92)
     , say "Orange = + particle, blue = - particle. Yellow arrow = force F, green = velocity v. Dots = B pointing out of the screen." 20 78 14 (gray 0.67)
     , say ("1 px = 1 mm, 1 s on screen = " ++ showJ (timeK * 1e6) ++ " us. Boris method: v- = v + (qE/m)(dt/2) -> rotate v- around B by the angle") 20 96 14 (gray 0.67)
     , say ("2 atan(qB dt/2m) -> add (qE/m)(dt/2) again. " ++ showJ stepsPerTurn ++ " steps per turn.") 20 114 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]

drawBackground :: Env -> World -> Picture
drawBackground env w = Pictures $
  -- ⊙：磁場は画面から手前へ
  [ Color (rgb255 70 90 120) (Pictures [ circleD x y 3 | y <- takeWhile (< envH env) [120, 160 ..], x <- takeWhile (< envW env) [20, 60 ..], not (wMode w == 4 && y > cyF) ]) | wMode w /= 2 ]
  ++ (if wMode w == 2
        then [ Color (rgb255 70 90 120) (Pictures [ Line [(f2 (cx + x * pxm), envH env - 40), (f2 (cx + x * pxm), 130)] | k <- [0 .. 10 :: Int], let x = -0.4 + 0.08 * fromIntegral k ])
             , say "B points up ^" (f2 (cx + 0.42 * pxm)) (envH env - 50) 14 (gray 0.67) ]
        else [])
  ++ (if wMode w == 3
        then [ Color (rgba255 255 120 120 90) (Pictures [ arrowTo 1 8 (x, cyF + 160) (x, cyF + 100) | x <- takeWhile (< envW env) [60, 180 ..] ])
             , say "Electric field E points up" 70 (cyF + 186) 14 (rgb255 255 150 150) ]
        else [])
  ++ (if wMode w == 4 then spec else [])
  where
    (cx, cy) = center w
    cyF = f2 cy
    x0 = f2 (cx - 0.25 * pxm)
    -- 当たった位置のヒストグラムと、式の位置
    bins = foldl' (\acc h -> let b = floor ((h / 0.6) * 120) :: Int in if b >= 0 && b < 120 then Seq.adjust' (+ 1) b acc else acc) (Seq.replicate 120 (0 :: Int)) (wHits w)
    mx = maximum (1 : toList bins)
    spec =
      [ Color (rgb255 200 205 215) (thickLine 3 (x0, cyF) (f2 (cx + 0.6 * pxm), cyF))
      , say ("accelerate (" ++ showJ (wVacc w) ++ " V)") (x0 - 30) (cyF + 40) 14 (rgb255 160 200 255)
      , say "detector (y = 0)" (f2 (cx + 0.45 * pxm)) (cyF + 24) 14 (gray 0.78)
      , Color (rgb255 255 200 100) (Pictures [ rectTL (x0 + (fromIntegral b * 0.6 * f2 pxm) / 120) (cyF + 4) ((0.6 * f2 pxm) / 120 - 1) ((fromIntegral c / fromIntegral mx) * 60)
                                             | (b, c) <- zip [0 :: Int ..] (toList bins), c > 0 ]) ]
      ++ concat [ [ Color (rgb255 150 255 170) (Line [(x, cyF - 10), (x, cyF + 70 + 16 * fromIntegral k)])
                  , say (showJ m ++ " u") (x + 4) (cyF + 84 + 16 * fromIntegral k) 14 (rgb255 150 255 170) ]   -- ラベルは段をずらす
                | (k, m) <- zip [0 :: Int ..] masses
                , let d = 2 * sqrt ((2 * m * uKg * wVacc w) / qe) / wBspec w
                      x = x0 + f2 (d * pxm) ]

infoLines :: World -> [String]
infoLines w
  | wMode w == 4 =
      [ "B = " ++ fmt 0 (wBspec w * 1000) ++ " mT, accel. " ++ showJ (wVacc w) ++ " V", "speed v = sqrt(2qV/m)", "radius r = mv/(qB)"
      , "-> hits at 2r = (2/B)sqrt(2mV/q)", "heavier -> farther (~ sqrt m)" ]
      ++ [ showJ m ++ " u: 2r = " ++ fmt 2 (200 * sqrt ((2 * m * uKg * wVacc w) / qe) / wBspec w) ++ " cm" | m <- masses ]
  | otherwise = case lastM (wParts w) of
      Nothing -> []
      Just p ->
        let bn = bNow w
            v = sqrt (pVx p * pVx p + pVy p * pVy p + pVz p * pVz p)
            vp = hyp (pVx p) (pVy p)
            tp = (2 * pi * pM p) / (abs (pQ p) * bn)
        in [ "B = " ++ fmt 1 (bn * 1000) ++ " mT", "(proton m = 1.67e-27 kg)", "charge q = " ++ (if pQ p > 0 then "+" else "-") ++ "e"
           , "speed |v| = " ++ sci 3 v ++ " m/s" ]
           ++ (if wMode w /= 3 then [ "r = m v_perp/(qB) = " ++ fmt 2 (((pM p * vp) / (abs (pQ p) * bn)) * 100) ++ " cm"
                                    , "T = 2 pi m/(qB) = " ++ fmt 3 (tp * 1e6) ++ " us", "(T does not depend on speed!)" ] else [])
           ++ (if wMode w == 2 then [ "pitch = v_par T = " ++ fmt 2 ((pVz p * tp) * 100) ++ " cm" ] else [])
           ++ (if wMode w == 3 then [ "field E = " ++ showJ (wE w) ++ " V/m", "drift speed E/B = " ++ sci 3 (wE w / bn) ++ " m/s"
                                    , "measured mean = " ++ sci 3 (driftNow w) ++ " m/s" ] else [])

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMode w of
    1 -> launch x y (wSpeed w) 0 0 Nothing Nothing w
    2 -> launch x 0 (wSpeed w * 0.95) 0 (wSpeed w * 0.3) Nothing Nothing w
    3 -> launch x y 0 0 0 Nothing Nothing w
    _ -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> startMode (fromEnum c - fromEnum '0') w
    | c `elem` "fF" -> w { wSign = -(wSign w) }
    | c `elem` "cC" -> w { wParts = [], wHits = [], wDriftLog = Seq.empty }
    | c `elem` "rR" -> startMode (wMode w) w { wBm = 0.01, wSpeed = 1e5, wE = 200, wVacc = 1000, wBspec = 0.1, wSign = 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _
    | wMode w == 4 -> w { wBspec = min 0.3 (wBspec w * 1.25) }
    | otherwise -> w { wBm = min 0.05 (wBm w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _
    | wMode w == 4 -> w { wBspec = max 0.05 (wBspec w / 1.25) }
    | otherwise -> w { wBm = max 0.002 (wBm w / 1.25) }
  EventKey (SpecialKey KeyRight) Down _ _ -> lr 1
  EventKey (SpecialKey KeyLeft) Down _ _ -> lr (-1)
  _ -> w
  where
    (cx, cy) = center w
    (mx, my) = mouseP5 env
    x = (realToFrac mx - cx) / pxm
    y = (cy - realToFrac my) / pxm
    lr d | wMode w == 3 = w { wE = clampv 0 600 (wE w + 50 * d) }
         | wMode w == 4 = w { wVacc = clampv 250 3000 (wVacc w + 250 * d) }
         | otherwise = w { wSpeed = clampv 2e4 3e5 (wSpeed w * 1.25 ** d) }

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("magnetic field only: speed does not change (within 1e-12 over 20 turns)", maxV < 1e-12)
  , ("period T = 2 pi m/(qB) (within 0.1%)"
    , case cross of
        (c0 : _ : _ : _ : _ : _ : _) -> nearRel ((lastD c0 cross - c0) / fromIntegral (length cross - 1)) tp 0.001
        _ -> False)
  , ("radius r = mv/(qB) (within 0.1%)", nearRel rmin r0 0.001 && nearRel rmax r0 0.001)
  , ("helix pitch = v_par T", nearRel (pZ hel) (3 * 3e4 * tp) 1e-9)
  , ("ExB drift: mean speed over 10 turns = E/B (within 1%)", nearRel (pX ed / (10 * tp)) (200 / 0.01) 0.01)
  , ("mass spectrometer: mass x2 -> landing position x sqrt 2 (within 0.5%)", nearRel (land (24 * uKg) / land (12 * uKg)) (sqrt 2) 0.005)
  , ("flipping the charge sign bends the other way", pVy n1 > 0 && pVy p1 < 0 && near (pVy n1) (-(pVy p1)) 1e-9) ]
  where
    b = (0, 0, 0.01)
    e0 = (0, 0, 0)
    tp = (2 * pi * mp) / (qe * 0.01)
    dt = tp / stepsPerTurn
    v0 = 1e5
    mk vx vz q = P 0 0 0 vx 0 vz mp q Seq.empty
    r0 = (mp * v0) / (qe * 0.01)
    nSteps = round stepsPerTurn * 20 :: Int
    -- 20 周：速さのずれ、位置、vy が − から + になる時刻
    (_, maxV, posR, crossR, _) = foldl' one (mk v0 0 qe, 0, [], [], 0) [0 .. nSteps - 1]
    one (p, mv, ps, cs, prevY) k =
      let q = boris e0 b dt p
          mv' = max mv (abs (sqrt (pVx q * pVx q + pVy q * pVy q + pVz q * pVz q) - v0) / v0)
          cs' = if prevY < 0 && pVy q >= 0 then (fromIntegral (k + 1) * dt - (dt * pVy q) / (pVy q - prevY)) : cs else cs
      in (q, mv', (pX q, pY q) : ps, cs', pVy q)
    cross = reverse crossR
    xs = map fst posR
    ys = map snd posR
    ext f zs = case zs of { (z : rest) -> foldl' f z rest; [] -> 0 }
    ccx = (ext max xs + ext min xs) / 2
    ccy = (ext max ys + ext min ys) / 2
    rs = [ hyp (x - ccx) (y - ccy) | (x, y) <- posR ]
    rmin = minimum (1e9 : rs)
    rmax = maximum (0 : rs)
    hel = iterate (boris e0 b dt) (mk v0 3e4 qe) !!! (round stepsPerTurn * 3)
    ed = iterate (boris (0, 200, 0) b dt) (mk 0 0 qe) !!! (round stepsPerTurn * 10)
    land m = go (P 0 0 0 0 (sqrt ((2 * qe * 1000) / m)) 0 m qe Seq.empty) (0 :: Int)
      where d = (2 * pi * m) / (qe * 0.1) / 2000
            go q k | k >= 4000 = 0 / 0
                   | otherwise = let q' = boris e0 (0, 0, 0.1) d q
                                 in if pY q > 0 && pY q' <= 0 then pX q' else go q' (k + 1)
    n1 = boris e0 b dt (mk v0 0 (-qe))
    p1 = boris e0 b dt (mk v0 0 qe)
    lastD d xs' = case reverse xs' of { (x : _) -> x; [] -> d }

-- ---------- やってみよう ----------
-- 1. ←→ で速さを変えると半径は？ 周期は？（周期が速さによらないのがサイクロトロンのしくみ）
-- 2. ↑ で B を 2 倍にすると半径と周期は何倍？
-- 3. 3 の E×B ドリフトで、電荷の符号を F で変えても 進む向きが同じなのはなぜ？
-- 4. 4 の質量分析器で、12 u と 13 u がちゃんと分かれる B と電圧を探そう
-- 5. stepsPerTurn を 10 にしても、ボリス法なら速さは変わらない。半径と周期はどうなる？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- ボリス法の 1 歩：粒子 p を E [V/m]、B [T] の中で dt 秒進める
boris :: V3d -> V3d -> Double -> P -> P
boris (ex, ey, ez) (bx, by, bz) dt p =
  p { pVx = vx2 + k * ex, pVy = vy2 + k * ey, pVz = vz2 + k * ez
    , pX = pX p + (vx2 + k * ex) * dt, pY = pY p + (vy2 + k * ey) * dt, pZ = pZ p + (vz2 + k * ez) * dt }
  where
    k = (pQ p * dt) / (2 * pM p)
    vx = pVx p + k * ex                                       -- 電場で半分
    vy = pVy p + k * ey
    vz = pVz p + k * ez
    tx = k * bx
    ty = k * by
    tz = k * bz
    t2 = tx * tx + ty * ty + tz * tz
    px = vx + (vy * tz - vz * ty)                             -- v' = v⁻ + v⁻ × t
    py = vy + (vz * tx - vx * tz)
    pz = vz + (vx * ty - vy * tx)
    s = 2 / (1 + t2)
    vx2 = vx + s * (py * tz - pz * ty)                        -- v⁺ = v⁻ + v' × s
    vy2 = vy + s * (pz * tx - px * tz)
    vz2 = vz + s * (px * ty - py * tx)

-- リストの最後（なければ Nothing）
lastM :: [a] -> Maybe a
lastM xs = case reverse xs of
  (x : _) -> Just x
  [] -> Nothing

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where
    e0 = floor (logBase 10 (abs v)) :: Int
    e | 10 ^^ (e0 + 1) <= abs v = e0 + 1
      | 10 ^^ e0 > abs v = e0 - 1
      | otherwise = e0

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d / Nothing）
getAt :: a -> [a] -> Int -> a
getAt d xs i = maybe d id (getAtM xs i)

getAtM :: [a] -> Int -> Maybe a
getAtM xs i = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

-- n 回くり返した結果（iterate と組み合わせる。途中を正格に評価する）
(!!!) :: [a] -> Int -> a
xs !!! n = go xs n
  where go (y : ys) k | k <= 0 = y
                      | otherwise = y `seq` go ys (k - 1)
        go [] _ = error "empty"
infixl 9 !!!

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 線分（太さ w。1 以下なら細い線）
seg :: Float -> Point -> Point -> Picture
seg w a b = if w <= 1 then Line [a, b] else thickLine w a b

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- (x1, y1) → (x2, y2) の矢印。太さ w、hs = 矢じりの大きさ
arrowTo :: Float -> Float -> Point -> Point -> Picture
arrowTo w hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Pictures [ seg w (x1, y1) (x2, y2)
                         , seg w (x2, y2) (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux)
                         , seg w (x2, y2) (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)

-- ---- グラフの道具：枠 (x, y, w, h)、lo〜hi の範囲
type G = (Float, Float, Float, Float)

plotFrame :: G -> String -> Double -> Double -> Picture
plotFrame (x, y, w, h) title lo hi = Pictures $
  [ Color (rgb255 70 80 100) (frameTL 1 x y w h) ]
  ++ [ Color (rgba255 70 80 100 160) (Line [(x, y0), (x + w, y0)]) | lo < 0 && hi > 0, let y0 = y + h * f2 (hi / (hi - lo)) ]
  ++ [ say title (x + 4) (y - 6) 14 (gray 0.78)
     , say (sci 2 hi) (x + w + 4) (y + 12) 14 (gray 0.55)
     , say (sci 2 lo) (x + w + 4) (y + h) 14 (gray 0.55) ]

-- 数の列を左から右へ（cap 個ぶんの幅）
plotTrace :: Float -> G -> [Double] -> Double -> Double -> Int -> Picture
plotTrace wt (x, y, w, h) arr lo hi cap =
  polyline wt [ (x + fromIntegral i / fromIntegral (cap - 1) * w, y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h) | (i, v) <- zip [0 :: Int ..] arr ]

-- 長さ cap までの履歴（古いものから消す）
pushCap :: Int -> a -> Seq.Seq a -> Seq.Seq a
pushCap cap v s = let s1 = s Seq.|> v in if Seq.length s1 > cap then Seq.drop (Seq.length s1 - cap) s1 else s1

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h
