-- ============================================
-- 54-07 電流のつくる磁場 ─ ビオ・サバールの法則を数値で足し合わせる
-- 電流を短いまっすぐな部分に分け、それぞれがつくる磁場 dB = (μ0 I / 4π) dl × r / r³ を全部足す
-- 画面は電流をたてに切った断面。⊙ = 手前へ流れる電流、⊗ = 奥へ。白線 = 磁力線、針 = 方位磁針（赤が N）
-- 操作：1 直線 / 2 円電流 / 3 ソレノイド / 4 平行な 2 本 / ↑↓ = 巻き数 / ←→ = 電流 / F = 向きを反転 / D = 2 本の向き / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mu0 :: Double
mu0 = 4 * pi * 1e-7                    -- 真空の透磁率 μ0 [T·m/A]

scaleM :: Double
scaleM = 1400                          -- ★ 1 m = 1400 px（1 cm = 14 px）

rLoop :: Double
rLoop = 0.08                           -- ★ 円電流・ソレノイドの半径 [m]

segs :: Int
segs = 48                              -- ★ 円を何本のまっすぐな部分に分けるか

cell :: Int
cell = 10                              -- 磁場を計算する格子 [px]

-- 電流のまっすぐな部分：a → b [m]（x = 右、y = 奥、z = 上）、電流 I [A]
data Wire = Wire { wa, wb :: !(Double, Double, Double), wI :: !Double }

-- 断面を横切る電流の印（x, z [m]、奥へ流れるか）
data Mark = Mark { mX, mZ :: !Double, mInto :: !Bool }

-- 計算中の格子：次に計算する行と、できた行のかたまり（新しい順）
data Job = Job !Int [U.Vector (Double, Double)]

-- 格子の磁場（Bx と Bz）、格子の大きさ
data Grid = Grid { gNx, gNz :: !Int, gBx, gBz :: !(U.Vector Double) }

data World = World
  { wPreset, wTurns, wCurrent, wFlip :: !Int
  , wAnti   :: !Bool
  , wDims   :: !(Double, Double)        -- 画面の大きさ（起動したとき）
  , wWires  :: !(U.Vector Double)       -- 部分 1 本 = 7 つの数 (ax, ay, az, bx, by, bz, I)
  , wNWire  :: !Int
  , wMarks  :: [Mark]
  , wGrid   :: !Grid
  , wLay    :: Picture                  -- 背景と磁力線（変えたときだけ作る）
  , wJob    :: !(Maybe Job)             -- 計算中の格子
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-07 Magnetic field of currents" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      let w = rebuildAll (rebuild world0 { wDims = (realToFrac (envW env), realToFrac (envH env)), wTest = selfTest })
      pictureSize (wLay w) `seq` pure w
  , skStep  = \_ _ w -> pure (rebuildStep w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 2 8 10 1 False (1280, 800) U.empty 0 [] (Grid 0 0 U.empty U.empty) Blank Nothing []

-- 画面の中心（電流の原点）
center :: World -> (Double, Double)
center w = let (wd, ht) = wDims w in (wd * 0.42, ht * 0.56)

-- ---- 電流の形を作る。格子の磁場は rebuildStep で何フレームかに分けて計算する
rebuild :: World -> World
rebuild w = w { wWires = wiresVec ws, wNWire = length ws, wMarks = marks, wJob = Just (Job 0 []) }
  where
    i = fromIntegral (wCurrent w * wFlip w)
    ws = case wPreset w of
      1 -> [straightY 0 i]
      3 -> let len = 0.3; t = wTurns w
           in concat [ loopX (if t == 1 then 0 else -len / 2 + (len * fromIntegral k) / fromIntegral (t - 1)) rLoop i segs | k <- [0 .. t - 1] ]
      4 -> [straightY (-0.06) i, straightY 0.06 (if wAnti w then -i else i)]
      _ -> loopX 0 rLoop i segs
    -- 断面（y = 0 の平面）を横切る所に ⊙ ⊗
    marks = [ Mark (ax + t * (bx - ax)) (az + t * (bz - az)) ((by - ay) * wI s > 0)
            | s <- ws, let (ax, ay, az) = wa s; (bx, by, bz) = wb s
            , ay * by <= 0 && ay /= by, let t = ay / (ay - by) ]

-- 格子の大きさ
gridSize :: World -> (Int, Int)
gridSize w = let (wd, ht) = wDims w in (ceiling (wd / fromIntegral cell) + 1, ceiling (ht / fromIntegral cell) + 1)

-- 1 フレームに計算する量の目安（格子の点の数 × 電流の部分の数）
budget :: Int
budget = 600000

-- 格子の行を予算のぶんだけ計算する。全部できたら背景と磁力線を作る（計算しているあいだは前の絵のまま）
rebuildStep :: World -> World
rebuildStep w = case wJob w of
  Nothing -> w
  Just (Job row done)
    | row < nz ->
        let rows = max 1 (budget `div` (nx * max 1 (wNWire w)))
            r1 = min nz (row + rows)
            bs = U.generate ((r1 - row) * nx) (\k -> point (row + k `div` nx) (k `mod` nx))
        in bs `seq` w { wJob = Just (Job r1 (bs : done)) }
    | otherwise ->
        let bs = U.concat (reverse done)
            grid = Grid nx nz (U.map fst bs) (U.map snd bs)
            lay = makeLayer w grid
        in grid `seq` w { wGrid = grid, wLay = lay, wJob = Nothing }
  where
    (nx, nz) = gridSize w
    (cx, cy) = center w
    point j ii = let (bx, _, bz) = fieldB (wWires w) ((fromIntegral (ii * cell) - cx) / scaleM) 0 ((cy - fromIntegral (j * cell)) / scaleM) in (bx, bz)

-- 全部いっぺんに計算する（起動したとき）
rebuildAll :: World -> World
rebuildAll w = case wJob w of
  Nothing -> w
  Just _ -> rebuildAll (rebuildStep w)

-- 背景（|B| の明るさ）と磁力線の絵
makeLayer :: World -> Grid -> Picture
makeLayer w grid = Pictures [ img, Color (rgba255 235 240 255 170) (Pictures (map (polyline 1) fieldLines)) ]
  where
    (wd, ht) = wDims w
    (cx, cy) = center w
    nx = gNx grid
    nz = gNz grid
    -- 背景の明るさ = |B|（1 μT 〜 1 mT の対数）
    px x y = let k = y * nx + x
                 m = clampv 0 1 ((logBase 10 (hyp (gBx grid U.! k) (gBz grid U.! k) + 1e-12) + 6) / 3)
             in (q (14 + 60 * m), q (18 + 90 * m), q (30 + 110 * m))
    q v = fromIntegral (round v :: Int)
    c2 = fromIntegral cell
    img = at (-c2 / 2 + fromIntegral nx * c2 / 2, -c2 / 2 + fromIntegral nz * c2 / 2) (Scale 1 (-1) (pixelImage nx nz c2 px))
    seeds = case wPreset w of
      1 -> [ (d, 0) | d <- [0.015, 0.03, 0.05, 0.08, 0.12, 0.17, 0.23, 0.3] ]
      4 -> concat [ [(0.012 * k - 0.054, 0.0001), (0.07 + 0.03 * k, 0)] | k <- map fromIntegral [1 .. 9 :: Int] ]
      _ -> [ (0, (rLoop * 0.95 * fromIntegral k) / 6.5) | k <- [-6 .. 6 :: Int] ]
    tr = traceB (wd, ht) (cx, cy) grid (wMarks w)
    fieldLines = [ reverse (tr sx sz (-1)) ++ tr sx sz 1 | (sx, sz) <- seeds ]

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ wLay w ]
  -- 方位磁針
  ++ [ Pictures [ Color (rgba255 240 80 70 (al a)) (thickLine 3 (x, y) (x + ux * 11, y + uy * 11))
                , Color (rgba255 220 225 235 (al a)) (thickLine 3 (x, y) (x - ux * 11, y - uy * 11)) ]
     | y <- takeWhile (< envH env - 30) [130, 182 ..], x <- takeWhile (< envW env * 0.74) [30, 82 ..]
     , let (bx, bz) = sampleB (wGrid w) (realToFrac x) (realToFrac y)
           m = hyp bx bz
     , m > 0
     , let ux = f2 (bx / m); uy = f2 (-bz / m); a = clampv 0.15 1 ((logBase 10 m + 6.3) / 3) ]
  -- 電流の断面
  ++ concat [ [ Color (rgb255 40 30 20) (circleD sx sy 16), Color (rgb255 255 200 100) (at (sx, sy) (ring 8 2)) ]
              ++ (if mInto m then [ Color (rgb255 255 200 100) (Pictures [thickLine 2 (sx - 5, sy - 5) (sx + 5, sy + 5), thickLine 2 (sx - 5, sy + 5) (sx + 5, sy - 5)]) ]
                  else [ Color (rgb255 255 200 100) (circleD sx sy 5) ])
            | m <- wMarks w, let sx = f2 (cx + mX m * scaleM); sy = f2 (cy - mZ m * scaleM) ]
  -- マウスの所の磁場（その場で計算しなおす）
  ++ [ Color (rgb255 255 230 120) (arrowTo 2 8 (mx, my) (mx + f2 (bx / bm) * 34, my - f2 (bz / bm) * 34)) | bm > 0 ]
  ++ [ say s tx (155 + 20 * fromIntegral k) 15 (gray 0.92) | (k, s) <- zip [0 :: Int ..] infoLines ]
  ++ [ say "1 wire  2 loop  3 solenoid  4 two wires | Up/Down turns  Left/Right current  F flip  D pair dir  R reset" 20 34 16 (gray 0.92)
     , say "Cross-section. (.) = current toward you, (x) = away. Right-hand rule: the screw moves with the current, B turns with it." 20 58 14 (gray 0.67)
     , say "Background brightness = |B| (log scale, 1 uT to 1 mT). 1 cm = 14 px." 20 76 14 (gray 0.67)
     , say ("Every field value = sum of the Biot-Savart formulas of the " ++ show (wNWire w) ++ " straight pieces of current.") 20 94 14 (gray 0.67)
     , say "Field lines: B on the grid, interpolated smoothly and followed with RK4." 20 112 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    (cx, cy) = center w
    (mx, my) = mouseP5 env
    (bx, _, bz) = fieldB (wWires w) ((realToFrac mx - cx) / scaleM) 0 ((cy - realToFrac my) / scaleM)
    bm = hyp bx bz
    al a = round (255 * a) :: Int
    tx = envW env * 0.76
    cur = fromIntegral (wCurrent w) :: Double
    centerB = let (x, y, z) = fieldB (wWires w) 0 0 0 in sqrt (x * x + y * y + z * z)
    info = [ "Current I = " ++ show (wCurrent w * wFlip w) ++ " A", "Straight pieces: " ++ show (wNWire w)
           , "|B| at mouse = " ++ sci 3 (bm * 1e6) ++ " uT", "(Earth's field ~ 45 uT)" ]
           ++ case wPreset w of
                1 -> let d = hyp (realToFrac mx - cx) (realToFrac my - cy) / scaleM
                     in ["Formula mu0 I/(2 pi d) = " ++ sci 3 ((mu0 * cur) / (2 * pi * d) * 1e6) ++ " uT (d = " ++ fmt 1 (d * 100) ++ " cm)"]
                2 -> [ "Center: computed " ++ sci 3 (centerB * 1e6) ++ " uT", "Formula mu0 I/(2R) = " ++ sci 3 ((mu0 * cur) / (2 * rLoop) * 1e6) ++ " uT" ]
                3 -> [ "Turns N = " ++ show (wTurns w) ++ " (length 30 cm)", "Center: computed " ++ sci 3 (centerB * 1e6) ++ " uT"
                     , "Long solenoid formula mu0 n I = " ++ sci 3 (mu0 * (fromIntegral (wTurns w) / 0.3) * cur * 1e6) ++ " uT"
                     , "(a short coil compared with its radius gives less than the formula)" ]
                _ -> [ if wAnti w then "Opposite directions: fields add up between the wires" else "Same direction: 0 in the middle (they cancel)"
                     , "Currents in the same direction attract (D to switch)" ]
    -- 1 項目ずつ折り返し、項目のあいだは少しあける
    infoLines = concat [ wrapWords 29 s ++ [""] | s <- info ]

-- ---- キー（どのキーでも計算しなおす）
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey k Down _ _ -> pure (rebuild (key k))
  _ -> pure w
  where
    key k = case k of
      Char c | c >= '1' && c <= '4' -> w { wPreset = fromEnum c - fromEnum '0' }
             | c `elem` "fF" -> w { wFlip = -(wFlip w) }
             | c `elem` "dD" -> w { wAnti = not (wAnti w) }
             | c `elem` "rR" -> w { wPreset = 2, wTurns = 8, wCurrent = 10, wFlip = 1, wAnti = False }
      SpecialKey KeyUp -> w { wTurns = min 20 (wTurns w + 1) }
      SpecialKey KeyDown -> w { wTurns = max 1 (wTurns w - 1) }
      SpecialKey KeyRight -> w { wCurrent = min 50 (wCurrent w + 5) }
      SpecialKey KeyLeft -> w { wCurrent = max 5 (wCurrent w - 5) }
      _ -> w

-- ---- 自己テスト：式で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("long straight current: B = mu0 I/(2 pi d) (within 0.1%)", nearRel (mag (fieldB w1 d 0 0)) ((mu0 * i) / (2 * pi * d)) 0.001)
  , ("Ampere's law: once around, int B.dl = mu0 I (within 0.1%)", nearRel (abs amp) (mu0 * i) 0.001)
  , ("center of a loop: B = mu0 I/(2R) (72 pieces, within 0.5%)", nearRel (mag (fieldB lp 0 0 0)) ((mu0 * i) / (2 * rLoop)) 0.005)
  , ("on the loop's axis (x = R): B = mu0 I R^2/(2(R^2 + x^2)^(3/2))", nearRel (mag (fieldB lp rLoop 0 0)) ((mu0 * i * rLoop * rLoop) / (2 * (2 * rLoop * rLoop) ** 1.5)) 0.005)
  , ("long solenoid (100 turns per m) center: B ~ mu0 n I (within 1%)", nearRel (abs (fst3 (fieldB sol 0 0 0))) (mu0 * 100 * i) 0.01)
  , ("reversing the current reverses the field"
    , near (fst3 (fieldB (wiresVec (loopX 0 rLoop (-i) 48)) 0.03 0 0.02)) (-(fst3 (fieldB (wiresVec (loopX 0 rLoop i 48)) 0.03 0 0.02))) 1e-15)
  , ("two parallel wires, same direction: B = 0 in the middle", mag (fieldB (wiresVec [straightY (-0.06) i, straightY 0.06 i]) 0 0 0) < 1e-15) ]
  where
    i = 10
    d = 0.05
    w1 = wiresVec [straightY 0 i]
    n = 400 :: Int
    amp = foldl' (\s k -> let a = (2 * pi * (fromIntegral k + 0.5)) / fromIntegral n
                              (bx, _, bz) = fieldB w1 (d * cos a) 0 (d * sin a)
                          in s + (bx * (-(sin a)) + bz * cos a) * ((2 * pi * d) / fromIntegral n)) 0 [0 .. n - 1]
    lp = wiresVec (loopX 0 rLoop i 72)
    sol = wiresVec (concat [ loopX (-0.5 + (fromIntegral k + 0.5) / 100) 0.02 i 36 | k <- [0 .. 99 :: Int] ])
    mag (x, y, z) = sqrt (x * x + y * y + z * z)
    fst3 (x, _, _) = x

-- ---------- やってみよう ----------
-- 1. 1 で、直線から 2 倍はなれると B は何倍？ マウスの数字で確かめよう
-- 2. 3 のソレノイドで ↑ で巻き数をふやすと、中の磁場はどうなる？ 中はどれくらい一様？
-- 3. 2 の円電流と、ソレノイドの外側の磁力線を比べよう。棒磁石の磁力線に似ている？
-- 4. 4 で D を押して、平行電流が同じ向き・逆向きのときの磁力線を比べよう（モーターの力のもと）
-- 5. segs を 4 や 8 にすると、円が多角形になる。中心の磁場は式からどれくらいずれる？


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 電流の部分
straightY :: Double -> Double -> Wire
straightY x i = Wire (x, -50, 0) (x, 50, 0) i          -- 奥へのびた長い直線（100 m）

loopX :: Double -> Double -> Double -> Int -> [Wire]   -- x 軸のまわりの円（n 角形）
loopX x r i n = [ Wire (x, r * cos a0, r * sin a0) (x, r * cos a1, r * sin a1) i
                | k <- [0 .. n - 1], let a0 = (2 * pi * fromIntegral k) / fromIntegral n; a1 = (2 * pi * fromIntegral (k + 1)) / fromIntegral n ]

-- 速く計算するため、部分を 7 つの数ずつ並べた配列にする
wiresVec :: [Wire] -> U.Vector Double
wiresVec ws = U.fromList (concat [ [ax, ay, az, bx, by, bz, wI s] | s <- ws, let (ax, ay, az) = wa s; (bx, by, bz) = wb s ])

-- ---- まっすぐな部分の磁場の和（ビオ・サバールを線にそって積分した正確な式）
-- B = (μ0 I / 4π) (r1 × r2)(|r1| + |r2|) / (|r1||r2|(|r1||r2| + r1·r2))、r1 = P − a、r2 = P − b
fieldB :: U.Vector Double -> Double -> Double -> Double -> (Double, Double, Double)
fieldB v px py pz = go 0 0 0 0
  where
    n = U.length v `div` 7
    go !k !bx !by !bz
      | k >= n = (bx, by, bz)
      | otherwise =
          let o = 7 * k
              e j = U.unsafeIndex v (o + j)
              r1x = px - e 0; r1y = py - e 1; r1z = pz - e 2
              r2x = px - e 3; r2y = py - e 4; r2z = pz - e 5
              n1 = sqrt (r1x * r1x + r1y * r1y + r1z * r1z)
              n2 = sqrt (r2x * r2x + r2y * r2y + r2z * r2z)
              den = n1 * n2 * (n1 * n2 + r1x * r2x + r1y * r2y + r1z * r2z)
              f = (1e-7 * e 6 * (n1 + n2)) / den              -- μ0/4π = 10⁻⁷
          in if not (den > 1e-18) then go (k + 1) bx by bz       -- 電流の線の上は計算しない
             else go (k + 1) (bx + f * (r1y * r2z - r1z * r2y)) (by + f * (r1z * r2x - r1x * r2z)) (bz + f * (r1x * r2y - r1y * r2x))

-- ---- 格子の磁場をなめらかに（双線形補間）。画面の座標 → (Bx, Bz)
sampleB :: Grid -> Double -> Double -> (Double, Double)
sampleB g sx sy
  | gNx g < 2 || gNz g < 2 = (0, 0)
  | otherwise = (l (gBx g), l (gBz g))
  where
    nx = gNx g
    fi = clampv 0 (fromIntegral nx - 1.001) (sx / fromIntegral cell)
    fj = clampv 0 (fromIntegral (gNz g) - 1.001) (sy / fromIntegral cell)
    i = floor fi :: Int
    j = floor fj :: Int
    u = fi - fromIntegral i
    v = fj - fromIntegral j
    k = j * nx + i
    l a = (1 - u) * (1 - v) * a U.! k + u * (1 - v) * a U.! (k + 1) + (1 - u) * v * a U.! (k + nx) + u * v * a U.! (k + nx + 1)

-- ---- 磁力線：B の向きに RK4 で進む（dir = ±1）。もとの点にもどるか画面の外で止まる
traceB :: (Double, Double) -> (Double, Double) -> Grid -> [Mark] -> Double -> Double -> Double -> [Point]
traceB (wd, ht) (cx, cy) g marks x0 z0 dir = map (\(a, b) -> (f2 a, f2 b)) (reverse (go sx0 sy0 0 [(sx0, sy0)]))
  where
    sx0 = cx + x0 * scaleM
    sy0 = cy - z0 * scaleM
    h = 4
    u x y = let (bx, bz) = sampleB g x y
                m = hyp bx bz
            in if m > 0 then ((dir * bx) / m, (-dir * bz) / m) else (0, 0)
    go !sx !sy !n acc
      | n >= (1500 :: Int) = acc
      | otherwise =
          let (k1x, k1y) = u sx sy
              (k2x, k2y) = u (sx + (h / 2) * k1x) (sy + (h / 2) * k1y)
              (k3x, k3y) = u (sx + (h / 2) * k2x) (sy + (h / 2) * k2y)
              (k4x, k4y) = u (sx + h * k3x) (sy + h * k3y)
              x = sx + (h / 6) * (k1x + 2 * k2x + 2 * k3x + k4x)
              y = sy + (h / 6) * (k1y + 2 * k2y + 2 * k3y + k4y)
              acc' = (x, y) : acc
          in if x < 0 || y < 0 || x > wd || y > ht then acc'
             else if n > 30 && hyp (x - sx0) (y - sy0) < 3 then acc'
             else if any (\m -> hyp (cx + mX m * scaleM - x) (cy - mZ m * scaleM - y) < 5) marks then acc'
             else go x y (n + 1) acc'

-- 絵を全部たどる（起動したときに計算をすませておくため）
pictureSize :: Picture -> Int
pictureSize p = case p of
  Pictures ps -> foldl' (\a q -> a + pictureSize q) 0 ps
  Color _ q -> pictureSize q
  Translate _ _ q -> pictureSize q
  Scale _ _ q -> pictureSize q
  Line ps -> length ps
  _ -> 1

-- 文字を n 文字くらいで折り返す（単語の切れ目で）
wrapWords :: Int -> String -> [String]
wrapWords n s = reverse (map unwords (filter (not . null) (map reverse (fin' (foldl' add ([], []) (words s))))))
  where
    add (done, cur) wd
      | null cur = (done, [wd])
      | length (unwords (reverse (wd : cur))) > n = (cur : done, [wd])
      | otherwise = (done, wd : cur)
    fin' (done, cur) = cur : done

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

