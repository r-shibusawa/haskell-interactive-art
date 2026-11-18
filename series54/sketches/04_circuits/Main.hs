-- ============================================
-- 54-04 直流回路とキルヒホッフの法則 ─ 回路を「連立方程式」で解く（節点解析）
-- 各点（節点）で 入る電流 = 出る電流（キルヒホッフの電流則）、電池は 2 点の電位差を決める。
-- これを行列の連立方程式にして毎フレーム解き（ガウスの消去法）、電流を点の流れ、電位を色で描く
-- 操作：1〜4 = 回路 / 抵抗をクリック = 抵抗値を変える / 電池をクリック = 電圧を変える / スペース = 止める / R = 値をもどす
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

rVals :: [Double]
rVals = [5, 10, 20, 30, 40, 50, 100]   -- ★ クリックで切りかわる抵抗値 [Ω]

vVals :: [Double]
vVals = [3, 6, 8, 9, 12]               -- ★ 電池の電圧 [V]

dotSpeed :: Double
dotSpeed = 160                         -- ★ 点の速さ：1 A で 1 秒に何 px 進むか

-- 回路の要素：種類 'R' 抵抗 / 'V' 電池（p1 が −、p2 が ＋）/ 'W' 導線、両はし（格子の座標）、値（Ω か V）
data El = El { eT :: !Char, eP1, eP2 :: !(Double, Double), eV :: !Double }

-- 解いた結果：節点、節点の電位、要素の電流、未知数の数、電池・導線の数、電流則のずれ
data Sol = Sol { sNodes :: [(Double, Double)], sV :: [Double], sI :: [Double], sN :: !Int, sNv :: !Int, sKcl :: !Double }

data World = World
  { wCirc   :: [El]
  , wPh     :: [Double]                -- 電流の点の位置（要素ごと）
  , wPreset :: !Int
  , wPaused :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-04 DC circuits and Kirchhoff's laws" (setCirc 1 world0))
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (setCirc 1 world0) { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] 1 False []

setCirc :: Int -> World -> World
setCirc n w = w { wPreset = n, wCirc = c, wPh = map (const 0) c }
  where c = makeCircuit n

makeCircuit :: Int -> [El]
makeCircuit n = case n of
  1 -> [e 'V' 0 4 0 0 12, e 'R' 0 0 3 0 10, e 'R' 3 0 6 0 20, e 'R' 6 0 6 4 30, e 'W' 6 4 0 4 0]
  2 -> [ e 'V' 0 4 0 0 8, e 'W' 0 0 2 0 0, e 'W' 2 0 4 0 0, e 'W' 4 0 6 0 0, e 'W' 0 4 2 4 0, e 'W' 2 4 4 4 0, e 'W' 4 4 6 4 0
       , e 'R' 2 0 2 4 10, e 'R' 4 0 4 4 40, e 'R' 6 0 6 4 20 ]
  3 -> [e 'V' 0 4 0 0 12, e 'R' 0 0 3 0 20, e 'R' 3 0 3 4 30, e 'W' 3 0 6 0 0, e 'R' 6 0 6 4 30, e 'W' 6 4 3 4 0, e 'W' 3 4 0 4 0]
  _ -> [ e 'V' 0 4 0 0 12, e 'W' 0 0 3 0 0, e 'W' 0 4 3 4 0, e 'R' 3 0 1.5 2 10, e 'R' 3 0 4.5 2 20, e 'R' 1.5 2 3 4 30
       , e 'R' 4.5 2 3 4 50, e 'R' 1.5 2 4.5 2 40 ]
  where e t x1 y1 x2 y2 v = El t (x1, y1) (x2, y2) v

-- 毎フレーム解く（値を変えるとすぐ反映）。電流の点を p1 → p2 の向きに、速さ ∝ 電流で進める
step :: Float -> World -> World
step dt w
  | wPaused w = w
  | otherwise = w { wPh = zipWith (\ph i -> jsRem (ph + i * dotSpeed * realToFrac (min dt 0.05)) 24) (wPh w) (sI sol) }
  where sol = solveCircuit (wCirc w)

-- 画面の大きさと回路の位置
layout :: Env -> (Float, Float, Float)
layout env = (unit, 60 + unit * 1.2, 200)
  where unit = min ((envW env * 0.55) / 7) ((envH env - 300) / 4.5)

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  concat [ drawEl e (x1, y1) (x2, y2)
           ++ [ Color (rgb255 255 230 120) (circleD (x1 + (x2 - x1) * s / l) (y1 + (y2 - y1) * s / l) 6)
              | abs i > 1e-6, s <- takeWhile (< l) [s0, s0 + 24 ..] ]
           ++ (if eT e /= 'W'
                 then let mx = (x1 + x2) / 2
                          my = (y1 + y2) / 2
                          horiz = abs (y2 - y1) < abs (x2 - x1)
                          (lx, ly) = (mx + (if horiz then 0 else 46), my + (if horiz then -34 else 0))
                          col = if eT e == 'R' then rgb255 255 190 140 else rgb255 160 220 255
                      in [ sayC (showJ (eV e) ++ (if eT e == 'R' then " Ohm" else " V")) lx (ly - 2) 14 col
                         , sayC (fmt 3 (abs i) ++ " A") lx (ly + 16) 14 col ]
                 else [])
         | (e, i, ph) <- zip3 (wCirc w) (sI sol) (wPh w)
         , let (x1, y1) = pp (eP1 e)
               (x2, y2) = pp (eP2 e)
               l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
               s0 = f2 (jsRem (jsRem ph 24 + 24) 24) ]
  -- 節点：色 = 電位（青 0 V → 赤 最大）
  ++ concat [ [ Color (makeColorI (round (60 + 190 * t)) 90 (round (250 - 190 * t)) 255) (circleD x y 16)
              , Color (gray 0.08) (at (x, y) (ring 8 1))
              , say (fmt 2 v ++ " V") (x + 10) (y + 20) 14 (gray 0.86) ]
            | (p, v) <- zip (sNodes sol) (sV sol), let (x, y) = pp p; t = f2 (v / vmax) ]
  -- ---- 右：数字
  ++ [ say ("Current from the battery I = " ++ fmt 4 ib ++ " A") tx 200 16 (gray 0.92)
     , say ("Total resistance R = V / I = " ++ (if abs ib > 1e-12 then fmt 3 (vb / ib) else "-") ++ " Ohm") tx 228 16 (gray 0.92) ]
  ++ [ say s tx (256 + 20 * fromIntegral n) 16 (rgb255 255 230 120) | (n, s) <- zip [0 :: Int ..] fLines ]
  ++ [ say ("Battery power V I = " ++ fmt 3 (vb * ib) ++ " W") tx y1 16 (gray 0.92)
     , say ("Heat in resistors sum I^2 R = " ++ fmt 3 heat ++ " W") tx (y1 + 26) 16 (gray 0.92)
     , say ("Largest current-law error at a node = " ++ sci 2 (sKcl sol) ++ " A") tx (y1 + 56) 16 (rgb255 150 230 170) ]
  ++ [ say s tx (y1 + 80 + 19 * fromIntegral n) 14 (gray 0.67)
     | (n, s) <- zip [0 :: Int ..] (wrapText 14 (envW env - tx - 16)
         ("At every node: total current in = total current out. Unknowns = " ++ show (length (sNodes sol) - 1)
          ++ " node potentials + " ++ show (sNv sol) ++ " battery/wire currents -> " ++ show (sN sol) ++ " simultaneous equations")) ]
  ++ [ say ("1 series  2 parallel  3 mixed  4 bridge | click R or battery: change value | Space pause (" ++ (if wPaused w then "paused" else "flowing") ++ ")  R reset") 20 34 16 (gray 0.92) ]
  ++ [ say s 20 (60 + 20 * fromIntegral n) 14 (gray 0.67)
     | (n, s) <- zip [0 :: Int ..] (wrapText 14 (envW env - 40)
         "Yellow dots = current (speed ~ size of the current; dots move like + charges = direction of the current). Circles = nodes (color = potential: blue 0 V -> red high). The long line of the battery is +") ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    sol = solveCircuit (wCirc w)
    (unit, ox, oy) = layout env
    pp (a, b) = (ox + f2 a * unit, oy + f2 b * unit)
    vmax = maximum (1e-9 : sV sol)
    tx = envW env * 0.66
    bi = maybe 0 id (elemIndex 'V' (map eT (wCirc w)))
    ib = getAt 0 (sI sol) bi
    vb = maybe 0 eV (getAtM (wCirc w) bi)
    fLines = wrapText 16 (envW env - tx - 16) (formulaText w)
    y1 = 256 + 20 * fromIntegral (length fLines) + 14
    heat = foldl' (+) 0 [ i * i * eV e | (e, i) <- zip (wCirc w) (sI sol), eT e == 'R' ]

formulaText :: World -> String
formulaText w = case wPreset w of
  1 -> "Formula: series R1 + R2 + R3 = " ++ fmt 3 (r 0 + r 1 + r 2) ++ " Ohm"
  2 -> "Formula: parallel 1 / (1/R1 + 1/R2 + 1/R3) = " ++ fmt 3 (1 / (1 / r 0 + 1 / r 1 + 1 / r 2)) ++ " Ohm"
  3 -> "Formula: R1 + (R2 parallel R3) = " ++ fmt 3 (r 0 + 1 / (1 / r 1 + 1 / r 2)) ++ " Ohm"
  _ -> "Bridge: when R1/R3 = R2/R4 no current flows through the middle (" ++ showJ (r 4) ++ " Ohm). Now R1/R3 = "
       ++ fmt 3 (r 0 / r 2) ++ ", R2/R4 = " ++ fmt 3 (r 1 / r 3)
  where rs = [ eV e | e <- wCirc w, eT e == 'R' ]
        r = getAt 0 rs

-- 要素を描く：導線、抵抗（ぎざぎざ）、電池（短い線が −、長い線が ＋）
drawEl :: El -> Point -> Point -> [Picture]
drawEl e (x1, y1) (x2, y2)
  | eT e == 'W' = [ Color wireC (thickLine 3 (x1, y1) (x2, y2)) ]
  | eT e == 'R' = leads ++ [ Color (rgb255 255 170 110) (thickPath 3 [ (x1 + ux * s + nx * o, y1 + uy * s + ny * o)
                                                                       | i <- [0 .. 8 :: Int]
                                                                       , let s = a + ((b - a) * fromIntegral i) / 8
                                                                             o | i == 0 || i == 8 = 0
                                                                               | odd i = 10
                                                                               | otherwise = -10 ]) ]
  | otherwise = leads ++
      [ Color wireC (thickLine 3 (pt a) (pt (m - 6)))
      , Color wireC (thickLine 3 (pt (m + 6)) (pt b))
      , Color (rgb255 120 200 255) (thickLine 4 (c1x - nx * 10, c1y - ny * 10) (c1x + nx * 10, c1y + ny * 10))
      , Color (rgb255 120 200 255) (thickLine 4 (c2x - nx * 22, c2y - ny * 22) (c2x + nx * 22, c2y + ny * 22)) ]
  where
    wireC = rgb255 200 205 215
    l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    ux = (x2 - x1) / l
    uy = (y2 - y1) / l
    nx = -uy
    ny = ux
    a = l * 0.32
    b = l * 0.68
    m = l / 2
    pt s = (x1 + ux * s, y1 + uy * s)
    (c1x, c1y) = pt (m - 6)
    (c2x, c2y) = pt (m + 6)
    leads = [ Color wireC (thickLine 3 (x1, y1) (pt a)), Color wireC (thickLine 3 (pt b) (x2, y2)) ]

-- 文字を幅 wd [px] に収まるように単語で折り返す
wrapText :: Float -> Float -> String -> [String]
wrapText size wd s = go (words s)
  where
    maxC = max 10 (floor (wd / (size * 0.72)))
    go [] = []
    go (x : xs) = let (ln, rest) = fill x xs in ln : go rest
    fill acc [] = (acc, [])
    fill acc (y : ys) | length acc + 1 + length y <= maxC = fill (acc ++ " " ++ y) ys
                      | otherwise = (acc, y : ys)

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (unit, ox, oy) = layout env
        (mx, my) = mouseP5 env
        hits = [ (realToFrac d, k) | (k, e) <- zip [0 ..] (wCirc w), eT e /= 'W'
                                   , let (a, b) = eP1 e; (c, d') = eP2 e
                                         d = distToSeg mx my (ox + f2 a * unit) (oy + f2 b * unit) (ox + f2 c * unit) (oy + f2 d' * unit)
                                   , d < 16 ]
        hit = snd (foldl' (\(bd, bi) (d, k) -> if d < bd then (d, k) else (bd, bi)) (16, -1) hits)
        next e = let list = if eT e == 'R' then rVals else vVals
                     i = maybe (-1) id (elemIndex (eV e) list)
                 in e { eV = getAt (eV e) list ((i + 1) `mod` length list) }
    in pure (if hit < 0 then w else w { wCirc = updateAt hit next (wCirc w) })
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (setCirc (fromEnum c - fromEnum '0') w)
    | c `elem` "rR" -> pure w { wCirc = makeCircuit (wPreset w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  _ -> pure w

distToSeg :: Float -> Float -> Float -> Float -> Float -> Float -> Float
distToSeg px py x1 y1 x2 y2 = sqrt ((px - x1 - t * dx) ^ (2 :: Int) + (py - y1 - t * dy) ^ (2 :: Int))
  where dx = x2 - x1
        dy = y2 - y1
        t = clampF 0 1 (((px - x1) * dx + (py - y1) * dy) / (dx * dx + dy * dy))

-- ---- 自己テスト：手で計算した電流と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("series 10 + 20 + 30 Ohm, 12 V -> 0.2 A everywhere", all (\k -> near (abs (getAt 0 (sI s1) k)) 0.2 1e-12) [1, 2, 3, 4])
  , ("parallel 10, 40, 20 Ohm, 8 V -> 0.8, 0.2, 0.4 A, total 5.714 Ohm"
    , case ri of
        [r0, r1, r2] -> near r0 0.8 1e-12 && near r1 0.2 1e-12 && near r2 0.4 1e-12 && near (8 / abs (getAt 0 (sI s2) 0)) (1 / (1 / 10 + 1 / 40 + 1 / 20)) 1e-9
        _ -> False)
  , ("balanced bridge (10/30 = 20/60) -> no current through the middle", abs (getAt 1 (sI (solveCircuit c4)) 7) < 1e-12)
  , ("in every circuit, at every node: in = out (current law)", all (\n -> sKcl (solveCircuit (makeCircuit n)) < 1e-12) [1, 2, 3, 4])
  , ("voltage drops around the loop add up to the battery voltage (voltage law)", near dropSum 12 1e-12)
  , ("equation solver: 2x+y-z=8 ... -> (2, 3, -1)"
    , case gaussSolve [[2, 1, -1], [-3, -1, 2], [-2, 1, 2]] [8, -11, -3] of
        Just [x0, x1, x2] -> near x0 2 1e-12 && near x1 3 1e-12 && near x2 (-1) 1e-12
        _ -> False)
  , ("battery power V I = heat in resistors sum I^2 R (energy conservation)", nearRel (12 * abs (getAt 0 (sI s3) 0)) pr 1e-12) ]
  where
    s1 = solveCircuit (makeCircuit 1)
    s2 = solveCircuit (makeCircuit 2)
    ri = [ abs (getAt 0 (sI s2) k) | k <- [7, 8, 9] ]
    c4 = foldl' (\c (k, v) -> updateAt k (\e -> e { eV = v }) c) (makeCircuit 4) [(3, 10), (4, 20), (5, 30), (6, 60)]
    drop' = [ abs (getAt 0 (sI s1) k) * eV (getAt (El 'W' (0, 0) (0, 0) 0) (makeCircuit 1) k) | k <- [1, 2, 3] ]
    dropSum = foldl' (+) 0 drop'
    c3 = makeCircuit 3
    s3 = solveCircuit c3
    pr = foldl' (+) 0 [ i * i * eV e | (e, i) <- zip c3 (sI s3), eT e == 'R' ]

-- ---------- やってみよう ----------
-- 1. 直列で、まん中の抵抗を 100 Ω にすると電流と各抵抗の電圧はどう変わる？
-- 2. 並列で、1 本だけ 5 Ω にすると、ほかの 2 本の電流は変わる？ 電池の電流は？
-- 3. ブリッジで、まん中の電流が 0 になる組み合わせを探そう（R1/R3 = R2/R4）
-- 4. 電池の電力と、抵抗の発熱の合計が いつも同じになることを確かめよう
-- 5. makeCircuit に 5 番目の回路（電池 2 つ、はしご形など）を足してみよう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 節点解析（電池・導線の電流も未知数にする「修正節点解析」）
-- 未知数：節点の電位（最初の電池の − を 0 V とする）と、電池・導線を流れる電流
solveCircuit :: [El] -> Sol
solveCircuit c = Sol nodes vNode iEl n (length vs) kcl
  where
    -- 節点に番号をつける（はじめて出てきた順。最初は電池の − 側）
    addNode ns p = if any (samePt p) ns then ns else ns ++ [p]
    samePt (a, b) (x, y) = abs (x - a) < 1e-9 && abs (y - b) < 1e-9
    firstV = maybe (0, 0) eP1 (getAtM [ e | e <- c, eT e == 'V' ] 0)
    nodes = foldl' (\ns e -> addNode (addNode ns (eP1 e)) (eP2 e)) [firstV] c
    idx p = length (takeWhile (not . samePt p) nodes)
    g0 = 0                                                   -- 0 V の点
    ends = [ (idx (eP1 e), idx (eP2 e)) | e <- c ]
    vs = [ k | (k, e) <- zip [0 ..] c, eT e /= 'R' ]
    nn = length nodes
    row i = if i < g0 then i else i - 1
    n = nn - 1 + length vs
    vsIndex k = maybe 0 id (elemIndex k vs)
    -- 行列 A と右辺 b を組み立てる
    entries = concat (zipWith3 stamp [0 ..] c ends)
    stamp k e (a, bb)
      | eT e == 'R' =                                        -- 抵抗：コンダクタンス 1/R を 4 か所に
          let g = 1 / eV e
          in (if a /= g0 then [((row a, row a), g)] ++ [ ((row a, row bb), -g) | bb /= g0 ] else [])
             ++ (if bb /= g0 then [((row bb, row bb), g)] ++ [ ((row bb, row a), -g) | a /= g0 ] else [])
      | otherwise =                                          -- 電池・導線：V(p2) − V(p1) = 電圧（導線は 0 V）
          let m = nn - 1 + vsIndex k
          in (if a /= g0 then [((row a, m), 1), ((m, row a), -1)] else [])
             ++ (if bb /= g0 then [((row bb, m), -1), ((m, row bb), 1)] else [])
    amap = M.fromListWith (flip (+)) entries
    bvec = M.fromList [ (nn - 1 + vsIndex k, if eT e == 'V' then eV e else 0) | (k, e) <- zip [0 ..] c, eT e /= 'R' ]
    matA = [ [ M.findWithDefault 0 (r, q) amap | q <- [0 .. n - 1] ] | r <- [0 .. n - 1] ]
    vecB = [ M.findWithDefault 0 r bvec | r <- [0 .. n - 1] ]
    x = maybe (replicate n 0) id (gaussSolve matA vecB)
    vNode = [ if i == g0 then 0 else getAt 0 x (row i) | i <- [0 .. nn - 1] ]
    iEl = [ if eT e == 'R' then (getAt 0 vNode a - getAt 0 vNode bb) / eV e else getAt 0 x (nn - 1 + vsIndex k)
          | (k, e, (a, bb)) <- zip3 [0 ..] c ends ]
    -- 確かめ：節点から出ていく電流の合計
    net = M.fromListWith (+) (concat [ [(a, i), (bb, -i)] | ((a, bb), i) <- zip ends iEl ])
    kcl = foldl' max 0 (map abs (M.elems net))

-- ---- ガウスの消去法（部分ピボット選択つき）。解けないときは Nothing
gaussSolve :: [[Double]] -> [Double] -> Maybe [Double]
gaussSolve a0 b0 = go 0 (Seq.fromList (zip a0 b0))
  where
    n = length b0
    go c rows
      | c >= n = Just (backSub (toList rows))
      | abs (pv p) < 1e-14 = Nothing
      | otherwise = go (c + 1) (elim (swap rows))
      where
        pv r = getAt 0 (fst (Seq.index rows r)) c
        p = foldl' (\best r -> if abs (pv r) > abs (pv best) then r else best) c [c + 1 .. n - 1]
        swap rs = Seq.update p (Seq.index rs c) (Seq.update c (Seq.index rs p) rs)
        elim rs =
          let (pr, pb) = Seq.index rs c
              pc = getAt 0 pr c
          in Seq.mapWithIndex (\r (ar, br) ->
               if r <= c then (ar, br) else
                 let f = getAt 0 ar c / pc
                 in if f == 0 then (ar, br)
                    else ([ if k >= c then v - f * u else v | (k, v, u) <- zip3 [0 ..] ar pr ], br - f * pb)) rs
    backSub rows = foldr (\(r, (ar, br)) xs ->
                            let s = foldl' (\acc (k, xk) -> acc - getAt 0 ar k * xk) br (zip [r + 1 ..] xs)
                            in s / getAt 1 ar r : xs) [] (zip [0 ..] rows)

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

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
