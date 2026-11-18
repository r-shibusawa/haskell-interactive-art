-- ============================================
-- 54-03 導体とコンデンサ ─ ラプラス方程式を「緩和法」で解く
-- 導体の電位を決めると、そのあいだの電位 V は ∇²V = 0（どの点も まわり 4 つの平均）。何度もならすと答えに近づく（SOR 法）
-- 導体の表面の電場から電荷 Q を求め、電気容量 C = Q / ΔV を計算。2 次元（紙に垂直に長い導体）なので C は 1 m あたり
-- 操作：1 平行板 / 2 同軸 / 3 とがった導体 / ←→ = 間隔・大きさ / ドラッグ = 導体を描く / D = 描く電位 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad.ST (ST, runST)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

eps0 :: Double
eps0 = 8.854e-12                       -- 真空の誘電率 [F/m]

hM :: Double
hM = 0.001                             -- ★ 格子 1 マス = 1 mm

nx0, ny0 :: Int
nx0 = 160                              -- 格子の数
ny0 = 100

omega :: Double
omega = 1.9                            -- ★ SOR の加速の強さ（1 = ふつうのガウス・ザイデル、2 未満）

sweeps :: Int
sweeps = 40                            -- ★ 1 フレームでならす回数

-- 描く導体：名前、導体の番号（0 = 消しゴム）、電位
brushes :: [(String, Int, Double)]
brushes = [("+100 V conductor", 1, 100), ("0 V conductor", 2, 0), ("eraser", 0, 0)]

data World = World
  { wGrid     :: !Grid
  , wPreset   :: !Int
  , wGap      :: !Int
  , wCoaxA    :: !Int
  , wTipGap   :: !Int
  , wBrush    :: !Int
  , wResidual :: !Double
  , wSweeps   :: !Int
  , wContours :: [(Double, Double, Double, Double)]
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-03 Conductors and capacitors" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = build (World (makeGrid 1 1 []) 1 20 10 18 0 1 0 [] [])

-- ---- 導体の形を作る。外の枠は 0 V（接地した箱）
build :: World -> World
build w = w { wGrid = makeGrid nx0 ny0 (frame ++ shape), wResidual = 1, wSweeps = 0, wContours = [] }
  where
    frame = [ (i, 0, 3, 0) | i <- [0 .. nx0 - 1] ] ++ [ (i, ny0 - 1, 3, 0) | i <- [0 .. nx0 - 1] ]
         ++ [ (0, j, 3, 0) | j <- [0 .. ny0 - 1] ] ++ [ (nx0 - 1, j, 3, 0) | j <- [0 .. ny0 - 1] ]
    cx = nx0 `div` 2
    cy = ny0 `div` 2
    gap = wGap w
    tipGap = wTipGap w
    tipY = ny0 - 6 - tipGap                                                  -- とがった先の位置
    shape = case wPreset w of
      1 -> concat [ [ (i, jsRound (fromIntegral cy - fromIntegral gap / 2), 1, 50), (i, jsRound (fromIntegral cy + fromIntegral gap / 2), 2, -50) ]
                  | i <- [cx - 35 .. cx + 34] ]
      2 -> [ (i, j, if r <= a then 1 else 2, if r <= a then 100 else 0)
           | j <- [0 .. ny0 - 1], i <- [0 .. nx0 - 1]
           , let r = hyp (fromIntegral (i - cx)) (fromIntegral (j - cy)); a = fromIntegral (wCoaxA w)
           , r <= a || r >= 44 ]
      _ -> [ (i, j, 2, 0) | i <- [1 .. nx0 - 2], j <- [ny0 - 6 .. ny0 - 2] ]   -- 下の接地した板
           ++ [ (i, j, 1, 100) | j <- [8 .. tipY]
              , let wd = max 0 (jsRound (14 * sqrt (fromIntegral (tipY - j) / fromIntegral (tipY - 8))))
              , i <- [cx - wd .. cx + wd] ]
           ++ [ (i, j, 1, 100) | i <- [20 .. 49], j <- [30 .. ny0 - 6 - tipGap - 1]   -- 比べる：丸い導体
              , hyp (fromIntegral (i - 35)) (fromIntegral (j - (ny0 - 6 - tipGap - 10))) < 10 ]

-- 1 フレーム：まだ変化があれば 40 回ならす。等電位線は 6 フレームごと（収束したときも）作り直す
step :: Env -> World -> World
step env w
  | wResidual w > 1e-6 =
      let (g, r) = relaxN omega sweeps (wGrid w)
          w1 = w { wGrid = g, wResidual = r, wSweeps = wSweeps w + sweeps }
      in if envFrame env `mod` 6 == 0 || r <= 1e-6 then w1 { wContours = equipotentials g } else w1
  | otherwise = w

equipotentials :: Grid -> [(Double, Double, Double, Double)]
equipotentials g = concat [ marchingSquares (gV g) (gNx g) (gNy g) (l + 0.001) | l <- [-90, -80 .. 90] ]

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ at (ox + fromIntegral nx0 * cs / 2, oy + fromIntegral ny0 * cs / 2) (Scale 1 (-1) (pixelImage nx0 ny0 cs pix))
  , Color (rgba255 255 230 120 170) (Pictures [ Line [(sx x1, sy y1), (sx x2, sy y2)] | (x1, y1, x2, y2) <- wContours w ]) ]
  -- 電場の矢印（−∇V）
  ++ [ Color (rgba255 230 240 255 (round (clampv 40 230 (40 + m / 400)))) (arrow1 4 (px, py) (px + f2 (ex / m) * 2.2 * cs, py + f2 (ey / m) * 2.2 * cs))
     | j <- [2, 7 .. ny0 - 2], i <- [2, 7 .. nx0 - 2]
     , let k = j * nx0 + i
     , gCond g U.! k == 0
     , let (ex, ey) = eAt k
           m = hyp ex ey
     , m > 100
     , let px = ox + (fromIntegral i + 0.5) * cs; py = oy + (fromIntegral j + 0.5) * cs ]
  ++ [ Color (rgb255 120 255 160) (at (ox + (fromIntegral mi + 0.5) * cs, oy + (fromIntegral mj + 0.5) * cs) (ring 9 2)) ]
  -- 数字
  ++ concat (zipWith (\k ls -> [ say s tx (oy + 10 + 46 * fromIntegral k + 19 * fromIntegral n) 15 (gray 0.92) | (n, s) <- zip [0 :: Int ..] ls ]) [0 :: Int ..] (map (wrapText 15 (envW env - tx - 10)) items))
  ++ [ say ("1 plates  2 coax  3 sharp tip | Left/Right: " ++ getAt "" ["", "plate gap", "inner radius", "tip-plate distance"] (wPreset w)
            ++ " | drag: draw conductor (D: " ++ (let (n, _, _) = getAt ("", 0, 0) brushes (wBrush w) in n) ++ ")  R reset") 20 34 16 (gray 0.92) ]
  ++ [ say s 20 (58 + 20 * fromIntegral n) 14 (gray 0.67)
     | (n, s) <- zip [0 :: Int ..] (wrapText 14 (envW env - 40) ("Grid " ++ show nx0 ++ " x " ++ show ny0 ++ " (1 cell = 1 mm). Copper = conductor (fixed potential), the outer frame is 0 V. Red = high V, blue = low. Yellow = equipotentials every 10 V, white arrows = field E = -grad V"))
     ]
  ++ [ say ("Repeat until every point is the average of its 4 neighbours: V <- V + w (average - V), w = " ++ showJ omega) 20 104 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    g = wGrid w
    cs = max 2 (min ((envW env * 0.66 - 30) / fromIntegral nx0) ((envH env - 175) / fromIntegral ny0))
    ox = 20
    oy = 150
    sx v = ox + (f2 v + 0.5) * cs
    sy v = oy + (f2 v + 0.5) * cs
    pix i j = let k = j * nx0 + i
                  v = gV g U.! k / 100
              in if gCond g U.! k /= 0 then (200, 120, 60)
                 else (byte (25 + max 0 v * 220), byte (30 + 50 * abs v), byte (45 + max 0 (-v) * 220))
    eAt k = (-(gV g U.! (k + 1) - gV g U.! (k - 1)) / (2 * hM), -(gV g U.! (k + nx0) - gV g U.! (k - nx0)) / (2 * hM))
    -- いちばん強い電場の所
    (emax, mi, mj) = foldl' (\acc@(e0, _, _) (i, j) ->
                               let k = j * nx0 + i
                                   (ex, ey) = eAt k
                                   m = hyp ex ey
                               in if gCond g U.! k == 0 && m > e0 then (m, i, j) else acc)
                            (0, 0, 0) [ (i, j) | j <- [1 .. ny0 - 2], i <- [1 .. nx0 - 2] ]
    dV = 100
    q = chargeOf g 1
    c = q / dV
    tx = ox + fromIntegral nx0 * cs + 24
    items =
      [ "Sweeps " ++ show (wSweeps w) ++ "  change " ++ sci 2 (wResidual w) ++ (if wResidual w <= 1e-6 then " (converged)" else "")
      , "Charge on the + conductor Q = " ++ sci 3 (q * 1e12) ++ " pC/m"
      , "Potential difference dV = " ++ showJ dV ++ " V"
      , "Capacitance C = Q/dV = " ++ sci 3 (c * 1e12) ++ " pF/m"
      , "Strongest field (green circle) |E| = " ++ sci 3 (emax / 1000) ++ " kV/m" ]
      ++ case wPreset w of
           1 -> [ "Parallel-plate formula eps0 w/d = " ++ sci 3 ((eps0 * 70) / fromIntegral (wGap w) * 1e12) ++ " pF/m (w = 70 mm, d = " ++ show (wGap w) ++ " mm)"
                , "-> above the formula: field at the plate edges (fringe effect)" ]
           2 -> [ "Coaxial formula 2 pi eps0 / ln(b/a) = " ++ sci 3 (((2 * pi * eps0) / log (44 / fromIntegral (wCoaxA w))) * 1e12) ++ " pF/m (a = " ++ show (wCoaxA w) ++ ", b = 44 mm)" ]
           _ -> [ "Field at the tip / average between plates (100 V / " ++ show (wTipGap w) ++ " mm) = " ++ fmt 2 (emax / (100 / (fromIntegral (wTipGap w) * hM))) ++ " x"
                , "-> the field gathers at sharp points (lightning rod)" ]

byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round v :: Int)))

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
  EventMotion _ | envDown env ->
    let i = floor ((mx - ox) / cs) :: Int
        j = floor ((my - oy) / cs) :: Int
        (_, bid, bv) = getAt ("", 0, 0) brushes (wBrush w)
        cells = [ (ii, jj) | dj <- [-1 .. 1], di <- [-1 .. 1], let ii = i + di; jj = j + dj
                           , ii >= 1, jj >= 1, ii < nx0 - 1, jj < ny0 - 1 ]
    in pure (if i < 1 || j < 1 || i >= nx0 - 1 || j >= ny0 - 1 then w
             else w { wGrid = if bid == 0 then eraseCells (wGrid w) cells else setConds (wGrid w) [ (ii, jj, bid, bv) | (ii, jj) <- cells ], wResidual = 1 })
  EventKey k Down _ _ -> pure (key k)
  _ -> pure w
  where
    (mx, my) = mouseP5 env
    cs = max 2 (min ((envW env * 0.66 - 30) / fromIntegral nx0) ((envH env - 175) / fromIntegral ny0))
    ox = 20
    oy = 150
    key k = case k of
      Char c | c >= '1' && c <= '3' -> build w { wPreset = fromEnum c - fromEnum '0' }
             | c `elem` "dD" -> w { wBrush = (wBrush w + 1) `mod` length brushes }
             | c `elem` "rR" -> build w { wPreset = 1, wGap = 20, wCoaxA = 10, wTipGap = 18 }
      SpecialKey KeyRight -> arrowKey 1
      SpecialKey KeyLeft -> arrowKey (-1)
      _ -> w
    arrowKey d = build $ case wPreset w of
      1 -> w { wGap = clampI 4 60 (wGap w + 2 * d) }
      2 -> w { wCoaxA = clampI 2 36 (wCoaxA w + d) }
      _ -> w { wTipGap = clampI 4 50 (wTipGap w + 2 * d) }
    clampI a b v = max a (min b v)

-- ---- 自己テスト：答えのわかる形で確かめる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("relaxation converges (change < 1e-11)", na < 20000)
  , ("between parallel plates the potential is a straight line (uniform field)", all (\j -> near (gV ga U.! (j * 30 + 13)) (100 * (1 - fromIntegral j / 20)) 1e-6) [5, 10, 15])
  , ("parallel plates: C = eps0 w/d (within 1%)", nearRel ca ((eps0 * 30) / 20) 0.01)
  , ("half the gap -> C doubles", nearRel (cb / ca) 2 0.01)
  , ("coaxial: C = 2 pi eps0 / ln(b/a) (within 5%, the grid is jagged)", nearRel cc ((2 * pi * eps0) / log 4) 0.05)
  , ("coaxial: inner charge = -outer charge (every field line reaches the outside)", nearRel (chargeOf gc 2) (-cc) 0.01)
  , ("in the answer every point is the average of its 4 neighbours (Laplace)", mv < 1e-8) ]
  where
    -- (a) 横いっぱいの平行板（左右は「電場が横に出ない」境界）→ 電位はまっすぐ、C = ε0·w/d ぴったり
    pp d = let g0 = makeGrid 30 (d + 1) (concat [ [(i, 0, 1, 100), (i, d, 2, 0)] | i <- [0 .. 29] ])
               (g, n) = relaxUntil 1.8 1e-11 20000 g0
           in (g, n, chargeOf g 1 / 100)
    (ga, na, ca) = pp 20
    (_, _, cb) = pp 10
    -- (b) 同軸：C = 2πε0 / ln(b/a)
    gc0 = makeGrid 72 72 [ (i, j, if r <= 8 then 1 else 2, if r <= 8 then 1 else 0)
                         | j <- [0 .. 71], i <- [0 .. 71], let r = hyp (fromIntegral i - 35.5) (fromIntegral j - 35.5), r <= 8 || r >= 32 ]
    (gc, _) = relaxUntil 1.85 1e-10 20000 gc0
    cc = chargeOf gc 1
    vv = gV gc
    mv = foldl' max 0 [ abs (vv U.! k - 0.25 * (vv U.! (k - 1) + vv U.! (k + 1) + vv U.! (k - 72) + vv U.! (k + 72)))
                      | j <- [1 .. 70], i <- [1 .. 70], let k = j * 72 + i, gFix gc U.! k == 0 ]

-- ---------- やってみよう ----------
-- 1. 平行板で ←→ を押して間隔 d を変え、C が 1/d に比例するか表にしよう（ふち効果でずれるのはいつ？）
-- 2. 3 のとがった導体で、先の電場は丸い導体の所の何倍？ 雷が高い所に落ちやすい理由を考えよう
-- 3. omega を 1.0 と 1.95 にして、収束するまでの回数を比べよう（SOR の効果）
-- 4. 2 の同軸で a を変え、C が ln(b/a) に反比例するか確かめよう
-- 5. D で 0 V の導体を選び、平行板のあいだに描いて「静電遮へい」を作ろう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 格子：V = 電位、fix = 1 なら電位が決まっている、cond = 導体の番号（0 = なし）
data Grid = Grid { gNx, gNy :: !Int, gV :: !(U.Vector Double), gFix :: !(U.Vector Int), gCond :: !(U.Vector Int) }

-- 何もない格子に導体 (i, j, 番号, 電位) をのせる
makeGrid :: Int -> Int -> [(Int, Int, Int, Double)] -> Grid
makeGrid nx ny cs = setConds (Grid nx ny (U.replicate (nx * ny) 0) (U.replicate (nx * ny) 0) (U.replicate (nx * ny) 0)) cs

-- 導体をのせる（格子の外は無視）
setConds :: Grid -> [(Int, Int, Int, Double)] -> Grid
setConds g cs = g { gV = gV g U.// [ (k, v) | (k, _, v) <- ks ], gFix = gFix g U.// [ (k, 1) | (k, _, _) <- ks ], gCond = gCond g U.// [ (k, c) | (k, c, _) <- ks ] }
  where ks = [ (j * gNx g + i, c, v) | (i, j, c, v) <- cs, i >= 0, j >= 0, i < gNx g, j < gNy g ]

-- 消しゴム：導体でなくする（電位はそのまま）
eraseCells :: Grid -> [(Int, Int)] -> Grid
eraseCells g cells = g { gFix = gFix g U.// [ (k, 0) | k <- ks ], gCond = gCond g U.// [ (k, 0) | k <- ks ] }
  where ks = [ j * gNx g + i | (i, j) <- cells ]

-- ---- SOR を 1 回：決まっていない点を「まわり 4 つの平均」へ ω 倍だけ近づける。格子の外は鏡（電場が出ない境界）。
--      いちばん大きい変化を返す。p5.js 版と同じ順番（上の行から、左から右へ）で、その場で書きかえる
sorSweep :: Int -> Int -> U.Vector Int -> Double -> MU.MVector s Double -> ST s Double
sorSweep nx ny fix w v = goJ 0 0
  where
    goJ j mx | j >= ny = pure mx
             | otherwise = goI j 0 mx >>= goJ (j + 1)
    goI j i mx
      | i >= nx = pure mx
      | U.unsafeIndex fix k /= 0 = goI j (i + 1) mx
      | otherwise = do
          l <- MU.unsafeRead v (if i > 0 then k - 1 else k + 1)
          r <- MU.unsafeRead v (if i < nx - 1 then k + 1 else k - 1)
          u <- MU.unsafeRead v (if j > 0 then k - nx else k + nx)
          d <- MU.unsafeRead v (if j < ny - 1 then k + nx else k - nx)
          vk <- MU.unsafeRead v k
          let dv = w * (0.25 * (l + r + u + d) - vk)
          MU.unsafeWrite v k (vk + dv)
          goI j (i + 1) (if abs dv > mx then abs dv else mx)
      where k = j * nx + i

-- n 回ならす（最後の変化も返す）
relaxN :: Double -> Int -> Grid -> (Grid, Double)
relaxN w n g = runST $ do
  v <- U.thaw (gV g)
  let loop k r | k >= n = pure r
               | otherwise = sorSweep (gNx g) (gNy g) (gFix g) w v >>= loop (k + 1)
  r <- loop 0 1
  v' <- U.freeze v
  pure (g { gV = v' }, r)

-- 変化が tol 以下になるまで（最大 maxN 回）ならす。ならした回数も返す
relaxUntil :: Double -> Double -> Int -> Grid -> (Grid, Int)
relaxUntil w tol maxN g = runST $ do
  v <- U.thaw (gV g)
  let loop n r | r > tol && n < maxN = sorSweep (gNx g) (gNy g) (gFix g) w v >>= loop (n + 1)
               | otherwise = pure n
  n <- loop 0 1
  v' <- U.freeze v
  pure (g { gV = v' }, n)

-- ---- 導体 id の電荷（1 m あたり）：表面から出る電束 Σ (V導体 − V となり) × ε0（ガウスの法則を格子で）
chargeOf :: Grid -> Int -> Double
chargeOf g cid = eps0 * foldl' (+) 0
  [ vv U.! k - vv U.! kk
  | j <- [0 .. ny - 1], i <- [0 .. nx - 1], let k = j * nx + i, gCond g U.! k == cid
  , (di, dj) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
  , let ii = i + di; jj = j + dj
  , ii >= 0, jj >= 0, ii < nx, jj < ny
  , let kk = jj * nx + ii
  , gCond g U.! kk /= cid ]
  where nx = gNx g
        ny = gNy g
        vv = gV g

-- ---- マーチング・スクエア：値 = level の線分 (x1, y1, x2, y2)（格子の単位）
marchingSquares :: U.Vector Double -> Int -> Int -> Double -> [(Double, Double, Double, Double)]
marchingSquares g nx ny level = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
  where
    at' a b ia ja ib jb = let t = (level - a) / (b - a) in (ia + t * (ib - ia), ja + t * (jb - ja))
    cellSegs i j =
      let v0 = g U.! (j * nx + i)
          v1 = g U.! (j * nx + i + 1)
          v2 = g U.! ((j + 1) * nx + i + 1)
          v3 = g U.! ((j + 1) * nx + i)
          fi = fromIntegral i
          fj = fromIntegral j
          pts = [ at' v0 v1 fi fj (fi + 1) fj | (v0 > level) /= (v1 > level) ]
             ++ [ at' v1 v2 (fi + 1) fj (fi + 1) (fj + 1) | (v1 > level) /= (v2 > level) ]
             ++ [ at' v2 v3 (fi + 1) (fj + 1) fi (fj + 1) | (v2 > level) /= (v3 > level) ]
             ++ [ at' v3 v0 fi (fj + 1) fi fj | (v3 > level) /= (v0 > level) ]
      in case pts of
           [(a, b), (c, d)] -> [(a, b, c, d)]
           [(a, b), (c, d), (e, f), (gg, hh)] -> [(a, b, c, d), (e, f, gg, hh)]
           _ -> []

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- 細い矢印を 1 本の折れ線で（しっぽ → 先 → 矢じり → 先 → 矢じり）
arrow1 :: Float -> Point -> Point -> Picture
arrow1 hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Line [ (x1, y1), (x2, y2), (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux), (x2, y2), (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

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

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

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
