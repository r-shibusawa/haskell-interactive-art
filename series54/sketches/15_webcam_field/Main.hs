{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 54-15 カメラ映像を電荷に ─ 明るい所は ＋、暗い所は − の電荷とみなして電場を描く
-- 画素の明るさ − 平均 = 電荷密度 ρ（全体ではちょうど 0）。ポアソン方程式 ∇²V = −ρ/ε0 を緩和法（SOR）で解く
-- 毎フレーム前の答えから少しだけならすので、映像が動いても電位がついてくる。まわりの枠は 0 V（接地した箱）
-- 操作：1 = 電位の色 / 2 = 電気力線 / 3 = 両方 / ↑↓ = ならす回数 / ←→ = 線の数 / V = カメラ ⇔ テスト画像 / S = 保存（SVG）
-- ============================================
module Main (main) where

import Art
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)
import System.Environment (lookupEnv)

gw, gh :: Int
gw = 80                                -- 計算の格子（カメラを小さくして使う）
gh = 60

omega :: Double
omega = 1.85                           -- ★ SOR の加速

data World = World
  { wV      :: !(U.Vector Double)      -- 電位
  , wRho    :: !(U.Vector Double)      -- 電荷密度
  , wMean   :: !Double                 -- 明るさの平均
  , wRes    :: !Double                 -- 1 回ならしたときの変化
  , wSweeps :: !Int                    -- ★ 1 フレームにならす回数（はじめ 30）
  , wView   :: !Int                    -- 1 電位 / 2 電気力線 / 3 両方
  , wLines  :: !Int                    -- ★ 電気力線の数（はじめ 60）
  , wUseCam :: !Bool
  , wLive   :: !Bool
  , wCam    :: Maybe Camera
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-15 Webcam charges" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera gw gh
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = \_ _ w -> step w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (U.replicate (gw * gh) 0) (U.replicate (gw * gh) 0) 0 0 30 3 60 True False Nothing []

-- テスト画像：明るい丸 2 つと暗い丸 1 つ、ゆるやかな明るさの坂（0〜255 の灰色）
testImage :: U.Vector Double
testImage = U.generate (gw * gh) px
  where
    px k = let (j, i) = k `divMod` gw
               fi = fromIntegral i :: Double
               fj = fromIntegral j :: Double
               v0 = 90 + 40 * (fi / fromIntegral gw)
               v1 = if hyp (fi - 22) (fj - 20) < 8 then 250 else v0
               v2 = if hyp (fi - 55) (fj - 40) < 6 then 240 else v1
               v3 = if hyp (fi - 50) (fj - 15) < 9 then 10 else v2
           in fromIntegral (max 0 (min 255 (round v3 :: Int)))   -- Uint8ClampedArray と同じく偶数への丸め

-- 1 フレーム：明るさ → 電荷密度（平均を引いて全体を 0 に）→ SOR で何回かならす
step :: World -> IO World
step w = do
  fr <- case wCam w of
    Just cam | wUseCam w -> do
      live <- camLive cam
      if not live then pure Nothing else do
        f <- cameraFrame cam
        pure (if B.length f >= gw * gh * 3 then Just f else Nothing)
    _ -> pure Nothing
  let bright = case fr of
        Just f -> U.generate (gw * gh) (\k -> let g o = fromIntegral (BU.unsafeIndex f (3 * k + o)) :: Double
                                              in (0.299 * g 0 + 0.587 * g 1 + 0.114 * g 2) / 255)
        Nothing -> U.map (\v -> (0.299 * v + 0.587 * v + 0.114 * v) / 255) testImage
      mean = U.foldl' (+) 0 bright / fromIntegral (gw * gh)
      rho = U.map (\b -> (b - mean) * 4) bright
      (v, res) = sweeps (wSweeps w) rho gw gh omega (wV w)
  pure w { wV = v, wRho = rho, wMean = mean, wRes = res, wLive = maybe False (const True) fr }

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ at (ox + iw / 2, oy + ih / 2) (Scale 1 (-1) (pixelImage gw gh sc pix)) ]
  ++ [ Color (rgba255 255 240 200 200) (Pictures [ Line [ (ox + f2 x * sc, oy + f2 y * sc) | (x, y) <- path ]
                                                 | (sx, sy) <- seeds (wRho w) (wLines w)
                                                 , let path = traceE (wV w) (wRho w) sx sy, length path > 2 ])
     | wView w /= 1 ]
  ++ [ say ((if wLive w then "Camera" else "Test image (V: camera)") ++ " | 1 potential  2 field lines  3 both  V switch  S save") 20 28 17 (gray 0.92)
     , say ("Up/Down sweeps per frame (" ++ show (wSweeps w) ++ ")  Left/Right number of lines (" ++ show (wLines w) ++ ")   change in one sweep " ++ sci 2 (wRes w)) 20 50 14 (gray 0.67)
     , say ("Grid " ++ show gw ++ " x " ++ show gh ++ " (eps0 = 1, cell = 1). Bright = + charge (red potential), dark = - (blue). Lines run from bright to dark.") 20 68 14 (gray 0.67)
     , drawSelfTest env (wTest w) ]
  where
    sc = min ((envW env - 40) / fromIntegral gw) ((envH env - 130) / fromIntegral gh)
    iw = fromIntegral gw * sc
    ih = fromIntegral gh * sc
    ox = (envW env - iw) / 2
    oy = 90
    vg = wV w
    rho = wRho w
    vmax = U.foldl' (\m x -> max m (abs x)) 1e-9 vg
    q x = fromIntegral (max 0 (min 255 (round x :: Int)))
    pix i j =
      let k = j * gw + i
          v = vg U.! k / vmax
          y = rho U.! k / 4 + wMean w
      in if wView w == 2
           then let g = q (30 + 90 * y) in (g, g, g)
           else ( q (20 + (if v > 0 then 220 * v else 0) + 30 * y)
                , q (24 + 70 * abs v + 30 * y)
                , q (36 + (if v < 0 then -220 * v else 0) + 30 * y) )

-- 電気力線の出発点：電荷がいちばん多い ＋ の所から n 個（近すぎる所はとばす）
seeds :: U.Vector Double -> Int -> [(Double, Double)]
seeds rho n = reverse (go [] cand)
  where
    idx = [ j * gw + i | j <- [2 .. gh - 3], i <- [2 .. gw - 3], rho U.! (j * gw + i) > 0.2 ]
    cand = sortBy (comparing (\k -> negate (rho U.! k))) idx   -- 同じ値は元の順（安定な並べかえ）
    go out [] = out
    go out (k : ks)
      | length out >= n = out
      | otherwise =
          let x = fromIntegral (k `mod` gw)
              y = fromIntegral (k `div` gw)
              out' = if all (\(px, py) -> abs (px - x) + abs (py - y) > 4) out then (x + 0.3, y + 0.3) : out else out
          in if length out' >= n then out' else go out' ks

-- E の向きに RK4 で 0.5 マスずつたどる
traceE :: U.Vector Double -> U.Vector Double -> Double -> Double -> [(Double, Double)]
traceE vg rho x0 y0 = (x0, y0) : go (0 :: Int) x0 y0
  where
    h = 0.5
    u px py = let (ex, ey) = gradE vg px py
                  m = hyp ex ey
              in if m > 1e-12 then (ex / m, ey / m) else (0, 0)
    go n x y
      | n >= 300 = []
      | not (x' > 1 && y' > 1 && x' < fromIntegral gw - 2 && y' < fromIntegral gh - 2) = []
      | rho U.! (jsRound y' * gw + jsRound x') < -0.6 = [(x', y')]     -- とても暗い所（強い −）で止める
      | otherwise = (x', y') : go (n + 1) x' y'
      where
        (k1x, k1y) = u x y
        (k2x, k2y) = u (x + (h / 2) * k1x) (y + (h / 2) * k1y)
        (k3x, k3y) = u (x + (h / 2) * k2x) (y + (h / 2) * k2y)
        (k4x, k4y) = u (x + h * k3x) (y + h * k3y)
        x' = x + (h / 6) * (k1x + 2 * k2x + 2 * k3x + k4x)
        y' = y + (h / 6) * (k1y + 2 * k2y + 2 * k3y + k4y)

-- 格子の電位から E = −∇V（なめらかにつなぐ）
gradE :: U.Vector Double -> Double -> Double -> (Double, Double)
gradE vg x y = (l ex, l ey)
  where
    i = max 1 (min (gw - 3) (floor x))
    j = max 1 (min (gh - 3) (floor y))
    u = x - fromIntegral i
    v = y - fromIntegral j
    at' a b = U.unsafeIndex vg (b * gw + a)   -- i, j は格子の中にそろえてある
    ex a b = -(at' (a + 1) b - at' (a - 1) b) / 2
    ey a b = -(at' a (b + 1) - at' a (b - 1)) / 2
    l f = (1 - u) * (1 - v) * f i j + u * (1 - v) * f (i + 1) j + (1 - u) * v * f i (j + 1) + u * v * f (i + 1) (j + 1)

-- ---- キー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "camera_field" env (draw env w) >> pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSweeps = min 120 (wSweeps w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSweeps = max 1 (wSweeps w - 10) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wLines = min 150 (wLines w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wLines = max 10 (wLines w - 10) }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Poisson equation: a known solution (sin x sin) is reproduced within 1e-8", err < 1e-8)
  , ("Gauss's law on the grid: charge inside = flux out of the boundary", nearRel flux qIn 1e-6)
  , ("point charge: potential is highest there and symmetric up/down/left/right"
    , mxk == 15 * 31 + 15 && near (p U.! (15 * 31 + 5)) (p U.! (15 * 31 + 25)) 1e-9 && near (p U.! (5 * 31 + 15)) (p U.! (15 * 31 + 5)) 1e-9)
  , ("brightness - mean -> total charge is exactly 0", abs s0 < 1e-9)
  , ("E = -grad V: V = 3x - 2y gives E = (-3, 2)", near ex5 (-3) 1e-9 && near ey5 2 1e-9) ]
  where
    nx = 33; ny = 25
    lam = 4 - 2 * cos (pi / fromIntegral (nx - 1)) - 2 * cos (pi / fromIntegral (ny - 1))
    exF i j = sin ((pi * fromIntegral i) / fromIntegral (nx - 1)) * sin ((pi * fromIntegral j) / fromIntegral (ny - 1))
    r = U.generate (nx * ny) (\k -> let (j, i) = k `divMod` nx in lam * exF i j)
    v = untilConverged r nx ny 1.8
    err = maximum (0 : [ abs (v U.! (j * nx + i) - exF i j) | j <- [0 .. ny - 1], i <- [0 .. nx - 1] ])
    -- 長方形の中の電荷 = まわりから出る電束（格子のガウスの法則）
    (i0, i1, j0, j1) = (5, 20, 4, 15)
    vv a b = v U.! (b * nx + a)
    qIn = foldl' (+) 0 [ r U.! (j * nx + i) | j <- [j0 .. j1], i <- [i0 .. i1] ]
    flux = foldl' (+) 0 ([ (vv i0 j - vv (i0 - 1) j) + (vv i1 j - vv (i1 + 1) j) | j <- [j0 .. j1] ]
                         ++ [ (vv i j0 - vv i (j0 - 1)) + (vv i j1 - vv i (j1 + 1)) | i <- [i0 .. i1] ])
    rp = U.generate (31 * 31) (\k -> if k == 15 * 31 + 15 then 1 else 0)
    p = fst (sweeps 3000 rp 31 31 1.8 (U.replicate (31 * 31) 0))
    mxk = U.foldl' (\b k -> if p U.! k > p U.! b then k else b) 0 (U.enumFromN 0 (31 * 31))
    tmp = U.map (/ 255) testImage
    m = U.foldl' (+) 0 tmp / fromIntegral (U.length tmp)
    s0 = U.foldl' (\a x -> a + (x - m)) 0 tmp
    vLin = U.generate (gw * gh) (\k -> let (j, i) = k `divMod` gw in 3 * fromIntegral i - 2 * fromIntegral j)
    (ex5, ey5) = gradE vLin 30.4 20.7

-- ---------- やってみよう ----------
-- 1. 白い紙を 1 枚カメラに見せると、そこから電気力線が出る。黒いペンで点を描くと？
-- 2. 1 の電位の色で、明るい所・暗い所のまわりの「山」と「谷」を見よう
-- 3. ↓ でならす回数を 1 にすると、動いたときに電位がおくれてついてくる。なぜ？
-- 4. 明るさの代わりに「赤 − 青」を電荷にすると、どんな絵になる？（step の bright の計算を変える）
-- 5. S で保存して、電気力線のポスターを作ろう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- SOR を 1 回：∇²V = −ρ（格子の幅 1、ε0 = 1）。枠は 0 V。いちばん大きい変化を返す
--      書きかえられる配列（unboxed）の中で、左上から順にその場で更新する
poissonSweep :: M.MVector s Double -> U.Vector Double -> Int -> Int -> Double -> ST s Double
poissonSweep vg rho nx ny w = goJ 1 0
  where
    goJ !j !mx | j > ny - 2 = pure mx
               | otherwise = goI j 1 mx >>= goJ (j + 1)
    goI !j !i !mx
      | i > nx - 2 = pure mx
      | otherwise = do
          let k = j * nx + i
          a <- M.unsafeRead vg (k - 1)
          b <- M.unsafeRead vg (k + 1)
          c <- M.unsafeRead vg (k - nx)
          d0 <- M.unsafeRead vg (k + nx)
          vk <- M.unsafeRead vg k
          let nv = 0.25 * (a + b + c + d0 + U.unsafeIndex rho k)
              d = w * (nv - vk)
          M.unsafeWrite vg k (vk + d)
          goI j (i + 1) (if abs d > mx then abs d else mx)

-- n 回ならした電位と、最後の 1 回の変化
sweeps :: Int -> U.Vector Double -> Int -> Int -> Double -> U.Vector Double -> (U.Vector Double, Double)
sweeps n rho nx ny w v0 = runST $ do
  vg <- U.thaw v0
  let loop k res | k >= n = pure res
                 | otherwise = poissonSweep vg rho nx ny w >>= loop (k + 1)
  res <- loop 0 0
  v <- U.freeze vg
  pure (v, res)

-- 変化が 1e-12 以下になるまで（最大 5000 回）ならす
untilConverged :: U.Vector Double -> Int -> Int -> Double -> U.Vector Double
untilConverged rho nx ny w = runST $ do
  vg <- M.replicate (nx * ny) 0
  let loop n res | res > 1e-12 && n < (5000 :: Int) = poissonSweep vg rho nx ny w >>= loop (n + 1)
                 | otherwise = pure ()
  loop 0 1
  U.freeze vg

-- JavaScript の Math.round（0.5 は上へ）
jsRound :: Double -> Int
jsRound x = floor (x + 0.5)

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 大きい数・小さい数を読みやすく：0.000032 → "3.2e-5"（有効数字 sig 桁）
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

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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
