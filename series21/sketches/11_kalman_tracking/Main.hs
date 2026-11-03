-- ============================================
-- 11 カルマンフィルタで追跡
-- ノイズだらけの観測（×印）から、ボールの本当の位置と速度を推定する
--   予測：x ← F x（等速で進むはず）、P ← F P Fᵀ + Q（不確かさは増える）
--   更新：K = P Hᵀ (H P Hᵀ + R)⁻¹、x ← x + K (z − H x)、P ← (I − K H) P（観測で不確かさが減る）
-- ↑↓ = 観測ノイズ R / ←→ = プロセスノイズ Q / スペース = 観測を止める（ボールが隠れた状態）/ R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl', transpose)
import Text.Printf (printf)

-- 行列はリストのリスト、ベクトルはリスト
type Mat = [[Float]]
type Vec = [Float]

data World = World
  { wBx     :: !Float     -- 本当の状態
  , wBy     :: !Float
  , wVx     :: !Float
  , wVy     :: !Float
  , wMeasS  :: !Float     -- ★ 観測ノイズの標準偏差（px）
  , wProcS  :: !Float     -- ★ プロセスノイズ（モデルからのずれ）
  , wXs     :: !Vec       -- 推定 [x, y, vx, vy]
  , wP      :: !Mat       -- 共分散 4×4
  , wBlind  :: !Bool
  , wTrue   :: ![Point]   -- 軌跡（新しい順、400 点まで）
  , wEst    :: ![Point]
  , wZs     :: ![Point]   -- 観測（新しい順、120 個まで）
  , wGain   :: !Float
  }

main :: IO ()
main = runSketch (sketch "21-11 Kalman Tracking" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \_ _ w -> step w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = resetAll (World 0 0 0 0 25 0.3 [] [] False [] [] [] 0)

resetAll :: World -> World
resetAll w = w
  { wBx = width * 0.2, wBy = height * 0.5, wVx = 3, wVy = -1.5
  , wXs = [width / 2, height / 2, 0, 0]
  , wP = matScaled eye4 10000     -- 最初は何もわからない
  , wTrue = [], wEst = [], wZs = [] }

-- ★ 予測ステップ
kfPredict :: World -> World
kfPredict w =
  let f = [[1, 0, 1, 0], [0, 1, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1]]
      q = wProcS w * wProcS w
      bigQ = [[q / 4, 0, q / 2, 0], [0, q / 4, 0, q / 2], [q / 2, 0, q, 0], [0, q / 2, 0, q]]
  in w { wXs = matVec f (wXs w)
       , wP = matAdd (matMul (matMul f (wP w)) (transpose f)) bigQ }

-- ★ 更新ステップ（観測 z = [x, y]）
kfUpdate :: Float -> Float -> World -> World
kfUpdate zx zy w =
  let h = [[1, 0, 0, 0], [0, 1, 0, 0]]
      p = wP w
      r2 = wMeasS w * wMeasS w
      bigR = [[r2, 0], [0, r2]]
      s = matAdd (matMul (matMul h p) (transpose h)) bigR        -- 2×2
      k = matMul (matMul p (transpose h)) (inv2 s)               -- 4×2
      y = zipWith (-) [zx, zy] (take 2 (wXs w))
      ky = matVec k y
  in w { wXs = zipWith (+) (wXs w) ky
       , wP = matMul (matSub eye4 (matMul k h)) p
       , wGain = elemAt k 0 0 }

step :: World -> IO World
step w0 = do
  g1 <- gauss
  g2 <- gauss
  g3 <- gauss
  g4 <- gauss
  let -- 本当のボール：等速＋わずかな揺らぎ、壁で跳ね返る
      vx0 = wVx w0 + g1 * 0.05
      vy0 = wVy w0 + g2 * 0.05
      x0 = wBx w0 + vx0
      y0 = wBy w0 + vy0
      vx = if x0 < 30 || x0 > width - 30 then -vx0 else vx0
      vy = if y0 < 120 || y0 > height - 30 then -vy0 else vy0
      x = clampF 30 (width - 30) x0
      y = clampF 120 (height - 30) y0
      w1 = kfPredict w0 { wBx = x, wBy = y, wVx = vx, wVy = vy }
      z = (x + g3 * wMeasS w0, y + g4 * wMeasS w0)
      w2 = if wBlind w0 then w1 else kfUpdate (fst z) (snd z) w1
  pure w2
    { wTrue = take 400 ((x, y) : wTrue w2)
    , wEst = take 400 ((elemV (wXs w2) 0, elemV (wXs w2) 1) : wEst w2)
    , wZs = if wBlind w0 then wZs w2 else take 120 (z : wZs w2) }

-- 2×2 対称行列 [[a, b], [b, c]] の固有分解で不確かさの楕円（2σ）を描く
ellipsePts :: Float -> Float -> Float -> Float -> Float -> [Point]
ellipsePts cx cy a b c =
  let tr = a + c
      det = a * c - b * b
      d = sqrt (max 0 (tr * tr / 4 - det))
      l1 = max (tr / 2 + d) 0.01
      l2 = max (tr / 2 - d) 0.01
      th = if abs b < 1e-9 then 0 else atan2 (l1 - a) b
      rx = 2 * sqrt l1
      ry = 2 * sqrt l2
  in [ (cx + ex * cos th - ey * sin th, cy + ex * sin th + ey * cos th)
     | i <- [0 .. 48 :: Int]
     , let s = tau * fromIntegral i / 48
     , let ex = rx * cos s
     , let ey = ry * sin s ]

drawAll :: World -> Picture
drawAll w =
  let xs = wXs w
      ex = elemV xs 0
      ey = elemV xs 1
      evx = elemV xs 2
      evy = elemV xs 3
      p = wP w
      ball = (wBx w, wBy w)
      err = dist2 (ex, ey) ball
      cross (x, y) = Pictures [Line [(x - 4, y - 4), (x + 4, y + 4)], Line [(x - 4, y + 4), (x + 4, y - 4)]]
      px = width - 400
      py = 110
  in Pictures
  [ view
      [ Color (hsbA 0 80 100 50) (Pictures (map cross (wZs w)))     -- 観測
      , Color (hsbA 170 60 100 60) (thickPath 2 (wTrue w))          -- 軌跡
      , Color (hsbA 45 90 100 90) (thickPath 2 (wEst w))
      , Color (hsbA 45 90 100 60) (thickPath 1.5 (ellipsePts ex ey (elemAt p 0 0) (elemAt p 0 1) (elemAt p 1 1)))
      , Color (hsb 170 60 100) (at ball (disc 9))                   -- ボールと推定
      , Color (hsb 45 90 100) (at (ex, ey) (ring 11 3))
      , Color (hsbA 45 90 100 70) (thickLine 3 (ex, ey) (ex + evx * 12, ey + evy * 12))   -- 推定速度
      , Color (hsbA 0 0 100 8) (boxAt (px - 16) (py - 30) 390 96)
      ]
  , txt 20 34 16 (hsb 0 0 100) ("Meas. sigma = " ++ nf 0 (wMeasS w) ++ " (Up/Down)   Process q = " ++ nf 2 (wProcS w) ++ " (L/R)   Space: measure "
                                ++ (if wBlind w then "off (hidden)" else "on") ++ "   R: reset")
  , txt 20 58 14 (hsb 0 0 100) ("Estimate error " ++ nf 1 err ++ " px   Gain K = " ++ nf 2 (wGain w))
  , txt 20 78 14 (hsb 0 0 60) "Cyan = true ball. Red x = noisy measurement. Yellow = estimate (2-sigma ellipse, velocity line)."
  , txt px py 14 (hsb 45 90 100) "Predict  x = F x,  P = F P F^T + Q"
  , txt px (py + 20) 14 (hsb 45 90 100) "Update   K = P H^T (H P H^T + R)^-1"
  , txt px (py + 40) 14 (hsb 45 90 100) "         x += K (z - H x),  P = (I - K H) P"
  , txt px (py + 60) 14 (hsb 0 0 60) "K near 1: trust data. K near 0: trust model."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> w { wMeasS = min 150 (wMeasS w + 5) }
    KeyDown -> w { wMeasS = max 2 (wMeasS w - 5) }
    KeyRight -> w { wProcS = min 3 (wProcS w * 1.5) }
    KeyLeft -> w { wProcS = max 0.02 (wProcS w / 1.5) }
    KeySpace -> w { wBlind = not (wBlind w) }
    _ -> w
  EventKey (Char c) Down _ _ | toLower c == 'r' -> resetAll w
  _ -> w

-- ---------- やってみよう ----------
-- 1. σ を 100 にしても推定はボールに近い。観測 1 個では無理なのに、なぜ？（過去の観測を速度ごと平均している）
-- 2. q を小さくすると推定はなめらかだが、壁で跳ね返ったとき追いつくのが遅い。q と σ の役割のちがい
-- 3. スペースで観測を止めて 2 秒待つ。推定はまっすぐ進む（等速モデル）。壁は知らないのでつき抜ける
-- 4. モデルを「等加速度」（状態 6 つ）にすると F と Q はどうなる？


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

-- 平均 0、標準偏差 1 の正規乱数（p5 の randomGaussian。ボックス・ミュラー法）
gauss :: IO Float
gauss = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

-- ここから下は「行列の道具」（4×4 と 2×2 の小さな行列計算）
eye4 :: Mat
eye4 = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]

matScaled :: Mat -> Float -> Mat
matScaled a s = map (map (* s)) a

matAdd, matSub, matMul :: Mat -> Mat -> Mat
matAdd = zipWith (zipWith (+))
matSub = zipWith (zipWith (-))
matMul a b = [[sum (zipWith (*) r c) | c <- transpose b] | r <- a]

matVec :: Mat -> Vec -> Vec
matVec a v = [sum (zipWith (*) r v) | r <- a]

inv2 :: Mat -> Mat
inv2 m =
  let a = elemAt m 0 0
      b = elemAt m 0 1
      c = elemAt m 1 0
      e = elemAt m 1 1
      d0 = a * e - b * c
      d = if abs d0 < 1e-12 then 1e-12 else d0
  in [[e / d, -b / d], [-c / d, a / d]]

-- 安全な添字アクセス
elemV :: Vec -> Int -> Float
elemV v i = case drop i v of
  (x : _) -> x
  [] -> 0

elemAt :: Mat -> Int -> Int -> Float
elemAt m i j = case drop i m of
  (r : _) -> elemV r j
  [] -> 0
