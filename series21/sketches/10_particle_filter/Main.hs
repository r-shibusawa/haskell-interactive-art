-- ============================================
-- 10 パーティクルフィルタによる自己位置推定
-- 「自分はどこ？」を、たくさんの仮説（粒子）で表す
--   予測：各粒子をロボットと同じ指令で動かす（＋ノイズ）　重みづけ：目印までの距離がセンサーと合う粒子ほど重く
--   リサンプリング：重い粒子を増やし、軽い粒子を消す → 粒子が本当の位置に集まる
-- ←→ = 粒子の数 / K = 誘拐（ロボットを別の場所へ）/ ↑↓ = センサーのノイズ / スペース = 自動運転 ON/OFF（OFF なら W/A/D で操作）/ R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower, toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, newStdGen, randomR)
import Text.Printf (printf)

motionSigma, sensorRange :: Float
motionSigma = 1.2         -- ★ 動きのノイズ（粒子をばらまく大きさ）
sensorRange = 260         -- ★ 目印が見える距離

-- 粒子 1 個 = (x, y, 向き)
type Particle = (Float, Float, Float)

data World = World
  { wX      :: !Float     -- 本当の姿勢
  , wY      :: !Float
  , wTh     :: !Float
  , wParts  :: !(U.Vector Particle)
  , wN      :: !Int       -- ★ 粒子の数
  , wSigma  :: !Float     -- ★ 距離センサーのノイズ（px）
  , wAuto   :: !Bool
  , wEst    :: !Point     -- 推定（粒子の平均）
  , wHist   :: ![Float]   -- 誤差の履歴（新しい順、300 個まで）
  , wSeen   :: ![Point]   -- いま見えている目印
  , wGen    :: !StdGen    -- 乱数（粒子ごとに使うので、まとめて持ち回る）
  }

main :: IO ()
main = runSketch (sketch "21-10 Particle Filter" (world0 (mkStdGen 1)))
  { skBack  = hsb 215 35 12
  , skStart = \_ -> world0 <$> newStdGen
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: StdGen -> World
world0 g = resetAll (World 0 0 0 U.empty 500 15 True (0, 0) [] [] g)

-- 目印（位置は既知。乱数の種を固定、p5 の randomSeed(5)）
landmarks :: [Point]
landmarks = take 7 (go (mkStdGen 5))
  where
    go g0 =
      let (x, g1) = randomR (120, width - 120) g0
          (y, g2) = randomR (150, height - 60) g1
      in (x, y) : go g2

resetAll :: World -> World
resetAll w = initParticles w { wX = width * 0.3, wY = height * 0.5, wTh = 0, wHist = [] }

-- 粒子を画面全体にばらまく（「どこにいるか全くわからない」状態）
initParticles :: World -> World
initParticles w =
  let (ps, g') = generateN (wN w) new (wGen w)
      new g0 =
        let (x, g1) = randomR (0, width) g0
            (y, g2) = randomR (100, height) g1
            (t, g3) = randomR (0, tau) g2
        in ((x, y, t), g3)
  in w { wParts = ps, wGen = g' }

-- ★ 1：予測。指令（v, om）＋ノイズで各粒子を動かす
predict :: Float -> Float -> World -> World
predict v om w =
  let n = U.length (wParts w)
      (ps, g') = generateNI n move (wGen w)
      move i g0 =
        let (x, y, t) = wParts w U.! i
            (a, g1) = gaussR g0
            (b, g2) = gaussR g1
            t' = t + om + b * 0.03
            s = v + a * motionSigma
        in ((x + s * cos t', y + s * sin t', t'), g2)
  in w { wParts = ps, wGen = g' }

-- ★ 2：重みづけ。「この粒子の場所なら目印までの距離はこうなるはず」とセンサー値を比べる
weigh :: Float -> [(Point, Float)] -> U.Vector Particle -> U.Vector Double
weigh sigma meas ps =
  let s2 = realToFrac (2 * sigma * sigma) :: Double
      weight (x, y, _) =
        let lw = foldl' (\acc ((lx, ly), md) ->
                   let e = realToFrac (dist2 (lx, ly) (x, y) - md) :: Double
                   in acc - e * e / s2) 0 meas         -- ガウス分布の対数
        in exp lw + 1e-300
      ws = U.map weight ps
      total = U.sum ws
  in U.map (/ total) ws

-- ★ 3：リサンプリング（低分散法）。重い粒子を複製、軽い粒子を捨てる
resample :: U.Vector Double -> World -> World
resample ws w =
  let ps = wParts w
      n = U.length ps
      fn = fromIntegral n :: Double
      (u0, g') = randomR (0, 1 :: Double) (wGen w)
      r = u0 / fn
      next (m, i, c)
        | m >= n = Nothing
        | otherwise =
            let u = r + fromIntegral m / fn
                (i', c') = advance u i c
            in Just (ps U.! i', (m + 1, i', c'))
      advance u !i !c
        | u > c && i < n - 1 = advance u (i + 1) (c + ws U.! (i + 1))
        | otherwise = (i, c)
  in if n == 0 then w
     else w { wParts = U.unfoldrN n next (0 :: Int, 0 :: Int, ws U.! 0), wGen = g' }

step :: Env -> World -> World
step env w0 =
  let keyD c = isDown env (Char c) || isDown env (Char (toUpper c))
      -- ロボットの指令
      (v, om)
        | wAuto w0 = (2, 0.02 * sin (fromIntegral (envFrame env) * 0.01) + 0.01)
        | otherwise = ( if keyD 'w' then 2 else 0
                      , (if keyD 'a' then -0.04 else 0) + (if keyD 'd' then 0.04 else 0) )
      th1 = wTh w0 + om
      x1 = wX w0 + v * cos th1
      y1 = wY w0 + v * sin th1
      out = x1 < 20 || x1 > width - 20 || y1 < 120 || y1 > height - 20
      th2 = if out then th1 + pi else th1
      x2 = if out then clampF 20 (width - 20) x1 else x1
      y2 = if out then clampF 120 (height - 20) y1 else y1
      -- センサー：見える目印までの距離（ノイズつき）
      visible = filter (\l -> dist2 l (x2, y2) < sensorRange) landmarks
      (meas, g1) = foldl' sensor ([], wGen w0) visible
      sensor (acc, g) l =
        let (e, g') = gaussR g
        in ((l, dist2 l (x2, y2) + e * wSigma w0) : acc, g')
      w1 = predict v om w0 { wX = x2, wY = y2, wTh = th2, wGen = g1 }
      w2 | null meas = w1
         | otherwise = resample (weigh (wSigma w1) meas (wParts w1)) w1
      -- 推定 = 粒子の平均
      n = fromIntegral (max 1 (U.length (wParts w2)))
      (sx, sy) = U.foldl' (\(!a, !b) (x, y, _) -> (a + x, b + y)) (0, 0) (wParts w2)
      est = (sx / n, sy / n)
  in w2 { wEst = est, wHist = take 300 (dist2 est (x2, y2) : wHist w2), wSeen = visible }

drawAll :: World -> Picture
drawAll w =
  let robot = (wX w, wY w)
      th = wTh w
      est@(ex, ey) = wEst w
      err = dist2 est robot
      -- 粒子は数が多いので、小さな六角形で速く描く
      dot (x, y, _) = Polygon [(x + 2, y), (x + 1, y + 1.7), (x - 1, y + 1.7), (x - 2, y), (x - 1, y - 1.7), (x + 1, y - 1.7)]
      gx = width - 330
      gy = 200
      gw = 300
      gh = 80
      graph = [ (gx + fromIntegral i / 300 * gw, gy - clampF 0 1 (e / 300) * gh)
              | (i, e) <- zip [0 :: Int ..] (reverse (wHist w)) ]
  in Pictures
  [ view
      [ Color (hsbA 45 90 100 45) (Pictures (map dot (U.toList (wParts w))))
      -- 目印と観測
      , Color (hsbA 120 60 100 30) (Pictures [Line [robot, l] | l <- wSeen w])
      , Color (hsb 120 80 100) (Pictures [boxAt (x - 8) (y - 8) 16 16 | (x, y) <- landmarks])
      -- ロボット（本当）と推定
      , Color (hsb 170 60 95) (bodyRect robot th (-16) (-12) 32 24)
      , Color (hsb 0 0 100) (bodyTri robot th (16, 0) (6, -7) (6, 7))
      , Color (hsb 45 90 100) (Pictures
          [ at est (ring 13 3)
          , thickLine 3 (ex - 10, ey) (ex + 10, ey)
          , thickLine 3 (ex, ey - 10) (ex, ey + 10) ])
      -- 誤差グラフ
      , Color (hsbA 0 0 100 8) (boxAt (gx - 10) (gy - gh - 10) (gw + 20) (gh + 20))
      , Color (hsb 0 80 100) (thickPath 2 graph)
      ]
  , txt gx (gy - gh - 16) 14 (hsb 0 0 60) "Estimate error (0 to 300 px)"
  , txt 20 34 16 (hsb 0 0 100) ("Particles " ++ show (wN w) ++ " (L/R)   Sensor sigma = " ++ nf 0 (wSigma w) ++ " (Up/Down)   Error " ++ nf 0 err
                                ++ " px   Landmarks seen " ++ show (length (wSeen w)))
  , txt 20 58 14 (hsb 0 0 100) ("K: kidnap   Space: auto " ++ (if wAuto w then "ON" else "off (drive with W A D)") ++ "   R: reset")
  , txt 20 78 14 (hsb 0 0 60) "Yellow dots = particles. Yellow ring = estimate. Green squares = landmarks. After K they scatter and gather again."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyRight -> initParticles w { wN = min 5000 (wN w * 2) }
    KeyLeft -> initParticles w { wN = max 50 (wN w `div` 2) }
    KeyUp -> w { wSigma = min 80 (wSigma w + 5) }
    KeyDown -> w { wSigma = max 5 (wSigma w - 5) }
    KeySpace -> w { wAuto = not (wAuto w) }
    _ -> w
  EventKey (Char c) Down _ _ -> case toLower c of
    'k' ->
      let (x, g1) = randomR (60, width - 60) (wGen w)
          (y, g2) = randomR (140, height - 40) g1
          (t, g3) = randomR (0, tau) g2
      in w { wX = x, wY = y, wTh = t, wGen = g3 }
    'r' -> resetAll w
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 粒子 50 個と 5000 個で、誘拐（K）から回復するまでの時間を比べよう
-- 2. 誘拐したあと粒子が「戻れない」ことがある（全部が間違った場所に集まる）。粒子を少し画面全体にばらまき続ける（5%）と直るか
-- 3. landmarks を 2 個にすると？ 1 個だと？（距離だけでは位置が決まらない = 円の上のどこか）
-- 4. motionSigma を 0 にすると粒子が「痩せて」1 点に固まり、間違えたとき戻れなくなる。確かめよう


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

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])

-- 平均 0、標準偏差 1 の正規乱数（p5 の randomGaussian。乱数の状態を受け渡す版）
gaussR :: StdGen -> (Float, StdGen)
gaussR g0 =
  let (u, g1) = randomR (1e-6, 1) g0
      (v, g2) = randomR (0, 1) g1
  in (sqrt (-2 * log u) * cos (tau * v), g2)

-- 乱数の状態を受け渡しながら n 個つくる
generateN :: Int -> (StdGen -> (Particle, StdGen)) -> StdGen -> (U.Vector Particle, StdGen)
generateN n f = generateNI n (const f)

-- 番号つきの版。ベクトルと、使い終わった乱数の状態を返す
generateNI :: Int -> (Int -> StdGen -> (Particle, StdGen)) -> StdGen -> (U.Vector Particle, StdGen)
generateNI n f g0 =
  let (seed, gB) = randomR (0, maxBound :: Int) g0     -- 粒子用に別の乱数列をつくる
      gA = mkStdGen seed
      next (i, g)
        | i >= n = Nothing
        | otherwise = let (p, g') = f i g in Just (p, (i + 1, g'))
  in (U.unfoldrN n next (0, gA), gB)
