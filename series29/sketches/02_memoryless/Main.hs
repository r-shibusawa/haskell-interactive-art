-- ============================================
-- 02 無記憶性
-- 窓口で前の人がすでに t 分サービスを受けている。あと何分かかる？
--   平均 1 分の 3 種類のサービス時間で比べる：指数分布（無記憶）・一様分布 0〜2 分・いつもちょうど 1 分
--   指数分布だけは「もう t 分たった」ことを忘れる → 残り時間の分布が最初とまったく同じ
-- ←→ = すでに経った時間 t / A = t を自動で動かす / N = サンプルを取り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (PrintfArg, printf)

nSamp, nBars :: Int
nSamp = 30000     -- ★ 各分布のサンプル数
nBars = 36        -- 上に描く棒（サンプル）の数

data Dist = Dist
  { dName :: String
  , dHue :: !Float
  , dXs :: !(U.Vector Float)        -- サンプル
  , dResid :: Float -> Maybe Float  -- 残り時間の平均 E[X − t | X > t]（生き残りがなければ Nothing）
  }

data World = World
  { wT :: !Float     -- ★ すでに経った時間
  , wAuto :: !Bool
  , wDists :: [Dist]
  }

main :: IO ()
main = runSketch (sketch "29-02 Memorylessness" (World 0.5 False []))
  { skBack  = hsb 170 25 11
  , skStart = \_ -> World 0.5 False <$> resample
  , skStep  = \_ _ w -> pure (if wAuto w then w { wT = wrapF 2 (wT w + 0.004) } else w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

resample :: IO [Dist]
resample = do
  a <- U.replicateM nSamp (fl <$> expRand 1)
  b <- U.replicateM nSamp (randomRIO (0, 2))
  pure
    [ Dist "Exponential (mean 1)" 190 a (\_ -> Just 1)
    , Dist "Uniform 0 to 2" 45 b (\t -> if t < 2 then Just ((2 - t) / 2) else Nothing)
    , Dist "Always 1 minute" 330 (U.replicate nSamp 1) (\t -> if t < 1 then Just (1 - t) else Nothing) ]

drawAll :: World -> Picture
drawAll w = Pictures
  ( [ drawColumn (wT w) d (40 + k * cw) (cw - 30) | (k, d) <- zip [0 ..] (wDists w) ]
 ++ [ drawResidPlot w 40 (height - 30) (width - 80) (height * 0.15)
    , txt 20 40 18 (gray 1) ("Elapsed t = " ++ nf 2 (wT w) ++ " min (<- ->)  A: auto " ++ (if wAuto w then "ON" else "off")
        ++ "  N: resample   Top: samples (gray = done)  Middle: time left")
    , txt 20 68 15 (hsb 0 0 60) "White = distribution at t = 0. Color = time left for those not done after t. Only the exponential matches exactly." ] )
  where cw = (width - 80) / 3

binsOf :: U.Vector Int -> U.Vector Int
binsOf is = U.accumulate (+) (U.replicate nb 0) (U.map (\i -> (i, 1)) (U.filter (< nb) is))

nb :: Int
nb = 24

drawColumn :: Float -> Dist -> Float -> Float -> Picture
drawColumn tw d x w = Pictures
  ( txt x (top - 6) 17 (hsb (dHue d) 60 100) (dName d)
  : concatMap sampleBar [0 .. nBars - 1]
 ++ [ seg 2 (gray 1) (lx, top) (lx, top + 10 + fromIntegral nBars * rowH)
    , seg 1 (hsba 0 0 100 0.4) (x, gy) (x + w, gy) ]
 ++ [ let h = if alive > 0 then (fromIntegral (b1 U.! i) / fromIntegral alive) / peak * gh else 0
      in box (x + fromIntegral i * bwPx + 1) (gy - h) (bwPx - 2) h (hsba (dHue d) 60 90 0.75)
    | i <- [0 .. nb - 1] ]
 ++ [ path 2 (gray 1) (concat
        [ let h = (fromIntegral (b0 U.! i) / fromIntegral nSamp) / peak * gh
          in [(x + fromIntegral i * bwPx, gy - h), (x + fromIntegral (i + 1) * bwPx, gy - h)]
        | i <- [0 .. nb - 1] ])
    , txt x (gy + 18) 14 (hsb 0 0 60) "0"
    , txtR (x + w) (gy + 18) 14 (hsb 0 0 60) "3 min"
    , txt x (gy - gh - 30) 15 (gray 1) ("Not done yet " ++ nf 1 (fromIntegral alive / fromIntegral nSamp * 100 :: Float) ++ "%")
    , txt x (gy - gh - 10) 15 (gray 1) (if alive > 0 then "Mean time left " ++ nf 3 (sumR / fromIntegral alive) ++ " min" else "Everyone is done") ] )
  where
    -- サンプルの棒：長さ = サービス時間、縦線 = t
    top = 100
    rowH = (height * 0.28) / fromIntegral nBars
    sx = w / 3      -- 横 1 分 = w/3 px
    lx = x + tw * sx
    sampleBar i =
      let v = dXs d U.! i
          y = top + 6 + fromIntegral i * rowH
      in if v <= tw
           then [box x y (min v 3 * sx) (rowH - 2) (hsb 0 0 40)]
           else [ box x y (tw * sx) (rowH - 2) (hsb 0 0 55)
                , box (x + tw * sx) y (min (v - tw) (3 - tw) * sx) (rowH - 2) (hsb (dHue d) 70 95) ]
    -- 残り時間 X − t（X > t の人だけ）のヒストグラム
    gy = height * 0.72
    gh = height * 0.17
    bw = 3 / fromIntegral nb :: Float
    bwPx = w / fromIntegral nb
    xs = dXs d
    left = U.filter (> tw) xs
    alive = U.length left
    sumR = U.foldl' (\s v -> s + (v - tw)) 0 left
    b0 = binsOf (U.map (\v -> floor (v / bw)) xs)
    b1 = binsOf (U.map (\v -> floor ((v - tw) / bw)) left)
    peak = max 1e-6 (U.maximum (U.generate nb (\i ->
             max (fromIntegral (b0 U.! i) / fromIntegral nSamp)
                 (if alive > 0 then fromIntegral (b1 U.! i) / fromIntegral alive else 0)))) * 1.1

-- 下：残り時間の平均 E[X − t | X > t] を t の関数として
drawResidPlot :: World -> Float -> Float -> Float -> Float -> Picture
drawResidPlot w x y wd h = Pictures
  ( [ seg 1 ax (x, y) (x + wd, y), seg 1 ax (x, y) (x, y - h) ]
 ++ concatMap curve (wDists w)
 ++ [ seg 1 (hsba 0 0 100 0.6) (xOf tw, y) (xOf tw, y - h)
    , txt (x + 8) (y - h - 6) 14 (hsb 0 0 60) "Mean time left (y) vs elapsed t (x, 0 to 2 min). Exponential is flat: however long you wait, about 1 min more" ] )
  where
    tw = wT w
    ax = hsba 0 0 100 0.4
    xOf t = x + t / 2 * wd
    yOf v = y - v / 1.2 * h
    curve d =
      let c = hsb (dHue d) 70 100
          pts = [ (xOf t, yOf r) | i <- [0 .. 100 :: Int], let t = 2 * fromIntegral i / 100, Just r <- [dResid d t] ]
      in path 3 c pts : [ dot (xOf tw) (yOf r) 12 c | Just r <- [dResid d tw] ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT = min 1.95 (wT w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wT = max 0 (wT w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "nN" -> (\ds -> w { wDists = ds }) <$> resample
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. t を 0 → 1.5 に動かす。一様分布の「残り」はどう縮む？「いつも 1 分」は？
-- 2. 指数分布は「もう 1 分待ったのに、まだ平均 1 分」。これを電話のサポート待ちに当てはめるとどんな気持ち？
-- 3. resample の b を「2 つの指数分布の足し算 / 2」（アーラン分布）にすると、指数と一様の中間になるか試す
-- 4. 平均 0.2 分と平均 5 分の混ぜ合わせ（超指数分布）だと、待つほど残りが「長く」なる。a を書き換えて確かめる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 右ぞろえの文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- シミュレーションの時刻は Double、描くときは Float
fl :: Double -> Float
fl = realToFrac

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)
